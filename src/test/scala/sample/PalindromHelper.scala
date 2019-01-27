package sample

import com.typesafe.scalalogging.StrictLogging
import gnieh.diffson.Pointer
import gnieh.diffson.sprayJson._
import io.gatling.commons.validation.{Failure, Success, Validation}
import io.gatling.core.Predef._
import io.gatling.core.action.builder.{ActionBuilder, SessionHookBuilder}
import io.gatling.core.check.DefaultFindCheckBuilder
import io.gatling.core.check.extractor.string.BodyStringCheckType
import io.gatling.core.session.{Expression, Session}
import io.gatling.core.structure.ChainBuilder
import io.gatling.http.Predef._
import io.gatling.http.action.ws._
import io.gatling.http.request.builder.HttpRequestBuilder
import spray.json.DefaultJsonProtocol._
import spray.json.lenses.JsonLenses.{field, set, _}
import spray.json.lenses.{Id, Lens}
import spray.json.{JsNumber, JsObject, JsValue, JsonParser, JsonWriter, _}

import scala.concurrent.duration._
import scala.util.Try

trait PalindromHelper extends StrictLogging {

  val viewModelKey: String = "viewModel"
  val viewModel: Session => JsObject = _ (viewModelKey).as[Try[JsObject]].get
//  val viewModel1: Session => JsArray = _ (viewModelKey).as[Try[JsArray]].get
//  viewModel1(session).elements.last.asInstanceOf[JsObject].getFields("value").head
  val patchKey: String = "patch"
  val patchEl: Expression[String] = _ (patchKey).validate[String]
  val serverVersionPath: String = "_ver#s"
  val clientVersionPath: String = "_ver#c$"
  val sessionKey: String = "sessionId"
  val sessionEl: Expression[String] = _ (sessionKey).validate[String]

  def openPage(pagePath: Expression[String]): HttpRequestBuilder =
    http(s => pagePath(s).map("open " + _))
      .get(pagePath)
      .header("Accept", "application/json-patch+json")
      .header("X-Referer", sessionEl)
      .check(bodyString
        .transform(s => Try(Try(JsonParser(s)).asInstanceOf[Try[JsArray]].get.elements.last.asJsObject().getFields("value").head))
        .saveAs(viewModelKey)
      )

  def createSession(url: Expression[String]): ChainBuilder =
    ChainBuilder(List.empty)
      .exec(http("createSession")
        .get(url)
        // it needs to be text/html, otherwise session is not initialized properly, weird SC bug
        .header("Accept", "text/html")
        .check(regex("remote-url=\"(/__default/[^\"]+)\"")
          .saveAs(sessionKey)
        )
      )
      .exec(http("obtainViewModel")
        .get(sessionEl) // URI is the session key just obtained
        .header("Accept", "application/json")
        .check(
          bodyString
            .transform(s => Try(JsonParser(s)))
            .saveAs(viewModelKey)
        )
      )
      .exec(openWs())

  def openWs(): WsConnectBuilder =
    ws("open ws")
      .connect(sessionEl)

  private def updateViewModelInternal[T: JsonWriter, M[_]](name: String, lensExpression: Expression[Lens[M]], newValueExpression: Expression[T]): ChainBuilder =
    exec(onlyMakeChanges((vm, session) => {
      val lens = lensExpression(session).asInstanceOf[Success[Lens[M]]].value
      val newValue = newValueExpression(session).asInstanceOf[Success[T]].value
      vm.update(lens ! set(newValue))
    }))
      .exec(sendChanges(name))

  def updateViewModelId[T: JsonWriter](name: String, lens: Lens[Id], newValueExpression: Expression[T]): ChainBuilder =
    updateViewModelInternal(name, lens, newValueExpression)

  def validatePatch(checkLastPatch:Boolean,
                    validationRoot: DefaultFindCheckBuilder[BodyStringCheckType, String, String],
                    patchName: String) =

    validationRoot
      .transform(s => Try(JsonPatch.parse(s)))
      .transform((triedPatch) => {
        triedPatch match {
          case util.Success(p) => logger.debug("Received patch: "+p.toString())
          case util.Failure(e) => logger.error("Received bad patch: ",e)
        }
        triedPatch
      })
      // this is where patch is applied to old vm
      .transform((triedPatch, session) => triedPatch.map(patch => patch(viewModel(session))))
      .validate("check view model for exceptions", validateTry(checkLastPatch, patchName))
      .saveAs(viewModelKey)

  def sendChanges(patchName: String): ActionBuilder = {

    //validatePatch(true, ws.checkTextMessage("Checking messages").check(regex(".*")), patchName)
    ws("send changes")
      .sendText(s => {
        logger.debug("sending patch: " + patchEl(s))
        patchEl(s)
      })
      //.await(1 second)(validatePatch(true, bodyString, patchName))
      .await(2 second) {
      ws.checkTextMessage("Checking messages").check({
        jsonPath("$")
          .transform(s => Try(JsonPatch.parse(s)))
          .transform((triedPatch) => {
            triedPatch match {
              case util.Success(p) => logger.debug("Received patch: " + p.toString())
              case util.Failure(e) => logger.error("Received bad patch: ", e)
            }
            triedPatch
          })
          // this is where patch is applied to old vm
          .transform((triedPatch, session) => triedPatch.map(patch => patch(viewModel(session))))
          .validate("check view model for exceptions", validateTry(checkLastPatch = true, patchName))
          .saveAs(viewModelKey)
      })
    }
  }

  def onlyMakeChanges(change: (JsObject, Session) => JsValue): ActionBuilder = {
    new SessionHookBuilder((session: Session) => {
      try {
        val oldVm = viewModel(session)
        val oldClientVersion = oldVm.extract[Int](field(clientVersionPath))
        val newVm: JsValue = change(oldVm, session) // this is without client version change, because we already add that in headerPatch
        val changesPatch: JsonPatch = JsonDiff.diff(oldVm, newVm, remember = false)
        val headerPatch = JsonPatch(
          Replace(Pointer(clientVersionPath), JsNumber(oldClientVersion + 1)),
          Test(Pointer(serverVersionPath), JsNumber(oldVm.extract[Int](field(serverVersionPath)))))

        val patch = headerPatch.andThen(changesPatch)
        session.set(patchKey, patch)
          .set(viewModelKey, Try(newVm.update(field(clientVersionPath) ! set[BigDecimal](oldClientVersion + 1))))
      } catch {
        case e: Exception => logger.error("Exception", e)
          throw e;
      }
    }, exitable = true)
  }

  def validateTry[T](checkLastPatch:Boolean, patchName: String)(maybeTriedValue: Option[Try[T]], session: Session): Validation[Option[Try[T]]] = {
    val result =  (if (checkLastPatch && false)
    // a patch always contains two versions operations. Only when it contains 3 or more, we consider it non-empty
      patchEl(session) flatMap {p => if (JsonPatch.parse(p).ops.length >= 3) Success() else Failure(s"Last patch '$patchName' sent out was empty")}
    else Success())
      .flatMap(_ =>
        maybeTriedValue.map {
          case util.Failure(e) =>
            Failure(e.toString)
          case util.Success(v) => Success(Some(util.Success(v)))
        }.getOrElse(Failure("no option")))

    result match {
      case Failure(e) =>
        logger.error(formatErrorMessage(session, e))
        throw new Exception(e);
      case Success(_) => result
    }
  }

  def formatErrorMessage(session: Session, message: String) = {
    val vm = viewModel(session)
    val allProperties = session.remove(viewModelKey)
    s"Error: $message\nproperties: $allProperties\nview-model: $vm"
  }
}
