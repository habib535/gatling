package sample

import io.gatling.commons.validation.{Failure, Success, Validation}
import io.gatling.core.Predef.{value2Success, _}
import io.gatling.core.action.builder.ActionBuilder
import io.gatling.http.Predef._
import spray.json.DefaultJsonProtocol._
import spray.json.lenses.JsonLenses._
import spray.json.lenses.Lens

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

class HttpSimulation extends Simulation with PalindromHelper {

  private val targetAddress = "localhost:8080"

  val mainViewModel = 'Playground_0
  val newItemUri = mainViewModel / 'Items / * / 'Id

  var httpConf = http
    .baseUrl("http://" + targetAddress)
    .acceptHeader("application/json")
    .wsBaseUrl("ws://" + targetAddress)

  def openNewItemUri(): ActionBuilder = {
    openPage(session => {
      val itemIds = tryExtractViewModelSeq[String](session, newItemUri)
      var itemId = itemIds.asInstanceOf[Success[Seq[String]]].value.last
      if (itemId.isEmpty)
        Failure("no item found")
      else
        Success("/Items/" + itemId)
    })
  }

  def tryExtractViewModelSeq[T: spray.json.lenses.Reader](session: Session, lens: Lens[Seq]): Validation[Seq[T]] = {
    lens.tryGet[T](viewModel(session)) match {
      case Right(v) => Success(v)
      case Left(ex) => Failure(ex.toString)
    }
  }

  var scn = scenario("Kostiantyn Playground!")
    .exec(createSession("/Index"))
    .pause(2 seconds)
    .exec(updateViewModelId("Insert Trigger", mainViewModel / 'InsertTrigger$, 1))
    .pause(2 seconds)
    .exec(openNewItemUri())
    .pause(2 seconds)
    .exec(updateViewModelId("Index", mainViewModel / 'Item / 'Index$, 5))
    .exec(updateViewModelId("Guid", mainViewModel / 'Item / 'Guid$, System.currentTimeMillis()))
    .pause(2 seconds)
    .exec(updateViewModelId("Save Trigger", mainViewModel / 'SaveTrigger$, 1))
    .exec(ws("close ws").close)

  setUp(scn.inject(atOnceUsers(1)))
    .protocols(httpConf)
}
