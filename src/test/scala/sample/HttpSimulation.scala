package sample

import io.gatling.core.action.builder.ActionBuilder

import io.gatling.commons.validation.{Failure, Success, Validation}
import io.gatling.core.Predef.{value2Success, _}
import io.gatling.http.Predef._
import spray.json.DefaultJsonProtocol._
import spray.json.lenses.JsonLenses._
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import spray.json.{JsNumber, JsObject, JsValue, JsonParser, JsonWriter}

class HttpSimulation extends Simulation with PalindromHelper {

  private val targetAddress = "localhost:8080"

  var httpConf = http
    .baseUrl("http://" + targetAddress)
    .acceptHeader("application/json")
    .wsBaseUrl("ws://" + targetAddress)

  def openNewItemUri(): ActionBuilder = {
    openPage(session => {
      val newItemUri = 'Value / 'Id
      var foundItems = newItemUri(session)
      val lastPatch = session(viewModelKey).value.as[JsObject]
      val url = lastPatch.extract[String](newItemUri)
      if (url.isEmpty)
        Failure("cart url was empty")
      else
        Success(url)
    })
  }

  val mainViewModel = 'Playground_0


  var scn = scenario("Kostiantyn Playground!")
    .exec {
      session =>
        session
    }
    .exec(createSession("/index"))
    .pause(2 seconds)
    .exec(updateViewModelId("Insert Trigger", mainViewModel / 'InsertTrigger$, 1))
    .pause(2 seconds)
    .exec(openNewItemUri())
    .exec(ws("close ws").close)

  setUp(scn.inject(atOnceUsers(1)))
    .protocols(httpConf)
}
