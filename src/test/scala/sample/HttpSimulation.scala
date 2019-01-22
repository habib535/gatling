package sample

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import io.gatling.commons.validation._

import scala.concurrent.duration._

class HttpSimulation extends Simulation {

  private val targetAddress = "system-test"

  var httpProtocol = http
    .baseUrl("http://" + targetAddress)
    .acceptHeader("application/json")
  //.wsBaseUrl("ws://" + targetAddress)

  var scn = scenario("Http Hello World")
    .exec {
      session =>
        println(session)
        session
    }
    .exec(http("home page")
      .get("/showroom/products"))
    pause(5 seconds)

  setUp(scn.inject(atOnceUsers(1)))
    .protocols(httpProtocol)
}
