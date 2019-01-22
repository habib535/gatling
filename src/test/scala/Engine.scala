import io.gatling.app.Gatling
import io.gatling.core.config.GatlingPropertiesBuilder
import sample.HttpSimulation

object Engine extends App {

  val props = new GatlingPropertiesBuilder()
    .resourcesDirectory(IDEPathHelper.resourcesDirectory.toString)
    .resultsDirectory(IDEPathHelper.resultsDirectory.toString)
    .binariesDirectory(IDEPathHelper.mavenBinariesDirectory.toString)
    .simulationClass(classOf[HttpSimulation].getName)


  Gatling.fromMap(props.build)
}
