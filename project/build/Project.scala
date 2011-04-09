import sbt._

class Wiki2reStProject(info: ProjectInfo) extends DefaultProject(info) {
  val scalatoolsSnapshots = ScalaToolsSnapshots
  val scalatest = "org.scalatest" % "scalatest" % "1.4-SNAPSHOT" % "test"
}
