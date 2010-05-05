import sbt._

class SomProject( info: ProjectInfo) extends DefaultWebProject(info)
{
  val liftVersion = "2.0-M2"

  override def libraryDependencies = Set(
    "net.liftweb" % "lift-webkit" % liftVersion % "compile->default",
    "net.liftweb" % "lift-mapper" % liftVersion % "compile->default",
    "org.mortbay.jetty" % "jetty" % "6.1.22" % "test->default",
    "com.h2database" % "h2" % "1.2.121",
    "junit" % "junit" % "4.5" % "test->default",
    "org.scala-tools.testing" % "specs" % "1.6.1" % "test->default",
    "org.slf4j" % "slf4j-log4j12" % "1.4.1"
  ) ++ super.libraryDependencies

  val smackRepo = "m2-repository-smack" at "http://maven.reucon.com/public"

  override def compileOptions = super.compileOptions ++ Seq(Unchecked)
  override def artifactID = "som"
}
