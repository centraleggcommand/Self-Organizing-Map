import sbt._

class SomProject( info: ProjectInfo) extends DefaultProject(info)
{
  override def compileOptions = super.compileOptions ++ Seq(Unchecked)
}
