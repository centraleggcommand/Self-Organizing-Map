package somservice

sealed abstract class SomEntry
case class BasicContent(db:String, wordList:List[String], subject:String, origContent:String) extends SomEntry

