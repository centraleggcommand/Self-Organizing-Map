package somdisplay.snippet

import somservice._
import textprocessing.{Stemmer,StopWords}
import scala.collection.mutable.ListBuffer
import scala.xml._
import _root_.net.liftweb.common.{Full,Empty,Box,Failure}
import _root_.net.liftweb.http._
import S._
import js._
import _root_.net.liftweb.util._
import Helpers._
import org.apache.log4j.{Logger,PropertyConfigurator}

class EntryForm  {

  val logger = Logger.getLogger("somdisplay.snippet.EntryForm")
  PropertyConfigurator.configure("log4j.properties")

  def basic( entryData: NodeSeq): NodeSeq = {
    //Get the list of available databases
    val dbAgent = new CouchAgent("nada")
    val availableDbs = dbAgent.getAllDbs
    dbAgent.shutdown
    val initSelection = availableDbs match {
                          case Nil => ""
                          case a => a.head
                        }
    val dbSelection = availableDbs.map(x=>(x,x))  
    //the following vars are used by handleSubmit fxn and populated
    //by the Lift bind
    var dbName = initSelection
    var subject = ""
    var entry = ""
    def handleSubmit = {
      if( entry.length > 0) {
        //break entry data into list of filtered, stemmed words
        val input = new ListBuffer[String]
        val noPunc = """\s*\W*([\w\d\'_-]*)\W*\s*""".r
        for( noPunc(word) <- noPunc findAllIn entry) input += word
        val inputList = input.toList.map( _.toLowerCase )
        val filteredList = StopWords.removeStopwords(inputList)
        val stemmer = new Stemmer()
        val stemmedList = filteredList.map(stemmer.stemWord(_))
        val entryInfo = new BasicContent( dbName, stemmedList, subject, entry)
        val insertRequest = new SomInsertion( entryInfo)
        logger.debug("Inserting into db: " + dbName)
        logger.debug("Using insert weights: " + stemmedList)
        insertRequest.insertEntry
        insertRequest.cleanup
        logger.debug("Inserting the entry: " + entry)
        val expansionCheck = new SomExpansion(dbName)
        expansionCheck.checkExpansion
        expansionCheck.cleanup
      }
      else ("Not inserting zero length entry")
    }
    bind("entry", entryData,
      "db" -> SHtml.ajaxSelect(dbSelection, Full(initSelection), 
        { sel => dbName = sel
                 val selMap = new MapDisplay
                 selMap.getMap( sel, sel)  }),
      "subject" -> SHtml.text( subject, subject = _),
      "content" -> SHtml.textarea( entry, entry = _, "cols"->"40", "rows"->"6"),
    "submit" -> SHtml.submit("Submit", handleSubmit _))
  }


}

