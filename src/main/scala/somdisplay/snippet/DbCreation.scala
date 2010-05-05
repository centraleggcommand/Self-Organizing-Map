package somdisplay.snippet

import somservice._
import org.apache.log4j.{Logger, PropertyConfigurator}
import scala.collection.mutable.{Stack, ListBuffer}
import _root_.net.liftweb.http._
import scala.xml._
import S._
import js._
import _root_.net.liftweb.util._
import Helpers._

class DbForm {

  val logger = Logger.getLogger("somdisplay.snippet.DbCreation")
  PropertyConfigurator.configure("log4j.properties")

  def dbCreate( dbData: NodeSeq) : NodeSeq = {
    //store the db name entered for creation
    var dbName = ""
    def handleSubmit = {
      if( dbName.length > 0) {
        val dbAgent = new CouchAgent(dbName)
        logger.debug("Attempting to create db: " + dbName)
        dbAgent.createSom
        dbAgent.shutdown
      }
    }
    bind("create", dbData,
      "dbname" -> SHtml.text("",dbName = _),
      "submit" -> SHtml.submit("Create", handleSubmit _))
  }

}
