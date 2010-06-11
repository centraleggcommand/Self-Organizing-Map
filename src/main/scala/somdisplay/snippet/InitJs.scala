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

class InitJs {

  val logger = Logger.getLogger("somdisplay.snippet.InitJs")
  PropertyConfigurator.configure("log4j.properties")

  def initMap( dbData: NodeSeq) : NodeSeq = {
    //Get the name of the first database found
    val dbAgent = new CouchAgent("nada")
    val availableDbs = dbAgent.getAllDbs
    dbAgent.shutdown
    val initSelection = availableDbs match {
      case Nil => ""
      case a => a.head
    }
    logger.debug("Setting initial map")
    //Trigger a change event for the select box so that an ajax fxn
    //will run for setting dbname in the model, and then display map.
    //Seems like the select box value returned with the ajax fxn is
    //equal to whatever value is shown when page loads.
    val dbSelectFxn = JE.JsRaw(
      """for(var i=0;i<document.getElementById('dbselect').options.length;i++)
         {
           if('""" + initSelection + """' == document.getElementById('dbselect').options[i].text) {
             $('#dbselect').trigger('change');
             break;
           }
         }""").cmd
    //Return the xml to be included in the header (supposedly automatic by Lift)
    <head>{JsCmds.Script { 
      JsCmds.Function("initializeMap",Nil, dbSelectFxn) }}
    </head>
  }

}
