package somdisplay.snippet

import somservice._
import org.apache.log4j.{Logger, PropertyConfigurator}
import scala.collection.mutable.{Stack, ListBuffer}
import _root_.net.liftweb.http._
import scala.xml._
import S._
import js._

class MapDisplay {

  val logger = Logger.getLogger("somdisplay.snippet.MapDisplay")
  PropertyConfigurator.configure("log4j.properties")

  //This holds all the maps viewed so the user can traverse back.
  //This is data that persists through all requests for a session.
  object mapStackVar extends SessionVar[Stack[NodeSeq]](new Stack[NodeSeq])
  //This is used to determine if the database has been changed and
  //the mapStack should be cleared.
  object prevDbVar extends SessionVar[String] ("")
  //The data for map currently being displayed is saved here
  //and is next to be pushed onto mapStackVar.
  object currentMapVar extends SessionVar[NodeSeq] ( Nil)
  //This is the id of the html element where the new ajax content
  //should be inserted.
  val mapDisplayId = "mapdisplay"

  def init( xhtml: NodeSeq) : NodeSeq = 
    <div id={mapDisplayId}>
    </div>

  //This provides the callback fxn to update the section between the
  //tags identified by 'mapDisplayId'.
  def getMap(dbName:String, parent:String) = {
    //do some update of state
    val currMapStack = mapStackVar.is
    if( dbName != prevDbVar.is) {
      currMapStack.clear
      currentMapVar(Nil)
    }
    //currentMapVar should never really be zero if db unchanged.
    else {
      if(currentMapVar.is.length != 0) currMapStack.push(currentMapVar.is)
      else logger.error("currentMapVar was empty unexpectedly")
    }
    val dbAgent = new CouchAgent( dbName)
    val layerNodes = dbAgent.getNodesUsingParent(parent)
    val infoDisplay = layerNodes match {
      case Some(nodes:List[_]) => {
        //Each node will be represented by a list of .
        //The html will be a table of ajax anchors.
        val posMap = new MapPosition(dbAgent, parent)
        if( posMap.positionData.getGridList.length > 0) showNodes(posMap)
        else Text("Error in obtaining map")
      }
      case None => {
        //Get all child entries for this parent
        dbAgent.getEntries(parent) match {
          case Some(entryData:NodeEntry) => showEntries(entryData)
          case None => {SHtml.a(()=> getPrevMap, Text("Go up to previous map"))} ++ <br/> ++ Text("No entries found for selected node")
        }
      }
    }
    dbAgent.shutdown
    currentMapVar( infoDisplay)
    mapStackVar(currMapStack)
    prevDbVar(dbName)
    JsCmds.SetHtml(mapDisplayId, infoDisplay)
  }

  def getPrevMap:JsCmd = {
    val currMapStack = mapStackVar.is
    if(!currMapStack.isEmpty) {
      currentMapVar( currMapStack.top)
      currMapStack.pop
      mapStackVar(currMapStack)
      JsCmds.SetHtml(mapDisplayId, currentMapVar.is)
    }
    else JsCmds.SetHtml(mapDisplayId, Text("No previous map"))
  }

  //private def showNodes(members:List[Any], posMap:MapPosition): NodeSeq = {
  private def showNodes( posMap:MapPosition): NodeSeq = {
    val dbName = posMap.dbAgent.getDbName
    val elements = new ListBuffer[NodeSeq]
    val gridList = posMap.positionData.getGridList
    for( gridrow <- gridList) {
       val rs = getRowSubjects( gridrow)
       if( rs.length > 0) {
         elements += <tr>{rs.reduceLeft(_ ++ _)}</tr>
       }
    }
    def getRowSubjects( row:List[String]):List[NodeSeq] = {
      val rowbuf = new ListBuffer[NodeSeq]
      for( id <- row) {
        id match {
          case myid:String => {
	    posMap.dbAgent.getEntries(myid) match {
	      case Some(entries:NodeEntry) => {
		val nodeSubjects = entries.getEntrySubjects
		val anchor = <td>{SHtml.a(()=> getMap(dbName, myid), Text(nodeSubjects.take(5).reduceLeft((x,y) => x +"," + y)) ) }</td>
		rowbuf += anchor
	      }
	      case None => {
		logger.debug("setting empty anchor")
		val emptyAnchor = <td> </td>
		rowbuf += emptyAnchor
	      }
	    }
	  }
        }
      }
      rowbuf.toList
    }
    if( elements.length > 0) {
      val mapData = <table border="2">{elements.reduceLeft(_ ++ _)}</table>
      //Provide a link to previous map display if this is not the first
      if( currentMapVar.is.length > 0) {SHtml.a(()=> getPrevMap, Text("Go up to previous map"))} ++ <br/> ++ {mapData}
      else mapData
    }
    else Text("Could not obtain map info")

  }

  private def showEntries(entryData:NodeEntry) = {
    val elements = new ListBuffer[NodeSeq]
    for( (id,subject,content) <- entryData.getEntryData) {
      elements += <tr><td>Subject: {subject}</td></tr><tr><td>{content}</td></tr>
    }
    //Provide a link to previous map display if this is not the first
    {SHtml.a(()=> getPrevMap, Text("Go up to previous map"))} ++ <br/> ++ <table>{elements.reduceLeft(_ ++ _)}</table>
  }


}

