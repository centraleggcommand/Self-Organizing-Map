package somservice
import textprocessing.InputTicket
import org.apache.log4j.Logger
import org.apache.log4j.PropertyConfigurator
import scala.actors.Future
import scala.actors.Futures._
import scala.Math._

class SomInsertion(data:SomEntry)
{
  //Construction***
  val dbAgent:SomDbAgent = data match {
    case c:BasicContent => new CouchAgent(c.db)
  }
  //transform the list into a hashmap with word and occurrence pairs
  val ticket:InputTicket = data match {
    case c:BasicContent => new InputTicket(c.wordList)
  }
  val subject:String = data match {
    case c:BasicContent => c.subject 
  }
  val origContent:String = data match {
    case c:BasicContent => c.origContent
  }
  val minNodes = 4
  val logger = Logger.getLogger("somservice.SomInsertion")
  PropertyConfigurator.configure("log4j.properties")
  //**************

  def cleanup = dbAgent.shutdown

  def insertEntry:Unit = {
    if( ticket.wordMap.size > 0) {
      organizeEntry  match {
        case Some((parentId:String,dist:Double)) => {
          dbAgent.addEntry(parentId, dist, subject, origContent)
        }
        case None => logger.info("Could not insert entry")
      }
    }
    else logger.info("Skipping insert of entry with no content")
  }
    
  //run the som algorithm on the entry and associate with a node
  private def organizeEntry:Option[Tuple2[String,Double]] = {
    //Obtain a node id and the entry's distance from it.
    cycleThruLevels(dbAgent.getDbName, 0.0) match {
      case Some(data) => Some(data)
      case None => None
    }
  }

  private def addStarterNode(parent:String):Option[String] = {
    dbAgent.addInitNode(parent, ticket.wordMap) match {
      case Some(id:String) => {
        val pmap = new MapPosition(dbAgent, parent)
        pmap.addInitNodePosition(id)
        Some(id)
      }
      case None => None
    }
  }

  //compare the entry weight to node weights at each required level
  private def cycleThruLevels(parent:String, lastDistance:Double):Option[Tuple2[String,Double]] = {
    //level zero map nodes have the db name as the parent value
    dbAgent.getNodesUsingParent(parent) match {
      case None => {
        //this is an empty som
        if (parent == dbAgent.getDbName) {
        logger.debug("at first addStarterNode")
          addStarterNode(parent) match {
            case Some(nodeId) => Some((nodeId,0.0))
            case None => None
          }
        }
        //No sub levels, return this node id for entry's parent info
        else Some((parent,lastDistance))
      }
      case Some(levelNodes) => {
        //Does the map layer have minimum number of nodes?
        if( levelNodes.length < minNodes ) {
        logger.debug("at second addStarter ..." + levelNodes.length)
          addStarterNode(parent) match {
            case Some(nodeId) => Some((nodeId, 0.0))
            case None => None
          }
        }
        else { 
          //find closest matching node and recursively descend any levels
          val matchedNode = cycleThruNodes( levelNodes)
          matchedNode match {
            case None => None
            case Some((node,dist)) => {
              val upNode = node.updateWeight( ticket.wordMap)
              dbAgent.updateNode( upNode)
              cycleThruLevels( node.id, dist)
            }
          } 
        }
      }
    }
  }
      
  private def cycleThruNodes(nodes:List[Node]):Option[Tuple2[Node,Double]] = {
    //create a list of rankings
    try {

    if( nodes.isEmpty) None
    else {
      val rankList = for( node <- nodes ) yield {
        future[Tuple2[Node,Double]] {
          (node, node.calcNodeDistance( ticket.wordMap, dbAgent))
        }
      }
      val starter = rankList.head
      //return the tuple with smallest distance from node
      Some(rankList.foldLeft(starter())(compareDistance _))
    }

  } catch {
    case ex:NullPointerException => {
      logger.error("null pointer in cycleThruNodes: " + ex)
      None
    }
    case ex:RuntimeException => {
      logger.error(ex)
      None
    }
  }
  }

  private def compareDistance(t1:Tuple2[Node,Double],f2:Future[Tuple2[Node,Double]]):Tuple2[Node,Double] = {
    val t2 = f2()
    logger.debug("Distance from node: " + t2._1.id + " is " + t2._2)
    if( t1._2 < t2._2) t1
    else t2
  }

}
