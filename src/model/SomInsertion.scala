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
    case BasicContent(dbName,_,_) => new CouchAgent(dbName)
  }
  val ticket:InputTicket = data match {
    case BasicContent(_,wordList,_) => 
    //transform the list into a hashmap with word and occurrence pairs
    new InputTicket(wordList)
  }
  val origContent:String = data match {
    case BasicContent(_,_,raw) => raw
  }
  val minNodes = 4
  val logger = Logger.getLogger("somservice.SomInsertion")
  PropertyConfigurator.configure("log4j.properties")
  //**************

  def insertEntry:Unit = {
    if( ticket.wordMap.size > 0) {
      organizeEntry  match {
        case Some((parentId:String,dist:Double)) => {
          dbAgent.addEntry(parentId, dist, origContent)
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
  }

/*
  //This returns the distance between the ticket and node.
  //If the ticket is empty, then a 'None' value is returned.
  private def calcNodeDistance( node:Node ):Option[Double] = {
    //The word counts are given a calculated weight based on the 
    //increasing importance from higher count, but decreasing importance
    //as the global usage of the word increases.
    def offsetWeight(count:Double, global:Double):Double = {
      (pow(1.02,ceil(count))) / (pow(1.008,global) + (pow(1.0008,global))/10)
    }
    //Calculate the euclidean distance between node and ticket
    val calcDiff = for( (word,count) <- ticket.wordMap) yield {
      val wc:Double = dbAgent.getGlobalWordCount(word) match {
        case Some(num) => num
        case None => 1.0
      }
      val ticketOffset = offsetWeight(count, wc)
      if( node.weight.contains(word)) {
        val nodeWeight:Double = node.weight(word)
        //the node weight is converted to an average over the total
        //number of docs, then the offset is calculated
        val childNum = dbAgent.getChildDocNum(node.id) match {
          case Some(n) => n
          case None => 1.0
        }
        val avgNodeWeight = nodeWeight/childNum
        val nodeOffset = offsetWeight(nodeWeight, wc)
        val sqdiff = pow((nodeOffset - ticketOffset),2)
        (word -> sqdiff)
      }
      else {
        val sqdiff = pow(ticketOffset,2)
        (word -> sqdiff)
      }
    }
    //finish euclidean distance calc on squared differences in the map
    if(!calcDiff.isEmpty) {
      val sumDiff = calcDiff.reduceLeft((x,y) => ("result",x._2 + y._2))
      Some(sqrt(sumDiff._2))
    }
    //empty ticket?
    else None
  }
  */

  private def compareDistance(t1:Tuple2[Node,Double],f2:Future[Tuple2[Node,Double]]):Tuple2[Node,Double] = {
    val t2 = f2()
    /*
    t1 match {
      case None => t2
      case Some((node,dist)) => {
        t2 match { 
          case None => t1
          case Some((node2,dist2)) => {
            if (dist < dist2) t1
            else t2
          }
        }
      }
    }
    */
    if( t1._2 < t2._2) t1
    else t2
  }

}
