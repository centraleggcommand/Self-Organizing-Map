package somservice
import textprocessing.InputTicket
import scala.actors.Future
import scala.actors.Futures._
import scala.Math._

class SomInsertion(data:SomEntry)
{
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

  def insertEntry:Unit = {
    if( ticket.wordMap.size > 0) {
      organizeEntry  match {
        case Some((parentId:String,dist:Double)) => {
          val insertResult = dbAgent.addEntry(parentId, dist, origContent)
          println("Inserting entry: " + insertResult)
        }
        case None => println("Could not insert entry")
      }
    }
    else println("Skipping insert of entry with no content")
    dbAgent.shutdown
  }
    
  //run the som algorithm on the entry and associate with a node
  private def organizeEntry:Option[Tuple2[String,Double]] = {
    //Obtain a node id and the entry's distance from it.
    cycleThruLevels(dbAgent.getDbName, 0.0) match {
      case Some(data) => Some(data)
      case None => throw new RuntimeException("insertEntry failed because no node  id was returned")
    }
  }

  private def addStarterNode(parent:String):String = {
    val nodeId = dbAgent.addInitNode(parent, ticket.wordMap) match {
      case Some(id:String) => id
      case None => throw new RuntimeException("addInitNode did not return a node id on insert entry attempt")
    }
    val pmap = new MapPosition(dbAgent, parent)
    pmap.addInitNodePosition(nodeId)
    nodeId
  }

  //compare the entry weight to node weights at each required level
  private def cycleThruLevels(parent:String, lastDistance:Double):Option[Tuple2[String,Double]] = {
    //level zero map nodes have the db name as the parent value
    dbAgent.getNodesUsingParent(parent) match {
      case None => {
        //this is an empty som
        if (parent == dbAgent.getDbName) {
          val nodeId = addStarterNode(parent)
          Some((nodeId,0.0))
        }
        //Finally, return this node id for entry's parent info
        else Some((parent,lastDistance))
      }
      case Some(levelNodes) => {
        //Does the map layer have minimum number of nodes?
        if( levelNodes.length < minNodes ) {
          val nodeId = addStarterNode(parent)
          Some((nodeId, 0.0))
        }
        else { 
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
    val rankList = for( node <- nodes ) yield {
      future[Option[Tuple2[Node,Double]]] {
        val distance = calcNodeDistance( node)
        distance match {
          case Some(d) => Some((node,d))
          case None => None
        }
      }
    }
    val starter = rankList.head
    //return the tuple with smallest distance from node
    rankList.foldLeft(starter())(compareDistance _)
  }

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

  private def compareDistance(t1:Option[Tuple2[Node,Double]],f2:Future[Option[Tuple2[Node,Double]]]):Option[Tuple2[Node,Double]] = {
    val t2 = f2()
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
  }

}
