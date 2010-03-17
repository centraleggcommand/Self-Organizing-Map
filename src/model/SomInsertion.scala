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

  def insertEntry:Unit = {
    val parentNodeData = organizeEntry match {
      case Some((id:String,distance:Double)) => (id,distance)
      case _ => throw new RuntimeException("insertEntry failed because no node id was returned")
    }
    //Calculate the distance from the node, and percentage error.
    //Although this was previously calculated, it seemed messy to pass
    //around distance info through recursive calls when it was only needed
    //at the end.
    val insertResult = dbAgent.addEntry(parentNodeData._1, parentNodeData._2, origContent)
    println("Inserting entry: " + insertResult)
  }
    
  //run the som algorithm on the entry and associate with a node
  private def organizeEntry:Option[Tuple2[String,Double]] = {
    //An entry requires a node id as parent
    cycleThruLevels(dbAgent.getDbName, 0.0) 
  }
  //compare the entry weight to node weights at each required level
  private def cycleThruLevels(parent:String, lastDistance:Double):Option[Tuple2[String,Double]] = {
    //level zero map nodes have the db name as the parent value
    dbAgent.getNodesUsingParent(parent) match {
      //either no nodes exist (new map) or node is not a parent
      case None => {
        if (parent == dbAgent.getDbName) {
          dbAgent.addDbNode(ticket.wordMap) match {
            case Some(id) => Some((id,0.0))
            case None => throw new RuntimeException("addDbNode did not return a node id on insert entry attempt")
          }
        }
        //return this node id for entry's parent info
        else Some((parent,lastDistance))
      }
      case Some(levelNodes) => {
        val matchedNode = cycleThruNodes( levelNodes)
        val upNode = matchedNode._1.updateWeight( ticket.wordMap)
        dbAgent.updateNode( upNode)
        cycleThruLevels( matchedNode._1.id, matchedNode._2)
      }
    }
  }
      
  private def cycleThruNodes(nodes:List[Node]):Tuple2[Node,Double] = {
    //create a list of rankings
    val rankList = for( node <- nodes ) yield {
      future[Tuple2[Node,Double]] {
        val distance = calcNodeDistance( node)
        distance match {
          case Some(d) => (node,d)
          case None => (node,-1.0)
        }
      }
    }
    val starter = rankList.head
    //return the tuple with smallest distance from node
    rankList.foldLeft(starter())(compareDistance _)
  }

  //This returns the distance between the ticket and node.
  //If the maps have no words in common, then a 'None' value is returned.
  private def calcNodeDistance( node:Node ):Option[Double] = {
    //The word counts are given a calculated weight based on the 
    //increasing importance from higher count, but decreasing importance
    //as the global usage of the word increases.
    def offsetWeight(count:Double, global:Double):Double = {
      (pow(1.02,ceil(count))) / (pow(1.008,global) + (pow(1.0008,global))/10)
    }
    //Calculate the euclidean distance between node and ticket
    val calcDiff = for{ (word,count) <- ticket.wordMap 
                        if( node.weight.contains(word))} yield {
      val wc:Double = dbAgent.getGlobalWordCount(word) match {
        case Some(num) => num
        case None => 1.0
      }
      val nodeWeight:Double = node.weight(word)
      //the node weight is converted to an average over the total
      //number of docs, then the offset is calculated
      val childNum = dbAgent.getChildDocNum(node.id) match {
        case Some(n) => n
        case None => 1.0
      }
      val avgNodeWeight = nodeWeight/childNum
      val nodeOffset = offsetWeight(nodeWeight, wc)
      val ticketOffset = offsetWeight(count, wc)
      val sqdiff = pow((nodeOffset - ticketOffset),2)
      (word -> sqdiff)
    }
    //finish euclidean distance calc on squared differences in the map
    if(!calcDiff.isEmpty) {
      val sumDiff = calcDiff.reduceLeft((x,y) => ("result",x._2 + y._2))
      Some(sqrt(sumDiff._2))
    }
    //when the node and ticket have no words in common
    else None
  }

  private def compareDistance(t1:Tuple2[Node,Double],f2:Future[Tuple2[Node,Double]]):Tuple2[Node,Double] = {
    val t2 = f2()
    if( t1._2 < t2._2 ) t1
    else t2
  }

}
