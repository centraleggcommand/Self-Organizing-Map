package somservice
import textprocessing.InputTicket
import scala.actors.Future
import scala.actors.Futures._
import scala.Math._

class SomInsertion(data:SomEntry)
{
  //run the som algorithm on the entry and associate with a node
  def organizeEntry:String = {
    data match {
      case BasicContent(dbName,wordList) => 
        //transform the list into a hashmap with word and occurrence pairs
        val ticket = new InputTicket(wordList)
        //create the representation of the level zero map
        val dbAgent = new CouchAgent(dbName)
        //Add db entry using node id as parent
        cycleThruLevels(dbAgent,dbName,ticket) match {
          case Some(winnerNode) => {
            //make entry
            "We have a winner!"
          }
          case None => "There was an error in inserting the entry"
        }
    }
  }
  //compare the entry weight to node weights at each required level
  private def cycleThruLevels(dbAgent:SomDbAgent, parent:String, ticket:InputTicket):Option[String] = {
    //level zero map nodes have the db name as the parent value
    dbAgent.getNodesUsingParent(parent) match {
      //either no nodes exist (new map) or node is not a parent
      case None => {
        if (parent == dbAgent.getDbName) dbAgent.addDbNode(ticket.wordMap) 
        else Some(parent)
      }
      case Some(levelNodes) => {
        val matchedNode = cycleThruNodes(dbAgent, levelNodes, ticket)
        cycleThruLevels(dbAgent, matchedNode.id, ticket)
      }
    }
  }
      
  private def cycleThruNodes(dbAgent:SomDbAgent, nodes:List[Node], ticket:InputTicket):Node = {
    //create a list of rankings
    val rankList = for( node <- nodes ) yield {
      future[Tuple2[Node,Double]] {
        val distance = calcNodeDistance(dbAgent,node,ticket)
        (node, distance)
      }
    }
    val starter = rankList.head
    val winner = rankList.foldLeft(starter())(compareDistance _)
    winner._1
  }

  private def calcNodeDistance(dbAgent:SomDbAgent, node:Node, ticket:InputTicket):Double = {
    //The word counts are given a calculated weight based on the 
    //increasing importance from higher count, but decreasing importance
    //as the global usage of the word increases.
    def offsetWeight(count:Double, global:Double):Double = {
      (pow(1.02,count)) / (pow(1.008,global) + (pow(1.0008,global))/10)
    }
    //Calculate the euclidean distance between node and ticket
    val calcDiff = for{ (word,count) <- ticket.wordMap 
                        if( node.weight.contains(word))} yield {
      val wc:Double = dbAgent.getGlobalWordCount(word) match {
        case Some(num) => num
        case None => 1.0
      }
      val nodeWeight:Double = node.weight.apply(word)
      val nodeOffset = offsetWeight(nodeWeight, wc)
      val ticketOffset = offsetWeight(count, wc)
      val diffCalc = pow((nodeOffset - ticketOffset),2)
      (word -> diffCalc)
    }
    //finish euclidean distance calc on squared differences in the map
    val sumDiff = calcDiff.reduceLeft((x,y) => ("result",x._2 + y._2))
    sqrt(sumDiff._2)
  }

  private def compareDistance(t1:Tuple2[Node,Double],f2:Future[Tuple2[Node,Double]]):Tuple2[Node,Double] = {
    val t2 = f2()
    if( t1._2 > t2._2 ) t1
    else t2
  }

}
