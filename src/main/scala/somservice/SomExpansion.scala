package somservice

import textprocessing._
import org.apache.log4j.Logger
import org.apache.log4j.PropertyConfigurator

//This class obtains information about the map hierarchy.
class MapData( dbAgent:SomDbAgent)
{
  //List of ids for any node that has child nodes
  private var parentNodeIds:List[String] = Nil
  //List of ids for any node that has a child node that is a leaf node
  private var leafParentNodes:List[String] = Nil
  //accessor methods
  def getParentNodeIds:List[String] = parentNodeIds
  def getLeafParentIds:List[String] = leafParentNodes
  //constructor
  val logger = Logger.getLogger("somservice.SomExpansion.MapData")
  PropertyConfigurator.configure("log4j.properties")
  dbAgent.getPositionMapTree match {
    case None => logger.info("checkExpansion got no map data")
    case Some( levelData:PositionTree) => {
      parentNodeIds = levelData.getSrcNodeIds
      //the map ids that aren't parents to another map are leaf maps
      //get the parent node ids of the leaf maps
      leafParentNodes = levelData.getLeafSrcNodeIds
      logger.debug("Leaf map parent nodes:" + leafParentNodes)
    }
  }
}

class SomExpansion(dbAgent:SomDbAgent)
{
  val levelMin_c = 4.0 //min number of nodes to expand vertically
  val initScore_c = 2.0 //min score to expand horizontally
  val hzMin_c = 1.5 //min number of entries per node
  val expPercent_c = 1.2 //percent increase in score to expand vertically
  val logger = Logger.getLogger("somservice.SomExpansion")
  PropertyConfigurator.configure("log4j.properties")

  def cleanup = dbAgent.shutdown

  def checkExpansion(winnerNodeId:String):Unit = {
    //get data about all the map levels

       //The winner node is checked for vertical expansion.
       //Check that the winner is a leaf node.
       val mapData = new MapData( dbAgent)
       val leafParentNodes = mapData.getLeafParentIds
       val parentNodeId = dbAgent.getParentNode( winnerNodeId) match {
         case Some(id:String) => id
         case None => ""
       }
       if( leafParentNodes.exists((x)=>x==winnerNodeId) ) {
         logger.error("An entry was added to a non-leaf node")
       }
       else {
         if( dbAgent.getChildDocNum(winnerNodeId) match {
                case Some(num:Double) => if( num < levelMin_c) true else false
                case None => true } ) {
           //Don't vertically expand if a node has less than 4 entries
           chkHorizontalExp( parentNodeId)
         }
         else {   
            val parentId = dbAgent.getParentNode( winnerNodeId) match {
              case Some(id) => id
              case None => ""
            }
            //The first level map is compared to a deviation const
            if( parentId == dbAgent.getDbName) {
              val nodeDeviations = getNodeDeviations(parentId) match {
                case Some(dev) => dev
                case None => Nil
              }
              val size = nodeDeviations.length
              val avgMapScore:Double = (nodeDeviations.foldLeft(0.0)((x,y)=>y._2 + x)) / size
              val winnerScore:Double = nodeDeviations.find({case (x,y) => {if(x.id == winnerNodeId) true else false}}) match {
                case Some((node,err)) => err
                case None => 0.0
              }
              logger.debug("Vertical expansion calc: map score= " + avgMapScore + ", winnerScore= " + winnerScore)
              if( (avgMapScore >= initScore_c) && (winnerScore > 0) ) {
	        verticalExp(winnerNodeId)
              }
              else chkHorizontalExp( parentNodeId)
            }
            //Non-first level nodes are compared against parent node
            else if( parentId != "") {
              dbAgent.getAvgNodeDeviation( parentId) match {
                case Some(parentScore:Double) => {
                  val winnerScore = dbAgent.getAvgNodeDeviation( winnerNodeId) match {
                    case Some(n:Double) => n
                    case None => 0.0
                  }
                  //winner node must have increase in score by expPercent_c
                  if( (winnerScore/parentScore) > expPercent_c) {
                    verticalExp(winnerNodeId)
                  }
                  else chkHorizontalExp( parentNodeId)
		}
                case None => //do nothing
             }
           }
	}
       }
//	case _ => logger.info("checkExpansion could not recognize data")
//      }
  }

  private def getNodeDeviations( parent:String):Option[List[Tuple2[Node,Double]]] = {              
    dbAgent.getNodesUsingParent( parent) match {
      case None => {
        logger.debug("Map has no nodes for expansion")
        None
      }
      case Some(levelNodes) => {
	//calculate the avg deviation of entries belonging to each node
        val allNodeAvgs = for( node <- levelNodes) yield {
          dbAgent.getAvgNodeDeviation(node.id) match {
	    case None => (node, 0.0)
	    case Some(avg:Double) => (node, avg)
	  }
	}
	logger.debug("Node deviations: " + allNodeAvgs)
        Some(allNodeAvgs)
      }
    }
  }

  private def chkHorizontalExp( parentNodeId:String) = {
  //Check the number of entry insertions for this map since last expansion.
  //This number should be greater than some constant multiplied by
  //the number of nodes.
    val numNodes = dbAgent.getNodesUsingParent(parentNodeId) match {
      case Some(levelNodes) => levelNodes.length
      case None => 0
    }
    if( numNodes > 0) {
      dbAgent.getTally(parentNodeId) match {
        case Some(tally:Double) => if( tally > (hzMin_c*numNodes) ) expandLevel( parentNodeId)
                            else logger.debug("Not horizontally expanding with tally: " + tally)
        case None => logger.debug("Not horizontally expanding - no tally")
      }
    }
  }

  private def expandLevel( parentNodeId:String):Unit = {
    try {

         logger.debug("Attempting horizontal expansion")
        val nodeDeviations = getNodeDeviations(parentNodeId) match {
          case Some(dev) => dev
          case None => Nil
        }
        val sortedNodeDev = nodeDeviations.sort((x,y) => x._2 < y._2)
        val errorNode = sortedNodeDev.head._1 //lowest scoring node
        val posMap = new MapPosition(dbAgent, parentNodeId)
        posMap.getNeighbors(errorNode.id) match {
          case None =>
          case Some(neighbors:List[_]) => {
            //Match ids with nodes and calc distance from error node
            //Yield a tuple with node id and distance
            val nodeDistances = for( nId <- neighbors) yield {
              nodeDeviations.find((x)=>x._1.id==nId) match {
                case None => {
                  logger.error("Neighbor node id was not found in list of nodes")
                  (null,1000.0) //arbitrary large distance
                }
                case Some((nNode:Node,_)) => (nNode, errorNode.calcNodeDistance( nNode.weight, dbAgent))
              }
            }
            val sortedNodes = nodeDistances.sort((x:Tuple2[_,Double],y:Tuple2[_,Double]) => x._2 < y._2)
            val distantNode = sortedNodes.head._1
            //Add a row or column betw error node and distant node
            val levelNodes = for( (node,num) <- nodeDeviations) yield node
            posMap.expand(errorNode, distantNode, levelNodes)
            //Reset the entry count for expansion
            dbAgent.updateTally(parentNodeId,true)
          }
        }
 
  } catch {
    case ex: RuntimeException => logger.error("Error in chkHorizontalExp: " + ex)
  }
  }

  private def verticalExp(winnerNodeId:String):Unit = {
    //Create a new position map
    if(!dbAgent.addPositionDoc(winnerNodeId)) logger.error("Could not add a new position doc for vertical map expansion")
    else {
      //Perform insertion of each entry belonging to the winner node into
      //new nodes in a new map.
      val entryData = dbAgent.getEntries(winnerNodeId) match {
        case Some(data:NodeEntry) => data.getEntryData
        case None => Nil
      }
      for( (id,subject:String,content:String) <- entryData) {
        val stemmedList = DocProcessor.docToList(content)
        val entryInfo = new BasicContent( dbAgent.getDbName, stemmedList, subject, content)
        val insertRequest = new SomInsertion( entryInfo)
        logger.debug("Attempting to insert into vert exp level")
        insertRequest.insertAtLevel(winnerNodeId)
        insertRequest.cleanup
      }
      //The copy of the original entry is still in existence to provide
      //deviation stats to node, ... other reasons?
    }
  }

}
