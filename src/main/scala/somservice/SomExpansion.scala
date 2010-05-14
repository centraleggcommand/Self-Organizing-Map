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
	  //ids of all position maps
/*
       	  val levelIds = for( (mId,_,_) <- levelData) yield { mId }
        //ids of all maps that are a parent to another map
          val pLevelIds = for( (_,pMapId,_) <- levelData) yield {pMapId}
        //ids of nodes that are parent to a map - the actual expansion
        //source of a map.
          val allpNodeIds = for( List(_,_,("value",pNId:String)) <- levelData) yield {pNId}
	  */
	  parentNodeIds = levelData.getSrcNodeIds
	  //the map ids that aren't parents to another map are leaf maps
	  //get the parent node ids of the leaf maps
          leafParentNodes = levelData.getLeafSrcNodeIds
/*
	  leafParentNodes = diffList match {
	    case Nil => {
	      logger.info("checkExpansion got an empty leaf map list")
	      List()
	    }
	    case leafMapIds:List[_] => {
	      for{ mapId <- leafMapIds
		   data <- levelData
		   if{ data match {
		     case List(("id",mId),_,_) => mapId == mId
		     case _ => false
		   }} 
		 } yield {
		   data match {
		     case List(_,_,("value",pNodeId:String)) => pNodeId
		     case _ => ""
		   }
		 }
	    }
	  }
	  */ 
	  logger.debug("Leaf map parent nodes:" + leafParentNodes)
	}
  }
}

class SomExpansion(dbAgent:SomDbAgent)
{
  val initErr_c = 2.5
  val hzMin_c = 4.0
  val expPercent = 0.75
  val logger = Logger.getLogger("somservice.SomExpansion")
  PropertyConfigurator.configure("log4j.properties")

  def cleanup = dbAgent.shutdown

  def checkExpansion(winnerNodeId:String):Unit = {
    //get data about all the map levels

/*
      val allLevels:Option[List[Any]] = dbAgent.getPositionMapTree
      allLevels match {
	case None => logger.info("checkExpansion got no map data")
	case Some(levelData:List[_]) => {
	  //ids of all position maps
	  val levelIds = for( List(("id",mId:String),_,_) <- levelData) yield { mId }
        /*
        val levelIds = for{ data <- levelData
                            (tag,id) <- data
                            if tag == "id" } yield { id }
                            */
        //ids of all maps that are a parent to another map
          val pLevelIds = for( List(_,("key",pMapId),_) <- levelData) yield {pMapId}
        /*
        val pLevelIds = for{ data:List[Any] <- levelData
                            (tag,id) <- data
                            if tag == "key" } yield { id }
                            */
        //ids of nodes that are parent to a map - the actual expansion
        //source of a map.
          val allpNodeIds = for( List(_,_,("value",pNId:String)) <- levelData) yield {pNId}
        /*
        val allpNodeIds = for{ data:List[Any] <- levelData
                            (tag,id) <- data
                            if tag == "value" } yield { id }
                            */
	  val parentNodeIds = allpNodeIds.removeDuplicates
	  //the map ids that aren't parents to another map are leaf maps
	  val diffList = levelIds -- pLevelIds
	  logger.debug("Leaf maps: " + diffList)
	  //get the parent node ids of the leaf maps
	  val leafParentNodes = diffList match {
	    case Nil => {
	      logger.info("checkExpansion got an empty leaf map list")
	      List()
	    }
	    case leafMapIds:List[_] => {
	      for{ mapId <- leafMapIds
		   data <- levelData
		   if{ data match {
		     case List(("id",mId),_,_) => mapId == mId
		     case _ => false
		   }} 
		 } yield {
		   data match {
		     case List(_,_,("value",pNodeId)) => pNodeId
		     case _ => ""
		   }
		 }
	    }
	  }
	  logger.debug("Leaf map parent nodes:" + leafParentNodes)
	  */
          //The winner node is checked for vertical expansion.
          //Check if the node has already expanded 
       val mapData = new MapData( dbAgent)
       val leafParentNodes = mapData.getLeafParentIds
       val parentNodeIds = mapData.getParentNodeIds
          if( leafParentNodes.exists((x)=>x==winnerNodeId) ) {
            logger.error("An entry was added to a non-leaf node")
          }
          else {
            //The first level map is compared to a deviation const
            val parentId = dbAgent.getParentNode( winnerNodeId) match {
              case Some(id) => id
              case None => ""
            }
            if( parentId == dbAgent.getDbName) {
              val nodeDeviations = getNodeDeviations(parentId) match {
                case Some(dev) => dev
                case None => Nil
              }
              val size = nodeDeviations.length
              val avgMapError:Double = (nodeDeviations.foldLeft(0.0)((x,y)=>y._2 + x)) / size
              val winnerErr:Double = nodeDeviations.find({case (x,y) => {if(x.id == winnerNodeId) true else false}}) match {
                case Some((node,err)) => err
                case None => 1000.0
              }
              logger.debug("Vertical expansion calc: map error= " + avgMapError + " winnerErr= " + winnerErr)
              if( (avgMapError < initErr_c) && (winnerErr > avgMapError)) {
	      //grow another level for nodes that have more error
	        verticalExp(winnerNodeId)
              }
              else chkHorizontalExp( parentNodeIds)
            }
            //Non-first level nodes are compared against parent node
            else if( parentId != "") {
              val parentDeviation = dbAgent.getAvgNodeDeviation( parentId) match {
                case Some(num:Double) => num
                case None => 0.1 //this prevents expansion
              }
              val winnerDeviation = dbAgent.getAvgNodeDeviation( winnerNodeId) match {
                case Some(num:Double) => num
                case None => 1000.0 //this prevents expansion
              }
              if( (winnerDeviation/parentDeviation) < expPercent) {
                verticalExp(winnerNodeId)
              }
              else chkHorizontalExp( parentNodeIds)
            }
          }
//	}
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

  private def chkHorizontalExp( parentNodeIds:List[String]) = {
  //Go through all parent nodes for the maps and obtain child nodes. 
    for( nodeId <- parentNodeIds) {
      expandLevel( nodeId)
    }
  }

  private def expandLevel( parentNodeId:String):Unit = {
    try {

    dbAgent.getTally match {
      case Some(tally) => if( tally > hzMin_c ) {
        logger.debug("Attempting horizontal expansion")
        val nodeDeviations = getNodeDeviations(parentNodeId) match {
          case Some(dev) => dev
          case None => Nil
        }
        val sortedNodeDev = nodeDeviations.sort((x,y) => x._2 < y._2)
        val errorNode = sortedNodeDev.last._1
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
            val distantNode = sortedNodes.last._1
            //Add a row or column betw error node and distant node
            val levelNodes = for( (node,num) <- nodeDeviations) yield node
            posMap.expand(errorNode, distantNode, levelNodes)
            //Reset the entry count for expansion
            dbAgent.updateTally(true)
          }
        }
      }
      case None => //do nothing
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
        case Some(data) => data
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
