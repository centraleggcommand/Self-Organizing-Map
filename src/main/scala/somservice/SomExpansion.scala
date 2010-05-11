package somservice
import org.apache.log4j.Logger
import org.apache.log4j.PropertyConfigurator

class SomExpansion(dbAgent:SomDbAgent)
{
  val errorMin_c = 2.0
  val hzMin_c = 8.0
  val expPercent = 1.3
  val logger = Logger.getLogger("somservice.SomExpansion")
  PropertyConfigurator.configure("log4j.properties")

  def cleanup = dbAgent.shutdown

  def checkExpansion(winnerNodeId:String):Unit = {
    //get data about all the map levels
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
          val allpNodeIds = for( List(_,_,("value",pNId)) <- levelData) yield {pNId}
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
	  //Go through all parent nodes for the maps and obtain child nodes. 
	  //All maps checked for horizontal and vertical expansion.
	  for( nodeId <- parentNodeIds) {
	    nodeId match {
	      case nId:String => {
		dbAgent.getNodesUsingParent( nId) match {
		  case None => logger.debug("Map has no nodes for expansion")
		  case Some(levelNodes) => {
		//calculate the avg deviation of entries belonging to each node
		    val allNodeAvgs = for( node <- levelNodes) yield {
		      dbAgent.getAvgNodeDeviation(node.id) match {
			case None => (node, 0.0)
			case Some(avg:Double) => (node, avg)
		      }
		    }
		    logger.debug("Node deviations: " + allNodeAvgs)
		    //check if a node has already vertically expanded
                    if( !leafParentNodes.exists((x)=>x==winnerNodeId) ) {
		    //First compare the average deviation among nodes in this
                    //map with winner node for recently added entry. If the
                    //winner node's deviation is a percentage 'expPercent'
                    //of the avg deviation, then vertically expand.
                    //Otherwise, horizontally expand.
                      val size = allNodeAvgs.length
                      val avgMapError:Double = (allNodeAvgs.foldLeft(0.0)((x,y)=>y._2 + x)) / size
                      val winnerErr:Double = allNodeAvgs.find({case (x,y) => {if(x.id == winnerNodeId) true else false}}) match {
                        case Some((node,err)) => err
                        case None => 1000.0
                      }
                      if( (avgMapError/winnerErr) < expPercent) {
		        //grow another level
		        verticalExp(winnerNodeId)
                      }
                      else chkHorizontalExp(allNodeAvgs, nId)
                    }
                    else chkHorizontalExp(allNodeAvgs, nId)
		  }
		}
	      }
	      case _ => logger.info("a non-string id found")
	    }
	  }
	}
	case _ => logger.info("checkExpansion could not recognize data")
      }
  }


  private def chkHorizontalExp(nodeDeviations:List[Tuple2[Node,Double]],
                               parentNodeId:String):Unit = {
    try {

    dbAgent.getTally match {
      case Some(tally) => if( tally > hzMin_c ) {
        logger.debug("Attempting horizontal expansion")
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
    //Create a new position map, create initial nodes, add entries
    if(!dbAgent.addPositionDoc(winnerNodeId)) logger.error("Could not add a new position doc for vertical map expansion")
    else {
      
      println("vert-exp not done")
    }
  }

}
