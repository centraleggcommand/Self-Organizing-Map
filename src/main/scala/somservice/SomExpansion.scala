package somservice
import org.apache.log4j.Logger
import org.apache.log4j.PropertyConfigurator

class SomExpansion(dbName:String)
{
  val dbAgent = new CouchAgent(dbName)
  val hzError_c = 2.0
  val hzMin_c = 2.0
  val logger = Logger.getLogger("somservice.SomExpansion")
  PropertyConfigurator.configure("log4j.properties")

  def cleanup = dbAgent.shutdown

  def checkExpansion:Unit = {
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
                  val sortedNodeAvgs = allNodeAvgs.sort((x,y) => x._2 < y._2)
                  logger.debug("Node deviations: " + sortedNodeAvgs)
                  chkHorizontalExp(sortedNodeAvgs, nId)
                  //fxn to check if a node has already vertically expanded
                  val prevExp = (nodeId:String) => leafParentNodes.exists((x)=>x==nodeId)
                  chkVerticalExp(sortedNodeAvgs, prevExp)
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

    val entryAmts = for( n <- nodeDeviations) yield {
      dbAgent.getChildDocNum(n._1.id) match {
        case None => 0.0
        case Some(num) => num
      }
    }
    val totalEntries = entryAmts.reduceLeft(_ + _)
    val size = nodeDeviations.length
    val avgMapError = (nodeDeviations.foldLeft(0.0)((x,y)=>y._2 + x)) / size
    logger.debug("Checking horizontal expansion for map with entry count: " + totalEntries + " / " + (size * hzMin_c) + " min required")
    logger.debug("Average map error: " + avgMapError)
    if( (avgMapError < hzError_c) && (totalEntries > (size * hzMin_c))) {
      logger.debug("Attempting horizontal expansion")
      val errorNode = nodeDeviations.last._1
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
        }
      }
    }

  } catch {
    case ex: RuntimeException => logger.error("Error in chkHorizontalExp: " + ex)
  }
  }

  private def chkVerticalExp(nodeDeviations:List[Tuple2[Node,Double]],
                             prevExpFxn:(String)=>Boolean ):Unit = {
    println("vert-exp not done")
  }

}
