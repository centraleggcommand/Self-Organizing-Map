package somservice
import org.apache.log4j.Logger
import org.apache.log4j.PropertyConfigurator
import scala.collection.mutable
import scala.Iterable

class MapPosition(dbAgent:SomDbAgent, parent:String) {

  val logger = Logger.getLogger("somservice.MapPosition")
  PropertyConfigurator.configure("log4j.properties")

  val positionDoc = dbAgent.getPositionDoc(parent)

  val gridArray = positionDoc match {
    case None => {
      logger.info("The pos map gridArray was not found")
      new Array[Array[Any]](0)
    }
    case Some((mapId:String,Nil)) => {
      logger.debug("The pos map gridArray is empty")
      new Array[Array[Any]](0)
    }
    case Some((mapId:String,posGrid:List[_])) => {
      //change the list of lists into an array of arrays
      val myArray = new Array[Array[Any]](posGrid.length)
      var counter = 0
      for( row <- posGrid) {
        row match {
          case myrow:List[_] => {
            myArray(counter) = myrow.toArray
            counter = counter + 1
          }
          case _ => 
        }
      }
      myArray
    }
  }

  //The map position is saved with rows in descending order
  //and columns in descending order because it better suits
  //a list implementation.
  def addInitNodePosition(nodeId:String):Unit = {
    positionDoc match {
      case Some((mapId:String,Nil)) => {
        dbAgent.updatePositionDoc(mapId,List(List(nodeId)))
      }
      case Some((mapId:String,posGrid:List[_])) => {
        //Try to keep close to a square layout, as the number of initial
        //nodes desired is unknown to this fxn.
        //Change posGrid type to a list of lists
        val gridList = for( g <- posGrid) yield {
          g match {
            case glist:List[_] => glist
            case _ => throw new RuntimeException("position grid is not a    list of lists?")
          }
        }
        val modGrid = {
          //Match length of all rows
          val topLength = gridList.last.length
          var modified = false
          if( gridList.head.length < topLength) {
            for( row <- posGrid) yield {
              row match {
                case r:List[_] => {
                  if( !modified && r.length < topLength) {
                    modified = true
                    nodeId::r
                  }
                  else row
                }
                case _ => throw new RuntimeException("position grid is not a list of lists?")
              }
            }
          }
          //Add a row if there are more columns
          else if( posGrid.length < topLength) {
            //Need to transform list[Any] to list[list]
            val listoflist = for( row <- posGrid) yield {
              row match {
                case r:List[_] => r
                case _ => throw new RuntimeException("position grid is not a list of lists?")
              }
            }
            List(nodeId)::listoflist
          }
          //Add a column
          else {
            val revGrid = gridList.reverse
            val modRow = nodeId::(revGrid.head)
            val cutRevGrid:List[_] = revGrid.tail
            val modRevGrid = modRow::cutRevGrid
            modRevGrid.reverse
          }
        }
        //Update the database
        dbAgent.updatePositionDoc(mapId,modGrid)
      }
      case Some(data) => logger.info("addStartPosition - " + data)
      case None => logger.info("addStartPosition could not retrieve map")
    }
  } 

  //This fxn returns the list of neighbor ids
  def getNeighbors(nodeId:String):Option[List[Any]] = {
    if( gridArray.isEmpty) {
      logger.info("getNeighbors has an empty gridArray")
      None
    }
    else {
      findPos(nodeId) match {
        case None => {
          logger.info("getNeighbors could not find the node id in map")
          None
        }
        case Some((row,col)) => {
          //check for top, bottom, left, right neighbors
          val res1 = { if( (row-1) >= 0) List(gridArray(row-1)(col))
                     else Nil }
          val res2 = { if( (row+1) < gridArray.length) gridArray(row+1)(col)::res1
                     else res1}
          val res3 = { if( (col-1) >= 0) gridArray(row)(col-1)::res2
                     else res2 }
          val result = if( (col+1) < gridArray(row).length) {
                         gridArray(row)(col+1)::res3 }
                       else res3
          if( result.length > 0) Some(result)
          else {
            logger.info("No neighbors were found for node - should not have reached this fxn")
            None
          }
        }
      }
    }
  }

  def expand(errNode:Node, neighbor:Node, levelNodes:List[Node]):Unit = {
    val errPos = findPos( errNode.id)
    val neighborPos = findPos( neighbor.id)
    errPos match {
      case None => //do nothing
      case Some((eRow,eCol)) => {
        neighborPos match {
          case None => //do nothing
          case Some((nRow,nCol)) => {
            if( eRow < nRow) {
              //Add row below error node
              insertRow( eRow, nRow, levelNodes)
            }
            else if( eRow > nRow) {
              //Add row above error node
              insertRow( nRow, eRow, levelNodes)
            }
            else if( eCol < nCol) {
              //Add column to right of error node
              insertCol( eCol, nCol, levelNodes)
            }
            else {
              //Add column to left of error node
              insertCol( nCol, eCol, levelNodes)
            }
          }
        }
      }
    }
  }

  //find the row that contains the target node id
  private def findPos(nId:String):Option[Tuple2[Int,Int]] = {
    def chkElement(row:Int, col:Int):Option[Tuple2[Int,Int]] = {
      //check boundary
      if( row >= gridArray.length) None
      else {
        //check boundary
        if( col >= gridArray(row).length) chkElement(row+1, 0)
        //do comparison
        else {
          if( gridArray(row)(col) == nId) Some((row,col))
          else chkElement(row,col+1)
        }
      }
    }
    chkElement(0,0)
  }

  //Insert a new row betw top and bot rows
  private def insertRow( top:Int, bot:Int, levelNodes:List[Node] ):Unit = {
    //Create array to hold new node ids for the new row in the map
    try {
    val iRow = new Array[String](gridArray(0).length)
    //Create nodes in db
    for( col <- 0 to iRow.length-1) {
      val n1 = levelNodes.find((x)=>{x.id==gridArray(top)(col)})
      val n2 = levelNodes.find((x)=>{x.id==gridArray(bot)(col)})
      val nId = n1 match {
        case None => {
          n2 match {
            case None => dbAgent.addInitNode(parent, null)
            case Some(node2) => dbAgent.addInitNode(parent, node2.weight)
          }
        }
        case Some(node1) => {
          n2 match {
            case None => dbAgent.addInitNode(parent, node1.weight)
            case Some(node2) => dbAgent.addInitNode(parent, meldWeights(node1.weight, node2.weight))
          }
        }
      }
      nId match {
        case None => logger.info("insertRow could not add the node to db")
        case Some(id) => iRow(col) = id
      }
    }
    logger.debug("Inserting new row: " + iRow)
    //update the position map
    //using a list to conform with JsObject conversion fxn
    //change to array of lists
    val arrList = gridArray.map(_.toList).toList
    //insert new row
    val gridA = arrList.slice(0,bot)
    val gridB = arrList.drop(bot)
    val gridList = gridA:::(iRow.toList)::gridB
    positionDoc match {
      case None => {
        logger.info("getNeighbors could not obtain pos map")
      }
      case Some((mId:String, _)) => {
        dbAgent.updatePositionDoc( mId, gridList)
        logger.debug("Updating pos map with new row")
      }
    }
  } catch { 
    case ex:Exception => println("insert row: " + ex.toString)
  }
  }

  private def insertCol( rt:Int, lf:Int, levelNodes:List[Node] ):Unit = {
    //Each row in the position map needs to have a new element added
    try {
    logger.debug("Inserting a new column")
    val arrayOfLists = for( row <- gridArray ) yield {
      //Create a new node
      val n1 = levelNodes.find((x)=>{x.id==row(rt)})
      val n2 = levelNodes.find((x)=>{x.id==row(lf)})
      val nId = n1 match {
        case None => {
          n2 match {
            case None => dbAgent.addInitNode(parent, null)
            case Some(node2) => dbAgent.addInitNode(parent, node2.weight)
          }
        }
        case Some(node1) => {
          n2 match {
            case None => dbAgent.addInitNode(parent, node1.weight)
            case Some(node2) => dbAgent.addInitNode(parent, meldWeights(node1.weight, node2.weight))
          }
        }
      }
      //Create updated position map row
      nId match {
        case Some(id) => (row.slice(0,lf).toList):::id::(row.slice(lf,row.length-1)).toList
        case None => {
          logger.error("Expansion node for new column could not be created")
          row.toList
        }
      }
    }
    positionDoc match {
      case None => {
        logger.info("getNeighbors could not obtain pos map")
      }
      case Some((mId:String, _)) => {
        dbAgent.updatePositionDoc( mId, arrayOfLists.toList)
        logger.debug("Updating pos map with new column")
      }
    }
  } catch {
    case ex:Exception => println("insert col: " + ex.toString)
  }
  }

  private def meldWeights(w1:Map[String,Double], w2:Map[String,Double]):Map[String,Double] = {
    //Meld the weights of two nodes
    //Fxn to combine values if a key also exists in w2
    val combine = (k:String,v:Double) => { 
      if(w2.contains(k)) (v + w2(k))/2
      else v
    }
    val cmbWeight = w1.transform(combine)
    //add unique key/vals
    val w2Keys = w2.keys
    val tmpMap = mutable.Map.empty[String,Double]
    for( word <- w2Keys) {
      if( !cmbWeight.contains(word)) tmpMap += (word -> w2(word))
    }
    cmbWeight ++ tmpMap
  }

}
