package somservice
import org.apache.log4j.Logger
import org.apache.log4j.PropertyConfigurator
import scala.collection.mutable
import scala.Iterable

case class MapPosition(dbAgent:SomDbAgent, parent:String) {

  val logger = Logger.getLogger("somservice.MapPosition")
  PropertyConfigurator.configure("log4j.properties")

  val positionData = dbAgent.getPositionDoc(parent) match {
    case None => {
      logger.info("The pos map gridArray was not found")
      PositionData("",Nil)
    }
    case Some(pd:PositionData) => pd
  }

  val gridList:List[List[String]] = positionData.getGridList
  val gridArray:Array[Array[String]] = {
    //ListBuffer type being used to convert lists to arrays
    //because the toArray method of regular List type returns Array[Any]
    val tmpgrid = new mutable.ListBuffer[Array[String]]
    for( row <- gridList) {
      val tmprow = new mutable.ListBuffer[String]
      for( item:String <- row) {
        tmprow += item
      }
      tmpgrid += tmprow.toArray
    }
    tmpgrid.toArray
  }


  //The map position is saved with rows in descending order
  //and columns in descending order because it better suits
  //a list implementation.
  def addInitNodePosition(nodeId:String):Unit = {
    if( gridList.length == 0) {
      dbAgent.updatePositionDoc(positionData.id,List(List(nodeId)))
    }
    else {
      
/*
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
	  */
        //val gridArray = positionData.getGridArray
        val modGrid:List[List[String]] = {
          //Match length of all rows
          val topLength = gridList.last.length
          var modified = false
          if( gridList.head.length < topLength) {
            //what type is row seen as?
            for( row <- gridList) yield {
              if( !modified && row.length < topLength) {
                modified = true
                //change array to list to make use of list constructor
                nodeId::row
              }
              else row
            }
	  }
          //Add a row if I have a rectangle with more columns than rows
          else if( gridList.length < topLength) {
            //Need to transform list[Any] to list[list]
/*
            val listoflist = for( row <- posGrid) yield {
              row match {
                case r:List[_] => r
                case _ => throw new RuntimeException("position grid is not a list of lists?")
              }
            }
            List(nodeId)::listoflist
	    */
            List(nodeId)::gridList
          }
          //Start another column
          else {
            val revGrid = gridList.reverse
            val modRow = nodeId::(revGrid.head)
            val cutRevGrid = revGrid.tail
            val modRevGrid = modRow::cutRevGrid
            modRevGrid.reverse
          }
        }
        //Update the database
        dbAgent.updatePositionDoc(positionData.id,modGrid)
      }
//      case Some(data) => logger.info("addStartPosition - " + data)
//      case None => logger.info("addStartPosition could not retrieve map")
//    }
  } 

  //This fxn returns the list of neighbor ids
  def getNeighbors(nodeId:String):Option[List[Any]] = {
    //val gridArray = positionData.getGridArray
    if( gridList.isEmpty) {
      logger.info("getNeighbors has an empty gridList")
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
      case None => logger.error("Position data not found for node: " + errNode.id)
      case Some((eRow,eCol)) => {
        neighborPos match {
          case None => logger.error("Position data not found for node: " + neighbor.id)
          case Some((nRow,nCol)) => {
            if( eRow != nRow) {
              //Add row between error node and neighbor
              insertRow( eRow, nRow, levelNodes)
            }
            else {
              //Add column between error node and neighbor
              insertCol( nCol, eCol, levelNodes)
            }
          }
        }
      }
    }
  }

  //find the row and col that contains the target node id - start from zero
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
  private def insertRow( row1:Int, row2:Int, levelNodes:List[Node] ):Unit = {
    //Create array to hold new node ids for the new row in the map
    try {
    val top = if(row1 > row2) row1 else row2
    val bot = if(row1 > row2) row2 else row1 
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
    val gridA = arrList.take(bot+1)
    val gridB = arrList.drop(bot+1)
    val gridList = gridA:::(iRow.toList)::gridB
    dbAgent.updatePositionDoc( positionData.id, gridList)
    logger.debug("Updating pos map with new row")

  } catch { 
    case ex:Exception => println("insert row: " + ex.toString)
  }
  }

  private def insertCol( col1:Int, col2:Int, levelNodes:List[Node] ):Unit = {
    //Each row in the position map needs to have a new element added.
    //The new element is added between 'rt' and 'lf' parameters.
    try {
    logger.debug("Inserting a new column")
    val rt = if(col1 > col2) col1 else col2
    val lf = if(col1 > col2) col2 else col1
    val arrayOfLists = for( row <- gridArray ) yield {
      //Create a new node using weights from rt an lf neighbor.
      val n1 = levelNodes.find((x)=>{x.id==row(rt)})
      val n2 = levelNodes.find((x)=>{x.id==row(lf)})
      val nId = n1 match {
        case None => { //no rt neighbor, use only the lf neighbor weight
          n2 match {
            case None => {
              logger.error("insertCol could not find neighbors")
              dbAgent.addInitNode(parent, null)
	    }
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
        case Some(id) => {
          logger.debug("added column node: " + id)
          val freshrow = (row.take(lf+1).toList):::id::(row.drop(lf+1)).toList
          freshrow
	}
        case None => {
          logger.error("Expansion node for new column could not be created")
          row.toList
        }
      }
    }
    dbAgent.updatePositionDoc( positionData.id, arrayOfLists.toList)
    logger.debug("Updating pos map with new column: " + arrayOfLists)

  } catch {
    case ex:Exception => println("insert col: " + ex.toString)
  }
  }

  private def meldWeights(w1:Map[String,Double], w2:Map[String,Double]):Map[String,Double] = {
    //Meld the weights of two nodes
    //Fxn to combine values if a key also exists in w2
    logger.debug("Attempting to meld weights for expansion node")
    val combine = (k:String,v:Double) => { 
      if(w2.contains(k)) (v + w2(k))/2
      else v
    }
    val cmbWeight = w1.transform(combine)
    val cmbIntersect = cmbWeight.filter((ele) => w2.contains(ele._1))
    //take half of the unique keys from each maph
    val w1Unique = w1.filter((ele) => !cmbIntersect.contains(ele._1))
    val w2Unique = w2.filter((ele) => !cmbIntersect.contains(ele._1))
    val w1UniqueCut = w1Unique.toList.drop(w1Unique.size/2)
    val w2UniqueCut = w2Unique.toList.drop(w2Unique.size/2)
    cmbIntersect ++ w1UniqueCut ++ w2UniqueCut
  }

}
