package somservice

import scala.collection.mutable.ListBuffer

abstract class ListType

case class PositionData( id:String,grid:List[_]) extends ListType
{
  private val gridList = new ListBuffer[List[String]]
  private val gridArray = new Array[Array[String]](grid.length)
  //constructor
  for( row <- grid) {
    var counter = 0 //used to keep track of index for insert into gridArray
    row match {
      case myrow:List[_] => {
        val tmpList = new ListBuffer[String]
        for(rowItem <- myrow) {
          rowItem match {
            case item:String => tmpList += item
            case _ => //do nothing
          }
        }
        if( tmpList.length > 0 ) {
          gridList += tmpList.toList
          gridArray(counter) = tmpList.toArray
          counter = counter + 1
        }
      }
    }
  }

  def getGridList = gridList.toList
  def getGridArray = gridArray

}

case class PositionTree( mapData:List[Tuple3[String,String,String]] ) extends ListType
{
  def getSrcNodeIds = {
    val nodeSrc = for( (_,_,pNId:String) <- mapData) yield pNId
    //the same source node id can be referenced by multiple maps
    nodeSrc.removeDuplicates
  }

  def getLeafSrcNodeIds = {
    val mapIds:List[String] = for( (mId:String,_,_) <- mapData) yield mId
    val parentMapIds:List[String] = for( (_,pMapId:String,_) <- mapData) yield pMapId
    val leafIds = mapIds -- parentMapIds
    for{ mId:String <- leafIds
         (id:String,_,pNodeId) <- mapData
         if id == mId
       } yield pNodeId
  }
    
}
case class NodeEntry( entryData:List[Tuple3[String,String,String]] ) extends ListType
{
  def getEntryData:List[Tuple3[String,String,String]] = entryData
  def getEntrySubjects:List[String] = for ((id,subj,content) <- entryData) yield {subj}
}
