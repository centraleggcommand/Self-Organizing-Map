package somservice

abstract class ListType
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
