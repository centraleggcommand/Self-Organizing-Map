package somservice

class MapPosition(dbAgent:SomDbAgent, parent:String) {

  //The map position is saved with rows in descending order
  //and columns in descending order because it better suits
  //a list implementation.
  def addInitNodePosition(nodeId:String):Unit = {
    dbAgent.getPositionDoc(parent) match {
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
      case Some(data) => println("addStartPosition - " + data)
      case None => println("addStartPosition could not retrieve map")
    }
  } 

}
