package textprocessing
// Description: The ticket obj takes in a text doc and converts it into
// an a list of tuples containing word and word frequency info.

class InputTicket( doc:List[String]) {
  
  val wordMap = parseList
  //create a representation of the doc's word usage
  private def parseList:Map[String,Double] = {
    //defining a helper fxn 
    def createMap(remList:List[String],incMap:Map[String,Double]):Map[String,Double] = {
      remList match {
        case Nil => incMap
        case (hd::rest) => {
          incMap.get(hd) match {
            case Some(num) => createMap(rest, incMap.update(hd,num+1))
            case None => createMap(rest, (incMap + (hd -> 1.0)))
          }
        }
      }
    }
    createMap(doc,Map.empty[String,Double])
  }
// ** a head recursive implementation that is not as memory efficient
//    wordList match {
      //a one element map
//      case (hd::Nil) => Map(hd -> 1)
      //compare an element to all other elements in the list that follow it
//      case (hd::rest) => { 
//        val wordMap = parseList(rest)
//        wordMap.get(hd) match {
//          case Some(num) => wordMap.update(hd,num+1)
//          case None => wordMap + (hd -> 1)
//        }
//      }
//      case _ => Map.empty[String,Int]
//    }
//  }

}
