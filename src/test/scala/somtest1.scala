import textprocessing._
import somservice._

object SomTest1 {

  def main(args: Array[String]) {
    //create a database
    if(args.length > 0) {

      val dbagent = new CouchAgent(args(0))
      try {
        dbagent.createSom
        //setup data
        /*
        val mycontent = args.toList.tail
        println("inserting: " + mycontent)
        def convToStrList(rem:List[_],res:List[String]):List[String] = {
          rem match {
            case Nil => res.reverse
            case myrem => {
              val item = myrem.head match { 
                case i:String => i
                case unknown => unknown.toString
              }
              convToStrList(myrem.tail, item::res)
            }
          }
        }
        val inputList = convToStrList(mycontent,Nil)
*/

        //val mycontent = "In the spring, the flowers bloom all over the hillside"
        val inputList = if( args.length > 1 ) List.fromString(args(1),' ')
                        else Nil
        val filteredList = StopWords.removeStopwords(inputList)
        var stemmer = new Stemmer()
        val stemmedList = filteredList.map(stemmer.stemWord(_))
        //perform insertion
        val myentry = new BasicContent(args(0),stemmedList, args(1))
        val insert = new SomInsertion(myentry)
        insert.insertEntry
        //dbagent.getNodeMap("0d2e39dfa11374c2c98a7f001df5ab35")
        dbagent.shutdown
      } catch {
          case e:RuntimeException => println(e.toString) 
          case ex:Exception => println(ex.toString)
          dbagent.shutdown
        }

    }
  }
}
