import textprocessing._
import somservice._
import scala.collection.mutable.ListBuffer

object SomTest1 {

  def main(args: Array[String]) {
    //create a database
    if(args.length > 0) {

      val dbagent = new CouchAgent(args(0))
      try {
        dbagent.createSom
        //setup data
        val inputList = new ListBuffer[String]
        val input = if( args.length > 1 ) { //List.fromString(args(1),' ')
          //use regex
          val noPunc = """\s*\W*([\w\d_-]*)\W*\s*""".r
          for( noPunc(word) <- noPunc findAllIn args(1) )  inputList += word
          inputList.toList
          }
          else Nil
        println("Regex output: " + input)
        val filteredList = StopWords.removeStopwords(input)
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
