import textprocessing._
import somservice._

object SomTestDb {

  def main(args: Array[String]) {
    //create a database
    if(args.length > 0) {

      try {
        val dbagent = new CouchAgent(args(0))
        println(dbagent.createSom)
        //setup data
        val mycontent = "On a rainy day, there is always the chill and gloom that hangs over our heads. But there is also the hopeful expectation that one might see a rainbow. Then all the clouds may pass."
        val inputList = List.fromString(mycontent,' ')
        val filteredList = StopWords.removeStopwords(inputList)
        var stemmer = new Stemmer()
        val stemmedList = filteredList.map(stemmer.stemWord(_))
        //perform insertion
        val myentry = new BasicContent(args(0),stemmedList,mycontent)
        val insert = new SomInsertion(myentry)
        insert.insertEntry

      } catch {
          case e:RuntimeException => println(e.toString) }

    }
  }
}
