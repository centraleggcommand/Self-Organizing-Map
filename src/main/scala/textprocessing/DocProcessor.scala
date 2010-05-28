package textprocessing

import scala.collection.mutable.ListBuffer

//This takes in a string rep of a doc, and performs a series filtering
//to result in a list of words.

object DocProcessor {

  def docToList(doc:String):List[String] = {
    val input = new ListBuffer[String]
    val noPunc = """\s*\W*([\w\d\'_-]+)\W*\s*""".r
    for( noPunc(word) <- noPunc findAllIn doc) input += word
    //for some reason, the pattern match is letting empty words through?
    val inputList = input.toList.map( _.toLowerCase )
    val filteredList = StopWords.removeStopwords(inputList)
    val stemmer = new Stemmer()
    filteredList.map(stemmer.stemWord(_))
  }

}
