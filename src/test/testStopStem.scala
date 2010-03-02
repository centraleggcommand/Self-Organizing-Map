import scala.io.Source
import textprocessing.StopWords
import textprocessing.Stemmer

object testFilter
{

  def main(args: Array[String]) 
  {

    if( args.length > 0 ) {
      val source = Source.fromFile(args(0))
      var stemmer = new Stemmer()

      val choppedList = for( line <- source.getLines)
                          yield {
                          StopWords.removeStopwords(List.fromString(line.trim,' '))
                          }
      //convert from a list of lists to just a list of strings
      val filteredList = choppedList.reduceLeft(_ ::: _)
      val stemmedList = filteredList.map(stemmer.stemWord(_))
      for( item <- stemmedList) {
        print(item) }
    }
    else
      println("Please supply the name of a file to parse")
  }
}

