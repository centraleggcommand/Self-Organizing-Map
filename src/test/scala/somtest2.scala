import textprocessing._
import somservice._

object SomTest2 {

  def main(args: Array[String]) {
    //create a database
    if(args.length > 0) {

      val dbagent = new CouchAgent(args(0))
      try {
        val expand = new SomExpansion(args(0))
        expand.checkExpansion
      } catch {
          case e:RuntimeException => println(e.toString) 
          dbagent.shutdown
        }

    }
    else println("Need to have command line arg for db name")
  }
}
