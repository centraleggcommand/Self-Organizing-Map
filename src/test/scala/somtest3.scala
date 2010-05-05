import textprocessing._
import somservice._

object SomTest3 {

  def main(args: Array[String]) {
    //create a database
    if(args.length > 0) {

      val dbagent = new CouchAgent(args(0))
      try {
        dbagent.createSom
        dbagent.updateTally(false)
        dbagent.updateTally(false)
        dbagent.updateTally(false)
      } catch {
          case e:RuntimeException => println(e.toString) 
        }
        finally {
          dbagent.shutdown
        }

    }
    else println("Need to have command line arg for db name")
  }
}
