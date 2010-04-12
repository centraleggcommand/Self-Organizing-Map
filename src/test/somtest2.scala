import textprocessing._
import somservice._

object SomTest2 {

  def main(args: Array[String]) {
    //create a database
    if(args.length > 0) {

      val dbagent = new CouchAgent(args(0))
      try {
        dbagent.createSom
        val expand = new SomExpansion(dbagent)
        expand.checkExpansion
      } catch {
          case e:RuntimeException => println(e.toString) 
          dbagent.shutdown
        }

    }
    else println("Two args not received")
  }
}
