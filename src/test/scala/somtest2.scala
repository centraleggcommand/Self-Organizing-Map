import textprocessing._
import somservice._

object SomTest2 {

  def main(args: Array[String]) {
    //create a database
    if(args.length > 0) {

      val dbagent = new CouchAgent(args(0))
      try {
        val expand = new SomExpansion(dbagent)
        dbagent.getNodesUsingParent(args(0)) match {
          case None =>
          case Some(levelnodes) => {
            for(node <- levelnodes) {
              println("checking expansion")
              expand.checkExpansion(node.id)
	    }
	  }
	}
      } catch {
          case e:RuntimeException => println(e.toString) 
          dbagent.shutdown
        }

    }
    else println("Need to have command line arg for db name")
  }
}
