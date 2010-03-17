import somservice.CouchAgent

object TestDb {

  def main(args: Array[String]) {
    //create a database
    if(args.length > 0) {
      val dbagent = new CouchAgent(args(0))
      println(dbagent.createSom)
      val testmap1 = Map("sauce"->2.0,"burger"->5.0,"chile"->3.0)
      dbagent.addDbNode(testmap1)
      val testmap2 = Map.empty[String,Double]
      dbagent.addDbNode(testmap2)
      val testmap3 = Map("diet"->76.0)
      dbagent.addDbNode(testmap3)
      val result = dbagent.getNodesUsingParent(args(0))
      result match {
        case Some(mynodes) => {
          for( node <- mynodes) {
            println(node.weight.toString)
          }
        }
        case None => println("Fxn getNodesUsingParent rtn empty")
      }
    }
  }
}
