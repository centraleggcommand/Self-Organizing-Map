package somservice
import textprocessing.InputTicket

class SomInsertion(data:SomEntry)
{
  //the id of the node the entry has been associated with
  val winnerNode = organizeEntry
  //run the som algorithm on the entry and associate with a node
  private def organizeEntry:String = {
    data match {
      case BasicContent(dbName,wordList) => 
        //transform the list into a hashmap with word and occurrence pairs
        val ticket = new InputTicket(wordList)
        //create the representation of the level zero map
        val dbAgent = new CouchAgent(dbName)
//        val winner = cycleThruLevels(dbAgent,dbName,ticket)
        //Add db entry using node id as parent
        if (true) "Need to finish"
        else "Need to finish"
    }
  }
  //compare the entry weight to node weights at each required level
  private def cycleThruLevels(dbAgent:SomDbAgent, parent:String, ticket:InputTicket):String = {
    //level zero map nodes have the db name as the parent value
    dbAgent.getNodesUsingParent(parent) match {
      //either no nodes exist (new map) or node is not a parent
      case None => {
        if (parent == dbAgent.getDbName) dbAgent.addDbNode(ticket.getWordMap) 
        else parent
      }
      case Some(levelNodes) => {
        val matchedNode = cycleThruNodes(levelNodes, ticket)
        cycleThruLevels(dbAgent, matchedNode.id, ticket)
      }
    }
  }
      
  private def cycleThruNodes(nodes:List[Node], ticket:InputTicket):Node = {


  private def calcNodeDistance(node:Node, ticket:InputTicket):Double = {



}
