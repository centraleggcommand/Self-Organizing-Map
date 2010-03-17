package somservice
import org.apache.http._
import org.apache.http.client._
import org.apache.http.client.methods._
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.util.EntityUtils;
import org.apache.http.entity._
import java.io._
import scala.util.parsing.json._
import jsonwriter._

abstract class SomDbAgent(dbName:String)
{
  //Create a new map and return status msg
  def createSom():String
  //The db name is used as the parent value for level zero nodes
  def getDbName:String
  //Create the first node for a map when an initial som insertion is made
  //and return the node id if db fxn succeeded.
  def addDbNode(wordMap:Map[String,Double]):Option[String]
  //Change the values of a node's weight due to an insertion
  //and return status msg
  def updateNode(n:Node):String
  //Add content into the database and return status msg.
  def addEntry(parent:String, deviation:Double, content:String):String
  //Create a list of nodes with common parent.
  def getNodesUsingParent(parent:String):Option[List[Node]] 
  //Get the number of times a word appears in the entire som
  def getGlobalWordCount(word:String):Option[Double]
  //Get the number of documents mapped to a node
  def getChildDocNum(parent:String):Option[Double]
}

class CouchAgent(dbName:String) extends SomDbAgent(dbName)
{
    val couchUri = "http://127.0.0.1:5984/"
    val dbView = "_design/sominsert"
    val wordView = "globalWeight"
    val childView = "allChildren"
    val client = new DefaultHttpClient()

    override def createSom:String = {
      //check if the name is unique
      if (createDb) "The map '" + dbName + "' was created successfully"
      else "The map '" + dbName + "' could not be created in the database"
    }

    override def getDbName = dbName

    override def addDbNode(wordCount:Map[String,Double]):Option[String] = {
      val id = getUuid
      val addr = couchUri + dbName + "/" + id
      //create json data
      val jdata1 = new JsObject(List(("maptype","node")))
      val jdata2 = jdata1.addField(("parent",dbName))
      val jdata3 = jdata2.addField(("weight",wordCount.toList))
      val jsonData = jdata3.toJson
      val response = dbPut(addr, jsonData)
      val rdat = JSON.parse(response)
      rdat match {
        case Some(("error",_)::_) => None
        case Some(_) => {
          addPositionDoc(id)
          Some(id)
        }
        case _ => None
      }
    }

    private def addPositionDoc(nodeId:String) = {
      val id = getUuid
      val addr = couchUri + dbName + "/" + id
      //create json data
      val jdata1 = new JsObject(List(("maptype","position")))
      val jdata2 = jdata1.addField(("grid",List(List(nodeId))))
      val jsonData = jdata2.toJson
      val response = dbPut(addr, jsonData)
      val rdat = JSON.parse(response)
      rdat match {
        case Some(("error",_)::_) => throw new RuntimeException("Could not create a position doc for initial node")
        case _ => "Position doc created"
      }
    }  

    override def updateNode(upData:Node):String = {
      //get the revision number for the doc
      val revRequest = couchUri + dbName + "/" + upData.id
      val jsonData = dbGet(revRequest)
      val data = JSON.parse(jsonData)
      //extract rev and parent values
      val retrievedData:Tuple2[String,String] = data match {
        case Some(nodeData:List[_]) => {
          val revField = nodeData.find((field) => field match { case ("_rev",_) => true; case _ => false} )
          val rev = revField match {
            case Some((_,r:String)) => r
            case None => throw new RuntimeException("updateNode found a node with no rev") }
          val parentField = nodeData.find((field) => field match { case ("parent",_) => true; case _ => false} )
          val parent = parentField match {
            case Some((_,p:String)) => p
            case None => throw new RuntimeException("updateNode found a node with no parent")
          }
          //My return values
          (rev, parent)
        }
        case _ => throw new RuntimeException("Could not get node data from database")
      }
      val jdata1 = new JsObject(List(("maptype","node")))
      val jdata2 = jdata1.addField(("parent",retrievedData._2))
      val jdata3 = jdata2.addField(("weight",upData.weight.toList))
      val jdata4 = jdata3.addField(("_rev",retrievedData._1))
      val jsonSend = jdata4.toJson
      val response = dbPut(revRequest, jsonSend)
      val rdat = JSON.parse(response)
      rdat match {
        case Some(List(("error",_),("reason",reason))) => "Error: " + reason
        case Some(a) => "Updated node: " + a
        case None => "Likely error in updateNode attempt"
      }
    }

    override def addEntry(parent:String, deviation:Double, content:String):String = {
      val id = getUuid
      val addr = couchUri + dbName + "/" + id
      //create json data
      val jdata1 = new JsObject(List(("maptype","entry")))
      val jdata2 = jdata1.addField(("parent",parent))
      val jdata3 = jdata2.addField(("deviation",deviation))
      val jdata4 = jdata3.addField(("content",content))
      val jsonData = jdata4.toJson
      val response = dbPut(addr, jsonData)
      val rdat = JSON.parse(response)
      rdat match {
        case Some(List(("error",_),("reason",r:String))) => "Error: " + r
        case Some(info) => "Added entry - " + info
        case None => "Hmm... database quiet on addEntry"
      }
    }  

    override def getNodesUsingParent(parent:String):Option[List[Node]] = {
      //call predefined db fxn for specific parent; whenever a new level is 
      //created for a node, a parent fxn should be defined in db
      val jsonData = dbGet(couchUri + dbName + "/" + dbView + "/_view/" + parent)
      //convert the json data
      val data = JSON.parse(jsonData)
      data match {
        //check if the parent fxn existed
        case Some(("error",_)::rest) => None
        //extract data
        case Some(List(("total_rows",rowCount:Double),_,("rows",rows:List[_]))) if rowCount > 0 => {
          Some(pkgNodes(rows))
        }
        case _ => None
      }
    }

    override def getGlobalWordCount(word:String):Option[Double] = {
      val req = couchUri + dbName + "/" + dbView + "/_view/" + wordView + "?group=true&key=%22" + word + "%22"
      val jsonData = dbGet( req )
      val data = JSON.parse(jsonData)
      println(data.toString)
      data match {
        //check if the parent fxn existed
        case Some(("error",_)::rest) => None
        //extract data
        case Some(List(("rows",List(List(("key",_),("value",num:Double)))))) => Some(num)
        case _ => None
      }
    }

    override def getChildDocNum(parent:String):Option[Double] = {
      val req = couchUri + dbName + "/" + dbView + "/_view/" + childView
      val jsonData = dbGet( req )
      val data = JSON.parse(jsonData)
      println(data.toString)
      data match {
        //check if the parent fxn existed
        case Some(("error",_)::rest) => None
        //extract data
        case Some(List(("rows",List(List(("key",_),("value",num:Double)))))) => Some(num)
        case _ => None
      }
    }

    private def pkgNodes(rows:List[Any]):List[Node] = {
      for (row <- rows) yield {
        row match {
          case List(("id",id:String),_,("value",dataList:List[_])) => {
            val weights = for (factor <- dataList) yield {
              factor match {
                case (myword:String,usage:Double) => {
                  (myword,usage) }
                case _ => ("unmatched pair",0.0)
              }
            }
            new Node(id, (Map.empty[String,Double] ++ weights))
          }
          case List(("id",id:String),_,("value",_)) => new Node(id, Map.empty[String,Double])
          case _ => new Node("NoID",Map("unmatched row"->0.0))
        }
      }
    }

    private def createDb:Boolean = {
      val jsonResult = dbPut(couchUri + dbName)
      val result = JSON.parse(jsonResult)
      result match {
        case Some(("error",_)::rest) => false
        case Some(("ok",_)::rest) => {
          val fxnObj = new JsObject(JsFxn.getInitView(dbView,dbName,dbName,wordView,childView))
          val jsonData = fxnObj.toJson
          println(jsonData)
          val response = dbPut(couchUri + dbName + "/" + dbView, jsonData)
          true
        }
        case _ => false
      }
    }

    private def dbPut(request:String):String = {
      val httpPut = new HttpPut(request)
      val response = client.execute(httpPut)
      val rEnt = response.getEntity()
      EntityUtils.toString(rEnt)
    }

    private def dbPut(request:String, data:String):String = {
      val httpPut = new HttpPut(request)
      val entityData = new StringEntity(data)
      httpPut.setEntity(entityData)
      val response = client.execute(httpPut)
      val rEnt = response.getEntity()
      EntityUtils.toString(rEnt)
    }

    private def dbGet(request:String):String = {
      val httpget = new HttpGet(request)
      val response = client.execute(httpget)
      val rEnt = response.getEntity()
      EntityUtils.toString(rEnt)
    }

    def getUuid:String = {
      val httpget = new HttpGet(couchUri + "_uuids")
      val response = client.execute(httpget)
      val rEnt = response.getEntity()
      val rData = EntityUtils.toString(rEnt)
      val rJson = JSON.parse(rData)
      rJson match {
        case Some(("uuids",List(id:String))::rest) => id
        case None => ""
        case _ => ""
      }
    }

}
