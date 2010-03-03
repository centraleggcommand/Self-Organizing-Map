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
  //Create a new map
  def createSom():String
  //The db name is used as the parent value for level zero nodes
  def getDbName:String
  //Create the first node for a map when an initial som insertion is made.
  def addDbNode(wordMap:Map[String,Double]):Option[String]
  //Create a list of nodes with common parent.
  //The list will be popped for each node compared against.
  //Trying to stay immutable, so the node comparison is expected to return
  //a node id.
  def getNodesUsingParent(parent:String):Option[List[Node]] 
}

class CouchAgent(dbName:String) extends SomDbAgent(dbName)
{
    val couchUri = "http://127.0.0.1:5984/"
    val dbView = "_design/sominsert"
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
        case Some(_) => Some(id)
        case _ => None
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

    private def pkgNodes(rows:List[Any]):List[Node] = {
      for (row <- rows) yield {
        row match {
          case List(("id",id:String),_,("value",dataList:List[_])) => {
            val weights = for (factor <- dataList) yield {
              factor match {
                case List(("word",myword:String),("count",usage:Double)) => {
                  (myword,usage) }
                case _ => ("unmatched pair",0.0)
              }
            }
            new Node(id, (Map.empty[String,Double] ++ weights))
          }
          case _ => new Node("",Map("unmatched row"->0.0))
        }
      }
    }

    private def createDb:Boolean = {
      val jsonResult = dbPut(couchUri + dbName)
      val result = JSON.parse(jsonResult)
      result match {
        case Some(("error",_)::rest) => false
        case Some(("ok",_)::rest) => {
          val fxnObj = new JsObject(JsFxn.getInitView(dbView,dbName,dbName))
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
