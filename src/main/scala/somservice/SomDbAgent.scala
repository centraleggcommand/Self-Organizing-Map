package somservice
import org.apache.http._
import org.apache.http.params._
import org.apache.http.conn.ClientConnectionManager
import org.apache.http.conn.routing.HttpRoute
import org.apache.http.conn.params.ConnManagerParams
import org.apache.http.conn.params.ConnPerRouteBean
import org.apache.http.conn.scheme._
import org.apache.http.conn.ssl.SSLSocketFactory
import org.apache.http.client._
import org.apache.http.client.methods._
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.impl.conn.tsccm._
import org.apache.http.util.EntityUtils;
import org.apache.http.entity._
import org.apache.commons.logging._
import org.apache.log4j.Logger
import org.apache.log4j.PropertyConfigurator
import java.io._
import scala.util.parsing.json._
import jsonwriter._

abstract class SomDbAgent(dbName:String)
{
  //Create a new map and return status msg
  def createSom():Boolean
  //Get the names of all databases created
  def getAllDbs:List[String]
  //Release socket resources
  def shutdown:Unit
  //The db name is used as the parent value for level zero nodes
  def getDbName:String
  //Create one of the initial nodes for a som layer when an insertion is made
  //and return the node id if db fxn succeeded.
  def addInitNode(parent:String, wordMap:Map[String,Double]):Option[String]
  //Change the values of a node's weight due to an insertion
  //and return status msg
  def updateNode(n:Node):Boolean
  //Add content into the database and return status msg.
  def addEntry(parent:String, deviation:Double, subject:String, content:String):Boolean
  //Update count of number of entries. Used to determine when a 
  //map expansion should occur
  def updateTally(reset:Boolean):Boolean
  def getTally:Option[Double]
  //Get a representation of the location of every node on a som layer
  def getPositionDoc(parent:String):Option[Tuple2[String,List[Any]]]
  //Create a new position map
  def addPositionDoc(parent:String):Boolean
  //Change the position data
  def updatePositionDoc(mapId:String, posData:List[Any]):Boolean
  //Get the position map that this node belongs to
  def getNodeMap(nodeId:String):Option[String]
  //Get the map id, parent node id, and parent map id
  def getPositionMapTree:Option[ListType]
  //Get the deviation value for each entry belonging to a node
  def getAvgNodeDeviation(parent:String):Option[Double]
  //Create a list of nodes with common parent.
  def getNodesUsingParent(parent:String):Option[List[Node]] 
  //Get the number of times a word appears in the entire som
  def getGlobalWordCount(word:String):Option[Double]
  //Get the number of documents mapped to a node
  def getChildDocNum(parent:String):Option[Double]
  //Get entry subject and content for all entries under a node
  def getEntries(parent:String):Option[List[Tuple3[Any,Any,Any]]]
  //Get the parent node id for a node
  def getParentNode(nodeId:String):Option[String]
}

class CouchAgent(dbn:String) extends SomDbAgent(dbn)
{
    val dbName = if(dbn.length > 0) dbn
                 else "dummy"
    val couchUri = "http://127.0.0.1:34719/"
    val dbView = "_design/sominsert"
    val parentWeightView = "parentWeight"
    val wordView = "globalWeight"
    val childView = "allChildren"
    val posView = "mapPosition"
    val nodePosView = "parentMap"
    val allMapsView = "allMaps"
    val entryDevView = "entryDev"
    //get the subject and content of all entries
    val entrySCView = "entrySC"
    val tallyView = "tally"
    //val client = new DefaultHttpClient()
    val client = {
      val params:HttpParams = new BasicHttpParams
      // Increase max total connection to 200
      ConnManagerParams.setMaxTotalConnections(params, 200)
      // Increase default max connection per route to 20
      val connPerRoute:ConnPerRouteBean = new ConnPerRouteBean(20)
      // Increase max connections for localhost:80 to 50
      val localhost:HttpHost = new HttpHost("locahost", 80)
      connPerRoute.setMaxForRoute(new HttpRoute(localhost), 50)
      ConnManagerParams.setMaxConnectionsPerRoute(params, connPerRoute)
      val schemeRegistry:SchemeRegistry = new SchemeRegistry
      schemeRegistry.register(
              new Scheme("http", PlainSocketFactory.getSocketFactory(), 80))
      schemeRegistry.register(
                      new Scheme("https", SSLSocketFactory.getSocketFactory(), 443))
      val cm:ClientConnectionManager = new ThreadSafeClientConnManager(params, schemeRegistry)
      new DefaultHttpClient(cm, params)
    }
    val logger = Logger.getLogger("somservice.CouchAgent")

    PropertyConfigurator.configure("log4j.properties")

    override def createSom:Boolean = {
      //check if the name is unique
      if (createDb) {
        addPositionDoc(dbName)
        addTallyDoc
        logger.debug("The map '" + dbName + "' was created successfully")
        true
      }
      else {
        logger.info("The map '" + dbName + "' could not be created in the database")
        false
      }
    }

    override def shutdown:Unit = client.getConnectionManager.shutdown

    override def getAllDbs:List[String] = {
      val req = couchUri + "_all_dbs"
      val jsonData = dbGet( req )
      val data = JSON.parse(jsonData)
      data match {
        case Some(List(("error",_),("reason",r:String))) => {
          logger.error("getAllDbs db error: " + r)
          Nil
        }
        case Some(dbs:List[_]) => dbs.map(_.toString)
        case None => {
          logger.error("getAllDbs fxn got no db response")
          Nil
        }
      }
    }

    override def getDbName = dbName

    override def addInitNode(parent:String, wordCount:Map[String,Double]):Option[String] = {
      getUuid match {
        case Some(id) => {
          val addr = couchUri + dbName + "/" + id
          //create json data
          val jdata1 = new JsObject(List(("maptype","node")))
          val jdata2 = jdata1.addField(("parent",parent))
          val jdata3 = jdata2.addField(("weight",wordCount.toList))
          val jsonData = jdata3.toJson
          val response = dbPut(addr, jsonData)
          val rdat = JSON.parse(response)
          rdat match {
            case Some(List(("error",_),("reason",r:String))) => {
              logger.error("addInitNode db error: " + r)
              None
            }
            case Some(_) => Some(id)
            case None => {
              logger.error("addInitNode fxn got no db response")
              None
            }
          }
        }
        case None => {
          logger.error("addInitNode could not get Uuid")
          None
        }
      }
    }

    override def addPositionDoc(parent:String):Boolean = {
      logger.debug("Trying to add a position doc")
      getUuid match {
        case Some(id) => {
          val addr = couchUri + dbName + "/" + id
          //create json data
          val jdata1 = new JsObject(List(("maptype","position")))
          val jdata2 = jdata1.addField(("grid",Nil))
          val jdata3 = jdata2.addField(("parentNode",parent))
          val pm = getNodeMap(parent) match {
            case Some(id) => id
            case None => null
          }
          val jdata4 = jdata3.addField(("parentMap",pm))
          val jsonData = jdata4.toJson
          val response = dbPut(addr, jsonData)
          val rdat = JSON.parse(response)
          rdat match {
            case Some(List(("error",_),("reason",r:String))) => {
              logger.error("addPositionDoc db error: " + r)
              false
            } 
            case Some(_) => {
              logger.debug("Position doc created")
              true
            }
            case None => {
              logger.error("addPositionDoc fxn got no db response")
              false
            }
          }
        }
        case None => {
          logger.error("addPositionDoc could not get Uuid")
          false
        }
      }
    }  

    private def addTallyDoc:Boolean = {
      logger.debug("Trying to add a tally doc")
      getUuid match {
        case Some(id) => {
          val addr = couchUri + dbName + "/" + id
          //create json data
          val jdata1 = new JsObject(List(("maptype","tally")))
          val jdata2 = jdata1.addField(("entryTally",0.0))
          val jsonData = jdata2.toJson
          val response = dbPut(addr, jsonData)
          val rdat = JSON.parse(response)
          rdat match {
            case Some(List(("error",_),("reason",r:String))) => {
              logger.error("addTallyDoc db error: " + r)
              false
            } 
            case Some(_) => {
              logger.debug("Tally doc created")
              true
            }
            case None => {
              logger.error("addTallyDoc fxn got no db response")
              false
            }
          }
        }
        case None => {
          logger.error("addTallyDoc could not get Uuid")
          false
        }
      }
    }  

    override def updateNode(upData:Node):Boolean = {
      substituteField(upData.id,"weight",upData.weight.toList)
    }
 /*     //get the revision number for the doc
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
*/



    override def addEntry(parent:String, deviation:Double, subject:String, content:String): Boolean = {
      getUuid match {
        case Some(id) => {
          val addr = couchUri + dbName + "/" + id
          //create json data
          val jdata1 = new JsObject(List(("maptype","entry")))
          val jdata2 = jdata1.addField(("parent",parent))
          val jdata3 = jdata2.addField(("deviation",deviation))
          val jdata4 = jdata3.addField(("subject", subject))
          val jsstr = content.replaceAll("\"","'")
          val jdata5 = jdata4.addField(("content",jsstr))
          val jsonData = jdata5.toJson
          logger.debug("adding entry: " + jsonData)
          val response = dbPut(addr, jsonData)
          val rdat = JSON.parse(response)
          rdat match {
            case Some(List(("error",_),("reason",r:String))) => {
              logger.error("addEntry db error: " + r)
              false
            }
            case Some(info) => {
              logger.debug("Added entry - " + info)
              updateTally(false)
              true
            }
            case None => {
              logger.error("addEntry got no db response")
              false
            }
          }
        }
        case None => {
          logger.error("addEntry could not get Uuid")
          false
        }
      }
    }  

    override def updateTally(reset:Boolean):Boolean = {
      val jsonData = dbGet(couchUri + dbName + "/" + dbView + "/_view/" + tallyView)
      //convert the json data
      val data = JSON.parse(jsonData)
      data match {
        //check if the db fxn existed
        case Some(List(("error",_),("reason",r:String))) => {
          logger.error("updateTally db error: " + r)
          false
        }
        //extract data
        case Some(List(_,_,("rows",List(List(("id",id:String),_,("value",num:Double)))))) => {
          val n = if(!reset) num+1
                  else 0.0
          substituteField(id,"entryTally",n)
          logger.debug("Updating tally: " + n)
          true
        }
        case Some(List(_,_,("rows",Nil))) => {
          logger.debug("No entry tally found")
          false
        }
        case resp => {
          logger.debug("Unrecognized db response: " + resp)
          false
        }
      }
    }

    override def getTally:Option[Double] = {
      val jsonData = dbGet(couchUri + dbName + "/" + dbView + "/_view/" + tallyView)
      //convert the json data
      val data = JSON.parse(jsonData)
      data match {
        //check if the db fxn existed
        case Some(List(("error",_),("reason",r:String))) => {
          logger.error("getTally db error: " + r)
          None
        }
        //extract data
        case Some(List(_,_,("rows",List(List(("id",id:String),_,("value",num:Double)))))) => {
          Some(num)
        }
        case Some(List(_,_,("rows",Nil))) => {
          logger.debug("No entry tally found")
          None
        }
        case resp => {
          logger.debug("Unrecognized db response: " + resp)
          None
        }
      }
    }

    override def getAvgNodeDeviation(parent:String):Option[Double] = {
      val jsonData = dbGet(couchUri + dbName + "/" + dbView + "/_view/" + entryDevView + "?key=%22" + parent + "%22")
      //convert the json data
      val data = JSON.parse(jsonData)
      logger.debug("entry deviation avg: " + data)
      data match {
        //check if the db fxn existed
        case Some(List(("error",_),("reason",r:String))) => {
          logger.error("getAvgNodeDeviation db error: " + r)
          None
        }
        //extract data
        case Some(List(("rows",List(List(_,("value",avg:Double)))))) => {
          Some(avg)
        }
        case Some(List(("rows",Nil))) => None
        case _ => None
      }
    }



    override def getNodesUsingParent(parent:String):Option[List[Node]] = {
      //call predefined db fxn for specific parent; whenever a new level is 
      //created for a node, a parent fxn should be defined in db
      val jsonData = dbGet(couchUri + dbName + "/" + dbView + "/_view/" + parentWeightView + "?key=%22" + parent + "%22")
      //convert the json data
      val data = JSON.parse(jsonData)
      data match {
        //check if the parent fxn existed
        case Some(List(("error",_),("reason",r:String))) => {
          logger.error("getNodesUsingParent db error: " + r)
          None
        }
        //extract data
        case Some(List(("total_rows",rowCount:Double),_,("rows",rows:List[_]))) => {
          rows match {
            case Nil => None
            case _ => Some(pkgNodes(rows))
          }
          //if( rowCount > 0) Some(pkgNodes(rows))
          //else None
        }
        case Some(r) => {
          logger.error("getNodesUsingParent could not recognize db response: " + r)
          None
        }
        case None => { logger.error("getNodesUsingParent got no db response")
          None
        }
      }
    }

    override def getPositionDoc(parent:String):Option[Tuple2[String,List[Any]]] = {
      val req = couchUri + dbName + "/" + dbView + "/_view/" + posView + "?key=%22" + parent + "%22"
      val jsonData = dbGet( req )
      val data = JSON.parse(jsonData)
      data match {
        case Some(List(("error",_),("reason",r:String))) => {
          logger.error("getPositionDoc db error: " + r)
          None
        }
        case Some(List(_,_,("rows",List(List(("id",id:String),_,("value",grid)))))) => {
          grid match {
            case posRows:List[_] => {
              //Re-occuring problem: how to match multileveled Lists with
              //an unknown number of elements and property of type erasure.
              Some((id,posRows))
            }
            case null => Some((id, Nil))
            case _ => {
              logger.error("getPositionDoc did not recognize value: " + grid)
              None
            }
          }
        }
        case Some(rsp) => { 
          logger.error("getPositionDoc did not recognize response: " + rsp)
          None
        }
        case None => { logger.error("getPositionDoc got no db response")
          None
        }
      }
    }

    override def updatePositionDoc(id:String, posData:List[Any]):Boolean = {
      if(substituteField(id,"grid",posData)) {
        logger.debug("Field updated: " + posData.toString)
        true
      }
      else false
    }

    override def getNodeMap(nodeId:String):Option[String] = {
      val req = couchUri + dbName + "/" + dbView + "/_view/" + nodePosView + "?key=%22" + nodeId + "%22"
      val jsonData = dbGet( req )
      val data = JSON.parse(jsonData)
      data match {
        case Some(List(("error",_),("reason",r:String))) => {
          logger.error("getNodeMap db error: " + r)
          None
        }
        case Some(List(_,_,("rows",List(List(("id",nodeId:String),_,("value",mapId:String)))))) => Some(mapId)
        case Some(List(_,_,("rows",Nil))) => {
          logger.debug("An empty position map was returned")
          None
        }
        case Some(rsp) => { 
          logger.error("getNodeMap did not recognize response: " + rsp)
          None
        }
        case None => { logger.error("getNodeMap got no db response")
          None
        }
      }
    }


    //This fxn assumes that the database ref by dbAgent is only used by 
    //a single som
    override def getPositionMapTree:Option[ListType] = {
      val req = couchUri + dbName + "/" + dbView + "/_view/" + allMapsView
      val jsonData = dbGet( req )
      val data = JSON.parse(jsonData)
      logger.debug("db get allmaps: " + data)
      data match {
        case Some(List(("error",_),("reason",r:String))) => {
          logger.error("getPositionMapTree db error: " + r)
          None
        }
        //Expect: Some(List((total_rows,_),(offset,_),(rows,List( List((id,_),(key,_),(value,_))...) )))
        case Some(List(_,_,("rows", mapData:List[_]))) => {
          val res = for( List( ("id",id:String),("key",key:String),("value",v:String) ) <- mapData)
            yield { (id,key,v) }
          Some(PositionTree(res))
        }
        case None => {
          logger.info("getPositionMapTree got not db data")
          None
        }
      }
    }

    private def substituteField(id:String,fieldName:String,subData:Any):Boolean = {
      //Remove from the origDoc the field and value that needs to be changed
      //and add the new data.
      val revRequest = couchUri + dbName + "/" + id
      val jsonData = dbGet(revRequest)
      val data = JSON.parse(jsonData)
      data match {
        case Some(List(("error",_),("reason",reason))) => {
          logger.error("substituteField get doc error: " + reason)
          false
        }
        case Some(origDoc:List[_]) => {
          val uDoc = for( (d1,d2) <- origDoc) yield {
            if(d1 == fieldName) (fieldName,subData)
            else (d1,d2)
          }
          //Send info back to database
          val jdata1 = new JsObject(uDoc)
          val jsonSend = jdata1.toJson
          logger.debug("substituteField info: " + jsonSend)
          val response = dbPut(revRequest, jsonSend)
          val rdat = JSON.parse(response)
          rdat match {
            case Some(List(("error",_),("reason",reason))) => {
              logger.error("substituteField update error: " + reason)
              false
            }
            case Some(a) => { true
            }
            case None => {
              logger.error("substituteField got no response from db for update")
              false
            }
          }
        }
        case None => {
          logger.error("substituteField got no response from db")
          false
        }
      }
    }


    override def getGlobalWordCount(word:String):Option[Double] = {
      val req = couchUri + dbName + "/" + dbView + "/_view/" + wordView + "?group=true&key=%22" + word + "%22"
      val jsonData = dbGet( req )
      val data = JSON.parse(jsonData)
      data match {
        //check if the parent fxn existed
        case Some(List(("error",_),("reason",reason))) => {
          logger.error("getGlobalWordCount error: " + reason)
          None
        }
        //extract data
        case Some(List(("rows",List(List(("key",_),("value",num:Double)))))) => Some(num)
        case Some(List(("rows",Nil))) => None
        case Some(_) => {
          logger.error("getGlobalWordCount could not recognize db response format")
          None
        }
        case None => {
          logger.error("getGlobalWordCount got no response from db")
          None
        }
      }
    }

    override def getChildDocNum(parent:String):Option[Double] = {
      val req = couchUri + dbName + "/" + dbView + "/_view/" + childView + "?key=%22" + parent + "%22"
      val jsonData = dbGet( req )
      val data = JSON.parse(jsonData)
      data match {
        //check if the parent fxn existed
        case Some(List(("error",_),("reason",reason))) => {
          logger.error("getChildDocNum error: " + reason)
          None
        }
        //extract data
        case Some(List(("rows",List(List(("key",_),("value",num:Double)))))) => Some(num)
        case Some(List(("rows",Nil))) => None
        case Some(_) => {
          logger.error("getChildDocNum could not recognize db response format")
          None
        }
        case None => {
          logger.error("getChildDocNum got no response from db")
          None
        }
      }
    }

    //This returns the subject and content of entries with the given parent.
    override def getEntries(parent:String):Option[List[Tuple3[Any,Any,Any]]] = {
      val req = couchUri + dbName + "/" + dbView + "/_view/" + entrySCView + "?key=%22" + parent + "%22"
      val jsonData = dbGet( req )
      val data = JSON.parse(jsonData)
      data match {
        //check if the parent fxn existed
        case Some(List(("error",_),("reason",reason))) => {
          logger.error("getEntries error: " + reason)
          None
        }
        //extract data
        case Some(List(_,_,("rows", data:List[_]))) => { 
          val scData = for( List(("id",id),("key",_),("value",List(subject,content))) <- data) yield {
            (id,subject,content)
          }
          if(scData.length > 0) Some(scData)
          else None
        }
        case Some(_) => {
          logger.error("getEntries could not recognize db response format")
          None
        }
        case None => {
          logger.error("getEntries got no response from db")
          None
        }
      }
    }

    //Return the parent node id for a node
    override def getParentNode(nodeId:String):Option[String] = {
      val jsonData = dbGet(couchUri + dbName + "/" + nodeId)
      //convert the json data
      val data = JSON.parse(jsonData)
      logger.debug("node parent: " + data)
      data match {
        //check if the db fxn existed
        case Some(List(("error",_),("reason",r:String))) => {
          logger.error("getParentNode db error: " + r)
          None
        }
        //extract data
        case Some(List(_,_,_,("parent",parent:String),_)) => {
          Some(parent)
        }
        case Some(List(("rows",Nil))) => None
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
        case Some(("error",_)::rest) => {
          logger.error("createDb error: " + rest)
          false
        }
        case Some(("ok",_)::rest) => {
          //all view names are defined at the top of this file
          logger.debug("Creating views")
          val fxnObj = new JsObject(JsFxn.getInitView(dbView,dbName,parentWeightView,wordView,childView,posView,nodePosView,allMapsView,entryDevView,entrySCView,tallyView))
          val jsonData = fxnObj.toJson
          logger.debug("Inserting json for db views: " + jsonData)
          val response = dbPut(couchUri + dbName + "/" + dbView, jsonData)
          JSON.parse(response) match {
            //just logging any error but no resolution
            case Some(("error",_)::rest) => {
              logger.error("createDb error: " + rest)
            }
            case msg => logger.debug(msg)
          }
          true
        }
        case _ => false
      }
    }

    private def dbPut(request:String):String = {
      val httpPut = new HttpPut(request)
      val response = client.execute(httpPut)
      val rEnt = response.getEntity()
      val output = EntityUtils.toString(rEnt)
      httpPut.abort
      output
    }

    private def dbPut(request:String, data:String):String = {
      val httpPut = new HttpPut(request)
      val entityData = new StringEntity(data)
      httpPut.setEntity(entityData)
      val response = client.execute(httpPut)
      val rEnt = response.getEntity()
      val output = EntityUtils.toString(rEnt)
      httpPut.abort
      output
    }

    private def dbGet(request:String):String = {
      val httpget = new HttpGet(request)
      val response = client.execute(httpget)
      val rEnt = response.getEntity()
      val output = EntityUtils.toString(rEnt)
      httpget.abort
      output
    }

    def getUuid:Option[String] = {
      val httpget = new HttpGet(couchUri + "_uuids")
      val response = client.execute(httpget)
      val rEnt = response.getEntity()
      val rData = EntityUtils.toString(rEnt)
      val rJson = JSON.parse(rData)
      httpget.abort
      rJson match {
        case Some(("uuids",List(id:String))::rest) => Some(id)
        case None => None
        case _ => None
      }
    }

}
