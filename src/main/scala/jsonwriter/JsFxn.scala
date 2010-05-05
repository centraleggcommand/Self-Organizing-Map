package jsonwriter


//Provide templates to add functions to a couchdb design doc;
object JsFxn {

  //fxn to get all docs with maptype = node and having
  //the specified parent. This should be added to the
  //"views" list.
  def getNodeParentFxn(fxnName:String):Tuple2[String,Any] = {
    val fxnbody = """function(doc) { if( doc.maptype == \"node\") { emit( doc.parent,doc.weight);}}"""
    //return a representation consistent with jsonparse lib
    (fxnName,List(("map",fxnbody)))
  }
  
  def getGlobalWordFxn(parent:String, fxnName:String):Tuple2[String,Any] = {
    val mapBody = """function(doc) { if( doc.maptype == \"node\" && doc.parent  == \""" + "\"" + parent + """\") { for( var wc in doc.weight ) { emit(wc,doc.weight[wc]); }}}"""
    val reduceBody = "function(keys,values) { return sum(values) }"
    (fxnName,List(("map",mapBody),("reduce",reduceBody)))
  }

  def getChildrenFxn(fxnName:String):Tuple2[String,Any] = {
    val mapBody = """function(doc) { if( doc.maptype == \"entry\"){ emit( doc.parent,doc._id);}}"""
    //return a representation consistent with jsonparse lib
    val reduceBody = "function(keys,values) { return values.length }"
    (fxnName,List(("map",mapBody),("reduce",reduceBody)))
  }

  def getPositionFxn(fxnName:String):Tuple2[String,Any] = {
    val fxnbody = """function(doc) { if( doc.maptype == \"position\"){ emit( doc.parentNode,doc.grid);}}"""
    //return a representation consistent with jsonparse lib
    (fxnName,List(("map",fxnbody)))
  }

  def getNodePosFxn(fxnName:String):Tuple2[String,Any] = {
    val fxnbody = """function(doc) { if( doc.maptype == \"position\"){ for(var row in doc.grid) { for( var node in row) { emit( doc.grid[row][node],doc._id);}}}}"""
    //return a representation consistent with jsonparse lib
    (fxnName,List(("map",fxnbody)))
  }

  def getAllMaps(fxnName:String):Tuple2[String,Any] = {
    val fxnbody = """function(doc) { if( doc.maptype == \"position\"){ emit( doc.parentMap, doc.parentNode);}}"""
    //return a representation consistent with jsonparse lib
    (fxnName,List(("map",fxnbody)))
  }

  def getEntryDev(fxnName:String):Tuple2[String,Any] = {
    val mapBody = """function(doc) { if( doc.maptype == \"entry\"){ emit( doc.parent, doc.deviation);}}"""
    //return a representation consistent with jsonparse lib
    val reduceBody = "function(keys,values) { var tot = sum(values); return (tot / values.length) }"
    (fxnName,List(("map",mapBody),("reduce",reduceBody)))
  }

  def getEntrySC(fxnName:String):Tuple2[String,Any] = {
    val fxnBody = """function(doc) { if(doc.maptype ==\"entry\") emit(doc.parent, Array(doc.subject,doc.content) )}"""
    (fxnName,List(("map",fxnBody)))
  }

  def getTally(fxnName:String):Tuple2[String,Any] = {
    val fxnBody = """function(doc) { if(doc.maptype ==\"tally\") emit(doc._id, doc.entryTally) }"""
    (fxnName,List(("map",fxnBody)))
  }


  def getInitView(viewName:String, parent:String, pFxn:String, wFxn:String, cFxn:String, posFxn:String, nodePosFxn:String, allMapsFxn:String, entryDevFxn:String, entrySCFxn:String, tallyFxn:String):List[Tuple2[Any,Any]] = {
    List(("_id",viewName),("views",List(getNodeParentFxn(pFxn), getGlobalWordFxn(parent,wFxn),getChildrenFxn(cFxn),getPositionFxn(posFxn),getNodePosFxn(nodePosFxn),getAllMaps(allMapsFxn),getEntryDev(entryDevFxn),getEntrySC(entrySCFxn),getTally(tallyFxn))))
  }

}
