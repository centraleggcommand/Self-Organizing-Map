package jsonwriter


//Provide templates to add functions to a couchdb design doc;
object JsFxn {

  //fxn to get all docs with maptype = node and having
  //the specified parent. This should be added to the
  //"views" list.
  def getNodeParentFxn(parent:String, fxnName:String):Tuple2[String,Any] = {
    val fxnbody = """function(doc) { if( doc.maptype == \"node\" && doc.parent == \""" + "\"" + parent + """\"){ emit( doc.parent,doc.weight);}}"""
    //return a representation consistent with jsonparse lib
    (fxnName,List(("map",fxnbody)))
  }
  
  def getGlobalWordFxn(parent:String, fxnName:String):Tuple2[String,Any] = {
    val mapBody = """function(doc) { if( doc.maptype == \"node\" && doc.parent  == \""" + "\"" + parent + """\") { for( var wc in doc.weight ) { emit(wc,doc.weight[wc]); }}}"""
    val reduceBody = "function(keys,values) { return sum(values) }"
    (fxnName,List(("map",mapBody),("reduce",reduceBody)))
  }


  def getInitView(viewName:String, parent:String, pFxn:String, wFxn:String):List[Tuple2[Any,Any]] = {
    List(("_id",viewName),("views",List(getNodeParentFxn(parent,pFxn), getGlobalWordFxn(parent,wFxn))))
  }

}
