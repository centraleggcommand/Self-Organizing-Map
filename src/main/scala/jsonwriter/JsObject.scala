package jsonwriter

class JsObject(fields:List[Tuple2[Any,Any]]) {

  def addField(nfield:Tuple2[Any,Any]):JsObject = {
    new JsObject(nfield::fields)
  }

  def toJson:String = toJsObj(fields)

  private def toJsObj(data:List[Any]):String = {
    //create string rep of each field
    if (data.length == 0) toJsNull
    else {
      val jsonList:List[String] = for ((k,v)<-data) yield {
        val myKey = convertToJson(k)
        val myVal = convertToJson(v)
        myKey + ":" + myVal
      }
      //combine all fields to one string
      "{" + jsonList.reduceLeft(commaConcat _) + "}"
    }
  }

  private def convertToJson(data:Any):String = {
    data match {
      //an object
      case obj @ (hd::_) => {
        hd match {
          case (_,_) => toJsObj(obj)
          case _ => toJsArray(obj)
        }
      }
      case Nil => toJsNull
      case ival:Int => toJsInt(ival)
      case dval:Double => toJsDbl(dval)
      case bval:Boolean => toJsBool(bval)
      case sval:String => { 
        if(sval == "null") toJsNull
        else toJsStr(sval)
      }
      case null => toJsNull
      case unsupported => unsupported.toString
    }
  }

  private def commaConcat(str1:String,str2:String):String = {
    str1 + "," + str2
  }
/*
  private def commaConcat(mylist:List[String]):String = {
    def createString(remList:List[String],incString:String):String = {
      remList match {
        case Nil => ""
        case (hd::Nil) => hd
        case (hd::rest) => {
          if(incString == "") createString(rest,hd)
          else createString(rest,(incString + "," + hd))
        }
      }
    }
    createString(mylist,"")
  }
*/
  private def toJsInt(ival:Int):String = ival.toString
  private def toJsDbl(dval:Double):String = dval.toString
  private def toJsBool(bval:Boolean):String = bval.toString
  private def toJsStr(sval:String):String = "\"" + sval + "\""
  private def toJsNull = "null"
  private def toJsArray(aval:List[Any]):String = aval match {
    case nestedList @ List(List(_)) => {
      val flatList = nestedList.map(convertToJson)
      "[" + flatList.reduceLeft(commaConcat _) + "]"
    }
    case simpleList => {
      val jsonList = simpleList.map(convertToJson)
      "[" + jsonList.reduceLeft(commaConcat _) + "]"
    }
  }

}


