package somservice
import scala.Math._
import scala.collection.mutable

sealed case class Node(id:String, weight:Map[String,Double])
{

  def updateWeight(incoming:Map[String,Double]):Node = {
    val upWeight = mutable.Map.empty[String,Double]
    for ((word,count) <- incoming ) {
      if( weight.contains(word) )
        upWeight += (word -> (weight(word) + count))
      else upWeight += (word -> count)
    }
    //Insert original weight words that were not already counted
    for ((word,count) <- weight) {
      if( !incoming.contains(word) )
        upWeight += (word -> count)
    }
    //convert to immutable map
    val w = upWeight.toList
    val m = Map.empty[String,Double]
    new Node(id, m ++ w)
  }


  //The word counts are given a calculated weight based on the 
  //increasing importance from higher count, but decreasing importance
  //as the global usage of the word increases.
  private def offsetWeight(count:Double, global:Double):Double = {
    (pow(1.02,ceil(count))) / (pow(1.008,global) + (pow(1.0008,global))/10)
  }

  //I wanted to make a more functional generic solution to replace all 
  //the logic handled by calcNodeDistance, but I am afraid of the
  //performance hit from the increased database accesses, since 
  //the latter fxn would not call getGlobalWordCount on words that
  //were not in common with the node and entry.
  private def calcOffsetWeight(srcWeight:Map[String,Double], dbAgent:SomDbAgent): Map[String,Double] = {
    val offWeight = mutable.Map.empty[String,Double]
    for((word,count) <- srcWeight) {
      val gwc = dbAgent.getGlobalWordCount(word) match {
        case Some(num) => num
        case None => 1.0
      }
      offWeight += (word -> offsetWeight(count,gwc))
    }
    //convert to immutable map
    val w = offWeight.toList
    val m = Map.empty[String,Double]
    m ++ w
  }

  //This returns the distance between the node and another weight.
  //If the weight is empty, then a 'None' value is returned.
  def calcNodeDistance( cmpWeight:Map[String,Double], dbAgent:SomDbAgent ):Double = {
    //Calculate the euclidean distance between weights.
    //Each word in the node's weight is searched for in the other weight,
    //and a zero value is used if it doesn't exist.
    val calcDiff = for( (word,count) <- weight) yield {
      //the node weight is converted to an average over the total
      //number of docs, then the offset is calculated
      val childNum = dbAgent.getChildDocNum(id) match {
        case Some(n) => n
        case None => 1.0
      }
      val avgWordWeight = count/childNum
      val wc:Double = dbAgent.getGlobalWordCount(word) match {
        case Some(num) => num
        case None => 1.0
      }
      val nodeWordOffset = offsetWeight(avgWordWeight, wc)
      if( cmpWeight.contains(word)) {
        val cmpWordCount:Double = cmpWeight(word)
        val cmpWordOffset = offsetWeight(cmpWordCount, wc)
        val sqdiff = pow((nodeWordOffset - cmpWordOffset),2)
        (word -> sqdiff)
      }
      else {
        val sqdiff = pow(nodeWordOffset,2)
        (word -> sqdiff)
      }
    }
    //finish euclidean distance calc on squared differences in the map
    val sumDiff = calcDiff.reduceLeft((x,y) => ("result",x._2 + y._2))
    sqrt(sumDiff._2)
  }

  private def myAvgWeight(dbAgent:SomDbAgent):Map[String,Double] = {
    val childNum = dbAgent.getChildDocNum(id) match {
      case Some(n) => n
      case None => 1.0
    }
    val avgWeight = mutable.Map.empty[String,Double]
    for((word,count) <- weight) {
      avgWeight += (word -> count/childNum)
    }
    //convert to immutable map
    val w = avgWeight.toList
    val m = Map.empty[String,Double]
    m ++ w
  }

  def getTopWords(dbName:String):List[String] = {
    //Get 5 words with the highest rating
    val dbAgent = new CouchAgent(dbName)
    val offsetList = calcOffsetWeight(myAvgWeight(dbAgent),dbAgent).toList
    offsetList.sort((x,y)=>x._2 < y._2).take(5).map(x=>x._1)
  }

}
