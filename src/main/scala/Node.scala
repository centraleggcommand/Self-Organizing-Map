package somservice
import scala.Math._
import scala.collection.mutable

class Node(node_id:String, node_weight:Map[String,Double])
{
  val id = node_id
  val weight = node_weight

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

  //This returns the distance between the node and another weight.
  //If the weight is empty, then a 'None' value is returned.
  def calcNodeDistance( cmpWeight:Map[String,Double], dbAgent:SomDbAgent ):Double = {
    //The word counts are given a calculated weight based on the 
    //increasing importance from higher count, but decreasing importance
    //as the global usage of the word increases.
    def offsetWeight(count:Double, global:Double):Double = {
      (pow(1.02,ceil(count))) / (pow(1.008,global) + (pow(1.0008,global))/10)
    }
    //Calculate the euclidean distance between weights
    val calcDiff = for( (word,count) <- cmpWeight) yield {
      val wc:Double = dbAgent.getGlobalWordCount(word) match {
        case Some(num) => num
        case None => 1.0
      }
      val cmpOffset = offsetWeight(count, wc)
      if( weight.contains(word)) {
        val nodeWeight:Double = weight(word)
        //the node weight is converted to an average over the total
        //number of docs, then the offset is calculated
        val childNum = dbAgent.getChildDocNum(id) match {
          case Some(n) => n
          case None => 1.0
        }
        val avgNodeWeight = nodeWeight/childNum
        val nodeOffset = offsetWeight(nodeWeight, wc)
        val sqdiff = pow((nodeOffset - cmpOffset),2)
        (word -> sqdiff)
      }
      else {
        val sqdiff = pow(cmpOffset,2)
        (word -> sqdiff)
      }
    }
    //finish euclidean distance calc on squared differences in the map
    val sumDiff = calcDiff.reduceLeft((x,y) => ("result",x._2 + y._2))
    sqrt(sumDiff._2)
  }


}
