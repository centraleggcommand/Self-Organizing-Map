package somservice
import scala.collection.mutable

class Node(node_id:String, node_weight:Map[String,Double])
{
  val id = node_id
  val weight = node_weight
  def updateWeight(incoming:Map[String,Double]):Node = {
    val upWeight = mutable.Map.empty[String,Double]
    for ((word,count) <- incoming ) {
      if( weight.contains(word) )
        upWeight += (word -> (weight.apply(word) + count))
      else upWeight += (word -> count)
    }
    //convert to immutable map
    val w = upWeight.toList
    val m = Map.empty[String,Double]
    new Node(id, m ++ w)
  }
}
