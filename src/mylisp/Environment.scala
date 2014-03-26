package mylisp
import scala.collection.mutable.{Map => MutableMap}

class Environment(val parent:Option[Environment] = None, val context: Option[Any] = None, last: Boolean = true){

  val variables = MutableMap[String, Any]()

  def get(key:String):Any = {
    if(variables.contains(key)){
      variables(key)
    } else {
      parent match {
        case Some(p) => p.get(key)
        case None => throw new Exception("symbol '%s' not found".format(key))
      }
    }
  }

  def set(key:String, value:Any): Unit = {
    if(variables.contains(key) || parent == None){
      variables(key) = value
    } else {
      parent.get.set(key, value)
    }
  }

  def define(key: String, local: Boolean): Unit = {
    if(local || parent == None){
      variables.put(key, Nil)
    } else {
      parent.get.define(key, local)
    }
  }

}
