package operations

import scala.collection.mutable

object NamedIsometryRegistry {
  private val registry = mutable.Map[String, Isometry]()
  
  def saveIsometry(name: String, isometry: Isometry): Unit = {
    require(name.nonEmpty, "Name cannot be empty")
    require(isometry != null, "Isometry cannot be null")
    registry(name) = isometry
  }
  
  def getIsometry(name: String): Option[Isometry] = {
    registry.get(name)
  }
  
  def getAllNames: Seq[String] = {
    registry.keys.toSeq.sorted
  }
  
  def removeIsometry(name: String): Boolean = {
    registry.remove(name).isDefined
  }
  
  def clear(): Unit = {
    registry.clear()
  }
} 