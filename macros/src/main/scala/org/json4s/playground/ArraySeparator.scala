package playground

object squareBracketArraySeparator extends ArraySeparator("[","]")

abstract class ArraySeparator(val arrayBeginning:String, val arrayEnd: String) {
  // We require a beginning otherwise name collisions could occur
  val hasEnd = arrayEnd != null && arrayEnd.trim.nonEmpty
  
  def startsWithArray(key: String): Boolean = key.startsWith(arrayBeginning)
  
  def wrapIndex(index: Int): String = {
    if (hasEnd) arrayBeginning + index.toString + arrayEnd
    else arrayBeginning + index.toString
  }
  
  def appendIndex(key: String, index: Int): String = key + wrapIndex(index)

  def hasArray(key: String): Boolean = key.indexOf(arrayBeginning) >= 0
  
  def getIndex(key: String): Option[Int] = {
    val indexStart = key.indexOf(arrayBeginning)+arrayBeginning.length
    val indexEnd = if (hasEnd) key.indexOf(arrayEnd) else key.length
    try {
      Some(key.substring(indexStart, indexEnd) toInt)
    } catch {
      case _: Throwable => None
    }
  }
  
  // Gives the start and the remainder
  def stripFirstIndex(key: String): (String,String) = {
    val indexStart = key.indexOf(arrayBeginning)
    if(indexStart < 0) return (key,"")
    
    val indexEnd = if (hasEnd) {
      key.indexOf(arrayEnd) + arrayEnd.length
    } else {
      val i = key.substring(indexStart+arrayBeginning.length,key.length).indexOf(arrayBeginning) + arrayBeginning.length
      if (i < 0) key.length else i+indexStart
    }
    
    (key.substring(0,indexStart), key.substring(indexEnd,key.length))
  }
  
  

}
