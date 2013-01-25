package playground

object squareBraketArraySeparator extends ArraySeparator("[","]")

abstract class ArraySeparator(val beginning:String, val end: String) {
  // We require a beginning otherwise name collisions could occure
  val hasEnd = end != null && end.trim.nonEmpty
  
  def startsWithArray(key: String): Boolean = key.startsWith(beginning)
  
  def wrapIndex(index: Int): String = {
    if (hasEnd) beginning + index.toString + end
    else beginning + index.toString
  }
  
  def appendIndex(key: String, index: Int): String = key + wrapIndex(index)

  def hasArray(key: String): Boolean = key.indexOf(beginning) >= 0
  
  def getIndex(key: String): Option[Int] = {
    val indexStart = key.indexOf(beginning)+beginning.length
    val indexEnd = if (hasEnd) key.indexOf(end) else key.length
    try {
      Some(key.substring(indexStart, indexEnd) toInt)
    } catch {
      case _: Throwable => None
    }
  }
  
  // Gives the start and the remainder
  def stripFirstIndex(key: String): (String,String) = {
    val indexStart = key.indexOf(beginning)
    if(indexStart < 0) return (key,"")
    
    val indexEnd = if (hasEnd) {
      key.indexOf(end) + end.length 
    } else {
      val i = key.substring(indexStart+beginning.length,key.length).indexOf(beginning) + beginning.length
      if (i < 0) key.length else i+indexStart
    }
    
    (key.substring(0,indexStart), key.substring(indexEnd,key.length))
  }
  
  

}
