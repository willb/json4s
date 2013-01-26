package playground

abstract class Separator(val beginning: String, end: String, val arrayBeginning:String, val arrayEnd: String) {

  val hasBeginning = beginning != null && beginning.trim.nonEmpty
  val hasEnd = end != null && end.trim.nonEmpty

  def wrap(part: String, prefix: String = "") = {
    val hasPrefix = prefix != null && prefix.trim.nonEmpty
    if (hasPrefix) prefix + wrapped(part)
    else part
  }

  def wrapped(part: String) = {
    val sb = new StringBuilder
    if (hasBeginning && !part.startsWith(beginning))
      sb.append(beginning)
    sb.append(part)
    if (hasEnd && !part.endsWith(end))
      sb.append(end)
    sb.toString()
  }

  def stripFirst(key: String) = {
    val endIndex = if (hasEnd) key.indexOf(end) else -1
    def rest = {
      val realEnd = endIndex + end.size
      val hasMore = key.size > (realEnd + 1)
      if (hasMore) key.substring(realEnd) else ""
    }
    if (hasBeginning && key.startsWith(beginning)) {
      if (hasEnd && endIndex > -1) {
         key.substring(beginning.size, endIndex) + rest
      } else key.substring(beginning.size)
    } else if (hasBeginning && hasEnd && endIndex > -1 && endIndex < key.indexOf(beginning)) {
      key.substring(0, endIndex) + rest
    } else key
  }

  def topLevelOnly(key: String, prefix: String = "") = {
    val path = stripPrefix(key, prefix)
    val startIndex = path.indexOf(beginning)
    if (startIndex > -1)
      path.substring(0, startIndex)
    else {
      val endIndex = path.indexOf(end)
      if (hasEnd && endIndex > -1)
        path.substring(0, endIndex)
      else path
    }
  }

  def stripPrefix(path: String, prefix: String) = {
    val hasPrefix = prefix != null && prefix.trim.nonEmpty
    if (hasPrefix && path.startsWith(prefix)) {
      stripFirst(path.substring(prefix.length))
    }
    else stripFirst(path)
  }

  // ArraySeparator methods
  val arrayHasEnd = arrayEnd != null && arrayEnd.trim.nonEmpty

  def startsWithArray(key: String): Boolean = key.startsWith(arrayBeginning)

  def wrapIndex(index: Int): String = {
    if (arrayHasEnd) arrayBeginning + index.toString + arrayEnd
    else arrayBeginning + index.toString
  }

  def appendIndex(key: String, index: Int): String = key + wrapIndex(index)

  def hasArray(key: String): Boolean = key.indexOf(arrayBeginning) >= 0

  def getIndex(key: String): Option[Int] = {
    val indexStart = key.indexOf(arrayBeginning)+arrayBeginning.length
    val indexEnd = if (arrayHasEnd) key.indexOf(arrayEnd) else key.length
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

    val indexEnd = if (arrayHasEnd) {
      key.indexOf(arrayEnd) + arrayEnd.length
    } else {
      val i = key.substring(indexStart+arrayBeginning.length,key.length).indexOf(arrayBeginning) + arrayBeginning.length
      if (i < 0) key.length else i+indexStart
    }

    (key.substring(0,indexStart), key.substring(indexEnd,key.length))
  }

}