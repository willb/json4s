package playground

import annotation.tailrec

abstract class Separator(val beginning: String, end: String, val arrayBeginning:String, val arrayEnd: String) {

  val hasBeginning = beginning != null && beginning.trim.nonEmpty
  val hasEnd = end != null && end.trim.nonEmpty
  private[this] val endLength = if (hasEnd) end.length else 0

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

  // TODO: make this friendly to arrays
  /** Strips the separators from around the first key
    * eg, stripFirst("[foo][bar]") -> "foo[bar]"
    * don't want to strip indexes (0)[foo] -> (0)[foo]
   */
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

  // TODO: Consider how to deal with arrays, or not deal with them and punt to an array friendly method
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

  /* Strips some prefix from the provided path.
   * {{{
   * val path = "baz[foo(0)][bar]
   * stripPrefix(path, "baz") == foo(0)[bar]
   * stripPrefix(path, "baz[foo(0)] = bar
   * stripPrefix(path, "baz[foo] = (0)[bar]
   * }}}
   */
  def stripPrefix(path: String, prefix: String) = {
    val hasPrefix = prefix != null && prefix.trim.nonEmpty
    if (hasPrefix && (endsWithIndex(prefix) == false) &&
        startsWithIndex(path.substring(prefix.length - endLength, path.length))) {
      // case like stripPrefix("cats[dogs(0)][pigs]", cats[dogs])
      val part = path.substring(path.indexOf(arrayBeginning),path.length)
      if (arrayHasEnd) {
        val endIndex = part.indexOf(end)
        part.substring(0, endIndex) + part.substring(endIndex + endLength, part.length)
      } else part
    } else if (hasPrefix && path.startsWith(prefix)) {
      // normal case
      stripFirst(path.substring(prefix.length))
    }
      // No prefix, just strip tags if needed
    else stripFirst(path)
  }

  // ArraySeparator methods
  @inline
  val arrayHasEnd = arrayEnd != null && arrayEnd.trim.nonEmpty

  @inline
  def startsWithIndex(key: String): Boolean = key.startsWith(arrayBeginning)

  def endsWithIndex(key: String): Boolean = {
    var i: String = key
    var continue = true
    while (continue) {
      val (a,b) = splitAtFirstIndex(i)
      if (a == i) continue = false
      else i = b
    }

    i == (if (hasEnd) end else "")
  }

  def wrapIndex(index: Int): String = {
    if (arrayHasEnd) arrayBeginning + index.toString + arrayEnd
    else arrayBeginning + index.toString
  }

  @inline
  def appendIndex(key: String, index: Int): String =  key + wrapIndex(index)

  @inline
  def hasIndex(key: String): Boolean = key.indexOf(arrayBeginning) >= 0

  def getIndex(key: String): Option[Int] = {
    val indexStart = key.indexOf(arrayBeginning)+arrayBeginning.length
    val indexEnd = if (arrayHasEnd) key.indexOf(arrayEnd) else key.length
    try {
      Some(key.substring(indexStart, indexEnd) toInt)
    } catch {
      case _: Throwable => None
    }
  }

  // Gives the strings before and after the first index
  def splitAtFirstIndex(key: String): (String,String) = {
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

  def stripTailingIndex(key: String): String = {
    @tailrec
    def findLastIndex(key: String, i:Int): Int = {
      val index = key.indexOf(arrayBeginning)
      if (index <= 0) i
      else {
        val (_,b) = key.splitAt(index)
        findLastIndex(b,index)
      }
    }
    key.substring(0,findLastIndex(key,0)) + ( if (hasEnd) end else "" )
  }

}