package playground

import annotation.tailrec

abstract class Separator(val beginning: String, end: String, val arrayBeginning:String, val arrayEnd: String) {

  val hasBeginning = beginning != null && beginning.trim.nonEmpty
  val hasEnd = end != null && end.trim.nonEmpty

  val arrayHasEnd = arrayEnd != null && arrayEnd.trim.nonEmpty

  private[this] val endLength = if (hasEnd) end.length else 0
  private[this] val arrayEndLength = if (arrayHasEnd) arrayEnd.length else 0

  /*  Wrap keys into an existing prefix. Does not to level changing for map type keys.
   *  Can handle single keys and multiple indexes at the same time.
   *  {{{
   *  wrap("foo","") = foo
   *  wrap("foo","bar") = bar[foo]
   *  wrap("(0).foo,"bar" = bar(0)[foo]
   *  }}}
   */
  def wrap(part: String, prefix: String = ""): String = {
    val hasPrefix = prefix != null && prefix.trim.nonEmpty
    if (!hasPrefix) return part
    var rest = part
    var result = prefix
    while (startsWithIndex(rest)) { // Deal with appending indexes
      result = appendIndex(result,getIndex(rest).get)
      rest = splitAtFirstIndex(rest)._2
    }
    // Strip off any beginning remaining
    if (hasEnd && rest.startsWith(end)) rest = rest.substring(endLength,rest.length)
    if (rest.nonEmpty) result + wrapped(rest)
    else result
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

  // Gives the top level only as either a raw key or a wrapped index
  def topLevelOnly(key: String, prefix: String = ""): String = {
    val path = stripPrefix(key, prefix)
    // Do we start with an index?
    if (startsWithIndex(path)) {
      //wrapIndex(getIndex(path).get)
      path.substring(0,getEndIndexPosition(path) + arrayEndLength)
    } else {
      @ inline def indexManip(in: Int) = {
        if (in < 0) Int.MaxValue
        else in
      }

      val startKeyToken = indexManip(path.indexOf(beginning))
      val startArrayToken = indexManip(path.indexOf(arrayBeginning))
      val result = math.min(startKeyToken, startArrayToken)

      if (result != Int.MaxValue) {
        path.substring(0,result)
      } else path
    }
  }

  /* Strips some key from the provided path.
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
      // No key, just strip tags if needed
    else stripFirst(path)
  }

  // ArraySeparator methods

  def startsWithIndex(key: String): Boolean = key.startsWith(arrayBeginning)

  def hasIndex(key: String): Boolean = key.indexOf(arrayBeginning) >= 0

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

  def getIndex(key: String): Option[Int] = {
    val indexStart = key.indexOf(arrayBeginning)+arrayBeginning.length
    val indexEnd = getEndIndexPosition(key)
    try {
      Some(key.substring(indexStart, indexEnd).toInt)
    } catch {
      case _: Throwable => None
    }
  }

  def getIndexes(key: String): List[Int] = {
    getIndex(key) match {
      case Some(i) => i::getIndexes(splitAtFirstIndex(key)._2)
      case None    => Nil
    }
  }

  // Gives the strings before and after the first index
  def splitAtFirstIndex(key: String): (String,String) = {
    val indexStart = key.indexOf(arrayBeginning)
    if(indexStart < 0) return (key,"")
    (key.substring(0,indexStart), key.substring(getEndIndexPosition(key) + arrayEndLength,key.length))
  }

  def stripFrontIndex(key: String): String = {
    if (key.startsWith(arrayBeginning)) {
      val stub = key.substring(
        getEndIndexPosition(key) + arrayEndLength,
        key.length
      )
      stripFirst(
        if (hasEnd && stub.startsWith(end)) stub.substring(end.length, stub.length)
        else stub
      )
    } else key
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

  def appendIndex(key: String, index: Int): String = {
    if (hasEnd && key.indexOf(end) > 0) {
      key.substring(0, key.length - endLength) + wrapIndex(index) + end
    } else key + wrapIndex(index)
  }

  private[this] def getEndIndexPosition(key: String): Int = {
    if (arrayHasEnd) {
      key.indexOf(arrayEnd)
    } else {  // Look for the next character that isn't a number
    var currentIndex = key.indexOf(arrayBeginning) + arrayBeginning.length
      while (currentIndex < key.length && key(currentIndex) <= '9' && '0' <= key(currentIndex)) {
        currentIndex += 1
      }
      currentIndex
    }
  }
}
