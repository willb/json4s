package org.json4s

import playground._
//import playground.ValueProvider
import org.json4s.ParserUtil.ParseException

import util.control.Exception._

object JsonValueProvider {
  
}
  
class JsonValueProvider(override protected val data:JValue, val prefix:String = "") extends ValueProvider[JValue] {

  lazy val keySet: Set[String] = data match {
    case obj:JObject => obj.obj.map { case (k,jv) => k } toSet
    case arr:JArray  => (0 until arr.arr.length) map { _.toString } toSet
    case _ => Set()
  }
  
  val separated: Separator = by.Dots
  def indexSeparator: ArraySeparator = squareBraketArraySeparator

  def contains(key: String): Boolean = get(key) match {
    case Some(_) => true
    case None    => false
  }
  
  // This should spit out the raw type, not the JValue?
  def get(key: String): Option[Any] = get(key, data)
  
  def forPrefix(key: String): JsonValueProvider = new JsonValueProvider(
    get(key,data) getOrElse {throw new java.util.NoSuchElementException(s"key $key not found!")},
    separated.wrap(key, prefix)
  )
  
  def isComplex(key: String): Boolean = get(key, data) map { _ match {
    case _:JObject => true
    case _ => false
  }} getOrElse false
  
  def isArray(key: String): Boolean = get(key, data) map { _ match {
    case _:JArray => true
    case _ => false
  }} getOrElse false
  
  def values: JValue = data // Seems redundent
  
  protected def get(path: String, jv: JValue):Option[JValue] = {
    val (part,rest) = indexSeparator.stripFirstIndex(path)
    if(indexSeparator.isArray(path)) {  // Started with array indexing
      val index = indexSeparator.getIndex(path).get
      if (rest.isEmpty ) {
        Some(jv(index))
      } else {
        // need to strip '.' if there is one
        get(
          if (rest.startsWith(separated.beginning)) rest.substring(separated.beginning.length,rest.length) 
          else rest,
          jv(index)
        )
      }
    } else {  // Didn't start with a array indexing selection
      indexSeparator.getIndex(path) match {
        case Some(i) if rest.isEmpty => Some((jv \ part)(i))
        case Some(i) => get (rest, (jv \ part)(i))
        case None if rest.isEmpty => Some(jv \ part)
        case None => get (rest, jv \ part)
      }
    }
  }
  
  // this may be tough to get right in order to prune individual keys
  // Will have to build large portions of the tree again...
  def --(keys: Iterable[String]): JsonValueProvider = ???
  
}
 