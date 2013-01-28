package org.json4s

import playground._
//import playground.ValueProvider
import org.json4s.ParserUtil.ParseException

import util.control.Exception._

object JsonValueProvider {
  def apply(data: JValue) = new JsonValueProvider(data)
}
  
class JsonValueProvider(override protected val data:JValue, val prefix:String = "", val separated: Separator = by.Dots) extends ValueProvider[JValue] {

  private[this] def extractPrimative(in: JValue): Any = in match {
    case jv: JBool => jv.value
    case jv: JDecimal => jv.num
    case jv: JDouble => jv.num
    case jv: JInt => jv.num
    case jv: JString => jv.s
    case jv => jv
  }

  def get(index: Int): Option[Any] = data match {
    case JArray(arr) => Some(extractPrimative(arr(index)))
    case _ => None
  }

  lazy val keySet: Set[String] = data match {
    case obj:JObject => obj.obj.map{ case (k,jv) => k }.toSet
    case arr:JArray  => (0 until arr.arr.length).map{ _.toString }.toSet
    case _ => Set()
  }

  def contains(key: String): Boolean = get(key) match {
    case Some(_) => true
    case None    => false
  }
  
  // This should spit out the raw type, not the JValue?
  def get(key: String): Option[Any] = getJson(key) map extractPrimative

  def getJson(key: String): Option[JValue] = get(key,data)
  
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
  
  def values: JValue = data // Seems redundant
  
  protected def get(path: String, jv: JValue):Option[JValue] = {
    
    // Rely only on the AST, not pimping functions from MonadicJValue
    def objPart(jv: JValue,key:String): JValue = jv match {
      case JObject(l) => (l.find ( _._1 == key)) map (_._2) getOrElse JNothing
      case _ => JNothing
    }
    if(separated.startsWithIndex(path)) {  // Started with array indexing
      val rest = separated.stripFrontIndex(path)
      val index = separated.getIndex(path).get
      try {
        if (rest.isEmpty ) {
          Some(jv(index))
        } else  get(rest, jv(index))
      } catch {
        case e: IndexOutOfBoundsException => Some(JNothing)
      }
    } else {  // Didn't start with a array indexing selection
      val part = separated.topLevelOnly(path)
      val rest = separated.stripPrefix(path,part)
      if (rest.nonEmpty) {
        get(rest, objPart(jv,part))
      } else Some(objPart(jv,part))
    }
  }
  
  // this may be tough to get right in order to prune individual keys
  // Will have to build large portions of the tree again...
  def --(keys: Iterable[String]): JsonValueProvider = ???
  
}
 