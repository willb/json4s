package org.json4s
package macros

import scala.language.experimental.macros
import scala.reflect.macros.Context
import org.json4s.JsonAST._
import org.json4s.reflect.TypeInfo

object MacroImpls {

  case class H1(id: Int, name: String, age: Int)

  private[this] def findByName(jv: JValue, nameToFind: String): JValue =
    findDirectByName(List(jv), nameToFind) match {
      case Nil ⇒ JNothing
      case x :: Nil ⇒ x
      case x ⇒ JArray(x)
    }

  private[this] def findDirectByName(xs: List[JValue], name: String): List[JValue] = xs.flatMap {
    case JObject(l) ⇒ l.filter {
      case (n, _) if n == name ⇒ true
      case _ ⇒ false
    } map (_._2)
    case JArray(l) ⇒ findDirectByName(l, name)
    case _ ⇒ Nil
  }

  def rdr(implicit formats: Formats) = new Reader[H1] {
    def read(value: JValue): H1 = {
      val ti = TypeInfo(classOf[H1], None)
      val deser = formats.customDeserializer
      val v = (ti, value)
      if (deser.isDefinedAt(v)) deser(v).asInstanceOf[H1]
      else {
        H1(
          findByName(value, "id").asInstanceOf[JInt].values.intValue(),
          findByName(value, "name").asInstanceOf[JString].values,
          findByName(value, "age").asInstanceOf[JInt].values.intValue())
      }
    }
  }

  class Helper[T <: Context](c: T)
}