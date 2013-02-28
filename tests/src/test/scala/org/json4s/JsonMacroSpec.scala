package org.json4s

import org.specs2.mutable.Specification
import java.util.Date
import Macros._

import native.JsonMethods._  // For debugging purposes

class JsonMacroSpec extends Specification {

  // Needed to use the implicit calling style
  implicit val dateFormats = DefaultFormats

  case class Simple(one: Int, two: String)
  case class Compound(one: Int, two: Simple)

  "Macros with Json" should {
    /* // TODO: Make this work again
    "Serialize and deserialize a simple case class" in {
      val writer = JsonWriter.ast
      val obj = Simple(1, "two")
      serialize(obj, "simple", writer)

      val valueProvider = new JsonValueProvider(writer.result)

      deserialize[Simple](valueProvider,"simple") must_== obj

    }

    "Serialize and deserialize a compound case class" in {
      val writer = JsonWriter.ast
      val obj = Compound(1, Simple(1, "two"))
      serialize(obj,"compound",writer)

      val valueProvider = new JsonValueProvider(writer.result)

      deserialize[Compound](valueProvider,"compound") must_== obj

    }
    */
  }
}
