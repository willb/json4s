package org.json4s

import org.specs2.mutable.Specification
import java.util.Date
import Macros._

import native.JsonMethods._

class MacroSerializerSpec extends Specification {

  case class Simple(one: Int, two:String)
  case class WithOption(one: Int, two: Option[String])
  case class WithOptionSimple(option: Option[Simple])
  case class NotSimple(one: Int, simple: Simple)
  case class WithList(lst: List[Int])
  case class ObjWithListMap(lst: List[Int], map: Map[String,Int])
  case class WithDate(date: Date)
  case class WithSymbol(symbol: Symbol)

  type Foo = Simple
  case class WithTypeAlias(in: Foo)
  case class Generic[A](in: A)

  implicit val defaultFormats = DefaultFormats

  "Macro Serializer" should {

    "Build primitive case classes" in {
      val writer = JsonWriter.ast
      val sim = Simple(1, "two")
      serialize(sim, writer)

      writer.result must_== JObject(
          ("one", JInt(1))::("two", JString("two"))::Nil: List[JField]
        )
    }

    "Serializer should do compound objects" in {
      val writer = JsonWriter.ast
      val notSimple = NotSimple(1, Simple(1,"two"))
      serialize(notSimple, writer)

      writer.result must_== JObject(("one", JInt(1))::("simple",JObject(
          ("one", JInt(1))::("two", JString("two"))::Nil
        ))::Nil)
    }

    "Serialize a present Option in a case class" in {
      val writer = JsonWriter.ast
      val a = WithOption(1, Some("string"))

      serialize(a, writer)

      writer.result must_== JObject(("one", JInt(1))::("two",JString("string"))::Nil)
    }

    "Serialize a missing Option in a case class" in {
      val writer = JsonWriter.ast
      val a = WithOption(1, None)

      serialize(a, writer)

      writer.result must_== JObject(("one", JInt(1))::("two", JNothing)::Nil)
    }

    "Serialize a case class with an option class present" in {
      val writer = JsonWriter.ast
      val a = WithOptionSimple(Some(Simple(1, "two")))

      serialize(a, writer)
      writer.result must_== JObject(("option" -> JObject(("one" -> JInt(1))::("two" -> JString("two"))::Nil)))
    }

    "Serialize a case class with an option class missing" in {
      val writer = JsonWriter.ast
      val a = WithOptionSimple(None)

      serialize(a, writer)
      writer.result must_== JObject(("option" -> JNothing))
    }

    "serialize a List of primitives" in {
      val writer = JsonWriter.ast
      val a = (4 to 60).toList
      serialize(a, writer)

      writer.result must_== JArray(a.map(JInt(_)))
    }

    "serialize a object with a list" in {
      val writer = JsonWriter.ast
      val lst = (0 to 60).toList
      val a = WithList(lst)
      serialize(a, writer)
      writer.result must_== JObject(("lst" -> JArray(lst.map(JInt(_)))))
    }

    "serialize a List of Simples" in {
      val writer = JsonWriter.ast
      val a = Simple(4, "four")::Simple(5, "five")::Simple(6, "six")::Nil
      serialize(a, writer)

      writer.result must_== JArray(
          JObject(("one", JInt(4))::("two", JString("four"))::Nil)::
          JObject(("one", JInt(5))::("two", JString("five"))::Nil)::
          JObject(("one", JInt(6))::("two", JString("six"))::Nil)::Nil
        )
    }

    "serialize a Map[Str,Int]" in {
      val writer = JsonWriter.ast
      val a = Map("one" -> 1, "two" -> 2, "three" -> 3)
      serialize(a, writer)

      writer.result must_== JObject(("one", JInt(1))::("two", JInt(2))::("three", JInt(3))::Nil)
    }

    "serialize a Map[Str,Simple(Int,String)]" in {
      val writer = JsonWriter.ast
      val a = Map("one" -> Simple(1, "one"), "two" -> Simple(2, "two"), "three" -> Simple(3, "three"))
      serialize(a, writer)

      writer.result must_== JObject(
          ("one", JObject(("one",JInt(1))::("two", JString("one"))::Nil))::
          ("two", JObject(("one",JInt(2))::("two", JString("two"))::Nil))::
          ("three", JObject(("one",JInt(3))::("two", JString("three"))::Nil))::Nil
        )
    }

    "serialize case class ObjWithListMap(lst:List[Int],map:Map[String,Int])" in {
      val writer = JsonWriter.ast
      val a = ObjWithListMap(3::4::5::Nil,Map("one" -> 1, "two" -> 2, "three" -> 3))
      serialize(a, writer)

      writer.result must_== JObject(
          ("lst", JArray((3 to 5).map(JInt(_)).toList))::
          ("map", JObject(("one", JInt(1))::("two", JInt(2))::("three", JInt(3))::Nil))::Nil
        )
    }

    "Serialize a Date" in {
      val writer = JsonWriter.ast
      val date = new java.util.Date
      serialize(WithDate(date), writer)
      val json = writer.result

      json must_== JObject(List(("date",JString(defaultFormats.dateFormat.format(date)))))
    }

    "Serialize a Symbol" in {
      val writer = JsonWriter.ast
      val sym = 'cool
      serialize(WithSymbol(sym), writer)
      val json = writer.result

      json must_== JObject(List(("symbol", JString("cool"))))
    }

    "Work with type params" in {
      val w = WithTypeAlias(new Foo(1,"2"))
      val writer = JsonWriter.ast
      serialize(w, writer)

      writer.result must_== JObject(("in",
        JObject(
          ("one",JInt(1))::("two",JString("2"))::Nil
        ))::Nil)
    }

    "Work with generic params" in {
      val w = Generic(1)
      val writer = JsonWriter.ast
      serialize(w, writer)

      writer.result must_== JObject(("in",JInt(1))::Nil)
    }

    "Serialize with decomposeWithBuilder" in {
      val writer = JsonWriter.ast
      val sym = 'cool
      val json = decomposeWithBuilder(WithSymbol(sym), writer)

      json must_== JObject(List(("symbol", JString("cool"))))
    }
  }
}