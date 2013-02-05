package org.json4s

import org.specs2.mutable.Specification
import java.util.Date
import Macros._

import native.JsonMethods._

class MacroSerializerSpec extends Specification {

  case class Simple(one:Int, two:String)
  case class WithOption(one:Int, two: Option[String])
  case class NotSimple(one:Int,simple:Simple)
  case class ObjWithListMap(lst:List[Int],map:Map[String,Int])

  implicit val defaultFormats = DefaultFormats
  
  def jsonPrint(value:JValue) = compact(render(value))
  "Macro Serializer" should {

    "Serialize primatives" in {
      val writer = JsonWriter.ast
      serialize(453,"number",writer)
      writer.result must_== JObject(("number", JInt(453)))
    }

    "Build primative case classes" in {
      val writer = JsonWriter.ast
      val sim = Simple(1,"two")
      serialize(sim,"simp",writer)

      writer.result must_== JObject(("simp",
        JObject(
          ("one", JInt(1))::("two", JString("two"))::Nil
        )
      ))
    }

    "Serializer should do compound objects" in {
      val writer = JsonWriter.ast
      val notSimple = NotSimple(1,Simple(1,"two"))
      serialize(notSimple,"NotSimple",writer)

      writer.result must_== JObject(("NotSimple",
        JObject(("one", JInt(1))::("simple",JObject(
          ("one", JInt(1))::("two", JString("two"))::Nil
        ))::Nil)
      ))
    }

    "Serialize a present Option" in {
      val writer = JsonWriter.ast
      serialize(Some(43),"numberOpt",writer)

      writer.result must_== JObject(("numberOpt", JInt(43)))
    }

    "Serialize a present Optional complex type" in {
      val writer = JsonWriter.ast
      serialize(Some(Simple(1,"two")),"SimpleOpt",writer)

      writer.result must_== JObject(("SimpleOpt",
        JObject(("one", JInt(1))::("two", JString("two"))::Nil)
      ))
    }

    "Serialize a missing Option" in {
      val writer = JsonWriter.ast
      val a:Option[Int] = None
      serialize(a,"missing_number",writer)

      writer.result must_== JObject(("missing_number",JNothing))
    }

    "Serialize a present Option in a case class" in {
      val writer = JsonWriter.ast
      val a:WithOption = WithOption(1,Some("string"))

      serialize(a,"withOption",writer)

      writer.result must_== JObject(("withOption",
        JObject(("one",JInt(1))::("two",JString("string"))::Nil)
      ))
    }

    "Serialize a missiong Option in a case class" in {
      val writer = JsonWriter.ast
      val a:WithOption = WithOption(1,None)

      serialize(a,"withOption",writer)

      writer.result must_== JObject(("withOption",
        JObject(("one",JInt(1))::("two",JNothing)::Nil)
        ))
    }

    "serialize a List of primatives" in {
      val writer = JsonWriter.ast
      val a = (4 to 60).toList
      serialize(a,"my_list",writer)

      writer.result must_== JObject(("my_list",
        JArray(a.map(JInt(_)))
      ))
    }

    "serialize a List of Simples" in {
      val writer = JsonWriter.ast
      val a = Simple(4,"four")::Simple(5,"five")::Simple(6,"six")::Nil
      serialize(a,"my_list",writer)

      writer.result must_== JObject(("my_list",
        JArray(
          JObject(("one",JInt(4))::("two",JString("four"))::Nil)::
          JObject(("one",JInt(5))::("two",JString("five"))::Nil)::
          JObject(("one",JInt(6))::("two",JString("six"))::Nil)::Nil
        )
      ))
    }

    "serialize a Map[Str,Int]" in {
      val writer = JsonWriter.ast
      val a = Map("one"->1,"two"->2,"three"->3)
      serialize(a,"my_map",writer)

      writer.result must_== JObject(("my_map",
        JObject(
          ("one", JInt(1))::("two", JInt(2))::("three", JInt(3))::Nil
        )
      ))
    }

    "serialize a Map[Str,Simple(Int,String)]" in {
      val writer = JsonWriter.ast
      val a = Map("one"->Simple(1,"one"),"two"->Simple(2,"two"),"three"->Simple(3,"three"))
      serialize(a,"my_map",writer)

      writer.result must_== JObject(("my_map",
        JObject(
          ("one", JObject(("one",JInt(1))::("two",JString("one"))::Nil))::
          ("two", JObject(("one",JInt(2))::("two",JString("two"))::Nil))::
          ("three", JObject(("one",JInt(3))::("two",JString("three"))::Nil))::Nil
        )
      ))
    }

    "serialize case class ObjWithListMap(lst:List[Int],map:Map[String,Int])" in {
      val writer = JsonWriter.ast
      val a = ObjWithListMap(3::4::5::Nil,Map("one"->1,"two"->2,"three"->3))
      serialize(a,"my_objwithlistmap",writer)

      writer.result must_== JObject(("my_objwithlistmap",
        JObject(
          ("lst", JArray((3 to 5).map(JInt(_)).toList))::
          ("map", JObject(("one",JInt(1))::("two",JInt(2))::("three",JInt(3))::Nil))::Nil
        )
      ))
    }

    // --------------- Serialize Object -------------------
    "serObject should build primative case classes" in {
      val sim = Simple(1,"two")
      val json = serObject(sim)

      json must_== JObject(("one", JInt(1))::("two", JString("two"))::Nil)
    }
  }
}