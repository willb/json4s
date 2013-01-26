package org.json4s

import org.specs2.mutable.Specification
import java.util.Date
import Macros._

import native.JsonMethods._

case class Simple(one:Int, two:String)
case class NotSimple(one:Int,simple:Simple)
case class ObjWithListMap(lst:List[Int],map:Map[String,Int])

class MacroSerializerSpec extends Specification {
  implicit val defaultFormats = DefaultFormats
  
  def jsonPrint(value:JValue) = compact(render(value))
  
  "Serializer should do primatives" in {
    val writer = JsonWriter.ast
    serialize(453,"number",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "Serializer should do primative case classes" in {
    val writer = JsonWriter.ast
    val sim = Simple(1,"three")
    serialize(sim,"simp",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "Serializer should do compound objects" in {
    val writer = JsonWriter.ast
    val notSimple = NotSimple(1,Simple(1,"two"))
    serialize(notSimple,"NotSimple",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "Serialize a present Option" in {
    val writer = JsonWriter.ast
    serialize(Some(43),"numberOpt",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "Serialize a present Optional complex type" in {
    val writer = JsonWriter.ast
    serialize(Some(Simple(1,"twotwo")),"SimpleOpt",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "Serialize a missing Option" in {
    val writer = JsonWriter.ast
    val a:Option[Int] = None
    serialize(a,"missing_number",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "serialize a List of primatives" in {
    val writer = JsonWriter.ast
    val a = 4::5::6::Nil
    serialize(a,"my_list",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "serialize a List of Simples" in {
    val writer = JsonWriter.ast
    val a = Simple(4,"four")::Simple(5,"five")::Simple(6,"six")::Nil
    serialize(a,"my_list",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "serialize a Map[Str,Int]" in {
    val writer = JsonWriter.ast
    val a = Map("one"->1,"two"->2,"three"->3)
    serialize(a,"my_map",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "serialize a Map[Str,Simple(Int,String)]" in {
    val writer = JsonWriter.ast
    val a = Map("one"->Simple(1,"one"),"two"->Simple(2,"two"),"three"->Simple(3,"three"))
    serialize(a,"my_map",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "serialize case class ObjWithListMap(lst:List[Int],map:Map[String,Int])" in {
    val writer = JsonWriter.ast
    val a = ObjWithListMap(3::4::5::Nil,Map("one"->1,"two"->2,"three"->3))
    serialize(a,"my_objwithlistmap",writer)
    println(s"Json: ${jsonPrint(writer.result)}")
    true must_== true
  }
  
  "The whole of these are crap right now because they dont test anything" in {
    true must_== false
  }
}