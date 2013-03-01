package org.json4s

import org.specs2.mutable.Specification
import java.util.Date
import org.json4s.ParserUtil.ParseException
import Macros._
import collection.mutable

case class Junk(in1:Int, in2:String)
case class MutableJunk(var in1:Int,var in2:String)
case class MutableJunkWithField(var in1:Int) {
  var in2:String = ""
}
case class ThingWithJunk(name:String, junk:Junk)
case class Crazy(name:String,thg:ThingWithJunk)
case class WithOption(in:Int,opt:Option[String])
case class OptionOption(in:Option[Option[Int]])
case class JunkWithDefault(in1:Int, in2:String="Default...")
case class WithList(name:String, lst:List[Int])
case class WithObjList(name:String, list:List[ThingWithJunk])
case class Curried(in1:Int,in2:Int)(in3:Int)
case class WithTpeParams[U](in1:U)
class WithNstedTpeParams[U,U2](val in1: U, val in2:WithTpeParams[U2]) {
  override def equals(in:Any) = in match {
    case in:WithNstedTpeParams[U,U2] => in.in1 == in.in1 && in.in2 == in2
	case _ => false
  }
}
case class ResolvedParams[U](in3: U, in4:WithTpeParams[Int]) extends WithNstedTpeParams[U,Int](in3,in4)

case class Bill(in:Int)

class Billy[U](in:U)
case class BillyB(in:Int) extends Billy[Int](in)

case class WithDate(name:String, date: Date) {
  override def equals(in:Any) = in match {
    case that: WithDate => name == that.name && date.toString == that.date.toString
	case _ => false
  }
}

class ClassWithDef(val in: Int=4) {
  override def toString = s"ClassWithDef(in:$in)"
  override def equals(obj:Any) = obj match {
	  case a:ClassWithDef => a.in == in
	  case _ => false
  }
}

class Stopwatch {

  private var startTime = -1L
  private var stopTime = -1L
  private var running = false

  def start(): Stopwatch = {
    startTime = System.currentTimeMillis()
    running = true
    this
  }

  def stop(): Stopwatch = {
    stopTime = System.currentTimeMillis()
    running = false
    this 
  }

  def isRunning(): Boolean = running

  def getElapsedTime() = {
    if (startTime == -1) {
      0
    }
    if (running) {
      System.currentTimeMillis() - startTime
    }
    else {
      stopTime - startTime
    }
  }

  def reset() {
    startTime = -1
    stopTime = -1
    running = false
  }
}

case class ObjWithDefJunk(name:String, junk:Junk=Junk(-1,"Default"))

class MacroDeserializerSpec extends Specification {
  import JsonDSL._
  implicit val defaultFormats = DefaultFormats
  val refJunk = Junk(2,"cats")
  //val refJunkDict = Map("d.in1"->refJunk.in1.toString,"d.in2"->refJunk.in2)
  val refJunkDict: JValue = serializeObj(refJunk)

  "Macros.deserialize" should {

    "Build maps of primatives with string key" in {
      val expected = Map[String, Int](("a" -> 1), ("b" -> 2), ("c" -> 3))
      val params: JValue = expected

      deserialize[Map[String,Int]](params) must_== expected
    }

    "Build maps of primatives with Int key" in {
      val expected = Map(1 -> 1, 2 -> 2, 100 -> 3)
      val data: JValue = expected.map{case (k,v) => (k.toString, v)}

      deserialize[Map[Int,Int]](data) must_== expected
    }


    "Build maps of Junks with string key" in {
      //val data: JValue = "dd.a.in1"->"1","dd.a.in2"->"aaa","dd.b.in1" ->"2","dd.b.in2"->"bbb", "dd.c.in1" -> "3","dd.c.in2"->"ccc")
      val data: JValue = ("a" -> (("in1" -> 1) ~ ("in2" -> "aaa"))) ~
        ("b" -> (("in1" -> 2) ~ ("in2" -> "bbb"))) ~
        ("c" -> (("in1" -> 3) ~ ("in2" -> "ccc")))

      val expected = Map("a"->Junk(1,"aaa"),"b"->Junk(2,"bbb"),"c"->Junk(3,"ccc"))

      deserialize[Map[String,Junk]](data) must_== expected
    }

    "Build a map of objects with type parameters" in {
      //val data = Map("dd.a.in1"->"2","dd.b.in1"->"3","dd.d.in1"->"4")
      val data: JValue = ("a" -> ("in1" -> 2)) ~ ("b" -> ("in1" -> 3)) ~ ("c" -> ("in1" -> 4))
      val expected = Map("a" -> WithTpeParams(2), "b" -> WithTpeParams(3), "c" -> WithTpeParams(4))
      deserialize[Map[String,WithTpeParams[Int]]](data) must_== expected
    }

    "Make a lot of things" in {
      //Junk(in1:Int, in2:String)
      //ThingWithJunk(name:String, junk:Junk)
      val stuff = new scala.collection.mutable.MutableList[ThingWithJunk]()
      val params = new mutable.MutableList[JObject]()
      import scala.util.Random
      val numObjs = 60000
      val cycles = 1000
      (0 until numObjs) foreach{ i =>
        val thng = ThingWithJunk("name_"+i,Junk(Random.nextInt,"junker"+Random.nextInt))
        stuff += thng
        val json: JObject = ("name" -> thng.name) ~ ("junk" -> (("in1" -> thng.junk.in1) ~ ("in2" -> thng.junk.in2)))
        params += json
      }
      val fullParams = params.toList
      val stopwatch = new Stopwatch
      var result = deserialize[List[ThingWithJunk]](fullParams) // Warmup
      stopwatch.start
      (0 until cycles).foreach { _ =>
        result = deserialize[List[ThingWithJunk]](fullParams)
      }
      stopwatch.stop
      println(s"--------------- Time to deserialize ${numObjs*cycles} objects: ${stopwatch.getElapsedTime} millisec ------------------")
      result must_== stuff.toList
    }

    "Primative Int" in {
      val expected:Int = 5
      val params = JInt(5)
      deserialize[Int](params) must_== expected
    }

    "Primative Long" in {
      val expected:Long = 5
      val params = JInt(5)
      deserialize[Long](params) must_== expected
    }

    "Generate a Junk" in {
      deserialize[Junk](refJunkDict) must_== refJunk
    }

    "Generate a MutableJunk" in {
      deserialize[MutableJunk](refJunkDict) must_== MutableJunk(2,"cats")
    }

    "Generate a MutableJunkWithField when field provided" in {
      val expected = MutableJunkWithField(2)
      expected.in2 = "cats"
      deserialize[MutableJunkWithField](refJunkDict) must_== expected

    }

    "Generate a MutableJunkWithField when field missing" in {
      val expected = MutableJunkWithField(2)
      val params = JObject(("in1" -> JInt(2))::Nil)
      deserialize[MutableJunkWithField](params) must_== expected

    }

    "Generate a ThingWithJunk" in {
      val expected = ThingWithJunk("Bob", Junk(2, "SomeJunk..."))
      val stuff =("name" -> "Bob") ~ ("junk" -> ("in1" -> 2) ~ ("in2" -> expected.junk.in2))
      val result = deserialize[ThingWithJunk](stuff)
      result must_== expected
    }

    "Generate a 3 fold deap case class" in {
      val expected = Crazy("crazyBob...",ThingWithJunk("Bob",Junk(2,"SomeJunk...")))
      val stuff = ("name" -> expected.name) ~ ( "thg" ->
        ( "name" -> expected.thg.name) ~ ( "junk" ->
          ("in1" -> 2) ~ ("in2" -> expected.thg.junk.in2)
          )
      )

      val result = deserialize[Crazy](stuff)
      result must_== expected
    }

    "Parse date info" in {
      val expected = WithDate("Bob", new Date)
      val params = ("name" -> expected.name) ~ ("date" -> defaultFormats.dateFormat.format(expected.date))

      deserialize[WithDate](params) must_== expected
    }

    "Created ClassWithDef with param" in {
      val params = ("in" -> 1)
      deserialize[ClassWithDef](params) must_== (new ClassWithDef(1))
    }

    "Created ClassWithDef without param" in {
      val params = JObject(Nil)
      deserialize[ClassWithDef](params) must_== (new ClassWithDef)
    }

    "Generate a JunkWithDefault with a value" in {
      var expected = JunkWithDefault(refJunk.in1,refJunk.in2)
      deserialize[JunkWithDefault](refJunkDict) must_== expected
    }

    "Generate a JunkWithDefault without a value" in {
      var expected = JunkWithDefault(refJunk.in1)
      val params: JObject = ("in1" -> 2)    // TODO: causes problems if not explicitly wrapped as JObject
      deserialize[JunkWithDefault](params) must_== expected
    }

    "Created ObjWithDefJunk without junk" in {
      val expected = ObjWithDefJunk("Name")
      val params: JObject = ("name" -> "Name")
      deserialize[ObjWithDefJunk](params) must_== expected
    }

    "Created ObjWithDefJunk with provided junk" in {
      val expected = ObjWithDefJunk("Name",Junk(2,"Provided"))
      val params = ("name" -> "Name") ~ ("junk" ->
          ("in1" -> 2) ~ ("in2" -> "Provided")
        )
      deserialize[ObjWithDefJunk](params) must_== expected
    }

    "Instance a case class with an Option" in {
      val expected = WithOption(2,Some("Pizza pockets forever!"))
      val params = ("in" -> expected.in) ~ ("opt" -> expected.opt.get)
      deserialize[WithOption](params) must_== expected
    }

    "Instance a case class with a missing Option" in {
      val expected = WithOption(2, None)
      val params: JObject = ("in" -> expected.in)
      deserialize[WithOption](params) must_== expected
    }

    "Generate a recursive Option" in {
      val expected = OptionOption(Some(Some(5)))
      val params: JObject = ("in" -> 5)
      val result = deserialize[OptionOption](params)
      result must_== expected
    }

    "Handle type parameters" in {
      val expected = WithTpeParams(100)
      val params: JValue = ("in1" -> expected.in1)
      deserialize[WithTpeParams[Int]](params) must_== expected
    }

    "Handle a tuple" in {
      val expected = (2,3,"cats")
      val params: JValue = ("_1" -> expected._1) ~ ("_2" -> expected._2) ~ ("_3" -> expected._3)
      deserialize[(Int, Int, String)](params) must_== expected
    }

    "Handle nested type parameters, WithNstedTpeParams[U,U2](U, WithTpeParams[U2])" in {
      val expected = new WithNstedTpeParams("cat",WithTpeParams(100))
      val params: JValue = ("in1" -> expected.in1) ~ ("in2" -> ("in1" -> expected.in2.in1))
      deserialize[WithNstedTpeParams[String, Int]](params) must_== expected
    }

    "Handle partially resolved, ResolvedParams[U](in3: U, in4:WithTpeParams[Int])" in {
      val expected = new ResolvedParams("cat",WithTpeParams(100))
      val params = ("in3" -> expected.in3) ~ ("in4" -> ("in1" -> expected.in4.in1))
      deserialize[ResolvedParams[String]](params) must_== expected
    }

    "Curried case class" in {
      val expected = Curried(1,2)(3)
      val params = ("in1" -> expected.in1) ~ ("in2" -> expected.in2) ~ ("in3" -> 3)
      deserialize[Curried](params) must_== expected
    }

    "parse List[Int]" in {
        val expected = 1::2::3::4::Nil
      val params: JValue = expected
      val result = deserialize[List[Int]](params)
      result must_== expected
    }

    "parse List[WithTpeParams[String]]" in {
        val expected = WithTpeParams("one")::WithTpeParams("two")::Nil
        val params: JValue = List( ("in1" -> "one"), ("in1" -> "two"))
      val result = deserialize[List[WithTpeParams[String]]](params)
      result must_== expected
    }

    "parse empty List[Int]" in {
        val expected:List[Int] = Nil
        val params: JValue = expected
      val result = deserialize[List[Int]](params)
      result must_== expected
    }

    "parse List[List[Int]]" in {
        val expected = (1::2::Nil)::(3::4::Nil)::Nil
        val params: JValue = expected
      val result = deserialize[List[List[Int]]](params)

      result must_== expected
    }

    "Parse WithList" in {
      val expected = WithList("Bob", 1::4::Nil)
      val params: JValue = ("name" -> "Bob") ~ ("lst" -> (1::4::Nil))
      deserialize[WithList](params) must_== expected
    }

    "parse WithObjList" in {
        val expected = WithObjList("Bob",ThingWithJunk("Bobby",Junk(1,"one"))::ThingWithJunk("Bill",Junk(2,"two"))::Nil)
        val params: JValue = ("name" -> "Bob") ~ ("list" ->
            ((("name" -> "Bobby") ~ ("junk" -> (("in1" -> 1)~("in2" -> "one"))))
              ::(("name" -> "Bill") ~ ("junk" -> (("in1" -> 2)~("in2" -> "two"))))::Nil)
          )
        deserialize[WithObjList](params) must_== expected
      }

    "parse List[Bill]" in {
    //case class ThingWithJunk(name:String, junk:Junk)
        val expected = Bill(1)::Bill(3)::Nil
        val params: JValue = List(("in" -> 1),("in" -> 3))
        deserialize[List[Bill]](params) must_== expected
      }

    "parse BillyB which extends Billy[Int]" in {
      val expected = BillyB(3)
      val params: JValue = ("in" -> 3)
      deserialize[BillyB](params) must_== expected
    }

    "Throw ParseException with a bad map value for 'in'" in {
      val params: JValue = ("in1" -> "2ffds") ~ ("in2" -> "cats")
      deserialize[Junk](params) must throwA[ParseException]
    }
  }

  "deserializeEither" should {
    "Give the Left on error" in {
      val params: JValue = ("in1" -> "2ffds") ~ ("in2" -> "cats")
      val result = deserializeEither[Junk](params)
      println(s"DEBUG The error was: ${result.left.get}")
      result must beAnInstanceOf[Left[ParseException, Junk]]
    }

    "Give the Right on Success" in {
      val params: JValue = ("in1" -> 2) ~ ("in2" -> "cats")
      val result = deserializeEither[Junk](params)
      result must beAnInstanceOf[Right[ParseException, Junk]]
    }
  }
}