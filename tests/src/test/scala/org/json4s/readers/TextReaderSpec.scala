package org.json4s
package macro_readers

import org.specs2.mutable.Specification

/**
 * @author Bryce Anderson
 * Created on 3/25/13 at 9:57 PM
 */

class TextReaderSpec extends Specification {
  val json =
    """{ "cats": 34, "dogs": { "cats": true, "fish": [1, "cats", [1, 2, 3]] } }"""



  "JsonTextCursor" should {
    "Find next object" in {
      val r1 = new JsonStringCursor(json)
      val abj = r1.extractField()
      abj must beAnInstanceOf[JsonObject]
      r1.remainder must_== ""
    }

    "Find next string" in {
      val cursor = new JsonStringCursor(""""Hello world" """)
      cursor.findNextString() must_== JsonString("Hello world")
      cursor.remainder must_== " "
    }

    "Find next string with escaped escape" in {
      val cursor = new JsonStringCursor(""""Hello world\\" """)
      cursor.findNextString() must_== JsonString("""Hello world\""")
      cursor.remainder must_== " "
    }

    "Unescape string properly" in {
      (new JsonStringCursor("\"abc\\\"\\\\\\/\\b\\f\\n\\r\\t\\u00a0\"")).findNextString() must_==
        JsonString("abc\"\\/\b\f\n\r\t\u00a0")
    }

    "Strip down a string" in {
      (new JsonStringCursor("\"Hello world! \\\" this is cool \\\" \" "))
        .findNextString() must_== JsonString("Hello world! \" this is cool \" ")
    }

    "Strip down a number" in {
      val r1 = new JsonStringCursor("34 }")
      r1.findNextNumber() must_== JsonNumber("34")
      r1.remainder must_== " }"

      val r2 = new JsonStringCursor("34, ")
      r2.findNextNumber() must_== JsonNumber("34")
      r2.remainder must_== ", "

      val r3 = new JsonStringCursor("34.54, ")
      r3.findNextNumber() must_== JsonNumber("34.54")
      r3.remainder must_== ", "

      val r4 = new JsonStringCursor("-34e-5, ")
      r4.findNextNumber() must_== JsonNumber("-34e-5")
      r4.remainder must_== ", "
    }

    "Find a boolean" in {
      val r1 = new JsonStringCursor("true, ")
      r1.findNextBoolean() must_== JsonBool(true)
      r1.remainder must_== ", "

      val r2 = new JsonStringCursor("false, ")
      r2.findNextBoolean() must_== JsonBool(false)
      r2.remainder must_== ", "
    }

    "break down an object" in {
      val reader = JsonTextReader.bindText(json)
      reader.asInstanceOf[TextObjectReader].fields must_==
        ("cats", JsonNumber("34"))::
        ("dogs", JsonObject(new TextObjectReader(new JsonStringCursor(
          """{ "cats": true, "fish": [1, "cats", [1, 2, 3]]} """
        ))))::Nil

      val reader2 = JsonTextReader.bindText("""{ "ca[]ts": true, "fi{sh": [1, "cats", [1, 2, 3]] }""")
      reader2.asInstanceOf[TextObjectReader].fields must_==
        ("ca[]ts", JsonBool(true))::
          ("fi{sh", JsonArray(JsonTextReader.bindText("""[1, "cats", [1, 2, 3]]""").asInstanceOf[TextArrayIterator]))::Nil
    }

    "break down an array" in {
      val reader = new TextArrayIterator(new JsonStringCursor("""[ 3, false, { "cat": "cool" }, [ 1, 2]] """))
      reader.nextInt must_== 3
      reader.nextBool must_== false
      reader.nextObjectReader.asInstanceOf[TextObjectReader].fields must_== ("cat", JsonString("cool") )::Nil
      val r2 = reader.nextArrayReader
      r2.nextInt must_== 1
      r2.nextInt must_== 2
    }

    "Throw a ParseException on bad json" in {
      case class Simple(one: Int, two: Boolean)
      val json =  """[{"one": 1, "two": true}, "one": 11, "two": false}]"""
      (new TextArrayIterator(new JsonStringCursor(json))) must throwA[ParserUtil.ParseException]
    }

    "Throw a MappingException on wrong type of json" in {
      case class Simple(one: Int, two: Boolean)
      val json =  """[{"one": 1, "two": true}, "one": 11, "two": false}]"""
      (new TextObjectReader(new JsonStringCursor(json))) must throwA[MappingException]
    }


    "Throw a ParseException on bad json" in {
      case class Simple(one: Int, two: Boolean)
      val json =  """{"one": 1, "two": gtrue}"""
      (new TextObjectReader(new JsonStringCursor(json))) must throwA[ParserUtil.ParseException]
    }
  }

  "JsonTextReader helpers" should {
    "Extract an object" in {
      val r = JsonTextReader.bindText(json)
      r must beAnInstanceOf[TextObjectReader]
      //r.asInstanceOf[TextObjectReader].remainder must_== ""
    }

    "Extract an array" in {
      val r = JsonTextReader.bindText("""[1, "cats", [1, 2, 3]]""")
      r must beAnInstanceOf[TextArrayIterator]
     // r.asInstanceOf[TextArrayIterator].remainder must_== "1, \"cats\", [1, 2, 3]"
    }
  }

  "JsonReaderCursor" should {
    "Find next object" in {
      val r1 = new JsonReaderCursor(new java.io.StringReader(json))
      val abj = r1.extractField()
      abj must beAnInstanceOf[JsonObject]
    }

    "Find next string" in {
      val cursor = new JsonReaderCursor(new java.io.StringReader(""""Hello world" """))
      cursor.findNextString() must_== JsonString("Hello world")
    }
//
//    "Find next string with escaped escape" in {
//      val cursor = new JsonStringCursor(""""Hello world\\" """)
//      cursor.findNextString() must_== JsonString("""Hello world\""")
//      cursor.remainder must_== " "
//    }
//
//    "Unescape string properly" in {
//      (new JsonStringCursor("\"abc\\\"\\\\\\/\\b\\f\\n\\r\\t\\u00a0\"")).findNextString() must_==
//        JsonString("abc\"\\/\b\f\n\r\t\u00a0")
//    }
//
//    "Strip down a string" in {
//      (new JsonStringCursor("\"Hello world! \\\" this is cool \\\" \" "))
//        .findNextString() must_== JsonString("Hello world! \" this is cool \" ")
//    }
//
//    "Strip down a number" in {
//      val r1 = new JsonStringCursor("34 }")
//      r1.findNextNumber() must_== JsonNumber("34")
//      r1.remainder must_== " }"
//
//      val r2 = new JsonStringCursor("34, ")
//      r2.findNextNumber() must_== JsonNumber("34")
//      r2.remainder must_== ", "
//
//      val r3 = new JsonStringCursor("34.54, ")
//      r3.findNextNumber() must_== JsonNumber("34.54")
//      r3.remainder must_== ", "
//
//      val r4 = new JsonStringCursor("-34e-5, ")
//      r4.findNextNumber() must_== JsonNumber("-34e-5")
//      r4.remainder must_== ", "
//    }
//
//    "Find a boolean" in {
//      val r1 = new JsonStringCursor("true, ")
//      r1.findNextBoolean() must_== JsonBool(true)
//      r1.remainder must_== ", "
//
//      val r2 = new JsonStringCursor("false, ")
//      r2.findNextBoolean() must_== JsonBool(false)
//      r2.remainder must_== ", "
//    }
//
//    "break down an object" in {
//      val reader = JsonTextReader.bindText(json)
//      reader.asInstanceOf[TextObjectReader].fields must_==
//        ("cats", JsonNumber("34"))::
//          ("dogs", JsonObject(new TextObjectReader(new JsonStringCursor(
//            """{ "cats": true, "fish": [1, "cats", [1, 2, 3]]} """
//          ))))::Nil
//
//      val reader2 = JsonTextReader.bindText("""{ "ca[]ts": true, "fi{sh": [1, "cats", [1, 2, 3]] }""")
//      reader2.asInstanceOf[TextObjectReader].fields must_==
//        ("ca[]ts", JsonBool(true))::
//          ("fi{sh", JsonArray(JsonTextReader.bindText("""[1, "cats", [1, 2, 3]]""").asInstanceOf[TextArrayIterator]))::Nil
//    }
//
//    "break down an array" in {
//      val reader = new TextArrayIterator(new JsonStringCursor("""[ 3, false, { "cat": "cool" }, [ 1, 2]] """))
//      reader.nextInt must_== 3
//      reader.nextBool must_== false
//      reader.nextObjectReader.asInstanceOf[TextObjectReader].fields must_== ("cat", JsonString("cool") )::Nil
//      val r2 = reader.nextArrayReader
//      r2.nextInt must_== 1
//      r2.nextInt must_== 2
//    }
//
//    "Throw a ParseException on bad json" in {
//      case class Simple(one: Int, two: Boolean)
//      val json =  """[{"one": 1, "two": true}, "one": 11, "two": false}]"""
//      (new TextArrayIterator(new JsonStringCursor(json))) must throwA[ParserUtil.ParseException]
//    }
//
//    "Throw a MappingException on wrong type of json" in {
//      case class Simple(one: Int, two: Boolean)
//      val json =  """[{"one": 1, "two": true}, "one": 11, "two": false}]"""
//      (new TextObjectReader(new JsonStringCursor(json))) must throwA[MappingException]
//    }
//
//
//    "Throw a ParseException on bad json" in {
//      case class Simple(one: Int, two: Boolean)
//      val json =  """{"one": 1, "two": gtrue}"""
//      (new TextObjectReader(new JsonStringCursor(json))) must throwA[ParserUtil.ParseException]
//    }
  }

  "JsonTextReader helpers" should {
    "Extract an object" in {
      val r = JsonTextReader.bindText(json)
      r must beAnInstanceOf[TextObjectReader]
    }

    "Extract an array" in {
      val r = JsonTextReader.bindText("""[1, "cats", [1, 2, 3]]""")
      r must beAnInstanceOf[TextArrayIterator]
    }
  }

  "JsonTextReader with Macro serializer" should {
    implicit val defaultFormats = DefaultFormats


    case class Animals(cats: Int, dogs: String)
    case class AnimalFarm(count: Int, one: Animals)

    "Parse simple object" in {
      val json = """{"cats": 34, "dogs": "Hello World!"}"""
      Macros.read[Animals](json) must_== Animals(34, "Hello World!")
    }

    "Parse simple array" in {
      val json =  """[1, 2, 3]"""
      Macros.read[List[Int]](json) must_== 1::2::3::Nil
    }

    "Parse compound object" in {
      val json =  """ {"count": 1, "one" : { "cats": 34, "dogs": "Hello World!" }}"""
      Macros.read[AnimalFarm](json) must_== AnimalFarm(1, Animals(34, "Hello World!"))
    }

    "Parse object with array" in {
      case class WithArray(in: Double, lst: List[String])
      val json = """{ "in": 1.2431, "lst": [ "one", "two" , "three" ] }"""
      Macros.read[WithArray](json) must_== WithArray(1.2431, "one"::"two"::"three"::Nil)
    }

    "Parse array of objects" in {
      case class Simple(one: Int, two: Boolean)
      val json =  """ [{"one": 1, "two": true}, {"one": 11, "two": false}]"""
      Macros.read[List[Simple]](json) must_== Simple(1, true)::Simple(11, false)::Nil
    }
  }

  "JsonAST" should {
    "Escape a string properly" in {
      val str = "He\tllo \" world!"
      JsonAST.quote(str) must_== "He\\tllo \\\" world!"
    }
  }
}
