package org.json4s
package playground

import org.specs2.mutable.Specification

/**
 * @author Bryce Anderson
 * Created on 3/25/13 at 9:57 PM
 */

class TextReaderSpec extends Specification {
  val json =
    """{ "cats": 34, "dogs": { "cats": true, "fish": [1, "cats", [1, 2, 3]] } }"""



  "TextReader" should {
    "Find next object" in {
      (new JsonTextCursor(json)).findNextObject() must_==
        """ "cats": 34, "dogs": { "cats": true, "fish": [1, "cats", [1, 2, 3]] } """
    }

    "Recursively find next object" in {
      val r1 = new JsonTextCursor(json)
      val str = r1.findNextObject()
      str must_== """ "cats": 34, "dogs": { "cats": true, "fish": [1, "cats", [1, 2, 3]] } """

      val r2 = new JsonTextCursor(str)
      r2.findNextObject() must_== """ "cats": true, "fish": [1, "cats", [1, 2, 3]] """
    }

    "Find next array" in {
      val cursor = new JsonTextCursor(json)
        cursor.findNextArray() must_== """1, "cats", [1, 2, 3]"""
        cursor.remainder must_== " } }"
    }

    "Unescape string properly" in {
      (new JsonTextCursor("")).unescapeString("abc\\\"\\\\\\/\\b\\f\\n\\r\\t\\u00a0") must_== "abc\"\\/\b\f\n\r\t\u00a0"
    }

    "Strip down a string" in {
      (new JsonTextCursor("\"Hello world! \\\" this is cool \\\" \" "))
        .findNextString() must_== "Hello world! \" this is cool \" "
    }

    "Strip down a number" in {
      val r1 = new JsonTextCursor("34 }")
      r1.findNextNumber() must_== "34"
      r1.remainder must_== " }"

      val r2 = new JsonTextCursor("34, ")
      r2.findNextNumber() must_== "34"
      r2.remainder must_== ", "

      val r3 = new JsonTextCursor("34.54, ")
      r3.findNextNumber() must_== "34.54"
      r3.remainder must_== ", "

      val r4 = new JsonTextCursor("-34e-5, ")
      r4.findNextNumber() must_== "-34e-5"
      r4.remainder must_== ", "
    }

    "Find a boolean" in {
      val r1 = new JsonTextCursor("true, ")
      r1.findNextBoolean() must_== true
      r1.remainder must_== ", "

      val r2 = new JsonTextCursor("false, ")
      r2.findNextBoolean() must_== false
      r2.remainder must_== ", "
    }

    "break down an object" in {
      val reader = new TextObjectReader(json.substring(1, json.length-1))
      reader.fields must_==
        ("cats", JsonNumber("34"))::
          ("dogs", JsonObject(""" "cats": true, "fish": [1, "cats", [1, 2, 3]] """))::Nil

      val reader2 = new TextObjectReader(""" "ca[]ts": true, "fi{sh": [1, "cats", [1, 2, 3]] """)
      reader2.fields must_==
        ("ca[]ts", JsonBool(true))::
          ("fi{sh", JsonArray("""1, "cats", [1, 2, 3]"""))::Nil
    }

    "break down an array" in {
      val reader = new TextArrayIterator(""" 3, false, { "cat": "cool" }, [ 1, 2] """)
      reader.nextInt must_== 3
      reader.nextBool must_== false
      reader.nextObjectReader.asInstanceOf[TextObjectReader].fields must_== ("cat", JsonString("cool") )::Nil
      val r2 = reader.nextArrayReader
      r2.nextInt must_== 1
      r2.nextInt must_== 2
    }
  }

  "TextReader with Macro serializer" should {
    import org.json4s.Macros
    implicit val defaultFormats = DefaultFormats


    case class Animals(cats: Int, dogs: String)
    case class AnimalFarm(count: Int, one: Animals)

    "Parse simple object" in {
      val json =
        """"cats": 34, "dogs": "Hello World!""""
      val reader = new TextObjectReader(json)
      Macros.deserialize[Animals](reader) must_== Animals(34, "Hello World!")
    }

    "Parse simple array" in {
      val json =
        """1, 2, 3"""
      val reader = new TextArrayIterator(json)
      Macros.deserialize[List[Int]](reader) must_== 1::2::3::Nil
    }

    "Parse compound object" in {
      val json =
        """"count": 1, "one" : { "cats": 34, "dogs": "Hello World!" }"""
      val reader = new TextObjectReader(json)
      Macros.deserialize[AnimalFarm](reader) must_== AnimalFarm(1, Animals(34, "Hello World!"))
    }

    "Parse object with array" in {
      case class WithArray(in: Double, lst: List[String])
      val json =
        """"in": 1.2431, "lst": [ "one", "two" , "three" ]"""
      val reader = new TextObjectReader(json)
      Macros.deserialize[WithArray](reader) must_== WithArray(1.2431, "one"::"two"::"three"::Nil)
    }

    "Parse array of objects" in {
      case class Simple(one: Int, two: Boolean)
      val json =
        """{"one": 1, "two": true}, {"one": 11, "two": false}"""
      val reader = new TextArrayIterator(json)
      Macros.deserialize[List[Simple]](reader) must_== Simple(1, true)::Simple(11, false)::Nil
    }
  }
}
