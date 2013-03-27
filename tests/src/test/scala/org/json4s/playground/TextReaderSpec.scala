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

  val textReader = new TextReader {}

  "TextReader" should {
    "Find next object" in {
      textReader.findNextObject(json) must_==
        (""" "cats": 34, "dogs": { "cats": true, "fish": [1, "cats", [1, 2, 3]] } """, "")
    }
    "Recursively find next object" in {
      textReader.findNextObject(textReader.findNextObject(json)._1)._1 must_==
        """ "cats": true, "fish": [1, "cats", [1, 2, 3]] """
    }

    "Find next array" in {
      textReader.findNextArray(json) must_== ("""1, "cats", [1, 2, 3]""", " } }")
    }

    "Unescape string properly" in {
      textReader.unescapeString("abc\\\"\\\\\\/\\b\\f\\n\\r\\t\\u00a0") must_== "abc\"\\/\b\f\n\r\t\u00a0"
    }

    "Strip down a string" in {
      textReader.findNextString(" , \"Hello world! \\\" this is cool \\\" \" ") must_== ("Hello world! \" this is cool \" ", " ")
    }

    "Strip down a number" in {
      textReader.findNextNumber("34 }") must_== ("34", " }")
      textReader.findNextNumber("34, ") must_== ("34", ", ")
      textReader.findNextNumber("34.54, ") must_== ("34.54", ", ")
      textReader.findNextNumber("-34e-5, ") must_== ("-34e-5", ", ")
    }

    "Find a boolean" in {
      textReader.findNextBoolean("true, ") must_== ("true", ", ")
      textReader.findNextBoolean("false, ") must_== ("false", ", ")
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

    "Parse compound object" in {
      val json =
        """"count": 1, "one" : { "cats": 34, "dogs": "Hello World!" }"""
      val reader = new TextObjectReader(json)
      Macros.deserialize[AnimalFarm](reader) must_== AnimalFarm(1, Animals(34, "Hello World!"))

    }

  }
}
