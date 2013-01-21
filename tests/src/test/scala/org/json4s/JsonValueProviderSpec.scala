package org.json4s

import native.JsonMethods._
import JsonDSL._

import org.specs2.mutable.Specification
import text.Document

class JsonValueProviderSpec extends Specification {
  val json2 = parse("""
                        {
                          "content": [
                            {"cat":4},
                            [1,2,3],
                            [{"cat":1}]
                          ],
                          "fish" : {
                            "pigs":"Hello"
                          }
                        }
                        """)
                        
  "JsonValueProvider" should {
    /*
    "Do simple selections" in {
      val json1 = read("/diff-example-json1.json")
      val provider = new JsonValueProvider(json1.asInstanceOf[JObject])
      provider("content[0]") must_== (json1 \ "content")(0)
    }
    */
    "Do nested selections" in {
      val provider = new JsonValueProvider(json2.asInstanceOf[JObject])
      provider("content.fish.pigs") must_== (json2 \ "content" \ "fish" \ "pigs")
    }
    
    "Traverse multiple indexes" in {
      val provider = new JsonValueProvider(json2.asInstanceOf[JObject])
      provider("content[1][0]") must_== JInt(1)
    }
    
    "Traverse multiple indexes" in {
      val provider = new JsonValueProvider(json2.asInstanceOf[JObject])
      provider("content[1]") must_== JArray(JInt(1)::JInt(2)::JInt(3)::Nil)
    }
    
    "Traverse multiple indexes and return objects" in {
      val provider = new JsonValueProvider(json2.asInstanceOf[JObject])
      provider("content[2][0]") must_== JObject(("cat",JInt(1))::Nil)
    }
    
    "Traverse multiple indexes and transition to objects" in {
      val provider = new JsonValueProvider(json2.asInstanceOf[JObject])
      provider("content[2][0].cat") must_== JInt(1)
    }
    
    "Produce working sub value providers" in {
      val provider = new JsonValueProvider(json2.asInstanceOf[JObject])
      val provider2 = provider.forPrefix("content[2]")
      println(provider2.values.getClass)
      provider2("[0].cat") must_== JInt(1)
    }
    // Should be consistant between bad indexes and bad keys
    "Throw IndexOutOfBoundsException on bad index" in {
      val provider = new JsonValueProvider(json2.asInstanceOf[JObject])
      provider("content[4][0].cat") must throwA[IndexOutOfBoundsException]
    }
    
    "Return JNothing on bad key" in {
      val provider = new JsonValueProvider(json2.asInstanceOf[JObject])
      provider("content[2][0].catdd") must_== JNothing
    }
    
  }
  
  private def read(resource: String) =
    parse(getClass.getResourceAsStream(resource))
}