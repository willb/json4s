package org.json4s.mongo

import org.specs2.mutable.Specification
import org.json4s.Macros.deserialize
import org.json4s.DefaultFormats
import com.mongodb.{BasicDBObject, BasicDBList}

/**
 * @author Bryce Anderson
 *         Created on 4/30/13
 */
class BsonReaderSpec extends Specification {
  implicit val defaultFormats = DefaultFormats

  val test = new BasicDBObject()
  test.put("one", "one")
  test.put("two", 2)
  val three = new BasicDBList()
  three.add(new Integer(1))
  three.add(new Integer(2))
  three.add(new Integer(3))
  test.put("three", three)

  case class Test(one: String, two: Int, three: List[Int])

  "BsonReader" should {
    "Extract List" in {
       deserialize[List[Int]](BsonReader(three)) must_== 1::2::3::Nil
    }

    "Extract an object" in {
      deserialize[Test](BsonReader(test)) must_== Test("one", 2, 1::2::3::Nil)
    }

  }

}
