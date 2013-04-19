package org.json4s

import org.specs2.Specification
import java.io.StringReader

object PullReaderSpec {
  val jacksonParser: (String, Boolean) => Parser = (json: String, bigDec: Boolean) => {
    new jackson.JsonParser(jackson.JsonParser.factory.createParser(json), bigDec)
  }

  val nativeParser: (String, Boolean) => Parser = (json: String, bigDec: Boolean) => {
    new native.JsonParser.Parser(new ParserUtil.Buffer(new StringReader(json), true), bigDec)
  }
}

class JacksonPullReaderSpec extends PullReaderSpec(PullReaderSpec.jacksonParser)
class NativePullReaderSpec extends PullReaderSpec(PullReaderSpec.nativeParser)
abstract class PullReaderSpec(createParser: (String, Boolean) => Parser) extends Specification { def is =
  "A pull reader should read"  ^
    "integer" ! example().integer ^
    "short" ! example().short ^
    "byte" ! example().byte ^
    "long" ! example().long ^
    "bigint" ! example().bigint ^
    "java.lang.bigint" ! example().javaBigint ^
    "double" ! example().double ^
    "float" ! example().float ^
    "bigdecimal" ! example().bigdecimal ^
    "java.lang.bigdecimal" ! example().javaBigdecimal ^
    "string" ! example().string ^
    "date" ! example().date ^
    "Map[String, Int]" ! pending ^
    "List[String]" ! pending ^
    "Array[String]" ! pending ^
    "JValue" ! pending ^
    "JObject" ! pending ^
    "JArray" ! pending ^
  end

  case class example() {
    def integer = {
      val json = "2"
      val parser = createParser(json, false)
      DefaultPullReaders.IntReader.read(parser) must_== 2
    }
    def short = {
      val json = "2"
      val parser = createParser(json, false)
      DefaultPullReaders.ShortReader.read(parser) must_== 2.toShort
    }
    def byte = {
      val json = "2"
      val parser = createParser(json, false)
      DefaultPullReaders.ByteReader.read(parser) must_== 2.toByte
    }
    def long = {
      val json = (Long.MaxValue - 10003093).toString
      val parser = createParser(json, false)
      DefaultPullReaders.LongReader.read(parser) must_== Long.MaxValue - 10003093
    }
    def bigint = {
      val json = (Long.MaxValue - 10003093).toString
      val parser = createParser(json, false)
      DefaultPullReaders.BigIntReader.read(parser) must_== BigInt(Long.MaxValue - 10003093)
    }
    def javaBigint = {
      val json = (Long.MaxValue - 10003093).toString
      val parser = createParser(json, false)
      DefaultPullReaders.JavaBigIntReader.read(parser) must_== java.math.BigInteger.valueOf(Long.MaxValue - 10003093)
    }
    def double = {
      val json = "23030.3003"
      val parser = createParser(json, false)
      DefaultPullReaders.DoubleReader.read(parser) must beCloseTo(23030.3003, 1)
    }
    def float = {
      val json = "23030.3003"
      val parser = createParser(json, false)
      DefaultPullReaders.FloatReader.read(parser) must beCloseTo(23030.3003f, 1)
    }
    def bigdecimal = {
      val json = "23030.3003"
      val parser = createParser(json, true)
      DefaultPullReaders.BigDecimalReader.read(parser) must_== BigDecimal(json)
    }
    def javaBigdecimal = {
      val json = "23030.3003"
      val parser = createParser(json, true)
      DefaultPullReaders.JavaBigDecimalReader.read(parser) must_== new java.math.BigDecimal(json)
    }
    def string = {
      val json = "\"hello\""
      val parser = createParser(json, false)
      DefaultPullReaders.StringReader.read(parser) must_== "hello"
    }
    def date = {
//      val json = "hello"
//      val parser = createParser(json, false)
//      DefaultPullReaders.JavaBigIntReader.read(parser) must_== java.math.BigInteger.valueOf(Long.MaxValue - 10003093)
      pending
    }
  }
}