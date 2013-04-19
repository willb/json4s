package org.json4s

import collection.{generic, immutable}
import scala.annotation.{tailrec, implicitNotFound}
import JsonAST._
import org.json4s.Parser._
import scala.collection.generic.CanBuildFrom

// based on the type classes from play 2 but with the conversions from lift-json
@implicitNotFound(
  "No JSON deserializer found for type ${T}. Try to implement an implicit PullReader or PullFormat for this type."
)
trait PullReader[T] {
  def read(parser: Parser): T
}

object DefaultPullReaders extends DefaultPullReaders
trait DefaultPullReaders {
  implicit object IntReader extends PullReader[Int] {
    def read(parser: Parser): Int = parser.nextToken match {
      case IntVal(x) => x.intValue
      case DoubleVal(x) => x.intValue
      case BigDecimalVal(x) => x.intValue
      case x => throw new MappingException(s"Can't convert $x to Int.")
    }
  }

  implicit object JavaBigIntReader extends PullReader[java.math.BigInteger] {
    def read(parser: Parser): java.math.BigInteger = parser.nextToken match {
      case IntVal(x) => x.bigInteger
      case DoubleVal(x) => java.math.BigInteger.valueOf(x.longValue)
      case BigDecimalVal(x) => x.toBigInt().bigInteger
      case x => throw new MappingException("Can't convert %s to Int." format x)
    }
  }

  implicit object BigIntReader extends PullReader[BigInt] {
    def read(parser: Parser): BigInt = parser.nextToken match {
      case IntVal(x) => x
      case DoubleVal(x) => BigInt(x.longValue)
      case BigDecimalVal(x) => x.toBigInt()
      case x => throw new MappingException("Can't convert %s to Int." format x)
    }
  }

  implicit object LongReader extends PullReader[Long] {
    def read(parser: Parser): Long = parser.nextToken match {
      case IntVal(x) => x.longValue
      case DoubleVal(x) => x.longValue
      case BigDecimalVal(x) => x.longValue
      case x => throw new MappingException("Can't convert %s to Long." format x)
    }
  }

  implicit object ShortReader extends PullReader[Short] {
    def read(parser: Parser): Short = parser.nextToken match {
      case IntVal(x) => x.shortValue
      case DoubleVal(x) => x.shortValue
      case BigDecimalVal(x) => x.shortValue
      case x => throw new MappingException("Can't convert %s to Short." format x)
    }
  }

  implicit object ByteReader extends PullReader[Byte] {
    def read(parser: Parser): Byte = parser.nextToken match {
      case IntVal(x) => x.byteValue
      case DoubleVal(x) => x.byteValue
      case BigDecimalVal(x) => x.byteValue
      case x => throw new MappingException("Can't convert %s to Byte." format x)
    }
  }

  implicit object FloatReader extends PullReader[Float] {
    def read(parser: Parser): Float = parser.nextToken match {
      case IntVal(x) => x.floatValue
      case DoubleVal(x) => x.floatValue
      case BigDecimalVal(x) => x.floatValue
      case x => throw new MappingException("Can't convert %s to Float." format x)
    }
  }

  implicit object DoubleReader extends PullReader[Double] {
    def read(parser: Parser): Double = parser.nextToken match {
      case IntVal(x) => x.doubleValue
      case DoubleVal(x) => x
      case BigDecimalVal(x) => x.doubleValue
      case x => throw new MappingException("Can't convert %s to Double." format x)
    }
  }

  implicit object BigDecimalReader extends PullReader[BigDecimal] {
    def read(parser: Parser): BigDecimal = parser.nextToken match {
      case IntVal(x) => BigDecimal(x)
      case DoubleVal(x) => BigDecimal(x)
      case BigDecimalVal(x) => x
      case x => throw new MappingException("Can't convert %s to BigDecimal." format x)
    }
  }

  implicit object JavaBigDecimalReader extends PullReader[java.math.BigDecimal] {
    def read(parser: Parser): java.math.BigDecimal = parser.nextToken match {
      case IntVal(x) => new java.math.BigDecimal(x.bigInteger)
      case DoubleVal(x) => java.math.BigDecimal.valueOf(x)
      case BigDecimalVal(x) => x.bigDecimal
      case x => throw new MappingException("Can't convert %s to BigDecimal." format x)
    }
  }

  implicit object BooleanReader extends PullReader[Boolean] {
    def read(parser: Parser): Boolean = parser.nextToken match {
      case BoolVal(v) => v
      case NullVal => false
      case x => throw new MappingException("Can't convert %s to Boolean." format x)
    }
  }

  implicit object StringReader extends PullReader[String] {
    def read(parser: Parser): String = parser.nextToken match {
      case IntVal(x) => x.toString
      case BigDecimalVal(x) => x.toString
      case DoubleVal(x) => x.toString
      case BoolVal(x) => x.toString
      case StringVal(s) => s
      case NullVal => null
      case x => throw new MappingException("Can't convert %s to String." format x)
    }
  }

  implicit def mapReader[V](implicit valueReader: PullReader[V]): PullReader[immutable.Map[String, V]] = new PullReader[immutable.Map[String, V]] {
    def read(parser: Parser): Map[String, V] = parser.nextToken match {
      case OpenObj =>
        def parse(fields: List[(String, V)] = Nil): List[(String, V)] = parser.nextToken match {
          case FieldStart(name) => parse(name -> valueReader.read(parser) :: fields)
          case CloseObj => fields
          case x => throw new MappingException("Can't convert %s to Map." format x)
        }
        Map(parse():_*)
      case x => throw new MappingException("Can't convert %s to Map." format x)
    }
  }

  /*
   * Allows for going one step back in the parsing process,
   * This is so that the parser can be passed on to another pull reader
   */
  private class StepBackParser(underlying: Parser) extends Parser {
    def fail(msg: String): Nothing = underlying.fail(msg)

    private[this] var _previousToken: Token = End
    private[this] var _currentToken: Token = End
    private[this] var _isRewound: Boolean = false

    def stepBack() = {
      _isRewound = true
      _currentToken = _previousToken
      _currentToken
    }

    private[this] def pushToken(t: Token): Token = {
      _isRewound = false
      _previousToken = _currentToken
      _currentToken
    }

    def nextToken: Token = {
      if (_isRewound) {
        pushToken(_currentToken)
      } else {
        pushToken(underlying.nextToken)
      }
    }
  }

  implicit def traversableReader[F[_], V](implicit cbf: CanBuildFrom[F[_], V, F[V]], valueReader: PullReader[V]) =
    new PullReader[F[V]] {
      def read(parser: Parser): F[V] = {
        val p = new StepBackParser(parser)
        p.nextToken match {
          case OpenArr =>
            val builder = cbf()
            @tailrec def parse: F[V] =
              p.nextToken match {
                case CloseArr => builder.result()
                case End => throw new MappingException(s"Can't convert $End to Traversable")
                case x =>
                  p.stepBack()
                  builder += valueReader.read(p)
                  parse
              }
            parse
          case x => throw new MappingException("Can't convert %s to Traversable." format x)
        }
      }
    }

//  implicit def arrayReader[T:Reader]: PullReader[Array[T]] = new PullReader[Array[T]] {
//    def read(parser: Parser): Array[T] = {
//      val reader = implicitly[PullReader[List[T]]]
//      reader.read(parser).toArray
//    }
//  }

  implicit object JValueReader extends PullReader[JValue] {
    def read(parser: Parser): JValue = {
      val p = new StepBackParser(parser)
      p.nextToken match {
        case IntVal(i) => JInt(i)
        case DoubleVal(i) => JDouble(i)
        case BigDecimalVal(i) => JDecimal(i)
        case BoolVal(i) => JBool(i)
        case StringVal(i) => JString(i)
        case NullVal => JNull
        case OpenObj =>
          p.stepBack()
          JObjectReader.read(p)
        case OpenArr =>
          p.stepBack()
          JArrayReader.read(p)
        case _ => JNothing
      }
    }
  }

  implicit object JObjectReader extends PullReader[JObject] {
    def read(parser: Parser): JObject = {
      parser.nextToken match {
        case OpenObj =>
          def parse(fields: List[JField] = Nil): List[JField] = parser.nextToken match {
            case FieldStart(name) => parse(name -> JValueReader.read(parser) :: fields)
            case CloseObj => fields
            case x => throw new MappingException(s"FieldStart expected, but got $x")
          }
          JObject(parse())
        case x => throw new MappingException(s"OpenObj expected, but got $x")
      }
    }
  }

  implicit object JArrayReader extends PullReader[JArray] {
    def read(parser: Parser): JArray = {
      val p = new StepBackParser(parser)
      p.nextToken match {
        case OpenArr =>
          def parse(fields: List[JValue] = Nil): List[JValue] = {
            p.nextToken match {
              case CloseArr => fields
              case _: IntVal | _: DoubleVal | _: BigDecimalVal | _: BoolVal | _: StringVal | NullVal =>
                p.stepBack()
                parse(JValueReader.read(p) :: fields)
              case x => throw new MappingException(s"Value or CloseArr expected, but got $x")
            }
          }
          JArray(parse())
        case x => throw new MappingException(s"OpenArr expected, but got $x")
      }
    }
  }

  implicit def OptionReader[T](implicit valueReader: PullReader[T]) = new PullReader[Option[T]] {
    def read(parser: Parser): Option[T] = {
      import scala.util.control.Exception.catching
      catching(classOf[RuntimeException], classOf[MappingException]) opt { valueReader read parser }
    }
  }
}

//@implicitNotFound(
//  "No JSON serializer found for type ${T}. Try to implement an implicit Writer or JsonFormat for this type."
//)
//trait StreamWriter[-T] {
//  def write(obj: T): JValue
//}
//trait DefaultStreamWriters {
//
//  protected abstract class W[-T](fn: T => JValue) extends Writer[T] {
//    def write(obj: T): JValue = fn(obj)
//  }
//
//  implicit object IntWriter extends W[Int](JInt(_))
//  implicit object ByteWriter extends W[Byte](JInt(_))
//  implicit object ShortWriter extends W[Short](JInt(_))
//  implicit object LongWriter extends W[Long](JInt(_))
//  implicit object BigIntWriter extends W[BigInt](JInt(_))
//  implicit object BooleanWriter extends W[Boolean](JBool(_))
//  implicit object StringWriter extends W[String](JString(_))
//  implicit def arrayWriter[T](implicit valueWriter: Writer[T], mf: Manifest[T]): Writer[Array[T]] = new Writer[Array[T]] {
//    def write(obj: Array[T]): JValue = JArray(obj.map(valueWriter.write(_)).toList)
//  }
//  implicit def mapWriter[V](implicit valueWriter: Writer[V]): Writer[immutable.Map[String, V]] = new Writer[Map[String, V]] {
//    def write(obj: Map[String, V]): JValue = JObject(obj.map({case (k, v) => k -> valueWriter.write(v)}).toList)
//  }
//  implicit object JValueWriter extends W[JValue](identity)
//  implicit def OptionWriter[T](implicit valueWriter: Writer[T]): Writer[Option[T]] = new Writer[Option[T]] {
//    def write(obj: Option[T]): JValue = obj match {
//      case Some(v) => valueWriter.write(v)
//      case _ => JNull
//    }
//  }
//}
//
//trait DoubleWriters extends DefaultStreamWriters {
//  implicit object FloatWriter extends W[Float](JDouble(_))
//  implicit object DoubleWriter extends W[Double](JDouble(_))
//  implicit object BigDecimalWriter extends W[BigDecimal](d => JDouble(d.doubleValue))
//}
//
//trait BigDecimalWriters extends DefaultStreamWriters {
//  implicit object FloatWriter extends W[Float](JDecimal(_))
//  implicit object DoubleWriter extends W[Double](JDecimal(_))
//  implicit object BigDecimalWriter extends W[BigDecimal](d => JDecimal(d))
//}
//
//object BigDecimalWriters extends BigDecimalWriters
//object DoubleWriters extends DoubleWriters
//object DefaultWriters extends DoubleWriters // alias for DoubleWriters
//
//
//@implicitNotFound(
//  "No Json formatter found for type ${T}. Try to implement an implicit JsonFormat for this type."
//)
//trait JsonPullFormat[T] extends StreamWriter[T] with PullReader[T]
//trait BigDecimalJsonFormats extends DefaultJsonFormats with DefaultReaders with BigDecimalWriters
//trait DoubleJsonFormats extends DefaultJsonFormats with DefaultReaders with DoubleWriters
//object BigDecimalJsonFormats extends BigDecimalJsonFormats
//object DoubleJsonFormats extends DoubleJsonFormats
//object DefaultJsonFormats extends DoubleJsonFormats
//
//trait DefaultJsonFormats {
//
//  implicit def GenericFormat[T](implicit reader: PullReader[T], writer: Writer[T]): JsonPullFormat[T] = new JsonPullFormat[T] {
//    def write(obj: T): JValue = writer.write(obj)
//    def read(parser: Parser): T = reader.read(parser)
//  }
//}

