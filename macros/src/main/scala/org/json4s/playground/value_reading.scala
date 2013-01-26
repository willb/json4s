package playground

import util.control.Exception._

trait ValueProvider[S]  {
  def prefix: String
  protected def data: S
  def separated: Separator
  def read(key: String): Either[Throwable, Option[Any]] = allCatch either { get(key) }
  def get(key: String): Option[Any]
  def apply(key: String): Any = get(key) get
  def forPrefix(key: String): ValueProvider[S]
  def values: S
  def keySet: Set[String]
  def keyCount:Int = keySet.size
  def --(keys: Iterable[String]): ValueProvider[S]
  def isComplex(key: String): Boolean
  def contains(key: String): Boolean
  
  def isArray(key: String): Boolean
  def read(index: Int): Either[Throwable, Option[Any]] = allCatch either { get(index) }
  def get(index: Int): Option[Any]
  def apply(index: Int): Any = get(index) get
  def indexCount:Int = {
    keySet.map { rawKey =>
      separated.getIndex(rawKey)
    }.toSet size
  }
}

object ValueProvider {
  implicit def mapToMapValueReader(data: Map[String,Any]):ValueProvider[_] = new MapValueReader(data)
  //implicit def jsonToValueReader
}

object MapValueReader {
  
  def apply(data: Map[String, Any], separated: Separator = by.Dots) = new MapValueReader(data, separated = separated)
}
// TODO: make work with array style
class MapValueReader(protected val data: Map[String, Any], val prefix: String = "", val separated: Separator = by.Dots) extends ValueProvider[Map[String, Any]] {
  
  def get(index: Int) = get(separated.wrapIndex(index))
  
  def get(key: String):Option[Any] = {
    if (separated.startsWithArray(key)) data.get(prefix + key)
    else data.get(separated.wrap(key, prefix))
  }

  def forPrefix(key: String): ValueProvider[Map[String, Any]] = new MapValueReader(
    data,
    if (separated.startsWithArray(key)) separated.wrap(prefix + key)
    else separated.wrap(key, prefix),
    separated
  )
  
  
  lazy val values: Map[String, Any] = stripPrefix(data)

  def keySet: Set[String] = data.keys.collect{ 
    case (key) if key.startsWith(prefix) => separated.topLevelOnly(key,prefix)
  }.toSet

  def --(keys: Iterable[String]) = new MapValueReader(data -- keys.map(separated.wrap(_, prefix)), prefix, separated)

  def isComplex(key: String) = {
    val pref = separated.wrap(key, prefix)
    if (pref != null && pref.trim.nonEmpty) {
      data exists {
        case (k, _)  =>
          separated.stripPrefix(k, prefix).contains(separated.beginning) && k.startsWith(pref + separated.beginning)
      }
    } else false
  }
  
  def isArray(key: String): Boolean = false

  def contains(key: String): Boolean = (data contains separated.wrap(key, prefix)) || isComplex(key)

  private[this] def stripPrefix(d: Map[String, Any]): Map[String, Any] = {
    if (prefix != null && prefix.trim.nonEmpty) {
      d collect {
        case (k, v) if k startsWith (prefix + separated.beginning) => separated.stripPrefix(k, prefix) -> v
      }
    } else d
  }
}
