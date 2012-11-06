package org.json4s

import scala.reflect.runtime.{currentMirror => cm, universe}
import scala.reflect.runtime.universe._

object MirrorReader extends ReflectiveReader {

  def readConstructor(argName: String, clazz: Class[_], typeArgIndex: Int, argNames: List[String]): Class[_] = {
    val klass = cm.reflectClass(cm.classSymbol(clazz))
    val constr = findConstructor(klass, argNames)
    constr map { case (ctor, args) => }
    ???
  }

  def readField(name: String, clazz: Class[_], typeArgIndex: Int): Class[_] = ???

  private[this] def findConstructor(clazz: ClassMirror, argNames: List[String]): Option[(MethodSymbol, List[universe.Symbol])] = {
    val ctor = clazz.symbol.typeSignature.member(nme.CONSTRUCTOR).asMethod
    val ctorParams = ctor.paramss
    ctorParams.sortBy(_.size).reverse find { plist =>
      val names = plist.map(_.name.decoded)
      names.sorted == argNames.sorted
    } map { (ctor, _) }
  }
}