package org.json4s

import scala.reflect.runtime.universe._
import java.util.Date


case class A(a: Int, b: Double, c: String, d: Date, e: List[Long], f: Option[String] = None)
case class B(a: BigDecimal, `here-we-go`: String)

class C {
  val f1 = 2
  var f2 = 3

  def m1 = 4
  def m2() = 5
  def m3[T >: String <: Int]: T = ???
  def m4[A[_], B <: A[Int]](x: A[B])(implicit y: Int) = ???
  def m5(x: => Int, y: Int*): String = ???

  class C
  object M

  override def toString = "an instance of C"
}
object M

object ReflectionTest extends App {

//  val a = A(1, 3.394, "c", new Date, List(1,3,4,5))
  val b = A(1, 3.394, "c", new Date, List(1,3,4,5))

  val cm = scala.reflect.runtime.currentMirror

  println(cm.reflectClass(cm.staticClass("org.json4s.C")))
  println(cm.reflectModule(cm.staticModule("org.json4s.M")))
  println(cm.reflect(new C))

  val im = cm.reflect(new C)
  println(im.reflectField(typeOf[C].member(newTermName("f1")).asTerm))
  println(im.reflectField(typeOf[C].member(newTermName("f2")).asTerm))
  println(im.reflectMethod(typeOf[C].member(newTermName("m1")).asMethod))
  println(im.reflectMethod(typeOf[C].member(newTermName("m2")).asMethod))
  println(im.reflectMethod(typeOf[C].member(newTermName("m3")).asMethod))
  println(im.reflectMethod(typeOf[C].member(newTermName("m4")).asMethod))
  println(im.reflectMethod(typeOf[C].member(newTermName("m5")).asMethod))
  println(im.reflectClass(typeOf[C].member(newTypeName("C")).asClass))
  println(im.reflectModule(typeOf[C].member(newTermName("M")).asModule))

  val c = cm.staticClass("org.json4s.A")

  val cc = typeOf[C].member(newTypeName("C")).asClass
  println(cm.reflectClass(c).reflectConstructor(c.typeSignature.member(nme.CONSTRUCTOR).asMethod))
  println(im.reflectClass(cc).reflectConstructor(cc.typeSignature.member(nme.CONSTRUCTOR).asMethod))

  println(c.typeSignature.member(nme.CONSTRUCTOR).asMethod.paramss.flatMap(_.map(_.name.decoded)))

  val a = cm.classSymbol(classOf[A])
  val clm = cm.reflectClass(a)
  println(clm.reflectConstructor(a.typeSignature.member(nme.CONSTRUCTOR).asMethod))
//  println(cm.reflectClass(cm.classSymbol(classOf[B])).reflectConstructor(nme.CONSTRUCTOR))

}