package org.json4s.macroimpls.macrohelpers

import scala.reflect.macros.Context
import org.json4s.{JsonWriter, JValue, JNothing, JObject}
import java.util.Date


class MacroHelpers[C <: Context](val c1: C) {
  
  import c1.universe._
  
  def LIT[B](x: B) = c1.Expr[B](Literal(Constant(x)))
  
  // For building objects that take type parameters
  def typeArgumentTree(t: c1.Type): c1.Tree = t match {
    case TypeRef(_, _, typeArgs @ _ :: _) => AppliedTypeTree(
          Ident(t.typeSymbol), typeArgs map (t => typeArgumentTree(t)) )
    case _                                => Ident(t.typeSymbol.name)
  }

  def getVars(tpe: Type): List[Symbol] = tpe
    .members
    .filter(_.isTerm)
    .filter(x => x.asTerm.isVar && x.asTerm.setter.isPublic && x.asTerm.getter.isPublic)
    .toList

  def getVals(tpe: Type): List[Symbol] = tpe.members.filter(_.isTerm).filter(_.asTerm.isVal).toList
  
  def getNonConstructorVars(tpe: Type): List[Symbol] = {
    // Make sure that the param isn't part of the constructor
    val ctorParams = tpe.member(nme.CONSTRUCTOR).asTerm.alternatives
      .map(_.asMethod.paramss.flatten.map(_.name.toTermName.toString.trim))
      .flatten
      .toSet

    // TODO: Looks like these are always accessed with getters and setters. Need to find if the getters and setters
    //       are valid
    for {
      // TODO: need to check if the var is public or not, but doesn't seem to work properly
      sym <- getVars(tpe) if (!ctorParams.exists(sym.name.toTermName.toString.trim == _))
    } yield sym
  }

  def getJavaStyleSetters(tpe: Type) = {
    val candidates = tpe.members.filter(_.isTerm).filter(_.asTerm.isMethod).filter{ s =>
      val name = s.asTerm.name.decoded
      name.startsWith("get") || name.startsWith("set")
    }
    candidates.filter ( sym =>
      candidates.exists(_.asTerm.name.decoded.trim.endsWith(sym.asTerm.name.decoded.trim.substring("set".length))) &&
        sym.asMethod.paramss.flatten.length == 1
    ).toList
  }

  private val primitiveTypes =  {
    c1.typeOf[Int]::
      c1.typeOf[String]::
      c1.typeOf[Char]::
      c1.typeOf[Float]::
      c1.typeOf[Double]::
      c1.typeOf[Boolean]::
      c1.typeOf[Long]::
      c1.typeOf[Byte]::
      c1.typeOf[BigInt]::
      c1.typeOf[Short]::
      c1.typeOf[BigDecimal]::
      c1.typeOf[Date]::
      c1.typeOf[scala.Symbol]::Nil
  }

  def isPrimitive(tpe: c1.Type) = primitiveTypes.exists(tpe =:= _)
}
