package org.json4s.macroimpls

import language.experimental.macros
import scala.reflect.macros.Context
import playground.Separator
import util.control.Exception._
import scala.Some


object Deserializer {
  import macrohelpers._
  import PrimativeHelpers._
  import org.json4s._

  import java.util.Date
  import org.json4s.Formats

  def eitherDeserialize_impl[U: c.WeakTypeTag](c: Context)(params: c.Expr[JValue])
         (defaultFormats: c.Expr[Formats]): c.Expr[Either[ParserUtil.ParseException,U]] =  c.universe.reify {
    try { Right(deserialize_impl(c)(params)(defaultFormats).splice) }
    catch { case e: ParserUtil.ParseException => Left(e) }
  }

  // The meat and potatoes of the implementation.
  def deserialize[U](params: JValue)(implicit defaultFormats: Formats) = macro deserialize_impl[U]
  def deserialize_impl[U: c.WeakTypeTag](c: Context)(params: c.Expr[JValue])
          (defaultFormats: c.Expr[Formats]): c.Expr[U] = {

    import c.universe._
    import Flag._
    import org.json4s.ParserUtil.ParseException

    val helpers = new MacroHelpers[c.type](c)
    import helpers._


    def rparseList(tpe:Type, params: c.Expr[JValue]): Tree = {
      val TypeRef(_, _, List(argTpe)) = tpe
      val wrappedIndex: c.Expr[JValue] = c.Expr[JValue](Ident("i"))
      
      reify{
        params.splice match {
          case JArray(arr) => arr.map { i =>
            c.Expr(buildObject(argTpe, wrappedIndex)).splice
          }
          case _ => Nil
      }
      }.tree
    } // rparseList

    def rparseMap(tpe:Type, params: c.Expr[JValue])   = {
      val TypeRef(_, _, keyTpe::valTpe::Nil) = tpe
      // Capable of parsing maps that contain primatives as keys, not only strings
      val kExpr = c.Expr[String](Ident("k"))
      val keyParser = keyTpe match {
        case a if a =:= typeOf[Int] => reify{getInt(JInt(kExpr.splice.toInt))}
        case a if a =:= typeOf[Long] => reify{getLong(JInt(kExpr.splice.toLong))}
        case a if a =:= typeOf[Float] => reify{getFloat(JDouble(kExpr.splice.toDouble))}
        case a if a =:= typeOf[Double] => reify{getDouble(JDouble(kExpr.splice.toDouble))}
        case a if a =:= typeOf[String] => reify{getString(JString(kExpr.splice))}
        case _ => c.abort(c.enclosingPosition, "Map must contain primative types as keys!")
      }

      reify {
        params.splice match {
          case JObject(objs) =>
            objs.map { case (k, v) => (
              keyParser.splice,
              c.Expr(buildObject(valTpe, c.Expr[JValue](Ident("v")))).splice
            )
            }.toMap

          case o => throw new JsonStructureException(JObject.getClass(), o.getClass())
        }
      }.tree
    }

    def rparseDate(params: c.Expr[JValue])  = reify {
      getDate(params.splice, defaultFormats.splice.dateFormat)
    }

    def rparseInt(params: c.Expr[JValue])    = reify {
      getInt(params.splice)
    }

    def rparseLong(params: c.Expr[JValue])   = reify {
      getLong(params.splice)
    }

    def rparseFloat(params: c.Expr[JValue])  = reify {
      getFloat(params.splice)
    }

    def rparseDouble(params: c.Expr[JValue]) = reify {
      getDouble(params.splice)
    }
    
    def rparseString(params: c.Expr[JValue]) = reify {
      getString(params.splice)
    }

    def rparseSymbol(params: c.Expr[JValue]) = reify {
      Symbol(getString(params.splice))
    }

    def rparseOption(tpe:Type, params: c.Expr[JValue]):Tree = {
      val TypeRef(_, _, List(argTpe)) = tpe
      reify{
        try{
          Some(c.Expr(buildObject(argTpe, params)).splice)
        } catch {
          case _: JsonStructureException => None
        }
      }.tree
    }

    // The really heavyweight function. Most of the magic happens in the last else statement
    def buildObject(tpe: Type, params: c.Expr[JValue]): Tree = {
      // simple types
      if      (tpe =:= typeOf[Int])          { rparseInt(params).tree    }
      else if (tpe =:= typeOf[Long])         { rparseLong( params).tree  }
      else if (tpe =:= typeOf[Float])        { rparseFloat(params).tree  }
      else if (tpe =:= typeOf[Double])       { rparseDouble(params).tree }
      else if (tpe =:= typeOf[String])       { rparseString(params).tree }
      else if (tpe =:= typeOf[Date])         { rparseDate(params).tree   }
      else if (tpe =:= typeOf[scala.Symbol]) { rparseSymbol(params).tree }

      // The privileged types
      else if (tpe.erasure <:< typeOf[Option[Any]]) {
        rparseOption(tpe, params)
      }
      else if (typeOf[Map[_, _]] <:< tpe.erasure) {
        rparseMap(tpe, params)
      }
      else if (typeOf[List[_]] <:< tpe.erasure) {
        rparseList(tpe, params)
      }
      else { // Must be a complex object
        val TypeRef(_, sym: Symbol, tpeArgs: List[Type]) = tpe
        val ctorParams = tpe.member(nme.CONSTRUCTOR).asMethod.paramss
        
        val newObjTerm = newTermName(c.fresh("newObj$"))
        val newObjTypeTree = typeArgumentTree(tpe)
        val newObjTree = ValDef(Modifiers(), newObjTerm, newObjTypeTree,
          New(newObjTypeTree, ctorParams.map(_.zipWithIndex.map {
            case (pSym, index) =>
            // Change out the types if it has type parameters
            val pTpe = pSym.typeSignature.substituteTypes(sym.asClass.typeParams, tpeArgs)
            val fieldName = LIT(pSym.name.decoded)
            val (_, freshObj, freshObjTree) = getObjectField(fieldName, params)

            // If param has defaults, try to find the val in map, or call 
            // default evaluation from its companion object
            Block(freshObjTree ,if (pSym.asTerm.isParamWithDefault) {
              reify {
                try {
                  c.Expr(buildObject(pTpe, freshObj)).splice // splice in another obj tree
                } catch {
                  case e: ParseException =>
                    // Need to use the origional symbol.companionObj to get defaults
                    // Would be better to find the generated TermNames if possible
                    c.Expr(Select(Ident(sym.companionSymbol), newTermName(
                      "$lessinit$greater$default$" + (index+1).toString))
                    ).splice
                }
              }.tree
            } else (reify { // repackage the exception to be more informative
              try c.Expr(buildObject(pTpe, freshObj)).splice
              catch {
                case e: ParseException =>
                  val msg = "Error getting field '" + fieldName.splice +
                             "' in object of type '" + LIT(tpe.toString).splice +
                             "'. Reason: " + e.toString
                  throw new ParseException(msg, e)
              }
            }.tree)) // Required
          })) // Using the New(Tree, List(List(Tree))) constructor
        ) // newObjTree ValDef
        
        // Generate the code for setting fields not in the constructor
        val vars = getNonConstructorVars(tpe)
        val setParamsBlocks = vars map { vari =>
          val varName = vari.name.toTermName.toString.trim
          val compName = LIT(varName)
          val (_, freshObj, freshObjTree) = getObjectField(compName, params)
          Block(freshObjTree, reify{
            try {
            c.Expr(Assign(Select(Ident(newObjTerm), newTermName(varName)),
              buildObject(
                vari.typeSignature,
                freshObj
              )
            )).splice
            } catch { // Don't care if they fail
              case _: JsonStructureException =>
            }
          }.tree)
        }
        
        Block(newObjTree::setParamsBlocks, Ident(newObjTerm))
      }
    }
    
    val tpe = weakTypeOf[U]
    val expr = c.Expr[U](buildObject(tpe, params))
    // println(expr)  // Debug
    expr
  }
}
