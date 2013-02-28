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

  // The meat and potatoes of the implementation.
  def deserialize[U](params: JValue)(implicit defaultFormats: Formats) = macro deserialize_impl[U]
  def deserialize_impl[U: c.WeakTypeTag](c: Context)(params: c.Expr[JValue])
          (defaultFormats: c.Expr[Formats]): c.Expr[U] = {

    import c.universe._
    import Flag._
    import org.json4s.ParserUtil.ParseException

    val helpers = new MacroHelpers[c.type](c)
    import helpers._


    
    // For generating new new params set down the tree one step
    def genFreshParams(name: c.Expr[String], params: c.Expr[JValue]):(TermName, c.Expr[JValue], Tree) = {
      val freshNme = newTermName(c.fresh("freshJValue$"))
      val freshParams = c.Expr[JValue](Ident(freshNme))
      
      val freshParamsTree = ValDef( // Need to use a fresh name to avoid stuff like 'val nme = nme'
                    Modifiers(), 
                    freshNme,
                    TypeTree(typeOf[JValue]),
                    reify(
                      getValueByName(params.splice, name.splice)
                        .getOrElse(JNothing)
                    ).tree
                  )
      (freshNme, freshParams, freshParamsTree)
    }

    def rparseList(tpe:Type, params: c.Expr[JValue]): Tree = {
      val TypeRef(_, _, List(argTpe)) = tpe
      
      /*val (freshNme, freshParams, freshParamsTree) = genFreshParams(name, params)
      
      val listNme = newTermName("lst")
      val listTree = ValDef(
                      Modifiers(MUTABLE),
                      listNme,
                      typeArgumentTree(tpe),
                      reify{Nil}.tree
      )
      */
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
    /*
    def rparseMap(tpe:Type, name:c.Expr[String], params: c.Expr[JValue])   = {
      val TypeRef(_, _, keyTpe::valTpe::Nil) = tpe
      val (freshNme, freshParams, freshParamsTree) = genFreshParams(name, params)
      
      // Capable of parsing maps that contain primatives as keys, not only strings
      val kExpr = c.Expr[String](Ident("k"))
      val keyParser = keyTpe match {
        case a if a =:= typeOf[Int] => reify{getInt(kExpr.splice, kExpr.splice)}
        case a if a =:= typeOf[Long] => reify{getLong(kExpr.splice, kExpr.splice)}
        case a if a =:= typeOf[Float] => reify{getFloat(kExpr.splice, kExpr.splice)}
        case a if a =:= typeOf[Double] => reify{getDouble(kExpr.splice, kExpr.splice)}
        case a if a =:= typeOf[String] => reify{kExpr.splice}
        case _ => c.abort(c.enclosingPosition, "Map must contain primative types as keys!")
      }

      // Build the tree much like the function toMap does
      val mapBuilderTree = ValDef(Modifiers(), newTermName("b"), TypeTree(),
          TypeApply(reify{scala.collection.mutable.HashMap.empty}.tree,
            List(typeArgumentTree(keyTpe), typeArgumentTree(valTpe))
          )
        )

      val addValTree = Apply(Select(Ident("b"), newTermName("update")),
                            List(keyParser.tree, buildObject(valTpe, kExpr, freshParams))
                            )
   
      reify {
        // This is more simple to do using collections functions, but this is more 
        // performant. Check older commits for simpler version 
        // Jan 9, 2013 (da050fb6d1c317228cbcedeac9db91221d4565da)
        c.Expr(freshParamsTree).splice
        c.Expr(mapBuilderTree).splice // Defines val b = mapBuilder
        freshParams.splice.keySet.foreach { k => // Must use k or rename above
          c.Expr(addValTree).splice
        }
        c.Expr(Select(Ident("b"), newTermName("toMap"))).splice
      }.tree
    }
     */
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
    
    def rparseString(params: c.Expr[JValue]) = reify{
      getString(params.splice)
    }

    def rparseOption(tpe:Type, params: c.Expr[JValue]):Tree = {
      val TypeRef(_, _, List(argTpe)) = tpe
      reify{
        try{
          Some(c.Expr(buildObject(argTpe, params)).splice)
        } catch {
          case _: NothingException => None
        }
      }.tree
    }

    // The really heavyweight function. Most of the magic happens in the last else statement
    def buildObject(tpe: Type, params: c.Expr[JValue]):Tree = {
      // simple types
      if      (tpe =:= typeOf[Int])    { rparseInt(params).tree    }
      else if (tpe =:= typeOf[Long])   { rparseLong( params).tree   }
      else if (tpe =:= typeOf[Float])  { rparseFloat(params).tree  }
      else if (tpe =:= typeOf[Double]) { rparseDouble(params).tree }
      else if (tpe =:= typeOf[String]) { rparseString(params).tree }
      else if (tpe =:= typeOf[Date])   { rparseDate(params).tree   }

      // The privileged types
      else if (tpe.erasure <:< typeOf[Option[Any]]) {
        rparseOption(tpe, params)
      }       /*
      else if (tpe.erasure =:= typeOf[Map[_, _]]) {
        rparseMap(tpe, name, params)
      } */
      else if (tpe.erasure =:= typeOf[List[Any]]) {
        rparseList(tpe, params)
      }
      else { // Must be a complex object
      
        val TypeRef(_, sym:Symbol, tpeArgs:List[Type]) = tpe
        val ctorParams = tpe.member(nme.CONSTRUCTOR).asMethod.paramss

        
        val newObjTerm = newTermName(c.fresh("newObj$"))
        val newObjTypeTree = typeArgumentTree(tpe)
        val newObjTree = ValDef(Modifiers(), newObjTerm, newObjTypeTree,
          // New(typeTree, List(List(params))
          New(newObjTypeTree, ctorParams.map{_.zipWithIndex.map {
            case (pSym, index) =>
            // Change out the types if it has type parameters
            val pTpe = pSym.typeSignature.substituteTypes(sym.asClass.typeParams, tpeArgs)
            val compName = LIT(pSym.name.decoded)
            val (_, freshParams, freshParamsTree) = genFreshParams(compName, params)
            // If param has defaults, try to find the val in map, or call 
            // default evaluation from its companion object
            Block(freshParamsTree ,if (pSym.asTerm.isParamWithDefault) {
              reify {
                try {
                  c.Expr(buildObject(pTpe, freshParams)).splice // splice in another obj tree
                } catch {
                  case e: NothingException =>
                    // Need to use the origional symbol.companionObj to get defaults
                    // Would be better to find the generated TermNames if possible
                    c.Expr(Select(Ident(sym.companionSymbol), newTermName(
                      "$lessinit$greater$default$" + (index+1).toString))
                    ).splice
                }
              }.tree
            } else buildObject(pTpe, freshParams)) // Required
            
          }}) // Using the New(Tree, List(List(Tree))) constructor
        ) // newObjTree ValDef
        
        // Generate the code for setting fields not in the constructor
        val vars = getNonConstructorVars(tpe)
        val setParamsBlocks = vars map { vari =>
          val varName = vari.name.toTermName.toString.trim
          val compName = LIT(varName)
          val (_, freshParams, freshParamsTree) = genFreshParams(compName, params)
          Block(freshParamsTree, reify{
            try {
            c.Expr(Assign(Select(Ident(newObjTerm), newTermName(varName)),
              buildObject(
                vari.typeSignature,
                freshParams
              )
            )).splice
            } catch { // Don't care if they fail
              case _ : NothingException =>
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
