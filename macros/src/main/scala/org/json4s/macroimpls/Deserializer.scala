package org.json4s.macroimpls

import language.experimental.macros
import scala.reflect.macros.Context
import scala.Some
import org.json4s.playground.{JsonReader, JsonObjectReader, JsonArrayIterator, InvalidStructure}


object Deserializer {
  import macrohelpers._
  import PrimativeHelpers._
  import org.json4s._

  import java.util.Date
  import org.json4s.Formats

//  def eitherDeserialize_impl[U: c.WeakTypeTag](c: Context)(params: c.Expr[JValue])
//         (defaultFormats: c.Expr[Formats]): c.Expr[Either[ParserUtil.ParseException,U]] =  c.universe.reify {
//    try { Right(deserialize_impl(c)(params)(defaultFormats).splice) }
//    catch { case e: ParserUtil.ParseException => Left(e) }
//  }

  // The meat and potatoes of the implementation.
  def deserialize[U](reader: JsonReader)(implicit defaultFormats: Formats) = macro deserialize_impl[U]
  def deserialize_impl[U: c.WeakTypeTag](c: Context)(reader: c.Expr[JsonReader])
          (defaultFormats: c.Expr[Formats]): c.Expr[U] = {

    import c.universe._
    import Flag._
    import org.json4s.ParserUtil.ParseException

    val helpers = new MacroHelpers[c.type](c)
    import helpers._

    def rparseDate(field: c.Expr[String], params: c.Expr[JsonObjectReader])  = reify {
      defaultFormats.splice.dateFormat.parse(rparseString(field, params).splice).get
    }

    def rparseInt(field: c.Expr[String], params: c.Expr[JsonObjectReader])    = reify {
      params.splice.getInt(field.splice)
    }

    def rparseLong(field: c.Expr[String], params:c.Expr[JsonObjectReader])   = reify {
      params.splice.getLong(field.splice)
    }

    def rparseFloat(field: c.Expr[String], params: c.Expr[JsonObjectReader])  = reify {
      params.splice.getFloat(field.splice)
    }

    def rparseDouble(field: c.Expr[String], params: c.Expr[JsonObjectReader]) = reify {
      params.splice.getDouble(field.splice)
    }

    def rparseString(field: c.Expr[String], params: c.Expr[JsonObjectReader]) = reify {
      params.splice.getString(field.splice)
    }

    def rparseSymbol(field: c.Expr[String], params: c.Expr[JsonObjectReader]) = reify {
      Symbol(rparseString(field, params).splice)
    }

    def rparseOption(tpe:Type, field: c.Expr[String], params: c.Expr[JsonObjectReader]):Tree = {
      val TypeRef(_, _, List(argTpe)) = tpe
      reify{
        try{
          Some(c.Expr(buildField(argTpe, field, params)).splice)
        } catch {
          case _: Throwable => None
        }
      }.tree
    }

        def buildMap(tpe:Type, params: c.Expr[JsonObjectReader]): c.Tree   = {
    //      val TypeRef(_, _, keyTpe::valTpe::Nil) = tpe
    //      // Capable of parsing maps that contain primatives as keys, not only strings
    //      val kExpr = c.Expr[String](Ident("k"))
    //      val keyParser = keyTpe match {
    //        case a if a =:= typeOf[Int] => reify{getInt(JInt(kExpr.splice.toInt))}
    //        case a if a =:= typeOf[Long] => reify{getLong(JInt(kExpr.splice.toLong))}
    //        case a if a =:= typeOf[Float] => reify{getFloat(JDouble(kExpr.splice.toDouble))}
    //        case a if a =:= typeOf[Double] => reify{getDouble(JDouble(kExpr.splice.toDouble))}
    //        case a if a =:= typeOf[String] => reify{getString(JString(kExpr.splice))}
    //        case _ => c.abort(c.enclosingPosition, "Map must contain primative types as keys!")
    //      }
    //
    //      reify {
    //        params.splice match {
    //          case JObject(objs) =>
    //            objs.map { case (k, v) => (
    //              keyParser.splice,
    //              c.Expr(buildObject(valTpe, c.Expr[JValue](Ident("v")))).splice
    //            )
    //            }.toMap
    //
    //          case o => throw new JsonStructureException(JObject.getClass(), o.getClass())
    //        }
    //      }.tree
          ???
        }

    def buildList(tpe: Type, reader: c.Expr[JsonArrayIterator]): Tree = {
      val TypeRef(_, _, List(argTpe)) = tpe
      val builderExpr = Apply(Select(New(AppliedTypeTree(Ident("collection.mutable.ListBuffer"), List(Ident(argTpe.toString)))), nme.CONSTRUCTOR), List())


//        reify{ // Cant seem to get this to work.
//          val builder = c.Expr[scala.collection.mutable.ListBuffer[Any]](builderExpr).splice
//          while(reader.splice.hasNext){
//              builder.append(c.Expr[Any](buildCell(argTpe, reader)).splice)
//          }
//          builder.result()
//        }.tree

      val listNme = c.fresh("lst")
      val listTree = ValDef(
                        Modifiers(MUTABLE),
                        listNme,
                        typeArgumentTree(tpe),
                        reify(Nil).tree
            )

      val itNme = c.fresh("jsonIterator")
      val itExpr = c.Expr[JsonArrayIterator](Ident(itNme))
      val itTree = ValDef(
        Modifiers(),
        newTermName(itNme),
        TypeTree(typeOf[JsonArrayIterator]),
        reader.tree
      )

        reify{
          c.Expr(itTree).splice
          c.Expr(listTree).splice
          while(itExpr.splice.hasNext) {

          // Manual tree manipulation is fastest, but more subject to scala churn
            c.Expr(Assign(Ident(listNme),Apply(Select(Ident(listNme),
                newTermName("$colon$colon")),
                List(buildCell(argTpe, itExpr))))
            ).splice

          }
          c.Expr[List[_]](Ident(listNme)).splice.reverse
       }.tree
    } // rparseList

    def buildCell(pTpe: Type, reader: c.Expr[JsonArrayIterator]): Tree = {
      if      (pTpe =:= typeOf[Int])         reify { reader.splice.nextInt }.tree
      else if (pTpe =:= typeOf[Long])         reify { reader.splice.nextLong }.tree
      else if (pTpe =:= typeOf[Float])         reify { reader.splice.nextFloat }.tree
      else if (pTpe =:= typeOf[Double])         reify { reader.splice.nextDouble }.tree
      else if (pTpe =:= typeOf[String])         reify { reader.splice.nextString }.tree
      else if (typeOf[List[_]] <:< pTpe.erasure) buildList(pTpe, reify{reader.splice.nextArrayReader})
      else buildObject(pTpe, reify{reader.splice.nextObjectReader})

    }

    def buildField(pTpe: Type, fieldName: c.Expr[String], params: c.Expr[JsonObjectReader]): Tree = {
      if      (pTpe =:= typeOf[Int])         { rparseInt(fieldName, params).tree }
      else if (pTpe =:= typeOf[Long])         { rparseLong(fieldName, params).tree  }
      else if (pTpe =:= typeOf[Float])        { rparseFloat(fieldName, params).tree  }
      else if (pTpe =:= typeOf[Double])       { rparseDouble(fieldName, params).tree }
      else if (pTpe =:= typeOf[String])      { rparseString(fieldName, params).tree }
      else if (pTpe =:= typeOf[Date])         { rparseDate(fieldName, params).tree   }
      else if (pTpe =:= typeOf[scala.Symbol]) { rparseSymbol(fieldName, params).tree }
      // The privileged types
      else if (pTpe.erasure <:< typeOf[Option[_]]) {
        rparseOption(pTpe, fieldName, params)
      }
      //      else if (typeOf[Map[_, _]] <:< tpe.erasure) {
      //        rparseMap(tpe, params)
      //      }
      else if (typeOf[List[_]] <:< pTpe.erasure) {
        buildList(pTpe, reify{params.splice.getArrayReader(fieldName.splice)})
      }
      else { // Must be complex
        buildObject(pTpe, reify{ params.splice.getObjectReader(fieldName.splice)})
      }
    }

    // The really heavyweight function. Most of the magic happens in the last else statement
    def buildObject(tpe: Type, params: c.Expr[JsonObjectReader]): Tree = {

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

          // If param has defaults, try to find the val in map, or call
          // default evaluation from its companion object
          if (pSym.asTerm.isParamWithDefault) {
            reify {
              try {
                c.Expr(buildField(pTpe, fieldName, params)).splice // splice in another obj tree
              } catch {
                case _: Throwable =>
                  // Need to use the origional symbol.companionObj to get defaults
                  // Would be better to find the generated TermNames if possible
                  c.Expr(Select(Ident(sym.companionSymbol), newTermName(
                    "$lessinit$greater$default$" + (index+1).toString))
                  ).splice
              }
            }.tree
          } else buildField(pTpe, fieldName, params)
        })) // Using the New(Tree, List(List(Tree))) constructor
      ) // newObjTree ValDef
        
      // Generate the code for setting fields not in the constructor
      val setParamsBlocks = getNonConstructorVars(tpe).map{ pSym =>
        val pTpe = pSym.typeSignature.substituteTypes(sym.asClass.typeParams, tpeArgs)
        val varName = pSym.name.toTermName.toString.trim
        val compName = LIT(varName)
        reify{
          try {
          c.Expr(Assign(Select(Ident(newObjTerm), newTermName(varName)),
          buildField(pTpe, compName, params)
          )).splice
          } catch { // Don't care if they fail
            case _: JsonStructureException =>
            case _: InvalidStructure =>
          }
        }.tree
      }
        
        //Block(newObjTree::setParamsBlocks, Ident(newObjTerm))
      Block(newObjTree::setParamsBlocks, Ident(newObjTerm))
      //}
    }
    
    val tpe = weakTypeOf[U]
    val expr = if (typeOf[Map[_, _]] <:< tpe.erasure) {
      val i = c.Expr[U](buildMap(tpe, c.Expr[JsonObjectReader](Ident("r"))))
      reify {
        reader.splice match {
          case r: JsonObjectReader => i.splice
          case e => throw new InvalidStructure("Need reader type Object to read object fields", e)
        }
      }
    }
    else if (typeOf[List[_]] <:< tpe.erasure) {
      val i = c.Expr[U](buildList(tpe, c.Expr[JsonArrayIterator](Ident("r"))))
      reify {
        reader.splice match {
          case r: JsonArrayIterator => i.splice
          case e => throw new InvalidStructure("Need reader type Array to read object fields", e)
        }
      }
    } else {
      val i = c.Expr[U](buildObject(tpe, c.Expr[JsonObjectReader](Ident("r"))))
      reify {
        reader.splice match {
          case r: JsonObjectReader => i.splice
          case e => throw new InvalidStructure("Need reader type Object to read object fields", e)
        }
      }
    }
    println(expr)  // Debug
    expr
  }
}
