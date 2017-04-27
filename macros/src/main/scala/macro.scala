package macros

//import scala.reflect.macros.whitebox.Context
import scala.reflect.macros.blackbox.Context

import scala.language.experimental.macros
import scala.language.higherKinds

trait ProductWriters[Writer[_]] {
  implicit def productWriter[P]: Writer[P] = macro ProductMapperBundle.productWriterImpl[Writer[_], P]
}

class ProductMapperBundle(val c: Context) {
  import c.universe._

  /** Summon an implicit value and return the tree representing its
    * invocation, or fail if no implicit is available. */
 private  def implicitlyOrFail(tpe: Type, message: String): Tree = {
    c.typecheck(
      q"""{
        import ${c.prefix}._
        implicitly[$tpe]
      }""",
      silent = true
    ) match {
      case EmptyTree => c.abort(c.enclosingPosition, message)
      case tree => tree
    }
  }

  private case class TypeClass(baseType: Type, method: MethodSymbol)

  private def writerTypeClass[T: c.WeakTypeTag]: TypeClass = {
    val baseType = weakTypeOf[T] 
    val typeParam: Symbol = baseType.typeSymbol.asType.typeParams.head

    // extract methods that take one type parameter, matching the type classes type parameter
    val methods = baseType.decls.collect{
      case m: MethodSymbol if m.isAbstract => m
    }.filter{ m =>
      m.paramLists match {
        case (param :: Nil) :: Nil =>
          param.info.typeSymbol == typeParam
        case _ => false
      }
    }.toList

    methods match {
      case head :: Nil => TypeClass(baseType, head)
      case list =>
        val message = list.map(_.name.toString).map(n => "<" + n + ">") match {
          case Nil => "<no abstract method>"
          case list => list.mkString(", ")
        }
        c.abort(
          c.prefix.tree.pos,
          s"${weakTypeOf[T]} must have exaclty one abstract method with conforming signature. It currently has the following abstract methods: $message."
        )
    }
  }

  /** Create a new writer.
    * @param tc the base writer type class
    * @param genericType the elemnt type of the new writer to create
    * @param body a function that generates the writer's body from a given parameter name
    */
  private def newWriter(tc: TypeClass, genericType: Type, body: TermName => Tree): Block = {
    val parent = appliedType(tc.baseType.typeConstructor, genericType)

    val paramName = TermName("value")
    val param = ValDef(Modifiers(Flag.PARAM), paramName, TypeTree(genericType), EmptyTree)
    val defn = DefDef(Modifiers(), tc.method.name, Nil, List(List(param)), TypeTree(tc.method.returnType), body(paramName))

    val tree = Block(
      List(
        q"final class $$anon extends $parent { $defn }"
      ),
      Apply(Select(New(Ident(TypeName("$anon"))), termNames.CONSTRUCTOR), List())
    )
    tree
  }

  def productWriterImpl[W: c.WeakTypeTag, P: c.WeakTypeTag]: c.Tree = {
    val product = weakTypeOf[P]
    val writer: TypeClass = writerTypeClass[W]

    if (!(product <:< weakTypeOf[Product])) {
      c.abort(c.enclosingPosition, s"Cannot generate product writer for non-product type $product")
    }

    val mapType: Type = appliedType(typeOf[Map[_, _]], typeOf[String], writer.method.returnType)

    val mapWriter = implicitlyOrFail(appliedType(writer.baseType, mapType),
      s"No implicit decomposer available for ${appliedType(writer.baseType, product)}. Make sure an implicit $mapType is in scope.")

    val fields: List[(TermName, Type)] = product.decls.collect{
      case m: MethodSymbol if m.isCaseAccessor =>
        m.name -> m.info.resultType
    }.toList
    val fieldWriters: List[(TermName, Tree)] = fields.map{ case (name, tpe) =>
      val writerType = appliedType(writer.baseType, tpe)
      name -> implicitlyOrFail(writerType, s"Cannot create writer for $product: no implicit writer available for $product.$name of type $tpe")
    }

    def mapBody(param: TermName): List[Tree] = fieldWriters.map{ case (name, fieldWriter) =>
      q"""(${name.toString}, $fieldWriter.${writer.method.name}($param.$name))"""
    }

    def data(param: TermName): Tree = q"""{
      val data = scala.collection.immutable.Map(..${mapBody(param)}); $mapWriter.${writer.method.name}(data)
    }"""

    val tree = newWriter(writer, product, param => data(param))
    //println(tree)
    tree
  }
}
