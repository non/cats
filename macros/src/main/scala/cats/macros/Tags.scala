package cats
package macros

import scala.reflect.macros.whitebox.Context

object Tags {

  type Tagged[A, T] = { type Data = A; type Tag = T }

  def tagMacro[A: c.WeakTypeTag, T: c.WeakTypeTag](c: Context): c.Expr[Tagged[A, T]] = {
    import c.universe._
    val AT = weakTypeOf[Tagged[A, T]]
    val a = c.prefix.tree match {
      case Apply(_, List(x)) => x
      case t => c.abort(c.enclosingPosition, s"Cannot extract .tag target (tree = $t)")
    }
    c.Expr[Tagged[A, T]](q"$a.asInstanceOf[$AT]")
  }

  def untagMacro[A: c.WeakTypeTag, T: c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.universe._
    val A = weakTypeOf[A]
    val at = c.prefix.tree match {
      case Apply(_, List(x)) => x
      case t => c.abort(c.enclosingPosition, s"Cannot extract .untag target (tree = $t)")
    }
    c.Expr[A](q"$at.asInstanceOf[$A]")
  }

  def wrapMacro[A: c.WeakTypeTag, T: c.WeakTypeTag](c: Context)(a: c.Expr[A]): c.Expr[Tagged[A, T]] = {
    import c.universe._
    val A = weakTypeOf[A]
    val T = weakTypeOf[T]
    val AT = weakTypeOf[Tagged[A, T]]
    c.Expr[Tagged[A, T]](q"$a.asInstanceOf[cats.macros.Tags.Tagged[$A, $T]]")
  }

  def unwrapMacro[A: c.WeakTypeTag, T](c: Context)(at: c.Expr[Tagged[A, T]]): c.Expr[A] = {
    import c.universe._
    val A = weakTypeOf[A]
    c.Expr[A](q"$at.asInstanceOf[$A]")
  }

  def wrapfMacro[F[_], A: c.WeakTypeTag, T: c.WeakTypeTag](c: Context)(fa: c.Expr[F[A]])(implicit F: c.WeakTypeTag[F[_]]): c.Expr[F[Tagged[A, T]]] = {
    import c.universe._
    val AT = appliedType(typeOf[Tagged[_, _]], List(weakTypeOf[A], weakTypeOf[T]))
    val FAT = appliedType(F.tpe.typeConstructor, List(AT))
    val t = q"$fa.asInstanceOf[$FAT]"
    c.Expr[F[Tagged[A, T]]](t)
  }

  def unwrapfMacro[F[_], A: c.WeakTypeTag, T](c: Context)(fat: c.Expr[F[Tagged[A, T]]])(implicit F: c.WeakTypeTag[F[_]]): c.Expr[F[A]] = {
    import c.universe._
    val FA = appliedType(F.tpe.typeConstructor, List(weakTypeOf[A]))
    c.Expr[F[A]](q"$fat.asInstanceOf[$FA]")
  }

  // def wrapkMacro[F[_[_]], G[_], T: c.WeakTypeTag](c: Context)(fg: c.Tree)(implicit F: c.WeakTypeTag[F[List]], G: c.WeakTypeTag[G[_]]): c.Tree = {
  //   import c.universe._
  //   val A = internal.typeDef(G.tpe.typeParams.head)
  //   val LGT = typeLambda(c)(List(A), TypeTree(appliedType(G.tpe.typeConstructor, List(A.tpe))))
  //   val FGT = appliedType(F.tpe.typeConstructor, List(LGT))
  //   q"$fg.asInstanceOf[$FGT]"
  // }
  // 
  // def unwrapkMacro[F[_[_]], G[_], T](c: Context)(fgt: c.Tree)(implicit F: c.WeakTypeTag[F[List]], G: c.WeakTypeTag[G[_]]): c.Tree = {
  //   import c.universe._
  //   val FG = appliedType(F.tpe.typeConstructor, List(G.tpe))
  //   q"$fgt.asInstanceOf[$FG]"
  // }
  // 
  // def typeLambda(c: Context)(params: List[c.universe.TypeDef], body: c.universe.TypeTree): c.universe.Type = {
  //   import c.universe._
  //   val L = TypeName("L$")
  //   SelectFromTypeTree(
  //     CompoundTypeTree(
  //       Template(
  //         q"_root_.scala.AnyRef" :: Nil,
  //         ValDef(NoMods, termNames.WILDCARD, TypeTree(), EmptyTree),
  //         TypeDef(NoMods, L, params, body) :: Nil)), L).tpe
  // }
}
