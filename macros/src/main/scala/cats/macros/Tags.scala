package cats
package macros

import algebra.Eq

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

  def tagfMacro[F[_], A: c.WeakTypeTag, T: c.WeakTypeTag](c: Context): c.Expr[Tagged[F[A], T]] = {
    import c.universe._
    val AT = appliedType(typeOf[Tagged[_, _]], List(weakTypeOf[A], weakTypeOf[T]))
    val FAT = appliedType(F.tpe.typeConstructor, List(AT))

    val AT = weakTypeOf[Tagged[F[A], T]]
    val a = c.prefix.tree match {
      case Apply(_, List(x)) => x
      case t => c.abort(c.enclosingPosition, s"Cannot extract .tag target (tree = $t)")
    }
    c.Expr[Tagged[A, T]](q"$a.asInstanceOf[$FAT]")
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
}
