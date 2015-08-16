package cats
package macros

import scala.reflect.macros.whitebox.Context

object Tags {

  trait Tagged[A, T]

  def wrapMacro[A: c.WeakTypeTag, T: c.WeakTypeTag](c: Context)(a: c.Expr[A]): c.Expr[Tagged[A, T]] = {
    import c.universe._
    val A = weakTypeOf[A]
    val T = weakTypeOf[T]
    c.Expr[Tagged[A, T]](q"$a.asInstanceOf[Tagged[$A, $T]]")
  }

  def unwrapMacro[A: c.WeakTypeTag, T](c: Context)(at: c.Expr[Tagged[A, T]]): c.Expr[A] = {
    import c.universe._
    val A = weakTypeOf[A]
    c.Expr[A](q"$at.asInstanceOf[$A]")
  }

  def wrapfMacro[F[_], A: c.WeakTypeTag, T: c.WeakTypeTag](c: Context)(fa: c.Expr[F[A]]): c.Expr[F[Tagged[A, T]]] = {
    import c.universe._
    val FAT = weakTypeOf[F[Tagged[A, T]]]
    c.Expr[F[Tagged[A, T]]](q"$fa.asInstanceOf[$FAT]")
  }

  def unwrapfMacro[F[_], A: c.WeakTypeTag, T](c: Context)(fat: c.Expr[F[Tagged[A, T]]]): c.Expr[F[A]] = {
    import c.universe._
    val FA = weakTypeOf[F[A]]
    c.Expr[F[A]](q"$fat.asInstanceOf[$FA]")
  }
}
