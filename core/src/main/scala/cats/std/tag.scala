package cats
package std

import cats.Tag._
import cats.syntax.all._

trait TagInstances {
  implicit def taggedEq[A: Eq, T]: Eq[A @@ T] =
    new Eq[A @@ T] {
      def eqv(x: A @@ T, y: A @@ T): Boolean = unwrap(x) === unwrap(y)
    }

  implicit def taggedPartialOrder[A: PartialOrder, T]: PartialOrder[A @@ T] =
    new PartialOrder[A @@ T] {
      def partialCompare(x: A @@ T, y: A @@ T): Double = unwrap(x) partialCompare unwrap(y)
    }

  implicit def taggedOrder[A: Order, T]: Order[A @@ T] =
    new Order[A @@ T] {
      def compare(x: A @@ T, y: A @@ T): Int = unwrap(x) compare unwrap(y)
    }

  implicit def taggedSemigroup[A: Semigroup, T]: Semigroup[A @@ T] =
    new Semigroup[A @@ T] {
      def combine(x: A @@ T, y: A @@ T): A @@ T = (unwrap(x) |+| unwrap(y)).tag[T]
    }

  implicit def taggedMonoid[A, T](implicit ev: Monoid[A]): Monoid[A @@ T] =
    new Monoid[A @@ T] {
      def empty: A @@ T = ev.empty.tag[T]
      def combine(x: A @@ T, y: A @@ T): A @@ T = (unwrap(x) |+| unwrap(y)).tag[T]
    }

  implicit def taggedGroup[A, T](implicit ev: Group[A]): Group[A @@ T] =
    new Group[A @@ T] {
      def empty: A @@ T = ev.empty.tag[T]
      def combine(x: A @@ T, y: A @@ T): A @@ T = (unwrap(x) |+| unwrap(y)).tag[T]
      def inverse(x: A @@ T): A @@ T = unwrap(x).inverse.tag[T]
    }
}
