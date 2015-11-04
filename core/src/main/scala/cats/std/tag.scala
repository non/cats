package cats
package std

import cats.Tag._
import cats.syntax.all._

trait TagInstances extends GenericTagInstances {
  import cats.Tags._

  implicit def ascendingPartialOrder[A: PartialOrder]: PartialOrder[A @@ Ascending] =
    new PartialOrder[A @@ Ascending] {
      def partialCompare(x: A @@ Ascending, y: A @@ Ascending): Double =
        unwrap(x) partialCompare unwrap(y)
    }

  implicit def descendingPartialOrder[A: PartialOrder]: PartialOrder[A @@ Descending] =
    new PartialOrder[A @@ Descending] {
      def partialCompare(x: A @@ Descending, y: A @@ Descending): Double =
        unwrap(y) partialCompare unwrap(x)
    }

  implicit def ascendingOrder[A: Order]: Order[A @@ Ascending] =
    new Order[A @@ Ascending] {
      def compare(x: A @@ Ascending, y: A @@ Ascending): Int =
        unwrap(x) compare unwrap(y)
    }

  implicit def descendingOrder[A: Order]: Order[A @@ Descending] =
    new Order[A @@ Descending] {
      def compare(x: A @@ Descending, y: A @@ Descending): Int =
        unwrap(y) compare unwrap(x)
    }
}

trait GenericTagInstances {
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
      def combine(x: A @@ T, y: A @@ T): A @@ T = wrap[A, T](unwrap(x) |+| unwrap(y))
    }

  // implicit def taggedMonoid[A, T](implicit ev: Monoid[A]): Monoid[A @@ T] =
  //   new Monoid[A @@ T] {
  //     def empty: A @@ T = ev.empty.tag[T]
  //     def combine(x: A @@ T, y: A @@ T): A @@ T = (unwrap(x) |+| unwrap(y)).tag[T]
  //   }
  // 
  // implicit def taggedGroup[A, T](implicit ev: Group[A]): Group[A @@ T] =
  //   new Group[A @@ T] {
  //     def empty: A @@ T = ev.empty.tag[T]
  //     def combine(x: A @@ T, y: A @@ T): A @@ T = (unwrap(x) |+| unwrap(y)).tag[T]
  //     def inverse(x: A @@ T): A @@ T = unwrap(x).inverse.tag[T]
  //   }
}
