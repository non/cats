package cats

object Tags {

  /**
   * PartialOrder tags for (x compare y)
   *
   * - Ascending: least-first ordering (default)
   * - Reverse:   greatest-first ordering
   */
  sealed trait Ascending
  sealed trait Descending

  /**
   * Semigroup tags for (x |+| y)
   *
   * - Min:       min(x, y)
   * - Max:       max(x, y)
   * - First:     x
   * - Last:      y
   * - Add:       x + y
   * - Multiply:  x * y
   * - And:       x && y
   * - Or:        x || y
   * - Intersect: x & y
   * - Union:     x | y
   * - Concat:    x ++ y
   * - Pairwise:  per-element (_ |+| _)
   * - Product:   cartesian product (_ |+| _)
   * - Empty:     if (x.isEmpty) x else y
   * - NonEmpty:  if (x.nonEmpty) x else y
   * - Gcd:       greatest-common denominator
   * - Lcm:       least-common multiple
   */
  sealed trait Min
  sealed trait Max
  sealed trait First
  sealed trait Last
  sealed trait Add
  sealed trait Multiply
  sealed trait And
  sealed trait Or
  sealed trait Intersect
  sealed trait Union
  sealed trait Concat
  sealed trait Pairwise
  sealed trait Empty
  sealed trait NonEmpty
  sealed trait Gcd
  sealed trait Lcm
}
