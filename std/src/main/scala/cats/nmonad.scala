package cats

import cats.implicits._

// this is translation of the work at:
//
// https://ku-fpg.github.io/files/Sculthorpe-13-ConstrainedMonad.pdf
//
// into Scala using the Cats library

object N {

  /**
   * Explicit encoding of set operations in a monad, SetM.
   *
   * We are creating a normalized embedding of the monadic operations
   * on a set, which defers the application of constraints until they
   * are needed.
   *
   * (We are pretending that Order[A] is required to construct Set[A],
   * which means that we can only "lower" a SetM back to a Set if we
   * have the appropriate Order.)
   */

  // used to help be explicit about the "interface" of Set
  def returnSet[A: Order](a: A): Set[A] =
    Set(a)
  def bindSet[A, B: Order](sa: Set[A], f: A => Set[B]): Set[B] =
    sa.flatMap(f)

  sealed abstract class SetM[A] {
    def flatMap[B](f: A => SetM[B]): SetM[B]
    def lower(implicit ev: Order[A]): Set[A]
  }

  object SetM {

    def lift[A](sa: Set[A]): SetM[A] =
      FlatMapM[A, A](sa, PureM(_))

    case class PureM[A](a: A) extends SetM[A] {
      def flatMap[B](g: A => SetM[B]): SetM[B] = g(a)
      def lower(implicit ev: Order[A]): Set[A] = returnSet(a)
    }

    case class FlatMapM[X, A](sx: Set[X], f: X => SetM[A]) extends SetM[A] {
      def flatMap[B](g: A => SetM[B]): FlatMapM[X, B] =
        FlatMapM(sx, x => f(x).flatMap(g))
      def lower(implicit ev: Order[A]): Set[A] =
        bindSet[X, A](sx, x => f(x).lower)
    }

    implicit val monadSetM: Monad[SetM] =
      new Monad[SetM] {
        def pure[A](a: A): SetM[A] = PureM(a)
        def flatMap[A, B](sa: SetM[A])(f: A => SetM[B]): SetM[B] = sa.flatMap(f)
      }
  }

  /**
   * Explicit encoding of quantum effects in a monad, VecM.
   *
   * Expresses uncertainty about a value as complex numbers across a
   * finite enumeration of states. Presumably values should sum to 1.
   *
   * We require Eq[A] and Finite[A] at various points to ensure that
   * we can represent a known value (PureM) as well as chained
   * probabilities (FlatMapM).
   */

  trait Finite[A] {
    def enumerate: Stream[A]
  }

  case class Complex(r: Double, i: Double) {
    def +(that: Complex): Complex =
      Complex(r + that.r, i + that.i)
    def *(that: Complex): Complex =
      Complex(r * that.r - i * that.i, r * that.i + i * that.r)
  }

  object Complex {
    val zero = Complex(0.0, 0.0)
    val one = Complex(1.0, 0.0)
    def sum(zs: Iterable[Complex]): Complex = zs.foldLeft(zero)(_ + _)
  }

  type Vec[A] = A => Complex

  // used to help be explicit about the "interface" of Vec
  def returnVec[A: Eq](a: A): Vec[A] =
    x => if (x === a) Complex.one else Complex.zero
  def bindVec[A, B](v: Vec[A], f: A => Vec[B])(implicit ev: Finite[A]): Vec[B] =
    x => Complex.sum(ev.enumerate.map(a => v(a) * f(a)(x)))

  sealed abstract class VecM[A] {
    def flatMap[B](f: A => VecM[B]): VecM[B]
    def lower(implicit ev: Eq[A]): Vec[A]
  }

  object VecM {

    def lift[A: Finite](sa: Vec[A]): VecM[A] =
      FlatMapM[A, A](sa, PureM(_))

    case class PureM[A](a: A) extends VecM[A] {
      def flatMap[B](f: A => VecM[B]): VecM[B] = f(a)
      def lower(implicit ev: Eq[A]): Vec[A] = returnVec(a)
    }

    case class FlatMapM[X: Finite, A](vx: Vec[X], f: X => VecM[A]) extends VecM[A] {
      def flatMap[B](g: A => VecM[B]): VecM[B] = FlatMapM[X, B](vx, x => f(x).flatMap(g))
      def lower(implicit ev: Eq[A]): Vec[A] = bindVec[X, A](vx, x => f(x).lower)
    }

    implicit val monadVecM: Monad[VecM] =
      new Monad[VecM] {
        def pure[A](a: A): VecM[A] = PureM(a)
        def flatMap[A, B](sa: VecM[A])(f: A => VecM[B]): VecM[B] = sa.flatMap(f)
      }
  }

  /**
   * NM is a generic encoding of normalized monadic embeddings.
   *
   * T[_] is the data type which is constrained, e.g. the rough
   *  flatMap signature is T[A] => (A => T[B]) => T[B].
   *
   * C[_] is the contraint on C[A] in the previous signature. If no
   * constraint is needed (as in the case of Set), Unconstrained may
   * be used.
   *
   * A is the concrete type we are starting with.
   */

  // used when no constraint is needed
  trait Unconstrained[A]
  object Unconstrained {
    implicit def apply[A]: Unconstrained[A] = new Unconstrained[A] {}
  }

  // Polymorphic constrained fold
  //
  // The idea here is that we can call this function with any type X
  // that satisfies the C[X] constraint. It is more sophisticated than
  // a regular Function2 in that one of the types (X) is applied later
  // than all the others (C, T, R).
  //
  // This definition is tightly-bound to the definition of FlatMapM.
  trait PCF[C[_], T[_], R] {
    def apply[X: C](tx: T[X], f: X => R): R
  }

  sealed abstract class NM[C[_], T[_], A] {
    def flatMap[B](f: A => NM[C, T, B]): NM[C, T, B]
    def fold[R](ret: A => R, bin: PCF[C, T, R]): R
  }

  object NM {

    def lift[C[_], T[_], A: C](ta: T[A]): NM[C, T, A] =
      FlatMapM[C, T, A, A](ta, PureM[C, T, A](_))

    case class PureM[C[_], T[_], A](a: A) extends NM[C, T, A] {
      def flatMap[B](f: A => NM[C, T, B]): NM[C, T, B] =
        f(a)
      def fold[R](ret: A => R, bin: PCF[C, T, R]): R =
        ret(a)
    }

    case class FlatMapM[C[_], T[_], X: C, A](tx: T[X], f: X => NM[C, T, A]) extends NM[C, T, A] {
      def flatMap[B](g: A => NM[C, T, B]): NM[C, T, B] =
        FlatMapM(tx, (x: X) => f(x).flatMap(g))
      def fold[R](ret: A => R, bin: PCF[C, T, R]): R =
        bin[X](tx, x => f(x).fold(ret, bin))
    }

    implicit def monadNM[C[_], T[_]]: Monad[NM[C, T, ?]] =
      new Monad[NM[C, T, ?]] {
        def pure[A](a: A): NM[C, T, A] =
          PureM(a)
        def flatMap[A, B](sa: NM[C, T, A])(f: A => NM[C, T, B]): NM[C, T, B] =
          sa.flatMap(f)
      }

    type SetM2[A] = NM[Unconstrained, Set, A]

    def liftSet[A](set: Set[A]): SetM2[A] = NM.lift(set)

    val sm1: SetM2[(Int, Char)] = for {
      n <- liftSet(Set(3, 2, 1, 2))
      c <- liftSet(Set('a', 'b'))
    } yield (n, c)

    def setPcf2[A: Order]: PCF[Unconstrained, Set, Set[A]] =
      new PCF[Unconstrained, Set, Set[A]] {
        def apply[X: Unconstrained](tx: Set[X], f: X => Set[A]): Set[A] =
          bindSet(tx, f)
      }

    implicit val o: Order[(Int, Char)] = null

    def lowerSetM2[A: Order](setm: SetM2[A]): Set[A] =
      setm.fold[Set[A]](returnSet, setPcf2)

    val s1: Set[(Int, Char)] = lowerSetM2(sm1)

    /**
     * Use the novel example of arrays-with-classtags.
     *
     * Unlike Vec and Set, this is an exmample that has constraints at
     * both the "beginning" and the "end" of the embedding.
     */

    import scala.reflect.ClassTag

    type ArrayM2[A] = NM[ClassTag, Array, A]

    def liftArray[A: ClassTag](array: Array[A]): ArrayM2[A] =
      NM.lift[ClassTag, Array, A](array)

    val arrm1: ArrayM2[(Int, Char)] = for {
      n <- liftArray(Array(3, 2, 1, 2))
      c <- liftArray(Array('a', 'b'))
    } yield (n, c)

    def arrayPcf2[A: ClassTag]: PCF[ClassTag, Array, Array[A]] =
      new PCF[ClassTag, Array, Array[A]] {
        def apply[X: ClassTag](tx: Array[X], f: X => Array[A]): Array[A] =
          tx.map(f).flatten.toArray
      }

    def lowerArrayM2[A: ClassTag](arraym: ArrayM2[A]): Array[A] =
      arraym.fold(Array(_), arrayPcf2)

    val arr1: Array[(Int, Char)] = lowerArrayM2(arrm1)

    /**
     * Final thoughts:
     *
     * One thing that I don't love is that lowering from the embedding
     * will usually be expensive. For example, if we have lifted a Set
     * with 1000 elements and immediately lower it, we will create
     * 1000 Sets (each of 1 item) and then merge them.
     *
     * It might be nice to have a MapM(tx, f) in addition to PureM(a)
     * and FlatMapM(tx, f). This would have two advantage:
     *
     *   1. It would parallel Scala's use of flatMap/map to avoid this
     *      same problem in for-comprehensions.
     *
     *   2. It would make map operations much more efficient,
     *      especially the case where .map class are composed.
     *
     * Disadvantages to that include increased complexity, deviating
     * from the strict relationship to the monad laws, and an
     * additional polymoprhic function we'd need to create that would
     * parallel the existing PCF but for MapM.
     */
  }
}
