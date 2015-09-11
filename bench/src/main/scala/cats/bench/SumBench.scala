package cats.bench

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

import cats._
//import cats.implicits._
import cats.Tag._

import scala.util.Random.nextInt

@State(Scope.Benchmark)
class SumBench {

  trait NonNeg
  type N = Long @@ NonNeg

  val ns: Vector[Long] = (1 to 10000).map(_ => nextInt & 0xffL).toVector
  val tns: Vector[N] = ns.tagf[NonNeg]

  object NonNegLongMonoid extends Monoid[N] {
    val empty: N = 0L.tag[NonNeg]
    def combine(x: N, y: N): N = (x.untag + y.untag).tag[NonNeg]
  }

  def withSomeTag[A](a: A): A @@ NonNeg  = a.tag[NonNeg]
  type TypeAlias = String @@ NonNeg
  def need(a: TypeAlias): TypeAlias = a
  need(withSomeTag("foo"))

  @Benchmark
  def stdlibSum(): Long =
    ns.sum

  @Benchmark
  def vectorFoldLeftSum(): Long =
    ns.foldLeft(0L)(_ + _)

  // @Benchmark
  // def monoidCombineAll(): Long =
  //   Monoid[Long].combineAll(ns)

  @Benchmark
  def monoidCombineAllTagged(): N =
    NonNegLongMonoid.combineAll(tns)
}
