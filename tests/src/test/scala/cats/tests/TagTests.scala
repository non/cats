package cats
package tests

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.BooleanOperators

import cats.Tag._

class TagTests extends CatsSuite {

  trait NonNegative

  def double(n: BigInt @@ NonNegative): BigInt @@ NonNegative =
    (n.untag * 2).tag[NonNegative]

  test("type class instances available") {
    Eq[BigInt @@ NonNegative]
    true shouldBe true
  }

  test("tag works") {
    val one = BigInt(1).tag[NonNegative]
    val two = BigInt(2).tag[NonNegative]
    assert(double(one) === two)
  }
}

class TagBytecode {

  trait Good // tag type to use

  // we can inspect the bytecode of this method to ensure the macros
  // are working:
  //
  // javap -c -classpath tests/.jvm/target/scala-2.11/test-classes cats.tests.TagBytecode
  def work(n: Int, lst: List[Int], fn: Functor[List]): Unit = {

    import cats.Tag
    import cats.Tag._

    val x0: Int @@ Good = Tag.wrap[Int, Good](n)
    val x1: Int @@ Good = n.tag[Good]

    val xs0: List[Int @@ Good] = Tag.wrapf[List, Int, Good](lst)
    val xs1: List[Int @@ Good] = lst.tagf[Good]

    val xss: Functor[λ[α => List[α] @@ Good]] =
      Tag.wrapk[Functor, List, Good](fn)

    ()
  }
}
