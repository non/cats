package cats
package tests

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.BooleanOperators
//import org.scalacheck.Arbitrary._

import cats.Tag._

class TagTests extends CatsSuite {

  trait NonNegative

  // implicit val nonNegativeBigInt: Arbitrary[BigInt @@ NonNegative] =
  //   Arbitrary(arbitrary[BigInt].map(_.abs.tag[NonNegative]))

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
