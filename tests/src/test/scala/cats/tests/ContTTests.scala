package cats
package tests

import cats.data.{Cont, ContT}
import cats.laws.discipline.{ArbitraryK, MonadTests, SerializableTests}
import cats.laws.discipline.eq.contTEq
import cats.laws.discipline.ArbitraryK.contTArbitraryK

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop._
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Arbitrary._

class ContTTests extends CatsSuite {

  implicit val ai: ArbitraryK[ContT[Int, Id, ?]] = contTArbitraryK[Int, Id]
  implicit val ei: Eq[ContT[Int, Id, Int]] = contTEq[Int, Id, Int]

  type C[A] = ContT[Int, Id, A]
  checkAll("ContT[Int, Id, ?]", MonadTests[C].monad[Int, Int, Int])
  checkAll("ContT[Int, Id, ?]]", SerializableTests.serializable(Monad[C]))

  type O[A] = ContT[Int, Option, A]
  checkAll("ContT[Int, Option, ?]", MonadTests[O].monad[Int, Int, Int])
  checkAll("ContT[Int, Option, ?]]", SerializableTests.serializable(Monad[O]))

  type L[A] = ContT[Int, List, A]
  checkAll("ContT[Int, List, ?]", MonadTests[L].monad[Int, Int, Int])
  checkAll("ContT[Int, List, ?]]", SerializableTests.serializable(Monad[L]))

  test("x1") {
    def len[R, A](as: List[A]): Cont[R, Int] = Cont(as.length)
    def main(): String = len(List('1','2','3')).run(_.toString)
    assert(main() == "3")
  }

  // test("0 = 0") {
  // 
  //   def foo: Cont[Int, Int] = Cont.shift { (k: Int => Int) => k(7) }
  // 
  //   //val c: Cont[Int, Int] = Cont.reset { 2 * foo() }
  //   assert(foo.run(_ * 2) == 14)
  // 
  // }

  // check {
  //   forAll { (x: Int, y: Int) =>
  //     x <= y || x >= y
  //   }
  // }
}
