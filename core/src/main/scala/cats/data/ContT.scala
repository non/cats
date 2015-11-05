package cats
package data

import cats.syntax.all._

trait ContT[R, M[_], A] {

  def run(f: A => M[R]): M[R]

  def mapResult(f: M[R] => M[R]): ContT[R, M, A] =
    ContT(c => f(run(c)))

  def map[B](f: A => B): ContT[R, M, B] =
    ContT(c => run(a => c(f(a))))

  def flatMap[B](f: A => ContT[R, M, B]): ContT[R, M, B] =
    ContT(c => run(a => f(a).run(c)))

  def with_[B](f: (B => M[R]) => (A => M[R])): ContT[R, M, B] =
    ContT(c => run(f(c)))

  def liftLocal[R1](ask: M[R1], local: (R1 => R1, M[R]) => M[R], f: R1 => R1)(implicit M: Monad[M]): ContT[R, M, A] =
    ContT(c => ask.flatMap(r => local(f, run(a => local(_ => r, c(a))))))
}

object ContT {

  def apply[R, M[_], A](a: A): ContT[R, M, A] =
    ContT(c => c(a))

  def apply[R, M[_], A](c: (A => M[R]) => M[R]): ContT[R, M, A] =
    new ContT[R, M, A] {
      def run(f: A => M[R]): M[R] = c(f)
    }

  def eval[R, M[_]: Monad](co: ContT[R, M, R]): M[R] =
    co.run(Monad[M].pure)

  def callCC[R, M[_], A, B](f: (A => ContT[R, M, B]) => ContT[R, M, A]): ContT[R, M, A] =
    ContT(c => f(a => ContT(_ => c(a))).run(c))

  def shift[R, M[_]: Monad, A](f: (A => M[R]) => ContT[R, M, R]): ContT[R, M, A] =
    ContT(c => ContT.eval(f(c)))

  def reset[R, M[_]: Monad, R1](co: ContT[R, M, R]): ContT[R1, M, R] =
    ContT(c => eval(co).flatMap(c))

  implicit def monad[R, M[_]: Monad]: Monad[ContT[R, M, ?]] =
    new Monad[ContT[R, M, ?]] {
      def pure[A](a: A): ContT[R, M, A] =
        ContT(a)
      override def ap[A, B](co: ContT[R, M, A])(cof: ContT[R, M, A => B]): ContT[R, M, B] =
        ContT(c => cof.run(f => co.run(a => c(f(a)))))
      override def map[A, B](co: ContT[R, M, A])(f: A => B): ContT[R, M, B] =
        co.map(f)
      override def map2[A, B, Z](fa: ContT[R, M, A], fb: ContT[R, M, B])(f: (A, B) => Z): ContT[R, M, Z] =
        fa.flatMap(a => fb.map(b => f(a, b)))
      override def flatten[A](co: ContT[R, M, ContT[R, M, A]]): ContT[R, M, A] =
        ContT(c => co.run(_.run(c)))
      def flatMap[A, B](co: ContT[R, M, A])(f: A => ContT[R, M, B]): ContT[R, M, B] =
        co.flatMap(f)
    }
}

object Main {

  type Cont[R, A] = ContT[R, Id, A]

  object Cont {
    def apply[R, A](a: A): Cont[R, A] =
      ContT[R, Id, A](a)

    def apply[R, A](c: (A => R) => R): Cont[R, A] =
      ContT[R, Id, A](c)

    def eval[R](co: Cont[R, R]): R =
      ContT.eval[R, Id](co)

    def callCC[R, A, B](f: (A => Cont[R, B]) => Cont[R, A]): Cont[R, A] =
      ContT.callCC[R, Id, A, B](f)

    def shift[R, A](f: (A => R) => Cont[R, R]): Cont[R, A] =
      ContT.shift[R, Id, A](f)

    def reset[R, R1](co: Cont[R, R]): Cont[R1, R] =
      ContT.reset[R, Id, R1](co)
  }

  import cats.data.Streaming

  sealed abstract class SearchTree[A]
  case class Leaf[A](a: A) extends SearchTree[A]
  case class Node[A](fs: Streaming[() => SearchTree[A]]) extends SearchTree[A]

  def choose[A, W](as: Streaming[A]): Cont[SearchTree[W], A] = ???
  def reify[A](c: Cont[SearchTree[A], A]): SearchTree[A] = ???

  def main(args: Array[String]): Unit = ()
}
