package cats
package data

object Cont {

  def apply[R, A](a: A): Cont[R, A] =
    ContT[R, Id, A](a)

  def apply[R, A](c: (A => R) => R): Cont[R, A] =
    ContT[R, Id, A](c)

  def eval[R](co: Cont[R, R]): R =
    ContT.eval(co)

  def callCC[R, A, B](f: (A => Cont[R, B]) => Cont[R, A]): Cont[R, A] =
    ContT.callCC(f)

  def shift[R, A](f: (A => R) => Cont[R, R]): Cont[R, A] =
    ContT.shift[R, Id, A](f)

  def reset[R, R1](co: Cont[R, R]): Cont[R1, R] =
    ContT.reset(co)
}
