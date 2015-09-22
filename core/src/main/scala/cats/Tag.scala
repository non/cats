package cats

import cats.macros.Tags

object Tag {

  type @@[A, T] = Tags.Tagged[A, T]

  implicit class TagOps[A](a: A) {
    def tag[T]: A @@ T = macro Tags.tagMacro[A, T]
  }

  implicit class UntagOps[A, T](at: A @@ T) {
    def untag: A = macro Tags.untagMacro[A, T]
  }

  implicit class TagfOps[F[_], A](val fa: F[A]) extends AnyVal {
    def tagf[T]: F[A @@ T] = wrapf[F, A, T](fa)
  }

  implicit class UntagfOps[F[_], A, T](val fat: F[A @@ T]) {
    def untagf: F[A] = unwrapf[F, A, T](fat)
  }

  def wrap[A, T](a: A): A @@ T =
    macro Tags.wrapMacro[A, T]
  def unwrap[A, T](at: A @@ T): A =
    macro Tags.unwrapMacro[A, T]

  def wrapf[F[_], A, T](fa: F[A]): F[A @@ T] =
    macro Tags.wrapfMacro[F, A, T]
  def unwrapf[F[_], A, T](fat: F[A @@ T]): F[A] =
    macro Tags.unwrapfMacro[F, A, T]

  // TODO: use macros here. can't currently figure out how to
  // construct these type lambdas correctly from within a macro.
  def wrapk[F[_[_]], G[_], T](fg: F[G]): F[λ[α => G[α] @@ T]] =
    fg.asInstanceOf[F[λ[α => G[α] @@ T]]]
  def unwrapk[F[_[_]], G[_], T](fgt: F[λ[α => G[α] @@ T]]): F[G] =
    fgt.asInstanceOf[F[G]]
}
