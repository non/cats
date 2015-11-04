package cats

import cats.Tag._

trait TagSyntax {
  implicit class TagOps[A](a: A) {
    def tag[T]: A @@ T = macro macros.Tags.tagMacro[A, T]
  }

  implicit class UntagOps[A, T](at: A @@ T) {
    def untag: A = macro macros.Tags.untagMacro[A, T]
  }

  implicit class TagfOps[F[_], A](val fa: F[A]) {
    def tagf[T]: F[A @@ T] = wrapf[F, A, T](fa)
  }

  implicit class UntagfOps[F[_], A, T](val fat: F[A @@ T]) {
    def untagf: F[A] = unwrapf[F, A, T](fat)
  }
}

