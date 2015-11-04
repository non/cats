package cats

object Tag {

  def wrap[A, T](a: A): A @@ T =
    macro macros.Tags.wrapMacro[A, T]
  def unwrap[A, T](at: A @@ T): A =
    macro macros.Tags.unwrapMacro[A, T]

  def wrapf[F[_], A, T](fa: F[A]): F[A @@ T] =
    macro macros.Tags.wrapfMacro[F, A, T]
  def unwrapf[F[_], A, T](fat: F[A @@ T]): F[A] =
    macro macros.Tags.unwrapfMacro[F, A, T]

  // TODO: use macros here. can't currently figure out how to
  // construct these type lambdas correctly from within a macro.
  def wrapk[F[_[_]], G[_], T](fg: F[G]): F[λ[α => G[α] @@ T]] =
    fg.asInstanceOf[F[λ[α => G[α] @@ T]]]
  def unwrapk[F[_[_]], G[_], T](fgt: F[λ[α => G[α] @@ T]]): F[G] =
    fgt.asInstanceOf[F[G]]
}
