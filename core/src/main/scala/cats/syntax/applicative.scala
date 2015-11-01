package cats
package syntax

trait ApplicativeSyntax {
  implicit def applicativeIdSyntax[A](a: A): ApplicativeIdOps[A] =
    new ApplicativeIdOps(a)
}

class ApplicativeIdOps[A](val a: A) extends AnyVal {
  def pure[F[_]](implicit ev: Applicative[F]): F[A] = ev.pure(a)
}
