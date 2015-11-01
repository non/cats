package du

import cats._
import cats.implicits._
import cats.data.Streaming
import java.io.File

/**
 * This is a port of this Haskell code to Scala using Cats.
 *
 * https://www.reddit.com/r/haskell/comments/cs54i/how_would_you_write_du_in_haskell/c0uvqqo
 *
 * Most of this file is just setting up things we don't currently
 * have:
 *
 * 1. Basic I/O type (Io)
 * 2. Open-recursive filesystem type (Fs) with java.io-based methods
 * 3. Type class instances for Fs (Monad, Traverse, etc.)
 * 4. The actual "du" code.
 */

/**
 * Set up a couple nice type aliases
 */
object Types {
  type Path = String
  type Size = Long
}

import Types._

/**
 * Basic I/O type.
 */
class Io[A] private[du] (private[du] val e: Eval[A]) {
  def map[B](f: A => B): Io[B] = new Io(e.map(f))
  def flatMap[B](f: A => Io[B]): Io[B] = new Io(e.flatMap(a => f(a).e))

  def unsafeRun: A = e.value
}

object Io {
  def apply[A](a: => A): Io[A] = new Io(Always(a))

  implicit val ioMonad: Monad[Io] =
    new Monad[Io] {
      def pure[A](a: A): Io[A] = new Io(Now(a))
      override def map[A, B](fa: Io[A])(f: A => B): Io[B] = fa.map(f)
      def flatMap[A, B](fa: Io[A])(f: A => Io[B]): Io[B] = fa.flatMap(f)
    }
}

/**
 * Open-recursive FS type.
 */
sealed abstract class Fs[R] {
  def cata[A](f: Path => A, g: (Path, Streaming[R]) => A): A
}

object Fs {

  def file[R](p: Path): Fs[R] =
    new Fs[R] {
      def cata[A](f: Path => A, g: (Path, Streaming[R]) => A): A = f(p)
    }

  def dir[R](p: Path, rs: Streaming[R]): Fs[R] =
    new Fs[R] {
      def cata[A](f: Path => A, g: (Path, Streaming[R]) => A): A = g(p, rs)
    }

  def filesize(p: Path): Io[Size] =
    Io(new File(p).length)

  def isdir(p: Path): Io[Boolean] =
    Io(new File(p).isDirectory)

  def direntries(p: Path): Io[Streaming[Path]] =
    Io(Streaming.fromIterable(new File(p).listFiles).map(_.getPath))

  implicit val fsFunctor: Functor[Fs] =
    new Functor[Fs] {
      def map[A, B](fa: Fs[A])(f: A => B): Fs[B] =
        fa.cata(p => Fs.file(p), (p, as) => Fs.dir(p, as.map(f)))
    }

  implicit val fsTraverse: Traverse[Fs] =
    new Traverse[Fs] {
      def traverse[G[_]: Applicative, A, B](fa: Fs[A])(f: A => G[B]): G[Fs[B]] =
        fa.cata(Fs.file(_).pure[G], (p, as) => as.traverse(f).map(Fs.dir(p, _)))
      def foldLeft[A, B](fa: Fs[A], b: B)(f: (B, A) => B): B =
        fa.cata(_ => b, (_, as) => as.foldLeft(b)(f))
      def foldRight[A, B](fa: Fs[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.cata(_ => lb, (_, as) => as.foldRight(lb)(f))
    }
}


object Example {

  def hylo[F[_]: Functor, A, B](g: A => F[A], f: F[B] => B): A => B =
    (a: A) => f(g(a).map(hylo(g, f)))

  def hyloM[F[_]: Traverse, M[_]: Monad, A, B](g: A => M[F[A]], f: F[B] => M[B]): A => M[B] =
    (a: A) => g(a).flatMap(_.traverse(hyloM(g, f)).flatMap(f))

  def getFiles(p: Path): Io[Fs[Path]] =
    Fs.isdir(p).flatMap { b =>
      if (b) Fs.direntries(p).map(Fs.dir(p, _)) else Io(Fs.file(p))
    }

  def sumFiles(fs: Fs[Size]): Io[Size] =
    fs.cata(
      p => Fs.filesize(p),
      (p, ns) => Fs.filesize(p).map(_ + ns.foldLeft(0L)(_ + _)))

  def du(p: Path): Io[Size] =
    hyloM(getFiles, sumFiles).apply(p)

  def main(args: Array[String]): Unit = {
    val path = if (args.isEmpty) "." else args(0)
    du(path).map(println(_)).unsafeRun
  }
}

