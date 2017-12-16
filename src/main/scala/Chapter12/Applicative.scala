package Chapter12

import Chapter10.Foldable
import Chapter11.{Functor, Id, Monad, Monads}

trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def map2[A, B, C](aa: F[A], ab: F[B])(f: (A, B) => C): F[C]

//  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]

  def applyInTermsOfMap2[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((ab: (A) => B, a: A) => ab(a))

//  def mapInTermsOfApply[A, B](fa: F[A])(f: A => B): F[B] =
//    apply[A, B](unit(f))(fa)
//
//  def map3[A,B,C,D](fa: F[A], fb: F[B],
//                    fc: F[C])(f: (A, B, C) => D): F[D] =
//    apply[C, D](apply[B, C => D](apply[A, B => C => D](unit(f.curried))(fa))(fb))(fc)
//
//
//  def map4[A,B,C,D, E](fa: F[A], fb: F[B],
//                    fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
//    apply[D, E](apply[C, D => E](apply[B, C => D => E](apply[A, B => C => D => E](unit(f.curried))(fa))(fb))(fc))(fd)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb)((_, _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this

    new Applicative[({type f[x] = (F[x], G[x])})#f] {

      override def map2[A, B, C](aa: (F[A], G[A]), ab: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
        (self.map2(aa._1, ab._1)(f), G.map2(aa._2, ab._2)(f))

      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def map[A, B](fa: (F[A], G[A]))(f: (A) => B): (F[B], G[B]) =
        (self.map(fa._1)(f), G.map(fa._2)(f))
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this

    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def map2[A, B, C](aa: F[G[A]], ab: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(aa, ab)(G.map2(_, _)(f))

      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def map[A, B](fa: F[G[A]])(f: (A) => B): F[G[B]] =
        self.map(fa)(G.map(_)(f))
    }
  }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    ofa.foldLeft(unit(Map.empty[K, V])){
      case (acc, (k, fv)) => map2(acc, fv)((m, v) => m + (k -> v))
    }
}


sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object Applicatives {
  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] =
      Stream.continually(a)

    def map2[A,B,C](a: Stream[A], b: Stream[B])( f: (A,B) => C): Stream[C] =
      a zip b map f.tupled

    override def map[A, B](fa: Stream[A])(f: (A) => B): Stream[B] =
      fa.map(f)
  }

  def monadEither[L] = new Monad[({type f[x] = Either[L, x]})#f] {
    override def flatMap[A, B](ma: Either[L, A])(f: (A) => Either[L, B]): Either[L, B] =
      ma match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
      }

    override def unit[A](a: A): Either[L, A] =
      Right(a)

    override def map[A, B](fa: Either[L, A])(f: (A) => B): Either[L, B] =
      fa match {
        case Right(a) => Right(f(a))
        case Left(e) => Left(e)
      }
  }

  def applicativeValidation[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def unit[A](a: => A): Validation[E, A] =
      Success(a)

    override def map2[A, B, C](aa: Validation[E, A], ab: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
      (aa, ab) match {
        case (Success(a1), Success(a2)) => Success(f((a1, a2)))
        case (e @ Failure(_, _), _) => e
        case (_, e @ Failure(_, _)) => e
        case (Failure(h1, tail1), Failure(h2, tail2)) => Failure(h1, tail1 ++ Vector(h2) ++ tail2)
      }

    override def map[A, B](fa: Validation[E, A])(f: (A) => B): Validation[E, B] =
      fa match {
        case Success(a) => Success(f(a))
        case Failure(head, tail) => Failure(head, tail)
      }
  }

}


trait Traverse[F[_]] extends Functor[F]  {
  def traverse[G[_],A,B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_],A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(ga => ga)

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(Monads.idMonad)

}
