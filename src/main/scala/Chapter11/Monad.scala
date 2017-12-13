package Chapter11

import Chapter6.State

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Funtors {
  def listFunctor = new Functor[List] {
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] =
      fa.map(f)
  }


}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  override def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List.empty[A]))((ma, mla) => map2(ma, mla)(_ :: _))

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List.empty[B]))((a, mla) => map2(f(a), mla)(_ :: _))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h))(b =>
        if (!b) filterM(t)(f)
        else map(filterM(t)(f))(h :: _))
    }

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(flatMap(unit(a))(f))(g)

  def flatMapUsingCompose[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit)=> ma, f)(())

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(ma => ma)



  object Laws {
//    compose(compose(f, g), h) == compose(f, compose(g, h))
  }
}

object Monads {
  val optionMonad = new Monad[Option] {
    override def unit[A](a: A): Option[A] = Some(a)

    override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] =
      fa.flatMap(f)
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: (A) => List[B]): List[B] =
      fa.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: A): Stream[A] = Stream(a)

    override def flatMap[A, B](fa: Stream[A])(f: (A) => Stream[B]): Stream[B] =
      fa.flatMap(f)
  }

  val idMonad = new Monad[Id] {
    override def unit[A](a: A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] =
      f(ma.value)
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] =
      ma flatMap f
  }

}

case class Reader[R, A](run: R => A)
object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] =
      Reader(_ => a)

    def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
      f(st.run)
  }
}

case class Id[A](value: A)
