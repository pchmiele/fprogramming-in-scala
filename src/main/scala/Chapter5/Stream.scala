package Chapter5

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(head, tail) if n > 1 => cons(head(), tail().take(n-1))
    case Cons(head, tail) if n == 1 => cons(head(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(head, tail) if n > 0 => tail().drop(n -1)
    case Cons(head, tail) if n == 0 => this
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(head, tail) if (p(head())) => cons(head(), tail().takeWhile(p))
    case _ => empty
  }

  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] =
    this.foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty )

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(head, tail) if p(head()) => tail().forAll(p)
    case Empty => true
    case _ => false
  }

  def headOption: Option[A] =
    this.foldRight[Option[A]](None)((a,b) => Some(a))

  def map[B](p: A => B): Stream[B] =
    this.foldRight(empty[B])((head, tail) => cons(p(head), tail))

  def filter(p: A => Boolean): Stream[A] =
    this.foldRight(empty[A])((head, tail) => if (p(head)) cons(head, tail) else tail)

  def append[B>:A](stream: => Stream[B]): Stream[B] =
    this.foldRight(stream)((head, tail) => cons(head, tail))

  def flatMap[B](p: A => Stream[B]): Stream[B] =
    this.foldRight(empty[B])((head, tail) => p(head) append tail)

  def startsWith[B](stream: Stream[B]): Boolean =
    this.zipAll(stream) takeWhile(_._2.isEmpty != None) forAll{ case (a, b) => a == b }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(head, tail) => head() :: tail().toList
  }

  def mapUsingUnfold[B](p: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(head, tail) => Some(p(head()), tail())
    }

  def takeUsingUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(head, tail), x) if x > 1 => Some(head(), (tail(), x - 1))
      case (Cons(head, tail), 1) => Some(head(), (empty, 0))
      case _ => None
    }

  def takeWhileUsingUnfold(p: A => Boolean): Stream[A]  =
    unfold(this) {
      case Cons(head, tail) if (p(head())) => Some(head(), tail())
      case _ => None
    }

  def zipWith[B, C](stream: Stream[B])(f:(A, B) => C): Stream[C] = {
    unfold((this, stream)){
      case (Cons(x, xs), Cons(y, ys)) => Some(f(x(), y()), (xs(), ys()))
      case _ => None
    }
  }

  def zipAll[B](stream: Stream[B]): Stream[(Option[A],Option[B])] = {
    zipWithAll(stream)((_, _))
  }

  def zipWithAll[B, C](stream: Stream[B])(f:(Option[A], Option[B]) => C): Stream[C] = {
    unfold((this, stream)){
      case (Empty, Empty) => None
      case (Cons(head1, tail1), Cons(head2, tail2)) => {
        val value = f(Some(head1()), Some(head2()))
        val newState = (tail1(), tail2())
        Some(value, newState)
      }
      case (Empty, Cons(head2, tail2)) => {
        val value = f(None, Some(head2()))
        val newState = (empty[A], tail2())
        Some(value, newState)
      }
      case (Cons(head1, tail1), Empty) => {
        val value = f(Some(head1()), Option.empty[B])
        val newState = (tail1(), empty[B])
        Some(value, newState)
      }
    }
  }

  def tails: Stream[Stream[A]] =
    unfold(this){
      case Empty => None
      case Cons(head, tail) => Some(Cons(head, tail), tail())
    } append Stream(empty)
}
case object Empty extends Stream[Nothing]
case class  Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def fib(n: Int, m: Int): Stream[Int] = {
      cons(n, fib(m, n+m))
    }
    fib(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }
  }
}