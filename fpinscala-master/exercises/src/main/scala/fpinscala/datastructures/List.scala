package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sumFoldLeft(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def productFoldLeft(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] =
    l match {
      case Cons(_, xs) => xs
      case _ => sys.error("tail on empty list")
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Cons(_, xs) => Cons(h, xs)
      case _ => sys.error("setHead on empty list")
    }

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case _ if n <= 0 => l
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if (f(x)) => dropWhile(xs, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
      case Nil => sys.error("init of empty list")
    }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)( (a,acc) => acc + 1 )

  def lengthWithFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0)( (acc, x) => acc + 1 )

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((acc, a) => Cons(a, acc))

  def appendFoldRight[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)((a, acc) => Cons(a, acc))

  def foldRightUsingFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, z)((acc, elem) => f(elem,acc))

  def foldLeftUsingFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B=
    foldRight(l, z)((elem, acc) => f(acc,elem))

  def concat[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil:List[A])(append)

  def add1(list: List[Int]): List[Int] =
    foldRight(list, Nil:List[Int])((a, acc) => Cons(a + 1, acc))

  def fromDoubleToString(list: List[Double]): List[String] =
    foldRight(list, Nil:List[String])((a, acc) => Cons(a.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((a, acc) => if (f(a) == false) Cons(a, acc) else acc )

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if(f(a)) Nil else List(a))

  def sumLists(list: List[Int], list1: List[Int]) = {
    @annotation.tailrec
    def loop(l1: List[Int], l2: List[Int], acc: List[Int]): List[Int] =
      (l1, l2) match {
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(x, xs), Cons(y, ys)) => loop(xs, ys, Cons(x + y, acc))
      }

      reverse(loop(list,list1, Nil))
  }

  def zipWith[A](list: List[A], list1: List[A])(f:(A, A) => A) = {
    @annotation.tailrec
    def loop(l1: List[A], l2: List[A], acc: List[A]): List[A] =
      (l1, l2) match {
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(x, xs), Cons(y, ys)) => loop(xs, ys, Cons(f(x,y), acc))
      }

    reverse(loop(list,list1, Nil))
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Nil,Nil) => true
    case (Nil, _) => false
    case (_, Nil) => true
    case (Cons(x, xs), Cons(y, ys)) if x == y => hasSubsequence(xs, ys)
    case (Cons(x, xs), Cons(y, ys)) => hasSubsequence(xs, sub)
  }
}
