package Chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree:Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(tree:Tree[Int]): Int = tree match {
    case Leaf(a) => a
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](tree:Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](tree:Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree:Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeUsingFold[A](tree:Tree[A]): Int =
    fold(tree)(a => 1)(1 + _ + _)

  def maximumUsingFold[A](tree:Tree[Int]): Int =
    fold(tree)(a => a)(_ max _)

  def depthUsingFold[A](tree:Tree[A]): Int =
    fold(tree)(a => 1)((l,r) => 1 + (l  max r ))

  def mapUsingFold[A, B](tree:Tree[A])(f: A => B): Tree[B] =
    fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}