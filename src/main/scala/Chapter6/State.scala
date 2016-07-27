package Chapter6

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (value, rng1) = rng.nextInt
    (if (value < 0) -(value+1) else value, rng1)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (value1, rng1) = nonNegativeInt(rng)
    (value1 / (Int.MaxValue.toDouble + 1), rng1)
  }

  def doubleUsingMap(rng: RNG): Rand[Double] = {
    map(nonNegativeInt)(i => (i/(Int.MaxValue.toDouble + 1)))
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (value1, rng1) = rng.nextInt
    val (value2, rng2) = double(rng1)
    ((value1, value2), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), rgn1) = intDouble(rng)
    ((d, i), rgn1)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (value1, rng1) = double(rng)
    val (value2, rng2) = double(rng1)
    val (value3, rng3) = double(rng2)
    ((value1, value2, value3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(count: Int, acc: (List[Int], RNG)): (List[Int], RNG) = count match {
      case 0 => acc
      case _ => {
        val (value, rng) = acc._2.nextInt
        val (list, prev_rng) = acc
        loop(count - 1, (value :: list, rng))
      }
    }
    loop(count, (Nil, rng))
  }

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      rng => {
        val (value1, rng1) =  ra(rng)
        val (value2, rng2) =  rb(rng1)

        (f(value1, value2), rng2)
      }
  }
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
