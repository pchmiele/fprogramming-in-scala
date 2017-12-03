package Chapter6

import Chapter6.RNG._

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

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i  % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng2)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (value1, rng1) = f(rng)
      g(value1)(rng1)
    }

  def nonNegativeLessThanWithFlatMap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){ x =>
      val mod = x  % n
      if (x + (n-1) - mod >= 0)
        unit(mod)
      else nonNegativeLessThanWithFlatMap(n)
    }

  def mapUsingFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s) { a =>
      unit(f(a))
    }

  def map2UsingFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra){ a =>
      map(rb) { b =>
        f(a,b)
      }
    }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(f(a, _)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(state => {
      val (value2, state2) = run(state)
      f(value2).run(state2)
    })
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S,A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)( _ :: _ ))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  import State._

  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)
}