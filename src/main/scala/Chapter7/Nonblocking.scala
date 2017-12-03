package Chapter7

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

object Nonblocking {
  sealed trait Future[+A] {
    private[Nonblocking] def apply(cb: A => Unit): Unit
  }


  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def unit[A](a: A): Par[A] =
      _ => new Future[A] {
        def apply(cb: (A) => Unit) = {
          cb(a)
        }
      }

    def eval(executorService: ExecutorService)(r: => Unit): Unit =
      executorService.submit(new Callable[Unit] {
        def call = r
      })

    def fork[A](a: => Par[A]): Par[A] =
      (executorService: ExecutorService) => new Future[A] {
        def apply(cb: (A) => Unit) = {
          eval(executorService)(a(executorService)(cb))
        }
      }


    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      (executorService: ExecutorService) => new Future[C] {
        def apply(cb: (C) => Unit) = {
          var ar: Option[A] = None
          var br: Option[B] = None

          val combiner = Actor[Either[A, B]](executorService) {
            case Left(a) => br match {
              case None => ar = Some(a)
              case Some(b) => eval(executorService)(cb(f(a, b)))
            }
            case Right(b) => ar match {
              case None => br = Some(b)
              case Some(a) => eval(executorService)(cb(f(a, b)))
            }
          }

          p1(executorService)(a => combiner ! Left(a))
          p2(executorService)(b => combiner ! Right(b))
        }
      }

    def run[A](executorService: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)

      p(executorService) { a => ref.set(a); latch.countDown() }
      latch.await
      ref.get
    }

    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a, _) => f(a))

    def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
      map(parList)(_.sorted)

    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
      ps.foldRight[Par[List[A]]](unit(List.empty))((acc, a) => map2(acc, a)((x: A, y: List[A]) => x :: y))

    def parMap[A, B](parList: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = parList.map(asyncF(f))
      sequence(fbs)
    }

    def parFilter[A](elements: List[A])(f: A => Boolean): Par[List[A]] = {
      val result: List[Par[List[A]]] = elements.map(asyncF((a: A) => if (f(a)) List(a) else List.empty))
      map(sequence(result))(_.flatten)
    }

    def delay[A](a: => Par[A]): Par[A] =
      es => a(es)
  }
}
