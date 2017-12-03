package Chapter7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

trait Par[A] {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] =
    _ => UnitFuture(a)

  private case class UnitFuture[A](a: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(): A = a

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def fork[A](a: => Par[A]): Par[A] =
    (executorService: ExecutorService) => {
      executorService.submit(new Callable[A] {
        override def call(): A = a(executorService).get
      })
    }

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (executorService: ExecutorService) => {
      val futureA = a(executorService)
      val futureB = b(executorService)

      UnitFuture(f(futureA.get, futureB.get))
    }

  def run[A](executorService: ExecutorService)(a: Par[A]): Future[A] = a(executorService)

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
    val result: List[Par[List[A]]] = elements.map(asyncF((a: A) => if(f(a)) List(a) else List.empty))
    map(sequence(result))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](a: => Par[A]): Par[A] =
    es => a(es)

}