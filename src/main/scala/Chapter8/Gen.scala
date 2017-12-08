package Chapter8

import Chapter6.{RNG, State}
import Chapter8.Prop.{FailedCase, SuccessCount}

case class Gen[A](sample: State[RNG, A])

object Gen {
  def listOf[A](gen: Gen[A]): Gen[List[A]] = ???
  def forAll[A](genList: Gen[List[A]])(predicate: A => Boolean): Prop = ???
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]

//  def &&(p: Prop): Prop = new Prop {
//    override def check: Either[(FailedCase, SuccessCount), SuccessCount] = this.check && p.check
//  }
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
}