package Chapter7

import org.scalatest._

class ParallelismSpec extends FlatSpec with Matchers {
  val myParImplementation = new Par[Int] {}
  import myParImplementation._


  //  "unit law" should "be filled" in {
  //    map(unit(1)) (_ + 1) should be (unit(2))
  //  }

}