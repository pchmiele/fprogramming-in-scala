import fpinscala.gettingstarted.MyModule._
import fpinscala.gettingstarted.PolymorphicFunctions._
import org.scalatest._

class DataStructuresSpec extends FlatSpec with Matchers {
  "Fibonacci" should "for given 0 return 0" in {
    fib(0) should be (0)
  }

  it should "for given 1 return 1" in {
    fib(1) should be (1)
  }

  it should "for given 2 return 1" in {
    fib(2) should be (1)
  }

  it should "for given 3 return 2" in {
    fib(2) should be (1)
  }

  "IsSorted" should "return true for sorted array of ints" in {
    isSorted[Int](Array(1,2,3),(a,b) => a < b) should be (true)
  }

  it should "return false for unsorted array of ints" in {
    isSorted[Int](Array(1,5,3),(a,b) => a < b) should be (false)
  }

  it should "return true for empty array" in {
    isSorted[Int](Array[Int](),(a,b) => a < b) should be (true)
  }
}