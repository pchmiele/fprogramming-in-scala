package Chapter6

import org.scalatest._

class StateSpec extends FlatSpec with Matchers {
  val (value1, rng) = RNG.Simple(1).nextInt

  "nonNegativeInt" should "return 1151252338" in {
    RNG.nonNegativeInt(rng)._1 should be (1151252338)
  }

  "double" should "return 0.5360936457291245" in {
    RNG.double(rng)._1 should be (0.5360936457291245)
  }

  "doubleUsingMap" should "return 0.5360936457291245" in {
    RNG.doubleUsingMap(rng)(rng)._1 should be (0.5360936457291245)
  }

  "intDouble" should "return (-1151252339,0.2558267889544368)" in {
    RNG.intDouble(rng)._1 should be ((-1151252339,0.2558267889544368))
  }

  "doubleInt" should "return (0.2558267889544368,-1151252339)" in {
    RNG.doubleInt(rng)._1 should be ((0.2558267889544368,-1151252339))
  }

  "randIntDouble" should "return (-1151252339,0.2558267889544368)" in {
    RNG.randIntDouble(rng)._1 should be ((-1151252339,0.2558267889544368))
  }

  "randDoubleInt" should "return (0.5360936457291245,-549383847)" in {
    RNG.randDoubleInt(rng)._1 should be ((0.5360936457291245,-549383847))
  }

  "double3" should "return (0.5360936457291245,0.2558267889544368,0.7510961224325001)" in {
    RNG.double3(rng)._1 should be (0.5360936457291245,0.2558267889544368,0.7510961224325001)
  }

  "nonNegativeLessThan" should "return value >= 0 and less than maximum" in {
    val result = RNG.nonNegativeLessThan(100)(rng)._1
    result should be >= 0
  }

  "nonNegativeLessThan" should "return same value as nonNegativeLessThan" in {
    val result1 = RNG.nonNegativeLessThan(100)
    val result2 = RNG.nonNegativeLessThanWithFlatMap(100)
    result1(rng) should be (result2(rng))
  }


}