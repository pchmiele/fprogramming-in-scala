import fpinscala.laziness._
import org.scalatest._

class StreamsSpec extends FlatSpec with Matchers {
  "toList" should "return Nil for given empty Stream" in {
    Empty.toList should be (Nil)
  }

  it should "return List(1,2,3) for given Stream(1,2,3)" in {
    Stream(1,2,3).toList should be (List(1,2,3))
  }

  "take(3)" should "return Empty for given empty Stream" in {
    Empty.take(3) should be (Empty)
  }

  it should "return Stream(1,2,3) for given Stream(1,2,3,4)" in {
    Stream(1,2,3,4).take(3).toList should be (List(1,2,3))
  }

  "take(5)" should "return Empty for given Stream(1,2,3,4)" in {
    Stream(1,2,3,4).take(5).toList  should be (List(1,2,3,4))
  }

  "drop(3)" should "return Empty for given empty Stream" in {
    Empty.drop(3) should be (Empty)
  }

  it should "return Stream(4) for given Stream(1,2,3,4)" in {
    Stream(1,2,3,4).drop(3).toList should be (List(4))
  }

  "drop(5)" should "return Empty for given Stream(1,2,3,4)" in {
    Stream(1,2,3,4).drop(5).toList  should be (Nil)
  }

  "takeWile( p => p < 2 )" should "return Stream(0,1) for given Stream(0,1,2,3,4)" in {
    Stream(0,1,2,3,4).takeWhile(p => p < 2).toList  should be (List(0,1))
  }

  "forall( p => p < 10 )" should "return true for given Stream(0,1,2,3,4)" in {
    Stream(0,1,2,3,4).forAll(p => p < 10) should be (true)
  }

  it should "return false for given Stream(0,1,2,10,4)" in {
    Stream(0,1,2,3,4).forAll(p => p < 10) should be (true)
  }

  "takeWhileUsingFoldRight( p => p < 2 )" should "return Stream(0,1) for given Stream(0,1,2,3,4)" in {
    Stream(0,1,2,3,4).takeWhileUsingFoldRight(p => p < 2).toList  should be (List(0,1))
  }

  "headOption" should "return Some(0) for given Stream(0,1,2,3,4)" in {
    Stream(0,1,2,3,4).headOption should be (Some(0))
  }

  it should "return None for given Empty" in {
    Empty.headOption should be (None)
  }

  "map" should "return Stream(\"1.0\", \"2.0\", \"3.0\") for given Stream(1.0,2.0,3.0) " in {
    Stream(1.0,2.0,3.0).map((x) => x.toString).toList should be (List("1.0", "2.0", "3.0"))
  }

  "filter" should "return Stream(2,4,6) for given Stream(1,2,3,4,5,6)" in {
    Stream(1,2,3,4,5,6).filter((x) => x % 2 == 0).toList should be (List(2,4,6))
  }

  "flatMap" should "return Stream(1,1,2,2,3,3) for given Stream(1,2,3)" in {
    Stream(1,2,3).flatMap(i => Stream(i,i)).toList should be (List(1,1,2,2,3,3))
  }

  "append" should "return Stream(1,2,3,4,5,6) for given Stream(1,2,3) and Stream(4,5,6)" in {
    (Stream(1,2,3) append Stream(4,5,6)).toList should be (List(1,2,3,4,5,6))
  }
}