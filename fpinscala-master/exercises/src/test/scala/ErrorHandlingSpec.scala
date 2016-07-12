import fpinscala.errorhandling._
import org.scalatest._

class OptionsSpec extends FlatSpec with Matchers {
  def helper(a: Int): Option[Int] =
    if (a == 0) None else Some(a)

  "Map" should "return None if used on None" in {
    None.map(a => a) should be(None)
  }

  it should "return Some(val) if used on None" in {
    Some("test").map(_.toUpperCase) should be(Some("TEST"))
  }

  "Map2" should "return None if first param is None" in {
    Option.map2[Int, Int, Int](None, Some(1))((a,b) => a * b) should be(None)
  }

  it should "return None if second param is None" in {
    Option.map2[Int, Int, Int](Some(1), None)((a,b) => a * b) should be(None)
  }

  it should "return Some(6) if params are Some(2) and Some(3)" in {
    Option.map2(Some(2), Some(3))((a,b) => a * b) should be(Some(6))
  }

  "Variance" should "return 2.0 for given Seq(1,2,3,4,5)" in {
    Option.variance(Seq(1,2,3,4,5)) should be(Some(2.0))
  }

  it should "return None for given empty seq" in {
    Option.variance(Nil) should be(None)
  }

  "Sequence" should "return None for any number of None in given List" in {
    Option.sequence(List(Some(1),None, Some(2))) should be(None)
  }

  it should "return Some(List(1,2,3)) for given List(Some(1), Some(2), Some(3))" in {
    Option.sequence(List(Some(1),Some(2), Some(3))) should be(Some(List(1,2,3)))
  }

  "Traverse" should "return None when given method fails for any given element of list" in {
    Option.traverse(List(1, 2, 0))(helper) should be(None)
  }

  it should "return Some(List(1,2,3)) when everything is ok" in {
    Option.traverse(List(1, 2, 3))(helper) should be(Some(List(1,2,3)))
  }
}

class EitherSpec extends FlatSpec with Matchers {
  def helper[A](a: A): Either[A, A] =
    if (a == 0) Left(a) else Right(a)

  "Map" should "return Left(1) when used on Left(1)" in {
    Left(1).map((a: Int) => a * 2 ) should be(Left(1))
  }

  it should "return Right(2) when used on Right(1)" in {
    Right(1).map((a: Int) => a * 2 ) should be(Right(2))
  }

  "FlatMap" should "return Right(2) when used on Right(1)" in {
    Right(1).flatMap((a: Int) => Right(a * 2)) should be(Right(2))
  }

  it should "return Left(1) when used on Left(1)" in {
    Left(1).flatMap((a: Int) => Right(a * 2)) should be(Left(1))
  }

  "orElse" should "return Right(10) when used on Left(1)" in {
    Left(1).orElse(Right(10)) should be(Right(10))
  }

  it should "return Right(1) when used on Right(1)" in {
    Right(1).orElse(Right(10)) should be(Right(1))
  }

  "Map2" should "return Left(2) if first param is Left(2) and second param is Left(3)" in {
    Left(2).map2[Int, Int, Int](Left(3))((a: Int, b: Int) => a * b) should be(Left(2))
  }

  it should "return Left(2) if first param is Right(2) and second param is Left(3)" in {
    Right(2).map2[Int, Int, Int](Left(3))((a: Int, b: Int) => a * b) should be(Left(3))
  }

  it should "return Left(6) if first param is Right(2) and second is Right(3)" in {
    Right(2).map2[Int, Int, Int](Right(3))((a: Int, b: Int) => a * b) should be(Right(6))
  }

  it should "return Left(2) if first param is Left(2) and second is Right(3)" in {
    Left(2).map2[Int, Int, Int](Right(3))((a: Int, b: Int) => a * b) should be(Left(2))
  }

  "Sequence" should "return Left if there is any Left in given List" in {
    Either.sequence(List(Right(1),Left(1), Right(1))) should be(Left(1))
  }

  it should "return Right(List(1,2,3)) for given List(Right(1), Right(2), Right(3))" in {
    Either.sequence(List(Right(1),Right(2), Right(3))) should be(Right(List(1,2,3)))
  }

  "Traverse" should "return Left when given method returns Left for any given element of list" in {
    Either.traverse(List(1,0,3))(helper) should be(Left(0))
  }

  it should "return Right(List(1,2,3)) when given method returns no Left for any given element of list" in {
    Either.traverse(List(1,2,3))(helper) should be(Right(List(1,2,3)))
  }
}