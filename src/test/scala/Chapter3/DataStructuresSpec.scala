package Chapter3

import Chapter3._
import org.scalatest._

class ListSpec extends FlatSpec with Matchers {
  "tail" should "return List(2,3) for given List(1,2,3)" in {
    List.tail(List(1,2,3)) should be (List(2,3))
  }

  "setHead" should "return List(2,2,3) for given List(1,2,3) and 2" in {
    List.setHead(List(1,2,3), 2) should be (List(2,2,3))
  }

  "drop" should "return List(3) for given List(1,2,3) and 2" in {
    List.drop(List(1,2,3), 2) should be (List(3))
  }

  it should "return Nil on empty List" in {
    List.drop(Nil, 2) should be (Nil)
  }

  "dropWhile" should "skip elements until given function return false" in {
    List.dropWhile[Int](List(1,2,3), _ < 3) should be (List(3))
  }

  it should "return Nil on empty List" in {
    List.dropWhile[Int](Nil, _ < 3) should be (Nil)
  }

  "init" should "return List(1,2) for given List(1,2,3)" in {
    List.init(List(1,2,3)) should be (List(1,2))
  }

  "length" should "return 3 for given list(1,2,3)" in {
    List.length(List(1,2,3)) should be (3)
  }

  it should "return 0 for given Nil" in {
    List.length(Nil) should be (0)
  }

  "lengthWithFoldLeft" should "return 3 for given list(1,2,3)" in {
    List.lengthWithFoldLeft(List(1,2,3)) should be (3)
  }

  it should "return 0 for given Nil" in {
    List.lengthWithFoldLeft(Nil) should be (0)
  }

  "sumFoldLeft" should "return 6 for given List(1,2,3)" in {
    List.sumFoldLeft(List(1,2,3)) should be (6)
  }

  "productFoldLeft" should "return 6 for given List(1,2,3)" in {
    List.productFoldLeft(List(1,2,3)) should be (6)
  }

  "reverse" should "return List(3,2,1) for given List(1,2,3)" in {
    List.reverse(List(1,2,3)) should be (List(3,2,1))
  }

  "appendFoldRight" should "return List(1,2,3,4,5,6) for given List(1,2,3) and List(4,5,6)" in {
    List.appendFoldRight[Int](List(1,2,3), List(4,5,6)) should be (List(1,2,3,4,5,6))
  }

  "Append using foldLeftUsingFoldRight" should "return List(1,2,3,4,5,6) for given List(1,2,3) and List(4,5,6)" in {
    List.foldLeftUsingFoldRight[Int, List[Int]](List(1,2,3), List(4,5,6))((acc, a) => Cons(a, acc)) should be (List(1,2,3,4,5,6))
  }

  "concat" should "return List(1,2,3,4,5,6) for given List(List(1,2,3), List(4,5,6))" in {
    List.concat[Int](List(List(1,2,3),List(4,5,6))) should be (List(1,2,3,4,5,6))
  }

  "add1" should "return List(2,3,4) for given List(1,2,3)" in {
    List.add1(List(1,2,3)) should be (List(2,3,4))
  }

  "List.fromDoubleToString" should "return List(\"1.0\", \"2.0\", \"3.0\") for given List(1.0, 2.0, 3.0)" in {
    List.fromDoubleToString(List(1.0,2.0,3.0)) should be (List("1.0", "2.0", "3.0"))
  }

  "map" should "return Stream(\"1.0\", \"2.0\", \"3.0\") for given Stream(1.0,2.0,3.0) " in {
    List.map[Double, String](List(1.0,2.0,3.0))((x) => x.toString) should be (List("1.0", "2.0", "3.0"))
  }

  "filter" should "return Stream(2,4,6) for given Stream(1,2,3,4,5,6)" in {
    List.filter(List(1,2,3,4,5,6))((x) => x % 2 == 0) should be (List(2,4,6))
  }

  "filterUsingFlatMap" should "return Stream(2,4,6) for given Stream(1,2,3,4,5,6)"  in {
    List.filterUsingFlatMap(List(1,2,3,4,5,6))((x) => x % 2 == 0) should be (List(2,4,6))
  }

  "flatMap" should "return Stream(1,1,2,2,3,3) for given Stream(1,2,3)" in {
    List.flatMap(List(1,2,3))(i => List(i,i)) should be (List(1,1,2,2,3,3))
  }

  "sumLists" should "return List(5,7,9) for given List(1,2,3) and List(4,5,6)" in {
    List.sumLists(List(1,2,3), List(4,5,6)) should be (List(5,7,9))
  }

  "zipWith" should "return List(5,7,9) for given List(1,2,3) and List(4,5,6)" in {
    List.zipWith(List(1,2,3), List(4,5,6))((a, b) => a + b) should be (List(5,7,9))
  }

  "hasSubsequence" should "return true for given list(1,2,3,4,5) and nil" in {
    List.hasSubsequence(List(1,2,3,4,5), Nil) should be (true)
  }

  it should "return true for given list(1,2,3,4,5) and List(1,2,3,4,5)" in {
    List.hasSubsequence(List(1,2,3,4,5), List(1,2,3,4,5)) should be (true)
  }

  it should "return true for given list(1,2,3,4,5) and List(1,2,3)" in {
    List.hasSubsequence(List(1,2,3,4,5), List(1,2,3)) should be (true)
  }

  it should "return true for given list(1,2,3,4,5) and List(2,3)" in {
    List.hasSubsequence(List(1,2,3,4,5), List(2,3)) should be (true)
  }

  it should "return false for given list(1,2,3,4,5) and List(1,2,3,7)" in {
    List.hasSubsequence(List(1,2,3,4,5), List(1,2,3,7)) should be (false)
  }
}

class TreeSpec extends FlatSpec with Matchers {
  "Size" should "should return 1 for given single Leaf" in {
    Tree.size(Leaf(1)) should be(1)
  }

  it should "should return 3 for given branch with two Leafs" in {
    Tree.size(Branch(Leaf(1), Leaf(2))) should be(3)
  }

  "Max" should "should return maximal element of tree" in {
    Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)),Branch(Leaf(20), Leaf(5)))) should be(20)
  }

  "Depth" should "return correct depth of tree" in {
    Tree.depth(Branch(Leaf(1), Branch(Leaf(20), Leaf(5)))) should be(3)
  }

  "Map" should "return new tree with all elements * 2" in {
    Tree.map(Branch(Leaf(1), Branch(Leaf(20), Leaf(5))))(_ * 2) should be(Branch(Leaf(2), Branch(Leaf(40), Leaf(10))))
  }

  "Size using fold" should "should return 1 for given single Leaf" in {
    Tree.sizeUsingFold(Leaf(1)) should be(1)
  }

  it should "should return 3 for given branch with two Leafs" in {
    Tree.size(Branch(Leaf(1), Leaf(2))) should be(3)
  }

  "Maximum using fold" should "should return maximal element of tree" in {
    Tree.maximumUsingFold(Branch(Branch(Leaf(1), Leaf(2)),Branch(Leaf(20), Leaf(5)))) should be(20)
  }

  "Depth using fold" should "return correct depth of tree" in {
    Tree.depthUsingFold(Branch(Leaf(1), Branch(Leaf(20), Leaf(5)))) should be(3)
  }

  "Map using fold" should "return new tree with all elements * 2" in {
    Tree.map(Branch(Leaf(1), Branch(Leaf(20), Leaf(5))))(_ * 2) should be(Branch(Leaf(2), Branch(Leaf(40), Leaf(10))))
  }
}