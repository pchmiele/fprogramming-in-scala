import fpinscala.datastructures._
import org.scalatest._

class ListSpec extends FlatSpec with Matchers {
  "Tail" should "return tail of non empty list" in {
    List.tail(List(1,2,3)) should be (List(2,3))
  }

  "SetHead" should "change value of head correctly" in {
    List.setHead(List(1,2,3), 2) should be (List(2,2,3))
  }

  "Drop" should "skip given number of elements" in {
    List.drop(List(1,2,3), 2) should be (List(3))
  }

  it should "return Nil on empty List" in {
    List.drop(Nil, 2) should be (Nil)
  }

  "DropWhile" should "skip elements until given function return false" in {
    List.dropWhile[Int](List(1,2,3), _ < 3) should be (List(3))
  }

  it should "return Nil on empty List" in {
    List.dropWhile[Int](Nil, _ < 3) should be (Nil)
  }

  "Init" should "return entire list without last element" in {
    List.init(List(1,2,3)) should be (List(1,2))
  }

  "Length" should "return correct length of List" in {
    List.length(List(1,2,3)) should be (3)
  }

  it should "return 0 on empty List" in {
    List.length(Nil) should be (0)
  }

  "lengthWithFoldLeft" should "return correct length of List" in {
    List.lengthWithFoldLeft(List(1,2,3)) should be (3)
  }

  it should "return 0 on empty List" in {
    List.lengthWithFoldLeft(Nil) should be (0)
  }

  "SumFoldLeft" should "return sum of all elements of List" in {
    List.sumFoldLeft(List(1,2,3)) should be (6)
  }

  "ProductFoldLeft" should "return product of all elements of List" in {
    List.productFoldLeft(List(1,2,3)) should be (6)
  }

  "Reverse" should "return List with elements in reversed order" in {
    List.reverse(List(1,2,3)) should be (List(3,2,1))
  }

  "AppendFoldRight" should "append correctly two lists" in {
    List.appendFoldRight[Int](List(1,2,3), List(4,5,6)) should be (List(1,2,3,4,5,6))
  }

  "Append using foldLeftUsingFoldRight" should "append correctly two lists" in {
    List.foldLeftUsingFoldRight[Int, List[Int]](List(1,2,3), List(4,5,6))((acc, a) => Cons(a, acc)) should be (List(1,2,3,4,5,6))
  }

  "Concat list of lists" should "return single list" in {
    List.concat[Int](List(List(1,2,3),List(4,5,6))) should be (List(1,2,3,4,5,6))
  }

  "Add1" should "increment all elements of list by 1" in {
    List.add1(List(1,2,3)) should be (List(2,3,4))
  }

  "List.fromDoubleToString" should "convert all Double elements into String" in {
    List.fromDoubleToString(List(1.0,2.0,3.0)) should be (List("1.0", "2.0", "3.0"))
  }

  "Map" should "apply function to all elements" in {
    List.map[Double, String](List(1.0,2.0,3.0))((x) => x.toString) should be (List("1.0", "2.0", "3.0"))
  }

  "Filter" should "filter all odd elements from" in {
    List.filter(List(1,2,3,4,5,6))((x) => x % 2 == 1) should be (List(2,4,6))
  }

  "filterUsingFlatMap" should "filter all odd elements from" in {
    List.filterUsingFlatMap(List(1,2,3,4,5,6))((x) => x % 2 == 1) should be (List(2,4,6))
  }

  "FlatMap" should "should works correctly" in {
    List.flatMap(List(1,2,3))(i => List(i,i)) should be (List(1,1,2,2,3,3))
  }

  "SumLists" should "should be sum of element of 2 lists" in {
    List.sumLists(List(1,2,3), List(4,5,6)) should be (List(5,7,9))
  }

  "ZipWith" should "should works fine" in {
    List.zipWith(List(1,2,3), List(4,5,6))((a, b) => a + b) should be (List(5,7,9))
  }

  "hasSubsequence" should "return true when looking for empty list" in {
    List.hasSubsequence(List(1,2,3,4,5), Nil) should be (true)
  }

  it should "return true when looking for same list" in {
    List.hasSubsequence(List(1,2,3,4,5), List(1,2,3,4,5)) should be (true)
  }

  it should "return true when looking for sublist - ver1" in {
    List.hasSubsequence(List(1,2,3,4,5), List(1,2,3)) should be (true)
  }

  it should "return true when looking for sublist - ver2" in {
    List.hasSubsequence(List(1,2,3,4,5), List(2,3)) should be (true)
  }

  it should "return false when looking for sublist with not existring elements" in {
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