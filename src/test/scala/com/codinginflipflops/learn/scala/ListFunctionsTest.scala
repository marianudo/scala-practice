import org.scalatest._
import com.codinginflipflops.learn.scala.ListFunctions._

class ListFunctionsTest extends FlatSpec {
  "Last" should "return the last value of a list multi-element list" in {
    assert(last(List(1, 2, 3, 4, 7)) == 7)
  }

  "Last" should "return the only element of a 1 element list" in {
    assert(last(List(5)) == 5)
  }

  "Last" should "throw an exception if the list is empty" in {
    intercept[RuntimeException] {
      last(Nil)
    }
  }

  "Init" should "return the beginning of a several elements list" in {
    val fixture = List(1, 3, 5, 2, 4, 6)
    val actual = init(fixture)
    assert (actual == List(1, 3, 5, 2, 4))
  }

  "Init" should "return Nil on a one element list" in {
    assert (init(List(4)) == Nil)
  }

  "Init" should "throw an exception if the list is empty" in {
    intercept[RuntimeException] {
      init(Nil)
    }
  }

  "Take(5)" should "return the first 5 elements of a seven elements list" in {
    assert (List(1, 2, 3, 4, 5) == take(List(1, 2, 3, 4, 5, 6, 7), 5))
  }

  "Take(3)" should "return the first 3 elements of a 7 elements list" in {
    assert (List(1, 3, 5) == take(List(1, 3, 5, 2, 4, 6, 7), 3))
  }

  "Take(1)" should "return the only element of a single element list" in {
    assert (List(2) == take(List(2), 1))
  }

  "Take(3)" should "return the input list when applied to a 3 elements list" in {
    assert (List(1, 2, 3) == take(List(1, 2, 3), 3))
  }

  "Take(0)" should "return Nil on a non-empty list" in {
    assert (Nil == take(List(1, 2, 3), 0))
  }

  "Take(5)" should "return the input list when invoked on a less than five elements list" in {
    assert (List(1, 2, 3) == take(List(1, 2, 3), 5))
  }

  "Take(n)" should "return Nil when invoked on an empty list" in {
    assert (Nil == take(Nil, 3))
  }

  "Drop(3)" should "drop the first 3 elements in a 5 elements list" in {
    assert (List(4, 5) == drop(List(1, 2, 3, 4, 5), 3))
  }

  "Drop(1)" should "return Nil on a single element list" in {
    assert (Nil == drop(List(5), 1))
  }

  "Drop(3)" should "return Nil on a 3 elements list" in {
    assert (Nil == drop(List(1, 2, 3), 3))
  }

  "Drop(3)" should "return the last element of a 4 elements list" in {
    assert (List(4) == drop(List(1, 2, 3, 4), 3))
  }

  "Drop(n)" should "throw an exception when invoked on Nil" in {
    intercept[RuntimeException] {
      drop(Nil, 1)
    }
  }

  "Drop(n)" should "throw an exception when invoked on a less than n elements list" in {
    intercept[RuntimeException] {
      drop(List(1, 2, 3), 4)
    }
  }

  "Drop(0)" should "return the input list" in {
    assert (List(1, 2, 3) == drop(List(1, 2, 3), 0))
  }

  "Zip" should "zip two non-empty, equally sized lists" in {
    assert (List((1, 2), (3, 4)) == zip(List(1, 3), List(2, 4)))
  }

  "Zip" should "return Nil of either of the 2 lists are empty" in {
    assert (Nil == zip(List(), List(1, 2)))
    assert (Nil == zip(List(1, 2), List()))
  }

  "Zip" should "zip two non-empty different size lists truncating the longer one" in {
    assert (List((1, 2), (3, 4)) == zip(List(1, 3, 5), List(2, 4)))
  }

  "Reverse" should "turn a list inside out" in {
    assert (List(1, 2, 3, 4) == reverse(List(4, 3, 2, 1)))
  }

  "Map" should "transform an input list in some way" in {
    assert (List(1, 4, 9) == map(List(1, 2, 3), x => x * x))
  }

}
