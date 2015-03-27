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

  "Take(0)" should "return Nil on a non-empty list" in {
    assert (Nil == take(List(1, 2, 3), 0))
  }

  "Take(n)" should "throw an exception on an empty list" in {
    intercept[RuntimeException] {
      take(Nil, 1)
    }
  }
}
