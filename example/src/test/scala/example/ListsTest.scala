package example

import example.Lists._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class ListsTest extends FunSuite {
  
  test("max of List(1, 2, 3) = 3")(assert(max(List(1, 2, 3)) == 3))

  test("max of List(3, 1, 3) = 3")(assert(max(List(3, 1, 3)) == 3))

  test("max of List() = NoSuchElementException exception") {
    intercept[NoSuchElementException] {
      max(List())
    }
  }

  test("sum of List(1, 2, 3) = 6")(assert(sum(List(1, 2, 3)) == 6))

  test("sum of List() = 0")(assert(sum(List()) == 0))

}