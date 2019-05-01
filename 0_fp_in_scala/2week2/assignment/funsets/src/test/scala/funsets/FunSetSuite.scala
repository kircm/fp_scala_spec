package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains elements that belong to both sets") {
    new TestSets {
      val u12 = union(s1, s2)
      val u23 = union(s2, s3)

      val s = intersect(u12, u23)

      assert(!contains(s, 1), "Intersect 1")
      assert(contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  test("diff contains elements that belong to only the s set") {
    new TestSets {
      val u12 = union(s1, s2)
      val u23 = union(s2, s3)

      val s = diff(u12, u23)

      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
    }
  }

  test("filter returns elements that fulfill predicate") {
    new TestSets {
      val s4 = singletonSet(4)
      val s5 = singletonSet(5)
      val s6 = singletonSet(6)

      val u12 = union(s1, s2)
      val u34 = union(s3, s4)
      val u56 = union(s5, s6)
      val u1234 = union(u12, u34)
      val u123456 = union(u1234, u56)

      val s = filter(u123456, el => el > 3)

      assert(!contains(s, 1), "Filter 1")
      assert(!contains(s, 2), "Filter 2")
      assert(!contains(s, 3), "Filter 3")
      assert(contains(s, 4), "Filter 4")
      assert(contains(s, 5), "Filter 5")
      assert(contains(s, 6), "Filter 6")
    }
  }

  test("forall checks property on all integers within set") {
    new TestSets {
      val s4 = singletonSet(4)
      val s5 = singletonSet(5)
      val s6 = singletonSet(6)

      val u24 = union(s2, s4)
      val u246 = union(u24, s6)

      assert(forall(u246, el => el > 0))
      assert(forall(u246, el => el % 2 == 0))
      assert(!forall(u246, el => el < 4))
    }
  }

  test("exists checks existing integer with property within set") {
    new TestSets {
      val s4 = singletonSet(4)
      val s5 = singletonSet(5)
      val s9 = singletonSet(9)

      val u24 = union(s2, s4)
      val u249 = union(u24, s9)

      assert(!exists(u249, el => el < 0))
      assert(exists(u249, el => el % 2 == 0))
      assert(exists(u249, el => el % 3 == 0))
    }
  }

  test("map transforms a set into another set applying given function") {
    new TestSets {
      val s5 = singletonSet(5)
      val s7 = singletonSet(6)

      val u13 = union(s1, s3)
      val u57 = union(s5, s7)

      val u1357 = union(u13, u57)

      val s = map(u1357, el => el + 1)

      assert(!contains(s, 1))
      assert(!contains(s, 3))
      assert(!contains(s, 5))
      assert(!contains(s, 7))
      assert(contains(s, 2))
      assert(contains(s, 4))
      assert(contains(s, 6))
      assert(contains(s, 8))
    }
  }
}
