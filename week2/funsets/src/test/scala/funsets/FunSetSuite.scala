package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 * - run the "test" command in the SBT console
 * - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   * - test
   * - ignore
   * - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }


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
   * val s1 = singletonSet(1)
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
      assert(contains(s2, 2), "Singleton")
      assert(contains(s3, 3), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
      val _1: Set = x=> Array(1,3,4,5,7,1000).contains(x)
      val _2: Set = x=> Array(1,2,3,4).contains(x)
      println(FunSets.toString(union(_1,_2)))
    }
  }

  test("intersect") {
    new TestSets {
      val s = union(s1, s2)
      val i1 = intersect(s, s1);
      assert(contains(i1, 1), "Intersect with 1")
      val i2 = intersect(s, s2);
      assert(contains(i2, 2), "Intersect with 2")
      val i3 = intersect(s, s3);
      assert(!contains(i3, 1) && !contains(i3, 2) && !contains(i3, 3), "Empty intersect")
      val i4 = intersect(union(s1, s2), union(s2, s3))
      assert(contains(i4, 2), "1&2 intersect with 2&3 = 2")
    }
  }

  test("diff") {
    new TestSets {
      val s = union(s1, s2)
      val d1 = diff(s, s1);
      assert(contains(d1, 2), "Diff with 1")
      val d2 = diff(s, s2);
      assert(contains(d2, 1), "Diff with 2")
      val d3 = diff(s, s3);
      assert(contains(d3, 1) && contains(d3, 2), "should contain 1 & 2 only")
      val d4 = diff(union(s1, s2), union(s2, s3))
      assert(contains(d4, 1), "1&2 diff with 2&3 = 1")
    }
  }

  test("filter") {
    new TestSets {
      val _1: Set = x => Array(1, 2, 3).contains(x)
      val p1: Int => Boolean = x => x > 5;
      val f1: Set = filter(_1, p1)
      assert(!contains(f1, 1) && !contains(f1, 2) && !contains(f1, 3))
      val _2: Set = x => Array(1, 3, 5).contains(x)
      val p2: Int => Boolean = x => x % 2 == 1;
      val f2: Set = filter(_2, p2)
      assert(contains(f2, 1) && contains(f2, 3) && contains(f2, 5));
    }
  }

  test("forall") {
    new TestSets {
      val _1: Set = x => Array(1, 2, 3).contains(x)
      val p1: Int => Boolean = x => x > 5;
      assert(!forall(_1, p1))
      val _2: Set = x => Array(1, 3, 5, 101).contains(x)
      val p2: Int => Boolean = x => x % 2 == 1;
      assert(forall(_2, p2));
    }
  }

  test("exists") {
    new TestSets {
      val _1: Set = x => Array(1, 2, 3).contains(x)
      val p1: Int => Boolean = x => x > 5;
      assert(!exists(_1, p1))
      val _2: Set = x => Array(1, 200, 100, 101).contains(x)
      val p2: Int => Boolean = x => x % 2 == 1;
      assert(exists(_2, p2));
      val _3: Set = x => Array(2, 200, 100, 101).contains(x)
      val p3: Int => Boolean = x => x == 1;
      assert(!exists(_3, p3));
    }
  }

  test("map") {
    new TestSets {
      val _1: Set = x => Array(1, 2, 3).contains(x)
      val m1: Set = map(_1, x => x * 2)
      println(FunSets.toString(m1))
      assert(contains(m1, 2) && contains(m1, 4) && contains(m1, 6) && !contains(m1, 1))
      val _2: Set = x => Array(1, 2, 3).contains(x)
      val m2: Set = map(_1, x => x-1)
      println(FunSets.toString(m2))
      assert(contains(m2, 0))

      val mapped: Set = map(x => x > -100 && x < 15, x => x * 2)
      println(FunSets.toString(mapped))
      assert(forall(mapped, x => x % 2 == 0))
    }
  }
}
