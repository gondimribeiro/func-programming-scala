package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
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
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   * @Ignore annotation.
   */
  //@Ignore("not ready yet")
  @Test def `singleton set one contains one`: Unit = {

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

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }


  /**
   *
   * Intersect test
   */
  @Test def `intersection contains only intersection elements`: Unit = {
    new TestSets {
      val t1 = union(s1, s2)
      val t2 = union(s1, s3)
      val s = intersect(t1, t2)

      assert(contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }


  /**
   *
   * Diff test
   */
  @Test def `diff contains only diff elements`: Unit = {
    new TestSets {
      val t1 = union(s1, s2)
      val t2 = union(s1, s3)
      val s = diff(t1, t2)

      assert(!contains(s, 1), "Intersect 1")
      assert(contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  /**
   *
   * Filter test
   */
  @Test def `filter odd elements`: Unit = {
    new TestSets {
      val s = filter(union(union(s1, s2), s3), x => x % 2 == 1)

      assert(contains(s, 1), "Filter odd 1")
      assert(!contains(s, 2), "Filter odd  2")
      assert(contains(s, 3), "Filter odd  3")
    }
  }

  /**
   *
   * Forall test
   */
  @Test def `forall test`: Unit = {
    new TestSets {
      val s = union(union(s1, s2), s3)

      assert(forall(s, x => x < 10), "Less then 10")
      assert(!forall(s, x => x < 2), "Less then 2")
    }
  }


  /**
   *
   * Exists test
   */
  @Test def `exists test`: Unit = {
    new TestSets {
      val s = union(union(s1, s2), s3)

      assert(exists(s, x => x == 1), "Equal to 1")
      assert(exists(s, x => x == 2), "Equal to 2")
      assert(exists(s, x => x == 3), "Equal to 3")
      assert(!exists(s, x => !(x == 1 || x == 2 || x == 3)), "Different from 1, 2 and 3")
    }
  }

  /**
   *
   * Exists map
   */
  @Test def `map test`: Unit = {
    new TestSets {
      val s = map(union(union(s1, s2), s3), x => 2 * x)

      assert(!contains(s, 1), "Map 1")
      assert(contains(s, 2), "Map 2")
      assert(!contains(s, 3), "Map 3")
      assert(contains(s, 4), "Map 4")
      assert(!contains(s, 5), "Map 5")
      assert(contains(s, 6), "Map 6")
      assert(!exists(s, x => x > 6 || x < 0), "Less than 0 or greater than 6")
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
