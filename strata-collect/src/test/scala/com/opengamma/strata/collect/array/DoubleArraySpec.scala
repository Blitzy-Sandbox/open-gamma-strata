/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */

// This file declares two top-level packages, which is why it is written with package blocks
// rather than with a leading package clause. The suite belongs in the package of its subject,
// `com.opengamma.strata.collect.array`; the access probe at the foot of the file must sit
// *outside* `com.opengamma.strata.collect`, because what it proves is that the two escape
// hatches of the array type are unreachable from there, and a compile-time access check is
// answered in the package of the code that asks. A block nested inside a package clause would
// nest under that clause and stay inside `collect`, which is exactly the arrangement that could
// not prove anything, so the two blocks are siblings at the root.
package com.opengamma.strata.collect.array {

  import java.util.Arrays

  import scala.annotation.tailrec
  import scala.collection.immutable.ArraySeq

  import cats.Eq
  import cats.Hash
  import cats.Show

  import org.scalacheck.Gen
  import org.scalatest.Assertion
  import org.scalatest.funsuite.AnyFunSuite
  import org.scalatest.matchers.should.Matchers
  import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

  import com.opengamma.strata.collect.Arbitraries
  import com.opengamma.strata.collect.DoubleArrayMath
  import com.opengamma.strata.collect.json.Codecs

  /**
   * Test [[DoubleArray]].
   *
   * The Java original held forty-eight test methods and every one of them is carried here under
   * its own name, so that the migration is traceable method by method rather than in bulk: the
   * test-scope manifest of this port maps each Java method to the test below that answers for it,
   * and nothing in this class is recorded as partial or dropped. Four of the Java methods exercise
   * members that the port does not have; each of those is consolidated onto the member that
   * replaces it and still has a test of its own, listed as such in the map below. Every
   * hard-coded expected value of the original is carried across unchanged, including the
   * comparison tolerance of 1e-14 that its array helper used.
   *
   * ===Java method to Scala test===
   *
   *  - `test_EMPTY` -> `test_EMPTY`, ported
   *  - `test_of` -> `test_of`, ported through the single varargs factory that replaces the ten
   *    arity-specific overloads of the original
   *  - `test_of_lambda` -> `test_of_lambda`, ported onto `DoubleArray.tabulate`
   *  - `test_of_stream` -> `test_of_stream`, consolidated onto construction from a sequence, since
   *    the port has no factory taking a primitive stream
   *  - `test_ofUnsafe` -> `test_ofUnsafe`, ported
   *  - `test_copyOf_List` -> `test_copyOf_List`, ported onto the immutable list of the standard
   *    library, which replaces the immutable-list fixture of the original
   *  - `test_copyOf_array` -> `test_copyOf_array`, ported
   *  - `test_copyOf_array_fromIndex` -> `test_copyOf_array_fromIndex`, ported
   *  - `test_copyOf_array_fromToIndex` -> `test_copyOf_array_fromToIndex`, ported
   *  - `test_filled` -> `test_filled`, ported
   *  - `test_filled_withValue` -> `test_filled_withValue`, ported
   *  - `test_get` -> `test_get`, ported
   *  - `test_contains` -> `test_contains`, ported
   *  - `test_indexOf` -> `test_indexOf`, ported
   *  - `test_lastIndexOf` -> `test_lastIndexOf`, ported
   *  - `test_copyInto` -> `test_copyInto`, consolidated onto the copying `toArray`, since the port
   *    has no member that writes into an array the caller supplies
   *  - `test_subArray_from` -> `test_subArray_from`, ported
   *  - `test_subArray_fromTo` -> `test_subArray_fromTo`, ported
   *  - `test_toList` -> `test_toList`, ported
   *  - `test_toList_iterator` -> `test_toList_iterator`, ported
   *  - `test_toList_listIterator` -> `test_toList_listIterator`, consolidated onto the cursors an
   *    immutable list offers, since it has no bidirectional mutating cursor
   *  - `test_stream` -> `test_stream`, consolidated onto `iterator`
   *  - `test_forEach` -> `test_forEach`, ported
   *  - `test_with` -> `test_with`, ported
   *  - `test_plus` -> `test_plus`, ported
   *  - `test_minus` -> `test_minus`, ported
   *  - `test_multipliedBy` -> `test_multipliedBy`, ported
   *  - `test_dividedBy` -> `test_dividedBy`, ported
   *  - `test_map` -> `test_map`, ported
   *  - `test_mapWithIndex` -> `test_mapWithIndex`, ported
   *  - `test_plus_array` -> `test_plus_array`, ported
   *  - `test_minus_array` -> `test_minus_array`, ported
   *  - `test_multipliedBy_array` -> `test_multipliedBy_array`, ported
   *  - `test_dividedBy_array` -> `test_dividedBy_array`, ported
   *  - `test_combine` -> `test_combine`, ported
   *  - `test_combineReduce` -> `test_combineReduce`, ported
   *  - `test_sorted` -> `test_sorted`, ported
   *  - `test_min` -> `test_min`, ported
   *  - `test_max` -> `test_max`, ported
   *  - `test_sum` -> `test_sum`, ported
   *  - `test_reduce` -> `test_reduce`, ported
   *  - `test_concat_varargs` -> `test_concat_varargs`, ported
   *  - `test_concat_object` -> `test_concat_object`, ported
   *  - `test_equalWithTolerance` -> `test_equalWithTolerance`, ported
   *  - `test_equalZeroWithTolerance` -> `test_equalZeroWithTolerance`, ported
   *  - `test_equalsHashCode` -> `test_equalsHashCode`, ported
   *  - `test_toString` -> `test_toString`, ported
   *  - `coverage` -> `coverage`, ported as compile-time and instance assertions in place of the
   *    reflective bean sweep and meta-property probes of the original, which have no target here
   *    and whose machinery this port does without entirely
   *
   * The remaining tests answer requirements of this port rather than of the Java original: the
   * copy-safety and deliberate-aliasing tests (`copy_safety_*`, `aliasing_*`), the proof that the
   * two escape hatches cannot be reached from outside this module
   * (`unsafe_members_are_inaccessible_outside_collect`), the failure paths of the sized and range
   * factories that the original left untested (`negative_size_of_filled_and_tabulate`,
   * `reversed_range_of_copyOf_and_subArray`), the bit-level equality cases (`ieee_*`) and the
   * property section (`property_*`).
   *
   * ===Divergences from the Java original that this spec asserts===
   *
   * Each of these is asserted below rather than described only, and each is a candidate line for
   * the migration note of the port:
   *
   *  - a bound beyond the end of the array being copied - `copyOf(array, 4)`,
   *    `copyOf(array, 0, 5)`, `subArray(4)`, `subArray(0, 4)` - fails with
   *    `IllegalArgumentException` where the original
   *    raised `IndexOutOfBoundsException`. The message text is unchanged. An index that reaches the
   *    stored array directly, and a negative start index, still surface as the index exception the
   *    runtime raises, exactly as in the original;
   *  - `min` and `max` on an empty array fail with `IllegalArgumentException` where the original
   *    raised `IllegalStateException`. Both messages are unchanged, and both are asserted here;
   *  - `equalWithTolerance` never matches a not-a-number element: an array holding one is not
   *    equal within any tolerance to an array holding one at the same index, because no tolerance
   *    reaches such a value. Bit-for-bit structural equality - `equals`, `hashCode` and the
   *    lookups built on them - does keep such an element reflexive, and that asymmetry between
   *    the two contracts is deliberate: both are asserted here, side by side. The fuzzy contract
   *    itself belongs to the comparison this delegates to, whose own spec owns it;
   *  - `ofUnsafe` and `toArrayUnsafe` are visible only inside this module, where the original
   *    exposed both to every caller. This spec is inside the module and exercises both positively;
   *    the prohibition outside it is proved from a probe object in a sibling package;
   *  - the port has no meta-bean, no ordering and no JSON codec of its own for this type. The
   *    absence of each is asserted, next to a positive control proving the same assertion shape
   *    succeeds where the member or instance does exist, so that none of them can pass merely
   *    because the expression was malformed;
   *  - every zero-length result is the canonical empty instance, as in the original, so the helper
   *    below asserts that identity rather than merely a size of zero.
   *
   * ===Tolerances===
   *
   * The element comparison of the array helper keeps the 1e-14 tolerance of the original, and
   * every other comparison here is exact. The numerical parity of this type against values
   * captured from the Java implementation, to 1e-9 absolute and relative, is measured by the
   * parity spec of this module against its committed fixture; it is deliberately not duplicated
   * here, and this spec reads no fixture and performs no effect.
   */
  final class DoubleArraySpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

    /**
     * The element comparison tolerance of the Java original, carried across unchanged.
     *
     * It is used only by [[assertArray]], which is the helper the original used it in.
     */
    private val DELTA: Double = 1e-14

    /**
     * A value of an unrelated type, mirroring the fixture of the same purpose in the original.
     *
     * It is typed as `Any` so that it can be handed to `equals`, and to the list lookups that
     * accept a supertype of the element, without the compiler inferring that supertype for a call
     * that was written with a `Double` in mind.
     */
    private val anotherType: Any = ""

    /**
     * An absent reference, bound rather than written inline.
     *
     * The Java original asserted that an array does not equal `null`. Binding the reference keeps
     * that case while keeping a literal comparison against nothing out of the source.
     */
    private val nullRef: Any = null

    /**
     * The hashing of arrays, summoned once for the properties that use it.
     *
     * [[coverage]] summons the instances itself, because summoning them is part of what that test
     * asserts; everything else uses this one.
     */
    private val arrayHash: Hash[DoubleArray] = implicitly[Hash[DoubleArray]]

    /**
     * The bit pattern of a value, which is how this type compares elements.
     *
     * Every assertion about an element of an array that may hold a not-a-number value or a signed
     * zero is written through this method, because the numeric comparison of the platform
     * disagrees with the equality of this type on exactly those values: it reports a not-a-number
     * value as unequal to itself and the two zeroes as equal, so asserting with it would pass for
     * the wrong reason wherever it did pass at all.
     *
     * @param value  the value to render
     * @return the bit pattern of the value
     */
    private def bitsOf(value: Double): Long = java.lang.Double.doubleToLongBits(value)

    /**
     * Answers whether two values are the same instance.
     *
     * The typeclasses of this library are universal traits, so that a value class can carry one;
     * an instance of one is therefore not statically known to be a reference, and identity cannot
     * be asked of it directly. Naming the two arguments as values and asking the question through
     * this method is what makes the question expressible. At run time it is exactly the identity
     * comparison, because none of the instances compared here is a value class.
     *
     * @param left  the first value
     * @param right  the second value
     * @return true if the two are the same instance
     */
    private def sameInstance(left: Any, right: Any): Boolean =
      left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]

    /**
     * Asserts that an array holds exactly the expected values, as the helper of the same name in
     * the Java original did.
     *
     * With no expected value the array must be the canonical empty instance, which is asserted by
     * reference: every factory and every operation of the port answers with that one instance
     * rather than with a fresh empty array, so identity is a stronger and equally true statement
     * than emptiness. Otherwise the size is checked, then the contents through both the copying
     * accessor and the one that hands back the stored array - checking both is what the original
     * did, and it is what makes the copy and the stored array provably agree - and finally the two
     * invariants every array of this type reports, one dimension and non-emptiness.
     *
     * @param array  the array to inspect
     * @param expected  the values it must hold, in order
     * @return the assertion that the array holds those values
     */
    private def assertContent(array: DoubleArray, expected: Double*): Assertion =
      if (expected.isEmpty) {
        array should be theSameInstanceAs DoubleArray.EMPTY
        array.isEmpty shouldBe true
      } else {
        array.size shouldBe expected.length
        assertArray(array.toArray, expected)
        assertArray(array.toArrayUnsafe, expected)
        array.dimensions shouldBe 1
        array.isEmpty shouldBe false
      }

    /**
     * Asserts that a run of values matches the expected values element by element.
     *
     * The tolerance and the failure wording are those of the Java original, so a failure names the
     * index that differs rather than only the two runs.
     *
     * @param actual  the values produced
     * @param expected  the values required
     */
    private def assertArray(actual: Array[Double], expected: Seq[Double]): Unit = {
      actual.length shouldBe expected.length
      assertElementsFrom(actual, expected, 0)
    }

    /**
     * Asserts that an operation over two arrays of different lengths reports the caller.
     *
     * The element-wise operations require their operands to agree on length, and a pair that does
     * not is a broken caller rather than a data-dependent outcome, so it is raised rather than
     * handed back. The exception type and the message are those of the Java original, and one
     * helper keeps every call site of this spec holding the operation to both.
     *
     * @param operation  the operation to evaluate, which must fail
     * @return the assertion that it failed with the expected type and message
     */
    private def assertDifferentSizes(operation: => Any): Assertion = {
      val failure = intercept[IllegalArgumentException](operation)
      failure.getMessage shouldBe "Arrays have different sizes"
    }

    // compares the elements from the index upwards, in index order, one clue per element
    @tailrec
    private def assertElementsFrom(actual: Array[Double], expected: Seq[Double], index: Int): Unit =
      if (index < actual.length) {
        withClue(s"Unexpected value at index $index, ") {
          actual(index) shouldBe expected(index) +- DELTA
        }
        assertElementsFrom(actual, expected, index + 1)
      }

    //-------------------------------------------------------------------------
    test("test_EMPTY") {
      assertContent(DoubleArray.EMPTY)
    }

    test("test_of") {
      // the original declared ten arity-specific factories, which exist to spare a caller an
      // array allocation; the port has one varargs factory, so each arity is exercised through it
      assertContent(DoubleArray.of())
      assertContent(DoubleArray.of(1.0), 1.0)
      assertContent(DoubleArray.of(1.0, 2.0), 1.0, 2.0)
      assertContent(DoubleArray.of(1.0, 2.0, 3.0), 1.0, 2.0, 3.0)
      assertContent(DoubleArray.of(1.0, 2.0, 3.0, 4.0), 1.0, 2.0, 3.0, 4.0)
      assertContent(DoubleArray.of(1.0, 2.0, 3.0, 4.0, 5.0), 1.0, 2.0, 3.0, 4.0, 5.0)
      assertContent(DoubleArray.of(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
      assertContent(
        DoubleArray.of(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0),
        1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
      assertContent(
        DoubleArray.of(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0),
        1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
      assertContent(
        DoubleArray.of(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0),
        1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)
    }

    test("test_of_lambda") {
      // the factory of the original that took a size and a function is `tabulate` here, whose two
      // parameter lists let the function be written as a block at the call site
      assertContent(DoubleArray.tabulate(0)(_ => fail("the function must not be invoked")))

      // a counter proves both that the function is called once per element and that the calls
      // happen in index order: the values land in the array in the order they were produced
      var counter: Int = 2
      def nextValue(): Double = {
        val value = counter.toDouble
        counter = counter + 1
        value
      }
      assertContent(DoubleArray.tabulate(1)(_ => nextValue()), 2.0)
      assertContent(DoubleArray.tabulate(2)(_ => nextValue()), 3.0, 4.0)
      counter shouldBe 5
    }

    test("test_of_stream") {
      // the original built an array from a primitive stream. The port has no such factory - a
      // stream is a one-shot pipeline of the platform, and the sequence types of the language
      // cover the same ground - so the equivalent construction path is asserted instead: from the
      // lazy sequence that stands in for a stream here, and from a strict one, both empty and not
      assertContent(DoubleArray.copyOf(LazyList.empty[Double]))
      assertContent(DoubleArray.copyOf(LazyList(1.0, 2.0, 3.0)), 1.0, 2.0, 3.0)
      assertContent(DoubleArray.copyOf(List.empty[Double]))
      assertContent(DoubleArray.copyOf(Vector(1.0, 2.0, 3.0)), 1.0, 2.0, 3.0)
      assertContent(DoubleArray.of(List(1.0, 2.0, 3.0): _*), 1.0, 2.0, 3.0)
      assertContent(DoubleArray.copyOf(Iterator(1.0, 2.0, 3.0).toList), 1.0, 2.0, 3.0)
    }

    test("test_ofUnsafe") {
      // `ofUnsafe` adopts the array it is given instead of copying it, which is why it is visible
      // only inside this module - a caller outside it cannot reach it at all, as
      // `unsafe_members_are_inaccessible_outside_collect` proves. Adopting is exactly what is
      // asserted here: the value observes a later change to the array it was built from, which is
      // why nothing outside this module is allowed to build one this way
      val base = Array(1.0, 2.0, 3.0)
      val test = DoubleArray.ofUnsafe(base)
      assertContent(test, 1.0, 2.0, 3.0)
      base(0) = 4.0
      assertContent(test, 4.0, 2.0, 3.0)
      assertContent(DoubleArray.ofUnsafe(DoubleArrayMath.EMPTY_DOUBLE_ARRAY))
    }

    test("test_copyOf_List") {
      // the immutable-list fixture of the original becomes the immutable list of the standard
      // library, which is the type the collection factory of the port accepts
      assertContent(DoubleArray.copyOf(List(1.0, 2.0, 3.0)), 1.0, 2.0, 3.0)
      assertContent(DoubleArray.copyOf(List.empty[Double]))
    }

    test("test_copyOf_array") {
      val base = Array(1.0, 2.0, 3.0)
      val test = DoubleArray.copyOf(base)
      assertContent(test, 1.0, 2.0, 3.0)
      base(0) = 4.0
      // the value copied the array it was given, so it does not observe the change
      assertContent(test, 1.0, 2.0, 3.0)
      assertContent(DoubleArray.copyOf(DoubleArrayMath.EMPTY_DOUBLE_ARRAY))
    }

    test("test_copyOf_array_fromIndex") {
      assertContent(DoubleArray.copyOf(Array(1.0, 2.0, 3.0), 0), 1.0, 2.0, 3.0)
      assertContent(DoubleArray.copyOf(Array(1.0, 2.0, 3.0), 1), 2.0, 3.0)
      assertContent(DoubleArray.copyOf(Array(1.0, 2.0, 3.0), 3))
      // a negative start index reaches the range copy of the platform, which raises the index
      // exception for it, exactly as in the original
      assertThrows[IndexOutOfBoundsException](DoubleArray.copyOf(Array(1.0, 2.0, 3.0), -1))
      // a start index beyond the end is checked before anything is copied, and fails with an
      // illegal argument where the original raised an index exception; the message is unchanged
      val failure = intercept[IllegalArgumentException](DoubleArray.copyOf(Array(1.0, 2.0, 3.0), 4))
      failure.getMessage shouldBe "Array index out of bounds: 4 > 3"
    }

    test("test_copyOf_array_fromToIndex") {
      assertContent(DoubleArray.copyOf(Array(1.0, 2.0, 3.0), 0, 3), 1.0, 2.0, 3.0)
      assertContent(DoubleArray.copyOf(Array(1.0, 2.0, 3.0), 1, 2), 2.0)
      assertContent(DoubleArray.copyOf(Array(1.0, 2.0, 3.0), 1, 1))
      assertThrows[IndexOutOfBoundsException](DoubleArray.copyOf(Array(1.0, 2.0, 3.0), -1, 3))
      val failure =
        intercept[IllegalArgumentException](DoubleArray.copyOf(Array(1.0, 2.0, 3.0), 0, 5))
      failure.getMessage shouldBe "Array index out of bounds: 5 > 3"
    }

    test("test_filled") {
      assertContent(DoubleArray.filled(0))
      assertContent(DoubleArray.filled(3), 0.0, 0.0, 0.0)
    }

    test("test_filled_withValue") {
      assertContent(DoubleArray.filled(0, 1.5))
      assertContent(DoubleArray.filled(3, 1.5), 1.5, 1.5, 1.5)
    }

    test("negative_size_of_filled_and_tabulate") {
      // The three factories that are told how many elements to produce document a negative size
      // as a failure, and each fails it at the allocation of the storage rather than by checking
      // the argument first: an array of negative length is not a value the platform can make, so
      // the runtime raises the size exception and names the size it was asked for. This is a
      // distinct path from the bounds failures of the copying factories above, which are checked
      // against the length of an input array and reported as illegal arguments
      val zeroes = intercept[NegativeArraySizeException](DoubleArray.filled(-1))
      zeroes.getMessage shouldBe "-1"
      val valued = intercept[NegativeArraySizeException](DoubleArray.filled(-1, 1.5))
      valued.getMessage shouldBe "-1"

      // the failure precedes any call of the value function, which is why the function here fails
      // the test if it is invoked at all - the same shape `test_of_lambda` uses for a size of zero
      val tabulated = intercept[NegativeArraySizeException](
        DoubleArray.tabulate(-1)(_ => fail("the function must not be invoked")))
      tabulated.getMessage shouldBe "-1"
    }

    test("reversed_range_of_copyOf_and_subArray") {
      // A range whose start is after its end is a fourth failure path, separate from a negative
      // start index and from either bound running past the end of the array: both indices here
      // are inside the array, so the two bounds checks of the range factory pass and the failure
      // comes from the range copy of the platform, which reports the reversed pair. The exception
      // type and the message are those of the Java original, which reached the same range copy
      val base = Array(1.0, 2.0, 3.0)
      val reversed = intercept[IllegalArgumentException](DoubleArray.copyOf(base, 2, 1))
      reversed.getMessage shouldBe "2 > 1"

      // a negative end index is a reversed range too, and is reported the same way. It is not
      // caught by the bounds checks, which only ask whether an index is beyond the end
      val negativeEnd = intercept[IllegalArgumentException](DoubleArray.copyOf(base, 0, -1))
      negativeEnd.getMessage shouldBe "0 > -1"

      // a start index at the end of the array is legal on its own - `copyOf(base, 3, 3)` is the
      // empty array - so it is the reversal that fails this one
      val fromTheEnd = intercept[IllegalArgumentException](DoubleArray.copyOf(base, 3, 1))
      fromTheEnd.getMessage shouldBe "3 > 1"
      assertContent(DoubleArray.copyOf(base, 3, 3))

      // the sub-array member delegates to that factory, so it fails the same two ways
      val test = DoubleArray.of(1.0, 2.0, 3.0)
      val subReversed = intercept[IllegalArgumentException](test.subArray(2, 1))
      subReversed.getMessage shouldBe "2 > 1"
      val subNegativeEnd = intercept[IllegalArgumentException](test.subArray(0, -1))
      subNegativeEnd.getMessage shouldBe "0 > -1"
    }

    //-------------------------------------------------------------------------
    test("test_get") {
      val test = DoubleArray.of(1.0, 2.0, 3.0, 3.0, 4.0)
      test.get(0) shouldBe 1.0
      test.get(4) shouldBe 4.0
      // the index reaches the stored array directly, so both ends fail with the index exception
      // the runtime raises, as the member documents and as the original did
      forAll(Table("index", -1, 5)) { (index: Int) =>
        assertThrows[IndexOutOfBoundsException](test.get(index))
      }
    }

    test("test_contains") {
      val test = DoubleArray.of(1.0, 2.0, 3.0, 3.0, 4.0)
      test.contains(1.0) shouldBe true
      test.contains(3.0) shouldBe true
      test.contains(5.0) shouldBe false
      DoubleArray.EMPTY.contains(5.0) shouldBe false
    }

    test("test_indexOf") {
      val test = DoubleArray.of(1.0, 2.0, 3.0, 3.0, 4.0)
      test.indexOf(2.0) shouldBe 1
      test.indexOf(3.0) shouldBe 2
      test.indexOf(5.0) shouldBe -1
      DoubleArray.EMPTY.indexOf(5.0) shouldBe -1
    }

    test("test_lastIndexOf") {
      val test = DoubleArray.of(1.0, 2.0, 3.0, 3.0, 4.0)
      test.lastIndexOf(2.0) shouldBe 1
      test.lastIndexOf(3.0) shouldBe 3
      test.lastIndexOf(5.0) shouldBe -1
      DoubleArray.EMPTY.lastIndexOf(5.0) shouldBe -1
    }

    //-------------------------------------------------------------------------
    test("test_copyInto") {
      // the original wrote the elements into an array the caller supplied, at an offset. The port
      // has no such member: `toArray` answers with a fresh array of exactly the right length,
      // which a caller may then place wherever it likes. What the original's member had to promise
      // - that the elements arrive intact and that the value is unaffected by what happens to them
      // afterwards - is asserted here of that copy
      val test = DoubleArray.of(1.0, 2.0, 3.0)
      val first = test.toArray
      Arrays.equals(first, Array(1.0, 2.0, 3.0)) shouldBe true

      val second = test.toArray
      (first eq second) shouldBe false
      (first eq test.toArrayUnsafe) shouldBe false

      first(0) = 9.0
      assertContent(test, 1.0, 2.0, 3.0)
      Arrays.equals(second, Array(1.0, 2.0, 3.0)) shouldBe true
    }

    //-------------------------------------------------------------------------
    test("test_subArray_from") {
      val test = DoubleArray.of(1.0, 2.0, 3.0)
      assertContent(test.subArray(0), 1.0, 2.0, 3.0)
      assertContent(test.subArray(1), 2.0, 3.0)
      assertContent(test.subArray(2), 3.0)
      assertContent(test.subArray(3))
      // beyond the end is checked, and reported as an illegal argument where the original raised
      // an index exception; negative still reaches the range copy and its index exception
      val failure = intercept[IllegalArgumentException](test.subArray(4))
      failure.getMessage shouldBe "Array index out of bounds: 4 > 3"
      assertThrows[IndexOutOfBoundsException](test.subArray(-1))
    }

    test("test_subArray_fromTo") {
      val test = DoubleArray.of(1.0, 2.0, 3.0)
      assertContent(test.subArray(0, 3), 1.0, 2.0, 3.0)
      assertContent(test.subArray(1, 3), 2.0, 3.0)
      assertContent(test.subArray(2, 3), 3.0)
      assertContent(test.subArray(3, 3))
      assertContent(test.subArray(1, 2), 2.0)
      val failure = intercept[IllegalArgumentException](test.subArray(0, 4))
      failure.getMessage shouldBe "Array index out of bounds: 4 > 3"
      assertThrows[IndexOutOfBoundsException](test.subArray(-1, 3))
    }

    //-------------------------------------------------------------------------
    test("test_toList") {
      val test = DoubleArray.of(1.0, 2.0, 3.0)
      val list = test.toList
      assertContent(DoubleArray.copyOf(list), 1.0, 2.0, 3.0)
      list.size shouldBe 3
      list.isEmpty shouldBe false
      list(0) shouldBe 1.0
      list(2) shouldBe 3.0
      list.contains(2.0) shouldBe true
      list.contains(5.0) shouldBe false
      list.indexOf(2.0) shouldBe 1
      list.indexOf(5.0) shouldBe -1
      list.lastIndexOf(3.0) shouldBe 2
      list.lastIndexOf(5.0) shouldBe -1

      // the original also looked a value of an unrelated type up in the list. The lookups of the
      // standard library are generic in a supertype of the element, so those calls compile here
      // and the honest assertion is the one the original made: nothing of another type is found.
      // The type argument is written out rather than inferred, so that widening to the common
      // supertype is a decision of this spec and not something the compiler arrived at quietly
      list.contains[Any](anotherType) shouldBe false
      list.indexOf[Any](anotherType) shouldBe -1
      list.lastIndexOf[Any](anotherType) shouldBe -1

      // the original asserted that the mutators of its list view were unsupported at run time.
      // The list here is an immutable list of the standard library, which declares no mutator at
      // all, so the same claim is settled at compile time. The first assertion is the positive
      // control: the copying update does exist, so the two below fail because the member is absent
      // and not because the expression around it is malformed
      assertCompiles(
        "com.opengamma.strata.collect.array.DoubleArray.of(1.0).toList.updated(0, 3.0)")
      assertDoesNotCompile(
        "com.opengamma.strata.collect.array.DoubleArray.of(1.0).toList.clear()")
      assertDoesNotCompile(
        "com.opengamma.strata.collect.array.DoubleArray.of(1.0).toList.update(0, 3.0)")
    }

    test("test_toList_iterator") {
      val test = DoubleArray.of(1.0, 2.0, 3.0)
      val it = test.toList.iterator
      it.hasNext shouldBe true
      it.next() shouldBe 1.0
      it.hasNext shouldBe true
      it.next() shouldBe 2.0
      it.hasNext shouldBe true
      it.next() shouldBe 3.0
      it.hasNext shouldBe false
      assertThrows[NoSuchElementException](it.next())

      // the iterator of the original could be asked to remove the element it had just returned,
      // and refused at run time; this one has no such method to ask
      assertDoesNotCompile(
        "com.opengamma.strata.collect.array.DoubleArray.of(1.0).toList.iterator.remove()")

      // the array offers an iterator directly as well, over the stored elements
      val direct = test.iterator
      direct.next() shouldBe 1.0
      direct.toList shouldBe List(2.0, 3.0)
    }

    test("test_toList_listIterator") {
      // the original walked its list with a bidirectional cursor that could also mutate: it
      // asserted the two index positions at each step, the exhaustion at both ends, and that each
      // of the three mutating operations was unsupported. An immutable list has no such cursor -
      // there is nothing for it to mutate - so the traversal in both directions is asserted
      // through the two iterators it does offer, the positions through the index of each element,
      // and the absence of the mutators at compile time
      val list = DoubleArray.of(1.0, 2.0, 3.0).toList
      list.iterator.toList shouldBe List(1.0, 2.0, 3.0)
      list.reverseIterator.toList shouldBe List(3.0, 2.0, 1.0)
      list.zipWithIndex shouldBe List((1.0, 0), (2.0, 1), (3.0, 2))
      list.indices.toList shouldBe List(0, 1, 2)

      val forwards = list.iterator
      forwards.toList shouldBe List(1.0, 2.0, 3.0)
      assertThrows[NoSuchElementException](forwards.next())

      val backwards = list.reverseIterator
      backwards.toList shouldBe List(3.0, 2.0, 1.0)
      assertThrows[NoSuchElementException](backwards.next())

      assertCompiles("com.opengamma.strata.collect.array.DoubleArray.of(1.0).toList.iterator")
      assertDoesNotCompile(
        "com.opengamma.strata.collect.array.DoubleArray.of(1.0).toList.listIterator")
      assertDoesNotCompile(
        "com.opengamma.strata.collect.array.DoubleArray.of(1.0).toList.set(0, 3.0)")
      assertDoesNotCompile(
        "com.opengamma.strata.collect.array.DoubleArray.of(1.0).toList.add(3.0)")
    }

    //-------------------------------------------------------------------------
    test("test_stream") {
      // the original exposed a primitive stream of the elements and collected it back to an
      // array. The port exposes an iterator, which is the sequential traversal the language uses
      // for the same purpose, and the same round trip is asserted through it
      val test = DoubleArray.of(1.0, 2.0, 3.0)
      Arrays.equals(test.iterator.toArray, Array(1.0, 2.0, 3.0)) shouldBe true
      test.iterator.toList shouldBe List(1.0, 2.0, 3.0)
      test.iterator.sum shouldBe 6.0
      DoubleArray.EMPTY.iterator.hasNext shouldBe false
    }

    //-------------------------------------------------------------------------
    test("test_forEach") {
      val test = DoubleArray.of(1.0, 2.0, 3.0)
      val extracted = new Array[Double](3)
      test.forEach((index, value) => extracted(index) = value)
      Arrays.equals(extracted, Array(1.0, 2.0, 3.0)) shouldBe true

      // the action is applied once per element, in index order, and not at all to an empty array
      DoubleArray.EMPTY.forEach((_, _) => fail("the action must not be invoked"))
      succeed
    }

    //-------------------------------------------------------------------------
    test("test_with") {
      // the member keeps the name of the original, which is a reserved word here and so is
      // written in backquotes at every call
      val test = DoubleArray.of(1.0, 2.0, 3.0)
      assertContent(test.`with`(0, 2.6), 2.6, 2.0, 3.0)
      assertContent(test.`with`(0, 1.0), 1.0, 2.0, 3.0)
      // storing the value already there answers with this instance rather than with a copy
      test.`with`(0, 1.0) should be theSameInstanceAs test
      forAll(Table("index", -1, 3)) { (index: Int) =>
        assertThrows[IndexOutOfBoundsException](test.`with`(index, 2.0))
      }
    }

    //-------------------------------------------------------------------------
    test("test_plus") {
      val test = DoubleArray.of(1.0, 2.0, 3.0)
      assertContent(test.plus(5.0), 6.0, 7.0, 8.0)
      assertContent(test.plus(0.0), 1.0, 2.0, 3.0)
      assertContent(test.plus(-5.0), -4.0, -3.0, -2.0)
      // adding zero answers with this instance, so it is not a way to obtain a distinct value
      test.plus(0.0) should be theSameInstanceAs test
    }

    test("test_minus") {
      val test = DoubleArray.of(1.0, 2.0, 3.0)
      assertContent(test.minus(5.0), -4.0, -3.0, -2.0)
      assertContent(test.minus(0.0), 1.0, 2.0, 3.0)
      assertContent(test.minus(-5.0), 6.0, 7.0, 8.0)
      test.minus(0.0) should be theSameInstanceAs test
    }

    test("test_multipliedBy") {
      val test = DoubleArray.of(1.0, 2.0, 3.0)
      assertContent(test.multipliedBy(5.0), 5.0, 10.0, 15.0)
      assertContent(test.multipliedBy(1.0), 1.0, 2.0, 3.0)
      test.multipliedBy(1.0) should be theSameInstanceAs test
    }

    test("test_dividedBy") {
      val test = DoubleArray.of(10.0, 20.0, 30.0)
      assertContent(test.dividedBy(5.0), 2.0, 4.0, 6.0)
      assertContent(test.dividedBy(1.0), 10.0, 20.0, 30.0)
      test.dividedBy(1.0) should be theSameInstanceAs test
    }

    test("test_map") {
      val test = DoubleArray.of(1.0, 2.0, 3.0)
      // the expectations are the same expressions the original used, rather than decimal
      // approximations of them, because two of the three have no exact decimal form
      assertContent(test.map(value => 1.0 / value), 1.0, 1.0 / 2.0, 1.0 / 3.0)
    }

    test("test_mapWithIndex") {
      val test = DoubleArray.of(1.0, 2.0, 3.0)
      // the index is converted explicitly: the build rejects an implicit widening of a whole
      // number to a double, and the conversion is what the original was doing silently
      assertContent(test.mapWithIndex((index, value) => index.toDouble * value), 0.0, 2.0, 6.0)
    }

    //-------------------------------------------------------------------------
    test("test_plus_array") {
      val test1 = DoubleArray.of(1.0, 2.0, 3.0)
      val test2 = DoubleArray.of(0.5, 0.6, 0.7)
      assertContent(test1.plus(test2), 1.5, 2.6, 3.7)
      assertDifferentSizes(test1.plus(DoubleArray.EMPTY))
    }

    test("test_minus_array") {
      val test1 = DoubleArray.of(1.0, 2.0, 3.0)
      val test2 = DoubleArray.of(0.5, 0.6, 0.7)
      assertContent(test1.minus(test2), 0.5, 1.4, 2.3)
      assertDifferentSizes(test1.minus(DoubleArray.EMPTY))
    }

    test("test_multipliedBy_array") {
      val test1 = DoubleArray.of(1.0, 2.0, 3.0)
      val test2 = DoubleArray.of(0.5, 0.6, 0.7)
      assertContent(test1.multipliedBy(test2), 0.5, 1.2, 2.1)
      assertDifferentSizes(test1.multipliedBy(DoubleArray.EMPTY))
    }

    test("test_dividedBy_array") {
      val test1 = DoubleArray.of(10.0, 20.0, 30.0)
      val test2 = DoubleArray.of(2.0, 5.0, 10.0)
      assertContent(test1.dividedBy(test2), 5.0, 4.0, 3.0)
      assertDifferentSizes(test1.dividedBy(DoubleArray.EMPTY))
    }

    test("test_combine") {
      val test1 = DoubleArray.of(1.0, 2.0, 3.0)
      val test2 = DoubleArray.of(0.5, 0.6, 0.7)
      assertContent(test1.combine(test2, (a, b) => a * b), 0.5, 2.0 * 0.6, 3.0 * 0.7)
      assertDifferentSizes(test1.combine(DoubleArray.EMPTY, (a, b) => a * b))
    }

    test("test_combineReduce") {
      val test1 = DoubleArray.of(1.0, 2.0, 3.0)
      val test2 = DoubleArray.of(0.5, 0.6, 0.7)
      test1.combineReduce(test2, (total, a, b) => total + a * b) shouldBe
        0.5 + 2.0 * 0.6 + 3.0 * 0.7
      assertDifferentSizes(test1.combineReduce(DoubleArray.EMPTY, (total, a, b) => total + a * b))
    }

    //-------------------------------------------------------------------------
    test("test_sorted") {
      assertContent(DoubleArray.of().sorted)
      assertContent(DoubleArray.of(2.0).sorted, 2.0)
      assertContent(DoubleArray.of(2.0, 1.0, 3.0, 0.0).sorted, 0.0, 1.0, 2.0, 3.0)
      // fewer than two elements are already sorted and answer with this instance
      val single = DoubleArray.of(2.0)
      single.sorted should be theSameInstanceAs single
    }

    //-------------------------------------------------------------------------
    test("test_min") {
      DoubleArray.of(2.0).min shouldBe 2.0
      DoubleArray.of(2.0, 1.0, 3.0).min shouldBe 1.0
      // an empty array has no minimum. The original reported that as an illegal state; every
      // failure of this port is reported through its one fail-fast checker, which raises an
      // illegal argument, and the message of the original is kept
      val failure = intercept[IllegalArgumentException](DoubleArray.EMPTY.min)
      failure.getMessage shouldBe "Unable to find minimum of an empty array"
    }

    test("test_max") {
      DoubleArray.of(2.0).max shouldBe 2.0
      DoubleArray.of(2.0, 1.0, 3.0).max shouldBe 3.0
      val failure = intercept[IllegalArgumentException](DoubleArray.EMPTY.max)
      failure.getMessage shouldBe "Unable to find maximum of an empty array"
    }

    test("test_sum") {
      DoubleArray.EMPTY.sum shouldBe 0.0
      DoubleArray.of(2.0).sum shouldBe 2.0
      DoubleArray.of(2.0, 1.0, 3.0).sum shouldBe 6.0
    }

    test("test_reduce") {
      // an empty array reduces to the identity without the operator being called at all
      DoubleArray.EMPTY.reduce(2.0, (_, _) => fail("the operator must not be invoked")) shouldBe 2.0
      DoubleArray.of(2.0).reduce(1.0, (total, value) => total * value) shouldBe 2.0
      DoubleArray.of(2.0, 1.0, 3.0).reduce(1.0, (total, value) => total * value) shouldBe 6.0
    }

    //-------------------------------------------------------------------------
    test("test_concat_varargs") {
      val test1 = DoubleArray.of(1.0, 2.0, 3.0)
      assertContent(test1.concat(0.5, 0.6, 0.7), 1.0, 2.0, 3.0, 0.5, 0.6, 0.7)
      // the original had an overload taking an array; the port takes a sequence of values, and an
      // array is offered to it as a sequence. Handing the array itself to the varargs member is
      // rejected by the build, which is why the conversion is written out here
      assertContent(
        test1.concat(Array(0.5, 0.6, 0.7).toIndexedSeq: _*),
        1.0, 2.0, 3.0, 0.5, 0.6, 0.7)
      assertContent(
        test1.concat(DoubleArrayMath.EMPTY_DOUBLE_ARRAY.toIndexedSeq: _*),
        1.0, 2.0, 3.0)
      assertContent(DoubleArray.EMPTY.concat(Array(1.0, 2.0, 3.0).toIndexedSeq: _*), 1.0, 2.0, 3.0)
      // concatenating nothing answers with this instance
      test1.concat() should be theSameInstanceAs test1
    }

    test("test_concat_object") {
      val test1 = DoubleArray.of(1.0, 2.0, 3.0)
      val test2 = DoubleArray.of(0.5, 0.6, 0.7)
      assertContent(test1.concat(test2), 1.0, 2.0, 3.0, 0.5, 0.6, 0.7)
      assertContent(test2.concat(test1), 0.5, 0.6, 0.7, 1.0, 2.0, 3.0)
      assertContent(test1.concat(DoubleArray.EMPTY), 1.0, 2.0, 3.0)
      assertContent(DoubleArray.EMPTY.concat(test1), 1.0, 2.0, 3.0)
      // both empty cases answer with an existing instance, since both are immutable
      test1.concat(DoubleArray.EMPTY) should be theSameInstanceAs test1
      DoubleArray.EMPTY.concat(test1) should be theSameInstanceAs test1
    }

    //-------------------------------------------------------------------------
    test("test_equalWithTolerance") {
      val a1 = DoubleArray.of(1.0, 2.0)
      val a2 = DoubleArray.of(1.0, 2.02)
      val a3 = DoubleArray.of(1.0, 2.009)
      val b = DoubleArray.of(1.0, 2.0, 3.0)
      a1.equalWithTolerance(a2, 0.01) shouldBe false
      a1.equalWithTolerance(a3, 0.01) shouldBe true
      a1.equalWithTolerance(b, 0.01) shouldBe false
    }

    test("test_equalZeroWithTolerance") {
      val a1 = DoubleArray.of(0.0, 0.0)
      val a2 = DoubleArray.of(0.0, 0.02)
      val a3 = DoubleArray.of(0.0, 0.009)
      val b = DoubleArray.of(1.0, 2.0, 3.0)
      a1.equalZeroWithTolerance(0.01) shouldBe true
      a2.equalZeroWithTolerance(0.01) shouldBe false
      a3.equalZeroWithTolerance(0.01) shouldBe true
      b.equalZeroWithTolerance(0.01) shouldBe false
      // an empty array holds no value that differs from zero
      DoubleArray.EMPTY.equalZeroWithTolerance(0.0) shouldBe true
    }

    //-------------------------------------------------------------------------
    test("test_equalsHashCode") {
      val a1 = DoubleArray.of(1.0, 2.0)
      val a2 = DoubleArray.of(1.0, 2.0)
      val b = DoubleArray.of(1.0, 2.0, 3.0)
      a1.equals(a1) shouldBe true
      a1.equals(a2) shouldBe true
      a1.equals(b) shouldBe false
      a1.equals(anotherType) shouldBe false
      a1.equals(nullRef) shouldBe false
      a1.hashCode shouldBe a2.hashCode
      // hashing is that of the elements, bit for bit, as the rendering oracle below also is
      a1.hashCode shouldBe Arrays.hashCode(Array(1.0, 2.0))
    }

    test("test_toString") {
      val test = DoubleArray.of(1.0, 2.0)
      test.toString shouldBe "[1.0, 2.0]"
      DoubleArray.EMPTY.toString shouldBe "[]"
    }

    //-------------------------------------------------------------------------
    test("coverage") {
      // The original called a reflective sweep over the properties of a bean and then reached
      // through a meta-bean to a meta-property by name. This port has no bean, no meta-bean and no
      // reflective property access - doing without them is one of its requirements - so none of
      // those calls has a target. Their substance does: they stood for the claims that two
      // instances built from equal parts are equal, that instances differing anywhere are not,
      // that an instance renders itself faithfully, and that the type's declared surface is what
      // it claims to be. Each is asserted here directly, on the type's own members and on its
      // typeclass instances, which names the expected outcome of each case instead of merely
      // visiting the fields. Every assertion of an absence is preceded by a positive control in
      // the same shape, so that none of them can pass because an expression was malformed.
      val test = DoubleArray.of(1.0, 2.0, 3.0)
      val equal = DoubleArray.of(1.0, 2.0, 3.0)
      val different = DoubleArray.of(1.0, 2.0, 4.0)
      val longer = DoubleArray.of(1.0, 2.0, 3.0, 4.0)

      val hash = implicitly[Hash[DoubleArray]]
      hash.eqv(test, equal) shouldBe true
      hash.eqv(test, different) shouldBe false
      hash.eqv(test, longer) shouldBe false
      hash.hash(test) shouldBe test.hashCode
      hash.hash(equal) shouldBe hash.hash(test)

      val show = implicitly[Show[DoubleArray]]
      show.show(test) shouldBe "[1.0, 2.0, 3.0]"
      show.show(test) shouldBe test.toString
      show.show(DoubleArray.EMPTY) shouldBe "[]"

      // the abstraction the type declares itself under: an array is a one-dimensional matrix, and
      // the two members that abstraction asks for answer for every value, the empty one included
      test shouldBe a[Matrix]
      (test: Matrix).dimensions shouldBe 1
      (test: Matrix).size shouldBe 3
      (DoubleArray.EMPTY: Matrix).size shouldBe 0

      // Exactly one equality-bearing instance is declared for this type: `Hash` extends `Eq`, so
      // summoning an `Eq` finds that very instance rather than a second one that could disagree.
      // That it compiles is itself half of the claim - two candidate instances would be ambiguous
      // and would not - and that it is the same instance is the other half.
      assertCompiles("implicitly[cats.Eq[com.opengamma.strata.collect.array.DoubleArray]]")
      val equality = implicitly[Eq[DoubleArray]]
      sameInstance(equality, hash) shouldBe true
      equality.eqv(test, equal) shouldBe true
      equality.eqv(test, different) shouldBe false

      // no ordering is declared: arrays are compared for equality only
      assertCompiles("implicitly[cats.Order[com.opengamma.strata.collect.Decimal]]")
      assertDoesNotCompile("implicitly[cats.Order[com.opengamma.strata.collect.array.DoubleArray]]")

      // no meta-bean and no meta-property, by name or otherwise
      assertCompiles("com.opengamma.strata.collect.array.DoubleArray.of(1.0).size")
      assertDoesNotCompile("com.opengamma.strata.collect.array.DoubleArray.of(1.0).metaBean")
      assertDoesNotCompile(
        """com.opengamma.strata.collect.array.DoubleArray.of(1.0).metaBean.metaProperty("array")""")

      // the type publishes no JSON codec of its own. The codec object of this module owns it, as
      // the round trip below shows, and a derivation site reaches it through an import rather than
      // finding it in the implicit scope of the type - which the first of the three assertions
      // proves is a choice, since a type of this module that does publish one is found that way
      assertCompiles("implicitly[_root_.io.circe.Encoder[com.opengamma.strata.collect.Decimal]]")
      assertDoesNotCompile(
        "implicitly[_root_.io.circe.Encoder[com.opengamma.strata.collect.array.DoubleArray]]")
      assertDoesNotCompile(
        "implicitly[_root_.io.circe.Decoder[com.opengamma.strata.collect.array.DoubleArray]]")
      Codecs.doubleArrayCodec.decodeJson(Codecs.doubleArrayCodec(test)) shouldBe Right(test)
    }

    //-------------------------------------------------------------------------
    test("copy_safety_of_copyOf_array") {
      // the array factory copies what it is handed, so the caller may go on using it
      val base = Array(1.0, 2.0, 3.0)
      val whole = DoubleArray.copyOf(base)
      val part = DoubleArray.copyOf(base, 1)
      val range = DoubleArray.copyOf(base, 0, 2)
      base(0) = 9.0
      base(1) = 8.0
      base(2) = 7.0
      assertContent(whole, 1.0, 2.0, 3.0)
      assertContent(part, 2.0, 3.0)
      assertContent(range, 1.0, 2.0)
    }

    test("copy_safety_of_toArray") {
      // the copying accessor answers with a fresh array on every call, and nothing done to one of
      // those arrays reaches the value it came from
      val test = DoubleArray.of(1.0, 2.0, 3.0)
      val first = test.toArray
      val second = test.toArray
      (first eq second) shouldBe false
      first(0) = 9.0
      second(2) = 7.0
      assertContent(test, 1.0, 2.0, 3.0)
      Arrays.equals(test.toArray, Array(1.0, 2.0, 3.0)) shouldBe true
    }

    test("copy_safety_of_of_varargs") {
      // A sequence expanded into a varargs call can share its storage with the caller, and the
      // wrapper used here does exactly that: it is the sharpest input the factory can be given,
      // because the sequence it receives is a view of `base` rather than a copy of it. The value
      // built from it must still not observe a later change to `base`
      val base = Array(1.0, 2.0, 3.0)
      val test = DoubleArray.of(ArraySeq.unsafeWrapArray(base): _*)
      base(0) = 9.0
      assertContent(test, 1.0, 2.0, 3.0)

      // the same holds for the collection factory, which unboxes into an array of its own
      val elements = List(1.0, 2.0, 3.0)
      assertContent(DoubleArray.copyOf(elements), 1.0, 2.0, 3.0)
      elements shouldBe List(1.0, 2.0, 3.0)
    }

    test("copy_safety_of_concat_varargs") {
      // `concat` allocates its result at the final length and copies each source into it, which
      // is one copy per source rather than two. The values arriving from the sequence therefore
      // reach the result directly, and this is what proves that reaching them directly is still
      // copy-safe: the sequence expanded here is a view of `base` rather than a copy of it - the
      // sharpest input the member can be given - so if the result shared storage with the caller
      // it would be this call that showed it
      val test = DoubleArray.of(1.0, 2.0)
      val base = Array(0.5, 0.6)
      val joined = test.concat(ArraySeq.unsafeWrapArray(base): _*)
      assertContent(joined, 1.0, 2.0, 0.5, 0.6)
      (joined.toArrayUnsafe eq base) shouldBe false
      base(0) = 9.0
      assertContent(joined, 1.0, 2.0, 0.5, 0.6)

      // the copy answered by the result is fresh on every call, as it is for any other value of
      // this type, so nothing done to one of those arrays reaches the result
      val extracted = joined.toArray
      extracted(3) = 8.0
      assertContent(joined, 1.0, 2.0, 0.5, 0.6)

      // a sequence that is not backed by a primitive array takes the element-by-element path of
      // the copy rather than a bulk move, so it is exercised too, from a non-empty receiver
      assertContent(test.concat(List(0.5, 0.6, 0.7): _*), 1.0, 2.0, 0.5, 0.6, 0.7)
      assertContent(test.concat(Vector(0.5): _*), 1.0, 2.0, 0.5)
      assertContent(DoubleArray.EMPTY.concat(List(0.5, 0.6): _*), 0.5, 0.6)
    }

    test("copy_safety_of_tabulate_and_filled") {
      // the two factories that produce their own elements allocate the storage they wrap, so
      // nothing the caller holds can reach it
      val source = Array(1.0, 2.0, 3.0)
      val tabulated = DoubleArray.tabulate(3)(index => source(index))
      source(0) = 9.0
      assertContent(tabulated, 1.0, 2.0, 3.0)

      val filled = DoubleArray.filled(3, 1.5)
      val extracted = filled.toArray
      extracted(0) = 9.0
      assertContent(filled, 1.5, 1.5, 1.5)

      val zeroes = DoubleArray.filled(3)
      zeroes.toArray(1) = 9.0
      assertContent(zeroes, 0.0, 0.0, 0.0)
    }

    test("aliasing_of_the_unsafe_escape_hatches") {
      // These two members are the deliberate exceptions to the copying above: one adopts the array
      // it is given and the other hands back the array the value holds, so both alias. They are
      // visible only inside this module - `private[collect]`, not `private[array]`, because the
      // codec object of the module consumes them - which is why this spec can call them at all
      // and why no caller outside the module can. Their aliasing is asserted rather than merely
      // documented, because it is the reason the scope exists.
      val base = Array(1.0, 2.0, 3.0)
      val adopted = DoubleArray.ofUnsafe(base)
      (adopted.toArrayUnsafe eq base) shouldBe true
      base(1) = 9.0
      adopted.get(1) shouldBe 9.0

      val stored = adopted.toArrayUnsafe
      stored(2) = 8.0
      adopted.get(2) shouldBe 8.0
      Arrays.equals(adopted.toArray, Array(1.0, 9.0, 8.0)) shouldBe true

      // an empty array is answered by the canonical instance even here, so no fresh empty value
      // can be adopted into existence
      DoubleArray.ofUnsafe(new Array[Double](0)) should be theSameInstanceAs DoubleArray.EMPTY
    }

    test("unsafe_members_are_inaccessible_outside_collect") {
      // The two calls compile here, inside `com.opengamma.strata.collect`, which is the positive
      // control: the expressions are well formed and the members exist. The same two expressions
      // are then offered to a probe object in a sibling package outside `collect`, where the
      // compile-time access check denies them - which is the whole of the guarantee, since a
      // scope that is merely documented is a convention and one the compiler keeps is a rule.
      assertCompiles("com.opengamma.strata.collect.array.DoubleArray.ofUnsafe(Array(1.0))")
      assertCompiles("com.opengamma.strata.collect.array.DoubleArray.of(1.0).toArrayUnsafe")
      com.opengamma.strata.audit.DoubleArrayUnsafeAccessProbe.ofUnsafeIsInaccessible
      com.opengamma.strata.audit.DoubleArrayUnsafeAccessProbe.toArrayUnsafeIsInaccessible
    }

    //-------------------------------------------------------------------------
    test("ieee_equality_of_nan_and_signed_zero") {
      // equality compares elements bit for bit, so a not-a-number element equals itself and an
      // array holding one can be compared and hashed like any other
      val nan1 = DoubleArray.of(1.0, Double.NaN)
      val nan2 = DoubleArray.of(1.0, Double.NaN)
      nan1.equals(nan2) shouldBe true
      nan1.hashCode shouldBe nan2.hashCode
      arrayHash.eqv(nan1, nan2) shouldBe true
      arrayHash.hash(nan1) shouldBe nan1.hashCode
      nan1.contains(Double.NaN) shouldBe true
      nan1.indexOf(Double.NaN) shouldBe 1
      nan1.lastIndexOf(Double.NaN) shouldBe 1

      // the two zeroes have different bit patterns, so they are different elements
      val positiveZero = DoubleArray.of(0.0)
      val negativeZero = DoubleArray.of(-0.0)
      positiveZero.equals(negativeZero) shouldBe false
      positiveZero.hashCode should not be negativeZero.hashCode
      arrayHash.eqv(positiveZero, negativeZero) shouldBe false
      positiveZero.contains(0.0) shouldBe true
      positiveZero.contains(-0.0) shouldBe false
      negativeZero.indexOf(-0.0) shouldBe 0
      negativeZero.indexOf(0.0) shouldBe -1
      bitsOf(negativeZero.get(0)) shouldBe bitsOf(-0.0)
    }

    test("ieee_equality_of_the_infinities") {
      val positive = DoubleArray.of(Double.PositiveInfinity)
      val negative = DoubleArray.of(Double.NegativeInfinity)
      positive.equals(DoubleArray.of(Double.PositiveInfinity)) shouldBe true
      positive.hashCode shouldBe DoubleArray.of(Double.PositiveInfinity).hashCode
      positive.equals(negative) shouldBe false
      arrayHash.eqv(positive, DoubleArray.of(Double.PositiveInfinity)) shouldBe true
      arrayHash.eqv(positive, negative) shouldBe false
      positive.contains(Double.PositiveInfinity) shouldBe true
      positive.contains(Double.NegativeInfinity) shouldBe false
      positive.toString shouldBe "[Infinity]"
      negative.toString shouldBe "[-Infinity]"
    }

    test("ieee_tolerance_comparison_of_nan") {
      // The tolerance comparison is delegated, and it matches no not-a-number value at all: such
      // a value has no distance from anything, so no tolerance reaches it and an array holding
      // one is not equal to an array holding one at the same index. It is asserted here because
      // it is the opposite of the bitwise equality asserted above, where such an element is
      // reflexive; the fuzzy contract belongs to the comparison, whose own spec owns it.
      val nan = DoubleArray.of(1.0, Double.NaN)
      nan.equalWithTolerance(DoubleArray.of(1.0, Double.NaN), 0.0) shouldBe false
      nan.equalWithTolerance(DoubleArray.of(1.0, Double.NaN), Double.PositiveInfinity) shouldBe false
      nan.equalWithTolerance(nan, 0.01) shouldBe false
      nan.equalWithTolerance(DoubleArray.of(1.0, 2.0), 0.01) shouldBe false
      nan.equalZeroWithTolerance(0.01) shouldBe false
      nan.equalZeroWithTolerance(Double.PositiveInfinity) shouldBe false

      // the same array without that element is equal to itself within a tolerance, so it is the
      // element and not the delegation that refuses the comparison
      val finite = DoubleArray.of(1.0, 2.0)
      finite.equalWithTolerance(DoubleArray.of(1.0, 2.0), 0.0) shouldBe true

      // each infinity is equal to itself under any tolerance, an infinite one included, and to
      // nothing else - neither the other infinity nor any finite value, zero among them
      val positive = DoubleArray.of(Double.PositiveInfinity)
      positive.equalWithTolerance(DoubleArray.of(Double.PositiveInfinity), 0.0) shouldBe true
      positive.equalWithTolerance(DoubleArray.of(Double.PositiveInfinity), Double.PositiveInfinity) shouldBe true
      positive.equalWithTolerance(DoubleArray.of(Double.NegativeInfinity), 0.01) shouldBe false
      positive.equalWithTolerance(DoubleArray.of(Double.NegativeInfinity), Double.PositiveInfinity) shouldBe false
      positive.equalZeroWithTolerance(0.01) shouldBe false
      positive.equalZeroWithTolerance(Double.PositiveInfinity) shouldBe false

      // the tolerance itself is checked, as a caller-contract invariant: a negative tolerance and
      // a not-a-number tolerance are both caller errors rather than comparisons that answer false
      assertThrows[IllegalArgumentException](nan.equalWithTolerance(nan, -0.01))
      assertThrows[IllegalArgumentException](nan.equalZeroWithTolerance(-0.01))
      assertThrows[IllegalArgumentException](nan.equalWithTolerance(nan, Double.NaN))
      assertThrows[IllegalArgumentException](nan.equalZeroWithTolerance(Double.NaN))
    }

    test("ieee_sorted_orders_signed_zero_and_nan") {
      val test = DoubleArray.of(
        Double.NaN,
        1.0,
        0.0,
        -0.0,
        Double.NegativeInfinity,
        Double.PositiveInfinity,
        -1.0)

      // the primitive sort of the platform is the oracle: the port sorts by delegating to it, and
      // comparing through it is the only way to state the order of the two zeroes and of the
      // not-a-number element without restating the platform's own rule
      val expected = test.toArray
      Arrays.sort(expected)
      Arrays.equals(test.sorted.toArray, expected) shouldBe true

      // and that order, written out
      val sorted = test.sorted
      sorted.get(0) shouldBe Double.NegativeInfinity
      sorted.get(1) shouldBe -1.0
      bitsOf(sorted.get(2)) shouldBe bitsOf(-0.0)
      bitsOf(sorted.get(3)) shouldBe bitsOf(0.0)
      sorted.get(4) shouldBe 1.0
      sorted.get(5) shouldBe Double.PositiveInfinity
      sorted.get(6).isNaN shouldBe true
      // sorting copies, so the value sorted is unchanged
      bitsOf(test.get(0)) shouldBe bitsOf(Double.NaN)
    }

    test("ieee_min_and_max_with_nan") {
      // the minimum and the maximum of two values are those of the platform, which answer with a
      // not-a-number value whenever one of the operands is one
      val test = DoubleArray.of(1.0, Double.NaN, 3.0)
      test.min.isNaN shouldBe true
      test.max.isNaN shouldBe true
      DoubleArray.of(Double.NaN).min.isNaN shouldBe true
      DoubleArray.of(Double.NaN).max.isNaN shouldBe true

      // the infinities are ordinary extremes
      DoubleArray.of(1.0, Double.NegativeInfinity).min shouldBe Double.NegativeInfinity
      DoubleArray.of(1.0, Double.PositiveInfinity).max shouldBe Double.PositiveInfinity
      DoubleArray.of(1.0, Double.PositiveInfinity).min shouldBe 1.0
      DoubleArray.of(1.0, Double.NegativeInfinity).max shouldBe 1.0

      // the sum of an array holding a not-a-number element is one, since the addition is plain
      DoubleArray.of(1.0, Double.NaN).sum.isNaN shouldBe true
    }

    //-------------------------------------------------------------------------
    // The properties below are drawn from the generators of this module's shared generator
    // object, which is a cross-module contract: no generator of arrays is declared here, so a
    // property here and a property in the dependent module see the same values. The generator
    // that mixes in the IEEE-754 edge values is named wherever the property is about equality,
    // hashing, searching, ordering or rendering, all of which those values decide; the finite
    // generator is named wherever the property is about what arithmetic computes, and the name of
    // the test says so.
    //-------------------------------------------------------------------------
    test("property_copyOf_of_toArray_round_trips") {
      forAll(Arbitraries.genDoubleArray) { (a: DoubleArray) =>
        DoubleArray.copyOf(a.toArray) shouldBe a
        arrayHash.eqv(DoubleArray.copyOf(a.toArray), a) shouldBe true
        DoubleArray.copyOf(a.toArray).hashCode shouldBe a.hashCode
      }
    }

    test("property_toArray_is_fresh_and_independent") {
      forAll(Arbitraries.genNonEmptyDoubleArray) { (a: DoubleArray) =>
        val first = a.toArray
        val second = a.toArray
        (first eq second) shouldBe false
        (first eq a.toArrayUnsafe) shouldBe false
        first(0) = 123456.5
        Arrays.equals(a.toArray, second) shouldBe true
      }
    }

    test("property_size_isEmpty_and_dimensions_agree") {
      forAll(Arbitraries.genDoubleArray) { (a: DoubleArray) =>
        a.size shouldBe a.toArray.length
        a.isEmpty shouldBe (a.size == 0)
        a.dimensions shouldBe 1
        a.toList.size shouldBe a.size
        a.iterator.size shouldBe a.size
      }
    }

    test("property_with_changes_one_index_only") {
      forAll(Arbitraries.genNonEmptyDoubleArray, Gen.choose(0, 1000), Arbitraries.genDouble) {
        (a: DoubleArray, offset: Int, value: Double) =>
          val index = offset % a.size
          val updated = a.`with`(index, value)
          updated.size shouldBe a.size
          bitsOf(updated.get(index)) shouldBe bitsOf(value)
          val others = (0 until a.size).filter(_ != index)
          others.map(other => bitsOf(updated.get(other))) shouldBe
            others.map(other => bitsOf(a.get(other)))
      }
    }

    test("property_map_of_the_identity_returns_an_equal_array") {
      forAll(Arbitraries.genDoubleArray) { (a: DoubleArray) =>
        arrayHash.eqv(a.map(value => value), a) shouldBe true
        arrayHash.eqv(a.mapWithIndex((_, value) => value), a) shouldBe true
        arrayHash.eqv(a.combine(a, (left, _) => left), a) shouldBe true
      }
    }

    test("property_plus_then_minus_returns_the_original_for_finite_values") {
      forAll(Arbitraries.genFiniteDoubleArray, Arbitraries.genFiniteDouble) {
        (a: DoubleArray, amount: Double) =>
          val roundTripped = a.plus(amount).minus(amount)
          roundTripped.size shouldBe a.size
          // The tolerance is relative to the magnitudes involved rather than absolute: adding a
          // value of one magnitude to a value of another and taking it away again loses digits
          // in proportion to the larger of the two, so an absolute bound alone would be a claim
          // about doubles that is not true of them. Scaling by the larger magnitude leaves the
          // bound between four and seven orders of magnitude clear of the error these ranges can
          // produce, so a genuine defect still fails it.
          val offenders = (0 until a.size).filterNot { index =>
            val magnitude = math.max(1.0, math.max(math.abs(a.get(index)), math.abs(amount)))
            math.abs(roundTripped.get(index) - a.get(index)) <= 1e-9 * magnitude
          }
          offenders shouldBe empty
      }
    }

    test("property_sum_agrees_with_reduce_for_finite_values") {
      forAll(Arbitraries.genFiniteDoubleArray) { (a: DoubleArray) =>
        // the sum accumulates sequentially from zero in index order, which is a reduction with
        // that identity and that operator, so the two agree exactly rather than approximately
        a.sum shouldBe a.reduce(0.0, (total, value) => total + value)
        a.sum shouldBe a.toList.foldLeft(0.0)((total, value) => total + value)
        a.plus(0.0).sum shouldBe a.sum
      }
    }

    test("property_concat_appends_the_second_array") {
      forAll(Arbitraries.genDoubleArray, Arbitraries.genDoubleArray) {
        (a: DoubleArray, b: DoubleArray) =>
          val joined = a.concat(b)
          joined.size shouldBe (a.size + b.size)
          Arrays.equals(joined.toArray, a.toArray ++ b.toArray) shouldBe true
          arrayHash.eqv(a.concat(DoubleArray.EMPTY), a) shouldBe true
          arrayHash.eqv(DoubleArray.EMPTY.concat(a), a) shouldBe true
      }
    }

    test("property_subArray_at_the_bounds") {
      forAll(Arbitraries.genDoubleArray) { (a: DoubleArray) =>
        arrayHash.eqv(a.subArray(0), a) shouldBe true
        arrayHash.eqv(a.subArray(0, a.size), a) shouldBe true
        a.subArray(a.size) should be theSameInstanceAs DoubleArray.EMPTY
      }
    }

    test("property_sorted_agrees_with_the_primitive_sort") {
      forAll(Arbitraries.genDoubleArray) { (a: DoubleArray) =>
        val expected = a.toArray
        Arrays.sort(expected)
        Arrays.equals(a.sorted.toArray, expected) shouldBe true
        a.sorted.size shouldBe a.size
      }
    }

    test("property_hashCode_and_toString_agree_with_the_primitive_oracles") {
      forAll(Arbitraries.genDoubleArray) { (a: DoubleArray) =>
        a.hashCode shouldBe Arrays.hashCode(a.toArray)
        a.toString shouldBe Arrays.toString(a.toArray)
        implicitly[Show[DoubleArray]].show(a) shouldBe a.toString
      }
    }

    test("property_contains_indexOf_and_lastIndexOf_agree") {
      forAll(Arbitraries.genDoubleArray) { (a: DoubleArray) =>
        val elements = a.toList
        // every element is found; the first occurrence is at or before the last; both positions
        // hold the element, compared by bit pattern so that a not-a-number element counts
        elements.map(value => a.indexOf(value) >= 0).forall(identity) shouldBe true
        elements
          .map { value =>
            val first = a.indexOf(value)
            val last = a.lastIndexOf(value)
            first <= last &&
              bitsOf(a.get(first)) == bitsOf(value) &&
              bitsOf(a.get(last)) == bitsOf(value)
          }
          .forall(identity) shouldBe true
        elements
          .map(value => a.contains(value) == (a.indexOf(value) >= 0))
          .forall(identity) shouldBe true

        // and a value no generator produces is absent, consistently across the three members
        a.contains(Double.MaxValue) shouldBe false
        a.indexOf(Double.MaxValue) shouldBe -1
        a.lastIndexOf(Double.MaxValue) shouldBe -1
      }
    }

    test("property_equal_values_from_different_factories_agree") {
      forAll(Arbitraries.genDoubleArray) { (a: DoubleArray) =>
        val candidates = List(
          DoubleArray.copyOf(a.toArray),
          DoubleArray.copyOf(a.toList),
          DoubleArray.of(a.toList: _*),
          DoubleArray.tabulate(a.size)(index => a.get(index)),
          DoubleArray.copyOf(a.toArray, 0),
          a.concat(DoubleArray.EMPTY))
        candidates.map(candidate => arrayHash.eqv(candidate, a)) shouldBe candidates.map(_ => true)
        candidates.map(_.hashCode).distinct shouldBe List(a.hashCode)
        candidates.map(_.toString).distinct shouldBe List(a.toString)
      }
    }
  }
}

package com.opengamma.strata.audit {

  /**
   * The proof that the two unsafe members of the array type cannot be reached from outside the
   * `strata-collect` module.
   *
   * Those members - the factory that adopts an array without copying it, and the accessor that
   * hands back the array a value holds - are scoped to the whole of
   * `com.opengamma.strata.collect` rather than to the package of the type itself, because the
   * codec object of the module consumes both. That scope is what makes the copying of the public
   * surface complete rather than conventional, and a scope is worth asserting only from a place
   * the scope excludes: the spec of the array type lives inside `collect`, where both members are
   * visible and where a check that they are not would fail.
   *
   * So the two checks are made from here. This object is a sibling of `com.opengamma.strata` at
   * the root rather than a member of `collect`, its package name says what it is for, and the
   * compile-time check that rejects each expression below is answered in this package because
   * that is where the code asking the question sits. Each reference is written out in full, so
   * that nothing an import brought into the spec can change the outcome, and each method answers
   * with the assertion it made so that the spec can state it as its own result.
   *
   * The corresponding positive controls - the same two expressions compiling inside `collect` -
   * are in the spec, which is the only place they can be.
   */
  object DoubleArrayUnsafeAccessProbe extends org.scalatest.Assertions {

    /**
     * Asserts that the adopting factory cannot be called from outside the module.
     *
     * @return the assertion that the call does not compile here
     */
    def ofUnsafeIsInaccessible: org.scalatest.Assertion =
      assertDoesNotCompile("com.opengamma.strata.collect.array.DoubleArray.ofUnsafe(Array(1.0))")

    /**
     * Asserts that the accessor handing back the stored array cannot be called from outside the
     * module.
     *
     * @return the assertion that the call does not compile here
     */
    def toArrayUnsafeIsInaccessible: org.scalatest.Assertion =
      assertDoesNotCompile("com.opengamma.strata.collect.array.DoubleArray.of(1.0).toArrayUnsafe")
  }
}
