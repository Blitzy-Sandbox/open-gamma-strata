/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import java.util.Arrays

import scala.collection.immutable.List

import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.Outcome
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import com.opengamma.strata.collect.Arbitraries._
import com.opengamma.strata.collect.array.DoubleArray

/**
 * Tests [[DoubleArrayMath]], the primitive-array arithmetic the immutable array wrappers and,
 * through them, the whole domain layer of this port are built on.
 *
 * ===How this spec is named===
 *
 * Each test that carries a value or an expectation over from the Java test class is named
 * after the Java test method it comes from, letter for letter, because the method-level
 * traceability manifest of this migration maps every Java test method to a test name in the
 * spec that replaced it and that mapping is checked mechanically. The tests that cover the
 * surface this port adds - the two scalar comparison members, the reordering member and the
 * properties that replace the reflective sweeps - are named as sentences instead, since no
 * Java method corresponds to them.
 *
 * ===What the port changed, and how that is asserted here===
 *
 * The Java class offered each element-wise operation twice, once returning a new array and
 * once writing back into the array it was given, and sorted a pair of arrays in place. Only
 * the copying half is ported, so the seven Java test methods that exercised the in-place
 * members have no operation left to call. They are not dropped: each is carried over as a
 * test of the copying member that replaced it, asserting both that it computes the value the
 * in-place member used to leave behind '''and''' that the array it was given is unchanged
 * afterwards, followed by a compile-time proof that the in-place member does not exist. The
 * same is done for the Java coverage method, whose subject - a private constructor reached by
 * reflection - has no counterpart in an object.
 *
 * Immutability is therefore the central claim of this spec rather than an aside. It is
 * asserted three times over: in each individual test, by an overridden `withFixture` that
 * re-checks all five shared fixtures after '''every''' test in the suite - the standing form
 * of the Java class's own after-each check - and by a property that runs every member of the
 * object over generated arrays and compares them to snapshots taken beforehand.
 *
 * ===The two scalar members this spec alone covers===
 *
 * `fuzzyEquals` and `isMathematicalInteger` are available for scalar values as well as for
 * arrays. The scalar forms replace a numeric comparison that the Java class delegated to an
 * external helper this port does not depend on, and they are called directly from the domain
 * layer: a conversion that is asked to restate an amount in the currency it is already in
 * accepts only a rate that compares equal to one, and an amount renders as a whole number
 * exactly when its value is integral. Nothing else in the port covers them, so every edge of
 * their contract is asserted here individually:
 *
 *   - two finite values no further apart than the tolerance are equal, and values further
 *     apart are not - so two finite values any distance apart are equal at an infinite
 *     tolerance;
 *   - each infinity is equal to itself and to no other value, at any tolerance, an infinite
 *     tolerance included: neither the other infinity nor any finite value is brought to it;
 *   - negative zero and positive zero are equal, at any tolerance;
 *   - a not-a-number value is equal to nothing, itself included, however large the tolerance.
 *     It has no distance from any value, so no tolerance reaches it, and reflexivity of the
 *     comparison therefore holds exactly for the values that are not not-a-number. Bit-for-bit
 *     structural equality is a different contract and does keep such a value reflexive; the
 *     two stand side by side deliberately;
 *   - a tolerance that is negative, or that is not a number, is a caller error and is reported
 *     as one, by all three comparison members, ahead of any comparison - so an empty array and
 *     a mismatched pair of arrays report it too. Negative zero is not a negative tolerance and
 *     is accepted, comparing two finite values exactly;
 *   - a value is a mathematical integer when it is finite and has no fractional part, which
 *     both zeroes and every large enough magnitude satisfy, and which no infinity and no
 *     not-a-number value satisfies.
 *
 * ===Exceptions, and why there are any===
 *
 * Every failure of this object states a caller contract rather than a property of the data -
 * two arrays that cannot be combined or sorted because they differ in length, a position that
 * does not index the array being reordered, a tolerance that is negative or is not a number -
 * so each is a fail-fast check from [[ArgCheck]] and arrives as an
 * `IllegalArgumentException`. That is what the
 * failing cases here expect, and the complete message text is asserted rather than the type
 * alone. There is no failure channel on this object to assert instead, and the one question
 * it answers about data rather than about its caller - whether two arrays of different
 * lengths are equal within a tolerance - answers `false`, which is asserted as such.
 *
 * ===Numerical parity===
 *
 * This spec is unit-level: its expectations are the exact values of the Java test class plus
 * the edges above. The fixture-driven comparison of this object's arithmetic against values
 * captured from the Java implementation, to nine decimal places absolute and relative, lives
 * in the parity spec of the array package and not here.
 */
final class DoubleArrayMathSpec
    extends AnyFunSuite
    with Matchers
    with TableDrivenPropertyChecks
    with ScalaCheckPropertyChecks {

  //-------------------------------------------------------------------------
  // The five fixtures of the Java test class, with its values. They are shared across the
  // suite deliberately: a member that wrote into an array it was given would be caught by the
  // after-each check below, in whichever test it happened.

  /** The Java fixture `ARRAY_0_0`: two values close enough to zero to compare equal to it. */
  private val Array00: Array[Double] = Array(-1.0e-4, 1.0e-3)

  /** The Java fixture `ARRAY_1_2`. */
  private val Array12: Array[Double] = Array(1.0, 2.0)

  /** The Java fixture `ARRAY_1_2B`: `ARRAY_1_2` perturbed by the two tolerances under test. */
  private val Array12B: Array[Double] = Array(1.0 - 1.0e-4, 2.0 + 1.0e-3)

  /** The Java fixture `ARRAY_3_4`. */
  private val Array34: Array[Double] = Array(3.0, 4.0)

  /** The Java fixture `ARRAY_3`, one element long, which every mismatched-length case uses. */
  private val Array3: Array[Double] = Array(3.0)

  //-------------------------------------------------------------------------
  // The standing immutability check. The Java class asserted its five fixtures after each
  // test through an after-each hook; this is that hook, and it runs for every test in the
  // suite - the ported ones, the added ones and the properties alike.

  override protected def withFixture(testToRun: NoArgTest): Outcome = {
    val outcome = super.withFixture(testToRun)
    assertFixturesUnchanged()
    outcome
  }

  /** Re-asserts the value of every shared fixture, naming the one that changed if any did. */
  private def assertFixturesUnchanged(): Unit = {
    assertFixture("ARRAY_0_0", Array00, Array(-1.0e-4, 1.0e-3))
    assertFixture("ARRAY_1_2", Array12, Array(1.0, 2.0))
    assertFixture("ARRAY_1_2B", Array12B, Array(1.0 - 1.0e-4, 2.0 + 1.0e-3))
    assertFixture("ARRAY_3_4", Array34, Array(3.0, 4.0))
    assertFixture("ARRAY_3", Array3, Array(3.0))
  }

  private def assertFixture(name: String, actual: Array[Double], expected: Array[Double]): Unit =
    if (!Arrays.equals(actual, expected)) {
      fail(s"the shared fixture $name was modified: expected ${expected.toList} but found ${actual.toList}")
    }

  //-------------------------------------------------------------------------
  // Assertion helpers.

  /**
   * Asserts the exact contents of an array, comparing each element by its bits.
   *
   * This is the comparison the shared fixture check and every case involving an infinity, a
   * not-a-number value or a signed zero uses, because ordinary numeric comparison answers
   * that a not-a-number value differs from itself and that the two zeroes are the same value,
   * and both of those are exactly what such a case is asserting about.
   */
  private def assertValues(actual: Array[Double], expected: Array[Double]): Unit =
    if (!Arrays.equals(actual, expected)) {
      fail(s"expected ${expected.toList} but found ${actual.toList}")
    }

  /**
   * The bits of every element of an array, which is how a conversion is held to its values.
   *
   * A conversion that lost the sign of a zero, or answered one not-a-number value where it was
   * given another, would pass an ordinary comparison of the elements: `-0.0 == 0.0` holds and
   * `Double.NaN == Double.NaN` does not. Comparing the bits is what makes either visible.
   */
  private def bitsOf(values: Array[Double]): List[Long] =
    values.iterator.map(value => java.lang.Double.doubleToLongBits(value)).toList

  /** The bits of every element of an array of boxed values, the counterpart of [[bitsOf]]. */
  private def bitsOfBoxed(values: Array[java.lang.Double]): List[Long] =
    values.iterator.map(value => java.lang.Double.doubleToLongBits(value.doubleValue)).toList

  /** Runs a check that is required to fail, and answers the message it failed with. */
  private def messageOf(check: => Any): String =
    intercept[IllegalArgumentException](check).getMessage

  /** The message every length-mismatched combination reports. */
  private val CombineLengthMessage = "Arrays cannot be combined as they differ in length"

  /** The message every length-mismatched paired sort reports. */
  private val SortLengthMessage = "Arrays cannot be sorted as they differ in length"

  /** The message a length-mismatched reordering reports. */
  private val ReorderLengthMessage = "Value array cannot be reordered as they differ in length"

  /** The message a negative tolerance reports, worded by the fail-fast check itself. */
  private def negativeToleranceMessage(tolerance: Double): String =
    s"Argument 'tolerance' must not be negative but has value $tolerance"

  /** The message a not-a-number tolerance reports, worded by the fail-fast check itself. */
  private val NanToleranceMessage: String = "Argument 'tolerance' must not be NaN"

  /** The message a position that does not index the values being reordered reports. */
  private def positionRangeMessage(length: Int, position: Int): String =
    s"Expected 0 <= 'key' < $length, but found $position"

  /** The positions that reorder an array of the given length into itself. */
  private def identityPositions(length: Int): Array[Int] = Array.range(0, length)

  /** The elements of a generated array, as a primitive array this object can be given. */
  private def elementsOf(array: DoubleArray): Array[Double] = array.toArray

  //-------------------------------------------------------------------------
  // The two empty constants.

  test("test_EMPTY_DOUBLE_ARRAY") {
    DoubleArrayMath.EMPTY_DOUBLE_ARRAY.length shouldBe 0
    DoubleArrayMath.EMPTY_DOUBLE_ARRAY.toList shouldBe List.empty[Double]
  }

  test("test_EMPTY_DOUBLE_OBJECT_ARRAY") {
    DoubleArrayMath.EMPTY_DOUBLE_OBJECT_ARRAY.length shouldBe 0
    DoubleArrayMath.EMPTY_DOUBLE_OBJECT_ARRAY.toList shouldBe List.empty[java.lang.Double]
  }

  //-------------------------------------------------------------------------
  // Conversion between the primitive and the boxed element type.

  test("toPrimitive") {
    DoubleArrayMath.toPrimitive(Array.empty[java.lang.Double]).toList shouldBe List.empty[Double]
    val boxed = Array[java.lang.Double](java.lang.Double.valueOf(1.0), java.lang.Double.valueOf(2.5))
    DoubleArrayMath.toPrimitive(boxed).toList shouldBe List(1.0, 2.5)
  }

  test("toObject") {
    DoubleArrayMath.toObject(Array.empty[Double]).length shouldBe 0
    val boxed = DoubleArrayMath.toObject(Array(1.0, 2.5))
    boxed.length shouldBe 2
    boxed.map(value => value.doubleValue).toList shouldBe List(1.0, 2.5)
    DoubleArrayMath.toPrimitive(boxed).toList shouldBe List(1.0, 2.5)
  }

  test("an empty conversion answers the shared empty constant rather than a fresh array") {
    (DoubleArrayMath.toPrimitive(Array.empty[java.lang.Double]) eq DoubleArrayMath.EMPTY_DOUBLE_ARRAY) shouldBe true
    (DoubleArrayMath.toObject(Array.empty[Double]) eq DoubleArrayMath.EMPTY_DOUBLE_OBJECT_ARRAY) shouldBe true
  }

  test("conversion carries an infinity, a signed zero and a not-a-number value through unaltered") {
    val values = Array(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity, -0.0, 0.0)
    assertValues(DoubleArrayMath.toPrimitive(DoubleArrayMath.toObject(values)), values)
  }

  test("conversion preserves the bits of every element, in both directions") {
    val values = Array(0.0, -0.0, Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity, 1.5, -2.5)
    val boxed = DoubleArrayMath.toObject(values)
    boxed.length shouldBe values.length
    bitsOfBoxed(boxed) shouldBe bitsOf(values)
    val primitive = DoubleArrayMath.toPrimitive(boxed)
    primitive.length shouldBe values.length
    bitsOf(primitive) shouldBe bitsOf(values)
  }

  test("conversion answers a fresh array that the array it was given can no longer reach") {
    val values = Array(1.0, 2.0, 3.0)
    val boxed = DoubleArrayMath.toObject(values)
    val primitive = DoubleArrayMath.toPrimitive(boxed)
    (primitive ne values) shouldBe true
    values(0) = 9.0
    bitsOfBoxed(boxed) shouldBe bitsOf(Array(1.0, 2.0, 3.0))
    bitsOf(primitive) shouldBe bitsOf(Array(1.0, 2.0, 3.0))
  }

  test("conversion of a generated array preserves its length and the bits of every element") {
    forAll(genDoubleArray) { array =>
      val values = elementsOf(array)
      val boxed = DoubleArrayMath.toObject(values)
      boxed.length shouldBe values.length
      bitsOfBoxed(boxed) shouldBe bitsOf(values)
      val primitive = DoubleArrayMath.toPrimitive(boxed)
      primitive.length shouldBe values.length
      assertValues(primitive, values)
      succeed
    }
  }

  //-------------------------------------------------------------------------
  // Summation.

  test("test_sum") {
    DoubleArrayMath.sum(Array12) shouldBe 3.0
    DoubleArrayMath.sum(Array3) shouldBe 3.0
    DoubleArrayMath.sum(DoubleArrayMath.EMPTY_DOUBLE_ARRAY) shouldBe 0.0
  }

  test("the sum is taken in index order, so it reproduces the order-dependent result exactly") {
    val values = Array(1.0e16, 1.0, -1.0e16)
    DoubleArrayMath.sum(values) shouldBe ((1.0e16 + 1.0) - 1.0e16)
  }

  //-------------------------------------------------------------------------
  // Element-wise operations against a constant and an operator.

  test("test_applyAddition") {
    DoubleArrayMath.applyAddition(Array12, 2.0).toList shouldBe List(3.0, 4.0)
  }

  test("test_applyMultiplication") {
    DoubleArrayMath.applyMultiplication(Array12, 4.0).toList shouldBe List(4.0, 8.0)
  }

  test("test_apply") {
    DoubleArrayMath.apply(Array12, value => 1.0 / value).toList shouldBe List(1.0, 1.0 / 2.0)
  }

  test("an element-wise operation over an empty array is an empty array") {
    DoubleArrayMath.applyAddition(DoubleArrayMath.EMPTY_DOUBLE_ARRAY, 2.0).toList shouldBe List.empty[Double]
    DoubleArrayMath.applyMultiplication(DoubleArrayMath.EMPTY_DOUBLE_ARRAY, 2.0).toList shouldBe List.empty[Double]
    DoubleArrayMath
      .apply(DoubleArrayMath.EMPTY_DOUBLE_ARRAY, value => 1.0 / value)
      .toList shouldBe List.empty[Double]
  }

  test("an element-wise operation returns a new array rather than the one it was given") {
    val values = Array(1.0, 2.0)
    (DoubleArrayMath.applyAddition(values, 0.0) ne values) shouldBe true
    (DoubleArrayMath.applyMultiplication(values, 1.0) ne values) shouldBe true
    (DoubleArrayMath.apply(values, value => value) ne values) shouldBe true
  }

  //-------------------------------------------------------------------------
  // The in-place operations of the Java class, which this port does not have. Each test below
  // stands in for the Java test method of the same name: it asserts that the copying member
  // that replaced the in-place one computes the value the in-place one used to leave behind,
  // that the array it was given is unchanged, and that the in-place member cannot be called.

  test("test_mutateByAddition_byConstant") {
    val values = Array(1.0, 2.0)
    DoubleArrayMath.applyAddition(values, 2.0).toList shouldBe List(3.0, 4.0)
    values.toList shouldBe List(1.0, 2.0)
    assertDoesNotCompile("DoubleArrayMath.mutateByAddition(Array(1.0, 2.0), 2.0)")
  }

  test("test_mutateByMultiplication_byConstant") {
    val values = Array(1.0, 2.0)
    DoubleArrayMath.applyMultiplication(values, 4.0).toList shouldBe List(4.0, 8.0)
    values.toList shouldBe List(1.0, 2.0)
    assertDoesNotCompile("DoubleArrayMath.mutateByMultiplication(Array(1.0, 2.0), 4.0)")
  }

  test("test_mutateByAddition_byArray") {
    val values = Array(1.0, 2.0)
    val addend = Array(2.0, 3.0)
    DoubleArrayMath.combineByAddition(values, addend).toList shouldBe List(3.0, 5.0)
    values.toList shouldBe List(1.0, 2.0)
    addend.toList shouldBe List(2.0, 3.0)
    assertDoesNotCompile("DoubleArrayMath.mutateByAddition(Array(1.0, 2.0), Array(2.0, 3.0))")
  }

  test("test_mutateByMultiplication_byArray") {
    val values = Array(1.0, 2.0)
    val multiplier = Array(4.0, 5.0)
    DoubleArrayMath.combineByMultiplication(values, multiplier).toList shouldBe List(4.0, 10.0)
    values.toList shouldBe List(1.0, 2.0)
    multiplier.toList shouldBe List(4.0, 5.0)
    assertDoesNotCompile("DoubleArrayMath.mutateByMultiplication(Array(1.0, 2.0), Array(4.0, 5.0))")
  }

  test("test_mutateByAddition_byArray_sizeDifferent") {
    val values = Array(1.0, 2.0)
    messageOf(DoubleArrayMath.combineByAddition(values, Array(2.0))) shouldBe CombineLengthMessage
    values.toList shouldBe List(1.0, 2.0)
  }

  test("test_mutateByMultiplication_byArray_sizeDifferent") {
    val values = Array(1.0, 2.0)
    messageOf(DoubleArrayMath.combineByMultiplication(values, Array(4.0))) shouldBe CombineLengthMessage
    values.toList shouldBe List(1.0, 2.0)
  }

  test("test_mutate") {
    val values = Array(1.0, 2.0)
    DoubleArrayMath.apply(values, value => 1.0 / value).toList shouldBe List(1.0, 1.0 / 2.0)
    values.toList shouldBe List(1.0, 2.0)
    assertDoesNotCompile("DoubleArrayMath.mutate(Array(1.0, 2.0), (value: Double) => 1.0 / value)")
  }

  //-------------------------------------------------------------------------
  // Combining two arrays.

  test("test_combineByAddition") {
    DoubleArrayMath.combineByAddition(Array12, Array34).toList shouldBe List(4.0, 6.0)
    messageOf(DoubleArrayMath.combineByAddition(Array12, Array3)) shouldBe CombineLengthMessage
  }

  test("test_combineByMultiplication") {
    DoubleArrayMath.combineByMultiplication(Array12, Array34).toList shouldBe List(3.0, 8.0)
    messageOf(DoubleArrayMath.combineByMultiplication(Array12, Array3)) shouldBe CombineLengthMessage
  }

  test("test_combine") {
    val operator: (Double, Double) => Double = (a, b) => a / b
    DoubleArrayMath.combine(Array12, Array34, operator).toList shouldBe List(1.0 / 3.0, 2.0 / 4.0)
    messageOf(DoubleArrayMath.combine(Array12, Array3, operator)) shouldBe CombineLengthMessage
  }

  test("test_combineLenient") {
    val operator: (Double, Double) => Double = (a, b) => a / b
    DoubleArrayMath.combineLenient(Array12, Array34, operator).toList shouldBe List(1.0 / 3.0, 2.0 / 4.0)
    DoubleArrayMath.combineLenient(Array12, Array3, operator).toList shouldBe List(1.0 / 3.0, 2.0)
    DoubleArrayMath.combineLenient(Array3, Array12, operator).toList shouldBe List(3.0 / 1.0, 2.0)
  }

  test("combining two empty arrays is an empty array, strictly and leniently alike") {
    val empty = DoubleArrayMath.EMPTY_DOUBLE_ARRAY
    val operator: (Double, Double) => Double = (a, b) => a + b
    DoubleArrayMath.combine(empty, empty, operator).toList shouldBe List.empty[Double]
    DoubleArrayMath.combineLenient(empty, empty, operator).toList shouldBe List.empty[Double]
    DoubleArrayMath.combineByAddition(empty, empty).toList shouldBe List.empty[Double]
    DoubleArrayMath.combineByMultiplication(empty, empty).toList shouldBe List.empty[Double]
  }

  test("a lenient combination carries the elements of the longer array through untouched") {
    val operator: (Double, Double) => Double = (a, b) => a * b
    DoubleArrayMath
      .combineLenient(Array(2.0, 3.0, 5.0, 7.0), Array(11.0, 13.0), operator)
      .toList shouldBe List(22.0, 39.0, 5.0, 7.0)
    DoubleArrayMath
      .combineLenient(Array(11.0, 13.0), Array(2.0, 3.0, 5.0, 7.0), operator)
      .toList shouldBe List(22.0, 39.0, 5.0, 7.0)
    DoubleArrayMath
      .combineLenient(DoubleArrayMath.EMPTY_DOUBLE_ARRAY, Array(2.0, 3.0), operator)
      .toList shouldBe List(2.0, 3.0)
  }

  //-------------------------------------------------------------------------
  // Comparison within a tolerance, over arrays: the two cases of the Java test class.

  test("test_fuzzyEqualsZero") {
    DoubleArrayMath.fuzzyEqualsZero(DoubleArrayMath.EMPTY_DOUBLE_ARRAY, 1.0e-2) shouldBe true
    DoubleArrayMath.fuzzyEqualsZero(Array00, 1.0e-2) shouldBe true
    DoubleArrayMath.fuzzyEqualsZero(Array12, 1.0e-2) shouldBe false
  }

  test("test_fuzzyEquals") {
    DoubleArrayMath.fuzzyEquals(DoubleArrayMath.EMPTY_DOUBLE_ARRAY, Array00, 1.0e-2) shouldBe false
    DoubleArrayMath.fuzzyEquals(Array00, Array00, 1.0e-2) shouldBe true
    DoubleArrayMath.fuzzyEquals(Array12, Array12, 1.0e-2) shouldBe true
    DoubleArrayMath.fuzzyEquals(Array12, Array12B, 1.0e-2) shouldBe true
    DoubleArrayMath.fuzzyEquals(Array12, Array12B, 1.0e-3) shouldBe true
    DoubleArrayMath.fuzzyEquals(Array12, Array12B, 1.0e-4) shouldBe false
  }

  //-------------------------------------------------------------------------
  // Comparison within a tolerance, over scalar values. This is one of the two members the
  // port adds, and the domain layer calls it directly, so every edge of it is asserted here.

  test("the scalar comparison holds for two values that are the same value") {
    DoubleArrayMath.fuzzyEquals(1.0, 1.0, 0.0) shouldBe true
    DoubleArrayMath.fuzzyEquals(-12.5, -12.5, 1.0e-8) shouldBe true
    DoubleArrayMath.fuzzyEquals(0.0, 0.0, 0.0) shouldBe true
  }

  test("the scalar comparison holds for values no further apart than the tolerance") {
    DoubleArrayMath.fuzzyEquals(1.0, 1.0 + 1.0e-9, 1.0e-8) shouldBe true
    DoubleArrayMath.fuzzyEquals(1.0 + 1.0e-9, 1.0, 1.0e-8) shouldBe true
    DoubleArrayMath.fuzzyEquals(100.0, 100.5, 0.5) shouldBe true
    DoubleArrayMath.fuzzyEquals(100.5, 100.0, 0.5) shouldBe true
  }

  test("the scalar comparison fails for values further apart than the tolerance") {
    DoubleArrayMath.fuzzyEquals(1.0, 1.0 + 1.0e-7, 1.0e-8) shouldBe false
    DoubleArrayMath.fuzzyEquals(1.0 + 1.0e-7, 1.0, 1.0e-8) shouldBe false
    DoubleArrayMath.fuzzyEquals(100.0, 100.6, 0.5) shouldBe false
    DoubleArrayMath.fuzzyEquals(1.0, 2.0, 0.0) shouldBe false
  }

  test("a zero tolerance compares the two values exactly") {
    DoubleArrayMath.fuzzyEquals(1.0, 1.0, 0.0) shouldBe true
    DoubleArrayMath.fuzzyEquals(1.0, math.nextUp(1.0), 0.0) shouldBe false
    DoubleArrayMath.fuzzyEquals(1.0, math.nextUp(1.0), math.ulp(1.0)) shouldBe true
  }

  test("negative zero and positive zero are equal at any tolerance") {
    DoubleArrayMath.fuzzyEquals(-0.0, 0.0, 0.0) shouldBe true
    DoubleArrayMath.fuzzyEquals(0.0, -0.0, 0.0) shouldBe true
    DoubleArrayMath.fuzzyEquals(-0.0, -0.0, 0.0) shouldBe true
    DoubleArrayMath.fuzzyEquals(-0.0, 0.0, 1.0e-8) shouldBe true
  }

  test("each infinity is equal to itself and to no other value, at any tolerance") {
    DoubleArrayMath.fuzzyEquals(Double.PositiveInfinity, Double.PositiveInfinity, 0.0) shouldBe true
    DoubleArrayMath.fuzzyEquals(Double.NegativeInfinity, Double.NegativeInfinity, 0.0) shouldBe true
    DoubleArrayMath.fuzzyEquals(Double.PositiveInfinity, Double.NegativeInfinity, Double.MaxValue) shouldBe false
    DoubleArrayMath.fuzzyEquals(Double.NegativeInfinity, Double.PositiveInfinity, Double.MaxValue) shouldBe false
    DoubleArrayMath.fuzzyEquals(Double.PositiveInfinity, 1.0e300, Double.MaxValue) shouldBe false
    DoubleArrayMath.fuzzyEquals(1.0e300, Double.NegativeInfinity, Double.MaxValue) shouldBe false

    // an infinite tolerance is no exception: it brings neither the other infinity nor any
    // finite value to an infinity, which is the edge the comparison used to get wrong
    DoubleArrayMath.fuzzyEquals(Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity) shouldBe
      true
    DoubleArrayMath.fuzzyEquals(Double.NegativeInfinity, Double.NegativeInfinity, Double.PositiveInfinity) shouldBe
      true
    DoubleArrayMath.fuzzyEquals(Double.PositiveInfinity, Double.NegativeInfinity, Double.PositiveInfinity) shouldBe
      false
    DoubleArrayMath.fuzzyEquals(Double.NegativeInfinity, Double.PositiveInfinity, Double.PositiveInfinity) shouldBe
      false
    DoubleArrayMath.fuzzyEquals(Double.PositiveInfinity, 1.0e300, Double.PositiveInfinity) shouldBe false
    DoubleArrayMath.fuzzyEquals(Double.NegativeInfinity, 0.0, Double.PositiveInfinity) shouldBe false
    DoubleArrayMath.fuzzyEquals(0.0, Double.PositiveInfinity, Double.PositiveInfinity) shouldBe false
  }

  test("an infinite tolerance admits any two finite values, and still brings no infinity to another value") {
    DoubleArrayMath.fuzzyEquals(0.0, 1.0e300, Double.PositiveInfinity) shouldBe true
    DoubleArrayMath.fuzzyEquals(-Double.MaxValue, Double.MaxValue, Double.PositiveInfinity) shouldBe true
    DoubleArrayMath.fuzzyEquals(Double.PositiveInfinity, Double.NegativeInfinity, Double.PositiveInfinity) shouldBe
      false
    DoubleArrayMath.fuzzyEquals(Double.PositiveInfinity, 0.0, Double.PositiveInfinity) shouldBe false
    DoubleArrayMath.fuzzyEquals(Double.NaN, 0.0, Double.PositiveInfinity) shouldBe false
  }

  test("a not-a-number value is equal to no different value, however large the tolerance") {
    DoubleArrayMath.fuzzyEquals(Double.NaN, 0.0, Double.MaxValue) shouldBe false
    DoubleArrayMath.fuzzyEquals(0.0, Double.NaN, Double.MaxValue) shouldBe false
    DoubleArrayMath.fuzzyEquals(Double.NaN, 1.0, 1.0e-8) shouldBe false
    DoubleArrayMath.fuzzyEquals(Double.NaN, Double.PositiveInfinity, Double.PositiveInfinity) shouldBe false
    DoubleArrayMath.fuzzyEquals(Double.NegativeInfinity, Double.NaN, Double.PositiveInfinity) shouldBe false
  }

  test("a not-a-number value is not equal to another not-a-number value, at any tolerance") {
    DoubleArrayMath.fuzzyEquals(Double.NaN, Double.NaN, 0.0) shouldBe false
    DoubleArrayMath.fuzzyEquals(Double.NaN, Double.NaN, 1.0e-8) shouldBe false
    DoubleArrayMath.fuzzyEquals(Double.NaN, Double.NaN, Double.MaxValue) shouldBe false
    DoubleArrayMath.fuzzyEquals(Double.NaN, Double.NaN, Double.PositiveInfinity) shouldBe false
  }

  test("a negative tolerance is rejected by every comparison member") {
    messageOf(DoubleArrayMath.fuzzyEquals(1.0, 1.0, -1.0e-9)) shouldBe negativeToleranceMessage(-1.0e-9)
    messageOf(DoubleArrayMath.fuzzyEquals(Array12, Array12, -1.0e-9)) shouldBe negativeToleranceMessage(-1.0e-9)
    messageOf(DoubleArrayMath.fuzzyEqualsZero(Array12, -1.0e-9)) shouldBe negativeToleranceMessage(-1.0e-9)
  }

  test("a negative tolerance is rejected ahead of the comparison, so empty and mismatched arrays report it too") {
    messageOf(
      DoubleArrayMath.fuzzyEquals(DoubleArrayMath.EMPTY_DOUBLE_ARRAY, Array12, -1.0)) shouldBe
      negativeToleranceMessage(-1.0)
    messageOf(
      DoubleArrayMath.fuzzyEquals(
        DoubleArrayMath.EMPTY_DOUBLE_ARRAY,
        DoubleArrayMath.EMPTY_DOUBLE_ARRAY,
        -1.0)) shouldBe negativeToleranceMessage(-1.0)
    messageOf(
      DoubleArrayMath.fuzzyEqualsZero(DoubleArrayMath.EMPTY_DOUBLE_ARRAY, -1.0)) shouldBe
      negativeToleranceMessage(-1.0)
  }

  test("a negative zero tolerance is not a negative tolerance and compares exactly") {
    DoubleArrayMath.fuzzyEquals(1.0, 1.0, -0.0) shouldBe true
    DoubleArrayMath.fuzzyEquals(1.0, math.nextUp(1.0), -0.0) shouldBe false
    DoubleArrayMath.fuzzyEqualsZero(Array(0.0, -0.0), -0.0) shouldBe true
  }

  test("a not-a-number tolerance is rejected by every comparison member") {
    messageOf(DoubleArrayMath.fuzzyEquals(1.0, 1.0, Double.NaN)) shouldBe NanToleranceMessage
    messageOf(DoubleArrayMath.fuzzyEquals(Array12, Array12, Double.NaN)) shouldBe NanToleranceMessage
    messageOf(DoubleArrayMath.fuzzyEqualsZero(Array12, Double.NaN)) shouldBe NanToleranceMessage
  }

  test("a not-a-number tolerance is rejected ahead of the comparison, so empty and mismatched arrays report it too") {
    messageOf(
      DoubleArrayMath.fuzzyEquals(DoubleArrayMath.EMPTY_DOUBLE_ARRAY, Array12, Double.NaN)) shouldBe
      NanToleranceMessage
    messageOf(
      DoubleArrayMath.fuzzyEquals(
        DoubleArrayMath.EMPTY_DOUBLE_ARRAY,
        DoubleArrayMath.EMPTY_DOUBLE_ARRAY,
        Double.NaN)) shouldBe NanToleranceMessage
    messageOf(
      DoubleArrayMath.fuzzyEqualsZero(DoubleArrayMath.EMPTY_DOUBLE_ARRAY, Double.NaN)) shouldBe
      NanToleranceMessage
  }

  //-------------------------------------------------------------------------
  // The array comparisons over the edges of the value space, and over lengths that differ.

  test("the comparison of two arrays sees every element, including the edges of the value space") {
    // one not-a-number element is enough to make the two arrays unequal, however the rest compare
    DoubleArrayMath.fuzzyEquals(
      Array(Double.NaN, Double.PositiveInfinity, -0.0),
      Array(Double.NaN, Double.PositiveInfinity, 0.0),
      0.0) shouldBe false

    // the same pair without that element is equal, so it is the element and not the infinity or
    // the signed zero that the comparison refuses
    DoubleArrayMath.fuzzyEquals(
      Array(Double.PositiveInfinity, -0.0),
      Array(Double.PositiveInfinity, 0.0),
      0.0) shouldBe true
    DoubleArrayMath.fuzzyEquals(Array(Double.PositiveInfinity), Array(Double.NegativeInfinity), 1.0) shouldBe false
    DoubleArrayMath.fuzzyEquals(
      Array(Double.PositiveInfinity),
      Array(Double.NegativeInfinity),
      Double.PositiveInfinity) shouldBe false
    DoubleArrayMath.fuzzyEquals(Array(Double.NaN), Array(Double.NaN), Double.PositiveInfinity) shouldBe false
    DoubleArrayMath.fuzzyEquals(Array(1.0, Double.NaN), Array(1.0, 2.0), 1.0) shouldBe false
    DoubleArrayMath.fuzzyEquals(Array(1.0, 2.0), Array(1.0, Double.NaN), 1.0) shouldBe false
  }

  test("two arrays of different lengths are not equal, and that is an answer rather than an error") {
    DoubleArrayMath.fuzzyEquals(Array12, Array3, 1.0e2) shouldBe false
    DoubleArrayMath.fuzzyEquals(Array3, Array12, 1.0e2) shouldBe false
    DoubleArrayMath.fuzzyEquals(DoubleArrayMath.EMPTY_DOUBLE_ARRAY, Array3, 1.0e2) shouldBe false
  }

  test("two empty arrays are equal, and an empty array is effectively zero") {
    DoubleArrayMath.fuzzyEquals(
      DoubleArrayMath.EMPTY_DOUBLE_ARRAY,
      DoubleArrayMath.EMPTY_DOUBLE_ARRAY,
      0.0) shouldBe true
    DoubleArrayMath.fuzzyEqualsZero(DoubleArrayMath.EMPTY_DOUBLE_ARRAY, 0.0) shouldBe true
  }

  test("comparison with zero sees every element, including the edges of the value space") {
    DoubleArrayMath.fuzzyEqualsZero(Array(0.0, -0.0), 0.0) shouldBe true
    DoubleArrayMath.fuzzyEqualsZero(Array(1.0e-4, -1.0e-4), 1.0e-3) shouldBe true
    DoubleArrayMath.fuzzyEqualsZero(Array(1.0e-2), 1.0e-3) shouldBe false
    DoubleArrayMath.fuzzyEqualsZero(Array(Double.NaN), Double.MaxValue) shouldBe false
    DoubleArrayMath.fuzzyEqualsZero(Array(Double.PositiveInfinity), Double.MaxValue) shouldBe false
    DoubleArrayMath.fuzzyEqualsZero(Array(Double.NegativeInfinity), Double.MaxValue) shouldBe false

    // no tolerance brings an infinity or a not-a-number element to zero, an infinite one included
    DoubleArrayMath.fuzzyEqualsZero(Array(Double.PositiveInfinity), Double.PositiveInfinity) shouldBe false
    DoubleArrayMath.fuzzyEqualsZero(Array(Double.NegativeInfinity), Double.PositiveInfinity) shouldBe false
    DoubleArrayMath.fuzzyEqualsZero(Array(Double.NaN), Double.PositiveInfinity) shouldBe false
    DoubleArrayMath.fuzzyEqualsZero(Array(1.0e300), Double.PositiveInfinity) shouldBe true
  }

  //-------------------------------------------------------------------------
  // Whether a value is an integer in the mathematical sense: the second member the port adds.

  /** Values with no fractional part, spanning both zeroes, both signs and four magnitudes. */
  private val IntegralValues =
    Table(
      "value",
      0.0,
      -0.0,
      1.0,
      -1.0,
      2.0,
      100.0,
      -100.0,
      1.0e9,
      -1.0e9,
      1.0e15,
      9.007199254740992e15,
      1.0e300,
      Double.MaxValue)

  /** Values with a fractional part, down to the smallest magnitude a double can hold. */
  private val FractionalValues =
    Table(
      "value",
      0.5,
      -0.5,
      0.1,
      -0.1,
      1.5,
      -1.5,
      100.5,
      -100.5,
      3.14159,
      1.0e-3,
      -1.0e-3,
      Double.MinPositiveValue)

  test("a finite value with no fractional part is an integer in the mathematical sense") {
    forAll(IntegralValues) { value =>
      DoubleArrayMath.isMathematicalInteger(value) shouldBe true
    }
  }

  test("a value with a fractional part is not an integer in the mathematical sense") {
    forAll(FractionalValues) { value =>
      DoubleArrayMath.isMathematicalInteger(value) shouldBe false
    }
  }

  test("neither infinity and no not-a-number value is an integer in the mathematical sense") {
    DoubleArrayMath.isMathematicalInteger(Double.NaN) shouldBe false
    DoubleArrayMath.isMathematicalInteger(Double.PositiveInfinity) shouldBe false
    DoubleArrayMath.isMathematicalInteger(Double.NegativeInfinity) shouldBe false
  }

  //-------------------------------------------------------------------------
  // Reordering a copy of an array by a list of positions.

  test("reorderedCopy takes the element named by the position at each index") {
    val values = Array(1.0, 5.0, 10.0)
    val positions = Array(2, 0, 1)
    DoubleArrayMath.reorderedCopy(values, positions).toList shouldBe List(10.0, 1.0, 5.0)
    values.toList shouldBe List(1.0, 5.0, 10.0)
    positions.toList shouldBe List(2, 0, 1)
  }

  test("reorderedCopy repeats a value whose position is named more than once") {
    DoubleArrayMath.reorderedCopy(Array(1.0, 5.0, 10.0), Array(0, 0, 2)).toList shouldBe List(1.0, 1.0, 10.0)
    DoubleArrayMath.reorderedCopy(Array(1.0, 5.0, 10.0), Array(1, 1, 1)).toList shouldBe List(5.0, 5.0, 5.0)
  }

  test("reorderedCopy by the identity positions is a copy rather than the array it was given") {
    val values = Array(1.0, 5.0, 10.0)
    val copy = DoubleArrayMath.reorderedCopy(values, identityPositions(values.length))
    copy.toList shouldBe List(1.0, 5.0, 10.0)
    (copy ne values) shouldBe true
  }

  test("reorderedCopy of an empty array is an empty array") {
    DoubleArrayMath
      .reorderedCopy(DoubleArrayMath.EMPTY_DOUBLE_ARRAY, Array.empty[Int])
      .toList shouldBe List.empty[Double]
  }

  test("reorderedCopy carries the edges of the value space through unaltered") {
    val values = Array(Double.NaN, -0.0, Double.PositiveInfinity)
    assertValues(
      DoubleArrayMath.reorderedCopy(values, Array(2, 1, 0)),
      Array(Double.PositiveInfinity, -0.0, Double.NaN))
  }

  test("reorderedCopy rejects arrays that differ in length") {
    messageOf(DoubleArrayMath.reorderedCopy(Array(1.0, 5.0, 10.0), Array(0, 1))) shouldBe ReorderLengthMessage
    messageOf(DoubleArrayMath.reorderedCopy(Array(1.0, 5.0), Array(0, 1, 2))) shouldBe ReorderLengthMessage
    messageOf(DoubleArrayMath.reorderedCopy(Array(1.0), Array.empty[Int])) shouldBe ReorderLengthMessage
  }

  test("reorderedCopy rejects a position that does not index the values") {
    messageOf(DoubleArrayMath.reorderedCopy(Array(1.0, 5.0, 10.0), Array(3, 0, 1))) shouldBe
      positionRangeMessage(3, 3)
    messageOf(DoubleArrayMath.reorderedCopy(Array(1.0, 5.0, 10.0), Array(0, -1, 1))) shouldBe
      positionRangeMessage(3, -1)
  }

  //-------------------------------------------------------------------------
  // Sorting a pair of arrays by the first, with a value of each of the three element types.
  // Every case asserts the two results and that both inputs are in the order they were given
  // in, which is what makes the change from sorting in place to returning copies observable.

  test("test_sortPairs_doubledouble_1") {
    val keys = Array(3.0, 5.0, 2.0, 4.0)
    val values = Array(6.0, 10.0, 4.0, 8.0)
    val (sortedKeys, sortedValues) = DoubleArrayMath.sortPairs(keys, values)
    sortedKeys.toList shouldBe List(2.0, 3.0, 4.0, 5.0)
    sortedValues.toList shouldBe List(4.0, 6.0, 8.0, 10.0)
    keys.toList shouldBe List(3.0, 5.0, 2.0, 4.0)
    values.toList shouldBe List(6.0, 10.0, 4.0, 8.0)
  }

  test("test_sortPairs_doubledouble_2") {
    val keys = Array(3.0, 2.0, 5.0, 4.0)
    val values = Array(6.0, 4.0, 10.0, 8.0)
    val (sortedKeys, sortedValues) = DoubleArrayMath.sortPairs(keys, values)
    sortedKeys.toList shouldBe List(2.0, 3.0, 4.0, 5.0)
    sortedValues.toList shouldBe List(4.0, 6.0, 8.0, 10.0)
    keys.toList shouldBe List(3.0, 2.0, 5.0, 4.0)
    values.toList shouldBe List(6.0, 4.0, 10.0, 8.0)
  }

  test("test_sortPairs_doubledouble_sizeDifferent") {
    val keys = Array(3.0, 2.0, 5.0, 4.0)
    val values = Array(6.0, 4.0)
    messageOf(DoubleArrayMath.sortPairs(keys, values)) shouldBe SortLengthMessage
    keys.toList shouldBe List(3.0, 2.0, 5.0, 4.0)
    values.toList shouldBe List(6.0, 4.0)
  }

  test("test_sortPairs_doubleObject_1") {
    val keys = Array(3.0, 5.0, 2.0, 4.0)
    val values = boxedInts(6, 10, 4, 8)
    val (sortedKeys, sortedValues) = DoubleArrayMath.sortPairs(keys, values)
    sortedKeys.toList shouldBe List(2.0, 3.0, 4.0, 5.0)
    unboxedInts(sortedValues) shouldBe List(4, 6, 8, 10)
    keys.toList shouldBe List(3.0, 5.0, 2.0, 4.0)
    unboxedInts(values) shouldBe List(6, 10, 4, 8)
    (sortedKeys ne keys) shouldBe true
    (sortedValues ne values) shouldBe true
  }

  test("test_sortPairs_doubleObject_2") {
    val keys = Array(3.0, 2.0, 5.0, 4.0)
    val values = boxedInts(6, 4, 10, 8)
    val (sortedKeys, sortedValues) = DoubleArrayMath.sortPairs(keys, values)
    sortedKeys.toList shouldBe List(2.0, 3.0, 4.0, 5.0)
    unboxedInts(sortedValues) shouldBe List(4, 6, 8, 10)
    keys.toList shouldBe List(3.0, 2.0, 5.0, 4.0)
    unboxedInts(values) shouldBe List(6, 4, 10, 8)
  }

  test("test_sortPairs_doubleObject_sizeDifferent") {
    val keys = Array(3.0, 2.0, 5.0, 4.0)
    val values = boxedInts(6, 4)
    messageOf(DoubleArrayMath.sortPairs(keys, values)) shouldBe SortLengthMessage
    keys.toList shouldBe List(3.0, 2.0, 5.0, 4.0)
    unboxedInts(values) shouldBe List(6, 4)
  }

  test("test_sortPairs_doubleint_1") {
    val keys = Array(3.0, 5.0, 2.0, 4.0)
    val values = Array(6, 10, 4, 8)
    val (sortedKeys, sortedValues) = DoubleArrayMath.sortPairs(keys, values)
    sortedKeys.toList shouldBe List(2.0, 3.0, 4.0, 5.0)
    sortedValues.toList shouldBe List(4, 6, 8, 10)
    keys.toList shouldBe List(3.0, 5.0, 2.0, 4.0)
    values.toList shouldBe List(6, 10, 4, 8)
    (sortedKeys ne keys) shouldBe true
    (sortedValues ne values) shouldBe true
  }

  test("test_sortPairs_doubleint_2") {
    val keys = Array(3.0, 2.0, 5.0, 4.0)
    val values = Array(6, 4, 10, 8)
    val (sortedKeys, sortedValues) = DoubleArrayMath.sortPairs(keys, values)
    sortedKeys.toList shouldBe List(2.0, 3.0, 4.0, 5.0)
    sortedValues.toList shouldBe List(4, 6, 8, 10)
    keys.toList shouldBe List(3.0, 2.0, 5.0, 4.0)
    values.toList shouldBe List(6, 4, 10, 8)
  }

  test("test_sortPairs_doubleint_sizeDifferent") {
    val keys = Array(3.0, 2.0, 5.0, 4.0)
    val values = Array(6, 4)
    messageOf(DoubleArrayMath.sortPairs(keys, values)) shouldBe SortLengthMessage
    keys.toList shouldBe List(3.0, 2.0, 5.0, 4.0)
    values.toList shouldBe List(6, 4)
  }

  //-------------------------------------------------------------------------
  // What the paired sort does differently from the in-place quicksort it replaces.

  test("sortPairs returns two new arrays rather than the two it was given") {
    val keys = Array(3.0, 1.0, 2.0)
    val values = Array(30.0, 10.0, 20.0)
    val (sortedKeys, sortedValues) = DoubleArrayMath.sortPairs(keys, values)
    (sortedKeys ne keys) shouldBe true
    (sortedValues ne values) shouldBe true
    sortedKeys.toList shouldBe List(1.0, 2.0, 3.0)
    sortedValues.toList shouldBe List(10.0, 20.0, 30.0)
    keys.toList shouldBe List(3.0, 1.0, 2.0)
    values.toList shouldBe List(30.0, 10.0, 20.0)
  }

  test("sortPairs is stable, so entries whose keys are equal keep the order they were given in") {
    val (sortedKeys, sortedValues) = DoubleArrayMath.sortPairs(Array(1.0, 1.0, 0.0), Array(10.0, 20.0, 30.0))
    sortedKeys.toList shouldBe List(0.0, 1.0, 1.0)
    sortedValues.toList shouldBe List(30.0, 10.0, 20.0)
  }

  test("sortPairs orders the keys by the total ordering of doubles, so a not-a-number key sorts last") {
    val keys =
      Array(Double.NaN, 0.0, -0.0, 1.0, Double.NegativeInfinity, Double.PositiveInfinity)
    val values = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    val (sortedKeys, sortedValues) = DoubleArrayMath.sortPairs(keys, values)
    assertValues(
      sortedKeys,
      Array(Double.NegativeInfinity, -0.0, 0.0, 1.0, Double.PositiveInfinity, Double.NaN))
    sortedValues.toList shouldBe List(5.0, 3.0, 2.0, 4.0, 6.0, 1.0)
  }

  test("sortPairs keeps a value of any element type with its key") {
    val keys = Array(3.0, 1.0, 2.0)
    val values = Array("three", "one", "two")
    val (sortedKeys, sortedValues) = DoubleArrayMath.sortPairs(keys, values)
    sortedKeys.toList shouldBe List(1.0, 2.0, 3.0)
    sortedValues.toList shouldBe List("one", "two", "three")
    keys.toList shouldBe List(3.0, 1.0, 2.0)
    values.toList shouldBe List("three", "one", "two")
  }

  test("sorting an empty pair of arrays is a pair of empty arrays") {
    val (sortedKeys, sortedValues) =
      DoubleArrayMath.sortPairs(DoubleArrayMath.EMPTY_DOUBLE_ARRAY, DoubleArrayMath.EMPTY_DOUBLE_ARRAY)
    sortedKeys.toList shouldBe List.empty[Double]
    sortedValues.toList shouldBe List.empty[Double]
  }

  //-------------------------------------------------------------------------
  // The lengths and the key shapes the sort itself turns on. It orders a permutation of the
  // indices by merging runs of it and doubling the width of the runs until one run spans the
  // whole array, so what has to be covered is: the lengths too short to merge at all; a length
  // that leaves a run without a partner to merge with, at more than one width; the ties a merge
  // has to resolve in favour of the run that came first, which is what makes the sort stable;
  // and the keys where the total ordering of doubles and the primitive comparison part company.

  test("sorting a single pair answers that pair, with nothing to merge") {
    val (sortedKeys, sortedValues) = DoubleArrayMath.sortPairs(Array(7.5), Array(75.0))
    sortedKeys.toList shouldBe List(7.5)
    sortedValues.toList shouldBe List(75.0)
  }

  test("sorting two pairs orders them whichever way round they were given") {
    val (orderedKeys, orderedValues) = DoubleArrayMath.sortPairs(Array(1.0, 2.0), Array(10.0, 20.0))
    orderedKeys.toList shouldBe List(1.0, 2.0)
    orderedValues.toList shouldBe List(10.0, 20.0)
    val (reversedKeys, reversedValues) = DoubleArrayMath.sortPairs(Array(2.0, 1.0), Array(20.0, 10.0))
    reversedKeys.toList shouldBe List(1.0, 2.0)
    reversedValues.toList shouldBe List(10.0, 20.0)
  }

  test("sorting an odd number of pairs merges the run that has no partner") {
    val keys = Array(5.0, 1.0, 4.0, 2.0, 7.0, 3.0, 6.0)
    val values = Array(50.0, 10.0, 40.0, 20.0, 70.0, 30.0, 60.0)
    val (sortedKeys, sortedValues) = DoubleArrayMath.sortPairs(keys, values)
    sortedKeys.toList shouldBe List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)
    sortedValues.toList shouldBe List(10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0)
    keys.toList shouldBe List(5.0, 1.0, 4.0, 2.0, 7.0, 3.0, 6.0)
    values.toList shouldBe List(50.0, 10.0, 40.0, 20.0, 70.0, 30.0, 60.0)
  }

  test("sorting keys that are all equal keeps every value where it was, at every run width") {
    val values = Array.range(0, 9)
    val (sortedKeys, sortedValues) = DoubleArrayMath.sortPairs(Array.fill(9)(4.0), values)
    sortedKeys.toList shouldBe List.fill(9)(4.0)
    sortedValues.toList shouldBe List.range(0, 9)
  }

  test("sorting keys that are all not-a-number keeps every value where it was") {
    val values = Array.range(0, 5)
    val (sortedKeys, sortedValues) = DoubleArrayMath.sortPairs(Array.fill(5)(Double.NaN), values)
    assertValues(sortedKeys, Array.fill(5)(Double.NaN))
    sortedValues.toList shouldBe List.range(0, 5)
  }

  test("sorting not-a-number keys mixed in among finite ones sorts every one of them last") {
    val keys = Array(Double.NaN, 3.0, Double.NaN, 1.0, 2.0, Double.NaN, -1.0)
    val values = Array(1, 2, 3, 4, 5, 6, 7)
    val (sortedKeys, sortedValues) = DoubleArrayMath.sortPairs(keys, values)
    assertValues(sortedKeys, Array(-1.0, 1.0, 2.0, 3.0, Double.NaN, Double.NaN, Double.NaN))
    sortedValues.toList shouldBe List(7, 4, 5, 2, 1, 3, 6)
  }

  test("sorting orders negative zero before positive zero and keeps each value with its own zero") {
    val keys = Array(0.0, -0.0, 0.0, -0.0)
    val values = Array(1, 2, 3, 4)
    val (sortedKeys, sortedValues) = DoubleArrayMath.sortPairs(keys, values)
    assertValues(sortedKeys, Array(-0.0, -0.0, 0.0, 0.0))
    sortedValues.toList shouldBe List(2, 4, 1, 3)
  }

  test("sorting agrees with the total ordering of doubles at every length from empty up to forty") {
    forAll(SortLengths) { length =>
      assertSortedByTotalOrdering(scrambledKeys(length))
    }
  }

  test("sorting a long array agrees with the total ordering, merging several times over") {
    assertSortedByTotalOrdering(scrambledKeys(1501))
  }

  //-------------------------------------------------------------------------
  // The Java coverage method reached a private constructor by reflection, to satisfy a
  // coverage tool that a utility class cannot be instantiated. The unit under test is an
  // object, so there is no constructor to reach and nothing to reflect over.

  test("coverage") {
    assertDoesNotCompile("new DoubleArrayMath()")
    DoubleArrayMath.EMPTY_DOUBLE_ARRAY.length shouldBe 0
    DoubleArrayMath.EMPTY_DOUBLE_OBJECT_ARRAY.length shouldBe 0
    DoubleArrayMath.sum(DoubleArrayMath.EMPTY_DOUBLE_ARRAY) shouldBe 0.0
    DoubleArrayMath.toPrimitive(DoubleArrayMath.toObject(Array(1.0))).toList shouldBe List(1.0)
  }

  //-------------------------------------------------------------------------
  // Properties over generated arrays. The first is the standing immutability claim: it runs
  // every member that reads an array and compares the inputs against snapshots taken before.

  test("no member modifies the arrays it is given, for any pair of equal-sized arrays") {
    forAll(genDoubleArrayPair) { case (left, right) =>
      val array1 = elementsOf(left)
      val array2 = elementsOf(right)
      val snapshot1 = elementsOf(left)
      val snapshot2 = elementsOf(right)
      val results = List(
        DoubleArrayMath.applyAddition(array1, 2.0),
        DoubleArrayMath.applyMultiplication(array1, 2.0),
        DoubleArrayMath.apply(array1, value => value * value),
        DoubleArrayMath.combineByAddition(array1, array2),
        DoubleArrayMath.combineByMultiplication(array1, array2),
        DoubleArrayMath.combine(array1, array2, (a, b) => a - b),
        DoubleArrayMath.combineLenient(array1, array2, (a, b) => a - b),
        DoubleArrayMath.reorderedCopy(array1, identityPositions(array1.length)))
      results.foreach(result => result.length shouldBe array1.length)
      val (sortedKeys, sortedValues) = DoubleArrayMath.sortPairs(array1, array2)
      sortedKeys.length shouldBe array1.length
      sortedValues.length shouldBe array2.length
      // the other two element types of the paired sort, so that every overload is covered
      val intValues = Array.tabulate(array1.length)(index => index)
      val textValues = Array.tabulate(array1.length)(index => index.toString)
      val (intKeys, sortedIntValues) = DoubleArrayMath.sortPairs(array1, intValues)
      val (textKeys, sortedTextValues) = DoubleArrayMath.sortPairs(array1, textValues)
      intKeys.length shouldBe array1.length
      textKeys.length shouldBe array1.length
      sortedIntValues.toList.sorted shouldBe intValues.toList.sorted
      sortedTextValues.toList.sorted shouldBe textValues.toList.sorted
      intValues.toList shouldBe List.tabulate(array1.length)(index => index)
      textValues.toList shouldBe List.tabulate(array1.length)(index => index.toString)
      DoubleArrayMath.toObject(array1).length shouldBe array1.length
      DoubleArrayMath.toPrimitive(DoubleArrayMath.toObject(array2)).length shouldBe array2.length
      DoubleArrayMath.fuzzyEquals(array1, array2, 1.0) shouldBe DoubleArrayMath.fuzzyEquals(array2, array1, 1.0)
      DoubleArrayMath.fuzzyEqualsZero(array1, 0.0) shouldBe array1.forall(value => value == 0.0)
      java.lang.Double.compare(DoubleArrayMath.sum(array1), DoubleArrayMath.sum(array1)) shouldBe 0
      assertValues(array1, snapshot1)
      assertValues(array2, snapshot2)
      succeed
    }
  }

  test("every operation returns an array that shares nothing with the arrays it was given") {
    forAll(genFiniteDoubleArrayPair) { case (left, right) =>
      val array1 = elementsOf(left)
      val array2 = elementsOf(right)
      val results = List(
        DoubleArrayMath.applyAddition(array1, 0.0),
        DoubleArrayMath.applyMultiplication(array1, 1.0),
        DoubleArrayMath.apply(array1, value => value),
        DoubleArrayMath.combineByAddition(array1, array2),
        DoubleArrayMath.combine(array1, array2, (a, _) => a),
        DoubleArrayMath.combineLenient(array1, array2, (a, _) => a),
        DoubleArrayMath.reorderedCopy(array1, identityPositions(array1.length)))
      results.foreach { result =>
        (result ne array1) shouldBe true
        (result ne array2) shouldBe true
      }
      succeed
    }
  }

  test("an element-wise operation over a generated array computes the operator at every index") {
    forAll(genFiniteDoubleArrayPair) { case (left, right) =>
      val array1 = elementsOf(left)
      val array2 = elementsOf(right)
      DoubleArrayMath.applyAddition(array1, 2.5).toList shouldBe array1.map(value => value + 2.5).toList
      DoubleArrayMath.applyMultiplication(array1, 2.5).toList shouldBe array1.map(value => value * 2.5).toList
      DoubleArrayMath.apply(array1, value => value * value).toList shouldBe
        array1.map(value => value * value).toList
      DoubleArrayMath.combineByAddition(array1, array2).toList shouldBe
        array1.toList.zip(array2.toList).map { case (a, b) => a + b }
      DoubleArrayMath.combineByMultiplication(array1, array2).toList shouldBe
        array1.toList.zip(array2.toList).map { case (a, b) => a * b }
    }
  }

  test("the sum of a generated array is the running total taken in index order") {
    forAll(genFiniteDoubleArray) { array =>
      val values = elementsOf(array)
      DoubleArrayMath.sum(values) shouldBe values.foldLeft(0.0)((total, value) => total + value)
    }
  }

  test("a strict combination of generated arrays of different lengths is rejected") {
    forAll(genFiniteDoubleArray) { array =>
      val values = elementsOf(array)
      messageOf(DoubleArrayMath.combineByAddition(values, values :+ 1.0)) shouldBe CombineLengthMessage
      messageOf(DoubleArrayMath.combineByMultiplication(values :+ 1.0, values)) shouldBe CombineLengthMessage
    }
  }

  test("a lenient combination of generated arrays takes the overlap from both and the rest from the longer") {
    forAll(genFiniteDoubleArray, genFiniteDoubleArray) { (left, right) =>
      val array1 = elementsOf(left)
      val array2 = elementsOf(right)
      val combined = DoubleArrayMath.combineLenient(array1, array2, (a, b) => a + b)
      val overlap = math.min(array1.length, array2.length)
      val longer = if (array1.length >= array2.length) array1 else array2
      combined.length shouldBe math.max(array1.length, array2.length)
      combined.take(overlap).toList shouldBe
        array1.take(overlap).toList.zip(array2.take(overlap).toList).map { case (a, b) => a + b }
      combined.drop(overlap).toList shouldBe longer.drop(overlap).toList
    }
  }

  test("the comparison of a generated array with itself holds exactly when no element is not-a-number") {
    forAll(genDoubleArray) { array =>
      val values = elementsOf(array)
      val expected = !values.exists(value => value.isNaN)
      DoubleArrayMath.fuzzyEquals(values, values, 0.0) shouldBe expected
      DoubleArrayMath.fuzzyEquals(values, elementsOf(array), 1.0) shouldBe expected
      DoubleArrayMath.fuzzyEquals(values, values :+ 1.0, 1.0) shouldBe false
    }
  }

  test("the scalar comparison is reflexive for every generated value that is not not-a-number, and for no other") {
    forAll(genDouble) { value =>
      val expected = !value.isNaN
      DoubleArrayMath.fuzzyEquals(value, value, 0.0) shouldBe expected
      DoubleArrayMath.fuzzyEquals(value, value, 1.0e-8) shouldBe expected
    }
  }

  test("the scalar comparison is symmetric for every generated pair of values") {
    forAll(genDouble, genDouble) { (a, b) =>
      DoubleArrayMath.fuzzyEquals(a, b, 1.0e-8) shouldBe DoubleArrayMath.fuzzyEquals(b, a, 1.0e-8)
      DoubleArrayMath.fuzzyEquals(a, b, 0.0) shouldBe DoubleArrayMath.fuzzyEquals(b, a, 0.0)
    }
  }

  test("a generated value is an integer in the mathematical sense exactly when its decimal expansion is whole") {
    forAll(genDouble) { value =>
      val expected =
        if (value.isNaN || value.isInfinite) {
          false
        } else {
          new java.math.BigDecimal(value).stripTrailingZeros.scale <= 0
        }
      DoubleArrayMath.isMathematicalInteger(value) shouldBe expected
    }
  }

  test("sorting a generated pair orders the keys and keeps every value with its key") {
    forAll(genFiniteDoubleArrayPair) { case (left, right) =>
      val keys = elementsOf(left)
      val values = elementsOf(right)
      val (sortedKeys, sortedValues) = DoubleArrayMath.sortPairs(keys, values)
      val expected = keys.toList.zip(values.toList).sortBy(entry => entry._1)(Ordering.Double.TotalOrdering)
      sortedKeys.toList shouldBe expected.map(entry => entry._1)
      sortedValues.toList shouldBe expected.map(entry => entry._2)
    }
  }

  test("sorting a generated array of keys agrees with the total ordering of doubles") {
    forAll(genSortKeys) { keys =>
      assertSortedByTotalOrdering(keys)
    }
  }

  test("reordering a generated array by a permutation of its indices permutes its elements") {
    forAll(genDoubleArray) { array =>
      val values = elementsOf(array)
      val reversed = DoubleArrayMath.reorderedCopy(values, identityPositions(values.length).reverse)
      assertValues(reversed, values.reverse)
      assertValues(DoubleArrayMath.reorderedCopy(reversed, identityPositions(values.length).reverse), values)
      succeed
    }
  }

  //-------------------------------------------------------------------------
  // Fixture helpers for the boxed element type of the generic paired sort.

  private def boxedInts(values: Int*): Array[java.lang.Integer] =
    values.map(value => java.lang.Integer.valueOf(value)).toArray

  private def unboxedInts(values: Array[java.lang.Integer]): List[Int] =
    values.iterator.map(value => value.intValue).toList

  //-------------------------------------------------------------------------
  // Fixtures and the expectation of the paired sort. The sort orders a permutation of the
  // indices by merging runs of it, so the cases it turns on are lengths and ties rather than
  // values, and the expectation below is computed independently of it at every length.

  /** Every length the run widths of the sort turn on, from empty up to forty. */
  private val SortLengths = Table("length", (0 to 40).toList: _*)

  /**
   * A deterministic array of keys of the given length.
   *
   * The keys recur on a cycle that shares no factor with the powers of two the run widths of the
   * sort take, so that ties are merged at every width and a run boundary falls in a different
   * place at every length, and one position in eleven carries one of the IEEE-754 values the
   * total ordering of doubles is held to. The sequence is computed rather than generated so that
   * a failure names an array that can be reproduced exactly.
   */
  private def scrambledKeys(length: Int): Array[Double] =
    Array.tabulate(length)(index => scrambledKey(index))

  /** The key at one position of [[scrambledKeys]]. */
  private def scrambledKey(index: Int): Double =
    index % 11 match {
      case 3 => Double.NaN
      case 5 => Double.PositiveInfinity
      case 7 => Double.NegativeInfinity
      case 9 => if (index % 22 == 9) -0.0 else 0.0
      case _ => ((index * 7919) % 13).toDouble - 6.0
    }

  /**
   * Generates keys for the sort property, at every length from empty up to forty.
   *
   * Most elements come from the generator that mixes ordinary values with the IEEE-754 edges,
   * and the rest from a handful of values that recur, so that a generated array of any length
   * holds ties for the merge to resolve as well as the edges the ordering is held to.
   */
  private val genSortKeys: Gen[Array[Double]] =
    for {
      length <- Gen.choose(0, 40)
      keys <- Gen.listOfN(length, Gen.frequency(3 -> genDouble, 2 -> Gen.oneOf(-1.0, 0.0, 1.0, 2.0)))
    } yield keys.toArray

  /**
   * Asserts that the paired sort of the given keys is the one the total ordering of doubles
   * defines, for a value array of each of the three element types, and that nothing was modified.
   *
   * The expectation is computed here rather than by the object under test: each key is tagged
   * with the position it was given in, and that pairing is sorted by key under
   * `Ordering.Double.TotalOrdering` by the library's own stable sort, which fixes the order of
   * the keys and, through the positions, the order of the values among keys that compare equal.
   * The values are the positions themselves, in each of the three element types, so a value that
   * failed to follow its key names the position it came from. The keys are compared by their
   * bits, because a not-a-number key and the two signed zeroes are the cases the ordering is
   * being held to and ordinary comparison has no answer for either.
   */
  private def assertSortedByTotalOrdering(keys: Array[Double]): Assertion = {
    val expected = keys.zipWithIndex.sortBy(entry => entry._1)(Ordering.Double.TotalOrdering)
    val expectedKeys = expected.map(entry => entry._1)
    val expectedPositions = expected.map(entry => entry._2).toList
    val keysSnapshot = keys.clone()
    val doubleValues = Array.tabulate(keys.length)(index => index.toDouble)
    val intValues = Array.range(0, keys.length)
    val textValues = Array.tabulate(keys.length)(index => index.toString)
    val (doubleKeys, sortedDoubles) = DoubleArrayMath.sortPairs(keys, doubleValues)
    val (intKeys, sortedInts) = DoubleArrayMath.sortPairs(keys, intValues)
    val (textKeys, sortedTexts) = DoubleArrayMath.sortPairs(keys, textValues)
    assertValues(doubleKeys, expectedKeys)
    assertValues(intKeys, expectedKeys)
    assertValues(textKeys, expectedKeys)
    sortedDoubles.toList shouldBe expectedPositions.map(position => position.toDouble)
    sortedInts.toList shouldBe expectedPositions
    sortedTexts.toList shouldBe expectedPositions.map(position => position.toString)
    doubleValues.toList shouldBe List.tabulate(keys.length)(index => index.toDouble)
    intValues.toList shouldBe List.range(0, keys.length)
    textValues.toList shouldBe List.tabulate(keys.length)(index => index.toString)
    assertValues(keys, keysSnapshot)
    succeed
  }
}
