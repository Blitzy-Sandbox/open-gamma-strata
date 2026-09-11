/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import scala.annotation.tailrec

/**
 * Arithmetic and comparison over primitive arrays of doubles.
 *
 * This object is the low-level numeric toolkit of the library. Every member reads the
 * arrays it is given and returns a fresh result, so an operation can be applied to a value
 * that the caller goes on using afterwards:
 *
 * {{{
 * val rates = Array(0.01, 0.02, 0.03)
 * val shifted = DoubleArrayMath.applyAddition(rates, 0.005)  // rates is unchanged
 * }}}
 *
 * The immutable array wrappers of this library - and through them the whole of the domain
 * layer - are built on these operations, which is why they stay as close to the machine as
 * the language allows: the arrays are primitive, the result of an operation is allocated
 * once and filled in index order, and no element is ever boxed.
 *
 * ===Copying, never mutating===
 *
 * The Java class that this is ported from offered each element-wise operation twice, once
 * returning a new array and once writing back into the array it was given. Only the
 * copying half is ported. A helper that writes into an array supplied by its caller changes
 * that value underneath every other holder of the same reference, which no immutable
 * wrapper built on this object could then defend against. The five members concerned - the
 * two that added a constant or another array in place, the two that multiplied in place,
 * and the one that applied an operator in place - therefore have no counterpart here;
 * `applyAddition`, `applyMultiplication`, `apply`, `combineByAddition`,
 * `combineByMultiplication` and `combine` compute exactly the same values and hand back a
 * new array.
 *
 * The paired sort is ported in the same spirit. Its Java form sorted both of the arrays it
 * was given in place with a dual-array quicksort; `sortPairs` here returns the sorted keys
 * and the correspondingly reordered values as a pair and leaves its inputs untouched.
 * Because it sorts a permutation of indices instead of swapping elements it is also
 * stable, which the quicksort was not - observable only between entries whose keys are
 * equal, where the original relative order is now retained.
 *
 * ===Comparing doubles within a tolerance===
 *
 * `fuzzyEquals` and `isMathematicalInteger` are available for scalar values as well as for
 * arrays. The Java original delegated the scalar comparison to an external numeric helper
 * that this port does not depend on; defining the comparison here is what keeps this
 * module's arithmetic resting on nothing but the standard library, and the domain layer
 * calls the scalar forms directly - a conversion rate is compared with one, and an amount
 * chooses between an integral and a fractional text form with the other.
 *
 * The comparison is defined over three disjoint cases, taken in this order, and every edge
 * of it follows from them:
 *
 *   - a not-a-number value is equal to nothing, itself included, at every tolerance. It
 *     has no distance from any value, so no tolerance can bring it to one;
 *   - where either value is infinite the two are equal only when they are the same
 *     infinity, at every tolerance ''including'' an infinite one. So positive infinity is
 *     not equal to negative infinity, and no infinity is equal to any finite value, at any
 *     tolerance;
 *   - two finite values are equal when they are no further apart than the tolerance, which
 *     is the ordinary case. Negative zero and positive zero are no distance apart and are
 *     therefore equal at every tolerance, and two finite values any distance apart are
 *     equal at an infinite tolerance.
 *
 * ===The tolerance precondition===
 *
 * A tolerance that is negative, or that is not a number, is a caller error rather than a
 * comparison that answers `false`, and the replaced helper rejected both. Each of the three
 * comparison members checks the tolerance through `ArgCheck` before comparing anything,
 * rejecting a not-a-number tolerance first and a negative one second, so each is reported
 * in its own words. Negative zero is not a negative tolerance and is accepted; under it
 * two finite values are compared exactly. One difference follows from checking the
 * tolerance once, up front, rather than once per element as the delegating original did:
 *
 *   - an empty array, and a pair of arrays of different lengths, report a bad tolerance as
 *     well, where the original returned its answer without ever reaching the per-element
 *     check.
 *
 * It is recorded in the migration note. Every other failure is that of the Java
 * original, message included: arrays of different lengths can neither be combined nor
 * sorted, and a reordering position has to index the array being reordered. All of them
 * state a caller contract rather than a property of the data, so each is reported by a
 * fail-fast check from `ArgCheck` - the one place in this library where a failure is
 * thrown instead of returned.
 *
 * ===Ordering===
 *
 * `sortPairs` orders keys by the total ordering of doubles rather than by the primitive
 * comparison of the Java quicksort. The two agree on every array of ordinary numbers and
 * differ only where a primitive comparison has no answer: a not-a-number key sorts last
 * here instead of landing wherever the partitioning left it, and negative zero sorts
 * before positive zero instead of comparing equal to it. A total order is what makes the
 * result of a stable sort of arbitrary keys a function of its input.
 *
 * ===Thread safety===
 *
 * Every member is a pure function of its arguments and this object holds no state, so it
 * is safe to use from any number of threads. The two empty-array constants are shared, and
 * safely so: an array of length zero has no element to read or to change, so exposing one
 * grants no ability to mutate anything.
 */
object DoubleArrayMath {

  /**
   * An empty array of doubles.
   *
   * This is returned wherever an operation has no elements to produce, so that the common
   * empty result costs no allocation.
   */
  val EMPTY_DOUBLE_ARRAY: Array[Double] = new Array[Double](0)

  /**
   * An empty array of boxed doubles.
   *
   * The counterpart of `EMPTY_DOUBLE_ARRAY` for the boxed element type, returned by
   * `toObject` for an empty input.
   */
  val EMPTY_DOUBLE_OBJECT_ARRAY: Array[java.lang.Double] = new Array[java.lang.Double](0)

  //-------------------------------------------------------------------------
  /**
   * Converts an array of doubles to an array of boxed doubles.
   *
   * Each element is boxed individually, so the result is as long as the input and holds
   * the same values in the same order. This member and `toPrimitive` are the bridge
   * between the primitive arithmetic of this object and an interface that can only carry
   * references; the arithmetic itself never crosses that bridge.
   *
   * @param array  the array to convert
   * @return the converted array
   */
  def toObject(array: Array[Double]): Array[java.lang.Double] =
    if (array.length == 0) {
      EMPTY_DOUBLE_OBJECT_ARRAY
    } else {
      val result = new Array[java.lang.Double](array.length)
      boxInto(result, array, 0)
      result
    }

  // boxes the elements from the index upwards into the result, in index order
  //
  // Boxing each value is the whole point of the conversion and is the only thing this does: the
  // index stays a primitive integer, where tabulating the result through a function of the index
  // would box that as well.
  @tailrec
  private def boxInto(result: Array[java.lang.Double], array: Array[Double], index: Int): Unit =
    if (index < array.length) {
      result(index) = java.lang.Double.valueOf(array(index))
      boxInto(result, array, index + 1)
    }

  /**
   * Converts an array of boxed doubles to an array of doubles.
   *
   * Each element is unboxed individually, so the result is as long as the input and holds
   * the same values in the same order. Every element of the input is required to refer to
   * an actual boxed value: an element referring to nothing is a caller error, which this
   * port neither produces nor tests for, and which surfaces as a failure from the runtime
   * at the element that caused it.
   *
   * @param array  the array to convert
   * @return the converted array
   */
  def toPrimitive(array: Array[java.lang.Double]): Array[Double] =
    if (array.length == 0) {
      EMPTY_DOUBLE_ARRAY
    } else {
      val result = new Array[Double](array.length)
      unboxInto(result, array, 0)
      result
    }

  // unboxes the elements from the index upwards into the result, in index order
  //
  // Reading each value out of its box is the whole point of the conversion and is the only
  // thing this does: the index stays a primitive integer and the value is written straight into
  // the primitive result, where tabulating the result through a function of the index would box
  // the index, box the value the function returned and read it out of that box again.
  @tailrec
  private def unboxInto(result: Array[Double], array: Array[java.lang.Double], index: Int): Unit =
    if (index < array.length) {
      result(index) = array(index).doubleValue()
      unboxInto(result, array, index + 1)
    }

  //-------------------------------------------------------------------------
  /**
   * Calculates the sum total of all the elements in the array.
   *
   * The elements are added in index order, starting from zero, and an empty array sums to
   * zero. The order is part of the answer rather than an implementation detail, because
   * addition of doubles is not associative: a sum computed in another order, however
   * evenly it were split, would differ in its last digits from the value this library is
   * required to reproduce. No compensated or reordered summation is used for that reason.
   *
   * The input array is not modified.
   *
   * @param array  the array to sum
   * @return the sum total of all the elements
   */
  def sum(array: Array[Double]): Double =
    sumFrom(array, 0, 0.0)

  // adds the elements from the index upwards to the running total, in index order
  @tailrec
  private def sumFrom(array: Array[Double], index: Int, total: Double): Double =
    if (index >= array.length) {
      total
    } else {
      sumFrom(array, index + 1, total + array(index))
    }

  //-------------------------------------------------------------------------
  /**
   * Applies an addition to each element in the array, returning a new array.
   *
   * The result is always a new array. The input array is not modified.
   *
   * @param array  the input array, not modified
   * @param valueToAdd  the value to add
   * @return the resulting array
   */
  def applyAddition(array: Array[Double], valueToAdd: Double): Array[Double] = {
    val result = new Array[Double](array.length)
    addInto(result, array, valueToAdd, 0)
    result
  }

  @tailrec
  private def addInto(result: Array[Double], array: Array[Double], valueToAdd: Double, index: Int): Unit =
    if (index < array.length) {
      result(index) = array(index) + valueToAdd
      addInto(result, array, valueToAdd, index + 1)
    }

  /**
   * Applies a multiplication to each element in the array, returning a new array.
   *
   * The result is always a new array. The input array is not modified.
   *
   * @param array  the input array, not modified
   * @param valueToMultiplyBy  the value to multiply by
   * @return the resulting array
   */
  def applyMultiplication(array: Array[Double], valueToMultiplyBy: Double): Array[Double] = {
    val result = new Array[Double](array.length)
    multiplyInto(result, array, valueToMultiplyBy, 0)
    result
  }

  @tailrec
  private def multiplyInto(
      result: Array[Double],
      array: Array[Double],
      valueToMultiplyBy: Double,
      index: Int): Unit =

    if (index < array.length) {
      result(index) = array(index) * valueToMultiplyBy
      multiplyInto(result, array, valueToMultiplyBy, index + 1)
    }

  /**
   * Applies an operator to each element in the array, returning a new array.
   *
   * The operator is a plain function from one double to another, so it can be written as a
   * lambda at the call site and is applied to each element in index order:
   *
   * {{{
   * val squared = DoubleArrayMath.apply(values, x => x * x)
   * }}}
   *
   * The result is always a new array. The input array is not modified.
   *
   * @param array  the input array, not modified
   * @param operator  the operator to use
   * @return the resulting array
   */
  def apply(array: Array[Double], operator: Double => Double): Array[Double] = {
    val result = new Array[Double](array.length)
    applyInto(result, array, operator, 0)
    result
  }

  @tailrec
  private def applyInto(
      result: Array[Double],
      array: Array[Double],
      operator: Double => Double,
      index: Int): Unit =

    if (index < array.length) {
      result(index) = operator(array(index))
      applyInto(result, array, operator, index + 1)
    }

  //-------------------------------------------------------------------------
  /**
   * Combines two arrays, returning an array where each element is the sum of the two
   * matching inputs.
   *
   * Each element of the result is the sum of the elements at the same index in the two
   * inputs, which must therefore have the same length:
   *
   * {{{
   * val array1 = Array(1.0, 5.0, 9.0)
   * val array2 = Array(2.0, 3.0, 2.0)
   * DoubleArrayMath.combineByAddition(array1, array2)  // Array(3.0, 8.0, 11.0)
   * }}}
   *
   * The result is always a new array. The input arrays are not modified.
   *
   * @param array1  the first array
   * @param array2  the second array
   * @return an array combining the two input arrays using the plus operator
   * @throws IllegalArgumentException if the arrays differ in length
   */
  def combineByAddition(array1: Array[Double], array2: Array[Double]): Array[Double] =
    combine(array1, array2, (a, b) => a + b)

  /**
   * Combines two arrays, returning an array where each element is the multiplication of
   * the two matching inputs.
   *
   * Each element of the result is the product of the elements at the same index in the two
   * inputs, which must therefore have the same length:
   *
   * {{{
   * val array1 = Array(1.0, 5.0, 9.0)
   * val array2 = Array(2.0, 3.0, 4.0)
   * DoubleArrayMath.combineByMultiplication(array1, array2)  // Array(2.0, 15.0, 36.0)
   * }}}
   *
   * The result is always a new array. The input arrays are not modified.
   *
   * @param array1  the first array
   * @param array2  the second array
   * @return an array combining the two input arrays using the multiply operator
   * @throws IllegalArgumentException if the arrays differ in length
   */
  def combineByMultiplication(array1: Array[Double], array2: Array[Double]): Array[Double] =
    combine(array1, array2, (a, b) => a * b)

  /**
   * Combines two arrays, returning an array where each element is the combination of the
   * two matching inputs.
   *
   * Each element of the result is the operator applied to the elements at the same index
   * in the two inputs, which must therefore have the same length. Use `combineLenient`
   * where the lengths may legitimately differ.
   *
   * The result is always a new array. The input arrays are not modified.
   *
   * @param array1  the first array
   * @param array2  the second array
   * @param operator  the operator to use when combining values
   * @return an array combining the two input arrays using the operator
   * @throws IllegalArgumentException if the arrays differ in length
   */
  def combine(
      array1: Array[Double],
      array2: Array[Double],
      operator: (Double, Double) => Double): Array[Double] = {

    val length = combinedLength(array1, array2)
    val result = new Array[Double](length)
    combineInto(result, array1, array2, operator, 0, length)
    result
  }

  /**
   * Combines two arrays, returning an array where each element is the combination of the
   * two matching inputs, tolerating inputs of different lengths.
   *
   * Each element of the result up to the length of the shorter input is the operator
   * applied to the elements at the same index in both inputs. The result has the length of
   * the longer input, and beyond the shorter one it carries the elements of the longer
   * input unchanged - the operator is not applied there, because it has only one operand:
   *
   * {{{
   * val array1 = Array(1.0, 2.0)
   * val array2 = Array(3.0)
   * DoubleArrayMath.combineLenient(array1, array2, (a, b) => a / b)  // Array(1.0 / 3.0, 2.0)
   * }}}
   *
   * The result is always a new array. The input arrays are not modified.
   *
   * @param array1  the first array
   * @param array2  the second array
   * @param operator  the operator to use when combining values
   * @return an array combining the two input arrays using the operator
   */
  def combineLenient(
      array1: Array[Double],
      array2: Array[Double],
      operator: (Double, Double) => Double): Array[Double] = {

    val len1 = array1.length
    val len2 = array2.length
    if (len1 == len2) {
      combine(array1, array2, operator)
    } else {
      val size = math.max(len1, len2)
      val result = new Array[Double](size)
      combineLenientInto(result, array1, array2, operator, 0, size)
      result
    }
  }

  // the shared length of two arrays, reporting a caller error when they differ
  private def combinedLength(array1: Array[Double], array2: Array[Double]): Int = {
    ArgCheck.isTrue(array1.length == array2.length, "Arrays cannot be combined as they differ in length")
    array1.length
  }

  @tailrec
  private def combineInto(
      result: Array[Double],
      array1: Array[Double],
      array2: Array[Double],
      operator: (Double, Double) => Double,
      index: Int,
      length: Int): Unit =

    if (index < length) {
      result(index) = operator(array1(index), array2(index))
      combineInto(result, array1, array2, operator, index + 1, length)
    }

  @tailrec
  private def combineLenientInto(
      result: Array[Double],
      array1: Array[Double],
      array2: Array[Double],
      operator: (Double, Double) => Double,
      index: Int,
      size: Int): Unit =

    if (index < size) {
      result(index) =
        if (index >= array1.length) {
          array2(index)
        } else if (index >= array2.length) {
          array1(index)
        } else {
          operator(array1(index), array2(index))
        }
      combineLenientInto(result, array1, array2, operator, index + 1, size)
    }

  //-------------------------------------------------------------------------
  /**
   * Compares two values within a tolerance.
   *
   * Two finite values are equal when they are no further apart than the tolerance, which
   * makes both zeroes equal. An infinity is equal only to the same infinity, at every
   * tolerance including an infinite one, and a not-a-number value is equal to nothing at
   * all, itself included. The class-level documentation sets out the full edge behaviour
   * and why it is what it is.
   *
   * This member, together with `isMathematicalInteger`, replaces the scalar comparison
   * that the Java original delegated to an external numeric helper.
   *
   * @param a  the first value
   * @param b  the second value
   * @param tolerance  the tolerance to use, zero or greater
   * @return true if the two values are effectively equal
   * @throws IllegalArgumentException if the tolerance is negative or is not a number
   */
  def fuzzyEquals(a: Double, b: Double, tolerance: Double): Boolean = {
    ArgCheck.notNaN(tolerance, "tolerance")
    ArgCheck.notNegative(tolerance, "tolerance")
    fuzzyEqualsUnchecked(a, b, tolerance)
  }

  /**
   * Checks whether a value is an integer in the mathematical sense.
   *
   * A value qualifies when it is finite and has no fractional part, so both zeroes
   * qualify, every finite value large enough to have no fractional digits left qualifies,
   * and the infinities and a not-a-number value do not. The domain layer uses this to
   * decide how to render an amount, printing a whole number where the value is integral
   * and the full decimal form otherwise; that decision is made on the value alone, with no
   * regard for the range of any integral type it may afterwards be converted to.
   *
   * @param x  the value to check
   * @return true if the value is finite and has no fractional part
   */
  def isMathematicalInteger(x: Double): Boolean =
    !java.lang.Double.isNaN(x) && !java.lang.Double.isInfinite(x) && x == math.floor(x)

  /**
   * Compares each element in the array to zero within a tolerance.
   *
   * An empty array is effectively zero and returns true.
   *
   * The input array is not modified.
   *
   * @param array  the array to check
   * @param tolerance  the tolerance to use, zero or greater
   * @return true if the array is effectively equal to zero
   * @throws IllegalArgumentException if the tolerance is negative or is not a number
   */
  def fuzzyEqualsZero(array: Array[Double], tolerance: Double): Boolean = {
    ArgCheck.notNaN(tolerance, "tolerance")
    ArgCheck.notNegative(tolerance, "tolerance")
    allFuzzyEqualsZero(array, tolerance, 0)
  }

  /**
   * Compares each element in the first array to the matching index in the second array
   * within a tolerance.
   *
   * Arrays that differ in length are not equal, and that is reported as `false` rather
   * than as an error: unlike `combine`, this member asks a question about the two arrays
   * instead of computing a result from them, and two arrays of different lengths are a
   * perfectly good answer of no.
   *
   * The input arrays are not modified.
   *
   * @param array1  the first array to check
   * @param array2  the second array to check
   * @param tolerance  the tolerance to use, zero or greater
   * @return true if the arrays are effectively equal
   * @throws IllegalArgumentException if the tolerance is negative or is not a number
   */
  def fuzzyEquals(array1: Array[Double], array2: Array[Double], tolerance: Double): Boolean = {
    ArgCheck.notNaN(tolerance, "tolerance")
    ArgCheck.notNegative(tolerance, "tolerance")
    array1.length == array2.length && allFuzzyEquals(array1, array2, tolerance, 0)
  }

  // The comparison itself, with the tolerance already checked by the caller. The three
  // cases are disjoint and are taken in this order: a not-a-number value has no distance
  // from anything and is equal to nothing, itself included; an infinity is equal only to
  // the same infinity, whatever the tolerance, which an unguarded distance would get wrong
  // at an infinite one; and two finite values are equal within the tolerance, which covers
  // the two zeroes because their distance is zero.
  private def fuzzyEqualsUnchecked(a: Double, b: Double, tolerance: Double): Boolean =
    if (java.lang.Double.isNaN(a) || java.lang.Double.isNaN(b)) {
      false
    } else if (java.lang.Double.isInfinite(a) || java.lang.Double.isInfinite(b)) {
      a == b
    } else {
      math.abs(a - b) <= tolerance
    }

  @tailrec
  private def allFuzzyEqualsZero(array: Array[Double], tolerance: Double, index: Int): Boolean =
    if (index >= array.length) {
      true
    } else if (!fuzzyEqualsUnchecked(array(index), 0.0, tolerance)) {
      false
    } else {
      allFuzzyEqualsZero(array, tolerance, index + 1)
    }

  @tailrec
  private def allFuzzyEquals(
      array1: Array[Double],
      array2: Array[Double],
      tolerance: Double,
      index: Int): Boolean =

    if (index >= array1.length) {
      true
    } else if (!fuzzyEqualsUnchecked(array1(index), array2(index), tolerance)) {
      false
    } else {
      allFuzzyEquals(array1, array2, tolerance, index + 1)
    }

  //-------------------------------------------------------------------------
  /**
   * Returns a copy of the first array in the order defined by the position values of the
   * second array.
   *
   * The two arrays must be the same length. The element of the result at each index is the
   * element of the values array at the position named by the positions array at that
   * index:
   *
   * {{{
   * val values = Array(1.0, 5.0, 10.0)
   * val positions = Array(2, 0, 1)
   * DoubleArrayMath.reorderedCopy(values, positions)  // Array(10.0, 1.0, 5.0)
   * }}}
   *
   * As in the Java original, the positions are not checked for duplicates: a position may
   * appear any number of times, and the corresponding value is then repeated in the
   * result. Each position is checked for being an index of the values array.
   *
   * The result is a new array. The input arrays are not modified.
   *
   * @param values  the array of values
   * @param positions  the array of positions
   * @return the reordered copy
   * @throws IllegalArgumentException if the arrays differ in length, or if any of the
   *   positions does not correspond to an index in the values
   */
  def reorderedCopy(values: Array[Double], positions: Array[Int]): Array[Double] = {
    val length = positions.length
    ArgCheck.isTrue(length == values.length, "Value array cannot be reordered as they differ in length")
    val result = new Array[Double](length)
    reorderInto(result, values, positions, 0, length)
    result
  }

  @tailrec
  private def reorderInto(
      result: Array[Double],
      values: Array[Double],
      positions: Array[Int],
      index: Int,
      length: Int): Unit =

    if (index < length) {
      val key = positions(index)
      ArgCheck.inRange(key, 0, length, "key")
      result(index) = values(key)
      reorderInto(result, values, positions, index + 1, length)
    }

  //-------------------------------------------------------------------------
  /**
   * Sorts a pair of arrays by the first, keeping each value with its key.
   *
   * The two arrays must be the same length and represent a mapping of key to value. The
   * keys are sorted into ascending order and each value moves with the key it belonged to:
   *
   * {{{
   * val (keys, values) = DoubleArrayMath.sortPairs(Array(3.0, 1.0, 2.0), Array(30.0, 10.0, 20.0))
   * // keys   == Array(1.0, 2.0, 3.0)
   * // values == Array(10.0, 20.0, 30.0)
   * }}}
   *
   * The result is a pair of new arrays and the inputs are not modified, where the Java
   * original sorted both of the arrays it was given in place; the sort is also stable, so
   * entries with equal keys keep the order they were given in. Both differences are
   * recorded in the migration note.
   *
   * @param keys  the array of keys to sort
   * @param values  the array of associated values to retain
   * @return the sorted keys and the values reordered to match
   * @throws IllegalArgumentException if the arrays differ in length
   */
  def sortPairs(keys: Array[Double], values: Array[Double]): (Array[Double], Array[Double]) = {
    val order = sortedOrder(keys, values.length)
    (selectDoubles(keys, order), selectDoubles(values, order))
  }

  /**
   * Sorts a pair of arrays by the first, keeping each integer value with its key.
   *
   * This behaves exactly as the form taking two arrays of doubles, and the same notes on
   * purity and stability apply.
   *
   * @param keys  the array of keys to sort
   * @param values  the array of associated values to retain
   * @return the sorted keys and the values reordered to match
   * @throws IllegalArgumentException if the arrays differ in length
   */
  def sortPairs(keys: Array[Double], values: Array[Int]): (Array[Double], Array[Int]) = {
    val order = sortedOrder(keys, values.length)
    (selectDoubles(keys, order), selectInts(values, order))
  }

  /**
   * Sorts an array of keys and an array of values of any type by the keys, keeping each
   * value with its key.
   *
   * This behaves exactly as the form taking two arrays of doubles, and the same notes on
   * purity and stability apply. The result array is obtained by copying the array of
   * values, which yields an array of the same element type without asking the caller for a
   * runtime description of that type, and every element of the copy is then replaced.
   *
   * @tparam V  the type of the values
   * @param keys  the array of keys to sort
   * @param values  the array of associated values to retain
   * @return the sorted keys and the values reordered to match
   * @throws IllegalArgumentException if the arrays differ in length
   */
  def sortPairs[V](keys: Array[Double], values: Array[V]): (Array[Double], Array[V]) = {
    val order = sortedOrder(keys, values.length)
    (selectDoubles(keys, order), selectValues(values, order))
  }

  // the indices of the keys in ascending order of key, checking that the pair match in length
  //
  // The permutation is sorted rather than the keys themselves, which is what lets every value
  // type follow its key and what makes the sort stable. It is sorted here by a bottom-up merge
  // sort written over the primitive index array directly: the indices are integers and the
  // comparison is between two doubles, so sorting them through a general-purpose ordering would
  // box each index at every comparison, allocate the ordering and the mapping function, and copy
  // the permutation more than once. This form allocates the permutation and one merge buffer of
  // the same length, and nothing else, whatever the length of the input.
  //
  // A merge sort is what a stable result asks for - runs are merged rather than elements
  // exchanged, and a tie is resolved by taking from the earlier run - and it is the same
  // O(n log n) as the quicksort of the Java original.
  private def sortedOrder(keys: Array[Double], valuesLength: Int): Array[Int] = {
    ArgCheck.isTrue(keys.length == valuesLength, "Arrays cannot be sorted as they differ in length")
    val order = new Array[Int](keys.length)
    identityInto(order, 0)
    if (order.length > 1) {
      mergePasses(order, new Array[Int](order.length), keys, 1L)
    }
    order
  }

  // fills the order with the identity permutation, so that index zero holds zero and so on
  @tailrec
  private def identityInto(order: Array[Int], index: Int): Unit =
    if (index < order.length) {
      order(index) = index
      identityInto(order, index + 1)
    }

  // merges every adjacent pair of runs of the given width into the buffer, copies the result
  // back over the order and repeats with the runs twice as wide, until one run spans the whole
  // order and it is therefore sorted
  //
  // The width is carried as a `Long` because doubling an `Int` width overflows into a negative
  // number once it passes half of the integer range, which would leave the doubling unable to
  // reach the length of an array that large and the merging unable to terminate. Every
  // comparison and conversion between the two widths is written out for that reason.
  @tailrec
  private def mergePasses(order: Array[Int], buffer: Array[Int], keys: Array[Double], width: Long): Unit =
    if (width < order.length.toLong) {
      mergeRunsFrom(order, buffer, keys, width, 0)
      System.arraycopy(buffer, 0, order, 0, order.length)
      mergePasses(order, buffer, keys, width * 2L)
    }

  // merges the pair of runs starting at the given position into the buffer and moves on to the
  // next pair, up to the end of the order
  //
  // A final run without a partner is merged with an empty one, which copies it into the buffer
  // unchanged and keeps the buffer a permutation of the order at the end of every pass.
  @tailrec
  private def mergeRunsFrom(
      order: Array[Int],
      buffer: Array[Int],
      keys: Array[Double],
      width: Long,
      start: Int): Unit =

    if (start < order.length) {
      val middle = boundedIndex(order.length, start.toLong + width)
      val end = boundedIndex(order.length, start.toLong + width + width)
      mergeRunInto(order, buffer, keys, start, start, middle, middle, end)
      mergeRunsFrom(order, buffer, keys, width, end)
    }

  // the given run boundary, or the length of the array where the boundary is beyond its end
  private def boundedIndex(length: Int, boundary: Long): Int =
    if (boundary < length.toLong) boundary.toInt else length

  // merges the run from the left position to the middle and the run from the right position to
  // the end into the buffer, starting at the target position
  //
  // The index whose key compares no greater is taken at each step, and where the two keys
  // compare equal it is the one from the left run - the run that came first - which is what
  // makes the sort stable. The comparison is `java.lang.Double.compare`, the total ordering of
  // doubles, under which a not-a-number key is greater than every other and negative zero is
  // less than positive zero; it reads the two keys through the primitive array directly, so no
  // key and no index is boxed.
  @tailrec
  private def mergeRunInto(
      order: Array[Int],
      buffer: Array[Int],
      keys: Array[Double],
      target: Int,
      left: Int,
      middle: Int,
      right: Int,
      end: Int): Unit =

    if (target < end) {
      val takeLeft =
        left < middle &&
          (right >= end || java.lang.Double.compare(keys(order(left)), keys(order(right))) <= 0)
      if (takeLeft) {
        buffer(target) = order(left)
        mergeRunInto(order, buffer, keys, target + 1, left + 1, middle, right, end)
      } else {
        buffer(target) = order(right)
        mergeRunInto(order, buffer, keys, target + 1, left, middle, right + 1, end)
      }
    }

  private def selectDoubles(source: Array[Double], order: Array[Int]): Array[Double] = {
    val result = new Array[Double](order.length)
    selectDoublesInto(result, source, order, 0)
    result
  }

  @tailrec
  private def selectDoublesInto(
      result: Array[Double],
      source: Array[Double],
      order: Array[Int],
      index: Int): Unit =

    if (index < order.length) {
      result(index) = source(order(index))
      selectDoublesInto(result, source, order, index + 1)
    }

  private def selectInts(source: Array[Int], order: Array[Int]): Array[Int] = {
    val result = new Array[Int](order.length)
    selectIntsInto(result, source, order, 0)
    result
  }

  @tailrec
  private def selectIntsInto(result: Array[Int], source: Array[Int], order: Array[Int], index: Int): Unit =
    if (index < order.length) {
      result(index) = source(order(index))
      selectIntsInto(result, source, order, index + 1)
    }

  private def selectValues[V](source: Array[V], order: Array[Int]): Array[V] = {
    val result = source.clone()
    selectValuesInto(result, source, order, 0)
    result
  }

  @tailrec
  private def selectValuesInto[V](result: Array[V], source: Array[V], order: Array[Int], index: Int): Unit =
    if (index < order.length) {
      result(index) = source(order(index))
      selectValuesInto(result, source, order, index + 1)
    }
}
