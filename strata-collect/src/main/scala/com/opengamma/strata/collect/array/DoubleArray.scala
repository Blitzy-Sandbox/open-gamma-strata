/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.array

import java.util.Arrays

import scala.annotation.tailrec

import cats.Hash
import cats.Show

import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.DoubleArrayMath

/**
 * An immutable array of doubles.
 *
 * In mathematical terms this is a vector, or a one-dimensional matrix. An instance wraps a
 * primitive array of doubles and offers the operations a vector needs - arithmetic against a
 * scalar, arithmetic against another vector of the same length, mapping, reduction and
 * copy-on-write update - each of which answers with a new value and leaves its inputs alone:
 *
 * {{{
 * val base = DoubleArray.of(1.0, 2.0, 3.0)
 * val scaled = base.multipliedBy(2.0)  // [2.0, 4.0, 6.0], while base still holds [1.0, 2.0, 3.0]
 * val total = scaled.sum               // 12.0
 * }}}
 *
 * ===The stored array never escapes===
 *
 * Immutability here is enforced rather than promised. Every factory that is handed an array or
 * a sequence copies it before storing it, and `toArray` answers with a copy, so no caller can
 * reach the array an instance holds and change the value from underneath it. Two members skip
 * that copy - `ofUnsafe`, which adopts an array, and `toArrayUnsafe`, which hands back the
 * stored one - and both are visible only inside this module, where the array involved is known
 * to be freshly allocated and published nowhere else. The Java original exposed both to every
 * caller and relied on a documented convention instead; scoping them is what turns the
 * convention into a rule the compiler keeps.
 *
 * ===Numerical fidelity===
 *
 * This type carries the numerical parity duty of the port: its results are compared element by
 * element against values captured from the Java original, to an absolute and relative tolerance
 * of 1e-9. Floating-point arithmetic is neither associative nor distributive, so the order in
 * which elements are visited, and the exact form each expression takes, are part of the answer
 * rather than implementation detail. Every operation therefore visits elements in ascending
 * index order, every reduction accumulates sequentially from the documented starting value, and
 * no operation is rewritten into an algebraically equal but numerically different form - there
 * is no compensated summation and no reordered or tree-shaped reduction anywhere in this class.
 * Two consequences are worth naming, because both are observable:
 *
 *   - dividing by a scalar computes the reciprocal once and multiplies each element by it, as
 *     the original does, which differs in the last bits from dividing each element in turn;
 *   - adding or subtracting zero, multiplying by one and dividing by one answer with the same
 *     instance rather than with a copy, so `plus(0.0)` is not a way to obtain a distinct value.
 *
 * ===Equality===
 *
 * Two arrays are equal when they are the same length and their elements agree bit for bit,
 * which is the comparison the Java original makes and the one the rest of the library uses for
 * every type holding doubles. A not-a-number element is therefore equal to itself, so an array
 * holding one can still be compared and used as a map key, and a negative zero is not equal to
 * a positive zero. `contains`, `indexOf` and `lastIndexOf` search by that same comparison, so
 * they find a not-a-number element that an ordinary comparison could never match. Hashing
 * agrees with equality, and the `Hash` instance in the companion is the single equality-bearing
 * instance of the type. There is deliberately no ordering: arrays are compared for equality
 * only, and no useful total order over vectors exists to offer.
 *
 * ===Failures===
 *
 * Every failure this type reports is a caller-contract violation rather than a data-dependent
 * outcome - an index outside the array, or two arrays that have to match in length and do not -
 * so each is raised as an `IllegalArgumentException` through `ArgCheck` instead of being handed
 * back as a value to inspect. `min` and `max` on an empty array, and the range checks of
 * `copyOf`, keep the message of the Java original while raising that type in place of the state
 * and index exceptions it used, and both differences are recorded on the members concerned. An
 * index passed straight through to the stored array still surfaces as the index exception the
 * runtime raises for it.
 *
 * ===Implementation===
 *
 * The class holds no mutable state and declares no mutable local: every loop is a tail-recursive
 * private method threading its index, and any running total, as parameters, which the compiler
 * turns into the same jump a hand-written loop would produce. An operation that produces an
 * array allocates it once, fills it in index order and wraps it only once it is complete, and
 * bulk moves - copying, filling, sorting, comparing and rendering - go straight to the primitive
 * array operations of the platform. No element is boxed on any of those paths.
 *
 * ===Thread safety===
 *
 * An instance is immutable, so it is safe to share between any number of threads without
 * synchronisation.
 */
final class DoubleArray private (private val array: Array[Double]) extends Matrix {

  //-------------------------------------------------------------------------
  /**
   * The number of dimensions of this array.
   *
   * An array is a one-dimensional matrix, so this is always one.
   *
   * @return one
   */
  override def dimensions: Int = 1

  /**
   * The size of this array.
   *
   * @return the number of elements, zero or greater
   */
  override def size: Int = array.length

  /**
   * Checks whether this array is empty.
   *
   * @return true if this array holds no elements
   */
  def isEmpty: Boolean = array.length == 0

  //-------------------------------------------------------------------------
  /**
   * Gets the value at the specified index.
   *
   * The index is read straight from the stored array, so an index outside it fails with the
   * index exception the runtime raises rather than with a check of this library's own.
   *
   * @param index  the zero-based index to retrieve
   * @return the value at the index
   * @throws IndexOutOfBoundsException if the index is outside this array
   */
  def get(index: Int): Double = array(index)

  /**
   * Checks whether this array contains the specified value.
   *
   * The value is compared by its bit pattern, in order to match `equals`. That also makes this
   * the way to discover whether the array holds a not-a-number value, which no ordinary
   * comparison can find, and it means a negative zero is not found where the array holds a
   * positive zero.
   *
   * @param value  the value to find
   * @return true if the value is contained in this array
   */
  def contains(value: Double): Boolean = indexOf(value) >= 0

  /**
   * Finds the index of the first occurrence of the specified value.
   *
   * The value is compared by its bit pattern, in order to match `equals`, so this finds a
   * not-a-number value and distinguishes the two zeroes.
   *
   * @param value  the value to find
   * @return the index of the first occurrence of the value, -1 if it is not present
   */
  def indexOf(value: Double): Int = firstIndexOfFrom(DoubleArray.bitsOf(value), 0)

  // scans upwards from the index for the first element with the given bit pattern
  @tailrec
  private def firstIndexOfFrom(bits: Long, index: Int): Int =
    if (index >= array.length) {
      -1
    } else if (DoubleArray.bitsOf(array(index)) == bits) {
      index
    } else {
      firstIndexOfFrom(bits, index + 1)
    }

  /**
   * Finds the index of the last occurrence of the specified value.
   *
   * The value is compared by its bit pattern, in order to match `equals`, so this finds a
   * not-a-number value and distinguishes the two zeroes.
   *
   * @param value  the value to find
   * @return the index of the last occurrence of the value, -1 if it is not present
   */
  def lastIndexOf(value: Double): Int =
    lastIndexOfFrom(DoubleArray.bitsOf(value), array.length - 1)

  // scans downwards from the index for the first element with the given bit pattern
  @tailrec
  private def lastIndexOfFrom(bits: Long, index: Int): Int =
    if (index < 0) {
      -1
    } else if (DoubleArray.bitsOf(array(index)) == bits) {
      index
    } else {
      lastIndexOfFrom(bits, index - 1)
    }

  //-------------------------------------------------------------------------
  /**
   * Returns an array holding the values from the specified index onwards.
   *
   * @param fromIndexInclusive  the start index of the array to copy from
   * @return an array holding the values from the index to the end of this array
   * @throws IllegalArgumentException if the start index is beyond the end of this array,
   *   where the Java original raised an index exception
   * @throws IndexOutOfBoundsException if the start index is negative
   */
  def subArray(fromIndexInclusive: Int): DoubleArray =
    subArray(fromIndexInclusive, array.length)

  /**
   * Returns an array holding the values between the specified from and to indices.
   *
   * @param fromIndexInclusive  the start index of the array to copy from
   * @param toIndexExclusive  the end index of the array to copy to
   * @return an array holding the values between the two indices
   * @throws IllegalArgumentException if either index is beyond the end of this array, where the
   *   Java original raised an index exception, or if the start index is after the end index,
   *   which includes a negative end index
   * @throws IndexOutOfBoundsException if the start index is negative
   */
  def subArray(fromIndexInclusive: Int, toIndexExclusive: Int): DoubleArray =
    DoubleArray.copyOf(array, fromIndexInclusive, toIndexExclusive)

  //-------------------------------------------------------------------------
  /**
   * Converts this instance to an independent array of doubles.
   *
   * The result is a copy, so a caller is free to modify it without affecting this instance.
   *
   * @return a copy of the underlying array
   */
  def toArray: Array[Double] = array.clone()

  /**
   * Returns the underlying array itself.
   *
   * This is visible only inside this module, because the array it hands back is the one this
   * instance holds: modifying it would violate the immutability of this class. It exists for
   * the few places within the module - the codecs above all - that read every element of an
   * array they never retain, where copying would be waste with no safety to show for it.
   *
   * @return the stored array, which the caller must not modify
   */
  private[collect] def toArrayUnsafe: Array[Double] = array

  /**
   * Returns an immutable list holding the values of this array.
   *
   * The elements are boxed, as any list of doubles must box them, and the list is built once
   * rather than viewing the array, so it is independent of this instance. The Java original
   * answered with a view over the stored array instead, together with the iterator types that
   * view needed; a list built here needs neither.
   *
   * @return a list holding the values of this array in index order
   */
  def toList: List[Double] = array.iterator.toList

  /**
   * Returns an iterator over the values of this array.
   *
   * The iterator reads the stored array, which no operation ever modifies, so it cannot observe
   * a change part-way through a traversal.
   *
   * @return an iterator over the values of this array in index order
   */
  def iterator: Iterator[Double] = array.iterator

  /**
   * Applies an action to each value in this array.
   *
   * The action receives both the index and the value, and is applied in ascending index order:
   *
   * {{{
   * base.forEach((index, value) => println(s"$index: $value"))
   * }}}
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param action  the action to be applied to each index and value
   */
  def forEach(action: (Int, Double) => Unit): Unit = forEachFrom(action, 0)

  // applies the action to each index from the given one upwards, in index order
  @tailrec
  private def forEachFrom(action: (Int, Double) => Unit, index: Int): Unit =
    if (index < array.length) {
      action(index, array(index))
      forEachFrom(action, index + 1)
    }

  //-------------------------------------------------------------------------
  /**
   * Returns an instance with the value at the specified index changed.
   *
   * The new value is compared with the stored one by bit pattern, so replacing a value with the
   * one already there answers with this instance rather than with a copy. The name is that of
   * the Java original, which is a reserved word here and so is written in backquotes.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param index  the zero-based index to set
   * @param newValue  the new value to store at the index
   * @return a copy of this array with the value at the index changed
   * @throws IndexOutOfBoundsException if the index is outside this array
   */
  def `with`(index: Int, newValue: Double): DoubleArray =
    if (DoubleArray.bitsOf(array(index)) == DoubleArray.bitsOf(newValue)) {
      this
    } else {
      val result = array.clone()
      result(index) = newValue
      DoubleArray.ofUnsafe(result)
    }

  //-------------------------------------------------------------------------
  /**
   * Returns an instance with the specified amount added to each value.
   *
   * Adding zero answers with this instance, which includes adding a negative zero, since the two
   * zeroes compare equal. This is a special case of `map`.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param amount  the amount to add, which may be negative
   * @return a copy of this array with the amount added to each value
   */
  def plus(amount: Double): DoubleArray =
    if (amount == 0d) {
      this
    } else {
      val result = new Array[Double](array.length)
      plusInto(result, amount, 0)
      DoubleArray.ofUnsafe(result)
    }

  // fills the result from the index upwards with each value plus the amount, in index order
  @tailrec
  private def plusInto(result: Array[Double], amount: Double, index: Int): Unit =
    if (index < result.length) {
      result(index) = array(index) + amount
      plusInto(result, amount, index + 1)
    }

  /**
   * Returns an instance with the specified amount subtracted from each value.
   *
   * Subtracting zero answers with this instance, which includes subtracting a negative zero,
   * since the two zeroes compare equal. This is a special case of `map`.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param amount  the amount to subtract, which may be negative
   * @return a copy of this array with the amount subtracted from each value
   */
  def minus(amount: Double): DoubleArray =
    if (amount == 0d) {
      this
    } else {
      val result = new Array[Double](array.length)
      minusInto(result, amount, 0)
      DoubleArray.ofUnsafe(result)
    }

  // fills the result from the index upwards with each value minus the amount, in index order
  @tailrec
  private def minusInto(result: Array[Double], amount: Double, index: Int): Unit =
    if (index < result.length) {
      result(index) = array(index) - amount
      minusInto(result, amount, index + 1)
    }

  /**
   * Returns an instance with each value multiplied by the specified factor.
   *
   * Multiplying by one answers with this instance. This is a special case of `map`.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param factor  the multiplicative factor
   * @return a copy of this array with each value multiplied by the factor
   */
  def multipliedBy(factor: Double): DoubleArray =
    if (factor == 1d) {
      this
    } else {
      val result = new Array[Double](array.length)
      scaledInto(result, factor, 0)
      DoubleArray.ofUnsafe(result)
    }

  /**
   * Returns an instance with each value divided by the specified divisor.
   *
   * Dividing by one answers with this instance. Otherwise the reciprocal of the divisor is
   * computed once and each value is multiplied by it, which is what the Java original does:
   * a multiplication is cheaper than a division, and the result differs in the last bits from
   * dividing each value in turn, so the choice is part of the behaviour this port reproduces
   * rather than an optimisation it is free to make differently. This is a special case of `map`.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param divisor  the value by which each value is divided
   * @return a copy of this array with each value divided by the divisor
   */
  def dividedBy(divisor: Double): DoubleArray =
    if (divisor == 1d) {
      this
    } else {
      val factor = 1 / divisor
      val result = new Array[Double](array.length)
      scaledInto(result, factor, 0)
      DoubleArray.ofUnsafe(result)
    }

  // fills the result from the index upwards with each value times the factor, in index order;
  // shared by multipliedBy and dividedBy because the original multiplies in both cases
  @tailrec
  private def scaledInto(result: Array[Double], factor: Double, index: Int): Unit =
    if (index < result.length) {
      result(index) = array(index) * factor
      scaledInto(result, factor, index + 1)
    }

  //-------------------------------------------------------------------------
  /**
   * Returns an instance with an operation applied to each value in this array.
   *
   * The operator receives the value alone and is applied in ascending index order, so it may be
   * written as a lambda at the call site:
   *
   * {{{
   * val inverted = base.map(value => 1 / value)
   * }}}
   *
   * There is no short cut for an operator that happens to be the identity: the result is always
   * a new array.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param operator  the operator to be applied to each value
   * @return a copy of this array with the operator applied to each value
   */
  def map(operator: Double => Double): DoubleArray = {
    val result = new Array[Double](array.length)
    mapInto(result, operator, 0)
    DoubleArray.ofUnsafe(result)
  }

  // fills the result from the index upwards with the operator applied to each value
  @tailrec
  private def mapInto(result: Array[Double], operator: Double => Double, index: Int): Unit =
    if (index < result.length) {
      result(index) = operator(array(index))
      mapInto(result, operator, index + 1)
    }

  /**
   * Returns an instance with an operation applied to each indexed value in this array.
   *
   * The function receives both the index and the value, and is applied in ascending index order:
   *
   * {{{
   * val weighted = base.mapWithIndex((index, value) => index.toDouble * value)
   * }}}
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param function  the function to be applied to each index and value
   * @return a copy of this array with the function applied to each indexed value
   */
  def mapWithIndex(function: (Int, Double) => Double): DoubleArray = {
    val result = new Array[Double](array.length)
    mapWithIndexInto(result, function, 0)
    DoubleArray.ofUnsafe(result)
  }

  // fills the result from the index upwards with the function applied to each indexed value
  @tailrec
  private def mapWithIndexInto(
      result: Array[Double],
      function: (Int, Double) => Double,
      index: Int): Unit =

    if (index < result.length) {
      result(index) = function(index, array(index))
      mapWithIndexInto(result, function, index + 1)
    }

  //-------------------------------------------------------------------------
  /**
   * Returns an instance where each element is the sum of the matching values in this array and
   * the other array.
   *
   * Element `n` of the result is element `n` of this array plus element `n` of the other array,
   * so the two arrays must be the same size. This is a special case of `combine`.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param other  the other array
   * @return a copy of this array with the matching elements added
   * @throws IllegalArgumentException if the arrays have different sizes
   */
  def plus(other: DoubleArray): DoubleArray = {
    ArgCheck.isTrue(array.length == other.array.length, DoubleArray.differentSizes)
    val result = new Array[Double](array.length)
    plusEachInto(result, other.array, 0)
    DoubleArray.ofUnsafe(result)
  }

  // fills the result from the index upwards with the sum of the matching elements
  @tailrec
  private def plusEachInto(result: Array[Double], other: Array[Double], index: Int): Unit =
    if (index < result.length) {
      result(index) = array(index) + other(index)
      plusEachInto(result, other, index + 1)
    }

  /**
   * Returns an instance where each element is the difference between the matching values in this
   * array and the other array.
   *
   * Element `n` of the result is element `n` of this array minus element `n` of the other array,
   * so the two arrays must be the same size. This is a special case of `combine`.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param other  the other array
   * @return a copy of this array with the matching elements subtracted
   * @throws IllegalArgumentException if the arrays have different sizes
   */
  def minus(other: DoubleArray): DoubleArray = {
    ArgCheck.isTrue(array.length == other.array.length, DoubleArray.differentSizes)
    val result = new Array[Double](array.length)
    minusEachInto(result, other.array, 0)
    DoubleArray.ofUnsafe(result)
  }

  // fills the result from the index upwards with the difference of the matching elements
  @tailrec
  private def minusEachInto(result: Array[Double], other: Array[Double], index: Int): Unit =
    if (index < result.length) {
      result(index) = array(index) - other(index)
      minusEachInto(result, other, index + 1)
    }

  /**
   * Returns an instance where each element is the product of the matching values in this array
   * and the other array.
   *
   * Element `n` of the result is element `n` of this array multiplied by element `n` of the
   * other array, so the two arrays must be the same size. This is a special case of `combine`.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param other  the other array
   * @return a copy of this array with the matching elements multiplied
   * @throws IllegalArgumentException if the arrays have different sizes
   */
  def multipliedBy(other: DoubleArray): DoubleArray = {
    ArgCheck.isTrue(array.length == other.array.length, DoubleArray.differentSizes)
    val result = new Array[Double](array.length)
    multipliedByEachInto(result, other.array, 0)
    DoubleArray.ofUnsafe(result)
  }

  // fills the result from the index upwards with the product of the matching elements
  @tailrec
  private def multipliedByEachInto(result: Array[Double], other: Array[Double], index: Int): Unit =
    if (index < result.length) {
      result(index) = array(index) * other(index)
      multipliedByEachInto(result, other, index + 1)
    }

  /**
   * Returns an instance where each element is the quotient of the matching values in this array
   * and the other array.
   *
   * Element `n` of the result is element `n` of this array divided by element `n` of the other
   * array, so the two arrays must be the same size. Unlike the scalar form, which multiplies by
   * a reciprocal computed once, this divides each pair of elements, exactly as the Java original
   * does. This is a special case of `combine`.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param other  the other array
   * @return a copy of this array with the matching elements divided
   * @throws IllegalArgumentException if the arrays have different sizes
   */
  def dividedBy(other: DoubleArray): DoubleArray = {
    ArgCheck.isTrue(array.length == other.array.length, DoubleArray.differentSizes)
    val result = new Array[Double](array.length)
    dividedByEachInto(result, other.array, 0)
    DoubleArray.ofUnsafe(result)
  }

  // fills the result from the index upwards with the quotient of the matching elements
  @tailrec
  private def dividedByEachInto(result: Array[Double], other: Array[Double], index: Int): Unit =
    if (index < result.length) {
      result(index) = array(index) / other(index)
      dividedByEachInto(result, other, index + 1)
    }

  /**
   * Returns an instance where each element is formed by combining the matching values in this
   * array and the other array.
   *
   * Element `n` of the result is the operator applied to element `n` of this array and element
   * `n` of the other array, in ascending index order, so the two arrays must be the same size:
   *
   * {{{
   * val products = base.combine(other, (a, b) => a * b)
   * }}}
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param other  the other array
   * @param operator  the operator used to combine each pair of values
   * @return a copy of this array combined with the other array
   * @throws IllegalArgumentException if the arrays have different sizes
   */
  def combine(other: DoubleArray, operator: (Double, Double) => Double): DoubleArray = {
    ArgCheck.isTrue(array.length == other.array.length, DoubleArray.differentSizes)
    val result = new Array[Double](array.length)
    combineEachInto(result, other.array, operator, 0)
    DoubleArray.ofUnsafe(result)
  }

  // fills the result from the index upwards with the operator applied to the matching elements
  @tailrec
  private def combineEachInto(
      result: Array[Double],
      other: Array[Double],
      operator: (Double, Double) => Double,
      index: Int): Unit =

    if (index < result.length) {
      result(index) = operator(array(index), other(index))
      combineEachInto(result, other, operator, index + 1)
    }

  /**
   * Combines this array and the other array, returning a single reduced value.
   *
   * The operator is called once per index, in ascending order. Its first argument is the running
   * total of the reduction, which starts at zero; its second argument is the element of this
   * array and its third the element of the other array. The two arrays must be the same size:
   *
   * {{{
   * val dotProduct = base.combineReduce(other, (total, a, b) => total + a * b)
   * }}}
   *
   * The operator is a [[DoubleArray.DoubleTernaryOperator]] rather than a three-argument
   * function, and the lambda above is converted into one where it is written. That is what keeps
   * this operation unboxed like every other operation here: the language specialises functions
   * of one and two arguments over the primitive types but not functions of three, so a
   * `(Double, Double, Double) => Double` would box its three arguments and unbox its result once
   * per element, whereas the callback type declares those four values as primitive doubles and
   * the reduction loop passes them as such.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param other  the other array
   * @param operator  the operator used to combine each pair of values with the running total
   * @return the result of the reduction
   * @throws IllegalArgumentException if the arrays have different sizes
   */
  def combineReduce(other: DoubleArray, operator: DoubleArray.DoubleTernaryOperator): Double = {
    ArgCheck.isTrue(array.length == other.array.length, DoubleArray.differentSizes)
    ternaryFoldFrom(other.array, operator, 0, 0d)
  }

  // applies the ternary operator to the running total and the matching elements, in index order
  @tailrec
  private def ternaryFoldFrom(
      other: Array[Double],
      operator: DoubleArray.DoubleTernaryOperator,
      index: Int,
      total: Double): Double =

    if (index >= array.length) {
      total
    } else {
      ternaryFoldFrom(
        other,
        operator,
        index + 1,
        operator.applyAsDouble(total, array(index), other(index)))
    }

  //-------------------------------------------------------------------------
  /**
   * Returns an array that combines this array and the specified values.
   *
   * The result is as long as this array plus the number of values supplied. Concatenating
   * nothing answers with this instance.
   *
   * The result is allocated once, at its final length, and each source is moved into it in one
   * bulk copy: the stored elements into the front, the supplied values into the tail. Asking the
   * sequence for an array of its own first would copy those values twice, once into that array
   * and once out of it again.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param values  the values to add to the end of this array
   * @return a copy of this array with the values added at the end
   */
  def concat(values: Double*): DoubleArray =
    if (values.isEmpty) {
      this
    } else {
      // the result is freshly allocated here and published nowhere else, which is what makes it
      // safe to adopt rather than copy; `copyToArray` answers with the number of elements it
      // moved, which is the length of the sequence and so tells us nothing we do not know
      val result = new Array[Double](array.length + values.length)
      System.arraycopy(array, 0, result, 0, array.length)
      val _ = values.copyToArray(result, array.length)
      DoubleArray.ofUnsafe(result)
    }

  /**
   * Returns an array that combines this array and the other array.
   *
   * The result is as long as this array plus the other array. Concatenating an empty array
   * answers with this instance, and concatenating onto an empty array answers with the other
   * instance itself, which is safe because both are immutable.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param other  the array to add to the end of this array
   * @return a copy of this array with the other array added at the end
   */
  def concat(other: DoubleArray): DoubleArray =
    if (array.length == 0) {
      other
    } else if (other.array.length == 0) {
      this
    } else {
      concatArray(other.array)
    }

  // joins this array and a non-empty other array into one freshly allocated array
  private def concatArray(other: Array[Double]): DoubleArray = {
    val result = Arrays.copyOf(array, array.length + other.length)
    System.arraycopy(other, 0, result, array.length, other.length)
    DoubleArray.ofUnsafe(result)
  }

  //-------------------------------------------------------------------------
  /**
   * Returns a sorted copy of this array.
   *
   * An array of fewer than two elements is already sorted and answers with this instance. The
   * order is that of the primitive sort of the platform, which places a negative zero before a
   * positive zero and any not-a-number element last.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @return a sorted copy of this array
   */
  def sorted: DoubleArray =
    if (array.length < 2) {
      this
    } else {
      val result = array.clone()
      Arrays.sort(result)
      DoubleArray.ofUnsafe(result)
    }

  //-------------------------------------------------------------------------
  /**
   * Returns the minimum value held in this array.
   *
   * An array holding a not-a-number element has a minimum of not-a-number, because that is how
   * the minimum of two values is defined on this platform. An empty array has no minimum and
   * fails: the Java original raised an illegal state here, where this port raises an illegal
   * argument through `ArgCheck`, keeping the message unchanged.
   *
   * @return the minimum value
   * @throws IllegalArgumentException if this array is empty
   */
  def min: Double = {
    ArgCheck.isTrue(array.length > 0, "Unable to find minimum of an empty array")
    if (array.length == 1) {
      array(0)
    } else {
      minFrom(0, Double.PositiveInfinity)
    }
  }

  // folds the smaller of the running minimum and each element, in index order
  @tailrec
  private def minFrom(index: Int, min: Double): Double =
    if (index >= array.length) {
      min
    } else {
      minFrom(index + 1, math.min(min, array(index)))
    }

  /**
   * Returns the maximum value held in this array.
   *
   * An array holding a not-a-number element has a maximum of not-a-number, because that is how
   * the maximum of two values is defined on this platform. An empty array has no maximum and
   * fails: the Java original raised an illegal state here, where this port raises an illegal
   * argument through `ArgCheck`, keeping the message unchanged.
   *
   * @return the maximum value
   * @throws IllegalArgumentException if this array is empty
   */
  def max: Double = {
    ArgCheck.isTrue(array.length > 0, "Unable to find maximum of an empty array")
    if (array.length == 1) {
      array(0)
    } else {
      maxFrom(0, Double.NegativeInfinity)
    }
  }

  // folds the larger of the running maximum and each element, in index order
  @tailrec
  private def maxFrom(index: Int, max: Double): Double =
    if (index >= array.length) {
      max
    } else {
      maxFrom(index + 1, math.max(max, array(index)))
    }

  /**
   * Returns the sum of all the values in this array.
   *
   * The elements are added in ascending index order, starting from a positive zero, so an empty
   * array sums to zero. The order is part of the answer rather than an implementation detail,
   * because addition of doubles is not associative: a sum computed in any other order would
   * differ in its last digits from the value this port is required to reproduce. This is a
   * special case of `reduce`.
   *
   * @return the total of all the values
   */
  def sum: Double = sumFrom(0, 0d)

  // adds each element from the index upwards to the running total, in index order
  @tailrec
  private def sumFrom(index: Int, total: Double): Double =
    if (index >= array.length) {
      total
    } else {
      sumFrom(index + 1, total + array(index))
    }

  /**
   * Reduces this array to a single value.
   *
   * The operator is called once per element, in ascending index order. Its first argument is the
   * running total of the reduction, which starts at the identity supplied, and its second is the
   * element. An empty array reduces to the identity without the operator being called at all:
   *
   * {{{
   * val product = base.reduce(1.0, (total, value) => total * value)
   * }}}
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param identity  the identity value to start the reduction from
   * @param operator  the operator used to combine each value with the running total
   * @return the result of the reduction
   */
  def reduce(identity: Double, operator: (Double, Double) => Double): Double =
    reduceFrom(operator, 0, identity)

  // applies the operator to the running total and each element, in index order
  @tailrec
  private def reduceFrom(operator: (Double, Double) => Double, index: Int, total: Double): Double =
    if (index >= array.length) {
      total
    } else {
      reduceFrom(operator, index + 1, operator(total, array(index)))
    }

  //-------------------------------------------------------------------------
  /**
   * Checks whether this array equals another within the specified tolerance.
   *
   * Arrays of different sizes are simply unequal, which is reported rather than raised: this
   * asks a question about the two arrays instead of computing a result from them. The edge
   * behaviour - which values a tolerance does and does not bring together - belongs to the
   * comparison this delegates to, and is documented there.
   *
   * @param other  the other array
   * @param tolerance  the tolerance to use, zero or greater
   * @return true if the arrays are equal up to the tolerance
   * @throws IllegalArgumentException if the tolerance is negative or is not a number
   */
  def equalWithTolerance(other: DoubleArray, tolerance: Double): Boolean =
    DoubleArrayMath.fuzzyEquals(array, other.array, tolerance)

  /**
   * Checks whether every value in this array equals zero within the specified tolerance.
   *
   * An empty array holds no value that differs from zero and so is equal to zero.
   *
   * @param tolerance  the tolerance to use, zero or greater
   * @return true if every value is equal to zero up to the tolerance
   * @throws IllegalArgumentException if the tolerance is negative or is not a number
   */
  def equalZeroWithTolerance(tolerance: Double): Boolean =
    DoubleArrayMath.fuzzyEqualsZero(array, tolerance)

  //-------------------------------------------------------------------------
  /**
   * Checks whether this array equals another object.
   *
   * Another array is equal when it is the same length and its elements agree bit for bit, so a
   * not-a-number element equals itself and the two zeroes are distinct. An object of any other
   * type is not equal.
   *
   * @param obj  the object to compare to
   * @return true if the other object is an array holding the same values
   */
  override def equals(obj: Any): Boolean = obj match {
    case other: DoubleArray => (this eq other) || Arrays.equals(array, other.array)
    case _ => false
  }

  /**
   * Returns a hash code consistent with `equals`.
   *
   * @return the hash code of the values held
   */
  override def hashCode: Int = Arrays.hashCode(array)

  /**
   * Returns the values of this array as text.
   *
   * The form is that of the Java original, the values in index order between square brackets,
   * as in `[1.0, 2.0, 3.0]`.
   *
   * @return the rendering of this array
   */
  override def toString: String = Arrays.toString(array)
}

/**
 * Factories and typeclass instances for immutable arrays of doubles.
 *
 * Construction is total: every factory here either answers with an array or fails on a
 * caller-contract violation, and none of them can reject the data it is given, so there is no
 * validated form of construction and no error to hand back as a value.
 */
object DoubleArray {

  /**
   * An empty array.
   *
   * Every factory that has no elements to produce answers with this instance, so the common
   * empty result costs no allocation and can be recognised by identity as well as by equality.
   */
  val EMPTY: DoubleArray = new DoubleArray(new Array[Double](0))

  // The message reported when two arrays that have to match in length do not. One definition
  // keeps it identical at every site, and identical to the message of the Java original, which
  // reaches logs and test expectations.
  private val differentSizes: String = "Arrays have different sizes"

  //-------------------------------------------------------------------------
  /**
   * An operator over three doubles, used by [[DoubleArray.combineReduce]].
   *
   * This is a callback type rather than a function type, and it exists for one reason: the
   * language specialises functions of one and two arguments over the primitive types but not
   * functions of three, so a `(Double, Double, Double) => Double` would box each of its three
   * arguments and unbox its result on every call, which on a reduction means four allocations
   * per element. A trait with one abstract method typed in `Double` compiles to the primitive
   * descriptor `(DDD)D`, so the reduction loop passes its values in registers and allocates
   * nothing:
   *
   * {{{
   * val dotProduct = base.combineReduce(other, (total, a, b) => total + a * b)
   * }}}
   *
   * Nothing is lost at the call site by using it. A trait with a single abstract method is a
   * target for function-literal conversion, so a lambda written in the shape above is accepted
   * where one of these is expected and reads exactly as a three-argument function would; the
   * conversion is what turns it into an implementation of this trait. A caller that wants to
   * retain and reuse an operator may also implement it explicitly.
   *
   * The Java original passed its own primitive functional interface here. That interface lives
   * in the part of the Java module this port does not carry, so the type it named is declared
   * where the one member that needs it lives.
   */
  trait DoubleTernaryOperator {

    /**
     * Applies the operator to the running total and a pair of values.
     *
     * @param total  the running total of the reduction
     * @param first  the value from the array the reduction was invoked on
     * @param second  the value from the other array
     * @return the new running total
     */
    def applyAsDouble(total: Double, first: Double, second: Double): Double
  }

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance holding the specified values.
   *
   * This one member replaces the eleven arity-specific factories of the Java original, which
   * existed only to spare a caller the cost of an array allocation per call:
   *
   * {{{
   * val empty = DoubleArray.of()             // the empty array
   * val three = DoubleArray.of(1.0, 2.0, 3.0)
   * }}}
   *
   * The sequence copies itself into a fresh array, so a caller that expanded an array of its own
   * into this call cannot reach the array the result holds.
   *
   * @param values  the values to hold, in order
   * @return an array holding the specified values, the empty array if none are supplied
   */
  def of(values: Double*): DoubleArray = ofUnsafe(values.toArray)

  /**
   * Obtains an instance with entries filled using a function.
   *
   * The function is passed each index in turn, in ascending order, and returns the value for
   * that index:
   *
   * {{{
   * val squares = DoubleArray.tabulate(4)(index => index.toDouble * index.toDouble)
   * }}}
   *
   * The two parameter lists are what let the function be written as a block at the call site.
   * The Java original spelled this as another overload of its value factory, taking the size and
   * a function together; as a named member it says what it does, and it is the member a caller
   * outside this module reaches for in place of building an array and wrapping it.
   *
   * @param size  the number of elements, zero or greater
   * @param valueFunction  the function from index to value
   * @return an array of the specified size populated by the function
   * @throws NegativeArraySizeException if the size is negative
   */
  def tabulate(size: Int)(valueFunction: Int => Double): DoubleArray =
    if (size == 0) {
      EMPTY
    } else {
      val result = new Array[Double](size)
      tabulateInto(result, valueFunction, 0)
      new DoubleArray(result)
    }

  // fills the result from the index upwards with the function applied to each index
  @tailrec
  private def tabulateInto(
      result: Array[Double],
      valueFunction: Int => Double,
      index: Int): Unit =

    if (index < result.length) {
      result(index) = valueFunction(index)
      tabulateInto(result, valueFunction, index + 1)
    }

  /**
   * Obtains an instance by adopting an array without copying it.
   *
   * This is visible only inside this module, because it makes the caller responsible for the
   * immutability of the result: the array passed in must be freshly allocated, or otherwise
   * published nowhere else, and must never be modified afterwards. Inside the module that
   * condition is met and checkable by reading the call site - each one either allocates the
   * array a line earlier or takes it from a sequence that has just copied itself - which is what
   * makes the copying elsewhere in this class complete rather than conventional.
   *
   * @param array  the array to adopt, which the caller must never modify
   * @return an array instance holding the specified array
   */
  private[collect] def ofUnsafe(array: Array[Double]): DoubleArray =
    if (array.length == 0) {
      EMPTY
    } else {
      new DoubleArray(array)
    }

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance from an iterable of doubles.
   *
   * The values are held in iteration order. The elements of the input are boxed, as any generic
   * collection of doubles must box them, and they are unboxed once into the primitive array of
   * the result.
   *
   * @param collection  the iterable to initialise from
   * @return an array holding the values of the iterable in iteration order
   */
  def copyOf(collection: Iterable[Double]): DoubleArray = ofUnsafe(collection.toArray)

  /**
   * Obtains an instance by copying an array of doubles.
   *
   * The input array is copied and never modified, so the caller may go on using it.
   *
   * @param array  the array to copy
   * @return an array holding the values of the specified array
   */
  def copyOf(array: Array[Double]): DoubleArray =
    if (array.length == 0) {
      EMPTY
    } else {
      new DoubleArray(array.clone())
    }

  /**
   * Obtains an instance by copying part of an array of doubles.
   *
   * The input array is copied and never modified, so the caller may go on using it.
   *
   * @param array  the array to copy
   * @param fromIndexInclusive  the index of the input array to copy from
   * @return an array holding the values from the index to the end of the input array
   * @throws IllegalArgumentException if the start index is beyond the end of the input array,
   *   where the Java original raised an index exception
   * @throws IndexOutOfBoundsException if the start index is negative
   */
  def copyOf(array: Array[Double], fromIndexInclusive: Int): DoubleArray =
    copyOf(array, fromIndexInclusive, array.length)

  /**
   * Obtains an instance by copying part of an array of doubles.
   *
   * The input array is copied and never modified, so the caller may go on using it.
   *
   * Both bounds are checked against the length of the input before anything is copied. That
   * check is not redundant: the primitive range copy of the platform pads with zeroes when asked
   * for elements beyond the end of an array, so without it a range that overruns the input would
   * answer with zeroes rather than fail. The messages are those of the Java original, which
   * raised an index exception where this port raises an illegal argument through `ArgCheck`. A
   * negative start index is left to the range copy itself, which raises the index exception for
   * it, exactly as in the original.
   *
   * @param array  the array to copy
   * @param fromIndexInclusive  the start index of the input array to copy from
   * @param toIndexExclusive  the end index of the input array to copy to
   * @return an array holding the values between the two indices
   * @throws IllegalArgumentException if either index is beyond the end of the input array, where
   *   the Java original raised an index exception, or if the start index is after the end index
   * @throws IndexOutOfBoundsException if the start index is negative
   */
  def copyOf(array: Array[Double], fromIndexInclusive: Int, toIndexExclusive: Int): DoubleArray = {
    ArgCheck.isTrue(
      fromIndexInclusive <= array.length,
      s"Array index out of bounds: $fromIndexInclusive > ${array.length}")
    ArgCheck.isTrue(
      toIndexExclusive <= array.length,
      s"Array index out of bounds: $toIndexExclusive > ${array.length}")
    if ((toIndexExclusive - fromIndexInclusive) == 0) {
      EMPTY
    } else {
      new DoubleArray(Arrays.copyOfRange(array, fromIndexInclusive, toIndexExclusive))
    }
  }

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance with every entry equal to zero.
   *
   * @param size  the number of elements, zero or greater
   * @return an array of the specified size filled with zeroes
   * @throws NegativeArraySizeException if the size is negative
   */
  def filled(size: Int): DoubleArray =
    if (size == 0) {
      EMPTY
    } else {
      new DoubleArray(new Array[Double](size))
    }

  /**
   * Obtains an instance with every entry equal to the same value.
   *
   * @param size  the number of elements, zero or greater
   * @param value  the value of every element
   * @return an array of the specified size filled with the specified value
   * @throws NegativeArraySizeException if the size is negative
   */
  def filled(size: Int, value: Double): DoubleArray =
    if (size == 0) {
      EMPTY
    } else {
      val result = new Array[Double](size)
      Arrays.fill(result, value)
      new DoubleArray(result)
    }

  //-------------------------------------------------------------------------
  // The bit pattern of a value, which is how this type compares elements: it is the comparison
  // `equals` makes, so searching by it agrees with equality, a not-a-number value is equal to
  // itself and the two zeroes are distinct.
  private def bitsOf(value: Double): Long = java.lang.Double.doubleToLongBits(value)

  //-------------------------------------------------------------------------
  /**
   * The hashing of arrays, which is also their equality.
   *
   * This is the only equality-bearing instance of the type: `Hash` extends `Eq`, so declaring
   * `Eq` as well would leave two instances that could disagree and one of them ambiguous.
   * Hashing and equality are those of the values themselves, element by element and bit for
   * bit, as `equals` and `hashCode` define them. There is deliberately no `Order`.
   *
   * @return the hashing of arrays
   */
  implicit val hash: Hash[DoubleArray] = Hash.fromUniversalHashCode[DoubleArray]

  /**
   * The rendering of arrays as text.
   *
   * An array renders as its values between square brackets, as `toString` does.
   *
   * @return the rendering of an array
   */
  implicit val show: Show[DoubleArray] = Show.show(_.toString)
}
