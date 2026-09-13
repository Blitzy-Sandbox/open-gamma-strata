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
 * Immutability here is enforced by the compiled code rather than promised by a convention, and
 * the enforcement rests on two facts about this class that hold together. The sole constructor
 * produces the storage an instance keeps - a copy of the run of values it is handed, or a copy of
 * a range of that run - rather than storing what it was handed, so an instance's storage is
 * allocated by that constructor and is reachable from nowhere the caller can name; and no member
 * hands that storage out - `toArray` answers with a copy of it, and every other member answers
 * with an element, a count or a new array. Whoever holds an array, and whatever they do with it
 * afterwards, they cannot change a value built from it.
 *
 * Stating it that way is deliberate, because the alternative does not hold on this platform. A
 * member restricted to this module is restricted in ''source'' only: the compiler emits it as a
 * public method, so a caller compiled against the class - in this language or in Java, in this
 * package or in one that merely claims the name - reaches it regardless. The same is true of the
 * constructor, which a companion has to reach and which is therefore public whatever it is
 * declared to be. A guarantee that depended on either would be a guarantee about what a source
 * file may say and not about what a run-time can do, which is why the copy sits in the one place
 * every construction path passes through instead.
 *
 * The Java original published two members that skipped the copy - `ofUnsafe`, which adopted an
 * array, and `toArrayUnsafe`, which handed back the stored one - and relied on a documented
 * convention that no caller would write through either. Neither is ported, under any name or
 * any visibility: the copy the constructor makes is cheap next to the arithmetic these arrays
 * exist for, and an aliasing pair of members is the one way this type could stop being immutable
 * in fact while still being immutable in its documentation.
 *
 * ===Numerical fidelity===
 *
 * This type carries the numerical parity duty of this module: its results are compared element
 * by element against the captured baseline values that the parity fixture of this module holds,
 * to an absolute and relative tolerance of 1e-9. Floating-point arithmetic is neither
 * associative nor distributive, so the order in which elements are visited, and the exact form
 * each expression takes, are part of the answer rather than implementation detail. Every
 * operation therefore visits elements in ascending index order, every reduction accumulates
 * sequentially from the documented starting value, and no operation is rewritten into an
 * algebraically equal but numerically different form - there is no compensated summation and no
 * reordered or tree-shaped reduction anywhere in this class. Two consequences are worth naming,
 * because both are observable:
 *
 *   - dividing by a scalar computes the reciprocal once and multiplies each element by it, which
 *     differs in the last bits from dividing each element in turn;
 *   - adding or subtracting zero, multiplying by one and dividing by one answer with the same
 *     instance rather than with a copy, so `plus(0.0)` is not a way to obtain a distinct value.
 *
 * ===Equality===
 *
 * Two arrays are equal when they are the same length and their elements agree bit for bit,
 * which is the comparison the rest of the library uses for every type holding doubles. A
 * not-a-number element is therefore equal to itself, so an array holding one can still be
 * compared and used as a map key, and a negative zero is not equal to a positive zero.
 * `contains`, `indexOf` and `lastIndexOf` search by that same comparison, so they find a
 * not-a-number element that an ordinary comparison could never match. Hashing agrees with
 * equality, and the `Hash` instance in the companion is the single equality-bearing instance of
 * the type. There is deliberately no ordering: arrays are compared for equality only, and no
 * useful total order over vectors exists to offer.
 *
 * ===Failures===
 *
 * Every failure this type reports is a caller-contract violation rather than a data-dependent
 * outcome, so each is raised rather than handed back as a value to inspect. There are two
 * kinds, and which kind a failure belongs to is what decides the exception it raises:
 *
 *   - a size, length or state violation - a negative size asked of a factory, two arrays that
 *     have to match in length and do not, a sub-array boundary beyond the end of the array, or
 *     the smallest or largest element of an array that has none - is checked before anything is
 *     allocated or read and raised as an `IllegalArgumentException` through `ArgCheck`. Checking
 *     a size before acting on it is what keeps an invalid size from allocating at all, and an
 *     invalid size is a caller-contract violation like any other, so it is reported like one;
 *   - an index outside the array surfaces as the index exception the runtime raises for the
 *     array access itself. `get`, `with` and the other members that address a single element
 *     read the stored array directly, and no check stands in front of them to change that.
 *
 * ===Implementation===
 *
 * The class holds no mutable state and declares no mutable local: every loop is a tail-recursive
 * private method threading its index, and any running total, as parameters, which the compiler
 * turns into the same jump a hand-written loop would produce. Bulk moves - copying, filling,
 * sorting, comparing and rendering - go straight to the primitive array operations of the
 * platform. No element is boxed on any of those paths.
 *
 * An operation that produces a run of values allocates it exactly once, and that one allocation
 * is the copy the constructor makes. The constructor is told which operation it is constructing
 * for, alongside the values: it produces the storage it will keep - a clone of the run it was
 * handed, or a copy of a range of it - and then applies that operation's own loop to that
 * storage, in ascending index order, before the value is published. Cloning the source and
 * rewriting the clone in place leaves exactly what filling a fresh buffer from the source would
 * have left, because the clone starts out holding the source, so a produced value costs one
 * allocation and one bulk copy and the loop that follows allocates nothing at all. The choice of
 * loop is made once per operation, where the operation names its rewrite, and not once per
 * element: the loops stay one per operation, each monomorphic over the primitive array and
 * specialised in its callback, so nothing here trades a bulk copy for a virtual call per element.
 *
 * Two properties of that arrangement matter beyond the allocation it saves. The rewrite happens
 * inside the constructor, so the stored array is written only while it is being initialised and
 * is final by the time any other thread can reach the value - which is what the ''Thread safety''
 * note below rests on; and the constructor still never stores the array it is handed, which is
 * what keeps every factory and every accessor copy-safe for any caller the bytecode admits, in
 * the way ''The stored array never escapes'' above sets out. There is no member, and no
 * constructor argument, through which an array a caller retains can become a value's storage: a
 * member that adopted a run of values instead of producing it would be exactly that, and would be
 * callable by anything compiled against this class, because a companion-private helper reached
 * from here is emitted as a public method under a mangled name - which is why the build's check on
 * the members of these two numeric types has to name the matrix's deep copy explicitly. That is
 * the aliasing entry point `ofUnsafe` and `toArrayUnsafe` were, and this type deliberately has
 * none of it.
 *
 * Construction that does not start from a run of values keeps a buffer of its own, and pays the
 * extra bulk copy the operations above no longer pay: `of` and `copyOf` of a collection unbox a
 * boxed sequence, `tabulate` and `filled` compute their elements from a size, and `concat`
 * gathers two sources at a length neither of them has - which is also what keeps it free of
 * per-element boxing, since each source is moved in one bulk copy. None of those has a run of
 * values for the constructor to derive storage from, so each fills a buffer and the constructor
 * copies it. That copy is of a freshly allocated buffer no caller holds, and it is paid once per
 * construction rather than once per element, which is the cost `copyOf` records along with the
 * property it buys: that no array anywhere - inside this class or outside it - is reachable both
 * by a caller and by an instance, whatever language or compiler produced that caller.
 *
 * ===Thread safety===
 *
 * An instance is immutable, so it is safe to share between any number of threads without
 * synchronisation.
 */
final class DoubleArray private (values: Array[Double], rewrite: DoubleArray.Rewrite)
    extends Matrix {

  // The values of this array: storage produced here, rewritten here, and held by nothing else.
  //
  // This is the single expression that makes the type immutable in the compiled code, which is
  // why every branch of it produces an array rather than keeping the argument: every factory of
  // the companion, and every operation that produces a new array, reaches this constructor, so a
  // run of values stored here was allocated here and is held by nothing else. The field is
  // private to the class and is read only from within it - including from another instance of it,
  // which the platform allows and which `equals` and the element-wise rewrites below do - so the
  // compiler emits no accessor for it beyond the private one, and there is no member of this type
  // through which it can be reached.
  //
  // The rewrite says which operation this value is being constructed for, and the match selects
  // that operation's loop once, here, rather than once per element. Every case produces the
  // storage first - a clone of what the constructor was handed, or a copy of a range of it - and
  // then applies that operation to it in place, where there is one to apply; the two routes that
  // name no operation, the one every factory takes and the one that keeps a range, stop at the
  // copy. Rewriting the clone leaves what a fresh buffer filled from the same source would have
  // held, because the clone begins as a copy of that source.
  //
  // Each loop is a private method of this class, so it is emitted private and may read another
  // instance's storage directly, and none of them reads this instance's own field: the field is
  // what they are producing. The rewrite is consumed here and nowhere else, which is what keeps
  // the compiler from retaining it in a field of every instance.
  private val array: Array[Double] = rewrite match {
    case _: DoubleArray.NoRewrite =>
      values.clone()
    case range: DoubleArray.CopyRange =>
      Arrays.copyOfRange(values, range.fromIndexInclusive, range.toIndexExclusive)
    case single: DoubleArray.SetAt =>
      val storage = values.clone()
      storage(single.index) = single.newValue
      storage
    case added: DoubleArray.PlusScalar =>
      val storage = values.clone()
      plusInto(storage, added.amount, 0)
      storage
    case subtracted: DoubleArray.MinusScalar =>
      val storage = values.clone()
      minusInto(storage, subtracted.amount, 0)
      storage
    case scaled: DoubleArray.ScaledBy =>
      val storage = values.clone()
      scaledInto(storage, scaled.factor, 0)
      storage
    case mapped: DoubleArray.Mapped =>
      val storage = values.clone()
      mapInto(storage, mapped.operator, 0)
      storage
    case indexed: DoubleArray.MappedWithIndex =>
      val storage = values.clone()
      mapWithIndexInto(storage, indexed.function, 0)
      storage
    case summed: DoubleArray.PlusEach =>
      val storage = values.clone()
      plusEachInto(storage, summed.other.array, 0)
      storage
    case differenced: DoubleArray.MinusEach =>
      val storage = values.clone()
      minusEachInto(storage, differenced.other.array, 0)
      storage
    case multiplied: DoubleArray.MultipliedByEach =>
      val storage = values.clone()
      multipliedByEachInto(storage, multiplied.other.array, 0)
      storage
    case divided: DoubleArray.DividedByEach =>
      val storage = values.clone()
      dividedByEachInto(storage, divided.other.array, 0)
      storage
    case combined: DoubleArray.CombinedWith =>
      val storage = values.clone()
      combineEachInto(storage, combined.other.array, combined.operator, 0)
      storage
    case _: DoubleArray.SortedRun =>
      val storage = values.clone()
      Arrays.sort(storage)
      storage
  }

  // Constructs the value this array's storage, rewritten by the given operation, describes.
  //
  // Every operation of this class that produces an array of the same length as this one answers
  // through here, which is where the one short circuit they share lives: an operation on an array
  // of no elements has no element to rewrite, so it answers with the shared empty instance, as
  // every factory of this type does. The storage this instance holds is passed to the
  // constructor, which copies it rather than keeping it, so the two values share nothing.
  private def rewritten(rewrite: DoubleArray.Rewrite): DoubleArray =
    if (array.length == 0) {
      DoubleArray.EMPTY
    } else {
      new DoubleArray(array, rewrite)
    }

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
   * @throws java.lang.IndexOutOfBoundsException if the index is outside this array
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
   * @throws java.lang.IllegalArgumentException if the start index is beyond the end of this array,
   *   where the Java original raised an index exception
   * @throws java.lang.IndexOutOfBoundsException if the start index is negative
   */
  def subArray(fromIndexInclusive: Int): DoubleArray =
    subArray(fromIndexInclusive, array.length)

  /**
   * Returns an array holding the values between the specified from and to indices.
   *
   * @param fromIndexInclusive  the start index of the array to copy from
   * @param toIndexExclusive  the end index of the array to copy to
   * @return an array holding the values between the two indices
   * @throws java.lang.IllegalArgumentException if either index is beyond the end of this array, where the
   *   Java original raised an index exception, or if the start index is after the end index,
   *   which includes a negative end index
   * @throws java.lang.IndexOutOfBoundsException if the start index is negative
   */
  def subArray(fromIndexInclusive: Int, toIndexExclusive: Int): DoubleArray =
    DoubleArray.copyOf(array, fromIndexInclusive, toIndexExclusive)

  //-------------------------------------------------------------------------
  /**
   * Converts this instance to an independent array of doubles.
   *
   * The result is a copy, so a caller is free to modify it without affecting this instance, and
   * a fresh copy is made on every call, so two calls hand back two arrays. This is the only
   * member of this type that answers with an array of values at all: every other member answers
   * with an element, a count, a collection built for the call, or another array of this type,
   * which is what leaves the stored run of values unreachable from outside the class.
   *
   * @return a copy of the underlying array
   */
  def toArray: Array[Double] = array.clone()

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
   * @throws java.lang.IndexOutOfBoundsException if the index is outside this array
   */
  def `with`(index: Int, newValue: Double): DoubleArray =
    if (DoubleArray.bitsOf(array(index)) == DoubleArray.bitsOf(newValue)) {
      this
    } else {
      rewritten(new DoubleArray.SetAt(index, newValue))
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
      rewritten(new DoubleArray.PlusScalar(amount))
    }

  // adds the amount to each element of the target from the index upwards, in index order
  //
  // The target is the storage of the value being constructed, which the constructor has just
  // cloned from this array, so adding the amount to the element already there is adding it to the
  // element of this array at the same index.
  @tailrec
  private def plusInto(target: Array[Double], amount: Double, index: Int): Unit =
    if (index < target.length) {
      target(index) = target(index) + amount
      plusInto(target, amount, index + 1)
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
      rewritten(new DoubleArray.MinusScalar(amount))
    }

  // subtracts the amount from each element of the target from the index upwards, in index order,
  // the target being the clone of this array the constructor is producing
  @tailrec
  private def minusInto(target: Array[Double], amount: Double, index: Int): Unit =
    if (index < target.length) {
      target(index) = target(index) - amount
      minusInto(target, amount, index + 1)
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
      rewritten(new DoubleArray.ScaledBy(factor))
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
      // the reciprocal is computed once, here, and each element is multiplied by it below, which
      // is what the Java original does and differs in the last bits from dividing each element
      rewritten(new DoubleArray.ScaledBy(1 / divisor))
    }

  // multiplies each element of the target by the factor from the index upwards, in index order,
  // the target being the clone of this array the constructor is producing; shared by multipliedBy
  // and dividedBy because the original multiplies in both cases
  @tailrec
  private def scaledInto(target: Array[Double], factor: Double, index: Int): Unit =
    if (index < target.length) {
      target(index) = target(index) * factor
      scaledInto(target, factor, index + 1)
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
  def map(operator: Double => Double): DoubleArray = rewritten(new DoubleArray.Mapped(operator))

  // applies the operator to each element of the target from the index upwards, in index order,
  // the target being the clone of this array the constructor is producing
  @tailrec
  private def mapInto(target: Array[Double], operator: Double => Double, index: Int): Unit =
    if (index < target.length) {
      target(index) = operator(target(index))
      mapInto(target, operator, index + 1)
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
  def mapWithIndex(function: (Int, Double) => Double): DoubleArray =
    rewritten(new DoubleArray.MappedWithIndex(function))

  // applies the function to each indexed element of the target from the index upwards, in index
  // order, the target being the clone of this array the constructor is producing
  @tailrec
  private def mapWithIndexInto(
      target: Array[Double],
      function: (Int, Double) => Double,
      index: Int): Unit =

    if (index < target.length) {
      target(index) = function(index, target(index))
      mapWithIndexInto(target, function, index + 1)
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
   * @throws java.lang.IllegalArgumentException if the arrays have different sizes
   */
  def plus(other: DoubleArray): DoubleArray = {
    ArgCheck.isTrue(array.length == other.array.length, DoubleArray.differentSizes)
    rewritten(new DoubleArray.PlusEach(other))
  }

  // adds the matching element of the other array to each element of the target from the index
  // upwards, in index order. The target is the clone of this array the constructor is producing
  // and the other array is another instance's storage, which is only read: the two are distinct
  // arrays whatever the two values are, because each was allocated by the constructor that keeps
  // it, so this loop cannot observe its own writes through the other array.
  @tailrec
  private def plusEachInto(target: Array[Double], other: Array[Double], index: Int): Unit =
    if (index < target.length) {
      target(index) = target(index) + other(index)
      plusEachInto(target, other, index + 1)
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
   * @throws java.lang.IllegalArgumentException if the arrays have different sizes
   */
  def minus(other: DoubleArray): DoubleArray = {
    ArgCheck.isTrue(array.length == other.array.length, DoubleArray.differentSizes)
    rewritten(new DoubleArray.MinusEach(other))
  }

  // subtracts the matching element of the other array from each element of the target from the
  // index upwards, in index order, the target being the clone of this array the constructor is
  // producing and the other array another instance's storage, which is only read
  @tailrec
  private def minusEachInto(target: Array[Double], other: Array[Double], index: Int): Unit =
    if (index < target.length) {
      target(index) = target(index) - other(index)
      minusEachInto(target, other, index + 1)
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
   * @throws java.lang.IllegalArgumentException if the arrays have different sizes
   */
  def multipliedBy(other: DoubleArray): DoubleArray = {
    ArgCheck.isTrue(array.length == other.array.length, DoubleArray.differentSizes)
    rewritten(new DoubleArray.MultipliedByEach(other))
  }

  // multiplies each element of the target by the matching element of the other array from the
  // index upwards, in index order, the target being the clone of this array the constructor is
  // producing and the other array another instance's storage, which is only read
  @tailrec
  private def multipliedByEachInto(target: Array[Double], other: Array[Double], index: Int): Unit =
    if (index < target.length) {
      target(index) = target(index) * other(index)
      multipliedByEachInto(target, other, index + 1)
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
   * @throws java.lang.IllegalArgumentException if the arrays have different sizes
   */
  def dividedBy(other: DoubleArray): DoubleArray = {
    ArgCheck.isTrue(array.length == other.array.length, DoubleArray.differentSizes)
    rewritten(new DoubleArray.DividedByEach(other))
  }

  // divides each element of the target by the matching element of the other array from the index
  // upwards, in index order, the target being the clone of this array the constructor is producing
  // and the other array another instance's storage, which is only read
  @tailrec
  private def dividedByEachInto(target: Array[Double], other: Array[Double], index: Int): Unit =
    if (index < target.length) {
      target(index) = target(index) / other(index)
      dividedByEachInto(target, other, index + 1)
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
   * @throws java.lang.IllegalArgumentException if the arrays have different sizes
   */
  def combine(other: DoubleArray, operator: (Double, Double) => Double): DoubleArray = {
    ArgCheck.isTrue(array.length == other.array.length, DoubleArray.differentSizes)
    rewritten(new DoubleArray.CombinedWith(other, operator))
  }

  // replaces each element of the target from the index upwards with the operator applied to it
  // and the matching element of the other array, in index order, the target being the clone of
  // this array the constructor is producing and the other array another instance's storage, which
  // is only read
  @tailrec
  private def combineEachInto(
      target: Array[Double],
      other: Array[Double],
      operator: (Double, Double) => Double,
      index: Int): Unit =

    if (index < target.length) {
      target(index) = operator(target(index), other(index))
      combineEachInto(target, other, operator, index + 1)
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
   * @throws java.lang.IllegalArgumentException if the arrays have different sizes
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
   * The elements are gathered once, at their final length, and each source is moved in one bulk
   * copy: the stored elements into the front, the supplied values into the tail. Asking the
   * sequence for an array of its own first would move those values twice before the gathering
   * even began, once into that array and once out of it again.
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
      // the two sources are gathered into one run of values here, each in a single bulk move, and
      // the factory below turns that run into the result; `copyToArray` answers with the number of
      // elements it moved, which is the length of the sequence and so tells us nothing we do not
      // know, and the value is bound and discarded because an ignored result is a warning
      val result = new Array[Double](array.length + values.length)
      System.arraycopy(array, 0, result, 0, array.length)
      val _ = values.copyToArray(result, array.length)
      DoubleArray.copyOf(result)
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
    DoubleArray.copyOf(result)
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
      rewritten(DoubleArray.SortedRun)
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
   * @throws java.lang.IllegalArgumentException if this array is empty
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
   * @throws java.lang.IllegalArgumentException if this array is empty
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
   * comparison this delegates to, and is documented there in full. Two parts of it are worth
   * repeating, because they decide whether this member agrees with `equals`: a not-a-number
   * element is equal to no element at all, so an array holding one is ''not'' equal to itself
   * within any tolerance, where under `equals` it is; and an infinite tolerance brings any two
   * finite elements together but brings nothing to an infinite or a not-a-number element.
   *
   * @param other  the other array
   * @param tolerance  the tolerance to use, zero or greater
   * @return true if the arrays are equal up to the tolerance
   * @throws java.lang.IllegalArgumentException if the tolerance is negative or is not a number
   */
  def equalWithTolerance(other: DoubleArray, tolerance: Double): Boolean =
    DoubleArrayMath.fuzzyEquals(array, other.array, tolerance)

  /**
   * Checks whether every value in this array equals zero within the specified tolerance.
   *
   * An empty array holds no value that differs from zero and so is equal to zero. Each
   * element is compared with zero by the same comparison `equalWithTolerance` uses, so a
   * not-a-number element is never equal to zero and an infinite element is never equal to
   * zero either, at any tolerance; every finite element is equal to zero at an infinite
   * tolerance.
   *
   * @param tolerance  the tolerance to use, zero or greater
   * @return true if every value is equal to zero up to the tolerance
   * @throws java.lang.IllegalArgumentException if the tolerance is negative or is not a number
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

  //-------------------------------------------------------------------------
  // The construction routes of this type: which storage the sole constructor is to produce, and
  // which operation it is to apply to that storage before the value is published.
  //
  // These are internal to the package - the two specs of this package name them, and nothing
  // else does - and they exist so that an operation producing a run of values costs one
  // allocation instead of two. Filling a fresh buffer and handing it over cost the buffer plus
  // the copy the constructor makes of it; naming the operation instead lets the constructor
  // produce the storage itself and rewrite it in place, so the copy IS the result buffer.
  //
  // Three properties of the family are deliberate, and each is a property of the compiled form
  // rather than of a convention:
  //
  //   - no descriptor carries a run of values. Each carries a scalar, an index, a function, or
  //     another value of this type; a descriptor holding an `Array[Double]` would publish an
  //     accessor handing that array out, which for a binary operation is another value's
  //     storage, and the aliasing this type does not have would be back;
  //   - a descriptor selects a loop and nothing else. The constructor matches once per value
  //     constructed, and the loop it selects is that operation's own specialised loop, so the
  //     dispatch is one type test per operation rather than a virtual call per element;
  //   - no descriptor can make the constructor keep the array it is handed. Every branch of the
  //     field initialiser produces storage - a clone of the argument, or a copy of a range of it
  //     - so any caller the bytecode admits, in any language, that reaches the constructor with
  //     an array it retains gets a value that copied it. There is no adopting route to reach,
  //     and so none to defend.
  //
  // They are declared ahead of `EMPTY` because that value constructs through this family, and a
  // `val` of an object is initialised in the order the object declares it.
  private[array] sealed abstract class Rewrite

  // Keep the values as they are: the storage is a clone of the run the constructor was handed.
  // This is the route of every factory, and of the shared empty instance.
  private[array] final class NoRewrite extends Rewrite

  // Keep a range of the values: the storage is a copy of `[fromIndexInclusive, toIndexExclusive)`
  // of the run the constructor was handed, which is the whole of what `copyOf` of a range and
  // `subArray` produce.
  private[array] final class CopyRange(val fromIndexInclusive: Int, val toIndexExclusive: Int)
      extends Rewrite

  // Replace the value at one index, which is what `with` produces.
  private[array] final class SetAt(val index: Int, val newValue: Double) extends Rewrite

  // Add the amount to every element, which is what `plus` of a scalar produces.
  private[array] final class PlusScalar(val amount: Double) extends Rewrite

  // Subtract the amount from every element, which is what `minus` of a scalar produces.
  private[array] final class MinusScalar(val amount: Double) extends Rewrite

  // Multiply every element by the factor, which is what `multipliedBy` of a scalar produces and,
  // through the reciprocal it computes once, what `dividedBy` of a scalar produces.
  private[array] final class ScaledBy(val factor: Double) extends Rewrite

  // Apply the operator to every element, which is what `map` produces.
  private[array] final class Mapped(val operator: Double => Double) extends Rewrite

  // Apply the function to every indexed element, which is what `mapWithIndex` produces.
  private[array] final class MappedWithIndex(val function: (Int, Double) => Double) extends Rewrite

  // Add the matching element of the other array to every element, which is what `plus` of an
  // array produces. The other value is carried whole rather than as its storage, which is what
  // keeps this descriptor from having an array to hand out.
  private[array] final class PlusEach(val other: DoubleArray) extends Rewrite

  // Subtract the matching element of the other array from every element, which is what `minus`
  // of an array produces.
  private[array] final class MinusEach(val other: DoubleArray) extends Rewrite

  // Multiply every element by the matching element of the other array, which is what
  // `multipliedBy` of an array produces.
  private[array] final class MultipliedByEach(val other: DoubleArray) extends Rewrite

  // Divide every element by the matching element of the other array, which is what `dividedBy`
  // of an array produces.
  private[array] final class DividedByEach(val other: DoubleArray) extends Rewrite

  // Combine every element with the matching element of the other array, which is what `combine`
  // produces.
  private[array] final class CombinedWith(
      val other: DoubleArray,
      val operator: (Double, Double) => Double) extends Rewrite

  // Sort the values, which is what `sorted` produces.
  private[array] final class SortedRun extends Rewrite

  // The two routes that carry nothing are single instances, so naming either of them costs no
  // allocation: every factory of this type names the first, and `sorted` names the second.
  private[array] val NoRewrite: NoRewrite = new NoRewrite
  private[array] val SortedRun: SortedRun = new SortedRun

  //-------------------------------------------------------------------------
  /**
   * An empty array.
   *
   * Every factory that has no elements to produce answers with this instance, so the common
   * empty result costs no allocation and can be recognised by identity as well as by equality.
   */
  val EMPTY: DoubleArray = new DoubleArray(new Array[Double](0), NoRewrite)

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
   * The values are held in the order they are supplied, however many of them there are:
   *
   * {{{
   * val empty = DoubleArray.of()             // the empty array
   * val three = DoubleArray.of(1.0, 2.0, 3.0)
   * val many = DoubleArray.of(readings: _*)  // a sequence of the caller's own, expanded
   * }}}
   *
   * A call supplying no values is the shared empty instance, which a caller may recognise by
   * identity as well as by equality. The sequence copies itself into a fresh array, so a caller
   * that expanded a sequence of its own into this call cannot reach the array the result holds.
   * This is the member a caller reaches for in place of allocating an array and wrapping it. A
   * caller that already holds an array, or any other collection, is better served by `copyOf`,
   * which takes it without a sequence in between, and a caller computing its values from their
   * positions by `tabulate`.
   *
   * @param values  the values to hold, in order
   * @return an array holding the specified values, the empty array if none are supplied
   */
  def of(values: Double*): DoubleArray = copyOf(values.toArray)

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
   * @throws java.lang.IllegalArgumentException if the size is negative
   */
  def tabulate(size: Int)(valueFunction: Int => Double): DoubleArray = {
    ArgCheck.notNegative(size, "size")
    if (size == 0) {
      EMPTY
    } else {
      val result = new Array[Double](size)
      tabulateInto(result, valueFunction, 0)
      new DoubleArray(result, NoRewrite)
    }
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
  def copyOf(collection: Iterable[Double]): DoubleArray = copyOf(collection.toArray)

  /**
   * Obtains an instance by copying an array of doubles.
   *
   * The input array is copied and never modified, so the caller may go on using it. The copy is
   * the one the constructor makes, which is why there is no second one here: every construction
   * path of this type produces its own storage, so a factory that copied first would move the
   * same values twice.
   *
   * That one copy is what buys the property this type rests on: no array anywhere - inside this
   * class or outside it - is reachable both by a caller and by an instance. The operations of
   * this class no longer pay it a second time, because each of them has the constructor produce
   * the storage and rewrite it in place rather than filling a buffer for the constructor to copy;
   * the construction paths that start from something other than a run of values - a boxed
   * sequence, a size, or two sources gathered at a new length - still hand over a buffer of their
   * own and are copied here, as the implementation notes of this type set out.
   *
   * @param array  the array to copy
   * @return an array holding the values of the specified array
   */
  def copyOf(array: Array[Double]): DoubleArray =
    if (array.length == 0) {
      EMPTY
    } else {
      new DoubleArray(array, NoRewrite)
    }

  /**
   * Obtains an instance by copying part of an array of doubles.
   *
   * The input array is copied and never modified, so the caller may go on using it.
   *
   * @param array  the array to copy
   * @param fromIndexInclusive  the index of the input array to copy from
   * @return an array holding the values from the index to the end of the input array
   * @throws java.lang.IllegalArgumentException if the start index is beyond the end of the input array,
   *   where the Java original raised an index exception
   * @throws java.lang.IndexOutOfBoundsException if the start index is negative
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
   * it, exactly as in the original - and that range copy is made by the constructor, from the
   * bounds this factory hands it, so the values are moved once rather than copied here and
   * cloned again.
   *
   * @param array  the array to copy
   * @param fromIndexInclusive  the start index of the input array to copy from
   * @param toIndexExclusive  the end index of the input array to copy to
   * @return an array holding the values between the two indices
   * @throws java.lang.IllegalArgumentException if either index is beyond the end of the input array, where
   *   the Java original raised an index exception, or if the start index is after the end index
   * @throws java.lang.IndexOutOfBoundsException if the start index is negative
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
      new DoubleArray(array, new CopyRange(fromIndexInclusive, toIndexExclusive))
    }
  }

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance with every entry equal to zero.
   *
   * @param size  the number of elements, zero or greater
   * @return an array of the specified size filled with zeroes
   * @throws java.lang.IllegalArgumentException if the size is negative
   */
  def filled(size: Int): DoubleArray = {
    ArgCheck.notNegative(size, "size")
    if (size == 0) {
      EMPTY
    } else {
      new DoubleArray(new Array[Double](size), NoRewrite)
    }
  }

  /**
   * Obtains an instance with every entry equal to the same value.
   *
   * @param size  the number of elements, zero or greater
   * @param value  the value of every element
   * @return an array of the specified size filled with the specified value
   * @throws java.lang.IllegalArgumentException if the size is negative
   */
  def filled(size: Int, value: Double): DoubleArray = {
    ArgCheck.notNegative(size, "size")
    if (size == 0) {
      EMPTY
    } else {
      val result = new Array[Double](size)
      Arrays.fill(result, value)
      new DoubleArray(result, NoRewrite)
    }
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
