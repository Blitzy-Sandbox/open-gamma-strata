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

/**
 * An immutable two-dimensional array of doubles.
 *
 * In mathematical terms this is a two-dimensional matrix. An instance wraps a nested primitive
 * array of doubles - rectangular in every ordinary case, every row having the same length - and
 * offers the operations a matrix needs: element access by row and column, row and column
 * extraction, scaling, mapping, element-wise arithmetic against another matrix of the same shape,
 * reduction, transposition and copy-on-write update. Each of those answers with a new value and
 * leaves its inputs alone:
 *
 * {{{
 * val base = DoubleMatrix.of(2, 2, 1.0, 2.0, 3.0, 4.0)
 * val scaled = base.multipliedBy(2.0)  // [[2.0, 4.0], [6.0, 8.0]], base unchanged
 * val total = scaled.total             // 20.0
 * }}}
 *
 * ===Shape===
 *
 * A matrix has a row count and a column count, both fixed at construction, and every member that
 * addresses an element by row and column is defined in terms of them. Every factory funnels a
 * zero row count or a zero column count to the single empty instance, which has no rows at all,
 * so no value can ever have a first row of length zero: a matrix is either empty or has at least
 * one element in its first row. `size` is the row count times the column count, computed once at
 * construction rather than on each call.
 *
 * '''Every value of this type is rectangular.''' The shape and the rows agree by construction,
 * because the one constructor every factory passes through measures them against each other: the
 * number of rows it is given must be the row count, and every row must hold exactly the column
 * count. So a position the shape names is a position the rows hold, for every value, and no
 * member has to allow for one that is not. The factories given a column count measure each row
 * against it as they build; `copyOf`, which instead reads the shape off the array it is given,
 * reaches the same check with the shape it read and reports an array whose rows disagree as the
 * caller error it is. This is the one place the port refuses input the Java original accepted -
 * that factory shaped a ragged array by its first row, leaving a matrix that misstated its own
 * shape - and the departure is recorded on `copyOf`, where it is visible.
 *
 * ===The stored rows never escape===
 *
 * Immutability here is enforced by the compiled code rather than promised by a convention, and
 * the enforcement rests on two facts about this class that hold together. The sole constructor
 * deep-copies the rows it is handed - the array of rows and every row within it - and stores the
 * copy, so a matrix's storage is allocated by that constructor and is reachable from nowhere the
 * caller can name; and no member hands that storage out. `toArray` answers with a deep copy, and
 * the four members that expose a single row or column - `row`, `rowArray`, `column` and
 * `columnArray` - each answer with independent data, a row cloned once and a column read element
 * by element into one buffer per call.
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
 * The Java original returned the stored row from `row`, handed out the array behind a freshly
 * built column, and published two further members that skipped the copy outright - `ofUnsafe`,
 * which adopted an array of rows, and `toArrayUnsafe`, which handed back the stored one. None of
 * those aliases survives here: the two members are not ported under any name or any visibility,
 * and the accessors copy. What that costs is one copy of a freshly built rectangle per operation,
 * which is small next to the arithmetic these matrices exist for; what it buys is that no array
 * anywhere is reachable both by a caller and by a matrix.
 *
 * `with` is the member where that cost is most visible, and it is paid there too. It clones the
 * row it changes rather than writing into the row it was derived from - a stored row is never
 * modified, which is what makes the derivation safe at all - and the constructor then copies
 * every row, so the result it answers with shares nothing with the matrix it came from.
 *
 * ===Numerical fidelity===
 *
 * This type shares the numerical parity duty of this module with `DoubleArray`: its results are
 * compared element by element against the captured baseline values that the parity fixture of
 * this module holds, to an absolute and relative tolerance of 1e-9. Floating-point arithmetic is
 * neither associative nor distributive, so the order in which elements are visited, and the
 * exact form each expression takes, are part of the answer rather than implementation detail.
 * Every operation therefore visits elements in row-major order - row 0 left to right, then row 1,
 * and so on - every reduction accumulates sequentially from the documented starting value, and no
 * operation is rewritten into an algebraically equal but numerically different form: there is no
 * compensated summation, no reordering and no blocking anywhere in this class. One consequence is
 * observable: multiplying by one answers with the same instance rather than with a copy, so
 * `multipliedBy(1.0)` is not a way to obtain a distinct value.
 *
 * Scaling is the only arithmetic this type offers against a number, and there is deliberately no
 * matrix product: that operation belongs to the mathematics module built on top of this one.
 *
 * ===Equality===
 *
 * Two matrices are equal when they have the same shape and their elements agree bit for bit,
 * which is the comparison the rest of the library uses for every type holding doubles. A
 * not-a-number element is therefore equal to itself, so a matrix holding one can still be
 * compared and used as a map key, and a negative zero is not equal to a positive zero. Hashing
 * agrees with equality - it folds the hash of each row in row order - and the `Hash` instance in
 * the companion is the single equality-bearing instance of the type. There is deliberately no
 * ordering: matrices are compared for equality only, and no useful total order over them exists
 * to offer.
 *
 * ===Failures===
 *
 * Every failure this type reports is a caller-contract violation rather than a data-dependent
 * outcome, so each is raised rather than handed back as a value to inspect. There are two kinds,
 * and which kind a failure belongs to is what decides the exception it raises:
 *
 *   - a shape violation - a negative row, column or size count, values that do not fill the
 *     requested shape, a function or an array that supplies a row of the wrong length, or two
 *     matrices that have to match in shape and do not - is raised as an
 *     `IllegalArgumentException` through `ArgCheck`, carrying the message of the Java original
 *     word for word wherever that original threw one directly. A negative dimension is the one case where the port reports a
 *     different type from the Java original, which let the runtime raise
 *     `NegativeArraySizeException` from the allocation it had already begun: checking the
 *     dimension first is what keeps an invalid shape from allocating at all, and an invalid
 *     dimension is a caller-contract violation like any other, so it is reported like one;
 *   - an index outside the matrix surfaces as the index exception the runtime raises for the
 *     array access itself, with no check of this library's own standing in front of it.
 *
 * No member of this type returns an error as a value, because none of them can fail on the
 * ''data'' it is given: `total` and `reduce` are total functions of the elements, and a matrix
 * of any shape and any contents can be built, scaled, mapped, reduced and transposed. The one
 * argument the port refuses that the Java original accepted - an array of rows that cannot
 * describe one rectangle, handed to `copyOf` - is refused in the same way, as a caller-contract
 * violation, because rows that disagree with the shape they state are a broken caller and not a
 * matrix with unusual data.
 *
 * ===Implementation===
 *
 * The class holds no mutable state and declares no mutable variable: every loop is a
 * tail-recursive private method threading its row index, its column index and any running total
 * as parameters, which the compiler turns into the same jump a hand-written loop would produce.
 * One member owns mutable storage for the length of a single call - `toString` creates a string
 * builder, threads it through its two recursions as an argument and a result, and reads its
 * contents once at the end - and that is safe for the reason a freshly allocated array is safe:
 * it is published nowhere, no other thread can observe it, and it is unreachable the moment the
 * string it produced is returned. An operation that produces a matrix allocates its rows once,
 * fills them in row-major order and wraps them only once they are complete, and bulk moves -
 * copying, filling, hashing and cloning - go straight to the primitive array operations of the
 * platform. Every element-wise operation funnels through the one row-major fill in the companion,
 * so the traversal order that the parity duty above depends on is defined in a single place.
 *
 * Wrapping those completed rows copies them once more. Every one of those operations reaches the
 * sole constructor, whose single line deep-copies the array of rows and every row within it - the
 * comment on that line records why it is a copy rather than the argument - so a produced matrix
 * costs one further allocation and one bulk copy per row beyond the rectangle the operation
 * filled. That is the one copy of a freshly built rectangle per operation named above, and what
 * it buys is named there too: no array anywhere is reachable both by a caller and by a matrix.
 *
 * No internal path skips that copy, and none can, for the reason ''The stored rows never escape''
 * above gives. The constructor is emitted public because the companion every factory lives in has
 * to reach it, and a companion-private helper reached from this class is emitted as a public
 * method as well, under a mangled name - the deep copy this class reaches is emitted exactly that
 * way, which is why the build's check on the members of the two numeric types has to name it. A
 * member that adopted internally built rows instead of copying them would therefore be callable
 * by anything compiled against this class, outside the access the language grants its own
 * callers, and would be the aliasing entry point that `ofUnsafe` and `toArrayUnsafe` were and
 * this type deliberately has none of.
 *
 * No element and no index is boxed on any path of this type. Where a member takes a one- or
 * two-argument function - `map`, `multipliedBy`, `combine`, `reduce`, `tabulate` and `diagonal` -
 * the standard library specialises that function type over primitives and nothing has to be
 * done. Where a member needs a shape the standard library does not specialise, the callback is
 * declared here instead: `forEach` and `mapWithIndex` would each box two indices and a value on
 * every element through a three-argument function, and the two row factories would box a row
 * index on every row through a one-argument function whose result is a reference, so the four
 * take the single-abstract-method traits `ElementAction`, `ElementFunction`, `RowArrayFunction`
 * and `RowArrayObjectFunction` of the companion, whose compiled methods pass `int` and `double`
 * directly. A lambda at the call site is converted to the trait automatically, so those members
 * are called exactly as a member taking a plain function would be, and the absence of boxing is
 * a property of the compiled code rather than a claim about it: the build asserts it by
 * disassembling these methods and requiring no boxing call in any of them.
 *
 * This type carries no bean or serialization framework: there is no meta-object, no property or
 * builder machinery, no deserialization hook and no platform serialization support. Nothing in
 * this class reads or writes a type by reflection, and rendering a matrix to text or to JSON is
 * the business of other code.
 *
 * ===Thread safety===
 *
 * An instance is immutable, so it is safe to share between any number of threads without
 * synchronisation.
 *
 * @param rows  the rows to hold, which are copied rather than retained
 * @param rowCount  the number of rows, zero or greater
 * @param columnCount  the number of columns, zero or greater
 */
final class DoubleMatrix private (
    rows: Array[Array[Double]],
    val rowCount: Int,
    val columnCount: Int) extends Matrix {

  // The shape and the rows are measured against each other as this value is constructed, before
  // anything is copied.
  //
  // Here is where it belongs because here is where every construction path of this type meets:
  // the factories that are given a column count check each row as they build, and the factory
  // that reads its shape off an array checks nothing itself, so a check made at any one of them
  // would be a check some other path could avoid. Made at the constructor, it is a property of
  // the type - every value of it is rectangular, and a position the shape names is a position
  // the rows hold - which is what lets `get`, `transpose`, `equals`, the element-wise operations
  // and the rendering all be defined in terms of the shape alone.
  //
  // Both checks report the caller through `ArgCheck`, like every other shape violation of this
  // type, because a caller that hands over rows its stated shape does not describe is a broken
  // caller rather than a data-dependent outcome. The messages name the offending row and both
  // lengths, since the row is what has to be corrected and neither length is visible from the
  // other end of the call.
  //
  // They are made by a helper of the companion rather than written out here, and that is a
  // requirement of the compiled form rather than a matter of taste. The messages are built only
  // when a check fails, which means passing them unevaluated, and an unevaluated argument
  // written here would close over this constructor's own parameter - which the compiler answers
  // by keeping that parameter in a field of every instance. The rows of the caller would then be
  // held for as long as the matrix, defeating half the point of copying them. Passed to a helper
  // instead, the rows are an argument of that helper and nothing of the caller's outlives the
  // construction.
  DoubleMatrix.checkShape(rows, rowCount, columnCount)

  // The rows of this matrix, deep-copied out of whatever was handed to the constructor.
  //
  // This is the line that makes the type immutable in the compiled code, which is why it is a
  // copy and not the argument itself: every factory of the companion, and every operation that
  // produces a new matrix, reaches this constructor, so the rows stored here were allocated here
  // and are held by nothing else. The deep copy is the companion's own, so the one loop that
  // clones a run of rows serves both this and `toArray`. The field is private to the class and is
  // read only from within it - including from another instance of it, which the platform allows
  // and which `equals` and the element-wise operations do - so the compiler emits no accessor for
  // it beyond the private one, and there is no member of this type through which it can be
  // reached.
  private val array: Array[Array[Double]] = DoubleMatrix.deepClone(rows)

  //-------------------------------------------------------------------------
  /**
   * The number of dimensions of this matrix.
   *
   * A matrix of this type is two-dimensional, so this is always two.
   *
   * @return two
   */
  override def dimensions: Int = 2

  /**
   * The size of this matrix.
   *
   * This is the total number of elements, the number of rows multiplied by the number of
   * columns. It is computed once, when the matrix is built, as the Java original computes it.
   *
   * @return the total number of elements, zero or greater
   */
  override val size: Int = rowCount * columnCount

  /**
   * Checks whether this matrix is square.
   *
   * A square matrix has the same number of rows as columns, which makes the empty matrix
   * square.
   *
   * @return true if this matrix has as many rows as columns
   */
  def isSquare: Boolean = rowCount == columnCount

  /**
   * Checks whether this matrix is empty.
   *
   * @return true if this matrix holds no elements
   */
  def isEmpty: Boolean = size == 0

  //-------------------------------------------------------------------------
  /**
   * Gets the value at the specified row and column.
   *
   * Both indices are read straight from the stored rows, so an index outside the matrix fails
   * with the index exception the runtime raises rather than with a check of this library's own.
   *
   * @param row  the zero-based row index to retrieve
   * @param column  the zero-based column index to retrieve
   * @return the value at the row and column
   * @throws java.lang.IndexOutOfBoundsException if either index is outside this matrix
   */
  def get(row: Int, column: Int): Double = array(row)(column)

  //-------------------------------------------------------------------------
  /**
   * Gets the row at the specified index.
   *
   * The row is copied, so the result is independent of this matrix. The Java original wrapped
   * the stored row instead and relied on the caller not to write through it.
   *
   * @param row  the zero-based row index to retrieve
   * @return the row, as an independent array of doubles
   * @throws java.lang.IndexOutOfBoundsException if the row index is outside this matrix
   */
  def row(row: Int): DoubleArray = DoubleArray.copyOf(array(row))

  /**
   * Gets the row at the specified index as an independent primitive array.
   *
   * The array is a copy, so the caller may modify it freely without affecting this matrix.
   *
   * @param row  the zero-based row index to retrieve
   * @return the row, as a cloned array
   * @throws java.lang.IndexOutOfBoundsException if the row index is outside this matrix
   */
  def rowArray(row: Int): Array[Double] = array(row).clone()

  /**
   * Gets the column at the specified index.
   *
   * The column is built by reading one element from each row in turn into a buffer allocated for
   * the purpose, and the result is that buffer's contents: nothing the caller holds and nothing
   * this matrix holds is reachable from it. An empty matrix has no rows to read, so every column
   * index - including one that no matrix could hold - answers with the empty array, which is the
   * behaviour of the Java original, and it answers with the shared empty instance because a
   * column of no elements has nothing to hold.
   *
   * @param column  the zero-based column index to retrieve
   * @return the column, as an independent array of doubles
   * @throws java.lang.IndexOutOfBoundsException if the column index is outside a non-empty matrix
   */
  def column(column: Int): DoubleArray = DoubleArray.copyOf(columnCopy(column))

  /**
   * Gets the column at the specified index as an independent primitive array.
   *
   * The array is the buffer `column` reads its elements into, handed over directly: the two
   * members share one way of materialising a column, so neither reads an element twice. The
   * caller owns the result and may modify it freely without affecting this matrix.
   *
   * @param column  the zero-based column index to retrieve
   * @return the column, as an independent array
   * @throws java.lang.IndexOutOfBoundsException if the column index is outside a non-empty matrix
   */
  def columnArray(column: Int): Array[Double] = columnCopy(column)

  // Materialises one column into a freshly allocated array of the row count: this is the single
  // read behind both column accessors, and the buffer is never stored on this instance, so
  // `columnArray` can hand it out as it stands and `column` can hand it to the factory. An empty
  // matrix has no row to read, so the result is an array of no elements for any index at all.
  private def columnCopy(column: Int): Array[Double] = {
    val result = new Array[Double](rowCount)
    fillColumn(result, column, 0)
    result
  }

  // reads the column out of each row from the index upwards, in row order
  @tailrec
  private def fillColumn(result: Array[Double], column: Int, row: Int): Unit =
    if (row < result.length) {
      result(row) = array(row)(column)
      fillColumn(result, column, row + 1)
    }

  //-------------------------------------------------------------------------
  /**
   * Converts this matrix to an independent array of rows.
   *
   * Both the array of rows and each row within it are copies, so the caller may modify any part
   * of the result without affecting this matrix, and fresh copies are made on every call, so two
   * calls hand back two structures. This is the only member of this type that answers with an
   * array of rows at all: every other member answers with an element, a count, a single row or
   * column built for the call, or another matrix, which is what leaves the stored rows
   * unreachable from outside the class.
   *
   * @return an array of arrays holding a copy of the elements of this matrix
   */
  def toArray: Array[Array[Double]] = DoubleMatrix.deepClone(array)

  //-------------------------------------------------------------------------
  /**
   * Applies an action to each element of this matrix.
   *
   * The action receives the row index, the column index and the value, and is applied to the
   * elements in row-major order:
   *
   * {{{
   * base.forEach((row, column, value) => println(s"$row: $column: $value"))
   * }}}
   *
   * The action is a `DoubleMatrix.ElementAction`, a callback whose two indices and value are
   * primitives, so traversing a matrix of any size boxes nothing. A three-argument function of
   * the standard library would have boxed all three on every element, because that function type
   * is not specialised over primitives; the lambda above is converted to the callback type
   * automatically, so the call site is the one the Java original had.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param action  the action to apply to each row index, column index and value
   */
  def forEach(action: DoubleMatrix.ElementAction): Unit = forEachFrom(action, 0, 0)

  // applies the action to each element from the position upwards, in row-major order
  @tailrec
  private def forEachFrom(action: DoubleMatrix.ElementAction, row: Int, column: Int): Unit =
    if (row < rowCount) {
      if (column >= columnCount) {
        forEachFrom(action, row + 1, 0)
      } else {
        action(row, column, array(row)(column))
        forEachFrom(action, row, column + 1)
      }
    }

  //-------------------------------------------------------------------------
  /**
   * Returns an instance with the value at the specified row and column changed.
   *
   * The new value is compared with the stored one by bit pattern, so replacing a value with the
   * one already there answers with this instance rather than with a copy. Otherwise the row that
   * changes is cloned and the new value written into the clone, which is what keeps this matrix
   * unchanged - a stored row is never modified - and the constructor then copies every row, so
   * the result shares no storage with this matrix at all. The name is that of the Java original,
   * which is a reserved word here and so is written in backquotes.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param row  the zero-based row index to set
   * @param column  the zero-based column index to set
   * @param newValue  the new value to store at the row and column
   * @return a copy of this matrix with the value at the row and column changed
   * @throws java.lang.IndexOutOfBoundsException if either index is outside this matrix
   */
  def `with`(row: Int, column: Int, newValue: Double): DoubleMatrix =
    if (DoubleMatrix.bitsOf(array(row)(column)) == DoubleMatrix.bitsOf(newValue)) {
      this
    } else {
      val result = array.clone()
      val changed = result(row).clone()
      changed(column) = newValue
      result(row) = changed
      new DoubleMatrix(result, rowCount, columnCount)
    }

  //-------------------------------------------------------------------------
  /**
   * Returns an instance with each element multiplied by the specified factor.
   *
   * Multiplying by one answers with this instance, which is the fast path of the Java original
   * and is observable by identity. This is a special case of `map`.
   *
   * This is the only multiplication this type offers: it scales a matrix by a number, and there
   * is deliberately no product of two matrices, which the Java original does not have either.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param factor  the multiplicative factor
   * @return a copy of this matrix with each element multiplied by the factor
   */
  def multipliedBy(factor: Double): DoubleMatrix =
    if (factor == 1d) {
      this
    } else {
      DoubleMatrix.tabulate(rowCount, columnCount)((row, column) => array(row)(column) * factor)
    }

  /**
   * Returns an instance with an operation applied to each element of this matrix.
   *
   * The operator receives the value only and is applied to the elements in row-major order:
   *
   * {{{
   * val inverted = base.map(value => 1 / value)
   * }}}
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param operator  the operator to apply to each value
   * @return a copy of this matrix with the operator applied to each element
   */
  def map(operator: Double => Double): DoubleMatrix =
    DoubleMatrix.tabulate(rowCount, columnCount)((row, column) => operator(array(row)(column)))

  /**
   * Returns an instance with an operation applied to each indexed element of this matrix.
   *
   * The function receives the row index, the column index and the value, and is applied to the
   * elements in row-major order:
   *
   * {{{
   * val weighted = base.mapWithIndex((row, column, value) => row * (column + 1) * value)
   * }}}
   *
   * The function is a `DoubleMatrix.ElementFunction`, a callback whose two indices, value and
   * result are all primitives, so mapping a matrix of any size boxes nothing - as it boxes
   * nothing in `map` and `multipliedBy`, which take a one-argument function the standard library
   * specialises. A three-argument function of the standard library is not specialised, and would
   * have boxed all four on every element; the lambda above is converted to the callback type
   * automatically, so the call site is the one the Java original had.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param function  the function to apply to each row index, column index and value
   * @return a copy of this matrix with the function applied to each element
   */
  def mapWithIndex(function: DoubleMatrix.ElementFunction): DoubleMatrix =
    DoubleMatrix.tabulate(rowCount, columnCount)((row, column) =>
      function(row, column, array(row)(column)))

  //-------------------------------------------------------------------------
  /**
   * Returns an instance where each element is the sum of the matching elements of this matrix
   * and the other matrix.
   *
   * Element `(i,j)` of the result is element `(i,j)` of this matrix plus element `(i,j)` of the
   * other matrix, so the two matrices must have the same shape. This is a special case of
   * `combine`.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param other  the other matrix
   * @return a copy of this matrix with the matching elements added
   * @throws java.lang.IllegalArgumentException if the matrices have different shapes
   */
  def plus(other: DoubleMatrix): DoubleMatrix = {
    ArgCheck.isTrue(sameShapeAs(other), DoubleMatrix.differentSizes)
    DoubleMatrix.tabulate(rowCount, columnCount)((row, column) =>
      array(row)(column) + other.array(row)(column))
  }

  /**
   * Returns an instance where each element is the difference between the matching elements of
   * this matrix and the other matrix.
   *
   * Element `(i,j)` of the result is element `(i,j)` of this matrix minus element `(i,j)` of the
   * other matrix, so the two matrices must have the same shape. This is a special case of
   * `combine`.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param other  the other matrix
   * @return a copy of this matrix with the matching elements subtracted
   * @throws java.lang.IllegalArgumentException if the matrices have different shapes
   */
  def minus(other: DoubleMatrix): DoubleMatrix = {
    ArgCheck.isTrue(sameShapeAs(other), DoubleMatrix.differentSizes)
    DoubleMatrix.tabulate(rowCount, columnCount)((row, column) =>
      array(row)(column) - other.array(row)(column))
  }

  /**
   * Returns an instance where each element is formed by combining the matching elements of this
   * matrix and the other matrix.
   *
   * Element `(i,j)` of the result is the operator applied to element `(i,j)` of this matrix and
   * element `(i,j)` of the other matrix, so the two matrices must have the same shape. The
   * elements are visited in row-major order:
   *
   * {{{
   * val products = base.combine(other, (a, b) => a * b)
   * }}}
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param other  the other matrix
   * @param operator  the operator used to combine each pair of elements
   * @return a copy of this matrix combined with the other matrix
   * @throws java.lang.IllegalArgumentException if the matrices have different shapes
   */
  def combine(other: DoubleMatrix, operator: (Double, Double) => Double): DoubleMatrix = {
    ArgCheck.isTrue(sameShapeAs(other), DoubleMatrix.differentSizes)
    DoubleMatrix.tabulate(rowCount, columnCount)((row, column) =>
      operator(array(row)(column), other.array(row)(column)))
  }

  // whether the other matrix has the same number of rows and the same number of columns
  private def sameShapeAs(other: DoubleMatrix): Boolean =
    rowCount == other.rowCount && columnCount == other.columnCount

  //-------------------------------------------------------------------------
  /**
   * Returns the total of all the elements of this matrix.
   *
   * The elements are added in row-major order, starting from zero, so the total of an empty
   * matrix is zero. The order is part of the answer rather than an implementation detail,
   * because addition of doubles is not associative: a sum computed in any other order would
   * differ in its last digits from the value this port is required to reproduce. This is a
   * special case of `reduce`.
   *
   * @return the total of all the elements
   */
  def total: Double = totalFrom(0, 0, 0d)

  // adds each element from the position upwards to the running total, in row-major order
  @tailrec
  private def totalFrom(row: Int, column: Int, total: Double): Double =
    if (row >= rowCount) {
      total
    } else if (column >= columnCount) {
      totalFrom(row + 1, 0, total)
    } else {
      totalFrom(row, column + 1, total + array(row)(column))
    }

  /**
   * Reduces this matrix to a single value.
   *
   * The operator is called once per element, in row-major order. Its first argument is the
   * running total of the reduction, which starts at the identity supplied, and its second is the
   * element. An empty matrix reduces to the identity without the operator being called at all:
   *
   * {{{
   * val product = base.reduce(1.0, (total, value) => total * value)
   * }}}
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param identity  the identity value to start the reduction from
   * @param operator  the operator used to combine each element with the running total
   * @return the result of the reduction
   */
  def reduce(identity: Double, operator: (Double, Double) => Double): Double =
    reduceFrom(operator, 0, 0, identity)

  // applies the operator to the running total and each element, in row-major order
  @tailrec
  private def reduceFrom(
      operator: (Double, Double) => Double,
      row: Int,
      column: Int,
      total: Double): Double =

    if (row >= rowCount) {
      total
    } else if (column >= columnCount) {
      reduceFrom(operator, row + 1, 0, total)
    } else {
      reduceFrom(operator, row, column + 1, operator(total, array(row)(column)))
    }

  //-------------------------------------------------------------------------
  /**
   * Transposes this matrix.
   *
   * This converts a matrix of `m x n` into a matrix of `n x m`, moving each element to the
   * opposite position: element `(i,j)` of the result is element `(j,i)` of this matrix. An empty
   * matrix transposes to itself.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @return the transposed matrix
   */
  def transpose: DoubleMatrix =
    DoubleMatrix.tabulate(columnCount, rowCount)((row, column) => array(column)(row))

  //-------------------------------------------------------------------------
  /**
   * Checks whether this matrix equals another object.
   *
   * Another matrix is equal when it has the same shape and its elements agree bit for bit, so a
   * not-a-number element equals itself and the two zeroes are distinct. An object of any other
   * type is not equal.
   *
   * @param obj  the object to compare to
   * @return true if the other object is a matrix of the same shape holding the same elements
   */
  override def equals(obj: Any): Boolean = obj match {
    case other: DoubleMatrix =>
      (this eq other) || (sameShapeAs(other) && sameElementsAs(other.array, 0, 0))
    case _ => false
  }

  // whether every element from the position upwards has the same bit pattern in both matrices
  @tailrec
  private def sameElementsAs(other: Array[Array[Double]], row: Int, column: Int): Boolean =
    if (row >= rowCount) {
      true
    } else if (column >= columnCount) {
      sameElementsAs(other, row + 1, 0)
    } else if (DoubleMatrix.bitsOf(array(row)(column)) != DoubleMatrix.bitsOf(other(row)(column))) {
      false
    } else {
      sameElementsAs(other, row, column + 1)
    }

  /**
   * Returns a hash code consistent with `equals`.
   *
   * The hash of each row is folded into the result in row order, which is how the Java original
   * hashes a matrix.
   *
   * @return the hash code of the elements held
   */
  override def hashCode: Int = hashFrom(0, 1)

  // folds the hash of each row from the index upwards into the running result, in row order
  @tailrec
  private def hashFrom(row: Int, result: Int): Int =
    if (row >= rowCount) {
      result
    } else {
      hashFrom(row + 1, 31 * result + Arrays.hashCode(array(row)))
    }

  /**
   * Returns the elements of this matrix as text.
   *
   * The form is that of the Java original: each row holds its elements separated by single
   * spaces and is followed by a line break, so a two by two matrix renders over two lines and
   * the empty matrix renders as empty text. Each row is rendered at its own length, exactly as
   * the Java original rendered it, which for every value of this type is the column count of the
   * matrix, since the rows and the shape agree by construction.
   *
   * The whole rendering appends to one buffer, which is what keeps it free of per-element and
   * per-row garbage: appending a `Double` to a string builder takes the primitive, so no element
   * is boxed, and no text for a row exists apart from the text of the whole matrix. The buffer is
   * created here, threaded through the two recursions below as an argument and a result, and
   * released once its contents have been read, so it is owned by this one call and reachable
   * from nowhere else.
   *
   * @return the rendering of this matrix
   */
  override def toString: String = appendRows(new java.lang.StringBuilder, 0).toString

  // Appends each row from the index upwards to the builder, in row order, and answers with the
  // builder. Answering with it rather than relying on the append is what makes each step a value
  // the next step consumes, which is how this renders without a mutable local of its own.
  @tailrec
  private def appendRows(builder: java.lang.StringBuilder, row: Int): java.lang.StringBuilder =
    if (row >= rowCount) {
      builder
    } else {
      appendRows(appendRow(builder, array(row), 0), row + 1)
    }

  // appends one row from the column upwards, each element separated by a space and the last
  // followed by the line break that terminates every row, including the last row of the matrix
  //
  // The row is rendered at its own length, which is the length the shape of the matrix states:
  // the constructor measures the two against each other, so the rendering walks each row exactly
  // to its end whichever of the two it is written in terms of. Reading the length off the row is
  // what the Java original did, and it is kept because it is the local fact - this method is
  // handed a row and nothing else - rather than because a row of another length could arrive.
  @tailrec
  private def appendRow(
      builder: java.lang.StringBuilder,
      row: Array[Double],
      column: Int): java.lang.StringBuilder =

    if (column >= row.length) {
      builder
    } else {
      appendRow(
        builder.append(row(column)).append(if (column == row.length - 1) '\n' else ' '),
        row,
        column + 1)
    }
}

/**
 * Factories and typeclass instances for immutable two-dimensional arrays of doubles.
 *
 * Every factory here either answers with a matrix or fails on a caller-contract violation - a
 * negative dimension, values that do not fill the requested shape, a function that returns a row
 * of the wrong length, or an array whose rows cannot describe one rectangle. None of them reports
 * a failure as a value, because each of those is a broken caller rather than data a matrix could
 * not represent, so there is no validated form of construction here and no error to hand back.
 * `copyOf` is the only one whose refusal depends on the argument's shape rather than on numbers
 * the caller stated, and it is documented in full there.
 *
 * Every factory that is given its shape as numbers validates both dimensions as its first act,
 * before it allocates anything, before it decides whether the shape is empty and before it calls
 * any function it was passed. That order matters rather than being tidy: a negative column count
 * discovered after the array of rows had been allocated would already have reserved memory
 * proportional to a row count the caller chose, and a negative count discovered after a row
 * function had been called would already have run a caller's code, so an argument that can never
 * produce a matrix would still be able to consume resources. The factories that take their shape
 * from an array they are given - `copyOf` and `diagonal` - need no such check, because an array's
 * length cannot be negative, and neither has a stated column count to measure a row against:
 * each takes the shape the array it is given describes, which for `copyOf` is the length of the
 * array and the length of its first row.
 *
 * Every factory funnels a zero row count or a zero column count to `EMPTY`, so a matrix whose
 * column count is zero cannot be built, and a caller may recognise the empty result by identity
 * as well as by equality.
 */
object DoubleMatrix {

  /**
   * An empty matrix.
   *
   * It has no rows, no columns and no elements. Every factory that has no elements to produce
   * answers with this instance, so the common empty result costs no allocation.
   */
  val EMPTY: DoubleMatrix = new DoubleMatrix(new Array[Array[Double]](0), 0, 0)

  // The message reported when two matrices that have to match in shape do not. One definition
  // keeps it identical at every site, and identical to the message of the Java original, which
  // reaches logs and test expectations - including its wording in terms of arrays rather than
  // matrices.
  private val differentSizes: String = "Arrays have different sizes"

  //-------------------------------------------------------------------------
  /**
   * An action applied to one element of a matrix, identified by its position.
   *
   * This is the callback `forEach` takes. It exists in place of the three-argument function type
   * of the standard library because that type is generic in all three of its parameters: the
   * standard library specialises a function of one or two arguments over primitives, but not one
   * of three, so calling through it would box the two indices and the value once per element.
   * Declaring the shape here as a trait with a single abstract method whose parameters are
   * `Int`, `Int` and `Double` gives the compiled method the descriptor `(IID)V`, which passes
   * the position and the value in registers and allocates nothing.
   *
   * A caller does not name this type: a trait with one abstract method is a target for lambda
   * conversion, so a lambda written at the call site becomes an instance of it automatically and
   * reads exactly as a call on a function would:
   *
   * {{{
   * matrix.forEach((row, column, value) => println(s"$row: $column: $value"))
   * }}}
   */
  trait ElementAction {

    /**
     * Applies this action to one element.
     *
     * @param row  the zero-based row index of the element
     * @param column  the zero-based column index of the element
     * @param value  the value of the element
     */
    def apply(row: Int, column: Int, value: Double): Unit
  }

  /**
   * A function from one element of a matrix, identified by its position, to a new value.
   *
   * This is the callback `mapWithIndex` takes, and it exists for the reason `ElementAction`
   * does: a three-argument function of the standard library is generic in every parameter and
   * in its result, so a call through one would box the two indices, the value and the result.
   * The single abstract method declared here compiles to the descriptor `(IID)D`, so a mapping
   * over a matrix of any size boxes nothing at all.
   *
   * As with `ElementAction`, a caller writes a lambda and the compiler converts it:
   *
   * {{{
   * val weighted = matrix.mapWithIndex((row, column, value) => row * (column + 1) * value)
   * }}}
   */
  trait ElementFunction {

    /**
     * Applies this function to one element.
     *
     * @param row  the zero-based row index of the element
     * @param column  the zero-based column index of the element
     * @param value  the value of the element
     * @return the new value for that position
     */
    def apply(row: Int, column: Int, value: Double): Double
  }

  /**
   * A function from a row index to the elements of that row, as a primitive array.
   *
   * This is the callback `ofArrays` takes. A one-argument function of the standard library is
   * specialised over primitive ''results'' only - a function returning an array returns a
   * reference, so no specialisation applies - and its parameter would therefore be boxed once
   * per row. The single abstract method declared here compiles to the descriptor `(I)[D`, so
   * building a matrix row by row allocates nothing beyond the rows themselves.
   *
   * A caller writes a lambda and the compiler converts it:
   *
   * {{{
   * val matrix = DoubleMatrix.ofArrays(2, 2)(row => Array(row.toDouble, row.toDouble))
   * }}}
   */
  trait RowArrayFunction {

    /**
     * Returns the elements of one row.
     *
     * @param row  the zero-based row index
     * @return the elements of that row, which must number the column count of the matrix
     */
    def apply(row: Int): Array[Double]
  }

  /**
   * A function from a row index to the elements of that row, as an immutable array.
   *
   * This is the callback `ofArrayObjects` takes, and it exists for the reason
   * `RowArrayFunction` does: the row index would be boxed once per row if the callback were a
   * one-argument function of the standard library, whose result here is a reference type. The
   * single abstract method declared here compiles to a descriptor taking a primitive `int`.
   *
   * A caller writes a lambda and the compiler converts it:
   *
   * {{{
   * val matrix = DoubleMatrix.ofArrayObjects(2, 2)(row => rows(row))
   * }}}
   */
  trait RowArrayObjectFunction {

    /**
     * Returns the elements of one row.
     *
     * @param row  the zero-based row index
     * @return the elements of that row, which must number the column count of the matrix
     */
    def apply(row: Int): DoubleArray
  }

  //-------------------------------------------------------------------------
  /**
   * Obtains an empty instance.
   *
   * @return the empty matrix
   */
  def of(): DoubleMatrix = EMPTY

  /**
   * Obtains an instance with the specified shape and elements.
   *
   * The first two arguments give the shape. The remaining arguments give the elements, all of
   * row 0, then all of row 1, and so on, so there must be exactly `rows * columns` of them:
   *
   * {{{
   * val matrix = DoubleMatrix.of(2, 3, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
   * }}}
   *
   * Each dimension is checked for negativity first, then the count is checked before the shape is
   * examined, as in the Java original, so supplying the wrong number of elements for an empty
   * shape is reported rather than ignored. The expected count is computed as a long, because the
   * product of two large positive dimensions overflows an int and would otherwise compare equal
   * to a small number of elements.
   *
   * The elements are copied straight into the rows of the result, one bulk move per row from the
   * array the compiler builds behind the argument list, so the payload is written once and no
   * flat copy of the whole matrix exists at any point. A sequence of some other kind - which only
   * a caller expanding a collection of its own with `: _*` can supply - is drained through its
   * iterator into the same freshly allocated rows. Either way the rows of the result are the
   * only arrays the elements reach, so a caller that expanded an array of its own into this call
   * cannot reach them afterwards.
   *
   * @param rows  the number of rows, zero or greater
   * @param columns  the number of columns, zero or greater
   * @param values  the elements, row by row
   * @return a matrix of the specified shape holding the specified elements
   * @throws java.lang.IllegalArgumentException if either dimension is negative, or the number of elements
   *   is not `rows * columns`
   */
  def of(rows: Int, columns: Int, values: Double*): DoubleMatrix = {
    ArgCheck.notNegative(rows, "rows")
    ArgCheck.notNegative(columns, "columns")
    ArgCheck.isTrue(
      values.length.toLong == rows.toLong * columns.toLong,
      "Values array not of length rows * columns")
    if (rows == 0 || columns == 0) {
      EMPTY
    } else {
      val result = new Array[Array[Double]](rows)
      values match {
        case flat: scala.collection.immutable.ArraySeq.ofDouble =>
          fillFromFlat(result, flat.unsafeArray, columns, 0)
        case other =>
          fillFromElements(result, other.iterator, columns, 0)
      }
      new DoubleMatrix(result, rows, columns)
    }
  }

  // Copies each row from the index upwards out of a flat array of every element in row-major
  // order. This is the path a call with elements written out takes: the compiler gathers them
  // into one primitive array to pass them, which this reads directly, so each element is moved
  // exactly once, by the bulk copy of the platform, into a row the result owns.
  @tailrec
  private def fillFromFlat(
      result: Array[Array[Double]],
      source: Array[Double],
      columns: Int,
      row: Int): Unit =

    if (row < result.length) {
      val start = row * columns
      result(row) = Arrays.copyOfRange(source, start, start + columns)
      fillFromFlat(result, source, columns, row + 1)
    }

  // Fills each row from the index upwards from a sequence of elements in row-major order, taking
  // the elements from one iterator advanced across all the rows. This is the path a sequence
  // expanded at the call site takes, where the elements are already held boxed and no flat
  // primitive array exists to copy from; the iterator is drained exactly once, and its element
  // count has been checked against the shape before any of this runs.
  @tailrec
  private def fillFromElements(
      result: Array[Array[Double]],
      source: Iterator[Double],
      columns: Int,
      row: Int): Unit =

    if (row < result.length) {
      val inner = new Array[Double](columns)
      drainInto(inner, source, 0)
      result(row) = inner
      fillFromElements(result, source, columns, row + 1)
    }

  // takes the next element of the iterator into each position of one row, from the column upwards
  @tailrec
  private def drainInto(inner: Array[Double], source: Iterator[Double], column: Int): Unit =
    if (column < inner.length) {
      inner(column) = source.next()
      drainInto(inner, source, column + 1)
    }

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance with elements filled using a function.
   *
   * The function is passed the row and column index of each element in turn, in row-major order,
   * and returns the value for that element:
   *
   * {{{
   * val products = DoubleMatrix.tabulate(2, 2)((row, column) => ((row + 1) * (column + 1)).toDouble)
   * }}}
   *
   * The two parameter lists are what let the function be written as a block at the call site.
   * The Java original spelled this as another overload of its value factory, taking the shape and
   * a function together; as a named member it says what it does, and it is the member a caller
   * reaches for in place of building an array of rows and wrapping it.
   *
   * Every element-wise operation of this type is expressed in terms of this factory, so the
   * row-major traversal that the numerical parity of the port depends on is defined here alone.
   *
   * @param rows  the number of rows, zero or greater
   * @param columns  the number of columns, zero or greater
   * @param valueFunction  the function from row and column index to value
   * @return a matrix of the specified shape populated by the function
   * @throws java.lang.IllegalArgumentException if either dimension is negative
   */
  def tabulate(rows: Int, columns: Int)(valueFunction: (Int, Int) => Double): DoubleMatrix = {
    ArgCheck.notNegative(rows, "rows")
    ArgCheck.notNegative(columns, "columns")
    if (rows == 0 || columns == 0) {
      EMPTY
    } else {
      new DoubleMatrix(build(rows, columns, valueFunction), rows, columns)
    }
  }

  // builds the rows of a matrix of the specified shape, each element taken from the function
  private def build(
      rows: Int,
      columns: Int,
      valueFunction: (Int, Int) => Double): Array[Array[Double]] = {

    val result = new Array[Array[Double]](rows)
    buildRows(result, columns, valueFunction, 0)
    result
  }

  // builds each row from the index upwards, in row order, publishing a row only once it is full
  @tailrec
  private def buildRows(
      result: Array[Array[Double]],
      columns: Int,
      valueFunction: (Int, Int) => Double,
      row: Int): Unit =

    if (row < result.length) {
      val inner = new Array[Double](columns)
      fillRow(inner, valueFunction, row, 0)
      result(row) = inner
      buildRows(result, columns, valueFunction, row + 1)
    }

  // fills one row from the column upwards with the function applied to each position
  @tailrec
  private def fillRow(
      inner: Array[Double],
      valueFunction: (Int, Int) => Double,
      row: Int,
      column: Int): Unit =

    if (column < inner.length) {
      inner(column) = valueFunction(row, column)
      fillRow(inner, valueFunction, row, column + 1)
    }

  /**
   * Obtains an instance with rows filled using a function.
   *
   * The function is passed each row index in turn, in ascending order, and returns the elements
   * of that row, which must number exactly `columns`. Each array returned is copied, so the
   * function may reuse a buffer of its own.
   *
   * The function is a `DoubleMatrix.RowArrayFunction` rather than a one-argument function of the
   * standard library, which would have boxed the row index on every row: specialisation applies
   * to a function with a primitive result, and the result here is an array. A lambda written at
   * the call site is converted to it automatically.
   *
   * @param rows  the number of rows, zero or greater
   * @param columns  the number of columns, zero or greater
   * @param valuesFunction  the function from row index to the elements of that row
   * @return a matrix of the specified shape populated by the function
   * @throws java.lang.IllegalArgumentException if either dimension is negative, or the function returns a
   *   row of the wrong length
   */
  def ofArrays(rows: Int, columns: Int)(valuesFunction: RowArrayFunction): DoubleMatrix = {
    ArgCheck.notNegative(rows, "rows")
    ArgCheck.notNegative(columns, "columns")
    if (rows == 0 || columns == 0) {
      EMPTY
    } else {
      val result = new Array[Array[Double]](rows)
      fillFromArrays(result, columns, valuesFunction, 0)
      new DoubleMatrix(result, rows, columns)
    }
  }

  // takes each row from the index upwards from the function, checking its length and copying it
  @tailrec
  private def fillFromArrays(
      result: Array[Array[Double]],
      columns: Int,
      valuesFunction: RowArrayFunction,
      row: Int): Unit =

    if (row < result.length) {
      val values = valuesFunction(row)
      ArgCheck.isTrue(values.length == columns, incorrectLength(values.length, columns))
      result(row) = values.clone()
      fillFromArrays(result, columns, valuesFunction, row + 1)
    }

  /**
   * Obtains an instance with rows filled using a function.
   *
   * The function is passed each row index in turn, in ascending order, and returns the elements
   * of that row as an array of doubles, which must hold exactly `columns` elements. Each row is
   * read out of the array it arrives in through that type's copying accessor, so the function may
   * hand over a value it keeps.
   *
   * The function is a `DoubleMatrix.RowArrayObjectFunction` rather than a one-argument function
   * of the standard library, for the reason `ofArrays` takes its own callback type: the row index
   * would otherwise be boxed on every row. A lambda written at the call site is converted to it
   * automatically.
   *
   * @param rows  the number of rows, zero or greater
   * @param columns  the number of columns, zero or greater
   * @param valuesFunction  the function from row index to the elements of that row
   * @return a matrix of the specified shape populated by the function
   * @throws java.lang.IllegalArgumentException if either dimension is negative, or the function returns a
   *   row of the wrong length
   */
  def ofArrayObjects(
      rows: Int,
      columns: Int)(valuesFunction: RowArrayObjectFunction): DoubleMatrix = {

    ArgCheck.notNegative(rows, "rows")
    ArgCheck.notNegative(columns, "columns")
    if (rows == 0 || columns == 0) {
      EMPTY
    } else {
      val result = new Array[Array[Double]](rows)
      fillFromArrayObjects(result, columns, valuesFunction, 0)
      new DoubleMatrix(result, rows, columns)
    }
  }

  // takes each row from the index upwards from the function, checking its length and reading its
  // elements out through the copying accessor of the array type, which is the only member of that
  // type that answers with a run of values
  @tailrec
  private def fillFromArrayObjects(
      result: Array[Array[Double]],
      columns: Int,
      valuesFunction: RowArrayObjectFunction,
      row: Int): Unit =

    if (row < result.length) {
      val values = valuesFunction(row)
      ArgCheck.isTrue(values.size == columns, incorrectLength(values.size, columns))
      result(row) = values.toArray
      fillFromArrayObjects(result, columns, valuesFunction, row + 1)
    }

  // The message reported when a function returns a row of the wrong length. The wording is that
  // of the Java original, whose message template is replaced by interpolation the compiler checks.
  private def incorrectLength(actual: Int, expected: Int): String =
    s"Function returned array of incorrect length $actual, expected $expected"

  // Measures the rows the constructor was handed against the shape it was given, and reports the
  // first disagreement. This is the whole of the constructor's validation, gathered here so that
  // the messages - which are built only when a check fails - are closed over the parameters of
  // this method rather than over those of the constructor, for the reason recorded there.
  private def checkShape(rows: Array[Array[Double]], rowCount: Int, columnCount: Int): Unit = {
    ArgCheck.isTrue(rows.length == rowCount, rowCountMismatch(rows.length, rowCount))
    checkRectangular(rows, columnCount, 0)
  }

  // Checks every row against the column count the shape states, from the index upwards, and
  // reports the first row that disagrees. This is the check the constructor makes, so it runs
  // once per value built, before the rows are copied: a shape and a set of rows that cannot
  // describe one rectangle are refused rather than stored.
  @tailrec
  private def checkRectangular(rows: Array[Array[Double]], columns: Int, row: Int): Unit =
    if (row < rows.length) {
      ArgCheck.isTrue(rows(row).length == columns, rowLengthMismatch(row, rows(row).length, columns))
      checkRectangular(rows, columns, row + 1)
    }

  // The message reported when the number of rows handed over is not the row count stated.
  private def rowCountMismatch(actual: Int, expected: Int): String =
    s"Expected $expected rows in the matrix, but $actual were supplied"

  // The message reported when one row does not hold the number of elements the shape states. It
  // names the row and both lengths, which together say what to correct.
  private def rowLengthMismatch(row: Int, actual: Int, expected: Int): String =
    s"Expected every row of the matrix to hold $expected elements, but row $row holds $actual"

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance by copying an array of rows.
   *
   * Both the array of rows and each row within it are copied and never modified, so the caller
   * may go on using them. The shape is taken from the first row: the row count is the length of
   * the array and the column count is the length of its first row. An array with no rows, and an
   * array whose rows all hold no elements, are the empty matrix.
   *
   * ===An array whose rows differ in length is refused===
   *
   * This is the one factory of this type that is not total over its argument, and the one place
   * the port refuses input the Java original accepted. That original read the column count off
   * the first row and copied the remaining rows as they stood, so an array whose rows differed in
   * length produced a matrix that misstated its own shape: a position the shape promised but a
   * short row did not hold failed as an index error when it was read, an over-long row kept
   * elements the shape could not reach, and every member defined in terms of the shape had to
   * allow for both. Here the rows are measured against the shape read from the first of them, by
   * the constructor this factory reaches, and an array that cannot describe one rectangle is
   * reported as the caller error it is - naming the offending row and both lengths - before any
   * of it is copied.
   *
   * The order of the two decisions matters and is fixed. The empty short circuit of the Java
   * original is kept, so an array with no rows and an array whose rows are all empty are both
   * the shared empty instance; but the measurement comes first, which is what distinguishes
   * `[[], []]` from `[[], [1.0]]`. Both state a column count of zero from their first row, and in
   * the other order the second would collapse onto the empty matrix and lose the element it
   * holds - the quietest possible outcome for exactly the argument this refusal exists for. So
   * the first is the empty matrix and the second is refused.
   *
   * A caller holding an array of uncertain shape has two ways to be explicit instead: `ofArrays`,
   * which measures each row against a column count the caller states, and `tabulate`, which
   * builds the rows itself.
   *
   * @param array  the rows to copy
   * @return a matrix holding the elements of the specified rows, shaped by its first row
   * @throws IllegalArgumentException if the rows do not all hold as many elements as the first,
   *   where the Java original built a matrix that misstated its own shape
   */
  def copyOf(array: Array[Array[Double]]): DoubleMatrix = {
    val rows = array.length
    if (rows == 0) {
      EMPTY
    } else {
      // The rows are measured by the constructor, which is therefore reached before the empty
      // short circuit rather than after it: an array whose first row is empty and whose later
      // rows are not is refused here, where the other order would answer with the empty matrix.
      // Building the value and then answering with the shared empty instance costs the copy of
      // an array of empty rows, which is the price of deciding in that order.
      val copied = new DoubleMatrix(array, rows, array(0).length)
      if (copied.columnCount == 0) {
        EMPTY
      } else {
        copied
      }
    }
  }

  // Copies an array of rows, cloning each row, so that the result shares nothing with the input.
  // This is the copy the constructor makes of what it is handed, and the copy `toArray` hands
  // back, which is why one definition serves both.
  private def deepClone(input: Array[Array[Double]]): Array[Array[Double]] = {
    val cloned = new Array[Array[Double]](input.length)
    cloneRows(cloned, input, 0)
    cloned
  }

  // clones each row from the index upwards, in row order
  @tailrec
  private def cloneRows(
      cloned: Array[Array[Double]],
      input: Array[Array[Double]],
      row: Int): Unit =

    if (row < cloned.length) {
      cloned(row) = input(row).clone()
      cloneRows(cloned, input, row + 1)
    }

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance with every element equal to zero.
   *
   * @param rows  the number of rows, zero or greater
   * @param columns  the number of columns, zero or greater
   * @return a matrix of the specified shape filled with zeroes
   * @throws java.lang.IllegalArgumentException if either dimension is negative
   */
  def filled(rows: Int, columns: Int): DoubleMatrix = {
    ArgCheck.notNegative(rows, "rows")
    ArgCheck.notNegative(columns, "columns")
    if (rows == 0 || columns == 0) {
      EMPTY
    } else {
      new DoubleMatrix(rectangle(rows, columns), rows, columns)
    }
  }

  /**
   * Obtains an instance with every element equal to the same value.
   *
   * @param rows  the number of rows, zero or greater
   * @param columns  the number of columns, zero or greater
   * @param value  the value of every element
   * @return a matrix of the specified shape filled with the specified value
   * @throws java.lang.IllegalArgumentException if either dimension is negative
   */
  def filled(rows: Int, columns: Int, value: Double): DoubleMatrix = {
    ArgCheck.notNegative(rows, "rows")
    ArgCheck.notNegative(columns, "columns")
    if (rows == 0 || columns == 0) {
      EMPTY
    } else {
      val result = rectangle(rows, columns)
      fillWith(result, value, 0)
      new DoubleMatrix(result, rows, columns)
    }
  }

  // allocates the rows of a matrix of the specified shape, every element left at zero
  private def rectangle(rows: Int, columns: Int): Array[Array[Double]] = {
    val result = new Array[Array[Double]](rows)
    allocateRows(result, columns, 0)
    result
  }

  // allocates each row from the index upwards, in row order
  @tailrec
  private def allocateRows(result: Array[Array[Double]], columns: Int, row: Int): Unit =
    if (row < result.length) {
      result(row) = new Array[Double](columns)
      allocateRows(result, columns, row + 1)
    }

  // fills each row from the index upwards with the value, in row order
  @tailrec
  private def fillWith(result: Array[Array[Double]], value: Double, row: Int): Unit =
    if (row < result.length) {
      Arrays.fill(result(row), value)
      fillWith(result, value, row + 1)
    }

  //-------------------------------------------------------------------------
  /**
   * Obtains an identity matrix of the specified size.
   *
   * An identity matrix is square, with every element equal to zero except those on the primary
   * diagonal, which are one.
   *
   * @param size  the number of rows and columns, zero or greater
   * @return an identity matrix of the specified size
   * @throws java.lang.IllegalArgumentException if the size is negative
   */
  def identity(size: Int): DoubleMatrix = {
    ArgCheck.notNegative(size, "size")
    if (size == 0) {
      EMPTY
    } else {
      diagonalMatrix(size, _ => 1d)
    }
  }

  /**
   * Obtains a diagonal matrix holding the specified values.
   *
   * A diagonal matrix is square, with every element equal to zero except those on the primary
   * diagonal, which are taken in order from the specified array. A diagonal matrix of ones is
   * therefore the identity matrix of the same size.
   *
   * @param array  the values of the primary diagonal, in order
   * @return a diagonal matrix holding the specified values
   */
  def diagonal(array: DoubleArray): DoubleMatrix = {
    val size = array.size
    if (size == 0) {
      EMPTY
    } else {
      diagonalMatrix(size, index => array.get(index))
    }
  }

  // builds a square matrix whose primary diagonal is taken from the function
  private def diagonalMatrix(size: Int, valueFunction: Int => Double): DoubleMatrix = {
    val result = rectangle(size, size)
    fillDiagonal(result, valueFunction, 0)
    new DoubleMatrix(result, size, size)
  }

  // sets each diagonal element from the index upwards from the function, in row order
  @tailrec
  private def fillDiagonal(
      result: Array[Array[Double]],
      valueFunction: Int => Double,
      index: Int): Unit =

    if (index < result.length) {
      result(index)(index) = valueFunction(index)
      fillDiagonal(result, valueFunction, index + 1)
    }

  //-------------------------------------------------------------------------
  // The bit pattern of a value, which is how this type compares elements: it is the comparison
  // `equals` makes, so a not-a-number value is equal to itself and the two zeroes are distinct.
  private def bitsOf(value: Double): Long = java.lang.Double.doubleToLongBits(value)

  //-------------------------------------------------------------------------
  /**
   * The hashing of matrices, which is also their equality.
   *
   * This is the only equality-bearing instance of the type: `Hash` extends `Eq`, so declaring
   * `Eq` as well would leave two instances that could disagree and one of them ambiguous.
   * Hashing and equality are those of the shape and the elements themselves, element by element
   * and bit for bit, as `equals` and `hashCode` define them. There is deliberately no `Order`.
   *
   * @return the hashing of matrices
   */
  implicit val hash: Hash[DoubleMatrix] = Hash.fromUniversalHashCode[DoubleMatrix]

  /**
   * The rendering of matrices as text.
   *
   * A matrix renders one row per line, as `toString` does.
   *
   * @return the rendering of a matrix
   */
  implicit val show: Show[DoubleMatrix] = Show.show(_.toString)
}
