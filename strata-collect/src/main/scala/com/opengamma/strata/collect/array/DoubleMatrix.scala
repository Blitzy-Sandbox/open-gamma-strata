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
 * In mathematical terms this is a two-dimensional matrix. An instance wraps a rectangular
 * primitive array of doubles - every row has the same length - and offers the operations a
 * matrix needs: element access by row and column, row and column extraction, scaling, mapping,
 * element-wise arithmetic against another matrix of the same shape, reduction, transposition
 * and copy-on-write update. Each of those answers with a new value and leaves its inputs alone:
 *
 * {{{
 * val base = DoubleMatrix.of(2, 2, 1.0, 2.0, 3.0, 4.0)
 * val scaled = base.multipliedBy(2.0)  // [[2.0, 4.0], [6.0, 8.0]], base unchanged
 * val total = scaled.total             // 20.0
 * }}}
 *
 * ===Shape===
 *
 * A matrix is rectangular and its shape is fixed at construction. Every factory funnels a zero
 * row count or a zero column count to the single empty instance, which has no rows at all, so
 * no value can ever have rows of length zero: a matrix is either empty or has at least one
 * element in every row. `size` is the total number of elements, computed once at construction
 * rather than on each call, exactly as the Java original precomputes it.
 *
 * ===The stored array never escapes===
 *
 * Immutability here is enforced rather than promised. Every factory that is handed an array
 * copies it before storing it, `toArray` answers with a deep copy, and the two members that
 * expose a single row or column - `row`, `rowArray`, `column` and `columnArray` - each answer
 * with independent data. The Java original returned the stored row from `row` and handed out
 * the array behind a freshly built column, relying on a documented convention that callers
 * would not write to either; this port copies instead, so no caller can reach the array an
 * instance holds and change the value from underneath it. Two members skip the copy -
 * `ofUnsafe`, which adopts an array, and `toArrayUnsafe`, which hands back the stored one - and
 * both are visible only inside this module, where the array involved is known to be freshly
 * allocated and published nowhere else. Scoping them is what turns the convention of the
 * original into a rule the compiler keeps.
 *
 * The one place where data is deliberately shared is internal and invisible: `with` clones only
 * the row it changes and shares every other row with the instance it was derived from. That is
 * safe precisely because a stored row is never modified after construction.
 *
 * ===Numerical fidelity===
 *
 * This type shares the numerical parity duty of the port with `DoubleArray`: its results are
 * compared element by element against values captured from the Java original, to an absolute
 * and relative tolerance of 1e-9. Floating-point arithmetic is neither associative nor
 * distributive, so the order in which elements are visited, and the exact form each expression
 * takes, are part of the answer rather than implementation detail. Every operation therefore
 * visits elements in row-major order - row 0 left to right, then row 1, and so on - every
 * reduction accumulates sequentially from the documented starting value, and no operation is
 * rewritten into an algebraically equal but numerically different form: there is no compensated
 * summation, no reordering and no blocking anywhere in this class. One consequence is
 * observable: multiplying by one answers with the same instance rather than with a copy, so
 * `multipliedBy(1.0)` is not a way to obtain a distinct value.
 *
 * Scaling is the only arithmetic this type offers against a number, and there is deliberately no
 * matrix product: the Java original has none either, because that operation belongs to the
 * mathematics module built on top of this one.
 *
 * ===Equality===
 *
 * Two matrices are equal when they have the same shape and their elements agree bit for bit,
 * which is the comparison the Java original makes and the one the rest of the library uses for
 * every type holding doubles. A not-a-number element is therefore equal to itself, so a matrix
 * holding one can still be compared and used as a map key, and a negative zero is not equal to
 * a positive zero. Hashing agrees with equality - it folds the hash of each row in row order -
 * and the `Hash` instance in the companion is the single equality-bearing instance of the type.
 * There is deliberately no ordering: matrices are compared for equality only, and no useful
 * total order over them exists to offer.
 *
 * ===Failures===
 *
 * Every failure this type reports is a caller-contract violation rather than a data-dependent
 * outcome, so each is raised rather than handed back as a value to inspect. There are two kinds
 * and the port preserves the type of both:
 *
 *   - a shape violation - values that do not fill the requested shape, a function that returns a
 *     row of the wrong length, or two matrices that have to match in shape and do not - is
 *     raised as an `IllegalArgumentException` through `ArgCheck`, carrying the message of the
 *     Java original word for word, which is what that original threw directly;
 *   - an index outside the matrix surfaces as the index exception the runtime raises for the
 *     array access, which is a subclass of the exception the Java original documents.
 *
 * No member of this type returns an error as a value, because none of them can fail on the
 * ''data'' it is given: `total` and `reduce` are total functions of the elements, and a matrix
 * of any shape and any contents can be built, scaled, mapped, reduced and transposed.
 *
 * ===Implementation===
 *
 * The class holds no mutable state and declares no mutable local: every loop is a tail-recursive
 * private method threading its row index, its column index and any running total as parameters,
 * which the compiler turns into the same jump a hand-written loop would produce. An operation
 * that produces a matrix allocates its rows once, fills them in row-major order and wraps them
 * only once they are complete, and bulk moves - copying, filling, hashing and cloning - go
 * straight to the primitive array operations of the platform. Every element-wise operation
 * funnels through the one row-major fill in the companion, so the traversal order that the
 * parity duty above depends on is defined in a single place.
 *
 * No element is boxed on any of those paths, because the one- and two-argument function types
 * they use are specialised over primitives by the standard library. The two members that take a
 * three-argument function, `mapWithIndex` and `forEach`, are the exception: the standard library
 * does not specialise that function type, so a call through one boxes its two indices, its value
 * and its result. That cost is inherent to the function type rather than to this
 * implementation - it is the shape the corresponding members of the Java original expose - and
 * it is confined to those two members, each of which passes its function through exactly one
 * lambda.
 *
 * The bean and serialization framework that the Java original was built on is gone entirely:
 * there is no meta-object, no property or builder machinery, no deserialization hook and no
 * platform serialization support. Nothing in this class reads or writes a type by reflection,
 * and rendering a matrix to text or to JSON is the business of other code.
 *
 * ===Thread safety===
 *
 * An instance is immutable, so it is safe to share between any number of threads without
 * synchronisation.
 *
 * @param array  the underlying rows, which this instance owns and never modifies
 * @param rowCount  the number of rows, zero or greater
 * @param columnCount  the number of columns, zero or greater
 */
final class DoubleMatrix private (
    private val array: Array[Array[Double]],
    val rowCount: Int,
    val columnCount: Int) extends Matrix {

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
   * @throws IndexOutOfBoundsException if either index is outside this matrix
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
   * @throws IndexOutOfBoundsException if the row index is outside this matrix
   */
  def row(row: Int): DoubleArray = DoubleArray.copyOf(array(row))

  /**
   * Gets the row at the specified index as an independent primitive array.
   *
   * The array is a copy, so the caller may modify it freely without affecting this matrix.
   *
   * @param row  the zero-based row index to retrieve
   * @return the row, as a cloned array
   * @throws IndexOutOfBoundsException if the row index is outside this matrix
   */
  def rowArray(row: Int): Array[Double] = array(row).clone()

  /**
   * Gets the column at the specified index.
   *
   * The column is built by reading one element from each row in turn, so the result is
   * independent of this matrix. An empty matrix has no rows to read, so every column index -
   * including one that no matrix could hold - answers with the empty array, which is the
   * behaviour of the Java original.
   *
   * @param column  the zero-based column index to retrieve
   * @return the column, as an independent array of doubles
   * @throws IndexOutOfBoundsException if the column index is outside a non-empty matrix
   */
  def column(column: Int): DoubleArray = DoubleArray.tabulate(rowCount)(row => array(row)(column))

  /**
   * Gets the column at the specified index as an independent primitive array.
   *
   * The array is a copy, so the caller may modify it freely without affecting this matrix.
   *
   * @param column  the zero-based column index to retrieve
   * @return the column, as a cloned array
   * @throws IndexOutOfBoundsException if the column index is outside a non-empty matrix
   */
  def columnArray(column: Int): Array[Double] = this.column(column).toArray

  //-------------------------------------------------------------------------
  /**
   * Converts this matrix to an independent array of rows.
   *
   * Both the array of rows and each row within it are copies, so the caller may modify any part
   * of the result without affecting this matrix.
   *
   * @return an array of arrays holding a copy of the elements of this matrix
   */
  def toArray: Array[Array[Double]] = DoubleMatrix.deepClone(array)

  /**
   * Returns the underlying rows without copying them.
   *
   * This is visible only inside this module, because it makes the caller responsible for the
   * immutability of this matrix: the rows returned are the ones this instance holds, and
   * modifying any of them would change a value that other code may already be using. Inside
   * the module it exists so that code which only reads the elements - rendering a matrix, or
   * writing it to a serialized form - can do so without paying for a copy of every row.
   *
   * @return the stored rows, which the caller must never modify
   */
  private[collect] def toArrayUnsafe: Array[Array[Double]] = array

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
   * The action is called through a three-argument function type, which the standard library does
   * not specialise over primitives, so each call boxes the two indices and the value. That is
   * the cost of the function shape the Java original exposes, and it is the reason the
   * element-wise operations of this type, which do not need it, use narrower function types.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param action  the action to apply to each row index, column index and value
   */
  def forEach(action: (Int, Int, Double) => Unit): Unit = forEachFrom(action, 0, 0)

  // applies the action to each element from the position upwards, in row-major order
  @tailrec
  private def forEachFrom(action: (Int, Int, Double) => Unit, row: Int, column: Int): Unit =
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
   * one already there answers with this instance rather than with a copy. Otherwise only the row
   * that changes is copied and every other row is shared with this matrix, which is safe because
   * a stored row is never modified. The name is that of the Java original, which is a reserved
   * word here and so is written in backquotes.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param row  the zero-based row index to set
   * @param column  the zero-based column index to set
   * @param newValue  the new value to store at the row and column
   * @return a copy of this matrix with the value at the row and column changed
   * @throws IndexOutOfBoundsException if either index is outside this matrix
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
   * The function is called through a three-argument function type, which the standard library
   * does not specialise over primitives, so each call boxes the two indices, the value and the
   * result. That is the cost of the function shape the Java original exposes; `map` and
   * `multipliedBy` take narrower function types and box nothing.
   *
   * This instance is immutable and unaffected by this method.
   *
   * @param function  the function to apply to each row index, column index and value
   * @return a copy of this matrix with the function applied to each element
   */
  def mapWithIndex(function: (Int, Int, Double) => Double): DoubleMatrix =
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
   * @throws IllegalArgumentException if the matrices have different shapes
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
   * @throws IllegalArgumentException if the matrices have different shapes
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
   * @throws IllegalArgumentException if the matrices have different shapes
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
   * the empty matrix renders as empty text. The Java original built this by appending to a
   * mutable buffer; joining the rows produces the same characters without one.
   *
   * @return the rendering of this matrix
   */
  override def toString: String = array.iterator.map(row => row.mkString(" ") + "\n").mkString
}

/**
 * Factories and typeclass instances for immutable two-dimensional arrays of doubles.
 *
 * Construction is total: every factory here either answers with a matrix or fails on a
 * caller-contract violation - values that do not fill the requested shape, or a function that
 * returns a row of the wrong length - and none of them can reject the data it is given, so there
 * is no validated form of construction and no error to hand back as a value.
 *
 * Every factory funnels a zero row count or a zero column count to `EMPTY`, so a matrix with
 * rows of length zero cannot be built, and a caller may recognise the empty result by identity
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
   * The count is checked before the shape is examined, as in the Java original, so supplying the
   * wrong number of elements for an empty shape is reported rather than ignored. The sequence
   * copies itself into a fresh array, so a caller that expanded an array of its own into this
   * call cannot reach the rows the result holds.
   *
   * @param rows  the number of rows
   * @param columns  the number of columns
   * @param values  the elements, row by row
   * @return a matrix of the specified shape holding the specified elements
   * @throws IllegalArgumentException if the number of elements is not `rows * columns`
   */
  def of(rows: Int, columns: Int, values: Double*): DoubleMatrix = {
    ArgCheck.isTrue(values.length == rows * columns, "Values array not of length rows * columns")
    if (rows == 0 || columns == 0) {
      EMPTY
    } else {
      val source = values.toArray
      tabulate(rows, columns)((row, column) => source(row * columns + column))
    }
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
   * @throws NegativeArraySizeException if either dimension is negative
   */
  def tabulate(rows: Int, columns: Int)(valueFunction: (Int, Int) => Double): DoubleMatrix =
    if (rows == 0 || columns == 0) {
      EMPTY
    } else {
      new DoubleMatrix(build(rows, columns, valueFunction), rows, columns)
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
   * @param rows  the number of rows, zero or greater
   * @param columns  the number of columns, zero or greater
   * @param valuesFunction  the function from row index to the elements of that row
   * @return a matrix of the specified shape populated by the function
   * @throws IllegalArgumentException if the function returns a row of the wrong length
   * @throws NegativeArraySizeException if the row count is negative
   */
  def ofArrays(rows: Int, columns: Int)(valuesFunction: Int => Array[Double]): DoubleMatrix =
    if (rows == 0 || columns == 0) {
      EMPTY
    } else {
      val result = new Array[Array[Double]](rows)
      fillFromArrays(result, columns, valuesFunction, 0)
      new DoubleMatrix(result, rows, columns)
    }

  // takes each row from the index upwards from the function, checking its length and copying it
  @tailrec
  private def fillFromArrays(
      result: Array[Array[Double]],
      columns: Int,
      valuesFunction: Int => Array[Double],
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
   * of that row as an array of doubles, which must hold exactly `columns` elements. The elements
   * are taken over without copying, which is safe because both types are immutable: neither the
   * array handed over nor the matrix built from it can be modified afterwards.
   *
   * @param rows  the number of rows, zero or greater
   * @param columns  the number of columns, zero or greater
   * @param valuesFunction  the function from row index to the elements of that row
   * @return a matrix of the specified shape populated by the function
   * @throws IllegalArgumentException if the function returns a row of the wrong length
   * @throws NegativeArraySizeException if the row count is negative
   */
  def ofArrayObjects(rows: Int, columns: Int)(valuesFunction: Int => DoubleArray): DoubleMatrix =
    if (rows == 0 || columns == 0) {
      EMPTY
    } else {
      val result = new Array[Array[Double]](rows)
      fillFromArrayObjects(result, columns, valuesFunction, 0)
      new DoubleMatrix(result, rows, columns)
    }

  // takes each row from the index upwards from the function, checking its length
  @tailrec
  private def fillFromArrayObjects(
      result: Array[Array[Double]],
      columns: Int,
      valuesFunction: Int => DoubleArray,
      row: Int): Unit =

    if (row < result.length) {
      val values = valuesFunction(row)
      ArgCheck.isTrue(values.size == columns, incorrectLength(values.size, columns))
      result(row) = values.toArrayUnsafe
      fillFromArrayObjects(result, columns, valuesFunction, row + 1)
    }

  // The message reported when a function returns a row of the wrong length. The wording is that
  // of the Java original, whose message template is replaced by interpolation the compiler checks.
  private def incorrectLength(actual: Int, expected: Int): String =
    s"Function returned array of incorrect length $actual, expected $expected"

  /**
   * Obtains an instance by adopting an array of rows without copying it.
   *
   * This is visible only inside this module, because it makes the caller responsible for the
   * immutability of the result: the array passed in, and every row within it, must be freshly
   * allocated, or otherwise published nowhere else, and must never be modified afterwards.
   * Inside the module that condition is met and checkable by reading the call site.
   *
   * The rows must all be the same length, which is deliberately not validated: the shape is
   * taken from the first row, as the Java original takes it, and a caller that hands over a
   * ragged array gets a matrix whose behaviour is undefined.
   *
   * @param array  the rows to adopt, which the caller must never modify
   * @return a matrix wrapping the specified rows
   */
  private[collect] def ofUnsafe(array: Array[Array[Double]]): DoubleMatrix = {
    val rows = array.length
    if (rows == 0 || array(0).length == 0) {
      EMPTY
    } else {
      new DoubleMatrix(array, rows, array(0).length)
    }
  }

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance by copying an array of rows.
   *
   * Both the array of rows and each row within it are copied and never modified, so the caller
   * may go on using them. The shape is taken from the first row.
   *
   * @param array  the rows to copy
   * @return a matrix holding the elements of the specified rows
   */
  def copyOf(array: Array[Array[Double]]): DoubleMatrix = {
    val rows = array.length
    if (rows == 0 || array(0).length == 0) {
      EMPTY
    } else {
      new DoubleMatrix(deepClone(array), rows, array(0).length)
    }
  }

  // copies an array of rows, cloning each row, so that the result shares nothing with the input
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
   * @throws NegativeArraySizeException if either dimension is negative
   */
  def filled(rows: Int, columns: Int): DoubleMatrix =
    if (rows == 0 || columns == 0) {
      EMPTY
    } else {
      new DoubleMatrix(rectangle(rows, columns), rows, columns)
    }

  /**
   * Obtains an instance with every element equal to the same value.
   *
   * @param rows  the number of rows, zero or greater
   * @param columns  the number of columns, zero or greater
   * @param value  the value of every element
   * @return a matrix of the specified shape filled with the specified value
   * @throws NegativeArraySizeException if either dimension is negative
   */
  def filled(rows: Int, columns: Int, value: Double): DoubleMatrix =
    if (rows == 0 || columns == 0) {
      EMPTY
    } else {
      val result = rectangle(rows, columns)
      fillWith(result, value, 0)
      new DoubleMatrix(result, rows, columns)
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
   * @throws NegativeArraySizeException if the size is negative
   */
  def identity(size: Int): DoubleMatrix =
    if (size == 0) {
      EMPTY
    } else {
      diagonalMatrix(size, _ => 1d)
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
