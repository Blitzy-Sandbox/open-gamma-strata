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
 * produces the storage a matrix keeps - a deep copy of the rows it is handed, the array of rows
 * and every row within it, or a rectangle it allocates itself and fills - rather than storing
 * what it was handed, so a matrix's storage is allocated by that constructor, or by the
 * constructor of the matrix it was derived from, and is reachable from nowhere the caller can
 * name; and no member hands that storage out. `toArray` answers with a deep copy, and the four
 * members that expose a single row or column - `row`, `rowArray`, `column` and `columnArray` -
 * each answer with independent data, a row cloned once and a column read element by element into
 * one buffer per call.
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
 * and the accessors copy. What that costs is one copy of a rectangle per matrix built, which is
 * small next to the arithmetic these matrices exist for; what it buys is that no array anywhere
 * is reachable both by a caller and by a matrix.
 *
 * One rectangle per matrix produced is also all an operation of this type allocates. The
 * constructor is told which operation it is constructing for, produces the storage that operation
 * describes and writes it in row-major order before the matrix is published, so a produced matrix
 * costs one array of rows and one row of elements per row of the shape. '''No stored row is ever
 * modified''', which is what makes deriving one matrix from another safe at all: every loop of
 * this class writes only to storage the constructor allocated moments earlier, and reads the
 * matrix it was derived from.
 *
 * One operation goes further and shares what it does not change. `with`, which replaces a single
 * element, copies the array of row references and clones only the row that changes, as the Java
 * original did, so it costs a row count plus a column count rather than a whole rectangle and the
 * two matrices hold the same arrays for every other row. That is unobservable, and the two facts
 * above are why: no member hands a stored row out, so no caller can reach one of the shared rows,
 * and no operation writes to a row it did not allocate, so neither matrix can change one. A
 * matrix derived by any other operation shares nothing with its source.
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
 * string it produced is returned. Bulk moves - copying, filling, hashing and cloning - go
 * straight to the primitive array operations of the platform.
 *
 * A construction that produces a matrix allocates its rectangle exactly once, and that one
 * allocation is the storage the matrix keeps. The constructor is told which operation it is
 * constructing for, alongside the rows and the shape, and produces the storage that operation
 * describes: the deep copy of the rows it was handed where the operation keeps them as they are;
 * that same deep copy rewritten where it lies where the operation scales every element by one
 * value; the array of row references with one row cloned where it replaces a single element; and
 * otherwise a rectangle of the stated shape, written once in row-major order from the rows it was
 * handed, from another matrix, or from the size, value or function the operation carries. Writing
 * a fresh rectangle in one pass is what the Java original does for the operations that read a
 * second matrix or a function, and it leaves exactly what a deep copy followed by a rewrite of
 * that copy left, at one read and one write per element instead of two reads and a write. For
 * scaling, which reads nothing besides, the two shapes were measured and the deep copy rewritten
 * in place is the cheaper, so that is the shape it takes. The choice of loop is made once per matrix
 * constructed, where the operation names its rewrite, and not once per element; each loop is a
 * single row-major recursion of this class, so the traversal order the parity duty above depends
 * on is written out once per operation, in one place, in the one shape `forEach` also has.
 *
 * Two properties of that arrangement matter beyond the allocation it saves. The rewrite happens
 * inside the constructor, so the stored rows are written only while they are being initialised
 * and are final by the time any other thread can reach the matrix - which is what the ''Thread
 * safety'' note below rests on; and the constructor still never stores the rows it is handed,
 * which is what keeps every factory and every accessor copy-safe for any caller the bytecode
 * admits, in the way ''The stored rows never escape'' above sets out. There is no member, and no
 * constructor argument, through which rows a caller retains can become a matrix's storage: a
 * member that adopted rows instead of producing them would be exactly that, and would be
 * callable by anything compiled against this class, because a companion-private helper reached
 * from here is emitted as a public method under a mangled name - the deep copy this class reaches
 * is emitted exactly that way, which is why the build's check on the members of the two numeric
 * types has to name it. That is the aliasing entry point `ofUnsafe` and `toArrayUnsafe` were, and
 * this type deliberately has none of it.
 *
 * Construction that does not start from the rows of an existing matrix states its result instead,
 * and pays for that result alone: `tabulate`, `filled`, `identity` and `diagonal` name a shape,
 * with a value or a function to fill it from, `ofArrays` and `ofArrayObjects` name the function
 * each row comes from, and `transpose` names this matrix, its result having the opposite shape -
 * which is not the shape of anything the constructor could have rewritten in place, and is filled
 * a row at a time from the columns of the source. The constructor allocates the rectangle for
 * each of them and fills it, so no buffer exists for it to copy. Two constructions still hand
 * over a rectangle of their own and are deep-copied here: `copyOf`, where that copy is what makes
 * the caller's rows unreachable from the matrix, and `of`, which unboxes a sequence of elements
 * into rows of its own - the one place in this type where boxing is the operation rather than a
 * cost inside a loop.
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
 * @param rows  the rows the operation reads, which are copied rather than retained, and which the
 *   operations that describe their own storage do not read at all
 * @param rowCount  the number of rows, zero or greater
 * @param columnCount  the number of columns, zero or greater
 * @param rewrite  the operation whose storage this matrix is to hold, produced and written before
 *   the matrix is published
 */
final class DoubleMatrix private (
    rows: Array[Array[Double]],
    val rowCount: Int,
    val columnCount: Int,
    rewrite: DoubleMatrix.Rewrite) extends Matrix {

  // The rows of this matrix: storage the constructor produces out of what it is handed and out of
  // the operation it is constructing for, written here, and held by nothing else.
  //
  // This is the expression that makes the type immutable in the compiled code, which is why every
  // branch of it produces a rectangle rather than keeping the argument: every factory of the
  // companion, and every operation that produces a new matrix, reaches this constructor, so the
  // rows stored here were allocated here and are held by nothing else. The field is private to
  // the class and is read only from within it - including from another instance of it, which the
  // platform allows and which `equals`, the element-wise operations and the single-element
  // replacement below do - so the compiler emits no accessor for it beyond the private one, and
  // there is no member of this type through which it can be reached.
  //
  // The rewrite says which operation this matrix is being constructed for, and the match selects
  // that operation's loop once, here, rather than once per element. Which shape a case takes
  // follows from what its result is a function of:
  //
  //   - the route every factory handed rows takes keeps them as they are, so its storage is the
  //     companion's deep copy of them - one array of rows and one clone per row, which no loop of
  //     this class can better;
  //   - the single-element replacement derives its storage from the matrix it is replacing in,
  //     copying the array of row references and cloning only the row that changes, which is the
  //     shape of the Java original and is recorded in full where that operation is declared;
  //   - the element-wise operations that read a second matrix, or a function, allocate the
  //     rectangle of the result and write every element of it once, reading the rows this
  //     constructor was handed and, for a binary operation, the other matrix's rows as well.
  //     Writing a fresh rectangle in one pass is what a deep copy followed by a rewrite of the
  //     copy used to do in two, and it is measurably the cheaper of the two shapes for these
  //     operations;
  //   - scaling every element by one value takes the deep copy as its storage and rewrites it
  //     where it lies. Both shapes were measured for this operation and the copy is the cheaper:
  //     it fills each row at the speed of the platform's copy and without the zero fill a fresh
  //     row pays, and a rewrite that scales in place is too cheap per element to repay that. The
  //     shape of each operation is therefore the one measured faster for the work it does;
  //   - the operations whose result is a function of a shape, a value, a function or another
  //     matrix have no rows behind them at all: each allocates its own rectangle through the
  //     companion's builders and fills it, so a shape-driven factory pays for the matrix it
  //     produces and for nothing besides.
  //
  // Each loop is a private method of this class, so it is emitted private and may read another
  // instance's rows directly, and none of them reads this instance's own field: the field is what
  // they are producing. The rewrite is consumed here and nowhere else, which is what keeps the
  // compiler from retaining it in a field of every instance - the hazard recorded below for the
  // rows the shape check is measured against.
  private val array: Array[Array[Double]] = rewrite match {
    case _: DoubleMatrix.NoRewrite =>
      DoubleMatrix.deepClone(rows)
    case single: DoubleMatrix.SetAt =>
      // the row references of the source are copied and only the row that changes is cloned, so
      // this costs a row count plus a column count rather than the whole rectangle; the source is
      // taken from the descriptor, whose rows are provably this library's own
      val storage = single.source.array.clone()
      val replaced = storage(single.row).clone()
      replaced(single.column) = single.newValue
      storage(single.row) = replaced
      storage
    case scaled: DoubleMatrix.ScaledBy =>
      // the deep copy is the storage and the loop rewrites it where it lies; both shapes were
      // measured for this operation and this is the faster, for the reason recorded at
      // `scaledInto`
      val storage = DoubleMatrix.deepClone(rows)
      scaledInto(storage, scaled.factor, 0, 0)
      storage
    case mapped: DoubleMatrix.Mapped =>
      val storage = rectangleOfThisShape
      mapInto(storage, rows, mapped.operator, 0, 0)
      storage
    case indexed: DoubleMatrix.MappedWithIndex =>
      val storage = rectangleOfThisShape
      mapWithIndexInto(storage, rows, indexed.function, 0, 0)
      storage
    case summed: DoubleMatrix.PlusEach =>
      val storage = rectangleOfThisShape
      plusEachInto(storage, rows, summed.other.array, 0, 0)
      storage
    case differenced: DoubleMatrix.MinusEach =>
      val storage = rectangleOfThisShape
      minusEachInto(storage, rows, differenced.other.array, 0, 0)
      storage
    case combined: DoubleMatrix.CombinedWith =>
      val storage = rectangleOfThisShape
      combineEachInto(storage, rows, combined.other.array, combined.operator, 0, 0)
      storage
    case _: DoubleMatrix.Blank =>
      rectangleOfThisShape
    case filled: DoubleMatrix.FilledWith =>
      val storage = rectangleOfThisShape
      DoubleMatrix.fillWith(storage, filled.value, 0)
      storage
    case tabulated: DoubleMatrix.Tabulated =>
      val storage = new Array[Array[Double]](rowCount)
      DoubleMatrix.build(storage, columnCount, tabulated.valueFunction)
      storage
    case diagonal: DoubleMatrix.DiagonalOf =>
      val storage = rectangleOfThisShape
      DoubleMatrix.fillDiagonal(storage, diagonal.valueFunction, 0)
      storage
    case fromArrays: DoubleMatrix.RowsFrom =>
      val storage = new Array[Array[Double]](rowCount)
      DoubleMatrix.fillFromArrays(storage, columnCount, fromArrays.valuesFunction, 0)
      storage
    case fromValues: DoubleMatrix.RowObjectsFrom =>
      val storage = new Array[Array[Double]](rowCount)
      DoubleMatrix.fillFromArrayObjects(storage, columnCount, fromValues.valuesFunction, 0)
      storage
    case transposed: DoubleMatrix.Transposed =>
      val storage = new Array[Array[Double]](rowCount)
      transposeInto(storage, transposed.source, 0)
      storage
  }

  // The shape and the storage are measured against each other as this value is constructed,
  // before it is published.
  //
  // Here is where it belongs because here is where every construction path of this type meets:
  // the factories that are given a column count check each row as they build, and the factory
  // that reads its shape off an array checks nothing itself, so a check made at any one of them
  // would be a check some other path could avoid. Made at the constructor, it is a property of
  // the type - every value of it is rectangular, and a position the shape names is a position
  // the rows hold - which is what lets `get`, `transpose`, `equals`, the element-wise operations
  // and the rendering all be defined in terms of the shape alone.
  //
  // It measures the storage the field above produced rather than the rows handed over, and the
  // difference matters for exactly one reason: the routes that derive their storage from the
  // operation are handed no rows at all, so the argument says nothing about the value being
  // built. On the route that keeps the rows it is handed, the two are the same measurement - a
  // deep copy has the shape of its source, row for row - so the factory that reads its shape off
  // a caller's array is refused here as precisely as it was when the argument was measured, with
  // the same message naming the same row. What it costs is that such a refusal is now reported
  // after the copy rather than before it, which is one rectangle allocated for a broken caller
  // and then dropped; what it buys is one check covering every route rather than a check per
  // route.
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
  // written here would close over this constructor's own parameters - which the compiler answers
  // by keeping those parameters in fields of every instance. The rows of the caller would then be
  // held for as long as the matrix, defeating half the point of copying them. Passed to a helper
  // instead, they are arguments of that helper and nothing of the caller's outlives the
  // construction.
  DoubleMatrix.checkShape(array, rowCount, columnCount)

  // Constructs the matrix this one's rows, rewritten by the given operation, describe.
  //
  // Every operation of this class that produces a matrix of the same shape as this one answers
  // through here, which is where the one short circuit they share lives: a shape with no rows or
  // no columns has no element to write, so it answers with the shared empty instance, as every
  // factory of this type does. The rows this matrix holds are passed to the constructor, which
  // reads them into storage of its own rather than keeping them, so the two matrices share
  // nothing - with the one exception the single-element replacement documents, which shares the
  // rows it does not change and takes them from the descriptor rather than from this argument.
  private def rewritten(rewrite: DoubleMatrix.Rewrite): DoubleMatrix =
    if (rowCount == 0 || columnCount == 0) {
      DoubleMatrix.EMPTY
    } else {
      new DoubleMatrix(array, rowCount, columnCount, rewrite)
    }

  // Allocates the rectangle of this matrix's own shape, every element left at zero. This is the
  // storage the element-wise operations that read a second matrix or a function write, and the
  // storage each shape-driven construction fills; the rows are allocated by the companion's own
  // loop, so the one loop that allocates a run of rows serves every route through the constructor
  // that allocates at all. It reads the shape of the value being built and no field of any
  // matrix, which is what makes it safe to call while the storage field is still being
  // initialised.
  private def rectangleOfThisShape: Array[Array[Double]] = {
    val storage = new Array[Array[Double]](rowCount)
    DoubleMatrix.rectangle(storage, columnCount)
    storage
  }

  // The element-wise rewrites, each a single row-major recursion writing the storage the
  // constructor produced. Each visits row 0 left to right, then row 1, and so on - the order of
  // `forEach` and of the Java original, and the order the numerical parity of this type is
  // measured in - and each reads the row length off the storage it is walking, which the shape
  // check of the constructor measures against the column count. Each element of the target is
  // written exactly once, from the same position of the source rows and, for a binary operation,
  // of the other matrix's rows, which are read and never written. Where the constructor allocated
  // a fresh rectangle the target is distinct from every source, because no matrix holds it; where
  // it took a deep copy and rewrote it, the target is also the source, which is sound for the
  // same reason each loop is written this way - every element is written from its own position
  // and from no other, so no loop can observe a write it has not made itself.

  // multiplies every element of the target by the factor where it lies, in row-major order. The
  // target is the deep copy the constructor took, and the loop names that one rectangle rather
  // than a target and a source: two parameters holding the same rows may alias as far as the
  // compiler can tell, and the loop it emits for them is half again as dear, measured
  @tailrec
  private def scaledInto(
      target: Array[Array[Double]],
      factor: Double,
      row: Int,
      column: Int): Unit =

    if (row < target.length) {
      if (column >= target(row).length) {
        scaledInto(target, factor, row + 1, 0)
      } else {
        target(row)(column) = target(row)(column) * factor
        scaledInto(target, factor, row, column + 1)
      }
    }

  // writes the operator applied to every element of the source into the target, in row-major order
  @tailrec
  private def mapInto(
      target: Array[Array[Double]],
      source: Array[Array[Double]],
      operator: Double => Double,
      row: Int,
      column: Int): Unit =

    if (row < target.length) {
      if (column >= target(row).length) {
        mapInto(target, source, operator, row + 1, 0)
      } else {
        target(row)(column) = operator(source(row)(column))
        mapInto(target, source, operator, row, column + 1)
      }
    }

  // writes the function applied to every positioned element of the source into the target, in
  // row-major order
  @tailrec
  private def mapWithIndexInto(
      target: Array[Array[Double]],
      source: Array[Array[Double]],
      function: DoubleMatrix.ElementFunction,
      row: Int,
      column: Int): Unit =

    if (row < target.length) {
      if (column >= target(row).length) {
        mapWithIndexInto(target, source, function, row + 1, 0)
      } else {
        target(row)(column) = function(row, column, source(row)(column))
        mapWithIndexInto(target, source, function, row, column + 1)
      }
    }

  // writes the sum of the matching elements of the source and the other matrix into the target,
  // in row-major order, both sources being read and never written
  @tailrec
  private def plusEachInto(
      target: Array[Array[Double]],
      source: Array[Array[Double]],
      other: Array[Array[Double]],
      row: Int,
      column: Int): Unit =

    if (row < target.length) {
      if (column >= target(row).length) {
        plusEachInto(target, source, other, row + 1, 0)
      } else {
        target(row)(column) = source(row)(column) + other(row)(column)
        plusEachInto(target, source, other, row, column + 1)
      }
    }

  // writes each element of the source less the matching element of the other matrix into the
  // target, in row-major order, both sources being read and never written
  @tailrec
  private def minusEachInto(
      target: Array[Array[Double]],
      source: Array[Array[Double]],
      other: Array[Array[Double]],
      row: Int,
      column: Int): Unit =

    if (row < target.length) {
      if (column >= target(row).length) {
        minusEachInto(target, source, other, row + 1, 0)
      } else {
        target(row)(column) = source(row)(column) - other(row)(column)
        minusEachInto(target, source, other, row, column + 1)
      }
    }

  // writes the operator applied to the matching elements of the source and the other matrix into
  // the target, in row-major order, both sources being read and never written
  @tailrec
  private def combineEachInto(
      target: Array[Array[Double]],
      source: Array[Array[Double]],
      other: Array[Array[Double]],
      operator: (Double, Double) => Double,
      row: Int,
      column: Int): Unit =

    if (row < target.length) {
      if (column >= target(row).length) {
        combineEachInto(target, source, other, operator, row + 1, 0)
      } else {
        target(row)(column) = operator(source(row)(column), other(row)(column))
        combineEachInto(target, source, other, operator, row, column + 1)
      }
    }

  // Writes each row of the target from the matching column of the source, in row order: row `i`
  // of the transpose of an `m x n` matrix is column `i` of that matrix. The column materialiser
  // of the source allocates each row at the source's own row count, which is the column count of
  // the result, so this is one allocation per row and no element is read or written twice. Going
  // column by column of the source rather than element by element of the result is also what
  // keeps the writes sequential within a row.
  @tailrec
  private def transposeInto(
      target: Array[Array[Double]],
      source: DoubleMatrix,
      row: Int): Unit =

    if (row < target.length) {
      target(row) = source.columnCopy(row)
      transposeInto(target, source, row + 1)
    }

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
   * '''A call costs a copy of the whole row''', which is the price of that independence: this is
   * linear in the column count where the original was constant, so a caller reading elements of
   * one row should bind the row once and index the result rather than call this inside a loop,
   * and a caller reading single elements at scattered positions is better served by `get`, which
   * copies nothing.
   *
   * @param row  the zero-based row index to retrieve
   * @return the row, as an independent array of doubles
   * @throws java.lang.IndexOutOfBoundsException if the row index is outside this matrix
   */
  def row(row: Int): DoubleArray = DoubleArray.copyOf(array(row))

  /**
   * Gets the row at the specified index as an independent primitive array.
   *
   * The array is a copy, so the caller may modify it freely without affecting this matrix, and a
   * call therefore costs a copy of the whole row - linear in the column count, as `row` records
   * in full, and paid again on every call.
   *
   * @param row  the zero-based row index to retrieve
   * @return the row, as a cloned array
   * @throws java.lang.IndexOutOfBoundsException if the row index is outside this matrix
   */
  def rowArray(row: Int): Array[Double] = array(row).clone()

  /**
   * Gets the column at the specified index.
   *
   * The column is built by reading one element from each row in turn into the storage of the
   * result, which the array type's tabulating factory allocates: nothing the caller holds and
   * nothing this matrix holds is reachable from it, and the column is materialised once rather
   * than into a buffer that is then copied. An empty matrix has no rows to read, so every column
   * index - including one that no matrix could hold - answers with the empty array, which is the
   * behaviour of the Java original, and it answers with the shared empty instance because a
   * column of no elements has nothing to hold.
   *
   * '''A call costs an element read and a write per row''', so it is linear in the row count -
   * dearer per element than `row`, because the elements of a column are one row's length apart in
   * memory - and it is paid again on every call. A caller walking a column should bind it once.
   *
   * @param column  the zero-based column index to retrieve
   * @return the column, as an independent array of doubles
   * @throws java.lang.IndexOutOfBoundsException if the column index is outside a non-empty matrix
   */
  def column(column: Int): DoubleArray =
    DoubleArray.tabulate(rowCount)(row => array(row)(column))

  /**
   * Gets the column at the specified index as an independent primitive array.
   *
   * The array is the buffer the column materialiser of this class reads its elements into, handed
   * over directly, so no element is read or moved twice. The caller owns the result and may modify
   * it freely without affecting this matrix, and a call costs an element read and a write per row,
   * exactly as `column` records.
   *
   * @param column  the zero-based column index to retrieve
   * @return the column, as an independent array
   * @throws java.lang.IndexOutOfBoundsException if the column index is outside a non-empty matrix
   */
  def columnArray(column: Int): Array[Double] = columnCopy(column)

  // Materialises one column into a freshly allocated array of the row count. The buffer is never
  // stored on this instance, so `columnArray` can hand it out as it stands; `transposeInto` takes
  // one of these per row of a transpose, which is where each becomes a row of the result. An empty
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
   * one already there answers with this instance rather than with a copy. Otherwise the
   * constructor copies the array of row references and clones only the row that changes, writing
   * the new value into that clone, so the cost is the row count plus the column count rather than
   * the whole rectangle - the shape of the Java original. This matrix is unchanged either way: no
   * stored row is ever modified, and the row that would have been is the one that was cloned. The
   * name is that of the Java original, which is a reserved word here and so is written in
   * backquotes.
   *
   * The rows the two matrices then have in common are shared, which is unobservable and is what
   * makes the linear cost possible: no member of this type hands a stored row out - `row`,
   * `rowArray`, `column`, `columnArray` and `toArray` all copy - and no operation writes to one,
   * each of them allocating the storage it writes. A row is therefore reachable only from the
   * matrices that hold it, and neither of them can change it.
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
      rewritten(new DoubleMatrix.SetAt(this, row, column, newValue))
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
      rewritten(new DoubleMatrix.ScaledBy(factor))
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
  def map(operator: Double => Double): DoubleMatrix = rewritten(new DoubleMatrix.Mapped(operator))

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
    rewritten(new DoubleMatrix.MappedWithIndex(function))

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
    rewritten(new DoubleMatrix.PlusEach(other))
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
    rewritten(new DoubleMatrix.MinusEach(other))
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
    rewritten(new DoubleMatrix.CombinedWith(other, operator))
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
    if (rowCount == 0 || columnCount == 0) {
      DoubleMatrix.EMPTY
    } else {
      // This matrix is named to the constructor, which allocates the rectangle of the opposite
      // shape and fills each of its rows from the matching column of this one, so transposing
      // costs the result and nothing besides. The rows handed over are this matrix's own, as they
      // are for every operation that derives one matrix from another; this route reads them
      // through the matrix the descriptor carries, because a column is materialised by the member
      // of this class that knows the shape, so the argument is a formality here and the storage
      // is produced from the descriptor either way.
      new DoubleMatrix(array, columnCount, rowCount, new DoubleMatrix.Transposed(this))
    }

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

  //-------------------------------------------------------------------------
  // The construction routes of this type: which storage the sole constructor is to produce, and
  // which operation it is to apply to that storage before the matrix is published.
  //
  // These are internal to the package - the two specs of this package name them, and nothing else
  // does - and they exist so that a construction producing a matrix costs one rectangle instead
  // of two. Building a rectangle and handing it over cost that rectangle plus the deep copy the
  // constructor makes of it; naming the operation instead lets the constructor produce the
  // storage itself and write it once, so that single rectangle IS the result.
  //
  // Three properties of the family are deliberate, and each is a property of the compiled form
  // rather than of a convention:
  //
  //   - no descriptor carries rows. Each carries a scalar, a position, a callback, or another
  //     matrix; a descriptor holding an `Array[Array[Double]]` would publish an accessor handing
  //     those rows out, which for a binary operation is another matrix's storage, and the
  //     aliasing this type does not have would be back. Carrying another matrix is not that: a
  //     matrix's rows were allocated by this constructor and no member hands one out, so a
  //     descriptor naming a matrix names something no caller can have injected and nothing a
  //     caller can reach;
  //   - a descriptor selects a loop and nothing else. The constructor matches once per matrix
  //     constructed, and the loop it selects is that operation's own row-major loop, so the
  //     dispatch is one type test per operation rather than a virtual call per element;
  //   - no descriptor can make the constructor keep the rows it is handed. Every branch of the
  //     field initialiser produces storage - the companion's deep copy of the argument, a
  //     rectangle allocated at the shape of the result and written from the argument, or a
  //     rectangle built from the descriptor alone - so any caller the bytecode admits, in any
  //     language, that reaches the constructor with rows it retains gets a matrix that copied
  //     them. There is no adopting route to reach, and so none to defend. That is why the routes
  //     that derive their storage from the descriptor are handed the shared run of no rows below
  //     rather than a rectangle of the shape they are about to produce: nothing is handed over
  //     that could be kept, so the property holds branch by branch and not only case by case.
  //
  // They are declared ahead of `EMPTY` because that value constructs through this family, and a
  // `val` of an object is initialised in the order the object declares it.
  private[array] sealed abstract class Rewrite

  // Keep the rows as they are: the storage is the deep copy of what the constructor was handed.
  // This is the route of every factory, and of the shared empty instance.
  private[array] final class NoRewrite extends Rewrite

  // Replace the element at one position, which is what `with` produces. The matrix being replaced
  // in is carried, as the binary operations carry theirs, because this is the one operation whose
  // storage is derived from it rather than from the rows the constructor is handed: the array of
  // row references is copied and only the row that changes is cloned, so the result shares every
  // other row with the source. That sharing is safe for two reasons that hold together, and both
  // are properties of this class rather than conventions. A matrix's rows are allocated by this
  // constructor and are handed out by no member of the type - `row`, `rowArray`, `column`,
  // `columnArray` and `toArray` all copy - so no caller holds one and no caller can inject one;
  // and no operation of this type writes into a row it did not allocate, each of the branches
  // above allocating its storage before writing, so a shared row cannot change once published.
  private[array] final class SetAt(
      val source: DoubleMatrix,
      val row: Int,
      val column: Int,
      val newValue: Double) extends Rewrite

  // Multiply every element by the factor, which is what `multipliedBy` produces.
  private[array] final class ScaledBy(val factor: Double) extends Rewrite

  // Apply the operator to every element, which is what `map` produces.
  private[array] final class Mapped(val operator: Double => Double) extends Rewrite

  // Apply the callback to every positioned element, which is what `mapWithIndex` produces.
  private[array] final class MappedWithIndex(val function: ElementFunction) extends Rewrite

  // Add the matching element of the other matrix to every element, which is what `plus` produces.
  // The other matrix is carried whole rather than as its rows, which is what keeps this
  // descriptor from having rows to hand out.
  private[array] final class PlusEach(val other: DoubleMatrix) extends Rewrite

  // Subtract the matching element of the other matrix from every element, which is what `minus`
  // produces.
  private[array] final class MinusEach(val other: DoubleMatrix) extends Rewrite

  // Combine every element with the matching element of the other matrix, which is what `combine`
  // produces.
  private[array] final class CombinedWith(
      val other: DoubleMatrix,
      val operator: (Double, Double) => Double) extends Rewrite

  //-------------------------------------------------------------------------
  // The routes whose storage is a function of the descriptor and the shape rather than of rows
  // the constructor is handed. Each states how to fill the rectangle of the stated shape, so the
  // constructor allocates exactly that and fills it once: the rows these are handed are the
  // shared empty run below, which holds nothing to copy and nothing to keep.

  // Produce the rectangle of the shape with every element equal to zero, which is what `filled`
  // of a shape produces: freshly allocated rows hold zeroes already, so there is nothing further
  // to write. It carries nothing, so the single instance below serves every such construction.
  private[array] final class Blank extends Rewrite

  // Produce the rectangle of the shape with every element equal to the value, which is what
  // `filled` of a shape and a value produces.
  private[array] final class FilledWith(val value: Double) extends Rewrite

  // Produce the rectangle of the shape with each element taken from the function applied to its
  // position, in row-major order, which is what `tabulate` produces.
  private[array] final class Tabulated(val valueFunction: (Int, Int) => Double) extends Rewrite

  // Produce the square rectangle of the shape whose primary diagonal is taken from the function
  // and whose other elements are zero, which is what `identity` and `diagonal` produce.
  private[array] final class DiagonalOf(val valueFunction: Int => Double) extends Rewrite

  // Produce the rectangle of the shape with each row taken from the function, measured against
  // the column count and copied, which is what `ofArrays` produces.
  private[array] final class RowsFrom(val valuesFunction: RowArrayFunction) extends Rewrite

  // Produce the rectangle of the shape with each row taken from the function as a value of the
  // array type, measured against the column count and read out through that type's copying
  // accessor, which is what `ofArrayObjects` produces.
  private[array] final class RowObjectsFrom(val valuesFunction: RowArrayObjectFunction)
      extends Rewrite

  // Produce the transpose of the matrix, which is what `transpose` produces: the one route whose
  // result has a shape the source does not, so there is no rectangle of the source's shape for it
  // to have rewritten. Each row of the result is a column of the source, materialised by the
  // source's own column reader, and the source is carried for the reason `SetAt` carries its
  // matrix - a matrix's rows are this library's own and are reachable from no caller.
  private[array] final class Transposed(val source: DoubleMatrix) extends Rewrite

  // The two routes that carry nothing are single instances, so naming either of them costs no
  // allocation: every factory handed rows names the first, and `filled` of a shape the second.
  private[array] val NoRewrite: NoRewrite = new NoRewrite
  private[array] val Blank: Blank = new Blank

  // The rows handed to the constructor on the routes that derive their storage from the
  // descriptor and the shape. It holds nothing, so it is shared by every such construction and
  // sharing it is safe for the reason the empty matrix itself is safe: a run of no rows has
  // nothing to read and no position to write. It is private to the companion rather than to the
  // package because a member restricted to the package is emitted public, and a public member of
  // either numeric type that answered with rows - whatever it held - is what the build's check on
  // these two types forbids.
  private val noRows: Array[Array[Double]] = new Array[Array[Double]](0)

  //-------------------------------------------------------------------------
  /**
   * An empty matrix.
   *
   * It has no rows, no columns and no elements. Every factory that has no elements to produce
   * answers with this instance, so the common empty result costs no allocation.
   */
  val EMPTY: DoubleMatrix = new DoubleMatrix(new Array[Array[Double]](0), 0, 0, NoRewrite)

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
      new DoubleMatrix(result, rows, columns, NoRewrite)
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
   * This factory is one construction among several that fill a rectangle the constructor
   * allocates: the other shape-driven factories, `transpose`, and the element-wise operations all
   * reach the constructor with their own description rather than through here, which is one
   * allocation each rather than two. Every one of them walks its elements in the same row-major
   * order this factory fills them in - the order the numerical parity of the port depends on.
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
      // the shape and the function are named to the constructor, which allocates the rectangle
      // once and fills it through the loop below, so tabulating costs the result and nothing else
      new DoubleMatrix(noRows, rows, columns, new Tabulated(valueFunction))
    }
  }

  // Builds every row of the target, each element taken from the function applied to its position.
  // This is the entry point of the tabulating route, which the constructor reaches, and it is
  // restricted to the package rather than to this object for that reason: a member of a companion
  // that the class reaches is emitted under a compiler-chosen name, and the build's check on the
  // numeric types names this builder. It answers with nothing and fills what it is given, because
  // a member of either numeric type that answered with rows would breach that same check.
  private[array] def build(
      target: Array[Array[Double]],
      columns: Int,
      valueFunction: (Int, Int) => Double): Unit =

    buildRows(target, columns, valueFunction, 0)

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
      // the shape and the function are named to the constructor, which allocates the array of
      // rows and takes each row from the function, so the rectangle is built exactly once
      new DoubleMatrix(noRows, rows, columns, new RowsFrom(valuesFunction))
    }
  }

  // takes each row of the target from the index upwards from the function, checking its length
  // and copying it; restricted to the package because the constructor reaches it, for the reason
  // recorded on `build`
  @tailrec
  private[array] def fillFromArrays(
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
      // as `ofArrays`, with each row arriving as a value of the array type
      new DoubleMatrix(noRows, rows, columns, new RowObjectsFrom(valuesFunction))
    }
  }

  // takes each row of the target from the index upwards from the function, checking its length and
  // reading its elements out through the copying accessor of the array type, which is the only
  // member of that type that answers with a run of values; restricted to the package because the
  // constructor reaches it, for the reason recorded on `build`
  @tailrec
  private[array] def fillFromArrayObjects(
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

  // Measures the storage the constructor produced against the shape it was given, and reports the
  // first disagreement. This is the whole of the constructor's validation, gathered here so that
  // the messages - which are built only when a check fails - are closed over the parameters of
  // this method rather than over those of the constructor, for the reason recorded there.
  private def checkShape(rows: Array[Array[Double]], rowCount: Int, columnCount: Int): Unit = {
    ArgCheck.isTrue(rows.length == rowCount, rowCountMismatch(rows.length, rowCount))
    checkRectangular(rows, columnCount, 0)
  }

  // Checks every row against the column count the shape states, from the index upwards, and
  // reports the first row that disagrees. This is the check the constructor makes, so it runs
  // once per value built, before the value is published: a shape and a set of rows that cannot
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
      val copied = new DoubleMatrix(array, rows, array(0).length, NoRewrite)
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
      // the shape alone is named to the constructor, which allocates the rectangle it keeps:
      // freshly allocated rows hold zeroes, so this construction is that one rectangle
      new DoubleMatrix(noRows, rows, columns, Blank)
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
      // the shape and the value are named to the constructor, which allocates the rectangle and
      // fills each row with the platform's own fill
      new DoubleMatrix(noRows, rows, columns, new FilledWith(value))
    }
  }

  // Allocates every row of the target, each of the stated column count, every element left at
  // zero. This is the entry point of the routes that build a rectangle of a stated shape, which
  // the constructor reaches, so it is restricted to the package and answers with nothing, for the
  // reason recorded on `build`.
  private[array] def rectangle(target: Array[Array[Double]], columns: Int): Unit =
    allocateRows(target, columns, 0)

  // allocates each row from the index upwards, in row order
  @tailrec
  private def allocateRows(result: Array[Array[Double]], columns: Int, row: Int): Unit =
    if (row < result.length) {
      result(row) = new Array[Double](columns)
      allocateRows(result, columns, row + 1)
    }

  // fills each row from the index upwards with the value, in row order; restricted to the package
  // because the constructor reaches it, for the reason recorded on `build`
  @tailrec
  private[array] def fillWith(result: Array[Array[Double]], value: Double, row: Int): Unit =
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

  // builds a square matrix whose primary diagonal is taken from the function: the size and the
  // function are named to the constructor, which allocates the rectangle and sets its diagonal
  private def diagonalMatrix(size: Int, valueFunction: Int => Double): DoubleMatrix =
    new DoubleMatrix(noRows, size, size, new DiagonalOf(valueFunction))

  // sets each diagonal element of the target from the index upwards from the function, in row
  // order; restricted to the package because the constructor reaches it, for the reason recorded
  // on `build`
  @tailrec
  private[array] def fillDiagonal(
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
