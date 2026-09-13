/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */

// This file declares two top-level packages, which is why it is written with package blocks
// rather than with a leading package clause. The suite belongs in the package of its subject,
// `com.opengamma.strata.collect.array`; the access probe at the foot of the file must sit
// *outside* `com.opengamma.strata.collect`, because what it proves is that the two names the
// Java original used for its escape hatches resolve to nothing from there, and a compile-time
// name check is answered in the package of the code that asks. A block nested inside a package clause would
// nest under that clause and stay inside `collect`, which is exactly the arrangement that could
// not prove anything, so the two blocks are siblings at the root. The spec of the array type,
// written immediately before this one, is laid out the same way.
package com.opengamma.strata.collect.array {

  import java.lang.reflect.Modifier
  import java.util.Arrays

  import scala.annotation.tailrec

  import cats.Eq
  import cats.Hash
  import cats.Show

  import org.scalacheck.Gen
  import org.scalacheck.Shrink
  import org.scalatest.Assertion
  import org.scalatest.funsuite.AnyFunSuite
  import org.scalatest.matchers.should.Matchers
  import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

  import com.opengamma.strata.collect.Arbitraries
  import com.opengamma.strata.collect.json.Codecs

  /**
   * Test [[DoubleMatrix]].
   *
   * The Java original held thirty-one test methods and every one of them is carried here under
   * its own name, so that the migration is traceable method by method rather than in bulk: the
   * test-scope manifest of this port maps each Java method to the test below that answers for it,
   * and nothing in this class is recorded as partial or dropped. Every hard-coded expected value
   * of the original is carried across unchanged, including the two expected values that the
   * original wrote as products rather than as decimals and the rendering it asserted character for
   * character. The original compared elements exactly, with no tolerance, and so does this spec.
   *
   * ===Java method to Scala test===
   *
   *  - `test_EMPTY` -> `test_EMPTY`, ported
   *  - `test_of` -> `test_of`, ported
   *  - `test_of_values` -> `test_of_values`, ported
   *  - `test_of_intintlambda` -> `test_of_intintlambda`, ported onto `DoubleMatrix.tabulate`,
   *    which is the named factory that replaces the shape-and-function overload of the original
   *  - `test_ofArrayObjects` -> `test_ofArrayObjects`, ported
   *  - `test_ofArrays` -> `test_ofArrays`, ported
   *  - `test_ofUnsafe` -> `test_ofUnsafe`, answered by what replaces the member rather than by
   *    the member: the adopting factory of the original is not ported, and the test holds the
   *    construction that replaces it to the copy it makes
   *  - `test_copyOf_array` -> `test_copyOf_array`, ported
   *  - `test_filled` -> `test_filled`, ported
   *  - `test_filled_withValue` -> `test_filled_withValue`, ported
   *  - `test_identity` -> `test_identity`, ported
   *  - `test_diagonal` -> `test_diagonal`, ported
   *  - `test_get` -> `test_get`, ported
   *  - `test_row` -> `test_row`, ported, with the copying of the result asserted as well
   *  - `test_rowArray` -> `test_rowArray`, ported
   *  - `test_column` -> `test_column`, ported
   *  - `test_columnArray` -> `test_columnArray`, ported
   *  - `test_forEach` -> `test_forEach`, ported
   *  - `test_with` -> `test_with`, ported; the member keeps the name of the original, which is a
   *    reserved word here, so every call is written in backquotes
   *  - `test_multipliedBy` -> `test_multipliedBy`, ported, with the absence of a matrix product
   *    asserted rather than merely omitted
   *  - `test_map` -> `test_map`, ported
   *  - `test_mapWithIndex` -> `test_mapWithIndex`, ported
   *  - `test_plus` -> `test_plus`, ported
   *  - `test_minus` -> `test_minus`, ported
   *  - `test_combine` -> `test_combine`, ported
   *  - `test_total` -> `test_total`, ported
   *  - `test_reduce` -> `test_reduce`, ported
   *  - `testTransposeMatrix` -> `testTransposeMatrix`, ported; the name of the original carries no
   *    prefix and is reproduced exactly as it stands
   *  - `test_equalsHashCode` -> `test_equalsHashCode`, ported
   *  - `test_toString` -> `test_toString`, ported
   *  - `coverage` -> `coverage`, ported as compile-time and instance assertions in place of the
   *    reflective sweep over the properties of a bean that the original performed, which has no
   *    target here and whose machinery this port does without entirely
   *
   * The remaining tests answer requirements of this port rather than of the Java original: the
   * copy-safety tests (`copy_safety_*`), the proof that the type hands out none of the rows it
   * holds - asserted against the compiled class itself, and from outside this module
   * (`no_member_hands_out_the_stored_rows`, `the_public_constructor_copies_what_it_is_handed`,
   * `unsafe_members_resolve_to_nothing`), the bit-level equality cases (`ieee_*`),
   * the oracles for hashing and rendering (`hashCode_*`, `toString_*`), the shape invariants
   * (`shape_*`, and `every_value_of_this_type_is_rectangular_whatever_route_built_it` for the one
   * the constructor enforces), the refusal of an array whose rows disagree
   * (`copyOf_refuses_rows_that_differ_in_length_where_the_original_shaped_them_by_the_first`), the
   * inventory of failure types (`exception_types_*`), the rejection of a negative dimension before
   * any work is done (`negative_dimensions_*`), the two branches only the empty matrix reaches
   * (`the_empty_matrix_*`) and the property section (`property_*`).
   *
   * ===Divergences from the Java original that this spec asserts===
   *
   * Each of these is asserted below rather than described only, and each is a candidate line for
   * the migration note of the port:
   *
   *  - `row` and `column` answer with independent data. The original returned the stored row from
   *    `row`, and handed out the array behind a freshly built column, relying on a documented
   *    convention that no caller would write to either. This is the headline behavioural difference
   *    for this type and it is asserted directly, by showing that the array a row is built on is
   *    not the array the matrix holds, and that writing to the array `rowArray` or `columnArray`
   *    answers with changes nothing;
   *  - an index outside the matrix fails with `ArrayIndexOutOfBoundsException`, which is a subclass
   *    of the `IndexOutOfBoundsException` the original raised and the member documents, because the
   *    index reaches the stored rows directly rather than passing a check of this library's own.
   *    Both the concrete type and the documented supertype are asserted. This matches the array
   *    type of this module, whose own spec records the same finding for its direct-index members;
   *  - a column index is never rejected by the empty matrix: it has no row to read, so every index
   *    answers with the empty array, which is the behaviour of the original and is asserted here
   *    because it is surprising next to the failure a non-empty matrix produces;
   *  - values that do not fill the requested shape, and a function that returns a row of the wrong
   *    length, fail with `IllegalArgumentException` exactly as in the original, and the message of
   *    the original is preserved word for word, which this spec asserts rather than assumes;
   *  - an array whose rows differ in length is refused by `copyOf`, where the original shaped such
   *    an array by its first row and produced a matrix that misstated its own shape. This is the
   *    one argument the port rejects that the original accepted, and it is a shape violation like
   *    any other - an `IllegalArgumentException` through `ArgCheck`, naming the offending row and
   *    both lengths. The refusal is made at the constructor every factory passes through, so
   *    every value of this type is rectangular, which this spec asserts over every route into
   *    one. The order of `copyOf`'s two decisions is asserted with it: the rows are measured
   *    before the empty short circuit, so `[[], [1.0]]` is refused rather than collapsing onto
   *    the empty matrix;
   *  - a negative row count, column count or size fails with `IllegalArgumentException` naming the
   *    argument and its value, where the original let the allocation it had already begun raise a
   *    negative-size error of the platform's. Every factory checks its dimensions before it
   *    allocates anything, before it decides that a shape has no elements, and before it calls any
   *    function it was given, so an invalid shape costs nothing and a negative count is never
   *    mistaken for an empty one. This is the one failure of this type whose exception differs from
   *    the original's, and it is asserted case by case rather than described only;
   *  - the number of elements a shape needs is computed in wider arithmetic than the original's,
   *    so a shape whose product overflows the integer range - `65536` by `65536`, for one - is
   *    reported as the count violation it is instead of passing a count test against a wrapped
   *    product and proceeding to allocate;
   *  - `ofUnsafe` and `toArrayUnsafe` are not ported, where the original exposed both to every
   *    caller. Restricting them to this module would not have been enough: such a restriction
   *    holds in the source only, and the compiler emits the member as a public method either way,
   *    so what this spec asserts instead is that the two names resolve nowhere, that no declared
   *    member of the compiled class returns the rows an instance holds, and that the one public
   *    constructor copies what it is handed. The first of those is asserted here and again from a
   *    probe object in a sibling package, the other two against the class itself;
   *  - the port has no meta-bean, no ordering and no JSON codec of its own for this type, and no
   *    product of two matrices - which the original does not have either. The absence of each is
   *    asserted, next to a positive control proving the same assertion shape succeeds where the
   *    member or instance does exist, so that none of them can pass merely because the expression
   *    was malformed;
   *  - every zero-size result is the canonical empty instance, as in the original, so the helper
   *    below asserts that identity rather than merely a size of zero. The port canonicalises all
   *    three degenerate shapes - no rows, no columns, and a row count with no columns - to it, so
   *    no matrix whose column count is zero exists to be tested.
   *
   * ===Tolerances===
   *
   * Every comparison here is exact, and the comparisons that concern a value the platform's
   * numeric equality disagrees about - a not-a-number element, a signed zero - are made on bit
   * patterns, which is the comparison the type itself makes. The one exception is the arithmetic
   * round trip in the property section, whose bound is stated and justified where it is used. The
   * numerical parity of this type against values captured from the Java implementation, to 1e-9
   * absolute and relative, is measured by the parity spec of this module against its committed
   * fixture; it is deliberately not duplicated here, and this spec reads no fixture and performs
   * no effect.
   */
  final class DoubleMatrixSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

    /**
     * A value of an unrelated type, mirroring the fixture of the same purpose in the original.
     *
     * It is typed as `Any` so that it can be handed to `equals` without the compiler inferring a
     * supertype for a call that was written with a matrix in mind.
     */
    private val anotherType: Any = ""

    /**
     * An absent reference, bound rather than written inline.
     *
     * The Java original asserted that a matrix does not equal `null`. Binding the reference keeps
     * that case while keeping a literal comparison against nothing out of the source.
     */
    private val nullRef: Any = null

    /**
     * The hashing of matrices, summoned once for the tests that use it.
     *
     * [[coverage]] summons the instances itself, because summoning them is part of what that test
     * asserts; everything else uses this one.
     */
    private val matrixHash: Hash[DoubleMatrix] = implicitly[Hash[DoubleMatrix]]

    /**
     * Generates a matrix that has at least one element.
     *
     * The shared generators of this module produce the empty matrix about a third of the time,
     * because both degenerate shapes collapse to it, and a few properties below are about a
     * position inside a matrix, which the empty one has none of. Rather than declare a generator
     * of matrices here - the generator object is a cross-module contract and no spec adds to it -
     * this narrows the shared one, so the values seen are still exactly the values it produces.
     */
    private val genNonEmptyMatrix: Gen[DoubleMatrix] =
      Arbitraries.genDoubleMatrix.filter(matrix => !matrix.isEmpty)

    /**
     * The rows the Java original used for most of its cases, freshly allocated on each call.
     *
     * Several of its tests write to the array after building a matrix from it, so each caller
     * needs its own; a method rather than a field is what gives them that.
     *
     * @return the three by two rows holding one to six
     */
    private def rows3x2: Array[Array[Double]] =
      Array(Array(1.0, 2.0), Array(3.0, 4.0), Array(5.0, 6.0))

    /**
     * The three by two matrix holding one to six, which is the subject of most cases below.
     *
     * @return the matrix holding one to six over three rows
     */
    private def matrix3x2: DoubleMatrix = DoubleMatrix.copyOf(rows3x2)

    /**
     * The bit pattern of a value, which is how this type compares elements.
     *
     * Every assertion about an element that may be a not-a-number value or a signed zero is
     * written through this method, because the numeric comparison of the platform disagrees with
     * the equality of this type on exactly those values: it reports a not-a-number value as
     * unequal to itself and the two zeroes as equal, so asserting with it would pass for the wrong
     * reason wherever it passed at all.
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
     * The positions of a matrix, in row-major order.
     *
     * The order is the one every operation of the type visits elements in, and the one the
     * flattened expectations of the helper below are indexed by. An empty matrix has no positions,
     * so a statement made over them holds of it without a special case.
     *
     * @param matrix  the matrix to enumerate
     * @return every row and column index pair, row by row
     */
    private def positionsOf(matrix: DoubleMatrix): IndexedSeq[(Int, Int)] =
      for {
        row <- 0 until matrix.rowCount
        column <- 0 until matrix.columnCount
      } yield (row, column)

    /**
     * The elements of a matrix in row-major order, as bit patterns.
     *
     * @param matrix  the matrix to read
     * @return the bit pattern of each element, row by row
     */
    private def elementBitsOf(matrix: DoubleMatrix): IndexedSeq[Long] =
      positionsOf(matrix).map { case (row, column) => bitsOf(matrix.get(row, column)) }

    /**
     * Asserts that a matrix holds exactly the expected values, as the helper of the same name in
     * the Java original did.
     *
     * With no expected value the matrix must be the canonical empty instance, which is asserted by
     * reference: every factory of the port answers with that one instance for each of the three
     * degenerate shapes rather than with a fresh empty matrix, so identity is a stronger and
     * equally true statement than emptiness. Otherwise the size is checked, then two copies
     * handed out by `toArray` are compared against each other - the original compared its copy
     * against the stored rows and required the two to agree, and with no member handing out the
     * stored rows the two reads that can be made are two calls of that accessor, which must agree
     * and must share neither their outer array nor any row - then every element is compared
     * through one of them and through the element accessor against the expectation for its
     * position, and finally the three invariants every matrix of this type reports: two
     * dimensions, non-emptiness, and squareness exactly when the two counts agree.
     *
     * The expectations are supplied flattened in row-major order and indexed by
     * `row * columnCount + column`, which is the convention of the original, so its expected-value
     * lists transfer unchanged.
     *
     * @param matrix  the matrix to inspect
     * @param expected  the values it must hold, row by row
     * @return the assertion that the matrix holds those values
     */
    private def assertMatrix(matrix: DoubleMatrix, expected: Double*): Assertion =
      if (expected.isEmpty) {
        matrix should be theSameInstanceAs DoubleMatrix.EMPTY
        matrix.size shouldBe 0
        matrix.dimensions shouldBe 2
        matrix.isEmpty shouldBe true
      } else {
        matrix.size shouldBe expected.length
        val copied = matrix.toArray
        val again = matrix.toArray
        // the deep comparison of the original, expressed by handing both to it wrapped, so that it
        // recurses into the rows rather than comparing two references
        Arrays.deepEquals(Array[AnyRef](copied), Array[AnyRef](again)) shouldBe true
        (copied eq again) shouldBe false
        (copied(0) eq again(0)) shouldBe false
        assertElementsFrom(matrix, copied, expected, 0, 0)
        matrix.dimensions shouldBe 2
        matrix.isEmpty shouldBe false
        matrix.isSquare shouldBe (matrix.rowCount == matrix.columnCount)
      }

    // compares the elements from the position upwards, in row-major order, one clue per element,
    // through the element accessor and through a copy of the rows alike
    @tailrec
    private def assertElementsFrom(
        matrix: DoubleMatrix,
        rows: Array[Array[Double]],
        expected: Seq[Double],
        row: Int,
        column: Int): Unit =

      if (row < matrix.rowCount) {
        if (column >= matrix.columnCount) {
          assertElementsFrom(matrix, rows, expected, row + 1, 0)
        } else {
          withClue(s"Unexpected value at row $row, column $column, ") {
            val required = expected(row * matrix.columnCount + column)
            matrix.get(row, column) shouldBe required
            rows(row)(column) shouldBe required
          }
          assertElementsFrom(matrix, rows, expected, row, column + 1)
        }
      }

    /**
     * Asserts that an operation over two matrices of different shapes reports the caller.
     *
     * The element-wise operations require their operands to agree on shape, and a pair that does
     * not is a broken caller rather than a data-dependent outcome, so it is raised rather than
     * handed back. The exception type and the message are those of the Java original - including
     * its wording in terms of arrays rather than matrices - and one helper keeps every call site
     * of this spec holding the operation to both.
     *
     * @param operation  the operation to evaluate, which must fail
     * @return the assertion that it failed with the expected type and message
     */
    private def assertDifferentSizes(operation: => Any): Assertion = {
      val failure = intercept[IllegalArgumentException](operation)
      failure.getMessage shouldBe "Arrays have different sizes"
    }

    /**
     * Asserts that a row-producing function whose result is the wrong length reports the caller.
     *
     * @param operation  the operation to evaluate, which must fail
     * @param actual  the length the function returned
     * @param expected  the length the shape required
     * @return the assertion that it failed with the expected type and message
     */
    private def assertIncorrectLength(operation: => Any, actual: Int, expected: Int): Assertion = {
      val failure = intercept[IllegalArgumentException](operation)
      failure.getMessage shouldBe
        s"Function returned array of incorrect length $actual, expected $expected"
    }

    /**
     * Asserts that a factory given a negative dimension reports the caller and names the argument.
     *
     * A negative row count, column count or size is a broken caller rather than a shape with no
     * elements, so it is raised rather than answered with the empty matrix, and it is raised
     * before the factory allocates anything or calls anything. The message names the argument
     * that was wrong, which is what distinguishes a negative row count from a negative column
     * count at the point of failure, so the name and the value are both asserted rather than only
     * the type. One helper keeps every case of this spec holding each factory to the same
     * contract.
     *
     * @param operation  the operation to evaluate, which must fail
     * @param argument  the name the factory gives the offending argument
     * @param value  the negative value that was passed
     * @return the assertion that it failed with the expected type and message
     */
    private def assertNegativeDimension(
        operation: => Any,
        argument: String,
        value: Int): Assertion = {

      val failure = intercept[IllegalArgumentException](operation)
      failure.getMessage shouldBe s"Argument '$argument' must not be negative but has value $value"
    }

    /**
     * The hash a matrix must report, computed independently of the type.
     *
     * The Java original folded the hash of each row into a running result in row order, starting
     * from one and multiplying by thirty-one at each step. Recomputing it here from the copy the
     * matrix hands out pins that formula rather than trusting the type to have kept it.
     *
     * @param matrix  the matrix to hash
     * @return the hash the matrix must report
     */
    private def hashOracle(matrix: DoubleMatrix): Int =
      matrix.toArray.foldLeft(1)((result, row) => 31 * result + Arrays.hashCode(row))

    /**
     * The text a matrix must render as, computed independently of the type.
     *
     * Each row holds its elements separated by single spaces and is followed by a line break -
     * every row, including the last, which is why the rendering of a non-empty matrix always ends
     * in one - and the empty matrix renders as empty text.
     *
     * @param matrix  the matrix to render
     * @return the text the matrix must render as
     */
    private def textOracle(matrix: DoubleMatrix): String =
      matrix.toArray
        .map(row => row.iterator.map(value => value.toString).mkString(" ") + "\n")
        .mkString("")

    //-------------------------------------------------------------------------
    test("test_EMPTY") {
      assertMatrix(DoubleMatrix.EMPTY)
    }

    test("test_of") {
      assertMatrix(DoubleMatrix.of())
    }

    test("test_of_values") {
      // each of the three degenerate shapes is the empty matrix, and the count of values is
      // checked before the shape is examined, so supplying none for a shape that needs none is
      // accepted while supplying one for a shape that needs two is not
      assertMatrix(DoubleMatrix.of(0, 0))
      assertMatrix(DoubleMatrix.of(1, 0))
      assertMatrix(DoubleMatrix.of(0, 1))
      assertMatrix(DoubleMatrix.of(2, 3, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
      assertMatrix(DoubleMatrix.of(6, 1, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)

      val failure = intercept[IllegalArgumentException](DoubleMatrix.of(1, 2, 1.0))
      failure.getMessage shouldBe "Values array not of length rows * columns"
    }

    //-------------------------------------------------------------------------
    test("test_of_intintlambda") {
      // the factory of the original that took a shape and a function is `tabulate` here, whose two
      // parameter lists let the function be written as a block at the call site. A shape with no
      // element must not call it at all, which is asserted by passing one that fails if it is
      assertMatrix(DoubleMatrix.tabulate(0, 0)((_, _) => fail("the function must not be invoked")))
      assertMatrix(DoubleMatrix.tabulate(0, 2)((_, _) => fail("the function must not be invoked")))
      assertMatrix(DoubleMatrix.tabulate(2, 0)((_, _) => fail("the function must not be invoked")))

      // a counter proves both that the function is called once per element and that the calls
      // happen in row-major order: the values land in the matrix in the order they were produced
      var counter: Int = 2
      def nextValue(): Double = {
        val value = counter.toDouble
        counter = counter + 1
        value
      }
      assertMatrix(DoubleMatrix.tabulate(1, 2)((_, _) => nextValue()), 2.0, 3.0)
      counter shouldBe 4

      // the indexed case of the original, whose integer arithmetic is written out as a product of
      // doubles here so that nothing is widened silently
      assertMatrix(
        DoubleMatrix.tabulate(2, 2)((row, column) => (row + 1).toDouble * (column + 1).toDouble),
        1.0, 2.0, 2.0, 4.0)
    }

    test("test_ofArrayObjects") {
      assertMatrix(DoubleMatrix.ofArrayObjects(0, 0)(_ => fail("the function must not be invoked")))
      assertMatrix(DoubleMatrix.ofArrayObjects(0, 2)(_ => fail("the function must not be invoked")))
      assertMatrix(DoubleMatrix.ofArrayObjects(2, 0)(_ => fail("the function must not be invoked")))

      // the original drew the two elements of the single row from one counter inside one call to
      // the array factory, so the row is the two values in the order they were drawn
      var counter: Int = 2
      def nextValue(): Double = {
        val value = counter.toDouble
        counter = counter + 1
        value
      }
      assertMatrix(
        DoubleMatrix.ofArrayObjects(1, 2)(_ => DoubleArray.of(nextValue(), nextValue())),
        2.0, 3.0)
      counter shouldBe 4

      // a row of the wrong length is a broken caller, reported with the message of the original
      assertIncorrectLength(DoubleMatrix.ofArrayObjects(1, 2)(_ => DoubleArray.EMPTY), 0, 2)
    }

    test("test_ofArrays") {
      assertMatrix(DoubleMatrix.ofArrays(0, 0)(_ => fail("the function must not be invoked")))
      assertMatrix(DoubleMatrix.ofArrays(0, 2)(_ => fail("the function must not be invoked")))
      assertMatrix(DoubleMatrix.ofArrays(2, 0)(_ => fail("the function must not be invoked")))

      var counter: Int = 2
      def nextValue(): Double = {
        val value = counter.toDouble
        counter = counter + 1
        value
      }
      assertMatrix(DoubleMatrix.ofArrays(1, 2)(_ => Array(nextValue(), nextValue())), 2.0, 3.0)
      counter shouldBe 4

      assertIncorrectLength(DoubleMatrix.ofArrays(1, 2)(_ => Array.empty[Double]), 0, 2)
    }

    test("test_ofUnsafe") {
      // The original's adopting factory took over the array of rows it was given, so a value built
      // that way observed every later change to any of them. The port has no such member, and this
      // is the test that answers for the Java method: the same call written against what replaces
      // it - the copying factory, which is the only route from an array of rows to a matrix - and
      // the assertion the original could not have made, that the value is unaffected by what
      // happens to the rows afterwards.
      val base = Array(Array(1.0, 2.0), Array(3.0, 4.0))
      val test = DoubleMatrix.copyOf(base)
      assertMatrix(test, 1.0, 2.0, 3.0, 4.0)
      base(0)(0) = 7.0
      base(1) = Array(9.0, 9.0)
      assertMatrix(test, 1.0, 2.0, 3.0, 4.0)

      // both degenerate shapes an array can describe are still the empty matrix, and by identity,
      // so no fresh empty value can be brought into existence by this route either
      assertMatrix(DoubleMatrix.copyOf(Array.ofDim[Double](0, 0)))
      assertMatrix(DoubleMatrix.copyOf(Array.ofDim[Double](0, 2)))
      assertMatrix(DoubleMatrix.copyOf(Array.ofDim[Double](2, 0)))
    }

    test("test_copyOf_array") {
      val base = Array(Array(1.0, 2.0), Array(3.0, 4.0))
      val test = DoubleMatrix.copyOf(base)
      assertMatrix(test, 1.0, 2.0, 3.0, 4.0)
      base(0)(0) = 7.0
      // the value copied the rows it was given, so it does not observe the change
      assertMatrix(test, 1.0, 2.0, 3.0, 4.0)

      assertMatrix(DoubleMatrix.copyOf(Array.ofDim[Double](0, 0)))
      assertMatrix(DoubleMatrix.copyOf(Array.ofDim[Double](0, 2)))
      assertMatrix(DoubleMatrix.copyOf(Array.ofDim[Double](2, 0)))
    }

    test("copyOf_refuses_rows_that_differ_in_length_where_the_original_shaped_them_by_the_first") {
      // The Java original read the column count off the first row and copied the remaining rows
      // as they stood, so an array whose rows differed in length produced a matrix that misstated
      // its own shape. The port refuses such an array instead, at the constructor every factory
      // passes through, and this is the test of that refusal - the one place the port rejects
      // input the original accepted.
      //
      // Both orientations are asserted, because they were observed differently before: a row
      // shorter than the first left the shape promising an element the row did not hold, and a
      // row longer than it kept elements the shape could not reach. Each message names the row
      // and both lengths, which is what says which row to correct.
      val shortSecondRow = Array(Array(1.0, 2.0), Array(3.0))
      val fromShort = intercept[IllegalArgumentException](DoubleMatrix.copyOf(shortSecondRow))
      fromShort.getMessage shouldBe
        "Expected every row of the matrix to hold 2 elements, but row 1 holds 1"

      val longSecondRow = Array(Array(1.0), Array(2.0, 3.0))
      val fromLong = intercept[IllegalArgumentException](DoubleMatrix.copyOf(longSecondRow))
      fromLong.getMessage shouldBe
        "Expected every row of the matrix to hold 1 elements, but row 1 holds 2"

      // the first row that disagrees is the one reported, so a message names one row however many
      // of them are wrong
      val severalWrong = Array(Array(1.0, 2.0), Array(3.0), Array(4.0, 5.0, 6.0))
      intercept[IllegalArgumentException](DoubleMatrix.copyOf(severalWrong)).getMessage shouldBe
        "Expected every row of the matrix to hold 2 elements, but row 1 holds 1"

      // neither input array is modified by the refusal, so a caller may inspect what it handed
      // over and correct it
      shortSecondRow(0)(0) shouldBe 1.0
      shortSecondRow(0).length shouldBe 2
      shortSecondRow(1).length shouldBe 1
      longSecondRow(1)(1) shouldBe 3.0
      longSecondRow(1).length shouldBe 2

      // The order of the two decisions this factory makes, which is the case worth writing down:
      // an array whose rows are all empty states a column count of zero and is the empty matrix,
      // while one whose first row is empty and whose second is not states the same column count
      // and is refused. The measurement comes before the empty short circuit, so the element the
      // second array holds cannot be lost silently.
      assertMatrix(DoubleMatrix.copyOf(Array(Array.emptyDoubleArray, Array.emptyDoubleArray)))
      intercept[IllegalArgumentException](
        DoubleMatrix.copyOf(Array(Array.emptyDoubleArray, Array(1.0)))).getMessage shouldBe
        "Expected every row of the matrix to hold 0 elements, but row 1 holds 1"

      // and rectangular input of every shape is copied as before
      assertMatrix(DoubleMatrix.copyOf(Array(Array(1.0, 2.0, 3.0))), 1.0, 2.0, 3.0)
      assertMatrix(DoubleMatrix.copyOf(Array(Array(1.0), Array(2.0), Array(3.0))), 1.0, 2.0, 3.0)
      assertMatrix(DoubleMatrix.copyOf(Array(Array(1.0, 2.0), Array(3.0, 4.0))), 1.0, 2.0, 3.0, 4.0)
    }

    test("every_value_of_this_type_is_rectangular_whatever_route_built_it") {
      // The refusal above is one factory's; this is the property it exists for, asserted of the
      // type. The check is made by the constructor, so it covers every route into a value - the
      // factories given a shape, the factories given rows, and every operation that derives one
      // value from another - and what it buys is that a position the shape names is a position
      // the rows hold. That is asserted here the only way it can be asserted from outside the
      // class: every position of the shape is read, through the element accessor and through the
      // rows the copying accessor hands back, and every row is measured against the column count.
      val values =
        List(
          DoubleMatrix.copyOf(Array(Array(1.0, 2.0), Array(3.0, 4.0))),
          DoubleMatrix.of(2, 3, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0),
          DoubleMatrix.tabulate(3, 2)((row, column) => (row + column).toDouble),
          DoubleMatrix.ofArrays(2, 2)(row => Array(row.toDouble, row.toDouble)),
          DoubleMatrix.ofArrayObjects(2, 2)(row => DoubleArray.of(row.toDouble, row.toDouble)),
          DoubleMatrix.filled(2, 2, 1.5),
          DoubleMatrix.identity(3),
          DoubleMatrix.diagonal(DoubleArray.of(1.0, 2.0)),
          DoubleMatrix.of(2, 2, 1.0, 2.0, 3.0, 4.0).transpose,
          DoubleMatrix.of(2, 2, 1.0, 2.0, 3.0, 4.0).`with`(1, 1, 9.0),
          DoubleMatrix.of(2, 2, 1.0, 2.0, 3.0, 4.0).multipliedBy(2.0),
          DoubleMatrix.of(2, 2, 1.0, 2.0, 3.0, 4.0).map(value => value + 1.0),
          DoubleMatrix.EMPTY)
      values.foreach { value =>
        withClue(s"the rows of ${value.toString} against a shape of ${value.rowCount} x ${value.columnCount}: ") {
          val rows = value.toArray
          rows.length shouldBe value.rowCount
          rows.map(row => row.length).distinct.filterNot(_ == value.columnCount) shouldBe empty
          positionsOf(value).filterNot { case (row, column) =>
            bitsOf(value.get(row, column)) == bitsOf(rows(row)(column))
          } shouldBe empty
        }
      }
    }

    test("toString_renders_every_shape_a_value_of_this_type_can_have") {
      // Rendering walks each row to that row's own length, which for every value of this type is
      // the column count the shape states, because the constructor measures the two against each
      // other. The expected text is that of the Java original, which rendered each row at its own
      // length for the same reason this does - the row is what the rendering is handed.
      //
      // The shapes below are every shape there is: several rows and several columns, one row, one
      // column, and no elements at all. The rows that differ in length which the original could
      // render are not among them, because no value of this type has them any more - that is
      // asserted where it belongs, at the factory that used to accept them.
      DoubleMatrix.copyOf(Array(Array(1.0, 2.0), Array(3.0, 4.0))).toString shouldBe "1.0 2.0\n3.0 4.0\n"
      DoubleMatrix.copyOf(Array(Array(1.0), Array(2.0), Array(3.0))).toString shouldBe "1.0\n2.0\n3.0\n"
      DoubleMatrix.of(1, 3, 1.0, 2.0, 3.0).toString shouldBe "1.0 2.0 3.0\n"
      DoubleMatrix.of(1, 1, 1.0).toString shouldBe "1.0\n"
      DoubleMatrix.EMPTY.toString shouldBe ""
    }

    //-------------------------------------------------------------------------
    test("test_filled") {
      assertMatrix(DoubleMatrix.filled(0, 0))
      assertMatrix(DoubleMatrix.filled(0, 2))
      assertMatrix(DoubleMatrix.filled(2, 0))
      assertMatrix(DoubleMatrix.filled(3, 2), 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    }

    test("test_filled_withValue") {
      assertMatrix(DoubleMatrix.filled(0, 0, 7.0))
      assertMatrix(DoubleMatrix.filled(0, 2, 7.0))
      assertMatrix(DoubleMatrix.filled(2, 0, 7.0))
      assertMatrix(DoubleMatrix.filled(3, 2, 7.0), 7.0, 7.0, 7.0, 7.0, 7.0, 7.0)
    }

    //-------------------------------------------------------------------------
    test("test_identity") {
      assertMatrix(DoubleMatrix.identity(0))
      assertMatrix(DoubleMatrix.identity(2), 1.0, 0.0, 0.0, 1.0)
    }

    //-------------------------------------------------------------------------
    test("test_diagonal") {
      assertMatrix(DoubleMatrix.diagonal(DoubleArray.EMPTY))
      assertMatrix(
        DoubleMatrix.diagonal(DoubleArray.of(2.0, 3.0, 4.0)),
        2.0, 0.0, 0.0,
        0.0, 3.0, 0.0,
        0.0, 0.0, 4.0)
      DoubleMatrix.diagonal(DoubleArray.of(1.0, 1.0, 1.0)) shouldBe DoubleMatrix.identity(3)
    }

    //-------------------------------------------------------------------------
    test("test_get") {
      val test = matrix3x2
      test.get(0, 0) shouldBe 1.0
      test.get(2, 1) shouldBe 6.0
      // both indices reach the stored rows directly, so either one outside the matrix fails with
      // the index exception the runtime raises, as the member documents and as the original did
      assertThrows[IndexOutOfBoundsException](test.get(-1, 0))
      assertThrows[IndexOutOfBoundsException](test.get(0, 4))
    }

    test("test_row") {
      val test = matrix3x2
      test.row(0) shouldBe DoubleArray.of(1.0, 2.0)
      test.row(1) shouldBe DoubleArray.of(3.0, 4.0)
      test.row(2) shouldBe DoubleArray.of(5.0, 6.0)
      assertThrows[IndexOutOfBoundsException](test.row(-1))
      assertThrows[IndexOutOfBoundsException](test.row(4))

      // The port copies the row rather than wrapping the stored one, which the original did. The
      // array type is itself immutable, so the difference is invisible through its own surface and
      // has to be shown one level down, through the copying accessors of both types: the run of
      // values a returned row answers with is not the row the matrix hands out a copy of, and two
      // reads of the same row are two distinct runs. `copy_safety_of_row_and_column` shows that
      // writing to either changes nothing
      (test.row(0).toArray eq test.toArray(0)) shouldBe false
      (test.row(0).toArray eq test.row(0).toArray) shouldBe false
    }

    test("test_rowArray") {
      val test = matrix3x2
      Arrays.equals(test.rowArray(0), Array(1.0, 2.0)) shouldBe true
      Arrays.equals(test.rowArray(1), Array(3.0, 4.0)) shouldBe true
      Arrays.equals(test.rowArray(2), Array(5.0, 6.0)) shouldBe true
      assertThrows[IndexOutOfBoundsException](test.rowArray(-1))
      assertThrows[IndexOutOfBoundsException](test.rowArray(4))
    }

    test("test_column") {
      val test = matrix3x2
      test.column(0) shouldBe DoubleArray.of(1.0, 3.0, 5.0)
      test.column(1) shouldBe DoubleArray.of(2.0, 4.0, 6.0)
      // a column is built by reading one element from each row in turn, so an index outside the
      // matrix fails on the first row it reads
      assertThrows[IndexOutOfBoundsException](test.column(-1))
      assertThrows[IndexOutOfBoundsException](test.column(4))

      // the empty matrix has no row to read, so every column index answers with the empty array
      // rather than failing. That is the behaviour of the original, and it is the one place where
      // the two accessors of a column differ from those of a row
      DoubleMatrix.EMPTY.column(0) should be theSameInstanceAs DoubleArray.EMPTY
      DoubleMatrix.EMPTY.column(-1) should be theSameInstanceAs DoubleArray.EMPTY
      DoubleMatrix.EMPTY.column(4) should be theSameInstanceAs DoubleArray.EMPTY
    }

    test("test_columnArray") {
      val test = matrix3x2
      Arrays.equals(test.columnArray(0), Array(1.0, 3.0, 5.0)) shouldBe true
      Arrays.equals(test.columnArray(1), Array(2.0, 4.0, 6.0)) shouldBe true
      assertThrows[IndexOutOfBoundsException](test.columnArray(-1))
      assertThrows[IndexOutOfBoundsException](test.columnArray(4))
      test.columnArray(0).length shouldBe test.rowCount
    }

    //-------------------------------------------------------------------------
    test("test_forEach") {
      val test = matrix3x2
      val extracted = new Array[Double](6)
      // the action answers with the assignment, which is the one shape that keeps the lambda's
      // result a unit rather than a value nobody reads
      test.forEach((row, column, value) => extracted(row * 2 + column) = value)
      Arrays.equals(extracted, Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)) shouldBe true
    }

    //-------------------------------------------------------------------------
    test("test_with") {
      val test = matrix3x2
      assertMatrix(test.`with`(0, 0, 2.6), 2.6, 2.0, 3.0, 4.0, 5.0, 6.0)
      assertMatrix(test.`with`(0, 0, 1.0), 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
      // replacing a value with the one already there answers with the same instance, which the
      // member documents; the assertion above only shows the elements are unchanged
      test.`with`(0, 0, 1.0) should be theSameInstanceAs test

      assertThrows[IndexOutOfBoundsException](test.`with`(-1, 0, 2.0))
      assertThrows[IndexOutOfBoundsException](test.`with`(3, 0, 2.0))
      assertThrows[IndexOutOfBoundsException](test.`with`(0, -1, 2.0))
      assertThrows[IndexOutOfBoundsException](test.`with`(0, 3, 2.0))
    }

    //-------------------------------------------------------------------------
    test("test_multipliedBy") {
      val test = matrix3x2
      assertMatrix(test.multipliedBy(5.0), 5.0, 10.0, 15.0, 20.0, 25.0, 30.0)
      assertMatrix(test.multipliedBy(1.0), 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
      test.multipliedBy(1.0) should be theSameInstanceAs test

      // Scaling by a number is the only multiplication either the original or the port offers:
      // there is no product of two matrices, which belongs to the mathematics built on top of this
      // type. Asserting the absence keeps a later slice from adding one unnoticed, and the
      // positive control above it proves the assertion shape works where the member does exist
      assertCompiles(
        "com.opengamma.strata.collect.array.DoubleMatrix.of(1, 1, 1.0).multipliedBy(2.0)")
      assertDoesNotCompile(
        "com.opengamma.strata.collect.array.DoubleMatrix.of(1, 1, 1.0)" +
          ".multipliedBy(com.opengamma.strata.collect.array.DoubleMatrix.of(1, 1, 1.0))")
    }

    test("test_map") {
      val test = matrix3x2
      assertMatrix(
        test.map(value => 1.0 / value),
        1.0, 1.0 / 2.0,
        1.0 / 3.0, 1.0 / 4.0,
        1.0 / 5.0, 1.0 / 6.0)
    }

    test("test_mapWithIndex") {
      val test = matrix3x2
      assertMatrix(
        test.mapWithIndex((row, column, value) => row.toDouble * (column + 1).toDouble * value),
        0.0, 0.0, 3.0, 8.0, 10.0, 24.0)
    }

    //-------------------------------------------------------------------------
    test("testTransposeMatrix") {
      DoubleMatrix.EMPTY.transpose shouldBe DoubleMatrix.EMPTY
      DoubleMatrix.EMPTY.transpose should be theSameInstanceAs DoubleMatrix.EMPTY

      val square = DoubleMatrix.copyOf(
        Array(
          Array(1.0, 2.0, 3.0),
          Array(4.0, 5.0, 6.0),
          Array(7.0, 8.0, 9.0)))
      square.transpose shouldBe DoubleMatrix.copyOf(
        Array(
          Array(1.0, 4.0, 7.0),
          Array(2.0, 5.0, 8.0),
          Array(3.0, 6.0, 9.0)))

      val oblong = DoubleMatrix.copyOf(
        Array(
          Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0),
          Array(7.0, 8.0, 9.0, 10.0, 11.0, 12.0),
          Array(13.0, 14.0, 15.0, 16.0, 17.0, 18.0)))
      oblong.transpose shouldBe DoubleMatrix.copyOf(
        Array(
          Array(1.0, 7.0, 13.0),
          Array(2.0, 8.0, 14.0),
          Array(3.0, 9.0, 15.0),
          Array(4.0, 10.0, 16.0),
          Array(5.0, 11.0, 17.0),
          Array(6.0, 12.0, 18.0)))
      oblong.transpose.rowCount shouldBe 6
      oblong.transpose.columnCount shouldBe 3
    }

    //-------------------------------------------------------------------------
    test("test_plus") {
      val test1 = DoubleMatrix.of(2, 3, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
      val test2 = DoubleMatrix.of(2, 3, 0.5, 0.6, 0.7, 0.5, 0.6, 0.7)
      assertMatrix(test1.plus(test2), 1.5, 2.6, 3.7, 4.5, 5.6, 6.7)
      assertDifferentSizes(test1.plus(DoubleMatrix.EMPTY))
    }

    test("test_minus") {
      val test1 = DoubleMatrix.of(2, 3, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
      val test2 = DoubleMatrix.of(2, 3, 0.5, 0.6, 0.7, 0.5, 0.6, 0.7)
      assertMatrix(test1.minus(test2), 0.5, 1.4, 2.3, 3.5, 4.4, 5.3)
      assertDifferentSizes(test1.minus(DoubleMatrix.EMPTY))
    }

    test("test_combine") {
      val test1 = DoubleMatrix.of(2, 3, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
      val test2 = DoubleMatrix.of(2, 3, 0.5, 0.6, 0.7, 0.5, 0.6, 0.7)
      // the expectations of the original are products rather than decimals, because the product of
      // two doubles is not the decimal a reader would write for it; they are carried as written
      assertMatrix(
        test1.combine(test2, (left, right) => left * right),
        0.5, 2.0 * 0.6, 3.0 * 0.7,
        4.0 * 0.5, 5.0 * 0.6, 6.0 * 0.7)
      assertDifferentSizes(test1.combine(DoubleMatrix.EMPTY, (left, right) => left * right))
    }

    //-------------------------------------------------------------------------
    test("test_total") {
      DoubleMatrix.EMPTY.total shouldBe 0.0
      matrix3x2.total shouldBe 21.0
    }

    test("test_reduce") {
      // an empty matrix reduces to the identity without the operator being called at all, which is
      // asserted by passing one that fails if it is
      DoubleMatrix.EMPTY.reduce(2.0, (_, _) => fail("the operator must not be invoked")) shouldBe
        2.0
      DoubleMatrix
        .copyOf(Array(Array(2.0)))
        .reduce(1.0, (result, value) => result * value) shouldBe 2.0
      DoubleMatrix
        .copyOf(Array(Array(2.0, 3.0)))
        .reduce(1.0, (result, value) => result * value) shouldBe 6.0
    }

    //-------------------------------------------------------------------------
    test("test_equalsHashCode") {
      val a1 = DoubleMatrix.copyOf(Array(Array(2.0, 3.0)))
      val a2 = DoubleMatrix.copyOf(Array(Array(2.0, 3.0)))
      val b = DoubleMatrix.copyOf(Array(Array(3.0, 3.0)))
      val c = DoubleMatrix.copyOf(Array(Array(2.0, 3.0), Array(4.0, 5.0)))
      val d = DoubleMatrix.copyOf(Array(Array(2.0)))
      a1.equals(a1) shouldBe true
      a1.equals(a2) shouldBe true
      a1.equals(b) shouldBe false
      // a different row count and a different column count are each enough to make two matrices
      // unequal, whatever their elements
      a1.equals(c) shouldBe false
      a1.equals(d) shouldBe false
      a1.equals(anotherType) shouldBe false
      a1.equals(nullRef) shouldBe false
      a1.hashCode shouldBe a2.hashCode
    }

    test("test_toString") {
      matrix3x2.toString shouldBe "1.0 2.0\n3.0 4.0\n5.0 6.0\n"
    }

    //-------------------------------------------------------------------------
    test("coverage") {
      // The original called a reflective sweep over the properties of a bean, on the empty matrix
      // and on a three-by-two one. This port has no bean and no reflective property access - doing
      // without them is one of its requirements - so that call has no target. Its substance does:
      // it stood for the claims that two instances built from equal parts are equal, that
      // instances differing anywhere are not, that an instance renders itself faithfully, and that
      // the type's declared surface is what it claims to be. Each is asserted here directly, on
      // the type's own members and on its typeclass instances, over both of the values the
      // original swept. Every assertion of an absence is preceded by a positive control in the
      // same shape, so that none of them can pass because an expression was malformed.
      val test = DoubleMatrix.of(2, 3, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
      val equal = DoubleMatrix.of(2, 3, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
      val different = DoubleMatrix.of(2, 3, 1.0, 2.0, 3.0, 4.0, 5.0, 7.0)
      val reshaped = DoubleMatrix.of(3, 2, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)

      val hash = implicitly[Hash[DoubleMatrix]]
      hash.eqv(test, equal) shouldBe true
      hash.eqv(test, different) shouldBe false
      hash.eqv(test, reshaped) shouldBe false
      hash.eqv(DoubleMatrix.EMPTY, DoubleMatrix.EMPTY) shouldBe true
      hash.eqv(DoubleMatrix.EMPTY, test) shouldBe false
      hash.hash(test) shouldBe test.hashCode
      hash.hash(equal) shouldBe hash.hash(test)
      hash.hash(DoubleMatrix.EMPTY) shouldBe DoubleMatrix.EMPTY.hashCode

      val show = implicitly[Show[DoubleMatrix]]
      show.show(test) shouldBe "1.0 2.0 3.0\n4.0 5.0 6.0\n"
      show.show(test) shouldBe test.toString
      show.show(DoubleMatrix.EMPTY) shouldBe ""
      show.show(DoubleMatrix.EMPTY) shouldBe DoubleMatrix.EMPTY.toString

      // the abstraction the type declares itself under: a matrix of this type is two-dimensional,
      // and the two members that abstraction asks for answer for every value, the empty one
      // included
      test shouldBe a[Matrix]
      (test: Matrix).dimensions shouldBe 2
      (test: Matrix).size shouldBe 6
      (DoubleMatrix.EMPTY: Matrix).dimensions shouldBe 2
      (DoubleMatrix.EMPTY: Matrix).size shouldBe 0

      // Exactly one equality-bearing instance is declared for this type: `Hash` extends `Eq`, so
      // summoning an `Eq` finds that very instance rather than a second one that could disagree.
      // That it compiles is itself half of the claim - two candidate instances would be ambiguous
      // and would not - and that it is the same instance is the other half.
      assertCompiles("implicitly[cats.Eq[com.opengamma.strata.collect.array.DoubleMatrix]]")
      val equality = implicitly[Eq[DoubleMatrix]]
      sameInstance(equality, hash) shouldBe true
      equality.eqv(test, equal) shouldBe true
      equality.eqv(test, different) shouldBe false

      // no ordering is declared: matrices are compared for equality only, and no useful total
      // order over them exists to offer
      assertCompiles("implicitly[cats.Order[com.opengamma.strata.collect.Decimal]]")
      assertDoesNotCompile(
        "implicitly[cats.Order[com.opengamma.strata.collect.array.DoubleMatrix]]")

      // no meta-bean and no meta-property, by name or otherwise
      assertCompiles("com.opengamma.strata.collect.array.DoubleMatrix.of(1, 1, 1.0).size")
      assertDoesNotCompile("com.opengamma.strata.collect.array.DoubleMatrix.of(1, 1, 1.0).metaBean")
      assertDoesNotCompile(
        """com.opengamma.strata.collect.array.DoubleMatrix.of(1, 1, 1.0)""" +
          """.metaBean.metaProperty("array")""")

      // the type publishes no JSON codec of its own. The codec object of this module owns it, as
      // the two round trips below show, and a derivation site reaches it through an import rather
      // than finding it in the implicit scope of the type - which the first of the three
      // assertions proves is a choice, since a type of this module that does publish one is found
      // that way
      assertCompiles("implicitly[_root_.io.circe.Encoder[com.opengamma.strata.collect.Decimal]]")
      assertDoesNotCompile(
        "implicitly[_root_.io.circe.Encoder[com.opengamma.strata.collect.array.DoubleMatrix]]")
      assertDoesNotCompile(
        "implicitly[_root_.io.circe.Decoder[com.opengamma.strata.collect.array.DoubleMatrix]]")
      Codecs.doubleMatrixCodec.decodeJson(Codecs.doubleMatrixCodec(test)) shouldBe Right(test)
      Codecs.doubleMatrixCodec.decodeJson(
        Codecs.doubleMatrixCodec(DoubleMatrix.EMPTY)) shouldBe Right(DoubleMatrix.EMPTY)
    }

    //-------------------------------------------------------------------------
    test("copy_safety_of_copyOf_array") {
      // the factory copies both the array of rows and every row within it, so neither the row
      // references nor the elements the caller still holds can reach the matrix
      val base = rows3x2
      val test = DoubleMatrix.copyOf(base)
      base(0) = Array(9.0, 9.0)
      base(1)(0) = 8.0
      base(2)(1) = 7.0
      assertMatrix(test, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    }

    test("copy_safety_of_toArray") {
      // The copying accessor answers with a fresh array of fresh rows on every call. Both halves
      // matter: a shallow copy would hand out the stored rows inside a new outer array, and
      // writing to one of those rows would change the matrix. Writing to a row of the result is
      // therefore the assertion that distinguishes a deep copy from a shallow one.
      val test = matrix3x2
      val first = test.toArray
      val second = test.toArray
      (first eq second) shouldBe false
      (first eq test.toArray) shouldBe false
      (first(0) eq second(0)) shouldBe false
      (first(0) eq test.toArray(0)) shouldBe false

      first(0)(0) = 9.0
      first(2) = Array(0.0, 0.0)
      second(1)(1) = 8.0
      assertMatrix(test, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
      Arrays.deepEquals(
        Array[AnyRef](test.toArray),
        Array[AnyRef](rows3x2)) shouldBe true
    }

    test("copy_safety_of_row_and_column") {
      // This is the headline behavioural difference from the Java original for this type: it
      // returned the stored row from `row`, and the array behind a freshly built column, relying
      // on a documented convention that no caller would write to either. The port copies, so the
      // convention becomes a property of the value: nothing obtained from a row or a column of a
      // matrix is a path back into it.
      val test = matrix3x2

      val extractedRow = test.rowArray(1)
      extractedRow(0) = 9.0
      test.get(1, 0) shouldBe 3.0
      test.row(1) shouldBe DoubleArray.of(3.0, 4.0)
      Arrays.equals(test.rowArray(1), Array(3.0, 4.0)) shouldBe true

      val extractedColumn = test.columnArray(0)
      extractedColumn(2) = 9.0
      test.get(2, 0) shouldBe 5.0
      test.column(0) shouldBe DoubleArray.of(1.0, 3.0, 5.0)
      Arrays.equals(test.columnArray(0), Array(1.0, 3.0, 5.0)) shouldBe true

      // and each call answers with storage of its own, so two callers cannot reach each other
      (test.rowArray(0) eq test.rowArray(0)) shouldBe false
      (test.columnArray(0) eq test.columnArray(0)) shouldBe false
      assertMatrix(test, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    }

    test("copy_safety_of_the_row_producing_factories") {
      // The factory that takes rows as primitive arrays copies each one, which is what lets the
      // function reuse a single buffer - the sharpest input it can be given, since every row it
      // returns is the same array. The factory that takes them as values of the array type does
      // not need to copy, because that type is immutable and the caller's own source was copied
      // when the value was built.
      val buffer = new Array[Double](2)
      val fromArrays = DoubleMatrix.ofArrays(3, 2) { row =>
        buffer(0) = (row * 2 + 1).toDouble
        buffer(1) = (row * 2 + 2).toDouble
        buffer
      }
      assertMatrix(fromArrays, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
      buffer(0) = 9.0
      buffer(1) = 9.0
      assertMatrix(fromArrays, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)

      val source = rows3x2
      val fromArrayObjects =
        DoubleMatrix.ofArrayObjects(3, 2)(row => DoubleArray.copyOf(source(row)))
      source(0)(0) = 9.0
      assertMatrix(fromArrayObjects, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)

      // the factory that produces its own elements allocates the rows it wraps, so nothing the
      // caller holds can reach them
      val tabulated = DoubleMatrix.tabulate(3, 2)((row, column) => source(row)(column))
      source(1)(1) = 9.0
      assertMatrix(tabulated, 9.0, 2.0, 3.0, 4.0, 5.0, 6.0)
      tabulated.toArray(0)(0) = 0.0
      assertMatrix(tabulated, 9.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    }

    test("no_member_hands_out_the_stored_rows") {
      // The copy safety above is asserted through the source, which is the right level for what
      // each member does and the wrong level for the claim that no member does otherwise: a
      // source file can only be read for the members it happens to mention. This test reads the
      // compiled class instead, so a member added later is covered by it whether anyone thought
      // to test that member or not.
      //
      // Two statements, and the second is the one that matters. No declared name carries the word
      // the original's escape hatches were named for - that is the cheap check, and it is here
      // because those two names are what a reader looks for. Then: of every member the class
      // publishes, exactly three answer with a run of values and one with an array of rows, and
      // all four are the copying accessors. That is the statement immutability rests on, because
      // a member of any other name that returned a stored row would defeat it just as thoroughly.
      //
      // The deep copy of the rows lives on the companion, where the constructor and `toArray`
      // both reach it, and the compiler emits it under a name of its own devising. It answers
      // with an array of rows and is therefore named here as what it is: a copier of the argument
      // it is handed, which reads no field of any instance, so it is no route into a matrix.
      val declared = classOf[DoubleMatrix].getDeclaredMethods.toList
      val companion = DoubleMatrix.getClass.getDeclaredMethods.toList
      withClue("members whose name carries the word the original's escape hatches were named for: ") {
        (declared ::: companion).map(method => method.getName).filter(name =>
          name.contains("Unsafe")) shouldBe empty
      }

      val publicArrayReturns =
        (declared ::: companion)
          .filter(method => Modifier.isPublic(method.getModifiers))
          .filter(method =>
            method.getReturnType == classOf[Array[Double]] ||
              method.getReturnType == classOf[Array[Array[Double]]])
          .map(method => method.getName)
          .distinct
          .sorted
      withClue(s"public members answering with rows or with a run of values: $publicArrayReturns: ") {
        publicArrayReturns shouldBe
          List("columnArray", "com$opengamma$strata$collect$array$DoubleMatrix$$deepClone",
            "rowArray", "toArray")
      }

      // and each of those accessors answers with fresh data on every call, so holding the result
      // of one call is not a way to observe or change what a later call sees
      val test = matrix3x2
      val rows = test.toArray
      val again = test.toArray
      (rows eq again) shouldBe false
      (rows(0) eq again(0)) shouldBe false
      (test.rowArray(0) eq test.rowArray(0)) shouldBe false
      (test.columnArray(0) eq test.columnArray(0)) shouldBe false
      rows(0)(0) = 9.0
      again(1)(1) = 8.0
      test.rowArray(2)(0) = 7.0
      test.columnArray(1)(0) = 6.0
      assertMatrix(test, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    }

    test("the_public_constructor_copies_what_it_is_handed") {
      // The constructor is declared private and is emitted public regardless, because the
      // companion that every factory lives in has to reach it. That is not a defect to be hidden
      // - it cannot be hidden, in this language, on this platform - so it is the path this test
      // takes deliberately: the rows are handed to the constructor itself, reflectively, exactly
      // as a caller outside this language would hand them over, and then changed, outer array and
      // row alike. The value does not move, because the copy is made by the constructor rather
      // than by the factory in front of it. There is exactly one such constructor, which is
      // asserted too: a second one would be a second construction path to hold to the same
      // contract.
      val constructors = classOf[DoubleMatrix].getConstructors.toList
      constructors should have size 1

      val source = Array(Array(1.0, 2.0), Array(3.0, 4.0))
      val built = constructors.head
        .newInstance(source.asInstanceOf[AnyRef], Integer.valueOf(2), Integer.valueOf(2))
        .asInstanceOf[DoubleMatrix]
      assertMatrix(built, 1.0, 2.0, 3.0, 4.0)
      source(0)(0) = 9.0
      source(1) = Array(7.0, 7.0)
      assertMatrix(built, 1.0, 2.0, 3.0, 4.0)

      // and nothing the value hands back afterwards reaches the rows that built it
      (built.toArray eq source) shouldBe false
      (built.toArray(0) eq source(0)) shouldBe false
    }

    test("unsafe_members_resolve_to_nothing") {
      // The two names the Java original published are offered to the compiler here, inside
      // `com.opengamma.strata.collect`, where a member restricted to this module would have been
      // visible - so this is the assertion that neither exists at all rather than that neither is
      // reachable. The same two expressions are then offered to a probe object in a sibling
      // package, which is what shows the answer does not depend on where the question is asked.
      //
      // The positive control is the line above each rejection: the same expression with a member
      // that does exist compiles, so a rejection cannot be the reward for a malformed expression.
      assertCompiles("com.opengamma.strata.collect.array.DoubleMatrix.copyOf(Array(Array(1.0)))")
      assertDoesNotCompile("com.opengamma.strata.collect.array.DoubleMatrix.ofUnsafe(Array(Array(1.0)))")
      assertCompiles("com.opengamma.strata.collect.array.DoubleMatrix.of(1, 1, 1.0).toArray")
      assertDoesNotCompile("com.opengamma.strata.collect.array.DoubleMatrix.of(1, 1, 1.0).toArrayUnsafe")
      com.opengamma.strata.audit.DoubleMatrixUnsafeAccessProbe.ofUnsafeIsInaccessible
      com.opengamma.strata.audit.DoubleMatrixUnsafeAccessProbe.toArrayUnsafeIsInaccessible
    }

    //-------------------------------------------------------------------------
    test("ieee_equality_of_nan_and_signed_zero") {
      // equality compares elements bit for bit, so a not-a-number element equals itself and a
      // matrix holding one can be compared and hashed like any other
      val nan1 = DoubleMatrix.of(1, 1, Double.NaN)
      val nan2 = DoubleMatrix.of(1, 1, Double.NaN)
      nan1.equals(nan2) shouldBe true
      nan1.equals(nan1) shouldBe true
      nan1.hashCode shouldBe nan2.hashCode
      matrixHash.eqv(nan1, nan2) shouldBe true
      matrixHash.hash(nan1) shouldBe nan1.hashCode
      bitsOf(nan1.get(0, 0)) shouldBe bitsOf(Double.NaN)

      // the two zeroes have different bit patterns, so they are different elements, and the hash
      // that folds them disagrees as well
      val positiveZero = DoubleMatrix.of(1, 1, 0.0)
      val negativeZero = DoubleMatrix.of(1, 1, -0.0)
      positiveZero.equals(negativeZero) shouldBe false
      positiveZero.hashCode should not be negativeZero.hashCode
      matrixHash.eqv(positiveZero, negativeZero) shouldBe false
      bitsOf(negativeZero.get(0, 0)) shouldBe bitsOf(-0.0)

      // the same element compared at a position inside a larger matrix, so that the comparison is
      // reached through the row-major walk rather than at the first element
      val mixed1 = DoubleMatrix.of(2, 2, 1.0, Double.NaN, -0.0, 2.0)
      val mixed2 = DoubleMatrix.of(2, 2, 1.0, Double.NaN, -0.0, 2.0)
      val differing = DoubleMatrix.of(2, 2, 1.0, Double.NaN, 0.0, 2.0)
      matrixHash.eqv(mixed1, mixed2) shouldBe true
      matrixHash.eqv(mixed1, differing) shouldBe false
      mixed1.hashCode shouldBe mixed2.hashCode
    }

    test("ieee_equality_of_the_infinities") {
      val positive = DoubleMatrix.of(1, 1, Double.PositiveInfinity)
      val negative = DoubleMatrix.of(1, 1, Double.NegativeInfinity)
      positive.equals(DoubleMatrix.of(1, 1, Double.PositiveInfinity)) shouldBe true
      positive.hashCode shouldBe DoubleMatrix.of(1, 1, Double.PositiveInfinity).hashCode
      positive.equals(negative) shouldBe false
      matrixHash.eqv(positive, DoubleMatrix.of(1, 1, Double.PositiveInfinity)) shouldBe true
      matrixHash.eqv(positive, negative) shouldBe false
      matrixHash.hash(negative) shouldBe negative.hashCode
      positive.toString shouldBe "Infinity\n"
      negative.toString shouldBe "-Infinity\n"
    }

    //-------------------------------------------------------------------------
    test("hashCode_agrees_with_the_row_folding_oracle") {
      // the hash of each row folded into a running result in row order, from one, multiplying by
      // thirty-one at each step, which is the formula of the Java original
      val samples = Table[String, DoubleMatrix](
        ("shape", "matrix"),
        ("empty", DoubleMatrix.EMPTY),
        ("one by one", DoubleMatrix.of(1, 1, 1.0)),
        ("one row", DoubleMatrix.of(1, 3, 1.0, 2.0, 3.0)),
        ("one column", DoubleMatrix.of(3, 1, 1.0, 2.0, 3.0)),
        ("three by two", matrix3x2),
        ("edge values", DoubleMatrix.of(2, 2, Double.NaN, -0.0, 0.0, Double.PositiveInfinity)))
      forAll(samples) { (shape: String, matrix: DoubleMatrix) =>
        withClue(s"$shape, ") {
          matrix.hashCode shouldBe hashOracle(matrix)
          matrixHash.hash(matrix) shouldBe hashOracle(matrix)
        }
      }
      // the empty matrix folds nothing, so it reports the starting value of the fold
      DoubleMatrix.EMPTY.hashCode shouldBe 1
    }

    test("toString_agrees_with_the_row_joining_layout") {
      // every row is terminated by a line break, including the last, so the rendering of a
      // non-empty matrix always ends in one and a single-row matrix is not a bare line. The two
      // degenerate shapes are where the space and the line break could be confused, so both are
      // covered explicitly.
      val samples = Table[String, DoubleMatrix, String](
        ("shape", "matrix", "rendering"),
        ("empty", DoubleMatrix.EMPTY, ""),
        ("one by one", DoubleMatrix.of(1, 1, 1.0), "1.0\n"),
        ("one row", DoubleMatrix.of(1, 3, 1.0, 2.0, 3.0), "1.0 2.0 3.0\n"),
        ("one column", DoubleMatrix.of(3, 1, 1.0, 2.0, 3.0), "1.0\n2.0\n3.0\n"),
        ("three by two", matrix3x2, "1.0 2.0\n3.0 4.0\n5.0 6.0\n"))
      forAll(samples) { (shape: String, matrix: DoubleMatrix, rendering: String) =>
        withClue(s"$shape, ") {
          matrix.toString shouldBe rendering
          matrix.toString shouldBe textOracle(matrix)
          implicitly[Show[DoubleMatrix]].show(matrix) shouldBe rendering
        }
      }
    }

    //-------------------------------------------------------------------------
    test("shape_invariants_and_the_degenerate_empty_shapes") {
      val samples = Table[DoubleMatrix](
        "matrix",
        DoubleMatrix.EMPTY,
        DoubleMatrix.of(1, 1, 1.0),
        DoubleMatrix.of(1, 3, 1.0, 2.0, 3.0),
        DoubleMatrix.of(3, 1, 1.0, 2.0, 3.0),
        matrix3x2,
        DoubleMatrix.identity(4))
      forAll(samples) { (matrix: DoubleMatrix) =>
        matrix.size shouldBe matrix.rowCount * matrix.columnCount
        matrix.dimensions shouldBe 2
        matrix.isSquare shouldBe (matrix.rowCount == matrix.columnCount)
        matrix.isEmpty shouldBe (matrix.size == 0)
        matrix.toArray.length shouldBe matrix.rowCount
      }

      // all three degenerate shapes are the one empty matrix, which is square, two-dimensional and
      // has no row at all - so no matrix of this type has a row of length zero
      val degenerate = Table[String, DoubleMatrix](
        ("shape", "matrix"),
        ("no rows and no columns", DoubleMatrix.filled(0, 0)),
        ("no rows", DoubleMatrix.filled(0, 2)),
        ("no columns", DoubleMatrix.filled(2, 0)))
      forAll(degenerate) { (shape: String, matrix: DoubleMatrix) =>
        withClue(s"$shape, ") {
          matrix should be theSameInstanceAs DoubleMatrix.EMPTY
          matrix.isEmpty shouldBe true
          matrix.rowCount shouldBe 0
          matrix.columnCount shouldBe 0
          matrix.isSquare shouldBe true
        }
      }
    }

    test("exception_types_of_the_documented_failures") {
      // Every failure of this type is a caller-contract violation rather than a data-dependent
      // outcome, so each is raised rather than handed back as a value to inspect, and there are
      // exactly two kinds. An index outside the matrix reaches the stored rows directly, so it
      // surfaces as the exception the runtime raises for the access, which is a subclass of the
      // one the Java original raised and the member documents; both are asserted, so the record of
      // the divergence is the test rather than only the note.
      val test = matrix3x2
      val indexFailures = Table[String, () => Any](
        ("member", "operation"),
        ("get, negative row", () => test.get(-1, 0)),
        ("get, row past the end", () => test.get(3, 0)),
        ("get, negative column", () => test.get(0, -1)),
        ("get, column past the end", () => test.get(0, 4)),
        ("row, negative", () => test.row(-1)),
        ("row, past the end", () => test.row(4)),
        ("rowArray, negative", () => test.rowArray(-1)),
        ("rowArray, past the end", () => test.rowArray(4)),
        ("column, negative", () => test.column(-1)),
        ("column, past the end", () => test.column(4)),
        ("columnArray, negative", () => test.columnArray(-1)),
        ("columnArray, past the end", () => test.columnArray(4)),
        ("with, negative row", () => test.`with`(-1, 0, 2.0)),
        ("with, row past the end", () => test.`with`(3, 0, 2.0)),
        ("with, negative column", () => test.`with`(0, -1, 2.0)),
        ("with, column past the end", () => test.`with`(0, 3, 2.0)))
      forAll(indexFailures) { (member: String, operation: () => Any) =>
        withClue(s"$member, ") {
          val failure = intercept[ArrayIndexOutOfBoundsException](operation())
          failure shouldBe an[IndexOutOfBoundsException]
        }
      }

      // A shape violation is checked by this library and raised as an illegal argument, carrying
      // the message of the Java original word for word wherever the original raised one. The two
      // `copyOf` rows are the exception on both counts: that factory shaped an array whose rows
      // differed in length by its first row and raised nothing, so the message is the port's own
      // and names the offending row and both lengths.
      val shapeFailures = Table[String, () => Any, String](
        ("member", "operation", "message"),
        (
          "of, too few values",
          () => DoubleMatrix.of(1, 2, 1.0),
          "Values array not of length rows * columns"),
        (
          "of, too many values",
          () => DoubleMatrix.of(1, 2, 1.0, 2.0, 3.0),
          "Values array not of length rows * columns"),
        (
          "of, values for an empty shape",
          () => DoubleMatrix.of(0, 0, 1.0),
          "Values array not of length rows * columns"),
        (
          "ofArrays, short row",
          () => DoubleMatrix.ofArrays(1, 2)(_ => Array(1.0)),
          "Function returned array of incorrect length 1, expected 2"),
        (
          "ofArrays, long row",
          () => DoubleMatrix.ofArrays(1, 2)(_ => Array(1.0, 2.0, 3.0)),
          "Function returned array of incorrect length 3, expected 2"),
        (
          "ofArrayObjects, short row",
          () => DoubleMatrix.ofArrayObjects(1, 2)(_ => DoubleArray.of(1.0)),
          "Function returned array of incorrect length 1, expected 2"),
        (
          "copyOf, short row",
          () => DoubleMatrix.copyOf(Array(Array(1.0, 2.0), Array(3.0))),
          "Expected every row of the matrix to hold 2 elements, but row 1 holds 1"),
        (
          "copyOf, long row",
          () => DoubleMatrix.copyOf(Array(Array(1.0), Array(2.0, 3.0))),
          "Expected every row of the matrix to hold 1 elements, but row 1 holds 2"),
        (
          "plus, different shape",
          () => test.plus(DoubleMatrix.EMPTY),
          "Arrays have different sizes"),
        (
          "minus, different shape",
          () => test.minus(DoubleMatrix.of(2, 3, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)),
          "Arrays have different sizes"),
        (
          "combine, different shape",
          () => test.combine(DoubleMatrix.EMPTY, (left, right) => left * right),
          "Arrays have different sizes"))
      forAll(shapeFailures) { (member: String, operation: () => Any, message: String) =>
        withClue(s"$member, ") {
          val failure = intercept[IllegalArgumentException](operation())
          failure.getMessage shouldBe message
        }
      }

      // A negative dimension is the third family, and every factory that takes one is held to it
      // here: each reports the caller with an illegal argument naming the argument that was wrong
      // and the value it was given, rather than letting the allocation it would have made raise a
      // negative-size error of the platform's. The two guards of a two-dimensional factory run in
      // declaration order, which the both-negative case pins by expecting the row count to be the
      // one reported, and the values differ from case to case so that the message is shown to
      // carry the argument it was given rather than a constant.
      val dimensionFailures = Table[String, () => Any, String, Int](
        ("member", "operation", "argument", "value"),
        ("of, negative row count", () => DoubleMatrix.of(-1, 2), "rows", -1),
        ("of, negative column count", () => DoubleMatrix.of(2, -1), "columns", -1),
        ("of, both counts negative", () => DoubleMatrix.of(-2, -3), "rows", -2),
        (
          "tabulate, negative row count",
          () => DoubleMatrix.tabulate(-1, 2)((_, _) => 0.0),
          "rows",
          -1),
        (
          "tabulate, negative column count",
          () => DoubleMatrix.tabulate(2, -3)((_, _) => 0.0),
          "columns",
          -3),
        (
          "ofArrays, negative row count",
          () => DoubleMatrix.ofArrays(-1, 2)(_ => Array(1.0, 2.0)),
          "rows",
          -1),
        (
          "ofArrays, negative column count",
          () => DoubleMatrix.ofArrays(2, -1)(_ => Array(1.0, 2.0)),
          "columns",
          -1),
        (
          "ofArrayObjects, negative row count",
          () => DoubleMatrix.ofArrayObjects(-3, 2)(_ => DoubleArray.of(1.0, 2.0)),
          "rows",
          -3),
        (
          "ofArrayObjects, negative column count",
          () => DoubleMatrix.ofArrayObjects(2, -2)(_ => DoubleArray.of(1.0, 2.0)),
          "columns",
          -2),
        ("filled, negative row count", () => DoubleMatrix.filled(-1, 2), "rows", -1),
        ("filled, negative column count", () => DoubleMatrix.filled(2, -1), "columns", -1),
        (
          "filled with a value, negative row count",
          () => DoubleMatrix.filled(-2, 2, 7.0),
          "rows",
          -2),
        (
          "filled with a value, negative column count",
          () => DoubleMatrix.filled(2, -2, 7.0),
          "columns",
          -2),
        ("identity, negative size", () => DoubleMatrix.identity(-1), "size", -1),
        (
          "identity, the most negative size",
          () => DoubleMatrix.identity(Int.MinValue),
          "size",
          Int.MinValue))
      forAll(dimensionFailures) {
        (member: String, operation: () => Any, argument: String, value: Int) =>
          withClue(s"$member, ") {
            assertNegativeDimension(operation(), argument, value)
          }
      }
    }

    test("negative_dimensions_are_rejected_before_any_allocation_or_callback") {
      // A dimension of zero and a negative dimension are different inputs and this type answers
      // them differently: zero is a shape with no elements, which every factory canonicalises to
      // the one empty instance, while a negative count is a broken caller. The two are asserted
      // side by side here, factory by factory, because the difference is easy to lose - a check
      // written as `rows <= 0` would answer a negative count with the empty matrix and no caller
      // would ever learn of the mistake.
      val contrasts = Table[String, () => DoubleMatrix, () => Any](
        ("factory", "zero shape", "negative shape"),
        ("of", () => DoubleMatrix.of(0, 2), () => DoubleMatrix.of(-1, 2)),
        (
          "tabulate",
          () => DoubleMatrix.tabulate(0, 2)((_, _) => 0.0),
          () => DoubleMatrix.tabulate(-1, 2)((_, _) => 0.0)),
        (
          "ofArrays",
          () => DoubleMatrix.ofArrays(2, 0)(_ => Array.empty[Double]),
          () => DoubleMatrix.ofArrays(2, -1)(_ => Array.empty[Double])),
        (
          "ofArrayObjects",
          () => DoubleMatrix.ofArrayObjects(2, 0)(_ => DoubleArray.EMPTY),
          () => DoubleMatrix.ofArrayObjects(2, -1)(_ => DoubleArray.EMPTY)),
        ("filled", () => DoubleMatrix.filled(0, 2), () => DoubleMatrix.filled(0, -1)),
        (
          "filled with a value",
          () => DoubleMatrix.filled(2, 0, 7.0),
          () => DoubleMatrix.filled(2, -1, 7.0)),
        ("identity", () => DoubleMatrix.identity(0), () => DoubleMatrix.identity(-1)))
      forAll(contrasts) {
        (factory: String, zeroShape: () => DoubleMatrix, negativeShape: () => Any) =>
          withClue(s"$factory, ") {
            zeroShape() should be theSameInstanceAs DoubleMatrix.EMPTY
            a[IllegalArgumentException] should be thrownBy negativeShape()
          }
      }

      // The two cases above that pair a zero count with a negative one - `filled(0, -1)` and
      // `ofArrays(2, -1)` - are the reason the guards run before the short-circuit rather than
      // after it: a factory that tested for an empty shape first would answer the first of them
      // with the empty matrix, and the negative column count would never be reported at all.
      assertNegativeDimension(DoubleMatrix.of(0, -1), "columns", -1)
      assertNegativeDimension(DoubleMatrix.filled(0, -1), "columns", -1)
      assertNegativeDimension(DoubleMatrix.filled(0, -1, 7.0), "columns", -1)
      assertNegativeDimension(DoubleMatrix.tabulate(0, -1)((_, _) => 0.0), "columns", -1)

      // The guards also run before the function a factory was given, so a negative dimension
      // costs nothing: no row array is allocated and no row is produced. A function that fails
      // the test if it is called is what shows it, and it is the same device the factory tests
      // above use for a shape with no elements.
      assertNegativeDimension(
        DoubleMatrix.tabulate(2, -1)((_, _) => fail("the function must not be invoked")),
        "columns",
        -1)
      assertNegativeDimension(
        DoubleMatrix.ofArrays(2, -1)(_ => fail("the function must not be invoked")),
        "columns",
        -1)
      assertNegativeDimension(
        DoubleMatrix.ofArrayObjects(2, -1)(_ => fail("the function must not be invoked")),
        "columns",
        -1)
      assertNegativeDimension(
        DoubleMatrix.ofArrays(-1, 2)(_ => fail("the function must not be invoked")),
        "rows",
        -1)
      assertNegativeDimension(
        DoubleMatrix.ofArrayObjects(-1, 2)(_ => fail("the function must not be invoked")),
        "rows",
        -1)

      // The count of elements a shape needs is computed without overflowing, which matters for
      // exactly the shapes whose product is a multiple of the integer range: computed as an
      // integer, `65536 * 65536` and `4 * 1073741824` are both zero, so a call supplying no
      // elements at all would have satisfied a count test written that way and gone on to
      // allocate a matrix of four thousand million elements. Both are reported as the count
      // violation they are, and neither allocates anything to find that out.
      val overflowed = Table[String, () => Any](
        ("shape", "operation"),
        ("two counts of 65536", () => DoubleMatrix.of(65536, 65536)),
        ("four rows of 2^30", () => DoubleMatrix.of(4, 1073741824)),
        ("2^30 rows of four", () => DoubleMatrix.of(1073741824, 4)))
      forAll(overflowed) { (shape: String, operation: () => Any) =>
        withClue(s"$shape, ") {
          val failure = intercept[IllegalArgumentException](operation())
          failure.getMessage shouldBe "Values array not of length rows * columns"
        }
      }

      // and the shapes either side of the integer range are still accepted when the elements
      // supplied do fill them, so the wider arithmetic rejects nothing it should not
      assertMatrix(DoubleMatrix.of(1, 1, 1.0), 1.0)
      assertMatrix(DoubleMatrix.of(2, 3, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    }

    test("elements_supplied_in_any_shape_fill_the_matrix_and_stay_the_caller's") {
      // The elements of a flat construction can reach the factory in more than one shape: written
      // out one by one, or expanded from a collection of the caller's with `: _*`. An array
      // expanded that way arrives as a sequence backed by that very array, while a list or a
      // vector arrives holding its elements individually, and the factory reads the two
      // differently - the first in place, the second through its iterator - so both are asserted
      // to fill the matrix identically rather than only the one the literal form takes.
      val expected = DoubleMatrix.of(2, 3, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
      val elements = List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
      val supplied = Table[String, DoubleMatrix](
        ("supplied as", "matrix"),
        ("an expanded list", DoubleMatrix.of(2, 3, elements: _*)),
        ("an expanded vector", DoubleMatrix.of(2, 3, elements.toVector: _*)),
        ("an expanded lazy list", DoubleMatrix.of(2, 3, LazyList.from(elements): _*)),
        (
          "an expanded wrapped array",
          DoubleMatrix.of(
            2,
            3,
            scala.collection.immutable.ArraySeq.unsafeWrapArray(elements.toArray): _*)))
      forAll(supplied) { (shape: String, matrix: DoubleMatrix) =>
        withClue(s"$shape, ") {
          assertMatrix(matrix, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
          matrix shouldBe expected
          matrix.hashCode shouldBe expected.hashCode
        }
      }

      // And the values a caller expands are still the caller's: the elements reach no array but
      // the rows of the result, so writing to the source afterwards changes nothing. The case
      // worth holding on to is the last of the four above, where the sequence handed over is
      // backed by an array the caller still holds - expanding the array itself would have the
      // language copy it defensively first, so wrapping it is what puts the factory face to face
      // with a caller-owned array and shows that it keeps none of it.
      val owned = Array(1.0, 2.0, 3.0, 4.0)
      val built = DoubleMatrix.of(2, 2, scala.collection.immutable.ArraySeq.unsafeWrapArray(owned): _*)
      owned(0) = 99.0
      owned(3) = 99.0
      assertMatrix(built, 1.0, 2.0, 3.0, 4.0)

      // the elements of an expanded sequence are read in row-major order, which is the order the
      // written-out form is read in, so a sequence whose elements are all distinct lands the same
      // way round either way
      DoubleMatrix.of(3, 2, List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0): _*) shouldBe matrix3x2
      DoubleMatrix.of(1, 4, List(1.0, 2.0, 3.0, 4.0): _*).row(0) shouldBe
        DoubleArray.of(1.0, 2.0, 3.0, 4.0)
      DoubleMatrix.of(4, 1, List(1.0, 2.0, 3.0, 4.0): _*).column(0) shouldBe
        DoubleArray.of(1.0, 2.0, 3.0, 4.0)

      // a count that does not fill the shape is reported whichever way the elements arrive, and
      // an expanded empty sequence is the empty matrix for a shape that needs no elements
      val failure = intercept[IllegalArgumentException](
        DoubleMatrix.of(2, 2, List(1.0, 2.0, 3.0): _*))
      failure.getMessage shouldBe "Values array not of length rows * columns"
      assertMatrix(DoubleMatrix.of(0, 2, List.empty[Double]: _*))
    }

    test("the_empty_matrix_answers_every_column_index_and_traverses_nothing") {
      // The empty matrix has no row to read a column out of, so it answers every column index -
      // including one that no matrix could hold - with an array of no elements rather than
      // failing, which is the behaviour of the Java original and is surprising next to the
      // failure a non-empty matrix produces for the same index. `test_column` pins it for the
      // accessor that wraps the result; this pins it for the one that hands back the array, and
      // for indices either side of the range as well as inside it.
      val indices = Table[Int]("column", Int.MinValue, -2, -1, 0, 1, 4, Int.MaxValue)
      forAll(indices) { (column: Int) =>
        withClue(s"column $column, ") {
          DoubleMatrix.EMPTY.columnArray(column).length shouldBe 0
          DoubleMatrix.EMPTY.column(column) should be theSameInstanceAs DoubleArray.EMPTY
        }
      }

      // Each call allocates the array it answers with, which is what makes the result the
      // caller's to keep: the accessor documents a freshly allocated array of the row count, and
      // two calls are therefore two arrays rather than one shared instance handed out twice.
      val first = DoubleMatrix.EMPTY.columnArray(0)
      val second = DoubleMatrix.EMPTY.columnArray(0)
      (first eq second) shouldBe false
      // the same holds of a matrix that has elements, where it is the property that keeps the
      // stored rows unreachable rather than merely a fresh allocation
      val test = matrix3x2
      (test.columnArray(0) eq test.columnArray(0)) shouldBe false

      // Nothing is traversed either: the action a traversal is given is never applied, because
      // there is no element to apply it to. A matrix with elements applies it once per element,
      // which `test_forEach` asserts, so the two together fix both ends of the loop.
      DoubleMatrix.EMPTY.forEach((row, column, value) =>
        fail(s"the action must not be invoked, but saw row $row, column $column, value $value"))
      DoubleMatrix.EMPTY.total shouldBe 0.0
      DoubleMatrix.EMPTY.reduce(2.0, (_, _) => fail("the operator must not be invoked")) shouldBe 2.0
      DoubleMatrix.EMPTY.toString shouldBe ""
    }

    //-------------------------------------------------------------------------
    // The properties below are drawn from the generators of this module's shared generator
    // object, which is a cross-module contract: no generator of matrices is declared here, so a
    // property here and a property in the dependent module see the same values. The generator that
    // mixes in the IEEE-754 edge values is named wherever the property is about equality, hashing
    // or rendering, all of which those values decide; the finite generator is named wherever the
    // property is about what arithmetic computes, and the name of the test says so.
    //-------------------------------------------------------------------------
    test("property_copyOf_of_toArray_round_trips") {
      forAll(Arbitraries.genDoubleMatrix) { (matrix: DoubleMatrix) =>
        DoubleMatrix.copyOf(matrix.toArray) shouldBe matrix
        matrixHash.eqv(DoubleMatrix.copyOf(matrix.toArray), matrix) shouldBe true
        DoubleMatrix.copyOf(matrix.toArray).hashCode shouldBe matrix.hashCode
        DoubleMatrix.copyOf(matrix.toArray).rowCount shouldBe matrix.rowCount
        DoubleMatrix.copyOf(matrix.toArray).columnCount shouldBe matrix.columnCount
      }
    }

    test("property_toArray_is_a_fresh_deep_copy") {
      forAll(genNonEmptyMatrix) { (matrix: DoubleMatrix) =>
        val first = matrix.toArray
        val second = matrix.toArray
        (first eq second) shouldBe false
        (first eq matrix.toArray) shouldBe false
        (first(0) eq second(0)) shouldBe false
        (first(0) eq matrix.toArray(0)) shouldBe false
        first(0)(0) = 123456.5
        first(matrix.rowCount - 1) = new Array[Double](matrix.columnCount)
        matrixHash.eqv(DoubleMatrix.copyOf(second), matrix) shouldBe true
      }
    }

    test("property_transpose_is_its_own_inverse") {
      forAll(Arbitraries.genDoubleMatrix) { (matrix: DoubleMatrix) =>
        matrix.transpose.rowCount shouldBe matrix.columnCount
        matrix.transpose.columnCount shouldBe matrix.rowCount
        matrix.transpose.size shouldBe matrix.size
        matrixHash.eqv(matrix.transpose.transpose, matrix) shouldBe true
        elementBitsOf(matrix.transpose) shouldBe
          positionsOf(matrix.transpose).map {
            case (row, column) => bitsOf(matrix.get(column, row))
          }
      }
    }

    test("property_get_agrees_with_row_and_column") {
      forAll(Arbitraries.genDoubleMatrix) { (matrix: DoubleMatrix) =>
        val positions = positionsOf(matrix)
        positions.map { case (row, column) => bitsOf(matrix.row(row).get(column)) } shouldBe
          elementBitsOf(matrix)
        positions.map { case (row, column) => bitsOf(matrix.column(column).get(row)) } shouldBe
          elementBitsOf(matrix)
      }
    }

    test("property_rowArray_and_columnArray_agree_with_the_array_accessors") {
      forAll(Arbitraries.genDoubleMatrix) { (matrix: DoubleMatrix) =>
        val rowMismatches = (0 until matrix.rowCount)
          .filterNot(row => Arrays.equals(matrix.rowArray(row), matrix.row(row).toArray))
        rowMismatches shouldBe empty

        val columnMismatches = (0 until matrix.columnCount)
          .filterNot(column =>
            Arrays.equals(matrix.columnArray(column), matrix.column(column).toArray))
        columnMismatches shouldBe empty

        (0 until matrix.rowCount)
          .map(row => matrix.rowArray(row).length)
          .distinct shouldBe (0 until matrix.rowCount).map(_ => matrix.columnCount).distinct
      }
    }

    test("property_total_agrees_with_reduce_for_finite_values") {
      forAll(Arbitraries.genFiniteDoubleMatrix) { (matrix: DoubleMatrix) =>
        // the total accumulates sequentially from zero in row-major order, which is a reduction
        // with that identity and that operator, so the two agree exactly rather than approximately
        matrix.total shouldBe matrix.reduce(0.0, (total, value) => total + value)
        matrix.total shouldBe
          positionsOf(matrix)
            .map { case (row, column) => matrix.get(row, column) }
            .foldLeft(0.0)((total, value) => total + value)
      }
    }

    test("property_with_changes_one_element_only") {
      forAll(genNonEmptyMatrix, Gen.choose(0, 1000), Gen.choose(0, 1000), Arbitraries.genDouble) {
        (matrix: DoubleMatrix, rowOffset: Int, columnOffset: Int, value: Double) =>
          val row = rowOffset % matrix.rowCount
          val column = columnOffset % matrix.columnCount
          val updated = matrix.`with`(row, column, value)
          updated.rowCount shouldBe matrix.rowCount
          updated.columnCount shouldBe matrix.columnCount
          bitsOf(updated.get(row, column)) shouldBe bitsOf(value)
          val others = positionsOf(matrix).filterNot(position => position == ((row, column)))
          others.map { case (r, c) => bitsOf(updated.get(r, c)) } shouldBe
            others.map { case (r, c) => bitsOf(matrix.get(r, c)) }
      }
    }

    test("property_map_of_the_identity_and_multipliedBy_one_return_an_equal_matrix") {
      forAll(Arbitraries.genDoubleMatrix) { (matrix: DoubleMatrix) =>
        matrixHash.eqv(matrix.map(value => value), matrix) shouldBe true
        matrixHash.eqv(matrix.mapWithIndex((_, _, value) => value), matrix) shouldBe true
        matrixHash.eqv(matrix.multipliedBy(1.0), matrix) shouldBe true
        matrixHash.eqv(matrix.combine(matrix, (left, _) => left), matrix) shouldBe true
      }
    }

    test("property_plus_then_minus_returns_the_original_for_finite_values") {
      forAll(Arbitraries.genFiniteDoubleMatrixPair) {
        case (matrix, other) =>
          val roundTripped = matrix.plus(other).minus(other)
          roundTripped.rowCount shouldBe matrix.rowCount
          roundTripped.columnCount shouldBe matrix.columnCount
          // The tolerance is relative to the magnitudes involved rather than absolute: adding a
          // value of one magnitude to a value of another and taking it away again loses digits in
          // proportion to the larger of the two, so an absolute bound alone would be a claim about
          // doubles that is not true of them. Scaling by the larger magnitude leaves the bound
          // between four and seven orders of magnitude clear of the error these ranges can
          // produce, so a genuine defect still fails it. This is the bound the array spec of this
          // module uses for the same round trip.
          val offenders = positionsOf(matrix).filterNot {
            case (row, column) =>
              val original = matrix.get(row, column)
              val added = other.get(row, column)
              val magnitude = math.max(1.0, math.max(math.abs(original), math.abs(added)))
              math.abs(roundTripped.get(row, column) - original) <= 1e-9 * magnitude
          }
          offenders shouldBe empty
      }
    }

    test("property_size_and_isSquare_agree_with_the_shape") {
      forAll(Arbitraries.genDoubleMatrix) { (matrix: DoubleMatrix) =>
        matrix.size shouldBe matrix.rowCount * matrix.columnCount
        matrix.isSquare shouldBe (matrix.rowCount == matrix.columnCount)
        matrix.isEmpty shouldBe (matrix.size == 0)
        matrix.dimensions shouldBe 2
        positionsOf(matrix).size shouldBe matrix.size
      }
    }

    test("property_identity_agrees_with_a_diagonal_of_ones") {
      forAll(Gen.choose(0, 8)) { (size: Int) =>
        matrixHash.eqv(
          DoubleMatrix.identity(size),
          DoubleMatrix.diagonal(DoubleArray.filled(size, 1.0))) shouldBe true
        DoubleMatrix.identity(size).isSquare shouldBe true
        DoubleMatrix.identity(size).size shouldBe size * size
      }
    }

    test("property_hashCode_agrees_with_the_row_folding_oracle") {
      forAll(Arbitraries.genDoubleMatrix) { (matrix: DoubleMatrix) =>
        matrix.hashCode shouldBe hashOracle(matrix)
        matrixHash.hash(matrix) shouldBe hashOracle(matrix)
      }
    }

    test("property_toString_agrees_with_the_row_joining_layout") {
      forAll(Arbitraries.genDoubleMatrix) { (matrix: DoubleMatrix) =>
        matrix.toString shouldBe textOracle(matrix)
        implicitly[Show[DoubleMatrix]].show(matrix) shouldBe matrix.toString
        matrix.toString.count(character => character == '\n') shouldBe matrix.rowCount
      }
    }

    test("property_equal_values_from_different_factories_agree") {
      forAll(Arbitraries.genDoubleMatrix) { (matrix: DoubleMatrix) =>
        val elements = positionsOf(matrix).map { case (row, column) => matrix.get(row, column) }
        val candidates = List(
          DoubleMatrix.copyOf(matrix.toArray),
          DoubleMatrix.of(matrix.rowCount, matrix.columnCount, elements: _*),
          DoubleMatrix.tabulate(matrix.rowCount, matrix.columnCount)((row, column) =>
            matrix.get(row, column)),
          DoubleMatrix.ofArrays(matrix.rowCount, matrix.columnCount)(row => matrix.rowArray(row)),
          DoubleMatrix.ofArrayObjects(matrix.rowCount, matrix.columnCount)(row => matrix.row(row)),
          matrix.transpose.transpose,
          matrix.multipliedBy(1.0))
        candidates.map(candidate => matrixHash.eqv(candidate, matrix)) shouldBe
          candidates.map(_ => true)
        candidates.map(_.hashCode).distinct shouldBe List(matrix.hashCode)
        candidates.map(_.toString).distinct shouldBe List(matrix.toString)
      }
    }

    //-------------------------------------------------------------------------
    // Properties at a shape the shared generators never reach. Every property above is written
    // over matrices of at most five rows and five columns, which is the right default - a
    // failing case shrinks to something readable - but a five-by-five matrix is small enough
    // that a row walk and a column walk of the same length cannot be told apart, and its
    // twenty-five elements never put a nested loop under strain. These two name the large
    // generators explicitly, so the same claims are made again over thousands of elements in
    // shapes that are mostly not square.

    test("property_large_matrices_satisfy_the_element_wise_and_shape_claims") {
      forAll(Arbitraries.genLargeFiniteDoubleMatrixPair) {
        case (a: DoubleMatrix, b: DoubleMatrix) =>
          a.rowCount should be >= 8
          a.rowCount shouldBe b.rowCount
          a.columnCount shouldBe b.columnCount
          a.size shouldBe a.rowCount * a.columnCount

          // the element-wise operations compute the operator at every position of the shape,
          // which at a non-square shape also establishes that rows and columns are not confused
          val positions = positionsOf(a)
          positions.size shouldBe a.size
          val sum = a.plus(b)
          val difference = a.minus(b)
          val scaled = a.multipliedBy(2.5)
          val mapped = a.map(value => value * 3.0)
          val combined = a.combine(b, (left, right) => left + right)
          val indexed = a.mapWithIndex((row, column, value) => row * 1000.0 + column + value)
          List(sum, difference, scaled, mapped, combined, indexed).foreach { result =>
            result.rowCount shouldBe a.rowCount
            result.columnCount shouldBe a.columnCount
          }
          val offenders = positions.filterNot { case (row, column) =>
            val left = a.get(row, column)
            val right = b.get(row, column)
            sum.get(row, column) == left + right &&
              difference.get(row, column) == left - right &&
              scaled.get(row, column) == left * 2.5 &&
              mapped.get(row, column) == left * 3.0 &&
              combined.get(row, column) == left + right &&
              indexed.get(row, column) == row * 1000.0 + column + left
          }
          offenders shouldBe empty

          // transposition exchanges the two dimensions, moves every element to the mirrored
          // position and is its own inverse at this size as at any other
          val transposed = a.transpose
          transposed.rowCount shouldBe a.columnCount
          transposed.columnCount shouldBe a.rowCount
          positions.filterNot { case (row, column) =>
            transposed.get(column, row) == a.get(row, column)
          } shouldBe empty
          matrixHash.eqv(transposed.transpose, a) shouldBe true

          // the total is the running sum over every element in row-major order
          a.total shouldBe positions.foldLeft(0.0) { case (running, (row, column)) =>
            running + a.get(row, column)
          }

          // and every element survives a round trip through the nested primitive array, through
          // row and column extraction, and through the copying factory
          val roundTripped = DoubleMatrix.copyOf(a.toArray)
          matrixHash.eqv(roundTripped, a) shouldBe true
          roundTripped.hashCode shouldBe hashOracle(a)
          roundTripped.toString shouldBe textOracle(a)
          (0 until a.rowCount).filterNot(row =>
            a.row(row).toList == List.tabulate(a.columnCount)(column => a.get(row, column))
          ) shouldBe empty
          (0 until a.columnCount).filterNot(column =>
            a.column(column).toList == List.tabulate(a.rowCount)(row => a.get(row, column))
          ) shouldBe empty
      }
    }

    test("property_large_square_matrices_multiply_and_transpose_as_the_small_ones_do") {
      forAll(Arbitraries.genLargeSquareFiniteDoubleMatrix) { (matrix: DoubleMatrix) =>
        matrix.rowCount should be >= 8
        matrix.isSquare shouldBe true

        // scaling by one and by minus one twice leave the value alone exactly, since neither
        // multiplication loses a bit, and both walk every element of a matrix of thousands
        matrixHash.eqv(matrix.multipliedBy(1.0), matrix) shouldBe true
        matrixHash.eqv(matrix.multipliedBy(-1.0).multipliedBy(-1.0), matrix) shouldBe true

        // the identity of this side agrees with a diagonal of ones, and the diagonal of the
        // matrix is read from the positions where the two indices agree
        val identity = DoubleMatrix.identity(matrix.rowCount)
        matrixHash.eqv(identity, DoubleMatrix.diagonal(DoubleArray.filled(matrix.rowCount, 1.0))) shouldBe true
        identity.size shouldBe matrix.rowCount * matrix.rowCount

        // transposition is its own inverse, and a square transposition keeps the shape, so the
        // mirrored read is over the same index range in both directions
        val transposed = matrix.transpose
        transposed.rowCount shouldBe matrix.rowCount
        transposed.columnCount shouldBe matrix.columnCount
        matrixHash.eqv(transposed.transpose, matrix) shouldBe true
        positionsOf(matrix).filterNot { case (row, column) =>
          transposed.get(column, row) == matrix.get(row, column)
        } shouldBe empty
      }
    }

    //-------------------------------------------------------------------------
    // The shared shrinkings of this type, and the shape they used to have to allow for.
    //
    // `Arbitraries` builds its shrinking candidates by walking the stated shape of the value it
    // is minimising, and it guards that walk with a rectangularity check, because `copyOf` once
    // shaped an array whose rows differed in length by its first row - so a matrix could state a
    // column count one of its rows did not reach, and reading such a position would have raised
    // from the minimisation rather than reporting the counterexample that was found.
    //
    // No value of this type can be shaped that way any more: the constructor measures the rows
    // against the shape, so the guard is now satisfied by construction. That is what the first
    // test below asserts - the shape those candidates had to allow for cannot be built at all -
    // and the two after it assert that the shrinkings themselves still produce candidates for the
    // shapes that do exist, which is what the guard must not have cost.
    //-------------------------------------------------------------------------
    test("the shape the shared shrinkings guard against can no longer be built") {
      // the two orientations the guard was written for, refused at the factory that used to
      // produce them, so the walk the shrinkings make cannot meet a row it would read past
      intercept[IllegalArgumentException](DoubleMatrix.copyOf(Array(Array(1.5, 2.5), Array(3.5))))
      intercept[IllegalArgumentException](DoubleMatrix.copyOf(Array(Array(1.5), Array(2.5, 3.5))))

      // and every value that can be built agrees with its own shape, which is the condition the
      // guard tests for, read here the way the guard reads it - through `row`, at its own length
      val values =
        List(
          DoubleMatrix.copyOf(Array(Array(1.5, 2.5), Array(3.5, 4.5))),
          DoubleMatrix.of(2, 3, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0),
          DoubleMatrix.tabulate(3, 1)((row, _) => row.toDouble),
          DoubleMatrix.EMPTY)
      values.foreach { value =>
        withClue(s"the rows of a ${value.rowCount} x ${value.columnCount} value: ") {
          (0 until value.rowCount).filterNot(row => value.row(row).size == value.columnCount) shouldBe
            empty
        }
      }
    }

    test("the shared matrix shrinking produces candidates for every shape that exists") {
      // a rectangular matrix shrinks, which is what the guard must not have cost, and the
      // candidates it offers are rectangular in their turn
      val rectangular = DoubleMatrix.copyOf(Array(Array(1.5, 2.5), Array(3.5, 4.5)))
      val candidates = Shrink.shrink(rectangular)(Arbitraries.shrinkDoubleMatrix).toList
      candidates should not be empty
      candidates.filterNot(candidate =>
        (0 until candidate.rowCount).forall(row => candidate.row(row).size == candidate.columnCount)
      ) shouldBe empty

      // and the empty matrix, which has nothing to reduce, is the floor it already is
      Shrink.shrink(DoubleMatrix.EMPTY)(Arbitraries.shrinkDoubleMatrix).toList shouldBe Nil
    }

    test("the shared matrix-pair shrinking reduces both sides in lockstep") {
      val rectangular = DoubleMatrix.copyOf(Array(Array(1.5, 2.5), Array(3.5, 4.5)))
      val candidates = Shrink.shrink((rectangular, rectangular))(Arbitraries.shrinkDoubleMatrixPair).toList
      candidates should not be empty
      candidates.filterNot { case (left, right) =>
        left.rowCount == right.rowCount && left.columnCount == right.columnCount
      } shouldBe empty

      // a pair with nothing to reduce is the floor it already is, which is the other branch of
      // that shrinking
      Shrink.shrink((DoubleMatrix.EMPTY, DoubleMatrix.EMPTY))(
        Arbitraries.shrinkDoubleMatrixPair).toList shouldBe Nil
    }

  }
}

package com.opengamma.strata.audit {

  /**
   * The proof that the two aliasing members of the Java matrix type are absent from the port, put
   * from outside the `strata-collect` module.
   *
   * Those members - the factory that adopted an array of rows without copying it, and the accessor
   * that handed back the rows a value held - are not ported under any name or any visibility. The
   * spec of the matrix type asserts that from inside `com.opengamma.strata.collect`, which is the
   * sharper place to assert it from, since a member merely restricted to that module would still
   * be visible there. This object asserts it from a place no such restriction could ever have
   * reached, so that the two answers together say the names resolve to nothing wherever the
   * question is asked - and it is the place the assertion would have to be made from if either
   * member were ever reintroduced behind a module scope.
   *
   * This object is a sibling of `com.opengamma.strata` at the root rather than a member of
   * `collect`, its package name says what it is for, and the compile-time check that rejects each
   * expression below is answered in this package because that is where the code asking the
   * question sits. Each reference is written out in full, so that nothing an import brought into
   * the spec can change the outcome, and each method answers with the assertion it made so that
   * the spec can state it as its own result. The array type of this module has a probe of its own
   * in this package, which is why this one is named for the type it serves.
   *
   * The corresponding positive controls - the copying members of the same type, in the same
   * expression shape, compiling from here - are the first line of each method, so a rejection
   * below cannot be the reward for an expression that was malformed.
   */
  object DoubleMatrixUnsafeAccessProbe extends org.scalatest.Assertions {

    /**
     * Asserts that the name of the original's adopting factory resolves to nothing here.
     *
     * @return the assertion that the call does not compile here
     */
    def ofUnsafeIsInaccessible: org.scalatest.Assertion = {
      assertCompiles("com.opengamma.strata.collect.array.DoubleMatrix.copyOf(Array(Array(1.0)))")
      assertDoesNotCompile(
        "com.opengamma.strata.collect.array.DoubleMatrix.ofUnsafe(Array(Array(1.0)))")
    }

    /**
     * Asserts that the name of the original's aliasing accessor resolves to nothing here.
     *
     * @return the assertion that the call does not compile here
     */
    def toArrayUnsafeIsInaccessible: org.scalatest.Assertion = {
      assertCompiles("com.opengamma.strata.collect.array.DoubleMatrix.of(1, 1, 1.0).toArray")
      assertDoesNotCompile(
        "com.opengamma.strata.collect.array.DoubleMatrix.of(1, 1, 1.0).toArrayUnsafe")
    }
  }
}
