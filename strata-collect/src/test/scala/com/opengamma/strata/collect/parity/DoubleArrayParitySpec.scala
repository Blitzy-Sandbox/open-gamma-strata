/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.parity

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all._

import io.circe.Decoder
import io.circe.Json
import io.circe.generic.semiauto.deriveDecoder
import io.circe.parser

import org.scalatest.Assertion
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.array.DoubleMatrix
import com.opengamma.strata.collect.io.Resources
import com.opengamma.strata.collect.json.Codecs

/**
 * Numerical parity of `DoubleArray` and `DoubleMatrix` against the Java baseline.
 *
 * This is the whole of this module's numerical-parity obligation. The two array types own that
 * obligation for the port, and this spec replays every captured operation through the Scala port,
 * measures the difference, publishes a report of the measurement, and only then asserts that
 * nothing differed.
 *
 * The gate that consumes it runs
 *
 * {{{
 * sbt -batch "testOnly *ParitySpec"
 * }}}
 *
 * and then reads `<parity.report.dir>/double-array.json`, requiring `failed == 0`. Both the
 * `*ParitySpec` suffix of this class and its package are therefore part of that contract, as are
 * the five keys of the report document and the name of the file it is written to.
 *
 * ===Where the expectations come from===
 *
 * The values are not re-derived here by hand: they were captured from the *Java* implementation by
 * `tools/parity-capture/capture-baseline.jsh` and are committed as
 * `strata-collect/src/test/resources/parity/double-array-baseline.json`. What the capture proves
 * about them differs by the kind of row, and the row's `id` says which kind it is:
 *
 *   - a `javatest-` row is built from inputs `DoubleArrayTest` or `DoubleMatrixTest` uses, and the
 *     capture compares the expectations it emits - the objects it serializes, not a recomputation
 *     of them - against the literals those tests assert for those inputs, arrays at that test's
 *     own tolerance of `1e-14` and scalars exactly. A disagreement aborts the capture before
 *     anything is written.
 *   - a `random-seeded-` or `ieee-` row has no Java literal to be compared against. The capture
 *     counts it as capture-only in its own summary, and what pins it is that the Java
 *     implementation produced it from recorded inputs.
 *
 * ===The fixture is the authority===
 *
 * The fixture is generated, never hand-authored, and this spec never edits it, never "corrects" an
 * expectation, never loosens the tolerance and never skips a row. If a row disagrees with the
 * port, the port is wrong and the disagreement is reported: that is the gate doing its job.
 *
 * ===Row schema===
 *
 * The document is a JSON array of '''uniform, fully populated''' row objects: every field is
 * present in every row, so the row model below has no optional field and a fixture missing one
 * fails the decode outright rather than being evaluated with a substituted default. The schema of
 * record is section 6 of `tools/parity-capture/README.md`; strict JSON admits no comments, so it
 * is restated here.
 *
 * The inputs are `id`, the two equal-length, non-empty arrays `a` and `b`, the `scalar` operand,
 * the half-open slice bounds `subArrayFrom` and `subArrayTo`, the two identically shaped non-empty
 * matrices `matrixA` and `matrixB`, and the single-element replacement `withRow`, `withColumn` and
 * `withValue`.
 *
 * The twenty-two expectations are, in the order they are measured:
 *
 *   - `plusScalar`, `plusArray`, `minusScalar`, `minusArray`, `multipliedByScalar`,
 *     `multipliedByArray`, `dividedByScalar`, `dividedByArray` - '''both''' overload families of
 *     the four arithmetic operations. `DoubleArray` has a scalar and an element-wise overload of
 *     each, so measuring one of the two would leave four retained public methods unmeasured and
 *     one of the row's two declared operands unused.
 *   - `mapSquared` (`x => x * x`), `reduceSum` (addition from an identity of zero), `sum`, `min`,
 *     `max`, `sorted`, `concat`, `subArray` - the two-argument, half-open slice.
 *   - `matrixMultipliedBy`, `matrixPlus`, `matrixMinus`, `matrixTranspose`, `matrixTotal`,
 *     `matrixWith`.
 *
 * `matrixMultipliedBy` is a '''scalar''' multiply: `DoubleMatrix` exposes no matrix product, and
 * none is invented here. `matrixB` is consumed only by `matrixPlus` and `matrixMinus`, the two
 * members that take a second matrix. `matrixTranspose` changes the shape of a non-square matrix,
 * so it is compared against the expectation's dimensions and never against the input's.
 *
 * Every row is a success row: the fixture has no `error` column, because the exception paths of
 * the two types - the empty-array `min` and `max`, the mismatched element-wise lengths, the
 * out-of-range indices - are unit behaviour owned by `array.DoubleArraySpec` and
 * `array.DoubleMatrixSpec`. A row that made the port throw is therefore a parity failure and is
 * reported as one.
 *
 * ===Why the expectations are decoded into plain collections===
 *
 * An expectation arrives as a `Double`, a `Vector[Double]` or a `Vector[Vector[Double]]`, and
 * never as a `DoubleArray` or a `DoubleMatrix`. Decoding them through the codecs of the types
 * under measurement would route this gate's own inputs through those very types, so a defect in a
 * codec could mask a real parity failure or manufacture a false one. The codecs are tested
 * elsewhere; here they are deliberately kept off the path, and the only piece of
 * [[com.opengamma.strata.collect.json.Codecs]] used is `taggedDouble`, which is the port's single
 * policy for a value JSON has no number syntax for and is what lets `"NaN"`, `"Infinity"` and
 * `"-Infinity"` appear in an input or in an expectation.
 *
 * Actual values, correspondingly, are built only through the copy-safe public factories. The two
 * types also carry module-private factories and accessors that hand out their backing storage
 * without copying it, and those are reachable from this package; none of them is used here,
 * because a harness that aliased its own inputs would be measuring something other than the
 * public API.
 *
 * ===The tolerance rule===
 *
 * A comparison passes only when '''both''' bounds hold:
 *
 * {{{
 * |actual - expected| <= 1e-9
 * |actual - expected| <= 1e-9 * max(|actual|, |expected|, 1e-300)
 * }}}
 *
 * Neither alone is sufficient. A large magnitude would let the relative bound admit a difference
 * of a millidegree; a near-zero magnitude would let the absolute bound admit a difference of fifty
 * percent. The floor of `1e-300` keeps the relative bound meaningful as the values approach zero.
 * The tolerance of the Java test the fixture was cross-checked against, `1e-14`, belongs to the
 * capture step and is not used here.
 *
 * Values outside the finite range are classified '''before''' any difference is computed, because
 * `NaN <= x` is false and the fixture carries not-a-number expectations by design: computed
 * naively, every one of them would be reported as a failure. Two not-a-numbers are equal for the
 * purpose of parity, each infinity equals itself, and anything else involving a value outside the
 * finite range differs.
 *
 * ===Self-containment===
 *
 * The parity harness of the dependent module cannot be reused: the build's dependency edge runs
 * from `strata-basics` to `strata-collect` and never the other way. The tolerance rule, the report
 * writer and the one lift from `Either` into `IO` are consequently defined here, and are private
 * to this file.
 *
 * ===Scope===
 *
 * This spec owns numerical parity and nothing else. The unit behaviour of the two types - index
 * and dimension failures, aliasing, equality, hashing, string form - belongs to the specs beside
 * them in the `array` package, and is not repeated here.
 */
class DoubleArrayParitySpec extends AsyncFunSuite with AsyncIOSpec with Matchers with TableDrivenPropertyChecks {

  import DoubleArrayParitySpec._

  /*
   * The measurement is declared first, deliberately: the report it publishes is the artifact the
   * gate collects, so it is written on every run of this suite rather than only on the runs where
   * the guards below happen to pass first.
   */
  test("DoubleArray and DoubleMatrix reproduce the Java baseline within 1e-9 absolute and relative") {
    for {
      rows <- loadFixture
      tally <- IO(evaluate(rows.flatMap(prepare)))
      report <- writeReport(tally, rows.size)
    } yield {
      withClue(
        s"parity report written to $report; " +
          s"${tally.passed} checks passed, ${tally.failed} failed over ${rows.size} fixture rows" +
          tally.firstFailureSummary) {
        tally.failed shouldBe 0
      }
    }
  }

  test("the fixture carries the population the baseline is required to measure") {
    loadFixture.map { rows =>
      // The floors are part of the contract, not a preference: a thinned fixture measures less
      // than the baseline is required to measure, and a gate that reads `failed == 0` cannot tell
      // the difference. Asserting them here is what makes a reduction fail rather than pass
      // quietly. The capture asserts the same floors on its own side.
      withClue(s"fixture rows: ${rows.size}; ids: ${rows.map(_.id).mkString(", ")}: ") {
        rows.size should be >= MinimumRows
        rows.map(_.id).distinct should have size rows.size.toLong
        rows.count(row => row.id.startsWith(JavaDerivedPrefix)) should be >= MinimumJavaDerivedRows
        rows.count(row => row.id.startsWith(SeededRandomPrefix)) should be >= MinimumSeededRandomRows
        rows.count(row => row.id.startsWith(IeeeEdgePrefix)) should be >= MinimumIeeeEdgeRows
        // Every row belongs to one of the three populations, so a row of unstated provenance is
        // reported rather than counted towards a floor it does not meet.
        rows.filterNot(row => KnownPrefixes.exists(prefix => row.id.startsWith(prefix))) shouldBe empty
      }
      succeed
    }
  }

  test("every fixture row is fully populated and measures all twenty-two expectations") {
    loadFixture.map { rows =>
      rows.foreach { row =>
        withClue(s"fixture row '${row.id}': ") {
          // The two arrays are operands of one another, so a length difference would make the
          // element-wise operations and `concat` measure something the capture never computed,
          // and an empty `a` has no minimum or maximum at all.
          row.a should not be empty
          row.a should have size row.b.size.toLong
          rectangular(row.matrixA)
          rectangular(row.matrixB)
          sameShape(row.matrixA, row.matrixB)
          row.subArrayFrom should be >= 0
          row.subArrayTo should be >= row.subArrayFrom
          row.subArrayTo should be <= row.a.size
          row.withRow should (be >= 0 and be < row.matrixA.size)
          row.withColumn should (be >= 0 and be < row.matrixA.head.size)
          // Each expectation must have the shape its operation produces, so a fixture whose rows
          // were reshaped is reported here rather than measured element by element against the
          // wrong quantity.
          row.elementWiseExpectations.foreach { case (name, values) =>
            withClue(s"expectation '$name': ") {
              values should have size row.a.size.toLong
            }
          }
          row.concat should have size (2L * row.a.size)
          row.subArray should have size (row.subArrayTo - row.subArrayFrom).toLong
          row.sameShapeMatrixExpectations.foreach { case (name, matrix) =>
            withClue(s"expectation '$name': ") {
              sameShape(row.matrixA, matrix)
            }
          }
          row.matrixTranspose should have size row.matrixA.head.size.toLong
          row.matrixTranspose.head should have size row.matrixA.size.toLong
          // And the row must contribute every expectation to the measurement: this is the count
          // Gate 3 ultimately reports, so an expectation dropped from the model would reduce it
          // silently.
          prepare(row).map(prepared => prepared.expectation) shouldBe ExpectationNames
        }
      }
      succeed
    }
  }

  test("the parity tolerance rule applies both bounds and treats matching non-finite values as equal") {
    val comparisons = Table(
      ("case", "actual", "expected", "matches"),
      ("identical finite values", 1.25d, 1.25d, true),
      ("a difference inside both bounds", 1d, 1d + 1e-12d, true),
      ("positive and negative zero", 0d, -0d, true),
      ("a large magnitude failing only the absolute bound", 1e12d, 1e12d + 1e-3d, false),
      ("a near-zero magnitude failing only the relative bound", 1e-20d, 2e-20d, false),
      ("a difference beyond the absolute bound", 1d, 1.5d, false),
      ("not-a-number against itself", Double.NaN, Double.NaN, true),
      ("positive infinity against itself", Double.PositiveInfinity, Double.PositiveInfinity, true),
      ("negative infinity against itself", Double.NegativeInfinity, Double.NegativeInfinity, true),
      ("not-a-number against zero", Double.NaN, 0d, false),
      ("a finite value against not-a-number", 1d, Double.NaN, false),
      ("the two infinities", Double.PositiveInfinity, Double.NegativeInfinity, false),
      ("positive infinity against a finite value", Double.PositiveInfinity, 1d, false)
    )
    IO {
      forAll(comparisons) { (label: String, actual: Double, expected: Double, matches: Boolean) =>
        withClue(s"$label: ") {
          delta(actual, expected).matched shouldBe matches
        }
      }
      // Each bound must be shown to reject a case the other bound accepts, so that neither of
      // the two conjuncts can quietly become dead code.
      val largeMagnitude = delta(1e12d, 1e12d + 1e-3d)
      largeMagnitude.absoluteWithin shouldBe false
      largeMagnitude.relativeWithin shouldBe true
      val nearZero = delta(1e-20d, 2e-20d)
      nearZero.absoluteWithin shouldBe true
      nearZero.relativeWithin shouldBe false
      succeed
    }
  }

  test("the fixture decoder refuses an empty, oversized, over-long or malformed document") {
    IO {
      // An empty array is the case that matters: it decodes perfectly well into no rows, and
      // the report of a run over no rows is zero rows, zero passed and zero failed - the exact
      // shape of a measurement that succeeded. An emptied baseline would therefore retire every
      // comparison in this file while the gate still read a pass.
      decodeFixture("[]") match {
        case Left(message) => message should include("holds no rows")
        case Right(rows) => fail(s"an empty fixture was accepted as ${rows.size} rows")
      }
      // The two ceilings are applied before the work they bound - the character count before
      // the parse, the row count before any row model - and are supplied here at a size a test
      // can afford, on exactly the code the committed fixture goes through.
      val oneRow = """[{"source":"probe"}]"""
      decodeFixture(oneRow, maxCharacters = oneRow.length - 1) match {
        case Left(message) => message should include(s"${oneRow.length} characters")
        case Right(rows) => fail(s"an oversized fixture was accepted as ${rows.size} rows")
      }
      decodeFixture("""[{"source":"a"},{"source":"b"}]""", maxRows = 1) match {
        case Left(message) => message should include("holds 2 rows")
        case Right(rows) => fail(s"an over-long fixture was accepted as ${rows.size} rows")
      }
      // A document that has stopped being an array of rows, and one that is not JSON at all,
      // are each reported as what they are rather than as a row that failed to decode.
      decodeFixture("{}") match {
        case Left(message) => message should include("not a top-level JSON array")
        case Right(rows) => fail(s"a JSON object was accepted as ${rows.size} rows")
      }
      decodeFixture("[{\"source\":").isLeft shouldBe true
      succeed
    }
  }

  test("the parity report directory resolves from the parity.report.dir system property") {
    // The property is part of the gate's contract, not a convenience: the build supplies it as an
    // absolute path and this spec refuses anything else, so the report can only ever be written to
    // the one directory the gate collects from. The three refusals are asserted on the decision
    // function, which keeps them free of any mutation of this JVM's properties.
    for {
      resolved <- reportDirectory
      file <- reportFile
    } yield withClue(s"resolved parity report directory: $resolved; report file: $file: ") {
      resolved.isAbsolute shouldBe true
      sys.props.get(ReportDirectoryProperty).map(_.trim).filter(_.nonEmpty) match {
        case Some(configured) => resolved shouldBe Paths.get(configured).normalize
        case None => fail(s"the build must supply the '$ReportDirectoryProperty' system property")
      }
      file.getFileName.toString shouldBe ReportFileName
      file.getParent shouldBe resolved

      // An unset property has no fallback, so the report cannot land in a relative directory
      // that the gate does not read.
      reportDirectoryFrom(None).isLeft shouldBe true
      reportDirectoryFrom(Some("   ")).isLeft shouldBe true
      // A relative value is refused for the same reason, and the message says which.
      reportDirectoryFrom(Some("target/parity-report")) match {
        case Left(message) => message should include("relative")
        case Right(accepted) => fail(s"a relative report directory was accepted as $accepted")
      }
      reportDirectoryFrom(Some("/tmp/absolute-report-directory")) shouldBe
        Right(Paths.get("/tmp/absolute-report-directory"))
    }
  }

  //-------------------------------------------------------------------------
  /** Asserts that a decoded matrix has at least one row and that every row is the same width. */
  private def rectangular(matrix: Vector[Vector[Double]]): Assertion = {
    matrix should not be empty
    matrix.head should not be empty
    matrix.map(_.size).distinct should have size 1L
  }

  /** Asserts that two decoded matrices have identical dimensions. */
  private def sameShape(first: Vector[Vector[Double]], second: Vector[Vector[Double]]): Assertion = {
    second should have size first.size.toLong
    second.head should have size first.head.size.toLong
  }
}

/**
 * The fixture model, the tolerance rule, the evaluator and the report writer of this spec.
 *
 * Everything here is confined to this package, which holds this one file, so none of it is part
 * of any module's surface. It lives in the companion rather than in the suite for two reasons:
 * the JSON derivation of section "Decoding" needs the row types to sit on a stable path, and
 * keeping the measurement apparatus out of the suite body makes it plain that the suite itself
 * contributes nothing to the measurement beyond ordering it.
 */
private[parity] object DoubleArrayParitySpec {

  //-------------------------------------------------------------------------
  // Contract constants. Each of the first five is part of an agreement with something outside
  // this file, so none of them may drift: the resource name with the capture script that writes
  // it, and the fixture, report and property names with the gate script that reads them.
  //-------------------------------------------------------------------------

  /** The fixture name, which is also the `fixture` field of the report. */
  val FixtureName: String = "double-array"

  /** The classpath name of the captured baseline, relative to the test resource root. */
  val FixtureResource: String = "parity/double-array-baseline.json"

  /** The file the report is written to inside the report directory. */
  val ReportFileName: String = "double-array.json"

  /** The system property through which the build supplies the report directory. */
  val ReportDirectoryProperty: String = "parity.report.dir"

  /**
   * The longest fixture document this spec decodes, in characters.
   *
   * Checked before the document is parsed, so that the size of a resource cannot decide the
   * memory of the test process. The committed baseline is roughly 46 KiB, so the ceiling is far
   * above any regeneration of it while still being a ceiling.
   */
  val MaxFixtureCharacters: Int = 32 * 1024 * 1024

  /**
   * The largest number of rows this spec measures.
   *
   * Checked on the parsed array before any row model is built. The committed baseline holds
   * fourteen rows.
   */
  val MaxFixtureRows: Int = 200000

  /** The number of failures written to the report; `failed` always carries the true total. */
  val FailureReportLimit: Int = 200

  /** The absolute and relative bound of the parity rule. */
  val Tolerance: Double = 1e-9

  /** The floor of the relative bound, which keeps it meaningful as the values approach zero. */
  val RelativeFloor: Double = 1e-300

  //-------------------------------------------------------------------------
  // The population the fixture must carry, and the id prefixes that identify it. These are the
  // same floors the capture asserts on its own side, restated here so that the consumer of the
  // fixture rejects a reduced one instead of reporting a green measurement of less.
  //-------------------------------------------------------------------------

  /** The least number of rows the baseline is worth measuring. */
  val MinimumRows: Int = 40

  /** The least number of rows built from the inputs of the Java tests. */
  val MinimumJavaDerivedRows: Int = 16

  /** The least number of seeded-random rows, which exercise the relative bound across magnitudes. */
  val MinimumSeededRandomRows: Int = 16

  /** The least number of rows carrying values outside the finite range. */
  val MinimumIeeeEdgeRows: Int = 4

  /** The id prefix of a row whose expectations the capture cross-checked against Java literals. */
  val JavaDerivedPrefix: String = "javatest-"

  /** The id prefix of a seeded-random row. */
  val SeededRandomPrefix: String = "random-seeded-"

  /** The id prefix of a row built around signed zero, not-a-number or an infinity. */
  val IeeeEdgePrefix: String = "ieee-"

  /** The three prefixes, so that a row of unstated provenance can be reported. */
  val KnownPrefixes: Vector[String] = Vector(JavaDerivedPrefix, SeededRandomPrefix, IeeeEdgePrefix)

  /**
   * The twenty-two expectations of a row, in the order they are measured.
   *
   * The order is the order [[prepare]] produces, and the suite asserts the two agree: the list is
   * what makes "every expectation of every row was measured" checkable rather than asserted.
   */
  val ExpectationNames: Vector[String] =
    Vector(
      "plusScalar",
      "plusArray",
      "minusScalar",
      "minusArray",
      "multipliedByScalar",
      "multipliedByArray",
      "dividedByScalar",
      "dividedByArray",
      "mapSquared",
      "reduceSum",
      "sum",
      "min",
      "max",
      "sorted",
      "concat",
      "subArray",
      "matrixMultipliedBy",
      "matrixPlus",
      "matrixMinus",
      "matrixTranspose",
      "matrixTotal",
      "matrixWith")

  /** Squares a value: the function the fixture's `mapSquared` was captured through. */
  val square: Double => Double = value => value * value

  /** Adds a value to an accumulator: the reduction the fixture's `reduceSum` was captured through. */
  val add: (Double, Double) => Double = (accumulated, value) => accumulated + value

  /** The identity of that reduction, which is what makes `reduceSum` the general form of `sum`. */
  val ReduceIdentity: Double = 0d

  //-------------------------------------------------------------------------
  // The fixture model.
  //
  // One flat product of primitives, arrays and arrays of arrays, with every field required. The
  // shape is deliberately not polymorphic: it is what makes the row decodable by compile-time
  // derivation, and it is what makes "the fixture is fully populated" a property of the decode
  // rather than of a check that could be forgotten.
  //-------------------------------------------------------------------------

  /**
   * One row of the fixture: the inputs an operation was applied to, and every result it produced.
   *
   * `a` and `b` are non-empty and of equal length, `matrixA` and `matrixB` are non-empty and of
   * equal shape, the slice bounds satisfy `0 <= subArrayFrom <= subArrayTo <= a.size` and the
   * `with` indices are inside `matrixA`. The suite asserts all of that before the measurement so
   * that a reshaped fixture is reported as such.
   *
   * @param id  the stable row identifier, which is what makes a reported failure attributable
   * @param a  the first operand
   * @param b  the second operand, of the same length as the first
   * @param scalar  the scalar operand of the four scalar array operations and of the matrix multiply
   * @param subArrayFrom  the inclusive lower bound of the captured slice
   * @param subArrayTo  the exclusive upper bound of the captured slice
   * @param matrixA  the first matrix operand
   * @param matrixB  the second matrix operand, of the same shape as the first
   * @param withRow  the row of the single-element matrix replacement
   * @param withColumn  the column of the single-element matrix replacement
   * @param withValue  the value of the single-element matrix replacement
   */
  final case class Row(
      id: String,
      a: Vector[Double],
      b: Vector[Double],
      scalar: Double,
      subArrayFrom: Int,
      subArrayTo: Int,
      matrixA: Vector[Vector[Double]],
      matrixB: Vector[Vector[Double]],
      withRow: Int,
      withColumn: Int,
      withValue: Double,
      plusScalar: Vector[Double],
      plusArray: Vector[Double],
      minusScalar: Vector[Double],
      minusArray: Vector[Double],
      multipliedByScalar: Vector[Double],
      multipliedByArray: Vector[Double],
      dividedByScalar: Vector[Double],
      dividedByArray: Vector[Double],
      mapSquared: Vector[Double],
      reduceSum: Double,
      sum: Double,
      min: Double,
      max: Double,
      sorted: Vector[Double],
      concat: Vector[Double],
      subArray: Vector[Double],
      matrixMultipliedBy: Vector[Vector[Double]],
      matrixPlus: Vector[Vector[Double]],
      matrixMinus: Vector[Vector[Double]],
      matrixTranspose: Vector[Vector[Double]],
      matrixTotal: Double,
      matrixWith: Vector[Vector[Double]]) {

    /** The expectations that must have the length of `a`, named for the shape assertions. */
    def elementWiseExpectations: Vector[(String, Vector[Double])] =
      Vector(
        "plusScalar" -> plusScalar,
        "plusArray" -> plusArray,
        "minusScalar" -> minusScalar,
        "minusArray" -> minusArray,
        "multipliedByScalar" -> multipliedByScalar,
        "multipliedByArray" -> multipliedByArray,
        "dividedByScalar" -> dividedByScalar,
        "dividedByArray" -> dividedByArray,
        "mapSquared" -> mapSquared,
        "sorted" -> sorted)

    /** The matrix expectations that must have the shape of `matrixA`; `matrixTranspose` does not. */
    def sameShapeMatrixExpectations: Vector[(String, Vector[Vector[Double]])] =
      Vector(
        "matrixMultipliedBy" -> matrixMultipliedBy,
        "matrixPlus" -> matrixPlus,
        "matrixMinus" -> matrixMinus,
        "matrixWith" -> matrixWith)
  }

  //-------------------------------------------------------------------------
  // Decoding.
  //
  // The single tagged-double decoder below is the whole of the non-finite policy on this path.
  // Being declared here, in lexical scope, it outranks the decoder the JSON library publishes
  // for `Double` in its own companion, so every double of the document - an input or an
  // expectation - accepts a number or exactly one of the three tags "NaN", "Infinity" and
  // "-Infinity" and rejects everything else. The two collection decoders are built from it
  // explicitly rather than summoned, which is also what makes it visibly used.
  //
  // The row decoder is derived at compile time by the library's semi-automatic derivation, so
  // nothing on this path reflects at run time.
  //-------------------------------------------------------------------------

  implicit val doubleDecoder: Decoder[Double] = Codecs.taggedDouble

  implicit val doublesDecoder: Decoder[Vector[Double]] = Decoder.decodeVector(doubleDecoder)

  implicit val matrixDecoder: Decoder[Vector[Vector[Double]]] = Decoder.decodeVector(doublesDecoder)

  implicit val rowDecoder: Decoder[Row] = deriveDecoder[Row]

  //-------------------------------------------------------------------------
  // The three shapes an expectation and an observation can take. Keeping the expectation and the
  // observation in separate hierarchies is what lets the comparison pair a captured
  // `Vector[Double]` against a `DoubleArray` without either side having to be converted into the
  // other first.
  //-------------------------------------------------------------------------

  /** What the Java baseline recorded for one operation. */
  sealed trait Expectation

  object Expectation {

    /** A single number. */
    final case class Value(value: Double) extends Expectation

    /** An array of numbers. */
    final case class Elements(values: Vector[Double]) extends Expectation

    /** An array of row arrays. */
    final case class Rows(values: Vector[Vector[Double]]) extends Expectation
  }

  /** What the Scala port produced for the same operation. */
  sealed trait Observed

  object Observed {

    /** A single number. */
    final case class Value(value: Double) extends Observed

    /** An array of numbers. */
    final case class Elements(values: DoubleArray) extends Observed

    /** An array of row arrays. */
    final case class Rows(values: DoubleMatrix) extends Observed

    /**
     * The port raised an error where the baseline recorded a value.
     *
     * The fixture has no failure expectations - every row satisfies every precondition of every
     * operation it names - so this is always a parity failure. It is observed rather than allowed
     * to escape so that the report is still written.
     */
    final case class Thrown(text: String) extends Observed
  }

  /**
   * A deferred call into the port, in the shape the operation returns.
   *
   * The call is deferred rather than made during preparation so that an exception it raises is
   * observed as an outcome of the measurement instead of escaping and preventing the report from
   * being written.
   */
  sealed trait Computation

  object Computation {

    /** An operation returning a number. */
    final case class OfValue(run: () => Double) extends Computation

    /** An operation returning an array. */
    final case class OfElements(run: () => DoubleArray) extends Computation

    /** An operation returning a matrix. */
    final case class OfRows(run: () => DoubleMatrix) extends Computation
  }

  /** One expectation of one row, ready to be measured. */
  final case class PreparedCase(
      id: String,
      expectation: String,
      expected: Expectation,
      computation: Computation)

  //-------------------------------------------------------------------------
  // The tolerance rule.
  //-------------------------------------------------------------------------

  /**
   * The outcome of one comparison.
   *
   * Both bounds are reported separately from the verdict so that a spec can demonstrate that
   * each of them rejects a case the other accepts, which is how the two conjuncts are kept from
   * becoming dead code.
   *
   * @param matched  whether the two values are at parity
   * @param absolute  the absolute difference, which is not a number when either value is not
   * @param relative  the absolute difference over the scale of the larger operand
   * @param absoluteWithin  whether the absolute bound holds
   * @param relativeWithin  whether the relative bound holds
   */
  final case class Delta(
      matched: Boolean,
      absolute: Double,
      relative: Double,
      absoluteWithin: Boolean,
      relativeWithin: Boolean)

  /**
   * Compares one value against its captured expectation.
   *
   * A finite pair is at parity only when the absolute difference is within `1e-9` '''and''' also
   * within `1e-9` of the scale of the larger operand, floored at `1e-300`. Applying only one of
   * the two would leave a gap at one end of the range: the relative bound alone admits a large
   * absolute error between large values, and the absolute bound alone admits an arbitrarily
   * large proportional error between values near zero.
   *
   * A pair involving a value outside the finite range is classified before any difference is
   * computed, because a comparison against a difference that is itself not a number is always
   * false and would therefore report two equal not-a-numbers as a discrepancy. Two
   * not-a-numbers are at parity, each infinity is at parity with itself, and every other
   * combination differs. The reported differences remain the values the arithmetic actually
   * produces, so the report states what was measured rather than a substitute.
   *
   * @param actual  the value the port produced
   * @param expected  the value captured from the Java implementation
   * @return the outcome of the comparison
   */
  def delta(actual: Double, expected: Double): Delta = {
    val absolute = math.abs(actual - expected)
    val scale = math.max(math.max(math.abs(actual), math.abs(expected)), RelativeFloor)
    val relative = absolute / scale
    val absoluteWithin = absolute <= Tolerance
    val relativeWithin = absolute <= Tolerance * scale
    val matched =
      if (actual.isNaN || expected.isNaN) {
        actual.isNaN && expected.isNaN
      } else if (actual.isInfinite || expected.isInfinite) {
        actual == expected
      } else {
        absoluteWithin && relativeWithin
      }
    Delta(matched, absolute, relative, absoluteWithin, relativeWithin)
  }

  //-------------------------------------------------------------------------
  // The running result.
  //-------------------------------------------------------------------------

  /**
   * The accumulated outcome of a run, threaded through the evaluation as an immutable value.
   *
   * `passed` and `failed` count individual checks: one per number compared, one per shape
   * compared and one per unexpected error, so their sum is far larger than the number of rows.
   * `failures` holds the first [[FailureReportLimit]] failures only, which keeps a
   * systematically broken port from emitting an unusable report, while `failed` remains the true
   * total that the gate reads.
   */
  final case class Tally(passed: Int, failed: Int, failures: Vector[Json]) {

    /** Records one check that met the tolerance rule. */
    def pass: Tally = copy(passed = passed + 1)

    /** Records one check that did not, retaining its description up to the reporting limit. */
    def record(failure: Json): Tally =
      copy(
        failed = failed + 1,
        failures = if (failures.size < FailureReportLimit) failures :+ failure else failures)

    /** The failures to write, with a final entry making any truncation explicit. */
    def reportedFailures: Vector[Json] =
      if (failed <= failures.size) {
        failures
      } else {
        failures :+ Json.obj(
          "truncated" -> Json.fromBoolean(true),
          "reportedFailures" -> Json.fromInt(failures.size),
          "omittedFailures" -> Json.fromInt(failed - failures.size),
          "message" -> Json.fromString(
            s"the failure list is capped at $FailureReportLimit entries; 'failed' is the true total"))
      }

    /** The first failure, for the message of the assertion, or empty text when there was none. */
    def firstFailureSummary: String =
      failures.headOption.fold("")(first => s"; first failure: ${first.noSpaces}")
  }

  object Tally {

    /** No checks run. */
    val empty: Tally = Tally(0, 0, Vector.empty)
  }

  //-------------------------------------------------------------------------
  // Preparation.
  //
  // Every row yields exactly the twenty-two cases of [[ExpectationNames]], in that order, each
  // pairing a captured expectation with the call that reproduces it. There is no dispatch on a
  // name read out of the document and no operation that a row may or may not carry: the row
  // model requires every field, so a fixture that dropped one fails the decode outright. A field
  // the fixture gained would be ignored by the derived decoder instead, which is why the twenty-
  // two names are fixed here rather than read from the document - an operation added to the
  // capture is measured only once it is added to this list, and the capture asserts its own row
  // and expectation counts so the two cannot drift apart unnoticed.
  //
  // The operand of each operation is the operand the capture used, which the name alone does not
  // reveal: the scalar operations take `scalar`, the array operations take `b`,
  // `matrixMultipliedBy` is a scalar multiply, and the second matrix is used only by `matrixPlus`
  // and `matrixMinus`.
  //-------------------------------------------------------------------------

  /** Prepares every expectation of one row, in the order [[ExpectationNames]] states. */
  def prepare(row: Row): Vector[PreparedCase] = {
    val a = DoubleArray.copyOf(row.a)
    val b = DoubleArray.copyOf(row.b)
    val first = toMatrix(row.matrixA)
    val second = toMatrix(row.matrixB)

    def elements(name: String, expected: Vector[Double], computation: => DoubleArray): PreparedCase =
      PreparedCase(row.id, name, Expectation.Elements(expected), Computation.OfElements(() => computation))

    def value(name: String, expected: Double, computation: => Double): PreparedCase =
      PreparedCase(row.id, name, Expectation.Value(expected), Computation.OfValue(() => computation))

    def matrix(
        name: String,
        expected: Vector[Vector[Double]],
        computation: => DoubleMatrix): PreparedCase =
      PreparedCase(row.id, name, Expectation.Rows(expected), Computation.OfRows(() => computation))

    Vector(
      elements("plusScalar", row.plusScalar, a.plus(row.scalar)),
      elements("plusArray", row.plusArray, a.plus(b)),
      elements("minusScalar", row.minusScalar, a.minus(row.scalar)),
      elements("minusArray", row.minusArray, a.minus(b)),
      elements("multipliedByScalar", row.multipliedByScalar, a.multipliedBy(row.scalar)),
      elements("multipliedByArray", row.multipliedByArray, a.multipliedBy(b)),
      elements("dividedByScalar", row.dividedByScalar, a.dividedBy(row.scalar)),
      elements("dividedByArray", row.dividedByArray, a.dividedBy(b)),
      elements("mapSquared", row.mapSquared, a.map(square)),
      value("reduceSum", row.reduceSum, a.reduce(ReduceIdentity, add)),
      value("sum", row.sum, a.sum),
      value("min", row.min, a.min),
      value("max", row.max, a.max),
      elements("sorted", row.sorted, a.sorted),
      elements("concat", row.concat, a.concat(b)),
      elements("subArray", row.subArray, a.subArray(row.subArrayFrom, row.subArrayTo)),
      matrix("matrixMultipliedBy", row.matrixMultipliedBy, first.multipliedBy(row.scalar)),
      matrix("matrixPlus", row.matrixPlus, first.plus(second)),
      matrix("matrixMinus", row.matrixMinus, first.minus(second)),
      matrix("matrixTranspose", row.matrixTranspose, first.transpose),
      value("matrixTotal", row.matrixTotal, first.total),
      matrix("matrixWith", row.matrixWith, first.`with`(row.withRow, row.withColumn, row.withValue)))
  }

  /**
   * Builds a matrix from a decoded one.
   *
   * The intermediate arrays are freshly allocated here and the factory copies them again, so the
   * matrix shares no storage with anything, which is the same guarantee the public factories of
   * the type give every other caller. The module-private unchecked factories are reachable from
   * this package and are deliberately not used.
   */
  def toMatrix(values: Vector[Vector[Double]]): DoubleMatrix =
    DoubleMatrix.copyOf(values.map(_.toArray).toArray)

  //-------------------------------------------------------------------------
  // Evaluation.
  //-------------------------------------------------------------------------

  /** Measures every prepared case, folding the outcomes into one immutable result. */
  def evaluate(cases: Vector[PreparedCase]): Tally =
    cases.foldLeft(Tally.empty)((tally, prepared) => check(prepared, observe(prepared), tally))

  /**
   * Runs one deferred call, turning an error into an observation.
   *
   * An error is never an expectation here - the fixture carries none - but it must not escape
   * either, because an escaping error would end the run before the report was written.
   */
  def observe(prepared: PreparedCase): Observed = {
    val attempted = prepared.computation match {
      case Computation.OfValue(run) => Try(Observed.Value(run()): Observed)
      case Computation.OfElements(run) => Try(Observed.Elements(run()): Observed)
      case Computation.OfRows(run) => Try(Observed.Rows(run()): Observed)
    }
    attempted match {
      case Success(observed) => observed
      case Failure(thrown) => Observed.Thrown(describeThrowable(thrown))
    }
  }

  /** Renders an error as `SimpleClassName: message`. */
  def describeThrowable(thrown: Throwable): String =
    s"${thrown.getClass.getSimpleName}: ${Option(thrown.getMessage).getOrElse("")}"

  /**
   * Compares one observation against its expectation, adding its checks to the running result.
   *
   * The pairing of the two shapes is established during preparation, so the final case cannot
   * arise from a well-formed fixture; it is present because a total match is worth more than a
   * pattern the compiler cannot verify.
   */
  def check(prepared: PreparedCase, observed: Observed, tally: Tally): Tally =
    (prepared.expected, observed) match {
      case (expected, Observed.Thrown(actual)) =>
        tally.record(
          descriptiveFailure(
            prepared,
            "evaluation",
            describeExpectation(expected),
            actual,
            "the port raised an error on an operation the Java baseline completed"))
      case (Expectation.Value(expected), Observed.Value(actual)) =>
        checkValue(prepared, "value", Vector.empty, expected, actual, tally)
      case (Expectation.Elements(expected), Observed.Elements(actual)) =>
        checkElements(prepared, expected, actual, tally)
      case (Expectation.Rows(expected), Observed.Rows(actual)) =>
        checkRows(prepared, expected, actual, tally)
      case (expected, actual) =>
        tally.record(
          descriptiveFailure(
            prepared,
            "kind",
            describeExpectation(expected),
            describeObserved(actual),
            "the port returned a different kind of value than the fixture recorded"))
    }

  /**
   * Compares an array against its expectation, the size first.
   *
   * The size is compared before any element, and a difference in it ends the comparison of that
   * operation: indexing into an array of a different length would raise an exception from the
   * harness itself, which is the one failure mode that would cost a run its report.
   */
  def checkElements(
      prepared: PreparedCase,
      expected: Vector[Double],
      actual: DoubleArray,
      tally: Tally): Tally =
    if (expected.size != actual.size) {
      tally.record(
        descriptiveFailure(
          prepared,
          "shape",
          s"${expected.size} elements",
          s"${actual.size} elements",
          "the port produced an array of a different length"))
    } else {
      expected.indices.foldLeft(tally.pass)((running, index) =>
        checkValue(
          prepared,
          "element",
          Vector("index" -> Json.fromInt(index)),
          expected(index),
          actual.get(index),
          running))
    }

  /**
   * Compares a matrix against its expectation, both dimensions first.
   *
   * The dimensions are compared against the expectation and never against an input, because
   * `transpose` changes them: the fixture carries a three-by-six matrix whose transpose is
   * six-by-three, so measuring against the input would be wrong for exactly the operation most
   * likely to be got wrong. Elements are read through the indexed accessor rather than by row
   * or column, both of which copy.
   */
  def checkRows(
      prepared: PreparedCase,
      expected: Vector[Vector[Double]],
      actual: DoubleMatrix,
      tally: Tally): Tally = {
    val expectedRows = expected.size
    val expectedColumns = expected.headOption.fold(0)(first => first.size)
    if (expectedRows != actual.rowCount || expectedColumns != actual.columnCount) {
      tally.record(
        descriptiveFailure(
          prepared,
          "shape",
          s"$expectedRows x $expectedColumns",
          s"${actual.rowCount} x ${actual.columnCount}",
          "the port produced a matrix of different dimensions"))
    } else {
      val positions = for {
        rowIndex <- 0 until expectedRows
        columnIndex <- 0 until expectedColumns
      } yield (rowIndex, columnIndex)
      positions.foldLeft(tally.pass) { (running, position) =>
        val (rowIndex, columnIndex) = position
        checkValue(
          prepared,
          "element",
          Vector("row" -> Json.fromInt(rowIndex), "column" -> Json.fromInt(columnIndex)),
          expected(rowIndex)(columnIndex),
          actual.get(rowIndex, columnIndex),
          running)
      }
    }
  }

  /** Applies the tolerance rule to one number, recording the outcome. */
  def checkValue(
      prepared: PreparedCase,
      kind: String,
      position: Vector[(String, Json)],
      expected: Double,
      actual: Double,
      tally: Tally): Tally = {
    val measured = delta(actual, expected)
    if (measured.matched) {
      tally.pass
    } else {
      tally.record(numericFailure(prepared, kind, position, expected, actual, measured))
    }
  }

  //-------------------------------------------------------------------------
  // Failure descriptions.
  //
  // Every entry names the row it came from, the expectation, the kind of check and, where the
  // value sits inside a result, its position - so a failure can be acted on without re-running
  // anything. Numbers go through the tagged-double encoder, so a difference that is itself not a
  // number still leaves the document valid JSON for the shell that reads it.
  //-------------------------------------------------------------------------

  /** Describes one numeric discrepancy. */
  def numericFailure(
      prepared: PreparedCase,
      kind: String,
      position: Vector[(String, Json)],
      expected: Double,
      actual: Double,
      measured: Delta): Json =
    Json.obj(
      (attribution(prepared) ++
        Vector("check" -> Json.fromString(kind)) ++
        position ++
        Vector(
          "expected" -> encodeDouble(expected),
          "actual" -> encodeDouble(actual),
          "absDelta" -> encodeDouble(measured.absolute),
          "relDelta" -> encodeDouble(measured.relative))): _*)

  /** Describes one discrepancy that is not a difference between two numbers. */
  def descriptiveFailure(
      prepared: PreparedCase,
      kind: String,
      expected: String,
      actual: String,
      message: String): Json =
    Json.obj(
      (attribution(prepared) ++
        Vector(
          "check" -> Json.fromString(kind),
          "expected" -> Json.fromString(expected),
          "actual" -> Json.fromString(actual),
          "message" -> Json.fromString(message))): _*)

  /** The fields that attribute a failure to a row and an expectation. */
  def attribution(prepared: PreparedCase): Vector[(String, Json)] =
    Vector(
      "id" -> Json.fromString(prepared.id),
      "expectation" -> Json.fromString(prepared.expectation))

  /** Encodes a number under the port's single policy for a value JSON cannot express. */
  def encodeDouble(value: Double): Json = Codecs.taggedDouble(value)

  /** Renders an expectation for a message. */
  def describeExpectation(expectation: Expectation): String = expectation match {
    case Expectation.Value(value) => value.toString
    case Expectation.Elements(values) => values.mkString("[", ", ", "]")
    case Expectation.Rows(values) => values.map(_.mkString("[", ", ", "]")).mkString("[", ", ", "]")
  }

  /** Renders an observation for a message. */
  def describeObserved(observed: Observed): String = observed match {
    case Observed.Value(value) => value.toString
    case Observed.Elements(values) => values.toList.mkString("[", ", ", "]")
    case Observed.Rows(values) =>
      Vector
        .tabulate(values.rowCount, values.columnCount)((rowIndex, columnIndex) =>
          values.get(rowIndex, columnIndex))
        .map(_.mkString("[", ", ", "]"))
        .mkString("[", ", ", "]")
    case Observed.Thrown(text) => text
  }

  //-------------------------------------------------------------------------
  // Loading and reporting - the only two places that touch anything outside this process, and
  // both of them inside an effect, so nothing is read or written while this object initialises.
  //-------------------------------------------------------------------------

  /** Reads and decodes the captured baseline, failing the effect if it cannot be read. */
  def loadFixture: IO[Vector[Row]] =
    Resources.readClasspathText(FixtureResource).flatMap(text => lift(decodeFixture(text)))

  /**
   * Decodes the whole document under explicit bounds, reporting a refusal as a single message.
   *
   * Each bound is applied before the work it bounds: the length of the text is known without
   * parsing it, and the number of rows is known without decoding them, so neither an oversized
   * resource nor an oversized array is ever materialised as row models. The empty case is refused
   * for a different reason - a fixture of no rows would publish a report of zero rows, zero passed
   * and zero failed, which is exactly the shape of a measurement that succeeded, so an emptied
   * baseline would retire every comparison in this file while the gate still read a pass.
   */
  def decodeFixture(
      text: String,
      maxCharacters: Int = MaxFixtureCharacters,
      maxRows: Int = MaxFixtureRows): Either[String, Vector[Row]] =
    if (text.length > maxCharacters) {
      Left(
        s"the parity fixture '$FixtureResource' is ${text.length} characters, which is beyond " +
          s"the $maxCharacters this spec decodes")
    } else {
      parser
        .parse(text)
        .leftMap(error =>
          s"the parity fixture '$FixtureResource' did not parse: " +
            Option(error.getMessage).getOrElse(error.toString))
        .flatMap(json =>
          json.asArray.toRight(
            s"the parity fixture '$FixtureResource' is not a top-level JSON array of rows"))
        .flatMap { rows =>
          if (rows.isEmpty) {
            Left(
              s"the parity fixture '$FixtureResource' holds no rows, so it would measure nothing " +
                "while reporting zero failures")
          } else if (rows.size > maxRows) {
            Left(
              s"the parity fixture '$FixtureResource' holds ${rows.size} rows, which is beyond " +
                s"the $maxRows this spec measures")
          } else {
            Decoder[Vector[Row]]
              .decodeJson(Json.fromValues(rows))
              .leftMap(error =>
                s"the parity fixture '$FixtureResource' did not decode: " +
                  Option(error.getMessage).getOrElse(error.toString))
          }
        }
    }

  /**
   * Lifts a malformed-document report into a failed effect.
   *
   * This is the one place where a value in the error channel becomes a thrown error, and it is
   * at the edge of the spec by design. A fixture that cannot be read or understood is not a
   * parity result of any kind, so it fails the run outright rather than being counted.
   */
  def lift[A](result: Either[String, A]): IO[A] =
    IO.fromEither(result.leftMap(message => new IllegalStateException(message)))

  /**
   * The directory the report is written to, which the build must have named as an absolute path.
   *
   * There is deliberately no fallback of any kind. The gate reads the six reports from one
   * absolute directory, so a default - or an accepted relative value, which under a forked test
   * JVM resolves against that JVM's working directory rather than the build root - would let this
   * spec write its report where the gate never looks. A gate that finds no report cannot tell it
   * apart from a measurement that was never made, so a misconfigured run must fail loudly instead
   * of quietly publishing somewhere else.
   */
  def reportDirectory: IO[Path] =
    IO.delay(sys.props.get(ReportDirectoryProperty)).flatMap(configured =>
      lift(reportDirectoryFrom(configured)))

  /**
   * Decides where the report goes, from the configured text alone, so the decision is testable
   * without touching the system properties of the running JVM.
   *
   * @param configured  the value of the system property, if it is set
   * @return the directory, or the explanation of why the configured value cannot be used
   */
  def reportDirectoryFrom(configured: Option[String]): Either[String, Path] =
    configured.map(_.trim).filter(_.nonEmpty) match {
      case None =>
        Left(
          s"the system property '$ReportDirectoryProperty' is not set, so there is nowhere to " +
            "write the parity report; build.sbt supplies it to the forked test JVM through " +
            "'Test / javaOptions' under 'Test / fork := true', and this spec has no default on " +
            "purpose, because a report the gate cannot find is indistinguishable from a " +
            "measurement that was never made")
      case Some(value) =>
        Try(Paths.get(value)) match {
          case Failure(error) =>
            Left(
              s"the system property '$ReportDirectoryProperty' is '$value', which is not a " +
                s"usable path: ${error.getMessage}")
          case Success(path) if path.isAbsolute => Right(path.normalize)
          case Success(_) =>
            Left(
              s"the system property '$ReportDirectoryProperty' is '$value', which is relative; " +
                "it must be absolute, because the test JVM is forked and a relative path would " +
                "resolve against its working directory rather than the one directory the gate " +
                "reads the reports from")
        }
    }

  /** The report document itself. */
  def reportFile: IO[Path] = reportDirectory.map(_.resolve(ReportFileName))

  /**
   * Writes the report, and is called before the run is judged.
   *
   * The order matters: the counts of a failing run are the whole point of publishing them, so
   * they are on disk before any assertion can end the test. The document carries exactly the
   * five fields the gate reads and no others.
   *
   * @param tally  the accumulated outcome of the run
   * @param rows  the number of fixture rows evaluated
   * @return the path written, so that it can be named in the assertion's message
   */
  def writeReport(tally: Tally, rows: Int): IO[Path] =
    reportDirectory.flatMap(directory =>
      IO.blocking {
        val _ = Files.createDirectories(directory)
        val document = Json.obj(
          "fixture" -> Json.fromString(FixtureName),
          "rows" -> Json.fromInt(rows),
          "passed" -> Json.fromInt(tally.passed),
          "failed" -> Json.fromInt(tally.failed),
          "failures" -> Json.arr(tally.reportedFailures: _*))
        Files.write(
          directory.resolve(ReportFileName),
          (document.spaces2 + "\n").getBytes(StandardCharsets.UTF_8))
      })
}
