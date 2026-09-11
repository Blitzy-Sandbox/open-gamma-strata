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
 * obligation for the port, and the values they are held to are not re-derived here by hand: they
 * were captured from the *Java* implementation by `tools/parity-capture/capture-baseline.jsh`,
 * which cross-checked every one of them against the constants of the Java tests before writing
 * them out, and they are committed as
 * `strata-collect/src/test/resources/parity/double-array-baseline.json`. This spec replays every
 * captured operation through the Scala port, measures the difference, publishes a report of the
 * measurement, and only then asserts that nothing differed.
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
 * ===The fixture is the authority===
 *
 * The fixture is generated, never hand-authored, and this spec never edits it, never "corrects"
 * an expectation, never loosens the tolerance and never skips a row. If a row disagrees with the
 * port, the port is wrong and the disagreement is reported: that is the gate doing its job.
 *
 * ===Row schema===
 *
 * The document is a JSON array of row objects, each carrying the inputs an operation was applied
 * to and the results it produced. The schema of record is section 6 of
 * `tools/parity-capture/README.md`; strict JSON admits no comments, so it is restated here:
 *
 *   - `source` - the Java test method or generated population the row came from, which is what
 *     makes a failure attributable.
 *   - `a`, `b` - two arrays of the same length, either of which may be empty.
 *   - `scalar` - the scalar operand of the operations that take one.
 *   - `matrixA`, `matrixB` - rectangular arrays of row arrays, or a JSON nothing where the row has
 *     no matrix inputs. `matrixB` is present only when `matrixA` is.
 *   - `results` - one entry per operation, each naming its `op` and carrying either a `result` or
 *     an `error`, plus whichever parameters that operation took.
 *
 * Twenty operations appear. Thirteen are array operations and are present in every row; four more
 * are present when `matrixA` is; three more when `matrixB` is as well. The operand each takes is
 * the operand the capture used, which is not always the one the name suggests: `plus` and `minus`
 * take the array `b`, while `multipliedBy` and `dividedBy` take the scalar. `matrixMultipliedBy`
 * is a '''scalar''' multiply - `DoubleMatrix` exposes no matrix product, and none is invented
 * here - and `matrixB` is consumed only by `matrixPlus`, `matrixMinus` and `matrixCombine`.
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
 * `"-Infinity"` appear in an input, in a parameter or in an expectation.
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
 * The tolerance of the Java test this fixture was captured from, `1e-14`, belongs to the capture
 * step and is not used here.
 *
 * Values outside the finite range are classified '''before''' any difference is computed, because
 * `NaN <= x` is false and the fixture carries forty-one not-a-number expectations: computed
 * naively, every one of them would be reported as a failure. Two not-a-numbers are equal for the
 * purpose of parity, each infinity equals itself, and anything else involving a value outside the
 * finite range differs.
 *
 * ===Error entries===
 *
 * Four entries record that Java threw rather than produced a value, all of them in the row of the
 * empty array. An error is an expectation in its own right, so each is checked: the port must fail
 * too, and with the same message. The exception *type* is not compared, because three of the four
 * differ by design - the port routes the `min`, `max` and `subArray` preconditions through
 * `ArgCheck`, so an `IllegalArgumentException` replaces Java's `IllegalStateException` and
 * `IndexOutOfBoundsException` while the message is kept unchanged. Both sides therefore have the
 * leading `<ClassName>: ` removed before the messages are compared.
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
      cases <- lift(prepareAll(rows))
      tally <- IO(evaluate(cases))
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

  test("the fixture is a non-empty array of fully populated rows") {
    loadFixture.map { rows =>
      rows should not be empty
      rows.map(_.source).distinct should have size rows.size.toLong
      rows.foreach { row =>
        withClue(s"fixture row '${row.source}': ") {
          // The two arrays are operands of one another, so a length difference would make
          // `plus`, `minus` and `concat` measure something the capture never computed.
          row.a should have size row.b.size.toLong
          if (row.matrixB.isDefined) {
            row.matrixA shouldBe defined
          }
          row.matrixA.foreach(matrix => rectangular(matrix))
          row.matrixB.foreach(matrix => rectangular(matrix))
          row.matrixA.foreach(first => row.matrixB.foreach(second => sameShape(first, second)))
          // Exactly one of `result` and `error` carries the expectation of an entry, and the
          // inventory of operations is fixed by which matrices the row carries - so a fixture
          // truncated mid-row, or extended with an operation this spec cannot evaluate, is
          // reported here rather than silently reducing the number of checks.
          row.results.map(_.op).distinct should have size row.results.size.toLong
          row.results.foreach { entry =>
            withClue(s"operation '${entry.op}': ") {
              entry.result.isDefined should not be entry.error.isDefined
            }
          }
          row.results.map(_.op).toSet shouldBe expectedOperations(row)
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

  test("the parity report directory resolves from the parity.report.dir system property") {
    IO {
      val resolved = reportDirectory
      withClue(s"resolved parity report directory: $resolved; report file: $reportFile: ") {
        sys.props.get(ReportDirectoryProperty).filter(_.nonEmpty) match {
          case Some(configured) => resolved.toString shouldBe configured
          case None => resolved.toString shouldBe DefaultReportDirectory
        }
        reportFile.getFileName.toString shouldBe ReportFileName
      }
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

  /** The directory used when the build has not supplied one, so an ad-hoc run still works. */
  val DefaultReportDirectory: String = "target/parity-report"

  /** The number of failures written to the report; `failed` always carries the true total. */
  val FailureReportLimit: Int = 200

  /** The absolute and relative bound of the parity rule. */
  val Tolerance: Double = 1e-9

  /** The floor of the relative bound, which keeps it meaningful as the values approach zero. */
  val RelativeFloor: Double = 1e-300

  //-------------------------------------------------------------------------
  // The operation inventory. The array operations are present in every row; the next four
  // whenever the row carries `matrixA`; the last three whenever it carries `matrixB` as well.
  //-------------------------------------------------------------------------

  /** The operations every row carries. */
  val ArrayOperations: Set[String] =
    Set(
      "plus",
      "minus",
      "multipliedBy",
      "dividedBy",
      "map",
      "reduce",
      "sum",
      "min",
      "max",
      "sorted",
      "concat",
      "subArray",
      "with")

  /** The operations a row carrying `matrixA` adds. */
  val MatrixOperations: Set[String] = Set("matrixMultipliedBy", "transpose", "total", "matrixWith")

  /** The operations a row carrying both matrices adds. */
  val PairedMatrixOperations: Set[String] = Set("matrixPlus", "matrixMinus", "matrixCombine")

  /** The operations the given row must carry, given which matrices it has. */
  def expectedOperations(row: Row): Set[String] =
    ArrayOperations ++
      (if (row.matrixA.isDefined) MatrixOperations else Set.empty[String]) ++
      (if (row.matrixB.isDefined) PairedMatrixOperations else Set.empty[String])

  //-------------------------------------------------------------------------
  // The functions the capture applied, named in the fixture beside the results they produced.
  // The text is checked against the fixture before the function is used, so a capture that
  // changed the function cannot lead this spec to compare a different quantity in silence.
  //-------------------------------------------------------------------------

  /** The text the fixture carries for the mapped function, and its implementation. */
  val MapFunctionText: String = "x -> x * x"

  /** The text the fixture carries for the reduction, and its implementation. */
  val ReduceFunctionText: String = "(acc, v) -> acc + v"

  /** The text the fixture carries for the element-wise matrix combination, and its implementation. */
  val CombineFunctionText: String = "(x, y) -> x * y"

  /** Squares a value, the function the fixture names `x -> x * x`. */
  val square: Double => Double = value => value * value

  /** Adds a value to an accumulator, the function the fixture names `(acc, v) -> acc + v`. */
  val add: (Double, Double) => Double = (accumulated, value) => accumulated + value

  /** Multiplies two values, the function the fixture names `(x, y) -> x * y`. */
  val multiply: (Double, Double) => Double = (left, right) => left * right

  //-------------------------------------------------------------------------
  // The fixture model.
  //-------------------------------------------------------------------------

  /**
   * One operation of one fixture row.
   *
   * `op` names the operation and `result` or `error` carries its expectation - exactly one of
   * the two is present. Every remaining field is a parameter the operation took, and is present
   * only for the operations that take it; each is read as required where it applies, so a
   * fixture that dropped one is reported as a malformed document rather than evaluated with a
   * substituted default.
   *
   * The expectation stays as undecoded JSON here on purpose: the shape it decodes to is fixed by
   * the operation's name, so it is decoded during preparation, in the shape that name demands,
   * and never through the codecs of the types being measured.
   */
  final case class OpEntry(
      op: String,
      result: Option[Json],
      error: Option[String],
      mapFn: Option[String],
      reduceFn: Option[String],
      identity: Option[Double],
      fromIndex: Option[Int],
      index: Option[Int],
      value: Option[Double],
      row: Option[Int],
      column: Option[Int],
      scalar: Option[Double],
      combineFn: Option[String])

  /**
   * One row of the fixture: the inputs, and the results every operation produced from them.
   *
   * `a` and `b` always have the same length and may both be empty. `matrixA` and `matrixB` are
   * absent for a row that has no matrix inputs, and `matrixB` is never present without
   * `matrixA`; those are the only optional inputs, so the operations a row carries follow from
   * them alone.
   */
  final case class Row(
      source: String,
      a: Vector[Double],
      b: Vector[Double],
      scalar: Double,
      matrixA: Option[Vector[Vector[Double]]],
      matrixB: Option[Vector[Vector[Double]]],
      results: Vector[OpEntry])

  //-------------------------------------------------------------------------
  // Decoding.
  //
  // The single tagged-double decoder below is the whole of the non-finite policy on this path.
  // Being declared here, in lexical scope, it outranks the decoder the JSON library publishes
  // for `Double` in its own companion, so every double of the document - an input, an operation
  // parameter or an expectation - accepts a number or exactly one of the three tags "NaN",
  // "Infinity" and "-Infinity" and rejects everything else. The two collection decoders are
  // built from it explicitly rather than summoned, which is also what makes it visibly used.
  //
  // Both row decoders are derived at compile time by the library's semi-automatic derivation,
  // so nothing on this path reflects at run time.
  //-------------------------------------------------------------------------

  implicit val doubleDecoder: Decoder[Double] = Codecs.taggedDouble

  implicit val doublesDecoder: Decoder[Vector[Double]] = Decoder.decodeVector(doubleDecoder)

  implicit val matrixDecoder: Decoder[Vector[Vector[Double]]] = Decoder.decodeVector(doublesDecoder)

  implicit val opEntryDecoder: Decoder[OpEntry] = deriveDecoder[OpEntry]

  implicit val rowDecoder: Decoder[Row] = deriveDecoder[Row]

  //-------------------------------------------------------------------------
  // The three shapes an expectation and an observation can take, plus the fourth outcome of a
  // failure. Keeping the expectation and the observation in separate hierarchies is what lets
  // the comparison pair a captured `Vector[Double]` against a `DoubleArray` without either side
  // having to be converted into the other first.
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

    /** A failure, rendered by the capture as `SimpleClassName: message`. */
    final case class Thrown(text: String) extends Expectation
  }

  /** What the Scala port produced for the same operation. */
  sealed trait Observed

  object Observed {

    /** A single number. */
    final case class Value(value: Double) extends Observed

    /** An array. */
    final case class Elements(values: DoubleArray) extends Observed

    /** A matrix. */
    final case class Rows(values: DoubleMatrix) extends Observed

    /** A failure, rendered as `SimpleClassName: message` to match the captured form. */
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

  /** One operation of one row, ready to be measured. */
  final case class PreparedCase(source: String, op: String, expectation: Expectation, computation: Computation)

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
   * compared and one per failure expectation, so their sum is far larger than the number of
   * rows. `failures` holds the first [[FailureReportLimit]] failures only, which keeps a
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
  // Turning a decoded row into prepared cases is the point at which a document that this spec
  // cannot evaluate is rejected outright. An unknown operation, a missing parameter, a missing
  // expectation, an expectation of the wrong shape and a changed function text are all reported
  // as malformed-document errors rather than as parity failures: they say that the fixture and
  // this spec no longer agree on what is being measured, which is a different fact from the
  // port disagreeing with Java, and conflating the two would make the gate's verdict useless.
  //-------------------------------------------------------------------------

  /** Prepares every operation of every row, or reports the first disagreement with the schema. */
  def prepareAll(rows: Vector[Row]): Either[String, Vector[PreparedCase]] =
    rows.traverse(prepare).map(_.flatten)

  /**
   * Prepares every operation of one row.
   *
   * The two arrays and the up-to-two matrices are built once for the row and shared by its
   * operations, which is both what the capture did and what keeps the inputs of the row's
   * operations identical to one another.
   */
  def prepare(row: Row): Either[String, Vector[PreparedCase]] = {
    val a = DoubleArray.copyOf(row.a)
    val b = DoubleArray.copyOf(row.b)
    val matrixA = row.matrixA.map(toMatrix)
    val matrixB = row.matrixB.map(toMatrix)
    row.results.traverse(entry => prepareEntry(row, a, b, matrixA, matrixB, entry))
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

  /**
   * Prepares one operation, pairing its captured expectation with the call that reproduces it.
   *
   * The operand of each operation is the operand the capture used, which the name alone does not
   * always reveal: `plus` and `minus` take the second array, `multipliedBy` and `dividedBy` take
   * the row's scalar, `matrixMultipliedBy` is a scalar multiply, and the second matrix is used
   * only by the three paired matrix operations.
   */
  def prepareEntry(
      row: Row,
      a: DoubleArray,
      b: DoubleArray,
      matrixA: Option[DoubleMatrix],
      matrixB: Option[DoubleMatrix],
      entry: OpEntry): Either[String, PreparedCase] = {

    val source = row.source
    val op = entry.op

    def valueCase(computation: => Double): Either[String, PreparedCase] =
      expectedValue(row, entry)
        .map(expectation => PreparedCase(source, op, expectation, Computation.OfValue(() => computation)))

    def elementsCase(computation: => DoubleArray): Either[String, PreparedCase] =
      expectedElements(row, entry)
        .map(expectation => PreparedCase(source, op, expectation, Computation.OfElements(() => computation)))

    def rowsCase(computation: => DoubleMatrix): Either[String, PreparedCase] =
      expectedRows(row, entry)
        .map(expectation => PreparedCase(source, op, expectation, Computation.OfRows(() => computation)))

    def firstMatrix: Either[String, DoubleMatrix] = requireMatrix(matrixA, source, op, "matrixA")

    def secondMatrix: Either[String, DoubleMatrix] = requireMatrix(matrixB, source, op, "matrixB")

    op match {
      case "plus" => elementsCase(a.plus(b))
      case "minus" => elementsCase(a.minus(b))
      case "multipliedBy" => elementsCase(a.multipliedBy(row.scalar))
      case "dividedBy" => elementsCase(a.dividedBy(row.scalar))
      case "map" =>
        requireFunction(entry.mapFn, MapFunctionText, source, op, "mapFn")
          .flatMap(_ => elementsCase(a.map(square)))
      case "reduce" =>
        for {
          _ <- requireFunction(entry.reduceFn, ReduceFunctionText, source, op, "reduceFn")
          start <- required(entry.identity, source, op, "identity")
          prepared <- valueCase(a.reduce(start, add))
        } yield prepared
      case "sum" => valueCase(a.sum)
      case "min" => valueCase(a.min)
      case "max" => valueCase(a.max)
      case "sorted" => elementsCase(a.sorted)
      case "concat" => elementsCase(a.concat(b))
      case "subArray" =>
        required(entry.fromIndex, source, op, "fromIndex")
          .flatMap(fromIndex => elementsCase(a.subArray(fromIndex)))
      case "with" =>
        for {
          index <- required(entry.index, source, op, "index")
          value <- required(entry.value, source, op, "value")
          prepared <- elementsCase(a.`with`(index, value))
        } yield prepared
      case "matrixMultipliedBy" =>
        for {
          matrix <- firstMatrix
          factor <- required(entry.scalar, source, op, "scalar")
          prepared <- rowsCase(matrix.multipliedBy(factor))
        } yield prepared
      case "transpose" => firstMatrix.flatMap(matrix => rowsCase(matrix.transpose))
      case "total" => firstMatrix.flatMap(matrix => valueCase(matrix.total))
      case "matrixWith" =>
        for {
          matrix <- firstMatrix
          rowIndex <- required(entry.row, source, op, "row")
          columnIndex <- required(entry.column, source, op, "column")
          value <- required(entry.value, source, op, "value")
          prepared <- rowsCase(matrix.`with`(rowIndex, columnIndex, value))
        } yield prepared
      case "matrixPlus" =>
        for {
          first <- firstMatrix
          second <- secondMatrix
          prepared <- rowsCase(first.plus(second))
        } yield prepared
      case "matrixMinus" =>
        for {
          first <- firstMatrix
          second <- secondMatrix
          prepared <- rowsCase(first.minus(second))
        } yield prepared
      case "matrixCombine" =>
        for {
          _ <- requireFunction(entry.combineFn, CombineFunctionText, source, op, "combineFn")
          first <- firstMatrix
          second <- secondMatrix
          prepared <- rowsCase(first.combine(second, multiply))
        } yield prepared
      case unknown =>
        Left(
          s"fixture row '$source' declares the operation '$unknown', which this spec cannot " +
            s"evaluate; the fixture schema and this spec have to change together")
    }
  }

  //-------------------------------------------------------------------------
  // Reading an expectation.
  //-------------------------------------------------------------------------

  /** Reads the expectation of an operation that returns a number. */
  def expectedValue(row: Row, entry: OpEntry): Either[String, Expectation] =
    expectation(row, entry, "a number")(_.as[Double])(value => Expectation.Value(value))

  /** Reads the expectation of an operation that returns an array. */
  def expectedElements(row: Row, entry: OpEntry): Either[String, Expectation] =
    expectation(row, entry, "an array of numbers")(_.as[Vector[Double]])(values =>
      Expectation.Elements(values))

  /** Reads the expectation of an operation that returns a matrix. */
  def expectedRows(row: Row, entry: OpEntry): Either[String, Expectation] =
    expectation(row, entry, "an array of row arrays")(_.as[Vector[Vector[Double]]])(values =>
      Expectation.Rows(values))

  /**
   * Reads the expectation of one operation in the shape its name demands.
   *
   * A recorded failure is an expectation in its own right and is taken as it stands. Otherwise
   * the result must be present and must decode to the shape the operation returns; either
   * absence or a shape the operation never produces means the fixture is no longer the document
   * this spec was written against.
   */
  def expectation[A](row: Row, entry: OpEntry, description: String)(
      decode: Json => Decoder.Result[A])(wrap: A => Expectation): Either[String, Expectation] =
    entry.error match {
      case Some(text) => Right(Expectation.Thrown(text))
      case None =>
        required(entry.result, row.source, entry.op, "result").flatMap(json =>
          decode(json).bimap(
            failure =>
              s"fixture row '${row.source}' operation '${entry.op}' should carry $description " +
                s"but its result did not decode: ${failure.message}",
            wrap))
    }

  //-------------------------------------------------------------------------
  // Schema checks used during preparation.
  //-------------------------------------------------------------------------

  /** Reads a field the operation needs, reporting its absence as a malformed document. */
  def required[A](value: Option[A], source: String, op: String, field: String): Either[String, A] =
    value.toRight(
      s"fixture row '$source' operation '$op' is missing the field '$field', which that " +
        s"operation needs")

  /** Reads a matrix the operation needs, reporting its absence as a malformed document. */
  def requireMatrix(
      matrix: Option[DoubleMatrix],
      source: String,
      op: String,
      field: String): Either[String, DoubleMatrix] =
    matrix.toRight(s"fixture row '$source' declares the operation '$op' but carries no '$field'")

  /**
   * Confirms that the function the fixture names is the function this spec implements.
   *
   * This is the check that stops a silent change of the captured function from turning the
   * measurement into a comparison of two different quantities that happens to be green.
   */
  def requireFunction(
      text: Option[String],
      expectedText: String,
      source: String,
      op: String,
      field: String): Either[String, Unit] =
    required(text, source, op, field).flatMap(actual =>
      if (actual == expectedText) {
        Right(())
      } else {
        Left(
          s"fixture row '$source' operation '$op' names the function '$actual' in '$field' " +
            s"where this spec implements '$expectedText'")
      })

  //-------------------------------------------------------------------------
  // Evaluation.
  //-------------------------------------------------------------------------

  /** Measures every prepared case, folding the outcomes into one immutable result. */
  def evaluate(cases: Vector[PreparedCase]): Tally =
    cases.foldLeft(Tally.empty)((tally, prepared) =>
      check(prepared, observe(prepared.computation), tally))

  /**
   * Calls into the port and records what came back, including a failure.
   *
   * A failure is an outcome here rather than an error, both because four of the fixture's
   * entries expect one and because an exception allowed to escape would prevent the report from
   * being written - which is exactly the diagnostic a failing run needs most.
   */
  def observe(computation: Computation): Observed = {
    val attempted = computation match {
      case Computation.OfValue(run) => Try(Observed.Value(run()): Observed)
      case Computation.OfElements(run) => Try(Observed.Elements(run()): Observed)
      case Computation.OfRows(run) => Try(Observed.Rows(run()): Observed)
    }
    attempted match {
      case Success(observed) => observed
      case Failure(thrown) => Observed.Thrown(describeThrowable(thrown))
    }
  }

  /** Renders a failure in the `SimpleClassName: message` form the capture writes. */
  def describeThrowable(thrown: Throwable): String =
    s"${thrown.getClass.getSimpleName}: ${Option(thrown.getMessage).getOrElse("")}"

  /**
   * Removes the leading `SimpleClassName: ` of a rendered failure, leaving the message.
   *
   * The exception type is not part of the parity contract. Three of the fixture's four failure
   * expectations name a type the port deliberately does not raise: it routes the `min`, `max`
   * and `subArray` preconditions through its own argument check, which reports an illegal
   * argument where the Java original reported an illegal state or an index out of bounds, and
   * keeps the message unchanged. Comparing the messages therefore holds the port to the
   * behaviour that was ported while allowing the divergence that was intended.
   */
  def errorMessage(text: String): String = {
    val separator = text.indexOf(": ")
    if (separator < 0) text else text.substring(separator + 2)
  }

  /**
   * Compares one observation against its expectation, adding its checks to the running result.
   *
   * The pairing of the two shapes is established during preparation, so the final case cannot
   * arise from a well-formed fixture; it is present because a total match is worth more than a
   * pattern the compiler cannot verify.
   */
  def check(prepared: PreparedCase, observed: Observed, tally: Tally): Tally =
    (prepared.expectation, observed) match {
      case (Expectation.Thrown(expected), Observed.Thrown(actual)) =>
        if (errorMessage(expected) == errorMessage(actual)) {
          tally.pass
        } else {
          tally.record(
            descriptiveFailure(
              prepared,
              "errorMessage",
              expected,
              actual,
              "the port failed where Java failed, but reported a different message"))
        }
      case (Expectation.Thrown(expected), other) =>
        tally.record(
          descriptiveFailure(
            prepared,
            "expectedFailure",
            expected,
            describeObserved(other),
            "Java failed on this operation but the port produced a value"))
      case (expected, Observed.Thrown(actual)) =>
        tally.record(
          descriptiveFailure(
            prepared,
            "evaluation",
            describeExpectation(expected),
            actual,
            "the port failed on an operation Java completed"))
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
   * `transpose` changes them: the fixture carries a one-by-six matrix whose transpose is
   * six-by-one, so measuring against the input would be wrong for exactly the operation most
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
  // Every entry names the row it came from, the operation, the kind of check and, where the
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

  /** The fields that attribute a failure to a row and an operation. */
  def attribution(prepared: PreparedCase): Vector[(String, Json)] =
    Vector("source" -> Json.fromString(prepared.source), "op" -> Json.fromString(prepared.op))

  /** Encodes a number under the port's single policy for a value JSON cannot express. */
  def encodeDouble(value: Double): Json = Codecs.taggedDouble(value)

  /** Renders an expectation for a message. */
  def describeExpectation(expectation: Expectation): String = expectation match {
    case Expectation.Value(value) => value.toString
    case Expectation.Elements(values) => values.mkString("[", ", ", "]")
    case Expectation.Rows(values) => values.map(_.mkString("[", ", ", "]")).mkString("[", ", ", "]")
    case Expectation.Thrown(text) => text
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

  /** Decodes the whole document, reporting a malformed one as a single message. */
  def decodeFixture(text: String): Either[String, Vector[Row]] =
    parser
      .decode[Vector[Row]](text)
      .leftMap(error =>
        s"the parity fixture '$FixtureResource' did not decode: " +
          Option(error.getMessage).getOrElse(error.toString))

  /**
   * Lifts a malformed-document report into a failed effect.
   *
   * This is the one place where a value in the error channel becomes a thrown error, and it is
   * at the edge of the spec by design. A fixture that cannot be read or understood is not a
   * parity result of any kind, so it fails the run outright rather than being counted.
   */
  def lift[A](result: Either[String, A]): IO[A] =
    IO.fromEither(result.leftMap(message => new IllegalStateException(message)))

  /** The directory the report is written to. */
  def reportDirectory: Path =
    Paths.get(sys.props.get(ReportDirectoryProperty).filter(_.nonEmpty).getOrElse(DefaultReportDirectory))

  /** The report document itself. */
  def reportFile: Path = reportDirectory.resolve(ReportFileName)

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
  def writeReport(tally: Tally, rows: Int): IO[Path] = IO.blocking {
    val directory = reportDirectory
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
  }
}
