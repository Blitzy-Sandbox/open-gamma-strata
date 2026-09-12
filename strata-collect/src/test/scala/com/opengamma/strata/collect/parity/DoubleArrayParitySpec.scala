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
import io.circe.DecodingFailure
import io.circe.HCursor
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
 * ===What the report counts, and why it counts the same as the other five===
 *
 * `rows` is the number of fixture rows evaluated, `passed` the number of those rows that matched
 * the baseline in '''every''' respect, and `failed` the number of discrepancies found over all of
 * them. That is the convention the five `strata-basics` reports publish under - its authoritative
 * statement is the scaladoc of `com.opengamma.strata.basics.parity.ParityReport` - and it is
 * restated here, with the invariants that enforce it, because Gate 3 prints all six reports in one
 * table and a reader comparing two of its lines has to be comparing the same quantity. Neither
 * that class nor `ParityHarness` can be imported from this module: the build's dependency edge
 * runs from `strata-basics` to `strata-collect` and never the other way. See [[ParityReport]] and
 * [[DoubleArrayParitySpec.contractViolations]].
 *
 * The number of individual checks the run executed - between forty and a hundred and eighty-eight
 * per row of this fixture, against one number for a day-count row - is not comparable between
 * fixtures and is not a key of the report. It is asserted instead, against the count
 * [[DoubleArrayParitySpec.expectedChecks]] derives from the decoded rows, which is what
 * distinguishes a clean measurement of the whole fixture from a clean measurement of nothing.
 *
 * ===The report survives a failing run===
 *
 * Reading the document, decoding it, building the operands and comparing them are all attempted,
 * and a failure of any of them is published as a report of one discrepancy over the rows that were
 * decoded rather than allowed to end the run. A gate that finds no report cannot tell it apart
 * from a measurement that was never made, so the one outcome a failing parity run must not have is
 * no artifact at all.
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
      // The whole path - read, decode, prepare, evaluate - is attempted, so that a failure in any
      // of its stages becomes the content of a report rather than an escape that would leave the
      // gate with no artifact at all.
      attempted <- attemptMeasurement
      report = attempted.fold(refusalReport, measured => measured.measurement.report)
      // Published before anything below is asserted, on both outcomes.
      path <- writeReport(report)
    } yield withClue(
      s"parity report written to $path; ${report.summary}${report.firstFailureSummary}: ") {
      // The counts are held to the convention the other five reports publish under before they
      // are read as anything, which is what makes Gate 3's table comparable across the modules.
      contractViolations(report) shouldBe empty
      attempted match {
        case Left(refusal) =>
          fail(
            s"the parity fixture could not be measured; the refusal was published to $path as " +
              s"one discrepancy over ${refusal.rowsDecoded} decoded rows: ${refusal.reason}")
        case Right(measured) =>
          report.failed shouldBe 0
          report.rows shouldBe measured.rows.size
          report.passed shouldBe measured.rows.size
          // And the run must be shown to have done the work: `failed == 0` is also what a run
          // that measured nothing would report, so the number of checks executed is compared with
          // the number the decoded shapes require, and that number with the floor the committed
          // fixture yields.
          val required = expectedChecks(measured.rows)
          withClue(
            s"${measured.measurement.checksExecuted} checks executed, $required required by the " +
              s"${measured.rows.size} decoded rows, floor $CommittedFixtureChecks: ") {
            required should be >= CommittedFixtureChecks
            measured.measurement.checksExecuted shouldBe required
          }
      }
    }
  }

  test("the evaluator counts one check per comparison and attributes every discrepancy") {
    IO {
      // Hand-built cases, so that the count and the outcome of each are known independently of
      // the fixture: this is what pins the counting the expected-work assertion above relies on.
      val matching = evaluate(Vector(valueCase("sum", 2.5d, () => 2.5d)))
      withClue(s"a matching value: ${matching.failures.map(_.noSpaces)}: ") {
        matching.checksExecuted shouldBe 1
        matching.checksPassed shouldBe 1
        matching.checksFailed shouldBe 0
        matching.matchedEverything shouldBe true
        matching.failures shouldBe empty
      }

      // One mismatched element is one failing check among the row's passing ones, and the entry
      // carries the index, without which a failure in a long array cannot be acted on.
      val mismatched =
        evaluate(
          Vector(
            elementsCase("plusArray", Vector(1d, 2d, 3d), () => DoubleArray.of(1d, 2d, 4d))))
      withClue(s"a mismatched element: ${mismatched.failures.map(_.noSpaces)}: ") {
        mismatched.checksExecuted shouldBe 4
        mismatched.checksFailed shouldBe 1
        mismatched.failures should have size 1L
        field[Int](mismatched.failures.head, "index") shouldBe Some(2)
        field[String](mismatched.failures.head, "check") shouldBe Some("element")
        field[String](mismatched.failures.head, "expectation") shouldBe Some("plusArray")
        field[Double](mismatched.failures.head, "expected") shouldBe Some(3d)
        field[Double](mismatched.failures.head, "actual") shouldBe Some(4d)
      }

      // A shape difference ends that operation's comparison: one failing check and no element
      // check at all, because indexing into an array of another length would raise from the
      // harness itself.
      val reshaped =
        evaluate(Vector(elementsCase("sorted", Vector(1d, 2d, 3d), () => DoubleArray.of(1d, 2d))))
      withClue(s"a shape difference: ${reshaped.failures.map(_.noSpaces)}: ") {
        reshaped.checksExecuted shouldBe 1
        reshaped.checksFailed shouldBe 1
        reshaped.failures should have size 1L
        field[String](reshaped.failures.head, "check") shouldBe Some("shape")
      }

      // A matrix whose dimensions differ behaves the same way, which is the case `matrixTranspose`
      // would hit if the port transposed the wrong axis.
      val reshapedMatrix =
        evaluate(
          Vector(
            rowsCase(
              "matrixTranspose",
              Vector(Vector(1d, 2d), Vector(3d, 4d), Vector(5d, 6d)),
              () => DoubleMatrix.copyOf(Array(Array(1d, 2d, 3d), Array(4d, 5d, 6d))))))
      withClue(s"a matrix shape difference: ${reshapedMatrix.failures.map(_.noSpaces)}: ") {
        reshapedMatrix.checksExecuted shouldBe 1
        reshapedMatrix.checksFailed shouldBe 1
        field[String](reshapedMatrix.failures.head, "check") shouldBe Some("shape")
        field[String](reshapedMatrix.failures.head, "expected") shouldBe Some("3 x 2")
        field[String](reshapedMatrix.failures.head, "actual") shouldBe Some("2 x 3")
      }

      // And an error raised by the port is one failing check that does not escape, because an
      // escaping error is exactly what would cost the run its report.
      val thrown =
        evaluate(
          Vector(valueCase("min", 1d, () => throw new IllegalStateException("the array is empty"))))
      withClue(s"a computation that throws: ${thrown.failures.map(_.noSpaces)}: ") {
        thrown.checksExecuted shouldBe 1
        thrown.checksFailed shouldBe 1
        thrown.failures should have size 1L
        field[String](thrown.failures.head, "check") shouldBe Some("evaluation")
        field[String](thrown.failures.head, "actual") shouldBe
          Some("IllegalStateException: the array is empty")
      }
      succeed
    }
  }

  test("the report counts rows and discrepancies the way all six parity reports do") {
    IO {
      // Two rows, one of them clean: `passed` counts rows and `failed` counts discrepancies, so
      // this is one passed row and two discrepancies rather than three passed checks and two
      // failed ones. The distinction is the whole of the cross-module contract.
      val clean = Tally.empty.pass.pass.pass
      val differing = Tally.empty.pass.record(discrepancy("first")).record(discrepancy("second"))
      val measurement = Measurement.empty.add(clean).add(differing)
      measurement.rows shouldBe 2
      // Three checks in the clean row and three in the differing one, of which two failed.
      measurement.checksExecuted shouldBe 6
      measurement.passedRows shouldBe 1
      measurement.discrepancies shouldBe 2
      val report = measurement.report
      report.fixture shouldBe FixtureName
      report.rows shouldBe 2
      report.passed shouldBe 1
      report.failed shouldBe 2
      report.failures should have size 2L
      contractViolations(report) shouldBe empty

      // The document the gate parses is exactly the five keys, in this order, and the
      // executed-check count of the assertion above is deliberately not among them.
      report.document.asObject.map(_.keys.toVector) shouldBe
        Some(Vector("fixture", "rows", "passed", "failed", "failures"))

      // A capped list still reports the true total, and saying so is an entry of the list.
      val many =
        (1 to FailureReportLimit + 50)
          .foldLeft(Tally.empty)((tally, index) => tally.record(discrepancy(s"failure $index")))
      val capped = Measurement.empty.add(many).report
      capped.failed shouldBe FailureReportLimit + 50
      capped.failures should have size (FailureReportLimit + 1L)
      field[Boolean](capped.failures.last, "truncated") shouldBe Some(true)
      field[Int](capped.failures.last, "omittedFailures") shouldBe Some(50)
      contractViolations(capped) shouldBe empty

      // The four invariants reject the four ways a report can contradict the convention, and the
      // clean case is the one shape that is a pass.
      contractViolations(ParityReport(FixtureName, 43, 43, 0, Vector.empty)) shouldBe empty
      withClue("a run that measured nothing: ") {
        contractViolations(ParityReport(FixtureName, 0, 0, 0, Vector.empty)) should not be empty
      }
      withClue("a clean report that did not pass every row: ") {
        contractViolations(ParityReport(FixtureName, 43, 42, 0, Vector.empty)) should not be empty
      }
      withClue("counts out of range: ") {
        contractViolations(ParityReport(FixtureName, 43, 44, 0, Vector.empty)) should not be empty
        contractViolations(ParityReport(FixtureName, 43, 43, -1, Vector.empty)) should not be empty
      }
      withClue("a discrepancy that is counted but not described: ") {
        contractViolations(ParityReport(FixtureName, 43, 40, 3, Vector.empty)) should not be empty
        contractViolations(
          ParityReport(FixtureName, 43, 40, 1, Vector(discrepancy("a"), discrepancy("b")))) should
          not be empty
      }
      withClue("a discrepancy attributed to a run in which every row passed: ") {
        contractViolations(ParityReport(FixtureName, 43, 43, 1, Vector(discrepancy("a")))) should
          not be empty
      }
      // And the two shapes a refusal takes are admitted, because neither is a pass and both are
      // worth publishing: a fixture that could not be read, and rows that could not be measured.
      withClue("the report of a refusal: ") {
        contractViolations(refusalReport(Refusal(0, "NoSuchFileException: absent"))) shouldBe empty
        contractViolations(
          refusalReport(Refusal(43, "ArrayIndexOutOfBoundsException: ragged"))) shouldBe empty
      }
      succeed
    }
  }

  test("a fixture that cannot be read is still reported, as a refusal rather than a pass") {
    // The durability guarantee, driven through the production path with a resource that is not
    // there: nothing about the failure escapes, a report is published, and what it publishes is
    // one discrepancy over no measured rows. A gate that found no report at all could not tell
    // this apart from a measurement that was never made, which is the whole reason it exists.
    for {
      attempted <- attemptMeasurementOf(loadFixtureFrom(AbsentFixtureResource))
      report = attempted.fold(refusalReport, measured => measured.measurement.report)
      document <- publishToTemporaryDirectory(report)
    } yield withClue(s"the refusal published as ${document.noSpaces}: ") {
      attempted.isLeft shouldBe true
      attempted.left.toOption.map(_.rowsDecoded) shouldBe Some(0)
      document.asObject.map(_.keys.toVector) shouldBe
        Some(Vector("fixture", "rows", "passed", "failed", "failures"))
      field[Int](document, "rows") shouldBe Some(0)
      field[Int](document, "passed") shouldBe Some(0)
      field[Int](document, "failed") shouldBe Some(1)
      report.failures should have size 1L
      text(report.failures.head, "id") shouldBe s"$FixtureName:fixture"
      text(report.failures.head, "message") should include(AbsentFixtureResource)
      contractViolations(report) shouldBe empty
    }
  }

  test("rows that cannot be measured are still reported, and the report says how many there were") {
    // The other refusal: the document was read and decoded, so the report names the rows it held.
    // The rows are the committed ones with a matrix made ragged by [[ragged]]. The port raising
    // inside one of the operations is a discrepancy rather than an escape - `matrixPlus` on such a
    // row is one - but a matrix that misstates its shape is also read by the comparison, and the
    // raise from there is a genuine escape: it is what would end the run with no artifact written
    // at all, which is the one outcome a failing parity run must not have.
    for {
      attempted <- attemptMeasurementOf(loadFixture.map(rows => rows.map(ragged)))
      report = attempted.fold(refusalReport, measured => measured.measurement.report)
      document <- publishToTemporaryDirectory(report)
    } yield withClue(s"the refusal published as ${document.noSpaces}: ") {
      attempted.isLeft shouldBe true
      report.rows should be >= MinimumRows
      report.passed shouldBe 0
      report.failed shouldBe 1
      field[Int](document, "rows") shouldBe Some(report.rows)
      text(report.failures.head, "message") should include(s"${report.rows} rows was measured")
      contractViolations(report) shouldBe empty
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
          // And the row must contribute every expectation to the measurement, in the documented
          // order: an expectation dropped from the model would reduce the work the run does
          // without reducing anything the gate reads, which is the same silence the
          // executed-check assertion of the measurement above is there to break.
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

  test("the committed fixture carries exactly the thirty-three declared row keys") {
    // The producer's inventory and the consumer's are tied together here. The capture writes the
    // thirty-three keys of `RowSchema`; this spec measures the twenty-two expectations and eleven
    // inputs that schema is built from; and a key on one side that is not on the other is a
    // measurement that has quietly stopped happening. So the document's own keys are compared with
    // the declared set for equality, rather than the decoder being trusted to have read them all.
    for {
      text <- Resources.readClasspathText(FixtureResource)
      keySets <- lift(rowKeySets(text))
      rows <- loadFixture
    } yield withClue(s"declared row keys: ${RowSchema.describe}: ") {
      InputNames should have size 11L
      ExpectationNames should have size 22L
      RowSchema.keys should have size (InputNames.size + ExpectationNames.size).toLong
      keySets should not be empty
      keySets.distinct should have size 1L
      keySets.foreach { present =>
        withClue(s"unknown ${RowSchema.unknown(present)}, missing ${RowSchema.missing(present)}: ") {
          present shouldBe RowSchema.keys
        }
      }
      // Every declared key is therefore present in every row, which is what makes the row model's
      // fields all required and the decoded rows all fully populated.
      rows should have size keySets.size.toLong
      succeed
    }
  }

  test("the row decoder refuses a row whose keys are not the documented ones") {
    for {
      text <- Resources.readClasspathText(FixtureResource)
      objects <- lift(rowObjects(text))
    } yield {
      val encoded = objects.head
      withClue(s"the first captured row, re-read as ${encoded.noSpaces.take(120)}...: ") {
        // The row the fixture holds decodes, so the refusals below are about the keys and nothing
        // else. Each mutation is applied to that same row, in the position the document has it, so
        // the refusal carries the row's index in the document as well as the key at fault.
        Decoder[Row].decodeJson(encoded).isRight shouldBe true

        val gained = withKey(encoded, "medianOfSomething", Json.fromDoubleOrNull(1d))
        decodeRows(Vector(encoded, gained)) match {
          case Left(message) =>
            message should include("medianOfSomething")
            message should include("unknown keys")
            message should include("[1]")
          case Right(decoded) => fail(s"a row with a gained key decoded as ${decoded.size} rows")
        }

        val lost = withoutKey(encoded, "matrixTranspose")
        decodeRows(Vector(lost)) match {
          case Left(message) =>
            message should include("is missing {matrixTranspose}")
            message should include("[0]")
          case Right(decoded) => fail(s"a row with a lost key decoded as ${decoded.size} rows")
        }

        // A renamed key is both at once, and is the case a derived decoder would have reported as
        // a missing field alone, saying nothing about the name the capture now writes.
        val renamed = withKey(withoutKey(encoded, "sorted"), "sortedAscending", Json.arr())
        decodeRows(Vector(renamed)) match {
          case Left(message) =>
            message should include("unknown keys {sortedAscending}")
            message should include("is missing {sorted}")
          case Right(decoded) => fail(s"a row with a renamed key decoded as ${decoded.size} rows")
        }

        // And a row that has stopped being an object at all is reported as that, rather than as a
        // field that failed to decode.
        decodeRows(Vector(Json.fromString("a row"))) match {
          case Left(message) => message should include("expected a JSON object")
          case Right(decoded) => fail(s"a row that is a string decoded as ${decoded.size} rows")
        }
        succeed
      }
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

  //-------------------------------------------------------------------------
  // The hand-built material of the evaluator, report and durability checks above. Each of these
  // exists so that a count or an outcome is known without reference to the fixture: a test that
  // took its expectations from the same document the measurement reads could not tell a correct
  // count from a consistent one.
  //-------------------------------------------------------------------------

  /** One prepared case expecting a number, with the call that produces it. */
  private def valueCase(expectation: String, expected: Double, run: () => Double): PreparedCase =
    PreparedCase("hand-built", expectation, Expectation.Value(expected), Computation.OfValue(run))

  /** One prepared case expecting an array, with the call that produces it. */
  private def elementsCase(
      expectation: String,
      expected: Vector[Double],
      run: () => DoubleArray): PreparedCase =
    PreparedCase(
      "hand-built",
      expectation,
      Expectation.Elements(expected),
      Computation.OfElements(run))

  /** One prepared case expecting a matrix, with the call that produces it. */
  private def rowsCase(
      expectation: String,
      expected: Vector[Vector[Double]],
      run: () => DoubleMatrix): PreparedCase =
    PreparedCase("hand-built", expectation, Expectation.Rows(expected), Computation.OfRows(run))

  /** Decodes hand-made rows through the production decode path, so its refusals are the real ones. */
  private def decodeRows(values: Vector[Json]): Either[String, Vector[Row]] =
    decodeFixture(Json.fromValues(values).noSpaces)

  /** The same row object with one key added, which is what a fixture that gained a field looks like. */
  private def withKey(row: Json, key: String, value: Json): Json =
    row.mapObject(fields => fields.add(key, value))

  /** The same row object with one key removed. */
  private def withoutKey(row: Json, key: String): Json =
    row.mapObject(fields => fields.remove(key))

  /** A stand-in failure entry for the counting checks, which are about numbers and not text. */
  private def discrepancy(message: String): Json =
    Json.obj(
      "id" -> Json.fromString("hand-built"),
      "message" -> Json.fromString(message))

  /** One field of a JSON object, absent when the key is missing or of another type. */
  private def field[A: Decoder](json: Json, key: String): Option[A] =
    json.hcursor.get[A](key).toOption

  /** One textual field, rendered so that a missing key fails an assertion rather than a match. */
  private def text(json: Json, key: String): String =
    field[String](json, key).getOrElse(s"<no '$key' in ${json.noSpaces}>")

  /**
   * The same row with the last row of `matrixA` one element short.
   *
   * `DoubleMatrix.copyOf` is total, exactly as the Java factory it is a port of: it takes the
   * column count from the first row and clones the rest as they stand, so a ragged array is
   * accepted and the matrix built from it misstates its own shape. Preparing such a row therefore
   * succeeds, and it is the measurement that cannot be carried out - reading the position the
   * short row does not hold raises from the comparison itself, which is outside the deferred call
   * [[observe]] guards. That is how a row that cannot be measured is obtained from the committed
   * fixture rather than hand-written: it drives the real refusal path of [[attemptMeasurementOf]]
   * over real rows, rather than a simulated one.
   */
  private def ragged(row: Row): Row =
    if (row.matrixA.isEmpty) {
      row
    } else {
      row.copy(matrixA = row.matrixA.init :+ row.matrixA.last.dropRight(1))
    }

  /**
   * Publishes a report to a directory of this suite's own and reads the document back.
   *
   * The production report directory holds the one artifact the gate collects, so a check that
   * wants to see a refusal on disk must not write there. The document is read back from the file
   * rather than taken from the value that was written, because "the report survives a failing run"
   * is a statement about the file. The directory is removed on every outcome.
   *
   * @param report  the report to publish
   * @return the document as it was read back from disk
   */
  private def publishToTemporaryDirectory(report: ParityReport): IO[Json] =
    IO.blocking(Files.createTempDirectory("double-array-parity-report")).bracket { directory =>
      for {
        path <- writeReportTo(directory, report)
        written <- IO.blocking(new String(Files.readAllBytes(path), StandardCharsets.UTF_8))
        parsed <- lift(
          parser
            .parse(written)
            .leftMap(error => s"the published report did not parse back: ${error.getMessage}"))
      } yield parsed
    } { directory =>
      IO.blocking {
        val _ = Files.deleteIfExists(directory.resolve(ReportFileName))
        val _ = Files.deleteIfExists(directory)
      }
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

  /**
   * A classpath name no resource of this module carries, used to drive the refusal path.
   *
   * The durability of the report is a property of the production read, so it is measured by
   * pointing that read at a document that is not there rather than by simulating its failure. The
   * name is kept beside the real one so that the two cannot be confused, and it must stay absent:
   * a resource of this name would turn that check into a measurement of nothing.
   */
  val AbsentFixtureResource: String = "parity/double-array-baseline-absent.json"

  /** The file the report is written to inside the report directory. */
  val ReportFileName: String = "double-array.json"

  /** The system property through which the build supplies the report directory. */
  val ReportDirectoryProperty: String = "parity.report.dir"

  /**
   * The longest fixture document this spec decodes, in characters.
   *
   * Checked before the document is parsed, so that the size of a resource cannot decide the
   * memory of the test process. The committed baseline is 90,821 characters, all of them in the
   * ASCII range and therefore one byte each, so the ceiling is far above any regeneration of it
   * while still being a ceiling.
   */
  val MaxFixtureCharacters: Int = 32 * 1024 * 1024

  /**
   * The largest number of rows this spec measures.
   *
   * Checked on the parsed array before any row model is built. The committed baseline holds
   * forty-three rows, each of the thirty-three keys of [[RowSchema]].
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
   * The number of individual checks the committed baseline yields, which is the floor of the work
   * a run must do.
   *
   * This is not the assertion by itself. [[expectedChecks]] derives the figure from the rows that
   * were actually decoded and the suite requires the run to have executed exactly that many, which
   * is what proves the rows were measured rather than skipped. That equality alone would still be
   * satisfied by a fixture that had been thinned, because both sides of it would shrink together,
   * so the derived figure is also held to this floor: the count the committed document yields,
   * measured from it (forty-three rows, 4,111 checks). A regeneration that adds rows raises the
   * derived figure above the floor and passes; one that removes rows fails here rather than
   * reporting a green measurement of less.
   */
  val CommittedFixtureChecks: Int = 4111

  /**
   * The eleven inputs of a row, which are the operands every expectation was captured from.
   *
   * Named here, beside [[ExpectationNames]], because the two together are the '''declared key
   * set''' of a row: [[RowSchema]] is built from them rather than from a hand-typed list of
   * thirty-three strings, so the schema the decoder enforces cannot drift from the inventory this
   * spec measures.
   */
  val InputNames: Vector[String] =
    Vector(
      "id",
      "a",
      "b",
      "scalar",
      "subArrayFrom",
      "subArrayTo",
      "matrixA",
      "matrixB",
      "withRow",
      "withColumn",
      "withValue")

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

  /**
   * The declared key set of a row: its eleven inputs and its twenty-two expectations.
   *
   * Derived from the two inventories above rather than written out, so the keys the decoder
   * enforces are the keys this spec measures - an expectation added to [[ExpectationNames]] and to
   * the row model is admitted by the schema at the same moment it starts being measured, and an
   * expectation the capture emits that this file does not know is refused by name. The suite also
   * asserts that the committed document's own key set is exactly this one, which is what ties the
   * producer's inventory to the consumer's.
   */
  val RowSchema: KeySchema =
    KeySchema("a double-array parity row", (InputNames ++ ExpectationNames).toSet)

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

    /** The expectations that are one number, each of which is one check. */
    def scalarExpectations: Vector[(String, Double)] =
      Vector(
        "reduceSum" -> reduceSum,
        "sum" -> sum,
        "min" -> min,
        "max" -> max,
        "matrixTotal" -> matrixTotal)

    /**
     * Every matrix expectation with the dimensions it carries, `matrixTranspose` included.
     *
     * The dimensions are the expectation's own, never `matrixA`'s, which is what makes this usable
     * for counting the checks of the one operation that changes the shape.
     */
    def matrixExpectations: Vector[(String, Vector[Vector[Double]])] =
      sameShapeMatrixExpectations :+ ("matrixTranspose" -> matrixTranspose)
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
  // nothing on this path reflects at run time, and it is wrapped in the strict key check of
  // [[strictObject]] so that the keys of a row are the documented ones before any field of it is
  // read.
  //-------------------------------------------------------------------------

  implicit val doubleDecoder: Decoder[Double] = Codecs.taggedDouble

  implicit val doublesDecoder: Decoder[Vector[Double]] = Decoder.decodeVector(doubleDecoder)

  implicit val matrixDecoder: Decoder[Vector[Vector[Double]]] = Decoder.decodeVector(doublesDecoder)

  implicit val rowDecoder: Decoder[Row] = strictObject(RowSchema)(deriveDecoder[Row])

  //-------------------------------------------------------------------------
  // The strict schema layer.
  //
  // This is the rule `ParityHarness.strictKeys` applies to the five fixtures of `strata-basics`,
  // restated here because that file cannot be imported from this module - the build's dependency
  // edge runs from `strata-basics` to `strata-collect` and never the other way - and kept
  // deliberately alike in message content so that a reader of one recognises the other. The
  // schema of this fixture has one shape and no optional key, because the captured document is
  // uniform: every row carries the same thirty-three keys, so satisfying the schema is exact
  // equality of key sets and the variant machinery of the basics harness has nothing to do here.
  //
  // Why a fixture consumer needs it: a derived decoder reads the fields its model declares and
  // ignores every other key of the object it is given. That is the right default for a wire
  // format that evolves and precisely the wrong one for a measurement - a key the capture started
  // emitting, a new expectation or a renamed operand, would be dropped in silence, the rows would
  // decode perfectly, the report would show `failed == 0`, and Gate 3 would publish a pass over a
  // fixture that was no longer being measured in full. The check closes that gap in the one place
  // it can be closed: before the object is decoded, where the keys are still visible. It costs one
  // set comparison per row.
  //-------------------------------------------------------------------------

  /**
   * The keys a row of this fixture carries, and the name it is refused under.
   *
   * @param shape  what is being decoded, for the message of a refusal
   * @param keys  the documented key set, of which a conforming object carries exactly these
   */
  final case class KeySchema(shape: String, keys: Set[String]) {

    require(keys.nonEmpty, s"a key schema names at least one key; '$shape' names none")

    /** Whether an object carrying these keys is the documented shape. */
    def matches(present: Set[String]): Boolean = present == keys

    /** The keys the object carries that the schema does not know, in a stable order. */
    def unknown(present: Set[String]): Vector[String] = (present -- keys).toVector.sorted

    /** The documented keys the object does not carry, in a stable order. */
    def missing(present: Set[String]): Vector[String] = (keys -- present).toVector.sorted

    /** The schema as one line of a refusal message. */
    def describe: String = s"$shape{${keys.toVector.sorted.mkString(", ")}}"
  }

  /**
   * Checks one JSON object against its documented key set.
   *
   * Two refusals are possible and they are different facts. A value that is not an object at all
   * is reported as such, naming what the document has instead: a row that has become a string is
   * a fixture that no longer agrees with this spec, not a field that failed to decode. A key set
   * that is not the documented one is reported with the keys the object carries that the schema
   * does not know, the documented keys it does not carry, and the schema itself, so the message
   * says what to do about it without opening the document beside it. The position in the document
   * comes from the cursor's own history, so the refusal names the row it came from.
   *
   * @param schema  the documented shape
   * @param cursor  the object being decoded
   * @return nothing, when the object is the documented shape; otherwise the refusal
   */
  def strictKeys(schema: KeySchema, cursor: HCursor): Decoder.Result[Unit] =
    cursor.keys match {
      case None =>
        Left(
          DecodingFailure(
            s"expected a JSON object for ${schema.shape}, whose documented shape is " +
              s"${schema.describe}; the document carries ${cursor.value.name} here",
            cursor.history))
      case Some(keys) =>
        val present = keys.toSet
        if (schema.matches(present)) {
          Right(())
        } else {
          Left(
            DecodingFailure(
              s"${schema.shape} carries the keys {${present.toVector.sorted.mkString(", ")}}, " +
                s"which are not the documented keys of ${schema.describe}: unknown keys " +
                s"{${schema.unknown(present).mkString(", ")}}, and it is missing " +
                s"{${schema.missing(present).mkString(", ")}}. A captured object whose keys are " +
                "not the documented ones is a fixture that has stopped agreeing with this spec: " +
                "decoding it anyway would leave whatever the new key carries unmeasured while " +
                "the report still read zero failures",
              cursor.history))
        }
    }

  /**
   * Wraps a decoder so that the object is checked against its documented key set first.
   *
   * @param schema  the documented shape
   * @param decoder  the decoder to apply once the keys are known to be the documented ones
   * @return the strict decoder
   */
  def strictObject[A](schema: KeySchema)(decoder: Decoder[A]): Decoder[A] =
    Decoder.instance(cursor => strictKeys(schema, cursor).flatMap(_ => decoder(cursor)))

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
   * The outcome of measuring '''one row''', threaded through its evaluation as an immutable value.
   *
   * `checksPassed` and `checksFailed` count individual checks - one per number compared, one per
   * shape compared and one per unexpected error - so a single row of this fixture contributes
   * between forty and a hundred and eighty-eight of them, ninety-six on average. Neither number is
   * published: the report publishes rows and discrepancies (see [[ParityReport]]), and the count
   * of executed checks is asserted by the suite against [[expectedChecks]] instead, which is what
   * makes "the fixture was measured, not skipped" checkable without adding a sixth key the gate
   * does not read.
   *
   * A failing check is one '''discrepancy''', so `checksFailed` is the row's discrepancy count.
   * `failures` retains the first [[FailureReportLimit]] of the row's descriptions, which bounds
   * what a systematically broken port can accumulate, while `checksFailed` remains the true
   * total.
   */
  final case class Tally(checksPassed: Int, checksFailed: Int, failures: Vector[Json]) {

    /** Records one check that met the tolerance rule. */
    def pass: Tally = copy(checksPassed = checksPassed + 1)

    /** Records one check that did not, retaining its description up to the reporting limit. */
    def record(failure: Json): Tally =
      copy(
        checksFailed = checksFailed + 1,
        failures = if (failures.size < FailureReportLimit) failures :+ failure else failures)

    /** The number of checks this row executed, which is the evidence that it was measured. */
    def checksExecuted: Int = checksPassed + checksFailed

    /** Whether the row matched the Java baseline in every respect. */
    def matchedEverything: Boolean = checksFailed == 0
  }

  object Tally {

    /** No checks run. */
    val empty: Tally = Tally(0, 0, Vector.empty)
  }

  /**
   * The outcome of measuring the whole fixture, row by row.
   *
   * Rows are folded in one at a time, which is what makes the row-based counts of
   * [[ParityReport]] available at all: a run-wide fold of individual checks cannot say afterwards
   * how many '''rows''' were clean. The failure list is capped here rather than per row, so the
   * cap bounds the report document as a whole.
   *
   * @param rows  the number of fixture rows evaluated
   * @param passedRows  how many of them matched the Java baseline in every respect
   * @param discrepancies  the number of failing checks over all rows, which is the true total
   * @param checksExecuted  the number of checks the run executed, passing and failing together
   * @param failures  the retained descriptions, at most [[FailureReportLimit]] of them
   */
  final case class Measurement(
      rows: Int,
      passedRows: Int,
      discrepancies: Int,
      checksExecuted: Int,
      failures: Vector[Json]) {

    /** Folds one row's outcome in, retaining its failures up to the reporting limit. */
    def add(tally: Tally): Measurement =
      Measurement(
        rows + 1,
        passedRows + (if (tally.matchedEverything) 1 else 0),
        discrepancies + tally.checksFailed,
        checksExecuted + tally.checksExecuted,
        failures ++ tally.failures.take(FailureReportLimit - failures.size))

    /** The failures to publish, with a final entry making any truncation explicit. */
    def reportedFailures: Vector[Json] =
      if (discrepancies <= failures.size) {
        failures
      } else {
        failures :+ Json.obj(
          "truncated" -> Json.fromBoolean(true),
          "reportedFailures" -> Json.fromInt(failures.size),
          "omittedFailures" -> Json.fromInt(discrepancies - failures.size),
          "message" -> Json.fromString(
            s"the failure list is capped at $FailureReportLimit entries; 'failed' is the true total"))
      }

    /** The report this measurement publishes, under the convention both modules share. */
    def report: ParityReport =
      ParityReport(FixtureName, rows, passedRows, discrepancies, reportedFailures)
  }

  object Measurement {

    /** No row measured yet. */
    val empty: Measurement = Measurement(0, 0, 0, 0, Vector.empty)
  }

  /**
   * The document this suite publishes, in the five fields the gate reads.
   *
   * ===The counting convention is one convention, across both modules===
   *
   * Gate 3 reads `daycount`, `schedule`, `fx`, `currency-math` and `holiday` from `strata-basics`
   * and `double-array` from this module, prints their `rows` and `passed` side by side in one
   * table of `target/gate-report.md`, and requires `failed` to be zero. A reader comparing two
   * lines of that table has to be comparing the same quantity, so all '''six''' reports mean the
   * same thing by the same key:
   *
   *  - `rows` is the number of fixture rows evaluated.
   *  - `passed` is the number of those rows that matched the Java baseline in '''every''' respect.
   *  - `failed` is the number of '''discrepancies''' found over all rows, not the number of rows
   *    that differ: one row can differ in several elements at once and each of them is worth
   *    reporting.
   *  - `failures` carries those discrepancies. This module caps the list at
   *    [[FailureReportLimit]] entries and says so in the list itself, and `failed` is still the
   *    true total.
   *
   * The authoritative statement of that convention is the scaladoc of
   * `com.opengamma.strata.basics.parity.ParityReport`, and the invariants that hold a report to
   * the letter of it are `ParityHarness.contractViolations`. Neither can be imported here,
   * because the build's dependency edge runs from `strata-basics` to `strata-collect` and never
   * the other way, so the convention is restated above and the invariants are restated in
   * [[DoubleArrayParitySpec.contractViolations]] - which this suite asserts of the report it
   * publishes, and which [[writeReport]] refuses to publish a report that contradicts.
   *
   * The number of individual '''checks''' a run executed is deliberately not a field here. It is
   * not comparable between fixtures - a day-count row is one number, a double-array row is forty
   * to a hundred and eighty-eight - and the gate requires exactly these five keys and no sixth.
   * Where the count of executed checks is the evidence that the fixture was measured rather than
   * skipped, this suite asserts it directly against the shapes it decoded, through
   * [[expectedChecks]].
   *
   * @param fixture  the fixture stem, which is also the name of the report file
   * @param rows  the number of fixture rows evaluated
   * @param passed  the number of rows that matched the Java baseline in every respect
   * @param failed  the number of discrepancies found, over all rows
   * @param failures  the retained discrepancies, in fixture order
   */
  final case class ParityReport(
      fixture: String,
      rows: Int,
      passed: Int,
      failed: Int,
      failures: Vector[Json]) {

    /**
     * The report as the document the gate parses: exactly the five keys, in this order.
     *
     * `scripts/verify-gates.sh` compares the key set of the parsed document against
     * `{fixture, rows, passed, failed, failures}` for equality, so an addition here - a
     * timestamp, an echo of the tolerance, the executed-check count - would fail Gate 3 rather
     * than enrich it.
     */
    def document: Json =
      Json.obj(
        "fixture" -> Json.fromString(fixture),
        "rows" -> Json.fromInt(rows),
        "passed" -> Json.fromInt(passed),
        "failed" -> Json.fromInt(failed),
        "failures" -> Json.arr(failures: _*))

    /** The counts as one line of an assertion's message. */
    def summary: String = s"$passed of $rows rows matched the baseline; $failed discrepancies"

    /** The first discrepancy, for the message of the assertion, or empty text when there was none. */
    def firstFailureSummary: String =
      failures.headOption.fold("")(first => s"; first failure: ${first.noSpaces}")
  }

  /**
   * The ways in which a report can contradict the counting convention.
   *
   * These are the four invariants of `ParityHarness.contractViolations` in `strata-basics`,
   * restated here because that file cannot be imported from this module, and deliberately worded
   * the same way so that the two can be compared by reading them:
   *
   *  - '''The counts are in range.''' `passed` counts rows, so it is between zero and `rows`, and
   *    neither it nor `failed` nor `rows` is negative.
   *  - '''A clean report measured something, and measured all of it.''' `failed == 0` says every
   *    comparison matched, which is only meaningful over at least one row and requires `passed`
   *    to be all of them. This is the invariant that stops an empty or abandoned run from being
   *    read as a pass - the condition Gate 3 asserts is `failed == 0`, so a run that measured
   *    nothing must not be able to publish it.
   *  - '''A discrepancy is described.''' When `failed` is positive the report lists at least one
   *    discrepancy and never more than it counts, so a module that caps its list - as this one
   *    does - still reports the true total.
   *  - '''A discrepancy belongs to a row that did not pass.''' When something differed and rows
   *    were measured, `passed` is short of `rows`.
   *
   * The last three are what admit the shape a report takes when the measurement could not be
   * completed at all: the refusal of [[refusalReport]], which is worth publishing and is not a
   * pass - the rows the document held, none of them passed, and one discrepancy naming the
   * refusal, including the case where the fixture could not be read and `rows` is therefore zero.
   *
   * @param report  the report to check
   * @return every violation found, or nothing when the report is consistent
   */
  def contractViolations(report: ParityReport): List[String] = {
    val inRange =
      if (report.rows >= 0 && report.passed >= 0 && report.passed <= report.rows &&
        report.failed >= 0) {
        Nil
      } else {
        List(
          s"the counts are out of range: ${report.passed} passed and ${report.failed} failed " +
            s"over ${report.rows} rows")
      }
    val cleanRunMeasuredEverything =
      if (report.failed != 0 || (report.rows >= 1 && report.passed == report.rows)) Nil
      else
        List(
          s"the report claims no discrepancy while ${report.passed} of ${report.rows} rows " +
            "passed; a clean report has at least one row and every one of them passing, because " +
            "'failed == 0' is the condition the gate reads as parity")
    val discrepanciesDescribed =
      if (report.failed == 0 || (report.failures.nonEmpty && report.failures.size <= report.failed)) {
        Nil
      } else {
        List(
          s"the report counts ${report.failed} discrepancies and lists ${report.failures.size}; " +
            "a report lists at least one of the discrepancies it counts and never more than it " +
            "counts")
      }
    val attributedToAFailingRow =
      if (report.failed == 0 || report.rows == 0 || report.passed < report.rows) Nil
      else
        List(
          s"the report counts ${report.failed} discrepancies while all ${report.rows} rows " +
            "passed; a discrepancy belongs to a row that did not pass")
    inRange ::: cleanRunMeasuredEverything ::: discrepanciesDescribed ::: attributedToAFailingRow
  }

  //-------------------------------------------------------------------------
  // Preparation.
  //
  // Every row yields exactly the twenty-two cases of [[ExpectationNames]], in that order, each
  // pairing a captured expectation with the call that reproduces it. There is no dispatch on a
  // name read out of the document and no operation that a row may or may not carry: the row
  // model requires every field, so a fixture that dropped one fails the decode outright, and a
  // field the fixture gained fails it too, because [[RowSchema]] is checked against the row's
  // keys before any of them is read. The twenty-two names are therefore fixed here rather than
  // read from the document without that leaving anything unmeasured - an operation added to the
  // capture is refused until it is added to this list and to the row model, rather than being
  // ignored while the report still reads zero failures.
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

  /**
   * Measures the whole document, one row at a time.
   *
   * The row is the unit of the fold because the row is the unit the report counts in: `passed` is
   * the number of rows that matched in every respect, which is only knowable if each row's checks
   * are tallied on their own before being folded into the run.
   *
   * @param rows  the decoded fixture
   * @return the run's outcome, from which the report is built
   */
  def measure(rows: Vector[Row]): Measurement =
    rows.foldLeft(Measurement.empty)((measurement, row) => measurement.add(evaluate(prepare(row))))

  /** Measures every prepared case of one row, folding the outcomes into one immutable result. */
  def evaluate(cases: Vector[PreparedCase]): Tally =
    cases.foldLeft(Tally.empty)((tally, prepared) => check(prepared, observe(prepared), tally))

  /**
   * The number of checks a clean run over these rows executes.
   *
   * This is the expected-work figure of the measurement, and it is '''derived''' from the shapes
   * the document was decoded into rather than written down, so that a regenerated fixture is held
   * to its own size instead of to a number that has gone stale. Per row, counting exactly what
   * the evaluator does:
   *
   *   - each of the ten element-wise expectations: one shape check, then one check per element of
   *     `a`;
   *   - `concat`: one shape check, then one check per element of the concatenation, which is
   *     twice `a`;
   *   - `subArray`: one shape check, then one check per element of the half-open slice;
   *   - each of the five scalar expectations: one check;
   *   - each of the five matrix expectations: one shape check, then one check per element of the
   *     '''expectation's''' dimensions, which is what makes `matrixTranspose` - whose shape is
   *     the transpose of the input's - counted correctly.
   *
   * It is the count of a run in which every shape matched, which is the only kind of run that can
   * report `failed == 0`: a shape discrepancy is one check that replaces the element checks it
   * would have introduced, and an error raised by the port is one check in place of all of them.
   * The suite therefore asserts this figure of a run it has already established to be clean, and a
   * run that is not clean fails on its discrepancies first.
   *
   * @param rows  the decoded fixture
   * @return the number of checks measuring it must execute
   */
  def expectedChecks(rows: Vector[Row]): Int = rows.map(expectedChecksOf).sum

  /** The number of checks a clean run over one row executes; see [[expectedChecks]]. */
  def expectedChecksOf(row: Row): Int = {
    val elementWise = row.elementWiseExpectations.size * (1 + row.a.size)
    val concatenation = 1 + 2 * row.a.size
    val slice = 1 + (row.subArrayTo - row.subArrayFrom)
    val scalars = row.scalarExpectations.size
    val matrices =
      row.matrixExpectations.map {
        case (_, expected) => 1 + expected.size * expected.headOption.fold(0)(first => first.size)
      }.sum
    elementWise + concatenation + slice + scalars + matrices
  }

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
  def loadFixture: IO[Vector[Row]] = loadFixtureFrom(FixtureResource)

  /**
   * Reads and decodes a captured baseline named by the caller.
   *
   * The name is a parameter of this one function only, so that the durability of the report can be
   * measured on the production path rather than described: the suite drives it with an absent
   * resource and observes that a report is still published. Every other caller goes through
   * [[loadFixture]], which supplies [[FixtureResource]] and nothing else.
   *
   * @param resource  the classpath name of the document to read
   * @return the decoded rows; the effect fails when the document cannot be read or understood
   */
  def loadFixtureFrom(resource: String): IO[Vector[Row]] =
    Resources.readClasspathText(resource).flatMap(text => lift(decodeFixture(text)))

  /**
   * The measurement of a run that completed, with the rows it was made over.
   *
   * The rows are retained beside their outcome because the expected-work assertion is a function
   * of them: [[expectedChecks]] derives from the decoded shapes what the run should have executed,
   * and the suite compares that with what it did execute.
   *
   * @param rows  the decoded fixture
   * @param measurement  its outcome
   */
  final case class Measured(rows: Vector[Row], measurement: Measurement)

  /**
   * A run that could not be carried out, and how far it got.
   *
   * @param rowsDecoded  the number of rows the document yielded, or zero when it could not even
   *                     be read or decoded, all of them unmeasured
   * @param reason  what stopped the run, as `SimpleClassName: message`
   */
  final case class Refusal(rowsDecoded: Int, reason: String)

  /**
   * Attempts the whole of the measurement - read, decode, prepare, evaluate - as one step that
   * cannot fail the effect.
   *
   * This is what makes the report durable. Every stage here can fail for a reason that is not a
   * parity result: the resource can be absent, the document can be malformed, a row can be shaped
   * so that building the operands raises. Left to propagate, any of them would end the run before
   * the report was written, and Gate 3 cannot tell a missing report apart from a measurement that
   * was never made - so the one outcome a failing parity run must not have is no artifact. Each
   * stage is therefore attempted, and its failure is turned into the value the caller publishes as
   * [[refusalReport]] before asserting anything.
   *
   * The two stages are attempted separately because they know different things: a read or decode
   * that failed yields no row count, while a preparation or evaluation that failed yields the
   * count of rows the document held, all of them unmeasured.
   *
   * @return the measurement, or the refusal to publish in its place
   */
  def attemptMeasurement: IO[Either[Refusal, Measured]] = attemptMeasurementOf(loadFixture)

  /**
   * The same attempt, over rows the caller supplies, which is how both of its outcomes are
   * measured rather than assumed.
   *
   * @param loading  the read-and-decode step to attempt
   * @return the measurement, or the refusal to publish in its place
   */
  def attemptMeasurementOf(loading: IO[Vector[Row]]): IO[Either[Refusal, Measured]] =
    loading.attempt.flatMap {
      case Left(error) => IO.pure(Left(Refusal(0, describeThrowable(error))))
      case Right(rows) =>
        IO(measure(rows)).attempt.map {
          case Right(measurement) => Right(Measured(rows, measurement))
          case Left(error) => Left(Refusal(rows.size, describeThrowable(error)))
        }
    }

  /**
   * The report of a run that could not be carried out.
   *
   * No row was measured, so `passed` is zero and the single discrepancy is attributed to the
   * fixture rather than to a row: attributing it to a row would name a row that is not at fault,
   * and leaving it unattributed would put an entry in the report that says nothing about where to
   * look. The identity is the fixture stem with `:fixture`, which no captured row carries. The
   * counts satisfy [[contractViolations]] and are emphatically not a pass, which is the whole
   * point of publishing them.
   *
   * @param refusal  what stopped the run, and how far it got
   * @return the report to publish
   */
  def refusalReport(refusal: Refusal): ParityReport =
    ParityReport(FixtureName, refusal.rowsDecoded, 0, 1, Vector(refusalFailure(refusal)))

  /**
   * Describes a refusal in the same shape as every other failure entry, so that a reader - or the
   * gate, which prints the first few entries verbatim - does not meet a second document format in
   * the one place the report matters most.
   */
  def refusalFailure(refusal: Refusal): Json =
    Json.obj(
      "id" -> Json.fromString(s"$FixtureName:fixture"),
      "expectation" -> Json.fromString("the whole fixture"),
      "check" -> Json.fromString("measurement"),
      "expected" -> Json.fromString("every row of the fixture measured"),
      "actual" -> Json.fromString(s"no row measured, of ${refusal.rowsDecoded} decoded"),
      "message" -> Json.fromString(
        s"the parity fixture '$FixtureResource' could not be read, decoded or measured, so none " +
          s"of its ${refusal.rowsDecoded} rows was measured: ${refusal.reason}"))

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
   * The rows of a document as the JSON values they are, before any of them is decoded.
   *
   * This is the view the key-set check needs: the question it asks - are the document's keys the
   * declared ones? - cannot be asked of the decoded rows, because decoding is exactly the step
   * that turns keys into fields and forgets everything it did not read.
   *
   * @param text  the document
   * @return its top-level array, or the explanation of why it has none
   */
  def rowObjects(text: String): Either[String, Vector[Json]] =
    parser
      .parse(text)
      .leftMap(error =>
        s"the parity fixture '$FixtureResource' did not parse: " +
          Option(error.getMessage).getOrElse(error.toString))
      .flatMap(json =>
        json.asArray.toRight(
          s"the parity fixture '$FixtureResource' is not a top-level JSON array of rows"))

  /**
   * The key set of every row of a document, in document order.
   *
   * @param text  the document
   * @return one key set per row, or the explanation of why a row has none
   */
  def rowKeySets(text: String): Either[String, Vector[Set[String]]] =
    rowObjects(text).flatMap(values =>
      values.zipWithIndex.traverse {
        case (value, index) =>
          value.asObject
            .map(_.keys.toSet)
            .toRight(
              s"row $index of the parity fixture '$FixtureResource' is not a JSON object, so it " +
                "carries no keys to compare with the declared schema")
      })

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
   * The order matters twice over. The counts of a failing run are the whole point of publishing
   * them, so they are on disk before any assertion can end the test; and a run that could not be
   * carried out publishes its refusal here too, through [[refusalReport]], so the artifact exists
   * on both outcomes. The document carries exactly the five fields the gate reads and no others.
   *
   * The counts are checked against [[contractViolations]] first, and a violation is not a parity
   * result: counts that contradict the convention are a defect in this file, and publishing them
   * would put a number into `target/gate-report.md` that means something other than what the
   * column says. There is nothing worth publishing in that case, so the effect fails naming the
   * violation instead. This mirrors `ParityHarness.publish` in `strata-basics`, which guards the
   * other five reports the same way.
   *
   * @param report  the report to publish
   * @return the path written, so that it can be named in the assertion's message
   */
  def writeReport(report: ParityReport): IO[Path] =
    reportDirectory.flatMap(directory => writeReportTo(directory, report))

  /**
   * Writes a report to a directory the caller names, once its counts have been checked.
   *
   * The directory is a parameter so that the suite can publish a refusal to a directory of its
   * own and read it back, which is how the durability of the report is demonstrated without
   * overwriting the one artifact the gate collects. [[writeReport]] is the production form and
   * supplies [[reportDirectory]].
   *
   * @param directory  the directory to write into, which is created if absent
   * @param report  the report to publish
   * @return the path written
   */
  def writeReportTo(directory: Path, report: ParityReport): IO[Path] =
    contractViolations(report) match {
      case Nil =>
        IO.blocking {
          val _ = Files.createDirectories(directory)
          Files.write(
            directory.resolve(ReportFileName),
            (report.document.spaces2 + "\n").getBytes(StandardCharsets.UTF_8))
        }
      case violations =>
        IO.raiseError(
          new IllegalStateException(
            s"the parity report for '${report.fixture}' does not satisfy the counting convention " +
              s"both modules publish under: ${violations.mkString("; ")}"))
    }
}
