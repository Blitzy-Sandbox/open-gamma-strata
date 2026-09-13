/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.parity

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.time.LocalDate

import cats.data.NonEmptyChain
import cats.effect.IO
import cats.effect.Ref
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all._

import io.circe.Decoder
import io.circe.Json
import io.circe.generic.semiauto.deriveDecoder
import io.circe.parser.parse

import org.scalatest.Assertion
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.date.DayCount
import com.opengamma.strata.basics.schedule.Frequency
import com.opengamma.strata.collect.io.Resources
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * Parity of the day-count conventions against the committed baseline.
 *
 * The fixture measures, for every standard convention, the year fraction, the relative year
 * fraction in both directions and the day count, together with `Bus/252 BRBD` and its weekend-only
 * behaviour outside the range of its calendar. The four conventions that read schedule information
 * are measured against each shape of schedule information the document carries.
 * `daycount-baseline.json` holds the reference values and is read-only here: no expectation is
 * corrected or loosened, and no row is skipped.
 *
 * A year fraction is compared by [[ParityHarness.assertParity]], which passes only inside `1e-9`
 * absolute '''and''' `1e-9` relative. No tolerance operator of the test framework appears in this
 * file - no `===` with a tolerance, no `+-` and no `shouldBe` on a `Double` - so the bound is
 * stated in one place only. A day count is an `Int`, compared exactly by
 * [[ParityHarness.assertExact]] and never widened to a `Double`.
 *
 * ===Schedule information===
 *
 * Seventeen of the twenty-one standard conventions ignore schedule information. Four read it:
 * `Act/Act ICMA` reads the schedule end date, the period end date and the frequency, `Act/365L`
 * the period end date and the frequency, `30E/360 ISDA` the schedule end date, and `30U/360` the
 * end-of-month flag. A row carries the schedule information its evaluation was given, in one of
 * three shapes, each of which is reproduced exactly:
 *
 *   - '''Nothing at all''', every subfield absent. Such rows are measured against
 *     `DayCount.ScheduleInfo.simple`, the value the two-argument overloads pass.
 *   - '''One fixed period end date''', `periodEnd`, whose `periodEndDate` '''ignores its
 *     argument''' and answers the single date the row carries.
 *     [[DayCountParitySpec.StubScheduleInfo]].
 *   - '''The boundary list of a schedule''', `periodEnds`, where `periodEndDate(d)` is the first
 *     boundary '''strictly''' after `d`, and only while `start <= d < end`; anywhere else it is
 *     `None`. No interpolation, no clamping and no nearest-boundary rule.
 *     [[DayCountParitySpec.ListedScheduleInfo]].
 *
 * `null` inside `scheduleInfo` means absent, therefore `None`, with one exception: an absent `eom`
 * is the interface default, which is `true`. Mapping it to `false` would re-point every `30U/360`
 * row at the other of that convention's two day-of-month rules.
 *
 * `Bus/252 BRBD` is resolved by the same `DayCount.parse` call as every other subject, and its
 * calendar comes from the calendars built into this library - constant data - so nothing here
 * supplies or threads a `ReferenceData`, and the convention goes through exactly the path the other
 * twenty-one do. The nine `bus252` rows probe a span inside the calendar's 1950-2099 range, spans
 * wholly outside it in both directions, and spans crossing each end, where the calendar falls back
 * to a weekend-only test.
 *
 * ===The two refusals===
 *
 * Two day-count throw families are documented `ArgCheck` preconditions rather than error-channel
 * failures, because both are caller-contract violations that do not depend on the data: dates out
 * of order, where `yearFraction(later, earlier)` and `days(later, earlier)` refuse for every
 * convention and the 22 `order` rows carry an `error` for both operations; and required schedule
 * information absent, which 1,028 rows carry, with the five `missing` rows stating the case once
 * per convention commonly said to need it - recording that `Act/365 Actual` and `30U/360` do
 * '''not''' raise.
 *
 * Both are observed with [[ParityHarness.attemptArgCheck]], which runs the call inside an effect
 * and turns the throw back into a value, so a refusing row stays in the report instead of ending
 * the run. Neither `intercept` nor `try`/`catch` appears in this file, and only the '''type''' of
 * the refusal is asserted: the captured message is quoted in the diagnostic and compared nowhere.
 * An `error` row is an expectation, and so is its absence: a row carrying neither a value nor an
 * error for an operation the fixture always records is a fixture that no longer agrees with this
 * spec, and is reported as that rather than measured as nothing.
 *
 * ===The relative year fraction===
 *
 * `relativeYearFraction` has no order check: it swaps a reversed pair and negates, which is why it
 * answers where `yearFraction` refuses, and it is never asserted to throw. Where a row carries the
 * captured value, that value is the expectation; where it does not, the expectation is the
 * documented identity - negative when the first date is after the second - applied to the row's
 * own year fraction, and only where that identity holds: a zero-length span answers `+yearFraction`
 * rather than its negation, which `1/1` makes observable by answering `1` for any pair.
 * [[DayCountParitySpec.expectedForward]] and [[DayCountParitySpec.expectedReversed]] are that rule.
 *
 * No row reaches this spec carrying `"NaN"` as a year fraction. A `"NaN"` expectation is reported
 * as an ordinary discrepancy by the same comparator that measures everything else, because a
 * not-a-number is at parity only with a not-a-number.
 */
class DayCountParitySpec extends AsyncFunSuite with AsyncIOSpec with Matchers {

  import DayCountParitySpec._

  test("the day count conventions reproduce the Java baseline exactly") {
    for {
      // One resolution per distinct convention name for the whole run, behind a cats-effect
      // reference over an immutable map.
      conventions <- Ref.of[IO, Map[String, DayCount]](Map.empty)
      report <- ParityHarness.runFixture[DayCountRow](FixtureName, FixtureResource, RowSchema)(row =>
        checkRow(conventions, row))
      _ <- ParityHarness.failIfAny(report)
    } yield succeed
  }

  // A verdict that reads `failed == 0` cannot tell a complete measurement from a thinned one, so
  // this case asserts the population the baseline is required to carry.
  test("the fixture carries the population the day-count baseline is required to measure") {
    ParityHarness.loadStrict[DayCountRow](FixtureResource, RowSchema).map { rows =>
      val families = rows.groupBy(familyOf).view.mapValues(_.size).toMap
      withClue(
        s"fixture rows: ${rows.size}; rows by id family: " +
          s"${families.toVector.sortBy(_._1).mkString(", ")}: ") {
        // Floors rather than equalities, so that extending the coverage stays possible.
        rows.size should be >= MinimumRows
        rows.map(_.id).distinct should have size rows.size.toLong
        MinimumRowsByFamily.foreach { case (family, minimum) =>
          withClue(s"id family '$family': ") {
            families.getOrElse(family, 0) should be >= minimum
          }
        }
        // A row of an undeclared family is reported rather than counted towards a floor.
        rows.map(familyOf).distinct.filterNot(MinimumRowsByFamily.contains) shouldBe empty
        // Every required convention appears, and appears in both generated populations, so
        // thinning a subject out of either cannot pass on the row count alone.
        val subjects = rows.map(_.dayCount).distinct.toSet
        RequiredDayCounts.filterNot(subjects.contains) shouldBe empty
        GeneratedFamilies.foreach { family =>
          withClue(s"subjects missing from the '$family' family: ") {
            val covered = rows.filter(row => familyOf(row) == family).map(_.dayCount).toSet
            RequiredDayCounts.filterNot(covered.contains) shouldBe empty
          }
        }
        // Each operation the fixture always records carries exactly one of a value and an error;
        // a row carrying neither would measure nothing at all.
        rows.filter(row => row.yearFraction.isDefined == row.error.isDefined).map(_.id) shouldBe empty
        rows.filter(row => row.days.isDefined == row.daysError.isDefined).map(_.id) shouldBe empty
        rows.count(_.error.isDefined) should be >= MinimumYearFractionRefusals
        rows.count(_.daysError.isDefined) should be >= MinimumDaysRefusals
        rows
          .filter(row => familyOf(row) == OutOfOrderFamily)
          .filter(row => row.error.isEmpty || row.daysError.isEmpty || !row.start.isAfter(row.end))
          .map(_.id) shouldBe empty
        // The relative forms are captured as a pair or not at all, and enough rows carry them for
        // the captured expectation, rather than the derived identity, to be what is measured.
        rows
          .filter(row => row.relativeYearFraction.isDefined != row.relativeYearFractionReversed.isDefined)
          .map(_.id) shouldBe empty
        rows.count(_.relativeYearFraction.isDefined) should be >= MinimumRelativeRows
        // The schedule information is in one of the three shapes this spec reads. A row carrying
        // both period-end shapes, an empty boundary list, or a boundary list without the schedule
        // span that bounds it, is a fixture this spec cannot measure and is named here.
        rows
          .filter(row => row.scheduleInfo.periodEnd.isDefined && row.scheduleInfo.periodEnds.isDefined)
          .map(_.id) shouldBe empty
        rows.filter(_.scheduleInfo.periodEnds.exists(_.isEmpty)).map(_.id) shouldBe empty
        rows
          .filter(row => row.scheduleInfo.periodEnds.isDefined && !row.scheduleInfo.spansSchedule)
          .map(_.id) shouldBe empty
        rows.count(_.scheduleInfo.periodEnds.isDefined) should be >= MinimumScheduleRows
        rows.count(_.scheduleInfo.isAbsent) should be >= MinimumSimpleRows
        succeed
      }
    }
  }

  //-------------------------------------------------------------------------
  // THE CONTRACT OF THE SHARED HARNESS
  //
  // The harness has no spec of its own, so its decisions are asserted from inside its heaviest
  // consumer, which can reach them because the seams that carry them are `private[parity]` and
  // this file is in that package. Weaken the conjunction of the two tolerance bounds to a
  // disjunction, drop the sign of a presence mismatch, count discrepancies as rows or accept a
  // fixture with no rows, and the measurements above still pass while measuring something weaker.
  // Every case below that publishes anything does so under a probe stem, into a temporary
  // directory it creates and removes for itself, never into the directory named by
  // `parity.report.dir`, which is where the measurement above publishes the real `daycount.json`.
  //-------------------------------------------------------------------------

  // `HarnessProbeRow` and its decoder are declared in the companion, because a row model declared
  // inside the class would carry an outer reference its type test cannot check at run time.

  /** The smallest committed baseline that carries row identities, used by the driver cases. */
  private val HarnessFxFixture: String = "parity/fx-baseline.json"

  /** A committed baseline of several hundred rows, so the ceilings are exercised on real data. */
  private val HarnessScheduleFixture: String = "parity/schedule-baseline.json"

  private val HarnessCommittedFixtures: Vector[String] =
    Vector(
      FixtureResource,
      HarnessScheduleFixture,
      HarnessFxFixture,
      "parity/currency-math-baseline.json",
      "parity/holiday-baseline.json")

  /**
   * A difference that both bounds admit next to a value of one: two to the power of minus
   * thirty-one, which is exactly representable there, so the difference under test is exact.
   */
  private val HarnessJustInsideBothBounds: Double = math.pow(2.0d, -31.0d)

  /** A difference next to one that neither bound admits: two to the power of minus twenty-nine. */
  private val HarnessJustOutsideBothBounds: Double = math.pow(2.0d, -29.0d)

  /** A report with the given stem and nothing in it, for the cases that only publish one. */
  private def harnessReportOf(fixture: String): ParityReport =
    ParityReport(fixture, 1, 1, 0, Vector.empty)

  /**
   * Creates an empty directory, hands it to the case, and removes it and anything the case wrote.
   * The finalizer runs on every outcome, so a failing case leaves nothing behind.
   */
  private def withHarnessTempDirectory(use: Path => IO[Assertion]): IO[Assertion] =
    IO.blocking(Files.createTempDirectory("daycount-parity-harness-")).flatMap { directory =>
      use(directory).guarantee(IO.blocking {
        Option(directory.toFile.listFiles())
          .map(_.toVector)
          .getOrElse(Vector.empty)
          .foreach(entry => Files.deleteIfExists(entry.toPath))
        val _ = Files.deleteIfExists(directory)
      })
    }

  test("the shared harness: assertParity passes a pair inside both bounds") {
    IO {
      ParityHarness.assertParity("equal", 1.0d, 1.0d) shouldBe Nil
      ParityHarness.assertParity("equal zero", 0.0d, 0.0d) shouldBe Nil
      ParityHarness.assertParity("just inside", 1.0d, 1.0d + HarnessJustInsideBothBounds) shouldBe Nil
      ParityHarness.assertParity("negative, just inside", -1.0d, -1.0d - HarnessJustInsideBothBounds) shouldBe Nil
    }
  }

  test("the shared harness: assertParity rejects a difference that only the relative bound admits") {
    IO {
      // A thousandth between values of the order of 1e12: a relative difference of 1e-15 and an
      // absolute difference a million times the absolute bound, which a disjunction would admit.
      val discrepancies = ParityHarness.assertParity("large magnitude", 1e12d, 1e12d + 1e-3d)
      discrepancies should have size 1L
      discrepancies.head should include("met: false")
      discrepancies.head should include("met: true")
    }
  }

  test("the shared harness: assertParity rejects a difference that only the absolute bound admits") {
    IO {
      // One value twice the other, both of the order of 1e-15: an absolute difference far inside
      // 1e-9 and a relative difference of fifty percent, which a disjunction would admit.
      val discrepancies = ParityHarness.assertParity("near zero", 1e-15d, 2e-15d)
      discrepancies should have size 1L
      discrepancies.head should include("met: true")
      discrepancies.head should include("met: false")
    }
  }

  test("the shared harness: assertParity rejects a difference outside both bounds") {
    IO {
      val discrepancies = ParityHarness.assertParity("outside", 1.0d, 1.0d + HarnessJustOutsideBothBounds)
      discrepancies should have size 1L
      discrepancies.head should include("met: false")
    }
  }

  test("the shared harness: assertParity treats identical values outside the finite range as parity") {
    IO {
      // Bit identity is decided before any arithmetic, the only way these can pass: the difference
      // of two infinities is not a number, and every comparison against one is false.
      ParityHarness.assertParity("nan", Double.NaN, Double.NaN) shouldBe Nil
      ParityHarness.assertParity("positive infinity", Double.PositiveInfinity, Double.PositiveInfinity) shouldBe Nil
      ParityHarness.assertParity("negative infinity", Double.NegativeInfinity, Double.NegativeInfinity) shouldBe Nil
    }
  }

  test("the shared harness: assertParity reports a pair involving a value outside the finite range") {
    IO {
      val opposedInfinities =
        ParityHarness.assertParity("opposed", Double.PositiveInfinity, Double.NegativeInfinity)
      opposedInfinities should have size 1L
      opposedInfinities.head should include("outside the finite range")
      ParityHarness.assertParity("nan against finite", Double.NaN, 1.0d) should have size 1L
      ParityHarness.assertParity("finite against infinity", 1.0d, Double.PositiveInfinity) should have size 1L
      // No difference is computed for such a pair, so no arithmetic on one appears in the message.
      opposedInfinities.head should not include "difference NaN"
    }
  }

  test("the shared harness: assertParity treats positive and negative zero as parity") {
    IO {
      // The two zeros are not bit identical, so they reach the tolerance test, where their
      // difference is zero and both bounds hold.
      ParityHarness.assertParity("signed zero", -0.0d, 0.0d) shouldBe Nil
      ParityHarness.assertParity("signed zero, reversed", 0.0d, -0.0d) shouldBe Nil
    }
  }

  test("the shared harness: assertParity names the field, both values and both differences") {
    IO {
      val discrepancies = ParityHarness.assertParity("yearFraction", 0.5d, 0.25d)
      discrepancies should have size 1L
      val message = discrepancies.head
      // A report has to be diagnosable without re-running the suite that wrote it.
      message should include("yearFraction")
      message should include("0.5")
      message should include("0.25")
      message should include("absolute difference")
      message should include("relative difference")
    }
  }

  test("the shared harness: assertParityOpt compares presence as an expectation in its own right") {
    IO {
      ParityHarness.assertParityOpt("absent", None, None) shouldBe Nil
      ParityHarness.assertParityOpt("present", Some(1.0d), Some(1.0d)) shouldBe Nil
      ParityHarness.assertParityOpt("beyond tolerance", Some(1.0d), Some(2.0d)) should have size 1L
      // A value produced where the baseline has none, and the reverse, are reported differently.
      val unexpected = ParityHarness.assertParityOpt("unexpected", Some(1.0d), None)
      unexpected should have size 1L
      unexpected.head should include("expected no value")
      val missing = ParityHarness.assertParityOpt("missing", None, Some(1.0d))
      missing should have size 1L
      missing.head should include("actual no value")
    }
  }

  test("the shared harness: assertParitySeq reports a length difference once, else compares by index") {
    IO {
      ParityHarness.assertParitySeq("equal", Vector(1.0d, 2.0d), Vector(1.0d, 2.0d)) shouldBe Nil
      // A shifted sequence differs at every index; reporting the length once keeps that visible.
      val lengths = ParityHarness.assertParitySeq("lengths", Vector(1.0d), Vector(1.0d, 2.0d))
      lengths should have size 1L
      lengths.head should include("1 elements")
      lengths.head should include("2 elements")
      // Equal lengths are compared by index, so two differing positions are two discrepancies.
      val elements =
        ParityHarness.assertParitySeq("elements", Vector(1.0d, 9.0d, 3.0d), Vector(1.0d, 2.0d, 4.0d))
      elements should have size 2L
      elements.head should include("elements[1]")
      elements(1) should include("elements[2]")
    }
  }

  test("the shared harness: assertExact compares dates, strings and whole numbers exactly") {
    IO {
      ParityHarness.assertExact("date", LocalDate.of(2024, 1, 31), LocalDate.of(2024, 1, 31)) shouldBe Nil
      ParityHarness.assertExact(
        "dates",
        List(LocalDate.of(2024, 1, 31)),
        List(LocalDate.of(2024, 1, 31))) shouldBe Nil
      ParityHarness.assertExact("name", "Act/365F", "Act/365F") shouldBe Nil
      ParityHarness.assertExact("days", 31, 31) shouldBe Nil
      // No tolerance anywhere near these: a money amount out by one unit in the last place is
      // wrong rather than close, which is why amounts are captured as their decimal text.
      ParityHarness.assertExact("amount", "12.34", "12.35") should have size 1L
      val dates = ParityHarness.assertExact("date", LocalDate.of(2024, 1, 31), LocalDate.of(2024, 2, 1))
      dates should have size 1L
      dates.head should include("2024-01-31")
      dates.head should include("2024-02-01")
    }
  }

  test("the shared harness: assertLeft passes only when the operation failed") {
    IO {
      ParityHarness.assertLeft("refused", Left(Failure.Invalid("refused"))) shouldBe Nil
      val accepted = ParityHarness.assertLeft("accepted", Right(1.5d))
      accepted should have size 1L
      accepted.head should include("1.5")
    }
  }

  test("the shared harness: assertRight runs the check on success and reports the failure instead") {
    IO {
      // On success the check decides, so a spec never reaches into an Either for the value.
      ParityHarness.assertRight("succeeded", Right(2.0d))(value =>
        ParityHarness.assertParity("value", value, 2.0d)) shouldBe Nil
      ParityHarness.assertRight("succeeded, differing", Right(2.0d))(value =>
        ParityHarness.assertParity("value", value, 3.0d)) should have size 1L
      // On failure the check is not run: reporting comparisons that had nothing to compare would
      // multiply one fact into several.
      val failed = ParityHarness.assertRight("failed", Left(Failure.MissingData("no calendar")))(_ =>
        List("the check must not run"))
      failed should have size 1L
      failed.head should include("no calendar")
    }
  }

  test("the shared harness: attemptArgCheck passes only when the precondition was refused") {
    for {
      refused <- ParityHarness.attemptArgCheck("refused")(
        throw new IllegalArgumentException("dates supplied out of order"))
      wrongType <- ParityHarness.attemptArgCheck("wrong type")(throw new IllegalStateException("something else"))
      returned <- ParityHarness.attemptArgCheck("returned")(42)
    } yield {
      refused shouldBe Nil
      // A precondition enforced by a different failure is not the one recorded.
      wrongType should have size 1L
      wrongType.head should include("IllegalStateException")
      returned should have size 1L
      returned.head should include("42")
    }
  }

  test("the shared harness: the raise lifts carry every failure message into the effect") {
    for {
      value <- ParityHarness.raise(Right(7))
      single <- ParityHarness.raise(Left(Failure.Invalid("one cause"))).attempt
      several <- ParityHarness
        .raiseNec(Left(NonEmptyChain(Failure.Invalid("first cause"), Failure.Parsing("second cause"))))
        .attempt
    } yield {
      value shouldBe 7
      single.left.map(_.getMessage) shouldBe Left("one cause")
      // Every cause survives the lift, joined: two reasons are not diagnosable from one of them.
      several.left.map(_.getMessage) shouldBe Left("first cause; second cause")
    }
  }

  test("the shared harness: load reads a committed baseline into its rows, in fixture order") {
    for {
      rows <- ParityHarness.load[HarnessProbeRow](HarnessFxFixture)
      text <- Resources.readClasspathText(HarnessFxFixture)
      larger <- ParityHarness.load[HarnessProbeRow](HarnessScheduleFixture)
    } yield {
      rows.size should be >= 14
      rows.map(_.id).head shouldBe "cross-rate-triangulating-matrix"
      rows.filter(_.id.isEmpty) shouldBe empty
      // Fixture order is asserted against the document itself: each row's identity is located in
      // the text, and those positions have to ascend in the order the rows were decoded.
      val offsets = rows.map(row => text.indexOf("\"" + row.id + "\""))
      offsets.filter(_ < 0) shouldBe empty
      offsets.distinct should have size offsets.size.toLong
      offsets shouldBe offsets.sorted
      // A fixture of several hundred rows passes the same ceilings, which therefore admit real
      // data.
      larger.size should be > 100
    }
  }

  test("the shared harness: load fails for a resource that is not on the classpath") {
    ParityHarness.load[HarnessProbeRow]("parity/no-such-baseline.json").attempt.map { outcome =>
      outcome.isLeft shouldBe true
      outcome.left.map(failure => Option(failure.getMessage).getOrElse("")) match {
        case Left(message) => message should include("no-such-baseline.json")
        case Right(rows) => fail(s"expected a failed effect, got ${rows.size} rows")
      }
    }
  }

  test("the shared harness: load refuses an empty fixture, so an emptied baseline cannot pass") {
    // An empty array decodes into no rows, and zero rows, zero passed and zero failed is the shape
    // of a measurement that succeeded, so an emptied baseline would retire every comparison in it.
    ParityHarness.decodeRows[HarnessProbeRow]("probe", "[]", ParityHarness.MaxFixtureCharacters, 10).attempt.map {
      case Left(failure) => failure.getMessage should include("holds no rows")
      case Right(rows) => fail(s"expected an empty fixture to be refused, got ${rows.size} rows")
    }
  }

  test("the shared harness: load refuses a document that is not an array of rows") {
    for {
      anObject <- ParityHarness
        .decodeRows[HarnessProbeRow]("probe", "{}", ParityHarness.MaxFixtureCharacters, 10)
        .attempt
      malformed <- ParityHarness
        .decodeRows[HarnessProbeRow]("probe", "[{\"id\":", ParityHarness.MaxFixtureCharacters, 10)
        .attempt
      wrongRows <- ParityHarness
        .decodeRows[HarnessProbeRow]("probe", "[{\"unexpected\":1}]", ParityHarness.MaxFixtureCharacters, 10)
        .attempt
    } yield {
      // Each of the three is a fact about the fixture rather than a discrepancy of this
      // implementation, and absorbing one into a parity result would make the verdict useless.
      anObject.left.map(_.getMessage) match {
        case Left(message) => message should include("not a top-level JSON array")
        case Right(rows) => fail(s"expected a JSON object to be refused, got ${rows.size} rows")
      }
      malformed.isLeft shouldBe true
      wrongRows.isLeft shouldBe true
    }
  }

  test("the shared harness: load refuses a fixture beyond the character ceiling before parsing it") {
    val document = "[{\"id\":\"a\",\"source\":\"a\"}]"
    ParityHarness.decodeRows[HarnessProbeRow]("probe", document, document.length - 1, 10).attempt.map {
      case Left(failure) =>
        failure.getMessage should include(s"${document.length} characters")
        failure.getMessage should include(s"${document.length - 1}")
      case Right(rows) => fail(s"expected an oversized fixture to be refused, got ${rows.size} rows")
    }
  }

  test("the shared harness: load refuses a fixture with more rows than the ceiling before decoding") {
    val document = "[{\"id\":\"a\",\"source\":\"a\"},{\"id\":\"b\",\"source\":\"b\"}]"
    ParityHarness
      .decodeRows[HarnessProbeRow]("probe", document, ParityHarness.MaxFixtureCharacters, 1)
      .attempt
      .map {
        case Left(failure) => failure.getMessage should include("holds 2 rows")
        case Right(rows) => fail(s"expected an over-long fixture to be refused, got ${rows.size} rows")
      }
  }

  test("the shared harness: load decodes a bounded fixture into its rows in document order") {
    val document = "[{\"id\":\"first\",\"source\":\"a\"},{\"id\":\"second\",\"source\":\"b\"}]"
    ParityHarness
      .decodeRows[HarnessProbeRow]("probe", document, ParityHarness.MaxFixtureCharacters, 10)
      .map(rows => rows.map(_.id) shouldBe Vector("first", "second"))
  }

  test("the shared harness: the fixture ceilings admit every committed baseline") {
    HarnessCommittedFixtures
      .traverse(resource => ParityHarness.load[HarnessProbeRow](resource).map(rows => (resource, rows.size)))
      .map { measured =>
        measured.foreach { case (resource, rows) =>
          withClue(s"$resource: ") {
            rows should be > 0
            rows should be <= ParityHarness.MaxFixtureRows
          }
        }
        // The largest baseline committed today is the day-count document this suite measures, at
        // roughly 12.7 MiB and 18,356 rows, and it is loaded here rather than assumed.
        ParityHarness.MaxFixtureCharacters should be > 16 * 1024 * 1024
        ParityHarness.MaxFixtureRows should be > 20000
        measured.map(_._2).max should be > 18000
      }
  }

  test("the shared harness: the report directory must be configured, and must be absolute") {
    IO {
      // An unset property has no fallback: a default would let a misconfigured run publish where
      // nothing collects it, which cannot be told apart from a measurement nobody made.
      ParityHarness.reportDirectoryFrom(None) match {
        case Left(message) => message should include(ParityHarness.ReportDirectoryProperty)
        case Right(directory) => fail(s"an unset property was accepted as $directory")
      }
      ParityHarness.reportDirectoryFrom(Some("   ")).isLeft shouldBe true
      // A relative path resolves against the working directory of a forked test JVM.
      ParityHarness.reportDirectoryFrom(Some("target/parity-report")) match {
        case Left(message) => message should include("relative")
        case Right(directory) => fail(s"a relative property was accepted as $directory")
      }
      ParityHarness.reportDirectoryFrom(Some("/tmp/parity-report-probe")) shouldBe
        Right(Paths.get("/tmp/parity-report-probe"))
      // The build supplies exactly that: the absolute path `daycount.json` is published into.
      ParityHarness.reportDirectoryFrom(sys.props.get(ParityHarness.ReportDirectoryProperty)) match {
        case Right(directory) => directory.isAbsolute shouldBe true
        case Left(message) => fail(s"the build's own report directory was refused: $message")
      }
    }
  }

  test("the shared harness: a fixture stem must name one file inside the report directory") {
    withHarnessTempDirectory { directory =>
      for {
        nested <- ParityHarness.writeReportTo(directory, harnessReportOf("nested/stem")).attempt
        windows <- ParityHarness.writeReportTo(directory, harnessReportOf("nested\\stem")).attempt
        blank <- ParityHarness.writeReportTo(directory, harnessReportOf("   ")).attempt
        plain <- ParityHarness.writeReportTo(directory, harnessReportOf("probe"))
        published <- IO.blocking(Option(directory.toFile.listFiles()).map(_.length).getOrElse(0))
      } yield {
        // A stem becomes part of a path, so one carrying a separator would publish elsewhere.
        nested.left.map(_.getClass.getName) shouldBe Left("java.lang.IllegalArgumentException")
        windows.isLeft shouldBe true
        blank.isLeft shouldBe true
        plain.getFileName.toString shouldBe "probe.json"
        plain.getParent shouldBe directory
        published shouldBe 1
      }
    }
  }

  test("the shared harness: runFixtureIn counts rows that matched and discrepancies that did not") {
    withHarnessTempDirectory { directory =>
      for {
        rows <- ParityHarness.load[HarnessProbeRow](HarnessFxFixture)
        firstId = rows.head.id
        report <- ParityHarness.runFixtureIn[HarnessProbeRow](directory, "probe-counts", HarnessFxFixture) { row =>
          IO.pure(if (row.id == firstId) List("first difference", "second difference") else Nil)
        }
      } yield {
        // The counting convention: `passed` counts rows with no message, `failed` counts
        // discrepancies rather than rows, so one row differing twice exceeds the number of rows.
        report.fixture shouldBe "probe-counts"
        report.rows shouldBe rows.size
        report.passed shouldBe rows.size - 1
        report.failed shouldBe 2
        report.failures.map(_.id) shouldBe Vector(firstId, firstId)
        report.failures.map(_.message) shouldBe Vector("first difference", "second difference")
      }
    }
  }

  test("the shared harness: runFixtureIn publishes the report before the failure gate can end the test") {
    withHarnessTempDirectory { directory =>
      for {
        report <- ParityHarness.runFixtureIn[HarnessProbeRow](directory, "probe-published", HarnessFxFixture)(_ =>
          IO.pure(List("every row differs")))
        published <- IO.blocking(
          new String(Files.readAllBytes(directory.resolve("probe-published.json")), StandardCharsets.UTF_8))
        gate <- ParityHarness.failIfAny(report).attempt
        clean <- ParityHarness.failIfAny(report.copy(failed = 0, failures = Vector.empty)).attempt
      } yield {
        // The counts of a failing run are the reason the report exists, so they are on disk before
        // anything can decide the run failed; reading the file back here proves that order.
        report.failed shouldBe report.rows
        parse(published).map(_.asObject.map(_.keys.toVector)) shouldBe
          Right(Some(Vector("fixture", "rows", "passed", "failed", "failures")))
        published should include("probe-published")
        published should include("every row differs")
        gate.left.map(_.getClass.getName) shouldBe Left("java.lang.AssertionError")
        gate.left.map(failure => Option(failure.getMessage).getOrElse("")) match {
          case Left(message) =>
            message should include("probe-published")
            message should include(s"${report.failed} discrepancies")
          case Right(_) => fail("a report holding discrepancies did not fail the gate")
        }
        clean shouldBe Right(())
      }
    }
  }

  test("the shared harness: runFixtureIn records a row whose check raises and keeps measuring") {
    withHarnessTempDirectory { directory =>
      for {
        rows <- ParityHarness.load[HarnessProbeRow](HarnessFxFixture)
        firstId = rows.head.id
        report <- ParityHarness.runFixtureIn[HarnessProbeRow](directory, "probe-raised", HarnessFxFixture) { row =>
          if (row.id == firstId) IO.raiseError(new RuntimeException("the check itself broke")) else IO.pure(Nil)
        }
      } yield {
        // An exception escaping a row would take the counts of every row after it with it.
        report.rows shouldBe rows.size
        report.failed shouldBe 1
        report.passed shouldBe rows.size - 1
        report.failures.head.id shouldBe firstId
        report.failures.head.message should include("the check itself broke")
      }
    }
  }

  test("the shared harness: the failure gate quotes twenty discrepancies and says how many it left out") {
    val failures = Vector.tabulate(25)(index => ParityFailure(s"row-$index", s"difference $index"))
    val report = ParityReport("probe-truncation", 25, 0, failures.size, failures)
    withHarnessTempDirectory { directory =>
      for {
        written <- ParityHarness.writeReportTo(directory, report)
        document <- IO.blocking(new String(Files.readAllBytes(written), StandardCharsets.UTF_8))
        gate <- ParityHarness.failIfAny(report).attempt
      } yield {
        gate.left.map(failure => Option(failure.getMessage).getOrElse("")) match {
          case Left(message) =>
            message should include("difference 0")
            message should include("difference 19")
            // Everything beyond the quota stays in the document rather than the console.
            message should not include "difference 20"
            message should include("and 5 more")
          case Right(_) => fail("a report holding discrepancies did not fail the gate")
        }
        // The document on disk stays the complete record: truncation belongs to the message.
        parse(document).map(_.asObject.flatMap(_("failures")).flatMap(_.asArray).map(_.size)) shouldBe
          Right(Some(failures.size))
        document should include("difference 20")
        document should include("difference 24")
      }
    }
  }

  /*
   * The three cases below are about the decoding of the fixture: a verdict of `failed == 0` over
   * rows that decoded perfectly cannot tell a fixture measured in full from one that has grown a
   * key nothing reads.
   */

  test("the declared row and scheduleInfo key sets are the ones the row models read") {
    IO {
      // The schema is the model's own field set, so a field added to the model without being added
      // to the schema, or the reverse, fails here.
      RowSchema.known shouldBe DocumentedRowModel.productElementNames.toSet
      RowSchema.known.size shouldBe 12
      ScheduleInfoSchema.known shouldBe DocumentedListedScheduleInfoModel.productElementNames.toSet
      ScheduleInfoSchema.known.size shouldBe 6
      DocumentedRow.asObject.map(_.keys.toSet) shouldBe Some(RowSchema.known)
      ScheduleInfoSchema.variants.map(_._1) shouldBe Vector(FixedPeriodEndVariant, BoundaryListVariant)
      ScheduleInfoSchema.variants.map(_._2.size) shouldBe Vector(5, 5)
      // Each variant is satisfied by exactly one committed encoding, and the schema names which.
      variantOf(DocumentedFixedScheduleInfo) shouldBe Some(FixedPeriodEndVariant)
      variantOf(DocumentedListedScheduleInfo) shouldBe Some(BoundaryListVariant)
      succeed
    }
  }

  test("a captured row whose keys are not the documented twelve is refused by name") {
    IO {
      // The documented shape decodes, field for field: strictness refuses what the document does
      // not document and nothing else.
      StrictRowDecoder.decodeJson(DocumentedRow) shouldBe Right(DocumentedRowModel)
      // A key that has appeared in the document: a derived decoder would ignore it and measure
      // the row as though the new expectation did not exist.
      refusalOf(
        StrictRowDecoder,
        withKey(DocumentedRow, "yearFractionRounded", Json.fromDoubleOrNull(1.0d))) should include(
        "unknown keys {yearFractionRounded}")
      // A schema-required key the document no longer carries, which `Option` alone cannot tell
      // apart from the `null` that means "this evaluation was not performed".
      refusalOf(StrictRowDecoder, withoutKey(DocumentedRow, "days")) should include(
        "a day-count parity row is missing {days}")
      val renamed = refusalOf(StrictRowDecoder, withRenamedKey(DocumentedRow, "daysError", "dayCountError"))
      renamed should include("unknown keys {dayCountError}")
      renamed should include("a day-count parity row is missing {daysError}")
      succeed
    }
  }

  test("a scheduleInfo object that is neither documented variant is refused by name") {
    IO {
      StrictRowDecoder.decodeJson(DocumentedRow).map(_.scheduleInfo) shouldBe
        Right(DocumentedRowModel.scheduleInfo)
      StrictRowDecoder
        .decodeJson(rowWithScheduleInfo(DocumentedListedScheduleInfo))
        .map(_.scheduleInfo) shouldBe Right(DocumentedListedScheduleInfoModel)
      // Both encodings at once: a row carries exactly one, and the two read `periodEndDate`
      // differently, so the object is refused with the keys it carries.
      val both = refusalOf(
        StrictRowDecoder,
        rowWithScheduleInfo(withKey(DocumentedListedScheduleInfo, FixedPeriodEndKey, Json.Null)))
      both should include("satisfy no documented variant")
      both should include("periodEnd, periodEnds")
      // Neither encoding: each variant is refused by the key it wanted and did not get.
      val neither = refusalOf(
        StrictRowDecoder,
        rowWithScheduleInfo(withoutKey(DocumentedFixedScheduleInfo, FixedPeriodEndKey)))
      neither should include(s"$FixedPeriodEndVariant is missing {$FixedPeriodEndKey}")
      neither should include(s"$BoundaryListVariant is missing {$BoundaryListKey}")
      // A key that has appeared inside the nested object, which only its own decoder sees.
      refusalOf(
        StrictRowDecoder,
        rowWithScheduleInfo(
          withKey(DocumentedListedScheduleInfo, "periodStarts", Json.arr()))) should include(
        "unknown keys {periodStarts}")
      val renamed = refusalOf(
        StrictRowDecoder,
        rowWithScheduleInfo(withRenamedKey(DocumentedListedScheduleInfo, EndOfMonthKey, "endOfMonth")))
      renamed should include("unknown keys {endOfMonth}")
      renamed should include(s"$BoundaryListVariant is missing {$EndOfMonthKey}")
      succeed
    }
  }
}

/**
 * The row model of `daycount-baseline.json`, the schedule information it encodes, and the checks
 * applied to one row. It lives in the companion because the JSON derivation needs the row types on
 * a stable path, and everything here is confined to this package.
 */
private[parity] object DayCountParitySpec {

  /*
   * The single policy for a double, in scope before the row decoders are derived. The baseline
   * writes a non-finite double as one of the tagged strings `"NaN"`, `"Infinity"` and
   * `"-Infinity"`, and every finite one as a full-precision JSON number; an imported implicit
   * outranks the one circe publishes for `Double`, so the `Option[Double]` fields below read a
   * tagged value with no ambiguity. No row is expected to carry one, and reading them is what lets
   * such a row be reported as a discrepancy instead of a decode failure of the whole document.
   */
  import Codecs.implicits.doubleCodec

  //-------------------------------------------------------------------------
  // Contract constants. The resource name and the fixture stem are agreements with something
  // outside this file - the document read, and the `<parity.report.dir>/daycount.json` written.
  //-------------------------------------------------------------------------

  /** The fixture stem, which is also the `fixture` field of the report and its file name. */
  val FixtureName: String = "daycount"

  /** The classpath name of the captured baseline, relative to the test resource root. */
  val FixtureResource: String = "parity/daycount-baseline.json"

  /**
   * The end-of-month convention an absent `eom` stands for: `ScheduleInfo` defaults
   * `isEndOfMonthConvention` to `true`, and `30U/360` chooses between two day-of-month rules by it.
   */
  val EndOfMonthDefault: Boolean = true

  /** The least number of rows the baseline is worth measuring; the document holds 18,356. */
  val MinimumRows: Int = 18356

  /**
   * The least number of rows of each population, keyed by the row's id family. The key set is
   * closed: a row of another family is reported rather than measured silently, and the counts sum
   * to [[MinimumRows]].
   */
  val MinimumRowsByFamily: Map[String, Int] =
    Map(
      // all 22 subjects x consecutive month-end and mid-month pairs, 2010-2030, no schedule info
      "grid" -> 11044,
      // all 22 subjects x P1M/P3M/P6M/P12M x regular, short initial, short final x every period
      "sched" -> 6446,
      // the tabulated year-fraction and day-count cases, one row per case per consumer
      "yf" -> 201,
      "days" -> 185,
      // every 1000th iteration of the Act/Act Year versus Act/Act ICMA equivalence
      "yvi" -> 146,
      // the 30U/360 table across its four consumers
      "u360" -> 88,
      "afb" -> 57,
      // the 30E/360 ISDA table, at and away from maturity
      "e360i" -> 38,
      // the official ISDA test cases
      "isda" -> 24,
      // the four portable consumers over all 22 subjects
      "same" -> 22,
      "half" -> 22,
      "whole" -> 22,
      "order" -> 22,
      // the Act/Act ICMA stub series
      "icma" -> 13,
      "l365" -> 12,
      // in range, wholly outside the calendar's range, and crossing it
      "bus252" -> 9,
      // missing schedule information, stated once per convention said to need it
      "missing" -> 5
    )

  val GeneratedFamilies: Vector[String] = Vector("grid", "sched")

  /** The family whose every row supplies its dates out of order, refusing both operations. */
  val OutOfOrderFamily: String = "order"

  /**
   * Every day-count name the baseline is required to measure: the 21 standard conventions and the
   * business-day convention `Bus/252 BRBD`, whose calendar comes from the built-in set.
   */
  val RequiredDayCounts: Vector[String] =
    Vector(
      "1/1",
      "Act/Act ISDA",
      "Act/Act AFB",
      "Act/Act Year",
      "Act/365 Actual",
      "Act/365L",
      "Act/360",
      "Act/364",
      "Act/365F",
      "Act/365.25",
      "NL/360",
      "NL/365",
      "30/360 ISDA",
      "30U/360",
      "30U/360 EOM",
      "30/360 PSA",
      "30E/360 ISDA",
      "30E/360",
      "30E+/360",
      "30E/365",
      "Act/Act ICMA",
      "Bus/252 BRBD"
    )

  /** The least number of rows whose year fraction the baseline records as refused. */
  val MinimumYearFractionRefusals: Int = 1050

  /** The least number of rows whose day count the baseline records as refused. */
  val MinimumDaysRefusals: Int = 22

  /** The least number of rows carrying the captured relative year fractions, in both directions. */
  val MinimumRelativeRows: Int = 221

  /** The least number of rows evaluated against the boundary list of a real schedule. */
  val MinimumScheduleRows: Int = 6446

  /** The least number of rows evaluated against schedule information that carries nothing. */
  val MinimumSimpleRows: Int = 11561

  //-------------------------------------------------------------------------
  // The row model. Field for field the shape of the document: every row carries the identical
  // twelve keys in the same order, `scheduleInfo` is always an object, and a key whose evaluation
  // was not performed is present with the value `null`, which is where `Option` sits. Each shape
  // is declared as a `KeySchema` beside the model it describes, and every object is checked
  // against its schema before it is decoded. Without that, a derived decoder would ignore every
  // key its model does not declare, so a key that had been added, lost or renamed would decode in
  // silence and leave its expectation unmeasured while the report still read zero failures.
  //-------------------------------------------------------------------------

  /**
   * The schedule information one evaluation was given. Every subfield is optional because `null`
   * here means absent, the one exception being `eom`; see [[endOfMonth]]. A row carries exactly one
   * of `periodEnd` and `periodEnds` as a key.
   */
  final case class ScheduleInfoRow(
      start: Option[LocalDate],
      end: Option[LocalDate],
      frequency: Option[String],
      eom: Option[Boolean],
      periodEnd: Option[LocalDate],
      periodEnds: Option[Vector[LocalDate]]) {

    /** Whether this carries no schedule fact at all, and so is measured against `simple`. */
    def isAbsent: Boolean =
      start.isEmpty && end.isEmpty && frequency.isEmpty && eom.isEmpty &&
        periodEnd.isEmpty && periodEnds.isEmpty

    /**
     * The end-of-month convention in force, resolving an absent flag to the interface default of
     * `true` rather than to `false`.
     */
    def endOfMonth: Boolean = eom.getOrElse(EndOfMonthDefault)

    /**
     * Whether this carries the schedule span and frequency a boundary list needs beside it: a list
     * is read only inside the span, and the conventions that read a period end date read the
     * frequency.
     */
    def spansSchedule: Boolean = start.isDefined && end.isDefined && frequency.isDefined
  }

  val ScheduleStartKey: String = "start"

  val ScheduleEndKey: String = "end"

  val FrequencyKey: String = "frequency"

  val EndOfMonthKey: String = "eom"

  val FixedPeriodEndKey: String = "periodEnd"

  val BoundaryListKey: String = "periodEnds"

  val FixedPeriodEndVariant: String = "the fixed period end date encoding"

  val BoundaryListVariant: String = "the period boundary list encoding"

  /** The four keys every `scheduleInfo` object carries, whichever period-end encoding it uses. */
  val ScheduleInfoCommonKeys: Set[String] =
    Set(ScheduleStartKey, ScheduleEndKey, FrequencyKey, EndOfMonthKey)

  /**
   * The documented shape of the `scheduleInfo` object of a row, which has two variants: `start`,
   * `end`, `frequency` and `eom` plus '''exactly one of''' `periodEnd` and `periodEnds` - 11,910
   * objects of the first variant and 6,446 of the second. Each variant is an exact key set, so a
   * key must be '''present''' even where its value is `null`, which is what keeps `null` meaning
   * "not performed" rather than indistinguishable from a required key that has gone missing; an
   * object carrying both encodings, or neither, is refused with the keys it names.
   */
  val ScheduleInfoSchema: KeySchema =
    KeySchema.variants(
      "the scheduleInfo object of a day-count parity row",
      FixedPeriodEndVariant -> (ScheduleInfoCommonKeys + FixedPeriodEndKey),
      BoundaryListVariant -> (ScheduleInfoCommonKeys + BoundaryListKey))

  /**
   * One evaluated row of the baseline, whose `end` may be before its `start`. A value and its error
   * are never both set, and both absent means that evaluation was not performed: the ordinary case
   * for the two relative forms, and for the other two a fixture that no longer agrees with this
   * spec.
   */
  final case class DayCountRow(
      id: String,
      source: String,
      dayCount: String,
      start: LocalDate,
      end: LocalDate,
      scheduleInfo: ScheduleInfoRow,
      yearFraction: Option[Double],
      relativeYearFraction: Option[Double],
      relativeYearFractionReversed: Option[Double],
      days: Option[Int],
      error: Option[String],
      daysError: Option[String])
      extends ParityRow

  val ScheduleInfoKey: String = "scheduleInfo"

  /**
   * The documented shape of one row: the twelve keys the document carries on every one of its
   * 18,356 rows, and exactly the twelve fields [[DayCountRow]] declares, asserted against each
   * other by `the declared row and scheduleInfo key sets are the ones the row models read`.
   * Satisfying it is equality of key sets, so a gained key and a lost key are each refused by name.
   */
  val RowSchema: KeySchema =
    KeySchema.uniform(
      "a day-count parity row",
      Set(
        "id",
        "source",
        "dayCount",
        "start",
        "end",
        ScheduleInfoKey,
        "yearFraction",
        "relativeYearFraction",
        "relativeYearFractionReversed",
        "days",
        "error",
        "daysError"))

  /**
   * Reads one `scheduleInfo` object, given the variant its keys were validated against.
   * [[ParityHarness.strictVariant]] decided the variant while checking the keys, so this decoder
   * reads '''only''' the period-end key that variant names and never asks which key happens to be
   * present - the step that could disagree with the check that validated the object.
   */
  private def scheduleInfoDecoder(variant: String): Decoder[ScheduleInfoRow] =
    Decoder.instance { cursor =>
      val listed = variant == BoundaryListVariant
      for {
        start <- cursor.get[Option[LocalDate]](ScheduleStartKey)
        end <- cursor.get[Option[LocalDate]](ScheduleEndKey)
        frequency <- cursor.get[Option[String]](FrequencyKey)
        eom <- cursor.get[Option[Boolean]](EndOfMonthKey)
        periodEnd <-
          if (listed) AbsentPeriodEnd else cursor.get[Option[LocalDate]](FixedPeriodEndKey)
        periodEnds <-
          if (listed) cursor.get[Option[Vector[LocalDate]]](BoundaryListKey) else AbsentBoundaries
      } yield ScheduleInfoRow(start, end, frequency, eom, periodEnd, periodEnds)
    }

  /** The reading of `periodEnd` for the variant that does not carry that key. */
  private val AbsentPeriodEnd: Decoder.Result[Option[LocalDate]] = Right(None)

  /** The reading of `periodEnds` for the variant that does not carry that key. */
  private val AbsentBoundaries: Decoder.Result[Option[Vector[LocalDate]]] = Right(None)

  implicit val scheduleInfoRowDecoder: Decoder[ScheduleInfoRow] =
    ParityHarness.strictVariant(ScheduleInfoSchema)(scheduleInfoDecoder)

  /**
   * The row's own fields, read once the keys are known. Not implicit: the only reference to it is
   * the composition below that makes it strict about keys.
   */
  private val dayCountRowFields: Decoder[DayCountRow] = deriveDecoder[DayCountRow]

  /**
   * The decoder the fixture is read through: the row's fields behind the key check. Being the
   * implicit a loader summons is what makes every path that reads a row strict about keys.
   */
  implicit val StrictRowDecoder: Decoder[DayCountRow] =
    ParityHarness.strictObject(RowSchema)(dayCountRowFields)

  //-------------------------------------------------------------------------
  // The documented shapes as documents, and the three ways a document departs from one. A schema
  // exercised only by the fixture it already agrees with proves nothing about what it would refuse,
  // so the strictness cases decode these shapes and then the same documents with one key added,
  // one removed and one renamed.
  //-------------------------------------------------------------------------

  /** The `scheduleInfo` object of the first variant, as the committed document writes it. */
  val DocumentedFixedScheduleInfo: Json =
    Json.obj(
      ScheduleStartKey -> Json.Null,
      ScheduleEndKey -> Json.Null,
      FrequencyKey -> Json.Null,
      EndOfMonthKey -> Json.Null,
      FixedPeriodEndKey -> Json.Null)

  /** The `scheduleInfo` object of the second variant, as the committed document writes it. */
  val DocumentedListedScheduleInfo: Json =
    Json.obj(
      ScheduleStartKey -> Json.fromString("2015-01-15"),
      ScheduleEndKey -> Json.fromString("2020-01-15"),
      FrequencyKey -> Json.fromString("P1M"),
      EndOfMonthKey -> Json.fromBoolean(false),
      BoundaryListKey -> Json.arr(Json.fromString("2015-02-15"), Json.fromString("2015-03-15")))

  /** The first row of the committed document, key for key. */
  val DocumentedRow: Json =
    Json.obj(
      "id" -> Json.fromString("yf-1-1-2011-12-28-2012-02-28"),
      "source" -> Json.fromString("DayCountTest.data_yearFraction"),
      "dayCount" -> Json.fromString("1/1"),
      "start" -> Json.fromString("2011-12-28"),
      "end" -> Json.fromString("2012-02-28"),
      ScheduleInfoKey -> DocumentedFixedScheduleInfo,
      "yearFraction" -> Json.fromDoubleOrNull(1.0d),
      "relativeYearFraction" -> Json.fromDoubleOrNull(1.0d),
      "relativeYearFractionReversed" -> Json.fromDoubleOrNull(-1.0d),
      "days" -> Json.fromInt(1),
      "error" -> Json.Null,
      "daysError" -> Json.Null)

  val DocumentedRowModel: DayCountRow =
    DayCountRow(
      id = "yf-1-1-2011-12-28-2012-02-28",
      source = "DayCountTest.data_yearFraction",
      dayCount = "1/1",
      start = LocalDate.of(2011, 12, 28),
      end = LocalDate.of(2012, 2, 28),
      scheduleInfo = ScheduleInfoRow(None, None, None, None, None, None),
      yearFraction = Some(1.0d),
      relativeYearFraction = Some(1.0d),
      relativeYearFractionReversed = Some(-1.0d),
      days = Some(1),
      error = None,
      daysError = None)

  val DocumentedListedScheduleInfoModel: ScheduleInfoRow =
    ScheduleInfoRow(
      start = Some(LocalDate.of(2015, 1, 15)),
      end = Some(LocalDate.of(2020, 1, 15)),
      frequency = Some("P1M"),
      eom = Some(false),
      periodEnd = None,
      periodEnds = Some(Vector(LocalDate.of(2015, 2, 15), LocalDate.of(2015, 3, 15))))

  def rowWithScheduleInfo(info: Json): Json = withKey(DocumentedRow, ScheduleInfoKey, info)

  /**
   * The `scheduleInfo` variant a document satisfies, through the same [[ScheduleInfoSchema]] method
   * [[ParityHarness.strictVariant]] uses, so a case can state the variant without repeating the
   * rule.
   */
  def variantOf(info: Json): Option[String] =
    info.asObject.flatMap(fields => ScheduleInfoSchema.matching(fields.keys.toSet))

  /** The same object with one key added, which is the shape a newly added field arrives in. */
  def withKey(document: Json, key: String, value: Json): Json =
    document.mapObject(fields => fields.add(key, value))

  /** The same object with one key removed, which is the shape a retired field leaves behind. */
  def withoutKey(document: Json, key: String): Json =
    document.mapObject(fields => fields.remove(key))

  /**
   * The same object with one key renamed, keeping its value - a rename is a loss and a gain at
   * once, and a schema has to report both halves.
   */
  def withRenamedKey(document: Json, from: String, to: String): Json =
    document.mapObject(fields => fields.remove(from).add(to, fields(from).getOrElse(Json.Null)))

  /**
   * The message a decoder refuses a document with. A decoder that '''accepts''' it answers with a
   * description of what it accepted, so an assertion on the wording fails naming what got through.
   */
  def refusalOf[A](decoder: Decoder[A], document: Json): String =
    decoder.decodeJson(document) match {
      case Left(failure) => failure.message
      case Right(value) => s"the decoder accepted $value"
    }

  /**
   * The least a row model can be: the identity of a fixture row and the case it came from, which is
   * what lets any committed baseline stand in for a fixture of its own.
   */
  final case class HarnessProbeRow(id: String, source: String) extends ParityRow

  /**
   * The decoder of [[HarnessProbeRow]], written out because the fallback from `id` to `source` is
   * an identity rule of these documents rather than a shape circe can infer: a row carries an `id`
   * exactly where its `source` cannot identify it alone.
   */
  implicit val harnessProbeRowDecoder: Decoder[HarnessProbeRow] = Decoder.instance { cursor =>
    for {
      source <- cursor.get[String]("source")
      id <- cursor.getOrElse[String]("id")(source)
    } yield HarnessProbeRow(id, source)
  }

  //-------------------------------------------------------------------------
  // The two schedule-information adapters, both immutable and reading nothing outside their own
  // fields, so a row's evaluation is a pure function of the row.
  //-------------------------------------------------------------------------

  /**
   * The fixed-period-end adapter: `periodEndDate` answers `fixedPeriodEnd` for every date it is
   * given, which is the behaviour every expectation recorded against it was produced with.
   */
  final case class StubScheduleInfo(
      override val startDate: Option[LocalDate],
      override val endDate: Option[LocalDate],
      override val frequency: Option[Frequency],
      override val isEndOfMonthConvention: Boolean,
      fixedPeriodEnd: Option[LocalDate])
      extends DayCount.ScheduleInfo {

    override def periodEndDate(date: LocalDate): Option[LocalDate] = fixedPeriodEnd
  }

  /**
   * The boundary-list adapter: `periodEndDate(d)` is the first boundary strictly after `d` while
   * `scheduleStart <= d < scheduleEnd`, and `None` anywhere else - this library's reading of a date
   * the schedule does not contain, which the convention that reads it answers for itself. The
   * lookup is a linear scan of an immutable vector, bounded by the sixty-one boundaries of the
   * longest schedule in the fixture.
   */
  final case class ListedScheduleInfo(
      scheduleStart: LocalDate,
      scheduleEnd: LocalDate,
      override val frequency: Option[Frequency],
      override val isEndOfMonthConvention: Boolean,
      boundaries: Vector[LocalDate])
      extends DayCount.ScheduleInfo {

    override val startDate: Option[LocalDate] = Some(scheduleStart)

    override val endDate: Option[LocalDate] = Some(scheduleEnd)

    override def periodEndDate(date: LocalDate): Option[LocalDate] =
      if (date.isBefore(scheduleStart) || !date.isBefore(scheduleEnd)) {
        None
      } else {
        boundaries.find(boundary => boundary.isAfter(date))
      }
  }

  /**
   * The population a row belongs to, the part of its identity before the first `'-'`;
   * [[MinimumRowsByFamily]] is the closed set of families the baseline must carry.
   */
  def familyOf(row: DayCountRow): String = row.id.takeWhile(character => character != '-')

  /**
   * Builds the schedule information a row was evaluated with, or names why it cannot be built. The
   * three shapes map onto the three outcomes and nothing is inferred beyond them: no schedule fact
   * becomes `ScheduleInfo.simple`, a fixed period end date becomes [[StubScheduleInfo]], a boundary
   * list becomes [[ListedScheduleInfo]]. A row carrying both encodings, or a list without its span,
   * is reported as a fixture disagreement instead of measured against a guess.
   */
  def scheduleInfoFor(row: DayCountRow): IO[Either[List[String], DayCount.ScheduleInfo]] = {
    val captured = row.scheduleInfo
    if (captured.isAbsent) {
      IO.pure(Right(DayCount.ScheduleInfo.simple))
    } else {
      frequencyOf(captured.frequency).map { frequency =>
        (captured.periodEnd, captured.periodEnds) match {
          case (Some(fixed), Some(boundaries)) =>
            Left(
              List(fixtureDefect(
                s"the schedule information carries both a fixed period end date of $fixed and a " +
                  s"boundary list of ${boundaries.size} dates; section 6 of the capture README " +
                  "states that a row carries exactly one of the two encodings, and the two have " +
                  "different readings of periodEndDate, so this row is reported rather than " +
                  "measured against a guess")))
          case (None, Some(boundaries)) =>
            (captured.start, captured.end) match {
              case (Some(scheduleStart), Some(scheduleEnd)) if boundaries.nonEmpty =>
                Right(
                  ListedScheduleInfo(
                    scheduleStart,
                    scheduleEnd,
                    frequency,
                    captured.endOfMonth,
                    boundaries))
              case _ =>
                Left(
                  List(fixtureDefect(
                    s"the schedule information carries a boundary list of ${boundaries.size} " +
                      s"dates with schedule start ${captured.start} and end ${captured.end}; the " +
                      "list is read only inside a schedule's own span, so a list that is empty " +
                      "or has no span cannot be evaluated")))
            }
          case (fixedPeriodEnd, None) =>
            Right(
              StubScheduleInfo(
                captured.start,
                captured.end,
                frequency,
                captured.endOfMonth,
                fixedPeriodEnd))
        }
      }
    }
  }

  /** Parses the frequency a row names, failing the effect for a name that cannot be parsed. */
  private def frequencyOf(name: Option[String]): IO[Option[Frequency]] =
    name.traverse(text => ParityHarness.raise(Frequency.parse(text)))

  /**
   * Resolves the convention a row names, reusing the one already resolved for that name. The
   * subject is resolved '''by name''' rather than through a constant, so the lookup the fixture
   * depends on is part of what is measured; a name that cannot be resolved is a defect rather than
   * a parity result, so [[ParityHarness.raiseNec]] fails the effect.
   */
  def dayCountFor(conventions: Ref[IO, Map[String, DayCount]], name: String): IO[DayCount] =
    conventions.get.flatMap { resolved =>
      resolved.get(name) match {
        case Some(dayCount) => IO.pure(dayCount)
        case None =>
          ParityHarness
            .raiseNec(DayCount.parse(name))
            .flatMap(dayCount => conventions.update(_.updated(name, dayCount)).as(dayCount))
      }
    }

  /**
   * Measures one row, answering with everything that differed: every operation is measured and the
   * messages concatenated, so a row differing in two of them names both.
   */
  def checkRow(conventions: Ref[IO, Map[String, DayCount]], row: DayCountRow): IO[List[String]] =
    scheduleInfoFor(row).flatMap {
      case Left(disagreements) => IO.pure(disagreements)
      case Right(info) =>
        dayCountFor(conventions, row.dayCount).flatMap(dayCount => measureRow(dayCount, info, row))
    }

  /**
   * Applies the measurements of a row to the convention it names. That convention's name is itself
   * an expectation: a lenient lookup that resolved to the wrong member, or a `Bus/252` name that
   * lost its calendar, would otherwise be measured as the arithmetic of whatever it resolved to.
   */
  private def measureRow(
      dayCount: DayCount,
      info: DayCount.ScheduleInfo,
      row: DayCountRow): IO[List[String]] = {

    val name = ParityHarness.assertExact("day count name", dayCount.name, row.dayCount)
    List(
      checkYearFraction(dayCount, info, row),
      checkDays(dayCount, row),
      checkRelative(dayCount, info, row)
    ).sequence.map(messages => name ::: messages.flatten)
  }

  /**
   * Measures the year fraction, or the refusal recorded in its place, whose '''type''' alone is
   * asserted through [[ParityHarness.attemptArgCheck]].
   */
  private def checkYearFraction(
      dayCount: DayCount,
      info: DayCount.ScheduleInfo,
      row: DayCountRow): IO[List[String]] =

    (row.yearFraction, row.error) match {
      case (Some(expected), None) =>
        measured("yearFraction", dayCount.yearFraction(row.start, row.end, info))(actual =>
          ParityHarness.assertParity("yearFraction", actual, expected))
      case (None, Some(message)) =>
        ParityHarness.attemptArgCheck(s"yearFraction, which Java refused with '$message'")(
          dayCount.yearFraction(row.start, row.end, info))
      case (Some(expected), Some(message)) =>
        IO.pure(
          List(fixtureDefect(
            s"the row carries both a year fraction of $expected and the error '$message'; a " +
              "value and its error are never both set, so it is not stated whether Java " +
              "answered or refused")))
      case (None, None) =>
        IO.pure(
          List(fixtureDefect(
            "the row carries neither a year fraction nor an error, so nothing about " +
              "yearFraction would be measured; the capture performs that evaluation for every " +
              "row of this fixture")))
    }

  /**
   * Measures the day count, or the refusal recorded in its place. The comparison is exact and the
   * value stays an `Int`: a tolerance applied to a count would admit an answer that is wrong.
   */
  private def checkDays(dayCount: DayCount, row: DayCountRow): IO[List[String]] =
    (row.days, row.daysError) match {
      case (Some(expected), None) =>
        measured("days", dayCount.days(row.start, row.end))(actual =>
          ParityHarness.assertExact("days", actual, expected))
      case (None, Some(message)) =>
        ParityHarness.attemptArgCheck(s"days, which Java refused with '$message'")(
          dayCount.days(row.start, row.end))
      case (Some(expected), Some(message)) =>
        IO.pure(
          List(fixtureDefect(
            s"the row carries both a day count of $expected and the error '$message'; a value " +
              "and its error are never both set, so it is not stated whether Java answered or " +
              "refused")))
      case (None, None) =>
        IO.pure(
          List(fixtureDefect(
            "the row carries neither a day count nor an error, so nothing about days would be " +
              "measured; the capture performs that evaluation for every row of this fixture")))
    }

  /**
   * Measures the relative year fraction in both directions, from the expectations
   * [[expectedForward]] and [[expectedReversed]] supply; having no order check, it is never
   * asserted to refuse, not even on the out-of-order rows.
   */
  private def checkRelative(
      dayCount: DayCount,
      info: DayCount.ScheduleInfo,
      row: DayCountRow): IO[List[String]] =

    List(
      expectedForward(row).fold(IO.pure(List.empty[String]))(expected =>
        measured(
          ForwardLabel,
          dayCount.relativeYearFraction(row.start, row.end, info)
        )(actual => ParityHarness.assertParity(ForwardLabel, actual, expected))),
      expectedReversed(row).fold(IO.pure(List.empty[String]))(expected =>
        measured(
          ReversedLabel,
          dayCount.relativeYearFraction(row.end, row.start, info)
        )(actual => ParityHarness.assertParity(ReversedLabel, actual, expected)))
    ).sequence.map(_.flatten)

  private val ForwardLabel: String = "relativeYearFraction"

  private val ReversedLabel: String = "relativeYearFraction with the dates reversed"

  /**
   * The expectation for the relative year fraction as the dates were supplied: the recorded value
   * where the fixture carries one, otherwise the row's own year fraction, the dates being in order.
   * Where the row carries neither, its year fraction was refused and nothing is measured.
   */
  def expectedForward(row: DayCountRow): Option[Double] =
    row.relativeYearFraction.orElse(row.yearFraction)

  /**
   * The expectation for the relative year fraction over the row's dates reversed: the recorded
   * value where the fixture carries one, and otherwise `-yearFraction`, '''but only for a span of
   * non-zero length'''. For a zero-length span the second date is not before the first, so the
   * answer is `+yearFraction` rather than its negation, which `1/1` makes a difference of `2`.
   */
  def expectedReversed(row: DayCountRow): Option[Double] =
    row.relativeYearFractionReversed.orElse(
      if (row.start.isBefore(row.end)) row.yearFraction.map(expected => -expected) else None)

  /**
   * Compares what a call produced, reporting a refusal as the discrepancy it is: the call is made
   * inside an effect and its outcome turned back into a value, so a refusal where the baseline
   * holds a value does not take the rest of the row, or of the fixture, with it.
   */
  private def measured[A](label: String, thunk: => A)(compare: A => List[String]): IO[List[String]] =
    IO.delay[A](thunk).attempt.map {
      case Right(value) => compare(value)
      case Left(error) =>
        List(s"$label: expected a value, but the call failed with ${describe(error)}")
    }

  /**
   * Names a way in which the fixture has stopped agreeing with this spec: not a parity result, and
   * worded so that it cannot be read as one. It is reported through the same channel as a
   * discrepancy so that it reaches the published report, because a row that measures nothing is the
   * one outcome a `failed == 0` check cannot see.
   */
  private def fixtureDefect(explanation: String): String = s"fixture disagreement: $explanation"

  /** Renders a thrown error as its type and message, for a report that has to stand alone. */
  private def describe(error: Throwable): String =
    s"${error.getClass.getName}: ${Option(error.getMessage).getOrElse("no message")}"
}
