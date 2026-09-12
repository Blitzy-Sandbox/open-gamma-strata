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
 * Parity of the day-count conventions against the Java baseline.
 *
 * This is the measurement that pins the day-count layer of the port: the year fraction of every
 * standard convention, the relative year fraction in both directions, the day count itself, the
 * business-day convention `Bus/252 BRBD` with its weekend-only fallback outside the calendar's
 * range, and - the part no unit spec can cover as broadly - the four conventions that read
 * schedule information, measured against both shapes of schedule information the capture wrote.
 *
 * The gate that consumes it (AAP section 0.10.1, Gate 3, for the user's Rule 2) runs
 *
 * {{{
 * sbt -batch "testOnly *ParitySpec"
 * }}}
 *
 * and then reads `<parity.report.dir>/daycount.json`, requiring `failed == 0` and copying `rows`
 * and `passed` into `target/gate-report.md`. The `ParitySpec` suffix of this class, its package,
 * the fixture stem `daycount` and the five keys of the report document are therefore all part of
 * that contract and none of them may drift: renaming the class removes this measurement from the
 * gate without failing anything.
 *
 * ===Every number goes through the harness===
 *
 * A year fraction is compared by [[ParityHarness.assertParity]], which passes only inside `1e-9`
 * absolute '''and''' `1e-9` relative. No tolerance operator of the test framework appears in this
 * file - no `===` with a tolerance, no `+-`, and no `shouldBe` on a `Double` - because each of
 * those would put the bound somewhere other than the one place the rule is stated. A day count is
 * an `Int` and is compared exactly by [[ParityHarness.assertExact]], never widened to a `Double`.
 *
 * ===The schedule information is the subtle part===
 *
 * Seventeen of the twenty-one standard conventions ignore schedule information entirely. Four read
 * it: `Act/Act ICMA` reads the schedule end date, the period end date and the frequency,
 * `Act/365L` reads the period end date and the frequency, `30E/360 ISDA` reads the schedule end
 * date, and `30U/360` reads the end-of-month flag
 * (`modules/basics/src/main/java/com/opengamma/strata/basics/date/StandardDayCounts.java:71-74,212-213,419,345`).
 * The captured document therefore carries the schedule information each evaluation was given, in
 * one of three shapes, and this spec reproduces each of them exactly:
 *
 *   - '''Nothing at all.''' Every subfield absent, which is precisely the set of rows the capture
 *     evaluated through the Java two-argument overload against `DayCounts.SIMPLE_SCHEDULE_INFO`
 *     (capture README, section 6). Those rows are measured against the port's own
 *     `DayCount.ScheduleInfo.simple` - the value its two-argument overloads pass, so the captured
 *     evaluation is reproduced with the same schedule information rather than simulated by an
 *     adapter that happens to answer the same way.
 *   - '''One fixed period end date''' - `periodEnd`. This reproduces the `DayCountTest` stub, whose
 *     `getPeriodEndDate(LocalDate date)` '''ignores its argument''' and returns the single date it
 *     was built with (`modules/basics/src/test/java/com/opengamma/strata/basics/date/DayCountTest.java:1373-1416`).
 *     That is not a simplification to be improved on: reproducing the stub faithfully is what makes
 *     the expectations captured through it reproducible at all. [[DayCountParitySpec.StubScheduleInfo]].
 *   - '''The boundary list of a real schedule''' - `periodEnds`. There `periodEndDate(d)` is the
 *     first boundary '''strictly''' after `d`, and only when `start <= d < end`; anywhere else it
 *     is `None`. No interpolation, no clamping and no nearest-boundary rule: `None` outside every
 *     period is the port's documented behaviour where the interface being ported raised
 *     (`modules/basics/src/main/scala/com/opengamma/strata/basics/schedule/Schedule.scala`, and
 *     `modules/basics/src/main/java/com/opengamma/strata/basics/schedule/Schedule.java:332-338`
 *     for what it replaces). [[DayCountParitySpec.ListedScheduleInfo]].
 *
 * `null` inside `scheduleInfo` has exactly one meaning - absent, therefore `None` - with one
 * exception a reader has to know: `eom` absent means the '''interface default''', which is `true`
 * (`DayCounts.SIMPLE_SCHEDULE_INFO` answers `true` from `isEndOfMonthConvention` while raising from
 * the other four accessors). Mapping it to `false` would quietly re-point every `30U/360` row at
 * the other of that convention's two day-of-month rules.
 *
 * ===`Bus/252` needs no reference data, and no special case===
 *
 * `Bus/252 BRBD` is resolved by the same `DayCount.parse` call as every other subject, and the
 * port resolves its calendar against the calendars '''built into the library''' - constant data,
 * not ambient reference data (AAP section 0.6.5) - so nothing here supplies or threads a
 * `ReferenceData`, and the convention is measured by exactly the code path that measures the other
 * twenty-one. The two identities the Java tests assert for it, `yearFraction` equal to the
 * business days between the dates divided by `252` and `days` equal to that same count
 * (`modules/basics/src/test/java/com/opengamma/strata/basics/date/Business252DayCountTest.java`),
 * are what the captured numbers '''are''': they were produced by the Java implementation, so
 * measuring the port against them is the assertion, and recomputing either side here would replace
 * a captured baseline with a hand-derived one. The nine `bus252` rows probe a span inside the
 * calendar's 1950-2099 range, spans wholly outside it in both directions, and spans crossing each
 * end, where Java falls back to a weekend-only test - and the reversed-date row for this
 * convention is an ordinary member of the out-of-order family below.
 *
 * ===The two refusals, and why they are refusals rather than failures===
 *
 * AAP section 0.3.3 keeps exactly two day-count throw families as documented `ArgCheck`
 * preconditions instead of moving them into the error channel, because both are violations of a
 * caller contract that do not depend on the values of the data:
 *
 *   1. '''Dates out of order.''' `yearFraction(later, earlier)` and `days(later, earlier)` refuse,
 *      for every convention (`DayCountTest.test_wrongOrder`). The 22 `order` rows of the fixture
 *      carry an `error` for both operations.
 *   2. '''Required schedule information absent.''' Where Java raised
 *      `UnsupportedOperationException` from an accessor of `SIMPLE_SCHEDULE_INFO`, the port refuses
 *      through `ArgCheck` off the absent `Option`. 1,028 rows carry that `error`, and the five
 *      `missing` rows state the case once per convention commonly said to need schedule
 *      information - recording that `Act/365 Actual` and `30U/360` do '''not''' raise, which makes
 *      that evidence rather than folklore.
 *
 * Both are observed with [[ParityHarness.attemptArgCheck]], which runs the call inside an effect
 * and turns the throw back into a value, so a refusing row stays in the report beside every other
 * row instead of ending the run. Neither `intercept` nor `try`/`catch` appears in this file. Only
 * the '''type''' of the refusal is asserted: the captured text is the message of the implementation
 * being replaced, and message parity is deliberately not claimed here - AAP section 0.8.3 records
 * the exception-to-`Either` and message divergences - so the captured message is quoted in the
 * diagnostic and never compared.
 *
 * An `error` row is not a gap in the data, and the reverse also holds: a row that carries neither a
 * value nor an error for an operation the capture always performs is a fixture that has stopped
 * agreeing with this spec, and is reported as that rather than silently measured as nothing.
 *
 * ===The relative year fraction is total, and is measured in both directions===
 *
 * `relativeYearFraction` has no order check by design: it swaps a reversed pair and negates the
 * result, which is why it answers where `yearFraction` refuses. It is therefore '''never''' asserted
 * to throw for a reversed pair. The fixture captured it, in both directions, exactly where it
 * carries information - the 201 `data_yearFraction` rows, whose Java consumers assert the reverse
 * against `-expected`, and the 20 out-of-order rows where it succeeds while the other two
 * operations refuse. Where a row carries the captured value, that value is the expectation. Where
 * it does not, the expectation is the identity the Java contract documents - "will be negative if
 * the first date is after the second date" - applied to the row's own year fraction, and applied
 * only where it holds: for a '''zero-length''' span Java answers `+yearFraction` rather than its
 * negation, because the second date is then not before the first, and `1/1` - which answers `1`
 * for any pair, a zero-length span included - is what makes that observable rather than academic.
 * [[DayCountParitySpec.expectedForward]] and [[DayCountParitySpec.expectedReversed]] are that rule.
 *
 * ===The fixture is the authority===
 *
 * `daycount-baseline.json` was captured from the untouched Java modules by
 * `tools/parity-capture/capture-baseline.jsh`, which cross-checked its rows against the constants
 * of the Java tests before writing them - including the hard anchor that `Act/Act ISDA` from
 * 2011-12-28 to 2012-02-28 equals `4/365 + 58/366`. It is never edited here, no expectation is
 * ever "corrected" and no row is ever skipped: a row that disagrees with the port means the port is
 * wrong, and reporting it is the whole job. Because a gate that reads `failed == 0` cannot tell a
 * complete measurement from a thinned one, the second test below asserts the population the
 * baseline is required to carry.
 *
 * The `SIMPLE_30_360` and `SIMPLE_30_360DAYS` markers of `DayCountTest` are table flags rather than
 * values - `Double.NaN` and `0` standing for "compute it with `calc360`" - and the capture resolves
 * them as the Java consumers do, so no row reaches this spec carrying `"NaN"` as a year fraction. A
 * `"NaN"` expectation here would be a defect of the fixture, and it is reported as an ordinary
 * discrepancy by the same comparator that measures everything else, because a not-a-number is at
 * parity only with a not-a-number.
 *
 * ===No timing===
 *
 * Parity is a value comparison. Nothing here, and nothing in the report it publishes, asserts a
 * duration. The cost of 18,356 rows is controlled structurally instead: each convention is resolved
 * once per distinct name for the whole run, and the calendars behind `Bus/252` are the built-in
 * lazily generated set, so the rule generation is paid once per JVM.
 */
class DayCountParitySpec extends AsyncFunSuite with AsyncIOSpec with Matchers {

  import DayCountParitySpec._

  /*
   * The measurement is declared first, deliberately: the report it publishes is the artifact
   * Gate 3 collects, so it is written on every run of this suite rather than only on the runs
   * where some other case happens to pass first. `runFixture` writes it before returning, and
   * `failIfAny` is what then decides the verdict, so the counts survive a failing run.
   */
  test("the day count conventions reproduce the Java baseline exactly") {
    for {
      // One resolution per distinct convention name for the whole run, behind a cats-effect
      // reference over an immutable map: no `var`, no mutable collection, nothing ambient. The
      // rows are measured one after another, so the map is built in fixture order and a repeated
      // name costs a lookup.
      conventions <- Ref.of[IO, Map[String, DayCount]](Map.empty)
      report <- ParityHarness.runFixture[DayCountRow](FixtureName, FixtureResource, RowSchema)(row =>
        checkRow(conventions, row))
      _ <- ParityHarness.failIfAny(report)
    } yield succeed
  }

  test("the fixture carries the population the day-count baseline is required to measure") {
    ParityHarness.loadStrict[DayCountRow](FixtureResource, RowSchema).map { rows =>
      val families = rows.groupBy(familyOf).view.mapValues(_.size).toMap
      withClue(
        s"fixture rows: ${rows.size}; rows by id family: " +
          s"${families.toVector.sortBy(_._1).mkString(", ")}: ") {
        // The floors are the contract of the capture, restated on the consuming side so that a
        // reduced fixture fails here instead of reporting a green measurement of less. They are
        // floors rather than equalities so that extending the coverage stays possible.
        rows.size should be >= MinimumRows
        rows.map(_.id).distinct should have size rows.size.toLong
        MinimumRowsByFamily.foreach { case (family, minimum) =>
          withClue(s"id family '$family': ") {
            families.getOrElse(family, 0) should be >= minimum
          }
        }
        // A row of unstated provenance is reported rather than counted towards a floor it does
        // not belong to.
        rows.map(familyOf).distinct.filterNot(MinimumRowsByFamily.contains) shouldBe empty
        // Every convention the port is required to reproduce appears, and appears in both
        // generated populations - the one with no schedule information and the one with a real
        // schedule - so thinning a subject out of either cannot pass on the row count alone.
        val subjects = rows.map(_.dayCount).distinct.toSet
        RequiredDayCounts.filterNot(subjects.contains) shouldBe empty
        GeneratedFamilies.foreach { family =>
          withClue(s"subjects missing from the '$family' family: ") {
            val covered = rows.filter(row => familyOf(row) == family).map(_.dayCount).toSet
            RequiredDayCounts.filterNot(covered.contains) shouldBe empty
          }
        }
        // Each operation the capture always performs carries exactly one of a value and an error.
        // These are the two row kinds this spec measures differently, so the distinction is
        // asserted rather than assumed: a row carrying neither would measure nothing at all.
        rows.filter(row => row.yearFraction.isDefined == row.error.isDefined).map(_.id) shouldBe empty
        rows.filter(row => row.days.isDefined == row.daysError.isDefined).map(_.id) shouldBe empty
        rows.count(_.error.isDefined) should be >= MinimumYearFractionRefusals
        rows.count(_.daysError.isDefined) should be >= MinimumDaysRefusals
        // Every out-of-order row refuses both operations, for every subject: that family is the
        // whole of the first precondition's coverage.
        rows
          .filter(row => familyOf(row) == OutOfOrderFamily)
          .filter(row => row.error.isEmpty || row.daysError.isEmpty || !row.start.isAfter(row.end))
          .map(_.id) shouldBe empty
        // The relative forms are captured as a pair or not at all, and enough rows carry them for
        // the captured expectation - rather than the derived identity - to be what is measured
        // where the Java tests assert it.
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
  // ===Why these cases are in a day-count parity spec===
  //
  // The AAP's frozen test inventory (sections 0.3.1 and 0.4.1) plans `parity/ParityHarness.scala`
  // together with exactly five basics `*ParitySpec` files and no spec of its own for the harness,
  // so the harness's contract has to be asserted from inside one of the specs that consume it.
  // This is the heaviest consumer of it by a wide margin - 18,356 fixture rows, every comparator,
  // both refusal families, the `raise` and `raiseNec` lifts and the published `daycount.json` - so
  // it is the spec whose measurement a silently weakened harness decision invalidates most
  // completely, and therefore the spec these cases belong in. They can reach the decisions at all
  // because the seams that carry them - `decodeRows`, `runFixtureIn`, `writeReportTo`,
  // `reportDirectoryFrom`, `raise`, `raiseNec`, `MaxFixtureCharacters`, `MaxFixtureRows` and
  // `ReportDirectoryProperty` - are `private[parity]` and this file is in that package.
  //
  // ===Why the harness needs its own cases at all===
  //
  // An apparatus that measures its own subject cannot also vouch for itself: a fixture row either
  // agrees with the port or it does not, and either way it says nothing about whether the
  // comparison that decided it was the comparison Rule 2 calls for. Turn the conjunction of the
  // two tolerance bounds into a disjunction, drop the sign of a presence mismatch, count
  // discrepancies as rows, or accept a fixture with no rows at all, and the two measurements above
  // - and every other parity spec of this module - still pass while measuring something weaker
  // than required; in the last case, nothing at all. Each case below therefore fixes one decision
  // of the harness against inputs chosen for that decision, and every one is deterministic: fixed
  // values, committed fixtures, no generated data, no timing and no ordering that depends on the
  // runner.
  //
  // ===Nothing here writes where Gate 3 looks===
  //
  // The measurement at the top of this suite publishes the real `daycount.json` into the directory
  // named by `parity.report.dir`. The cases below that publish anything go through
  // `ParityHarness.runFixtureIn` or `ParityHarness.writeReportTo` into a temporary directory each
  // creates and removes for itself, under probe stems: never through `runFixture`, never into the
  // configured directory, and never under one of the six stems the gate collects. A report written
  // here while measuring the harness would otherwise be indistinguishable from a measurement of
  // the port, which is exactly the confusion the report exists to prevent.
  //-------------------------------------------------------------------------

  // The row model the cases below decode with, `HarnessProbeRow`, and its decoder are declared in
  // the companion object beside the other row models of this file, and reach this body through the
  // wildcard import above. A row model declared inside the class would carry an outer reference
  // its synthesized type test cannot check at run time, which this build rejects.

  /** The smallest committed baseline that carries row identities, used by the driver cases. */
  private val HarnessFxFixture: String = "parity/fx-baseline.json"

  /** A committed baseline of several hundred rows, so the ceilings are exercised on real data. */
  private val HarnessScheduleFixture: String = "parity/schedule-baseline.json"

  /** Every baseline this module commits, in the order the gate reports them. */
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
   * Creates an empty directory, hands it to the case, and removes it and anything the case wrote
   * afterwards.
   *
   * Reports published while measuring the harness go here rather than into the directory the gate
   * reads, and the finalizer runs on every outcome - in the same effect that created the directory
   * - so a failing case leaves nothing behind for the next run to find.
   *
   * @param use  the case, given the absolute path of the directory
   * @return the effect of the case, with creation and removal around it
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

  //-------------------------------------------------------------------------
  // The tolerance rule.
  //-------------------------------------------------------------------------
  test("the shared harness: assertParity passes a pair inside both bounds") {
    IO {
      ParityHarness.assertParity("equal", 1.0d, 1.0d) shouldBe Nil
      ParityHarness.assertParity("equal zero", 0.0d, 0.0d) shouldBe Nil
      // A difference of two to the power of minus thirty-one: exactly representable next to
      // one, so the difference computed here is exact rather than approximate. It is inside
      // the absolute bound of 1e-9, and inside the relative bound because the scale is one.
      ParityHarness.assertParity("just inside", 1.0d, 1.0d + HarnessJustInsideBothBounds) shouldBe Nil
      ParityHarness.assertParity("negative, just inside", -1.0d, -1.0d - HarnessJustInsideBothBounds) shouldBe Nil
    }
  }

  test("the shared harness: assertParity rejects a difference that only the relative bound admits") {
    IO {
      // A thousandth, between values of the order of 1e12: a relative difference of 1e-15,
      // which the relative bound admits comfortably, and an absolute difference a million
      // times the absolute bound. Were the rule a disjunction, this pair would pass, and a
      // parity measurement of large quantities would be worthless.
      val discrepancies = ParityHarness.assertParity("large magnitude", 1e12d, 1e12d + 1e-3d)
      discrepancies should have size 1L
      discrepancies.head should include("met: false")
      discrepancies.head should include("met: true")
    }
  }

  test("the shared harness: assertParity rejects a difference that only the absolute bound admits") {
    IO {
      // One value twice the other, both of the order of 1e-15: an absolute difference far
      // inside 1e-9 and a relative difference of fifty percent. Were the rule a disjunction,
      // this pair would pass, and a parity measurement of small quantities would be worthless.
      val discrepancies = ParityHarness.assertParity("near zero", 1e-15d, 2e-15d)
      discrepancies should have size 1L
      discrepancies.head should include("met: true")
      discrepancies.head should include("met: false")
    }
  }

  test("the shared harness: assertParity rejects a difference outside both bounds") {
    IO {
      // Two to the power of minus twenty-nine next to one: outside the absolute bound, and
      // outside the relative bound at a scale of one. Both conjuncts reject it.
      val discrepancies = ParityHarness.assertParity("outside", 1.0d, 1.0d + HarnessJustOutsideBothBounds)
      discrepancies should have size 1L
      discrepancies.head should include("met: false")
    }
  }

  test("the shared harness: assertParity treats identical values outside the finite range as parity") {
    IO {
      // Bit identity is decided before any arithmetic, which is the only way these can pass:
      // the difference of two infinities is not a number, and every comparison against a value
      // that is not a number is false.
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
      // No difference is computed for such a pair, so no arithmetic on a value outside the
      // range appears in the message.
      opposedInfinities.head should not include "difference NaN"
    }
  }

  test("the shared harness: assertParity treats positive and negative zero as parity") {
    IO {
      // The two zeros are not bit identical, so they reach the tolerance test, where their
      // difference is zero and both bounds hold. Signed zero is a representation rather than a
      // quantity, and the captured baselines carry negative zero.
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

  //-------------------------------------------------------------------------
  // The other comparators.
  //-------------------------------------------------------------------------
  test("the shared harness: assertParityOpt compares presence as an expectation in its own right") {
    IO {
      ParityHarness.assertParityOpt("absent", None, None) shouldBe Nil
      ParityHarness.assertParityOpt("present", Some(1.0d), Some(1.0d)) shouldBe Nil
      ParityHarness.assertParityOpt("beyond tolerance", Some(1.0d), Some(2.0d)) should have size 1L
      // A stub the port produced where the baseline has none, and the reverse, are different
      // facts and are reported differently.
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
      // A shifted sequence differs at every index; reporting the length once is what keeps the
      // one fact worth knowing visible.
      val lengths = ParityHarness.assertParitySeq("lengths", Vector(1.0d), Vector(1.0d, 2.0d))
      lengths should have size 1L
      lengths.head should include("1 elements")
      lengths.head should include("2 elements")
      // Equal lengths are compared element by element, each under its own indexed label, so
      // two differing positions are two discrepancies rather than one.
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
      // wrong rather than close, which is why the amounts are captured as their decimal text.
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
      // The value that should not have been produced is named, because that is the evidence.
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
      // On failure the check is not run at all: its comparisons would have nothing to compare,
      // and reporting them as further discrepancies would multiply one fact into several.
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
      // A precondition enforced by a different failure is not the precondition the baseline
      // recorded, so the type that did refuse is named.
      wrongType should have size 1L
      wrongType.head should include("IllegalStateException")
      // A call that returned refused nothing at all.
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
      // Every cause survives the lift, joined, because an input the port refuses for two
      // reasons is not diagnosable from one of them.
      several.left.map(_.getMessage) shouldBe Left("first cause; second cause")
    }
  }

  //-------------------------------------------------------------------------
  // Loading a fixture.
  //-------------------------------------------------------------------------
  test("the shared harness: load reads a committed baseline into its rows, in fixture order") {
    for {
      rows <- ParityHarness.load[HarnessProbeRow](HarnessFxFixture)
      text <- Resources.readClasspathText(HarnessFxFixture)
      larger <- ParityHarness.load[HarnessProbeRow](HarnessScheduleFixture)
    } yield {
      rows.size should be >= 14
      rows.map(_.id).head shouldBe "cross-rate-triangulating-matrix"
      rows.filter(_.id.isEmpty) shouldBe empty
      // Fixture order is asserted against the document itself rather than against a copy of
      // its row names: each row's identity is located in the text the resource carries, and
      // those positions have to ascend in the order the rows were decoded. A reordering
      // decoder, or one that indexed rows by identity, would fail here while a hard-coded
      // list of names would keep passing as the fixture grew.
      val offsets = rows.map(row => text.indexOf("\"" + row.id + "\""))
      offsets.filter(_ < 0) shouldBe empty
      offsets.distinct should have size offsets.size.toLong
      offsets shouldBe offsets.sorted
      // A fixture of several hundred rows goes through the same ceilings untouched, so the
      // bounds are not quietly refusing real data.
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
    // The finding this case exists for: an empty array decodes perfectly well into no rows,
    // and a report of zero rows, zero passed and zero failed is exactly the shape of a
    // measurement that succeeded. Replacing a baseline with an empty array would therefore
    // retire every comparison in it while Gate 3 still read a pass.
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
      // Each of the three is a different fact about the fixture, and none of them is the port
      // disagreeing with Java; absorbing any of them into a parity result would make the
      // gate's verdict worthless.
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
        // roughly 12.7 MiB and 18,356 rows. Both ceilings are set clear of it, so enforcing them
        // cannot start refusing legitimate data, and the largest fixture is loaded here rather
        // than assumed.
        ParityHarness.MaxFixtureCharacters should be > 16 * 1024 * 1024
        ParityHarness.MaxFixtureRows should be > 20000
        measured.map(_._2).max should be > 18000
      }
  }

  //-------------------------------------------------------------------------
  // Where the report goes.
  //-------------------------------------------------------------------------
  test("the shared harness: the report directory must be configured, and must be absolute") {
    IO {
      // Nothing configured has no fallback: a default would let a misconfigured run publish
      // where the gate never looks, and a gate that finds no report cannot tell that apart
      // from a measurement nobody made.
      ParityHarness.reportDirectoryFrom(None) match {
        case Left(message) => message should include(ParityHarness.ReportDirectoryProperty)
        case Right(directory) => fail(s"an unset property was accepted as $directory")
      }
      ParityHarness.reportDirectoryFrom(Some("   ")).isLeft shouldBe true
      // A relative path resolves against the working directory of a forked test JVM, which is
      // not the one directory the gate collects from.
      ParityHarness.reportDirectoryFrom(Some("target/parity-report")) match {
        case Left(message) => message should include("relative")
        case Right(directory) => fail(s"a relative property was accepted as $directory")
      }
      ParityHarness.reportDirectoryFrom(Some("/tmp/parity-report-probe")) shouldBe
        Right(Paths.get("/tmp/parity-report-probe"))
      // The build supplies exactly that: an absolute path, which is what the measurement at the
      // top of this suite then publishes `daycount.json` into.
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
        // A stem becomes part of a path, so one carrying a separator would publish outside the
        // directory the gate collects.
        nested.left.map(_.getClass.getName) shouldBe Left("java.lang.IllegalArgumentException")
        windows.isLeft shouldBe true
        blank.isLeft shouldBe true
        plain.getFileName.toString shouldBe "probe.json"
        plain.getParent shouldBe directory
        // The three refused stems wrote nothing anywhere: the plain one is the only file in the
        // directory afterwards.
        published shouldBe 1
      }
    }
  }

  //-------------------------------------------------------------------------
  // The driver.
  //-------------------------------------------------------------------------
  test("the shared harness: runFixtureIn counts rows that matched and discrepancies that did not") {
    withHarnessTempDirectory { directory =>
      for {
        rows <- ParityHarness.load[HarnessProbeRow](HarnessFxFixture)
        firstId = rows.head.id
        report <- ParityHarness.runFixtureIn[HarnessProbeRow](directory, "probe-counts", HarnessFxFixture) { row =>
          IO.pure(if (row.id == firstId) List("first difference", "second difference") else Nil)
        }
      } yield {
        // The counting convention: `passed` counts rows with no message at all, `failed`
        // counts discrepancies rather than rows, so one row differing twice makes their sum
        // exceed the number of rows.
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
        // The counts of a failing run are the reason the report exists, so they are on disk
        // before anything can decide the run failed. Reading the file back here is what proves
        // the order: the gate below has not run yet.
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
        // An exception escaping a row would take the counts of every row after it with it, so
        // it becomes one discrepancy of that row and the run continues.
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
            // Everything beyond the quota stays in the report document rather than in the console,
            // so the console output of a systematically broken port stays readable.
            message should not include "difference 20"
            message should include("and 5 more")
          case Right(_) => fail("a report holding discrepancies did not fail the gate")
        }
        // The other half of that decision: the document on disk is the complete record. The
        // truncation is a property of the message alone, and a report that dropped the five it
        // did not quote would lose exactly the detail the report exists to keep.
        parse(document).map(_.asObject.flatMap(_("failures")).flatMap(_.asArray).map(_.size)) shouldBe
          Right(Some(failures.size))
        document should include("difference 20")
        document should include("difference 24")
      }
    }
  }

  /*
   * The three tests below are about the decoding of the fixture rather than about the port. They
   * exist because the two above cannot see what they are not given: a gate that reads
   * `failed == 0` over rows that decoded perfectly cannot tell a fixture that is measured in full
   * from one that has grown a key nothing reads. So the schemas are asserted to be the key sets
   * the row models actually read, and the refusals are exercised rather than assumed.
   */

  test("the declared row and scheduleInfo key sets are the ones the row models read") {
    IO {
      // The schema is the model's own field set, so the keys the decoder enforces cannot drift
      // from the fields this spec measures: a field added to the model without being added to the
      // schema, or the reverse, fails here.
      RowSchema.known shouldBe DocumentedRowModel.productElementNames.toSet
      RowSchema.known.size shouldBe 12
      ScheduleInfoSchema.known shouldBe DocumentedListedScheduleInfoModel.productElementNames.toSet
      ScheduleInfoSchema.known.size shouldBe 6
      // And the documented shapes are those key sets, which is what ties the two committed
      // documents below to the declarations above.
      DocumentedRow.asObject.map(_.keys.toSet) shouldBe Some(RowSchema.known)
      ScheduleInfoSchema.variants.map(_._1) shouldBe Vector(FixedPeriodEndVariant, BoundaryListVariant)
      ScheduleInfoSchema.variants.map(_._2.size) shouldBe Vector(5, 5)
      // Each variant is satisfied by exactly one of the two committed encodings, and the schema
      // names which - the decision the decoder is then handed instead of making a second time.
      variantOf(DocumentedFixedScheduleInfo) shouldBe Some(FixedPeriodEndVariant)
      variantOf(DocumentedListedScheduleInfo) shouldBe Some(BoundaryListVariant)
      succeed
    }
  }

  test("a captured row whose keys are not the documented twelve is refused by name") {
    IO {
      // The documented shape decodes, field for field: strictness refuses what the document does
      // not document and nothing else. This is a decode identity compared exactly - the mapping
      // of keys onto fields - and not a measurement, so the tolerance rule of this file, which
      // governs the comparison of the port's numbers against the baseline, is untouched by it.
      StrictRowDecoder.decodeJson(DocumentedRow) shouldBe Right(DocumentedRowModel)
      // A key the capture has started emitting. This is the case the finding is about: a derived
      // decoder would ignore it and measure the row as though the new expectation did not exist.
      refusalOf(
        StrictRowDecoder,
        withKey(DocumentedRow, "yearFractionRounded", Json.fromDoubleOrNull(1.0d))) should include(
        "unknown keys {yearFractionRounded}")
      // A schema-required key the document no longer carries, which `Option` alone cannot tell
      // apart from the `null` that means "this evaluation was not performed".
      refusalOf(StrictRowDecoder, withoutKey(DocumentedRow, "days")) should include(
        "a day-count parity row is missing {days}")
      // A renamed key is both at once, and the refusal names both halves.
      val renamed = refusalOf(StrictRowDecoder, withRenamedKey(DocumentedRow, "daysError", "dayCountError"))
      renamed should include("unknown keys {dayCountError}")
      renamed should include("a day-count parity row is missing {daysError}")
      succeed
    }
  }

  test("a scheduleInfo object that is neither documented variant is refused by name") {
    IO {
      // Both documented encodings decode, and the boundary list decodes to its dates in order.
      StrictRowDecoder.decodeJson(DocumentedRow).map(_.scheduleInfo) shouldBe
        Right(DocumentedRowModel.scheduleInfo)
      StrictRowDecoder
        .decodeJson(rowWithScheduleInfo(DocumentedListedScheduleInfo))
        .map(_.scheduleInfo) shouldBe Right(DocumentedListedScheduleInfoModel)
      // Both period-end encodings at once: section 6 states never both, and the two have
      // different readings of `periodEndDate`, so the object is refused with the keys it carries.
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
      // A key the capture has started emitting inside the nested object, which only that object's
      // own decoder ever sees.
      refusalOf(
        StrictRowDecoder,
        rowWithScheduleInfo(
          withKey(DocumentedListedScheduleInfo, "periodStarts", Json.arr()))) should include(
        "unknown keys {periodStarts}")
      // And a renamed common key, which is a loss and a gain of the nested object at once.
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
 * applied to one row.
 *
 * It lives in the companion rather than in the suite because the JSON derivation needs the row
 * types on a stable path, and because keeping the measurement out of the suite body makes it plain
 * that the suite contributes nothing to the measurement beyond ordering it. Everything here is
 * confined to this package.
 */
private[parity] object DayCountParitySpec {

  /*
   * The single policy for a double, in scope before the row decoders are derived.
   *
   * The captured documents write a non-finite double as one of the three tagged strings `"NaN"`,
   * `"Infinity"` and `"-Infinity"` and every finite one as a full-precision JSON number. An
   * imported implicit outranks the one circe publishes for `Double` in its own companion, so the
   * `Option[Double]` fields below read a tagged value with no ambiguity. No row of this fixture is
   * expected to carry one - the capture resolves the `DayCountTest` table markers - and reading
   * them is what lets such a row be reported as a discrepancy instead of a decode failure of the
   * whole document.
   */
  import Codecs.implicits.doubleCodec

  //-------------------------------------------------------------------------
  // Contract constants. The first two are agreements with something outside this file - the
  // resource name with the capture script that writes it, the fixture stem with the gate script
  // that reads `<parity.report.dir>/daycount.json` - so neither may drift.
  //-------------------------------------------------------------------------

  /** The fixture stem, which is also the `fixture` field of the report and its file name. */
  val FixtureName: String = "daycount"

  /** The classpath name of the captured baseline, relative to the test resource root. */
  val FixtureResource: String = "parity/daycount-baseline.json"

  /**
   * The end-of-month convention that an absent `eom` stands for.
   *
   * The interface being ported defaults `isEndOfMonthConvention` to `true`, and so does the port's
   * `ScheduleInfo`. An absent flag is therefore the default rather than `false`, which matters
   * because `30U/360` chooses between two day-of-month rules by it.
   */
  val EndOfMonthDefault: Boolean = true

  /**
   * The least number of rows the baseline is worth measuring.
   *
   * The committed document holds 18,356. Section 6 of `tools/parity-capture/README.md` records
   * that count and the population behind it.
   */
  val MinimumRows: Int = 18356

  /**
   * The least number of rows of each captured population, keyed by the row's id family.
   *
   * The family is the part of the identity before its first `'-'`, and the key set is closed: a
   * row whose family is not one of these seventeen is a population this spec does not know how to
   * account for, and is reported rather than measured silently. The counts are those of section 6
   * of the capture README, and they sum to [[MinimumRows]] exactly.
   */
  val MinimumRowsByFamily: Map[String, Int] =
    Map(
      // all 22 subjects x consecutive month-end and mid-month pairs, 2010-2030, no schedule info
      "grid" -> 11044,
      // all 22 subjects x P1M/P3M/P6M/P12M x regular, short initial, short final x every period
      "sched" -> 6446,
      // the two Java tables, one row per table row per consumer
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

  /** The two generated populations, each of which covers every subject. */
  val GeneratedFamilies: Vector[String] = Vector("grid", "sched")

  /** The family whose every row supplies its dates out of order, refusing both operations. */
  val OutOfOrderFamily: String = "order"

  /**
   * Every day-count name the baseline is required to measure.
   *
   * The 21 standard conventions and the business-day convention `Bus/252 BRBD`, whose calendar the
   * port resolves against the built-in set - constant data rather than ambient state (AAP section
   * 0.6.5).
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

  /** The least number of rows whose year fraction the Java implementation refused to produce. */
  val MinimumYearFractionRefusals: Int = 1050

  /** The least number of rows whose day count the Java implementation refused to produce. */
  val MinimumDaysRefusals: Int = 22

  /** The least number of rows carrying the captured relative year fractions, in both directions. */
  val MinimumRelativeRows: Int = 221

  /** The least number of rows evaluated against the boundary list of a real schedule. */
  val MinimumScheduleRows: Int = 6446

  /** The least number of rows evaluated against schedule information that carries nothing. */
  val MinimumSimpleRows: Int = 11561

  //-------------------------------------------------------------------------
  // The row model. Field for field the schema of section 6 of `tools/parity-capture/README.md`,
  // which records that every row of this fixture carries the identical twelve keys in the same
  // order, that `scheduleInfo` is always an object, and that a key whose evaluation was not
  // performed is present with the value `null`. `Option` is therefore exactly where that schema
  // writes `null`, and no field is dropped: `source`, the two relative year fractions and
  // `daysError` are captured baseline like any other expectation, and a model that omitted them
  // would measure less than was captured.
  //
  // Each shape is declared as a [[KeySchema]] beside the model it describes, and every object of
  // the document is checked against its schema before it is decoded - the row by `loadStrict`,
  // the nested `scheduleInfo` by its own decoder, which is the only place that object's keys are
  // ever visible. Without that, derived decoding would read the fields the models declare and
  // ignore every other key, so a key the capture started emitting - a new expectation, a new
  // operand, a renamed field - would be dropped in silence while the report still read
  // `failed == 0`. Declaring the keys also makes `Option` mean what the schema says it means: the
  // key must be present, and `null` is then the one way it says "not performed", which a merely
  // optional field cannot distinguish from a key that has gone missing.
  //-------------------------------------------------------------------------

  /**
   * The schedule information one evaluation was given.
   *
   * Every subfield is optional because `null` inside this object means '''absent''' - `None` on
   * this side and the raising default of the interface being ported on the other - with the one
   * documented exception of `eom`, where absent means the interface default of `true`; see
   * [[endOfMonth]].
   *
   * `periodEnd` and `periodEnds` are the two encodings of the period end date, and a row carries
   * exactly one of them as a key: `periodEnd` is the fixed date of the Java test stub, `periodEnds`
   * the ordered boundary list of a real schedule. Either may be `null`, which is the ordinary case
   * for the rows evaluated with no schedule information at all.
   *
   * @param start  the adjusted start date of the schedule
   * @param end  the adjusted end date of the schedule, which is its maturity
   * @param frequency  the name of the periodic frequency, such as `P3M` or `Term`
   * @param eom  the end-of-month convention flag, absent where the interface default applies
   * @param periodEnd  the one period end date the Java stub answers for every date
   * @param periodEnds  the ordered end dates of every period of a real schedule
   */
  final case class ScheduleInfoRow(
      start: Option[LocalDate],
      end: Option[LocalDate],
      frequency: Option[String],
      eom: Option[Boolean],
      periodEnd: Option[LocalDate],
      periodEnds: Option[Vector[LocalDate]]) {

    /**
     * Whether this carries no schedule fact at all.
     *
     * These are the rows the capture evaluated through the Java two-argument overload against
     * `DayCounts.SIMPLE_SCHEDULE_INFO`, and they are measured against the port's own
     * `DayCount.ScheduleInfo.simple` rather than against an adapter that would merely answer the
     * same way.
     */
    def isAbsent: Boolean =
      start.isEmpty && end.isEmpty && frequency.isEmpty && eom.isEmpty &&
        periodEnd.isEmpty && periodEnds.isEmpty

    /**
     * The end-of-month convention in force, resolving an absent flag to the interface default.
     *
     * The default is `true`, on both sides of the port. Resolving it to `false` would silently
     * re-point every `30U/360` row at the other of that convention's two day-of-month rules.
     */
    def endOfMonth: Boolean = eom.getOrElse(EndOfMonthDefault)

    /**
     * Whether this carries the schedule span and frequency that a boundary list needs beside it.
     *
     * A boundary list is read only inside the schedule's own span, so a list without that span is
     * an encoding this spec cannot evaluate; the frequency belongs with it because the two
     * conventions that read a period end date also read the frequency.
     */
    def spansSchedule: Boolean = start.isDefined && end.isDefined && frequency.isDefined
  }

  /** The key of the adjusted start date of the schedule, inside `scheduleInfo`. */
  val ScheduleStartKey: String = "start"

  /** The key of the adjusted end date of the schedule, inside `scheduleInfo`. */
  val ScheduleEndKey: String = "end"

  /** The key of the periodic frequency name, inside `scheduleInfo`. */
  val FrequencyKey: String = "frequency"

  /** The key of the end-of-month convention flag, inside `scheduleInfo`. */
  val EndOfMonthKey: String = "eom"

  /** The key of the one fixed period end date of the Java test stub. */
  val FixedPeriodEndKey: String = "periodEnd"

  /** The key of the ordered period boundary list of a real schedule. */
  val BoundaryListKey: String = "periodEnds"

  /** The name of the `scheduleInfo` variant that carries [[FixedPeriodEndKey]]. */
  val FixedPeriodEndVariant: String = "the fixed period end date encoding"

  /** The name of the `scheduleInfo` variant that carries [[BoundaryListKey]]. */
  val BoundaryListVariant: String = "the period boundary list encoding"

  /** The four keys every `scheduleInfo` object carries, whichever period-end encoding it uses. */
  val ScheduleInfoCommonKeys: Set[String] =
    Set(ScheduleStartKey, ScheduleEndKey, FrequencyKey, EndOfMonthKey)

  /**
   * The documented shape of the `scheduleInfo` object of a row, which has two variants.
   *
   * This is the schema '''of''' [[ScheduleInfoRow]], and its authority is the committed document
   * together with section 6 of `tools/parity-capture/README.md`: the object carries `start`,
   * `end`, `frequency` and `eom` plus '''exactly one of''' `periodEnd` and `periodEnds` - never
   * both, never neither - which over the committed 18,356 rows is 11,910 objects of the first
   * variant and 6,446 of the second.
   *
   * Two variants rather than six optional keys is what makes that statement enforceable. Each
   * variant is an exact key set, so a key must be '''present''' even where its value is `null`:
   * that is what keeps `null` meaning "this evaluation was not performed" instead of being
   * indistinguishable from a schema-required key the capture has stopped emitting. An object
   * carrying both encodings, or neither, satisfies no variant and is refused with the keys it
   * carries named, rather than decoded into a reading of `periodEndDate` that the capture never
   * used.
   */
  val ScheduleInfoSchema: KeySchema =
    KeySchema.variants(
      "the scheduleInfo object of a day-count parity row",
      FixedPeriodEndVariant -> (ScheduleInfoCommonKeys + FixedPeriodEndKey),
      BoundaryListVariant -> (ScheduleInfoCommonKeys + BoundaryListKey))

  /**
   * One evaluated row of the captured baseline.
   *
   * A value and its error are never both set. Both absent means the capture did not perform that
   * evaluation, which is the ordinary case for the two relative forms - they were captured only
   * where they carry information - and which for the year fraction and the day count is a fixture
   * that has stopped agreeing with this spec, because the capture performs both for every row.
   *
   * @param id  the identity of the row, unique across the document and what the report names
   * @param source  the Java test method, or the generated population, the row came from
   * @param dayCount  the canonical name of the convention, `Bus/252 BRBD` included
   * @param start  the first date of the pair, as supplied
   * @param end  the second date of the pair, as supplied, which may be before the first
   * @param scheduleInfo  the schedule information the evaluation was given
   * @param yearFraction  `yearFraction(start, end, scheduleInfo)`, where it produced a value
   * @param relativeYearFraction  `relativeYearFraction(start, end, scheduleInfo)`, where captured
   * @param relativeYearFractionReversed  the same with the dates swapped, where captured
   * @param days  `days(start, end)`, where it produced a value
   * @param error  the Java failure of the year-fraction evaluation, where it refused
   * @param daysError  the Java failure of the day-count evaluation, where it refused
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

  /** The key of the schedule information object of a row, the one object nested in a row. */
  val ScheduleInfoKey: String = "scheduleInfo"

  /**
   * The documented shape of one row of `daycount-baseline.json`.
   *
   * This is the schema '''of''' [[DayCountRow]] - the twelve keys of the committed document,
   * which are exactly the twelve fields that model declares, asserted against each other by
   * `the declared row and scheduleInfo key sets are the ones the row models read`. Its authority
   * is the committed document, which carries these twelve keys on every one of its 18,356 rows,
   * together with section 6 of `tools/parity-capture/README.md`, which records that shape as
   * uniform and states that a key whose evaluation was not performed is '''present''' with the
   * value `null`.
   *
   * One documented key set, so satisfying it is equality of key sets: a row that has gained a key
   * is refused with that key named, and a row that has lost one is refused with the loss named,
   * rather than decoded into a model that quietly has nothing to say about either.
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
   *
   * The variant is handed over by [[ParityHarness.strictVariant]], which decided it while
   * checking the keys, so this decoder reads '''only''' the period-end key the matched variant
   * names: for a boundary-list object `periodEnd` is provably not a key of the document, and for
   * a fixed-period-end object `periodEnds` is not, so neither is looked up. Deciding it a second
   * time here - by asking which key happens to be present - is the step that could disagree with
   * the check that validated the object, and it is the step this form removes.
   *
   * The four common keys are read as `Option`, which is what the schema's `null` means: absent on
   * this side, and the raising default of the interface being ported on the other. Their
   * '''presence''' is not this decoder's business, because the schema has already required it.
   *
   * @param variant  the name of the variant the object's keys satisfied
   * @return the decoder for an object of that variant
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
   * The row's own fields, read once the keys are known to be the documented ones.
   *
   * Deliberately not implicit: nothing may summon a decoder for a row of this document that is
   * not the strict one below, so the only reference to this value is the composition that makes
   * it strict.
   */
  private val dayCountRowFields: Decoder[DayCountRow] = deriveDecoder[DayCountRow]

  /**
   * The decoder the fixture is read through, which is the row's fields behind the key check.
   *
   * This is the implicit a loader summons, so every path that reads a row of this document -
   * `ParityHarness.loadStrict`, which composes this with the same check again while reading, and
   * the strictness tests of this suite, which decode hand-built objects through it - is strict
   * about keys by construction rather than by remembering to be.
   */
  implicit val StrictRowDecoder: Decoder[DayCountRow] =
    ParityHarness.strictObject(RowSchema)(dayCountRowFields)

  //-------------------------------------------------------------------------
  // The documented shapes as documents, and the three ways a document departs from one.
  //
  // A schema that is only exercised by the fixture it already agrees with proves nothing about
  // what it would refuse, so the strictness tests of this suite decode these documents: the
  // documented shapes, which must be accepted and must decode to the models below, and then the
  // same documents with one key added, one key removed and one key renamed, each of which must be
  // refused with the offending key named. The accepted documents are the two committed shapes,
  // key for key: the first row of `daycount-baseline.json` and the `scheduleInfo` object of its
  // first `sched` row, with the boundary list shortened to the two dates a decode needs.
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

  /** What [[DocumentedRow]] is required to decode to, field for field. */
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

  /** What [[DocumentedListedScheduleInfo]] is required to decode to, field for field. */
  val DocumentedListedScheduleInfoModel: ScheduleInfoRow =
    ScheduleInfoRow(
      start = Some(LocalDate.of(2015, 1, 15)),
      end = Some(LocalDate.of(2020, 1, 15)),
      frequency = Some("P1M"),
      eom = Some(false),
      periodEnd = None,
      periodEnds = Some(Vector(LocalDate.of(2015, 2, 15), LocalDate.of(2015, 3, 15))))

  /**
   * The documented row carrying the given schedule information, which is its one nested object.
   *
   * @param info  the `scheduleInfo` object to put on the row
   * @return the row document
   */
  def rowWithScheduleInfo(info: Json): Json = withKey(DocumentedRow, ScheduleInfoKey, info)

  /**
   * The `scheduleInfo` variant a document satisfies, as [[ScheduleInfoSchema]] decides it.
   *
   * This is the same decision [[ParityHarness.strictVariant]] hands to [[scheduleInfoDecoder]],
   * made through the same method, so a test can state which variant a shape is without
   * duplicating the rule that answers it.
   *
   * @param info  the `scheduleInfo` object to classify
   * @return the name of the variant it satisfies, or nothing where it satisfies none
   */
  def variantOf(info: Json): Option[String] =
    info.asObject.flatMap(fields => ScheduleInfoSchema.matching(fields.keys.toSet))

  /**
   * The same object with one key added, which is the shape a newly captured field arrives in.
   *
   * @param document  the object to change
   * @param key  the key to add, or to replace where the object already carries it
   * @param value  the value of that key
   * @return the changed object
   */
  def withKey(document: Json, key: String, value: Json): Json =
    document.mapObject(fields => fields.add(key, value))

  /**
   * The same object with one key removed, which is the shape a retired field leaves behind.
   *
   * @param document  the object to change
   * @param key  the key to remove
   * @return the changed object
   */
  def withoutKey(document: Json, key: String): Json =
    document.mapObject(fields => fields.remove(key))

  /**
   * The same object with one key renamed, keeping its value - a rename is a loss and a gain at
   * once, and a schema has to report both halves for the message to say what happened.
   *
   * @param document  the object to change
   * @param from  the key as the schema declares it
   * @param to  the key the document is to carry instead
   * @return the changed object
   */
  def withRenamedKey(document: Json, from: String, to: String): Json =
    document.mapObject(fields => fields.remove(from).add(to, fields(from).getOrElse(Json.Null)))

  /**
   * The message a decoder refuses a document with.
   *
   * A decoder that '''accepts''' the document answers with a description of what it accepted, so
   * that the assertion on the refusal's wording fails naming the value that got through rather
   * than failing on an empty string that says nothing.
   *
   * @param decoder  the decoder under test
   * @param document  the document to offer it
   * @return the refusal message, or what was accepted instead
   */
  def refusalOf[A](decoder: Decoder[A], document: Json): String =
    decoder.decodeJson(document) match {
      case Left(failure) => failure.message
      case Right(value) => s"the decoder accepted $value"
    }

  /**
   * The least a row model can be: the identity of a fixture row and the case it came from.
   *
   * The cases of this suite that measure the shared harness rather than the port need no
   * expectation fields at all; two keys are enough, and reading only those is what lets any real
   * committed baseline stand in for a fixture of their own - including the day-count document this
   * suite measures. It is declared here, beside the row models above, because a case class
   * declared inside the suite class would carry an outer reference its synthesized type test
   * cannot check at run time.
   *
   * @param id  the name a discrepancy of the row would be reported under
   * @param source  the Java test method, or the generated population, the row came from
   */
  final case class HarnessProbeRow(id: String, source: String) extends ParityRow

  /**
   * The decoder of [[HarnessProbeRow]], written out rather than derived because the fallback from
   * `id` to `source` is the identity rule of the captured documents rather than a shape circe can
   * infer: a row carries an `id` exactly where its `source` cannot identify it alone, so an absent
   * `id` means the `source` is the identity, and `schedule-baseline.json` is the one committed
   * document of that shape. Every other key of the row is ignored, and no `Double` is read, so the
   * tagged-double policy of the captured documents is not needed here.
   */
  implicit val harnessProbeRowDecoder: Decoder[HarnessProbeRow] = Decoder.instance { cursor =>
    for {
      source <- cursor.get[String]("source")
      id <- cursor.getOrElse[String]("id")(source)
    } yield HarnessProbeRow(id, source)
  }

  //-------------------------------------------------------------------------
  // The two schedule-information adapters.
  //
  // Both are immutable, hold no effect and read nothing outside their own fields, so a row's
  // evaluation is a pure function of the row. Neither carries a `var` or a mutable collection.
  //-------------------------------------------------------------------------

  /**
   * The schedule information of the Java test stub: one fixed period end date, and no rule.
   *
   * `periodEndDate` '''ignores the date it is given''' and answers the single value the row
   * carries. That is the stub's behaviour, verbatim: its field is one `periodEnd` and its
   * `getPeriodEndDate(LocalDate date)` returns it whatever the argument
   * (`modules/basics/src/test/java/com/opengamma/strata/basics/date/DayCountTest.java:1373-1416`).
   * Every expectation captured through that stub was produced with that behaviour, so reproducing
   * it is what makes those expectations measurable; "improving" it to consult the date would
   * change the inputs and therefore the answers.
   *
   * @param startDate  the schedule start date the stub carries, if any
   * @param endDate  the schedule end date the stub carries, if any
   * @param frequency  the frequency the stub carries, if any
   * @param isEndOfMonthConvention  the end-of-month flag, always explicit here
   * @param fixedPeriodEnd  the one period end date answered for every date
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
   * The schedule information of a real schedule, encoded as its ordered period boundaries.
   *
   * `periodEndDate(d)` is the first boundary '''strictly''' after `d`, and only while `d` lies in
   * the schedule's own span, `scheduleStart <= d < scheduleEnd`; anywhere else it is `None`. That
   * is the encoding section 6 of the capture README defines, and the capture demonstrated it
   * lossless by re-evaluating every such row through an implementation that sees only this list.
   * `None` outside every period is the port's documented reading of a date the schedule does not
   * contain, where the interface being ported raised
   * (`modules/basics/src/main/java/com/opengamma/strata/basics/schedule/Schedule.java:332-338`);
   * the convention that reads it refuses on its own behalf if it cannot proceed.
   *
   * The lookup is a linear scan of an immutable vector, which is what the rule says and is
   * bounded by the sixty-one boundaries of the longest schedule in the fixture. No interpolation,
   * no clamping and no nearest-boundary fallback: each of those would answer where the rule says
   * nothing is answered.
   *
   * @param scheduleStart  the adjusted start date of the schedule
   * @param scheduleEnd  the adjusted end date of the schedule
   * @param frequency  the periodic frequency of the schedule
   * @param isEndOfMonthConvention  the end-of-month flag of the schedule
   * @param boundaries  the ordered, non-empty adjusted end dates of every period
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

  //-------------------------------------------------------------------------
  // Reading a row: its family, its schedule information and its subject.
  //-------------------------------------------------------------------------

  /**
   * The population a row belongs to, which is the part of its identity before the first `'-'`.
   *
   * The identities are kebab-case and begin with a family name - `grid`, `sched`, `yf`, `order`
   * and so on - so this is a property of the document rather than a guess about it;
   * [[MinimumRowsByFamily]] is the closed set of families the baseline is required to carry.
   *
   * @param row  the row to classify
   * @return the family name of the row
   */
  def familyOf(row: DayCountRow): String = row.id.takeWhile(character => character != '-')

  /**
   * Builds the schedule information a row was evaluated with, or names why it cannot be built.
   *
   * The three shapes of the schema map onto the three outcomes here, and nothing is inferred
   * beyond them. A row carrying no schedule fact is measured against the port's own
   * `ScheduleInfo.simple`; a row carrying a fixed period end date - or none, alongside some other
   * fact - becomes [[StubScheduleInfo]]; a row carrying a boundary list becomes
   * [[ListedScheduleInfo]], which needs the schedule span that bounds the list.
   *
   * A row that carries both period-end encodings, or a boundary list without its span, has no
   * single reading that both encodings agree on. Guessing one would measure the port against an
   * input the capture never used, and raising would end the row without saying why, so it is
   * reported as a fixture disagreement and the row contributes that message instead of a
   * measurement.
   *
   * The frequency is a name the port parses. A frequency the port cannot parse is a defect in the
   * port or in the fixture rather than an expectation of any kind, so it is lifted into a failed
   * effect through [[ParityHarness.raise]], which the driver records against the row.
   *
   * @param row  the row whose schedule information is wanted
   * @return the schedule information, or the disagreement that prevents it being built
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

  /**
   * Parses the frequency a row names, where it names one.
   *
   * @param name  the captured frequency name, such as `P3M` or `Term`
   * @return the frequency, or nothing where the row carries none; the effect fails when a name
   *         cannot be parsed, which is a defect rather than an expectation
   */
  private def frequencyOf(name: Option[String]): IO[Option[Frequency]] =
    name.traverse(text => ParityHarness.raise(Frequency.parse(text)))

  /**
   * Resolves the convention a row names, reusing the one already resolved for that name.
   *
   * The subject is resolved '''by name''' rather than by referring to a constant of the port, so
   * the lookup the fixture depends on is part of what is measured: the canonical names are the
   * identities the captured document, the JSON codecs and every caller share. `Bus/252 BRBD`
   * resolves through the same call, against the calendars built into the library - constant data
   * rather than ambient reference data (AAP section 0.6.5) - which is why no reference data is
   * needed here at all.
   *
   * A name the port cannot resolve is a defect in the port or in the fixture rather than a parity
   * result, so it is lifted into a failed effect through [[ParityHarness.raiseNec]] and recorded
   * against the row by the driver.
   *
   * @param conventions  the conventions resolved so far in this run, keyed by captured name
   * @param name  the canonical name of the convention
   * @return the convention that name resolves to
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

  //-------------------------------------------------------------------------
  // Measuring one row.
  //-------------------------------------------------------------------------

  /**
   * Measures one row of the fixture, answering with everything that differed.
   *
   * Every operation of the row is measured, and the messages are concatenated, so a row that
   * differs in its year fraction '''and''' its day count names both instead of only the first.
   *
   * @param conventions  the conventions resolved so far in this run
   * @param row  the row to measure
   * @return every discrepancy found in the row, empty where it matched in every respect
   */
  def checkRow(conventions: Ref[IO, Map[String, DayCount]], row: DayCountRow): IO[List[String]] =
    scheduleInfoFor(row).flatMap {
      case Left(disagreements) => IO.pure(disagreements)
      case Right(info) =>
        dayCountFor(conventions, row.dayCount).flatMap(dayCount => measureRow(dayCount, info, row))
    }

  /**
   * Applies the four measurements of a row to the convention it names.
   *
   * The name of the resolved convention is itself an expectation: a lenient lookup that resolved
   * to the wrong member, or a `Bus/252` name that lost its calendar, would otherwise be measured
   * as the arithmetic of whatever it resolved to.
   *
   * @param dayCount  the convention the row names
   * @param info  the schedule information the row was evaluated with
   * @param row  the row to measure
   * @return every discrepancy found in the row
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
   * Measures the year fraction, or the refusal the capture recorded in its place.
   *
   * A refusal is asserted by '''type''' alone through [[ParityHarness.attemptArgCheck]]. The
   * captured text is the message of the implementation being replaced - an
   * `UnsupportedOperationException` from an accessor of `SIMPLE_SCHEDULE_INFO`, or the order check
   * of `ArgChecker` - and the port answers both with its own `ArgCheck` message, so message parity
   * is deliberately not claimed (AAP section 0.8.3 records the divergence). The captured text is
   * quoted in the diagnostic, where it helps, and compared nowhere.
   *
   * @param dayCount  the convention the row names
   * @param info  the schedule information the row was evaluated with
   * @param row  the row to measure
   * @return the discrepancy, or nothing when the port answered as Java did
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
   * Measures the day count, or the refusal the capture recorded in its place.
   *
   * The comparison is exact and the value stays an `Int`: a day count is a count, a tolerance
   * applied to one would admit an answer that is simply wrong, and widening it to a `Double` to
   * borrow the numeric comparator would be a conversion this measurement has no use for.
   *
   * @param dayCount  the convention the row names
   * @param row  the row to measure
   * @return the discrepancy, or nothing when the port answered as Java did
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
   * Measures the relative year fraction in both directions.
   *
   * The relative form is '''total''': it has no order check, and for a reversed pair it swaps the
   * dates and negates the result, which is why it answers where the year fraction refuses. It is
   * therefore never asserted to refuse - not even on the out-of-order rows, where the other two
   * operations are.
   *
   * Both directions are evaluated wherever an expectation exists for them, and
   * [[expectedForward]] and [[expectedReversed]] are where that expectation comes from.
   *
   * @param dayCount  the convention the row names
   * @param info  the schedule information the row was evaluated with
   * @param row  the row to measure
   * @return every discrepancy found in the two relative directions
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

  /** The name the forward relative year fraction is reported under. */
  private val ForwardLabel: String = "relativeYearFraction"

  /** The name the reversed relative year fraction is reported under. */
  private val ReversedLabel: String = "relativeYearFraction with the dates reversed"

  /**
   * The expectation for the relative year fraction over the row's dates as supplied.
   *
   * The captured value is the expectation wherever the fixture carries one - the 201
   * `data_yearFraction` rows and the 20 out-of-order rows. Everywhere else the dates are in order,
   * so the relative form is the year fraction itself, which is the reading the contract of the
   * ported type states and the reading `DayCountTest.test_relativeYearFraction` asserts. Using the
   * row's own year fraction there measures that identity on every row of the fixture instead of on
   * the 201 rows that happen to carry the value twice.
   *
   * Where the row carries neither - the rows whose year fraction Java refused - there is no
   * expectation and nothing is measured: the refusal is measured by [[checkYearFraction]], and
   * inventing a relative expectation for a row the capture recorded none for would be deriving by
   * hand the very value a captured baseline exists to supply.
   *
   * @param row  the row to read
   * @return the expectation, or nothing where the row states none
   */
  def expectedForward(row: DayCountRow): Option[Double] =
    row.relativeYearFraction.orElse(row.yearFraction)

  /**
   * The expectation for the relative year fraction over the row's dates reversed.
   *
   * The captured value is again the expectation wherever the fixture carries one, and the Java
   * consumer of those rows - `DayCountTest.test_relativeYearFraction_reverse` - asserts exactly
   * `-expected`, which is the contract of the ported type: the result "will be negative if the
   * first date is after the second date".
   *
   * Where the row carries no captured reversed value, that identity supplies the expectation from
   * the row's own year fraction, '''but only for a span of non-zero length'''. For a zero-length
   * span the second date is not before the first, so the relative form answers `+yearFraction`
   * rather than its negation - and `1/1`, which answers `1` for any pair including a zero-length
   * one, makes that a difference of `2` rather than a question of signed zero. The guard is
   * therefore a statement of where the identity holds, not a way around a case that failed.
   *
   * @param row  the row to read
   * @return the expectation, or nothing where the row states none and none follows
   */
  def expectedReversed(row: DayCountRow): Option[Double] =
    row.relativeYearFractionReversed.orElse(
      if (row.start.isBefore(row.end)) row.yearFraction.map(expected => -expected) else None)

  //-------------------------------------------------------------------------
  // Internals: observing a call that must answer, and naming a fixture disagreement.
  //-------------------------------------------------------------------------

  /**
   * Compares what a call produced, reporting a refusal as the discrepancy it is.
   *
   * The call is made inside an effect and its outcome turned back into a value, for the same
   * reason [[ParityHarness.attemptArgCheck]] does it for the opposite expectation: a port that
   * refuses where Java answered is a discrepancy of one operation of one row, and it must not take
   * the rest of the row - or the rest of the fixture - with it. The driver would record an escaping
   * error as a single message for the whole row; this names the operation instead, and lets the
   * row's other operations be measured.
   *
   * @param label  the name of the operation, which is what makes a report readable
   * @param thunk  the call that must answer
   * @param compare  the comparison to apply to what it answered
   * @tparam A  the type of the answer
   * @return the discrepancy, or nothing when the call answered and the comparison passed
   */
  private def measured[A](label: String, thunk: => A)(compare: A => List[String]): IO[List[String]] =
    IO.delay[A](thunk).attempt.map {
      case Right(value) => compare(value)
      case Left(error) =>
        List(s"$label: expected a value, but the call failed with ${describe(error)}")
    }

  /**
   * Names a way in which the fixture has stopped agreeing with this spec.
   *
   * This is not a parity result and is deliberately worded so that it cannot be read as one: the
   * port may be perfectly correct and the row still unmeasurable. It is reported through the same
   * channel as a discrepancy so that it reaches the published report and fails the gate, because a
   * row that measures nothing is the one outcome a `failed == 0` gate cannot otherwise detect.
   *
   * @param explanation  what about the row cannot be measured, and why
   * @return the message to report against the row
   */
  private def fixtureDefect(explanation: String): String = s"fixture disagreement: $explanation"

  /** Renders a thrown error as its type and message, for a report that has to stand alone. */
  private def describe(error: Throwable): String =
    s"${error.getClass.getName}: ${Option(error.getMessage).getOrElse("no message")}"
}
