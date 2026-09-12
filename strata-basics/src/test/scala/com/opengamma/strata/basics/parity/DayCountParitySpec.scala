/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.parity

import java.time.LocalDate

import cats.effect.IO
import cats.effect.Ref
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all._

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.date.DayCount
import com.opengamma.strata.basics.schedule.Frequency
import com.opengamma.strata.collect.json.Codecs

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
      report <- ParityHarness.runFixture[DayCountRow](FixtureName, FixtureResource)(row =>
        checkRow(conventions, row))
      _ <- ParityHarness.failIfAny(report)
    } yield succeed
  }

  test("the fixture carries the population the day-count baseline is required to measure") {
    ParityHarness.load[DayCountRow](FixtureResource).map { rows =>
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

  implicit val scheduleInfoRowDecoder: Decoder[ScheduleInfoRow] = deriveDecoder[ScheduleInfoRow]

  implicit val dayCountRowDecoder: Decoder[DayCountRow] = deriveDecoder[DayCountRow]

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
