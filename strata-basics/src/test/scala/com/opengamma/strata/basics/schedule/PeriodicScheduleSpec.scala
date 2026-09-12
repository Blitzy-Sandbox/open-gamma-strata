/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.schedule

import java.time.DayOfWeek.SATURDAY
import java.time.DayOfWeek.SUNDAY
import java.time.LocalDate
import java.time.Month.APRIL
import java.time.Month.AUGUST
import java.time.Month.FEBRUARY
import java.time.Month.JULY
import java.time.Month.JUNE
import java.time.Month.MAY
import java.time.Month.NOVEMBER
import java.time.Month.OCTOBER
import java.time.Month.SEPTEMBER

import cats.Eq
import cats.Hash
import cats.Show
import cats.data.NonEmptyChain

import io.circe.DecodingFailure
import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor12
import org.scalatest.prop.TableFor14

import com.opengamma.strata.basics.ImmutableReferenceData
import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.date.AdjustableDate
import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.basics.date.BusinessDayConventions.FOLLOWING
import com.opengamma.strata.basics.date.BusinessDayConventions.MODIFIED_FOLLOWING
import com.opengamma.strata.basics.date.BusinessDayConventions.MODIFIED_PRECEDING
import com.opengamma.strata.basics.date.BusinessDayConventions.PRECEDING
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.basics.date.HolidayCalendarIds.JPTO
import com.opengamma.strata.basics.date.HolidayCalendarIds.SAT_SUN
import com.opengamma.strata.basics.date.HolidayCalendars
import com.opengamma.strata.basics.date.ImmutableHolidayCalendar
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers
import com.opengamma.strata.collect.testkit.TestHelper.date
import com.opengamma.strata.collect.testkit.TestHelper.list

import Frequency.P12M
import Frequency.P1D
import Frequency.P1M
import Frequency.P1W
import Frequency.P2M
import Frequency.P3M
import Frequency.P6M
import Frequency.TERM
import RollConventions.DAY_11
import RollConventions.DAY_17
import RollConventions.DAY_22
import RollConventions.DAY_24
import RollConventions.DAY_28
import RollConventions.DAY_29
import RollConventions.DAY_30
import RollConventions.DAY_4
import RollConventions.EOM
import RollConventions.IMM
import RollConventions.SFE
import StubConvention.LONG_FINAL
import StubConvention.LONG_INITIAL
import StubConvention.SHORT_FINAL
import StubConvention.SHORT_INITIAL
import StubConvention.SMART_FINAL
import StubConvention.SMART_INITIAL

/**
 * Test [[PeriodicSchedule]], ported from the Java `PeriodicScheduleTest`.
 *
 * This is a one-to-one port of the richest behavioural table in the module. The Java class
 * declares thirty-nine annotated methods - thirty-one plain and eight parameterised over just two
 * providers, `data_generation` seven times and `data_replace` once - and this file declares
 * '''thirty-nine''' tests, each keeping the Java method name verbatim and appearing in the Java
 * order. Every parameterised method is '''one''' test that drives its whole table inside itself
 * rather than one test per row, because the acceptance gate joins
 * `manifest/java-test-mapping.csv` to `target/test-reports/TEST-*.xml` on the suite class and the
 * test name, so the roster of names has to be closed and exact (AAP §0.10.1).
 *
 * Both providers are transcribed '''verbatim''': `data_generation` keeps all eighty-seven rows and
 * its twelve columns in the Java order, `data_replace` all eleven rows and its fourteen columns,
 * with the Java comments that group them. Each column the Java table left as a missing
 * reference is an `Option` here, and `list(...)` is the test kit's own list builder, so a row
 * reads as it read in Java.
 *
 * ===How the shape of the port changes the assertions===
 *
 *   - '''The bean builder has no target.''' Every Java body began with
 *     `PeriodicSchedule.builder()…build()`, which threw on a broken invariant. Construction
 *     here is through the four factories of the companion, all of which answer
 *     `EitherNec[Failure, PeriodicSchedule]`, so each Java builder block becomes one call to
 *     the local [[definition]] helper - the eleven properties with the seven optional ones
 *     defaulting to `None` - and the result is unwrapped by [[valid]].
 *   - '''Schedule creation reports failure as a value.''' `createSchedule`,
 *     `createUnadjustedDates` and `createAdjustedDates` return `Either[Failure, …]`, and
 *     `replaceStartDate` returns `EitherNec`. Every Java `ScheduleException` is a
 *     `Left(Failure.Invalid)` carrying the rejected definition under the `definition` attribute -
 *     which is what the exception carried as a field - so each of the failure tests goes through
 *     [[generationFailure]], which asserts the reason '''and''' that attribute rather than the
 *     reason alone (Rule 5). No test in this file asserts a raised exception, and no test writes
 *     `null`.
 *   - '''Reference data is threaded explicitly.''' `test_monthly_schedule` asserts both forms of
 *     schedule creation - the direct `createSchedule(refData)` and the `toReader` reader that
 *     awaits its reference data - and that the two agree on every row of the table (AAP §0.6.5).
 *   - '''Java's null-argument tests have no counterpart.''' `test_of_LocalDateEom_null` and
 *     `test_of_LocalDateRoll_null` keep their names and assert the validation the factories
 *     actually perform, for the reason given on each.
 *   - '''Three Java tests extended a type that is now sealed.''' `HolidayCalendar`,
 *     `BusinessDayConvention` and `RollConvention` are closed families (Rule 4), so the anonymous
 *     subclasses of `test_combinePeriodsWhenNecessary_1w_createSchedule`,
 *     `test_brokenWhenAdjusted_twoPeriods_createSchedule` and
 *     `test_emptyWhenAdjusted_badRoll_createUnadjustedDates` cannot be written here at all. Each of
 *     those three tests carries a comment naming the substitution it uses instead and why.
 *   - '''`getFrequency` split in two.''' `Schedule` implements the schedule information interface,
 *     whose `frequency` is an `Option`, so the plainly typed property the Java getter returned is
 *     [[Schedule.periodicFrequency]] and that is what the ported assertions read.
 *   - '''The reflective helpers have no target.''' `coverImmutableBean` and `assertSerialization`
 *     do not exist in this port's test kit, so `coverage`, `coverage_builder` and
 *     `test_serialization` assert what those helpers stood for: the properties of two distinct
 *     definitions, the typeclass instances, construction through the factory and the `with*`
 *     copies, and a circe round trip. Joda-Beans wire compatibility is out of scope
 *     (AAP §0.2.2).
 *
 * ===The four tests that have no Java counterpart===
 *
 * Beyond the thirty-nine ported tests this file declares '''four''' of its own, at the end, each
 * stating something the ported implementation did not do and therefore had no method to test:
 * `test_generation_boundedPeriodCount` (generation stops exactly at the documented period ceiling,
 * and a span provably beyond it is refused before anything is materialised, while every span the
 * generation accepts is refused by nothing),
 * `test_generation_datesOutsideSupportedRange` (roll arithmetic at the edges of `LocalDate` is
 * reported rather than raised), `test_interiorAdjustmentResolvedOnce` (the business day adjustment
 * is resolved once per generation, and only where there is an interior date to adjust) and
 * `test_invalidPeriod_branchOrder` (the failure a schedule of invalid periods reports is chosen in
 * the Java order, and duplicate-date messages render their date lists as Java's message formatter
 * did). They are additions, not replacements: no ported test is weakened by them, and the
 * traceability join the acceptance gate performs runs from a manifest row to a test case, so a
 * test case that no row names costs nothing.
 *
 * Numerical parity with the Java implementation is '''not''' this file's job: the sibling
 * `parity/ScheduleParitySpec` asserts it against `schedule-baseline.json`, whose inputs are drawn
 * from these same two providers. Neither spec is weakened on the assumption that the other covers
 * it - this one asserts exact behaviour, that one asserts parity.
 */
class PeriodicScheduleSpec
    extends AnyFunSuite
    with Matchers
    with TableDrivenPropertyChecks
    with ResultMatchers {

  //-------------------------------------------------------------------------
  // The fixtures of the Java test class, unchanged.
  private val REF_DATA: ReferenceData = ReferenceData.standard
  private val ROLL_NONE: RollConvention = RollConventions.NONE
  private val STUB_NONE: StubConvention = StubConvention.NONE
  private val STUB_BOTH: StubConvention = StubConvention.BOTH
  private val BDA: BusinessDayAdjustment = BusinessDayAdjustment.of(MODIFIED_FOLLOWING, SAT_SUN)
  private val BDA_JPY_MF: BusinessDayAdjustment = BusinessDayAdjustment.of(MODIFIED_FOLLOWING, JPTO)
  private val BDA_JPY_P: BusinessDayAdjustment = BusinessDayAdjustment.of(PRECEDING, JPTO)
  private val BDA_NONE: BusinessDayAdjustment = BusinessDayAdjustment.NONE
  private val NOV_29_2013: LocalDate = date(2013, NOVEMBER, 29) // Fri
  private val NOV_30_2013: LocalDate = date(2013, NOVEMBER, 30) // Sat
  private val FEB_28: LocalDate = date(2014, FEBRUARY, 28) // Fri
  private val APR_01: LocalDate = date(2014, APRIL, 1) // Tue
  private val MAY_17: LocalDate = date(2014, MAY, 17) // Sat
  private val MAY_19: LocalDate = date(2014, MAY, 19) // Mon
  private val MAY_30: LocalDate = date(2014, MAY, 30) // Fri
  private val MAY_31: LocalDate = date(2014, MAY, 31) // Sat
  private val JUN_03: LocalDate = date(2014, JUNE, 3) // Tue
  private val JUN_04: LocalDate = date(2014, JUNE, 4) // Wed
  private val JUN_10: LocalDate = date(2014, JUNE, 10) // Tue
  private val JUN_11: LocalDate = date(2014, JUNE, 11) // Wed
  private val JUN_17: LocalDate = date(2014, JUNE, 17) // Tue
  private val JUL_04: LocalDate = date(2014, JULY, 4) // Fri
  private val JUL_11: LocalDate = date(2014, JULY, 11) // Fri
  private val JUL_17: LocalDate = date(2014, JULY, 17) // Thu
  private val JUL_30: LocalDate = date(2014, JULY, 30) // Wed
  private val AUG_04: LocalDate = date(2014, AUGUST, 4) // Mon
  private val AUG_11: LocalDate = date(2014, AUGUST, 11) // Mon
  private val AUG_17: LocalDate = date(2014, AUGUST, 17) // Sun
  private val AUG_18: LocalDate = date(2014, AUGUST, 18) // Mon
  private val AUG_29: LocalDate = date(2014, AUGUST, 29) // Fri
  private val AUG_30: LocalDate = date(2014, AUGUST, 30) // Sat
  private val AUG_31: LocalDate = date(2014, AUGUST, 31) // Sun
  private val SEP_04: LocalDate = date(2014, SEPTEMBER, 4) // Thu
  private val SEP_05: LocalDate = date(2014, SEPTEMBER, 5) // Fri
  private val SEP_10: LocalDate = date(2014, SEPTEMBER, 10) // Wed
  private val SEP_11: LocalDate = date(2014, SEPTEMBER, 11) // Thu
  private val SEP_17: LocalDate = date(2014, SEPTEMBER, 17) // Wed
  private val SEP_18: LocalDate = date(2014, SEPTEMBER, 18) // Thu
  private val SEP_30: LocalDate = date(2014, SEPTEMBER, 30) // Tue
  private val OCT_17: LocalDate = date(2014, OCTOBER, 17) // Fri
  private val OCT_30: LocalDate = date(2014, OCTOBER, 30) // Thu
  private val NOV_28: LocalDate = date(2014, NOVEMBER, 28) // Fri
  private val NOV_30: LocalDate = date(2014, NOVEMBER, 30) // Sun

  //-------------------------------------------------------------------------
  /**
   * Builds a definition from the eleven properties, which is what the Java bean builder did.
   *
   * The seven optional properties default to absent, so a body that declared none of them names
   * four arguments, exactly as the Java builder blocks that set only four properties did. The
   * result is '''not''' unwrapped: the order invariants are part of what several tests assert, and
   * a caller that wants the value passes the result to [[valid]].
   *
   * @param startDate  the unadjusted start date of the schedule
   * @param endDate  the unadjusted end date of the schedule
   * @param frequency  the regular periodic frequency
   * @param businessDayAdjustment  the adjustment applied to each date of the calculated schedule
   * @param startDateBusinessDayAdjustment  the adjustment of the start date, if any
   * @param endDateBusinessDayAdjustment  the adjustment of the end date, if any
   * @param stubConvention  the convention defining how to handle stubs, if any
   * @param rollConvention  the convention defining how to roll dates, if any
   * @param firstRegularStartDate  the start date of the first regular period, if any
   * @param lastRegularEndDate  the end date of the last regular period, if any
   * @param overrideStartDate  the overriding start date of the first period, if any
   * @return the definition, or the failures describing why the properties describe none
   */
  private def definition(
      startDate: LocalDate,
      endDate: LocalDate,
      frequency: Frequency,
      businessDayAdjustment: BusinessDayAdjustment,
      startDateBusinessDayAdjustment: Option[BusinessDayAdjustment] = None,
      endDateBusinessDayAdjustment: Option[BusinessDayAdjustment] = None,
      stubConvention: Option[StubConvention] = None,
      rollConvention: Option[RollConvention] = None,
      firstRegularStartDate: Option[LocalDate] = None,
      lastRegularEndDate: Option[LocalDate] = None,
      overrideStartDate: Option[AdjustableDate] = None): ResultNec[PeriodicSchedule] =
    PeriodicSchedule.of(
      startDate,
      endDate,
      frequency,
      businessDayAdjustment,
      startDateBusinessDayAdjustment,
      endDateBusinessDayAdjustment,
      stubConvention,
      rollConvention,
      firstRegularStartDate,
      lastRegularEndDate,
      overrideStartDate)

  /**
   * The four-date helper of the Java test class, which `test_builder_invalidDateOrder` drives.
   *
   * It answers the result rather than the value, because that test asserts the rejection of six of
   * its seven cases and the acceptance of the seventh.
   *
   * @param start  the unadjusted start date of the schedule
   * @param end  the unadjusted end date of the schedule
   * @param first  the start date of the first regular period, if any
   * @param last  the end date of the last regular period, if any
   * @return the definition, or the failures describing why the dates describe none
   */
  private def createDates(
      start: LocalDate,
      end: LocalDate,
      first: Option[LocalDate],
      last: Option[LocalDate]): ResultNec[PeriodicSchedule] =
    definition(
      start,
      end,
      P1M,
      BDA,
      firstRegularStartDate = first,
      lastRegularEndDate = last)

  //-------------------------------------------------------------------------
  /**
   * Unwraps a definition built by the validating factory, failing the test if it was rejected.
   *
   * @param result  the result of the factory
   * @return the definition the factory built
   */
  private def valid(result: ResultNec[PeriodicSchedule]): PeriodicSchedule =
    result.fold(failures => fail(rejectionMessage("periodic schedule", failures)), value => value)

  /**
   * Unwraps a period built by the validating factory, failing the test if it was rejected.
   *
   * @param result  the result of the factory
   * @return the period the factory built
   */
  private def sp(result: ResultNec[SchedulePeriod]): SchedulePeriod =
    result.fold(failures => fail(rejectionMessage("schedule period", failures)), value => value)

  /**
   * Unwraps a frequency built by the validating factory, failing the test if it was rejected.
   *
   * The Java table wrote `Frequency.ofDays(2)` and `Frequency.ofYears(2)` inline; both factories
   * answer a result here, because a non-positive or over-long period names no frequency, so the
   * table wraps them in this helper.
   *
   * @param result  the result of the factory
   * @return the frequency the factory built
   */
  private def freq(result: ResultNec[Frequency]): Frequency =
    result.fold(failures => fail(rejectionMessage("frequency", failures)), value => value)

  /**
   * Unwraps a schedule created from a definition, failing the test if creation reported a failure.
   *
   * @param result  the result of schedule creation
   * @return the schedule that was created
   */
  private def sched(result: FailureOr[Schedule]): Schedule =
    result.fold(
      failure => fail(s"Expected a schedule but creation failed: ${failure.message}"),
      schedule => schedule)

  /**
   * Unwraps a list of dates created from a definition, failing the test if it reported a failure.
   *
   * @param result  the result of date generation
   * @return the dates that were generated
   */
  private def dates(result: FailureOr[List[LocalDate]]): List[LocalDate] =
    result.fold(
      failure => fail(s"Expected dates but generation failed: ${failure.message}"),
      generated => generated)

  /**
   * The message reporting that a value this spec builds was rejected by its factory.
   *
   * @param subject  what was being built
   * @param failures  the reasons the factory gave, of which there is at least one
   * @return the message
   */
  private def rejectionMessage(subject: String, failures: NonEmptyChain[Failure]): String =
    failures.toNonEmptyList.toList
      .map(failure => failure.message)
      .mkString(s"Expected a valid $subject but the factory rejected it: ", "; ", "")

  /**
   * The failures of a rejected definition, failing the test if it was accepted.
   *
   * @param result  the result of the factory
   * @return the failures the factory reported, of which there is at least one
   */
  private def failuresOf(result: ResultNec[PeriodicSchedule]): List[Failure] =
    result.fold(
      failures => failures.toNonEmptyList.toList,
      value => fail(s"Expected a rejection but a definition was produced: $value"))

  /**
   * Asserts that a schedule operation failed as the ported exception did, and returns the failure.
   *
   * The `ScheduleException` of the library being ported has no counterpart type: it is a
   * `Failure.Invalid` whose message is the ported message and whose `definition` attribute holds
   * the definition the exception carried as a field. Both halves are asserted here, so every
   * failure test of this file states that the rejected definition is reported and not merely that
   * something was rejected (Rule 5).
   *
   * @param result  the result of the operation
   * @param definition  the definition the operation was performed on
   * @tparam A  the type the operation would have produced
   * @return the failure the operation reported
   */
  private def generationFailure[A](
      result: FailureOr[A],
      definition: PeriodicSchedule): Failure = {
    val failure = result.fold(
      reported => reported,
      value => fail(s"Expected a failure but a value was produced: $value"))
    failure shouldBe a[Failure.Invalid]
    failure.reason shouldBe FailureReason.INVALID
    failure.attributes.get("definition") shouldBe Some(definition.toString)
    failure
  }

  /**
   * The constant-time preflight of date generation, applied to one walk's span.
   *
   * `PeriodicSchedule.provablyExceedsPeriodCount` is `private[schedule]`, so this spec - which
   * sits in that package - can put spans to the predicate itself. That is what makes the preflight
   * assertable at all: a refusal from it and a refusal from the bound inside the walk carry the
   * same message, so nothing observable from outside the package tells them apart, and only the
   * predicate can state that the refusal is conservative.
   *
   * @param from  the earlier end of the span
   * @param to  the later end of the span
   * @param frequency  the periodic frequency each step of the walk would apply
   * @return true if no walk over this span with this frequency can stay within the maximum
   */
  private def provablyOversized(from: LocalDate, to: LocalDate, frequency: Frequency): Boolean =
    PeriodicSchedule.provablyExceedsPeriodCount(from, to, frequency)

  //-------------------------------------------------------------------------
  /**
   * The schedule definitions and the dates they generate, transcribed verbatim from the Java
   * provider of the same name, with its comments.
   *
   * All eighty-seven rows are present and the twelve columns keep the Java order: the start and
   * end dates, the frequency, the stub convention, the roll convention, the business day
   * adjustment, the first regular start date, the last regular end date, the start date's own
   * business day adjustment, the unadjusted dates, the adjusted dates, and the roll convention the
   * resulting schedule is expected to carry. The five columns the Java table left as a missing
   * reference - the two conventions, the two regular dates and the start date's adjustment - are
   * `Option`s.
   *
   * Seven of this file's tests drive this one table, which is why it is a `val` and not a method:
   * the Java provider was re-invoked per parameterised method, and building the rows once instead
   * changes nothing about them, every value in them being immutable.
   */
  private val data_generation: TableFor12[
      LocalDate,
      LocalDate,
      Frequency,
      Option[StubConvention],
      Option[RollConvention],
      BusinessDayAdjustment,
      Option[LocalDate],
      Option[LocalDate],
      Option[BusinessDayAdjustment],
      List[LocalDate],
      List[LocalDate],
      RollConvention] = Table(
    (
      "start",
      "end",
      "frequency",
      "stubConvention",
      "rollConvention",
      "businessDayAdjustment",
      "firstRegularStartDate",
      "lastRegularEndDate",
      "startDateBusinessDayAdjustment",
      "unadjusted",
      "adjusted",
      "expectedRoll"),
    // stub null
    (JUN_17, SEP_17, P1M, None, None, BDA, None, None, None,
      list(JUN_17, JUL_17, AUG_17, SEP_17),
      list(JUN_17, JUL_17, AUG_18, SEP_17),
      DAY_17),

    // stub NONE
    (JUN_17, SEP_17, P1M, Some(STUB_NONE), None, BDA, None, None, None,
      list(JUN_17, JUL_17, AUG_17, SEP_17),
      list(JUN_17, JUL_17, AUG_18, SEP_17),
      DAY_17),
    (JUN_17, JUL_17, P1M, Some(STUB_NONE), None, BDA, None, None, None,
      list(JUN_17, JUL_17),
      list(JUN_17, JUL_17),
      DAY_17),

    // stub SHORT_INITIAL
    (JUN_04, SEP_17, P1M, Some(SHORT_INITIAL), None, BDA, None, None, None,
      list(JUN_04, JUN_17, JUL_17, AUG_17, SEP_17),
      list(JUN_04, JUN_17, JUL_17, AUG_18, SEP_17),
      DAY_17),
    (JUN_17, SEP_17, P1M, Some(SHORT_INITIAL), None, BDA, None, None, None,
      list(JUN_17, JUL_17, AUG_17, SEP_17),
      list(JUN_17, JUL_17, AUG_18, SEP_17),
      DAY_17),
    (JUN_17, JUL_04, P1M, Some(SHORT_INITIAL), None, BDA, None, None, None,
      list(JUN_17, JUL_04),
      list(JUN_17, JUL_04),
      DAY_4),
    (date(2011, 6, 28), date(2011, 6, 30), P1M, Some(SHORT_INITIAL), Some(EOM), BDA, None, None,
      None,
      list(date(2011, 6, 28), date(2011, 6, 30)),
      list(date(2011, 6, 28), date(2011, 6, 30)),
      EOM),
    (date(2014, 12, 12), date(2015, 8, 24), P3M, Some(SHORT_INITIAL), None, BDA, None, None, None,
      list(date(2014, 12, 12), date(2015, 2, 24), date(2015, 5, 24), date(2015, 8, 24)),
      list(date(2014, 12, 12), date(2015, 2, 24), date(2015, 5, 25), date(2015, 8, 24)),
      DAY_24),
    (date(2014, 12, 12), date(2015, 8, 24), P3M, Some(SHORT_INITIAL), Some(RollConventions.NONE),
      BDA, None, None, None,
      list(date(2014, 12, 12), date(2015, 2, 24), date(2015, 5, 24), date(2015, 8, 24)),
      list(date(2014, 12, 12), date(2015, 2, 24), date(2015, 5, 25), date(2015, 8, 24)),
      DAY_24),
    (date(2014, 11, 24), date(2015, 8, 24), P3M, None, Some(RollConventions.NONE), BDA, None, None,
      None,
      list(date(2014, 11, 24), date(2015, 2, 24), date(2015, 5, 24), date(2015, 8, 24)),
      list(date(2014, 11, 24), date(2015, 2, 24), date(2015, 5, 25), date(2015, 8, 24)),
      DAY_24),

    // stub LONG_INITIAL
    (JUN_04, SEP_17, P1M, Some(LONG_INITIAL), None, BDA, None, None, None,
      list(JUN_04, JUL_17, AUG_17, SEP_17),
      list(JUN_04, JUL_17, AUG_18, SEP_17),
      DAY_17),
    (JUN_17, SEP_17, P1M, Some(LONG_INITIAL), None, BDA, None, None, None,
      list(JUN_17, JUL_17, AUG_17, SEP_17),
      list(JUN_17, JUL_17, AUG_18, SEP_17),
      DAY_17),
    (JUN_17, JUL_04, P1M, Some(LONG_INITIAL), None, BDA, None, None, None,
      list(JUN_17, JUL_04),
      list(JUN_17, JUL_04),
      DAY_4),
    (JUN_17, AUG_04, P1M, Some(LONG_INITIAL), None, BDA, None, None, None,
      list(JUN_17, AUG_04),
      list(JUN_17, AUG_04),
      DAY_4),

    // stub SMART_INITIAL
    (JUN_04, SEP_17, P1M, Some(SMART_INITIAL), None, BDA, None, None, None,
      list(JUN_04, JUN_17, JUL_17, AUG_17, SEP_17),
      list(JUN_04, JUN_17, JUL_17, AUG_18, SEP_17),
      DAY_17),
    (JUN_10, SEP_17, P1M, Some(SMART_INITIAL), None, BDA, None, None, None,
      list(JUN_10, JUN_17, JUL_17, AUG_17, SEP_17),
      list(JUN_10, JUN_17, JUL_17, AUG_18, SEP_17),
      DAY_17),
    (JUN_11, SEP_17, P1M, Some(SMART_INITIAL), None, BDA, None, None, None,
      list(JUN_11, JUL_17, AUG_17, SEP_17),
      list(JUN_11, JUL_17, AUG_18, SEP_17),
      DAY_17),
    (JUN_17, JUL_04, P1M, Some(SMART_INITIAL), None, BDA, None, None, None,
      list(JUN_17, JUL_04),
      list(JUN_17, JUL_04),
      DAY_4),

    // stub SHORT_FINAL
    (JUN_04, SEP_17, P1M, Some(SHORT_FINAL), None, BDA, None, None, None,
      list(JUN_04, JUL_04, AUG_04, SEP_04, SEP_17),
      list(JUN_04, JUL_04, AUG_04, SEP_04, SEP_17),
      DAY_4),
    (JUN_17, SEP_17, P1M, Some(SHORT_FINAL), None, BDA, None, None, None,
      list(JUN_17, JUL_17, AUG_17, SEP_17),
      list(JUN_17, JUL_17, AUG_18, SEP_17),
      DAY_17),
    (JUN_17, JUL_04, P1M, Some(SHORT_FINAL), None, BDA, None, None, None,
      list(JUN_17, JUL_04),
      list(JUN_17, JUL_04),
      DAY_17),
    (date(2011, 6, 28), date(2011, 6, 30), P1M, Some(SHORT_FINAL), Some(EOM), BDA, None, None,
      None,
      list(date(2011, 6, 28), date(2011, 6, 30)),
      list(date(2011, 6, 28), date(2011, 6, 30)),
      DAY_28),
    (date(2014, 11, 29), date(2015, 9, 2), P3M, Some(SHORT_FINAL), None, BDA, None, None, None,
      list(date(2014, 11, 29), date(2015, 2, 28), date(2015, 5, 29), date(2015, 8, 29),
        date(2015, 9, 2)),
      list(date(2014, 11, 28), date(2015, 2, 27), date(2015, 5, 29), date(2015, 8, 31),
        date(2015, 9, 2)),
      DAY_29),
    (date(2014, 11, 29), date(2015, 9, 2), P3M, Some(SHORT_FINAL), Some(RollConventions.NONE), BDA,
      None, None, None,
      list(date(2014, 11, 29), date(2015, 2, 28), date(2015, 5, 29), date(2015, 8, 29),
        date(2015, 9, 2)),
      list(date(2014, 11, 28), date(2015, 2, 27), date(2015, 5, 29), date(2015, 8, 31),
        date(2015, 9, 2)),
      DAY_29),

    // stub LONG_FINAL
    (JUN_04, SEP_17, P1M, Some(LONG_FINAL), None, BDA, None, None, None,
      list(JUN_04, JUL_04, AUG_04, SEP_17),
      list(JUN_04, JUL_04, AUG_04, SEP_17),
      DAY_4),
    (JUN_17, SEP_17, P1M, Some(LONG_FINAL), None, BDA, None, None, None,
      list(JUN_17, JUL_17, AUG_17, SEP_17),
      list(JUN_17, JUL_17, AUG_18, SEP_17),
      DAY_17),
    (JUN_17, JUL_04, P1M, Some(LONG_FINAL), None, BDA, None, None, None,
      list(JUN_17, JUL_04),
      list(JUN_17, JUL_04),
      DAY_17),
    (JUN_17, AUG_04, P1M, Some(LONG_FINAL), None, BDA, None, None, None,
      list(JUN_17, AUG_04),
      list(JUN_17, AUG_04),
      DAY_17),

    // stub SMART_FINAL
    (JUN_04, SEP_17, P1M, Some(SMART_FINAL), None, BDA, None, None, None,
      list(JUN_04, JUL_04, AUG_04, SEP_04, SEP_17),
      list(JUN_04, JUL_04, AUG_04, SEP_04, SEP_17),
      DAY_4),
    (JUN_04, SEP_11, P1M, Some(SMART_FINAL), None, BDA, None, None, None,
      list(JUN_04, JUL_04, AUG_04, SEP_04, SEP_11),
      list(JUN_04, JUL_04, AUG_04, SEP_04, SEP_11),
      DAY_4),
    (JUN_04, SEP_10, P1M, Some(SMART_FINAL), None, BDA, None, None, None,
      list(JUN_04, JUL_04, AUG_04, SEP_10),
      list(JUN_04, JUL_04, AUG_04, SEP_10),
      DAY_4),
    (JUN_17, JUL_04, P1M, Some(SMART_FINAL), None, BDA, None, None, None,
      list(JUN_17, JUL_04),
      list(JUN_17, JUL_04),
      DAY_17),

    // explicit initial stub
    (JUN_04, SEP_17, P1M, None, None, BDA, Some(JUN_17), None, None,
      list(JUN_04, JUN_17, JUL_17, AUG_17, SEP_17),
      list(JUN_04, JUN_17, JUL_17, AUG_18, SEP_17),
      DAY_17),
    (JUN_04, SEP_17, P1M, Some(SHORT_INITIAL), None, BDA, Some(JUN_17), None, None,
      list(JUN_04, JUN_17, JUL_17, AUG_17, SEP_17),
      list(JUN_04, JUN_17, JUL_17, AUG_18, SEP_17),
      DAY_17),
    (JUN_17, SEP_17, P1M, None, None, BDA, Some(JUN_17), None, None,
      list(JUN_17, JUL_17, AUG_17, SEP_17),
      list(JUN_17, JUL_17, AUG_18, SEP_17),
      DAY_17),
    (JUN_04, SEP_04, P1M, Some(SMART_FINAL), None, BDA, Some(JUN_17), None, None,
      list(JUN_04, JUN_17, JUL_17, AUG_17, SEP_04),
      list(JUN_04, JUN_17, JUL_17, AUG_18, SEP_04),
      DAY_17),

    // explicit final stub
    (JUN_04, SEP_17, P1M, None, None, BDA, None, Some(AUG_04), None,
      list(JUN_04, JUL_04, AUG_04, SEP_17),
      list(JUN_04, JUL_04, AUG_04, SEP_17),
      DAY_4),
    (JUN_04, SEP_17, P1M, Some(SHORT_FINAL), None, BDA, None, Some(AUG_04), None,
      list(JUN_04, JUL_04, AUG_04, SEP_17),
      list(JUN_04, JUL_04, AUG_04, SEP_17),
      DAY_4),
    (JUN_17, SEP_17, P1M, None, None, BDA, None, Some(AUG_17), None,
      list(JUN_17, JUL_17, AUG_17, SEP_17),
      list(JUN_17, JUL_17, AUG_18, SEP_17),
      DAY_17),
    (JUN_04, SEP_04, P1M, Some(SMART_INITIAL), None, BDA, None, Some(AUG_17), None,
      list(JUN_04, JUN_17, JUL_17, AUG_17, SEP_04),
      list(JUN_04, JUN_17, JUL_17, AUG_18, SEP_04),
      DAY_17),

    // explicit double stub
    (JUN_04, SEP_17, P1M, None, None, BDA, Some(JUL_11), Some(AUG_11), None,
      list(JUN_04, JUL_11, AUG_11, SEP_17),
      list(JUN_04, JUL_11, AUG_11, SEP_17),
      DAY_11),
    (JUN_04, OCT_17, P1M, Some(STUB_BOTH), None, BDA, Some(JUL_11), Some(SEP_11), None,
      list(JUN_04, JUL_11, AUG_11, SEP_11, OCT_17),
      list(JUN_04, JUL_11, AUG_11, SEP_11, OCT_17),
      DAY_11),
    (JUN_17, SEP_17, P1M, None, None, BDA, Some(JUN_17), Some(SEP_17), None,
      list(JUN_17, JUL_17, AUG_17, SEP_17),
      list(JUN_17, JUL_17, AUG_18, SEP_17),
      DAY_17),

    // stub null derive from roll convention
    (JUN_04, SEP_17, P1M, None, Some(DAY_17), BDA, None, None, None,
      list(JUN_04, JUN_17, JUL_17, AUG_17, SEP_17),
      list(JUN_04, JUN_17, JUL_17, AUG_18, SEP_17),
      DAY_17),
    (JUN_04, SEP_17, P1M, None, Some(DAY_4), BDA, None, None, None,
      list(JUN_04, JUL_04, AUG_04, SEP_04, SEP_17),
      list(JUN_04, JUL_04, AUG_04, SEP_04, SEP_17),
      DAY_4),

    // near end of month
    // EOM flag false, thus roll on 30th
    (NOV_30_2013, NOV_30, P3M, Some(STUB_NONE), None, BDA, None, None, None,
      list(NOV_30_2013, FEB_28, MAY_30, AUG_30, NOV_30),
      list(NOV_29_2013, FEB_28, MAY_30, AUG_29, NOV_28),
      DAY_30),
    // EOM flag true and is EOM, thus roll at EOM
    (NOV_30_2013, NOV_30, P3M, Some(STUB_NONE), Some(EOM), BDA, None, None, None,
      list(NOV_30_2013, FEB_28, MAY_31, AUG_31, NOV_30),
      list(NOV_29_2013, FEB_28, MAY_30, AUG_29, NOV_28),
      EOM),
    // EOM flag true, and last business day, thus roll at EOM (stub convention defined)
    (MAY_30, NOV_30, P3M, Some(STUB_NONE), Some(EOM), BDA, None, None, None,
      list(MAY_31, AUG_31, NOV_30),
      list(MAY_30, AUG_29, NOV_28),
      EOM),
    // EOM flag true, and last business day, thus roll at EOM
    (MAY_30, NOV_30, P3M, None, Some(EOM), BDA, None, None, None,
      list(MAY_31, AUG_31, NOV_30),
      list(MAY_30, AUG_29, NOV_28),
      EOM),
    // EOM flag true, and last business day, thus roll at EOM (start adjustment none)
    (MAY_30, NOV_30, P3M, None, Some(EOM), BDA, None, None, Some(BDA_NONE),
      list(MAY_31, AUG_31, NOV_30),
      list(MAY_30, AUG_29, NOV_28),
      EOM),
    // roll date set to 30th, so roll on 30th
    (MAY_30, NOV_30, P3M, None, Some(DAY_30), BDA, None, None, None,
      list(MAY_30, AUG_30, NOV_30),
      list(MAY_30, AUG_29, NOV_28),
      DAY_30),
    // EOM flag true, but not EOM, thus roll on 30th
    (JUL_30, OCT_30, P1M, None, Some(EOM), BDA, None, None, None,
      list(JUL_30, AUG_30, SEP_30, OCT_30),
      list(JUL_30, AUG_29, SEP_30, OCT_30),
      DAY_30),
    // EOM flag true and is EOM, double stub, thus roll at EOM
    (date(2014, 1, 3), SEP_17, P3M, Some(STUB_BOTH), Some(EOM), BDA, Some(FEB_28), Some(AUG_31),
      None,
      list(date(2014, 1, 3), FEB_28, MAY_31, AUG_31, SEP_17),
      list(date(2014, 1, 3), FEB_28, MAY_30, AUG_29, SEP_17),
      EOM),
    // EOM flag true plus start date as last business day of month with start date adjust of NONE
    (NOV_29_2013, NOV_30, P3M, Some(STUB_NONE), Some(EOM), BDA, None, None, Some(BDA_NONE),
      list(NOV_30_2013, FEB_28, MAY_31, AUG_31, NOV_30),
      list(NOV_29_2013, FEB_28, MAY_30, AUG_29, NOV_28),
      EOM),
    // EOM flag true plus start date as last business day of month with start date adjust of NONE
    (NOV_29_2013, NOV_30, P3M, None, Some(EOM), BDA, None, None, Some(BDA_NONE),
      list(NOV_30_2013, FEB_28, MAY_31, AUG_31, NOV_30),
      list(NOV_29_2013, FEB_28, MAY_30, AUG_29, NOV_28),
      EOM),
    // EOM flag false, short initial, implies EOM true
    (date(2011, 6, 2), date(2011, 8, 31), P1M, Some(SHORT_INITIAL), None, BDA, None, None, None,
      list(date(2011, 6, 2), date(2011, 6, 30), date(2011, 7, 31), date(2011, 8, 31)),
      list(date(2011, 6, 2), date(2011, 6, 30), date(2011, 7, 29), date(2011, 8, 31)),
      EOM),
    // EOM flag false, explicit stub, implies EOM true
    (date(2011, 6, 2), date(2011, 8, 31), P1M, None, None, BDA, Some(date(2011, 6, 30)), None,
      None,
      list(date(2011, 6, 2), date(2011, 6, 30), date(2011, 7, 31), date(2011, 8, 31)),
      list(date(2011, 6, 2), date(2011, 6, 30), date(2011, 7, 29), date(2011, 8, 31)),
      EOM),
    // EOM flag false, explicit stub, implies EOM true
    (date(2011, 7, 31), date(2011, 10, 10), P1M, None, None, BDA, None, Some(date(2011, 9, 30)),
      None,
      list(date(2011, 7, 31), date(2011, 8, 31), date(2011, 9, 30), date(2011, 10, 10)),
      list(date(2011, 7, 29), date(2011, 8, 31), date(2011, 9, 30), date(2011, 10, 10)),
      EOM),
    // EOM flag false, explicit stub, implies EOM true
    (date(2011, 2, 2), date(2011, 5, 30), P1M, None, None, BDA, Some(date(2011, 2, 28)), None,
      None,
      list(date(2011, 2, 2), date(2011, 2, 28), date(2011, 3, 30), date(2011, 4, 30),
        date(2011, 5, 30)),
      list(date(2011, 2, 2), date(2011, 2, 28), date(2011, 3, 30), date(2011, 4, 29),
        date(2011, 5, 30)),
      DAY_30),
    // EOM flag true and is EOM, but end date equals start day rather than EOM
    (date(2018, 2, 28), date(2024, 2, 28), freq(Frequency.ofYears(2)), Some(STUB_NONE), Some(EOM),
      BDA, None, None, None,
      list(date(2018, 2, 28), date(2020, 2, 29), date(2022, 2, 28), date(2024, 2, 28)),
      list(date(2018, 2, 28), date(2020, 2, 28), date(2022, 2, 28), date(2024, 2, 28)),
      EOM),
    // EOM flag true and is EOM, but end date equals start day rather than EOM
    (date(2018, 4, 30), date(2018, 10, 30), P2M, Some(STUB_NONE), Some(EOM), BDA, None, None, None,
      list(date(2018, 4, 30), date(2018, 6, 30), date(2018, 8, 31), date(2018, 10, 30)),
      list(date(2018, 4, 30), date(2018, 6, 29), date(2018, 8, 31), date(2018, 10, 30)),
      EOM),

    // pre-adjusted start date, no change needed
    (JUL_17, OCT_17, P1M, None, Some(DAY_17), BDA, None, None, Some(BDA_NONE),
      list(JUL_17, AUG_17, SEP_17, OCT_17),
      list(JUL_17, AUG_18, SEP_17, OCT_17),
      DAY_17),
    // pre-adjusted start date, change needed
    (AUG_18, OCT_17, P1M, None, Some(DAY_17), BDA, None, None, Some(BDA_NONE),
      list(AUG_17, SEP_17, OCT_17),
      list(AUG_18, SEP_17, OCT_17),
      DAY_17),
    // pre-adjusted first regular, change needed
    (JUL_11, OCT_17, P1M, None, Some(DAY_17), BDA, Some(AUG_18), None, Some(BDA_NONE),
      list(JUL_11, AUG_17, SEP_17, OCT_17),
      list(JUL_11, AUG_18, SEP_17, OCT_17),
      DAY_17),
    // pre-adjusted last regular, change needed
    (JUL_17, OCT_17, P1M, None, Some(DAY_17), BDA, None, Some(AUG_18), Some(BDA_NONE),
      list(JUL_17, AUG_17, OCT_17),
      list(JUL_17, AUG_18, OCT_17),
      DAY_17),
    // pre-adjusted first+last regular, change needed
    (APR_01, OCT_17, P1M, None, Some(DAY_17), BDA, Some(MAY_19), Some(AUG_18), Some(BDA_NONE),
      list(APR_01, MAY_17, JUN_17, JUL_17, AUG_17, OCT_17),
      list(APR_01, MAY_19, JUN_17, JUL_17, AUG_18, OCT_17),
      DAY_17),
    // pre-adjusted end date, change needed
    (JUL_17, AUG_18, P1M, None, Some(DAY_17), BDA, None, None, Some(BDA_NONE),
      list(JUL_17, AUG_17),
      list(JUL_17, AUG_18),
      DAY_17),
    // pre-adjusted end date, change needed, with adjustment
    (JUL_17, AUG_18, P1M, None, Some(DAY_17), BDA, None, None, Some(BDA),
      list(JUL_17, AUG_17),
      list(JUL_17, AUG_18),
      DAY_17),

    // TERM period
    (JUN_04, SEP_17, TERM, Some(STUB_NONE), None, BDA, None, None, None,
      list(JUN_04, SEP_17),
      list(JUN_04, SEP_17),
      ROLL_NONE),
    // TERM period defined as a stub and no regular periods
    (JUN_04, SEP_17, P12M, Some(SHORT_INITIAL), None, BDA, Some(SEP_17), None, None,
      list(JUN_04, SEP_17),
      list(JUN_04, SEP_17),
      DAY_17),
    (JUN_04, SEP_17, P12M, Some(SHORT_INITIAL), None, BDA, None, Some(JUN_04), None,
      list(JUN_04, SEP_17),
      list(JUN_04, SEP_17),
      DAY_4),
    (date(2014, 9, 24), date(2016, 11, 24), freq(Frequency.ofYears(2)), Some(SHORT_INITIAL), None,
      BDA, None, None, None,
      list(date(2014, 9, 24), date(2014, 11, 24), date(2016, 11, 24)),
      list(date(2014, 9, 24), date(2014, 11, 24), date(2016, 11, 24)),
      DAY_24),

    // IMM
    (date(2014, 9, 17), date(2014, 10, 15), P1M, Some(STUB_NONE), Some(IMM), BDA, None, None, None,
      list(date(2014, 9, 17), date(2014, 10, 15)),
      list(date(2014, 9, 17), date(2014, 10, 15)),
      IMM),
    (date(2014, 9, 17), date(2014, 10, 15), TERM, Some(STUB_NONE), Some(IMM), BDA, None, None,
      None,
      list(date(2014, 9, 17), date(2014, 10, 15)),
      list(date(2014, 9, 17), date(2014, 10, 15)),
      IMM),
    // IMM with stupid short period still works
    (date(2014, 9, 17), date(2014, 10, 15), freq(Frequency.ofDays(2)), Some(STUB_NONE), Some(IMM),
      BDA, None, None, None,
      list(date(2014, 9, 17), date(2014, 10, 15)),
      list(date(2014, 9, 17), date(2014, 10, 15)),
      IMM),
    (date(2014, 9, 17), date(2014, 10, 1), freq(Frequency.ofDays(2)), Some(STUB_NONE), Some(IMM),
      BDA, None, None, None,
      list(date(2014, 9, 17), date(2014, 10, 1)),
      list(date(2014, 9, 17), date(2014, 10, 1)),
      IMM),

    //IMM with adjusted start dates and various conventions
    //MF, no stub
    (date(2018, 3, 22), date(2020, 3, 18), P6M, Some(STUB_NONE), Some(IMM), BDA_JPY_MF, None, None,
      Some(BDA_NONE),
      list(date(2018, 3, 21), date(2018, 9, 19), date(2019, 3, 20), date(2019, 9, 18),
        date(2020, 3, 18)),
      list(date(2018, 3, 22), date(2018, 9, 19), date(2019, 3, 20), date(2019, 9, 18),
        date(2020, 3, 18)),
      IMM),
    //Preceding, no stub
    (date(2018, 3, 20), date(2019, 3, 20), P6M, Some(STUB_NONE), Some(IMM), BDA_JPY_P, None, None,
      Some(BDA_NONE),
      list(date(2018, 3, 21), date(2018, 9, 19), date(2019, 3, 20)),
      list(date(2018, 3, 20), date(2018, 9, 19), date(2019, 3, 20)),
      IMM),
    //MF, null stub
    (date(2018, 3, 22), date(2019, 3, 20), P6M, None, Some(IMM), BDA_JPY_MF, None, None,
      Some(BDA_NONE),
      list(date(2018, 3, 21), date(2018, 9, 19), date(2019, 3, 20)),
      list(date(2018, 3, 22), date(2018, 9, 19), date(2019, 3, 20)),
      IMM),
    //Explicit long front stub with (adjusted) first regular start date
    (date(2017, 9, 2), date(2018, 9, 19), P6M, Some(LONG_INITIAL), Some(IMM), BDA_JPY_MF,
      Some(date(2018, 3, 22)), None, Some(BDA_NONE),
      list(date(2017, 9, 2), date(2018, 3, 21), date(2018, 9, 19)),
      list(date(2017, 9, 2), date(2018, 3, 22), date(2018, 9, 19)),
      IMM),
    //Implicit short front stub with (adjusted) first regular start date
    (date(2018, 1, 2), date(2018, 9, 19), P6M, None, Some(IMM), BDA_JPY_MF,
      Some(date(2018, 3, 22)), None, Some(BDA_NONE),
      list(date(2018, 1, 2), date(2018, 3, 21), date(2018, 9, 19)),
      list(date(2018, 1, 2), date(2018, 3, 22), date(2018, 9, 19)),
      IMM),
    //Implicit back stub with (adjusted) last regular start date
    (date(2017, 3, 15), date(2018, 5, 19), P6M, None, Some(IMM), BDA_JPY_MF, None,
      Some(date(2018, 3, 22)), Some(BDA_NONE),
      list(date(2017, 3, 15), date(2017, 9, 20), date(2018, 3, 21), date(2018, 5, 19)),
      list(date(2017, 3, 15), date(2017, 9, 20), date(2018, 3, 22), date(2018, 5, 21)),
      IMM),

    // Day30 rolling with February
    (date(2015, 1, 30), date(2015, 4, 30), P1M, Some(STUB_NONE), Some(DAY_30), BDA, None, None,
      None,
      list(date(2015, 1, 30), date(2015, 2, 28), date(2015, 3, 30), date(2015, 4, 30)),
      list(date(2015, 1, 30), date(2015, 2, 27), date(2015, 3, 30), date(2015, 4, 30)),
      DAY_30),
    (date(2015, 2, 28), date(2015, 4, 30), P1M, Some(STUB_NONE), Some(DAY_30), BDA, None, None,
      None,
      list(date(2015, 2, 28), date(2015, 3, 30), date(2015, 4, 30)),
      list(date(2015, 2, 27), date(2015, 3, 30), date(2015, 4, 30)),
      DAY_30),
    (date(2015, 2, 28), date(2015, 4, 30), P1M, Some(SHORT_INITIAL), Some(DAY_30), BDA, None, None,
      None,
      list(date(2015, 2, 28), date(2015, 3, 30), date(2015, 4, 30)),
      list(date(2015, 2, 27), date(2015, 3, 30), date(2015, 4, 30)),
      DAY_30),

    // Two stubs no regular
    (date(2019, 1, 16), date(2020, 10, 22), P12M, None, Some(DAY_22), BDA, Some(date(2020, 1, 22)),
      Some(date(2020, 1, 22)), None,
      list(date(2019, 1, 16), date(2020, 1, 22), date(2020, 10, 22)),
      list(date(2019, 1, 16), date(2020, 1, 22), date(2020, 10, 22)),
      DAY_22),
    (date(2019, 1, 16), date(2020, 10, 22), P12M, Some(STUB_BOTH), Some(DAY_22), BDA,
      Some(date(2020, 1, 22)), Some(date(2020, 1, 22)), None,
      list(date(2019, 1, 16), date(2020, 1, 22), date(2020, 10, 22)),
      list(date(2019, 1, 16), date(2020, 1, 22), date(2020, 10, 22)),
      DAY_22)
  )

  //-------------------------------------------------------------------------
  test("test_of_LocalDateEomFalse") {
    val test: PeriodicSchedule =
      valid(PeriodicSchedule.of(JUN_04, SEP_17, P1M, BDA, SHORT_INITIAL, false))
    test.startDate shouldBe JUN_04
    test.endDate shouldBe SEP_17
    test.frequency shouldBe P1M
    test.businessDayAdjustment shouldBe BDA
    test.startDateBusinessDayAdjustment shouldBe None
    test.endDateBusinessDayAdjustment shouldBe None
    test.stubConvention shouldBe Some(SHORT_INITIAL)
    test.rollConvention shouldBe None
    test.firstRegularStartDate shouldBe None
    test.lastRegularEndDate shouldBe None
    test.overrideStartDate shouldBe None
    test.calculatedRollConvention shouldBe DAY_17
    test.calculatedFirstRegularStartDate shouldBe JUN_04
    test.calculatedLastRegularEndDate shouldBe SEP_17
    test.calculatedStartDate shouldBe AdjustableDate.of(JUN_04, BDA)
    test.calculatedEndDate shouldBe AdjustableDate.of(SEP_17, BDA)
  }

  test("test_of_LocalDateEomTrue") {
    val test: PeriodicSchedule =
      valid(PeriodicSchedule.of(JUN_04, SEP_17, P1M, BDA, SHORT_FINAL, true))
    test.startDate shouldBe JUN_04
    test.endDate shouldBe SEP_17
    test.frequency shouldBe P1M
    test.businessDayAdjustment shouldBe BDA
    test.startDateBusinessDayAdjustment shouldBe None
    test.endDateBusinessDayAdjustment shouldBe None
    test.stubConvention shouldBe Some(SHORT_FINAL)
    test.rollConvention shouldBe Some(EOM)
    test.firstRegularStartDate shouldBe None
    test.lastRegularEndDate shouldBe None
    test.overrideStartDate shouldBe None
    test.calculatedRollConvention shouldBe DAY_4
    test.calculatedFirstRegularStartDate shouldBe JUN_04
    test.calculatedLastRegularEndDate shouldBe SEP_17
    test.calculatedStartDate shouldBe AdjustableDate.of(JUN_04, BDA)
    test.calculatedEndDate shouldBe AdjustableDate.of(SEP_17, BDA)
  }

  test("test_of_LocalDateEom_null") {
    // The Java case passed `null` for each of the five arguments in turn and asserted that the
    // factory refused it. That has no counterpart: this port states absence with `Option` and
    // never writes `null` (Rule 5), and the three convention-typed arguments of this factory are
    // values of closed families, so no absent one can be offered. What the factory does decide is
    // whether the dates it is given describe a schedule, and that is what the name now asserts -
    // the two rejections the Java builder would also have made, reported as failures rather than
    // raised, each naming the properties it compared.
    val sameDates: ResultNec[PeriodicSchedule] =
      PeriodicSchedule.of(SEP_17, SEP_17, P1M, BDA, SHORT_INITIAL, false)
    sameDates should beFailureWith(FailureReason.INVALID)
    failuresOf(sameDates).map(failure => failure.message) shouldBe
      List(
        "Invalid order: Expected 'startDate' < 'endDate', " +
          s"but found: '$SEP_17' >= '$SEP_17'")

    val reversedDates: ResultNec[PeriodicSchedule] =
      PeriodicSchedule.of(SEP_17, JUN_04, P1M, BDA, SHORT_INITIAL, false)
    reversedDates should beFailureWith(FailureReason.INVALID)
    failuresOf(reversedDates).map(failure => failure.message) shouldBe
      List(
        "Invalid order: Expected 'startDate' < 'endDate', " +
          s"but found: '$SEP_17' >= '$JUN_04'")

    // Dates that do describe a schedule are accepted by the same call, so the two rejections above
    // are the factory discriminating rather than refusing everything.
    valid(PeriodicSchedule.of(JUN_04, SEP_17, P1M, BDA, SHORT_INITIAL, false)).startDate shouldBe
      JUN_04
  }

  //-------------------------------------------------------------------------
  test("test_of_LocalDateRoll") {
    val test: PeriodicSchedule =
      valid(PeriodicSchedule.of(JUN_04, SEP_17, P1M, BDA, SHORT_INITIAL, DAY_17))
    test.startDate shouldBe JUN_04
    test.endDate shouldBe SEP_17
    test.frequency shouldBe P1M
    test.businessDayAdjustment shouldBe BDA
    test.startDateBusinessDayAdjustment shouldBe None
    test.endDateBusinessDayAdjustment shouldBe None
    test.stubConvention shouldBe Some(SHORT_INITIAL)
    test.rollConvention shouldBe Some(DAY_17)
    test.firstRegularStartDate shouldBe None
    test.lastRegularEndDate shouldBe None
    test.overrideStartDate shouldBe None
    test.calculatedRollConvention shouldBe DAY_17
    test.calculatedFirstRegularStartDate shouldBe JUN_04
    test.calculatedLastRegularEndDate shouldBe SEP_17
    test.calculatedStartDate shouldBe AdjustableDate.of(JUN_04, BDA)
    test.calculatedEndDate shouldBe AdjustableDate.of(SEP_17, BDA)
  }

  test("test_firstPaymentDate_before_effectiveDate") {
    // Schedule where the combination of override start date and regular first period start date
    // produce a first payment date which is before the (non-overridden) start date.
    val startDate: LocalDate = LocalDate.of(2018, 7, 26)
    val endDate: LocalDate = LocalDate.of(2019, 6, 20)
    val overrideStartDate: LocalDate = LocalDate.of(2018, 3, 20)
    val firstRegularStartDate: LocalDate = LocalDate.of(2018, 6, 20)

    val scheduleDefinition: PeriodicSchedule = valid(
      definition(
        startDate,
        endDate,
        P3M,
        BDA,
        firstRegularStartDate = Some(firstRegularStartDate),
        overrideStartDate = Some(AdjustableDate.of(overrideStartDate))))

    val schedule: Schedule = sched(scheduleDefinition.createSchedule(REF_DATA))
    schedule.size shouldBe 5

    (0 until schedule.size).foreach { index =>
      val expectedStart: LocalDate = overrideStartDate.plusMonths(3L * index.toLong)
      val expectedEnd: LocalDate = expectedStart.plusMonths(3L)
      val expectedPeriod: SchedulePeriod = sp(SchedulePeriod.of(expectedStart, expectedEnd))
      withClue(s"period $index: ") {
        expectedPeriod shouldBe schedule.period(index)
      }
    }
  }

  test("test_of_LocalDateRoll_null") {
    // As `test_of_LocalDateEom_null`: the six null arguments the Java case passed cannot be
    // written here, so the name asserts the validation this overload performs instead. The roll
    // convention is carried into the definition without being checked against the dates - that is
    // schedule creation's decision, which `test_backwards_badStub` and `test_forwards_badStub`
    // assert - so the rejections are again the order of the dates.
    val sameDates: ResultNec[PeriodicSchedule] =
      PeriodicSchedule.of(SEP_17, SEP_17, P1M, BDA, SHORT_INITIAL, DAY_17)
    sameDates should beFailureWith(FailureReason.INVALID)

    val reversedDates: ResultNec[PeriodicSchedule] =
      PeriodicSchedule.of(SEP_17, JUN_04, P1M, BDA, SHORT_INITIAL, DAY_17)
    reversedDates should beFailureWith(FailureReason.INVALID)
    failuresOf(reversedDates) should have size 1

    // The roll convention this overload declares is retained, so the refusals above are about the
    // dates and nothing else.
    valid(PeriodicSchedule.of(JUN_04, SEP_17, P1M, BDA, SHORT_INITIAL, DAY_17)).rollConvention
      .shouldBe(Some(DAY_17))
  }

  //-------------------------------------------------------------------------
  test("test_builder_invalidDateOrder") {
    // start vs end
    createDates(SEP_17, SEP_17, None, None) should beFailureWith(FailureReason.INVALID)
    createDates(SEP_17, JUN_04, None, None) should beFailureWith(FailureReason.INVALID)
    // first/last regular vs start/end
    createDates(JUN_04, SEP_17, Some(JUN_03), None) should beFailureWith(FailureReason.INVALID)
    createDates(JUN_04, SEP_17, None, Some(SEP_18)) should beFailureWith(FailureReason.INVALID)
    // first regular vs last regular
    val allowed: PeriodicSchedule = valid(createDates(JUN_04, SEP_05, Some(SEP_05), Some(SEP_05)))
    allowed.firstRegularStartDate shouldBe Some(SEP_05)
    allowed.lastRegularEndDate shouldBe Some(SEP_05)
    createDates(JUN_04, SEP_17, Some(SEP_05), Some(SEP_04)) should
      beFailureWith(FailureReason.INVALID)
    // first regular vs override start date
    definition(
      JUN_04,
      SEP_17,
      P1M,
      BDA,
      firstRegularStartDate = Some(JUL_17),
      overrideStartDate = Some(AdjustableDate.of(AUG_04))) should
      beFailureWith(FailureReason.INVALID)

    // The validating factory accumulates rather than stopping at the first broken invariant, which
    // the raising validator being ported could not do: dates that break several are reported
    // together, every one of them an `Invalid` naming the two properties it compared.
    val several: List[Failure] = failuresOf(createDates(SEP_17, JUN_04, Some(SEP_05), Some(SEP_04)))
    several.size should be > 1
    several.map(failure => failure.reason).distinct shouldBe List(FailureReason.INVALID)
    several.map(failure => failure.message) shouldBe
      List(
        s"Invalid order: Expected 'startDate' < 'endDate', but found: '$SEP_17' >= '$JUN_04'",
        s"Invalid order: Expected 'firstRegularStartDate' <= 'endDate', but found: '$SEP_05' > " +
          s"'$JUN_04'",
        "Invalid order: Expected 'firstRegularStartDate' <= 'lastRegularEndDate', but found: " +
          s"'$SEP_05' > '$SEP_04'",
        "Invalid order: Expected 'unadjusted' <= 'firstRegularStartDate', but found: " +
          s"'$SEP_17' > '$SEP_05'",
        s"Invalid order: Expected 'unadjusted' <= 'lastRegularEndDate', but found: '$SEP_17' > " +
          s"'$SEP_04'",
        s"Invalid order: Expected 'lastRegularEndDate' <= 'endDate', but found: '$SEP_04' > " +
          s"'$JUN_04'")
  }


  //-------------------------------------------------------------------------
  test("test_monthly_schedule") {
    forAll(data_generation) {
      (
          start: LocalDate,
          end: LocalDate,
          freq: Frequency,
          stubConv: Option[StubConvention],
          rollConv: Option[RollConvention],
          businessDayAdjustment: BusinessDayAdjustment,
          firstReg: Option[LocalDate],
          lastReg: Option[LocalDate],
          startBusDayAdjustment: Option[BusinessDayAdjustment],
          unadjusted: List[LocalDate],
          adjusted: List[LocalDate],
          expRoll: RollConvention) =>
        val defn: PeriodicSchedule = valid(
          definition(
            start,
            end,
            freq,
            businessDayAdjustment,
            startDateBusinessDayAdjustment = startBusDayAdjustment,
            stubConvention = stubConv,
            rollConvention = rollConv,
            firstRegularStartDate = firstReg,
            lastRegularEndDate = lastReg))
        withClue(s"$defn: ") {
          val test: Schedule = sched(defn.createSchedule(REF_DATA))
          test.size shouldBe unadjusted.size - 1
          (0 until test.size).foreach { index =>
            val period: SchedulePeriod = test.period(index)
            withClue(s"period $index: ") {
              period.unadjustedStartDate shouldBe unadjusted(index)
              period.unadjustedEndDate shouldBe unadjusted(index + 1)
              period.startDate shouldBe adjusted(index)
              period.endDate shouldBe adjusted(index + 1)
            }
          }
          test.periodicFrequency shouldBe freq
          test.rollConvention shouldBe expRoll

          // Reference data is threaded explicitly (AAP §0.6.5), and this type offers both forms of
          // that threading: the direct call above and the reader that awaits its data, which a
          // caller composes with other reference-data operations and supplies the data to once.
          // The two agree on every row of the table.
          defn.toReader.run(REF_DATA) shouldBe defn.createSchedule(REF_DATA)
          defn.toReader.run(REF_DATA) shouldBe Right(test)
        }
    }
  }

  test("test_monthly_schedule_withOverride") {
    forAll(data_generation) {
      (
          start: LocalDate,
          end: LocalDate,
          freq: Frequency,
          stubConv: Option[StubConvention],
          rollConv: Option[RollConvention],
          businessDayAdjustment: BusinessDayAdjustment,
          firstReg: Option[LocalDate],
          lastReg: Option[LocalDate],
          startBusDayAdjustment: Option[BusinessDayAdjustment],
          unadjusted: List[LocalDate],
          adjusted: List[LocalDate],
          expRoll: RollConvention) =>
        val defn: PeriodicSchedule = valid(
          definition(
            start,
            end,
            freq,
            businessDayAdjustment,
            startDateBusinessDayAdjustment = startBusDayAdjustment,
            stubConvention = stubConv,
            rollConvention = rollConv,
            firstRegularStartDate = firstReg,
            lastRegularEndDate = lastReg,
            overrideStartDate = Some(
              AdjustableDate.of(date(2011, 1, 9), BusinessDayAdjustment.of(FOLLOWING, SAT_SUN)))))
        withClue(s"$defn: ") {
          val test: Schedule = sched(defn.createSchedule(REF_DATA))
          test.size shouldBe unadjusted.size - 1
          val period0: SchedulePeriod = test.period(0)
          period0.unadjustedStartDate shouldBe date(2011, 1, 9)
          period0.unadjustedEndDate shouldBe unadjusted(1)
          period0.startDate shouldBe date(2011, 1, 10)
          period0.endDate shouldBe adjusted(1)
          (1 until test.size).foreach { index =>
            val period: SchedulePeriod = test.period(index)
            withClue(s"period $index: ") {
              period.unadjustedStartDate shouldBe unadjusted(index)
              period.unadjustedEndDate shouldBe unadjusted(index + 1)
              period.startDate shouldBe adjusted(index)
              period.endDate shouldBe adjusted(index + 1)
            }
          }
          test.periodicFrequency shouldBe freq
          test.rollConvention shouldBe expRoll
        }
    }
  }

  test("test_monthly_unadjusted") {
    forAll(data_generation) {
      (
          start: LocalDate,
          end: LocalDate,
          freq: Frequency,
          stubConv: Option[StubConvention],
          rollConv: Option[RollConvention],
          businessDayAdjustment: BusinessDayAdjustment,
          firstReg: Option[LocalDate],
          lastReg: Option[LocalDate],
          startBusDayAdjustment: Option[BusinessDayAdjustment],
          unadjusted: List[LocalDate],
          _: List[LocalDate],
          _: RollConvention) =>
        val defn: PeriodicSchedule = valid(
          definition(
            start,
            end,
            freq,
            businessDayAdjustment,
            startDateBusinessDayAdjustment = startBusDayAdjustment,
            stubConvention = stubConv,
            rollConvention = rollConv,
            firstRegularStartDate = firstReg,
            lastRegularEndDate = lastReg))
        withClue(s"$defn: ") {
          dates(defn.createUnadjustedDates(REF_DATA)) shouldBe unadjusted
          // createUnadjustedDates() does not work as expected without ReferenceData
          if (startBusDayAdjustment.isEmpty && !rollConv.contains(EOM)) {
            dates(defn.createUnadjustedDates()) shouldBe unadjusted
          }
        }
    }
  }

  test("test_monthly_unadjusted_withOverride") {
    forAll(data_generation) {
      (
          start: LocalDate,
          end: LocalDate,
          freq: Frequency,
          stubConv: Option[StubConvention],
          rollConv: Option[RollConvention],
          businessDayAdjustment: BusinessDayAdjustment,
          firstReg: Option[LocalDate],
          lastReg: Option[LocalDate],
          startBusDayAdjustment: Option[BusinessDayAdjustment],
          unadjusted: List[LocalDate],
          _: List[LocalDate],
          _: RollConvention) =>
        val defn: PeriodicSchedule = valid(
          definition(
            start,
            end,
            freq,
            businessDayAdjustment,
            startDateBusinessDayAdjustment = startBusDayAdjustment,
            stubConvention = stubConv,
            rollConvention = rollConv,
            firstRegularStartDate = firstReg,
            lastRegularEndDate = lastReg,
            overrideStartDate = Some(
              AdjustableDate.of(date(2011, 1, 9), BusinessDayAdjustment.of(FOLLOWING, SAT_SUN)))))
        withClue(s"$defn: ") {
          val test: List[LocalDate] = dates(defn.createUnadjustedDates(REF_DATA))
          test.head shouldBe date(2011, 1, 9)
          test.drop(1) shouldBe unadjusted.slice(1, test.size)
          // createUnadjustedDates() does not work as expected without ReferenceData
          if (startBusDayAdjustment.isEmpty && !rollConv.contains(EOM)) {
            val testNoRefData: List[LocalDate] = dates(defn.createUnadjustedDates())
            testNoRefData.head shouldBe date(2011, 1, 9)
            testNoRefData.drop(1) shouldBe unadjusted.slice(1, testNoRefData.size)
          }
        }
    }
  }

  test("test_monthly_adjusted") {
    forAll(data_generation) {
      (
          start: LocalDate,
          end: LocalDate,
          freq: Frequency,
          stubConv: Option[StubConvention],
          rollConv: Option[RollConvention],
          businessDayAdjustment: BusinessDayAdjustment,
          firstReg: Option[LocalDate],
          lastReg: Option[LocalDate],
          startBusDayAdjustment: Option[BusinessDayAdjustment],
          _: List[LocalDate],
          adjusted: List[LocalDate],
          _: RollConvention) =>
        val defn: PeriodicSchedule = valid(
          definition(
            start,
            end,
            freq,
            businessDayAdjustment,
            startDateBusinessDayAdjustment = startBusDayAdjustment,
            stubConvention = stubConv,
            rollConvention = rollConv,
            firstRegularStartDate = firstReg,
            lastRegularEndDate = lastReg))
        withClue(s"$defn: ") {
          dates(defn.createAdjustedDates(REF_DATA)) shouldBe adjusted
        }
    }
  }

  test("test_monthly_adjusted_withOverride") {
    forAll(data_generation) {
      (
          start: LocalDate,
          end: LocalDate,
          freq: Frequency,
          stubConv: Option[StubConvention],
          rollConv: Option[RollConvention],
          businessDayAdjustment: BusinessDayAdjustment,
          firstReg: Option[LocalDate],
          lastReg: Option[LocalDate],
          startBusDayAdjustment: Option[BusinessDayAdjustment],
          _: List[LocalDate],
          adjusted: List[LocalDate],
          _: RollConvention) =>
        val defn: PeriodicSchedule = valid(
          definition(
            start,
            end,
            freq,
            businessDayAdjustment,
            startDateBusinessDayAdjustment = startBusDayAdjustment,
            stubConvention = stubConv,
            rollConvention = rollConv,
            firstRegularStartDate = firstReg,
            lastRegularEndDate = lastReg,
            overrideStartDate = Some(
              AdjustableDate.of(date(2011, 1, 9), BusinessDayAdjustment.of(FOLLOWING, SAT_SUN)))))
        withClue(s"$defn: ") {
          val test: List[LocalDate] = dates(defn.createAdjustedDates(REF_DATA))
          test.head shouldBe date(2011, 1, 10)
          test.drop(1) shouldBe adjusted.slice(1, test.size)
        }
    }
  }


  //-------------------------------------------------------------------------
  test("test_override_fallbackWhenStartDateMismatch") {
    val defn: PeriodicSchedule = valid(
      definition(
        JUL_04,
        SEP_17,
        P1M,
        BDA,
        rollConvention = Some(DAY_17),
        overrideStartDate =
          Some(AdjustableDate.of(JUN_17, BusinessDayAdjustment.of(FOLLOWING, SAT_SUN)))))
    val test: Schedule = sched(defn.createSchedule(REF_DATA))
    test.size shouldBe 3
    val period0: SchedulePeriod = test.period(0)
    period0.unadjustedStartDate shouldBe JUN_17
    period0.unadjustedEndDate shouldBe JUL_17
    period0.startDate shouldBe JUN_17
    period0.endDate shouldBe JUL_17
    val period1: SchedulePeriod = test.period(1)
    period1.unadjustedStartDate shouldBe JUL_17
    period1.unadjustedEndDate shouldBe AUG_17
    period1.startDate shouldBe JUL_17
    period1.endDate shouldBe AUG_18
    val period2: SchedulePeriod = test.period(2)
    period2.unadjustedStartDate shouldBe AUG_17
    period2.unadjustedEndDate shouldBe SEP_17
    period2.startDate shouldBe AUG_18
    period2.endDate shouldBe SEP_17
  }

  test("test_override_fallbackWhenStartDateMismatchEndStub") {
    val defn: PeriodicSchedule = valid(
      definition(
        JUL_04,
        SEP_04,
        P1M,
        BDA,
        rollConvention = Some(DAY_17),
        lastRegularEndDate = Some(AUG_17),
        overrideStartDate =
          Some(AdjustableDate.of(JUN_17, BusinessDayAdjustment.of(FOLLOWING, SAT_SUN)))))
    val test: Schedule = sched(defn.createSchedule(REF_DATA))
    test.size shouldBe 3
    val period0: SchedulePeriod = test.period(0)
    period0.unadjustedStartDate shouldBe JUN_17
    period0.unadjustedEndDate shouldBe JUL_17
    period0.startDate shouldBe JUN_17
    period0.endDate shouldBe JUL_17
    val period1: SchedulePeriod = test.period(1)
    period1.unadjustedStartDate shouldBe JUL_17
    period1.unadjustedEndDate shouldBe AUG_17
    period1.startDate shouldBe JUL_17
    period1.endDate shouldBe AUG_18
    val period2: SchedulePeriod = test.period(2)
    period2.unadjustedStartDate shouldBe AUG_17
    period2.unadjustedEndDate shouldBe SEP_04
    period2.startDate shouldBe AUG_18
    period2.endDate shouldBe SEP_04
  }

  //-------------------------------------------------------------------------
  test("test_startEndAdjust") {
    val bda1: BusinessDayAdjustment = BusinessDayAdjustment.of(PRECEDING, SAT_SUN)
    val bda2: BusinessDayAdjustment = BusinessDayAdjustment.of(MODIFIED_PRECEDING, SAT_SUN)
    val test: PeriodicSchedule = valid(
      definition(
        date(2014, 10, 4),
        date(2015, 4, 4),
        P3M,
        BDA,
        startDateBusinessDayAdjustment = Some(bda1),
        endDateBusinessDayAdjustment = Some(bda2),
        stubConvention = Some(STUB_NONE)))
    test.calculatedStartDate shouldBe AdjustableDate.of(date(2014, 10, 4), bda1)
    test.calculatedEndDate shouldBe AdjustableDate.of(date(2015, 4, 4), bda2)
    dates(test.createUnadjustedDates()) shouldBe
      list(date(2014, 10, 4), date(2015, 1, 4), date(2015, 4, 4))
    dates(test.createAdjustedDates(REF_DATA)) shouldBe
      list(date(2014, 10, 3), date(2015, 1, 5), date(2015, 4, 3))
  }

  //-------------------------------------------------------------------------
  /**
   * The definitions whose start date is replaced and what the replacement holds, transcribed
   * verbatim from the Java provider of the same name, with its comments.
   *
   * All eleven rows are present and the fourteen columns keep the Java order: the replacement
   * start date, then the ten properties of the definition it is applied to, then the unadjusted
   * dates the replacement generates - absent where the replacement is expected to be refused - and
   * the stub convention, last regular end date and roll convention the replacement is expected to
   * carry.
   */
  private val data_replace: TableFor14[
      LocalDate,
      LocalDate,
      LocalDate,
      Frequency,
      Option[StubConvention],
      Option[RollConvention],
      BusinessDayAdjustment,
      Option[LocalDate],
      Option[LocalDate],
      Option[BusinessDayAdjustment],
      Option[List[LocalDate]],
      Option[StubConvention],
      Option[LocalDate],
      Option[RollConvention]] = Table(
    (
      "replaceStart",
      "start",
      "end",
      "frequency",
      "stubConvention",
      "rollConvention",
      "businessDayAdjustment",
      "firstRegularStartDate",
      "lastRegularEndDate",
      "startDateBusinessDayAdjustment",
      "unadjusted",
      "expectedStubConvention",
      "expectedLastRegular",
      "expectedRollConvention"),
    // SmartInitial is set
    (JUN_11, JUN_17, AUG_17, P1M, None, Some(DAY_17), BDA, None, None, Some(BDA_JPY_P),
      Some(list(JUN_11, JUL_17, AUG_17)),
      Some(SMART_INITIAL), None, Some(DAY_17)),
    // SmartInitial not set
    (MAY_19, JUN_17, AUG_17, P1M, Some(LONG_INITIAL), Some(DAY_17), BDA, Some(JUN_17),
      Some(AUG_17), Some(BDA_JPY_P),
      Some(list(MAY_19, JUL_17, AUG_17)),
      Some(LONG_INITIAL), Some(AUG_17), Some(DAY_17)),
    // start set to be later
    (JUL_04, JUN_17, AUG_17, P1M, None, Some(DAY_17), BDA, Some(JUN_17), Some(AUG_17),
      Some(BDA_JPY_P),
      Some(list(JUL_04, JUL_17, AUG_17)),
      Some(SMART_INITIAL), Some(AUG_17), Some(DAY_17)),
    // original schedule had no stubs and NONE, new schedule uses SmartInitial instead
    (JUN_04, JUN_17, AUG_17, P1M, Some(STUB_NONE), None, BDA, None, None, None,
      Some(list(JUN_04, JUN_17, JUL_17, AUG_17)),
      Some(SMART_INITIAL), None, None),
    // original schedule had double stubs with stub convention and first regular date to determine
    // roll of 17th
    // new schedule uses SmartInitial and calculated last regular
    (JUN_04, JUN_03, AUG_30, P1M, Some(SMART_FINAL), None, BDA, Some(JUN_17), None, None,
      Some(list(JUN_04, JUN_17, JUL_17, AUG_17, AUG_30)),
      Some(SMART_INITIAL), Some(AUG_17), None),
    // original schedule had double explicit stubs, new schedule uses SmartInitial instead of first
    // regular
    (JUN_04, JUN_03, AUG_30, P1M, None, None, BDA, Some(JUN_17), Some(AUG_17), None,
      Some(list(JUN_04, JUN_17, JUL_17, AUG_17, AUG_30)),
      Some(SMART_INITIAL), Some(AUG_17), None),
    // original schedule had double explicit stubs and BOTH, new schedule uses SmartInitial instead
    // of first regular
    (JUN_04, JUN_03, AUG_30, P1M, Some(STUB_BOTH), None, BDA, Some(JUN_17), Some(AUG_17), None,
      Some(list(JUN_04, JUN_17, JUL_17, AUG_17, AUG_30)),
      Some(SMART_INITIAL), Some(AUG_17), None),
    // original schedule had first regular date, new schedule just uses SmartInitial
    (JUN_04, JUN_03, AUG_17, P1M, None, None, BDA, Some(JUN_17), None, None,
      Some(list(JUN_04, JUN_17, JUL_17, AUG_17)),
      Some(SMART_INITIAL), None, None),
    // original schedule had last regular date and uneccessary final stub convention
    (JUN_04, JUN_17, AUG_04, P1M, Some(SHORT_FINAL), None, BDA, None, Some(JUL_17), None,
      Some(list(JUN_04, JUN_17, JUL_17, AUG_04)),
      Some(SMART_INITIAL), Some(JUL_17), None),
    // original schedule was final, but resulted in Term schedule, new schedule retains the stub
    // convention
    (JUN_04, JUL_17, AUG_17, P1M, Some(SHORT_FINAL), None, BDA, None, None, None,
      Some(list(JUN_04, JUL_04, AUG_04, AUG_17)),
      Some(SHORT_FINAL), None, None),
    // cannot set start after end
    (SEP_04, JUN_17, AUG_17, P1M, None, Some(DAY_17), BDA, Some(JUN_17), Some(AUG_17),
      Some(BDA_JPY_P),
      None,
      None, None, None)
  )

  test("test_replace") {
    forAll(data_replace) {
      (
          replaceStart: LocalDate,
          start: LocalDate,
          end: LocalDate,
          freq: Frequency,
          stubConv: Option[StubConvention],
          rollConv: Option[RollConvention],
          businessDayAdjustment: BusinessDayAdjustment,
          firstReg: Option[LocalDate],
          lastReg: Option[LocalDate],
          startBusDayAdjustment: Option[BusinessDayAdjustment],
          unadjusted: Option[List[LocalDate]],
          expectedStubConvention: Option[StubConvention],
          expectedLastRegular: Option[LocalDate],
          expectedRollConvention: Option[RollConvention]) =>
        val base: PeriodicSchedule = valid(
          definition(
            start,
            end,
            freq,
            businessDayAdjustment,
            startDateBusinessDayAdjustment = startBusDayAdjustment,
            stubConvention = stubConv,
            rollConvention = rollConv,
            firstRegularStartDate = firstReg,
            lastRegularEndDate = lastReg))
        withClue(s"$base replaced with $replaceStart: ") {
          unadjusted match {
            case None =>
              // The row that cannot set the start after the end. The Java case asserted a raised
              // `IllegalArgumentException` from the replacement followed by schedule creation;
              // here the replacement itself reports the refusal as a value, so the whole
              // expression - the replacement and the creation it feeds - is a `Left` (Rule 5).
              val replaced: ResultNec[Schedule] = base
                .replaceStartDate(replaceStart)
                .flatMap(defn =>
                  defn.createSchedule(REF_DATA).left.map(failure => NonEmptyChain.one(failure)))
              replaced should beFailureWith(FailureReason.INVALID)
              replaced.left.map(failures =>
                failures.toNonEmptyList.toList.map(failure => failure.message)) shouldBe
                Left(List("Cannot alter leg to have start date after end date"))
              replaced.left.map(failures =>
                failures.toNonEmptyList.toList.flatMap(failure =>
                  failure.attributes.get("definition"))) shouldBe Left(List(base.toString))
            case Some(expectedDates) =>
              val test: PeriodicSchedule = valid(base.replaceStartDate(replaceStart))
              test.overrideStartDate shouldBe None
              test.startDate shouldBe replaceStart
              test.startDateBusinessDayAdjustment shouldBe Some(BDA_NONE)
              test.firstRegularStartDate shouldBe None
              test.businessDayAdjustment shouldBe businessDayAdjustment
              test.lastRegularEndDate shouldBe expectedLastRegular
              test.endDate shouldBe end
              test.endDateBusinessDayAdjustment shouldBe None
              test.stubConvention shouldBe expectedStubConvention
              test.rollConvention shouldBe expectedRollConvention
              dates(test.createUnadjustedDates()) shouldBe expectedDates
          }
        }
    }
  }

  //-------------------------------------------------------------------------
  test("test_none_badStub") {
    // Jun 4th to Sep 17th requires a stub, but NONE specified
    val defn: PeriodicSchedule = valid(
      definition(
        JUN_04,
        SEP_17,
        P1M,
        BDA,
        stubConvention = Some(STUB_NONE),
        rollConvention = Some(DAY_4),
        firstRegularStartDate = None,
        lastRegularEndDate = None))
    generationFailure(defn.createUnadjustedDates(), defn).message shouldBe
      s"Period '$JUN_04' to '$SEP_17' resulted in a disallowed stub with frequency 'P1M'"
  }

  test("test_none_stubDate") {
    // Jun 17th to Sep 17th is correct for NONE stub convention, but firstRegularStartDate specified
    val defn: PeriodicSchedule = valid(
      definition(
        JUN_17,
        SEP_17,
        P1M,
        BDA,
        stubConvention = Some(STUB_NONE),
        rollConvention = Some(DAY_4),
        firstRegularStartDate = Some(JUL_17),
        lastRegularEndDate = None))
    generationFailure(defn.createUnadjustedDates(), defn).message shouldBe
      "Dates specify an explicit stub, but stub convention is 'None'"
  }

  test("test_both_badStub") {
    val defn: PeriodicSchedule = valid(
      definition(
        JUN_17,
        SEP_17,
        P1M,
        BDA,
        stubConvention = Some(STUB_BOTH),
        rollConvention = None,
        firstRegularStartDate = Some(JUN_17),
        lastRegularEndDate = Some(SEP_17)))
    generationFailure(defn.createUnadjustedDates(), defn).message shouldBe
      "Stub convention is 'Both' but explicit dates not specified"
  }

  test("test_backwards_badStub") {
    val defn: PeriodicSchedule = valid(
      definition(
        JUN_17,
        SEP_17,
        P1M,
        BDA,
        stubConvention = Some(SHORT_INITIAL),
        rollConvention = Some(DAY_11),
        firstRegularStartDate = None,
        lastRegularEndDate = None))
    generationFailure(defn.createUnadjustedDates(), defn).message should
      fullyMatch regex ".*does not match roll convention 'Day11' when starting to roll backwards"
  }

  test("test_forwards_badStub") {
    val defn: PeriodicSchedule = valid(
      definition(
        JUN_17,
        SEP_17,
        P1M,
        BDA,
        stubConvention = Some(SHORT_FINAL),
        rollConvention = Some(DAY_11),
        firstRegularStartDate = None,
        lastRegularEndDate = None))
    generationFailure(defn.createUnadjustedDates(), defn).message should
      fullyMatch regex ".*does not match roll convention 'Day11' when starting to roll forwards"
  }

  //-------------------------------------------------------------------------
  test("test_termFrequency_badInitialStub") {
    val defn: PeriodicSchedule = valid(
      definition(
        JUN_04,
        SEP_17,
        TERM,
        BDA,
        stubConvention = Some(STUB_NONE),
        rollConvention = Some(DAY_4),
        firstRegularStartDate = Some(JUN_17),
        lastRegularEndDate = None))
    generationFailure(defn.createUnadjustedDates(), defn).message shouldBe
      "Explicit stubs must not be specified when using 'Term' frequency"
  }

  test("test_termFrequency_badFinalStub") {
    val defn: PeriodicSchedule = valid(
      definition(
        JUN_04,
        SEP_17,
        TERM,
        BDA,
        stubConvention = Some(STUB_NONE),
        rollConvention = Some(DAY_4),
        firstRegularStartDate = None,
        lastRegularEndDate = Some(SEP_04)))
    generationFailure(defn.createUnadjustedDates(), defn).message shouldBe
      "Explicit stubs must not be specified when using 'Term' frequency"
  }


  //-------------------------------------------------------------------------
  test("test_emptyWhenAdjusted_term_createUnadjustedDates") {
    val defn: PeriodicSchedule = valid(
      definition(
        date(2015, 5, 29),
        date(2015, 5, 31),
        TERM,
        BDA,
        stubConvention = None,
        rollConvention = None,
        firstRegularStartDate = None,
        lastRegularEndDate = None))
    dates(defn.createUnadjustedDates()) shouldBe list(date(2015, 5, 29), date(2015, 5, 31))
  }

  test("test_emptyWhenAdjusted_term_createAdjustedDates") {
    val defn: PeriodicSchedule = valid(
      definition(
        date(2015, 5, 29),
        date(2015, 5, 31),
        TERM,
        BDA,
        stubConvention = None,
        rollConvention = None,
        firstRegularStartDate = None,
        lastRegularEndDate = None))
    val result: FailureOr[List[LocalDate]] = defn.createAdjustedDates(REF_DATA)
    result should haveFailureMessageMatching(".*duplicate adjusted dates.*")
    generationFailure(result, defn)
  }

  test("test_emptyWhenAdjusted_term_createSchedule") {
    val defn: PeriodicSchedule = valid(
      definition(
        date(2015, 5, 29),
        date(2015, 5, 31),
        TERM,
        BDA,
        stubConvention = None,
        rollConvention = None,
        firstRegularStartDate = None,
        lastRegularEndDate = None))
    val result: FailureOr[Schedule] = defn.createSchedule(REF_DATA)
    result should haveFailureMessageMatching(".*duplicate adjusted dates.*")
    generationFailure(result, defn)
  }

  test("test_combinePeriodsWhenNecessary_1w_createSchedule") {
    // Rule 4 substitution. The Java case built an anonymous `HolidayCalendar` whose holidays were
    // the weekend plus the first eight days of October 2020, under the identifier "calendar".
    // `HolidayCalendar` is a closed family here, so no subtype can be declared outside its file
    // and that anonymous class cannot be written at all. The faithful substitute is the built-in
    // immutable calendar carrying exactly those holidays: its year range is derived from them, so
    // it covers the whole of 2020, and outside that range it falls back to the weekend - which is
    // precisely what the anonymous class did for every date, the definition below lying entirely
    // within 2020.
    val id: HolidayCalendarId = HolidayCalendarId.of("calendar")
    val calendar: ImmutableHolidayCalendar = ImmutableHolidayCalendar.of(
      id,
      (1 to 8).toList.map(day => LocalDate.of(2020, 10, day)),
      Set(SATURDAY, SUNDAY))
    calendar.id shouldBe id
    calendar.isHoliday(date(2020, 10, 1)) shouldBe true
    calendar.isHoliday(date(2020, 10, 8)) shouldBe true
    calendar.isHoliday(date(2020, 10, 9)) shouldBe false
    calendar.isHoliday(date(2020, 9, 26)) shouldBe true // Saturday

    val referenceData: ReferenceData = ImmutableReferenceData.of(id, calendar)
    val businessDayAdjustment: BusinessDayAdjustment =
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING, id)
    val defn: PeriodicSchedule = valid(
      definition(
        date(2020, 9, 18),
        date(2020, 12, 18),
        P1W,
        businessDayAdjustment,
        stubConvention = Some(SHORT_FINAL),
        rollConvention = None))

    val schedule: Schedule = sched(defn.createSchedule(referenceData, true))
    schedule.periods.size shouldBe 12
    schedule.period(1).startDate shouldBe date(2020, 9, 25)
    schedule.period(2).startDate shouldBe date(2020, 10, 9)
  }

  test("test_combinePeriodsWhenNecessary_1d_createSchedule_duplicate_exception") {
    // Despite the Java method name this is a success case: combining is requested, so the runs of
    // coincident adjusted dates that a daily frequency produces over weekends are merged into one
    // boundary instead of being reported as a failure. The name is kept verbatim because the
    // acceptance gate joins on it.
    val id: HolidayCalendarId = SAT_SUN
    val calendar = HolidayCalendars.SAT_SUN

    val referenceData: ReferenceData = ImmutableReferenceData.of(id, calendar)
    val businessDayAdjustment: BusinessDayAdjustment =
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING, id)
    val defn: PeriodicSchedule = valid(
      definition(
        date(2020, 9, 18),
        date(2020, 12, 18),
        P1D,
        businessDayAdjustment,
        stubConvention = Some(SHORT_FINAL),
        rollConvention = None))

    val schedule: Schedule = sched(defn.createSchedule(referenceData, true))
    schedule.periods.size shouldBe 65
    schedule.period(0).startDate shouldBe date(2020, 9, 18)
    schedule.period(0).endDate shouldBe date(2020, 9, 21)
  }

  test("test_combinePeriodsWhenNecessary_1d_createSchedule") {
    val id: HolidayCalendarId = SAT_SUN
    val calendar = HolidayCalendars.SAT_SUN

    val referenceData: ReferenceData = ImmutableReferenceData.of(id, calendar)
    val businessDayAdjustment: BusinessDayAdjustment =
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING, id)
    val defn: PeriodicSchedule = valid(
      definition(
        date(2020, 9, 18),
        date(2020, 12, 18),
        P1D,
        businessDayAdjustment,
        stubConvention = Some(SHORT_FINAL),
        rollConvention = None))

    val result: FailureOr[Schedule] = defn.createSchedule(referenceData, false)
    result should haveFailureMessageMatching(".*duplicate adjusted dates.*")
    generationFailure(result, defn)
  }

  test("test_emptyWhenAdjusted_twoPeriods_createUnadjustedDates") {
    val defn: PeriodicSchedule = valid(
      definition(
        date(2015, 5, 27),
        date(2015, 5, 31),
        freq(Frequency.ofDays(2)),
        BDA,
        stubConvention = Some(STUB_NONE),
        rollConvention = None,
        firstRegularStartDate = None,
        lastRegularEndDate = None))
    dates(defn.createUnadjustedDates()) shouldBe
      list(date(2015, 5, 27), date(2015, 5, 29), date(2015, 5, 31))
  }

  test("test_emptyWhenAdjusted_twoPeriods_createAdjustedDates") {
    val defn: PeriodicSchedule = valid(
      definition(
        date(2015, 5, 27),
        date(2015, 5, 31),
        freq(Frequency.ofDays(2)),
        BDA,
        stubConvention = Some(STUB_NONE),
        rollConvention = None,
        firstRegularStartDate = None,
        lastRegularEndDate = None))
    val result: FailureOr[List[LocalDate]] = defn.createAdjustedDates(REF_DATA)
    result should haveFailureMessageMatching(".*duplicate adjusted dates.*")
    generationFailure(result, defn)
  }

  test("test_emptyWhenAdjusted_twoPeriods_createSchedule") {
    val defn: PeriodicSchedule = valid(
      definition(
        date(2015, 5, 27),
        date(2015, 5, 31),
        freq(Frequency.ofDays(2)),
        BDA,
        stubConvention = Some(STUB_NONE),
        rollConvention = None,
        firstRegularStartDate = None,
        lastRegularEndDate = None))
    val result: FailureOr[Schedule] = defn.createSchedule(REF_DATA)
    result should haveFailureMessageMatching(".*duplicate adjusted dates.*")
    generationFailure(result, defn)
  }

  test("test_brokenWhenAdjusted_twoPeriods_createSchedule") {
    // Rule 4 substitution. The Java case declared an anonymous `BusinessDayConvention`
    // ("TestBack3OnSun", moving a Sunday back three days) so that the adjusted dates came out
    // unsorted while remaining distinct, and asserted the message
    // "Schedule calculation resulted in invalid period". `BusinessDayConvention` is a closed
    // seven-member family here, so that anonymous class cannot be written; the proof of the
    // sealing is below. The same failure is reached with two real conventions instead: the
    // unadjusted dates straddle a weekend (Wed 27th, Sat 30th, Sun 31st of May 2015), the
    // schedule's own adjustment moves the interior Saturday forwards to Mon 1st June, and the end
    // date's own adjustment moves the final Sunday backwards to Fri 29th May. The adjusted dates
    // are therefore Wed 27th, Mon 1st, Fri 29th - out of order and with no duplicate, which is
    // exactly the state the anonymous convention produced, so the general message is reported
    // rather than the duplicate-dates one.
    val defn: PeriodicSchedule = valid(
      definition(
        date(2015, 5, 27),
        date(2015, 5, 31),
        freq(Frequency.ofDays(3)),
        BusinessDayAdjustment.of(FOLLOWING, SAT_SUN),
        endDateBusinessDayAdjustment = Some(BusinessDayAdjustment.of(PRECEDING, SAT_SUN)),
        stubConvention = Some(SHORT_FINAL),
        rollConvention = None,
        firstRegularStartDate = None,
        lastRegularEndDate = None))

    // The unadjusted dates are sorted and distinct; the adjusted ones are neither in order nor
    // duplicated, which is what the case is about.
    dates(defn.createUnadjustedDates()) shouldBe
      list(date(2015, 5, 27), date(2015, 5, 30), date(2015, 5, 31))
    dates(defn.createAdjustedDates(REF_DATA)) shouldBe
      list(date(2015, 5, 27), date(2015, 6, 1), date(2015, 5, 29))

    val result: FailureOr[Schedule] = defn.createSchedule(REF_DATA)
    generationFailure(result, defn).message shouldBe
      "Schedule calculation resulted in invalid period"

    // The sealing itself: no business day convention can be declared outside the file that
    // declares the family, which is why the Java anonymous convention has no counterpart.
    assertDoesNotCompile(
      """object TestBack3OnSun extends com.opengamma.strata.basics.date.BusinessDayConvention {
           def adjust(
               date: LocalDate,
               calendar: com.opengamma.strata.basics.date.HolidayCalendar): LocalDate = date
         }""")
  }

  test("test_emptyWhenAdjusted_badRoll_createUnadjustedDates") {
    // Rule 3 and Rule 4 substitution, and the one Java case whose scenario is unreachable by
    // construction rather than merely inexpressible. The Java case declared an anonymous
    // `RollConvention` holding a mutable `seen` flag, so that its first `next` answered the date
    // it was given and the schedule came out with duplicate unadjusted dates. Neither half of
    // that can exist here: the family is closed (Rule 4), so no subtype can be declared outside
    // its file, and nothing in this port carries mutable state (Rule 3).
    assertDoesNotCompile(
      """object TestRoll extends RollConvention("Test") {
           def adjust(date: LocalDate): LocalDate = date
         }""")
    assertDoesNotCompile("""RollConvention("Test")""")

    // Beyond being undeclarable, the behaviour the anonymous convention faked cannot arise from
    // any of the real conventions: `next` and `previous` each answer a date strictly after or
    // strictly before the one they are given, correcting by a month where adding or subtracting
    // the frequency would not move past it. The closest real analogue of the Java scenario is the
    // IMM convention rolled with a frequency far shorter than a month - the very configuration the
    // generation table covers - where every rolled date would otherwise land back on the same
    // third Wednesday. It yields strictly increasing dates and a schedule, not duplicates.
    val shortRoll: PeriodicSchedule = valid(
      definition(
        date(2014, 9, 17),
        date(2014, 10, 15),
        freq(Frequency.ofDays(2)),
        BDA,
        stubConvention = Some(STUB_NONE),
        rollConvention = Some(IMM),
        firstRegularStartDate = None,
        lastRegularEndDate = None))
    val generated: List[LocalDate] = dates(shortRoll.createUnadjustedDates())
    generated shouldBe list(date(2014, 9, 17), date(2014, 10, 15))
    generated shouldBe generated.distinct
    generated.sliding(2).forall(pair => pair.head.isBefore(pair(1))) shouldBe true
    IMM.next(date(2014, 9, 17), freq(Frequency.ofDays(2))) should
      be > date(2014, 9, 17)
    IMM.previous(date(2014, 10, 15), freq(Frequency.ofDays(2))) should
      be < date(2014, 10, 15)
  }


  //-------------------------------------------------------------------------
  test("coverage_equals") {
    forAll(data_generation) {
      (
          start: LocalDate,
          end: LocalDate,
          freq: Frequency,
          stubConv: Option[StubConvention],
          rollConv: Option[RollConvention],
          busDayAdjustment: BusinessDayAdjustment,
          firstReg: Option[LocalDate],
          lastReg: Option[LocalDate],
          _: Option[BusinessDayAdjustment],
          _: List[LocalDate],
          _: List[LocalDate],
          _: RollConvention) =>
        // The thirteen variants of the Java case, each differing from the first in one property.
        // Note that `LocalDate.MIN` and `LocalDate.MAX` are accepted rather than rejected: the
        // factory decides the order of the dates and nothing about their magnitude, so both
        // variants are values and the assertion is the inequality, as in Java.
        val a1: PeriodicSchedule = valid(
          definition(
            start,
            end,
            freq,
            busDayAdjustment,
            stubConvention = stubConv,
            rollConvention = rollConv,
            firstRegularStartDate = firstReg,
            lastRegularEndDate = lastReg))
        val a2: PeriodicSchedule = valid(
          definition(
            start,
            end,
            freq,
            busDayAdjustment,
            stubConvention = stubConv,
            rollConvention = rollConv,
            firstRegularStartDate = firstReg,
            lastRegularEndDate = lastReg))
        val b: PeriodicSchedule = valid(
          definition(
            LocalDate.MIN,
            end,
            freq,
            busDayAdjustment,
            stubConvention = stubConv,
            rollConvention = rollConv,
            firstRegularStartDate = firstReg,
            lastRegularEndDate = lastReg))
        val c: PeriodicSchedule = valid(
          definition(
            start,
            LocalDate.MAX,
            freq,
            busDayAdjustment,
            stubConvention = stubConv,
            rollConvention = rollConv,
            firstRegularStartDate = firstReg,
            lastRegularEndDate = lastReg))
        val d: PeriodicSchedule = valid(
          definition(
            start,
            end,
            if (freq == P1M) P3M else P1M,
            busDayAdjustment,
            stubConvention = stubConv,
            rollConvention = rollConv,
            firstRegularStartDate = firstReg,
            lastRegularEndDate = lastReg))
        val e: PeriodicSchedule = valid(
          definition(
            start,
            end,
            freq,
            BDA_NONE,
            stubConvention = stubConv,
            rollConvention = rollConv,
            firstRegularStartDate = firstReg,
            lastRegularEndDate = lastReg))
        val f: PeriodicSchedule = valid(
          definition(
            start,
            end,
            freq,
            busDayAdjustment,
            stubConvention = Some(if (stubConv.contains(STUB_NONE)) SHORT_FINAL else STUB_NONE),
            rollConvention = rollConv,
            firstRegularStartDate = firstReg,
            lastRegularEndDate = lastReg))
        val g: PeriodicSchedule = valid(
          definition(
            start,
            end,
            freq,
            busDayAdjustment,
            stubConvention = stubConv,
            rollConvention = Some(SFE),
            firstRegularStartDate = firstReg,
            lastRegularEndDate = lastReg))
        val h: PeriodicSchedule = valid(
          definition(
            start,
            end,
            freq,
            busDayAdjustment,
            stubConvention = stubConv,
            rollConvention = rollConv,
            firstRegularStartDate = Some(start.plusDays(1L)),
            lastRegularEndDate = None))
        val i: PeriodicSchedule = valid(
          definition(
            start,
            end,
            freq,
            busDayAdjustment,
            stubConvention = stubConv,
            rollConvention = rollConv,
            firstRegularStartDate = None,
            lastRegularEndDate = Some(end.minusDays(1L))))
        val j: PeriodicSchedule = valid(
          definition(
            start,
            end,
            freq,
            busDayAdjustment,
            startDateBusinessDayAdjustment = Some(BDA),
            stubConvention = stubConv,
            rollConvention = rollConv,
            firstRegularStartDate = firstReg,
            lastRegularEndDate = lastReg))
        val k: PeriodicSchedule = valid(
          definition(
            start,
            end,
            freq,
            busDayAdjustment,
            endDateBusinessDayAdjustment = Some(BDA),
            stubConvention = stubConv,
            rollConvention = rollConv,
            firstRegularStartDate = firstReg,
            lastRegularEndDate = lastReg))
        val m: PeriodicSchedule = valid(
          definition(
            start,
            end,
            freq,
            busDayAdjustment,
            stubConvention = stubConv,
            rollConvention = rollConv,
            firstRegularStartDate = firstReg,
            lastRegularEndDate = lastReg,
            overrideStartDate = Some(AdjustableDate.of(start.minusDays(1L)))))
        withClue(s"$a1: ") {
          (a1 == a1) shouldBe true
          (a1 == a2) shouldBe true
          (a1 == b) shouldBe false
          (a1 == c) shouldBe false
          (a1 == d) shouldBe false
          (a1 == e) shouldBe false
          (a1 == f) shouldBe false
          (a1 == g) shouldBe false
          (a1 == h) shouldBe false
          (a1 == i) shouldBe false
          (a1 == j) shouldBe false
          (a1 == k) shouldBe false
          (a1 == m) shouldBe false

          // The published instances agree with that equality: `Eq` is the equality above and
          // `Hash` is the hash of the same eleven properties, so two equal definitions hash alike.
          Eq[PeriodicSchedule].eqv(a1, a2) shouldBe true
          Eq[PeriodicSchedule].eqv(a1, b) shouldBe false
          Hash[PeriodicSchedule].hash(a1) shouldBe a1.hashCode
          Hash[PeriodicSchedule].hash(a2) shouldBe a1.hashCode
        }
    }
  }

  test("coverage_builder") {
    // The bean builder has no target, so the eleven properties the Java case set are set through
    // the full-field factory, and the same value is reached again through the `with*` copies that
    // replace the builder - each of which re-validates, so no sequence of them can arrive at a
    // definition the factory would have refused.
    val test: PeriodicSchedule = valid(
      definition(
        JUL_17,
        SEP_17,
        P2M,
        BDA_NONE,
        startDateBusinessDayAdjustment = Some(BDA_NONE),
        endDateBusinessDayAdjustment = Some(BDA_NONE),
        stubConvention = Some(STUB_NONE),
        rollConvention = Some(EOM),
        firstRegularStartDate = Some(JUL_17),
        lastRegularEndDate = Some(SEP_17),
        overrideStartDate = Some(AdjustableDate.of(JUL_11))))
    test.startDate shouldBe JUL_17
    test.endDate shouldBe SEP_17
    test.calculatedStartDate shouldBe AdjustableDate.of(JUL_11, BDA_NONE)
    test.calculatedEndDate shouldBe AdjustableDate.of(SEP_17, BDA_NONE)

    val chained: ResultNec[PeriodicSchedule] = PeriodicSchedule
      .of(JUL_17, SEP_17, P2M, BDA_NONE)
      .flatMap(defn => defn.withStartDateBusinessDayAdjustment(Some(BDA_NONE)))
      .flatMap(defn => defn.withEndDateBusinessDayAdjustment(Some(BDA_NONE)))
      .flatMap(defn => defn.withStubConvention(Some(STUB_NONE)))
      .flatMap(defn => defn.withRollConvention(Some(EOM)))
      .flatMap(defn => defn.withFirstRegularStartDate(Some(JUL_17)))
      .flatMap(defn => defn.withLastRegularEndDate(Some(SEP_17)))
      .flatMap(defn => defn.withOverrideStartDate(Some(AdjustableDate.of(JUL_11))))
    valid(chained) shouldBe test

    // A `with*` copy that breaks an invariant is refused rather than built, which is the builder
    // behaviour this replaces.
    test.withEndDate(JUN_04) should beFailureWith(FailureReason.INVALID)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // `coverImmutableBean` walked the bean's meta-properties reflectively. There is no meta-bean
    // to walk, so the ground it covered is asserted directly: the properties of two distinct
    // definitions and the instances the companion publishes in place of the bean's equality,
    // hashing and generated text.
    val bda: BusinessDayAdjustment = BusinessDayAdjustment.of(FOLLOWING, SAT_SUN)
    val defn: PeriodicSchedule = valid(
      PeriodicSchedule.of(
        date(2014, JUNE, 4),
        date(2014, SEPTEMBER, 17),
        P1M,
        bda,
        SHORT_INITIAL,
        false))
    val other: PeriodicSchedule = valid(
      definition(
        JUL_17,
        OCT_17,
        P3M,
        BDA,
        endDateBusinessDayAdjustment = Some(BDA_JPY_MF),
        stubConvention = Some(SHORT_FINAL),
        rollConvention = Some(DAY_17)))
    defn.startDate shouldBe date(2014, JUNE, 4)
    defn.endDate shouldBe date(2014, SEPTEMBER, 17)
    defn.frequency shouldBe P1M
    defn.businessDayAdjustment shouldBe bda
    defn.stubConvention shouldBe Some(SHORT_INITIAL)
    other.frequency shouldBe P3M
    other.endDateBusinessDayAdjustment shouldBe Some(BDA_JPY_MF)
    other.rollConvention shouldBe Some(DAY_17)
    Eq[PeriodicSchedule].eqv(defn, defn) shouldBe true
    Eq[PeriodicSchedule].eqv(defn, other) shouldBe false
    Hash[PeriodicSchedule].hash(defn) shouldBe defn.hashCode
    Hash[PeriodicSchedule].hash(other) shouldBe other.hashCode
    Show[PeriodicSchedule].show(defn) shouldBe defn.toString
    Show[PeriodicSchedule].show(other) shouldBe other.toString
    defn.toString shouldBe
      "PeriodicSchedule(startDate=2014-06-04, endDate=2014-09-17, frequency=P1M, " +
        "businessDayAdjustment=Following using calendar Sat/Sun, stubConvention=ShortInitial)"
  }

  test("test_serialization") {
    // `assertSerialization` checked Java serialization, which this port does not support, and
    // Joda-Beans wire compatibility is out of scope (AAP §0.2.2). The replacement is the circe
    // round trip through the compile-time derived product codec: the keys are the Java property
    // names in declaration order, an absent optional property is omitted from the object rather
    // than written with an empty value, and a payload whose dates are out of order is rejected by
    // the validating decoder instead of being carried into a value.
    val bda: BusinessDayAdjustment = BusinessDayAdjustment.of(FOLLOWING, SAT_SUN)
    val defn: PeriodicSchedule = valid(
      PeriodicSchedule.of(
        date(2014, JUNE, 4),
        date(2014, SEPTEMBER, 17),
        P1M,
        bda,
        SHORT_INITIAL,
        false))
    val encoded: Json = defn.asJson
    encoded.noSpaces shouldBe
      """{"startDate":"2014-06-04","endDate":"2014-09-17","frequency":"P1M",""" +
        """"businessDayAdjustment":{"convention":"Following","calendar":"Sat/Sun"},""" +
        """"stubConvention":"ShortInitial"}"""
    decode[PeriodicSchedule](encoded.noSpaces) shouldBe Right(defn)

    // The six optional properties this definition does not declare are omitted, which is the
    // `Codecs.dropNulls` contract.
    encoded.asObject.map(json => json.keys.toList) shouldBe
      Some(
        List(
          "startDate",
          "endDate",
          "frequency",
          "businessDayAdjustment",
          "stubConvention"))

    // Equal values encode to identical bytes, no property having a representation that depends on
    // how it was built.
    valid(
      PeriodicSchedule.of(
        date(2014, JUNE, 4),
        date(2014, SEPTEMBER, 17),
        P1M,
        bda,
        SHORT_INITIAL,
        false)).asJson.noSpaces shouldBe encoded.noSpaces

    // A definition whose optional properties are all declared round-trips too, so the omission
    // above is the absence of a value and not the loss of one.
    val full: PeriodicSchedule = valid(
      definition(
        JUL_17,
        OCT_17,
        P2M,
        BDA,
        startDateBusinessDayAdjustment = Some(BDA_NONE),
        endDateBusinessDayAdjustment = Some(BDA_JPY_P),
        stubConvention = Some(STUB_BOTH),
        rollConvention = Some(DAY_17),
        firstRegularStartDate = Some(AUG_17),
        lastRegularEndDate = Some(SEP_17),
        overrideStartDate = Some(AdjustableDate.of(JUL_11, BDA))))
    decode[PeriodicSchedule](full.asJson.noSpaces) shouldBe Right(full)
    full.asJson.asObject.map(json => json.keys.size) shouldBe Some(11)

    // The dates of a payload are decided exactly as a caller's arguments are: out of order, they
    // are a decoding failure carrying the reasons rather than a value the factory would not have
    // built.
    val outOfOrder: String =
      """{"startDate":"2014-09-17","endDate":"2014-06-04","frequency":"P1M",""" +
        """"businessDayAdjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""
    decode[PeriodicSchedule](outOfOrder).fold(
      error => error shouldBe a[DecodingFailure],
      value => fail(s"Expected a decoding failure but a definition was produced: $value"))

    // A missing required property and a frequency that names no frequency are rejected as well.
    decode[PeriodicSchedule]("""{"endDate":"2014-09-17","frequency":"P1M"}""").isLeft shouldBe true
    decode[PeriodicSchedule](
      """{"startDate":"2014-06-04","endDate":"2014-09-17","frequency":"NotAFrequency",""" +
        """"businessDayAdjustment":{"convention":"Following","calendar":"Sat/Sun"}}""")
      .isLeft shouldBe true
  }

  //-------------------------------------------------------------------------
  // The four tests below have no counterpart in the Java test class, for the reason given in the
  // scaladoc of this spec: each states a containment or an ordering that the ported implementation
  // did not have, so there was no Java method to port. They are named in this file's style and are
  // not part of the traceability roster, which runs from a manifest row to a test case.

  test("test_generation_boundedPeriodCount") {
    // A definition asks for as many periods as its dates and frequency imply, and both are chosen
    // by the caller, so generation is bounded: a daily frequency over a span wider than the
    // ceiling is refused rather than materialised (CWE-400). The refusal names the limit and
    // carries the rejected definition, as every refusal of this type does.
    //
    // The limit is a number of *periods*, which is what the message says, so the boundary is
    // asserted on periods and not on dates: a schedule of n dates has n - 1 periods.
    val start: LocalDate = date(2000, 1, 1)
    val tooManyPeriods: String =
      "Schedule calculation resulted in more than 100000 periods, which is the maximum number " +
        "of periods that can be generated"

    // Exactly the ceiling's worth of periods is generated in full: one hundred thousand and one
    // daily dates, and a schedule of one hundred thousand periods built from them.
    val widest: PeriodicSchedule = valid(
      definition(
        start,
        start.plusDays(100000L),
        P1D,
        BDA_NONE,
        stubConvention = Some(SHORT_FINAL)))
    dates(widest.createUnadjustedDates()).size shouldBe 100001
    sched(widest.createSchedule(REF_DATA)).size shouldBe 100000

    // One day wider is one period too many, and is refused - through the dates alone and through
    // schedule creation, the two routes being the same generation.
    val tooWide: PeriodicSchedule = valid(
      definition(
        start,
        start.plusDays(100001L),
        P1D,
        BDA_NONE,
        stubConvention = Some(SHORT_FINAL)))
    generationFailure(tooWide.createUnadjustedDates(), tooWide).message shouldBe tooManyPeriods
    generationFailure(tooWide.createSchedule(REF_DATA), tooWide).message shouldBe tooManyPeriods

    // The preflight that refuses an oversized span before anything is materialised, asserted on
    // the predicate itself. It is conservative: it answers true only where the span is provably
    // above the ceiling - every step advances at most the length of the period, or of the one-month
    // step a convention falls back to, plus the convention's adjustment, so the span divided by
    // that is a lower bound on the steps the walk must take - and false for every span a generation
    // completes, including the one exactly at the ceiling above.
    provablyOversized(date(1000, 1, 1), date(999999, 1, 1), P1D) shouldBe true
    provablyOversized(LocalDate.MIN, LocalDate.MAX, P1D) shouldBe true
    provablyOversized(LocalDate.MIN, LocalDate.MAX, P12M) shouldBe true
    provablyOversized(start, start.plusDays(100000L), P1D) shouldBe false
    provablyOversized(start, date(2003, 1, 1), P1D) shouldBe false
    provablyOversized(start, start.plusYears(100L), P3M) shouldBe false
    provablyOversized(LocalDate.MAX, LocalDate.MIN, P1D) shouldBe false

    // Between the two sits the bound inside the walk, and this is the span that shows it is still
    // doing the work: one million days of daily periods divided by the widest step any frequency
    // and roll convention can take is far below the ceiling, so the preflight proves nothing here
    // and the refusal can only be the walk stopping once it has rolled the ceiling's worth.
    provablyOversized(start, start.plusDays(1000000L), P1D) shouldBe false
    val beyondTheWalk: PeriodicSchedule = valid(
      definition(
        start,
        start.plusDays(1000000L),
        P1D,
        BDA_NONE,
        stubConvention = Some(SHORT_FINAL)))
    generationFailure(beyondTheWalk.createUnadjustedDates(), beyondTheWalk).message shouldBe
      tooManyPeriods

    // And the same refusal reached through the public members, on a definition wide enough for the
    // preflight to prove the ceiling unreachable: every route into generation reports it, with the
    // message the ceiling names, and none of them walks the span to find out.
    val enormous: PeriodicSchedule = valid(
      definition(
        start,
        date(200000, 1, 1),
        P1D,
        BDA_NONE,
        stubConvention = Some(SHORT_FINAL)))
    val refused: FailureOr[List[LocalDate]] = enormous.createUnadjustedDates()
    generationFailure(refused, enormous).message shouldBe tooManyPeriods
    generationFailure(enormous.createUnadjustedDates(REF_DATA), enormous).message shouldBe
      tooManyPeriods
    generationFailure(enormous.createAdjustedDates(REF_DATA), enormous).message shouldBe
      tooManyPeriods
    generationFailure(enormous.createSchedule(REF_DATA), enormous).message shouldBe tooManyPeriods

    // A daily schedule over a few years - the everyday large case - is generated in full, dates
    // and periods alike, so nothing about the bound changes what this library actually builds.
    val threeYears: PeriodicSchedule = valid(
      definition(
        start,
        date(2003, 1, 1),
        P1D,
        BDA_NONE,
        stubConvention = Some(SHORT_FINAL)))
    val generated: List[LocalDate] = dates(threeYears.createUnadjustedDates())
    generated.size shouldBe 1097
    generated.head shouldBe start
    generated.last shouldBe date(2003, 1, 1)
    sched(threeYears.createSchedule(REF_DATA)).size shouldBe 1096
  }

  test("test_generation_datesOutsideSupportedRange") {
    // The roll arithmetic of a generation is total over almost the whole of `LocalDate` and fails
    // within one frequency of its two extremes, where `java.time` raises. That is a failure of the
    // data of a definition, so it is reported through the channel every other generation failure
    // uses (AAP §0.3.3) rather than raised out of a member that answers with `Either`.
    val backwards: PeriodicSchedule = valid(
      definition(
        LocalDate.MIN,
        LocalDate.MIN.plusMonths(1L),
        P2M,
        BDA_NONE,
        stubConvention = Some(SHORT_INITIAL)))
    generationFailure(backwards.createUnadjustedDates(), backwards).message shouldBe
      "Schedule calculation moved outside the range of supported dates"

    val forwards: PeriodicSchedule = valid(
      definition(
        LocalDate.MAX.minusDays(1L),
        LocalDate.MAX,
        P1M,
        BDA_NONE,
        stubConvention = Some(SHORT_FINAL)))
    generationFailure(forwards.createUnadjustedDates(), forwards).message shouldBe
      "Schedule calculation moved outside the range of supported dates"

    // Every route into generation reports it, not only the one that generates dates alone.
    generationFailure(backwards.createSchedule(REF_DATA), backwards).message shouldBe
      "Schedule calculation moved outside the range of supported dates"
    generationFailure(forwards.createAdjustedDates(REF_DATA), forwards).message shouldBe
      "Schedule calculation moved outside the range of supported dates"
  }

  test("test_invalidPeriod_branchOrder") {
    // The definition below is pre-adjusted, which is the case on which the two generations of a
    // definition differ, and it is what makes the order of the failure branches observable.
    //
    // The start date is the 28th of November 2014, a Friday, declared with no adjustment of its
    // own and a 'Day30' roll convention: the 30th of that month is a Sunday which
    // 'ModifiedFollowing' maps back onto the 28th, so schedule creation recovers the 30th as the
    // unadjusted start date, and generation from it succeeds. The no-argument
    // `createUnadjustedDates` performs no such recovery - it generates from the declared dates -
    // so it rolls forwards from the 28th, which 'Day30' does not match, and reports that.
    val defn: PeriodicSchedule = valid(
      definition(
        NOV_28,
        date(2015, 2, 1),
        P1M,
        BDA,
        startDateBusinessDayAdjustment = Some(BDA_NONE),
        endDateBusinessDayAdjustment = Some(BusinessDayAdjustment.of(PRECEDING, SAT_SUN)),
        stubConvention = Some(SHORT_FINAL),
        rollConvention = Some(DAY_30)))

    // The two branches, each reached on its own, so the order asserted below is an order over
    // failures that both genuinely occur. The no-argument generation reports the roll mismatch of
    // the declared start date; the reference-data generation succeeds and its adjusted dates hold
    // a duplicate, the end date's own 'Preceding' adjustment mapping the 1st of February back onto
    // the 30th of January, which is already a boundary.
    generationFailure(defn.createUnadjustedDates(), defn).message shouldBe
      "Date '2014-11-28' does not match roll convention 'Day30' when starting to roll forwards"
    dates(defn.createUnadjustedDates(REF_DATA)) shouldBe
      list(NOV_30, date(2014, 12, 30), date(2015, 1, 30), date(2015, 2, 1))
    // The duplicate-date message names its two lists exactly as the ported message formatter did,
    // between square brackets - `[2014-11-28, ...]` - and not as a Scala `List(...)`, so a caller
    // matching on the text of a rejected schedule reads what it always read.
    generationFailure(defn.createAdjustedDates(REF_DATA), defn).message shouldBe
      "Schedule calculation resulted in duplicate adjusted dates " +
        "[2014-11-28, 2014-12-30, 2015-01-30, 2015-01-30] from unadjusted dates " +
        "[2014-11-30, 2014-12-30, 2015-01-30, 2015-02-01] using adjustment " +
        s"'$BDA'"

    // Schedule creation therefore cannot build its periods, and the failure it reports is the
    // first of those two - the no-argument one - which is the order the ported implementation
    // reported in [PeriodicSchedule.java:466-473]. Reporting on the reference-data-derived lists
    // instead would answer with the duplicate-adjusted-dates message asserted above.
    generationFailure(defn.createSchedule(REF_DATA), defn).message shouldBe
      "Date '2014-11-28' does not match roll convention 'Day30' when starting to roll forwards"

    // Asking for coincident boundaries to be combined removes the reason the branch was reached at
    // all, so this definition then produces a schedule of two periods. The branch is reported on
    // only where the periods really cannot be built, which is the behaviour of the ported form.
    sched(defn.createSchedule(REF_DATA, true)).size shouldBe 2
  }

  test("test_interiorAdjustmentResolvedOnce") {
    // The interior dates take `businessDayAdjustment`, and it is resolved to an adjuster once per
    // generation rather than per date. The observable half of that change is when the resolution
    // happens: a schedule with no interior date puts nothing through the adjustment - the ported
    // loop ran from the second date to the second-to-last, so it had no iterations - and must
    // therefore still produce its schedule when that adjustment names a calendar the supplied
    // reference data does not hold.
    val missing: HolidayCalendarId = HolidayCalendarId.of("NotSupplied")
    val onlySatSun: ReferenceData = ImmutableReferenceData.of(SAT_SUN, HolidayCalendars.SAT_SUN)

    val twoDates: PeriodicSchedule = valid(
      definition(
        JUL_17,
        AUG_17,
        P1M,
        BusinessDayAdjustment.of(MODIFIED_FOLLOWING, missing),
        startDateBusinessDayAdjustment = Some(BDA),
        endDateBusinessDayAdjustment = Some(BDA),
        stubConvention = Some(STUB_NONE),
        rollConvention = Some(ROLL_NONE)))
    dates(twoDates.createUnadjustedDates(onlySatSun)) shouldBe list(JUL_17, AUG_17)
    dates(twoDates.createAdjustedDates(onlySatSun)) shouldBe list(JUL_17, AUG_18)
    sched(twoDates.createSchedule(onlySatSun)).size shouldBe 1

    // With an interior date the adjustment is resolved, so the same missing calendar is reported -
    // the laziness above is about there being nothing to adjust, not about the adjustment being
    // skipped.
    val threeDates: PeriodicSchedule = valid(
      definition(
        JUL_17,
        SEP_17,
        P1M,
        BusinessDayAdjustment.of(MODIFIED_FOLLOWING, missing),
        startDateBusinessDayAdjustment = Some(BDA),
        endDateBusinessDayAdjustment = Some(BDA),
        stubConvention = Some(STUB_NONE),
        rollConvention = Some(ROLL_NONE)))
    threeDates.createAdjustedDates(onlySatSun) should beFailureWith(FailureReason.MISSING_DATA)
    threeDates.createSchedule(onlySatSun) should beFailureWith(FailureReason.MISSING_DATA)

    // And where the calendar is supplied, the interior dates really are adjusted by it: the 17th
    // of August 2014 is a Sunday, which 'ModifiedFollowing' moves forwards to the Monday, while
    // the two ends keep their own adjustments.
    val resolvable: PeriodicSchedule = valid(
      definition(
        JUL_17,
        SEP_17,
        P1M,
        BDA,
        stubConvention = Some(STUB_NONE),
        rollConvention = Some(ROLL_NONE)))
    dates(resolvable.createUnadjustedDates(REF_DATA)) shouldBe list(JUL_17, AUG_17, SEP_17)
    dates(resolvable.createAdjustedDates(REF_DATA)) shouldBe list(JUL_17, AUG_18, SEP_17)
  }

}
