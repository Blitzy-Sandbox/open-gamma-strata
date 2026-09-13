/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.schedule

import java.time.LocalDate
import java.time.Month.AUGUST
import java.time.Month.DECEMBER
import java.time.Month.JULY
import java.time.Month.JUNE
import java.time.Month.NOVEMBER
import java.time.Month.OCTOBER
import java.time.Month.SEPTEMBER

import cats.Eq
import cats.Hash
import cats.Show
import cats.data.NonEmptyChain
import cats.data.NonEmptyList

import io.circe.DecodingFailure
import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.date.DateAdjuster
import com.opengamma.strata.basics.date.DayCount
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Tests [[Schedule]]: its accessors, its stub classification, the two merging algorithms, date
 * adjustment, the typeclass instances and the circe codec.
 *
 * Conventions that hold across the spec:
 *
 *   - A schedule is built through [[Schedule.of]], whose period list is a
 *     `cats.data.NonEmptyList`, and the validated result is unwrapped by [[sched]]; a schedule of
 *     one period covering the whole term is [[Schedule.ofTerm]].
 *   - `merge`, `mergeRegular` and `toAdjusted` report failure as `Left(Failure.Invalid)`, whose
 *     message text is asserted alongside its reason; `mergeToTerm` and `toUnadjusted` are total.
 *   - `Schedule` is a [[com.opengamma.strata.basics.date.DayCount.ScheduleInfo]], which declares
 *     `startDate`, `endDate`, `frequency` and `periodEndDate` as `Option`s, while
 *     [[Schedule.adjustedStartDate]], [[Schedule.adjustedEndDate]] and
 *     [[Schedule.periodicFrequency]] are the plainly typed readings. The accessor tests assert
 *     both readings of the schedule they build, reaching the `Option`-valued ones through an
 *     explicitly bound `info` view.
 *   - `stubs(preferFinal)` answers a `(Option[SchedulePeriod], Option[SchedulePeriod])`, so an
 *     expected pair is written `((initial, final))`.
 *   - `Schedule.period(index)` refuses an index outside the schedule through `ArgCheck`, which is
 *     the one raised refusal this spec asserts.
 */
class ScheduleSpec extends AnyFunSuite with Matchers with ResultMatchers {

  //-------------------------------------------------------------------------
  private val JUN_15: LocalDate = date(2014, JUNE, 15)
  private val JUN_16: LocalDate = date(2014, JUNE, 16)
  private val JUL_03: LocalDate = date(2014, JULY, 3)
  private val JUL_04: LocalDate = date(2014, JULY, 4)
  private val JUL_16: LocalDate = date(2014, JULY, 16)
  private val JUL_17: LocalDate = date(2014, JULY, 17)
  private val AUG_16: LocalDate = date(2014, AUGUST, 16)
  private val AUG_17: LocalDate = date(2014, AUGUST, 17)
  private val SEP_17: LocalDate = date(2014, SEPTEMBER, 17)
  private val SEP_30: LocalDate = date(2014, SEPTEMBER, 30)
  private val OCT_15: LocalDate = date(2014, OCTOBER, 15)
  private val OCT_17: LocalDate = date(2014, OCTOBER, 17)
  private val NOV_17: LocalDate = date(2014, NOVEMBER, 17)
  private val DEC_17: LocalDate = date(2014, DECEMBER, 17)

  //-------------------------------------------------------------------------
  /**
   * Unwraps a period built by the validated factory, failing the test if it was rejected.
   *
   * `SchedulePeriod.of` reports a pair of dates that describes no period as a chain of reasons
   * rather than by raising, so a fixture the spec intends to be valid that turns out not to be is
   * a failure of the spec, reported with the reasons the factory gave.
   *
   * @param result  the result of the validated factory
   * @return the period the factory built
   */
  private def sp(result: ResultNec[SchedulePeriod]): SchedulePeriod =
    result.fold(failures => fail(rejectionMessage("schedule period", failures)), period => period)

  /**
   * Unwraps a schedule built by the validated factory, failing the test if it was rejected.
   *
   * @param result  the result of the validated factory
   * @return the schedule the factory built
   */
  private def sched(result: ResultNec[Schedule]): Schedule =
    result.fold(failures => fail(rejectionMessage("schedule", failures)), schedule => schedule)

  /**
   * Reads the failures out of an outcome that is expected to hold some.
   *
   * The counterpart of [[sched]], for the cases that assert what the factory refused: the chain
   * is flattened into a list so that the number of reasons and the text of each can be asserted,
   * and an outcome that unexpectedly holds a schedule is reported as a failure of the spec rather
   * than read as no reasons at all.
   *
   * @param result  the outcome of a validated factory, expected to have been refused
   * @return the failures the factory reported, in the order it reported them
   */
  private def rejectionsOf(result: ResultNec[Schedule]): List[Failure] =
    result.fold(
      failures => failures.toNonEmptyList.toList,
      schedule => fail(s"Expected the factory to refuse its input but it built: $schedule"))

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
   * The message of a failed schedule operation, failing the test if the operation succeeded.
   *
   * @param result  the result of the operation
   * @return the message of the failure the operation reported
   */
  private def messageOf(result: FailureOr[Schedule]): String =
    result.fold(
      failure => failure.message,
      schedule => fail(s"Expected a failure but a schedule was produced: $schedule"))

  /**
   * A one-month period of the lookup fixtures, running between two month offsets from [[JUN_15]].
   *
   * The adjusted dates are the unadjusted ones, which is what the lookup reads: `periodEndDate`
   * compares the adjusted pair of each period.
   *
   * @param startMonths  the month offset of the start date
   * @param endMonths  the month offset of the end date, which is after the start offset
   * @return the period
   */
  private def monthlyPeriod(startMonths: Long, endMonths: Long): SchedulePeriod =
    sp(SchedulePeriod.of(JUN_15.plusMonths(startMonths), JUN_15.plusMonths(endMonths)))

  /**
   * A chain of one-month periods, each starting `step` months after the one before it.
   *
   * A step of one produces the adjacent periods a generated schedule has. A step of two leaves a
   * one-month gap between each period and the next, which [[Schedule.of]] accepts - gaps are
   * allowed, disorder is not - and which is the shape that makes the `contains` test the lookup
   * performs after locating its candidate period load-bearing.
   *
   * @param count  the number of periods, which is at least one
   * @param step  the number of months from one period's start date to the next period's
   * @return the periods, running from earliest to latest
   */
  private def monthlyChain(count: Long, step: Long): NonEmptyList[SchedulePeriod] =
    NonEmptyList(
      monthlyPeriod(0L, 1L),
      (1L until count).toList.map(index => monthlyPeriod(index * step, index * step + 1L)))

  /**
   * The naive lookup that [[Schedule.periodEndDate]] answers exactly: the end date of the first
   * period containing the date, found by walking the periods.
   *
   * @param schedule  the schedule to search
   * @param date  the date to find
   * @return the end date of the first period containing the date, empty if none does
   */
  private def linearPeriodEndDate(schedule: Schedule, date: LocalDate): Option[LocalDate] =
    schedule.periods.find(period => period.contains(date)).map(period => period.endDate)

  /**
   * Every date a lookup over a schedule is probed at.
   *
   * Each period contributes seven dates - the day before its start date, its start date, the day
   * after it, its midpoint, the day before its end date, its end date and the day after it - and
   * the span of the schedule is then probed day by day, extended three days at each end so that a
   * date before the first period and a date after the last one are probed as well. Over a
   * schedule with gaps the day-by-day sweep covers every date of every gap.
   *
   * @param schedule  the schedule to probe
   * @return the probe dates
   */
  private def lookupProbes(schedule: Schedule): List[LocalDate] = {
    val boundaries: List[LocalDate] = (0 until schedule.size).toList.flatMap { index =>
      val period: SchedulePeriod = schedule.period(index)
      val midpoint: LocalDate =
        period.startDate.plusDays((period.endDate.toEpochDay - period.startDate.toEpochDay) / 2L)
      List(
        period.startDate.minusDays(1L),
        period.startDate,
        period.startDate.plusDays(1L),
        midpoint,
        period.endDate.minusDays(1L),
        period.endDate,
        period.endDate.plusDays(1L))
    }
    val first: LocalDate = schedule.adjustedStartDate.minusDays(3L)
    val span: Long = schedule.adjustedEndDate.plusDays(3L).toEpochDay - first.toEpochDay
    boundaries ::: (0L to span).toList.map(offset => first.plusDays(offset))
  }

  /** The absence of a stub, typed so that the tuples `stubs` answers compare without inference. */
  private val NoStub: Option[SchedulePeriod] = None

  //-------------------------------------------------------------------------
  // The period fixtures, each built through the validated factory.
  private val P1_STUB: SchedulePeriod = sp(SchedulePeriod.of(JUL_03, JUL_17, JUL_04, JUL_17))
  private val P2_NORMAL: SchedulePeriod = sp(SchedulePeriod.of(JUL_17, AUG_16, JUL_17, AUG_17))
  private val P3_NORMAL: SchedulePeriod = sp(SchedulePeriod.of(AUG_16, SEP_17, AUG_17, SEP_17))
  private val P4_STUB: SchedulePeriod = sp(SchedulePeriod.of(SEP_17, SEP_30))
  private val P4_NORMAL: SchedulePeriod = sp(SchedulePeriod.of(SEP_17, OCT_17))
  private val P5_NORMAL: SchedulePeriod = sp(SchedulePeriod.of(OCT_17, NOV_17))
  private val P6_NORMAL: SchedulePeriod = sp(SchedulePeriod.of(NOV_17, DEC_17))

  private val P1_2: SchedulePeriod = sp(SchedulePeriod.of(JUL_03, AUG_16, JUL_04, AUG_17))
  private val P1_3: SchedulePeriod = sp(SchedulePeriod.of(JUL_03, SEP_17, JUL_04, SEP_17))
  private val P2_3: SchedulePeriod = sp(SchedulePeriod.of(JUL_17, SEP_17))
  private val P3_4: SchedulePeriod = sp(SchedulePeriod.of(AUG_16, OCT_17, AUG_17, OCT_17))
  private val P3_4STUB: SchedulePeriod = sp(SchedulePeriod.of(AUG_16, SEP_30, AUG_17, SEP_30))
  private val P4_5: SchedulePeriod = sp(SchedulePeriod.of(SEP_17, NOV_17))
  private val P5_6: SchedulePeriod = sp(SchedulePeriod.of(OCT_17, DEC_17))

  private val P2_4: SchedulePeriod = sp(SchedulePeriod.of(JUL_17, OCT_17))
  private val P4_6: SchedulePeriod = sp(SchedulePeriod.of(SEP_17, DEC_17))

  //-------------------------------------------------------------------------
  test("test_of_size0") {
    // The "at least one period" invariant is carried by `NonEmptyList` rather than by a runtime
    // check, so there is no rejection to assert: an ordinary list, an empty one and the `Option`
    // that `fromList` answers for an empty one are each refused by the compiler.
    assertDoesNotCompile("Schedule.of(Nil, Frequency.P1M, RollConventions.DAY_17)")
    assertDoesNotCompile(
      "Schedule.of(List.empty[SchedulePeriod], Frequency.P1M, RollConventions.DAY_17)")
    assertDoesNotCompile(
      "Schedule.of(NonEmptyList.fromList(Nil), Frequency.P1M, RollConventions.DAY_17)")

    // The type publishes no `apply` and no `copy`, so the factory is the published route into it.
    assertDoesNotCompile(
      "Schedule(NonEmptyList.one(P1_STUB), Frequency.P1M, RollConventions.DAY_17)")

    val smallest: Schedule =
      sched(Schedule.of(NonEmptyList.one(P1_STUB), Frequency.P1M, RollConventions.DAY_17))
    smallest.size shouldBe 1
    smallest.periods shouldBe NonEmptyList.one(P1_STUB)

    // The other invariant is the ordering the factory decides: the periods run from earliest to
    // latest, and one failure is reported for each ordering that does not hold. The unadjusted
    // pair and the adjusted pair are two statements about the same list, so a wholly reversed
    // pair reports both.
    val reversed: ResultNec[Schedule] =
      Schedule.of(NonEmptyList.of(P2_NORMAL, P1_STUB), Frequency.P1M, RollConventions.DAY_17)
    reversed should beFailureWith(FailureReason.INVALID)
    val reasons: List[Failure] = rejectionsOf(reversed)
    reasons should have size 2
    reasons.map(failure => failure.message).mkString("; ") should include(
      "the periods must run from earliest to latest")
    reasons.count(failure => failure.message.contains("the unadjusted end date")) shouldBe 1
    reasons.count(failure => failure.message.contains("the adjusted end date")) shouldBe 1

    // Three periods in reverse are two misplaced adjacencies, each reported for both date pairs,
    // so a caller correcting one is told about the other rather than meeting it on the next
    // attempt.
    val allReversed: ResultNec[Schedule] =
      Schedule.of(
        NonEmptyList.of(P3_NORMAL, P2_NORMAL, P1_STUB),
        Frequency.P1M,
        RollConventions.DAY_17)
    rejectionsOf(allReversed) should have size 4

    // The check does not require adjacency: a gap between one period and the next is accepted,
    // and so is the adjacency of a generated schedule, where each period begins on the day the
    // one before it ended.
    Schedule.of(
      NonEmptyList.of(P1_STUB, P3_NORMAL),
      Frequency.P1M,
      RollConventions.DAY_17) should beSuccess
    Schedule.of(
      NonEmptyList.of(P1_STUB, P2_NORMAL, P3_NORMAL),
      Frequency.P1M,
      RollConventions.DAY_17) should beSuccess
  }

  test("test_ofTerm") {
    val test: Schedule = Schedule.ofTerm(P1_STUB)
    val info: DayCount.ScheduleInfo = test
    test.size shouldBe 1
    test.isTerm shouldBe true
    test.isSinglePeriod shouldBe true
    test.periodicFrequency shouldBe Frequency.TERM
    info.frequency shouldBe Some(Frequency.TERM)
    test.rollConvention shouldBe RollConventions.NONE
    info.isEndOfMonthConvention shouldBe false
    test.periods shouldBe NonEmptyList.one(P1_STUB)
    test.periods.toList shouldBe List(P1_STUB)
    test.period(0) shouldBe P1_STUB
    test.adjustedStartDate shouldBe P1_STUB.startDate
    test.adjustedEndDate shouldBe P1_STUB.endDate
    info.startDate shouldBe Some(P1_STUB.startDate)
    info.endDate shouldBe Some(P1_STUB.endDate)
    test.unadjustedStartDate shouldBe P1_STUB.unadjustedStartDate
    test.unadjustedEndDate shouldBe P1_STUB.unadjustedEndDate
    test.firstPeriod shouldBe P1_STUB
    test.lastPeriod shouldBe P1_STUB
    test.initialStub shouldBe NoStub
    test.finalStub shouldBe NoStub
    test.stubs(true) shouldBe ((NoStub, NoStub))
    test.stubs(false) shouldBe ((NoStub, NoStub))
    test.regularPeriods shouldBe List(P1_STUB)
    assertThrows[IllegalArgumentException](test.period(1))
    test.unadjustedDates.toList shouldBe List(JUL_04, JUL_17)
  }

  test("test_size1_stub") {
    val test: Schedule =
      sched(Schedule.of(NonEmptyList.one(P1_STUB), Frequency.P1M, RollConventions.DAY_17))
    val info: DayCount.ScheduleInfo = test
    test.size shouldBe 1
    test.isTerm shouldBe false
    test.isSinglePeriod shouldBe true
    test.periodicFrequency shouldBe Frequency.P1M
    info.frequency shouldBe Some(Frequency.P1M)
    test.rollConvention shouldBe RollConventions.DAY_17
    info.isEndOfMonthConvention shouldBe false
    test.periods.toList shouldBe List(P1_STUB)
    test.period(0) shouldBe P1_STUB
    test.adjustedStartDate shouldBe P1_STUB.startDate
    test.adjustedEndDate shouldBe P1_STUB.endDate
    info.startDate shouldBe Some(P1_STUB.startDate)
    info.endDate shouldBe Some(P1_STUB.endDate)
    test.unadjustedStartDate shouldBe P1_STUB.unadjustedStartDate
    test.unadjustedEndDate shouldBe P1_STUB.unadjustedEndDate
    test.firstPeriod shouldBe P1_STUB
    test.lastPeriod shouldBe P1_STUB
    test.initialStub shouldBe Some(P1_STUB)
    test.finalStub shouldBe NoStub
    test.stubs(true) shouldBe ((NoStub, Some(P1_STUB)))
    test.stubs(false) shouldBe ((Some(P1_STUB), NoStub))
    test.regularPeriods shouldBe List.empty[SchedulePeriod]
    assertThrows[IllegalArgumentException](test.period(1))
    test.unadjustedDates.toList shouldBe List(JUL_04, JUL_17)
  }

  test("test_size1_noStub") {
    val test: Schedule =
      sched(Schedule.of(NonEmptyList.one(P2_NORMAL), Frequency.P1M, RollConventions.DAY_17))
    val info: DayCount.ScheduleInfo = test
    test.size shouldBe 1
    test.isTerm shouldBe false
    test.isSinglePeriod shouldBe true
    test.periodicFrequency shouldBe Frequency.P1M
    info.frequency shouldBe Some(Frequency.P1M)
    test.rollConvention shouldBe RollConventions.DAY_17
    info.isEndOfMonthConvention shouldBe false
    test.periods.toList shouldBe List(P2_NORMAL)
    test.period(0) shouldBe P2_NORMAL
    test.adjustedStartDate shouldBe P2_NORMAL.startDate
    test.adjustedEndDate shouldBe P2_NORMAL.endDate
    info.startDate shouldBe Some(P2_NORMAL.startDate)
    info.endDate shouldBe Some(P2_NORMAL.endDate)
    test.unadjustedStartDate shouldBe P2_NORMAL.unadjustedStartDate
    test.unadjustedEndDate shouldBe P2_NORMAL.unadjustedEndDate
    test.firstPeriod shouldBe P2_NORMAL
    test.lastPeriod shouldBe P2_NORMAL
    test.initialStub shouldBe NoStub
    test.finalStub shouldBe NoStub
    test.stubs(true) shouldBe ((NoStub, NoStub))
    test.stubs(false) shouldBe ((NoStub, NoStub))
    test.regularPeriods shouldBe List(P2_NORMAL)
    assertThrows[IllegalArgumentException](test.period(1))
    test.unadjustedDates.toList shouldBe List(JUL_17, AUG_17)
  }

  //-------------------------------------------------------------------------
  test("test_of_size2_initialStub") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P1_STUB, P2_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    val info: DayCount.ScheduleInfo = test
    test.size shouldBe 2
    test.isTerm shouldBe false
    test.isSinglePeriod shouldBe false
    test.periodicFrequency shouldBe Frequency.P1M
    info.frequency shouldBe Some(Frequency.P1M)
    test.rollConvention shouldBe RollConventions.DAY_17
    info.isEndOfMonthConvention shouldBe false
    test.periods.toList shouldBe List(P1_STUB, P2_NORMAL)
    test.period(0) shouldBe P1_STUB
    test.period(1) shouldBe P2_NORMAL
    test.adjustedStartDate shouldBe P1_STUB.startDate
    test.adjustedEndDate shouldBe P2_NORMAL.endDate
    info.startDate shouldBe Some(P1_STUB.startDate)
    info.endDate shouldBe Some(P2_NORMAL.endDate)
    test.unadjustedStartDate shouldBe P1_STUB.unadjustedStartDate
    test.unadjustedEndDate shouldBe P2_NORMAL.unadjustedEndDate
    test.firstPeriod shouldBe P1_STUB
    test.lastPeriod shouldBe P2_NORMAL
    test.initialStub shouldBe Some(P1_STUB)
    test.finalStub shouldBe NoStub
    test.stubs(true) shouldBe ((Some(P1_STUB), NoStub))
    test.stubs(false) shouldBe ((Some(P1_STUB), NoStub))
    test.regularPeriods shouldBe List(P2_NORMAL)
    assertThrows[IllegalArgumentException](test.period(2))
    test.unadjustedDates.toList shouldBe List(JUL_04, JUL_17, AUG_17)
  }

  test("test_of_size2_noStub") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    val info: DayCount.ScheduleInfo = test
    test.size shouldBe 2
    test.isTerm shouldBe false
    test.isSinglePeriod shouldBe false
    test.periodicFrequency shouldBe Frequency.P1M
    info.frequency shouldBe Some(Frequency.P1M)
    test.rollConvention shouldBe RollConventions.DAY_17
    info.isEndOfMonthConvention shouldBe false
    test.periods.toList shouldBe List(P2_NORMAL, P3_NORMAL)
    test.period(0) shouldBe P2_NORMAL
    test.period(1) shouldBe P3_NORMAL
    test.adjustedStartDate shouldBe P2_NORMAL.startDate
    test.adjustedEndDate shouldBe P3_NORMAL.endDate
    info.startDate shouldBe Some(P2_NORMAL.startDate)
    info.endDate shouldBe Some(P3_NORMAL.endDate)
    test.unadjustedStartDate shouldBe P2_NORMAL.unadjustedStartDate
    test.unadjustedEndDate shouldBe P3_NORMAL.unadjustedEndDate
    test.firstPeriod shouldBe P2_NORMAL
    test.lastPeriod shouldBe P3_NORMAL
    test.initialStub shouldBe NoStub
    test.finalStub shouldBe NoStub
    test.stubs(true) shouldBe ((NoStub, NoStub))
    test.stubs(false) shouldBe ((NoStub, NoStub))
    test.regularPeriods shouldBe List(P2_NORMAL, P3_NORMAL)
    assertThrows[IllegalArgumentException](test.period(2))
    test.unadjustedDates.toList shouldBe List(JUL_17, AUG_17, SEP_17)
  }

  test("test_of_size2_finalStub") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P3_NORMAL, P4_STUB),
        Frequency.P1M,
        RollConventions.DAY_17))
    val info: DayCount.ScheduleInfo = test
    test.size shouldBe 2
    test.isTerm shouldBe false
    test.isSinglePeriod shouldBe false
    test.periodicFrequency shouldBe Frequency.P1M
    info.frequency shouldBe Some(Frequency.P1M)
    test.rollConvention shouldBe RollConventions.DAY_17
    info.isEndOfMonthConvention shouldBe false
    test.periods.toList shouldBe List(P3_NORMAL, P4_STUB)
    test.period(0) shouldBe P3_NORMAL
    test.period(1) shouldBe P4_STUB
    test.adjustedStartDate shouldBe P3_NORMAL.startDate
    test.adjustedEndDate shouldBe P4_STUB.endDate
    info.startDate shouldBe Some(P3_NORMAL.startDate)
    info.endDate shouldBe Some(P4_STUB.endDate)
    test.unadjustedStartDate shouldBe P3_NORMAL.unadjustedStartDate
    test.unadjustedEndDate shouldBe P4_STUB.unadjustedEndDate
    test.firstPeriod shouldBe P3_NORMAL
    test.lastPeriod shouldBe P4_STUB
    test.initialStub shouldBe NoStub
    test.finalStub shouldBe Some(P4_STUB)
    test.stubs(true) shouldBe ((NoStub, Some(P4_STUB)))
    test.stubs(false) shouldBe ((NoStub, Some(P4_STUB)))
    test.regularPeriods shouldBe List(P3_NORMAL)
    assertThrows[IllegalArgumentException](test.period(2))
    test.unadjustedDates.toList shouldBe List(AUG_17, SEP_17, SEP_30)
  }

  test("test_of_size3_initialStub") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P1_STUB, P2_NORMAL, P3_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    val info: DayCount.ScheduleInfo = test
    test.size shouldBe 3
    test.isTerm shouldBe false
    test.isSinglePeriod shouldBe false
    test.periodicFrequency shouldBe Frequency.P1M
    info.frequency shouldBe Some(Frequency.P1M)
    test.rollConvention shouldBe RollConventions.DAY_17
    info.isEndOfMonthConvention shouldBe false
    test.periods.toList shouldBe List(P1_STUB, P2_NORMAL, P3_NORMAL)
    test.period(0) shouldBe P1_STUB
    test.period(1) shouldBe P2_NORMAL
    test.period(2) shouldBe P3_NORMAL
    test.adjustedStartDate shouldBe P1_STUB.startDate
    test.adjustedEndDate shouldBe P3_NORMAL.endDate
    info.startDate shouldBe Some(P1_STUB.startDate)
    info.endDate shouldBe Some(P3_NORMAL.endDate)
    test.unadjustedStartDate shouldBe P1_STUB.unadjustedStartDate
    test.unadjustedEndDate shouldBe P3_NORMAL.unadjustedEndDate
    test.firstPeriod shouldBe P1_STUB
    test.lastPeriod shouldBe P3_NORMAL
    test.initialStub shouldBe Some(P1_STUB)
    test.finalStub shouldBe NoStub
    test.regularPeriods shouldBe List(P2_NORMAL, P3_NORMAL)
    assertThrows[IllegalArgumentException](test.period(3))
    test.unadjustedDates.toList shouldBe List(JUL_04, JUL_17, AUG_17, SEP_17)
  }

  test("test_of_size4_bothStubs") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P1_STUB, P2_NORMAL, P3_NORMAL, P4_STUB),
        Frequency.P1M,
        RollConventions.DAY_17))
    val info: DayCount.ScheduleInfo = test
    test.size shouldBe 4
    test.isTerm shouldBe false
    test.isSinglePeriod shouldBe false
    test.periodicFrequency shouldBe Frequency.P1M
    info.frequency shouldBe Some(Frequency.P1M)
    test.rollConvention shouldBe RollConventions.DAY_17
    info.isEndOfMonthConvention shouldBe false
    test.periods.toList shouldBe List(P1_STUB, P2_NORMAL, P3_NORMAL, P4_STUB)
    test.period(0) shouldBe P1_STUB
    test.period(1) shouldBe P2_NORMAL
    test.period(2) shouldBe P3_NORMAL
    test.period(3) shouldBe P4_STUB
    test.adjustedStartDate shouldBe P1_STUB.startDate
    test.adjustedEndDate shouldBe P4_STUB.endDate
    info.startDate shouldBe Some(P1_STUB.startDate)
    info.endDate shouldBe Some(P4_STUB.endDate)
    test.unadjustedStartDate shouldBe P1_STUB.unadjustedStartDate
    test.unadjustedEndDate shouldBe P4_STUB.unadjustedEndDate
    test.firstPeriod shouldBe P1_STUB
    test.lastPeriod shouldBe P4_STUB
    test.initialStub shouldBe Some(P1_STUB)
    test.finalStub shouldBe Some(P4_STUB)
    test.regularPeriods shouldBe List(P2_NORMAL, P3_NORMAL)
    assertThrows[IllegalArgumentException](test.period(4))
    test.unadjustedDates.toList shouldBe List(JUL_04, JUL_17, AUG_17, SEP_17, SEP_30)
  }

  //-------------------------------------------------------------------------
  test("test_isEndOfMonthConvention_eom") {
    // The periods are not an end-of-month schedule; the case only needs the roll convention.
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL),
        Frequency.P1M,
        RollConventions.EOM))
    val info: DayCount.ScheduleInfo = test
    info.isEndOfMonthConvention shouldBe true
    // `isEndOfMonthConvention` is derived from the roll convention, which is read here too.
    test.rollConvention shouldBe RollConventions.EOM
  }

  //-------------------------------------------------------------------------
  test("test_getPeriodEndDate") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    val info: DayCount.ScheduleInfo = test
    info.periodEndDate(P2_NORMAL.startDate) shouldBe Some(P2_NORMAL.endDate)
    info.periodEndDate(P2_NORMAL.startDate.plusDays(1)) shouldBe Some(P2_NORMAL.endDate)
    info.periodEndDate(P3_NORMAL.startDate) shouldBe Some(P3_NORMAL.endDate)
    info.periodEndDate(P3_NORMAL.startDate.plusDays(1)) shouldBe Some(P3_NORMAL.endDate)

    // A date lying in none of the periods describes the data rather than a broken call, so the
    // accessor answers `None` rather than raising: before the first period, and - since a period
    // excludes its end date - on the schedule's own end date.
    info.periodEndDate(P2_NORMAL.startDate.minusDays(1)) shouldBe None
    info.periodEndDate(P3_NORMAL.endDate) shouldBe None
  }

  test("test_getPeriodEndDate_agreesWithLinearScan") {
    // `periodEndDate` locates the one period that can contain the date by halving the ascending
    // adjusted start dates rather than by walking the periods, so every shape of schedule is
    // checked against the walk it replaces, date for date: the adjacent periods of a generated
    // schedule, a schedule with a gap between every period and the next, the mixed fixtures of
    // this spec, a single-period schedule and a term schedule. The two long fixtures are long
    // enough that a lookup over them crosses several halvings.
    val adjacent: Schedule =
      sched(Schedule.of(monthlyChain(400L, 1L), Frequency.P1M, RollConventions.DAY_15))
    val gapped: Schedule =
      sched(Schedule.of(monthlyChain(250L, 2L), Frequency.P1M, RollConventions.DAY_15))
    val mixed: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P1_STUB, P2_NORMAL, P3_NORMAL, P4_NORMAL, P5_NORMAL, P6_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    val singlePeriod: Schedule =
      sched(Schedule.of(NonEmptyList.one(P2_NORMAL), Frequency.P1M, RollConventions.DAY_17))
    val term: Schedule = Schedule.ofTerm(P1_3)
    adjacent.size shouldBe 400
    gapped.size shouldBe 250
    mixed.size shouldBe 6
    singlePeriod.isSinglePeriod shouldBe true
    term.isTerm shouldBe true

    List(adjacent, gapped, mixed, singlePeriod, term).foreach { schedule =>
      val probes: List[LocalDate] = lookupProbes(schedule)
      probes.size should be > schedule.size
      val disagreements: List[LocalDate] =
        probes.filter(date => schedule.periodEndDate(date) != linearPeriodEndDate(schedule, date))
      withClue(
        s"over a schedule of ${schedule.size} periods the lookup disagreed with the walk at " +
          s"${disagreements.take(5)} of ${probes.size} probe dates: ") {
        disagreements shouldBe empty
      }
    }

    // The answers themselves, stated rather than compared with a second implementation. Over
    // adjacent periods a date at a period's start date, inside it or on the day before its end
    // date answers that period's end date, and the end date itself belongs to the period that
    // follows - or to none, for the last period of the schedule.
    (0 until adjacent.size).foreach { index =>
      val period: SchedulePeriod = adjacent.period(index)
      adjacent.periodEndDate(period.startDate) shouldBe Some(period.endDate)
      adjacent.periodEndDate(period.startDate.plusDays(7L)) shouldBe Some(period.endDate)
      adjacent.periodEndDate(period.endDate.minusDays(1L)) shouldBe Some(period.endDate)
      adjacent.periodEndDate(period.endDate) shouldBe
        (if (index == adjacent.size - 1) None else Some(adjacent.period(index + 1).endDate))
    }

    // Over a schedule with gaps the search still finds the containing period, and the date a
    // period ends on - which begins the gap following it - lies in no period at all, which is the
    // `contains` test after the search answering rather than the search itself.
    (0 until gapped.size).foreach { index =>
      val period: SchedulePeriod = gapped.period(index)
      gapped.periodEndDate(period.startDate) shouldBe Some(period.endDate)
      gapped.periodEndDate(period.startDate.plusDays(7L)) shouldBe Some(period.endDate)
      gapped.periodEndDate(period.endDate) shouldBe None
      gapped.periodEndDate(period.endDate.plusDays(7L)) shouldBe None
    }

    // A date outside every period, read through the schedule information interface as a day count
    // reads it: before the first period, on the schedule's own end date and long after it.
    val info: DayCount.ScheduleInfo = adjacent
    info.periodEndDate(adjacent.adjustedStartDate) shouldBe Some(adjacent.period(0).endDate)
    info.periodEndDate(adjacent.adjustedStartDate.minusDays(1L)) shouldBe None
    info.periodEndDate(adjacent.adjustedEndDate) shouldBe None
    info.periodEndDate(adjacent.adjustedEndDate.plusYears(5L)) shouldBe None
    gapped.periodEndDate(gapped.adjustedStartDate.minusYears(5L)) shouldBe None
    gapped.periodEndDate(gapped.adjustedEndDate) shouldBe None

    // One period, and one 'Term' period, are the shortest schedules the search runs over.
    singlePeriod.periodEndDate(P2_NORMAL.startDate) shouldBe Some(P2_NORMAL.endDate)
    singlePeriod.periodEndDate(P2_NORMAL.startDate.plusDays(1L)) shouldBe Some(P2_NORMAL.endDate)
    singlePeriod.periodEndDate(P2_NORMAL.startDate.minusDays(1L)) shouldBe None
    singlePeriod.periodEndDate(P2_NORMAL.endDate) shouldBe None
    term.periodEndDate(P1_3.startDate) shouldBe Some(P1_3.endDate)
    term.periodEndDate(P1_3.endDate.minusDays(1L)) shouldBe Some(P1_3.endDate)
    term.periodEndDate(P1_3.startDate.minusDays(1L)) shouldBe None
    term.periodEndDate(P1_3.endDate) shouldBe None
  }

  test("test_period_indexedAccess") {
    // Positional access agrees with `periods`: every index, read ascending, descending and
    // shuffled, and the first, last and regular periods and the unadjusted dates, against
    // `periods.toList`.
    val all: List[SchedulePeriod] =
      List(P1_STUB, P2_NORMAL, P3_NORMAL, P4_NORMAL, P5_NORMAL, P6_NORMAL)
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P1_STUB, P2_NORMAL, P3_NORMAL, P4_NORMAL, P5_NORMAL, P6_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))

    test.periods.toList shouldBe all
    test.size shouldBe all.size
    (0 until test.size).map(index => test.period(index)).toList shouldBe all
    (test.size - 1 to 0 by -1).map(index => test.period(index)).toList shouldBe all.reverse
    List(3, 0, 5, 1, 4, 2).map(index => test.period(index)) shouldBe
      List(3, 0, 5, 1, 4, 2).map(index => all(index))
    test.firstPeriod shouldBe all.head
    test.lastPeriod shouldBe all.last
    test.period(0) shouldBe test.firstPeriod
    test.period(test.size - 1) shouldBe test.lastPeriod
    test.regularPeriods shouldBe all.tail
    test.unadjustedDates.toList shouldBe
      all.head.unadjustedStartDate :: all.map(_.unadjustedEndDate)

    // The index bounds are a caller contract, refused rather than reported, and a valid index
    // still answers after a refused one.
    assertThrows[IllegalArgumentException](test.period(-1))
    assertThrows[IllegalArgumentException](test.period(all.size))
    test.period(2) shouldBe all(2)

    val single: Schedule = Schedule.ofTerm(P1_STUB)
    single.size shouldBe 1
    single.period(0) shouldBe P1_STUB
    single.firstPeriod shouldBe P1_STUB
    single.lastPeriod shouldBe P1_STUB
    single.regularPeriods shouldBe List(P1_STUB)
  }


  //-------------------------------------------------------------------------
  test("test_mergeToTerm") {
    // `mergeToTerm` is total - the span of a schedule whose periods run from earliest to latest
    // is always a valid period - so the result is a schedule rather than an `Either`.
    val testNormal: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P1_STUB, P2_NORMAL, P3_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    testNormal.mergeToTerm shouldBe Schedule.ofTerm(P1_3)
    testNormal.mergeToTerm.mergeToTerm shouldBe Schedule.ofTerm(P1_3)
  }

  test("test_mergeToTerm_size1_stub") {
    val test: Schedule =
      sched(Schedule.of(NonEmptyList.one(P1_STUB), Frequency.P1M, RollConventions.DAY_17))
    test.mergeToTerm shouldBe Schedule.ofTerm(P1_STUB)
  }

  //-------------------------------------------------------------------------
  test("test_merge_group2_within2_initialStub") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P1_STUB, P2_NORMAL, P3_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    val expected: Schedule =
      sched(Schedule.of(NonEmptyList.of(P1_STUB, P2_3), Frequency.P2M, RollConventions.DAY_17))
    test.mergeRegular(2, true) shouldBe Right(expected)
    test.mergeRegular(2, false) shouldBe Right(expected)
    test.merge(2, P2_NORMAL.unadjustedStartDate, P3_NORMAL.unadjustedEndDate) shouldBe
      Right(expected)
    test.merge(2, P2_NORMAL.startDate, P3_NORMAL.endDate) shouldBe Right(expected)
  }

  test("test_merge_group2_within2_noStub") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    val expected: Schedule =
      sched(Schedule.of(NonEmptyList.one(P2_3), Frequency.P2M, RollConventions.DAY_17))
    test.mergeRegular(2, true) shouldBe Right(expected)
    test.mergeRegular(2, false) shouldBe Right(expected)
    test.merge(2, P2_NORMAL.unadjustedStartDate, P3_NORMAL.unadjustedEndDate) shouldBe
      Right(expected)
    test.merge(2, P2_NORMAL.startDate, P3_NORMAL.endDate) shouldBe Right(expected)
  }

  test("test_merge_group2_within2_finalStub") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL, P4_STUB),
        Frequency.P1M,
        RollConventions.DAY_17))
    val expected: Schedule =
      sched(Schedule.of(NonEmptyList.of(P2_3, P4_STUB), Frequency.P2M, RollConventions.DAY_17))
    test.mergeRegular(2, true) shouldBe Right(expected)
    test.mergeRegular(2, false) shouldBe Right(expected)
    test.merge(2, P2_NORMAL.unadjustedStartDate, P3_NORMAL.unadjustedEndDate) shouldBe
      Right(expected)
    test.merge(2, P2_NORMAL.startDate, P3_NORMAL.endDate) shouldBe Right(expected)
  }

  test("test_merge_group2_within3_forwards") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL, P4_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    val expected: Schedule =
      sched(Schedule.of(NonEmptyList.of(P2_3, P4_NORMAL), Frequency.P2M, RollConventions.DAY_17))
    test.mergeRegular(2, true) shouldBe Right(expected)
    test.merge(2, P2_NORMAL.unadjustedStartDate, P3_NORMAL.unadjustedEndDate) shouldBe
      Right(expected)
    test.merge(2, P2_NORMAL.startDate, P3_NORMAL.endDate) shouldBe Right(expected)
  }

  test("test_merge_group2_within3_backwards") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL, P4_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    val expected: Schedule =
      sched(Schedule.of(NonEmptyList.of(P2_NORMAL, P3_4), Frequency.P2M, RollConventions.DAY_17))
    test.mergeRegular(2, false) shouldBe Right(expected)
    test.merge(2, P3_NORMAL.unadjustedStartDate, P4_NORMAL.unadjustedEndDate) shouldBe
      Right(expected)
    test.merge(2, P3_NORMAL.startDate, P4_NORMAL.endDate) shouldBe Right(expected)
  }

  test("test_merge_group2_within5_forwards") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL, P4_NORMAL, P5_NORMAL, P6_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    val expected: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_3, P4_5, P6_NORMAL),
        Frequency.P2M,
        RollConventions.DAY_17))
    test.mergeRegular(2, true) shouldBe Right(expected)
    test.merge(2, P2_NORMAL.unadjustedStartDate, P5_NORMAL.unadjustedEndDate) shouldBe
      Right(expected)
    test.merge(2, P2_NORMAL.startDate, P5_NORMAL.endDate) shouldBe Right(expected)
  }

  test("test_merge_group2_within5_backwards") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL, P4_NORMAL, P5_NORMAL, P6_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    val expected: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_4, P5_6),
        Frequency.P2M,
        RollConventions.DAY_17))
    test.mergeRegular(2, false) shouldBe Right(expected)
    test.merge(2, P3_NORMAL.unadjustedStartDate, P6_NORMAL.unadjustedEndDate) shouldBe
      Right(expected)
    test.merge(2, P3_NORMAL.startDate, P6_NORMAL.endDate) shouldBe Right(expected)
  }

  test("test_merge_group2_within6_includeInitialStub") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P1_STUB, P2_NORMAL, P3_NORMAL, P4_NORMAL, P5_NORMAL, P6_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    val expected: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P1_2, P3_4, P5_6),
        Frequency.P2M,
        RollConventions.DAY_17))
    test.merge(2, P3_NORMAL.unadjustedStartDate, P6_NORMAL.unadjustedEndDate) shouldBe
      Right(expected)
    test.merge(2, P3_NORMAL.startDate, P6_NORMAL.endDate) shouldBe Right(expected)
  }

  test("test_merge_group2_within6_includeFinalStub") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P1_STUB, P2_NORMAL, P3_NORMAL, P4_STUB),
        Frequency.P1M,
        RollConventions.DAY_17))
    val expected: Schedule =
      sched(Schedule.of(NonEmptyList.of(P1_2, P3_4STUB), Frequency.P2M, RollConventions.DAY_17))
    test.merge(2, P1_STUB.unadjustedStartDate, P2_NORMAL.unadjustedEndDate) shouldBe
      Right(expected)
    test.merge(2, P1_STUB.startDate, P2_NORMAL.endDate) shouldBe Right(expected)
  }

  test("test_merge_group3_within5_forwards") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL, P4_NORMAL, P5_NORMAL, P6_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    val expected: Schedule =
      sched(Schedule.of(NonEmptyList.of(P2_4, P5_6), Frequency.P3M, RollConventions.DAY_17))
    test.mergeRegular(3, true) shouldBe Right(expected)
    test.merge(3, P2_NORMAL.unadjustedStartDate, P4_NORMAL.unadjustedEndDate) shouldBe
      Right(expected)
    test.merge(3, P2_NORMAL.startDate, P4_NORMAL.endDate) shouldBe Right(expected)
  }

  test("test_merge_group3_within5_backwards") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL, P4_NORMAL, P5_NORMAL, P6_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    val expected: Schedule =
      sched(Schedule.of(NonEmptyList.of(P2_3, P4_6), Frequency.P3M, RollConventions.DAY_17))
    test.mergeRegular(3, false) shouldBe Right(expected)
    test.merge(3, P4_NORMAL.unadjustedStartDate, P6_NORMAL.unadjustedEndDate) shouldBe
      Right(expected)
    test.merge(3, P4_NORMAL.startDate, P6_NORMAL.endDate) shouldBe Right(expected)
  }


  //-------------------------------------------------------------------------
  test("test_merge_termNoChange") {
    val test: Schedule = Schedule.ofTerm(P1_STUB)
    test.mergeRegular(2, true) shouldBe Right(test)
    test.mergeRegular(2, false) shouldBe Right(test)
    test.merge(2, P1_STUB.unadjustedStartDate, P1_STUB.unadjustedEndDate) shouldBe Right(test)
    test.merge(2, P1_STUB.startDate, P1_STUB.endDate) shouldBe Right(test)
  }

  test("test_merge_size1_stub") {
    val test: Schedule =
      sched(Schedule.of(NonEmptyList.one(P1_STUB), Frequency.P1M, RollConventions.DAY_17))
    test.mergeRegular(2, true) shouldBe Right(test)
    test.mergeRegular(2, false) shouldBe Right(test)
    test.merge(2, P1_STUB.unadjustedStartDate, P1_STUB.unadjustedEndDate) shouldBe Right(test)
    test.merge(2, P1_STUB.startDate, P1_STUB.endDate) shouldBe Right(test)
  }

  test("test_merge_groupSizeOneNoChange") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL, P4_NORMAL, P5_NORMAL, P6_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    test.mergeRegular(1, true) shouldBe Right(test)
    test.mergeRegular(1, false) shouldBe Right(test)
    test.merge(1, P2_NORMAL.unadjustedStartDate, P6_NORMAL.unadjustedEndDate) shouldBe Right(test)
    test.merge(1, P2_NORMAL.startDate, P6_NORMAL.endDate) shouldBe Right(test)
  }

  test("test_merge_groupSizeInvalid") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL, P4_NORMAL, P5_NORMAL, P6_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))

    // A group size of zero or less is reported as a failure value by both methods rather than
    // raised, and the failure names the argument at fault.
    val zeroForwards: FailureOr[Schedule] = test.mergeRegular(0, true)
    val zeroBackwards: FailureOr[Schedule] = test.mergeRegular(0, false)
    val negativeForwards: FailureOr[Schedule] = test.mergeRegular(-1, true)
    val negativeBackwards: FailureOr[Schedule] = test.mergeRegular(-1, false)
    val zeroMerge: FailureOr[Schedule] =
      test.merge(0, P2_NORMAL.unadjustedStartDate, P6_NORMAL.unadjustedEndDate)
    val negativeMerge: FailureOr[Schedule] =
      test.merge(-1, P2_NORMAL.unadjustedStartDate, P6_NORMAL.unadjustedEndDate)

    val refusals: List[FailureOr[Schedule]] =
      List(
        zeroForwards,
        zeroBackwards,
        negativeForwards,
        negativeBackwards,
        zeroMerge,
        negativeMerge)
    refusals.foreach { refusal =>
      refusal should beFailureWith(FailureReason.INVALID)
      messageOf(refusal) should include("groupSize")
    }
  }

  test("test_merge_badDate") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL, P4_NORMAL, P5_NORMAL, P6_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))

    val badStart: FailureOr[Schedule] = test.merge(2, JUL_03, AUG_17)
    val badEnd: FailureOr[Schedule] = test.merge(2, JUL_17, SEP_30)
    badStart should beFailureWith(FailureReason.INVALID)
    badEnd should beFailureWith(FailureReason.INVALID)
    messageOf(badStart) should startWith(
      s"Unable to merge schedule, firstRegularStartDate $JUL_03 " +
        "does not match any date in the underlying schedule")
    messageOf(badEnd) should startWith(
      s"Unable to merge schedule, lastRegularEndDate $SEP_30 " +
        "does not match any date in the underlying schedule")
  }

  test("test_merge_badGroupSize") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL, P4_NORMAL, P5_NORMAL, P6_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))

    val expectedMessage: String =
      s"Unable to merge schedule, firstRegularStartDate ${P2_NORMAL.unadjustedStartDate}" +
        s" and lastRegularEndDate ${P6_NORMAL.unadjustedEndDate}" +
        " cannot be used to create regular periods of frequency 'P2M'"
    val result: FailureOr[Schedule] =
      test.merge(2, P2_NORMAL.unadjustedStartDate, P6_NORMAL.unadjustedEndDate)
    result should beFailureWith(FailureReason.INVALID)
    messageOf(result) shouldBe expectedMessage
  }

  test("test_merge_groupSizeTooLargeToMultiply") {
    // The group size is multiplied into the frequency's period by `Period.multipliedBy`, which
    // multiplies each component exactly and raises `ArithmeticException` where the product does
    // not fit; both merges report that overflow as a failure value instead. The frequency is
    // quarterly deliberately: a one-month period multiplied by the largest group size an `Int`
    // can hold still fits, while a three-month one does not, so the overflow is a property of the
    // frequency and the group size together and the case states both.
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL, P4_NORMAL, P5_NORMAL, P6_NORMAL),
        Frequency.P3M,
        RollConventions.DAY_17))

    val overflowing: Int = Int.MaxValue
    val fromMerge: FailureOr[Schedule] =
      test.merge(overflowing, P2_NORMAL.unadjustedStartDate, P6_NORMAL.unadjustedEndDate)
    val fromMergeRegularForwards: FailureOr[Schedule] = test.mergeRegular(overflowing, true)
    val fromMergeRegularBackwards: FailureOr[Schedule] = test.mergeRegular(overflowing, false)

    List(fromMerge, fromMergeRegularForwards, fromMergeRegularBackwards).foreach { refusal =>
      refusal should beFailureWith(FailureReason.INVALID)
      messageOf(refusal) shouldBe
        s"Unable to merge schedule, 'groupSize' of $overflowing is too large to multiply the " +
          "frequency 'P3M' by"
    }

    // `merge` multiplies only after it has matched its two dates, so a date matching nothing in
    // the schedule is reported in place of the overflow.
    messageOf(test.merge(overflowing, JUL_03, P6_NORMAL.unadjustedEndDate)) should startWith(
      s"Unable to merge schedule, firstRegularStartDate $JUL_03 " +
        "does not match any date in the underlying schedule")

    // A group size that multiplies without overflowing but names no frequency this library
    // expresses is reported by the factory that builds the merged frequency, so the two failures
    // of the multiplication are distinct and both are values.
    val overlong: FailureOr[Schedule] = test.mergeRegular(100000, true)
    overlong should beFailureWith(FailureReason.INVALID)
    messageOf(overlong) shouldBe "Period must not exceed 1000 years"

    // The two early returns answer before anything is multiplied, which keeps a group size of
    // one and a single-period schedule total whatever the group size would do to the frequency.
    test.mergeRegular(1, true) shouldBe Right(test)
    test.merge(1, P2_NORMAL.unadjustedStartDate, P6_NORMAL.unadjustedEndDate) shouldBe Right(test)
    val single: Schedule = Schedule.ofTerm(P1_STUB)
    single.mergeRegular(overflowing, true) shouldBe Right(single)
    single.mergeRegular(overflowing, false) shouldBe Right(single)
    single.merge(
      overflowing,
      P1_STUB.unadjustedStartDate,
      P1_STUB.unadjustedEndDate) shouldBe Right(single)
  }

  //-------------------------------------------------------------------------
  test("test_toAdjusted") {
    val period1: SchedulePeriod = sp(SchedulePeriod.of(JUN_15, SEP_17))
    val period2: SchedulePeriod = sp(SchedulePeriod.of(SEP_17, SEP_30))
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(period1, period2),
        Frequency.P3M,
        RollConventions.DAY_17))

    // Nothing moved, so the schedule itself is the answer.
    test.toAdjusted(DateAdjuster(adjusted => adjusted)) shouldBe Right(test)

    // One date moved, so the first period is rebuilt with its unadjusted dates retained.
    val expected: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(sp(SchedulePeriod.of(JUN_16, SEP_17, JUN_15, SEP_17)), period2),
        Frequency.P3M,
        RollConventions.DAY_17))
    test.toAdjusted(DateAdjuster(adjusted => if (adjusted == JUN_15) JUN_16 else adjusted)) shouldBe
      Right(expected)

    // Where adjustment drives a period's dates out of order, `SchedulePeriod.of` rejects the
    // rebuilt period and `toAdjusted` reports that rejection as a failure value. The adjuster
    // below moves the end of the last period before its start, which the rule that rescues an
    // empty first or last period does not cover, since the two adjusted dates differ rather than
    // coincide.
    val outOfOrder: DateAdjuster =
      DateAdjuster(adjusted => if (adjusted == SEP_30) JUN_15 else adjusted)
    val rejected: FailureOr[Schedule] = test.toAdjusted(outOfOrder)
    rejected should beFailureWith(FailureReason.INVALID)
    messageOf(rejected) should include("Invalid order")
  }

  test("test_toAdjustedSmallFirstLast") {
    val period1: SchedulePeriod = sp(SchedulePeriod.of(JUL_03, JUL_04, JUL_03, JUL_04))
    val period2: SchedulePeriod = sp(SchedulePeriod.of(JUL_04, AUG_16))
    val period3: SchedulePeriod = sp(SchedulePeriod.of(AUG_16, AUG_17, AUG_16, AUG_17))
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(period1, period2, period3),
        Frequency.P1M,
        RollConventions.DAY_4))

    // The first period's start and the last period's end are kept where adjustment would
    // collapse them onto the neighbouring date, so the schedule is unchanged.
    val adjuster: DateAdjuster = DateAdjuster { adjusted =>
      if (adjusted == JUL_03) JUL_04 else if (adjusted == AUG_17) AUG_16 else adjusted
    }
    val expected: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(
          sp(SchedulePeriod.of(JUL_03, JUL_04, JUL_03, JUL_04)),
          period2,
          sp(SchedulePeriod.of(AUG_16, AUG_17, AUG_16, AUG_17))),
        Frequency.P1M,
        RollConventions.DAY_4))
    test.toAdjusted(adjuster) shouldBe Right(expected)
  }

  test("test_toUnadjusted") {
    val a: SchedulePeriod = sp(SchedulePeriod.of(JUL_17, OCT_17, JUL_16, OCT_15))
    val b: SchedulePeriod = sp(SchedulePeriod.of(JUL_16, OCT_15, JUL_16, OCT_15))
    val test: Schedule =
      sched(Schedule.of(NonEmptyList.one(a), Frequency.P1M, RollConventions.DAY_17)).toUnadjusted
    val expected: Schedule =
      sched(Schedule.of(NonEmptyList.one(b), Frequency.P1M, RollConventions.DAY_17))
    test shouldBe expected
  }


  //-------------------------------------------------------------------------
  test("coverage_equals") {
    val a: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL, P4_NORMAL, P5_NORMAL, P6_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    val b: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    val c: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL, P4_NORMAL, P5_NORMAL, P6_NORMAL),
        Frequency.P3M,
        RollConventions.DAY_17))
    val d: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL, P4_NORMAL, P5_NORMAL, P6_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_1))
    (a == a) shouldBe true
    (a == b) shouldBe false
    (a == c) shouldBe false
    (a == d) shouldBe false

    // The companion publishes one equality-bearing instance, a `Hash`, which agrees with
    // `equals` and `hashCode` on the same four values: a difference in any of the three
    // properties is a difference, and two schedules built the same way are equal.
    val again: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL, P4_NORMAL, P5_NORMAL, P6_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    Eq[Schedule].eqv(a, a) shouldBe true
    Eq[Schedule].eqv(a, again) shouldBe true
    Eq[Schedule].eqv(a, b) shouldBe false
    Eq[Schedule].eqv(a, c) shouldBe false
    Eq[Schedule].eqv(a, d) shouldBe false
    Hash[Schedule].hash(a) shouldBe a.hashCode
    Hash[Schedule].hash(a) shouldBe Hash[Schedule].hash(again)
  }

  //-------------------------------------------------------------------------
  test("coverage_builder") {
    // The type is a `sealed abstract case class` with a private constructor, so it publishes no
    // `apply` and no `copy`: construction goes through [[Schedule.of]] and every property reads
    // back what was supplied, while neither a builder call nor a `copy` call compiles.
    val test: Schedule =
      sched(Schedule.of(NonEmptyList.one(P1_STUB), Frequency.P1M, RollConventions.DAY_17))
    test.periods shouldBe NonEmptyList.one(P1_STUB)
    test.periodicFrequency shouldBe Frequency.P1M
    test.rollConvention shouldBe RollConventions.DAY_17
    test.size shouldBe 1
    assertDoesNotCompile("Schedule.builder()")
    assertDoesNotCompile("Schedule.ofTerm(P1_STUB).copy(rollConvention = RollConventions.EOM)")
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P1_STUB, P2_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    val other: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL),
        Frequency.P3M,
        RollConventions.EOM))
    test.periods.toList shouldBe List(P1_STUB, P2_NORMAL)
    test.periodicFrequency shouldBe Frequency.P1M
    test.rollConvention shouldBe RollConventions.DAY_17
    other.periods.toList shouldBe List(P2_NORMAL, P3_NORMAL)
    other.periodicFrequency shouldBe Frequency.P3M
    other.rollConvention shouldBe RollConventions.EOM
    Eq[Schedule].eqv(test, test) shouldBe true
    Eq[Schedule].eqv(test, other) shouldBe false
    Hash[Schedule].hash(test) shouldBe test.hashCode
    Hash[Schedule].hash(other) shouldBe other.hashCode
    Show[Schedule].show(test) shouldBe test.toString
    Show[Schedule].show(other) shouldBe other.toString
    test.toString shouldBe s"Schedule(P1M, Day17, [$P1_STUB, $P2_NORMAL])"
  }

  test("test_serialization") {
    // The circe round trip through the semiauto product codec: the keys are `periods`,
    // `frequency` - the document's name for [[Schedule.periodicFrequency]] - and
    // `rollConvention`, in that order, the periods are an array in schedule order, and a payload
    // that violates an invariant of the type is rejected by the validating decoder rather than
    // carried into a value.
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P1_STUB, P2_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17))
    val encoded: Json = test.asJson
    encoded.noSpaces shouldBe
      """{"periods":[""" +
        """{"startDate":"2014-07-03","endDate":"2014-07-17",""" +
        """"unadjustedStartDate":"2014-07-04","unadjustedEndDate":"2014-07-17"},""" +
        """{"startDate":"2014-07-17","endDate":"2014-08-16",""" +
        """"unadjustedStartDate":"2014-07-17","unadjustedEndDate":"2014-08-17"}],""" +
        """"frequency":"P1M","rollConvention":"Day17"}"""
    decode[Schedule](encoded.noSpaces) shouldBe Right(test)
    encoded.asObject.map(json => json.keys.toList) shouldBe
      Some(List("periods", "frequency", "rollConvention"))
    encoded.hcursor.downField("periods").as[List[SchedulePeriod]] shouldBe
      Right(List(P1_STUB, P2_NORMAL))

    // Equal values encode to the same document, no property having a representation that
    // depends on how it was built.
    sched(
      Schedule.of(
        NonEmptyList.of(P1_STUB, P2_NORMAL),
        Frequency.P1M,
        RollConventions.DAY_17)).asJson.noSpaces shouldBe encoded.noSpaces

    // A period whose dates are out of order is an invariant of `SchedulePeriod`, and the
    // validating decoder reports it as a decoding failure.
    val outOfOrder: String =
      """{"periods":[{"startDate":"2014-08-16","endDate":"2014-07-17",""" +
        """"unadjustedStartDate":"2014-08-16","unadjustedEndDate":"2014-07-17"}],""" +
        """"frequency":"P1M","rollConvention":"Day17"}"""
    decode[Schedule](outOfOrder).fold(
      error => error shouldBe a[DecodingFailure],
      schedule => fail(s"Expected a decoding failure but a schedule was produced: $schedule"))

    // A document whose periods are each valid but whose list runs backwards is the other
    // invariant, and the reason the decoder builds through the validated factory: a payload is
    // the route by which a reversed list would otherwise reach code that reads the periods as a
    // time line. The failure names both orderings that do not hold.
    val reversedDocument: String =
      """{"periods":[""" +
        """{"startDate":"2014-07-17","endDate":"2014-08-16",""" +
        """"unadjustedStartDate":"2014-07-17","unadjustedEndDate":"2014-08-17"},""" +
        """{"startDate":"2014-07-03","endDate":"2014-07-17",""" +
        """"unadjustedStartDate":"2014-07-04","unadjustedEndDate":"2014-07-17"}],""" +
        """"frequency":"P1M","rollConvention":"Day17"}"""
    decode[Schedule](reversedDocument).fold(
      error => {
        error shouldBe a[DecodingFailure]
        error.getMessage should include("the periods must run from earliest to latest")
      },
      schedule => fail(s"Expected a decoding failure but a schedule was produced: $schedule"))

    // An empty period list, a document missing the periods or missing every property, an unknown
    // frequency and a bare string in place of the object are rejected too.
    decode[Schedule](
      """{"periods":[],"frequency":"P1M","rollConvention":"Day17"}""").isLeft shouldBe true
    decode[Schedule]("""{"frequency":"P1M","rollConvention":"Day17"}""").isLeft shouldBe true
    decode[Schedule]("{}").isLeft shouldBe true
    decode[Schedule](
      """{"periods":[{"startDate":"2014-07-17","endDate":"2014-08-16",""" +
        """"unadjustedStartDate":"2014-07-17","unadjustedEndDate":"2014-08-17"}],""" +
        """"frequency":"Rubbish","rollConvention":"Day17"}""").isLeft shouldBe true
    Json.fromString("P1M").as[Schedule].isLeft shouldBe true
  }

}
