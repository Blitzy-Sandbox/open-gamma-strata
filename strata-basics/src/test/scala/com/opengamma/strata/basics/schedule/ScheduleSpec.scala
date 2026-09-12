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
 * Test [[Schedule]], ported from the Java `ScheduleTest`.
 *
 * This is a one-to-one port: each of the Java class's thirty-seven test methods has a test of the
 * same name here, in the same order, and no test has been added, split off or dropped. The Java
 * class parameterised nothing, so every method became one plain `test("…")` block, which is what
 * keeps the method-level traceability recorded in `manifest/java-test-mapping.csv` exact - the
 * acceptance gate joins that file to the JUnit XML on the suite class and the test name.
 *
 * ===How the shape of the port changes the assertions===
 *
 *   - '''The bean builder has no target.''' Every Java body began with
 *     `Schedule.builder().periods(…).frequency(…).rollConvention(…).build()`, and here a
 *     schedule is built through [[Schedule.of]], whose period list is a
 *     `cats.data.NonEmptyList`, and the result is unwrapped by [[sched]]. A single period becomes
 *     [[Schedule.ofTerm]] where the Java test used it.
 *   - '''The accessors of the schedule information interface are `Option`s.''' `Schedule` is a
 *     [[com.opengamma.strata.basics.date.DayCount.ScheduleInfo]], and that interface declares
 *     `startDate`, `endDate`, `frequency` and `periodEndDate` as `Option`s. The schedule's own
 *     plainly typed values are [[Schedule.adjustedStartDate]], [[Schedule.adjustedEndDate]] and
 *     [[Schedule.periodicFrequency]], which are what the Java getters `getStartDate`,
 *     `getEndDate` and `getFrequency` returned. Both readings are asserted wherever Java asserted
 *     the getter, the `Option`-valued ones through the explicitly bound `info` view, so the two
 *     cannot drift apart.
 *   - '''`getPeriodEndDate` answers `None` where Java raised.''' See `test_getPeriodEndDate`.
 *   - '''`getStubs` returns a tuple.''' The pair type of the Java collect module is not part of
 *     this port, so `stubs(preferFinal)` is a `(Option[SchedulePeriod], Option[SchedulePeriod])`
 *     and Java's `Pair.of(a, b)` became `((a, b))`.
 *   - '''Merging and adjusting report failure as a value.''' `merge`, `mergeRegular` and
 *     `toAdjusted` return `Either[Failure, Schedule]`; `mergeToTerm` and `toUnadjusted` are total.
 *     Every Java `ScheduleException` - and the group-size check Java raised
 *     `IllegalArgumentException` for - is a `Left(Failure.Invalid)` here, carrying the message
 *     text of the Java exception unchanged. No test in this file asserts a raised exception except
 *     the one caller-contract refusal described next.
 *   - '''An out-of-range period index stays a refusal.''' `Schedule.period(index)` refuses an
 *     index outside the schedule through `ArgCheck`, as the indexed list access being ported did;
 *     the Agent Action Plan keeps index bounds among the documented `ArgCheck` throws rather than
 *     in the failable surface. Java asserted `IndexOutOfBoundsException`, so the assertion is
 *     kept, against the `IllegalArgumentException` the port documents.
 *   - '''The reflective coverage helpers have no target.''' `coverImmutableBean`,
 *     `coverBeanEquals` and `assertSerialization` do not exist in this port's test kit, so
 *     `coverage`, `coverage_builder` and `test_serialization` assert what those helpers were
 *     checking: the typeclass instances, construction through the factory, and a circe round trip.
 *     Joda-Beans wire compatibility is out of scope (AAP §0.2.2).
 */
class ScheduleSpec extends AnyFunSuite with Matchers with ResultMatchers {

  //-------------------------------------------------------------------------
  // The dates of the Java test class, unchanged.
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
   * rather than by raising, so every fixture and expectation of this spec goes through this
   * helper: a value the spec intends to be valid that turns out not to be is a failure of the
   * spec, reported with the reasons the factory gave.
   *
   * @param result  the result of the validated factory
   * @return the period the factory built
   */
  private def sp(result: ResultNec[SchedulePeriod]): SchedulePeriod =
    result.fold(failures => fail(rejectionMessage("schedule period", failures)), period => period)

  /**
   * Unwraps a schedule built by the validated factory, failing the test if it was rejected.
   *
   * This is the replacement for the bean builder every Java test body used, so a Java
   * `Schedule.builder().periods(ImmutableList.of(a, b)).frequency(f).rollConvention(r).build()`
   * reads here as `sched(Schedule.of(NonEmptyList.of(a, b), f, r))`.
   *
   * @param result  the result of the validated factory
   * @return the schedule the factory built
   */
  private def sched(result: ResultNec[Schedule]): Schedule =
    result.fold(failures => fail(rejectionMessage("schedule", failures)), schedule => schedule)

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
   * `merge`, `mergeRegular` and `toAdjusted` report failure as a value, and several cases assert
   * the text of that value because it is the text of the Java exception they replace.
   *
   * @param result  the result of the operation
   * @return the message of the failure the operation reported
   */
  private def messageOf(result: FailureOr[Schedule]): String =
    result.fold(
      failure => failure.message,
      schedule => fail(s"Expected a failure but a schedule was produced: $schedule"))

  /** The absence of a stub, typed so that the tuples `stubs` answers compare without inference. */
  private val NoStub: Option[SchedulePeriod] = None

  //-------------------------------------------------------------------------
  // The periods of the Java test class, unchanged, each built through the validated factory.
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
    // The Java case asserted that the builder rejected an empty period list. In this port that
    // state is not expressible: `Schedule.periods` is a `cats.data.NonEmptyList`, so the
    // "at least one period" invariant is carried by the type rather than by a check (AAP §0.3.3,
    // "non-empty invariants become non-empty types"). The case is therefore asserted in the
    // strongest form it can take - a compile-time proof that no empty list, and no ordinary list
    // at all, can be offered as the periods of a schedule - and no exception is asserted.
    assertDoesNotCompile("Schedule.of(Nil, Frequency.P1M, RollConventions.DAY_17)")
    assertDoesNotCompile(
      "Schedule.of(List.empty[SchedulePeriod], Frequency.P1M, RollConventions.DAY_17)")
    assertDoesNotCompile(
      "Schedule.of(NonEmptyList.fromList(Nil), Frequency.P1M, RollConventions.DAY_17)")

    // The type also has no public constructor, so the factory is the only route in and the
    // invariant cannot be bypassed.
    assertDoesNotCompile(
      "Schedule(NonEmptyList.one(P1_STUB), Frequency.P1M, RollConventions.DAY_17)")

    // What the Java check was protecting is that the smallest schedule has one period, which is
    // what the type now guarantees; that smallest schedule is asserted here so the case carries a
    // positive statement as well as the four proofs above.
    val smallest: Schedule =
      sched(Schedule.of(NonEmptyList.one(P1_STUB), Frequency.P1M, RollConventions.DAY_17))
    smallest.size shouldBe 1
    smallest.periods shouldBe NonEmptyList.one(P1_STUB)
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
    // An index outside the schedule is a broken call rather than data to report on, so it is
    // refused; the port documents `IllegalArgumentException` where Java documented
    // `IndexOutOfBoundsException`.
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
    // The schedule does not make sense, but the case only requires a roll convention of EOM.
    val test: Schedule = sched(
      Schedule.of(
        NonEmptyList.of(P2_NORMAL, P3_NORMAL),
        Frequency.P1M,
        RollConventions.EOM))
    val info: DayCount.ScheduleInfo = test
    info.isEndOfMonthConvention shouldBe true
    // Read the same fact off the schedule's own roll convention, so the flag cannot agree with
    // the interface while disagreeing with the value it is derived from.
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

    // The divergence this file owns (AAP §0.6.1, recorded in `SCALA_MIGRATION.md`): the Java
    // method raised `IllegalArgumentException("Date is not contained in any period")` for a date
    // lying in none of the periods, and the ported `DayCount.ScheduleInfo` makes the accessor
    // `Option`-valued instead, because a date outside the schedule describes the data rather than
    // a broken call. The assertion is therefore the absence of a value, not a raised exception -
    // before the first period, and, since a period excludes its end date, on the schedule's own
    // end date.
    info.periodEndDate(P2_NORMAL.startDate.minusDays(1)) shouldBe None
    info.periodEndDate(P3_NORMAL.endDate) shouldBe None
  }


  //-------------------------------------------------------------------------
  test("test_mergeToTerm") {
    // `mergeToTerm` is total in this port - the span of a schedule whose periods run from
    // earliest to latest is always a valid period - so the result is a schedule rather than an
    // `Either`, exactly as in Java.
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

    // Java raised `IllegalArgumentException` for a group size of zero or less. The Agent Action
    // Plan places the group-size checks of `merge` and `mergeRegular` in the failable inventory
    // (§0.3.3) and states the mapping outright in §0.4.1 - "group-size checks -> Failure.Invalid"
    // - so each of the six calls Java expected to raise reports a failure value here instead. No
    // exception is asserted, and each failure names the argument at fault.
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

    // The Java `ScheduleException` for a date matching nothing in the schedule is a
    // `Failure.Invalid` carrying the same message text.
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

    // The message is the Java text verbatim, including the quoted frequency, built here from the
    // same fixture dates the Java case used.
    val expectedMessage: String =
      s"Unable to merge schedule, firstRegularStartDate ${P2_NORMAL.unadjustedStartDate}" +
        s" and lastRegularEndDate ${P6_NORMAL.unadjustedEndDate}" +
        " cannot be used to create regular periods of frequency 'P2M'"
    val result: FailureOr[Schedule] =
      test.merge(2, P2_NORMAL.unadjustedStartDate, P6_NORMAL.unadjustedEndDate)
    result should beFailureWith(FailureReason.INVALID)
    messageOf(result) shouldBe expectedMessage
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

    // AAP §0.4.1 for `Schedule.scala`: where adjustment drives a period's dates out of order,
    // `SchedulePeriod.of` rejects the rebuilt period and `toAdjusted` reports that rejection as a
    // failure value - the Java implementation propagated it by raising. The adjuster below moves
    // the end of the last period before its start, which is the rejection that cannot be
    // sidestepped by the first/last collapse rule, since the two adjusted dates differ.
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

    // The port publishes one equality-bearing instance, a `Hash`, so the case additionally
    // asserts that it agrees with `equals` and `hashCode` on the same four values: a difference in
    // any of the three properties is a difference, and two values built the same way agree.
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
    // The Joda bean builder has no target in this port: [[Schedule.of]] replaces it, and the type
    // is a `sealed abstract case class` with a private constructor, so there is no public `apply`
    // and no `copy` either. The case therefore asserts the same construction through the factory -
    // every property reads back what was supplied - and proves the two removed routes are absent.
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
    // `coverImmutableBean` walked the bean's meta-properties reflectively. There is no meta-bean
    // to walk, so the ground it covered is asserted directly: the three properties of two
    // distinct schedules, and the three typeclass instances the companion publishes in place of
    // the bean's equality, hashing and generated text.
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
    // `assertSerialization` checked Java serialization, which this port does not support, and
    // Joda-Beans wire compatibility is out of scope (AAP §0.2.2). The replacement is the circe
    // round trip through the semiauto product codec: the keys are the Java property names, the
    // periods are an array in schedule order, and a payload that violates an invariant of the
    // type is rejected by the validating decoder rather than carried into a value.
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

    // Equal values encode to identical bytes, no property having a representation that depends on
    // how it was built.
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

    // The non-empty invariant of the period list, a missing property, a frequency that names no
    // frequency, and a bare string in place of the object are all rejected too.
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
