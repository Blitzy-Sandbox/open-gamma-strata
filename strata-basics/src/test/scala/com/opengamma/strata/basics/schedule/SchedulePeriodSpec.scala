/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.schedule

import java.time.LocalDate
import java.time.Month.AUGUST
import java.time.Month.JULY
import java.time.Month.JUNE
import java.time.Month.SEPTEMBER
import java.time.Period

import cats.Eq
import cats.Hash
import cats.Order
import cats.Show
import cats.data.NonEmptyChain

import io.circe.DecodingFailure
import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.basics.date.DateAdjuster
import com.opengamma.strata.basics.date.DayCounts
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[SchedulePeriod]], ported from the Java `SchedulePeriodTest`.
 *
 * This is a one-to-one port: each of the Java class's twenty-three test methods has a test of the
 * same name here, in the same order, and no test has been added, split off or dropped. The Java
 * class parameterised nothing, so every method became one plain `test("…")` block, which is what
 * keeps the method-level traceability recorded in `manifest/java-test-mapping.csv` exact - the
 * acceptance gate joins that file to the JUnit XML on the suite class and the test name, so both
 * have to match character for character.
 *
 * The eleven dates of the Java fixture and its `1e-6` comparison tolerance are reproduced
 * unchanged, and every expected value in this file is the value the Java test asserted. Where the
 * shape of the port makes a Java assertion inexpressible, the test keeps its name and asserts what
 * the port does in its place; each such substitution is commented at the case that makes it, and
 * the four kinds of substitution are these:
 *
 *   - '''The null-argument cases have no counterpart.''' Five of the Java methods -
 *     `test_of_null`, `test_yearFraction_null`, `test_isRegular_null`, `test_contains_null` and the
 *     null rows inside them - asserted that `ArgChecker.notNull` rejects a `null` argument. This
 *     port never writes `null` (Agent Action Plan §0.10.1, Rule 5, which greps the sources for it),
 *     and every parameter involved is a reference type the caller can only satisfy with a value, so
 *     there is no such call to make. `test_of_null` asserts instead the validation `of` really does
 *     perform - both pairs of dates in time-line order and distinct - and the three
 *     `*_null` cases assert that the member is total for valid arguments and that the arguments
 *     cannot be omitted, swapped or mistyped, proved with `assertDoesNotCompile`.
 *   - '''Failure is a value, never a raised exception.''' `SchedulePeriod` is a validated type
 *     (AAP §0.3.3 `[V]`): `of` and `toAdjusted` return `EitherNec[Failure, SchedulePeriod]` and
 *     `subSchedule` returns `EitherNec[Failure, PeriodicSchedule]`. Every Java
 *     `assertThatIllegalArgumentException` therefore becomes an assertion about a `Left`, made
 *     through the matchers of [[ResultMatchers]], and no test in this file asserts a throw. The
 *     expectations that the Java test wrote as bare values are unwrapped by [[sp]] so that they
 *     read as they did in the original.
 *   - '''The Joda bean builder has no target.''' `SchedulePeriod.builder()` does not exist: the type
 *     is a `sealed abstract case class` with a private constructor, so it has no public `apply` and
 *     no `copy` either, and the builder's defaulting of each absent unadjusted date to its adjusted
 *     counterpart survives as the two-argument [[SchedulePeriod.of]] (AAP §0.1.2).
 *     `test_builder_defaults` and `coverage_builder` assert that defaulting through the factory and
 *     prove the three removed routes are absent.
 *   - '''The reflective coverage helpers have no target.''' `coverImmutableBean` and
 *     `assertSerialization` are not part of this port's test kit, so `coverage` asserts what the
 *     bean sweep was checking - the four properties of two distinct values and the typeclass
 *     instances the companion publishes - and `test_serialization` asserts a circe round trip in
 *     place of Java serialization. Joda-Beans wire compatibility is out of scope (AAP §0.2.2).
 *
 * One assertion in this file has no Java ancestor at all, and this spec owns it: the ordering of
 * `SchedulePeriod` carries a '''tie-break''' the Java `compareTo` did not have. Java compared the
 * unadjusted start date and then the unadjusted end date and stopped, so two periods that differ
 * only in their adjusted dates compared equal while being unequal; cats requires `compare == 0`
 * exactly where `eqv` holds, so [[SchedulePeriod.order]] continues with the adjusted start date and
 * then the adjusted end date. The first two keys are untouched, so every ordering Java produced is
 * still produced. `coverage_equals` asserts the tie-break and the agreement of comparison with
 * equality over the whole Java fixture; the divergence is recorded in `SCALA_MIGRATION.md`.
 */
class SchedulePeriodSpec extends AnyFunSuite with Matchers with ResultMatchers {

  //-------------------------------------------------------------------------
  /** The reference data the sub-schedule cases are resolved against, as in the Java fixture. */
  private val REF_DATA: ReferenceData = ReferenceData.standard

  // The dates of the Java test class, unchanged, including its notes on the days of the week.
  private val JUN_15: LocalDate = date(2014, JUNE, 15) // Sunday
  private val JUN_16: LocalDate = date(2014, JUNE, 16)
  private val JUN_17: LocalDate = date(2014, JUNE, 17)
  private val JUN_18: LocalDate = date(2014, JUNE, 18)
  private val JUL_04: LocalDate = date(2014, JULY, 4)
  private val JUL_05: LocalDate = date(2014, JULY, 5)
  private val JUL_17: LocalDate = date(2014, JULY, 17)
  private val JUL_18: LocalDate = date(2014, JULY, 18)
  private val AUG_17: LocalDate = date(2014, AUGUST, 17) // Sunday
  private val AUG_18: LocalDate = date(2014, AUGUST, 18) // Monday
  private val SEP_17: LocalDate = date(2014, SEPTEMBER, 17)

  /** The comparison tolerance of the Java test class, `within(1e-6)`. */
  private val TOLERANCE: Double = 1e-6

  //-------------------------------------------------------------------------
  /**
   * Unwraps a period built by the validated factory, failing the test if it was rejected.
   *
   * [[SchedulePeriod.of]] reports a pair of dates that describes no period as a chain of reasons
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
   * Unwraps a sub-schedule definition, failing the test if it was rejected.
   *
   * [[SchedulePeriod.subSchedule]] hands its four arguments to [[PeriodicSchedule.of]], which is
   * validated in the same way, so the four sub-schedule cases unwrap the definition before
   * generating dates from it.
   *
   * @param result  the result of the validated factory
   * @return the definition the factory built
   */
  private def ps(result: ResultNec[PeriodicSchedule]): PeriodicSchedule =
    result.fold(
      failures => fail(rejectionMessage("periodic schedule", failures)),
      definition => definition)

  /**
   * Unwraps a generated schedule, failing the test if generation failed.
   *
   * [[PeriodicSchedule.createSchedule]] reports a single cause, so this is the `Either` form of
   * the two helpers above rather than the accumulating one.
   *
   * @param result  the result of schedule generation
   * @return the schedule that was generated
   */
  private def sched(result: FailureOr[Schedule]): Schedule =
    result.fold(
      failure => fail(s"Expected a schedule but generation failed: ${failure.message}"),
      schedule => schedule)

  /**
   * Returns the failures the validated factory reported, failing the test if it built a period.
   *
   * `test_of_null` asserts the '''number''' and the '''text''' of the reasons a rejected pair of
   * dates produces, because accumulating both causes of one call is the behaviour that replaces
   * the throwing validator of the bean being ported.
   *
   * @param result  the result of the validated factory
   * @return the failures the factory reported, in the order it reported them
   */
  private def failuresOf(result: ResultNec[SchedulePeriod]): List[Failure] =
    result.fold(
      failures => failures.toNonEmptyList.toList,
      period => fail(s"Expected a failure but a period was produced: $period"))

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

  //-------------------------------------------------------------------------
  test("test_of_null") {
    // Java asserted that each of the four dates being `null`, and all four being `null`, is
    // rejected by `ArgChecker.notNull`. There is no counterpart: every parameter is a `LocalDate`
    // and this port never writes `null`, so the five calls cannot be made. What the factory does
    // decide about its arguments is their order, which is the check Java ran immediately after the
    // null guards, so that is what the five rows become - one per way a pair of dates can fail to
    // describe a period, each reported as `Failure.Invalid`.
    SchedulePeriod.of(JUL_18, JUL_05, JUL_04, JUL_17) should beFailureWith(FailureReason.INVALID)
    SchedulePeriod.of(JUL_05, JUL_18, JUL_17, JUL_04) should beFailureWith(FailureReason.INVALID)
    SchedulePeriod.of(JUL_05, JUL_05, JUL_04, JUL_17) should beFailureWith(FailureReason.INVALID)
    SchedulePeriod.of(JUL_05, JUL_18, JUL_04, JUL_04) should beFailureWith(FailureReason.INVALID)
    SchedulePeriod.of(JUL_18, JUL_05) should beFailureWith(FailureReason.INVALID)

    // A single broken pair is one reason, naming the pair that broke.
    failuresOf(SchedulePeriod.of(JUL_18, JUL_05, JUL_04, JUL_17)).map(failure => failure.message)
      .shouldBe(
        List(
          "Invalid order: Expected 'startDate' < 'endDate', " +
            "but found: '2014-07-18' >= '2014-07-05'"))

    // Both pairs broken at once are reported together, which the throwing validator could not do:
    // it raised at the first pair it found out of order (AAP §0.3.3 requires `[V]` accumulation).
    failuresOf(SchedulePeriod.of(JUL_18, JUL_05, JUL_17, JUL_04)).map(failure => failure.message)
      .shouldBe(
        List(
          "Invalid order: Expected 'unadjustedStartDate' < 'unadjustedEndDate', " +
            "but found: '2014-07-17' >= '2014-07-04'",
          "Invalid order: Expected 'startDate' < 'endDate', " +
            "but found: '2014-07-18' >= '2014-07-05'"))

    // The two-date factory fills both pairs from the one pair it is given, so a pair that
    // describes no period is reported under both sets of names.
    failuresOf(SchedulePeriod.of(JUL_18, JUL_05)) should have size 2
    failuresOf(SchedulePeriod.of(JUL_05, JUL_05)) should have size 2
  }

  test("test_of_all") {
    val test: SchedulePeriod = sp(SchedulePeriod.of(JUL_05, JUL_18, JUL_04, JUL_17))
    test.startDate shouldBe JUL_05
    test.endDate shouldBe JUL_18
    test.unadjustedStartDate shouldBe JUL_04
    test.unadjustedEndDate shouldBe JUL_17
    SchedulePeriod.of(JUL_05, JUL_18, JUL_04, JUL_17) should beSuccess
  }

  test("test_of_noUnadjusted") {
    val test: SchedulePeriod = sp(SchedulePeriod.of(JUL_05, JUL_18))
    test.startDate shouldBe JUL_05
    test.endDate shouldBe JUL_18
    test.unadjustedStartDate shouldBe JUL_05
    test.unadjustedEndDate shouldBe JUL_18
  }

  test("test_builder_defaults") {
    // The Joda bean builder has no target in this port. Java built a period from the two adjusted
    // dates alone and relied on the builder's pre-build step to default each unadjusted date to
    // its adjusted counterpart; the two-argument factory is that behaviour (AAP §0.1.2), so the
    // case asserts the same four values through it and proves the builder is gone.
    val test: SchedulePeriod = sp(SchedulePeriod.of(JUL_05, JUL_18))
    test.startDate shouldBe JUL_05
    test.endDate shouldBe JUL_18
    test.unadjustedStartDate shouldBe JUL_05
    test.unadjustedEndDate shouldBe JUL_18
    assertDoesNotCompile("SchedulePeriod.builder()")
  }

  //-------------------------------------------------------------------------
  test("test_yearFraction") {
    val test: SchedulePeriod = sp(SchedulePeriod.of(JUN_16, JUL_18, JUN_16, JUL_17))
    val schedule: Schedule = Schedule.ofTerm(test)
    // The delegation Java asserted: the period's own year fraction is the day count applied to its
    // adjusted dates with the schedule supplied as the schedule information the convention reads.
    test.yearFraction(DayCounts.ACT_360, schedule) shouldBe
      (DayCounts.ACT_360.yearFraction(JUN_16, JUL_18, schedule) +- TOLERANCE)
    // The value itself, which 'Act/360' fixes: thirty-two days over a three-hundred-and-sixty-day
    // year. Asserted as well as the delegation so the case cannot pass on two agreeing mistakes.
    test.yearFraction(DayCounts.ACT_360, schedule) shouldBe (32.0 / 360.0 +- TOLERANCE)
  }

  test("test_yearFraction_null") {
    val test: SchedulePeriod = sp(SchedulePeriod.of(JUN_16, JUL_18, JUN_16, JUL_17))
    val schedule: Schedule = Schedule.ofTerm(test)
    // Java asserted that a `null` day count, a `null` schedule and both being `null` are rejected.
    // Neither parameter can be `null` here, so the case asserts what remains of the contract: the
    // member is total for the valid pair - it answers a number rather than a failure - and neither
    // argument can be omitted or supplied in the other's position.
    test.yearFraction(DayCounts.ACT_360, schedule) should be > 0.0
    assertDoesNotCompile("test.yearFraction(DayCounts.ACT_360)")
    assertDoesNotCompile("test.yearFraction(schedule)")
    assertDoesNotCompile("test.yearFraction(schedule, DayCounts.ACT_360)")
    assertDoesNotCompile("test.yearFraction()")
  }

  //-------------------------------------------------------------------------
  test("test_length") {
    sp(SchedulePeriod.of(JUN_16, JUN_18, JUN_16, JUN_18)).length shouldBe
      Period.between(JUN_16, JUN_18)
    sp(SchedulePeriod.of(JUN_16, JUL_18, JUN_16, JUL_17)).length shouldBe
      Period.between(JUN_16, JUL_18)
  }

  //-------------------------------------------------------------------------
  test("test_lengthInDays") {
    sp(SchedulePeriod.of(JUN_16, JUN_18, JUN_16, JUN_18)).lengthInDays shouldBe 2
    sp(SchedulePeriod.of(JUN_16, JUL_18, JUN_16, JUL_17)).lengthInDays shouldBe 32
  }

  //-------------------------------------------------------------------------
  test("test_isRegular") {
    sp(SchedulePeriod.of(JUN_18, JUL_18))
      .isRegular(Frequency.P1M, RollConventions.DAY_18) shouldBe true
    sp(SchedulePeriod.of(JUN_18, JUL_05))
      .isRegular(Frequency.P1M, RollConventions.DAY_18) shouldBe false
    sp(SchedulePeriod.of(JUL_05, JUL_18))
      .isRegular(Frequency.P1M, RollConventions.DAY_18) shouldBe false
    sp(SchedulePeriod.of(JUN_18, JUL_05))
      .isRegular(Frequency.P2M, RollConventions.DAY_18) shouldBe false
  }

  test("test_isRegular_null") {
    val test: SchedulePeriod = sp(SchedulePeriod.of(JUN_16, JUL_18))
    // Java asserted a `null` frequency, a `null` roll convention and both being `null`. As above,
    // neither is expressible, so the case asserts the member is total for valid arguments - both
    // answers are produced, not just one - and that the two arguments cannot be transposed. The
    // period of the Java fixture is not regular under any monthly convention, because its two
    // dates fall on different days of the month and the check is symmetric, so the regular answer
    // is taken from a period whose dates share a day of the month.
    test.isRegular(Frequency.P1M, RollConventions.DAY_18) shouldBe false
    sp(SchedulePeriod.of(JUN_18, JUL_18))
      .isRegular(Frequency.P1M, RollConventions.DAY_18) shouldBe true
    assertDoesNotCompile("test.isRegular(Frequency.P1M)")
    assertDoesNotCompile("test.isRegular(RollConventions.DAY_18, Frequency.P1M)")
    assertDoesNotCompile("test.isRegular()")
  }

  //-------------------------------------------------------------------------
  test("test_contains") {
    sp(SchedulePeriod.of(JUN_16, JUL_18, JUN_16, JUL_17)).contains(JUN_15) shouldBe false
    sp(SchedulePeriod.of(JUN_16, JUL_18, JUN_16, JUL_17)).contains(JUN_16) shouldBe true
    sp(SchedulePeriod.of(JUN_16, JUL_18, JUN_16, JUL_17)).contains(JUL_05) shouldBe true
    sp(SchedulePeriod.of(JUN_16, JUL_18, JUN_16, JUL_17)).contains(JUL_17) shouldBe true
    sp(SchedulePeriod.of(JUN_16, JUL_18, JUN_16, JUL_17)).contains(JUL_18) shouldBe false
  }

  test("test_contains_null") {
    val test: SchedulePeriod = sp(SchedulePeriod.of(JUN_16, JUL_18))
    // Java asserted a `null` date. Not expressible: the parameter is a `LocalDate`, so the case
    // asserts the member is total on both sides of its boundary and that nothing but a date is
    // accepted in its place.
    test.contains(JUN_16) shouldBe true
    test.contains(JUL_18) shouldBe false
    assertDoesNotCompile("""test.contains("2014-06-16")""")
    assertDoesNotCompile("test.contains()")
  }

  //-------------------------------------------------------------------------
  test("test_subSchedule_1monthIn3Month") {
    // `subSchedule` answers the sub-schedule's definition, as the method being ported did, and the
    // dates are generated from it with reference data the caller supplies - two steps where Java
    // wrote one, because the definition is validated and the generation needs a holiday calendar.
    val test: SchedulePeriod = sp(SchedulePeriod.of(JUN_17, SEP_17))
    val schedule: Schedule = sched(
      ps(
        test.subSchedule(
          Frequency.P1M,
          RollConventions.DAY_17,
          StubConvention.NONE,
          BusinessDayAdjustment.NONE)).createSchedule(REF_DATA))
    schedule.size shouldBe 3
    schedule.period(0) shouldBe sp(SchedulePeriod.of(JUN_17, JUL_17))
    schedule.period(1) shouldBe sp(SchedulePeriod.of(JUL_17, AUG_17))
    schedule.period(2) shouldBe sp(SchedulePeriod.of(AUG_17, SEP_17))
    // Java's `getFrequency` is `periodicFrequency` here, the plainly typed value; the name
    // `frequency` belongs to the `Option`-returning member of the schedule information interface.
    schedule.periodicFrequency shouldBe Frequency.P1M
    schedule.rollConvention shouldBe RollConventions.DAY_17
  }

  test("test_subSchedule_3monthIn3Month") {
    val test: SchedulePeriod = sp(SchedulePeriod.of(JUN_17, SEP_17))
    val schedule: Schedule = sched(
      ps(
        test.subSchedule(
          Frequency.P3M,
          RollConventions.DAY_17,
          StubConvention.NONE,
          BusinessDayAdjustment.NONE)).createSchedule(REF_DATA))
    schedule.size shouldBe 1
    schedule.period(0) shouldBe sp(SchedulePeriod.of(JUN_17, SEP_17))
  }

  test("test_subSchedule_2monthIn3Month_shortInitial") {
    val test: SchedulePeriod = sp(SchedulePeriod.of(JUN_17, SEP_17))
    val schedule: Schedule = sched(
      ps(
        test.subSchedule(
          Frequency.P2M,
          RollConventions.DAY_17,
          StubConvention.SHORT_INITIAL,
          BusinessDayAdjustment.NONE)).createSchedule(REF_DATA))
    schedule.size shouldBe 2
    schedule.period(0) shouldBe sp(SchedulePeriod.of(JUN_17, JUL_17))
    schedule.period(1) shouldBe sp(SchedulePeriod.of(JUL_17, SEP_17))
    schedule.periodicFrequency shouldBe Frequency.P2M
    schedule.rollConvention shouldBe RollConventions.DAY_17
  }

  test("test_subSchedule_2monthIn3Month_shortFinal") {
    val test: SchedulePeriod = sp(SchedulePeriod.of(JUN_17, SEP_17))
    val schedule: Schedule = sched(
      ps(
        test.subSchedule(
          Frequency.P2M,
          RollConventions.DAY_17,
          StubConvention.SHORT_FINAL,
          BusinessDayAdjustment.NONE)).createSchedule(REF_DATA))
    schedule.size shouldBe 2
    schedule.period(0) shouldBe sp(SchedulePeriod.of(JUN_17, AUG_17))
    schedule.period(1) shouldBe sp(SchedulePeriod.of(AUG_17, SEP_17))
    schedule.periodicFrequency shouldBe Frequency.P2M
    schedule.rollConvention shouldBe RollConventions.DAY_17
  }

  //-------------------------------------------------------------------------
  test("test_toAdjusted") {
    // `toAdjusted` reports failure as a value, because adjustment can bring the two dates of a
    // period onto one day, so each expectation is a `Right`. Java's lambda `date -> date` is a
    // Scala function handed to [[DateAdjuster]].
    val test1: SchedulePeriod = sp(SchedulePeriod.of(JUN_15, SEP_17))
    test1.toAdjusted(DateAdjuster(adjusted => adjusted)) shouldBe Right(test1)
    // The identity path answers this very instance, which is load-bearing: `Schedule.toAdjusted`
    // decides by reference whether any period of a schedule moved.
    sp(test1.toAdjusted(DateAdjuster(adjusted => adjusted))) should be theSameInstanceAs test1
    test1.toAdjusted(DateAdjuster(adjusted => if (adjusted == JUN_15) JUN_16 else adjusted)) shouldBe
      Right(sp(SchedulePeriod.of(JUN_16, SEP_17, JUN_15, SEP_17)))
    val test2: SchedulePeriod = sp(SchedulePeriod.of(JUN_16, AUG_17))
    test2.toAdjusted(DateAdjuster(adjusted => if (adjusted == AUG_17) AUG_18 else adjusted)) shouldBe
      Right(sp(SchedulePeriod.of(JUN_16, AUG_18, JUN_16, AUG_17)))
  }

  test("test_toUnadjusted") {
    sp(SchedulePeriod.of(JUN_15, SEP_17)).toUnadjusted shouldBe
      sp(SchedulePeriod.of(JUN_15, SEP_17))
    sp(SchedulePeriod.of(JUN_16, SEP_17, JUN_15, SEP_17)).toUnadjusted shouldBe
      sp(SchedulePeriod.of(JUN_15, SEP_17))
    sp(SchedulePeriod.of(JUN_16, JUL_18, JUN_16, JUL_17)).toUnadjusted shouldBe
      sp(SchedulePeriod.of(JUN_16, JUL_17))
  }

  //-------------------------------------------------------------------------
  test("test_compareTo") {
    // Java's `Comparable` has no counterpart; the comparison is the `Order` instance the companion
    // publishes, and the nine rows below are Java's matrix unchanged. Each of the three periods is
    // built from two dates, so its adjusted pair equals its unadjusted pair and the two keys the
    // port adds after Java's two cannot change any of these answers.
    val a: SchedulePeriod = sp(SchedulePeriod.of(JUL_05, JUL_18))
    val b: SchedulePeriod = sp(SchedulePeriod.of(JUL_04, JUL_18))
    val c: SchedulePeriod = sp(SchedulePeriod.of(JUL_05, JUL_17))
    (Order[SchedulePeriod].compare(a, a) == 0) shouldBe true
    (Order[SchedulePeriod].compare(a, b) > 0) shouldBe true
    (Order[SchedulePeriod].compare(a, c) > 0) shouldBe true

    (Order[SchedulePeriod].compare(b, a) < 0) shouldBe true
    (Order[SchedulePeriod].compare(b, b) == 0) shouldBe true
    (Order[SchedulePeriod].compare(b, c) < 0) shouldBe true

    (Order[SchedulePeriod].compare(c, a) < 0) shouldBe true
    (Order[SchedulePeriod].compare(c, b) > 0) shouldBe true
    (Order[SchedulePeriod].compare(c, c) == 0) shouldBe true
  }

  //-------------------------------------------------------------------------
  test("coverage_equals") {
    val a1: SchedulePeriod = sp(SchedulePeriod.of(JUL_05, JUL_18, JUL_04, JUL_17))
    val a2: SchedulePeriod = sp(SchedulePeriod.of(JUL_05, JUL_18, JUL_04, JUL_17))
    val b: SchedulePeriod = sp(SchedulePeriod.of(JUL_04, JUL_18, JUL_04, JUL_17))
    val c: SchedulePeriod = sp(SchedulePeriod.of(JUL_05, JUL_17, JUL_04, JUL_17))
    val d: SchedulePeriod = sp(SchedulePeriod.of(JUL_05, JUL_18, JUL_05, JUL_17))
    val e: SchedulePeriod = sp(SchedulePeriod.of(JUL_05, JUL_18, JUL_04, JUL_18))
    (a1 == a1) shouldBe true
    (a1 == a2) shouldBe true
    (a1 == b) shouldBe false
    (a1 == c) shouldBe false
    (a1 == d) shouldBe false
    (a1 == e) shouldBe false

    // The port publishes one equality-bearing instance, an `Order` that is also a `Hash`, so the
    // case additionally asserts that it agrees with `equals` and `hashCode` on the same values.
    Eq[SchedulePeriod].eqv(a1, a2) shouldBe true
    Eq[SchedulePeriod].eqv(a1, b) shouldBe false
    Eq[SchedulePeriod].eqv(a1, c) shouldBe false
    Eq[SchedulePeriod].eqv(a1, d) shouldBe false
    Eq[SchedulePeriod].eqv(a1, e) shouldBe false
    Hash[SchedulePeriod].hash(a1) shouldBe a1.hashCode
    Hash[SchedulePeriod].hash(a1) shouldBe Hash[SchedulePeriod].hash(a2)

    // The ordering tie-break this spec owns (AAP §0.3.3, recorded in `SCALA_MIGRATION.md`). Java's
    // `compareTo` compared the unadjusted start date and then the unadjusted end date and stopped,
    // so `a1` against `b` and `a1` against `c` - pairs sharing both unadjusted dates and differing
    // in one adjusted date - compared '''equal''' while being '''unequal'''. Cats requires the two
    // notions to agree, so the ordering continues with the adjusted start date and then the
    // adjusted end date: the comparison is non-zero exactly where equality is false.
    a1.unadjustedStartDate shouldBe c.unadjustedStartDate
    a1.unadjustedEndDate shouldBe c.unadjustedEndDate
    (Order[SchedulePeriod].compare(a1, c) > 0) shouldBe true
    Eq[SchedulePeriod].eqv(a1, c) shouldBe false
    (Order[SchedulePeriod].compare(a1, b) > 0) shouldBe true
    Order[SchedulePeriod].compare(a1, a2) shouldBe 0

    // `compare == 0` if and only if `eqv`, over every pair of the Java fixture.
    val values: List[SchedulePeriod] = List(a1, a2, b, c, d, e)
    values.foreach { left =>
      values.foreach { right =>
        (Order[SchedulePeriod].compare(left, right) == 0) shouldBe
          Eq[SchedulePeriod].eqv(left, right)
      }
    }
  }

  //-------------------------------------------------------------------------
  test("coverage_builder") {
    // Java exercised the four setters of the bean builder. There is no builder, no public `apply`
    // and no `copy`, so the case asserts the same construction through the four-date factory -
    // every accessor reads back exactly what was supplied - and proves the three routes are absent.
    val test: SchedulePeriod = sp(SchedulePeriod.of(JUL_05, JUL_18, JUL_04, JUL_17))
    test.startDate shouldBe JUL_05
    test.endDate shouldBe JUL_18
    test.unadjustedStartDate shouldBe JUL_04
    test.unadjustedEndDate shouldBe JUL_17
    assertDoesNotCompile("SchedulePeriod.builder()")
    assertDoesNotCompile("SchedulePeriod(JUL_05, JUL_18, JUL_04, JUL_17)")
    assertDoesNotCompile("test.copy(startDate = JUL_04)")
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // `coverImmutableBean` walked the bean's meta-properties reflectively. There is no meta-bean to
    // walk, so the ground it covered is asserted directly: the four properties of two distinct
    // values, and the two typeclass instances the companion publishes in place of the bean's
    // equality, hashing and generated text.
    val test: SchedulePeriod = sp(SchedulePeriod.of(JUL_05, JUL_18, JUL_04, JUL_17))
    val other: SchedulePeriod = sp(SchedulePeriod.of(JUL_05, JUL_18))
    test.startDate shouldBe JUL_05
    test.endDate shouldBe JUL_18
    test.unadjustedStartDate shouldBe JUL_04
    test.unadjustedEndDate shouldBe JUL_17
    other.startDate shouldBe JUL_05
    other.endDate shouldBe JUL_18
    other.unadjustedStartDate shouldBe JUL_05
    other.unadjustedEndDate shouldBe JUL_18
    Eq[SchedulePeriod].eqv(test, test) shouldBe true
    Eq[SchedulePeriod].eqv(test, other) shouldBe false
    Hash[SchedulePeriod].hash(test) shouldBe test.hashCode
    Hash[SchedulePeriod].hash(other) shouldBe other.hashCode
    Show[SchedulePeriod].show(test) shouldBe test.toString
    Show[SchedulePeriod].show(other) shouldBe other.toString
    // The text is this port's own form: a period that adjusts nothing renders as its two dates,
    // and any other period adds the unadjusted pair, so nothing a value holds is hidden.
    test.toString shouldBe "2014-07-05 to 2014-07-18 (unadjusted 2014-07-04 to 2014-07-17)"
    other.toString shouldBe "2014-07-05 to 2014-07-18"
  }

  test("test_serialization") {
    // `assertSerialization` checked Java serialization, which this port does not support, and
    // Joda-Beans wire compatibility is out of scope (AAP §0.2.2). The replacement is the circe
    // round trip through the compile-time product codec: the keys are the four Java property names
    // in their declaration order, each date is an ISO-8601 string, and a payload that violates an
    // invariant of the type is rejected by the validating decoder rather than carried into a value.
    val test: SchedulePeriod = sp(SchedulePeriod.of(JUL_05, JUL_18, JUL_04, JUL_17))
    val encoded: Json = test.asJson
    encoded.noSpaces shouldBe
      """{"startDate":"2014-07-05","endDate":"2014-07-18",""" +
        """"unadjustedStartDate":"2014-07-04","unadjustedEndDate":"2014-07-17"}"""
    decode[SchedulePeriod](encoded.noSpaces) shouldBe Right(test)
    encoded.asObject.map(json => json.keys.toList) shouldBe
      Some(List("startDate", "endDate", "unadjustedStartDate", "unadjustedEndDate"))

    // Equal values encode to identical bytes, no property having a representation that depends on
    // how it was built.
    sp(SchedulePeriod.of(JUL_05, JUL_18, JUL_04, JUL_17)).asJson.noSpaces shouldBe encoded.noSpaces

    // The validating decoder: a payload whose adjusted dates are out of order is a decoding
    // failure carrying the message of the validation that rejected it, not a value the factory
    // would never have built.
    val outOfOrder: String =
      """{"startDate":"2014-07-18","endDate":"2014-07-05",""" +
        """"unadjustedStartDate":"2014-07-04","unadjustedEndDate":"2014-07-17"}"""
    decode[SchedulePeriod](outOfOrder) match {
      case Left(failure: DecodingFailure) => failure.message should include("Invalid order")
      case other => fail(s"Expected a decoding failure but was: $other")
    }

    // Both pairs out of order are reported together, as they are by the factory the decoder uses.
    val bothOutOfOrder: String =
      """{"startDate":"2014-07-18","endDate":"2014-07-05",""" +
        """"unadjustedStartDate":"2014-07-17","unadjustedEndDate":"2014-07-04"}"""
    decode[SchedulePeriod](bothOutOfOrder) match {
      case Left(failure: DecodingFailure) =>
        failure.message should include("'unadjustedStartDate' < 'unadjustedEndDate'")
        failure.message should include("'startDate' < 'endDate'")
      case other => fail(s"Expected a decoding failure but was: $other")
    }

    // All four fields are required, and the object is the only shape accepted.
    decode[SchedulePeriod]("""{"startDate":"2014-07-05","endDate":"2014-07-18"}""")
      .isLeft shouldBe true
    decode[SchedulePeriod]("{}").isLeft shouldBe true
    decode[SchedulePeriod](
      """{"startDate":"2014-07-05","endDate":"not-a-date",""" +
        """"unadjustedStartDate":"2014-07-04","unadjustedEndDate":"2014-07-17"}""")
      .isLeft shouldBe true
    Json.fromString("2014-07-05 to 2014-07-18").as[SchedulePeriod].isLeft shouldBe true
  }
}
