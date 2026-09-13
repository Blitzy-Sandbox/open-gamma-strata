/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.DayOfWeek
import java.time.LocalDate

import cats.Hash
import cats.Show
import cats.syntax.apply._

import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.ImmutableReferenceData
import com.opengamma.strata.basics.RefDataReader
import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[DaysAdjustment]].
 *
 * An adjustment counts its days against one calendar and then applies a [[BusinessDayAdjustment]]
 * to the result, which may name another: a date reached by walking the business days of the first
 * calendar can still be a holiday of the second, and it is the trailing adjustment that moves it
 * off. The two-calendar tests below drive exactly that pairing.
 *
 * `adjust` and `resolve` answer `Either[Failure, _]`. A calendar the reference data cannot supply
 * is the data-dependent failure this API reports rather than throws, and every failure is
 * identified by the `FailureReason` it carries - a value of a closed family - with its message
 * asserted only where the identifier that message names is the point.
 *
 * Three resolution paths are exercised throughout - `adjust(date, refData)`,
 * `resolve(refData).adjust(date)` and `toReader.run(refData)`. Resolving once and applying the
 * result to several dates, and composing two readers and supplying the data to the composition,
 * must compute what supplying reference data at each call computes.
 */
class DaysAdjustmentSpec extends AnyFunSuite with Matchers {

  /** Reference data holding every built-in calendar. */
  private val REF_DATA: ReferenceData = ReferenceData.standard

  /** The identifier of the fictional calendar whose weekend is Wednesday and Thursday. */
  private val WED_THU: HolidayCalendarId = HolidayCalendarId.of("WedThu")

  private val BDA_NONE: BusinessDayAdjustment = BusinessDayAdjustment.NONE

  private val BDA_FOLLOW_SAT_SUN: BusinessDayAdjustment =
    BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.SAT_SUN)

  private val BDA_FOLLOW_WED_THU: BusinessDayAdjustment =
    BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, WED_THU)

  /**
   * No holidays of its own and a weekend of Wednesday and Thursday, which is what makes the
   * two-calendar form observable: a date reached by walking Saturday/Sunday business days can
   * still be a holiday here.
   */
  private val WED_THU_CALENDAR: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(WED_THU, Nil, DayOfWeek.WEDNESDAY, DayOfWeek.THURSDAY)

  /** The fictional calendar first, the built-in set behind it. */
  private val WED_THU_REF_DATA: ReferenceData =
    ImmutableReferenceData.of(WED_THU, WED_THU_CALENDAR).combinedWith(REF_DATA)

  /**
   * A store holding the weekend calendar but not the no-holidays one, which is what separates
   * `adjust` from `resolve` for a calendar-day addition.
   */
  private val SAT_SUN_ONLY_REF_DATA: ReferenceData =
    ImmutableReferenceData.of(HolidayCalendarIds.SAT_SUN, HolidayCalendars.SAT_SUN)

  /** An identifier no reference data of this library holds, used to observe the failure path. */
  private val UNKNOWN_CALENDAR: HolidayCalendarId = HolidayCalendarId.of("XXXX")

  /**
   * The part both refusals of an out-of-range business day count share, which names the bound.
   *
   * A business day addition walks the calendar a day at a time, so its count is the work it
   * does, and a count no calendar can be walked is refused - by raising from the factories that
   * hold their fields as given, and in the error channel of the validated factory. The two say
   * the same thing in the same words up to the count they quote, so one string asserts both, and
   * it is built from the bound itself rather than repeating the number.
   */
  private val MagnitudeMessage: String =
    s"An addition of more than ${HolidayCalendar.MaxBusinessDayShift} business days is " +
      "outside the accepted range"

  // the dates of the Java class, named by their day of the week so that a line below can be read
  // against the Java source without counting days
  private val FRI_2014_08_15: LocalDate = date(2014, 8, 15)
  private val SAT_2014_08_16: LocalDate = date(2014, 8, 16)
  private val SUN_2014_08_17: LocalDate = date(2014, 8, 17)
  private val MON_2014_08_18: LocalDate = date(2014, 8, 18)
  private val TUE_2014_08_19: LocalDate = date(2014, 8, 19)
  private val WED_2014_08_20: LocalDate = date(2014, 8, 20)
  private val FRI_2014_08_22: LocalDate = date(2014, 8, 22)

  /** The London summer bank holiday of 2014, which is a business day of every weekend calendar. */
  private val MON_2014_08_25: LocalDate = date(2014, 8, 25)

  /** The Tuesday after that bank holiday, which is the next London business day. */
  private val TUE_2014_08_26: LocalDate = date(2014, 8, 26)

  //-------------------------------------------------------------------------
  test("test_NONE") {
    val test: DaysAdjustment = DaysAdjustment.NONE

    test.days shouldBe 0
    test.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    test.adjustment shouldBe BDA_NONE
    test.toString shouldBe "0 calendar days"

    // Equality is by field, so the constant is not privileged: it is `ofCalendarDays(0)`.
    test shouldBe DaysAdjustment.ofCalendarDays(0)
    Show[DaysAdjustment].show(test) shouldBe "0 calendar days"

    // And it is the identity: every date comes back unaltered, weekend or not.
    test.adjust(FRI_2014_08_15, REF_DATA) should haveValue(FRI_2014_08_15)
    test.adjust(SAT_2014_08_16, REF_DATA) should haveValue(SAT_2014_08_16)
    test.resolve(REF_DATA).map(adjuster => adjuster.adjust(SAT_2014_08_16)) should
      haveValue(SAT_2014_08_16)
  }

  //-------------------------------------------------------------------------
  test("test_ofCalendarDays1_oneDay") {
    val test: DaysAdjustment = DaysAdjustment.ofCalendarDays(1)

    test.days shouldBe 1
    test.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    test.adjustment shouldBe BDA_NONE
    test.toString shouldBe "1 calendar day"
  }

  test("test_ofCalendarDays1_threeDays") {
    val test: DaysAdjustment = DaysAdjustment.ofCalendarDays(3)

    test.days shouldBe 3
    test.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    test.adjustment shouldBe BDA_NONE
    test.toString shouldBe "3 calendar days"

    // The plural is "not one" rather than "more than one", and a negative count subtracts.
    DaysAdjustment.ofCalendarDays(0).toString shouldBe "0 calendar days"
    DaysAdjustment.ofCalendarDays(-1).toString shouldBe "-1 calendar days"
    DaysAdjustment.ofCalendarDays(-2).adjust(MON_2014_08_18, REF_DATA) should
      haveValue(SAT_2014_08_16)

    // Any count at all is held, at either extreme, because adding calendar days is arithmetic on
    // the date: no calendar is walked, so the size of the count decides nothing about what the
    // addition costs. The business-day factories are the ones that bound their count, and they
    // bound it for that reason and no other - see `test_ofBusinessDays2_threeDays`.
    DaysAdjustment.ofCalendarDays(Int.MaxValue).days shouldBe Int.MaxValue
    DaysAdjustment.ofCalendarDays(Int.MinValue).days shouldBe Int.MinValue
    DaysAdjustment.of(Int.MaxValue, HolidayCalendarIds.NO_HOLIDAYS, BDA_NONE) should
      haveValue(DaysAdjustment.ofCalendarDays(Int.MaxValue))
    DaysAdjustment.of(Int.MinValue, HolidayCalendarIds.NO_HOLIDAYS, BDA_FOLLOW_SAT_SUN) should
      haveValue(DaysAdjustment.ofCalendarDays(Int.MinValue, BDA_FOLLOW_SAT_SUN))
  }

  test("test_ofCalendarDays1_adjust") {
    val test: DaysAdjustment = DaysAdjustment.ofCalendarDays(2)
    val base: LocalDate = FRI_2014_08_15 // Fri

    // The addition takes no account of holidays or weekends, and no trailing adjustment follows.
    test.adjust(base, REF_DATA) should haveValue(SUN_2014_08_17) // Sun
    test.resolve(REF_DATA).map(adjuster => adjuster.adjust(base)) should haveValue(SUN_2014_08_17)
    test.toReader.run(REF_DATA).map(adjuster => adjuster.adjust(base)) should
      haveValue(SUN_2014_08_17)

    // One resolved adjuster serves any number of dates and agrees with resolving afresh for each.
    val resolvedOnce: Either[Failure, DateAdjuster] = test.resolve(REF_DATA)
    resolvedOnce should beSuccess
    resolvedOnce.map(adjuster => adjuster.adjust(MON_2014_08_18)) should haveValue(WED_2014_08_20)
    resolvedOnce.map(adjuster => adjuster.adjust(SAT_2014_08_16)) shouldBe
      test.adjust(SAT_2014_08_16, REF_DATA)
  }

  test("test_ofCalendarDays2_oneDay") {
    val test: DaysAdjustment = DaysAdjustment.ofCalendarDays(1, BDA_FOLLOW_SAT_SUN)

    test.days shouldBe 1
    // The addition is by calendar days whatever the trailing adjustment names, so the addition
    // calendar is the no-holidays identifier.
    test.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN
    test.toString shouldBe "1 calendar day then apply Following using calendar Sat/Sun"
  }

  test("test_ofCalendarDays2_fourDays") {
    val test: DaysAdjustment = DaysAdjustment.ofCalendarDays(4, BDA_FOLLOW_SAT_SUN)

    test.days shouldBe 4
    test.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN
    test.toString shouldBe "4 calendar days then apply Following using calendar Sat/Sun"
  }

  test("test_ofCalendarDays2_adjust") {
    val test: DaysAdjustment = DaysAdjustment.ofCalendarDays(2, BDA_FOLLOW_SAT_SUN)
    val base: LocalDate = FRI_2014_08_15 // Fri

    test.adjust(base, REF_DATA) should haveValue(MON_2014_08_18) // Mon
    test.resolve(REF_DATA).map(adjuster => adjuster.adjust(base)) should haveValue(MON_2014_08_18)
    test.toReader.run(REF_DATA).map(adjuster => adjuster.adjust(base)) should
      haveValue(MON_2014_08_18)

    // `resolve` resolves only the trailing adjustment's calendar and performs the addition as
    // plain date arithmetic, so it succeeds against a store that omits the no-holidays calendar
    // where `adjust` reports it missing. `ReferenceData.of`, `minimal` and `standard` lay that
    // entry in; a raw `ImmutableReferenceData.of` store such as the one below holds only what it
    // was given.
    SAT_SUN_ONLY_REF_DATA.containsValue(HolidayCalendarIds.NO_HOLIDAYS) shouldBe false
    test.resolve(SAT_SUN_ONLY_REF_DATA).map(adjuster => adjuster.adjust(base)) should
      haveValue(MON_2014_08_18)
    test.adjust(base, SAT_SUN_ONLY_REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)

    // A trailing calendar the reference data cannot supply fails all three entry points, by name.
    val unknown: DaysAdjustment =
      DaysAdjustment.ofCalendarDays(
        2,
        BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, UNKNOWN_CALENDAR))
    unknown.adjust(base, REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    unknown.resolve(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    unknown.toReader.run(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    unknown.adjust(base, REF_DATA) should
      haveFailureMessageMatching("Reference data not found for identifier 'XXXX'")
    noException should be thrownBy unknown.adjust(base, REF_DATA)
  }

  test("test_ofCalendarDays2_adjustHoliday") {
    val test: DaysAdjustment = DaysAdjustment.ofCalendarDays(2, BDA_FOLLOW_SAT_SUN)
    val base: LocalDate = SAT_2014_08_16 // Sat

    // The base date is itself a holiday, which the addition ignores.
    test.adjust(base, REF_DATA) should haveValue(MON_2014_08_18) // Mon
    test.resolve(REF_DATA).map(adjuster => adjuster.adjust(base)) should haveValue(MON_2014_08_18)
    test.toReader.run(REF_DATA).map(adjuster => adjuster.adjust(base)) should
      haveValue(MON_2014_08_18)

    // The other half: the addition lands on a holiday and the trailing adjustment moves it.
    val oneDay: DaysAdjustment = DaysAdjustment.ofCalendarDays(1, BDA_FOLLOW_SAT_SUN)
    oneDay.adjust(FRI_2014_08_15, REF_DATA) should haveValue(MON_2014_08_18)
    oneDay.resolve(REF_DATA).map(adjuster => adjuster.adjust(FRI_2014_08_15)) should
      haveValue(MON_2014_08_18)

    // Without the trailing adjustment the same addition stops on the Saturday.
    DaysAdjustment.ofCalendarDays(1).adjust(FRI_2014_08_15, REF_DATA) should
      haveValue(SAT_2014_08_16)
  }

  test("test_ofCalendarDays2_null") {
    // The trailing adjustment is a required `BusinessDayAdjustment`, so these do not compile.
    assertDoesNotCompile("""DaysAdjustment.ofCalendarDays(2, "Following")""")
    assertDoesNotCompile("DaysAdjustment.ofCalendarDays(2, HolidayCalendarIds.SAT_SUN)")
    assertDoesNotCompile("DaysAdjustment.ofCalendarDays(2, BDA_FOLLOW_SAT_SUN, BDA_NONE)")

    val test: DaysAdjustment = DaysAdjustment.ofCalendarDays(2, BDA_FOLLOW_SAT_SUN)
    test.days shouldBe 2
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN
  }

  //-------------------------------------------------------------------------
  test("test_ofBusinessDays2_oneDay") {
    val test: DaysAdjustment = DaysAdjustment.ofBusinessDays(1, HolidayCalendarIds.SAT_SUN)

    test.days shouldBe 1
    // The calendar given performs the addition, which is what makes these business days.
    test.calendar shouldBe HolidayCalendarIds.SAT_SUN
    test.adjustment shouldBe BDA_NONE
    test.toString shouldBe "1 business day using calendar Sat/Sun"
  }

  test("test_ofBusinessDays2_threeDays") {
    val test: DaysAdjustment = DaysAdjustment.ofBusinessDays(3, HolidayCalendarIds.SAT_SUN)

    test.days shouldBe 3
    test.calendar shouldBe HolidayCalendarIds.SAT_SUN
    test.adjustment shouldBe BDA_NONE
    test.toString shouldBe "3 business days using calendar Sat/Sun"

    // A negative count walks the calendar backwards; zero is rewritten - see test_ofBusinessDays0.
    DaysAdjustment.ofBusinessDays(-1, HolidayCalendarIds.SAT_SUN).toString shouldBe
      "-1 business days using calendar Sat/Sun"
    DaysAdjustment.ofBusinessDays(-2, HolidayCalendarIds.SAT_SUN)
      .adjust(TUE_2014_08_19, REF_DATA) should haveValue(FRI_2014_08_15)

    // The count has a bound, which the count of a calendar-day addition does not: adding
    // business days walks the calendar a day at a time looking for the next business day, so the
    // count is how much work the adjustment does. A count of two thousand million would walk two
    // thousand million days - through years no calendar holds data for, and for minutes - so a
    // count beyond what any calendar can be walked is refused, in either direction. It is refused
    // by raising, because these factories are total in the sense the ones being ported were: they
    // hold the fields they are given, so the only thing they say about them is a precondition of
    // the call. `DaysAdjustment.of` refuses the same count in its error channel, which is what
    // keeps the two routes in step - see `coverage_builder`.
    val refusesCount = (days: Int) =>
      intercept[IllegalArgumentException](
        DaysAdjustment.ofBusinessDays(days, HolidayCalendarIds.SAT_SUN))
        .getMessage should include(MagnitudeMessage)

    refusesCount(Int.MaxValue)
    // the smallest `Int` is the case a check written in `Int` arithmetic would let through, its
    // magnitude not being an `Int` at all: negating it overflows back to itself
    refusesCount(Int.MinValue)
    refusesCount(HolidayCalendar.MaxBusinessDayShift + 1)
    refusesCount(-HolidayCalendar.MaxBusinessDayShift - 1)

    // The bound itself is a count that is held, so the refusal is of what lies beyond it rather
    // than of a large count, and the value it builds is an ordinary adjustment.
    val atTheBound: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(HolidayCalendar.MaxBusinessDayShift, HolidayCalendarIds.SAT_SUN)
    atTheBound.days shouldBe HolidayCalendar.MaxBusinessDayShift
    atTheBound.calendar shouldBe HolidayCalendarIds.SAT_SUN
    DaysAdjustment.ofBusinessDays(-HolidayCalendar.MaxBusinessDayShift, HolidayCalendarIds.SAT_SUN)
      .days shouldBe -HolidayCalendar.MaxBusinessDayShift

    // A count that names no calendar to walk is not bounded by this factory either, the addition
    // then being arithmetic: zero is rewritten into a trailing adjustment, as
    // `test_ofBusinessDays0` describes, and the no-holidays calendar performs no search.
    DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.SAT_SUN).days shouldBe 0
    DaysAdjustment.ofBusinessDays(Int.MaxValue, HolidayCalendarIds.NO_HOLIDAYS).days shouldBe
      Int.MaxValue
  }

  test("test_ofBusinessDays2_adjust") {
    val test: DaysAdjustment = DaysAdjustment.ofBusinessDays(2, HolidayCalendarIds.SAT_SUN)
    val base: LocalDate = FRI_2014_08_15 // Fri

    // The weekend is skipped by the addition itself rather than by a trailing adjustment.
    test.adjust(base, REF_DATA) should haveValue(TUE_2014_08_19) // Tue
    test.resolve(REF_DATA).map(adjuster => adjuster.adjust(base)) should haveValue(TUE_2014_08_19)
    test.toReader.run(REF_DATA).map(adjuster => adjuster.adjust(base)) should
      haveValue(TUE_2014_08_19)

    // The London summer bank holiday of 2014 falls on Monday the 25th, so two London business
    // days after Friday the 22nd is the Wednesday where two Sat/Sun business days is the Tuesday.
    val london: DaysAdjustment = DaysAdjustment.ofBusinessDays(2, HolidayCalendarIds.GBLO)
    london.adjust(FRI_2014_08_22, REF_DATA) should haveValue(date(2014, 8, 27))
    test.adjust(FRI_2014_08_22, REF_DATA) should haveValue(TUE_2014_08_26)

    // An addition calendar the reference data cannot supply fails each of the three entry points.
    val unknown: DaysAdjustment = DaysAdjustment.ofBusinessDays(2, UNKNOWN_CALENDAR)
    unknown.adjust(base, REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    unknown.resolve(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    unknown.toReader.run(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    noException should be thrownBy unknown.resolve(REF_DATA)
  }

  test("test_ofBusinessDays2_null") {
    // The addition calendar is a required `HolidayCalendarId`; there is no one-argument factory.
    assertDoesNotCompile("DaysAdjustment.ofBusinessDays(2)")
    assertDoesNotCompile("DaysAdjustment.ofBusinessDays(2, BDA_FOLLOW_SAT_SUN)")
    assertDoesNotCompile("""DaysAdjustment.ofBusinessDays(2, "Sat/Sun")""")

    val test: DaysAdjustment = DaysAdjustment.ofBusinessDays(2, HolidayCalendarIds.SAT_SUN)
    test.days shouldBe 2
    test.calendar shouldBe HolidayCalendarIds.SAT_SUN
  }

  //-------------------------------------------------------------------------
  test("test_ofBusinessDays3_oneDay") {
    val test: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(1, HolidayCalendarIds.SAT_SUN, BDA_FOLLOW_WED_THU)

    test.days shouldBe 1
    test.calendar shouldBe HolidayCalendarIds.SAT_SUN
    test.adjustment shouldBe BDA_FOLLOW_WED_THU
    test.toString shouldBe
      "1 business day using calendar Sat/Sun then apply Following using calendar WedThu"
  }

  test("test_ofBusinessDays3_fourDays") {
    val test: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(4, HolidayCalendarIds.SAT_SUN, BDA_FOLLOW_WED_THU)

    test.days shouldBe 4
    test.calendar shouldBe HolidayCalendarIds.SAT_SUN
    test.adjustment shouldBe BDA_FOLLOW_WED_THU
    test.toString shouldBe
      "4 business days using calendar Sat/Sun then apply Following using calendar WedThu"

    // With a non-zero count this form holds exactly the fields it is given, and the validated
    // factory accepts those fields - which is where the two construction routes agree.
    DaysAdjustment.of(test.days, test.calendar, test.adjustment) should haveValue(test)

    // A count of zero is the one input it rewrites: there is no addition of zero business days,
    // so the addition calendar is dropped and the adjustment supplied is kept, while the
    // validated factory refuses the pairing outright.
    val zero: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.SAT_SUN, BDA_NONE)
    zero.days shouldBe 0
    zero.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    zero.adjustment shouldBe BDA_NONE
    zero shouldBe DaysAdjustment.NONE
    DaysAdjustment.of(0, HolidayCalendarIds.SAT_SUN, BDA_NONE) should
      beFailureWith(FailureReason.INVALID)

    // The two-argument form reads the same request as the rule it can mean, so the two zero-day
    // forms are different values: this one adjusts nothing, that one moves a holiday forwards.
    zero should not be DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.SAT_SUN)

    // This form bounds its count as the two-argument one does, and says the same thing about it,
    // because the count means the same thing in both: the days the addition walks the calendar
    // for. The trailing adjustment is no part of that, so it is refused whatever the adjustment.
    intercept[IllegalArgumentException](
      DaysAdjustment.ofBusinessDays(Int.MaxValue, HolidayCalendarIds.SAT_SUN, BDA_FOLLOW_WED_THU))
      .getMessage should include(MagnitudeMessage)
    intercept[IllegalArgumentException](
      DaysAdjustment.ofBusinessDays(Int.MinValue, HolidayCalendarIds.SAT_SUN, BDA_NONE))
      .getMessage should include(MagnitudeMessage)
    DaysAdjustment
      .ofBusinessDays(HolidayCalendar.MaxBusinessDayShift, HolidayCalendarIds.SAT_SUN, BDA_FOLLOW_WED_THU)
      .days shouldBe HolidayCalendar.MaxBusinessDayShift
  }

  test("test_ofBusinessDays3_adjust") {
    val test: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(3, HolidayCalendarIds.SAT_SUN, BDA_FOLLOW_WED_THU)
    val base: LocalDate = FRI_2014_08_15 // Fri

    // Three Saturday/Sunday business days after the Friday is the Wednesday, which is a holiday
    // of the Wednesday/Thursday calendar, so the trailing adjustment carries it over the Thursday
    // to the Friday: two calendars, each doing its own step.
    test.adjust(base, WED_THU_REF_DATA) should haveValue(FRI_2014_08_22)
    test.resolve(WED_THU_REF_DATA).map(adjuster => adjuster.adjust(base)) should
      haveValue(FRI_2014_08_22)
    test.toReader.run(WED_THU_REF_DATA).map(adjuster => adjuster.adjust(base)) should
      haveValue(FRI_2014_08_22)

    // `resolve` looks both calendars up once and binds them into the adjuster returned, so one
    // adjuster applied to several dates must agree with resolving afresh for each of them.
    val resolvedOnce: Either[Failure, DateAdjuster] = test.resolve(WED_THU_REF_DATA)
    val readOnce: Either[Failure, DateAdjuster] = test.toReader.run(WED_THU_REF_DATA)
    resolvedOnce should beSuccess
    readOnce should beSuccess
    resolvedOnce.map(adjuster => adjuster.adjust(base)) should haveValue(FRI_2014_08_22)
    resolvedOnce.map(adjuster => adjuster.adjust(WED_2014_08_20)) should haveValue(MON_2014_08_25)
    readOnce.map(adjuster => adjuster.adjust(WED_2014_08_20)) should haveValue(MON_2014_08_25)

    val dates: List[LocalDate] =
      Iterator
        .iterate(date(2014, 8, 1))(day => day.plusDays(1L))
        .takeWhile(day => day.getMonthValue == 8)
        .toList
    dates should have size 31
    dates.foreach { day =>
      withClue(s"$test adjusting $day: ") {
        val perDate: Either[Failure, LocalDate] = test.adjust(day, WED_THU_REF_DATA)
        perDate should beSuccess
        resolvedOnce.map(adjuster => adjuster.adjust(day)) shouldBe perDate
        readOnce.map(adjuster => adjuster.adjust(day)) shouldBe perDate
      }
    }

    // Two readers compose: adjustments are assembled while no reference data is available, and
    // the data is supplied once, to the composition, rather than to each of them.
    val pair: RefDataReader[(LocalDate, LocalDate)] =
      (test.toReader, DaysAdjustment.ofBusinessDays(1, HolidayCalendarIds.SAT_SUN).toReader)
        .mapN((twoCalendars, oneCalendar) =>
          (twoCalendars.adjust(base), oneCalendar.adjust(base)))
    pair.run(WED_THU_REF_DATA) should haveValue((FRI_2014_08_22, MON_2014_08_18))

    // The failure path, in both calendars: the built-in reference data knows nothing of `WedThu`,
    // an unresolvable addition calendar fails the same way, and a composition fails as a whole.
    test.adjust(base, REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    test.resolve(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    test.adjust(base, REF_DATA) should
      haveFailureMessageMatching("Reference data not found for identifier 'WedThu'")

    val unknownAddition: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(3, UNKNOWN_CALENDAR, BDA_FOLLOW_SAT_SUN)
    unknownAddition.adjust(base, WED_THU_REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    unknownAddition.resolve(WED_THU_REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)

    val partlyUnknown: RefDataReader[(LocalDate, LocalDate)] =
      (test.toReader, unknownAddition.toReader)
        .mapN((known, unknown) => (known.adjust(base), unknown.adjust(base)))
    partlyUnknown.run(WED_THU_REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
  }

  test("test_ofBusinessDays3_null") {
    // Calendar and adjustment are required parameters of their own types, so these do not compile.
    assertDoesNotCompile("DaysAdjustment.ofBusinessDays(3, BDA_FOLLOW_SAT_SUN, BDA_FOLLOW_SAT_SUN)")
    assertDoesNotCompile(
      "DaysAdjustment.ofBusinessDays(3, HolidayCalendarIds.SAT_SUN, HolidayCalendarIds.SAT_SUN)")
    assertDoesNotCompile("""DaysAdjustment.ofBusinessDays(3, "Sat/Sun", "Following")""")

    val test: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(3, HolidayCalendarIds.SAT_SUN, BDA_FOLLOW_SAT_SUN)
    test.days shouldBe 3
    test.calendar shouldBe HolidayCalendarIds.SAT_SUN
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN
  }

  //-------------------------------------------------------------------------
  test("test_ofBusinessDays0") {
    val test: DaysAdjustment = DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.SAT_SUN)

    // Adding zero business days names no day, so the two-argument factory reads the request as
    // "the next business day of this calendar, or this date if it already is one" and stores the
    // calendar in a `Following` adjustment: the fields are NOT the ones passed in.
    test.days shouldBe 0
    test.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN
    test.toString shouldBe "0 calendar days then apply Following using calendar Sat/Sun"

    // A business day is returned unaltered, and a holiday moves to the next business day.
    test.adjust(FRI_2014_08_15, REF_DATA) should haveValue(FRI_2014_08_15) // Fri
    test.adjust(SAT_2014_08_16, REF_DATA) should haveValue(MON_2014_08_18) // Sat -> Mon
    test.adjust(SUN_2014_08_17, REF_DATA) should haveValue(MON_2014_08_18) // Sun -> Mon
    test.adjust(MON_2014_08_18, REF_DATA) should haveValue(MON_2014_08_18) // Mon

    val resolvedOnce: Either[Failure, DateAdjuster] = test.resolve(REF_DATA)
    resolvedOnce should beSuccess
    List(FRI_2014_08_15, SAT_2014_08_16, SUN_2014_08_17, MON_2014_08_18).foreach { day =>
      withClue(s"$test adjusting $day: ") {
        val perDate: Either[Failure, LocalDate] = test.adjust(day, REF_DATA)
        resolvedOnce.map(adjuster => adjuster.adjust(day)) shouldBe perDate
        test.toReader.run(REF_DATA).map(adjuster => adjuster.adjust(day)) shouldBe perDate
      }
    }

    // The alternative reading is a different value: a zero-day addition carrying no trailing
    // adjustment adds nothing and adjusts nothing, so a holiday stays a holiday.
    val addsNothing: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.SAT_SUN, BDA_NONE)
    addsNothing should not be test
    addsNothing shouldBe DaysAdjustment.NONE
    addsNothing.adjust(SAT_2014_08_16, REF_DATA) should haveValue(SAT_2014_08_16)
    addsNothing.toString shouldBe "0 calendar days"

    // Dropping the addition calendar costs no computed date: a shift of zero business days
    // returns the date whichever calendar is asked, so the rewritten value computes what the
    // trailing adjustment alone computes, over sixty days spanning weekends and a bank holiday.
    val adjustmentAlone: BusinessDayAdjustment = BDA_FOLLOW_SAT_SUN
    val rewritten: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.GBLO, adjustmentAlone)
    rewritten.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    (0 until 60).foreach { offset =>
      val day: LocalDate = date(2014, 8, 1).plusDays(offset.toLong)
      withClue(s"the rewritten zero-day addition adjusting $day: ") {
        rewritten.adjust(day, REF_DATA) shouldBe adjustmentAlone.adjust(day, REF_DATA)
      }
    }

    // Where the trailing adjustment names no calendar either the calendar passed in is
    // unrecoverable, so `resultCalendar` answers no-holidays; otherwise it reads the adjustment.
    addsNothing.resultCalendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    rewritten.resultCalendar shouldBe HolidayCalendarIds.SAT_SUN

    // The calendar given is recoverable in spite of the rewrite, and the value is in normal form.
    test.resultCalendar shouldBe HolidayCalendarIds.SAT_SUN
    test.normalized shouldBe test

    // Named the no-holidays calendar, the rewrite yields a `Following` adjustment that is a no-op.
    val none: DaysAdjustment = DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.NO_HOLIDAYS)
    none.days shouldBe 0
    none.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    none.adjustment shouldBe
      BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.NO_HOLIDAYS)
    none.adjust(SAT_2014_08_16, REF_DATA) should haveValue(SAT_2014_08_16)

    // The London summer bank holiday of 2014 is a Monday, so zero London business days from the
    // Saturday before it is the Tuesday.
    DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.GBLO)
      .adjust(date(2014, 8, 23), REF_DATA) should haveValue(TUE_2014_08_26)
  }


  //-------------------------------------------------------------------------
  test("test_getResultCalendar1") {
    val test: DaysAdjustment = DaysAdjustment.ofBusinessDays(3, HolidayCalendarIds.SAT_SUN)

    // The addition walks this calendar and nothing follows it, so it had the last word.
    test.resultCalendar shouldBe HolidayCalendarIds.SAT_SUN

    test.adjust(FRI_2014_08_15, REF_DATA).map(adjusted =>
      HolidayCalendars.SAT_SUN.isBusinessDay(adjusted)) shouldBe Right(true)
  }

  test("test_getResultCalendar2") {
    val test: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(3, HolidayCalendarIds.SAT_SUN, BDA_FOLLOW_WED_THU)

    // Two calendars: the trailing adjustment's had the last word, so the result falls on it.
    test.resultCalendar shouldBe WED_THU
    test.resultCalendar should not be test.calendar

    test.adjust(FRI_2014_08_15, WED_THU_REF_DATA).map(adjusted =>
      WED_THU_CALENDAR.isBusinessDay(adjusted)) shouldBe Right(true)
  }

  test("test_getResultCalendar3") {
    val test: DaysAdjustment = DaysAdjustment.ofCalendarDays(3)

    // Neither step names a calendar, so the result is a plain date which may well be a weekend.
    test.resultCalendar shouldBe HolidayCalendarIds.NO_HOLIDAYS

    DaysAdjustment.ofBusinessDays(3, HolidayCalendarIds.SAT_SUN).resultCalendar shouldBe
      HolidayCalendarIds.SAT_SUN
    DaysAdjustment.ofCalendarDays(3).resultCalendar should not be
      DaysAdjustment.ofBusinessDays(3, HolidayCalendarIds.SAT_SUN).resultCalendar

    DaysAdjustment.ofCalendarDays(3, BDA_FOLLOW_SAT_SUN).resultCalendar shouldBe
      HolidayCalendarIds.SAT_SUN
    DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.SAT_SUN).resultCalendar shouldBe
      HolidayCalendarIds.SAT_SUN
    DaysAdjustment.NONE.resultCalendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
  }

  //-------------------------------------------------------------------------
  test("test_normalized") {
    val zeroDays: DaysAdjustment = DaysAdjustment.ofCalendarDays(0, BDA_FOLLOW_SAT_SUN)
    val zeroDaysWithCalendar: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(0, WED_THU, BDA_FOLLOW_SAT_SUN)
    val twoDays: DaysAdjustment = DaysAdjustment.ofCalendarDays(2, BDA_FOLLOW_SAT_SUN)
    val twoDaysWithCalendar: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(2, WED_THU, BDA_FOLLOW_SAT_SUN)
    val twoDaysWithSameCalendar: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(2, HolidayCalendarIds.SAT_SUN, BDA_FOLLOW_SAT_SUN)
    val twoDaysWithNoAdjust: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(2, HolidayCalendarIds.SAT_SUN)

    zeroDays.normalized shouldBe zeroDays
    // adding no days is the same calculation whichever calendar is asked to do it, so the
    // addition calendar is dropped
    zeroDaysWithCalendar.normalized shouldBe zeroDays
    twoDays.normalized shouldBe twoDays
    // two calendars that differ are both meaningful, so nothing is dropped
    twoDaysWithCalendar.normalized shouldBe twoDaysWithCalendar
    // a date reached by walking a calendar's business days already falls on one of them, so the
    // trailing adjustment against that same calendar is dropped
    twoDaysWithSameCalendar.normalized shouldBe twoDaysWithNoAdjust
    twoDaysWithNoAdjust.normalized shouldBe twoDaysWithNoAdjust

    // Normalising is idempotent and never changes what an adjustment computes.
    List(
      zeroDays,
      zeroDaysWithCalendar,
      twoDays,
      twoDaysWithCalendar,
      twoDaysWithSameCalendar,
      twoDaysWithNoAdjust,
      DaysAdjustment.NONE,
      DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.SAT_SUN)).foreach { adjustment =>
      withClue(s"$adjustment: ") {
        adjustment.normalized.normalized shouldBe adjustment.normalized
        List(FRI_2014_08_15, SAT_2014_08_16, SUN_2014_08_17, MON_2014_08_18).foreach { day =>
          adjustment.normalized.adjust(day, WED_THU_REF_DATA) shouldBe
            adjustment.adjust(day, WED_THU_REF_DATA)
        }
      }
    }

    // The normalised zero-day form is the calendar-day form and renders as one.
    zeroDaysWithCalendar.normalized.toString shouldBe
      "0 calendar days then apply Following using calendar Sat/Sun"
    zeroDaysWithCalendar.normalized.resultCalendar shouldBe HolidayCalendarIds.SAT_SUN
  }

  //-------------------------------------------------------------------------
  test("equals") {
    val a: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(3, HolidayCalendarIds.NO_HOLIDAYS, BDA_FOLLOW_SAT_SUN)
    val b: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(4, HolidayCalendarIds.NO_HOLIDAYS, BDA_FOLLOW_SAT_SUN)
    val c: DaysAdjustment = DaysAdjustment.ofBusinessDays(3, WED_THU, BDA_FOLLOW_SAT_SUN)
    val d: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(3, HolidayCalendarIds.NO_HOLIDAYS, BDA_FOLLOW_WED_THU)

    a.equals(b) shouldBe false
    a.equals(c) shouldBe false
    a.equals(d) shouldBe false

    // equality is by field, so a value built twice is one value, with one hash, and `Hash` agrees
    val same: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(3, HolidayCalendarIds.NO_HOLIDAYS, BDA_FOLLOW_SAT_SUN)
    a shouldBe same
    a.hashCode shouldBe same.hashCode
    Hash[DaysAdjustment].eqv(a, same) shouldBe true
    Hash[DaysAdjustment].hash(a) shouldBe Hash[DaysAdjustment].hash(same)
    Hash[DaysAdjustment].eqv(a, b) shouldBe false
    Hash[DaysAdjustment].eqv(a, c) shouldBe false
    Hash[DaysAdjustment].eqv(a, d) shouldBe false

    // a value of another type, asserted through the method because the types are unrelated
    a.equals("3 calendar days then apply Following using calendar Sat/Sun") shouldBe false

    // Two adjustments that compute the same dates while holding different fields are deliberately
    // not equal; `normalized` is how a caller asks for the representative form before comparing.
    // The pair below is such a case: the adjustment repeats the calendar that did the addition.
    val sameBehaviour: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(3, WED_THU, BDA_FOLLOW_WED_THU)
    val normalForm: DaysAdjustment = DaysAdjustment.ofBusinessDays(3, WED_THU)
    sameBehaviour should not be normalForm
    sameBehaviour.normalized shouldBe normalForm

    // A zero-day addition naming a calendar never reaches `normalized`, because no value holds
    // those fields: the factory drops the calendar and the validated factory refuses the pairing.
    DaysAdjustment.ofBusinessDays(0, WED_THU, BDA_FOLLOW_SAT_SUN) shouldBe
      DaysAdjustment.ofCalendarDays(0, BDA_FOLLOW_SAT_SUN)
    DaysAdjustment.of(0, WED_THU, BDA_FOLLOW_SAT_SUN) should beFailureWith(FailureReason.INVALID)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    val test: DaysAdjustment = DaysAdjustment.ofCalendarDays(4, BDA_FOLLOW_SAT_SUN)
    val other: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(4, HolidayCalendarIds.SAT_SUN, BDA_FOLLOW_WED_THU)

    test.days shouldBe 4
    test.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN
    other.days shouldBe 4
    other.calendar shouldBe HolidayCalendarIds.SAT_SUN
    other.adjustment shouldBe BDA_FOLLOW_WED_THU

    test shouldBe DaysAdjustment.ofCalendarDays(4, BDA_FOLLOW_SAT_SUN)
    test should not be other
    Hash[DaysAdjustment].eqv(test, DaysAdjustment.ofCalendarDays(4, BDA_FOLLOW_SAT_SUN)) shouldBe true
    Hash[DaysAdjustment].eqv(test, other) shouldBe false
    Hash[DaysAdjustment].hash(test) shouldBe test.hashCode
    test.equals(other.toString) shouldBe false

    // `toString` and `Show` are the two ways of rendering an adjustment and must not differ
    test.toString shouldBe "4 calendar days then apply Following using calendar Sat/Sun"
    Show[DaysAdjustment].show(test) shouldBe test.toString
    Show[DaysAdjustment].show(other) shouldBe
      "4 business days using calendar Sat/Sun then apply Following using calendar WedThu"
    Show[DaysAdjustment].show(DaysAdjustment.NONE) shouldBe "0 calendar days"

    // `unapply` is the read path; there is no `copy` to pair it with.
    DaysAdjustment.unapply(test) shouldBe
      Some((4, HolidayCalendarIds.NO_HOLIDAYS, BDA_FOLLOW_SAT_SUN))
    DaysAdjustment.unapply(other) shouldBe
      Some((4, HolidayCalendarIds.SAT_SUN, BDA_FOLLOW_WED_THU))
  }

  test("coverage_builder") {
    // This type publishes no `with*` method, so a value is built through a factory.
    val test: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(1, HolidayCalendarIds.SAT_SUN, BDA_FOLLOW_WED_THU)

    test.days shouldBe 1
    test.calendar shouldBe HolidayCalendarIds.SAT_SUN
    test.adjustment shouldBe BDA_FOLLOW_WED_THU

    // Rebuilding from the fields of a value returns that value; each change is visible in it.
    DaysAdjustment.ofBusinessDays(test.days, test.calendar, test.adjustment) shouldBe test
    DaysAdjustment.ofBusinessDays(2, test.calendar, test.adjustment).days shouldBe 2
    DaysAdjustment.ofBusinessDays(test.days, WED_THU, test.adjustment).calendar shouldBe WED_THU
    DaysAdjustment.ofBusinessDays(test.days, test.calendar, BDA_NONE).adjustment shouldBe BDA_NONE
    DaysAdjustment.ofBusinessDays(test.days, test.calendar, BDA_NONE).toString shouldBe
      "1 business day using calendar Sat/Sun"

    // The two-argument factories are the three-argument one with a field defaulted.
    DaysAdjustment.ofBusinessDays(1, HolidayCalendarIds.SAT_SUN) shouldBe
      DaysAdjustment.ofBusinessDays(1, HolidayCalendarIds.SAT_SUN, BDA_NONE)
    DaysAdjustment.ofCalendarDays(1) shouldBe
      DaysAdjustment.ofBusinessDays(1, HolidayCalendarIds.NO_HOLIDAYS, BDA_NONE)
    DaysAdjustment.ofCalendarDays(1, BDA_FOLLOW_WED_THU) shouldBe
      DaysAdjustment.ofBusinessDays(1, HolidayCalendarIds.NO_HOLIDAYS, BDA_FOLLOW_WED_THU)

    // The validated factory is the route for fields a caller did not choose, and it accepts every
    // combination describing an adjustment: a negative count, a composite calendar, any convention.
    DaysAdjustment.of(test.days, test.calendar, test.adjustment) should haveValue(test)
    DaysAdjustment.of(-2, HolidayCalendarIds.SAT_SUN, BDA_NONE) should
      haveValue(DaysAdjustment.ofBusinessDays(-2, HolidayCalendarIds.SAT_SUN))
    DaysAdjustment.of(
      3,
      HolidayCalendarIds.GBLO.combinedWith(HolidayCalendarIds.USNY),
      BDA_FOLLOW_WED_THU) should beSuccess
    DaysAdjustment.of(0, HolidayCalendarIds.NO_HOLIDAYS, BDA_NONE) should
      haveValue(DaysAdjustment.NONE)

    // It refuses two things about a count, and nothing else. The first is the pairing the class
    // documents as naming no day: a count of zero against a calendar that would have performed
    // the addition. The failure says so, and it is a single cause, the three fields having
    // nothing else about them to be wrong.
    val zeroDaysNamingACalendar: ResultNec[DaysAdjustment] =
      DaysAdjustment.of(0, WED_THU, BDA_FOLLOW_SAT_SUN)
    zeroDaysNamingACalendar should beFailureWith(FailureReason.INVALID)
    zeroDaysNamingACalendar should haveFailureMessageMatching(
      "A business day addition of zero days names no day.*WedThu.*")
    zeroDaysNamingACalendar.left.toOption.map(failures => failures.length) shouldBe Some(1L)

    // That message names the identifier it rejected, and an identifier is arbitrary text:
    // `HolidayCalendarId.of` is total and accepts any string, line breaks and any length
    // included. The failure carries that text as supplied, while its rendering is one line,
    // escapes what a line-oriented reader could act on, and is bounded however long the name was.
    val forgedName: String = "GBLO\nWARN  the addition succeeded\u2028and again"
    val forgedLines: ResultNec[DaysAdjustment] =
      DaysAdjustment.of(0, HolidayCalendarId.of(forgedName), BDA_NONE)
    val forgedFailure: Failure =
      forgedLines.left.toOption
        .map(failures => failures.head)
        .getOrElse(fail("a zero-day addition naming a calendar should have been refused"))
    forgedFailure.message should include(forgedName)
    val forgedRendering: String = Show[Failure].show(forgedFailure)
    forgedRendering should include("GBLO\\n")
    forgedRendering should include("\\u2028")
    forgedRendering.linesIterator.size shouldBe 1

    val overLongName: String = "Z" * 4096
    val overLong: ResultNec[DaysAdjustment] =
      DaysAdjustment.of(0, HolidayCalendarId.of(overLongName), BDA_NONE)
    val overLongFailure: Failure =
      overLong.left.toOption
        .map(failures => failures.head)
        .getOrElse(fail("a zero-day addition naming a calendar should have been refused"))
    overLongFailure.message should include(overLongName)
    val overLongRendering: String = Show[Failure].show(overLongFailure)
    overLongRendering should not include overLongName
    overLongRendering should include("...")
    overLongRendering.length should be < overLongName.length

    // The second is a count of business days beyond what any calendar can be walked. Adding
    // business days searches the calendar a day at a time, so the count is the work the
    // adjustment does, and a document naming two thousand million of them describes an
    // adjustment that would spend minutes walking through years no calendar holds data for. It
    // is refused here rather than raised, because these three fields are data - the route that
    // holds them as given raises instead, and the two agree about what is out of range.
    val unwalkableCount: ResultNec[DaysAdjustment] =
      DaysAdjustment.of(Int.MaxValue, WED_THU, BDA_NONE)
    unwalkableCount should beFailureWith(FailureReason.INVALID)
    unwalkableCount should haveFailureMessageMatching(s"$MagnitudeMessage.*")
    unwalkableCount.left.toOption.map(failures => failures.length) shouldBe Some(1L)
    DaysAdjustment.of(Int.MinValue, WED_THU, BDA_FOLLOW_SAT_SUN) should
      beFailureWith(FailureReason.INVALID)
    DaysAdjustment.of(HolidayCalendar.MaxBusinessDayShift + 1, WED_THU, BDA_NONE) should
      beFailureWith(FailureReason.INVALID)
    intercept[IllegalArgumentException](
      DaysAdjustment.ofBusinessDays(Int.MaxValue, WED_THU)).getMessage should include(
      MagnitudeMessage)

    // The bound is a count it accepts, and a count that names no calendar to walk is not bounded
    // at all, the addition then being arithmetic on the date rather than a search.
    DaysAdjustment.of(HolidayCalendar.MaxBusinessDayShift, WED_THU, BDA_NONE) should
      haveValue(DaysAdjustment.ofBusinessDays(HolidayCalendar.MaxBusinessDayShift, WED_THU))
    DaysAdjustment.of(-HolidayCalendar.MaxBusinessDayShift, WED_THU, BDA_FOLLOW_SAT_SUN) should
      beSuccess
    DaysAdjustment.of(Int.MaxValue, HolidayCalendarIds.NO_HOLIDAYS, BDA_FOLLOW_SAT_SUN) should
      beSuccess

    // The two refusals cannot both apply to one set of fields - a count of zero is inside every
    // bound and an out-of-range count is not zero - so there is no pairing of them to accumulate,
    // and each failure carries the one cause asserted above.

    // No factory builds either of the refused shapes, which is what makes the refusals statements
    // about the type rather than about one route into it: every value any factory produces is
    // accepted by `of`, each zero-day value names no addition calendar, and every count a factory
    // held is within the bound because the factory checked it.
    List(
      DaysAdjustment.NONE,
      DaysAdjustment.ofCalendarDays(0),
      DaysAdjustment.ofCalendarDays(0, BDA_FOLLOW_SAT_SUN),
      DaysAdjustment.ofCalendarDays(4, BDA_FOLLOW_WED_THU),
      DaysAdjustment.ofCalendarDays(Int.MaxValue),
      DaysAdjustment.ofBusinessDays(0, WED_THU),
      DaysAdjustment.ofBusinessDays(0, WED_THU, BDA_FOLLOW_SAT_SUN),
      DaysAdjustment.ofBusinessDays(4, WED_THU),
      DaysAdjustment.ofBusinessDays(HolidayCalendar.MaxBusinessDayShift, WED_THU),
      DaysAdjustment.ofBusinessDays(-4, WED_THU, BDA_FOLLOW_SAT_SUN)).foreach { built =>
      withClue(s"the fields of '$built' read back through the validated factory: ") {
        DaysAdjustment.of(built.days, built.calendar, built.adjustment) should haveValue(built)
        if (built.days == 0) {
          built.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
        }
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // The document is pinned here: the three fields under the names the class declares, in
    // declaration order, the count a number and each calendar the bare string its type publishes.
    val test: DaysAdjustment = DaysAdjustment.ofCalendarDays(4, BDA_FOLLOW_SAT_SUN)
    val encoded: Json = test.asJson

    encoded.asObject.map(obj => obj.keys.toList) shouldBe Some(List("days", "calendar", "adjustment"))
    encoded.noSpaces shouldBe
      """{"days":4,"calendar":"NoHolidays","adjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""

    // No field is optional and the encoder drops absent values, so no null appears in a document.
    encoded.noSpaces should not include "null"
    DaysAdjustment.NONE.asJson.noSpaces shouldBe
      """{"days":0,"calendar":"NoHolidays","adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}"""

    // The round trip, for the constant, the two-calendar form and a composite calendar.
    decode[DaysAdjustment](encoded.noSpaces) shouldBe Right(test)
    decode[DaysAdjustment](DaysAdjustment.NONE.asJson.noSpaces) shouldBe Right(DaysAdjustment.NONE)
    val twoCalendars: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(3, HolidayCalendarIds.SAT_SUN, BDA_FOLLOW_WED_THU)
    decode[DaysAdjustment](twoCalendars.asJson.noSpaces) shouldBe Right(twoCalendars)
    val composite: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(
        2,
        HolidayCalendarIds.GBLO.combinedWith(HolidayCalendarIds.USNY),
        BDA_FOLLOW_SAT_SUN)
    decode[DaysAdjustment](composite.asJson.noSpaces) shouldBe Right(composite)
    composite.asJson.noSpaces should include(""""calendar":"GBLO+USNY"""")

    // Equal values encode to identical bytes whichever factory built them.
    DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.SAT_SUN).asJson.noSpaces shouldBe
      DaysAdjustment.ofCalendarDays(0, BDA_FOLLOW_SAT_SUN).asJson.noSpaces

    // Decoding hands the fields to the validated factory, so a document describing the one
    // pairing no adjustment has - a count of zero against a named addition calendar - is refused
    // rather than wrapped. No encoder here writes one, so the refusal reaches hand-written input.
    val zeroDaysNamingACalendar: String =
      """{"days":0,"calendar":"Sat/Sun","adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}"""
    decode[DaysAdjustment](zeroDaysNamingACalendar).isLeft shouldBe true
    decode[DaysAdjustment](zeroDaysNamingACalendar).left.map(_.getMessage) match {
      case Left(message) => message should include("zero")
      case Right(value) => fail(s"the document should have been refused but decoded to $value")
    }

    // The calendar of such a document is arbitrary text and the refusal names it; the message is
    // built in one place, so the bounded single-line rendering asserted above holds here too.
    val forgedInDocument: String =
      """{"days":0,"calendar":"GBLO\nWARN  decoded","adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}"""
    decode[DaysAdjustment](forgedInDocument).left.map(_.getMessage) match {
      case Left(message) =>
        message should include("GBLO\\nWARN  decoded")
        message.linesIterator.size shouldBe 1
      case Right(value) => fail(s"the document should have been refused but decoded to $value")
    }
    val longInDocument: String = "Z" * 4096
    decode[DaysAdjustment](
      s"""{"days":0,"calendar":"$longInDocument","adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}""")
      .left.map(_.getMessage) match {
      case Left(message) =>
        message should not include longInDocument
        message should include("Z" * 256)
        message should include("...")
        message.length should be < longInDocument.length
      case Right(value) => fail(s"the document should have been refused but decoded to $value")
    }
    val zeroDays: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.SAT_SUN, BDA_NONE)
    zeroDays.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    decode[DaysAdjustment](zeroDays.asJson.noSpaces) shouldBe Right(zeroDays)

    // The documents below vary one part of a payload at a time; the shared parts are named here.
    val noAdjustObject: String = """{"convention":"NoAdjust","calendar":"NoHolidays"}"""
    def document(days: String, calendar: String, adjustment: String): String =
      s"""{"days":$days,"calendar":"$calendar","adjustment":$adjustment}"""

    // Beyond that one pairing the decoder rejects the shape of the payload: every field is
    // required, the count a number, an adjustment an object. A failure is a `Left`, not a throw.
    decode[DaysAdjustment](s"""{"calendar":"NoHolidays","adjustment":$noAdjustObject}""")
      .isLeft shouldBe true
    decode[DaysAdjustment](s"""{"days":4,"adjustment":$noAdjustObject}""").isLeft shouldBe true
    decode[DaysAdjustment]("""{"days":4,"calendar":"NoHolidays"}""").isLeft shouldBe true
    decode[DaysAdjustment](document("\"four\"", "NoHolidays", noAdjustObject))
      .isLeft shouldBe true
    decode[DaysAdjustment](document("true", "NoHolidays", noAdjustObject)).isLeft shouldBe true
    decode[DaysAdjustment]("{}").isLeft shouldBe true
    Json.fromString("4 calendar days").as[DaysAdjustment].isLeft shouldBe true

    // The count is read by the integer decoder of the JSON library, which this codec does not
    // override and which reads a JSON string holding an integer as that integer.
    decode[DaysAdjustment](document("\"4\"", "NoHolidays", noAdjustObject)) shouldBe
      Right(DaysAdjustment.ofCalendarDays(4))

    // The convention is read by the name lookup of its own closed family, so unknown text is
    // rejected here; an unknown calendar decodes and fails only when it is resolved.
    decode[DaysAdjustment](
      document("4", "NoHolidays", """{"convention":"Rubbish","calendar":"Sat/Sun"}"""))
      .isLeft shouldBe true
    val unknown: Either[io.circe.Error, DaysAdjustment] =
      decode[DaysAdjustment](document("4", "XXXX", noAdjustObject))
    unknown shouldBe
      Right(DaysAdjustment.ofBusinessDays(4, UNKNOWN_CALENDAR, BDA_NONE))
    unknown.map(adjustment => adjustment.adjust(FRI_2014_08_15, REF_DATA).isLeft) shouldBe Right(true)
  }
}
