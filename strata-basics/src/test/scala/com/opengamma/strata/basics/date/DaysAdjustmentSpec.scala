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
 * Test [[DaysAdjustment]], ported from the Java `DaysAdjustmentTest`.
 *
 * All twenty-six methods of the Java class are kept, each under the name the Java method had -
 * including the bare `equals` and the four `*_null` names - so that a Java test method and a test
 * of this suite stay in one-to-one correspondence and the method-level traceability the migration
 * manifest records resolves on the pair of suite class and test name. Nothing is added under a
 * name of its own: everything asserted beyond the Java assertions belongs to whichever of the
 * twenty-six methods already owned that ground.
 *
 * ===How the shape of the port changes the assertions===
 *
 *   - `adjust` and `resolve` return `Either[Failure, _]` where the Java methods returned a bare
 *     value and threw `ReferenceDataNotFoundException` for a calendar the reference data does not
 *     hold. Every ported assertion is therefore that the outcome is a success carrying the Java
 *     value, and the failure path - which the Java class never exercised, having only an
 *     exception to exercise it with - is asserted by the reason the failure carries rather than
 *     by its message, in the `_adjust` test of each family.
 *   - Java's `_adjust` methods asserted two paths, `adjust(date, refData)` and
 *     `resolve(refData).adjust(date)`. This port has a third, `toReader.run(refData)`, which is
 *     the same resolution expressed as a value awaiting reference data. `test_ofBusinessDays3_adjust`
 *     drives all three over the two-calendar form, applies one resolved adjuster to several dates
 *     - which is what resolving once is for - and composes two readers, because supplying
 *     reference data once, later, or in composition must not change what is computed.
 *   - The four `*_null` methods asserted that a null argument raises `IllegalArgumentException`.
 *     The `notNull` family of `ArgChecker` has no counterpart in this port, deliberately: a
 *     required argument of a reference type is required by the signature, so the case those six
 *     Java assertions covered cannot be written. Each of the four tests keeps its name and
 *     proves that at compile time instead, with a well-typed call alongside as the positive
 *     control.
 *   - `coverage` called `coverImmutableBean`, a reflective sweep of a Joda bean's properties,
 *     equality, hashing and rendering. There is no bean and no reflection in this port, so what
 *     the sweep stood for is asserted directly over the same value the Java method swept.
 *   - `coverage_builder` used the generated Joda builder. Builders are gone from this port -
 *     factories and `with*` methods replace them - and this type publishes no `with*` method,
 *     construction being a choice between four factories rather than a field at a time. The test
 *     asserts instead that an adjustment rebuilt from the fields of another is that other, which
 *     is the property a builder test was checking.
 *   - `test_serialization` asserted Joda-Beans binary and JSON round trips. Joda wire
 *     compatibility is out of scope for this port, so the test is a circe round trip through the
 *     derived codec, which additionally pins the concrete document: the three Java property
 *     names, in declaration order.
 *
 * ===What this file does not own===
 *
 * The per-type invalid-input matrix of a validated type lives in `SmartConstructorSpec`, one case
 * per failable method in `FailableSurfaceSpec`, the compile-time proofs that no `apply` or `copy`
 * exists in `ApiSurfaceSpec`, and the property-based codec round trip over every codec-bearing
 * type of the module in `json.JsonRoundTripSpec`. This file is the behaviour of this one type.
 *
 * @see [[BusinessDayAdjustmentSpec]] for the second step of the calculation
 * @see [[HolidayCalendarIdSpec]] for the calendar resolution both steps delegate to
 */
class DaysAdjustmentSpec extends AnyFunSuite with Matchers {

  /** The reference data the Java class used throughout, which holds every built-in calendar. */
  private val REF_DATA: ReferenceData = ReferenceData.standard

  /** The identifier of the Java class's fictional calendar, whose weekend is Wednesday and Thursday. */
  private val WED_THU: HolidayCalendarId = HolidayCalendarId.of("WedThu")

  private val BDA_NONE: BusinessDayAdjustment = BusinessDayAdjustment.NONE

  private val BDA_FOLLOW_SAT_SUN: BusinessDayAdjustment =
    BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.SAT_SUN)

  private val BDA_FOLLOW_WED_THU: BusinessDayAdjustment =
    BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, WED_THU)

  /**
   * The calendar the Java class built for `WedThu`: no holidays of its own and a weekend of
   * Wednesday and Thursday, which is what makes the two-calendar form observable - a date reached
   * by walking Saturday/Sunday business days can still be a holiday here.
   */
  private val WED_THU_CALENDAR: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(WED_THU, Nil, DayOfWeek.WEDNESDAY, DayOfWeek.THURSDAY)

  /**
   * The Java class's `ImmutableReferenceData.of(ImmutableMap.of(WED_THU, cal)).combinedWith(REF_DATA)`,
   * written with the single-entry raw store this port supplies: the fictional calendar first, the
   * built-in set behind it.
   */
  private val WED_THU_REF_DATA: ReferenceData =
    ImmutableReferenceData.of(WED_THU, WED_THU_CALENDAR).combinedWith(REF_DATA)

  /**
   * A store holding the weekend calendar and nothing else - in particular not the no-holidays
   * calendar - which is what separates `adjust` from `resolve` for a calendar-day addition.
   */
  private val SAT_SUN_ONLY_REF_DATA: ReferenceData =
    ImmutableReferenceData.of(HolidayCalendarIds.SAT_SUN, HolidayCalendars.SAT_SUN)

  /** An identifier no reference data of this library holds, used to observe the failure path. */
  private val UNKNOWN_CALENDAR: HolidayCalendarId = HolidayCalendarId.of("XXXX")

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

    // The constant is the value the calendar-day factory builds for zero days - equality is by
    // field, so the constant is not privileged - and `Show` renders what `toString` renders.
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
    // one day, singular, and no trailing adjustment to name
    test.toString shouldBe "1 calendar day"
  }

  test("test_ofCalendarDays1_threeDays") {
    val test: DaysAdjustment = DaysAdjustment.ofCalendarDays(3)

    test.days shouldBe 3
    test.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    test.adjustment shouldBe BDA_NONE
    test.toString shouldBe "3 calendar days"

    // The plural is chosen by "not one" rather than by "more than one", as in the library being
    // ported, so zero and a negative count are plural too. A negative count is meaningful here:
    // the days are added, and adding a negative number of them subtracts.
    DaysAdjustment.ofCalendarDays(0).toString shouldBe "0 calendar days"
    DaysAdjustment.ofCalendarDays(-1).toString shouldBe "-1 calendar days"
    DaysAdjustment.ofCalendarDays(-2).adjust(MON_2014_08_18, REF_DATA) should
      haveValue(SAT_2014_08_16)
  }

  test("test_ofCalendarDays1_adjust") {
    val test: DaysAdjustment = DaysAdjustment.ofCalendarDays(2)
    val base: LocalDate = FRI_2014_08_15 // Fri

    // Two calendar days after a Friday is the Sunday: holidays and weekends are not taken into
    // account by the addition, and there is no trailing adjustment to move the result off it.
    test.adjust(base, REF_DATA) should haveValue(SUN_2014_08_17) // Sun
    test.resolve(REF_DATA).map(adjuster => adjuster.adjust(base)) should haveValue(SUN_2014_08_17)
    test.toReader.run(REF_DATA).map(adjuster => adjuster.adjust(base)) should
      haveValue(SUN_2014_08_17)

    // One resolved adjuster serves any number of dates, which is the reason `resolve` exists, and
    // it must agree with resolving afresh for each of them.
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
    // calendar is the no-holidays identifier and only the adjustment knows about the weekend.
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

    // Two calendar days after the Friday is the Sunday, which the trailing adjustment then moves
    // forwards to the Monday - the two steps, each doing its own part.
    test.adjust(base, REF_DATA) should haveValue(MON_2014_08_18) // Mon
    test.resolve(REF_DATA).map(adjuster => adjuster.adjust(base)) should haveValue(MON_2014_08_18)
    test.toReader.run(REF_DATA).map(adjuster => adjuster.adjust(base)) should
      haveValue(MON_2014_08_18)

    // `resolve` resolves the trailing adjustment's calendar and performs the addition as plain
    // date arithmetic, so it succeeds against a store that holds the weekend calendar and not the
    // no-holidays one, where `adjust` - which asks the addition calendar to shift the date -
    // reports that calendar missing. Both behaviours are those of the library being ported, and
    // every store this library builds holds the no-holidays entry, so the two agree in practice.
    SAT_SUN_ONLY_REF_DATA.containsValue(HolidayCalendarIds.NO_HOLIDAYS) shouldBe false
    test.resolve(SAT_SUN_ONLY_REF_DATA).map(adjuster => adjuster.adjust(base)) should
      haveValue(MON_2014_08_18)
    test.adjust(base, SAT_SUN_ONLY_REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)

    // The failure path of the trailing adjustment, which the Java class never exercised: a
    // calendar the reference data cannot supply is reported rather than raised, through each of
    // the three entry points, and the failure names the identifier.
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

    // The base date is itself a holiday, which the addition ignores - two calendar days after the
    // Saturday is the Monday, and the trailing adjustment leaves a business day alone.
    test.adjust(base, REF_DATA) should haveValue(MON_2014_08_18) // Mon
    test.resolve(REF_DATA).map(adjuster => adjuster.adjust(base)) should haveValue(MON_2014_08_18)
    test.toReader.run(REF_DATA).map(adjuster => adjuster.adjust(base)) should
      haveValue(MON_2014_08_18)

    // The other half of the same ground: the addition lands on a holiday and the trailing
    // adjustment is what moves it. One calendar day after the Friday is the Saturday, which
    // `Following` carries to the Monday.
    val oneDay: DaysAdjustment = DaysAdjustment.ofCalendarDays(1, BDA_FOLLOW_SAT_SUN)
    oneDay.adjust(FRI_2014_08_15, REF_DATA) should haveValue(MON_2014_08_18)
    oneDay.resolve(REF_DATA).map(adjuster => adjuster.adjust(FRI_2014_08_15)) should
      haveValue(MON_2014_08_18)

    // Without the trailing adjustment the same addition stops on the Saturday, which is what the
    // adjustment is there to prevent.
    DaysAdjustment.ofCalendarDays(1).adjust(FRI_2014_08_15, REF_DATA) should
      haveValue(SAT_2014_08_16)
  }

  test("test_ofCalendarDays2_null") {
    // The Java method asserted `ofCalendarDays(2, null)` raises `IllegalArgumentException`,
    // through the `notNull` validation of the bean being ported. That validation has no
    // counterpart here and needs none: the second parameter is a `BusinessDayAdjustment`, so a
    // call that omits it - or that passes something which is not one - is rejected when this file
    // is compiled rather than when it runs, and the state the Java test guarded against cannot be
    // reached. These are the compile-time proofs of that.
    assertDoesNotCompile("""DaysAdjustment.ofCalendarDays(2, "Following")""")
    assertDoesNotCompile("DaysAdjustment.ofCalendarDays(2, HolidayCalendarIds.SAT_SUN)")
    assertDoesNotCompile("DaysAdjustment.ofCalendarDays(2, BDA_FOLLOW_SAT_SUN, BDA_NONE)")

    // The positive control: the well-typed call the proofs above are the complement of.
    val test: DaysAdjustment = DaysAdjustment.ofCalendarDays(2, BDA_FOLLOW_SAT_SUN)
    test.days shouldBe 2
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN
  }

  //-------------------------------------------------------------------------
  test("test_ofBusinessDays2_oneDay") {
    val test: DaysAdjustment = DaysAdjustment.ofBusinessDays(1, HolidayCalendarIds.SAT_SUN)

    test.days shouldBe 1
    // The calendar given performs the addition, which is what makes these business days, and no
    // trailing adjustment is needed because the addition already lands on a business day of it.
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

    // As with calendar days, the plural is "not one", and a negative count walks the calendar
    // backwards. Zero is the one count this factory does not hold as given - see
    // `test_ofBusinessDays0`.
    DaysAdjustment.ofBusinessDays(-1, HolidayCalendarIds.SAT_SUN).toString shouldBe
      "-1 business days using calendar Sat/Sun"
    DaysAdjustment.ofBusinessDays(-2, HolidayCalendarIds.SAT_SUN)
      .adjust(TUE_2014_08_19, REF_DATA) should haveValue(FRI_2014_08_15)
  }

  test("test_ofBusinessDays2_adjust") {
    val test: DaysAdjustment = DaysAdjustment.ofBusinessDays(2, HolidayCalendarIds.SAT_SUN)
    val base: LocalDate = FRI_2014_08_15 // Fri

    // Two business days after the Friday is the Tuesday: the weekend is skipped by the addition
    // itself rather than by a trailing adjustment.
    test.adjust(base, REF_DATA) should haveValue(TUE_2014_08_19) // Tue
    test.resolve(REF_DATA).map(adjuster => adjuster.adjust(base)) should haveValue(TUE_2014_08_19)
    test.toReader.run(REF_DATA).map(adjuster => adjuster.adjust(base)) should
      haveValue(TUE_2014_08_19)

    // A calendar with holidays of its own, which a weekend-only calendar cannot show: the London
    // summer bank holiday of 2014 falls on Monday the 25th, so two London business days after
    // Friday the 22nd is Wednesday the 27th rather than Tuesday the 26th.
    val london: DaysAdjustment = DaysAdjustment.ofBusinessDays(2, HolidayCalendarIds.GBLO)
    london.adjust(FRI_2014_08_22, REF_DATA) should haveValue(date(2014, 8, 27))
    test.adjust(FRI_2014_08_22, REF_DATA) should haveValue(TUE_2014_08_26)

    // The failure path: an addition calendar the reference data cannot supply is reported rather
    // than raised, through each of the three entry points.
    val unknown: DaysAdjustment = DaysAdjustment.ofBusinessDays(2, UNKNOWN_CALENDAR)
    unknown.adjust(base, REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    unknown.resolve(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    unknown.toReader.run(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    noException should be thrownBy unknown.resolve(REF_DATA)
  }

  test("test_ofBusinessDays2_null") {
    // The Java method asserted `ofBusinessDays(2, null)` raises `IllegalArgumentException`. Here
    // the calendar is a required parameter of a type that has no null-like member, so the call
    // the Java test made cannot be written: omitting the calendar names no factory - there is no
    // one-argument `ofBusinessDays` - and passing anything that is not a `HolidayCalendarId` in
    // its place is a type error. Both are proved when this file is compiled.
    assertDoesNotCompile("DaysAdjustment.ofBusinessDays(2)")
    assertDoesNotCompile("DaysAdjustment.ofBusinessDays(2, BDA_FOLLOW_SAT_SUN)")
    assertDoesNotCompile("""DaysAdjustment.ofBusinessDays(2, "Sat/Sun")""")

    // The positive control: the well-typed call the proofs above are the complement of.
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
    // Both calendars are named by the rendering, in the order they are applied, because this form
    // exists precisely for the case where they differ.
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

    // With a non-zero count this form holds exactly the fields it is given, which is what makes
    // it the way to rebuild an adjustment from the fields of another - and the fields of every
    // adjustment are accepted by the validated factory, which is the statement that the two
    // construction routes agree.
    DaysAdjustment.of(test.days, test.calendar, test.adjustment) should haveValue(test)

    // A count of zero is the one input it rewrites, as the two-argument form does and for the
    // same reason: there is no addition of zero business days, so the addition calendar is
    // dropped and the adjustment supplied is kept. The value is the one `normalized` answers with
    // for the fields as given, and it is a value the validated factory accepts - which is why
    // that factory can refuse the pairing outright.
    val zero: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.SAT_SUN, BDA_NONE)
    zero.days shouldBe 0
    zero.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    zero.adjustment shouldBe BDA_NONE
    zero shouldBe DaysAdjustment.NONE
    DaysAdjustment.of(0, HolidayCalendarIds.SAT_SUN, BDA_NONE) should
      beFailureWith(FailureReason.INVALID)

    // The two-argument form of the same request reads it as the rule it can mean, so the two
    // zero-day forms are different values: this one adjusts nothing, that one moves a holiday
    // forwards.
    zero should not be DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.SAT_SUN)
  }

  test("test_ofBusinessDays3_adjust") {
    val test: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(3, HolidayCalendarIds.SAT_SUN, BDA_FOLLOW_WED_THU)
    val base: LocalDate = FRI_2014_08_15 // Fri

    // Three Saturday/Sunday business days after the Friday is the Wednesday, which is a holiday
    // of the Wednesday/Thursday calendar, so the trailing adjustment carries it over the Thursday
    // to the Friday. Two calendars, each doing its own step - the whole point of this form.
    test.adjust(base, WED_THU_REF_DATA) should haveValue(FRI_2014_08_22)
    test.resolve(WED_THU_REF_DATA).map(adjuster => adjuster.adjust(base)) should
      haveValue(FRI_2014_08_22)
    test.toReader.run(WED_THU_REF_DATA).map(adjuster => adjuster.adjust(base)) should
      haveValue(FRI_2014_08_22)

    // `resolve` looks both calendars up once, here, and binds them into the adjuster returned:
    // that is what it is for, and what it has to leave unchanged is the answer. One adjuster is
    // therefore applied to several dates and compared with resolving afresh for each of them.
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

    // Two readers compose, which is the reason the reader form exists: several adjustments are
    // assembled while no reference data is available, and the data is supplied once, to the
    // composition, rather than to each of them.
    val pair: RefDataReader[(LocalDate, LocalDate)] =
      (test.toReader, DaysAdjustment.ofBusinessDays(1, HolidayCalendarIds.SAT_SUN).toReader)
        .mapN((twoCalendars, oneCalendar) =>
          (twoCalendars.adjust(base), oneCalendar.adjust(base)))
    pair.run(WED_THU_REF_DATA) should haveValue((FRI_2014_08_22, MON_2014_08_18))

    // The failure path, in both calendars. The built-in reference data knows nothing of `WedThu`,
    // so the trailing adjustment cannot be resolved against it; and an unresolvable addition
    // calendar fails the same way. A composition one reader of which cannot resolve fails as a
    // whole, with that reader's failure.
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
    // The Java method asserted three calls raise `IllegalArgumentException` - a null calendar, a
    // null adjustment, and both null. None of the three can be written here: each parameter is
    // required and typed, so the proofs below are the compile-time complement of those three Java
    // assertions, in the same order.
    assertDoesNotCompile("DaysAdjustment.ofBusinessDays(3, BDA_FOLLOW_SAT_SUN, BDA_FOLLOW_SAT_SUN)")
    assertDoesNotCompile(
      "DaysAdjustment.ofBusinessDays(3, HolidayCalendarIds.SAT_SUN, HolidayCalendarIds.SAT_SUN)")
    assertDoesNotCompile("""DaysAdjustment.ofBusinessDays(3, "Sat/Sun", "Following")""")

    // The positive control: the well-typed call the proofs above are the complement of.
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
    // the one thing it can mean - "the next business day of this calendar, or this date if it
    // already is one" - and stores the calendar in a `Following` adjustment rather than as the
    // addition calendar. The fields are therefore NOT the ones passed in, which is the subtlest
    // behaviour of this type and exactly what the Java method pinned.
    test.days shouldBe 0
    test.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN
    // and it therefore renders in the calendar-day form, the calendar appearing in the trailing
    // adjustment instead
    test.toString shouldBe "0 calendar days then apply Following using calendar Sat/Sun"

    // The behaviour that shape buys: a business day is returned unaltered, and a holiday moves
    // forwards to the next business day.
    test.adjust(FRI_2014_08_15, REF_DATA) should haveValue(FRI_2014_08_15) // Fri
    test.adjust(SAT_2014_08_16, REF_DATA) should haveValue(MON_2014_08_18) // Sat -> Mon
    test.adjust(SUN_2014_08_17, REF_DATA) should haveValue(MON_2014_08_18) // Sun -> Mon
    test.adjust(MON_2014_08_18, REF_DATA) should haveValue(MON_2014_08_18) // Mon

    // The resolved and reader forms agree on every one of those four dates, the weekend ones
    // included, because the shape of the value is what decides and not the entry point.
    val resolvedOnce: Either[Failure, DateAdjuster] = test.resolve(REF_DATA)
    resolvedOnce should beSuccess
    List(FRI_2014_08_15, SAT_2014_08_16, SUN_2014_08_17, MON_2014_08_18).foreach { day =>
      withClue(s"$test adjusting $day: ") {
        val perDate: Either[Failure, LocalDate] = test.adjust(day, REF_DATA)
        resolvedOnce.map(adjuster => adjuster.adjust(day)) shouldBe perDate
        test.toReader.run(REF_DATA).map(adjuster => adjuster.adjust(day)) shouldBe perDate
      }
    }

    // The alternative reading of the same request is a different value with different behaviour:
    // a zero-day addition carrying no trailing adjustment adds nothing and adjusts nothing, so a
    // holiday stays a holiday. That is why the zero case is special-cased at all. The three-
    // argument factory builds it by dropping the addition calendar, so it is the no-adjustment
    // constant and renders in the calendar-day form; the calendar the caller named is gone
    // because nothing in that value would ever have consulted it.
    val addsNothing: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.SAT_SUN, BDA_NONE)
    addsNothing should not be test
    addsNothing shouldBe DaysAdjustment.NONE
    addsNothing.adjust(SAT_2014_08_16, REF_DATA) should haveValue(SAT_2014_08_16)
    addsNothing.toString shouldBe "0 calendar days"

    // The three-argument rewrite is the one place this port departs from the factory being
    // ported, which held a zero-day business-day addition as it was given, so what it costs is
    // asserted rather than described. It costs no computed date: shifting a date by zero business
    // days returns the date whichever calendar is asked, so the value the rewrite keeps computes,
    // for every date, exactly what the trailing adjustment alone computes - which is what the
    // discarded calendar would have contributed nothing to. Sixty consecutive days carry that
    // across weekends, a London bank holiday and a month end.
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

    // What it does cost is one reading of one field, and only where the trailing adjustment names
    // no calendar either: the calendar the caller passed is no longer recoverable from the value,
    // so `resultCalendar` answers the no-holidays identifier where the type being ported answered
    // the discarded calendar. The port's answer is the accurate one for this value - it adds
    // nothing and adjusts nothing, so it returns its input, and an arbitrary input is not a
    // business day of any calendar - which is why the rewrite is kept and recorded rather than
    // repaired. With a trailing adjustment that does name a calendar, as `test` has, the reading
    // is unchanged, because `resultCalendar` reads the adjustment first.
    addsNothing.resultCalendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    rewritten.resultCalendar shouldBe HolidayCalendarIds.SAT_SUN

    // The calendar it was given is recoverable from the value in spite of the rewrite, which is
    // what `resultCalendar` is for, and the value is already in normal form.
    test.resultCalendar shouldBe HolidayCalendarIds.SAT_SUN
    test.normalized shouldBe test

    // The rewrite applies whatever calendar is named, including the no-holidays one, where the
    // resulting `Following` adjustment is a no-op over a calendar with no holidays at all.
    val none: DaysAdjustment = DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.NO_HOLIDAYS)
    none.days shouldBe 0
    none.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    none.adjustment shouldBe
      BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.NO_HOLIDAYS)
    none.adjust(SAT_2014_08_16, REF_DATA) should haveValue(SAT_2014_08_16)

    // A real calendar shows the same rewrite doing real work: the London summer bank holiday of
    // 2014 is a Monday, so zero London business days from the Saturday before it is the Tuesday.
    DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.GBLO)
      .adjust(date(2014, 8, 23), REF_DATA) should haveValue(TUE_2014_08_26)
  }


  //-------------------------------------------------------------------------
  test("test_getResultCalendar1") {
    val test: DaysAdjustment = DaysAdjustment.ofBusinessDays(3, HolidayCalendarIds.SAT_SUN)

    // The addition walks this calendar and no trailing adjustment follows it, so the calendar
    // that had the last word - and which the result is therefore a business day of - is the
    // addition calendar.
    test.resultCalendar shouldBe HolidayCalendarIds.SAT_SUN

    // which the result bears out: the date returned is a business day of it
    test.adjust(FRI_2014_08_15, REF_DATA).map(adjusted =>
      HolidayCalendars.SAT_SUN.isBusinessDay(adjusted)) shouldBe Right(true)
  }

  test("test_getResultCalendar2") {
    val test: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(3, HolidayCalendarIds.SAT_SUN, BDA_FOLLOW_WED_THU)

    // Two calendars, so the one that had the last word is the trailing adjustment's, not the one
    // the addition used - and it is the trailing one the result is guaranteed to be a business
    // day of.
    test.resultCalendar shouldBe WED_THU
    test.resultCalendar should not be test.calendar

    test.adjust(FRI_2014_08_15, WED_THU_REF_DATA).map(adjusted =>
      WED_THU_CALENDAR.isBusinessDay(adjusted)) shouldBe Right(true)
  }

  test("test_getResultCalendar3") {
    val test: DaysAdjustment = DaysAdjustment.ofCalendarDays(3)

    // Neither step names a calendar, so there is none to return and the result is a plain date
    // which may well be a weekend - the calendar-day form makes no promise about it.
    test.resultCalendar shouldBe HolidayCalendarIds.NO_HOLIDAYS

    // The two forms therefore answer differently for the same count and the same rendering
    // prefix, which is the distinction this method exists to expose.
    DaysAdjustment.ofBusinessDays(3, HolidayCalendarIds.SAT_SUN).resultCalendar shouldBe
      HolidayCalendarIds.SAT_SUN
    DaysAdjustment.ofCalendarDays(3).resultCalendar should not be
      DaysAdjustment.ofBusinessDays(3, HolidayCalendarIds.SAT_SUN).resultCalendar

    // A trailing adjustment on the calendar-day form supplies the result calendar it otherwise
    // lacks, and the zero-day business form recovers the calendar the factory moved.
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

    // the six Java cases, in the Java order
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

    // Normalising is idempotent - a normal form that changed under normalising would not be one -
    // and it never changes what an adjustment computes, which is the property that makes dropping
    // a field legitimate at all.
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

    // The normalised zero-day form is the calendar-day form, so it renders as one, and the
    // calendar the input named is gone from the addition but still present in the adjustment.
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

    // the three Java assertions: a difference in any one of the three fields is a different value
    a.equals(b) shouldBe false
    a.equals(c) shouldBe false
    a.equals(d) shouldBe false

    // and the reflexive half the Java method left implicit: equality is by field, so a value
    // built twice is one value, with one hash, and the typeclass instance agrees with both
    val same: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(3, HolidayCalendarIds.NO_HOLIDAYS, BDA_FOLLOW_SAT_SUN)
    a shouldBe same
    a.hashCode shouldBe same.hashCode
    Hash[DaysAdjustment].eqv(a, same) shouldBe true
    Hash[DaysAdjustment].hash(a) shouldBe Hash[DaysAdjustment].hash(same)
    Hash[DaysAdjustment].eqv(a, b) shouldBe false
    Hash[DaysAdjustment].eqv(a, c) shouldBe false
    Hash[DaysAdjustment].eqv(a, d) shouldBe false

    // A value of another type is not equal to an adjustment, asserted through the method rather
    // than the operator because the two types are unrelated.
    a.equals("3 calendar days then apply Following using calendar Sat/Sun") shouldBe false

    // Two adjustments that compute the same dates while holding different fields are deliberately
    // not equal; `normalized` is how a caller asks for the representative form before comparing.
    // The pair below is the surviving case of that: a business-day addition adjusted against the
    // very calendar that performed it lands on a business day of that calendar already, so the
    // trailing adjustment is redundant and the two values behave alike while holding different
    // fields.
    val sameBehaviour: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(3, WED_THU, BDA_FOLLOW_WED_THU)
    val normalForm: DaysAdjustment = DaysAdjustment.ofBusinessDays(3, WED_THU)
    sameBehaviour should not be normalForm
    sameBehaviour.normalized shouldBe normalForm

    // The other case the method being ported repaired - a zero-day addition naming a calendar -
    // cannot reach `normalized` here, because no value holds those fields: the factory drops the
    // calendar and the validated factory refuses the pairing, so the two forms are the same value
    // rather than two values one of which normalises to the other.
    DaysAdjustment.ofBusinessDays(0, WED_THU, BDA_FOLLOW_SAT_SUN) shouldBe
      DaysAdjustment.ofCalendarDays(0, BDA_FOLLOW_SAT_SUN)
    DaysAdjustment.of(0, WED_THU, BDA_FOLLOW_SAT_SUN) should beFailureWith(FailureReason.INVALID)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java method was `coverImmutableBean(DaysAdjustment.ofCalendarDays(4,
    // BDA_FOLLOW_SAT_SUN))`: a reflective sweep over a Joda bean's properties, equality, hashing
    // and rendering. There is no bean and no reflection in this port, so that sweep has no target
    // and the properties it stood for are asserted directly, over exactly the value the Java
    // method named and a second value differing from it.
    val test: DaysAdjustment = DaysAdjustment.ofCalendarDays(4, BDA_FOLLOW_SAT_SUN)
    val other: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(4, HolidayCalendarIds.SAT_SUN, BDA_FOLLOW_WED_THU)

    // the three properties, read back as the factory stored them
    test.days shouldBe 4
    test.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN
    other.days shouldBe 4
    other.calendar shouldBe HolidayCalendarIds.SAT_SUN
    other.adjustment shouldBe BDA_FOLLOW_WED_THU

    // equality and hashing, which the sweep exercised by comparing a bean with itself, with an
    // equal bean and with a different one
    test shouldBe DaysAdjustment.ofCalendarDays(4, BDA_FOLLOW_SAT_SUN)
    test should not be other
    Hash[DaysAdjustment].eqv(test, DaysAdjustment.ofCalendarDays(4, BDA_FOLLOW_SAT_SUN)) shouldBe true
    Hash[DaysAdjustment].eqv(test, other) shouldBe false
    Hash[DaysAdjustment].hash(test) shouldBe test.hashCode
    test.equals(other.toString) shouldBe false

    // rendering, which the sweep read through the bean's `toString`, and the agreement of `Show`
    // with it - the two ways of putting an adjustment into a message must not differ
    test.toString shouldBe "4 calendar days then apply Following using calendar Sat/Sun"
    Show[DaysAdjustment].show(test) shouldBe test.toString
    Show[DaysAdjustment].show(other) shouldBe
      "4 business days using calendar Sat/Sun then apply Following using calendar WedThu"
    Show[DaysAdjustment].show(DaysAdjustment.NONE) shouldBe "0 calendar days"

    // The read half of the sweep, expressed as destructuring rather than as property lookup:
    // there is no `copy` to exercise for the write half, construction of this type being a choice
    // between factories rather than a field at a time.
    DaysAdjustment.unapply(test) shouldBe
      Some((4, HolidayCalendarIds.NO_HOLIDAYS, BDA_FOLLOW_SAT_SUN))
    DaysAdjustment.unapply(other) shouldBe
      Some((4, HolidayCalendarIds.SAT_SUN, BDA_FOLLOW_WED_THU))
  }

  test("coverage_builder") {
    // The Java method assembled `(1, Sat/Sun, Following using WedThu)` a field at a time through
    // the generated Joda builder. That builder has no counterpart here - factories and `with*`
    // methods replace builders in this port, and this type publishes no `with*` method, its four
    // factories being the whole of its construction - so the value is built through the factory
    // that holds the fields it is given, and the assertions the Java method made on the result
    // are made on it.
    val test: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(1, HolidayCalendarIds.SAT_SUN, BDA_FOLLOW_WED_THU)

    test.days shouldBe 1
    test.calendar shouldBe HolidayCalendarIds.SAT_SUN
    test.adjustment shouldBe BDA_FOLLOW_WED_THU

    // What a builder was used for beyond that is starting from an existing value and changing one
    // field, which here is naming the change through the same factory. Rebuilding from the fields
    // of a value returns that value, and each single-field change is visible in the result and in
    // its rendering.
    DaysAdjustment.ofBusinessDays(test.days, test.calendar, test.adjustment) shouldBe test
    DaysAdjustment.ofBusinessDays(2, test.calendar, test.adjustment).days shouldBe 2
    DaysAdjustment.ofBusinessDays(test.days, WED_THU, test.adjustment).calendar shouldBe WED_THU
    DaysAdjustment.ofBusinessDays(test.days, test.calendar, BDA_NONE).adjustment shouldBe BDA_NONE
    DaysAdjustment.ofBusinessDays(test.days, test.calendar, BDA_NONE).toString shouldBe
      "1 business day using calendar Sat/Sun"

    // The two-argument factories are the same construction with a field defaulted, so they agree
    // with the three-argument one wherever it is given those defaults - which is what lets a
    // ported call site keep whichever form it used.
    DaysAdjustment.ofBusinessDays(1, HolidayCalendarIds.SAT_SUN) shouldBe
      DaysAdjustment.ofBusinessDays(1, HolidayCalendarIds.SAT_SUN, BDA_NONE)
    DaysAdjustment.ofCalendarDays(1) shouldBe
      DaysAdjustment.ofBusinessDays(1, HolidayCalendarIds.NO_HOLIDAYS, BDA_NONE)
    DaysAdjustment.ofCalendarDays(1, BDA_FOLLOW_WED_THU) shouldBe
      DaysAdjustment.ofBusinessDays(1, HolidayCalendarIds.NO_HOLIDAYS, BDA_FOLLOW_WED_THU)

    // The validated factory is the fifth route in, and the one a caller holding three fields it
    // did not choose - read off a document, or computed from data - reaches for, because it is the
    // one that judges them. It accepts every field combination that describes an adjustment: a
    // negative count, a composite calendar, and any convention over any calendar.
    DaysAdjustment.of(test.days, test.calendar, test.adjustment) should haveValue(test)
    DaysAdjustment.of(-2, HolidayCalendarIds.SAT_SUN, BDA_NONE) should
      haveValue(DaysAdjustment.ofBusinessDays(-2, HolidayCalendarIds.SAT_SUN))
    DaysAdjustment.of(
      3,
      HolidayCalendarIds.GBLO.combinedWith(HolidayCalendarIds.USNY),
      BDA_FOLLOW_WED_THU) should beSuccess
    DaysAdjustment.of(0, HolidayCalendarIds.NO_HOLIDAYS, BDA_NONE) should
      haveValue(DaysAdjustment.NONE)

    // It refuses exactly one pairing, the one the class documents as naming no day: a count of
    // zero against a calendar that would have performed the addition. The failure says so, and
    // it is a single cause, the three fields having nothing else about them to be wrong.
    val zeroDaysNamingACalendar: ResultNec[DaysAdjustment] =
      DaysAdjustment.of(0, WED_THU, BDA_FOLLOW_SAT_SUN)
    zeroDaysNamingACalendar should beFailureWith(FailureReason.INVALID)
    zeroDaysNamingACalendar should haveFailureMessageMatching(
      "A business day addition of zero days names no day.*WedThu.*")
    zeroDaysNamingACalendar.left.toOption.map(failures => failures.length) shouldBe Some(1L)

    // That message names the identifier it rejected, and an identifier is arbitrary text:
    // `HolidayCalendarId.of` is total and accepts any string, including one carrying line breaks
    // or running to any length. The failure carries that text as it was supplied, so a caller
    // correcting its input is handed back exactly what was refused; bounding it and escaping what
    // it may hold is the business of writing the failure out, and the rendering of a failure is
    // one line, escapes anything a line-oriented reader could act on, and is bounded however long
    // the name was. Both halves are asserted: what the failure carries, and what reaches a log.
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

    // No factory builds that pairing, which is what makes the refusal a statement about the type
    // rather than about one route into it: every value any factory produces is accepted by `of`,
    // and each zero-day value names no addition calendar.
    List(
      DaysAdjustment.NONE,
      DaysAdjustment.ofCalendarDays(0),
      DaysAdjustment.ofCalendarDays(0, BDA_FOLLOW_SAT_SUN),
      DaysAdjustment.ofCalendarDays(4, BDA_FOLLOW_WED_THU),
      DaysAdjustment.ofBusinessDays(0, WED_THU),
      DaysAdjustment.ofBusinessDays(0, WED_THU, BDA_FOLLOW_SAT_SUN),
      DaysAdjustment.ofBusinessDays(4, WED_THU),
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
    // The Java method was `assertSerialization(DaysAdjustment.ofCalendarDays(4,
    // BDA_FOLLOW_SAT_SUN))`, a Joda-Beans binary and JSON round trip. Joda wire compatibility is
    // out of scope for this port, so the round trip is the derived circe codec, and the document
    // it produces is pinned here: an object of the three fields under the names the Java bean
    // declared, in declaration order, the count a number and each calendar the bare string its
    // own type publishes.
    val test: DaysAdjustment = DaysAdjustment.ofCalendarDays(4, BDA_FOLLOW_SAT_SUN)
    val encoded: Json = test.asJson

    encoded.asObject.map(obj => obj.keys.toList) shouldBe Some(List("days", "calendar", "adjustment"))
    encoded.noSpaces shouldBe
      """{"days":4,"calendar":"NoHolidays","adjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""

    // No field is optional, and the encoder drops absent values in any case, so no null can appear
    // in the document of any adjustment.
    encoded.noSpaces should not include "null"
    DaysAdjustment.NONE.asJson.noSpaces shouldBe
      """{"days":0,"calendar":"NoHolidays","adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}"""

    // The round trip itself, for the Java value, for the constant, for the two-calendar form and
    // for a composite calendar, which is written out in full and normalised by its own type.
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

    // Equal values encode to identical bytes, whichever factory built them - which for the
    // zero-day business form means the document of a calendar-day adjustment, because that is
    // what the value is.
    DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.SAT_SUN).asJson.noSpaces shouldBe
      DaysAdjustment.ofCalendarDays(0, BDA_FOLLOW_SAT_SUN).asJson.noSpaces

    // Decoding hands the fields to the validated factory, so a document describing the one
    // pairing no adjustment has - a count of zero against a named addition calendar - is refused
    // rather than wrapped. Such a document is one no encoder of this port writes, because every
    // factory drops the addition calendar of a zero-day addition, so the round trip is unaffected
    // and the refusal reaches only hand-written input.
    val zeroDaysNamingACalendar: String =
      """{"days":0,"calendar":"Sat/Sun","adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}"""
    decode[DaysAdjustment](zeroDaysNamingACalendar).isLeft shouldBe true
    decode[DaysAdjustment](zeroDaysNamingACalendar).left.map(_.getMessage) match {
      case Left(message) => message should include("zero")
      case Right(value) => fail(s"the document should have been refused but decoded to $value")
    }

    // The calendar of such a document is arbitrary text, and the refusal names it, so the
    // decoder is the second way a hostile identifier could reach whatever records the failure.
    // It reaches it as it stands, exactly as it does through the factory, because the message is
    // built in one place - and the rendering asserted above, which is what a log receives, is
    // therefore bounded and single-line on this path as well.
    val forgedInDocument: String =
      """{"days":0,"calendar":"GBLO\nWARN  decoded","adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}"""
    decode[DaysAdjustment](forgedInDocument).left.map(_.getMessage) match {
      case Left(message) => message should include("GBLO\nWARN  decoded")
      case Right(value) => fail(s"the document should have been refused but decoded to $value")
    }
    val longInDocument: String = "Z" * 4096
    decode[DaysAdjustment](
      s"""{"days":0,"calendar":"$longInDocument","adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}""")
      .left.map(_.getMessage) match {
      case Left(message) => message should include(longInDocument)
      case Right(value) => fail(s"the document should have been refused but decoded to $value")
    }
    val zeroDays: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.SAT_SUN, BDA_NONE)
    zeroDays.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    decode[DaysAdjustment](zeroDays.asJson.noSpaces) shouldBe Right(zeroDays)

    // The documents below vary one part of a payload at a time, so the parts they share are named
    // once here: the adjustment object of `BusinessDayAdjustment.NONE`, and a function assembling
    // a document from a count, a calendar and an adjustment object.
    val noAdjustObject: String = """{"convention":"NoAdjust","calendar":"NoHolidays"}"""
    def document(days: String, calendar: String, adjustment: String): String =
      s"""{"days":$days,"calendar":"$calendar","adjustment":$adjustment}"""

    // Beyond that one pairing the decoder rejects the shape of the payload: every field is
    // required, the count has to be a number, and an adjustment is an object rather than a
    // string. A failure here is a `DecodingFailure` carried in the `Left`, not an exception.
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
    // override and which reads a JSON string holding an integer as that integer. That leniency is
    // recorded rather than relied on: nothing this port writes produces such a document, since
    // the encoder above writes the count as a number.
    decode[DaysAdjustment](document("\"4\"", "NoHolidays", noAdjustObject)) shouldBe
      Right(DaysAdjustment.ofCalendarDays(4))

    // The convention inside the adjustment is read by the name lookup of its own closed family,
    // so text naming no convention is rejected here rather than at resolution time; a calendar
    // this library knows nothing about, by contrast, is a fact about the reference data rather
    // than about the document, so it decodes and fails only when it is resolved.
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
