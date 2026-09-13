/*
 * Copyright (C) 2022 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.DayOfWeek
import java.time.DayOfWeek.SATURDAY
import java.time.DayOfWeek.SUNDAY
import java.time.LocalDate

import io.circe.parser.decode
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.TestingReferenceDataId
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[HolidaySafeReferenceData]].
 *
 * The subject decorates another set of reference data so that a holiday calendar identifier the
 * underlying data does not hold still resolves, to a calendar of '''that''' identifier whose only
 * holidays are Saturday and Sunday. Its four rules are asserted here:
 *
 *   1. a simple calendar identifier the underlying data does not hold is defaulted, and the
 *      defaulted calendar carries the identifier that was asked for;
 *   2. a composite identifier is deliberately '''not''' defaulted as a whole, so that resolving
 *      it falls through to its parts and each missing part is defaulted in turn;
 *   3. membership is true for every calendar identifier, composite ones included, and is the
 *      underlying data's answer for anything else;
 *   4. combining re-applies the decoration, so defaulting survives however data is layered.
 *
 * [[ImmutableHolidayCalendar]] compares equal on its identifier and nothing else, so a defaulted
 * `GBLO` calendar and the real London calendar are '''equal''' values, as are a combination built
 * from a stored `EUTA` calendar and one built from a defaulted one. Every assertion about which
 * calendar came back is therefore paired with one about a date: a defaulted calendar holds no
 * holidays, so it reports [[GBLO_HOLIDAY]] or [[EUTA_HOLIDAY]] as a business day where a stored
 * calendar reports a holiday. Without that pairing several of these tests would pass however
 * wrong the subject was.
 *
 * Composite resolution lives on the identifier, not on the store: `findValue` is the store's own
 * lookup and answers nothing for a composite identifier, while `HolidayCalendarId.resolve` tries
 * the whole name and then the parts, so the composite tests assert both halves. A missing item of
 * reference data is a `Left` carrying `FailureReason.MISSING_DATA`, compared as a value of the
 * closed family of reasons rather than as text. [[storedCalendar]] reads each fixture calendar
 * through `HolidayCalendars.of`, the lookup an application makes of the built-in set, and fails
 * the fixture rather than substituting anything of its own.
 */
class HolidaySafeReferenceDataSpec extends AnyFunSuite with Matchers {

  /** The weekend a defaulted calendar observes, and the weekend every calendar built here has. */
  private val WEEKEND_DAYS: List[DayOfWeek] = List(SATURDAY, SUNDAY)

  private val NO_HOL_ID: HolidayCalendarId = HolidayCalendarIds.NO_HOLIDAYS

  private val NO_HOL_CAL: HolidayCalendar = HolidayCalendars.NO_HOLIDAYS

  private val SAT_SUN_ID: HolidayCalendarId = HolidayCalendarIds.SAT_SUN

  private val SAT_SUN_CAL: HolidayCalendar = HolidayCalendars.SAT_SUN

  private val GBLO_ID: HolidayCalendarId = HolidayCalendarIds.GBLO

  /**
   * A holiday of the London calendar that is a business day for the European TARGET calendar:
   * Monday 31 August 2015, the English summer bank holiday. Being a weekday, it is a business day
   * of a weekend-only calendar, which is what separates a stored London calendar from a defaulted
   * one where equality cannot; `test_singleCalendar_inReferenceData` asserts both properties.
   */
  private val GBLO_HOLIDAY: LocalDate = LocalDate.of(2015, 8, 31)

  private val GBLO_CAL: HolidayCalendar = storedCalendar(GBLO_ID, GBLO_HOLIDAY)

  private val EUTA_ID: HolidayCalendarId = HolidayCalendarIds.EUTA

  /**
   * A holiday of the European TARGET calendar that is a business day in London: Friday 1 May
   * 2015, Labour Day, which the United Kingdom does not observe - its early May bank holiday
   * falls on the first Monday. Being a holiday of one part of `EUTA+GBLO` and a business day of
   * the other, it reports which part a resolved composite was assembled from;
   * `test_combinedCalendar_getValue_referenceDataContainsBoth` asserts that of the fixtures.
   */
  private val EUTA_HOLIDAY: LocalDate = LocalDate.of(2015, 5, 1)

  private val EUTA_CAL: HolidayCalendar = storedCalendar(EUTA_ID, EUTA_HOLIDAY)

  /** The weekend-only calendar supplied for the TARGET identifier when it is missing. */
  private val TEST_EUTA_CAL: HolidayCalendar = createCalendar(EUTA_ID)

  /** An identifier this library defines no calendar for, and no fixture below holds. */
  private val TEST_ID: HolidayCalendarId = HolidayCalendarId.of("TEST")

  private val TEST_CAL: HolidayCalendar = createCalendar(TEST_ID)

  /** A second identifier no fixture holds: `GBXX` answers with a calendar named `GBXX`. */
  private val UNKNOWN_ID: HolidayCalendarId = HolidayCalendarId.of("GBXX")

  /**
   * The identifier that combines London with the European TARGET calendar. `HolidayCalendarId.of`
   * deduplicates and sorts the parts, so it is named `EUTA+GBLO` and resolves its parts in that
   * order - which is why every expected combination below names the TARGET calendar first.
   */
  private val COMBINED_GBLO_EUTA_ID: HolidayCalendarId = HolidayCalendarId.of("GBLO+EUTA")

  private val LINKED_GBLO_EUTA_ID: HolidayCalendarId = HolidayCalendarId.of("GBLO~EUTA")

  /** An identifier that is not a calendar identifier, and so is never defaulted. */
  private val NON_CAL_ID: TestingReferenceDataId = TestingReferenceDataId("1")

  /** The value filed under [[NON_CAL_ID]], boxed because that identifier refers to a `Number`. */
  private val NON_CAL_VAL: java.lang.Number = Int.box(1)

  /**
   * A Saturday, on which every calendar built by this spec is closed: 29 August 2015. A defaulted
   * calendar observes the weekend and nothing else, so this date shows it is a calendar and not
   * an empty one - including for `NoHolidays`, whose built-in calendar is closed on no day.
   */
  private val A_SATURDAY: LocalDate = LocalDate.of(2015, 8, 29)

  //-------------------------------------------------------------------------
  test("test_singleCalendar_inReferenceData") {
    // Rule 1 from the side on which nothing is defaulted: a calendar the underlying data - here
    // `ReferenceData.minimal` itself - holds is returned unchanged.
    val test = HolidaySafeReferenceData(ReferenceData.minimal)

    // The two dates this spec discriminates on, asserted here so that a mistake in either is
    // reported as a mistake in the fixture rather than as a fault in the subject.
    GBLO_HOLIDAY.getDayOfWeek shouldBe DayOfWeek.MONDAY
    A_SATURDAY.getDayOfWeek shouldBe SATURDAY

    // The two national calendars the fixtures store, read through the published lookup. The
    // assertions are behavioural rather than by equality: a calendar compares on its identifier
    // alone, so any calendar named `GBLO` would satisfy an equality comparison.
    HolidayCalendars.of(GBLO_ID.name).map(calendar => calendar.isHoliday(GBLO_HOLIDAY)) shouldBe
      Right(true)
    HolidayCalendars.of(EUTA_ID.name).map(calendar => calendar.isHoliday(EUTA_HOLIDAY)) shouldBe
      Right(true)

    // Each calendar observes one of the two dates and is open on the other, so a date says which
    // of them a value came from; the London calendar's own holiday is asserted below.
    GBLO_CAL.isBusinessDay(EUTA_HOLIDAY) shouldBe true
    EUTA_CAL.isHoliday(EUTA_HOLIDAY) shouldBe true
    EUTA_CAL.isBusinessDay(GBLO_HOLIDAY) shouldBe true

    test.findValue(NO_HOL_ID) shouldBe Some(NO_HOL_CAL)
    test.findValue(SAT_SUN_ID) shouldBe Some(SAT_SUN_CAL)

    // London is not in the minimal set, so it is defaulted although the two above were not.
    test.findValue(GBLO_ID) shouldBe Some(createCalendar(GBLO_ID))
    test.findValue(TEST_ID) shouldBe Some(TEST_CAL)

    // The defaulted London calendar is an equal value to the real one, so only its holidays tell
    // them apart: it carries the requested identifier and observes the weekend alone.
    test.findValue(GBLO_ID).map(calendar => calendar.id) shouldBe Some(GBLO_ID)
    test.findValue(GBLO_ID).map(calendar => calendar.name) shouldBe Some("GBLO")
    test.findValue(GBLO_ID).map(calendar => calendar.isHoliday(A_SATURDAY)) shouldBe Some(true)
    test.findValue(GBLO_ID).map(calendar => calendar.isHoliday(GBLO_HOLIDAY)) shouldBe Some(false)
    GBLO_CAL.isHoliday(GBLO_HOLIDAY) shouldBe true

    // The underlying data's own calendars: no-holidays is closed on no day, Sat/Sun on Saturday.
    test.findValue(NO_HOL_ID).map(calendar => calendar.isHoliday(A_SATURDAY)) shouldBe Some(false)
    test.findValue(SAT_SUN_ID).map(calendar => calendar.isHoliday(A_SATURDAY)) shouldBe Some(true)
  }

  test("test_singleCalendar_missingFromReferenceData") {
    // The underlying data holds nothing, so every calendar identifier is defaulted.
    val test = HolidaySafeReferenceData(ReferenceData.empty)

    test.findValue(NO_HOL_ID) shouldBe Some(createCalendar(NO_HOL_ID))
    test.findValue(SAT_SUN_ID) shouldBe Some(createCalendar(SAT_SUN_ID))
    test.findValue(GBLO_ID) shouldBe Some(createCalendar(GBLO_ID))
    test.findValue(TEST_ID) shouldBe Some(TEST_CAL)

    // What defaulting means for those two: the answer is a weekend-only calendar named after the
    // requested identifier, and not the built-in value of that identifier.
    test.findValue(NO_HOL_ID) should not be Some(NO_HOL_CAL)
    test.findValue(SAT_SUN_ID) should not be Some(SAT_SUN_CAL)
    test.findValue(NO_HOL_ID).map(calendar => calendar.id) shouldBe Some(NO_HOL_ID)
    test.findValue(SAT_SUN_ID).map(calendar => calendar.id) shouldBe Some(SAT_SUN_ID)

    test.findValue(NO_HOL_ID).map(calendar => calendar.isHoliday(A_SATURDAY)) shouldBe Some(true)
    NO_HOL_CAL.isHoliday(A_SATURDAY) shouldBe false

    test.findValue(GBLO_ID).map(calendar => calendar.isHoliday(GBLO_HOLIDAY)) shouldBe Some(false)
    test.findValue(EUTA_ID).map(calendar => calendar.isHoliday(EUTA_HOLIDAY)) shouldBe Some(false)

    // Rule 1 applies to an identifier this library has never seen: `GBXX` answers with `GBXX`.
    test.findValue(UNKNOWN_ID).map(calendar => calendar.name) shouldBe Some("GBXX")
  }

  test("test_singleCalendar_getValue") {
    val test: ReferenceData = HolidaySafeReferenceData(
      store(
        ReferenceData.Entry(NO_HOL_ID, NO_HOL_CAL),
        ReferenceData.Entry(SAT_SUN_ID, SAT_SUN_CAL),
        ReferenceData.Entry(GBLO_ID, GBLO_CAL)))

    test.getValue(NO_HOL_ID) should haveValue(NO_HOL_CAL)
    test.getValue(SAT_SUN_ID) should haveValue(SAT_SUN_CAL)
    test.getValue(GBLO_ID) should haveValue(GBLO_CAL)

    test.getValue(GBLO_ID).map(calendar => calendar.isHoliday(GBLO_HOLIDAY)) shouldBe Right(true)

    // An identifier the store does not hold is defaulted, so `getValue` succeeds for it.
    test.getValue(TEST_ID) should haveValue(TEST_CAL)
    test.getValue(TEST_ID).map(calendar => calendar.isHoliday(A_SATURDAY)) shouldBe Right(true)

    // An identifier that is not a calendar identifier is never defaulted, so a store that lacks
    // it fails, the reason compared as a value of the closed family of reasons.
    test.getValue(NON_CAL_ID) should beFailureWith(FailureReason.MISSING_DATA)
  }

  test("test_singleCalendar_findValue") {
    val test: ReferenceData = HolidaySafeReferenceData(
      store(
        ReferenceData.Entry(NO_HOL_ID, NO_HOL_CAL),
        ReferenceData.Entry(SAT_SUN_ID, SAT_SUN_CAL),
        ReferenceData.Entry(GBLO_ID, GBLO_CAL)))

    test.findValue(NO_HOL_ID) shouldBe Some(NO_HOL_CAL)
    test.findValue(SAT_SUN_ID) shouldBe Some(SAT_SUN_CAL)
    test.findValue(GBLO_ID) shouldBe Some(GBLO_CAL)
    test.findValue(GBLO_ID).map(calendar => calendar.isHoliday(GBLO_HOLIDAY)) shouldBe Some(true)

    test.findValue(TEST_ID) shouldBe Some(TEST_CAL)

    // Not a calendar identifier, so nothing is invented for it: the store's own answer stands.
    test.findValue(NON_CAL_ID) shouldBe None
  }

  //-------------------------------------------------------------------------
  test("test_combinedCalendar_getValue_referenceDataContainsBoth") {
    // Rule 2 with both parts available, the combination coming from resolving the identifier.
    val test: ReferenceData = HolidaySafeReferenceData(
      store(ReferenceData.Entry(EUTA_ID, EUTA_CAL), ReferenceData.Entry(GBLO_ID, GBLO_CAL)))

    // The fixture property the assertions below rest on: the TARGET calendar is closed on
    // Labour Day and London is open, so that date reports which part a combination holds.
    EUTA_CAL.isHoliday(EUTA_HOLIDAY) shouldBe true
    GBLO_CAL.isBusinessDay(EUTA_HOLIDAY) shouldBe true

    // The store answers nothing for a composite identifier, which sends resolution to the parts.
    test.findValue(COMBINED_GBLO_EUTA_ID) shouldBe None
    test.getValue(COMBINED_GBLO_EUTA_ID) should beFailureWith(FailureReason.MISSING_DATA)
    test.getValue(LINKED_GBLO_EUTA_ID) should beFailureWith(FailureReason.MISSING_DATA)

    // The parts resolve in the normalised name's order, so TARGET is the first operand of each.
    COMBINED_GBLO_EUTA_ID.resolve(test) should haveValue(HolidayCalendar.Combined(EUTA_CAL, GBLO_CAL))
    LINKED_GBLO_EUTA_ID.resolve(test) should haveValue(HolidayCalendar.Linked(EUTA_CAL, GBLO_CAL))

    COMBINED_GBLO_EUTA_ID.resolve(test).map(cal => cal.isHoliday(EUTA_HOLIDAY)) shouldBe Right(true)
    COMBINED_GBLO_EUTA_ID.resolve(test).map(cal => cal.isHoliday(GBLO_HOLIDAY)) shouldBe Right(true)
    COMBINED_GBLO_EUTA_ID.resolve(test).map(cal => cal.id) shouldBe Right(COMBINED_GBLO_EUTA_ID)

    // A linked calendar keeps only the holidays both parts agree on.
    LINKED_GBLO_EUTA_ID.resolve(test).map(cal => cal.isHoliday(EUTA_HOLIDAY)) shouldBe Right(false)
    LINKED_GBLO_EUTA_ID.resolve(test).map(cal => cal.isHoliday(GBLO_HOLIDAY)) shouldBe Right(false)
    LINKED_GBLO_EUTA_ID.resolve(test).map(cal => cal.id) shouldBe Right(LINKED_GBLO_EUTA_ID)

    // A host that supplies the composite identifier pre-combined: the whole name is tried first,
    // which the absence of the TARGET holiday from the result shows.
    val preCombined = createCalendar(COMBINED_GBLO_EUTA_ID)
    val wholeName: ReferenceData = HolidaySafeReferenceData(
      store(
        ReferenceData.Entry(COMBINED_GBLO_EUTA_ID, preCombined),
        ReferenceData.Entry(EUTA_ID, EUTA_CAL),
        ReferenceData.Entry(GBLO_ID, GBLO_CAL)))
    wholeName.getValue(COMBINED_GBLO_EUTA_ID) should haveValue(preCombined)
    COMBINED_GBLO_EUTA_ID.resolve(wholeName) should haveValue(preCombined)
    COMBINED_GBLO_EUTA_ID.resolve(wholeName).map(cal => cal.isHoliday(EUTA_HOLIDAY)) shouldBe Right(false)
  }

  test("test_combinedCalendar_getValue_referenceDataOnlyContainsOne") {
    // Rule 2 with one part missing, the case it exists for: the composite identifier is left
    // unresolved, so resolution runs part by part and the missing TARGET calendar is defaulted on
    // its own - defaulting the whole composite would have discarded London's holidays.
    val test: ReferenceData =
      HolidaySafeReferenceData(store(ReferenceData.Entry(GBLO_ID, GBLO_CAL)))

    test.findValue(COMBINED_GBLO_EUTA_ID) shouldBe None
    test.getValue(COMBINED_GBLO_EUTA_ID) should beFailureWith(FailureReason.MISSING_DATA)

    COMBINED_GBLO_EUTA_ID.resolve(test) should haveValue(HolidayCalendar.Combined(TEST_EUTA_CAL, GBLO_CAL))
    LINKED_GBLO_EUTA_ID.resolve(test) should haveValue(HolidayCalendar.Linked(TEST_EUTA_CAL, GBLO_CAL))

    // A calendar compares on its identifier alone, so the previous test's combination satisfies
    // those two assertions as well:
    HolidayCalendar.Combined(TEST_EUTA_CAL, GBLO_CAL) shouldBe HolidayCalendar.Combined(EUTA_CAL, GBLO_CAL)

    // ... so the dates distinguish the two: London's holiday survived into the combination and
    // the TARGET holiday did not, that part having been defaulted.
    COMBINED_GBLO_EUTA_ID.resolve(test).map(cal => cal.isHoliday(GBLO_HOLIDAY)) shouldBe Right(true)
    COMBINED_GBLO_EUTA_ID.resolve(test).map(cal => cal.isHoliday(EUTA_HOLIDAY)) shouldBe Right(false)

    COMBINED_GBLO_EUTA_ID.resolve(test).map(cal => cal.id) shouldBe Right(COMBINED_GBLO_EUTA_ID)
    LINKED_GBLO_EUTA_ID.resolve(test).map(cal => cal.id) shouldBe Right(LINKED_GBLO_EUTA_ID)

    // Both parts missing: the combination is closed at the weekend and on no other day.
    val empty: ReferenceData = HolidaySafeReferenceData(ReferenceData.empty)
    COMBINED_GBLO_EUTA_ID.resolve(empty) should
      haveValue(HolidayCalendar.Combined(TEST_EUTA_CAL, createCalendar(GBLO_ID)))
    COMBINED_GBLO_EUTA_ID.resolve(empty).map(cal => cal.isHoliday(A_SATURDAY)) shouldBe Right(true)
    COMBINED_GBLO_EUTA_ID.resolve(empty).map(cal => cal.isHoliday(GBLO_HOLIDAY)) shouldBe Right(false)
  }

  test("test_combinedCalendar_findValue_referenceDataContainsBoth") {
    val test: ReferenceData = HolidaySafeReferenceData(
      store(ReferenceData.Entry(EUTA_ID, EUTA_CAL), ReferenceData.Entry(GBLO_ID, GBLO_CAL)))

    test.findValue(COMBINED_GBLO_EUTA_ID) shouldBe None
    test.findValue(LINKED_GBLO_EUTA_ID) shouldBe None

    // Resolving the identifier is what answers; `toOption` is its `Option`-valued form.
    COMBINED_GBLO_EUTA_ID.resolve(test).toOption shouldBe Some(HolidayCalendar.Combined(EUTA_CAL, GBLO_CAL))
    LINKED_GBLO_EUTA_ID.resolve(test).toOption shouldBe Some(HolidayCalendar.Linked(EUTA_CAL, GBLO_CAL))

    COMBINED_GBLO_EUTA_ID.resolve(test).toOption.map(cal => cal.isHoliday(EUTA_HOLIDAY)) shouldBe Some(true)
    COMBINED_GBLO_EUTA_ID.resolve(test).toOption.map(cal => cal.isHoliday(GBLO_HOLIDAY)) shouldBe Some(true)

    // A pre-combined calendar filed under the whole name is found by the store.
    val preCombined = createCalendar(COMBINED_GBLO_EUTA_ID)
    val wholeName: ReferenceData = HolidaySafeReferenceData(
      store(
        ReferenceData.Entry(COMBINED_GBLO_EUTA_ID, preCombined),
        ReferenceData.Entry(EUTA_ID, EUTA_CAL),
        ReferenceData.Entry(GBLO_ID, GBLO_CAL)))
    wholeName.findValue(COMBINED_GBLO_EUTA_ID) shouldBe Some(preCombined)
    COMBINED_GBLO_EUTA_ID.resolve(wholeName).toOption.map(cal => cal.isHoliday(EUTA_HOLIDAY)) shouldBe Some(false)
  }

  test("test_combinedCalendar_findValue_referenceDataOnlyContainsOne") {
    val test: ReferenceData =
      HolidaySafeReferenceData(store(ReferenceData.Entry(GBLO_ID, GBLO_CAL)))

    test.findValue(COMBINED_GBLO_EUTA_ID) shouldBe None
    test.findValue(LINKED_GBLO_EUTA_ID) shouldBe None

    COMBINED_GBLO_EUTA_ID.resolve(test).toOption shouldBe
      Some(HolidayCalendar.Combined(TEST_EUTA_CAL, GBLO_CAL))
    LINKED_GBLO_EUTA_ID.resolve(test).toOption shouldBe
      Some(HolidayCalendar.Linked(TEST_EUTA_CAL, GBLO_CAL))

    // The defaulted part, shown by the dates rather than by equality.
    COMBINED_GBLO_EUTA_ID.resolve(test).toOption.map(cal => cal.isHoliday(GBLO_HOLIDAY)) shouldBe Some(true)
    COMBINED_GBLO_EUTA_ID.resolve(test).toOption.map(cal => cal.isHoliday(EUTA_HOLIDAY)) shouldBe Some(false)
    COMBINED_GBLO_EUTA_ID.resolve(test).toOption.map(cal => cal.id) shouldBe Some(COMBINED_GBLO_EUTA_ID)
  }

  //-------------------------------------------------------------------------
  test("test_nonCalendarId") {
    // Rule 1 from the other side: an identifier that is not a calendar identifier is never
    // defaulted, defaulting one meaning inventing a value of a type this module knows nothing of.
    val test: ReferenceData = HolidaySafeReferenceData(ReferenceData.empty)

    test.findValue(NON_CAL_ID) shouldBe None

    test.getValue(NON_CAL_ID) should beFailureWith(FailureReason.MISSING_DATA)

    // Rule 3: membership is true for every calendar identifier - one this library defines, one
    // it has never seen, and a composite one - and is the underlying data's answer otherwise.
    test.containsValue(GBLO_ID) shouldBe true
    test.containsValue(UNKNOWN_ID) shouldBe true
    test.containsValue(COMBINED_GBLO_EUTA_ID) shouldBe true
    test.containsValue(LINKED_GBLO_EUTA_ID) shouldBe true
    test.containsValue(NON_CAL_ID) shouldBe false

    // Membership is true for a composite identifier although the store reports nothing for it:
    // what a caller can obtain is the resolved calendar, and it can obtain one.
    test.findValue(COMBINED_GBLO_EUTA_ID) shouldBe None
    COMBINED_GBLO_EUTA_ID.resolve(test) should beSuccess

    test.findValue(UNKNOWN_ID).map(calendar => calendar.id) shouldBe Some(UNKNOWN_ID)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    val test = HolidaySafeReferenceData(store(ReferenceData.Entry(NON_CAL_ID, NON_CAL_VAL)))
    val test2 = HolidaySafeReferenceData(store(ReferenceData.Entry(TEST_ID, TEST_CAL)))

    test.underlying shouldBe store(ReferenceData.Entry(NON_CAL_ID, NON_CAL_VAL))

    // `same` decorates an independently built store holding the same entry, so what is asserted
    // is that two decorations of equal data are one value, not that an object equals itself.
    val same = HolidaySafeReferenceData(store(ReferenceData.Entry(NON_CAL_ID, NON_CAL_VAL)))
    test shouldBe same
    test.hashCode shouldBe same.hashCode

    test should not be test2
    test.findValue(NON_CAL_ID) shouldBe Some(NON_CAL_VAL)
    test2.findValue(NON_CAL_ID) shouldBe None
    test.toString should include("HolidaySafeReferenceData")
    test.toString should not be test2.toString

    // Rule 4: `combinedWith` re-wraps the combined underlying store, so defaulting survives
    // layering - without the re-wrap, an identifier neither side held would fail again.
    val combined = test.combinedWith(test2.underlying)
    combined shouldBe HolidaySafeReferenceData(test.underlying.combinedWith(test2.underlying))
    combined.findValue(NON_CAL_ID) shouldBe Some(NON_CAL_VAL)
    combined.findValue(TEST_ID) shouldBe Some(TEST_CAL)
    combined.findValue(UNKNOWN_ID).map(calendar => calendar.id) shouldBe Some(UNKNOWN_ID)
    combined.containsValue(UNKNOWN_ID) shouldBe true
  }

  test("test_serialization") {
    // Reference data and its implementations sit outside this module's JSON surface - the store is
    // heterogeneous, keyed by identifiers that each determine their own value type - so no
    // `Encoder` or `Decoder` is provided for them, which is what the two `assertDoesNotCompile`
    // calls observe: no such instance is in scope here. The one kind of reference data that does
    // serialize is a holiday calendar, which carries its own codec, and the rest of this test
    // round-trips the calendars this store answers with.
    val test = HolidaySafeReferenceData(store(ReferenceData.Entry(TEST_ID, TEST_CAL)))

    assertDoesNotCompile("implicitly[io.circe.Encoder[HolidaySafeReferenceData]]")
    assertDoesNotCompile("implicitly[io.circe.Decoder[HolidaySafeReferenceData]]")

    // Equality compares identifiers alone, so the dates show what came across with the calendar.
    val stored = test.findValue(TEST_ID).getOrElse(fail("the stored calendar was not found"))
    val storedAgain = decode[HolidayCalendar](stored.asJson.noSpaces)
      .getOrElse(fail(s"the stored calendar did not survive the round trip: ${stored.asJson.noSpaces}"))
    storedAgain shouldBe stored
    storedAgain.id shouldBe TEST_ID
    storedAgain.isHoliday(A_SATURDAY) shouldBe true
    storedAgain.isHoliday(GBLO_HOLIDAY) shouldBe false

    // A defaulted calendar is an ordinary calendar and is written as one, carrying its identifier.
    val defaulted = test.findValue(UNKNOWN_ID).getOrElse(fail("the defaulted calendar was not supplied"))
    val defaultedAgain = decode[HolidayCalendar](defaulted.asJson.noSpaces)
      .getOrElse(fail(s"the defaulted calendar did not survive the round trip: ${defaulted.asJson.noSpaces}"))
    defaultedAgain shouldBe defaulted
    defaultedAgain.id shouldBe UNKNOWN_ID
    defaultedAgain.name shouldBe "GBXX"
    defaultedAgain.isHoliday(A_SATURDAY) shouldBe true
    defaultedAgain.isHoliday(GBLO_HOLIDAY) shouldBe false
  }

  //-------------------------------------------------------------------------
  /**
   * Builds the calendar the subject supplies for an identifier nothing holds, which every
   * defaulted expectation here compares against: the requested identifier, no holidays, and the
   * Saturday/Sunday weekend.
   *
   * @param id  the identifier the calendar carries
   * @return the weekend-only calendar of that identifier
   */
  private def createCalendar(id: HolidayCalendarId): ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(id, Nil, WEEKEND_DAYS)

  /**
   * Reads the calendar this library publishes for the given identifier, to be stored in a
   * fixture.
   *
   * The calendar is read through `HolidayCalendars.of`, the lookup by which an application
   * reaches the built-in set, so that a fixture is the real London or TARGET calendar rather than
   * one this spec assembled. A lookup that does not answer fails the fixture, as does a calendar
   * that answers but does not observe the date the assertions discriminate on - such a fixture
   * would make every assertion that reads it vacuous, and a substitute would satisfy those
   * assertions by construction.
   *
   * @param id  the identifier of the published calendar to read
   * @param holiday  the date the published calendar is required to observe as a holiday
   * @return the published calendar of that identifier, which is closed on that date
   */
  private def storedCalendar(id: HolidayCalendarId, holiday: LocalDate): HolidayCalendar =
    HolidayCalendars.of(id.name) match {
      case Right(calendar) if calendar.isHoliday(holiday) => calendar
      case Right(_) =>
        fail(s"Fixture calendar ${id.name} is published but no longer observes $holiday as a holiday")
      case Left(failure) =>
        fail(s"Fixture calendar ${id.name} is not published by this library: ${failure.message}")
    }

  /**
   * Builds reference data holding the given entries over the minimal set. `ReferenceData.of` lays
   * the caller's entries over the four weekend and no-holiday calendars and reports the one way
   * it can fail, two entries filed under the same identifier, so a fixture that cannot be built
   * is reported as a failed test naming its cause.
   *
   * @param entries  the reference data entries, which must not repeat an identifier
   * @return the reference data holding those entries
   */
  private def store(entries: ReferenceData.Entry[_]*): ReferenceData =
    ReferenceData.of(entries: _*) match {
      case Right(data) => data
      case Left(failure) => fail(s"Fixture reference data could not be built: ${failure.message}")
    }
}
