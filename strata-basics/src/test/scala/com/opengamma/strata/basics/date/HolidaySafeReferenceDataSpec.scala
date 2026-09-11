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
 * Test [[HolidaySafeReferenceData]], ported from the Java `HolidaySafeReferenceDataTest`.
 *
 * The subject decorates another set of reference data so that a holiday calendar identifier
 * the underlying data does not hold still resolves, to a calendar of '''that''' identifier
 * whose only holidays are Saturday and Sunday. Its four rules are asserted here, and each
 * test below says which of them it is about:
 *
 *   1. a simple calendar identifier the underlying data does not hold is defaulted, and the
 *      defaulted calendar carries the identifier that was asked for;
 *   2. a composite identifier is deliberately '''not''' defaulted as a whole, so that
 *      resolving it falls through to its parts and each missing part is defaulted in turn;
 *   3. membership is true for every calendar identifier, composite ones included, and is the
 *      underlying data's answer for anything else;
 *   4. combining re-applies the decoration, so defaulting survives however data is layered.
 *
 * All eleven Java test methods are ported, one test each, under the name the Java method had,
 * which is what keeps the method-level traceability of the migration exact.
 *
 * ===Equality is by identifier, so this spec asserts holidays as well===
 *
 * [[ImmutableHolidayCalendar]] compares equal on its identifier and nothing else. A defaulted
 * `GBLO` calendar and the real London calendar are therefore '''equal''' values, and so are a
 * combination built from a stored `EUTA` calendar and one built from a defaulted one. Every
 * assertion here that is about which calendar came back is consequently paired with an
 * assertion about a date: a defaulted calendar holds no holidays at all, so it reports
 * [[GBLO_HOLIDAY]] or [[EUTA_HOLIDAY]] as a business day where a stored calendar reports it
 * as a holiday. Without that pairing several of these tests would pass however wrong the
 * subject was.
 *
 * ===Three differences from the original are structural===
 *
 *   - The Java interface's low-level query, which signalled absence by returning a reference
 *     to nothing, has no counterpart: [[ReferenceData.findValue]] returning an `Option` is
 *     the primitive of this port. The three Java tests written against that query are written
 *     against `findValue` here, and no lookup in either module returns a reference to nothing.
 *   - `getValue` reports a missing item of reference data as a `Left` carrying
 *     `FailureReason.MISSING_DATA` rather than throwing `ReferenceDataNotFoundException`,
 *     which is not ported. The two Java `assertThatExceptionOfType` sites - both asking for
 *     the identifier that is not a calendar identifier - are failure assertions here, and
 *     every failure assertion in this spec compares the reason as a value of the closed family
 *     of reasons, never as text. Nothing in this API throws, so no test here asserts a throw.
 *   - Composite resolution moved from the store to the identifier. In the library being
 *     ported, `ReferenceData.getValue`, `findValue` and `containsValue` all delegated to the
 *     identifier's own low-level query, and a `HolidayCalendarId` carried a resolver that tried
 *     the whole name and then the parts - which is why the Java tests could ask the store
 *     itself for `GBLO+EUTA`. Here `findValue` is the store's own lookup and answers nothing
 *     for a composite identifier, while `HolidayCalendarId.resolve(refData)` - the call an
 *     application makes, directly or through a date adjustment - performs the whole-name and
 *     part-by-part resolution. The four composite tests below therefore assert both halves of
 *     that: what the store answers, and what resolving the identifier against it produces.
 *
 * ===The calendars the fixtures store are the ones this library publishes===
 *
 * [[storedCalendar]] reads a fixture calendar through `HolidayCalendars.of`, which is the
 * lookup an application makes of the built-in set, and it fails the fixture rather than
 * substituting anything of its own where that lookup does not answer or answers a calendar
 * that does not observe the date the assertions below discriminate on. Nothing is synthesized
 * because nothing else in this module would notice if the publication broke:
 * `HolidayCalendarsSpec` says outright that it never calls `HolidayCalendars.of` and never
 * reads `ReferenceData.standard`, and `GlobalHolidayCalendarsSpec` asserts what the calendar
 * generators produce rather than that the built-in set publishes what they produced. So the
 * fixtures that make the assertions about the decoration meaningful are also this module's
 * only guard on that publication path, and `test_singleCalendar_inReferenceData` asserts it
 * outright rather than leaving it implied.
 *
 * What this spec owns of those two calendars is narrow, and deliberately so. It does not
 * re-assert which holidays London or the European TARGET system observe - the generated set is
 * asserted year by year by `GlobalHolidayCalendarsSpec` - it asserts that both calendars are
 * found under the names they are published as, and that each of them observes one of
 * [[GBLO_HOLIDAY]] and [[EUTA_HOLIDAY]] and is open on the other, which is exactly what the
 * assertions here read them for. `ReferenceData.minimal` is used as it is published too: the
 * store `test_singleCalendar_inReferenceData` decorates is that constant itself, so the
 * calendars it is specified to hold are asserted of it rather than laid over it.
 *
 * ===What is asserted elsewhere===
 *
 * `ReferenceDataSpec` and `CombinedReferenceDataSpec` own the general contract of a store,
 * `HolidayCalendarIdSpec` owns the composite resolution algorithm from the identifier's side,
 * and `json/JsonRoundTripSpec` owns the property-based codec round trips. This spec asserts
 * the decoration alone.
 */
class HolidaySafeReferenceDataSpec extends AnyFunSuite with Matchers {

  /**
   * The weekend a defaulted calendar observes, and the weekend every calendar built here has.
   *
   * A list rather than a set, as the `ImmutableList` of the Java fixture was; the factory
   * takes any `Iterable[DayOfWeek]`.
   */
  private val WEEKEND_DAYS: List[DayOfWeek] = List(SATURDAY, SUNDAY)

  /** The identifier of the calendar that has no holidays. */
  private val NO_HOL_ID: HolidayCalendarId = HolidayCalendarIds.NO_HOLIDAYS

  /** The calendar that has no holidays, one of the four the minimal set holds. */
  private val NO_HOL_CAL: HolidayCalendar = HolidayCalendars.NO_HOLIDAYS

  /** The identifier of the Saturday/Sunday weekend calendar. */
  private val SAT_SUN_ID: HolidayCalendarId = HolidayCalendarIds.SAT_SUN

  /** The Saturday/Sunday weekend calendar, one of the four the minimal set holds. */
  private val SAT_SUN_CAL: HolidayCalendar = HolidayCalendars.SAT_SUN

  /** The identifier of the London calendar. */
  private val GBLO_ID: HolidayCalendarId = HolidayCalendarIds.GBLO

  /**
   * A holiday of the London calendar that is not a holiday of the European TARGET calendar.
   *
   * Monday 31 August 2015, the English summer bank holiday. It is a weekday, so a weekend-only
   * calendar reports it as a business day, and that is what separates a stored London calendar
   * from a defaulted one - a distinction equality cannot make, since a calendar compares on its
   * identifier alone. Both properties are asserted on the fixture itself in
   * `test_singleCalendar_inReferenceData`, so a mistake in the date is reported as such.
   */
  private val GBLO_HOLIDAY: LocalDate = LocalDate.of(2015, 8, 31)

  /** The London calendar, as stored by the fixtures that hold it. */
  private val GBLO_CAL: HolidayCalendar = storedCalendar(GBLO_ID, GBLO_HOLIDAY)

  /** The identifier of the European TARGET settlement calendar. */
  private val EUTA_ID: HolidayCalendarId = HolidayCalendarIds.EUTA

  /**
   * A holiday of the European TARGET calendar that is a business day in London.
   *
   * Friday 1 May 2015, Labour Day, which the TARGET system observes and the United Kingdom
   * does not - the English early May bank holiday falls on the first Monday. Being a holiday of
   * one part of `EUTA+GBLO` and a business day of the other is exactly what makes it able to
   * report which part a resolved composite calendar was assembled from;
   * `test_combinedCalendar_getValue_referenceDataContainsBoth` asserts that of the fixtures
   * before relying on it.
   */
  private val EUTA_HOLIDAY: LocalDate = LocalDate.of(2015, 5, 1)

  /** The European TARGET calendar, as stored by the fixtures that hold it. */
  private val EUTA_CAL: HolidayCalendar = storedCalendar(EUTA_ID, EUTA_HOLIDAY)

  /**
   * The weekend-only calendar that is supplied for the TARGET identifier when it is missing.
   *
   * The Java fixture of the same name. It is what the two `referenceDataOnlyContainsOne` tests
   * expect the missing part of the composite identifier to have been defaulted to.
   */
  private val TEST_EUTA_CAL: HolidayCalendar = createCalendar(EUTA_ID)

  /** An identifier this library defines no calendar for, and no fixture below holds. */
  private val TEST_ID: HolidayCalendarId = HolidayCalendarId.of("TEST")

  /** The weekend-only calendar that is supplied for [[TEST_ID]]. */
  private val TEST_CAL: HolidayCalendar = createCalendar(TEST_ID)

  /**
   * A second identifier no fixture holds, used where a test needs one it has not already
   * mentioned.
   *
   * The name is the one the type's own documentation discusses: a request for `GBXX` answers
   * with a calendar named `GBXX`, and never with the shared weekend calendar.
   */
  private val UNKNOWN_ID: HolidayCalendarId = HolidayCalendarId.of("GBXX")

  /**
   * The identifier that combines London with the European TARGET calendar.
   *
   * Written `GBLO+EUTA`, as the Java fixture wrote it. `HolidayCalendarId.of` deduplicates and
   * sorts the parts, so the identifier is named `EUTA+GBLO` and resolves its parts in that
   * order - which is why every expected combination below names the TARGET calendar first.
   */
  private val COMBINED_GBLO_EUTA_ID: HolidayCalendarId = HolidayCalendarId.of("GBLO+EUTA")

  /** The identifier that links London to the European TARGET calendar, named `EUTA~GBLO`. */
  private val LINKED_GBLO_EUTA_ID: HolidayCalendarId = HolidayCalendarId.of("GBLO~EUTA")

  /**
   * An identifier that is not a calendar identifier, and so is never defaulted.
   *
   * [[TestingReferenceDataId]] is the fixture of the root test package, shared with the specs
   * of [[ReferenceData]] itself; it is used as declared rather than restated here, because its
   * shape is a contract several specs depend on.
   */
  private val NON_CAL_ID: TestingReferenceDataId = TestingReferenceDataId("1")

  /**
   * The value filed under [[NON_CAL_ID]] by the store `coverage` builds.
   *
   * Boxed explicitly because the identifier refers to a `java.lang.Number`, exactly as the
   * Java fixture's `Number NON_CAL_VAL = 1` was boxed by the compiler.
   */
  private val NON_CAL_VAL: java.lang.Number = Int.box(1)

  /**
   * A Saturday, on which every calendar built by this spec is closed.
   *
   * Saturday 29 August 2015. A defaulted calendar observes the weekend and nothing else, so
   * this is the date that shows a defaulted calendar is a calendar and not an empty one -
   * including where the identifier asked for was `NoHolidays`, whose built-in calendar is
   * closed on no day at all.
   */
  private val A_SATURDAY: LocalDate = LocalDate.of(2015, 8, 29)

  //-------------------------------------------------------------------------
  test("test_singleCalendar_inReferenceData") {
    // Rule 1, from the side on which nothing is defaulted: a calendar the underlying data
    // holds is the calendar returned, unchanged. The underlying data is `ReferenceData.minimal`
    // as this library publishes it, so what the two assertions below read back is what that
    // constant holds.
    val test = HolidaySafeReferenceData(ReferenceData.minimal)

    // The two dates this spec discriminates on, asserted here so that a mistake in either is
    // reported as a mistake in the fixture rather than as a fault in the subject.
    GBLO_HOLIDAY.getDayOfWeek shouldBe DayOfWeek.MONDAY
    A_SATURDAY.getDayOfWeek shouldBe SATURDAY

    // The publication of the two national calendars the fixtures store, asserted here because
    // this is the module's only test of it: `HolidayCalendarsSpec` never calls
    // `HolidayCalendars.of` and `GlobalHolidayCalendarsSpec` asserts the generators rather than
    // the built-in set they are published through. The assertions are behavioural rather than by
    // equality, since a calendar compares on its identifier alone and any calendar named `GBLO`
    // would satisfy a comparison: what is stated is that the lookup answers and that the
    // calendar it answers with is closed on the date this spec reads it for. A lookup that
    // stopped answering at all is reported one step earlier still, by [[storedCalendar]] as it
    // builds the fixture.
    HolidayCalendars.of(GBLO_ID.name).map(calendar => calendar.isHoliday(GBLO_HOLIDAY)) shouldBe
      Right(true)
    HolidayCalendars.of(EUTA_ID.name).map(calendar => calendar.isHoliday(EUTA_HOLIDAY)) shouldBe
      Right(true)

    // And the property of those two published calendars that every assertion in this spec reads
    // them for: each observes one of the two dates and is open on the other, so a date says
    // which of them a value came from where equality cannot.
    // `GBLO_CAL.isHoliday(GBLO_HOLIDAY)`, the fourth of the four facts, is asserted below at the
    // point where the stored London calendar is set against the defaulted one.
    GBLO_CAL.isBusinessDay(EUTA_HOLIDAY) shouldBe true
    EUTA_CAL.isHoliday(EUTA_HOLIDAY) shouldBe true
    EUTA_CAL.isBusinessDay(GBLO_HOLIDAY) shouldBe true

    // The Java original read these through the low-level query that has no counterpart here;
    // the primitive of this port is `findValue`, and absence is `None`.
    test.findValue(NO_HOL_ID) shouldBe Some(NO_HOL_CAL)
    test.findValue(SAT_SUN_ID) shouldBe Some(SAT_SUN_CAL)

    // London is not part of the minimal set, so this one is defaulted even though the two
    // above were not - which is what makes this test about the underlying data being
    // consulted first, rather than about defaulting being off.
    test.findValue(GBLO_ID) shouldBe Some(createCalendar(GBLO_ID))
    test.findValue(TEST_ID) shouldBe Some(TEST_CAL)

    // And this is why the assertion above is not enough on its own. A calendar compares on
    // its identifier alone, so the defaulted London calendar is an equal value to the real
    // one; only its holidays can tell them apart. It carries the identifier that was asked
    // for, observes the weekend, and observes nothing else.
    test.findValue(GBLO_ID).map(calendar => calendar.id) shouldBe Some(GBLO_ID)
    test.findValue(GBLO_ID).map(calendar => calendar.name) shouldBe Some("GBLO")
    test.findValue(GBLO_ID).map(calendar => calendar.isHoliday(A_SATURDAY)) shouldBe Some(true)
    test.findValue(GBLO_ID).map(calendar => calendar.isHoliday(GBLO_HOLIDAY)) shouldBe Some(false)
    GBLO_CAL.isHoliday(GBLO_HOLIDAY) shouldBe true

    // The two calendars that came from the underlying data are its own values, which their
    // behaviour confirms rather than their equality: the no-holidays calendar is closed on no
    // day at all, not even a Saturday, while the Saturday/Sunday calendar is closed on one.
    test.findValue(NO_HOL_ID).map(calendar => calendar.isHoliday(A_SATURDAY)) shouldBe Some(false)
    test.findValue(SAT_SUN_ID).map(calendar => calendar.isHoliday(A_SATURDAY)) shouldBe Some(true)
  }

  test("test_singleCalendar_missingFromReferenceData") {
    // Rule 1 in its sharpest form. The underlying data holds nothing, so every identifier is
    // defaulted - including the two whose calendars this library itself defines.
    val test = HolidaySafeReferenceData(ReferenceData.empty)

    test.findValue(NO_HOL_ID) shouldBe Some(createCalendar(NO_HOL_ID))
    test.findValue(SAT_SUN_ID) shouldBe Some(createCalendar(SAT_SUN_ID))
    test.findValue(GBLO_ID) shouldBe Some(createCalendar(GBLO_ID))
    test.findValue(TEST_ID) shouldBe Some(TEST_CAL)

    // What "defaulted" means for those two, stated rather than left to the reader: the answer
    // is a weekend-only calendar named after the requested identifier and is not the built-in
    // value of that identifier. A weekend calendar is never equal to a calendar carrying
    // holiday data, even where the two share an identifier, so these comparisons are exact.
    test.findValue(NO_HOL_ID) should not be Some(NO_HOL_CAL)
    test.findValue(SAT_SUN_ID) should not be Some(SAT_SUN_CAL)
    test.findValue(NO_HOL_ID).map(calendar => calendar.id) shouldBe Some(NO_HOL_ID)
    test.findValue(SAT_SUN_ID).map(calendar => calendar.id) shouldBe Some(SAT_SUN_ID)

    // The defaulted no-holidays calendar is closed at the weekend, where the built-in
    // no-holidays calendar is closed on no day - the one behavioural difference that proves
    // the identifier was carried through rather than the value being looked up.
    test.findValue(NO_HOL_ID).map(calendar => calendar.isHoliday(A_SATURDAY)) shouldBe Some(true)
    NO_HOL_CAL.isHoliday(A_SATURDAY) shouldBe false

    // No defaulted calendar holds a holiday of its own, whatever it is named.
    test.findValue(GBLO_ID).map(calendar => calendar.isHoliday(GBLO_HOLIDAY)) shouldBe Some(false)
    test.findValue(EUTA_ID).map(calendar => calendar.isHoliday(EUTA_HOLIDAY)) shouldBe Some(false)

    // Rule 1 applies to an identifier this library has never seen, which is the case the type
    // exists for: `GBXX` answers with a calendar named `GBXX`.
    test.findValue(UNKNOWN_ID).map(calendar => calendar.name) shouldBe Some("GBXX")
  }

  test("test_singleCalendar_getValue") {
    // The same rule read through `getValue`, over a store that holds three calendars.
    val test: ReferenceData = HolidaySafeReferenceData(
      store(
        ReferenceData.Entry(NO_HOL_ID, NO_HOL_CAL),
        ReferenceData.Entry(SAT_SUN_ID, SAT_SUN_CAL),
        ReferenceData.Entry(GBLO_ID, GBLO_CAL)))

    test.getValue(NO_HOL_ID) should haveValue(NO_HOL_CAL)
    test.getValue(SAT_SUN_ID) should haveValue(SAT_SUN_CAL)
    test.getValue(GBLO_ID) should haveValue(GBLO_CAL)

    // The stored London calendar rather than a default, which its holidays are what show.
    test.getValue(GBLO_ID).map(calendar => calendar.isHoliday(GBLO_HOLIDAY)) shouldBe Right(true)

    // An identifier the store does not hold is defaulted, so `getValue` succeeds where it
    // would otherwise have failed - the whole purpose of this decoration.
    test.getValue(TEST_ID) should haveValue(TEST_CAL)
    test.getValue(TEST_ID).map(calendar => calendar.isHoliday(A_SATURDAY)) shouldBe Right(true)

    // The Java original asserted that this threw `ReferenceDataNotFoundException`. That type
    // is not ported: an identifier that is not a calendar identifier is never defaulted, and
    // an identifier no source of reference data holds is data about the request rather than a
    // defect in the program, so it is reported as a failure the caller can act on. The reason
    // is compared as a value of the closed family of reasons, never as text.
    test.getValue(NON_CAL_ID) should beFailureWith(FailureReason.MISSING_DATA)
  }

  test("test_singleCalendar_findValue") {
    // The same store and the same rule, read through the `Option`-valued primitive. The Java
    // assertions were written against `Optional.of` and `Optional.empty`.
    val test: ReferenceData = HolidaySafeReferenceData(
      store(
        ReferenceData.Entry(NO_HOL_ID, NO_HOL_CAL),
        ReferenceData.Entry(SAT_SUN_ID, SAT_SUN_CAL),
        ReferenceData.Entry(GBLO_ID, GBLO_CAL)))

    test.findValue(NO_HOL_ID) shouldBe Some(NO_HOL_CAL)
    test.findValue(SAT_SUN_ID) shouldBe Some(SAT_SUN_CAL)
    test.findValue(GBLO_ID) shouldBe Some(GBLO_CAL)
    test.findValue(GBLO_ID).map(calendar => calendar.isHoliday(GBLO_HOLIDAY)) shouldBe Some(true)

    // Defaulted, as in `test_singleCalendar_getValue`.
    test.findValue(TEST_ID) shouldBe Some(TEST_CAL)

    // Not a calendar identifier, so nothing is invented for it: the store's own answer stands.
    test.findValue(NON_CAL_ID) shouldBe None
  }

  //-------------------------------------------------------------------------
  test("test_combinedCalendar_getValue_referenceDataContainsBoth") {
    // Rule 2, with both parts available. See the note on the class: the store answers for the
    // identifiers it holds and a composite identifier is not one of them, so the combination
    // is what resolving the identifier against the store produces.
    val test: ReferenceData = HolidaySafeReferenceData(
      store(ReferenceData.Entry(EUTA_ID, EUTA_CAL), ReferenceData.Entry(GBLO_ID, GBLO_CAL)))

    // The fixture property the assertions below rest on: the TARGET calendar is closed on
    // Labour Day and London is open, so that date reports which part a combination holds.
    EUTA_CAL.isHoliday(EUTA_HOLIDAY) shouldBe true
    GBLO_CAL.isBusinessDay(EUTA_HOLIDAY) shouldBe true

    // What the store itself answers for the composite identifiers. In the library being
    // ported this call resolved them, because the lookup was driven by the identifier; here it
    // is the store's own lookup, and reporting nothing is what sends resolution to the parts.
    test.findValue(COMBINED_GBLO_EUTA_ID) shouldBe None
    test.getValue(COMBINED_GBLO_EUTA_ID) should beFailureWith(FailureReason.MISSING_DATA)
    test.getValue(LINKED_GBLO_EUTA_ID) should beFailureWith(FailureReason.MISSING_DATA)

    // And what an application observes. The parts are resolved in the order the normalised
    // name gives, which sorts them, so the TARGET calendar is the first operand of each.
    COMBINED_GBLO_EUTA_ID.resolve(test) should haveValue(HolidayCalendar.Combined(EUTA_CAL, GBLO_CAL))
    LINKED_GBLO_EUTA_ID.resolve(test) should haveValue(HolidayCalendar.Linked(EUTA_CAL, GBLO_CAL))

    // Both parts came from the store, which equality cannot show and these dates can: a
    // combined calendar is closed where either part is closed.
    COMBINED_GBLO_EUTA_ID.resolve(test).map(cal => cal.isHoliday(EUTA_HOLIDAY)) shouldBe Right(true)
    COMBINED_GBLO_EUTA_ID.resolve(test).map(cal => cal.isHoliday(GBLO_HOLIDAY)) shouldBe Right(true)
    COMBINED_GBLO_EUTA_ID.resolve(test).map(cal => cal.id) shouldBe Right(COMBINED_GBLO_EUTA_ID)

    // A linked calendar keeps only the holidays both parts agree on, so neither date is one.
    LINKED_GBLO_EUTA_ID.resolve(test).map(cal => cal.isHoliday(EUTA_HOLIDAY)) shouldBe Right(false)
    LINKED_GBLO_EUTA_ID.resolve(test).map(cal => cal.isHoliday(GBLO_HOLIDAY)) shouldBe Right(false)
    LINKED_GBLO_EUTA_ID.resolve(test).map(cal => cal.id) shouldBe Right(LINKED_GBLO_EUTA_ID)

    // The third case of the resolution flow: a host that supplies the composite identifier
    // itself, pre-combined. The whole name is tried first, so its calendar is answered with
    // directly and the parts are never consulted - which the absence of the TARGET holiday
    // from the result is what proves.
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
    // Rule 2 with one part missing, which is the case the rule exists for: the composite
    // identifier is left unresolved by the decoration, so resolution runs part by part and the
    // missing TARGET calendar is defaulted on its own - rather than the whole composite being
    // defaulted, which would have discarded London's holidays.
    val test: ReferenceData =
      HolidaySafeReferenceData(store(ReferenceData.Entry(GBLO_ID, GBLO_CAL)))

    test.findValue(COMBINED_GBLO_EUTA_ID) shouldBe None
    test.getValue(COMBINED_GBLO_EUTA_ID) should beFailureWith(FailureReason.MISSING_DATA)

    COMBINED_GBLO_EUTA_ID.resolve(test) should haveValue(HolidayCalendar.Combined(TEST_EUTA_CAL, GBLO_CAL))
    LINKED_GBLO_EUTA_ID.resolve(test) should haveValue(HolidayCalendar.Linked(TEST_EUTA_CAL, GBLO_CAL))

    // Those two assertions are satisfied by the combination of the previous test as well,
    // because a calendar compares on its identifier alone:
    HolidayCalendar.Combined(TEST_EUTA_CAL, GBLO_CAL) shouldBe HolidayCalendar.Combined(EUTA_CAL, GBLO_CAL)

    // ... so the dates are what distinguish this test from that one. London's holiday survived
    // into the combination and the TARGET holiday did not, because that part was defaulted to
    // a calendar with no holidays at all.
    COMBINED_GBLO_EUTA_ID.resolve(test).map(cal => cal.isHoliday(GBLO_HOLIDAY)) shouldBe Right(true)
    COMBINED_GBLO_EUTA_ID.resolve(test).map(cal => cal.isHoliday(EUTA_HOLIDAY)) shouldBe Right(false)

    // The result is still identified by the whole composite name, so nothing about the
    // identifier was lost by defaulting one of its parts.
    COMBINED_GBLO_EUTA_ID.resolve(test).map(cal => cal.id) shouldBe Right(COMBINED_GBLO_EUTA_ID)
    LINKED_GBLO_EUTA_ID.resolve(test).map(cal => cal.id) shouldBe Right(LINKED_GBLO_EUTA_ID)

    // Both parts missing is the same rule applied twice, and neither part contributes a
    // holiday: the combination is closed at the weekend and on no other day.
    val empty: ReferenceData = HolidaySafeReferenceData(ReferenceData.empty)
    COMBINED_GBLO_EUTA_ID.resolve(empty) should
      haveValue(HolidayCalendar.Combined(TEST_EUTA_CAL, createCalendar(GBLO_ID)))
    COMBINED_GBLO_EUTA_ID.resolve(empty).map(cal => cal.isHoliday(A_SATURDAY)) shouldBe Right(true)
    COMBINED_GBLO_EUTA_ID.resolve(empty).map(cal => cal.isHoliday(GBLO_HOLIDAY)) shouldBe Right(false)
  }

  test("test_combinedCalendar_findValue_referenceDataContainsBoth") {
    // The `ContainsBoth` scenario read through the `Option`-valued surface.
    val test: ReferenceData = HolidaySafeReferenceData(
      store(ReferenceData.Entry(EUTA_ID, EUTA_CAL), ReferenceData.Entry(GBLO_ID, GBLO_CAL)))

    // The store reports nothing for a composite identifier, as in the `getValue` test.
    test.findValue(COMBINED_GBLO_EUTA_ID) shouldBe None
    test.findValue(LINKED_GBLO_EUTA_ID) shouldBe None

    // Resolving the identifier is what answers, and `toOption` is the `Option`-valued form of
    // it - the counterpart of the `Optional` the Java assertions were written against.
    COMBINED_GBLO_EUTA_ID.resolve(test).toOption shouldBe Some(HolidayCalendar.Combined(EUTA_CAL, GBLO_CAL))
    LINKED_GBLO_EUTA_ID.resolve(test).toOption shouldBe Some(HolidayCalendar.Linked(EUTA_CAL, GBLO_CAL))

    // Both parts came from the store.
    COMBINED_GBLO_EUTA_ID.resolve(test).toOption.map(cal => cal.isHoliday(EUTA_HOLIDAY)) shouldBe Some(true)
    COMBINED_GBLO_EUTA_ID.resolve(test).toOption.map(cal => cal.isHoliday(GBLO_HOLIDAY)) shouldBe Some(true)

    // A pre-combined calendar filed under the whole name is found by the store itself, since
    // the identifier it is filed under is the identifier being looked up.
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
    // The `OnlyContainsOne` scenario read through the `Option`-valued surface.
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
    // Rule 1 from the other side, and rule 3. An identifier that is not a calendar identifier
    // is never defaulted, however empty the underlying data is: defaulting one would mean
    // inventing a value of a type this module knows nothing about.
    val test: ReferenceData = HolidaySafeReferenceData(ReferenceData.empty)

    // The Java original made two assertions here, one that its low-level query answered a
    // reference to nothing and one that `findValue` answered an empty `Optional`. They are the
    // same call in this port: `findValue` is the primitive, and `None` is how it reports
    // absence.
    test.findValue(NON_CAL_ID) shouldBe None

    // The Java original asserted a thrown `ReferenceDataNotFoundException`; see the note in
    // `test_singleCalendar_getValue`.
    test.getValue(NON_CAL_ID) should beFailureWith(FailureReason.MISSING_DATA)

    // Rule 3: membership is true for every calendar identifier - one this library defines, one
    // it has never seen, and a composite one - and is the underlying data's answer otherwise.
    test.containsValue(GBLO_ID) shouldBe true
    test.containsValue(UNKNOWN_ID) shouldBe true
    test.containsValue(COMBINED_GBLO_EUTA_ID) shouldBe true
    test.containsValue(LINKED_GBLO_EUTA_ID) shouldBe true
    test.containsValue(NON_CAL_ID) shouldBe false

    // Membership answers true for a composite identifier although the store reports nothing
    // for it, and that is not an inconsistency: what a caller can obtain is the resolved
    // calendar, and it can obtain one.
    test.findValue(COMBINED_GBLO_EUTA_ID) shouldBe None
    COMBINED_GBLO_EUTA_ID.resolve(test) should beSuccess

    // Membership is true for the unknown simple identifier because it does answer for it.
    test.findValue(UNKNOWN_ID).map(calendar => calendar.id) shouldBe Some(UNKNOWN_ID)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java test swept the immutable bean reflectively with `coverImmutableBean` - reading
    // every property and exercising equality, hashing and rendering - and then compared the
    // instance with one built over different data through `coverBeanEquals`. Neither helper
    // has a target here: there is no bean, and this port performs no reflection. What the pair
    // of calls stood for is asserted directly, on the case class the port is.
    val test = HolidaySafeReferenceData(store(ReferenceData.Entry(NON_CAL_ID, NON_CAL_VAL)))
    val test2 = HolidaySafeReferenceData(store(ReferenceData.Entry(TEST_ID, TEST_CAL)))

    // The single property reads back as it was supplied.
    test.underlying shouldBe store(ReferenceData.Entry(NON_CAL_ID, NON_CAL_VAL))

    // Equality is by value: `same` decorates an independently built store holding the same
    // entry, so this asserts that two decorations of equal data are one value rather than that
    // an object equals itself, and that equal values hash alike.
    val same = HolidaySafeReferenceData(store(ReferenceData.Entry(NON_CAL_ID, NON_CAL_VAL)))
    test shouldBe same
    test.hashCode shouldBe same.hashCode

    // Decorating different data is a different value, and the difference is observable.
    test should not be test2
    test.findValue(NON_CAL_ID) shouldBe Some(NON_CAL_VAL)
    test2.findValue(NON_CAL_ID) shouldBe None
    test.toString should include("HolidaySafeReferenceData")
    test.toString should not be test2.toString

    // Rule 4, which the Java test did not reach: combining re-applies the decoration, so
    // defaulting survives however many times reference data is layered. Without the re-wrap,
    // an identifier neither side held would fail again.
    val combined = test.combinedWith(test2.underlying)
    combined shouldBe HolidaySafeReferenceData(test.underlying.combinedWith(test2.underlying))
    combined.findValue(NON_CAL_ID) shouldBe Some(NON_CAL_VAL)
    combined.findValue(TEST_ID) shouldBe Some(TEST_CAL)
    combined.findValue(UNKNOWN_ID).map(calendar => calendar.id) shouldBe Some(UNKNOWN_ID)
    combined.containsValue(UNKNOWN_ID) shouldBe true
  }

  test("test_serialization") {
    // The Java test round-tripped the bean through Java serialization, which no type of this
    // port supports. Reference data additionally has no JSON codec, and that is a property of
    // the type rather than an omission: AAP section 0.6.4 excludes `ReferenceData` and its
    // implementations because a store maps a type-erased identifier to a value whose type is
    // known only through that identifier, so no encoder for an arbitrary store could be
    // written. The one kind of reference data this library does serialize is a holiday
    // calendar, which carries its own codec. Both halves of that are asserted here: that no
    // codec exists for the decoration, and that the calendar it answers with does round-trip.
    val test = HolidaySafeReferenceData(store(ReferenceData.Entry(TEST_ID, TEST_CAL)))

    assertDoesNotCompile("implicitly[io.circe.Encoder[HolidaySafeReferenceData]]")
    assertDoesNotCompile("implicitly[io.circe.Decoder[HolidaySafeReferenceData]]")

    // The stored calendar survives the cycle. Equality compares identifiers alone, so the
    // dates are what show that the weekend and the empty holiday set came across with it.
    val stored = test.findValue(TEST_ID).getOrElse(fail("the stored calendar was not found"))
    val storedAgain = decode[HolidayCalendar](stored.asJson.noSpaces)
      .getOrElse(fail(s"the stored calendar did not survive the round trip: ${stored.asJson.noSpaces}"))
    storedAgain shouldBe stored
    storedAgain.id shouldBe TEST_ID
    storedAgain.isHoliday(A_SATURDAY) shouldBe true
    storedAgain.isHoliday(GBLO_HOLIDAY) shouldBe false

    // And so does a calendar the decoration invented, which is the value that exists only
    // because of this type: a defaulted calendar is an ordinary calendar and is written as
    // one, carrying the identifier that was asked for.
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
   * Builds the calendar the subject supplies for an identifier nothing holds.
   *
   * The Java fixture method of the same name, and the value every defaulted expectation below
   * compares against: a calendar carrying the identifier that was asked for, no holidays, and
   * the Saturday/Sunday weekend.
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
   * reaches the built-in set, so that a fixture is the real London or TARGET calendar rather
   * than one this spec assembled. The contract is strict in both directions, and a breach of
   * either is a failed fixture naming its cause, exactly as an unbuildable [[store]] is: a
   * lookup that does not answer fails, and so does a calendar that answers but does not observe
   * the date the assertions discriminate on, because such a fixture would make every assertion
   * that reads it vacuous. Nothing is substituted for a missing or wrong built-in - a
   * substitute would satisfy every assertion below by construction and would leave the
   * publication of these two calendars unguarded, which no other spec in this module covers.
   * See the note on the class.
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
   * Builds reference data holding the given entries over the minimal set.
   *
   * `ReferenceData.of` is the factory the Java tests used - it lays the caller's entries over
   * the four weekend and no-holiday calendars - and it reports the one way it can fail, two
   * entries filed under the same identifier. A fixture that cannot be built is a defect in this
   * spec rather than a property of the subject, so the failure is reported as a failed test
   * naming its cause.
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
