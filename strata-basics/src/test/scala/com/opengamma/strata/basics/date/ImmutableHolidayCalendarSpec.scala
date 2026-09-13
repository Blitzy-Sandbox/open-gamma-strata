/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Modifier
import java.time.DayOfWeek
import java.time.DayOfWeek.FRIDAY
import java.time.DayOfWeek.SATURDAY
import java.time.DayOfWeek.SUNDAY
import java.time.DayOfWeek.THURSDAY
import java.time.LocalDate

import scala.collection.immutable.SortedSet
import scala.util.Random

import cats.Eq
import cats.Hash
import cats.Show

import io.circe.parser.decode
import io.circe.parser.parse
import io.circe.syntax.EncoderOps

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableFor3

import com.opengamma.strata.collect.testkit.TestHelper._

/**
 * Test [[ImmutableHolidayCalendar]].
 *
 * '''Equality is the identifier alone.''' A calendar compares equal on its identifier and nothing
 * else, which `test_equals` records by asserting that two calendars with different holidays and
 * the same identifier '''are''' equal. Every assertion about the content of a calendar therefore
 * goes through [[ImmutableHolidayCalendar.holidays]], [[ImmutableHolidayCalendar.workingDays]] or
 * [[HolidayCalendar.isHoliday]], never through `==`.
 *
 * '''What a calendar holds.''' Holidays are sorted and deduplicated, the year range runs from the
 * earliest to the latest holiday, working days are applied last and override both holidays and
 * weekend days, and a working day outside that range is ignored. The dates are stored as one bit
 * mask per month of the range: a date outside the range is answered from the weekend alone, and
 * the holiday and working-day sets are recovered by scanning the stored months.
 *
 * '''The one precondition.''' A calendar can neither answer for, nor hold data for, a date whose
 * year lies outside 0 to 9999. That is a fault in the calling code rather than a property of
 * market data, so it fails fast through `ArgCheck` - see [[rejectsUnsupportedDate]] - while in a
 * JSON document the same rule is a decoding failure, a document being data.
 *
 * @see [[HolidayCalendar]] for the sealed family and its composites
 * @see [[HolidayCalendarId]] for the identifiers, whose composition this spec relies on
 */
final class ImmutableHolidayCalendarSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  private val TEST_ID: HolidayCalendarId = HolidayCalendarId.of("Test1")

  private val TEST_ID2: HolidayCalendarId = HolidayCalendarId.of("Test2")

  private val MON_2014_06_30: LocalDate = LocalDate.of(2014, 6, 30)
  private val TUE_2014_07_08: LocalDate = LocalDate.of(2014, 7, 8)
  private val WED_2014_07_09: LocalDate = LocalDate.of(2014, 7, 9)
  private val THU_2014_07_10: LocalDate = LocalDate.of(2014, 7, 10)
  private val FRI_2014_07_11: LocalDate = LocalDate.of(2014, 7, 11)
  private val SAT_2014_07_12: LocalDate = LocalDate.of(2014, 7, 12)
  private val SUN_2014_07_13: LocalDate = LocalDate.of(2014, 7, 13)
  private val MON_2014_07_14: LocalDate = LocalDate.of(2014, 7, 14)
  private val TUE_2014_07_15: LocalDate = LocalDate.of(2014, 7, 15)
  private val WED_2014_07_16: LocalDate = LocalDate.of(2014, 7, 16)
  private val THU_2014_07_17: LocalDate = LocalDate.of(2014, 7, 17)
  private val FRI_2014_07_18: LocalDate = LocalDate.of(2014, 7, 18)
  private val SAT_2014_07_19: LocalDate = LocalDate.of(2014, 7, 19)
  private val SUN_2014_07_20: LocalDate = LocalDate.of(2014, 7, 20)
  private val MON_2014_07_21: LocalDate = LocalDate.of(2014, 7, 21)
  private val TUE_2014_07_22: LocalDate = LocalDate.of(2014, 7, 22)
  private val WED_2014_07_23: LocalDate = LocalDate.of(2014, 7, 23)
  private val WED_2014_07_30: LocalDate = LocalDate.of(2014, 7, 30)
  private val THU_2014_07_31: LocalDate = LocalDate.of(2014, 7, 31)
  private val MON_2014_12_29: LocalDate = LocalDate.of(2014, 12, 29)
  private val TUE_2014_12_30: LocalDate = LocalDate.of(2014, 12, 30)
  private val WED_2014_12_31: LocalDate = LocalDate.of(2014, 12, 31)

  private val THU_2015_01_01: LocalDate = LocalDate.of(2015, 1, 1)
  private val FRI_2015_01_02: LocalDate = LocalDate.of(2015, 1, 2)
  private val SAT_2015_01_03: LocalDate = LocalDate.of(2015, 1, 3)
  private val MON_2015_01_05: LocalDate = LocalDate.of(2015, 1, 5)
  private val FRI_2015_02_27: LocalDate = LocalDate.of(2015, 2, 27)
  private val SAT_2015_02_28: LocalDate = LocalDate.of(2015, 2, 28)
  private val SAT_2015_03_28: LocalDate = LocalDate.of(2015, 3, 28)
  private val SUN_2015_03_29: LocalDate = LocalDate.of(2015, 3, 29)
  private val MON_2015_03_30: LocalDate = LocalDate.of(2015, 3, 30)
  private val TUE_2015_03_31: LocalDate = LocalDate.of(2015, 3, 31)
  private val WED_2015_04_01: LocalDate = LocalDate.of(2015, 4, 1)
  private val THU_2015_04_02: LocalDate = LocalDate.of(2015, 4, 2)

  private val SAT_2018_07_14: LocalDate = LocalDate.of(2018, 7, 14)
  private val SUN_2018_07_15: LocalDate = LocalDate.of(2018, 7, 15)
  private val MON_2018_07_16: LocalDate = LocalDate.of(2018, 7, 16)
  private val TUE_2018_07_17: LocalDate = LocalDate.of(2018, 7, 17)
  private val WED_2018_07_18: LocalDate = LocalDate.of(2018, 7, 18)

  private val HOLCAL_MON_WED: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(TEST_ID, List(MON_2014_07_14, WED_2014_07_16), SATURDAY, SUNDAY)

  /** A calendar whose holidays straddle a year end, exercising a step across stored years. */
  private val HOLCAL_YEAR_END: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(
      HolidayCalendarId.of("TestYearEnd"),
      List(TUE_2014_12_30, THU_2015_01_01),
      SATURDAY,
      SUNDAY)

  /** A calendar with no holidays, so it stores no months and only its weekend applies. */
  private val HOLCAL_SAT_SUN: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(
      HolidayCalendarId.of("TestSatSun"),
      List.empty[LocalDate],
      SATURDAY,
      SUNDAY)

  private val HOLCAL_END_MONTH: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(
      HolidayCalendarId.of("TestEndOfMonth"),
      List(MON_2014_06_30, THU_2014_07_31),
      SATURDAY,
      SUNDAY)

  /** The part of the `ArgCheck` message that names the precondition on the year of a date. */
  private val UnsupportedDateMessage: String = "outside the accepted range"

  /**
   * The part of the refusal that names the precondition on the weekend of a calendar.
   *
   * One string serves both routes the precondition is stated on - the raised refusal a caller
   * of a factory gets and the decoding failure a document gets - because both name the
   * condition in the same words, and asserting the words rather than only the type keeps a
   * refusal raised for some other reason from satisfying the assertion.
   */
  private val EveryDayClosedMessage: String = "leave at least one day of the week open"

  //-------------------------------------------------------------------------
  /** Returns the weekend days sorted, the accessor's set carrying no order in its contract. */
  private def weekendDaysOf(calendar: ImmutableHolidayCalendar): List[DayOfWeek] =
    calendar.weekendDays.toList.sortBy(day => day.getValue)

  /** The days of a range in ascending order, for comparing two calendars day by day. */
  private def datesFrom(startInclusive: LocalDate, endExclusive: LocalDate): Iterator[LocalDate] =
    Iterator.iterate(startInclusive)(date => date.plusDays(1L)).takeWhile(date => date.isBefore(endExclusive))

  /**
   * Asserts that an operation rejects a date whose year lies outside 0 to 9999: a caller-contract
   * precondition rather than a data failure, so a fail-fast `ArgCheck` throw. The message is
   * asserted as well as the exception, so an `IllegalArgumentException` raised for another reason
   * cannot satisfy the assertion.
   */
  private def rejectsUnsupportedDate(operation: => Any): Assertion =
    intercept[IllegalArgumentException](operation).getMessage should include(UnsupportedDateMessage)

  //-------------------------------------------------------------------------
  test("test_of_IterableDayOfWeekDayOfWeek_null") {
    val holidays = List(MON_2014_07_14, FRI_2014_07_18)
    // No argument of this factory is optional and each has its own type, so a missing or
    // misplaced argument is a compile error rather than something a `null` check could report.
    assertDoesNotCompile("ImmutableHolidayCalendar.of(holidays, SATURDAY, SUNDAY)") // the id
    assertDoesNotCompile("ImmutableHolidayCalendar.of(TEST_ID, SATURDAY, SUNDAY)") // the holidays
    assertDoesNotCompile("ImmutableHolidayCalendar.of(TEST_ID, holidays, holidays, SUNDAY)") // first weekend day
    assertDoesNotCompile("ImmutableHolidayCalendar.of(TEST_ID, holidays, SATURDAY)") // second weekend day
    val test = ImmutableHolidayCalendar.of(TEST_ID, holidays, SATURDAY, SUNDAY)
    test.id shouldBe TEST_ID
    test.holidays.toList shouldBe holidays
  }

  test("test_of_IterableIterable_null") {
    val holidays = List(MON_2014_07_14, FRI_2014_07_18)
    val weekendDays = List(THURSDAY, FRIDAY)
    assertDoesNotCompile("ImmutableHolidayCalendar.of(holidays, weekendDays)") // the id
    assertDoesNotCompile("ImmutableHolidayCalendar.of(TEST_ID, weekendDays, weekendDays)") // the holidays
    assertDoesNotCompile("ImmutableHolidayCalendar.of(TEST_ID, holidays)") // the weekend days
    val test = ImmutableHolidayCalendar.of(TEST_ID, holidays, weekendDays)
    test.id shouldBe TEST_ID
    weekendDaysOf(test) shouldBe List(THURSDAY, FRIDAY)
    test.isHoliday(MON_2014_07_14) shouldBe true
    test.isHoliday(FRI_2014_07_18) shouldBe true
    // The accessor reports the declared holiday alone: the stored months record a day as a
    // business day or not, so a date the weekend already closes leaves no mark there.
    test.holidays.toList shouldBe List(MON_2014_07_14)
  }

  //-------------------------------------------------------------------------
  private val data_createSatSunWeekend: TableFor2[LocalDate, Boolean] = Table(
    ("date", "isBusinessDay"),
    (FRI_2014_07_11, true),
    (SAT_2014_07_12, false),
    (SUN_2014_07_13, false),
    (MON_2014_07_14, false),
    (TUE_2014_07_15, true),
    (WED_2014_07_16, true),
    (THU_2014_07_17, true),
    (FRI_2014_07_18, false),
    (SAT_2014_07_19, false),
    (SUN_2014_07_20, false),
    (MON_2014_07_21, true)
  )

  test("test_of_IterableDayOfWeekDayOfWeek_satSunWeekend") {
    val holidays = List(MON_2014_07_14, FRI_2014_07_18)
    val test = ImmutableHolidayCalendar.of(TEST_ID, holidays, SATURDAY, SUNDAY)
    forAll(data_createSatSunWeekend) { (date: LocalDate, isBusinessDay: Boolean) =>
      withClue(s"$date: ") {
        test.isBusinessDay(date) shouldBe isBusinessDay
        test.isHoliday(date) shouldBe !isBusinessDay
      }
    }
    test.holidays.toList shouldBe holidays
    weekendDaysOf(test) shouldBe List(SATURDAY, SUNDAY)
    test.toString shouldBe s"HolidayCalendar[${TEST_ID.name}]"
  }

  test("test_of_IterableIterable_satSunWeekend") {
    val holidays = List(MON_2014_07_14, FRI_2014_07_18)
    val weekendDays = List(SATURDAY, SUNDAY)
    val test = ImmutableHolidayCalendar.of(TEST_ID, holidays, weekendDays)
    forAll(data_createSatSunWeekend) { (date: LocalDate, isBusinessDay: Boolean) =>
      withClue(s"$date: ") {
        test.isBusinessDay(date) shouldBe isBusinessDay
        test.isHoliday(date) shouldBe !isBusinessDay
      }
    }
    test.holidays.toList shouldBe holidays
    weekendDaysOf(test) shouldBe List(SATURDAY, SUNDAY)

    // The factory normalises what it is given: the same holidays supplied out of order and
    // duplicated describe the same calendar, and the set reported back is sorted and deduplicated.
    val unordered =
      ImmutableHolidayCalendar.of(TEST_ID, List(FRI_2014_07_18, MON_2014_07_14, FRI_2014_07_18), weekendDays)
    unordered.holidays.toList shouldBe holidays
    unordered.startYear shouldBe 2014
    unordered.endYearExclusive shouldBe 2015
    // ... and the two answer alike on every day they hold data for, compared day by day
    datesFrom(LocalDate.of(2014, 1, 1), LocalDate.of(2015, 1, 1)).foreach { date =>
      withClue(s"$date: ") {
        unordered.isHoliday(date) shouldBe test.isHoliday(date)
      }
    }
    succeed
  }

  //-------------------------------------------------------------------------
  private val data_createThuFriWeekend: TableFor2[LocalDate, Boolean] = Table(
    ("date", "isBusinessDay"),
    (FRI_2014_07_11, false),
    (SAT_2014_07_12, true),
    (SUN_2014_07_13, true),
    (MON_2014_07_14, false),
    (TUE_2014_07_15, true),
    (WED_2014_07_16, true),
    (THU_2014_07_17, false),
    (FRI_2014_07_18, false),
    (SAT_2014_07_19, false),
    (SUN_2014_07_20, true),
    (MON_2014_07_21, true)
  )

  test("test_of_IterableDayOfWeekDayOfWeek_thuFriWeekend") {
    val holidays = List(MON_2014_07_14, SAT_2014_07_19)
    val test = ImmutableHolidayCalendar.of(TEST_ID, holidays, THURSDAY, FRIDAY)
    forAll(data_createThuFriWeekend) { (date: LocalDate, isBusinessDay: Boolean) =>
      withClue(s"$date: ") {
        test.isBusinessDay(date) shouldBe isBusinessDay
        test.isHoliday(date) shouldBe !isBusinessDay
      }
    }
    test.holidays.toList shouldBe holidays
    weekendDaysOf(test) shouldBe List(THURSDAY, FRIDAY)
    test.toString shouldBe s"HolidayCalendar[${TEST_ID.name}]"
  }

  test("test_of_IterableIterable_thuFriWeekend") {
    val holidays = List(MON_2014_07_14, SAT_2014_07_19)
    val weekendDays = List(THURSDAY, FRIDAY)
    val test = ImmutableHolidayCalendar.of(TEST_ID, holidays, weekendDays)
    forAll(data_createThuFriWeekend) { (date: LocalDate, isBusinessDay: Boolean) =>
      withClue(s"$date: ") {
        test.isBusinessDay(date) shouldBe isBusinessDay
        test.isHoliday(date) shouldBe !isBusinessDay
      }
    }
    test.holidays.toList shouldBe holidays
    weekendDaysOf(test) shouldBe List(THURSDAY, FRIDAY)
  }

  //-------------------------------------------------------------------------
  private val data_createSunWeekend: TableFor2[LocalDate, Boolean] = Table(
    ("date", "isBusinessDay"),
    (FRI_2014_07_11, true),
    (SAT_2014_07_12, true),
    (SUN_2014_07_13, false),
    (MON_2014_07_14, false),
    (TUE_2014_07_15, true),
    (WED_2014_07_16, true),
    (THU_2014_07_17, false),
    (FRI_2014_07_18, true),
    (SAT_2014_07_19, true),
    (SUN_2014_07_20, false),
    (MON_2014_07_21, true)
  )

  test("test_of_IterableDayOfWeekDayOfWeek_sunWeekend") {
    val holidays = List(MON_2014_07_14, THU_2014_07_17)
    val test = ImmutableHolidayCalendar.of(TEST_ID, holidays, SUNDAY, SUNDAY)
    forAll(data_createSunWeekend) { (date: LocalDate, isBusinessDay: Boolean) =>
      withClue(s"$date: ") {
        test.isBusinessDay(date) shouldBe isBusinessDay
        test.isHoliday(date) shouldBe !isBusinessDay
      }
    }
    test.holidays.toList shouldBe holidays
    // the same day named twice is one weekend day, not two
    test.weekendDays shouldBe Set(SUNDAY)
    test.toString shouldBe s"HolidayCalendar[${TEST_ID.name}]"
  }

  test("test_of_IterableIterable_sunWeekend") {
    val holidays = List(MON_2014_07_14, THU_2014_07_17)
    val weekendDays = List(SUNDAY)
    val test = ImmutableHolidayCalendar.of(TEST_ID, holidays, weekendDays)
    forAll(data_createSunWeekend) { (date: LocalDate, isBusinessDay: Boolean) =>
      withClue(s"$date: ") {
        test.isBusinessDay(date) shouldBe isBusinessDay
        test.isHoliday(date) shouldBe !isBusinessDay
      }
    }
    test.holidays.toList shouldBe holidays
    test.weekendDays shouldBe Set(SUNDAY)
  }

  //-------------------------------------------------------------------------
  private val data_createThuFriSatWeekend: TableFor2[LocalDate, Boolean] = Table(
    ("date", "isBusinessDay"),
    (FRI_2014_07_11, false),
    (SAT_2014_07_12, false),
    (SUN_2014_07_13, true),
    (MON_2014_07_14, false),
    (TUE_2014_07_15, false),
    (WED_2014_07_16, true),
    (THU_2014_07_17, false),
    (FRI_2014_07_18, false),
    (SAT_2014_07_19, false),
    (SUN_2014_07_20, true),
    (MON_2014_07_21, true)
  )

  test("test_of_IterableIterable_thuFriSatWeekend") {
    val holidays = List(MON_2014_07_14, TUE_2014_07_15)
    val weekendDays = List(THURSDAY, FRIDAY, SATURDAY)
    val test = ImmutableHolidayCalendar.of(TEST_ID, holidays, weekendDays)
    forAll(data_createThuFriSatWeekend) { (date: LocalDate, isBusinessDay: Boolean) =>
      withClue(s"$date: ") {
        test.isBusinessDay(date) shouldBe isBusinessDay
        test.isHoliday(date) shouldBe !isBusinessDay
      }
    }
    test.holidays.toList shouldBe holidays
    weekendDaysOf(test) shouldBe List(THURSDAY, FRIDAY, SATURDAY)
    test.toString shouldBe s"HolidayCalendar[${TEST_ID.name}]"
  }

  //-------------------------------------------------------------------------
  private val data_createNoWeekends: TableFor2[LocalDate, Boolean] = Table(
    ("date", "isBusinessDay"),
    (FRI_2014_07_11, true),
    (SAT_2014_07_12, true),
    (SUN_2014_07_13, true),
    (MON_2014_07_14, false),
    (TUE_2014_07_15, true),
    (WED_2014_07_16, true),
    (THU_2014_07_17, true),
    (FRI_2014_07_18, false),
    (SAT_2014_07_19, true),
    (SUN_2014_07_20, true),
    (MON_2014_07_21, true)
  )

  test("test_of_IterableIterable_noWeekends") {
    val holidays = List(MON_2014_07_14, FRI_2014_07_18)
    val weekendDays = List.empty[DayOfWeek]
    val test = ImmutableHolidayCalendar.of(TEST_ID, holidays, weekendDays)
    forAll(data_createNoWeekends) { (date: LocalDate, isBusinessDay: Boolean) =>
      withClue(s"$date: ") {
        test.isBusinessDay(date) shouldBe isBusinessDay
        test.isHoliday(date) shouldBe !isBusinessDay
      }
    }
    test.holidays.toList shouldBe holidays
    test.weekendDays shouldBe Set.empty[DayOfWeek]
    test.toString shouldBe s"HolidayCalendar[${TEST_ID.name}]"
  }

  //-------------------------------------------------------------------------
  private val data_createNoHolidays: TableFor2[LocalDate, Boolean] = Table(
    ("date", "isBusinessDay"),
    (FRI_2014_07_11, false),
    (SAT_2014_07_12, false),
    (SUN_2014_07_13, true),
    (MON_2014_07_14, true),
    (TUE_2014_07_15, true),
    (WED_2014_07_16, true),
    (THU_2014_07_17, true),
    (FRI_2014_07_18, false),
    (SAT_2014_07_19, false),
    (SUN_2014_07_20, true),
    (MON_2014_07_21, true)
  )

  test("test_of_IterableIterable_noHolidays") {
    val holidays = List.empty[LocalDate]
    val weekendDays = List(FRIDAY, SATURDAY)
    val test = ImmutableHolidayCalendar.of(TEST_ID, holidays, weekendDays)
    forAll(data_createNoHolidays) { (date: LocalDate, isBusinessDay: Boolean) =>
      withClue(s"$date: ") {
        test.isBusinessDay(date) shouldBe isBusinessDay
        test.isHoliday(date) shouldBe !isBusinessDay
      }
    }
    test.holidays.toList shouldBe holidays
    weekendDaysOf(test) shouldBe List(FRIDAY, SATURDAY)
    test.toString shouldBe s"HolidayCalendar[${TEST_ID.name}]"
  }

  //-------------------------------------------------------------------------
  /**
   * Each row reads one of the four ways a day is decided: business day, declared holiday, weekend
   * day, and weekend day overridden into a business day. The Sunday is a business day because it
   * is not part of this calendar's weekend.
   */
  private val data_createWorkingDayOverrides: TableFor2[LocalDate, Boolean] = Table(
    ("date", "isBusinessDay"),
    (TUE_2014_07_08, true),
    (WED_2014_07_09, false),
    (THU_2014_07_10, false),
    (FRI_2014_07_11, false),
    (SAT_2014_07_12, true),
    (SUN_2014_07_13, true)
  )

  test("test_of_IterableIterableIterable") {
    val holidays = List(WED_2014_07_09, THU_2014_07_10)
    val weekendDays = List(FRIDAY, SATURDAY)
    val workingDays = List(SAT_2014_07_12)
    val test = ImmutableHolidayCalendar.of(TEST_ID, holidays, weekendDays, workingDays)
    forAll(data_createWorkingDayOverrides) { (date: LocalDate, isBusinessDay: Boolean) =>
      withClue(s"$date: ") {
        test.isBusinessDay(date) shouldBe isBusinessDay
        test.isHoliday(date) shouldBe !isBusinessDay
      }
    }
    test.holidays.toList shouldBe holidays
    weekendDaysOf(test) shouldBe List(FRIDAY, SATURDAY)
    test.workingDays.toList shouldBe workingDays
    test.toString shouldBe s"HolidayCalendar[${TEST_ID.name}]"

    // Working days are applied last and override a declared holiday as well as a weekend: the
    // Thursday, named as both, is a business day. It is not reported as a working day, that
    // accessor reporting the weekend days a calendar treats as business days.
    val overridesHoliday =
      ImmutableHolidayCalendar.of(TEST_ID, holidays, weekendDays, List(THU_2014_07_10))
    overridesHoliday.isBusinessDay(THU_2014_07_10) shouldBe true
    overridesHoliday.holidays.toList shouldBe List(WED_2014_07_09)
    overridesHoliday.workingDays.toList shouldBe List.empty[LocalDate]

    // A working day outside the years the holidays span is ignored, there being no data there for
    // it to override: the Saturday of January 2015 stays a holiday, the one inside takes effect.
    val outsideRange =
      ImmutableHolidayCalendar.of(TEST_ID, holidays, weekendDays, List(SAT_2014_07_12, SAT_2015_01_03))
    outsideRange.startYear shouldBe 2014
    outsideRange.endYearExclusive shouldBe 2015
    outsideRange.workingDays.toList shouldBe List(SAT_2014_07_12)
    outsideRange.isBusinessDay(SAT_2014_07_12) shouldBe true
    outsideRange.isBusinessDay(SAT_2015_01_03) shouldBe false
  }

  test("test_of_IterableIterableIterable_combined") {
    val holidays = List(WED_2014_07_09, THU_2014_07_10)
    val weekendDays = List(FRIDAY, SATURDAY)
    val workingDays = List(SAT_2014_07_12)
    val base1 = ImmutableHolidayCalendar.of(TEST_ID, holidays, weekendDays, workingDays)
    val base2 = ImmutableHolidayCalendar.of(TEST_ID, List(date(2010, 6, 1)), weekendDays)
    val test = ImmutableHolidayCalendar.combined(base1, base2)
    forAll(data_createWorkingDayOverrides) { (date: LocalDate, isBusinessDay: Boolean) =>
      withClue(s"$date: ") {
        test.isBusinessDay(date) shouldBe isBusinessDay
        test.isHoliday(date) shouldBe !isBusinessDay
      }
    }
    // the ranges are four years apart, so the merged calendar is rebuilt from the dates both
    // declare, and carries the working-day override
    test.holidays.toList shouldBe List(date(2010, 6, 1), WED_2014_07_09, THU_2014_07_10)
    weekendDaysOf(test) shouldBe List(FRIDAY, SATURDAY)
    test.workingDays.toList shouldBe workingDays
    test.startYear shouldBe 2010
    test.endYearExclusive shouldBe 2015
    test.toString shouldBe s"HolidayCalendar[${TEST_ID.name}]"
  }

  //-------------------------------------------------------------------------
  test("test_combined") {
    val base1 = ImmutableHolidayCalendar.of(TEST_ID, List(MON_2014_07_14), SATURDAY, SUNDAY)
    val base2 = ImmutableHolidayCalendar.of(TEST_ID2, List(WED_2014_07_16), FRIDAY, SATURDAY)

    val test = ImmutableHolidayCalendar.combined(base1, base2)
    test.id shouldBe base1.id.combinedWith(base2.id)
    test.name shouldBe base1.id.combinedWith(base2.id).name
    // the holidays of both calendars are observed, and either weekend closes a day
    test.holidays.toList shouldBe List(MON_2014_07_14, WED_2014_07_16)
    weekendDaysOf(test) shouldBe List(FRIDAY, SATURDAY, SUNDAY)
  }

  test("test_combined_same") {
    val base = ImmutableHolidayCalendar.of(TEST_ID, List(MON_2014_07_14), SATURDAY, SUNDAY)

    // merging a calendar with itself has nothing to merge, and answers with that calendar
    val test = ImmutableHolidayCalendar.combined(base, base)
    test should be theSameInstanceAs base
  }

  test("test_combined_differentStartYear1") {
    // the merge aligns the two arrays of months before intersecting them: an off-by-one would
    // move a holiday by a month, which the dates in March and April 2015 detect
    val base1 = ImmutableHolidayCalendar.of(TEST_ID, List(WED_2015_04_01), SATURDAY, SUNDAY)
    val base2 = ImmutableHolidayCalendar.of(TEST_ID2, List(MON_2014_07_14, TUE_2015_03_31), SATURDAY, SUNDAY)
    val test: HolidayCalendar = ImmutableHolidayCalendar.combined(base1, base2)
    test.name shouldBe "Test1+Test2"

    test.isHoliday(THU_2014_07_10) shouldBe false
    test.isHoliday(FRI_2014_07_11) shouldBe false
    test.isHoliday(SAT_2014_07_12) shouldBe true
    test.isHoliday(SUN_2014_07_13) shouldBe true
    test.isHoliday(MON_2014_07_14) shouldBe true
    test.isHoliday(TUE_2014_07_15) shouldBe false

    test.isHoliday(MON_2015_03_30) shouldBe false
    test.isHoliday(TUE_2015_03_31) shouldBe true
    test.isHoliday(WED_2015_04_01) shouldBe true
    test.isHoliday(THU_2015_04_02) shouldBe false
  }

  test("test_combined_differentStartYear2") {
    // the same pair the other way round: the merge takes the earlier start as its base
    val base1 = ImmutableHolidayCalendar.of(TEST_ID, List(MON_2014_07_14, TUE_2015_03_31), SATURDAY, SUNDAY)
    val base2 = ImmutableHolidayCalendar.of(TEST_ID2, List(WED_2015_04_01), SATURDAY, SUNDAY)
    val test: HolidayCalendar = ImmutableHolidayCalendar.combined(base1, base2)
    test.name shouldBe "Test1+Test2"

    test.isHoliday(THU_2014_07_10) shouldBe false
    test.isHoliday(FRI_2014_07_11) shouldBe false
    test.isHoliday(SAT_2014_07_12) shouldBe true
    test.isHoliday(SUN_2014_07_13) shouldBe true
    test.isHoliday(MON_2014_07_14) shouldBe true
    test.isHoliday(TUE_2014_07_15) shouldBe false

    test.isHoliday(MON_2015_03_30) shouldBe false
    test.isHoliday(TUE_2015_03_31) shouldBe true
    test.isHoliday(WED_2015_04_01) shouldBe true
    test.isHoliday(THU_2015_04_02) shouldBe false
  }

  test("test_combined_splitYears") {
    // the ranges do not meet - 2018 against 2015 - so the merge covers the years between
    val base1 = ImmutableHolidayCalendar.of(TEST_ID, List(TUE_2018_07_17), SATURDAY, SUNDAY)
    val base2 = ImmutableHolidayCalendar.of(TEST_ID2, List(WED_2015_04_01), SATURDAY, SUNDAY)
    val test: HolidayCalendar = ImmutableHolidayCalendar.combined(base1, base2)
    test.name shouldBe "Test1+Test2"

    test.isHoliday(SAT_2014_07_12) shouldBe true
    test.isHoliday(SUN_2014_07_13) shouldBe true

    test.isHoliday(SAT_2015_03_28) shouldBe true
    test.isHoliday(SUN_2015_03_29) shouldBe true
    test.isHoliday(MON_2015_03_30) shouldBe false
    test.isHoliday(TUE_2015_03_31) shouldBe false
    test.isHoliday(WED_2015_04_01) shouldBe true
    test.isHoliday(THU_2015_04_02) shouldBe false

    test.isHoliday(SAT_2018_07_14) shouldBe true
    test.isHoliday(SUN_2018_07_15) shouldBe true
    test.isHoliday(MON_2018_07_16) shouldBe false
    test.isHoliday(TUE_2018_07_17) shouldBe true
    test.isHoliday(WED_2018_07_18) shouldBe false
  }

  //-------------------------------------------------------------------------
  test("test_isBusinessDay_outOfRange") {
    val test = ImmutableHolidayCalendar.of(TEST_ID, List(MON_2014_07_14, TUE_2014_07_15), SATURDAY, SUNDAY)
    // outside the years its holidays span, the calendar applies its weekend alone
    test.isBusinessDay(LocalDate.of(2013, 12, 31)) shouldBe true
    test.isBusinessDay(LocalDate.of(2015, 1, 1)) shouldBe true
    rejectsUnsupportedDate(test.isBusinessDay(LocalDate.MIN))
    rejectsUnsupportedDate(test.isBusinessDay(LocalDate.MAX))
  }

  //-------------------------------------------------------------------------
  /**
   * Shifts over a fortnight containing two holidays and two weekends. A shift of zero returns the
   * date given, business day or not, which the third block pins.
   */
  private val data_shift: TableFor3[LocalDate, Int, LocalDate] = Table(
    ("date", "amount", "expected"),
    (THU_2014_07_10, 1, FRI_2014_07_11),
    (FRI_2014_07_11, 1, TUE_2014_07_15),
    (SAT_2014_07_12, 1, TUE_2014_07_15),
    (SUN_2014_07_13, 1, TUE_2014_07_15),
    (MON_2014_07_14, 1, TUE_2014_07_15),
    (TUE_2014_07_15, 1, THU_2014_07_17),
    (WED_2014_07_16, 1, THU_2014_07_17),
    (THU_2014_07_17, 1, FRI_2014_07_18),
    (FRI_2014_07_18, 1, MON_2014_07_21),
    (SAT_2014_07_19, 1, MON_2014_07_21),
    (SUN_2014_07_20, 1, MON_2014_07_21),
    (MON_2014_07_21, 1, TUE_2014_07_22),
    (THU_2014_07_10, 2, TUE_2014_07_15),
    (FRI_2014_07_11, 2, THU_2014_07_17),
    (SAT_2014_07_12, 2, THU_2014_07_17),
    (SUN_2014_07_13, 2, THU_2014_07_17),
    (MON_2014_07_14, 2, THU_2014_07_17),
    (TUE_2014_07_15, 2, FRI_2014_07_18),
    (WED_2014_07_16, 2, FRI_2014_07_18),
    (THU_2014_07_17, 2, MON_2014_07_21),
    (FRI_2014_07_18, 2, TUE_2014_07_22),
    (SAT_2014_07_19, 2, TUE_2014_07_22),
    (SUN_2014_07_20, 2, TUE_2014_07_22),
    (MON_2014_07_21, 2, WED_2014_07_23),
    (THU_2014_07_10, 0, THU_2014_07_10),
    (FRI_2014_07_11, 0, FRI_2014_07_11),
    (SAT_2014_07_12, 0, SAT_2014_07_12),
    (SUN_2014_07_13, 0, SUN_2014_07_13),
    (MON_2014_07_14, 0, MON_2014_07_14),
    (TUE_2014_07_15, 0, TUE_2014_07_15),
    (WED_2014_07_16, 0, WED_2014_07_16),
    (THU_2014_07_17, 0, THU_2014_07_17),
    (FRI_2014_07_18, 0, FRI_2014_07_18),
    (SAT_2014_07_19, 0, SAT_2014_07_19),
    (SUN_2014_07_20, 0, SUN_2014_07_20),
    (MON_2014_07_21, 0, MON_2014_07_21),
    (FRI_2014_07_11, -1, THU_2014_07_10),
    (SAT_2014_07_12, -1, FRI_2014_07_11),
    (SUN_2014_07_13, -1, FRI_2014_07_11),
    (MON_2014_07_14, -1, FRI_2014_07_11),
    (TUE_2014_07_15, -1, FRI_2014_07_11),
    (WED_2014_07_16, -1, TUE_2014_07_15),
    (THU_2014_07_17, -1, TUE_2014_07_15),
    (FRI_2014_07_18, -1, THU_2014_07_17),
    (SAT_2014_07_19, -1, FRI_2014_07_18),
    (SUN_2014_07_20, -1, FRI_2014_07_18),
    (MON_2014_07_21, -1, FRI_2014_07_18),
    (TUE_2014_07_22, -1, MON_2014_07_21),
    (FRI_2014_07_11, -2, WED_2014_07_09),
    (SAT_2014_07_12, -2, THU_2014_07_10),
    (SUN_2014_07_13, -2, THU_2014_07_10),
    (MON_2014_07_14, -2, THU_2014_07_10),
    (TUE_2014_07_15, -2, THU_2014_07_10),
    (WED_2014_07_16, -2, FRI_2014_07_11),
    (THU_2014_07_17, -2, FRI_2014_07_11),
    (FRI_2014_07_18, -2, TUE_2014_07_15),
    (SAT_2014_07_19, -2, THU_2014_07_17),
    (SUN_2014_07_20, -2, THU_2014_07_17),
    (MON_2014_07_21, -2, THU_2014_07_17),
    (TUE_2014_07_22, -2, FRI_2014_07_18)
  )

  test("test_shift") {
    forAll(data_shift) { (date: LocalDate, amount: Int, expected: LocalDate) =>
      withClue(s"$date shifted by $amount: ") {
        HOLCAL_MON_WED.shift(date, amount) shouldBe expected
      }
    }
  }

  test("test_shift_SatSun") {
    HOLCAL_SAT_SUN.shift(SAT_2014_07_12, -2) shouldBe THU_2014_07_10
    HOLCAL_SAT_SUN.shift(SAT_2014_07_12, 2) shouldBe TUE_2014_07_15
  }

  test("test_shift_range") {
    HOLCAL_MON_WED.shift(date(2010, 1, 1), 1) shouldBe date(2010, 1, 4)
    rejectsUnsupportedDate(HOLCAL_MON_WED.shift(LocalDate.MIN, 1))
    rejectsUnsupportedDate(HOLCAL_MON_WED.shift(LocalDate.MAX.minusDays(1L), 1))
  }

  test("test_adjustBy") {
    forAll(data_shift) { (date: LocalDate, amount: Int, expected: LocalDate) =>
      withClue(s"$date adjusted by $amount: ") {
        // the same shift, called directly or handed to a date as the temporal adjuster it is
        HOLCAL_MON_WED.adjustBy(amount).adjust(date) shouldBe expected
        date.`with`(HOLCAL_MON_WED.adjustBy(amount)) shouldBe expected
      }
    }
  }

  //-------------------------------------------------------------------------
  /**
   * The second block steps from one stored year into the next; the last row uses a calendar that
   * stores no months at all.
   */
  private val data_next: TableFor3[LocalDate, LocalDate, ImmutableHolidayCalendar] = Table(
    ("date", "expectedNext", "calendar"),
    (THU_2014_07_10, FRI_2014_07_11, HOLCAL_MON_WED),
    (FRI_2014_07_11, TUE_2014_07_15, HOLCAL_MON_WED),
    (SAT_2014_07_12, TUE_2014_07_15, HOLCAL_MON_WED),
    (SUN_2014_07_13, TUE_2014_07_15, HOLCAL_MON_WED),
    (MON_2014_07_14, TUE_2014_07_15, HOLCAL_MON_WED),
    (TUE_2014_07_15, THU_2014_07_17, HOLCAL_MON_WED),
    (WED_2014_07_16, THU_2014_07_17, HOLCAL_MON_WED),
    (THU_2014_07_17, FRI_2014_07_18, HOLCAL_MON_WED),
    (FRI_2014_07_18, MON_2014_07_21, HOLCAL_MON_WED),
    (SAT_2014_07_19, MON_2014_07_21, HOLCAL_MON_WED),
    (SUN_2014_07_20, MON_2014_07_21, HOLCAL_MON_WED),
    (MON_2014_07_21, TUE_2014_07_22, HOLCAL_MON_WED),
    (MON_2014_12_29, WED_2014_12_31, HOLCAL_YEAR_END),
    (TUE_2014_12_30, WED_2014_12_31, HOLCAL_YEAR_END),
    (WED_2014_12_31, FRI_2015_01_02, HOLCAL_YEAR_END),
    (THU_2015_01_01, FRI_2015_01_02, HOLCAL_YEAR_END),
    (FRI_2015_01_02, MON_2015_01_05, HOLCAL_YEAR_END),
    (SAT_2015_01_03, MON_2015_01_05, HOLCAL_YEAR_END),
    (TUE_2015_03_31, WED_2015_04_01, HOLCAL_YEAR_END),
    (SAT_2014_07_12, MON_2014_07_14, HOLCAL_SAT_SUN)
  )

  test("test_next") {
    forAll(data_next) { (date: LocalDate, expectedNext: LocalDate, calendar: ImmutableHolidayCalendar) =>
      withClue(s"next after $date of ${calendar.name}: ") {
        calendar.next(date) shouldBe expectedNext
      }
    }
  }

  test("test_next_range") {
    HOLCAL_MON_WED.next(date(2010, 1, 1)) shouldBe date(2010, 1, 4)
    rejectsUnsupportedDate(HOLCAL_MON_WED.next(LocalDate.MIN))
    rejectsUnsupportedDate(HOLCAL_MON_WED.next(LocalDate.MAX.minusDays(1L)))
  }

  //-------------------------------------------------------------------------
  private val data_nextOrSame: TableFor3[LocalDate, LocalDate, ImmutableHolidayCalendar] = Table(
    ("date", "expectedNext", "calendar"),
    (THU_2014_07_10, THU_2014_07_10, HOLCAL_MON_WED),
    (FRI_2014_07_11, FRI_2014_07_11, HOLCAL_MON_WED),
    (SAT_2014_07_12, TUE_2014_07_15, HOLCAL_MON_WED),
    (SUN_2014_07_13, TUE_2014_07_15, HOLCAL_MON_WED),
    (MON_2014_07_14, TUE_2014_07_15, HOLCAL_MON_WED),
    (TUE_2014_07_15, TUE_2014_07_15, HOLCAL_MON_WED),
    (WED_2014_07_16, THU_2014_07_17, HOLCAL_MON_WED),
    (THU_2014_07_17, THU_2014_07_17, HOLCAL_MON_WED),
    (FRI_2014_07_18, FRI_2014_07_18, HOLCAL_MON_WED),
    (SAT_2014_07_19, MON_2014_07_21, HOLCAL_MON_WED),
    (SUN_2014_07_20, MON_2014_07_21, HOLCAL_MON_WED),
    (MON_2014_07_21, MON_2014_07_21, HOLCAL_MON_WED),
    (MON_2014_12_29, MON_2014_12_29, HOLCAL_YEAR_END),
    (TUE_2014_12_30, WED_2014_12_31, HOLCAL_YEAR_END),
    (WED_2014_12_31, WED_2014_12_31, HOLCAL_YEAR_END),
    (THU_2015_01_01, FRI_2015_01_02, HOLCAL_YEAR_END),
    (FRI_2015_01_02, FRI_2015_01_02, HOLCAL_YEAR_END),
    (SAT_2015_01_03, MON_2015_01_05, HOLCAL_YEAR_END),
    (TUE_2015_03_31, TUE_2015_03_31, HOLCAL_YEAR_END),
    (WED_2015_04_01, WED_2015_04_01, HOLCAL_YEAR_END),
    (SAT_2014_07_12, MON_2014_07_14, HOLCAL_SAT_SUN)
  )

  test("test_nextOrSame") {
    forAll(data_nextOrSame) { (date: LocalDate, expectedNext: LocalDate, calendar: ImmutableHolidayCalendar) =>
      withClue(s"nextOrSame from $date of ${calendar.name}: ") {
        calendar.nextOrSame(date) shouldBe expectedNext
      }
    }
  }

  test("test_nextOrSame_range") {
    HOLCAL_MON_WED.nextOrSame(date(2010, 1, 1)) shouldBe date(2010, 1, 1)
    rejectsUnsupportedDate(HOLCAL_MON_WED.nextOrSame(LocalDate.MIN))
    rejectsUnsupportedDate(HOLCAL_MON_WED.nextOrSame(LocalDate.MAX))
  }

  //-------------------------------------------------------------------------
  private val data_previous: TableFor3[LocalDate, LocalDate, ImmutableHolidayCalendar] = Table(
    ("date", "expectedPrevious", "calendar"),
    (FRI_2014_07_11, THU_2014_07_10, HOLCAL_MON_WED),
    (SAT_2014_07_12, FRI_2014_07_11, HOLCAL_MON_WED),
    (SUN_2014_07_13, FRI_2014_07_11, HOLCAL_MON_WED),
    (MON_2014_07_14, FRI_2014_07_11, HOLCAL_MON_WED),
    (TUE_2014_07_15, FRI_2014_07_11, HOLCAL_MON_WED),
    (WED_2014_07_16, TUE_2014_07_15, HOLCAL_MON_WED),
    (THU_2014_07_17, TUE_2014_07_15, HOLCAL_MON_WED),
    (FRI_2014_07_18, THU_2014_07_17, HOLCAL_MON_WED),
    (SAT_2014_07_19, FRI_2014_07_18, HOLCAL_MON_WED),
    (SUN_2014_07_20, FRI_2014_07_18, HOLCAL_MON_WED),
    (MON_2014_07_21, FRI_2014_07_18, HOLCAL_MON_WED),
    (TUE_2014_07_22, MON_2014_07_21, HOLCAL_MON_WED),
    (TUE_2014_12_30, MON_2014_12_29, HOLCAL_YEAR_END),
    (WED_2014_12_31, MON_2014_12_29, HOLCAL_YEAR_END),
    (THU_2015_01_01, WED_2014_12_31, HOLCAL_YEAR_END),
    (FRI_2015_01_02, WED_2014_12_31, HOLCAL_YEAR_END),
    (SAT_2015_01_03, FRI_2015_01_02, HOLCAL_YEAR_END),
    (MON_2015_01_05, FRI_2015_01_02, HOLCAL_YEAR_END),
    (WED_2015_04_01, TUE_2015_03_31, HOLCAL_YEAR_END),
    (SAT_2014_07_12, FRI_2014_07_11, HOLCAL_SAT_SUN)
  )

  test("test_previous") {
    forAll(data_previous) { (date: LocalDate, expectedPrevious: LocalDate, calendar: ImmutableHolidayCalendar) =>
      withClue(s"previous before $date of ${calendar.name}: ") {
        calendar.previous(date) shouldBe expectedPrevious
      }
    }
  }

  test("test_previous_range") {
    HOLCAL_MON_WED.previous(date(2010, 1, 1)) shouldBe date(2009, 12, 31)
    rejectsUnsupportedDate(HOLCAL_MON_WED.previous(LocalDate.MIN.plusDays(1L)))
    rejectsUnsupportedDate(HOLCAL_MON_WED.previous(LocalDate.MAX))
  }

  //-------------------------------------------------------------------------
  private val data_previousOrSame: TableFor3[LocalDate, LocalDate, ImmutableHolidayCalendar] = Table(
    ("date", "expectedPrevious", "calendar"),
    (FRI_2014_07_11, FRI_2014_07_11, HOLCAL_MON_WED),
    (SAT_2014_07_12, FRI_2014_07_11, HOLCAL_MON_WED),
    (SUN_2014_07_13, FRI_2014_07_11, HOLCAL_MON_WED),
    (MON_2014_07_14, FRI_2014_07_11, HOLCAL_MON_WED),
    (TUE_2014_07_15, TUE_2014_07_15, HOLCAL_MON_WED),
    (WED_2014_07_16, TUE_2014_07_15, HOLCAL_MON_WED),
    (THU_2014_07_17, THU_2014_07_17, HOLCAL_MON_WED),
    (FRI_2014_07_18, FRI_2014_07_18, HOLCAL_MON_WED),
    (SAT_2014_07_19, FRI_2014_07_18, HOLCAL_MON_WED),
    (SUN_2014_07_20, FRI_2014_07_18, HOLCAL_MON_WED),
    (MON_2014_07_21, MON_2014_07_21, HOLCAL_MON_WED),
    (TUE_2014_07_22, TUE_2014_07_22, HOLCAL_MON_WED),
    (MON_2014_12_29, MON_2014_12_29, HOLCAL_YEAR_END),
    (TUE_2014_12_30, MON_2014_12_29, HOLCAL_YEAR_END),
    (WED_2014_12_31, WED_2014_12_31, HOLCAL_YEAR_END),
    (THU_2015_01_01, WED_2014_12_31, HOLCAL_YEAR_END),
    (FRI_2015_01_02, FRI_2015_01_02, HOLCAL_YEAR_END),
    (SAT_2015_01_03, FRI_2015_01_02, HOLCAL_YEAR_END),
    (MON_2015_01_05, MON_2015_01_05, HOLCAL_YEAR_END),
    (TUE_2015_03_31, TUE_2015_03_31, HOLCAL_YEAR_END),
    (WED_2015_04_01, WED_2015_04_01, HOLCAL_YEAR_END),
    (SAT_2014_07_12, FRI_2014_07_11, HOLCAL_SAT_SUN)
  )

  test("test_previousOrSame") {
    forAll(data_previousOrSame) { (date: LocalDate, expectedPrevious: LocalDate, calendar: ImmutableHolidayCalendar) =>
      withClue(s"previousOrSame from $date of ${calendar.name}: ") {
        calendar.previousOrSame(date) shouldBe expectedPrevious
      }
    }
  }

  test("test_previousOrSame_range") {
    HOLCAL_MON_WED.previousOrSame(date(2010, 1, 1)) shouldBe date(2010, 1, 1)
    rejectsUnsupportedDate(HOLCAL_MON_WED.previousOrSame(LocalDate.MIN))
    rejectsUnsupportedDate(HOLCAL_MON_WED.previousOrSame(LocalDate.MAX))
  }

  //-------------------------------------------------------------------------
  /**
   * The adjustment the modified-following convention makes, so the answer can be earlier than the
   * date given: where the next business day would fall in the following month, the last business
   * day of this one is returned.
   */
  private val data_nextSameOrLastInMonth: TableFor3[LocalDate, LocalDate, ImmutableHolidayCalendar] = Table(
    ("date", "expectedNext", "calendar"),
    (THU_2014_07_10, THU_2014_07_10, HOLCAL_MON_WED),
    (FRI_2014_07_11, FRI_2014_07_11, HOLCAL_MON_WED),
    (SAT_2014_07_12, TUE_2014_07_15, HOLCAL_MON_WED),
    (SUN_2014_07_13, TUE_2014_07_15, HOLCAL_MON_WED),
    (MON_2014_07_14, TUE_2014_07_15, HOLCAL_MON_WED),
    (TUE_2014_07_15, TUE_2014_07_15, HOLCAL_MON_WED),
    (WED_2014_07_16, THU_2014_07_17, HOLCAL_MON_WED),
    (THU_2014_07_17, THU_2014_07_17, HOLCAL_MON_WED),
    (FRI_2014_07_18, FRI_2014_07_18, HOLCAL_MON_WED),
    (SAT_2014_07_19, MON_2014_07_21, HOLCAL_MON_WED),
    (SUN_2014_07_20, MON_2014_07_21, HOLCAL_MON_WED),
    (MON_2014_07_21, MON_2014_07_21, HOLCAL_MON_WED),
    (MON_2014_12_29, MON_2014_12_29, HOLCAL_YEAR_END),
    (TUE_2014_12_30, WED_2014_12_31, HOLCAL_YEAR_END),
    (WED_2014_12_31, WED_2014_12_31, HOLCAL_YEAR_END),
    (THU_2015_01_01, FRI_2015_01_02, HOLCAL_YEAR_END),
    (FRI_2015_01_02, FRI_2015_01_02, HOLCAL_YEAR_END),
    (SAT_2015_01_03, MON_2015_01_05, HOLCAL_YEAR_END),
    (TUE_2015_03_31, TUE_2015_03_31, HOLCAL_YEAR_END),
    (WED_2015_04_01, WED_2015_04_01, HOLCAL_YEAR_END),
    (SAT_2014_07_12, MON_2014_07_14, HOLCAL_SAT_SUN),
    (SAT_2015_02_28, FRI_2015_02_27, HOLCAL_SAT_SUN),
    (WED_2014_07_30, WED_2014_07_30, HOLCAL_END_MONTH),
    (THU_2014_07_31, WED_2014_07_30, HOLCAL_END_MONTH)
  )

  test("test_nextLastOrSame") {
    forAll(data_nextSameOrLastInMonth) {
      (date: LocalDate, expectedNext: LocalDate, calendar: ImmutableHolidayCalendar) =>
        withClue(s"nextSameOrLastInMonth from $date of ${calendar.name}: ") {
          calendar.nextSameOrLastInMonth(date) shouldBe expectedNext
        }
    }
  }

  test("test_nextSameOrLastInMonth_range") {
    HOLCAL_MON_WED.nextSameOrLastInMonth(date(2010, 1, 1)) shouldBe date(2010, 1, 1)
    rejectsUnsupportedDate(HOLCAL_MON_WED.nextSameOrLastInMonth(LocalDate.MIN))
    rejectsUnsupportedDate(HOLCAL_MON_WED.nextSameOrLastInMonth(LocalDate.MAX))
  }

  //-------------------------------------------------------------------------
  private val data_lastBusinessDayOfMonth: TableFor2[LocalDate, LocalDate] = Table(
    ("date", "expectedEom"),
    // June 30th is Monday holiday, June 28/29 is weekend
    (date(2014, 6, 26), date(2014, 6, 27)),
    (date(2014, 6, 27), date(2014, 6, 27)),
    (date(2014, 6, 28), date(2014, 6, 27)),
    (date(2014, 6, 29), date(2014, 6, 27)),
    (date(2014, 6, 30), date(2014, 6, 27)),
    // July 31st is Thursday holiday
    (date(2014, 7, 29), date(2014, 7, 30)),
    (date(2014, 7, 30), date(2014, 7, 30)),
    (date(2014, 7, 31), date(2014, 7, 30)),
    // August 31st is Sunday weekend
    (date(2014, 8, 28), date(2014, 8, 29)),
    (date(2014, 8, 29), date(2014, 8, 29)),
    (date(2014, 8, 30), date(2014, 8, 29)),
    (date(2014, 8, 31), date(2014, 8, 29)),
    // September 30th is Tuesday not holiday
    (date(2014, 9, 28), date(2014, 9, 30)),
    (date(2014, 9, 29), date(2014, 9, 30)),
    (date(2014, 9, 30), date(2014, 9, 30))
  )

  test("test_lastBusinessDayOfMonth") {
    forAll(data_lastBusinessDayOfMonth) { (date: LocalDate, expectedEom: LocalDate) =>
      withClue(s"last business day of the month of $date: ") {
        HOLCAL_END_MONTH.lastBusinessDayOfMonth(date) shouldBe expectedEom
      }
    }
  }

  test("test_isLastBusinessDayOfMonth") {
    forAll(data_lastBusinessDayOfMonth) { (date: LocalDate, expectedEom: LocalDate) =>
      withClue(s"$date: ") {
        HOLCAL_END_MONTH.isLastBusinessDayOfMonth(date) shouldBe (date == expectedEom)
      }
    }
  }

  test("test_lastBusinessDayOfMonth_satSun") {
    HOLCAL_SAT_SUN.isLastBusinessDayOfMonth(MON_2014_06_30) shouldBe true
    HOLCAL_SAT_SUN.lastBusinessDayOfMonth(MON_2014_06_30) shouldBe MON_2014_06_30
  }

  test("test_lastBusinessDayOfMonth_range") {
    HOLCAL_END_MONTH.lastBusinessDayOfMonth(date(2010, 1, 1)) shouldBe date(2010, 1, 29)
    rejectsUnsupportedDate(HOLCAL_END_MONTH.lastBusinessDayOfMonth(LocalDate.MIN))
    rejectsUnsupportedDate(HOLCAL_END_MONTH.lastBusinessDayOfMonth(LocalDate.MAX))
  }

  test("test_isLastBusinessDayOfMonth_range") {
    HOLCAL_END_MONTH.isLastBusinessDayOfMonth(date(2010, 1, 1)) shouldBe false
    rejectsUnsupportedDate(HOLCAL_END_MONTH.isLastBusinessDayOfMonth(LocalDate.MIN))
    rejectsUnsupportedDate(HOLCAL_END_MONTH.isLastBusinessDayOfMonth(LocalDate.MAX))
  }

  //-------------------------------------------------------------------------
  /**
   * Counted from the same Friday: the start date is included and the end date is not, which is why
   * a range whose ends are equal counts zero.
   */
  private val data_daysBetween: TableFor3[LocalDate, LocalDate, Int] = Table(
    ("start", "end", "expected"),
    (FRI_2014_07_11, FRI_2014_07_11, 0),
    (FRI_2014_07_11, SAT_2014_07_12, 1),
    (FRI_2014_07_11, SUN_2014_07_13, 1),
    (FRI_2014_07_11, MON_2014_07_14, 1),
    (FRI_2014_07_11, TUE_2014_07_15, 1),
    (FRI_2014_07_11, WED_2014_07_16, 2),
    (FRI_2014_07_11, THU_2014_07_17, 2),
    (FRI_2014_07_11, FRI_2014_07_18, 3),
    (FRI_2014_07_11, SAT_2014_07_19, 4),
    (FRI_2014_07_11, SUN_2014_07_20, 4),
    (FRI_2014_07_11, MON_2014_07_21, 4),
    (FRI_2014_07_11, TUE_2014_07_22, 5)
  )

  test("test_daysBetween_LocalDateLocalDate") {
    forAll(data_daysBetween) { (start: LocalDate, end: LocalDate, expected: Int) =>
      withClue(s"business days from $start to $end: ") {
        HOLCAL_MON_WED.daysBetween(start, end) shouldBe expected
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_combinedWith") {
    // `combinedWith` reads both calendars on every query rather than merging their data
    val base1 = ImmutableHolidayCalendar.of(TEST_ID, List(WED_2014_07_16), SATURDAY, SUNDAY)
    val base2 = ImmutableHolidayCalendar.of(TEST_ID2, List(MON_2014_07_14), FRIDAY, SATURDAY)
    val test = base1.combinedWith(base2)
    test.name shouldBe "Test1+Test2"

    test.isHoliday(THU_2014_07_10) shouldBe false
    test.isHoliday(FRI_2014_07_11) shouldBe true
    test.isHoliday(SAT_2014_07_12) shouldBe true
    test.isHoliday(SUN_2014_07_13) shouldBe true
    test.isHoliday(MON_2014_07_14) shouldBe true
    test.isHoliday(TUE_2014_07_15) shouldBe false
    test.isHoliday(WED_2014_07_16) shouldBe true
    test.isHoliday(THU_2014_07_17) shouldBe false
    test.isHoliday(FRI_2014_07_18) shouldBe true
    test.isHoliday(SAT_2014_07_19) shouldBe true
    test.isHoliday(SUN_2014_07_20) shouldBe true
    test.isHoliday(MON_2014_07_21) shouldBe false
  }

  //-------------------------------------------------------------------------
  test("test_combineWith_same") {
    val base = ImmutableHolidayCalendar.of(TEST_ID, List(WED_2014_07_16), SATURDAY, SUNDAY)
    // combining a calendar with itself observes nothing new, and answers with that calendar
    val test = base.combinedWith(base)
    test should be theSameInstanceAs base
  }

  test("test_combineWith_none") {
    val base = ImmutableHolidayCalendar.of(TEST_ID, List(WED_2014_07_16), SATURDAY, SUNDAY)
    // the calendar with no holidays closes no day, so it is the identity of this combination
    val test = base.combinedWith(HolidayCalendars.NO_HOLIDAYS)
    test should be theSameInstanceAs base
  }

  test("test_combineWith_satSun") {
    val base = ImmutableHolidayCalendar.of(TEST_ID, List(WED_2014_07_16), SATURDAY, SUNDAY)
    // combining with a weekend calendar adds that weekend: Friday now closes as well, and the
    // combined name is the two names sorted, as combining identifiers always normalises them
    val test = base.combinedWith(HolidayCalendars.FRI_SAT)
    test.name shouldBe "Fri/Sat+Test1"

    test.isHoliday(THU_2014_07_10) shouldBe false
    test.isHoliday(FRI_2014_07_11) shouldBe true
    test.isHoliday(SAT_2014_07_12) shouldBe true
    test.isHoliday(SUN_2014_07_13) shouldBe true
    test.isHoliday(MON_2014_07_14) shouldBe false
    test.isHoliday(TUE_2014_07_15) shouldBe false
    test.isHoliday(WED_2014_07_16) shouldBe true
    test.isHoliday(THU_2014_07_17) shouldBe false
    test.isHoliday(FRI_2014_07_18) shouldBe true
    test.isHoliday(SAT_2014_07_19) shouldBe true
    test.isHoliday(SUN_2014_07_20) shouldBe true
    test.isHoliday(MON_2014_07_21) shouldBe false
  }

  //-------------------------------------------------------------------------
  test("test_broadCheck") {
    // Calendars of pseudo-random holidays over a decade, checked day by day against the rule that
    // a day is a holiday where it falls at the weekend or belongs to the dates the calendar was
    // built from. This exercises the monthly bit masks over thousands of dates, deterministically:
    // the generator is seeded with a fixed value.
    val start = LocalDate.of(2010, 1, 1)
    val end = LocalDate.of(2020, 1, 1)
    val random = new Random(547698L)
    (0 until 10).foreach { index =>
      val holidaySet = Iterator
        .iterate(start)(current => current.plusDays((random.nextInt(10) + 1).toLong))
        .takeWhile(current => current.isBefore(end))
        .toSet
      val test =
        ImmutableHolidayCalendar.of(HolidayCalendarId.of(s"TestBroad$index"), holidaySet, SATURDAY, SUNDAY)
      datesFrom(start, end).foreach { checkDate =>
        val dayOfWeek = checkDate.getDayOfWeek
        val expectedHoliday =
          dayOfWeek == SATURDAY || dayOfWeek == SUNDAY || holidaySet.contains(checkDate)
        withClue(s"${test.name} on $checkDate: ") {
          test.isHoliday(checkDate) shouldBe expectedHoliday
        }
      }
    }
    succeed
  }

  //-------------------------------------------------------------------------
  test("test_equals") {
    val a1 = ImmutableHolidayCalendar.of(TEST_ID, List(WED_2014_07_16), SATURDAY, SUNDAY)
    val a2 = ImmutableHolidayCalendar.of(TEST_ID, List(WED_2014_07_16), SATURDAY, SUNDAY)
    val b = ImmutableHolidayCalendar.of(TEST_ID2, List(WED_2014_07_16), SATURDAY, SUNDAY)
    val c = ImmutableHolidayCalendar.of(TEST_ID, List(THU_2014_07_10), SATURDAY, SUNDAY)
    (a1 == a2) shouldBe true
    (a1 == b) shouldBe false
    (a1 == c) shouldBe true // only the id is compared
    // the last assertion is why no content is asserted through equality here: only the id counts
    a1.holidays.toList should not be c.holidays.toList
    a1.hashCode shouldBe c.hashCode
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // the family's one equality-bearing instance agrees with `equals`, hashes alike, shows the
    // calendar's `toString`
    val sameIdOtherHolidays = ImmutableHolidayCalendar.of(TEST_ID, List(THU_2014_07_10), SATURDAY, SUNDAY)
    val otherId = ImmutableHolidayCalendar.of(TEST_ID2, List(MON_2014_07_14, WED_2014_07_16), SATURDAY, SUNDAY)
    val calendarHash = Hash[HolidayCalendar]

    calendarHash.eqv(HOLCAL_MON_WED, sameIdOtherHolidays) shouldBe (HOLCAL_MON_WED == sameIdOtherHolidays)
    calendarHash.eqv(HOLCAL_MON_WED, sameIdOtherHolidays) shouldBe true
    calendarHash.hash(HOLCAL_MON_WED) shouldBe calendarHash.hash(sameIdOtherHolidays)
    calendarHash.hash(HOLCAL_MON_WED) shouldBe HOLCAL_MON_WED.hashCode
    calendarHash.eqv(HOLCAL_MON_WED, otherId) shouldBe false

    // `Eq` is reached by subtyping from that instance rather than declared a second time
    Eq[HolidayCalendar].eqv(HOLCAL_MON_WED, HOLCAL_MON_WED) shouldBe true
    Eq[HolidayCalendar].eqv(HOLCAL_MON_WED, otherId) shouldBe false

    Show[HolidayCalendar].show(HOLCAL_MON_WED) shouldBe "HolidayCalendar[Test1]"
    Show[HolidayCalendar].show(HOLCAL_MON_WED) shouldBe HOLCAL_MON_WED.toString
    HOLCAL_MON_WED.name shouldBe TEST_ID.name
    HOLCAL_MON_WED.id shouldBe TEST_ID

    // typed as `Any` so the comparison is the `equals` contract, not one of unrelated types
    val unrelated: Any = TEST_ID.name
    HOLCAL_MON_WED.equals(unrelated) shouldBe false
  }

  test("test_serialization") {
    // A calendar that is not one of the built-in set is written as an object under the
    // `Immutable` key carrying the dates it declares, never the array of bit masks it holds.
    val json = HOLCAL_MON_WED.asJson
    val expected = parse(
      """{"Immutable":{"id":"Test1","weekendDays":["SATURDAY","SUNDAY"],"startYear":2014,""" +
        """"holidays":["2014-07-14","2014-07-16"],"workingWeekendDays":[]}}"""
    ).getOrElse(fail("the expected JSON of this test is not valid JSON"))
    json shouldBe expected

    val roundTripped =
      decode[ImmutableHolidayCalendar](json.noSpaces)
        .getOrElse(fail(s"the calendar did not survive the round trip: ${json.noSpaces}"))

    // Equality compares identifiers alone, so on its own it would accept a calendar whose
    // holidays had been lost; the holiday set and the day-by-day sweep show the dates survived.
    roundTripped shouldBe HOLCAL_MON_WED
    Eq[HolidayCalendar].eqv(roundTripped, HOLCAL_MON_WED) shouldBe true
    roundTripped.holidays.toList shouldBe HOLCAL_MON_WED.holidays.toList
    roundTripped.workingDays.toList shouldBe HOLCAL_MON_WED.workingDays.toList
    roundTripped.weekendDays shouldBe HOLCAL_MON_WED.weekendDays
    roundTripped.startYear shouldBe HOLCAL_MON_WED.startYear
    roundTripped.endYearExclusive shouldBe HOLCAL_MON_WED.endYearExclusive
    datesFrom(
      LocalDate.of(HOLCAL_MON_WED.startYear, 1, 1),
      LocalDate.of(HOLCAL_MON_WED.endYearExclusive, 1, 1)).foreach { current =>
      withClue(s"$current: ") {
        roundTripped.isHoliday(current) shouldBe HOLCAL_MON_WED.isHoliday(current)
      }
    }

    // ... and this is why: a document that lost its holidays decodes to a calendar equal to this
    // one, so only the content assertions above tell them apart
    val withoutHolidays = decode[ImmutableHolidayCalendar](
      """{"Immutable":{"id":"Test1","weekendDays":["SATURDAY","SUNDAY"],""" +
        """"holidays":[],"workingWeekendDays":[]}}"""
    ).getOrElse(fail("the calendar without holidays could not be decoded"))
    withoutHolidays shouldBe HOLCAL_MON_WED
    withoutHolidays.holidays.toList should not be HOLCAL_MON_WED.holidays.toList
    withoutHolidays.isHoliday(MON_2014_07_14) shouldBe false
  }

  test("test_serialization_overriddenBuiltInId") {
    // A calendar an application supplies under a standard identifier keeps its own dates across a
    // round trip. The calendar below is `==` to the library's London calendar, so an encoder
    // choosing the string form by equality with the built-in set would write the bare name `GBLO`
    // and read back the library's calendar, replacing the dates the application declared.
    val ownLondon = ImmutableHolidayCalendar.of(
      HolidayCalendarIds.GBLO,
      List(MON_2014_07_14, WED_2014_07_16),
      List(SATURDAY, SUNDAY),
      List(SAT_2014_07_12))
    val library = StandardHolidayCalendars.GBLO

    (ownLondon == library) shouldBe true
    ownLondon.holidays.toList should not be library.holidays.toList

    library.asJson shouldBe parse("\"GBLO\"").getOrElse(fail("the expected JSON is not valid JSON"))
    decode[HolidayCalendar]("\"GBLO\"") shouldBe Right(library)
    decode[HolidayCalendar]("\"GBLO\"").getOrElse(fail("GBLO did not decode")) should
      be theSameInstanceAs library

    val json = ownLondon.asJson
    json.isString shouldBe false
    json shouldBe parse(
      """{"Immutable":{"id":"GBLO","weekendDays":["SATURDAY","SUNDAY"],"startYear":2014,""" +
        """"holidays":["2014-07-14","2014-07-16"],"workingWeekendDays":["2014-07-12"]}}"""
    ).getOrElse(fail("the expected JSON of this test is not valid JSON"))

    val roundTripped = decode[ImmutableHolidayCalendar](json.noSpaces)
      .getOrElse(fail(s"the calendar did not survive the round trip: ${json.noSpaces}"))
    roundTripped.id shouldBe HolidayCalendarIds.GBLO
    roundTripped.holidays.toList shouldBe List(MON_2014_07_14, WED_2014_07_16)
    roundTripped.workingDays.toList shouldBe List(SAT_2014_07_12)
    roundTripped.isHoliday(MON_2014_07_14) shouldBe true
    roundTripped.isBusinessDay(SAT_2014_07_12) shouldBe true
    // the dates it kept are its own: 26 December 2014 is a holiday in London, a business day here
    val boxingDay2014 = date(2014, 12, 26)
    library.isHoliday(boxingDay2014) shouldBe true
    roundTripped.isHoliday(boxingDay2014) shouldBe false
  }

  test("test_of_unsupportedYears") {
    // A calendar holds one machine word per month from its earliest holiday to its latest, so the
    // years its holidays span decide what building it allocates. The years it can be asked about -
    // 0 to 9999 - are therefore the years it can be built from: a caller contract, refused fast,
    // that caps the array at 120,000 months whoever supplied the dates.
    val weekend = List(SATURDAY, SUNDAY)
    val rejectsUnsupportedHoliday = (holidays: List[LocalDate]) =>
      intercept[IllegalArgumentException](
        ImmutableHolidayCalendar.of(TEST_ID, holidays, weekend)).getMessage should include(
        UnsupportedDateMessage)

    rejectsUnsupportedHoliday(List(LocalDate.MAX))
    rejectsUnsupportedHoliday(List(LocalDate.MIN))
    rejectsUnsupportedHoliday(List(date(10000, 1, 1)))
    rejectsUnsupportedHoliday(List(date(-1, 12, 31)))
    // one end in range and the other not, in both directions: both ends size the array
    rejectsUnsupportedHoliday(List(MON_2014_07_14, LocalDate.MAX))
    rejectsUnsupportedHoliday(List(LocalDate.MIN, MON_2014_07_14))

    val earliest = ImmutableHolidayCalendar.of(TEST_ID, List(date(0, 1, 3)), weekend)
    earliest.startYear shouldBe 0
    earliest.isHoliday(date(0, 1, 3)) shouldBe true
    val latest = ImmutableHolidayCalendar.of(TEST_ID, List(date(9999, 12, 31)), weekend)
    latest.endYearExclusive shouldBe 10000
    latest.isHoliday(date(9999, 12, 31)) shouldBe true

    // A working day outside the years the holidays span is ignored rather than rejected: it names
    // a date in a range the calendar holds no data for, so there is nothing there to override.
    val ignoredWorkingDay =
      ImmutableHolidayCalendar.of(TEST_ID, List(MON_2014_07_14), weekend, List(date(9999, 12, 25)))
    ignoredWorkingDay.workingDays shouldBe empty
    ignoredWorkingDay.endYearExclusive shouldBe 2015
  }

  test("test_unsupportedYears_optimizedPaths") {
    // The month index of a date is measured in `Long` arithmetic. With 2014 as the first year
    // stored, the index of January 357915956 is (357915956 - 2014) * 12 = 4,294,967,304, eight
    // more than two to the thirty-two, so in `Int` arithmetic it wraps to 8 - an index the stored
    // months of 2014 contain, and the date would have been answered from September 2014.
    val overflowed = date(357915956, 1, 1)
    HOLCAL_MON_WED.startYear shouldBe 2014
    HOLCAL_MON_WED.endYearExclusive shouldBe 2015
    ((overflowed.getYear.toLong - 2014L) * 12L).toInt shouldBe 8

    rejectsUnsupportedDate(HOLCAL_MON_WED.isHoliday(overflowed))
    rejectsUnsupportedDate(HOLCAL_MON_WED.isBusinessDay(overflowed))
    rejectsUnsupportedDate(HOLCAL_MON_WED.shift(overflowed, 1))
    rejectsUnsupportedDate(HOLCAL_MON_WED.shift(overflowed, -1))
    rejectsUnsupportedDate(HOLCAL_MON_WED.next(overflowed))
    rejectsUnsupportedDate(HOLCAL_MON_WED.previous(overflowed))
    rejectsUnsupportedDate(HOLCAL_MON_WED.nextOrSame(overflowed))
    rejectsUnsupportedDate(HOLCAL_MON_WED.previousOrSame(overflowed))
    rejectsUnsupportedDate(HOLCAL_MON_WED.nextSameOrLastInMonth(overflowed))
    rejectsUnsupportedDate(HOLCAL_MON_WED.lastBusinessDayOfMonth(overflowed))
    rejectsUnsupportedDate(HOLCAL_MON_WED.isLastBusinessDayOfMonth(overflowed))
    rejectsUnsupportedDate(HOLCAL_MON_WED.daysBetween(MON_2014_07_14, overflowed))
    rejectsUnsupportedDate(HOLCAL_MON_WED.daysBetween(LocalDate.MIN, MON_2014_07_14))

    // a date inside the stored months is unaffected, so the check has not swallowed what it guards
    HOLCAL_MON_WED.isHoliday(MON_2014_07_14) shouldBe true
    HOLCAL_MON_WED.next(MON_2014_07_14) shouldBe TUE_2014_07_15
    HOLCAL_MON_WED.previous(MON_2014_07_14) shouldBe FRI_2014_07_11
  }

  test("test_serialization_hostileDocument") {
    // A document is data, so a document describing a calendar no calendar could be is a decoding
    // failure rather than a throw, and it fails before anything is built - building allocates.
    val weekend = """"weekendDays":["SATURDAY","SUNDAY"]"""
    val rejects = (document: String) =>
      decode[ImmutableHolidayCalendar](document) match {
        case Left(_) => succeed
        case Right(calendar) =>
          fail(s"the document should have been rejected but decoded to: ${calendar.id.name}")
      }

    // holiday dates a calendar cannot hold, which are also the dates that would size its storage
    rejects(s"""{"Immutable":{"id":"Test1",$weekend,"holidays":["+999999999-12-31"]}}""")
    rejects(s"""{"Immutable":{"id":"Test1",$weekend,"holidays":["-999999999-01-01"]}}""")
    rejects(s"""{"Immutable":{"id":"Test1",$weekend,"holidays":["2014-07-14","+999999999-12-31"]}}""")
    rejects(s"""{"Immutable":{"id":"Test1",$weekend,"holidays":["0000-01-01","10000-01-01"]}}""")

    // a working day a calendar cannot hold: ignored once built, but refused in a document
    rejects(
      s"""{"Immutable":{"id":"Test1",$weekend,"holidays":["2014-07-14"],""" +
        """"workingWeekendDays":["+999999999-12-31"]}}""")

    // the first year the document declares must be a year a calendar can cover, and cannot be
    // later than the earliest date the document names: a range cannot begin after the dates in it
    rejects(s"""{"Immutable":{"id":"Test1",$weekend,"startYear":1000000,"holidays":["2014-07-14"]}}""")
    rejects(s"""{"Immutable":{"id":"Test1",$weekend,"startYear":-1,"holidays":["2014-07-14"]}}""")
    rejects(s"""{"Immutable":{"id":"Test1",$weekend,"startYear":2015,"holidays":["2014-07-14"]}}""")
    rejects(s"""{"Immutable":{"id":"Test1",$weekend,"startYear":"soon","holidays":["2014-07-14"]}}""")
    rejects(
      s"""{"Immutable":{"id":"Test1",$weekend,"startYear":2015,"holidays":["2015-07-14"],""" +
        """"workingWeekendDays":["2014-07-12"]}}""")

    // and what is accepted: the year of the earliest holiday, an earlier year - informational
    // only, the range following from the holidays - no year at all, and a year with no date to
    // compare against
    decode[ImmutableHolidayCalendar](
      s"""{"Immutable":{"id":"Test1",$weekend,"startYear":2014,"holidays":["2014-07-14"]}}""")
      .map(calendar => calendar.holidays.toList) shouldBe Right(List(MON_2014_07_14))
    decode[ImmutableHolidayCalendar](
      s"""{"Immutable":{"id":"Test1",$weekend,"startYear":2000,"holidays":["2014-07-14"]}}""")
      .map(calendar => (calendar.startYear, calendar.endYearExclusive)) shouldBe Right((2014, 2015))
    decode[ImmutableHolidayCalendar](
      s"""{"Immutable":{"id":"Test1",$weekend,"holidays":["2014-07-14"]}}""")
      .map(calendar => calendar.startYear) shouldBe Right(2014)
    decode[ImmutableHolidayCalendar](
      s"""{"Immutable":{"id":"Test1",$weekend,"startYear":2014,"holidays":[]}}""")
      .map(calendar => calendar.holidays.toList) shouldBe Right(Nil)
  }

  test("test_serialization_rangeBeganBeforeEveryHoliday") {
    // A calendar whose range begins before every holiday it reports: its range begins at a holiday
    // that fell on a Saturday, which the stored months cannot tell from the weekend.
    val weekend = List(SATURDAY, SUNDAY)
    val sat2013 = date(2013, 7, 13)
    val test = ImmutableHolidayCalendar.of(TEST_ID, List(sat2013, MON_2014_07_14), weekend, List(sat2013))
    test.startYear shouldBe 2013
    test.endYearExclusive shouldBe 2015
    test.holidays.toList shouldBe List(MON_2014_07_14)
    test.workingDays.toList shouldBe List(sat2013)
    test.isBusinessDay(sat2013) shouldBe true

    // Its document records where its range began, earlier than every holiday it lists.
    val json = test.asJson
    json shouldBe parse(
      """{"Immutable":{"id":"Test1","weekendDays":["SATURDAY","SUNDAY"],"startYear":2013,""" +
        """"holidays":["2014-07-14"],"workingWeekendDays":["2013-07-13"]}}"""
    ).getOrElse(fail("the expected JSON of this test is not valid JSON"))

    // Read back, the range follows from the holidays the document lists - 2014 alone - the
    // document being decoded through the same normalising factory a caller builds with. The 2013
    // override therefore names a year the rebuilt calendar holds no data for and is ignored, just
    // as construction ignores a working day outside the range its holidays span.
    val roundTripped = decode[ImmutableHolidayCalendar](json.noSpaces)
      .getOrElse(fail(s"the calendar did not survive the round trip: ${json.noSpaces}"))
    roundTripped.startYear shouldBe 2014
    roundTripped.endYearExclusive shouldBe 2015
    roundTripped.holidays.toList shouldBe List(MON_2014_07_14)
    roundTripped.workingDays shouldBe empty
    roundTripped.isBusinessDay(sat2013) shouldBe false
    roundTripped.isHoliday(sat2013) shouldBe true

    // the two agree over the years the document lists holidays for, and differ in 2013 at one
    // date: the Saturday the original overrode
    datesFrom(date(2014, 1, 1), date(2015, 1, 1)).foreach { current =>
      withClue(s"$current: ")(roundTripped.isHoliday(current) shouldBe test.isHoliday(current))
    }
    datesFrom(date(2013, 1, 1), date(2014, 1, 1)).foreach { current =>
      withClue(s"$current: ") {
        roundTripped.isHoliday(current) shouldBe (current == sat2013 || test.isHoliday(current))
      }
    }

    // A second shape of the same case: the only date the calendar was built from fell at its
    // weekend, so the document lists no holiday and the rebuilt calendar stores no months.
    val weekendHolidayOnly = ImmutableHolidayCalendar.of(TEST_ID, List(SAT_2014_07_12), weekend)
    weekendHolidayOnly.startYear shouldBe 2014
    weekendHolidayOnly.holidays shouldBe empty
    val rebuilt = decode[ImmutableHolidayCalendar](weekendHolidayOnly.asJson.noSpaces)
      .getOrElse(fail("the weekend-only calendar did not survive the round trip"))
    rebuilt.startYear shouldBe 0
    rebuilt.endYearExclusive shouldBe 0
    rebuilt.holidays.toList shouldBe Nil
    rebuilt.workingDays.toList shouldBe Nil
    datesFrom(date(2014, 1, 1), date(2015, 1, 1)).foreach { current =>
      withClue(s"$current: ")(rebuilt.isHoliday(current) shouldBe weekendHolidayOnly.isHoliday(current))
    }

    // the declared year sizes nothing, and is still checked before anything is built
    decode[ImmutableHolidayCalendar](
      s"""{"Immutable":{"id":"Test1","weekendDays":["SATURDAY","SUNDAY"],"startYear":0,""" +
        s""""holidays":["9999-12-31"]}}""")
      .map(calendar => (calendar.startYear, calendar.endYearExclusive)) shouldBe Right((9999, 10000))
    decode[ImmutableHolidayCalendar](
      s"""{"Immutable":{"id":"Test1","weekendDays":["SATURDAY","SUNDAY"],"startYear":-1,""" +
        s""""holidays":["2014-07-14"]}}""")
      .isLeft shouldBe true
  }

  test("test_serialization_workingDayOutsideHolidayRange") {
    // A document whose only holiday is in 2020 and which declares a Saturday of 2021 to be a
    // business day: the range covers 2020 alone, so the override is ignored and that Saturday is
    // still a weekend holiday. Letting it widen the range would build a calendar no factory can.
    val weekendField = """"weekendDays":["SATURDAY","SUNDAY"]"""
    val weekend = List(SATURDAY, SUNDAY)
    val fri2020Christmas = date(2020, 12, 25)
    val sat2021 = date(2021, 1, 2)
    sat2021.getDayOfWeek shouldBe SATURDAY

    val outside = decode[ImmutableHolidayCalendar](
      s"""{"Immutable":{"id":"Test1",$weekendField,"startYear":2020,"holidays":["2020-12-25"],""" +
        """"workingWeekendDays":["2021-01-02"]}}"""
    ).getOrElse(fail("the document naming a working day outside the holiday range was rejected"))
    outside.startYear shouldBe 2020
    outside.endYearExclusive shouldBe 2021
    outside.holidays.toList shouldBe List(fri2020Christmas)
    outside.workingDays shouldBe empty
    outside.isHoliday(sat2021) shouldBe true
    outside.isBusinessDay(sat2021) shouldBe false

    // ... which is what the factory does with the same dates, construction setting the semantics
    val built = ImmutableHolidayCalendar.of(TEST_ID, List(fri2020Christmas), weekend, List(sat2021))
    built.startYear shouldBe outside.startYear
    built.endYearExclusive shouldBe outside.endYearExclusive
    built.holidays.toList shouldBe outside.holidays.toList
    built.workingDays shouldBe outside.workingDays
    built.isHoliday(sat2021) shouldBe outside.isHoliday(sat2021)

    // the positive control: a working day '''inside''' the range still takes effect
    val sat2020 = date(2020, 3, 7)
    sat2020.getDayOfWeek shouldBe SATURDAY
    val inside = decode[ImmutableHolidayCalendar](
      s"""{"Immutable":{"id":"Test1",$weekendField,"startYear":2020,"holidays":["2020-12-25"],""" +
        """"workingWeekendDays":["2020-03-07"]}}"""
    ).getOrElse(fail("the document naming a working day inside the holiday range was rejected"))
    inside.startYear shouldBe 2020
    inside.endYearExclusive shouldBe 2021
    inside.holidays.toList shouldBe List(fri2020Christmas)
    inside.workingDays.toList shouldBe List(sat2020)
    inside.isBusinessDay(sat2020) shouldBe true
  }

  //-------------------------------------------------------------------------
  test("test_holidays_workingDays_recoveredFromStoredMonths") {
    // The two bulk accessors and the pair they are read from recover their dates by scanning the
    // stored months, so they have to agree with what the calendar says about every day it covers:
    // a holiday is a date it calls a holiday and not a weekend day, a working day is a date it
    // calls a business day and does call a weekend day. Asserted day by day rather than against a
    // list, so the scan cannot agree with a list while disagreeing with the calendar.
    val newYears = (2012 to 2015).map(year => date(year, 1, 1)).toList
    val christmases = (2012 to 2015).map(year => date(year, 12, 25)).toList
    val workingSaturdays = List(date(2013, 3, 2), date(2014, 11, 29))
    val test = ImmutableHolidayCalendar.of(
      HolidayCalendarId.of("TestRecovery"),
      newYears ++ christmases ++ workingSaturdays,
      List(SATURDAY, SUNDAY),
      workingSaturdays)
    val covered = datesFrom(date(test.startYear, 1, 1), date(test.endYearExclusive, 1, 1)).toList
    val weekend = test.weekendDays

    val expectedHolidays = covered.filter(day => test.isHoliday(day) && !weekend.contains(day.getDayOfWeek))
    val expectedWorkingDays = covered.filter(day => test.isBusinessDay(day) && weekend.contains(day.getDayOfWeek))
    test.holidays.toList shouldBe expectedHolidays
    test.workingDays.toList shouldBe expectedWorkingDays

    // New Year 2012 fell on a Sunday, so it is absent: a date the weekend already closes leaves no
    // mark on the stored months. The two Saturdays supplied as holidays and as working days are
    // working days, the overrides being applied last.
    test.holidays.toList shouldBe List(
      date(2012, 12, 25),
      date(2013, 1, 1),
      date(2013, 12, 25),
      date(2014, 1, 1),
      date(2014, 12, 25),
      date(2015, 1, 1),
      date(2015, 12, 25))
    test.holidays.toList shouldBe (newYears ++ christmases).sortBy(day => day.toEpochDay).filterNot(day =>
      weekend.contains(day.getDayOfWeek))
    expectedWorkingDays shouldBe workingSaturdays
    test.startYear shouldBe 2012
    test.endYearExclusive shouldBe 2016

    val (holidays, workingDays) = test.holidaysAndWorkingDays
    holidays shouldBe test.holidays
    workingDays shouldBe test.workingDays

    // Scanning is repeatable and consumes nothing: the second reading equals the first.
    test.holidays shouldBe test.holidays
    test.workingDays shouldBe test.workingDays
    test.holidaysAndWorkingDays._1 shouldBe holidays

    // The scan tests each day against the weekend of the calendar rather than a fixed pair of
    // days: the Saturday holiday is reported, the Thursday holiday closed by this weekend is not.
    val thuFri = ImmutableHolidayCalendar.of(
      HolidayCalendarId.of("TestRecoveryThuFri"),
      List(THU_2014_07_17, FRI_2014_07_18, SAT_2014_07_19),
      List(THURSDAY, FRIDAY),
      List(FRI_2014_07_18))
    thuFri.holidays.toList shouldBe List(SAT_2014_07_19)
    thuFri.workingDays.toList shouldBe List(FRI_2014_07_18)
    thuFri.isHoliday(THU_2014_07_17) shouldBe true
    thuFri.isBusinessDay(FRI_2014_07_18) shouldBe true
    datesFrom(date(thuFri.startYear, 1, 1), date(thuFri.endYearExclusive, 1, 1)).foreach { day =>
      withClue(s"$day: ") {
        thuFri.holidays.contains(day) shouldBe
          (thuFri.isHoliday(day) && !thuFri.weekendDays.contains(day.getDayOfWeek))
        thuFri.workingDays.contains(day) shouldBe
          (thuFri.isBusinessDay(day) && thuFri.weekendDays.contains(day.getDayOfWeek))
      }
    }

    // A calendar storing no months has nothing to scan, and answers with two empty sets.
    HOLCAL_SAT_SUN.holidays shouldBe empty
    HOLCAL_SAT_SUN.workingDays shouldBe empty
    HOLCAL_SAT_SUN.holidaysAndWorkingDays._1 shouldBe empty
    HOLCAL_SAT_SUN.holidaysAndWorkingDays._2 shouldBe empty
  }

  //-------------------------------------------------------------------------
  test("test_readOldJodaFormat") {
    // A calendar carrying its own dates decodes only from an object under the `Immutable` key, so
    // a document listing the same fields at its top level is refused while the same calendar in
    // the current structural form decodes with the dates it declares.
    val weekendDays = """"weekendDays":["SATURDAY","SUNDAY"]"""
    val holidays = """"holidays":["1950-01-02","1950-01-03","1950-12-25","1951-01-01"]"""
    val expectedHolidays = List(date(1950, 1, 2), date(1950, 1, 3), date(1950, 12, 25), date(1951, 1, 1))

    // Both documents below are well-formed JSON, so the refusal is about shape, not bad text.
    val legacyDocument =
      s"""{"@bean":"com.opengamma.strata.basics.date.ImmutableHolidayCalendar","id":"NZAU",""" +
        s"""$holidays,$weekendDays}"""
    val withoutBeanName = s"""{"id":"NZAU",$holidays,$weekendDays}"""
    parse(legacyDocument).isRight shouldBe true
    parse(withoutBeanName).isRight shouldBe true

    val refusalOf = (document: String) =>
      decode[ImmutableHolidayCalendar](document) match {
        case Left(failure) => Option(failure.getMessage).getOrElse("")
        case Right(calendar) =>
          fail(s"the document should have been rejected but decoded to: ${calendar.id.name}")
      }

    // The failure names the wrapper the decoder requires.
    decode[ImmutableHolidayCalendar](legacyDocument).isLeft shouldBe true
    refusalOf(legacyDocument) should include("Immutable")

    // ... and refused just the same without the type-name key, so it is the wrapper that is missing
    decode[ImmutableHolidayCalendar](withoutBeanName).isLeft shouldBe true
    refusalOf(withoutBeanName) should include("Immutable")

    val ported = decode[ImmutableHolidayCalendar](s"""{"Immutable":{"id":"NZAU",$weekendDays,$holidays}}""")
      .getOrElse(fail("the calendar in this port's own form did not decode"))
    ported.id shouldBe HolidayCalendarId.of("NZAU")
    ported.holidays.toList shouldBe expectedHolidays
  }
  test("test_of_weekendClosingEveryDay") {
    // A weekend that closes all seven days leaves a calendar with no business day anywhere: no
    // next or previous business day to answer with, and therefore no shift, adjustment, schedule
    // or index observation that could be built on it. That is not a calendar, so no factory of
    // this type builds one. The refusal is a caller-contract `ArgCheck`, as the year-range
    // precondition beside it is, and it is stated at the one place a calendar comes into being,
    // so no factory and no normalising route goes round it.
    val everyDay: List[DayOfWeek] = DayOfWeek.values().toList
    val holidays = List(MON_2014_07_14, WED_2014_07_16)
    val refuses = (weekendDays: List[DayOfWeek], workingDays: List[LocalDate]) =>
      intercept[IllegalArgumentException](
        ImmutableHolidayCalendar.of(TEST_ID, holidays, weekendDays, workingDays))
        .getMessage should include(EveryDayClosedMessage)

    refuses(everyDay, Nil)
    // the order the days arrive in does not matter, and neither does naming one of them twice
    refuses(everyDay.reverse, Nil)
    refuses(everyDay ::: everyDay, Nil)
    // the working days a calendar may declare do not rescue it: they name individual dates
    // inside the years its holidays cover, while a calendar is asked about any year at all
    refuses(everyDay, List(MON_2014_07_14, TUE_2014_07_15, WED_2014_07_16))
    // the three-argument factory and the pre-sorted route this module builds its own calendars
    // through reach the same gate
    intercept[IllegalArgumentException](ImmutableHolidayCalendar.of(TEST_ID, holidays, everyDay))
      .getMessage should include(EveryDayClosedMessage)
    intercept[IllegalArgumentException](
      ImmutableHolidayCalendar.ofNormalized(TEST_ID, SortedSet(holidays: _*), everyDay, Nil))
      .getMessage should include(EveryDayClosedMessage)

    // Six closed days is a different thing and is accepted: such a calendar has one business
    // day a week, which every search of it can reach.
    val thursdaysOnly =
      ImmutableHolidayCalendar.of(TEST_ID, Nil, everyDay.filterNot(day => day == THURSDAY))
    thursdaysOnly.isBusinessDay(THU_2014_07_17) shouldBe true
    thursdaysOnly.isBusinessDay(FRI_2014_07_18) shouldBe false
    thursdaysOnly.next(THU_2014_07_17) shouldBe LocalDate.of(2014, 7, 24)
    thursdaysOnly.previous(THU_2014_07_17) shouldBe LocalDate.of(2014, 7, 10)

    // A document describing a calendar closed on every day is data rather than a mistake in
    // calling code, so the reader of this family refuses it in its error channel instead of
    // raising out of it - and refuses it before building anything from it.
    val everyDayNames = everyDay.map(day => s""""${day.name()}"""").mkString(",")
    decode[ImmutableHolidayCalendar](
      s"""{"Immutable":{"id":"Test1","weekendDays":[$everyDayNames],"holidays":[]}}""") match {
      case Left(failure) =>
        Option(failure.getMessage).getOrElse("") should include(EveryDayClosedMessage)
      case Right(calendar) =>
        fail(s"the document should have been rejected but decoded to: ${calendar.id.name}")
    }

    // ... while the same document with one day left open is read, so the refusal is the weekend
    // that closes everything and not the field it arrived in.
    val sixDayNames = everyDay.filterNot(day => day == THURSDAY).map(day => s""""${day.name()}"""").mkString(",")
    decode[ImmutableHolidayCalendar](
      s"""{"Immutable":{"id":"Test1","weekendDays":[$sixDayNames],"holidays":[]}}""")
      .map(calendar => weekendDaysOf(calendar).size) shouldBe Right(6)
  }

  test("test_packedMonthsAreOwned") {
    // The holiday data of a calendar is an array of packed months, and an array is mutable. A
    // caller able to reach the one a calendar reads could turn a holiday into a business day -
    // or a whole month of them - inside a calendar other code already holds; a caller able to
    // supply one could describe a range of years no factory would accept, or a month layout the
    // indexing arithmetic would misread. Neither is reachable: the array is a private field
    // with no accessor of any kind, and the one constructor that takes an array copies it and
    // judges it before the calendar exists.
    //
    // This test reads the compiled class rather than the source, because what a caller in
    // another language can reach is decided by the class file and not by the source: the
    // visibility Scala spells `private[date]` compiles to public, and the anonymous subclass a
    // `sealed abstract case class` is instantiated through carries a constructor the machine
    // publishes however this file spells it. Reflection is used here for that reason, and only
    // here - it reads the shape of the compiled class, which is the thing being asserted.
    val packedType = classOf[Array[Int]]
    val declaring = classOf[ImmutableHolidayCalendar]
    val runtime = HOLCAL_MON_WED.getClass

    // No published member hands the array out, on the class or on the subclass an instance is.
    withClue("published methods returning the packed months: ") {
      declaring.getMethods.filter(method => method.getReturnType == packedType)
        .map(method => method.getName).toList shouldBe Nil
      runtime.getMethods.filter(method => method.getReturnType == packedType)
        .map(method => method.getName).toList shouldBe Nil
    }
    // ... which follows from the field being private, so that no accessor is generated for it.
    val packedFields = declaring.getDeclaredFields.filter(field => field.getType == packedType).toList
    packedFields.size shouldBe 1
    packedFields.foreach(field => Modifier.isPrivate(field.getModifiers) shouldBe true)
    packedFields.foreach(field => Modifier.isFinal(field.getModifiers) shouldBe true)

    // The constructor the machine publishes is reached below as a caller in another language
    // would reach it, and it makes every check a factory makes.
    val constructor = runtime.getDeclaredConstructors
      .find(candidate =>
        candidate.getParameterTypes.toList ==
          List(classOf[HolidayCalendarId], classOf[Int], classOf[Int], packedType))
      .getOrElse(fail("the compiled subclass no longer has the four-argument constructor this test reads"))
    val storedField = packedFields.head
    storedField.setAccessible(true)
    val stored = storedField.get(HOLCAL_MON_WED).asInstanceOf[Array[Int]]
    stored.length shouldBe 12
    val forge = (weekends: Int, firstYear: Int, months: Array[Int]) =>
      constructor
        .newInstance(Array[AnyRef](TEST_ID, Int.box(weekends), Int.box(firstYear), months): _*)
        .asInstanceOf[ImmutableHolidayCalendar]
    val refusalOfForge = (weekends: Int, firstYear: Int, months: Array[Int]) =>
      Option(intercept[InvocationTargetException](forge(weekends, firstYear, months)).getCause)
        .getOrElse(fail("the constructor refused the representation without saying why"))

    // A representation the factories do produce: the forged calendar answers as the calendar
    // whose data it was built from does, so the route itself is sound.
    val copied = forge(HOLCAL_MON_WED.weekends, HOLCAL_MON_WED.startYear, stored.clone())
    copied.isHoliday(MON_2014_07_14) shouldBe true
    copied.isBusinessDay(TUE_2014_07_15) shouldBe true
    copied.holidays.toList shouldBe List(MON_2014_07_14, WED_2014_07_16)

    // ... and the array it was handed is not the array it reads: writing every day of every
    // stored month to 'business day' after construction changes nothing it answers.
    val supplied = stored.clone()
    val forged = forge(HOLCAL_MON_WED.weekends, HOLCAL_MON_WED.startYear, supplied)
    forged.isHoliday(MON_2014_07_14) shouldBe true
    supplied.indices.foreach(month => supplied(month) = -1)
    forged.isHoliday(MON_2014_07_14) shouldBe true
    forged.isHoliday(WED_2014_07_16) shouldBe true
    forged.holidays.toList shouldBe List(MON_2014_07_14, WED_2014_07_16)

    // The representations no factory of this library could produce, each refused where it
    // cannot be gone round. A weekend closing every day of the week ...
    refusalOfForge((1 << 7) - 1, HOLCAL_MON_WED.startYear, stored.clone())
      .getMessage should include(EveryDayClosedMessage)
    // ... a weekend naming something that is not a day of the week, which nothing would read
    // but equality would carry ...
    refusalOfForge(HOLCAL_MON_WED.weekends | (1 << 7), HOLCAL_MON_WED.startYear, stored.clone())
      .getMessage should include("nothing else")
    refusalOfForge(Int.MinValue, HOLCAL_MON_WED.startYear, stored.clone())
      .getMessage should include("nothing else")
    // ... data that is not whole years of months, which the indexing arithmetic assumes ...
    refusalOfForge(HOLCAL_MON_WED.weekends, HOLCAL_MON_WED.startYear, Array.fill(13)(0))
      .getMessage should include("whole years of months")
    refusalOfForge(HOLCAL_MON_WED.weekends, HOLCAL_MON_WED.startYear, Array.fill(1)(0))
      .getMessage should include("whole years of months")
    // ... and a range of years outside the ones a calendar answers about, at either end and
    // where the first year is inside the range but the months run past the end of it.
    refusalOfForge(HOLCAL_MON_WED.weekends, -1, stored.clone())
      .getMessage should include(UnsupportedDateMessage)
    refusalOfForge(HOLCAL_MON_WED.weekends, 10000, stored.clone())
      .getMessage should include(UnsupportedDateMessage)
    refusalOfForge(HOLCAL_MON_WED.weekends, 9999, Array.fill(24)(0))
      .getMessage should include(UnsupportedDateMessage)
    refusalOfForge(HOLCAL_MON_WED.weekends, Int.MaxValue, Array.fill(12)(0))
      .getMessage should include(UnsupportedDateMessage)

    // ... a month naming a day that month does not have, which no factory can express - each
    // month of a built calendar starts from its own length - and which is not inert: the search
    // of the stored months would answer with the day the bit sits at, and the date would be
    // constructed, and fail, in a caller that asked an ordinary question. The 32nd of January,
    // which is the one bit a 31-day month leaves over ...
    val januaryDay32 = stored.clone()
    januaryDay32(0) = januaryDay32(0) | (1 << 31)
    withClue("a 32nd day of January: ") {
      val refusal = refusalOfForge(HOLCAL_MON_WED.weekends, HOLCAL_MON_WED.startYear, januaryDay32)
      refusal.getMessage should include("cannot name a day its month does not have")
      refusal.getMessage should include("2014-01")
    }
    // ... the 29th of February in a year that has 28, which is why the check is against the
    // month of its own year and not against a month of some nominal one ...
    val februaryDay29 = stored.clone()
    februaryDay29(1) = februaryDay29(1) | (1 << 28)
    withClue("a 29th day of February 2014: ") {
      val refusal = refusalOfForge(HOLCAL_MON_WED.weekends, HOLCAL_MON_WED.startYear, februaryDay29)
      refusal.getMessage should include("cannot name a day its month does not have")
      refusal.getMessage should include("2014-02")
    }
    // ... and the 31st of a 30-day month.
    val aprilDay31 = stored.clone()
    aprilDay31(3) = aprilDay31(3) | (1 << 30)
    withClue("a 31st day of April: ") {
      val refusal = refusalOfForge(HOLCAL_MON_WED.weekends, HOLCAL_MON_WED.startYear, aprilDay31)
      refusal.getMessage should include("cannot name a day its month does not have")
      refusal.getMessage should include("2014-04")
    }
    // The same bit in a leap year names a day that exists, and is accepted and answered with,
    // so the check refuses what is impossible rather than what is merely unusual.
    val leapFebruary = Array.fill(12)(0)
    leapFebruary(1) = 1 << 28
    val leapDayOnly = forge(HOLCAL_MON_WED.weekends, 2016, leapFebruary)
    leapDayOnly.isBusinessDay(LocalDate.of(2016, 2, 29)) shouldBe true
    leapDayOnly.isHoliday(LocalDate.of(2016, 2, 26)) shouldBe true
    leapDayOnly.holidays.toList should contain(LocalDate.of(2016, 2, 26))

    // An empty range is accepted, a calendar with no holidays of its own being an ordinary
    // thing, and it answers from its weekend ...
    val weekendOnly = forge(HOLCAL_MON_WED.weekends, 0, Array.emptyIntArray)
    weekendOnly.isHoliday(SAT_2014_07_12) shouldBe true
    weekendOnly.isBusinessDay(MON_2014_07_14) shouldBe true
    weekendOnly.holidays.toList shouldBe Nil
    // ... at the one year an empty range is written with. There is no first year of a range
    // that holds nothing, every factory writes zero there, and two calendars that hold nothing
    // are therefore one value rather than ten thousand.
    refusalOfForge(HOLCAL_MON_WED.weekends, 2014, Array.emptyIntArray)
      .getMessage should include("must start at year 0")
  }

  test("test_shift_beyondTheLimit_withinStoredMonths") {
    // A calendar walks a day at a time to shift by business days, so the number of days it may
    // be asked to shift by is bounded - see the limit on `HolidayCalendar.shift`. This calendar
    // holds data for seven thousand years, which is what makes the case worth stating on its
    // own: `ImmutableHolidayCalendar` answers a shift from its stored months without reaching
    // the inherited method at all, so a calendar holding a long range would walk millions of
    // days inside its own data before any check written only in the inherited method was
    // reached. The limit would then hold for exactly the calendars holding the least data.
    val longRange = ImmutableHolidayCalendar.of(
      HolidayCalendarId.of("TestLongRange"),
      List(LocalDate.of(2000, 1, 1), LocalDate.of(9000, 1, 1)),
      SATURDAY,
      SUNDAY)
    longRange.monthCount shouldBe (9000 - 2000 + 1) * 12

    // At the limit the search stays inside the stored months and answers, which is what makes
    // the refusal below a refusal by the limit rather than by the fallback: the fallback is
    // never reached on either side of it.
    val start = LocalDate.of(2000, 1, 3)
    val atTheLimit = longRange.shift(start, HolidayCalendar.MaxBusinessDayShift)
    atTheLimit.getYear should be > 2300
    atTheLimit.getYear should be < 9000
    longRange.isBusinessDay(atTheLimit) shouldBe true
    val backAgain = longRange.shift(atTheLimit, -HolidayCalendar.MaxBusinessDayShift)
    backAgain shouldBe start

    // One day past it is refused, in both directions, before any day is walked.
    val beyond = s"A shift of more than ${HolidayCalendar.MaxBusinessDayShift} business days"
    intercept[IllegalArgumentException] {
      longRange.shift(start, HolidayCalendar.MaxBusinessDayShift + 1)
    }.getMessage should include(beyond)
    intercept[IllegalArgumentException] {
      longRange.shift(atTheLimit, -(HolidayCalendar.MaxBusinessDayShift + 1))
    }.getMessage should include(beyond)
    // ... including the two magnitudes that would overrun a machine word, the smaller of which
    // is its own negation and so passes a check made in `Int` arithmetic.
    intercept[IllegalArgumentException](longRange.shift(start, Int.MaxValue))
      .getMessage should include(beyond)
    intercept[IllegalArgumentException](longRange.shift(atTheLimit, Int.MinValue))
      .getMessage should include(beyond)

    // The one calendar that answers a shift without walking anything is unaffected, its answer
    // being arithmetic on the date rather than a search: every day is a business day, so the
    // shift is the number of days.
    HolidayCalendars.NO_HOLIDAYS.shift(start, Int.MaxValue).getYear should be > 5000000
  }
}
