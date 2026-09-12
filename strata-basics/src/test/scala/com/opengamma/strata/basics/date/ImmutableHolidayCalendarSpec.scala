/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.DayOfWeek
import java.time.DayOfWeek.FRIDAY
import java.time.DayOfWeek.SATURDAY
import java.time.DayOfWeek.SUNDAY
import java.time.DayOfWeek.THURSDAY
import java.time.LocalDate

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
 * The Java original ran forty-eight annotated methods - twenty-seven plain tests and
 * twenty-one parameterised ones fed by fifteen data providers. All forty-eight are ported
 * here, one test each, under the name the Java method had, and every provider is transcribed
 * row for row into a table that its tests drive. Several providers feed two methods, which is
 * why a table is declared once as a private value and read by each test that used it: the
 * method-level mapping of the migration stays one-to-one rather than collapsing a pair of
 * methods into a single test.
 *
 * Java `ImmutableHolidayCalendarTest.test_readOldJodaFormat` read a calendar out of a classpath
 * fixture written by the previous serialization library, and AAP section 0.2.2 places that
 * fixture and all wire compatibility with that library out of scope. What survives the exclusion
 * is this port's own position on such a document, which is a behaviour rather than an absence: a
 * document in the legacy shape is refused, and the same calendar in the shape this port
 * publishes decodes. The method is ported as those assertions - `test_readOldJodaFormat`, the
 * last test of this spec - with both documents transcribed inline, so the fixture is not read
 * and this spec still loads no resource of any kind. The JSON form asserted by
 * `test_serialization` is likewise this port's own structural form rather than the serialized
 * form of the library being ported.
 *
 * ===Equality is the identifier alone===
 *
 * [[ImmutableHolidayCalendar]] compares equal on its identifier and nothing else, exactly as
 * the Java original did, which `test_equals` proves by asserting that two calendars with
 * different holidays and the same identifier '''are''' equal. Every assertion in this spec that
 * is about the content of a calendar therefore goes through [[ImmutableHolidayCalendar.holidays]],
 * [[ImmutableHolidayCalendar.workingDays]] or [[HolidayCalendar.isHoliday]], never through `==`:
 * an `==` between two calendars would pass however wrong their holidays were.
 *
 * ===Where an exception is still the right answer===
 *
 * The factories of this type are total - a list of dates and a weekend always describe a
 * calendar - so no construction in this spec has an error channel to assert, and nothing here
 * expects a `Left`. What the Java original asserted as `IllegalArgumentException` in nineteen
 * places is the precondition a calendar keeps about the years it deals in: it can neither answer
 * for, nor hold data for, a date whose year lies outside 0 to 9999. That is a fault in the
 * calling code rather than a property of market data, so it fails fast through `ArgCheck`, as
 * AAP section 0.3.3 sanctions, and [[rejectsUnsupportedDate]] asserts both the exception and the
 * message that names the precondition. The same rule reaches construction, where it also bounds
 * what building a calendar costs - see `test_of_unsupportedYears` - and reaches a JSON document,
 * where it is a decoding failure rather than a throw because a document is data; see
 * `test_serialization_hostileDocument`. The two `null` tests lose their subject entirely and
 * become compile-time proofs; see them for the reasoning.
 *
 * Numerical parity of the built-in national calendars against the Java implementation is a
 * separate concern owned by the holiday parity fixture and its spec. What is asserted here is
 * what the Java test asserted, transcribed row for row, over calendars this spec builds itself.
 *
 * @see [[HolidayCalendar]] for the sealed family and its composites
 * @see [[HolidayCalendarId]] for the identifiers, whose composition this spec relies on
 */
final class ImmutableHolidayCalendarSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /** The identifier of most of the calendars built here. */
  private val TEST_ID: HolidayCalendarId = HolidayCalendarId.of("Test1")

  /** A second identifier, for the calendars that are combined with the first. */
  private val TEST_ID2: HolidayCalendarId = HolidayCalendarId.of("Test2")

  // The dates of the Java original, named by their day of the week so that a weekend
  // expectation can be read without consulting a calendar. Only the dates this spec asserts on
  // are declared, because an unused value is a compile error under the build's warning settings.
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

  /** A calendar whose holidays are a Monday and the Wednesday of the following week. */
  private val HOLCAL_MON_WED: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(TEST_ID, List(MON_2014_07_14, WED_2014_07_16), SATURDAY, SUNDAY)

  /** A calendar whose holidays straddle a year end, which is what exercises the month array. */
  private val HOLCAL_YEAR_END: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(
      HolidayCalendarId.of("TestYearEnd"),
      List(TUE_2014_12_30, THU_2015_01_01),
      SATURDAY,
      SUNDAY)

  /** A calendar with no holidays of its own, so that only its weekend applies. */
  private val HOLCAL_SAT_SUN: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(
      HolidayCalendarId.of("TestSatSun"),
      List.empty[LocalDate],
      SATURDAY,
      SUNDAY)

  /** A calendar whose holidays fall at the end of two consecutive months. */
  private val HOLCAL_END_MONTH: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(
      HolidayCalendarId.of("TestEndOfMonth"),
      List(MON_2014_06_30, THU_2014_07_31),
      SATURDAY,
      SUNDAY)

  /** The part of the `ArgCheck` message that names the precondition on the year of a date. */
  private val UnsupportedDateMessage: String = "outside the accepted range"

  //-------------------------------------------------------------------------
  /**
   * Returns the weekend days of a calendar in ascending order of day of the week.
   *
   * The accessor answers with a set, whose iteration order is not part of its contract, and the
   * Java original asserted its weekend days in ascending order. Sorting here gives an ordered
   * comparison that is both faithful to the original and independent of how a set happens to be
   * laid out.
   *
   * @param calendar  the calendar to read
   * @return the weekend days, from Monday towards Sunday
   */
  private def weekendDaysOf(calendar: ImmutableHolidayCalendar): List[DayOfWeek] =
    calendar.weekendDays.toList.sortBy(day => day.getValue)

  /**
   * Returns the days of a range, one at a time and in ascending order.
   *
   * Used where a calendar has to be compared with another over every day it holds data for,
   * which is the only way to compare the content of two calendars: equality compares their
   * identifiers alone.
   *
   * @param startInclusive  the first day to visit
   * @param endExclusive  the day to stop before
   * @return the days of the range, in ascending order
   */
  private def datesFrom(startInclusive: LocalDate, endExclusive: LocalDate): Iterator[LocalDate] =
    Iterator.iterate(startInclusive)(date => date.plusDays(1L)).takeWhile(date => date.isBefore(endExclusive))

  /**
   * Asserts that an operation rejects a date whose year lies outside 0 to 9999.
   *
   * This is the calendar's one caller-contract precondition, sanctioned as a fail-fast
   * `ArgCheck` throw by AAP section 0.3.3: a calendar holds its holidays as an array of months
   * and cannot answer for a date no calendar could hold data for. The message is asserted as
   * well as the exception, so that a different `IllegalArgumentException` - one raised for some
   * other reason - cannot satisfy the assertion.
   *
   * @param operation  the calendar operation to invoke
   * @return the assertion that the operation rejected the date
   */
  private def rejectsUnsupportedDate(operation: => Any): Assertion =
    intercept[IllegalArgumentException](operation).getMessage should include(UnsupportedDateMessage)

  //-------------------------------------------------------------------------
  test("test_of_IterableDayOfWeekDayOfWeek_null") {
    val holidays = List(MON_2014_07_14, FRI_2014_07_18)
    // The Java original asserted that each of the four arguments rejects null. Scala's types
    // make those cases unrepresentable rather than checked: `ArgCheck` has no `notNull` family
    // (AAP section 0.4.1), because a caller cannot reach a factory without supplying every
    // argument and no argument of this factory is optional. What remains to prove is the
    // compile-time requirement itself, one line for each of the four arguments the Java test
    // passed as null, followed by the complete call that does build a calendar.
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
    // As above: the three null cases of the Java original are unrepresentable, and what is
    // proved instead is that none of the three arguments can be left out or filled with a value
    // of another kind - one line for each argument the Java test passed as null.
    assertDoesNotCompile("ImmutableHolidayCalendar.of(holidays, weekendDays)") // the id
    assertDoesNotCompile("ImmutableHolidayCalendar.of(TEST_ID, weekendDays, weekendDays)") // the holidays
    assertDoesNotCompile("ImmutableHolidayCalendar.of(TEST_ID, holidays)") // the weekend days
    val test = ImmutableHolidayCalendar.of(TEST_ID, holidays, weekendDays)
    test.id shouldBe TEST_ID
    weekendDaysOf(test) shouldBe List(THURSDAY, FRIDAY)
    // both declared dates are holidays, one as a declared holiday and one because it is also a
    // weekend day of this calendar
    test.isHoliday(MON_2014_07_14) shouldBe true
    test.isHoliday(FRI_2014_07_18) shouldBe true
    // the accessor reports the declared holiday alone, because a date that is a holiday and a
    // weekend day of the same calendar is carried by the weekend: the stored months record a
    // day as a business day or not, so the two reasons for closing are indistinguishable once
    // stored. This is the behaviour of the Java original, whose own accessor skips a date
    // falling on a weekend day in exactly the same way.
    test.holidays.toList shouldBe List(MON_2014_07_14)
  }

  //-------------------------------------------------------------------------
  /**
   * The Saturday/Sunday provider of the Java original, transcribed row for row.
   *
   * The calendar the two tests build holds a Monday and the Friday of the following week as
   * holidays, so the rows walk a fortnight in which every kind of day appears: an ordinary
   * business day, each of the two weekend days, and a holiday that falls on a weekday.
   */
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

    // The factory normalises what it is given, which is what makes this a construction of kind
    // [N] in AAP section 0.3.3: the same holidays supplied out of order and duplicated describe
    // the same calendar, and the set it reports back is sorted and deduplicated.
    val unordered =
      ImmutableHolidayCalendar.of(TEST_ID, List(FRI_2014_07_18, MON_2014_07_14, FRI_2014_07_18), weekendDays)
    unordered.holidays.toList shouldBe holidays
    unordered.startYear shouldBe 2014
    unordered.endYearExclusive shouldBe 2015
    // ... and the two calendars answer alike on every day they hold data for. Compared day by
    // day rather than by equality, which would compare their identifiers and pass regardless.
    datesFrom(LocalDate.of(2014, 1, 1), LocalDate.of(2015, 1, 1)).foreach { date =>
      withClue(s"$date: ") {
        unordered.isHoliday(date) shouldBe test.isHoliday(date)
      }
    }
    succeed
  }

  //-------------------------------------------------------------------------
  /**
   * The Thursday/Friday provider of the Java original, transcribed row for row.
   *
   * The calendar holds a Monday and a Saturday as holidays, the Saturday being a holiday that
   * falls outside this calendar's weekend, so that a holiday and a weekend day are told apart.
   */
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
  /**
   * The single-day weekend provider of the Java original, transcribed row for row.
   *
   * The calendar is built by naming Sunday twice, which is how a one-day weekend is described
   * through the two-day factory, and it holds a Monday and a Thursday as holidays.
   */
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
  /**
   * The three-day weekend provider of the Java original, transcribed row for row.
   *
   * A weekend of Thursday, Friday and Saturday leaves Sunday to Wednesday as the working week,
   * and the calendar holds the Monday and Tuesday of one of those weeks as holidays.
   */
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
  /**
   * The no-weekend provider of the Java original, transcribed row for row.
   *
   * A calendar with an empty weekend has to name every non-business day as a holiday, which is
   * how a centre that works a seven-day week is described; only the two declared holidays are
   * not business days.
   */
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
  /**
   * The no-holiday provider of the Java original, transcribed row for row.
   *
   * A calendar with a Friday/Saturday weekend and no holidays at all holds no month data, so
   * every answer comes from the weekend alone - the path this provider exercises.
   */
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
   * The working-day override provider of the Java original, transcribed row for row.
   *
   * The calendar the two tests build holds the Wednesday and Thursday of one week as holidays,
   * takes Friday and Saturday as its weekend, and names the Saturday a working day. Each row
   * therefore reads one of the four ways a day can be decided: an ordinary business day, a
   * declared holiday, a weekend day, and a weekend day overridden into a business day. The
   * Sunday is a business day because Sunday is not part of this calendar's weekend.
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
    // Thursday is named as both a holiday and a working day, and is a business day. It is not
    // reported as a working day, because that accessor reports the weekend days a calendar
    // treats as business days, and the Thursday is not part of this calendar's weekend.
    val overridesHoliday =
      ImmutableHolidayCalendar.of(TEST_ID, holidays, weekendDays, List(THU_2014_07_10))
    overridesHoliday.isBusinessDay(THU_2014_07_10) shouldBe true
    overridesHoliday.holidays.toList shouldBe List(WED_2014_07_09)
    overridesHoliday.workingDays.toList shouldBe List.empty[LocalDate]

    // A working day outside the years the holidays span is ignored, because outside that range
    // the calendar applies its weekend and holds no data an override could apply to. The
    // Saturday of January 2015 is therefore still a holiday, and is not reported as a working
    // day, while the Saturday inside the range is both.
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
    // the two calendars cover ranges four years apart, so the merged calendar is rebuilt from
    // the dates both declare and now starts in 2010, carrying the working-day override with it
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
    // the first calendar starts a year after the second, so the merge has to align the two
    // arrays of months before intersecting them - an off-by-one here would move a holiday by a
    // month, which is what the two dates in March and April 2015 detect
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
    // the same pair of calendars the other way round, which has to give the same answers: the
    // merge chooses the earlier start as its base rather than the calendar it was handed first
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
    // the two ranges do not meet at all - 2018 against 2015 - so the merged calendar is rebuilt
    // from the dates both declare and covers the years in between; the 2014 dates fall outside
    // even the merged range and are answered by the weekend alone
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
    // outside the years its holidays span the calendar applies its weekend alone, rather than
    // declaring those years free of holidays
    test.isBusinessDay(LocalDate.of(2013, 12, 31)) shouldBe true
    test.isBusinessDay(LocalDate.of(2015, 1, 1)) shouldBe true
    // precondition: a calendar cannot answer for a date whose year lies outside 0 to 9999
    rejectsUnsupportedDate(test.isBusinessDay(LocalDate.MIN))
    rejectsUnsupportedDate(test.isBusinessDay(LocalDate.MAX))
  }

  //-------------------------------------------------------------------------
  /**
   * The shift provider of the Java original, transcribed row for row.
   *
   * Every row shifts a date against [[HOLCAL_MON_WED]] by one, two, zero, minus one or minus two
   * business days, and the twelve dates of each block walk a fortnight containing two holidays
   * and two weekends. A shift of zero returns the date given, business day or not, which is the
   * behaviour the third block pins.
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
    // a calendar that holds no month data shifts through its weekend alone
    HOLCAL_SAT_SUN.shift(SAT_2014_07_12, -2) shouldBe THU_2014_07_10
    HOLCAL_SAT_SUN.shift(SAT_2014_07_12, 2) shouldBe TUE_2014_07_15
  }

  test("test_shift_range") {
    // a year the calendar holds no holidays for is shifted through its weekend alone
    HOLCAL_MON_WED.shift(date(2010, 1, 1), 1) shouldBe date(2010, 1, 4)
    // precondition: a calendar cannot answer for a date whose year lies outside 0 to 9999
    rejectsUnsupportedDate(HOLCAL_MON_WED.shift(LocalDate.MIN, 1))
    rejectsUnsupportedDate(HOLCAL_MON_WED.shift(LocalDate.MAX.minusDays(1L), 1))
  }

  test("test_adjustBy") {
    forAll(data_shift) { (date: LocalDate, amount: Int, expected: LocalDate) =>
      withClue(s"$date adjusted by $amount: ") {
        // the adjuster applies the same shift, whether it is called directly or handed to a
        // date as the `java.time` temporal adjuster it also is - the second form is the one the
        // Java original used
        HOLCAL_MON_WED.adjustBy(amount).adjust(date) shouldBe expected
        date.`with`(HOLCAL_MON_WED.adjustBy(amount)) shouldBe expected
      }
    }
  }

  //-------------------------------------------------------------------------
  /**
   * The `next` provider of the Java original, transcribed row for row.
   *
   * The first block walks a fortnight of [[HOLCAL_MON_WED]]; the second crosses the end of 2014
   * with [[HOLCAL_YEAR_END]], whose holidays straddle the year boundary, so that a step from one
   * stored year into the next is exercised; the last two rows step out of the stored months
   * altogether and into a calendar that holds none.
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
    // precondition: a calendar cannot answer for a date whose year lies outside 0 to 9999
    rejectsUnsupportedDate(HOLCAL_MON_WED.next(LocalDate.MIN))
    rejectsUnsupportedDate(HOLCAL_MON_WED.next(LocalDate.MAX.minusDays(1L)))
  }

  //-------------------------------------------------------------------------
  /**
   * The `nextOrSame` provider of the Java original, transcribed row for row.
   *
   * The same walks as the `next` provider, with the expectations of a method that returns the
   * date itself where it is already a business day - the whole of what "or same" adds.
   */
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
    // precondition: a calendar cannot answer for a date whose year lies outside 0 to 9999
    rejectsUnsupportedDate(HOLCAL_MON_WED.nextOrSame(LocalDate.MIN))
    rejectsUnsupportedDate(HOLCAL_MON_WED.nextOrSame(LocalDate.MAX))
  }

  //-------------------------------------------------------------------------
  /**
   * The `previous` provider of the Java original, transcribed row for row.
   *
   * The mirror of the `next` provider: the same fortnight and the same year end, read backwards,
   * so that the search towards earlier months is exercised as thoroughly as the search forwards.
   */
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
    // precondition: a calendar cannot answer for a date whose year lies outside 0 to 9999
    rejectsUnsupportedDate(HOLCAL_MON_WED.previous(LocalDate.MIN.plusDays(1L)))
    rejectsUnsupportedDate(HOLCAL_MON_WED.previous(LocalDate.MAX))
  }

  //-------------------------------------------------------------------------
  /**
   * The `previousOrSame` provider of the Java original, transcribed row for row.
   */
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
    // precondition: a calendar cannot answer for a date whose year lies outside 0 to 9999
    rejectsUnsupportedDate(HOLCAL_MON_WED.previousOrSame(LocalDate.MIN))
    rejectsUnsupportedDate(HOLCAL_MON_WED.previousOrSame(LocalDate.MAX))
  }

  //-------------------------------------------------------------------------
  /**
   * The `nextSameOrLastInMonth` provider of the Java original, transcribed row for row.
   *
   * This is the adjustment the modified-following convention makes, so the answer can be earlier
   * than the date given: the last two blocks are the rows where the next business day would fall
   * in the following month and the last business day of this one is returned instead.
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
    // precondition: a calendar cannot answer for a date whose year lies outside 0 to 9999
    rejectsUnsupportedDate(HOLCAL_MON_WED.nextSameOrLastInMonth(LocalDate.MIN))
    rejectsUnsupportedDate(HOLCAL_MON_WED.nextSameOrLastInMonth(LocalDate.MAX))
  }

  //-------------------------------------------------------------------------
  /**
   * The last-business-day-of-month provider of the Java original, transcribed row for row
   * including its comments.
   *
   * Read against [[HOLCAL_END_MONTH]], the four months cover the four ways a month can end: on a
   * declared holiday that is a weekday, on a holiday that is the last day of the month, at a
   * weekend, and on an ordinary business day.
   */
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
        // only the date that is the last business day of its month answers true
        HOLCAL_END_MONTH.isLastBusinessDayOfMonth(date) shouldBe (date == expectedEom)
      }
    }
  }

  test("test_lastBusinessDayOfMonth_satSun") {
    // a calendar that holds no month data answers from its weekend alone, and the Monday that
    // ends June 2014 is the last business day of that month
    HOLCAL_SAT_SUN.isLastBusinessDayOfMonth(MON_2014_06_30) shouldBe true
    HOLCAL_SAT_SUN.lastBusinessDayOfMonth(MON_2014_06_30) shouldBe MON_2014_06_30
  }

  test("test_lastBusinessDayOfMonth_range") {
    // a year the calendar holds no holidays for is answered from its weekend alone
    HOLCAL_END_MONTH.lastBusinessDayOfMonth(date(2010, 1, 1)) shouldBe date(2010, 1, 29)
    // precondition: a calendar cannot answer for a date whose year lies outside 0 to 9999
    rejectsUnsupportedDate(HOLCAL_END_MONTH.lastBusinessDayOfMonth(LocalDate.MIN))
    rejectsUnsupportedDate(HOLCAL_END_MONTH.lastBusinessDayOfMonth(LocalDate.MAX))
  }

  test("test_isLastBusinessDayOfMonth_range") {
    HOLCAL_END_MONTH.isLastBusinessDayOfMonth(date(2010, 1, 1)) shouldBe false
    // precondition: a calendar cannot answer for a date whose year lies outside 0 to 9999
    rejectsUnsupportedDate(HOLCAL_END_MONTH.isLastBusinessDayOfMonth(LocalDate.MIN))
    rejectsUnsupportedDate(HOLCAL_END_MONTH.isLastBusinessDayOfMonth(LocalDate.MAX))
  }

  //-------------------------------------------------------------------------
  /**
   * The business-day counting provider of the Java original, transcribed row for row.
   *
   * Every row counts from the same Friday, so the expected counts grow by one for each business
   * day crossed and stay level across the weekend and the two holidays of [[HOLCAL_MON_WED]].
   * The start date is included in the count and the end date is not, which is why a range whose
   * ends are equal counts zero.
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
    // `combinedWith` reads both calendars on every query rather than merging their data, so the
    // holidays of both and the weekends of both close a day
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
    // Ten calendars of pseudo-random holidays over a decade, each checked day by day against
    // the simple algorithm the Java original used: a day is a holiday where it falls at the
    // weekend or belongs to the set of dates the calendar was built from. This is what exercises
    // the array of monthly bit masks over thousands of dates rather than the dozens the tables
    // above cover. The generator is seeded with the seed of the Java original and
    // `scala.util.Random` wraps the same algorithm, so the ten sets of dates are the sets the
    // Java test built and the sweep is deterministic, as the AAP requires of every test here.
    val start = LocalDate.of(2010, 1, 1)
    val end = LocalDate.of(2020, 1, 1)
    val random = new Random(547698L)
    (0 until 10).foreach { index =>
      // the holidays of this calendar: the start date, then a step of one to ten days at a time
      // until the end of the decade, which is the loop of the Java original
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
    // The last assertion is the reason no content is ever asserted through equality in this
    // spec: two calendars claiming to be the same calendar are equal however far apart their
    // holidays are, as the Java original also held, because a calendar is identified by what it
    // claims to be rather than by the dates one set of reference data happened to supply. The
    // content of a calendar is therefore compared through `holidays` or `isHoliday` throughout.
    a1.holidays.toList should not be c.holidays.toList
    a1.hashCode shouldBe c.hashCode
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java original swept the bean reflectively. Nothing reflective is ported, so what is
    // asserted here is the contract that sweep stood for: the one equality-bearing type class
    // instance of the family agrees with `equals`, equal values hash alike, calendars of
    // different identifiers are unequal, and the rendering is the `toString` of the original.
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

    // a calendar is equal to nothing that is not a calendar; the value is typed as `Any` so that
    // the comparison is the `equals` contract rather than a comparison of unrelated types
    val unrelated: Any = TEST_ID.name
    HOLCAL_MON_WED.equals(unrelated) shouldBe false
  }

  test("test_serialization") {
    // The Java original round-tripped the calendar through Java serialization, which is not
    // supported by any type of this port. The equivalent here is the JSON form of AAP section
    // 0.6.4: a calendar that is not one of the built-in set is written as an object under the
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

    // Asserted both ways, as AAP section 0.6.2 requires. Equality compares identifiers alone, so
    // on its own it would accept a calendar whose holidays had been lost; the holiday set and a
    // day-by-day sweep of the years the calendar covers are what prove the dates survived.
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

    // ... and this is why: a document that has lost its holidays decodes to a calendar that is
    // equal to the original, so only the two content assertions above can tell them apart. The
    // first year is omitted here rather than kept at 2014, because a calendar with no holidays
    // starts at year zero and a document whose declared start year sits after its earliest
    // holiday is rejected - see `test_serialization_hostileDocument`.
    val withoutHolidays = decode[ImmutableHolidayCalendar](
      """{"Immutable":{"id":"Test1","weekendDays":["SATURDAY","SUNDAY"],""" +
        """"holidays":[],"workingWeekendDays":[]}}"""
    ).getOrElse(fail("the calendar without holidays could not be decoded"))
    withoutHolidays shouldBe HOLCAL_MON_WED
    withoutHolidays.holidays.toList should not be HOLCAL_MON_WED.holidays.toList
    withoutHolidays.isHoliday(MON_2014_07_14) shouldBe false
  }

  test("test_serialization_overriddenBuiltInId") {
    // A calendar an application supplies under a standard identifier must survive a round trip
    // with its own dates. Nothing but identity can decide that: this calendar is `==` to the
    // library's London calendar, because a calendar carrying holiday data compares on its
    // identifier alone, so a document written from equality with the built-in set would hold the
    // bare name `GBLO` and read back as the library's calendar - silently replacing every
    // holiday and working day the application declared. The same would happen to the
    // weekend-only calendar `HolidayCalendars.defaultingReferenceData` supplies under a standard
    // identifier.
    val ownLondon = ImmutableHolidayCalendar.of(
      HolidayCalendarIds.GBLO,
      List(MON_2014_07_14, WED_2014_07_16),
      List(SATURDAY, SUNDAY),
      List(SAT_2014_07_12))
    val library = StandardHolidayCalendars.GBLO

    (ownLondon == library) shouldBe true
    ownLondon.holidays.toList should not be library.holidays.toList

    // The library's own calendar is written as its name, and read back as that very instance.
    library.asJson shouldBe parse("\"GBLO\"").getOrElse(fail("the expected JSON is not valid JSON"))
    decode[HolidayCalendar]("\"GBLO\"") shouldBe Right(library)
    decode[HolidayCalendar]("\"GBLO\"").getOrElse(fail("GBLO did not decode")) should
      be theSameInstanceAs library

    // The application's calendar of the same identifier is written structurally instead ...
    val json = ownLondon.asJson
    json.isString shouldBe false
    json shouldBe parse(
      """{"Immutable":{"id":"GBLO","weekendDays":["SATURDAY","SUNDAY"],"startYear":2014,""" +
        """"holidays":["2014-07-14","2014-07-16"],"workingWeekendDays":["2014-07-12"]}}"""
    ).getOrElse(fail("the expected JSON of this test is not valid JSON"))

    // ... and reads back carrying its own dates, which is the whole point of the distinction.
    val roundTripped = decode[ImmutableHolidayCalendar](json.noSpaces)
      .getOrElse(fail(s"the calendar did not survive the round trip: ${json.noSpaces}"))
    roundTripped.id shouldBe HolidayCalendarIds.GBLO
    roundTripped.holidays.toList shouldBe List(MON_2014_07_14, WED_2014_07_16)
    roundTripped.workingDays.toList shouldBe List(SAT_2014_07_12)
    roundTripped.isHoliday(MON_2014_07_14) shouldBe true
    roundTripped.isBusinessDay(SAT_2014_07_12) shouldBe true
    // the dates it kept are its own, not London's: 26 December 2014 is a holiday in London and a
    // business day here
    val boxingDay2014 = date(2014, 12, 26)
    library.isHoliday(boxingDay2014) shouldBe true
    roundTripped.isHoliday(boxingDay2014) shouldBe false
  }

  test("test_of_unsupportedYears") {
    // A calendar holds one machine word per month from its earliest holiday to its latest, so
    // the years its holidays span decide what building it allocates. The years a calendar can be
    // asked about - 0 to 9999 - are therefore also the years it can be built from, which caps
    // that array at 120,000 months whoever supplied the dates. Without the cap a pair of dates a
    // million years apart would ask for an array of tens of millions of months, and a year far
    // enough out would overflow its length.
    val weekend = List(SATURDAY, SUNDAY)
    val rejectsUnsupportedHoliday = (holidays: List[LocalDate]) =>
      intercept[IllegalArgumentException](
        ImmutableHolidayCalendar.of(TEST_ID, holidays, weekend)).getMessage should include(
        UnsupportedDateMessage)

    rejectsUnsupportedHoliday(List(LocalDate.MAX))
    rejectsUnsupportedHoliday(List(LocalDate.MIN))
    rejectsUnsupportedHoliday(List(date(10000, 1, 1)))
    rejectsUnsupportedHoliday(List(date(-1, 12, 31)))
    // one end in range and the other not, in both directions: both ends are checked because both
    // are needed to work out how many months lie between them
    rejectsUnsupportedHoliday(List(MON_2014_07_14, LocalDate.MAX))
    rejectsUnsupportedHoliday(List(LocalDate.MIN, MON_2014_07_14))

    // The ends of the supported range are accepted, and a calendar built at them behaves.
    val earliest = ImmutableHolidayCalendar.of(TEST_ID, List(date(0, 1, 3)), weekend)
    earliest.startYear shouldBe 0
    earliest.isHoliday(date(0, 1, 3)) shouldBe true
    val latest = ImmutableHolidayCalendar.of(TEST_ID, List(date(9999, 12, 31)), weekend)
    latest.endYearExclusive shouldBe 10000
    latest.isHoliday(date(9999, 12, 31)) shouldBe true

    // A working day outside the years the holidays span is ignored rather than rejected, which is
    // the behaviour of the library being ported and is unchanged: it names a date in a range the
    // calendar holds no data for, so there is nothing there for it to override.
    val ignoredWorkingDay =
      ImmutableHolidayCalendar.of(TEST_ID, List(MON_2014_07_14), weekend, List(date(9999, 12, 25)))
    ignoredWorkingDay.workingDays shouldBe empty
    ignoredWorkingDay.endYearExclusive shouldBe 2015
  }

  test("test_unsupportedYears_optimizedPaths") {
    // Every operation that reads the stored months rejects a date in a year the calendar cannot
    // hold data for, and does so whatever arithmetic the date implies. The date below is the case
    // that makes this worth a test of its own: with 2014 as the first year stored, the month
    // index of January 357915956 is (357915956 - 2014) * 12, which is 4,294,967,304 - eight more
    // than two to the thirty-two. Computed in `Int` arithmetic that wraps to 8, an index the
    // twelve stored months of 2014 contain, so the calendar would have answered for a date
    // 355,900 years away from its data using the bits of September 2014. Measured exactly, the
    // month is outside the stored months and the date is refused.
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

    // A date inside the stored months is unaffected, so the check has not swallowed the fast path
    // it guards: this Monday is one of the two holidays the calendar declares.
    HOLCAL_MON_WED.isHoliday(MON_2014_07_14) shouldBe true
    HOLCAL_MON_WED.next(MON_2014_07_14) shouldBe TUE_2014_07_15
    HOLCAL_MON_WED.previous(MON_2014_07_14) shouldBe FRI_2014_07_11
  }

  test("test_serialization_hostileDocument") {
    // A document is data, so a document that describes a calendar no calendar could be is a
    // decoding failure rather than a throw - and the failure comes before anything is built from
    // it, because building is what allocates from the years the dates span.
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

    // a working day a calendar cannot hold: ignored once built, but still not a date a document
    // may name, so it is refused rather than quietly dropped
    rejects(
      s"""{"Immutable":{"id":"Test1",$weekend,"holidays":["2014-07-14"],""" +
        """"workingWeekendDays":["+999999999-12-31"]}}""")

    // the first year the document declares: it must be a year a calendar can cover, and it cannot
    // be later than the earliest date the document names, since a range cannot begin after the
    // dates inside it
    rejects(s"""{"Immutable":{"id":"Test1",$weekend,"startYear":1000000,"holidays":["2014-07-14"]}}""")
    rejects(s"""{"Immutable":{"id":"Test1",$weekend,"startYear":-1,"holidays":["2014-07-14"]}}""")
    rejects(s"""{"Immutable":{"id":"Test1",$weekend,"startYear":2015,"holidays":["2014-07-14"]}}""")
    rejects(s"""{"Immutable":{"id":"Test1",$weekend,"startYear":"soon","holidays":["2014-07-14"]}}""")
    rejects(
      s"""{"Immutable":{"id":"Test1",$weekend,"startYear":2015,"holidays":["2015-07-14"],""" +
        """"workingWeekendDays":["2014-07-12"]}}""")

    // and what is accepted: the year of the earliest holiday, an earlier year - which is
    // informational and does not widen the range, the range of a decoded calendar following from
    // its holidays as it does for every calendar this library builds - no year at all, and any
    // supported year where the document names no date to compare with
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
    // A calendar whose range begins before every holiday it reports, and what its document can
    // and cannot carry. This calendar covers 2013 and 2014: its range begins at a holiday that
    // fell on a Saturday, which the stored months cannot tell from the weekend and which it
    // therefore no longer reports, and that same Saturday is declared a working day - so the
    // calendar's earliest '''reported''' holiday is in 2014 while its range begins in 2013.
    val weekend = List(SATURDAY, SUNDAY)
    val sat2013 = date(2013, 7, 13)
    val test = ImmutableHolidayCalendar.of(TEST_ID, List(sat2013, MON_2014_07_14), weekend, List(sat2013))
    test.startYear shouldBe 2013
    test.endYearExclusive shouldBe 2015
    test.holidays.toList shouldBe List(MON_2014_07_14)
    test.workingDays.toList shouldBe List(sat2013)
    test.isBusinessDay(sat2013) shouldBe true

    // Its document records where its range began, and the year recorded is earlier than every
    // holiday the document lists.
    val json = test.asJson
    json shouldBe parse(
      """{"Immutable":{"id":"Test1","weekendDays":["SATURDAY","SUNDAY"],"startYear":2013,""" +
        """"holidays":["2014-07-14"],"workingWeekendDays":["2013-07-13"]}}"""
    ).getOrElse(fail("the expected JSON of this test is not valid JSON"))

    // Read back, the range follows from the holidays the document lists - 2014 alone - because a
    // document is decoded through the same normalising factory a caller builds with, as AAP
    // section 0.6.4 requires. The first year the document declares is informational, so the 2013
    // override names a year the rebuilt calendar holds no data for and is ignored. That is the
    // documented consequence of routing the decoder through that factory, and it is the same rule
    // that makes construction ignore a working day outside the range its holidays span - see
    // `test_of_unsupportedYears`. The alternative, letting the declared year or the override
    // widen the range, would let a document reach a calendar no factory of this library can
    // build and decide there that a weekend date is a business day.
    val roundTripped = decode[ImmutableHolidayCalendar](json.noSpaces)
      .getOrElse(fail(s"the calendar did not survive the round trip: ${json.noSpaces}"))
    roundTripped.startYear shouldBe 2014
    roundTripped.endYearExclusive shouldBe 2015
    roundTripped.holidays.toList shouldBe List(MON_2014_07_14)
    roundTripped.workingDays shouldBe empty
    roundTripped.isBusinessDay(sat2013) shouldBe false
    roundTripped.isHoliday(sat2013) shouldBe true

    // The two calendars therefore answer alike on every day of the years the document's holidays
    // span, and differ in 2013 at exactly one date: the Saturday the original overrode and the
    // rebuilt calendar holds no data for, where its weekend alone applies. Asserted day by day
    // because equality cannot make either statement - it compares identifiers alone.
    datesFrom(date(2014, 1, 1), date(2015, 1, 1)).foreach { current =>
      withClue(s"$current: ")(roundTripped.isHoliday(current) shouldBe test.isHoliday(current))
    }
    datesFrom(date(2013, 1, 1), date(2014, 1, 1)).foreach { current =>
      withClue(s"$current: ") {
        roundTripped.isHoliday(current) shouldBe (current == sat2013 || test.isHoliday(current))
      }
    }

    // A second shape of the same case, with no holiday left to report at all: the only date the
    // calendar was built from fell at its weekend, so the document lists no holiday. Decoded
    // through the factory, a calendar with no holidays holds no months at all and applies its
    // weekend alone - which is what the original has to say about the year it covers too, that
    // Saturday having left no mark on its stored months.
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

    // The declared year cannot be used to ask for an unbounded range - it does not size anything
    // - and it is still checked against the years a calendar may cover before anything is built.
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
    // The control on the range semantics of a decoded calendar: a document whose only holiday is
    // in 2020 and which declares a Saturday of 2021 to be a business day. The range follows from
    // the holiday, so it covers 2020 alone, the 2021 override names a year the calendar holds no
    // data for and is ignored, and that Saturday is still a weekend holiday. A decoder that let
    // the override - or the declared first year - widen the range would make it a business day
    // instead, in a calendar no public factory of this library can produce, and that calendar
    // would go on to answer for date adjustments, schedules, index observations and `Bus/252`.
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

    // ... which is exactly what the factory does with the same dates, that being the point: the
    // document is held to the semantics of construction rather than to any of its own.
    val built = ImmutableHolidayCalendar.of(TEST_ID, List(fri2020Christmas), weekend, List(sat2021))
    built.startYear shouldBe outside.startYear
    built.endYearExclusive shouldBe outside.endYearExclusive
    built.holidays.toList shouldBe outside.holidays.toList
    built.workingDays shouldBe outside.workingDays
    built.isHoliday(sat2021) shouldBe outside.isHoliday(sat2021)

    // The positive control: a working day '''inside''' the range the holidays span still takes
    // effect, so the field has not been neutered along with the widening it used to cause.
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
    // The two bulk accessors and the pair they are read from recover their dates from the stored
    // months, so what they answer with has to be what the calendar itself says about every day of
    // the years it covers - a holiday it holds is a date it calls a holiday and does not call a
    // weekend, and a working day is a date it calls a business day and does call a weekend. That
    // is the definition, and it is asserted here day by day rather than against a list of expected
    // dates, so the recovery cannot agree with a list while disagreeing with the calendar.
    //
    // It is worth asserting over a range of some size because the recovery walks the months and
    // the days within them: this calendar covers four years, one holiday of which falls at the
    // weekend - so it is not reported - and two weekend days of which are declared working days.
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

    // The dates the calendar was built from, read back. New Year 2012 fell on a Sunday, so it is
    // absent: a date the weekend already closes leaves no mark on the stored months and is not a
    // holiday the calendar reports. The two Saturdays supplied as both holidays and working days
    // are working days, the overrides being applied last.
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

    // Both sets from one reading agree with the two accessors, which is what lets a caller that
    // needs both - writing a calendar out, or merging two whose years do not meet - read once.
    val (holidays, workingDays) = test.holidaysAndWorkingDays
    holidays shouldBe test.holidays
    workingDays shouldBe test.workingDays

    // Reading is repeatable and does not consume anything: the second reading is the first.
    test.holidays shouldBe test.holidays
    test.workingDays shouldBe test.workingDays
    test.holidaysAndWorkingDays._1 shouldBe holidays

    // A weekend that is not Saturday and Sunday is recovered the same way, which matters because
    // the recovery tests each day against the weekend of the calendar rather than against a fixed
    // pair of days. Here the Saturday holiday is reported, Saturday being an ordinary business day
    // for this calendar, while the Thursday holiday is not, its weekend having closed that day
    // already; the Friday, supplied as both a holiday and a working day, is a working day.
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

    // A calendar holding no months at all has nothing to recover, and answers with two empty sets
    // rather than failing to walk them.
    HOLCAL_SAT_SUN.holidays shouldBe empty
    HOLCAL_SAT_SUN.workingDays shouldBe empty
    HOLCAL_SAT_SUN.holidaysAndWorkingDays._1 shouldBe empty
    HOLCAL_SAT_SUN.holidaysAndWorkingDays._2 shouldBe empty
  }

  //-------------------------------------------------------------------------
  test("test_readOldJodaFormat") {
    // The Java method of this name read a calendar out of a classpath fixture written by the
    // previous serialization library and asserted its identifier. AAP section 0.2.2 places that
    // fixture and all wire compatibility with that library out of scope, and this class is not
    // one of the five Java test classes the migration is allowed to drop - so what is ported is
    // the exclusion itself, stated as the behaviour it implies: a document in the legacy shape is
    // refused, and the calendar that document described is reachable through the shape this port
    // publishes. Both documents are transcribed inline; the fixture is not read, and this spec
    // reads no resource of any kind.
    val weekendDays = """"weekendDays":["SATURDAY","SUNDAY"]"""
    val holidays = """"holidays":["1950-01-02","1950-01-03","1950-12-25","1951-01-01"]"""
    val expectedHolidays = List(date(1950, 1, 2), date(1950, 1, 3), date(1950, 12, 25), date(1951, 1, 1))

    // The legacy shape: the calendar's fields at the top level of the document, under the
    // bean-name key that library wrote its type into. Both documents below are well-formed JSON,
    // so the refusal is about the shape of the document and not about malformed text.
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

    // A calendar carrying its own dates is an object under the `Immutable` key, which the legacy
    // document does not have; the failure names the wrapper this port requires.
    decode[ImmutableHolidayCalendar](legacyDocument).isLeft shouldBe true
    refusalOf(legacyDocument) should include("Immutable")

    // ... and the same object with the bean-name key removed is refused just the same, so the
    // refusal is the missing wrapper rather than the one key this port does not know.
    decode[ImmutableHolidayCalendar](withoutBeanName).isLeft shouldBe true
    refusalOf(withoutBeanName) should include("Immutable")

    // The fact the Java case actually asserted - that calendar, obtained from a document - is
    // available in the shape this port publishes, with the dates it declared.
    val ported = decode[ImmutableHolidayCalendar](s"""{"Immutable":{"id":"NZAU",$weekendDays,$holidays}}""")
      .getOrElse(fail("the calendar in this port's own form did not decode"))
    ported.id shouldBe HolidayCalendarId.of("NZAU")
    ported.holidays.toList shouldBe expectedHolidays
  }
}
