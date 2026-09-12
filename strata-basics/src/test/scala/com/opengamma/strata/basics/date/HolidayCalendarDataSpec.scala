/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.DayOfWeek.SATURDAY
import java.time.DayOfWeek.SUNDAY
import java.time.LocalDate
import java.time.MonthDay

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/**
 * Test [[HolidayCalendarData]], the transcription of the one holiday calendar the library being
 * ported held as configuration rather than as generated rules.
 *
 * There is no Java test class to port here, and so no row in
 * `manifest/java-test-mapping.csv` for this spec: the Java implementation read the `[THBA]`
 * section of `HolidayCalendarData.ini` off the class path at class-initialisation time, and the
 * Java tests that touched it - `HolidayCalendarIniLookupTest` - tested the '''parser''', which
 * this port does not have and whose rows the migration manifest records as dropped for that
 * reason. The tests below are therefore named descriptively rather than after Java methods.
 *
 * What replaces the parser test is a test of the '''data''', because in this port the rows are
 * code: a mistyped or dropped date would be invisible, where in the Java implementation it would
 * at worst have been a bad resource. The authority is the published section itself
 * (`modules/basics/src/main/resources/META-INF/com/opengamma/strata/config/base/HolidayCalendarData.ini:32-106`),
 * and what is asserted here is everything about the transcription that can be stated without
 * repeating all 1,220 dates: the shape of the table, three rows plus the leap-day row
 * transcribed verbatim, and the invariants the published section has.
 *
 * Two aspects of the published data are deliberately preserved rather than tidied up, and both
 * are asserted below because either would be easy to "fix" by mistake:
 *
 *   - the rows are '''not''' filtered against the weekend, unlike the rule-generated calendars,
 *     so the one published holiday that falls on a Sunday - 2031-05-04 - stays in the table;
 *   - the dates keep their published order, which is ascending, so [[HolidayCalendarData
 *     .thbaHolidays]] is ascending overall and no sort is applied on the way out.
 *
 * @see [[StandardHolidayCalendars]] for the calendar this table is assembled into
 * @see [[ImmutableHolidayCalendarSpec]] for the calendar value's own behaviour
 */
class HolidayCalendarDataSpec extends AnyFunSuite with Matchers {

  /** The first published year of the Thai bank calendar. */
  private val FirstYear: Int = 2005

  /** The last published year of the Thai bank calendar. */
  private val LastYear: Int = 2079

  /**
   * Creates a month-day pair, in the compact form the published rows use.
   *
   * @param month  the month-of-year, from 1 (January) to 12 (December)
   * @param day  the day-of-month
   * @return the month-day
   */
  private def md(month: Int, day: Int): MonthDay = MonthDay.of(month, day)

  //-------------------------------------------------------------------------
  test("test_thba_hasOneRowForEveryPublishedYear") {
    // 75 rows, one for every year from 2005 to 2079 inclusive, with no year missing and none
    // beyond the published range. The map is sorted by year, so iterating it yields the rows in
    // the order the section publishes them.
    HolidayCalendarData.thba should have size 75
    HolidayCalendarData.thba.keySet shouldBe (FirstYear to LastYear).toSet
    HolidayCalendarData.thba.keys.toList shouldBe (FirstYear to LastYear).toList
    LastYear - FirstYear + 1 shouldBe 75

    // Nothing outside the published range is present, which is what makes the calendar built
    // from this table fall back to its weekend rule outside those years.
    HolidayCalendarData.thba.get(FirstYear - 1) shouldBe None
    HolidayCalendarData.thba.get(LastYear + 1) shouldBe None
  }

  test("test_thba_everyRowIsNonEmptyAscendingAndFreeOfDuplicates") {
    HolidayCalendarData.thba.foreach {
      case (year, monthDays) =>
        withClue(s"$year: ") {
          monthDays should not be empty
          monthDays.distinct.size shouldBe monthDays.size
          // strictly ascending within the row, which is the order the section publishes and the
          // reason the resolved dates need no sorting
          monthDays.sliding(2).foreach {
            case List(earlier, later) => earlier.isBefore(later) shouldBe true
            case _ => succeed
          }
        }
    }

    // The shortest published year holds 13 dates and the longest 19, so a row that lost or
    // gained a date en bloc would be caught even if the total happened to be preserved.
    val rowSizes = HolidayCalendarData.thba.values.map(monthDays => monthDays.size).toList
    rowSizes.min shouldBe 13
    rowSizes.max shouldBe 19
    rowSizes.sum shouldBe 1220
  }

  test("test_thba_holdsTheOneThousandTwoHundredAndTwentyPublishedDates") {
    HolidayCalendarData.thbaHolidays should have size 1220
    HolidayCalendarData.thbaHolidays.distinct should have size 1220
    HolidayCalendarData.thbaHolidays.head shouldBe LocalDate.of(2005, 1, 3)
    HolidayCalendarData.thbaHolidays.last shouldBe LocalDate.of(2079, 12, 11)

    // Strictly ascending overall, which follows from the map being sorted by year and every row
    // being ascending - and is asserted directly, because it is the property a consumer relies
    // on when it builds a calendar from the list without sorting it.
    HolidayCalendarData.thbaHolidays.sliding(2).foreach {
      case List(earlier, later) => withClue(s"$earlier then $later: ")(earlier.isBefore(later) shouldBe true)
      case _ => succeed
    }

    // Consistent with the table it is derived from: the same number of dates, each one the
    // month-day of its row resolved against the year that row is filed under, in that order.
    val expected: List[LocalDate] =
      HolidayCalendarData.thba.toList.flatMap {
        case (year, monthDays) => monthDays.map(monthDay => monthDay.atYear(year))
      }
    HolidayCalendarData.thbaHolidays shouldBe expected
    HolidayCalendarData.thbaHolidays.map(date => date.getYear).distinct shouldBe
      (FirstYear to LastYear).toList

    // The accessor is a method over a value computed once, so asking twice gives the same list.
    HolidayCalendarData.thbaHolidays shouldBe HolidayCalendarData.thbaHolidays
  }

  test("test_thba_transcribedRowsMatchThePublishedSection") {
    // Three rows transcribed from the published section character for character - the first, the
    // last, and the row holding the weekend holiday - so that a row-level error at either end of
    // the table is caught. The remaining rows are covered by the shape and invariant assertions.
    HolidayCalendarData.thba(2005) shouldBe
      List(
        md(1, 3),
        md(2, 23),
        md(4, 13),
        md(4, 14),
        md(4, 15),
        md(5, 2),
        md(5, 5),
        md(5, 23),
        md(7, 1),
        md(7, 22),
        md(8, 12),
        md(10, 24),
        md(12, 5),
        md(12, 12))

    HolidayCalendarData.thba(2031) shouldBe
      List(
        md(1, 1),
        md(3, 7),
        md(4, 7),
        md(4, 14),
        md(4, 15),
        md(5, 1),
        md(5, 4),
        md(6, 3),
        md(7, 28),
        md(8, 4),
        md(8, 12),
        md(10, 13),
        md(10, 23),
        md(12, 5),
        md(12, 10),
        md(12, 31))

    HolidayCalendarData.thba(2079) shouldBe
      List(
        md(1, 2),
        md(2, 16),
        md(4, 6),
        md(4, 13),
        md(4, 14),
        md(5, 1),
        md(5, 15),
        md(6, 5),
        md(7, 13),
        md(7, 28),
        md(8, 14),
        md(10, 13),
        md(10, 23),
        md(12, 5),
        md(12, 11))
  }

  test("test_thba_everyMonthDayIsValidForTheYearItIsFiledUnder") {
    // A month-day is resolved against its year on the way out, and that resolution '''adjusts'''
    // the 29th of February in a year that is not a leap year rather than failing. One published
    // row holds a leap day - 2056 - so this assertion is what distinguishes correct data from a
    // leap day filed under the wrong year, which would otherwise become the 28th in silence.
    HolidayCalendarData.thba.foreach {
      case (year, monthDays) =>
        monthDays.foreach { monthDay =>
          val resolved = monthDay.atYear(year)
          withClue(s"$year-$monthDay resolved to $resolved: ") {
            resolved.getYear shouldBe year
            resolved.getMonthValue shouldBe monthDay.getMonthValue
            resolved.getDayOfMonth shouldBe monthDay.getDayOfMonth
          }
        }
    }

    // The one leap day the section publishes, asserted concretely.
    HolidayCalendarData.thba(2056) should contain(md(2, 29))
    HolidayCalendarData.thbaHolidays should contain(LocalDate.of(2056, 2, 29))
  }

  test("test_thba_keepsTheOnePublishedHolidayThatFallsOnAWeekend") {
    // The published rows are not filtered against the weekend, unlike the rule-generated
    // calendars, and exactly one row exercises that: the 4th of May 2031 is a Sunday and stays in
    // the table. Asserted as an exhaustive scan rather than as a single date, so that filtering
    // the table - or adding a second such date - would fail here.
    val weekendHolidays: List[LocalDate] =
      HolidayCalendarData.thbaHolidays.filter(date =>
        HolidayCalendarData.thbaWeekendDays.contains(date.getDayOfWeek))

    weekendHolidays shouldBe List(LocalDate.of(2031, 5, 4))
    LocalDate.of(2031, 5, 4).getDayOfWeek shouldBe SUNDAY
  }

  test("test_thbaWeekendDays") {
    // The section declares `Weekend = Sat,Sun`, and the weekend applies to every year, including
    // those outside the range the published rows cover.
    HolidayCalendarData.thbaWeekendDays shouldBe Set(SATURDAY, SUNDAY)
    HolidayCalendarData.thbaWeekendDays should have size 2
  }

  test("test_thba_isTheDataTheBuiltInCalendarIsAssembledFrom") {
    // The purpose of this table: it is the whole content of the built-in Thai bank calendar, so
    // every published date is a holiday of that calendar and the weekend it declares is the one
    // the calendar applies. `StandardHolidayCalendars` is the assembler - this object never
    // builds a calendar itself, which is what keeps the two free of an initialisation cycle.
    val calendar: ImmutableHolidayCalendar = StandardHolidayCalendars.THBA
    calendar.id shouldBe HolidayCalendarIds.THBA
    calendar.weekendDays shouldBe HolidayCalendarData.thbaWeekendDays

    HolidayCalendarData.thbaHolidays.foreach { date =>
      withClue(s"$date: ")(calendar.isHoliday(date) shouldBe true)
    }

    // And a year outside the published range is answered by the weekend rule alone, which is the
    // documented fallback of a calendar asked about a date its data does not cover.
    calendar.isHoliday(LocalDate.of(2004, 1, 1)) shouldBe false
    calendar.isBusinessDay(LocalDate.of(2004, 1, 1)) shouldBe true
    calendar.isHoliday(LocalDate.of(2080, 12, 31)) shouldBe false
    calendar.isHoliday(LocalDate.of(2080, 12, 28)) shouldBe true
  }
}
