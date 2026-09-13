/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.DayOfWeek.FRIDAY
import java.time.DayOfWeek.SATURDAY
import java.time.DayOfWeek.SUNDAY
import java.time.DayOfWeek.THURSDAY
import java.time.LocalDate

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableFor3

import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper._

/**
 * Test [[HolidayCalendar]].
 *
 * [[HolidayCalendar]] answers one question - whether a date is a holiday - and derives a dozen
 * more from it. This spec owns that derivation, the four built-in calendars whose content
 * follows from their names - each swept over the 1,491 days from 2011-01-01 to 2015-01-30 - and
 * composition: [[HolidayCalendar.combinedWith]] is a holiday where '''either''' part is closed
 * and [[HolidayCalendar.linkedWith]] only where '''both''' are, each with its own identities.
 *
 * Neighbouring specs own the rest: a calendar built from a list of dates belongs to
 * `ImmutableHolidayCalendarSpec`, the constants as a holder and the defaulting decoration to
 * `HolidayCalendarsSpec`, the national calendars - generated and data-backed `THBA` - to
 * `GlobalHolidayCalendarsSpec` and `parity/HolidayCalendarParitySpec`, and the closed-family
 * sweeps to `NamedEnumClosedSpec` and `ApiSurfaceSpec`.
 */
final class HolidayCalendarSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  // Named for their day of the week; July 2014 begins on a Tuesday.
  private val MON_2014_07_07: LocalDate = date(2014, 7, 7)
  private val WED_2014_07_09: LocalDate = date(2014, 7, 9)
  private val THU_2014_07_10: LocalDate = date(2014, 7, 10)
  private val FRI_2014_07_11: LocalDate = date(2014, 7, 11)
  private val SAT_2014_07_12: LocalDate = date(2014, 7, 12)
  private val SUN_2014_07_13: LocalDate = date(2014, 7, 13)
  private val MON_2014_07_14: LocalDate = date(2014, 7, 14)
  private val TUE_2014_07_15: LocalDate = date(2014, 7, 15)
  private val WED_2014_07_16: LocalDate = date(2014, 7, 16)
  private val THU_2014_07_17: LocalDate = date(2014, 7, 17)
  private val FRI_2014_07_18: LocalDate = date(2014, 7, 18)
  private val SAT_2014_07_19: LocalDate = date(2014, 7, 19)
  private val SUN_2014_07_20: LocalDate = date(2014, 7, 20)
  private val MON_2014_07_21: LocalDate = date(2014, 7, 21)
  private val TUE_2014_07_22: LocalDate = date(2014, 7, 22)
  private val WED_2014_07_23: LocalDate = date(2014, 7, 23)

  private val WED_2014_07_30: LocalDate = date(2014, 7, 30)
  private val THU_2014_07_31: LocalDate = date(2014, 7, 31)

  /** The first day of the sweep each of the four built-in calendars is put through. */
  private val SWEEP_START: LocalDate = date(2011, 1, 1)

  /** The day after the last day of that sweep, which therefore ends on 2015-01-30. */
  private val SWEEP_END_EXCLUSIVE: LocalDate = date(2015, 1, 31)

  /** A value of a type that is not a holiday calendar at all. */
  private val ANOTHER_TYPE: Any = ""

  /** The null reference, held as a value so the assertions read as equality-contract calls. */
  private val NULL_REFERENCE: Any = null

  //-------------------------------------------------------------------------
  // The two mock calendars the tables below are read against. The family is sealed, so no
  // implementation can be declared here; each is built as an `ImmutableHolidayCalendar` holding
  // the dates its rule makes holidays over 2012 to 2016. Outside the years its holidays span an
  // `ImmutableHolidayCalendar` applies its weekend alone, so each rule holds only inside that
  // range: every date these two are asked about lies in 2014 and the furthest any test steps is
  // a business day into the neighbouring month, about two years of margin at each end. The
  // identifiers `Mock` and `MockEom` are load-bearing too - `test_combinedWith` and
  // `test_linkedWith` assert a composite's name, which is composed from its parts' names.

  private val MOCK_FIRST_YEAR: Int = 2012

  private val MOCK_END_YEAR_EXCLUSIVE: Int = 2017

  private val MOCK_YEARS: List[Int] = (MOCK_FIRST_YEAR until MOCK_END_YEAR_EXCLUSIVE).toList

  /** The days of the month the first mock calendar treats as holidays, in every month. */
  private val MOCK_HOLIDAY_DAYS_OF_MONTH: List[Int] = List(16, 18, 31)

  /**
   * A holiday is the 16th, the 18th or the 31st of any month, or a Saturday or a Sunday. In July
   * 2014 - the month nearly every table below works in - the business days are the 1st to 4th,
   * 7th to 11th, 14th, 15th, 17th, 21st to 25th and 28th to 30th, which is why the expectations
   * skip from the 15th to the 17th and from the 17th to the 21st.
   */
  private val MOCK: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(
      HolidayCalendarId.of("Mock"),
      for {
        year <- MOCK_YEARS
        month <- (1 to 12).toList
        dayOfMonth <- MOCK_HOLIDAY_DAYS_OF_MONTH
        // the 31st exists in seven months of the year only
        if dayOfMonth <= LocalDate.of(year, month, 1).lengthOfMonth
      } yield LocalDate.of(year, month, dayOfMonth),
      Set(SATURDAY, SUNDAY))

  /**
   * A holiday is the 30th of June, the 31st of July, or a Saturday or a Sunday, so the month's
   * last day falls each of the four ways [[data_lastBusinessDayOfMonth]] covers.
   */
  private val MOCK_EOM: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(
      HolidayCalendarId.of("MockEom"),
      MOCK_YEARS.flatMap(year => List(LocalDate.of(year, 6, 30), LocalDate.of(year, 7, 31))),
      Set(SATURDAY, SUNDAY))

  //-------------------------------------------------------------------------
  /** Business-day shifts forward, none and backward, read by `test_shift` and `test_adjustBy`. */
  private val data_shift: TableFor3[LocalDate, Int, LocalDate] = Table(
    ("date", "amount", "expected"),
    (THU_2014_07_10, 1, FRI_2014_07_11),
    (FRI_2014_07_11, 1, MON_2014_07_14),
    (SAT_2014_07_12, 1, MON_2014_07_14),
    (SUN_2014_07_13, 1, MON_2014_07_14),
    (MON_2014_07_14, 1, TUE_2014_07_15),
    (TUE_2014_07_15, 1, THU_2014_07_17),
    (WED_2014_07_16, 1, THU_2014_07_17),
    (THU_2014_07_17, 1, MON_2014_07_21),
    (FRI_2014_07_18, 1, MON_2014_07_21),
    (SAT_2014_07_19, 1, MON_2014_07_21),
    (SUN_2014_07_20, 1, MON_2014_07_21),
    (MON_2014_07_21, 1, TUE_2014_07_22),
    (THU_2014_07_10, 2, MON_2014_07_14),
    (FRI_2014_07_11, 2, TUE_2014_07_15),
    (SAT_2014_07_12, 2, TUE_2014_07_15),
    (SUN_2014_07_13, 2, TUE_2014_07_15),
    (MON_2014_07_14, 2, THU_2014_07_17),
    (TUE_2014_07_15, 2, MON_2014_07_21),
    (WED_2014_07_16, 2, MON_2014_07_21),
    (THU_2014_07_17, 2, TUE_2014_07_22),
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
    (TUE_2014_07_15, -1, MON_2014_07_14),
    (WED_2014_07_16, -1, TUE_2014_07_15),
    (THU_2014_07_17, -1, TUE_2014_07_15),
    (FRI_2014_07_18, -1, THU_2014_07_17),
    (SAT_2014_07_19, -1, THU_2014_07_17),
    (SUN_2014_07_20, -1, THU_2014_07_17),
    (MON_2014_07_21, -1, THU_2014_07_17),
    (TUE_2014_07_22, -1, MON_2014_07_21),
    (FRI_2014_07_11, -2, WED_2014_07_09),
    (SAT_2014_07_12, -2, THU_2014_07_10),
    (SUN_2014_07_13, -2, THU_2014_07_10),
    (MON_2014_07_14, -2, THU_2014_07_10),
    (TUE_2014_07_15, -2, FRI_2014_07_11),
    (WED_2014_07_16, -2, MON_2014_07_14),
    (THU_2014_07_17, -2, MON_2014_07_14),
    (FRI_2014_07_18, -2, TUE_2014_07_15),
    (SAT_2014_07_19, -2, TUE_2014_07_15),
    (SUN_2014_07_20, -2, TUE_2014_07_15),
    (MON_2014_07_21, -2, TUE_2014_07_15),
    (TUE_2014_07_22, -2, THU_2014_07_17)
  )

  private val data_next: TableFor2[LocalDate, LocalDate] = Table(
    ("date", "expectedNext"),
    (THU_2014_07_10, FRI_2014_07_11),
    (FRI_2014_07_11, MON_2014_07_14),
    (SAT_2014_07_12, MON_2014_07_14),
    (SUN_2014_07_13, MON_2014_07_14),
    (MON_2014_07_14, TUE_2014_07_15),
    (TUE_2014_07_15, THU_2014_07_17),
    (WED_2014_07_16, THU_2014_07_17),
    (THU_2014_07_17, MON_2014_07_21),
    (FRI_2014_07_18, MON_2014_07_21),
    (SAT_2014_07_19, MON_2014_07_21),
    (SUN_2014_07_20, MON_2014_07_21),
    (MON_2014_07_21, TUE_2014_07_22)
  )

  private val data_nextOrSame: TableFor2[LocalDate, LocalDate] = Table(
    ("date", "expectedNext"),
    (THU_2014_07_10, THU_2014_07_10),
    (FRI_2014_07_11, FRI_2014_07_11),
    (SAT_2014_07_12, MON_2014_07_14),
    (SUN_2014_07_13, MON_2014_07_14),
    (MON_2014_07_14, MON_2014_07_14),
    (TUE_2014_07_15, TUE_2014_07_15),
    (WED_2014_07_16, THU_2014_07_17),
    (THU_2014_07_17, THU_2014_07_17),
    (FRI_2014_07_18, MON_2014_07_21),
    (SAT_2014_07_19, MON_2014_07_21),
    (SUN_2014_07_20, MON_2014_07_21),
    (MON_2014_07_21, MON_2014_07_21)
  )

  private val data_previous: TableFor2[LocalDate, LocalDate] = Table(
    ("date", "expectedPrevious"),
    (FRI_2014_07_11, THU_2014_07_10),
    (SAT_2014_07_12, FRI_2014_07_11),
    (SUN_2014_07_13, FRI_2014_07_11),
    (MON_2014_07_14, FRI_2014_07_11),
    (TUE_2014_07_15, MON_2014_07_14),
    (WED_2014_07_16, TUE_2014_07_15),
    (THU_2014_07_17, TUE_2014_07_15),
    (FRI_2014_07_18, THU_2014_07_17),
    (SAT_2014_07_19, THU_2014_07_17),
    (SUN_2014_07_20, THU_2014_07_17),
    (MON_2014_07_21, THU_2014_07_17),
    (TUE_2014_07_22, MON_2014_07_21)
  )

  private val data_previousOrSame: TableFor2[LocalDate, LocalDate] = Table(
    ("date", "expectedPrevious"),
    (FRI_2014_07_11, FRI_2014_07_11),
    (SAT_2014_07_12, FRI_2014_07_11),
    (SUN_2014_07_13, FRI_2014_07_11),
    (MON_2014_07_14, MON_2014_07_14),
    (TUE_2014_07_15, TUE_2014_07_15),
    (WED_2014_07_16, TUE_2014_07_15),
    (THU_2014_07_17, THU_2014_07_17),
    (FRI_2014_07_18, THU_2014_07_17),
    (SAT_2014_07_19, THU_2014_07_17),
    (SUN_2014_07_20, THU_2014_07_17),
    (MON_2014_07_21, MON_2014_07_21),
    (TUE_2014_07_22, TUE_2014_07_22)
  )

  /**
   * The final row distinguishes this method from `nextOrSame`: the 31st of July is a holiday
   * here and the next business day falls in August, so the answer runs '''backwards'''.
   */
  private val data_nextSameOrLastInMonth: TableFor2[LocalDate, LocalDate] = Table(
    ("date", "expectedNext"),
    (THU_2014_07_10, THU_2014_07_10),
    (FRI_2014_07_11, FRI_2014_07_11),
    (SAT_2014_07_12, MON_2014_07_14),
    (SUN_2014_07_13, MON_2014_07_14),
    (MON_2014_07_14, MON_2014_07_14),
    (TUE_2014_07_15, TUE_2014_07_15),
    (WED_2014_07_16, THU_2014_07_17),
    (THU_2014_07_17, THU_2014_07_17),
    (FRI_2014_07_18, MON_2014_07_21),
    (SAT_2014_07_19, MON_2014_07_21),
    (SUN_2014_07_20, MON_2014_07_21),
    (MON_2014_07_21, MON_2014_07_21),
    (THU_2014_07_31, WED_2014_07_30)
  )

  /** Read against [[MOCK_EOM]], by `test_lastBusinessDayOfMonth` and `test_isLastBusinessDayOfMonth`. */
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

  private val data_daysBetween: TableFor3[LocalDate, LocalDate, Int] = Table(
    ("start", "end", "expected"),
    (FRI_2014_07_11, FRI_2014_07_11, 0),
    (FRI_2014_07_11, SAT_2014_07_12, 1),
    (FRI_2014_07_11, SUN_2014_07_13, 1),
    (FRI_2014_07_11, MON_2014_07_14, 1),
    (FRI_2014_07_11, TUE_2014_07_15, 2),
    (FRI_2014_07_11, WED_2014_07_16, 3),
    (FRI_2014_07_11, THU_2014_07_17, 3),
    (FRI_2014_07_11, FRI_2014_07_18, 4),
    (FRI_2014_07_11, SAT_2014_07_19, 4),
    (FRI_2014_07_11, SUN_2014_07_20, 4),
    (FRI_2014_07_11, MON_2014_07_21, 4),
    (FRI_2014_07_11, TUE_2014_07_22, 5)
  )

  private val data_businessDays: TableFor3[LocalDate, LocalDate, List[LocalDate]] = Table(
    ("start", "end", "expected"),
    (FRI_2014_07_11, FRI_2014_07_11, Nil),
    (FRI_2014_07_11, SAT_2014_07_12, List(FRI_2014_07_11)),
    (FRI_2014_07_11, SUN_2014_07_13, List(FRI_2014_07_11)),
    (FRI_2014_07_11, MON_2014_07_14, List(FRI_2014_07_11)),
    (FRI_2014_07_11, TUE_2014_07_15, List(FRI_2014_07_11, MON_2014_07_14)),
    (FRI_2014_07_11, WED_2014_07_16, List(FRI_2014_07_11, MON_2014_07_14, TUE_2014_07_15)),
    (FRI_2014_07_11, THU_2014_07_17, List(FRI_2014_07_11, MON_2014_07_14, TUE_2014_07_15)),
    (
      FRI_2014_07_11,
      FRI_2014_07_18,
      List(FRI_2014_07_11, MON_2014_07_14, TUE_2014_07_15, THU_2014_07_17)),
    (
      FRI_2014_07_11,
      SAT_2014_07_19,
      List(FRI_2014_07_11, MON_2014_07_14, TUE_2014_07_15, THU_2014_07_17)),
    (
      FRI_2014_07_11,
      SUN_2014_07_20,
      List(FRI_2014_07_11, MON_2014_07_14, TUE_2014_07_15, THU_2014_07_17)),
    (
      FRI_2014_07_11,
      MON_2014_07_21,
      List(FRI_2014_07_11, MON_2014_07_14, TUE_2014_07_15, THU_2014_07_17)),
    (
      FRI_2014_07_11,
      TUE_2014_07_22,
      List(FRI_2014_07_11, MON_2014_07_14, TUE_2014_07_15, THU_2014_07_17, MON_2014_07_21))
  )

  private val data_holidays: TableFor3[LocalDate, LocalDate, List[LocalDate]] = Table(
    ("start", "end", "expected"),
    (FRI_2014_07_11, FRI_2014_07_11, Nil),
    (FRI_2014_07_11, SAT_2014_07_12, Nil),
    (FRI_2014_07_11, SUN_2014_07_13, List(SAT_2014_07_12)),
    (FRI_2014_07_11, MON_2014_07_14, List(SAT_2014_07_12, SUN_2014_07_13)),
    (FRI_2014_07_11, TUE_2014_07_15, List(SAT_2014_07_12, SUN_2014_07_13)),
    (FRI_2014_07_11, WED_2014_07_16, List(SAT_2014_07_12, SUN_2014_07_13)),
    (FRI_2014_07_11, THU_2014_07_17, List(SAT_2014_07_12, SUN_2014_07_13, WED_2014_07_16)),
    (FRI_2014_07_11, FRI_2014_07_18, List(SAT_2014_07_12, SUN_2014_07_13, WED_2014_07_16)),
    (
      FRI_2014_07_11,
      SAT_2014_07_19,
      List(SAT_2014_07_12, SUN_2014_07_13, WED_2014_07_16, FRI_2014_07_18)),
    (
      FRI_2014_07_11,
      SUN_2014_07_20,
      List(SAT_2014_07_12, SUN_2014_07_13, WED_2014_07_16, FRI_2014_07_18, SAT_2014_07_19)),
    (
      FRI_2014_07_11,
      MON_2014_07_21,
      List(
        SAT_2014_07_12,
        SUN_2014_07_13,
        WED_2014_07_16,
        FRI_2014_07_18,
        SAT_2014_07_19,
        SUN_2014_07_20)),
    (
      FRI_2014_07_11,
      TUE_2014_07_22,
      List(
        SAT_2014_07_12,
        SUN_2014_07_13,
        WED_2014_07_16,
        FRI_2014_07_18,
        SAT_2014_07_19,
        SUN_2014_07_20))
  )

  //-------------------------------------------------------------------------
  /**
   * Sweeps a calendar over the 1,491 days from 2011-01-01 to 2015-01-30, asserting that it calls
   * exactly the expected days holidays and that `isBusinessDay` is the complement of `isHoliday`
   * on each. Those days are a sample of a date range, not every date.
   *
   * @param test  the calendar to sweep
   * @param isHolidayExpected  the days the calendar is expected to call holidays
   * @return the assertion that the sweep found no disagreement
   */
  private def assertSweep(test: HolidayCalendar, isHolidayExpected: LocalDate => Boolean): Assertion = {
    LocalDateUtils.dates(SWEEP_START, SWEEP_END_EXCLUSIVE).foreach { day =>
      withClue(s"$day: ") {
        test.isHoliday(day) shouldBe isHolidayExpected(day)
        test.isBusinessDay(day) shouldBe !isHolidayExpected(day)
      }
    }
    succeed
  }

  /**
   * Asserts a fixed set of `Sat/Sun` shifts. `test_SAT_SUN_shift` applies it to two calendars
   * that reach their answers by different routes - one applies its weekend directly, the other
   * has stored months to search first - and must agree.
   *
   * @param test  the calendar to shift against
   * @return the assertion that every shift landed where it should
   */
  private def assertSatSun(test: HolidayCalendar): Assertion = {
    test.shift(THU_2014_07_10, 2) shouldBe MON_2014_07_14
    test.shift(FRI_2014_07_11, 2) shouldBe TUE_2014_07_15
    test.shift(SUN_2014_07_13, 2) shouldBe TUE_2014_07_15
    test.shift(MON_2014_07_14, 2) shouldBe WED_2014_07_16

    test.shift(FRI_2014_07_11, -2) shouldBe WED_2014_07_09
    test.shift(SAT_2014_07_12, -2) shouldBe THU_2014_07_10
    test.shift(SUN_2014_07_13, -2) shouldBe THU_2014_07_10
    test.shift(MON_2014_07_14, -2) shouldBe THU_2014_07_10
    test.shift(TUE_2014_07_15, -2) shouldBe FRI_2014_07_11
    test.shift(WED_2014_07_16, -2) shouldBe MON_2014_07_14

    test.shift(FRI_2014_07_11, 5) shouldBe FRI_2014_07_18
    test.shift(FRI_2014_07_11, 6) shouldBe MON_2014_07_21

    test.shift(FRI_2014_07_18, -5) shouldBe FRI_2014_07_11
    test.shift(MON_2014_07_21, -6) shouldBe FRI_2014_07_11

    test.shift(SAT_2014_07_12, 5) shouldBe FRI_2014_07_18
    test.shift(SAT_2014_07_12, -5) shouldBe MON_2014_07_07
  }

  //-------------------------------------------------------------------------
  test("test_NO_HOLIDAYS") {
    val test = HolidayCalendars.NO_HOLIDAYS
    assertSweep(test, _ => false)
    test.name shouldBe "NoHolidays"
    test.toString shouldBe "HolidayCalendar[NoHolidays]"
  }

  test("test_NO_HOLIDAYS_of") {
    // `of` reports an unknown name as a failure, so a success carrying the constant is asserted
    HolidayCalendars.of("NoHolidays") should haveValue(HolidayCalendars.NO_HOLIDAYS)
  }

  test("test_NO_HOLIDAYS_shift") {
    HolidayCalendars.NO_HOLIDAYS.shift(FRI_2014_07_11, 2) shouldBe SUN_2014_07_13
    HolidayCalendars.NO_HOLIDAYS.shift(SUN_2014_07_13, -2) shouldBe FRI_2014_07_11
  }

  test("test_NO_HOLIDAYS_next") {
    HolidayCalendars.NO_HOLIDAYS.next(FRI_2014_07_11) shouldBe SAT_2014_07_12
    HolidayCalendars.NO_HOLIDAYS.next(SAT_2014_07_12) shouldBe SUN_2014_07_13
  }

  test("test_NO_HOLIDAYS_nextOrSame") {
    HolidayCalendars.NO_HOLIDAYS.nextOrSame(FRI_2014_07_11) shouldBe FRI_2014_07_11
    HolidayCalendars.NO_HOLIDAYS.nextOrSame(SAT_2014_07_12) shouldBe SAT_2014_07_12
  }

  test("test_NO_HOLIDAYS_previous") {
    HolidayCalendars.NO_HOLIDAYS.previous(SAT_2014_07_12) shouldBe FRI_2014_07_11
    HolidayCalendars.NO_HOLIDAYS.previous(SUN_2014_07_13) shouldBe SAT_2014_07_12
  }

  test("test_NO_HOLIDAYS_previousOrSame") {
    HolidayCalendars.NO_HOLIDAYS.previousOrSame(SAT_2014_07_12) shouldBe SAT_2014_07_12
    HolidayCalendars.NO_HOLIDAYS.previousOrSame(SUN_2014_07_13) shouldBe SUN_2014_07_13
  }

  test("test_NO_HOLIDAYS_nextSameOrLastInMonth") {
    HolidayCalendars.NO_HOLIDAYS.nextSameOrLastInMonth(FRI_2014_07_11) shouldBe FRI_2014_07_11
    HolidayCalendars.NO_HOLIDAYS.nextSameOrLastInMonth(SAT_2014_07_12) shouldBe SAT_2014_07_12
  }

  test("test_NO_HOLIDAYS_daysBetween_LocalDateLocalDate") {
    // every day counts, so this is the plain difference of the two dates
    HolidayCalendars.NO_HOLIDAYS.daysBetween(FRI_2014_07_11, MON_2014_07_14) shouldBe 3
  }

  test("test_NO_HOLIDAYS_combineWith") {
    // the calendar with no holidays is the identity of combination, and the result is that
    // calendar itself rather than a composite equal to it
    val base: HolidayCalendar = MOCK
    val test = HolidayCalendars.NO_HOLIDAYS.combinedWith(base)
    test should be theSameInstanceAs base
  }

  //-------------------------------------------------------------------------
  test("test_SAT_SUN") {
    val test = HolidayCalendars.SAT_SUN
    assertSweep(test, day => day.getDayOfWeek == SATURDAY || day.getDayOfWeek == SUNDAY)
    test.name shouldBe "Sat/Sun"
    test.toString shouldBe "HolidayCalendar[Sat/Sun]"
  }

  test("test_SAT_SUN_of") {
    HolidayCalendars.of("Sat/Sun") should haveValue(HolidayCalendars.SAT_SUN)
  }

  test("test_SAT_SUN_shift") {
    val equivalent =
      ImmutableHolidayCalendar.of(HolidayCalendarId.of("TEST-SAT-SUN"), Nil, Set(SATURDAY, SUNDAY))
    assertSatSun(equivalent)
    assertSatSun(HolidayCalendars.SAT_SUN)
  }

  test("test_SAT_SUN_next") {
    HolidayCalendars.SAT_SUN.next(THU_2014_07_10) shouldBe FRI_2014_07_11
    HolidayCalendars.SAT_SUN.next(FRI_2014_07_11) shouldBe MON_2014_07_14
    HolidayCalendars.SAT_SUN.next(SAT_2014_07_12) shouldBe MON_2014_07_14
    HolidayCalendars.SAT_SUN.next(SUN_2014_07_13) shouldBe MON_2014_07_14
  }

  test("test_SAT_SUN_previous") {
    HolidayCalendars.SAT_SUN.previous(SAT_2014_07_12) shouldBe FRI_2014_07_11
    HolidayCalendars.SAT_SUN.previous(SUN_2014_07_13) shouldBe FRI_2014_07_11
    HolidayCalendars.SAT_SUN.previous(MON_2014_07_14) shouldBe FRI_2014_07_11
    HolidayCalendars.SAT_SUN.previous(TUE_2014_07_15) shouldBe MON_2014_07_14
  }

  test("test_SAT_SUN_daysBetween_LocalDateLocalDate") {
    // Friday counts, the weekend does not, and Monday is excluded as the end of the range
    HolidayCalendars.SAT_SUN.daysBetween(FRI_2014_07_11, MON_2014_07_14) shouldBe 1
  }

  //-------------------------------------------------------------------------
  test("test_FRI_SAT") {
    val test = HolidayCalendars.FRI_SAT
    assertSweep(test, day => day.getDayOfWeek == FRIDAY || day.getDayOfWeek == SATURDAY)
    test.name shouldBe "Fri/Sat"
    test.toString shouldBe "HolidayCalendar[Fri/Sat]"
  }

  test("test_FRI_SAT_of") {
    HolidayCalendars.of("Fri/Sat") should haveValue(HolidayCalendars.FRI_SAT)
  }

  test("test_FRI_SAT_shift") {
    HolidayCalendars.FRI_SAT.shift(THU_2014_07_10, 2) shouldBe MON_2014_07_14
    HolidayCalendars.FRI_SAT.shift(FRI_2014_07_11, 2) shouldBe MON_2014_07_14
    HolidayCalendars.FRI_SAT.shift(SUN_2014_07_13, 2) shouldBe TUE_2014_07_15
    HolidayCalendars.FRI_SAT.shift(MON_2014_07_14, 2) shouldBe WED_2014_07_16

    HolidayCalendars.FRI_SAT.shift(FRI_2014_07_11, -2) shouldBe WED_2014_07_09
    HolidayCalendars.FRI_SAT.shift(SAT_2014_07_12, -2) shouldBe WED_2014_07_09
    HolidayCalendars.FRI_SAT.shift(SUN_2014_07_13, -2) shouldBe WED_2014_07_09
    HolidayCalendars.FRI_SAT.shift(MON_2014_07_14, -2) shouldBe THU_2014_07_10
    HolidayCalendars.FRI_SAT.shift(TUE_2014_07_15, -2) shouldBe SUN_2014_07_13
    HolidayCalendars.FRI_SAT.shift(WED_2014_07_16, -2) shouldBe MON_2014_07_14

    HolidayCalendars.FRI_SAT.shift(THU_2014_07_10, 5) shouldBe THU_2014_07_17
    HolidayCalendars.FRI_SAT.shift(THU_2014_07_10, 6) shouldBe SUN_2014_07_20

    HolidayCalendars.FRI_SAT.shift(THU_2014_07_17, -5) shouldBe THU_2014_07_10
    HolidayCalendars.FRI_SAT.shift(SUN_2014_07_20, -6) shouldBe THU_2014_07_10
  }

  test("test_FRI_SAT_next") {
    HolidayCalendars.FRI_SAT.next(WED_2014_07_09) shouldBe THU_2014_07_10
    HolidayCalendars.FRI_SAT.next(THU_2014_07_10) shouldBe SUN_2014_07_13
    HolidayCalendars.FRI_SAT.next(FRI_2014_07_11) shouldBe SUN_2014_07_13
    HolidayCalendars.FRI_SAT.next(SAT_2014_07_12) shouldBe SUN_2014_07_13
    HolidayCalendars.FRI_SAT.next(SUN_2014_07_13) shouldBe MON_2014_07_14
  }

  test("test_FRI_SAT_previous") {
    HolidayCalendars.FRI_SAT.previous(FRI_2014_07_11) shouldBe THU_2014_07_10
    HolidayCalendars.FRI_SAT.previous(SAT_2014_07_12) shouldBe THU_2014_07_10
    HolidayCalendars.FRI_SAT.previous(SUN_2014_07_13) shouldBe THU_2014_07_10
    HolidayCalendars.FRI_SAT.previous(MON_2014_07_14) shouldBe SUN_2014_07_13
  }

  test("test_FRI_SAT_daysBetween_LocalDateLocalDate") {
    // Friday and Saturday are holidays, so only Sunday counts
    HolidayCalendars.FRI_SAT.daysBetween(FRI_2014_07_11, MON_2014_07_14) shouldBe 1
  }

  //-------------------------------------------------------------------------
  test("test_THU_FRI") {
    val test = HolidayCalendars.THU_FRI
    assertSweep(test, day => day.getDayOfWeek == THURSDAY || day.getDayOfWeek == FRIDAY)
    test.name shouldBe "Thu/Fri"
    test.toString shouldBe "HolidayCalendar[Thu/Fri]"
  }

  test("test_THU_FRI_of") {
    HolidayCalendars.of("Thu/Fri") should haveValue(HolidayCalendars.THU_FRI)
  }

  test("test_THU_FRI_shift") {
    HolidayCalendars.THU_FRI.shift(WED_2014_07_09, 2) shouldBe SUN_2014_07_13
    HolidayCalendars.THU_FRI.shift(THU_2014_07_10, 2) shouldBe SUN_2014_07_13
    HolidayCalendars.THU_FRI.shift(FRI_2014_07_11, 2) shouldBe SUN_2014_07_13
    HolidayCalendars.THU_FRI.shift(SAT_2014_07_12, 2) shouldBe MON_2014_07_14

    HolidayCalendars.THU_FRI.shift(FRI_2014_07_18, -2) shouldBe TUE_2014_07_15
    HolidayCalendars.THU_FRI.shift(SAT_2014_07_19, -2) shouldBe TUE_2014_07_15
    HolidayCalendars.THU_FRI.shift(SUN_2014_07_20, -2) shouldBe WED_2014_07_16
    HolidayCalendars.THU_FRI.shift(MON_2014_07_21, -2) shouldBe SAT_2014_07_19

    HolidayCalendars.THU_FRI.shift(WED_2014_07_09, 5) shouldBe WED_2014_07_16
    HolidayCalendars.THU_FRI.shift(WED_2014_07_09, 6) shouldBe SAT_2014_07_19

    HolidayCalendars.THU_FRI.shift(WED_2014_07_16, -5) shouldBe WED_2014_07_09
    HolidayCalendars.THU_FRI.shift(SAT_2014_07_19, -6) shouldBe WED_2014_07_09
  }

  test("test_THU_FRI_next") {
    HolidayCalendars.THU_FRI.next(WED_2014_07_09) shouldBe SAT_2014_07_12
    HolidayCalendars.THU_FRI.next(THU_2014_07_10) shouldBe SAT_2014_07_12
    HolidayCalendars.THU_FRI.next(FRI_2014_07_11) shouldBe SAT_2014_07_12
    HolidayCalendars.THU_FRI.next(SAT_2014_07_12) shouldBe SUN_2014_07_13
  }

  test("test_THU_FRI_previous") {
    HolidayCalendars.THU_FRI.previous(THU_2014_07_10) shouldBe WED_2014_07_09
    HolidayCalendars.THU_FRI.previous(FRI_2014_07_11) shouldBe WED_2014_07_09
    HolidayCalendars.THU_FRI.previous(SAT_2014_07_12) shouldBe WED_2014_07_09
    HolidayCalendars.THU_FRI.previous(SUN_2014_07_13) shouldBe SAT_2014_07_12
  }

  test("test_THU_FRI_daysBetween_LocalDateLocalDate") {
    // Friday is a holiday, so Saturday and Sunday are the two business days of the range
    HolidayCalendars.THU_FRI.daysBetween(FRI_2014_07_11, MON_2014_07_14) shouldBe 2
  }

  //-------------------------------------------------------------------------
  test("test_of_combined") {
    // the parts of a combined name are sorted, so either order names the same calendar
    val test = HolidayCalendars.of("Thu/Fri+Fri/Sat")
    test.map(calendar => calendar.name) should haveValue("Fri/Sat+Thu/Fri")
    test.map(calendar => calendar.toString) should haveValue("HolidayCalendar[Fri/Sat+Thu/Fri]")

    val test2 = HolidayCalendars.of("Thu/Fri+Fri/Sat")
    test shouldBe test2

    // and it behaves as a combination: either part closes Thursday, Friday and Saturday
    test.map(calendar => calendar.isHoliday(THU_2014_07_10)) should haveValue(true)
    test.map(calendar => calendar.isHoliday(FRI_2014_07_11)) should haveValue(true)
    test.map(calendar => calendar.isHoliday(SAT_2014_07_12)) should haveValue(true)
    test.map(calendar => calendar.isHoliday(SUN_2014_07_13)) should haveValue(false)
    test.map(calendar => calendar.isHoliday(MON_2014_07_14)) should haveValue(false)
  }

  //-------------------------------------------------------------------------
  test("test_shift") {
    forAll(data_shift) { (day: LocalDate, amount: Int, expected: LocalDate) =>
      withClue(s"$day shifted by $amount: ") {
        MOCK.shift(day, amount) shouldBe expected
      }
    }
  }

  test("test_adjustBy") {
    forAll(data_shift) { (day: LocalDate, amount: Int, expected: LocalDate) =>
      withClue(s"$day adjusted by $amount: ") {
        // `with` is a Scala keyword, hence the back-ticks; `LocalDate.with(TemporalAdjuster)`
        // routes through `DateAdjuster.adjustInto`, and the direct call is asserted alongside it
        day.`with`(MOCK.adjustBy(amount)) shouldBe expected
        MOCK.adjustBy(amount).adjust(day) shouldBe expected
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_next") {
    forAll(data_next) { (day: LocalDate, expectedNext: LocalDate) =>
      withClue(s"$day: ") {
        MOCK.next(day) shouldBe expectedNext
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_nextOrSame") {
    forAll(data_nextOrSame) { (day: LocalDate, expectedNext: LocalDate) =>
      withClue(s"$day: ") {
        MOCK.nextOrSame(day) shouldBe expectedNext
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_previous") {
    forAll(data_previous) { (day: LocalDate, expectedPrevious: LocalDate) =>
      withClue(s"$day: ") {
        MOCK.previous(day) shouldBe expectedPrevious
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_previousOrSame") {
    forAll(data_previousOrSame) { (day: LocalDate, expectedPrevious: LocalDate) =>
      withClue(s"$day: ") {
        MOCK.previousOrSame(day) shouldBe expectedPrevious
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_nextLastOrSame") {
    forAll(data_nextSameOrLastInMonth) { (day: LocalDate, expectedNext: LocalDate) =>
      withClue(s"$day: ") {
        MOCK.nextSameOrLastInMonth(day) shouldBe expectedNext
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_lastBusinessDayOfMonth") {
    forAll(data_lastBusinessDayOfMonth) { (day: LocalDate, expectedEom: LocalDate) =>
      withClue(s"$day: ") {
        MOCK_EOM.lastBusinessDayOfMonth(day) shouldBe expectedEom
      }
    }
  }

  test("test_isLastBusinessDayOfMonth") {
    forAll(data_lastBusinessDayOfMonth) { (day: LocalDate, expectedEom: LocalDate) =>
      withClue(s"$day: ") {
        // the expectation is derived from the same row rather than tabulated separately
        MOCK_EOM.isLastBusinessDayOfMonth(day) shouldBe (day == expectedEom)
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_daysBetween_LocalDateLocalDate") {
    forAll(data_daysBetween) { (start: LocalDate, end: LocalDate, expected: Int) =>
      withClue(s"$start to $end: ") {
        MOCK.daysBetween(start, end) shouldBe expected
      }
    }
  }

  test("test_daysBetween_LocalDateLocalDate_endBeforeStart") {
    // Precondition: `startInclusive` must not be after `endExclusive` - a fault in the calling
    // code, independent of the holidays held, so it stays a fail-fast `ArgCheck` throw.
    val thrown =
      intercept[IllegalArgumentException](MOCK.daysBetween(TUE_2014_07_15, MON_2014_07_14))
    thrown.getMessage should include("startInclusive")
    thrown.getMessage should include("endExclusive")
  }

  //-------------------------------------------------------------------------
  test("test_businessDays_LocalDateLocalDate") {
    forAll(data_businessDays) { (start: LocalDate, end: LocalDate, expected: List[LocalDate]) =>
      withClue(s"$start to $end: ") {
        // `businessDays` returns a single-use `Iterator`, materialised exactly once here
        MOCK.businessDays(start, end).toList shouldBe expected
      }
    }
  }

  test("test_businessDays_LocalDateLocalDate_endBeforeStart") {
    // Precondition: `startInclusive` must not be after `endExclusive`, checked before the iterator.
    val thrown =
      intercept[IllegalArgumentException](MOCK.businessDays(TUE_2014_07_15, MON_2014_07_14))
    thrown.getMessage should include("startInclusive")
    thrown.getMessage should include("endExclusive")
  }

  //-------------------------------------------------------------------------
  test("test_holidays_LocalDateLocalDate") {
    forAll(data_holidays) { (start: LocalDate, end: LocalDate, expected: List[LocalDate]) =>
      withClue(s"$start to $end: ") {
        // as for `businessDays`, the single-use iterator is materialised exactly once
        MOCK.holidays(start, end).toList shouldBe expected
      }
    }
  }

  test("test_holidays_LocalDateLocalDate_endBeforeStart") {
    // Precondition: `startInclusive` must not be after `endExclusive`, checked before the iterator.
    val thrown =
      intercept[IllegalArgumentException](MOCK.holidays(TUE_2014_07_15, MON_2014_07_14))
    thrown.getMessage should include("startInclusive")
    thrown.getMessage should include("endExclusive")
  }

  //-------------------------------------------------------------------------
  test("test_combinedWith") {
    val base1: HolidayCalendar = MOCK
    val base2: HolidayCalendar = HolidayCalendars.FRI_SAT
    val test = base1.combinedWith(base2)

    // the name of a combination is composed from the names of its parts, sorted, so it does not
    // depend on which part was passed first; the identifier says the same thing
    test.toString shouldBe "HolidayCalendar[Fri/Sat+Mock]"
    test.name shouldBe "Fri/Sat+Mock"
    test.id shouldBe HolidayCalendarId.of("Fri/Sat+Mock")

    // equality is structural over the pair of parts, so an equal combination built a second time
    // is equal and hashes alike, while another type and the null reference are not
    test.equals(base1.combinedWith(base2)) shouldBe true
    test.equals(ANOTHER_TYPE) shouldBe false
    test.equals(NULL_REFERENCE) shouldBe false
    test.hashCode shouldBe base1.combinedWith(base2).hashCode

    // A day is a holiday of a combination where EITHER part is closed: `Fri/Sat` closes Friday,
    // Saturday, the 18th and the 19th, the mock calendar Saturday, Sunday, the 16th and the 18th,
    // and the result observes the union.
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

  test("test_combineWith_same") {
    // combining a calendar with itself yields the calendar, not a composite equal to it
    val base: HolidayCalendar = MOCK
    val test = base.combinedWith(base)
    test should be theSameInstanceAs base
  }

  test("test_combineWith_none") {
    val base: HolidayCalendar = MOCK
    val test = base.combinedWith(HolidayCalendars.NO_HOLIDAYS)
    test should be theSameInstanceAs base
  }

  //-------------------------------------------------------------------------
  test("test_linkedWith") {
    val base1: HolidayCalendar = MOCK
    val base2: HolidayCalendar = HolidayCalendars.FRI_SAT
    val test = base1.linkedWith(base2)

    // a link is named as a combination is, with its own separator
    test.toString shouldBe "HolidayCalendar[Fri/Sat~Mock]"
    test.name shouldBe "Fri/Sat~Mock"
    test.id shouldBe HolidayCalendarId.of("Fri/Sat~Mock")

    test.equals(base1.linkedWith(base2)) shouldBe true
    test.equals(ANOTHER_TYPE) shouldBe false
    test.equals(NULL_REFERENCE) shouldBe false
    test.hashCode shouldBe base1.linkedWith(base2).hashCode

    // A day is a holiday of a link only where BOTH parts are closed, the whole difference from a
    // combination: Friday the 11th is closed by `Fri/Sat` alone and Sunday the 13th by the mock
    // calendar alone, so neither is a holiday, while the 12th, 18th and 19th are closed by both.
    test.isHoliday(THU_2014_07_10) shouldBe false
    test.isHoliday(FRI_2014_07_11) shouldBe false
    test.isHoliday(SAT_2014_07_12) shouldBe true
    test.isHoliday(SUN_2014_07_13) shouldBe false
    test.isHoliday(MON_2014_07_14) shouldBe false
    test.isHoliday(TUE_2014_07_15) shouldBe false
    test.isHoliday(WED_2014_07_16) shouldBe false
    test.isHoliday(THU_2014_07_17) shouldBe false
    test.isHoliday(FRI_2014_07_18) shouldBe true
    test.isHoliday(SAT_2014_07_19) shouldBe true
    test.isHoliday(SUN_2014_07_20) shouldBe false
    test.isHoliday(MON_2014_07_21) shouldBe false
  }

  test("test_linkedWith_same") {
    // linking a calendar to itself changes nothing
    val base: HolidayCalendar = MOCK
    val test = base.linkedWith(base)
    test should be theSameInstanceAs base
  }

  //-------------------------------------------------------------------------
  // The three limits of the family. None of them has a counterpart in the library being ported,
  // which walked whatever it was asked to walk and nested however deep it was told to: each
  // bounds work whose size a value arriving from outside the program would otherwise decide.

  test("shift refuses a number of business days no search can satisfy") {
    // Shifting walks the business days asked for, so the cost of the call is the count - and the
    // count is `Int`-wide. Two thousand million business days occupy a processor for the best
    // part of a minute and name a date six million years away, which no rule of finance does.
    val limit: Int = HolidayCalendar.MaxBusinessDayShift
    limit should be > 100
    intercept[IllegalArgumentException](HolidayCalendars.SAT_SUN.shift(WED_2014_07_16, limit + 1))
    intercept[IllegalArgumentException](HolidayCalendars.SAT_SUN.shift(WED_2014_07_16, -limit - 1))
    intercept[IllegalArgumentException](HolidayCalendars.SAT_SUN.shift(WED_2014_07_16, Int.MaxValue))
    // the magnitude is measured in `Long` arithmetic, so the smallest `Int` - whose negation
    // overflows back to itself - is refused rather than slipping through the comparison
    intercept[IllegalArgumentException](HolidayCalendars.SAT_SUN.shift(WED_2014_07_16, Int.MinValue))
    intercept[IllegalArgumentException](MOCK.shift(WED_2014_07_16, Int.MaxValue))
    // the adjuster judges the count where it is named rather than when it is applied
    intercept[IllegalArgumentException](HolidayCalendars.SAT_SUN.adjustBy(Int.MaxValue))

    // a shift within the limit answers exactly as it always did
    HolidayCalendars.SAT_SUN.shift(WED_2014_07_16, 2) shouldBe FRI_2014_07_18
    HolidayCalendars.SAT_SUN.shift(WED_2014_07_16, -2) shouldBe MON_2014_07_14
    HolidayCalendars.SAT_SUN.adjustBy(2).adjust(WED_2014_07_16) shouldBe FRI_2014_07_18

    // the calendar of no holidays adds its days in one step whatever their number, so it is
    // deliberately not limited: nothing about its cost depends on the count
    HolidayCalendars.NO_HOLIDAYS.shift(WED_2014_07_16, Int.MaxValue).getYear should be > 5000000
    HolidayCalendars.NO_HOLIDAYS.adjustBy(Int.MaxValue).adjust(WED_2014_07_16).getYear should be > 5000000
  }

  test("a business day search refuses a calendar that has no business day") {
    // Both parts here are ordinary calendars - a western weekend and a working week - so no
    // factory could have refused either of them, and between them they close every day of the
    // week. A search of such a calendar cannot succeed, and without a progress requirement it
    // walks millions of days to the end of the range of dates before reporting the year it
    // reached, which says nothing about what is actually wrong.
    val workingWeek: HolidayCalendar = ImmutableHolidayCalendar.of(
      HolidayCalendarId.of("HolidayCalendarSpecWorkingWeek"),
      List(date(2014, 1, 1)),
      List(
        java.time.DayOfWeek.MONDAY,
        java.time.DayOfWeek.TUESDAY,
        java.time.DayOfWeek.WEDNESDAY,
        THURSDAY,
        FRIDAY))
    val alwaysClosed: HolidayCalendar = HolidayCalendars.SAT_SUN.combinedWith(workingWeek)

    val refusal: String =
      intercept[IllegalArgumentException](alwaysClosed.nextOrSame(WED_2014_07_16)).getMessage
    refusal should include(alwaysClosed.name)
    refusal should include(WED_2014_07_16.toString)
    refusal should include(HolidayCalendar.MaxConsecutiveHolidays.toString)
    intercept[IllegalArgumentException](alwaysClosed.previousOrSame(WED_2014_07_16))
    intercept[IllegalArgumentException](alwaysClosed.next(WED_2014_07_16))
    intercept[IllegalArgumentException](alwaysClosed.previous(WED_2014_07_16))
    intercept[IllegalArgumentException](alwaysClosed.shift(WED_2014_07_16, 1))

    // and it is bounded rather than merely eventual: the search crosses at most the stated
    // number of days, whichever calendar it is asked about
    HolidayCalendar.MaxConsecutiveHolidays should be > 366

    // a calendar that leaves a day open is unaffected, however long its closures
    alwaysClosed.isHoliday(WED_2014_07_16) shouldBe true
    HolidayCalendars.SAT_SUN.nextOrSame(SAT_2014_07_12) shouldBe MON_2014_07_14
  }

  test("composition refuses a composite deeper than the family allows") {
    // A composite reads a pair of calendars on every query, either of which may be a composite,
    // so every recursive operation of the family - deciding a date, composing the identifier,
    // writing the calendar out, reading one back - walks a tree whose height is the nesting. A
    // tree tall enough exhausts the stack, which ends the calling thread rather than the
    // calculation.
    val limit: Int = HolidayCalendar.MaxCompositeDepth
    limit should be > 30
    val deepest: HolidayCalendar =
      (1 to limit).foldLeft(HolidayCalendars.SAT_SUN)((inner, _) =>
        HolidayCalendar.Combined(inner, HolidayCalendars.THU_FRI))

    // a composite at the limit is a calendar like any other
    deepest.isHoliday(SAT_2014_07_12) shouldBe true
    deepest.isHoliday(WED_2014_07_16) shouldBe false

    // one calendar deeper is refused, by both composition methods and by the constructors of
    // both composites, so the limit cannot be gone round by building the tree directly
    intercept[IllegalArgumentException](deepest.combinedWith(MOCK)).getMessage should
      include(limit.toString)
    intercept[IllegalArgumentException](deepest.linkedWith(MOCK))
    intercept[IllegalArgumentException](HolidayCalendar.Combined(deepest, MOCK))
    intercept[IllegalArgumentException](HolidayCalendar.Linked(deepest, MOCK))
    intercept[IllegalArgumentException](HolidayCalendar.Combined(deepest, deepest))

    // the short-circuiting compositions are unaffected, neither of them building a composite
    deepest.combinedWith(deepest) should be theSameInstanceAs deepest
    deepest.combinedWith(HolidayCalendars.NO_HOLIDAYS) should be theSameInstanceAs deepest
    deepest.linkedWith(HolidayCalendars.NO_HOLIDAYS) should be theSameInstanceAs
      HolidayCalendars.NO_HOLIDAYS
  }

  //-------------------------------------------------------------------------
  test("test_linkedWith_none") {
    // the calendar with no holidays absorbs a link rather than being its identity: every day is
    // a business day of it, so no day is left on which both parts are closed
    val base: HolidayCalendar = MOCK
    val test = base.linkedWith(HolidayCalendars.NO_HOLIDAYS)
    test should be theSameInstanceAs HolidayCalendars.NO_HOLIDAYS
  }

  //-------------------------------------------------------------------------
  test("test_extendedEnum") {
    // The built-in calendars are closed data in `StandardHolidayCalendars`, and the three
    // name-resolution views over them - `HolidayCalendars.of`, `StandardHolidayCalendars.byName`
    // and `StandardHolidayCalendars.byUpperName` - agree over the whole of that set.
    HolidayCalendars.of("NoHolidays") should haveValue(HolidayCalendars.NO_HOLIDAYS)
    StandardHolidayCalendars.byName("NoHolidays") shouldBe Some(HolidayCalendars.NO_HOLIDAYS)

    val builtIn = StandardHolidayCalendars.all
    val expectedMembers =
      List(
        HolidayCalendarIds.NO_HOLIDAYS,
        HolidayCalendarIds.SAT_SUN,
        HolidayCalendarIds.FRI_SAT,
        HolidayCalendarIds.THU_FRI,
        HolidayCalendarIds.GBLO)
    expectedMembers.foreach(id => builtIn.keySet should contain(id))
    builtIn.foreach {
      case (id, calendar) =>
        withClue(s"${id.name}: ") {
          // the calendar filed under an identifier claims that identifier, which is what makes
          // the lookups below say something: a calendar holding holiday data is equal to any of
          // the same identifier, so they would otherwise accept wrong dates under a right name
          calendar.id shouldBe id
          HolidayCalendars.of(id.name) should haveValue(calendar)
          StandardHolidayCalendars.byName(id.name) shouldBe Some(calendar)
          StandardHolidayCalendars.byUpperName(id.name) shouldBe Some(calendar)
        }
    }

    // `of` resolves the upper-case of a name as well as the canonical name
    HolidayCalendars.of("NOHOLIDAYS") should haveValue(HolidayCalendars.NO_HOLIDAYS)
    HolidayCalendars.of("SAT/SUN") should haveValue(HolidayCalendars.SAT_SUN)
    HolidayCalendars.of("FRI/SAT") should haveValue(HolidayCalendars.FRI_SAT)
    HolidayCalendars.of("THU/FRI") should haveValue(HolidayCalendars.THU_FRI)

    // A name the closed data does not hold depends on the argument rather than the calling code,
    // so it is reported as a failure, whose reason is compared as a `FailureReason` value.
    HolidayCalendars.of("NotKnown") should beFailureWith(FailureReason.PARSING)
  }
}
