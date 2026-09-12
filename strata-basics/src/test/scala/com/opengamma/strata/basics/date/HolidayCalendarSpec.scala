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
 * The Java original ran fifty-one annotated methods - thirty-nine plain tests and twelve
 * parameterised ones fed by ten data providers - and all fifty-one are here, one test each,
 * under the name the Java method had. Every provider is transcribed row for row into a table,
 * and the two providers that fed two methods each (`data_shift`, which drove both `test_shift`
 * and `test_adjustBy`, and `data_lastBusinessDayOfMonth`, which drove both
 * `test_lastBusinessDayOfMonth` and `test_isLastBusinessDayOfMonth`) are hoisted into one value
 * apiece and read by both of their tests, exactly as the Java file shared one provider method.
 *
 * ===What this spec is about===
 *
 * [[HolidayCalendar]] answers one question - whether a date is a holiday - and derives a dozen
 * more from it. This spec owns that derivation and the calendars whose content follows from
 * their names:
 *
 *   - the four built-in values [[HolidayCalendars.NO_HOLIDAYS]], [[HolidayCalendars.SAT_SUN]],
 *     [[HolidayCalendars.FRI_SAT]] and [[HolidayCalendars.THU_FRI]], each swept over four years
 *     of dates and then put through the date arithmetic it overrides;
 *   - the default methods of the trait - `shift`, `adjustBy`, `next`, `nextOrSame`, `previous`,
 *     `previousOrSame`, `nextSameOrLastInMonth`, `lastBusinessDayOfMonth`,
 *     `isLastBusinessDayOfMonth`, `daysBetween`, `businessDays` and `holidays` - driven by the
 *     ten transcribed tables;
 *   - composition: [[HolidayCalendar.combinedWith]], a holiday where '''either''' part is
 *     closed, and [[HolidayCalendar.linkedWith]], a holiday only where '''both''' are, together
 *     with the identities each of them has.
 *
 * What belongs to neighbouring specs is deliberately not repeated here, which is the division
 * the Java file already had: the behaviour of a calendar built from a list of dates belongs to
 * `ImmutableHolidayCalendarSpec`, the four constants as a holder and the defaulting decoration
 * to `HolidayCalendarsSpec`, the twenty-six generated national calendars to
 * `GlobalHolidayCalendarsSpec` and `parity/HolidayCalendarParitySpec`, the closed-family and
 * alias sweeps to `NamedEnumClosedSpec`, the compile-time proof that the family cannot be
 * extended to `ApiSurfaceSpec`, and the property-based JSON round trip to
 * `json/JsonRoundTripSpec`.
 *
 * ===How the shape of the port changes the assertions===
 *
 *   - '''The two mock calendars are built rather than declared.''' The Java file declared
 *     `MockHolCal` and `MockEomHolCal`, two throwaway implementations of the interface. The
 *     Scala family is `sealed` and every member of it lives in `HolidayCalendar.scala` (AAP
 *     Rule 4), so no implementation can be declared here - which is the point of sealing it.
 *     Each mock is therefore reproduced as an [[ImmutableHolidayCalendar]] carrying the same
 *     identifier and the same holidays: see [[MOCK]] and [[MOCK_EOM]] below, where the
 *     equivalence and its range are set out. The identifiers matter as much as the holidays,
 *     because two of the tests assert the name a composite calendar takes.
 *   - '''`getName` became `name`''' and `HolidayCalendars.of` returns an `Either` rather than
 *     raising on an unknown name, so each of the four `test_*_of` methods asserts a successful
 *     outcome carrying the expected calendar. Every `Either` here is threaded through `map` or
 *     through the outcome matchers rather than forced with `.get` (AAP Rule 5).
 *   - '''`businessDays` and `holidays` return a Scala `Iterator`''' where the Java methods
 *     returned a Java stream, collected there by a Guava collector that has no port. Each
 *     result is materialised '''once''' with `.toList` and compared to a Scala `List`; an
 *     iterator is single-use, so reading one twice would compare the second pass against an
 *     exhausted iterator.
 *   - '''The three `endBeforeStart` methods keep their throw.''' A range supplied the wrong way
 *     round is a fault in the calling code and is independent of the data the calendar holds, so
 *     it remains a fail-fast `ArgCheck` precondition rather than becoming a `Left` (AAP section
 *     0.3.3's classification rule); each of the three tests notes the precondition it is
 *     asserting.
 *   - '''`test_extendedEnum` reads closed data.''' There is no runtime registry (AAP D-2 /
 *     Rule 4); the note on that test says what replaced it.
 */
final class HolidayCalendarSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  // The dates of the Java original, named for their day of the week so that every expectation
  // below can be read without a calendar to hand. July 2014 begins on a Tuesday.
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

  /** The day after the last day of that sweep, as the Java original's end date was exclusive. */
  private val SWEEP_END_EXCLUSIVE: LocalDate = date(2015, 1, 31)

  /**
   * A value of a type that is not a holiday calendar at all.
   *
   * The Java original held the same value under the same name, to assert that a composite
   * calendar reports a value of another type unequal to itself.
   */
  private val ANOTHER_TYPE: Any = ""

  /**
   * The null reference, as a value of `Any`.
   *
   * The Java original asserted `test.equals(null)` directly. It is held in a value here so that
   * the assertion reads as a call of the equality contract rather than as a comparison against
   * a literal `null`, which the compiler would be entitled to fold.
   */
  private val NULL_REFERENCE: Any = null

  //-------------------------------------------------------------------------
  // The two calendars the Java original declared as throwaway implementations of the interface.
  //
  // The family is sealed, so neither can be declared here; each is built instead as an
  // `ImmutableHolidayCalendar` holding exactly the dates the Java rule made holidays, over a
  // range of years that covers every date any test below reaches. The equivalence is therefore
  // exact where it is used and nowhere claimed beyond it: outside the years its holidays span,
  // an `ImmutableHolidayCalendar` applies its weekend alone, whereas the Java mock applied its
  // rule for all time. Every date these two calendars are asked about lies in 2014, and the
  // furthest any test steps is one business day into August or October 2014, so the five years
  // 2012 to 2016 leave four clear years of margin at each end.
  //
  // The identifiers are the identifiers the Java mocks returned - `Mock` and `MockEom` - which
  // `test_combinedWith` and `test_linkedWith` both depend on, because the name of a composite
  // calendar is composed from the names of its parts.

  /** The first year the two mock calendars hold holiday data for. */
  private val MOCK_FIRST_YEAR: Int = 2012

  /** The year after the last one the two mock calendars hold holiday data for. */
  private val MOCK_END_YEAR_EXCLUSIVE: Int = 2017

  /** The years the two mock calendars hold holiday data for. */
  private val MOCK_YEARS: List[Int] = (MOCK_FIRST_YEAR until MOCK_END_YEAR_EXCLUSIVE).toList

  /**
   * The days of the month the first mock calendar treats as holidays, whatever month they fall
   * in, which is the rule the Java `MockHolCal.isHoliday` applied alongside its weekend.
   */
  private val MOCK_HOLIDAY_DAYS_OF_MONTH: List[Int] = List(16, 18, 31)

  /**
   * The calendar the Java original called `MockHolCal`.
   *
   * A holiday is the 16th, the 18th or the 31st of any month, or a Saturday or a Sunday. In
   * July 2014 - the month nearly every table below works in - that leaves the business days
   * 1st to 4th, 7th to 11th, 14th, 15th, 17th, 21st to 25th and 28th to 30th, which is what
   * makes the Java expectations skip from the 15th to the 17th and from the 17th to the 21st.
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
   * The calendar the Java original called `MockEomHolCal`.
   *
   * A holiday is the 30th of June, the 31st of July, or a Saturday or a Sunday - chosen by the
   * Java original so that the last day of the month falls each of the four ways that matter to
   * `lastBusinessDayOfMonth`: a holiday on a weekday (June), a holiday on a weekday again but
   * with no weekend before it (July), a weekend day (August) and an ordinary business day
   * (September).
   */
  private val MOCK_EOM: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(
      HolidayCalendarId.of("MockEom"),
      MOCK_YEARS.flatMap(year => List(LocalDate.of(year, 6, 30), LocalDate.of(year, 7, 31))),
      Set(SATURDAY, SUNDAY))

  //-------------------------------------------------------------------------
  /**
   * The rows of the Java provider `data_shift`, transcribed in order.
   *
   * Five blocks of twelve dates - shifts of one and two business days forward, no shift at all,
   * and one and two business days back - each block running from Thursday the 10th to whichever
   * date the Java original started it from. This table drives both `test_shift` and
   * `test_adjustBy`, as the one Java provider drove both of those methods.
   */
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

  /** The rows of the Java provider `data_next`, transcribed in order. */
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

  /** The rows of the Java provider `data_nextOrSame`, transcribed in order. */
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

  /** The rows of the Java provider `data_previous`, transcribed in order. */
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

  /** The rows of the Java provider `data_previousOrSame`, transcribed in order. */
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
   * The rows of the Java provider `data_nextSameOrLastInMonth`, transcribed in order.
   *
   * The final row is the one that distinguishes this method from `nextOrSame`: the 31st of July
   * is a holiday of the mock calendar and the next business day after it falls in August, so the
   * answer runs '''backwards''' to the last business day of July.
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

  /**
   * The rows of the Java provider `data_lastBusinessDayOfMonth`, transcribed in order together
   * with the Java comments that say why each month is there.
   *
   * This table drives both `test_lastBusinessDayOfMonth` and `test_isLastBusinessDayOfMonth`, as
   * the one Java provider drove both of those methods, and it is read against [[MOCK_EOM]]
   * rather than [[MOCK]].
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

  /** The rows of the Java provider `data_daysBetween`, transcribed in order. */
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

  /**
   * The rows of the Java provider `data_businessDays`, transcribed in order.
   *
   * The expected values were `ImmutableList`s built with Guava; they are Scala `List`s here,
   * which is what the ported `businessDays` iterator is materialised into.
   */
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

  /** The rows of the Java provider `data_holidays`, transcribed in order. */
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
   * Sweeps a calendar over the four years of dates the Java original swept, asserting that it
   * calls exactly the expected days holidays and that `isBusinessDay` is the complement of
   * `isHoliday` on every one of them.
   *
   * The Java original wrote this sweep out once per built-in calendar, differing only in the
   * predicate; the predicate is the parameter here and the four tests below each supply their
   * own, so each of them still states the whole of what its calendar means.
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
   * Asserts the sixteen shifts the Java original's private `assertSatSun` helper asserted.
   *
   * It is applied to two calendars by `test_SAT_SUN_shift`: the built-in `Sat/Sun` calendar, and
   * an [[ImmutableHolidayCalendar]] holding no holidays at all but declaring Saturday and Sunday
   * as its weekend. The point of running the same sixteen shifts against both is that the two
   * reach their answers by different routes - one applies its weekend directly, the other has
   * stored months to search first - and must agree.
   *
   * @param test  the calendar to shift against
   * @return the assertion that all sixteen shifts landed where they should
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
    // every day is a business day, so the sweep expects no holiday at all
    assertSweep(test, _ => false)
    test.name shouldBe "NoHolidays"
    test.toString shouldBe "HolidayCalendar[NoHolidays]"
  }

  test("test_NO_HOLIDAYS_of") {
    // `of` reports an unknown name as a failure rather than raising, so the outcome is asserted
    // to be a success carrying the constant rather than compared with it directly
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
    // the calendar with no holidays is the identity of combination, and the identity is the
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
    // a calendar holding no holidays but declaring the same weekend must shift identically
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
    // the parts of a combined name are sorted, so a name written the other way round names the
    // same calendar and the calendar reports the sorted name
    val test = HolidayCalendars.of("Thu/Fri+Fri/Sat")
    test.map(calendar => calendar.name) should haveValue("Fri/Sat+Thu/Fri")
    test.map(calendar => calendar.toString) should haveValue("HolidayCalendar[Fri/Sat+Thu/Fri]")

    val test2 = HolidayCalendars.of("Thu/Fri+Fri/Sat")
    test shouldBe test2

    // and it behaves as a combination should: a holiday where either part is closed, which for
    // these two parts is Thursday, Friday and Saturday, leaving Sunday and Monday business days
    test.map(calendar => calendar.isHoliday(THU_2014_07_10)) should haveValue(true)
    test.map(calendar => calendar.isHoliday(FRI_2014_07_11)) should haveValue(true)
    test.map(calendar => calendar.isHoliday(SAT_2014_07_12)) should haveValue(true)
    test.map(calendar => calendar.isHoliday(SUN_2014_07_13)) should haveValue(false)
    test.map(calendar => calendar.isHoliday(MON_2014_07_14)) should haveValue(false)
  }

  //-------------------------------------------------------------------------
  test("test_shift") {
    // the mock calendar has Sat/Sun plus the 16th, 18th and 31st as holidays
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
    // mock calendar has Sat/Sun plus 16th, 18th and 31st as holidays
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
        // only the last business day of the month is the last business day of the month, so the
        // expectation is derived from the same row rather than tabulated separately
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
    // Precondition: `startInclusive` must not be after `endExclusive`. Supplying a range the
    // wrong way round is a fault in the calling code and is independent of the holidays the
    // calendar holds, so it stays a fail-fast `ArgCheck` throw rather than becoming a `Left`
    // (AAP section 0.3.3's classification rule for a caller-contract violation).
    val thrown =
      intercept[IllegalArgumentException](MOCK.daysBetween(TUE_2014_07_15, MON_2014_07_14))
    thrown.getMessage should include("startInclusive")
    thrown.getMessage should include("endExclusive")
  }

  //-------------------------------------------------------------------------
  test("test_businessDays_LocalDateLocalDate") {
    forAll(data_businessDays) { (start: LocalDate, end: LocalDate, expected: List[LocalDate]) =>
      withClue(s"$start to $end: ") {
        // the ported method returns a Scala `Iterator`, which is single-use, so it is
        // materialised exactly once and the list is what the row is compared against
        MOCK.businessDays(start, end).toList shouldBe expected
      }
    }
  }

  test("test_businessDays_LocalDateLocalDate_endBeforeStart") {
    // Precondition: `startInclusive` must not be after `endExclusive`, checked before the
    // iterator is built. As above, a range the wrong way round is a caller-contract violation
    // independent of the data, so it remains a fail-fast `ArgCheck` throw.
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
    // Precondition: `startInclusive` must not be after `endExclusive`, checked before the
    // iterator is built. As above, a range the wrong way round is a caller-contract violation
    // independent of the data, so it remains a fail-fast `ArgCheck` throw.
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

    // equality is structural over the pair of parts, so an equal combination built a second
    // time is equal and hashes alike, and neither a value of another type nor the null
    // reference is equal to it
    test.equals(base1.combinedWith(base2)) shouldBe true
    test.equals(ANOTHER_TYPE) shouldBe false
    test.equals(NULL_REFERENCE) shouldBe false
    test.hashCode shouldBe base1.combinedWith(base2).hashCode

    // A day is a holiday of a combination where EITHER part is closed. Friday, Saturday, the
    // 18th and the 19th are holidays of `Fri/Sat`; Saturday, Sunday, the 16th and the 18th are
    // holidays of the mock calendar; the result observes the union of the two.
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
    // combining a calendar with itself changes nothing, and the result is the calendar itself
    // rather than a composite equal to it
    val base: HolidayCalendar = MOCK
    val test = base.combinedWith(base)
    test should be theSameInstanceAs base
  }

  test("test_combineWith_none") {
    // the calendar with no holidays is the identity of combination
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

    // A day is a holiday of a link only where BOTH parts are closed, which is the whole
    // difference from a combination: Friday the 11th is a holiday of `Fri/Sat` alone and Sunday
    // the 13th of the mock calendar alone, so neither is a holiday here, while Saturday the
    // 12th, Friday the 18th and Saturday the 19th are holidays of both and remain holidays.
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

  test("test_linkedWith_none") {
    // the calendar with no holidays absorbs a link rather than being its identity: every day is
    // a business day of it, so no day is left on which both parts are closed
    val base: HolidayCalendar = MOCK
    val test = base.linkedWith(HolidayCalendars.NO_HOLIDAYS)
    test should be theSameInstanceAs HolidayCalendars.NO_HOLIDAYS
  }

  //-------------------------------------------------------------------------
  test("test_extendedEnum") {
    // The Java method read one entry of a runtime registry:
    //   HolidayCalendars.extendedEnum().lookupAll().get("NoHolidays") == NO_HOLIDAYS
    // There is no registry in this port (AAP D-2 / Rule 4). The built-in calendars are closed
    // data in `StandardHolidayCalendars`, and the three name-resolution views that replace the
    // registry - `HolidayCalendars.of`, `StandardHolidayCalendars.byName` (the canonical key
    // space, which is what `lookupAllNormalized` published) and
    // `StandardHolidayCalendars.byUpperName` (the English upper-case key space, which the
    // registry filed every calendar under as well) - are asserted to agree over the whole of
    // that data rather than over the single entry the Java method sampled.
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
          // the three lookups below say something: a calendar holding holiday data is equal to
          // any calendar of the same identifier, so without this the lookups could be satisfied
          // by a calendar of the right name holding the wrong dates. What the dates should be is
          // asserted by `GlobalHolidayCalendarsSpec` and `parity/HolidayCalendarParitySpec`.
          calendar.id shouldBe id
          HolidayCalendars.of(id.name) should haveValue(calendar)
          StandardHolidayCalendars.byName(id.name) shouldBe Some(calendar)
          StandardHolidayCalendars.byUpperName(id.name) shouldBe Some(calendar)
        }
    }

    // the upper-case key space of the four calendars whose content follows from their names,
    // spelled out so that the second of the registry's two keys is asserted literally
    HolidayCalendars.of("NOHOLIDAYS") should haveValue(HolidayCalendars.NO_HOLIDAYS)
    HolidayCalendars.of("SAT/SUN") should haveValue(HolidayCalendars.SAT_SUN)
    HolidayCalendars.of("FRI/SAT") should haveValue(HolidayCalendars.FRI_SAT)
    HolidayCalendars.of("THU/FRI") should haveValue(HolidayCalendars.THU_FRI)

    // A name the closed data does not hold depends on the argument rather than on the calling
    // code, so it is reported as a failure rather than raised (AAP Rule 5), and the reason is
    // compared as a value of the closed family of reasons rather than as message text.
    HolidayCalendars.of("NotKnown") should beFailureWith(FailureReason.PARSING)
  }
}
