/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.schedule

import java.time.DayOfWeek
import java.time.LocalDate
import java.time.Month.APRIL
import java.time.Month.AUGUST
import java.time.Month.FEBRUARY
import java.time.Month.JANUARY
import java.time.Month.JULY
import java.time.Month.JUNE
import java.time.Month.MARCH
import java.time.Month.NOVEMBER
import java.time.Month.OCTOBER
import java.time.Month.SEPTEMBER
import java.time.temporal.TemporalAdjusters
import java.util.Locale

import cats.Eq
import cats.Hash
import cats.Order
import cats.Show

import io.circe.Codec
import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor1
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableFor3
import org.scalatest.prop.TableFor4

import com.opengamma.strata.basics.date.HolidayCalendar
import com.opengamma.strata.basics.date.StandardHolidayCalendars
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[RollConvention]] - the rule each convention applies to a date, and the names it answers
 * to.
 *
 * Two things hold spec-wide. Lookup and construction report rejection rather than raising:
 * [[RollConvention.valueOf]] is the exact lookup over the canonical and upper-case keys and
 * answers an `Option`, [[RollConvention.parse]] applies the ordered lenient rewrites and answers
 * `EitherNec[Failure, _]`, and `RollConvention.ofDayOfMonth` answers a `FailureOr`; a test that
 * needs the convention itself opens such a result through `rc`, and a rejection is asserted as a
 * value carrying its `FailureReason` and its message text. And the family is closed, so
 * `RollConvention.values` is the published inventory of 45 members: the tables below state that
 * inventory, the identifiers of [[RollConventions]] that name it, the 44 external FpML spellings
 * and the 11 ordered lenient rewrites, because each of those is code here and a lost row would
 * otherwise be invisible.
 *
 * Dates are compared exactly; no tolerance takes part.
 *
 * @see [[FrequencySpec]] for the periodic frequencies these conventions are applied with
 */
final class RollConventionSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  import RollConventionSpec._

  //-------------------------------------------------------------------------
  /**
   * Unwraps a result that the test expects to hold a convention.
   *
   * The factories of this family report rejection on the left of an `Either` rather than raising,
   * so a test that needs the convention itself - to adjust a date with it, or to compare its
   * identity - has to open the result. Opening it here rather than at each call site means a
   * result that unexpectedly holds a failure fails the test naming that failure, instead of
   * failing later with a less useful message.
   *
   * @param result  the result to unwrap
   * @tparam E  the type of the failure the result may hold
   * @return the convention the result holds
   */
  private def rc[E](result: Either[E, RollConvention]): RollConvention =
    result.fold(failure => fail(s"unexpected failure: $failure"), identity)

  /**
   * Looks a convention up by name exactly, failing the test when the name resolves to nothing.
   *
   * This is [[RollConvention.valueOf]] with the absent case turned into a test failure, which is
   * what lets an assertion about the ''identity'' of the value a name resolves to be written
   * without unsafely opening an `Option`.
   *
   * @param name  the name to look up
   * @return the convention with that name
   */
  private def resolvedByName(name: String): RollConvention =
    RollConvention.valueOf(name).getOrElse(fail(s"no convention is named $name"))

  //-------------------------------------------------------------------------
  /**
   * Eight conventions with the name each renders as: six of the rule-based members, one
   * day-of-month member and one day-of-week member.
   */
  private val dataName: TableFor2[RollConvention, String] = Table(
    ("convention", "name"),
    (RollConventions.NONE, "None"),
    (RollConventions.EOM, "EOM"),
    (RollConventions.IMM, "IMM"),
    (RollConventions.IMMAUD, "IMMAUD"),
    (RollConventions.IMMNZD, "IMMNZD"),
    (RollConventions.SFE, "SFE"),
    (RollConventions.DAY_2, "Day2"),
    (RollConventions.DAY_THU, "DayThu")
  )

  /**
   * Eleven spellings that resolve leniently, each with the convention it resolves to.
   *
   * Every row is a spelling that the ordered chain of lenient rewrites turns into a canonical
   * name. The rows are what makes the order of that chain observable: the three spellings of the
   * 31st reach `EOM` only because the pattern for 31 is applied before the pattern that captures
   * a one- or two-digit day, and a reordering of the patterns - which would change what resolves
   * and to what - is caught here.
   */
  private val dataLenient: TableFor2[String, RollConvention] = Table(
    ("name", "convention"),
    ("2", RollConventions.DAY_2),
    ("29", RollConventions.DAY_29),
    ("Day29", RollConventions.DAY_29),
    ("Day_29", RollConventions.DAY_29),
    ("30", RollConventions.DAY_30),
    ("Day30", RollConventions.DAY_30),
    ("Day_30", RollConventions.DAY_30),
    ("31", RollConventions.EOM),
    ("Day31", RollConventions.EOM),
    ("Day_31", RollConventions.EOM),
    ("THU", RollConventions.DAY_THU)
  )

  /**
   * The identifiers of the 45 constants of [[RollConventions]], each with the member it names.
   *
   * The table fixes both halves of the holder: which 45 identifiers [[RollConventions]] publishes,
   * and which member each of them names.
   */
  private val dataConstantIdentifiers: TableFor2[String, RollConvention] = Table(
    ("identifier", "convention"),
    ("NONE", RollConventions.NONE),
    ("EOM", RollConventions.EOM),
    ("IMM", RollConventions.IMM),
    ("IMMCAD", RollConventions.IMMCAD),
    ("IMMAUD", RollConventions.IMMAUD),
    ("IMMNZD", RollConventions.IMMNZD),
    ("SFE", RollConventions.SFE),
    ("TBILL", RollConventions.TBILL),
    ("DAY_1", RollConventions.DAY_1),
    ("DAY_2", RollConventions.DAY_2),
    ("DAY_3", RollConventions.DAY_3),
    ("DAY_4", RollConventions.DAY_4),
    ("DAY_5", RollConventions.DAY_5),
    ("DAY_6", RollConventions.DAY_6),
    ("DAY_7", RollConventions.DAY_7),
    ("DAY_8", RollConventions.DAY_8),
    ("DAY_9", RollConventions.DAY_9),
    ("DAY_10", RollConventions.DAY_10),
    ("DAY_11", RollConventions.DAY_11),
    ("DAY_12", RollConventions.DAY_12),
    ("DAY_13", RollConventions.DAY_13),
    ("DAY_14", RollConventions.DAY_14),
    ("DAY_15", RollConventions.DAY_15),
    ("DAY_16", RollConventions.DAY_16),
    ("DAY_17", RollConventions.DAY_17),
    ("DAY_18", RollConventions.DAY_18),
    ("DAY_19", RollConventions.DAY_19),
    ("DAY_20", RollConventions.DAY_20),
    ("DAY_21", RollConventions.DAY_21),
    ("DAY_22", RollConventions.DAY_22),
    ("DAY_23", RollConventions.DAY_23),
    ("DAY_24", RollConventions.DAY_24),
    ("DAY_25", RollConventions.DAY_25),
    ("DAY_26", RollConventions.DAY_26),
    ("DAY_27", RollConventions.DAY_27),
    ("DAY_28", RollConventions.DAY_28),
    ("DAY_29", RollConventions.DAY_29),
    ("DAY_30", RollConventions.DAY_30),
    ("DAY_MON", RollConventions.DAY_MON),
    ("DAY_TUE", RollConventions.DAY_TUE),
    ("DAY_WED", RollConventions.DAY_WED),
    ("DAY_THU", RollConventions.DAY_THU),
    ("DAY_FRI", RollConventions.DAY_FRI),
    ("DAY_SAT", RollConventions.DAY_SAT),
    ("DAY_SUN", RollConventions.DAY_SUN)
  )

  /**
   * The seven days of the week with the canonical name of the convention that selects each.
   *
   * The names are written out rather than derived from each day's own name, so that the table
   * states the expected names instead of recomputing them.
   */
  private val dataDayOfWeekNames: TableFor2[DayOfWeek, String] = Table(
    ("dayOfWeek", "name"),
    (DayOfWeek.MONDAY, "DayMon"),
    (DayOfWeek.TUESDAY, "DayTue"),
    (DayOfWeek.WEDNESDAY, "DayWed"),
    (DayOfWeek.THURSDAY, "DayThu"),
    (DayOfWeek.FRIDAY, "DayFri"),
    (DayOfWeek.SATURDAY, "DaySat"),
    (DayOfWeek.SUNDAY, "DaySun")
  )

  //-------------------------------------------------------------------------
  test("test_null") {
    // What each of the four operations requires of its arguments. `adjust` and `matches` decide
    // on a date, `next` and `previous` on a date and a frequency, and every one of those
    // parameters is required and typed: a call that omits one, or supplies the two in the wrong
    // order, does not compile.
    assertDoesNotCompile("RollConventions.EOM.adjust()")
    assertDoesNotCompile("RollConventions.EOM.matches()")
    assertDoesNotCompile("RollConventions.EOM.next(date(2014, JULY, 1))")
    assertDoesNotCompile("RollConventions.EOM.previous(date(2014, JULY, 1))")
    assertDoesNotCompile("RollConventions.EOM.next(Frequency.P3M, date(2014, JULY, 1))")

    // What they then decide. Each of the eight rule-based conventions answers for a date rather
    // than raising, `matches` agrees with `adjust` on whether the date already satisfies the
    // rule, and the two sequence operations honour the contract that makes them usable in a
    // schedule - `next` moves strictly forward and `previous` strictly back, whatever the
    // relationship between the frequency and the convention's own cycle.
    val input: LocalDate = date(2014, JULY, 1)
    dataTypes should have size 8
    forAll(dataTypes) { (convention: RollConvention) =>
      withClue(s"${convention.name}: ") {
        noException should be thrownBy convention.adjust(input)
        noException should be thrownBy convention.matches(input)
        convention.matches(input) shouldBe (convention.adjust(input) == input)
        List(Frequency.P1D, Frequency.P1W, Frequency.P1M, Frequency.P3M).foreach { frequency =>
          withClue(s"${frequency.name}: ") {
            convention.next(input, frequency).isAfter(input) shouldBe true
            convention.previous(input, frequency).isBefore(input) shouldBe true
          }
        }
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_noAdjust") {
    val input: LocalDate = date(2014, AUGUST, 17)
    RollConventions.NONE.adjust(input) shouldBe input
    RollConventions.NONE.matches(input) shouldBe true
  }

  //-------------------------------------------------------------------------
  test("test_adjust") {
    // All 51 adjustment rows, including the rows only the three calendar-bearing conventions
    // produce: the IMMCAD dates in August and September 2014, the Sydney-adjusted IMMAUD dates,
    // and the two TBILL rows - 2018-08-31 and 2018-09-01 - that both roll to Tuesday 2018-09-04
    // because the Monday between them is Labor Day in New York.
    dataAdjust should have size 51
    forAll(dataAdjust) { (convention: RollConvention, input: LocalDate, expected: LocalDate) =>
      withClue(s"${convention.name} on $input: ")(convention.adjust(input) shouldBe expected)
    }

    // `IMMCAD`, `IMMAUD` and `TBILL` hold their holiday calendars as data - the built-in values
    // of `StandardHolidayCalendars`, with no lookup and so no fallback - and `adjust(date)` takes
    // a date and answers a date, with no reference data parameter and no error channel. The
    // assertions below restate the three rules over a five-year grid of probe dates rather than
    // only the rows above, so a member bound to the wrong calendar fails here.
    val london: HolidayCalendar = StandardHolidayCalendars.GBLO
    val canada: HolidayCalendar =
      StandardHolidayCalendars.CATO.combinedWith(StandardHolidayCalendars.CAMO)
    val sydney: HolidayCalendar = StandardHolidayCalendars.AUSY
    val newYork: HolidayCalendar = StandardHolidayCalendars.USNY

    calendarProbes should have size 304
    calendarProbes.foreach { input =>
      val thirdWednesday: LocalDate =
        input.`with`(TemporalAdjusters.dayOfWeekInMonth(3, DayOfWeek.WEDNESDAY))
      val secondFriday: LocalDate =
        input.`with`(TemporalAdjusters.dayOfWeekInMonth(2, DayOfWeek.FRIDAY))
      val nextOrSameMonday: LocalDate =
        input.`with`(TemporalAdjusters.nextOrSame(DayOfWeek.MONDAY))
      withClue(s"$input: ") {
        // Two London banking days before the third Wednesday, then back to a business day of the
        // combined Toronto and Montreal calendars.
        RollConventions.IMMCAD.adjust(input) shouldBe
          canada.previousOrSame(london.shift(thirdWednesday, -2))
        RollConventions.IMMAUD.adjust(input) shouldBe sydney.previous(secondFriday)
        RollConventions.TBILL.adjust(input) shouldBe newYork.nextOrSame(nextOrSameMonday)

        // The three conventions that consult no calendar are pure date arithmetic. Their results
        // are asserted against the arithmetic itself and then by the day of the week they always
        // land on: a rule that moved off a holiday could not guarantee that, so the three
        // day-of-week equalities are what states that no calendar takes part.
        RollConventions.IMM.adjust(input) shouldBe thirdWednesday
        RollConventions.IMM.adjust(input).getDayOfWeek shouldBe DayOfWeek.WEDNESDAY
        RollConventions.SFE.adjust(input) shouldBe secondFriday
        RollConventions.SFE.adjust(input).getDayOfWeek shouldBe DayOfWeek.FRIDAY
        RollConventions.IMMNZD.adjust(input) shouldBe
          input.withDayOfMonth(9).`with`(TemporalAdjusters.nextOrSame(DayOfWeek.WEDNESDAY))
        RollConventions.IMMNZD.adjust(input).getDayOfWeek shouldBe DayOfWeek.WEDNESDAY

        // Every rule keeps the year and month of the date it was given, apart from TBILL, which
        // rolls forward and may cross into the following month.
        List(
          RollConventions.IMM,
          RollConventions.IMMCAD,
          RollConventions.IMMAUD,
          RollConventions.IMMNZD,
          RollConventions.SFE).foreach { convention =>
          withClue(s"${convention.name}: ") {
            convention.adjust(input).getYear shouldBe input.getYear
            convention.adjust(input).getMonth shouldBe input.getMonth
          }
        }
        RollConventions.TBILL.adjust(input).isBefore(input) shouldBe false
      }
    }

    // The New York calendar is consulted rather than a weekend-only rule: the Monday of that
    // week is a holiday, so TBILL answers the Tuesday after it.
    RollConventions.TBILL.adjust(date(2018, AUGUST, 31)) shouldBe date(2018, SEPTEMBER, 4)
    RollConventions.TBILL.adjust(date(2018, AUGUST, 31)).getDayOfWeek shouldBe DayOfWeek.TUESDAY
    newYork.isHoliday(date(2018, SEPTEMBER, 3)) shouldBe true
  }

  //-------------------------------------------------------------------------
  test("test_matches") {
    dataMatches should have size 17
    forAll(dataMatches) { (convention: RollConvention, input: LocalDate, expected: Boolean) =>
      withClue(s"${convention.name} on $input: ")(convention.matches(input) shouldBe expected)
    }
  }

  //-------------------------------------------------------------------------
  test("test_next") {
    dataNext should have size 62
    forAll(dataNext) {
      (convention: RollConvention, input: LocalDate, frequency: Frequency, expected: LocalDate) =>
        withClue(s"${convention.name} on $input by ${frequency.name}: ") {
          convention.next(input, frequency) shouldBe expected
        }
    }
  }

  //-------------------------------------------------------------------------
  test("test_previous") {
    dataPrevious should have size 57
    forAll(dataPrevious) {
      (convention: RollConvention, input: LocalDate, frequency: Frequency, expected: LocalDate) =>
        withClue(s"${convention.name} on $input by ${frequency.name}: ") {
          convention.previous(input, frequency) shouldBe expected
        }
    }
  }

  //-------------------------------------------------------------------------
  test("test_dayOfMonth_constants") {
    // The thirty day-of-month constants, each adjusting the 30th of July 2014 to its own day.
    // Written out one constant at a time because the point of the test is that each identifier
    // names the convention its name claims: a loop over `values` would assert the members and
    // say nothing about the identifiers.
    val input: LocalDate = date(2014, JULY, 30)
    RollConventions.DAY_1.adjust(input) shouldBe date(2014, JULY, 1)
    RollConventions.DAY_2.adjust(input) shouldBe date(2014, JULY, 2)
    RollConventions.DAY_3.adjust(input) shouldBe date(2014, JULY, 3)
    RollConventions.DAY_4.adjust(input) shouldBe date(2014, JULY, 4)
    RollConventions.DAY_5.adjust(input) shouldBe date(2014, JULY, 5)
    RollConventions.DAY_6.adjust(input) shouldBe date(2014, JULY, 6)
    RollConventions.DAY_7.adjust(input) shouldBe date(2014, JULY, 7)
    RollConventions.DAY_8.adjust(input) shouldBe date(2014, JULY, 8)
    RollConventions.DAY_9.adjust(input) shouldBe date(2014, JULY, 9)
    RollConventions.DAY_10.adjust(input) shouldBe date(2014, JULY, 10)
    RollConventions.DAY_11.adjust(input) shouldBe date(2014, JULY, 11)
    RollConventions.DAY_12.adjust(input) shouldBe date(2014, JULY, 12)
    RollConventions.DAY_13.adjust(input) shouldBe date(2014, JULY, 13)
    RollConventions.DAY_14.adjust(input) shouldBe date(2014, JULY, 14)
    RollConventions.DAY_15.adjust(input) shouldBe date(2014, JULY, 15)
    RollConventions.DAY_16.adjust(input) shouldBe date(2014, JULY, 16)
    RollConventions.DAY_17.adjust(input) shouldBe date(2014, JULY, 17)
    RollConventions.DAY_18.adjust(input) shouldBe date(2014, JULY, 18)
    RollConventions.DAY_19.adjust(input) shouldBe date(2014, JULY, 19)
    RollConventions.DAY_20.adjust(input) shouldBe date(2014, JULY, 20)
    RollConventions.DAY_21.adjust(input) shouldBe date(2014, JULY, 21)
    RollConventions.DAY_22.adjust(input) shouldBe date(2014, JULY, 22)
    RollConventions.DAY_23.adjust(input) shouldBe date(2014, JULY, 23)
    RollConventions.DAY_24.adjust(input) shouldBe date(2014, JULY, 24)
    RollConventions.DAY_25.adjust(input) shouldBe date(2014, JULY, 25)
    RollConventions.DAY_26.adjust(input) shouldBe date(2014, JULY, 26)
    RollConventions.DAY_27.adjust(input) shouldBe date(2014, JULY, 27)
    RollConventions.DAY_28.adjust(input) shouldBe date(2014, JULY, 28)
    RollConventions.DAY_29.adjust(input) shouldBe date(2014, JULY, 29)
    RollConventions.DAY_30.adjust(input) shouldBe date(2014, JULY, 30)
  }

  //-------------------------------------------------------------------------
  test("test_ofDayOfMonth") {
    // Days 1 to 29; the 30th is covered by `test_dayOfMonth_constants` and by the February cases
    // below.
    (1 until 30).foreach { day =>
      val result: FailureOr[RollConvention] = RollConvention.ofDayOfMonth(day)
      val test: RollConvention = rc(result)
      withClue(s"Day$day: ") {
        result should beSuccess
        test.adjust(date(2014, JULY, 1)) shouldBe date(2014, JULY, day)
        test.name shouldBe s"Day$day"
        test.toString shouldBe s"Day$day"
        test.dayOfMonth shouldBe day

        // Every route to this member answers the same instance: the exact lookup and the lenient
        // one, each in the canonical spelling and in the upper-case one, and the factory itself.
        resolvedByName(test.name) should be theSameInstanceAs test
        resolvedByName(s"DAY$day") should be theSameInstanceAs test
        rc(RollConvention.parse(test.name)) should be theSameInstanceAs test
        rc(RollConvention.parse(s"DAY$day")) should be theSameInstanceAs test
        rc(RollConvention.ofDayOfMonth(day)) should be theSameInstanceAs test
      }
    }
  }

  test("test_ofDayOfMonth_31") {
    // A 31st day-of-month does not exist in every month, so the family publishes `EOM` in place
    // of a `Day31` member: `ofDayOfMonth(31)` answers `EOM`, and the name `Day31` resolves to
    // nothing, which `test_extendedEnum` asserts.
    RollConvention.ofDayOfMonth(31) should haveValue(RollConventions.EOM)
    rc(RollConvention.ofDayOfMonth(31)) should be theSameInstanceAs RollConventions.EOM
  }

  test("test_ofDayOfMonth_invalid") {
    // A day-of-month outside 1 to 31 is reported as a value carrying the reason and the message
    // that name it. Nothing is raised, which is asserted alongside the failure itself.
    RollConvention.ofDayOfMonth(0) should beFailureWith(FailureReason.INVALID)
    RollConvention.ofDayOfMonth(0) should haveFailureMessageMatching("Invalid day-of-month: 0")
    RollConvention.ofDayOfMonth(32) should beFailureWith(FailureReason.INVALID)
    RollConvention.ofDayOfMonth(32) should haveFailureMessageMatching("Invalid day-of-month: 32")
    noException should be thrownBy RollConvention.ofDayOfMonth(0)
    noException should be thrownBy RollConvention.ofDayOfMonth(32)

    // The bounds either side of the two rejected numbers are accepted, so the rejection is the
    // range it claims to be and not an off-by-one.
    RollConvention.ofDayOfMonth(1) should beSuccess
    RollConvention.ofDayOfMonth(31) should beSuccess
    RollConvention.ofDayOfMonth(-1) should beFailureWith(FailureReason.INVALID)
  }

  test("test_ofDayOfMonth_adjust_Day29") {
    rc(RollConvention.ofDayOfMonth(29)).adjust(date(2014, FEBRUARY, 2)) shouldBe
      date(2014, FEBRUARY, 28)
    rc(RollConvention.ofDayOfMonth(29)).adjust(date(2016, FEBRUARY, 2)) shouldBe
      date(2016, FEBRUARY, 29)
  }

  test("test_ofDayOfMonth_adjust_Day30") {
    rc(RollConvention.ofDayOfMonth(30)).adjust(date(2014, FEBRUARY, 2)) shouldBe
      date(2014, FEBRUARY, 28)
    rc(RollConvention.ofDayOfMonth(30)).adjust(date(2016, FEBRUARY, 2)) shouldBe
      date(2016, FEBRUARY, 29)
  }

  test("test_ofDayOfMonth_matches_Day29") {
    val day29: RollConvention = rc(RollConvention.ofDayOfMonth(29))
    day29.matches(date(2016, JANUARY, 30)) shouldBe false
    day29.matches(date(2016, JANUARY, 29)) shouldBe true
    day29.matches(date(2016, JANUARY, 30)) shouldBe false

    day29.matches(date(2016, FEBRUARY, 28)) shouldBe false
    day29.matches(date(2016, FEBRUARY, 29)) shouldBe true

    day29.matches(date(2015, FEBRUARY, 27)) shouldBe false
    day29.matches(date(2015, FEBRUARY, 28)) shouldBe true
  }

  test("test_ofDayOfMonth_matches_Day30") {
    val day30: RollConvention = rc(RollConvention.ofDayOfMonth(30))
    day30.matches(date(2016, JANUARY, 29)) shouldBe false
    day30.matches(date(2016, JANUARY, 30)) shouldBe true
    day30.matches(date(2016, JANUARY, 31)) shouldBe false

    day30.matches(date(2016, FEBRUARY, 28)) shouldBe false
    day30.matches(date(2016, FEBRUARY, 29)) shouldBe true

    day30.matches(date(2015, FEBRUARY, 27)) shouldBe false
    day30.matches(date(2015, FEBRUARY, 28)) shouldBe true
  }

  test("test_ofDayOfMonth_next_oneMonth") {
    for (start <- 1 to 5; day <- 1 to 30) {
      val test: RollConvention = rc(RollConvention.ofDayOfMonth(day))
      withClue(s"Day$day from the ${start}th: ") {
        test.next(date(2014, JULY, start), Frequency.P1M) shouldBe date(2014, AUGUST, day)
      }
    }
  }

  test("test_ofDayOfMonth_next_oneDay") {
    for (start <- 1 to 5; day <- 1 to 30) {
      val test: RollConvention = rc(RollConvention.ofDayOfMonth(day))
      // A one-day frequency is shorter than the monthly cycle of the convention, so the
      // adjustment lands on or before the date supplied whenever the day has already passed in
      // the month; a month is then added, which is what keeps `next` strictly forward.
      val expected: LocalDate =
        if (day <= start) date(2014, JULY, day).plusMonths(1L) else date(2014, JULY, day)
      withClue(s"Day$day from the ${start}th: ") {
        test.next(date(2014, JULY, start), Frequency.P1D) shouldBe expected
      }
    }
  }

  test("test_ofDayOfMonth_previous_oneMonth") {
    for (start <- 1 to 5; day <- 1 to 30) {
      val test: RollConvention = rc(RollConvention.ofDayOfMonth(day))
      withClue(s"Day$day from the ${start}th: ") {
        test.previous(date(2014, JULY, start), Frequency.P1M) shouldBe date(2014, JUNE, day)
      }
    }
  }

  test("test_ofDayOfMonth_previous_oneDay") {
    for (start <- 1 to 5; day <- 1 to 30) {
      val test: RollConvention = rc(RollConvention.ofDayOfMonth(day))
      val expected: LocalDate =
        if (day >= start) date(2014, JULY, day).minusMonths(1L) else date(2014, JULY, day)
      withClue(s"Day$day from the ${start}th: ") {
        test.previous(date(2014, JULY, start), Frequency.P1D) shouldBe expected
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_dayOfWeek_constants") {
    // The 11th of August 2014 is a Monday, so each constant rolls it forward to its own day of
    // the week and the seven results are the seven days of that week.
    val monday: LocalDate = date(2014, AUGUST, 11)
    RollConventions.DAY_MON.adjust(monday) shouldBe date(2014, AUGUST, 11)
    RollConventions.DAY_TUE.adjust(monday) shouldBe date(2014, AUGUST, 12)
    RollConventions.DAY_WED.adjust(monday) shouldBe date(2014, AUGUST, 13)
    RollConventions.DAY_THU.adjust(monday) shouldBe date(2014, AUGUST, 14)
    RollConventions.DAY_FRI.adjust(monday) shouldBe date(2014, AUGUST, 15)
    RollConventions.DAY_SAT.adjust(monday) shouldBe date(2014, AUGUST, 16)
    RollConventions.DAY_SUN.adjust(monday) shouldBe date(2014, AUGUST, 17)
  }

  //-------------------------------------------------------------------------
  test("test_ofDayOfWeek") {
    // `ofDayOfWeek` is total - every one of the seven days has a convention - so there is no
    // result to open here, unlike the day-of-month factory.
    forAll(dataDayOfWeekNames) { (dayOfWeek: DayOfWeek, name: String) =>
      val test: RollConvention = RollConvention.ofDayOfWeek(dayOfWeek)
      withClue(s"$dayOfWeek: ") {
        test.name shouldBe name
        test.toString shouldBe name
        test.dayOfMonth shouldBe 0

        // Resolution answers the same instance here too. The second lookup is the upper-case
        // spelling built from the first three letters of the day's own name.
        resolvedByName(test.name) should be theSameInstanceAs test
        resolvedByName(s"DAY${dayOfWeek.toString.substring(0, 3)}") should be theSameInstanceAs test
        rc(RollConvention.parse(test.name)) should be theSameInstanceAs test
        RollConvention.ofDayOfWeek(dayOfWeek) should be theSameInstanceAs test
      }
    }
  }

  test("test_ofDayOfWeek_adjust") {
    daysOfWeek.foreach { dayOfWeek =>
      val test: RollConvention = RollConvention.ofDayOfWeek(dayOfWeek)
      withClue(s"$dayOfWeek: ") {
        test.adjust(date(2014, AUGUST, 14)) shouldBe
          date(2014, AUGUST, 14).`with`(TemporalAdjusters.nextOrSame(dayOfWeek))
      }
    }
  }

  test("test_ofDayOfWeek_matches") {
    val tuesday: RollConvention = RollConvention.ofDayOfWeek(DayOfWeek.TUESDAY)
    tuesday.matches(date(2014, SEPTEMBER, 1)) shouldBe false
    tuesday.matches(date(2014, SEPTEMBER, 2)) shouldBe true
    tuesday.matches(date(2014, SEPTEMBER, 3)) shouldBe false
  }

  test("test_ofDayOfWeek_next_oneMonth") {
    // A week-based frequency added and then rolled forward to the required day of the week always
    // lands after the date supplied, which is why the day-of-week family overrides the monthly
    // correction of the default `next` - and why a one-week frequency here reaches the week after
    // next rather than the same week.
    daysOfWeek.foreach { dayOfWeek =>
      val test: RollConvention = RollConvention.ofDayOfWeek(dayOfWeek)
      withClue(s"$dayOfWeek: ") {
        test.next(date(2014, AUGUST, 14), Frequency.P1W) shouldBe
          date(2014, AUGUST, 21).`with`(TemporalAdjusters.nextOrSame(dayOfWeek))
      }
    }
  }

  test("test_ofDayOfWeek_next_oneDay") {
    daysOfWeek.foreach { dayOfWeek =>
      val test: RollConvention = RollConvention.ofDayOfWeek(dayOfWeek)
      withClue(s"$dayOfWeek: ") {
        test.next(date(2014, AUGUST, 14), Frequency.P1D) shouldBe
          date(2014, AUGUST, 15).`with`(TemporalAdjusters.nextOrSame(dayOfWeek))
      }
    }
  }

  test("test_ofDayOfWeek_previous_oneMonth") {
    daysOfWeek.foreach { dayOfWeek =>
      val test: RollConvention = RollConvention.ofDayOfWeek(dayOfWeek)
      withClue(s"$dayOfWeek: ") {
        test.previous(date(2014, AUGUST, 14), Frequency.P1W) shouldBe
          date(2014, AUGUST, 7).`with`(TemporalAdjusters.previousOrSame(dayOfWeek))
      }
    }
  }

  test("test_ofDayOfWeek_previous_oneDay") {
    daysOfWeek.foreach { dayOfWeek =>
      val test: RollConvention = RollConvention.ofDayOfWeek(dayOfWeek)
      withClue(s"$dayOfWeek: ") {
        test.previous(date(2014, AUGUST, 14), Frequency.P1D) shouldBe
          date(2014, AUGUST, 13).`with`(TemporalAdjusters.previousOrSame(dayOfWeek))
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_name") {
    dataName should have size 8
    forAll(dataName) { (convention: RollConvention, name: String) =>
      withClue(s"$name: ")(convention.name shouldBe name)
    }
  }

  test("test_toString") {
    forAll(dataName) { (convention: RollConvention, name: String) =>
      withClue(s"$name: ")(convention.toString shouldBe name)
    }
  }

  test("test_of_lookup") {
    // A canonical name is answered by both lookups - the exact one and the lenient one - and
    // resolves to the member itself rather than to an equal copy.
    forAll(dataName) { (convention: RollConvention, name: String) =>
      withClue(s"$name: ") {
        RollConvention.valueOf(name) shouldBe Some(convention)
        RollConvention.parse(name) should haveValue(convention)
        resolvedByName(name) should be theSameInstanceAs convention
      }
    }
  }

  test("test_lenientLookup_standardNames") {
    // The lower-case spelling of a canonical name is deliberately asserted to be outside the
    // exact key space: `parse` resolves it because of the leniency, while `valueOf` answers
    // nothing, so the family registers no third key for each member.
    forAll(dataName) { (convention: RollConvention, name: String) =>
      val lowerCase: String = name.toLowerCase(Locale.ENGLISH)
      withClue(s"$lowerCase: ") {
        RollConvention.parse(lowerCase) should haveValue(convention)
        RollConvention.valueOf(lowerCase) shouldBe None
      }
    }
  }

  test("test_extendedEnum") {
    // The two key views the family publishes: `byCanonicalName`, the 45 canonical keys, and
    // `byUpperName`, the upper-case spelling of each. Both are asserted, as is the inventory
    // `values` answers.
    val lookup = RollConvention.namedEnum

    forAll(dataName) { (convention: RollConvention, name: String) =>
      withClue(s"$name: ") {
        lookup.byCanonicalName.get(name) shouldBe Some(convention)
        lookup.values.toList should contain(convention)
      }
    }

    lookup.familyName shouldBe "RollConvention"
    lookup.toString shouldBe "NamedEnum[RollConvention]"
    lookup.values.toList shouldBe declarationOrder
    lookup.byCanonicalName should have size 45
    lookup.byUpperName should have size 45
    lookup.byCanonicalName.keySet shouldBe declarationOrder.map(_.name).toSet
    lookup.byUpperName.keySet shouldBe
      declarationOrder.map(_.name.toUpperCase(Locale.ENGLISH)).toSet

    // Seven of the 45 canonical names equal their own upper-case fold - `EOM`, `IMM`, `IMMCAD`,
    // `IMMAUD`, `IMMNZD`, `SFE` and `TBILL`; `None` and the `Day1`..`Day30` and
    // `DayMon`..`DaySun` names do not - so the union of the two key views is smaller than twice
    // 45. The assertion states that union itself rather than its size.
    val lookupAll: Set[String] = lookup.byCanonicalName.keySet ++ lookup.byUpperName.keySet
    lookupAll shouldBe
      (declarationOrder.map(_.name) ++ declarationOrder.map(_.name.toUpperCase(Locale.ENGLISH))).toSet
    lookupAll should contain allOf ("None", "NONE", "Day15", "DAY15", "DayMon", "DAYMON")

    // This family declares no alternate spelling: everything beyond the two key views arrives
    // through the ordered lenient chain or through the one external group.
    lookup.alternateNames shouldBe Map.empty[String, String]

    // The FpML group of external spellings, all 44 rows, asserted row for row. They take part in
    // no lookup - `MON` and `31` resolve through the lenient chain, which happens to accept them
    // - so a lost row would otherwise be invisible. `IMMCAD` and `TBILL` are deliberately
    // absent, FpML defining neither.
    lookup.externalNameGroups shouldBe Set("FpML")
    lookup.externalNames("FpML") shouldBe Some(expectedFpMLNames)
    lookup.externalNames("FpML").map(_.size) shouldBe Some(44)
    lookup.externalNamesRaw("FpML").map(_.size) shouldBe Some(44)
    lookup.externalNamesRaw("FpML").map(_.keySet) shouldBe lookup.externalNames("FpML").map(_.keySet)
    lookup.externalNamesRaw("FpML").map(_("NONE")) shouldBe Some("None")
    lookup.externalNamesRaw("FpML").map(_("31")) shouldBe Some("EOM")
    lookup.externalNamesRaw("FpML").map(_("MON")) shouldBe Some("DayMon")
    lookup.externalNames("FpML").flatMap(_.get("IMMCAD")) shouldBe None
    lookup.externalNames("FpML").flatMap(_.get("TBILL")) shouldBe None

    // A group the family does not publish is an empty answer rather than a failure.
    lookup.externalNames("SWIFT") shouldBe None
    lookup.externalNamesRaw("Rubbish") shouldBe None

    // The ordered lenient table, all 11 rows. The order is part of the data - a later pattern
    // sees what an earlier one produced - and it is load-bearing here rather than incidental: the
    // row for 31 precedes the row for 30 and both precede the row that captures a one- or
    // two-digit day, which is why text naming the 31st reaches `EOM` and is never rewritten to a
    // `Day31` that no member carries.
    lookup.lenientPatterns should have size 11
    lookup.lenientPatterns.map { case (expression, _) => expression.pattern.pattern() } shouldBe
      List(
        "(Day_?)?31",
        "(Day_?)?30",
        "(Day_?)?([1-2]?[0-9])",
        "NONE",
        "(Day_?)?MON",
        "(Day_?)?TUE",
        "(Day_?)?WED",
        "(Day_?)?THU",
        "(Day_?)?FRI",
        "(Day_?)?SAT",
        "(Day_?)?SUN")
    lookup.lenientPatterns.map { case (_, replacement) => replacement } shouldBe
      List(
        "EOM",
        "Day30",
        "Day$2",
        "None",
        "DayMon",
        "DayTue",
        "DayWed",
        "DayThu",
        "DayFri",
        "DaySat",
        "DaySun")

    // The consequence of that order: text naming the 31st resolves to the end of the month,
    // while the 30th keeps its own convention and `Day31` names nothing.
    RollConvention.parse("31") should haveValue(RollConventions.EOM)
    RollConvention.parse("30") should haveValue(RollConventions.DAY_30)
    RollConvention.valueOf("Day31") shouldBe None
  }

  test("test_of_lookup_notFound") {
    // Text naming no member is reported as a value rather than raised. The reason is compared as
    // a member of the closed family of reasons, and the message is asserted because it names the
    // family and the text - the two things a caller has to be told.
    RollConvention.valueOf("Rubbish") shouldBe None
    RollConvention.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
    RollConvention.parse("Rubbish") should
      haveFailureMessageMatching("RollConvention name not found: Rubbish")
    noException should be thrownBy RollConvention.parse("Rubbish")
  }

  test("test_of_lookup_null") {
    // What the two lookups require of their argument: the name is a required parameter of type
    // `String`, so a call that omits it, or offers a number in its place, does not compile.
    assertDoesNotCompile("RollConvention.parse()")
    assertDoesNotCompile("RollConvention.valueOf()")
    assertDoesNotCompile("RollConvention.parse(31)")

    // What text can decide is whether it names a member, so the rest of the case is every
    // spelling of "no usable name" - empty, blank, and several near-misses, including the two
    // that the lenient chain rewrites successfully into a name no member carries. Each resolves
    // to nothing and raises nothing.
    val hostile: List[String] =
      List(
        "",
        "   ",
        "\t\n",
        "Rubbish",
        "null",
        "Day0",
        "Day_0",
        "Day99",
        "Day31 ",
        " EOM",
        "EOM ",
        "IMMUSD",
        "DayMonday")

    hostile.foreach { name =>
      withClue(s"[$name]: ") {
        RollConvention.valueOf(name) shouldBe None
        RollConvention.parse(name) should beFailureWith(FailureReason.PARSING)
        noException should be thrownBy RollConvention.parse(name)
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_lenientLookup_specialNames") {
    // Each lenient row is asserted in three spellings - as written, folded down and folded up -
    // because the patterns are matched insensitively to case and the input is folded to upper
    // case before they are applied.
    dataLenient should have size 11
    forAll(dataLenient) { (name: String, convention: RollConvention) =>
      withClue(s"$name: ") {
        RollConvention.parse(name.toLowerCase(Locale.ENGLISH)) should haveValue(convention)
        RollConvention.parse(name) should haveValue(convention)
        RollConvention.parse(name.toUpperCase(Locale.ENGLISH)) should haveValue(convention)
      }
    }
  }

  test("test_lenientLookup_constants") {
    // Every identifier of the constants holder resolves leniently to the member it names, in its
    // own spelling and folded to lower case, and resolves to that member itself.
    forAll(dataConstantIdentifiers) { (identifier: String, convention: RollConvention) =>
      withClue(s"$identifier: ") {
        RollConvention.parse(identifier) should haveValue(convention)
        RollConvention.parse(identifier.toLowerCase(Locale.ENGLISH)) should haveValue(convention)
        rc(RollConvention.parse(identifier)) should be theSameInstanceAs convention
      }
    }

    // The table is the holder in full: 45 identifiers naming 45 distinct members, in the
    // declaration order of the family.
    dataConstantIdentifiers should have size 45
    dataConstantIdentifiers.map { case (_, convention) => convention }.distinct should have size 45
    dataConstantIdentifiers.map { case (_, convention) => convention }.toList shouldBe
      declarationOrder
  }

  //-------------------------------------------------------------------------
  test("test_equals") {
    val a: RollConvention = RollConventions.EOM
    val b: RollConvention = RollConventions.DAY_1
    val c: RollConvention = RollConventions.DAY_WED

    (a == a) shouldBe true
    (a == b) shouldBe false
    (a == c) shouldBe false

    (b == a) shouldBe false
    (b == b) shouldBe true
    (b == c) shouldBe false

    (c == a) shouldBe false
    (c == b) shouldBe false
    (c == c) shouldBe true

    a.hashCode shouldBe a.hashCode

    // The companion publishes one equality-bearing instance - an ordering that is also a hashing
    // - so the equality, the hashing and the ordering agree with each other and with `==`, and
    // `Show` agrees with `name`. The three members above are asked all of it.
    List(a, b, c).foreach { left =>
      List(a, b, c).foreach { right =>
        val sameValue: Boolean = left == right
        withClue(s"${left.name} against ${right.name}: ") {
          Eq[RollConvention].eqv(left, right) shouldBe sameValue
          Hash[RollConvention].eqv(left, right) shouldBe sameValue
          (Order[RollConvention].compare(left, right) == 0) shouldBe sameValue
          if (sameValue) {
            Hash[RollConvention].hash(left) shouldBe Hash[RollConvention].hash(right)
          } else {
            Order[RollConvention].compare(left, right) should not be 0
          }
        }
      }
      Show[RollConvention].show(left) shouldBe left.name
    }

    // The ordering is by name, and by name alone - alphabetically `Day1` precedes `DayWed`, which
    // precedes `EOM`. That is deliberately not the declaration order of the family, in which
    // `EOM` is the second member and both day-based members come after every rule-based one, so
    // these three comparisons are also what distinguishes the two orders.
    Order[RollConvention].compare(b, c) should be < 0
    Order[RollConvention].compare(c, a) should be < 0
    Order[RollConvention].compare(a, b) should be > 0
    List(c, b, a).sorted(Order[RollConvention].toOrdering).map(_.name) shouldBe
      List("Day1", "DayWed", "EOM")
    declarationOrder.indexOf(a) should be < declarationOrder.indexOf(b)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The family is closed, so its inventory is itself a subject: the members, the identifiers
    // that name them, and the round trip through the name.

    // Exactly 45 members, distinct as values and distinct by name, in declaration order rather
    // than the alphabetical order the `Order` instance imposes.
    RollConvention.values.toList should have size 45
    RollConvention.values.toList.distinct should have size 45
    RollConvention.values.toList.map(_.name).distinct should have size 45
    RollConvention.values.toList shouldBe declarationOrder

    // Every member round-trips through its own name, by both entry points, and renders as it.
    RollConvention.values.toList.foreach { convention =>
      withClue(s"${convention.name}: ") {
        RollConvention.valueOf(convention.name) shouldBe Some(convention)
        RollConvention.parse(convention.name) should haveValue(convention)
        Show[RollConvention].show(convention) shouldBe convention.name
        convention.toString shouldBe convention.name
      }
    }

    // Each of the 45 constants of the holder is the very member of the family, not a copy, so a
    // call site reading the constant and one reading the member are indistinguishable.
    declarationOrder.zip(RollConvention.values.toList).foreach {
      case (constant, member) =>
        withClue(s"${constant.name}: ")(constant should be theSameInstanceAs member)
    }

    // The eight rule-based members are reachable as members as well as constants, which is what
    // makes the holder a second name for them rather than a second set of values.
    RollConventions.NONE should be theSameInstanceAs RollConvention.NONE
    RollConventions.EOM should be theSameInstanceAs RollConvention.EOM
    RollConventions.IMM should be theSameInstanceAs RollConvention.IMM
    RollConventions.IMMCAD should be theSameInstanceAs RollConvention.IMMCAD
    RollConventions.IMMAUD should be theSameInstanceAs RollConvention.IMMAUD
    RollConventions.IMMNZD should be theSameInstanceAs RollConvention.IMMNZD
    RollConventions.SFE should be theSameInstanceAs RollConvention.SFE
    RollConventions.TBILL should be theSameInstanceAs RollConvention.TBILL

    // The member whose canonical name is `None` is declared as `NONE`, so that it cannot shadow
    // `scala.None` inside the companion. The rename is of the Scala identifier only: the name,
    // what `Show` prints and what the codec writes are all `None`.
    RollConvention.NONE.name shouldBe "None"
    RollConvention.valueOf("None") shouldBe Some(RollConventions.NONE)

    // The day-of-month accessor over the whole family: its own day for a day-of-month member, 31
    // for `EOM`, and zero for every member whose rule is not a day of the month.
    RollConventions.EOM.dayOfMonth shouldBe 31
    (1 to 30).foreach { day =>
      withClue(s"Day$day: ")(rc(RollConvention.ofDayOfMonth(day)).dayOfMonth shouldBe day)
    }
    List(
      RollConventions.NONE,
      RollConventions.IMM,
      RollConventions.IMMCAD,
      RollConventions.IMMAUD,
      RollConventions.IMMNZD,
      RollConventions.SFE,
      RollConventions.TBILL,
      RollConventions.DAY_MON,
      RollConventions.DAY_SUN).foreach { convention =>
      withClue(s"${convention.name}: ")(convention.dayOfMonth shouldBe 0)
    }
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // The codec the companion publishes is the one in implicit scope, and a document is the bare
    // canonical name rather than an object. Three conventions are asserted individually and then
    // all 45, the codec being one instance shared by the family.
    val codec: Codec[RollConvention] = implicitly[Codec[RollConvention]]
    codec should be theSameInstanceAs RollConvention.codec

    List(RollConventions.EOM, RollConventions.DAY_2, RollConventions.DAY_THU).foreach { convention =>
      withClue(s"${convention.name}: ") {
        val encoded: Json = convention.asJson
        encoded shouldBe Json.fromString(convention.name)
        encoded.isString shouldBe true
        encoded.isObject shouldBe false
        encoded.noSpaces shouldBe s""""${convention.name}""""
        encoded.as[RollConvention] shouldBe Right(convention)
      }
    }

    RollConvention.values.toList.foreach { convention =>
      withClue(s"${convention.name}: ") {
        convention.asJson.as[RollConvention] shouldBe Right(convention)
        rc(convention.asJson.as[RollConvention]) should be theSameInstanceAs convention
      }
    }

    // Text naming no convention is rejected by the reader, as is a document of the wrong JSON
    // type - the codec reads a string and nothing else, which is what keeps an unrecognised name
    // a decoding failure rather than an exception.
    Json.fromString("Rubbish").as[RollConvention].isLeft shouldBe true
    Json.fromInt(31).as[RollConvention].isLeft shouldBe true
    Json.obj("name" -> Json.fromString("EOM")).as[RollConvention].isLeft shouldBe true

    // The reader is as lenient as `parse`, so a document written by hand, or in the FpML
    // vocabulary, still reads.
    Json.fromString("Day_31").as[RollConvention] shouldBe Right(RollConventions.EOM)
    Json.fromString("15").as[RollConvention] shouldBe Right(RollConventions.DAY_15)
    Json.fromString("thu").as[RollConvention] shouldBe Right(RollConventions.DAY_THU)
  }

  test("test_jodaConvert") {
    // Rendering and recovery agree: a convention renders as its name through `Show` and
    // `toString`, and `parse` recovers the same member from either rendering. Two conventions are
    // asserted individually and then the whole family.
    List(RollConventions.NONE, RollConventions.EOM).foreach { convention =>
      withClue(s"${convention.name}: ") {
        Show[RollConvention].show(convention) shouldBe convention.name
        convention.toString shouldBe convention.name
        RollConvention.parse(convention.name) should haveValue(convention)
        rc(RollConvention.parse(Show[RollConvention].show(convention))) should
          be theSameInstanceAs convention
      }
    }

    RollConvention.values.toList.foreach { convention =>
      withClue(s"${convention.name}: ") {
        rc(RollConvention.parse(convention.toString)) should be theSameInstanceAs convention
        rc(RollConvention.parse(Show[RollConvention].show(convention))) should
          be theSameInstanceAs convention
      }
    }
  }
}

/**
 * The tables the tests above are driven by, together with the inventories the closed family is
 * asserted against.
 *
 * They live in a companion rather than in the class so that each test reads as the property it
 * asserts, and the object extends the table support because building a `Table` needs it.
 *
 * The eight conventions and the three frequencies are bound to short names here so that the four
 * large tables read as tables - a row of dates is checkable by eye, a row of qualified paths is
 * not. Each name stands for the constant of [[RollConventions]] or [[Frequency]] it is assigned
 * from, and for nothing else.
 */
private[schedule] object RollConventionSpec extends TableDrivenPropertyChecks {

  private val NONE: RollConvention = RollConventions.NONE
  private val EOM: RollConvention = RollConventions.EOM
  private val IMM: RollConvention = RollConventions.IMM
  private val IMMCAD: RollConvention = RollConventions.IMMCAD
  private val IMMAUD: RollConvention = RollConventions.IMMAUD
  private val IMMNZD: RollConvention = RollConventions.IMMNZD
  private val SFE: RollConvention = RollConventions.SFE
  private val TBILL: RollConvention = RollConventions.TBILL

  private val P1D: Frequency = Frequency.P1D
  private val P1M: Frequency = Frequency.P1M
  private val P3M: Frequency = Frequency.P3M

  //-------------------------------------------------------------------------
  /**
   * The eight rule-based conventions, in declaration order.
   *
   * The day-of-month and day-of-week members are not part of this table; the cases naming them
   * drive them from the two factories instead.
   */
  val dataTypes: TableFor1[RollConvention] = Table(
    "convention",
    NONE,
    EOM,
    IMM,
    IMMCAD,
    IMMAUD,
    IMMNZD,
    SFE,
    TBILL
  )

  //-------------------------------------------------------------------------
  /**
   * The 51 adjustment rows: a convention, a date, and the date the convention adjusts it to.
   *
   * The two TBILL rows marked below land on a Tuesday because the Monday of that week is a New
   * York holiday.
   */
  val dataAdjust: TableFor3[RollConvention, LocalDate, LocalDate] = Table(
    ("convention", "input", "expected"),
    (EOM, date(2014, AUGUST, 1), date(2014, AUGUST, 31)),
    (EOM, date(2014, AUGUST, 30), date(2014, AUGUST, 31)),
    (EOM, date(2014, SEPTEMBER, 1), date(2014, SEPTEMBER, 30)),
    (EOM, date(2014, SEPTEMBER, 30), date(2014, SEPTEMBER, 30)),
    (EOM, date(2014, FEBRUARY, 1), date(2014, FEBRUARY, 28)),
    (IMM, date(2014, AUGUST, 1), date(2014, AUGUST, 20)),
    (IMM, date(2014, AUGUST, 6), date(2014, AUGUST, 20)),
    (IMM, date(2014, AUGUST, 19), date(2014, AUGUST, 20)),
    (IMM, date(2014, AUGUST, 20), date(2014, AUGUST, 20)),
    (IMM, date(2014, AUGUST, 21), date(2014, AUGUST, 20)),
    (IMM, date(2014, AUGUST, 31), date(2014, AUGUST, 20)),
    (IMM, date(2014, SEPTEMBER, 1), date(2014, SEPTEMBER, 17)),
    (IMMCAD, date(2014, AUGUST, 1), date(2014, AUGUST, 18)),
    (IMMCAD, date(2014, AUGUST, 6), date(2014, AUGUST, 18)),
    (IMMCAD, date(2014, AUGUST, 7), date(2014, AUGUST, 18)),
    (IMMCAD, date(2014, AUGUST, 8), date(2014, AUGUST, 18)),
    (IMMCAD, date(2014, AUGUST, 31), date(2014, AUGUST, 18)),
    (IMMCAD, date(2014, SEPTEMBER, 1), date(2014, SEPTEMBER, 15)),
    (IMMAUD, date(2014, AUGUST, 1), date(2014, AUGUST, 7)),
    (IMMAUD, date(2014, AUGUST, 6), date(2014, AUGUST, 7)),
    (IMMAUD, date(2014, AUGUST, 7), date(2014, AUGUST, 7)),
    (IMMAUD, date(2014, AUGUST, 8), date(2014, AUGUST, 7)),
    (IMMAUD, date(2014, AUGUST, 31), date(2014, AUGUST, 7)),
    (IMMAUD, date(2014, SEPTEMBER, 1), date(2014, SEPTEMBER, 11)),
    (IMMAUD, date(2014, OCTOBER, 1), date(2014, OCTOBER, 9)),
    (IMMAUD, date(2014, NOVEMBER, 1), date(2014, NOVEMBER, 13)),
    (IMMNZD, date(2014, AUGUST, 1), date(2014, AUGUST, 13)),
    (IMMNZD, date(2014, AUGUST, 6), date(2014, AUGUST, 13)),
    (IMMNZD, date(2014, AUGUST, 12), date(2014, AUGUST, 13)),
    (IMMNZD, date(2014, AUGUST, 13), date(2014, AUGUST, 13)),
    (IMMNZD, date(2014, AUGUST, 14), date(2014, AUGUST, 13)),
    (IMMNZD, date(2014, AUGUST, 31), date(2014, AUGUST, 13)),
    (IMMNZD, date(2014, SEPTEMBER, 1), date(2014, SEPTEMBER, 10)),
    (IMMNZD, date(2014, OCTOBER, 1), date(2014, OCTOBER, 15)),
    (IMMNZD, date(2014, NOVEMBER, 1), date(2014, NOVEMBER, 12)),
    (SFE, date(2014, AUGUST, 1), date(2014, AUGUST, 8)),
    (SFE, date(2014, AUGUST, 6), date(2014, AUGUST, 8)),
    (SFE, date(2014, AUGUST, 7), date(2014, AUGUST, 8)),
    (SFE, date(2014, AUGUST, 8), date(2014, AUGUST, 8)),
    (SFE, date(2014, AUGUST, 31), date(2014, AUGUST, 8)),
    (SFE, date(2014, SEPTEMBER, 1), date(2014, SEPTEMBER, 12)),
    (SFE, date(2014, OCTOBER, 1), date(2014, OCTOBER, 10)),
    (SFE, date(2014, NOVEMBER, 1), date(2014, NOVEMBER, 14)),
    (TBILL, date(2014, AUGUST, 1), date(2014, AUGUST, 4)),
    (TBILL, date(2014, AUGUST, 2), date(2014, AUGUST, 4)),
    (TBILL, date(2014, AUGUST, 3), date(2014, AUGUST, 4)),
    (TBILL, date(2014, AUGUST, 4), date(2014, AUGUST, 4)),
    (TBILL, date(2014, AUGUST, 5), date(2014, AUGUST, 11)),
    (TBILL, date(2014, AUGUST, 7), date(2014, AUGUST, 11)),
    // Tuesday due to holiday
    (TBILL, date(2018, AUGUST, 31), date(2018, SEPTEMBER, 4)),
    // Tuesday due to holiday
    (TBILL, date(2018, SEPTEMBER, 1), date(2018, SEPTEMBER, 4))
  )

  //-------------------------------------------------------------------------
  /** The 17 match rows: a convention, a date, and whether the date already satisfies the rule. */
  val dataMatches: TableFor3[RollConvention, LocalDate, Boolean] = Table(
    ("convention", "input", "expected"),
    (EOM, date(2014, AUGUST, 1), false),
    (EOM, date(2014, AUGUST, 30), false),
    (EOM, date(2014, AUGUST, 31), true),
    (EOM, date(2014, SEPTEMBER, 1), false),
    (EOM, date(2014, SEPTEMBER, 30), true),
    (IMM, date(2014, SEPTEMBER, 16), false),
    (IMM, date(2014, SEPTEMBER, 17), true),
    (IMM, date(2014, SEPTEMBER, 18), false),
    (IMMAUD, date(2014, SEPTEMBER, 10), false),
    (IMMAUD, date(2014, SEPTEMBER, 11), true),
    (IMMAUD, date(2014, SEPTEMBER, 12), false),
    (IMMNZD, date(2014, SEPTEMBER, 9), false),
    (IMMNZD, date(2014, SEPTEMBER, 10), true),
    (IMMNZD, date(2014, SEPTEMBER, 11), false),
    (SFE, date(2014, SEPTEMBER, 11), false),
    (SFE, date(2014, SEPTEMBER, 12), true),
    (SFE, date(2014, SEPTEMBER, 13), false)
  )

  //-------------------------------------------------------------------------
  /**
   * The 62 `next` rows: a convention, a date, a frequency, and the date `next` answers.
   *
   * The one-day rows are the ones that exercise the correction the month-based default applies:
   * a frequency shorter than the convention's own cycle lands on or before the date supplied, so
   * a month is added instead and the result is always strictly after it.
   */
  val dataNext: TableFor4[RollConvention, LocalDate, Frequency, LocalDate] = Table(
    ("convention", "input", "frequency", "expected"),
    (EOM, date(2014, AUGUST, 1), P1M, date(2014, SEPTEMBER, 30)),
    (EOM, date(2014, AUGUST, 30), P1M, date(2014, SEPTEMBER, 30)),
    (EOM, date(2014, AUGUST, 31), P1M, date(2014, SEPTEMBER, 30)),
    (EOM, date(2014, SEPTEMBER, 1), P1M, date(2014, OCTOBER, 31)),
    (EOM, date(2014, SEPTEMBER, 30), P1M, date(2014, OCTOBER, 31)),
    (EOM, date(2014, JANUARY, 1), P1M, date(2014, FEBRUARY, 28)),
    (EOM, date(2014, FEBRUARY, 1), P1M, date(2014, MARCH, 31)),
    (EOM, date(2014, AUGUST, 1), P3M, date(2014, NOVEMBER, 30)),
    (EOM, date(2014, AUGUST, 1), P1D, date(2014, AUGUST, 31)),
    (EOM, date(2014, AUGUST, 30), P1D, date(2014, AUGUST, 31)),
    (EOM, date(2014, AUGUST, 31), P1D, date(2014, SEPTEMBER, 30)),
    (EOM, date(2014, JANUARY, 1), P1D, date(2014, JANUARY, 31)),
    (EOM, date(2014, JANUARY, 31), P1D, date(2014, FEBRUARY, 28)),
    (EOM, date(2014, FEBRUARY, 1), P1D, date(2014, FEBRUARY, 28)),
    (IMM, date(2014, AUGUST, 1), P1M, date(2014, SEPTEMBER, 17)),
    (IMM, date(2014, AUGUST, 31), P1M, date(2014, SEPTEMBER, 17)),
    (IMM, date(2014, SEPTEMBER, 1), P1M, date(2014, OCTOBER, 15)),
    (IMM, date(2014, SEPTEMBER, 30), P1M, date(2014, OCTOBER, 15)),
    (IMM, date(2014, AUGUST, 1), P1D, date(2014, AUGUST, 20)),
    (IMM, date(2014, AUGUST, 19), P1D, date(2014, AUGUST, 20)),
    (IMM, date(2014, AUGUST, 20), P1D, date(2014, SEPTEMBER, 17)),
    (IMM, date(2014, AUGUST, 31), P1D, date(2014, SEPTEMBER, 17)),
    (IMM, date(2014, SEPTEMBER, 1), P1D, date(2014, SEPTEMBER, 17)),
    (IMM, date(2014, SEPTEMBER, 16), P1D, date(2014, SEPTEMBER, 17)),
    (IMM, date(2014, SEPTEMBER, 17), P1D, date(2014, OCTOBER, 15)),
    (IMM, date(2014, SEPTEMBER, 30), P1D, date(2014, OCTOBER, 15)),
    (IMMAUD, date(2014, AUGUST, 1), P1M, date(2014, SEPTEMBER, 11)),
    (IMMAUD, date(2014, AUGUST, 31), P1M, date(2014, SEPTEMBER, 11)),
    (IMMAUD, date(2014, SEPTEMBER, 1), P1M, date(2014, OCTOBER, 9)),
    (IMMAUD, date(2014, SEPTEMBER, 30), P1M, date(2014, OCTOBER, 9)),
    (IMMAUD, date(2014, AUGUST, 1), P1D, date(2014, AUGUST, 7)),
    (IMMAUD, date(2014, AUGUST, 6), P1D, date(2014, AUGUST, 7)),
    (IMMAUD, date(2014, AUGUST, 7), P1D, date(2014, SEPTEMBER, 11)),
    (IMMAUD, date(2014, AUGUST, 31), P1D, date(2014, SEPTEMBER, 11)),
    (IMMAUD, date(2014, SEPTEMBER, 1), P1D, date(2014, SEPTEMBER, 11)),
    (IMMAUD, date(2014, SEPTEMBER, 10), P1D, date(2014, SEPTEMBER, 11)),
    (IMMAUD, date(2014, SEPTEMBER, 11), P1D, date(2014, OCTOBER, 9)),
    (IMMAUD, date(2014, SEPTEMBER, 30), P1D, date(2014, OCTOBER, 9)),
    (IMMNZD, date(2014, AUGUST, 1), P1M, date(2014, SEPTEMBER, 10)),
    (IMMNZD, date(2014, AUGUST, 31), P1M, date(2014, SEPTEMBER, 10)),
    (IMMNZD, date(2014, SEPTEMBER, 1), P1M, date(2014, OCTOBER, 15)),
    (IMMNZD, date(2014, SEPTEMBER, 30), P1M, date(2014, OCTOBER, 15)),
    (IMMNZD, date(2014, AUGUST, 1), P1D, date(2014, AUGUST, 13)),
    (IMMNZD, date(2014, AUGUST, 12), P1D, date(2014, AUGUST, 13)),
    (IMMNZD, date(2014, AUGUST, 13), P1D, date(2014, SEPTEMBER, 10)),
    (IMMNZD, date(2014, AUGUST, 31), P1D, date(2014, SEPTEMBER, 10)),
    (IMMNZD, date(2014, SEPTEMBER, 1), P1D, date(2014, SEPTEMBER, 10)),
    (IMMNZD, date(2014, SEPTEMBER, 9), P1D, date(2014, SEPTEMBER, 10)),
    (IMMNZD, date(2014, SEPTEMBER, 10), P1D, date(2014, OCTOBER, 15)),
    (IMMNZD, date(2014, SEPTEMBER, 30), P1D, date(2014, OCTOBER, 15)),
    (SFE, date(2014, AUGUST, 1), P1M, date(2014, SEPTEMBER, 12)),
    (SFE, date(2014, AUGUST, 31), P1M, date(2014, SEPTEMBER, 12)),
    (SFE, date(2014, SEPTEMBER, 1), P1M, date(2014, OCTOBER, 10)),
    (SFE, date(2014, SEPTEMBER, 30), P1M, date(2014, OCTOBER, 10)),
    (SFE, date(2014, AUGUST, 1), P1D, date(2014, AUGUST, 8)),
    (SFE, date(2014, AUGUST, 7), P1D, date(2014, AUGUST, 8)),
    (SFE, date(2014, AUGUST, 8), P1D, date(2014, SEPTEMBER, 12)),
    (SFE, date(2014, AUGUST, 31), P1D, date(2014, SEPTEMBER, 12)),
    (SFE, date(2014, SEPTEMBER, 1), P1D, date(2014, SEPTEMBER, 12)),
    (SFE, date(2014, SEPTEMBER, 11), P1D, date(2014, SEPTEMBER, 12)),
    (SFE, date(2014, SEPTEMBER, 12), P1D, date(2014, OCTOBER, 10)),
    (SFE, date(2014, SEPTEMBER, 30), P1D, date(2014, OCTOBER, 10))
  )

  //-------------------------------------------------------------------------
  /** The 57 `previous` rows: a convention, a date, a frequency, and the date `previous` answers. */
  val dataPrevious: TableFor4[RollConvention, LocalDate, Frequency, LocalDate] = Table(
    ("convention", "input", "frequency", "expected"),
    (EOM, date(2014, OCTOBER, 1), P1M, date(2014, SEPTEMBER, 30)),
    (EOM, date(2014, OCTOBER, 31), P1M, date(2014, SEPTEMBER, 30)),
    (EOM, date(2014, NOVEMBER, 1), P1M, date(2014, OCTOBER, 31)),
    (EOM, date(2014, NOVEMBER, 30), P1M, date(2014, OCTOBER, 31)),
    (EOM, date(2014, MARCH, 1), P1M, date(2014, FEBRUARY, 28)),
    (EOM, date(2014, APRIL, 1), P1M, date(2014, MARCH, 31)),
    (EOM, date(2014, NOVEMBER, 1), P3M, date(2014, AUGUST, 31)),
    (EOM, date(2014, OCTOBER, 1), P1D, date(2014, SEPTEMBER, 30)),
    (EOM, date(2014, OCTOBER, 30), P1D, date(2014, SEPTEMBER, 30)),
    (IMM, date(2014, OCTOBER, 1), P1M, date(2014, SEPTEMBER, 17)),
    (IMM, date(2014, OCTOBER, 31), P1M, date(2014, SEPTEMBER, 17)),
    (IMM, date(2014, NOVEMBER, 1), P1M, date(2014, OCTOBER, 15)),
    (IMM, date(2014, NOVEMBER, 30), P1M, date(2014, OCTOBER, 15)),
    (IMM, date(2014, AUGUST, 1), P1D, date(2014, JULY, 16)),
    (IMM, date(2014, AUGUST, 20), P1D, date(2014, JULY, 16)),
    (IMM, date(2014, AUGUST, 21), P1D, date(2014, AUGUST, 20)),
    (IMM, date(2014, AUGUST, 31), P1D, date(2014, AUGUST, 20)),
    (IMM, date(2014, SEPTEMBER, 1), P1D, date(2014, AUGUST, 20)),
    (IMM, date(2014, SEPTEMBER, 17), P1D, date(2014, AUGUST, 20)),
    (IMM, date(2014, SEPTEMBER, 18), P1D, date(2014, SEPTEMBER, 17)),
    (IMM, date(2014, SEPTEMBER, 30), P1D, date(2014, SEPTEMBER, 17)),
    (IMMAUD, date(2014, OCTOBER, 1), P1M, date(2014, SEPTEMBER, 11)),
    (IMMAUD, date(2014, OCTOBER, 31), P1M, date(2014, SEPTEMBER, 11)),
    (IMMAUD, date(2014, NOVEMBER, 1), P1M, date(2014, OCTOBER, 9)),
    (IMMAUD, date(2014, NOVEMBER, 30), P1M, date(2014, OCTOBER, 9)),
    (IMMAUD, date(2014, SEPTEMBER, 1), P1D, date(2014, AUGUST, 7)),
    (IMMAUD, date(2014, SEPTEMBER, 11), P1D, date(2014, AUGUST, 7)),
    (IMMAUD, date(2014, SEPTEMBER, 12), P1D, date(2014, SEPTEMBER, 11)),
    (IMMAUD, date(2014, SEPTEMBER, 30), P1D, date(2014, SEPTEMBER, 11)),
    (IMMAUD, date(2014, OCTOBER, 1), P1D, date(2014, SEPTEMBER, 11)),
    (IMMAUD, date(2014, OCTOBER, 9), P1D, date(2014, SEPTEMBER, 11)),
    (IMMAUD, date(2014, OCTOBER, 10), P1D, date(2014, OCTOBER, 9)),
    (IMMAUD, date(2014, OCTOBER, 30), P1D, date(2014, OCTOBER, 9)),
    (IMMNZD, date(2014, OCTOBER, 1), P1M, date(2014, SEPTEMBER, 10)),
    (IMMNZD, date(2014, OCTOBER, 31), P1M, date(2014, SEPTEMBER, 10)),
    (IMMNZD, date(2014, NOVEMBER, 1), P1M, date(2014, OCTOBER, 15)),
    (IMMNZD, date(2014, NOVEMBER, 30), P1M, date(2014, OCTOBER, 15)),
    (IMMNZD, date(2014, SEPTEMBER, 1), P1D, date(2014, AUGUST, 13)),
    (IMMNZD, date(2014, SEPTEMBER, 10), P1D, date(2014, AUGUST, 13)),
    (IMMNZD, date(2014, SEPTEMBER, 11), P1D, date(2014, SEPTEMBER, 10)),
    (IMMNZD, date(2014, SEPTEMBER, 30), P1D, date(2014, SEPTEMBER, 10)),
    (IMMNZD, date(2014, OCTOBER, 1), P1D, date(2014, SEPTEMBER, 10)),
    (IMMNZD, date(2014, OCTOBER, 15), P1D, date(2014, SEPTEMBER, 10)),
    (IMMNZD, date(2014, OCTOBER, 16), P1D, date(2014, OCTOBER, 15)),
    (IMMNZD, date(2014, OCTOBER, 30), P1D, date(2014, OCTOBER, 15)),
    (SFE, date(2014, OCTOBER, 1), P1M, date(2014, SEPTEMBER, 12)),
    (SFE, date(2014, OCTOBER, 31), P1M, date(2014, SEPTEMBER, 12)),
    (SFE, date(2014, NOVEMBER, 1), P1M, date(2014, OCTOBER, 10)),
    (SFE, date(2014, NOVEMBER, 30), P1M, date(2014, OCTOBER, 10)),
    (SFE, date(2014, SEPTEMBER, 1), P1D, date(2014, AUGUST, 8)),
    (SFE, date(2014, SEPTEMBER, 12), P1D, date(2014, AUGUST, 8)),
    (SFE, date(2014, SEPTEMBER, 13), P1D, date(2014, SEPTEMBER, 12)),
    (SFE, date(2014, SEPTEMBER, 30), P1D, date(2014, SEPTEMBER, 12)),
    (SFE, date(2014, OCTOBER, 1), P1D, date(2014, SEPTEMBER, 12)),
    (SFE, date(2014, OCTOBER, 10), P1D, date(2014, SEPTEMBER, 12)),
    (SFE, date(2014, OCTOBER, 11), P1D, date(2014, OCTOBER, 10)),
    (SFE, date(2014, OCTOBER, 30), P1D, date(2014, OCTOBER, 10))
  )

  //-------------------------------------------------------------------------
  /**
   * The 45 members of the family, in declaration order.
   *
   * Named through the constants of [[RollConventions]] rather than through the members of the
   * companion, so that the list is simultaneously the roster of the family and the roster of the
   * holder: `coverage` compares it against `RollConvention.values` element by element, which
   * asserts both that the family is closed at 45 and that each constant is the very member.
   */
  val declarationOrder: List[RollConvention] =
    List(
      RollConventions.NONE,
      RollConventions.EOM,
      RollConventions.IMM,
      RollConventions.IMMCAD,
      RollConventions.IMMAUD,
      RollConventions.IMMNZD,
      RollConventions.SFE,
      RollConventions.TBILL,
      RollConventions.DAY_1,
      RollConventions.DAY_2,
      RollConventions.DAY_3,
      RollConventions.DAY_4,
      RollConventions.DAY_5,
      RollConventions.DAY_6,
      RollConventions.DAY_7,
      RollConventions.DAY_8,
      RollConventions.DAY_9,
      RollConventions.DAY_10,
      RollConventions.DAY_11,
      RollConventions.DAY_12,
      RollConventions.DAY_13,
      RollConventions.DAY_14,
      RollConventions.DAY_15,
      RollConventions.DAY_16,
      RollConventions.DAY_17,
      RollConventions.DAY_18,
      RollConventions.DAY_19,
      RollConventions.DAY_20,
      RollConventions.DAY_21,
      RollConventions.DAY_22,
      RollConventions.DAY_23,
      RollConventions.DAY_24,
      RollConventions.DAY_25,
      RollConventions.DAY_26,
      RollConventions.DAY_27,
      RollConventions.DAY_28,
      RollConventions.DAY_29,
      RollConventions.DAY_30,
      RollConventions.DAY_MON,
      RollConventions.DAY_TUE,
      RollConventions.DAY_WED,
      RollConventions.DAY_THU,
      RollConventions.DAY_FRI,
      RollConventions.DAY_SAT,
      RollConventions.DAY_SUN)

  //-------------------------------------------------------------------------
  /**
   * The FpML group of external names, all 44 rows.
   *
   * Written out here independently of the production file - which holds the same rows - so that a
   * mistake in either is a disagreement between the two rather than a self-consistent error. The
   * row `31` maps to `EOM`, because FpML has no end-of-month spelling of its own, and `IMMCAD`
   * and `TBILL` appear nowhere, because FpML defines neither.
   */
  val expectedFpMLNames: Map[String, RollConvention] =
    Map(
      "NONE" -> RollConventions.NONE,
      "EOM" -> RollConventions.EOM,
      "IMM" -> RollConventions.IMM,
      "IMMAUD" -> RollConventions.IMMAUD,
      "IMMNZD" -> RollConventions.IMMNZD,
      "SFE" -> RollConventions.SFE,
      "1" -> RollConventions.DAY_1,
      "2" -> RollConventions.DAY_2,
      "3" -> RollConventions.DAY_3,
      "4" -> RollConventions.DAY_4,
      "5" -> RollConventions.DAY_5,
      "6" -> RollConventions.DAY_6,
      "7" -> RollConventions.DAY_7,
      "8" -> RollConventions.DAY_8,
      "9" -> RollConventions.DAY_9,
      "10" -> RollConventions.DAY_10,
      "11" -> RollConventions.DAY_11,
      "12" -> RollConventions.DAY_12,
      "13" -> RollConventions.DAY_13,
      "14" -> RollConventions.DAY_14,
      "15" -> RollConventions.DAY_15,
      "16" -> RollConventions.DAY_16,
      "17" -> RollConventions.DAY_17,
      "18" -> RollConventions.DAY_18,
      "19" -> RollConventions.DAY_19,
      "20" -> RollConventions.DAY_20,
      "21" -> RollConventions.DAY_21,
      "22" -> RollConventions.DAY_22,
      "23" -> RollConventions.DAY_23,
      "24" -> RollConventions.DAY_24,
      "25" -> RollConventions.DAY_25,
      "26" -> RollConventions.DAY_26,
      "27" -> RollConventions.DAY_27,
      "28" -> RollConventions.DAY_28,
      "29" -> RollConventions.DAY_29,
      "30" -> RollConventions.DAY_30,
      "31" -> RollConventions.EOM,
      "MON" -> RollConventions.DAY_MON,
      "TUE" -> RollConventions.DAY_TUE,
      "WED" -> RollConventions.DAY_WED,
      "THU" -> RollConventions.DAY_THU,
      "FRI" -> RollConventions.DAY_FRI,
      "SAT" -> RollConventions.DAY_SAT,
      "SUN" -> RollConventions.DAY_SUN)

  //-------------------------------------------------------------------------
  /**
   * The dates the three calendar-bearing conventions are checked against their own rules on.
   *
   * Five days of every month - the 1st, 8th, 15th, 20th and 28th - across five years, so that
   * the third Wednesday, the second Friday and the next Monday are each approached from before,
   * on and after their own day. The four dates appended are the ones the adjustment rows single
   * out, including the two New York holiday cases of September 2018.
   */
  val calendarProbes: List[LocalDate] =
    (2014 to 2018).toList.flatMap { year =>
      (1 to 12).toList.flatMap { month =>
        List(1, 8, 15, 20, 28).map(day => date(year, month, day))
      }
    } ++
      List(
        date(2014, AUGUST, 7),
        date(2014, AUGUST, 31),
        date(2018, AUGUST, 31),
        date(2018, SEPTEMBER, 1))

  /** The seven days of the week, in the order `java.time` numbers them. */
  val daysOfWeek: List[DayOfWeek] = DayOfWeek.values().toList
}
