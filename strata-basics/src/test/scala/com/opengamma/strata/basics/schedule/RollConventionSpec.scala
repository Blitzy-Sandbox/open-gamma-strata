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
 * Test [[RollConvention]], ported from the Java `RollConventionTest`.
 *
 * This is a one-to-one port: each of the Java class's thirty-nine test methods has a test of the
 * same name here, in the same order, and no test has been added or split off. Eleven of those
 * methods were parameterised over seven data providers; each provider is transcribed row for row
 * into one table, and the row loop lives inside the single test that the Java method became, so
 * the method-level traceability recorded in `manifest/java-test-mapping.csv` stays exact:
 *
 *   - `data_types` - the 8 rule-based conventions, driving `test_null`;
 *   - `data_adjust` - '''51''' rows, driving `test_adjust`;
 *   - `data_matches` - '''17''' rows;
 *   - `data_next` - '''62''' rows;
 *   - `data_previous` - '''57''' rows;
 *   - `data_name` - 8 rows, driving `test_name`, `test_toString`, `test_of_lookup`,
 *     `test_lenientLookup_standardNames` and `test_extendedEnum`;
 *   - `data_lenient` - '''11''' rows.
 *
 * Because the roster of tests is closed, what a reader might expect to be a test of its own is
 * folded into the test whose subject it belongs to. The agreement of the three calendar-bearing
 * conventions with the calendars they hold is part of `test_adjust`, which is where their dates
 * are asserted; the 44 external FpML spellings and the 11 ordered lenient rewrites that the
 * configuration resource used to declare are asserted in `test_extendedEnum`, which is the test
 * that read that registry in Java.
 *
 * ===How the shape of the port changes the assertions===
 *
 *   - The throwing `RollConvention.of(name)` became two members:
 *     [[RollConvention.valueOf]], the exact lookup over the canonical and upper-case keys,
 *     returning an `Option`, and [[RollConvention.parse]], the lenient lookup, returning
 *     `EitherNec[Failure, _]`. Both are asserted wherever Java asserted `of`, so the two entry
 *     points cannot drift apart. `RollConvention.ofDayOfMonth` likewise reports rejection as a
 *     `Left` carrying `Failure.Invalid` instead of raising.
 *   - `extendedEnum()` - the registry the Java type assembled by reflecting over an enum, merging
 *     a second lookup class and reading a configuration resource off the class path - became the
 *     `NamedEnum[RollConvention]` instance the companion publishes. `test_extendedEnum` asserts
 *     the canonical map the Java method read (`lookupAll`) and then the transcribed external and
 *     lenient tables, because in this port those rows are code and a lost row would otherwise be
 *     invisible.
 *   - `test_null` asserted that each convention rejected an absent date. The guard behind that
 *     rejection has no target here, because the argument it guarded against cannot be expressed:
 *     a date and a frequency are required parameters of required types. The case is asserted as a
 *     compile-time proof that both arguments are required, and then as the totality the Java
 *     guards were protecting - every convention answers for a valid date, and `next` and
 *     `previous` always move strictly forward and strictly back. No `null` is written anywhere in
 *     this file.
 *   - `test_lenientLookup_constants` reflected over the public constants of `RollConventions` with
 *     `java.lang.reflect`. This port performs no reflection, so the reflective sweep has no
 *     target: the 45 identifiers are written out as an explicit table, which states exactly what
 *     the sweep would have discovered and additionally fails if the holder ever loses one.
 *   - `coverage` called `coverPrivateConstructor` and `coverEnum`, which existed only to satisfy a
 *     coverage tool by reflectively touching a private constructor and the values of a Java enum.
 *     A Scala `object` has no constructor to reach and there is no second enum, so what those
 *     calls stood for is asserted directly: the family is closed at 45 distinct members, every
 *     one of them round-trips through its name, and each of the 45 constants is the very member.
 *   - `test_serialization` round-tripped a convention through Java serialization and through the
 *     binary and JSON encodings of the bean library the Java type belonged to, all three of which
 *     read the class back reflectively. None is a dependency of this port and neither Java
 *     serialization nor compatibility with that library's JSON is in its scope, so the round trip
 *     is asserted through the circe codec that replaced them.
 *   - `test_jodaConvert` asserted a round trip through the reflective string-conversion library
 *     the Java type was annotated for. That library is not a dependency either, and its two
 *     annotations became `Show` and `parse`, so the guarantee is asserted over those and over
 *     `toString`, which agrees with them.
 *
 * Numerical parity against the Java implementation is not this spec's subject: the roll
 * conventions as used inside schedule generation are pinned to the captured Java baseline by
 * `parity.ScheduleParitySpec`. The assertions here are behavioural and compare dates exactly.
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
  /** The Java `data_name` provider: each convention with the name it renders as. */
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
   * The Java `data_lenient` provider, all 11 rows, in the order the provider listed them.
   *
   * Every row is a spelling that the ordered chain of lenient rewrites turns into a canonical
   * name. The rows are what makes the order of that chain observable: the three spellings of the
   * 31st reach `EOM` only because the row for 31 is declared before the row that captures a one-
   * or two-digit day, and a reordering of the transcribed patterns - which would change what
   * resolves and to what - is caught here.
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
   * This table is the port of the reflective sweep of the Java `test_lenientLookup_constants`,
   * which read the public static fields of the constants holder and checked that each field name
   * resolved leniently to the value the field held. Reflection is not available to this port, so
   * the identifiers are written out. That is a stronger statement than the sweep was, not a
   * weaker one: the sweep asserted a property of whatever fields it happened to find, while this
   * table also fixes which 45 identifiers the holder publishes.
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
   * The Java test derived these names by asking a string-case utility of the collection library
   * it depended on to turn `MONDAY` into `Monday` and then taking the first three letters. That
   * utility has no target in this port, and deriving a name is in any case a weaker statement
   * than naming it, so the seven names are written out - which is also how the production file
   * declares them.
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
    // Reinterpretation. The Java method passed the absent reference to `adjust`, `matches`,
    // `next` and `previous` on each of the eight rule-based conventions and asserted that all
    // six calls raised. The guards behind those six throws have no target here, because what
    // they guarded against cannot be expressed: a date and a frequency are required parameters
    // of required types, so omitting either, or offering something that is not one, is rejected
    // when this spec is compiled rather than when it runs. That is asserted first, as the
    // strongest form the Java case can take, and no `null` is written anywhere in this file.
    assertDoesNotCompile("RollConventions.EOM.adjust()")
    assertDoesNotCompile("RollConventions.EOM.matches()")
    assertDoesNotCompile("RollConventions.EOM.next(date(2014, JULY, 1))")
    assertDoesNotCompile("RollConventions.EOM.previous(date(2014, JULY, 1))")
    assertDoesNotCompile("RollConventions.EOM.next(Frequency.P3M, date(2014, JULY, 1))")

    // What the six guards were protecting is the totality of the four operations, so the rest of
    // the case asserts that: every one of the eight conventions answers for a valid date, and
    // the two sequence operations honour the contract that makes them usable in a schedule -
    // `next` always moves strictly forward and `previous` always strictly back, whatever the
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
    // All 51 rows of the Java `data_adjust` provider, including the rows that only the three
    // calendar-bearing conventions produce: the IMMCAD dates in August and September 2014, the
    // Sydney-adjusted IMMAUD dates, and the two TBILL rows whose Java comment reads "Tuesday due
    // to holiday" - 2018-08-31 and 2018-09-01 both rolling to Tuesday 2018-09-04 because the
    // Monday between them is Labor Day in New York.
    dataAdjust should have size 51
    forAll(dataAdjust) { (convention: RollConvention, input: LocalDate, expected: LocalDate) =>
      withClue(s"${convention.name} on $input: ")(convention.adjust(input) shouldBe expected)
    }

    // The divergence this table discharges (AAP 0.3.3 and 0.6.5, recorded in
    // SCALA_MIGRATION.md): the Java `IMMCAD`, `IMMAUD` and `TBILL` captured their holiday
    // calendars from standard reference data while their enum class initialised, falling back to
    // a Saturday/Sunday calendar if that lookup missed. The members of this port hold the
    // built-in calendar values of `StandardHolidayCalendars` directly - as data, with no lookup
    // and so no fallback - while `adjust(date)` keeps the Java signature: a date in, a date out,
    // with no reference data parameter and no error channel. The assertions below state that
    // equivalence as the three rules themselves, over a five-year grid of probe dates rather
    // than only the dates the Java provider listed, so a member bound to the wrong calendar
    // cannot pass.
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
        // One Sydney business day before the second Friday.
        RollConventions.IMMAUD.adjust(input) shouldBe sydney.previous(secondFriday)
        // The next Monday, moved on to a New York business day.
        RollConventions.TBILL.adjust(input) shouldBe newYork.nextOrSame(nextOrSameMonday)

        // And the three conventions that consult no calendar are pure date arithmetic. Their
        // results are asserted against the arithmetic itself and then, more sharply, by the day
        // of the week they always land on: a rule that moved off a holiday could not guarantee
        // that, so these three equalities are what states that no calendar takes part.
        RollConventions.IMM.adjust(input) shouldBe thirdWednesday
        RollConventions.IMM.adjust(input).getDayOfWeek shouldBe DayOfWeek.WEDNESDAY
        RollConventions.SFE.adjust(input) shouldBe secondFriday
        RollConventions.SFE.adjust(input).getDayOfWeek shouldBe DayOfWeek.FRIDAY
        RollConventions.IMMNZD.adjust(input) shouldBe
          input.withDayOfMonth(9).`with`(TemporalAdjusters.nextOrSame(DayOfWeek.WEDNESDAY))
        RollConventions.IMMNZD.adjust(input).getDayOfWeek shouldBe DayOfWeek.WEDNESDAY

        // Every rule keeps the year and month of the date it was given, apart from TBILL, which
        // rolls forward and may cross into the following month - the asymmetry the Java
        // implementation had, and the reason the TBILL rows of the provider are the only ones
        // whose expected date leaves the input month.
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

    // The concrete evidence that the New York calendar really is consulted, which the two
    // holiday rows of the provider assert and which a weekend-only fallback would fail: the
    // Monday of that week is a holiday, so the result is the Tuesday after it.
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
    // Written out one constant at a time, as the Java method was, because the point of the test
    // is that each identifier names the convention its name claims: a loop over `values` would
    // assert the members and say nothing about the identifiers.
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
    // The Java loop ran from 1 to 29 inclusive - `i < 30` - and is kept at that bound, the 30th
    // being covered by `test_dayOfMonth_constants` and by the February cases below.
    (1 until 30).foreach { day =>
      val result: FailureOr[RollConvention] = RollConvention.ofDayOfMonth(day)
      val test: RollConvention = rc(result)
      withClue(s"Day$day: ") {
        result should beSuccess
        test.adjust(date(2014, JULY, 1)) shouldBe date(2014, JULY, day)
        test.name shouldBe s"Day$day"
        test.toString shouldBe s"Day$day"
        test.dayOfMonth shouldBe day

        // The Java assertions were `isSameAs`, because the thirty conventions are built once and
        // cached: a name resolving to an equal but distinct value would have failed there and
        // fails here too. Both the exact lookup and the lenient one are asserted, in the
        // canonical spelling and in the upper-case spelling the Java method used.
        resolvedByName(test.name) should be theSameInstanceAs test
        resolvedByName(s"DAY$day") should be theSameInstanceAs test
        rc(RollConvention.parse(test.name)) should be theSameInstanceAs test
        rc(RollConvention.parse(s"DAY$day")) should be theSameInstanceAs test
        rc(RollConvention.ofDayOfMonth(day)) should be theSameInstanceAs test
      }
    }
  }

  test("test_ofDayOfMonth_31") {
    // The 31st is `EOM` rather than a convention of its own, because the conventions for 29, 30
    // and 31 all have to roll to the end of February and so would differ in no month.
    RollConvention.ofDayOfMonth(31) should haveValue(RollConventions.EOM)
    rc(RollConvention.ofDayOfMonth(31)) should be theSameInstanceAs RollConventions.EOM
  }

  test("test_ofDayOfMonth_invalid") {
    // Where the Java factory raised for a day-of-month outside 1 to 31, this one reports it as a
    // value carrying the reason and the message of the ported factory. Nothing is raised, which
    // is asserted alongside the failure itself.
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
    // result to open, which is the one place this port's factory is simpler than the day-of-month
    // one rather than more explicit than the Java original.
    forAll(dataDayOfWeekNames) { (dayOfWeek: DayOfWeek, name: String) =>
      val test: RollConvention = RollConvention.ofDayOfWeek(dayOfWeek)
      withClue(s"$dayOfWeek: ") {
        test.name shouldBe name
        test.toString shouldBe name
        test.dayOfMonth shouldBe 0

        // The Java assertions were `isSameAs` here too, for the same reason: the seven
        // conventions are built once. The second lookup is the upper-case spelling the Java
        // method built from the first three letters of the day's own name.
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
    // The Java `RollConvention.of(name)` became two members, and both are asserted here so that
    // the exact lookup and the lenient one cannot drift apart on a canonical name.
    forAll(dataName) { (convention: RollConvention, name: String) =>
      withClue(s"$name: ") {
        RollConvention.valueOf(name) shouldBe Some(convention)
        RollConvention.parse(name) should haveValue(convention)
        resolvedByName(name) should be theSameInstanceAs convention
      }
    }
  }

  test("test_lenientLookup_standardNames") {
    // The Java method asked the registry for the lower-case spelling of each canonical name,
    // through `findLenient`. That is `parse` here, and the lower-case spelling is deliberately
    // asserted to be outside the exact key space: it resolves because of the leniency and not
    // because the family registered a third key for each member.
    forAll(dataName) { (convention: RollConvention, name: String) =>
      val lowerCase: String = name.toLowerCase(Locale.ENGLISH)
      withClue(s"$lowerCase: ") {
        RollConvention.parse(lowerCase) should haveValue(convention)
        RollConvention.valueOf(lowerCase) shouldBe None
      }
    }
  }

  test("test_extendedEnum") {
    // The Java method read `extendedEnum().lookupAll()` and looked each canonical name up in it.
    // The counterpart of that map here is `byCanonicalName`, which is the Java
    // `lookupAllNormalized` - the 45 canonical keys - while the Java `lookupAll` was the union of
    // those with the upper-case spellings each provider also registered. Both are asserted.
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

    // Two of the 45 names are already upper case throughout - `EOM` and `IMM` are, as are the
    // other five rule-based names bar `None` - so the union of the two key views is smaller than
    // twice 45. Stating it as the union rather than as a count keeps the relationship to the Java
    // `lookupAll` map exact.
    val lookupAll: Set[String] = lookup.byCanonicalName.keySet ++ lookup.byUpperName.keySet
    lookupAll shouldBe
      (declarationOrder.map(_.name) ++ declarationOrder.map(_.name.toUpperCase(Locale.ENGLISH))).toSet
    lookupAll should contain allOf ("None", "NONE", "Day15", "DAY15", "DayMon", "DAYMON")

    // This family declares no alternate spelling, because the configuration resource being
    // transcribed declared none for it: everything beyond the two key views arrives through the
    // ordered lenient chain or through the one external group.
    lookup.alternateNames shouldBe Map.empty[String, String]

    // The FpML group of external spellings, all 44 rows of the resource, asserted row for row.
    // They take part in no lookup - `MON` and `31` resolve through the lenient chain, which
    // happens to accept them - so a lost row would otherwise be invisible. `IMMCAD` and `TBILL`
    // are deliberately absent: FpML defines neither, and the resource declared neither.
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

    // The ordered lenient table, whose 11 rows are the rows of the resource in its order. The
    // order is part of the data - a later pattern sees what an earlier one produced - and it is
    // load-bearing here rather than incidental: the row for 31 precedes the row for 30 and both
    // precede the row that captures a one- or two-digit day, which is why text naming the 31st
    // reaches `EOM` and is never rewritten to a `Day31` that no member carries.
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

    // And the consequence of that order, which is the sharpest statement the table supports: the
    // 31st is the end of the month and the 30th is its own convention.
    RollConvention.parse("31") should haveValue(RollConventions.EOM)
    RollConvention.parse("30") should haveValue(RollConventions.DAY_30)
    RollConvention.valueOf("Day31") shouldBe None
  }

  test("test_of_lookup_notFound") {
    // Where the Java factory raised an error for text naming no member, the port reports it as a
    // value. The reason is compared as a member of the closed family of reasons and the message
    // is asserted once, because it names the family and the text - the two things a caller has to
    // be told.
    RollConvention.valueOf("Rubbish") shouldBe None
    RollConvention.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
    RollConvention.parse("Rubbish") should
      haveFailureMessageMatching("RollConvention name not found: Rubbish")
    noException should be thrownBy RollConvention.parse("Rubbish")
  }

  test("test_of_lookup_null") {
    // Reinterpretation. The Java method passed the absent reference to the factory and asserted
    // that it raised. The `notNull` guard behind that throw has no target here, because the
    // absent argument it guarded against is not something a Scala caller can express: the name is
    // a required parameter of a required type, so omitting it, or offering something that is not
    // text, is rejected when this spec is compiled rather than when it runs. That is asserted as
    // a compile-time proof, and no `null` is written.
    assertDoesNotCompile("RollConvention.parse()")
    assertDoesNotCompile("RollConvention.valueOf()")
    assertDoesNotCompile("RollConvention.parse(31)")

    // What a caller can actually supply is text that names nothing, so the rest of the case is
    // every spelling of "no usable name" - empty, blank, and several near-misses, including the
    // two that the lenient chain rewrites successfully into a name no member carries. Each
    // resolves to nothing and raises nothing.
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
    // The Java method drove every row of `data_lenient` through `findLenient` after folding it to
    // lower case. `parse` is that method here, and each row is asserted in three spellings - as
    // written, folded down and folded up - because the patterns are matched insensitively to case
    // and the input is folded to upper case before they are applied.
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
    // The port of the reflective sweep over the constants holder. The Java method read the public
    // static fields of `RollConventions` through `java.lang.reflect` and asserted that each field
    // name resolved leniently to the value the field held; reflection is forbidden in this port,
    // so the reflective sweep has no target and the 45 identifiers are written out as a table
    // instead. Each is asserted in its own spelling and folded to lower case, exactly as the Java
    // method asserted for the names it discovered.
    forAll(dataConstantIdentifiers) { (identifier: String, convention: RollConvention) =>
      withClue(s"$identifier: ") {
        RollConvention.parse(identifier) should haveValue(convention)
        RollConvention.parse(identifier.toLowerCase(Locale.ENGLISH)) should haveValue(convention)
        rc(RollConvention.parse(identifier)) should be theSameInstanceAs convention
      }
    }

    // The table is the holder in full: 45 identifiers naming 45 distinct members, which is the
    // half of the reflective sweep that a hand-written table can state and reflection could not.
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
    // - so the equality, the hashing and the ordering can never disagree with each other or with
    // `==`, and `Show` agrees with `toString`. The three members above are asked all of it.
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
    // The Java method was `coverPrivateConstructor(RollConventions.class)` and
    // `coverEnum(StandardRollConventions.class)`: the first reflectively invoked the private
    // constructor of a static holder, the second read the values of a package-private enum, both
    // so that a coverage tool would not report them as unexercised. A Scala `object` has no
    // constructor to reach and there is no second enum - the members are declared in the
    // companion - so what those two calls stood for is asserted directly, as the closedness of
    // the family. This is the assertion that carries Rule 4 for the roll conventions.

    // Exactly 45 members, distinct as values and distinct by name, in the declaration order of
    // the two Java providers rather than the alphabetical order the `Order` instance imposes.
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

    // Each of the 45 constants of the holder is the very member of the family, not a copy and not
    // a registry indirection, so a call site reading the constant and one reading the member are
    // indistinguishable - by `eq`, by `==` and in a pattern match.
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
    // for `EOM` - the two agree in every month - and zero for everything else.
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
    // The Java method was `assertSerialization(EOM)`, `assertSerialization(DAY_2)` and
    // `assertSerialization(DAY_THU)`: a round trip through Java serialization and through the
    // binary and JSON encodings of the bean library the Java type belonged to, all three of which
    // read the class back reflectively. None of the three is a dependency of this port, and
    // neither Java serialization nor wire compatibility with that library's JSON is in its scope
    // (AAP 0.2.2). What replaces them is the circe codec the companion publishes, so the round
    // trip is asserted through that - over the three conventions the Java method named, and then
    // over all 45, since the codec is one instance shared by the family.
    //
    // The document is the bare canonical name and never an object: that is the single-string form
    // the annotated string conversion of the Java type wrote, so a document from either side
    // names the same convention.
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

    // The reader is as lenient as `parse`, which is what lets a document written by hand, or
    // through the FpML vocabulary the resource published, still be read.
    Json.fromString("Day_31").as[RollConvention] shouldBe Right(RollConventions.EOM)
    Json.fromString("15").as[RollConvention] shouldBe Right(RollConventions.DAY_15)
    Json.fromString("thu").as[RollConvention] shouldBe Right(RollConventions.DAY_THU)
  }

  test("test_jodaConvert") {
    // The Java method was `assertJodaConvert(RollConvention.class, NONE)` and the same for `EOM`:
    // a round trip through the reflective string-conversion library the Java type was annotated
    // for. That library is not a dependency of this port, and the two annotations it read -
    // rendering a convention as its name and recovering it from that name - became `Show`,
    // `toString` and `parse`. The guarantee is therefore asserted over those three, for the two
    // conventions the Java method named and then for the whole family, which is a stronger
    // statement than the reflective round trip was.
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
 * The transcribed data providers of the Java `RollConventionTest`, together with the derived
 * tables this spec asserts the closed family against.
 *
 * They live in a companion rather than in the class so that the tests above read as the Java
 * methods did, and the object extends the table support because building a `Table` needs it.
 *
 * The eight conventions and the three frequencies are bound to short names here for the same
 * reason the Java class imported them statically: the four large tables are transcriptions, and a
 * transcription is only checkable against its original if it reads like it. Each name is the
 * constant of [[RollConventions]] or [[Frequency]] that the Java class imported, so the
 * identifiers the ported library published remain the ones this file names.
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
   * The Java `data_types` provider: the values of the rule-based enum, in its declaration order.
   *
   * The Java provider read `StandardRollConventions.values()`, the eight members that enum
   * declared. The day-based members came from a second provider and were never part of this
   * table, so it holds those eight and not the whole family.
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
   * The Java `data_adjust` provider, all 51 rows, in the order the provider listed them.
   *
   * The two comments the Java provider carried are kept where it carried them, on the two TBILL
   * rows whose result is a Tuesday because the Monday of that week is a New York holiday.
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
  /** The Java `data_matches` provider, all 17 rows, in the order the provider listed them. */
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
   * The Java `data_next` provider, all 62 rows, in the order the provider listed them.
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
  /** The Java `data_previous` provider, all 57 rows, in the order the provider listed them. */
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
   * The 45 members of the family, in the declaration order the two Java providers gave them.
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
   * The FpML group of external names, all 44 rows of the configuration resource being ported.
   *
   * Transcribed from the resource independently of the production file - which holds the same
   * rows as text - so that a mistranscription in either is a disagreement between the two rather
   * than a self-consistent error. The row `31` maps to `EOM`, because FpML has no end-of-month
   * spelling of its own, and `IMMCAD` and `TBILL` appear nowhere, because FpML defines neither.
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
   * Five years of five days each, covering the start, the middle and the end of every month, so
   * that the third Wednesday, the second Friday and the next Monday are each approached from
   * before, on and after. The four dates appended are the ones the Java provider singled out,
   * including the two New York holiday cases of September 2018.
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
