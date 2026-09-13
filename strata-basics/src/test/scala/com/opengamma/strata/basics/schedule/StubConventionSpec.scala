/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.schedule

import java.time.LocalDate
import java.time.Month.APRIL
import java.time.Month.AUGUST
import java.time.Month.FEBRUARY
import java.time.Month.JANUARY
import java.time.Month.JULY
import java.time.Month.JUNE
import java.time.Month.MARCH
import java.time.Month.OCTOBER
import java.util.Locale
import java.util.concurrent.atomic.AtomicInteger

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
import org.scalatest.prop.TableFor4
import org.scalatest.prop.TableFor6

import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.basics.date.BusinessDayConventions
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[StubConvention]]. Three conventions hold across this file:
 *
 *  - a rejected input is answered with a value carrying a [[FailureReason]] - `INVALID` for an
 *    incompatible stub declaration, `PARSING` for text naming no member - and never a throw;
 *  - the `definition` parameter of `toImplicit` is taken by name, so it is rendered only on the
 *    path that rejects, which every row of `test_toImplicit` asserts beside the result;
 *  - the family is closed, so [[StubConvention.values]] is the published inventory of all eight
 *    members, and `coverage` asserts its contents, its order and that a ninth cannot be declared.
 *
 * The spec is declared in `com.opengamma.strata.basics.schedule` because
 * [[StubConvention.toImplicit]] and [[StubConvention.isStubLong]] are `private[schedule]`: the
 * package is what makes them reachable.
 *
 * @see [[RollConventionSpec]] for the conventions [[StubConvention.toRollConvention]] derives
 * @see [[FrequencySpec]] for the periodic frequencies it derives them from
 */
final class StubConventionSpec
    extends AnyFunSuite
    with Matchers
    with TableDrivenPropertyChecks
    with ResultMatchers {

  //-------------------------------------------------------------------------
  /**
   * The eight members in declaration order, which is deliberately not the alphabetical order the
   * `Order` instance imposes. Writing it out rather than reading it from [[StubConvention.values]]
   * is what lets `coverage` assert that order rather than assume it.
   */
  private val declarationOrder: List[StubConvention] =
    List(
      StubConvention.NONE,
      StubConvention.SHORT_INITIAL,
      StubConvention.LONG_INITIAL,
      StubConvention.SMART_INITIAL,
      StubConvention.SHORT_FINAL,
      StubConvention.LONG_FINAL,
      StubConvention.SMART_FINAL,
      StubConvention.BOTH)

  /**
   * The rendering of a schedule definition that a rejected stub declaration carries.
   *
   * It stands in for `PeriodicSchedule.toString`, which is what [[PeriodicSchedule]] passes, and
   * is distinctive enough that finding it under a failure's `definition` attribute proves the
   * argument arrived rather than merely that some attribute exists.
   */
  private val definitionText: String =
    "PeriodicSchedule{startDate=2014-06-30, endDate=2015-09-30, frequency=P3M}"

  /**
   * Unwraps a result that the test expects to hold a convention.
   *
   * Opening the result here rather than at each call site means one that unexpectedly holds a
   * failure fails the test naming that failure.
   *
   * @param result  the result to unwrap
   * @tparam E  the type of the failure the result may hold
   * @return the convention the result holds
   */
  private def sc[E](result: Either[E, StubConvention]): StubConvention =
    result.fold(failure => fail(s"unexpected failure: $failure"), identity)

  /**
   * Unwraps a result that the test expects to hold a failure.
   *
   * The counterpart of [[sc]] for a rejected call, opening the failure so that its message and its
   * `definition` attribute can be asserted as well as its reason.
   *
   * @param result  the result to unwrap
   * @return the failure the result holds
   */
  private def failureOf(result: Either[Failure, StubConvention]): Failure =
    result.fold(identity, effective => fail(s"expected a failure but got $effective"))

  /**
   * Looks a convention up by name exactly, failing the test when the name resolves to nothing.
   *
   * [[StubConvention.valueOf]] with the empty case turned into a test failure, which is what lets
   * an assertion about the ''identity'' of the value a name resolves to avoid unsafely opening an
   * `Option`.
   *
   * @param name  the name to look up
   * @return the convention with that name
   */
  private def resolvedByName(name: String): StubConvention =
    StubConvention.valueOf(name).getOrElse(fail(s"no convention is named $name"))

  //-------------------------------------------------------------------------
  /**
   * Every member of the family, written out so that the table fixes which eight it drives;
   * `coverage` asserts that this list and [[StubConvention.values]] agree.
   */
  private val dataTypes: TableFor1[StubConvention] =
    Table(
      "type",
      StubConvention.NONE,
      StubConvention.SHORT_INITIAL,
      StubConvention.LONG_INITIAL,
      StubConvention.SMART_INITIAL,
      StubConvention.SHORT_FINAL,
      StubConvention.LONG_FINAL,
      StubConvention.SMART_FINAL,
      StubConvention.BOTH)

  /**
   * Each member with the name it renders as - the identity of the family in text, in JSON and in
   * every message that interpolates a convention.
   */
  private val dataName: TableFor2[StubConvention, String] =
    Table(
      ("convention", "name"),
      (StubConvention.NONE, "None"),
      (StubConvention.SHORT_INITIAL, "ShortInitial"),
      (StubConvention.LONG_INITIAL, "LongInitial"),
      (StubConvention.SMART_INITIAL, "SmartInitial"),
      (StubConvention.SHORT_FINAL, "ShortFinal"),
      (StubConvention.LONG_FINAL, "LongFinal"),
      (StubConvention.SMART_FINAL, "SmartFinal"),
      (StubConvention.BOTH, "Both"))

  /**
   * The rows of `test_toRollConvention`, grouped by member.
   *
   * Each row is a member, a start date, an end date, a frequency and the end-of-month preference,
   * together with the roll convention those five imply. The three initial members roll backwards
   * and derive the convention from the end date; the other five derive it from the start date,
   * `Both` among them, which answers false to both direction questions and so falls through to the
   * start date as a forward-rolling member does. Three groups carry the rules the table exists for:
   *
   *  - the `None` rows, which hold the whole of the month-end special case: with a month-based
   *    frequency, two dates that disagree about the day of the month, lie in different months and
   *    have at least one of them at a month end yield `EOM` when the end of the month is
   *    preferred, and otherwise the later of the two days. The pair 2016-03-16 to 2016-03-31 is
   *    the control for the different-months condition, sharing a month and so resolving to
   *    `Day16`, while 2016-03-16 to 2017-03-31 differs only in the year and resolves to `EOM`;
   *  - the `TERM` rows, which resolve to `RollConventions.NONE` because a term frequency is
   *    neither month-based nor week-based;
   *  - the week-based rows, which read a day of the week rather than a day of the month, and read
   *    it from the end date for the three initial members - 2014-08-16 is a Saturday - and from
   *    the start date for the rest - 2014-01-14 is a Tuesday. Those two dates are what makes the
   *    direction of the derivation observable.
   */
  private val dataRoll: TableFor6[StubConvention, LocalDate, LocalDate, Frequency, Boolean, RollConvention] =
    Table(
      ("convention", "start", "end", "frequency", "preferEndOfMonth", "expected"),
      (StubConvention.NONE, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_14),
      (StubConvention.NONE, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.DAY_14),
      (StubConvention.NONE, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, false, RollConventions.DAY_TUE),
      (StubConvention.NONE, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, true, RollConventions.DAY_TUE),
      (StubConvention.NONE, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, false, RollConventions.NONE),
      (StubConvention.NONE, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, true, RollConventions.NONE),
      (StubConvention.NONE, date(2014, JANUARY, 31), date(2014, APRIL, 30), Frequency.P1M, true, RollConventions.EOM),
      (StubConvention.NONE, date(2014, APRIL, 30), date(2014, AUGUST, 31), Frequency.P1M, true, RollConventions.EOM),
      (StubConvention.NONE, date(2014, APRIL, 30), date(2014, FEBRUARY, 28), Frequency.P1M, true, RollConventions.EOM),
      (StubConvention.NONE, date(2016, FEBRUARY, 29), date(2019, FEBRUARY, 28), Frequency.P6M, true, RollConventions.EOM),
      (StubConvention.NONE, date(2015, FEBRUARY, 28), date(2016, FEBRUARY, 29), Frequency.P6M, true, RollConventions.EOM),
      (StubConvention.NONE, date(2015, APRIL, 30), date(2016, FEBRUARY, 29), Frequency.P1M, true, RollConventions.EOM),
      (StubConvention.NONE, date(2016, MARCH, 31), date(2017, MARCH, 27), Frequency.P6M, true, RollConventions.EOM),
      (StubConvention.NONE, date(2016, MARCH, 16), date(2016, MARCH, 31), Frequency.P6M, true, RollConventions.DAY_16),
      (StubConvention.NONE, date(2016, MARCH, 16), date(2017, MARCH, 31), Frequency.P6M, true, RollConventions.EOM),
      (StubConvention.SHORT_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_16),
      (StubConvention.SHORT_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.DAY_16),
      (StubConvention.SHORT_INITIAL, date(2014, JANUARY, 14), date(2014, JUNE, 30), Frequency.P1M, false, RollConventions.DAY_30),
      (StubConvention.SHORT_INITIAL, date(2014, JANUARY, 14), date(2014, JUNE, 30), Frequency.P1M, true, RollConventions.EOM),
      (StubConvention.SHORT_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, true, RollConventions.DAY_SAT),
      (StubConvention.SHORT_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, false, RollConventions.DAY_SAT),
      (StubConvention.SHORT_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, false, RollConventions.NONE),
      (StubConvention.SHORT_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, true, RollConventions.NONE),
      (StubConvention.LONG_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_16),
      (StubConvention.LONG_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.DAY_16),
      (StubConvention.LONG_INITIAL, date(2014, JANUARY, 14), date(2014, JUNE, 30), Frequency.P1M, false, RollConventions.DAY_30),
      (StubConvention.LONG_INITIAL, date(2014, JANUARY, 14), date(2014, JUNE, 30), Frequency.P1M, true, RollConventions.EOM),
      (StubConvention.LONG_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, false, RollConventions.DAY_SAT),
      (StubConvention.LONG_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, true, RollConventions.DAY_SAT),
      (StubConvention.LONG_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, false, RollConventions.NONE),
      (StubConvention.LONG_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, true, RollConventions.NONE),
      // SmartInitial - the seven-day rule of the smart members is about stub length, not about the
      // roll convention, so it changes nothing here.
      (StubConvention.SMART_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_16),
      (StubConvention.SMART_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.DAY_16),
      (StubConvention.SMART_INITIAL, date(2014, JANUARY, 14), date(2014, JUNE, 30), Frequency.P1M, false, RollConventions.DAY_30),
      (StubConvention.SMART_INITIAL, date(2014, JANUARY, 14), date(2014, JUNE, 30), Frequency.P1M, true, RollConventions.EOM),
      (StubConvention.SMART_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, false, RollConventions.DAY_SAT),
      (StubConvention.SMART_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, true, RollConventions.DAY_SAT),
      (StubConvention.SMART_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, false, RollConventions.NONE),
      (StubConvention.SMART_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, true, RollConventions.NONE),
      (StubConvention.SHORT_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_14),
      (StubConvention.SHORT_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.DAY_14),
      (StubConvention.SHORT_FINAL, date(2014, JUNE, 30), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_30),
      (StubConvention.SHORT_FINAL, date(2014, JUNE, 30), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.EOM),
      (StubConvention.SHORT_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, false, RollConventions.DAY_TUE),
      (StubConvention.SHORT_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, true, RollConventions.DAY_TUE),
      (StubConvention.SHORT_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, false, RollConventions.NONE),
      (StubConvention.SHORT_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, true, RollConventions.NONE),
      (StubConvention.LONG_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_14),
      (StubConvention.LONG_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.DAY_14),
      (StubConvention.LONG_FINAL, date(2014, JUNE, 30), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_30),
      (StubConvention.LONG_FINAL, date(2014, JUNE, 30), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.EOM),
      (StubConvention.LONG_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, false, RollConventions.DAY_TUE),
      (StubConvention.LONG_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, true, RollConventions.DAY_TUE),
      (StubConvention.LONG_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, false, RollConventions.NONE),
      (StubConvention.LONG_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, true, RollConventions.NONE),
      (StubConvention.SMART_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_14),
      (StubConvention.SMART_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.DAY_14),
      (StubConvention.SMART_FINAL, date(2014, JUNE, 30), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_30),
      (StubConvention.SMART_FINAL, date(2014, JUNE, 30), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.EOM),
      (StubConvention.SMART_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, false, RollConventions.DAY_TUE),
      (StubConvention.SMART_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, true, RollConventions.DAY_TUE),
      (StubConvention.SMART_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, false, RollConventions.NONE),
      (StubConvention.SMART_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, true, RollConventions.NONE),
      (StubConvention.BOTH, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_14),
      (StubConvention.BOTH, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.DAY_14))

  /**
   * The rows of `test_toImplicit`: a member, the two flags saying whether an initial and a final
   * stub have been declared by dates, and the effective convention that combination implies. The
   * fourth column is an `Option`, `None` standing for "this combination is rejected", which is
   * what lets the row loop assert a `Left` carrying `Failure.Invalid` for those fourteen rows.
   *
   * The four rows of each member are its whole truth table, in the order (no stub, initial only,
   * final only, both), so the three shapes of the family are visible side by side: the strict
   * members reject the stub they cannot produce, the two smart members reject nothing and answer
   * `Both` when given both stubs, and `Both` itself demands both.
   */
  private val dataImplicit: TableFor4[StubConvention, Boolean, Boolean, Option[StubConvention]] =
    Table(
      ("convention", "explicitInitialStub", "explicitFinalStub", "expected"),
      (StubConvention.NONE, false, false, Some(StubConvention.NONE)),
      (StubConvention.NONE, true, false, None),
      (StubConvention.NONE, false, true, None),
      (StubConvention.NONE, true, true, None),
      (StubConvention.SHORT_INITIAL, false, false, Some(StubConvention.SHORT_INITIAL)),
      (StubConvention.SHORT_INITIAL, true, false, Some(StubConvention.NONE)),
      (StubConvention.SHORT_INITIAL, false, true, None),
      (StubConvention.SHORT_INITIAL, true, true, None),
      (StubConvention.LONG_INITIAL, false, false, Some(StubConvention.LONG_INITIAL)),
      (StubConvention.LONG_INITIAL, true, false, Some(StubConvention.NONE)),
      (StubConvention.LONG_INITIAL, false, true, None),
      (StubConvention.LONG_INITIAL, true, true, None),
      (StubConvention.SMART_INITIAL, false, false, Some(StubConvention.SMART_INITIAL)),
      (StubConvention.SMART_INITIAL, true, false, Some(StubConvention.NONE)),
      (StubConvention.SMART_INITIAL, false, true, Some(StubConvention.SMART_INITIAL)),
      (StubConvention.SMART_INITIAL, true, true, Some(StubConvention.BOTH)),
      (StubConvention.SHORT_FINAL, false, false, Some(StubConvention.SHORT_FINAL)),
      (StubConvention.SHORT_FINAL, true, false, None),
      (StubConvention.SHORT_FINAL, false, true, Some(StubConvention.NONE)),
      (StubConvention.SHORT_FINAL, true, true, None),
      (StubConvention.LONG_FINAL, false, false, Some(StubConvention.LONG_FINAL)),
      (StubConvention.LONG_FINAL, true, false, None),
      (StubConvention.LONG_FINAL, false, true, Some(StubConvention.NONE)),
      (StubConvention.LONG_FINAL, true, true, None),
      (StubConvention.SMART_FINAL, false, false, Some(StubConvention.SMART_FINAL)),
      (StubConvention.SMART_FINAL, true, false, Some(StubConvention.SMART_FINAL)),
      (StubConvention.SMART_FINAL, false, true, Some(StubConvention.NONE)),
      (StubConvention.SMART_FINAL, true, true, Some(StubConvention.BOTH)),
      (StubConvention.BOTH, false, false, None),
      (StubConvention.BOTH, true, false, None),
      (StubConvention.BOTH, false, true, None),
      (StubConvention.BOTH, true, true, Some(StubConvention.NONE)))

  /**
   * The rows of `test_isStubLong`: a member, the two boundaries of the candidate stub, and whether
   * the stub is merged into the period next to it.
   *
   * The non-smart members are asked about one seven-day gap, fixing their constant answers: the
   * two long members always merge and the other four never do. The smart rows approach the
   * threshold from both sides - one and six days merge, seven and eight days are retained - which
   * is what makes it exclusive rather than inclusive.
   */
  private val dataIsStubLong: TableFor4[StubConvention, LocalDate, LocalDate, Boolean] =
    Table(
      ("convention", "date1", "date2", "expected"),
      (StubConvention.NONE, date(2018, 6, 1), date(2018, 6, 8), false),
      (StubConvention.SHORT_INITIAL, date(2018, 6, 1), date(2018, 6, 8), false),
      (StubConvention.LONG_INITIAL, date(2018, 6, 1), date(2018, 6, 8), true),
      (StubConvention.SHORT_FINAL, date(2018, 6, 1), date(2018, 6, 8), false),
      (StubConvention.LONG_FINAL, date(2018, 6, 1), date(2018, 6, 8), true),
      (StubConvention.BOTH, date(2018, 6, 1), date(2018, 6, 8), false),
      (StubConvention.SMART_INITIAL, date(2018, 6, 1), date(2018, 6, 2), true),
      (StubConvention.SMART_INITIAL, date(2018, 6, 1), date(2018, 6, 7), true),
      (StubConvention.SMART_INITIAL, date(2018, 6, 1), date(2018, 6, 8), false),
      (StubConvention.SMART_INITIAL, date(2018, 6, 1), date(2018, 6, 9), false),
      (StubConvention.SMART_FINAL, date(2018, 6, 1), date(2018, 6, 2), true),
      (StubConvention.SMART_FINAL, date(2018, 6, 1), date(2018, 6, 7), true),
      (StubConvention.SMART_FINAL, date(2018, 6, 1), date(2018, 6, 8), false),
      (StubConvention.SMART_FINAL, date(2018, 6, 1), date(2018, 6, 9), false))

  //-------------------------------------------------------------------------
  test("test_null") {
    // `toRollConvention` takes four arguments and all four are required: each snippet below omits
    // one, or supplies one whose type is not the parameter's, and none of them compiles.
    assertDoesNotCompile("StubConvention.NONE.toRollConvention()")
    assertDoesNotCompile(
      "StubConvention.NONE.toRollConvention(date(2014, JULY, 1), Frequency.P3M, true)")
    assertDoesNotCompile(
      "StubConvention.NONE.toRollConvention(date(2014, JULY, 1), date(2014, OCTOBER, 1), true)")
    assertDoesNotCompile(
      "StubConvention.NONE.toRollConvention(date(2014, JULY, 1), date(2014, OCTOBER, 1), Frequency.P3M)")

    // The rest of the case is the totality of the family's operations: every member answers every
    // one of its operations for a month-based, a week-based and a term frequency and for both
    // settings of the end-of-month preference, and every answer is a member of the roll-convention
    // family rather than something outside it. The two direction questions are mutually exclusive
    // and at most one of long, short and smart holds, which is the invariant that makes the six
    // predicates a classification rather than six unrelated flags.
    val start: LocalDate = date(2014, JULY, 1)
    val end: LocalDate = date(2014, OCTOBER, 1)
    dataTypes should have size 8
    forAll(dataTypes) { (convention: StubConvention) =>
      withClue(s"${convention.name}: ") {
        val answers: List[Boolean] =
          List(
            convention.isCalculateForwards,
            convention.isCalculateBackwards,
            convention.isFinal,
            convention.isLong,
            convention.isShort,
            convention.isSmart)
        answers should have size 6
        (convention.isCalculateForwards && convention.isCalculateBackwards) shouldBe false
        answers.drop(3).count(identity) should be <= 1

        List(Frequency.P1M, Frequency.P2W, Frequency.P6M, Frequency.TERM).foreach { frequency =>
          List(false, true).foreach { preferEndOfMonth =>
            withClue(s"${frequency.name}/$preferEndOfMonth: ") {
              val derived: RollConvention =
                convention.toRollConvention(start, end, frequency, preferEndOfMonth)
              RollConvention.values.toList should contain(derived)
              derived.name should not be empty
            }
          }
        }

        // The two package-private operations are total in the same way: neither raises for any
        // combination of the flags or for any pair of dates, the stub validation reporting its
        // rejections as a value instead.
        List(false, true).foreach { explicitInitialStub =>
          List(false, true).foreach { explicitFinalStub =>
            withClue(s"$explicitInitialStub/$explicitFinalStub: ") {
              noException should be thrownBy
                convention.toImplicit(definitionText, explicitInitialStub, explicitFinalStub)
            }
          }
        }
        noException should be thrownBy convention.isStubLong(start, end)
        noException should be thrownBy convention.isStubLong(end, start)
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_toRollConvention") {
    // The month-end control row states `DAY_16`, which is what the day-of-month factory yields
    // for 16.
    RollConvention.ofDayOfMonth(16) shouldBe Right(RollConventions.DAY_16)

    dataRoll should have size 65
    forAll(dataRoll) {
      (convention: StubConvention,
          start: LocalDate,
          end: LocalDate,
          frequency: Frequency,
          preferEndOfMonth: Boolean,
          expected: RollConvention) =>
        withClue(
          s"${convention.name} $start to $end at ${frequency.name}, eom $preferEndOfMonth: ") {
          convention.toRollConvention(start, end, frequency, preferEndOfMonth) shouldBe expected
        }
    }
  }

  //-------------------------------------------------------------------------
  test("test_toImplicit") {
    // Eighteen of the thirty-two rows expect a convention and fourteen expect a `Left` carrying
    // `Failure.Invalid`, so both halves of the table are assertions about a value.
    //
    // Each row also asserts the by-name contract of the `definition` parameter: the counter
    // records how often the text is rendered, and it is rendered exactly once when the call
    // rejects - the failure carrying it under the `definition` attribute - and not at all when the
    // call succeeds, which is the whole point of taking it by name.
    dataImplicit should have size 32
    dataImplicit.count { case (_, _, _, expected) => expected.isEmpty } shouldBe 14

    forAll(dataImplicit) {
      (convention: StubConvention,
          explicitInitialStub: Boolean,
          explicitFinalStub: Boolean,
          expected: Option[StubConvention]) =>
        val renderings: AtomicInteger = new AtomicInteger(0)
        val result: Either[Failure, StubConvention] =
          convention.toImplicit(
            {
              val _ = renderings.incrementAndGet()
              definitionText
            },
            explicitInitialStub,
            explicitFinalStub)

        withClue(s"${convention.name}($explicitInitialStub, $explicitFinalStub): ") {
          expected match {
            case Some(effective) =>
              result shouldBe Right(effective)
              result should haveValue(effective)
              sc(result) should be theSameInstanceAs effective
              renderings.get shouldBe 0
            case None =>
              result should beFailure
              result should beFailureWith(FailureReason.INVALID)
              val failure: Failure = failureOf(result)
              failure.reason shouldBe FailureReason.INVALID
              failure.message should not be empty
              failure.message.toLowerCase(Locale.ENGLISH) should include("stub")
              failure.attributes.get("definition") shouldBe Some(definitionText)
              renderings.get shouldBe 1
          }
        }
    }

    // The six rejection messages, asserted verbatim because a caller reading a rejected schedule
    // definition is shown them. `SmartInitial` and `SmartFinal` are absent because they reject
    // nothing, and `None` and `Both` each have one message covering all of their rejected
    // combinations.
    failureOf(StubConvention.NONE.toImplicit(definitionText, true, false)).message shouldBe
      "Dates specify an explicit stub, but stub convention is 'None'"
    failureOf(StubConvention.SHORT_INITIAL.toImplicit(definitionText, false, true)).message shouldBe
      "Dates specify an explicit final stub, but stub convention is 'ShortInitial'"
    failureOf(StubConvention.LONG_INITIAL.toImplicit(definitionText, false, true)).message shouldBe
      "Dates specify an explicit final stub, but stub convention is 'LongInitial'"
    failureOf(StubConvention.SHORT_FINAL.toImplicit(definitionText, true, false)).message shouldBe
      "Dates specify an explicit initial stub, but stub convention is 'ShortFinal'"
    failureOf(StubConvention.LONG_FINAL.toImplicit(definitionText, true, false)).message shouldBe
      "Dates specify an explicit initial stub, but stub convention is 'LongFinal'"
    failureOf(StubConvention.BOTH.toImplicit(definitionText, false, false)).message shouldBe
      "Stub convention is 'Both' but explicit dates not specified"
  }

  //-------------------------------------------------------------------------
  test("test_isStubLong") {
    dataIsStubLong should have size 14
    forAll(dataIsStubLong) {
      (convention: StubConvention, date1: LocalDate, date2: LocalDate, expected: Boolean) =>
        withClue(s"${convention.name} from $date1 to $date2: ") {
          convention.isStubLong(date1, date2) shouldBe expected
        }
    }

    // The rule over the whole seven-day neighbourhood: for both smart members a gap shorter than
    // seven days merges and a gap of seven days or more is retained. The two long members ignore
    // the dates entirely and the other four always answer false, which makes the smart members the
    // only ones whose answer is a function of the dates.
    val anchor: LocalDate = date(2018, 6, 1)
    (0 to 14).foreach { days =>
      val other: LocalDate = anchor.plusDays(days.toLong)
      withClue(s"gap of $days days: ") {
        StubConvention.SMART_INITIAL.isStubLong(anchor, other) shouldBe days < 7
        StubConvention.SMART_FINAL.isStubLong(anchor, other) shouldBe days < 7
        StubConvention.LONG_INITIAL.isStubLong(anchor, other) shouldBe true
        StubConvention.LONG_FINAL.isStubLong(anchor, other) shouldBe true
        StubConvention.NONE.isStubLong(anchor, other) shouldBe false
        StubConvention.SHORT_INITIAL.isStubLong(anchor, other) shouldBe false
        StubConvention.SHORT_FINAL.isStubLong(anchor, other) shouldBe false
        StubConvention.BOTH.isStubLong(anchor, other) shouldBe false
      }
    }

    StubConvention.SMART_INITIAL.isStubLong(anchor, anchor.plusDays(6L)) shouldBe true
    StubConvention.SMART_FINAL.isStubLong(anchor, anchor.plusDays(6L)) shouldBe true
    StubConvention.SMART_INITIAL.isStubLong(anchor, anchor.plusDays(7L)) shouldBe false
    StubConvention.SMART_FINAL.isStubLong(anchor, anchor.plusDays(7L)) shouldBe false
    StubConvention.SMART_INITIAL.isStubLong(anchor, anchor.plusDays(8L)) shouldBe false
    StubConvention.SMART_FINAL.isStubLong(anchor, anchor.plusDays(8L)) shouldBe false

    // A reversed pair is absorbed, its gap being negative and therefore below the threshold.
    StubConvention.SMART_INITIAL.isStubLong(anchor, anchor.minusDays(1L)) shouldBe true
    StubConvention.SMART_FINAL.isStubLong(anchor, anchor.minusDays(30L)) shouldBe true

    // The rule is total over the whole of `LocalDate`: the gap is measured rather than stepped, so
    // the pairs below - at the extremes of the range, where stepping seven days forward from the
    // first date would overflow - are answered rather than refused.
    StubConvention.SMART_INITIAL.isStubLong(LocalDate.MAX.minusDays(1L), LocalDate.MAX) shouldBe
      true
    StubConvention.SMART_FINAL.isStubLong(LocalDate.MAX.minusDays(1L), LocalDate.MAX) shouldBe true
    StubConvention.SMART_INITIAL.isStubLong(LocalDate.MIN, LocalDate.MAX) shouldBe false
    StubConvention.SMART_FINAL.isStubLong(LocalDate.MIN, LocalDate.MAX) shouldBe false
    StubConvention.SMART_INITIAL.isStubLong(LocalDate.MAX, LocalDate.MIN) shouldBe true
    StubConvention.SMART_FINAL.isStubLong(LocalDate.MAX, LocalDate.MIN) shouldBe true
  }

  //-------------------------------------------------------------------------
  // Each of the eight tests below asserts the same six predicates in the same order -
  // isCalculateForwards, isCalculateBackwards, isFinal, isLong, isShort, isSmart - so that the
  // classification of a member is read in one place, one member at a time.
  test("test_NONE") {
    StubConvention.NONE.isCalculateForwards shouldBe true
    StubConvention.NONE.isCalculateBackwards shouldBe false
    StubConvention.NONE.isFinal shouldBe false
    StubConvention.NONE.isLong shouldBe false
    StubConvention.NONE.isShort shouldBe false
    StubConvention.NONE.isSmart shouldBe false
  }

  test("test_SHORT_INITIAL") {
    StubConvention.SHORT_INITIAL.isCalculateForwards shouldBe false
    StubConvention.SHORT_INITIAL.isCalculateBackwards shouldBe true
    StubConvention.SHORT_INITIAL.isFinal shouldBe false
    StubConvention.SHORT_INITIAL.isLong shouldBe false
    StubConvention.SHORT_INITIAL.isShort shouldBe true
    StubConvention.SHORT_INITIAL.isSmart shouldBe false
  }

  test("test_LONG_INITIAL") {
    StubConvention.LONG_INITIAL.isCalculateForwards shouldBe false
    StubConvention.LONG_INITIAL.isCalculateBackwards shouldBe true
    StubConvention.LONG_INITIAL.isFinal shouldBe false
    StubConvention.LONG_INITIAL.isLong shouldBe true
    StubConvention.LONG_INITIAL.isShort shouldBe false
    StubConvention.LONG_INITIAL.isSmart shouldBe false
  }

  test("test_SMART_INITIAL") {
    StubConvention.SMART_INITIAL.isCalculateForwards shouldBe false
    StubConvention.SMART_INITIAL.isCalculateBackwards shouldBe true
    StubConvention.SMART_INITIAL.isFinal shouldBe false
    StubConvention.SMART_INITIAL.isLong shouldBe false
    StubConvention.SMART_INITIAL.isShort shouldBe false
    StubConvention.SMART_INITIAL.isSmart shouldBe true
  }

  test("test_SHORT_FINAL") {
    StubConvention.SHORT_FINAL.isCalculateForwards shouldBe true
    StubConvention.SHORT_FINAL.isCalculateBackwards shouldBe false
    StubConvention.SHORT_FINAL.isFinal shouldBe true
    StubConvention.SHORT_FINAL.isLong shouldBe false
    StubConvention.SHORT_FINAL.isShort shouldBe true
    StubConvention.SHORT_FINAL.isSmart shouldBe false
  }

  test("test_LONG_FINAL") {
    StubConvention.LONG_FINAL.isCalculateForwards shouldBe true
    StubConvention.LONG_FINAL.isCalculateBackwards shouldBe false
    StubConvention.LONG_FINAL.isFinal shouldBe true
    StubConvention.LONG_FINAL.isLong shouldBe true
    StubConvention.LONG_FINAL.isShort shouldBe false
    StubConvention.LONG_FINAL.isSmart shouldBe false
  }

  test("test_SMART_FINAL") {
    StubConvention.SMART_FINAL.isCalculateForwards shouldBe true
    StubConvention.SMART_FINAL.isCalculateBackwards shouldBe false
    StubConvention.SMART_FINAL.isFinal shouldBe true
    StubConvention.SMART_FINAL.isLong shouldBe false
    StubConvention.SMART_FINAL.isShort shouldBe false
    StubConvention.SMART_FINAL.isSmart shouldBe true
  }

  test("test_BOTH") {
    StubConvention.BOTH.isCalculateForwards shouldBe false
    StubConvention.BOTH.isCalculateBackwards shouldBe false
    StubConvention.BOTH.isFinal shouldBe false
    StubConvention.BOTH.isLong shouldBe false
    StubConvention.BOTH.isShort shouldBe false
    StubConvention.BOTH.isSmart shouldBe false
  }

  //-------------------------------------------------------------------------
  test("test_toString") {
    dataName should have size 8
    forAll(dataName) { (convention: StubConvention, name: String) =>
      withClue(s"$name: ") {
        convention.toString shouldBe name
        // `toString`, `name` and `Show` are one string, asserted together so that a member
        // interpolated into a message, rendered through the type class or written to JSON does not
        // disagree with itself.
        convention.name shouldBe name
        Show[StubConvention].show(convention) shouldBe name
      }
    }
  }

  test("test_of_lookup") {
    // A canonical name is asserted through both entry points, the exact lookup and the lenient
    // one, so their answers are compared on the same input. The identity assertion shows the
    // lookup hands back the member itself rather than a copy.
    forAll(dataName) { (convention: StubConvention, name: String) =>
      withClue(s"$name: ") {
        StubConvention.valueOf(name) shouldBe Some(convention)
        StubConvention.parse(name) should haveValue(convention)
        StubConvention.parse(name) should beSuccess
        resolvedByName(name) should be theSameInstanceAs convention
      }
    }
  }

  test("test_of_lookupUpperCase") {
    // A member accepts three distinct spellings where its name is one word - `None`, `NONE`,
    // `none` - and five where it is compound. The upper-case fold of the rendered name is among
    // them as a registered key, so `SHORTINITIAL` resolves through the exact lookup and not
    // merely through the lenient one.
    forAll(dataName) { (convention: StubConvention, name: String) =>
      val upperCase: String = name.toUpperCase(Locale.ENGLISH)
      withClue(s"$upperCase: ") {
        StubConvention.valueOf(upperCase) shouldBe Some(convention)
        StubConvention.parse(upperCase) should haveValue(convention)
        sc(StubConvention.parse(upperCase)) should be theSameInstanceAs convention
      }
    }
  }

  test("test_of_lookupLowerCase") {
    // The lower-case fold of the rendered name is another of those three or five spellings, and
    // it is supplied by the family's alternate-name table, a fold to lower case not being derived
    // from a canonical name the way the upper-case key is. It resolves exactly too.
    forAll(dataName) { (convention: StubConvention, name: String) =>
      val lowerCase: String = name.toLowerCase(Locale.ENGLISH)
      withClue(s"$lowerCase: ") {
        StubConvention.valueOf(lowerCase) shouldBe Some(convention)
        StubConvention.parse(lowerCase) should haveValue(convention)
        sc(StubConvention.parse(lowerCase)) should be theSameInstanceAs convention
      }
    }
  }

  test("test_of_lookup_notFound") {
    // Text naming no member is reported as a value: reason `PARSING`, and a message naming both
    // the family and the text that could not be resolved - the two things a caller has to be told.
    // Nothing is raised.
    StubConvention.valueOf("Rubbish") shouldBe None
    StubConvention.parse("Rubbish") should beFailure
    StubConvention.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
    StubConvention.parse("Rubbish") should
      haveFailureMessageMatching("StubConvention name not found: Rubbish")
    noException should be thrownBy StubConvention.parse("Rubbish")
  }

  test("test_of_lookup_null") {
    // Both entry points take one required `String`: a call that omits the argument, or supplies a
    // value of another type, does not compile.
    assertDoesNotCompile("StubConvention.parse()")
    assertDoesNotCompile("StubConvention.valueOf()")
    assertDoesNotCompile("StubConvention.parse(1)")
    assertDoesNotCompile("StubConvention.valueOf(StubConvention.NONE)")

    // The rest of the case is text that names nothing: empty, blank and whitespace-only text, the
    // literal word `null`, and several near-misses - among them the spellings that differ from a
    // name by a space, a hyphen, a doubled underscore or a leading or trailing space.
    val hostile: List[String] =
      List(
        "",
        " ",
        "   ",
        "\t\n",
        "null",
        "Rubbish",
        "Short Initial",
        "Short-Initial",
        "SHORT__INITIAL",
        "Initial",
        "Final",
        "Smart",
        " None",
        "None ")

    hostile.foreach { name =>
      withClue(s"[$name]: ") {
        StubConvention.valueOf(name) shouldBe None
        StubConvention.parse(name) should beFailureWith(FailureReason.PARSING)
        noException should be thrownBy StubConvention.parse(name)
      }
    }
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // Exactly eight members, distinct as values and distinct by name, in declaration order rather
    // than the alphabetical order the `Order` instance imposes.
    StubConvention.values.toList should have size 8
    StubConvention.values.toList.distinct should have size 8
    StubConvention.values.toList.map(_.name).distinct should have size 8
    StubConvention.values.toList shouldBe declarationOrder
    NamedEnum[StubConvention] should be theSameInstanceAs StubConvention.namedEnum
    NamedEnum[StubConvention].values.toList shouldBe declarationOrder

    // A ninth member cannot be declared. The type is sealed and every member of it is declared in
    // the one file the type is declared in, so no other file can extend it - not even this spec,
    // which sits in the very package the constructor is visible in, so the refusal is structural
    // rather than a matter of the constructor being out of reach. It is asserted at compile time
    // because at run time there is nothing left to observe.
    assertDoesNotCompile("""final class NinthConvention extends StubConvention("Ninth")""")
    assertDoesNotCompile("""case object NINTH extends StubConvention("Ninth")""")

    StubConvention.values.toList.foreach { convention =>
      withClue(s"${convention.name}: ") {
        StubConvention.valueOf(convention.name) shouldBe Some(convention)
        StubConvention.parse(convention.name) should haveValue(convention)
        Show[StubConvention].show(convention) shouldBe convention.name
        convention.toString shouldBe convention.name
        sc(StubConvention.parse(convention.name)) should be theSameInstanceAs convention
      }
    }

    // The whole accepted name space of the family, member by member. Six candidate spellings are
    // derived from each member - the rendered name, the identifier, and each of those folded to
    // upper and to lower case - of which three are distinct for `None` and `Both` ("None",
    // "NONE", "none") and five for the six compound members, an identifier already being upper
    // case ("ShortInitial", "SHORTINITIAL", "shortinitial", "SHORT_INITIAL", "short_initial").
    // Each distinct spelling is asserted through the exact lookup as well as through `parse`, so
    // a lost alternate-name row cannot hide behind case-tolerant parsing.
    val identifiers: List[(StubConvention, String)] =
      List(
        StubConvention.NONE -> "NONE",
        StubConvention.SHORT_INITIAL -> "SHORT_INITIAL",
        StubConvention.LONG_INITIAL -> "LONG_INITIAL",
        StubConvention.SMART_INITIAL -> "SMART_INITIAL",
        StubConvention.SHORT_FINAL -> "SHORT_FINAL",
        StubConvention.LONG_FINAL -> "LONG_FINAL",
        StubConvention.SMART_FINAL -> "SMART_FINAL",
        StubConvention.BOTH -> "BOTH")

    identifiers should have size 8
    identifiers.foreach {
      case (convention, identifier) =>
        val spellings: List[String] =
          List(
            convention.name,
            convention.name.toUpperCase(Locale.ENGLISH),
            convention.name.toLowerCase(Locale.ENGLISH),
            identifier,
            identifier.toUpperCase(Locale.ENGLISH),
            identifier.toLowerCase(Locale.ENGLISH))
        spellings.distinct.foreach { spelling =>
          withClue(s"$spelling: ") {
            StubConvention.valueOf(spelling) shouldBe Some(convention)
            StubConvention.parse(spelling) should haveValue(convention)
          }
        }
    }

    // The name lookup itself. This family declares no lenient pattern and no external name group,
    // and the two empty tables are asserted rather than left unexamined, so their emptiness is
    // behaviour of the family rather than an untested gap.
    val lookup: NamedEnum[StubConvention] = StubConvention.namedEnum
    lookup.familyName shouldBe "StubConvention"
    lookup.toString shouldBe "NamedEnum[StubConvention]"
    lookup.byCanonicalName should have size 8
    lookup.byCanonicalName.keySet shouldBe declarationOrder.map(_.name).toSet
    lookup.byUpperName should have size 8
    lookup.byUpperName.keySet shouldBe
      declarationOrder.map(_.name.toUpperCase(Locale.ENGLISH)).toSet
    lookup.lenientPatterns shouldBe empty
    lookup.externalNameGroups shouldBe Set.empty[String]
    lookup.externalNames("FpML") shouldBe None
    lookup.externalNamesRaw("SWIFT") shouldBe None

    // Every row of the alternate-name table names a member of this family and resolves to it, so
    // no row points at a name the family does not carry.
    lookup.alternateNames should not be empty
    lookup.alternateNames.foreach {
      case (spelling, canonicalName) =>
        withClue(s"$spelling -> $canonicalName: ") {
          declarationOrder.map(_.name) should contain(canonicalName)
          StubConvention.valueOf(spelling).map(_.name) shouldBe Some(canonicalName)
        }
    }

    // One equality-bearing instance - an ordering that is also a hashing - so equality, hashing,
    // ordering and `==` are one relation. Every ordered pair of the eight members is asked all of
    // it, and `compare == 0` holds exactly for the equal pairs.
    declarationOrder.foreach { left =>
      declarationOrder.foreach { right =>
        val sameValue: Boolean = left == right
        withClue(s"${left.name} against ${right.name}: ") {
          Eq[StubConvention].eqv(left, right) shouldBe sameValue
          Hash[StubConvention].eqv(left, right) shouldBe sameValue
          (Order[StubConvention].compare(left, right) == 0) shouldBe sameValue
          if (sameValue) {
            Hash[StubConvention].hash(left) shouldBe Hash[StubConvention].hash(right)
            left.hashCode shouldBe right.hashCode
          } else {
            Order[StubConvention].compare(left, right) should not be 0
          }
        }
      }
    }

    // The ordering is by name, and by name alone, so it is alphabetical and deliberately not the
    // declaration order: `Both` is the last member declared and the first in this list.
    declarationOrder.sorted(Order[StubConvention].toOrdering).map(_.name) shouldBe
      List(
        "Both",
        "LongFinal",
        "LongInitial",
        "None",
        "ShortFinal",
        "ShortInitial",
        "SmartFinal",
        "SmartInitial")

    // The member whose canonical name is `None` is declared as `NONE`, so that it cannot shadow
    // `scala.None` in the companion or at a call site. The rename is of the Scala identifier only:
    // the name, what `Show` prints and what the codec writes are all `None`.
    StubConvention.NONE.name shouldBe "None"
    StubConvention.valueOf("None") shouldBe Some(StubConvention.NONE)
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // The codec the companion publishes is the one route to and from JSON, and the whole family
    // shares that single instance. A document is the bare canonical name and never an object, so
    // the round trip is asserted over two members and then over all eight.
    val codec: Codec[StubConvention] = implicitly[Codec[StubConvention]]
    codec should be theSameInstanceAs StubConvention.codec

    // The element type is named because the JSON encoder is invariant in it: a list left to be
    // inferred would have the intersection of the two singleton types as its element type, which
    // no codec of this family is published for.
    List[StubConvention](StubConvention.NONE, StubConvention.SHORT_FINAL).foreach { convention =>
      withClue(s"${convention.name}: ") {
        val encoded: Json = convention.asJson
        encoded shouldBe Json.fromString(convention.name)
        encoded.isString shouldBe true
        encoded.isObject shouldBe false
        encoded.noSpaces shouldBe s""""${convention.name}""""
        encoded.as[StubConvention] shouldBe Right(convention)
      }
    }

    (StubConvention.NONE: StubConvention).asJson.noSpaces shouldBe "\"None\""
    (StubConvention.SHORT_FINAL: StubConvention).asJson.noSpaces shouldBe "\"ShortFinal\""

    StubConvention.values.toList.foreach { convention =>
      withClue(s"${convention.name}: ") {
        convention.asJson shouldBe Json.fromString(convention.name)
        convention.asJson.as[StubConvention] shouldBe Right(convention)
        sc(convention.asJson.as[StubConvention]) should be theSameInstanceAs convention
      }
    }

    // Text naming no member is rejected by the reader, as is a document of the wrong JSON type -
    // the codec reads a string and nothing else, which is what keeps an unrecognised name a
    // decoding failure rather than an exception.
    Json.fromString("Rubbish").as[StubConvention].isLeft shouldBe true
    Json.fromString("").as[StubConvention].isLeft shouldBe true
    Json.fromInt(1).as[StubConvention].isLeft shouldBe true
    Json.Null.as[StubConvention].isLeft shouldBe true
    Json.obj("name" -> Json.fromString("None")).as[StubConvention].isLeft shouldBe true

    // The reader is as lenient as `parse`, which is what lets a document written by hand, or by a
    // system that spells a member as its identifier, still be read.
    Json.fromString("SHORTFINAL").as[StubConvention] shouldBe Right(StubConvention.SHORT_FINAL)
    Json.fromString("shortfinal").as[StubConvention] shouldBe Right(StubConvention.SHORT_FINAL)
    Json.fromString("SHORT_FINAL").as[StubConvention] shouldBe Right(StubConvention.SHORT_FINAL)
    Json.fromString("sHoRtFiNaL").as[StubConvention] shouldBe Right(StubConvention.SHORT_FINAL)
  }

  test("test_jodaConvert") {
    // `Show`, `toString` and `parse` are the string conversion of this family: a member renders as
    // its name and is recovered from that name. Asserted over two members and then over the whole
    // family.
    List[StubConvention](StubConvention.NONE, StubConvention.SHORT_FINAL).foreach { convention =>
      withClue(s"${convention.name}: ") {
        Show[StubConvention].show(convention) shouldBe convention.name
        convention.toString shouldBe convention.name
        StubConvention.parse(convention.name) should haveValue(convention)
        sc(StubConvention.parse(Show[StubConvention].show(convention))) should
          be theSameInstanceAs convention
      }
    }

    StubConvention.values.toList.foreach { convention =>
      withClue(s"${convention.name}: ") {
        StubConvention.parse(convention.toString) should haveValue(convention)
        resolvedByName(Show[StubConvention].show(convention)) should
          be theSameInstanceAs convention
      }
    }
  }

  //-------------------------------------------------------------------------
  // The fixtures of the hostile-name case. The definitions below are built through the ordinary
  // factory and rendered by their own `toString`, so the text driven into the rejection paths is
  // the text `PeriodicSchedule` itself passes rather than a string this spec composed.

  /**
   * A calendar name carrying a line feed and text that reads as a log line of its own.
   *
   * This is the shape a forged log entry takes: everything after the line feed would appear as a
   * line of its own wherever the failure was written out unescaped, stating something this library
   * never reported (CWE-117).
   */
  private val ForgedCalendarName: String = "GBLO\nINVALID: forged"

  /** A calendar name of a few thousand characters, which no part of the API rejects. */
  private val OversizedCalendarName: String = "GBLO\n" + ("FORGED " * 600)

  /**
   * The ceiling the text of a rejection is asserted to stay under.
   *
   * The bound is a concrete number comfortably above an ordinary rendering rather than an exact
   * length, so that it states what it is for - the text of a failure stays a line that can be read
   * - without pinning the rendering down character by character. A rejection of this family
   * renders as its reason, one message part and one attribute whose key is the ten-character
   * `definition`, each part bounded where it is written, so no rejection can reach this ceiling
   * however large the definition handed to it. The oversized name below is more than three times
   * this ceiling and the definition carries the whole of it, so the bound a rejection applies as
   * it writes its text is the only thing keeping that text readable.
   */
  private val RenderingCeiling: Int = 1200

  /**
   * The forged name as a reader of a diagnostic receives it, with its line feed written as the two
   * characters of an escape.
   *
   * This is what the text form of the '''rejection''' produces for that name, so it is what the
   * text of the rejection carries, whether that text is reached through `Show` or through the
   * failure's own text form. It is not what the identifier, the definition's rendering or the
   * attribute taken from that rendering carries: each of those is the value rather than a
   * diagnostic written out, so each carries the name as it stands.
   */
  private val NeutralisedForgedName: String = """GBLO\nINVALID: forged"""

  /** The definition whose rendering carries the forged calendar name. */
  private val forgedDefinition: PeriodicSchedule = definitionAdjustedBy(ForgedCalendarName)

  /** The definition whose rendering carries a calendar name of a few thousand characters. */
  private val oversizedDefinition: PeriodicSchedule = definitionAdjustedBy(OversizedCalendarName)

  //-------------------------------------------------------------------------
  test("test_rejectedDefinitionSurvivesHostileCalendarNames") {
    // The definition really does carry the name, so the text this case drives into the rejection
    // paths is the text a caller that parsed such a name would have arrived at - and it carries it
    // as the caller supplied it, because a definition renders its calendar by asking the
    // identifier for its text form, which is the whole of the name the identifier was built from
    // (AAP §0.1.1). The identifier's name and its text form are one text.
    forgedDefinition.businessDayAdjustment.calendar.name shouldBe ForgedCalendarName
    forgedDefinition.toString should include(ForgedCalendarName)

    // The first half, on the path that rejects an explicit stub the convention forbids: the
    // attribute is the rendering of the definition, character for character.
    val reported: Failure =
      failureOf(StubConvention.NONE.toImplicit(forgedDefinition.toString, true, false))
    reported.reason shouldBe FailureReason.INVALID
    reported.message shouldBe "Dates specify an explicit stub, but stub convention is 'None'"
    reported.attributes.get("definition") shouldBe Some(forgedDefinition.toString)
    reported.attributes("definition") should include(ForgedCalendarName)

    // The second half: the text of that same failure is one bounded line, the line feed appearing
    // in it as the two characters of its escape and nowhere as itself, so the failure cannot be
    // read as two lines (CWE-117) and cannot be made large by the definition it quotes (CWE-400).
    // This is where a name a caller supplied is made safe to write out.
    val rendered: String = neutralisedRendering(reported)
    rendered should include(NeutralisedForgedName)
    rendered should include(reported.message)
    rendered should not include "\n"
    rendered.linesIterator.size shouldBe 1
    rendered.length should be <= RenderingCeiling

    // The same two halves on a second rejection path, reported by a different member with a
    // different message, since each member attaches the definition the same way.
    val alsoReported: Failure =
      failureOf(StubConvention.SHORT_INITIAL.toImplicit(forgedDefinition.toString, false, true))
    alsoReported.attributes.get("definition") shouldBe Some(forgedDefinition.toString)
    alsoReported.attributes("definition") should include(ForgedCalendarName)
    val alsoRendered: String = neutralisedRendering(alsoReported)
    alsoRendered should include(NeutralisedForgedName)
    alsoRendered should not include "\n"
    alsoRendered.linesIterator.size shouldBe 1
    alsoRendered.length should be <= RenderingCeiling

    // The oversized case. The identifier keeps every character of a name of a few thousand
    // characters, so the definition's rendering - and the attribute taken from it - carries the
    // whole name and is therefore longer than the name itself, while the text of the failure
    // bounds what it writes as it writes it: that text carries the marker standing for what was
    // left out and stays under the same ceiling as every other rendering.
    val oversized: Failure =
      failureOf(StubConvention.BOTH.toImplicit(oversizedDefinition.toString, false, false))
    oversized.attributes.get("definition") shouldBe Some(oversizedDefinition.toString)
    oversized.attributes("definition") should include(OversizedCalendarName)
    oversized.attributes("definition").length should be > OversizedCalendarName.length
    oversizedDefinition.businessDayAdjustment.calendar.name shouldBe OversizedCalendarName
    OversizedCalendarName.length should be > RenderingCeiling
    val oversizedRendered: String = neutralisedRendering(oversized)
    oversizedRendered should include("...")
    oversizedRendered should not include "\n"
    oversizedRendered.linesIterator.size shouldBe 1
    oversizedRendered.length should be <= RenderingCeiling
  }

  //-------------------------------------------------------------------------
  /**
   * Builds a valid schedule definition adjusted by a calendar of the given name.
   *
   * `HolidayCalendarId.of` is total, so this is how a calendar name read from a document reaches a
   * definition, and the definition is accepted because the only invariants of the factory are over
   * its dates. Those dates are the ones [[definitionText]] renders, so every definition in this
   * spec describes the same schedule.
   *
   * @param calendarName  the name of the holiday calendar the definition is adjusted by
   * @return the definition, which the test renders with its own `toString`
   */
  private def definitionAdjustedBy(calendarName: String): PeriodicSchedule =
    PeriodicSchedule
      .of(
        date(2014, 6, 30),
        date(2015, 9, 30),
        Frequency.P3M,
        BusinessDayAdjustment
          .of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarId.of(calendarName)),
        StubConvention.NONE,
        false)
      .fold(
        failures =>
          fail(
            failures.toNonEmptyList.toList
              .map(failure => failure.message)
              .mkString("Expected a valid definition but the factory rejected it: ", "; ", "")),
        definition => definition)

  /**
   * Asserts of a failure the properties its text has to have, and answers with that text.
   *
   * The two routes to the text - the `Show` instance and the text form of the value - have to be
   * the same string, so a caller reaching for either gets the neutralised rendering. That string
   * is then asserted to be safe to write into a log or a report: it holds no character for which
   * `Character.isISOControl` holds, so it is one line and carries no terminal control, and it is
   * bounded, so a definition quoted by the failure cannot dominate it.
   *
   * @param failure  the failure whose text is asserted
   * @return the text of that failure
   */
  private def neutralisedRendering(failure: Failure): String = {
    val rendered: String = Show[Failure].show(failure)
    rendered shouldBe failure.toString
    rendered.exists(character => character.isControl) shouldBe false
    rendered.linesIterator.size shouldBe 1
    rendered.length should be <= RenderingCeiling
    rendered
  }
}
