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
 * Test [[StubConvention]], ported from the Java `StubConventionTest`.
 *
 * This is a one-to-one port: each of the Java class's twenty-one test methods has a test of the
 * same name here, in the same order, and none of them has been split or renamed. One case with no
 * Java counterpart follows them, `test_rejectedDefinitionSurvivesHostileCalendarNames`, described
 * at the foot of this comment. Eight of the Java methods were parameterised over five data
 * providers; each provider is transcribed row for row into one table, and the row loop lives
 * inside the single test that the Java method became, so the method-level traceability recorded in
 * `manifest/java-test-mapping.csv` stays exact:
 *
 *  - `data_types` - the 8 members of the family, driving `test_null`;
 *  - `data_roll` - '''65''' rows, driving `test_toRollConvention`;
 *  - `data_implicit` - '''32''' rows, driving `test_toImplicit`;
 *  - `data_isStubLong` - '''14''' rows, driving `test_isStubLong`;
 *  - `data_name` - 8 rows, driving `test_toString`, `test_of_lookup`, `test_of_lookupUpperCase`
 *    and `test_of_lookupLowerCase`.
 *
 * The manifest additionally records a `consolidated` row that routes the Java
 * `test_serialization` into `json.JsonRoundTripSpec`, which round-trips every codec-bearing type
 * of the module through generated values. The test of that name is kept here as well, and
 * deliberately: it is what pins the ''shape'' of this family's document - a bare name string and
 * never an object - beside the type it belongs to, where a reader of this spec will look for it.
 * The two are complementary, and the join the acceptance gate performs runs from a manifest row to
 * a test case, so a test case that no row names costs nothing and a row with no test case would
 * fail the gate.
 *
 * ===Why this spec lives in this package===
 *
 * Two of the members it exercises - [[StubConvention.toImplicit]] and
 * [[StubConvention.isStubLong]] - are `private[schedule]`, because only [[PeriodicSchedule]] has
 * any business calling them. Declaring this spec in `com.opengamma.strata.basics.schedule` is
 * what makes them reachable, so the package is part of the spec rather than an accident of where
 * the file sits. Nothing else is needed to reach them: no reflection, no accessor and no widening
 * of the production visibility.
 *
 * ===How the shape of the port changes the assertions===
 *
 *  - '''An invalid stub declaration is reported rather than raised.''' The Java `toImplicit` threw
 *    a `ScheduleException` for the fourteen rejected combinations of its two flags, and the Java
 *    test asserted a throw for each. That exception type has no target in this port (AAP 0.2.2),
 *    so `toImplicit` returns `Either[Failure, StubConvention]` and the fourteen rejections are
 *    asserted as a `Left` carrying [[com.opengamma.strata.collect.result.Failure.Invalid]] -
 *    reason `INVALID` - whose message is the Java message verbatim and whose `definition`
 *    attribute holds the rendered schedule definition. No test in this file asserts a throw.
 *  - '''The definition is passed as rendered text, by name.''' The Java method took the
 *    `PeriodicSchedule` itself and the Java test passed the absent reference for it. The ported
 *    parameter is a by-name `String`, evaluated only on the path that rejects, so this spec passes
 *    a representative rendering and additionally asserts the by-name contract: the text is
 *    rendered exactly once when the call rejects and not at all when it succeeds. That is a
 *    stronger statement than the Java test made, and it is the reason no `null` is written here.
 *  - '''The throwing factory became two reported ones.''' `StubConvention.of(name)` raised for
 *    text naming no member; [[StubConvention.valueOf]] answers with an `Option` and
 *    [[StubConvention.parse]] with `EitherNec[Failure, _]`. Both are asserted wherever the Java
 *    test asserted `of`, so the exact lookup and the lenient one cannot drift apart.
 *  - '''`test_null` has no direct target.''' The Java method passed the absent reference for each
 *    of the three reference arguments of `toRollConvention` and asserted that all three calls
 *    raised. The `notNull` guards behind those throws guarded against something a Scala caller
 *    cannot express: two dates and a frequency are required parameters of required types. The
 *    case is therefore asserted as a compile-time proof that the four arguments are required,
 *    followed by the totality those guards were protecting - every one of the eight members
 *    answers every one of its operations, for every frequency shape, without raising.
 *  - '''`coverage` has no target either.''' The Java method was `coverEnum(StubConvention.class)`,
 *    which reflectively read the values of a Java enum so that a coverage tool would not report
 *    them as unexercised. There is no enum and no reflection here, so what the call stood for is
 *    asserted directly, as the closedness of the family: exactly eight members in declaration
 *    order, each round-tripping through its own name, and a ninth member provably impossible to
 *    declare. That assertion is what carries Rule 4 for the stub conventions.
 *  - '''`test_serialization` and `test_jodaConvert` had reflective subjects.''' The first round-
 *    tripped a member through Java serialization and through the binary and JSON encodings of the
 *    bean library the Java type belonged to; the second through the reflective string-conversion
 *    library its two annotations registered with. None of those is a dependency of this port, and
 *    neither Java serialization nor wire compatibility with that library's JSON is in its scope
 *    (AAP 0.2.2). The circe codec replaced the first and `Show`, `toString` and `parse` replaced
 *    the second, so the two tests assert exactly those.
 *
 * ===The one added case===
 *
 * The definition a rejection carries is text the caller rendered, and part of that text is not
 * constrained by this library: a definition names a business day adjustment, which names a
 * holiday calendar identifier whose name `HolidayCalendarId.of` accepts as given, so it may hold
 * a line feed or several thousand characters. `test_rejectedDefinitionSurvivesHostileCalendarNames`
 * drives such a definition - a real [[PeriodicSchedule]], rendered by its own `toString` - into
 * three of the rejection paths above and asserts the two properties that have to hold together.
 * The `definition` attribute carries that text exactly, line feed and all, because it stands in
 * for the field the ported exception carried, while the text of the failure itself is one bounded
 * line holding no character a reader of lines could act on, because bounding and escaping belong
 * to the writing of a failure rather than to its construction.
 *
 * Numerical parity against the Java implementation is not this spec's subject: the effect of a
 * stub convention on a generated schedule is pinned to the captured Java baseline, to 1e-9
 * absolute and relative, by `parity.ScheduleParitySpec`. The assertions here are behavioural, and
 * they compare dates, conventions and booleans exactly.
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
   * The eight members in the declaration order of the Java enum constants.
   *
   * This is the order [[StubConvention.values]] is expected to publish, and it is deliberately
   * not the order the `Order` instance imposes, which is alphabetical by name. Writing it out
   * rather than reading it from `values` is what lets `coverage` assert the order rather than
   * assume it.
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
   * The Java test passed the absent reference where the ported method took the definition itself,
   * because the definition was used only to build the message of the exception. The ported
   * parameter is the text that definition renders to, taken by name, so this is the text this
   * spec supplies: it stands in for `PeriodicSchedule.toString`, which is what
   * [[PeriodicSchedule]] itself passes, and it is distinctive enough that finding it under the
   * `definition` attribute of a failure proves the argument arrived rather than merely that some
   * attribute exists.
   */
  private val definitionText: String =
    "PeriodicSchedule{startDate=2014-06-30, endDate=2015-09-30, frequency=P3M}"

  /**
   * Unwraps a result that the test expects to hold a convention.
   *
   * The lookups of this family report rejection on the left of an `Either` rather than raising,
   * so a test that needs the convention itself - to compare its identity, say - has to open the
   * result. Opening it here rather than at each call site means a result that unexpectedly holds
   * a failure fails the test naming that failure, instead of failing later with a less useful
   * message.
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
   * This is the counterpart of [[sc]] for the fourteen rejected rows of `data_implicit`: the
   * matchers say that the result is a failure carrying a reason, and this opens it so that its
   * message and its `definition` attribute can be asserted too.
   *
   * @param result  the result to unwrap
   * @return the failure the result holds
   */
  private def failureOf(result: Either[Failure, StubConvention]): Failure =
    result.fold(identity, effective => fail(s"expected a failure but got $effective"))

  /**
   * Looks a convention up by name exactly, failing the test when the name resolves to nothing.
   *
   * This is [[StubConvention.valueOf]] with the absent case turned into a test failure, which is
   * what lets an assertion about the ''identity'' of the value a name resolves to be written
   * without unsafely opening an `Option`.
   *
   * @param name  the name to look up
   * @return the convention with that name
   */
  private def resolvedByName(name: String): StubConvention =
    StubConvention.valueOf(name).getOrElse(fail(s"no convention is named $name"))

  //-------------------------------------------------------------------------
  /**
   * The Java `data_types` provider: every member of the family.
   *
   * The Java provider built its rows by reflecting over `StubConvention.values()`. The members
   * are written out here, which is the same eight rows and additionally fixes which eight they
   * are, and `coverage` asserts that this list and [[StubConvention.values]] agree.
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
   * The Java `data_name` provider: each member with the name it renders as.
   *
   * These eight strings are the identity of the family in text, in JSON and in every message that
   * interpolates a convention, so they are the rows that Java property-name preservation
   * (AAP 0.8.2) rests on for this type.
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
   * The Java `data_roll` provider, all 65 rows, in the order the provider listed them.
   *
   * Each row is a member, a start date, an end date, a frequency, the end-of-month preference and
   * the roll convention the six imply. The rows are grouped by member exactly as the Java provider
   * grouped them, and three groups of rows are worth naming because they are what the table is
   * really for:
   *
   *  - the fifteen `None` rows, which include the whole of the month-end special case: a pair of
   *    dates that disagree about the day of the month, lie in different months and has at least
   *    one of them at a month end yields `EOM` when the end of the month is preferred, and
   *    otherwise the later of the two days. The pair 2016-03-16 to 2016-03-31 is the control for
   *    the different-months condition, sharing a month and so resolving to `Day16`, while
   *    2016-03-16 to 2017-03-31 differs only in the year and resolves to `EOM`;
   *  - the eight `TERM` rows, one for each member that has any, which resolve to
   *    `RollConventions.NONE` because the term frequency is neither month-based nor week-based;
   *  - the week-based rows, which read a day of the week rather than a day of the month, and read
   *    it from the end date for the three initial members - 2014-08-16 is a Saturday - and from
   *    the start date for the rest - 2014-01-14 is a Tuesday. Those two dates are what makes the
   *    direction of the derivation observable.
   *
   * The two `SHORT_INITIAL` rows for `P2W` with the end-of-month preference set are a literal
   * duplicate in the Java provider, whose neighbouring groups carry one row with the preference
   * cleared and one with it set. The duplicate is transcribed rather than corrected, because this
   * table's job is to state what the Java provider stated; the case it omits - `P2W` with the
   * preference cleared - is covered for `SHORT_INITIAL` by the identical `LONG_INITIAL` and
   * `SMART_INITIAL` rows, all three members reading the same end date through the same code path.
   */
  private val dataRoll: TableFor6[StubConvention, LocalDate, LocalDate, Frequency, Boolean, RollConvention] =
    Table(
      ("convention", "start", "end", "frequency", "preferEndOfMonth", "expected"),
      // None - the day of the month of the start date, the day of the week for a week-based
      // frequency, and the month-end special case when the two dates leave the day undetermined.
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
      // The Java row read `RollConvention.ofDayOfMonth(16)`, which is `RollConventions.DAY_16`;
      // `test_toRollConvention` asserts that the two are the same value before using the table.
      (StubConvention.NONE, date(2016, MARCH, 16), date(2016, MARCH, 31), Frequency.P6M, true, RollConventions.DAY_16),
      (StubConvention.NONE, date(2016, MARCH, 16), date(2017, MARCH, 31), Frequency.P6M, true, RollConventions.EOM),
      // ShortInitial - rolls backwards, so every row reads the end date.
      (StubConvention.SHORT_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_16),
      (StubConvention.SHORT_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.DAY_16),
      (StubConvention.SHORT_INITIAL, date(2014, JANUARY, 14), date(2014, JUNE, 30), Frequency.P1M, false, RollConventions.DAY_30),
      (StubConvention.SHORT_INITIAL, date(2014, JANUARY, 14), date(2014, JUNE, 30), Frequency.P1M, true, RollConventions.EOM),
      (StubConvention.SHORT_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, true, RollConventions.DAY_SAT),
      (StubConvention.SHORT_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, true, RollConventions.DAY_SAT),
      (StubConvention.SHORT_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, false, RollConventions.NONE),
      (StubConvention.SHORT_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, true, RollConventions.NONE),
      // LongInitial - also backwards, and the same rows, because the direction is what decides.
      (StubConvention.LONG_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_16),
      (StubConvention.LONG_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.DAY_16),
      (StubConvention.LONG_INITIAL, date(2014, JANUARY, 14), date(2014, JUNE, 30), Frequency.P1M, false, RollConventions.DAY_30),
      (StubConvention.LONG_INITIAL, date(2014, JANUARY, 14), date(2014, JUNE, 30), Frequency.P1M, true, RollConventions.EOM),
      (StubConvention.LONG_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, false, RollConventions.DAY_SAT),
      (StubConvention.LONG_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, true, RollConventions.DAY_SAT),
      (StubConvention.LONG_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, false, RollConventions.NONE),
      (StubConvention.LONG_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, true, RollConventions.NONE),
      // SmartInitial - backwards as well; the seven-day rule of this member is about stub length,
      // not about the roll convention, so it changes nothing here.
      (StubConvention.SMART_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_16),
      (StubConvention.SMART_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.DAY_16),
      (StubConvention.SMART_INITIAL, date(2014, JANUARY, 14), date(2014, JUNE, 30), Frequency.P1M, false, RollConventions.DAY_30),
      (StubConvention.SMART_INITIAL, date(2014, JANUARY, 14), date(2014, JUNE, 30), Frequency.P1M, true, RollConventions.EOM),
      (StubConvention.SMART_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, false, RollConventions.DAY_SAT),
      (StubConvention.SMART_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, true, RollConventions.DAY_SAT),
      (StubConvention.SMART_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, false, RollConventions.NONE),
      (StubConvention.SMART_INITIAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, true, RollConventions.NONE),
      // ShortFinal - rolls forwards, so every row reads the start date.
      (StubConvention.SHORT_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_14),
      (StubConvention.SHORT_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.DAY_14),
      (StubConvention.SHORT_FINAL, date(2014, JUNE, 30), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_30),
      (StubConvention.SHORT_FINAL, date(2014, JUNE, 30), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.EOM),
      (StubConvention.SHORT_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, false, RollConventions.DAY_TUE),
      (StubConvention.SHORT_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, true, RollConventions.DAY_TUE),
      (StubConvention.SHORT_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, false, RollConventions.NONE),
      (StubConvention.SHORT_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, true, RollConventions.NONE),
      // LongFinal - forwards as well.
      (StubConvention.LONG_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_14),
      (StubConvention.LONG_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.DAY_14),
      (StubConvention.LONG_FINAL, date(2014, JUNE, 30), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_30),
      (StubConvention.LONG_FINAL, date(2014, JUNE, 30), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.EOM),
      (StubConvention.LONG_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, false, RollConventions.DAY_TUE),
      (StubConvention.LONG_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, true, RollConventions.DAY_TUE),
      (StubConvention.LONG_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, false, RollConventions.NONE),
      (StubConvention.LONG_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, true, RollConventions.NONE),
      // SmartFinal - forwards as well.
      (StubConvention.SMART_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_14),
      (StubConvention.SMART_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.DAY_14),
      (StubConvention.SMART_FINAL, date(2014, JUNE, 30), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_30),
      (StubConvention.SMART_FINAL, date(2014, JUNE, 30), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.EOM),
      (StubConvention.SMART_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, false, RollConventions.DAY_TUE),
      (StubConvention.SMART_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P2W, true, RollConventions.DAY_TUE),
      (StubConvention.SMART_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, false, RollConventions.NONE),
      (StubConvention.SMART_FINAL, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.TERM, true, RollConventions.NONE),
      // Both - neither forwards nor backwards by the two direction questions, so the derivation
      // falls through to the start date, as it does for a forward-rolling member.
      (StubConvention.BOTH, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, false, RollConventions.DAY_14),
      (StubConvention.BOTH, date(2014, JANUARY, 14), date(2014, AUGUST, 16), Frequency.P1M, true, RollConventions.DAY_14))

  /**
   * The Java `data_implicit` provider, all 32 rows, in the order the provider listed them.
   *
   * Each row is a member, the two flags saying whether an initial and a final stub have been
   * declared by dates, and the effective convention that combination implies. The Java provider
   * wrote the absent reference in the fourth column for the fourteen combinations the method
   * rejected; here that column is an `Option`, `None` standing for "this combination is rejected",
   * which is what lets the row loop assert a `Left` for it instead of a throw.
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
   * The Java `data_isStubLong` provider, all 14 rows, in the order the provider listed them.
   *
   * The first six rows ask every non-smart member about the same seven-day gap, which fixes the
   * two constant answers: the two long members always merge the stub into its neighbour and the
   * other four never do. The eight rows that follow are the seven-day rule of the two smart
   * members, approached from both sides of the threshold - one day and six days are shorter than
   * seven and merge, seven days and eight days are not and are retained - which is what makes the
   * threshold exclusive rather than inclusive.
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
    // Reinterpretation. The Java method passed the absent reference for each of the three
    // reference arguments of `toRollConvention` on every one of the eight members and asserted
    // that all three calls raised. The `notNull` guards behind those throws have no target here,
    // because what they guarded against cannot be expressed: two dates and a frequency are
    // required parameters of required types, so omitting one, or offering something that is not
    // one, is rejected when this spec is compiled rather than when it runs. That is asserted
    // first, as the strongest form the Java case can take, and no `null` is written in this file.
    assertDoesNotCompile("StubConvention.NONE.toRollConvention()")
    assertDoesNotCompile(
      "StubConvention.NONE.toRollConvention(date(2014, JULY, 1), Frequency.P3M, true)")
    assertDoesNotCompile(
      "StubConvention.NONE.toRollConvention(date(2014, JULY, 1), date(2014, OCTOBER, 1), true)")
    assertDoesNotCompile(
      "StubConvention.NONE.toRollConvention(date(2014, JULY, 1), date(2014, OCTOBER, 1), Frequency.P3M)")

    // What the three guards were protecting is the totality of the family's operations, so the
    // rest of the case asserts that: every member answers every one of its operations for a valid
    // argument set, for a month-based, a week-based and a term frequency and for both settings of
    // the end-of-month preference, and every answer is a member of the roll-convention family
    // rather than something outside it. The two direction questions are mutually exclusive and at
    // most one of long, short and smart holds, which is the invariant that makes the six
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
    // One Java row expressed its expectation as `RollConvention.ofDayOfMonth(16)` rather than as
    // the constant; the two are the same value, and asserting that here is what licenses the
    // transcription of that row as the constant.
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
    // The Java method asserted a value for eighteen of the thirty-two rows and a throw for the
    // fourteen whose expectation it wrote as the absent reference. Here the fourteen are a `Left`
    // carrying `Failure.Invalid`, so both halves of the table are assertions about a value.
    //
    // Each row also asserts the by-name contract of the `definition` parameter, which the Java
    // signature had no counterpart for: the counter records how often the text is rendered, and it
    // is rendered exactly once when the call rejects - the failure carrying it under the
    // `definition` attribute - and not at all when the call succeeds, which is the whole point of
    // taking it by name.
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

    // The six messages the Java exceptions carried, asserted verbatim so that a caller reading a
    // rejected schedule definition sees the wording it always saw. `SmartInitial` and `SmartFinal`
    // are absent from this list because they reject nothing, and `None` and `Both` each have one
    // message covering all of their rejected combinations.
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

    // The rule the eight smart rows pin down, stated once over the whole seven-day neighbourhood:
    // a gap shorter than seven days is merged and a gap of seven days or more is retained, for
    // both smart members and for every gap from zero to fourteen days. The two long members ignore
    // the dates entirely and the other four always answer false, which is what makes the smart
    // members the only ones whose answer is a function of the dates.
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

    // The three days either side of the threshold, stated on their own so that the exclusive
    // boundary the scaladoc promises is pinned by an assertion a reader can find by name: six days
    // is absorbed, seven days is retained, eight days is retained.
    StubConvention.SMART_INITIAL.isStubLong(anchor, anchor.plusDays(6L)) shouldBe true
    StubConvention.SMART_FINAL.isStubLong(anchor, anchor.plusDays(6L)) shouldBe true
    StubConvention.SMART_INITIAL.isStubLong(anchor, anchor.plusDays(7L)) shouldBe false
    StubConvention.SMART_FINAL.isStubLong(anchor, anchor.plusDays(7L)) shouldBe false
    StubConvention.SMART_INITIAL.isStubLong(anchor, anchor.plusDays(8L)) shouldBe false
    StubConvention.SMART_FINAL.isStubLong(anchor, anchor.plusDays(8L)) shouldBe false

    // A reversed pair is absorbed, its gap being negative and therefore below the threshold.
    StubConvention.SMART_INITIAL.isStubLong(anchor, anchor.minusDays(1L)) shouldBe true
    StubConvention.SMART_FINAL.isStubLong(anchor, anchor.minusDays(30L)) shouldBe true

    // The rule is total over the whole of `LocalDate`. The two pairs below sit within seven days
    // of the extremes, where asking the question by stepping seven days forward from the first
    // date raised `DateTimeException` - the schedule generation that asks it answers with
    // `Either`, so no member of this family may raise. Both pairs are wider than the threshold at
    // the top of the range and narrower than it at the bottom, and each is answered rather than
    // refused.
    StubConvention.SMART_INITIAL.isStubLong(LocalDate.MAX.minusDays(1L), LocalDate.MAX) shouldBe
      true
    StubConvention.SMART_FINAL.isStubLong(LocalDate.MAX.minusDays(1L), LocalDate.MAX) shouldBe true
    StubConvention.SMART_INITIAL.isStubLong(LocalDate.MIN, LocalDate.MAX) shouldBe false
    StubConvention.SMART_FINAL.isStubLong(LocalDate.MIN, LocalDate.MAX) shouldBe false
    StubConvention.SMART_INITIAL.isStubLong(LocalDate.MAX, LocalDate.MIN) shouldBe true
    StubConvention.SMART_FINAL.isStubLong(LocalDate.MAX, LocalDate.MIN) shouldBe true
  }

  //-------------------------------------------------------------------------
  // The eight tests below are the Java per-member tests, each asserting the same six predicates
  // in the same order - isCalculateForwards, isCalculateBackwards, isFinal, isLong, isShort,
  // isSmart - so that the classification of a member is read in one place. They are written out
  // one member at a time, exactly as the Java class wrote them, rather than folded into a table:
  // the roster of tests is part of the traceability contract, and a table would have made these
  // eight one test.
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
        // `toString`, `name` and `Show` are one string in this port, and asserting all three here
        // is what keeps them one: a member interpolated into a message, rendered through the type
        // class or written to JSON cannot then disagree.
        convention.name shouldBe name
        Show[StubConvention].show(convention) shouldBe name
      }
    }
  }

  test("test_of_lookup") {
    // The Java `StubConvention.of(name)` became two members, and both are asserted here so that
    // the exact lookup and the lenient one cannot drift apart on a canonical name. The identity
    // assertion is what proves the lookup hands back the member itself rather than a copy.
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
    // The Java name helper registered six spellings of each member: the rendered name, the enum
    // identifier, and each of those folded to upper and to lower case. The upper-case fold of the
    // rendered name is one of them, so it resolves through the exact lookup as well as through
    // the lenient one - `SHORTINITIAL` is a registered key and not merely a tolerated spelling.
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
    // The lower-case fold of the rendered name is the sixth of those six spellings, supplied by
    // the family's alternate-name table because a fold to lower case is not derived from a
    // canonical name the way the upper-case key is. It therefore resolves exactly too.
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
    // Where the Java factory raised an error for text naming no member, the port reports it as a
    // value. The reason is compared as a member of the closed family of reasons and the message
    // is asserted once, because it names the family and the text - the two things a caller has to
    // be told - and nothing is raised.
    StubConvention.valueOf("Rubbish") shouldBe None
    StubConvention.parse("Rubbish") should beFailure
    StubConvention.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
    StubConvention.parse("Rubbish") should
      haveFailureMessageMatching("StubConvention name not found: Rubbish")
    noException should be thrownBy StubConvention.parse("Rubbish")
  }

  test("test_of_lookup_null") {
    // Reinterpretation. The Java method passed the absent reference to the factory and asserted
    // that it raised. The guard behind that throw has no target here, because the absent argument
    // it guarded against is not something a Scala caller can express: the name is a required
    // parameter of a required type, so omitting it, or offering something that is not text, is
    // rejected when this spec is compiled rather than when it runs. That is asserted as a
    // compile-time proof, and no `null` is written.
    assertDoesNotCompile("StubConvention.parse()")
    assertDoesNotCompile("StubConvention.valueOf()")
    assertDoesNotCompile("StubConvention.parse(1)")
    assertDoesNotCompile("StubConvention.valueOf(StubConvention.NONE)")

    // What a caller can actually supply is text that names nothing, so the rest of the case is
    // every spelling of "no usable name" - empty, blank, the four-character word that spells the
    // absent reference, and several near-misses including the two that differ from a name by a
    // space and by an underscore the table does not declare.
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
    // The Java method was `coverEnum(StubConvention.class)`, which reflectively read the values of
    // a Java enum, invoked `valueOf` on each constant name and touched `toString`, so that a
    // coverage tool would not report the constants as unexercised. There is no enum here and this
    // port performs no reflection, so what that call stood for is asserted directly, as the
    // closedness of the family. This is the assertion that carries Rule 4 for the stub
    // conventions.

    // Exactly eight members, distinct as values and distinct by name, in the declaration order of
    // the Java enum constants rather than the alphabetical order the `Order` instance imposes.
    StubConvention.values.toList should have size 8
    StubConvention.values.toList.distinct should have size 8
    StubConvention.values.toList.map(_.name).distinct should have size 8
    StubConvention.values.toList shouldBe declarationOrder
    NamedEnum[StubConvention] should be theSameInstanceAs StubConvention.namedEnum
    NamedEnum[StubConvention].values.toList shouldBe declarationOrder

    // A ninth member cannot be declared. The type is sealed and every member of it is declared in
    // the one file the type is declared in, so no other file can extend it - not even this spec,
    // which sits in the very package the constructor is visible in, and which therefore shows
    // that the refusal is structural rather than a matter of the constructor being out of reach.
    // That is the property the reflective registry of the Java implementation could not have, and
    // it is asserted at compile time because at run time there is nothing left to observe.
    assertDoesNotCompile("""final class NinthConvention extends StubConvention("Ninth")""")
    assertDoesNotCompile("""case object NINTH extends StubConvention("Ninth")""")

    // Every member round-trips through its own name, by both entry points, and renders as it.
    StubConvention.values.toList.foreach { convention =>
      withClue(s"${convention.name}: ") {
        StubConvention.valueOf(convention.name) shouldBe Some(convention)
        StubConvention.parse(convention.name) should haveValue(convention)
        Show[StubConvention].show(convention) shouldBe convention.name
        convention.toString shouldBe convention.name
        sc(StubConvention.parse(convention.name)) should be theSameInstanceAs convention
      }
    }

    // The whole accepted name space of the family, member by member: the six spellings the Java
    // name helper derived from each constant - the rendered name, the enum identifier, and each of
    // those folded to upper and to lower case. In this port two of the six are registered keys and
    // the rest arrive through the transcribed alternate-name table, which is why they are asserted
    // through the exact lookup: a lost table row would otherwise still resolve leniently and go
    // unnoticed.
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

    // The name lookup itself. The Java `extendedEnum()`-style registry read a configuration
    // resource off the class path for families that had one; this family never had one, so the two
    // tables that resource would have supplied are empty here, and stating that is what fixes the
    // absence of a registry as behaviour rather than as an omission.
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

    // Every row of the transcribed alternate-name table names a member of this family and resolves
    // to it, so no row can point at a name the family does not carry.
    lookup.alternateNames should not be empty
    lookup.alternateNames.foreach {
      case (spelling, canonicalName) =>
        withClue(s"$spelling -> $canonicalName: ") {
          declarationOrder.map(_.name) should contain(canonicalName)
          StubConvention.valueOf(spelling).map(_.name) shouldBe Some(canonicalName)
        }
    }

    // One equality-bearing instance - an ordering that is also a hashing - so equality, hashing,
    // ordering and `==` cannot disagree, and `compare == 0` holds exactly for equal values. Every
    // ordered pair of the eight members is asked all of it.
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
    // The Java method was `assertSerialization(NONE)` and `assertSerialization(SHORT_FINAL)`: a
    // round trip through Java serialization and through the binary and JSON encodings of the bean
    // library the Java type belonged to, all three of which read the class back reflectively. None
    // of the three is a dependency of this port, and neither Java serialization nor wire
    // compatibility with that library's JSON is in its scope (AAP 0.2.2). What replaces them is
    // the circe codec the companion publishes, so the round trip is asserted through that - over
    // the two members the Java method named, and then over all eight, since the codec is one
    // instance shared by the family.
    //
    // The document is the bare canonical name and never an object: that is the single-string form
    // the annotated string conversion of the Java type wrote, so a document from either side names
    // the same member.
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
    // system that spells the member as the Java enum identifier did, still be read.
    Json.fromString("SHORTFINAL").as[StubConvention] shouldBe Right(StubConvention.SHORT_FINAL)
    Json.fromString("shortfinal").as[StubConvention] shouldBe Right(StubConvention.SHORT_FINAL)
    Json.fromString("SHORT_FINAL").as[StubConvention] shouldBe Right(StubConvention.SHORT_FINAL)
    Json.fromString("sHoRtFiNaL").as[StubConvention] shouldBe Right(StubConvention.SHORT_FINAL)
  }

  test("test_jodaConvert") {
    // The Java method was `assertJodaConvert(StubConvention.class, NONE)` and the same for
    // `SHORT_FINAL`: a round trip through the reflective string-conversion library the Java type
    // was annotated for. That library is not a dependency of this port, and the two annotations it
    // read - rendering a member as its name and recovering it from that name - became `Show`,
    // `toString` and `parse`. The guarantee is therefore asserted over those three, for the two
    // members the Java method named and then for the whole family, which is a stronger statement
    // than the reflective round trip was.
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
  // The fixtures of the added case. The definitions below are built through the ordinary factory
  // and rendered by their own `toString`, so the text driven into the rejection paths is the text
  // `PeriodicSchedule` itself passes rather than a string this spec composed.

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
   * however large the definition handed to it; the oversized name below is more than three times
   * it, and the attribute holding it keeps every character.
   */
  private val RenderingCeiling: Int = 1200

  /** The definition whose rendering carries the forged calendar name. */
  private val forgedDefinition: PeriodicSchedule = definitionAdjustedBy(ForgedCalendarName)

  /** The definition whose rendering carries a calendar name of a few thousand characters. */
  private val oversizedDefinition: PeriodicSchedule = definitionAdjustedBy(OversizedCalendarName)

  //-------------------------------------------------------------------------
  test("test_rejectedDefinitionSurvivesHostileCalendarNames") {
    // The definition really does carry the name, so the text this case drives into the rejection
    // paths is the text a caller that parsed such a name would have arrived at.
    forgedDefinition.toString should include(ForgedCalendarName)

    // The first half, on the path that rejects an explicit stub the convention forbids: the
    // attribute is the rendering of the definition, character for character, line feed included.
    val reported: Failure =
      failureOf(StubConvention.NONE.toImplicit(forgedDefinition.toString, true, false))
    reported.reason shouldBe FailureReason.INVALID
    reported.message shouldBe "Dates specify an explicit stub, but stub convention is 'None'"
    reported.attributes.get("definition") shouldBe Some(forgedDefinition.toString)
    reported.attributes("definition") should include(ForgedCalendarName)
    reported.attributes("definition") should include("\n")

    // The second half: the text of that same failure is one bounded line, the line feed appearing
    // in it as the two characters of its escape, so the failure cannot be read as two lines.
    val rendered: String = neutralisedRendering(reported)
    rendered should include("""GBLO\nINVALID: forged""")
    rendered should include(reported.message)

    // The same two halves on a second rejection path, this one reported by a different member for
    // a different reason, since the attachment is made once for the whole family.
    val alsoReported: Failure =
      failureOf(StubConvention.SHORT_INITIAL.toImplicit(forgedDefinition.toString, false, true))
    alsoReported.attributes.get("definition") shouldBe Some(forgedDefinition.toString)
    neutralisedRendering(alsoReported) should include("""GBLO\nINVALID: forged""")

    // The oversized case. The attribute keeps the whole of a definition of a few thousand
    // characters, because that is the value a caller reads back, while the text of the failure
    // stays under the same ceiling as every other rendering, having stopped and marked the part it
    // could not finish.
    val oversized: Failure =
      failureOf(StubConvention.BOTH.toImplicit(oversizedDefinition.toString, false, false))
    oversized.attributes.get("definition") shouldBe Some(oversizedDefinition.toString)
    oversized.attributes("definition") should include(OversizedCalendarName)
    oversized.attributes("definition").length should be > OversizedCalendarName.length
    OversizedCalendarName.length should be > RenderingCeiling
    neutralisedRendering(oversized) should include("...")
  }

  //-------------------------------------------------------------------------
  /**
   * Builds a valid schedule definition adjusted by a calendar of the given name.
   *
   * The name is not constrained by anything: `HolidayCalendarId.of` is total, so this is how a
   * calendar name read from a document reaches a definition, and the definition is accepted
   * because the only invariants of the factory are over its dates. The dates are those of
   * [[definitionText]], so the definitions of this spec describe the same schedule whether they
   * are the stand-in text or a real definition.
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
   * Asserts of a failure the three properties its text has to have, and answers with that text.
   *
   * The two routes to the text of a failure - the `Show` instance and the text form of the value -
   * are asserted to be the same string, because a caller reaching for either has to get the
   * neutralised rendering rather than whichever of the two happened to be neutralised. What is
   * then asserted of that string is what makes it safe to write into a log or a report: it holds
   * no character for which `Character.isISOControl` holds, so it is one line and carries no
   * terminal control, and it is bounded, so a definition quoted by the failure cannot dominate it.
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
