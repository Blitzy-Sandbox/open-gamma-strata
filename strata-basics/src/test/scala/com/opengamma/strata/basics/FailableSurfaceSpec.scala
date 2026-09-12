/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import java.time.DayOfWeek
import java.time.LocalDate
import java.time.Period
import java.time.YearMonth

import cats.data.NonEmptyList

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2

import com.opengamma.strata.basics.currency.BigMoney
import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.CurrencyAmount
import com.opengamma.strata.basics.currency.CurrencyAmountArray
import com.opengamma.strata.basics.currency.CurrencyPair
import com.opengamma.strata.basics.currency.FxMatrix
import com.opengamma.strata.basics.currency.FxRate
import com.opengamma.strata.basics.currency.FxRateProvider
import com.opengamma.strata.basics.currency.Money
import com.opengamma.strata.basics.currency.MultiCurrencyAmount
import com.opengamma.strata.basics.currency.MultiCurrencyAmountArray
import com.opengamma.strata.basics.date.AdjustableDates
import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.basics.date.BusinessDayConventions
import com.opengamma.strata.basics.date.DateAdjuster
import com.opengamma.strata.basics.date.DayCount
import com.opengamma.strata.basics.date.DayCounts
import com.opengamma.strata.basics.date.DaysAdjustment
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.basics.date.HolidayCalendarIds
import com.opengamma.strata.basics.date.ImmutableHolidayCalendar
import com.opengamma.strata.basics.date.MarketTenor
import com.opengamma.strata.basics.date.PeriodAdditionConventions
import com.opengamma.strata.basics.date.PeriodAdjustment
import com.opengamma.strata.basics.date.SequenceDate
import com.opengamma.strata.basics.date.Tenor
import com.opengamma.strata.basics.date.TenorAdjustment
import com.opengamma.strata.basics.index.FloatingRateName
import com.opengamma.strata.basics.index.FloatingRateNames
import com.opengamma.strata.basics.index.FxIndex
import com.opengamma.strata.basics.index.FxIndexObservation
import com.opengamma.strata.basics.index.FxIndices
import com.opengamma.strata.basics.index.IborIndexObservation
import com.opengamma.strata.basics.index.IborIndices
import com.opengamma.strata.basics.index.OvernightIndexObservation
import com.opengamma.strata.basics.index.OvernightIndices
import com.opengamma.strata.basics.index.PriceIndices
import com.opengamma.strata.basics.location.Country
import com.opengamma.strata.basics.schedule.Frequency
import com.opengamma.strata.basics.schedule.PeriodicSchedule
import com.opengamma.strata.basics.schedule.RollConvention
import com.opengamma.strata.basics.schedule.RollConventions
import com.opengamma.strata.basics.schedule.Schedule
import com.opengamma.strata.basics.schedule.SchedulePeriod
import com.opengamma.strata.basics.schedule.StubConvention
import com.opengamma.strata.basics.value.Rounding
import com.opengamma.strata.basics.value.ValueAdjustment
import com.opengamma.strata.basics.value.ValueSchedule
import com.opengamma.strata.basics.value.ValueStep
import com.opengamma.strata.basics.value.ValueStepSequence
import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.FixedScaleDecimal
import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.array.DoubleMatrix
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Asserts the failable public '''method''' surface of both ported modules, one case per entry of
 * the inventory of AAP section 0.3.3.
 *
 * That inventory was produced by applying one classification rule to every `throw` and every
 * `ArgChecker` call of the public methods of the Java classes being ported:
 *
 *   - a method whose failure depends on the '''data''' of its arguments - mismatched currencies,
 *     an unknown identifier, unequal sizes, unparseable text, an inconsistent schedule definition
 *     - reports that failure as a value, returning `Either[Failure, A]` or its accumulating form
 *     `EitherNec[Failure, A]`;
 *   - a throw that guards a '''caller contract''' independent of the values supplied - an index
 *     within bounds, dates given in order, the schedule information a convention is defined in
 *     terms of - or that guards a '''numeric domain edge''' remains a fail-fast `ArgCheck` throw,
 *     documented as such on the method that raises it.
 *
 * This suite asserts each method on the side of that line the inventory puts it on, and it must
 * never move one across: a method listed as reporting a value is asserted to return a `Left`, and
 * a method listed as raising is asserted with `intercept`, with a comment at every `intercept`
 * naming why that particular refusal is a contract or a numeric edge rather than a value. Together
 * with `SmartConstructorSpec`, which owns the '''constructor''' half of the same inventory, this is
 * the Rule 5 gate of AAP section 0.10.1:
 * `sbt -batch "testOnly *SmartConstructorSpec *FailableSurfaceSpec *ApiSurfaceSpec *FailureSpec"`.
 *
 * ===The inventory, and how to count it===
 *
 * [[eitherEntries]] and [[contractEntries]] below hold the inventory verbatim, each entry paired
 * with the name of the test that covers it, and `inventory_coverage` asserts that every one of
 * those tests exists in this suite. A reviewer checking the gate therefore reads two lists rather
 * than counting tests, and an entry that loses its test fails the suite rather than passing
 * silently. The counts are 88 value-reporting entries and 12 contract entries.
 *
 * Where several entries share one failure shape - the four element-wise arithmetic members of
 * `CurrencyAmountArray`, say - they are covered by one table-driven test holding '''one row per
 * entry''', so the mapping from inventory to assertion stays one-to-one and remains countable.
 *
 * ===What this suite deliberately does not assert===
 *
 * Three things belong to neighbouring suites and are not repeated here, so that a failure has one
 * home:
 *
 *   - the two '''numeric-edge''' throws of AAP section 0.3.3 - `CurrencyAmount` arithmetic that
 *     produces `NaN` from infinite operands, and `Decimal` arithmetic overflowing eighteen digits
 *     - are owned by `SmartConstructorSpec`, which owns every construction-time refusal;
 *   - the accumulation behaviour of the validated factories themselves - which reasons a factory
 *     reports and in what combination - is also `SmartConstructorSpec`'s, and this suite touches a
 *     factory only where the inventory lists it, or where it has to build a fixture;
 *   - the exact message '''text''' of a failure belongs to the spec of the type that produces it.
 *     Here a failure is asserted by its [[FailureReason]], compared as a value of the closed family
 *     and never as a string, and by the attribute the AAP fixes where it fixes one - the
 *     `definition` of a rejected schedule. A message is matched only where it is the one thing that
 *     distinguishes two failures of the same reason from the same method.
 *
 * One further asymmetry is asserted rather than assumed: `Schedule.periodEndDate` answers `None`
 * for a date outside every period where the Java original threw. That is the one place this port is
 * '''more''' total than Java, and it is asserted as a `None` and never as a throw.
 *
 * ===Positive assertions===
 *
 * A suite in which everything fails would pass every assertion here, so the entries whose failure
 * is easiest to trigger by accident carry the success case as well: `CurrencyPair.other` for a
 * currency that is in the pair, `Frequency.exactDivide` for an integral ratio, `Country.of` for an
 * unknown but well-formed code - `Country` is an open validated value and `of("ZZ")` succeeds -
 * and every reference-data-resolving member against `ReferenceData.standard`.
 *
 * ===Reference data===
 *
 * Reference data is threaded explicitly at every call, as AAP section 0.6.5 requires: the success
 * side of a resolving member is asserted against [[standardRefData]] and its failure side against
 * [[emptyRefData]], which holds nothing at all. No ambient default is consulted anywhere.
 *
 * @see `SmartConstructorSpec` for the constructor half of the same inventory
 * @see `ApiSurfaceSpec` for the compile-time half of Rule 5 - the absence of `apply` and `copy`
 */
final class FailableSurfaceSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  //-------------------------------------------------------------------------
  // Dates. Named as the Java tests of the schedule package named them, so that a reader comparing
  // the two sees the same fixtures.

  /** The 15th of January 2014, a Wednesday, and the start of the fixture schedule. */
  private val JAN_15: LocalDate = LocalDate.of(2014, 1, 15)

  /** The 15th of April 2014, the second boundary of the fixture schedule. */
  private val APR_15: LocalDate = LocalDate.of(2014, 4, 15)

  /** The 15th of July 2014, the third boundary of the fixture schedule. */
  private val JUL_15: LocalDate = LocalDate.of(2014, 7, 15)

  /** The 15th of October 2014, the end of the fixture schedule. */
  private val OCT_15: LocalDate = LocalDate.of(2014, 10, 15)

  /** The 4th of June 2014, the start of the Java `test_none_badStub` scenario. */
  private val JUN_04: LocalDate = LocalDate.of(2014, 6, 4)

  /** The 17th of September 2014, the end of the Java `test_none_badStub` scenario. */
  private val SEP_17: LocalDate = LocalDate.of(2014, 9, 17)

  /** The attribute key a rejected schedule definition is carried under, fixed by AAP 0.3.3. */
  private val DefinitionAttribute: String = "definition"

  /** The part of an `ArgCheck` message that names the year precondition of a holiday calendar. */
  private val UnsupportedDateMessage: String = "outside the accepted range"

  //-------------------------------------------------------------------------
  // Reference data, threaded explicitly into every resolving call (AAP 0.6.5).

  /** Every built-in holiday calendar, which is the success side of every resolving member. */
  private lazy val standardRefData: ReferenceData = ReferenceData.standard

  /** Reference data holding nothing, which is the failure side of every resolving member. */
  private lazy val emptyRefData: ReferenceData = ReferenceData.empty

  //-------------------------------------------------------------------------
  // Currency fixtures.

  /** One hundred pounds. */
  private lazy val gbp100: CurrencyAmount = obtained(CurrencyAmount.of(Currency.GBP, 100d))

  /** One hundred dollars, which disagrees in currency with [[gbp100]]. */
  private lazy val usd100: CurrencyAmount = obtained(CurrencyAmount.of(Currency.USD, 100d))

  /** One hundred euro, which no fixture matrix holds a rate for. */
  private lazy val eur100: CurrencyAmount = obtained(CurrencyAmount.of(Currency.EUR, 100d))

  /** One hundred pounds as money, rounded to the minor units of its currency. */
  private lazy val gbpMoney: Money = obtained(Money.of(Currency.GBP, 100d))

  /** One hundred dollars as money, which disagrees in currency with [[gbpMoney]]. */
  private lazy val usdMoney: Money = obtained(Money.of(Currency.USD, 100d))

  /** One hundred pounds as unrounded money. */
  private lazy val gbpBigMoney: BigMoney = obtained(BigMoney.of(Currency.GBP, 100d))

  /** One hundred dollars as unrounded money. */
  private lazy val usdBigMoney: BigMoney = obtained(BigMoney.of(Currency.USD, 100d))

  /** The rate two, which no conversion into a currency's own currency may apply. */
  private lazy val two: Decimal = obtained(Decimal.of(2L))

  /** Three pound values. */
  private lazy val gbpArray: CurrencyAmountArray =
    CurrencyAmountArray.of(Currency.GBP, DoubleArray.of(1d, 2d, 3d))

  /** Three dollar values, which disagree in currency with [[gbpArray]]. */
  private lazy val usdArray: CurrencyAmountArray =
    CurrencyAmountArray.of(Currency.USD, DoubleArray.of(1d, 2d, 3d))

  /** Two pound values, which disagree in size with [[gbpArray]]. */
  private lazy val shortGbpArray: CurrencyAmountArray =
    CurrencyAmountArray.of(Currency.GBP, DoubleArray.of(1d, 2d))

  /** A three-long run of pound values. */
  private lazy val gbpRun: MultiCurrencyAmountArray =
    accepted(MultiCurrencyAmountArray.of(Map(Currency.GBP -> DoubleArray.of(1d, 2d, 3d))))

  /** A two-long run of pound values, which disagrees in size with [[gbpRun]]. */
  private lazy val shortRun: MultiCurrencyAmountArray =
    accepted(MultiCurrencyAmountArray.of(Map(Currency.GBP -> DoubleArray.of(1d, 2d))))

  /** One hundred pounds as a multi-currency amount. */
  private lazy val gbpMulti: MultiCurrencyAmount = obtained(MultiCurrencyAmount.of(gbp100))

  //-------------------------------------------------------------------------
  // FX fixtures. Two matrices with no currency in common, and three rates.

  /** A matrix holding the pound and the dollar. */
  private lazy val gbpUsdMatrix: FxMatrix = FxMatrix.of(Currency.GBP, Currency.USD, 1.6d)

  /** A matrix holding the euro and the franc, sharing no currency with [[gbpUsdMatrix]]. */
  private lazy val eurChfMatrix: FxMatrix = FxMatrix.of(Currency.EUR, Currency.CHF, 1.1d)

  /** The rate of the pound against the dollar. */
  private lazy val gbpUsdRate: FxRate = accepted(FxRate.of(Currency.GBP, Currency.USD, 1.6d))

  /** The rate of the euro against the dollar, which crosses with [[gbpUsdRate]]. */
  private lazy val eurUsdRate: FxRate = accepted(FxRate.of(Currency.EUR, Currency.USD, 1.2d))

  /** The rate of the euro against the Canadian dollar, sharing no currency with [[gbpUsdRate]]. */
  private lazy val eurCadRate: FxRate = accepted(FxRate.of(Currency.EUR, Currency.CAD, 1.5d))

  //-------------------------------------------------------------------------
  // Schedule fixtures.

  /** A quarterly definition rolling on the fifteenth, which generates three periods. */
  private lazy val quarterly: PeriodicSchedule =
    accepted(
      PeriodicSchedule.of(
        JAN_15,
        OCT_15,
        Frequency.P3M,
        BusinessDayAdjustment.NONE,
        StubConvention.NONE,
        RollConventions.DAY_15))

  /** The three-period schedule the definition generates, resolved against the built-in calendars. */
  private lazy val schedule: Schedule = obtained(quarterly.createSchedule(standardRefData))

  /**
   * A definition that is accepted and then generates nothing, the Java `test_none_badStub`.
   *
   * The fourth of June to the seventeenth of September by `P1M` needs a stub, and the 'None' stub
   * convention forbids one. Nothing about a stub is decided at construction - deciding it needs the
   * schedule rolled out - so this is the fixture the generating members are asserted against.
   */
  private lazy val badStubDefinition: PeriodicSchedule =
    accepted(
      PeriodicSchedule.of(
        JUN_04,
        SEP_17,
        Frequency.P1M,
        BusinessDayAdjustment.NONE,
        StubConvention.NONE,
        RollConventions.DAY_4))

  /**
   * A definition whose two dates adjust onto one another, the Java `test_emptyWhenAdjusted_term`.
   *
   * The 29th to the 31st of May 2015 as one 'Term' period, adjusted by modified following over a
   * Saturday/Sunday weekend: the end date is a Sunday and moves back onto the start date, so the
   * generated dates hold a duplicate and the definition is rejected at generation.
   */
  private lazy val collapsingDefinition: PeriodicSchedule =
    accepted(
      PeriodicSchedule.of(
        LocalDate.of(2015, 5, 29),
        LocalDate.of(2015, 5, 31),
        Frequency.TERM,
        BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.SAT_SUN)))

  //-------------------------------------------------------------------------
  // Numeric fixtures.

  /** Three values. */
  private lazy val threeValues: DoubleArray = DoubleArray.of(1d, 2d, 3d)

  /** Two values, which disagree in size with [[threeValues]]. */
  private lazy val twoValues: DoubleArray = DoubleArray.of(1d, 2d)

  /** A two-by-two matrix. */
  private lazy val squareMatrix: DoubleMatrix = DoubleMatrix.of(2, 2, 1d, 2d, 3d, 4d)

  /** A one-by-two matrix, which disagrees in shape with [[squareMatrix]]. */
  private lazy val flatMatrix: DoubleMatrix = DoubleMatrix.of(1, 2, 1d, 2d)

  /**
   * A calendar holding two holidays of 2014 and nothing else.
   *
   * Built here rather than taken from the built-in set so that this suite depends on no calendar
   * data of its own: what matters is only that the calendar holds holidays, and therefore an array
   * of months, which is what gives its year precondition something to guard.
   */
  private lazy val datedCalendar: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(
      HolidayCalendarId.of("TestFailableSurface"),
      List(LocalDate.of(2014, 1, 1), LocalDate.of(2014, 12, 25)),
      List(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY))

  //-------------------------------------------------------------------------
  /**
   * Unwraps the outcome of a member that reports a single failure, failing the test if it failed.
   *
   * Used only to build fixtures. The message of the failure is reported, so a fixture that stops
   * being buildable says why rather than only that it did.
   *
   * @param outcome  the outcome to unwrap
   * @tparam A  the type of the value
   * @return the value the outcome carries
   */
  private def obtained[A](outcome: FailureOr[A]): A =
    outcome.fold(failure => fail(s"Expected a fixture but the call reported: ${failure.message}"), value => value)

  /**
   * Unwraps the outcome of a member that accumulates its failures, failing the test if it failed.
   *
   * @param outcome  the outcome to unwrap
   * @tparam A  the type of the value
   * @return the value the outcome carries
   */
  private def accepted[A](outcome: ResultNec[A]): A =
    outcome.fold(
      failures =>
        fail(
          failures.toNonEmptyList.toList
            .map(failure => failure.message)
            .mkString("Expected a fixture but the call reported: ", "; ", "")),
      value => value)

  /**
   * Reads the single failure an outcome reports, failing the test if it succeeded.
   *
   * @param outcome  the outcome to read
   * @return the failure the outcome carries
   */
  private def failureOf(outcome: FailureOr[Any]): Failure =
    outcome.fold(failure => failure, value => fail(s"Expected a failure but the call answered: $value"))

  /**
   * Reads the failures an accumulating outcome reports, failing the test if it succeeded.
   *
   * @param outcome  the outcome to read
   * @return the failures the outcome carries, in the order it holds them
   */
  private def failuresOf(outcome: ResultNec[Any]): List[Failure] =
    outcome.fold(
      failures => failures.toNonEmptyList.toList,
      value => fail(s"Expected a failure but the call answered: $value"))

  //-------------------------------------------------------------------------
  // FX and currency. Thirty-four entries of the inventory, every one of them a failure that
  // depends on the data supplied - a rate the provider does not hold, two currencies that
  // disagree, text that names no value - and therefore reported rather than raised.
  //-------------------------------------------------------------------------

  test("FxRateProvider.fxRate reports a rate the provider cannot supply") {
    // the provider that refuses everything, including a currency against itself, which is what
    // makes an unintended conversion visible rather than silent
    val refused: FailureOr[Double] = FxRateProvider.noConversion().fxRate(Currency.GBP, Currency.USD)
    refused should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    val refusedIdentity: FailureOr[Double] = FxRateProvider.noConversion().fxRate(Currency.GBP, Currency.GBP)
    refusedIdentity should beFailureWith(FailureReason.CURRENCY_CONVERSION)

    // the minimal provider answers the identity and refuses everything else
    val minimal: FailureOr[Double] = FxRateProvider.minimal().fxRate(Currency.GBP, Currency.USD)
    minimal should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    val identity: FailureOr[Double] = FxRateProvider.minimal().fxRate(Currency.GBP, Currency.GBP)
    identity should haveValue(1d)
  }

  test("FxRateProvider.convert reports a rate the provider cannot supply") {
    val refused: FailureOr[Double] = FxRateProvider.minimal().convert(100d, Currency.GBP, Currency.USD)
    refused should beFailureWith(FailureReason.CURRENCY_CONVERSION)

    // a conversion into the currency the amount already has needs no rate, so it answers
    val unchanged: FailureOr[Double] = FxRateProvider.minimal().convert(100d, Currency.GBP, Currency.GBP)
    unchanged should haveValue(100d)

    // the decimal-valued form reports the same failure, its rate lookup being the same one
    val refusedDecimal: FailureOr[Decimal] =
      FxRateProvider.minimal().convert(two, Currency.GBP, Currency.USD)
    refusedDecimal should beFailureWith(FailureReason.CURRENCY_CONVERSION)
  }

  test("FxMatrix.of reports rates that can never be placed") {
    // the pound/dollar rate places, and the euro/franc rate shares no currency with anything the
    // fold has placed by the time it is reached, so it can never be placed at all
    val unplaceable: FailureOr[FxMatrix] = FxMatrix.of(List(gbpUsdRate, eurCadRate))
    unplaceable should beFailureWith(FailureReason.CURRENCY_CONVERSION)

    // a collection whose rates connect places in the order it holds them
    val placed: FailureOr[FxMatrix] = FxMatrix.of(List(gbpUsdRate, eurUsdRate))
    placed should beSuccess
  }

  test("FxMatrix.withRate reports a pair the matrix has no currency in common with") {
    val disjoint: FailureOr[FxMatrix] =
      gbpUsdMatrix.withRate(CurrencyPair.of(Currency.EUR, Currency.CHF), 1.1d)
    disjoint should beFailureWith(FailureReason.CURRENCY_CONVERSION)

    // one currency in common is enough: the other is added and its cross rates computed
    val placed: FailureOr[FxMatrix] =
      gbpUsdMatrix.withRate(CurrencyPair.of(Currency.GBP, Currency.CHF), 1.2d)
    placed should beSuccess
  }

  test("FxMatrix.merge reports two matrices with no currency in common") {
    val disjoint: FailureOr[FxMatrix] = gbpUsdMatrix.merge(eurChfMatrix)
    disjoint should beFailureWith(FailureReason.CURRENCY_CONVERSION)

    // merging through a shared currency answers, every added rate being derived through it
    val shared: FxMatrix = obtained(gbpUsdMatrix.withRate(CurrencyPair.of(Currency.USD, Currency.CHF), 0.9d))
    val merged: FailureOr[FxMatrix] = gbpUsdMatrix.merge(shared)
    merged should beSuccess
  }

  test("FxMatrix.fxRate reports a rate the matrix does not hold") {
    val absent: FailureOr[Double] = gbpUsdMatrix.fxRate(Currency.EUR, Currency.CHF)
    absent should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    val held: FailureOr[Double] = gbpUsdMatrix.fxRate(Currency.GBP, Currency.USD)
    held should haveValue(1.6d)
  }

  test("FxMatrix.convert reports a rate the matrix does not hold") {
    // an amount in a currency the matrix does not hold cannot be converted
    val absent: FailureOr[CurrencyAmount] = gbpUsdMatrix.convert(eur100, Currency.USD)
    absent should beFailureWith(FailureReason.CURRENCY_CONVERSION)

    // and a single missing rate fails the whole of a multi-currency conversion, a total assembled
    // from some of its terms having no meaning
    val multi: MultiCurrencyAmount = obtained(MultiCurrencyAmount.of(gbp100, eur100))
    val partial: FailureOr[CurrencyAmount] = gbpUsdMatrix.convert(multi, Currency.USD)
    partial should beFailureWith(FailureReason.CURRENCY_CONVERSION)

    val converted: FailureOr[CurrencyAmount] = gbpUsdMatrix.convert(gbp100, Currency.USD)
    converted should beSuccess
  }

  test("FxRate.crossRate reports rates that do not cross") {
    // two rates over the same pair have no third currency to cross through
    val samePair: FailureOr[FxRate] = gbpUsdRate.crossRate(gbpUsdRate)
    samePair should beFailureWith(FailureReason.CURRENCY_CONVERSION)

    // and two rates over four currencies have no currency in common
    val disjoint: FailureOr[FxRate] = gbpUsdRate.crossRate(eurCadRate)
    disjoint should beFailureWith(FailureReason.CURRENCY_CONVERSION)

    val crossed: FailureOr[FxRate] = gbpUsdRate.crossRate(eurUsdRate)
    crossed should beSuccess
  }

  test("FxRate.parse reports text that names no rate") {
    val noRate: FailureOr[FxRate] = FxRate.parse("GBP/USD")
    noRate should beFailureWith(FailureReason.PARSING)
    val parsed: FailureOr[FxRate] = FxRate.parse("GBP/USD 1.6")
    parsed should haveValue(gbpUsdRate)
  }

  test("FxIndex.of(CurrencyPair) reports a pair no published index quotes") {
    // AAP 0.8.1 conflict 7: the family is closed, so an unconfigured pair is reported rather than
    // having an index minted for it as the Java `createFxIndex` fallback did
    val unconfigured: FailureOr[FxIndex] = FxIndex.of(CurrencyPair.of(Currency.GBP, Currency.BRL))
    unconfigured should beFailureWith(FailureReason.PARSING)

    val configured: FailureOr[FxIndex] = FxIndex.of(CurrencyPair.of(Currency.EUR, Currency.USD))
    configured should beSuccess
  }

  test("FxIndex.of(String) reports text that names no published index") {
    val rubbish: FailureOr[FxIndex] = FxIndex.of("Rubbish")
    rubbish should beFailureWith(FailureReason.PARSING)
    val named: FailureOr[FxIndex] = FxIndex.of(FxIndices.EUR_USD_ECB.name)
    named should haveValue(FxIndices.EUR_USD_ECB)
  }

  test("CurrencyAmount.plus and minus report a currency mismatch") {
    // one row per inventory entry: the two members that take an amount and can therefore disagree
    // about its currency. The scalar forms are total and are asserted below them.
    val mismatches: TableFor2[String, FailureOr[CurrencyAmount]] = Table(
      ("member", "outcome"),
      ("CurrencyAmount.plus(CurrencyAmount)", gbp100.plus(usd100)),
      ("CurrencyAmount.minus(CurrencyAmount)", gbp100.minus(usd100)))
    forAll(mismatches) { (member: String, outcome: FailureOr[CurrencyAmount]) =>
      withClue(s"$member: ") {
        outcome should beFailureWith(FailureReason.INVALID)
      }
    }

    // matching currencies answer, and the scalar forms carry no error channel at all
    gbp100.plus(gbp100) should beSuccess
    gbp100.minus(gbp100) should beSuccess
    gbp100.plus(50d).amount shouldBe 150d
    gbp100.minus(50d).amount shouldBe 50d
  }

  test("CurrencyAmount.parse reports text that names no amount") {
    val rubbish: FailureOr[CurrencyAmount] = CurrencyAmount.parse("Rubbish")
    rubbish should beFailureWith(FailureReason.PARSING)
    val parsed: FailureOr[CurrencyAmount] = CurrencyAmount.parse("GBP 100")
    parsed should haveValue(gbp100)
  }

  test("CurrencyAmount.convertedTo reports a non-unit rate for the same currency") {
    // the rate is not applied where no conversion happens, so a rate that is not one is a caller
    // scaling an amount through a conversion that does not occur - reported, since whether the two
    // currencies agree is a property of the arguments
    val nonUnit: FailureOr[CurrencyAmount] = gbp100.convertedTo(Currency.GBP, 2d)
    nonUnit should beFailureWith(FailureReason.INVALID)

    val unchanged: FailureOr[CurrencyAmount] = gbp100.convertedTo(Currency.GBP, 1d)
    unchanged should haveValue(gbp100)
    val converted: FailureOr[CurrencyAmount] = gbp100.convertedTo(Currency.USD, 1.6d)
    converted should beSuccess
  }

  test("Money.convertedTo and BigMoney.convertedTo report a non-unit rate for the same currency") {
    val nonUnit: TableFor2[String, FailureOr[Any]] = Table(
      ("member", "outcome"),
      ("Money.convertedTo(currency, rate)", gbpMoney.convertedTo(Currency.GBP, two)),
      ("BigMoney.convertedTo(currency, rate)", gbpBigMoney.convertedTo(Currency.GBP, two)))
    forAll(nonUnit) { (member: String, outcome: FailureOr[Any]) =>
      withClue(s"$member: ") {
        outcome should beFailureWith(FailureReason.INVALID)
      }
    }

    // a rate of one into the currency already held answers with the value unchanged
    val one: Decimal = obtained(Decimal.of(1L))
    gbpMoney.convertedTo(Currency.GBP, one) should haveValue(gbpMoney)
    gbpBigMoney.convertedTo(Currency.GBP, one) should haveValue(gbpBigMoney)
  }

  test("Money.plus, Money.minus, BigMoney.plus and BigMoney.minus report a currency mismatch") {
    val mismatches: TableFor2[String, FailureOr[Any]] = Table(
      ("member", "outcome"),
      ("Money.plus(Money)", gbpMoney.plus(usdMoney)),
      ("Money.minus(Money)", gbpMoney.minus(usdMoney)),
      ("BigMoney.plus(BigMoney)", gbpBigMoney.plus(usdBigMoney)),
      ("BigMoney.minus(BigMoney)", gbpBigMoney.minus(usdBigMoney)))
    forAll(mismatches) { (member: String, outcome: FailureOr[Any]) =>
      withClue(s"$member: ") {
        outcome should beFailureWith(FailureReason.INVALID)
      }
    }

    gbpMoney.plus(gbpMoney) should beSuccess
    gbpBigMoney.minus(gbpBigMoney) should beSuccess
  }

  test("CurrencyAmountArray arithmetic reports size and currency mismatches") {
    // Four inventory entries, six rows: the two element-wise members can disagree about either the
    // size or the currency, and the two scalar members only about the currency. The production code
    // checks the size before the currency, which is the order of the type being ported.
    val mismatches: TableFor2[String, FailureOr[CurrencyAmountArray]] = Table(
      ("member", "outcome"),
      ("CurrencyAmountArray.plus(CurrencyAmountArray), sizes", gbpArray.plus(shortGbpArray)),
      ("CurrencyAmountArray.minus(CurrencyAmountArray), sizes", gbpArray.minus(shortGbpArray)),
      ("CurrencyAmountArray.plus(CurrencyAmountArray), currencies", gbpArray.plus(usdArray)),
      ("CurrencyAmountArray.minus(CurrencyAmountArray), currencies", gbpArray.minus(usdArray)),
      ("CurrencyAmountArray.plus(CurrencyAmount), currencies", gbpArray.plus(usd100)),
      ("CurrencyAmountArray.minus(CurrencyAmount), currencies", gbpArray.minus(usd100)))
    forAll(mismatches) { (member: String, outcome: FailureOr[CurrencyAmountArray]) =>
      withClue(s"$member: ") {
        outcome should beFailureWith(FailureReason.INVALID)
      }
    }

    gbpArray.plus(gbpArray) should beSuccess
    gbpArray.minus(gbpArray) should beSuccess
    gbpArray.plus(gbp100) should beSuccess
    gbpArray.minus(gbp100) should beSuccess
  }

  test("MultiCurrencyAmountArray.getValues reports a currency the run does not hold") {
    val absent: FailureOr[DoubleArray] = gbpRun.getValues(Currency.AUD)
    absent should beFailureWith(FailureReason.INVALID)
    val held: FailureOr[DoubleArray] = gbpRun.getValues(Currency.GBP)
    held should haveValue(DoubleArray.of(1d, 2d, 3d))
  }

  test("MultiCurrencyAmountArray arithmetic reports a size mismatch") {
    // Four inventory entries. The two run-valued members disagree about size and report it; the two
    // amount-valued members keep the same channel but cannot fill it, an amount describing every
    // index of the run by construction, so they are asserted on the side they can actually answer.
    val mismatches: TableFor2[String, FailureOr[MultiCurrencyAmountArray]] = Table(
      ("member", "outcome"),
      ("MultiCurrencyAmountArray.plus(MultiCurrencyAmountArray)", gbpRun.plus(shortRun)),
      ("MultiCurrencyAmountArray.minus(MultiCurrencyAmountArray)", gbpRun.minus(shortRun)))
    forAll(mismatches) { (member: String, outcome: FailureOr[MultiCurrencyAmountArray]) =>
      withClue(s"$member: ") {
        outcome should beFailureWith(FailureReason.INVALID)
      }
    }

    val amountForms: TableFor2[String, FailureOr[MultiCurrencyAmountArray]] = Table(
      ("member", "outcome"),
      ("MultiCurrencyAmountArray.plus(MultiCurrencyAmount)", gbpRun.plus(gbpMulti)),
      ("MultiCurrencyAmountArray.minus(MultiCurrencyAmount)", gbpRun.minus(gbpMulti)))
    forAll(amountForms) { (member: String, outcome: FailureOr[MultiCurrencyAmountArray]) =>
      withClue(s"$member: ") {
        outcome should beSuccess
      }
    }

    gbpRun.plus(gbpRun) should beSuccess
    gbpRun.minus(gbpRun) should beSuccess
  }

  test("MultiCurrencyAmount.getAmount reports a currency the amount does not hold") {
    val absent: FailureOr[CurrencyAmount] = gbpMulti.getAmount(Currency.AUD)
    absent should beFailureWith(FailureReason.INVALID)
    val held: FailureOr[CurrencyAmount] = gbpMulti.getAmount(Currency.GBP)
    held should haveValue(gbp100)

    // the total-valued reader is the way to ask without an error channel, and answers zero
    gbpMulti.getAmountOrZero(Currency.AUD).amount shouldBe 0d
  }

  test("CurrencyPair.other reports a currency that is not in the pair") {
    val pair: CurrencyPair = CurrencyPair.of(Currency.GBP, Currency.USD)
    val absent: FailureOr[Currency] = pair.other(Currency.EUR)
    absent should beFailureWith(FailureReason.INVALID)

    // both currencies of the pair answer, which is the case this member exists for
    pair.other(Currency.GBP) should haveValue(Currency.USD)
    pair.other(Currency.USD) should haveValue(Currency.GBP)
  }

  test("CurrencyPair.parse reports text that names no pair") {
    val rubbish: FailureOr[CurrencyPair] = CurrencyPair.parse("Rubbish")
    rubbish should beFailureWith(FailureReason.PARSING)
    val parsed: FailureOr[CurrencyPair] = CurrencyPair.parse("EUR/USD")
    parsed should haveValue(CurrencyPair.of(Currency.EUR, Currency.USD))
  }

  test("Currency.parse reports a code outside the closed family") {
    // AAP 0.8.1 conflict 4: the family is the 74 configured codes, so an unknown code is reported
    // where Java minted a currency for it
    val unknown: FailureOr[Currency] = Currency.parse("XYZ")
    unknown should beFailureWith(FailureReason.PARSING)

    // case is folded, so the canonical code and its lower case both name the same currency
    Currency.parse("GBP") should haveValue(Currency.GBP)
    Currency.parse("gbp") should haveValue(Currency.GBP)
    val tooLong: FailureOr[Currency] = Currency.of("GBPX")
    tooLong should beFailure
  }


  //-------------------------------------------------------------------------
  // Location. Two entries. `Country` is an open validated value rather than a closed family, so a
  // well-formed code names a country whether or not the reference data knows it; only the three
  // letter lookup, which has to be translated through that data, can fail on a well-formed input.
  //-------------------------------------------------------------------------

  test("Country.of reports a malformed code and accepts any well-formed one") {
    val malformed: ResultNec[Country] = Country.of("abc")
    malformed should beFailureWith(FailureReason.INVALID)
    val tooShort: ResultNec[Country] = Country.of("G")
    tooShort should beFailureWith(FailureReason.INVALID)

    // the open code space: a code the reference data names no country for is still a country
    Country.of("ZZ") should beSuccess
    Country.of("GB") should haveValue(Country.GB)
  }

  test("Country.of3Char reports a code that names no country") {
    // well formed but absent from the reference data, which is the data-dependent half
    val unknown: FailureOr[Country] = Country.of3Char("ZZZ")
    unknown should beFailureWith(FailureReason.PARSING)

    // not the shape of a three letter code at all, which is the input-dependent half
    val malformed: FailureOr[Country] = Country.of3Char("zzz")
    malformed should beFailureWith(FailureReason.INVALID)

    Country.of3Char("GBR") should haveValue(Country.GB)
  }

  //-------------------------------------------------------------------------
  // Schedule and frequency. Fifteen entries. A schedule definition is judged against the calendar
  // it is rolled out over, so almost everything here is decided by data rather than by the call.
  //-------------------------------------------------------------------------

  test("Frequency.eventsPerYear reports a frequency with no exact count") {
    val irregular: Frequency = accepted(Frequency.of(Period.ofMonths(5)))
    val noCount: FailureOr[Int] = irregular.eventsPerYear
    noCount should beFailureWith(FailureReason.INVALID)

    Frequency.P3M.eventsPerYear should haveValue(4)
    Frequency.P1D.eventsPerYear should haveValue(364)
  }

  test("Frequency.exactDivide reports a non-integral ratio and divides an integral one") {
    val inexact: FailureOr[Int] = Frequency.P3M.exactDivide(Frequency.P2M)
    inexact should beFailureWith(FailureReason.INVALID)

    // a frequency of days does not divide a frequency of months at all, the two being incommensurate
    val incommensurate: FailureOr[Int] = Frequency.P3M.exactDivide(Frequency.P1D)
    incommensurate should beFailureWith(FailureReason.INVALID)

    Frequency.P12M.exactDivide(Frequency.P3M) should haveValue(4)
    Frequency.P13W.exactDivide(Frequency.P1W) should haveValue(13)
  }

  test("Frequency.of reports a period that is no frequency") {
    val zero: ResultNec[Frequency] = Frequency.of(Period.ZERO)
    zero should beFailureWith(FailureReason.INVALID)
    val negative: ResultNec[Frequency] = Frequency.of(Period.ofMonths(-1))
    negative should beFailureWith(FailureReason.INVALID)

    Frequency.of(Period.ofMonths(3)) should haveValue(Frequency.P3M)
  }

  test("Frequency.parse reports text that names no frequency") {
    val rubbish: FailureOr[Frequency] = Frequency.parse("Rubbish")
    rubbish should beFailureWith(FailureReason.PARSING)
    Frequency.parse("P3M") should haveValue(Frequency.P3M)
    Frequency.parse("Term") should haveValue(Frequency.TERM)
  }

  test("PeriodicSchedule.of reports dates that describe no schedule") {
    val reversed: ResultNec[PeriodicSchedule] =
      PeriodicSchedule.of(
        OCT_15,
        JAN_15,
        Frequency.P3M,
        BusinessDayAdjustment.NONE,
        StubConvention.NONE,
        RollConventions.DAY_15)
    reversed should beFailureWith(FailureReason.INVALID)

    // the fixture definition is the success side, built by the same factory
    quarterly.startDate shouldBe JAN_15
    quarterly.endDate shouldBe OCT_15
  }

  test("PeriodicSchedule.createSchedule reports a definition that generates nothing") {
    // AAP 0.3.3: `ScheduleException` becomes `Failure.Invalid` carrying the rejected definition
    // under the `definition` attribute, so a report can name it without parsing the message
    val rejected: FailureOr[Schedule] = collapsingDefinition.createSchedule(standardRefData)
    rejected should beFailureWith(FailureReason.INVALID)
    rejected should haveFailureMessageMatching(".*duplicate adjusted dates.*")
    failureOf(rejected).attributes.get(DefinitionAttribute) shouldBe
      Some(collapsingDefinition.toString)

    quarterly.createSchedule(standardRefData) should beSuccess
  }

  test("PeriodicSchedule.createUnadjustedDates reports a disallowed stub") {
    val rejected: FailureOr[List[LocalDate]] = badStubDefinition.createUnadjustedDates()
    rejected should beFailureWith(FailureReason.INVALID)
    rejected should haveFailureMessageMatching(".*disallowed stub.*")
    failureOf(rejected).attributes.get(DefinitionAttribute) shouldBe Some(badStubDefinition.toString)

    quarterly.createUnadjustedDates() should haveValue(List(JAN_15, APR_15, JUL_15, OCT_15))
  }

  test("PeriodicSchedule.createAdjustedDates reports dates that adjust onto one another") {
    val rejected: FailureOr[List[LocalDate]] = collapsingDefinition.createAdjustedDates(standardRefData)
    rejected should beFailureWith(FailureReason.INVALID)
    rejected should haveFailureMessageMatching(".*duplicate adjusted dates.*")
    failureOf(rejected).attributes.get(DefinitionAttribute) shouldBe
      Some(collapsingDefinition.toString)

    quarterly.createAdjustedDates(standardRefData) should haveValue(List(JAN_15, APR_15, JUL_15, OCT_15))
  }

  test("SchedulePeriod.of reports dates that describe no period") {
    val reversed: ResultNec[SchedulePeriod] = SchedulePeriod.of(OCT_15, JAN_15)
    reversed should beFailureWith(FailureReason.INVALID)

    // one pair of dates fills both roles, so a bad pair is reported under both pairs of names:
    // the accumulation the validator of the bean being ported could not express
    failuresOf(reversed) should have size 2

    SchedulePeriod.of(JAN_15, APR_15) should beSuccess
  }

  test("Schedule.of accepts the periods its own type already guarantees") {
    // The inventory lists this factory because the Java bean validated its period list. The only
    // reason it could report - an empty list - is carried by `NonEmptyList` in this port, so the
    // factory is total in effect while keeping the reported shape every factory here has. That is a
    // divergence recorded in SCALA_MIGRATION.md, and it is asserted rather than assumed.
    val single: ResultNec[Schedule] =
      Schedule.of(NonEmptyList.one(schedule.firstPeriod), Frequency.P3M, RollConventions.DAY_15)
    single should beSuccess
    single should haveValue(
      accepted(Schedule.of(NonEmptyList.one(schedule.firstPeriod), Frequency.P3M, RollConventions.DAY_15)))

    val all: ResultNec[Schedule] = Schedule.of(schedule.periods, Frequency.P3M, RollConventions.DAY_15)
    all should beSuccess
  }

  test("Schedule.merge reports a date that matches no period of the schedule") {
    // the first regular start date names no boundary of this schedule, which is data about the
    // schedule rather than a breach of the call: the two dates it is given are in order
    val unmatched: FailureOr[Schedule] = schedule.merge(3, LocalDate.of(2014, 2, 1), OCT_15)
    unmatched should beFailureWith(FailureReason.INVALID)

    // a group size that does not divide the regular periods is reported the same way
    val ungrouped: FailureOr[Schedule] = schedule.merge(2, JAN_15, OCT_15)
    ungrouped should beFailureWith(FailureReason.INVALID)

    schedule.merge(3, JAN_15, OCT_15) should beSuccess
    // `merge` additionally carries a documented dates-in-order `ArgCheck` precondition; it is a
    // caller contract and is asserted by `ScheduleSpec`, not here, because this entry of the
    // inventory is a reported failure and must not be asserted as a throw
  }

  test("Schedule.mergeRegular reports an unusable group size") {
    val zero: FailureOr[Schedule] = schedule.mergeRegular(0, true)
    zero should beFailureWith(FailureReason.INVALID)
    zero should haveFailureMessageMatching(".*must not be negative or zero.*")
    val negative: FailureOr[Schedule] = schedule.mergeRegular(-1, false)
    negative should beFailureWith(FailureReason.INVALID)

    schedule.mergeRegular(3, true) should beSuccess
  }

  test("Schedule.toAdjusted reports a period that collapses once adjusted") {
    // the adjuster moves the third boundary onto the second, which leaves the middle period with
    // one date at each end - a pair no period accepts. A middle period cannot be merged away, so
    // the collapse is reported rather than absorbed.
    val collapsing: DateAdjuster = DateAdjuster(date => if (date == JUL_15) APR_15 else date)
    val rejected: FailureOr[Schedule] = schedule.toAdjusted(collapsing)
    rejected should beFailureWith(FailureReason.INVALID)

    // an adjuster that moves nothing answers with the schedule itself
    schedule.toAdjusted(DateAdjuster(date => date)) should haveValue(schedule)
  }

  test("RollConvention.ofDayOfMonth reports a day outside one to thirty-one") {
    val zero: FailureOr[RollConvention] = RollConvention.ofDayOfMonth(0)
    zero should beFailureWith(FailureReason.INVALID)
    val beyond: FailureOr[RollConvention] = RollConvention.ofDayOfMonth(32)
    beyond should beFailureWith(FailureReason.INVALID)

    // the thirty-first normalises onto the end-of-month convention, as in the Java original
    RollConvention.ofDayOfMonth(31) should haveValue(RollConventions.EOM)
    RollConvention.ofDayOfMonth(15) should haveValue(RollConventions.DAY_15)
  }

  test("RollConvention.ofDayOfWeek is total for every day of the week") {
    // The inventory lists this member beside `ofDayOfMonth` because the Java pair shared a lookup.
    // Here the argument is an enumerated day rather than a number, so there is no out-of-range
    // input to report and the member is total: seven days, seven conventions, no error channel.
    RollConvention.ofDayOfWeek(DayOfWeek.MONDAY) shouldBe RollConventions.DAY_MON
    RollConvention.ofDayOfWeek(DayOfWeek.TUESDAY) shouldBe RollConventions.DAY_TUE
    RollConvention.ofDayOfWeek(DayOfWeek.WEDNESDAY) shouldBe RollConventions.DAY_WED
    RollConvention.ofDayOfWeek(DayOfWeek.THURSDAY) shouldBe RollConventions.DAY_THU
    RollConvention.ofDayOfWeek(DayOfWeek.FRIDAY) shouldBe RollConventions.DAY_FRI
    RollConvention.ofDayOfWeek(DayOfWeek.SATURDAY) shouldBe RollConventions.DAY_SAT
    RollConvention.ofDayOfWeek(DayOfWeek.SUNDAY) shouldBe RollConventions.DAY_SUN
  }

  //-------------------------------------------------------------------------
  // Dates and adjustments. Eleven entries. Every adjustment resolves its calendar against the
  // reference data it is handed, so its failure is the absence of that calendar - data, not a call.
  //-------------------------------------------------------------------------

  test("Tenor.of reports a period that is no tenor") {
    val zero: ResultNec[Tenor] = Tenor.of(Period.ZERO)
    zero should beFailureWith(FailureReason.INVALID)
    val negative: ResultNec[Tenor] = Tenor.of(Period.ofDays(-1))
    negative should beFailureWith(FailureReason.INVALID)

    Tenor.of(Period.ofMonths(3)) should haveValue(Tenor.TENOR_3M)
    Tenor.ofDays(7) should haveValue(Tenor.TENOR_1W)
  }

  test("Tenor.parse reports text that names no tenor") {
    // text that is not a period at all is a parse failure
    val rubbish: FailureOr[Tenor] = Tenor.parse("2K")
    rubbish should beFailureWith(FailureReason.PARSING)

    // text that names a period no tenor can carry is reported with the reason the period was
    // rejected for, so the caller is told the period is negative rather than that the text was bad
    val negative: FailureOr[Tenor] = Tenor.parse("-2D")
    negative should beFailureWith(FailureReason.INVALID)

    Tenor.parse("3M") should haveValue(Tenor.TENOR_3M)
    Tenor.parse("P3M") should haveValue(Tenor.TENOR_3M)
  }

  test("MarketTenor spot factories report a count that is no tenor") {
    val zeroDays: FailureOr[MarketTenor] = MarketTenor.ofSpotDays(0)
    zeroDays should beFailureWith(FailureReason.INVALID)
    val negativeMonths: FailureOr[MarketTenor] = MarketTenor.ofSpotMonths(-1)
    negativeMonths should beFailureWith(FailureReason.INVALID)

    // `ofSpot` itself cannot fail, its argument already being a tenor, and answers in the reported
    // shape so that it composes with the counted factories
    MarketTenor.ofSpot(Tenor.TENOR_1D) should haveValue(MarketTenor.SN)
    MarketTenor.ofSpotDays(1) should haveValue(MarketTenor.SN)
  }

  test("MarketTenor.parse reports text that names no market tenor") {
    // empty text is refused first, with the message of the check the type being ported performed
    val empty: FailureOr[MarketTenor] = MarketTenor.parse("")
    empty should beFailureWith(FailureReason.INVALID)
    val rubbish: FailureOr[MarketTenor] = MarketTenor.parse("2K")
    rubbish should beFailureWith(FailureReason.PARSING)

    MarketTenor.parse("ON") should haveValue(MarketTenor.ON)
    MarketTenor.parse("1W") should haveValue(MarketTenor.SW)
  }

  test("SequenceDate.of reports fields that describe no instruction") {
    // a month and a minimum period each name a different starting point
    val bothStartingPoints: ResultNec[SequenceDate] =
      SequenceDate.of(Some(YearMonth.of(2014, 6)), Some(Period.ofMonths(1)), 1, fullSequence = false)
    bothStartingPoints should beFailureWith(FailureReason.INVALID)

    // the three checks accumulate: a negative minimum period and a sequence number of zero are
    // both reported by one call
    val twoFaults: ResultNec[SequenceDate] =
      SequenceDate.of(None, Some(Period.ofMonths(-1)), 0, fullSequence = false)
    twoFaults should beFailureWith(FailureReason.INVALID)
    failuresOf(twoFaults) should have size 2

    SequenceDate.base(1) should beSuccess
    SequenceDate.full(YearMonth.of(2014, 6)) should beSuccess
  }

  test("DaysAdjustment resolution reports a calendar the reference data does not hold") {
    // Construction is total in this port - every invariant of the type is carried by the types of
    // its three fields - so the reported member of the inventory entry is the resolution, where the
    // calendar identifier becomes a calendar or does not.
    val adjustment: DaysAdjustment =
      DaysAdjustment.ofBusinessDays(2, HolidayCalendarId.of("NoSuchCalendarFS"))
    val unresolved: FailureOr[LocalDate] = adjustment.adjust(JAN_15, emptyRefData)
    unresolved should beFailureWith(FailureReason.MISSING_DATA)
    val unresolvedAdjuster: FailureOr[DateAdjuster] = adjustment.resolve(emptyRefData)
    unresolvedAdjuster should beFailureWith(FailureReason.MISSING_DATA)

    val known: DaysAdjustment = DaysAdjustment.ofBusinessDays(2, HolidayCalendarIds.GBLO)
    known.adjust(JAN_15, standardRefData) should beSuccess
    known.toReader.run(standardRefData) should beSuccess
  }

  test("PeriodAdjustment.of reports a period the convention cannot add") {
    // a month-based convention cannot be applied to a period holding days, which is the one
    // invariant of the type and a property of the two arguments together
    val mismatched: ResultNec[PeriodAdjustment] =
      PeriodAdjustment.of(
        Period.of(1, 2, 3),
        PeriodAdditionConventions.LAST_DAY,
        BusinessDayAdjustment.NONE)
    mismatched should beFailureWith(FailureReason.INVALID)

    PeriodAdjustment.of(Period.of(1, 2, 3), PeriodAdditionConventions.NONE, BusinessDayAdjustment.NONE) should
      beSuccess
    PeriodAdjustment.ofLastDay(Period.ofMonths(3), BusinessDayAdjustment.NONE) should beSuccess
  }

  test("TenorAdjustment.of reports a tenor the convention cannot add") {
    val mismatched: ResultNec[TenorAdjustment] =
      TenorAdjustment.of(
        Tenor.TENOR_1W,
        PeriodAdditionConventions.LAST_DAY,
        BusinessDayAdjustment.NONE)
    mismatched should beFailureWith(FailureReason.INVALID)

    TenorAdjustment.of(Tenor.TENOR_1W, PeriodAdditionConventions.NONE, BusinessDayAdjustment.NONE) should
      beSuccess
    TenorAdjustment.ofLastDay(Tenor.TENOR_3M, BusinessDayAdjustment.NONE) should beSuccess
  }

  test("AdjustableDates.of reports dates that describe no set") {
    val empty: ResultNec[AdjustableDates] = AdjustableDates.of(List.empty[LocalDate])
    empty should beFailureWith(FailureReason.INVALID)

    // the dates have to increase strictly, so a reversed or a repeated pair is reported
    val reversed: ResultNec[AdjustableDates] =
      AdjustableDates.of(BusinessDayAdjustment.NONE, List(OCT_15, JAN_15))
    reversed should beFailureWith(FailureReason.INVALID)
    val repeated: ResultNec[AdjustableDates] =
      AdjustableDates.of(BusinessDayAdjustment.NONE, List(JAN_15, JAN_15))
    repeated should beFailureWith(FailureReason.INVALID)

    AdjustableDates.of(JAN_15, OCT_15) should beSuccess
  }

  test("HolidayCalendarId.resolve reports a calendar the reference data does not hold") {
    val unresolved: FailureOr[Any] = HolidayCalendarIds.GBLO.resolve(emptyRefData)
    unresolved should beFailureWith(FailureReason.MISSING_DATA)
    failureOf(unresolved).attributes.get("id") shouldBe Some(HolidayCalendarIds.GBLO.name)

    // a composite identifier resolves component by component, so one absent component is reported
    val composite: HolidayCalendarId = HolidayCalendarIds.GBLO.combinedWith(HolidayCalendarIds.USNY)
    val partial: FailureOr[Any] =
      composite.resolve(obtained(ReferenceData.of(ReferenceData.Entry(HolidayCalendarIds.GBLO, datedCalendar))))
    partial should beFailureWith(FailureReason.MISSING_DATA)

    HolidayCalendarIds.GBLO.resolve(standardRefData) should beSuccess
    composite.resolve(standardRefData) should beSuccess
  }

  test("DayCount.ofBus252 reports a calendar the reference data does not hold") {
    // AAP 0.6.5: the Java factory resolved the identifier against ambient standard reference data;
    // here the caller passes the data, so the absence of the calendar is reported to it
    val unresolved: FailureOr[DayCount] = DayCount.ofBus252(HolidayCalendarIds.BRBD, emptyRefData)
    unresolved should beFailureWith(FailureReason.MISSING_DATA)

    val resolved: FailureOr[DayCount] = DayCount.ofBus252(HolidayCalendarIds.BRBD, standardRefData)
    resolved should beSuccess
    resolved.map(dayCount => dayCount.name) should haveValue("Bus/252 BRBD")
  }


  //-------------------------------------------------------------------------
  // Reference data. Two entries. An identifier that the data does not hold is the absence of data
  // rather than a broken call, which is why the Java `ReferenceDataNotFoundException` is not ported.
  //-------------------------------------------------------------------------

  test("ReferenceDataId.resolve reports an identifier the reference data does not hold") {
    val absent: TestingReferenceDataId = TestingReferenceDataId("missing")
    val unresolved: FailureOr[java.lang.Number] = absent.resolve(emptyRefData)
    unresolved should beFailureWith(FailureReason.MISSING_DATA)
    failureOf(unresolved).attributes.get("id") shouldBe Some(absent.toString)

    // the same identifier resolves against data that holds it, and through the reader form too
    val held: ReferenceData = ImmutableReferenceData.of(absent, Int.box(1))
    absent.resolve(held) should haveValue(Int.box(1))
    absent.toReader.run(held) should haveValue(Int.box(1))
  }

  test("ReferenceData.getValue reports an identifier the reference data does not hold") {
    val absent: TestingReferenceDataId = TestingReferenceDataId("absent")
    val unresolved: FailureOr[java.lang.Number] = emptyRefData.getValue(absent)
    unresolved should beFailureWith(FailureReason.MISSING_DATA)
    unresolved should haveFailureMessageMatching(".*Reference data not found.*")

    // `findValue` is the same lookup without the error channel, which is what a caller that can
    // proceed without the value uses
    emptyRefData.findValue(absent) shouldBe None
    standardRefData.getValue(HolidayCalendarIds.GBLO) should beSuccess
  }

  //-------------------------------------------------------------------------
  // Identifiers. Three entries, every one of them a judgement about text a caller supplied.
  //-------------------------------------------------------------------------

  test("StandardId.of reports parts that name no identifier") {
    val emptyScheme: ResultNec[StandardId] = StandardId.of("", "value")
    emptyScheme should beFailureWith(FailureReason.INVALID)

    // the two parts are checked together, so an identifier wrong in both is reported twice
    val bothEmpty: ResultNec[StandardId] = StandardId.of("", "")
    bothEmpty should beFailureWith(FailureReason.INVALID)
    failuresOf(bothEmpty) should have size 2

    StandardId.of(StandardSchemes.OG_TICKER_SCHEME, "AAPL") should beSuccess
  }

  test("StandardId.parse reports text that names no identifier") {
    val noSeparator: FailureOr[StandardId] = StandardId.parse("Rubbish")
    noSeparator should beFailureWith(FailureReason.PARSING)

    // a value may hold no tilde, so a second separator is rejected with the value that holds it
    val twoSeparators: FailureOr[StandardId] = StandardId.parse("a~b~c")
    twoSeparators should beFailure

    StandardId.parse("OG-Ticker~AAPL") should
      haveValue(accepted(StandardId.of(StandardSchemes.OG_TICKER_SCHEME, "AAPL")))
  }

  test("StandardSchemes.splitTicMic reports an identifier that is no TICMIC") {
    val notTicMic: StandardId = accepted(StandardId.of(StandardSchemes.TICMIC_SCHEME, "AAPL"))
    val rejected: FailureOr[(String, String)] = StandardSchemes.splitTicMic(notTicMic)
    rejected should beFailureWith(FailureReason.PARSING)

    val ticMic: StandardId = accepted(StandardSchemes.createTicMic("AAPL", "XNAS"))
    StandardSchemes.splitTicMic(ticMic) should haveValue(("AAPL", "XNAS"))
  }

  //-------------------------------------------------------------------------
  // Values. Nine entries. A value definition is judged against the schedule it is resolved
  // against, so the factories are the cheap half and resolution is where the data is judged.
  //-------------------------------------------------------------------------

  test("CurrencyAmountArray.of reports an empty collection and mixed currencies") {
    val empty: ResultNec[CurrencyAmountArray] = CurrencyAmountArray.of(List.empty[CurrencyAmount])
    empty should beFailureWith(FailureReason.INVALID)
    val mixed: ResultNec[CurrencyAmountArray] = CurrencyAmountArray.of(List(gbp100, usd100))
    mixed should beFailureWith(FailureReason.INVALID)

    // the size-and-function form reports the same two conditions of the values it produces
    val mixedFromFunction: ResultNec[CurrencyAmountArray] =
      CurrencyAmountArray.of(2, index => if (index == 0) gbp100 else usd100)
    mixedFromFunction should beFailureWith(FailureReason.INVALID)

    CurrencyAmountArray.of(List(gbp100, gbp100)) should beSuccess
  }

  test("MultiCurrencyAmountArray.of reports values of unequal length") {
    val ragged: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.of(
        Map(
          Currency.GBP -> DoubleArray.of(1d, 2d, 3d),
          Currency.USD -> DoubleArray.of(1d)))
    ragged should beFailureWith(FailureReason.INVALID)

    MultiCurrencyAmountArray.of(
      Map(
        Currency.GBP -> DoubleArray.of(1d, 2d, 3d),
        Currency.USD -> DoubleArray.of(4d, 5d, 6d))) should beSuccess
  }

  test("MultiCurrencyAmount.of reports a duplicated currency") {
    val duplicated: FailureOr[MultiCurrencyAmount] = MultiCurrencyAmount.of(gbp100, gbp100)
    duplicated should beFailureWith(FailureReason.INVALID)

    MultiCurrencyAmount.of(gbp100, usd100) should beSuccess

    // `total` is the merging counterpart, which is why the factory can refuse rather than guess
    MultiCurrencyAmount.total(List(gbp100, gbp100)).getAmount(Currency.GBP) should
      haveValue(obtained(CurrencyAmount.of(Currency.GBP, 200d)))
  }

  test("ValueStep.of reports a position that names no period") {
    // a change at the start of the first period is no change, so the index has to be one or greater
    val atZero: ResultNec[ValueStep] = ValueStep.of(0, ValueAdjustment.ofReplace(200d))
    atZero should beFailureWith(FailureReason.INVALID)

    // the field-wise factory reports a step positioned by neither field and one positioned by both
    val neither: ResultNec[ValueStep] = ValueStep.of(None, None, ValueAdjustment.ofReplace(200d))
    neither should beFailureWith(FailureReason.INVALID)
    val both: ResultNec[ValueStep] =
      ValueStep.of(Some(1), Some(APR_15), ValueAdjustment.ofReplace(200d))
    both should beFailureWith(FailureReason.INVALID)

    ValueStep.of(1, ValueAdjustment.ofReplace(200d)) should beSuccess
    ValueStep.of(APR_15, ValueAdjustment.ofReplace(200d)).value shouldBe ValueAdjustment.ofReplace(200d)
  }

  test("ValueSchedule.of is total, every judgement belonging to resolution") {
    // The inventory lists this factory because the Java bean validated its steps. In this port a
    // definition is only judged against the schedule it is resolved against - a step position means
    // nothing without one - so construction carries no error channel and `resolveValues` carries it
    // all. The divergence is recorded in SCALA_MIGRATION.md and asserted here.
    ValueSchedule.of(100d).initialValue shouldBe 100d
    val step: ValueStep = accepted(ValueStep.of(1, ValueAdjustment.ofReplace(200d)))
    ValueSchedule.of(100d, List(step)).steps shouldBe List(step)
    ValueSchedule.of(100d, step).steps shouldBe List(step)
  }

  test("ValueStepSequence.of reports arguments that describe no sequence") {
    val reversed: ResultNec[ValueStepSequence] =
      ValueStepSequence.of(OCT_15, JAN_15, Frequency.P3M, ValueAdjustment.ofDeltaAmount(-100d))
    reversed should beFailureWith(FailureReason.INVALID)

    // a sequence of changes may not replace the value, a replacement being absolute and therefore
    // the same at every step
    val replacing: ResultNec[ValueStepSequence] =
      ValueStepSequence.of(JAN_15, OCT_15, Frequency.P3M, ValueAdjustment.ofReplace(200d))
    replacing should beFailureWith(FailureReason.INVALID)

    // both faults at once are reported together
    val bothFaults: ResultNec[ValueStepSequence] =
      ValueStepSequence.of(OCT_15, JAN_15, Frequency.P3M, ValueAdjustment.ofReplace(200d))
    failuresOf(bothFaults) should have size 2

    ValueStepSequence.of(APR_15, JUL_15, Frequency.P3M, ValueAdjustment.ofDeltaAmount(-100d)) should
      beSuccess
  }

  test("ValueSchedule.resolveValues reports a step the schedule cannot carry") {
    // the schedule has three periods, so a step positioned at the sixth boundary names a period
    // that does not exist - a property of the pairing, which is why it is reported here and not
    // when the step was built
    val beyond: ValueSchedule =
      ValueSchedule.of(100d, accepted(ValueStep.of(5, ValueAdjustment.ofReplace(200d))))
    val rejected: FailureOr[DoubleArray] = beyond.resolveValues(schedule)
    rejected should beFailureWith(FailureReason.INVALID)

    // two steps resolving to one period with different adjustments contradict one another
    val contradictory: ValueSchedule =
      ValueSchedule.of(
        100d,
        List(
          accepted(ValueStep.of(1, ValueAdjustment.ofReplace(200d))),
          accepted(ValueStep.of(1, ValueAdjustment.ofReplace(300d)))))
    contradictory.resolveValues(schedule) should beFailureWith(FailureReason.INVALID)

    val resolvable: ValueSchedule =
      ValueSchedule.of(100d, accepted(ValueStep.of(1, ValueAdjustment.ofReplace(200d))))
    resolvable.resolveValues(schedule) should haveValue(DoubleArray.of(100d, 200d, 200d))
  }

  test("Rounding.ofDecimalPlaces reports a count outside zero to 255") {
    val negative: ResultNec[Rounding] = Rounding.ofDecimalPlaces(-1)
    negative should beFailureWith(FailureReason.INVALID)
    val beyond: ResultNec[Rounding] = Rounding.ofDecimalPlaces(256)
    beyond should beFailureWith(FailureReason.INVALID)

    Rounding.ofDecimalPlaces(4) should beSuccess
  }

  test("Rounding.ofFractionalDecimalPlaces accumulates both rejections") {
    val bothWrong: ResultNec[Rounding] = Rounding.ofFractionalDecimalPlaces(-1, 257)
    bothWrong should beFailureWith(FailureReason.INVALID)
    failuresOf(bothWrong) should have size 2

    // a fraction of one normalises to no fractional part, which happens only after both checks
    Rounding.ofFractionalDecimalPlaces(4, 32) should beSuccess
    Rounding.ofFractionalDecimalPlaces(4, 1) should haveValue(accepted(Rounding.ofDecimalPlaces(4)))
  }

  //-------------------------------------------------------------------------
  // Collect members reached from this module. Four entries. They belong to the ported subset of
  // `strata-collect` and are asserted here because `strata-basics` is where they are consumed.
  //-------------------------------------------------------------------------

  test("NamedEnum.parse reports a name no member of the family carries") {
    val unknown: ResultNec[Currency] = NamedEnum[Currency].parse("NotACurrency")
    unknown should beFailureWith(FailureReason.PARSING)

    // the alias-aware lookup and the lenient rewriting are what parsing is, and both answer for a
    // name the family does carry
    NamedEnum[Currency].parse("GBP") should haveValue(Currency.GBP)
    NamedEnum[Currency].valueOf("GBP") shouldBe Some(Currency.GBP)
    NamedEnum[Currency].valueOf("NotACurrency") shouldBe None
  }

  test("Decimal.of reports a value no decimal holds") {
    val notFinite: FailureOr[Decimal] = Decimal.of(Double.NaN)
    notFinite should beFailureWith(FailureReason.INVALID)
    val infinite: FailureOr[Decimal] = Decimal.of(Double.PositiveInfinity)
    infinite should beFailureWith(FailureReason.INVALID)

    // and a whole number needing more than eighteen digits is out of range
    val tooPrecise: FailureOr[Decimal] = Decimal.of(1000000000000000000L)
    tooPrecise should beFailureWith(FailureReason.INVALID)

    Decimal.of(1.5d) should beSuccess
  }

  test("Decimal.parse reports text that names no decimal") {
    val empty: FailureOr[Decimal] = Decimal.parse("")
    empty should beFailureWith(FailureReason.PARSING)
    val rubbish: FailureOr[Decimal] = Decimal.parse("Rubbish")
    rubbish should beFailureWith(FailureReason.PARSING)

    Decimal.parse("1.5") should haveValue(obtained(Decimal.of(1.5d)))
  }

  test("FixedScaleDecimal.of reports a scale the decimal cannot be held at") {
    val belowDecimal: ResultNec[FixedScaleDecimal] =
      FixedScaleDecimal.of(obtained(Decimal.of("1.2345")), 2)
    belowDecimal should beFailureWith(FailureReason.INVALID)
    val beyondMaximum: ResultNec[FixedScaleDecimal] =
      FixedScaleDecimal.of(obtained(Decimal.of(1L)), 19)
    beyondMaximum should beFailureWith(FailureReason.INVALID)

    FixedScaleDecimal.of(obtained(Decimal.of(1.5d)), 2) should beSuccess
  }

  //-------------------------------------------------------------------------
  // Index observations and floating rates. Eight entries. An observation computes dates from the
  // calendars of its index, so it is built only through a factory that resolves them.
  //-------------------------------------------------------------------------

  test("IborIndexObservation.of reports a calendar the reference data does not hold") {
    val unresolved: FailureOr[IborIndexObservation] =
      IborIndexObservation.of(IborIndices.GBP_LIBOR_3M, JAN_15, emptyRefData)
    unresolved should beFailureWith(FailureReason.MISSING_DATA)

    IborIndexObservation.of(IborIndices.GBP_LIBOR_3M, JAN_15, standardRefData) should beSuccess
  }

  test("OvernightIndexObservation.of reports a calendar the reference data does not hold") {
    val unresolved: FailureOr[OvernightIndexObservation] =
      OvernightIndexObservation.of(OvernightIndices.GBP_SONIA, JAN_15, emptyRefData)
    unresolved should beFailureWith(FailureReason.MISSING_DATA)

    OvernightIndexObservation.of(OvernightIndices.GBP_SONIA, JAN_15, standardRefData) should beSuccess
  }

  test("FxIndexObservation.of reports a calendar the reference data does not hold") {
    val unresolved: FailureOr[FxIndexObservation] =
      FxIndexObservation.of(FxIndices.EUR_USD_ECB, JAN_15, emptyRefData)
    unresolved should beFailureWith(FailureReason.MISSING_DATA)

    FxIndexObservation.of(FxIndices.EUR_USD_ECB, JAN_15, standardRefData) should beSuccess
  }

  test("FloatingRateName.toIborIndex reports a name of the wrong kind and a tenor no index carries") {
    // an Overnight name is not an Ibor name, which is a property of the name rather than of the call
    val wrongKind: FailureOr[Any] = FloatingRateNames.GBP_SONIA.toIborIndex(Tenor.TENOR_3M)
    wrongKind should beFailureWith(FailureReason.INVALID)

    // and the family publishes no three-week index, which is a property of the reference data
    val unpublished: FailureOr[Any] = FloatingRateNames.GBP_LIBOR.toIborIndex(Tenor.TENOR_3W)
    unpublished should beFailureWith(FailureReason.PARSING)

    FloatingRateNames.GBP_LIBOR.toIborIndex(Tenor.TENOR_3M) should haveValue(IborIndices.GBP_LIBOR_3M)
    // a tenor of one year normalises onto twelve months, as in the Java original
    FloatingRateNames.GBP_LIBOR.toIborIndex(Tenor.TENOR_1Y) should haveValue(IborIndices.GBP_LIBOR_12M)
  }

  test("FloatingRateName.toOvernightIndex reports a name of the wrong kind") {
    val wrongKind: FailureOr[Any] = FloatingRateNames.GBP_LIBOR.toOvernightIndex
    wrongKind should beFailureWith(FailureReason.INVALID)

    FloatingRateNames.GBP_SONIA.toOvernightIndex should haveValue(OvernightIndices.GBP_SONIA)
    // an averaging name converts to the index its name is that of, the suffix describing accrual
    FloatingRateNames.USD_FED_FUND_AVG.toOvernightIndex should haveValue(OvernightIndices.USD_FED_FUND)
  }

  test("FloatingRateName.toPriceIndex reports a name of the wrong kind") {
    val wrongKind: FailureOr[Any] = FloatingRateNames.GBP_LIBOR.toPriceIndex
    wrongKind should beFailureWith(FailureReason.INVALID)

    FloatingRateNames.GB_RPI.toPriceIndex should haveValue(PriceIndices.GB_RPI)
  }

  test("FloatingRateName.defaultIborIndex reports a currency with no published default") {
    // only 23 currencies publish one, and the Brazilian real - whose market has no term rate - is
    // one of those that does not
    val absent: FailureOr[FloatingRateName] = FloatingRateName.defaultIborIndex(Currency.BRL)
    absent should beFailureWith(FailureReason.MISSING_DATA)

    FloatingRateName.defaultIborIndex(Currency.GBP) should haveValue(FloatingRateNames.GBP_LIBOR)
  }

  test("FloatingRateName.defaultOvernightIndex reports a currency with no published default") {
    val absent: FailureOr[FloatingRateName] = FloatingRateName.defaultOvernightIndex(Currency.KRW)
    absent should beFailureWith(FailureReason.MISSING_DATA)

    FloatingRateName.defaultOvernightIndex(Currency.GBP) should haveValue(FloatingRateNames.GBP_SONIA)
  }


  //-------------------------------------------------------------------------
  // The other side of the classification line: the twelve documented `ArgCheck` refusals of AAP
  // section 0.3.3, plus the one member this port makes more total than Java. Every `intercept`
  // below carries the reason that particular refusal is a caller contract rather than a value:
  // the condition it guards is a property of how the method was called and not of the data it was
  // given, so a caller cannot correct it by supplying better data - it has to call differently.
  // Both `ArgCheck` throws are `IllegalArgumentException`, that object being the single place in
  // either module where a throw is written.
  //
  // The two numeric-edge throws of the same section - `CurrencyAmount` arithmetic reaching `NaN`
  // from infinite operands, and `Decimal` arithmetic overflowing eighteen digits - are asserted by
  // `SmartConstructorSpec`, which owns every construction-time refusal, and are deliberately not
  // duplicated here.
  //-------------------------------------------------------------------------

  test("DoubleArray.get raises for an index outside the array") {
    // Contract, not data: an index is the caller's own arithmetic over a size it can read, so an
    // index outside the array is a mistake in the calling code. The Java original let the array
    // access raise, and so does this port, which is why the exception is the one the JVM throws
    // for a bad array index rather than one this library composes.
    intercept[IndexOutOfBoundsException](threeValues.get(3))
    intercept[IndexOutOfBoundsException](threeValues.get(-1))

    threeValues.get(0) shouldBe 1d
    threeValues.get(2) shouldBe 3d
  }

  test("DoubleArray element-wise arithmetic raises for arrays of different sizes") {
    // Contract, not data: two arrays of different lengths have no element-wise sum, and the caller
    // holds both sizes before it calls. AAP 0.3.3 lists the dimension errors of the two numeric
    // wrappers as fail-fast throws for that reason, which also keeps the inner loops total.
    intercept[IllegalArgumentException](threeValues.plus(twoValues))
    intercept[IllegalArgumentException](threeValues.minus(twoValues))
    intercept[IllegalArgumentException](threeValues.multipliedBy(twoValues))
    intercept[IllegalArgumentException](threeValues.dividedBy(twoValues))
    intercept[IllegalArgumentException](threeValues.combine(twoValues, (a, b) => a + b))

    // and the reduction over an empty array has no answer to give
    intercept[IllegalArgumentException](DoubleArray.of().min)
    intercept[IllegalArgumentException](DoubleArray.of().max)

    threeValues.plus(threeValues) shouldBe DoubleArray.of(2d, 4d, 6d)
  }

  test("DoubleMatrix.get raises for a position outside the matrix") {
    // Contract, not data, for the reason given on `DoubleArray.get`: the row and the column are
    // the caller's arithmetic over dimensions it can read.
    intercept[IndexOutOfBoundsException](squareMatrix.get(2, 0))
    intercept[IndexOutOfBoundsException](squareMatrix.get(0, 2))
    intercept[IndexOutOfBoundsException](squareMatrix.get(-1, 0))

    squareMatrix.get(0, 0) shouldBe 1d
    squareMatrix.get(1, 1) shouldBe 4d
  }

  test("DoubleMatrix element-wise arithmetic raises for matrices of different shapes") {
    // Contract, not data: two matrices of different shapes have no element-wise sum, and the
    // shapes are known to the caller before the call.
    intercept[IllegalArgumentException](squareMatrix.plus(flatMatrix))
    intercept[IllegalArgumentException](squareMatrix.minus(flatMatrix))
    intercept[IllegalArgumentException](squareMatrix.combine(flatMatrix, (a, b) => a * b))

    squareMatrix.plus(squareMatrix) shouldBe DoubleMatrix.of(2, 2, 2d, 4d, 6d, 8d)
  }

  test("HolidayCalendar operations raise for a year outside zero to 9999") {
    // Contract, not data: a calendar holds its holidays as an array of months from its first year,
    // so a date whose year lies outside 0 to 9999 is a date no calendar could hold data for and
    // the question cannot be asked at all. That is a property of the argument's position in the
    // time line rather than of the holidays supplied, so AAP 0.3.3 keeps it a fail-fast refusal.
    intercept[IllegalArgumentException](datedCalendar.isHoliday(LocalDate.MIN)).getMessage should
      include(UnsupportedDateMessage)
    intercept[IllegalArgumentException](datedCalendar.isHoliday(LocalDate.MAX)).getMessage should
      include(UnsupportedDateMessage)
    intercept[IllegalArgumentException](datedCalendar.next(LocalDate.MAX.minusDays(1L))).getMessage should
      include(UnsupportedDateMessage)
    intercept[IllegalArgumentException](datedCalendar.shift(LocalDate.MIN, 1)).getMessage should
      include(UnsupportedDateMessage)
    intercept[IllegalArgumentException](
      datedCalendar.daysBetween(LocalDate.MIN, LocalDate.of(2014, 1, 1))).getMessage should
      include(UnsupportedDateMessage)

    // every date of the supported range answers, including the years the calendar holds no data for
    datedCalendar.isHoliday(LocalDate.of(2014, 1, 1)) shouldBe true
    datedCalendar.isHoliday(LocalDate.of(1800, 1, 1)) shouldBe false
  }

  test("DayCount.yearFraction raises for dates out of time-line order") {
    // Contract, not data: a year fraction is defined over a period, and a pair of dates in the
    // wrong order is not a period but a caller that swapped its arguments. The relative form
    // exists precisely for a reversed pair, so the refusal cannot be about the data.
    intercept[IllegalArgumentException](DayCounts.ACT_365F.yearFraction(APR_15, JAN_15)).getMessage shouldBe
      "Dates must be in time-line order"
    intercept[IllegalArgumentException](DayCounts.ACT_360.days(APR_15, JAN_15)).getMessage shouldBe
      "Dates must be in time-line order"

    DayCounts.ACT_365F.yearFraction(JAN_15, APR_15) should be > 0d
    DayCounts.ACT_360.days(JAN_15, APR_15) shouldBe 90
  }

  test("DayCount.relativeYearFraction accepts the reversed pair its own contract allows") {
    // The counterpart of the entry above, and the reason that one is a contract rather than a
    // value: the relative form documents that its dates may be given in either order, so it
    // bypasses the order check and negates the result instead of refusing.
    val forwards: Double = DayCounts.ACT_365F.relativeYearFraction(JAN_15, APR_15)
    val backwards: Double = DayCounts.ACT_365F.relativeYearFraction(APR_15, JAN_15)
    forwards should be > 0d
    backwards shouldBe -forwards
  }

  test("DayCount Act/Act ICMA raises for schedule information it is not given") {
    // Contract, not data: the rule of this convention is defined in terms of the end of the
    // schedule, the end of the period containing the first date and the frequency. A schedule that
    // cannot supply them is a caller asking this convention a question it has no rule for - which
    // is what Java's `UnsupportedOperationException` said - so the port refuses through `ArgCheck`
    // even though the accessors themselves are now total and answer `None` (AAP 0.6.1).
    val simple: DayCount.ScheduleInfo = DayCount.ScheduleInfo.simple
    intercept[IllegalArgumentException](
      DayCounts.ACT_ACT_ICMA.yearFraction(JAN_15, APR_15, simple)).getMessage shouldBe
      "The end date of the schedule is required"

    // supplying the end of the schedule moves the refusal on to the next fact the rule reads
    val endOnly: DayCount.ScheduleInfo = new DayCount.ScheduleInfo {
      override def endDate: Option[LocalDate] = Some(OCT_15)
    }
    intercept[IllegalArgumentException](
      DayCounts.ACT_ACT_ICMA.yearFraction(JAN_15, APR_15, endOnly)).getMessage shouldBe
      "The end date of the schedule period is required"

    // and a schedule that knows the dates but not the frequency is refused for the frequency
    val withoutFrequency: DayCount.ScheduleInfo = new DayCount.ScheduleInfo {
      override def endDate: Option[LocalDate] = Some(OCT_15)
      override def periodEndDate(date: LocalDate): Option[LocalDate] = Some(APR_15)
    }
    intercept[IllegalArgumentException](
      DayCounts.ACT_ACT_ICMA.yearFraction(JAN_15, APR_15, withoutFrequency)).getMessage shouldBe
      "The frequency of the schedule is required"

    // the real schedule carries all three, so the convention calculates against it
    DayCounts.ACT_ACT_ICMA.yearFraction(JAN_15, APR_15, schedule) should be > 0d
  }

  test("DayCount Act/365L raises for schedule information it is not given") {
    // Contract, not data, for the reason given above: this convention divides by 365 or 366
    // according to the end of the period containing the first date and whether the frequency is
    // annual, so neither fact is optional to its rule.
    val simple: DayCount.ScheduleInfo = DayCount.ScheduleInfo.simple
    intercept[IllegalArgumentException](
      DayCounts.ACT_365L.yearFraction(JAN_15, APR_15, simple)).getMessage shouldBe
      "The end date of the schedule period is required"

    val withoutFrequency: DayCount.ScheduleInfo = new DayCount.ScheduleInfo {
      override def periodEndDate(date: LocalDate): Option[LocalDate] = Some(APR_15)
    }
    intercept[IllegalArgumentException](
      DayCounts.ACT_365L.yearFraction(JAN_15, APR_15, withoutFrequency)).getMessage shouldBe
      "The frequency of the schedule is required"

    DayCounts.ACT_365L.yearFraction(JAN_15, APR_15, schedule) should be > 0d
  }

  test("DayCount 30E/360 ISDA raises for the schedule end it reads at the end of February") {
    // Contract, not data, and the narrowest of the four: this convention reads the end of the
    // schedule only to decide whether a second date that is the last day of February is the final
    // date of the schedule. A pair that reaches that branch without a schedule end is a caller
    // asking for a rule that cannot be evaluated; every other pair never asks.
    val simple: DayCount.ScheduleInfo = DayCount.ScheduleInfo.simple
    intercept[IllegalArgumentException](
      DayCounts.THIRTY_E_360_ISDA.yearFraction(
        LocalDate.of(2011, 12, 28),
        LocalDate.of(2012, 2, 29),
        simple)).getMessage shouldBe "The end date of the schedule is required"

    // the same convention, the same schedule information, a second date that is not the last day
    // of February: no fact is read and the calculation answers
    DayCounts.THIRTY_E_360_ISDA.yearFraction(JAN_15, APR_15, simple) shouldBe (90d / 360d)
  }

  test("DayCount 30U/360 reads the end-of-month flag and never refuses for it") {
    // The fourth reader of schedule information, and the one that cannot refuse: the flag is the
    // single accessor of `ScheduleInfo` that is not optional - it defaults to true, as the Java
    // interface defaulted it - so this convention always has the fact its rule reads. It is listed
    // in the inventory beside the three above to record exactly that asymmetry.
    val simple: DayCount.ScheduleInfo = DayCount.ScheduleInfo.simple
    simple.isEndOfMonthConvention shouldBe true
    DayCounts.THIRTY_U_360.yearFraction(JAN_15, APR_15, simple) shouldBe (90d / 360d)

    // and a schedule that turns the flag off is answered by the other rule, not refused
    val withoutEom: DayCount.ScheduleInfo = new DayCount.ScheduleInfo {
      override def isEndOfMonthConvention: Boolean = false
    }
    // a leap-year February month-end to the next February month-end: with the flag set, the
    // end-of-month rule moves both dates to the thirtieth and the period is a whole year; with it
    // clear, the ISDA rule leaves the 29th and the 28th where they are and the period is a day
    // short. The flag is read, and the difference is the reading - not a refusal.
    val endOfFebruary: LocalDate = LocalDate.of(2012, 2, 29)
    val eomResult: Double = DayCounts.THIRTY_U_360.yearFraction(endOfFebruary, LocalDate.of(2013, 2, 28), simple)
    val plainResult: Double =
      DayCounts.THIRTY_U_360.yearFraction(endOfFebruary, LocalDate.of(2013, 2, 28), withoutEom)
    eomResult shouldBe (360d / 360d)
    plainResult shouldBe (359d / 360d)
  }

  test("Schedule.periodEndDate answers None for a date outside every period") {
    // The one place this port is more total than Java: `getPeriodEndDate` threw for a date the
    // schedule does not contain, and here it answers `None`. A date outside the schedule is data
    // rather than a broken call - the caller may hold a date from anywhere - so it is asserted as a
    // `None` and never as a throw. The conventions that read this accessor refuse on their own
    // behalf, which is the entry above.
    schedule.periodEndDate(JAN_15) shouldBe Some(APR_15)
    schedule.periodEndDate(APR_15) shouldBe Some(JUL_15)
    schedule.periodEndDate(JAN_15.minusDays(1L)) shouldBe None
    schedule.periodEndDate(OCT_15) shouldBe None
    schedule.periodEndDate(OCT_15.plusYears(5L)) shouldBe None

    // the other three members of the same interface are total in the same way
    schedule.startDate shouldBe Some(JAN_15)
    schedule.endDate shouldBe Some(OCT_15)
    schedule.frequency shouldBe Some(Frequency.P3M)
  }

  //-------------------------------------------------------------------------
  /**
   * The inventory of AAP section 0.3.3, entry by entry, paired with the test that covers it.
   *
   * These two lists are the gate this suite exists for, written out so that a reviewer counts
   * entries rather than tests: the first holds every public method the inventory classifies as
   * reporting its failure as a value, and the second every documented `ArgCheck` refusal and the
   * one member this port makes more total than Java. `inventory_coverage` asserts that the test
   * named beside each entry exists in this suite, so an entry whose test is renamed or removed
   * fails the suite instead of passing unnoticed.
   *
   * Several entries share one test where they share one failure shape, which is why the test names
   * repeat; each such test holds one table row per entry, so the mapping stays one-to-one where it
   * matters - in what is actually asserted.
   */
  private val eitherEntries: List[(String, String)] = List(
    // FX and currency, 34 entries
    "FxRateProvider.fxRate" -> "FxRateProvider.fxRate reports a rate the provider cannot supply",
    "FxRateProvider.convert" -> "FxRateProvider.convert reports a rate the provider cannot supply",
    "FxMatrix.of" -> "FxMatrix.of reports rates that can never be placed",
    "FxMatrix.withRate" -> "FxMatrix.withRate reports a pair the matrix has no currency in common with",
    "FxMatrix.merge" -> "FxMatrix.merge reports two matrices with no currency in common",
    "FxMatrix.fxRate" -> "FxMatrix.fxRate reports a rate the matrix does not hold",
    "FxMatrix.convert" -> "FxMatrix.convert reports a rate the matrix does not hold",
    "FxRate.crossRate" -> "FxRate.crossRate reports rates that do not cross",
    "FxRate.parse" -> "FxRate.parse reports text that names no rate",
    "FxIndex.of(CurrencyPair)" -> "FxIndex.of(CurrencyPair) reports a pair no published index quotes",
    "FxIndex.of(String)" -> "FxIndex.of(String) reports text that names no published index",
    "CurrencyAmount.plus(CurrencyAmount)" -> "CurrencyAmount.plus and minus report a currency mismatch",
    "CurrencyAmount.minus(CurrencyAmount)" -> "CurrencyAmount.plus and minus report a currency mismatch",
    "CurrencyAmount.parse" -> "CurrencyAmount.parse reports text that names no amount",
    "CurrencyAmount.convertedTo(currency, rate)" ->
      "CurrencyAmount.convertedTo reports a non-unit rate for the same currency",
    "Money.convertedTo(currency, rate)" ->
      "Money.convertedTo and BigMoney.convertedTo report a non-unit rate for the same currency",
    "BigMoney.convertedTo(currency, rate)" ->
      "Money.convertedTo and BigMoney.convertedTo report a non-unit rate for the same currency",
    "Money.plus" -> "Money.plus, Money.minus, BigMoney.plus and BigMoney.minus report a currency mismatch",
    "Money.minus" -> "Money.plus, Money.minus, BigMoney.plus and BigMoney.minus report a currency mismatch",
    "BigMoney.plus" -> "Money.plus, Money.minus, BigMoney.plus and BigMoney.minus report a currency mismatch",
    "BigMoney.minus" -> "Money.plus, Money.minus, BigMoney.plus and BigMoney.minus report a currency mismatch",
    "CurrencyAmountArray.plus(CurrencyAmountArray)" ->
      "CurrencyAmountArray arithmetic reports size and currency mismatches",
    "CurrencyAmountArray.minus(CurrencyAmountArray)" ->
      "CurrencyAmountArray arithmetic reports size and currency mismatches",
    "CurrencyAmountArray.plus(CurrencyAmount)" ->
      "CurrencyAmountArray arithmetic reports size and currency mismatches",
    "CurrencyAmountArray.minus(CurrencyAmount)" ->
      "CurrencyAmountArray arithmetic reports size and currency mismatches",
    "MultiCurrencyAmountArray.getValues" ->
      "MultiCurrencyAmountArray.getValues reports a currency the run does not hold",
    "MultiCurrencyAmountArray.plus(MultiCurrencyAmountArray)" ->
      "MultiCurrencyAmountArray arithmetic reports a size mismatch",
    "MultiCurrencyAmountArray.minus(MultiCurrencyAmountArray)" ->
      "MultiCurrencyAmountArray arithmetic reports a size mismatch",
    "MultiCurrencyAmountArray.plus(MultiCurrencyAmount)" ->
      "MultiCurrencyAmountArray arithmetic reports a size mismatch",
    "MultiCurrencyAmountArray.minus(MultiCurrencyAmount)" ->
      "MultiCurrencyAmountArray arithmetic reports a size mismatch",
    "MultiCurrencyAmount.getAmount" ->
      "MultiCurrencyAmount.getAmount reports a currency the amount does not hold",
    "CurrencyPair.other" -> "CurrencyPair.other reports a currency that is not in the pair",
    "CurrencyPair.parse" -> "CurrencyPair.parse reports text that names no pair",
    "Currency.parse" -> "Currency.parse reports a code outside the closed family",
    // Location, 2 entries
    "Country.of" -> "Country.of reports a malformed code and accepts any well-formed one",
    "Country.of3Char" -> "Country.of3Char reports a code that names no country",
    // Schedule and frequency, 15 entries
    "Frequency.eventsPerYear" -> "Frequency.eventsPerYear reports a frequency with no exact count",
    "Frequency.exactDivide" ->
      "Frequency.exactDivide reports a non-integral ratio and divides an integral one",
    "Frequency.of" -> "Frequency.of reports a period that is no frequency",
    "Frequency.parse" -> "Frequency.parse reports text that names no frequency",
    "PeriodicSchedule.of" -> "PeriodicSchedule.of reports dates that describe no schedule",
    "PeriodicSchedule.createSchedule" ->
      "PeriodicSchedule.createSchedule reports a definition that generates nothing",
    "PeriodicSchedule.createUnadjustedDates" ->
      "PeriodicSchedule.createUnadjustedDates reports a disallowed stub",
    "PeriodicSchedule.createAdjustedDates" ->
      "PeriodicSchedule.createAdjustedDates reports dates that adjust onto one another",
    "SchedulePeriod.of" -> "SchedulePeriod.of reports dates that describe no period",
    "Schedule.of" -> "Schedule.of accepts the periods its own type already guarantees",
    "Schedule.merge" -> "Schedule.merge reports a date that matches no period of the schedule",
    "Schedule.mergeRegular" -> "Schedule.mergeRegular reports an unusable group size",
    "Schedule.toAdjusted" -> "Schedule.toAdjusted reports a period that collapses once adjusted",
    "RollConvention.ofDayOfMonth" -> "RollConvention.ofDayOfMonth reports a day outside one to thirty-one",
    "RollConvention.ofDayOfWeek" -> "RollConvention.ofDayOfWeek is total for every day of the week",
    // Dates and adjustments, 11 entries
    "Tenor.of" -> "Tenor.of reports a period that is no tenor",
    "Tenor.parse" -> "Tenor.parse reports text that names no tenor",
    "MarketTenor.of" -> "MarketTenor spot factories report a count that is no tenor",
    "MarketTenor.parse" -> "MarketTenor.parse reports text that names no market tenor",
    "SequenceDate.of" -> "SequenceDate.of reports fields that describe no instruction",
    "DaysAdjustment.of" -> "DaysAdjustment resolution reports a calendar the reference data does not hold",
    "PeriodAdjustment.of" -> "PeriodAdjustment.of reports a period the convention cannot add",
    "TenorAdjustment.of" -> "TenorAdjustment.of reports a tenor the convention cannot add",
    "AdjustableDates.of" -> "AdjustableDates.of reports dates that describe no set",
    "HolidayCalendarId.resolve" ->
      "HolidayCalendarId.resolve reports a calendar the reference data does not hold",
    "DayCount.ofBus252(id, refData)" ->
      "DayCount.ofBus252 reports a calendar the reference data does not hold",
    // Reference data, 2 entries
    "ReferenceDataId.resolve" ->
      "ReferenceDataId.resolve reports an identifier the reference data does not hold",
    "ReferenceData.getValue" ->
      "ReferenceData.getValue reports an identifier the reference data does not hold",
    // Identifiers, 3 entries
    "StandardId.of" -> "StandardId.of reports parts that name no identifier",
    "StandardId.parse" -> "StandardId.parse reports text that names no identifier",
    "StandardSchemes.splitTicMic" -> "StandardSchemes.splitTicMic reports an identifier that is no TICMIC",
    // Values, 9 entries
    "CurrencyAmountArray.of" -> "CurrencyAmountArray.of reports an empty collection and mixed currencies",
    "MultiCurrencyAmountArray.of" -> "MultiCurrencyAmountArray.of reports values of unequal length",
    "MultiCurrencyAmount.of" -> "MultiCurrencyAmount.of reports a duplicated currency",
    "ValueStep.of" -> "ValueStep.of reports a position that names no period",
    "ValueSchedule.of" -> "ValueSchedule.of is total, every judgement belonging to resolution",
    "ValueStepSequence.of" -> "ValueStepSequence.of reports arguments that describe no sequence",
    "ValueSchedule.resolveValues" -> "ValueSchedule.resolveValues reports a step the schedule cannot carry",
    "Rounding.ofDecimalPlaces" -> "Rounding.ofDecimalPlaces reports a count outside zero to 255",
    "Rounding.ofFractionalDecimalPlaces" -> "Rounding.ofFractionalDecimalPlaces accumulates both rejections",
    // Collect members reached from this module, 4 entries
    "NamedEnum.parse" -> "NamedEnum.parse reports a name no member of the family carries",
    "Decimal.of" -> "Decimal.of reports a value no decimal holds",
    "Decimal.parse" -> "Decimal.parse reports text that names no decimal",
    "FixedScaleDecimal.of" -> "FixedScaleDecimal.of reports a scale the decimal cannot be held at",
    // Index observations and floating rates, 8 entries
    "IborIndexObservation.of" ->
      "IborIndexObservation.of reports a calendar the reference data does not hold",
    "OvernightIndexObservation.of" ->
      "OvernightIndexObservation.of reports a calendar the reference data does not hold",
    "FxIndexObservation.of" -> "FxIndexObservation.of reports a calendar the reference data does not hold",
    "FloatingRateName.toIborIndex" ->
      "FloatingRateName.toIborIndex reports a name of the wrong kind and a tenor no index carries",
    "FloatingRateName.toOvernightIndex" ->
      "FloatingRateName.toOvernightIndex reports a name of the wrong kind",
    "FloatingRateName.toPriceIndex" -> "FloatingRateName.toPriceIndex reports a name of the wrong kind",
    "FloatingRateName.defaultIborIndex" ->
      "FloatingRateName.defaultIborIndex reports a currency with no published default",
    "FloatingRateName.defaultOvernightIndex" ->
      "FloatingRateName.defaultOvernightIndex reports a currency with no published default")

  /** The documented contract refusals of AAP 0.3.3, and the one member made more total than Java. */
  private val contractEntries: List[(String, String)] = List(
    "DoubleArray index" -> "DoubleArray.get raises for an index outside the array",
    "DoubleArray dimensions" -> "DoubleArray element-wise arithmetic raises for arrays of different sizes",
    "DoubleMatrix index" -> "DoubleMatrix.get raises for a position outside the matrix",
    "DoubleMatrix dimensions" ->
      "DoubleMatrix element-wise arithmetic raises for matrices of different shapes",
    "HolidayCalendar years 0 to 9999" -> "HolidayCalendar operations raise for a year outside zero to 9999",
    "DayCount.yearFraction dates in order" -> "DayCount.yearFraction raises for dates out of time-line order",
    "DayCount.relativeYearFraction reversed pair" ->
      "DayCount.relativeYearFraction accepts the reversed pair its own contract allows",
    "DayCount Act/Act ICMA ScheduleInfo" ->
      "DayCount Act/Act ICMA raises for schedule information it is not given",
    "DayCount Act/365L ScheduleInfo" -> "DayCount Act/365L raises for schedule information it is not given",
    "DayCount 30E/360 ISDA ScheduleInfo" ->
      "DayCount 30E/360 ISDA raises for the schedule end it reads at the end of February",
    "DayCount 30U/360 end-of-month flag" ->
      "DayCount 30U/360 reads the end-of-month flag and never refuses for it",
    "Schedule.periodEndDate totality" -> "Schedule.periodEndDate answers None for a date outside every period")

  test("inventory_coverage") {
    // the counts of AAP 0.3.3, stated so that an entry lost in an edit is a failing assertion
    eitherEntries should have size 88
    contractEntries should have size 12

    val declared: Set[String] = testNames
    val uncovered: List[String] =
      (eitherEntries ::: contractEntries).collect {
        case (entry, testName) if !declared.contains(testName) => s"$entry -> '$testName'"
      }
    withClue("inventory entries whose test does not exist in this suite: ") {
      uncovered shouldBe empty
    }

    // every entry is named once, so the inventory cannot be padded by repeating one
    val entryNames: List[String] = (eitherEntries ::: contractEntries).map { case (entry, _) => entry }
    entryNames.distinct.size shouldBe entryNames.size
  }
}
