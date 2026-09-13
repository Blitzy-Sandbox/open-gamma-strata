/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import java.io.File
import java.math.BigDecimal
import java.math.RoundingMode
import java.time.DateTimeException
import java.time.DayOfWeek
import java.time.LocalDate
import java.time.Period
import java.time.YearMonth

import scala.annotation.tailrec
import scala.io.Codec
import scala.io.Source
import scala.reflect.ClassTag
import scala.util.matching.Regex

import cats.data.NonEmptyList
import cats.syntax.traverse._

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2

import com.opengamma.strata.basics.currency.AdjustablePayment
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
import com.opengamma.strata.basics.currency.Payment
import com.opengamma.strata.basics.date.AdjustableDate
import com.opengamma.strata.basics.date.AdjustableDates
import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.basics.date.BusinessDayConvention
import com.opengamma.strata.basics.date.BusinessDayConventions
import com.opengamma.strata.basics.date.DateAdjuster
import com.opengamma.strata.basics.date.DateSequence
import com.opengamma.strata.basics.date.DateSequences
import com.opengamma.strata.basics.date.DayCount
import com.opengamma.strata.basics.date.DayCounts
import com.opengamma.strata.basics.date.DaysAdjustment
import com.opengamma.strata.basics.date.HolidayCalendar
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.basics.date.HolidayCalendarIds
import com.opengamma.strata.basics.date.HolidayCalendars
import com.opengamma.strata.basics.date.ImmutableHolidayCalendar
import com.opengamma.strata.basics.date.MarketTenor
import com.opengamma.strata.basics.date.PeriodAdditionConvention
import com.opengamma.strata.basics.date.PeriodAdditionConventions
import com.opengamma.strata.basics.date.PeriodAdjustment
import com.opengamma.strata.basics.date.SequenceDate
import com.opengamma.strata.basics.date.Tenor
import com.opengamma.strata.basics.date.TenorAdjustment
import com.opengamma.strata.basics.index.FloatingRate
import com.opengamma.strata.basics.index.FloatingRateIndex
import com.opengamma.strata.basics.index.FloatingRateName
import com.opengamma.strata.basics.index.FloatingRateNames
import com.opengamma.strata.basics.index.FloatingRateType
import com.opengamma.strata.basics.index.FxIndex
import com.opengamma.strata.basics.index.FxIndexObservation
import com.opengamma.strata.basics.index.FxIndices
import com.opengamma.strata.basics.index.IborIndex
import com.opengamma.strata.basics.index.IborIndexObservation
import com.opengamma.strata.basics.index.IborIndices
import com.opengamma.strata.basics.index.Index
import com.opengamma.strata.basics.index.OvernightIndex
import com.opengamma.strata.basics.index.OvernightIndexObservation
import com.opengamma.strata.basics.index.OvernightIndices
import com.opengamma.strata.basics.index.PriceIndex
import com.opengamma.strata.basics.index.PriceIndices
import com.opengamma.strata.basics.index.RateIndex
import com.opengamma.strata.basics.location.Country
import com.opengamma.strata.basics.schedule.Frequency
import com.opengamma.strata.basics.schedule.PeriodicSchedule
import com.opengamma.strata.basics.schedule.RollConvention
import com.opengamma.strata.basics.schedule.RollConventions
import com.opengamma.strata.basics.schedule.Schedule
import com.opengamma.strata.basics.schedule.SchedulePeriod
import com.opengamma.strata.basics.schedule.StubConvention
import com.opengamma.strata.basics.value.HalfUp
import com.opengamma.strata.basics.value.Rounding
import com.opengamma.strata.basics.value.ValueAdjustment
import com.opengamma.strata.basics.value.ValueAdjustmentType
import com.opengamma.strata.basics.value.ValueDerivatives
import com.opengamma.strata.basics.value.ValueSchedule
import com.opengamma.strata.basics.value.ValueStep
import com.opengamma.strata.basics.value.ValueStepSequence
import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.Collections
import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.DoubleArrayMath
import com.opengamma.strata.collect.FixedScaleDecimal
import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.TypedStringCompanion
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.array.DoubleMatrix
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.result.ValueWithFailures
import com.opengamma.strata.collect.testkit.Outcome
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Asserts the failable public '''method''' surface of both modules, one case per owner and method
 * family enumerated from their sources and one per entry of the documented inventory.
 * This suite and `SmartConstructorSpec`, which owns the constructor half of the same inventory,
 * are the explicit-error-handling gate:
 * `sbt -batch "testOnly *SmartConstructorSpec *FailableSurfaceSpec *ApiSurfaceSpec *FailureSpec"`.
 *
 * ===The classification rule===
 *
 * One rule places every failing member on one side of a line:
 *
 *   - a failure that depends on the '''data''' of the arguments - mismatched currencies, an
 *     unknown identifier, unequal sizes, unparseable text, an inconsistent schedule definition -
 *     is reported as a value, through `Either[Failure, A]` or its accumulating form
 *     `EitherNec[Failure, A]`;
 *   - a refusal that guards a '''caller contract''' independent of the values supplied - an index
 *     within bounds, dates given in order, the schedule information a convention is defined in
 *     terms of - or a '''numeric domain edge''' is a fail-fast `ArgCheck` throw, documented on the
 *     method that raises it.
 *
 * A member on the reporting side is asserted to return a `Left`; one on the raising side is
 * asserted with `intercept`, and each hand-written `intercept` names why that refusal is a
 * contract or a numeric edge rather than a value.
 *
 * ===The enumeration is derived, not transcribed===
 *
 * A suite asserting only the names the plan lists would cover whatever the list happened to hold,
 * so this one reads the sources of both modules while it is being built, keys the failable
 * declarations it finds by `Owner.method`, and requires one '''row''' of its registry per family
 * and one '''claim''' per signature. The coverage tests at the end compare rows and claims with
 * the enumeration in both directions, so a member added to either module, or one whose signature
 * changes, fails here until something accounts for it. Every count the suite reports is computed
 * from the enumeration or the registry and printed by `derived_inventory_counts`; none is written
 * down. The enumeration and the four row kinds are described where they are built, below.
 *
 * The plan's own list is kept as [[aapFailableEntries]], [[aapReconciledEntries]] and
 * [[aapContractEntries]], each entry paired with the hand-written test that covers it. Two entries
 * name no declaration of their own and are reconciled by name, each carrying its reason and the
 * row or test that stands in for it. The hand-written tests assert messages, attributes,
 * accumulation counts and success cases that a generated row does not; the row for such a member
 * names its test with `alsoAssertedBy`, and the coverage tests check the test named still exists.
 *
 * ===What neighbouring suites assert instead===
 *
 * The accumulation behaviour of the validated factories, and the messages of the two numeric-edge
 * refusals, are `SmartConstructorSpec`'s; here a factory appears only where the inventory lists
 * it, where the enumeration requires a row, or where it builds a fixture. The message '''text'''
 * of a failure belongs to the spec of the type that produces it: a failure here is asserted by its
 * [[FailureReason]], compared as a value of the closed family rather than as a string, and by the
 * attribute the contract fixes where it fixes one - the `definition` of a rejected schedule.
 *
 * ===Success cases and reference data===
 *
 * A suite in which everything failed would satisfy every failure assertion, so the entries whose
 * failure is easiest to trigger by accident carry the success case too. Reference data is threaded
 * explicitly at every call: the success side of a resolving member is asserted against
 * [[standardRefData]] and its failure side against [[emptyRefData]], which holds nothing at all.
 * No ambient default is consulted anywhere.
 *
 * @see `SmartConstructorSpec` for the constructor half of the same inventory
 * @see `ApiSurfaceSpec` for the compile-time half - the absence of `apply` and `copy`
 */
final class FailableSurfaceSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  import FailableSurfaceSpec._

  //-------------------------------------------------------------------------
  // Dates. The first four are the boundaries of the fixture schedule; the last two are the pair
  // that needs a stub the 'None' convention forbids.

  private val JAN_15: LocalDate = LocalDate.of(2014, 1, 15)

  private val APR_15: LocalDate = LocalDate.of(2014, 4, 15)

  private val JUL_15: LocalDate = LocalDate.of(2014, 7, 15)

  private val OCT_15: LocalDate = LocalDate.of(2014, 10, 15)

  private val JUN_04: LocalDate = LocalDate.of(2014, 6, 4)

  private val SEP_17: LocalDate = LocalDate.of(2014, 9, 17)

  /** The attribute key a rejected schedule definition is carried under. */
  private val DefinitionAttribute: String = "definition"

  /** The part of an `ArgCheck` message that names the year precondition of a holiday calendar. */
  private val UnsupportedDateMessage: String = "outside the accepted range"

  /** The part of an `ArgCheck` message that names the limit on how far a shift may walk. */
  private val ShiftMagnitudeMessage: String = "business days is outside the accepted range"

  /** The part of an `ArgCheck` message that names the limit on how deep a composite may read. */
  private val CompositeDepthMessage: String = "cannot read through more than"

  /** The part of an `ArgCheck` message that names a calendar with no possible business day. */
  private val NoBusinessDayMessage: String = "no business day"

  //-------------------------------------------------------------------------
  // Reference data, threaded explicitly into every resolving call.

  private lazy val standardRefData: ReferenceData = ReferenceData.standard

  private lazy val emptyRefData: ReferenceData = ReferenceData.empty

  //-------------------------------------------------------------------------
  // Currency fixtures. Each pair disagrees in exactly one respect - the currency, or the size of
  // the run - so that the failure a member reports for it has one cause.

  private lazy val gbp100: CurrencyAmount = obtained(CurrencyAmount.of(Currency.GBP, 100d))

  private lazy val usd100: CurrencyAmount = obtained(CurrencyAmount.of(Currency.USD, 100d))

  /** One hundred euro, which the pound/dollar matrix holds no rate for. */
  private lazy val eur100: CurrencyAmount = obtained(CurrencyAmount.of(Currency.EUR, 100d))

  private lazy val gbpMoney: Money = obtained(Money.of(Currency.GBP, 100d))

  private lazy val usdMoney: Money = obtained(Money.of(Currency.USD, 100d))

  private lazy val gbpBigMoney: BigMoney = obtained(BigMoney.of(Currency.GBP, 100d))

  private lazy val usdBigMoney: BigMoney = obtained(BigMoney.of(Currency.USD, 100d))

  /**
   * An infinite amount, which this type admits and no decimal holds.
   *
   * An amount rejects only a value that is not a number, so the two infinities are values of it
   * and neither is a value of either exact-decimal type. This is the failure side of the two
   * conversions to those types.
   */
  private lazy val infiniteAmount: CurrencyAmount =
    obtained(CurrencyAmount.of(Currency.GBP, Double.PositiveInfinity))

  /** A finite amount whose magnitude needs more than the eighteen digits a decimal holds. */
  private lazy val oversizedAmount: CurrencyAmount = obtained(CurrencyAmount.of(Currency.GBP, 1e30d))

  /** The decimal two, which as a rate no conversion into a currency's own currency may apply. */
  private lazy val two: Decimal = obtained(Decimal.of(2L))

  private lazy val gbpArray: CurrencyAmountArray =
    CurrencyAmountArray.of(Currency.GBP, DoubleArray.of(1d, 2d, 3d))

  private lazy val usdArray: CurrencyAmountArray =
    CurrencyAmountArray.of(Currency.USD, DoubleArray.of(1d, 2d, 3d))

  private lazy val shortGbpArray: CurrencyAmountArray =
    CurrencyAmountArray.of(Currency.GBP, DoubleArray.of(1d, 2d))

  private lazy val gbpRun: MultiCurrencyAmountArray =
    accepted(MultiCurrencyAmountArray.of(Map(Currency.GBP -> DoubleArray.of(1d, 2d, 3d))))

  private lazy val shortRun: MultiCurrencyAmountArray =
    accepted(MultiCurrencyAmountArray.of(Map(Currency.GBP -> DoubleArray.of(1d, 2d))))

  private lazy val gbpMulti: MultiCurrencyAmount = obtained(MultiCurrencyAmount.of(gbp100))

  //-------------------------------------------------------------------------
  // FX fixtures. Two matrices with no currency in common, and three rates of which two cross and
  // two share nothing.

  private lazy val gbpUsdMatrix: FxMatrix = FxMatrix.of(Currency.GBP, Currency.USD, 1.6d)

  private lazy val eurChfMatrix: FxMatrix = FxMatrix.of(Currency.EUR, Currency.CHF, 1.1d)

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

  /** The three-period schedule the definition generates, over the built-in calendars. */
  private lazy val schedule: Schedule = obtained(quarterly.createSchedule(standardRefData))

  /**
   * A definition that is accepted and then generates nothing.
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
   * A definition whose two dates adjust onto one another.
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
  // Numeric fixtures. Two arrays of different sizes and two matrices of different shapes, so that
  // an element-wise member can be handed a mismatched operand.

  private lazy val threeValues: DoubleArray = DoubleArray.of(1d, 2d, 3d)

  private lazy val twoValues: DoubleArray = DoubleArray.of(1d, 2d)

  private lazy val squareMatrix: DoubleMatrix = DoubleMatrix.of(2, 2, 1d, 2d, 3d, 4d)

  private lazy val flatMatrix: DoubleMatrix = DoubleMatrix.of(1, 2, 1d, 2d)

  /**
   * A calendar holding two holidays of 2014 and a Saturday/Sunday weekend.
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

  /**
   * A composite calendar reading through exactly as many calendars as the family allows.
   *
   * It is built by nesting combinations directly rather than through `combinedWith`, which
   * would collapse a combination of a calendar with itself; nothing about the parts matters
   * beyond their number. Combining or linking anything with this one exceeds the limit by one,
   * which is what the two composition rows below assert. Its identifier is never asked for, so
   * nesting this deeply costs nothing beyond the objects themselves.
   */
  private lazy val deepestCalendar: HolidayCalendar =
    (1 to HolidayCalendar.MaxCompositeDepth)
      .foldLeft(HolidayCalendars.SAT_SUN)((inner, _) => HolidayCalendar.Combined(inner, HolidayCalendars.THU_FRI))

  /**
   * A calendar whose parts between them close every day of the week.
   *
   * Both parts are ordinary calendars - a western weekend and a working week - so this is a
   * composite no single factory could have refused, and it is what makes the progress
   * requirement of a business-day search necessary rather than merely defensive.
   */
  private lazy val alwaysClosedCalendar: HolidayCalendar =
    HolidayCalendars.SAT_SUN.combinedWith(
      ImmutableHolidayCalendar.of(
        HolidayCalendarId.of("TestFailableSurfaceWorkingWeek"),
        List(LocalDate.of(2014, 1, 1)),
        List(
          DayOfWeek.MONDAY,
          DayOfWeek.TUESDAY,
          DayOfWeek.WEDNESDAY,
          DayOfWeek.THURSDAY,
          DayOfWeek.FRIDAY)))

  //-------------------------------------------------------------------------
  /**
   * Unwraps the outcome of a member that reports a single failure, failing the test if it failed.
   *
   * Used to build fixtures. The message is reported, so a fixture that stops being buildable says
   * why rather than only that it did.
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
  // FX and currency. Every failure of this group depends on the data supplied - a rate the
  // provider does not hold, two currencies that disagree, text that names no value, a mapped
  // amount no decimal holds - and is therefore reported rather than raised.
  //-------------------------------------------------------------------------

  test("FxRateProvider.fxRate reports a rate the provider cannot supply") {
    // the provider that refuses everything, including a currency against itself, which is what
    // makes an unintended conversion visible rather than silent
    val refused: FailureOr[Double] = FxRateProvider.noConversion().fxRate(Currency.GBP, Currency.USD)
    refused should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    val refusedIdentity: FailureOr[Double] = FxRateProvider.noConversion().fxRate(Currency.GBP, Currency.GBP)
    refusedIdentity should beFailureWith(FailureReason.CURRENCY_CONVERSION)

    val minimal: FailureOr[Double] = FxRateProvider.minimal().fxRate(Currency.GBP, Currency.USD)
    minimal should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    val identity: FailureOr[Double] = FxRateProvider.minimal().fxRate(Currency.GBP, Currency.GBP)
    identity should haveValue(1d)
  }

  test("FxRateProvider.convert reports a rate the provider cannot supply") {
    val refused: FailureOr[Double] = FxRateProvider.minimal().convert(100d, Currency.GBP, Currency.USD)
    refused should beFailureWith(FailureReason.CURRENCY_CONVERSION)

    val unchanged: FailureOr[Double] = FxRateProvider.minimal().convert(100d, Currency.GBP, Currency.GBP)
    unchanged should haveValue(100d)

    val refusedDecimal: FailureOr[Decimal] =
      FxRateProvider.minimal().convert(two, Currency.GBP, Currency.USD)
    refusedDecimal should beFailureWith(FailureReason.CURRENCY_CONVERSION)
  }

  test("FxMatrix.of reports rates that can never be placed") {
    // the pound/dollar rate places, and the euro/franc rate shares no currency with anything the
    // fold has placed by the time it is reached, so it can never be placed at all
    val unplaceable: FailureOr[FxMatrix] = FxMatrix.of(List(gbpUsdRate, eurCadRate))
    unplaceable should beFailureWith(FailureReason.CURRENCY_CONVERSION)

    val placed: FailureOr[FxMatrix] = FxMatrix.of(List(gbpUsdRate, eurUsdRate))
    placed should beSuccess
  }

  test("FxMatrix.withRate reports a pair the matrix has no currency in common with") {
    val disjoint: FailureOr[FxMatrix] =
      gbpUsdMatrix.withRate(CurrencyPair.of(Currency.EUR, Currency.CHF), 1.1d)
    disjoint should beFailureWith(FailureReason.CURRENCY_CONVERSION)

    val placed: FailureOr[FxMatrix] =
      gbpUsdMatrix.withRate(CurrencyPair.of(Currency.GBP, Currency.CHF), 1.2d)
    placed should beSuccess
  }

  test("FxMatrix.merge reports two matrices with no currency in common") {
    val disjoint: FailureOr[FxMatrix] = gbpUsdMatrix.merge(eurChfMatrix)
    disjoint should beFailureWith(FailureReason.CURRENCY_CONVERSION)

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
    // the family is closed, so a pair no published index quotes is reported rather than minted
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
    // the two members that take an amount can disagree about its currency; the scalar forms
    // carry no error channel and are asserted below them
    val mismatches: TableFor2[String, FailureOr[CurrencyAmount]] = Table(
      ("member", "outcome"),
      ("CurrencyAmount.plus(CurrencyAmount)", gbp100.plus(usd100)),
      ("CurrencyAmount.minus(CurrencyAmount)", gbp100.minus(usd100)))
    forAll(mismatches) { (member: String, outcome: FailureOr[CurrencyAmount]) =>
      withClue(s"$member: ") {
        outcome should beFailureWith(FailureReason.INVALID)
      }
    }

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
    // the rate is not applied where no conversion happens, so a rate that is not one would
    // silently scale the amount; whether the two currencies agree is a property of the arguments,
    // so the refusal is reported
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

  test("Money.mapAmount and BigMoney.mapAmount report a result no decimal holds") {
    // the function is on `BigDecimal`, whose precision is unbounded, so a mapper can return a
    // number outside the range a decimal holds - a property of the function and its argument, so
    // reported. The sibling `map` works on the decimal itself and is total.
    val oversized: TableFor2[String, FailureOr[Any]] = Table(
      ("member", "outcome"),
      ("Money.mapAmount", gbpMoney.mapAmount(amount => amount.multiply(new BigDecimal("1E+30")))),
      ("BigMoney.mapAmount", gbpBigMoney.mapAmount(amount => amount.multiply(new BigDecimal("1E+30")))))
    forAll(oversized) { (member: String, outcome: FailureOr[Any]) =>
      withClue(s"$member: ") {
        outcome should beFailureWith(FailureReason.INVALID)
      }
    }

    gbpMoney.mapAmount(amount => amount.multiply(new BigDecimal("2"))) should beSuccess
    gbpBigMoney.mapAmount(amount => amount.multiply(new BigDecimal("2"))) should beSuccess
  }

  test("CurrencyAmount.toMoney and CurrencyAmount.toBigMoney report an amount no decimal holds") {
    // Four rows: each conversion refuses the two classes of amount this type admits and no decimal
    // holds. An amount rejects only a value that is not a number, so an infinity is a value of it,
    // and a finite amount may need more than the eighteen digits a decimal carries. Both are
    // properties of the value converted, so both are reported.
    val unholdable: TableFor2[String, FailureOr[Any]] = Table(
      ("member", "outcome"),
      ("CurrencyAmount.toMoney, infinite", infiniteAmount.toMoney),
      ("CurrencyAmount.toBigMoney, infinite", infiniteAmount.toBigMoney),
      ("CurrencyAmount.toMoney, beyond eighteen digits", oversizedAmount.toMoney),
      ("CurrencyAmount.toBigMoney, beyond eighteen digits", oversizedAmount.toBigMoney))
    forAll(unholdable) { (member: String, outcome: FailureOr[Any]) =>
      withClue(s"$member: ") {
        outcome should beFailureWith(FailureReason.INVALID)
      }
    }

    infiniteAmount.amount.isInfinite shouldBe true
    gbp100.toMoney should haveValue(gbpMoney)
    gbp100.toBigMoney should haveValue(gbpBigMoney)
  }

  test("CurrencyAmountArray arithmetic reports size and currency mismatches") {
    // Six rows: the two element-wise members can disagree about either the size or the currency,
    // and the two scalar members only about the currency.
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
    // The two run-valued members disagree about size and report it; the two amount-valued members
    // keep the same channel but cannot fill it - an amount describes every index of the run by
    // construction - so they are asserted on the side they can answer.
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

    gbpMulti.getAmountOrZero(Currency.AUD).amount shouldBe 0d
  }

  test("CurrencyPair.other reports a currency that is not in the pair") {
    val pair: CurrencyPair = CurrencyPair.of(Currency.GBP, Currency.USD)
    val absent: FailureOr[Currency] = pair.other(Currency.EUR)
    absent should beFailureWith(FailureReason.INVALID)

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
    // the family is closed, so a code it does not hold is reported rather than minted
    val unknown: FailureOr[Currency] = Currency.parse("XYZ")
    unknown should beFailureWith(FailureReason.PARSING)

    Currency.parse("GBP") should haveValue(Currency.GBP)
    Currency.parse("gbp") should haveValue(Currency.GBP)
    val tooLong: FailureOr[Currency] = Currency.of("GBPX")
    tooLong should beFailure
  }


  //-------------------------------------------------------------------------
  // Location. `Country` is an open validated value rather than a closed family, so a well-formed
  // code names a country whether or not the reference data knows it; only the three letter lookup,
  // which has to be translated through that data, can fail on a well-formed input.
  //-------------------------------------------------------------------------

  test("Country.of reports a malformed code and accepts any well-formed one") {
    val malformed: ResultNec[Country] = Country.of("abc")
    malformed should beFailureWith(FailureReason.INVALID)
    val tooShort: ResultNec[Country] = Country.of("G")
    tooShort should beFailureWith(FailureReason.INVALID)

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
  // Schedule and frequency. A schedule definition is judged against the calendar it is rolled out
  // over, so almost everything here is decided by data rather than by the call.
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

    // a frequency of days does not divide a frequency of months at all, the two being
    // incommensurate
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

    quarterly.startDate shouldBe JAN_15
    quarterly.endDate shouldBe OCT_15
  }

  test("PeriodicSchedule.createSchedule reports a definition that generates nothing") {
    // the rejected definition is carried under the `definition` attribute, so a report can name
    // it without parsing the message
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

    // one pair of dates fills both roles, so a bad pair is reported under both pairs of names
    failuresOf(reversed) should have size 2

    SchedulePeriod.of(JAN_15, APR_15) should beSuccess
  }

  test("Schedule.of reports periods that do not run from earliest to latest") {
    // The one invariant the field's type does not carry: `NonEmptyList` states that there is a
    // period, this factory that the periods form a time line. A list that does not is data, and
    // every member reading the periods in order would otherwise answer wrongly rather than fail.
    val reversed: ResultNec[Schedule] =
      Schedule.of(
        NonEmptyList.of(schedule.period(1), schedule.period(0)),
        Frequency.P3M,
        RollConventions.DAY_15)
    reversed should beFailureWith(FailureReason.INVALID)

    // both date pairs of one misplacement are reported - the unadjusted time line the schedule
    // was generated on and the adjusted one its dates fall on
    val failures: List[Failure] = failuresOf(reversed)
    failures should have size 2
    failures.map(_.message).count(_.contains("the unadjusted end date")) shouldBe 1
    failures.map(_.message).count(_.contains("the adjusted end date")) shouldBe 1
    failures.foreach { failure =>
      failure.message should include("the periods must run from earliest to latest")
      failure.message should include("of the period at index 0 is after")
      failure.message should include("of the period at index 1")
    }

    // one failure per misplaced pair, so a list that is wholly reversed reports every one of them
    val whollyReversed: ResultNec[Schedule] =
      Schedule.of(
        NonEmptyList.of(schedule.period(2), schedule.period(1), schedule.period(0)),
        Frequency.P3M,
        RollConventions.DAY_15)
    failuresOf(whollyReversed) should have size 4

    // adjacency is what a generated schedule has and is accepted; a gap between two periods is
    // accepted too, since a schedule may describe accrual that pauses
    Schedule.of(schedule.periods, Frequency.P3M, RollConventions.DAY_15) should beSuccess
    Schedule.of(
      NonEmptyList.of(schedule.period(0), schedule.period(2)),
      Frequency.P3M,
      RollConventions.DAY_15) should beSuccess
    Schedule.of(
      NonEmptyList.one(schedule.firstPeriod),
      Frequency.P3M,
      RollConventions.DAY_15) should beSuccess
  }

  test("Schedule.merge reports a date that matches no period of the schedule") {
    // the first regular start date names no boundary of this schedule, which is data about the
    // schedule rather than a breach of the call: the two dates it is given are in order
    val unmatched: FailureOr[Schedule] = schedule.merge(3, LocalDate.of(2014, 2, 1), OCT_15)
    unmatched should beFailureWith(FailureReason.INVALID)

    val ungrouped: FailureOr[Schedule] = schedule.merge(2, JAN_15, OCT_15)
    ungrouped should beFailureWith(FailureReason.INVALID)

    schedule.merge(3, JAN_15, OCT_15) should beSuccess
    // `merge` also carries a dates-in-order `ArgCheck` precondition, which the registry's
    // `Schedule.merge(dates out of order)` row raises
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

    schedule.toAdjusted(DateAdjuster(date => date)) should haveValue(schedule)
  }

  test("RollConvention.ofDayOfMonth reports a day outside one to thirty-one") {
    val zero: FailureOr[RollConvention] = RollConvention.ofDayOfMonth(0)
    zero should beFailureWith(FailureReason.INVALID)
    val beyond: FailureOr[RollConvention] = RollConvention.ofDayOfMonth(32)
    beyond should beFailureWith(FailureReason.INVALID)

    RollConvention.ofDayOfMonth(31) should haveValue(RollConventions.EOM)
    RollConvention.ofDayOfMonth(15) should haveValue(RollConventions.DAY_15)
  }

  test("RollConvention.ofDayOfWeek is total for every day of the week") {
    // The argument is an enumerated day rather than a number, so there is no out-of-range input
    // to report and the member is total: seven days, seven conventions, no error channel.
    RollConvention.ofDayOfWeek(DayOfWeek.MONDAY) shouldBe RollConventions.DAY_MON
    RollConvention.ofDayOfWeek(DayOfWeek.TUESDAY) shouldBe RollConventions.DAY_TUE
    RollConvention.ofDayOfWeek(DayOfWeek.WEDNESDAY) shouldBe RollConventions.DAY_WED
    RollConvention.ofDayOfWeek(DayOfWeek.THURSDAY) shouldBe RollConventions.DAY_THU
    RollConvention.ofDayOfWeek(DayOfWeek.FRIDAY) shouldBe RollConventions.DAY_FRI
    RollConvention.ofDayOfWeek(DayOfWeek.SATURDAY) shouldBe RollConventions.DAY_SAT
    RollConvention.ofDayOfWeek(DayOfWeek.SUNDAY) shouldBe RollConventions.DAY_SUN
  }

  //-------------------------------------------------------------------------
  // Dates and adjustments. Every adjustment resolves its calendar against the reference data it
  // is handed, so its failure is the absence of that calendar - data, not a call.
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
    // empty text is refused before any attempt to parse it
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

  test("DaysAdjustment.of reports a business day addition of no days") {
    // The one condition the three fields can break together: the addition calendar decides whether
    // the days are calendar or business days, so zero days over any calendar but the no-holidays
    // identifier asks for a business-day addition of no day at all. It depends on two of the
    // caller's values, so it is reported.
    val zeroDaysOverCalendar: ResultNec[DaysAdjustment] =
      DaysAdjustment.of(0, HolidayCalendarIds.GBLO, BusinessDayAdjustment.NONE)
    zeroDaysOverCalendar should beFailureWith(FailureReason.INVALID)
    failuresOf(zeroDaysOverCalendar) should have size 1
    failureOf(DaysAdjustment.of(0, HolidayCalendarIds.GBLO, BusinessDayAdjustment.NONE).left.map(
      _.head)).message should include(
      "A business day addition of zero days names no day, so 'calendar' must be 'NoHolidays' " +
        "when 'days' is zero but was 'GBLO'")

    // everything else about the three fields is accepted: the count may be negative, and zero days
    // over the no-holidays identifier is the identity adjustment
    DaysAdjustment.of(-2, HolidayCalendarIds.GBLO, BusinessDayAdjustment.NONE) should beSuccess
    DaysAdjustment.of(2, HolidayCalendarIds.GBLO, BusinessDayAdjustment.NONE) should beSuccess
    DaysAdjustment.of(0, HolidayCalendarIds.NO_HOLIDAYS, BusinessDayAdjustment.NONE) should
      haveValue(DaysAdjustment.NONE)

    // the four named factories stay total, and each of them lands inside the accepted field space,
    // which is what makes the validated factory the only place the pairing is judged
    DaysAdjustment.ofCalendarDays(0) shouldBe DaysAdjustment.NONE
    DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.GBLO).calendar shouldBe
      HolidayCalendarIds.NO_HOLIDAYS
  }

  test("DaysAdjustment resolution reports a calendar the reference data does not hold") {
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
    val unresolved: FailureOr[DayCount] = DayCount.ofBus252(HolidayCalendarIds.BRBD, emptyRefData)
    unresolved should beFailureWith(FailureReason.MISSING_DATA)

    val resolved: FailureOr[DayCount] = DayCount.ofBus252(HolidayCalendarIds.BRBD, standardRefData)
    resolved should beSuccess
    resolved.map(dayCount => dayCount.name) should haveValue("Bus/252 BRBD")
  }


  //-------------------------------------------------------------------------
  // Reference data. An identifier the data does not hold is the absence of data rather than a
  // broken call, so it is reported rather than raised.
  //-------------------------------------------------------------------------

  test("ReferenceDataId.resolve reports an identifier the reference data does not hold") {
    val absent: TestingReferenceDataId = TestingReferenceDataId("missing")
    val unresolved: FailureOr[java.lang.Number] = absent.resolve(emptyRefData)
    unresolved should beFailureWith(FailureReason.MISSING_DATA)
    failureOf(unresolved).attributes.get("id") shouldBe Some(absent.toString)

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
  // Identifiers. Every failure here is a judgement about text a caller supplied.
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
  // Values. A value definition is judged against the schedule it is resolved against, so the
  // factories are the cheap half and resolution is where the data is judged.
  //-------------------------------------------------------------------------

  test("CurrencyAmountArray.of reports an empty collection and mixed currencies") {
    val empty: ResultNec[CurrencyAmountArray] = CurrencyAmountArray.of(List.empty[CurrencyAmount])
    empty should beFailureWith(FailureReason.INVALID)
    val mixed: ResultNec[CurrencyAmountArray] = CurrencyAmountArray.of(List(gbp100, usd100))
    mixed should beFailureWith(FailureReason.INVALID)

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

    val neither: ResultNec[ValueStep] = ValueStep.of(None, None, ValueAdjustment.ofReplace(200d))
    neither should beFailureWith(FailureReason.INVALID)
    val both: ResultNec[ValueStep] =
      ValueStep.of(Some(1), Some(APR_15), ValueAdjustment.ofReplace(200d))
    both should beFailureWith(FailureReason.INVALID)

    ValueStep.of(1, ValueAdjustment.ofReplace(200d)) should beSuccess
    ValueStep.of(APR_15, ValueAdjustment.ofReplace(200d)).value shouldBe ValueAdjustment.ofReplace(200d)
  }

  test("ValueSchedule.of reports two steps that name one position with different adjustments") {
    // The half of the contradiction that needs no schedule: two steps at the same position - the
    // same period index, or the same date - with different adjustments ask for two values at one
    // point of the time line whatever schedule resolves them. One failure per such position; the
    // rest of a definition is judged by `resolveValues` below.
    val replace200: ValueAdjustment = ValueAdjustment.ofReplace(200d)
    val replace300: ValueAdjustment = ValueAdjustment.ofReplace(300d)
    val byIndex: ResultNec[ValueSchedule] =
      ValueSchedule.of(
        100d,
        List(accepted(ValueStep.of(1, replace200)), accepted(ValueStep.of(1, replace300))))
    byIndex should beFailureWith(FailureReason.INVALID)
    val byDate: ResultNec[ValueSchedule] =
      ValueSchedule.of(
        100d,
        List(ValueStep.of(JAN_15, replace200), ValueStep.of(JAN_15, replace300)))
    byDate should beFailureWith(FailureReason.INVALID)
    // two contradicted positions are two failures, so a caller correcting one is told about both
    val twoPositions: ResultNec[ValueSchedule] =
      ValueSchedule.of(
        100d,
        List(
          accepted(ValueStep.of(1, replace200)),
          accepted(ValueStep.of(1, replace300)),
          ValueStep.of(JAN_15, replace200),
          ValueStep.of(JAN_15, replace300)))
    failuresOf(twoPositions) should have size 2

    // the same position twice with the same adjustment is no contradiction, and is accepted
    val step: ValueStep = accepted(ValueStep.of(1, replace200))
    accepted(ValueSchedule.of(100d)).initialValue shouldBe 100d
    accepted(ValueSchedule.of(100d, List(step, step))).steps shouldBe List(step, step)
    accepted(ValueSchedule.of(100d, step)).steps shouldBe List(step)
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
      accepted(ValueSchedule.of(100d, accepted(ValueStep.of(5, ValueAdjustment.ofReplace(200d)))))
    val rejected: FailureOr[DoubleArray] = beyond.resolveValues(schedule)
    rejected should beFailureWith(FailureReason.INVALID)

    // the pair only the schedule can see: one step names the period by its index and the other by
    // the date of its boundary, so the positions differ until the periods are in hand. A pair
    // naming one position twice is refused by construction instead.
    val contradictory: ValueSchedule =
      accepted(
        ValueSchedule.of(
          100d,
          List(
            accepted(ValueStep.of(1, ValueAdjustment.ofReplace(200d))),
            ValueStep.of(schedule.period(1).unadjustedStartDate, ValueAdjustment.ofReplace(300d)))))
    contradictory.resolveValues(schedule) should beFailureWith(FailureReason.INVALID)

    val resolvable: ValueSchedule =
      accepted(ValueSchedule.of(100d, accepted(ValueStep.of(1, ValueAdjustment.ofReplace(200d)))))
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
  // Collect members reached from this module. They belong to `strata-collect` and are asserted
  // here because `strata-basics` is where they are consumed.
  //-------------------------------------------------------------------------

  test("NamedEnum.parse reports a name no member of the family carries") {
    val unknown: ResultNec[Currency] = NamedEnum[Currency].parse("NotACurrency")
    unknown should beFailureWith(FailureReason.PARSING)

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
  // Index observations and floating rates. An observation computes dates from the calendars of
  // its index, so it is built only through a factory that resolves them - either per fixing,
  // through `of`, or once for a series of fixings, through the `resolve` of the index and of the
  // observation type, which report the same failure at the point of resolution.
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

  test("IborIndex.resolve reports a calendar the reference data does not hold") {
    // The batch route reports the missing calendar once, at the point of resolution, rather than
    // per fixing: there is no function to apply, so the failure cannot be deferred. Both published
    // entry points are exercised, the one on the index and the one on the observation type.
    val unresolvedIndex: FailureOr[java.time.LocalDate => IborIndexObservation] =
      IborIndices.GBP_LIBOR_3M.resolve(emptyRefData)
    unresolvedIndex should beFailureWith(FailureReason.MISSING_DATA)
    val unresolvedType: FailureOr[java.time.LocalDate => IborIndexObservation] =
      IborIndexObservation.resolve(IborIndices.GBP_LIBOR_3M, emptyRefData)
    unresolvedType should beFailureWith(FailureReason.MISSING_DATA)

    IborIndices.GBP_LIBOR_3M.resolve(standardRefData) should beSuccess
    IborIndexObservation.resolve(IborIndices.GBP_LIBOR_3M, standardRefData) should beSuccess
  }

  test("FxIndex.resolve reports a calendar the reference data does not hold") {
    val unresolvedIndex: FailureOr[java.time.LocalDate => FxIndexObservation] =
      FxIndices.EUR_USD_ECB.resolve(emptyRefData)
    unresolvedIndex should beFailureWith(FailureReason.MISSING_DATA)
    val unresolvedType: FailureOr[java.time.LocalDate => FxIndexObservation] =
      FxIndexObservation.resolve(FxIndices.EUR_USD_ECB, emptyRefData)
    unresolvedType should beFailureWith(FailureReason.MISSING_DATA)

    FxIndices.EUR_USD_ECB.resolve(standardRefData) should beSuccess
    FxIndexObservation.resolve(FxIndices.EUR_USD_ECB, standardRefData) should beSuccess
  }

  test("OvernightIndexObservation.resolve reports a calendar the reference data does not hold") {
    // The Overnight family publishes its batch resolution on the observation type alone - only
    // the Ibor and exchange-rate indices declare `resolve` - and reports the same failure as its
    // per-fixing factory.
    val unresolved: FailureOr[java.time.LocalDate => OvernightIndexObservation] =
      OvernightIndexObservation.resolve(OvernightIndices.GBP_SONIA, emptyRefData)
    unresolved should beFailureWith(FailureReason.MISSING_DATA)

    OvernightIndexObservation.resolve(OvernightIndices.GBP_SONIA, standardRefData) should beSuccess
  }

  test("FloatingRateName.toIborIndex reports a name of the wrong kind and a tenor no index carries") {
    // an Overnight name is not an Ibor name, which is a property of the name rather than of the
    // call
    val wrongKind: FailureOr[Any] = FloatingRateNames.GBP_SONIA.toIborIndex(Tenor.TENOR_3M)
    wrongKind should beFailureWith(FailureReason.INVALID)

    // and the family publishes no three-week index, which is a property of the reference data
    val unpublished: FailureOr[Any] = FloatingRateNames.GBP_LIBOR.toIborIndex(Tenor.TENOR_3W)
    unpublished should beFailureWith(FailureReason.PARSING)

    FloatingRateNames.GBP_LIBOR.toIborIndex(Tenor.TENOR_3M) should haveValue(IborIndices.GBP_LIBOR_3M)
    // a tenor of one year normalises onto twelve months
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
    // the Brazilian real, whose market has no term rate, publishes no default Ibor index
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
  // The other side of the classification line: twelve contract rows, nine of them refusals the
  // method raises and three of them totality contracts - a member that deliberately does not
  // refuse, asserted on the side it answers on. The classification rule is that a refusal guards
  // a property of how the method was called rather than of the data it was given, so a caller
  // cannot correct it by supplying better data; it has to call differently. `ArgCheck` is the one
  // place either module writes a throw, and what it writes is `IllegalArgumentException`; the two
  // index refusals below let the array access itself raise, which is `IndexOutOfBoundsException`.
  //
  // `SmartConstructorSpec` asserts the messages of the two numeric-edge throws of the same
  // inventory; the generated `Raises` rows below assert that those families raise at all.
  //-------------------------------------------------------------------------

  test("DoubleArray.get raises for an index outside the array") {
    // Contract, not data: an index is the caller's own arithmetic over a size it can read. The
    // array access itself raises, so the exception is the JVM's and not one this library composes.
    intercept[IndexOutOfBoundsException](threeValues.get(3))
    intercept[IndexOutOfBoundsException](threeValues.get(-1))

    threeValues.get(0) shouldBe 1d
    threeValues.get(2) shouldBe 3d
  }

  test("DoubleArray element-wise arithmetic raises for arrays of different sizes") {
    // Contract, not data: two arrays of different lengths have no element-wise sum, and the
    // caller holds both sizes before it calls. Refusing fail-fast keeps the inner loops total.
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
    // so a date whose year lies outside 0 to 9999 is one no calendar could hold data for - a
    // property of where the argument falls on the time line, not of the holidays supplied.
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

  test("HolidayCalendar refuses a shift larger than any search can satisfy") {
    // Contract, not data: shifting by business days walks them one at a time, so the cost of the
    // call is the number asked for - and the number is `Int`-wide, which makes two thousand
    // million business days a request that occupies a processor for the best part of a minute to
    // name a date six million years away. No convention of finance names such a shift, and the
    // caller holds the count before it calls, so the limit is a precondition and is refused
    // rather than reported. Its magnitude is measured in `Long` arithmetic, which is what makes
    // the smallest `Int` - whose negation overflows back to itself - refused as well.
    intercept[IllegalArgumentException](
      datedCalendar.shift(JAN_15, Int.MaxValue)).getMessage should include(ShiftMagnitudeMessage)
    intercept[IllegalArgumentException](
      datedCalendar.shift(JAN_15, Int.MinValue)).getMessage should include(ShiftMagnitudeMessage)
    intercept[IllegalArgumentException](
      HolidayCalendars.SAT_SUN.shift(JAN_15, HolidayCalendar.MaxBusinessDayShift + 1)).getMessage should
      include(ShiftMagnitudeMessage)
    // the adjuster form judges the count where it is named rather than when it is applied
    intercept[IllegalArgumentException](
      datedCalendar.adjustBy(Int.MinValue)).getMessage should include(ShiftMagnitudeMessage)

    // every shift within the limit answers, and the calendar of no holidays adds its days
    // arithmetically, so no limit applies to it at all
    datedCalendar.shift(JAN_15, 2) shouldBe LocalDate.of(2014, 1, 17)
    HolidayCalendars.NO_HOLIDAYS.shift(JAN_15, Int.MaxValue).getYear should be > 5000000
  }

  test("HolidayCalendar refuses a search of a calendar that has no business day") {
    // Contract, not data: a search for the next business day walks one day at a time, and a
    // calendar whose parts between them close every day of the week has none to find. Here the
    // two parts are a western weekend and a working week - both perfectly ordinary calendars - so
    // no single factory could have refused the combination, and without a progress requirement
    // the search would walk millions of days to the end of the range of dates and then report the
    // year it reached, which says nothing about what is wrong.
    intercept[IllegalArgumentException](
      alwaysClosedCalendar.nextOrSame(JAN_15)).getMessage should include(NoBusinessDayMessage)
    intercept[IllegalArgumentException](
      alwaysClosedCalendar.previousOrSame(JAN_15)).getMessage should include(NoBusinessDayMessage)
    // the refusal names the calendar and the date the search began at
    val refusal: String = intercept[IllegalArgumentException](alwaysClosedCalendar.nextOrSame(JAN_15)).getMessage
    refusal should include(JAN_15.toString)
    refusal should include(alwaysClosedCalendar.name)

    // a calendar that has business days answers as it always did
    HolidayCalendars.SAT_SUN.nextOrSame(LocalDate.of(2014, 1, 18)) shouldBe LocalDate.of(2014, 1, 20)
  }

  test("HolidayCalendar refuses a composite deeper than the family allows") {
    // Contract, not data: a composite reads a pair of calendars on every query, either of which
    // may be a composite, so deciding a date, composing the identifier, writing the calendar out
    // and reading one back all walk a tree whose height is the nesting. A tree tall enough
    // exhausts the stack, which ends the calling thread rather than the calculation, and nothing
    // about combining calendars needs the height: a payment settling in every centre this library
    // knows about reads some thirty calendars.
    intercept[IllegalArgumentException](
      deepestCalendar.combinedWith(datedCalendar)).getMessage should include(CompositeDepthMessage)
    intercept[IllegalArgumentException](
      deepestCalendar.linkedWith(datedCalendar)).getMessage should include(CompositeDepthMessage)
    // the limit cannot be gone round by building the composite directly, the judgement being part
    // of constructing one
    intercept[IllegalArgumentException](
      HolidayCalendar.Combined(deepestCalendar, datedCalendar)).getMessage should
      include(CompositeDepthMessage)
    intercept[IllegalArgumentException](
      HolidayCalendar.Linked(deepestCalendar, datedCalendar)).getMessage should
      include(CompositeDepthMessage)

    // a composite at the limit is a calendar like any other and answers for its dates
    deepestCalendar.isHoliday(LocalDate.of(2014, 1, 18)) shouldBe true
    HolidayCalendars.SAT_SUN.combinedWith(datedCalendar).isHoliday(LocalDate.of(2014, 1, 1)) shouldBe true
  }

  /**
   * A calendar whose weekend is Monday to Friday, which is a calendar: Saturday is open.
   *
   * Paired with [[weekendClosedCalendar]] below to merge two calendars that are each ordinary on
   * their own into one that would have no business day at all - the weekend of a merged calendar
   * closing every day either of its parts closes.
   */
  private lazy val weekdayClosedCalendar: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(
      HolidayCalendarId.of("TestFailableSurfaceWeekdaysClosed"),
      List(LocalDate.of(2014, 1, 1)),
      List(
        DayOfWeek.MONDAY,
        DayOfWeek.TUESDAY,
        DayOfWeek.WEDNESDAY,
        DayOfWeek.THURSDAY,
        DayOfWeek.FRIDAY))

  /** A calendar whose weekend is the ordinary one, over the same years as the calendar above. */
  private lazy val weekendClosedCalendar: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(
      HolidayCalendarId.of("TestFailableSurfaceWeekendClosed"),
      List(LocalDate.of(2014, 1, 1)),
      List(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY))

  test("ImmutableHolidayCalendar refuses a weekend that closes every day") {
    // Contract, not data: outside the years its holidays cover a calendar answers from its
    // weekend alone, so one whose weekend closes all seven days has no business day in any of
    // those years - no next, no previous, no shift and no schedule. The working days such a
    // calendar might declare do not make it one either, naming individual dates inside the years
    // the holidays cover rather than opening any day of the week. The library being ported built
    // the calendar and let every search on it run to the end of the range of dates instead.
    intercept[IllegalArgumentException](
      ImmutableHolidayCalendar.of(
        HolidayCalendarId.of("TestFailableSurfaceEveryDayClosed"),
        List(LocalDate.of(2014, 1, 1)),
        DayOfWeek.values().toList)).getMessage should include(NoBusinessDayMessage)
    intercept[IllegalArgumentException](
      ImmutableHolidayCalendar.of(
        HolidayCalendarId.of("TestFailableSurfaceEveryDayClosedWorking"),
        List(LocalDate.of(2014, 1, 1)),
        DayOfWeek.values().toList,
        List(LocalDate.of(2014, 1, 2)))).getMessage should include(NoBusinessDayMessage)

    // six days closed is a calendar, however unusual, and answers for the seventh
    val sixDaysClosed: ImmutableHolidayCalendar = ImmutableHolidayCalendar.of(
      HolidayCalendarId.of("TestFailableSurfaceSixDaysClosed"),
      List(LocalDate.of(2014, 1, 1)),
      DayOfWeek.values().toList.filterNot(day => day == DayOfWeek.WEDNESDAY))
    sixDaysClosed.isBusinessDay(LocalDate.of(2014, 1, 8)) shouldBe true

    // The same refusal reaches the eager merge, which is the other way such a calendar could
    // come about: the merged weekend closes every day either part closes, so two calendars that
    // are each ordinary alone - a Monday-to-Friday weekend and a Saturday/Sunday one - would
    // merge into a calendar with no business day. The lazy composite holds its parts apart
    // instead and is built; a search of it is what reports the pair has none.
    intercept[IllegalArgumentException](
      ImmutableHolidayCalendar.combined(weekdayClosedCalendar, weekendClosedCalendar))
      .getMessage should include(NoBusinessDayMessage)
    intercept[IllegalArgumentException](
      ImmutableHolidayCalendar.combined(weekendClosedCalendar, weekdayClosedCalendar))
      .getMessage should include(NoBusinessDayMessage)
    weekdayClosedCalendar.combinedWith(weekendClosedCalendar).name shouldBe
      "TestFailableSurfaceWeekdaysClosed+TestFailableSurfaceWeekendClosed"

    // ... and merging two calendars that between them leave a day open is unaffected
    ImmutableHolidayCalendar
      .combined(weekendClosedCalendar, datedCalendar)
      .isBusinessDay(LocalDate.of(2014, 1, 8)) shouldBe true
  }

  test("HolidayCalendarId.resolve reports a name joining more calendars than one can read") {
    // Data, not contract, and the counterpart of the three refusals above: `of` accepts any name,
    // so how many calendars a composite identifier joins is decided by whatever text reached it -
    // and resolving such a name would build a calendar as deep as the name is wide. Resolution
    // answers with a failure everywhere else, so it answers with one here too rather than raising
    // the refusal the construction of the calendar would have raised.
    val tooWide: FailureOr[HolidayCalendar] = overWideCompositeId.resolve(standardRefData)
    tooWide should beFailureWith(FailureReason.INVALID)
    failureOf(tooWide).attributes.get("components") shouldBe
      Some((HolidayCalendar.MaxCompositeDepth + 1).toString)
    failureOf(tooWide).message should include(HolidayCalendar.MaxCompositeDepth.toString)

    // a composite of ordinary width resolves exactly as it always did
    HolidayCalendarIds.GBLO.combinedWith(HolidayCalendarIds.USNY).resolve(standardRefData) should beSuccess
  }

  test("DaysAdjustment.of reports a business day addition no calendar can walk") {
    // Data, not contract, because the day count of an adjustment is data: it is read from a
    // document, held in a convention and passed around, so a count no calendar can walk arrives
    // from outside the program and is reported rather than raised. The same count handed to the
    // factory that names a kind of addition is refused where it is named, which is what keeps
    // every adjustment a factory builds one this factory accepts.
    val tooManyDays: ResultNec[DaysAdjustment] =
      DaysAdjustment.of(Int.MaxValue, HolidayCalendarIds.GBLO, BusinessDayAdjustment.NONE)
    tooManyDays should beFailureWith(FailureReason.INVALID)
    failuresOf(tooManyDays) should have size 1
    failureOf(tooManyDays.left.map(_.head)).message should include(ShiftMagnitudeMessage)
    intercept[IllegalArgumentException](
      DaysAdjustment.ofBusinessDays(Int.MaxValue, HolidayCalendarIds.GBLO)).getMessage should
      include(ShiftMagnitudeMessage)
    intercept[IllegalArgumentException](
      DaysAdjustment.ofBusinessDays(
        Int.MinValue,
        HolidayCalendarIds.GBLO,
        BusinessDayAdjustment.NONE)).getMessage should include(ShiftMagnitudeMessage)

    // a calendar-day addition adds its days in one step whatever their number, so the limit does
    // not apply to it, and an addition within the limit is accepted as it always was
    DaysAdjustment.of(Int.MaxValue, HolidayCalendarIds.NO_HOLIDAYS, BusinessDayAdjustment.NONE) should
      beSuccess
    DaysAdjustment.of(
      HolidayCalendar.MaxBusinessDayShift,
      HolidayCalendarIds.GBLO,
      BusinessDayAdjustment.NONE) should beSuccess
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
    // Contract, not data: this convention's rule is defined in terms of the end of the schedule,
    // the end of the period containing the first date and the frequency, so a schedule that cannot
    // supply them is a caller asking a question it has no rule for - refused even though the
    // accessors themselves answer `None`.
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
    // Contract, not data, and the narrowest of the four: the end of the schedule is read only to
    // decide whether a second date falling on the last day of February is the schedule's own end,
    // so only a pair reaching that branch without a schedule end is refused.
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
    // single accessor of `ScheduleInfo` that is not optional - it defaults to true - so the fact
    // this rule reads is always there. That asymmetry is why it has a row of its own.
    val simple: DayCount.ScheduleInfo = DayCount.ScheduleInfo.simple
    simple.isEndOfMonthConvention shouldBe true
    DayCounts.THIRTY_U_360.yearFraction(JAN_15, APR_15, simple) shouldBe (90d / 360d)

    // and a schedule that turns the flag off is answered by the other rule, not refused
    val withoutEom: DayCount.ScheduleInfo = new DayCount.ScheduleInfo {
      override def isEndOfMonthConvention: Boolean = false
    }
    // a leap-year February month-end to the next: with the flag set both dates move to the
    // thirtieth and the period is a whole year; with it clear the 29th and the 28th stay where
    // they are and the period is a day short. The difference is the reading, not a refusal.
    val endOfFebruary: LocalDate = LocalDate.of(2012, 2, 29)
    val eomResult: Double = DayCounts.THIRTY_U_360.yearFraction(endOfFebruary, LocalDate.of(2013, 2, 28), simple)
    val plainResult: Double =
      DayCounts.THIRTY_U_360.yearFraction(endOfFebruary, LocalDate.of(2013, 2, 28), withoutEom)
    eomResult shouldBe (360d / 360d)
    plainResult shouldBe (359d / 360d)
  }

  test("Schedule.periodEndDate answers None for a date outside every period") {
    // A date outside every period is data rather than a broken call - a caller may hold a date
    // from anywhere - so this accessor answers `None`, asserted as a `None` and never as a throw.
    // The conventions that read it refuse on their own behalf, which is the row above.
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

  //=========================================================================
  // THE DERIVED ENUMERATION
  //
  // Everything above is a hand-written test of one named member. Everything below reads the
  // sources of both modules while the suite is being built, taking two enumerations, one per side
  // of the classification line:
  //
  //   - every public `def` whose '''declared return type''' is a failure channel - `Either`,
  //     `EitherNec`, `ResultNec`, `FailureOr`, `ValidatedNec`, `ValidatedFailures`, `Validated`
  //     or `ValueWithFailures` - which is the surface that reports failure as a value;
  //   - every public `def` whose scaladoc carries a `@throws` tag, which is the surface that
  //     refuses by raising.
  //
  // The reading is textual and deliberately conservative, which is what makes the enumeration
  // honest about its own limits: it skips `private` and `protected` declarations, every member of
  // a non-visible owner and every `def` nested inside another `def`, and it reads a return type
  // only where the declaration states one. So a member returning a channel it does not name - a
  // function '''producing''' a validation, as `TypedStringCompanion.matchingPattern` does - is not
  // enumerated, and neither is a failure reached through a type parameter with no channel in the
  // signature. The sizes of both enumerations are printed by `derived_inventory_counts`.
  //=========================================================================

  /**
   * The main source roots of the two modules, relative to the repository root.
   *
   * The test JVM is forked with the build root as its working directory, so both paths resolve as
   * written. [[scalaSourcesOf]] refuses a root that is not a directory rather than enumerating
   * nothing, which would make every coverage assertion here pass for the wrong reason.
   */
  private val SourceRoots: List[String] =
    List("strata-collect/src/main/scala", "strata-basics/src/main/scala")

  /**
   * The number of lines a declaration's signature is read across.
   *
   * Twelve covers every declaration of both modules: the longest is the eleven-parameter factory
   * of `PeriodicSchedule`, whose return type stands on the twelfth line. The joined text is
   * truncated at the body, so reading past the end of a signature is harmless.
   */
  private val SignatureLines: Int = 12

  private val FailureChannelPattern: Regex =
    """^(?:Either|EitherNec|ResultNec|FailureOr|ValidatedFailures|ValidatedNec|Validated|ValueWithFailures)\[""".r

  /** A class, trait, object or package object declaration, with its modifiers and its name. */
  private val OwnerPattern: Regex =
    """^(\s*)((?:(?:final|sealed|abstract|case|implicit|private|protected)(?:\[[A-Za-z]+\])?\s+)*)(?:package\s+object|class|trait|object)\s+([A-Za-z_][A-Za-z0-9_]*)""".r

  /** A method declaration, with its modifiers and its name. */
  private val DefinitionPattern: Regex =
    """^(\s*)((?:(?:final|override|implicit|lazy|private|protected)(?:\[[A-Za-z]+\])?\s+)*)def\s+([A-Za-z_][A-Za-z0-9_]*)""".r


  /**
   * Lists the Scala sources under one source root, refusing a root that is not a directory.
   *
   * @param root  the source root, relative to the repository root
   * @return every Scala source under it, ordered by path
   */
  private def scalaSourcesOf(root: String): List[File] = {
    val directory: File = new File(root)
    if (!directory.isDirectory) {
      sys.error(
        s"The failable-surface enumeration reads '$root' relative to the working directory " +
          s"'${new File(".").getAbsolutePath}', which holds no such directory. The forked test " +
          "JVM is expected to run at the build root; anywhere else the enumeration is empty and " +
          "every coverage assertion of this suite would pass for the wrong reason.")
    } else {
      filesUnder(directory).filter(_.getName.endsWith(".scala")).sortBy(_.getPath)
    }
  }

  /**
   * Lists every file under a directory, however deeply nested.
   *
   * @param directory  the directory to read
   * @return every file below it, directories excluded
   */
  private def filesUnder(directory: File): List[File] =
    Option(directory.listFiles()).map(_.toList).getOrElse(Nil).flatMap(entry =>
      if (entry.isDirectory) filesUnder(entry) else List(entry))

  /**
   * Reads the lines of a source file.
   *
   * @param file  the file to read
   * @return its lines, in order
   */
  private def linesOf(file: File): Vector[String] = {
    val source: Source = Source.fromFile(file)(Codec.UTF8)
    try source.getLines().toVector
    finally source.close()
  }

  /**
   * Answers whether the `=` at a position is part of an operator rather than the body's.
   *
   * `==`, `=>`, `!=`, `<=` and `>=` each hold an `=` that opens no body, and a signature truncated
   * at one of them would lose its return type.
   *
   * @param text  the text being read
   * @param position  the position of the `=`
   * @return true where the `=` belongs to an operator
   */
  private def isOperatorEquals(text: String, position: Int): Boolean = {
    val next: Char = if (position + 1 < text.length) text.charAt(position + 1) else ' '
    val previous: Char = if (position > 0) text.charAt(position - 1) else ' '
    next == '=' || next == '>' || "=!<>".contains(previous)
  }

  /**
   * Truncates joined signature text at the point its body begins.
   *
   * The body begins at the first `=` or `{` outside every bracket, so a default argument, a
   * function type and a comparison inside a type argument are passed over. A declaration with no
   * body is left as it stands, so the caller reads the return type as a '''prefix'''.
   *
   * @param text  the joined lines of a declaration
   * @return the text up to the body
   */
  private def withoutBody(text: String): String = {
    @tailrec
    def bodyAt(position: Int, depth: Int): Int =
      if (position >= text.length) {
        text.length
      } else {
        val character: Char = text.charAt(position)
        if (character == '(' || character == '[') bodyAt(position + 1, depth + 1)
        else if (character == ')' || character == ']') bodyAt(position + 1, depth - 1)
        else if (depth == 0 && character == '{') position
        else if (depth == 0 && character == '=' && !isOperatorEquals(text, position)) position
        else bodyAt(position + 1, depth)
      }
    text.substring(0, bodyAt(0, 0))
  }

  /**
   * Reads the declared return type of a signature, where it states one.
   *
   * The return type follows the first `:` outside every bracket after the method's name, so the
   * type parameters, the context bounds and every parameter list are passed over. A declaration
   * whose type is inferred is answered with `None` rather than with a parameter's type.
   *
   * @param signature  the signature, already truncated at its body
   * @param method  the declared name of the method
   * @return the declared return type, or `None` where the declaration states none
   */
  private def returnTypeOf(signature: String, method: String): Option[String] = {
    val keyword: Int = signature.indexOf("def ")
    val name: Int = if (keyword < 0) -1 else signature.indexOf(method, keyword)
    if (name < 0) {
      None
    } else {
      @tailrec
      def typeAt(position: Int, depth: Int): Option[String] =
        if (position >= signature.length) {
          None
        } else {
          val character: Char = signature.charAt(position)
          if (character == '(' || character == '[') typeAt(position + 1, depth + 1)
          else if (character == ')' || character == ']') typeAt(position + 1, depth - 1)
          else if (depth == 0 && character == ':') Some(signature.substring(position + 1).trim)
          else typeAt(position + 1, depth)
        }
      typeAt(name + method.length, 0)
    }
  }

  /**
   * Reads the declared parameter types of a signature, as the identity of that one declaration.
   *
   * The section between the method's name and the depth-zero `:` [[returnTypeOf]] reads from is
   * split at the commas one bracket deep, so several parameter lists contribute to one rendering
   * and the comma inside a `Map[String, Int]` is passed over. Each parameter is reduced to its
   * declared type and whitespace is removed, so renaming a parameter, changing a default or
   * reformatting a signature across lines leaves a declaration's identity unchanged.
   *
   * @param signature  the signature, already truncated at its body
   * @param method  the declared name of the method
   * @return the parameter types, comma separated, empty where the declaration takes no parameter
   *   list at all
   */
  private def parametersOf(signature: String, method: String): String = {
    val keyword: Int = signature.indexOf("def ")
    val name: Int = if (keyword < 0) -1 else signature.indexOf(method, keyword)
    if (name < 0) {
      ""
    } else {
      val from: Int = name + method.length
      @tailrec
      def sectionEnd(position: Int, depth: Int): Int =
        if (position >= signature.length) {
          signature.length
        } else {
          val character: Char = signature.charAt(position)
          if (character == '(' || character == '[') sectionEnd(position + 1, depth + 1)
          else if (character == ')' || character == ']') sectionEnd(position + 1, depth - 1)
          else if (depth == 0 && character == ':') position
          else sectionEnd(position + 1, depth)
        }
      val section: String = signature.substring(from, sectionEnd(from, 0))
      splitAtDepth(flattenedLists(section), ',', 0)
        .map(typeOfParameter)
        .filter(_.nonEmpty)
        .mkString(",")
    }
  }

  /**
   * Flattens the parameter lists of a section into one comma-separated text.
   *
   * A top-level `(...)` is a parameter list and its contents are taken; a top-level `[...]` is the
   * type parameters and is dropped whole. Brackets nested inside a parameter are kept as they are.
   *
   * @param section  the parameter section of a signature, between the method's name and the `:`
   *   introducing its return type
   * @return the parameters of every list, comma separated
   */
  private def flattenedLists(section: String): String = {
    val (text, _, _) =
      section.foldLeft(("", 0, false)) {
        case ((out, depth, inTypes), character) =>
          if (depth == 0 && character == '[') (out, 1, true)
          else if (depth == 0 && character == '(') {
            (if (out.isEmpty) out else out + ",", 1, false)
          } else if (depth == 1 && (character == ']' || character == ')')) (out, 0, false)
          else if (depth == 0) (out, depth, inTypes)
          else {
            val moved: Int =
              if (character == '(' || character == '[') depth + 1
              else if (character == ')' || character == ']') depth - 1
              else depth
            (if (inTypes) out else out + character, moved, inTypes)
          }
      }
    text
  }

  /**
   * Reduces one declared parameter to the type it declares.
   *
   * Everything up to the parameter's own colon is its name and modifiers, and a default value is
   * dropped - the `=` that introduces one being distinguished from the `=>` of a by-name or
   * function type, which belongs to the type and stays.
   *
   * @param parameter  the parameter as it was written, with its name, its modifiers and any
   *   default value
   * @return the declared type, with every space removed
   */
  private def typeOfParameter(parameter: String): String = {
    val afterName: String = splitAtDepth(parameter, ':', 0) match {
      case _ :: rest if rest.nonEmpty => rest.mkString(":")
      case _ => parameter
    }
    @tailrec
    def defaultAt(position: Int, depth: Int): Int =
      if (position >= afterName.length) {
        afterName.length
      } else {
        val character: Char = afterName.charAt(position)
        val follows: Char = if (position + 1 < afterName.length) afterName.charAt(position + 1) else ' '
        if (character == '(' || character == '[') defaultAt(position + 1, depth + 1)
        else if (character == ')' || character == ']') defaultAt(position + 1, depth - 1)
        else if (depth == 0 && character == '=' && follows != '>') position
        else defaultAt(position + 1, depth)
      }
    afterName.substring(0, defaultAt(0, 0)).replaceAll("\\s+", "")
  }

  /**
   * Splits text at every occurrence of a separator standing at one bracket depth.
   *
   * Depth is counted over `(`, `[`, `)` and `]` from zero at the start of the text, so a separator
   * inside a nested type is passed over.
   *
   * @param text  the text to split
   * @param separator  the separating character
   * @param depth  the depth at which the separator separates
   * @return the pieces, in order, with the separators removed
   */
  private def splitAtDepth(text: String, separator: Char, depth: Int): List[String] = {
    val (pieces, last, _) =
      text.foldLeft((List.empty[String], "", 0)) {
        case ((done, current, level), character) =>
          if (character == '(' || character == '[') (done, current :+ character, level + 1)
          else if (character == ')' || character == ']') (done, current :+ character, level - 1)
          else if (character == separator && level == depth) (done :+ current, "", level)
          else (done, current :+ character, level)
      }
    (pieces :+ last).map(_.trim).filter(_.nonEmpty)
  }

  /**
   * Answers whether a declared return type is one of the failure channels of this library.
   *
   * The match is on the head of the type, so `Either[Failure, LocalDate => IborIndexObservation]`
   * and `Either[E, List[A]]` are both channels: what matters is that the caller has to handle a
   * failure to reach the value, not which type the left side names.
   *
   * @param returnType  the declared return type
   * @return true where the type is a failure channel
   */
  private def isFailureChannel(returnType: String): Boolean =
    FailureChannelPattern.findPrefixMatchOf(returnType.replaceAll("\\s+", " ").trim).isDefined

  /**
   * Reads one source file, enumerating its public failure-returning and throwing declarations.
   *
   * Indentation tracks the owners currently open and also closes them, and a `def` opened at a
   * smaller indentation makes the inner declaration local - no part of the public surface. A
   * `@throws` tag is attributed to the next declaration read after the scaladoc carrying it.
   *
   * @param file  the source file to read
   * @return the declarations it holds
   */
  private def scannedFile(file: File): ScanState = {
    val path: String = file.getPath
    val lines: Vector[String] = linesOf(file)
    lines.zipWithIndex.foldLeft(EmptyScan) {
      case (state, (line, index)) =>
        val trimmed: String = line.trim
        val opensDoc: Boolean = trimmed.startsWith("/**")
        val insideDoc: Boolean = state.inScaladoc || opensDoc
        val documentsThrow: Boolean =
          (!opensDoc && state.scaladocThrows) || (insideDoc && trimmed.contains("@throws"))
        val read: ScanState =
          state.copy(
            inScaladoc = insideDoc && !trimmed.endsWith("*/"),
            scaladocThrows = documentsThrow)
        OwnerPattern.findPrefixMatchOf(line) match {
          case Some(owner) =>
            val indent: Int = owner.group(1).length
            val outer: List[Enclosing] = read.enclosing.dropWhile(_.indent >= indent)
            val visible: Boolean =
              !owner.group(2).contains("private") && !owner.group(2).contains("protected") &&
                outer.headOption.forall(_.visible)
            read.copy(enclosing = Enclosing(indent, owner.group(3), visible) :: outer, openDefs = Nil)
          case None =>
            DefinitionPattern.findPrefixMatchOf(line) match {
              case None => read
              case Some(definition) =>
                val indent: Int = definition.group(1).length
                val modifiers: String = definition.group(2)
                val method: String = definition.group(3)
                val outerDefs: List[Int] = read.openDefs.dropWhile(_ >= indent)
                val owners: List[Enclosing] = read.enclosing.dropWhile(_.indent >= indent)
                val opened: ScanState =
                  read.copy(enclosing = owners, openDefs = indent :: outerDefs, scaladocThrows = false)
                val public: Boolean =
                  outerDefs.isEmpty && !modifiers.contains("private") &&
                    !modifiers.contains("protected") && owners.headOption.exists(_.visible)
                if (!public) {
                  opened
                } else {
                  val signature: String =
                    withoutBody(lines.slice(index, index + SignatureLines).mkString(" "))
                  val declaration: Declaration =
                    Declaration(
                      path,
                      owners.head.name,
                      method,
                      parametersOf(signature, method),
                      index + 1)
                  val channel: Boolean = returnTypeOf(signature, method).exists(isFailureChannel)
                  opened.copy(
                    failable = if (channel) declaration :: opened.failable else opened.failable,
                    throwing =
                      if (documentsThrow) declaration :: opened.throwing else opened.throwing)
                }
            }
        }
    }
  }

  /** Every source file of the two modules, read once. */
  private lazy val scannedFiles: List[ScanState] =
    SourceRoots.flatMap(scalaSourcesOf).map(scannedFile)

  private lazy val derivedFailableDeclarations: List[Declaration] =
    scannedFiles.flatMap(_.failable.reverse)

  private lazy val derivedThrowingDeclarations: List[Declaration] =
    scannedFiles.flatMap(_.throwing.reverse)

  private lazy val derivedFailableFamilies: Map[String, List[Declaration]] =
    derivedFailableDeclarations.groupBy(_.family)

  private lazy val derivedThrowingFamilies: Map[String, List[Declaration]] =
    derivedThrowingDeclarations.groupBy(_.family)

  //=========================================================================
  // THE REGISTRY
  //
  // One row per enumerated family, carrying the evidence of its own classification rather than the
  // name of a test trusted to hold some. Exactly one assertion is generated per row, in one of
  // four kinds:
  //
  //   - `Rejects`  - the call reports at least one failure, with the reason where one is fixed;
  //   - `Raises`   - the call raises, and the exception type is asserted;
  //   - `Total`    - the channel is carried but no input can fill it: the row states the reason
  //                  and the call must '''succeed''';
  //   - `Covered`  - a declaration or delegating overload whose failure another row asserts, which
  //                  must exist and must itself be a failing row.
  //
  // A row whose member a hand-written test above says more about names that test with
  // `alsoAssertedBy`, and `aap_inventory_entries_name_an_existing_test` checks it still exists.
  //=========================================================================


  /**
   * A row whose call must report at least one failure.
   *
   * @param label  the member the row accounts for
   * @param call  the call, evaluated when the generated test runs
   * @param outcome  reads the failures of whichever channel the call answers through
   * @tparam R  the type of the outcome the call answers with
   * @return the row
   */
  private def rejects[R](label: String)(call: => R)(implicit outcome: Outcome[R]): SurfaceRow =
    rejecting(label, None)(call)

  /**
   * A row whose call must report at least one failure carrying the specified reason.
   *
   * @param label  the member the row accounts for
   * @param reason  the reason the failure has to carry
   * @param call  the call, evaluated when the generated test runs
   * @param outcome  reads the failures of whichever channel the call answers through
   * @tparam R  the type of the outcome the call answers with
   * @return the row
   */
  private def rejectsWith[R](label: String, reason: FailureReason)(call: => R)(
      implicit outcome: Outcome[R]): SurfaceRow =
    rejecting(label, Some(reason))(call)

  /** Builds a rejecting row, with or without an expected reason. */
  private def rejecting[R](label: String, reason: Option[FailureReason])(call: => R)(
      implicit outcome: Outcome[R]): SurfaceRow =
    SurfaceRow(
      label,
      Rejects(reason),
      None,
      () => {
        val failures: List[Failure] = outcome.failures(call)
        withClue(s"$label was expected to report a failure and reported none: ") {
          failures should not be empty
        }
        reason match {
          case None => succeed
          case Some(expected) =>
            withClue(
              s"$label reported ${failures.map(failure => s"${failure.reason}('${failure.message}')").mkString("; ")}: ") {
              failures.map(_.reason) should contain(expected)
            }
        }
      })

  /**
   * A row whose call must raise the documented exception.
   *
   * @param label  the member the row accounts for
   * @param call  the call, evaluated when the generated test runs
   * @param tag  identifies the exception type at run time
   * @tparam E  the exception type the documented throw uses
   * @return the row
   */
  private def raises[E <: Throwable](label: String)(call: => Any)(
      implicit tag: ClassTag[E]): SurfaceRow =
    raising[E](label, None)(call)

  /**
   * A row whose call must raise the documented exception with the specified message part.
   *
   * @param label  the member the row accounts for
   * @param messagePart  text the message of the exception has to include
   * @param call  the call, evaluated when the generated test runs
   * @param tag  identifies the exception type at run time
   * @tparam E  the exception type the documented throw uses
   * @return the row
   */
  private def raisesWith[E <: Throwable](label: String, messagePart: String)(call: => Any)(
      implicit tag: ClassTag[E]): SurfaceRow =
    raising[E](label, Some(messagePart))(call)

  /** Builds a raising row, with or without an expected message part. */
  private def raising[E <: Throwable](label: String, messagePart: Option[String])(call: => Any)(
      implicit tag: ClassTag[E]): SurfaceRow =
    SurfaceRow(
      label,
      Raises(tag.runtimeClass.getSimpleName),
      None,
      () => {
        val thrown: E = intercept[E](call)
        messagePart match {
          case None => succeed
          case Some(expected) =>
            withClue(s"$label raised ${thrown.getClass.getName}('${thrown.getMessage}'): ") {
              Option(thrown.getMessage).getOrElse("") should include(expected)
            }
        }
      })

  /**
   * A row recording that no input of the family can fill the channel it carries.
   *
   * The call must succeed, which is what makes the classification an assertion rather than a
   * claim: a family that starts refusing something fails this row.
   *
   * @param label  the member the row accounts for
   * @param reason  why no input can make the member fail
   * @param call  the call, evaluated when the generated test runs
   * @param outcome  reads the failures of whichever channel the call answers through
   * @tparam R  the type of the outcome the call answers with
   * @return the row
   */
  private def total[R](label: String, reason: String)(call: => R)(
      implicit outcome: Outcome[R]): SurfaceRow =
    SurfaceRow(
      label,
      Total(reason),
      None,
      () => {
        val failures: List[Failure] = outcome.failures(call)
        withClue(
          s"$label is recorded as total because $reason, so the call must succeed, and it " +
            s"reported ${failures.map(_.message).mkString("; ")}: ") {
          failures shouldBe empty
        }
      })

  /**
   * A row recording that another row asserts this family's failure.
   *
   * @param label  the member the row accounts for
   * @param by  the label of the row that asserts the failure
   * @param reason  why this declaration is covered by that one
   * @return the row
   */
  private def covered(label: String, by: String, reason: String): SurfaceRow =
    SurfaceRow(
      label,
      Covered(by, reason),
      None,
      () => {
        withClue(
          s"$label is recorded as covered by '$by' because $reason, which has to be a failing " +
            "row of this registry: ") {
          registry.get(by).exists(_.establishment.failing) shouldBe true
        }
      })

  /**
   * A row recording that another row asserts this member's failure, for a member that can be
   * called.
   *
   * This is [[covered]] for a concrete member, and it asserts both halves of what such a row
   * claims: that a row of this registry really fails for the channel this member carries, and
   * that this member's own call reaches the value. A member that starts refusing what it accepts
   * today fails the second half, and one whose covering row stops failing fails the first.
   *
   * @param label  the member the row accounts for
   * @param by  the label of the row that asserts the failure of the channel
   * @param reason  why this member's own arguments cannot fill that channel
   * @param call  the call, evaluated when the generated test runs
   * @param outcome  reads the failures of whichever channel the call answers through
   * @tparam R  the type of the outcome the call answers with
   * @return the row
   */
  private def coveredBySucceeding[R](label: String, by: String, reason: String)(call: => R)(
      implicit outcome: Outcome[R]): SurfaceRow =
    SurfaceRow(
      label,
      Covered(by, reason),
      None,
      () => {
        withClue(
          s"$label carries the failure channel asserted by '$by', and $reason, so that row has " +
            "to be a failing row of this registry: ") {
          registry.get(by).exists(_.establishment.failing) shouldBe true
        }
        val failures: List[Failure] = outcome.failures(call)
        withClue(
          s"$label cannot fill that channel itself because $reason, so its own call must reach " +
            s"the value, and it reported ${failures.map(_.message).mkString("; ")}: ") {
          failures shouldBe empty
        }
      })

  //-------------------------------------------------------------------------
  // Fixtures the rows need beyond those the hand-written tests above use. The values are `lazy`,
  // so a fixture is built only if a row that reads it runs.

  /** A failure to hand to the members that propagate one rather than produce one. */
  private val surfaceFailure: Failure = Failure.Invalid("A failure supplied by FailableSurfaceSpec")

  /** An identifier no reference data holds, which reaches the default `resolve` of the trait. */
  private object SurfaceMissingId extends ReferenceDataId[HolidayCalendar] {

    override def valueType: ReferenceDataType[HolidayCalendar] = ReferenceDataType.holidayCalendar
  }

  /** A typed string, the abstraction having no instance of its own in either module. */
  private final class SurfaceLabel(val name: String) extends Named

  private object SurfaceLabel
      extends TypedStringCompanion[SurfaceLabel](
        TypedStringCompanion.matchingPattern(
          "[A-Z]{1,5}".r,
          "A surface label is one to five upper case letters"),
        new SurfaceLabel(_))

  private lazy val unknownCalendarId: HolidayCalendarId = HolidayCalendarId.of("NoSuchCalendarFS")

  /**
   * A composite identifier joining more calendars than one calendar may read through.
   *
   * `HolidayCalendarId.of` accepts any name, so the number of parts is whatever the text says,
   * and a name of this many parts describes a calendar deeper than the family allows. Resolving
   * it reports that in its own failure channel, which is what the row reading this asserts; the
   * parts are named so that none of them resolves either, so the row cannot pass for the wrong
   * reason.
   */
  private lazy val overWideCompositeId: HolidayCalendarId =
    HolidayCalendarId.of(
      (1 to HolidayCalendar.MaxCompositeDepth + 1).map(index => s"NoSuchCalendarFS$index").mkString("+"))

  /** The entry of the fixture calendar, used to supply one identifier twice. */
  private lazy val calendarEntry: ReferenceData.Entry[HolidayCalendar] =
    ReferenceData.Entry[HolidayCalendar](HolidayCalendarId.of("TestFailableSurface"), datedCalendar)

  private lazy val unresolvableDays: DaysAdjustment =
    DaysAdjustment.ofBusinessDays(2, unknownCalendarId)

  private lazy val unresolvableBusinessDay: BusinessDayAdjustment =
    BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, unknownCalendarId)

  private lazy val unresolvableDate: AdjustableDate =
    AdjustableDate.of(JAN_15, unresolvableBusinessDay)

  /** The largest decimal the representation holds, which its arithmetic overflows from. */
  private lazy val hugeDecimal: Decimal = obtained(Decimal.of("999999999999999999"))

  /** Two at a fixed scale of two, which is what a scale-changing map is applied to. */
  private lazy val fixedTwo: FixedScaleDecimal = accepted(FixedScaleDecimal.of(two, 2))

  /**
   * A run of one infinite value, whose scaling by zero is a value no amount admits.
   *
   * The element invariant of [[CurrencyAmountArray]] refuses a value that is not a number at
   * construction, so a run holding one is no longer a fixture this suite can build - which is
   * why the rows below assert the refusal at the routes that produce or accept such a value
   * rather than at the routes that read one back.
   */
  private lazy val infiniteArray: CurrencyAmountArray =
    CurrencyAmountArray.of(Currency.GBP, DoubleArray.of(Double.PositiveInfinity))

  /** A run of one infinite value per currency, whose scaling by zero is likewise refused. */
  private lazy val infiniteRun: MultiCurrencyAmountArray =
    accepted(
      MultiCurrencyAmountArray.of(
        Map(Currency.GBP -> DoubleArray.of(Double.PositiveInfinity))))

  /** An infinite amount, which is admitted, and whose sum with its negation is not. */
  private lazy val gbpInfinite: CurrencyAmount =
    obtained(CurrencyAmount.of(Currency.GBP, Double.PositiveInfinity))

  private lazy val gbpNegativeInfinite: CurrencyAmount =
    obtained(CurrencyAmount.of(Currency.GBP, Double.NegativeInfinity))

  /** An infinite dollar amount, which pairs with [[gbpInfinite]] in one multi-currency amount. */
  private lazy val usdNegativeInfinite: CurrencyAmount =
    obtained(CurrencyAmount.of(Currency.USD, Double.NegativeInfinity))

  /** An infinite multi-currency amount, for the same numeric edge one value further out. */
  private lazy val multiInfinite: MultiCurrencyAmount = obtained(MultiCurrencyAmount.of(gbpInfinite))

  private lazy val multiNegativeInfinite: MultiCurrencyAmount =
    obtained(MultiCurrencyAmount.of(gbpNegativeInfinite))

  /**
   * Two infinities of opposite sign in different currencies.
   *
   * Mapping both onto one currency is what makes their sum the value no amount admits, which is
   * the one input `mapCurrencyAmounts` documents a refusal for.
   */
  private lazy val multiTwoInfinities: MultiCurrencyAmount =
    obtained(MultiCurrencyAmount.of(gbpInfinite, usdNegativeInfinite))

  /** Money of eighteen digits, whose arithmetic overflows the decimal behind the type. */
  private lazy val hugeMoney: Money =
    obtained(Money.of(Currency.GBP, new BigDecimal("999999999999999999")))

  private lazy val hugeBigMoney: BigMoney =
    obtained(BigMoney.of(Currency.GBP, new BigDecimal("999999999999999999")))

  /** A date adjuster that adds one calendar day, which no date at the end of the line accepts. */
  private lazy val oneDayLater: DateAdjuster =
    obtained(DaysAdjustment.ofCalendarDays(1).resolve(standardRefData))

  /** The Euroyen TIBOR name, whose every published index has been retired. */
  private lazy val euroyenName: FloatingRateName =
    obtained(FloatingRateName.parse("JPY-TIBOR-EUROYEN"))

  /** An adjuster that moves the third boundary of the fixture schedule onto the second. */
  private lazy val collapsingAdjuster: DateAdjuster =
    DateAdjuster(date => if (date == JUL_15) APR_15 else date)

  private lazy val contradictorySteps: List[ValueStep] =
    List(
      accepted(ValueStep.of(1, ValueAdjustment.ofReplace(200d))),
      accepted(ValueStep.of(1, ValueAdjustment.ofReplace(300d))))

  /** A schedule of values holding one step, which is what the `with*` members derive from. */
  private lazy val valueSchedule: ValueSchedule =
    accepted(ValueSchedule.of(100d, accepted(ValueStep.of(1, ValueAdjustment.ofReplace(200d)))))

  private lazy val stepSequence: ValueStepSequence =
    accepted(ValueStepSequence.of(APR_15, JUL_15, Frequency.P3M, ValueAdjustment.ofDeltaAmount(-1d)))

  //-------------------------------------------------------------------------
  // The rows of `strata-collect`, in the order the enumeration reads its files. The module is the
  // error channel, so most of its failable surface is the channel itself: `Validate` produces
  // failures, the `result` package converts and combines them, and the two decimal types are where
  // a value can be out of range.

  private val collectFailableRows: List[SurfaceRow] = List(
    rejects("Collections.ensureOnlyOne")(Collections.ensureOnlyOne(List(1, 2))),
    rejects("Collections.toSortedMap")(
      Collections.toSortedMap(List(gbp100, gbp100), (amount: CurrencyAmount) => amount.currency)),
    rejectsWith("Decimal.mapAsDouble", FailureReason.INVALID)(two.mapAsDouble(_ => Double.NaN)),
    rejectsWith("Decimal.mapAsBigDecimal", FailureReason.INVALID)(
      two.mapAsBigDecimal(_ => new BigDecimal("1E+30"))),
    rejects("Decimal.toFixedScale")(two.toFixedScale(19)),
    rejectsWith("Decimal.of", FailureReason.INVALID)(Decimal.of(Double.NaN))
      .alsoAssertedBy("Decimal.of reports a value no decimal holds"),
    rejects("Decimal.ofScaled")(Decimal.ofScaled(Long.MaxValue, -1)),
    rejectsWith("Decimal.parse", FailureReason.PARSING)(Decimal.parse("Rubbish"))
      .alsoAssertedBy("Decimal.parse reports text that names no decimal"),
    rejects("FixedScaleDecimal.map")(fixedTwo.map(_ => obtained(Decimal.of(0.001)))),
    rejects("FixedScaleDecimal.of")(FixedScaleDecimal.of(two, 19))
      .alsoAssertedBy("FixedScaleDecimal.of reports a scale the decimal cannot be held at"),
    rejects("FixedScaleDecimal.parse")(FixedScaleDecimal.parse("Rubbish")),
    rejects("TypedStringCompanion.of")(SurfaceLabel.of("lower case")),
    total("Validate.valid", "it lifts a value into the passing outcome and reads nothing about it")(
      Validate.valid(1)),
    rejects("Validate.invalid")(Validate.invalid[Int](surfaceFailure)),
    rejects("Validate.invalidNec")(Validate.invalidNec[Int]("A rejection of FailableSurfaceSpec")),
    rejects("Validate.cond")(Validate.cond(test = false, 1, surfaceFailure)),
    rejects("Validate.fromResult")(Validate.fromResult[Int](Left(surfaceFailure))),
    rejects("Validate.toResult")(Validate.toResult(Validate.invalid[Int](surfaceFailure))),
    rejects("Validate.isTrue")(Validate.isTrue(validIfTrue = false, "The test value was false")),
    rejects("Validate.isFalse")(Validate.isFalse(validIfFalse = true, "The test value was true")),
    rejects("Validate.matches")(Validate.matches("[A-Z]+".r, "lower case", "name")),
    rejects("Validate.notBlank")(Validate.notBlank("   ", "name")),
    rejects("Validate.notEmpty")(Validate.notEmpty("", "name")),
    rejects("Validate.noDuplicates")(Validate.noDuplicates(Array(1d, 1d), "values")),
    rejects("Validate.noDuplicatesSorted")(Validate.noDuplicatesSorted(Array(2d, 1d), "values")),
    rejects("Validate.notPositive")(Validate.notPositive(1, "count")),
    rejects("Validate.notPositiveIfPresent")(Validate.notPositiveIfPresent(Some(two), "count")),
    rejects("Validate.notNegative")(Validate.notNegative(-1, "count")),
    rejects("Validate.notNaN")(Validate.notNaN(Double.NaN, "value")),
    rejects("Validate.notNegativeOrZero")(Validate.notNegativeOrZero(0, "count")),
    rejects("Validate.notZero")(Validate.notZero(0d, "value")),
    rejects("Validate.inRange")(Validate.inRange(5, 0, 5, "count")),
    rejects("Validate.inRangeInclusive")(Validate.inRangeInclusive(6, 0, 5, "count")),
    rejects("Validate.inRangeExclusive")(Validate.inRangeExclusive(5, 0, 5, "count")),
    rejects("Validate.inRangeComparable")(
      Validate.inRangeComparable(Currency.USD, Currency.AUD, Currency.EUR, "currency")),
    rejects("Validate.inRangeComparableInclusive")(
      Validate.inRangeComparableInclusive(Currency.USD, Currency.AUD, Currency.EUR, "currency")),
    rejects("Validate.inRangeComparableExclusive")(
      Validate.inRangeComparableExclusive(Currency.AUD, Currency.AUD, Currency.EUR, "currency")),
    rejects("Validate.inOrderNotEqual")(
      Validate.inOrderNotEqual(Currency.USD, Currency.AUD, "first", "second")),
    rejects("Validate.inOrderOrEqual")(
      Validate.inOrderOrEqual(Currency.USD, Currency.AUD, "first", "second")),
    rejectsWith("NamedEnum.parse", FailureReason.PARSING)(NamedEnum[Currency].parse("NotACurrency"))
      .alsoAssertedBy("NamedEnum.parse reports a name no member of the family carries"),
    rejectsWith("FailureReason.parse", FailureReason.PARSING)(FailureReason.parse("NotAReason")),
    rejects("result.toNec")(result.toNec[Int](Left(surfaceFailure))),
    rejects("result.toValidated")(result.toValidated(result.toNec[Int](Left(surfaceFailure)))),
    rejects("result.toResult")(result.toResult(Validate.invalid[Int](surfaceFailure))),
    rejects("result.sequence")(
      result.sequence(List(result.toNec[Int](Left(surfaceFailure)), Right(1)))),
    rejects("result.combine")(
      result.combine(List(result.toNec[Int](Left(surfaceFailure)), Right(1)))(values => values.sum)),
    rejects("result.flatCombine")(
      result.flatCombine(List(result.toNec[Int](Left(surfaceFailure)), Right(1)))(values =>
        Right(values.sum))),
    rejects("result.withAdditionalFailures")(
      result.withAdditionalFailures(ValueWithFailures.of(1), List(surfaceFailure))),
    rejects("ValueWithFailures.of")(ValueWithFailures.of(1, List(surfaceFailure))),
    rejects("ValueWithFailures.withValue")(
      ValueWithFailures.withValue(ValueWithFailures.of(1, List(surfaceFailure)), 2)),
    rejects("ValueWithFailures.combineValuesAsList")(
      ValueWithFailures.combineValuesAsList(List(ValueWithFailures.of(1, List(surfaceFailure))))),
    rejects("ValueWithFailures.combineValuesAsSet")(
      ValueWithFailures.combineValuesAsSet(List(ValueWithFailures.of(1, List(surfaceFailure))))))

  //-------------------------------------------------------------------------
  // The rows of the root package of `strata-basics`: reference data, resolution and identifiers.
  // The two resolution traits are extension points with no implementation inside this module, so
  // their rows are covered by the resolving members that do exist.

  private val basicsRootFailableRows: List[SurfaceRow] = List(
    rejectsWith("ReferenceData.getValue", FailureReason.MISSING_DATA)(
      emptyRefData.getValue(HolidayCalendarIds.GBLO))
      .alsoAssertedBy("ReferenceData.getValue reports an identifier the reference data does not hold"),
    rejectsWith("ReferenceData.of", FailureReason.INVALID)(
      ReferenceData.of(calendarEntry, calendarEntry)),
    rejectsWith("ImmutableReferenceData.of", FailureReason.INVALID)(
      ImmutableReferenceData.of(calendarEntry, calendarEntry)),
    rejectsWith("ReferenceDataId.resolve", FailureReason.MISSING_DATA)(
      SurfaceMissingId.resolve(standardRefData))
      .alsoAssertedBy("ReferenceDataId.resolve reports an identifier the reference data does not hold"),
    covered(
      "Resolvable.resolve",
      by = "BusinessDayAdjustment.resolve",
      reason =
        "it is the abstract declaration of the trait, and the adjustments of this module are its " +
          "implementations"),
    covered(
      "ResolvableCalculationTarget.resolveTarget",
      by = "HolidayCalendarId.resolve",
      reason =
        "no type of either module implements it - it is the contract the trades of later slices " +
          "implement - and the failure such an implementation propagates is the failure of " +
          "resolving an identifier"),
    rejectsWith("StandardId.of", FailureReason.INVALID)(StandardId.of("", "1"))
      .alsoAssertedBy("StandardId.of reports parts that name no identifier"),
    rejectsWith("StandardId.parse", FailureReason.PARSING)(StandardId.parse("NoSeparator"))
      .alsoAssertedBy("StandardId.parse reports text that names no identifier"),
    rejects("StandardSchemes.createTicMic")(StandardSchemes.createTicMic("ULVR", "LSE")),
    rejects("StandardSchemes.splitTicMic")(
      StandardSchemes.splitTicMic(accepted(StandardId.of("OG-Ticker", "NoMic"))))
      .alsoAssertedBy("StandardSchemes.splitTicMic reports an identifier that is no TICMIC"))

  //-------------------------------------------------------------------------
  // The rows of the currency package. Four failure shapes account for it: two currencies that
  // disagree, a rate the provider does not hold, an amount no decimal holds, and two runs that
  // disagree in length.

  private val currencyFailableRows: List[SurfaceRow] = List(
    rejects("AdjustablePayment.of")(AdjustablePayment.of(Currency.GBP, Double.NaN, JAN_15)),
    rejectsWith("AdjustablePayment.resolve", FailureReason.MISSING_DATA)(
      obtained(AdjustablePayment.of(Currency.GBP, 100d, unresolvableDate)).resolve(emptyRefData)),
    rejectsWith("BigMoney.plus", FailureReason.INVALID)(gbpBigMoney.plus(usdBigMoney))
      .alsoAssertedBy("Money.plus, Money.minus, BigMoney.plus and BigMoney.minus report a currency mismatch"),
    rejectsWith("BigMoney.minus", FailureReason.INVALID)(gbpBigMoney.minus(usdBigMoney))
      .alsoAssertedBy("Money.plus, Money.minus, BigMoney.plus and BigMoney.minus report a currency mismatch"),
    rejects("BigMoney.mapAmount")(gbpBigMoney.mapAmount(_ => new BigDecimal("1E+30"))),
    rejectsWith("BigMoney.isGreaterThan", FailureReason.INVALID)(gbpBigMoney.isGreaterThan(usdBigMoney)),
    rejectsWith("BigMoney.isGreaterThanEqualTo", FailureReason.INVALID)(
      gbpBigMoney.isGreaterThanEqualTo(usdBigMoney)),
    rejectsWith("BigMoney.isLessThan", FailureReason.INVALID)(gbpBigMoney.isLessThan(usdBigMoney)),
    rejectsWith("BigMoney.isLessThanEqualTo", FailureReason.INVALID)(
      gbpBigMoney.isLessThanEqualTo(usdBigMoney)),
    rejectsWith("BigMoney.convertedTo", FailureReason.INVALID)(
      gbpBigMoney.convertedTo(Currency.GBP, two))
      .alsoAssertedBy("Money.convertedTo and BigMoney.convertedTo report a non-unit rate for the same currency"),
    rejects("BigMoney.of")(BigMoney.of(Currency.GBP, Double.NaN)),
    rejectsWith("BigMoney.parse", FailureReason.PARSING)(BigMoney.parse("Rubbish")),
    rejects("Currency.of")(Currency.of("GBPX")),
    rejectsWith("Currency.parse", FailureReason.PARSING)(Currency.parse("XYZ"))
      .alsoAssertedBy("Currency.parse reports a code outside the closed family"),
    rejectsWith("CurrencyAmount.plus", FailureReason.INVALID)(gbp100.plus(usd100))
      .alsoAssertedBy("CurrencyAmount.plus and minus report a currency mismatch"),
    rejectsWith("CurrencyAmount.minus", FailureReason.INVALID)(gbp100.minus(usd100))
      .alsoAssertedBy("CurrencyAmount.plus and minus report a currency mismatch"),
    rejectsWith("CurrencyAmount.convertedTo", FailureReason.INVALID)(
      gbp100.convertedTo(Currency.GBP, 2d))
      .alsoAssertedBy("CurrencyAmount.convertedTo reports a non-unit rate for the same currency"),
    rejects("CurrencyAmount.of")(CurrencyAmount.of(Currency.GBP, Double.NaN)),
    rejectsWith("CurrencyAmount.toMoney", FailureReason.INVALID)(infiniteAmount.toMoney)
      .alsoAssertedBy(
        "CurrencyAmount.toMoney and CurrencyAmount.toBigMoney report an amount no decimal holds"),
    rejectsWith("CurrencyAmount.toBigMoney", FailureReason.INVALID)(infiniteAmount.toBigMoney)
      .alsoAssertedBy(
        "CurrencyAmount.toMoney and CurrencyAmount.toBigMoney report an amount no decimal holds"),
    rejectsWith("CurrencyAmount.parse", FailureReason.PARSING)(CurrencyAmount.parse("Rubbish"))
      .alsoAssertedBy("CurrencyAmount.parse reports text that names no amount"),
    rejectsWith("CurrencyAmountArray.convertedTo", FailureReason.CURRENCY_CONVERSION)(
      gbpArray.convertedTo(Currency.EUR, gbpUsdMatrix)),
    rejectsWith("CurrencyAmountArray.plus", FailureReason.INVALID)(gbpArray.plus(shortGbpArray))
      .alsoAssertedBy("CurrencyAmountArray arithmetic reports size and currency mismatches"),
    rejectsWith("CurrencyAmountArray.minus", FailureReason.INVALID)(gbpArray.minus(usdArray))
      .alsoAssertedBy("CurrencyAmountArray arithmetic reports size and currency mismatches"),
    rejectsWith("CurrencyAmountArray.of", FailureReason.INVALID)(
      CurrencyAmountArray.of(List.empty[CurrencyAmount]))
      .alsoAssertedBy("CurrencyAmountArray.of reports an empty collection and mixed currencies"),
    rejectsWith("CurrencyPair.other", FailureReason.INVALID)(
      CurrencyPair.of(Currency.GBP, Currency.USD).other(Currency.EUR))
      .alsoAssertedBy("CurrencyPair.other reports a currency that is not in the pair"),
    rejectsWith("CurrencyPair.parse", FailureReason.PARSING)(CurrencyPair.parse("Rubbish"))
      .alsoAssertedBy("CurrencyPair.parse reports text that names no pair"),
    covered(
      "FxConvertible.convertedTo",
      by = "CurrencyAmount.convertedTo",
      reason =
        "it is the abstract declaration of the trait, implemented by every amount type of this " +
          "package"),
    rejectsWith("FxMatrix.fxRate", FailureReason.CURRENCY_CONVERSION)(
      gbpUsdMatrix.fxRate(Currency.EUR, Currency.CHF))
      .alsoAssertedBy("FxMatrix.fxRate reports a rate the matrix does not hold"),
    rejectsWith("FxMatrix.convert", FailureReason.CURRENCY_CONVERSION)(
      gbpUsdMatrix.convert(eur100, Currency.GBP))
      .alsoAssertedBy("FxMatrix.convert reports a rate the matrix does not hold"),
    rejectsWith("FxMatrix.withRate", FailureReason.CURRENCY_CONVERSION)(
      gbpUsdMatrix.withRate(Currency.EUR, Currency.CHF, 1.1d))
      .alsoAssertedBy("FxMatrix.withRate reports a pair the matrix has no currency in common with"),
    rejectsWith("FxMatrix.withRates", FailureReason.CURRENCY_CONVERSION)(
      gbpUsdMatrix.withRates(List((CurrencyPair.of(Currency.EUR, Currency.CHF), 1.1d)))),
    rejectsWith("FxMatrix.merge", FailureReason.CURRENCY_CONVERSION)(gbpUsdMatrix.merge(eurChfMatrix))
      .alsoAssertedBy("FxMatrix.merge reports two matrices with no currency in common"),
    rejectsWith("FxMatrix.of", FailureReason.CURRENCY_CONVERSION)(FxMatrix.of(List(gbpUsdRate, eurCadRate)))
      .alsoAssertedBy("FxMatrix.of reports rates that can never be placed"),
    rejectsWith("FxMatrix.ofRates", FailureReason.CURRENCY_CONVERSION)(
      FxMatrix.ofRates(
        List(
          (CurrencyPair.of(Currency.GBP, Currency.USD), 1.6d),
          (CurrencyPair.of(Currency.EUR, Currency.CHF), 1.1d)))),
    rejects("FxMatrix.fromMatrix")(
      FxMatrix.fromMatrix(Vector(Currency.GBP, Currency.USD), DoubleMatrix.of(1, 2, 1d, 1.6d))),
    rejectsWith("FxRate.fxRate", FailureReason.CURRENCY_CONVERSION)(
      gbpUsdRate.fxRate(Currency.EUR, Currency.CHF)),
    rejectsWith("FxRate.crossRate", FailureReason.CURRENCY_CONVERSION)(gbpUsdRate.crossRate(eurCadRate))
      .alsoAssertedBy("FxRate.crossRate reports rates that do not cross"),
    rejects("FxRate.of")(FxRate.of(Currency.GBP, Currency.USD, 0d)),
    rejectsWith("FxRate.parse", FailureReason.PARSING)(FxRate.parse("Rubbish"))
      .alsoAssertedBy("FxRate.parse reports text that names no rate"),
    rejectsWith("FxRateProvider.fxRate", FailureReason.CURRENCY_CONVERSION)(
      FxRateProvider.noConversion().fxRate(Currency.GBP, Currency.USD))
      .alsoAssertedBy("FxRateProvider.fxRate reports a rate the provider cannot supply"),
    rejectsWith("FxRateProvider.convert", FailureReason.CURRENCY_CONVERSION)(
      FxRateProvider.noConversion().convert(100d, Currency.GBP, Currency.USD))
      .alsoAssertedBy("FxRateProvider.convert reports a rate the provider cannot supply"),
    total(
      "Money.getValue",
      "the amount is at the currency's scale by construction and no currency has more than " +
        "three minor unit digits, so neither reason the pairing reports is reachable")(
      gbpMoney.getValue),
    rejectsWith("Money.plus", FailureReason.INVALID)(gbpMoney.plus(usdMoney))
      .alsoAssertedBy("Money.plus, Money.minus, BigMoney.plus and BigMoney.minus report a currency mismatch"),
    rejectsWith("Money.minus", FailureReason.INVALID)(gbpMoney.minus(usdMoney))
      .alsoAssertedBy("Money.plus, Money.minus, BigMoney.plus and BigMoney.minus report a currency mismatch"),
    rejects("Money.mapAmount")(gbpMoney.mapAmount(_ => new BigDecimal("1E+30"))),
    rejectsWith("Money.convertedTo", FailureReason.INVALID)(gbpMoney.convertedTo(Currency.GBP, two))
      .alsoAssertedBy("Money.convertedTo and BigMoney.convertedTo report a non-unit rate for the same currency"),
    rejects("Money.of")(Money.of(Currency.GBP, Double.NaN)),
    rejectsWith("Money.parse", FailureReason.PARSING)(Money.parse("Rubbish")),
    rejectsWith("MultiCurrencyAmount.getAmount", FailureReason.INVALID)(
      gbpMulti.getAmount(Currency.AUD))
      .alsoAssertedBy("MultiCurrencyAmount.getAmount reports a currency the amount does not hold"),
    rejectsWith("MultiCurrencyAmount.convertedTo", FailureReason.CURRENCY_CONVERSION)(
      gbpMulti.convertedTo(Currency.EUR, eurChfMatrix)),
    rejectsWith("MultiCurrencyAmount.of", FailureReason.INVALID)(
      MultiCurrencyAmount.of(gbp100, gbp100))
      .alsoAssertedBy("MultiCurrencyAmount.of reports a duplicated currency"),
    rejectsWith("MultiCurrencyAmountArray.getValues", FailureReason.INVALID)(
      gbpRun.getValues(Currency.AUD))
      .alsoAssertedBy("MultiCurrencyAmountArray.getValues reports a currency the run does not hold"),
    rejectsWith("MultiCurrencyAmountArray.convertedTo", FailureReason.CURRENCY_CONVERSION)(
      gbpRun.convertedTo(Currency.EUR, gbpUsdMatrix)),
    rejectsWith("MultiCurrencyAmountArray.plus(MultiCurrencyAmountArray)", FailureReason.INVALID)(
      gbpRun.plus(shortRun))
      .alsoAssertedBy("MultiCurrencyAmountArray arithmetic reports a size mismatch"),
    coveredBySucceeding(
      "MultiCurrencyAmountArray.plus(MultiCurrencyAmount)",
      by = "MultiCurrencyAmountArray.of",
      reason =
        "the channel it reports is the checking factory's, which it hands the arrays it has " +
          "merged, and the amount is broadcast across the run - a currency the run already " +
          "holds keeps its own array and one it does not is filled to this run's length - so " +
          "every array that factory is handed has the size of this run by construction")(
      gbpRun.plus(gbpMulti))
      .alsoAssertedBy("MultiCurrencyAmountArray arithmetic reports a size mismatch"),
    rejectsWith("MultiCurrencyAmountArray.minus(MultiCurrencyAmountArray)", FailureReason.INVALID)(
      gbpRun.minus(shortRun))
      .alsoAssertedBy("MultiCurrencyAmountArray arithmetic reports a size mismatch"),
    coveredBySucceeding(
      "MultiCurrencyAmountArray.minus(MultiCurrencyAmount)",
      by = "MultiCurrencyAmountArray.of",
      reason =
        "the amount is broadcast across the run exactly as it is by the addition above, so the " +
          "arrays reaching the checking factory all have this run's size")(
      gbpRun.minus(gbpMulti))
      .alsoAssertedBy("MultiCurrencyAmountArray arithmetic reports a size mismatch"),
    rejects("MultiCurrencyAmountArray.of")(
      MultiCurrencyAmountArray.of(
        Map(
          Currency.GBP -> DoubleArray.of(1d, 2d, 3d),
          Currency.USD -> DoubleArray.of(1d, 2d))))
      .alsoAssertedBy("MultiCurrencyAmountArray.of reports values of unequal length"),
    rejects("MultiCurrencyAmountArray.total")(
      MultiCurrencyAmountArray.total(List(gbpArray, shortGbpArray))),
    rejectsWith("Payment.convertedTo", FailureReason.CURRENCY_CONVERSION)(
      obtained(Payment.of(Currency.GBP, 100d, JAN_15)).convertedTo(Currency.EUR, gbpUsdMatrix)),
    rejects("Payment.of")(Payment.of(Currency.GBP, Double.NaN, JAN_15)))

  //-------------------------------------------------------------------------
  // The rows of the date package. A resolving member fails through the absence of the calendar it
  // is asked to resolve; a factory over a period or a tenor refuses a period the convention it is
  // paired with cannot add, or a count that is no period at all.

  private val dateFailableRows: List[SurfaceRow] = List(
    rejectsWith("AdjustableDate.adjusted", FailureReason.MISSING_DATA)(
      unresolvableDate.adjusted(emptyRefData)),
    rejectsWith("AdjustableDates.adjusted", FailureReason.MISSING_DATA)(
      accepted(AdjustableDates.of(unresolvableBusinessDay, JAN_15)).adjusted(emptyRefData)),
    rejectsWith("AdjustableDates.of", FailureReason.INVALID)(
      AdjustableDates.of(List.empty[LocalDate]))
      .alsoAssertedBy("AdjustableDates.of reports dates that describe no set"),
    rejectsWith("BusinessDayAdjustment.adjust", FailureReason.MISSING_DATA)(
      unresolvableBusinessDay.adjust(JAN_15, emptyRefData)),
    rejectsWith("BusinessDayAdjustment.resolve", FailureReason.MISSING_DATA)(
      unresolvableBusinessDay.resolve(emptyRefData)),
    rejectsWith("BusinessDayConvention.parse", FailureReason.PARSING)(
      BusinessDayConvention.parse("NotAConvention")),
    rejectsWith("DateSequence.parse", FailureReason.PARSING)(DateSequence.parse("NotASequence")),
    rejectsWith("SequenceDate.base", FailureReason.INVALID)(SequenceDate.base(0))
      .alsoAssertedBy("SequenceDate.of reports fields that describe no instruction"),
    rejectsWith("SequenceDate.full", FailureReason.INVALID)(SequenceDate.full(0))
      .alsoAssertedBy("SequenceDate.of reports fields that describe no instruction"),
    rejectsWith("SequenceDate.of", FailureReason.INVALID)(
      SequenceDate.of(Some(YearMonth.of(2014, 6)), Some(Period.ofMonths(1)), 1, fullSequence = true))
      .alsoAssertedBy("SequenceDate.of reports fields that describe no instruction"),
    rejectsWith("DayCount.ofBus252", FailureReason.MISSING_DATA)(
      DayCount.ofBus252(unknownCalendarId, emptyRefData))
      .alsoAssertedBy("DayCount.ofBus252 reports a calendar the reference data does not hold"),
    rejectsWith("DayCount.parse", FailureReason.PARSING)(DayCount.parse("NotADayCount")),
    rejectsWith("DaysAdjustment.adjust", FailureReason.MISSING_DATA)(
      unresolvableDays.adjust(JAN_15, emptyRefData))
      .alsoAssertedBy("DaysAdjustment resolution reports a calendar the reference data does not hold"),
    rejectsWith("DaysAdjustment.resolve", FailureReason.MISSING_DATA)(
      unresolvableDays.resolve(emptyRefData))
      .alsoAssertedBy("DaysAdjustment resolution reports a calendar the reference data does not hold"),
    rejectsWith("DaysAdjustment.of", FailureReason.INVALID)(
      DaysAdjustment.of(0, HolidayCalendarIds.GBLO, BusinessDayAdjustment.NONE))
      .alsoAssertedBy("DaysAdjustment.of reports a business day addition of no days"),
    rejectsWith("DaysAdjustment.of(magnitude)", FailureReason.INVALID)(
      DaysAdjustment.of(Int.MaxValue, HolidayCalendarIds.GBLO, BusinessDayAdjustment.NONE))
      .alsoAssertedBy("DaysAdjustment.of reports a business day addition no calendar can walk"),
    // HolidayCalendar.scala
    rejectsWith("HolidayCalendars.of", FailureReason.PARSING)(
      HolidayCalendars.of("NoSuchCalendarFS")),
    rejectsWith("HolidayCalendarId.resolve", FailureReason.MISSING_DATA)(
      unknownCalendarId.resolve(emptyRefData))
      .alsoAssertedBy("HolidayCalendarId.resolve reports a calendar the reference data does not hold"),
    rejectsWith("HolidayCalendarId.resolve(parts)", FailureReason.INVALID)(
      overWideCompositeId.resolve(standardRefData))
      .alsoAssertedBy("HolidayCalendarId.resolve reports a name joining more calendars than one can read"),
    // MarketTenor.scala
    total(
      "MarketTenor.ofSpot",
      "its argument is already a tenor, so there is no count left to refuse; it answers in the " +
        "reported shape only so that it composes with the counted factories")(
      MarketTenor.ofSpot(Tenor.TENOR_1D))
      .alsoAssertedBy("MarketTenor spot factories report a count that is no tenor"),
    rejectsWith("MarketTenor.ofSpotDays", FailureReason.INVALID)(MarketTenor.ofSpotDays(0))
      .alsoAssertedBy("MarketTenor spot factories report a count that is no tenor"),
    rejectsWith("MarketTenor.ofSpotMonths", FailureReason.INVALID)(MarketTenor.ofSpotMonths(-1))
      .alsoAssertedBy("MarketTenor spot factories report a count that is no tenor"),
    rejectsWith("MarketTenor.ofSpotYears", FailureReason.INVALID)(MarketTenor.ofSpotYears(0)),
    rejectsWith("MarketTenor.parse", FailureReason.PARSING)(MarketTenor.parse("2K"))
      .alsoAssertedBy("MarketTenor.parse reports text that names no market tenor"),
    rejectsWith("PeriodAdditionConvention.parse", FailureReason.PARSING)(
      PeriodAdditionConvention.parse("NotAConvention")),
    rejectsWith("PeriodAdjustment.adjust", FailureReason.MISSING_DATA)(
      accepted(
        PeriodAdjustment.of(
          Period.ofMonths(3),
          PeriodAdditionConventions.NONE,
          unresolvableBusinessDay)).adjust(JAN_15, emptyRefData)),
    rejectsWith("PeriodAdjustment.resolve", FailureReason.MISSING_DATA)(
      accepted(
        PeriodAdjustment.of(
          Period.ofMonths(3),
          PeriodAdditionConventions.NONE,
          unresolvableBusinessDay)).resolve(emptyRefData)),
    rejectsWith("PeriodAdjustment.of", FailureReason.INVALID)(
      PeriodAdjustment.of(
        Period.of(1, 2, 3),
        PeriodAdditionConventions.LAST_DAY,
        BusinessDayAdjustment.NONE))
      .alsoAssertedBy("PeriodAdjustment.of reports a period the convention cannot add"),
    rejectsWith("PeriodAdjustment.ofLastDay", FailureReason.INVALID)(
      PeriodAdjustment.ofLastDay(Period.ofDays(3), BusinessDayAdjustment.NONE)),
    rejectsWith("PeriodAdjustment.ofLastBusinessDay", FailureReason.INVALID)(
      PeriodAdjustment.ofLastBusinessDay(Period.ofDays(3), BusinessDayAdjustment.NONE)),
    rejectsWith("Tenor.of", FailureReason.INVALID)(Tenor.of(Period.ZERO))
      .alsoAssertedBy("Tenor.of reports a period that is no tenor"),
    rejectsWith("Tenor.ofDays", FailureReason.INVALID)(Tenor.ofDays(0)),
    rejectsWith("Tenor.ofWeeks", FailureReason.INVALID)(Tenor.ofWeeks(0)),
    rejectsWith("Tenor.ofMonths", FailureReason.INVALID)(Tenor.ofMonths(-1)),
    rejectsWith("Tenor.ofYears", FailureReason.INVALID)(Tenor.ofYears(0)),
    rejectsWith("Tenor.parse", FailureReason.PARSING)(Tenor.parse("2K"))
      .alsoAssertedBy("Tenor.parse reports text that names no tenor"),
    rejectsWith("TenorAdjustment.adjust", FailureReason.MISSING_DATA)(
      accepted(
        TenorAdjustment.of(
          Tenor.TENOR_3M,
          PeriodAdditionConventions.NONE,
          unresolvableBusinessDay)).adjust(JAN_15, emptyRefData)),
    rejectsWith("TenorAdjustment.resolve", FailureReason.MISSING_DATA)(
      accepted(
        TenorAdjustment.of(
          Tenor.TENOR_3M,
          PeriodAdditionConventions.NONE,
          unresolvableBusinessDay)).resolve(emptyRefData)),
    rejectsWith("TenorAdjustment.of", FailureReason.INVALID)(
      TenorAdjustment.of(
        Tenor.TENOR_1W,
        PeriodAdditionConventions.LAST_DAY,
        BusinessDayAdjustment.NONE))
      .alsoAssertedBy("TenorAdjustment.of reports a tenor the convention cannot add"),
    rejectsWith("TenorAdjustment.ofLastDay", FailureReason.INVALID)(
      TenorAdjustment.ofLastDay(Tenor.TENOR_1W, BusinessDayAdjustment.NONE)),
    rejectsWith("TenorAdjustment.ofLastBusinessDay", FailureReason.INVALID)(
      TenorAdjustment.ofLastBusinessDay(Tenor.TENOR_1W, BusinessDayAdjustment.NONE)))

  //-------------------------------------------------------------------------
  // The rows of the index package. An index is a closed family, so text that names no member is a
  // parse failure; every calculation over a fixing date reads the index's calendars from the
  // reference data it is handed, so its failure is the absence of a calendar. The conversions of a
  // floating rate name refuse a name of the wrong kind, and the Euroyen TIBOR name - whose every
  // published index has been retired - is what reaches the retired-family failures.

  private val indexFailableRows: List[SurfaceRow] = List(
    rejectsWith("FloatingRate.parse", FailureReason.PARSING)(FloatingRate.parse("NotAnIndex")),
    rejectsWith("FloatingRateName.defaultTenor", FailureReason.MISSING_DATA)(euroyenName.defaultTenor),
    total(
      "FloatingRateName.normalized",
      "every published family carries its canonical name among the published names, so the " +
        "lookup cannot miss; the row runs it over every name of the family rather than over one")(
      FloatingRateName.values.toList.traverse(name => name.normalized)),
    rejectsWith("FloatingRateName.toIborIndex", FailureReason.INVALID)(
      FloatingRateNames.GBP_SONIA.toIborIndex(Tenor.TENOR_3M))
      .alsoAssertedBy("FloatingRateName.toIborIndex reports a name of the wrong kind and a tenor no index carries"),
    rejectsWith("FloatingRateName.toIborIndexFixingOffset", FailureReason.INVALID)(
      FloatingRateNames.GBP_SONIA.toIborIndexFixingOffset),
    rejectsWith("FloatingRateName.toOvernightIndex", FailureReason.INVALID)(
      FloatingRateNames.GBP_LIBOR.toOvernightIndex)
      .alsoAssertedBy("FloatingRateName.toOvernightIndex reports a name of the wrong kind"),
    rejectsWith("FloatingRateName.toPriceIndex", FailureReason.INVALID)(
      FloatingRateNames.GBP_LIBOR.toPriceIndex)
      .alsoAssertedBy("FloatingRateName.toPriceIndex reports a name of the wrong kind"),
    rejectsWith("FloatingRateName.toFloatingRateIndex", FailureReason.MISSING_DATA)(
      euroyenName.toFloatingRateIndex),
    rejectsWith("FloatingRateName.currency", FailureReason.MISSING_DATA)(euroyenName.currency),
    rejectsWith("FloatingRateName.parse", FailureReason.PARSING)(FloatingRateName.parse("NotAName")),
    rejectsWith("FloatingRateName.defaultIborIndex", FailureReason.MISSING_DATA)(
      FloatingRateName.defaultIborIndex(Currency.BRL))
      .alsoAssertedBy("FloatingRateName.defaultIborIndex reports a currency with no published default"),
    rejectsWith("FloatingRateName.defaultOvernightIndex", FailureReason.MISSING_DATA)(
      FloatingRateName.defaultOvernightIndex(Currency.KRW))
      .alsoAssertedBy("FloatingRateName.defaultOvernightIndex reports a currency with no published default"),
    rejectsWith("FloatingRateType.parse", FailureReason.PARSING)(
      FloatingRateType.parse("NotAType")),
    rejectsWith("FxIndexObservation.of", FailureReason.MISSING_DATA)(
      FxIndexObservation.of(FxIndices.EUR_GBP_ECB, JAN_15, emptyRefData))
      .alsoAssertedBy("FxIndexObservation.of reports a calendar the reference data does not hold"),
    rejectsWith("FxIndexObservation.resolve", FailureReason.MISSING_DATA)(
      FxIndexObservation.resolve(FxIndices.EUR_GBP_ECB, emptyRefData))
      .alsoAssertedBy("FxIndex.resolve reports a calendar the reference data does not hold"),
    rejectsWith("IborIndexObservation.of", FailureReason.MISSING_DATA)(
      IborIndexObservation.of(IborIndices.GBP_LIBOR_3M, JAN_15, emptyRefData))
      .alsoAssertedBy("IborIndexObservation.of reports a calendar the reference data does not hold"),
    rejectsWith("IborIndexObservation.resolve", FailureReason.MISSING_DATA)(
      IborIndexObservation.resolve(IborIndices.GBP_LIBOR_3M, emptyRefData)),
    rejectsWith("Index.parse", FailureReason.PARSING)(Index.parse("NotAnIndex")),
    rejectsWith("FloatingRateIndex.parse", FailureReason.PARSING)(
      FloatingRateIndex.parse("NotAnIndex")),
    rejectsWith("RateIndex.parse", FailureReason.PARSING)(RateIndex.parse("NotAnIndex")),
    rejectsWith("IborIndex.calculateEffectiveFromFixing", FailureReason.MISSING_DATA)(
      IborIndices.GBP_LIBOR_3M.calculateEffectiveFromFixing(JAN_15, emptyRefData)),
    rejectsWith("IborIndex.calculateMaturityFromFixing", FailureReason.MISSING_DATA)(
      IborIndices.GBP_LIBOR_3M.calculateMaturityFromFixing(JAN_15, emptyRefData)),
    rejectsWith("IborIndex.calculateFixingFromEffective", FailureReason.MISSING_DATA)(
      IborIndices.GBP_LIBOR_3M.calculateFixingFromEffective(JAN_15, emptyRefData)),
    rejectsWith("IborIndex.calculateMaturityFromEffective", FailureReason.MISSING_DATA)(
      IborIndices.GBP_LIBOR_3M.calculateMaturityFromEffective(JAN_15, emptyRefData)),
    rejectsWith("IborIndex.resolve", FailureReason.MISSING_DATA)(
      IborIndices.GBP_LIBOR_3M.resolve(emptyRefData))
      .alsoAssertedBy("IborIndex.resolve reports a calendar the reference data does not hold"),
    rejectsWith("IborIndex.parse", FailureReason.PARSING)(IborIndex.parse("NotAnIndex")),
    rejectsWith("OvernightIndex.calculatePublicationFromFixing", FailureReason.MISSING_DATA)(
      OvernightIndices.GBP_SONIA.calculatePublicationFromFixing(JAN_15, emptyRefData)),
    rejectsWith("OvernightIndex.calculateEffectiveFromFixing", FailureReason.MISSING_DATA)(
      OvernightIndices.GBP_SONIA.calculateEffectiveFromFixing(JAN_15, emptyRefData)),
    rejectsWith("OvernightIndex.calculateMaturityFromFixing", FailureReason.MISSING_DATA)(
      OvernightIndices.GBP_SONIA.calculateMaturityFromFixing(JAN_15, emptyRefData)),
    rejectsWith("OvernightIndex.calculateFixingFromEffective", FailureReason.MISSING_DATA)(
      OvernightIndices.GBP_SONIA.calculateFixingFromEffective(JAN_15, emptyRefData)),
    rejectsWith("OvernightIndex.calculateMaturityFromEffective", FailureReason.MISSING_DATA)(
      OvernightIndices.GBP_SONIA.calculateMaturityFromEffective(JAN_15, emptyRefData)),
    rejectsWith("OvernightIndex.parse", FailureReason.PARSING)(OvernightIndex.parse("NotAnIndex")),
    rejectsWith("PriceIndex.parse", FailureReason.PARSING)(PriceIndex.parse("NotAnIndex")),
    rejectsWith("FxIndex.calculateMaturityFromFixing", FailureReason.MISSING_DATA)(
      FxIndices.EUR_GBP_ECB.calculateMaturityFromFixing(JAN_15, emptyRefData)),
    rejectsWith("FxIndex.calculateFixingFromMaturity", FailureReason.MISSING_DATA)(
      FxIndices.EUR_GBP_ECB.calculateFixingFromMaturity(JAN_15, emptyRefData)),
    rejectsWith("FxIndex.resolve", FailureReason.MISSING_DATA)(
      FxIndices.EUR_GBP_ECB.resolve(emptyRefData))
      .alsoAssertedBy("FxIndex.resolve reports a calendar the reference data does not hold"),
    rejectsWith("FxIndex.parse", FailureReason.PARSING)(FxIndex.parse("NotAnIndex"))
      .alsoAssertedBy("FxIndex.of(String) reports text that names no published index"),
    rejectsWith("FxIndex.of", FailureReason.PARSING)(
      FxIndex.of(CurrencyPair.of(Currency.BRL, Currency.KRW)))
      .alsoAssertedBy("FxIndex.of(CurrencyPair) reports a pair no published index quotes"),
    rejectsWith("OvernightIndexObservation.resolve", FailureReason.MISSING_DATA)(
      OvernightIndexObservation.resolve(OvernightIndices.GBP_SONIA, emptyRefData))
      .alsoAssertedBy(
        "OvernightIndexObservation.resolve reports a calendar the reference data does not hold"),
    rejectsWith("OvernightIndexObservation.of", FailureReason.MISSING_DATA)(
      OvernightIndexObservation.of(OvernightIndices.GBP_SONIA, JAN_15, emptyRefData))
      .alsoAssertedBy("OvernightIndexObservation.of reports a calendar the reference data does not hold"))

  //-------------------------------------------------------------------------
  // The rows of the location, schedule and value packages. A schedule definition is judged
  // against the dates it holds and then against the calendar it is rolled out over, which is why
  // the generating members report separately from the factory; the `with*` members re-run that
  // judgement, so those replacing a date can report and those replacing an adjustment or a
  // convention cannot.

  private val scheduleFailableRows: List[SurfaceRow] = List(
    rejectsWith("Country.code3Char", FailureReason.MISSING_DATA)(accepted(Country.of("EU")).code3Char),
    rejectsWith("Country.of", FailureReason.INVALID)(Country.of("abc"))
      .alsoAssertedBy("Country.of reports a malformed code and accepts any well-formed one"),
    rejectsWith("Country.parse", FailureReason.INVALID)(Country.parse("abc")),
    rejectsWith("Country.of3Char", FailureReason.PARSING)(Country.of3Char("ZZZ"))
      .alsoAssertedBy("Country.of3Char reports a code that names no country"),
    rejectsWith("Frequency.eventsPerYear", FailureReason.INVALID)(
      accepted(Frequency.of(Period.ofMonths(5))).eventsPerYear)
      .alsoAssertedBy("Frequency.eventsPerYear reports a frequency with no exact count"),
    rejectsWith("Frequency.exactDivide", FailureReason.INVALID)(
      Frequency.P3M.exactDivide(Frequency.P2M))
      .alsoAssertedBy("Frequency.exactDivide reports a non-integral ratio and divides an integral one"),
    rejectsWith("Frequency.of", FailureReason.INVALID)(Frequency.of(Period.ZERO))
      .alsoAssertedBy("Frequency.of reports a period that is no frequency"),
    rejectsWith("Frequency.ofDays", FailureReason.INVALID)(Frequency.ofDays(0)),
    rejectsWith("Frequency.ofWeeks", FailureReason.INVALID)(Frequency.ofWeeks(0)),
    rejectsWith("Frequency.ofMonths", FailureReason.INVALID)(Frequency.ofMonths(-1)),
    rejectsWith("Frequency.ofYears", FailureReason.INVALID)(Frequency.ofYears(0)),
    rejectsWith("Frequency.parse", FailureReason.PARSING)(Frequency.parse("2K"))
      .alsoAssertedBy("Frequency.parse reports text that names no frequency"),
    rejectsWith("PeriodicSchedule.createSchedule", FailureReason.INVALID)(
      badStubDefinition.createSchedule(standardRefData))
      .alsoAssertedBy("PeriodicSchedule.createSchedule reports a definition that generates nothing"),
    rejectsWith("PeriodicSchedule.createUnadjustedDates", FailureReason.INVALID)(
      badStubDefinition.createUnadjustedDates())
      .alsoAssertedBy("PeriodicSchedule.createUnadjustedDates reports a disallowed stub"),
    rejectsWith("PeriodicSchedule.createAdjustedDates", FailureReason.INVALID)(
      collapsingDefinition.createAdjustedDates(standardRefData))
      .alsoAssertedBy("PeriodicSchedule.createAdjustedDates reports dates that adjust onto one another"),
    rejectsWith("PeriodicSchedule.of", FailureReason.INVALID)(
      PeriodicSchedule.of(OCT_15, JAN_15, Frequency.P3M, BusinessDayAdjustment.NONE))
      .alsoAssertedBy("PeriodicSchedule.of reports dates that describe no schedule"),
    rejectsWith("PeriodicSchedule.replaceStartDate", FailureReason.INVALID)(
      quarterly.replaceStartDate(OCT_15.plusYears(1L))),
    rejectsWith("PeriodicSchedule.withStartDate", FailureReason.INVALID)(
      quarterly.withStartDate(OCT_15.plusYears(1L))),
    rejectsWith("PeriodicSchedule.withEndDate", FailureReason.INVALID)(
      quarterly.withEndDate(JAN_15.minusYears(1L))),
    rejectsWith("PeriodicSchedule.withFirstRegularStartDate", FailureReason.INVALID)(
      quarterly.withFirstRegularStartDate(Some(OCT_15.plusYears(1L)))),
    rejectsWith("PeriodicSchedule.withLastRegularEndDate", FailureReason.INVALID)(
      quarterly.withLastRegularEndDate(Some(OCT_15.plusYears(1L)))),
    rejectsWith("PeriodicSchedule.withOverrideStartDate", FailureReason.INVALID)(
      quarterly.withOverrideStartDate(
        Some(AdjustableDate.of(OCT_15.plusYears(1L), BusinessDayAdjustment.NONE)))),
    total(
      "PeriodicSchedule.withBusinessDayAdjustment",
      "the seven order invariants of a definition read its five date properties alone, and this " +
        "member replaces none of them")(
      quarterly.withBusinessDayAdjustment(BusinessDayAdjustment.NONE)),
    total(
      "PeriodicSchedule.withStartDateBusinessDayAdjustment",
      "it replaces an adjustment, which takes part in no invariant of the definition")(
      quarterly.withStartDateBusinessDayAdjustment(Some(BusinessDayAdjustment.NONE))),
    total(
      "PeriodicSchedule.withEndDateBusinessDayAdjustment",
      "it replaces an adjustment, which takes part in no invariant of the definition")(
      quarterly.withEndDateBusinessDayAdjustment(None)),
    total(
      "PeriodicSchedule.withStubConvention",
      "a stub is decided by rolling the schedule out, not by the definition's invariants, so " +
        "no convention makes a valid definition invalid")(
      quarterly.withStubConvention(Some(StubConvention.SHORT_INITIAL))),
    total(
      "PeriodicSchedule.withRollConvention",
      "a roll convention is read when the schedule is generated, not by the definition's " +
        "invariants")(
      quarterly.withRollConvention(Some(RollConventions.DAY_15))),
    rejectsWith("RollConvention.parse", FailureReason.PARSING)(
      RollConvention.parse("NotAConvention")),
    rejectsWith("RollConvention.ofDayOfMonth", FailureReason.INVALID)(
      RollConvention.ofDayOfMonth(32))
      .alsoAssertedBy("RollConvention.ofDayOfMonth reports a day outside one to thirty-one"),
    rejectsWith("Schedule.merge", FailureReason.INVALID)(
      schedule.merge(3, LocalDate.of(2014, 2, 1), OCT_15))
      .alsoAssertedBy("Schedule.merge reports a date that matches no period of the schedule"),
    rejectsWith("Schedule.mergeRegular", FailureReason.INVALID)(schedule.mergeRegular(0, true))
      .alsoAssertedBy("Schedule.mergeRegular reports an unusable group size"),
    rejectsWith("Schedule.toAdjusted", FailureReason.INVALID)(
      schedule.toAdjusted(collapsingAdjuster))
      .alsoAssertedBy("Schedule.toAdjusted reports a period that collapses once adjusted"),
    rejectsWith("Schedule.of", FailureReason.INVALID)(
      Schedule.of(
        NonEmptyList.of(schedule.period(1), schedule.period(0)),
        Frequency.P3M,
        RollConventions.DAY_15))
      .alsoAssertedBy("Schedule.of reports periods that do not run from earliest to latest"),
    total(
      "SchedulePeriod.subSchedule",
      "it derives a definition from the two unadjusted dates of this period, which " +
        "`SchedulePeriod.of` has already established to be strictly in order, and the frequency, " +
        "the conventions and the adjustment take no part in the invariants of a definition")(
      schedule.period(0).subSchedule(
        Frequency.P1M,
        RollConventions.DAY_15,
        StubConvention.NONE,
        BusinessDayAdjustment.NONE)),
    rejectsWith("SchedulePeriod.toAdjusted", FailureReason.INVALID)(
      schedule.period(1).toAdjusted(collapsingAdjuster)),
    rejectsWith("SchedulePeriod.of", FailureReason.INVALID)(SchedulePeriod.of(OCT_15, JAN_15))
      .alsoAssertedBy("SchedulePeriod.of reports dates that describe no period"),
    rejectsWith("StubConvention.parse", FailureReason.PARSING)(
      StubConvention.parse("NotAConvention")),
    rejectsWith("HalfUp.ofDecimalPlaces", FailureReason.INVALID)(HalfUp.ofDecimalPlaces(-1)),
    rejectsWith("HalfUp.ofFractionalDecimalPlaces", FailureReason.INVALID)(
      HalfUp.ofFractionalDecimalPlaces(-1, 257)),
    rejectsWith("Rounding.ofDecimalPlaces", FailureReason.INVALID)(Rounding.ofDecimalPlaces(256))
      .alsoAssertedBy("Rounding.ofDecimalPlaces reports a count outside zero to 255"),
    rejectsWith("Rounding.ofFractionalDecimalPlaces", FailureReason.INVALID)(
      Rounding.ofFractionalDecimalPlaces(-1, 257))
      .alsoAssertedBy("Rounding.ofFractionalDecimalPlaces accumulates both rejections"),
    rejectsWith("ValueAdjustmentType.parse", FailureReason.PARSING)(
      ValueAdjustmentType.parse("NotAType")),
    rejectsWith("ValueSchedule.resolveValues", FailureReason.INVALID)(
      accepted(ValueSchedule.of(100d, accepted(ValueStep.of(5, ValueAdjustment.ofReplace(200d)))))
        .resolveValues(schedule))
      .alsoAssertedBy("ValueSchedule.resolveValues reports a step the schedule cannot carry"),
    rejectsWith("ValueSchedule.withSteps", FailureReason.INVALID)(
      valueSchedule.withSteps(contradictorySteps)),
    total(
      "ValueSchedule.withStepSequence",
      "it re-runs the construction check, which reads the steps of the definition and not the " +
        "sequence, and the steps of an existing schedule have already passed it")(
      valueSchedule.withStepSequence(stepSequence)),
    rejectsWith("ValueSchedule.of", FailureReason.INVALID)(
      ValueSchedule.of(100d, contradictorySteps))
      .alsoAssertedBy("ValueSchedule.of reports two steps that name one position with different adjustments"),
    rejectsWith("ValueStep.of", FailureReason.INVALID)(
      ValueStep.of(0, ValueAdjustment.ofReplace(200d)))
      .alsoAssertedBy("ValueStep.of reports a position that names no period"),
    rejectsWith("ValueStepSequence.of", FailureReason.INVALID)(
      ValueStepSequence.of(OCT_15, JAN_15, Frequency.P3M, ValueAdjustment.ofDeltaAmount(-100d)))
      .alsoAssertedBy("ValueStepSequence.of reports arguments that describe no sequence"))

  private val failableRows: List[SurfaceRow] =
    collectFailableRows ::: basicsRootFailableRows ::: currencyFailableRows ::: dateFailableRows :::
      indexFailableRows ::: scheduleFailableRows

  //=========================================================================
  // THE THROWING SIDE
  //
  // One row per family whose scaladoc documents a throw, and the scaladoc above each group names
  // why those refusals are caller contracts or numeric edges rather than values. A family that
  // also has a failure-returning row above names the refusal in brackets, the two rows being two
  // statements about one member: `Money.plus` reports a currency mismatch and raises for an amount
  // no decimal holds. A family documenting two throws has a row for each.
  //=========================================================================

  /**
   * The rows of `ArgCheck`, the single place in either module where a throw is written.
   *
   * Each guards a caller contract: the condition is a property of the call - a count that must be
   * positive, a text that must match, two values that must be in order - and not of data a caller
   * could correct with better values. Every other contract refusal is routed through these.
   */
  private val argCheckThrowingRows: List[SurfaceRow] = List(
    raises[IllegalArgumentException]("ArgCheck.isTrue")(ArgCheck.isTrue(validIfTrue = false)),
    raises[IllegalArgumentException]("ArgCheck.isFalse")(
      ArgCheck.isFalse(validIfFalse = true, "The test value was true")),
    raises[IllegalArgumentException]("ArgCheck.matches")(
      ArgCheck.matches("[A-Z]+".r, "lower case", "name")),
    raises[IllegalArgumentException]("ArgCheck.notBlank")(ArgCheck.notBlank("   ", "name")),
    raises[IllegalArgumentException]("ArgCheck.notEmpty")(ArgCheck.notEmpty("", "name")),
    raises[IllegalArgumentException]("ArgCheck.noDuplicates")(
      ArgCheck.noDuplicates(Array(1d, 1d), "values")),
    raises[IllegalArgumentException]("ArgCheck.noDuplicatesSorted")(
      ArgCheck.noDuplicatesSorted(Array(2d, 1d), "values")),
    raises[IllegalArgumentException]("ArgCheck.notPositive")(ArgCheck.notPositive(1, "count")),
    raises[IllegalArgumentException]("ArgCheck.notPositiveIfPresent")(
      ArgCheck.notPositiveIfPresent(Some(two), "count")),
    raises[IllegalArgumentException]("ArgCheck.notNegative")(ArgCheck.notNegative(-1, "count")),
    raises[IllegalArgumentException]("ArgCheck.notNaN")(ArgCheck.notNaN(Double.NaN, "value")),
    raises[IllegalArgumentException]("ArgCheck.notNegativeOrZero")(
      ArgCheck.notNegativeOrZero(0, "count")),
    raises[IllegalArgumentException]("ArgCheck.notZero")(ArgCheck.notZero(0d, "value")),
    raises[IllegalArgumentException]("ArgCheck.inRange")(ArgCheck.inRange(5, 0, 5, "count")),
    raises[IllegalArgumentException]("ArgCheck.inRangeInclusive")(
      ArgCheck.inRangeInclusive(6, 0, 5, "count")),
    raises[IllegalArgumentException]("ArgCheck.inRangeExclusive")(
      ArgCheck.inRangeExclusive(5, 0, 5, "count")),
    raises[IllegalArgumentException]("ArgCheck.inRangeComparable")(
      ArgCheck.inRangeComparable(Currency.USD, Currency.AUD, Currency.EUR, "currency")),
    raises[IllegalArgumentException]("ArgCheck.inRangeComparableInclusive")(
      ArgCheck.inRangeComparableInclusive(Currency.USD, Currency.AUD, Currency.EUR, "currency")),
    raises[IllegalArgumentException]("ArgCheck.inRangeComparableExclusive")(
      ArgCheck.inRangeComparableExclusive(Currency.AUD, Currency.AUD, Currency.EUR, "currency")),
    raises[IllegalArgumentException]("ArgCheck.inOrderNotEqual")(
      ArgCheck.inOrderNotEqual(Currency.USD, Currency.AUD, "first", "second")),
    raises[IllegalArgumentException]("ArgCheck.inOrderOrEqual")(
      ArgCheck.inOrderOrEqual(Currency.USD, Currency.AUD, "first", "second")))

  /**
   * The rows of the numeric types of `strata-collect`.
   *
   * Two kinds of refusal live here, both fail-fast. A dimension or an index is the caller's own
   * arithmetic over sizes it can read, so an array of the wrong length or an index outside one is
   * a mistake in the calling code; and the overflow of the decimal representation beyond eighteen
   * digits is a numeric domain edge, reachable only from values a caller chose to combine.
   */
  private val numericThrowingRows: List[SurfaceRow] = List(
    raises[IllegalArgumentException]("Decimal.plus")(hugeDecimal.plus(hugeDecimal)),
    raises[IllegalArgumentException]("Decimal.minus")(
      hugeDecimal.minus(hugeDecimal.multipliedBy(-1L))),
    raises[IllegalArgumentException]("Decimal.multipliedBy")(
      hugeDecimal.multipliedBy(hugeDecimal)),
    raises[IllegalArgumentException]("Decimal.movePoint")(hugeDecimal.movePoint(1)),
    raises[ArithmeticException]("Decimal.dividedBy")(two.dividedBy(Decimal.ZERO)),
    raises[ArithmeticException]("Decimal.remainder")(two.remainder(Decimal.ZERO)),
    raises[IllegalArgumentException]("Decimal.roundToScale")(
      two.roundToScale(-18, RoundingMode.HALF_UP)),
    raises[IllegalArgumentException]("Decimal.roundToPrecision")(
      two.roundToPrecision(-1, RoundingMode.HALF_UP)),
    raises[IllegalArgumentException]("Decimal.format")(two.format(19, RoundingMode.HALF_UP)),
    raises[IllegalArgumentException]("Decimal.formatAtLeast")(two.formatAtLeast(19)),
    raises[IllegalArgumentException]("DoubleArrayMath.combineByAddition")(
      DoubleArrayMath.combineByAddition(Array(1d), Array(1d, 2d))),
    raises[IllegalArgumentException]("DoubleArrayMath.combineByMultiplication")(
      DoubleArrayMath.combineByMultiplication(Array(1d), Array(1d, 2d))),
    raises[IllegalArgumentException]("DoubleArrayMath.combine")(
      DoubleArrayMath.combine(Array(1d), Array(1d, 2d), (first, second) => first + second)),
    raises[IllegalArgumentException]("DoubleArrayMath.fuzzyEquals")(
      DoubleArrayMath.fuzzyEquals(1d, 1d, -1d)),
    raises[IllegalArgumentException]("DoubleArrayMath.fuzzyEqualsZero")(
      DoubleArrayMath.fuzzyEqualsZero(Array(1d), -1d)),
    raises[IllegalArgumentException]("DoubleArrayMath.reorderedCopy")(
      DoubleArrayMath.reorderedCopy(Array(1d), Array(0, 1))),
    raises[IllegalArgumentException]("DoubleArrayMath.sortPairs")(
      DoubleArrayMath.sortPairs(Array(1d), Array(1d, 2d))),
    raises[IndexOutOfBoundsException]("DoubleArray.get")(threeValues.get(3))
      .alsoAssertedBy("DoubleArray.get raises for an index outside the array"),
    raises[IllegalArgumentException]("DoubleArray.subArray")(threeValues.subArray(5)),
    raises[IllegalArgumentException]("DoubleArray.plus")(threeValues.plus(twoValues))
      .alsoAssertedBy("DoubleArray element-wise arithmetic raises for arrays of different sizes"),
    raises[IllegalArgumentException]("DoubleArray.minus")(threeValues.minus(twoValues))
      .alsoAssertedBy("DoubleArray element-wise arithmetic raises for arrays of different sizes"),
    raises[IllegalArgumentException]("DoubleArray.multipliedBy")(
      threeValues.multipliedBy(twoValues))
      .alsoAssertedBy("DoubleArray element-wise arithmetic raises for arrays of different sizes"),
    raises[IllegalArgumentException]("DoubleArray.dividedBy")(threeValues.dividedBy(twoValues))
      .alsoAssertedBy("DoubleArray element-wise arithmetic raises for arrays of different sizes"),
    raises[IllegalArgumentException]("DoubleArray.combine")(
      threeValues.combine(twoValues, (first, second) => first + second))
      .alsoAssertedBy("DoubleArray element-wise arithmetic raises for arrays of different sizes"),
    raises[IllegalArgumentException]("DoubleArray.combineReduce")(
      threeValues.combineReduce(
        twoValues,
        (total, first, second) => total + first * second)),
    raises[IllegalArgumentException]("DoubleArray.min")(DoubleArray.of().min)
      .alsoAssertedBy("DoubleArray element-wise arithmetic raises for arrays of different sizes"),
    raises[IllegalArgumentException]("DoubleArray.max")(DoubleArray.of().max)
      .alsoAssertedBy("DoubleArray element-wise arithmetic raises for arrays of different sizes"),
    raises[IllegalArgumentException]("DoubleArray.equalWithTolerance")(
      threeValues.equalWithTolerance(threeValues, -1d)),
    raises[IllegalArgumentException]("DoubleArray.equalZeroWithTolerance")(
      threeValues.equalZeroWithTolerance(-1d)),
    raises[IllegalArgumentException]("DoubleArray.tabulate")(
      DoubleArray.tabulate(-1)(index => index.toDouble)),
    raises[IllegalArgumentException]("DoubleArray.copyOf")(DoubleArray.copyOf(Array(1d), 5)),
    raises[IllegalArgumentException]("DoubleArray.filled")(DoubleArray.filled(-1)),
    raises[IndexOutOfBoundsException]("DoubleMatrix.get")(squareMatrix.get(2, 0))
      .alsoAssertedBy("DoubleMatrix.get raises for a position outside the matrix"),
    raises[IndexOutOfBoundsException]("DoubleMatrix.row")(squareMatrix.row(2)),
    raises[IndexOutOfBoundsException]("DoubleMatrix.rowArray")(squareMatrix.rowArray(2)),
    raises[IndexOutOfBoundsException]("DoubleMatrix.column")(squareMatrix.column(2)),
    raises[IndexOutOfBoundsException]("DoubleMatrix.columnArray")(squareMatrix.columnArray(2)),
    raises[IllegalArgumentException]("DoubleMatrix.plus")(squareMatrix.plus(flatMatrix))
      .alsoAssertedBy("DoubleMatrix element-wise arithmetic raises for matrices of different shapes"),
    raises[IllegalArgumentException]("DoubleMatrix.minus")(squareMatrix.minus(flatMatrix))
      .alsoAssertedBy("DoubleMatrix element-wise arithmetic raises for matrices of different shapes"),
    raises[IllegalArgumentException]("DoubleMatrix.combine")(
      squareMatrix.combine(flatMatrix, (first, second) => first * second))
      .alsoAssertedBy("DoubleMatrix element-wise arithmetic raises for matrices of different shapes"),
    raises[IllegalArgumentException]("DoubleMatrix.copyOf")(
      DoubleMatrix.copyOf(Array(Array(1.0, 2.0), Array(3.0)))),
    raises[IllegalArgumentException]("DoubleMatrix.of")(DoubleMatrix.of(-1, 2)),
    raises[IllegalArgumentException]("DoubleMatrix.tabulate")(
      DoubleMatrix.tabulate(-1, 1)((row, column) => (row + column).toDouble)),
    raises[IllegalArgumentException]("DoubleMatrix.ofArrays")(
      DoubleMatrix.ofArrays(-1, 1)(row => Array(row.toDouble))),
    raises[IllegalArgumentException]("DoubleMatrix.ofArrayObjects")(
      DoubleMatrix.ofArrayObjects(-1, 1)(row => DoubleArray.of(row.toDouble))),
    raises[IllegalArgumentException]("DoubleMatrix.filled")(DoubleMatrix.filled(-1, 1)),
    raises[IllegalArgumentException]("DoubleMatrix.identity")(DoubleMatrix.identity(-1)))

  /**
   * The rows of the amount types.
   *
   * The same numeric domain edges one layer up: an infinite amount is admitted, so a sum of
   * opposite infinities is the one input producing a value no amount type holds, and an amount of
   * eighteen digits is the one whose arithmetic overflows the decimal behind the money types. Both
   * are reachable only from a value a caller supplied, which is why they are refused rather than
   * reported. Each family is asserted once, so the throwing surface is complete here.
   */
  private val amountThrowingRows: List[SurfaceRow] = List(
    raises[IllegalArgumentException]("BigMoney.plus(eighteen digits)")(
      hugeBigMoney.plus(hugeBigMoney)),
    raises[IllegalArgumentException]("BigMoney.minus(eighteen digits)")(
      hugeBigMoney.minus(hugeBigMoney.multipliedBy(-1L))),
    raises[IllegalArgumentException]("BigMoney.multipliedBy")(hugeBigMoney.multipliedBy(2L)),
    raises[IllegalArgumentException]("BigMoney.roundToScale")(
      gbpBigMoney.roundToScale(-18, RoundingMode.HALF_UP)),
    raises[IllegalArgumentException]("BigMoney.convertedTo(eighteen digits)")(
      hugeBigMoney.convertedTo(Currency.USD, new BigDecimal("2"))),
    raises[IllegalArgumentException]("CurrencyAmount.plus(not a number)")(
      gbpInfinite.plus(gbpNegativeInfinite)),
    raises[IllegalArgumentException]("CurrencyAmount.minus(not a number)")(
      gbpInfinite.minus(gbpInfinite)),
    raises[IllegalArgumentException]("CurrencyAmount.multipliedBy")(gbpInfinite.multipliedBy(0d)),
    raises[IllegalArgumentException]("CurrencyAmount.mapAmount")(
      gbp100.mapAmount(_ => Double.NaN)),
    raises[IndexOutOfBoundsException]("CurrencyAmountArray.get(index outside the array)")(
      gbpArray.get(3)),
    raises[IllegalArgumentException]("CurrencyAmountArray.of(a value that is not a number)")(
      CurrencyAmountArray.of(Currency.GBP, DoubleArray.of(Double.NaN))),
    raises[IllegalArgumentException]("CurrencyAmountArray.multipliedBy")(
      infiniteArray.multipliedBy(0d)),
    raises[IllegalArgumentException]("CurrencyAmountArray.mapAmounts")(
      gbpArray.mapAmounts(_ => Double.NaN)),
    // FxRate.scala
    raises[IllegalArgumentException]("FxRate.inverse")(
      accepted(FxRate.of(Currency.GBP, Currency.USD, Double.PositiveInfinity)).inverse),
    raises[IllegalArgumentException]("FxRate.toConventional")(
      accepted(FxRate.of(Currency.USD, Currency.GBP, Double.PositiveInfinity)).toConventional),
    raises[IllegalArgumentException]("FxRateProvider.convert(eighteen digits)")(
      gbpUsdMatrix.convert(hugeDecimal, Currency.GBP, Currency.USD)),
    raises[IllegalArgumentException]("Money.plus(eighteen digits)")(hugeMoney.plus(hugeMoney)),
    raises[IllegalArgumentException]("Money.minus(eighteen digits)")(
      hugeMoney.minus(hugeMoney.multipliedBy(-1L))),
    raises[IllegalArgumentException]("Money.multipliedBy")(hugeMoney.multipliedBy(2L)),
    raises[IllegalArgumentException]("Money.convertedTo(eighteen digits)")(
      hugeMoney.convertedTo(Currency.USD, new BigDecimal("2"))),
    raises[IllegalArgumentException]("MultiCurrencyAmount.plus(not a number)")(
      multiInfinite.plus(multiNegativeInfinite)),
    raises[IllegalArgumentException]("MultiCurrencyAmount.minus(not a number)")(
      multiInfinite.minus(multiInfinite)),
    raises[IllegalArgumentException]("MultiCurrencyAmount.multipliedBy")(
      multiInfinite.multipliedBy(0d)),
    raises[IllegalArgumentException]("MultiCurrencyAmount.mapAmounts")(
      multiInfinite.mapAmounts(_ => Double.NaN)),
    raises[IllegalArgumentException]("MultiCurrencyAmount.mapCurrencyAmounts")(
      multiTwoInfinities.mapCurrencyAmounts(amount =>
        obtained(CurrencyAmount.of(Currency.GBP, amount.amount)))),
    raises[IllegalArgumentException]("MultiCurrencyAmount.total")(
      MultiCurrencyAmount.total(List(gbpInfinite, gbpNegativeInfinite))),
    raises[IndexOutOfBoundsException]("MultiCurrencyAmountArray.get(3)")(gbpRun.get(3)),
    raises[IndexOutOfBoundsException]("MultiCurrencyAmountArray.get(-1)")(gbpRun.get(-1)),
    raises[IllegalArgumentException]("MultiCurrencyAmountArray.of(negative size)")(
      MultiCurrencyAmountArray.of(-1, _ => gbpMulti)),
    raises[IllegalArgumentException]("MultiCurrencyAmountArray.multipliedBy")(
      infiniteRun.multipliedBy(0d)),
    raises[IllegalArgumentException]("MultiCurrencyAmountArray.mapAmounts")(
      gbpRun.mapAmounts(_ => Double.NaN)),
    // Payment.scala
    raises[DateTimeException]("Payment.adjustDate")(
      obtained(Payment.of(Currency.GBP, 100d, JAN_15)).adjustDate(_.plusYears(1000000000L))))

  /**
   * The rows of the date, schedule and value packages.
   *
   * A holiday calendar holds its holidays as an array of months from its first year, so a date
   * whose year falls outside 0 to 9999 is one no calendar could hold data for - a property of
   * where the argument falls on the time line, not of the holidays supplied. The same goes for a
   * reversed pair of dates, an index outside a schedule, and the schedule information a day count
   * is defined in terms of.
   */
  private val dateThrowingRows: List[SurfaceRow] = List(
    raisesWith[IllegalArgumentException]("BusinessDayConvention.adjust", UnsupportedDateMessage)(
      BusinessDayConventions.FOLLOWING.adjust(LocalDate.of(12000, 1, 15), datedCalendar)),
    raises[DateTimeException]("DateAdjuster.adjust")(oneDayLater.adjust(LocalDate.MAX)),
    raises[DateTimeException]("DateAdjuster.adjustInto")(oneDayLater.adjustInto(LocalDate.MAX)),
    raises[IllegalArgumentException]("DateSequence.nth")(DateSequences.QUARTERLY_IMM.nth(JAN_15, 0)),
    raises[IllegalArgumentException]("DateSequence.nthOrSame")(
      DateSequences.QUARTERLY_IMM.nthOrSame(JAN_15, 0)),
    raisesWith[IllegalArgumentException]("DayCount.yearFraction", "time-line order")(
      DayCounts.ACT_365F.yearFraction(APR_15, JAN_15))
      .alsoAssertedBy("DayCount.yearFraction raises for dates out of time-line order"),
    raises[IllegalArgumentException]("DayCount.relativeYearFraction")(
      DayCounts.ACT_ACT_ICMA.relativeYearFraction(JAN_15, APR_15))
      .alsoAssertedBy("DayCount Act/Act ICMA raises for schedule information it is not given"),
    raisesWith[IllegalArgumentException]("DayCount.days", "time-line order")(
      DayCounts.ACT_360.days(APR_15, JAN_15))
      .alsoAssertedBy("DayCount.yearFraction raises for dates out of time-line order"),
    raisesWith[IllegalArgumentException]("HolidayCalendar.isHoliday", UnsupportedDateMessage)(
      datedCalendar.isHoliday(LocalDate.MAX))
      .alsoAssertedBy("HolidayCalendar operations raise for a year outside zero to 9999"),
    raisesWith[IllegalArgumentException]("HolidayCalendar.isBusinessDay", UnsupportedDateMessage)(
      datedCalendar.isBusinessDay(LocalDate.MAX)),
    raisesWith[IllegalArgumentException]("HolidayCalendar.nextOrSame", UnsupportedDateMessage)(
      datedCalendar.nextOrSame(LocalDate.of(12000, 1, 15))),
    raisesWith[IllegalArgumentException]("HolidayCalendar.next", UnsupportedDateMessage)(
      datedCalendar.next(LocalDate.of(12000, 1, 15)))
      .alsoAssertedBy("HolidayCalendar operations raise for a year outside zero to 9999"),
    raisesWith[IllegalArgumentException]("HolidayCalendar.previousOrSame", UnsupportedDateMessage)(
      datedCalendar.previousOrSame(LocalDate.MIN)),
    raisesWith[IllegalArgumentException]("HolidayCalendar.previous", UnsupportedDateMessage)(
      datedCalendar.previous(LocalDate.MIN.plusDays(1L))),
    raisesWith[IllegalArgumentException](
      "HolidayCalendar.nextSameOrLastInMonth",
      UnsupportedDateMessage)(datedCalendar.nextSameOrLastInMonth(LocalDate.of(12000, 1, 15))),
    raisesWith[IllegalArgumentException](
      "HolidayCalendar.isLastBusinessDayOfMonth",
      UnsupportedDateMessage)(datedCalendar.isLastBusinessDayOfMonth(LocalDate.of(12000, 1, 15))),
    raisesWith[IllegalArgumentException](
      "HolidayCalendar.lastBusinessDayOfMonth",
      UnsupportedDateMessage)(datedCalendar.lastBusinessDayOfMonth(LocalDate.of(12000, 1, 15))),
    raisesWith[IllegalArgumentException]("HolidayCalendar.shift", UnsupportedDateMessage)(
      datedCalendar.shift(LocalDate.MIN, 1))
      .alsoAssertedBy("HolidayCalendar operations raise for a year outside zero to 9999"),
    raisesWith[IllegalArgumentException]("HolidayCalendar.daysBetween", UnsupportedDateMessage)(
      datedCalendar.daysBetween(LocalDate.MIN, JAN_15))
      .alsoAssertedBy("HolidayCalendar operations raise for a year outside zero to 9999"),
    raises[IllegalArgumentException]("HolidayCalendar.businessDays")(
      datedCalendar.businessDays(OCT_15, JAN_15)),
    raises[IllegalArgumentException]("HolidayCalendar.holidays")(
      datedCalendar.holidays(OCT_15, JAN_15)),
    raisesWith[IllegalArgumentException]("HolidayCalendar.shift(magnitude)", ShiftMagnitudeMessage)(
      datedCalendar.shift(JAN_15, Int.MaxValue))
      .alsoAssertedBy("HolidayCalendar refuses a shift larger than any search can satisfy"),
    raisesWith[IllegalArgumentException]("HolidayCalendar.adjustBy", ShiftMagnitudeMessage)(
      datedCalendar.adjustBy(Int.MinValue))
      .alsoAssertedBy("HolidayCalendar refuses a shift larger than any search can satisfy"),
    raisesWith[IllegalArgumentException]("HolidayCalendar.nextOrSame(no business day)", NoBusinessDayMessage)(
      alwaysClosedCalendar.nextOrSame(JAN_15))
      .alsoAssertedBy("HolidayCalendar refuses a search of a calendar that has no business day"),
    raisesWith[IllegalArgumentException](
      "HolidayCalendar.previousOrSame(no business day)",
      NoBusinessDayMessage)(alwaysClosedCalendar.previousOrSame(JAN_15))
      .alsoAssertedBy("HolidayCalendar refuses a search of a calendar that has no business day"),
    raisesWith[IllegalArgumentException]("HolidayCalendar.combinedWith", CompositeDepthMessage)(
      deepestCalendar.combinedWith(datedCalendar))
      .alsoAssertedBy("HolidayCalendar refuses a composite deeper than the family allows"),
    raisesWith[IllegalArgumentException]("HolidayCalendar.linkedWith", CompositeDepthMessage)(
      deepestCalendar.linkedWith(datedCalendar))
      .alsoAssertedBy("HolidayCalendar refuses a composite deeper than the family allows"),
    raises[IllegalArgumentException]("ImmutableHolidayCalendar.of")(
      ImmutableHolidayCalendar.of(
        HolidayCalendarId.of("TestFailableSurfaceOutOfRange"),
        List(LocalDate.MAX),
        List(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY),
        List.empty[LocalDate])),
    raisesWith[IllegalArgumentException](
      "ImmutableHolidayCalendar.of(all days closed)",
      NoBusinessDayMessage)(
      ImmutableHolidayCalendar.of(
        HolidayCalendarId.of("TestFailableSurfaceAllClosed"),
        List(LocalDate.of(2014, 1, 1)),
        DayOfWeek.values().toList,
        List.empty[LocalDate]))
      .alsoAssertedBy("ImmutableHolidayCalendar refuses a weekend that closes every day"),
    raisesWith[IllegalArgumentException](
      "ImmutableHolidayCalendar.combined",
      NoBusinessDayMessage)(
      ImmutableHolidayCalendar.combined(weekdayClosedCalendar, weekendClosedCalendar))
      .alsoAssertedBy("ImmutableHolidayCalendar refuses a weekend that closes every day"),
    // date/DaysAdjustment.scala
    raisesWith[IllegalArgumentException]("DaysAdjustment.ofBusinessDays", ShiftMagnitudeMessage)(
      DaysAdjustment.ofBusinessDays(Int.MaxValue, HolidayCalendarIds.GBLO))
      .alsoAssertedBy("DaysAdjustment.of reports a business day addition no calendar can walk"),
    // date/PeriodAdditionConvention.scala
    raisesWith[IllegalArgumentException]("PeriodAdditionConvention.adjust", UnsupportedDateMessage)(
      PeriodAdditionConventions.LAST_BUSINESS_DAY.adjust(
        LocalDate.of(12000, 1, 15),
        Period.ofMonths(1),
        datedCalendar)),
    raises[IllegalArgumentException]("Schedule.period")(schedule.period(9)),
    raises[IllegalArgumentException]("Schedule.merge(dates out of order)")(
      schedule.merge(3, OCT_15, JAN_15)),
    raises[IllegalArgumentException]("SchedulePeriod.yearFraction")(
      schedule.period(0).yearFraction(DayCounts.ACT_ACT_ICMA, DayCount.ScheduleInfo.simple)),
    raises[ArithmeticException]("HalfUp.round")(
      accepted(HalfUp.ofFractionalDecimalPlaces(0, 3)).round(new BigDecimal("0.5"))),
    raises[IndexOutOfBoundsException]("ValueDerivatives.getDerivative")(
      ValueDerivatives.of(1d, DoubleArray.of(1d, 2d)).getDerivative(5)))

  private val throwingRows: List[SurfaceRow] =
    argCheckThrowingRows ::: numericThrowingRows ::: amountThrowingRows :::
      dateThrowingRows

  /** Every row of the registry, by label, which is what a covered row is resolved through. */
  private lazy val registry: Map[String, SurfaceRow] =
    (failableRows ::: throwingRows).map(row => (row.label, row)).toMap

  //=========================================================================
  // THE GENERATED TESTS
  //
  // One test per row, named by the row's label and its kind: a row that carries no assertion is
  // not expressible, and a row whose call stops behaving as its kind records fails on its own line
  // rather than inside a table of a neighbouring member.
  //=========================================================================

  failableRows.foreach { row =>
    test(s"derived failable surface: ${row.label} [${row.establishment.kind}]") {
      row.check()
    }
  }

  throwingRows.foreach { row =>
    test(s"derived throwing surface: ${row.label} [${row.establishment.kind}]") {
      row.check()
    }
  }

  //=========================================================================
  // THE PER-SIGNATURE CLAIMS
  //
  // A row is written against an `Owner.method` family, and a family is one or more declarations:
  // `ValueSchedule.of` is five of them. A row alone therefore says nothing about which overload it
  // reached, and an overload added, removed or quietly turned total beside a failable sibling
  // would leave the registry's own assertions passing.
  //
  // The claims below close that gap. There is one per public declaration the enumeration finds -
  // keyed by owner, method and declared parameter types, so overloads are distinct - each naming
  // the row that accounts for it, and the two coverage assertions below compare the claims with
  // the enumeration in '''both''' directions at that granularity. So a declaration added to either
  // module is unclaimed, one whose channel or parameters change leaves its claim naming nothing,
  // and a claim pointing at a deleted or renamed row names nothing - each a failure here. A claim
  // is not a second assertion of the failure itself; a row makes that once for the family.
  //=========================================================================

  private val FailableSide: String = "failure-returning"

  private val ThrowingSide: String = "throw-documenting"

  /**
   * Every public declaration whose declared return type is a failure channel, each with the row
   * of the registry that accounts for it.
   *
   * Twelve signatures appear here and among the throwing claims below, because they do both -
   * `CurrencyAmount.plus(CurrencyAmount)` reports a currency mismatch as a value and documents the
   * throw its numeric edge raises - and each side is accounted for by the row establishing it.
   */
  private val failableClaims: List[DeclarationClaim] = List(
    DeclarationClaim("AdjustableDate.adjusted(ReferenceData)", "AdjustableDate.adjusted"),
    DeclarationClaim("AdjustableDates.adjusted(ReferenceData)", "AdjustableDates.adjusted"),
    DeclarationClaim("AdjustableDates.of(BusinessDayAdjustment,List[LocalDate])", "AdjustableDates.of"),
    DeclarationClaim("AdjustableDates.of(BusinessDayAdjustment,LocalDate,LocalDate*)", "AdjustableDates.of"),
    DeclarationClaim("AdjustableDates.of(BusinessDayAdjustment,NonEmptyList[LocalDate])", "AdjustableDates.of"),
    DeclarationClaim("AdjustableDates.of(List[LocalDate])", "AdjustableDates.of"),
    DeclarationClaim("AdjustableDates.of(LocalDate,LocalDate*)", "AdjustableDates.of"),
    DeclarationClaim("AdjustablePayment.of(Currency,Double,AdjustableDate)", "AdjustablePayment.of"),
    DeclarationClaim("AdjustablePayment.of(Currency,Double,LocalDate)", "AdjustablePayment.of"),
    DeclarationClaim("AdjustablePayment.resolve(ReferenceData)", "AdjustablePayment.resolve"),
    DeclarationClaim("BigMoney.convertedTo(Currency,BigDecimal)", "BigMoney.convertedTo"),
    DeclarationClaim("BigMoney.convertedTo(Currency,Decimal)", "BigMoney.convertedTo"),
    DeclarationClaim("BigMoney.convertedTo(Currency,FxRateProvider)", "BigMoney.convertedTo"),
    DeclarationClaim("BigMoney.isGreaterThan(BigMoney)", "BigMoney.isGreaterThan"),
    DeclarationClaim("BigMoney.isGreaterThanEqualTo(BigMoney)", "BigMoney.isGreaterThanEqualTo"),
    DeclarationClaim("BigMoney.isLessThan(BigMoney)", "BigMoney.isLessThan"),
    DeclarationClaim("BigMoney.isLessThanEqualTo(BigMoney)", "BigMoney.isLessThanEqualTo"),
    DeclarationClaim("BigMoney.mapAmount(BigDecimal=>BigDecimal)", "BigMoney.mapAmount"),
    DeclarationClaim("BigMoney.minus(BigMoney)", "BigMoney.minus"),
    DeclarationClaim("BigMoney.of(Currency,BigDecimal)", "BigMoney.of"),
    DeclarationClaim("BigMoney.of(Currency,Double)", "BigMoney.of"),
    DeclarationClaim("BigMoney.of(CurrencyAmount)", "BigMoney.of"),
    DeclarationClaim("BigMoney.parse(String)", "BigMoney.parse"),
    DeclarationClaim("BigMoney.plus(BigMoney)", "BigMoney.plus"),
    DeclarationClaim("BusinessDayAdjustment.adjust(LocalDate,ReferenceData)", "BusinessDayAdjustment.adjust"),
    DeclarationClaim("BusinessDayAdjustment.resolve(ReferenceData)", "BusinessDayAdjustment.resolve"),
    DeclarationClaim("BusinessDayConvention.parse(String)", "BusinessDayConvention.parse"),
    DeclarationClaim("Collections.ensureOnlyOne(IterableOnce[A])", "Collections.ensureOnlyOne"),
    DeclarationClaim("Collections.ensureOnlyOne(IterableOnce[A],=>String)", "Collections.ensureOnlyOne"),
    DeclarationClaim("Collections.toSortedMap(IterableOnce[A],A=>K)", "Collections.toSortedMap"),
    DeclarationClaim("Collections.toSortedMap(IterableOnce[A],A=>K,A=>V)", "Collections.toSortedMap"),
    DeclarationClaim("Country.code3Char()", "Country.code3Char"),
    DeclarationClaim("Country.of(String)", "Country.of"),
    DeclarationClaim("Country.of3Char(String)", "Country.of3Char"),
    DeclarationClaim("Country.parse(String)", "Country.parse"),
    DeclarationClaim("Currency.of(String)", "Currency.of"),
    DeclarationClaim("Currency.parse(String)", "Currency.parse"),
    DeclarationClaim("CurrencyAmount.convertedTo(Currency,Double)", "CurrencyAmount.convertedTo"),
    DeclarationClaim("CurrencyAmount.convertedTo(Currency,FxRateProvider)", "CurrencyAmount.convertedTo"),
    DeclarationClaim("CurrencyAmount.minus(CurrencyAmount)", "CurrencyAmount.minus"),
    DeclarationClaim("CurrencyAmount.of(Currency,Double)", "CurrencyAmount.of"),
    DeclarationClaim("CurrencyAmount.of(String,Double)", "CurrencyAmount.of"),
    DeclarationClaim("CurrencyAmount.parse(String)", "CurrencyAmount.parse"),
    DeclarationClaim("CurrencyAmount.plus(CurrencyAmount)", "CurrencyAmount.plus"),
    DeclarationClaim("CurrencyAmount.toBigMoney()", "CurrencyAmount.toBigMoney"),
    DeclarationClaim("CurrencyAmount.toMoney()", "CurrencyAmount.toMoney"),
    DeclarationClaim("CurrencyAmountArray.convertedTo(Currency,FxRateProvider)", "CurrencyAmountArray.convertedTo"),
    DeclarationClaim("CurrencyAmountArray.minus(CurrencyAmount)", "CurrencyAmountArray.minus"),
    DeclarationClaim("CurrencyAmountArray.minus(CurrencyAmountArray)", "CurrencyAmountArray.minus"),
    DeclarationClaim("CurrencyAmountArray.of(Int,Int=>CurrencyAmount)", "CurrencyAmountArray.of"),
    DeclarationClaim("CurrencyAmountArray.of(Iterable[CurrencyAmount])", "CurrencyAmountArray.of"),
    DeclarationClaim("CurrencyAmountArray.plus(CurrencyAmount)", "CurrencyAmountArray.plus"),
    DeclarationClaim("CurrencyAmountArray.plus(CurrencyAmountArray)", "CurrencyAmountArray.plus"),
    DeclarationClaim("CurrencyPair.other(Currency)", "CurrencyPair.other"),
    DeclarationClaim("CurrencyPair.parse(String)", "CurrencyPair.parse"),
    DeclarationClaim("DateSequence.parse(String)", "DateSequence.parse"),
    DeclarationClaim("DayCount.ofBus252(HolidayCalendarId,ReferenceData)", "DayCount.ofBus252"),
    DeclarationClaim("DayCount.parse(String)", "DayCount.parse"),
    DeclarationClaim("DayCount.parse(String,ReferenceData)", "DayCount.parse"),
    DeclarationClaim("DaysAdjustment.adjust(LocalDate,ReferenceData)", "DaysAdjustment.adjust"),
    DeclarationClaim("DaysAdjustment.of(Int,HolidayCalendarId,BusinessDayAdjustment)", "DaysAdjustment.of"),
    DeclarationClaim("DaysAdjustment.resolve(ReferenceData)", "DaysAdjustment.resolve"),
    DeclarationClaim("Decimal.mapAsBigDecimal(BigDecimal=>BigDecimal)", "Decimal.mapAsBigDecimal"),
    DeclarationClaim("Decimal.mapAsDouble(Double=>Double)", "Decimal.mapAsDouble"),
    DeclarationClaim("Decimal.of(BigDecimal)", "Decimal.of"),
    DeclarationClaim("Decimal.of(Double)", "Decimal.of"),
    DeclarationClaim("Decimal.of(Long)", "Decimal.of"),
    DeclarationClaim("Decimal.of(String)", "Decimal.of"),
    DeclarationClaim("Decimal.ofScaled(Long,Int)", "Decimal.ofScaled"),
    DeclarationClaim("Decimal.parse(String)", "Decimal.parse"),
    DeclarationClaim("Decimal.toFixedScale(Int)", "Decimal.toFixedScale"),
    DeclarationClaim("FailureReason.parse(String)", "FailureReason.parse"),
    DeclarationClaim("FixedScaleDecimal.map(Decimal=>Decimal)", "FixedScaleDecimal.map"),
    DeclarationClaim("FixedScaleDecimal.of(Decimal,Int)", "FixedScaleDecimal.of"),
    DeclarationClaim("FixedScaleDecimal.parse(String)", "FixedScaleDecimal.parse"),
    DeclarationClaim("FloatingRate.parse(String)", "FloatingRate.parse"),
    DeclarationClaim("FloatingRateIndex.parse(String)", "FloatingRateIndex.parse"),
    DeclarationClaim("FloatingRateIndex.parse(String,Tenor)", "FloatingRateIndex.parse"),
    DeclarationClaim("FloatingRateName.currency()", "FloatingRateName.currency"),
    DeclarationClaim("FloatingRateName.defaultIborIndex(Currency)", "FloatingRateName.defaultIborIndex"),
    DeclarationClaim("FloatingRateName.defaultOvernightIndex(Currency)", "FloatingRateName.defaultOvernightIndex"),
    DeclarationClaim("FloatingRateName.defaultTenor()", "FloatingRateName.defaultTenor"),
    DeclarationClaim("FloatingRateName.normalized()", "FloatingRateName.normalized"),
    DeclarationClaim("FloatingRateName.parse(String)", "FloatingRateName.parse"),
    DeclarationClaim("FloatingRateName.toFloatingRateIndex()", "FloatingRateName.toFloatingRateIndex"),
    DeclarationClaim("FloatingRateName.toFloatingRateIndex(Tenor)", "FloatingRateName.toFloatingRateIndex"),
    DeclarationClaim("FloatingRateName.toIborIndex(Tenor)", "FloatingRateName.toIborIndex"),
    DeclarationClaim("FloatingRateName.toIborIndexFixingOffset()", "FloatingRateName.toIborIndexFixingOffset"),
    DeclarationClaim("FloatingRateName.toOvernightIndex()", "FloatingRateName.toOvernightIndex"),
    DeclarationClaim("FloatingRateName.toPriceIndex()", "FloatingRateName.toPriceIndex"),
    DeclarationClaim("FloatingRateType.parse(String)", "FloatingRateType.parse"),
    DeclarationClaim("Frequency.eventsPerYear()", "Frequency.eventsPerYear"),
    DeclarationClaim("Frequency.exactDivide(Frequency)", "Frequency.exactDivide"),
    DeclarationClaim("Frequency.of(Period)", "Frequency.of"),
    DeclarationClaim("Frequency.ofDays(Int)", "Frequency.ofDays"),
    DeclarationClaim("Frequency.ofMonths(Int)", "Frequency.ofMonths"),
    DeclarationClaim("Frequency.ofWeeks(Int)", "Frequency.ofWeeks"),
    DeclarationClaim("Frequency.ofYears(Int)", "Frequency.ofYears"),
    DeclarationClaim("Frequency.parse(String)", "Frequency.parse"),
    DeclarationClaim("FxConvertible.convertedTo(Currency,FxRateProvider)", "FxConvertible.convertedTo"),
    DeclarationClaim("FxIndex.calculateFixingFromMaturity(LocalDate,ReferenceData)", "FxIndex.calculateFixingFromMaturity"),
    DeclarationClaim("FxIndex.calculateMaturityFromFixing(LocalDate,ReferenceData)", "FxIndex.calculateMaturityFromFixing"),
    DeclarationClaim("FxIndex.of(CurrencyPair)", "FxIndex.of"),
    DeclarationClaim("FxIndex.of(String)", "FxIndex.of"),
    DeclarationClaim("FxIndex.parse(String)", "FxIndex.parse"),
    DeclarationClaim("FxIndex.resolve(ReferenceData)", "FxIndex.resolve"),
    DeclarationClaim("FxIndexObservation.of(FxIndex,LocalDate,ReferenceData)", "FxIndexObservation.of"),
    DeclarationClaim("FxMatrix.convert(CurrencyAmount,Currency)", "FxMatrix.convert"),
    DeclarationClaim("FxMatrix.convert(MultiCurrencyAmount,Currency)", "FxMatrix.convert"),
    DeclarationClaim("FxMatrix.fromMatrix(Vector[Currency],DoubleMatrix)", "FxMatrix.fromMatrix"),
    DeclarationClaim("FxMatrix.fxRate(Currency,Currency)", "FxMatrix.fxRate"),
    DeclarationClaim("FxMatrix.merge(FxMatrix)", "FxMatrix.merge"),
    DeclarationClaim("FxMatrix.of(Iterable[FxRate])", "FxMatrix.of"),
    DeclarationClaim("FxMatrix.ofRates(Iterable[(CurrencyPair,Double)])", "FxMatrix.ofRates"),
    DeclarationClaim("FxMatrix.withRate(Currency,Currency,Double)", "FxMatrix.withRate"),
    DeclarationClaim("FxMatrix.withRate(CurrencyPair,Double)", "FxMatrix.withRate"),
    DeclarationClaim("FxMatrix.withRates(Iterable[(CurrencyPair,Double)])", "FxMatrix.withRates"),
    DeclarationClaim("FxRate.crossRate(FxRate)", "FxRate.crossRate"),
    DeclarationClaim("FxRate.fxRate(Currency,Currency)", "FxRate.fxRate"),
    DeclarationClaim("FxRate.of(Currency,Currency,Double)", "FxRate.of"),
    DeclarationClaim("FxRate.of(CurrencyPair,Double)", "FxRate.of"),
    DeclarationClaim("FxRate.parse(String)", "FxRate.parse"),
    DeclarationClaim("FxRateProvider.convert(Decimal,Currency,Currency)", "FxRateProvider.convert"),
    DeclarationClaim("FxRateProvider.convert(Double,Currency,Currency)", "FxRateProvider.convert"),
    DeclarationClaim("FxRateProvider.fxRate(Currency,Currency)", "FxRateProvider.fxRate"),
    DeclarationClaim("FxRateProvider.fxRate(CurrencyPair)", "FxRateProvider.fxRate"),
    DeclarationClaim("HalfUp.ofDecimalPlaces(Int)", "HalfUp.ofDecimalPlaces"),
    DeclarationClaim("HalfUp.ofFractionalDecimalPlaces(Int,Int)", "HalfUp.ofFractionalDecimalPlaces"),
    DeclarationClaim("HolidayCalendarId.resolve(ReferenceData)", "HolidayCalendarId.resolve"),
    DeclarationClaim("HolidayCalendars.of(String)", "HolidayCalendars.of"),
    DeclarationClaim("IborIndex.calculateEffectiveFromFixing(LocalDate,ReferenceData)", "IborIndex.calculateEffectiveFromFixing"),
    DeclarationClaim("IborIndex.calculateFixingFromEffective(LocalDate,ReferenceData)", "IborIndex.calculateFixingFromEffective"),
    DeclarationClaim("IborIndex.calculateMaturityFromEffective(LocalDate,ReferenceData)", "IborIndex.calculateMaturityFromEffective"),
    DeclarationClaim("IborIndex.calculateMaturityFromFixing(LocalDate,ReferenceData)", "IborIndex.calculateMaturityFromFixing"),
    DeclarationClaim("IborIndex.parse(String)", "IborIndex.parse"),
    DeclarationClaim("IborIndex.resolve(ReferenceData)", "IborIndex.resolve"),
    DeclarationClaim("IborIndexObservation.of(IborIndex,LocalDate,ReferenceData)", "IborIndexObservation.of"),
    DeclarationClaim("IborIndexObservation.resolve(IborIndex,ReferenceData)", "IborIndexObservation.resolve"),
    DeclarationClaim("FxIndexObservation.resolve(FxIndex,ReferenceData)", "FxIndexObservation.resolve"),
    DeclarationClaim("OvernightIndexObservation.resolve(OvernightIndex,ReferenceData)",
      "OvernightIndexObservation.resolve"),
    DeclarationClaim("ImmutableReferenceData.of(ReferenceData.Entry[_]*)", "ImmutableReferenceData.of"),
    DeclarationClaim("Index.parse(String)", "Index.parse"),
    DeclarationClaim("MarketTenor.ofSpot(Tenor)", "MarketTenor.ofSpot"),
    DeclarationClaim("MarketTenor.ofSpotDays(Int)", "MarketTenor.ofSpotDays"),
    DeclarationClaim("MarketTenor.ofSpotMonths(Int)", "MarketTenor.ofSpotMonths"),
    DeclarationClaim("MarketTenor.ofSpotYears(Int)", "MarketTenor.ofSpotYears"),
    DeclarationClaim("MarketTenor.parse(String)", "MarketTenor.parse"),
    DeclarationClaim("Money.convertedTo(Currency,BigDecimal)", "Money.convertedTo"),
    DeclarationClaim("Money.convertedTo(Currency,Decimal)", "Money.convertedTo"),
    DeclarationClaim("Money.convertedTo(Currency,FxRateProvider)", "Money.convertedTo"),
    DeclarationClaim("Money.getValue()", "Money.getValue"),
    DeclarationClaim("Money.mapAmount(BigDecimal=>BigDecimal)", "Money.mapAmount"),
    DeclarationClaim("Money.minus(Money)", "Money.minus"),
    DeclarationClaim("Money.of(Currency,BigDecimal)", "Money.of"),
    DeclarationClaim("Money.of(Currency,Double)", "Money.of"),
    DeclarationClaim("Money.of(CurrencyAmount)", "Money.of"),
    DeclarationClaim("Money.parse(String)", "Money.parse"),
    DeclarationClaim("Money.plus(Money)", "Money.plus"),
    DeclarationClaim("MultiCurrencyAmount.convertedTo(Currency,FxRateProvider)", "MultiCurrencyAmount.convertedTo"),
    DeclarationClaim("MultiCurrencyAmount.getAmount(Currency)", "MultiCurrencyAmount.getAmount"),
    DeclarationClaim("MultiCurrencyAmount.of(Currency,Double)", "MultiCurrencyAmount.of"),
    DeclarationClaim("MultiCurrencyAmount.of(CurrencyAmount*)", "MultiCurrencyAmount.of"),
    DeclarationClaim("MultiCurrencyAmount.of(Iterable[CurrencyAmount])", "MultiCurrencyAmount.of"),
    DeclarationClaim("MultiCurrencyAmount.of(Map[Currency,Double])", "MultiCurrencyAmount.of"),
    DeclarationClaim("MultiCurrencyAmountArray.convertedTo(Currency,FxRateProvider)", "MultiCurrencyAmountArray.convertedTo"),
    DeclarationClaim("MultiCurrencyAmountArray.getValues(Currency)", "MultiCurrencyAmountArray.getValues"),
    DeclarationClaim("MultiCurrencyAmountArray.minus(MultiCurrencyAmount)", "MultiCurrencyAmountArray.minus(MultiCurrencyAmount)"),
    DeclarationClaim("MultiCurrencyAmountArray.minus(MultiCurrencyAmountArray)", "MultiCurrencyAmountArray.minus(MultiCurrencyAmountArray)"),
    DeclarationClaim("MultiCurrencyAmountArray.of(Map[Currency,DoubleArray])", "MultiCurrencyAmountArray.of"),
    DeclarationClaim("MultiCurrencyAmountArray.plus(MultiCurrencyAmount)", "MultiCurrencyAmountArray.plus(MultiCurrencyAmount)"),
    DeclarationClaim("MultiCurrencyAmountArray.plus(MultiCurrencyAmountArray)", "MultiCurrencyAmountArray.plus(MultiCurrencyAmountArray)"),
    DeclarationClaim("MultiCurrencyAmountArray.total(Iterable[CurrencyAmountArray])", "MultiCurrencyAmountArray.total"),
    DeclarationClaim("NamedEnum.parse(String)", "NamedEnum.parse"),
    DeclarationClaim("OvernightIndex.calculateEffectiveFromFixing(LocalDate,ReferenceData)", "OvernightIndex.calculateEffectiveFromFixing"),
    DeclarationClaim("OvernightIndex.calculateFixingFromEffective(LocalDate,ReferenceData)", "OvernightIndex.calculateFixingFromEffective"),
    DeclarationClaim("OvernightIndex.calculateMaturityFromEffective(LocalDate,ReferenceData)", "OvernightIndex.calculateMaturityFromEffective"),
    DeclarationClaim("OvernightIndex.calculateMaturityFromFixing(LocalDate,ReferenceData)", "OvernightIndex.calculateMaturityFromFixing"),
    DeclarationClaim("OvernightIndex.calculatePublicationFromFixing(LocalDate,ReferenceData)", "OvernightIndex.calculatePublicationFromFixing"),
    DeclarationClaim("OvernightIndex.parse(String)", "OvernightIndex.parse"),
    DeclarationClaim("OvernightIndexObservation.of(OvernightIndex,LocalDate,ReferenceData)", "OvernightIndexObservation.of"),
    DeclarationClaim("Payment.convertedTo(Currency,FxRateProvider)", "Payment.convertedTo"),
    DeclarationClaim("Payment.of(Currency,Double,LocalDate)", "Payment.of"),
    DeclarationClaim("PeriodAdditionConvention.parse(String)", "PeriodAdditionConvention.parse"),
    DeclarationClaim("PeriodAdjustment.adjust(LocalDate,ReferenceData)", "PeriodAdjustment.adjust"),
    DeclarationClaim("PeriodAdjustment.of(Period,PeriodAdditionConvention,BusinessDayAdjustment)", "PeriodAdjustment.of"),
    DeclarationClaim("PeriodAdjustment.ofLastBusinessDay(Period,BusinessDayAdjustment)", "PeriodAdjustment.ofLastBusinessDay"),
    DeclarationClaim("PeriodAdjustment.ofLastDay(Period,BusinessDayAdjustment)", "PeriodAdjustment.ofLastDay"),
    DeclarationClaim("PeriodAdjustment.resolve(ReferenceData)", "PeriodAdjustment.resolve"),
    DeclarationClaim("PeriodicSchedule.createAdjustedDates(ReferenceData)", "PeriodicSchedule.createAdjustedDates"),
    DeclarationClaim("PeriodicSchedule.createSchedule(ReferenceData)", "PeriodicSchedule.createSchedule"),
    DeclarationClaim("PeriodicSchedule.createSchedule(ReferenceData,Boolean)", "PeriodicSchedule.createSchedule"),
    DeclarationClaim("PeriodicSchedule.createUnadjustedDates()", "PeriodicSchedule.createUnadjustedDates"),
    DeclarationClaim("PeriodicSchedule.createUnadjustedDates(ReferenceData)", "PeriodicSchedule.createUnadjustedDates"),
    DeclarationClaim("PeriodicSchedule.of(LocalDate,LocalDate,Frequency,BusinessDayAdjustment)", "PeriodicSchedule.of"),
    DeclarationClaim("PeriodicSchedule.of(LocalDate,LocalDate,Frequency,BusinessDayAdjustment,Option[BusinessDayAdjustment],Option[BusinessDayAdjustment],Option[StubConvention],Option[RollConvention],Option[LocalDate],Option[LocalDate],Option[AdjustableDate])", "PeriodicSchedule.of"),
    DeclarationClaim("PeriodicSchedule.of(LocalDate,LocalDate,Frequency,BusinessDayAdjustment,StubConvention,Boolean)", "PeriodicSchedule.of"),
    DeclarationClaim("PeriodicSchedule.of(LocalDate,LocalDate,Frequency,BusinessDayAdjustment,StubConvention,RollConvention)", "PeriodicSchedule.of"),
    DeclarationClaim("PeriodicSchedule.replaceStartDate(LocalDate)", "PeriodicSchedule.replaceStartDate"),
    DeclarationClaim("PeriodicSchedule.withBusinessDayAdjustment(BusinessDayAdjustment)", "PeriodicSchedule.withBusinessDayAdjustment"),
    DeclarationClaim("PeriodicSchedule.withEndDate(LocalDate)", "PeriodicSchedule.withEndDate"),
    DeclarationClaim("PeriodicSchedule.withEndDateBusinessDayAdjustment(Option[BusinessDayAdjustment])", "PeriodicSchedule.withEndDateBusinessDayAdjustment"),
    DeclarationClaim("PeriodicSchedule.withFirstRegularStartDate(Option[LocalDate])", "PeriodicSchedule.withFirstRegularStartDate"),
    DeclarationClaim("PeriodicSchedule.withLastRegularEndDate(Option[LocalDate])", "PeriodicSchedule.withLastRegularEndDate"),
    DeclarationClaim("PeriodicSchedule.withOverrideStartDate(Option[AdjustableDate])", "PeriodicSchedule.withOverrideStartDate"),
    DeclarationClaim("PeriodicSchedule.withRollConvention(Option[RollConvention])", "PeriodicSchedule.withRollConvention"),
    DeclarationClaim("PeriodicSchedule.withStartDate(LocalDate)", "PeriodicSchedule.withStartDate"),
    DeclarationClaim("PeriodicSchedule.withStartDateBusinessDayAdjustment(Option[BusinessDayAdjustment])", "PeriodicSchedule.withStartDateBusinessDayAdjustment"),
    DeclarationClaim("PeriodicSchedule.withStubConvention(Option[StubConvention])", "PeriodicSchedule.withStubConvention"),
    DeclarationClaim("PriceIndex.parse(String)", "PriceIndex.parse"),
    DeclarationClaim("RateIndex.parse(String)", "RateIndex.parse"),
    DeclarationClaim("ReferenceData.getValue(ReferenceDataId[T])", "ReferenceData.getValue"),
    DeclarationClaim("ReferenceData.of(Entry[_]*)", "ReferenceData.of"),
    DeclarationClaim("ReferenceDataId.resolve(ReferenceData)", "ReferenceDataId.resolve"),
    DeclarationClaim("Resolvable.resolve(ReferenceData)", "Resolvable.resolve"),
    DeclarationClaim("ResolvableCalculationTarget.resolveTarget(ReferenceData)", "ResolvableCalculationTarget.resolveTarget"),
    DeclarationClaim("RollConvention.ofDayOfMonth(Int)", "RollConvention.ofDayOfMonth"),
    DeclarationClaim("RollConvention.parse(String)", "RollConvention.parse"),
    DeclarationClaim("Rounding.ofDecimalPlaces(Int)", "Rounding.ofDecimalPlaces"),
    DeclarationClaim("Rounding.ofFractionalDecimalPlaces(Int,Int)", "Rounding.ofFractionalDecimalPlaces"),
    DeclarationClaim("Schedule.merge(Int,LocalDate,LocalDate)", "Schedule.merge"),
    DeclarationClaim("Schedule.mergeRegular(Int,Boolean)", "Schedule.mergeRegular"),
    DeclarationClaim("Schedule.of(NonEmptyList[SchedulePeriod],Frequency,RollConvention)", "Schedule.of"),
    DeclarationClaim("Schedule.toAdjusted(DateAdjuster)", "Schedule.toAdjusted"),
    DeclarationClaim("SchedulePeriod.of(LocalDate,LocalDate)", "SchedulePeriod.of"),
    DeclarationClaim("SchedulePeriod.of(LocalDate,LocalDate,LocalDate,LocalDate)", "SchedulePeriod.of"),
    DeclarationClaim("SchedulePeriod.subSchedule(Frequency,RollConvention,StubConvention,BusinessDayAdjustment)", "SchedulePeriod.subSchedule"),
    DeclarationClaim("SchedulePeriod.toAdjusted(DateAdjuster)", "SchedulePeriod.toAdjusted"),
    DeclarationClaim("SequenceDate.base(Int)", "SequenceDate.base"),
    DeclarationClaim("SequenceDate.base(Period,Int)", "SequenceDate.base"),
    DeclarationClaim("SequenceDate.base(YearMonth)", "SequenceDate.base"),
    DeclarationClaim("SequenceDate.base(YearMonth,Int)", "SequenceDate.base"),
    DeclarationClaim("SequenceDate.full(Int)", "SequenceDate.full"),
    DeclarationClaim("SequenceDate.full(Period,Int)", "SequenceDate.full"),
    DeclarationClaim("SequenceDate.full(YearMonth)", "SequenceDate.full"),
    DeclarationClaim("SequenceDate.full(YearMonth,Int)", "SequenceDate.full"),
    DeclarationClaim("SequenceDate.of(Option[YearMonth],Option[Period],Int,Boolean)", "SequenceDate.of"),
    DeclarationClaim("StandardId.of(String,String)", "StandardId.of"),
    DeclarationClaim("StandardId.parse(String)", "StandardId.parse"),
    DeclarationClaim("StandardSchemes.createTicMic(String,String)", "StandardSchemes.createTicMic"),
    DeclarationClaim("StandardSchemes.splitTicMic(StandardId)", "StandardSchemes.splitTicMic"),
    DeclarationClaim("StubConvention.parse(String)", "StubConvention.parse"),
    DeclarationClaim("Tenor.of(Period)", "Tenor.of"),
    DeclarationClaim("Tenor.ofDays(Int)", "Tenor.ofDays"),
    DeclarationClaim("Tenor.ofMonths(Int)", "Tenor.ofMonths"),
    DeclarationClaim("Tenor.ofWeeks(Int)", "Tenor.ofWeeks"),
    DeclarationClaim("Tenor.ofYears(Int)", "Tenor.ofYears"),
    DeclarationClaim("Tenor.parse(String)", "Tenor.parse"),
    DeclarationClaim("TenorAdjustment.adjust(LocalDate,ReferenceData)", "TenorAdjustment.adjust"),
    DeclarationClaim("TenorAdjustment.of(Tenor,PeriodAdditionConvention,BusinessDayAdjustment)", "TenorAdjustment.of"),
    DeclarationClaim("TenorAdjustment.ofLastBusinessDay(Tenor,BusinessDayAdjustment)", "TenorAdjustment.ofLastBusinessDay"),
    DeclarationClaim("TenorAdjustment.ofLastDay(Tenor,BusinessDayAdjustment)", "TenorAdjustment.ofLastDay"),
    DeclarationClaim("TenorAdjustment.resolve(ReferenceData)", "TenorAdjustment.resolve"),
    DeclarationClaim("TypedStringCompanion.of(String)", "TypedStringCompanion.of"),
    DeclarationClaim("Validate.cond(Boolean,=>A,=>Failure)", "Validate.cond"),
    DeclarationClaim("Validate.fromResult(FailureOr[A])", "Validate.fromResult"),
    DeclarationClaim("Validate.inOrderNotEqual(T,T,String,String,Order[T])", "Validate.inOrderNotEqual"),
    DeclarationClaim("Validate.inOrderOrEqual(T,T,String,String,Order[T])", "Validate.inOrderOrEqual"),
    DeclarationClaim("Validate.inRange(Double,Double,Double,String)", "Validate.inRange"),
    DeclarationClaim("Validate.inRange(Int,Int,Int,String)", "Validate.inRange"),
    DeclarationClaim("Validate.inRangeComparable(T,T,T,String,Order[T])", "Validate.inRangeComparable"),
    DeclarationClaim("Validate.inRangeComparableExclusive(T,T,T,String,Order[T])", "Validate.inRangeComparableExclusive"),
    DeclarationClaim("Validate.inRangeComparableInclusive(T,T,T,String,Order[T])", "Validate.inRangeComparableInclusive"),
    DeclarationClaim("Validate.inRangeExclusive(Double,Double,Double,String)", "Validate.inRangeExclusive"),
    DeclarationClaim("Validate.inRangeExclusive(Int,Int,Int,String)", "Validate.inRangeExclusive"),
    DeclarationClaim("Validate.inRangeInclusive(Double,Double,Double,String)", "Validate.inRangeInclusive"),
    DeclarationClaim("Validate.inRangeInclusive(Int,Int,Int,String)", "Validate.inRangeInclusive"),
    DeclarationClaim("Validate.invalid(Failure)", "Validate.invalid"),
    DeclarationClaim("Validate.invalidNec(String)", "Validate.invalidNec"),
    DeclarationClaim("Validate.isFalse(Boolean,=>String)", "Validate.isFalse"),
    DeclarationClaim("Validate.isTrue(Boolean)", "Validate.isTrue"),
    DeclarationClaim("Validate.isTrue(Boolean,=>String)", "Validate.isTrue"),
    DeclarationClaim("Validate.matches(Char=>Boolean,Int,Int,String,String,String)", "Validate.matches"),
    DeclarationClaim("Validate.matches(Regex,String,String)", "Validate.matches"),
    DeclarationClaim("Validate.noDuplicates(Array[Double],String)", "Validate.noDuplicates"),
    DeclarationClaim("Validate.noDuplicatesSorted(Array[Double],String)", "Validate.noDuplicatesSorted"),
    DeclarationClaim("Validate.notBlank(String,String)", "Validate.notBlank"),
    DeclarationClaim("Validate.notEmpty(Array[Double],String)", "Validate.notEmpty"),
    DeclarationClaim("Validate.notEmpty(Array[Int],String)", "Validate.notEmpty"),
    DeclarationClaim("Validate.notEmpty(Array[Long],String)", "Validate.notEmpty"),
    DeclarationClaim("Validate.notEmpty(Array[T],String)", "Validate.notEmpty"),
    DeclarationClaim("Validate.notEmpty(Iterable[T],String)", "Validate.notEmpty"),
    DeclarationClaim("Validate.notEmpty(Map[K,V],String)", "Validate.notEmpty"),
    DeclarationClaim("Validate.notEmpty(String,String)", "Validate.notEmpty"),
    DeclarationClaim("Validate.notNaN(Double,String)", "Validate.notNaN"),
    DeclarationClaim("Validate.notNegative(Decimal,String)", "Validate.notNegative"),
    DeclarationClaim("Validate.notNegative(Double,String)", "Validate.notNegative"),
    DeclarationClaim("Validate.notNegative(Int,String)", "Validate.notNegative"),
    DeclarationClaim("Validate.notNegative(Long,String)", "Validate.notNegative"),
    DeclarationClaim("Validate.notNegativeOrZero(Decimal,String)", "Validate.notNegativeOrZero"),
    DeclarationClaim("Validate.notNegativeOrZero(Double,Double,String)", "Validate.notNegativeOrZero"),
    DeclarationClaim("Validate.notNegativeOrZero(Double,String)", "Validate.notNegativeOrZero"),
    DeclarationClaim("Validate.notNegativeOrZero(Int,String)", "Validate.notNegativeOrZero"),
    DeclarationClaim("Validate.notNegativeOrZero(Long,String)", "Validate.notNegativeOrZero"),
    DeclarationClaim("Validate.notPositive(Decimal,String)", "Validate.notPositive"),
    DeclarationClaim("Validate.notPositive(Double,String)", "Validate.notPositive"),
    DeclarationClaim("Validate.notPositive(Int,String)", "Validate.notPositive"),
    DeclarationClaim("Validate.notPositive(Long,String)", "Validate.notPositive"),
    DeclarationClaim("Validate.notPositiveIfPresent(Option[Decimal],String)", "Validate.notPositiveIfPresent"),
    DeclarationClaim("Validate.notZero(Double,Double,String)", "Validate.notZero"),
    DeclarationClaim("Validate.notZero(Double,String)", "Validate.notZero"),
    DeclarationClaim("Validate.toResult(ValidatedFailures[A])", "Validate.toResult"),
    DeclarationClaim("Validate.valid(A)", "Validate.valid"),
    DeclarationClaim("ValueAdjustmentType.parse(String)", "ValueAdjustmentType.parse"),
    DeclarationClaim("ValueSchedule.of(Double)", "ValueSchedule.of"),
    DeclarationClaim("ValueSchedule.of(Double,List[ValueStep])", "ValueSchedule.of"),
    DeclarationClaim("ValueSchedule.of(Double,List[ValueStep],Option[ValueStepSequence])", "ValueSchedule.of"),
    DeclarationClaim("ValueSchedule.of(Double,ValueStep,ValueStep*)", "ValueSchedule.of"),
    DeclarationClaim("ValueSchedule.of(Double,ValueStepSequence)", "ValueSchedule.of"),
    DeclarationClaim("ValueSchedule.resolveValues(Schedule)", "ValueSchedule.resolveValues"),
    DeclarationClaim("ValueSchedule.withStepSequence(ValueStepSequence)", "ValueSchedule.withStepSequence"),
    DeclarationClaim("ValueSchedule.withSteps(List[ValueStep])", "ValueSchedule.withSteps"),
    DeclarationClaim("ValueStep.of(Int,ValueAdjustment)", "ValueStep.of"),
    DeclarationClaim("ValueStep.of(Option[Int],Option[LocalDate],ValueAdjustment)", "ValueStep.of"),
    DeclarationClaim("ValueStepSequence.of(LocalDate,LocalDate,Frequency,ValueAdjustment)", "ValueStepSequence.of"),
    DeclarationClaim("ValueWithFailures.combineValuesAsList(IterableOnce[ValueWithFailures[A]])", "ValueWithFailures.combineValuesAsList"),
    DeclarationClaim("ValueWithFailures.combineValuesAsSet(IterableOnce[ValueWithFailures[A]])", "ValueWithFailures.combineValuesAsSet"),
    DeclarationClaim("ValueWithFailures.of(A)", "ValueWithFailures.of"),
    DeclarationClaim("ValueWithFailures.of(A,IterableOnce[Failure])", "ValueWithFailures.of"),
    DeclarationClaim("ValueWithFailures.withValue(ValueWithFailures[A],B)", "ValueWithFailures.withValue"),
    DeclarationClaim("ValueWithFailures.withValue(ValueWithFailures[A],B,IterableOnce[Failure])", "ValueWithFailures.withValue"),
    DeclarationClaim("ValueWithFailures.withValue(ValueWithFailures[A],ValueWithFailures[B])", "ValueWithFailures.withValue"),
    DeclarationClaim("result.combine(IterableOnce[Either[E,A]],List[A]=>B)", "result.combine"),
    DeclarationClaim("result.flatCombine(IterableOnce[Either[E,A]],List[A]=>Either[E,B])", "result.flatCombine"),
    DeclarationClaim("result.sequence(IterableOnce[Either[E,A]])", "result.sequence"),
    DeclarationClaim("result.toNec(FailureOr[A])", "result.toNec"),
    DeclarationClaim("result.toResult(ValidatedFailures[A])", "result.toResult"),
    DeclarationClaim("result.toValidated(ResultNec[A])", "result.toValidated"),
    DeclarationClaim("result.withAdditionalFailures(ValueWithFailures[A],IterableOnce[Failure])", "result.withAdditionalFailures")
  )

  /**
   * Every public declaration whose scaladoc documents a throw, each with the row that accounts
   * for it.
   */
  private val throwingClaims: List[DeclarationClaim] = List(
    DeclarationClaim("ArgCheck.inOrderNotEqual(T,T,String,String,Order[T])", "ArgCheck.inOrderNotEqual"),
    DeclarationClaim("ArgCheck.inOrderOrEqual(T,T,String,String,Order[T])", "ArgCheck.inOrderOrEqual"),
    DeclarationClaim("ArgCheck.inRange(Double,Double,Double,String)", "ArgCheck.inRange"),
    DeclarationClaim("ArgCheck.inRange(Int,Int,Int,String)", "ArgCheck.inRange"),
    DeclarationClaim("ArgCheck.inRangeComparable(T,T,T,String,Order[T])", "ArgCheck.inRangeComparable"),
    DeclarationClaim("ArgCheck.inRangeComparableExclusive(T,T,T,String,Order[T])", "ArgCheck.inRangeComparableExclusive"),
    DeclarationClaim("ArgCheck.inRangeComparableInclusive(T,T,T,String,Order[T])", "ArgCheck.inRangeComparableInclusive"),
    DeclarationClaim("ArgCheck.inRangeExclusive(Double,Double,Double,String)", "ArgCheck.inRangeExclusive"),
    DeclarationClaim("ArgCheck.inRangeExclusive(Int,Int,Int,String)", "ArgCheck.inRangeExclusive"),
    DeclarationClaim("ArgCheck.inRangeInclusive(Double,Double,Double,String)", "ArgCheck.inRangeInclusive"),
    DeclarationClaim("ArgCheck.inRangeInclusive(Int,Int,Int,String)", "ArgCheck.inRangeInclusive"),
    DeclarationClaim("ArgCheck.isFalse(Boolean,=>String)", "ArgCheck.isFalse"),
    DeclarationClaim("ArgCheck.isTrue(Boolean)", "ArgCheck.isTrue"),
    DeclarationClaim("ArgCheck.isTrue(Boolean,=>String)", "ArgCheck.isTrue"),
    DeclarationClaim("ArgCheck.matches(Char=>Boolean,Int,Int,String,String,String)", "ArgCheck.matches"),
    DeclarationClaim("ArgCheck.matches(Regex,String,String)", "ArgCheck.matches"),
    DeclarationClaim("ArgCheck.noDuplicates(Array[Double],String)", "ArgCheck.noDuplicates"),
    DeclarationClaim("ArgCheck.noDuplicatesSorted(Array[Double],String)", "ArgCheck.noDuplicatesSorted"),
    DeclarationClaim("ArgCheck.notBlank(String,String)", "ArgCheck.notBlank"),
    DeclarationClaim("ArgCheck.notEmpty(Array[Double],String)", "ArgCheck.notEmpty"),
    DeclarationClaim("ArgCheck.notEmpty(Array[Int],String)", "ArgCheck.notEmpty"),
    DeclarationClaim("ArgCheck.notEmpty(Array[Long],String)", "ArgCheck.notEmpty"),
    DeclarationClaim("ArgCheck.notEmpty(Array[T],String)", "ArgCheck.notEmpty"),
    DeclarationClaim("ArgCheck.notEmpty(Iterable[T],String)", "ArgCheck.notEmpty"),
    DeclarationClaim("ArgCheck.notEmpty(Map[K,V],String)", "ArgCheck.notEmpty"),
    DeclarationClaim("ArgCheck.notEmpty(Matrix,String)", "ArgCheck.notEmpty"),
    DeclarationClaim("ArgCheck.notEmpty(String,String)", "ArgCheck.notEmpty"),
    DeclarationClaim("ArgCheck.notNaN(Double,String)", "ArgCheck.notNaN"),
    DeclarationClaim("ArgCheck.notNegative(Decimal,String)", "ArgCheck.notNegative"),
    DeclarationClaim("ArgCheck.notNegative(Double,String)", "ArgCheck.notNegative"),
    DeclarationClaim("ArgCheck.notNegative(Int,String)", "ArgCheck.notNegative"),
    DeclarationClaim("ArgCheck.notNegative(Long,String)", "ArgCheck.notNegative"),
    DeclarationClaim("ArgCheck.notNegativeOrZero(Decimal,String)", "ArgCheck.notNegativeOrZero"),
    DeclarationClaim("ArgCheck.notNegativeOrZero(Double,Double,String)", "ArgCheck.notNegativeOrZero"),
    DeclarationClaim("ArgCheck.notNegativeOrZero(Double,String)", "ArgCheck.notNegativeOrZero"),
    DeclarationClaim("ArgCheck.notNegativeOrZero(Int,String)", "ArgCheck.notNegativeOrZero"),
    DeclarationClaim("ArgCheck.notNegativeOrZero(Long,String)", "ArgCheck.notNegativeOrZero"),
    DeclarationClaim("ArgCheck.notPositive(Decimal,String)", "ArgCheck.notPositive"),
    DeclarationClaim("ArgCheck.notPositive(Double,String)", "ArgCheck.notPositive"),
    DeclarationClaim("ArgCheck.notPositive(Int,String)", "ArgCheck.notPositive"),
    DeclarationClaim("ArgCheck.notPositive(Long,String)", "ArgCheck.notPositive"),
    DeclarationClaim("ArgCheck.notPositiveIfPresent(Option[Decimal],String)", "ArgCheck.notPositiveIfPresent"),
    DeclarationClaim("ArgCheck.notZero(Double,Double,String)", "ArgCheck.notZero"),
    DeclarationClaim("ArgCheck.notZero(Double,String)", "ArgCheck.notZero"),
    DeclarationClaim("BigMoney.convertedTo(Currency,BigDecimal)", "BigMoney.convertedTo(eighteen digits)"),
    DeclarationClaim("BigMoney.convertedTo(Currency,Decimal)", "BigMoney.convertedTo(eighteen digits)"),
    DeclarationClaim("BigMoney.minus(BigMoney)", "BigMoney.minus(eighteen digits)"),
    DeclarationClaim("BigMoney.multipliedBy(Long)", "BigMoney.multipliedBy"),
    DeclarationClaim("BigMoney.plus(BigMoney)", "BigMoney.plus(eighteen digits)"),
    DeclarationClaim("BigMoney.roundToScale(Int,RoundingMode)", "BigMoney.roundToScale"),
    DeclarationClaim("BusinessDayConvention.adjust(LocalDate,HolidayCalendar)", "BusinessDayConvention.adjust"),
    DeclarationClaim("CurrencyAmount.mapAmount(Double=>Double)", "CurrencyAmount.mapAmount"),
    DeclarationClaim("CurrencyAmount.minus(CurrencyAmount)", "CurrencyAmount.minus(not a number)"),
    DeclarationClaim("CurrencyAmount.minus(Double)", "CurrencyAmount.minus(not a number)"),
    DeclarationClaim("CurrencyAmount.multipliedBy(Double)", "CurrencyAmount.multipliedBy"),
    DeclarationClaim("CurrencyAmount.plus(CurrencyAmount)", "CurrencyAmount.plus(not a number)"),
    DeclarationClaim("CurrencyAmount.plus(Double)", "CurrencyAmount.plus(not a number)"),
    DeclarationClaim("CurrencyAmountArray.get(Int)", "CurrencyAmountArray.get(index outside the array)"),
    DeclarationClaim("CurrencyAmountArray.mapAmounts(Double=>Double)", "CurrencyAmountArray.mapAmounts"),
    DeclarationClaim("CurrencyAmountArray.multipliedBy(Double)", "CurrencyAmountArray.multipliedBy"),
    DeclarationClaim("CurrencyAmountArray.of(Currency,DoubleArray)", "CurrencyAmountArray.of(a value that is not a number)"),
    DeclarationClaim("DateAdjuster.adjust(LocalDate)", "DateAdjuster.adjust"),
    DeclarationClaim("DateAdjuster.adjustInto(Temporal)", "DateAdjuster.adjustInto"),
    DeclarationClaim("DateSequence.nth(LocalDate,Int)", "DateSequence.nth"),
    DeclarationClaim("DateSequence.nthOrSame(LocalDate,Int)", "DateSequence.nthOrSame"),
    DeclarationClaim("DayCount.days(LocalDate,LocalDate)", "DayCount.days"),
    DeclarationClaim("DayCount.relativeYearFraction(LocalDate,LocalDate)", "DayCount.relativeYearFraction"),
    DeclarationClaim("DayCount.relativeYearFraction(LocalDate,LocalDate,DayCount.ScheduleInfo)", "DayCount.relativeYearFraction"),
    DeclarationClaim("DayCount.yearFraction(LocalDate,LocalDate)", "DayCount.yearFraction"),
    DeclarationClaim("DayCount.yearFraction(LocalDate,LocalDate,DayCount.ScheduleInfo)", "DayCount.yearFraction"),
    DeclarationClaim("Decimal.dividedBy(Decimal)", "Decimal.dividedBy"),
    DeclarationClaim("Decimal.dividedBy(Decimal,RoundingMode)", "Decimal.dividedBy"),
    DeclarationClaim("Decimal.dividedBy(Double)", "Decimal.dividedBy"),
    DeclarationClaim("Decimal.dividedBy(Long)", "Decimal.dividedBy"),
    DeclarationClaim("Decimal.format(Int,RoundingMode)", "Decimal.format"),
    DeclarationClaim("Decimal.formatAtLeast(Int)", "Decimal.formatAtLeast"),
    DeclarationClaim("Decimal.minus(Decimal)", "Decimal.minus"),
    DeclarationClaim("Decimal.minus(Double)", "Decimal.minus"),
    DeclarationClaim("Decimal.minus(Long)", "Decimal.minus"),
    DeclarationClaim("Decimal.movePoint(Int)", "Decimal.movePoint"),
    DeclarationClaim("Decimal.multipliedBy(Decimal)", "Decimal.multipliedBy"),
    DeclarationClaim("Decimal.multipliedBy(Double)", "Decimal.multipliedBy"),
    DeclarationClaim("Decimal.multipliedBy(Long)", "Decimal.multipliedBy"),
    DeclarationClaim("Decimal.plus(Decimal)", "Decimal.plus"),
    DeclarationClaim("Decimal.plus(Double)", "Decimal.plus"),
    DeclarationClaim("Decimal.plus(Long)", "Decimal.plus"),
    DeclarationClaim("Decimal.remainder(Decimal)", "Decimal.remainder"),
    DeclarationClaim("Decimal.roundToPrecision(Int,RoundingMode)", "Decimal.roundToPrecision"),
    DeclarationClaim("Decimal.roundToScale(Int,RoundingMode)", "Decimal.roundToScale"),
    DeclarationClaim("DoubleArray.combine(DoubleArray,(Double,Double)=>Double)", "DoubleArray.combine"),
    DeclarationClaim("DoubleArray.combineReduce(DoubleArray,DoubleArray.DoubleTernaryOperator)", "DoubleArray.combineReduce"),
    DeclarationClaim("DoubleArray.copyOf(Array[Double],Int)", "DoubleArray.copyOf"),
    DeclarationClaim("DoubleArray.copyOf(Array[Double],Int,Int)", "DoubleArray.copyOf"),
    DeclarationClaim("DoubleArray.dividedBy(DoubleArray)", "DoubleArray.dividedBy"),
    DeclarationClaim("DoubleArray.equalWithTolerance(DoubleArray,Double)", "DoubleArray.equalWithTolerance"),
    DeclarationClaim("DoubleArray.equalZeroWithTolerance(Double)", "DoubleArray.equalZeroWithTolerance"),
    DeclarationClaim("DoubleArray.filled(Int)", "DoubleArray.filled"),
    DeclarationClaim("DoubleArray.filled(Int,Double)", "DoubleArray.filled"),
    DeclarationClaim("DoubleArray.get(Int)", "DoubleArray.get"),
    DeclarationClaim("DoubleArray.max()", "DoubleArray.max"),
    DeclarationClaim("DoubleArray.min()", "DoubleArray.min"),
    DeclarationClaim("DoubleArray.minus(DoubleArray)", "DoubleArray.minus"),
    DeclarationClaim("DoubleArray.multipliedBy(DoubleArray)", "DoubleArray.multipliedBy"),
    DeclarationClaim("DoubleArray.plus(DoubleArray)", "DoubleArray.plus"),
    DeclarationClaim("DoubleArray.subArray(Int)", "DoubleArray.subArray"),
    DeclarationClaim("DoubleArray.subArray(Int,Int)", "DoubleArray.subArray"),
    DeclarationClaim("DoubleArray.tabulate(Int,Int=>Double)", "DoubleArray.tabulate"),
    DeclarationClaim("DoubleArrayMath.combine(Array[Double],Array[Double],(Double,Double)=>Double)", "DoubleArrayMath.combine"),
    DeclarationClaim("DoubleArrayMath.combineByAddition(Array[Double],Array[Double])", "DoubleArrayMath.combineByAddition"),
    DeclarationClaim("DoubleArrayMath.combineByMultiplication(Array[Double],Array[Double])", "DoubleArrayMath.combineByMultiplication"),
    DeclarationClaim("DoubleArrayMath.fuzzyEquals(Array[Double],Array[Double],Double)", "DoubleArrayMath.fuzzyEquals"),
    DeclarationClaim("DoubleArrayMath.fuzzyEquals(Double,Double,Double)", "DoubleArrayMath.fuzzyEquals"),
    DeclarationClaim("DoubleArrayMath.fuzzyEqualsZero(Array[Double],Double)", "DoubleArrayMath.fuzzyEqualsZero"),
    DeclarationClaim("DoubleArrayMath.reorderedCopy(Array[Double],Array[Int])", "DoubleArrayMath.reorderedCopy"),
    DeclarationClaim("DoubleArrayMath.sortPairs(Array[Double],Array[Double])", "DoubleArrayMath.sortPairs"),
    DeclarationClaim("DoubleArrayMath.sortPairs(Array[Double],Array[Int])", "DoubleArrayMath.sortPairs"),
    DeclarationClaim("DoubleArrayMath.sortPairs(Array[Double],Array[V])", "DoubleArrayMath.sortPairs"),
    DeclarationClaim("DoubleMatrix.column(Int)", "DoubleMatrix.column"),
    DeclarationClaim("DoubleMatrix.columnArray(Int)", "DoubleMatrix.columnArray"),
    DeclarationClaim("DoubleMatrix.combine(DoubleMatrix,(Double,Double)=>Double)", "DoubleMatrix.combine"),
    DeclarationClaim("DoubleMatrix.copyOf(Array[Array[Double]])", "DoubleMatrix.copyOf"),
    DeclarationClaim("DoubleMatrix.filled(Int,Int)", "DoubleMatrix.filled"),
    DeclarationClaim("DoubleMatrix.filled(Int,Int,Double)", "DoubleMatrix.filled"),
    DeclarationClaim("DoubleMatrix.get(Int,Int)", "DoubleMatrix.get"),
    DeclarationClaim("DoubleMatrix.identity(Int)", "DoubleMatrix.identity"),
    DeclarationClaim("DoubleMatrix.minus(DoubleMatrix)", "DoubleMatrix.minus"),
    DeclarationClaim("DoubleMatrix.of(Int,Int,Double*)", "DoubleMatrix.of"),
    DeclarationClaim("DoubleMatrix.ofArrayObjects(Int,Int,RowArrayObjectFunction)", "DoubleMatrix.ofArrayObjects"),
    DeclarationClaim("DoubleMatrix.ofArrays(Int,Int,RowArrayFunction)", "DoubleMatrix.ofArrays"),
    DeclarationClaim("DoubleMatrix.plus(DoubleMatrix)", "DoubleMatrix.plus"),
    DeclarationClaim("DoubleMatrix.row(Int)", "DoubleMatrix.row"),
    DeclarationClaim("DoubleMatrix.rowArray(Int)", "DoubleMatrix.rowArray"),
    DeclarationClaim("DoubleMatrix.tabulate(Int,Int,(Int,Int)=>Double)", "DoubleMatrix.tabulate"),
    DeclarationClaim("FxRate.inverse()", "FxRate.inverse"),
    DeclarationClaim("FxRate.toConventional()", "FxRate.toConventional"),
    DeclarationClaim("FxRateProvider.convert(Decimal,Currency,Currency)", "FxRateProvider.convert(eighteen digits)"),
    DeclarationClaim("HalfUp.round(BigDecimal)", "HalfUp.round"),
    DeclarationClaim("HolidayCalendar.businessDays(LocalDate,LocalDate)", "HolidayCalendar.businessDays"),
    DeclarationClaim("HolidayCalendar.daysBetween(LocalDate,LocalDate)", "HolidayCalendar.daysBetween"),
    DeclarationClaim("HolidayCalendar.holidays(LocalDate,LocalDate)", "HolidayCalendar.holidays"),
    DeclarationClaim("HolidayCalendar.isBusinessDay(LocalDate)", "HolidayCalendar.isBusinessDay"),
    DeclarationClaim("HolidayCalendar.isHoliday(LocalDate)", "HolidayCalendar.isHoliday"),
    DeclarationClaim("HolidayCalendar.isLastBusinessDayOfMonth(LocalDate)", "HolidayCalendar.isLastBusinessDayOfMonth"),
    DeclarationClaim("HolidayCalendar.lastBusinessDayOfMonth(LocalDate)", "HolidayCalendar.lastBusinessDayOfMonth"),
    DeclarationClaim("HolidayCalendar.next(LocalDate)", "HolidayCalendar.next"),
    DeclarationClaim("HolidayCalendar.nextOrSame(LocalDate)", "HolidayCalendar.nextOrSame"),
    DeclarationClaim("HolidayCalendar.nextSameOrLastInMonth(LocalDate)", "HolidayCalendar.nextSameOrLastInMonth"),
    DeclarationClaim("HolidayCalendar.previous(LocalDate)", "HolidayCalendar.previous"),
    DeclarationClaim("HolidayCalendar.previousOrSame(LocalDate)", "HolidayCalendar.previousOrSame"),
    DeclarationClaim("HolidayCalendar.shift(LocalDate,Int)", "HolidayCalendar.shift"),
    DeclarationClaim("HolidayCalendar.adjustBy(Int)", "HolidayCalendar.adjustBy"),
    DeclarationClaim("HolidayCalendar.combinedWith(HolidayCalendar)", "HolidayCalendar.combinedWith"),
    DeclarationClaim("HolidayCalendar.linkedWith(HolidayCalendar)", "HolidayCalendar.linkedWith"),
    DeclarationClaim("DaysAdjustment.ofBusinessDays(Int,HolidayCalendarId)", "DaysAdjustment.ofBusinessDays"),
    DeclarationClaim(
      "DaysAdjustment.ofBusinessDays(Int,HolidayCalendarId,BusinessDayAdjustment)",
      "DaysAdjustment.ofBusinessDays"),
    DeclarationClaim(
      "ImmutableHolidayCalendar.of(HolidayCalendarId,Iterable[LocalDate],DayOfWeek,DayOfWeek)",
      "ImmutableHolidayCalendar.of"),
    DeclarationClaim(
      "ImmutableHolidayCalendar.of(HolidayCalendarId,Iterable[LocalDate],Iterable[DayOfWeek])",
      "ImmutableHolidayCalendar.of"),
    DeclarationClaim("ImmutableHolidayCalendar.of(HolidayCalendarId,Iterable[LocalDate],Iterable[DayOfWeek],Iterable[LocalDate])", "ImmutableHolidayCalendar.of"),
    DeclarationClaim(
      "ImmutableHolidayCalendar.combined(ImmutableHolidayCalendar,ImmutableHolidayCalendar)",
      "ImmutableHolidayCalendar.combined"),
    DeclarationClaim("Money.convertedTo(Currency,BigDecimal)", "Money.convertedTo(eighteen digits)"),
    DeclarationClaim("Money.convertedTo(Currency,Decimal)", "Money.convertedTo(eighteen digits)"),
    DeclarationClaim("Money.minus(Money)", "Money.minus(eighteen digits)"),
    DeclarationClaim("Money.multipliedBy(Long)", "Money.multipliedBy"),
    DeclarationClaim("Money.plus(Money)", "Money.plus(eighteen digits)"),
    DeclarationClaim("MultiCurrencyAmount.mapAmounts(Double=>Double)", "MultiCurrencyAmount.mapAmounts"),
    DeclarationClaim("MultiCurrencyAmount.mapCurrencyAmounts(CurrencyAmount=>CurrencyAmount)", "MultiCurrencyAmount.mapCurrencyAmounts"),
    DeclarationClaim("MultiCurrencyAmount.minus(Currency,Double)", "MultiCurrencyAmount.minus(not a number)"),
    DeclarationClaim("MultiCurrencyAmount.minus(CurrencyAmount)", "MultiCurrencyAmount.minus(not a number)"),
    DeclarationClaim("MultiCurrencyAmount.minus(MultiCurrencyAmount)", "MultiCurrencyAmount.minus(not a number)"),
    DeclarationClaim("MultiCurrencyAmount.multipliedBy(Double)", "MultiCurrencyAmount.multipliedBy"),
    DeclarationClaim("MultiCurrencyAmount.plus(Currency,Double)", "MultiCurrencyAmount.plus(not a number)"),
    DeclarationClaim("MultiCurrencyAmount.plus(CurrencyAmount)", "MultiCurrencyAmount.plus(not a number)"),
    DeclarationClaim("MultiCurrencyAmount.plus(MultiCurrencyAmount)", "MultiCurrencyAmount.plus(not a number)"),
    DeclarationClaim("MultiCurrencyAmount.total(Iterable[CurrencyAmount])", "MultiCurrencyAmount.total"),
    DeclarationClaim("MultiCurrencyAmountArray.get(Int)", "MultiCurrencyAmountArray.get(3)"),
    DeclarationClaim("MultiCurrencyAmountArray.mapAmounts(Double=>Double)", "MultiCurrencyAmountArray.mapAmounts"),
    DeclarationClaim("MultiCurrencyAmountArray.multipliedBy(Double)", "MultiCurrencyAmountArray.multipliedBy"),
    DeclarationClaim("MultiCurrencyAmountArray.of(Int,Int=>MultiCurrencyAmount)", "MultiCurrencyAmountArray.of(negative size)"),
    DeclarationClaim("Payment.adjustDate(LocalDate=>LocalDate)", "Payment.adjustDate"),
    DeclarationClaim("PeriodAdditionConvention.adjust(LocalDate,Period,HolidayCalendar)", "PeriodAdditionConvention.adjust"),
    DeclarationClaim("Schedule.merge(Int,LocalDate,LocalDate)", "Schedule.merge(dates out of order)"),
    DeclarationClaim("Schedule.period(Int)", "Schedule.period"),
    DeclarationClaim("SchedulePeriod.yearFraction(DayCount,DayCount.ScheduleInfo)", "SchedulePeriod.yearFraction"),
    DeclarationClaim("ValueDerivatives.getDerivative(Int)", "ValueDerivatives.getDerivative")
  )

  private lazy val allClaims: List[(String, DeclarationClaim)] =
    failableClaims.map((FailableSide, _)) ::: throwingClaims.map((ThrowingSide, _))

  /** The enumerated declarations of each side, by key. */
  private lazy val derivedDeclarationsByKey: Map[String, Map[String, List[Declaration]]] =
    Map(
      FailableSide -> derivedFailableDeclarations.groupBy(_.key),
      ThrowingSide -> derivedThrowingDeclarations.groupBy(_.key))

  allClaims.foreach { case (side, claim) =>
    test(s"declared signature [$side]: ${claim.key} accounted for by ${claim.row}") {
      withClue(s"the enumeration no longer finds '${claim.key}' among the $side declarations: ") {
        derivedDeclarationsByKey(side).contains(claim.key) shouldBe true
      }
      withClue(s"'${claim.key}' names row '${claim.row}', which is not in the registry: ") {
        registry.contains(claim.row) shouldBe true
      }
      withClue(s"row '${claim.row}' accounts for '${claim.key}' but is not a row of its family: ") {
        registry.get(claim.row).map(row => row.family) shouldBe Some(familyOf(claim.key))
      }
    }
  }

  //=========================================================================
  // THE DOCUMENTED INVENTORY
  //
  // The three lists below transcribe the documented failable-surface inventory - AAP section
  // 0.3.3, which the `aap` names of this section refer to - each entry paired with the
  // hand-written test that covers it, so that a named entry can be found from either side.
  // They are not what makes the coverage exhaustive - the derived enumeration above is - and
  // `aap_inventory_is_a_subset_of_the_derived_surface` asserts that every name they hold is a
  // family that enumeration found. The lists are the 95 failure-reporting entries that name a
  // declaration, the 2 reconciled by name because they name none, and the 12 contract rows.
  //=========================================================================

  /**
   * The failure-reporting entries of the inventory that name a declaration.
   *
   * Each is paired with the hand-written test that covers it, and
   * `aap_inventory_entries_name_an_existing_test` asserts that the test named still exists, so an
   * entry whose test is renamed or removed fails the suite. Entries sharing one failure shape
   * share one test, which is why the test names repeat; such a test holds one row per entry.
   */
  private val aapFailableEntries: List[(String, String)] = List(
    // FX and currency, 38 entries
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
    "Money.mapAmount" -> "Money.mapAmount and BigMoney.mapAmount report a result no decimal holds",
    "BigMoney.mapAmount" -> "Money.mapAmount and BigMoney.mapAmount report a result no decimal holds",
    "CurrencyAmount.toMoney" ->
      "CurrencyAmount.toMoney and CurrencyAmount.toBigMoney report an amount no decimal holds",
    "CurrencyAmount.toBigMoney" ->
      "CurrencyAmount.toMoney and CurrencyAmount.toBigMoney report an amount no decimal holds",
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
    // Schedule and frequency, 14 entries
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
    "Schedule.of" -> "Schedule.of reports periods that do not run from earliest to latest",
    "Schedule.merge" -> "Schedule.merge reports a date that matches no period of the schedule",
    "Schedule.mergeRegular" -> "Schedule.mergeRegular reports an unusable group size",
    "Schedule.toAdjusted" -> "Schedule.toAdjusted reports a period that collapses once adjusted",
    "RollConvention.ofDayOfMonth" -> "RollConvention.ofDayOfMonth reports a day outside one to thirty-one",
    // Dates and adjustments, 10 entries
    "Tenor.of" -> "Tenor.of reports a period that is no tenor",
    "Tenor.parse" -> "Tenor.parse reports text that names no tenor",
    "MarketTenor.parse" -> "MarketTenor.parse reports text that names no market tenor",
    "SequenceDate.of" -> "SequenceDate.of reports fields that describe no instruction",
    "DaysAdjustment.of" -> "DaysAdjustment.of reports a business day addition of no days",
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
    "ValueSchedule.of" ->
      "ValueSchedule.of reports two steps that name one position with different adjustments",
    "ValueStepSequence.of" -> "ValueStepSequence.of reports arguments that describe no sequence",
    "ValueSchedule.resolveValues" -> "ValueSchedule.resolveValues reports a step the schedule cannot carry",
    "Rounding.ofDecimalPlaces" -> "Rounding.ofDecimalPlaces reports a count outside zero to 255",
    "Rounding.ofFractionalDecimalPlaces" -> "Rounding.ofFractionalDecimalPlaces accumulates both rejections",
    // Collect members reached from this module, 4 entries
    "NamedEnum.parse" -> "NamedEnum.parse reports a name no member of the family carries",
    "Decimal.of" -> "Decimal.of reports a value no decimal holds",
    "Decimal.parse" -> "Decimal.parse reports text that names no decimal",
    "FixedScaleDecimal.of" -> "FixedScaleDecimal.of reports a scale the decimal cannot be held at",
    // Index observations and floating rates, 13 entries
    "IborIndexObservation.of" ->
      "IborIndexObservation.of reports a calendar the reference data does not hold",
    "OvernightIndexObservation.of" ->
      "OvernightIndexObservation.of reports a calendar the reference data does not hold",
    "FxIndexObservation.of" -> "FxIndexObservation.of reports a calendar the reference data does not hold",
    "IborIndex.resolve" -> "IborIndex.resolve reports a calendar the reference data does not hold",
    "IborIndexObservation.resolve" ->
      "IborIndex.resolve reports a calendar the reference data does not hold",
    "FxIndex.resolve" -> "FxIndex.resolve reports a calendar the reference data does not hold",
    "FxIndexObservation.resolve" ->
      "FxIndex.resolve reports a calendar the reference data does not hold",
    "OvernightIndexObservation.resolve" ->
      "OvernightIndexObservation.resolve reports a calendar the reference data does not hold",
    "FloatingRateName.toIborIndex" ->
      "FloatingRateName.toIborIndex reports a name of the wrong kind and a tenor no index carries",
    "FloatingRateName.toOvernightIndex" ->
      "FloatingRateName.toOvernightIndex reports a name of the wrong kind",
    "FloatingRateName.toPriceIndex" -> "FloatingRateName.toPriceIndex reports a name of the wrong kind",
    "FloatingRateName.defaultIborIndex" ->
      "FloatingRateName.defaultIborIndex reports a currency with no published default",
    "FloatingRateName.defaultOvernightIndex" ->
      "FloatingRateName.defaultOvernightIndex reports a currency with no published default")

  /**
   * The two entries of the inventory that name no failure-returning declaration.
   *
   * Each is paired with the derived row, or the hand-written test, that stands in for it and with
   * the reason it names no declaration of its own.
   * `aap_inventory_is_a_subset_of_the_derived_surface` asserts that the stand-in named exists.
   */
  private val aapReconciledEntries: List[(String, String, String)] = List(
    (
      "MarketTenor.of",
      "MarketTenor.ofSpot",
      "the plan names the shorthand for the four spot factories, which is what the type " +
        "publishes; the counted three of them are rejecting rows and `ofSpot` is a reasoned total " +
        "row"),
    (
      "RollConvention.ofDayOfWeek",
      "RollConvention.ofDayOfWeek is total for every day of the week",
      "the plan lists it beside `ofDayOfMonth` because the Java pair shared a lookup, but its " +
        "argument is an enumerated day rather than a number, so it is total by specification - " +
        "seven days, seven conventions, no error channel - and the named test asserts every one " +
        "of the seven"))

  /**
   * The contract half of the inventory: twelve rows, nine refusals and three totality contracts.
   *
   * The nine refusals are the `DoubleArray` and `DoubleMatrix` index and dimension errors, the
   * holiday calendar's year range, the dates-in-order precondition of `DayCount.yearFraction` and
   * the schedule information `Act/Act ICMA`, `Act/365L` and `30E/360 ISDA` each read. The three
   * totality contracts are members that carry the same shape and deliberately do not refuse:
   * `DayCount.relativeYearFraction` for a reversed pair, `DayCount 30U/360` for the end-of-month
   * flag, and `Schedule.periodEndDate` for a date outside every period.
   */
  private val aapContractEntries: List[(String, String)] = List(
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

  //=========================================================================
  // THE COVERAGE ASSERTIONS
  //
  // Nine tests: one reporting the sizes of the enumeration and the registry, and eight matching
  // the rows, the claims and the plan's entries against the enumeration in both directions. No
  // total is hard-coded below - a number written here would be the transcription these assertions
  // exist to replace - so none of them can be satisfied by a count.
  //=========================================================================

  test("derived_inventory_counts") {
    // The size of the surface is reported rather than asserted: what was enumerated and what
    // accounts for it are printed for a reader, and the assertions below are what hold the two
    // together. Both source roots have to contribute, which catches an enumeration taken from the
    // wrong working directory.
    SourceRoots.foreach { root =>
      withClue(s"source root '$root' relative to '${new File(".").getAbsolutePath}': ") {
        new File(root).isDirectory shouldBe true
      }
    }
    info(
      s"enumerated ${derivedFailableDeclarations.size} public failure-returning declarations in " +
        s"${derivedFailableFamilies.size} owner/method families")
    info(
      s"enumerated ${derivedThrowingDeclarations.size} public throw-documenting declarations in " +
        s"${derivedThrowingFamilies.size} owner/method families")
    info(
      s"registry holds ${failableRows.size} failable rows and ${throwingRows.size} throwing rows, " +
        rowCountsByKind(failableRows ::: throwingRows))
    info(
      s"AAP 0.3.3 names ${aapFailableEntries.size} failure-reporting entries with a declaration, " +
        s"${aapReconciledEntries.size} reconciled by name, and ${aapContractEntries.size} " +
        "contract entries")

    val collectDeclarations: Int =
      derivedFailableDeclarations.count(_.file.startsWith("strata-collect"))
    val basicsDeclarations: Int =
      derivedFailableDeclarations.count(_.file.startsWith("strata-basics"))
    withClue("failure-returning declarations read from strata-collect: ") {
      collectDeclarations should be > 0
    }
    withClue("failure-returning declarations read from strata-basics: ") {
      basicsDeclarations should be > 0
    }
    derivedThrowingDeclarations.size should be > 0
  }

  test("every_derived_failable_family_has_a_row") {
    // What makes this inventory exhaustive rather than representative: a public member that
    // reports a failure and has no row is one nothing here asserts anything about, and it fails
    // naming the declaration and where to find it.
    val covered: Set[String] = failableRows.map(_.family).toSet
    val uncovered: List[String] =
      derivedFailableFamilies.toList
        .filterNot { case (family, _) => covered.contains(family) }
        .sortBy { case (family, _) => family }
        .map { case (family, declarations) => s"$family (${declarations.map(_.location).mkString(", ")})" }
    withClue("failure-returning families the registry holds no row for: ") {
      uncovered shouldBe empty
    }
  }

  test("every_derived_throwing_family_has_a_row") {
    // The same assertion for the other side of the classification line, so that a documented
    // throw cannot be added - or a reported failure quietly turned into one - without a row.
    val covered: Set[String] = throwingRows.map(_.family).toSet
    val uncovered: List[String] =
      derivedThrowingFamilies.toList
        .filterNot { case (family, _) => covered.contains(family) }
        .sortBy { case (family, _) => family }
        .map { case (family, declarations) => s"$family (${declarations.map(_.location).mkString(", ")})" }
    withClue("throw-documenting families the registry holds no row for: ") {
      uncovered shouldBe empty
    }
  }

  test("every_derived_declaration_is_claimed") {
    // What lifts the inventory from one row per family to one per signature: a public declaration
    // with no claim is an overload nothing here accounts for, whatever its siblings are classified
    // as. The clue lists each one as its key and where to find it, which is also how the table
    // above is written when the surface changes.
    val unclaimed: List[String] =
      List(
        (FailableSide, derivedFailableDeclarations, failableClaims),
        (ThrowingSide, derivedThrowingDeclarations, throwingClaims))
        .flatMap { case (side, declarations, claims) =>
          val claimed: Set[String] = claims.map(_.key).toSet
          declarations
            .filterNot(declaration => claimed.contains(declaration.key))
            .map(declaration => s"$side ${declaration.key} (${declaration.location})")
        }
        .distinct
        .sorted
    withClue(
      s"public declarations with no claim (${unclaimed.size}):\n${unclaimed.mkString("\n")}\n") {
      unclaimed shouldBe empty
    }
    info(
      s"${failableClaims.size} per-signature claims account for the failure-returning " +
        s"declarations and ${throwingClaims.size} for the throw-documenting ones, across " +
        s"${(derivedFailableFamilies.keySet ++ derivedThrowingFamilies.keySet).size} families")
  }

  test("every_claim_names_one_derived_declaration_once") {
    // The reverse direction, and the uniqueness that makes a claim a statement about one
    // signature: a claim naming a declaration the enumeration does not find is a claim about a
    // surface that no longer exists, and two claims for one key would let one hide the other.
    val stale: List[String] =
      allClaims
        .filterNot { case (side, claim) => derivedDeclarationsByKey(side).contains(claim.key) }
        .map { case (side, claim) => s"$side ${claim.key}" }
        .distinct
        .sorted
    withClue(s"claims whose declaration the enumeration did not find:\n${stale.mkString("\n")}\n") {
      stale shouldBe empty
    }
    List((FailableSide, failableClaims), (ThrowingSide, throwingClaims)).foreach {
      case (side, claims) =>
        val keys: List[String] = claims.map(_.key)
        withClue(
          s"$side keys claimed more than once: " +
            s"${keys.diff(keys.distinct).distinct.mkString(", ")}: ") {
          keys.distinct.size shouldBe keys.size
        }
    }
  }

  test("every_row_names_a_derived_family") {
    // The reverse direction, which is what keeps the registry from rotting: a row for a member
    // that no longer returns a failure channel, or no longer documents a throw, is a row that
    // asserts something about a surface that no longer exists.
    val staleFailable: List[String] =
      failableRows.map(_.label).filterNot(label => derivedFailableFamilies.contains(familyOf(label)))
    withClue("failable rows whose family the enumeration did not find: ") {
      staleFailable shouldBe empty
    }
    val staleThrowing: List[String] =
      throwingRows.map(_.label).filterNot(label => derivedThrowingFamilies.contains(familyOf(label)))
    withClue("throwing rows whose family the enumeration did not find: ") {
      staleThrowing shouldBe empty
    }
  }

  test("every_row_is_named_once_and_carries_its_reason") {
    // A label is what a covered row is resolved through and what names the generated test, so a
    // repeated one would hide a row behind another. A total or covered row states a claim about
    // what cannot happen, so one carrying no reason fails here.
    val labels: List[String] = (failableRows ::: throwingRows).map(_.label)
    withClue(s"repeated row labels: ${labels.diff(labels.distinct).mkString(", ")}: ") {
      labels.distinct.size shouldBe labels.size
    }
    val unreasoned: List[String] =
      (failableRows ::: throwingRows).collect {
        case row if reasonOf(row.establishment).exists(_.trim.isEmpty) => row.label
      }
    withClue("rows recording a classification without stating why: ") {
      unreasoned shouldBe empty
    }
  }

  test("aap_inventory_entries_name_an_existing_test") {
    // Every entry of the plan names a hand-written test of this suite, and every row that
    // references a test names one too.
    val declared: Set[String] = testNames
    val missing: List[String] =
      (aapFailableEntries ::: aapContractEntries).collect {
        case (entry, testName) if !declared.contains(testName) => s"$entry -> '$testName'"
      }
    withClue("inventory entries whose test does not exist in this suite: ") {
      missing shouldBe empty
    }
    val missingReferences: List[String] =
      (failableRows ::: throwingRows).collect {
        case row if row.alsoAsserted.exists(name => !declared.contains(name)) =>
          s"${row.label} -> '${row.alsoAsserted.getOrElse("")}'"
      }
    withClue("rows referring to a hand-written test that does not exist in this suite: ") {
      missingReferences shouldBe empty
    }

    // every entry is named once across the three lists, so none can be padded by repeating one
    val entries: List[String] =
      (aapFailableEntries ::: aapContractEntries).map { case (entry, _) => entry } :::
        aapReconciledEntries.map { case (entry, _, _) => entry }
    entries.distinct.size shouldBe entries.size
  }

  test("aap_inventory_is_a_subset_of_the_derived_surface") {
    // The plan and the enumeration are held together here: every name the plan lists is a family
    // the enumeration found, bar the two reconciled entries, each of which has to name a row or a
    // test that stands in for it.
    val declaredFamilies: Set[String] = derivedFailableFamilies.keySet
    val absent: List[String] =
      aapFailableEntries.map { case (entry, _) => familyOf(entry) }.distinct.filterNot(declaredFamilies)
    withClue("AAP 0.3.3 names with no failure-returning declaration in either module: ") {
      absent shouldBe empty
    }

    val declared: Set[String] = testNames
    val unstood: List[String] =
      aapReconciledEntries.collect {
        case (entry, standIn, _)
            if !registry.contains(standIn) && !declared.contains(standIn) =>
          s"$entry -> '$standIn'"
      }
    withClue("reconciled entries whose stand-in row or test does not exist: ") {
      unstood shouldBe empty
    }
    info(
      s"${aapFailableEntries.size} AAP entries checked against the derived families, " +
        s"${aapReconciledEntries.size} reconciled by name")
  }

  /**
   * The `Owner.method` family a label or an inventory entry names.
   *
   * Both are written with the argument list in brackets where one member is classified per
   * overload - `MultiCurrencyAmountArray.plus(MultiCurrencyAmount)` - and the enumeration keys
   * families by owner and method alone, so the brackets are dropped here.
   *
   * @param label  the row label or inventory entry
   * @return the family it names
   */
  private def familyOf(label: String): String = label.takeWhile(_ != '(')

  /**
   * The reason a classification records, where its kind records one.
   *
   * A rejecting or raising row carries its evidence in the assertion it runs, so it has no reason
   * to state; a total or covered row is a claim about what cannot happen, and states one.
   *
   * @param establishment  the classification
   * @return the reason, where the kind has one
   */
  private def reasonOf(establishment: Establishment): Option[String] =
    establishment match {
      case Rejects(_) => None
      case Raises(_) => None
      case Total(reason) => Some(reason)
      case Covered(_, reason) => Some(reason)
    }

  /**
   * Counts the rows of each kind, for the line this suite prints.
   *
   * @param rows  the rows to count
   * @return the counts, as text, ordered by kind
   */
  private def rowCountsByKind(rows: List[SurfaceRow]): String =
    rows
      .groupBy(row =>
        row.establishment match {
          case Rejects(_) => "rejecting"
          case Raises(_) => "raising"
          case Total(_) => "total"
          case Covered(_, _) => "covered"
        })
      .toList
      .sortBy { case (kind, _) => kind }
      .map { case (kind, ofKind) => s"$kind=${ofKind.size}" }
      .mkString(", ")
}

/**
 * The types the enumeration and the registry of [[FailableSurfaceSpec]] are built from.
 *
 * They live in the companion because a case class nested in a class carries a reference to the
 * instance that declared it, which makes every pattern match over one an unchecked type test - a
 * warning, and so a compile error under this build's fatal warnings. Declared here they are
 * ordinary values with no outer reference, and the suite imports them as its own.
 */
private object FailableSurfaceSpec {

  /**
   * A public declaration of one of the two modules, as the enumeration read it.
   *
   * @param file  the path of the source file, relative to the repository root
   * @param owner  the name of the class, trait or object the declaration is a member of
   * @param method  the declared name of the method
   * @param parameters  the declared parameter types, as [[FailableSurfaceSpec.parametersOf]]
   *   renders them: the types of every list, in order, comma separated and with whitespace
   *   removed, and empty for a member declared with no parameter list at all
   * @param line  the one-based line the declaration starts on
   */
  private final case class Declaration(
      file: String,
      owner: String,
      method: String,
      parameters: String,
      line: Int) {

    /** The `Owner.method` key the rows of the registry are written against. */
    def family: String = s"$owner.$method"

    /**
     * The identity of this one declaration, which distinguishes it from its own overloads.
     *
     * Two declarations share a key only where the owner, the method name and the declared
     * parameter types all agree, which is what lets a family classified as a whole still account
     * for each of its overloads separately.
     */
    def key: String = s"$owner.$method($parameters)"

    /** The declaration as a reader chasing a failure of this suite would look it up. */
    def location: String = s"$file:$line"
  }

  /**
   * One public declaration, and the row of the registry that accounts for it.
   *
   * A row is written against an `Owner.method` family and asserts its failure once; a claim is
   * written against one signature of that family and names the row accounting for it. The pair is
   * what makes the inventory exhaustive per declaration rather than per method name.
   *
   * @param key  the declaration, as [[Declaration.key]] renders it
   * @param row  the label of the registry row that accounts for it, which must be a row of the
   *   same family
   */
  private final case class DeclarationClaim(key: String, row: String)

  /**
   * One enclosing owner of the file being read.
   *
   * @param indent  the indentation the owner is declared at, which is what closes it again
   * @param name  the name of the owner
   * @param visible  whether the owner and every owner enclosing it is public
   */
  private final case class Enclosing(indent: Int, name: String, visible: Boolean)

  /**
   * The state the reading of one source file threads from line to line.
   *
   * @param enclosing  the owners currently open, innermost first
   * @param openDefs  the indentations of the `def`s currently open, innermost first
   * @param inScaladoc  whether the line being read is inside a scaladoc comment
   * @param scaladocThrows  whether the scaladoc most recently read carries a `@throws` tag
   * @param failable  the declarations returning a failure channel, most recent first
   * @param throwing  the declarations documenting a throw, most recent first
   */
  private final case class ScanState(
      enclosing: List[Enclosing],
      openDefs: List[Int],
      inScaladoc: Boolean,
      scaladocThrows: Boolean,
      failable: List[Declaration],
      throwing: List[Declaration])

  private val EmptyScan: ScanState =
    ScanState(Nil, Nil, inScaladoc = false, scaladocThrows = false, Nil, Nil)

  /** How a row of the registry establishes the classification it records. */
  private sealed trait Establishment {

    /** The kind of the row, as the generated test names and the printed counts name it. */
    def kind: String

    /** Whether the row asserts a failure, which is what a covered row has to be covered by. */
    def failing: Boolean
  }

  /**
   * The call reports at least one failure.
   *
   * @param reason  the reason the inventory fixes for this member, where it fixes one
   */
  private final case class Rejects(reason: Option[FailureReason]) extends Establishment {
    override def kind: String = "rejects"
    override def failing: Boolean = true
  }

  /**
   * The call raises the documented exception.
   *
   * @param exception  the simple name of the exception type the row intercepts
   */
  private final case class Raises(exception: String) extends Establishment {
    override def kind: String = s"raises $exception"
    override def failing: Boolean = true
  }

  /**
   * The family carries the channel, or documents a throw, that no input of it can reach.
   *
   * @param reason  why no input can reach it, which is the substance of the row
   */
  private final case class Total(reason: String) extends Establishment {
    override def kind: String = "total"
    override def failing: Boolean = false
  }

  /**
   * The failure of this family is asserted by another row of the registry.
   *
   * @param row  the label of the row that asserts it, which must be a failing row
   * @param reason  why this declaration is covered by that one
   */
  private final case class Covered(row: String, reason: String) extends Establishment {
    override def kind: String = s"covered by $row"
    override def failing: Boolean = false
  }

  /**
   * One row of the derived inventory.
   *
   * @param label  the member the row accounts for, as `Owner.method`, optionally naming the
   *   overload in brackets where the family's overloads are classified differently
   * @param establishment  how the row establishes its classification
   * @param alsoAsserted  the name of a hand-written test of this suite that asserts more about
   *   the same member, where there is one
   * @param check  the assertion the generated test runs
   */
  private final case class SurfaceRow(
      label: String,
      establishment: Establishment,
      alsoAsserted: Option[String],
      check: () => Assertion) {

    /** The `Owner.method` family this row accounts for. */
    def family: String = label.takeWhile(_ != '(')

    /**
     * Names a hand-written test of this suite that asserts more about the same member.
     *
     * @param testName  the name of that test
     * @return this row, carrying the reference
     */
    def alsoAssertedBy(testName: String): SurfaceRow = copy(alsoAsserted = Some(testName))
  }
}
