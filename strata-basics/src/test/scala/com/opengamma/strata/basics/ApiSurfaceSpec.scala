/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import java.time.DayOfWeek
import java.time.LocalDate
import java.time.Period
import java.time.YearMonth

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.currency.BigMoney
import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.CurrencyAmount
import com.opengamma.strata.basics.currency.CurrencyAmountArray
import com.opengamma.strata.basics.currency.CurrencyPair
import com.opengamma.strata.basics.currency.FxConvertible
import com.opengamma.strata.basics.currency.FxMatrix
import com.opengamma.strata.basics.currency.FxRate
import com.opengamma.strata.basics.currency.FxRateProvider
import com.opengamma.strata.basics.currency.Money
import com.opengamma.strata.basics.currency.MultiCurrencyAmount
import com.opengamma.strata.basics.currency.MultiCurrencyAmountArray
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
import com.opengamma.strata.basics.index.IborIndex
import com.opengamma.strata.basics.index.IborIndexObservation
import com.opengamma.strata.basics.index.Index
import com.opengamma.strata.basics.index.IndexObservation
import com.opengamma.strata.basics.index.OvernightIndex
import com.opengamma.strata.basics.index.OvernightIndexObservation
import com.opengamma.strata.basics.index.PriceIndex
import com.opengamma.strata.basics.index.PriceIndexObservation
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
import com.opengamma.strata.basics.value.ValueSchedule
import com.opengamma.strata.basics.value.ValueStep
import com.opengamma.strata.basics.value.ValueStepSequence
import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.FixedScaleDecimal
import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.array.DoubleMatrix
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason

/**
 * Holds the '''shape''' of this module's public API to the policy the port was designed around,
 * by asserting things that no ordinary test can express: that a constructor does '''not''' exist,
 * that a hierarchy cannot be extended from outside the file that declares it, and that a member
 * of a neighbouring module is out of reach from here.
 *
 * Everything a running test can observe - what a factory returns, what a calculation computes -
 * is covered by the specs of the individual types. What they cannot observe is the absence of
 * something: a spec that never writes `CurrencyAmount(GBP, 100)` says nothing about whether that
 * expression would compile, and a spec that never declares a new `Index` says nothing about
 * whether the family is still closed. Those are the guarantees a well-meant future change breaks
 * silently, so they are asserted here against the compiler itself, through ScalaTest's
 * `assertTypeError`, `assertDoesNotCompile` and `assertCompiles`.
 *
 * ===What is asserted===
 *
 * There are three groups, and the layout of the file follows them:
 *
 *  1. '''Construction policy.''' Every validated (`[V]`) and normalising (`[N]`) type of both
 *     modules is represented as `sealed abstract case class X private (...)`, a form that
 *     generates neither `apply` nor `copy` while still supporting `unapply`. The alternative that
 *     suggests itself, `final case class X private (...)`, hides '''neither''' of them, so the
 *     distinction is invisible in the source and shows up only when someone writes `X(...)` and
 *     it compiles. Each type below is held to all three facts: no `apply`, no `copy`, and a
 *     working `unapply`. The last matters as much as the first two - a change that locked the
 *     types down harder, by dropping `case` altogether, would cost pattern matching everywhere.
 *  1. '''Closed families.''' The registry the port replaced could be extended at runtime from a
 *     configuration file; the sealed families that replaced it cannot be extended at all, which
 *     is what makes exhaustive matching sound. Scala 2 enforces sealing per '''file''', so the
 *     proof is simply that a subtype declared '''here''' does not compile - and that is also the
 *     reason `index/Index.scala` and `date/HolidayCalendar.scala` are single large files rather
 *     than one file per type. Two traits are deliberately '''open''', and are asserted to be so
 *     from this file: `ReferenceData`, which an application implements to supply its own
 *     holidays, and `FloatingRate`, which `FloatingRateName` implements from another file. The
 *     forward-path contracts kept for the next slice of the migration are asserted the same way,
 *     since being implementable is the only property this slice can give them.
 *  1. '''Copy safety.''' `DoubleArray` and `DoubleMatrix` wrap a primitive array that callers
 *     must not be able to alias, so the two escape hatches that would hand it over - `ofUnsafe`
 *     and `toArrayUnsafe` - are `private[collect]`. Their own module's specs cannot prove that,
 *     being inside `collect`; this module can, and does below, together with runtime proof that
 *     the public factories and `toArray` really copy.
 *
 * ===How the assertions are kept honest===
 *
 * A negative compile assertion has one failure mode that matters: the snippet fails for the
 * wrong reason - a typo, a missing import, an argument of the wrong type - and the assertion
 * passes while proving nothing. Three habits guard against it here, and every test in the file
 * follows them.
 *
 *  - '''`assertTypeError` rather than `assertDoesNotCompile`''' wherever the expected failure is
 *    a type error. `assertDoesNotCompile` is satisfied by a parse error too, so a mistyped
 *    snippet would satisfy it; `assertTypeError` requires the snippet to parse and then fail to
 *    typecheck, which is what "this member does not exist" and "this type cannot be extended"
 *    are.
 *  - '''Arguments taken from a live instance's own accessors.''' Every `X(...)` snippet is
 *    written as `X(instance.field1, instance.field2)`, so the arity and the types are right by
 *    construction and no value can be out of the type's domain. The only thing left for the
 *    compiler to object to is the missing `apply`.
 *  - '''A legal counterpart beside every negative.''' Each negative is paired with an
 *    `assertCompiles` of the nearest legal form - almost always the type's own factory over
 *    exactly the same arguments. When the legal form compiles and the illegal one does not, the
 *    difference between them is the only possible cause.
 *
 * One consequence of the build's `-Wunused` and `-Werror` settings is worth recording, because
 * it shapes every test below: a value referenced '''only''' inside a snippet string counts as
 * unused and fails the build. Every fixture here is therefore also used in ordinary code - which
 * is no loss, since that ordinary code is the `unapply` destructuring and the accessor reads the
 * construction policy calls for anyway.
 */
class ApiSurfaceSpec extends AnyFunSuite with Matchers {

  import ApiSurfaceSpec._

  //-------------------------------------------------------------------------
  // shared values, and the dates the fixtures are built from
  //-------------------------------------------------------------------------
  private val gbp: Currency = Currency.GBP
  private val usd: Currency = Currency.USD
  private val gbpUsd: CurrencyPair = CurrencyPair.of(gbp, usd)
  private val jan15: LocalDate = LocalDate.of(2020, 1, 15)
  private val apr15: LocalDate = LocalDate.of(2020, 4, 15)
  private val jul15: LocalDate = LocalDate.of(2020, 7, 15)
  private val jan15Next: LocalDate = LocalDate.of(2021, 1, 15)
  private val fixingDate: LocalDate = LocalDate.of(2020, 6, 10)
  private val gbloId: HolidayCalendarId = HolidayCalendarIds.GBLO
  private val refData: ReferenceData = ReferenceData.standard
  private val gblo: HolidayCalendar = valueOf(gbloId.resolve(refData))
  private val modifiedFollowing: BusinessDayConvention = BusinessDayConventions.MODIFIED_FOLLOWING
  private val adjustment: BusinessDayAdjustment = BusinessDayAdjustment.of(modifiedFollowing, gbloId)

  /**
   * Takes the value out of a factory result, failing the test with the reason where there is
   * none.
   *
   * The fixtures are built through the very factories the file is about, most of which report
   * their refusals rather than throwing. A fixture that cannot be built is a broken test rather
   * than a failed assertion, and this reports it as such - naming the failure, so that a fixture
   * broken by a change to a validation rule says which rule rejected it.
   *
   * @tparam E  the type of the failure the factory reports, either a `Failure` or a chain of them
   * @tparam A  the type being built
   * @param result  the result of the factory
   * @return the value the factory produced
   */
  private def valueOf[E, A](result: Either[E, A]): A =
    result match {
      case Right(value) => value
      case Left(failure) => fail(s"a fixture could not be built, the factory reported: $failure")
    }

  //-------------------------------------------------------------------------
  // one live instance of every type whose construction policy is asserted, each built through
  // the factory that type publishes - which is itself the first half of the proof, since a type
  // with no public constructor can be reached no other way
  //-------------------------------------------------------------------------
  private val standardId: StandardId = valueOf(StandardId.of("OG-Ticker", "AAPL"))
  private val country: Country = valueOf(Country.of("GB"))
  private val gbpUsdRate: FxRate = valueOf(FxRate.of(gbpUsd, 1.25))
  private val fxMatrix: FxMatrix = FxMatrix.of(gbpUsd, 1.25)
  private val currencyAmount: CurrencyAmount = valueOf(CurrencyAmount.of(gbp, 100.0))
  private val currencyAmountArray: CurrencyAmountArray =
    CurrencyAmountArray.of(gbp, DoubleArray.of(100.0, 200.0))
  private val multiCurrencyAmount: MultiCurrencyAmount = valueOf(MultiCurrencyAmount.of(currencyAmount))
  private val multiCurrencyAmountArray: MultiCurrencyAmountArray =
    MultiCurrencyAmountArray.of(multiCurrencyAmount)
  private val decimal: Decimal = valueOf(Decimal.of("12.34"))
  private val fixedScaleDecimal: FixedScaleDecimal = valueOf(FixedScaleDecimal.of(decimal, 4))
  private val money: Money = Money.of(gbp, decimal)
  private val bigMoney: BigMoney = BigMoney.of(gbp, decimal)
  private val tenor: Tenor = Tenor.TENOR_3M
  private val marketTenor: MarketTenor = valueOf(MarketTenor.ofSpot(tenor))
  private val frequency: Frequency = Frequency.P3M
  private val sequenceDate: SequenceDate = valueOf(SequenceDate.base(2))
  private val daysAdjustment: DaysAdjustment = DaysAdjustment.ofBusinessDays(2, gbloId)
  private val periodAdjustment: PeriodAdjustment =
    valueOf(PeriodAdjustment.of(Period.ofMonths(3), PeriodAdditionConventions.LAST_DAY, adjustment))
  private val tenorAdjustment: TenorAdjustment =
    valueOf(TenorAdjustment.of(tenor, PeriodAdditionConventions.LAST_DAY, adjustment))
  private val adjustableDates: AdjustableDates = valueOf(AdjustableDates.of(adjustment, jan15, apr15))
  private val schedulePeriod: SchedulePeriod = valueOf(SchedulePeriod.of(jan15, apr15))
  private val schedule: Schedule = Schedule.ofTerm(schedulePeriod)
  private val periodicSchedule: PeriodicSchedule =
    valueOf(PeriodicSchedule.of(jan15, jan15Next, frequency, adjustment))
  private val valueStep: ValueStep = ValueStep.of(apr15, ValueAdjustment.ofReplace(1000.0))
  private val valueStepSequence: ValueStepSequence =
    valueOf(ValueStepSequence.of(apr15, jul15, frequency, ValueAdjustment.ofDeltaAmount(100.0)))
  private val valueSchedule: ValueSchedule = ValueSchedule.of(1000.0, List(valueStep))
  private val halfUp: HalfUp = valueOf(HalfUp.ofFractionalDecimalPlaces(2, 4))
  private val bus252: DayCount = DayCount.ofBus252(gblo)
  private val compositeCalendarId: HolidayCalendarId = HolidayCalendarId.of("GBLO+USNY")
  private val immutableCalendar: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(
      HolidayCalendarId.of("APISURFACE"),
      List(jan15),
      List(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY))

  private val gbpLiborName: FloatingRateName = FloatingRateNames.GBP_LIBOR
  private val iborIndex: IborIndex = valueOf(IborIndex.parse("GBP-LIBOR-3M"))
  private val overnightIndex: OvernightIndex = valueOf(OvernightIndex.parse("GBP-SONIA"))
  private val priceIndex: PriceIndex = valueOf(PriceIndex.parse("GB-HICP"))
  private val fxIndex: FxIndex = valueOf(FxIndex.parse("EUR/GBP-ECB"))
  private val iborObservation: IborIndexObservation =
    valueOf(IborIndexObservation.of(iborIndex, fixingDate, refData))
  private val overnightObservation: OvernightIndexObservation =
    valueOf(OvernightIndexObservation.of(overnightIndex, fixingDate, refData))
  private val fxObservation: FxIndexObservation =
    valueOf(FxIndexObservation.of(fxIndex, fixingDate, refData))
  private val priceObservation: PriceIndexObservation =
    PriceIndexObservation.of(priceIndex, YearMonth.of(2020, 6))

  private val doubleArray: DoubleArray = DoubleArray.of(1.0, 2.0, 3.0)
  private val doubleMatrix: DoubleMatrix = DoubleMatrix.of(2, 2, 1.0, 2.0, 3.0, 4.0)

  //-------------------------------------------------------------------------
  // Group 1a - validated types of the root and currency packages, and of the collect module
  //
  // Each of these is a `[V]` type: its factory checks its inputs and reports what is wrong with
  // them, so a caller that could reach a constructor or a `copy` could build a value the factory
  // would have refused. The three assertions per type are the whole policy: the factory is the
  // only way in, the value cannot be modified into another one, and taking it apart still works.
  //-------------------------------------------------------------------------
  test("StandardId publishes no apply and no copy, and destructures through unapply") {
    val StandardId(scheme, value) = standardId
    scheme shouldBe "OG-Ticker"
    value shouldBe "AAPL"
    assertCompiles("""StandardId.of(standardId.scheme, standardId.value)""")
    assertTypeError("""StandardId(standardId.scheme, standardId.value)""")
    assertTypeError("""standardId.copy(value = standardId.value)""")
  }

  test("Country publishes no apply and no copy, and destructures through unapply") {
    val Country(code) = country
    code shouldBe "GB"
    assertCompiles("""Country.of(country.code)""")
    assertTypeError("""Country(country.code)""")
    assertTypeError("""country.copy(code = country.code)""")
  }

  test("FxRate publishes no apply and no copy, and destructures through unapply") {
    val FxRate(pair, rate) = gbpUsdRate
    pair shouldBe gbpUsd
    rate shouldBe 1.25
    assertCompiles("""FxRate.of(gbpUsdRate.pair, gbpUsdRate.rate)""")
    assertTypeError("""FxRate(gbpUsdRate.pair, gbpUsdRate.rate)""")
    assertTypeError("""gbpUsdRate.copy(rate = gbpUsdRate.rate)""")
  }

  test("FxMatrix publishes no apply and no copy, and destructures through unapply") {
    val FxMatrix(currencies, rates) = fxMatrix
    currencies should contain theSameElementsAs Vector(gbp, usd)
    rates.get(0, 0) shouldBe 1.0
    assertCompiles("""FxMatrix.fromMatrix(fxMatrix.currencies, fxMatrix.rates)""")
    assertTypeError("""FxMatrix(fxMatrix.currencies, fxMatrix.rates)""")
    assertTypeError("""fxMatrix.copy(rates = fxMatrix.rates)""")
  }

  test("CurrencyAmountArray publishes no apply and no copy, and destructures through unapply") {
    val CurrencyAmountArray(currency, values) = currencyAmountArray
    currency shouldBe gbp
    values.get(1) shouldBe 200.0
    assertCompiles("""CurrencyAmountArray.of(currencyAmountArray.currency, currencyAmountArray.values)""")
    assertTypeError("""CurrencyAmountArray(currencyAmountArray.currency, currencyAmountArray.values)""")
    assertTypeError("""currencyAmountArray.copy(currency = currencyAmountArray.currency)""")
  }

  test("MultiCurrencyAmountArray publishes no apply and no copy, and destructures through unapply") {
    val MultiCurrencyAmountArray(size, values) = multiCurrencyAmountArray
    size shouldBe 1
    values.keySet shouldBe Set(gbp)
    assertCompiles("""MultiCurrencyAmountArray.of(multiCurrencyAmountArray.values)""")
    assertTypeError("""MultiCurrencyAmountArray(multiCurrencyAmountArray.size, multiCurrencyAmountArray.values)""")
    assertTypeError("""multiCurrencyAmountArray.copy(size = multiCurrencyAmountArray.size)""")
  }

  test("FixedScaleDecimal publishes no apply and no copy, and destructures through unapply") {
    val FixedScaleDecimal(value, fixedScale) = fixedScaleDecimal
    value shouldBe decimal
    fixedScale shouldBe 4
    assertCompiles("""FixedScaleDecimal.of(fixedScaleDecimal.decimal, fixedScaleDecimal.fixedScale)""")
    assertTypeError("""FixedScaleDecimal(fixedScaleDecimal.decimal, fixedScaleDecimal.fixedScale)""")
    assertTypeError("""fixedScaleDecimal.copy(fixedScale = fixedScaleDecimal.fixedScale)""")
  }

  //-------------------------------------------------------------------------
  // Group 1b - validated types of the date, schedule, value and index packages
  //-------------------------------------------------------------------------
  test("MarketTenor publishes no apply and no copy, and destructures through unapply") {
    val MarketTenor(code) = marketTenor
    code shouldBe "3M"
    assertCompiles("""MarketTenor.parse(marketTenor.code)""")
    assertTypeError("""MarketTenor(marketTenor.code)""")
    assertTypeError("""marketTenor.copy(code = marketTenor.code)""")
  }

  test("DaysAdjustment publishes no apply and no copy, and destructures through unapply") {
    val DaysAdjustment(days, calendar, adjust) = daysAdjustment
    days shouldBe 2
    calendar shouldBe gbloId
    adjust shouldBe BusinessDayAdjustment.NONE
    assertCompiles(
      """DaysAdjustment.ofBusinessDays(
         |  daysAdjustment.days,
         |  daysAdjustment.calendar,
         |  daysAdjustment.adjustment)""".stripMargin)
    assertTypeError(
      """DaysAdjustment(
         |  daysAdjustment.days,
         |  daysAdjustment.calendar,
         |  daysAdjustment.adjustment)""".stripMargin)
    assertTypeError("""daysAdjustment.copy(days = daysAdjustment.days)""")
  }

  test("PeriodAdjustment publishes no apply and no copy, and destructures through unapply") {
    val PeriodAdjustment(period, additionConvention, adjust) = periodAdjustment
    period shouldBe Period.ofMonths(3)
    additionConvention shouldBe PeriodAdditionConventions.LAST_DAY
    adjust shouldBe adjustment
    assertCompiles(
      """PeriodAdjustment.of(
         |  periodAdjustment.period,
         |  periodAdjustment.additionConvention,
         |  periodAdjustment.adjustment)""".stripMargin)
    assertTypeError(
      """PeriodAdjustment(
         |  periodAdjustment.period,
         |  periodAdjustment.additionConvention,
         |  periodAdjustment.adjustment)""".stripMargin)
    assertTypeError("""periodAdjustment.copy(period = periodAdjustment.period)""")
  }

  test("TenorAdjustment publishes no apply and no copy, and destructures through unapply") {
    val TenorAdjustment(adjustedTenor, additionConvention, adjust) = tenorAdjustment
    adjustedTenor shouldBe tenor
    additionConvention shouldBe PeriodAdditionConventions.LAST_DAY
    adjust shouldBe adjustment
    assertCompiles(
      """TenorAdjustment.of(
         |  tenorAdjustment.tenor,
         |  tenorAdjustment.additionConvention,
         |  tenorAdjustment.adjustment)""".stripMargin)
    assertTypeError(
      """TenorAdjustment(
         |  tenorAdjustment.tenor,
         |  tenorAdjustment.additionConvention,
         |  tenorAdjustment.adjustment)""".stripMargin)
    assertTypeError("""tenorAdjustment.copy(tenor = tenorAdjustment.tenor)""")
  }

  test("AdjustableDates publishes no apply and no copy, and destructures through unapply") {
    val AdjustableDates(unadjusted, adjust) = adjustableDates
    unadjusted.toList shouldBe List(jan15, apr15)
    adjust shouldBe adjustment
    assertCompiles("""AdjustableDates.of(adjustableDates.adjustment, adjustableDates.unadjusted)""")
    assertTypeError("""AdjustableDates(adjustableDates.unadjusted, adjustableDates.adjustment)""")
    assertTypeError("""adjustableDates.copy(adjustment = adjustableDates.adjustment)""")
  }

  test("SchedulePeriod publishes no apply and no copy, and destructures through unapply") {
    val SchedulePeriod(startDate, endDate, unadjustedStartDate, unadjustedEndDate) = schedulePeriod
    startDate shouldBe jan15
    endDate shouldBe apr15
    unadjustedStartDate shouldBe jan15
    unadjustedEndDate shouldBe apr15
    assertCompiles(
      """SchedulePeriod.of(
         |  schedulePeriod.startDate,
         |  schedulePeriod.endDate,
         |  schedulePeriod.unadjustedStartDate,
         |  schedulePeriod.unadjustedEndDate)""".stripMargin)
    assertTypeError(
      """SchedulePeriod(
         |  schedulePeriod.startDate,
         |  schedulePeriod.endDate,
         |  schedulePeriod.unadjustedStartDate,
         |  schedulePeriod.unadjustedEndDate)""".stripMargin)
    assertTypeError("""schedulePeriod.copy(endDate = schedulePeriod.endDate)""")
  }

  test("Schedule publishes no apply and no copy, and destructures through unapply") {
    val Schedule(periods, periodicFrequency, rollConvention) = schedule
    periods.toList shouldBe List(schedulePeriod)
    periodicFrequency shouldBe Frequency.TERM
    rollConvention shouldBe RollConventions.NONE
    assertCompiles(
      """Schedule.of(schedule.periods, schedule.periodicFrequency, schedule.rollConvention)""")
    assertTypeError(
      """Schedule(schedule.periods, schedule.periodicFrequency, schedule.rollConvention)""")
    assertTypeError("""schedule.copy(rollConvention = schedule.rollConvention)""")
  }

  test("PeriodicSchedule publishes no apply and no copy, and destructures through unapply") {
    val PeriodicSchedule(
      startDate,
      endDate,
      scheduleFrequency,
      businessDayAdjustment,
      startDateBusinessDayAdjustment,
      endDateBusinessDayAdjustment,
      stubConvention,
      rollConvention,
      firstRegularStartDate,
      lastRegularEndDate,
      overrideStartDate) = periodicSchedule
    startDate shouldBe jan15
    endDate shouldBe jan15Next
    scheduleFrequency shouldBe frequency
    businessDayAdjustment shouldBe adjustment
    startDateBusinessDayAdjustment shouldBe None
    endDateBusinessDayAdjustment shouldBe None
    stubConvention shouldBe None
    rollConvention shouldBe None
    firstRegularStartDate shouldBe None
    lastRegularEndDate shouldBe None
    overrideStartDate shouldBe None
    assertCompiles(
      """PeriodicSchedule.of(
         |  periodicSchedule.startDate,
         |  periodicSchedule.endDate,
         |  periodicSchedule.frequency,
         |  periodicSchedule.businessDayAdjustment,
         |  periodicSchedule.startDateBusinessDayAdjustment,
         |  periodicSchedule.endDateBusinessDayAdjustment,
         |  periodicSchedule.stubConvention,
         |  periodicSchedule.rollConvention,
         |  periodicSchedule.firstRegularStartDate,
         |  periodicSchedule.lastRegularEndDate,
         |  periodicSchedule.overrideStartDate)""".stripMargin)
    assertTypeError(
      """PeriodicSchedule(
         |  periodicSchedule.startDate,
         |  periodicSchedule.endDate,
         |  periodicSchedule.frequency,
         |  periodicSchedule.businessDayAdjustment,
         |  periodicSchedule.startDateBusinessDayAdjustment,
         |  periodicSchedule.endDateBusinessDayAdjustment,
         |  periodicSchedule.stubConvention,
         |  periodicSchedule.rollConvention,
         |  periodicSchedule.firstRegularStartDate,
         |  periodicSchedule.lastRegularEndDate,
         |  periodicSchedule.overrideStartDate)""".stripMargin)
    assertTypeError("""periodicSchedule.copy(endDate = periodicSchedule.endDate)""")
  }

  test("ValueStep publishes no apply and no copy, and destructures through unapply") {
    val ValueStep(periodIndex, date, value) = valueStep
    periodIndex shouldBe None
    date shouldBe Some(apr15)
    value shouldBe ValueAdjustment.ofReplace(1000.0)
    assertCompiles("""ValueStep.of(valueStep.periodIndex, valueStep.date, valueStep.value)""")
    assertTypeError("""ValueStep(valueStep.periodIndex, valueStep.date, valueStep.value)""")
    assertTypeError("""valueStep.copy(date = valueStep.date)""")
  }

  test("ValueSchedule publishes no apply and no copy, and destructures through unapply") {
    val ValueSchedule(initialValue, steps, stepSequence) = valueSchedule
    initialValue shouldBe 1000.0
    steps shouldBe List(valueStep)
    stepSequence shouldBe None
    assertCompiles(
      """ValueSchedule.of(valueSchedule.initialValue, valueSchedule.steps, valueSchedule.stepSequence)""")
    assertTypeError(
      """ValueSchedule(valueSchedule.initialValue, valueSchedule.steps, valueSchedule.stepSequence)""")
    assertTypeError("""valueSchedule.copy(initialValue = valueSchedule.initialValue)""")
  }

  test("ValueStepSequence publishes no apply and no copy, and destructures through unapply") {
    val ValueStepSequence(firstStepDate, lastStepDate, sequenceFrequency, adjust) = valueStepSequence
    firstStepDate shouldBe apr15
    lastStepDate shouldBe jul15
    sequenceFrequency shouldBe frequency
    adjust shouldBe ValueAdjustment.ofDeltaAmount(100.0)
    assertCompiles(
      """ValueStepSequence.of(
         |  valueStepSequence.firstStepDate,
         |  valueStepSequence.lastStepDate,
         |  valueStepSequence.frequency,
         |  valueStepSequence.adjustment)""".stripMargin)
    assertTypeError(
      """ValueStepSequence(
         |  valueStepSequence.firstStepDate,
         |  valueStepSequence.lastStepDate,
         |  valueStepSequence.frequency,
         |  valueStepSequence.adjustment)""".stripMargin)
    assertTypeError("""valueStepSequence.copy(frequency = valueStepSequence.frequency)""")
  }

  test("HalfUp publishes no apply and no copy, and destructures through unapply") {
    val HalfUp(decimalPlaces, fraction) = halfUp
    decimalPlaces shouldBe 2
    fraction shouldBe 4
    assertCompiles("""HalfUp.ofFractionalDecimalPlaces(halfUp.decimalPlaces, halfUp.fraction)""")
    assertTypeError("""HalfUp(halfUp.decimalPlaces, halfUp.fraction)""")
    assertTypeError("""halfUp.copy(fraction = halfUp.fraction)""")
  }

  test("IborIndexObservation publishes no apply and no copy, and destructures through unapply") {
    val IborIndexObservation(index, fixing, effectiveDate, maturityDate, yearFraction) = iborObservation
    index shouldBe iborIndex
    fixing shouldBe fixingDate
    effectiveDate.isBefore(maturityDate) shouldBe true
    yearFraction should be > 0.0
    assertCompiles("""IborIndexObservation.of(iborObservation.index, iborObservation.fixingDate, refData)""")
    assertTypeError(
      """IborIndexObservation(
         |  iborObservation.index,
         |  iborObservation.fixingDate,
         |  iborObservation.effectiveDate,
         |  iborObservation.maturityDate,
         |  iborObservation.yearFraction)""".stripMargin)
    assertTypeError("""iborObservation.copy(fixingDate = iborObservation.fixingDate)""")
  }

  test("OvernightIndexObservation publishes no apply and no copy, and destructures through unapply") {
    val OvernightIndexObservation(
      index,
      fixing,
      publicationDate,
      effectiveDate,
      maturityDate,
      yearFraction) = overnightObservation
    index shouldBe overnightIndex
    fixing shouldBe fixingDate
    publicationDate.isBefore(fixing) shouldBe false
    effectiveDate.isBefore(maturityDate) shouldBe true
    yearFraction should be > 0.0
    assertCompiles(
      """OvernightIndexObservation.of(
         |  overnightObservation.index,
         |  overnightObservation.fixingDate,
         |  refData)""".stripMargin)
    assertTypeError(
      """OvernightIndexObservation(
         |  overnightObservation.index,
         |  overnightObservation.fixingDate,
         |  overnightObservation.publicationDate,
         |  overnightObservation.effectiveDate,
         |  overnightObservation.maturityDate,
         |  overnightObservation.yearFraction)""".stripMargin)
    assertTypeError("""overnightObservation.copy(fixingDate = overnightObservation.fixingDate)""")
  }

  test("FxIndexObservation publishes no apply and no copy, and destructures through unapply") {
    val FxIndexObservation(index, fixing, maturityDate) = fxObservation
    index shouldBe fxIndex
    fixing shouldBe fixingDate
    maturityDate.isAfter(fixingDate) shouldBe true
    assertCompiles("""FxIndexObservation.of(fxObservation.index, fxObservation.fixingDate, refData)""")
    assertTypeError(
      """FxIndexObservation(
         |  fxObservation.index,
         |  fxObservation.fixingDate,
         |  fxObservation.maturityDate)""".stripMargin)
    assertTypeError("""fxObservation.copy(fixingDate = fxObservation.fixingDate)""")
  }

  test("DayCount.Bus252 is reached only through ofBus252, with no apply and no copy") {
    // `Bus252` is the one `[V]` member of this port that is a plain `final class` rather than a
    // `sealed abstract case class`: it is a member of a sealed family whose equality, ordering and
    // rendering all come from the family's name, so the structural equality a case class would
    // synthesise would contradict the family. Its constructor is `private[DayCount]`, which is
    // what closes the two routes below; the calendar it holds is read through its accessor.
    bus252.name shouldBe "Bus/252 GBLO"
    assertCompiles("""DayCount.ofBus252(gblo)""")
    assertCompiles("""DayCount.ofBus252(gbloId, refData)""")
    assertTypeError("""DayCount.Bus252(gblo)""")
    assertTypeError("""new DayCount.Bus252(gblo)""")
  }

  //-------------------------------------------------------------------------
  // Group 2 - normalising types
  //
  // A `[N]` type rewrites what it is given as well as checking it: an amount of `-0.0` becomes
  // `0.0`, a period is normalised, a composite calendar identifier has its components sorted and
  // deduplicated, a money amount is rounded to the currency's minor units. A constructor or a
  // `copy` would hand back a value that had skipped the rewrite and would therefore compare
  // unequal to the same value built properly, which is the sharpest reason these types publish
  // neither.
  //-------------------------------------------------------------------------
  test("CurrencyAmount publishes no apply and no copy, and destructures through unapply") {
    val CurrencyAmount(currency, amount) = currencyAmount
    currency shouldBe gbp
    amount shouldBe 100.0
    assertCompiles("""CurrencyAmount.of(currencyAmount.currency, currencyAmount.amount)""")
    assertTypeError("""CurrencyAmount(currencyAmount.currency, currencyAmount.amount)""")
    assertTypeError("""currencyAmount.copy(amount = currencyAmount.amount)""")
  }

  test("Money publishes no apply and no copy, and destructures through unapply") {
    val Money(currency, amount) = money
    currency shouldBe gbp
    amount shouldBe decimal
    assertCompiles("""Money.of(money.currency, money.amount)""")
    assertTypeError("""Money(money.currency, money.amount)""")
    assertTypeError("""money.copy(amount = money.amount)""")
  }

  test("BigMoney publishes no apply and no copy, and destructures through unapply") {
    val BigMoney(currency, amount) = bigMoney
    currency shouldBe gbp
    amount shouldBe decimal
    assertCompiles("""BigMoney.of(bigMoney.currency, bigMoney.amount)""")
    assertTypeError("""BigMoney(bigMoney.currency, bigMoney.amount)""")
    assertTypeError("""bigMoney.copy(amount = bigMoney.amount)""")
  }

  test("MultiCurrencyAmount publishes no apply and no copy, and destructures through unapply") {
    val MultiCurrencyAmount(amounts) = multiCurrencyAmount
    amounts.keySet shouldBe Set(gbp)
    amounts(gbp) shouldBe 100.0
    assertCompiles("""MultiCurrencyAmount.of(multiCurrencyAmount.amounts)""")
    assertTypeError("""MultiCurrencyAmount(multiCurrencyAmount.amounts)""")
    assertTypeError("""multiCurrencyAmount.copy(amounts = multiCurrencyAmount.amounts)""")
  }

  test("Tenor publishes no apply and no copy, and destructures through unapply") {
    val Tenor(period) = tenor
    period shouldBe Period.ofMonths(3)
    assertCompiles("""Tenor.of(tenor.period)""")
    assertTypeError("""Tenor(tenor.period)""")
    assertTypeError("""tenor.copy(period = tenor.period)""")
  }

  test("Frequency publishes no apply and no copy, and destructures through unapply") {
    val Frequency(period) = frequency
    period shouldBe Period.ofMonths(3)
    assertCompiles("""Frequency.of(frequency.period)""")
    assertTypeError("""Frequency(frequency.period)""")
    assertTypeError("""frequency.copy(period = frequency.period)""")
  }

  test("SequenceDate publishes no apply and no copy, and destructures through unapply") {
    val SequenceDate(yearMonth, minimumPeriod, sequenceNumber, fullSequence) = sequenceDate
    yearMonth shouldBe None
    minimumPeriod shouldBe None
    sequenceNumber shouldBe 2
    fullSequence shouldBe false
    assertCompiles(
      """SequenceDate.of(
         |  sequenceDate.yearMonth,
         |  sequenceDate.minimumPeriod,
         |  sequenceDate.sequenceNumber,
         |  sequenceDate.fullSequence)""".stripMargin)
    assertTypeError(
      """SequenceDate(
         |  sequenceDate.yearMonth,
         |  sequenceDate.minimumPeriod,
         |  sequenceDate.sequenceNumber,
         |  sequenceDate.fullSequence)""".stripMargin)
    assertTypeError("""sequenceDate.copy(sequenceNumber = sequenceDate.sequenceNumber)""")
  }

  test("HolidayCalendarId publishes no apply and no copy, and destructures through unapply") {
    val HolidayCalendarId(name) = compositeCalendarId
    name shouldBe "GBLO+USNY"
    assertCompiles("""HolidayCalendarId.of(compositeCalendarId.name)""")
    assertTypeError("""HolidayCalendarId(compositeCalendarId.name)""")
    assertTypeError("""compositeCalendarId.copy(name = compositeCalendarId.name)""")
  }

  test("ImmutableHolidayCalendar is reached only through of, with no apply and no copy") {
    // This is the second of the two `[N]` types represented as a plain `final class` rather than
    // as a `sealed abstract case class`, and for a reason the type itself makes plain: two of its
    // four fields are the packed monthly bit masks it answers from, which no caller may see, and
    // its equality is the equality of its identifier, as in the bean being ported, rather than
    // the structural one a case class would synthesise. A case class cannot hide a field or
    // redefine equality without contradicting itself, so the data is read back through the
    // accessors below and there is deliberately no `unapply`.
    immutableCalendar.id shouldBe HolidayCalendarId.of("APISURFACE")
    immutableCalendar.holidays.toList shouldBe List(jan15)
    immutableCalendar.weekendDays shouldBe Set(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY)
    assertCompiles(
      """ImmutableHolidayCalendar.of(
         |  immutableCalendar.id,
         |  immutableCalendar.holidays,
         |  immutableCalendar.weekendDays)""".stripMargin)
    assertTypeError(
      """ImmutableHolidayCalendar(
         |  immutableCalendar.id,
         |  immutableCalendar.holidays,
         |  immutableCalendar.weekendDays)""".stripMargin)
    assertTypeError(
      """new ImmutableHolidayCalendar(
         |  immutableCalendar.id,
         |  immutableCalendar.holidays,
         |  immutableCalendar.weekendDays)""".stripMargin)
    assertTypeError("""immutableCalendar.copy(id = immutableCalendar.id)""")
  }

  test("Decimal publishes no apply and no copy, and destructures through unapply") {
    val Decimal(unscaled, scale) = decimal
    unscaled shouldBe 1234L
    scale shouldBe 2
    assertCompiles("""Decimal.ofScaled(decimal.unscaled, decimal.scale)""")
    assertTypeError("""Decimal(decimal.unscaled, decimal.scale)""")
    assertTypeError("""decimal.copy(scale = decimal.scale)""")
  }

  //-------------------------------------------------------------------------
  // Group 3 - the total types, which deliberately keep the case class surface
  //
  // The construction policy is not "lock everything down": a `[T]` type accepts every well-typed
  // input, has nothing to check and nothing to rewrite, and therefore keeps the ordinary case
  // class surface - `apply`, `copy` and `unapply` - because hiding it would cost callers
  // convenience and buy no invariant. These two assertions mark that boundary, so that a later
  // tidy-up which "made every type consistent" by locking these down would fail here and be
  // discussed rather than absorbed.
  //-------------------------------------------------------------------------
  test("CurrencyPair is a total type and keeps its public apply and copy") {
    val CurrencyPair(base, counter) = gbpUsd
    base shouldBe gbp
    counter shouldBe usd
    CurrencyPair(gbp, usd) shouldBe gbpUsd
    gbpUsd.copy(counter = gbp) shouldBe CurrencyPair.of(gbp, gbp)
    assertCompiles("""CurrencyPair(gbpUsd.base, gbpUsd.counter)""")
    assertCompiles("""gbpUsd.copy(base = gbpUsd.counter)""")
  }

  test("PriceIndexObservation is a total type and keeps its public apply and copy") {
    val PriceIndexObservation(index, fixingMonth) = priceObservation
    index shouldBe priceIndex
    fixingMonth shouldBe YearMonth.of(2020, 6)
    PriceIndexObservation(priceIndex, YearMonth.of(2020, 6)) shouldBe priceObservation
    priceObservation.copy(fixingMonth = YearMonth.of(2020, 7)).fixingMonth shouldBe YearMonth.of(2020, 7)
    assertCompiles("""PriceIndexObservation(priceObservation.index, priceObservation.fixingMonth)""")
    assertCompiles("""priceObservation.copy(fixingMonth = priceObservation.fixingMonth)""")
  }

  //-------------------------------------------------------------------------
  // Group 4 - the private constructors themselves
  //
  // Hiding `apply` would buy nothing if the constructor were reachable, and the representation
  // these types use - `sealed abstract case class X private (...)`, instantiated as `new X(...) {}`
  // inside the companion - makes it easy to assume the anonymous-subclass route is available to
  // everyone. It is not, and this is the proof, taken across every package that has such a type
  // so that no one package can drift on its own.
  //-------------------------------------------------------------------------
  test("no validated or normalising type can be instantiated through its private constructor") {
    assertTypeError("""new StandardId(standardId.scheme, standardId.value) {}""")
    assertTypeError("""new Country(country.code) {}""")
    assertTypeError("""new CurrencyAmount(currencyAmount.currency, currencyAmount.amount) {}""")
    assertTypeError("""new FxRate(gbpUsdRate.pair, gbpUsdRate.rate) {}""")
    assertTypeError("""new Tenor(tenor.period) {}""")
    assertTypeError("""new HolidayCalendarId(compositeCalendarId.name) {}""")
    assertTypeError("""new Frequency(frequency.period) {}""")
    assertTypeError("""new HalfUp(halfUp.decimalPlaces, halfUp.fraction) {}""")
    assertTypeError("""new Decimal(decimal.unscaled, decimal.scale) {}""")
    assertTypeError("""new FixedScaleDecimal(fixedScaleDecimal.decimal, fixedScaleDecimal.fixedScale) {}""")
  }

  test("no field of a published value can be assigned, whatever its kind") {
    // This is the one group written with `assertDoesNotCompile` rather than `assertTypeError`, and
    // deliberately: the compiler's objection to an assignment depends on the shape of the target -
    // a missing `_=` member for an accessor, a non-assignable expression for an application - and
    // the weaker assertion accepts either, where the stricter one would tie the test to whichever
    // diagnostic this compiler version happens to produce. Everywhere the shape of the rejection
    // is the point - a missing `apply`, a missing `copy`, an inaccessible member, an illegal
    // inheritance - the file uses `assertTypeError` instead.
    //
    // Accessors are read back first, so that a failure here means the field stopped being
    // read-only rather than that it stopped existing.
    currencyAmount.amount shouldBe 100.0
    schedulePeriod.startDate shouldBe jan15
    iborIndex.name shouldBe "GBP-LIBOR-3M"
    doubleArray.get(0) shouldBe 1.0
    assertDoesNotCompile("""currencyAmount.amount = 200.0""")
    assertDoesNotCompile("""schedulePeriod.startDate = apr15""")
    assertDoesNotCompile("""iborIndex.name = "Bespoke"""")
    assertDoesNotCompile("""immutableCalendar.startYear = 2021""")
    assertDoesNotCompile("""doubleArray.get(0) = 99.0""")
  }

  //-------------------------------------------------------------------------
  // Group 5 - the closed families
  //
  // The implementation being ported resolved a convention or an index through a registry that a
  // configuration file on the classpath could add to at run time. The port replaced that with
  // sealed families whose members exist only in their companions, which is what lets a match over
  // a family be checked for exhaustiveness and what makes `values` the whole truth about it.
  //
  // Scala 2 enforces `sealed` per '''file''': a direct subtype must be declared in the file that
  // declares the family. That is why `index/Index.scala` carries the whole index hierarchy and
  // `date/HolidayCalendar.scala` every kind of calendar, rather than one file per type - and it
  // is also what makes the proof below possible, since this file is not any of those files. Each
  // test states the family's own members are reachable, then that a new one cannot be declared
  // here. Where a family's constructor is package-private as well, both that and the `sealed`
  // modifier stand in the way, and the comment says so.
  //-------------------------------------------------------------------------
  test("the Index family is closed against a new index declared outside Index.scala") {
    (iborIndex: Index).name shouldBe "GBP-LIBOR-3M"
    Index.parse("GBP-LIBOR-3M") shouldBe Right(iborIndex)
    assertTypeError("""{ final class Host extends Index { def name: String = "Bespoke-Index" }; () }""")
  }

  test("the FloatingRateIndex family is closed against a new index declared outside Index.scala") {
    (priceIndex: FloatingRateIndex).currency shouldBe gbp
    assertTypeError(
      """{
         |  final class Host extends FloatingRateIndex {
         |    def name: String = "Bespoke-Floating-Rate-Index"
         |    def currency: Currency = gbp
         |    def active: Boolean = true
         |    def dayCount: DayCount = DayCounts.ACT_365F
         |    def floatingRateName: FloatingRateName = FloatingRateNames.GBP_LIBOR
         |  }
         |  ()
         |}""".stripMargin)
  }

  test("the RateIndex family is closed against a new index declared outside Index.scala") {
    (overnightIndex: RateIndex).fixingCalendar shouldBe gbloId
    assertTypeError(
      """{
         |  final class Host extends RateIndex {
         |    def name: String = "Bespoke-Rate-Index"
         |    def currency: Currency = gbp
         |    def active: Boolean = true
         |    def dayCount: DayCount = DayCounts.ACT_365F
         |    def fixingCalendar: HolidayCalendarId = gbloId
         |    def tenor: Tenor = Tenor.TENOR_3M
         |    def floatingRateName: FloatingRateName = FloatingRateNames.GBP_LIBOR
         |  }
         |  ()
         |}""".stripMargin)
  }

  test("the IborIndex family is closed against a new index declared outside Index.scala") {
    // The constructor is `private[index]` as well as the class being sealed, so the arguments
    // below - every one of them read off a published index, so the arity and the types are
    // exactly right - can be neither reached nor extended from here.
    iborIndex.tenor shouldBe tenor
    assertTypeError(
      """{
         |  final class Host extends IborIndex(
         |      iborIndex.name,
         |      iborIndex.currency,
         |      iborIndex.active,
         |      iborIndex.fixingCalendar,
         |      iborIndex.fixingTime,
         |      iborIndex.fixingZone,
         |      iborIndex.fixingDateOffset,
         |      iborIndex.effectiveDateOffset,
         |      iborIndex.maturityDateOffset,
         |      iborIndex.dayCount,
         |      iborIndex.defaultFixedLegDayCount)
         |  ()
         |}""".stripMargin)
  }

  test("the OvernightIndex family is closed against a new index declared outside Index.scala") {
    overnightIndex.currency shouldBe gbp
    assertTypeError(
      """{
         |  final class Host extends OvernightIndex(
         |      overnightIndex.name,
         |      overnightIndex.currency,
         |      overnightIndex.active,
         |      overnightIndex.fixingCalendar,
         |      overnightIndex.publicationDateOffset,
         |      overnightIndex.effectiveDateOffset,
         |      overnightIndex.dayCount,
         |      overnightIndex.defaultFixedLegDayCount)
         |  ()
         |}""".stripMargin)
  }

  test("the PriceIndex family is closed against a new index declared outside Index.scala") {
    priceIndex.region shouldBe valueOf(Country.of("GB"))
    assertTypeError(
      """{
         |  final class Host extends PriceIndex(
         |      priceIndex.name,
         |      priceIndex.currency,
         |      priceIndex.region,
         |      priceIndex.active,
         |      priceIndex.publicationFrequency)
         |  ()
         |}""".stripMargin)
  }

  test("the FxIndex family is closed against a new index declared outside Index.scala") {
    fxIndex.currencyPair.base shouldBe Currency.EUR
    assertTypeError(
      """{
         |  final class Host extends FxIndex(
         |      fxIndex.name,
         |      fxIndex.currencyPair,
         |      fxIndex.fixingCalendar,
         |      fxIndex.maturityDateOffset)
         |  ()
         |}""".stripMargin)
  }

  test("the HolidayCalendar family is closed against a calendar declared outside HolidayCalendar.scala") {
    HolidayCalendars.SAT_SUN.name shouldBe "Sat/Sun"
    gblo.isHoliday(LocalDate.of(2020, 12, 25)) shouldBe true
    assertTypeError(
      """{
         |  final class Host extends HolidayCalendar {
         |    def id: HolidayCalendarId = gbloId
         |    def isHoliday(date: LocalDate): Boolean = date.getDayOfMonth == 1
         |  }
         |  ()
         |}""".stripMargin)
  }

  test("the DayCount family is closed against a convention declared outside DayCount.scala") {
    // Three things forbid this at once, and all three are deliberate: the class is sealed, its
    // constructor is `private[date]`, and the two methods a member has to supply -
    // `calculateYearFraction` and `calculateDays` - are `protected[date]`, so a subtype declared
    // anywhere else could not implement them even if it could reach the constructor.
    DayCounts.ACT_365F.name shouldBe "Act/365F"
    assertTypeError("""{ final class Host extends DayCount("Bespoke-Day-Count"); () }""")
  }

  test("the Rounding family is closed against a convention declared outside Rounding.scala") {
    val rounding: Rounding = Rounding.of(gbp)
    rounding.round(1.2345) shouldBe 1.23
    assertTypeError(
      """{
         |  final class Host extends Rounding {
         |    def round(value: BigDecimal): BigDecimal = value
         |    def round(value: Decimal): Decimal = value
         |  }
         |  ()
         |}""".stripMargin)
  }

  test("the Currency family is closed against a currency declared outside Currency.scala") {
    gbp.minorUnitDigits shouldBe 2
    assertTypeError("""{ final class Host extends Currency("XYZ", 2, "USD"); () }""")
  }

  test("the BusinessDayConvention family is closed against a convention declared outside its file") {
    modifiedFollowing.adjust(jan15, gblo) shouldBe jan15
    assertTypeError(
      """{
         |  final class Host extends BusinessDayConvention("Bespoke-Business-Day-Convention") {
         |    def adjust(date: LocalDate, calendar: HolidayCalendar): LocalDate =
         |      if (calendar.isHoliday(date)) date.plusDays(1L) else date
         |  }
         |  ()
         |}""".stripMargin)
  }

  test("the RollConvention family is closed against a convention declared outside its file") {
    RollConventions.EOM.adjust(jan15) shouldBe LocalDate.of(2020, 1, 31)
    assertTypeError(
      """{
         |  final class Host extends RollConvention("Bespoke-Roll-Convention") {
         |    def adjust(date: LocalDate): LocalDate = date
         |  }
         |  ()
         |}""".stripMargin)
  }

  test("the PeriodAdditionConvention family is closed against a convention declared outside its file") {
    (PeriodAdditionConventions.LAST_DAY: PeriodAdditionConvention).isMonthBased shouldBe true
    assertTypeError(
      """{
         |  final class Host extends PeriodAdditionConvention("Bespoke-Period-Addition-Convention") {
         |    def adjust(baseDate: LocalDate, period: Period, calendar: HolidayCalendar): LocalDate =
         |      baseDate.plus(period)
         |    def isMonthBased: Boolean = true
         |  }
         |  ()
         |}""".stripMargin)
  }

  test("the DateSequence family is closed against a sequence declared outside its file") {
    (DateSequences.QUARTERLY_IMM: DateSequence).name shouldBe "Quarterly-IMM"
    assertTypeError(
      """{
         |  final class Host extends DateSequence("Bespoke-Date-Sequence") {
         |    def nextOrSame(date: LocalDate): LocalDate = date.withDayOfMonth(1)
         |    def dateMatching(yearMonth: YearMonth): LocalDate = yearMonth.atDay(1)
         |  }
         |  ()
         |}""".stripMargin)
  }

  test("the StubConvention family is closed against a convention declared outside its file") {
    StubConvention.SHORT_INITIAL.isCalculateBackwards shouldBe true
    assertTypeError("""{ final class Host extends StubConvention("Bespoke-Stub-Convention"); () }""")
  }

  test("the FloatingRateType family is closed against a type declared outside its file") {
    FloatingRateType.Ibor.isIbor shouldBe true
    assertTypeError("""{ final class Host extends FloatingRateType("Bespoke-Floating-Rate-Type"); () }""")
  }

  test("the ValueAdjustmentType family is closed against a type declared outside its file") {
    ValueAdjustmentType.Replace.adjust(100.0, 200.0) shouldBe 200.0
    assertTypeError(
      """{
         |  final class Host extends ValueAdjustmentType("Bespoke-Value-Adjustment-Type") {
         |    def adjust(baseValue: Double, modifyingValue: Double): Double =
         |      baseValue + modifyingValue
         |  }
         |  ()
         |}""".stripMargin)
  }

  test("the FloatingRateName family is closed against a name declared outside its file") {
    gbpLiborName.externalName shouldBe "GBP-LIBOR"
    assertTypeError(
      """{
         |  final class Host extends FloatingRateName(
         |      gbpLiborName.externalName,
         |      gbpLiborName.indexName,
         |      gbpLiborName.rateType,
         |      gbpLiborName.fixingDateOffsetDays)
         |  ()
         |}""".stripMargin)
  }

  test("the Failure family of the collect module is closed against a failure declared here") {
    Failure.Invalid("a reason").reason shouldBe FailureReason.INVALID
    assertTypeError(
      """{
         |  final case class Host(message: String) extends Failure {
         |    def reason: FailureReason = FailureReason.ERROR
         |    def attributes: scala.collection.immutable.SortedMap[String, String] =
         |      scala.collection.immutable.SortedMap.empty[String, String]
         |  }
         |  ()
         |}""".stripMargin)
  }

  test("the FailureReason family of the collect module is closed against a reason declared here") {
    FailureReason.MISSING_DATA.name shouldBe "MISSING_DATA"
    assertTypeError("""{ final class Host extends FailureReason("BESPOKE"); () }""")
  }

  //-------------------------------------------------------------------------
  // Group 6 - the contracts that are deliberately open
  //
  // Sealing is not free, and two traits of this module pay a price that is too high. An
  // application supplies its own holidays by implementing `ReferenceData`, which is what the
  // library being ported documented and what the module's own `HolidaySafeReferenceData` does, so
  // sealing it would make both impossible. `FloatingRate` is implemented by `FloatingRateName` as
  // well as by the index families, and sealing it would drag `FloatingRateName` and its four
  // hundred rows of data into `Index.scala`. `IndexObservation` is open for the same reason: the
  // four observation types have a file each, and Scala 2 would require them all to share the
  // trait's file.
  //
  // The rest are the forward-path contracts kept for the next slice of the migration, which have
  // no implementation inside this module at all. Being implementable is the only property this
  // slice can assert about them, and an unnoticed change to their shape would be found by nothing
  // else - so the implementations below are declared as ordinary classes rather than as snippets,
  // which puts the compiler's check on them at every build of this module.
  //-------------------------------------------------------------------------

  test("ReferenceData is open, so an application can supply reference data of its own") {
    val host: ReferenceData = new HostReferenceData(refData)
    host.findValue(gbloId) shouldBe Some(gblo)
    host.containsValue(gbloId) shouldBe true
    new HostReferenceData(ReferenceData.empty).findValue(gbloId) shouldBe None
    assertCompiles(
      """{
         |  final class Anonymous extends ReferenceData {
         |    def findValue[T](id: ReferenceDataId[T]): Option[T] = refData.findValue(id)
         |  }
         |  ()
         |}""".stripMargin)
  }

  test("ReferenceDataId is open, so an application can supply identifiers of its own") {
    val id: ReferenceDataId[HolidayCalendar] = HostReferenceDataId("a label")
    id.resolve(ReferenceData.empty).isLeft shouldBe true
    id.toReader.run(ReferenceData.empty).isLeft shouldBe true
    HostReferenceDataId("a label") shouldBe id
  }

  test("FloatingRate is open, so FloatingRateName can implement it from another file") {
    val host: FloatingRate = new HostFloatingRate("HOST-LIBOR", gbpLiborName)
    host.name shouldBe "HOST-LIBOR"
    host.floatingRateName shouldBe gbpLiborName
    (gbpLiborName: FloatingRate).floatingRateName shouldBe gbpLiborName
  }

  test("IndexObservation is open, so each observation type can keep its own file") {
    val host: IndexObservation = new HostIndexObservation(iborIndex)
    host.index shouldBe iborIndex
    (iborObservation: IndexObservation).index shouldBe iborIndex
  }

  test("CalculationTarget and Resolvable are open, as the next migration slice requires") {
    val target: CalculationTarget = HostTarget(currencyAmount)
    target shouldBe HostTarget(currencyAmount)
    val resolvable: Resolvable[HostTarget] = new HostResolvable(currencyAmount)
    resolvable.resolve(refData) shouldBe Right(HostTarget(currencyAmount))
    resolvable.resolve(ReferenceData.empty).isLeft shouldBe true
    val resolvableTarget: ResolvableCalculationTarget = new HostResolvableTarget(currencyAmount)
    resolvableTarget.resolveTarget(refData) shouldBe Right(HostTarget(currencyAmount))
  }

  test("DateAdjuster is open, and can also be built from a function") {
    val host: DateAdjuster = new HostDateAdjuster(3)
    host.adjust(jan15) shouldBe LocalDate.of(2020, 1, 18)
    DateAdjuster(date => date.plusDays(1L)).adjust(jan15) shouldBe LocalDate.of(2020, 1, 16)
  }

  test("FxRateProvider and FxConvertible are open, so a host can supply rates and convert with them") {
    val provider: FxRateProvider = new HostRateProvider(gbpUsd, 1.25)
    provider.fxRate(gbp, usd) shouldBe Right(1.25)
    provider.fxRate(usd, gbp) shouldBe Right(0.8)
    provider.fxRate(usd, Currency.EUR).isLeft shouldBe true
    val convertible: FxConvertible[HostConvertible] = HostConvertible(currencyAmount)
    convertible.convertedTo(usd, provider) shouldBe Right(HostConvertible(valueOf(CurrencyAmount.of(usd, 125.0))))
  }

  //-------------------------------------------------------------------------
  // Group 7 - the copy safety of the numeric wrappers
  //
  // `DoubleArray` and `DoubleMatrix` wrap a primitive array, and the whole immutability of the
  // types rests on no caller ever holding a reference to it. The two members that would hand it
  // over - `ofUnsafe`, which wraps an array the caller keeps, and `toArrayUnsafe`, which returns
  // the array itself - are `private[collect]`, so they serve the module that needs them and
  // nobody else. The collect module's own specs cannot show that, being inside `collect`; this
  // module is outside it, so the four assertions below are the proof, and they are followed by
  // the runtime half: that the public API really does copy in both directions.
  //
  // The consequence for call sites here is the one recorded in the migration note: where the Java
  // code filled a `double[]` and wrapped it, this port builds arrays with `tabulate`, `map`,
  // `mapWithIndex` and `combine` instead.
  //-------------------------------------------------------------------------
  test("the private[collect] escape hatches of DoubleArray are out of reach from this module") {
    doubleArray.get(0) shouldBe 1.0
    assertCompiles("""DoubleArray.of(1.0, 2.0, 3.0)""")
    assertCompiles("""DoubleArray.copyOf(Array(1.0, 2.0, 3.0))""")
    assertCompiles("""DoubleArray.tabulate(3)(index => index.toDouble)""")
    assertCompiles("""doubleArray.toArray""")
    assertTypeError("""DoubleArray.ofUnsafe(Array(1.0, 2.0, 3.0))""")
    assertTypeError("""doubleArray.toArrayUnsafe""")
  }

  test("the private[collect] escape hatches of DoubleMatrix are out of reach from this module") {
    doubleMatrix.get(1, 1) shouldBe 4.0
    assertCompiles("""DoubleMatrix.of(2, 2, 1.0, 2.0, 3.0, 4.0)""")
    assertCompiles("""DoubleMatrix.copyOf(Array(Array(1.0, 2.0), Array(3.0, 4.0)))""")
    assertCompiles("""DoubleMatrix.tabulate(2, 2)((row, column) => (row + column).toDouble)""")
    assertCompiles("""doubleMatrix.toArray""")
    assertTypeError("""DoubleMatrix.ofUnsafe(Array(Array(1.0, 2.0), Array(3.0, 4.0)))""")
    assertTypeError("""doubleMatrix.toArrayUnsafe""")
  }

  test("the private[collect] constants of Decimal are out of reach from this module") {
    decimal.scale shouldBe 2
    assertCompiles("""decimal.scale""")
    assertTypeError("""Decimal.MAX_SCALE""")
  }

  test("DoubleArray.toArray hands back a copy, so mutating it cannot reach the array") {
    val exported = doubleArray.toArray
    exported(0) = 99.0
    exported(0) shouldBe 99.0
    doubleArray.get(0) shouldBe 1.0
    doubleArray.toArray.toList shouldBe List(1.0, 2.0, 3.0)
  }

  test("the DoubleArray factories copy their input, so mutating it afterwards cannot reach the array") {
    val source = Array(1.0, 2.0, 3.0)
    val copied = DoubleArray.copyOf(source)
    val fromSeq = DoubleArray.of(source(0), source(1), source(2))
    val tabulated = DoubleArray.tabulate(source.length)(index => source(index))
    source(0) = 99.0
    source(0) shouldBe 99.0
    copied.get(0) shouldBe 1.0
    fromSeq.get(0) shouldBe 1.0
    tabulated.get(0) shouldBe 1.0
  }

  test("DoubleMatrix.toArray hands back a deep copy, so mutating it cannot reach the matrix") {
    val exported = doubleMatrix.toArray
    exported(0)(0) = 99.0
    exported(0)(0) shouldBe 99.0
    doubleMatrix.get(0, 0) shouldBe 1.0
    doubleMatrix.row(0).toList shouldBe List(1.0, 2.0)
  }

  test("the DoubleMatrix factories copy their input, so mutating it afterwards cannot reach the matrix") {
    val source = Array(Array(1.0, 2.0), Array(3.0, 4.0))
    val copied = DoubleMatrix.copyOf(source)
    val tabulated = DoubleMatrix.tabulate(2, 2)((row, column) => source(row)(column))
    source(0)(0) = 99.0
    source(0)(0) shouldBe 99.0
    copied.get(0, 0) shouldBe 1.0
    tabulated.get(0, 0) shouldBe 1.0
  }
}

/**
 * The host implementations of this module's open contracts, which are the positive half of the
 * closed-family proofs in [[ApiSurfaceSpec]].
 *
 * They live in the companion rather than inside the spec class for a reason the compiler insists
 * on: a case class nested in a class carries a reference to its enclosing instance, which makes
 * the equality check it synthesises unverifiable at run time and - under this build's `-Werror` -
 * an error. Nested in an object they have no such reference, and the equality of `HostTarget` and
 * `HostConvertible`, which two of the tests rely on, is the ordinary structural one.
 *
 * Every one of these is declared as ordinary code rather than inside a compile-assertion string,
 * so the compiler checks each of them at every build of this module: a change that sealed one of
 * these contracts, or altered the shape of a member, would stop this file from compiling.
 */
private object ApiSurfaceSpec {

  /**
   * A host's own reference data, wrapping another source and passing every question to it.
   *
   * This is the shape `HolidaySafeReferenceData` has, reduced to the part that matters here: it
   * lives outside `ReferenceData.scala`, it implements the one abstract member, and it overrides
   * the derived one - all three of which must be possible for an application to supply holidays
   * of its own.
   *
   * @param underlying  the reference data the host already has
   */
  final class HostReferenceData(underlying: ReferenceData) extends ReferenceData {
    override def findValue[T](id: ReferenceDataId[T]): Option[T] = underlying.findValue(id)
    override def containsValue(id: ReferenceDataId[_]): Boolean = underlying.containsValue(id)
  }

  /** A host's own identifier, which resolves through the default the trait supplies. */
  final case class HostReferenceDataId(label: String) extends ReferenceDataId[HolidayCalendar]

  /** A host's own floating rate, which is what `FloatingRateName` is from another file. */
  final class HostFloatingRate(val name: String, val floatingRateName: FloatingRateName)
      extends FloatingRate

  /** A host's own index observation, the shape the four observation types have. */
  final class HostIndexObservation(val index: Index) extends IndexObservation

  /** A host's own calculation target, the contract every financial instrument will implement. */
  final case class HostTarget(notional: CurrencyAmount) extends CalculationTarget

  /** A host's own resolvable, which needs reference data to become its resolved form. */
  final class HostResolvable(notional: CurrencyAmount) extends Resolvable[HostTarget] {
    override def resolve(refData: ReferenceData): Either[Failure, HostTarget] =
      HolidayCalendarIds.GBLO.resolve(refData).map(_ => HostTarget(notional))
  }

  /** A host's own resolvable target, which resolves to a calculation target. */
  final class HostResolvableTarget(notional: CurrencyAmount) extends ResolvableCalculationTarget {
    override def resolveTarget(refData: ReferenceData): Either[Failure, CalculationTarget] =
      HolidayCalendarIds.GBLO.resolve(refData).map(_ => HostTarget(notional))
  }

  /** A host's own date adjuster, shifting by a fixed number of calendar days. */
  final class HostDateAdjuster(days: Int) extends DateAdjuster {
    override def adjust(date: LocalDate): LocalDate = date.plusDays(days.toLong)
  }

  /** A host's own rate provider, answering for one pair in both directions. */
  final class HostRateProvider(pair: CurrencyPair, rate: Double) extends FxRateProvider {
    override def fxRate(baseCurrency: Currency, counterCurrency: Currency): Either[Failure, Double] =
      if (baseCurrency == pair.base && counterCurrency == pair.counter) {
        Right(rate)
      } else if (baseCurrency == pair.counter && counterCurrency == pair.base) {
        Right(1.0 / rate)
      } else {
        Left(Failure.CurrencyConversion(s"No rate available for $baseCurrency/$counterCurrency"))
      }
  }

  /** A host's own convertible amount, the contract every convertible value implements. */
  final case class HostConvertible(notional: CurrencyAmount) extends FxConvertible[HostConvertible] {
    override def convertedTo(
        resultCurrency: Currency,
        rateProvider: FxRateProvider): Either[Failure, HostConvertible] =
      notional.convertedTo(resultCurrency, rateProvider).map(HostConvertible.apply)
  }
}
