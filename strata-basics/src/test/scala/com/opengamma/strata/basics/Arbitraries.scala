/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import java.time.DayOfWeek
import java.time.LocalDate
import java.time.Period
import java.time.YearMonth

import scala.annotation.tailrec

import cats.data.NonEmptyChain
import cats.data.NonEmptyList

import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import org.scalacheck.Shrink

import com.opengamma.strata.collect.{Arbitraries => CollectArbitraries}
import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.FixedScaleDecimal
import com.opengamma.strata.collect.SampleNamed
import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.array.DoubleMatrix
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason

import com.opengamma.strata.basics.currency.AdjustablePayment
import com.opengamma.strata.basics.currency.BigMoney
import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.CurrencyAmount
import com.opengamma.strata.basics.currency.CurrencyAmountArray
import com.opengamma.strata.basics.currency.CurrencyPair
import com.opengamma.strata.basics.currency.FxMatrix
import com.opengamma.strata.basics.currency.FxRate
import com.opengamma.strata.basics.currency.Money
import com.opengamma.strata.basics.currency.MultiCurrencyAmount
import com.opengamma.strata.basics.currency.MultiCurrencyAmountArray
import com.opengamma.strata.basics.currency.Payment
import com.opengamma.strata.basics.date.AdjustableDate
import com.opengamma.strata.basics.date.AdjustableDates
import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.basics.date.BusinessDayConvention
import com.opengamma.strata.basics.date.DateSequence
import com.opengamma.strata.basics.date.DayCount
import com.opengamma.strata.basics.date.DaysAdjustment
import com.opengamma.strata.basics.date.HolidayCalendar
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.basics.date.ImmutableHolidayCalendar
import com.opengamma.strata.basics.date.MarketTenor
import com.opengamma.strata.basics.date.PeriodAdditionConvention
import com.opengamma.strata.basics.date.PeriodAdjustment
import com.opengamma.strata.basics.date.SequenceDate
import com.opengamma.strata.basics.date.StandardHolidayCalendars
import com.opengamma.strata.basics.date.Tenor
import com.opengamma.strata.basics.date.TenorAdjustment
import com.opengamma.strata.basics.index.FloatingRateName
import com.opengamma.strata.basics.index.FloatingRateType
import com.opengamma.strata.basics.index.FxIndex
import com.opengamma.strata.basics.index.FxIndexObservation
import com.opengamma.strata.basics.index.IborIndex
import com.opengamma.strata.basics.index.IborIndexObservation
import com.opengamma.strata.basics.index.OvernightIndex
import com.opengamma.strata.basics.index.OvernightIndexObservation
import com.opengamma.strata.basics.index.PriceIndex
import com.opengamma.strata.basics.index.PriceIndexObservation
import com.opengamma.strata.basics.location.Country
import com.opengamma.strata.basics.schedule.Frequency
import com.opengamma.strata.basics.schedule.PeriodicSchedule
import com.opengamma.strata.basics.schedule.RollConvention
import com.opengamma.strata.basics.schedule.Schedule
import com.opengamma.strata.basics.schedule.SchedulePeriod
import com.opengamma.strata.basics.schedule.StubConvention
import com.opengamma.strata.basics.value.HalfUp
import com.opengamma.strata.basics.value.NoRounding
import com.opengamma.strata.basics.value.Rounding
import com.opengamma.strata.basics.value.ValueAdjustment
import com.opengamma.strata.basics.value.ValueAdjustmentType
import com.opengamma.strata.basics.value.ValueDerivatives
import com.opengamma.strata.basics.value.ValueSchedule
import com.opengamma.strata.basics.value.ValueStep
import com.opengamma.strata.basics.value.ValueStepSequence

/** A calculation target carrying data, used wherever a spec needs one. */
final case class TestTarget(value: Int) extends CalculationTarget

/**
 * The ScalaCheck generators, perturbations and shrinkings of `strata-basics`.
 *
 * A value of a validated or normalising type is reached only through that type's own factory, so a
 * factory that rejects a generated input fails the draw through [[fromEither]] rather than yielding
 * a value. A member of a closed family is drawn from the family's `values`, and a total type is
 * built by its constructor, neither of which has anything to reject.
 */
object Arbitraries {

  //-------------------------------------------------------------------------
  // The bounds every generated value stays within, kept small so that a property run over the whole
  // inventory is cheap.
  //-------------------------------------------------------------------------
  private val MinYear: Int = 2010

  private val MaxYear: Int = 2030

  private val MaxCurrencies: Int = 4

  private val MaxRunLength: Int = 6

  private val MaxDates: Int = 4

  private val MaxPeriods: Int = 6

  private val MaxHolidays: Int = 6

  private val MaxSteps: Int = 3

  private val MaxRoundingPlaces: Int = 8

  /** The date every index is probed with to find whether its calendars resolve. */
  private val CalendarProbeDate: LocalDate = LocalDate.of(2015, 6, 15)

  //-------------------------------------------------------------------------
  /**
   * Turns the outcome of a validated factory into a generated value.
   *
   * A `Left` generates nothing and labels the draw with the factory that rejected its input, so a
   * generator handing a factory something outside its documented range reports that rather than
   * falling back to a value the property never asked for.
   *
   * @param description  the factory the outcome came from, named for the label
   */
  private def fromEither[E, A](description: String, outcome: Either[E, A]): Gen[A] =
    outcome match {
      case Right(value) => Gen.const(value)
      case Left(cause) => Gen.fail[A].label(s"$description rejected a generated input: $cause")
    }

  /**
   * Turns an optional value into a generated one, for recovering the non-empty collection types.
   *
   * `None` fails the draw with a label, as a `Left` does in [[fromEither]].
   *
   * @param description  the conversion the value came from, named for the label
   */
  private def fromOption[A](description: String, value: Option[A]): Gen[A] =
    value match {
      case Some(present) => Gen.const(present)
      case None => Gen.fail[A].label(s"$description produced nothing for a generated input")
    }

  /**
   * Runs a list of generators in order, collecting what they draw.
   *
   * The fold is stated directly rather than reached through `Gen.sequence` so that the element
   * order of the result is visibly the element order of the input: the order currencies are placed
   * into a matrix is part of the matrix.
   */
  private def sequenceGen[A](generators: List[Gen[A]]): Gen[List[A]] =
    generators.foldRight(Gen.const(List.empty[A]))((head, tail) =>
      head.flatMap(value => tail.map(drawn => value :: drawn)))

  /**
   * Collects a list of outcomes into an outcome of a list, keeping the first failure.
   *
   * The whole run reaches [[fromEither]] as one outcome, so a rejected element fails the draw with
   * its factory named rather than being dropped from the run.
   */
  private def sequenceEither[E, A](outcomes: List[Either[E, A]]): Either[E, List[A]] =
    outcomes.foldRight(Right(List.empty[A]): Either[E, List[A]])((head, tail) =>
      head.flatMap(value => tail.map(collected => value :: collected)))

  /**
   * Builds a `Cogen` from a rendering of the type.
   *
   * A `Cogen` has to agree with equality, and every type of this module renders the fields its
   * equality compares, so a rendering is a sound basis for one. Two values that are not equal may
   * share a rendering and so share a perturbation, which costs a little entropy and breaks no law.
   *
   * @param render  the rendering of the type, which must agree with its equality
   */
  private def cogenBy[A](render: A => String): Cogen[A] =
    implicitly[Cogen[String]].contramap[A](render)

  //-------------------------------------------------------------------------
  // The generators of `strata-collect`, re-exported by reference so that a spec of this module
  // imports one object: a wildcard import of both would make every instance below ambiguous.
  //-------------------------------------------------------------------------
  val genFiniteDouble: Gen[Double] = CollectArbitraries.genFiniteDouble

  val genEdgeDouble: Gen[Double] = CollectArbitraries.genEdgeDouble

  val genDouble: Gen[Double] = CollectArbitraries.genDouble

  val genNonEmptyText: Gen[String] = CollectArbitraries.genNonEmptyText

  val genUpperLetterText: Gen[String] = CollectArbitraries.genUpperLetterText

  val genDecimal: Gen[Decimal] = CollectArbitraries.genDecimal

  implicit val arbDecimal: Arbitrary[Decimal] = CollectArbitraries.arbDecimal

  implicit val cogenDecimal: Cogen[Decimal] = CollectArbitraries.cogenDecimal

  implicit val shrinkDecimal: Shrink[Decimal] = CollectArbitraries.shrinkDecimal

  val genFixedScaleDecimal: Gen[FixedScaleDecimal] = CollectArbitraries.genFixedScaleDecimal

  implicit val arbFixedScaleDecimal: Arbitrary[FixedScaleDecimal] =
    CollectArbitraries.arbFixedScaleDecimal

  implicit val cogenFixedScaleDecimal: Cogen[FixedScaleDecimal] =
    CollectArbitraries.cogenFixedScaleDecimal

  implicit val shrinkFixedScaleDecimal: Shrink[FixedScaleDecimal] =
    CollectArbitraries.shrinkFixedScaleDecimal

  val genFiniteDoubleArray: Gen[DoubleArray] = CollectArbitraries.genFiniteDoubleArray

  val genNonEmptyFiniteDoubleArray: Gen[DoubleArray] =
    CollectArbitraries.genNonEmptyFiniteDoubleArray

  val genDoubleArray: Gen[DoubleArray] = CollectArbitraries.genDoubleArray

  val genNonEmptyDoubleArray: Gen[DoubleArray] = CollectArbitraries.genNonEmptyDoubleArray

  implicit val arbDoubleArray: Arbitrary[DoubleArray] = CollectArbitraries.arbDoubleArray

  implicit val cogenDoubleArray: Cogen[DoubleArray] = CollectArbitraries.cogenDoubleArray

  implicit val shrinkDoubleArray: Shrink[DoubleArray] = CollectArbitraries.shrinkDoubleArray

  val genFiniteDoubleMatrix: Gen[DoubleMatrix] = CollectArbitraries.genFiniteDoubleMatrix

  val genDoubleMatrix: Gen[DoubleMatrix] = CollectArbitraries.genDoubleMatrix

  val genSquareFiniteDoubleMatrix: Gen[DoubleMatrix] =
    CollectArbitraries.genSquareFiniteDoubleMatrix

  val genSquareDoubleMatrix: Gen[DoubleMatrix] = CollectArbitraries.genSquareDoubleMatrix

  implicit val arbDoubleMatrix: Arbitrary[DoubleMatrix] = CollectArbitraries.arbDoubleMatrix

  implicit val cogenDoubleMatrix: Cogen[DoubleMatrix] = CollectArbitraries.cogenDoubleMatrix

  implicit val shrinkDoubleMatrix: Shrink[DoubleMatrix] = CollectArbitraries.shrinkDoubleMatrix

  val genFailureReason: Gen[FailureReason] = CollectArbitraries.genFailureReason

  implicit val arbFailureReason: Arbitrary[FailureReason] = CollectArbitraries.arbFailureReason

  implicit val cogenFailureReason: Cogen[FailureReason] = CollectArbitraries.cogenFailureReason

  val genFailure: Gen[Failure] = CollectArbitraries.genFailure

  def genFailureWithReason(reason: FailureReason): Gen[Failure] =
    CollectArbitraries.genFailureWithReason(reason)

  implicit val arbFailure: Arbitrary[Failure] = CollectArbitraries.arbFailure

  implicit val cogenFailure: Cogen[Failure] = CollectArbitraries.cogenFailure

  implicit val shrinkFailure: Shrink[Failure] = CollectArbitraries.shrinkFailure

  val genFailures: Gen[NonEmptyChain[Failure]] = CollectArbitraries.genFailures

  implicit val arbFailures: Arbitrary[NonEmptyChain[Failure]] = CollectArbitraries.arbFailures

  implicit val cogenFailures: Cogen[NonEmptyChain[Failure]] = CollectArbitraries.cogenFailures

  implicit val shrinkFailures: Shrink[NonEmptyChain[Failure]] =
    CollectArbitraries.shrinkFailures

  val genSampleNamed: Gen[SampleNamed] = CollectArbitraries.genSampleNamed

  implicit val arbSampleNamed: Arbitrary[SampleNamed] = CollectArbitraries.arbSampleNamed

  implicit val cogenSampleNamed: Cogen[SampleNamed] = CollectArbitraries.cogenSampleNamed

  implicit val shrinkSampleNamed: Shrink[SampleNamed] = CollectArbitraries.shrinkSampleNamed

  //-------------------------------------------------------------------------
  // The shared primitives: dates, months, periods and the double ranges the domain factories
  // accept. Every generator below is written over these, so one change to the window moves every
  // date this file produces.
  //-------------------------------------------------------------------------
  /**
   * Generates a date in the twenty-one year window.
   *
   * The day of the year runs to 365 rather than to the length of the year, so a leap year and a
   * common year are drawn from the same range.
   */
  val genLocalDate: Gen[LocalDate] =
    for {
      year <- Gen.choose(MinYear, MaxYear)
      dayOfYear <- Gen.choose(1, 365)
    } yield LocalDate.ofYearDay(year, dayOfYear)

  val genYearMonth: Gen[YearMonth] =
    for {
      year <- Gen.choose(MinYear, MaxYear)
      month <- Gen.choose(1, 12)
    } yield YearMonth.of(year, month)

  /**
   * Generates a period of whole months or years, which every addition convention accepts.
   *
   * The two month-based addition conventions reject a period holding days, so the adjustments
   * carrying one of those draw their periods from here.
   */
  val genMonthBasedPeriod: Gen[Period] = Gen.frequency(
    3 -> Gen.choose(1, 24).map(months => Period.ofMonths(months)),
    1 -> Gen.choose(1, 10).map(years => Period.ofYears(years)))

  val genPeriod: Gen[Period] = Gen.frequency(
    3 -> genMonthBasedPeriod,
    1 -> Gen.choose(1, 60).map(days => Period.ofDays(days)),
    1 -> Gen.choose(1, 8).map(weeks => Period.ofWeeks(weeks)))

  /**
   * Generates an amount a [[CurrencyAmount]] accepts.
   *
   * That type rejects `NaN`, admits both infinities and normalises `-0.0` to `0.0`, so the draws
   * are mostly ordinary finite values with those two edges reached often enough for a property over
   * a handful of amounts to see one, and `NaN` is never drawn.
   */
  val genCurrencyAmountValue: Gen[Double] = Gen.frequency(
    6 -> genFiniteDouble,
    1 -> Gen.oneOf(Double.PositiveInfinity, Double.NegativeInfinity, -0.0, 0.0))

  /**
   * Generates a finite amount of moderate magnitude.
   *
   * This is the amount generator of the restricted values the `Monoid` law suite needs, and of the
   * two money types, whose factories narrow an amount through a decimal and so accept only finite
   * input. A third of the draws fall below the smallest minor unit, which is what exercises the
   * rounding of a money value to its currency's minor units.
   */
  val genFiniteAmountValue: Gen[Double] = Gen.frequency(
    4 -> genFiniteDouble,
    2 -> Gen.choose(-1.0d, 1.0d),
    1 -> Gen.oneOf(0.0005d, -0.0005d, 1.0005d, 12.3456d, -12.3456d, 0.001d, 0.0d))

  /**
   * Generates a positive finite rate.
   *
   * Every rate an [[FxRate]] holds has to be strictly positive, and the range spans four orders of
   * magnitude either side of one, which is the range these currencies trade at against each other.
   */
  val genPositiveRate: Gen[Double] = Gen.frequency(
    4 -> Gen.choose(0.5d, 2.0d),
    2 -> Gen.choose(1.0e-4d, 1.0e4d),
    1 -> Gen.oneOf(1.0d, 1.6d, 1.4d, 1.2d, 0.008d, 0.1d))

  //-------------------------------------------------------------------------
  // The root package: identifiers and calculation targets.
  //-------------------------------------------------------------------------
  /**
   * Generates a standard identifier.
   *
   * Both parts are drawn from the upper-case letters, which every scheme and every value accepts.
   * The rendering is `scheme~value` and neither part may hold a tilde, so every generated
   * identifier parses back from its own rendering.
   */
  val genStandardId: Gen[StandardId] =
    for {
      scheme <- genUpperLetterText
      value <- genUpperLetterText
      identifier <- fromEither("StandardId.of", StandardId.of(scheme, value))
    } yield identifier

  implicit val arbStandardId: Arbitrary[StandardId] = Arbitrary(genStandardId)

  implicit val cogenStandardId: Cogen[StandardId] = cogenBy[StandardId](_.toString)

  val genTestTarget: Gen[TestTarget] = Gen.choose(0, 1000).map(value => TestTarget(value))

  implicit val arbTestTarget: Arbitrary[TestTarget] = Arbitrary(genTestTarget)

  implicit val cogenTestTarget: Cogen[TestTarget] = cogenBy[TestTarget](_.toString)

  /**
   * Generates a list of calculation targets.
   *
   * The list may be empty, which is a value the type accepts, and its elements are [[TestTarget]]
   * because [[CalculationTarget]] carries no data of its own.
   */
  val genCalculationTargetList: Gen[CalculationTargetList] =
    for {
      size <- Gen.choose(0, MaxDates)
      targets <- Gen.listOfN(size, genTestTarget)
    } yield CalculationTargetList.of(targets)

  implicit val arbCalculationTargetList: Arbitrary[CalculationTargetList] =
    Arbitrary(genCalculationTargetList)

  implicit val cogenCalculationTargetList: Cogen[CalculationTargetList] =
    cogenBy[CalculationTargetList](_.toString)

  //-------------------------------------------------------------------------
  // The currency package.
  //-------------------------------------------------------------------------
  val genCurrency: Gen[Currency] = Gen.oneOf(Currency.values.toList)

  /**
   * The currencies grouped by the number of minor units they hold, in increasing order.
   *
   * The grouping is sorted so that every draw made from it is the same on every run: a `Map` built
   * by grouping has no order of its own to rely on.
   */
  private val currenciesByMinorUnits: List[List[Currency]] =
    Currency.values.toList
      .groupBy(currency => currency.minorUnitDigits)
      .toList
      .sortBy { case (digits, _) => digits }
      .map { case (_, group) => group }

  /**
   * Generates a currency, drawing each number of minor units equally often.
   *
   * The family holds currencies of no minor units, of two and of three, and the two money types
   * round an amount to that number of places. Drawing uniformly over the currencies would make a
   * three-place rounding rare; drawing uniformly over the groups exercises each rounding equally.
   */
  val genCurrencyAcrossMinorUnits: Gen[Currency] =
    Gen.oneOf(currenciesByMinorUnits).flatMap(group => Gen.oneOf(group))

  implicit val arbCurrency: Arbitrary[Currency] = Arbitrary(genCurrency)

  implicit val cogenCurrency: Cogen[Currency] = cogenBy[Currency](_.code)

  /**
   * Generates a pair of two different currencies.
   *
   * The two are picked without replacement, which is what the rate-bearing types need of a pair.
   */
  val genDistinctCurrencyPair: Gen[CurrencyPair] =
    Gen
      .pick(2, Currency.values.toList)
      .map(picked => CurrencyPair.of(picked.head, picked.last))

  /**
   * Generates a currency pair.
   *
   * Mostly pairs of different currencies, and occasionally a pair of one currency with itself,
   * which the type accepts and whose only valid rate is one.
   */
  val genCurrencyPair: Gen[CurrencyPair] = Gen.frequency(
    6 -> genDistinctCurrencyPair,
    1 -> genCurrency.map(currency => CurrencyPair.of(currency, currency)))

  implicit val arbCurrencyPair: Arbitrary[CurrencyPair] = Arbitrary(genCurrencyPair)

  implicit val cogenCurrencyPair: Cogen[CurrencyPair] = cogenBy[CurrencyPair](_.toString)

  private def genAmountIn(currency: Currency): Gen[CurrencyAmount] =
    for {
      amount <- genCurrencyAmountValue
      value <- fromEither("CurrencyAmount.of", CurrencyAmount.of(currency, amount))
    } yield value

  /**
   * Generates an amount of a currency.
   *
   * The amount is never `NaN`, which [[CurrencyAmount.of]] rejects, and is occasionally an infinity
   * or a negative zero, which it admits and normalises respectively.
   */
  val genCurrencyAmount: Gen[CurrencyAmount] = genCurrency.flatMap(genAmountIn)

  /**
   * Generates an amount of a currency whose value is finite and of moderate magnitude.
   *
   * This is the generator the `Monoid` law suite of [[MultiCurrencyAmount]] must use: adding two
   * infinities of opposite sign reaches the `NaN` the amount factory rejects, and floating-point
   * addition is associative only approximately, so that suite also needs an `Eq` tolerant to within
   * 1e-9 relative. The wide generator above stays wide, because the JSON round trip needs the
   * infinities.
   */
  val genFiniteCurrencyAmount: Gen[CurrencyAmount] =
    for {
      currency <- genCurrency
      amount <- genFiniteAmountValue
      value <- fromEither("CurrencyAmount.of", CurrencyAmount.of(currency, amount))
    } yield value

  implicit val arbCurrencyAmount: Arbitrary[CurrencyAmount] = Arbitrary(genCurrencyAmount)

  implicit val cogenCurrencyAmount: Cogen[CurrencyAmount] = cogenBy[CurrencyAmount](_.toString)

  /**
   * Generates a money value, rounded to its currency's minor units.
   *
   * The currency is drawn evenly over the numbers of minor units and the amount carries digits
   * below the smallest unit, so the rounding the factory performs is exercised at each of the three
   * scales the family holds.
   */
  val genMoney: Gen[Money] =
    for {
      currency <- genCurrencyAcrossMinorUnits
      amount <- genFiniteAmountValue
      value <- fromEither("Money.of", Money.of(currency, amount))
    } yield value

  implicit val arbMoney: Arbitrary[Money] = Arbitrary(genMoney)

  implicit val cogenMoney: Cogen[Money] = cogenBy[Money](_.toString)

  /** Generates a money value of unrestricted currency scale, rounded to twelve places. */
  val genBigMoney: Gen[BigMoney] =
    for {
      currency <- genCurrencyAcrossMinorUnits
      amount <- genFiniteAmountValue
      value <- fromEither("BigMoney.of", BigMoney.of(currency, amount))
    } yield value

  implicit val arbBigMoney: Arbitrary[BigMoney] = Arbitrary(genBigMoney)

  implicit val cogenBigMoney: Cogen[BigMoney] = cogenBy[BigMoney](_.toString)

  /**
   * Generates a multi-currency amount holding up to four currencies.
   *
   * The currencies are picked without replacement, because [[MultiCurrencyAmount.of]] rejects a
   * duplicate; a value holding two amounts of one currency is what `total` produces, and a spec
   * wanting that merge calls it directly. The empty amount is drawn as well, since it is the
   * identity of the type's `Monoid` and a document may carry it.
   */
  val genMultiCurrencyAmount: Gen[MultiCurrencyAmount] =
    for {
      size <- Gen.choose(0, MaxCurrencies)
      currencies <- Gen.pick(size, Currency.values.toList)
      amounts <- sequenceGen(currencies.toList.map(currency => genAmountIn(currency)))
      value <- fromEither("MultiCurrencyAmount.of", MultiCurrencyAmount.of(amounts))
    } yield value

  /**
   * Generates a multi-currency amount whose every amount is finite.
   *
   * This is the generator the `Monoid` law suite must use, for the reason recorded on
   * [[genFiniteCurrencyAmount]].
   */
  val genFiniteMultiCurrencyAmount: Gen[MultiCurrencyAmount] =
    for {
      size <- Gen.choose(0, MaxCurrencies)
      currencies <- Gen.pick(size, Currency.values.toList)
      amounts <- sequenceGen(
        currencies.toList.map(currency =>
          genFiniteAmountValue.flatMap(amount =>
            fromEither("CurrencyAmount.of", CurrencyAmount.of(currency, amount)))))
      value <- fromEither("MultiCurrencyAmount.of", MultiCurrencyAmount.of(amounts))
    } yield value

  implicit val arbMultiCurrencyAmount: Arbitrary[MultiCurrencyAmount] =
    Arbitrary(genMultiCurrencyAmount)

  implicit val cogenMultiCurrencyAmount: Cogen[MultiCurrencyAmount] =
    cogenBy[MultiCurrencyAmount](_.toString)

  /**
   * Generates an array of elements of a given length for the two currency-array types.
   *
   * The elements are drawn from [[genCurrencyAmountValue]], the amount generator: both infinities
   * and both signed zeroes are drawn often enough for a property over a handful of runs to see
   * one, and `NaN` is never drawn. That is exactly the element domain of the two types this feeds
   * - the elements of a run are amounts kept as numbers, so each type rejects a `NaN` element at
   * construction as [[CurrencyAmount]] rejects it for a single amount - and drawing one would
   * therefore produce no run at all rather than an interesting run.
   *
   * The edge-bearing `genDoubleArray` remains what it was for the types that do admit a `NaN`
   * element, [[com.opengamma.strata.collect.array.DoubleArray]] itself among them.
   *
   * @param length  the number of elements
   * @return a generator of arrays of that length whose elements are amounts
   */
  private def genDoubleArrayOfLength(length: Int): Gen[DoubleArray] =
    Gen.listOfN(length, genCurrencyAmountValue).map(elements => DoubleArray.copyOf(elements))

  /**
   * Generates a run of amounts in one currency, of any length the type admits.
   *
   * The length starts at zero. This factory takes a currency and an array of values and answers a
   * run for any of them, so a run of length zero is a value the type holds, a document carries and
   * every operation over a run has to answer for; only the factory that derives the currency from a
   * collection of amounts needs one amount to read it from.
   */
  val genCurrencyAmountArray: Gen[CurrencyAmountArray] =
    for {
      currency <- genCurrency
      length <- Gen.choose(0, MaxRunLength)
      values <- genDoubleArrayOfLength(length)
    } yield CurrencyAmountArray.of(currency, values)

  implicit val arbCurrencyAmountArray: Arbitrary[CurrencyAmountArray] =
    Arbitrary(genCurrencyAmountArray)

  implicit val cogenCurrencyAmountArray: Cogen[CurrencyAmountArray] =
    cogenBy[CurrencyAmountArray](_.toString)

  /**
   * Generates a run of multi-currency amounts.
   *
   * Every currency carries a run of the same length, which the factory requires, and the currencies
   * are picked without replacement so that none is named twice.
   */
  val genMultiCurrencyAmountArray: Gen[MultiCurrencyAmountArray] =
    for {
      size <- Gen.choose(1, MaxCurrencies)
      currencies <- Gen.pick(size, Currency.values.toList)
      length <- Gen.choose(1, MaxRunLength)
      arrays <- sequenceGen(currencies.toList.map(_ => genDoubleArrayOfLength(length)))
      value <- fromEither(
        "MultiCurrencyAmountArray.of",
        MultiCurrencyAmountArray.of(currencies.toList.zip(arrays).toMap))
    } yield value

  implicit val arbMultiCurrencyAmountArray: Arbitrary[MultiCurrencyAmountArray] =
    Arbitrary(genMultiCurrencyAmountArray)

  implicit val cogenMultiCurrencyAmountArray: Cogen[MultiCurrencyAmountArray] =
    cogenBy[MultiCurrencyAmountArray](_.toString)

  val genPayment: Gen[Payment] =
    for {
      amount <- genCurrencyAmount
      date <- genLocalDate
    } yield Payment.of(amount, date)

  implicit val arbPayment: Arbitrary[Payment] = Arbitrary(genPayment)

  implicit val cogenPayment: Cogen[Payment] = cogenBy[Payment](_.toString)

  /**
   * Generates an exchange rate.
   *
   * Mostly a rate between two different currencies, and occasionally the rate of one currency
   * against itself, which [[FxRate.of]] accepts only where the rate is exactly one; it refuses
   * every non-positive rate.
   */
  val genFxRate: Gen[FxRate] = Gen.frequency(
    6 -> (for {
      pair <- genDistinctCurrencyPair
      rate <- genPositiveRate
      value <- fromEither("FxRate.of", FxRate.of(pair, rate))
    } yield value),
    1 -> (for {
      currency <- genCurrency
      value <- fromEither("FxRate.of", FxRate.of(CurrencyPair.of(currency, currency), 1.0d))
    } yield value))

  implicit val arbFxRate: Arbitrary[FxRate] = Arbitrary(genFxRate)

  implicit val cogenFxRate: Cogen[FxRate] = cogenBy[FxRate](_.toString)

  /**
   * Generates a rate an [[FxRate]] accepts, including the two non-finite ones.
   *
   * The check the factory applies is `!(rate <= 0.0)`, which passes a value that is not a number -
   * `NaN <= 0.0` is false, so the negation holds - and passes `+∞` for the plain reason that it is
   * greater than zero. The three values it refuses, both zeroes and `-∞`, are therefore absent: a
   * generator producing one would fail the draw rather than produce an edge value.
   */
  val genEdgeRate: Gen[Double] = Gen.frequency(
    4 -> genPositiveRate,
    1 -> Gen.const(Double.PositiveInfinity),
    1 -> Gen.const(Double.NaN))

  /**
   * Generates an exchange rate whose rate reaches the edges of the IEEE-754 values it admits.
   *
   * This is [[genFxRate]] over [[genEdgeRate]]. Equality on this type treats every `NaN` as equal
   * to every other and keeps `-0.0` distinct from `0.0`, so the rates that decide it are `+∞` and
   * `NaN`, and no generator of ordinary rates produces one. The finite generator stays the implicit
   * arbitrary, because the arithmetic properties - conversion, the cross rate, the reciprocal - are
   * written over rates whose products are numbers; this one is passed by hand to the audits about
   * equality, hashing, rendering and the document form.
   *
   * The identity-pair case is drawn here at the weight [[genFxRate]] draws it, so the two
   * generators cover the same shapes of value and differ only in the rates they reach.
   */
  val genEdgeFxRate: Gen[FxRate] = Gen.frequency(
    6 -> (for {
      pair <- genDistinctCurrencyPair
      rate <- genEdgeRate
      value <- fromEither("FxRate.of", FxRate.of(pair, rate))
    } yield value),
    1 -> (for {
      currency <- genCurrency
      value <- fromEither("FxRate.of", FxRate.of(CurrencyPair.of(currency, currency), 1.0d))
    } yield value))

  /**
   * Generates a rate to place into a matrix, including the zero rate.
   *
   * A matrix admits a rate of `0.0`, and that is the case worth drawing: its reciprocal is an
   * infinity, so a matrix holding one carries a non-finite entry, which the codec has to tag and
   * which the arithmetic of the type has to answer for.
   */
  private val genMatrixRate: Gen[Double] = Gen.frequency(6 -> genPositiveRate, 1 -> Gen.const(0.0d))

  /**
   * Places a run of rates into a matrix, each against the first currency.
   *
   * Every rate names the first currency, which the matrix holds after the first placement, so no
   * placement can be the one the builder rejects - the one whose two currencies are both unknown to
   * the matrix. The currencies therefore enter in the order they are given, and that order is part
   * of the value: it is the order the matrix reports them in and the order a document carries them
   * in.
   *
   * @param currencies  the currencies, the first being the one every rate is against
   * @param rates  the rate of each currency after the first, against the first
   */
  private def matrixOf(
      currencies: List[Currency],
      rates: List[Double]): Either[Failure, FxMatrix] =
    currencies match {
      case Nil => Right(FxMatrix.empty)
      case reference :: rest =>
        rest.zip(rates).foldLeft(Right(FxMatrix.empty): Either[Failure, FxMatrix]) {
          case (matrix, (currency, rate)) =>
            matrix.flatMap(placed => placed.withRate(reference, currency, rate))
        }
    }

  /**
   * Generates a matrix of exchange rates holding up to four currencies.
   *
   * The empty matrix is drawn as well as populated ones, since it is a value the type holds and a
   * document may carry.
   */
  val genFxMatrix: Gen[FxMatrix] = Gen.frequency(
    8 -> (for {
      size <- Gen.choose(2, MaxCurrencies)
      currencies <- Gen.pick(size, Currency.values.toList)
      rates <- Gen.listOfN(size - 1, genMatrixRate)
      value <- fromEither("FxMatrix.withRate", matrixOf(currencies.toList, rates))
    } yield value),
    1 -> Gen.const(FxMatrix.empty))

  implicit val arbFxMatrix: Arbitrary[FxMatrix] = Arbitrary(genFxMatrix)

  implicit val cogenFxMatrix: Cogen[FxMatrix] = cogenBy[FxMatrix](_.toString)

  /**
   * Generates an off-diagonal entry of an edge-bearing matrix.
   *
   * [[FxMatrix.fromMatrix]] checks three things and no more - the currencies are distinct, the
   * matrix is square and of their number, and the diagonal is one - so every `Double` there is may
   * stand off the diagonal: the two infinities, `NaN` and both signed zeroes included. Those are
   * the entries equality turns on and the entries the document form has to tag, so a third of the
   * draws reach them.
   */
  private val genEdgeMatrixRate: Gen[Double] = Gen.frequency(
    4 -> genPositiveRate,
    1 -> genEdgeDouble,
    1 -> Gen.oneOf(0.0d, -0.0d))

  /**
   * Builds the rates of a matrix of the size given, with a unit diagonal.
   *
   * The diagonal is one, which [[FxMatrix.fromMatrix]] requires of every matrix; each off-diagonal
   * entry is read by position and independently of the entry opposite it, so a generated matrix is
   * generally not reciprocal. That is a state the type holds - a matrix built by placing rates one
   * at a time can be updated in one direction only - so forcing reciprocity would cover less than
   * the type admits.
   *
   * @param size  the number of currencies, which is the number of rows and of columns
   * @param entries  the entries to read the off-diagonal positions from, `size * size` of them
   */
  private def edgeRatesOf(size: Int, entries: List[Double]): DoubleMatrix =
    DoubleMatrix.tabulate(size, size)((row, column) =>
      if (row == column) 1.0d else entries(row * size + column))

  private val PinnedEdgeCurrencies: Vector[Currency] =
    Vector(Currency.GBP, Currency.USD, Currency.EUR)

  /**
   * The rates of the matrix that pins the edge states, in the orientation of [[FxMatrix]].
   *
   * Every state a generated matrix reaches only by chance is present here at once: a non-reciprocal
   * pair - 2.0 one way and 5.0 the other, where reciprocity would require 0.5 - a negative zero, a
   * not-a-number entry and both infinities. The diagonal is one, which is the single structural
   * requirement.
   */
  private val PinnedEdgeRates: DoubleMatrix = DoubleMatrix.of(
    3,
    3,
    1.0d,
    2.0d,
    Double.NaN,
    5.0d,
    1.0d,
    Double.PositiveInfinity,
    -0.0d,
    Double.NegativeInfinity,
    1.0d)

  /**
   * Generates a structurally valid matrix whose entries reach the edges of the values it admits.
   *
   * Equality on this type treats every `NaN` as equal to every other and keeps `-0.0` distinct from
   * `0.0`, and its document form tags the three values JSON has no number for, so the entries that
   * decide both have to be generated. The finite [[genFxMatrix]] stays the implicit arbitrary,
   * because the arithmetic properties - conversion, triangulation, merging - are written over rates
   * whose products are numbers.
   *
   * Every matrix is built through [[FxMatrix.fromMatrix]], the factory a decoded document arrives
   * at, rather than by placing rates one at a time: placing a rate computes the reciprocal of it
   * for the opposite position, which is exactly what a non-reciprocal matrix does not hold. One
   * draw in eight is the pinned matrix above, so its states are all reached within a handful of
   * draws; the empty matrix is drawn as well, since it is a value the type holds.
   */
  val genEdgeFxMatrix: Gen[FxMatrix] = Gen.frequency(
    6 -> (for {
      size <- Gen.choose(2, MaxCurrencies)
      currencies <- Gen.pick(size, Currency.values.toList)
      entries <- Gen.listOfN(size * size, genEdgeMatrixRate)
      value <- fromEither(
        "FxMatrix.fromMatrix",
        FxMatrix.fromMatrix(currencies.toVector, edgeRatesOf(size, entries)))
    } yield value),
    1 -> fromEither("FxMatrix.fromMatrix", FxMatrix.fromMatrix(PinnedEdgeCurrencies, PinnedEdgeRates)),
    1 -> Gen.const(FxMatrix.empty))

  //-------------------------------------------------------------------------
  // The date package.
  //-------------------------------------------------------------------------
  val genBusinessDayConvention: Gen[BusinessDayConvention] =
    Gen.oneOf(BusinessDayConvention.values.toList)

  implicit val arbBusinessDayConvention: Arbitrary[BusinessDayConvention] =
    Arbitrary(genBusinessDayConvention)

  implicit val cogenBusinessDayConvention: Cogen[BusinessDayConvention] =
    cogenBy[BusinessDayConvention](_.name)

  /**
   * The built-in calendars, ordered by the name of the calendar.
   *
   * Sorted so that a draw from it is the same on every run: the built-in set is held in a `Map`,
   * whose iteration order is no part of its contract.
   */
  private val builtInCalendars: List[HolidayCalendar] =
    StandardHolidayCalendars.all.toList
      .sortBy { case (id, _) => id.name }
      .map { case (_, calendar) => calendar }

  private val builtInCalendarIds: List[HolidayCalendarId] =
    builtInCalendars.map(calendar => calendar.id)

  /**
   * The identifiers of the built-in calendars that declare holidays, ordered by name.
   *
   * The calendar of no holidays is excluded because it is absorbed by both composition operators -
   * combining with it drops it and linking to it yields it - so a composite naming it would not be
   * a composite at all.
   */
  private val datedCalendarIds: List[HolidayCalendarId] =
    builtInCalendarIds.filterNot(id => id == StandardHolidayCalendars.NO_HOLIDAYS.id)

  /**
   * Generates the identifier of one built-in calendar.
   *
   * These are the identifiers that resolve against the standard reference data, so they are the
   * ones an adjustment carries: a generated adjustment is meant to be applicable.
   */
  val genBuiltInHolidayCalendarId: Gen[HolidayCalendarId] = Gen.oneOf(builtInCalendarIds)

  /**
   * Generates the identifier of a calendar that is not built in.
   *
   * The name is upper-case letters beginning with `X`, which is neither a built-in name nor a name
   * holding either composition separator.
   */
  val genCustomHolidayCalendarId: Gen[HolidayCalendarId] =
    for {
      length <- Gen.choose(3, 5)
      letters <- Gen.listOfN(length, Gen.alphaUpperChar)
    } yield HolidayCalendarId.of(s"X${letters.mkString}")

  /**
   * Generates a holiday calendar identifier, simple or composite.
   *
   * Both composition forms are drawn - the combination written with a plus and the link written
   * with a tilde - because the identifier is carried in a document as its name and both forms have
   * to survive that. Composition is normalised, so a composite of two distinct dated calendars is
   * the same composite whichever order the two are drawn in.
   */
  val genHolidayCalendarId: Gen[HolidayCalendarId] = Gen.frequency(
    4 -> genBuiltInHolidayCalendarId,
    2 -> genCustomHolidayCalendarId,
    1 -> Gen
      .pick(2, datedCalendarIds)
      .map(picked => picked.head.combinedWith(picked.last)),
    1 -> Gen
      .pick(2, datedCalendarIds)
      .map(picked => picked.head.linkedWith(picked.last)))

  implicit val arbHolidayCalendarId: Arbitrary[HolidayCalendarId] =
    Arbitrary(genHolidayCalendarId)

  implicit val cogenHolidayCalendarId: Cogen[HolidayCalendarId] =
    cogenBy[HolidayCalendarId](_.name)

  /** Generates one of the day counts that need no calendar. */
  val genStandardDayCount: Gen[DayCount] = Gen.oneOf(DayCount.values.toList)

  /**
   * Generates a business day count over one of the built-in calendars, at the type of the member.
   *
   * This is the one day count carrying data, and the one with a structural document form rather
   * than a name, so it is generated separately and drawn alongside the standard members by
   * [[genDayCount]]. The calendar is built in, so the name form of the document - which resolves a
   * calendar by name against the built-in set - reads it back as well.
   *
   * The value comes from [[DayCount.ofBus252]], the one factory of the type, which answers at the
   * type of the family; the type test below narrows it to the member without bypassing that
   * factory. The narrow type is generated because the member carries `Hash` and `Show` instances of
   * its own, which the invariance of those typeclasses requires and which the law suites check at
   * this type.
   */
  val genCalendarBearingDayCount: Gen[DayCount.Bus252] =
    Gen.oneOf(builtInCalendars).flatMap { calendar =>
      DayCount.ofBus252(calendar) match {
        case bus252: DayCount.Bus252 => Gen.const(bus252)
        case other =>
          Gen.fail[DayCount.Bus252].label(s"DayCount.ofBus252 produced $other rather than a Bus252")
      }
    }

  implicit val arbCalendarBearingDayCount: Arbitrary[DayCount.Bus252] =
    Arbitrary(genCalendarBearingDayCount)

  implicit val cogenCalendarBearingDayCount: Cogen[DayCount.Bus252] =
    cogenBy[DayCount.Bus252](_.name)

  /**
   * Generates a business day count over one of the built-in calendars, at the type of the family.
   *
   * Expressed in terms of [[genCalendarBearingDayCount]] rather than drawing a calendar a second
   * time, so the two cannot drift apart in the set of calendars they cover.
   */
  val genBus252DayCount: Gen[DayCount] =
    genCalendarBearingDayCount.map(dayCount => dayCount: DayCount)

  val genDayCount: Gen[DayCount] = Gen.frequency(4 -> genStandardDayCount, 1 -> genBus252DayCount)

  implicit val arbDayCount: Arbitrary[DayCount] = Arbitrary(genDayCount)

  implicit val cogenDayCount: Cogen[DayCount] = cogenBy[DayCount](_.name)

  /**
   * Generates the weekend days of a calendar.
   *
   * Weekend days are an arbitrary set of days, so the four shapes drawn here span the sizes the
   * type admits rather than only the shapes the built-in calendars hold: the two-day western
   * weekend, a two-day Friday-Saturday weekend, a three-day weekend, and a single Sunday, which
   * makes every Saturday an ordinary business day unless a holiday says otherwise.
   */
  val genWeekendDays: Gen[Set[DayOfWeek]] = Gen.oneOf(
    Set(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY),
    Set(DayOfWeek.FRIDAY, DayOfWeek.SATURDAY),
    Set(DayOfWeek.THURSDAY, DayOfWeek.FRIDAY, DayOfWeek.SATURDAY),
    Set(DayOfWeek.SUNDAY))

  private def genDateInYear(year: Int, lastDayOfYear: Int): Gen[LocalDate] =
    Gen.choose(1, lastDayOfYear).map(dayOfYear => LocalDate.ofYearDay(year, dayOfYear))

  @tailrec
  private def firstWeekendDate(from: LocalDate, weekendDays: Set[DayOfWeek]): LocalDate =
    if (weekendDays.contains(from.getDayOfWeek)) from
    else firstWeekendDate(from.plusDays(1L), weekendDays)

  @tailrec
  private def firstWeekdayDate(from: LocalDate, weekendDays: Set[DayOfWeek]): LocalDate =
    if (weekendDays.contains(from.getDayOfWeek)) firstWeekdayDate(from.plusDays(1L), weekendDays)
    else from

  /**
   * Generates the holidays of a custom calendar, spanning three years from the one given.
   *
   * A date in the first year and a date in the third are always drawn, so the year range the
   * calendar derives from its holidays always spans more than one year. That matters because two
   * calendars of one identifier are equal whatever their holidays, so a round trip compares the
   * holidays over the year range as well, and a range of one year would make that comparison much
   * weaker than it looks.
   *
   * Those two spanning dates are moved off the weekend, because a calendar cannot tell a holiday
   * falling on a weekend day from the weekend itself - the two are one bit of the same mask - so
   * such a date is not among the holidays the calendar reports and would not widen the span of the
   * holidays a document carries. The middle dates are left where they fall, since a holiday
   * coinciding with a weekend is worth generating as well. The days the two spanning dates are
   * drawn from stop short of the end of the year by more than a week, so moving one off the weekend
   * cannot carry it into the next year.
   *
   * @param firstYear  the first of the three years the holidays span
   * @param weekendDays  the weekend days of the calendar being built
   */
  private def genHolidays(firstYear: Int, weekendDays: Set[DayOfWeek]): Gen[List[LocalDate]] =
    for {
      count <- Gen.choose(0, MaxHolidays - 2)
      first <- genDateInYear(firstYear, 300)
      last <- genDateInYear(firstYear + 2, 300)
      middle <- Gen.listOfN(
        count,
        Gen.choose(0, 2).flatMap(offset => genDateInYear(firstYear + offset, 365)))
    } yield firstWeekdayDate(first, weekendDays) ::
      firstWeekdayDate(last, weekendDays) ::
      middle

  /**
   * Generates a calendar of its own holidays, naming no built-in calendar.
   *
   * The identifier must not be a built-in one, or the encoder takes the name branch: the document
   * form of such a calendar is the structural one, carrying its identifier, its weekend days, its
   * first year, its holidays and the weekend dates it declares to be working days. One draw in
   * three declares a working day, which is the field of that object a calendar built without one
   * leaves empty; the date is inside the year range the holidays derive, because a working day
   * outside it is ignored.
   */
  val genCustomHolidayCalendar: Gen[ImmutableHolidayCalendar] =
    for {
      id <- genCustomHolidayCalendarId
      firstYear <- Gen.choose(MinYear, MaxYear - 2)
      weekendDays <- genWeekendDays
      holidays <- genHolidays(firstYear, weekendDays)
      workingDays <- Gen.frequency(
        2 -> Gen.const(List.empty[LocalDate]),
        1 -> Gen.const(List(firstWeekendDate(LocalDate.of(firstYear, 3, 1), weekendDays))))
    } yield ImmutableHolidayCalendar.of(id, holidays, weekendDays, workingDays)

  /**
   * Generates one of the built-in calendars.
   *
   * The set holds the twenty-six dated calendars and the four named after their weekends, including
   * the calendar of no holidays. Each is carried in a document as its name.
   */
  val genBuiltInHolidayCalendar: Gen[HolidayCalendar] = Gen.oneOf(builtInCalendars)

  val genLeafHolidayCalendar: Gen[HolidayCalendar] = Gen.frequency(
    3 -> genBuiltInHolidayCalendar,
    2 -> genCustomHolidayCalendar.map(calendar => calendar: HolidayCalendar))

  /**
   * Generates a holiday calendar of any of the four shapes the codec distinguishes.
   *
   * Those shapes are a built-in calendar carried as its name, a custom calendar carried
   * structurally, a combination and a link. The two compositions are built from leaves rather than
   * recursively, which keeps a generated calendar shallow while still exercising both wrappers; a
   * spec wanting a deeper one composes two generated calendars itself.
   */
  val genHolidayCalendar: Gen[HolidayCalendar] = Gen.frequency(
    4 -> genBuiltInHolidayCalendar,
    3 -> genCustomHolidayCalendar.map(calendar => calendar: HolidayCalendar),
    1 -> (for {
      first <- genLeafHolidayCalendar
      second <- genLeafHolidayCalendar
    } yield HolidayCalendar.Combined(first, second)),
    1 -> (for {
      first <- genLeafHolidayCalendar
      second <- genLeafHolidayCalendar
    } yield HolidayCalendar.Linked(first, second)))

  implicit val arbHolidayCalendar: Arbitrary[HolidayCalendar] = Arbitrary(genHolidayCalendar)

  implicit val arbImmutableHolidayCalendar: Arbitrary[ImmutableHolidayCalendar] =
    Arbitrary(genCustomHolidayCalendar)

  /**
   * The perturbation of holiday calendars, by their name.
   *
   * A calendar carrying holidays is equal to any calendar of the same identifier, and its name is
   * that identifier, so perturbing by the name is exactly as discriminating as equality.
   */
  implicit val cogenHolidayCalendar: Cogen[HolidayCalendar] = cogenBy[HolidayCalendar](_.name)

  /**
   * The perturbation of custom calendars, by their name.
   *
   * Sound for the reason above. It is declared at the type of the member because the law suites of
   * the member ask for a `Cogen[ImmutableHolidayCalendar]`, which a `Cogen[HolidayCalendar]` is
   * not: `Cogen` is invariant, as the `Hash` and `Show` of the member are.
   */
  implicit val cogenImmutableHolidayCalendar: Cogen[ImmutableHolidayCalendar] =
    cogenBy[ImmutableHolidayCalendar](_.name)

  /**
   * Generates a business day adjustment.
   *
   * The calendar is a built-in one, so a generated adjustment can be applied against the standard
   * reference data; the no-adjustment constant is drawn as well, since it is what every structure
   * carrying an adjustment defaults to.
   */
  val genBusinessDayAdjustment: Gen[BusinessDayAdjustment] = Gen.frequency(
    5 -> (for {
      convention <- genBusinessDayConvention
      calendar <- genBuiltInHolidayCalendarId
    } yield BusinessDayAdjustment.of(convention, calendar)),
    1 -> Gen.const(BusinessDayAdjustment.NONE))

  implicit val arbBusinessDayAdjustment: Arbitrary[BusinessDayAdjustment] =
    Arbitrary(genBusinessDayAdjustment)

  implicit val cogenBusinessDayAdjustment: Cogen[BusinessDayAdjustment] =
    cogenBy[BusinessDayAdjustment](_.toString)

  val genAdjustableDate: Gen[AdjustableDate] =
    for {
      date <- genLocalDate
      adjustment <- genBusinessDayAdjustment
    } yield AdjustableDate.of(date, adjustment)

  implicit val arbAdjustableDate: Arbitrary[AdjustableDate] = Arbitrary(genAdjustableDate)

  implicit val cogenAdjustableDate: Cogen[AdjustableDate] = cogenBy[AdjustableDate](_.toString)

  /**
   * Generates a run of adjustable dates.
   *
   * The dates have to be strictly increasing, so they are built by walking forward from one date in
   * steps of at least a day; non-emptiness is carried by the `NonEmptyList` type of the field
   * rather than by a check.
   */
  val genAdjustableDates: Gen[AdjustableDates] =
    for {
      first <- genLocalDate
      count <- Gen.choose(0, MaxDates - 1)
      steps <- Gen.listOfN(count, Gen.choose(1L, 400L))
      adjustment <- genBusinessDayAdjustment
      dates = NonEmptyList(
        first,
        steps.scanLeft(0L)((total, step) => total + step).tail.map(offset => first.plusDays(offset)))
      value <- fromEither("AdjustableDates.of", AdjustableDates.of(adjustment, dates))
    } yield value

  implicit val arbAdjustableDates: Arbitrary[AdjustableDates] = Arbitrary(genAdjustableDates)

  implicit val cogenAdjustableDates: Cogen[AdjustableDates] =
    cogenBy[AdjustableDates](_.toString)

  /**
   * Generates a days adjustment.
   *
   * All three shapes are drawn: an addition of calendar days, an addition of business days against
   * a named calendar, and the constant that adjusts nothing. The number of days may be negative, as
   * the type allows.
   */
  val genDaysAdjustment: Gen[DaysAdjustment] = Gen.frequency(
    3 -> (for {
      days <- Gen.choose(-10, 10)
      adjustment <- genBusinessDayAdjustment
    } yield DaysAdjustment.ofCalendarDays(days, adjustment)),
    3 -> (for {
      days <- Gen.choose(-10, 10)
      calendar <- genBuiltInHolidayCalendarId
      adjustment <- genBusinessDayAdjustment
    } yield DaysAdjustment.ofBusinessDays(days, calendar, adjustment)),
    1 -> Gen.const(DaysAdjustment.NONE))

  implicit val arbDaysAdjustment: Arbitrary[DaysAdjustment] = Arbitrary(genDaysAdjustment)

  implicit val cogenDaysAdjustment: Cogen[DaysAdjustment] = cogenBy[DaysAdjustment](_.toString)

  val genPeriodAdditionConvention: Gen[PeriodAdditionConvention] =
    Gen.oneOf(PeriodAdditionConvention.values.toList)

  implicit val arbPeriodAdditionConvention: Arbitrary[PeriodAdditionConvention] =
    Arbitrary(genPeriodAdditionConvention)

  implicit val cogenPeriodAdditionConvention: Cogen[PeriodAdditionConvention] =
    cogenBy[PeriodAdditionConvention](_.name)

  /**
   * Generates a period adjustment.
   *
   * The two month-based conventions reject a period holding days, so the period is drawn from the
   * month-based range where the convention drawn is one of them, and from the wider range where it
   * is not. That pairing is what keeps the factory from rejecting a draw.
   */
  val genPeriodAdjustment: Gen[PeriodAdjustment] =
    for {
      convention <- genPeriodAdditionConvention
      period <- if (convention.isMonthBased) genMonthBasedPeriod else genPeriod
      adjustment <- genBusinessDayAdjustment
      value <- fromEither(
        "PeriodAdjustment.of",
        PeriodAdjustment.of(period, convention, adjustment))
    } yield value

  implicit val arbPeriodAdjustment: Arbitrary[PeriodAdjustment] = Arbitrary(genPeriodAdjustment)

  implicit val cogenPeriodAdjustment: Cogen[PeriodAdjustment] =
    cogenBy[PeriodAdjustment](_.toString)

  val genMonthBasedTenor: Gen[Tenor] = Gen.frequency(
    3 -> Gen
      .choose(1, 24)
      .flatMap(months => fromEither("Tenor.ofMonths", Tenor.ofMonths(months))),
    1 -> Gen.choose(1, 50).flatMap(years => fromEither("Tenor.ofYears", Tenor.ofYears(years))))

  /**
   * Generates a tenor.
   *
   * Every tenor is positive, which the factory requires, and a tenor of whole weeks is drawn as
   * well as one of days and one of months, since the canonical name of a tenor depends on which of
   * those it is.
   */
  val genTenor: Gen[Tenor] = Gen.frequency(
    4 -> genMonthBasedTenor,
    1 -> Gen.choose(1, 6).flatMap(days => fromEither("Tenor.ofDays", Tenor.ofDays(days))),
    1 -> Gen.choose(1, 52).flatMap(weeks => fromEither("Tenor.ofWeeks", Tenor.ofWeeks(weeks))))

  implicit val arbTenor: Arbitrary[Tenor] = Arbitrary(genTenor)

  implicit val cogenTenor: Cogen[Tenor] = cogenBy[Tenor](_.name)

  /**
   * Generates a tenor adjustment.
   *
   * The pairing rule is the one [[genPeriodAdjustment]] follows: a month-based convention takes a
   * month-based tenor, because it rejects a tenor holding days.
   */
  val genTenorAdjustment: Gen[TenorAdjustment] =
    for {
      convention <- genPeriodAdditionConvention
      tenor <- if (convention.isMonthBased) genMonthBasedTenor else genTenor
      adjustment <- genBusinessDayAdjustment
      value <- fromEither("TenorAdjustment.of", TenorAdjustment.of(tenor, convention, adjustment))
    } yield value

  implicit val arbTenorAdjustment: Arbitrary[TenorAdjustment] = Arbitrary(genTenorAdjustment)

  implicit val cogenTenorAdjustment: Cogen[TenorAdjustment] =
    cogenBy[TenorAdjustment](_.toString)

  /**
   * Generates a market tenor.
   *
   * The four constants are drawn as well as the spot-starting tenor of a generated tenor, because
   * their codes - overnight, tomorrow-next, spot-next and spot-week - are the ones whose text form
   * is not the text form of a tenor.
   */
  val genMarketTenor: Gen[MarketTenor] = Gen.frequency(
    3 -> genTenor.flatMap(tenor => fromEither("MarketTenor.ofSpot", MarketTenor.ofSpot(tenor))),
    2 -> Gen.oneOf(MarketTenor.ON, MarketTenor.TN, MarketTenor.SN, MarketTenor.SW))

  implicit val arbMarketTenor: Arbitrary[MarketTenor] = Arbitrary(genMarketTenor)

  implicit val cogenMarketTenor: Cogen[MarketTenor] = cogenBy[MarketTenor](_.code)

  val genDateSequence: Gen[DateSequence] = Gen.oneOf(DateSequence.values.toList)

  implicit val arbDateSequence: Arbitrary[DateSequence] = Arbitrary(genDateSequence)

  implicit val cogenDateSequence: Cogen[DateSequence] = cogenBy[DateSequence](_.name)

  /**
   * Generates an instruction selecting a date from a sequence.
   *
   * The three starting points are drawn in turn: no starting point at all, a year and month, and a
   * minimum period. The factory rejects an instruction naming both a year and month and a minimum
   * period, so the two are never drawn together; the zero period is drawn deliberately, because the
   * factory normalises it away to no minimum period at all.
   */
  val genSequenceDate: Gen[SequenceDate] =
    for {
      sequenceNumber <- Gen.choose(1, 12)
      fullSequence <- Gen.oneOf(true, false)
      yearMonth <- genYearMonth
      minimumPeriod <- Gen.oneOf(
        Period.ZERO,
        Period.ofDays(2),
        Period.ofWeeks(1),
        Period.ofMonths(1))
      startingPoint <- Gen.choose(0, 2)
      value <- fromEither(
        "SequenceDate.of",
        SequenceDate.of(
          if (startingPoint == 1) Some(yearMonth) else None,
          if (startingPoint == 2) Some(minimumPeriod) else None,
          sequenceNumber,
          fullSequence))
    } yield value

  implicit val arbSequenceDate: Arbitrary[SequenceDate] = Arbitrary(genSequenceDate)

  implicit val cogenSequenceDate: Cogen[SequenceDate] = cogenBy[SequenceDate](_.toString)

  //-------------------------------------------------------------------------
  // The index package.
  //-------------------------------------------------------------------------
  val genIborIndex: Gen[IborIndex] = Gen.oneOf(IborIndex.values.toList)

  implicit val arbIborIndex: Arbitrary[IborIndex] = Arbitrary(genIborIndex)

  implicit val cogenIborIndex: Cogen[IborIndex] = cogenBy[IborIndex](_.name)

  val genOvernightIndex: Gen[OvernightIndex] = Gen.oneOf(OvernightIndex.values.toList)

  implicit val arbOvernightIndex: Arbitrary[OvernightIndex] = Arbitrary(genOvernightIndex)

  implicit val cogenOvernightIndex: Cogen[OvernightIndex] = cogenBy[OvernightIndex](_.name)

  val genPriceIndex: Gen[PriceIndex] = Gen.oneOf(PriceIndex.values.toList)

  implicit val arbPriceIndex: Arbitrary[PriceIndex] = Arbitrary(genPriceIndex)

  implicit val cogenPriceIndex: Cogen[PriceIndex] = cogenBy[PriceIndex](_.name)

  val genFxIndex: Gen[FxIndex] = Gen.oneOf(FxIndex.values.toList)

  implicit val arbFxIndex: Arbitrary[FxIndex] = Arbitrary(genFxIndex)

  implicit val cogenFxIndex: Cogen[FxIndex] = cogenBy[FxIndex](_.name)

  val genFloatingRateType: Gen[FloatingRateType] = Gen.oneOf(FloatingRateType.values.toList)

  implicit val arbFloatingRateType: Arbitrary[FloatingRateType] = Arbitrary(genFloatingRateType)

  implicit val cogenFloatingRateType: Cogen[FloatingRateType] = cogenBy[FloatingRateType](_.name)

  val genFloatingRateName: Gen[FloatingRateName] = Gen.oneOf(FloatingRateName.values.toList)

  implicit val arbFloatingRateName: Arbitrary[FloatingRateName] = Arbitrary(genFloatingRateName)

  implicit val cogenFloatingRateName: Cogen[FloatingRateName] = cogenBy[FloatingRateName](_.name)

  /**
   * The Ibor indices whose calendars the standard reference data resolves.
   *
   * An observation of a fixing is derived rather than given: the factory resolves the fixing
   * calendar and the two date offsets of the index and computes the dependent dates, so it reports
   * a failure for an index naming a calendar the reference data does not hold. Thirteen of the
   * default calendars have no built-in calendar, so the indices naming one of those cannot be
   * observed against the standard data, and are filtered out here rather than drawn and discarded.
   *
   * The filter probes one fixed date, which decides it for every date: an index resolves its
   * calendars or it does not, and that does not depend on the date being observed.
   */
  private val observableIborIndices: List[IborIndex] =
    IborIndex.values.toList.filter(index =>
      IborIndexObservation.of(index, CalendarProbeDate, ReferenceData.standard).isRight)

  private val observableOvernightIndices: List[OvernightIndex] =
    OvernightIndex.values.toList.filter(index =>
      OvernightIndexObservation.of(index, CalendarProbeDate, ReferenceData.standard).isRight)

  private val observableFxIndices: List[FxIndex] =
    FxIndex.values.toList.filter(index =>
      FxIndexObservation.of(index, CalendarProbeDate, ReferenceData.standard).isRight)

  val genObservableIborIndex: Gen[IborIndex] = Gen.oneOf(observableIborIndices)

  val genObservableOvernightIndex: Gen[OvernightIndex] = Gen.oneOf(observableOvernightIndices)

  val genObservableFxIndex: Gen[FxIndex] = Gen.oneOf(observableFxIndices)

  /**
   * Generates an observation of an Ibor index fixing.
   *
   * The observation is built by the one factory that exists for it, which derives the effective
   * date, the maturity date and the year fraction from the index and the fixing date. Nothing else
   * can build one, which is why an inconsistent set of dates is not a value this type holds - and
   * why the document form of an observation is read back through the same derivation.
   */
  val genIborIndexObservation: Gen[IborIndexObservation] =
    for {
      index <- genObservableIborIndex
      fixingDate <- genLocalDate
      value <- fromEither(
        "IborIndexObservation.of",
        IborIndexObservation.of(index, fixingDate, ReferenceData.standard))
    } yield value

  implicit val arbIborIndexObservation: Arbitrary[IborIndexObservation] =
    Arbitrary(genIborIndexObservation)

  implicit val cogenIborIndexObservation: Cogen[IborIndexObservation] =
    cogenBy[IborIndexObservation](_.toString)

  val genOvernightIndexObservation: Gen[OvernightIndexObservation] =
    for {
      index <- genObservableOvernightIndex
      fixingDate <- genLocalDate
      value <- fromEither(
        "OvernightIndexObservation.of",
        OvernightIndexObservation.of(index, fixingDate, ReferenceData.standard))
    } yield value

  implicit val arbOvernightIndexObservation: Arbitrary[OvernightIndexObservation] =
    Arbitrary(genOvernightIndexObservation)

  implicit val cogenOvernightIndexObservation: Cogen[OvernightIndexObservation] =
    cogenBy[OvernightIndexObservation](_.toString)

  val genFxIndexObservation: Gen[FxIndexObservation] =
    for {
      index <- genObservableFxIndex
      fixingDate <- genLocalDate
      value <- fromEither(
        "FxIndexObservation.of",
        FxIndexObservation.of(index, fixingDate, ReferenceData.standard))
    } yield value

  implicit val arbFxIndexObservation: Arbitrary[FxIndexObservation] =
    Arbitrary(genFxIndexObservation)

  implicit val cogenFxIndexObservation: Cogen[FxIndexObservation] =
    cogenBy[FxIndexObservation](_.toString)

  /**
   * Generates an observation of a price index fixing.
   *
   * This one needs no reference data: a price index is fixed for a month rather than on a business
   * day, so there is no calendar to resolve and nothing derived to keep consistent.
   */
  val genPriceIndexObservation: Gen[PriceIndexObservation] =
    for {
      index <- genPriceIndex
      fixingMonth <- genYearMonth
    } yield PriceIndexObservation.of(index, fixingMonth)

  implicit val arbPriceIndexObservation: Arbitrary[PriceIndexObservation] =
    Arbitrary(genPriceIndexObservation)

  implicit val cogenPriceIndexObservation: Cogen[PriceIndexObservation] =
    cogenBy[PriceIndexObservation](_.toString)

  //-------------------------------------------------------------------------
  // The location package.
  //-------------------------------------------------------------------------
  /**
   * The countries named by a constant of the type, in declaration order.
   *
   * The type is an open value rather than a closed family - it accepts any two upper-case letters -
   * so it publishes no list of its members. These are the constants a spec is likely to name, which
   * is what makes them worth drawing more often than an arbitrary code.
   */
  private val namedCountries: List[Country] = List(
    Country.EU,
    Country.GB,
    Country.US,
    Country.DE,
    Country.FR,
    Country.CH,
    Country.JP,
    Country.AU,
    Country.CA,
    Country.BR,
    Country.NZ,
    Country.ZA,
    Country.SG,
    Country.HK,
    Country.IN,
    Country.CN)

  /**
   * Generates a country.
   *
   * Mostly one of the countries a constant names, and occasionally an arbitrary pair of upper-case
   * letters, which the type accepts whether or not it names a country a constant knows. Both are
   * worth drawing: the constants are what a spec asserts against, and the open codes are what the
   * validation of the type actually admits.
   */
  val genCountry: Gen[Country] = Gen.frequency(
    3 -> Gen.oneOf(namedCountries),
    1 -> (for {
      letters <- Gen.listOfN(2, Gen.alphaUpperChar)
      value <- fromEither("Country.of", Country.of(letters.mkString))
    } yield value))

  implicit val arbCountry: Arbitrary[Country] = Arbitrary(genCountry)

  implicit val cogenCountry: Cogen[Country] = cogenBy[Country](_.code)

  //-------------------------------------------------------------------------
  // The schedule package.
  //-------------------------------------------------------------------------
  /**
   * Generates a frequency of one, three, six or twelve months.
   *
   * These are the frequencies a schedule is generated at, so the schedule-shaped values below draw
   * from here: a schedule whose frequency divides its own length is the case worth generating, and
   * an arbitrary frequency rarely does.
   */
  val genRegularFrequency: Gen[Frequency] =
    Gen
      .oneOf(1, 3, 6, 12)
      .flatMap(months => fromEither("Frequency.ofMonths", Frequency.ofMonths(months)))

  /**
   * Generates a frequency.
   *
   * Every length is positive, which the factory requires, and the term frequency is drawn as well,
   * since it is the one whose text form is a word rather than a period.
   */
  val genFrequency: Gen[Frequency] = Gen.frequency(
    4 -> genRegularFrequency,
    2 -> Gen
      .choose(1, 24)
      .flatMap(months => fromEither("Frequency.ofMonths", Frequency.ofMonths(months))),
    1 -> Gen
      .choose(1, 52)
      .flatMap(weeks => fromEither("Frequency.ofWeeks", Frequency.ofWeeks(weeks))),
    1 -> Gen
      .choose(1, 10)
      .flatMap(years => fromEither("Frequency.ofYears", Frequency.ofYears(years))),
    1 -> Gen.const(Frequency.TERM))

  implicit val arbFrequency: Arbitrary[Frequency] = Arbitrary(genFrequency)

  implicit val cogenFrequency: Cogen[Frequency] = cogenBy[Frequency](_.name)

  val genStubConvention: Gen[StubConvention] = Gen.oneOf(StubConvention.values.toList)

  implicit val arbStubConvention: Arbitrary[StubConvention] = Arbitrary(genStubConvention)

  implicit val cogenStubConvention: Cogen[StubConvention] = cogenBy[StubConvention](_.name)

  /**
   * Generates one of the roll conventions.
   *
   * The family holds the eight standard conventions, the thirty day-of-month conventions and the
   * seven day-of-week conventions, and every one of them is drawn from here.
   */
  val genRollConvention: Gen[RollConvention] = Gen.oneOf(RollConvention.values.toList)

  implicit val arbRollConvention: Arbitrary[RollConvention] = Arbitrary(genRollConvention)

  implicit val cogenRollConvention: Cogen[RollConvention] = cogenBy[RollConvention](_.name)

  /**
   * Generates a schedule period.
   *
   * The unadjusted dates are at least ten days apart and each adjusted date is within three days of
   * the unadjusted one it belongs to, so both pairs are strictly in order however the shifts fall -
   * which is what the factory checks, once for each pair.
   */
  val genSchedulePeriod: Gen[SchedulePeriod] =
    for {
      unadjustedStart <- genLocalDate
      length <- Gen.choose(10, 400)
      startShift <- Gen.choose(-3, 3)
      endShift <- Gen.choose(-3, 3)
      unadjustedEnd = unadjustedStart.plusDays(length.toLong)
      value <- fromEither(
        "SchedulePeriod.of",
        SchedulePeriod.of(
          unadjustedStart.plusDays(startShift.toLong),
          unadjustedEnd.plusDays(endShift.toLong),
          unadjustedStart,
          unadjustedEnd))
    } yield value

  implicit val arbSchedulePeriod: Arbitrary[SchedulePeriod] = Arbitrary(genSchedulePeriod)

  implicit val cogenSchedulePeriod: Cogen[SchedulePeriod] = cogenBy[SchedulePeriod](_.toString)

  /**
   * Generates a schedule of contiguous periods.
   *
   * The periods run end to end at the frequency the schedule reports, which satisfies the ordering
   * [[Schedule.of]] checks: it refuses a list that runs backwards or overlaps, in the unadjusted
   * pair or the adjusted one, although it allows a gap between one period and the next.
   * Non-emptiness is carried by the `NonEmptyList` type of the field. A schedule of unrelated
   * periods would be a value no generation could produce, and the specs reading periods off a
   * schedule expect the periods to fit together.
   */
  val genSchedule: Gen[Schedule] =
    for {
      start <- genLocalDate
      months <- Gen.oneOf(1, 3, 6, 12)
      count <- Gen.choose(1, MaxPeriods)
      frequency <- fromEither("Frequency.ofMonths", Frequency.ofMonths(months))
      rollConvention <- genRollConvention
      boundaries = (0 to count).toList
        .map(index => start.plusMonths(index.toLong * months.toLong))
      periods <- fromEither(
        "SchedulePeriod.of",
        sequenceEither(boundaries.zip(boundaries.tail).map {
          case (from, to) => SchedulePeriod.of(from, to)
        }))
      nonEmptyPeriods <- fromOption("Schedule.periods", NonEmptyList.fromList(periods))
      value <- fromEither(
        "Schedule.of",
        Schedule.of(nonEmptyPeriods, frequency, rollConvention))
    } yield value

  implicit val arbSchedule: Arbitrary[Schedule] = Arbitrary(genSchedule)

  implicit val cogenSchedule: Cogen[Schedule] = cogenBy[Schedule](_.toString)

  /**
   * Generates a periodic schedule definition.
   *
   * The eleven fields are drawn as one coherent shape rather than independently, because seven
   * order invariants hold between the five date-bearing ones and independent draws would fail most
   * of them: a start date, an end date a whole number of periods later, and where a regular period
   * boundary is drawn it falls on one of those period boundaries; an overriding start date, where
   * one is drawn, falls before the start date and therefore before every boundary.
   */
  val genPeriodicSchedule: Gen[PeriodicSchedule] =
    for {
      start <- genLocalDate
      months <- Gen.oneOf(1, 3, 6, 12)
      count <- Gen.choose(1, MaxPeriods)
      frequency <- fromEither("Frequency.ofMonths", Frequency.ofMonths(months))
      adjustment <- genBusinessDayAdjustment
      startAdjustment <- Gen.option(genBusinessDayAdjustment)
      endAdjustment <- Gen.option(genBusinessDayAdjustment)
      stubConvention <- Gen.option(genStubConvention)
      rollConvention <- Gen.option(genRollConvention)
      firstRegularIndex <- Gen.choose(0, count)
      lastRegularIndex <- Gen.choose(firstRegularIndex, count)
      firstRegular <- Gen.option(
        Gen.const(start.plusMonths(firstRegularIndex.toLong * months.toLong)))
      lastRegular <- Gen.option(
        Gen.const(start.plusMonths(lastRegularIndex.toLong * months.toLong)))
      overrideShift <- Gen.choose(1, 30)
      overrideAdjustment <- genBusinessDayAdjustment
      overrideStart <- Gen.option(
        Gen.const(
          AdjustableDate.of(start.minusDays(overrideShift.toLong), overrideAdjustment)))
      value <- fromEither(
        "PeriodicSchedule.of",
        PeriodicSchedule.of(
          start,
          start.plusMonths(count.toLong * months.toLong),
          frequency,
          adjustment,
          startAdjustment,
          endAdjustment,
          stubConvention,
          rollConvention,
          firstRegular,
          lastRegular,
          overrideStart))
    } yield value

  implicit val arbPeriodicSchedule: Arbitrary[PeriodicSchedule] = Arbitrary(genPeriodicSchedule)

  implicit val cogenPeriodicSchedule: Cogen[PeriodicSchedule] =
    cogenBy[PeriodicSchedule](_.toString)

  //-------------------------------------------------------------------------
  // The value package.
  //-------------------------------------------------------------------------
  val genValueAdjustmentType: Gen[ValueAdjustmentType] =
    Gen.oneOf(ValueAdjustmentType.values.toList)

  implicit val arbValueAdjustmentType: Arbitrary[ValueAdjustmentType] =
    Arbitrary(genValueAdjustmentType)

  implicit val cogenValueAdjustmentType: Cogen[ValueAdjustmentType] =
    cogenBy[ValueAdjustmentType](_.name)

  /**
   * Generates a value adjustment that does not replace the value it is applied to.
   *
   * A sequence of steps rejects a replacing adjustment - every step of a sequence applies the same
   * adjustment, and replacing a value repeatedly would leave the sequence with no effect beyond its
   * first step - so the sequence generator draws from here.
   */
  val genNonReplacingValueAdjustment: Gen[ValueAdjustment] = Gen.oneOf(
    genDouble.map(amount => ValueAdjustment.ofDeltaAmount(amount)),
    genDouble.map(multiplier => ValueAdjustment.ofDeltaMultiplier(multiplier)),
    genDouble.map(multiplier => ValueAdjustment.ofMultiplier(multiplier)))

  /**
   * Generates a value adjustment.
   *
   * The value carried is drawn from the whole range of `double`, edge values included: the type
   * holds whatever number it is given, its equality treats every `NaN` as equal to every other and
   * keeps `-0.0` distinct from `0.0`, and a document carries the three values JSON has no number
   * for in a tagged form.
   */
  val genValueAdjustment: Gen[ValueAdjustment] = Gen.frequency(
    1 -> genDouble.map(replacement => ValueAdjustment.ofReplace(replacement)),
    3 -> genNonReplacingValueAdjustment)

  implicit val arbValueAdjustment: Arbitrary[ValueAdjustment] = Arbitrary(genValueAdjustment)

  implicit val cogenValueAdjustment: Cogen[ValueAdjustment] =
    cogenBy[ValueAdjustment](_.toString)

  /**
   * Generates a value with its derivatives.
   *
   * Both the value and the derivatives are drawn with the IEEE-754 edge values: the factory takes
   * any number and any array, and equality treats every `NaN` as equal to every other and keeps
   * `-0.0` distinct from `0.0`, so a `NaN` derivative is a value the type holds.
   */
  val genValueDerivatives: Gen[ValueDerivatives] =
    for {
      value <- genDouble
      derivatives <- genDoubleArray
    } yield ValueDerivatives.of(value, derivatives)

  implicit val arbValueDerivatives: Arbitrary[ValueDerivatives] = Arbitrary(genValueDerivatives)

  implicit val cogenValueDerivatives: Cogen[ValueDerivatives] =
    cogenBy[ValueDerivatives](_.toString)

  /**
   * Generates a half-up rounding convention.
   *
   * Both factories are drawn: the one naming a number of decimal places, and the one naming a
   * fraction of the last place as well.
   */
  val genHalfUpRounding: Gen[HalfUp] = Gen.frequency(
    3 -> Gen
      .choose(0, MaxRoundingPlaces)
      .flatMap(places => fromEither("HalfUp.ofDecimalPlaces", HalfUp.ofDecimalPlaces(places))),
    1 -> (for {
      places <- Gen.choose(0, MaxRoundingPlaces)
      fraction <- Gen.oneOf(0, 2, 4, 8, 16, 32)
      value <- fromEither(
        "HalfUp.ofFractionalDecimalPlaces",
        HalfUp.ofFractionalDecimalPlaces(places, fraction))
    } yield value))

  implicit val arbHalfUp: Arbitrary[HalfUp] = Arbitrary(genHalfUpRounding)

  /**
   * The perturbation of half-up roundings, by their rendering.
   *
   * The rendering names both fields its equality compares - the number of decimal places always,
   * and the fraction where there is one. It is declared at the type of the member as well as at the
   * type of the family because the law suites of the member ask for a `Cogen[HalfUp]`, and a
   * `Cogen[Rounding]` is not one: `Cogen` is invariant in its type.
   */
  implicit val cogenHalfUp: Cogen[HalfUp] = cogenBy[HalfUp](_.toString)

  val genRounding: Gen[Rounding] =
    Gen.frequency(1 -> Gen.const[Rounding](NoRounding), 3 -> genHalfUpRounding)

  implicit val arbRounding: Arbitrary[Rounding] = Arbitrary(genRounding)

  implicit val cogenRounding: Cogen[Rounding] = cogenBy[Rounding](_.toString)

  /**
   * Generates a step changing a value.
   *
   * A step is positioned either by the index of the schedule period it falls at or by a date, and
   * exactly one of the two: the factory rejects a step naming both and a step naming neither, so
   * the two shapes are drawn from separate branches and never mixed. The index is one or greater,
   * which the factory also requires.
   */
  val genValueStep: Gen[ValueStep] = Gen.frequency(
    1 -> (for {
      periodIndex <- Gen.choose(1, MaxPeriods)
      adjustment <- genValueAdjustment
      value <- fromEither("ValueStep.of", ValueStep.of(periodIndex, adjustment))
    } yield value),
    1 -> (for {
      date <- genLocalDate
      adjustment <- genValueAdjustment
    } yield ValueStep.of(date, adjustment)))

  implicit val arbValueStep: Arbitrary[ValueStep] = Arbitrary(genValueStep)

  implicit val cogenValueStep: Cogen[ValueStep] = cogenBy[ValueStep](_.toString)

  /**
   * Generates a sequence of steps changing a value.
   *
   * The first date is on or before the last and the adjustment is never a replacing one, both of
   * which the factory requires.
   */
  val genValueStepSequence: Gen[ValueStepSequence] =
    for {
      firstStepDate <- genLocalDate
      months <- Gen.choose(1, 12)
      count <- Gen.choose(0, MaxSteps)
      frequency <- fromEither("Frequency.ofMonths", Frequency.ofMonths(months))
      adjustment <- genNonReplacingValueAdjustment
      value <- fromEither(
        "ValueStepSequence.of",
        ValueStepSequence.of(
          firstStepDate,
          firstStepDate.plusMonths(count.toLong * months.toLong),
          frequency,
          adjustment))
    } yield value

  implicit val arbValueStepSequence: Arbitrary[ValueStepSequence] =
    Arbitrary(genValueStepSequence)

  implicit val cogenValueStepSequence: Cogen[ValueStepSequence] =
    cogenBy[ValueStepSequence](_.toString)

  /**
   * Generates a schedule of values.
   *
   * The initial value, the steps and the sequence are drawn independently, because a schedule of
   * values places almost no constraint between them - whether a step lines up with a period
   * boundary is a question about the schedule it is resolved against, not about the definition. The
   * one constraint it does place is that two steps must not name the same position with different
   * adjustments, which the drawn steps are thinned to satisfy.
   */
  val genValueSchedule: Gen[ValueSchedule] =
    for {
      initialValue <- genDouble
      stepCount <- Gen.choose(0, MaxSteps)
      drawn <- Gen.listOfN(stepCount, genValueStep)
      // no position may be named twice with different adjustments, so the drawn steps are thinned
      // to one per position - keeping the first, so the list stays in the order it was drawn in -
      // which leaves the factory nothing to refuse
      steps = drawn.distinctBy(step => step.periodIndex.toLeft(step.date))
      stepSequence <- Gen.option(genValueStepSequence)
      value <- fromEither(
        "ValueSchedule.of",
        ValueSchedule.of(initialValue, steps, stepSequence))
    } yield value

  implicit val arbValueSchedule: Arbitrary[ValueSchedule] = Arbitrary(genValueSchedule)

  implicit val cogenValueSchedule: Cogen[ValueSchedule] = cogenBy[ValueSchedule](_.toString)

  //-------------------------------------------------------------------------
  // The currency package, continued: the adjustable payment, which needs an adjustable date.
  //-------------------------------------------------------------------------
  val genAdjustablePayment: Gen[AdjustablePayment] =
    for {
      amount <- genCurrencyAmount
      date <- genAdjustableDate
    } yield AdjustablePayment.of(amount, date)

  implicit val arbAdjustablePayment: Arbitrary[AdjustablePayment] = Arbitrary(genAdjustablePayment)

  implicit val cogenAdjustablePayment: Cogen[AdjustablePayment] =
    cogenBy[AdjustablePayment](_.toString)

  //-------------------------------------------------------------------------
  // The shrinkings.
  //
  // Three rules hold throughout: a candidate is built by the same validated factory the generator
  // uses, it keeps the invariant its generator promises, and it is strictly smaller than its input
  // under the measure named on the instance - so repeated shrinking terminates at the floor that
  // instance documents.
  //-------------------------------------------------------------------------
  /**
   * Turns the outcome of a validated factory into a shrink candidate.
   *
   * This is [[fromEither]] in the shape shrinking needs: a `Right` contributes the one candidate it
   * holds, a `Left` contributes none, so an outcome the factory rejected is dropped rather than
   * forced into a value. That branch is reached where a reduction happens to break a constraint of
   * the type - a pairing rule between two fields, say - and dropping it there is exactly right: the
   * candidate would not have been a value of the type.
   */
  private def candidateOf[E, A](outcome: Either[E, A]): LazyList[A] =
    outcome match {
      case Right(value) => LazyList(value)
      case Left(_) => LazyList.empty
    }

  /**
   * The floor every generated number shrinks to, held as bits because `==` cannot tell `-0.0` from
   * `0.0`.
   */
  private val PositiveZeroBits: Long = java.lang.Double.doubleToLongBits(0.0d)

  /**
   * Returns whether a number is already the positive zero shrinking simplifies towards.
   *
   * The test is on `doubleToLongBits` rather than on `==` because `==` cannot tell `-0.0` from
   * `0.0` and calls `NaN` unequal to itself, while the generators draw both and the domain types
   * hold both. Comparing the bits keeps the two zeroes distinct and makes every `NaN` one value,
   * which is what makes the measure fall by exactly one for each number a shrinking simplifies.
   */
  private def isSimplifiedNumber(value: Double): Boolean =
    java.lang.Double.doubleToLongBits(value) == PositiveZeroBits

  /**
   * The numbers a generated number shrinks to: positive zero, and nothing else.
   *
   * One step rather than a sequence of halvings, which is the shape `strata-collect` uses for the
   * elements of an array and for the same reason: these types compare a number exactly, so a
   * property that depends on a particular number is not made easier to read by a number half the
   * size, while a property that depends on none at all is reported with zeroes.
   */
  private def simplerNumbers(value: Double): LazyList[Double] =
    if (isSimplifiedNumber(value)) LazyList.empty else LazyList(0.0d)

  /** The first date of the generated window, which is the floor every date shrinks to. */
  private val FirstDate: LocalDate = LocalDate.of(MinYear, 1, 1)

  private val FirstYearMonth: YearMonth = YearMonth.of(MinYear, 1)

  /**
   * The dates a generated date shrinks to: the first date of the generated window.
   *
   * Every date this file produces falls in that window, so the floor is inside it and a candidate
   * is a date the generators themselves could have drawn.
   */
  private def simplerDates(date: LocalDate): LazyList[LocalDate] =
    if (date == FirstDate) LazyList.empty else LazyList(FirstDate)

  private def simplerMonths(month: YearMonth): LazyList[YearMonth] =
    if (month == FirstYearMonth) LazyList.empty else LazyList(FirstYearMonth)

  /**
   * The whole numbers a generated whole number shrinks to, towards zero.
   *
   * Zero first, so a property that fails for every count reports zero immediately, then half the
   * magnitude, which keeps the path short where zero is not itself a counterexample, then the
   * magnitude of a negative number, which drops the sign. Every candidate is strictly nearer zero
   * than the input, so zero has no candidates.
   */
  private def simplerIntegers(value: Int): LazyList[Int] =
    LazyList(0, value / 2, math.abs(value)).distinct.filter(candidate =>
      math.abs(candidate) < math.abs(value))

  /**
   * The positive whole numbers a generated one-based position shrinks to, towards one.
   *
   * The floor is one rather than zero, because the factories that take a position - the index of a
   * schedule period a step falls at - reject zero, so a candidate of zero would be dropped by the
   * factory and the shrinking would stall one step early.
   */
  private def simplerPositions(value: Int): LazyList[Int] =
    LazyList(1, value / 2).distinct.filter(candidate => candidate >= 1 && candidate < value)

  /**
   * The texts a generated text shrinks to: its first character.
   *
   * Every text this file generates is a scheme, a value or an identifier of at least one character,
   * and each of the factories taking one accepts a text of one character, so the floor is a value
   * of the type rather than a candidate the factory would reject.
   */
  private def shorterTexts(text: String): LazyList[String] =
    if (text.length > 1) LazyList(text.take(1)) else LazyList.empty

  /**
   * The members of a closed family a member shrinks to: the ones declared before it.
   *
   * The measure is the position of the member in the order the list gives, and the floor is its
   * first element, which has no candidates. A value the list does not hold is reported as absent by
   * `indexOf` and offers no candidate at all, rather than offering every member as `take` on a
   * negative index would.
   *
   * @param members  the members of the family, in declaration order
   */
  private def earlierMembers[A](members: List[A])(value: A): LazyList[A] =
    LazyList.from(members.take(math.max(members.indexOf(value), 0)))

  /**
   * The periods a generated period shrinks to, reduced in the unit the period is stated in.
   *
   * A period of months shrinks to fewer months, a period of years to fewer years, a period of days
   * to fewer days, and a period of a whole number of weeks to fewer whole weeks. The unit is never
   * changed, because the month-based addition conventions reject a period holding days and the
   * canonical name of a tenor or a frequency is decided by the unit it is stated in.
   *
   * The measure is the number of units the period holds, and the floor is one unit of whichever
   * kind the period is: a period of zero is rejected by every factory that takes one here.
   */
  private def simplerPeriods(period: Period): LazyList[Period] = {
    val years = period.getYears
    val months = period.getMonths
    val days = period.getDays
    if (years > 1 && months == 0 && days == 0) {
      LazyList(1, years / 2).distinct.filter(count => count >= 1 && count < years).map(Period.ofYears)
    } else if (years == 0 && months > 1 && days == 0) {
      LazyList(1, months / 2).distinct.filter(count => count >= 1 && count < months).map(Period.ofMonths)
    } else if (years == 0 && months == 0 && days > 7 && days % 7 == 0) {
      val weeks = days / 7
      LazyList(1, weeks / 2).distinct.filter(count => count >= 1 && count < weeks).map(Period.ofWeeks)
    } else if (years == 0 && months == 0 && days > 1) {
      LazyList(1, days / 2).distinct.filter(count => count >= 1 && count < days).map(Period.ofDays)
    } else {
      LazyList.empty
    }
  }

  //-------------------------------------------------------------------------
  // The shrinkings of the root package.
  //-------------------------------------------------------------------------
  /**
   * Shrinks an identifier by shortening its scheme and its value.
   *
   * The measure is the length of the scheme plus the length of the value, and each candidate cuts
   * one of the two to a single character, so the floor is an identifier of one character in each
   * part.
   */
  implicit val shrinkStandardId: Shrink[StandardId] = Shrink.withLazyList(standardIdCandidates)

  private def standardIdCandidates(identifier: StandardId): LazyList[StandardId] = {
    val schemes = shorterTexts(identifier.scheme)
      .flatMap(scheme => candidateOf(StandardId.of(scheme, identifier.value)))
    val values = shorterTexts(identifier.value)
      .flatMap(value => candidateOf(StandardId.of(identifier.scheme, value)))
    (schemes #::: values).distinct.filterNot(candidate => candidate == identifier)
  }

  /** Shrinks a calculation target towards the one carrying zero, by the magnitude it carries. */
  implicit val shrinkTestTarget: Shrink[TestTarget] = Shrink.withLazyList(testTargetCandidates)

  private def testTargetCandidates(target: TestTarget): LazyList[TestTarget] =
    simplerIntegers(target.value).map(value => TestTarget(value))

  /**
   * Shrinks a list of calculation targets by dropping its trailing target.
   *
   * The measure is the number of targets and the floor is the empty list, which the type accepts
   * and the generator draws, so shrinking is free to reach it: unlike an array of doubles, a list
   * of targets holds nothing that depends on there being an element.
   */
  implicit val shrinkCalculationTargetList: Shrink[CalculationTargetList] =
    Shrink.withLazyList(calculationTargetListCandidates)

  private def calculationTargetListCandidates(
      list: CalculationTargetList): LazyList[CalculationTargetList] =

    if (list.targets.isEmpty) LazyList.empty
    else LazyList(CalculationTargetList.of(list.targets.init))

  //-------------------------------------------------------------------------
  // The shrinkings of the currency package.
  //-------------------------------------------------------------------------
  implicit val shrinkCurrency: Shrink[Currency] =
    Shrink.withLazyList(earlierMembers(Currency.values.toList))

  /**
   * Shrinks a currency pair by moving each of its currencies earlier in the family.
   *
   * The measure is the position of the base plus the position of the counter, and the floor is the
   * pair of the two first currencies of the family.
   *
   * A pair of two different currencies never shrinks to a pair of one currency twice, and a pair of
   * one currency twice never shrinks to a pair of two. The rate-bearing types need a pair of
   * distinct currencies - [[FxRate.of]] accepts a pair of identical ones only at a rate of exactly
   * one - so a candidate that collapsed the two would be minimised into an input those types
   * reject, and a candidate that split them would be a value of the other kind.
   */
  implicit val shrinkCurrencyPair: Shrink[CurrencyPair] =
    Shrink.withLazyList(currencyPairCandidates)

  private def currencyPairCandidates(pair: CurrencyPair): LazyList[CurrencyPair] = {
    val members = Currency.values.toList
    if (pair.base == pair.counter) {
      earlierMembers(members)(pair.base).map(currency => CurrencyPair.of(currency, currency))
    } else {
      val bases = earlierMembers(members)(pair.base)
        .filterNot(currency => currency == pair.counter)
        .map(currency => CurrencyPair.of(currency, pair.counter))
      val counters = earlierMembers(members)(pair.counter)
        .filterNot(currency => currency == pair.base)
        .map(currency => CurrencyPair.of(pair.base, currency))
      (bases #::: counters).distinct
    }
  }

  /**
   * Shrinks an amount towards zero and towards the first currency of the family.
   *
   * The measure is the position of the currency plus one where the amount is not already positive
   * zero. Both candidates go through [[CurrencyAmount.of]], which rejects a value that is not a
   * number and normalises a negative zero, so a candidate is an amount on exactly the terms a
   * generated one is.
   */
  implicit val shrinkCurrencyAmount: Shrink[CurrencyAmount] =
    Shrink.withLazyList(currencyAmountCandidates)

  private def currencyAmountCandidates(amount: CurrencyAmount): LazyList[CurrencyAmount] = {
    val zeroed = simplerNumbers(amount.amount)
      .flatMap(value => candidateOf(CurrencyAmount.of(amount.currency, value)))
    val currencies = earlierMembers(Currency.values.toList)(amount.currency)
      .flatMap(currency => candidateOf(CurrencyAmount.of(currency, amount.amount)))
    (zeroed #::: currencies).distinct.filterNot(candidate => candidate == amount)
  }

  /**
   * Shrinks a money value towards zero and towards the first currency of the family.
   *
   * The candidates are built by the [[Money.of]] overload taking a decimal, which rounds to the
   * minor units of the currency it is given - and zero and an already rounded amount are unchanged
   * by that rounding, so no candidate is the value it came from by a different route.
   */
  implicit val shrinkMoney: Shrink[Money] = Shrink.withLazyList(moneyCandidates)

  private def moneyCandidates(money: Money): LazyList[Money] = {
    val zeroed =
      if (money.amount.isZero) LazyList.empty else LazyList(Money.of(money.currency, Decimal.ZERO))
    val currencies = earlierMembers(Currency.values.toList)(money.currency)
      .map(currency => Money.of(currency, money.amount))
    (zeroed #::: currencies).distinct.filterNot(candidate => candidate == money)
  }

  /**
   * Shrinks a big money value as [[shrinkMoney]] does, differing only in the scale it rounds to:
   * twelve places for every currency rather than the currency's own.
   */
  implicit val shrinkBigMoney: Shrink[BigMoney] = Shrink.withLazyList(bigMoneyCandidates)

  private def bigMoneyCandidates(money: BigMoney): LazyList[BigMoney] = {
    val zeroed =
      if (money.amount.isZero) LazyList.empty
      else LazyList(BigMoney.of(money.currency, Decimal.ZERO))
    val currencies = earlierMembers(Currency.values.toList)(money.currency)
      .map(currency => BigMoney.of(currency, money.amount))
    (zeroed #::: currencies).distinct.filterNot(candidate => candidate == money)
  }

  /**
   * Shrinks a multi-currency amount by dropping a currency and by zeroing an amount.
   *
   * The measure is the number of currencies held plus the number of amounts that are not already
   * positive zero, and the floor is the empty amount - the identity of the type's `Monoid`, which
   * the generator draws, so shrinking may reach it. Every candidate is built through the map
   * overload of [[MultiCurrencyAmount.of]] rather than around it, so any refusal it makes drops
   * the candidate; the duplicate-currency refusal cannot fire on one, since a map keyed by
   * currency holds no currency twice.
   */
  implicit val shrinkMultiCurrencyAmount: Shrink[MultiCurrencyAmount] =
    Shrink.withLazyList(multiCurrencyAmountCandidates)

  private def multiCurrencyAmountCandidates(
      amount: MultiCurrencyAmount): LazyList[MultiCurrencyAmount] = {

    val entries = amount.amounts
    val fewerCurrencies = LazyList
      .from(entries.keys)
      .flatMap(currency => candidateOf(MultiCurrencyAmount.of(entries - currency)))
    val zeroed = LazyList
      .from(entries.toList)
      .filterNot { case (_, value) => isSimplifiedNumber(value) }
      .flatMap { case (currency, _) =>
        candidateOf(MultiCurrencyAmount.of(entries.updated(currency, 0.0d)))
      }
    (fewerCurrencies #::: zeroed).distinct.filterNot(candidate => candidate == amount)
  }

  private def zeroedElement(values: DoubleArray, index: Int): DoubleArray =
    DoubleArray.tabulate(values.size)(position =>
      if (position == index) 0.0d else values.get(position))

  /**
   * Shrinks a run of amounts by shortening it, by zeroing an element and by moving its currency.
   *
   * The measure is the length of the run plus the number of elements that are not already positive
   * zero plus the position of the currency in the family, and the floor is the empty run of the
   * first currency. An empty run is reachable here, unlike in the array shrinking of
   * `strata-collect`, because this factory takes any array and the generator draws a run of length
   * zero: no operation of the type is undefined on one.
   */
  implicit val shrinkCurrencyAmountArray: Shrink[CurrencyAmountArray] =
    Shrink.withLazyList(currencyAmountArrayCandidates)

  private def currencyAmountArrayCandidates(
      run: CurrencyAmountArray): LazyList[CurrencyAmountArray] = {

    val shorter =
      if (run.size > 0) {
        LazyList(CurrencyAmountArray.of(run.currency, run.values.subArray(0, run.size - 1)))
      } else {
        LazyList.empty
      }
    val zeroed = LazyList
      .range(0, run.size)
      .filterNot(index => isSimplifiedNumber(run.values.get(index)))
      .map(index => CurrencyAmountArray.of(run.currency, zeroedElement(run.values, index)))
    val currencies = earlierMembers(Currency.values.toList)(run.currency)
      .map(currency => CurrencyAmountArray.of(currency, run.values))
    (shorter #::: zeroed #::: currencies).distinct.filterNot(candidate => candidate == run)
  }

  /**
   * Shrinks a run of multi-currency amounts by dropping a currency, shortening every run in
   * lockstep, and zeroing an element.
   *
   * The measure is the number of currencies plus the length of the runs plus the number of elements
   * that are not already positive zero. The floor is one currency holding one zero element: the
   * factory requires every run to be the same length and the generator promises at least one of
   * each, so a candidate that dropped the last currency or emptied the runs would be minimised into
   * a value the element-wise operations of the type were never given. Every run is shortened
   * together for that same reason.
   */
  implicit val shrinkMultiCurrencyAmountArray: Shrink[MultiCurrencyAmountArray] =
    Shrink.withLazyList(multiCurrencyAmountArrayCandidates)

  private def multiCurrencyAmountArrayCandidates(
      run: MultiCurrencyAmountArray): LazyList[MultiCurrencyAmountArray] = {

    val entries = run.values
    val fewerCurrencies =
      if (entries.sizeIs > 1) {
        LazyList
          .from(entries.keys)
          .flatMap(currency => candidateOf(MultiCurrencyAmountArray.of(entries - currency)))
      } else {
        LazyList.empty
      }
    val shorter =
      if (run.size > 1) {
        candidateOf(
          MultiCurrencyAmountArray.of(entries.unsorted.map { case (currency, values) =>
            (currency, values.subArray(0, run.size - 1))
          }))
      } else {
        LazyList.empty
      }
    val zeroed = LazyList
      .from(entries.toList)
      .flatMap { case (currency, values) =>
        LazyList
          .range(0, values.size)
          .filterNot(index => isSimplifiedNumber(values.get(index)))
          .flatMap(index =>
            candidateOf(
              MultiCurrencyAmountArray.of(entries.updated(currency, zeroedElement(values, index)))))
      }
    (fewerCurrencies #::: shorter #::: zeroed).distinct.filterNot(candidate => candidate == run)
  }

  /**
   * Shrinks a payment by simplifying its amount and its date.
   *
   * The measure is the measure of the amount plus one where the date is not already the first date
   * of the generated window.
   */
  implicit val shrinkPayment: Shrink[Payment] = Shrink.withLazyList(paymentCandidates)

  private def paymentCandidates(payment: Payment): LazyList[Payment] = {
    val amounts = currencyAmountCandidates(payment.value)
      .map(amount => Payment.of(amount, payment.date))
    val dates = simplerDates(payment.date).map(date => Payment.of(payment.value, date))
    (amounts #::: dates).distinct.filterNot(candidate => candidate == payment)
  }

  /**
   * Shrinks an exchange rate towards a rate of one and towards the earlier currencies.
   *
   * The measure is one where the rate is not already exactly one, plus the measure of the pair. The
   * floor is one rather than zero because the factory refuses a non-positive rate, and a rate of
   * one is the only rate an identical pair may carry, so this floor is a value both kinds of
   * generated rate can reach.
   */
  implicit val shrinkFxRate: Shrink[FxRate] = Shrink.withLazyList(fxRateCandidates)

  private def fxRateCandidates(rate: FxRate): LazyList[FxRate] = {
    val unitRate =
      if (java.lang.Double.compare(rate.rate, 1.0d) == 0) LazyList.empty
      else candidateOf(FxRate.of(rate.pair, 1.0d))
    val pairs = currencyPairCandidates(rate.pair)
      .flatMap(pair => candidateOf(FxRate.of(pair, rate.rate)))
    (unitRate #::: pairs).distinct.filterNot(candidate => candidate == rate)
  }

  private def zeroedEntry(rates: DoubleMatrix, row: Int, column: Int): DoubleMatrix =
    DoubleMatrix.tabulate(rates.rowCount, rates.columnCount)((atRow, atColumn) =>
      if (atRow == row && atColumn == column) 0.0d else rates.get(atRow, atColumn))

  private def leadingSubmatrix(rates: DoubleMatrix, size: Int): DoubleMatrix =
    DoubleMatrix.tabulate(size, size)((row, column) => rates.get(row, column))

  /**
   * Shrinks a matrix of rates by dropping its last currency and by zeroing an off-diagonal rate.
   *
   * The measure is the number of currencies plus the number of off-diagonal entries that are not
   * already positive zero, and the floor is the empty matrix, which the type holds and the
   * generators draw. Dropping the last currency takes the leading square submatrix, so the diagonal
   * stays a unit diagonal and the currencies stay distinct; zeroing touches off-diagonal entries
   * only, since a rate of a currency against itself has to be one, while a rate of zero elsewhere
   * is admitted - its reciprocal being the infinity the type also holds.
   *
   * Every candidate is built by [[FxMatrix.fromMatrix]], so the three structural checks are applied
   * to it rather than assumed.
   */
  implicit val shrinkFxMatrix: Shrink[FxMatrix] = Shrink.withLazyList(fxMatrixCandidates)

  private def fxMatrixCandidates(matrix: FxMatrix): LazyList[FxMatrix] = {
    val size = matrix.currencies.size
    val smaller =
      if (size > 0) {
        candidateOf(
          FxMatrix.fromMatrix(matrix.currencies.init, leadingSubmatrix(matrix.rates, size - 1)))
      } else {
        LazyList.empty
      }
    val zeroed = LazyList
      .range(0, size)
      .flatMap(row => LazyList.range(0, size).map(column => (row, column)))
      .filter { case (row, column) =>
        row != column && !isSimplifiedNumber(matrix.rates.get(row, column))
      }
      .flatMap { case (row, column) =>
        candidateOf(
          FxMatrix.fromMatrix(matrix.currencies, zeroedEntry(matrix.rates, row, column)))
      }
    (smaller #::: zeroed).distinct.filterNot(candidate => candidate == matrix)
  }

  //-------------------------------------------------------------------------
  // The shrinkings of the date package.
  //-------------------------------------------------------------------------
  implicit val shrinkBusinessDayConvention: Shrink[BusinessDayConvention] =
    Shrink.withLazyList(earlierMembers(BusinessDayConvention.values.toList))

  /**
   * Shrinks a holiday calendar identifier within the kind of identifier it is.
   *
   * None of the three kinds the generator draws shrinks into another, because the kind is what an
   * identifier is for: a built-in identifier is one the standard reference data resolves, a custom
   * one is deliberately not, and a composite one is the form whose name carries a separator.
   *
   *   - a built-in identifier shrinks to the built-in identifiers declared before it, so it stays
   *     resolvable;
   *   - a composite identifier - one whose name holds either separator - shrinks to the two
   *     identifiers it composes, which is the reduction that tells a reader which half of a
   *     combination a failure needs;
   *   - any other identifier, which is the custom kind, shrinks by cutting its name to its first
   *     character. The generated custom names begin with `X`, so the floor keeps that leading
   *     character and stays a name no built-in calendar holds and no separator appears in.
   *
   * The measure is the pair of the number of names composed, two for a composite and one otherwise,
   * and then, within the kind, the position among the built-in identifiers or the length of the
   * name. The pair is ordered lexicographically, which is well-founded on two non-negative whole
   * numbers.
   */
  implicit val shrinkHolidayCalendarId: Shrink[HolidayCalendarId] =
    Shrink.withLazyList(holidayCalendarIdCandidates)

  /** The separators the two composition forms of an identifier are written with. */
  private val CalendarIdSeparators: Set[Char] = Set('+', '~')

  private def holidayCalendarIdCandidates(id: HolidayCalendarId): LazyList[HolidayCalendarId] = {
    val name = id.name
    val separator = name.indexWhere(character => CalendarIdSeparators.contains(character))
    val candidates =
      if (builtInCalendarIds.contains(id)) {
        earlierMembers(builtInCalendarIds)(id)
      } else if (separator > 0) {
        LazyList(
          HolidayCalendarId.of(name.take(separator)),
          HolidayCalendarId.of(name.drop(separator + 1)))
      } else {
        shorterTexts(name).map(text => HolidayCalendarId.of(text))
      }
    candidates.distinct.filterNot(candidate => candidate == id)
  }

  /**
   * Shrinks a calendar-bearing day count to the day counts of the earlier built-in calendars.
   *
   * The candidates are drawn from the built-in set alone, which is the set
   * [[genCalendarBearingDayCount]] draws from, so a candidate stays a day count whose calendar the
   * name form of its document reads back.
   */
  implicit val shrinkCalendarBearingDayCount: Shrink[DayCount.Bus252] =
    Shrink.withLazyList(calendarBearingDayCountCandidates)

  private def calendarBearingDayCountCandidates(
      dayCount: DayCount.Bus252): LazyList[DayCount.Bus252] =

    earlierMembers(builtInCalendars)(dayCount.calendar)
      .flatMap(calendar =>
        DayCount.ofBus252(calendar) match {
          case bus252: DayCount.Bus252 => LazyList(bus252)
          case _ => LazyList.empty
        })
      .filterNot(candidate => candidate == dayCount)

  /**
   * Shrinks a day count within its kind: a standard member to an earlier standard member, and a
   * calendar-bearing one to the day count of an earlier built-in calendar.
   *
   * A calendar-bearing day count never shrinks to a standard member. The two kinds differ in the
   * document form they take - a name for a standard member, an object naming a calendar for a
   * calendar-bearing one - and in the data they carry, so a candidate of the other kind would
   * minimise a failure of the calendar-bearing form into a value of the form that never had it.
   */
  implicit val shrinkDayCount: Shrink[DayCount] = Shrink.withLazyList(dayCountCandidates)

  private def dayCountCandidates(dayCount: DayCount): LazyList[DayCount] =
    dayCount match {
      case bus252: DayCount.Bus252 =>
        calendarBearingDayCountCandidates(bus252).map(candidate => candidate: DayCount)
      case standard => earlierMembers(DayCount.values.toList)(standard)
    }

  /**
   * Shrinks a custom calendar by dropping its working days and then its last holiday.
   *
   * The working days go first, and a holiday is dropped only once none is left, because a working
   * day is ignored outside the range of years the holidays span: shortening the holidays first
   * could drop a working day silently and leave a candidate that is not the value the reduction
   * described. The last holiday is dropped rather than the first for that same reason - the first
   * holiday is where the range of the calendar starts.
   *
   * The measure is the number of holidays plus the number of working days, and the floor is a
   * calendar of one holiday and no working days. A calendar of the built-in set has no candidates
   * at all: its document form is its name, so a calendar holding a built-in identifier with
   * different holidays would not read back as itself.
   *
   * The candidates are not compared with the value they came from, as every other shrinking here
   * compares them: the `equals` of this type compares the identifier alone, so every candidate is
   * equal to its input and filtering by equality would drop all of them. The measure is what
   * establishes that a candidate is a reduction, and it falls by construction.
   */
  implicit val shrinkImmutableHolidayCalendar: Shrink[ImmutableHolidayCalendar] =
    Shrink.withLazyList(immutableHolidayCalendarCandidates)

  private def immutableHolidayCalendarCandidates(
      calendar: ImmutableHolidayCalendar): LazyList[ImmutableHolidayCalendar] = {

    val holidays = calendar.holidays.toList
    val workingDays = calendar.workingDays.toList
    if (builtInCalendarIds.contains(calendar.id)) {
      LazyList.empty
    } else if (workingDays.nonEmpty) {
      LazyList(
        ImmutableHolidayCalendar.of(calendar.id, holidays, calendar.weekendDays, List.empty))
    } else if (holidays.sizeIs > 1) {
      LazyList(
        ImmutableHolidayCalendar.of(calendar.id, holidays.init, calendar.weekendDays, List.empty))
    } else {
      LazyList.empty
    }
  }

  /**
   * Shrinks a holiday calendar within the shape it has.
   *
   *   - a composite - a combination or a link - shrinks to the two calendars it composes, which is
   *     the reduction saying which half of the composition a failure needs;
   *   - a built-in calendar shrinks to the built-in calendars declared before it, so it stays a
   *     calendar carried in a document as its name;
   *   - a custom calendar shrinks as [[shrinkImmutableHolidayCalendar]] describes, keeping its own
   *     identifier so its document form stays the structural one.
   *
   * The measure is defined over all four shapes at once: one plus the measures of both halves for a
   * composite, the number of holidays plus working days for a custom calendar, and the position
   * among the built-in calendars for a built-in one. A composite is strictly larger than either
   * half because both measures are non-negative. A calendar of none of these shapes offers no
   * candidate rather than being reduced by a rule written for another shape.
   */
  implicit val shrinkHolidayCalendar: Shrink[HolidayCalendar] =
    Shrink.withLazyList(holidayCalendarCandidates)

  private def holidayCalendarCandidates(calendar: HolidayCalendar): LazyList[HolidayCalendar] =
    if (builtInCalendars.contains(calendar)) {
      earlierMembers(builtInCalendars)(calendar)
    } else {
      calendar match {
        case HolidayCalendar.Combined(first, second) => LazyList(first, second)
        case HolidayCalendar.Linked(first, second) => LazyList(first, second)
        case custom: ImmutableHolidayCalendar =>
          immutableHolidayCalendarCandidates(custom).map(candidate => candidate: HolidayCalendar)
        case _ => LazyList.empty
      }
    }

  /**
   * Shrinks a business day adjustment towards the first convention over the first calendar.
   *
   * The measure is the position of the convention in its family plus the position of the calendar
   * among the built-in identifiers. The calendar is moved within the built-in set rather than
   * shortened, because the generator promises an adjustment that can be applied against the
   * standard reference data and a name outside that set resolves to nothing; an adjustment naming a
   * calendar outside it keeps its calendar and shrinks its convention alone.
   */
  implicit val shrinkBusinessDayAdjustment: Shrink[BusinessDayAdjustment] =
    Shrink.withLazyList(businessDayAdjustmentCandidates)

  private def businessDayAdjustmentCandidates(
      adjustment: BusinessDayAdjustment): LazyList[BusinessDayAdjustment] = {

    val conventions = earlierMembers(BusinessDayConvention.values.toList)(adjustment.convention)
      .map(convention => BusinessDayAdjustment.of(convention, adjustment.calendar))
    val calendars =
      if (builtInCalendarIds.contains(adjustment.calendar)) {
        earlierMembers(builtInCalendarIds)(adjustment.calendar)
          .map(calendar => BusinessDayAdjustment.of(adjustment.convention, calendar))
      } else {
        LazyList.empty
      }
    (conventions #::: calendars).distinct.filterNot(candidate => candidate == adjustment)
  }

  /**
   * Shrinks an adjustable date towards the first date of the generated window.
   *
   * The measure is one where the date is not already that first date, plus the measure of the
   * adjustment.
   */
  implicit val shrinkAdjustableDate: Shrink[AdjustableDate] =
    Shrink.withLazyList(adjustableDateCandidates)

  private def adjustableDateCandidates(date: AdjustableDate): LazyList[AdjustableDate] = {
    val dates = simplerDates(date.unadjusted).map(day => AdjustableDate.of(day, date.adjustment))
    val adjustments = businessDayAdjustmentCandidates(date.adjustment)
      .map(adjustment => AdjustableDate.of(date.unadjusted, adjustment))
    (dates #::: adjustments).distinct.filterNot(candidate => candidate == date)
  }

  /**
   * Shrinks a run of adjustable dates by dropping its last date and by moving the run earlier.
   *
   * The dates keep the order and the gaps the factory requires: dropping the last of a strictly
   * increasing run leaves it strictly increasing, and translating every date by the same number of
   * days moves the run without changing a single gap. The translation is offered only where the
   * first date is after the start of the window, so no candidate walks off the calendar.
   *
   * The measure is the number of dates plus one where the first date is not already the start of
   * the window plus the measure of the adjustment.
   */
  implicit val shrinkAdjustableDates: Shrink[AdjustableDates] =
    Shrink.withLazyList(adjustableDatesCandidates)

  private def adjustableDatesCandidates(run: AdjustableDates): LazyList[AdjustableDates] = {
    val dates = run.unadjusted
    val shorter =
      if (dates.tail.nonEmpty) {
        candidateOf(
          AdjustableDates.of(run.adjustment, NonEmptyList(dates.head, dates.tail.init)))
      } else {
        LazyList.empty
      }
    val moved =
      if (dates.head.isAfter(FirstDate)) {
        val offset = dates.head.toEpochDay - FirstDate.toEpochDay
        candidateOf(
          AdjustableDates.of(run.adjustment, dates.map(date => date.minusDays(offset))))
      } else {
        LazyList.empty
      }
    val adjustments = businessDayAdjustmentCandidates(run.adjustment)
      .flatMap(adjustment => candidateOf(AdjustableDates.of(adjustment, dates)))
    (shorter #::: moved #::: adjustments).distinct.filterNot(candidate => candidate == run)
  }

  /** The identifier naming no calendar, which an addition of calendar days carries. */
  private val NoHolidaysCalendarId: HolidayCalendarId = StandardHolidayCalendars.NO_HOLIDAYS.id

  /**
   * Shrinks a days adjustment towards no days at all under a simpler trailing adjustment.
   *
   * An addition of business days stays one and an addition of calendar days stays one. The two are
   * rebuilt through their own factories, so a candidate holds the addition calendar its input held
   * and walks the days the same way; a candidate of the other kind would minimise a failure of the
   * business-day walk into a value that never walked one.
   *
   * The measure is the magnitude of the day count plus the measure of the trailing adjustment.
   */
  implicit val shrinkDaysAdjustment: Shrink[DaysAdjustment] =
    Shrink.withLazyList(daysAdjustmentCandidates)

  private def daysAdjustmentCandidates(adjustment: DaysAdjustment): LazyList[DaysAdjustment] = {
    def rebuild(days: Int, trailing: BusinessDayAdjustment): DaysAdjustment =
      if (adjustment.calendar == NoHolidaysCalendarId) {
        DaysAdjustment.ofCalendarDays(days, trailing)
      } else {
        DaysAdjustment.ofBusinessDays(days, adjustment.calendar, trailing)
      }
    val fewerDays = simplerIntegers(adjustment.days).map(days => rebuild(days, adjustment.adjustment))
    val trailing = businessDayAdjustmentCandidates(adjustment.adjustment)
      .map(candidate => rebuild(adjustment.days, candidate))
    (fewerDays #::: trailing).distinct.filterNot(candidate => candidate == adjustment)
  }

  implicit val shrinkPeriodAdditionConvention: Shrink[PeriodAdditionConvention] =
    Shrink.withLazyList(earlierMembers(PeriodAdditionConvention.values.toList))

  /**
   * Shrinks a period adjustment by reducing its period, its convention and its adjustment.
   *
   * The period is reduced within the unit it is stated in, which keeps a month-based convention
   * paired with a month-based period by construction rather than by a check. A candidate moving the
   * convention earlier goes through [[PeriodAdjustment.of]] all the same, so the one reduction that
   * can break the pairing - a month-based convention over a period holding days - is rejected there
   * and contributes no candidate.
   */
  implicit val shrinkPeriodAdjustment: Shrink[PeriodAdjustment] =
    Shrink.withLazyList(periodAdjustmentCandidates)

  private def periodAdjustmentCandidates(
      adjustment: PeriodAdjustment): LazyList[PeriodAdjustment] = {

    val periods = simplerPeriods(adjustment.period)
      .flatMap(period =>
        candidateOf(
          PeriodAdjustment.of(period, adjustment.additionConvention, adjustment.adjustment)))
    val conventions =
      earlierMembers(PeriodAdditionConvention.values.toList)(adjustment.additionConvention)
        .flatMap(convention =>
          candidateOf(
            PeriodAdjustment.of(adjustment.period, convention, adjustment.adjustment)))
    val trailing = businessDayAdjustmentCandidates(adjustment.adjustment)
      .flatMap(candidate =>
        candidateOf(
          PeriodAdjustment.of(adjustment.period, adjustment.additionConvention, candidate)))
    (periods #::: conventions #::: trailing).distinct.filterNot(candidate => candidate == adjustment)
  }

  /**
   * Shrinks a tenor by reducing its period in the unit the tenor is stated in.
   *
   * The floor is one unit of that kind - one day, one week, one month or one year - because
   * [[Tenor.of]] rejects a period of zero and the canonical name of a tenor is decided by the unit
   * it holds.
   */
  implicit val shrinkTenor: Shrink[Tenor] = Shrink.withLazyList(tenorCandidates)

  private def tenorCandidates(tenor: Tenor): LazyList[Tenor] =
    simplerPeriods(tenor.period)
      .flatMap(period => candidateOf(Tenor.of(period)))
      .distinct
      .filterNot(candidate => candidate == tenor)

  /**
   * Shrinks a tenor adjustment by reducing its tenor, its convention and its adjustment.
   *
   * The measure, the floor and the pairing rule are those of [[shrinkPeriodAdjustment]], over a
   * tenor rather than a period.
   */
  implicit val shrinkTenorAdjustment: Shrink[TenorAdjustment] =
    Shrink.withLazyList(tenorAdjustmentCandidates)

  private def tenorAdjustmentCandidates(
      adjustment: TenorAdjustment): LazyList[TenorAdjustment] = {

    val tenors = tenorCandidates(adjustment.tenor)
      .flatMap(tenor =>
        candidateOf(
          TenorAdjustment.of(tenor, adjustment.additionConvention, adjustment.adjustment)))
    val conventions =
      earlierMembers(PeriodAdditionConvention.values.toList)(adjustment.additionConvention)
        .flatMap(convention =>
          candidateOf(TenorAdjustment.of(adjustment.tenor, convention, adjustment.adjustment)))
    val trailing = businessDayAdjustmentCandidates(adjustment.adjustment)
      .flatMap(candidate =>
        candidateOf(
          TenorAdjustment.of(adjustment.tenor, adjustment.additionConvention, candidate)))
    (tenors #::: conventions #::: trailing).distinct.filterNot(candidate => candidate == adjustment)
  }

  /** The four market tenors whose codes are not the code of a tenor, in declaration order. */
  private val MarketTenorConstants: List[MarketTenor] =
    List(MarketTenor.ON, MarketTenor.TN, MarketTenor.SN, MarketTenor.SW)

  /**
   * Shrinks a market tenor towards the overnight tenor.
   *
   * One of the four constants shrinks to the constants declared before it; any other market tenor
   * is spot-starting, and shrinks by reducing the tenor it starts from through
   * [[MarketTenor.ofSpot]]. That factory answers with spot-next for a tenor of one day and with
   * spot-week for a tenor of one week, so a spot-starting tenor may shrink into one of the
   * constants - which is no change of kind at all, those two constants being exactly what a
   * spot-starting tenor of that length is.
   *
   * The measure is the position among the four constants for a constant, and four plus the number
   * of units its tenor holds for any other, so the measure falls at every step.
   */
  implicit val shrinkMarketTenor: Shrink[MarketTenor] =
    Shrink.withLazyList(marketTenorCandidates)

  private def marketTenorCandidates(marketTenor: MarketTenor): LazyList[MarketTenor] = {
    val candidates =
      if (MarketTenorConstants.contains(marketTenor)) {
        earlierMembers(MarketTenorConstants)(marketTenor)
      } else {
        tenorCandidates(marketTenor.tenor).flatMap(tenor => candidateOf(MarketTenor.ofSpot(tenor)))
      }
    candidates.distinct.filterNot(candidate => candidate == marketTenor)
  }

  implicit val shrinkDateSequence: Shrink[DateSequence] =
    Shrink.withLazyList(earlierMembers(DateSequence.values.toList))

  /**
   * Shrinks a sequence date instruction towards the first date of the base sequence.
   *
   * The two starting points are never introduced, only removed, which keeps a candidate inside the
   * rule the factory enforces - a month and a minimum period are mutually exclusive - rather than
   * relying on the factory to reject a candidate that named both.
   *
   * The measure is the number of starting points named, plus the sequence number less one, plus one
   * where the month is not already the first of the window, plus one where the full sequence is
   * counted over.
   */
  implicit val shrinkSequenceDate: Shrink[SequenceDate] =
    Shrink.withLazyList(sequenceDateCandidates)

  private def sequenceDateCandidates(date: SequenceDate): LazyList[SequenceDate] = {
    val earlierMonths = LazyList
      .from(date.yearMonth.toList)
      .flatMap(month => simplerMonths(month))
      .flatMap(month =>
        candidateOf(
          SequenceDate.of(Some(month), None, date.sequenceNumber, date.fullSequence)))
    val withoutStartingPoint =
      if (date.yearMonth.isDefined || date.minimumPeriod.isDefined) {
        candidateOf(SequenceDate.of(None, None, date.sequenceNumber, date.fullSequence))
      } else {
        LazyList.empty
      }
    val positions = simplerPositions(date.sequenceNumber)
      .flatMap(position =>
        candidateOf(
          SequenceDate.of(date.yearMonth, date.minimumPeriod, position, date.fullSequence)))
    val baseSequence =
      if (date.fullSequence) {
        candidateOf(
          SequenceDate.of(
            date.yearMonth,
            date.minimumPeriod,
            date.sequenceNumber,
            fullSequence = false))
      } else {
        LazyList.empty
      }
    (earlierMonths #::: withoutStartingPoint #::: positions #::: baseSequence).distinct
      .filterNot(candidate => candidate == date)
  }

  //-------------------------------------------------------------------------
  // The shrinkings of the index package.
  //-------------------------------------------------------------------------
  implicit val shrinkIborIndex: Shrink[IborIndex] =
    Shrink.withLazyList(earlierMembers(IborIndex.values.toList))

  implicit val shrinkOvernightIndex: Shrink[OvernightIndex] =
    Shrink.withLazyList(earlierMembers(OvernightIndex.values.toList))

  implicit val shrinkPriceIndex: Shrink[PriceIndex] =
    Shrink.withLazyList(earlierMembers(PriceIndex.values.toList))

  implicit val shrinkFxIndex: Shrink[FxIndex] =
    Shrink.withLazyList(earlierMembers(FxIndex.values.toList))

  implicit val shrinkFloatingRateType: Shrink[FloatingRateType] =
    Shrink.withLazyList(earlierMembers(FloatingRateType.values.toList))

  implicit val shrinkFloatingRateName: Shrink[FloatingRateName] =
    Shrink.withLazyList(earlierMembers(FloatingRateName.values.toList))

  /**
   * Shrinks an observation of an Ibor index fixing towards the start of the generated window.
   *
   * An observation is derived rather than given - its effective date, maturity date and year
   * fraction come from the index and the fixing date - so both candidates go back through
   * [[IborIndexObservation.of]] against the standard reference data, which is what the generator
   * does. A candidate assembled field by field could hold dates the index does not derive, and no
   * reduction of it would then mean anything.
   *
   * The index is moved within the observable indices alone - the ones whose calendars the standard
   * data resolves - so a candidate is an observation that can be built rather than one whose
   * calendars are missing.
   */
  implicit val shrinkIborIndexObservation: Shrink[IborIndexObservation] =
    Shrink.withLazyList(iborIndexObservationCandidates)

  private def iborIndexObservationCandidates(
      observation: IborIndexObservation): LazyList[IborIndexObservation] = {

    val dates = simplerDates(observation.fixingDate)
      .flatMap(date =>
        candidateOf(IborIndexObservation.of(observation.index, date, ReferenceData.standard)))
    val indices = earlierMembers(observableIborIndices)(observation.index)
      .flatMap(index =>
        candidateOf(
          IborIndexObservation.of(index, observation.fixingDate, ReferenceData.standard)))
    (dates #::: indices).distinct.filterNot(candidate => candidate == observation)
  }

  /**
   * Shrinks an observation of an overnight index fixing towards the start of the window.
   *
   * As [[shrinkIborIndexObservation]]; this observation derives a publication date as well, which
   * the factory supplies for the same reason.
   */
  implicit val shrinkOvernightIndexObservation: Shrink[OvernightIndexObservation] =
    Shrink.withLazyList(overnightIndexObservationCandidates)

  private def overnightIndexObservationCandidates(
      observation: OvernightIndexObservation): LazyList[OvernightIndexObservation] = {

    val dates = simplerDates(observation.fixingDate)
      .flatMap(date =>
        candidateOf(OvernightIndexObservation.of(observation.index, date, ReferenceData.standard)))
    val indices = earlierMembers(observableOvernightIndices)(observation.index)
      .flatMap(index =>
        candidateOf(
          OvernightIndexObservation.of(index, observation.fixingDate, ReferenceData.standard)))
    (dates #::: indices).distinct.filterNot(candidate => candidate == observation)
  }

  /** Shrinks an observation of an FX index fixing as [[shrinkIborIndexObservation]] does. */
  implicit val shrinkFxIndexObservation: Shrink[FxIndexObservation] =
    Shrink.withLazyList(fxIndexObservationCandidates)

  private def fxIndexObservationCandidates(
      observation: FxIndexObservation): LazyList[FxIndexObservation] = {

    val dates = simplerDates(observation.fixingDate)
      .flatMap(date =>
        candidateOf(FxIndexObservation.of(observation.index, date, ReferenceData.standard)))
    val indices = earlierMembers(observableFxIndices)(observation.index)
      .flatMap(index =>
        candidateOf(FxIndexObservation.of(index, observation.fixingDate, ReferenceData.standard)))
    (dates #::: indices).distinct.filterNot(candidate => candidate == observation)
  }

  /**
   * Shrinks an observation of a price index fixing towards the first month of the window.
   *
   * This one needs no reference data and derives nothing: a price index is fixed for a month, so
   * the observation holds exactly the index and the month it is given.
   */
  implicit val shrinkPriceIndexObservation: Shrink[PriceIndexObservation] =
    Shrink.withLazyList(priceIndexObservationCandidates)

  private def priceIndexObservationCandidates(
      observation: PriceIndexObservation): LazyList[PriceIndexObservation] = {

    val months = simplerMonths(observation.fixingMonth)
      .map(month => PriceIndexObservation.of(observation.index, month))
    val indices = earlierMembers(PriceIndex.values.toList)(observation.index)
      .map(index => PriceIndexObservation.of(index, observation.fixingMonth))
    (months #::: indices).distinct.filterNot(candidate => candidate == observation)
  }

  //-------------------------------------------------------------------------
  // The shrinkings of the location package.
  //-------------------------------------------------------------------------
  /**
   * Shrinks a country towards the first country a constant names, or towards the code `AA`.
   *
   * The type is an open value rather than a closed family, so the two kinds the generator draws
   * shrink in two ways: a country a constant names shrinks to the countries the constants declare
   * before it, and any other code shrinks by moving one of its two letters to `A`. A code may
   * shrink into a country a constant names, which is no change of kind - the constants are codes
   * like any other - and the measure accounts for it: the position among the named countries for a
   * named one, and the number of the named countries plus the distance of the two letters from `A`
   * for any other.
   */
  implicit val shrinkCountry: Shrink[Country] = Shrink.withLazyList(countryCandidates)

  private def countryCandidates(country: Country): LazyList[Country] = {
    val candidates =
      if (namedCountries.contains(country)) {
        earlierMembers(namedCountries)(country)
      } else {
        val code = country.code
        LazyList(s"A${code.drop(1)}", s"${code.take(1)}A")
          .filterNot(candidate => candidate == code)
          .flatMap(candidate => candidateOf(Country.of(candidate)))
      }
    candidates.distinct.filterNot(candidate => candidate == country)
  }

  //-------------------------------------------------------------------------
  // The shrinkings of the schedule package.
  //-------------------------------------------------------------------------
  /**
   * Shrinks a frequency by reducing its length in the unit it is stated in.
   *
   * The floor is one unit of that kind - one day, one week, one month or one year - because
   * [[Frequency.of]] rejects a length of zero and the canonical name of a frequency is decided by
   * the unit it is stated in.
   *
   * The term frequency is its own floor and shrinks to nothing: it is the frequency of a single
   * period covering the whole schedule, its name is a word rather than a period, and a schedule at
   * a regular frequency is a different shape of value entirely.
   */
  implicit val shrinkFrequency: Shrink[Frequency] = Shrink.withLazyList(frequencyCandidates)

  private def frequencyCandidates(frequency: Frequency): LazyList[Frequency] =
    if (frequency == Frequency.TERM) {
      LazyList.empty
    } else {
      simplerPeriods(frequency.period)
        .flatMap(period => candidateOf(Frequency.of(period)))
        .distinct
        .filterNot(candidate => candidate == frequency)
    }

  implicit val shrinkStubConvention: Shrink[StubConvention] =
    Shrink.withLazyList(earlierMembers(StubConvention.values.toList))

  /**
   * Shrinks a roll convention to the conventions declared before it, in the one order the family
   * publishes: the standard conventions, then the day-of-month ones, then the day-of-week ones.
   */
  implicit val shrinkRollConvention: Shrink[RollConvention] =
    Shrink.withLazyList(earlierMembers(RollConvention.values.toList))

  /** The shortest unadjusted length a generated schedule period holds, in days. */
  private val MinimumPeriodLength: Long = 10L

  /**
   * Shrinks a schedule period by removing its shifts, shortening it and moving it to the window.
   *
   * Both pairs of dates stay in the order the factory checks. Removing the shifts leaves the
   * adjusted pair equal to the unadjusted pair, which is ordered because the unadjusted pair is;
   * translating moves all four dates by the same number of days, which changes no gap at all; and
   * cutting the length keeps the shifts, which the generator draws within three days of a length of
   * at least ten. A cut that did cross the dates is rejected by [[SchedulePeriod.of]] and
   * contributes no candidate.
   *
   * The measure is the number of adjusted dates that differ from their unadjusted date, plus the
   * unadjusted length in days beyond the shortest generated one, plus one where the unadjusted
   * start is not the first date of the window.
   */
  implicit val shrinkSchedulePeriod: Shrink[SchedulePeriod] =
    Shrink.withLazyList(schedulePeriodCandidates)

  private def schedulePeriodCandidates(period: SchedulePeriod): LazyList[SchedulePeriod] = {
    val startShift = period.startDate.toEpochDay - period.unadjustedStartDate.toEpochDay
    val endShift = period.endDate.toEpochDay - period.unadjustedEndDate.toEpochDay
    val length = period.unadjustedEndDate.toEpochDay - period.unadjustedStartDate.toEpochDay
    val unshifted =
      if (startShift == 0L && endShift == 0L) {
        LazyList.empty
      } else {
        candidateOf(
          SchedulePeriod.of(
            period.unadjustedStartDate,
            period.unadjustedEndDate,
            period.unadjustedStartDate,
            period.unadjustedEndDate))
      }
    val shorter =
      if (length > MinimumPeriodLength) {
        val unadjustedEnd = period.unadjustedStartDate.plusDays(MinimumPeriodLength)
        candidateOf(
          SchedulePeriod.of(
            period.startDate,
            unadjustedEnd.plusDays(endShift),
            period.unadjustedStartDate,
            unadjustedEnd))
      } else {
        LazyList.empty
      }
    val moved =
      if (period.unadjustedStartDate.isAfter(FirstDate)) {
        val offset = period.unadjustedStartDate.toEpochDay - FirstDate.toEpochDay
        candidateOf(
          SchedulePeriod.of(
            period.startDate.minusDays(offset),
            period.endDate.minusDays(offset),
            period.unadjustedStartDate.minusDays(offset),
            period.unadjustedEndDate.minusDays(offset)))
      } else {
        LazyList.empty
      }
    (unshifted #::: shorter #::: moved).distinct.filterNot(candidate => candidate == period)
  }

  /**
   * Shrinks a schedule by dropping its last period and by moving its roll convention earlier.
   *
   * A period is never reduced in place. The periods of a generated schedule run end to end at the
   * frequency the schedule reports, and a period made shorter or moved would leave a gap between
   * itself and its neighbour - which [[Schedule.of]] allows, but which no generation produces and
   * the specs reading periods off a schedule are not written against. Dropping the last period is
   * the one reduction that keeps the run contiguous from the start date it began at, and it says
   * how few periods the failure needs; the frequency is left alone for the same reason, since it is
   * the frequency those periods run at.
   *
   * The measure is the number of periods plus the position of the roll convention.
   */
  implicit val shrinkSchedule: Shrink[Schedule] = Shrink.withLazyList(scheduleCandidates)

  private def scheduleCandidates(schedule: Schedule): LazyList[Schedule] = {
    val periods = schedule.periods
    val shorter =
      if (periods.tail.nonEmpty) {
        candidateOf(
          Schedule.of(
            NonEmptyList(periods.head, periods.tail.init),
            schedule.periodicFrequency,
            schedule.rollConvention))
      } else {
        LazyList.empty
      }
    val conventions = earlierMembers(RollConvention.values.toList)(schedule.rollConvention)
      .flatMap(convention =>
        candidateOf(Schedule.of(periods, schedule.periodicFrequency, convention)))
    (shorter #::: conventions).distinct.filterNot(candidate => candidate == schedule)
  }

  /**
   * Shrinks a periodic schedule definition by dropping what it says beyond its four required
   * fields.
   *
   * An optional field is only ever emptied, never introduced or moved, because the seven order
   * invariants the type checks hold between the date-bearing ones - a regular boundary falls on a
   * period boundary, an overriding start date falls before the start date - and emptying a field
   * can only remove a constraint, while changing one would break it. The four required fields,
   * which carry the shape of the schedule, are left exactly as they are for that same reason.
   *
   * The measure is the number of optional fields the definition names, and the floor is the
   * definition of its four required fields alone.
   */
  implicit val shrinkPeriodicSchedule: Shrink[PeriodicSchedule] =
    Shrink.withLazyList(periodicScheduleCandidates)

  private def periodicScheduleCandidates(
      schedule: PeriodicSchedule): LazyList[PeriodicSchedule] = {

    def withoutField(
        startAdjustment: Option[BusinessDayAdjustment],
        endAdjustment: Option[BusinessDayAdjustment],
        stubConvention: Option[StubConvention],
        rollConvention: Option[RollConvention],
        firstRegular: Option[LocalDate],
        lastRegular: Option[LocalDate],
        overrideStart: Option[AdjustableDate]): LazyList[PeriodicSchedule] =

      candidateOf(
        PeriodicSchedule.of(
          schedule.startDate,
          schedule.endDate,
          schedule.frequency,
          schedule.businessDayAdjustment,
          startAdjustment,
          endAdjustment,
          stubConvention,
          rollConvention,
          firstRegular,
          lastRegular,
          overrideStart))

    val candidates =
      withoutField(
        None,
        schedule.endDateBusinessDayAdjustment,
        schedule.stubConvention,
        schedule.rollConvention,
        schedule.firstRegularStartDate,
        schedule.lastRegularEndDate,
        schedule.overrideStartDate) #:::
        withoutField(
          schedule.startDateBusinessDayAdjustment,
          None,
          schedule.stubConvention,
          schedule.rollConvention,
          schedule.firstRegularStartDate,
          schedule.lastRegularEndDate,
          schedule.overrideStartDate) #:::
        withoutField(
          schedule.startDateBusinessDayAdjustment,
          schedule.endDateBusinessDayAdjustment,
          None,
          schedule.rollConvention,
          schedule.firstRegularStartDate,
          schedule.lastRegularEndDate,
          schedule.overrideStartDate) #:::
        withoutField(
          schedule.startDateBusinessDayAdjustment,
          schedule.endDateBusinessDayAdjustment,
          schedule.stubConvention,
          None,
          schedule.firstRegularStartDate,
          schedule.lastRegularEndDate,
          schedule.overrideStartDate) #:::
        withoutField(
          schedule.startDateBusinessDayAdjustment,
          schedule.endDateBusinessDayAdjustment,
          schedule.stubConvention,
          schedule.rollConvention,
          None,
          schedule.lastRegularEndDate,
          schedule.overrideStartDate) #:::
        withoutField(
          schedule.startDateBusinessDayAdjustment,
          schedule.endDateBusinessDayAdjustment,
          schedule.stubConvention,
          schedule.rollConvention,
          schedule.firstRegularStartDate,
          None,
          schedule.overrideStartDate) #:::
        withoutField(
          schedule.startDateBusinessDayAdjustment,
          schedule.endDateBusinessDayAdjustment,
          schedule.stubConvention,
          schedule.rollConvention,
          schedule.firstRegularStartDate,
          schedule.lastRegularEndDate,
          None)
    candidates.distinct.filterNot(candidate => candidate == schedule)
  }

  //-------------------------------------------------------------------------
  // The shrinkings of the value package.
  //-------------------------------------------------------------------------
  implicit val shrinkValueAdjustmentType: Shrink[ValueAdjustmentType] =
    Shrink.withLazyList(earlierMembers(ValueAdjustmentType.values.toList))

  /** Rebuilds an adjustment of the kind given, through the factory that names that kind. */
  private def rebuiltValueAdjustment(
      value: Double,
      adjustmentType: ValueAdjustmentType): ValueAdjustment =

    adjustmentType match {
      case ValueAdjustmentType.Replace => ValueAdjustment.ofReplace(value)
      case ValueAdjustmentType.DeltaAmount => ValueAdjustment.ofDeltaAmount(value)
      case ValueAdjustmentType.DeltaMultiplier => ValueAdjustment.ofDeltaMultiplier(value)
      case ValueAdjustmentType.Multiplier => ValueAdjustment.ofMultiplier(value)
    }

  /**
   * Shrinks a value adjustment towards adjusting by positive zero, keeping the kind it is.
   *
   * The kind of adjustment is never changed. Replacing a value, adding a delta, adding a proportion
   * and multiplying are four different operations, and a step sequence accepts three of them and
   * refuses the fourth, so a candidate of another kind would minimise a failure of one operation
   * into a value describing another. Each candidate is rebuilt through the factory naming its own
   * kind.
   */
  implicit val shrinkValueAdjustment: Shrink[ValueAdjustment] =
    Shrink.withLazyList(valueAdjustmentCandidates)

  private def valueAdjustmentCandidates(
      adjustment: ValueAdjustment): LazyList[ValueAdjustment] =

    simplerNumbers(adjustment.modifyingValue)
      .map(value => rebuiltValueAdjustment(value, adjustment.`type`))
      .distinct
      .filterNot(candidate => candidate == adjustment)

  /**
   * Shrinks a value with its derivatives towards zero with one zero derivative.
   *
   * The derivatives shrink as an array does in `strata-collect`: the trailing element is dropped
   * while more than one is left, and an element that is not already positive zero is simplified to
   * it. The last element is never dropped, so a value drawn with derivatives keeps one; a value
   * drawn with none - which [[genDoubleArray]] produces, since it draws the empty array - is
   * already at the floor in that field.
   */
  implicit val shrinkValueDerivatives: Shrink[ValueDerivatives] =
    Shrink.withLazyList(valueDerivativesCandidates)

  private def valueDerivativesCandidates(
      derivatives: ValueDerivatives): LazyList[ValueDerivatives] = {

    val values = simplerNumbers(derivatives.value)
      .map(value => ValueDerivatives.of(value, derivatives.derivatives))
    val shorter =
      if (derivatives.derivatives.size > 1) {
        LazyList(
          ValueDerivatives.of(
            derivatives.value,
            derivatives.derivatives.subArray(0, derivatives.derivatives.size - 1)))
      } else {
        LazyList.empty
      }
    val zeroed = LazyList
      .range(0, derivatives.derivatives.size)
      .filterNot(index => isSimplifiedNumber(derivatives.derivatives.get(index)))
      .map(index =>
        ValueDerivatives.of(derivatives.value, zeroedElement(derivatives.derivatives, index)))
    (values #::: shorter #::: zeroed).distinct.filterNot(candidate => candidate == derivatives)
  }

  /**
   * Shrinks a half-up rounding towards rounding to no decimal places at all.
   *
   * Two candidates: the rounding without its fraction of the last place, and the rounding to fewer
   * decimal places. Both stay half-up roundings and both are within the image of
   * [[genHalfUpRounding]], which draws a fraction of zero as one of its fractions.
   */
  implicit val shrinkHalfUp: Shrink[HalfUp] = Shrink.withLazyList(halfUpCandidates)

  private def halfUpCandidates(rounding: HalfUp): LazyList[HalfUp] = {
    val withoutFraction =
      if (rounding.fraction > 0) {
        candidateOf(HalfUp.ofDecimalPlaces(rounding.decimalPlaces))
      } else {
        LazyList.empty
      }
    val fewerPlaces = simplerIntegers(rounding.decimalPlaces)
      .flatMap(places =>
        if (rounding.fraction > 0) {
          candidateOf(HalfUp.ofFractionalDecimalPlaces(places, rounding.fraction))
        } else {
          candidateOf(HalfUp.ofDecimalPlaces(places))
        })
    (withoutFraction #::: fewerPlaces).distinct.filterNot(candidate => candidate == rounding)
  }

  /**
   * Shrinks a rounding convention within the kind of rounding it is.
   *
   * A half-up rounding never shrinks to the rounding that does nothing: that one is a different
   * member of the family, with its own document form and its own behaviour of returning every value
   * unaltered, so a candidate of it would minimise a failure of rounding into a value that never
   * rounded. The rounding that does nothing carries no field and is its own floor.
   */
  implicit val shrinkRounding: Shrink[Rounding] = Shrink.withLazyList(roundingCandidates)

  private def roundingCandidates(rounding: Rounding): LazyList[Rounding] =
    rounding match {
      case halfUp: HalfUp => halfUpCandidates(halfUp).map(candidate => candidate: Rounding)
      case _ => LazyList.empty
    }

  /**
   * Shrinks a step changing a value, keeping the way the step is positioned.
   *
   * A step positioned by an index stays positioned by an index, and a step positioned by a date
   * stays positioned by a date. Exactly one of the two positions is named - the factory rejects a
   * step naming both and a step naming neither - and the two are resolved against a schedule in
   * different ways, so a candidate of the other shape would be a step of a different kind from the
   * one that failed. An index shrinks towards one, the smallest index the factory accepts, and a
   * date towards the first date of the generated window.
   */
  implicit val shrinkValueStep: Shrink[ValueStep] = Shrink.withLazyList(valueStepCandidates)

  private def valueStepCandidates(step: ValueStep): LazyList[ValueStep] = {
    val positions = step.periodIndex match {
      case Some(index) =>
        simplerPositions(index)
          .flatMap(position => candidateOf(ValueStep.of(position, step.value)))
      case None =>
        LazyList
          .from(step.date.toList)
          .flatMap(date => simplerDates(date))
          .map(date => ValueStep.of(date, step.value))
    }
    val adjustments = valueAdjustmentCandidates(step.value)
      .flatMap(adjustment =>
        candidateOf(ValueStep.of(step.periodIndex, step.date, adjustment)))
    (positions #::: adjustments).distinct.filterNot(candidate => candidate == step)
  }

  /**
   * Shrinks a sequence of steps towards a single step at the start of the generated window.
   *
   * The first date stays on or before the last, which the factory requires: collapsing the two
   * makes them equal, and translating moves both by the same number of days. The adjustment keeps
   * its kind, so an adjustment the factory accepts - it refuses one that replaces the value - stays
   * one it accepts.
   */
  implicit val shrinkValueStepSequence: Shrink[ValueStepSequence] =
    Shrink.withLazyList(valueStepSequenceCandidates)

  private def valueStepSequenceCandidates(
      sequence: ValueStepSequence): LazyList[ValueStepSequence] = {

    val collapsed =
      if (sequence.lastStepDate == sequence.firstStepDate) {
        LazyList.empty
      } else {
        candidateOf(
          ValueStepSequence.of(
            sequence.firstStepDate,
            sequence.firstStepDate,
            sequence.frequency,
            sequence.adjustment))
      }
    val moved =
      if (sequence.firstStepDate.isAfter(FirstDate)) {
        val offset = sequence.firstStepDate.toEpochDay - FirstDate.toEpochDay
        candidateOf(
          ValueStepSequence.of(
            sequence.firstStepDate.minusDays(offset),
            sequence.lastStepDate.minusDays(offset),
            sequence.frequency,
            sequence.adjustment))
      } else {
        LazyList.empty
      }
    val frequencies = frequencyCandidates(sequence.frequency)
      .flatMap(frequency =>
        candidateOf(
          ValueStepSequence.of(
            sequence.firstStepDate,
            sequence.lastStepDate,
            frequency,
            sequence.adjustment)))
    val adjustments = valueAdjustmentCandidates(sequence.adjustment)
      .flatMap(adjustment =>
        candidateOf(
          ValueStepSequence.of(
            sequence.firstStepDate,
            sequence.lastStepDate,
            sequence.frequency,
            adjustment)))
    (collapsed #::: moved #::: frequencies #::: adjustments).distinct
      .filterNot(candidate => candidate == sequence)
  }

  /**
   * Shrinks a schedule of values towards the initial value alone.
   *
   * The initial value goes to positive zero, the last step is dropped, a step that is kept is
   * replaced by one of its own candidates, and the sequence of steps is dropped or replaced by one
   * of its candidates. [[ValueSchedule.of]] refuses a position named by two steps with different
   * adjustments, so a candidate it refuses is dropped rather than offered.
   *
   * The measure is one where the initial value is not already positive zero, plus the number of
   * steps and the sum of their measures, plus one where a sequence is named and its measure.
   */
  implicit val shrinkValueSchedule: Shrink[ValueSchedule] =
    Shrink.withLazyList(valueScheduleCandidates)

  private def valueScheduleCandidates(schedule: ValueSchedule): LazyList[ValueSchedule] = {
    val values = simplerNumbers(schedule.initialValue)
      .flatMap(value => ValueSchedule.of(value, schedule.steps, schedule.stepSequence).toOption)
    val shorter =
      if (schedule.steps.nonEmpty) {
        LazyList.from(
          ValueSchedule.of(schedule.initialValue, schedule.steps.init, schedule.stepSequence).toOption)
      } else {
        LazyList.empty
      }
    val simplerSteps = LazyList
      .range(0, schedule.steps.size)
      .flatMap(index =>
        valueStepCandidates(schedule.steps(index)).flatMap(step =>
          ValueSchedule
            .of(schedule.initialValue, schedule.steps.updated(index, step), schedule.stepSequence)
            .toOption))
    val withoutSequence = schedule.stepSequence match {
      case Some(_) =>
        LazyList.from(ValueSchedule.of(schedule.initialValue, schedule.steps, None).toOption)
      case None => LazyList.empty
    }
    val simplerSequences = LazyList
      .from(schedule.stepSequence.toList)
      .flatMap(sequence => valueStepSequenceCandidates(sequence))
      .flatMap(sequence =>
        ValueSchedule.of(schedule.initialValue, schedule.steps, Some(sequence)).toOption)
    (values #::: shorter #::: simplerSteps #::: withoutSequence #::: simplerSequences).distinct
      .filterNot(candidate => candidate == schedule)
  }

  //-------------------------------------------------------------------------
  // The shrinkings of the currency package, continued: the adjustable payment.
  //-------------------------------------------------------------------------
  /**
   * Shrinks an adjustable payment by simplifying its amount and its date.
   *
   * The measure is the measure of the amount plus the measure of the adjustable date.
   */
  implicit val shrinkAdjustablePayment: Shrink[AdjustablePayment] =
    Shrink.withLazyList(adjustablePaymentCandidates)

  private def adjustablePaymentCandidates(
      payment: AdjustablePayment): LazyList[AdjustablePayment] = {

    val amounts = currencyAmountCandidates(payment.value)
      .map(amount => AdjustablePayment.of(amount, payment.date))
    val dates = adjustableDateCandidates(payment.date)
      .map(date => AdjustablePayment.of(payment.value, date))
    (amounts #::: dates).distinct.filterNot(candidate => candidate == payment)
  }
}
