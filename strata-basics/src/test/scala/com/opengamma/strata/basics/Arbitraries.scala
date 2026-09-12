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

/**
 * A calculation target carrying data, used wherever a spec needs one.
 *
 * [[CalculationTarget]] is a marker with no member of its own - it is the contract a later
 * module of the migration implements - so nothing in this module can be generated as one
 * without a concrete type to generate. This is that type: the smallest thing that is a
 * calculation target and can be told apart from another, which is all
 * [[CalculationTargetList]] needs of its elements.
 *
 * It is declared here, beside the generators, rather than in a spec of its own, because it is
 * a fixture of the generator surface rather than of any one suite: both
 * [[Arbitraries.genCalculationTargetList]] and the specs that build a list by hand reach for
 * the same type, and two near-identical throwaway types would be two things to keep in step.
 *
 * @param value  the number distinguishing this target from another
 */
final case class TestTarget(value: Int) extends CalculationTarget

/**
 * The ScalaCheck generators of `strata-basics`.
 *
 * This is the one generator source of the module's test tree. It is a fixture rather than a
 * suite - it declares no test - and every public name it holds is a contract of that fixture:
 * the JSON round-trip suite, the typeclass law suite, the smart-constructor suite and the
 * property-based sections of the ported specs all reach for the names below.
 *
 * The Java library achieved the same coverage reflectively, sweeping every bean through
 * `coverImmutableBean`, `coverBeanEquals` and `assertSerialization`. None of that machinery is
 * ported: reflection is exactly what the port removes from the serialization path. Generated
 * values take its place, which is why this file is exhaustive over the serializable surface of
 * the module rather than a sample of it - every type with a JSON codec has a generator here,
 * and so does every type carrying a `cats` instance the law suites check.
 *
 * Three rules hold throughout, and a reader checking a generator should check it against them:
 *
 *   - '''Every value is built by the type's own factory.''' No value of a validated or
 *     normalising type is reached by any other route - there is no other route, since those
 *     types have neither a public `apply` nor a `copy` - and no outcome is unwrapped by force.
 *     A factory that rejects its input contributes no value and fails the draw through
 *     [[fromEither]], carrying the name of the factory that rejected it; every generator below
 *     draws its inputs from the range its factory documents, so that branch reports a mistake
 *     in this file rather than an expected outcome.
 *   - '''Every generator is pure and every draw is reproducible.''' Nothing here reads a clock,
 *     a random source or any mutable state, so two runs from one seed produce byte-identical
 *     values. The JSON audit depends on it: it generates the whole inventory twice from the
 *     same seed, once encoding and once not, and compares the classes the two runs load.
 *     Collections built from an unordered `Map` are sorted before they are drawn from, so even
 *     an iteration order cannot leak into a value.
 *   - '''Every generated value is small.''' Lists, arrays, holiday sets and schedules are
 *     bounded by the constants at the head of this object, and dates are drawn from a
 *     twenty-one year window. The inventory is large, so a generator that produced the sizes
 *     ScalaCheck offers by default would make every property run over it slow.
 *
 * The double-bearing types are generated with the IEEE-754 edge values their equality turns on
 * - `NaN`, both infinities and both signed zeroes - wherever the type's factory admits them,
 * and without them where it does not. [[CurrencyAmount]] is the case to know: it rejects `NaN`
 * and accepts the infinities, so its generator produces an infinity but never a `NaN`, and
 * produces a negative zero so that the normalisation to positive zero is observed.
 *
 * The generators of `strata-collect` are re-exported rather than restated, so a spec of this
 * module needs one import. Importing both this object and the one it re-exports from would
 * make every re-exported instance ambiguous; import this one alone.
 */
object Arbitraries {

  //-------------------------------------------------------------------------
  /** The first year of the window every generated date falls in. */
  private val MinYear: Int = 2010

  /** The last year of the window every generated date falls in. */
  private val MaxYear: Int = 2030

  /** The largest number of currencies a generated multi-currency value holds. */
  private val MaxCurrencies: Int = 4

  /** The largest length of a generated run of amounts. */
  private val MaxRunLength: Int = 6

  /** The largest number of dates a generated run of dates holds. */
  private val MaxDates: Int = 4

  /** The largest number of periods a generated schedule holds. */
  private val MaxPeriods: Int = 6

  /** The largest number of holidays a generated custom calendar declares. */
  private val MaxHolidays: Int = 6

  /** The largest number of steps a generated value schedule holds. */
  private val MaxSteps: Int = 3

  /** The largest number of decimal places a generated rounding names. */
  private val MaxRoundingPlaces: Int = 8

  /** The date every index is probed with to find whether its calendars resolve. */
  private val CalendarProbeDate: LocalDate = LocalDate.of(2015, 6, 15)

  //-------------------------------------------------------------------------
  /**
   * Turns the outcome of a validated factory into a generated value.
   *
   * A `Right` generates its value. A `Left` generates nothing and labels the draw with the
   * factory that rejected its input, so a generator handing a factory something outside its
   * documented range reports that rather than falling back to a value the property never asked
   * for. Every call site below draws from a range its factory accepts, so the rejecting branch
   * is unreachable in practice; it is written rather than assumed away because the alternative
   * - unwrapping the outcome by force - would turn a mistake in a range into an exception
   * thrown from the middle of an unrelated property.
   *
   * The method is generic in the failure type, so it serves the factories answering
   * `Either[Failure, A]` and those answering `EitherNec[Failure, A]` alike.
   *
   * @param description  the factory the outcome came from, named for the label
   * @param outcome  the outcome of that factory
   * @tparam E  the type describing why the factory rejected its input
   * @tparam A  the type of the value produced
   * @return a generator of the value, or one that fails the draw
   */
  private def fromEither[E, A](description: String, outcome: Either[E, A]): Gen[A] =
    outcome match {
      case Right(value) => Gen.const(value)
      case Left(cause) => Gen.fail[A].label(s"$description rejected a generated input: $cause")
    }

  /**
   * Turns an optional value into a generated one.
   *
   * This is [[fromEither]] for the one shape that is not an outcome: recovering the non-empty
   * collection types the module's invariants are carried by. `None` fails the draw with a
   * label, for the same reason a `Left` does.
   *
   * @param description  the conversion the value came from, named for the label
   * @param value  the value, if there is one
   * @tparam A  the type of the value produced
   * @return a generator of the value, or one that fails the draw
   */
  private def fromOption[A](description: String, value: Option[A]): Gen[A] =
    value match {
      case Some(present) => Gen.const(present)
      case None => Gen.fail[A].label(s"$description produced nothing for a generated input")
    }

  /**
   * Runs a list of generators in order, collecting what they draw.
   *
   * ScalaCheck offers this as `Gen.sequence`, which needs a `Buildable` for the collection it
   * builds; this states the fold directly so that the element order of the result is visibly
   * the element order of the input, which several generators below depend on - the order
   * currencies are placed into a matrix is part of the matrix.
   *
   * @param generators  the generators to run, in the order their values are wanted
   * @tparam A  the type of value each generator draws
   * @return a generator of the drawn values, in the order the generators were given
   */
  private def sequenceGen[A](generators: List[Gen[A]]): Gen[List[A]] =
    generators.foldRight(Gen.const(List.empty[A]))((head, tail) =>
      head.flatMap(value => tail.map(drawn => value :: drawn)))

  /**
   * Collects a list of outcomes into an outcome of a list, keeping the first failure.
   *
   * The counterpart of [[sequenceGen]] for the factories that answer one value at a time where
   * a generator needs a run of them - the periods of a schedule, the amounts of a
   * multi-currency value. The whole run reaches [[fromEither]] as one outcome, so a rejected
   * element fails the draw with the factory named rather than being dropped from the run.
   *
   * @param outcomes  the outcomes to collect
   * @tparam E  the type describing why a factory rejected its input
   * @tparam A  the type of the values produced
   * @return every value, or the first failure
   */
  private def sequenceEither[E, A](outcomes: List[Either[E, A]]): Either[E, List[A]] =
    outcomes.foldRight(Right(List.empty[A]): Either[E, List[A]])((head, tail) =>
      head.flatMap(value => tail.map(collected => value :: collected)))

  /**
   * Builds a `Cogen` from a rendering of the type.
   *
   * A `Cogen` has to agree with equality - equal values must perturb a seed identically - and
   * every type of this module renders the fields its equality compares, so a rendering is a
   * sound basis for one. Two values that are ''not'' equal may share a rendering and so share a
   * perturbation, which costs a little entropy in a generated function and breaks no law.
   *
   * The law suites need these: `OrderTests` and `HashTests` check laws over generated
   * functions, and ScalaCheck builds an `A => A` only where it can perturb an `A`.
   *
   * @param render  the rendering of the type, which must agree with its equality
   * @tparam A  the type being perturbed
   * @return the perturbation of that type
   */
  private def cogenBy[A](render: A => String): Cogen[A] =
    implicitly[Cogen[String]].contramap[A](render)

  //-------------------------------------------------------------------------
  // The generators of `strata-collect`, re-exported by reference.
  //
  // Every one of these is declared in that module's own generator source, which is reachable
  // here over the test-scoped dependency between the two projects. They are named again rather
  // than imported wholesale so that a spec of this module imports one object: a wildcard import
  // of both would make each of these instances ambiguous, since both objects would offer it.
  // Nothing below restates a generator - each is the collect value itself.
  //-------------------------------------------------------------------------
  /** Generates a finite `double` of moderate magnitude, from `strata-collect`. */
  val genFiniteDouble: Gen[Double] = CollectArbitraries.genFiniteDouble

  /** Generates one of the five IEEE-754 edge values, from `strata-collect`. */
  val genEdgeDouble: Gen[Double] = CollectArbitraries.genEdgeDouble

  /** Generates a mostly finite `double` that is occasionally an edge value, from collect. */
  val genDouble: Gen[Double] = CollectArbitraries.genDouble

  /** Generates non-empty text of at most twenty-four characters, from `strata-collect`. */
  val genNonEmptyText: Gen[String] = CollectArbitraries.genNonEmptyText

  /** Generates non-empty text of one to twelve upper-case letters, from `strata-collect`. */
  val genUpperLetterText: Gen[String] = CollectArbitraries.genUpperLetterText

  /** Generates a decimal, from `strata-collect`. */
  val genDecimal: Gen[Decimal] = CollectArbitraries.genDecimal

  /** The arbitrary decimal of `strata-collect`. */
  implicit val arbDecimal: Arbitrary[Decimal] = CollectArbitraries.arbDecimal

  /** The perturbation of decimals of `strata-collect`. */
  implicit val cogenDecimal: Cogen[Decimal] = CollectArbitraries.cogenDecimal

  /** The shrinking of decimals of `strata-collect`. */
  implicit val shrinkDecimal: Shrink[Decimal] = CollectArbitraries.shrinkDecimal

  /** Generates a fixed-scale decimal, from `strata-collect`. */
  val genFixedScaleDecimal: Gen[FixedScaleDecimal] = CollectArbitraries.genFixedScaleDecimal

  /** The arbitrary fixed-scale decimal of `strata-collect`. */
  implicit val arbFixedScaleDecimal: Arbitrary[FixedScaleDecimal] =
    CollectArbitraries.arbFixedScaleDecimal

  /** The perturbation of fixed-scale decimals of `strata-collect`. */
  implicit val cogenFixedScaleDecimal: Cogen[FixedScaleDecimal] =
    CollectArbitraries.cogenFixedScaleDecimal

  /** The shrinking of fixed-scale decimals of `strata-collect`. */
  implicit val shrinkFixedScaleDecimal: Shrink[FixedScaleDecimal] =
    CollectArbitraries.shrinkFixedScaleDecimal

  /** Generates an array of finite elements, possibly empty, from `strata-collect`. */
  val genFiniteDoubleArray: Gen[DoubleArray] = CollectArbitraries.genFiniteDoubleArray

  /** Generates a non-empty array of finite elements, from `strata-collect`. */
  val genNonEmptyFiniteDoubleArray: Gen[DoubleArray] =
    CollectArbitraries.genNonEmptyFiniteDoubleArray

  /** Generates an array whose elements include the IEEE-754 edges, from `strata-collect`. */
  val genDoubleArray: Gen[DoubleArray] = CollectArbitraries.genDoubleArray

  /** Generates a non-empty array whose elements include the edges, from `strata-collect`. */
  val genNonEmptyDoubleArray: Gen[DoubleArray] = CollectArbitraries.genNonEmptyDoubleArray

  /** The arbitrary array of `strata-collect`, whose elements are finite. */
  implicit val arbDoubleArray: Arbitrary[DoubleArray] = CollectArbitraries.arbDoubleArray

  /** The perturbation of arrays of `strata-collect`. */
  implicit val cogenDoubleArray: Cogen[DoubleArray] = CollectArbitraries.cogenDoubleArray

  /** The shrinking of arrays of `strata-collect`. */
  implicit val shrinkDoubleArray: Shrink[DoubleArray] = CollectArbitraries.shrinkDoubleArray

  /** Generates a rectangular matrix of finite entries, from `strata-collect`. */
  val genFiniteDoubleMatrix: Gen[DoubleMatrix] = CollectArbitraries.genFiniteDoubleMatrix

  /** Generates a rectangular matrix whose entries include the edges, from collect. */
  val genDoubleMatrix: Gen[DoubleMatrix] = CollectArbitraries.genDoubleMatrix

  /** Generates a square matrix of finite entries, from `strata-collect`. */
  val genSquareFiniteDoubleMatrix: Gen[DoubleMatrix] =
    CollectArbitraries.genSquareFiniteDoubleMatrix

  /** Generates a square matrix whose entries include the edges, from `strata-collect`. */
  val genSquareDoubleMatrix: Gen[DoubleMatrix] = CollectArbitraries.genSquareDoubleMatrix

  /** The arbitrary matrix of `strata-collect`, whose entries are finite. */
  implicit val arbDoubleMatrix: Arbitrary[DoubleMatrix] = CollectArbitraries.arbDoubleMatrix

  /** The perturbation of matrices of `strata-collect`. */
  implicit val cogenDoubleMatrix: Cogen[DoubleMatrix] = CollectArbitraries.cogenDoubleMatrix

  /** The shrinking of matrices of `strata-collect`. */
  implicit val shrinkDoubleMatrix: Shrink[DoubleMatrix] = CollectArbitraries.shrinkDoubleMatrix

  /** Generates one of the ten failure reasons, from `strata-collect`. */
  val genFailureReason: Gen[FailureReason] = CollectArbitraries.genFailureReason

  /** The arbitrary failure reason of `strata-collect`. */
  implicit val arbFailureReason: Arbitrary[FailureReason] = CollectArbitraries.arbFailureReason

  /** The perturbation of failure reasons of `strata-collect`. */
  implicit val cogenFailureReason: Cogen[FailureReason] = CollectArbitraries.cogenFailureReason

  /** Generates a failure of any of the ten reasons, from `strata-collect`. */
  val genFailure: Gen[Failure] = CollectArbitraries.genFailure

  /** Generates a failure of one given reason, from `strata-collect`. */
  def genFailureWithReason(reason: FailureReason): Gen[Failure] =
    CollectArbitraries.genFailureWithReason(reason)

  /** The arbitrary failure of `strata-collect`. */
  implicit val arbFailure: Arbitrary[Failure] = CollectArbitraries.arbFailure

  /** The perturbation of failures of `strata-collect`. */
  implicit val cogenFailure: Cogen[Failure] = CollectArbitraries.cogenFailure

  /** The shrinking of failures of `strata-collect`. */
  implicit val shrinkFailure: Shrink[Failure] = CollectArbitraries.shrinkFailure

  /** Generates a non-empty chain of failures, from `strata-collect`. */
  val genFailures: Gen[NonEmptyChain[Failure]] = CollectArbitraries.genFailures

  /** The arbitrary chain of failures of `strata-collect`. */
  implicit val arbFailures: Arbitrary[NonEmptyChain[Failure]] = CollectArbitraries.arbFailures

  /** The perturbation of chains of failures of `strata-collect`. */
  implicit val cogenFailures: Cogen[NonEmptyChain[Failure]] = CollectArbitraries.cogenFailures

  /** The shrinking of chains of failures of `strata-collect`. */
  implicit val shrinkFailures: Shrink[NonEmptyChain[Failure]] =
    CollectArbitraries.shrinkFailures

  /** Generates a member of the sample named family of `strata-collect`. */
  val genSampleNamed: Gen[SampleNamed] = CollectArbitraries.genSampleNamed

  /** The arbitrary member of the sample named family of `strata-collect`. */
  implicit val arbSampleNamed: Arbitrary[SampleNamed] = CollectArbitraries.arbSampleNamed

  /** The perturbation of the sample named family of `strata-collect`. */
  implicit val cogenSampleNamed: Cogen[SampleNamed] = CollectArbitraries.cogenSampleNamed

  //-------------------------------------------------------------------------
  // The shared primitives: dates, months, periods and the double ranges the domain
  // factories accept. Every generator below is written over these rather than over a range
  // of its own, so one change to the window moves every date this file produces.
  //-------------------------------------------------------------------------
  /**
   * Generates a date in the twenty-one year window.
   *
   * The day of the year runs to 365 rather than to the length of the year, so that a leap year
   * and a common year are drawn from the same range and the draw is one number rather than a
   * month and a length-dependent day.
   *
   * @return a generator of dates from 2010 to 2030
   */
  val genLocalDate: Gen[LocalDate] =
    for {
      year <- Gen.choose(MinYear, MaxYear)
      dayOfYear <- Gen.choose(1, 365)
    } yield LocalDate.ofYearDay(year, dayOfYear)

  /**
   * Generates a year and month in the twenty-one year window.
   *
   * @return a generator of months from January 2010 to December 2030
   */
  val genYearMonth: Gen[YearMonth] =
    for {
      year <- Gen.choose(MinYear, MaxYear)
      month <- Gen.choose(1, 12)
    } yield YearMonth.of(year, month)

  /**
   * Generates a period of whole months or years, which every addition convention accepts.
   *
   * A period holding days is rejected by the two month-based addition conventions, so the
   * adjustments carrying one of those draw their periods from here.
   *
   * @return a generator of month-based periods
   */
  val genMonthBasedPeriod: Gen[Period] = Gen.frequency(
    3 -> Gen.choose(1, 24).map(months => Period.ofMonths(months)),
    1 -> Gen.choose(1, 10).map(years => Period.ofYears(years)))

  /**
   * Generates a period that may hold days as well as months.
   *
   * @return a generator of positive periods
   */
  val genPeriod: Gen[Period] = Gen.frequency(
    3 -> genMonthBasedPeriod,
    1 -> Gen.choose(1, 60).map(days => Period.ofDays(days)),
    1 -> Gen.choose(1, 8).map(weeks => Period.ofWeeks(weeks)))

  /**
   * Generates an amount a [[CurrencyAmount]] accepts.
   *
   * The distribution is mostly ordinary finite values, with the edge values that type admits
   * drawn often enough for a property over a handful of amounts to see one: both infinities,
   * which it accepts, and a negative zero, which it normalises to a positive one. `NaN` is
   * never drawn, because the factory rejects it.
   *
   * @return a generator of amounts that are finite or infinite, and never `NaN`
   */
  val genCurrencyAmountValue: Gen[Double] = Gen.frequency(
    6 -> genFiniteDouble,
    1 -> Gen.oneOf(Double.PositiveInfinity, Double.NegativeInfinity, -0.0, 0.0))

  /**
   * Generates a finite amount of moderate magnitude.
   *
   * This is the amount generator of the restricted values the `Monoid` law suite needs, and of
   * the two money types, whose factories narrow an amount through a decimal and so accept only
   * finite input. The sub-unit values are drawn deliberately: the rounding of a money value to
   * its currency's minor units is only exercised by an amount with digits below them.
   *
   * @return a generator of finite amounts, a third of them below the smallest minor unit
   */
  val genFiniteAmountValue: Gen[Double] = Gen.frequency(
    4 -> genFiniteDouble,
    2 -> Gen.choose(-1.0d, 1.0d),
    1 -> Gen.oneOf(0.0005d, -0.0005d, 1.0005d, 12.3456d, -12.3456d, 0.001d, 0.0d))

  /**
   * Generates a positive finite rate.
   *
   * Every exchange rate a [[FxRate]] holds has to be strictly positive, and the range spans
   * four orders of magnitude either side of one, which is the range the currencies of the
   * library actually trade at against each other.
   *
   * @return a generator of strictly positive finite rates
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
   * Both parts are drawn from the upper-case letters, which every scheme and every value
   * accepts, so the factory never rejects a draw. The rendering of an identifier is
   * `scheme~value`, and neither part can hold a tilde, so every generated identifier parses
   * back from its own rendering.
   *
   * @return a generator of standard identifiers
   */
  val genStandardId: Gen[StandardId] =
    for {
      scheme <- genUpperLetterText
      value <- genUpperLetterText
      identifier <- fromEither("StandardId.of", StandardId.of(scheme, value))
    } yield identifier

  /** The arbitrary standard identifier. */
  implicit val arbStandardId: Arbitrary[StandardId] = Arbitrary(genStandardId)

  /** The perturbation of standard identifiers, by their rendering. */
  implicit val cogenStandardId: Cogen[StandardId] = cogenBy[StandardId](_.toString)

  /**
   * Generates a calculation target carrying data.
   *
   * @return a generator of test targets
   */
  val genTestTarget: Gen[TestTarget] = Gen.choose(0, 1000).map(value => TestTarget(value))

  /** The arbitrary test target. */
  implicit val arbTestTarget: Arbitrary[TestTarget] = Arbitrary(genTestTarget)

  /** The perturbation of test targets, by their rendering. */
  implicit val cogenTestTarget: Cogen[TestTarget] = cogenBy[TestTarget](_.toString)

  /**
   * Generates a list of calculation targets.
   *
   * The list may be empty, which is a value the type accepts, and its elements are
   * [[TestTarget]] because the element type is a marker with no data of its own.
   *
   * @return a generator of calculation target lists
   */
  val genCalculationTargetList: Gen[CalculationTargetList] =
    for {
      size <- Gen.choose(0, MaxDates)
      targets <- Gen.listOfN(size, genTestTarget)
    } yield CalculationTargetList.of(targets)

  /** The arbitrary list of calculation targets. */
  implicit val arbCalculationTargetList: Arbitrary[CalculationTargetList] =
    Arbitrary(genCalculationTargetList)

  /** The perturbation of lists of calculation targets, by their rendering. */
  implicit val cogenCalculationTargetList: Cogen[CalculationTargetList] =
    cogenBy[CalculationTargetList](_.toString)

  //-------------------------------------------------------------------------
  // The currency package.
  //-------------------------------------------------------------------------
  /**
   * Generates one of the currencies of the closed family.
   *
   * @return a generator over every currency the module holds
   */
  val genCurrency: Gen[Currency] = Gen.oneOf(Currency.values.toList)

  /**
   * The currencies grouped by the number of minor units they hold, in increasing order.
   *
   * The grouping is sorted so that the list, and therefore every draw made from it, is the
   * same on every run: a `Map` built by grouping has no order of its own to rely on.
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
   * The family holds currencies of no minor units, of two and of three, and the two money
   * types round an amount to that number of places. Drawing uniformly over the currencies
   * would make a three-place rounding rare, since the two-place currencies far outnumber the
   * rest; drawing uniformly over the ''groups'' exercises each rounding equally.
   *
   * @return a generator of currencies spread evenly over the numbers of minor units
   */
  val genCurrencyAcrossMinorUnits: Gen[Currency] =
    Gen.oneOf(currenciesByMinorUnits).flatMap(group => Gen.oneOf(group))

  /** The arbitrary currency. */
  implicit val arbCurrency: Arbitrary[Currency] = Arbitrary(genCurrency)

  /** The perturbation of currencies, by their code. */
  implicit val cogenCurrency: Cogen[Currency] = cogenBy[Currency](_.code)

  /**
   * Generates a pair of two different currencies.
   *
   * The two are picked without replacement, so they are never the same currency, which is what
   * the rate-bearing types need of a pair.
   *
   * @return a generator of pairs of distinct currencies
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
   *
   * @return a generator of currency pairs
   */
  val genCurrencyPair: Gen[CurrencyPair] = Gen.frequency(
    6 -> genDistinctCurrencyPair,
    1 -> genCurrency.map(currency => CurrencyPair.of(currency, currency)))

  /** The arbitrary currency pair. */
  implicit val arbCurrencyPair: Arbitrary[CurrencyPair] = Arbitrary(genCurrencyPair)

  /** The perturbation of currency pairs, by their rendering. */
  implicit val cogenCurrencyPair: Cogen[CurrencyPair] = cogenBy[CurrencyPair](_.toString)

  /**
   * Generates an amount in a given currency.
   *
   * @param currency  the currency the amount is in
   * @return a generator of amounts in that currency
   */
  private def genAmountIn(currency: Currency): Gen[CurrencyAmount] =
    for {
      amount <- genCurrencyAmountValue
      value <- fromEither("CurrencyAmount.of", CurrencyAmount.of(currency, amount))
    } yield value

  /**
   * Generates an amount of a currency.
   *
   * The amount is drawn from [[genCurrencyAmountValue]], so it is never `NaN` - the factory
   * rejects that - and is occasionally an infinity or a negative zero, which the factory
   * accepts and normalises respectively.
   *
   * @return a generator of currency amounts
   */
  val genCurrencyAmount: Gen[CurrencyAmount] = genCurrency.flatMap(genAmountIn)

  /**
   * Generates an amount of a currency whose value is finite and of moderate magnitude.
   *
   * This is the generator the `Monoid` law suite of [[MultiCurrencyAmount]] must use. Adding
   * two infinities of opposite sign produces `NaN`, which the amount factory rejects, so the
   * associativity law would fail on a value the type cannot hold rather than on a defect; and
   * floating-point addition is only approximately associative, so that suite also needs the
   * tolerant equality the migration note records. The wide generator above is deliberately
   * left wide, because the JSON round trip needs the infinities.
   *
   * @return a generator of finite currency amounts
   */
  val genFiniteCurrencyAmount: Gen[CurrencyAmount] =
    for {
      currency <- genCurrency
      amount <- genFiniteAmountValue
      value <- fromEither("CurrencyAmount.of", CurrencyAmount.of(currency, amount))
    } yield value

  /** The arbitrary currency amount, whose value may be infinite but is never `NaN`. */
  implicit val arbCurrencyAmount: Arbitrary[CurrencyAmount] = Arbitrary(genCurrencyAmount)

  /** The perturbation of currency amounts, by their rendering. */
  implicit val cogenCurrencyAmount: Cogen[CurrencyAmount] = cogenBy[CurrencyAmount](_.toString)

  /**
   * Generates a money value, rounded to its currency's minor units.
   *
   * The currency is drawn evenly over the numbers of minor units and the amount is finite with
   * digits below the smallest unit, so the rounding the factory performs is exercised at each
   * of the three scales the family holds.
   *
   * @return a generator of money values
   */
  val genMoney: Gen[Money] =
    for {
      currency <- genCurrencyAcrossMinorUnits
      amount <- genFiniteAmountValue
      value <- fromEither("Money.of", Money.of(currency, amount))
    } yield value

  /** The arbitrary money value. */
  implicit val arbMoney: Arbitrary[Money] = Arbitrary(genMoney)

  /** The perturbation of money values, by their rendering. */
  implicit val cogenMoney: Cogen[Money] = cogenBy[Money](_.toString)

  /**
   * Generates a money value of unrestricted currency scale, rounded to twelve places.
   *
   * @return a generator of big money values
   */
  val genBigMoney: Gen[BigMoney] =
    for {
      currency <- genCurrencyAcrossMinorUnits
      amount <- genFiniteAmountValue
      value <- fromEither("BigMoney.of", BigMoney.of(currency, amount))
    } yield value

  /** The arbitrary big money value. */
  implicit val arbBigMoney: Arbitrary[BigMoney] = Arbitrary(genBigMoney)

  /** The perturbation of big money values, by their rendering. */
  implicit val cogenBigMoney: Cogen[BigMoney] = cogenBy[BigMoney](_.toString)

  /**
   * Generates a multi-currency amount.
   *
   * The currencies are picked without replacement, because the factory rejects a duplicate;
   * a value holding two amounts of one currency is what `total` produces, and a spec wanting
   * that merge calls it directly. The empty amount is drawn as well, since it is the identity
   * of the type's `Monoid` and a document may carry it.
   *
   * @return a generator of multi-currency amounts holding up to four currencies
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
   * [[genFiniteCurrencyAmount]]: combining opposite infinities reaches the `NaN` the amount
   * factory rejects, and the associativity of floating-point addition holds only to within a
   * tolerance.
   *
   * @return a generator of multi-currency amounts holding only finite amounts
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

  /** The arbitrary multi-currency amount. */
  implicit val arbMultiCurrencyAmount: Arbitrary[MultiCurrencyAmount] =
    Arbitrary(genMultiCurrencyAmount)

  /** The perturbation of multi-currency amounts, by their rendering. */
  implicit val cogenMultiCurrencyAmount: Cogen[MultiCurrencyAmount] =
    cogenBy[MultiCurrencyAmount](_.toString)

  /**
   * Generates an array of elements of a given length.
   *
   * The elements include the IEEE-754 edge values, which every array-bearing type of the
   * module admits - the equality of those types compares bit patterns, so a `NaN` is a value
   * they hold rather than one they reject.
   *
   * @param length  the number of elements
   * @return a generator of arrays of that length
   */
  private def genDoubleArrayOfLength(length: Int): Gen[DoubleArray] =
    Gen.listOfN(length, genDouble).map(elements => DoubleArray.copyOf(elements))

  /**
   * Generates a run of amounts in one currency.
   *
   * @return a generator of runs of single-currency amounts
   */
  val genCurrencyAmountArray: Gen[CurrencyAmountArray] =
    for {
      currency <- genCurrency
      length <- Gen.choose(1, MaxRunLength)
      values <- genDoubleArrayOfLength(length)
    } yield CurrencyAmountArray.of(currency, values)

  /** The arbitrary run of single-currency amounts. */
  implicit val arbCurrencyAmountArray: Arbitrary[CurrencyAmountArray] =
    Arbitrary(genCurrencyAmountArray)

  /** The perturbation of runs of single-currency amounts, by their rendering. */
  implicit val cogenCurrencyAmountArray: Cogen[CurrencyAmountArray] =
    cogenBy[CurrencyAmountArray](_.toString)

  /**
   * Generates a run of multi-currency amounts.
   *
   * Every currency carries a run of the same length, which is what the factory requires, and
   * the currencies are picked without replacement so that none is named twice.
   *
   * @return a generator of runs of multi-currency amounts
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

  /** The arbitrary run of multi-currency amounts. */
  implicit val arbMultiCurrencyAmountArray: Arbitrary[MultiCurrencyAmountArray] =
    Arbitrary(genMultiCurrencyAmountArray)

  /** The perturbation of runs of multi-currency amounts, by their rendering. */
  implicit val cogenMultiCurrencyAmountArray: Cogen[MultiCurrencyAmountArray] =
    cogenBy[MultiCurrencyAmountArray](_.toString)

  /**
   * Generates a payment.
   *
   * @return a generator of payments
   */
  val genPayment: Gen[Payment] =
    for {
      amount <- genCurrencyAmount
      date <- genLocalDate
    } yield Payment.of(amount, date)

  /** The arbitrary payment. */
  implicit val arbPayment: Arbitrary[Payment] = Arbitrary(genPayment)

  /** The perturbation of payments, by their rendering. */
  implicit val cogenPayment: Cogen[Payment] = cogenBy[Payment](_.toString)

  /**
   * Generates an exchange rate.
   *
   * Mostly a rate between two different currencies, and occasionally the rate of one currency
   * against itself, which the factory accepts only where the rate is exactly one.
   *
   * @return a generator of exchange rates
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

  /** The arbitrary exchange rate. */
  implicit val arbFxRate: Arbitrary[FxRate] = Arbitrary(genFxRate)

  /** The perturbation of exchange rates, by their rendering. */
  implicit val cogenFxRate: Cogen[FxRate] = cogenBy[FxRate](_.toString)

  /**
   * Generates a rate to place into a matrix, including the zero rate.
   *
   * The builder being ported accepted any `double` rate, and a zero rate is the case worth
   * drawing: its reciprocal is an infinity, so a matrix holding one carries a non-finite entry
   * that the codec has to tag and that equality has to compare by bit pattern. The Java suite
   * of this type placed a zero rate deliberately, and so does this.
   *
   * @return a generator of matrix rates, one draw in seven being zero
   */
  private val genMatrixRate: Gen[Double] = Gen.frequency(6 -> genPositiveRate, 1 -> Gen.const(0.0d))

  /**
   * Places a run of rates into a matrix, each against the first currency.
   *
   * Every rate names the first currency, which the matrix holds after the first placement, so
   * no placement can be the one the builder rejects - the one whose two currencies are both
   * unknown to the matrix. The currencies therefore enter in the order they are given, and
   * that order is part of the value: it is the order the matrix reports them in and the order
   * a document carries them in.
   *
   * @param currencies  the currencies, the first being the one every rate is against
   * @param rates  the rate of each currency after the first, against the first
   * @return the matrix, or the failure a placement reported
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
   * Generates a matrix of exchange rates.
   *
   * The empty matrix is drawn as well as populated ones, since it is a value the type holds
   * and a document may carry.
   *
   * @return a generator of matrices holding up to four currencies
   */
  val genFxMatrix: Gen[FxMatrix] = Gen.frequency(
    8 -> (for {
      size <- Gen.choose(2, MaxCurrencies)
      currencies <- Gen.pick(size, Currency.values.toList)
      rates <- Gen.listOfN(size - 1, genMatrixRate)
      value <- fromEither("FxMatrix.withRate", matrixOf(currencies.toList, rates))
    } yield value),
    1 -> Gen.const(FxMatrix.empty))

  /** The arbitrary matrix of exchange rates. */
  implicit val arbFxMatrix: Arbitrary[FxMatrix] = Arbitrary(genFxMatrix)

  /** The perturbation of matrices of exchange rates, by their rendering. */
  implicit val cogenFxMatrix: Cogen[FxMatrix] = cogenBy[FxMatrix](_.toString)

  //-------------------------------------------------------------------------
  // The date package.
  //-------------------------------------------------------------------------
  /**
   * Generates one of the seven business day conventions.
   *
   * @return a generator over every business day convention
   */
  val genBusinessDayConvention: Gen[BusinessDayConvention] =
    Gen.oneOf(BusinessDayConvention.values.toList)

  /** The arbitrary business day convention. */
  implicit val arbBusinessDayConvention: Arbitrary[BusinessDayConvention] =
    Arbitrary(genBusinessDayConvention)

  /** The perturbation of business day conventions, by their name. */
  implicit val cogenBusinessDayConvention: Cogen[BusinessDayConvention] =
    cogenBy[BusinessDayConvention](_.name)

  /**
   * The built-in calendars, ordered by the name of the calendar.
   *
   * Sorted so that a draw from it is the same on every run: the built-in set is held in a
   * `Map`, whose iteration order is no part of its contract.
   */
  private val builtInCalendars: List[HolidayCalendar] =
    StandardHolidayCalendars.all.toList
      .sortBy { case (id, _) => id.name }
      .map { case (_, calendar) => calendar }

  /** The identifiers of the built-in calendars, ordered by name. */
  private val builtInCalendarIds: List[HolidayCalendarId] =
    builtInCalendars.map(calendar => calendar.id)

  /**
   * The identifiers of the built-in calendars that declare holidays, ordered by name.
   *
   * The calendar of no holidays is excluded because it is absorbed by both composition
   * operators - combining with it drops it and linking to it yields it - so a composite naming
   * it would not be a composite at all.
   */
  private val datedCalendarIds: List[HolidayCalendarId] =
    builtInCalendarIds.filterNot(id => id == StandardHolidayCalendars.NO_HOLIDAYS.id)

  /**
   * Generates the identifier of one built-in calendar.
   *
   * These are the identifiers that resolve against the standard reference data, so they are
   * the ones an adjustment carries: a generated adjustment is meant to be applicable.
   *
   * @return a generator over the identifiers of the built-in calendars
   */
  val genBuiltInHolidayCalendarId: Gen[HolidayCalendarId] = Gen.oneOf(builtInCalendarIds)

  /**
   * Generates the identifier of a calendar that is not built in.
   *
   * The name is upper-case letters beginning with `X`, which is neither a built-in name nor a
   * name holding either composition separator.
   *
   * @return a generator of identifiers naming no built-in calendar
   */
  val genCustomHolidayCalendarId: Gen[HolidayCalendarId] =
    for {
      length <- Gen.choose(3, 5)
      letters <- Gen.listOfN(length, Gen.alphaUpperChar)
    } yield HolidayCalendarId.of(s"X${letters.mkString}")

  /**
   * Generates a holiday calendar identifier, simple or composite.
   *
   * Both composition forms are drawn - the combination written with a plus and the link
   * written with a tilde - because the identifier is carried in a document as its name and
   * both forms have to survive that. Composition is normalised, so a composite of two distinct
   * dated calendars is a composite whichever order the two are drawn in.
   *
   * @return a generator of simple, combined and linked identifiers
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

  /** The arbitrary holiday calendar identifier. */
  implicit val arbHolidayCalendarId: Arbitrary[HolidayCalendarId] =
    Arbitrary(genHolidayCalendarId)

  /** The perturbation of holiday calendar identifiers, by their name. */
  implicit val cogenHolidayCalendarId: Cogen[HolidayCalendarId] =
    cogenBy[HolidayCalendarId](_.name)

  /**
   * Generates one of the twenty-one standard day counts.
   *
   * @return a generator over the day counts that need no calendar
   */
  val genStandardDayCount: Gen[DayCount] = Gen.oneOf(DayCount.values.toList)

  /**
   * Generates a business day count over one of the built-in calendars.
   *
   * This is the one day count carrying data, and the one with a structural document form
   * rather than a name, so it is generated separately and drawn alongside the standard members
   * by [[genDayCount]]. The calendar is built in, so the name form of the document - which
   * resolves a calendar by name against the built-in set - reads it back as well.
   *
   * @return a generator of `Bus/252` day counts
   */
  val genBus252DayCount: Gen[DayCount] =
    Gen.oneOf(builtInCalendars).map(calendar => DayCount.ofBus252(calendar))

  /**
   * Generates a day count.
   *
   * @return a generator of the standard day counts and of `Bus/252` over a built-in calendar
   */
  val genDayCount: Gen[DayCount] = Gen.frequency(4 -> genStandardDayCount, 1 -> genBus252DayCount)

  /** The arbitrary day count, which is occasionally a `Bus/252` day count. */
  implicit val arbDayCount: Arbitrary[DayCount] = Arbitrary(genDayCount)

  /** The perturbation of day counts, by their name. */
  implicit val cogenDayCount: Cogen[DayCount] = cogenBy[DayCount](_.name)

  /**
   * Generates the weekend days of a calendar.
   *
   * The four shapes are the ones the library itself uses: the western weekend, the two
   * Middle-Eastern weekends and the single Sunday that Budapest declares, the last of which
   * makes every Saturday an ordinary business day unless a holiday says otherwise.
   *
   * @return a generator of sets of weekend days
   */
  val genWeekendDays: Gen[Set[DayOfWeek]] = Gen.oneOf(
    Set(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY),
    Set(DayOfWeek.FRIDAY, DayOfWeek.SATURDAY),
    Set(DayOfWeek.THURSDAY, DayOfWeek.FRIDAY, DayOfWeek.SATURDAY),
    Set(DayOfWeek.SUNDAY))

  /** Generates a date in one given year, up to the day of the year given. */
  private def genDateInYear(year: Int, lastDayOfYear: Int): Gen[LocalDate] =
    Gen.choose(1, lastDayOfYear).map(dayOfYear => LocalDate.ofYearDay(year, dayOfYear))

  /** Finds the first date on or after the one given that falls on a weekend day. */
  @tailrec
  private def firstWeekendDate(from: LocalDate, weekendDays: Set[DayOfWeek]): LocalDate =
    if (weekendDays.contains(from.getDayOfWeek)) from
    else firstWeekendDate(from.plusDays(1L), weekendDays)

  /** Finds the first date on or after the one given that falls on no weekend day. */
  @tailrec
  private def firstWeekdayDate(from: LocalDate, weekendDays: Set[DayOfWeek]): LocalDate =
    if (weekendDays.contains(from.getDayOfWeek)) firstWeekdayDate(from.plusDays(1L), weekendDays)
    else from

  /**
   * Generates the holidays of a custom calendar, spanning three years from the one given.
   *
   * A date in the first year and a date in the third are always drawn, so the year range the
   * calendar derives from its holidays always spans more than one year. That matters to the
   * round trip of a calendar: two calendars of one identifier are equal whatever their
   * holidays, so the suite compares the holidays over the year range as well, and a range of
   * one year would make that comparison much weaker than it looks.
   *
   * Those two spanning dates are moved off the weekend, because a calendar cannot tell a
   * holiday falling on a weekend day from the weekend itself - the two are one bit of the same
   * mask - so such a date is not among the holidays the calendar reports and would not widen
   * the span of the holidays a document carries. The middle dates are left where they fall,
   * since a holiday coinciding with a weekend is worth generating as well.
   *
   * The days of the year the two spanning dates are drawn from stop short of the end of the
   * year by more than a week, so moving one off the weekend cannot carry it into the next year
   * and the first year of the calendar is the year this was given.
   *
   * @param firstYear  the first of the three years the holidays span
   * @param weekendDays  the weekend days of the calendar being built
   * @return a generator of holiday dates, at least two of them and at least one in each of the
   *   outer two years
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
   * The document form of such a calendar is the structural one - the object carrying its
   * identifier, its weekend days, its first year, its holidays and the weekend dates it
   * declares to be working days - because the name form is reserved for the built-in set. One
   * draw in three declares a weekend date to be a working day, which is the field of that
   * object a calendar built without one leaves empty; the date is inside the year range the
   * holidays derive, because a working day outside it is ignored.
   *
   * @return a generator of custom calendars
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
   * The set holds the twenty-six dated calendars the library generates and the four named
   * after their weekends, including the calendar of no holidays. Each is carried in a document
   * as its name.
   *
   * @return a generator over the built-in calendars
   */
  val genBuiltInHolidayCalendar: Gen[HolidayCalendar] = Gen.oneOf(builtInCalendars)

  /**
   * Generates a calendar that is not a composition of two others.
   *
   * @return a generator of built-in and custom calendars
   */
  val genLeafHolidayCalendar: Gen[HolidayCalendar] = Gen.frequency(
    3 -> genBuiltInHolidayCalendar,
    2 -> genCustomHolidayCalendar.map(calendar => calendar: HolidayCalendar))

  /**
   * Generates a holiday calendar of any of the four shapes the codec distinguishes.
   *
   * Those shapes are a built-in calendar carried as its name, a custom calendar carried
   * structurally, a combination and a link. The two compositions are built from leaves rather
   * than recursively, which keeps a generated calendar shallow while still exercising both
   * wrappers; a spec wanting a deeper one composes two generated calendars itself.
   *
   * @return a generator of calendars of every shape
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

  /** The arbitrary holiday calendar. */
  implicit val arbHolidayCalendar: Arbitrary[HolidayCalendar] = Arbitrary(genHolidayCalendar)

  /** The arbitrary custom calendar, whose document form is the structural one. */
  implicit val arbImmutableHolidayCalendar: Arbitrary[ImmutableHolidayCalendar] =
    Arbitrary(genCustomHolidayCalendar)

  /**
   * The perturbation of holiday calendars, by their name.
   *
   * A calendar carrying holidays is equal to any calendar of the same identifier, and its name
   * ''is'' that identifier, so perturbing by the name is exactly as discriminating as the
   * equality of the type.
   */
  implicit val cogenHolidayCalendar: Cogen[HolidayCalendar] = cogenBy[HolidayCalendar](_.name)

  /**
   * Generates a business day adjustment.
   *
   * The calendar is a built-in one, so a generated adjustment can be applied against the
   * standard reference data; the no-adjustment constant is drawn as well, since it is the one
   * every structure carrying an adjustment defaults to.
   *
   * @return a generator of business day adjustments
   */
  val genBusinessDayAdjustment: Gen[BusinessDayAdjustment] = Gen.frequency(
    5 -> (for {
      convention <- genBusinessDayConvention
      calendar <- genBuiltInHolidayCalendarId
    } yield BusinessDayAdjustment.of(convention, calendar)),
    1 -> Gen.const(BusinessDayAdjustment.NONE))

  /** The arbitrary business day adjustment. */
  implicit val arbBusinessDayAdjustment: Arbitrary[BusinessDayAdjustment] =
    Arbitrary(genBusinessDayAdjustment)

  /** The perturbation of business day adjustments, by their rendering. */
  implicit val cogenBusinessDayAdjustment: Cogen[BusinessDayAdjustment] =
    cogenBy[BusinessDayAdjustment](_.toString)

  /**
   * Generates an adjustable date.
   *
   * @return a generator of adjustable dates
   */
  val genAdjustableDate: Gen[AdjustableDate] =
    for {
      date <- genLocalDate
      adjustment <- genBusinessDayAdjustment
    } yield AdjustableDate.of(date, adjustment)

  /** The arbitrary adjustable date. */
  implicit val arbAdjustableDate: Arbitrary[AdjustableDate] = Arbitrary(genAdjustableDate)

  /** The perturbation of adjustable dates, by their rendering. */
  implicit val cogenAdjustableDate: Cogen[AdjustableDate] = cogenBy[AdjustableDate](_.toString)

  /**
   * Generates a run of adjustable dates.
   *
   * The dates have to be strictly increasing, so they are built by walking forward from one
   * date in steps of at least a day; the run is never empty, which the type carries in the
   * type of its field rather than in a check.
   *
   * @return a generator of runs of adjustable dates
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

  /** The arbitrary run of adjustable dates. */
  implicit val arbAdjustableDates: Arbitrary[AdjustableDates] = Arbitrary(genAdjustableDates)

  /** The perturbation of runs of adjustable dates, by their rendering. */
  implicit val cogenAdjustableDates: Cogen[AdjustableDates] =
    cogenBy[AdjustableDates](_.toString)

  /**
   * Generates a days adjustment.
   *
   * All three shapes are drawn: an addition of calendar days, an addition of business days
   * against a named calendar, and the constant that adjusts nothing. The number of days may be
   * negative, as the type allows.
   *
   * @return a generator of days adjustments
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

  /** The arbitrary days adjustment. */
  implicit val arbDaysAdjustment: Arbitrary[DaysAdjustment] = Arbitrary(genDaysAdjustment)

  /** The perturbation of days adjustments, by their rendering. */
  implicit val cogenDaysAdjustment: Cogen[DaysAdjustment] = cogenBy[DaysAdjustment](_.toString)

  /**
   * Generates one of the three period addition conventions.
   *
   * @return a generator over every period addition convention
   */
  val genPeriodAdditionConvention: Gen[PeriodAdditionConvention] =
    Gen.oneOf(PeriodAdditionConvention.values.toList)

  /** The arbitrary period addition convention. */
  implicit val arbPeriodAdditionConvention: Arbitrary[PeriodAdditionConvention] =
    Arbitrary(genPeriodAdditionConvention)

  /** The perturbation of period addition conventions, by their name. */
  implicit val cogenPeriodAdditionConvention: Cogen[PeriodAdditionConvention] =
    cogenBy[PeriodAdditionConvention](_.name)

  /**
   * Generates a period adjustment.
   *
   * The two month-based conventions reject a period holding days, so the period is drawn from
   * the month-based range where the convention drawn is one of them, and from the wider range
   * where it is not. That pairing is what keeps the factory from rejecting a draw.
   *
   * @return a generator of period adjustments
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

  /** The arbitrary period adjustment. */
  implicit val arbPeriodAdjustment: Arbitrary[PeriodAdjustment] = Arbitrary(genPeriodAdjustment)

  /** The perturbation of period adjustments, by their rendering. */
  implicit val cogenPeriodAdjustment: Cogen[PeriodAdjustment] =
    cogenBy[PeriodAdjustment](_.toString)

  /**
   * Generates a tenor of whole months or years.
   *
   * @return a generator of month-based tenors
   */
  val genMonthBasedTenor: Gen[Tenor] = Gen.frequency(
    3 -> Gen
      .choose(1, 24)
      .flatMap(months => fromEither("Tenor.ofMonths", Tenor.ofMonths(months))),
    1 -> Gen.choose(1, 50).flatMap(years => fromEither("Tenor.ofYears", Tenor.ofYears(years))))

  /**
   * Generates a tenor.
   *
   * Every tenor is positive, which the factory requires, and a tenor of a whole number of
   * weeks is drawn as well as one of days and one of months, since the canonical name of a
   * tenor depends on which of those it is.
   *
   * @return a generator of tenors
   */
  val genTenor: Gen[Tenor] = Gen.frequency(
    4 -> genMonthBasedTenor,
    1 -> Gen.choose(1, 6).flatMap(days => fromEither("Tenor.ofDays", Tenor.ofDays(days))),
    1 -> Gen.choose(1, 52).flatMap(weeks => fromEither("Tenor.ofWeeks", Tenor.ofWeeks(weeks))))

  /** The arbitrary tenor. */
  implicit val arbTenor: Arbitrary[Tenor] = Arbitrary(genTenor)

  /** The perturbation of tenors, by their name. */
  implicit val cogenTenor: Cogen[Tenor] = cogenBy[Tenor](_.name)

  /**
   * Generates a tenor adjustment.
   *
   * The pairing rule is the one [[genPeriodAdjustment]] follows: a month-based convention
   * takes a month-based tenor, because it rejects a tenor holding days.
   *
   * @return a generator of tenor adjustments
   */
  val genTenorAdjustment: Gen[TenorAdjustment] =
    for {
      convention <- genPeriodAdditionConvention
      tenor <- if (convention.isMonthBased) genMonthBasedTenor else genTenor
      adjustment <- genBusinessDayAdjustment
      value <- fromEither("TenorAdjustment.of", TenorAdjustment.of(tenor, convention, adjustment))
    } yield value

  /** The arbitrary tenor adjustment. */
  implicit val arbTenorAdjustment: Arbitrary[TenorAdjustment] = Arbitrary(genTenorAdjustment)

  /** The perturbation of tenor adjustments, by their rendering. */
  implicit val cogenTenorAdjustment: Cogen[TenorAdjustment] =
    cogenBy[TenorAdjustment](_.toString)

  /**
   * Generates a market tenor.
   *
   * The four constants are drawn as well as the spot-starting tenor of a generated tenor,
   * because the codes of the constants - overnight, tomorrow-next, spot-next and spot-week -
   * are the ones whose text form is not simply the text form of a tenor.
   *
   * @return a generator of market tenors
   */
  val genMarketTenor: Gen[MarketTenor] = Gen.frequency(
    3 -> genTenor.flatMap(tenor => fromEither("MarketTenor.ofSpot", MarketTenor.ofSpot(tenor))),
    2 -> Gen.oneOf(MarketTenor.ON, MarketTenor.TN, MarketTenor.SN, MarketTenor.SW))

  /** The arbitrary market tenor. */
  implicit val arbMarketTenor: Arbitrary[MarketTenor] = Arbitrary(genMarketTenor)

  /** The perturbation of market tenors, by their code. */
  implicit val cogenMarketTenor: Cogen[MarketTenor] = cogenBy[MarketTenor](_.code)

  /**
   * Generates one of the six date sequences.
   *
   * @return a generator over every date sequence
   */
  val genDateSequence: Gen[DateSequence] = Gen.oneOf(DateSequence.values.toList)

  /** The arbitrary date sequence. */
  implicit val arbDateSequence: Arbitrary[DateSequence] = Arbitrary(genDateSequence)

  /** The perturbation of date sequences, by their name. */
  implicit val cogenDateSequence: Cogen[DateSequence] = cogenBy[DateSequence](_.name)

  /**
   * Generates an instruction selecting a date from a sequence.
   *
   * The three starting points are drawn in turn: no starting point at all, a year and month,
   * and a minimum period. The factory rejects an instruction naming both a year and month and
   * a minimum period, so the two are never drawn together; the zero period is drawn
   * deliberately, because the factory normalises it away to no minimum period at all.
   *
   * @return a generator of sequence date instructions
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

  /** The arbitrary sequence date instruction. */
  implicit val arbSequenceDate: Arbitrary[SequenceDate] = Arbitrary(genSequenceDate)

  /** The perturbation of sequence date instructions, by their rendering. */
  implicit val cogenSequenceDate: Cogen[SequenceDate] = cogenBy[SequenceDate](_.toString)

  //-------------------------------------------------------------------------
  // The index package.
  //-------------------------------------------------------------------------
  /**
   * Generates one of the Ibor indices of the closed family.
   *
   * @return a generator over every Ibor index the module holds
   */
  val genIborIndex: Gen[IborIndex] = Gen.oneOf(IborIndex.values.toList)

  /** The arbitrary Ibor index. */
  implicit val arbIborIndex: Arbitrary[IborIndex] = Arbitrary(genIborIndex)

  /** The perturbation of Ibor indices, by their name. */
  implicit val cogenIborIndex: Cogen[IborIndex] = cogenBy[IborIndex](_.name)

  /**
   * Generates one of the overnight indices of the closed family.
   *
   * @return a generator over every overnight index the module holds
   */
  val genOvernightIndex: Gen[OvernightIndex] = Gen.oneOf(OvernightIndex.values.toList)

  /** The arbitrary overnight index. */
  implicit val arbOvernightIndex: Arbitrary[OvernightIndex] = Arbitrary(genOvernightIndex)

  /** The perturbation of overnight indices, by their name. */
  implicit val cogenOvernightIndex: Cogen[OvernightIndex] = cogenBy[OvernightIndex](_.name)

  /**
   * Generates one of the price indices of the closed family.
   *
   * @return a generator over every price index the module holds
   */
  val genPriceIndex: Gen[PriceIndex] = Gen.oneOf(PriceIndex.values.toList)

  /** The arbitrary price index. */
  implicit val arbPriceIndex: Arbitrary[PriceIndex] = Arbitrary(genPriceIndex)

  /** The perturbation of price indices, by their name. */
  implicit val cogenPriceIndex: Cogen[PriceIndex] = cogenBy[PriceIndex](_.name)

  /**
   * Generates one of the FX indices of the closed family.
   *
   * @return a generator over every FX index the module holds
   */
  val genFxIndex: Gen[FxIndex] = Gen.oneOf(FxIndex.values.toList)

  /** The arbitrary FX index. */
  implicit val arbFxIndex: Arbitrary[FxIndex] = Arbitrary(genFxIndex)

  /** The perturbation of FX indices, by their name. */
  implicit val cogenFxIndex: Cogen[FxIndex] = cogenBy[FxIndex](_.name)

  /**
   * Generates one of the five floating rate types.
   *
   * @return a generator over every floating rate type
   */
  val genFloatingRateType: Gen[FloatingRateType] = Gen.oneOf(FloatingRateType.values.toList)

  /** The arbitrary floating rate type. */
  implicit val arbFloatingRateType: Arbitrary[FloatingRateType] = Arbitrary(genFloatingRateType)

  /** The perturbation of floating rate types, by their name. */
  implicit val cogenFloatingRateType: Cogen[FloatingRateType] = cogenBy[FloatingRateType](_.name)

  /**
   * Generates one of the floating rate names of the closed family.
   *
   * @return a generator over every floating rate name the module holds
   */
  val genFloatingRateName: Gen[FloatingRateName] = Gen.oneOf(FloatingRateName.values.toList)

  /** The arbitrary floating rate name. */
  implicit val arbFloatingRateName: Arbitrary[FloatingRateName] = Arbitrary(genFloatingRateName)

  /** The perturbation of floating rate names, by their name. */
  implicit val cogenFloatingRateName: Cogen[FloatingRateName] = cogenBy[FloatingRateName](_.name)

  /**
   * The Ibor indices whose calendars the standard reference data resolves.
   *
   * An observation of a fixing is derived rather than given: the factory resolves the fixing
   * calendar and the two date offsets of the index and computes the dependent dates, so it
   * reports a failure for an index naming a calendar the reference data does not hold. Thirteen
   * of the default calendars of the library have no built-in calendar - the Java library had
   * none for them either - so the indices naming one of those cannot be observed against the
   * standard data, and are filtered out here rather than being drawn and discarded.
   *
   * The filter probes one fixed date, which decides it for every date: an index resolves its
   * calendars or it does not, and that does not depend on the date being observed.
   */
  private val observableIborIndices: List[IborIndex] =
    IborIndex.values.toList.filter(index =>
      IborIndexObservation.of(index, CalendarProbeDate, ReferenceData.standard).isRight)

  /** The overnight indices whose calendars the standard reference data resolves. */
  private val observableOvernightIndices: List[OvernightIndex] =
    OvernightIndex.values.toList.filter(index =>
      OvernightIndexObservation.of(index, CalendarProbeDate, ReferenceData.standard).isRight)

  /** The FX indices whose calendars the standard reference data resolves. */
  private val observableFxIndices: List[FxIndex] =
    FxIndex.values.toList.filter(index =>
      FxIndexObservation.of(index, CalendarProbeDate, ReferenceData.standard).isRight)

  /**
   * Generates an Ibor index that can be observed against the standard reference data.
   *
   * @return a generator over the Ibor indices whose calendars the standard data resolves
   */
  val genObservableIborIndex: Gen[IborIndex] = Gen.oneOf(observableIborIndices)

  /**
   * Generates an overnight index that can be observed against the standard reference data.
   *
   * @return a generator over the overnight indices whose calendars the standard data resolves
   */
  val genObservableOvernightIndex: Gen[OvernightIndex] = Gen.oneOf(observableOvernightIndices)

  /**
   * Generates an FX index that can be observed against the standard reference data.
   *
   * @return a generator over the FX indices whose calendars the standard data resolves
   */
  val genObservableFxIndex: Gen[FxIndex] = Gen.oneOf(observableFxIndices)

  /**
   * Generates an observation of an Ibor index fixing.
   *
   * The observation is built by the one factory that exists for it, which derives the
   * effective date, the maturity date and the year fraction from the index and the fixing date.
   * Nothing else can build one, which is why an inconsistent set of dates is not a value this
   * type holds - and why the document form of an observation is read back through the same
   * derivation.
   *
   * @return a generator of Ibor index observations
   */
  val genIborIndexObservation: Gen[IborIndexObservation] =
    for {
      index <- genObservableIborIndex
      fixingDate <- genLocalDate
      value <- fromEither(
        "IborIndexObservation.of",
        IborIndexObservation.of(index, fixingDate, ReferenceData.standard))
    } yield value

  /** The arbitrary Ibor index observation. */
  implicit val arbIborIndexObservation: Arbitrary[IborIndexObservation] =
    Arbitrary(genIborIndexObservation)

  /** The perturbation of Ibor index observations, by their rendering. */
  implicit val cogenIborIndexObservation: Cogen[IborIndexObservation] =
    cogenBy[IborIndexObservation](_.toString)

  /**
   * Generates an observation of an overnight index fixing.
   *
   * @return a generator of overnight index observations
   */
  val genOvernightIndexObservation: Gen[OvernightIndexObservation] =
    for {
      index <- genObservableOvernightIndex
      fixingDate <- genLocalDate
      value <- fromEither(
        "OvernightIndexObservation.of",
        OvernightIndexObservation.of(index, fixingDate, ReferenceData.standard))
    } yield value

  /** The arbitrary overnight index observation. */
  implicit val arbOvernightIndexObservation: Arbitrary[OvernightIndexObservation] =
    Arbitrary(genOvernightIndexObservation)

  /** The perturbation of overnight index observations, by their rendering. */
  implicit val cogenOvernightIndexObservation: Cogen[OvernightIndexObservation] =
    cogenBy[OvernightIndexObservation](_.toString)

  /**
   * Generates an observation of an FX index fixing.
   *
   * @return a generator of FX index observations
   */
  val genFxIndexObservation: Gen[FxIndexObservation] =
    for {
      index <- genObservableFxIndex
      fixingDate <- genLocalDate
      value <- fromEither(
        "FxIndexObservation.of",
        FxIndexObservation.of(index, fixingDate, ReferenceData.standard))
    } yield value

  /** The arbitrary FX index observation. */
  implicit val arbFxIndexObservation: Arbitrary[FxIndexObservation] =
    Arbitrary(genFxIndexObservation)

  /** The perturbation of FX index observations, by their rendering. */
  implicit val cogenFxIndexObservation: Cogen[FxIndexObservation] =
    cogenBy[FxIndexObservation](_.toString)

  /**
   * Generates an observation of a price index fixing.
   *
   * This one needs no reference data: a price index is fixed for a month rather than on a
   * business day, so there is no calendar to resolve and nothing derived to keep consistent.
   *
   * @return a generator of price index observations
   */
  val genPriceIndexObservation: Gen[PriceIndexObservation] =
    for {
      index <- genPriceIndex
      fixingMonth <- genYearMonth
    } yield PriceIndexObservation.of(index, fixingMonth)

  /** The arbitrary price index observation. */
  implicit val arbPriceIndexObservation: Arbitrary[PriceIndexObservation] =
    Arbitrary(genPriceIndexObservation)

  /** The perturbation of price index observations, by their rendering. */
  implicit val cogenPriceIndexObservation: Cogen[PriceIndexObservation] =
    cogenBy[PriceIndexObservation](_.toString)

  //-------------------------------------------------------------------------
  // The location package.
  //-------------------------------------------------------------------------
  /**
   * The countries named by a constant of the type, in declaration order.
   *
   * The type is an open value rather than a closed family - it accepts any two upper-case
   * letters, as the library being ported does - so it publishes no list of its members. These
   * are the constants a spec is likely to name, which is what makes them worth drawing more
   * often than an arbitrary code.
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
   * Mostly one of the countries a constant names, and occasionally an arbitrary pair of
   * upper-case letters, which the type accepts whether or not it names a country the library
   * knows. Both are worth drawing: the constants are what a spec asserts against, and the open
   * codes are what the validation of the type actually admits.
   *
   * @return a generator of countries
   */
  val genCountry: Gen[Country] = Gen.frequency(
    3 -> Gen.oneOf(namedCountries),
    1 -> (for {
      letters <- Gen.listOfN(2, Gen.alphaUpperChar)
      value <- fromEither("Country.of", Country.of(letters.mkString))
    } yield value))

  /** The arbitrary country. */
  implicit val arbCountry: Arbitrary[Country] = Arbitrary(genCountry)

  /** The perturbation of countries, by their code. */
  implicit val cogenCountry: Cogen[Country] = cogenBy[Country](_.code)

  //-------------------------------------------------------------------------
  // The schedule package.
  //-------------------------------------------------------------------------
  /**
   * Generates a frequency of one, three, six or twelve months.
   *
   * These are the frequencies a schedule is generated at, so this is the generator the
   * schedule-shaped values below draw from: a schedule whose frequency divides its own length
   * is the case worth generating, and an arbitrary frequency rarely does.
   *
   * @return a generator of the four regular month-based frequencies
   */
  val genRegularFrequency: Gen[Frequency] =
    Gen
      .oneOf(1, 3, 6, 12)
      .flatMap(months => fromEither("Frequency.ofMonths", Frequency.ofMonths(months)))

  /**
   * Generates a frequency.
   *
   * Every length is positive, which the factory requires, and the term frequency is drawn as
   * well, since it is the one whose text form is a word rather than a period.
   *
   * @return a generator of frequencies
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

  /** The arbitrary frequency. */
  implicit val arbFrequency: Arbitrary[Frequency] = Arbitrary(genFrequency)

  /** The perturbation of frequencies, by their name. */
  implicit val cogenFrequency: Cogen[Frequency] = cogenBy[Frequency](_.name)

  /**
   * Generates one of the eight stub conventions.
   *
   * @return a generator over every stub convention
   */
  val genStubConvention: Gen[StubConvention] = Gen.oneOf(StubConvention.values.toList)

  /** The arbitrary stub convention. */
  implicit val arbStubConvention: Arbitrary[StubConvention] = Arbitrary(genStubConvention)

  /** The perturbation of stub conventions, by their name. */
  implicit val cogenStubConvention: Cogen[StubConvention] = cogenBy[StubConvention](_.name)

  /**
   * Generates one of the forty-five roll conventions.
   *
   * The family holds the eight standard conventions, the thirty day-of-month conventions and
   * the seven day-of-week conventions, and every one of them is drawn from here because the
   * family publishes them all.
   *
   * @return a generator over every roll convention
   */
  val genRollConvention: Gen[RollConvention] = Gen.oneOf(RollConvention.values.toList)

  /** The arbitrary roll convention. */
  implicit val arbRollConvention: Arbitrary[RollConvention] = Arbitrary(genRollConvention)

  /** The perturbation of roll conventions, by their name. */
  implicit val cogenRollConvention: Cogen[RollConvention] = cogenBy[RollConvention](_.name)

  /**
   * Generates a schedule period.
   *
   * The unadjusted dates are at least ten days apart and each adjusted date is within three
   * days of the unadjusted one it belongs to, so both pairs are strictly in order however the
   * shifts fall - which is what the factory checks, once for each pair.
   *
   * @return a generator of schedule periods
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

  /** The arbitrary schedule period. */
  implicit val arbSchedulePeriod: Arbitrary[SchedulePeriod] = Arbitrary(genSchedulePeriod)

  /** The perturbation of schedule periods, by their rendering. */
  implicit val cogenSchedulePeriod: Cogen[SchedulePeriod] = cogenBy[SchedulePeriod](_.toString)

  /**
   * Generates a schedule of contiguous periods.
   *
   * The periods run end to end at the frequency the schedule reports, which is what a schedule
   * built by generating one looks like. Nothing about the type requires it - the factory checks
   * nothing, because the invariants of a schedule are carried by the types of its three fields
   * - but a schedule of unrelated periods would be a value no generation could produce, and the
   * specs reading periods off a schedule expect the periods to fit together.
   *
   * @return a generator of schedules of up to six periods
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

  /** The arbitrary schedule. */
  implicit val arbSchedule: Arbitrary[Schedule] = Arbitrary(genSchedule)

  /** The perturbation of schedules, by their rendering. */
  implicit val cogenSchedule: Cogen[Schedule] = cogenBy[Schedule](_.toString)

  /**
   * Generates a periodic schedule definition.
   *
   * The eleven fields are drawn as one coherent shape rather than independently, because seven
   * order invariants hold between the five date-bearing ones and independent draws would fail
   * most of them. The shape is the one the generation cases of the Java suite use: a start
   * date, an end date a whole number of periods later, and where a regular period boundary is
   * drawn it falls on one of those period boundaries; an overriding start date, where one is
   * drawn, falls before the start date and therefore before every boundary.
   *
   * @return a generator of periodic schedule definitions
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

  /** The arbitrary periodic schedule definition. */
  implicit val arbPeriodicSchedule: Arbitrary[PeriodicSchedule] = Arbitrary(genPeriodicSchedule)

  /** The perturbation of periodic schedule definitions, by their rendering. */
  implicit val cogenPeriodicSchedule: Cogen[PeriodicSchedule] =
    cogenBy[PeriodicSchedule](_.toString)

  //-------------------------------------------------------------------------
  // The value package.
  //-------------------------------------------------------------------------
  /**
   * Generates one of the four value adjustment types.
   *
   * @return a generator over every value adjustment type
   */
  val genValueAdjustmentType: Gen[ValueAdjustmentType] =
    Gen.oneOf(ValueAdjustmentType.values.toList)

  /** The arbitrary value adjustment type. */
  implicit val arbValueAdjustmentType: Arbitrary[ValueAdjustmentType] =
    Arbitrary(genValueAdjustmentType)

  /** The perturbation of value adjustment types, by their name. */
  implicit val cogenValueAdjustmentType: Cogen[ValueAdjustmentType] =
    cogenBy[ValueAdjustmentType](_.name)

  /**
   * Generates a value adjustment that does not replace the value it is applied to.
   *
   * A sequence of steps rejects a replacing adjustment - every step of a sequence applies the
   * same adjustment, and replacing a value repeatedly would leave the sequence with no effect
   * beyond its first step - so the sequence generator draws from here.
   *
   * @return a generator of delta and multiplier adjustments
   */
  val genNonReplacingValueAdjustment: Gen[ValueAdjustment] = Gen.oneOf(
    genDouble.map(amount => ValueAdjustment.ofDeltaAmount(amount)),
    genDouble.map(multiplier => ValueAdjustment.ofDeltaMultiplier(multiplier)),
    genDouble.map(multiplier => ValueAdjustment.ofMultiplier(multiplier)))

  /**
   * Generates a value adjustment.
   *
   * The value carried is drawn from the whole range of `double`, edge values included: the type
   * holds whatever number it is given, compares it by bit pattern and carries it in a document
   * through the tagged form the port's double policy defines.
   *
   * @return a generator of value adjustments of all four types
   */
  val genValueAdjustment: Gen[ValueAdjustment] = Gen.frequency(
    1 -> genDouble.map(replacement => ValueAdjustment.ofReplace(replacement)),
    3 -> genNonReplacingValueAdjustment)

  /** The arbitrary value adjustment. */
  implicit val arbValueAdjustment: Arbitrary[ValueAdjustment] = Arbitrary(genValueAdjustment)

  /** The perturbation of value adjustments, by their rendering. */
  implicit val cogenValueAdjustment: Cogen[ValueAdjustment] =
    cogenBy[ValueAdjustment](_.toString)

  /**
   * Generates a value with its derivatives.
   *
   * Both the value and the derivatives are drawn with the IEEE-754 edge values, because the
   * type rejects nothing and its equality compares bit patterns - a `NaN` derivative is a value
   * it holds.
   *
   * @return a generator of values with derivatives
   */
  val genValueDerivatives: Gen[ValueDerivatives] =
    for {
      value <- genDouble
      derivatives <- genDoubleArray
    } yield ValueDerivatives.of(value, derivatives)

  /** The arbitrary value with derivatives. */
  implicit val arbValueDerivatives: Arbitrary[ValueDerivatives] = Arbitrary(genValueDerivatives)

  /** The perturbation of values with derivatives, by their rendering. */
  implicit val cogenValueDerivatives: Cogen[ValueDerivatives] =
    cogenBy[ValueDerivatives](_.toString)

  /**
   * Generates a half-up rounding convention.
   *
   * Both factories are drawn: the one naming a number of decimal places, and the one naming a
   * fraction of the last place as well.
   *
   * @return a generator of half-up roundings
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

  /** The arbitrary half-up rounding. */
  implicit val arbHalfUp: Arbitrary[HalfUp] = Arbitrary(genHalfUpRounding)

  /**
   * Generates a rounding convention.
   *
   * @return a generator of the rounding that does nothing and of half-up roundings
   */
  val genRounding: Gen[Rounding] =
    Gen.frequency(1 -> Gen.const[Rounding](NoRounding), 3 -> genHalfUpRounding)

  /** The arbitrary rounding convention. */
  implicit val arbRounding: Arbitrary[Rounding] = Arbitrary(genRounding)

  /** The perturbation of rounding conventions, by their rendering. */
  implicit val cogenRounding: Cogen[Rounding] = cogenBy[Rounding](_.toString)

  /**
   * Generates a step changing a value.
   *
   * A step is positioned either by the index of the schedule period it falls at or by a date,
   * and exactly one of the two: the factory rejects a step naming both and a step naming
   * neither, so the two shapes are drawn from separate branches and never mixed. The index is
   * one or greater, which the factory also requires.
   *
   * @return a generator of steps positioned by index and of steps positioned by date
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

  /** The arbitrary step changing a value. */
  implicit val arbValueStep: Arbitrary[ValueStep] = Arbitrary(genValueStep)

  /** The perturbation of steps, by their rendering. */
  implicit val cogenValueStep: Cogen[ValueStep] = cogenBy[ValueStep](_.toString)

  /**
   * Generates a sequence of steps changing a value.
   *
   * The first date is on or before the last, which the factory requires, and the adjustment is
   * never a replacing one, which it also requires.
   *
   * @return a generator of step sequences
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

  /** The arbitrary sequence of steps. */
  implicit val arbValueStepSequence: Arbitrary[ValueStepSequence] =
    Arbitrary(genValueStepSequence)

  /** The perturbation of step sequences, by their rendering. */
  implicit val cogenValueStepSequence: Cogen[ValueStepSequence] =
    cogenBy[ValueStepSequence](_.toString)

  /**
   * Generates a schedule of values.
   *
   * Construction is total - a schedule of values places no constraint between its initial
   * value, its steps and its sequence, because whether a step lines up with a period boundary
   * is a question about the schedule it is resolved against - so all three are drawn freely.
   *
   * @return a generator of value schedules
   */
  val genValueSchedule: Gen[ValueSchedule] =
    for {
      initialValue <- genDouble
      stepCount <- Gen.choose(0, MaxSteps)
      steps <- Gen.listOfN(stepCount, genValueStep)
      stepSequence <- Gen.option(genValueStepSequence)
    } yield ValueSchedule.of(initialValue, steps, stepSequence)

  /** The arbitrary schedule of values. */
  implicit val arbValueSchedule: Arbitrary[ValueSchedule] = Arbitrary(genValueSchedule)

  /** The perturbation of value schedules, by their rendering. */
  implicit val cogenValueSchedule: Cogen[ValueSchedule] = cogenBy[ValueSchedule](_.toString)

  //-------------------------------------------------------------------------
  // The currency package, continued: the adjustable payment, which needs an adjustable date.
  //-------------------------------------------------------------------------
  /**
   * Generates an adjustable payment.
   *
   * @return a generator of adjustable payments
   */
  val genAdjustablePayment: Gen[AdjustablePayment] =
    for {
      amount <- genCurrencyAmount
      date <- genAdjustableDate
    } yield AdjustablePayment.of(amount, date)

  /** The arbitrary adjustable payment. */
  implicit val arbAdjustablePayment: Arbitrary[AdjustablePayment] = Arbitrary(genAdjustablePayment)

  /** The perturbation of adjustable payments, by their rendering. */
  implicit val cogenAdjustablePayment: Cogen[AdjustablePayment] =
    cogenBy[AdjustablePayment](_.toString)
}
