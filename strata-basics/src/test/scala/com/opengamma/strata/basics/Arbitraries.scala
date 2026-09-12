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
 * Three types keep '''two''' generators for that reason, following the split `strata-collect`
 * makes between its finite `arbDoubleArray` and its edge-bearing `genDoubleArray`: the implicit
 * arbitrary holds the well-behaved values the arithmetic properties are written over, and a named
 * generator beside it reaches the edges the equality, hashing, rendering and document form turn
 * on. Those pairs are `genFxRate` with [[genEdgeFxRate]], `genFxMatrix` with [[genEdgeFxMatrix]],
 * and the two array generators re-exported above. The edge generators are passed by hand to the
 * audits that need them, which is what keeps them out of the properties that do not.
 *
 * Every type generated here carries a '''`Shrink`''' as well, declared in the shrinking section
 * at the foot of this object, so a property that fails reports the smallest counterexample it can
 * reach rather than the value it happened to draw - without one ScalaCheck falls back on
 * `shrinkAny`, which offers no candidate at all and leaves a failing schedule of six periods
 * reported with all six. Those instances follow the three rules `strata-collect`'s generator
 * source states and implements - a candidate built by the same validated factory, a candidate
 * keeping the invariant its generator promises, and a candidate strictly smaller under a measure
 * the instance names - and each one states its own measure and the floor it terminates at. The
 * one generated type without a shrinking is [[FailureReason]], whose family and generator belong
 * to `strata-collect`: an instance for it belongs beside that generator, where the six shrinkings
 * re-exported above are declared.
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

  /** The shrinking of the sample named family of `strata-collect`. */
  implicit val shrinkSampleNamed: Shrink[SampleNamed] = CollectArbitraries.shrinkSampleNamed

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
   * Generates a run of amounts in one currency, of any length the type admits.
   *
   * The length starts at '''zero'''. The direct factory of this type is total - a currency and an
   * array of values always describe a run of amounts, and an array of no values is one - so a run
   * of length zero is a value the type holds, a document carries and every operation over a run
   * has to answer for; only the factory that derives the currency from a collection of amounts
   * needs one amount to read it from, and that factory is not this one. Leaving zero out of the
   * range would have left the empty run outside every property written over this generator, which
   * is what `CurrencyAmountArraySpec` now covers by example as well.
   *
   * @return a generator of runs of single-currency amounts, one of them occasionally empty
   */
  val genCurrencyAmountArray: Gen[CurrencyAmountArray] =
    for {
      currency <- genCurrency
      length <- Gen.choose(0, MaxRunLength)
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
   * Generates a rate a [[FxRate]] accepts, including the two non-finite ones.
   *
   * The check the factory applies is `!(rate <= 0.0)`, which is documented to '''pass''' a value
   * that is not a number: `NaN <= 0.0` is false, so the negation holds. A rate of `+∞` passes for
   * the plain reason that it is greater than zero. The three values the check rejects - both
   * zeroes and `-∞` - are therefore deliberately absent, since a generator producing one would
   * fail the draw rather than produce an edge value.
   *
   * @return a generator of positive finite rates, of `+∞` and of `NaN`
   */
  val genEdgeRate: Gen[Double] = Gen.frequency(
    4 -> genPositiveRate,
    1 -> Gen.const(Double.PositiveInfinity),
    1 -> Gen.const(Double.NaN))

  /**
   * Generates an exchange rate whose rate reaches the edges of the IEEE-754 values it admits.
   *
   * This is [[genFxRate]] over [[genEdgeRate]], and it exists for the same reason
   * `genDoubleArray` exists beside the finite `arbDoubleArray` of `strata-collect`: the equality
   * of this type compares the rate by its bit pattern, so the values that decide it are `+∞` and
   * `NaN`, and no generator of ordinary rates will produce one. The finite generator stays the
   * implicit arbitrary, because the arithmetic properties of the type - conversion, the cross
   * rate, the reciprocal - are written over rates whose products are numbers; this one is passed
   * by hand to the audits that are about equality, hashing, rendering and the document form.
   *
   * The identity-pair case is drawn here as well, at the same weight [[genFxRate]] draws it, so
   * the two generators cover the same shapes of value and differ only in the rates they reach.
   *
   * @return a generator of exchange rates, one draw in three of them non-finite
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

  /**
   * Generates an off-diagonal entry of an edge-bearing matrix.
   *
   * [[FxMatrix.fromMatrix]] checks three things and no more - the currencies are distinct, the
   * matrix is square and of their number, and the diagonal is one - so every `Double` there is
   * may stand off the diagonal: the two infinities, `NaN` and both signed zeroes included. Those
   * are the entries the equality of the type turns on and the entries its document form has to
   * tag, so they are drawn here at a third of the draws.
   *
   * @return a generator of matrix entries, finite in two draws of three
   */
  private val genEdgeMatrixRate: Gen[Double] = Gen.frequency(
    4 -> genPositiveRate,
    1 -> genEdgeDouble,
    1 -> Gen.oneOf(0.0d, -0.0d))

  /**
   * Builds the rates of a matrix of the size given, with a unit diagonal.
   *
   * The diagonal is one, which [[FxMatrix.fromMatrix]] requires of every matrix; each off-diagonal
   * entry is taken from the entries drawn, read by position, and '''independently of the entry
   * opposite it''', so a generated matrix is generally not reciprocal. That is a state the type
   * holds: a matrix built by placing rates one at a time can be updated in one direction only, as
   * `FxMatrixSpec` pins, so a generator that forced reciprocity would cover less than the type
   * admits.
   *
   * @param size  the number of currencies, which is the number of rows and of columns
   * @param entries  the entries to read the off-diagonal positions from, `size * size` of them
   * @return the square matrix of those entries with a unit diagonal
   */
  private def edgeRatesOf(size: Int, entries: List[Double]): DoubleMatrix =
    DoubleMatrix.tabulate(size, size)((row, column) =>
      if (row == column) 1.0d else entries(row * size + column))

  /** The currencies of the matrix that pins the edge states, in the order they occupy. */
  private val PinnedEdgeCurrencies: Vector[Currency] =
    Vector(Currency.GBP, Currency.USD, Currency.EUR)

  /**
   * The rates of the matrix that pins the edge states, in the orientation of [[FxMatrix]].
   *
   * Every state a generated matrix reaches only by chance is present here at once, so that a
   * property run over this generator sees all of them however the draws fall: a '''non-reciprocal
   * pair''' - 2.0 one way and 5.0 the other, where reciprocity would require 0.5 - a
   * '''negative zero''', a '''not-a-number''' entry and '''both infinities'''. The diagonal is one,
   * which is the single structural requirement.
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
   * This is [[genFxMatrix]] over [[genEdgeMatrixRate]], and it stands to that generator as
   * `genDoubleMatrix` stands to the finite `arbDoubleMatrix` of `strata-collect`: the equality of
   * this type compares its entries by bit pattern and its document form tags the three values
   * JSON has no number for, so the values that decide both have to be generated. The finite
   * generator stays the implicit arbitrary, because the arithmetic properties of the type -
   * conversion, triangulation, merging - are written over rates whose products are numbers.
   *
   * Every matrix is built through [[FxMatrix.fromMatrix]], the factory a decoded document arrives
   * at, rather than by placing rates one at a time: placing a rate computes the reciprocal of it
   * for the opposite position, which is exactly what a non-reciprocal matrix does not hold. One
   * draw in eight is the pinned matrix above, so the non-reciprocal pair, the signed zero, the
   * not-a-number entry and the two infinities are all reached within a handful of draws; the
   * empty matrix is drawn as well, as it is by [[genFxMatrix]], since it is a value the type holds.
   *
   * @return a generator of matrices holding up to four currencies and entries at the IEEE edges
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
   * Generates a business day count over one of the built-in calendars, at the type of the member.
   *
   * This is the one day count carrying data, and the one with a structural document form
   * rather than a name, so it is generated separately and drawn alongside the standard members
   * by [[genDayCount]]. The calendar is built in, so the name form of the document - which
   * resolves a calendar by name against the built-in set - reads it back as well.
   *
   * The value is produced by [[DayCount.ofBus252]], the one factory of the type, which answers at
   * the type of the '''family'''; the type test below narrows it to the member without bypassing
   * that factory. It cannot fail - the factory returns `new Bus252(calendar)` and nothing else -
   * and the branch that would report it fails the draw with a label rather than being assumed
   * away, exactly as [[fromEither]] does for a factory that rejects its input.
   *
   * The narrow type is generated because the member carries `Hash` and `Show` instances of its
   * own, which the invariance of those typeclasses requires and which the law suites check at
   * this type; a `Gen[DayCount]` could not feed them.
   *
   * @return a generator of `Bus/252` day counts, typed as the member
   */
  val genCalendarBearingDayCount: Gen[DayCount.Bus252] =
    Gen.oneOf(builtInCalendars).flatMap { calendar =>
      DayCount.ofBus252(calendar) match {
        case bus252: DayCount.Bus252 => Gen.const(bus252)
        case other =>
          Gen.fail[DayCount.Bus252].label(s"DayCount.ofBus252 produced $other rather than a Bus252")
      }
    }

  /** The arbitrary `Bus/252` day count, typed as the member of the family that carries data. */
  implicit val arbCalendarBearingDayCount: Arbitrary[DayCount.Bus252] =
    Arbitrary(genCalendarBearingDayCount)

  /** The perturbation of `Bus/252` day counts, by their name, which carries their calendar. */
  implicit val cogenCalendarBearingDayCount: Cogen[DayCount.Bus252] =
    cogenBy[DayCount.Bus252](_.name)

  /**
   * Generates a business day count over one of the built-in calendars.
   *
   * This is [[genCalendarBearingDayCount]] widened to the type of the family, which is what
   * [[genDayCount]] and the specs drawing a day count of either kind need. It is expressed in
   * terms of that generator rather than drawing a calendar a second time, so the two cannot drift
   * apart in the set of calendars they cover.
   *
   * @return a generator of `Bus/252` day counts, typed as the family
   */
  val genBus252DayCount: Gen[DayCount] =
    genCalendarBearingDayCount.map(dayCount => dayCount: DayCount)

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
   * The perturbation of custom calendars, by their name.
   *
   * The same rendering as the family's perturbation above, and sound for the same reason: the
   * `equals` of this member compares the identifier alone, and its name ''is'' that identifier.
   * It is declared at the type of the member because the law suites of this member ask for a
   * `Cogen[ImmutableHolidayCalendar]`, which a `Cogen[HolidayCalendar]` is not - `Cogen` is
   * invariant, as the `Hash` and `Show` of the member are.
   */
  implicit val cogenImmutableHolidayCalendar: Cogen[ImmutableHolidayCalendar] =
    cogenBy[ImmutableHolidayCalendar](_.name)

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
   * The perturbation of half-up roundings, by their rendering.
   *
   * The rendering names both fields of the value - the number of decimal places always, and the
   * fraction where there is one - which are exactly the two fields its equality compares, so it
   * agrees with that equality. It is declared at the type of the member as well as at the type of
   * the family because the law suites of this member ask for a `Cogen[HalfUp]`, and a
   * `Cogen[Rounding]` is not one: `Cogen` is invariant in its type, exactly as the `Hash` and
   * `Show` of the member are.
   */
  implicit val cogenHalfUp: Cogen[HalfUp] = cogenBy[HalfUp](_.toString)

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
   * The initial value, the steps and the sequence are drawn independently, because a schedule of
   * values places almost no constraint between them - whether a step lines up with a period
   * boundary is a question about the schedule it is resolved against, not about the definition.
   * The one constraint it does place is that two steps must not name the same position with
   * different adjustments, which the drawn steps are thinned to satisfy.
   *
   * @return a generator of value schedules
   */
  val genValueSchedule: Gen[ValueSchedule] =
    for {
      initialValue <- genDouble
      stepCount <- Gen.choose(0, MaxSteps)
      drawn <- Gen.listOfN(stepCount, genValueStep)
      // the one condition construction decides is that no position is named twice with different
      // adjustments, so the drawn steps are thinned to one per position - keeping the first, so
      // the list stays in the order it was drawn in - and the factory then accepts every draw
      steps = drawn.distinctBy(step => step.periodIndex.toLeft(step.date))
      stepSequence <- Gen.option(genValueStepSequence)
      value <- fromEither(
        "ValueSchedule.of",
        ValueSchedule.of(initialValue, steps, stepSequence))
    } yield value

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

  //-------------------------------------------------------------------------
  // The shrinkings.
  //
  // A `Shrink` is what ScalaCheck minimises a failing case with. Without one it falls back on
  // `shrinkAny`, which offers no candidate at all, so a property that fails on a schedule of six
  // periods reports all six, and one that fails on a matrix of four currencies reports all
  // sixteen rates. Every instance below obeys the three rules `strata-collect`'s generator source
  // states and implements, and each rule is there because breaking it would turn minimising a
  // real failure into reporting a spurious one:
  //
  //   - a candidate is built by the '''same validated factory the generator uses''', and an
  //     outcome that factory rejects contributes no candidate rather than being unwrapped by
  //     force - which is what `candidateOf` below is for, exactly as `fromEither` is for the
  //     generators;
  //   - a candidate '''keeps the invariant its generator promises'''. A member of a closed family
  //     shrinks only to another member of that family; a calendar-bearing day count shrinks only
  //     to another calendar-bearing one, never to a standard member, because a spec drawing from
  //     `genBus252DayCount` is about that kind of day count; a month-based adjustment keeps a
  //     month-based period; a run of multi-currency amounts keeps its runs the same length as one
  //     another; a custom calendar keeps its own identifier, so its document form stays the
  //     structural one; and the dates of a schedule period, of a run of dates and of a step
  //     sequence stay in the order their factories require. A candidate that broke one of those
  //     would be minimised into an input the operation under test rejects, and the minimised case
  //     would fail for a reason the original never had;
  //   - every candidate is '''strictly smaller''' than its input under a measure named in the
  //     scaladoc of the instance, and every measure is a non-negative whole number, so repeated
  //     shrinking terminates at a value with no candidates - the documented floor of the type.
  //     The measures are built from five primitive ones: the position of a member in its family's
  //     declaration order, the number of fields not already at their floor (positive zero for a
  //     number, the first date or month of the generated window for a date, one character for a
  //     text, none for an optional field), the size of a collection, the magnitude of a whole
  //     number, and the number of units a period holds in its own unit.
  //-------------------------------------------------------------------------
  /**
   * Turns the outcome of a validated factory into a shrink candidate.
   *
   * This is [[fromEither]] in the shape shrinking needs. A `Right` contributes the one candidate
   * it holds; a `Left` contributes none, so an outcome the factory rejected is dropped rather
   * than forced into a value. Every call site below hands its factory a smaller version of parts
   * the input already carried, so the dropping branch is reached only where a reduction happens
   * to break a constraint of the type - a pairing rule between two fields, say - and dropping it
   * there is exactly right: the candidate would not have been a value of the type.
   *
   * @param outcome  the outcome of a validated factory
   * @tparam E  the type describing why the factory rejected its input
   * @tparam A  the type of the value produced
   * @return the single candidate, or no candidate at all
   */
  private def candidateOf[E, A](outcome: Either[E, A]): LazyList[A] =
    outcome match {
      case Right(value) => LazyList(value)
      case Left(_) => LazyList.empty
    }

  /** The bit pattern of positive zero, which is the floor every generated number shrinks to. */
  private val PositiveZeroBits: Long = java.lang.Double.doubleToLongBits(0.0d)

  /**
   * Returns whether a number is already the positive zero shrinking simplifies towards.
   *
   * The comparison is on bit patterns rather than with `==`, for the reason the equality of every
   * double-bearing type of this port compares bit patterns: `-0.0 == 0.0` holds while the two are
   * different values here, and `NaN == NaN` fails while it is one value. This is what makes the
   * measure fall by exactly one for each number a shrinking simplifies.
   *
   * @param value  the number to test
   * @return true where the number is positive zero
   */
  private def isSimplifiedNumber(value: Double): Boolean =
    java.lang.Double.doubleToLongBits(value) == PositiveZeroBits

  /**
   * The numbers a generated number shrinks to: positive zero, and nothing else.
   *
   * One step rather than a sequence of halvings, which is the shape `strata-collect` uses for the
   * elements of an array and for the same reason: the numbers these types carry are compared by
   * bit pattern, so a property that depends on a particular number is not made easier to read by
   * a number half the size, while a property that depends on none at all is reported with zeroes.
   *
   * @param value  the number to simplify
   * @return positive zero, or nothing where the number is already that
   */
  private def simplerNumbers(value: Double): LazyList[Double] =
    if (isSimplifiedNumber(value)) LazyList.empty else LazyList(0.0d)

  /** The first date of the window every generated date falls in, and the floor dates shrink to. */
  private val FirstDate: LocalDate = LocalDate.of(MinYear, 1, 1)

  /** The first month of the window every generated month falls in. */
  private val FirstYearMonth: YearMonth = YearMonth.of(MinYear, 1)

  /**
   * The dates a generated date shrinks to: the first date of the generated window.
   *
   * Every date this file produces falls in that window, so the floor is inside it and a candidate
   * is a date the generators themselves could have drawn.
   *
   * @param date  the date to simplify
   * @return the first date of the window, or nothing where the date is already that
   */
  private def simplerDates(date: LocalDate): LazyList[LocalDate] =
    if (date == FirstDate) LazyList.empty else LazyList(FirstDate)

  /**
   * The months a generated month shrinks to: the first month of the generated window.
   *
   * @param month  the month to simplify
   * @return the first month of the window, or nothing where the month is already that
   */
  private def simplerMonths(month: YearMonth): LazyList[YearMonth] =
    if (month == FirstYearMonth) LazyList.empty else LazyList(FirstYearMonth)

  /**
   * The whole numbers a generated whole number shrinks to, towards zero.
   *
   * Zero first, so a property that fails for every count reports zero immediately, then half the
   * magnitude, which is what keeps the path short where zero is not itself a counterexample, then
   * the magnitude of a negative number, which drops the sign. Every candidate is strictly nearer
   * zero than the input, so the magnitude falls at every step and zero has no candidates.
   *
   * @param value  the number to simplify
   * @return the candidates, nearest zero first
   */
  private def simplerIntegers(value: Int): LazyList[Int] =
    LazyList(0, value / 2, math.abs(value)).distinct.filter(candidate =>
      math.abs(candidate) < math.abs(value))

  /**
   * The positive whole numbers a generated one-based position shrinks to, towards one.
   *
   * The floor is one rather than zero, because the factories that take a position - the index of
   * a schedule period a step falls at - reject zero, so a candidate of zero would be dropped by
   * the factory and the shrinking would stall one step early.
   *
   * @param value  the position to simplify
   * @return the candidates, nearest one first
   */
  private def simplerPositions(value: Int): LazyList[Int] =
    LazyList(1, value / 2).distinct.filter(candidate => candidate >= 1 && candidate < value)

  /**
   * The texts a generated text shrinks to: its first character.
   *
   * Every text this file generates is a scheme, a value or an identifier of at least one
   * character, and each of the factories taking one accepts a text of one character, so the floor
   * is a value of the type rather than a candidate the factory would reject.
   *
   * @param text  the text to simplify
   * @return the first character of the text, or nothing where the text is one character already
   */
  private def shorterTexts(text: String): LazyList[String] =
    if (text.length > 1) LazyList(text.take(1)) else LazyList.empty

  /**
   * The members of a closed family a member shrinks to: the ones declared before it.
   *
   * This is the shape `strata-collect` uses for its sample family, and it carries over to the
   * fifteen closed families of this module: the candidates of a member are the members that
   * precede it in the family's own declaration order, so the measure is the position of the
   * member in that order and the floor is the first member declared, which has none.
   *
   * A value that is not in the list - which a sealed family makes impossible - is reported as
   * absent by `indexOf` and offers no candidate at all, rather than offering every member as
   * `take` on a negative index would.
   *
   * @param members  the members of the family, in declaration order
   * @param value  the member to simplify
   * @tparam A  the type of the family
   * @return the members declared before the one given
   */
  private def earlierMembers[A](members: List[A])(value: A): LazyList[A] =
    LazyList.from(members.take(math.max(members.indexOf(value), 0)))

  /**
   * The periods a generated period shrinks to, reduced in the unit the period is stated in.
   *
   * A period of months shrinks to a period of fewer months, a period of years to fewer years and
   * a period of days to fewer days - and a period of a whole number of weeks, which is a period
   * of a multiple of seven days, to fewer whole weeks. The unit is never changed, because the
   * month-based addition conventions reject a period holding days and the canonical name of a
   * tenor or a frequency is decided by the unit it is stated in, so a candidate in another unit
   * would be a value of a different kind from the one that failed.
   *
   * The measure is the number of units the period holds, which falls at every step, and the floor
   * is one unit of whichever kind the period is - a period of zero is rejected by every factory
   * that takes one here.
   *
   * @param period  the period to simplify
   * @return the candidates, the smallest first
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
   * The measure is the length of the scheme plus the length of the value, and both candidates cut
   * one of the two to a single character, so the measure falls at every step and the floor is an
   * identifier of one character in each part, which has none. Both candidates are built by
   * [[StandardId.of]], the factory the generator uses, so a candidate is checked against the
   * scheme and value rules rather than assumed to satisfy them.
   *
   * @return the shrinking of standard identifiers
   */
  implicit val shrinkStandardId: Shrink[StandardId] = Shrink.withLazyList(standardIdCandidates)

  /** The candidates an identifier shrinks to, the shorter scheme before the shorter value. */
  private def standardIdCandidates(identifier: StandardId): LazyList[StandardId] = {
    val schemes = shorterTexts(identifier.scheme)
      .flatMap(scheme => candidateOf(StandardId.of(scheme, identifier.value)))
    val values = shorterTexts(identifier.value)
      .flatMap(value => candidateOf(StandardId.of(identifier.scheme, value)))
    (schemes #::: values).distinct.filterNot(candidate => candidate == identifier)
  }

  /**
   * Shrinks a calculation target towards the one carrying zero.
   *
   * The measure is the magnitude of the number the target carries, which [[simplerIntegers]]
   * reduces, and the floor is the target carrying zero.
   *
   * @return the shrinking of test targets
   */
  implicit val shrinkTestTarget: Shrink[TestTarget] = Shrink.withLazyList(testTargetCandidates)

  /** The candidates a test target shrinks to, nearest zero first. */
  private def testTargetCandidates(target: TestTarget): LazyList[TestTarget] =
    simplerIntegers(target.value).map(value => TestTarget(value))

  /**
   * Shrinks a list of calculation targets by dropping its trailing target.
   *
   * The measure is the number of targets and the floor is the empty list, which the type accepts
   * and the generator draws, so shrinking is free to reach it: unlike an array of doubles, whose
   * operations are undefined when it is empty, a list of targets holds nothing that depends on
   * there being an element.
   *
   * @return the shrinking of lists of calculation targets
   */
  implicit val shrinkCalculationTargetList: Shrink[CalculationTargetList] =
    Shrink.withLazyList(calculationTargetListCandidates)

  /** The candidates a list of targets shrinks to, which is the list without its last element. */
  private def calculationTargetListCandidates(
      list: CalculationTargetList): LazyList[CalculationTargetList] =

    if (list.targets.isEmpty) LazyList.empty
    else LazyList(CalculationTargetList.of(list.targets.init))

  //-------------------------------------------------------------------------
  // The shrinkings of the currency package.
  //-------------------------------------------------------------------------
  /**
   * Shrinks a currency to the currencies declared before it in the closed family.
   *
   * The measure is the position of the currency in [[Currency.values]] and the floor is the first
   * currency the family declares.
   *
   * @return the shrinking of currencies
   */
  implicit val shrinkCurrency: Shrink[Currency] =
    Shrink.withLazyList(earlierMembers(Currency.values.toList))

  /**
   * Shrinks a currency pair by moving each of its currencies earlier in the family.
   *
   * The measure is the position of the base plus the position of the counter, and every candidate
   * moves one of the two earlier, so the sum falls at every step. The floor is a pair of the two
   * first currencies of the family.
   *
   * '''A pair of two different currencies never shrinks to a pair of one currency twice, and a
   * pair of one currency twice never shrinks to a pair of two.''' The rate-bearing types need a
   * pair of distinct currencies - [[FxRate.of]] accepts a pair of identical ones only at a rate
   * of exactly one - so a candidate that collapsed the two would be minimised into an input those
   * types reject, and a candidate that split them would be a value of the other kind.
   *
   * @return the shrinking of currency pairs
   */
  implicit val shrinkCurrencyPair: Shrink[CurrencyPair] =
    Shrink.withLazyList(currencyPairCandidates)

  /** The candidates a currency pair shrinks to, the earlier base before the earlier counter. */
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
   * zero, and the floor is zero of the first currency the family declares. Both candidates go
   * through [[CurrencyAmount.of]], which rejects a value that is not a number and normalises a
   * negative zero, so a candidate is an amount on exactly the terms a generated one is.
   *
   * @return the shrinking of currency amounts
   */
  implicit val shrinkCurrencyAmount: Shrink[CurrencyAmount] =
    Shrink.withLazyList(currencyAmountCandidates)

  /** The candidates an amount shrinks to, the zero amount before the earlier currencies. */
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
   * The measure is the position of the currency plus one where the amount is not already zero,
   * and the floor is zero of the first currency. The candidates are built by the total
   * [[Money.of]] overload taking a decimal, which rounds to the minor units of the currency it is
   * given - and zero and a rounded amount are unchanged by that rounding, so no candidate is the
   * value it came from by a different route.
   *
   * @return the shrinking of money values
   */
  implicit val shrinkMoney: Shrink[Money] = Shrink.withLazyList(moneyCandidates)

  /** The candidates a money value shrinks to, the zero amount before the earlier currencies. */
  private def moneyCandidates(money: Money): LazyList[Money] = {
    val zeroed =
      if (money.amount.isZero) LazyList.empty else LazyList(Money.of(money.currency, Decimal.ZERO))
    val currencies = earlierMembers(Currency.values.toList)(money.currency)
      .map(currency => Money.of(currency, money.amount))
    (zeroed #::: currencies).distinct.filterNot(candidate => candidate == money)
  }

  /**
   * Shrinks a big money value towards zero and towards the first currency of the family.
   *
   * The measure and the floor are those of [[shrinkMoney]]; this type differs only in the scale
   * it rounds to, which is twelve places for every currency rather than the currency's own.
   *
   * @return the shrinking of big money values
   */
  implicit val shrinkBigMoney: Shrink[BigMoney] = Shrink.withLazyList(bigMoneyCandidates)

  /** The candidates a big money value shrinks to, the zero amount first. */
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
   * the generator draws, so shrinking may reach it. Every candidate is built by the map overload
   * of [[MultiCurrencyAmount.of]], so the rejection of a duplicate currency is checked rather
   * than assumed; a map keyed by currency cannot hold one twice, so that branch is never taken.
   *
   * @return the shrinking of multi-currency amounts
   */
  implicit val shrinkMultiCurrencyAmount: Shrink[MultiCurrencyAmount] =
    Shrink.withLazyList(multiCurrencyAmountCandidates)

  /** The candidates a multi-currency amount shrinks to, the dropped currencies first. */
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

  /** Copies an array of values with the element at the index replaced by positive zero. */
  private def zeroedElement(values: DoubleArray, index: Int): DoubleArray =
    DoubleArray.tabulate(values.size)(position =>
      if (position == index) 0.0d else values.get(position))

  /**
   * Shrinks a run of amounts by shortening it, by zeroing an element and by moving its currency.
   *
   * The measure is the length of the run plus the number of elements that are not already positive
   * zero plus the position of the currency in the family, and the floor is the '''empty''' run of
   * the first currency. An empty run is reachable here, unlike in the array shrinking of
   * `strata-collect`, because the factory of this type is total and the generator draws a run of
   * length zero: no operation of the type is undefined on one.
   *
   * @return the shrinking of runs of single-currency amounts
   */
  implicit val shrinkCurrencyAmountArray: Shrink[CurrencyAmountArray] =
    Shrink.withLazyList(currencyAmountArrayCandidates)

  /** The candidates a run of amounts shrinks to, the shorter run first. */
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
   * The measure is the number of currencies plus the length of the runs plus the number of
   * elements that are not already positive zero. The floor is one currency holding one zero
   * element: the currencies stop at one and the length stops at one, because the factory requires
   * every run to be the same length and the generator promises at least one of each, and a
   * candidate that dropped the last currency or emptied the runs would be minimised into a value
   * the element-wise operations of the type - which report a caller supplying runs of different
   * sizes - were never given. Every run is shortened together for that same reason.
   *
   * @return the shrinking of runs of multi-currency amounts
   */
  implicit val shrinkMultiCurrencyAmountArray: Shrink[MultiCurrencyAmountArray] =
    Shrink.withLazyList(multiCurrencyAmountArrayCandidates)

  /** The candidates a run of multi-currency amounts shrinks to, the dropped currencies first. */
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
   * The measure is the measure of the amount, as [[shrinkCurrencyAmount]] defines it, plus one
   * where the date is not already the first date of the generated window. The floor is a payment
   * of zero of the first currency on that date. The factory is total, so no candidate is dropped.
   *
   * @return the shrinking of payments
   */
  implicit val shrinkPayment: Shrink[Payment] = Shrink.withLazyList(paymentCandidates)

  /** The candidates a payment shrinks to, the simpler amounts before the earlier date. */
  private def paymentCandidates(payment: Payment): LazyList[Payment] = {
    val amounts = currencyAmountCandidates(payment.value)
      .map(amount => Payment.of(amount, payment.date))
    val dates = simplerDates(payment.date).map(date => Payment.of(payment.value, date))
    (amounts #::: dates).distinct.filterNot(candidate => candidate == payment)
  }

  /**
   * Shrinks an exchange rate towards a rate of one and towards the earlier currencies.
   *
   * The measure is one where the rate is not already exactly one, plus the measure of the pair as
   * [[shrinkCurrencyPair]] defines it. The floor is the pair of the two first currencies of the
   * family at a rate of one. '''The floor is one rather than zero''' because the factory rejects a
   * rate of zero, and a rate of one is the only rate an identity pair may carry, so this floor is
   * a value both kinds of generated rate can reach.
   *
   * Every candidate is built by [[FxRate.of]], which checks both constraints of the type, so a
   * reduction of the pair that would need a rate of one - the collapse to an identity pair, which
   * the pair shrinking does not offer anyway - could not slip through as a value.
   *
   * @return the shrinking of exchange rates
   */
  implicit val shrinkFxRate: Shrink[FxRate] = Shrink.withLazyList(fxRateCandidates)

  /** The candidates an exchange rate shrinks to, the unit rate before the earlier pairs. */
  private def fxRateCandidates(rate: FxRate): LazyList[FxRate] = {
    val unitRate =
      if (java.lang.Double.compare(rate.rate, 1.0d) == 0) LazyList.empty
      else candidateOf(FxRate.of(rate.pair, 1.0d))
    val pairs = currencyPairCandidates(rate.pair)
      .flatMap(pair => candidateOf(FxRate.of(pair, rate.rate)))
    (unitRate #::: pairs).distinct.filterNot(candidate => candidate == rate)
  }

  /** Copies a matrix of rates with the entry at the position replaced by positive zero. */
  private def zeroedEntry(rates: DoubleMatrix, row: Int, column: Int): DoubleMatrix =
    DoubleMatrix.tabulate(rates.rowCount, rates.columnCount)((atRow, atColumn) =>
      if (atRow == row && atColumn == column) 0.0d else rates.get(atRow, atColumn))

  /** The leading square submatrix of the size given, which keeps the unit diagonal. */
  private def leadingSubmatrix(rates: DoubleMatrix, size: Int): DoubleMatrix =
    DoubleMatrix.tabulate(size, size)((row, column) => rates.get(row, column))

  /**
   * Shrinks a matrix of rates by dropping its last currency and by zeroing an off-diagonal rate.
   *
   * The measure is the number of currencies plus the number of off-diagonal entries that are not
   * already positive zero, and the floor is the '''empty''' matrix, which the type holds and the
   * generators draw. Dropping the last currency takes the leading square submatrix, so the
   * diagonal stays a unit diagonal and the currencies stay distinct; zeroing touches off-diagonal
   * entries only, for the same reason - a rate of a currency against itself has to be one, and a
   * rate of zero elsewhere is legal, its reciprocal being the infinity the type also holds.
   *
   * Every candidate is built by [[FxMatrix.fromMatrix]], the factory a decoded document arrives
   * at, so the three structural checks are applied to it rather than assumed.
   *
   * @return the shrinking of matrices of exchange rates
   */
  implicit val shrinkFxMatrix: Shrink[FxMatrix] = Shrink.withLazyList(fxMatrixCandidates)

  /** The candidates a matrix shrinks to, the smaller matrix before the simpler rates. */
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
  /**
   * Shrinks a business day convention to the conventions declared before it.
   *
   * The measure is the position of the convention in the family's declaration order, and the
   * floor is the first convention declared.
   *
   * @return the shrinking of business day conventions
   */
  implicit val shrinkBusinessDayConvention: Shrink[BusinessDayConvention] =
    Shrink.withLazyList(earlierMembers(BusinessDayConvention.values.toList))

  /**
   * Shrinks a holiday calendar identifier within the kind of identifier it is.
   *
   * The three kinds the generator draws shrink in three ways, and '''none of them shrinks into
   * another kind''', because the kind is what an identifier is for: a built-in identifier is one
   * the standard reference data resolves, a custom one is deliberately not, and a composite one
   * is the form whose name carries a separator.
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
   * The measure is the pair of the number of names composed - two for a composite and one
   * otherwise - and then, within the kind, the position among the built-in identifiers or the
   * length of the name. The pair is ordered lexicographically, which is well-founded on two
   * non-negative whole numbers, and the kind is preserved wherever the first element does not
   * fall, so repeated shrinking terminates: at the first built-in identifier, or at a custom
   * identifier of one character.
   *
   * Every candidate is built by [[HolidayCalendarId.of]], the factory the generator uses, so a
   * candidate is normalised exactly as a generated identifier is.
   *
   * @return the shrinking of holiday calendar identifiers
   */
  implicit val shrinkHolidayCalendarId: Shrink[HolidayCalendarId] =
    Shrink.withLazyList(holidayCalendarIdCandidates)

  /** The separators the two composition forms of an identifier are written with. */
  private val CalendarIdSeparators: Set[Char] = Set('+', '~')

  /** The candidates an identifier shrinks to, within its own kind. */
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
   * The measure is the position of the calendar among the built-in ones, and the floor is the
   * day count of the first built-in calendar. The candidates are drawn from the built-in set
   * alone, which is the set [[genCalendarBearingDayCount]] draws from, so a candidate stays a day
   * count whose calendar the name form of its document reads back.
   *
   * Every candidate is built by [[DayCount.ofBus252]], the one factory of the type, and narrowed
   * by the type test that generator uses; the factory answers at the type of the family and
   * cannot answer with anything else, so the branch dropping a value that is not a `Bus252`
   * contributes no candidate rather than being assumed away.
   *
   * @return the shrinking of calendar-bearing day counts
   */
  implicit val shrinkCalendarBearingDayCount: Shrink[DayCount.Bus252] =
    Shrink.withLazyList(calendarBearingDayCountCandidates)

  /** The candidates a calendar-bearing day count shrinks to, by the calendar it counts over. */
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
   * '''A calendar-bearing day count never shrinks to a standard member.''' The two kinds differ
   * in the document form they take - a name for a standard member, an object naming a calendar
   * for a calendar-bearing one - and in the data they carry, so a spec drawing from
   * [[genBus252DayCount]] is about that kind; a candidate of the other kind would minimise a
   * failure of the calendar-bearing form into a value of the form that never had it.
   *
   * The measure is the position of the member among the standard ones for a standard member, and
   * the position of its calendar among the built-in ones for a calendar-bearing one; the kind is
   * preserved, so the measure is well defined at every step. The floors are the first standard
   * member declared and the day count of the first built-in calendar.
   *
   * @return the shrinking of day counts
   */
  implicit val shrinkDayCount: Shrink[DayCount] = Shrink.withLazyList(dayCountCandidates)

  /** The candidates a day count shrinks to, within the kind of day count it is. */
  private def dayCountCandidates(dayCount: DayCount): LazyList[DayCount] =
    dayCount match {
      case bus252: DayCount.Bus252 =>
        calendarBearingDayCountCandidates(bus252).map(candidate => candidate: DayCount)
      case standard => earlierMembers(DayCount.values.toList)(standard)
    }

  /**
   * Shrinks a custom calendar by dropping its working days and then its last holiday.
   *
   * The two candidates are offered in that order - the working days go first, and a holiday is
   * dropped only once none is left - because a working day is ignored outside the range of years
   * the holidays span, so shortening the holidays first could drop a working day silently and
   * leave a candidate that is not the value the reduction described. The '''last''' holiday is the
   * one dropped rather than the first, for the same reason: the first holiday is where the range
   * of the calendar starts, and moving that start is the other way a working day disappears.
   *
   * The measure is the number of holidays plus the number of working days, which falls at every
   * step, and the floor is a calendar of one holiday and no working days. A calendar that is one
   * of the built-in set has '''no''' candidates at all: its document form is its name, so a
   * calendar holding a built-in identifier with different holidays would not read back as
   * itself. The generator of this type draws custom calendars only, whose names begin with `X`,
   * so that branch guards a calendar a spec built by hand rather than one that was drawn.
   *
   * Note that the candidates are '''not''' compared with the value they came from, as every other
   * shrinking here compares them: the `equals` of this type compares the identifier alone, so
   * every candidate is equal to its input and filtering by equality would drop all of them. The
   * measure is what establishes that a candidate is a reduction, and it falls by construction.
   *
   * @return the shrinking of custom holiday calendars
   */
  implicit val shrinkImmutableHolidayCalendar: Shrink[ImmutableHolidayCalendar] =
    Shrink.withLazyList(immutableHolidayCalendarCandidates)

  /** The candidates a custom calendar shrinks to, the working days before the holidays. */
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
   * The four shapes the generator draws shrink in three ways, and each keeps its own shape:
   *
   *   - a composite - a combination or a link - shrinks to the two calendars it composes, which
   *     is the reduction saying which half of the composition a failure needs;
   *   - a built-in calendar shrinks to the built-in calendars declared before it, so it stays a
   *     calendar carried in a document as its name;
   *   - a custom calendar shrinks as [[shrinkImmutableHolidayCalendar]] describes, keeping its own
   *     identifier so its document form stays the structural one.
   *
   * The measure is defined over all four shapes at once: one plus the measures of both halves for
   * a composite, the number of holidays plus working days for a custom calendar, and the position
   * among the built-in calendars for a built-in one. A composite is strictly larger than either
   * half because both measures are non-negative, and the other two reductions fall within their
   * own shape, so repeated shrinking terminates - at the first built-in calendar, or at a custom
   * calendar of one holiday.
   *
   * A calendar of none of these shapes, which a spec could build by hand, offers no candidate
   * rather than being reduced by a rule written for another shape.
   *
   * @return the shrinking of holiday calendars
   */
  implicit val shrinkHolidayCalendar: Shrink[HolidayCalendar] =
    Shrink.withLazyList(holidayCalendarCandidates)

  /** The candidates a calendar shrinks to, within the shape of calendar it is. */
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
   * among the built-in identifiers, and the floor is the first convention over the first built-in
   * calendar. The calendar is moved within the built-in set rather than shortened, because the
   * generator promises an adjustment that '''can be applied''' against the standard reference
   * data and a name outside that set resolves to nothing; an adjustment naming a calendar outside
   * it - which a spec could build by hand - keeps its calendar and shrinks its convention alone.
   *
   * @return the shrinking of business day adjustments
   */
  implicit val shrinkBusinessDayAdjustment: Shrink[BusinessDayAdjustment] =
    Shrink.withLazyList(businessDayAdjustmentCandidates)

  /** The candidates an adjustment shrinks to, the earlier convention before the earlier calendar. */
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
   * adjustment as [[shrinkBusinessDayAdjustment]] defines it. The floor is the first date of the
   * window under the floor adjustment. The factory is total, so no candidate is dropped.
   *
   * @return the shrinking of adjustable dates
   */
  implicit val shrinkAdjustableDate: Shrink[AdjustableDate] =
    Shrink.withLazyList(adjustableDateCandidates)

  /** The candidates an adjustable date shrinks to, the earlier date before the simpler adjustment. */
  private def adjustableDateCandidates(date: AdjustableDate): LazyList[AdjustableDate] = {
    val dates = simplerDates(date.unadjusted).map(day => AdjustableDate.of(day, date.adjustment))
    val adjustments = businessDayAdjustmentCandidates(date.adjustment)
      .map(adjustment => AdjustableDate.of(date.unadjusted, adjustment))
    (dates #::: adjustments).distinct.filterNot(candidate => candidate == date)
  }

  /**
   * Shrinks a run of adjustable dates by dropping its last date and by moving the run earlier.
   *
   * Three candidates, in the order a reader needs them: the run without its last date, the run
   * translated so that its first date is the first date of the generated window, and the run
   * under a simpler adjustment. '''The dates keep the order and the gaps the factory requires''' -
   * dropping the last of a strictly increasing run leaves it strictly increasing, and translating
   * every date by the same number of days moves the run without changing a single gap - so a
   * candidate is never a run [[AdjustableDates.of]] rejects. The translation is offered only
   * where the first date is after the start of the window, so no candidate walks off the calendar.
   *
   * The measure is the number of dates plus one where the first date is not already the start of
   * the window plus the measure of the adjustment, and the floor is one date at the start of the
   * window under the floor adjustment.
   *
   * @return the shrinking of runs of adjustable dates
   */
  implicit val shrinkAdjustableDates: Shrink[AdjustableDates] =
    Shrink.withLazyList(adjustableDatesCandidates)

  /** The candidates a run of dates shrinks to, the shorter run first. */
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

  /** The identifier naming no calendar, which is the calendar field of an addition of calendar days. */
  private val NoHolidaysCalendarId: HolidayCalendarId = StandardHolidayCalendars.NO_HOLIDAYS.id

  /**
   * Shrinks a days adjustment towards no days at all under a simpler trailing adjustment.
   *
   * '''An addition of business days stays one and an addition of calendar days stays one.''' The
   * two are rebuilt through their own factories - the three-argument
   * [[DaysAdjustment.ofBusinessDays]], which the type documents as the way to rebuild an
   * adjustment from the fields of an existing one, and [[DaysAdjustment.ofCalendarDays]] - so a
   * candidate holds the addition calendar its input held and walks the days the same way. A
   * candidate of the other kind would minimise a failure of the business-day walk into a value
   * that never walked one.
   *
   * The measure is the magnitude of the day count plus the measure of the trailing adjustment,
   * and the floor is no days at all under the floor adjustment - which is the adjustment that
   * adjusts nothing, the constant the generator draws.
   *
   * @return the shrinking of days adjustments
   */
  implicit val shrinkDaysAdjustment: Shrink[DaysAdjustment] =
    Shrink.withLazyList(daysAdjustmentCandidates)

  /** The candidates a days adjustment shrinks to, the fewer days before the simpler adjustment. */
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

  /**
   * Shrinks a period addition convention to the conventions declared before it.
   *
   * The measure is the position of the convention in the family's declaration order, and the
   * floor is the first convention declared.
   *
   * @return the shrinking of period addition conventions
   */
  implicit val shrinkPeriodAdditionConvention: Shrink[PeriodAdditionConvention] =
    Shrink.withLazyList(earlierMembers(PeriodAdditionConvention.values.toList))

  /**
   * Shrinks a period adjustment by reducing its period, its convention and its adjustment.
   *
   * The period is reduced within the unit it is stated in, as [[simplerPeriods]] describes, which
   * is what keeps a month-based convention paired with a month-based period: the pairing rule the
   * generator follows is preserved by construction rather than by a check. A candidate moving the
   * convention earlier is built by [[PeriodAdjustment.of]] all the same, so the one reduction that
   * can break the pairing - a convention that is month-based over a period holding days - is
   * rejected by the factory and contributes no candidate.
   *
   * The measure is the number of units the period holds plus the position of the convention plus
   * the measure of the business day adjustment, and the floor is one unit of the period's own kind
   * under the first convention and the floor adjustment.
   *
   * @return the shrinking of period adjustments
   */
  implicit val shrinkPeriodAdjustment: Shrink[PeriodAdjustment] =
    Shrink.withLazyList(periodAdjustmentCandidates)

  /** The candidates a period adjustment shrinks to, the shorter period first. */
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
   * The measure is the number of units the period holds and the floor is one unit of that kind -
   * one day, one week, one month or one year - because [[Tenor.of]] rejects a period of zero and
   * the canonical name of a tenor is decided by the unit it holds, as [[simplerPeriods]]
   * describes. Every candidate goes through that factory, so a candidate is a tenor on the terms
   * a generated one is.
   *
   * @return the shrinking of tenors
   */
  implicit val shrinkTenor: Shrink[Tenor] = Shrink.withLazyList(tenorCandidates)

  /** The candidates a tenor shrinks to, the shortest period first. */
  private def tenorCandidates(tenor: Tenor): LazyList[Tenor] =
    simplerPeriods(tenor.period)
      .flatMap(period => candidateOf(Tenor.of(period)))
      .distinct
      .filterNot(candidate => candidate == tenor)

  /**
   * Shrinks a tenor adjustment by reducing its tenor, its convention and its adjustment.
   *
   * The measure, the floor and the pairing rule are those of [[shrinkPeriodAdjustment]]; this type
   * differs only in holding a tenor where that one holds a period.
   *
   * @return the shrinking of tenor adjustments
   */
  implicit val shrinkTenorAdjustment: Shrink[TenorAdjustment] =
    Shrink.withLazyList(tenorAdjustmentCandidates)

  /** The candidates a tenor adjustment shrinks to, the shorter tenor first. */
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
   * A market tenor that is one of the four constants - overnight, tomorrow-next, spot-next and
   * spot-week - shrinks to the constants declared before it; any other one is spot-starting, and
   * shrinks by reducing the tenor it starts from through [[MarketTenor.ofSpot]], the factory the
   * generator uses. That factory answers with spot-next for a tenor of one day and with spot-week
   * for a tenor of one week, so a spot-starting tenor may shrink into one of the constants - which
   * is no change of kind at all, those two constants being exactly what a spot-starting tenor of
   * that length is.
   *
   * The measure is the position among the four constants for a constant, and four plus the number
   * of units its tenor holds for any other; every candidate of a spot-starting tenor either holds
   * fewer units or is a constant, whose measure is at most three, so the measure falls at every
   * step. The floor is the overnight tenor.
   *
   * @return the shrinking of market tenors
   */
  implicit val shrinkMarketTenor: Shrink[MarketTenor] =
    Shrink.withLazyList(marketTenorCandidates)

  /** The candidates a market tenor shrinks to, within the kind of market tenor it is. */
  private def marketTenorCandidates(marketTenor: MarketTenor): LazyList[MarketTenor] = {
    val candidates =
      if (MarketTenorConstants.contains(marketTenor)) {
        earlierMembers(MarketTenorConstants)(marketTenor)
      } else {
        tenorCandidates(marketTenor.tenor).flatMap(tenor => candidateOf(MarketTenor.ofSpot(tenor)))
      }
    candidates.distinct.filterNot(candidate => candidate == marketTenor)
  }

  /**
   * Shrinks a date sequence to the sequences declared before it.
   *
   * The measure is the position of the sequence in the family's declaration order, and the floor
   * is the first sequence declared.
   *
   * @return the shrinking of date sequences
   */
  implicit val shrinkDateSequence: Shrink[DateSequence] =
    Shrink.withLazyList(earlierMembers(DateSequence.values.toList))

  /**
   * Shrinks a sequence date instruction towards the first date of the base sequence.
   *
   * Four candidates, each dropping one thing the instruction says: the starting month moves to the
   * first month of the generated window and then goes away entirely, the minimum period goes away,
   * the sequence number moves towards one, and counting over the full sequence becomes counting
   * over the base sequence. '''The two starting points are never introduced, only removed''', which
   * is what keeps a candidate inside the rule the factory enforces - a month and a minimum period
   * are mutually exclusive - rather than relying on the factory to reject a candidate that named
   * both.
   *
   * The measure is the number of starting points named, plus the sequence number less one, plus
   * one where the month is not already the first of the window, plus one where the full sequence
   * is counted over. The floor is the first date of the base sequence counted from the input date:
   * no starting point, sequence number one, base sequence. Every candidate is built by
   * [[SequenceDate.of]], which reports all three of its rejections, so a reduction it refuses
   * contributes no candidate.
   *
   * @return the shrinking of sequence date instructions
   */
  implicit val shrinkSequenceDate: Shrink[SequenceDate] =
    Shrink.withLazyList(sequenceDateCandidates)

  /** The candidates an instruction shrinks to, the simpler starting point first. */
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
  /**
   * Shrinks an Ibor index to the indices declared before it.
   *
   * The measure is the position of the index in the family's declaration order, and the floor is
   * the first index declared.
   *
   * @return the shrinking of Ibor indices
   */
  implicit val shrinkIborIndex: Shrink[IborIndex] =
    Shrink.withLazyList(earlierMembers(IborIndex.values.toList))

  /**
   * Shrinks an overnight index to the indices declared before it.
   *
   * The measure and the floor are those of [[shrinkIborIndex]], over this family's own members.
   *
   * @return the shrinking of overnight indices
   */
  implicit val shrinkOvernightIndex: Shrink[OvernightIndex] =
    Shrink.withLazyList(earlierMembers(OvernightIndex.values.toList))

  /**
   * Shrinks a price index to the indices declared before it.
   *
   * The measure and the floor are those of [[shrinkIborIndex]], over this family's own members.
   *
   * @return the shrinking of price indices
   */
  implicit val shrinkPriceIndex: Shrink[PriceIndex] =
    Shrink.withLazyList(earlierMembers(PriceIndex.values.toList))

  /**
   * Shrinks an FX index to the indices declared before it.
   *
   * The measure and the floor are those of [[shrinkIborIndex]], over this family's own members.
   *
   * @return the shrinking of FX indices
   */
  implicit val shrinkFxIndex: Shrink[FxIndex] =
    Shrink.withLazyList(earlierMembers(FxIndex.values.toList))

  /**
   * Shrinks a floating rate type to the types declared before it.
   *
   * The measure is the position of the type in the family's declaration order, and the floor is
   * the first type declared.
   *
   * @return the shrinking of floating rate types
   */
  implicit val shrinkFloatingRateType: Shrink[FloatingRateType] =
    Shrink.withLazyList(earlierMembers(FloatingRateType.values.toList))

  /**
   * Shrinks a floating rate name to the names declared before it.
   *
   * The measure is the position of the name in the family's declaration order, and the floor is
   * the first name declared.
   *
   * @return the shrinking of floating rate names
   */
  implicit val shrinkFloatingRateName: Shrink[FloatingRateName] =
    Shrink.withLazyList(earlierMembers(FloatingRateName.values.toList))

  /**
   * Shrinks an observation of an Ibor index fixing towards the start of the generated window.
   *
   * An observation is derived rather than given - its effective date, maturity date and year
   * fraction come from the index and the fixing date - so both candidates go back through
   * [[IborIndexObservation.of]] against the standard reference data, which is what
   * [[genIborIndexObservation]] does. A candidate assembled field by field could hold dates the
   * index does not derive, and no reduction of it would then mean anything.
   *
   * The index is moved within the '''observable''' indices alone - the ones whose calendars the
   * standard data resolves, which is the set the generator draws from - so a candidate is an
   * observation that can be built rather than one whose calendars are missing.
   *
   * The measure is one where the fixing date is not already the first date of the generated
   * window, plus the position of the index among the observable ones; the floor is the first
   * observable index fixing on the first date of the window.
   *
   * @return the shrinking of Ibor index observations
   */
  implicit val shrinkIborIndexObservation: Shrink[IborIndexObservation] =
    Shrink.withLazyList(iborIndexObservationCandidates)

  /** The candidates an Ibor observation shrinks to, the earlier date before the earlier index. */
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
   * The measure, the floor and the reason for going back through the factory are those of
   * [[shrinkIborIndexObservation]]; this observation derives a publication date as well, which
   * the factory supplies for the same reason.
   *
   * @return the shrinking of overnight index observations
   */
  implicit val shrinkOvernightIndexObservation: Shrink[OvernightIndexObservation] =
    Shrink.withLazyList(overnightIndexObservationCandidates)

  /** The candidates an overnight observation shrinks to, the earlier date first. */
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

  /**
   * Shrinks an observation of an FX index fixing towards the start of the window.
   *
   * The measure, the floor and the reason for going back through the factory are those of
   * [[shrinkIborIndexObservation]].
   *
   * @return the shrinking of FX index observations
   */
  implicit val shrinkFxIndexObservation: Shrink[FxIndexObservation] =
    Shrink.withLazyList(fxIndexObservationCandidates)

  /** The candidates an FX observation shrinks to, the earlier date before the earlier index. */
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
   * the observation holds exactly the index and the month it is given. The measure is one where
   * the month is not already the first month of the generated window plus the position of the
   * index in its family, and the floor is the first index fixing in that first month.
   *
   * @return the shrinking of price index observations
   */
  implicit val shrinkPriceIndexObservation: Shrink[PriceIndexObservation] =
    Shrink.withLazyList(priceIndexObservationCandidates)

  /** The candidates a price observation shrinks to, the earlier month before the earlier index. */
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
   * like any other - and the measure accounts for it: it is the position among the named countries
   * for a named one, and the number of the named countries plus the distance of the two letters
   * from `A` for any other, so a candidate that lands on a named country is strictly smaller than
   * the code it came from. The floors are the first constant declared and the code `AA`.
   *
   * Every candidate is built by [[Country.of]], which accepts two upper-case letters and nothing
   * else, so a candidate is a country on exactly the terms a generated one is.
   *
   * @return the shrinking of countries
   */
  implicit val shrinkCountry: Shrink[Country] = Shrink.withLazyList(countryCandidates)

  /** The candidates a country shrinks to, within the kind of code it is. */
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
   * The measure is the number of units the length holds, as [[simplerPeriods]] defines it, and the
   * floor is one unit of that kind - one day, one week, one month or one year - because
   * [[Frequency.of]] rejects a length of zero and the canonical name of a frequency is decided by
   * the unit it is stated in.
   *
   * '''The term frequency is its own floor and shrinks to nothing.''' It is the frequency of a
   * single period covering the whole schedule, its name is a word rather than a period, and a
   * schedule at a regular frequency is a different shape of value entirely, so reducing it to one
   * would minimise a failure of the term case into a value that never had it.
   *
   * @return the shrinking of frequencies
   */
  implicit val shrinkFrequency: Shrink[Frequency] = Shrink.withLazyList(frequencyCandidates)

  /** The candidates a frequency shrinks to, the shortest length first. */
  private def frequencyCandidates(frequency: Frequency): LazyList[Frequency] =
    if (frequency == Frequency.TERM) {
      LazyList.empty
    } else {
      simplerPeriods(frequency.period)
        .flatMap(period => candidateOf(Frequency.of(period)))
        .distinct
        .filterNot(candidate => candidate == frequency)
    }

  /**
   * Shrinks a stub convention to the conventions declared before it.
   *
   * The measure is the position of the convention in the family's declaration order, and the floor
   * is the first convention declared.
   *
   * @return the shrinking of stub conventions
   */
  implicit val shrinkStubConvention: Shrink[StubConvention] =
    Shrink.withLazyList(earlierMembers(StubConvention.values.toList))

  /**
   * Shrinks a roll convention to the conventions declared before it.
   *
   * The family publishes its forty-five members in one order - the standard conventions, then the
   * day-of-month ones, then the day-of-week ones - so the measure is the position in that order and
   * the floor is the first standard convention.
   *
   * @return the shrinking of roll conventions
   */
  implicit val shrinkRollConvention: Shrink[RollConvention] =
    Shrink.withLazyList(earlierMembers(RollConvention.values.toList))

  /** The shortest unadjusted length a generated schedule period holds, in days. */
  private val MinimumPeriodLength: Long = 10L

  /**
   * Shrinks a schedule period by removing its shifts, shortening it and moving it to the window.
   *
   * Three candidates, each reducing one thing about the period: the period with each adjusted date
   * equal to the unadjusted one it belongs to, the period cut to the shortest unadjusted length the
   * generator draws while both shifts are kept, and the period translated so that its unadjusted
   * start is the first date of the generated window.
   *
   * '''Both pairs of dates stay in the order the factory checks.''' Removing the shifts leaves the
   * adjusted pair equal to the unadjusted pair, which is ordered because the unadjusted pair is;
   * translating moves all four dates by the same number of days, which changes no gap at all; and
   * cutting the length keeps the shifts, which the generator draws within three days of a length
   * of at least ten. A cut that did cross the dates - which a period built by hand with larger
   * shifts could - is rejected by [[SchedulePeriod.of]] and contributes no candidate.
   *
   * The measure is the number of adjusted dates that differ from their unadjusted date, plus the
   * unadjusted length in days beyond the shortest generated one, plus one where the unadjusted
   * start is not the first date of the window. The floor is a ten-day period at the start of the
   * window whose adjusted dates are its unadjusted ones.
   *
   * @return the shrinking of schedule periods
   */
  implicit val shrinkSchedulePeriod: Shrink[SchedulePeriod] =
    Shrink.withLazyList(schedulePeriodCandidates)

  /** The candidates a period shrinks to, the unshifted period first. */
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
   * '''A period is never reduced in place.''' The periods of a generated schedule run end to end
   * at the frequency the schedule reports, and a period made shorter or moved would leave a gap
   * between itself and its neighbour - a schedule no generation could produce and the specs
   * reading periods off a schedule are not written against. Dropping the '''last''' period is the
   * one reduction that keeps the run contiguous from the start date it began at, and it is the
   * reduction a reader needs: it says how few periods the failure needs.
   *
   * The frequency is left alone for the same reason, since it is the frequency those periods run
   * at. The measure is the number of periods plus the position of the roll convention, and the
   * floor is a schedule of one period under the first roll convention. Every candidate is built by
   * [[Schedule.of]], which is the factory the generator uses.
   *
   * @return the shrinking of schedules
   */
  implicit val shrinkSchedule: Shrink[Schedule] = Shrink.withLazyList(scheduleCandidates)

  /** The candidates a schedule shrinks to, the shorter schedule before the earlier convention. */
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
   * Seven candidates, one for each optional field the definition holds: the two overriding
   * business day adjustments, the stub convention, the roll convention, the two regular period
   * boundaries and the overriding start date. '''An optional field is only ever emptied, never
   * introduced or moved''', because the seven order invariants the type checks hold between the
   * date-bearing ones - a regular boundary falls on a period boundary, an overriding start date
   * falls before the start date - and emptying a field can only remove a constraint, while
   * changing one would break it. The four required fields, which carry the shape of the schedule,
   * are left exactly as they are for that same reason.
   *
   * The measure is the number of optional fields the definition names, and the floor is the
   * definition of its four required fields alone. Every candidate is built by
   * [[PeriodicSchedule.of]], the factory the generator uses, so a combination it refuses
   * contributes no candidate.
   *
   * @return the shrinking of periodic schedule definitions
   */
  implicit val shrinkPeriodicSchedule: Shrink[PeriodicSchedule] =
    Shrink.withLazyList(periodicScheduleCandidates)

  /** The candidates a definition shrinks to, one for each optional field it names. */
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
  /**
   * Shrinks a value adjustment type to the types declared before it.
   *
   * The measure is the position of the type in the family's declaration order, and the floor is
   * the first type declared.
   *
   * @return the shrinking of value adjustment types
   */
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
   * '''The kind of adjustment is never changed.''' Replacing a value, adding a delta, adding a
   * proportion and multiplying are four different operations, and a step sequence accepts three of
   * them and refuses the fourth, so a candidate of another kind would minimise a failure of one
   * operation into a value describing another - the rule `strata-collect` states for the reason of
   * a failure, which its shrinking also never changes. Each candidate is rebuilt through the
   * factory naming its own kind.
   *
   * The measure is one where the number carried is not already positive zero, and the floor is an
   * adjustment of positive zero of the same kind.
   *
   * @return the shrinking of value adjustments
   */
  implicit val shrinkValueAdjustment: Shrink[ValueAdjustment] =
    Shrink.withLazyList(valueAdjustmentCandidates)

  /** The candidates an adjustment shrinks to: the same kind adjusting by positive zero. */
  private def valueAdjustmentCandidates(
      adjustment: ValueAdjustment): LazyList[ValueAdjustment] =

    simplerNumbers(adjustment.modifyingValue)
      .map(value => rebuiltValueAdjustment(value, adjustment.`type`))
      .distinct
      .filterNot(candidate => candidate == adjustment)

  /**
   * Shrinks a value with its derivatives towards zero with one zero derivative.
   *
   * The derivatives shrink exactly as an array does in `strata-collect` - the trailing element is
   * dropped while more than one is left, and an element that is not already positive zero is
   * simplified to it - and the floor keeps one derivative for the reason that shrinking does: the
   * array generator draws at least one element, so an empty array is not a value that was drawn.
   *
   * The measure is one where the value is not already positive zero, plus the number of
   * derivatives, plus the number of derivatives that are not already positive zero. The floor is a
   * value of positive zero with one derivative of positive zero.
   *
   * @return the shrinking of values with derivatives
   */
  implicit val shrinkValueDerivatives: Shrink[ValueDerivatives] =
    Shrink.withLazyList(valueDerivativesCandidates)

  /** The candidates a value with derivatives shrinks to, the zeroed value first. */
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
   * decimal places. Both stay half-up roundings, and both are within the image of
   * [[genHalfUpRounding]], which draws a fraction of zero as one of its fractions.
   *
   * The measure is the number of decimal places plus the fraction of the last place, and the floor
   * is rounding to zero decimal places with no fraction. Both candidates are built by the factories
   * the generator uses, which check the ranges of both fields, so a candidate is a rounding on
   * exactly the terms a generated one is.
   *
   * @return the shrinking of half-up roundings
   */
  implicit val shrinkHalfUp: Shrink[HalfUp] = Shrink.withLazyList(halfUpCandidates)

  /** The candidates a half-up rounding shrinks to, the dropped fraction before fewer places. */
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
   * A half-up rounding shrinks as [[shrinkHalfUp]] describes and '''never shrinks to the rounding
   * that does nothing''': that one is a different member of the family, with its own document form
   * and its own behaviour of returning every value unaltered, so a candidate of it would minimise
   * a failure of rounding into a value that never rounded. The rounding that does nothing carries
   * no field and is its own floor.
   *
   * The measure is that of [[shrinkHalfUp]] for a half-up rounding and zero for the rounding that
   * does nothing.
   *
   * @return the shrinking of rounding conventions
   */
  implicit val shrinkRounding: Shrink[Rounding] = Shrink.withLazyList(roundingCandidates)

  /** The candidates a rounding shrinks to, within the kind of rounding it is. */
  private def roundingCandidates(rounding: Rounding): LazyList[Rounding] =
    rounding match {
      case halfUp: HalfUp => halfUpCandidates(halfUp).map(candidate => candidate: Rounding)
      case _ => LazyList.empty
    }

  /**
   * Shrinks a step changing a value, keeping the way the step is positioned.
   *
   * '''A step positioned by an index stays positioned by an index, and a step positioned by a date
   * stays positioned by a date.''' Exactly one of the two positions is named - the factory rejects
   * a step naming both and a step naming neither - and the two are resolved against a schedule in
   * different ways, so a candidate of the other shape would be a step of a different kind from the
   * one that failed. An index shrinks towards one, which is the smallest index the factory accepts,
   * and a date towards the first date of the generated window.
   *
   * The measure is the index less one, or one where the date is not already the start of the
   * window, plus the measure of the adjustment as [[shrinkValueAdjustment]] defines it. The floors
   * are a step at the first period index and a step on the first date of the window, each adjusting
   * by positive zero of its own kind.
   *
   * @return the shrinking of steps changing a value
   */
  implicit val shrinkValueStep: Shrink[ValueStep] = Shrink.withLazyList(valueStepCandidates)

  /** The candidates a step shrinks to, the simpler position before the simpler adjustment. */
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
   * Four candidates: the sequence whose last step date is its first, the sequence translated so
   * that its first step date is the start of the window, the sequence at a shorter frequency, and
   * the sequence under a simpler adjustment. '''The first date stays on or before the last''',
   * which the factory requires: collapsing the two makes them equal, and translating moves both by
   * the same number of days. The adjustment keeps its kind, so an adjustment the factory accepts -
   * it refuses one that replaces the value - stays one it accepts.
   *
   * The measure is one where the last step date is not the first, plus one where the first is not
   * the start of the window, plus the number of units the frequency holds, plus the measure of the
   * adjustment. The floor is a sequence whose two dates are both the start of the window, at one
   * unit of its frequency's kind, adjusting by positive zero.
   *
   * @return the shrinking of sequences of steps
   */
  implicit val shrinkValueStepSequence: Shrink[ValueStepSequence] =
    Shrink.withLazyList(valueStepSequenceCandidates)

  /** The candidates a sequence shrinks to, the collapsed span first. */
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
   * Construction places no constraint between the three fields - whether a step lines up with a
   * period boundary is a question about the schedule it is resolved against, not about this value -
   * so all three shrink freely and the factory rejects nothing: the initial value goes to positive
   * zero, the last step is dropped, a step that is kept is replaced by one of its own candidates,
   * and the sequence of steps is dropped or replaced by one of its candidates.
   *
   * The measure is one where the initial value is not already positive zero, plus the number of
   * steps and the sum of their measures, plus one where a sequence is named and its measure. The
   * floor is a schedule of positive zero with no steps and no sequence.
   *
   * @return the shrinking of schedules of values
   */
  implicit val shrinkValueSchedule: Shrink[ValueSchedule] =
    Shrink.withLazyList(valueScheduleCandidates)

  /**
   * The candidates a schedule of values shrinks to, the zeroed initial value first.
   *
   * Construction reports its outcome, so a candidate the factory refuses is simply not offered:
   * a shrinking may only produce values of the type, and the smaller fields a shrinking proposes
   * are not guaranteed to satisfy the factory's own conditions together.
   */
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
   * The measure is the measure of the amount, as [[shrinkCurrencyAmount]] defines it, plus the
   * measure of the adjustable date, as [[shrinkAdjustableDate]] defines it. The floor is a payment
   * of zero of the first currency on the first date of the generated window under the floor
   * adjustment. The factory is total, so no candidate is dropped.
   *
   * @return the shrinking of adjustable payments
   */
  implicit val shrinkAdjustablePayment: Shrink[AdjustablePayment] =
    Shrink.withLazyList(adjustablePaymentCandidates)

  /** The candidates a payment shrinks to, the simpler amount before the simpler date. */
  private def adjustablePaymentCandidates(
      payment: AdjustablePayment): LazyList[AdjustablePayment] = {

    val amounts = currencyAmountCandidates(payment.value)
      .map(amount => AdjustablePayment.of(amount, payment.date))
    val dates = adjustableDateCandidates(payment.date)
      .map(date => AdjustablePayment.of(payment.value, date))
    (amounts #::: dates).distinct.filterNot(candidate => candidate == payment)
  }
}
