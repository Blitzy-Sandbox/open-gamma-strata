/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import java.time.LocalDate

import scala.collection.immutable.SortedMap
import scala.reflect.ClassTag

import cats.Eq
import cats.Hash
import cats.Monoid
import cats.Order
import cats.Show
import cats.kernel.laws.discipline.EqTests
import cats.kernel.laws.discipline.HashTests
import cats.kernel.laws.discipline.MonoidTests
import cats.kernel.laws.discipline.OrderTests

import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import com.opengamma.strata.basics.Arbitraries._
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
import com.opengamma.strata.basics.value.Rounding
import com.opengamma.strata.basics.value.ValueAdjustment
import com.opengamma.strata.basics.value.ValueAdjustmentType
import com.opengamma.strata.basics.value.ValueDerivatives
import com.opengamma.strata.basics.value.ValueSchedule
import com.opengamma.strata.basics.value.ValueStep
import com.opengamma.strata.basics.value.ValueStepSequence
import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.FixedScaleDecimal
import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.array.DoubleMatrix
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason

/**
 * The `cats` instances of both modules, summoned at compile time.
 *
 * The value of this object is that it '''compiles''': a missing instance, or one offered twice and
 * so not choosable, is a compile error here. The shape is asserted as well as the presence -
 * [[InstanceInventory.ordered]] demands one implicit of type `Order[A] with Hash[A]`, which a
 * companion declaring the two separately does not satisfy, and `Eq[A]` is summoned from what was
 * given rather than declared, so a second equality-bearing instance makes that summon ambiguous.
 *
 * The entries are every type of the two modules that carries an instance: 28 ordered types, 35
 * unordered ones and the one additive instance, of [[MultiCurrencyAmount]] - 63 distinct types and
 * 218 instances, the counts [[InstanceInventory.report]] prints and [[TypeclassLawsSpec]] asserts.
 * `Hash` and `Show` being invariant, a member of a sealed family appears only where it declares
 * instances at its own type, which `DayCount.Bus252`, `ImmutableHolidayCalendar` and `HalfUp` do.
 */
object InstanceInventory {

  /**
   * One instance the compiler was asked for and produced.
   *
   * @param typeclass  the typeclass summoned, by its fully qualified name
   * @param typeName  the type it was summoned for, by its fully qualified name
   * @param runtimeName  the same type as the compiler names it, used to check `typeName`
   * @param instance  the instance itself, held so that the summon is a value rather than a
   *   discarded expression, and so that its initialisation can be checked
   */
  final case class Summoned(
      typeclass: String,
      typeName: String,
      runtimeName: String,
      instance: Any)

  /**
   * Summons the instances of an '''ordered''' type: one `Order[A] with Hash[A]`, and a `Show[A]`.
   *
   * The intersection type of the first parameter is the assertion: nothing but a single implicit
   * declared at that type satisfies it, so a companion splitting it in two fails to compile here.
   *
   * @param typeName  the fully qualified name of the type, as it is to be reported
   * @param orderHash  the single ordering, hashing and equality instance of the type
   * @param show  the rendering instance of the type
   * @param tag  the compiler's own name for the type, used to check `typeName`
   * @tparam A  the type whose instances are summoned
   * @return the four instances summoned for the type
   */
  private def ordered[A](typeName: String)(implicit
      orderHash: Order[A] with Hash[A],
      show: Show[A],
      tag: ClassTag[A]): List[Summoned] = {
    val runtimeName = tag.runtimeClass.getName
    List(
      Summoned("cats.Order", typeName, runtimeName, orderHash),
      Summoned("cats.Hash", typeName, runtimeName, orderHash),
      Summoned("cats.Eq", typeName, runtimeName, implicitly[Eq[A]]),
      Summoned("cats.Show", typeName, runtimeName, show))
  }

  /**
   * Summons the instances of an '''unordered''' type: a `Hash[A]` and a `Show[A]`.
   *
   * No `Order` is asked for, and none exists: [[TypeclassLawsSpec]] asserts that absence against
   * the compiler rather than leaving it to this omission.
   *
   * @param typeName  the fully qualified name of the type, as it is to be reported
   * @param hash  the hashing and equality instance of the type
   * @param show  the rendering instance of the type
   * @param tag  the compiler's own name for the type, used to check `typeName`
   * @tparam A  the type whose instances are summoned
   * @return the three instances summoned for the type
   */
  private def hashed[A](typeName: String)(implicit
      hash: Hash[A],
      show: Show[A],
      tag: ClassTag[A]): List[Summoned] = {
    val runtimeName = tag.runtimeClass.getName
    List(
      Summoned("cats.Hash", typeName, runtimeName, hash),
      Summoned("cats.Eq", typeName, runtimeName, implicitly[Eq[A]]),
      Summoned("cats.Show", typeName, runtimeName, show))
  }

  /**
   * Summons the additive instance of a type, of which one type has one.
   *
   * @param typeName  the fully qualified name of the type, as it is to be reported
   * @param monoid  the additive instance of the type
   * @param tag  the compiler's own name for the type, used to check `typeName`
   * @tparam A  the type whose instance is summoned
   * @return the instance summoned for the type
   */
  private def additive[A](typeName: String)(implicit
      monoid: Monoid[A],
      tag: ClassTag[A]): List[Summoned] =
    List(Summoned("cats.Monoid", typeName, tag.runtimeClass.getName, monoid))

  //-------------------------------------------------------------------------
  /** The 28 ordered types, the closed named families among them comparing by name. */
  val orderedTypes: List[Summoned] =
    ordered[StandardId]("com.opengamma.strata.basics.StandardId") :::
      ordered[Currency]("com.opengamma.strata.basics.currency.Currency") :::
      ordered[CurrencyPair]("com.opengamma.strata.basics.currency.CurrencyPair") :::
      ordered[CurrencyAmount]("com.opengamma.strata.basics.currency.CurrencyAmount") :::
      ordered[Money]("com.opengamma.strata.basics.currency.Money") :::
      ordered[BigMoney]("com.opengamma.strata.basics.currency.BigMoney") :::
      ordered[DayCount]("com.opengamma.strata.basics.date.DayCount") :::
      ordered[HolidayCalendarId]("com.opengamma.strata.basics.date.HolidayCalendarId") :::
      ordered[BusinessDayConvention]("com.opengamma.strata.basics.date.BusinessDayConvention") :::
      ordered[PeriodAdditionConvention](
        "com.opengamma.strata.basics.date.PeriodAdditionConvention") :::
      ordered[DateSequence]("com.opengamma.strata.basics.date.DateSequence") :::
      ordered[Tenor]("com.opengamma.strata.basics.date.Tenor") :::
      ordered[MarketTenor]("com.opengamma.strata.basics.date.MarketTenor") :::
      ordered[IborIndex]("com.opengamma.strata.basics.index.IborIndex") :::
      ordered[OvernightIndex]("com.opengamma.strata.basics.index.OvernightIndex") :::
      ordered[PriceIndex]("com.opengamma.strata.basics.index.PriceIndex") :::
      ordered[FxIndex]("com.opengamma.strata.basics.index.FxIndex") :::
      ordered[FloatingRateType]("com.opengamma.strata.basics.index.FloatingRateType") :::
      ordered[FloatingRateName]("com.opengamma.strata.basics.index.FloatingRateName") :::
      ordered[Country]("com.opengamma.strata.basics.location.Country") :::
      ordered[Frequency]("com.opengamma.strata.basics.schedule.Frequency") :::
      ordered[StubConvention]("com.opengamma.strata.basics.schedule.StubConvention") :::
      ordered[RollConvention]("com.opengamma.strata.basics.schedule.RollConvention") :::
      ordered[SchedulePeriod]("com.opengamma.strata.basics.schedule.SchedulePeriod") :::
      ordered[ValueAdjustmentType]("com.opengamma.strata.basics.value.ValueAdjustmentType") :::
      ordered[Decimal]("com.opengamma.strata.collect.Decimal") :::
      ordered[FixedScaleDecimal]("com.opengamma.strata.collect.FixedScaleDecimal") :::
      ordered[FailureReason]("com.opengamma.strata.collect.result.FailureReason")

  /**
   * The 35 unordered types: every other value type of the two modules.
   *
   * No ordering of these means anything - neither of two payments in different currencies is the
   * greater, and two holiday calendars are not ranked - so each carries hashing, equality and a
   * rendering and no more.
   */
  val unorderedTypes: List[Summoned] =
    hashed[CalculationTargetList]("com.opengamma.strata.basics.CalculationTargetList") :::
      hashed[ReferenceData.Entry[HolidayCalendar]](
        "com.opengamma.strata.basics.ReferenceData.Entry") :::
      hashed[CurrencyAmountArray]("com.opengamma.strata.basics.currency.CurrencyAmountArray") :::
      hashed[MultiCurrencyAmount]("com.opengamma.strata.basics.currency.MultiCurrencyAmount") :::
      hashed[MultiCurrencyAmountArray](
        "com.opengamma.strata.basics.currency.MultiCurrencyAmountArray") :::
      hashed[Payment]("com.opengamma.strata.basics.currency.Payment") :::
      hashed[AdjustablePayment]("com.opengamma.strata.basics.currency.AdjustablePayment") :::
      hashed[FxRate]("com.opengamma.strata.basics.currency.FxRate") :::
      hashed[FxMatrix]("com.opengamma.strata.basics.currency.FxMatrix") :::
      hashed[DayCount.Bus252]("com.opengamma.strata.basics.date.DayCount.Bus252") :::
      hashed[HolidayCalendar]("com.opengamma.strata.basics.date.HolidayCalendar") :::
      hashed[ImmutableHolidayCalendar](
        "com.opengamma.strata.basics.date.ImmutableHolidayCalendar") :::
      hashed[BusinessDayAdjustment]("com.opengamma.strata.basics.date.BusinessDayAdjustment") :::
      hashed[AdjustableDate]("com.opengamma.strata.basics.date.AdjustableDate") :::
      hashed[AdjustableDates]("com.opengamma.strata.basics.date.AdjustableDates") :::
      hashed[DaysAdjustment]("com.opengamma.strata.basics.date.DaysAdjustment") :::
      hashed[PeriodAdjustment]("com.opengamma.strata.basics.date.PeriodAdjustment") :::
      hashed[TenorAdjustment]("com.opengamma.strata.basics.date.TenorAdjustment") :::
      hashed[SequenceDate]("com.opengamma.strata.basics.date.SequenceDate") :::
      hashed[IborIndexObservation]("com.opengamma.strata.basics.index.IborIndexObservation") :::
      hashed[OvernightIndexObservation](
        "com.opengamma.strata.basics.index.OvernightIndexObservation") :::
      hashed[PriceIndexObservation]("com.opengamma.strata.basics.index.PriceIndexObservation") :::
      hashed[FxIndexObservation]("com.opengamma.strata.basics.index.FxIndexObservation") :::
      hashed[Schedule]("com.opengamma.strata.basics.schedule.Schedule") :::
      hashed[PeriodicSchedule]("com.opengamma.strata.basics.schedule.PeriodicSchedule") :::
      hashed[Rounding]("com.opengamma.strata.basics.value.Rounding") :::
      hashed[HalfUp]("com.opengamma.strata.basics.value.HalfUp") :::
      hashed[ValueAdjustment]("com.opengamma.strata.basics.value.ValueAdjustment") :::
      hashed[ValueDerivatives]("com.opengamma.strata.basics.value.ValueDerivatives") :::
      hashed[ValueStep]("com.opengamma.strata.basics.value.ValueStep") :::
      hashed[ValueStepSequence]("com.opengamma.strata.basics.value.ValueStepSequence") :::
      hashed[ValueSchedule]("com.opengamma.strata.basics.value.ValueSchedule") :::
      hashed[DoubleArray]("com.opengamma.strata.collect.array.DoubleArray") :::
      hashed[DoubleMatrix]("com.opengamma.strata.collect.array.DoubleMatrix") :::
      hashed[Failure]("com.opengamma.strata.collect.result.Failure")

  /** The one additive instance. */
  val additiveTypes: List[Summoned] =
    additive[MultiCurrencyAmount]("com.opengamma.strata.basics.currency.MultiCurrencyAmount")

  /** Every instance the two modules promise, in declaration order. */
  val summoned: List[Summoned] = orderedTypes ::: unorderedTypes ::: additiveTypes

  //-------------------------------------------------------------------------
  /** The prefix every reported line carries, so that the report can be extracted by name. */
  val LinePrefix: String = "TYPECLASS-INVENTORY"

  /** The prefix of the single counts line that closes the report. */
  val SummaryPrefix: String = "TYPECLASS-INVENTORY-SUMMARY"

  /**
   * Returns the report of the inventory: one line per instance, then one line of counts.
   *
   * The lines are sorted and hold nothing but the two names, so two runs produce byte-identical
   * output: an identity hash code, a timestamp or a rendering would make them differ.
   *
   * @return the lines of the report, the instances sorted and the counts last
   */
  def report: List[String] = {
    val instances = summoned
      .map(entry => s"$LinePrefix ${entry.typeclass} ${entry.typeName}")
      .sorted
    val counts = summoned.groupBy(_.typeclass).map { case (typeclass, entries) =>
      s"$typeclass=${entries.size}"
    }
    val summary =
      s"$SummaryPrefix types=${summoned.map(_.typeName).distinct.size} " +
        s"instances=${summoned.size} ${counts.toList.sorted.mkString(" ")}"
    instances :+ summary
  }
}

/**
 * Checks the `cats` instances of both modules against the laws of their typeclasses.
 *
 * This is the suite the typeclass-instance acceptance gate runs, and the only file in the
 * repository that uses `cats-laws` and `discipline-scalatest`. Every one of the 63 types of
 * [[InstanceInventory]] is checked under `EqTests` and `HashTests`, the 28 ordered ones also
 * under `OrderTests` and [[MultiCurrencyAmount]] also under `MonoidTests`. The absences no
 * running test can observe are asserted against the compiler, each paired with the nearest
 * positive.
 *
 * The generators come from [[Arbitraries]], and every value they produce is built by the type's
 * own factory. Four types are checked over an explicit generator instead of their implicit one -
 * [[DoubleArray]], [[DoubleMatrix]], [[FxRate]] and [[FxMatrix]], whose implicit arbitraries hold
 * the well-behaved values - because the equality of each is bit-pattern equality and the values
 * that decide it are `NaN`, the infinities and both zeroes. [[MultiCurrencyAmount]] is drawn from
 * `genFiniteMultiCurrencyAmount` for its additive laws alone, its `EqTests` and `HashTests`
 * keeping the ordinary generator, which produces infinities.
 *
 * ===When the absence proofs are authoritative===
 *
 * The absences at the foot of this file are asserted with `assertDoesNotCompile` and
 * `assertTypeError`, which are macros: each compiles its snippet while '''this file''' is
 * compiled, and the test that runs later only reports the answer the macro already reached.
 * Nothing in this suite's signature depends on the instances those snippets summon, so
 * incremental compilation is free to recompile a companion without recompiling this suite - and
 * then the run reports the previous compilation's answer, in either direction: an instance added
 * and removed again is still reported as present until this file is recompiled, and one added
 * without this file being recompiled is not reported at all. Those rows are therefore
 * authoritative after a clean compilation, which is what the acceptance gate runs
 * (`sbt -batch clean compile Test/compile test`); after an edit to a main source, re-run them as
 * `sbt -batch clean Test/compile "strata-basics/testOnly
 * com.opengamma.strata.basics.TypeclassLawsSpec"`. The law suites themselves and
 * [[InstanceInventory]] carry no such caveat: the instances they use are resolved when this file
 * is compiled and then '''exercised''' at run time, so a changed instance changes what they do.
 * [[ApiSurfaceSpec]] and `json.JsonRoundTripSpec` use the same macros and share the caveat.
 */
class TypeclassLawsSpec
    extends AnyFunSuite
    with Matchers
    with ScalaCheckPropertyChecks
    with FunSuiteDiscipline {

  /**
   * The property configuration of every rule set and property in this suite.
   *
   * A hundred cases is ten times the ScalaTest default and costs little across the 63 types, the
   * generators being bounded and the operations under test comparisons. The IEEE-754 edge values
   * that decide the equality of a type are asserted directly by the tests at the foot of this
   * file rather than left to a generator to stumble on.
   *
   * @return the configuration every property in this suite runs under
   */
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100, sizeRange = 8)

  //-------------------------------------------------------------------------
  // the tolerance and the values the explicit tests at the foot of the file are built from
  //-------------------------------------------------------------------------
  /** The relative tolerance the additive laws of [[MultiCurrencyAmount]] are stated at. */
  private val MonoidTolerance: Double = 1e-9

  private val gbp: Currency = Currency.GBP
  private val usd: Currency = Currency.USD
  private val eur: Currency = Currency.EUR
  private val gbpUsd: CurrencyPair = CurrencyPair.of(gbp, usd)
  private val jan15: LocalDate = LocalDate.of(2020, 1, 15)
  private val apr15: LocalDate = LocalDate.of(2020, 4, 15)
  private val jan14: LocalDate = LocalDate.of(2020, 1, 14)
  private val apr14: LocalDate = LocalDate.of(2020, 4, 14)

  /**
   * Takes the value out of a factory result, failing the test where there is none.
   *
   * The fixtures below are built through validated factories, so a fixture that cannot be built
   * is a broken test rather than a failed assertion, and naming the failure says which rule
   * rejected it.
   *
   * @param result  the result of the factory
   * @tparam E  the type of the failure reported, either a `Failure` or a chain of them
   * @tparam A  the type being built
   * @return the value the factory produced
   */
  private def valueOf[E, A](result: Either[E, A]): A =
    result match {
      case Right(value) => value
      case Left(failure) => fail(s"a fixture could not be built, the factory reported: $failure")
    }

  //-------------------------------------------------------------------------
  // the generators this suite supplies itself
  //-------------------------------------------------------------------------
  /**
   * Generates a reference data entry holding a holiday calendar under its own identifier.
   *
   * The identifier is the calendar's own, so a generated entry is one a reference data store
   * could hold, and the value type is fixed to [[HolidayCalendar]] because the instances of an
   * entry are given for every value type, so checking them at one type checks them at all.
   */
  private val arbEntry: Arbitrary[ReferenceData.Entry[HolidayCalendar]] =
    Arbitrary(genHolidayCalendar.map(calendar => ReferenceData.Entry(calendar.id, calendar)))

  /**
   * Perturbs a seed by a reference data entry, through its rendering.
   *
   * The rendering names the identifier and the calendar, the two fields the equality of an entry
   * compares, so it agrees with that equality - the one thing a `Cogen` has to do. It is implicit,
   * unlike the generator above, because the law suites take their `Cogen` implicitly.
   */
  private implicit val cogenEntry: Cogen[ReferenceData.Entry[HolidayCalendar]] =
    Cogen[String].contramap[ReferenceData.Entry[HolidayCalendar]](entry => entry.toString)

  /**
   * The tolerant equality the additive laws of [[MultiCurrencyAmount]] are checked with.
   *
   * '''This is not an implicit and must never become one.''' It is passed by hand to the one rule
   * set that needs it, so that the `EqTests` and `HashTests` of the same type, which are about
   * the production instance and nothing else, cannot pick it up. Two values are equal here when
   * they hold the same currencies and their amounts agree to within a relative `1e-9`, the
   * tolerance falling back to an absolute `1e-9` near zero.
   *
   * IEEE-754 addition is only approximately associative, so `(a + b) + c` and `a + (b + c)` can
   * differ in their last bit, and under the exact equality of the type the associativity law is
   * falsified within a handful of ordinary finite examples. The tolerance states the strongest
   * thing that is true of double arithmetic. The other restriction of that rule set, a generator
   * of finite amounts, is applied where it is registered.
   */
  private val tolerantMultiCurrencyAmountEq: Eq[MultiCurrencyAmount] =
    Eq.instance { (left, right) =>
      val leftAmounts = left.toMap
      val rightAmounts = right.toMap
      leftAmounts.keySet == rightAmounts.keySet &&
      leftAmounts.forall { case (currency, amount) =>
        closeEnough(amount, rightAmounts(currency))
      }
    }

  /**
   * Returns whether two amounts agree to within the tolerance of the additive laws.
   *
   * Bit equality is taken first, so two identical amounts agree however large, and the tolerance
   * is relative to the larger of the two with a floor of one, an absolute `1e-9` near zero.
   *
   * @return true if the two amounts agree to within the tolerance
   */
  private def closeEnough(left: Double, right: Double): Boolean =
    java.lang.Double.compare(left, right) == 0 ||
      math.abs(left - right) <= MonoidTolerance * math.max(
        math.max(math.abs(left), math.abs(right)),
        1.0d)

  /**
   * The leading part of the rendering of a [[Failure]]: its reason, a colon, a space and its
   * message, written out here rather than taken from the instance under test.
   *
   * @param failure  the failure whose leading rendering is wanted
   * @return the reason, a colon and the message
   */
  private def javaFailureForm(failure: Failure): String =
    s"${failure.reason.name}: ${failure.message}"

  /**
   * Asserts the rendering of a [[Failure]] by its structure.
   *
   * The `toString` of a failure delegates to its `Show`, so the default check of the rule sets
   * below - `rendered shouldBe value.toString` - would compare the rendering with itself and
   * assert nothing at all. This is the check registered for the type instead, and it states the
   * shape of the rendering independently of the instance that produced it: the rendering begins
   * with the reason, a colon and the message; a failure holding no attributes renders as that
   * and nothing more; every attribute appears once as `key=value`; the keys appear in ascending
   * order, which is what makes the rendering a function of the value rather than of the order
   * the attributes were added in; and the appended part is exactly as long as those pairs, their
   * separators and the brackets, so a rendering carrying anything further fails. The generated
   * failures hold short messages and attributes, which the rendering reproduces as they stand.
   *
   * The shape is read off the rendering rather than rebuilt from the failure, which is the point
   * of writing it this way: an oracle recomputing the instance's own expression would agree with
   * it whatever either of them did.
   *
   * @param failure  the failure that was rendered
   * @param rendered  what the instance under test produced for it
   * @return the assertion that the rendering has the shape the type promises
   */
  private def assertFailureRendering(failure: Failure, rendered: String): Assertion = {
    val javaForm = javaFailureForm(failure)
    rendered should startWith(javaForm)
    if (failure.attributes.isEmpty) {
      rendered shouldBe javaForm
    } else {
      val appended = rendered.substring(javaForm.length)
      appended should startWith(" [")
      appended should endWith("]")
      val pairs = failure.attributes.toList.map { case (key, value) => s"$key=$value" }
      pairs.foreach { pair =>
        withClue(s"the attribute $pair of $failure is rendered once in $appended: ") {
          appended.sliding(pair.length).count(window => window == pair) shouldBe 1
        }
      }
      val positions = failure.attributes.keysIterator.map(key => appended.indexOf(s"$key=")).toList
      withClue(s"the attributes of $failure are rendered in key order in $appended: ") {
        positions shouldBe positions.sorted
        positions.distinct.size shouldBe positions.size
      }
      // `" ["`, the pairs, `", "` between each two of them, and `"]"`, and nothing else
      appended.length shouldBe pairs.map(pair => pair.length).sum + 2 * (pairs.size - 1) + 3
    }
  }

  /**
   * The rendering [[CalculationTargetList]] promises: the bean form
   * `CalculationTargetList{targets=[a, b]}`.
   *
   * This is the one type of the inventory whose rendering differs from its own `toString`: the
   * type is a `final case class`, so its `toString` is the generated product rendering
   * `CalculationTargetList(List(a, b))`, while its `Show` reproduces the bean form above. The
   * whole of the rendering is fixed, so it is written out here as the text it must equal, an
   * oracle agreeing with the instance by construction asserting nothing.
   *
   * @return the bean rendering of the list, its targets in order
   */
  private def renderedTargetList(list: CalculationTargetList): String =
    s"CalculationTargetList{targets=[${list.targets.mkString(", ")}]}"

  //-------------------------------------------------------------------------
  // the registration of the law suites
  //-------------------------------------------------------------------------
  /** Registers the law suites of an unordered type, over its implicit generator. */
  private def unorderedLaws[A](typeName: String)(implicit
      arb: Arbitrary[A],
      cogen: Cogen[A],
      hash: Hash[A],
      show: Show[A]): Unit =
    unorderedLawsOf[A](typeName, arb)

  /**
   * Registers the law suites of an unordered type, over the generator given.
   *
   * The generator is a parameter rather than an implicit so that a type whose implicit generator
   * is the wrong one here - [[DoubleArray]], [[DoubleMatrix]], [[FxRate]] and [[FxMatrix]], whose
   * implicit arbitraries hold the well-behaved values - can be checked over the right one: a
   * second implicit of the same type would be ambiguous with the first rather than replacing it.
   *
   * The rendering check is a parameter for the same reason and defaults to comparing against
   * `toString`, which is the contract of every type of the inventory but one:
   * [[CalculationTargetList]] renders the bean form while its `toString` stays the generated
   * product rendering, so it is registered below with the text its rendering must equal.
   * [[Failure]] takes a check of its own for the opposite reason - its `toString` delegates to
   * its `Show`, so the default comparison would assert nothing - and is registered with
   * [[assertFailureRendering]], which states the structure of the rendering instead.
   *
   * @param typeName  the name the rule sets and tests are labelled with
   * @param arb  the generator of values
   * @param rendersAs  the check the rendering of a value has to pass, taking the value and what
   *   the instance produced for it; by default the rendering must equal `toString`
   * @param cogen  the perturbation of values, needed to generate the functions the laws use
   * @param hash  the hashing and equality instance under test
   * @param show  the rendering instance under test
   * @tparam A  the type whose laws are checked
   */
  private def unorderedLawsOf[A](
      typeName: String,
      arb: Arbitrary[A],
      rendersAs: (A, String) => Assertion = (value: A, rendered: String) =>
        rendered shouldBe value.toString)(implicit
      cogen: Cogen[A],
      hash: Hash[A],
      show: Show[A]): Unit = {
    implicit val arbitrary: Arbitrary[A] = arb
    checkAll(s"Eq[$typeName]", EqTests[A](hash).eqv)
    checkAll(s"Hash[$typeName]", HashTests[A](hash).hash)
    test(s"Show renders every $typeName and agrees with its equality") {
      forAll(arb.arbitrary, arb.arbitrary) { (left: A, right: A) =>
        // `cats` states no laws for `Show`, so these are the two properties asserted of one:
        // that it renders what the type renders, which is `rendersAs`, and that two values its
        // `Eq` calls equal render alike - without which a message about a value would depend on
        // which of two equal values reached it. An instance rendering a constant, or the name of
        // its class, or one field of two, fails here rather than passing a non-emptiness check.
        // Non-emptiness is deliberately not asserted: the empty matrix renders as the empty
        // string, which the test at the foot of this file pins.
        rendersAs(left, show.show(left))
        (!hash.eqv(left, right) || show.show(left) == show.show(right)) shouldBe true
      }
    }
  }

  /**
   * Registers the law suites of an ordered type, over its implicit generator.
   *
   * The ordering, hashing and equality arrive as one instance of the intersection type, the shape
   * [[InstanceInventory]] asserts. The test registered here names the property the tie-breaks of
   * these orderings exist for - `compare` returns zero for exactly the pairs equality calls equal
   * - and states it on its own, where the `compare` law of `OrderTests` checks it as one conjunct
   * of three, beside those for `lt` and `gt`. The pairs a tie-break decides are pinned at the
   * foot of this file.
   *
   * @param typeName  the name the rule sets and tests are labelled with
   * @param arb  the generator of values, taken implicitly
   * @param cogen  the perturbation of values, needed to generate the functions the laws use
   * @param orderHash  the single ordering, hashing and equality instance under test
   * @param show  the rendering instance under test
   * @tparam A  the type whose laws are checked
   */
  private def orderedLaws[A](typeName: String)(implicit
      arb: Arbitrary[A],
      cogen: Cogen[A],
      orderHash: Order[A] with Hash[A],
      show: Show[A]): Unit = {
    unorderedLawsOf[A](typeName, arb)(cogen, orderHash, show)
    checkAll(s"Order[$typeName]", OrderTests[A](orderHash).order)
    test(s"compare returns zero exactly where equality holds for $typeName") {
      forAll(arb.arbitrary, arb.arbitrary) { (left: A, right: A) =>
        (orderHash.compare(left, right) == 0) shouldBe orderHash.eqv(left, right)
      }
    }
  }


  //-------------------------------------------------------------------------
  // the law suites of the ordered types, in the order of the inventory
  //-------------------------------------------------------------------------
  orderedLaws[StandardId]("StandardId")

  orderedLaws[Currency]("Currency")
  orderedLaws[CurrencyPair]("CurrencyPair")
  orderedLaws[CurrencyAmount]("CurrencyAmount")
  orderedLaws[Money]("Money")
  orderedLaws[BigMoney]("BigMoney")

  orderedLaws[DayCount]("DayCount")
  orderedLaws[HolidayCalendarId]("HolidayCalendarId")
  orderedLaws[BusinessDayConvention]("BusinessDayConvention")
  orderedLaws[PeriodAdditionConvention]("PeriodAdditionConvention")
  orderedLaws[DateSequence]("DateSequence")
  orderedLaws[Tenor]("Tenor")
  orderedLaws[MarketTenor]("MarketTenor")

  orderedLaws[IborIndex]("IborIndex")
  orderedLaws[OvernightIndex]("OvernightIndex")
  orderedLaws[PriceIndex]("PriceIndex")
  orderedLaws[FxIndex]("FxIndex")
  orderedLaws[FloatingRateType]("FloatingRateType")
  orderedLaws[FloatingRateName]("FloatingRateName")

  orderedLaws[Country]("Country")

  orderedLaws[Frequency]("Frequency")
  orderedLaws[StubConvention]("StubConvention")
  orderedLaws[RollConvention]("RollConvention")
  orderedLaws[SchedulePeriod]("SchedulePeriod")

  orderedLaws[ValueAdjustmentType]("ValueAdjustmentType")

  orderedLaws[Decimal]("Decimal")
  orderedLaws[FixedScaleDecimal]("FixedScaleDecimal")
  orderedLaws[FailureReason]("FailureReason")

  //-------------------------------------------------------------------------
  // the law suites of the unordered types, in the order of the inventory
  //-------------------------------------------------------------------------
  // The one type of the inventory whose `Show` is deliberately not its `toString`: it renders the
  // bean form `renderedTargetList` above states, while `toString` is the product rendering.
  unorderedLawsOf[CalculationTargetList](
    "CalculationTargetList",
    arbCalculationTargetList,
    (list: CalculationTargetList, rendered: String) => rendered shouldBe renderedTargetList(list))
  unorderedLawsOf[ReferenceData.Entry[HolidayCalendar]]("ReferenceData.Entry", arbEntry)

  unorderedLaws[CurrencyAmountArray]("CurrencyAmountArray")
  unorderedLaws[MultiCurrencyAmount]("MultiCurrencyAmount")
  unorderedLaws[MultiCurrencyAmountArray]("MultiCurrencyAmountArray")
  unorderedLaws[Payment]("Payment")
  unorderedLaws[AdjustablePayment]("AdjustablePayment")
  // The two FX types go over the edge-value generators, not their implicit arbitraries, which
  // hold strictly positive finite rates: `FxRate.of` admits a rate that is not a number, its
  // check being `!(rate <= 0.0)`, and a positive infinity, and `FxMatrix.fromMatrix` checks only
  // distinct currencies, a square shape and a unit diagonal, so a matrix may hold `NaN`, either
  // infinity, a signed zero and an off-diagonal pair that are not reciprocals - the values that
  // decide whether the bit-pattern equality of the two is lawful.
  unorderedLawsOf[FxRate]("FxRate", Arbitrary(genEdgeFxRate))
  unorderedLawsOf[FxMatrix]("FxMatrix", Arbitrary(genEdgeFxMatrix))

  unorderedLaws[DayCount.Bus252]("DayCount.Bus252")
  unorderedLaws[HolidayCalendar]("HolidayCalendar")
  unorderedLaws[ImmutableHolidayCalendar]("ImmutableHolidayCalendar")
  unorderedLaws[BusinessDayAdjustment]("BusinessDayAdjustment")
  unorderedLaws[AdjustableDate]("AdjustableDate")
  unorderedLaws[AdjustableDates]("AdjustableDates")
  unorderedLaws[DaysAdjustment]("DaysAdjustment")
  unorderedLaws[PeriodAdjustment]("PeriodAdjustment")
  unorderedLaws[TenorAdjustment]("TenorAdjustment")
  unorderedLaws[SequenceDate]("SequenceDate")

  unorderedLaws[IborIndexObservation]("IborIndexObservation")
  unorderedLaws[OvernightIndexObservation]("OvernightIndexObservation")
  unorderedLaws[PriceIndexObservation]("PriceIndexObservation")
  unorderedLaws[FxIndexObservation]("FxIndexObservation")

  unorderedLaws[Schedule]("Schedule")
  unorderedLaws[PeriodicSchedule]("PeriodicSchedule")

  unorderedLaws[Rounding]("Rounding")
  unorderedLaws[HalfUp]("HalfUp")
  unorderedLaws[ValueAdjustment]("ValueAdjustment")
  unorderedLaws[ValueDerivatives]("ValueDerivatives")
  unorderedLaws[ValueStep]("ValueStep")
  unorderedLaws[ValueStepSequence]("ValueStepSequence")
  unorderedLaws[ValueSchedule]("ValueSchedule")

  // The two numeric wrappers are checked over the edge-value generators rather than their
  // implicit arbitraries, which hold finite values only: without `NaN`, the infinities and the
  // negative zero a generator never presents the values that decide bit-pattern equality.
  unorderedLawsOf[DoubleArray]("DoubleArray", Arbitrary(genDoubleArray))
  unorderedLawsOf[DoubleMatrix]("DoubleMatrix", Arbitrary(genDoubleMatrix))

  // The `toString` of a failure delegates to its `Show`, so the default check would compare the
  // rendering with itself: `assertFailureRendering` above asserts the structure of the rendering
  // instead, reading it rather than rebuilding it. The literal text of both shapes - with and
  // without attributes - is pinned by the test at the foot of this file.
  unorderedLawsOf[Failure]("Failure", arbFailure, assertFailureRendering)

  //-------------------------------------------------------------------------
  // the additive laws of the one type that has them
  //
  // Both arguments are passed by hand. The generator holds finite amounts only, because combining
  // an infinity with an infinity of the opposite sign is not a number and no amount may hold one,
  // so a generator emitting infinities falsifies associativity by reaching the invariant of
  // `CurrencyAmount` rather than by finding this instance unlawful; and the equality is the
  // tolerant one declared above, IEEE-754 addition being only approximately associative. Passing
  // them keeps them out of the `Eq` and `Hash` rule sets of the same type above, which use the
  // production instance and the ordinary generator, infinities included.
  //-------------------------------------------------------------------------
  checkAll(
    "Monoid[MultiCurrencyAmount]",
    MonoidTests[MultiCurrencyAmount]
      .monoid(Arbitrary(genFiniteMultiCurrencyAmount), tolerantMultiCurrencyAmountEq))


  //-------------------------------------------------------------------------
  // the inventory itself
  //-------------------------------------------------------------------------
  test("the inventory holds every promised instance, initialised, and reports them stably") {
    val orderedNames = InstanceInventory.orderedTypes.map(_.typeName).distinct
    val unorderedNames = InstanceInventory.unorderedTypes.map(_.typeName).distinct
    val allNames = InstanceInventory.summoned.map(_.typeName).distinct
    // the three counts of the inventory
    orderedNames should have size 28
    unorderedNames should have size 35
    allNames should have size 63
    // four instances per ordered type, three per unordered one, and the single additive instance
    InstanceInventory.summoned should have size 218
    InstanceInventory.additiveTypes should have size 1
    orderedNames.intersect(unorderedNames) shouldBe empty
    // every summon produced an initialised instance. A companion refers to the companions of the
    // types it is built from, so a cycle in that initialisation order leaves one of these values
    // null rather than failing - which is why the instances are held rather than discarded.
    InstanceInventory.summoned.filter(entry => entry.instance == null) shouldBe empty
    // every name reported is the name the compiler itself gives the type, so a typo in one of the
    // literals above fails here. A nested type is named with a dollar by the runtime and with a
    // dot by a reader, and an entry naming its type argument is compared without it, the runtime
    // carrying neither
    InstanceInventory.summoned.filterNot { entry =>
      entry.runtimeName.replace('$', '.') == entry.typeName.takeWhile(_ != '[')
    } shouldBe empty
    // the report must not vary between runs, which the repeated call asserts
    InstanceInventory.report shouldBe InstanceInventory.report
    InstanceInventory.report should have size 219
    InstanceInventory.report.init.map(_.takeWhile(_ != ' ')).distinct shouldBe
      List(InstanceInventory.LinePrefix)
    InstanceInventory.report.last should startWith(InstanceInventory.SummaryPrefix)
    InstanceInventory.report.init shouldBe InstanceInventory.report.init.sorted
    InstanceInventory.report.foreach(println)
    InstanceInventory.report.last shouldBe
      s"${InstanceInventory.SummaryPrefix} types=63 instances=218 " +
      "cats.Eq=63 cats.Hash=63 cats.Monoid=1 cats.Order=28 cats.Show=63"
  }

  /**
   * Asserts that every member of one closed named family renders as its canonical name.
   *
   * A name is the identity of a member, the key of its JSON form and the text a caller parses
   * back, so an instance rendering anything else would make the two ways of writing a member
   * disagree. A closed family has enumerable members, so this is asserted over '''every''' one of
   * them rather than over the values a generator draws.
   *
   * @param typeName  the name of the family, used to say which member failed
   * @param named  the closed family, which is what makes the members enumerable
   * @param show  the rendering instance under test
   * @tparam A  the named family being checked
   * @return the names of the members checked, so that the caller can count the families
   */
  private def namesRenderedBy[A <: Named](typeName: String)(implicit
      named: NamedEnum[A],
      show: Show[A]): List[String] = {
    val members = named.values.toList
    members.foreach { member =>
      withClue(s"the $typeName member ${member.name} renders as ${show.show(member)}: ") {
        show.show(member) shouldBe member.name
      }
    }
    members.map(member => member.name)
  }

  test("every member of every closed named family renders as its canonical name") {
    // For the fifteen closed families the contract is stronger than the rendering property
    // registered above: the rendering is the canonical `name`, for every member of every family
    // rather than for the members a generator reached. The families are the ones publishing a
    // `NamedEnum`; their count is asserted, so a family added and not added here fails rather
    // than passing unnoticed.
    val families: List[List[String]] = List(
      namesRenderedBy[Currency]("Currency"),
      namesRenderedBy[DayCount]("DayCount"),
      namesRenderedBy[BusinessDayConvention]("BusinessDayConvention"),
      namesRenderedBy[PeriodAdditionConvention]("PeriodAdditionConvention"),
      namesRenderedBy[DateSequence]("DateSequence"),
      namesRenderedBy[IborIndex]("IborIndex"),
      namesRenderedBy[OvernightIndex]("OvernightIndex"),
      namesRenderedBy[PriceIndex]("PriceIndex"),
      namesRenderedBy[FxIndex]("FxIndex"),
      namesRenderedBy[FloatingRateType]("FloatingRateType"),
      namesRenderedBy[FloatingRateName]("FloatingRateName"),
      namesRenderedBy[StubConvention]("StubConvention"),
      namesRenderedBy[RollConvention]("RollConvention"),
      namesRenderedBy[ValueAdjustmentType]("ValueAdjustmentType"),
      namesRenderedBy[FailureReason]("FailureReason"))
    families should have size 15
    families.filter(members => members.isEmpty) shouldBe empty
    // and a calendar-bearing day count, which `DayCount.values` does not hold: it is named for
    // its calendar, and its rendering is that name
    val bus252: DayCount = DayCount.ofBus252(StandardHolidayCalendars.GBLO)
    implicitly[Show[DayCount]].show(bus252) shouldBe bus252.name
    bus252.name shouldBe "Bus/252 GBLO"
  }

  test("every type of the inventory has its law suites registered, and only the ones it claims") {
    // What the summons above cannot say: a registration deleted or never written leaves the
    // inventory still claiming a type whose laws nothing checks, so the inventory's names are
    // compared with the names ScalaTest holds. Discipline names every law after the suite it came
    // from - `Order[Tenor].order.totality` - while the inventory names a type by its qualified
    // name; the leading segments of a package are lower case and those of a type are not, which
    // is what `declaredName` reads it by, so `ReferenceData.Entry` keeps both parts.
    def declaredName(typeName: String): String =
      typeName.takeWhile(character => character != '[').split('.').iterator
        .dropWhile(segment => segment.headOption.exists(character => character.isLower))
        .mkString(".")

    def hasSuite(family: String, typeName: String): Boolean =
      testNames.exists(name => name.startsWith(s"$family[${declaredName(typeName)}]."))

    val orderedNames = InstanceInventory.orderedTypes.map(_.typeName).distinct
    val unorderedNames = InstanceInventory.unorderedTypes.map(_.typeName).distinct
    val additiveNames = InstanceInventory.additiveTypes.map(_.typeName).distinct

    withClue("every ordered type has its equality, hashing and ordering laws registered: ")(
      orderedNames.filterNot(typeName =>
        hasSuite("Eq", typeName) && hasSuite("Hash", typeName) &&
          hasSuite("Order", typeName)) shouldBe empty)
    withClue("every unordered type has its equality and hashing laws registered: ")(
      unorderedNames.filterNot(typeName =>
        hasSuite("Eq", typeName) && hasSuite("Hash", typeName)) shouldBe empty)
    // and the other direction, which is the point of the split: an unordered type has no ordering
    // laws registered, so an ordering added without being inventoried fails here
    withClue("no unordered type has ordering laws registered: ")(
      unorderedNames.filter(typeName => hasSuite("Order", typeName)) shouldBe empty)
    withClue("the additive type has its monoid laws registered: ")(
      additiveNames.filterNot(typeName => hasSuite("Monoid", typeName)) shouldBe empty)
  }

  //-------------------------------------------------------------------------
  // the instances deliberately not offered
  //-------------------------------------------------------------------------
  test("no ordering is offered for a type the port leaves unordered") {
    // Neither of two payments in different currencies is the greater, so `Payment` carries
    // hashing and a rendering and no ordering. The negative is asserted twice because
    // `assertDoesNotCompile` is satisfied by a snippet that merely fails to parse, while
    // `assertTypeError` requires it to parse and then fail to typecheck, which is what a missing
    // implicit is.
    assertDoesNotCompile("""implicitly[Order[Payment]]""")
    assertTypeError("""implicitly[Order[Payment]]""")
    assertCompiles("""implicitly[Hash[Payment]]""")
    assertCompiles("""implicitly[Show[Payment]]""")
  }

  test("no additive instance is offered for a single-currency amount") {
    // Adding two amounts of different currencies has no answer and a `Semigroup` has nowhere to
    // report that it has none, which is why `CurrencyAmount.plus` returns an `Either` and this
    // type carries no additive instance. Combining two multi-currency amounts cannot fail.
    assertDoesNotCompile("""implicitly[Monoid[CurrencyAmount]]""")
    assertTypeError("""implicitly[Monoid[CurrencyAmount]]""")
    assertDoesNotCompile("""implicitly[cats.Semigroup[CurrencyAmount]]""")
    assertTypeError("""implicitly[cats.Semigroup[CurrencyAmount]]""")
    assertCompiles("""implicitly[Monoid[MultiCurrencyAmount]]""")
  }

  test("no domain type is a Foldable or a Traverse") {
    // Those two typeclasses describe a type constructor - something with a hole, such as a list -
    // and no domain type is one, so the summons below fail on the kind of the type rather than on
    // a missing instance: the two are used on the standard collections, never on a domain type.
    assertTypeError("""implicitly[cats.Foldable[MultiCurrencyAmount]]""")
    assertTypeError("""implicitly[cats.Traverse[Schedule]]""")
    assertCompiles("""implicitly[cats.Foldable[List]]""")
  }

  /**
   * Returns whether two instances are one and the same object.
   *
   * `cats.kernel.Eq` extends `Any`, so that a value class can carry one, which is why an instance
   * is not statically a reference and why ScalaTest's `theSameInstanceAs`, taking an `AnyRef`,
   * cannot be handed one. Every instance here is an object, so the conversion is a no-op.
   *
   * @return true if the two are the same object
   */
  private def sameInstance(left: Any, right: Any): Boolean =
    left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]

  test("the rendering of an empty matrix is the empty string, as it was in the original") {
    // The one value of the inventory whose rendering is empty: the rendering appends a line per
    // row, so a matrix of no rows renders as nothing at all. It is pinned here because the
    // property registered for every type deliberately does not assert non-emptiness.
    DoubleMatrix.of().toString shouldBe ""
    implicitly[Show[DoubleMatrix]].show(DoubleMatrix.of()) shouldBe ""
    // the empty array and the empty multi-currency amount render as something a reader can see
    implicitly[Show[DoubleArray]].show(DoubleArray.of()) shouldBe "[]"
    implicitly[Show[MultiCurrencyAmount]].show(MultiCurrencyAmount.empty) shouldBe "[]"
  }

  /**
   * Pins the two rendering shapes of a [[Failure]] as the literal text they produce.
   *
   * The property registered for this type asserts the structure of the rendering it is given,
   * deliberately without rebuilding it; this test states the other half of that argument, the
   * '''literal''' text of both shapes, written here by hand so that the format is pinned by
   * something no change to the instance can move with it. The first shape holds no attributes -
   * the reason, a colon, a space and the message - and the second appends the attributes in key
   * order, asserted with a failure whose attributes are given in the opposite order, which is
   * what makes the rendering a function of the value rather than of the order they arrived in.
   */
  test("a failure renders as the Java form, and as that form and its attributes in key order") {
    val rendering = implicitly[Show[Failure]]
    // the shape without attributes, to the character
    rendering.show(Failure.Invalid("Schedule is invalid")) shouldBe "INVALID: Schedule is invalid"
    rendering.show(Failure.MissingData("No holiday calendar")) shouldBe
      "MISSING_DATA: No holiday calendar"
    // and the shape with attributes, for a failure holding one and for one holding two, whose
    // keys are given in descending order and rendered in ascending order
    rendering.show(
      Failure.Invalid("Schedule is invalid", SortedMap("definition" -> "P3M"))) shouldBe
      "INVALID: Schedule is invalid [definition=P3M]"
    rendering.show(
      Failure.Parsing("Unknown currency", SortedMap("value" -> "XYZ", "scale" -> "2"))) shouldBe
      "PARSING: Unknown currency [scale=2, value=XYZ]"
    // The `toString` of a member is that rendering and deliberately so: a failure is written out
    // in one form whichever path writes it - the instance, interpolation, or a library calling
    // `toString`. It is also why the property above checks this type against a structure of its
    // own: `toString` is the rendering, so comparing the two would say nothing.
    Failure.Invalid("Schedule is invalid").toString shouldBe
      rendering.show(Failure.Invalid("Schedule is invalid"))
    Failure.Parsing("Unknown currency", SortedMap("value" -> "XYZ", "scale" -> "2")).toString shouldBe
      "PARSING: Unknown currency [scale=2, value=XYZ]"
  }

  test("one implicit answers for equality, ordering and hashing alike") {
    // `Hash` and `Order` both extend `Eq`, so a type declaring either has declared an `Eq` as
    // well, and the proof that no second one is declared is that these summons resolve at all: an
    // `Eq` of its own beside a `Hash` would make every one of them ambiguous. The instance
    // recovered is the very same object, which is what makes it impossible for the three to
    // disagree.
    assertCompiles("""implicitly[Eq[Tenor]]""")
    assertCompiles("""implicitly[Eq[Payment]]""")
    sameInstance(implicitly[Eq[Tenor]], implicitly[Order[Tenor]]) shouldBe true
    sameInstance(implicitly[Eq[Tenor]], implicitly[Hash[Tenor]]) shouldBe true
    sameInstance(implicitly[Eq[Payment]], implicitly[Hash[Payment]]) shouldBe true
    sameInstance(implicitly[Eq[CurrencyAmount]], implicitly[Order[CurrencyAmount]]) shouldBe true
  }

  //-------------------------------------------------------------------------
  // divergence one: the bit-pattern equality of the double-bearing types
  //
  // Every type carrying a `Double` or an array of them compares its fields with
  // `java.lang.Double.compare` and `java.util.Arrays.equals`, hashing them with the matching
  // `hashCode`, so it compares the bits rather than the numbers. Two consequences follow that the
  // law suites cannot state, a generator having to stumble on the values: a value holding a `NaN`
  // equals itself, and a negative zero differs from a positive one, where `==` on the primitive
  // would answer the other way in both cases.
  //-------------------------------------------------------------------------
  /**
   * Asserts that two separately built values are equal, by their own equality and by the instance.
   *
   * The two values are built by two calls to the same factory rather than shared, so reference
   * equality cannot satisfy the assertion, and the hashes are compared as well, an equality its
   * hashing disagrees with being what breaks a hash map.
   *
   * @return the assertion that the two agree
   */
  private def assertEqualByBits[A](left: A, right: A)(implicit hash: Hash[A]): Assertion = {
    (left == right) shouldBe true
    hash.eqv(left, right) shouldBe true
    left.hashCode shouldBe right.hashCode
    hash.hash(left) shouldBe hash.hash(right)
  }

  /** Asserts that two values differing only in the sign of a zero are not equal. */
  private def assertDistinctByBits[A](negative: A, positive: A)(implicit
      hash: Hash[A]): Assertion = {
    (negative == positive) shouldBe false
    hash.eqv(negative, positive) shouldBe false
  }

  test("a value holding NaN equals itself and hashes with itself") {
    // the primitive comparison this equality deliberately does not use
    (Double.NaN == Double.NaN) shouldBe false
    java.lang.Double.compare(Double.NaN, Double.NaN) shouldBe 0
    val derivatives = DoubleArray.of(1.0d, Double.NaN, Double.NegativeInfinity)
    assertEqualByBits(
      DoubleArray.of(1.0d, Double.NaN, Double.NegativeInfinity),
      DoubleArray.of(1.0d, Double.NaN, Double.NegativeInfinity))
    assertEqualByBits(
      DoubleMatrix.of(1, 2, Double.NaN, 1.0d),
      DoubleMatrix.of(1, 2, Double.NaN, 1.0d))
    assertEqualByBits(
      ValueDerivatives.of(Double.NaN, derivatives),
      ValueDerivatives.of(Double.NaN, derivatives))
    assertEqualByBits(
      ValueAdjustment.ofReplace(Double.NaN),
      ValueAdjustment.ofReplace(Double.NaN))
    assertEqualByBits(
      valueOf(ValueStep.of(1, ValueAdjustment.ofReplace(Double.NaN))),
      valueOf(ValueStep.of(1, ValueAdjustment.ofReplace(Double.NaN))))
    assertEqualByBits(valueOf(ValueSchedule.of(Double.NaN)), valueOf(ValueSchedule.of(Double.NaN)))
    // the two currency arrays are deliberately absent: their element invariant rejects a value
    // that is not a number at construction, so neither can hold one to be compared
  }

  test("a value holding an infinity equals itself and hashes with itself") {
    // `CurrencyAmount` rejects a value that is not a number and accepts the infinities, so an
    // infinite amount is a value these types hold
    val infinite = valueOf(CurrencyAmount.of(gbp, Double.PositiveInfinity))
    assertEqualByBits(infinite, valueOf(CurrencyAmount.of(gbp, Double.PositiveInfinity)))
    assertEqualByBits(
      valueOf(MultiCurrencyAmount.of(infinite)),
      valueOf(MultiCurrencyAmount.of(valueOf(CurrencyAmount.of(gbp, Double.PositiveInfinity)))))
    assertEqualByBits(
      valueOf(FxRate.of(gbpUsd, Double.PositiveInfinity)),
      valueOf(FxRate.of(gbpUsd, Double.PositiveInfinity)))
    // a rate of zero has an infinite reciprocal, which the matrix holds as the rate the other way
    val zeroRate = FxMatrix.of(gbpUsd, 0.0d)
    zeroRate.rates.get(1, 0).isPosInfinity shouldBe true
    assertEqualByBits(zeroRate, FxMatrix.of(gbpUsd, 0.0d))
  }

  test("the two zeroes are distinct, except where a factory normalises one away") {
    // the primitive comparison this equality deliberately does not use
    (-0.0d == 0.0d) shouldBe true
    java.lang.Double.compare(-0.0d, 0.0d) should be < 0
    assertDistinctByBits(DoubleArray.of(-0.0d), DoubleArray.of(0.0d))
    assertDistinctByBits(DoubleMatrix.of(1, 1, -0.0d), DoubleMatrix.of(1, 1, 0.0d))
    assertDistinctByBits(
      ValueDerivatives.of(-0.0d, DoubleArray.of(1.0d)),
      ValueDerivatives.of(0.0d, DoubleArray.of(1.0d)))
    assertDistinctByBits(ValueAdjustment.ofReplace(-0.0d), ValueAdjustment.ofReplace(0.0d))
    assertDistinctByBits(
      valueOf(ValueStep.of(1, ValueAdjustment.ofReplace(-0.0d))),
      valueOf(ValueStep.of(1, ValueAdjustment.ofReplace(0.0d))))
    assertDistinctByBits(valueOf(ValueSchedule.of(-0.0d)), valueOf(ValueSchedule.of(0.0d)))
    assertDistinctByBits(
      CurrencyAmountArray.of(gbp, DoubleArray.of(-0.0d)),
      CurrencyAmountArray.of(gbp, DoubleArray.of(0.0d)))
    assertDistinctByBits(
      valueOf(MultiCurrencyAmountArray.of(Map(gbp -> DoubleArray.of(-0.0d)))),
      valueOf(MultiCurrencyAmountArray.of(Map(gbp -> DoubleArray.of(0.0d)))))
  }

  test("an amount normalises a negative zero away, so its equality never sees one") {
    // The one exception to the rule above, and a property of the factory rather than of the
    // equality: `CurrencyAmount.of` maps a negative zero to a positive one, so no amount holds a
    // negative zero and the bit comparison its equality performs is never presented with one. The
    // amounts are compared by their bits here rather than by `==`, which would hold either way
    val negativeZero = valueOf(CurrencyAmount.of(gbp, -0.0d))
    java.lang.Double.doubleToLongBits(-0.0d) should not be 0L
    java.lang.Double.doubleToLongBits(negativeZero.amount) shouldBe 0L
    java.lang.Double.compare(negativeZero.amount, 0.0d) shouldBe 0
    assertEqualByBits(negativeZero, valueOf(CurrencyAmount.of(gbp, 0.0d)))
    // and a value that is not a number is not an amount at all
    CurrencyAmount.of(gbp, Double.NaN).isLeft shouldBe true
    MultiCurrencyAmount.of(gbp, Double.NaN).isLeft shouldBe true
    // the same normalisation reaches the multi-currency amount, which is built from amounts
    val multiNegativeZero = valueOf(MultiCurrencyAmount.of(gbp, -0.0d))
    java.lang.Double.doubleToLongBits(multiNegativeZero.getAmountOrZero(gbp).amount) shouldBe 0L
    assertEqualByBits(multiNegativeZero, valueOf(MultiCurrencyAmount.of(gbp, 0.0d)))
  }


  //-------------------------------------------------------------------------
  // divergence two: an ordering that agrees with equality
  //
  // `Order` requires that `compare` return zero exactly where the values are equal, which a
  // comparison reading fewer fields than its equality does not. Four of these orderings therefore
  // continue past their leading keys over the remaining fields, so a pair the leading keys rank
  // strictly keeps its order and only a pair they call equal is decided further. The tests below
  // pin the pairs the continuation decides, and the monetary ordering, by currency then amount.
  //-------------------------------------------------------------------------
  test("a fixed-scale decimal is ordered by its decimal and then by its scale") {
    val decimal = valueOf(Decimal.of("12.3"))
    val atOnePlace = valueOf(FixedScaleDecimal.of(decimal, 1))
    val atFourPlaces = valueOf(FixedScaleDecimal.of(decimal, 4))
    val ordering = implicitly[Order[FixedScaleDecimal]]
    // the two hold the same amount shown to a different number of places, so the leading key of
    // the ordering, the decimal, ranks them equal
    atOnePlace.decimal.compareTo(atFourPlaces.decimal) shouldBe 0
    atOnePlace.toString shouldBe "12.3"
    atFourPlaces.toString shouldBe "12.3000"
    // while equality holds them unequal, so the continuation over the scale decides them
    ordering.eqv(atOnePlace, atFourPlaces) shouldBe false
    ordering.compare(atOnePlace, atFourPlaces) should be < 0
    ordering.compare(atFourPlaces, atOnePlace) should be > 0
    ordering.compare(atOnePlace, valueOf(FixedScaleDecimal.of(decimal, 1))) shouldBe 0
  }

  test("a tenor is ordered by its length and then by its name") {
    val twelveMonths = valueOf(Tenor.ofMonths(12))
    val oneYear = valueOf(Tenor.ofYears(1))
    val ordering = implicitly[Order[Tenor]]
    // `compareTo`, the leading key of the ordering, ranks these two equal - twelve months and a
    // year are the same length - while equality holds them unequal, months not being normalised
    // into years
    twelveMonths.compareTo(oneYear) shouldBe 0
    ordering.eqv(twelveMonths, oneYear) shouldBe false
    twelveMonths.name shouldBe "12M"
    oneYear.name shouldBe "1Y"
    // the tie-break by name decides them, putting `12M` before `1Y`
    ordering.compare(twelveMonths, oneYear) should be < 0
    ordering.compare(oneYear, twelveMonths) should be > 0
    ordering.compare(twelveMonths, valueOf(Tenor.ofMonths(12))) shouldBe 0
    // and the tie-break cannot reorder a pair `compareTo` ranks strictly, following it
    ordering.compare(valueOf(Tenor.ofMonths(3)), oneYear) should be < 0
  }

  test("a market tenor is ordered by its length and then by its code") {
    val twelveMonths = valueOf(MarketTenor.ofSpotMonths(12))
    val oneYear = valueOf(MarketTenor.ofSpotYears(1))
    val ordering = implicitly[Order[MarketTenor]]
    twelveMonths.compareTo(oneYear) shouldBe 0
    ordering.eqv(twelveMonths, oneYear) shouldBe false
    twelveMonths.code shouldBe "12M"
    oneYear.code shouldBe "1Y"
    ordering.compare(twelveMonths, oneYear) should be < 0
    ordering.compare(oneYear, twelveMonths) should be > 0
    // `ON`, `TN` and `SN` are ordered by the spot lag indicator `compareTo` reads first
    ordering.compare(MarketTenor.ON, MarketTenor.TN) should be < 0
    ordering.compare(MarketTenor.TN, MarketTenor.SN) should be < 0
  }

  test("a schedule period is ordered by its unadjusted dates and then by its adjusted dates") {
    val adjusted = valueOf(SchedulePeriod.of(jan15, apr15, jan14, apr14))
    val unadjusted = valueOf(SchedulePeriod.of(jan14, apr14, jan14, apr14))
    val ordering = implicitly[Order[SchedulePeriod]]
    // the leading keys are the two unadjusted dates, which these two share: equal periodic dates
    // falling on different business days
    adjusted.unadjustedStartDate shouldBe unadjusted.unadjustedStartDate
    adjusted.unadjustedEndDate shouldBe unadjusted.unadjustedEndDate
    adjusted.startDate should not be unadjusted.startDate
    ordering.eqv(adjusted, unadjusted) shouldBe false
    // the continuation over the adjusted dates decides them
    ordering.compare(adjusted, unadjusted) should be > 0
    ordering.compare(unadjusted, adjusted) should be < 0
    ordering.compare(adjusted, valueOf(SchedulePeriod.of(jan15, apr15, jan14, apr14))) shouldBe 0
  }

  test("a monetary amount is ordered by its currency and then by its amount") {
    val hundred = valueOf(Decimal.of("100"))
    val twoHundred = valueOf(Decimal.of("200"))
    val one = valueOf(Decimal.of("1"))
    val amountOrdering = implicitly[Order[CurrencyAmount]]
    val moneyOrdering = implicitly[Order[Money]]
    val bigMoneyOrdering = implicitly[Order[BigMoney]]
    // the currency decides first, so the smaller amount of the earlier currency is the smaller
    // value, and the amount decides the pairs of one currency
    amountOrdering.compare(
      valueOf(CurrencyAmount.of(gbp, 100.0d)),
      valueOf(CurrencyAmount.of(usd, 1.0d))) should be < 0
    amountOrdering.compare(
      valueOf(CurrencyAmount.of(gbp, 100.0d)),
      valueOf(CurrencyAmount.of(gbp, 200.0d))) should be < 0
    moneyOrdering.compare(Money.of(gbp, hundred), Money.of(usd, one)) should be < 0
    moneyOrdering.compare(Money.of(gbp, hundred), Money.of(gbp, twoHundred)) should be < 0
    moneyOrdering.compare(Money.of(gbp, hundred), Money.of(gbp, hundred)) shouldBe 0
    bigMoneyOrdering.compare(BigMoney.of(gbp, hundred), BigMoney.of(usd, one)) should be < 0
    bigMoneyOrdering.compare(
      BigMoney.of(gbp, hundred),
      BigMoney.of(gbp, twoHundred)) should be < 0
    bigMoneyOrdering.compare(BigMoney.of(gbp, hundred), BigMoney.of(gbp, hundred)) shouldBe 0
  }

  //-------------------------------------------------------------------------
  // divergence three: the two restrictions the additive laws are stated under
  //
  // The rule set registered above passes a finite generator and a tolerant equality by hand. The
  // tests below say what that instance computes, which the laws - being statements about identity,
  // associativity and what follows from them - do not, and why the tolerance is needed.
  //-------------------------------------------------------------------------
  test("the additive identity holds no amounts and leaves its partner unchanged") {
    val additive = implicitly[Monoid[MultiCurrencyAmount]]
    val amount = valueOf(MultiCurrencyAmount.of(valueOf(CurrencyAmount.of(gbp, 100.0d))))
    additive.empty shouldBe MultiCurrencyAmount.empty
    additive.empty.size shouldBe 0
    additive.empty.toMap shouldBe empty
    additive.empty.getAmounts shouldBe empty
    additive.combine(amount, additive.empty) shouldBe amount
    additive.combine(additive.empty, amount) shouldBe amount
    additive.combineAll(List.empty[MultiCurrencyAmount]) shouldBe MultiCurrencyAmount.empty
  }

  test("combining multi-currency amounts sums each currency over a map held in code order") {
    val additive = implicitly[Monoid[MultiCurrencyAmount]]
    val left = valueOf(
      MultiCurrencyAmount.of(
        valueOf(CurrencyAmount.of(gbp, 100.0d)),
        valueOf(CurrencyAmount.of(usd, 200.0d))))
    val right = valueOf(
      MultiCurrencyAmount.of(
        valueOf(CurrencyAmount.of(gbp, 50.0d)),
        valueOf(CurrencyAmount.of(eur, 10.0d))))
    val combined = additive.combine(left, right)
    // the currency held by both sides is summed, the currencies held by one are carried across,
    // and the result is a sorted map, so the entries arrive in the order of the currency codes
    // however the values were combined, which is what makes the JSON of a value byte-stable
    combined.toMap.toList.map { case (currency, amount) => (currency.code, amount) } shouldBe
      List(("EUR", 10.0d), ("GBP", 150.0d), ("USD", 200.0d))
    combined.toMap shouldBe a[SortedMap[_, _]]
    // aggregating a collection agrees with combining the values one at a time, in either order
    additive.combineAll(List(left, right)) shouldBe combined
    additive.combineAll(List(right, left)) shouldBe combined
  }

  test("total merges a repeated currency where of rejects it") {
    val hundred = valueOf(CurrencyAmount.of(gbp, 100.0d))
    val fifty = valueOf(CurrencyAmount.of(gbp, 50.0d))
    // the two factories of the type are deliberately separate: one totals a repeated currency,
    // the other reports it, and neither is a safe default for the other's use
    MultiCurrencyAmount.total(List(hundred, fifty)).toMap.toList.map { case (currency, amount) =>
      (currency.code, amount)
    } shouldBe List(("GBP", 150.0d))
    MultiCurrencyAmount.of(List(hundred, fifty)).isLeft shouldBe true
    MultiCurrencyAmount.of(List(hundred)).isRight shouldBe true
  }

  test("the tolerant equality of the additive laws is weaker than the equality of the type") {
    // The rule set above compares with `tolerantMultiCurrencyAmountEq` rather than with the
    // instance of the type, floating-point addition being only approximately associative. This is
    // that difference made visible: two values a rounding apart are one value to the tolerance and
    // two to the type. The tolerance is not blanket - it separates amounts that genuinely differ,
    // and values holding different currencies - so the associativity it checks is still a
    // statement about this instance.
    val exact = valueOf(MultiCurrencyAmount.of(gbp, 1.0d))
    val rounded = valueOf(MultiCurrencyAmount.of(gbp, 1.0d + 1.0e-12d))
    val doubled = valueOf(MultiCurrencyAmount.of(gbp, 2.0d))
    val elsewhere = valueOf(MultiCurrencyAmount.of(usd, 1.0d))
    implicitly[Hash[MultiCurrencyAmount]].eqv(exact, rounded) shouldBe false
    tolerantMultiCurrencyAmountEq.eqv(exact, rounded) shouldBe true
    tolerantMultiCurrencyAmountEq.eqv(exact, exact) shouldBe true
    tolerantMultiCurrencyAmountEq.eqv(exact, doubled) shouldBe false
    tolerantMultiCurrencyAmountEq.eqv(exact, elsewhere) shouldBe false
    tolerantMultiCurrencyAmountEq.eqv(exact, MultiCurrencyAmount.empty) shouldBe false
    // and the reason the generator of that rule set is the finite one: an infinity combined with
    // an infinity of the opposite sign is not a number, which no amount may hold
    val positive = valueOf(MultiCurrencyAmount.of(gbp, Double.PositiveInfinity))
    val negative = valueOf(MultiCurrencyAmount.of(gbp, Double.NegativeInfinity))
    an[IllegalArgumentException] should be thrownBy positive.plus(negative)
  }
}
