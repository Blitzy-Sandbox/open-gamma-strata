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
 * The `cats` instances both modules of this port promise, summoned at compile time.
 *
 * This object is the inventory of the port's typeclass instances, and its value is that it
 * '''compiles'''. Every entry below asks the compiler for an instance by type; an instance that
 * is missing, or that is offered twice and so cannot be chosen between, is a compile error in
 * this file rather than a gap a reader has to notice. A list of names checked by a regular
 * expression could not do that: it would still match after the instance it names had been
 * deleted.
 *
 * ===What each entry asserts===
 *
 * The two helpers encode the port's "one implicit per type" rule directly in their implicit
 * parameter lists, so an entry is a statement about the '''shape''' of a companion's instances
 * and not only about their presence:
 *
 *  - [[InstanceInventory.ordered]] demands a single implicit of type `Order[A] with Hash[A]`.
 *    A companion that declared a separate `Order[A]` and a separate `Hash[A]` would not satisfy
 *    it, even though both instances existed, because neither alone has the intersection type.
 *    That is the point: two independently declared instances can disagree, and an ordering that
 *    disagrees with equality breaks the `Order` laws checked in [[TypeclassLawsSpec]].
 *  - [[InstanceInventory.hashed]] demands `Hash[A]` and `Show[A]` for a type the port
 *    deliberately leaves unordered.
 *  - both then summon `Eq[A]` from what they were given. `Hash` and `Order` each extend `Eq`, so
 *    this resolves by subtyping to the one instance in scope. Were a companion to declare an
 *    `Eq[A]` of its own beside its `Hash[A]`, the summon would become ambiguous and this file
 *    would stop compiling - which is exactly the outcome wanted, since the second instance would
 *    be a second answer to the same question.
 *
 * ===Coverage===
 *
 * The entries are the complete set of types in `strata-basics` and in the ported subset of
 * `strata-collect` that carry an instance: 28 ordered types, 35 unordered ones and the single
 * additive instance of [[MultiCurrencyAmount]], for 218 instances in all. The counts are
 * printed by [[InstanceInventory.report]] and asserted in [[TypeclassLawsSpec]], so a type added
 * to one list and forgotten in the other cannot pass unnoticed.
 *
 * ===Which members of the sealed families appear===
 *
 * `Hash` and `Show` are '''invariant''', so `Hash[HalfUp]` is not `Hash[Rounding]`: a member is
 * served by the family's instance only for a value typed as the family, and a value typed as the
 * member needs an instance declared at that type. Three members are value types in their own
 * right in the port's construction inventory - `Rounding.HalfUp` and `DayCount.Bus252` are
 * validated types and `ImmutableHolidayCalendar` a normalising one - and the inventory requires
 * `Hash` and `Show` of every validated, normalising, registry-backed, sealed-family and total
 * type, so each of the three declares the pair at its own type and appears here in its own right.
 * That is where the 35 unordered types come from: the 32 families and plain products, plus those
 * three members.
 *
 * The other members declare nothing of their own and appear nowhere here, which is equally
 * deliberate: `NoRounding`, the three weekend calendars, the two calendar compositions and the
 * ten `Failure` cases are not separate types of the construction inventory - it lists `Rounding`,
 * `HolidayCalendar` and `Failure` as sealed families and names no member of them beyond the three
 * above - so each is covered by the law suites of its family, which the generators of
 * [[Arbitraries]] feed with the members.
 *
 * The types with no instance at all are the ones with no data to compare: the contract and
 * function types (`ReferenceData`, `ReferenceDataId`, `DateAdjuster`, `FxRateProvider`,
 * `FxConvertible`, `Resolvable`, `CalculationTarget`, `Index` and its two intermediate traits,
 * `IndexObservation`, `DayCount.ScheduleInfo`, `Matrix`) and the helpers and typeclasses
 * themselves. Two values of a function type are not equal in any useful sense, and hashing one
 * says nothing, so an instance for them would be noise rather than coverage.
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
   * The intersection type of the first parameter is the assertion. The port declares exactly one
   * equality-bearing implicit per type, and for an ordered type that implicit is declared as
   * `Order[X] with Hash[X]`; nothing else satisfies this parameter, so a companion that split it
   * into two instances fails to compile here.
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
   * No `Order` is asked for, and for these types none exists - which
   * [[TypeclassLawsSpec]] proves against the compiler rather than by omission.
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
   * Summons the additive instance of a type.
   *
   * One type has one, and the inventory says which: [[MultiCurrencyAmount]]. The instance is
   * summoned here and its laws are checked in [[TypeclassLawsSpec]] under the two documented
   * restrictions of the migration note.
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
  /**
   * The ordered types: the ones comparable in the library being ported, together with the closed
   * named families, which compare by name.
   *
   * The Java types implementing `Comparable` are `Currency`, `CurrencyAmount`, `Money`,
   * `BigMoney`, `Tenor`, `MarketTenor`, `Country`, `StandardId` and `SchedulePeriod`; `Decimal`,
   * `FixedScaleDecimal`, `HolidayCalendarId`, `CurrencyPair` and `Frequency` are ordered by this
   * port as well, and so is every one of the fifteen named families, by name.
   */
  val orderedTypes: List[Summoned] =
    // the root package
    ordered[StandardId]("com.opengamma.strata.basics.StandardId") :::
      // currency
      ordered[Currency]("com.opengamma.strata.basics.currency.Currency") :::
      ordered[CurrencyPair]("com.opengamma.strata.basics.currency.CurrencyPair") :::
      ordered[CurrencyAmount]("com.opengamma.strata.basics.currency.CurrencyAmount") :::
      ordered[Money]("com.opengamma.strata.basics.currency.Money") :::
      ordered[BigMoney]("com.opengamma.strata.basics.currency.BigMoney") :::
      // date
      ordered[DayCount]("com.opengamma.strata.basics.date.DayCount") :::
      ordered[HolidayCalendarId]("com.opengamma.strata.basics.date.HolidayCalendarId") :::
      ordered[BusinessDayConvention]("com.opengamma.strata.basics.date.BusinessDayConvention") :::
      ordered[PeriodAdditionConvention](
        "com.opengamma.strata.basics.date.PeriodAdditionConvention") :::
      ordered[DateSequence]("com.opengamma.strata.basics.date.DateSequence") :::
      ordered[Tenor]("com.opengamma.strata.basics.date.Tenor") :::
      ordered[MarketTenor]("com.opengamma.strata.basics.date.MarketTenor") :::
      // index
      ordered[IborIndex]("com.opengamma.strata.basics.index.IborIndex") :::
      ordered[OvernightIndex]("com.opengamma.strata.basics.index.OvernightIndex") :::
      ordered[PriceIndex]("com.opengamma.strata.basics.index.PriceIndex") :::
      ordered[FxIndex]("com.opengamma.strata.basics.index.FxIndex") :::
      ordered[FloatingRateType]("com.opengamma.strata.basics.index.FloatingRateType") :::
      ordered[FloatingRateName]("com.opengamma.strata.basics.index.FloatingRateName") :::
      // location
      ordered[Country]("com.opengamma.strata.basics.location.Country") :::
      // schedule
      ordered[Frequency]("com.opengamma.strata.basics.schedule.Frequency") :::
      ordered[StubConvention]("com.opengamma.strata.basics.schedule.StubConvention") :::
      ordered[RollConvention]("com.opengamma.strata.basics.schedule.RollConvention") :::
      ordered[SchedulePeriod]("com.opengamma.strata.basics.schedule.SchedulePeriod") :::
      // value
      ordered[ValueAdjustmentType]("com.opengamma.strata.basics.value.ValueAdjustmentType") :::
      // the ported subset of strata-collect
      ordered[Decimal]("com.opengamma.strata.collect.Decimal") :::
      ordered[FixedScaleDecimal]("com.opengamma.strata.collect.FixedScaleDecimal") :::
      ordered[FailureReason]("com.opengamma.strata.collect.result.FailureReason")

  /**
   * The unordered types: every other value type of the two modules.
   *
   * None of these was comparable in the library being ported and no ordering of them means
   * anything - neither of two payments in different currencies is the greater, and two holiday
   * calendars are not ranked - so each carries hashing, equality and a rendering and no more.
   *
   * Three of the entries are members of a sealed family rather than a family or a plain product:
   * `DayCount.Bus252`, `ImmutableHolidayCalendar` and `HalfUp`, each of which is a value type of
   * the construction inventory in its own right and each of which therefore declares the pair of
   * instances at its own type, as the invariance of those typeclasses requires. None of the three
   * is ordered: the ordering their families have is by name for the day counts and absent for the
   * other two, and a member that is one value among many of its kind - a rounding to four places,
   * a calendar of one centre - is no more comparable than the family it belongs to.
   */
  val unorderedTypes: List[Summoned] =
    // the root package
    hashed[CalculationTargetList]("com.opengamma.strata.basics.CalculationTargetList") :::
      hashed[ReferenceData.Entry[HolidayCalendar]](
        "com.opengamma.strata.basics.ReferenceData.Entry") :::
      // currency
      hashed[CurrencyAmountArray]("com.opengamma.strata.basics.currency.CurrencyAmountArray") :::
      hashed[MultiCurrencyAmount]("com.opengamma.strata.basics.currency.MultiCurrencyAmount") :::
      hashed[MultiCurrencyAmountArray](
        "com.opengamma.strata.basics.currency.MultiCurrencyAmountArray") :::
      hashed[Payment]("com.opengamma.strata.basics.currency.Payment") :::
      hashed[AdjustablePayment]("com.opengamma.strata.basics.currency.AdjustablePayment") :::
      hashed[FxRate]("com.opengamma.strata.basics.currency.FxRate") :::
      hashed[FxMatrix]("com.opengamma.strata.basics.currency.FxMatrix") :::
      // date
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
      // index
      hashed[IborIndexObservation]("com.opengamma.strata.basics.index.IborIndexObservation") :::
      hashed[OvernightIndexObservation](
        "com.opengamma.strata.basics.index.OvernightIndexObservation") :::
      hashed[PriceIndexObservation]("com.opengamma.strata.basics.index.PriceIndexObservation") :::
      hashed[FxIndexObservation]("com.opengamma.strata.basics.index.FxIndexObservation") :::
      // schedule
      hashed[Schedule]("com.opengamma.strata.basics.schedule.Schedule") :::
      hashed[PeriodicSchedule]("com.opengamma.strata.basics.schedule.PeriodicSchedule") :::
      // value
      hashed[Rounding]("com.opengamma.strata.basics.value.Rounding") :::
      hashed[HalfUp]("com.opengamma.strata.basics.value.HalfUp") :::
      hashed[ValueAdjustment]("com.opengamma.strata.basics.value.ValueAdjustment") :::
      hashed[ValueDerivatives]("com.opengamma.strata.basics.value.ValueDerivatives") :::
      hashed[ValueStep]("com.opengamma.strata.basics.value.ValueStep") :::
      hashed[ValueStepSequence]("com.opengamma.strata.basics.value.ValueStepSequence") :::
      hashed[ValueSchedule]("com.opengamma.strata.basics.value.ValueSchedule") :::
      // the ported subset of strata-collect
      hashed[DoubleArray]("com.opengamma.strata.collect.array.DoubleArray") :::
      hashed[DoubleMatrix]("com.opengamma.strata.collect.array.DoubleMatrix") :::
      hashed[Failure]("com.opengamma.strata.collect.result.Failure")

  /** The one additive instance of the port. */
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
   * The lines are sorted and hold nothing but the two names, so two runs of the suite produce
   * byte-identical output and the report can be copied into the gate report as it stands. No
   * identity hash code, no timestamp and no instance rendering appears, because any of the three
   * would make two runs differ.
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
 * Checks the `cats` instances of both modules against the laws of their typeclasses, and the
 * three places where this port's instances deliberately differ from the library being ported.
 *
 * This is the suite the "typeclass instances" acceptance gate runs, and it is the only file in
 * the repository that uses `cats-laws` and `discipline-scalatest`; the build scopes those two
 * libraries to this module's tests for it alone.
 *
 * ===The four things checked===
 *
 *  1. '''That the instances exist and are unique.''' [[InstanceInventory]] summons every one of
 *     them, so this file does not compile unless they do. That check is stronger than a search
 *     of the sources for the text of a declaration, which would still match a declaration that
 *     had stopped being an implicit or had been given a type nothing can use, and it is the form
 *     the gate calls for. The inventory is printed, sorted and without anything that varies
 *     between runs, by the first test below.
 *  1. '''That the instances obey their laws.''' Every type gets `EqTests` and `HashTests`; the
 *     ordered ones also get `OrderTests`; [[MultiCurrencyAmount]] also gets `MonoidTests`. The
 *     laws are what make the instances substitutable for one another - an `Eq` that is not
 *     transitive or a `Hash` that disagrees with its own `Eq` breaks every collection and every
 *     property built on it - and the library being ported had no equivalent check: it swept its
 *     beans reflectively for a non-null `toString` and a self-consistent `equals`, which is a
 *     small part of what is asserted here.
 *  1. '''That absent instances stay absent.''' Three of the port's decisions are statements
 *     about what does '''not''' exist: no ordering for the unordered types, no additive instance
 *     for [[CurrencyAmount]], and no `Foldable` or `Traverse` for any domain type. Nothing a
 *     running test does can observe an absence, so each is asserted against the compiler, and
 *     each negative is paired with the nearest positive so that a snippet failing for the wrong
 *     reason cannot pass unnoticed.
 *  1. '''That the documented divergences hold.''' The migration note records three, and this is
 *     where each is proved: bit-pattern equality on the types carrying a `Double`, an ordering
 *     that agrees with equality where the Java comparison did not, and the two restrictions the
 *     additive laws of [[MultiCurrencyAmount]] are stated under.
 *
 * ===How the generated values are chosen===
 *
 * The generators come from [[Arbitraries]], which is the module's single generator source, and
 * every value they produce is built by the type's own factory. Two choices are made here rather
 * than there, and both are deliberate:
 *
 *  - [[DoubleArray]] and [[DoubleMatrix]] are drawn from the '''edge-value''' generators
 *    `genDoubleArray` and `genDoubleMatrix` rather than from their implicit arbitraries, which
 *    are the finite ones. The equality of those two types is the bit-pattern equality this file
 *    exists to check, so the values it checks it over have to include `NaN`, the infinities and
 *    both zeroes.
 *  - [[MultiCurrencyAmount]] is drawn from `genFiniteMultiCurrencyAmount` for its additive laws
 *    alone, for the reason the migration note gives and the comment on that rule set repeats.
 *    Its `EqTests` and `HashTests` use the ordinary generator, which produces infinities.
 */
class TypeclassLawsSpec
    extends AnyFunSuite
    with Matchers
    with ScalaCheckPropertyChecks
    with FunSuiteDiscipline {

  /**
   * The property configuration of every rule set and property in this suite.
   *
   * The inventory is sixty-three types and the rule sets over them are more than a thousand
   * properties, several of which generate a function as well as its arguments, so the number of
   * cases each property is run at is the one thing in this suite worth choosing carefully. A
   * hundred is ten times the ScalaTest default and was settled by measurement: the whole suite
   * takes about two and a half seconds at that number, against about two at ten and about three
   * at four hundred, because the generators of [[Arbitraries]] are bounded and the operations
   * under test are comparisons. There is therefore nothing to buy by lowering it, and the type
   * coverage - which is what this suite is for - is never traded for time at all. The IEEE-754
   * edge values that decide the equality of a type are asserted directly by the tests at the
   * foot of this file rather than left to a generator to stumble on.
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
   * The fixtures below are built through the validated factories of the types they belong to, so
   * a fixture that cannot be built is a broken test rather than a failed assertion. This reports
   * it as one, naming the failure the factory gave, so that a fixture invalidated by a change to
   * a validation rule says which rule rejected it.
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
   * [[ReferenceData.Entry]] is a plain product with a public constructor, so this builds one
   * directly - there is no factory to bypass - and it is generated here rather than in
   * [[Arbitraries]] because this suite is its only consumer: an entry carries no JSON codec,
   * being one half of a heterogeneous store that is deliberately not serializable.
   *
   * The identifier is the calendar's own, so a generated entry is one a reference data store
   * could actually hold. Its value type is fixed to [[HolidayCalendar]] because the instances of
   * an entry are given for every value type, so checking them at one type checks them at all.
   */
  private val arbEntry: Arbitrary[ReferenceData.Entry[HolidayCalendar]] =
    Arbitrary(genHolidayCalendar.map(calendar => ReferenceData.Entry(calendar.id, calendar)))

  /**
   * Perturbs a seed by a reference data entry, through its rendering.
   *
   * The rendering names the identifier and the calendar, which are exactly the two fields the
   * equality of an entry compares, so it agrees with that equality - the one thing a `Cogen` has
   * to do.
   *
   * It is implicit, unlike the generator above, because nothing else in scope offers a `Cogen`
   * for this type and the law suites take theirs implicitly.
   */
  private implicit val cogenEntry: Cogen[ReferenceData.Entry[HolidayCalendar]] =
    Cogen[String].contramap[ReferenceData.Entry[HolidayCalendar]](entry => entry.toString)

  /**
   * The tolerant equality the additive laws of [[MultiCurrencyAmount]] are checked with.
   *
   * '''This is not an implicit and must never become one.''' It is passed by hand to the one rule
   * set that needs it, so that the `EqTests` and `HashTests` of the same type - which are about
   * the production instance and nothing else - cannot pick it up. Two values are equal here when
   * they hold the same currencies and their amounts agree to within a relative `1e-9`, with the
   * tolerance falling back to an absolute `1e-9` near zero.
   *
   * The reason is recorded in `SCALA_MIGRATION.md` and in the scaladoc of the instance itself:
   * IEEE-754 addition is only approximately associative, so `(a + b) + c` and `a + (b + c)` can
   * differ in their last bit, and under the exact equality of the type the associativity law is
   * falsified within a handful of examples on wholly ordinary finite inputs. The tolerance
   * states the strongest thing that is true of double arithmetic. The second restriction of the
   * same note - that the generators hold finite amounts only - is applied where that rule set is
   * registered.
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
   * Bit equality is taken first, so that two identical amounts agree however large they are, and
   * the tolerance is relative to the larger of the two with a floor of one, which makes it an
   * absolute `1e-9` for amounts near zero.
   *
   * @param left  one amount
   * @param right  the other amount
   * @return true if the two amounts agree to within the tolerance
   */
  private def closeEnough(left: Double, right: Double): Boolean =
    java.lang.Double.compare(left, right) == 0 ||
      math.abs(left - right) <= MonoidTolerance * math.max(
        math.max(math.abs(left), math.abs(right)),
        1.0d)

  /**
   * The text the `Show` of a [[Failure]] has to begin with, taken from the Java source.
   *
   * The Java type being ported rendered a failure as its reason, a colon, a space and its
   * message, and then a summary of the stack trace it held where it held one
   * [modules/collect/src/main/java/com/opengamma/strata/collect/result/FailureItem.java:388-389].
   * This port carries no stack trace, so that third part has no source and cannot appear, which
   * leaves the reason and the message as the whole of the Java rendering - and this is it,
   * written from that source and from nothing else.
   *
   * @param failure  the failure whose Java rendering is wanted
   * @return the reason, a colon and the message
   */
  private def javaFailureForm(failure: Failure): String =
    s"${failure.reason.name}: ${failure.message}"

  /**
   * Asserts the rendering of a [[Failure]], which is the one exception to the `toString` contract.
   *
   * Every other type of the inventory renders through `Show` exactly what its own `toString`
   * renders, so the property registered below compares the two. This type deliberately does not:
   * the `toString` of each Scala member stays the generated product rendering so that a failure
   * inspected in a debugger still shows its class and its fields, and its `Show` renders the Java
   * form instead [strata-collect `result/Failure.scala`, the scaladoc of its `show`].
   *
   * '''The rendering of a failure carrying attributes goes beyond the Java form.''' The Java
   * `toString` above never wrote attributes - in the library being ported the values they hold
   * were already substituted into the message by the formatter that produced both - while this
   * port appends them so that a log line holding failures of several kinds stays readable. That
   * is a divergence of the `strata-collect` instance rather than of this suite, it is recorded in
   * the resolution report of this checkpoint for the owner of that file, and it is why the
   * assertions below are split in two: the Java part is asserted '''exactly''', and the appended
   * part is asserted by its structure.
   *
   * The structure is checked by reading the rendering rather than by rebuilding it, which is the
   * point of writing it this way: an oracle that recomputed the same expression as the instance
   * would agree with the instance whatever either of them did. What is asserted instead is that
   * the rendering begins with the Java form; that a failure holding no attributes renders as that
   * form and nothing more; that every attribute appears once as `key=value`; that the keys appear
   * in ascending order, which is what makes the rendering a function of the value rather than of
   * the order attributes were added in; and that the appended part is exactly as long as those
   * pairs, their separators and the brackets - so a rendering carrying anything further fails.
   *
   * @param failure  the failure that was rendered
   * @param rendered  what the instance under test produced for it
   * @return the assertion that the rendering is the one the type promises
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
   * The rendering [[CalculationTargetList]] promises, the second exception to `toString`.
   *
   * The instance of this type reproduces the rendering of the '''Java''' bean it replaces -
   * `CalculationTargetList{targets=[a, b]}`, the form Joda-Beans generated from the bean's one
   * property - while the `toString` of the Scala case class is the generated product rendering
   * `CalculationTargetList(List(a, b))`. Reproducing the Java text is what the instance inventory
   * of the port requires of a `Show`, and the instance says so in its own scaladoc
   * [strata-basics `CalculationTarget.scala`, the `show` of `object CalculationTargetList`].
   *
   * The shape is stated here from the Java bean rendering rather than taken from the instance
   * under test, for the same reason [[assertFailureRendering]] reads its rendering rather than
   * rebuilding it: an oracle agreeing with the instance by construction asserts nothing. Unlike
   * that one this shape is the whole of the Java rendering
   * [modules/basics/src/main/java/com/opengamma/strata/basics/CalculationTargetList.java:128-133],
   * so it is stated as the text it must equal.
   *
   * @param list  the list whose promised rendering is wanted
   * @return the bean rendering of the list, its targets in order
   */
  private def renderedTargetList(list: CalculationTargetList): String =
    s"CalculationTargetList{targets=[${list.targets.mkString(", ")}]}"

  //-------------------------------------------------------------------------
  // the registration of the law suites
  //-------------------------------------------------------------------------
  /**
   * Registers the law suites of an unordered type, over its implicit generator.
   *
   * @param typeName  the name the rule sets and tests are labelled with
   * @param arb  the generator of values, taken implicitly
   * @param cogen  the perturbation of values, needed to generate the functions the laws use
   * @param hash  the hashing and equality instance under test
   * @param show  the rendering instance under test
   * @tparam A  the type whose laws are checked
   */
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
   * is the wrong one for this suite - [[DoubleArray]], [[DoubleMatrix]], [[FxRate]] and
   * [[FxMatrix]], whose implicit arbitraries hold the well-behaved values - can be checked over
   * the right one without a second implicit of the same type being brought into scope, where it
   * would be ambiguous with the first rather than replacing it.
   *
   * The rendering check is a parameter for the same reason and defaults to comparing against
   * `toString`, which is the contract of every type of the inventory but two. [[Failure]] and
   * [[CalculationTargetList]] are those two: each reproduces the rendering of the '''Java''' type
   * it replaces, which is what the instance inventory of the port asks of a `Show`, while its own
   * `toString` stays the generated product rendering. Both are registered below with the check
   * their rendering actually has to pass, stated there with the sources that decide it - a text
   * to equal where the Java rendering is the whole of it, and a set of properties read off the
   * rendering where this port appends to it.
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
        // `cats` states no laws for `Show`, so these are the two properties the port needs of
        // one: that it renders what the type renders, and that two values its `Eq` calls equal
        // render alike - without which a message about a value would depend on which of two
        // equal values reached it. The first is the contract the port actually promises: the
        // rendering of a value is the rendering the type being ported wrote, which for all but
        // two types of this inventory is what `toString` produces, so the check is `rendersAs`
        // and defaults to that comparison. An instance rendering a constant, or the name of its
        // class, or one field of two, fails here rather than passing a non-emptiness check.
        // Non-emptiness is deliberately not what is asserted: the empty matrix renders as the
        // empty string, as the Java original did, which the test at the foot of this file pins.
        rendersAs(left, show.show(left))
        (!hash.eqv(left, right) || show.show(left) == show.show(right)) shouldBe true
      }
    }
  }

  /**
   * Registers the law suites of an ordered type, over its implicit generator.
   *
   * The ordering, hashing and equality arrive as one instance of the intersection type, which is
   * the shape the port declares and the shape [[InstanceInventory]] asserts. The
   * `compare == 0` test registered here is the divergence from the Java comparison stated as a
   * property: the ordering of this port agrees with equality for every pair of values, which the
   * comparison of the library being ported did not for four of these types.
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
  // The first of the two types whose `Show` is deliberately not its `toString`: it reproduces the
  // rendering of the Java bean, which is what the instance inventory asks of a `Show`, while the
  // `toString` of the case class stays the generated product rendering. `renderedTargetList`
  // above states the promised shape and names where the instance documents it.
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
  // The two FX types are checked over the edge-value generators rather than over their implicit
  // arbitraries, which hold strictly positive finite rates: the equality of both is the
  // bit-pattern equality this file exists to check, and both domains admit values an ordinary
  // rate generator never reaches. `FxRate.of` passes a rate that is not a number - its check is
  // `!(rate <= 0.0)` - and accepts a positive infinity, and `FxMatrix.fromMatrix` checks only
  // distinct currencies, a square shape and a unit diagonal, so a matrix may hold `NaN`, either
  // infinity, a signed zero and an off-diagonal pair that are not reciprocals of one another.
  // Those are the values that decide whether these two instances are lawful, so they are the
  // values the rule sets see.
  unorderedLawsOf[FxRate]("FxRate", Arbitrary(genEdgeFxRate))
  unorderedLawsOf[FxMatrix]("FxMatrix", Arbitrary(genEdgeFxMatrix))

  // Two of the three members that declare instances of their own are registered here, each after
  // the family whose instances it restates at its own type: the calendar-bearing day count, whose
  // family is ordered and so appears among the ordered rule sets above, and the calendar built
  // from holiday dates. The third, `HalfUp`, is registered with the value package below.
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

  // The two numeric wrappers are checked over the edge-value generators rather than over their
  // implicit arbitraries, which hold finite values only: the equality of both types is the
  // bit-pattern equality this file exists to check, and a generator without `NaN`, the
  // infinities and the negative zero would never present it with the values that decide it.
  unorderedLawsOf[DoubleArray]("DoubleArray", Arbitrary(genDoubleArray))
  unorderedLawsOf[DoubleMatrix]("DoubleMatrix", Arbitrary(genDoubleMatrix))

  // The second type whose `Show` is not its `toString`, and the one whose rendering this port
  // extends beyond the Java form: `assertFailureRendering` above asserts the Java part exactly
  // and reads the appended attributes off the rendering rather than rebuilding them, so the check
  // cannot agree with the instance by construction. The concrete text of both shapes is pinned by
  // the test at the foot of this file.
  unorderedLawsOf[Failure]("Failure", arbFailure, assertFailureRendering)

  //-------------------------------------------------------------------------
  // the additive laws of the one type that has them
  //
  // Both arguments are passed by hand, and both are restrictions the migration note records:
  //
  //   - the generator holds finite amounts only. Combining an infinity with an infinity of the
  //     opposite sign produces a value that is not a number, which no amount may hold, so a
  //     generator emitting infinities falsifies associativity by reaching the documented
  //     invariant of `CurrencyAmount` - which says nothing about whether this instance is
  //     associative;
  //   - the equality is the tolerant one declared above rather than the production instance,
  //     because IEEE-754 addition is only approximately associative.
  //
  // Passing them rather than declaring them implicit is what keeps them out of the `Eq` and
  // `Hash` rule sets of the same type above, which use the production instance and the ordinary
  // generator, infinities included.
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
    // the three counts of the inventory, which are the counts the gate reports
    orderedNames should have size 28
    unorderedNames should have size 35
    allNames should have size 63
    // four instances per ordered type, three per unordered one, and the single additive instance
    InstanceInventory.summoned should have size 218
    InstanceInventory.additiveTypes should have size 1
    orderedNames.intersect(unorderedNames) shouldBe empty
    // every summon produced an initialised instance. A companion of this port refers to the
    // companions of the types it is built from, so a cycle in that initialisation order would
    // leave one of these values null rather than failing - which is exactly the defect this
    // catches, and the reason the instances are held rather than discarded.
    InstanceInventory.summoned.filter(entry => entry.instance == null) shouldBe empty
    // every name reported is the name the compiler itself gives the type, so a typo in one of
    // the literals above is a failure here rather than a wrong line in the gate report. A nested
    // type is named with a dollar by the runtime and with a dot by a reader, and an entry naming
    // its type argument is compared without it, since the runtime carries neither
    InstanceInventory.summoned.filterNot { entry =>
      entry.runtimeName.replace('$', '.') == entry.typeName.takeWhile(_ != '[')
    } shouldBe empty
    // the report is what the gate copies, so it must not vary between runs
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
   * The rendering of a named type is its name, which is the contract the whole port rests on: a
   * name is the identity of a member, the key of its JSON form and the text a caller parses back,
   * so an instance rendering anything else would make the two ways of writing a member disagree.
   * A family is closed and its members are enumerable, so this is asserted over '''every''' one of
   * them rather than over the values a generator happens to draw, which costs nothing and leaves
   * no member unchecked.
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
    // The property registered for each type above compares its rendering against its `toString`,
    // which is the contract of a plain product. For the fifteen closed families the contract is
    // stronger and is stated here: the rendering is the canonical `name`, the form the library
    // being ported wrote and parsed, and it holds for every member of every family rather than
    // for the members a generator reached. The families are exactly the ones that publish a
    // `NamedEnum`, which is what makes their members enumerable; the count is asserted so that a
    // family added to the port and not added here is a failure rather than a silent omission.
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
    // and a member the calendar-bearing day count adds outside `DayCount.values`, which the
    // family's own list does not hold: it is named for its calendar, and its rendering is that
    // name, which is what the law suite registered for the member checks over generated values
    val bus252: DayCount = DayCount.ofBus252(StandardHolidayCalendars.GBLO)
    implicitly[Show[DayCount]].show(bus252) shouldBe bus252.name
    bus252.name shouldBe "Bus/252 GBLO"
  }

  test("every type of the inventory has its law suites registered, and only the ones it claims") {
    // What the summons above cannot say. The inventory records which instances exist, and the law
    // suites are registered separately by the calls further up this file, so a registration that
    // was deleted or never written leaves the inventory intact and still claiming a type whose
    // laws nothing checks. This ties the two together by comparing the inventory's names with
    // the names ScalaTest actually holds.
    //
    // Discipline names every law after the suite it came from - `Order[Tenor].order.totality` -
    // so the presence of a suite for a type is the presence of a test whose name begins with the
    // family and the type. The inventory names a type by its qualified name and the law suites by
    // its declared name, which is the qualified name with its package dropped; the leading
    // segments of a package are lower case and the segments of a type are not, which is what
    // `declaredName` reads it by, so a nested type such as `ReferenceData.Entry` keeps both parts.
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
    // and the other direction, which is the point of the split: a type the port leaves unordered
    // has no ordering laws registered, so an ordering added without being inventoried fails here
    withClue("no unordered type has ordering laws registered: ")(
      unorderedNames.filter(typeName => hasSuite("Order", typeName)) shouldBe empty)
    withClue("the additive type has its monoid laws registered: ")(
      additiveNames.filterNot(typeName => hasSuite("Monoid", typeName)) shouldBe empty)
  }

  //-------------------------------------------------------------------------
  // what the port deliberately does not offer
  //-------------------------------------------------------------------------
  test("no ordering is offered for a type the port leaves unordered") {
    // `Payment` is a plain product of an amount and a date. Neither of two payments in different
    // currencies is the greater, so the port offers hashing and a rendering and no ordering; an
    // ordering invented for it would be an ordering nothing means. The negative is asserted twice
    // - `assertDoesNotCompile` is satisfied by a snippet that merely fails to parse, while
    // `assertTypeError` requires it to parse and then fail to typecheck, which is what a missing
    // implicit is - and paired with the instance that does exist, so a snippet failing for some
    // unrelated reason cannot pass for a proof.
    assertDoesNotCompile("""implicitly[Order[Payment]]""")
    assertTypeError("""implicitly[Order[Payment]]""")
    assertCompiles("""implicitly[Hash[Payment]]""")
    assertCompiles("""implicitly[Show[Payment]]""")
  }

  test("no additive instance is offered for a single-currency amount") {
    // Adding two amounts of different currencies has no answer, and a `Semigroup` has nowhere to
    // report that it has none - which is why `CurrencyAmount.plus` returns an `Either` and why
    // this type carries no additive instance. `MultiCurrencyAmount` carries one because combining
    // two of those cannot fail: an amount of a currency held by one side is carried across.
    assertDoesNotCompile("""implicitly[Monoid[CurrencyAmount]]""")
    assertTypeError("""implicitly[Monoid[CurrencyAmount]]""")
    assertDoesNotCompile("""implicitly[cats.Semigroup[CurrencyAmount]]""")
    assertTypeError("""implicitly[cats.Semigroup[CurrencyAmount]]""")
    assertCompiles("""implicitly[Monoid[MultiCurrencyAmount]]""")
  }

  test("no domain type is a Foldable or a Traverse") {
    // Those two typeclasses describe a type constructor - something with a hole, such as a list -
    // and no domain type of this port is one, so the summons below fail on the kind of the type
    // rather than on a missing instance. That is the statement being made: the port uses
    // `Foldable` and `Traverse` through cats syntax on the standard collections and on
    // `NonEmptyList`, and never by making one of its own types traversable.
    assertTypeError("""implicitly[cats.Foldable[MultiCurrencyAmount]]""")
    assertTypeError("""implicitly[cats.Traverse[Schedule]]""")
    assertCompiles("""implicitly[cats.Foldable[List]]""")
  }

  /**
   * Returns whether two instances are one and the same object.
   *
   * `cats.kernel.Eq` is a universal trait - it extends `Any`, so that a value class can carry one
   * - which is why an instance is not statically a reference and why ScalaTest's
   * `theSameInstanceAs`, which takes an `AnyRef`, cannot be handed one. Every instance of this
   * port is an object, so the conversion below is a no-op at runtime and the comparison is the
   * reference comparison it appears to be.
   *
   * @param left  one instance
   * @param right  the other instance
   * @return true if the two are the same object
   */
  private def sameInstance(left: Any, right: Any): Boolean =
    left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]

  test("the rendering of an empty matrix is the empty string, as it was in the original") {
    // The one value in the whole inventory whose rendering is empty, and it is empty by parity:
    // the implementation being ported appended a line per row to an empty buffer, so a matrix of
    // no rows rendered as nothing at all. It is pinned here because the property registered for
    // every type asserts only that a rendering exists, and this is the reason it asserts no more.
    DoubleMatrix.of().toString shouldBe ""
    implicitly[Show[DoubleMatrix]].show(DoubleMatrix.of()) shouldBe ""
    // every other empty value of the inventory renders as something a reader can see
    implicitly[Show[DoubleArray]].show(DoubleArray.of()) shouldBe "[]"
    implicitly[Show[MultiCurrencyAmount]].show(MultiCurrencyAmount.empty) shouldBe "[]"
  }

  /**
   * Pins the two rendering shapes of a [[Failure]] as the text they produce.
   *
   * The property registered for this type reads the rendering it is given and asserts the Java
   * part exactly and the appended part by its structure, deliberately without rebuilding it. This
   * test states the other half of that argument: the '''literal''' text of both shapes, written
   * here by hand, so that the format itself is pinned by something no change to the instance can
   * move with it.
   *
   * The first shape is the rendering of the Java type being ported, to the character: the reason,
   * a colon, a space and the message
   * [modules/collect/src/main/java/com/opengamma/strata/collect/result/FailureItem.java:388-389].
   * That method also appended a summary of the stack trace where the failure held one, which this
   * port has no field for, so no part of the Java rendering is missing from the first assertion
   * below.
   *
   * The second shape is this port's extension and a '''divergence''' from that Java rendering,
   * which never wrote attributes - the formatter of the library being ported substituted their
   * values into the message itself and returned both, so the attributes were a structured copy of
   * text the message already carried. This port keeps the message as it was given and appends the
   * attributes in key order, so that a log line holding failures of several kinds stays readable;
   * the instance documents that choice, and the resolution report of this checkpoint records it
   * for the owner of `strata-collect`, whose file declares it. The ascending key order is asserted
   * with a failure whose attributes are built in the opposite order, which is what makes the
   * rendering a function of the value rather than of the order the attributes arrived in.
   *
   * The text form of a failure is that same rendering - `toString` delegates to it - so a failure
   * written out by interpolation or by a library reads as it does through the instance, and the
   * bounding and escaping the rendering performs cannot be bypassed.
   */
  test("a failure renders as the Java form, and as that form and its attributes in key order") {
    val rendering = implicitly[Show[Failure]]
    // the Java form, to the character, for a failure holding no attributes
    rendering.show(Failure.Invalid("Schedule is invalid")) shouldBe "INVALID: Schedule is invalid"
    rendering.show(Failure.MissingData("No holiday calendar")) shouldBe
      "MISSING_DATA: No holiday calendar"
    // and the extension, for a failure holding one attribute and for one holding two, whose keys
    // are given in descending order and rendered in ascending order
    rendering.show(
      Failure.Invalid("Schedule is invalid", SortedMap("definition" -> "P3M"))) shouldBe
      "INVALID: Schedule is invalid [definition=P3M]"
    rendering.show(
      Failure.Parsing("Unknown currency", SortedMap("value" -> "XYZ", "scale" -> "2"))) shouldBe
      "PARSING: Unknown currency [scale=2, value=XYZ]"
    // The `toString` of a member is that rendering and deliberately so: a failure is written out
    // in one form whichever path writes it - the instance, interpolation, or a library calling
    // `toString` - so text that arrived from outside cannot reach a log in a form that has not
    // been bounded and neutralised. It is why this type is one of the two the property above
    // checks against a rendering of its own: `toString` is the rendering rather than the product
    // form, so comparing the two would say nothing.
    Failure.Invalid("Schedule is invalid").toString shouldBe
      rendering.show(Failure.Invalid("Schedule is invalid"))
    Failure.Parsing("Unknown currency", SortedMap("value" -> "XYZ", "scale" -> "2")).toString shouldBe
      "PARSING: Unknown currency [scale=2, value=XYZ]"
  }

  test("one implicit answers for equality, ordering and hashing alike") {
    // `Hash` and `Order` both extend `Eq`, so a type declaring either of them has declared an
    // `Eq` as well. The port declares exactly one such instance per type and never a second, and
    // the proof is that these summons resolve at all: a companion offering an `Eq` of its own
    // beside its `Hash` would make every one of them ambiguous. At runtime the instance
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
  // Every type of the port that carries a `Double` or an array of them compares its fields with
  // `java.lang.Double.compare` and `java.util.Arrays.equals`, and hashes them with the matching
  // `hashCode` - the semantics of the Joda-Beans the port replaces, which compared the bits
  // rather than the numbers. Two consequences follow, and the law suites above cannot state
  // either of them because a generator would have to stumble on the values: a value holding a
  // `NaN` equals itself, where `==` on the primitive would not, and a negative zero differs from
  // a positive one, where `==` on the primitive would not either.
  //-------------------------------------------------------------------------
  /**
   * Asserts that two separately built values are equal, by their own equality and by the instance.
   *
   * The two values are built by two calls to the same factory rather than shared, so that
   * reference equality cannot satisfy the assertion, and the hashes are compared as well, since
   * an equality that its hashing disagrees with is what breaks a hash map.
   *
   * @param left  one value
   * @param right  the value built the same way
   * @param hash  the instance under test
   * @tparam A  the type being asserted
   * @return the assertion that the two agree
   */
  private def assertEqualByBits[A](left: A, right: A)(implicit hash: Hash[A]): Assertion = {
    (left == right) shouldBe true
    hash.eqv(left, right) shouldBe true
    left.hashCode shouldBe right.hashCode
    hash.hash(left) shouldBe hash.hash(right)
  }

  /**
   * Asserts that two values differing only in the sign of a zero are not equal.
   *
   * @param negative  the value holding the negative zero
   * @param positive  the value holding the positive zero
   * @param hash  the instance under test
   * @tparam A  the type being asserted
   * @return the assertion that the two differ
   */
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
    assertEqualByBits(
      CurrencyAmountArray.of(gbp, DoubleArray.of(Double.NaN)),
      CurrencyAmountArray.of(gbp, DoubleArray.of(Double.NaN)))
    assertEqualByBits(
      valueOf(MultiCurrencyAmountArray.of(Map(gbp -> DoubleArray.of(Double.NaN)))),
      valueOf(MultiCurrencyAmountArray.of(Map(gbp -> DoubleArray.of(Double.NaN)))))
  }

  test("a value holding an infinity equals itself and hashes with itself") {
    // `CurrencyAmount` rejects a value that is not a number and accepts the infinities, exactly
    // as the implementation being ported did, so an infinite amount is a value these types hold
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
    // This is the one documented exception to the rule above, and it is a property of the
    // factory rather than of the equality: `CurrencyAmount.of` maps a negative zero to a
    // positive one, as the implementation being ported did, so no amount holds a negative zero
    // and the bit comparison its equality performs can never be presented with one. The amounts
    // are compared by their bits here rather than by `==`, which would hold whatever the sign
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
  // `Order` requires that `compare` return zero exactly where the values are equal, and four of
  // the comparisons being ported do not: each reads some of the fields its equality reads and
  // ranks two unequal values equal. The port keeps the original comparison as the leading keys
  // and continues over the remaining fields, so every pair the original ranked strictly keeps
  // its order and only the pairs it called equal are decided. The property holds for every
  // ordered type of the inventory - it is registered beside each `OrderTests` above - and the
  // four tests below pin the specific pairs, each of which would rank equal under the Java
  // comparison.
  //-------------------------------------------------------------------------
  test("a fixed-scale decimal is ordered by its decimal and then by its scale") {
    val decimal = valueOf(Decimal.of("12.3"))
    val atOnePlace = valueOf(FixedScaleDecimal.of(decimal, 1))
    val atFourPlaces = valueOf(FixedScaleDecimal.of(decimal, 4))
    val ordering = implicitly[Order[FixedScaleDecimal]]
    // the two hold the same amount of money shown to a different number of places, which is the
    // whole point of the type, and the comparison being ported read the amount alone
    atOnePlace.decimal.compareTo(atFourPlaces.decimal) shouldBe 0
    atOnePlace.toString shouldBe "12.3"
    atFourPlaces.toString shouldBe "12.3000"
    // so it ranked two unequal values equal, which this ordering does not
    ordering.eqv(atOnePlace, atFourPlaces) shouldBe false
    ordering.compare(atOnePlace, atFourPlaces) should be < 0
    ordering.compare(atFourPlaces, atOnePlace) should be > 0
    ordering.compare(atOnePlace, valueOf(FixedScaleDecimal.of(decimal, 1))) shouldBe 0
  }

  test("a tenor is ordered by its length and then by its name") {
    val twelveMonths = valueOf(Tenor.ofMonths(12))
    val oneYear = valueOf(Tenor.ofYears(1))
    val ordering = implicitly[Order[Tenor]]
    // the comparison being ported ranks these two equal - twelve months and a year are the same
    // length - while holding them unequal, since months are not normalised into years
    twelveMonths.compareTo(oneYear) shouldBe 0
    ordering.eqv(twelveMonths, oneYear) shouldBe false
    twelveMonths.name shouldBe "12M"
    oneYear.name shouldBe "1Y"
    // the tie-break by name decides them, and puts `12M` immediately before `1Y`
    ordering.compare(twelveMonths, oneYear) should be < 0
    ordering.compare(oneYear, twelveMonths) should be > 0
    ordering.compare(twelveMonths, valueOf(Tenor.ofMonths(12))) shouldBe 0
    // and it cannot reorder a pair the original ranked strictly, being consulted only after it
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
    // the codes that are not spot-starting keep the order the original gave them
    ordering.compare(MarketTenor.ON, MarketTenor.TN) should be < 0
    ordering.compare(MarketTenor.TN, MarketTenor.SN) should be < 0
  }

  test("a schedule period is ordered by its unadjusted dates and then by its adjusted dates") {
    val adjusted = valueOf(SchedulePeriod.of(jan15, apr15, jan14, apr14))
    val unadjusted = valueOf(SchedulePeriod.of(jan14, apr14, jan14, apr14))
    val ordering = implicitly[Order[SchedulePeriod]]
    // the comparison being ported read the two unadjusted dates only, which these two share:
    // equal periodic dates falling on different business days
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
    // value, which is the comparison the implementation being ported performed
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
  // three tests below say what that instance actually computes - which the laws do not, being
  // statements about identity and associativity alone - and why the tolerance is needed.
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
    // however the values were combined - which is what makes the JSON of a value byte-stable
    combined.toMap.toList.map { case (currency, amount) => (currency.code, amount) } shouldBe
      List(("EUR", 10.0d), ("GBP", 150.0d), ("USD", 200.0d))
    combined.toMap shouldBe a[SortedMap[_, _]]
    // aggregating a collection is one pass and agrees with combining the values one at a time
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
    // instance of the type, because floating-point addition is only approximately associative.
    // This is that difference, made visible: two values a rounding apart are one value to the
    // tolerance and two values to the type. The tolerance is not blanket - it discriminates
    // amounts that genuinely differ, and values holding different currencies - so the
    // associativity it checks is still a statement about this instance.
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
