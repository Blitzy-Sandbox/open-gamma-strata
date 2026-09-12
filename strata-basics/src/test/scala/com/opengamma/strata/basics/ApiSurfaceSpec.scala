/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.time.DayOfWeek
import java.time.LocalDate
import java.time.Period
import java.time.YearMonth

import scala.util.Using

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.currency.AdjustablePayment
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
import com.opengamma.strata.basics.value.ValueDerivatives
import com.opengamma.strata.basics.value.ValueSchedule
import com.opengamma.strata.basics.value.ValueStep
import com.opengamma.strata.basics.value.ValueStepSequence
import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.FixedScaleDecimal
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.ValidatedFailures
import com.opengamma.strata.collect.ValueWithFailures
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
 * The file is one '''inventory''' of the module's public surface. Every entry is a
 * [[ApiSurfaceSpec.SurfaceRow]] declared once, carrying the kind of surface it is, the subject it
 * is about, the claim it makes and the audit that proves the claim; the tests of this suite are
 * then registered from that inventory, one per row, and the coverage assertions at the end
 * compare the inventory against the kind lists the port's construction policy fixes. A row and
 * its test are therefore the same thing: deleting a subject's audit deletes its row, which the
 * coverage assertions report, rather than leaving a name in a list that nothing exercises.
 *
 * The kinds, which are the sections of the inventory:
 *
 *  1. '''Validated (`[V]`) and normalising (`[N]`) types.''' Each is represented as
 *     `sealed abstract case class X private (...)`, a form that generates neither `apply` nor
 *     `copy` while still supporting `unapply`. The alternative that suggests itself,
 *     `final case class X private (...)`, hides '''neither''' of them, so the distinction is
 *     invisible in the source and shows up only when someone writes `X(...)` and it compiles.
 *     Each type is held to all three facts: no `apply`, no `copy`, and a working `unapply`. The
 *     last matters as much as the first two - a change that locked the types down harder, by
 *     dropping `case` altogether, would cost pattern matching everywhere.
 *  1. '''Total (`[T]`) types.''' The construction policy is not "lock everything down": a total
 *     type accepts every well-typed input, has nothing to check and nothing to rewrite, and
 *     therefore keeps the ordinary case class surface - `apply`, `copy` and `unapply`. Every one
 *     of them is audited positively, so that a later tidy-up which "made every type consistent"
 *     by locking them down would fail here and be discussed rather than absorbed. The two
 *     numeric wrappers are the policy's own exception - a final class over a private primitive
 *     array, with copy-safe total factories - and are audited as that.
 *  1. '''Closed families.''' The registry the port replaced could be extended at runtime from a
 *     configuration file; the sealed families that replaced it cannot be extended at all, which
 *     is what makes exhaustive matching sound. Scala 2 enforces sealing per '''file''', so a
 *     subtype declared '''here''' does not compile - and that is also the reason
 *     `index/Index.scala` and `date/HolidayCalendar.scala` are single large files rather than one
 *     file per type. How that is proved without proving something else by accident is described
 *     below, because it is the one place where the obvious assertion is the wrong one.
 *  1. '''Open contracts.''' Several traits are deliberately '''not''' sealed: `ReferenceData`,
 *     which an application implements to supply its own holidays, `ReferenceDataId`, which it
 *     implements to name data of its own, `FloatingRate`, which `FloatingRateName` implements
 *     from another file, and the forward-path contracts kept for the next slice of the migration.
 *     Being implementable is the only property this slice can give them, and it is asserted by
 *     implementing them here.
 *  1. '''Function, alias and witness surfaces.''' What is left of the public surface once the
 *     data types are accounted for: the single-abstract-method callbacks the numeric wrappers
 *     take, the lookup function type of `FloatingRate`, the type aliases both module roots
 *     re-export, and the value-type witness reference data narrows a lookup with. None of them
 *     carries data, which is why none of them appears in the port's codec inventory, and each is
 *     recorded here so that the two inventories together account for every public surface of both
 *     modules rather than for the data types alone.
 *  1. '''Module-internal reference data.''' The transcribed index tables are `private[basics]`,
 *     so they are outside the published surface altogether; each is recorded with the published
 *     family that carries its columns, which is what a caller reads instead.
 *  1. '''Policy rows.''' The private constructors themselves, the unassignability of published
 *     fields, and the copy safety of the numeric wrappers: `DoubleArray` and `DoubleMatrix` wrap
 *     a primitive array that callers must not be able to alias, so the two escape hatches that
 *     would hand it over - `ofUnsafe` and `toArrayUnsafe` - are `private[collect]`. Their own
 *     module's specs cannot prove that, being inside `collect`; this module can, and does,
 *     together with runtime proof that the public factories and `toArray` really copy.
 *
 * ===How sealing is proved without proving something else instead===
 *
 * Most of these families also hide their constructor, so the obvious probe -
 * `final class Host extends Currency("XYZ", 2, "USD")` - is rejected for '''two''' reasons at
 * once, and would still be rejected if the family stopped being sealed. An assertion like that
 * cannot report what it claims to.
 *
 * So sealing is proved by a '''trait''' probe: `trait Host extends Currency` calls no
 * constructor, needs no arguments and implements no member, which leaves the `sealed` modifier as
 * the only thing the compiler can object to. Constructor privacy is then asserted separately, by
 * the class probe, and labelled as the different property it is. The control that keeps the
 * technique honest is at the end of the file: the same trait probe '''compiles''' over every
 * contract this module leaves open, so a trait probe is not something the compiler rejects out of
 * hand.
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
 * One consequence of the build's `-Wunused` setting, under warnings as errors, is worth recording, because
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
  private val valueSchedule: ValueSchedule = valueOf(ValueSchedule.of(1000.0, List(valueStep)))
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

  /**
   * Names the kind of an observation by matching the sealed family with no default branch.
   *
   * This is the compile-time half of the closedness proof of `IndexObservation`, and it is
   * deliberately written as a method of this suite rather than inside a compile-assertion string:
   * the match is exhaustive only because the family admits exactly the four types below, so a
   * fifth direct subtype - anywhere, since the family is sealed and its file is the only place
   * one could be declared - would make it inexhaustive, which this build's fatal-warning setting
   * reports as a compilation error of this file. The absence of a `case _` is the whole point and must not be
   * "fixed" by adding one.
   *
   * @param observation  the observation to name the kind of
   * @return the name of its kind
   */
  private def kindOfObservation(observation: IndexObservation): String = observation match {
    case _: IborIndexObservation => "Ibor"
    case _: OvernightIndexObservation => "Overnight"
    case _: PriceIndexObservation => "Price"
    case _: FxIndexObservation => "Fx"
  }

  private val payment: Payment = Payment(currencyAmount, jan15)
  private val adjustableDate: AdjustableDate = AdjustableDate(jan15, adjustment)
  private val adjustablePayment: AdjustablePayment = AdjustablePayment(currencyAmount, adjustableDate)
  private val valueAdjustment: ValueAdjustment = ValueAdjustment.ofReplace(1000.0)
  private val valueDerivatives: ValueDerivatives = ValueDerivatives(1.5, DoubleArray.of(0.5, 0.25))
  private val calculationTarget: HostTarget = HostTarget(currencyAmount)
  private val calculationTargetList: CalculationTargetList = CalculationTargetList.of(calculationTarget)
  private val referenceDataEntry: ReferenceData.Entry[HolidayCalendar] =
    ReferenceData.Entry(gbloId, gblo)

  /**
   * The `Bus/252` day count, typed as the member of the family rather than as the family.
   *
   * `ofBus252` answers with a `DayCount`, which is what a caller wants, but an audit of the
   * member's own construction surface has to be written against the member: a `copy` added to
   * `DayCount.Bus252` would be invisible through a witness typed as `DayCount`, so the negative
   * assertion in that row would pass while the policy it stands for had been broken. The pattern
   * below is what types it, and it fails the fixture rather than the assertion if `ofBus252` ever
   * answers with something else.
   */
  private val bus252Member: DayCount.Bus252 = bus252 match {
    case member: DayCount.Bus252 => member
    case other => fail(s"DayCount.ofBus252 answered with $other, which is not a DayCount.Bus252")
  }

  /**
   * Registers one test per inventory row, which is what makes a row and its audit the same thing.
   *
   * The name of each test is the row's own rendering, so a failure names the kind, the subject and
   * the claim that failed, and the coverage assertions at the end of the file compare the
   * registered names against the inventory they came from.
   */
  private def register(rows: List[SurfaceRow]): Unit =
    rows.foreach(entry => test(entry.testName)(entry.audit()))

  /**
   * Builds one inventory row.
   *
   * The audit is taken by name and stored as a function, so it runs when its test runs rather than
   * while the inventory is being built.
   *
   * @param kind  the kind of surface the row is about
   * @param subject  the type, member or alias the row is about
   * @param claim  what the row asserts about it, which completes the sentence its test is named
   * @param audit  the assertions that prove the claim
   * @return the row
   */
  private def row(kind: Kind, subject: String, claim: String)(audit: => Any): SurfaceRow =
    SurfaceRow(kind, subject, claim, () => audit)

  /**
   * The subjects the inventory covers for one kind of surface.
   *
   * @param kind  the kind of surface
   * @return the subjects of that kind, as a set so that the coverage assertions compare content
   *   rather than order
   */
  private def subjectsOf(kind: Kind): Set[String] =
    inventory.filter(entry => entry.kind == kind).map(entry => entry.subject).toSet

  /**
   * The root of the checkout, found by walking up from wherever the tests were started.
   *
   * The derivation below reads the module sources, so it needs the one directory they are both
   * under. It is identified by what it contains rather than by a path handed in, so the
   * derivation works from any working directory a runner might choose; failing to find it fails
   * the test, because a derivation that quietly skipped would assert nothing.
   */
  private lazy val repositoryRoot: Path =
    Iterator
      .unfold(Option(Paths.get("").toAbsolutePath.normalize)) {
        case Some(path) => Some((path, Option(path.getParent)))
        case None => None
      }
      .find(isRepositoryRoot)
      .getOrElse(
        fail(
          "the public surface is derived from the module sources, and the root of the checkout " +
            s"could not be found above ${Paths.get("").toAbsolutePath.normalize}"))

  /**
   * Checks whether a directory is the root of the checkout.
   *
   * @param path  the directory to test
   * @return true if it holds the build definition and both modules' Scala sources
   */
  private def isRepositoryRoot(path: Path): Boolean =
    Files.isRegularFile(path.resolve("build.sbt")) &&
      Files.isDirectory(path.resolve("strata-basics/src/main/scala")) &&
      Files.isDirectory(path.resolve("strata-collect/src/main/scala"))

  /**
   * Reads a module source, by its path from the root of the checkout.
   *
   * @param relative  the path of the source from the root of the checkout
   * @return its lines
   */
  private def sourceLines(relative: String): List[String] = {
    val file = repositoryRoot.resolve(relative)
    withClue(s"the public surface is derived from $relative, which must be readable: ")(
      Files.isRegularFile(file) shouldBe true)
    Using(scala.io.Source.fromFile(file.toFile, "UTF-8"))(source => source.getLines().toList)
      .fold(cause => fail(s"$relative could not be read: $cause"), lines => lines)
  }

  /**
   * The declarations of one keyword a source publishes.
   *
   * A declaration counts as published when its line begins with the keyword, possibly behind
   * the modifiers that may precede it, and does '''not''' begin with `private` or `protected`.
   * Anchoring at the start of the line is what keeps documentation out: a Scaladoc line begins
   * with `*` and a commented-out one with `/`, so neither can be mistaken for a declaration,
   * and the compile assertions of this file - which quote declarations inside strings - are in
   * a different file from the sources being read.
   *
   * @param lines  the lines of the source
   * @param keyword  the declaration keyword, such as `trait` or `type`
   * @return the names declared with that keyword and published
   */
  private def publicDeclarations(lines: List[String], keyword: String): Set[String] = {
    val declaration = s"""^\\s*(?:final\\s+|sealed\\s+|abstract\\s+|implicit\\s+|case\\s+)*$keyword\\s+([A-Za-z]\\w*)""".r
    lines.iterator
      .filterNot(line => line.trim.startsWith("private") || line.trim.startsWith("protected"))
      .flatMap(line => declaration.findFirstMatchIn(line).map(matched => matched.group(1)))
      .toSet
  }

  //-------------------------------------------------------------------------
  // The validated and normalising types of both modules.
  //
  // Each of these checks, rewrites, or both, what it is given, so a caller that could reach a
  // constructor or a `copy` could build a value the factory would have refused, or one that had
  // skipped the rewrite and would then compare unequal to the same value built properly. The
  // three assertions per type are the whole policy: the factory is the only way in, the value
  // cannot be modified into another one, and taking it apart still works.
  //-------------------------------------------------------------------------
  private val validatedRows: List[SurfaceRow] = List(
    row(Validated, "StandardId", "publishes no apply and no copy, and destructures through unapply") {
      val StandardId(scheme, value) = standardId
      scheme shouldBe "OG-Ticker"
      value shouldBe "AAPL"
      assertCompiles("""StandardId.of(standardId.scheme, standardId.value)""")
      assertTypeError("""StandardId(standardId.scheme, standardId.value)""")
      assertTypeError("""standardId.copy(value = standardId.value)""")
    },
    row(Validated, "Country", "publishes no apply and no copy, and destructures through unapply") {
      val Country(code) = country
      code shouldBe "GB"
      assertCompiles("""Country.of(country.code)""")
      assertTypeError("""Country(country.code)""")
      assertTypeError("""country.copy(code = country.code)""")
    },
    row(Validated, "FxRate", "publishes no apply and no copy, and destructures through unapply") {
      val FxRate(pair, rate) = gbpUsdRate
      pair shouldBe gbpUsd
      rate shouldBe 1.25
      assertCompiles("""FxRate.of(gbpUsdRate.pair, gbpUsdRate.rate)""")
      assertTypeError("""FxRate(gbpUsdRate.pair, gbpUsdRate.rate)""")
      assertTypeError("""gbpUsdRate.copy(rate = gbpUsdRate.rate)""")
    },
    row(Validated, "FxMatrix", "publishes no apply and no copy, and destructures through unapply") {
      val FxMatrix(currencies, rates) = fxMatrix
      currencies should contain theSameElementsAs Vector(gbp, usd)
      rates.get(0, 0) shouldBe 1.0
      assertCompiles("""FxMatrix.fromMatrix(fxMatrix.currencies, fxMatrix.rates)""")
      assertTypeError("""FxMatrix(fxMatrix.currencies, fxMatrix.rates)""")
      assertTypeError("""fxMatrix.copy(rates = fxMatrix.rates)""")
    },
    row(Validated, "CurrencyAmountArray", "publishes no apply and no copy, and destructures through unapply") {
      val CurrencyAmountArray(currency, values) = currencyAmountArray
      currency shouldBe gbp
      values.get(1) shouldBe 200.0
      assertCompiles("""CurrencyAmountArray.of(currencyAmountArray.currency, currencyAmountArray.values)""")
      assertTypeError("""CurrencyAmountArray(currencyAmountArray.currency, currencyAmountArray.values)""")
      assertTypeError("""currencyAmountArray.copy(currency = currencyAmountArray.currency)""")
    },
    row(Validated, "MultiCurrencyAmountArray", "publishes no apply and no copy, and destructures through unapply") {
      val MultiCurrencyAmountArray(size, values) = multiCurrencyAmountArray
      size shouldBe 1
      values.keySet shouldBe Set(gbp)
      assertCompiles("""MultiCurrencyAmountArray.of(multiCurrencyAmountArray.values)""")
      assertTypeError("""MultiCurrencyAmountArray(multiCurrencyAmountArray.size, multiCurrencyAmountArray.values)""")
      assertTypeError("""multiCurrencyAmountArray.copy(size = multiCurrencyAmountArray.size)""")
    },
    row(Validated, "FixedScaleDecimal", "publishes no apply and no copy, and destructures through unapply") {
      val FixedScaleDecimal(value, fixedScale) = fixedScaleDecimal
      value shouldBe decimal
      fixedScale shouldBe 4
      assertCompiles("""FixedScaleDecimal.of(fixedScaleDecimal.decimal, fixedScaleDecimal.fixedScale)""")
      assertTypeError("""FixedScaleDecimal(fixedScaleDecimal.decimal, fixedScaleDecimal.fixedScale)""")
      assertTypeError("""fixedScaleDecimal.copy(fixedScale = fixedScaleDecimal.fixedScale)""")
    },
    row(Validated, "MarketTenor", "publishes no apply and no copy, and destructures through unapply") {
      val MarketTenor(code) = marketTenor
      code shouldBe "3M"
      assertCompiles("""MarketTenor.parse(marketTenor.code)""")
      assertTypeError("""MarketTenor(marketTenor.code)""")
      assertTypeError("""marketTenor.copy(code = marketTenor.code)""")
    },
    row(Validated, "DaysAdjustment", "publishes no apply and no copy, and destructures through unapply") {
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
    },
    row(Validated, "PeriodAdjustment", "publishes no apply and no copy, and destructures through unapply") {
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
    },
    row(Validated, "TenorAdjustment", "publishes no apply and no copy, and destructures through unapply") {
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
    },
    row(Validated, "AdjustableDates", "publishes no apply and no copy, and destructures through unapply") {
      val AdjustableDates(unadjusted, adjust) = adjustableDates
      unadjusted.toList shouldBe List(jan15, apr15)
      adjust shouldBe adjustment
      assertCompiles("""AdjustableDates.of(adjustableDates.adjustment, adjustableDates.unadjusted)""")
      assertTypeError("""AdjustableDates(adjustableDates.unadjusted, adjustableDates.adjustment)""")
      assertTypeError("""adjustableDates.copy(adjustment = adjustableDates.adjustment)""")
    },
    row(Validated, "SchedulePeriod", "publishes no apply and no copy, and destructures through unapply") {
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
    },
    row(Validated, "Schedule", "publishes no apply and no copy, and destructures through unapply") {
      val Schedule(periods, periodicFrequency, rollConvention) = schedule
      periods.toList shouldBe List(schedulePeriod)
      periodicFrequency shouldBe Frequency.TERM
      rollConvention shouldBe RollConventions.NONE
      assertCompiles(
        """Schedule.of(schedule.periods, schedule.periodicFrequency, schedule.rollConvention)""")
      assertTypeError(
        """Schedule(schedule.periods, schedule.periodicFrequency, schedule.rollConvention)""")
      assertTypeError("""schedule.copy(rollConvention = schedule.rollConvention)""")
    },
    row(Validated, "PeriodicSchedule", "publishes no apply and no copy, and destructures through unapply") {
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
    },
    row(Validated, "ValueStep", "publishes no apply and no copy, and destructures through unapply") {
      val ValueStep(periodIndex, date, value) = valueStep
      periodIndex shouldBe None
      date shouldBe Some(apr15)
      value shouldBe ValueAdjustment.ofReplace(1000.0)
      assertCompiles("""ValueStep.of(valueStep.periodIndex, valueStep.date, valueStep.value)""")
      assertTypeError("""ValueStep(valueStep.periodIndex, valueStep.date, valueStep.value)""")
      assertTypeError("""valueStep.copy(date = valueStep.date)""")
    },
    row(Validated, "ValueSchedule", "publishes no apply and no copy, and destructures through unapply") {
      val ValueSchedule(initialValue, steps, stepSequence) = valueSchedule
      initialValue shouldBe 1000.0
      steps shouldBe List(valueStep)
      stepSequence shouldBe None
      assertCompiles(
        """ValueSchedule.of(valueSchedule.initialValue, valueSchedule.steps, valueSchedule.stepSequence)""")
      assertTypeError(
        """ValueSchedule(valueSchedule.initialValue, valueSchedule.steps, valueSchedule.stepSequence)""")
      assertTypeError("""valueSchedule.copy(initialValue = valueSchedule.initialValue)""")
    },
    row(Validated, "ValueStepSequence", "publishes no apply and no copy, and destructures through unapply") {
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
    },
    row(Validated, "HalfUp", "publishes no apply and no copy, and destructures through unapply") {
      val HalfUp(decimalPlaces, fraction) = halfUp
      decimalPlaces shouldBe 2
      fraction shouldBe 4
      assertCompiles("""HalfUp.ofFractionalDecimalPlaces(halfUp.decimalPlaces, halfUp.fraction)""")
      assertTypeError("""HalfUp(halfUp.decimalPlaces, halfUp.fraction)""")
      assertTypeError("""halfUp.copy(fraction = halfUp.fraction)""")
    },
    row(Validated, "IborIndexObservation", "publishes no apply and no copy, and destructures through unapply") {
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
    },
    row(Validated, "OvernightIndexObservation", "publishes no apply and no copy, and destructures through unapply") {
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
    },
    row(Validated, "FxIndexObservation", "publishes no apply and no copy, and destructures through unapply") {
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
    },
    row(
      Validated,
      "DayCount.Bus252",
      "publishes no apply and no copy, and destructures through unapply") {
      // The one member of a sealed family that is not a singleton - the calendar is part of the
      // convention and appears in its name - and a validated type under the same policy as every
      // other: a `sealed abstract case class` whose constructor is visible to `DayCount` alone.
      // `ofBus252` is the only way in, and `unapply` is what the `case` keyword is kept for.
      //
      // The witness is typed as the member rather than as the family, deliberately: a `copy`
      // added to `DayCount.Bus252` would be invisible through a `DayCount`, so the negative
      // assertion would pass while the policy it stands for had been broken.
      val DayCount.Bus252(calendar) = bus252Member
      calendar shouldBe gblo
      bus252Member.name shouldBe "Bus/252 GBLO"
      bus252Member.calendar shouldBe gblo
      assertCompiles("""DayCount.ofBus252(gblo)""")
      assertCompiles("""DayCount.ofBus252(gbloId, refData)""")
      assertCompiles("""bus252Member.calendar""")
      assertTypeError("""DayCount.Bus252(bus252Member.calendar)""")
      assertTypeError("""bus252Member.copy(calendar = bus252Member.calendar)""")
      assertTypeError("""new DayCount.Bus252(bus252Member.calendar) {}""")
    })

  //-------------------------------------------------------------------------
  // The normalising types, which rewrite what they are given as well as checking it: an amount of
  // `-0.0` becomes `0.0`, a period is normalised, a composite calendar identifier has its
  // components sorted and deduplicated, a money amount is rounded to the currency's minor units,
  // and a calendar's holidays are sorted and deduplicated with its year range derived from them.
  //-------------------------------------------------------------------------
  private val normalisingRows: List[SurfaceRow] = List(
    row(Normalising, "CurrencyAmount", "publishes no apply and no copy, and destructures through unapply") {
      val CurrencyAmount(currency, amount) = currencyAmount
      currency shouldBe gbp
      amount shouldBe 100.0
      assertCompiles("""CurrencyAmount.of(currencyAmount.currency, currencyAmount.amount)""")
      assertTypeError("""CurrencyAmount(currencyAmount.currency, currencyAmount.amount)""")
      assertTypeError("""currencyAmount.copy(amount = currencyAmount.amount)""")
    },
    row(Normalising, "Money", "publishes no apply and no copy, and destructures through unapply") {
      val Money(currency, amount) = money
      currency shouldBe gbp
      amount shouldBe decimal
      assertCompiles("""Money.of(money.currency, money.amount)""")
      assertTypeError("""Money(money.currency, money.amount)""")
      assertTypeError("""money.copy(amount = money.amount)""")
    },
    row(Normalising, "BigMoney", "publishes no apply and no copy, and destructures through unapply") {
      val BigMoney(currency, amount) = bigMoney
      currency shouldBe gbp
      amount shouldBe decimal
      assertCompiles("""BigMoney.of(bigMoney.currency, bigMoney.amount)""")
      assertTypeError("""BigMoney(bigMoney.currency, bigMoney.amount)""")
      assertTypeError("""bigMoney.copy(amount = bigMoney.amount)""")
    },
    row(Normalising, "MultiCurrencyAmount", "publishes no apply and no copy, and destructures through unapply") {
      val MultiCurrencyAmount(amounts) = multiCurrencyAmount
      amounts.keySet shouldBe Set(gbp)
      amounts(gbp) shouldBe 100.0
      assertCompiles("""MultiCurrencyAmount.of(multiCurrencyAmount.amounts)""")
      assertTypeError("""MultiCurrencyAmount(multiCurrencyAmount.amounts)""")
      assertTypeError("""multiCurrencyAmount.copy(amounts = multiCurrencyAmount.amounts)""")
    },
    row(Normalising, "Tenor", "publishes no apply and no copy, and destructures through unapply") {
      val Tenor(period) = tenor
      period shouldBe Period.ofMonths(3)
      assertCompiles("""Tenor.of(tenor.period)""")
      assertTypeError("""Tenor(tenor.period)""")
      assertTypeError("""tenor.copy(period = tenor.period)""")
    },
    row(Normalising, "Frequency", "publishes no apply and no copy, and destructures through unapply") {
      val Frequency(period) = frequency
      period shouldBe Period.ofMonths(3)
      assertCompiles("""Frequency.of(frequency.period)""")
      assertTypeError("""Frequency(frequency.period)""")
      assertTypeError("""frequency.copy(period = frequency.period)""")
    },
    row(Normalising, "SequenceDate", "publishes no apply and no copy, and destructures through unapply") {
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
    },
    row(Normalising, "HolidayCalendarId", "publishes no apply and no copy, and destructures through unapply") {
      val HolidayCalendarId(name) = compositeCalendarId
      name shouldBe "GBLO+USNY"
      assertCompiles("""HolidayCalendarId.of(compositeCalendarId.name)""")
      assertTypeError("""HolidayCalendarId(compositeCalendarId.name)""")
      assertTypeError("""compositeCalendarId.copy(name = compositeCalendarId.name)""")
    },
    row(Normalising, "Decimal", "publishes no apply and no copy, and destructures through unapply") {
      val Decimal(unscaled, scale) = decimal
      unscaled shouldBe 1234L
      scale shouldBe 2
      assertCompiles("""Decimal.ofScaled(decimal.unscaled, decimal.scale)""")
      assertTypeError("""Decimal(decimal.unscaled, decimal.scale)""")
      assertTypeError("""decimal.copy(scale = decimal.scale)""")
    },
    row(
      Normalising,
      "ImmutableHolidayCalendar",
      "publishes no apply and no copy, and destructures through unapply") {
      // `of` normalises what it is given, so the factory is the only way in. The case parameter is
      // the identifier, which is the whole of this type's equality - as in the library being
      // ported, where two calendars claiming to be `GBLO` are one calendar - so `unapply` hands
      // back exactly what equality is defined on.
      //
      // The packed months the calendar answers from are abstract members rather than case
      // parameters, and that is what keeps them out of reach: the last two assertions are that
      // they cannot be read from outside the `date` package at all, which would not be true of a
      // case parameter, since `unapply` would hand the array over to anyone.
      val ImmutableHolidayCalendar(calendarId) = immutableCalendar
      calendarId shouldBe HolidayCalendarId.of("APISURFACE")
      immutableCalendar.holidays.toList shouldBe List(jan15)
      immutableCalendar.weekendDays shouldBe Set(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY)
      immutableCalendar.startYear shouldBe jan15.getYear
      assertCompiles(
        """ImmutableHolidayCalendar.of(
           |  immutableCalendar.id,
           |  immutableCalendar.holidays,
           |  immutableCalendar.weekendDays)""".stripMargin)
      assertTypeError("""ImmutableHolidayCalendar(immutableCalendar.id)""")
      assertTypeError("""immutableCalendar.copy(id = immutableCalendar.id)""")
      assertTypeError("""new ImmutableHolidayCalendar(immutableCalendar.id) {}""")
      assertTypeError("""immutableCalendar.lookup""")
      assertTypeError("""immutableCalendar.weekends""")
    })

  //-------------------------------------------------------------------------
  // The total types, which deliberately keep the case class surface.
  //
  // A total type accepts every well-typed input, has nothing to check and nothing to rewrite, so
  // it keeps `apply`, `copy` and `unapply`: hiding them would cost callers convenience and buy no
  // invariant. Every total type of the construction policy is audited here, so that a later
  // tidy-up which "made every type consistent" by locking one of them down would fail and be
  // discussed rather than absorbed.
  //-------------------------------------------------------------------------
  private val totalRows: List[SurfaceRow] = List(
    row(Total, "CurrencyPair", "is a total type and keeps its public apply and copy") {
      val CurrencyPair(base, counter) = gbpUsd
      base shouldBe gbp
      counter shouldBe usd
      CurrencyPair(gbp, usd) shouldBe gbpUsd
      gbpUsd.copy(counter = gbp) shouldBe CurrencyPair.of(gbp, gbp)
      assertCompiles("""CurrencyPair(gbpUsd.base, gbpUsd.counter)""")
      assertCompiles("""gbpUsd.copy(base = gbpUsd.counter)""")
    },
    row(Total, "PriceIndexObservation", "is a total type and keeps its public apply and copy") {
      val PriceIndexObservation(index, fixingMonth) = priceObservation
      index shouldBe priceIndex
      fixingMonth shouldBe YearMonth.of(2020, 6)
      PriceIndexObservation(priceIndex, YearMonth.of(2020, 6)) shouldBe priceObservation
      priceObservation.copy(fixingMonth = YearMonth.of(2020, 7)).fixingMonth shouldBe YearMonth.of(2020, 7)
      assertCompiles("""PriceIndexObservation(priceObservation.index, priceObservation.fixingMonth)""")
      assertCompiles("""priceObservation.copy(fixingMonth = priceObservation.fixingMonth)""")
    },
    row(Total, "Payment", "is a total type and keeps its public apply and copy") {
      val Payment(value, date) = payment
      value shouldBe currencyAmount
      date shouldBe jan15
      Payment(currencyAmount, jan15) shouldBe payment
      payment.copy(date = apr15).date shouldBe apr15
      assertCompiles("""Payment(payment.value, payment.date)""")
      assertCompiles("""payment.copy(value = payment.value)""")
    },
    row(Total, "AdjustablePayment", "is a total type and keeps its public apply and copy") {
      val AdjustablePayment(value, date) = adjustablePayment
      value shouldBe currencyAmount
      date shouldBe adjustableDate
      AdjustablePayment(currencyAmount, adjustableDate) shouldBe adjustablePayment
      adjustablePayment.copy(value = currencyAmount) shouldBe adjustablePayment
      assertCompiles("""AdjustablePayment(adjustablePayment.value, adjustablePayment.date)""")
      assertCompiles("""adjustablePayment.copy(date = adjustablePayment.date)""")
    },
    row(Total, "AdjustableDate", "is a total type and keeps its public apply and copy") {
      val AdjustableDate(unadjusted, dateAdjustment) = adjustableDate
      unadjusted shouldBe jan15
      dateAdjustment shouldBe adjustment
      AdjustableDate(jan15, adjustment) shouldBe adjustableDate
      adjustableDate.copy(unadjusted = apr15).unadjusted shouldBe apr15
      assertCompiles("""AdjustableDate(adjustableDate.unadjusted, adjustableDate.adjustment)""")
      assertCompiles("""adjustableDate.copy(adjustment = adjustableDate.adjustment)""")
    },
    row(Total, "BusinessDayAdjustment", "is a total type and keeps its public apply and copy") {
      val BusinessDayAdjustment(convention, calendar) = adjustment
      convention shouldBe modifiedFollowing
      calendar shouldBe gbloId
      BusinessDayAdjustment(modifiedFollowing, gbloId) shouldBe adjustment
      adjustment.copy(calendar = HolidayCalendarIds.NO_HOLIDAYS).calendar shouldBe
        HolidayCalendarIds.NO_HOLIDAYS
      assertCompiles("""BusinessDayAdjustment(adjustment.convention, adjustment.calendar)""")
      assertCompiles("""adjustment.copy(convention = adjustment.convention)""")
    },
    row(Total, "ValueAdjustment", "is a total type and keeps its public apply and copy") {
      val ValueAdjustment(modifyingValue, adjustmentType) = valueAdjustment
      modifyingValue shouldBe 1000.0
      adjustmentType shouldBe ValueAdjustmentType.Replace
      ValueAdjustment(1000.0, ValueAdjustmentType.Replace) shouldBe valueAdjustment
      valueAdjustment.copy(modifyingValue = 2000.0).modifyingValue shouldBe 2000.0
      assertCompiles("""ValueAdjustment(valueAdjustment.modifyingValue, valueAdjustment.`type`)""")
      assertCompiles("""valueAdjustment.copy(modifyingValue = valueAdjustment.modifyingValue)""")
    },
    row(Total, "ValueDerivatives", "is a total type and keeps its public apply and copy") {
      val ValueDerivatives(value, derivatives) = valueDerivatives
      value shouldBe 1.5
      derivatives.toList shouldBe List(0.5, 0.25)
      ValueDerivatives(1.5, DoubleArray.of(0.5, 0.25)) shouldBe valueDerivatives
      valueDerivatives.copy(value = 2.5).value shouldBe 2.5
      assertCompiles("""ValueDerivatives(valueDerivatives.value, valueDerivatives.derivatives)""")
      assertCompiles("""valueDerivatives.copy(derivatives = valueDerivatives.derivatives)""")
    },
    row(Total, "CalculationTargetList", "is a total type and keeps its public apply and copy") {
      val CalculationTargetList(targets) = calculationTargetList
      targets shouldBe List(calculationTarget)
      CalculationTargetList(List(calculationTarget)) shouldBe calculationTargetList
      calculationTargetList.copy(targets = Nil).targets shouldBe Nil
      assertCompiles("""CalculationTargetList(calculationTargetList.targets)""")
      assertCompiles("""calculationTargetList.copy(targets = calculationTargetList.targets)""")
    },
    row(Total, "ReferenceData.Entry", "is a total type and keeps its public apply and copy") {
      // The entry is what makes filing type-safe - it pairs an identifier only with a value of
      // its own type - and beyond that it has nothing to check, the compiler having checked it.
      // So it keeps the whole case class surface, and the identifier's type parameter travels
      // through `copy`; the last assertion is the check itself, which is a compile error rather
      // than a validation failure.
      val ReferenceData.Entry(id, value) = referenceDataEntry
      id shouldBe gbloId
      value shouldBe gblo
      ReferenceData.Entry(gbloId, gblo) shouldBe referenceDataEntry
      referenceDataEntry.copy(id = HolidayCalendarIds.USNY).id shouldBe HolidayCalendarIds.USNY
      assertCompiles("""ReferenceData.Entry(referenceDataEntry.id, referenceDataEntry.value)""")
      assertCompiles("""referenceDataEntry.copy(value = referenceDataEntry.value)""")
      assertTypeError("""ReferenceData.Entry(gbloId, "GBLO")""")
    },
    row(
      Total,
      "DoubleArray",
      "is total through copy-safe factories, which is the policy's own representation exception") {
      // The construction policy's one representation exception, and the primitive array is the
      // reason for it: a case class over `Array[Double]` would hand the array out through
      // `unapply` and `copy`, so this is a final class whose factories copy instead. It is total
      // in the sense that matters - every well-typed input is accepted, nothing is rejected and
      // nothing rewritten - and the copy safety that stands in for the case surface is audited by
      // the policy rows at the end of this file.
      doubleArray.size shouldBe 3
      doubleArray.get(0) shouldBe 1.0
      DoubleArray.of(1.0, 2.0, 3.0) shouldBe doubleArray
      DoubleArray.copyOf(Array(1.0, 2.0, 3.0)) shouldBe doubleArray
      DoubleArray.tabulate(3)(index => index + 1.0) shouldBe doubleArray
      DoubleArray.filled(3) shouldBe DoubleArray.of(0.0, 0.0, 0.0)
      assertTypeError("""doubleArray.copy()""")
      assertTypeError("""val DoubleArray(values) = doubleArray""")
    },
    row(
      Total,
      "DoubleMatrix",
      "is total through copy-safe factories, which is the policy's own representation exception") {
      doubleMatrix.rowCount shouldBe 2
      doubleMatrix.get(1, 0) shouldBe 3.0
      DoubleMatrix.of(2, 2, 1.0, 2.0, 3.0, 4.0) shouldBe doubleMatrix
      DoubleMatrix.copyOf(Array(Array(1.0, 2.0), Array(3.0, 4.0))) shouldBe doubleMatrix
      DoubleMatrix.tabulate(2, 2)((rowIndex, columnIndex) =>
        rowIndex * 2.0 + columnIndex + 1.0) shouldBe doubleMatrix
      assertTypeError("""doubleMatrix.copy()""")
      assertTypeError("""val DoubleMatrix(values) = doubleMatrix""")
    })

  //-------------------------------------------------------------------------
  // The closed families.
  //
  // The implementation being ported resolved a convention or an index through a registry that a
  // configuration file on the classpath could add to at run time. The port replaced that with
  // sealed families whose members exist only in their companions, which is what lets a match over
  // a family be checked for exhaustiveness and what makes `values` the whole truth about it.
  //
  // Each row states that the family's own members are reachable, then proves the sealing with a
  // trait probe, then - where the family also hides its constructor - asserts that separately
  // with a class probe. The two are different properties and neither stands in for the other: a
  // class probe over a family with a package-private constructor is rejected whether or not the
  // family is sealed, which is exactly why the trait probe exists. The control that keeps the
  // trait probe honest is the last test of this file.
  //-------------------------------------------------------------------------
  private val closedFamilyRows: List[SurfaceRow] = List(
    row(
      ClosedFamily,
      "Index",
      "is sealed against a subtype declared outside its file, against a class as well as a trait") {
      (iborIndex: Index).name shouldBe "GBP-LIBOR-3M"
      Index.parse("GBP-LIBOR-3M") shouldBe Right(iborIndex)
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends Index; () }""")
      // the same over a class supplying every member, which is what someone extending the
      // family would write: this family is a trait, so sealing is again the only cause
      assertTypeError("""{ final class Host extends Index { def name: String = "Bespoke-Index" }; () }""")
    },
    row(
      ClosedFamily,
      "RateIndex",
      "is sealed against a subtype declared outside its file, against a class as well as a trait") {
      (overnightIndex: RateIndex).fixingCalendar shouldBe gbloId
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends RateIndex; () }""")
      // the same over a class supplying every member, which is what someone extending the
      // family would write: this family is a trait, so sealing is again the only cause
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
    },
    row(
      ClosedFamily,
      "FloatingRateIndex",
      "is sealed against a subtype declared outside its file, against a class as well as a trait") {
      (priceIndex: FloatingRateIndex).currency shouldBe gbp
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends FloatingRateIndex; () }""")
      // the same over a class supplying every member, which is what someone extending the
      // family would write: this family is a trait, so sealing is again the only cause
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
    },
    row(
      ClosedFamily,
      "IborIndex",
      "is sealed against a subtype declared outside its file, and hides its constructor") {
      iborIndex.tenor shouldBe tenor
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends IborIndex; () }""")
      // constructor privacy, a separate property asserted separately: the class probe is
      // rejected by `private[index]` too, so it cannot stand in for the assertion
      // above; its arguments are read off a published value, so nothing else can be the cause
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
    },
    row(
      ClosedFamily,
      "OvernightIndex",
      "is sealed against a subtype declared outside its file, and hides its constructor") {
      overnightIndex.currency shouldBe gbp
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends OvernightIndex; () }""")
      // constructor privacy, a separate property asserted separately: the class probe is
      // rejected by `private[index]` too, so it cannot stand in for the assertion
      // above; its arguments are read off a published value, so nothing else can be the cause
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
    },
    row(
      ClosedFamily,
      "PriceIndex",
      "is sealed against a subtype declared outside its file, and hides its constructor") {
      priceIndex.region shouldBe valueOf(Country.of("GB"))
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends PriceIndex; () }""")
      // constructor privacy, a separate property asserted separately: the class probe is
      // rejected by `private[index]` too, so it cannot stand in for the assertion
      // above; its arguments are read off a published value, so nothing else can be the cause
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
    },
    row(ClosedFamily, "FxIndex", "is sealed against a subtype declared outside its file, and hides its constructor") {
      fxIndex.currencyPair.base shouldBe Currency.EUR
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends FxIndex; () }""")
      // constructor privacy, a separate property asserted separately: the class probe is
      // rejected by `private[index]` too, so it cannot stand in for the assertion
      // above; its arguments are read off a published value, so nothing else can be the cause
      assertTypeError(
        """{
           |  final class Host extends FxIndex(
           |      fxIndex.name,
           |      fxIndex.currencyPair,
           |      fxIndex.fixingCalendar,
           |      fxIndex.maturityDateOffset)
           |  ()
           |}""".stripMargin)
    },
    row(
      ClosedFamily,
      "HolidayCalendar",
      "is sealed against a subtype declared outside its file, against a class as well as a trait") {
      HolidayCalendars.SAT_SUN.name shouldBe "Sat/Sun"
      gblo.isHoliday(LocalDate.of(2020, 12, 25)) shouldBe true
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends HolidayCalendar; () }""")
      // the same over a class supplying every member, which is what someone extending the
      // family would write: this family is a trait, so sealing is again the only cause
      assertTypeError(
        """{
           |  final class Host extends HolidayCalendar {
           |    def id: HolidayCalendarId = gbloId
           |    def isHoliday(date: LocalDate): Boolean = date.getDayOfMonth == 1
           |  }
           |  ()
           |}""".stripMargin)
    },
    row(
      ClosedFamily,
      "DayCount",
      "is sealed against a subtype declared outside its file, and hides its constructor") {
      DayCounts.ACT_365F.name shouldBe "Act/365F"
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends DayCount; () }""")
      // constructor privacy, a separate property asserted separately: the class probe is
      // rejected by `private[date]` too, so it cannot stand in for the assertion
      // above; its arguments are read off a published value, so nothing else can be the cause
      assertTypeError("""{ final class Host extends DayCount("Bespoke-Day-Count"); () }""")
    },
    row(
      ClosedFamily,
      "Rounding",
      "is sealed against a subtype declared outside its file, against a class as well as a trait") {
      val rounding: Rounding = Rounding.of(gbp)
      rounding.round(1.2345) shouldBe 1.23
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends Rounding; () }""")
      // the same over a class supplying every member, which is what someone extending the
      // family would write: this family is a trait, so sealing is again the only cause
      assertTypeError(
        """{
           |  final class Host extends Rounding {
           |    def round(value: BigDecimal): BigDecimal = value
           |    def round(value: Decimal): Decimal = value
           |  }
           |  ()
           |}""".stripMargin)
    },
    row(
      ClosedFamily,
      "Currency",
      "is sealed against a subtype declared outside its file, and hides its constructor") {
      gbp.minorUnitDigits shouldBe 2
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends Currency; () }""")
      // constructor privacy, a separate property asserted separately: the class probe is
      // rejected by `private[currency]` too, so it cannot stand in for the assertion
      // above; its arguments are read off a published value, so nothing else can be the cause
      assertTypeError("""{ final class Host extends Currency("XYZ", 2, "USD"); () }""")
    },
    row(
      ClosedFamily,
      "BusinessDayConvention",
      "is sealed against a subtype declared outside its file, and hides its constructor") {
      modifiedFollowing.adjust(jan15, gblo) shouldBe jan15
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends BusinessDayConvention; () }""")
      // constructor privacy, a separate property asserted separately: the class probe is
      // rejected by `private[date]` too, so it cannot stand in for the assertion
      // above; its arguments are read off a published value, so nothing else can be the cause
      assertTypeError(
        """{
           |  final class Host extends BusinessDayConvention("Bespoke-Business-Day-Convention") {
           |    def adjust(date: LocalDate, calendar: HolidayCalendar): LocalDate =
           |      if (calendar.isHoliday(date)) date.plusDays(1L) else date
           |  }
           |  ()
           |}""".stripMargin)
    },
    row(
      ClosedFamily,
      "RollConvention",
      "is sealed against a subtype declared outside its file, and hides its constructor") {
      RollConventions.EOM.adjust(jan15) shouldBe LocalDate.of(2020, 1, 31)
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends RollConvention; () }""")
      // constructor privacy, a separate property asserted separately: the class probe is
      // rejected by `private[schedule]` too, so it cannot stand in for the assertion
      // above; its arguments are read off a published value, so nothing else can be the cause
      assertTypeError(
        """{
           |  final class Host extends RollConvention("Bespoke-Roll-Convention") {
           |    def adjust(date: LocalDate): LocalDate = date
           |  }
           |  ()
           |}""".stripMargin)
    },
    row(
      ClosedFamily,
      "PeriodAdditionConvention",
      "is sealed against a subtype declared outside its file, and hides its constructor") {
      (PeriodAdditionConventions.LAST_DAY: PeriodAdditionConvention).isMonthBased shouldBe true
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends PeriodAdditionConvention; () }""")
      // constructor privacy, a separate property asserted separately: the class probe is
      // rejected by `private[date]` too, so it cannot stand in for the assertion
      // above; its arguments are read off a published value, so nothing else can be the cause
      assertTypeError(
        """{
           |  final class Host extends PeriodAdditionConvention("Bespoke-Period-Addition-Convention") {
           |    def adjust(baseDate: LocalDate, period: Period, calendar: HolidayCalendar): LocalDate =
           |      baseDate.plus(period)
           |    def isMonthBased: Boolean = true
           |  }
           |  ()
           |}""".stripMargin)
    },
    row(
      ClosedFamily,
      "DateSequence",
      "is sealed against a subtype declared outside its file, and hides its constructor") {
      (DateSequences.QUARTERLY_IMM: DateSequence).name shouldBe "Quarterly-IMM"
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends DateSequence; () }""")
      // constructor privacy, a separate property asserted separately: the class probe is
      // rejected by `private[date]` too, so it cannot stand in for the assertion
      // above; its arguments are read off a published value, so nothing else can be the cause
      assertTypeError(
        """{
           |  final class Host extends DateSequence("Bespoke-Date-Sequence") {
           |    def nextOrSame(date: LocalDate): LocalDate = date.withDayOfMonth(1)
           |    def dateMatching(yearMonth: YearMonth): LocalDate = yearMonth.atDay(1)
           |  }
           |  ()
           |}""".stripMargin)
    },
    row(
      ClosedFamily,
      "StubConvention",
      "is sealed against a subtype declared outside its file, and hides its constructor") {
      StubConvention.SHORT_INITIAL.isCalculateBackwards shouldBe true
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends StubConvention; () }""")
      // constructor privacy, a separate property asserted separately: the class probe is
      // rejected by `private[schedule]` too, so it cannot stand in for the assertion
      // above; its arguments are read off a published value, so nothing else can be the cause
      assertTypeError("""{ final class Host extends StubConvention("Bespoke-Stub-Convention"); () }""")
    },
    row(
      ClosedFamily,
      "FloatingRateType",
      "is sealed against a subtype declared outside its file, and hides its constructor") {
      FloatingRateType.Ibor.isIbor shouldBe true
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends FloatingRateType; () }""")
      // constructor privacy, a separate property asserted separately: the class probe is
      // rejected by `private[index]` too, so it cannot stand in for the assertion
      // above; its arguments are read off a published value, so nothing else can be the cause
      assertTypeError("""{ final class Host extends FloatingRateType("Bespoke-Floating-Rate-Type"); () }""")
    },
    row(
      ClosedFamily,
      "ValueAdjustmentType",
      "is sealed against a subtype declared outside its file, and hides its constructor") {
      ValueAdjustmentType.Replace.adjust(100.0, 200.0) shouldBe 200.0
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends ValueAdjustmentType; () }""")
      // constructor privacy, a separate property asserted separately: the class probe is
      // rejected by `private[value]` too, so it cannot stand in for the assertion
      // above; its arguments are read off a published value, so nothing else can be the cause
      assertTypeError(
        """{
           |  final class Host extends ValueAdjustmentType("Bespoke-Value-Adjustment-Type") {
           |    def adjust(baseValue: Double, modifyingValue: Double): Double =
           |      baseValue + modifyingValue
           |  }
           |  ()
           |}""".stripMargin)
    },
    row(
      ClosedFamily,
      "FloatingRateName",
      "is sealed against a subtype declared outside its file, and hides its constructor") {
      gbpLiborName.externalName shouldBe "GBP-LIBOR"
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends FloatingRateName; () }""")
      // constructor privacy, a separate property asserted separately: the class probe is
      // rejected by `private[index]` too, so it cannot stand in for the assertion
      // above; its arguments are read off a published value, so nothing else can be the cause
      assertTypeError(
        """{
           |  final class Host extends FloatingRateName(
           |      gbpLiborName.externalName,
           |      gbpLiborName.indexName,
           |      gbpLiborName.rateType,
           |      gbpLiborName.fixingDateOffsetDays)
           |  ()
           |}""".stripMargin)
    },
    row(
      ClosedFamily,
      "Failure",
      "is sealed against a subtype declared outside its file, against a class as well as a trait") {
      Failure.Invalid("a reason").reason shouldBe FailureReason.INVALID
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends Failure; () }""")
      // the same over a class supplying every member, which is what someone extending the
      // family would write: this family is a trait, so sealing is again the only cause
      assertTypeError(
        """{
           |  final case class Host(message: String) extends Failure {
           |    def reason: FailureReason = FailureReason.ERROR
           |    def attributes: scala.collection.immutable.SortedMap[String, String] =
           |      scala.collection.immutable.SortedMap.empty[String, String]
           |  }
           |  ()
           |}""".stripMargin)
    },
    row(
      ClosedFamily,
      "IndexObservation",
      "is sealed against an observation declared outside its file, and matches exhaustively") {
      // The observations are a closed set of four, one per index family, and the families are
      // themselves closed - so there is no fifth kind of thing to observe, and the trait says so.
      // The positive half is the exhaustiveness the sealing buys: `kindOfObservation` matches the
      // four types with no default branch, and a family that admitted a fifth type would make that
      // match inexhaustive, which this build's fatal-warning setting turns into a compilation
      // error. It is therefore not merely an assertion here but a condition of this file
      // compiling at all.
      val observations: List[IndexObservation] =
        List(iborObservation, overnightObservation, priceObservation, fxObservation)
      observations.map(kindOfObservation) shouldBe List("Ibor", "Overnight", "Price", "Fx")
      observations.map(_.index) shouldBe List(iborIndex, overnightIndex, priceIndex, fxIndex)
      // The negative half. Both names the snippet uses resolve in this scope - the control on the
      // first line proves it - and the trait's single member is satisfied by the body, so the only
      // thing left to reject the declaration is the `sealed` modifier on the trait.
      assertCompiles("""{ val witness: Index = iborIndex; witness.name; () }""")
      assertTypeError("""{ trait Host extends IndexObservation; () }""")
      assertTypeError(
        """{
           |  final class Host extends IndexObservation { def index: Index = iborIndex }
           |  ()
           |}""".stripMargin)
    },
    row(
      ClosedFamily,
      "FailureReason",
      "is sealed against a subtype declared outside its file, and hides its constructor") {
      FailureReason.MISSING_DATA.name shouldBe "MISSING_DATA"
      // sealing, isolated: the probe is a trait, so it calls no constructor and implements no
      // member, and the `sealed` modifier is the only objection the compiler has left
      assertTypeError("""{ trait Host extends FailureReason; () }""")
      // constructor privacy, a separate property asserted separately: the class probe is
      // rejected by `private[result]` too, so it cannot stand in for the assertion
      // above; its arguments are read off a published value, so nothing else can be the cause
      assertTypeError("""{ final class Host extends FailureReason("BESPOKE"); () }""")
    })

  //-------------------------------------------------------------------------
  // The contracts that are deliberately open.
  //
  // Sealing is not free, and several traits of this module pay a price that is too high. An
  // application supplies its own holidays by implementing `ReferenceData`, and names data of its
  // own by implementing `ReferenceDataId`, which is what the library being ported documented and
  // what the module's own `HolidaySafeReferenceData` does, so sealing either would make both
  // impossible. `FloatingRate` is implemented by `FloatingRateName` as well as by the index
  // families, and sealing it would drag `FloatingRateName` and its four hundred rows of data into
  // `Index.scala`. `IndexObservation` is not among them: it is sealed, its four implementations
  // share its file, and its closedness is proved in the group above.
  //
  // The rest are the forward-path contracts kept for the next slice of the migration, which have
  // no implementation inside this module at all. Being implementable is the only property this
  // slice can assert about them, and an unnoticed change to their shape would be found by nothing
  // else - so the implementations live in the companion as ordinary classes rather than inside
  // compile-assertion strings, which puts the compiler's check on them at every build.
  //-------------------------------------------------------------------------
  private val openContractRows: List[SurfaceRow] = List(
    row(OpenContract, "ReferenceData", "is open, so an application can supply reference data of its own") {
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
    },
    row(OpenContract, "ReferenceDataId", "is open, so an application can supply identifiers of its own") {
      val id: ReferenceDataId[HolidayCalendar] = HostReferenceDataId("a label")
      id.resolve(ReferenceData.empty).isLeft shouldBe true
      id.toReader.run(ReferenceData.empty).isLeft shouldBe true
      HostReferenceDataId("a label") shouldBe id
    },
    row(OpenContract, "FloatingRate", "is open, so FloatingRateName can implement it from another file") {
      val host: FloatingRate = new HostFloatingRate("HOST-LIBOR", gbpLiborName)
      host.name shouldBe "HOST-LIBOR"
      host.floatingRateName shouldBe gbpLiborName
      (gbpLiborName: FloatingRate).floatingRateName shouldBe gbpLiborName
    },
    row(OpenContract, "CalculationTarget", "and Resolvable are open, as the next migration slice requires") {
      val target: CalculationTarget = HostTarget(currencyAmount)
      target shouldBe HostTarget(currencyAmount)
      val resolvable: Resolvable[HostTarget] = new HostResolvable(currencyAmount)
      resolvable.resolve(refData) shouldBe Right(HostTarget(currencyAmount))
      resolvable.resolve(ReferenceData.empty).isLeft shouldBe true
      val resolvableTarget: ResolvableCalculationTarget = new HostResolvableTarget(currencyAmount)
      resolvableTarget.resolveTarget(refData) shouldBe Right(HostTarget(currencyAmount))
    },
    row(OpenContract, "DateAdjuster", "is open, and can also be built from a function") {
      val host: DateAdjuster = new HostDateAdjuster(3)
      host.adjust(jan15) shouldBe LocalDate.of(2020, 1, 18)
      DateAdjuster(date => date.plusDays(1L)).adjust(jan15) shouldBe LocalDate.of(2020, 1, 16)
    },
    row(
      OpenContract,
      "FxRateProvider",
      "and FxConvertible are open, so a host can supply rates and convert with them") {
      val provider: FxRateProvider = new HostRateProvider(gbpUsd, 1.25)
      provider.fxRate(gbp, usd) shouldBe Right(1.25)
      provider.fxRate(usd, gbp) shouldBe Right(0.8)
      provider.fxRate(usd, Currency.EUR).isLeft shouldBe true
      val convertible: FxConvertible[HostConvertible] = HostConvertible(currencyAmount)
      convertible.convertedTo(usd, provider) shouldBe Right(HostConvertible(valueOf(CurrencyAmount.of(usd, 125.0))))
    })

  //-------------------------------------------------------------------------
  // What is left of the public surface once the data types are accounted for.
  //
  // A data type is covered twice over: by its construction-kind row above, and by the port's
  // codec inventory, which lists every type it serializes and every type it excludes with the
  // reason. Neither list can hold the surfaces below, because none of them is data: four are the
  // single-abstract-method callbacks the numeric wrappers take instead of boxing an index, one is
  // the lookup function type `FloatingRate` composes, five are the type aliases the two module
  // roots re-export, and one is the witness reference data narrows a lookup with. They are
  // recorded here so that the two inventories '''together''' account for every public surface of
  // both modules, which is the property the codec inventory's closure claim rests on.
  //-------------------------------------------------------------------------
  private val functionSurfaceRows: List[SurfaceRow] = List(
    row(
      FunctionSurface,
      "DoubleArray.DoubleTernaryOperator",
      "is a callback a lambda satisfies, and carries no data to serialize") {
      // The port's replacement for the primitive functional interface the Java original took
      // here: a single-abstract-method trait, so a lambda is converted to it and the call site
      // reads as a three-argument function, while the compiled method passes `double` rather than
      // a boxed argument. A caller that wants to retain an operator implements it explicitly.
      val other: DoubleArray = DoubleArray.of(4.0, 5.0, 6.0)
      doubleArray.combineReduce(other, (total, first, second) => total + first * second) shouldBe 32.0
      val explicit: DoubleArray.DoubleTernaryOperator = new DoubleArray.DoubleTernaryOperator {
        override def applyAsDouble(total: Double, first: Double, second: Double): Double =
          total + first * second
      }
      doubleArray.combineReduce(other, explicit) shouldBe 32.0
    },
    row(
      FunctionSurface,
      "DoubleMatrix.ElementAction",
      "is a callback a lambda satisfies, and carries no data to serialize") {
      // The callback `forEach` takes. The action is applied in row-major order, which the cells
      // below record; the assignment keeps the lambda's result `Unit`, as the callback's own
      // method is.
      val cells: Array[String] = Array.fill(4)("")
      doubleMatrix.forEach((rowIndex, columnIndex, value) =>
        cells(rowIndex * 2 + columnIndex) = s"$rowIndex$columnIndex=$value")
      cells.toList shouldBe List("00=1.0", "01=2.0", "10=3.0", "11=4.0")
    },
    row(
      FunctionSurface,
      "DoubleMatrix.ElementFunction",
      "is a callback a lambda satisfies, and carries no data to serialize") {
      // The callback `mapWithIndex` takes, which differs from `ElementAction` only in answering
      // with the new value of the element rather than with nothing.
      doubleMatrix.mapWithIndex((rowIndex, columnIndex, value) =>
        value + rowIndex + columnIndex) shouldBe DoubleMatrix.of(2, 2, 1.0, 3.0, 4.0, 6.0)
    },
    row(
      FunctionSurface,
      "DoubleMatrix.RowArrayFunction",
      "is a callback a lambda satisfies, and carries no data to serialize") {
      // The callback `ofArrays` takes, which builds a matrix one row of primitives at a time.
      DoubleMatrix.ofArrays(2, 2)(rowIndex =>
        Array(rowIndex * 2.0 + 1.0, rowIndex * 2.0 + 2.0)) shouldBe doubleMatrix
    },
    row(
      FunctionSurface,
      "DoubleMatrix.RowArrayObjectFunction",
      "is a callback a lambda satisfies, and carries no data to serialize") {
      // The same for `ofArrayObjects`, whose rows are `DoubleArray` values rather than arrays.
      DoubleMatrix.ofArrayObjects(2, 2)(rowIndex =>
        DoubleArray.of(rowIndex * 2.0 + 1.0, rowIndex * 2.0 + 2.0)) shouldBe doubleMatrix
    },
    row(
      FunctionSurface,
      "FloatingRate.Lookup",
      "is the function type a family's probe takes, and carries no data to serialize") {
      // One family's alias-aware lookup by name, widened to the floating rate it answers with.
      // It is a function type, so a host composes a search of its own from ordinary lambdas, and
      // the standard composition is the four families of this module in their documented order.
      val hostLookup: FloatingRate.Lookup =
        name => if (name == "HOST-LIBOR") Some(gbpLiborName) else None
      hostLookup("HOST-LIBOR") shouldBe Some(gbpLiborName)
      hostLookup("GBP-LIBOR") shouldBe None
      FloatingRate.tryParseWith("GBP-LIBOR-3M", FloatingRate.standardLookups) shouldBe Some(iborIndex)
      FloatingRate.tryParseWith("HOST-LIBOR", List(hostLookup)) shouldBe Some(gbpLiborName)
    })

  private val aliasSurfaceRows: List[SurfaceRow] = List(
    row(
      AliasSurface,
      "collect.FailureOr",
      "is the module root's name for the type the result package defines") {
      // The four aliases below are re-exports: the module root names the same type its `result`
      // package defines, so that a caller importing the root needs no second import. Each row is
      // the proof that the two names denote one type - the evidence is a compile-time one, and
      // applying it is what uses it - rather than two similar ones that could drift apart.
      val nested: com.opengamma.strata.collect.result.FailureOr[Int] = Right(1)
      val root: FailureOr[Int] = nested
      root shouldBe Right(1)
      val evidence: FailureOr[Int] =:= com.opengamma.strata.collect.result.FailureOr[Int] = implicitly
      evidence(root) shouldBe nested
    },
    row(
      AliasSurface,
      "collect.ResultNec",
      "is the module root's name for the type the result package defines") {
      val nested: com.opengamma.strata.collect.result.ResultNec[Int] = Right(1)
      val root: ResultNec[Int] = nested
      root shouldBe Right(1)
      val evidence: ResultNec[Int] =:= com.opengamma.strata.collect.result.ResultNec[Int] = implicitly
      evidence(root) shouldBe nested
    },
    row(
      AliasSurface,
      "collect.ValidatedFailures",
      "is the module root's name for the type the result package defines") {
      val nested: com.opengamma.strata.collect.result.ValidatedFailures[Int] = cats.data.Validated.validNec(1)
      val root: ValidatedFailures[Int] = nested
      root.isValid shouldBe true
      val evidence: ValidatedFailures[Int] =:=
        com.opengamma.strata.collect.result.ValidatedFailures[Int] = implicitly
      evidence(root) shouldBe nested
    },
    row(
      AliasSurface,
      "collect.ValueWithFailures",
      "is the module root's name for the type the result package defines") {
      val nested: com.opengamma.strata.collect.result.ValueWithFailures[Int] = cats.data.Ior.right(1)
      val root: ValueWithFailures[Int] = nested
      root.right shouldBe Some(1)
      val evidence: ValueWithFailures[Int] =:=
        com.opengamma.strata.collect.result.ValueWithFailures[Int] = implicitly
      evidence(root) shouldBe nested
    },
    row(
      AliasSurface,
      "basics.RefDataReader",
      "is the name this module gives a resolution awaiting reference data") {
      // Not a re-export but a named partial application: the build carries no compiler plugin
      // supplying type-lambda syntax, so the failure type has to be applied by a named alias
      // before `Kleisli` can be written at all. The alias is public because every `toReader` of
      // this module answers with one.
      val reader: RefDataReader[HolidayCalendar] = gbloId.toReader
      reader.run(refData) shouldBe Right(gblo)
      reader.run(ReferenceData.empty).isLeft shouldBe true
      val evidence: RefDataReader[HolidayCalendar] =:=
        cats.data.Kleisli[FailureOr, ReferenceData, HolidayCalendar] = implicitly
      evidence(reader).run(refData) shouldBe Right(gblo)
    })

  private val witnessSurfaceRows: List[SurfaceRow] = List(
    row(
      WitnessSurface,
      "ReferenceDataType",
      "recognises a value by pattern, and is reached only through its factory") {
      // The witness an identifier carries so that reference data can check the value it found
      // against the identifier that asked for it. It is behaviour rather than data - it holds the
      // pattern that recognises a value - so it has no codec, and it is not a construction kind
      // either: the constructor is private, `of` is the only factory, and there is no `unapply`
      // because a caller has nothing to take out of it.
      val text: ReferenceDataType[String] =
        ReferenceDataType.of("Sample") { case sample: String => sample }
      text.name shouldBe "Sample"
      text.narrow("a value") shouldBe Some("a value")
      text.narrow(1) shouldBe None
      text shouldBe ReferenceDataType.of("Sample") { case sample: String => sample }
      ReferenceDataType.holidayCalendar.narrow(gblo) shouldBe Some(gblo)
      ReferenceDataType.holidayCalendar.narrow("GBLO") shouldBe None
      assertTypeError("""new ReferenceDataType[String]("Sample", value => Some(value.toString))""")
    })

  //-------------------------------------------------------------------------
  // The transcribed reference data, which is module-internal rather than published.
  //
  // Each index family is built from a table of rows transcribed from the configuration the Java
  // implementation loaded at run time. A row is the shape that transcription is written in, not a
  // value of the published API: a caller reads the family, whose members carry every column of
  // the row it was built from. So the tables and their row types are `private[basics]`, which is
  // the visibility `PriceIndexData` has always had and which the other four now share, and they
  // are outside the published surface altogether - neither a construction kind nor a codec
  // applies to them. Each row below records that decision against the family that carries the
  // data, which is what a caller reads instead.
  //-------------------------------------------------------------------------
  private val moduleInternalRows: List[SurfaceRow] = List(
    row(
      ModuleInternal,
      "IborIndexRow",
      "is module-internal data, published as the Ibor index family built from it") {
      val index: IborIndex = valueOf(IborIndex.parse("GBP-LIBOR-3M"))
      index.currency shouldBe gbp
      index.active shouldBe true
      index.dayCount shouldBe DayCounts.ACT_365F
      index.fixingCalendar shouldBe gbloId
      index.tenor shouldBe tenor
    },
    row(
      ModuleInternal,
      "OvernightIndexRow",
      "is module-internal data, published as the overnight index family built from it") {
      val index: OvernightIndex = valueOf(OvernightIndex.parse("GBP-SONIA"))
      index.currency shouldBe gbp
      index.active shouldBe true
      index.fixingCalendar shouldBe gbloId
      index.publicationDateOffset shouldBe 1
    },
    row(
      ModuleInternal,
      "PriceIndexRow",
      "is module-internal data, published as the price index family built from it") {
      val index: PriceIndex = valueOf(PriceIndex.parse("GB-HICP"))
      index.currency shouldBe gbp
      index.region shouldBe country
      index.active shouldBe true
      index.publicationFrequency shouldBe Frequency.P1M
    },
    row(
      ModuleInternal,
      "FxIndexRow",
      "is module-internal data, published as the FX index family built from it") {
      val index: FxIndex = valueOf(FxIndex.parse("EUR/GBP-ECB"))
      index.currencyPair shouldBe CurrencyPair.of(Currency.EUR, gbp)
      index.fixingCalendar shouldBe HolidayCalendarIds.EUTA
      index.maturityDateOffset.days shouldBe 2
    },
    row(
      ModuleInternal,
      "FloatingRateNameRow",
      "is module-internal data, published as the floating rate name family built from it") {
      gbpLiborName.externalName shouldBe "GBP-LIBOR"
      gbpLiborName.rateType shouldBe FloatingRateType.Ibor
      gbpLiborName.toIborIndex(tenor) shouldBe Right(iborIndex)
    })

  //-------------------------------------------------------------------------
  // The policy rows: the private constructors themselves, the unassignability of published
  // fields, and the copy safety of the two numeric wrappers.
  //
  // Hiding `apply` would buy nothing if the constructor were reachable, and the representation
  // the validated and normalising types use - `sealed abstract case class X private (...)`,
  // instantiated as `new X(...) {}` inside the companion - makes it easy to assume the
  // anonymous-subclass route is available to everyone. It is not, and the first row is the proof,
  // taken across every package that has such a type so that no one package can drift on its own.
  //
  // `DoubleArray` and `DoubleMatrix` wrap a primitive array, and the whole immutability of the
  // types rests on no caller ever holding a reference to it. The two members that would hand it
  // over - `ofUnsafe`, which wraps an array the caller keeps, and `toArrayUnsafe`, which returns
  // the array itself - are `private[collect]`, so they serve the module that needs them and
  // nobody else. The collect module's own specs cannot show that, being inside `collect`; this
  // module is outside it, so the assertions below are the proof, and they are followed by the
  // runtime half: that the public API really does copy in both directions.
  //-------------------------------------------------------------------------
  private val policyRows: List[SurfaceRow] = List(
    row(Policy, "PrivateConstructors", "close every validated and normalising type to direct instantiation") {
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
    },
    row(Policy, "ReadOnlyFields", "make every published field unassignable, whatever the type's kind") {
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
    },
    row(Policy, "DoubleArray.ofUnsafe", "and toArrayUnsafe are private[collect] and out of reach from this module") {
      doubleArray.get(0) shouldBe 1.0
      assertCompiles("""DoubleArray.of(1.0, 2.0, 3.0)""")
      assertCompiles("""DoubleArray.copyOf(Array(1.0, 2.0, 3.0))""")
      assertCompiles("""DoubleArray.tabulate(3)(index => index.toDouble)""")
      assertCompiles("""doubleArray.toArray""")
      assertTypeError("""DoubleArray.ofUnsafe(Array(1.0, 2.0, 3.0))""")
      assertTypeError("""doubleArray.toArrayUnsafe""")
    },
    row(Policy, "DoubleMatrix.ofUnsafe", "and toArrayUnsafe are private[collect] and out of reach from this module") {
      doubleMatrix.get(1, 1) shouldBe 4.0
      assertCompiles("""DoubleMatrix.of(2, 2, 1.0, 2.0, 3.0, 4.0)""")
      assertCompiles("""DoubleMatrix.copyOf(Array(Array(1.0, 2.0), Array(3.0, 4.0)))""")
      assertCompiles("""DoubleMatrix.tabulate(2, 2)((row, column) => (row + column).toDouble)""")
      assertCompiles("""doubleMatrix.toArray""")
      assertTypeError("""DoubleMatrix.ofUnsafe(Array(Array(1.0, 2.0), Array(3.0, 4.0)))""")
      assertTypeError("""doubleMatrix.toArrayUnsafe""")
    },
    row(Policy, "Decimal.MAX_SCALE", "is private[collect] and out of reach from this module") {
      decimal.scale shouldBe 2
      assertCompiles("""decimal.scale""")
      assertTypeError("""Decimal.MAX_SCALE""")
    },
    row(Policy, "DoubleArray.toArray", "hands back a copy, so mutating it cannot reach the array") {
      val exported = doubleArray.toArray
      exported(0) = 99.0
      exported(0) shouldBe 99.0
      doubleArray.get(0) shouldBe 1.0
      doubleArray.toArray.toList shouldBe List(1.0, 2.0, 3.0)
    },
    row(Policy, "DoubleArray factories", "copy their input, so mutating it afterwards cannot reach the array") {
      val source = Array(1.0, 2.0, 3.0)
      val copied = DoubleArray.copyOf(source)
      val fromSeq = DoubleArray.of(source(0), source(1), source(2))
      val tabulated = DoubleArray.tabulate(source.length)(index => source(index))
      source(0) = 99.0
      source(0) shouldBe 99.0
      copied.get(0) shouldBe 1.0
      fromSeq.get(0) shouldBe 1.0
      tabulated.get(0) shouldBe 1.0
    },
    row(Policy, "DoubleMatrix.toArray", "hands back a deep copy, so mutating it cannot reach the matrix") {
      val exported = doubleMatrix.toArray
      exported(0)(0) = 99.0
      exported(0)(0) shouldBe 99.0
      doubleMatrix.get(0, 0) shouldBe 1.0
      doubleMatrix.row(0).toList shouldBe List(1.0, 2.0)
    },
    row(Policy, "DoubleMatrix factories", "copy their input, so mutating it afterwards cannot reach the matrix") {
      val source = Array(Array(1.0, 2.0), Array(3.0, 4.0))
      val copied = DoubleMatrix.copyOf(source)
      val tabulated = DoubleMatrix.tabulate(2, 2)((row, column) => source(row)(column))
      source(0)(0) = 99.0
      source(0)(0) shouldBe 99.0
      copied.get(0, 0) shouldBe 1.0
      tabulated.get(0, 0) shouldBe 1.0
    },
    row(Policy, "ImmutableReferenceData.values", "is public, and an immutable Scala map a caller cannot alter") {
      val store: ImmutableReferenceData = ImmutableReferenceData.of(gbloId, gblo)

      // published, and it answers with what the store holds
      store.values(gbloId) shouldBe gblo

      // an immutable Scala map: the immutable ascription compiles, the mutable one does not, and
      // no member that would alter the store in place exists on what a caller is handed. Rule 10
      // also forbids a `java.util` type in a public signature, which the first line pins.
      //
      // The first three rejections are type errors - a mismatched ascription and a member that
      // does not exist - so they are asserted as such. The fourth is an assignment, whose
      // diagnostic depends on the shape of the target in the way the `ReadOnlyFields` row above
      // describes, so it takes the weaker assertion for the same reason that row does.
      assertCompiles(
        """val view: scala.collection.immutable.Map[ReferenceDataId[_], Any] = store.values""")
      assertTypeError(
        """val view: scala.collection.mutable.Map[ReferenceDataId[_], Any] = store.values""")
      assertTypeError("""store.values.put(gbloId, gblo)""")
      assertDoesNotCompile("""store.values(gbloId) = gblo""")

      // and it is a view, not a route in: what a caller does with the map it was given cannot
      // change the store, whose only construction path remains the entry-based factories
      val exported: Map[ReferenceDataId[_], Any] = store.values
      (exported - (gbloId: ReferenceDataId[_])) shouldBe Map.empty[ReferenceDataId[_], Any]
      store.values(gbloId) shouldBe gblo
      store.findValue(gbloId) shouldBe Some(gblo)
    })

  //-------------------------------------------------------------------------
  // The inventory itself, the tests derived from it, and the assertions that hold the two to
  // each other.
  //-------------------------------------------------------------------------
  /**
   * Every row of the audit, in the order the sections above declare them.
   *
   * This is the single source the tests of this suite are registered from, which is the point of
   * the structure: a subject is audited because it has a row, and it is counted as covered because
   * it has a row, so the two cannot disagree. The coverage assertions below compare the subjects
   * of this list against the kind lists the construction policy fixes, and the registration
   * assertion compares the names it produces against the names ScalaTest actually holds.
   */
  private val inventory: List[SurfaceRow] =
    validatedRows :::
      normalisingRows :::
      totalRows :::
      closedFamilyRows :::
      openContractRows :::
      functionSurfaceRows :::
      aliasSurfaceRows :::
      witnessSurfaceRows :::
      moduleInternalRows :::
      policyRows

  register(inventory)

  test("the construction-policy inventory is exactly the one the port's policy fixes") {
    // The expected sets are the policy's own kind lists, transcribed in the companion. A type
    // that loses its audit loses its row, and a row that is added for a type the policy does not
    // classify has nowhere to be counted, so either shows up here rather than in a review.
    subjectsOf(Validated) shouldBe ExpectedValidatedSubjects
    subjectsOf(Normalising) shouldBe ExpectedNormalisingSubjects
    subjectsOf(Total) shouldBe ExpectedTotalSubjects
  }

  test("the closed families, the open contracts and the remaining public surfaces are accounted for") {
    subjectsOf(ClosedFamily) shouldBe ExpectedClosedFamilySubjects
    subjectsOf(OpenContract) shouldBe ExpectedOpenContractSubjects
    subjectsOf(FunctionSurface) shouldBe ExpectedFunctionSurfaceSubjects
    subjectsOf(AliasSurface) shouldBe ExpectedAliasSurfaceSubjects
    subjectsOf(WitnessSurface) shouldBe ExpectedWitnessSurfaceSubjects
    subjectsOf(ModuleInternal) shouldBe ExpectedModuleInternalSubjects
    subjectsOf(Policy) shouldBe ExpectedPolicySubjects
  }

  test("every inventory row registers its own test, and every test of this suite comes from one") {
    // The two halves of the same property, and both are needed. The first says the inventory is
    // exercised: every row's name is a test ScalaTest holds. The second says the inventory is the
    // whole of the audit: the only tests not derived from a row are the three assertions of this
    // section, the sensitivity control and the source derivation, so an audit added outside the
    // inventory - which would then be counted by nothing - changes the total and fails here.
    inventory.map(entry => entry.testName).distinct should have size inventory.size.toLong
    inventory.map(entry => entry.testName).toSet.subsetOf(testNames) shouldBe true
    testNames should have size (inventory.size + StandaloneTests).toLong
  }

  test("the sealing proof is sensitive, because the same probe compiles over an open contract") {
    // What keeps every sealing assertion above honest. A trait probe is rejected by the `sealed`
    // modifier and by nothing else, which is only worth asserting if the compiler would otherwise
    // accept it - so here it is accepted, over every contract this module leaves open. If some
    // later change made a trait probe fail for an unrelated reason, this test would fail and the
    // sealing rows would stop being evidence of anything.
    assertCompiles("""{ trait Host extends ReferenceData; () }""")
    assertCompiles("""{ trait Host extends ReferenceDataId[HolidayCalendar]; () }""")
    assertCompiles("""{ trait Host extends FloatingRate; () }""")
    assertCompiles("""{ trait Host extends CalculationTarget; () }""")
    assertCompiles("""{ trait Host extends Resolvable[CalculationTarget]; () }""")
    assertCompiles("""{ trait Host extends DateAdjuster; () }""")
    assertCompiles("""{ trait Host extends FxRateProvider; () }""")
    assertCompiles("""{ trait Host extends FxConvertible[CurrencyAmount]; () }""")
  }

  test("the function, alias and witness surfaces are derived from the sources, not transcribed") {
    // The other coverage assertions of this file compare the inventory against sets transcribed
    // from the port's construction policy, which is the right check for a data type: the policy
    // names it, so a type that lost its audit is a type missing from the inventory.
    //
    // The surfaces below are the ones the policy does not enumerate - a callback trait nested in
    // a numeric wrapper's companion, a type alias a module root publishes, the witness an
    // identifier carries - so a transcribed set cannot report a new one. Here the expectation is
    // read out of the sources instead: every published `trait` of the two numeric wrappers, every
    // published `type` of the two module roots and of the floating-rate companion, and every
    // published class of the identifier source. Adding a sixth callback, a fifth alias or a
    // second witness therefore fails this test until it is classified and audited, which is what
    // the closure claim of the codec inventory rests on.
    val callbacks =
      publicDeclarations(
        sourceLines("strata-collect/src/main/scala/com/opengamma/strata/collect/array/DoubleArray.scala"),
        "trait").map(name => s"DoubleArray.$name") ++
        publicDeclarations(
          sourceLines("strata-collect/src/main/scala/com/opengamma/strata/collect/array/DoubleMatrix.scala"),
          "trait").map(name => s"DoubleMatrix.$name")
    val floatingRateAliases =
      publicDeclarations(
        sourceLines("strata-basics/src/main/scala/com/opengamma/strata/basics/index/FloatingRate.scala"),
        "type").map(name => s"FloatingRate.$name")
    withClue("every published callback and function type is audited as a function surface: ")(
      subjectsOf(FunctionSurface) shouldBe (callbacks ++ floatingRateAliases))

    val rootAliases =
      publicDeclarations(
        sourceLines("strata-collect/src/main/scala/com/opengamma/strata/collect/package.scala"),
        "type").map(name => s"collect.$name") ++
        publicDeclarations(
          sourceLines("strata-basics/src/main/scala/com/opengamma/strata/basics/package.scala"),
          "type").map(name => s"basics.$name")
    withClue("every alias the two module roots publish is audited as an alias surface: ")(
      subjectsOf(AliasSurface) shouldBe rootAliases)

    val witnesses =
      publicDeclarations(
        sourceLines("strata-basics/src/main/scala/com/opengamma/strata/basics/ReferenceDataId.scala"),
        "class")
    withClue("every published class of the identifier source is audited as a witness surface: ")(
      subjectsOf(WitnessSurface) shouldBe witnesses)

    // and the transcribed tables are read the same way: each is module-internal in the source
    // itself, so a row type that became published would be an unaudited public data type - which
    // is the form the omission originally took
    val tables =
      List(
        "IborIndexData" -> "IborIndexRow",
        "OvernightIndexData" -> "OvernightIndexRow",
        "PriceIndexData" -> "PriceIndexRow",
        "FxIndexData" -> "FxIndexRow",
        "FloatingRateNameData" -> "FloatingRateNameRow")
    subjectsOf(ModuleInternal) shouldBe tables.map { case (_, rowType) => rowType }.toSet
    tables.foreach {
      case (table, rowType) =>
        val lines =
          sourceLines(s"strata-basics/src/main/scala/com/opengamma/strata/basics/index/$table.scala")
        withClue(s"$table publishes no class of its own: ")(
          publicDeclarations(lines, "class") shouldBe Set.empty[String])
        withClue(s"$table publishes no object of its own: ")(
          publicDeclarations(lines, "object") shouldBe Set.empty[String])
        withClue(s"$rowType is declared module-internal: ")(
          lines.exists(line => line.startsWith(s"private[basics] final case class $rowType")) shouldBe true)
        withClue(s"$table is declared module-internal: ")(
          lines.exists(line => line.startsWith(s"private[basics] object $table")) shouldBe true)
    }
  }
}

/**
 * What [[ApiSurfaceSpec]] is built from: the vocabulary of its inventory, the kind lists the
 * inventory is held to, and the host implementations of this module's open contracts.
 *
 * The vocabulary is [[ApiSurfaceSpec.Kind]] and [[ApiSurfaceSpec.SurfaceRow]] - one kind of public
 * surface, and one row of the audit - followed by the expected subjects of each kind, transcribed
 * from the port's construction policy. Those sets are the '''expectation''': the suite's coverage
 * assertions compare the inventory it actually declares against them, so a subject that loses its
 * audit is reported rather than silently stopping being audited, and a row added for a subject the
 * policy does not classify has nowhere to be counted.
 *
 * The host implementations are the positive half of the open-contract rows. They live here rather
 * than inside the spec class for a reason the compiler insists on: a case class nested in a class
 * carries a reference to its enclosing instance, which makes the equality check it synthesises
 * unverifiable at run time and - under this build's fatal-warning setting - an error. Nested in
 * an object they have no such reference, and the equality of `HostTarget` and `HostConvertible`,
 * which two of the rows rely on, is the ordinary structural one.
 *
 * Every one of them is declared as ordinary code rather than inside a compile-assertion string, so
 * the compiler checks each of them at every build of this module: a change that sealed one of
 * these contracts, or altered the shape of a member, would stop this file from compiling.
 */
private object ApiSurfaceSpec {

  /**
   * The kind of public surface an inventory row is about.
   *
   * The first three are the construction kinds of the port's policy, which fix what a type's
   * constructor surface must look like. The rest are the other ways a public surface can be
   * classified: a closed family, an open contract, a function type, an alias, a witness, data
   * that is not published at all, or a policy that spans several types at once. Every row carries
   * exactly one of them, and the coverage assertions compare the rows of each kind against the
   * subjects that kind is expected to have.
   *
   * @param tag  how the kind is rendered at the start of a test name
   */
  sealed abstract class Kind(val tag: String)

  /** A validated type: its factory checks its inputs and reports what is wrong with them. */
  case object Validated extends Kind("[V]")

  /** A normalising type: its factory rewrites its inputs, and may reject them as well. */
  case object Normalising extends Kind("[N]")

  /** A total type: every well-typed input is accepted, so the case class surface is kept. */
  case object Total extends Kind("[T]")

  /** A sealed family, whose members exist only in their companion. */
  case object ClosedFamily extends Kind("sealed")

  /** A contract left open on purpose, because an application or another file implements it. */
  case object OpenContract extends Kind("open")

  /** A single-abstract-method callback or function type, which carries no data. */
  case object FunctionSurface extends Kind("function")

  /** A type alias a module root publishes. */
  case object AliasSurface extends Kind("alias")

  /** A witness carrying behaviour rather than data. */
  case object WitnessSurface extends Kind("witness")

  /** Transcribed reference data, visible within the module and not published. */
  case object ModuleInternal extends Kind("internal")

  /** A policy that spans several types, such as the copy safety of the numeric wrappers. */
  case object Policy extends Kind("policy")

  /**
   * One row of the audit: what it is about, what it claims, and the assertions that prove it.
   *
   * The audit is held as a function rather than run when the row is built, so that the inventory
   * can be assembled - and counted - before any of it executes.
   *
   * @param kind  the kind of surface the row is about
   * @param subject  the type, member or alias the row is about
   * @param claim  what the row asserts about the subject
   * @param audit  the assertions that prove the claim
   */
  final case class SurfaceRow(kind: Kind, subject: String, claim: String, audit: () => Any) {

    /**
     * The name of the test this row registers.
     *
     * @return the kind, the subject and the claim as one sentence
     */
    def testName: String = s"${kind.tag} $subject $claim"
  }

  /**
   * The validated types of the construction policy, transcribed from it.
   *
   * Twenty-three types, every one of which must publish neither `apply` nor `copy` and must still
   * destructure. The set is the expectation the inventory is held to, so a type that loses its
   * audit is reported here rather than quietly stopping being audited.
   */
  val ExpectedValidatedSubjects: Set[String] = Set(
    "StandardId",
    "Country",
    "FxRate",
    "FxMatrix",
    "CurrencyAmountArray",
    "MultiCurrencyAmountArray",
    "FixedScaleDecimal",
    "MarketTenor",
    "DaysAdjustment",
    "PeriodAdjustment",
    "TenorAdjustment",
    "AdjustableDates",
    "SchedulePeriod",
    "Schedule",
    "PeriodicSchedule",
    "ValueStep",
    "ValueSchedule",
    "ValueStepSequence",
    "HalfUp",
    "IborIndexObservation",
    "OvernightIndexObservation",
    "FxIndexObservation",
    "DayCount.Bus252")

  /** The normalising types of the construction policy, transcribed from it. */
  val ExpectedNormalisingSubjects: Set[String] = Set(
    "CurrencyAmount",
    "Money",
    "BigMoney",
    "MultiCurrencyAmount",
    "Tenor",
    "Frequency",
    "SequenceDate",
    "HolidayCalendarId",
    "Decimal",
    "ImmutableHolidayCalendar")

  /**
   * The total types of the construction policy, transcribed from it.
   *
   * The last two are the policy's own representation exception - a final class over a private
   * primitive array, with copy-safe total factories - and are audited as that rather than as case
   * classes.
   */
  val ExpectedTotalSubjects: Set[String] = Set(
    "CurrencyPair",
    "Payment",
    "AdjustablePayment",
    "AdjustableDate",
    "BusinessDayAdjustment",
    "ValueAdjustment",
    "ValueDerivatives",
    "CalculationTargetList",
    "PriceIndexObservation",
    "ReferenceData.Entry",
    "DoubleArray",
    "DoubleMatrix")

  /** The sealed families of both modules, each of which must be closed from outside its file. */
  val ExpectedClosedFamilySubjects: Set[String] = Set(
    "Index",
    "RateIndex",
    "FloatingRateIndex",
    "IborIndex",
    "OvernightIndex",
    "PriceIndex",
    "FxIndex",
    "HolidayCalendar",
    "DayCount",
    "Rounding",
    "Currency",
    "BusinessDayConvention",
    "RollConvention",
    "PeriodAdditionConvention",
    "DateSequence",
    "StubConvention",
    "FloatingRateType",
    "ValueAdjustmentType",
    "FloatingRateName",
    "Failure",
    "FailureReason",
    "IndexObservation")

  /**
   * The contracts this module leaves open.
   *
   * `IndexObservation` is deliberately absent: its four implementations share its file, so the
   * family is sealed, its row sits with the closed families and the sensitivity control below
   * carries one probe fewer than it once did.
   */
  val ExpectedOpenContractSubjects: Set[String] = Set(
    "ReferenceData",
    "ReferenceDataId",
    "FloatingRate",
    "CalculationTarget",
    "DateAdjuster",
    "FxRateProvider")

  /** The callback and function types the public API takes, none of which carries data. */
  val ExpectedFunctionSurfaceSubjects: Set[String] = Set(
    "DoubleArray.DoubleTernaryOperator",
    "DoubleMatrix.ElementAction",
    "DoubleMatrix.ElementFunction",
    "DoubleMatrix.RowArrayFunction",
    "DoubleMatrix.RowArrayObjectFunction",
    "FloatingRate.Lookup")

  /** The type aliases the two module roots publish. */
  val ExpectedAliasSurfaceSubjects: Set[String] = Set(
    "collect.FailureOr",
    "collect.ResultNec",
    "collect.ValidatedFailures",
    "collect.ValueWithFailures",
    "basics.RefDataReader")

  /** The witnesses the public API carries, which hold behaviour rather than data. */
  val ExpectedWitnessSurfaceSubjects: Set[String] = Set("ReferenceDataType")

  /** The transcribed reference data tables, which are `private[basics]` rather than published. */
  val ExpectedModuleInternalSubjects: Set[String] = Set(
    "IborIndexRow",
    "OvernightIndexRow",
    "PriceIndexRow",
    "FxIndexRow",
    "FloatingRateNameRow")

  /** The policies that span several types at once. */
  val ExpectedPolicySubjects: Set[String] = Set(
    "PrivateConstructors",
    "ReadOnlyFields",
    "DoubleArray.ofUnsafe",
    "DoubleMatrix.ofUnsafe",
    "Decimal.MAX_SCALE",
    "DoubleArray.toArray",
    "DoubleArray factories",
    "DoubleMatrix.toArray",
    "DoubleMatrix factories",
    "ImmutableReferenceData.values")

  /**
   * The number of tests this suite declares outside the inventory.
   *
   * The three coverage assertions, which compare the inventory against the sets above, the
   * sensitivity control for the sealing probes, and the derivation that reads the function,
   * alias and witness surfaces out of the module sources. Nothing else may be registered by
   * hand: the registration assertion compares this number plus the size of the inventory
   * against what ScalaTest holds, so an audit written outside a row fails it.
   */
  val StandaloneTests: Int = 5

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

  /**
   * A host's own identifier, which resolves through the default the trait supplies.
   *
   * It implements the one member the trait leaves abstract - the witness for the type of data
   * it refers to - and nothing else, which is the whole of what an application has to write
   * for reference data of a type this library already knows: the witness for a holiday
   * calendar is published by `ReferenceDataType`. A host naming data of its own type declares
   * a witness for it the same way, with `ReferenceDataType.of`.
   */
  final case class HostReferenceDataId(label: String) extends ReferenceDataId[HolidayCalendar] {
    override def valueType: ReferenceDataType[HolidayCalendar] = ReferenceDataType.holidayCalendar
  }

  /** A host's own floating rate, which is what `FloatingRateName` is from another file. */
  final class HostFloatingRate(val name: String, val floatingRateName: FloatingRateName)
      extends FloatingRate

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
