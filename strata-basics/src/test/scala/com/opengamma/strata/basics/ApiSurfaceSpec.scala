/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import java.io.ByteArrayOutputStream
import java.io.File
import java.io.ObjectOutputStream
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Modifier
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.time.DayOfWeek
import java.time.LocalDate
import java.time.Period
import java.time.YearMonth

import scala.util.Try
import scala.util.Using

import org.scalatest.Assertion
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
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.ValidatedFailures
import com.opengamma.strata.collect.ValueWithFailures
import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.array.DoubleMatrix
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason

/**
 * Holds the '''shape''' of this module's public API to its construction policy, by asserting what
 * no ordinary test can express: that a constructor does '''not''' exist, that a hierarchy cannot
 * be extended from outside the file that declares it, and that a member of a neighbouring module
 * is out of reach from here. A spec that never writes `CurrencyAmount(GBP, 100)` says nothing
 * about whether that expression would compile, and one that never declares a new `Index` says
 * nothing about whether the family is still closed, so the compiler is what these guarantees are
 * asserted against, through ScalaTest's `assertTypeError`, `assertDoesNotCompile` and
 * `assertCompiles`. The acceptance gate for explicit error handling runs this suite alongside
 * `SmartConstructorSpec`, `FailableSurfaceSpec` and `FailureSpec`.
 *
 * Every entry is a [[ApiSurfaceSpec.SurfaceRow]] declared once, carrying the kind of surface it
 * is, the subject, the claim and the audit that proves it; one test is registered per row, and
 * the coverage assertions at the end hold the inventory to the kind lists of the construction
 * policy, so deleting a subject's audit deletes its row and is reported. Each section below is
 * one kind, and says what its rows assert and why.
 *
 * ===How sealing is proved without proving something else instead===
 *
 * Most closed families also hide their constructor, so the obvious probe -
 * `final class Host extends Currency("XYZ", 2, "USD")` - is rejected for '''two''' reasons at
 * once, and would still be rejected if the family stopped being sealed. Sealing is therefore
 * proved by a '''trait''' probe: `trait Host extends Currency` calls no constructor, needs no
 * arguments and implements no member, which leaves the `sealed` modifier as the only thing the
 * compiler can object to; constructor privacy is asserted separately, by the class probe. The
 * control is the sensitivity test near the end, where the same trait probe '''compiles''' over
 * every contract this module leaves open - so a trait probe is not something the compiler
 * rejects out of hand.
 *
 * ===How the negatives are kept honest===
 *
 * A negative compile assertion can fail for the wrong reason - a typo, a missing import, an
 * argument of the wrong type - and pass while proving nothing. Three habits guard against that:
 * `assertTypeError` rather than `assertDoesNotCompile` wherever the expected failure is a type
 * error, because the weaker form is satisfied by a parse error too; arguments read off a live
 * instance's own accessors, so that the arity and the types of every `X(...)` snippet are right
 * by construction and the missing `apply` is the only thing left to object to; and an
 * `assertCompiles` of the nearest legal form - almost always the type's own factory over the same
 * arguments - beside every negative, so the difference between the two is the only possible
 * cause. The exception to the first is an assignment, whose diagnostic depends on the shape of
 * the target; the `ReadOnlyFields` row says why that group deliberately takes the weaker form.
 *
 * A value referenced '''only''' inside a snippet string counts as unused under the build's
 * `-Wunused` setting, with warnings as errors, and fails the build; every fixture here is
 * therefore also used in ordinary code.
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
   * Takes the value out of a factory result, naming the failure where there is no value: a
   * fixture broken by a change to a validation rule is a broken test, and says which rule
   * rejected it.
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
  // the live instances the rows below audit, each built through the factory its type publishes -
  // the only way in, for a type with no public constructor
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
   * Names the kind of an observation by narrowing to the implementations this module publishes.
   *
   * `IndexObservation` is an open contract, so the four implementations below are what the module
   * builds rather than every value the type admits, and the compiler cannot check a match over it
   * for exhaustiveness. The default branch is therefore required rather than optional, and it is
   * the branch a host's own observation reaches - which is what the open-contract row uses it to
   * show. It must not be removed in an attempt to make the match a closedness proof: the trait is
   * deliberately extensible, and a match without the branch does not compile here.
   *
   * @param observation  the observation to name the kind of
   * @return the name of its kind, or `other` for an implementation from outside this module
   */
  private def kindOfObservation(observation: IndexObservation): String = observation match {
    case _: IborIndexObservation => "Ibor"
    case _: OvernightIndexObservation => "Overnight"
    case _: PriceIndexObservation => "Price"
    case _: FxIndexObservation => "Fx"
    case _ => "other"
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
   * `ofBus252` answers with a `DayCount`, but a `copy` added to `DayCount.Bus252` would be
   * invisible through a witness typed as `DayCount`, so that row's negative assertion would pass
   * while the property it stands for had been broken. The pattern below types it, and fails the
   * fixture rather than the assertion if `ofBus252` ever answers with something else.
   */
  private val bus252Member: DayCount.Bus252 = bus252 match {
    case member: DayCount.Bus252 => member
    case other => fail(s"DayCount.ofBus252 answered with $other, which is not a DayCount.Bus252")
  }

  /**
   * Registers one test per inventory row, which is what makes a row and its audit the same thing.
   *
   * The name of each test is the row's own rendering, so a failure names the kind, the subject
   * and the claim.
   *
   * @param rows  the inventory rows to register
   */
  private def register(rows: List[SurfaceRow]): Unit =
    rows.foreach(entry => test(entry.testName)(entry.audit()))

  /**
   * Builds one inventory row, taking the audit by name so that it runs when its test runs rather
   * than while the inventory is being built.
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
   * It is identified by what it contains rather than by a path handed in, so the derivation below
   * works from any working directory a runner might choose.
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
   * A declaration counts as published when its line begins with the keyword, possibly behind the
   * modifiers that may precede it, and does '''not''' begin with `private` or `protected`.
   * Anchoring at the start of the line keeps documentation out, a Scaladoc line beginning with
   * `*` and a commented-out declaration with `/`.
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
  // The compiled classes of both modules, and the two properties asserted over all of them.
  //
  // Everything above this point is asserted against the compiler: a constructor that does not
  // exist, a hierarchy that cannot be extended, a member that is out of reach. None of that
  // survives into the class file. `sealed` leaves no trace in the bytecode of this language
  // version, a `private` or `private[pkg]` constructor is emitted public because the JVM has no
  // matching access level, and the compiler gives every case class a `java.io.Serializable`
  // supertype. A caller compiled by another language against the same class files therefore sees
  // an extensible hierarchy with reachable constructors, and `java.io.ObjectInputStream` sees a
  // read path into every product that does not go through a factory.
  //
  // The two policy rows at the end of the inventory close that gap and are asserted over the
  // compiled output rather than over a list of types, so a type added later is audited by the
  // same assertion that audits the ones written today.
  //-------------------------------------------------------------------------
  /**
   * The directories the build compiles the two modules' main sources into.
   *
   * Found under each module's `target`, by the `scala-<binary version>/classes` layout sbt uses,
   * so the audit does not have to name a compiler version. Both must be present: an audit that
   * quietly found no classes would assert nothing at all, which is the one way a sweep over
   * "every compiled class" can pass while being empty.
   */
  private lazy val classDirectories: List[File] = {
    val directories: List[File] =
      List("strata-collect", "strata-basics").flatMap { module =>
        val target: File = repositoryRoot.resolve(s"$module/target").toFile
        Option(target.listFiles())
          .fold(List.empty[File])(entries => entries.toList)
          .filter(entry => entry.isDirectory && entry.getName.startsWith("scala-"))
          .map(entry => new File(entry, "classes"))
          .filter(entry => entry.isDirectory)
      }
    withClue(
      "the JVM closure is audited over both modules' compiled output, which must be present: ")(
      directories.map(directory => directory.getPath) should have size 2)
    directories
  }

  /**
   * Every class file under a directory, at any depth.
   *
   * @param directory  the directory to walk
   * @return the class files it holds, including those of its subdirectories
   */
  private def classFiles(directory: File): List[File] =
    Option(directory.listFiles())
      .fold(List.empty[File])(entries => entries.toList)
      .flatMap { entry =>
        if (entry.isDirectory) { classFiles(entry) }
        else if (entry.getName.endsWith(".class")) { List(entry) }
        else { Nil }
      }

  /**
   * Every class the two modules' main sources compile to.
   *
   * Loaded without initialising, so that reading the shape of a family costs nothing and building
   * its instances is left to the fixtures. A class that cannot be loaded fails the audit rather
   * than being skipped, since a skipped class is one this file would claim to have audited.
   */
  private lazy val compiledClasses: List[Class[_]] = {
    val loaded: List[Either[String, Class[_]]] =
      classDirectories.flatMap { directory =>
        val root: Path = directory.toPath
        classFiles(directory).map { file =>
          val binaryName: String =
            root
              .relativize(file.toPath)
              .toString
              .stripSuffix(".class")
              .replace(File.separator, ".")
          Try(Class.forName(binaryName, false, getClass.getClassLoader)).toEither.left
            .map(cause => s"$binaryName could not be loaded: $cause")
        }
      }
    withClue("every compiled class of both modules must be loadable to be audited: ")(
      loaded.collect { case Left(failure) => failure } shouldBe empty)
    loaded.collect { case Right(loadedClass) => loadedClass }
  }

  /**
   * Whether a class carries the compiler's product encoding, which is what a case class and a
   * case object are given and what `java.io.ObjectInputStream` would otherwise populate field by
   * field.
   *
   * @param candidate  the class to test
   * @return true when the class is a product
   */
  private def isProduct(candidate: Class[_]): Boolean =
    classOf[Product].isAssignableFrom(candidate)

  /**
   * Whether a class refuses Java serialization, by carrying the refusal hooks of the two-way
   * blocker every value type of these modules mixes in.
   *
   * @param candidate  the class to test
   * @return true when the class refuses Java serialization
   */
  private def refusesSerialization(candidate: Class[_]): Boolean =
    classOf[NoJavaSerialization].isAssignableFrom(candidate)

  /**
   * Whether a class is one the compiler generated rather than one these sources declare.
   *
   * A singleton module, an anonymous class of a derivation or a partial function, and a lambda
   * carry no state of this library: a module deserializes to the singleton it already is, and the
   * others exist only to hold the machinery of a derived codec or a function literal. They are
   * `java.io.Serializable` because their supertypes are, and they are the only classes the sweep
   * of the second policy row below exempts - which is why the exemption is defined by the shape of
   * a compiler-generated name rather than by a list of classes that could grow quietly.
   *
   * @param candidate  the class to test
   * @return true when the class is compiler-generated
   */
  private def isCompilerGenerated(candidate: Class[_]): Boolean = {
    val name: String = candidate.getName
    name.endsWith("$") || name.contains("$anon") || name.contains("$$Lambda")
  }

  /**
   * Whether an implementation is declared inside the family it implements.
   *
   * The declaring class of a member class is the class it is nested in, and the implementations
   * of these modules are nested in the companion of the type they implement or in the companion
   * of one of that type's own ancestors - the second being the shape `DayCount.Bus252` has, whose
   * implementation is declared by `DayCount` because that is the family whose construction guard
   * admits it. A class nested in anything else, or in nothing at all, is one the family did not
   * declare.
   *
   * @param candidate  the implementation class
   * @param parent  the abstract class it extends
   * @return true when the implementation is declared inside the family
   */
  private def declaredInFamily(candidate: Class[_], parent: Class[_]): Boolean =
    Option(candidate.getDeclaringClass).exists { declaring =>
      // The type arguments are written out because `getSuperclass` answers with a class bounded
      // below by its own argument, which an inferred existential cannot thread through the fold.
      val ancestors: Iterator[Class[_]] =
        Iterator.unfold[Class[_], Option[Class[_]]](Option(parent)) { current =>
          current.map(ancestor => (ancestor, Option[Class[_]](ancestor.getSuperclass)))
        }
      ancestors.exists(ancestor => ancestor == declaring)
    }

  /**
   * Asserts that one value refuses Java serialization in both directions.
   *
   * The write path is the whole of `java.io.ObjectOutputStream.writeObject`, which consults the
   * value's `writeReplace` before it writes a single byte, so the refusal happens before anything
   * leaves the process. The read path cannot be exercised from a stream - there is no way to
   * produce one - so it is exercised through the hook `java.io.ObjectInputStream` would call on
   * the object it had just populated: `readResolve`, which refuses, so an instance reconstructed
   * around a forged stream never reaches the caller that asked for it.
   *
   * @param subject  how the value is described in a failure
   * @param value  the value that must refuse
   * @return the assertion that it refused in both directions
   */
  private def refusesJavaSerialization(subject: String, value: AnyRef): Assertion = {
    val refused: IllegalArgumentException =
      intercept[IllegalArgumentException] {
        Using.resource(new ObjectOutputStream(new ByteArrayOutputStream()))(stream =>
          stream.writeObject(value))
      }
    withClue(s"$subject refuses to be written by java.io.ObjectOutputStream: ")(
      refused.getMessage should include("Java serialization is not supported by this library"))

    val readHook: InvocationTargetException =
      intercept[InvocationTargetException](value.getClass.getMethod("readResolve").invoke(value))
    withClue(s"$subject raises rather than answers on the read hook: ")(
      readHook.getCause shouldBe an[IllegalArgumentException])
    withClue(s"$subject refuses the read hook java.io.ObjectInputStream would call: ")(
      readHook.getCause.getMessage should include(
        "Java serialization is not supported by this library"))
  }

  //-------------------------------------------------------------------------
  // The validated and normalising types of both modules.
  //
  // Each checks, rewrites, or both, what it is given, so a caller who could reach a constructor
  // or a `copy` could build a value the factory would have refused, or one that had skipped the
  // rewrite and would then compare unequal to the same value built properly. The representation
  // is `sealed abstract case class X private (...)`: `abstract` leaves `apply` and `copy`
  // ungenerated, `case` keeps `unapply`, and the three assertions per type are those three facts.
  // The `unapply` matters as much as the other two, since dropping `case` to lock the types down
  // further would cost pattern matching everywhere.
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
      // convention and appears in its name - and a validated type under the same policy, whose
      // constructor is visible to `DayCount` alone, so `ofBus252` is the only way in. The witness
      // is typed as the member rather than as the family, for the reason `bus252Member` gives.
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
      // The case parameter is the identifier, which is the whole of this type's equality - two
      // calendars claiming to be `GBLO` are one calendar - so `unapply` hands back exactly what
      // equality is defined on. The packed months the calendar answers from are `private[date]`
      // abstract members rather than case parameters, which is what keeps them out of reach: the
      // last two assertions are that they cannot be read from outside the `date` package, which
      // would not be true of a case parameter, since `unapply` would hand the array to anyone.
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
  // invariant. Every one is audited positively, so that a later tidy-up which "made every type
  // consistent" by locking one of them down fails here and is discussed rather than absorbed.
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
      // The entry pairs an identifier only with a value of its own type, which the compiler
      // checks, so it has nothing to check itself and keeps the whole case class surface, the
      // type parameter travelling through `copy`. The last assertion is that check, which is a
      // compile error rather than a validation failure.
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
      // The one representation exception, and the primitive array is the reason for it: a case
      // class over `Array[Double]` would hand the array out through `unapply` and `copy`, so this
      // is a final class whose factories copy instead. It is total in the sense that matters -
      // every well-typed input is accepted - and the copy safety that stands in for the case
      // surface is audited by the policy rows at the end, at run time as well as structurally.
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
  // Every subtype of each is declared in the family's own file, which is what lets a match over
  // the family be checked for exhaustiveness and makes the `values` a named family publishes the
  // whole truth about it. Scala 2 enforces sealing per file, so a subtype declared here does not
  // compile.
  //
  // Each row states that the family's own members are reachable, then proves the sealing with a
  // trait probe, then - where the family also hides its constructor - asserts that separately
  // with a class probe whose arguments are read off a published value. The scaladoc above says
  // why the two probes are separate assertions and neither stands in for the other.
  //-------------------------------------------------------------------------
  private val closedFamilyRows: List[SurfaceRow] = List(
    row(
      ClosedFamily,
      "Index",
      "is sealed against a subtype declared outside its file, against a class as well as a trait") {
      (iborIndex: Index).name shouldBe "GBP-LIBOR-3M"
      Index.parse("GBP-LIBOR-3M") shouldBe Right(iborIndex)
      assertTypeError("""{ trait Host extends Index; () }""")
      assertTypeError("""{ final class Host extends Index { def name: String = "Bespoke-Index" }; () }""")
    },
    row(
      ClosedFamily,
      "RateIndex",
      "is sealed against a subtype declared outside its file, against a class as well as a trait") {
      (overnightIndex: RateIndex).fixingCalendar shouldBe gbloId
      assertTypeError("""{ trait Host extends RateIndex; () }""")
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
      assertTypeError("""{ trait Host extends FloatingRateIndex; () }""")
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
      assertTypeError("""{ trait Host extends IborIndex; () }""")
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
      assertTypeError("""{ trait Host extends OvernightIndex; () }""")
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
      assertTypeError("""{ trait Host extends PriceIndex; () }""")
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
      assertTypeError("""{ trait Host extends FxIndex; () }""")
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
      assertTypeError("""{ trait Host extends HolidayCalendar; () }""")
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
      assertTypeError("""{ trait Host extends DayCount; () }""")
      assertTypeError("""{ final class Host extends DayCount("Bespoke-Day-Count"); () }""")
    },
    row(
      ClosedFamily,
      "Rounding",
      "is sealed against a subtype declared outside its file, against a class as well as a trait") {
      val rounding: Rounding = Rounding.of(gbp)
      rounding.round(1.2345) shouldBe 1.23
      assertTypeError("""{ trait Host extends Rounding; () }""")
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
      assertTypeError("""{ trait Host extends Currency; () }""")
      assertTypeError("""{ final class Host extends Currency("XYZ", 2, "USD"); () }""")
    },
    row(
      ClosedFamily,
      "BusinessDayConvention",
      "is sealed against a subtype declared outside its file, and hides its constructor") {
      modifiedFollowing.adjust(jan15, gblo) shouldBe jan15
      assertTypeError("""{ trait Host extends BusinessDayConvention; () }""")
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
      assertTypeError("""{ trait Host extends RollConvention; () }""")
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
      assertTypeError("""{ trait Host extends PeriodAdditionConvention; () }""")
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
      assertTypeError("""{ trait Host extends DateSequence; () }""")
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
      assertTypeError("""{ trait Host extends StubConvention; () }""")
      assertTypeError("""{ final class Host extends StubConvention("Bespoke-Stub-Convention"); () }""")
    },
    row(
      ClosedFamily,
      "FloatingRateType",
      "is sealed against a subtype declared outside its file, and hides its constructor") {
      FloatingRateType.Ibor.isIbor shouldBe true
      assertTypeError("""{ trait Host extends FloatingRateType; () }""")
      assertTypeError("""{ final class Host extends FloatingRateType("Bespoke-Floating-Rate-Type"); () }""")
    },
    row(
      ClosedFamily,
      "ValueAdjustmentType",
      "is sealed against a subtype declared outside its file, and hides its constructor") {
      ValueAdjustmentType.Replace.adjust(100.0, 200.0) shouldBe 200.0
      assertTypeError("""{ trait Host extends ValueAdjustmentType; () }""")
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
      assertTypeError("""{ trait Host extends FloatingRateName; () }""")
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
      assertTypeError("""{ trait Host extends Failure; () }""")
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
      "FailureReason",
      "is sealed against a subtype declared outside its file, and hides its constructor") {
      FailureReason.MISSING_DATA.name shouldBe "MISSING_DATA"
      assertTypeError("""{ trait Host extends FailureReason; () }""")
      assertTypeError("""{ final class Host extends FailureReason("BESPOKE"); () }""")
    })

  //-------------------------------------------------------------------------
  // The contracts that are deliberately open.
  //
  // Sealing is not free, and several traits of this module pay a price that is too high. An
  // application supplies its own holidays by implementing `ReferenceData`, and names data of its
  // own by implementing `ReferenceDataId`, as this module's own `HolidaySafeReferenceData` does,
  // so sealing either would make both impossible. `FloatingRate` is implemented by
  // `FloatingRateName` as well as by the index families, and sealing it would drag
  // `FloatingRateName` and its four hundred rows of data into `Index.scala`. `IndexObservation`
  // pays the same price for the same reason: the ported interface is a plain Java interface an
  // application may implement, and sealing it would drag all four observations into its file
  // instead of leaving each in its own.
  //
  // Being implementable is the property asserted, and every host implementation lives in the
  // companion as ordinary code rather than inside a compile-assertion string, which puts the
  // compiler's check on it at every build.
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
    row(OpenContract, "IndexObservation", "is open, so a host can observe an indicator of its own") {
      // The positive half: an implementation declared outside this module - in the companion of
      // this suite, which is outside `IndexObservation.scala` - satisfies the contract, is carried
      // by a signature written in terms of the trait, and reaches the default branch of
      // `kindOfObservation`, which is the branch an open contract obliges a caller to write.
      val host: IndexObservation = HostIndexObservation(iborIndex)
      host.index shouldBe iborIndex
      kindOfObservation(host) shouldBe "other"
      // and the same declaration compiles as written, so the openness is a property of the trait
      // rather than of the one implementation above.
      assertCompiles(
        """{
           |  final class Anonymous extends IndexObservation { def index: Index = iborIndex }
           |  ()
           |}""".stripMargin)
      // The four implementations this module publishes, each narrowing to its own kind and
      // reporting the index it observes - which is the whole of what the trait declares.
      val observations: List[IndexObservation] =
        List(iborObservation, overnightObservation, priceObservation, fxObservation)
      observations.map(kindOfObservation) shouldBe List("Ibor", "Overnight", "Price", "Fx")
      observations.map(_.index) shouldBe List(iborIndex, overnightIndex, priceIndex, fxIndex)
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
  // None of these is data: five are the single-abstract-method callbacks the numeric wrappers
  // take instead of boxing an index, one is the lookup function type `FloatingRate` composes,
  // five are the type aliases the two module roots re-export, and one is the witness reference
  // data narrows a lookup with. Recording them here is what makes this inventory account for
  // every public surface of both modules rather than for the data types alone.
  //-------------------------------------------------------------------------
  private val functionSurfaceRows: List[SurfaceRow] = List(
    row(
      FunctionSurface,
      "DoubleArray.DoubleTernaryOperator",
      "is a callback a lambda satisfies, and carries no data to serialize") {
      // A single-abstract-method trait, so a lambda is converted to it and the call site reads as
      // a three-argument function, while the compiled method passes `double` rather than a boxed
      // argument. A caller that wants to retain an operator implements it explicitly.
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
      // The callback `forEach` takes, applied in row-major order, which the cells below record;
      // the assignment keeps the lambda's result `Unit`, as the callback's own method is.
      val cells: Array[String] = Array.fill(4)("")
      doubleMatrix.forEach((rowIndex, columnIndex, value) =>
        cells(rowIndex * 2 + columnIndex) = s"$rowIndex$columnIndex=$value")
      cells.toList shouldBe List("00=1.0", "01=2.0", "10=3.0", "11=4.0")
    },
    row(
      FunctionSurface,
      "DoubleMatrix.ElementFunction",
      "is a callback a lambda satisfies, and carries no data to serialize") {
      doubleMatrix.mapWithIndex((rowIndex, columnIndex, value) =>
        value + rowIndex + columnIndex) shouldBe DoubleMatrix.of(2, 2, 1.0, 3.0, 4.0, 6.0)
    },
    row(
      FunctionSurface,
      "DoubleMatrix.RowArrayFunction",
      "is a callback a lambda satisfies, and carries no data to serialize") {
      DoubleMatrix.ofArrays(2, 2)(rowIndex =>
        Array(rowIndex * 2.0 + 1.0, rowIndex * 2.0 + 2.0)) shouldBe doubleMatrix
    },
    row(
      FunctionSurface,
      "DoubleMatrix.RowArrayObjectFunction",
      "is a callback a lambda satisfies, and carries no data to serialize") {
      DoubleMatrix.ofArrayObjects(2, 2)(rowIndex =>
        DoubleArray.of(rowIndex * 2.0 + 1.0, rowIndex * 2.0 + 2.0)) shouldBe doubleMatrix
    },
    row(
      FunctionSurface,
      "FloatingRate.Lookup",
      "is the function type a family's probe takes, and carries no data to serialize") {
      // One family's alias-aware lookup by name, widened to the floating rate it answers with. It
      // is a function type, so a host composes a search of its own from ordinary lambdas; the
      // standard composition is the four families of this module in their documented order.
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
      // package defines, so a caller importing the root needs no second import. Each row proves
      // the two names denote one type rather than two that could drift apart, and applying the
      // compile-time evidence is what uses it.
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
      // against the identifier that asked for it. It holds the pattern that recognises a value,
      // so it is behaviour rather than data and no construction kind applies: the constructor is
      // private, `of` is the only factory, and a caller has nothing to take out of it.
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
  // Each index family is built from a table of rows. A row is the shape the transcription is
  // written in, not a value of the published API: a caller reads the family, whose members carry
  // every column of the row it was built from. The tables and their row types are all
  // `private[basics]`, so neither a construction kind nor a codec applies to them; each row below
  // records that against the published family that carries the data.
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
  // Hiding `apply` would buy nothing if the constructor were reachable, and the `new X(...) {}`
  // the validated and normalising types use inside their companions makes it easy to assume the
  // anonymous-subclass route is open to everyone. It is not, and the first row is the proof, over
  // ten types spanning seven packages of the two modules.
  //
  // `DoubleArray` and `DoubleMatrix` wrap a primitive array, and the whole immutability of the
  // types rests on no caller ever holding a reference to it. The two members of the Java original
  // that would have handed it over - `ofUnsafe`, which wrapped an array the caller kept, and
  // `toArrayUnsafe`, which returned the array itself - are not ported under any name: a module
  // restriction would have held in the source only, leaving both public in the compiled class, so
  // the copy is made by the one constructor every factory passes through instead. The rows below
  // are what this module can prove about that from outside `collect`: neither name resolves, no
  // public member of either compiled class answers with the storage it holds, and the public API
  // really does copy in both directions.
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
      // Assignments are asserted with `assertDoesNotCompile` rather than `assertTypeError` - here
      // and in the `ImmutableReferenceData.values` row, the only other one - and deliberately:
      // the compiler's objection to an assignment depends on the shape of the target, a missing
      // `_=` member for an accessor and a non-assignable expression for an application, and the
      // weaker assertion accepts either, where the stricter one would tie the test to whichever
      // diagnostic this compiler version happens to produce. Accessors are read back first, so a
      // failure means the field stopped being read-only rather than that it stopped existing.
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
    row(Policy, "DoubleArray.ofUnsafe", "and toArrayUnsafe are not ported, so neither name resolves here") {
      doubleArray.get(0) shouldBe 1.0
      assertCompiles("""DoubleArray.of(1.0, 2.0, 3.0)""")
      assertCompiles("""DoubleArray.copyOf(Array(1.0, 2.0, 3.0))""")
      assertCompiles("""DoubleArray.tabulate(3)(index => index.toDouble)""")
      assertCompiles("""doubleArray.toArray""")
      assertTypeError("""DoubleArray.ofUnsafe(Array(1.0, 2.0, 3.0))""")
      assertTypeError("""doubleArray.toArrayUnsafe""")
    },
    row(Policy, "DoubleMatrix.ofUnsafe", "and toArrayUnsafe are not ported, so neither name resolves here") {
      doubleMatrix.get(1, 1) shouldBe 4.0
      assertCompiles("""DoubleMatrix.of(2, 2, 1.0, 2.0, 3.0, 4.0)""")
      assertCompiles("""DoubleMatrix.copyOf(Array(Array(1.0, 2.0), Array(3.0, 4.0)))""")
      assertCompiles("""DoubleMatrix.tabulate(2, 2)((row, column) => (row + column).toDouble)""")
      assertCompiles("""doubleMatrix.toArray""")
      assertTypeError("""DoubleMatrix.ofUnsafe(Array(Array(1.0, 2.0), Array(3.0, 4.0)))""")
      assertTypeError("""doubleMatrix.toArrayUnsafe""")
    },
    row(
      Policy,
      "UnsafeArrayHooks",
      "are absent from the compiled numeric classes, whose only route to an array copies") {
      // The two rows above ask the compiler, which answers about names. This one asks the
      // compiled classes, which is the level the guarantee actually holds at: a member restricted
      // to `collect` would be rejected by the compiler here and would still be a public method
      // callable from bytecode, so a name that does not resolve is necessary and not sufficient.
      //
      // Of every public member of the four classes - the two types and their companions - the
      // ones that answer with a primitive array are named here, and each is a copier: the three
      // accessors, which copy out of a value, and the matrix's compiler-named deep copy, which
      // copies the argument it is handed and reads no field of any instance. Anything else
      // appearing in this list would be a route to the storage, whatever it was called.
      val members =
        List(
          classOf[DoubleArray].getDeclaredMethods.toList,
          DoubleArray.getClass.getDeclaredMethods.toList,
          classOf[DoubleMatrix].getDeclaredMethods.toList,
          DoubleMatrix.getClass.getDeclaredMethods.toList).flatten

      members.map(member => member.getName).filter(name => name.contains("Unsafe")) shouldBe empty

      val arrayReturns =
        members
          .filter(member => Modifier.isPublic(member.getModifiers))
          .filter(member =>
            member.getReturnType == classOf[Array[Double]] ||
              member.getReturnType == classOf[Array[Array[Double]]])
          .map(member => member.getName)
          .distinct
          .sorted
      withClue(s"public members answering with a primitive array: $arrayReturns: ") {
        arrayReturns shouldBe
          List("columnArray", "com$opengamma$strata$collect$array$DoubleMatrix$$deepClone",
            "rowArray", "toArray")
      }

      // and the copying is observable from here, through the one construction path bytecode can
      // reach: the constructor of each type, invoked with an array this module still holds.
      //
      // Each constructor also takes the operation it is constructing for - the two types produce
      // the storage they keep inside the constructor and rewrite it there, which is what keeps an
      // element-wise operation to one allocation - and that operation's type is restricted to the
      // package the numeric types live in, so this module cannot name it. It is fetched from the
      // companion by reflection instead, which is exactly how a caller compiled against these
      // classes would have to reach it, and is therefore the sharper form of this probe: the copy
      // below is made for a caller that has no source-level access to any of this.
      val arrayRoute = DoubleArray.getClass.getMethod("NoRewrite").invoke(DoubleArray)
      val arraySource = Array(1.0, 2.0, 3.0)
      val builtArray = classOf[DoubleArray].getConstructors.head
        .newInstance(arraySource.asInstanceOf[AnyRef], arrayRoute)
        .asInstanceOf[DoubleArray]
      arraySource(0) = 99.0
      builtArray.get(0) shouldBe 1.0

      val matrixRoute = DoubleMatrix.getClass.getMethod("NoRewrite").invoke(DoubleMatrix)
      val rowSource = Array(Array(1.0, 2.0), Array(3.0, 4.0))
      val builtMatrix = classOf[DoubleMatrix].getConstructors.head
        .newInstance(
          rowSource.asInstanceOf[AnyRef],
          Integer.valueOf(2),
          Integer.valueOf(2),
          matrixRoute)
        .asInstanceOf[DoubleMatrix]
      rowSource(0)(0) = 99.0
      rowSource(1) = Array(99.0, 99.0)
      builtMatrix.get(0, 0) shouldBe 1.0
      builtMatrix.get(1, 1) shouldBe 4.0
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

      store.values(gbloId) shouldBe gblo

      // an immutable Scala map, which the first ascription pins: the immutable form compiles, the
      // mutable one does not, and no member that would alter the store in place exists on what a
      // caller is handed. The two type errors - a mismatched ascription and a member that does
      // not exist - are asserted as such; the third rejection is an assignment, so it takes the
      // weaker assertion for the reason `ReadOnlyFields` gives.
      assertCompiles(
        """val view: scala.collection.immutable.Map[ReferenceDataId[_], Any] = store.values""")
      assertTypeError(
        """val view: scala.collection.mutable.Map[ReferenceDataId[_], Any] = store.values""")
      assertTypeError("""store.values.put(gbloId, gblo)""")
      assertDoesNotCompile("""store.values(gbloId) = gblo""")

      // and it is a view, not a route in: what a caller does with the map it was given cannot
      // change the store, whose only construction path remains its own factories
      val exported: Map[ReferenceDataId[_], Any] = store.values
      (exported - (gbloId: ReferenceDataId[_])) shouldBe Map.empty[ReferenceDataId[_], Any]
      store.values(gbloId) shouldBe gblo
      store.findValue(gbloId) shouldBe Some(gblo)
    },
    row(
      Policy,
      "JvmConstructionClosure",
      "keeps every implementation of a closed type inside that type at run time, not only in the source") {
      // The first half: every validated and normalising value in this file is an instance of the
      // one implementation its companion declares, and that class is private and final in the
      // class file - so a caller compiled by another language against these class files cannot
      // name it, cannot extend it, and has nothing else to extend. The names are read off the
      // values the fixtures built through the factories, so a type whose factory started
      // answering with something else fails here rather than silently reopening.
      val published: List[(String, AnyRef)] = List(
        "StandardId" -> standardId,
        "Country" -> country,
        "Decimal" -> decimal,
        "FixedScaleDecimal" -> fixedScaleDecimal,
        "CurrencyAmount" -> currencyAmount,
        "CurrencyAmountArray" -> currencyAmountArray,
        "MultiCurrencyAmount" -> multiCurrencyAmount,
        "MultiCurrencyAmountArray" -> multiCurrencyAmountArray,
        "Money" -> money,
        "BigMoney" -> bigMoney,
        "FxRate" -> gbpUsdRate,
        "FxMatrix" -> fxMatrix,
        "Tenor" -> tenor,
        "MarketTenor" -> marketTenor,
        "HolidayCalendarId" -> compositeCalendarId,
        "ImmutableHolidayCalendar" -> immutableCalendar,
        "DayCount.Bus252" -> bus252Member,
        "SequenceDate" -> sequenceDate,
        "AdjustableDates" -> adjustableDates,
        "DaysAdjustment" -> daysAdjustment,
        "PeriodAdjustment" -> periodAdjustment,
        "TenorAdjustment" -> tenorAdjustment,
        "IborIndexObservation" -> iborObservation,
        "OvernightIndexObservation" -> overnightObservation,
        "FxIndexObservation" -> fxObservation,
        "Frequency" -> frequency,
        "SchedulePeriod" -> schedulePeriod,
        "Schedule" -> schedule,
        "PeriodicSchedule" -> periodicSchedule,
        "HalfUp" -> halfUp,
        "ValueStep" -> valueStep,
        "ValueStepSequence" -> valueStepSequence,
        "ValueSchedule" -> valueSchedule)
      withClue("every validated and normalising type of both modules is audited here: ")(
        published should have size 33)
      published.foreach {
        case (subject, value) =>
          val implementation: Class[_] = value.getClass
          withClue(s"$subject is built as the hidden implementation its family declares: ") {
            Modifier.isPrivate(implementation.getModifiers) shouldBe true
            Modifier.isFinal(implementation.getModifiers) shouldBe true
            declaredInFamily(implementation, implementation.getSuperclass) shouldBe true
          }
      }

      // The second half, and the exhaustive one: over every class the two modules compile to,
      // every subclass of an abstract class of theirs is closed, in one of the three ways a class
      // file can be closed. It is abstract, so no instance of it exists; or it is a singleton; or
      // it is a named final class, so nothing compiled elsewhere can extend it. What this reports
      // is therefore a subclass something outside these modules could extend, and an anonymous
      // implementation - the `new X(...) {}` form the validated types were first written with,
      // which the compiler publishes as a class no declaration names and whose constructor is
      // public - wherever either is reintroduced.
      //
      // Two of the three allowances are worth stating, because each is a shape that only became
      // legitimate once the hierarchies were closed properly. An abstract candidate passes
      // without being nested inside its parent: no instance of it can exist, and the concrete
      // classes that do extend it are swept by this same pass, which is what closes the
      // hierarchy. It is also the only shape a multi-level closed hierarchy can take - the index
      // families are the case in point, `RateIndex` extending `FloatingRateIndex` extending
      // `Index`, each a top-level abstract class so that a class file compiled elsewhere cannot
      // claim the type without running a constructor this library controls, and a top-level class
      // has no declaring class to be nested in. A named final candidate passes likewise, because
      // a total type is published as itself: `PriceIndexObservation` is a `final case class` with
      // a public `apply` by AAP 0.3.3, since an index paired with a month cannot be inconsistent,
      // and it is closed against being extended all the same.
      //
      // The stronger claim - that the implementation of a validated or normalising type is nested
      // inside that type and private - is not weakened by any of this: it is asserted above, over
      // all 33 of them, one live value at a time.
      val openImplementations: List[String] =
        compiledClasses.flatMap { candidate =>
          Option(candidate.getSuperclass)
            .filter(parent =>
              parent.getName.startsWith("com.opengamma.strata.") &&
                Modifier.isAbstract(parent.getModifiers))
            .flatMap { parent =>
              val named: Boolean =
                !candidate.isAnonymousClass && !candidate.isSynthetic &&
                  !candidate.getName.contains("$anon$")
              val closedShape: Boolean =
                Modifier.isAbstract(candidate.getModifiers) ||
                  candidate.getName.endsWith("$") ||
                  (named && Modifier.isFinal(candidate.getModifiers))
              if (closedShape) { None }
              else { Some(s"${candidate.getName} extends ${parent.getName}") }
            }
        }
      withClue("the sweep must find implementations to audit, or it asserts nothing: ")(
        compiledClasses.size should be >= 300)
      withClue("every implementation of an abstract type of these modules is closed: ")(
        openImplementations shouldBe empty)

      // The closure of the three levels that carry no data and were traits until the class file
      // was read rather than the source: a trait compiles to a plain JVM interface, which a class
      // compiled elsewhere may implement without running any constructor of this library, so a
      // match the compiler proved exhaustive could meet a case that does not exist in the source.
      // All three are abstract classes now, and the root refuses a subtype outside the families it
      // admits. `IndexObservation` is deliberately not among them - it is an open contract, which
      // the open-contract row above audits as one, and its four leaf types carry their own closure.
      withClue("the root of the index hierarchy is a class, so claiming it runs a constructor: ")(
        classOf[Index].isInterface shouldBe false)
      withClue("the intermediates of the index hierarchy are classes for the same reason: ") {
        classOf[RateIndex].isInterface shouldBe false
        classOf[FloatingRateIndex].isInterface shouldBe false
      }

      // And the guard itself, which is what makes the two halves above hold at run time rather
      // than only in the class file: it admits the implementation a family publishes and refuses
      // every other class, so a subtype compiled elsewhere cannot finish construction.
      JvmClosure.requireSoleImplementation(currencyAmount, currencyAmount.getClass)
      JvmClosure.requireDeclaredMember(DayCounts.ACT_360, classOf[DayCount])
      JvmClosure.requireDeclaredMember(gbp, classOf[Currency])
      JvmClosure.requireDeclaredMember(iborIndex, classOf[IborIndex])
      val foreignImplementation: IllegalArgumentException =
        intercept[IllegalArgumentException](
          JvmClosure.requireSoleImplementation(currencyAmount, classOf[FxRate]))
      foreignImplementation.getMessage should include("admits only the implementation it publishes")
      val foreignMember: IllegalArgumentException =
        intercept[IllegalArgumentException](
          JvmClosure.requireDeclaredMember(new AnyRef, classOf[Currency]))
      foreignMember.getMessage should include("is a closed family")
      foreignMember.getMessage should include("is not one of its published members")

      // The subtype guard of a closed root, which is what the two roots above run: it admits a
      // value of a permitted family and refuses one of any other class, so a class file that
      // claims the root type cannot finish construction.
      JvmClosure.requirePermittedSubtype(iborIndex, classOf[IborIndex], classOf[FxIndex])
      val foreignSubtype: IllegalArgumentException =
        intercept[IllegalArgumentException](
          JvmClosure.requirePermittedSubtype(new AnyRef, classOf[IborIndex], classOf[FxIndex]))
      foreignSubtype.getMessage should include("is not one of the subtypes this hierarchy admits")

      // And the last of the four, which is the one that closes the gap the other three cannot.
      // A hidden implementation is private in the source and in the `InnerClasses` metadata that
      // `javac` reads, but the class and its constructor are both `ACC_PUBLIC` in the class file,
      // so a class file emitted without a Scala or Java compiler can call that constructor and
      // present arguments no factory would have accepted. Its runtime class is then exactly the
      // one the identity guard admits, which is why identity alone is not closure: what refuses
      // such a value is each type restating its own invariant over the fields it holds.
      // (Reflection reports the `InnerClasses` access flag for the class itself, which is the
      // `private` the assertion above reads and the one `javac` honours; the constructor's own
      // access flag is the entry point, and it is public.)
      withClue("the implementation constructors are public in the class file, which is why the " +
        "invariant and not the identity is what closes them: ")(
        currencyAmount.getClass.getDeclaredConstructors.exists(constructor =>
          Modifier.isPublic(constructor.getModifiers)) shouldBe true)
      JvmClosure.requireInvariant("a satisfied condition holds", condition = true)
      val brokenInvariant: IllegalArgumentException =
        intercept[IllegalArgumentException](
          JvmClosure.requireInvariant("its amount is a number", condition = false))
      brokenInvariant.getMessage should include("a value of this type requires that")
      brokenInvariant.getMessage should include("its amount is a number")
    },
    row(
      Policy,
      "JavaSerialization",
      "is refused by every product of both modules, on the write path and on the read path") {
      // The exhaustive half. The compiler gives every case class and every case object a
      // `java.io.Serializable` supertype, which is a second construction path into each of them:
      // `java.io.ObjectInputStream` populates the fields of a product from a stream without
      // consulting the factory that validated them. Every product of both modules therefore
      // carries the refusal, and this is asserted over the compiled classes so that a type added
      // later is covered by the same assertion.
      val products: List[Class[_]] = compiledClasses.filter(isProduct)
      withClue("both modules publish products, or this audit would assert nothing: ")(
        products.size should be >= 150)
      withClue("every compiled product of both modules refuses Java serialization: ")(
        products.filterNot(refusesSerialization).map(candidate => candidate.getName) shouldBe empty)

      // and the refusal is not overridable, which is the difference between a refusal and a
      // convention. `writeReplace` and `readResolve` are the two hooks the JDK consults, and a
      // subclass that overrode them - returning itself rather than refusing - would be written
      // and read normally, its fields populated by the stream. Both are declared `final` in
      // `NoJavaSerialization`, so the compiler emits them `ACC_FINAL` on every class that mixes
      // it in, and a class file that declares an override is rejected when it is loaded, with
      // `IncompatibleClassChangeError`, before any stream is read. That is asserted here over the
      // emitted methods of every product rather than over the source that declares them.
      val overridableHooks: List[String] =
        products.flatMap { product =>
          List("writeReplace", "readResolve").flatMap { hook =>
            Try(product.getMethod(hook)).toOption
              .filterNot(method => Modifier.isFinal(method.getModifiers))
              .map(method => s"${product.getName}.${method.getName}")
          }
        }
      withClue("neither serialization hook can be overridden by a subclass of a product: ")(
        overridableHooks shouldBe empty)
      withClue("the hooks are present to be final, or the assertion above is vacuous: ") {
        Modifier.isFinal(currencyAmount.getClass.getMethod("writeReplace").getModifiers) shouldBe true
        Modifier.isFinal(currencyAmount.getClass.getMethod("readResolve").getModifiers) shouldBe true
      }

      // and nothing else that takes part in Java serialization holds data of this library: what
      // remains is the compiler's own encoding, which `isCompilerGenerated` describes
      val residue: List[String] =
        compiledClasses
          .filter(candidate =>
            classOf[java.io.Serializable].isAssignableFrom(candidate) &&
              !refusesSerialization(candidate) &&
              !isProduct(candidate))
          .filterNot(isCompilerGenerated)
          .map(candidate => candidate.getName)
      withClue("no type of either module takes part in Java serialization: ")(residue shouldBe empty)

      // The behavioural half: one value of every shape these modules publish, refused on the way
      // out and refused by the hook that would hand a forged instance back to a caller.
      refusesJavaSerialization("a validated value", currencyAmount)
      refusesJavaSerialization("a normalised value", tenor)
      refusesJavaSerialization("a total case class", payment)
      refusesJavaSerialization("a case object member of a closed family", DayCounts.ACT_360)
      refusesJavaSerialization("a family member built from a data table", gbp)
      refusesJavaSerialization("an index", iborIndex)
      refusesJavaSerialization("a floating rate name", gbpLiborName)
      refusesJavaSerialization("an index observation", iborObservation)
      refusesJavaSerialization("a holiday calendar", immutableCalendar)
      refusesJavaSerialization("a member of a sealed sum", Rounding.none)
      refusesJavaSerialization("a failure", Failure.Invalid("refused"))
      refusesJavaSerialization("a failure reason", FailureReason.INVALID)
      refusesJavaSerialization("a schedule", schedule)
    })

  //-------------------------------------------------------------------------
  // The inventory itself, the tests derived from it, and the assertions that hold the two to
  // each other.
  //-------------------------------------------------------------------------
  /** Every row of the audit, in the order the sections above declare them. */
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
    // A type that loses its audit loses its row, and a row added for a type the construction
    // policy does not classify has nowhere to be counted, so either shows up here.
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
    // Both halves are needed. The first says the inventory is exercised: every row's name is a
    // test ScalaTest holds. The second says the inventory is the whole of the audit: the only
    // tests not derived from a row are the three of this section, the sensitivity control and the
    // source derivation, so an audit added outside the inventory changes the total and fails.
    inventory.map(entry => entry.testName).distinct should have size inventory.size.toLong
    inventory.map(entry => entry.testName).toSet.subsetOf(testNames) shouldBe true
    testNames should have size (inventory.size + StandaloneTests).toLong
  }

  test("the sealing proof is sensitive, because the same probe compiles over an open contract") {
    // What keeps every sealing assertion above honest. A trait probe is rejected by the `sealed`
    // modifier and by nothing else, which is only worth asserting if the compiler would otherwise
    // accept it - so here it is accepted, over every contract this module leaves open. A later
    // change that made a trait probe fail for an unrelated reason fails this test rather than
    // leaving the sealing rows as evidence of nothing.
    assertCompiles("""{ trait Host extends ReferenceData; () }""")
    assertCompiles("""{ trait Host extends ReferenceDataId[HolidayCalendar]; () }""")
    assertCompiles("""{ trait Host extends IndexObservation; () }""")
    assertCompiles("""{ trait Host extends FloatingRate; () }""")
    assertCompiles("""{ trait Host extends CalculationTarget; () }""")
    assertCompiles("""{ trait Host extends Resolvable[CalculationTarget]; () }""")
    assertCompiles("""{ trait Host extends DateAdjuster; () }""")
    assertCompiles("""{ trait Host extends FxRateProvider; () }""")
    assertCompiles("""{ trait Host extends FxConvertible[CurrencyAmount]; () }""")
  }

  test("the function, alias and witness surfaces are derived from the sources, not transcribed") {
    // A set transcribed from the construction policy cannot report a surface the policy does not
    // enumerate, so these expectations are read out of the sources instead: every published
    // `trait` of the two numeric wrappers, every published `type` of the two module roots and of
    // the floating-rate companion, and every published class of the identifier source. Adding a
    // sixth callback, a sixth alias or a second witness fails this test until it is audited.
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
    // itself, so a row type that became published would be an unaudited public data type
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
 * The vocabulary is [[ApiSurfaceSpec.Kind]] and [[ApiSurfaceSpec.SurfaceRow]], followed by the
 * expected subjects of each kind, transcribed from the construction policy and held to the
 * declared inventory by the coverage assertions.
 *
 * The host implementations are the positive half of the open-contract rows, and they live here
 * rather than inside the spec class for a reason the compiler insists on: a case class nested in
 * a class carries a reference to its enclosing instance, which makes the equality check it
 * synthesises unverifiable at run time and - under this build's fatal-warning setting - an error.
 * Nested in an object they have no such reference, and the equality of the three case classes
 * below, which three of the rows rely on, is the ordinary structural one.
 */
private object ApiSurfaceSpec {

  /**
   * The kind of public surface an inventory row is about.
   *
   * The first three are the construction kinds, which fix what a type's constructor surface must
   * look like; the rest are the other ways a public surface can be classified. Every row carries
   * exactly one kind.
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

  /** A sealed family, whose subtypes are all declared in its own file. */
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
   * The audit is held as a function, so the inventory can be assembled - and counted - before it
   * executes.
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
   * Twenty-three types, each publishing neither `apply` nor `copy` and still destructuring.
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
   * The last two are the representation exception - a final class over a private primitive array,
   * with copy-safe total factories - and are audited as that rather than as case classes.
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
    "FailureReason")

  /**
   * The contracts this module leaves open.
   *
   * `IndexObservation` is one of them: the ported interface is a plain Java interface, so an
   * application may observe an indicator of its own, and the four observations this module builds
   * each live in a file of their own - which sealing the trait would forbid. Its row therefore
   * sits with the open contracts, and the sensitivity control below carries a probe for it.
   */
  val ExpectedOpenContractSubjects: Set[String] = Set(
    "ReferenceData",
    "ReferenceDataId",
    "IndexObservation",
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
    "UnsafeArrayHooks",
    "Decimal.MAX_SCALE",
    "DoubleArray.toArray",
    "DoubleArray factories",
    "DoubleMatrix.toArray",
    "DoubleMatrix factories",
    "ImmutableReferenceData.values",
    "JvmConstructionClosure",
    "JavaSerialization")

  /**
   * The number of tests this suite declares outside the inventory.
   *
   * The two coverage assertions, the registration assertion, the sensitivity control for the
   * sealing probes, and the derivation that reads the function, alias and witness surfaces out of
   * the module sources. The registration assertion compares this number plus the size of the
   * inventory against what ScalaTest holds, so an audit written outside a row fails it.
   */
  val StandaloneTests: Int = 5

  /**
   * A host's own reference data, wrapping another source and passing every question to it.
   *
   * It lives outside `ReferenceData.scala`, implements the one abstract member and overrides the
   * derived one - all three of which an application supplying its own holidays needs.
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
   * It implements the one member the trait leaves abstract - the witness for the type of data it
   * refers to - and nothing else, which is the whole of what an application has to write for data
   * of a type this library already knows.
   */
  final case class HostReferenceDataId(label: String) extends ReferenceDataId[HolidayCalendar] {
    override def valueType: ReferenceDataType[HolidayCalendar] = ReferenceDataType.holidayCalendar
  }

  /** A host's own floating rate, which is what `FloatingRateName` is from another file. */
  final class HostFloatingRate(val name: String, val floatingRateName: FloatingRateName)
      extends FloatingRate

  /**
   * A host's own observation, declared outside the file that declares the contract it satisfies.
   *
   * It exists to be compiled: an implementation of `IndexObservation` from outside the module is
   * the extension point the open trait offers, and a change that sealed the trait would stop this
   * declaration compiling rather than merely fail an assertion.
   */
  final case class HostIndexObservation(index: Index) extends IndexObservation

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
