/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Modifier
import java.time.DayOfWeek
import java.time.LocalDate
import java.time.Period
import java.time.YearMonth

import scala.util.Using

import cats.data.NonEmptyList

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.FixedScaleDecimal
import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.array.DoubleMatrix
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

import com.opengamma.strata.basics.Arbitraries._
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
import com.opengamma.strata.basics.date.AdjustableDates
import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.basics.date.BusinessDayConventions
import com.opengamma.strata.basics.date.DayCount
import com.opengamma.strata.basics.date.DaysAdjustment
import com.opengamma.strata.basics.date.HolidayCalendar
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.basics.date.HolidayCalendarIds
import com.opengamma.strata.basics.date.ImmutableHolidayCalendar
import com.opengamma.strata.basics.date.MarketTenor
import com.opengamma.strata.basics.date.PeriodAdditionConventions
import com.opengamma.strata.basics.date.PeriodAdjustment
import com.opengamma.strata.basics.date.SequenceDate
import com.opengamma.strata.basics.date.Tenor
import com.opengamma.strata.basics.date.TenorAdjustment
import com.opengamma.strata.basics.index.FxIndexObservation
import com.opengamma.strata.basics.index.FxIndices
import com.opengamma.strata.basics.index.IborIndexObservation
import com.opengamma.strata.basics.index.IborIndices
import com.opengamma.strata.basics.index.OvernightIndexObservation
import com.opengamma.strata.basics.index.OvernightIndices
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
import com.opengamma.strata.basics.value.ValueSchedule
import com.opengamma.strata.basics.value.ValueStep
import com.opengamma.strata.basics.value.ValueStepSequence

/**
 * Holds the '''construction''' of every validated and normalising type of both modules to the
 * explicit-error-handling policy, which is the half of Rule 5 / Gate 5 this suite serves. Its
 * sibling `FailableSurfaceSpec` holds the other half - the '''methods''' that return an outcome
 * - and neither repeats the other's cases.
 *
 * ===What construction has to prove===
 *
 * A validated type is a `sealed abstract case class X private (...)`, a form that generates
 * neither `apply` nor `copy`, so its factory is the only way a value of it comes into existence.
 * That makes the factory the whole of the type's guarantee, and three things about it are worth
 * asserting:
 *
 *  1. '''Every distinct invalid input is rejected, and rejected as the right kind of failure.'''
 *     A spec asserting only `isLeft` would pass even if every cause collapsed onto one reason, so
 *     each case below asserts the [[FailureReason]] by value through `beFailureWith`, and the
 *     attribute where one is fixed - the `definition` key a rejected schedule definition carries.
 *  1. '''Independent causes accumulate.''' The accumulating channel of `Validate` is the reason
 *     these factories return `EitherNec` rather than `Either`, and it is invisible unless a test
 *     supplies an input that is wrong in two ways at once and counts the chain. The third group
 *     below does that, for nine factories.
 *  1. '''Normalisation is what it says it is, and is idempotent.''' A normalising factory may
 *     rewrite its input - sort it, deduplicate it, round it, canonicalise a period, turn `-0.0`
 *     into `0.0` - and a value that has been through it must survive being put through it again
 *     unchanged, or no caller could rebuild a value from its own accessors.
 *
 * That `X(...)` and `.copy` do '''not''' exist, and that a sealed family cannot be extended,
 * belongs to `ApiSurfaceSpec`. Every value below comes from a factory or a constant of its type.
 *
 * ===Where a throw is still correct===
 *
 * Three numeric-domain edges stay `ArgCheck` throws rather than failures, because the routes
 * they sit behind are total in signature. Group five of this file owns all three, in twelve
 * `intercept` calls: four where `CurrencyAmount` arithmetic would produce a value that is not a
 * number, three where `Decimal` arithmetic passes eighteen digits, and five where a run of
 * amounts would be built with, or transformed into, an element no amount holds. Those twelve are
 * every `intercept` of this file that stands for a rejection of data; everywhere else a
 * data-dependent rejection is asserted as a reported failure rather than as a throw. The three
 * further `intercept` calls of the closure group at the end of the file stand for the guards a
 * caller reaches only from another language, which reject no data at all.
 *
 * ===Coverage===
 *
 * Every entry of the two lists below has a test in this file. The suite prints both lists from
 * [[SmartConstructorSpec.ValidatedTypes]] and [[SmartConstructorSpec.NormalisingTypes]], and its
 * first two tests count them and tie each name to a test registered against it.
 *
 * Validated `[V]` - `of` returns `EitherNec[Failure, X]`, or `Either[Failure, X]` where a single
 * cause is all there is to report:
 *
 * {{{
 * StandardId  Country  FxRate  FxMatrix  CurrencyAmountArray  MultiCurrencyAmountArray
 * MarketTenor  DaysAdjustment  PeriodAdjustment  TenorAdjustment  AdjustableDates
 * SchedulePeriod  Schedule  PeriodicSchedule  ValueStep  ValueSchedule  ValueStepSequence
 * Rounding.HalfUp  DayCount.Bus252  FixedScaleDecimal  IborIndexObservation
 * OvernightIndexObservation  FxIndexObservation
 * }}}
 *
 * Normalising `[N]` - `of` canonicalises its input and may still reject it:
 *
 * {{{
 * CurrencyAmount  Money  BigMoney  MultiCurrencyAmount  Decimal  Tenor  Frequency
 * SequenceDate  HolidayCalendarId  ImmutableHolidayCalendar  RollConvention.ofDayOfMonth
 * }}}
 *
 * `DaysAdjustment`, `Schedule` and `ValueSchedule` are the three entries whose condition is a
 * relation between fields rather than a property of one; each test states the relation its
 * factory enforces, and the failures those types report from their '''methods''' belong to
 * `FailableSurfaceSpec`.
 */
final class SmartConstructorSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  import SmartConstructorSpec._

  /**
   * The number of draws each idempotence property is checked against.
   *
   * Every property of group four holds of every value of its type - it claims that re-applying a
   * factory to the accessors of a value that factory produced yields an equal value - so the
   * verdict does not depend on which values are drawn and two runs agree whatever seed each is
   * given. The count is chosen for coverage and for runtime: fifty draws across the eleven
   * properties exercise each generator's corners several times over.
   */
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50)

  //-------------------------------------------------------------------------
  // Shared values, each reached through a factory or a constant of its type.
  //-------------------------------------------------------------------------
  private val gbp: Currency = Currency.GBP
  private val usd: Currency = Currency.USD
  private val eur: Currency = Currency.EUR
  private val jpy: Currency = Currency.JPY
  private val bhd: Currency = Currency.BHD

  private val gbpUsd: CurrencyPair = CurrencyPair.of(gbp, usd)
  private val eurJpy: CurrencyPair = CurrencyPair.of(eur, jpy)
  private val gbpGbp: CurrencyPair = CurrencyPair.of(gbp, gbp)

  private val gbp100: CurrencyAmount = amount(gbp, 100d)
  private val usd50: CurrencyAmount = amount(usd, 50d)

  /** A fortnight: the pair of dates most of the date and schedule cases are built from. */
  private val jul04: LocalDate = date(2014, 7, 4)
  private val jul18: LocalDate = date(2014, 7, 18)

  /** Four days of one week - two holidays, a Saturday declared working, and a weekend Sunday. */
  private val wed20140709: LocalDate = date(2014, 7, 9)
  private val thu20140710: LocalDate = date(2014, 7, 10)
  private val sat20140712: LocalDate = date(2014, 7, 12)
  private val sun20140713: LocalDate = date(2014, 7, 13)

  /** A Saturday outside the year range the fixture's holidays span. */
  private val sat20130713: LocalDate = date(2013, 7, 13)

  /** The identifier the calendar fixtures are built under, which resolves against no data. */
  private val testCalendarId: HolidayCalendarId = HolidayCalendarId.of("SmartCtorTest")

  /** The reference data every resolution in this file is offered, and the data that has none. */
  private val standardData: ReferenceData = ReferenceData.standard
  private val emptyData: ReferenceData = ReferenceData.empty

  /** The fixing date the three derived observations are built from, a mid-week day. */
  private val fixingDate: LocalDate = date(2020, 6, 10)

  //-------------------------------------------------------------------------
  // Unwrapping helpers. A factory of a validated type answers an outcome, so a fixture has to
  // take a value out of one; these fail the suite naming the reasons it was rejected for.
  //-------------------------------------------------------------------------
  private def accepted[A](result: ResultNec[A]): A =
    result.fold(
      failures =>
        fail(
          failures.toNonEmptyList.toList
            .map(failure => failure.message)
            .mkString("Expected a valid value but the factory rejected it: ", "; ", "")),
      value => value)

  private def produced[A](result: FailureOr[A]): A =
    result.fold(
      failure => fail(s"Expected a valid value but the factory rejected it: ${failure.message}"),
      value => value)

  /** The single failure of an outcome that reports one cause, for a check of its attributes. */
  private def failureOf[A](result: FailureOr[A]): Failure =
    result.fold(failure => failure, value => fail(s"Expected a failure but the factory produced $value"))

  /** Every failure of an accumulating outcome, in the order the factory produced them. */
  private def failuresOf[A](result: ResultNec[A]): List[Failure] =
    result.fold(
      failures => failures.toNonEmptyList.toList,
      value => fail(s"Expected failures but the factory produced $value"))

  /** The reasons of every failure of an accumulating outcome, in order. */
  private def reasonsOf[A](result: ResultNec[A]): List[FailureReason] =
    failuresOf(result).map(failure => failure.reason)

  /**
   * The messages of every failure of an accumulating outcome, in order.
   *
   * The accumulation tests compare these rather than the reasons: every failure `Validate`
   * produces carries the reason `INVALID`, so reasons cannot tell one cause from another.
   */
  private def messagesOf[A](result: ResultNec[A]): List[String] =
    failuresOf(result).map(failure => failure.message)

  /**
   * Rebuilds a calendar from its own accessors, which is what its idempotence is a claim about.
   *
   * The arguments are the identifier and the three inputs the factory rewrites: the holidays it
   * sorts and deduplicates, the weekend it applies, and the overrides it applies last. A
   * calendar that survives this unchanged is one a decoder or a document can reproduce.
   */
  private def rebuilt(calendar: ImmutableHolidayCalendar): ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(
      calendar.id,
      calendar.holidays,
      calendar.weekendDays,
      calendar.workingDays)

  /** A currency amount, for a fixture that needs one rather than the outcome of building one. */
  private def amount(currency: Currency, value: Double): CurrencyAmount =
    produced(CurrencyAmount.of(currency, value))

  /** A decimal of text, for a fixture that needs the value rather than the outcome. */
  private def decimal(text: String): Decimal = produced(Decimal.of(text))

  //-------------------------------------------------------------------------
  // The coverage claim of the header, checked rather than asserted in prose: the two lists are
  // counted, and every name they hold is tied to a test of this suite.
  //-------------------------------------------------------------------------
  test("every validated and normalising type of AAP 0.3.3 is covered by this suite") {
    ValidatedTypes should have size 23
    NormalisingTypes should have size 11
    ValidatedTypes.distinct shouldBe ValidatedTypes
    NormalisingTypes.distinct shouldBe NormalisingTypes
    (ValidatedTypes intersect NormalisingTypes) shouldBe empty
    info(s"[V] covered: ${ValidatedTypes.mkString(", ")}")
    info(s"[N] covered: ${NormalisingTypes.mkString(", ")}")
  }

  test("every type the coverage lists name has a test of this suite registered against it") {
    // What the counts above cannot say: the lists are names, and the tests that exercise them
    // are written separately, so a test renamed or deleted would leave the counts intact and a
    // list still claiming coverage of a type nothing exercises. A type is claimed by a test whose
    // name begins with it, or - for a member - by a test of its type naming it, which is how
    // `DayCount.Bus252` is claimed. The three derived observations share one test, required here
    // by name.
    val sharedObservationTest = "the three derived observations"
    val sharedObservationTypes =
      Set("IborIndexObservation", "OvernightIndexObservation", "FxIndexObservation")
    withClue(s"the test named '$sharedObservationTest ...' must exist: ")(
      testNames.exists(name => name.startsWith(sharedObservationTest)) shouldBe true)

    def claimedByATest(entry: String): Boolean =
      if (sharedObservationTypes.contains(entry)) {
        testNames.exists(name => name.startsWith(sharedObservationTest))
      } else if (testNames.exists(name => name.startsWith(entry))) {
        true
      } else {
        entry.split('.').toList match {
          case owner :: member :: Nil =>
            testNames.exists(name => name.startsWith(s"$owner.") && name.contains(member))
          case _ => false
        }
      }

    withClue("every name the coverage lists claim is exercised by a test of this suite: ")(
      (ValidatedTypes ++ NormalisingTypes).filterNot(claimedByATest) shouldBe empty)
  }

  //-------------------------------------------------------------------------
  // Group one: the validated types. One test per type, one case per distinct invalid input, and
  // one acceptable value, which is what gives the rejections their meaning.
  //-------------------------------------------------------------------------
  test("StandardId.of rejects a scheme and a value that do not match their permitted shapes") {
    // accumulating: the scheme and the value are described separately, counted in group three
    StandardId.of("", "AAPL") should beFailureWith(FailureReason.INVALID)
    StandardId.of("OG~Ticker", "AAPL") should beFailureWith(FailureReason.INVALID)
    StandardId.of("OG-Ticker", "") should beFailureWith(FailureReason.INVALID)
    StandardId.of("OG-Ticker", " AAPL") should beFailureWith(FailureReason.INVALID)
    StandardId.of("OG-Ticker", "AA~PL") should beFailureWith(FailureReason.INVALID)
    StandardId.parse("no-separator-here") should beFailureWith(FailureReason.PARSING)
    StandardId.of("OG-Ticker", "AAPL") should beSuccess
  }

  test("Country.of rejects a code that is not two upper case letters") {
    Country.of("") should beFailureWith(FailureReason.INVALID)
    Country.of("G") should beFailureWith(FailureReason.INVALID)
    Country.of("GBR") should beFailureWith(FailureReason.INVALID)
    Country.of("gb") should beFailureWith(FailureReason.INVALID)
    Country.of("G1") should beFailureWith(FailureReason.INVALID)
    // the code space is open, so an unassigned but well-shaped code is accepted
    Country.of("ZZ") should beSuccess
    Country.of("GB") should beSuccess

    // single-cause: of3Char reports the code's shape or a missing translation, different reasons
    Country.of3Char("GB") should beFailureWith(FailureReason.INVALID)
    Country.of3Char("QQQ") should beFailureWith(FailureReason.PARSING)
    Country.of3Char("GBR") should beSuccess
  }

  test("FxRate.of rejects a rate that is not positive and an identical pair whose rate is not one") {
    FxRate.of(gbpUsd, 0d) should beFailureWith(FailureReason.INVALID)
    FxRate.of(gbpUsd, -1.5d) should beFailureWith(FailureReason.INVALID)
    FxRate.of(gbpGbp, 2d) should beFailureWith(FailureReason.INVALID)
    FxRate.of(gbp, usd, -1.5d) should beFailureWith(FailureReason.INVALID)
    FxRate.of(gbp, gbp, 2d) should beFailureWith(FailureReason.INVALID)
    FxRate.parse("GBP/USD") should beFailureWith(FailureReason.PARSING)
    FxRate.of(gbpUsd, 1.25d) should beSuccess
    FxRate.of(gbpGbp, 1d) should beSuccess
  }

  test("FxMatrix construction rejects rates it cannot place and a stated matrix of the wrong shape") {
    val gbpUsdRate: FxRate = accepted(FxRate.of(gbpUsd, 1.25d))
    val eurJpyRate: FxRate = accepted(FxRate.of(eurJpy, 130d))

    // single-cause: the rates are folded in one at a time, so only the first misfit is reported
    val disjoint: FailureOr[FxMatrix] = FxMatrix.of(List(gbpUsdRate, eurJpyRate))
    disjoint should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    FxMatrix.of(List(gbpUsdRate)) should beSuccess

    // accumulating: the three structural conditions of a stated matrix are independent
    FxMatrix.fromMatrix(Vector(gbp, gbp), DoubleMatrix.identity(2)) should
      beFailureWith(FailureReason.INVALID)
    FxMatrix.fromMatrix(Vector(gbp, usd), DoubleMatrix.of(2, 3, 1d, 1.25d, 0d, 0.8d, 1d, 0d)) should
      beFailureWith(FailureReason.INVALID)
    FxMatrix.fromMatrix(Vector(gbp, usd), DoubleMatrix.identity(3)) should
      beFailureWith(FailureReason.INVALID)
    FxMatrix.fromMatrix(Vector(gbp, usd), DoubleMatrix.of(2, 2, 2d, 1.25d, 0.8d, 1d)) should
      beFailureWith(FailureReason.INVALID)
    FxMatrix.fromMatrix(Vector(gbp, usd), DoubleMatrix.of(2, 2, 1d, 1.25d, 0.8d, 1d)) should beSuccess
  }

  test("FxMatrix accepts a rate of zero, because reciprocity and triangulation are not checked") {
    // What is not validated is as much a part of the contract as what is: a rate is placed as
    // given, so zero makes a matrix whose opposite direction is infinite, and a decoder that
    // rejected zero would reject a matrix that can be built
    val zeroRate: FxMatrix = FxMatrix.of(gbpUsd, 0d)
    zeroRate.fxRate(gbp, usd) should haveValue(0d)
    produced(zeroRate.fxRate(usd, gbp)) shouldBe Double.PositiveInfinity
    accepted(FxMatrix.fromMatrix(zeroRate.currencies, zeroRate.rates)) shouldBe zeroRate
  }

  test("CurrencyAmountArray.of rejects an empty collection and a collection of several currencies") {
    // one failure per call: a collection holding no amount names no currency to disagree about
    val empty: ResultNec[CurrencyAmountArray] = CurrencyAmountArray.of(List.empty[CurrencyAmount])
    empty should beFailureWith(FailureReason.INVALID)
    failuresOf(empty) should have size 1

    val mixed: ResultNec[CurrencyAmountArray] = CurrencyAmountArray.of(List(gbp100, usd50))
    mixed should beFailureWith(FailureReason.INVALID)
    failuresOf(mixed) should have size 1

    CurrencyAmountArray.of(0, (_: Int) => gbp100) should beFailureWith(FailureReason.INVALID)
    CurrencyAmountArray.of(2, (index: Int) => if (index == 0) gbp100 else usd50) should
      beFailureWith(FailureReason.INVALID)

    CurrencyAmountArray.of(List(gbp100, amount(gbp, 200d))) should beSuccess
    CurrencyAmountArray.of(gbp, DoubleArray.of(1d, 2d)).values shouldBe DoubleArray.of(1d, 2d)
  }

  test("MultiCurrencyAmountArray.of rejects disagreeing lengths and a value that is not a number") {
    val unequal: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.of(Map(gbp -> DoubleArray.of(1d, 2d), usd -> DoubleArray.of(3d)))
    unequal should beFailureWith(FailureReason.INVALID)

    val totalled: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.total(
        List(
          CurrencyAmountArray.of(gbp, DoubleArray.of(1d, 2d)),
          CurrencyAmountArray.of(usd, DoubleArray.of(3d))))
    totalled should beFailureWith(FailureReason.INVALID)

    // The second invalid input is an element rather than a shape. A run of amounts is a run of
    // amounts, and this module's amount refuses a value that is not a number, so a run holding
    // one describes nothing: reading it in and letting a later read fail is what admitting it
    // would amount to. The factory that takes values per currency therefore examines them, and
    // names the currency and the index it found rather than only that something was wrong.
    val notANumber: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.of(Map(gbp -> DoubleArray.of(1d, Double.NaN)))
    notANumber should beFailureWith(FailureReason.INVALID)
    messagesOf(notANumber) shouldBe List("Argument 'values' for GBP must not be NaN at index 1")

    // and the same of the factory that adds runs up, where the value is produced by the addition
    // rather than supplied: two runs of one currency carrying opposed infinities sum to one
    val opposed: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.total(
        List(
          CurrencyAmountArray.of(gbp, DoubleArray.of(Double.PositiveInfinity)),
          CurrencyAmountArray.of(gbp, DoubleArray.of(Double.NegativeInfinity))))
    messagesOf(opposed) shouldBe List("Argument 'values' for GBP must not be NaN at index 0")

    // the infinities themselves are values this module holds, exactly as its amount holds them,
    // so the rejections above are of the one value that has no amount and not of a large one
    MultiCurrencyAmountArray.of(
      Map(gbp -> DoubleArray.of(1d, 2d), usd -> DoubleArray.of(3d, 4d))) should beSuccess
    MultiCurrencyAmountArray.of(
      Map(gbp -> DoubleArray.of(Double.PositiveInfinity, 2d))) should beSuccess
  }

  test("MarketTenor construction rejects a count that is not a tenor and text that names none") {
    // single-cause: the reasons are joined into one failure, hence Either rather than EitherNec
    MarketTenor.ofSpotDays(0) should beFailureWith(FailureReason.INVALID)
    MarketTenor.ofSpotDays(-1) should beFailureWith(FailureReason.INVALID)
    MarketTenor.ofSpotMonths(0) should beFailureWith(FailureReason.INVALID)
    MarketTenor.ofSpotYears(-1) should beFailureWith(FailureReason.INVALID)
    MarketTenor.parse("") should beFailureWith(FailureReason.INVALID)
    MarketTenor.parse("QQ") should beFailureWith(FailureReason.PARSING)
    MarketTenor.ofSpot(Tenor.TENOR_3M) should beSuccess
    MarketTenor.parse("ON") should beSuccess
  }

  test("DaysAdjustment.of rejects a day count of zero paired with an addition calendar") {
    // The one thing about the three fields of an adjustment that can be wrong: the addition
    // calendar is what makes the days business days, so a count of zero paired with one asks for
    // a walk of zero business days, which names no day. Whether it resolves belongs to a method.
    val zeroAgainstACalendar: ResultNec[DaysAdjustment] =
      DaysAdjustment.of(0, HolidayCalendarIds.GBLO, BusinessDayAdjustment.NONE)
    zeroAgainstACalendar should beFailureWith(FailureReason.INVALID)
    // one cause and one only: a count is any integer and either calendar may be composite
    failuresOf(zeroAgainstACalendar) should have size 1
    failuresOf(zeroAgainstACalendar).head shouldBe a[Failure.Invalid]
    messagesOf(zeroAgainstACalendar).head should include(HolidayCalendarIds.GBLO.name)
    DaysAdjustment.of(0, HolidayCalendarId.of("GBLO+USNY"), BusinessDayAdjustment.NONE) should
      beFailureWith(FailureReason.INVALID)

    // the positive controls: a non-zero count walks business days, a zero count moves nothing
    DaysAdjustment.of(2, HolidayCalendarIds.GBLO, BusinessDayAdjustment.NONE) should beSuccess
    DaysAdjustment.of(-2, HolidayCalendarIds.GBLO, BusinessDayAdjustment.NONE) should beSuccess
    DaysAdjustment.of(0, HolidayCalendarIds.NO_HOLIDAYS, BusinessDayAdjustment.NONE) should
      haveValue(DaysAdjustment.NONE)

    // The four named factories are total because each lands inside the field space `of` accepts,
    // asserted rather than assumed: every value they build is run back through `of` unchanged.
    val follow: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.USNY)
    val built: List[DaysAdjustment] =
      List(
        DaysAdjustment.ofCalendarDays(3),
        DaysAdjustment.ofCalendarDays(-3),
        DaysAdjustment.ofCalendarDays(0, follow),
        DaysAdjustment.ofBusinessDays(2, HolidayCalendarIds.GBLO),
        DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.GBLO),
        DaysAdjustment.ofBusinessDays(2, HolidayCalendarIds.GBLO, follow),
        DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.GBLO, follow),
        DaysAdjustment.NONE)
    built.map(value => DaysAdjustment.of(value.days, value.calendar, value.adjustment)) shouldBe
      built.map(value => Right(value))

    // and the rejected pairing is reachable through none of them: every zero-day value names the
    // no-holidays identifier, the calendar a caller named surviving as the trailing adjustment
    built.filter(value => value.days == 0).map(value => value.calendar).distinct shouldBe
      List(HolidayCalendarIds.NO_HOLIDAYS)
    DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.GBLO).adjustment.calendar shouldBe
      HolidayCalendarIds.GBLO
    DaysAdjustment.ofCalendarDays(3).days shouldBe 3
    DaysAdjustment.ofBusinessDays(2, HolidayCalendarIds.GBLO).calendar shouldBe
      HolidayCalendarIds.GBLO
  }

  test("PeriodAdjustment.of rejects a period holding days paired with a month-based convention") {
    PeriodAdjustment.of(
      Period.ofDays(3),
      PeriodAdditionConventions.LAST_DAY,
      BusinessDayAdjustment.NONE) should beFailureWith(FailureReason.INVALID)
    PeriodAdjustment.of(
      Period.of(0, 1, 2),
      PeriodAdditionConventions.LAST_BUSINESS_DAY,
      BusinessDayAdjustment.NONE) should beFailureWith(FailureReason.INVALID)
    PeriodAdjustment.ofLastDay(Period.ofDays(3), BusinessDayAdjustment.NONE) should
      beFailureWith(FailureReason.INVALID)
    PeriodAdjustment.ofLastBusinessDay(Period.ofDays(3), BusinessDayAdjustment.NONE) should
      beFailureWith(FailureReason.INVALID)
    PeriodAdjustment.of(
      Period.ofDays(3),
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.NONE) should beSuccess
    PeriodAdjustment.ofLastDay(Period.ofMonths(3), BusinessDayAdjustment.NONE) should beSuccess
  }

  test("TenorAdjustment.of rejects a tenor that is not month-based paired with a month convention") {
    TenorAdjustment.of(
      Tenor.TENOR_1W,
      PeriodAdditionConventions.LAST_DAY,
      BusinessDayAdjustment.NONE) should beFailureWith(FailureReason.INVALID)
    TenorAdjustment.ofLastDay(Tenor.TENOR_1W, BusinessDayAdjustment.NONE) should
      beFailureWith(FailureReason.INVALID)
    TenorAdjustment.ofLastBusinessDay(Tenor.TENOR_1W, BusinessDayAdjustment.NONE) should
      beFailureWith(FailureReason.INVALID)
    TenorAdjustment.of(
      Tenor.TENOR_1W,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.NONE) should beSuccess
    TenorAdjustment.ofLastDay(Tenor.TENOR_3M, BusinessDayAdjustment.NONE) should beSuccess
  }

  test("AdjustableDates.of rejects an empty run and a run that does not strictly increase") {
    val empty: ResultNec[AdjustableDates] =
      AdjustableDates.of(BusinessDayAdjustment.NONE, List.empty[LocalDate])
    empty should beFailureWith(FailureReason.INVALID)
    failuresOf(empty) should have size 1

    AdjustableDates.of(BusinessDayAdjustment.NONE, List(jul18, jul04)) should
      beFailureWith(FailureReason.INVALID)
    AdjustableDates.of(BusinessDayAdjustment.NONE, List(jul04, jul04)) should
      beFailureWith(FailureReason.INVALID)
    AdjustableDates.of(List(jul18, jul04)) should beFailureWith(FailureReason.INVALID)
    AdjustableDates.of(BusinessDayAdjustment.NONE, List(jul04, jul18)) should beSuccess
  }

  test("SchedulePeriod.of rejects dates that are out of order or equal, under both pairs of names") {
    SchedulePeriod.of(jul18, jul04) should beFailureWith(FailureReason.INVALID)
    SchedulePeriod.of(jul04, jul04) should beFailureWith(FailureReason.INVALID)
    // one pair wrong is one failure: the adjusted pair here is in order, the unadjusted is not
    val unadjustedOnly: ResultNec[SchedulePeriod] =
      SchedulePeriod.of(jul04, jul18, jul18, jul04)
    unadjustedOnly should beFailureWith(FailureReason.INVALID)
    failuresOf(unadjustedOnly) should have size 1
    SchedulePeriod.of(jul04, jul18) should beSuccess
    SchedulePeriod.of(jul04, jul18, jul04, jul18) should beSuccess
  }

  test("Schedule.of rejects periods that do not run from earliest to latest, and allows gaps") {
    // Every member that reads the periods reads them as a time line: the schedule is the
    // ScheduleInfo a day count accrues against, periodEndDate answers with the first period
    // containing a date, a value schedule resolves a step by a period boundary - and a list that
    // is not a time line makes all of those answer wrongly rather than fail.
    val july: SchedulePeriod = accepted(SchedulePeriod.of(jul04, jul18))
    val august: SchedulePeriod = accepted(SchedulePeriod.of(date(2014, 8, 1), date(2014, 8, 15)))
    val september: SchedulePeriod = accepted(SchedulePeriod.of(date(2014, 9, 1), date(2014, 9, 15)))

    // accumulating: a misplaced pair is wrong under both pairs of dates and both are reported, a
    // caller correcting the unadjusted dates being helped by knowing of the adjusted ones
    val reversedPair: ResultNec[Schedule] =
      Schedule.of(NonEmptyList.of(august, july), Frequency.P1M, RollConventions.DAY_15)
    reversedPair should beFailureWith(FailureReason.INVALID)
    failuresOf(reversedPair) should have size 2
    failuresOf(reversedPair).head shouldBe a[Failure.Invalid]
    val pairMessages: List[String] = messagesOf(reversedPair)
    pairMessages.count(message => message.contains("the unadjusted end date")) shouldBe 1
    pairMessages.count(message => message.contains("the adjusted end date")) shouldBe 1
    val reported: String = pairMessages.mkString("; ")
    reported should include("2014-08-15")
    reported should include("2014-07-04")
    reported should include("index 0")
    reported should include("index 1")

    // every misplaced pair is reported, not only the first, under each pair of dates
    val reversedRun: ResultNec[Schedule] =
      Schedule.of(NonEmptyList.of(september, august, july), Frequency.P1M, RollConventions.DAY_15)
    failuresOf(reversedRun) should have size 4
    reasonsOf(reversedRun).distinct shouldBe List(FailureReason.INVALID)
    val runMessages: List[String] = messagesOf(reversedRun)
    runMessages.distinct should have size 4
    runMessages.count(message => message.contains("the unadjusted end date")) shouldBe 2
    runMessages.count(message => message.contains("the adjusted end date")) shouldBe 2

    // the two pairs of dates are checked independently, so a list ordered unadjusted and
    // overlapping adjusted reports only that - and only an adjustment can produce such a list,
    // which is why the adjusted pair is checked at all
    val adjustedOverlap: ResultNec[Schedule] =
      Schedule.of(
        NonEmptyList.of(
          accepted(SchedulePeriod.of(jul04, date(2014, 7, 21), jul04, jul18)),
          accepted(SchedulePeriod.of(jul18, date(2014, 8, 1), jul18, date(2014, 8, 1)))),
        Frequency.P2W,
        RollConventions.DAY_4)
    adjustedOverlap should beFailureWith(FailureReason.INVALID)
    failuresOf(adjustedOverlap) should have size 1
    messagesOf(adjustedOverlap).head should include("the adjusted end date")

    // the positive controls. One period is a time line whatever its dates; a gap is allowed,
    // since accrual may pause; and one period ending as the next begins is why the check is
    // order rather than strict order
    val one: ResultNec[Schedule] =
      Schedule.of(NonEmptyList.one(july), Frequency.P2W, RollConventions.DAY_4)
    one should beSuccess
    accepted(one).periods shouldBe NonEmptyList.one(july)
    Schedule.of(NonEmptyList.of(july, august), Frequency.P1M, RollConventions.DAY_15) should
      beSuccess
    Schedule.of(
      NonEmptyList.of(july, accepted(SchedulePeriod.of(jul18, date(2014, 8, 1)))),
      Frequency.P2W,
      RollConventions.DAY_4) should beSuccess
  }

  test("PeriodicSchedule.of rejects a definition whose dates are out of order") {
    PeriodicSchedule.of(jul18, jul04, Frequency.P1M, BusinessDayAdjustment.NONE) should
      beFailureWith(FailureReason.INVALID)
    PeriodicSchedule.of(
      jul04,
      jul18,
      Frequency.P1M,
      BusinessDayAdjustment.NONE,
      None,
      None,
      None,
      None,
      Some(date(2015, 1, 1)),
      None,
      None) should beFailureWith(FailureReason.INVALID)
    PeriodicSchedule.of(
      jul04,
      jul18,
      Frequency.P1M,
      BusinessDayAdjustment.NONE,
      None,
      None,
      None,
      None,
      None,
      Some(date(2015, 1, 1)),
      None) should beFailureWith(FailureReason.INVALID)
    PeriodicSchedule.of(jul04, jul18, Frequency.P1M, BusinessDayAdjustment.NONE) should beSuccess
  }

  test("a rejected schedule definition carries the definition it rejected as an attribute") {
    // One attribute name is fixed for this module - the `definition` key a schedule failure
    // carries - so it is asserted as well as the reason. The definition below is accepted by `of`
    // and rejected by generation: whether a stub is allowed needs the schedule rolled out.
    val definition: PeriodicSchedule =
      accepted(
        PeriodicSchedule.of(
          date(2014, 6, 4),
          date(2014, 9, 17),
          Frequency.P1M,
          BusinessDayAdjustment.NONE,
          StubConvention.NONE,
          RollConventions.DAY_4))
    val rejection: FailureOr[List[LocalDate]] = definition.createUnadjustedDates()
    rejection should beFailureWith(FailureReason.INVALID)
    val reported: Failure = failureOf(rejection)
    reported shouldBe a[Failure.Invalid]
    reported.attributes.get(DefinitionAttribute) shouldBe Some(definition.toString)
  }

  test("ValueStep.of rejects a step positioned in neither way, in both ways, or at an index of zero") {
    val adjustment: ValueAdjustment = ValueAdjustment.ofDeltaAmount(-2000d)
    ValueStep.of(None, None, adjustment) should beFailureWith(FailureReason.INVALID)
    ValueStep.of(Some(1), Some(jul04), adjustment) should beFailureWith(FailureReason.INVALID)
    ValueStep.of(Some(0), None, adjustment) should beFailureWith(FailureReason.INVALID)
    ValueStep.of(Some(-1), None, adjustment) should beFailureWith(FailureReason.INVALID)
    ValueStep.of(0, adjustment) should beFailureWith(FailureReason.INVALID)
    ValueStep.of(1, adjustment) should beSuccess
    ValueStep.of(None, Some(jul04), adjustment) should beSuccess
    ValueStep.of(jul04, adjustment).date shouldBe Some(jul04)
  }

  test("ValueSchedule.of rejects two steps naming one position with different adjustments") {
    // The half of the contradiction that needs no schedule to see: two steps carrying the same
    // position - a period index, or a date - ask one point of the time line for two values. The
    // other half, an index and that period's boundary date, stays with resolveValues.
    val replace300: ValueAdjustment = ValueAdjustment.ofReplace(300d)
    val replace400: ValueAdjustment = ValueAdjustment.ofReplace(400d)
    val atIndex1: ValueStep = accepted(ValueStep.of(1, replace300))
    val alsoAtIndex1: ValueStep = accepted(ValueStep.of(1, replace400))
    val atJul04: ValueStep = ValueStep.of(jul04, replace300)
    val alsoAtJul04: ValueStep = ValueStep.of(jul04, replace400)

    val index: ResultNec[ValueSchedule] = ValueSchedule.of(200d, List(atIndex1, alsoAtIndex1))
    index should beFailureWith(FailureReason.INVALID)
    failuresOf(index) should have size 1
    failuresOf(index).head shouldBe a[Failure.Invalid]
    messagesOf(index).head should include("period index 1")

    val dated: ResultNec[ValueSchedule] = ValueSchedule.of(200d, atJul04, alsoAtJul04)
    dated should beFailureWith(FailureReason.INVALID)
    failuresOf(dated) should have size 1
    messagesOf(dated).head should include("date 2014-07-04")

    // accumulating: each doubly named position is a cause of its own, index and date alike
    val both: ResultNec[ValueSchedule] =
      ValueSchedule.of(200d, List(atIndex1, alsoAtIndex1, atJul04, alsoAtJul04), None)
    failuresOf(both) should have size 2
    reasonsOf(both).distinct shouldBe List(FailureReason.INVALID)
    messagesOf(both) should contain allElementsOf messagesOf(index)
    messagesOf(both) should contain allElementsOf messagesOf(dated)
    messagesOf(both).distinct should have size 2

    // The positive controls. Two steps at one position asking for the same adjustment agree
    // rather than contradict. An index and a date are different positions, so a definition
    // naming one of each is judged on resolution.
    val twice: ResultNec[ValueSchedule] = ValueSchedule.of(200d, List(atIndex1, atIndex1))
    twice should beSuccess
    accepted(twice).steps shouldBe List(atIndex1, atIndex1)
    ValueSchedule.of(200d, List(atIndex1, atJul04), None) should beSuccess

    // the remaining overloads name no position twice, so they cannot reach the check at all
    val sequence: ValueStepSequence =
      accepted(
        ValueStepSequence.of(jul04, jul18, Frequency.P1M, ValueAdjustment.ofDeltaAmount(-100d)))
    accepted(ValueSchedule.of(100d)).initialValue shouldBe 100d
    accepted(ValueSchedule.of(100d, List.empty[ValueStep], None)).stepSequence shouldBe None
    accepted(ValueSchedule.of(100d, List(atIndex1))).steps shouldBe List(atIndex1)
    accepted(ValueSchedule.of(100d, sequence)).stepSequence shouldBe Some(sequence)

    // the two `with` operations re-validate: the steps they are given are the caller's, so they
    // route through the factory and answer with its outcome rather than with a schedule
    val base: ValueSchedule = accepted(ValueSchedule.of(200d, List(atIndex1)))
    base.withSteps(List(atIndex1, alsoAtIndex1)) should beFailureWith(FailureReason.INVALID)
    accepted(base.withSteps(List(atJul04))).steps shouldBe List(atJul04)
    accepted(base.withStepSequence(sequence)).stepSequence shouldBe Some(sequence)
  }

  test("ValueStepSequence.of rejects dates out of order and an adjustment that replaces the value") {
    val delta: ValueAdjustment = ValueAdjustment.ofDeltaAmount(-100d)
    val replace: ValueAdjustment = ValueAdjustment.ofReplace(100d)
    val outOfOrder: ResultNec[ValueStepSequence] =
      ValueStepSequence.of(jul18, jul04, Frequency.P1M, delta)
    outOfOrder should beFailureWith(FailureReason.INVALID)
    failuresOf(outOfOrder) should have size 1
    ValueStepSequence.of(jul04, jul18, Frequency.P1M, replace) should
      beFailureWith(FailureReason.INVALID)
    ValueStepSequence.of(jul04, jul18, Frequency.P1M, delta) should beSuccess
    ValueStepSequence.of(jul04, jul04, Frequency.P1M, delta) should beSuccess
  }

  test("Rounding.HalfUp rejects decimal places and a fraction outside their permitted ranges") {
    HalfUp.ofDecimalPlaces(-1) should beFailureWith(FailureReason.INVALID)
    HalfUp.ofDecimalPlaces(257) should beFailureWith(FailureReason.INVALID)
    HalfUp.ofFractionalDecimalPlaces(-1, 0) should beFailureWith(FailureReason.INVALID)
    HalfUp.ofFractionalDecimalPlaces(257, 0) should beFailureWith(FailureReason.INVALID)
    HalfUp.ofFractionalDecimalPlaces(0, -1) should beFailureWith(FailureReason.INVALID)
    HalfUp.ofFractionalDecimalPlaces(0, 257) should beFailureWith(FailureReason.INVALID)
    Rounding.ofDecimalPlaces(-1) should beFailureWith(FailureReason.INVALID)
    Rounding.ofFractionalDecimalPlaces(0, 257) should beFailureWith(FailureReason.INVALID)
    HalfUp.ofDecimalPlaces(4) should beSuccess
    HalfUp.ofFractionalDecimalPlaces(4, 32) should beSuccess
    Rounding.ofDecimalPlaces(2) should beSuccess
  }

  test("DayCount.ofBus252 reports a calendar identifier the reference data cannot resolve") {
    // single-cause: the identifier resolves against the data it is given, or it does not
    DayCount.ofBus252(HolidayCalendarIds.BRBD, emptyData) should
      beFailureWith(FailureReason.MISSING_DATA)
    DayCount.ofBus252(testCalendarId, standardData) should beFailureWith(FailureReason.MISSING_DATA)
    val resolved: FailureOr[DayCount] = DayCount.ofBus252(HolidayCalendarIds.BRBD, standardData)
    resolved should beSuccess
    produced(resolved).name shouldBe "Bus/252 BRBD"
    // the calendar-taking form is total, since a resolved calendar cannot be wrong
    val calendar: HolidayCalendar = produced(HolidayCalendarIds.BRBD.resolve(standardData))
    DayCount.ofBus252(calendar).name shouldBe "Bus/252 BRBD"
  }

  test("FixedScaleDecimal.of rejects a scale below the decimal's own and a scale above eighteen") {
    // The two causes are combined rather than sequenced, but cannot both hold: a scale below a
    // decimal's own is at most seventeen, eighteen being the largest a decimal has
    val below: ResultNec[FixedScaleDecimal] = FixedScaleDecimal.of(decimal("12.345"), 2)
    below should beFailureWith(FailureReason.INVALID)
    failuresOf(below) should have size 1

    val above: ResultNec[FixedScaleDecimal] = FixedScaleDecimal.of(decimal("12.3"), 19)
    above should beFailureWith(FailureReason.INVALID)
    failuresOf(above) should have size 1

    FixedScaleDecimal.parse("12.3456789012345678901") should beFailureWith(FailureReason.INVALID)
    FixedScaleDecimal.of(decimal("12.3"), 3) should beSuccess
    FixedScaleDecimal.parse("12.30") should beSuccess
  }

  test("the three derived observations report a fixing calendar the reference data cannot resolve") {
    // single-cause each: an unresolvable fixing calendar is the one thing that can go wrong
    IborIndexObservation.of(IborIndices.GBP_LIBOR_3M, fixingDate, emptyData) should
      beFailureWith(FailureReason.MISSING_DATA)
    OvernightIndexObservation.of(OvernightIndices.GBP_SONIA, fixingDate, emptyData) should
      beFailureWith(FailureReason.MISSING_DATA)
    FxIndexObservation.of(FxIndices.EUR_USD_ECB, fixingDate, emptyData) should
      beFailureWith(FailureReason.MISSING_DATA)

    IborIndexObservation.of(IborIndices.GBP_LIBOR_3M, fixingDate, standardData) should beSuccess
    OvernightIndexObservation.of(OvernightIndices.GBP_SONIA, fixingDate, standardData) should
      beSuccess
    FxIndexObservation.of(FxIndices.EUR_USD_ECB, fixingDate, standardData) should beSuccess
  }

  //-------------------------------------------------------------------------
  // Group two: the normalising types. Each test states what the factory rejects and what it
  // rewrites, the rewrite being as much a part of the contract as the rejection.
  //-------------------------------------------------------------------------
  test("CurrencyAmount.of rejects a non-number, keeps the infinities, and normalises negative zero") {
    // single-cause: there is one condition on an amount, so the outcome carries one failure
    CurrencyAmount.of(gbp, Double.NaN) should beFailureWith(FailureReason.INVALID)
    failureOf(CurrencyAmount.of(gbp, Double.NaN)) shouldBe a[Failure.Invalid]

    // the infinities are numbers for this purpose and are accepted; only a not-a-number is not
    produced(CurrencyAmount.of(gbp, Double.PositiveInfinity)).amount shouldBe Double.PositiveInfinity
    produced(CurrencyAmount.of(gbp, Double.NegativeInfinity)).amount shouldBe Double.NegativeInfinity

    // negative zero is normalised to positive zero, which `==` cannot see: the assertion is on
    // the bit pattern, this type's equality being doubleToLongBits equality
    val normalised: CurrencyAmount = produced(CurrencyAmount.of(gbp, -0.0d))
    java.lang.Double.compare(normalised.amount, 0.0d) shouldBe 0
    java.lang.Double.doubleToLongBits(normalised.amount) shouldBe
      java.lang.Double.doubleToLongBits(0.0d)
    normalised.isZero shouldBe true
    normalised.isPositive shouldBe false
    normalised.isNegative shouldBe false
    normalised shouldBe CurrencyAmount.zero(gbp)

    CurrencyAmount.of("QQQ", 1d) should beFailureWith(FailureReason.PARSING)
    CurrencyAmount.parse("GBP") should beFailureWith(FailureReason.PARSING)
    CurrencyAmount.parse("GBP 100") should beSuccess
  }

  test("Money.of rounds its amount to the minor units of its currency, half up") {
    // one case per distinct minorUnitDigits in the family: two for most of its currencies,
    // nought for the twelve that have no minor unit, three for BHD and OMR alone
    produced(Money.of(gbp, 100.1249d)).amount shouldBe decimal("100.12")
    produced(Money.of(gbp, 100.125d)).amount shouldBe decimal("100.13")
    produced(Money.of(bhd, 100.1249d)).amount shouldBe decimal("100.125")
    produced(Money.of(bhd, 100.1255d)).amount shouldBe decimal("100.126")
    produced(Money.of(jpy, 100.4d)).amount shouldBe decimal("100")
    produced(Money.of(jpy, 100.5d)).amount shouldBe decimal("101")
    // the rounding is in the one private creation of the type, so every route reaches it
    Money.of(gbp, decimal("100.125")).amount shouldBe decimal("100.13")
    produced(Money.of(amount(gbp, 100.125d))).amount shouldBe decimal("100.13")
    Money.zero(gbp).amount shouldBe Decimal.ZERO
    Money.parse("$100") should beFailureWith(FailureReason.PARSING)
    Money.parse("GBP 100.13") should beSuccess
  }

  test("BigMoney.of rounds its amount to twelve decimal places, half up") {
    BigMoney.of(gbp, decimal("1.0000000000005")).amount shouldBe decimal("1.000000000001")
    BigMoney.of(gbp, decimal("1.0000000000015")).amount shouldBe decimal("1.000000000002")
    BigMoney.of(gbp, decimal("1.0000000000004")).amount shouldBe decimal("1")
    BigMoney.of(gbp, decimal("0.1234567890123456")).amount shouldBe decimal("0.123456789012")
    // an amount already within twelve places is carried through as it stands, whatever the
    // currency's minor units say - which is the whole difference between this type and Money
    BigMoney.of(gbp, decimal("100.125")).amount shouldBe decimal("100.125")
    BigMoney.of(jpy, decimal("100.125")).amount shouldBe decimal("100.125")
    BigMoney.of(produced(Money.of(gbp, 100.125d))).amount shouldBe decimal("100.13")
    produced(BigMoney.of(gbp, 1.5d)).amount shouldBe decimal("1.5")
    BigMoney.parse("$100") should beFailureWith(FailureReason.PARSING)
    BigMoney.parse("GBP 100.125") should beSuccess
  }

  test("MultiCurrencyAmount.of rejects a duplicated currency, while total merges and the map is sorted") {
    // single-cause: the amounts are read until the first currency that repeats
    MultiCurrencyAmount.of(gbp100, amount(gbp, 200d)) should beFailureWith(FailureReason.INVALID)
    MultiCurrencyAmount.of(List(gbp100, amount(gbp, 200d))) should
      beFailureWith(FailureReason.INVALID)
    MultiCurrencyAmount.of(gbp, Double.NaN) should beFailureWith(FailureReason.INVALID)
    MultiCurrencyAmount.of(Map(gbp -> Double.NaN)) should beFailureWith(FailureReason.INVALID)

    // total is the merging factory, and is where a repeated currency is added up instead
    val merged: MultiCurrencyAmount = MultiCurrencyAmount.total(List(gbp100, amount(gbp, 200d)))
    merged.amounts.get(gbp) shouldBe Some(300d)
    merged.amounts should have size 1

    // the backing map is sorted by currency code whatever order the amounts arrive in, which is
    // what makes the rendering and the JSON of an equal value byte-stable
    produced(MultiCurrencyAmount.of(usd50, gbp100)).amounts.keys.toList shouldBe List(gbp, usd)
    produced(MultiCurrencyAmount.of(gbp100, usd50)).amounts.keys.toList shouldBe List(gbp, usd)
    produced(MultiCurrencyAmount.of(usd50, gbp100)) shouldBe
      produced(MultiCurrencyAmount.of(gbp100, usd50))
  }

  test("Decimal.of rejects a value that is not finite and normalises the scale of what it accepts") {
    Decimal.of(Double.NaN) should beFailureWith(FailureReason.INVALID)
    Decimal.of(Double.PositiveInfinity) should beFailureWith(FailureReason.INVALID)
    Decimal.of(Double.NegativeInfinity) should beFailureWith(FailureReason.INVALID)
    Decimal.of("") should beFailureWith(FailureReason.PARSING)
    Decimal.parse("not a number") should beFailureWith(FailureReason.PARSING)
    Decimal.of("1000000000000000000") should beFailureWith(FailureReason.INVALID)

    // a trailing zero of the fraction is not part of the value, so the scale is reduced to the
    // shortest that holds it - which is what makes two spellings of one value the same value
    decimal("1.2300").scale shouldBe 2
    decimal("1.2300") shouldBe decimal("1.23")
    decimal("100").scale shouldBe 0
    produced(Decimal.ofScaled(1230L, 3)) shouldBe decimal("1.23")
    produced(Decimal.ofScaled(0L, 5)) shouldBe Decimal.ZERO
  }

  test("Tenor.of rejects a period that is not positive and names what it accepts as Java named it") {
    Tenor.of(Period.ZERO) should beFailureWith(FailureReason.INVALID)
    Tenor.of(Period.ofMonths(-1)) should beFailureWith(FailureReason.INVALID)
    Tenor.ofDays(0) should beFailureWith(FailureReason.INVALID)
    Tenor.ofWeeks(-1) should beFailureWith(FailureReason.INVALID)

    accepted(Tenor.of(Period.ofMonths(3))).name shouldBe "3M"
    accepted(Tenor.of(Period.ofYears(1))).name shouldBe "1Y"
    accepted(Tenor.ofDays(7)).name shouldBe "1W"
    accepted(Tenor.ofDays(14)).name shouldBe "2W"
    accepted(Tenor.ofDays(8)).name shouldBe "8D"
    // months are never folded into years, so twelve months and one year are different tenors
    accepted(Tenor.ofMonths(12)).name shouldBe "12M"
    accepted(Tenor.ofMonths(12)) should not be accepted(Tenor.ofYears(1))

    produced(Tenor.parse("3M")) shouldBe accepted(Tenor.of(Period.ofMonths(3)))
    produced(Tenor.parse("P3M")) shouldBe produced(Tenor.parse("3M"))
    Tenor.parse("QQ") should beFailureWith(FailureReason.PARSING)
  }

  test("Frequency.of rejects a period that is not positive, canonicalises it, and keeps the P prefix") {
    Frequency.of(Period.ZERO) should beFailureWith(FailureReason.INVALID)
    Frequency.of(Period.ofMonths(-1)) should beFailureWith(FailureReason.INVALID)
    Frequency.ofMonths(0) should beFailureWith(FailureReason.INVALID)
    Frequency.ofDays(0) should beFailureWith(FailureReason.INVALID)
    // a period beyond the thousand years the factory admits - which is why `Frequency.TERM`,
    // ten thousand years long, is reachable as a constant and through `parse` but not through `of`
    Frequency.of(Period.ofYears(1001)) should beFailureWith(FailureReason.INVALID)
    Frequency.of(Frequency.TERM.period) should beFailureWith(FailureReason.INVALID)

    // the canonical name keeps the `P`, which is where this type and Tenor differ, and a year
    // canonicalises to twelve months rather than staying a year
    accepted(Frequency.of(Period.ofMonths(3))).name shouldBe "P3M"
    accepted(Frequency.of(Period.ofYears(1))) shouldBe Frequency.P12M
    accepted(Frequency.of(Period.ofYears(1))).name shouldBe "P12M"
    accepted(Frequency.ofDays(7)).name shouldBe "P1W"
    accepted(Frequency.ofDays(8)).name shouldBe "P8D"
    Frequency.TERM.name shouldBe "Term"
    produced(Frequency.parse("Term")) shouldBe Frequency.TERM
    produced(Frequency.parse("P3M")) shouldBe Frequency.P3M
    Frequency.parse("QQ") should beFailureWith(FailureReason.PARSING)
  }

  test("SequenceDate.of rejects two starting points, a backward period and a non-positive number") {
    val march2020: YearMonth = YearMonth.of(2020, 3)
    // accumulating: the three conditions are independent - see the accumulation group
    SequenceDate.of(Some(march2020), Some(Period.ofMonths(1)), 1, false) should
      beFailureWith(FailureReason.INVALID)
    SequenceDate.of(None, Some(Period.ofMonths(-1)), 1, false) should
      beFailureWith(FailureReason.INVALID)
    SequenceDate.of(None, None, 0, false) should beFailureWith(FailureReason.INVALID)
    SequenceDate.of(None, None, -1, false) should beFailureWith(FailureReason.INVALID)
    SequenceDate.base(Period.ofMonths(-1), 3) should beFailureWith(FailureReason.INVALID)
    SequenceDate.base(Period.ofDays(-1), 3) should beFailureWith(FailureReason.INVALID)
    SequenceDate.base(0) should beFailureWith(FailureReason.INVALID)
    SequenceDate.full(0) should beFailureWith(FailureReason.INVALID)

    // a minimum period of zero passes the checks and is then normalised away
    accepted(SequenceDate.of(None, Some(Period.ZERO), 1, false)).minimumPeriod shouldBe None
    accepted(SequenceDate.base(Period.ZERO, 1)).minimumPeriod shouldBe None
    accepted(SequenceDate.base(march2020)).yearMonth shouldBe Some(march2020)
    accepted(SequenceDate.full(2)).sequenceNumber shouldBe 2
  }

  test("HolidayCalendarId.of is total and normalises a composite name to sorted, unique parts") {
    // the factory has no failure channel at all: every string names an identifier, and whether
    // that identifier resolves is a question for the reference data rather than for the name
    HolidayCalendarId.of("GBLO").name shouldBe "GBLO"
    HolidayCalendarId.of("QQ").name shouldBe "QQ"

    // two orderings of the same parts are the same identifier, with the same canonical name
    HolidayCalendarId.of("USNY+GBLO") shouldBe HolidayCalendarId.of("GBLO+USNY")
    HolidayCalendarId.of("USNY+GBLO").name shouldBe "GBLO+USNY"
    HolidayCalendarId.of("USNY~GBLO") shouldBe HolidayCalendarId.of("GBLO~USNY")
    HolidayCalendarId.of("USNY~GBLO").name shouldBe "GBLO~USNY"

    // a repeated part is dropped, and a part naming no holidays is absorbed by each operator
    HolidayCalendarId.of("GBLO+GBLO").name shouldBe "GBLO"
    HolidayCalendarId.of("GBLO+USNY+GBLO").name shouldBe "GBLO+USNY"
    HolidayCalendarId.of("GBLO+NoHolidays") shouldBe HolidayCalendarIds.GBLO
    HolidayCalendarId.of("GBLO~NoHolidays") shouldBe HolidayCalendarIds.NO_HOLIDAYS
    HolidayCalendarId.of("GBLO+USNY").isComposite shouldBe true
    HolidayCalendarId.of("GBLO").isComposite shouldBe false
  }

  test("ImmutableHolidayCalendar.of is total and normalises holidays, its year range and overrides") {
    val weekend: List[DayOfWeek] = List(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY)

    // clause one: the holidays are sorted and deduplicated, in any order they arrive in
    val deduplicated: ImmutableHolidayCalendar =
      ImmutableHolidayCalendar.of(
        testCalendarId,
        List(thu20140710, wed20140709, wed20140709),
        weekend)
    deduplicated.holidays.toList shouldBe List(wed20140709, thu20140710)

    // clause two: the range of years covered is derived from the earliest and latest holiday
    deduplicated.startYear shouldBe 2014
    deduplicated.endYearExclusive shouldBe 2015
    val spanning: ImmutableHolidayCalendar =
      ImmutableHolidayCalendar.of(testCalendarId, List(date(2016, 7, 11), wed20140709), weekend)
    spanning.startYear shouldBe 2014
    spanning.endYearExclusive shouldBe 2017

    // clause three: the working days are applied last and override both the holidays and the
    // weekend - here a Friday/Saturday weekend with a Saturday declared working
    val overridden: ImmutableHolidayCalendar =
      ImmutableHolidayCalendar.of(
        testCalendarId,
        List(wed20140709, thu20140710),
        List(DayOfWeek.FRIDAY, DayOfWeek.SATURDAY),
        List(sat20140712))
    overridden.isHoliday(wed20140709) shouldBe true
    overridden.isHoliday(thu20140710) shouldBe true
    overridden.isBusinessDay(sat20140712) shouldBe true
    overridden.isBusinessDay(sun20140713) shouldBe true
    overridden.holidays.toList shouldBe List(wed20140709, thu20140710)
    overridden.weekendDays shouldBe Set(DayOfWeek.FRIDAY, DayOfWeek.SATURDAY)
    overridden.workingDays.toList shouldBe List(sat20140712)

    val holidayOverridden: ImmutableHolidayCalendar =
      ImmutableHolidayCalendar.of(
        testCalendarId,
        List(wed20140709, thu20140710),
        weekend,
        List(wed20140709))
    holidayOverridden.isBusinessDay(wed20140709) shouldBe true
    holidayOverridden.isHoliday(thu20140710) shouldBe true
    holidayOverridden.holidays.toList shouldBe List(thu20140710)

    // clause four: a working day outside the range the holidays span has no data to override
    val outsideRange: ImmutableHolidayCalendar =
      ImmutableHolidayCalendar.of(testCalendarId, List(wed20140709), weekend, List(sat20130713))
    outsideRange.isHoliday(sat20130713) shouldBe true
    outsideRange.workingDays shouldBe empty

    // equality is by identifier alone, which is why each clause compares holiday sets
    deduplicated shouldBe overridden
    deduplicated.holidays should not be overridden.workingDays
  }

  test("RollConvention.ofDayOfMonth maps thirty-one to end of month and rejects a day outside the month") {
    produced(RollConvention.ofDayOfMonth(1)) shouldBe RollConventions.DAY_1
    produced(RollConvention.ofDayOfMonth(15)) shouldBe RollConventions.DAY_15
    produced(RollConvention.ofDayOfMonth(30)) shouldBe RollConventions.DAY_30
    // thirty-one is not a day-of-month convention: it is the end-of-month convention, because a
    // month that has no thirty-first rolls to its last day instead
    produced(RollConvention.ofDayOfMonth(31)) shouldBe RollConventions.EOM
    RollConvention.ofDayOfMonth(0) should beFailureWith(FailureReason.INVALID)
    RollConvention.ofDayOfMonth(32) should beFailureWith(FailureReason.INVALID)
    RollConvention.ofDayOfMonth(-1) should beFailureWith(FailureReason.INVALID)
    // the day-of-week form is total: every day of the week names a convention
    RollConvention.ofDayOfWeek(DayOfWeek.MONDAY).name shouldBe "DayMon"
    RollConvention.ofDayOfWeek(DayOfWeek.SUNDAY).name shouldBe "DaySun"
  }

  //-------------------------------------------------------------------------
  // Group three: accumulation, over the nine factories given an input wrong in two ways at once.
  //
  // Each compares the chain of the multi-cause call with the chains of the calls that have one
  // cause each: when every single-cause message is present and the count matches, no cause has
  // been dropped and none substituted. The fixtures report the same values either way, since the
  // wording of an order or range failure names the values it rejected.
  //-------------------------------------------------------------------------
  test("StandardId.of accumulates the failure of its scheme and the failure of its value") {
    val both: ResultNec[StandardId] = StandardId.of("", "")
    failuresOf(both) should have size 2
    reasonsOf(both) shouldBe List(FailureReason.INVALID, FailureReason.INVALID)
    messagesOf(both) should contain allElementsOf messagesOf(StandardId.of("", "AAPL"))
    messagesOf(both) should contain allElementsOf messagesOf(StandardId.of("OG-Ticker", ""))
    messagesOf(both).distinct should have size 2
  }

  test("FxRate.of accumulates the failure of its rate and the failure of its identical pair") {
    val both: ResultNec[FxRate] = FxRate.of(gbpGbp, -1.5d)
    failuresOf(both) should have size 2
    reasonsOf(both).distinct shouldBe List(FailureReason.INVALID)
    messagesOf(both) should contain allElementsOf messagesOf(FxRate.of(gbpUsd, -1.5d))
    messagesOf(both) should contain allElementsOf messagesOf(FxRate.of(gbpGbp, 2d))
    messagesOf(both).distinct should have size 2
  }

  test("MultiCurrencyAmountArray.of accumulates one element failure per offending currency") {
    // Two currencies, each holding a value that is not a number at a different index. The map is
    // written counter-first so that the order of the reasons cannot come from the order of the
    // input: what comes back is ordered by currency code, which is the order the run itself
    // holds its currencies in, so a caller comparing two reports - or a document decoded twice -
    // reads the same reasons in the same order however the map reached the factory.
    val both: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.of(
        Map(usd -> DoubleArray.of(1d, Double.NaN), gbp -> DoubleArray.of(Double.NaN, 2d)))
    failuresOf(both) should have size 2
    reasonsOf(both) shouldBe List(FailureReason.INVALID, FailureReason.INVALID)
    messagesOf(both) shouldBe List(
      "Argument 'values' for GBP must not be NaN at index 0",
      "Argument 'values' for USD must not be NaN at index 1")

    // and an element failure accumulates beside the structural one rather than displacing it,
    // which is what says the examination is a check of this factory and not a gate before it
    val withLengths: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.of(
        Map(gbp -> DoubleArray.of(Double.NaN), usd -> DoubleArray.of(1d, 2d)))
    failuresOf(withLengths) should have size 2
    messagesOf(withLengths) should contain("Argument 'values' for GBP must not be NaN at index 0")
    messagesOf(withLengths) should contain allElementsOf
      messagesOf(
        MultiCurrencyAmountArray.of(
          Map(gbp -> DoubleArray.of(1d), usd -> DoubleArray.of(1d, 2d))))
    messagesOf(withLengths).distinct should have size 2
  }

  test("FxMatrix.fromMatrix accumulates all three of its structural failures") {
    val allThree: ResultNec[FxMatrix] =
      FxMatrix.fromMatrix(Vector(gbp, gbp), DoubleMatrix.of(2, 3, 2d, 0d, 0d, 0d, 2d, 0d))
    failuresOf(allThree) should have size 3
    reasonsOf(allThree).distinct shouldBe List(FailureReason.INVALID)
    messagesOf(allThree) should contain allElementsOf
      messagesOf(FxMatrix.fromMatrix(Vector(gbp, gbp), DoubleMatrix.identity(2)))
    messagesOf(allThree) should contain allElementsOf
      messagesOf(
        FxMatrix.fromMatrix(Vector(gbp, usd), DoubleMatrix.of(2, 3, 1d, 1.25d, 0d, 0.8d, 1d, 0d)))
    messagesOf(allThree) should contain allElementsOf
      messagesOf(FxMatrix.fromMatrix(Vector(gbp, usd), DoubleMatrix.of(2, 2, 2d, 1.25d, 0.8d, 1d)))
    messagesOf(allThree).distinct should have size 3
  }

  test("SchedulePeriod.of accumulates the order of its unadjusted dates and of its adjusted dates") {
    val both: ResultNec[SchedulePeriod] = SchedulePeriod.of(jul18, jul04)
    failuresOf(both) should have size 2
    reasonsOf(both).distinct shouldBe List(FailureReason.INVALID)
    messagesOf(both) should contain allElementsOf
      messagesOf(SchedulePeriod.of(jul04, jul18, jul18, jul04))
    messagesOf(both) should contain allElementsOf
      messagesOf(SchedulePeriod.of(jul18, jul04, jul04, jul18))
    messagesOf(both).distinct should have size 2
  }

  test("PeriodicSchedule.of accumulates every date-order invariant that a definition breaks") {
    val firstRegular: Option[LocalDate] = Some(date(2015, 1, 1))
    val both: ResultNec[PeriodicSchedule] =
      PeriodicSchedule.of(
        jul18,
        jul04,
        Frequency.P1M,
        BusinessDayAdjustment.NONE,
        None,
        None,
        None,
        None,
        firstRegular,
        None,
        None)
    failuresOf(both) should have size 2
    reasonsOf(both).distinct shouldBe List(FailureReason.INVALID)
    // the start-date failure on its own, over the same pair of dates
    messagesOf(both) should contain allElementsOf
      messagesOf(PeriodicSchedule.of(jul18, jul04, Frequency.P1M, BusinessDayAdjustment.NONE))
    // the first-regular-date failure on its own, over the same pair of dates
    messagesOf(both) should contain allElementsOf
      messagesOf(
        PeriodicSchedule.of(
          date(2014, 7, 1),
          jul04,
          Frequency.P1M,
          BusinessDayAdjustment.NONE,
          None,
          None,
          None,
          None,
          firstRegular,
          None,
          None))
    messagesOf(both).distinct should have size 2
  }

  test("ValueStep.of accumulates the failure of its position and the failure of its index") {
    val adjustment: ValueAdjustment = ValueAdjustment.ofDeltaAmount(-2000d)
    val both: ResultNec[ValueStep] = ValueStep.of(Some(0), Some(jul04), adjustment)
    failuresOf(both) should have size 2
    reasonsOf(both).distinct shouldBe List(FailureReason.INVALID)
    messagesOf(both) should contain allElementsOf
      messagesOf(ValueStep.of(Some(1), Some(jul04), adjustment))
    messagesOf(both) should contain allElementsOf messagesOf(ValueStep.of(Some(0), None, adjustment))
    messagesOf(both).distinct should have size 2
  }

  test("ValueStepSequence.of accumulates its date order and its disallowed adjustment") {
    val delta: ValueAdjustment = ValueAdjustment.ofDeltaAmount(-100d)
    val replace: ValueAdjustment = ValueAdjustment.ofReplace(100d)
    val both: ResultNec[ValueStepSequence] =
      ValueStepSequence.of(jul18, jul04, Frequency.P1M, replace)
    failuresOf(both) should have size 2
    reasonsOf(both).distinct shouldBe List(FailureReason.INVALID)
    messagesOf(both) should contain allElementsOf
      messagesOf(ValueStepSequence.of(jul18, jul04, Frequency.P1M, delta))
    messagesOf(both) should contain allElementsOf
      messagesOf(ValueStepSequence.of(jul04, jul18, Frequency.P1M, replace))
    messagesOf(both).distinct should have size 2
  }

  test("HalfUp.ofFractionalDecimalPlaces accumulates the failure of each of its two inputs") {
    val both: ResultNec[HalfUp] = HalfUp.ofFractionalDecimalPlaces(-1, 257)
    failuresOf(both) should have size 2
    reasonsOf(both).distinct shouldBe List(FailureReason.INVALID)
    messagesOf(both) should contain allElementsOf
      messagesOf(HalfUp.ofFractionalDecimalPlaces(-1, 0))
    messagesOf(both) should contain allElementsOf
      messagesOf(HalfUp.ofFractionalDecimalPlaces(0, 257))
    messagesOf(both).distinct should have size 2
  }

  test("SequenceDate.of accumulates all three of its independent failures") {
    val march2020: YearMonth = YearMonth.of(2020, 3)
    val allThree: ResultNec[SequenceDate] =
      SequenceDate.of(Some(march2020), Some(Period.ofMonths(-1)), 0, false)
    failuresOf(allThree) should have size 3
    reasonsOf(allThree).distinct shouldBe List(FailureReason.INVALID)
    messagesOf(allThree) should contain allElementsOf
      messagesOf(SequenceDate.of(Some(march2020), Some(Period.ofMonths(1)), 1, false))
    messagesOf(allThree) should contain allElementsOf
      messagesOf(SequenceDate.of(None, Some(Period.ofMonths(-1)), 1, false))
    messagesOf(allThree) should contain allElementsOf
      messagesOf(SequenceDate.of(None, None, 0, false))
    messagesOf(allThree).distinct should have size 3
  }

  //-------------------------------------------------------------------------
  // Group four: the idempotence of normalisation, one property for each of the eleven entries of
  // the `[N]` list. Each puts a value a factory produced back through that factory - through its
  // accessors, or through its canonical name where the factory takes a number - and claims the
  // result equals it: the guarantee a decoder, a `with*` method and the JSON round trip all rest
  // on. The values are drawn from the module's `Arbitraries`, which build through these factories.
  //-------------------------------------------------------------------------
  test("normalisation is idempotent: CurrencyAmount") {
    forAll { (value: CurrencyAmount) =>
      CurrencyAmount.of(value.currency, value.amount) should haveValue(value)
    }
  }

  test("normalisation is idempotent: Money") {
    forAll { (value: Money) =>
      Money.of(value.currency, value.amount) shouldBe value
    }
  }

  test("normalisation is idempotent: BigMoney") {
    forAll { (value: BigMoney) =>
      BigMoney.of(value.currency, value.amount) shouldBe value
    }
  }

  test("normalisation is idempotent: MultiCurrencyAmount") {
    forAll { (value: MultiCurrencyAmount) =>
      MultiCurrencyAmount.of(value.amounts) should haveValue(value)
    }
  }

  test("normalisation is idempotent: Decimal") {
    forAll { (value: Decimal) =>
      Decimal.ofScaled(value.unscaled, value.scale) should haveValue(value)
    }
  }

  test("normalisation is idempotent: Tenor") {
    forAll { (value: Tenor) =>
      Tenor.of(value.period) should haveValue(value)
    }
  }

  test("normalisation is idempotent: Frequency") {
    forAll { (value: Frequency) =>
      if (value.isTerm) {
        // The one frequency outside the range of its own period-taking factory: 'Term' is ten
        // thousand years long and `of` admits a thousand, so its idempotence runs through its
        // name, the route `parse` and the JSON codec take, rather than through its period.
        Frequency.of(value.period) should beFailureWith(FailureReason.INVALID)
        Frequency.parse(value.name) should haveValue(value)
      } else {
        Frequency.of(value.period) should haveValue(value)
      }
    }
  }

  test("normalisation is idempotent: SequenceDate") {
    forAll { (value: SequenceDate) =>
      SequenceDate.of(
        value.yearMonth,
        value.minimumPeriod,
        value.sequenceNumber,
        value.fullSequence) should haveValue(value)
    }
  }

  test("normalisation is idempotent: HolidayCalendarId") {
    forAll { (value: HolidayCalendarId) =>
      HolidayCalendarId.of(value.name) shouldBe value
      HolidayCalendarId.of(value.name).name shouldBe value.name
    }
  }

  test("normalisation is idempotent: ImmutableHolidayCalendar") {
    // Equality of a calendar is by identifier alone, so comparing calendars would prove nothing
    // here: the claim is about content - holidays, weekend, overrides and the year range spanned
    forAll { (value: ImmutableHolidayCalendar) =>
      val once: ImmutableHolidayCalendar = rebuilt(value)
      val twice: ImmutableHolidayCalendar = rebuilt(once)
      twice.holidays shouldBe once.holidays
      twice.weekendDays shouldBe once.weekendDays
      twice.workingDays shouldBe once.workingDays
      twice.startYear shouldBe once.startYear
      twice.endYearExclusive shouldBe once.endYearExclusive
      twice.id shouldBe once.id
    }
  }

  test("normalisation is idempotent: RollConvention.ofDayOfMonth") {
    forAll(Gen.choose(1, 31)) { (dayOfMonth: Int) =>
      val convention: RollConvention = produced(RollConvention.ofDayOfMonth(dayOfMonth))
      // a factory taking a number rather than its own output round-trips through its name
      RollConvention.parse(convention.name) should haveValue(convention)
    }
  }

  //-------------------------------------------------------------------------
  // Group five: the numeric-domain edges that stay throws.
  //
  // AAP section 0.3.3 records a deliberate narrowing here, and `SCALA_MIGRATION.md` lists it as
  // a divergence: the arithmetic of the domain types stays total in signature, so the places
  // where a numeric domain has no answer raise a broken precondition through `ArgCheck` rather
  // than returning a failure. The `intercept` calls of this group are the only ones in this file
  // that stand for a rejection of data, and they are deliberate. A data-dependent rejection
  // anywhere else in this module must be an `Either`, which is what the rest of this file
  // asserts; the closure group at the end of the file intercepts guards a caller reaches only
  // from another language, which reject no data at all.
  //
  // Do not "fix" any of these into an outcome: a caller adding two ordinary amounts, or two
  // ordinary decimals, cannot reach the check, and giving the operator a failure channel would
  // cost every such caller an unwrapping for a case its values cannot produce.
  //
  // The third test of the group is the same rule applied to the element of a run rather than to
  // the result of an operator. The two amount arrays hold a value per position, each of which is
  // an amount of the module's own amount type, and that type has no value that is not a number -
  // so a run holding one describes nothing, and admitting it only moves the failure to whichever
  // later read happens to touch that position. The check therefore belongs at the one point that
  // builds a run, and what it does there is decided by the signature AAP section 0.4.1 fixed for
  // the route reaching it: the routes named total raise, and every route that already answers
  // with an outcome reports, which the groups above assert.
  //-------------------------------------------------------------------------
  test("CurrencyAmount arithmetic that produces a value that is not a number raises its invariant") {
    val positive: CurrencyAmount = produced(CurrencyAmount.of(gbp, Double.PositiveInfinity))
    val negative: CurrencyAmount = produced(CurrencyAmount.of(gbp, Double.NegativeInfinity))

    intercept[IllegalArgumentException](positive.plus(negative))
    intercept[IllegalArgumentException](positive.plus(Double.NegativeInfinity))
    intercept[IllegalArgumentException](positive.minus(Double.PositiveInfinity))
    intercept[IllegalArgumentException](positive.multipliedBy(0d))

    // everything short of that domain edge is ordinary arithmetic, and stays a value
    positive.plus(1d).amount shouldBe Double.PositiveInfinity
    produced(positive.plus(positive)).amount shouldBe Double.PositiveInfinity
    gbp100.plus(usd50) should beFailureWith(FailureReason.INVALID)
    produced(gbp100.plus(gbp100)).amount shouldBe 200d
  }

  test("Decimal arithmetic beyond eighteen digits of precision raises its invariant") {
    intercept[IllegalArgumentException](Decimal.MAX_VALUE.plus(Decimal.MAX_VALUE))
    intercept[IllegalArgumentException](Decimal.MIN_VALUE.minus(Decimal.MAX_VALUE))
    intercept[IllegalArgumentException](Decimal.MAX_VALUE.multipliedBy(10L))

    // the factory, by contrast, reports a value beyond the precision of the type as a failure,
    // because there the value came from outside rather than from the type's own arithmetic
    Decimal.of("1000000000000000000") should beFailureWith(FailureReason.INVALID)
    Decimal.MAX_VALUE.plus(Decimal.ZERO) shouldBe Decimal.MAX_VALUE
  }

  test("the two amount arrays raise their element invariant where their route is a total one") {
    // A run holding an infinity, which is a value both types hold: it is the fixture the three
    // total routes are driven with, because an operator applied to it can produce the one value
    // they do not hold without anything invalid having been supplied.
    val infinite: CurrencyAmountArray =
      CurrencyAmountArray.of(gbp, DoubleArray.of(Double.PositiveInfinity))

    // the factory that states a run, and the two transforms, are total in AAP section 0.4.1, so
    // each names the index it refused rather than answering with a run that has no amount there
    intercept[IllegalArgumentException](
      CurrencyAmountArray.of(gbp, DoubleArray.of(1d, Double.NaN))
    ).getMessage should include("Argument 'values' must not be NaN at index 1")
    intercept[IllegalArgumentException](infinite.multipliedBy(0d)) // documented ArgCheck throw
      .getMessage should include("Argument 'values' must not be NaN at index 0")
    intercept[IllegalArgumentException](infinite.mapAmounts(_ => Double.NaN)) // documented
      .getMessage should include("Argument 'values' must not be NaN at index 0")

    // the run of several currencies says which currency as well as which index, because a run of
    // a dozen currencies says nothing about which of them carries the value that was refused
    val infiniteRun: MultiCurrencyAmountArray =
      accepted(MultiCurrencyAmountArray.of(Map(gbp -> DoubleArray.of(Double.PositiveInfinity))))
    intercept[IllegalArgumentException](infiniteRun.multipliedBy(0d)) // documented
      .getMessage should include("Argument 'values' for GBP must not be NaN at index 0")
    intercept[IllegalArgumentException](infiniteRun.mapAmounts(_ => Double.NaN)) // documented
      .getMessage should include("Argument 'values' for GBP must not be NaN at index 0")

    // and the counterpart, which is what makes the raises above a choice rather than the only
    // thing available: converting a run collapses its currencies into a run of the other type,
    // and opposed infinities in two of them produce the value neither type holds - reported,
    // because that member answers with an outcome, and reported by the type that would hold it
    val opposedRun: MultiCurrencyAmountArray = accepted(
      MultiCurrencyAmountArray.of(
        Map(
          gbp -> DoubleArray.of(Double.PositiveInfinity),
          usd -> DoubleArray.of(Double.NegativeInfinity))))
    val converted: FailureOr[CurrencyAmountArray] =
      opposedRun.convertedTo(gbp, accepted(FxRate.of(gbpUsd, 1d)))
    converted should beFailureWith(FailureReason.INVALID)
    failureOf(converted).message shouldBe "Argument 'values' must not be NaN at index 0"

    // ordinary values pass through every one of those routes untouched
    infinite.multipliedBy(2d).values shouldBe DoubleArray.of(Double.PositiveInfinity)
    CurrencyAmountArray.of(gbp, DoubleArray.of(1d, 2d)).multipliedBy(3d).values shouldBe
      DoubleArray.of(3d, 6d)
    accepted(MultiCurrencyAmountArray.of(Map(gbp -> DoubleArray.of(1d, 2d))))
      .mapAmounts(value => value + 1d)
      .getValues(gbp) shouldBe Right(DoubleArray.of(2d, 3d))
  }

  //-------------------------------------------------------------------------
  // Group six: what the text factories decide before they transform their input.
  //
  // Every factory above is given values; these are given text, and text arrives from outside the
  // process - a document, a request, a file - at whatever length its sender chose. A factory that
  // transforms first and checks afterwards therefore does work proportional to its input before
  // it has any reason to believe the input is of its grammar at all: splitting on every separator
  // of a million-separator string, case-folding a ten-megabyte blob, or decomposing a composite
  // identifier into a hundred thousand parts, in every case only to discard the result at the
  // very next comparison (CWE-400/CWE-770). This group asserts the opposite order of work, and
  // does so for the types of the two lists above together with the four whose text is currency
  // text: the concern is a property of the text factories rather than of one construction kind,
  // and asserting it type by type would scatter one rule across six tests that share it.
  //
  // Two things are asserted, and the second is what keeps the first honest:
  //
  //  - The bound, by the message it reports. Three of these grammars have a longest spelling, so
  //    text beyond it is refused naming the bound and quoting nothing - the wording of `Decimal`,
  //    which has had a bound of the same kind from the start. The fixed-shape grammars have no
  //    bound to name: their whole decision is a comparison of a length or the position of one
  //    separator, so they refuse the text they were given and quote it as they always have, which
  //    is what the specs of those types pin and what this group leaves alone.
  //  - That the bound is a bound on WORK and not a narrowing of the grammar. Upper-casing text
  //    can lengthen it - a sweep of every code point of this runtime finds seventy-two characters
  //    whose upper case is longer than themselves, and none whose upper case is shorter or
  //    introduces a separator - so a fixed-shape factory that tested its length for EQUALITY
  //    before folding would refuse text whose folded form is exactly of its grammar. That is not
  //    hypothetical: `FIM` is one of this module's currencies and `\ufb01M` is two characters that
  //    fold to it. Each of these factories therefore tests an upper bound, folds, and matches -
  //    and the second test below is the guard that stops the cheaper-looking equality from coming
  //    back, since every other test of the module spells its input in the alphabet of its grammar
  //    and would not notice.
  //-------------------------------------------------------------------------
  test("the text factories bound the work their input can cause before they do any of it") {
    // the three grammars with a longest spelling: beyond it the bound is named and nothing is
    // quoted, so a rejection cannot be made to carry the text that caused it
    val overLong: String = "P" * (MaxPeriodTextLength + 1)
    Tenor.parse(overLong) should beFailureWith(FailureReason.PARSING)
    failureOf(Tenor.parse(overLong)).message shouldBe
      s"Tenor string must not exceed $MaxPeriodTextLength characters"
    failureOf(Frequency.parse(overLong)).message shouldBe
      s"Frequency string must not exceed $MaxPeriodTextLength characters"
    failureOf(MarketTenor.parse(overLong)).message shouldBe
      s"Market tenor string must not exceed $MaxPeriodTextLength characters"

    // at the bound the text reaches the grammar and is refused by the grammar, which is what
    // says the bound is above every spelling the grammar has rather than inside it
    val atBound: String = "P" * MaxPeriodTextLength
    failureOf(Tenor.parse(atBound)).message should not include "must not exceed"
    failureOf(Frequency.parse(atBound)).message should not include "must not exceed"
    failureOf(MarketTenor.parse(atBound)).message should not include "must not exceed"

    // the two parts of an identifier, each bounded on its own and both reported at once
    val overLongPart: String = "A" * (MaxIdentifierPartLength + 1)
    messagesOf(StandardId.of(overLongPart, "AAPL")) shouldBe
      List(s"Argument 'scheme' must not exceed $MaxIdentifierPartLength characters")
    messagesOf(StandardId.of("OG-Ticker", overLongPart)) shouldBe
      List(s"Argument 'value' must not exceed $MaxIdentifierPartLength characters")
    messagesOf(StandardId.of(overLongPart, overLongPart)) should have size 2

    // and the whole text, whose ceiling is derived from the part ceiling rather than equal to
    // it: an identifier renders as `scheme~value`, so the longest text the factories can produce
    // is two parts and their separator, and `parse` has to read every one of them back
    val overLongText: String = "A" * (MaxIdentifierTextLength + 1)
    failureOf(StandardId.parse(overLongText)).message shouldBe
      s"Identifier string must not exceed $MaxIdentifierTextLength characters"
    val maximalPart: String = "A" * MaxIdentifierPartLength
    val maximal: StandardId = accepted(StandardId.of(maximalPart, maximalPart))
    maximal.toString.length shouldBe MaxIdentifierTextLength
    StandardId.parse(maximal.toString) shouldBe Right(maximal)

    // the amount of a currency amount, bounded before the text is cut in two or read as a
    // number: a well-formed prefix followed by a tail of a sender's choosing is decided from the
    // length and the separator alone, and reaches the wording a text that names no amount has
    // always reached rather than a wording of its own
    val longAmount: String = s"GBP 0.${"0" * MaxAmountTextLength}"
    failureOf(CurrencyAmount.parse(longAmount)).message should startWith("Unable to parse amount:")
    failureOf(CurrencyAmount.parse(s"GBP ${"H" * 4096} ")).message should
      startWith("Unable to parse amount, invalid format:")
    // at the bound the amount is read exactly as it always was, zeroes and all
    produced(CurrencyAmount.parse(s"GBP 0.${"0" * (MaxAmountTextLength - 2)}")).amount shouldBe 0d
    produced(CurrencyAmount.parse("GBP 12.34")).amount shouldBe 12.34d

    // the same of the rate of an exchange rate, whose group the expression lets run to any
    // length, and which is therefore measured before the text is folded at all
    val longRate: String = s"EUR/GBP 1.${"0" * MaxAmountTextLength}"
    failureOf(FxRate.parse(longRate)).message should startWith("Unable to parse rate:")
    produced(FxRate.parse(s"EUR/GBP 1.${"0" * (MaxAmountTextLength - 2)}")).toString shouldBe
      "EUR/GBP 1"

    // the two money parsers decide their shape from the position of the first separator and the
    // absence of a second, so text made of separators is refused without a part being built
    failureOf(Money.parse("GBP 12.34 56")).message should include("invalid format")
    failureOf(Money.parse(" " * 4096)).message should include("invalid format")
    failureOf(BigMoney.parse(" " * 4096)).message should include("invalid format")
    // and the trailing separator the ported splitter kept is still two parts, so it is refused
    // for the decimal it does not name rather than for the shape it does have
    failureOf(Money.parse("GBP ")).message should not include "invalid format"
    produced(Money.parse("GBP 12.34")).toString shouldBe "GBP 12.34"

    // their numeral is bounded at the ceiling their decimal already applies, moved ahead of the
    // copy rather than left behind it, so what is accepted is unchanged and the tail of a
    // hostile text is no longer copied in order to be measured
    val longDecimal: String = s"GBP 0.${"0" * (MaxDecimalTextLength - 1)}"
    failureOf(Money.parse(longDecimal)).message should startWith("Unable to parse amount:")
    failureOf(BigMoney.parse(longDecimal)).message should startWith("Unable to parse amount:")
    produced(Money.parse(s"GBP 0.${"0" * (MaxDecimalTextLength - 2)}")).toString shouldBe "GBP 0.00"
    produced(BigMoney.parse(s"GBP 0.${"0" * (MaxDecimalTextLength - 2)}")).toString shouldBe
      "GBP 0.00"

    // the composite identifier caps the work rather than the input, because its factory is total
    // in AAP section 0.4.1 and both the codec and the key decoder of this module depend on that:
    // a name beyond either cap is kept whole, which is a name that resolves against no data
    val overLongComposite: String = "GBLO+" * 20000
    val capped: HolidayCalendarId = HolidayCalendarId.of(overLongComposite)
    capped.isComposite shouldBe false
    capped.name shouldBe overLongComposite
    capped.resolve(standardData) should beFailureWith(FailureReason.MISSING_DATA)
    // while a composite within the caps decomposes and normalises exactly as it always has
    HolidayCalendarId.of("USNY+GBLO").name shouldBe "GBLO+USNY"
    HolidayCalendarId.of(("GBLO+" * 2000) + "USNY").name shouldBe "GBLO+USNY"
    produced(HolidayCalendarIds.GBLO.combinedWith(HolidayCalendarIds.USNY).resolve(standardData))
      .id shouldBe HolidayCalendarId.of("GBLO+USNY")
  }

  test("the text factories fold their input before they match it, so folding that lengthens parses") {
    // "\ufb01" is one character that upper-cases to "FI", so "\ufb01M" is two characters whose
    // folded form is the three-letter code of a currency this module defines. An equality test on
    // the length before the fold refuses it; an upper bound admits it, and the grammar decides.
    Currency.parse("\ufb01M").map(currency => currency.code) shouldBe Right("FIM")
    CurrencyPair.parse("\ufb01M/USD").map(pair => pair.toString) shouldBe Right("FIM/USD")
    FxRate.parse("\ufb01M/USD 1.25").map(rate => rate.toString) shouldBe Right("FIM/USD 1.25")
    // the same of the two-letter grammar, where three separate characters reach a defined country
    Country.parse("\u00df").map(country => country.code) shouldBe Right("SS")
    Country.parse("\ufb01").map(country => country.code) shouldBe Right("FI")
    Country.parse("\ufb05").map(country => country.code) shouldBe Right("ST")

    // the ordinary spellings are untouched, in both cases of either alphabet
    Currency.parse("eur").map(currency => currency.code) shouldBe Right("EUR")
    Currency.parse("EUR").map(currency => currency.code) shouldBe Right("EUR")
    CurrencyPair.parse("eur/usd").map(pair => pair.toString) shouldBe Right("EUR/USD")
    FxRate.parse("eur/usd 1.25").map(rate => rate.toString) shouldBe Right("EUR/USD 1.25")
    Country.parse("gb").map(country => country.code) shouldBe Right("GB")

    // and the grammar still decides everything it decided before: text of the right length that
    // is not a code, and text whose separator is in the wrong place, are refused as they were
    Currency.parse("ZYX") should beFailureWith(FailureReason.PARSING)
    CurrencyPair.parse("EURUSD") should beFailureWith(FailureReason.PARSING)
    FxRate.parse("EUR/USD1.25") should beFailureWith(FailureReason.PARSING)
    Country.parse("A1") should beFailureWith(FailureReason.INVALID)
  }

  //-------------------------------------------------------------------------
  // Group seven: the closure of the construction paths every group above asserts over.
  //
  // A rejection by a factory is a statement about the values that can exist only while the factory
  // is the only way in. In the source it is: a validated or normalising type is represented as
  // `sealed abstract case class X private (...)`, which generates no `apply` and no `copy` and
  // cannot be extended from another file, and `ApiSurfaceSpec` holds each of those against the
  // compiler.
  //
  // None of that survives into the class file. `sealed` has no bytecode form in this language
  // version and a `private` constructor is emitted public, so a class compiled against these class
  // files by another language could extend the type and carry whatever fields it liked; and the
  // compiler's product encoding gives every one of these types a `java.io.Serializable` supertype,
  // so `java.io.ObjectInputStream` could populate those fields from a stream. Either route
  // produces a value of the type holding exactly the input the groups above assert is refused -
  // an amount that is not a number, a period that is not positive, a scale beyond the precision
  // of its decimal.
  //
  // Both routes are closed, and this group asserts the mechanism that closes them over values the
  // factories above built: the base class of each type runs a guard admitting only the one
  // implementation its companion declares, and the type refuses Java serialization on the way out
  // and on the way back in. The exhaustive halves are elsewhere and named here so that a reader
  // can find them: `ApiSurfaceSpec` audits all 33 validated and normalising types and every
  // product both modules compile to, `NamedEnumClosedSpec` audits every member of every closed
  // family, and the acceptance gate compiles the attack in Java and runs it.
  //-------------------------------------------------------------------------
  test("the values these factories build are the only instances of their types that can exist") {
    val built: List[(String, AnyRef)] =
      List(
        "CurrencyAmount" -> gbp100,
        "Money" -> produced(Money.of(gbp, 12.34)),
        "FxRate" -> accepted(FxRate.of(gbpUsd, 1.25)),
        "Decimal" -> decimal("12.345"),
        "FixedScaleDecimal" -> accepted(FixedScaleDecimal.of(decimal("12.3"), 3)),
        "Tenor" -> accepted(Tenor.of(Period.ofMonths(3))),
        "Frequency" -> accepted(Frequency.of(Period.ofMonths(3))),
        "HolidayCalendarId" -> testCalendarId,
        "SchedulePeriod" -> accepted(SchedulePeriod.of(jul04, jul18)),
        "HalfUp" -> accepted(HalfUp.ofDecimalPlaces(4)))
    built.map { case (subject, _) => subject }.distinct should have size built.size.toLong

    built.foreach {
      case (subject, value) =>
        val implementation: Class[_] = value.getClass
        val foreign: IllegalArgumentException =
          intercept[IllegalArgumentException](
            JvmClosure.requireSoleImplementation(new AnyRef, implementation))
        val written: IllegalArgumentException =
          intercept[IllegalArgumentException] {
            Using.resource(new ObjectOutputStream(new ByteArrayOutputStream()))(stream =>
              stream.writeObject(value))
          }
        val read: InvocationTargetException =
          intercept[InvocationTargetException](implementation.getMethod("readResolve").invoke(value))

        withClue(s"$subject admits no implementation other than the one it publishes: ")(
          foreign.getMessage should include("admits only the implementation it publishes"))
        withClue(s"$subject refuses to be written by java.io.ObjectOutputStream: ")(
          written.getMessage should include("Java serialization is not supported by this library"))
        withClue(s"$subject refuses the read hook java.io.ObjectInputStream would call: ")(
          read.getCause.getMessage should include(
            "Java serialization is not supported by this library"))
        withClue(s"$subject is the one hidden implementation its companion declares: ") {
          Modifier.isPrivate(implementation.getModifiers) shouldBe true
          Modifier.isFinal(implementation.getModifiers) shouldBe true
          // and the guard the type's own constructor ran for this value admits it, which is why
          // the factory above could answer with it at all
          JvmClosure.requireSoleImplementation(value, implementation)
        }
    }
  }
}


/**
 * The two coverage lists of [[SmartConstructorSpec]] and the one attribute name it asserts.
 *
 * The lists are what the suite's first two tests count, print and tie to its tests; they live in
 * the companion so that the names are stated once.
 */
object SmartConstructorSpec {

  /**
   * The attribute name a rejected schedule definition carries.
   *
   * The production code holds this name privately - it is the `definition` key of
   * [[com.opengamma.strata.basics.schedule.PeriodicSchedule]] - so the spec states it
   * independently, and the two drifting apart fails the assertion that reads it.
   */
  private val DefinitionAttribute: String = "definition"

  /**
   * The longest text the three period grammars accept, stated independently of them.
   *
   * `Tenor`, `Frequency` and `MarketTenor` each hold this bound privately, and each names it in
   * the failure it reports, so the suite states it here and compares the whole wording rather
   * than looking for a fragment of it. A bound changed on one side alone therefore fails, which
   * is the point: the number is part of what those factories promise a caller, not an internal
   * detail - a document holding a spelling of a tenor is either inside it or refused.
   *
   * The value is far above every spelling the grammars have. The longest tenor this module names
   * is five characters and the longest period a caller can write is a dozen, so the bound is not
   * a limit anything legitimate meets; it is a ceiling on the work a sender can ask for.
   */
  private val MaxPeriodTextLength: Int = 256

  /**
   * The longest scheme, value, or whole identifier that [[StandardId]] accepts.
   *
   * Stated here for the same reason as the bound above, and separate from it because the two
   * bound different things: a period grammar has a longest spelling and this one does not - an
   * identifier's value is whatever the scheme that issued it says - so this ceiling is set where
   * no identifier in use can reach it while a sender still cannot ask for unbounded work.
   */
  private val MaxIdentifierPartLength: Int = 65536

  /**
   * The longest text [[com.opengamma.strata.basics.StandardId.parse]] reads.
   *
   * Derived from the bound above the way the type derives it, and stated as the computation
   * rather than as a number so that the relation is what the suite asserts: an identifier renders
   * as `scheme~value`, so the longest text the factories can produce is two parts at their
   * ceiling and the separator between them. A text ceiling lower than this would refuse the
   * rendering of a value the factories admit, breaking the inverse the type documents and the
   * codec round trip AAP section 0.6.4 requires rather than bounding them.
   */
  private val MaxIdentifierTextLength: Int = 2 * MaxIdentifierPartLength + 1

  /**
   * The longest the numeral of an amount, or of an exchange rate, may be as text.
   *
   * Both types read their number as a double, which has no longest spelling, so each states this
   * bound and tests it before copying the numeral out of the text or reading it. The value is
   * what it takes to write a double exactly - the smallest subnormal needs 767 significant
   * digits and every other value fewer - so nothing that names a number exactly is refused for
   * its size.
   */
  private val MaxAmountTextLength: Int = 1024

  /**
   * The longest numeral [[com.opengamma.strata.collect.Decimal]] reads, which
   * [[com.opengamma.strata.basics.currency.Money]] and
   * [[com.opengamma.strata.basics.currency.BigMoney]] restate ahead of their own copy.
   *
   * Lower than the bound above because those two types hold a decimal of eighteen digits rather
   * than a double, so the text that can name one of their values is shorter. Applying it before
   * the numeral is copied changes nothing about what they accept - the decimal refused the same
   * text, one copy later - which is what the assertions in the group above check from both sides
   * of the bound.
   */
  private val MaxDecimalTextLength: Int = 256

  /**
   * The twenty-three validated types, in the order the suite's header lists them.
   *
   * Each has a test naming the inputs its factory refuses, including the three whose condition
   * is a relation between fields rather than a property of one.
   */
  private val ValidatedTypes: List[String] =
    List(
      "StandardId",
      "Country",
      "FxRate",
      "FxMatrix",
      "CurrencyAmountArray",
      "MultiCurrencyAmountArray",
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
      "Rounding.HalfUp",
      "DayCount.Bus252",
      "FixedScaleDecimal",
      "IborIndexObservation",
      "OvernightIndexObservation",
      "FxIndexObservation")

  /**
   * The eleven normalising types, in the order the suite's header lists them.
   *
   * Each has a test of group two asserting its rewrite and a property of group four asserting
   * the idempotence of that rewrite.
   */
  private val NormalisingTypes: List[String] =
    List(
      "CurrencyAmount",
      "Money",
      "BigMoney",
      "MultiCurrencyAmount",
      "Decimal",
      "Tenor",
      "Frequency",
      "SequenceDate",
      "HolidayCalendarId",
      "ImmutableHolidayCalendar",
      "RollConvention.ofDayOfMonth")
}
