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

import cats.data.NonEmptyList

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import com.opengamma.strata.collect.Decimal
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
 * policy of AAP section 0.3.3, which is one half of the explicit-error-handling gate (AAP Rule 5
 * / Gate 5). Its sibling `FailableSurfaceSpec` holds the other half - the '''methods''' that
 * return an outcome - and the two are deliberately independent: either can fail without the
 * other, and neither repeats the other's cases.
 *
 * ===What construction has to prove===
 *
 * A validated type of this port is a `sealed abstract case class X private (...)`, a form that
 * generates neither `apply` nor `copy`, so its factory is the only way a value of it comes into
 * existence. That makes the factory the whole of the type's guarantee, and three things about it
 * are worth asserting:
 *
 *  1. '''Every distinct invalid input is rejected, and rejected as the right kind of failure.'''
 *     A spec that asserted only `isLeft` would pass even if every cause collapsed onto one
 *     reason, so each case below asserts the [[FailureReason]] by value through `beFailureWith`,
 *     and asserts the attribute where the AAP fixes one.
 *  1. '''Independent causes accumulate.''' The accumulating channel of `Validate` is the reason
 *     these factories return `EitherNec` rather than `Either`, and it is invisible unless a test
 *     supplies an input that is wrong in two ways at once and counts the chain. That is what the
 *     third group below does, for nine factories.
 *  1. '''Normalisation is what it says it is, and is idempotent.''' A normalising factory may
 *     rewrite its input - sort it, deduplicate it, round it, canonicalise a period, turn `-0.0`
 *     into `0.0` - and a value that has been through it must survive being put through it again
 *     unchanged, or no caller could rebuild a value from its own accessors. The fourth group
 *     asserts each documented rewrite and then its idempotence, by property.
 *
 * The compile-level half of the policy - that `X(...)` and `.copy` do '''not''' exist, and that a
 * sealed family cannot be extended - belongs to `ApiSurfaceSpec` and is not repeated here. Every
 * value below is therefore built through `of` or `parse`, which is the only way to build one.
 *
 * ===Where a throw is still correct===
 *
 * AAP section 0.3.3 keeps two numeric-domain edges as `ArgCheck` throws rather than converting
 * them to failures, because the arithmetic they sit behind stays total in signature exactly as it
 * was in the library being ported. The last group of this file owns those two, and they are the
 * '''only''' `intercept` in the file: a data-dependent rejection anywhere else would be a
 * regression, and the Rule 5 gate greps this file for exactly that.
 *
 * ===Coverage against AAP section 0.3.3===
 *
 * The lists below are transcribed from the construction-kind tables of the AAP, and every entry
 * has a test in this file. A reviewer can compare the two at a glance; the suite also prints the
 * two lists when it runs, from [[SmartConstructorSpec.ValidatedTypes]] and
 * [[SmartConstructorSpec.NormalisingTypes]], so the coverage claim is checked against a count
 * rather than taken on trust.
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
 * Three of the validated entries carry a condition that the shape of their fields cannot state,
 * so it is worth naming what each of their factories refuses and where this file asserts it:
 *
 *  - `DaysAdjustment.of` refuses a day count of zero paired with an addition calendar other than
 *    the no-holidays identifier, because a business-day addition of zero days names no day. Its
 *    four '''named''' factories are total, and they are total because each of them lands inside
 *    the field space `of` accepts - `ofCalendarDays` fixes the addition calendar, and both
 *    `ofBusinessDays` forms drop it for a zero day count - which this file proves by running
 *    every value they build back through `of` rather than assuming it.
 *  - `Schedule.of` refuses a list of periods that does not run from earliest to latest, reporting
 *    the unadjusted and the adjusted date pair of each misplaced pair separately. A gap between
 *    one period and the next is allowed, as is one period ending on the day the next begins, so
 *    the positive controls below assert those two as well as the rejections.
 *  - `ValueSchedule.of` refuses two steps that name one position (one period index, or one date)
 *    with different adjustments, once per position so named. Two steps naming a position with the
 *    '''same''' adjustment agree rather than contradict, and are accepted.
 *
 * The data-dependent failures those three types report from their '''methods''' - resolving a
 * calendar identifier, merging or adjusting a schedule, resolving a definition against a schedule
 * of periods - belong to `FailableSurfaceSpec` and are not repeated here.
 */
final class SmartConstructorSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  import SmartConstructorSpec._

  /**
   * The number of draws each idempotence property is checked against.
   *
   * Every property in the last group is seed-independent - it claims that re-applying a factory
   * to the accessors of a value that factory produced yields an equal value, which holds of
   * every value of the type - so the verdict does not depend on which values are drawn and two
   * runs of the suite agree whatever seed each is given. The count is therefore chosen for
   * coverage and for runtime rather than to make a flaky claim pass: fifty draws per property
   * across a dozen properties exercise each generator's corners several times over while leaving
   * this file, which touches nearly every type of the module, inside a couple of seconds.
   */
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50)

  //-------------------------------------------------------------------------
  // Shared values. Everything here is built through a factory, because there is no other way to
  // build one, and each is unwrapped through a helper that fails the suite rather than
  // substituting a fallback - a fixture that stopped being valid must break loudly.
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

  /** The two dates of the Java `SchedulePeriodTest`, in the order it held them. */
  private val jul04: LocalDate = date(2014, 7, 4)
  private val jul18: LocalDate = date(2014, 7, 18)

  /** The working-day-override fixture of the Java `ImmutableHolidayCalendarTest`. */
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
  // take a value out of one; doing that here rather than with `getOrElse` and a fabricated
  // fallback keeps a fixture that has stopped being valid visible - the suite fails naming the
  // reasons it was rejected for, instead of quietly testing some other value.
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
   * The accumulation tests compare these rather than the reasons, because every failure that
   * `Validate` produces carries the reason `INVALID` and a comparison of reasons could therefore
   * not tell one cause from another.
   */
  private def messagesOf[A](result: ResultNec[A]): List[String] =
    failuresOf(result).map(failure => failure.message)

  /**
   * Rebuilds a calendar from its own accessors, which is what its idempotence is a claim about.
   *
   * The four arguments are exactly the four things the factory decides - the holidays it sorts
   * and deduplicates, the weekend it applies, the overrides it applies last, and the range of
   * years it derives from the holidays - so a calendar that survives this unchanged is one a
   * decoder or a document can reproduce.
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
  // The coverage claim of the header, checked rather than asserted in prose. The two lists name
  // the types of the AAP's construction-kind tables, and the suite prints them so that a
  // reviewer comparing this file with section 0.3.3 reads the same names from both.
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
    // What the counts above cannot say. The two lists are names, and the tests that exercise
    // those names are written separately, so a test deleted or renamed leaves the counts intact
    // and the list still claiming coverage of a type nothing exercises. This ties the two
    // together the only way a heterogeneous suite can be tied together without rewriting it:
    // the names ScalaTest actually holds are compared with the names the lists claim.
    //
    // A type is claimed by a test whose name begins with it - the convention every test of the
    // groups below follows - or, for a member of a type, by a test of that type naming the
    // member, which is how `DayCount.Bus252` is claimed by `DayCount.ofBus252 ...`. The three
    // derived observations share one test, since they share the one failure they can report, and
    // that test is required by name so that deleting it fails here rather than going unnoticed.
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
  // the reason of each asserted by value so that two different faults cannot quietly become the
  // same failure. Each test also builds one acceptable value, which is what gives the rejections
  // their meaning: the factory is discriminating, not merely refusing.
  //-------------------------------------------------------------------------
  test("StandardId.of rejects a scheme and a value that do not match their permitted shapes") {
    // accumulating: the scheme and the value are two arguments a caller supplied, so both are
    // described - see the accumulation group for the count
    StandardId.of("", "AAPL") should beFailureWith(FailureReason.INVALID)
    StandardId.of("OG~Ticker", "AAPL") should beFailureWith(FailureReason.INVALID)
    StandardId.of("OG-Ticker", "") should beFailureWith(FailureReason.INVALID)
    StandardId.of("OG-Ticker", " AAPL") should beFailureWith(FailureReason.INVALID)
    StandardId.of("OG-Ticker", "AA~PL") should beFailureWith(FailureReason.INVALID)
    // text naming no identifier is a parse failure rather than an invalid argument
    StandardId.parse("no-separator-here") should beFailureWith(FailureReason.PARSING)
    StandardId.of("OG-Ticker", "AAPL") should beSuccess
  }

  test("Country.of rejects a code that is not two upper case letters") {
    Country.of("") should beFailureWith(FailureReason.INVALID)
    Country.of("G") should beFailureWith(FailureReason.INVALID)
    Country.of("GBR") should beFailureWith(FailureReason.INVALID)
    Country.of("gb") should beFailureWith(FailureReason.INVALID)
    Country.of("G1") should beFailureWith(FailureReason.INVALID)
    // the code space is open, as it was in the library being ported, so an unassigned but
    // well-shaped code is accepted rather than rejected
    Country.of("ZZ") should beSuccess
    Country.of("GB") should beSuccess

    // single-cause: of3Char reports the shape of the code or the absence of a translation for
    // it, and the two are different reasons rather than two spellings of one
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
    // text naming no rate is a parse failure
    FxRate.parse("GBP/USD") should beFailureWith(FailureReason.PARSING)
    FxRate.of(gbpUsd, 1.25d) should beSuccess
    FxRate.of(gbpGbp, 1d) should beSuccess
  }

  test("FxMatrix construction rejects rates it cannot place and a stated matrix of the wrong shape") {
    val gbpUsdRate: FxRate = accepted(FxRate.of(gbpUsd, 1.25d))
    val eurJpyRate: FxRate = accepted(FxRate.of(eurJpy, 130d))

    // single-cause: a set of rates is folded into the matrix one at a time, so the first set
    // that cannot be joined to what has been placed is the one failure there is to report
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
    // What is deliberately NOT validated is as much a part of the contract as what is: the
    // builder being ported placed whatever rate it was given, so a rate of zero is a matrix
    // whose opposite direction is infinite, and a decoder that rejected it would reject a matrix
    // that can be built. AAP section 0.6.4 states this for the codec and 0.3.3 for the factory.
    val zeroRate: FxMatrix = FxMatrix.of(gbpUsd, 0d)
    zeroRate.fxRate(gbp, usd) should haveValue(0d)
    produced(zeroRate.fxRate(usd, gbp)) shouldBe Double.PositiveInfinity
    accepted(FxMatrix.fromMatrix(zeroRate.currencies, zeroRate.rates)) shouldBe zeroRate
  }

  test("CurrencyAmountArray.of rejects an empty collection and a collection of several currencies") {
    // single failure per call, and deliberately so: the two conditions are combined rather than
    // sequenced, but a collection holding no amount names no currency to disagree about, so the
    // two are mutually exclusive in practice
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

  test("MultiCurrencyAmountArray.of rejects arrays whose lengths disagree across currencies") {
    val unequal: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.of(Map(gbp -> DoubleArray.of(1d, 2d), usd -> DoubleArray.of(3d)))
    unequal should beFailureWith(FailureReason.INVALID)

    val totalled: ResultNec[MultiCurrencyAmountArray] =
      MultiCurrencyAmountArray.total(
        List(
          CurrencyAmountArray.of(gbp, DoubleArray.of(1d, 2d)),
          CurrencyAmountArray.of(usd, DoubleArray.of(3d))))
    totalled should beFailureWith(FailureReason.INVALID)

    MultiCurrencyAmountArray.of(
      Map(gbp -> DoubleArray.of(1d, 2d), usd -> DoubleArray.of(3d, 4d))) should beSuccess
  }

  test("MarketTenor construction rejects a count that is not a tenor and text that names none") {
    // single-cause: the reasons a count was rejected are joined into the one failure this type
    // reports, which is why its factories answer Either rather than EitherNec
    MarketTenor.ofSpotDays(0) should beFailureWith(FailureReason.INVALID)
    MarketTenor.ofSpotDays(-1) should beFailureWith(FailureReason.INVALID)
    MarketTenor.ofSpotMonths(0) should beFailureWith(FailureReason.INVALID)
    MarketTenor.ofSpotYears(-1) should beFailureWith(FailureReason.INVALID)
    // empty text is reported as the argument check it is, other unreadable text as a parse
    MarketTenor.parse("") should beFailureWith(FailureReason.INVALID)
    MarketTenor.parse("QQ") should beFailureWith(FailureReason.PARSING)
    MarketTenor.ofSpot(Tenor.TENOR_3M) should beSuccess
    MarketTenor.parse("ON") should beSuccess
  }

  test("DaysAdjustment.of rejects a day count of zero paired with an addition calendar") {
    // The one thing about the three fields of an adjustment that can be wrong, and the condition
    // the class being ported stated in the same words: the addition calendar is what makes the
    // days business days, so a count of zero paired with one asks for a walk of zero business
    // days, which names no day at all. The identifier is a name rather than a resolved calendar,
    // so whether it resolves is a question for the reference data and belongs to a method.
    val zeroAgainstACalendar: ResultNec[DaysAdjustment] =
      DaysAdjustment.of(0, HolidayCalendarIds.GBLO, BusinessDayAdjustment.NONE)
    zeroAgainstACalendar should beFailureWith(FailureReason.INVALID)
    // one cause and one only: nothing else about the fields can be wrong - a count is any
    // integer and either calendar may be composite - so there is no second cause to accumulate
    failuresOf(zeroAgainstACalendar) should have size 1
    failuresOf(zeroAgainstACalendar).head shouldBe a[Failure.Invalid]
    messagesOf(zeroAgainstACalendar).head should include(HolidayCalendarIds.GBLO.name)
    DaysAdjustment.of(0, HolidayCalendarId.of("GBLO+USNY"), BusinessDayAdjustment.NONE) should
      beFailureWith(FailureReason.INVALID)

    // the positive controls: the same pairing with a count that is not zero is an addition that
    // walks business days, and a count of zero against the no-holidays identifier is the
    // adjustment that moves nothing - so the factory is discriminating rather than refusing
    DaysAdjustment.of(2, HolidayCalendarIds.GBLO, BusinessDayAdjustment.NONE) should beSuccess
    DaysAdjustment.of(-2, HolidayCalendarIds.GBLO, BusinessDayAdjustment.NONE) should beSuccess
    DaysAdjustment.of(0, HolidayCalendarIds.NO_HOLIDAYS, BusinessDayAdjustment.NONE) should
      haveValue(DaysAdjustment.NONE)

    // The four named factories are total, and what makes that correct is that each of them lands
    // inside the field space `of` accepts: `ofCalendarDays` fixes the addition calendar to the
    // no-holidays identifier, and both `ofBusinessDays` forms drop it for a count of zero. That
    // is asserted rather than assumed - every value they build is run back through `of` and has
    // to be accepted unchanged, so the two construction routes cannot drift apart.
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
    // no-holidays identifier as its addition calendar. The two-argument business-day form is
    // where the calendar a caller named survives the rewrite - it becomes the trailing
    // adjustment, which is the interpretable reading of the request `of` refuses.
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
    // a period of days is acceptable where the convention is not month-based
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
    // The bean being ported validated that its list of periods was not empty - which is the type
    // of the field here - and documented, without checking, that the periods ran from earliest to
    // latest. This factory checks it, because every member that reads the periods reads them as a
    // time line: the schedule is the ScheduleInfo a day count accrues against, periodEndDate
    // answers with the first period containing a date, stub classification reads the first and
    // last period, and a value schedule resolves a step by finding the period whose boundary it
    // names. A list that is not a time line makes all of those answer wrongly rather than fail.
    val july: SchedulePeriod = accepted(SchedulePeriod.of(jul04, jul18))
    val august: SchedulePeriod = accepted(SchedulePeriod.of(date(2014, 8, 1), date(2014, 8, 15)))
    val september: SchedulePeriod = accepted(SchedulePeriod.of(date(2014, 9, 1), date(2014, 9, 15)))

    // accumulating: a misplaced pair is wrong under both pairs of dates and both are reported,
    // because they are two statements about one list and a caller correcting the unadjusted dates
    // is helped by knowing whether the adjusted dates are wrong too
    val reversedPair: ResultNec[Schedule] =
      Schedule.of(NonEmptyList.of(august, july), Frequency.P1M, RollConventions.DAY_15)
    reversedPair should beFailureWith(FailureReason.INVALID)
    failuresOf(reversedPair) should have size 2
    failuresOf(reversedPair).head shouldBe a[Failure.Invalid]
    val pairMessages: List[String] = messagesOf(reversedPair)
    pairMessages.count(message => message.contains("the unadjusted end date")) shouldBe 1
    pairMessages.count(message => message.contains("the adjusted end date")) shouldBe 1
    // each failure names the pair it rejected, by position in the list and by date
    val reported: String = pairMessages.mkString("; ")
    reported should include("2014-08-15")
    reported should include("2014-07-04")
    reported should include("index 0")
    reported should include("index 1")

    // every misplaced pair is reported rather than only the first, so a list held backwards
    // reports each of its consecutive pairs under each pair of dates
    val reversedRun: ResultNec[Schedule] =
      Schedule.of(NonEmptyList.of(september, august, july), Frequency.P1M, RollConventions.DAY_15)
    failuresOf(reversedRun) should have size 4
    reasonsOf(reversedRun).distinct shouldBe List(FailureReason.INVALID)
    val runMessages: List[String] = messagesOf(reversedRun)
    runMessages.distinct should have size 4
    runMessages.count(message => message.contains("the unadjusted end date")) shouldBe 2
    runMessages.count(message => message.contains("the adjusted end date")) shouldBe 2

    // the two pairs of dates are checked independently, so a list whose unadjusted dates are in
    // order and whose adjusted dates overlap reports the one statement that is false of it. Only
    // a business day adjustment can produce such a list, which is why the adjusted pair is
    // checked at all.
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

    // the positive controls. One period is a time line whatever its dates, there being no pair to
    // compare; a gap between one period and the next is allowed exactly as the bean allowed it,
    // since a schedule may describe accrual that pauses; and one period ending on the day the
    // next begins is the ordinary case, which is why the check is order rather than strict order.
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
    // AAP section 0.3.3 fixes one attribute name for this module - the `definition` key a
    // schedule failure carries, which replaces the field the ported exception held - so the
    // attribute is asserted here rather than only the reason. The definition below is accepted
    // by the factory and rejected by generation, because whether a stub is allowed cannot be
    // decided until the schedule is rolled out.
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
    // The half of the contradiction the bean being ported reported only on resolution that needs
    // no schedule to see: a position is a period index or a date, whichever the step carries, and
    // two steps carrying the same one ask a single point of the time line for two different
    // values whatever schedule they are later resolved against. The other half - a step named by
    // an index and a step named by the boundary date of that period - is a question about the
    // periods and stays with resolveValues, which is FailableSurfaceSpec's.
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

    // accumulating: each doubly named position is a cause of its own, so a definition that
    // contradicts itself at an index and at a date reports both rather than the first of them
    val both: ResultNec[ValueSchedule] =
      ValueSchedule.of(200d, List(atIndex1, alsoAtIndex1, atJul04, alsoAtJul04), None)
    failuresOf(both) should have size 2
    reasonsOf(both).distinct shouldBe List(FailureReason.INVALID)
    messagesOf(both) should contain allElementsOf messagesOf(index)
    messagesOf(both) should contain allElementsOf messagesOf(dated)
    messagesOf(both).distinct should have size 2

    // The positive controls. Two steps at one position asking for the same adjustment agree
    // rather than contradict, and are accepted exactly as the bean being ported accepted them:
    // the value changes once, to the value both steps ask for. An index and a date are different
    // positions here, so a definition naming one of each is built and judged on resolution.
    val twice: ResultNec[ValueSchedule] = ValueSchedule.of(200d, List(atIndex1, atIndex1))
    twice should beSuccess
    accepted(twice).steps shouldBe List(atIndex1, atIndex1)
    ValueSchedule.of(200d, List(atIndex1, atJul04), None) should beSuccess

    // the remaining overloads: one naming no step, and one naming a single step, name no position
    // twice and so cannot reach the check. They report through the same channel all the same,
    // because a caller of a validated factory of this port reads one shape at every type.
    val sequence: ValueStepSequence =
      accepted(
        ValueStepSequence.of(jul04, jul18, Frequency.P1M, ValueAdjustment.ofDeltaAmount(-100d)))
    accepted(ValueSchedule.of(100d)).initialValue shouldBe 100d
    accepted(ValueSchedule.of(100d, List.empty[ValueStep], None)).stepSequence shouldBe None
    accepted(ValueSchedule.of(100d, List(atIndex1))).steps shouldBe List(atIndex1)
    accepted(ValueSchedule.of(100d, sequence)).stepSequence shouldBe Some(sequence)

    // the two `with` operations re-validate, which is what the [V] policy of AAP section 0.3.3
    // requires of a field-wise modification: the steps they are given are the caller's, so they
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
    // equal dates are permitted, since the check is order-or-equal
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
    // single-cause: the identifier either resolves or it does not, and the failure is the one
    // the resolution reports. This is also where the port stopped consulting ambient reference
    // data - the Java factory resolved against ReferenceData.standard() from inside itself.
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
    // The two causes are combined rather than sequenced, but they cannot both hold: a scale
    // below a decimal's own scale is at most seventeen, since eighteen is the largest scale a
    // decimal has. Each call therefore reports one failure, and the test says so rather than
    // demanding an accumulation the arithmetic makes impossible.
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
    // single-cause each: the observation is built only through the factory that resolves the
    // fixing calendar and computes the dependent dates from it, so a calendar that cannot be
    // resolved is the one thing that can go wrong and there is no consistent value to return.
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
  // rewrites, because for these types the rewrite is as much a part of the contract as the
  // rejection - a caller reading a value back gets the canonical form, not the form it supplied.
  // The idempotence of each rewrite is asserted by property in group four.
  //-------------------------------------------------------------------------
  test("CurrencyAmount.of rejects a non-number, keeps the infinities, and normalises negative zero") {
    // single-cause: there is one condition on an amount, so the outcome carries one failure
    CurrencyAmount.of(gbp, Double.NaN) should beFailureWith(FailureReason.INVALID)
    failureOf(CurrencyAmount.of(gbp, Double.NaN)) shouldBe a[Failure.Invalid]

    // the infinities are numbers for this purpose and are accepted, as they were in the library
    // being ported - only a not-a-number value is rejected
    produced(CurrencyAmount.of(gbp, Double.PositiveInfinity)).amount shouldBe Double.PositiveInfinity
    produced(CurrencyAmount.of(gbp, Double.NegativeInfinity)).amount shouldBe Double.NegativeInfinity

    // negative zero is normalised to positive zero, which `==` cannot see: the assertion is on
    // the bit pattern, because AAP section 0.3.3 gives this type doubleToLongBits equality
    val normalised: CurrencyAmount = produced(CurrencyAmount.of(gbp, -0.0d))
    java.lang.Double.compare(normalised.amount, 0.0d) shouldBe 0
    java.lang.Double.doubleToLongBits(normalised.amount) shouldBe
      java.lang.Double.doubleToLongBits(0.0d)
    normalised.isZero shouldBe true
    normalised.isPositive shouldBe false
    normalised.isNegative shouldBe false
    normalised shouldBe CurrencyAmount.zero(gbp)

    // a currency code outside the closed family names no amount, which is a parse failure
    CurrencyAmount.of("QQQ", 1d) should beFailureWith(FailureReason.PARSING)
    CurrencyAmount.parse("GBP") should beFailureWith(FailureReason.PARSING)
    CurrencyAmount.parse("GBP 100") should beSuccess
  }

  test("Money.of rounds its amount to the minor units of its currency, half up") {
    // one case per distinct minorUnitDigits among the seventy-four currencies of the family:
    // two for the ordinary currencies, nought for the unit currencies, three for BHD and OMR
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
    // the half-way case rounds away from zero, and a thirteenth digit below the half stays down
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
    // single-cause: the amounts are read until the first currency that repeats, so the failure
    // names that currency and there is no second cause to accumulate
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
    // text that names no decimal is a parse failure rather than an invalid argument
    Decimal.of("") should beFailureWith(FailureReason.PARSING)
    Decimal.parse("not a number") should beFailureWith(FailureReason.PARSING)
    // a value needing more than eighteen digits at scale zero is beyond the type
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

    // the canonical name is the Java form, which is the ISO period without its `P`
    accepted(Tenor.of(Period.ofMonths(3))).name shouldBe "3M"
    accepted(Tenor.of(Period.ofYears(1))).name shouldBe "1Y"
    accepted(Tenor.ofDays(7)).name shouldBe "1W"
    accepted(Tenor.ofDays(14)).name shouldBe "2W"
    accepted(Tenor.ofDays(8)).name shouldBe "8D"
    // months are never folded into years, so twelve months and one year are different tenors
    accepted(Tenor.ofMonths(12)).name shouldBe "12M"
    accepted(Tenor.ofMonths(12)) should not be accepted(Tenor.ofYears(1))

    // parsing accepts the canonical form and the ISO form it was derived from
    produced(Tenor.parse("3M")) shouldBe accepted(Tenor.of(Period.ofMonths(3)))
    produced(Tenor.parse("P3M")) shouldBe produced(Tenor.parse("3M"))
    Tenor.parse("QQ") should beFailureWith(FailureReason.PARSING)
  }

  test("Frequency.of rejects a period that is not positive, canonicalises it, and keeps the P prefix") {
    Frequency.of(Period.ZERO) should beFailureWith(FailureReason.INVALID)
    Frequency.of(Period.ofMonths(-1)) should beFailureWith(FailureReason.INVALID)
    Frequency.ofMonths(0) should beFailureWith(FailureReason.INVALID)
    Frequency.ofDays(0) should beFailureWith(FailureReason.INVALID)
    // a period beyond the thousand years the factory admits, which is the bound the library
    // being ported applied in the same place - and which is why `Frequency.TERM`, whose period
    // is ten thousand years, is reachable as a constant and through `parse` but not through `of`
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

    // a minimum period of zero passes the checks and is then normalised away, which is what
    // makes the factory idempotent over the fields of its own output
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

    // a repeated part is dropped, and a part naming no holidays is absorbed by each operator in
    // the way that operator's meaning requires
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
    // weekend. The fixture is the one the Java test used - a Friday/Saturday weekend with a
    // Saturday declared working - extended with a holiday that is overridden as well.
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

    // clause four: a working day outside the range the holidays span is ignored, because outside
    // that range the calendar holds no data for an override to apply to
    val outsideRange: ImmutableHolidayCalendar =
      ImmutableHolidayCalendar.of(testCalendarId, List(wed20140709), weekend, List(sat20130713))
    outsideRange.isHoliday(sat20130713) shouldBe true
    outsideRange.workingDays shouldBe empty

    // equality is by identifier alone, as in the library being ported, which is why every clause
    // above compares holiday sets rather than comparing calendars
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
  // Group three: accumulation. This is the behaviour that distinguishes the accumulating channel
  // of `Validate` from the fail-fast channel of `ArgCheck`, and it is invisible unless a test
  // supplies an input that is wrong in two ways at once and counts what comes back.
  //
  // Every failure of `Validate` carries the reason INVALID, so counting reasons would not show
  // that both expected causes are present. Each test below therefore compares the chain of the
  // multi-cause call with the chains of the calls that have one cause each: when the messages of
  // the single-cause outcomes are all present in the multi-cause outcome, and the count matches,
  // no cause has been dropped and none has been substituted. The fixtures are chosen so that
  // each single-cause call reports the same values as the multi-cause one, since the wording of
  // an order or range failure names the values it rejected.
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
  // Group four: the idempotence of normalisation, one property per normalising type.
  //
  // Each property re-applies the factory to the accessors of a value that factory produced and
  // claims the result equals it. That is the guarantee a caller needs in order to rebuild a
  // value from its own parts - a decoder, a builder-style `with*` method and the JSON round trip
  // all depend on it - and it is what makes a rewrite safe rather than lossy.
  //
  // Every property here holds of every value of its type, so its verdict does not depend on
  // which values are drawn and two runs of this suite agree whatever seed each is given. The
  // generators are those of the module's `Arbitraries`, which build through the same factories
  // and therefore produce only values that are already normalised.
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
        // The one frequency outside the range of its own period-taking factory. 'Term' holds a
        // period of ten thousand years, and `of` admits a thousand - which is exactly the
        // library being ported, where the constant was built by the private constructor and
        // `of` rejected its period. Its idempotence therefore runs through its name, which is
        // the route `parse` and the JSON codec take, rather than through its period.
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
    // here: the claim is about content, and it is made over the four things construction decides
    // - the holidays, the weekend, the overrides, and the range of years the holidays span.
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
      // the convention a day-of-month names round-trips through its own canonical name, which is
      // the idempotence available to a type whose factory takes a number rather than its output
      RollConvention.parse(convention.name) should haveValue(convention)
    }
  }

  //-------------------------------------------------------------------------
  // Group five: the two numeric-domain edges that stay throws.
  //
  // AAP section 0.3.3 records a deliberate narrowing here, and `SCALA_MIGRATION.md` lists it as
  // a divergence: the arithmetic of the domain types stays total in signature, exactly as it was
  // in the library being ported, so the two places where a numeric domain has no answer raise a
  // broken precondition through `ArgCheck` rather than returning a failure. They are the ONLY
  // `intercept` in this file, and they are deliberate. A data-dependent rejection anywhere else
  // in this module must be an `Either`, which is what the rest of this file asserts.
  //
  // Do not "fix" either of these into an outcome: a caller adding two ordinary amounts, or two
  // ordinary decimals, cannot reach the check, and giving the operator a failure channel would
  // cost every such caller an unwrapping for a case its values cannot produce.
  //-------------------------------------------------------------------------
  test("CurrencyAmount arithmetic that produces a value that is not a number raises its invariant") {
    val positive: CurrencyAmount = produced(CurrencyAmount.of(gbp, Double.PositiveInfinity))
    val negative: CurrencyAmount = produced(CurrencyAmount.of(gbp, Double.NegativeInfinity))

    // the case AAP section 0.3.3 names explicitly: positive infinity plus negative infinity
    intercept[IllegalArgumentException](positive.plus(negative)) // documented ArgCheck throw
    intercept[IllegalArgumentException](positive.plus(Double.NegativeInfinity)) // documented
    intercept[IllegalArgumentException](positive.minus(Double.PositiveInfinity)) // documented
    intercept[IllegalArgumentException](positive.multipliedBy(0d)) // documented ArgCheck throw

    // everything short of that domain edge is ordinary arithmetic, and stays a value
    positive.plus(1d).amount shouldBe Double.PositiveInfinity
    produced(positive.plus(positive)).amount shouldBe Double.PositiveInfinity
    gbp100.plus(usd50) should beFailureWith(FailureReason.INVALID)
    produced(gbp100.plus(gbp100)).amount shouldBe 200d
  }

  test("Decimal arithmetic beyond eighteen digits of precision raises its invariant") {
    intercept[IllegalArgumentException](Decimal.MAX_VALUE.plus(Decimal.MAX_VALUE)) // documented
    intercept[IllegalArgumentException](Decimal.MIN_VALUE.minus(Decimal.MAX_VALUE)) // documented
    intercept[IllegalArgumentException](Decimal.MAX_VALUE.multipliedBy(10L)) // documented

    // the factory, by contrast, reports a value beyond the precision of the type as a failure,
    // because there the value came from outside rather than from the type's own arithmetic
    Decimal.of("1000000000000000000") should beFailureWith(FailureReason.INVALID)
    Decimal.MAX_VALUE.plus(Decimal.ZERO) shouldBe Decimal.MAX_VALUE
  }
}

/**
 * The two coverage lists of [[SmartConstructorSpec]] and the one attribute name it asserts.
 *
 * The lists are transcribed from the construction-kind tables of AAP section 0.3.3 and are what
 * the first test of the suite counts and prints. They live in the companion rather than in the
 * suite so that the names are stated once, next to the doc comment that explains what each kind
 * means, and so that a reader comparing this file with the AAP has one place to look.
 */
object SmartConstructorSpec {

  /**
   * The attribute name a rejected schedule definition carries.
   *
   * The production code holds this name privately - it is the `definition` key of
   * [[com.opengamma.strata.basics.schedule.PeriodicSchedule]] - so the spec states it
   * independently. Were the two to drift apart the assertion would fail, which is the point.
   */
  private val DefinitionAttribute: String = "definition"

  /**
   * The validated types of AAP section 0.3.3, in the order that section lists them.
   *
   * Each has a test in the suite naming the inputs its factory refuses. The three whose condition
   * is a relation between fields rather than a property of one - `DaysAdjustment`, whose day count
   * has to agree with its addition calendar; `Schedule`, whose periods have to run from earliest
   * to latest; and `ValueSchedule`, whose steps must not name one position twice with different
   * adjustments - are no different in kind: each is checked by its `of`, and the class-level
   * documentation of the suite says what each of the three refuses.
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
   * The normalising types of AAP section 0.3.3, in the order that section lists them.
   *
   * Each has a test asserting its documented rewrite and a property asserting the idempotence of
   * that rewrite.
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
