/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate
import java.time.Period

import cats.Hash
import cats.Show
import cats.data.NonEmptyChain
import cats.syntax.apply._

import io.circe.DecodingFailure
import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor3

import com.opengamma.strata.basics.RefDataReader
import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[TenorAdjustment]].
 *
 * An adjustment is two steps in a fixed order: the tenor is added to the base date under the
 * addition convention, then the business day adjustment is applied to the sum. Both steps read
 * the one calendar the adjustment names, so the order shows in the dates `test_adjust` asserts.
 * A month-based addition convention admits only a month-based tenor and the factories refuse any
 * other pairing with [[FailureReason.INVALID]]. `adjust(date, refData)`,
 * `resolve(refData).adjust(date)` and `toReader.run(refData)` answer alike, and a calendar the
 * reference data does not hold is a [[Failure]] value rather than a raised error.
 */
class TenorAdjustmentSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  private val REF_DATA: ReferenceData = ReferenceData.standard

  /** The convention that adds the period unchanged; it renders and parses as `None`. */
  private val PAC_NONE: PeriodAdditionConvention = PeriodAdditionConventions.NONE

  /** The last day of month convention, which is month-based. */
  private val PAC_LAST_DAY: PeriodAdditionConvention = PeriodAdditionConventions.LAST_DAY

  /** The last business day of month convention, which is month-based and reads the calendar. */
  private val PAC_LAST_BUSINESS_DAY: PeriodAdditionConvention =
    PeriodAdditionConventions.LAST_BUSINESS_DAY

  private val BDA_NONE: BusinessDayAdjustment = BusinessDayAdjustment.NONE

  /** `Following` over `Sat/Sun`, a calendar whose only non-business days are the weekends. */
  private val BDA_FOLLOW_SAT_SUN: BusinessDayAdjustment =
    BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.SAT_SUN)

  /** The message the validator reports for a day-bearing tenor under a month-based convention. */
  private val MONTH_BASED_MESSAGE: String =
    "Tenor must not contain days when addition convention is month-based"

  private val MONTH_BASED_FAILURE: Failure = Failure.Invalid(MONTH_BASED_MESSAGE)

  private val UNKNOWN_CALENDAR: HolidayCalendarId = HolidayCalendarId.of("XXXX")

  /** An adjustment's rendering, typed as `Any` because `==` on unrelated types is an error here. */
  private val ANOTHER_TYPE: Any = "3M with LastDay then apply Following using calendar Sat/Sun"

  /** A period whose tenor is not month-based, because it contains days. */
  private val MIXED_PERIOD: Period = Period.of(1, 2, 3)

  //-------------------------------------------------------------------------
  private val LAST_DAY_DOCUMENT: String =
    """{"tenor":"3M","additionConvention":"LastDay","adjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""

  private val PLAIN_DOCUMENT: String =
    """{"tenor":"1Y2M3D","additionConvention":"None","adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}"""

  /** A document pairing a week tenor with a month-based convention, which the factory rejects. */
  private val INVALID_DOCUMENT: String =
    """{"tenor":"1W","additionConvention":"LastDay","adjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""

  private val MISSING_TENOR_DOCUMENT: String =
    """{"additionConvention":"LastDay","adjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""

  private val UNKNOWN_TENOR_DOCUMENT: String =
    """{"tenor":"Rubbish","additionConvention":"LastDay","adjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""

  private val UNKNOWN_CONVENTION_DOCUMENT: String =
    """{"tenor":"3M","additionConvention":"Rubbish","adjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""

  /** A document naming a calendar no reference data holds, which decodes but does not resolve. */
  private val UNKNOWN_CALENDAR_DOCUMENT: String =
    """{"tenor":"3M","additionConvention":"LastDay","adjustment":{"convention":"Following","calendar":"XXXX"}}"""

  //-------------------------------------------------------------------------
  /** Returns the value a factory produced, or fails the test with the messages it reported. */
  private def accepted[A](outcome: Either[NonEmptyChain[Failure], A]): A =
    outcome.fold(
      failures =>
        fail(
          "Expected a value but the factory failed with: " +
            failures.toChain.toList.map(failure => failure.message).mkString(", ")),
      value => value)

  /** Returns the failures a factory produced, so a rejection can be asserted as a whole value. */
  private def rejected[A](outcome: Either[NonEmptyChain[Failure], A]): List[Failure] =
    outcome.fold(
      failures => failures.toChain.toList,
      value => fail(s"Expected failures but the factory produced: $value"))

  //-------------------------------------------------------------------------
  /**
   * Months, base date and adjusted date, under `LastDay` then `Following` over `Sat/Sun`.
   *
   * In the first group the base date is not the last day of its month, so the tenor is added
   * unchanged and only the third row needs the business day convention - the sum lands on
   * Saturday 15 November 2014 and moves to Monday the 17th. In the second it is, so the addition
   * convention carries the sum to the last day of the target month.
   */
  private val data_adjust: TableFor3[Int, LocalDate, LocalDate] = Table(
    ("months", "input", "expected"),
    // not last day
    (1, date(2014, 8, 15), date(2014, 9, 15)),
    (2, date(2014, 8, 15), date(2014, 10, 15)),
    (3, date(2014, 8, 15), date(2014, 11, 17)),
    // last day
    (1, date(2014, 2, 28), date(2014, 3, 31)),
    (1, date(2014, 6, 30), date(2014, 7, 31)))

  //-------------------------------------------------------------------------
  test("test_of_additionConventionNone") {
    val tenor: Tenor = accepted(Tenor.of(MIXED_PERIOD))
    val outcome: Either[NonEmptyChain[Failure], TenorAdjustment] = TenorAdjustment.of(tenor, PAC_NONE, BDA_NONE)
    outcome should beSuccess
    val test: TenorAdjustment = accepted(outcome)

    test.tenor shouldBe tenor
    test.tenor shouldBe accepted(Tenor.of(Period.of(1, 2, 3)))
    test.additionConvention shouldBe PAC_NONE
    test.adjustment shouldBe BDA_NONE

    // Neither part says anything, so the tenor alone is the whole description.
    test.toString shouldBe "1Y2M3D"
    Show[TenorAdjustment].show(test) shouldBe "1Y2M3D"

    tenor.isMonthBased shouldBe false
    PAC_NONE.isMonthBased shouldBe false

    test.adjust(date(2014, 8, 15), REF_DATA) should haveValue(date(2015, 10, 18))
  }

  test("test_of_additionConventionLastDay") {
    val outcome: Either[NonEmptyChain[Failure], TenorAdjustment] =
      TenorAdjustment.of(Tenor.TENOR_3M, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN)
    outcome should beSuccess
    val test: TenorAdjustment = accepted(outcome)

    test.tenor shouldBe Tenor.TENOR_3M
    test.additionConvention shouldBe PAC_LAST_DAY
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN

    test.toString shouldBe "3M with LastDay then apply Following using calendar Sat/Sun"
    Show[TenorAdjustment].show(test) shouldBe test.toString

    test.tenor.isMonthBased shouldBe true
    PAC_LAST_DAY.isMonthBased shouldBe true
  }

  test("test_ofLastDay") {
    val outcome: Either[NonEmptyChain[Failure], TenorAdjustment] =
      TenorAdjustment.ofLastDay(Tenor.TENOR_3M, BDA_FOLLOW_SAT_SUN)
    outcome should beSuccess
    val test: TenorAdjustment = accepted(outcome)

    test.tenor shouldBe Tenor.TENOR_3M
    test.additionConvention shouldBe PAC_LAST_DAY
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN
    test.toString shouldBe "3M with LastDay then apply Following using calendar Sat/Sun"

    val explicit: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_3M, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN))
    test shouldBe explicit
    test.hashCode shouldBe explicit.hashCode
    Hash[TenorAdjustment].eqv(test, explicit) shouldBe true
    Show[TenorAdjustment].show(test) shouldBe Show[TenorAdjustment].show(explicit)
    outcome should haveValue(explicit)

    test.adjust(date(2014, 8, 15), REF_DATA) should haveValue(date(2014, 11, 17))
  }

  test("test_ofLastBusinessDay") {
    val outcome: Either[NonEmptyChain[Failure], TenorAdjustment] =
      TenorAdjustment.ofLastBusinessDay(Tenor.TENOR_3M, BDA_FOLLOW_SAT_SUN)
    outcome should beSuccess
    val test: TenorAdjustment = accepted(outcome)

    test.tenor shouldBe Tenor.TENOR_3M
    test.additionConvention shouldBe PAC_LAST_BUSINESS_DAY
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN
    test.toString shouldBe "3M with LastBusinessDay then apply Following using calendar Sat/Sun"

    val explicit: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_3M, PAC_LAST_BUSINESS_DAY, BDA_FOLLOW_SAT_SUN))
    test shouldBe explicit
    test.hashCode shouldBe explicit.hashCode
    Hash[TenorAdjustment].eqv(test, explicit) shouldBe true
    outcome should haveValue(explicit)

    test should not be accepted(TenorAdjustment.ofLastDay(Tenor.TENOR_3M, BDA_FOLLOW_SAT_SUN))
    PAC_LAST_BUSINESS_DAY.isMonthBased shouldBe true
  }

  //-------------------------------------------------------------------------
  test("test_of_invalid_conventionForPeriod") {
    // The validator fires on `additionConvention.isMonthBased && !tenor.isMonthBased`.
    Tenor.TENOR_1W.isMonthBased shouldBe false
    Tenor.TENOR_1D.isMonthBased shouldBe false
    PAC_LAST_DAY.isMonthBased shouldBe true
    PAC_LAST_BUSINESS_DAY.isMonthBased shouldBe true

    val invalid: List[Either[NonEmptyChain[Failure], TenorAdjustment]] = List(
      TenorAdjustment.of(Tenor.TENOR_1W, PAC_LAST_DAY, BDA_NONE),
      TenorAdjustment.of(Tenor.TENOR_1W, PAC_LAST_BUSINESS_DAY, BDA_NONE),
      TenorAdjustment.ofLastDay(Tenor.TENOR_1W, BDA_NONE),
      TenorAdjustment.ofLastBusinessDay(Tenor.TENOR_1W, BDA_NONE))
    invalid should have size 4

    invalid.zipWithIndex.foreach {
      case (outcome, index) =>
        withClue(s"invalid case $index: ") {
          outcome should beFailure
          outcome should beFailureWith(FailureReason.INVALID)
          outcome should haveFailureMessageMatching(MONTH_BASED_MESSAGE)
          rejected(outcome) shouldBe List(MONTH_BASED_FAILURE)
          rejected(outcome).map(failure => failure.message) shouldBe List(MONTH_BASED_MESSAGE)
          rejected(outcome).map(failure => failure.reason) shouldBe List(FailureReason.INVALID)
          rejected(outcome).flatMap(failure => failure.attributes.toList) shouldBe Nil
        }
    }

    val mixed: Tenor = accepted(Tenor.of(MIXED_PERIOD))
    mixed.isMonthBased shouldBe false
    TenorAdjustment.of(mixed, PAC_LAST_DAY, BDA_NONE) should beFailureWith(FailureReason.INVALID)
    TenorAdjustment.of(mixed, PAC_LAST_BUSINESS_DAY, BDA_NONE) should
      beFailureWith(FailureReason.INVALID)
    TenorAdjustment.of(mixed, PAC_NONE, BDA_NONE) should beSuccess

    TenorAdjustment.of(Tenor.TENOR_1W, PAC_NONE, BDA_NONE) should beSuccess
    TenorAdjustment.of(Tenor.TENOR_1D, PAC_NONE, BDA_NONE) should beSuccess
    TenorAdjustment.of(Tenor.TENOR_1M, PAC_LAST_DAY, BDA_NONE) should beSuccess
    TenorAdjustment.of(Tenor.TENOR_1M, PAC_LAST_BUSINESS_DAY, BDA_NONE) should beSuccess
    TenorAdjustment.ofLastDay(Tenor.TENOR_1M, BDA_NONE) should beSuccess
    TenorAdjustment.ofLastBusinessDay(Tenor.TENOR_1M, BDA_NONE) should beSuccess

    rejected(TenorAdjustment.of(Tenor.TENOR_1W, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN)) shouldBe
      List(MONTH_BASED_FAILURE)

    noException should be thrownBy TenorAdjustment.of(Tenor.TENOR_1W, PAC_LAST_DAY, BDA_NONE)
    noException should be thrownBy TenorAdjustment.ofLastBusinessDay(Tenor.TENOR_1W, BDA_NONE)

    // All three arguments are required, so omitting one does not compile.
    assertDoesNotCompile("TenorAdjustment.of(Tenor.TENOR_3M, PeriodAdditionConventions.LAST_DAY)")
    assertDoesNotCompile("TenorAdjustment.ofLastDay(Tenor.TENOR_3M)")
  }

  //-------------------------------------------------------------------------
  test("test_adjust") {
    forAll(data_adjust) { (months: Int, input: LocalDate, expected: LocalDate) =>
      val tenor: Tenor = accepted(Tenor.ofMonths(months))
      val test: TenorAdjustment =
        accepted(TenorAdjustment.of(tenor, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN))
      withClue(s"$test adjusting $input: ") {
        val direct: Either[Failure, LocalDate] = test.adjust(input, REF_DATA)
        direct should haveValue(expected)
        test.resolve(REF_DATA).map(adjuster => adjuster.adjust(input)) should haveValue(expected)
        test.toReader.run(REF_DATA).map(adjuster => adjuster.adjust(input)) should
          haveValue(expected)
      }
    }

    // Friday 29 August 2014 is the last *business* day of its month over `Sat/Sun` but not the
    // last *day* of it, so the month rules part company: `LastBusinessDay` reads the calendar and
    // carries the sum to Tuesday 30 September, `LastDay` leaves it at Monday the 29th. The
    // addition convention therefore runs first, and reads the calendar.
    val lastBusinessDay: TenorAdjustment =
      accepted(TenorAdjustment.ofLastBusinessDay(Tenor.TENOR_1M, BDA_FOLLOW_SAT_SUN))
    val lastDay: TenorAdjustment =
      accepted(TenorAdjustment.ofLastDay(Tenor.TENOR_1M, BDA_FOLLOW_SAT_SUN))
    lastBusinessDay.adjust(date(2014, 8, 29), REF_DATA) should haveValue(date(2014, 9, 30))
    lastDay.adjust(date(2014, 8, 29), REF_DATA) should haveValue(date(2014, 9, 29))

    // `resolve` looks the calendar up once and binds it into the adjuster, so a run costs one
    // lookup rather than one per date.
    val resolvedOnce: Either[Failure, DateAdjuster] = lastDay.resolve(REF_DATA)
    val readOnce: Either[Failure, DateAdjuster] = lastDay.toReader.run(REF_DATA)
    resolvedOnce should beSuccess
    readOnce should beSuccess
    resolvedOnce.map(adjuster =>
      (adjuster.adjust(date(2014, 2, 28)), adjuster.adjust(date(2014, 6, 30)))) should
      haveValue((date(2014, 3, 31), date(2014, 7, 31)))
    readOnce.map(adjuster =>
      (adjuster.adjust(date(2014, 2, 28)), adjuster.adjust(date(2014, 6, 30)))) should
      haveValue((date(2014, 3, 31), date(2014, 7, 31)))

    val dates: List[LocalDate] =
      Iterator
        .iterate(date(2014, 1, 1))(day => day.plusDays(1L))
        .takeWhile(day => day.getYear == 2014)
        .toList
    dates should have size 365

    dates.foreach { day =>
      withClue(s"$lastDay adjusting $day: ") {
        val perDate: Either[Failure, LocalDate] = lastDay.adjust(day, REF_DATA)
        perDate should beSuccess
        resolvedOnce.map(adjuster => adjuster.adjust(day)) shouldBe perDate
        readOnce.map(adjuster => adjuster.adjust(day)) shouldBe perDate
      }
    }

    val threeMonths: TenorAdjustment =
      accepted(TenorAdjustment.ofLastDay(Tenor.TENOR_3M, BDA_FOLLOW_SAT_SUN))
    val bothEnds: RefDataReader[(LocalDate, LocalDate)] =
      (lastDay.toReader, threeMonths.toReader).mapN((near, far) =>
        (near.adjust(date(2014, 8, 15)), far.adjust(date(2014, 8, 15))))
    bothEnds.run(REF_DATA) should haveValue((date(2014, 9, 15), date(2014, 11, 17)))

    val unknown: TenorAdjustment =
      accepted(
        TenorAdjustment.ofLastDay(
          Tenor.TENOR_1M,
          BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, UNKNOWN_CALENDAR)))
    unknown.adjust(date(2014, 8, 15), REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    unknown.resolve(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    unknown.toReader.run(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    noException should be thrownBy unknown.adjust(date(2014, 8, 15), REF_DATA)

    unknown.adjust(date(2014, 8, 15), REF_DATA) should
      haveFailureMessageMatching("Reference data not found for identifier 'XXXX'")

    val partlyUnknown: RefDataReader[(LocalDate, LocalDate)] =
      (lastDay.toReader, unknown.toReader).mapN((known, missing) =>
        (known.adjust(date(2014, 8, 15)), missing.adjust(date(2014, 8, 15))))
    partlyUnknown.run(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)

    // One calendar serves both steps, so even two do-nothing conventions need it resolved.
    accepted(TenorAdjustment.of(Tenor.TENOR_1M, PAC_NONE, BDA_NONE))
      .adjust(date(2014, 8, 15), ReferenceData.empty) should beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  test("equals") {
    val a: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_3M, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN))
    val b: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_1M, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN))
    val c: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_3M, PAC_NONE, BDA_FOLLOW_SAT_SUN))
    val d: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_3M, PAC_LAST_DAY, BDA_NONE))
    val again: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_3M, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN))

    a.equals(b) shouldBe false
    a.equals(c) shouldBe false
    a.equals(d) shouldBe false
    Hash[TenorAdjustment].eqv(a, b) shouldBe false
    Hash[TenorAdjustment].eqv(a, c) shouldBe false
    Hash[TenorAdjustment].eqv(a, d) shouldBe false

    a.equals(a) shouldBe true
    a shouldBe again
    a.hashCode shouldBe again.hashCode
    Hash[TenorAdjustment].eqv(a, again) shouldBe true
    Hash[TenorAdjustment].hash(a) shouldBe Hash[TenorAdjustment].hash(again)
    Hash[TenorAdjustment].hash(a) shouldBe a.hashCode

    b.equals(a) shouldBe false
    c.equals(a) shouldBe false
    d.equals(a) shouldBe false

    again.equals(a) shouldBe true

    a.equals(ANOTHER_TYPE) shouldBe false
    a.toString shouldBe ANOTHER_TYPE
  }

  //-------------------------------------------------------------------------
  test("test_beanBuilder") {
    // `TenorAdjustment` publishes no `copy` and no `with*` method, so every route to a value -
    // factory, convenience factory, decoder - goes through a factory and carries its check.
    val fromFactory: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_3M, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN))
    val fromConvenience: TenorAdjustment =
      accepted(TenorAdjustment.ofLastDay(Tenor.TENOR_3M, BDA_FOLLOW_SAT_SUN))
    val fromDecoded: Either[io.circe.Error, TenorAdjustment] =
      decode[TenorAdjustment](fromFactory.asJson.noSpaces)

    fromFactory.tenor shouldBe Tenor.TENOR_3M
    fromFactory.additionConvention shouldBe PAC_LAST_DAY
    fromFactory.adjustment shouldBe BDA_FOLLOW_SAT_SUN

    fromConvenience.tenor shouldBe fromFactory.tenor
    fromConvenience.additionConvention shouldBe fromFactory.additionConvention
    fromConvenience.adjustment shouldBe fromFactory.adjustment
    fromDecoded.map(adjustment => adjustment.tenor) shouldBe Right(fromFactory.tenor)
    fromDecoded.map(adjustment => adjustment.additionConvention) shouldBe
      Right(fromFactory.additionConvention)
    fromDecoded.map(adjustment => adjustment.adjustment) shouldBe Right(fromFactory.adjustment)

    fromConvenience shouldBe fromFactory
    fromDecoded shouldBe Right(fromFactory)
    Hash[TenorAdjustment].hash(fromConvenience) shouldBe Hash[TenorAdjustment].hash(fromFactory)
    Show[TenorAdjustment].show(fromConvenience) shouldBe Show[TenorAdjustment].show(fromFactory)

    // A changed field is a fresh construction through the factory, which re-checks the pairing:
    // the month tenor is accepted, the week tenor under a month-based convention is refused.
    accepted(TenorAdjustment.of(Tenor.TENOR_1M, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN)).tenor shouldBe
      Tenor.TENOR_1M
    TenorAdjustment.of(Tenor.TENOR_1W, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN) should
      beFailureWith(FailureReason.INVALID)

    // Every field is required, which the signature states rather than checking at run time.
    assertDoesNotCompile("TenorAdjustment.of(Tenor.TENOR_3M)")
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    val test: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_3M, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN))
    val other: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_6M, PAC_LAST_BUSINESS_DAY, BDA_NONE))
    val same: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_3M, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN))

    test.tenor shouldBe Tenor.TENOR_3M
    test.additionConvention shouldBe PAC_LAST_DAY
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN
    other.tenor shouldBe Tenor.TENOR_6M
    other.additionConvention shouldBe PAC_LAST_BUSINESS_DAY
    other.adjustment shouldBe BDA_NONE

    // pattern matching works through the synthesised `unapply` although the constructor is private
    test match {
      case TenorAdjustment(tenor, additionConvention, adjustment) =>
        tenor shouldBe Tenor.TENOR_3M
        additionConvention shouldBe PAC_LAST_DAY
        adjustment shouldBe BDA_FOLLOW_SAT_SUN
    }

    test shouldBe same
    test.hashCode shouldBe same.hashCode
    Hash[TenorAdjustment].eqv(test, same) shouldBe true
    Hash[TenorAdjustment].hash(test) shouldBe Hash[TenorAdjustment].hash(same)
    test should not be other
    Hash[TenorAdjustment].eqv(test, other) shouldBe false
    test.equals(ANOTHER_TYPE) shouldBe false

    // The second value covers the description branch that omits a `NONE` business day adjustment.
    test.toString shouldBe "3M with LastDay then apply Following using calendar Sat/Sun"
    other.toString shouldBe "6M with LastBusinessDay"
    Show[TenorAdjustment].show(test) shouldBe test.toString
    Show[TenorAdjustment].show(other) shouldBe other.toString
  }

  test("test_serialization") {
    // The document is the three fields in declaration order, tenor and convention as bare strings.
    val test: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_3M, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN))
    val encoded: Json = test.asJson

    encoded.asObject.map(obj => obj.keys.toList) shouldBe
      Some(List("tenor", "additionConvention", "adjustment"))
    encoded.noSpaces shouldBe LAST_DAY_DOCUMENT

    encoded.noSpaces should not include "null"

    decode[TenorAdjustment](encoded.noSpaces) shouldBe Right(test)
    val plain: TenorAdjustment =
      accepted(TenorAdjustment.of(accepted(Tenor.of(MIXED_PERIOD)), PAC_NONE, BDA_NONE))
    plain.asJson.noSpaces shouldBe PLAIN_DOCUMENT
    decode[TenorAdjustment](plain.asJson.noSpaces) shouldBe Right(plain)
    plain.asJson.noSpaces should not include "null"

    accepted(TenorAdjustment.ofLastDay(Tenor.TENOR_3M, BDA_FOLLOW_SAT_SUN)).asJson.noSpaces shouldBe
      encoded.noSpaces
    accepted(TenorAdjustment.ofLastBusinessDay(Tenor.TENOR_3M, BDA_FOLLOW_SAT_SUN)).asJson
      .noSpaces should include("\"additionConvention\":\"LastBusinessDay\"")

    // The decoder goes through the checking factory, so an invalid pairing fails to decode.
    val invalid: Either[io.circe.Error, TenorAdjustment] = decode[TenorAdjustment](INVALID_DOCUMENT)
    invalid.isLeft shouldBe true
    val invalidMessage: Option[String] =
      invalid.swap.toOption.collect { case failure: DecodingFailure => failure.message }
    invalidMessage should not be empty
    invalidMessage.getOrElse("") should include(MONTH_BASED_MESSAGE)

    decode[TenorAdjustment]("""{"tenor":"3M","additionConvention":"LastDay"}""").isLeft shouldBe true
    decode[TenorAdjustment](MISSING_TENOR_DOCUMENT).isLeft shouldBe true
    decode[TenorAdjustment]("{}").isLeft shouldBe true
    Json.fromString("3M").as[TenorAdjustment].isLeft shouldBe true

    decode[TenorAdjustment](UNKNOWN_TENOR_DOCUMENT).isLeft shouldBe true
    decode[TenorAdjustment](UNKNOWN_CONVENTION_DOCUMENT).isLeft shouldBe true

    // The calendar field accepts any name, so the value decodes and fails only when adjusted.
    val unknownCalendar: Either[io.circe.Error, TenorAdjustment] =
      decode[TenorAdjustment](UNKNOWN_CALENDAR_DOCUMENT)
    unknownCalendar shouldBe Right(
      accepted(
        TenorAdjustment.of(
          Tenor.TENOR_3M,
          PAC_LAST_DAY,
          BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, UNKNOWN_CALENDAR))))
    unknownCalendar
      .map(adjustment => adjustment.adjust(date(2014, 8, 15), REF_DATA).isLeft) shouldBe Right(true)
  }
}
