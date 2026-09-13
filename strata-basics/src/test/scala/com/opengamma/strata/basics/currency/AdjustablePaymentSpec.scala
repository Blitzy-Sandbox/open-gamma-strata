/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.time.DayOfWeek
import java.time.LocalDate

import cats.Hash
import cats.Show

import io.circe.Json
import io.circe.parser.decode
import io.circe.parser.parse
import io.circe.syntax.EncoderOps

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.currency.Currency.EUR
import com.opengamma.strata.basics.currency.Currency.GBP
import com.opengamma.strata.basics.date.AdjustableDate
import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.basics.date.BusinessDayConventions.FOLLOWING
import com.opengamma.strata.basics.date.HolidayCalendarIds.GBLO
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[AdjustablePayment]].
 *
 * `ofPay` and `ofReceive` state a direction rather than negating the amount they are given, so an
 * amount already carrying the wanted sign passes through unchanged and both are idempotent. The
 * two three-argument factories build the amount themselves and inherit its refusal of a value
 * that is not a number, so they return an outcome where the two-argument forms are total.
 * `resolve` reports reference data that cannot supply the calendar the date names as
 * `Left(`[[com.opengamma.strata.collect.result.Failure.MissingData]]`)`, and `toReader` offers the
 * same resolution as a `Kleisli` over `FailureOr` awaiting its data. The companion publishes a
 * `Hash` and a `Show` and no `Order`.
 */
final class AdjustablePaymentSpec extends AnyFunSuite with Matchers {

  private val REF_DATA: ReferenceData = ReferenceData.standard

  private val GBP_P1000: CurrencyAmount = unwrap(CurrencyAmount.of(GBP, 1000d))
  private val GBP_M1000: CurrencyAmount = unwrap(CurrencyAmount.of(GBP, -1000d))
  private val EUR_P1600: CurrencyAmount = unwrap(CurrencyAmount.of(EUR, 1600d))

  private val DATE_2015_06_29: LocalDate = date(2015, 6, 29)

  private val DATE_2015_06_28_ADJ: AdjustableDate =
    AdjustableDate.of(date(2015, 6, 28), BusinessDayAdjustment.of(FOLLOWING, GBLO))

  private val DATE_2015_06_30: LocalDate = date(2015, 6, 30)

  private val DATE_2015_06_30_FIX: AdjustableDate = AdjustableDate.of(date(2015, 6, 30))

  /**
   * The absent adjustment of a date is written out in full, as the no-adjustment convention over
   * the no-holidays calendar, rather than omitted from the document.
   */
  private val ExpectedFixedJson: String =
    """{"value":{"currency":"GBP","amount":1000.0},""" +
      """"date":{"unadjusted":"2015-06-30",""" +
      """"adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}}"""

  private val ExpectedAdjustableJson: String =
    """{"value":{"currency":"GBP","amount":1000.0},""" +
      """"date":{"unadjusted":"2015-06-28",""" +
      """"adjustment":{"convention":"Following","calendar":"GBLO"}}}"""

  //-------------------------------------------------------------------------
  test("test_of_3argsFixed") {
    val test = unwrap(AdjustablePayment.of(GBP, 1000d, DATE_2015_06_30))

    assertPayment(test, GBP_P1000, GBP, 1000d, DATE_2015_06_30_FIX)

    AdjustablePayment.of(GBP, Double.NaN, DATE_2015_06_30).isLeft shouldBe true
  }

  test("test_of_3argsAdjustable") {
    val test = unwrap(AdjustablePayment.of(GBP, 1000d, DATE_2015_06_28_ADJ))

    assertPayment(test, GBP_P1000, GBP, 1000d, DATE_2015_06_28_ADJ)

    AdjustablePayment.of(GBP, Double.NaN, DATE_2015_06_28_ADJ).isLeft shouldBe true
  }

  test("test_of_2argsFixed") {
    val test = AdjustablePayment.of(GBP_P1000, DATE_2015_06_30)

    assertPayment(test, GBP_P1000, GBP, 1000d, DATE_2015_06_30_FIX)

    test shouldBe AdjustablePayment.of(GBP_P1000, DATE_2015_06_30_FIX)
  }

  test("test_of_2argsAdjustable") {
    val test = AdjustablePayment.of(GBP_P1000, DATE_2015_06_28_ADJ)

    assertPayment(test, GBP_P1000, GBP, 1000d, DATE_2015_06_28_ADJ)

    test shouldBe AdjustablePayment(GBP_P1000, DATE_2015_06_28_ADJ)
  }

  //-------------------------------------------------------------------------
  test("test_ofPayFixed") {
    val test = AdjustablePayment.ofPay(GBP_P1000, DATE_2015_06_30)

    assertPayment(test, GBP_M1000, GBP, -1000d, DATE_2015_06_30_FIX)

    AdjustablePayment.ofPay(GBP_M1000, DATE_2015_06_30) shouldBe test
  }

  test("test_ofPayAdjustable") {
    val test = AdjustablePayment.ofPay(GBP_P1000, DATE_2015_06_28_ADJ)

    assertPayment(test, GBP_M1000, GBP, -1000d, DATE_2015_06_28_ADJ)

    AdjustablePayment.ofPay(GBP_M1000, DATE_2015_06_28_ADJ) shouldBe test
  }

  test("test_ofReceiveFixed") {
    val test = AdjustablePayment.ofReceive(GBP_P1000, DATE_2015_06_30)

    assertPayment(test, GBP_P1000, GBP, 1000d, DATE_2015_06_30_FIX)

    AdjustablePayment.ofReceive(GBP_M1000, DATE_2015_06_30) shouldBe test
  }

  test("test_ofReceiveAdjustable") {
    val test = AdjustablePayment.ofReceive(GBP_P1000, DATE_2015_06_28_ADJ)

    assertPayment(test, GBP_P1000, GBP, 1000d, DATE_2015_06_28_ADJ)

    AdjustablePayment.ofReceive(GBP_M1000, DATE_2015_06_28_ADJ) shouldBe test
  }

  //-------------------------------------------------------------------------
  test("test_resolve") {
    val test = AdjustablePayment.ofReceive(GBP_P1000, DATE_2015_06_28_ADJ)
    val expected = Payment.of(GBP_P1000, DATE_2015_06_29)

    DATE_2015_06_28_ADJ.unadjusted.getDayOfWeek shouldBe DayOfWeek.SUNDAY
    DATE_2015_06_29.getDayOfWeek shouldBe DayOfWeek.MONDAY

    test.resolve(REF_DATA) should haveValue(expected)
    test.resolve(REF_DATA).map(payment => payment.value) shouldBe Right(GBP_P1000)
    test.resolve(REF_DATA).map(payment => payment.date) shouldBe Right(DATE_2015_06_29)

    test.toReader.run(REF_DATA) should haveValue(expected)
    test.toReader.run(REF_DATA) shouldBe test.resolve(REF_DATA)

    // a date carrying no adjustment is not exempt from the lookup - its absent adjustment names
    // the no-holidays calendar - but the convention then moves nothing
    AdjustablePayment.ofReceive(GBP_P1000, DATE_2015_06_30).resolve(REF_DATA) should
      haveValue(Payment.of(GBP_P1000, DATE_2015_06_30))

    // `minimal` holds the weekend and no-holiday calendars and nothing else, so GBLO is genuinely
    // absent from it while the calendar an unadjusted date needs is still there
    test.resolve(ReferenceData.minimal) should beFailureWith(FailureReason.MISSING_DATA)
    test.toReader.run(ReferenceData.minimal) should beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  test("test_negated") {
    val test = AdjustablePayment.ofReceive(GBP_P1000, DATE_2015_06_30)

    // negation is unconditional, which is what distinguishes it from the sign normalisation of
    // `ofPay` and `ofReceive`
    test.negated shouldBe AdjustablePayment.of(GBP_M1000, DATE_2015_06_30)
    test.negated.date shouldBe DATE_2015_06_30_FIX
    test.negated.negated shouldBe test
  }

  //-------------------------------------------------------------------------
  /**
   * Equality holds no comparison of its own. It delegates the amount to [[CurrencyAmount]], whose
   * `equals` compares with `Double.compare` - a value that is not a number equal to itself and
   * `-0.0` distinct from `+0.0`, neither of them reachable through a constructed amount - and the
   * date to [[com.opengamma.strata.basics.date.AdjustableDate]], which compares the agreement and
   * not the day that agreement works out to. Both delegations are asserted below.
   */
  test("coverage") {
    val test = AdjustablePayment.of(GBP_P1000, DATE_2015_06_30)
    val test2 = AdjustablePayment.of(EUR_P1600, DATE_2015_06_28_ADJ)

    // `Hash` is the type's single equality-bearing instance, so it is asserted to agree with the
    // platform equality and with the platform hash
    val same =
      AdjustablePayment(unwrap(CurrencyAmount.of(GBP, 1000d)), AdjustableDate.of(date(2015, 6, 30)))
    same shouldBe test
    same.hashCode shouldBe test.hashCode
    Hash[AdjustablePayment].eqv(same, test) shouldBe true
    Hash[AdjustablePayment].hash(same) shouldBe test.hashCode

    test2 should not be test
    Hash[AdjustablePayment].eqv(test2, test) shouldBe false
    AdjustablePayment.of(GBP_P1000, DATE_2015_06_28_ADJ) should not be test
    AdjustablePayment.of(EUR_P1600, DATE_2015_06_30) should not be test
    Hash[AdjustablePayment].eqv(AdjustablePayment.of(GBP_P1000, DATE_2015_06_28_ADJ), test) shouldBe false
    Hash[AdjustablePayment].eqv(AdjustablePayment.of(EUR_P1600, DATE_2015_06_30), test) shouldBe false

    // the amount refuses a value that is not a number, so an infinity is the boundary case this
    // type can hold: two infinite payments built separately are one value
    val infinite =
      AdjustablePayment.of(unwrap(CurrencyAmount.of(GBP, Double.PositiveInfinity)), DATE_2015_06_30)
    infinite shouldBe AdjustablePayment.of(unwrap(CurrencyAmount.of(GBP, 1d / 0d)), DATE_2015_06_30)
    Hash[AdjustablePayment].eqv(infinite, test) shouldBe false

    AdjustablePayment.of(GBP_P1000, AdjustableDate.of(date(2015, 6, 28))) should not be
      AdjustablePayment.of(GBP_P1000, DATE_2015_06_28_ADJ)

    Show[AdjustablePayment].show(test) shouldBe test.toString
    test.toString shouldBe "AdjustablePayment{value=GBP 1000, date=2015-06-30}"
    test2.toString shouldBe
      "AdjustablePayment{value=EUR 1600, date=2015-06-28 adjusted by Following using calendar GBLO}"
  }

  test("test_serialization") {
    val test = AdjustablePayment.of(GBP_P1000, DATE_2015_06_30)

    // compared as a parsed document, so what is claimed is the fields present, their names and
    // their values, not the whitespace or the order a printer happens to choose
    test.asJson shouldBe json(ExpectedFixedJson)

    decode[AdjustablePayment](test.asJson.noSpaces) shouldBe Right(test)

    test.asJson.asObject.map(obj => obj.keys.toList) shouldBe Some(List("value", "date"))
    test.asJson.hcursor.downField("value").get[String]("currency") shouldBe Right("GBP")
    test.asJson.hcursor.downField("value").get[Double]("amount") shouldBe Right(1000d)
    test.asJson.hcursor.downField("date").get[String]("unadjusted") shouldBe Right("2015-06-30")

    // the adjustable variant is the only one that puts a business day adjustment - and through it
    // a holiday calendar identifier - into the document
    val adjustable = AdjustablePayment.of(GBP_P1000, DATE_2015_06_28_ADJ)
    adjustable.asJson shouldBe json(ExpectedAdjustableJson)
    decode[AdjustablePayment](adjustable.asJson.noSpaces) shouldBe Right(adjustable)

    decode[AdjustablePayment](adjustable.asJson.noSpaces)
      .map(payment => payment.date.adjustment.calendar) shouldBe Right(GBLO)

    // the encoder is wrapped in the policy that drops absent values; neither field of this type
    // is optional, so that policy leaves the document at two members
    test.asJson.asObject.map(obj => obj.size) shouldBe Some(2)
    adjustable.asJson.asObject.map(obj => obj.size) shouldBe Some(2)
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts of a payment the four facts every factory test asserts of its result: the amount it
   * holds after any sign normalisation, the currency and signed amount it reports, and the
   * adjustable date it holds, each labelled so a failure names the fact that failed.
   */
  private def assertPayment(
      actual: AdjustablePayment,
      value: CurrencyAmount,
      currency: Currency,
      amount: Double,
      paymentDate: AdjustableDate): Assertion = {
    withClue("value: ")(actual.value shouldBe value)
    withClue("getCurrency: ")(actual.getCurrency shouldBe currency)
    withClue("getAmount: ")(actual.getAmount shouldBe amount)
    withClue("date: ")(actual.date shouldBe paymentDate)
  }

  /**
   * Reads the value out of an outcome expected to have produced one, folding it rather than
   * opening it with a partial accessor so that a fixture which unexpectedly fails is reported as
   * a test failure naming the reason. The resolution outcomes are asserted as outcomes instead,
   * because whether they succeed is the subject of their test.
   */
  private def unwrap[A](outcome: FailureOr[A]): A =
    outcome.fold(
      failure => fail(s"Expected a value but the call failed with: ${failure.message}"),
      value => value)

  /**
   * Parses an expected JSON form of this suite. A literal here that is not itself valid JSON is a
   * defect in the suite rather than a failure of the subject, so it is reported as one.
   */
  private def json(text: String): Json =
    parse(text).getOrElse(fail(s"the expected JSON of this spec is not itself valid JSON: $text"))
}
