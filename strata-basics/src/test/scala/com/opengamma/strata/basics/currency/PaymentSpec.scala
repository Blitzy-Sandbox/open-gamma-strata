/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.time.LocalDate

import cats.Hash
import cats.Show

import io.circe.Json
import io.circe.parser.decode
import io.circe.parser.parse
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.currency.Currency.EUR
import com.opengamma.strata.basics.currency.Currency.GBP
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[Payment]]. The contract asserted here:
 *
 *   - the identity of a payment is its amount and its date, and `copy` produces the changed value
 *     while the payment it came from is untouched;
 *   - [[Payment.ofPay]] and [[Payment.ofReceive]] normalise the sign rather than negating, so an
 *     amount that already has the stated direction passes through unchanged;
 *   - [[Payment.adjustDate]] and [[Payment.negated]] answer with new values, and `adjustDate`
 *     answers with this payment when the adjusted date is the date it started from;
 *   - the JSON form is the derived object shape, `value` then `date`, and decoding it returns the
 *     same payment;
 *   - equality, hashing and rendering are asserted over distinct instances.
 *
 * The three-argument [[Payment.of]] is the one factory that can fail, because it builds a
 * [[CurrencyAmount]] from a raw number; [[unwrap]] opens the outcome it reports.
 */
final class PaymentSpec extends AnyFunSuite with Matchers {

  private val GBP_P1000: CurrencyAmount = unwrap(CurrencyAmount.of(GBP, 1000d))

  private val GBP_M1000: CurrencyAmount = unwrap(CurrencyAmount.of(GBP, -1000d))

  /**
   * A thousand pounds at the flat rate of 1.6 that `test_convertedTo_rateProvider` supplies, and
   * exactly this value rather than close to it, which is why that test needs no tolerance.
   */
  private val EUR_P1600: CurrencyAmount = unwrap(CurrencyAmount.of(EUR, 1600d))

  private val DATE_2015_06_29: LocalDate = date(2015, 6, 29)

  private val DATE_2015_06_30: LocalDate = date(2015, 6, 30)

  /** The JSON form of `Payment.of(GBP_P1000, DATE_2015_06_30)`. */
  private val ExpectedJson: String =
    """{"value":{"currency":"GBP","amount":1000.0},"date":"2015-06-30"}"""

  //-------------------------------------------------------------------------
  test("test_of_3args") {
    val test = unwrap(Payment.of(GBP, 1000d, DATE_2015_06_30))

    test.value shouldBe GBP_P1000
    test.getCurrency shouldBe GBP
    test.getAmount shouldBe 1000d
    test.date shouldBe DATE_2015_06_30

    // the amount arrives as a raw number and has to become a CurrencyAmount, which refuses a
    // value that is not a number
    Payment.of(GBP, Double.NaN, DATE_2015_06_30).isLeft shouldBe true
  }

  test("test_of_2args") {
    val test = Payment.of(GBP_P1000, DATE_2015_06_30)

    // this overload takes an amount that is already checked, so it answers with a payment rather
    // than with an outcome
    test.value shouldBe GBP_P1000
    test.getCurrency shouldBe GBP
    test.getAmount shouldBe 1000d
    test.date shouldBe DATE_2015_06_30

    test shouldBe Payment(GBP_P1000, DATE_2015_06_30)
  }

  test("test_ofPay") {
    val test = Payment.ofPay(GBP_P1000, DATE_2015_06_30)

    test.value shouldBe GBP_M1000
    test.getCurrency shouldBe GBP
    test.getAmount shouldBe -1000d
    test.date shouldBe DATE_2015_06_30

    // an amount that is already negative passes through unchanged, where an unconditional
    // negation would turn it back into money received
    Payment.ofPay(GBP_M1000, DATE_2015_06_30).getAmount shouldBe -1000d
    Payment.ofPay(GBP_M1000, DATE_2015_06_30) shouldBe test
  }

  test("test_ofReceive") {
    val test = Payment.ofReceive(GBP_P1000, DATE_2015_06_30)

    test.value shouldBe GBP_P1000
    test.getCurrency shouldBe GBP
    test.getAmount shouldBe 1000d
    test.date shouldBe DATE_2015_06_30

    Payment.ofReceive(GBP_M1000, DATE_2015_06_30).getAmount shouldBe 1000d
    Payment.ofReceive(GBP_M1000, DATE_2015_06_30) shouldBe test
  }

  /**
   * The payment copied from is equal to the target in neither field, so the assertions cannot be
   * satisfied by a `copy` that quietly ignores one of its arguments.
   */
  test("test_builder") {
    val test = Payment.of(EUR_P1600, DATE_2015_06_29).copy(value = GBP_P1000, date = DATE_2015_06_30)

    test.value shouldBe GBP_P1000
    test.getCurrency shouldBe GBP
    test.getAmount shouldBe 1000d
    test.date shouldBe DATE_2015_06_30

    // a copy that renames nothing is the payment it came from, and one field may be replaced
    // without disturbing the other
    test.copy() shouldBe test
    test.copy(date = DATE_2015_06_29).value shouldBe GBP_P1000
    test.copy(date = DATE_2015_06_29).date shouldBe DATE_2015_06_29
    test.copy(value = EUR_P1600).value shouldBe EUR_P1600
    test.copy(value = EUR_P1600).date shouldBe DATE_2015_06_30
  }

  //-------------------------------------------------------------------------
  test("test_adjustDate") {
    val test = Payment.ofReceive(GBP_P1000, DATE_2015_06_29)
    val expected = Payment.of(GBP_P1000, DATE_2015_06_29.plusDays(1))

    test.adjustDate(d => d.plusDays(1)) shouldBe expected
    test.adjustDate(_.plusDays(1)).value shouldBe GBP_P1000
    test.adjustDate(_.plusDays(1)).date shouldBe DATE_2015_06_29.plusDays(1)
  }

  /**
   * `adjustDate` answers with this payment when the function hands back the date it was given, so
   * identity is asserted rather than equality, which would pass for a fresh copy of equal value.
   * The adjuster moves the date and moves it back, so the short circuit is reached through a
   * function that is not the identity function: what is compared is the date, not the function.
   */
  test("test_adjustDate_noChange") {
    val test = Payment.ofReceive(GBP_P1000, DATE_2015_06_29)
    val adjusted = test.adjustDate(d => d.plusDays(1).minusDays(1))

    adjusted should be theSameInstanceAs test
    assert(adjusted eq test, "adjustDate must return this payment when the adjusted date is unchanged")
  }

  //-------------------------------------------------------------------------
  test("test_negated") {
    val test = Payment.ofReceive(GBP_P1000, DATE_2015_06_30)

    // negation is unconditional, which is what distinguishes it from the sign normalisation of
    // `ofPay` and `ofReceive`: applying it twice returns to the payment it started from
    test.negated shouldBe Payment.of(GBP_M1000, DATE_2015_06_30)
    test.negated.date shouldBe DATE_2015_06_30
    test.negated.negated shouldBe test
  }

  //-------------------------------------------------------------------------
  test("test_convertedTo_rateProvider") {
    val test = Payment.ofReceive(GBP_P1000, DATE_2015_06_30)

    val provider = FxRateProvider.fromFunction((_, _) => Right(1.6d))

    // a conversion changes the currency of a payment and never the date it is made on
    test.convertedTo(EUR, provider) should haveValue(Payment.ofReceive(EUR_P1600, DATE_2015_06_30))

    // a conversion into the currency the payment already has answers with this payment, so
    // identity is asserted under the equality an equal copy would also satisfy
    test.convertedTo(GBP, provider) should haveValue(test)
    assert(
      unwrap(test.convertedTo(GBP, provider)) eq test,
      "a conversion into the currency the payment already has must return this payment")

    // that route needs no rate at all, so it succeeds under a provider that supplies none where
    // any other conversion is declined
    test.convertedTo(GBP, FxRateProvider.noConversion()) should haveValue(test)
    test.convertedTo(EUR, FxRateProvider.noConversion()).isLeft shouldBe true
  }

  //-------------------------------------------------------------------------
  /**
   * Equality delegates the amount to [[CurrencyAmount]], which compares with
   * `java.lang.Double.compare` rather than with the `==` a case class would synthesise, so an
   * infinity - the one non-finite amount the type admits - is equal to an infinity that was
   * arrived at another way.
   */
  test("coverage") {
    val test = Payment.of(GBP_P1000, DATE_2015_06_30)
    val test2 = Payment.of(EUR_P1600, DATE_2015_06_29)

    // an instance built independently from equal parts is equal to the first and hashes equally,
    // and the `Hash` instance agrees with the platform equality and hash
    val same = Payment(unwrap(CurrencyAmount.of(GBP, 1000d)), date(2015, 6, 30))
    same shouldBe test
    same.hashCode shouldBe test.hashCode
    Hash[Payment].eqv(same, test) shouldBe true
    Hash[Payment].hash(same) shouldBe test.hashCode

    // an instance differing from the first in one field only is unequal too, so both fields
    // participate rather than only the one read first
    test2 should not be test
    Hash[Payment].eqv(test2, test) shouldBe false
    Payment.of(GBP_P1000, DATE_2015_06_29) should not be test
    Payment.of(EUR_P1600, DATE_2015_06_30) should not be test
    Hash[Payment].eqv(Payment.of(GBP_P1000, DATE_2015_06_29), test) shouldBe false
    Hash[Payment].eqv(Payment.of(EUR_P1600, DATE_2015_06_30), test) shouldBe false

    val infinite = Payment.of(unwrap(CurrencyAmount.of(GBP, Double.PositiveInfinity)), DATE_2015_06_30)
    infinite shouldBe Payment.of(unwrap(CurrencyAmount.of(GBP, 1d / 0d)), DATE_2015_06_30)
    Hash[Payment].eqv(infinite, test) shouldBe false
    java.lang.Double.compare(Double.PositiveInfinity, 1d / 0d) shouldBe 0

    // the rendered literal is pinned, so a change to it cannot pass unnoticed
    Show[Payment].show(test) shouldBe test.toString
    test.toString shouldBe "Payment{value=GBP 1000, date=2015-06-30}"
    test2.toString shouldBe "Payment{value=EUR 1600, date=2015-06-29}"
  }

  test("test_serialization") {
    val test = Payment.of(GBP_P1000, DATE_2015_06_30)

    // compared as a parsed document, so what is claimed is the fields, their names and their
    // values rather than the whitespace or the order a printer happens to choose
    test.asJson shouldBe json(ExpectedJson)

    decode[Payment](test.asJson.noSpaces) shouldBe Right(test)

    // the shape is pinned key by key as well, because the comparison above would also be
    // satisfied by an expected literal that had drifted along with the codec
    test.asJson.asObject.map(obj => obj.keys.toList) shouldBe Some(List("value", "date"))
    test.asJson.hcursor.get[String]("date") shouldBe Right("2015-06-30")
    test.asJson.hcursor.downField("value").get[String]("currency") shouldBe Right("GBP")
    test.asJson.hcursor.downField("value").get[Double]("amount") shouldBe Right(1000d)
  }

  //-------------------------------------------------------------------------
  /**
   * Reads the value out of an outcome, folding rather than opening it with a partial accessor, so
   * that a fixture or a call which unexpectedly fails is reported as a test failure naming the
   * reason.
   */
  private def unwrap[A](outcome: FailureOr[A]): A =
    outcome.fold(
      failure => fail(s"Expected a value but the call failed with: ${failure.message}"),
      value => value)

  /**
   * Parses an expected JSON literal of this suite, failing the test if the literal is not itself
   * valid JSON - a defect in the suite rather than in the subject.
   */
  private def json(text: String): Json =
    parse(text).getOrElse(fail(s"the expected JSON of this spec is not itself valid JSON: $text"))
}
