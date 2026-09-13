/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import scala.util.matching.Regex

import cats.Hash
import cats.Show

import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableFor3
import org.scalatest.prop.TableFor4
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import com.opengamma.strata.basics.Arbitraries.genEdgeFxRate
import com.opengamma.strata.basics.currency.Currency.AUD
import com.opengamma.strata.basics.currency.Currency.CAD
import com.opengamma.strata.basics.currency.Currency.EUR
import com.opengamma.strata.basics.currency.Currency.GBP
import com.opengamma.strata.basics.currency.Currency.USD
import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[FxRate]].
 *
 * `FxRate.of` refuses a rate at or below zero, and refuses an identity pair whose rate is not
 * one, reporting both reasons in order as a `Left`; `crossRate` and `parse` report failure as a
 * `Left` value as well, rather than by throwing.
 *
 * The positivity check is `!(rate <= 0.0)`, so `NaN` and a positive infinity are rates of this
 * type, and its equality, hashing and JSON form all have to answer for them - the edge-generator
 * test at the end of the suite is where they do. Equality compares the rate with
 * `java.lang.Double.compare`, which canonicalises every NaN payload to one value, so a NaN rate
 * equals itself while `-0.0` stays distinct from `0.0`; it is not raw-bit identity.
 *
 * The JSON form is `{"pair":"EUR/USD","rate":1.6}`, with the pair as its text form and a
 * non-finite rate as a tagged string, and decoding routes both fields through `of`.
 */
final class FxRateSpec
    extends AnyFunSuite
    with Matchers
    with TableDrivenPropertyChecks
    with ScalaCheckDrivenPropertyChecks {

  /** A value of a type unrelated to a rate, for the equality assertion that needs one. */
  private val ANOTHER_TYPE: Any = ""

  /** The refusal of a rate of zero, which is what the reciprocal of an infinite rate is. */
  private val ZeroRateMessage: String =
    "Argument 'rate' must not be negative or zero but has value 0.0"

  /**
   * The longest the rate part of the text form may be, as [[FxRate]] states it.
   *
   * Restated here because it is not visible outside that companion and because both sides of it
   * are asserted. The value is the 256 characters [[CurrencyAmount]], [[Money]] and [[BigMoney]]
   * read their numeral within: one bound holds across the four types of this package that read a
   * number out of text, where this type once carried a thousand characters calibrated on the
   * longest exact decimal spelling of a double.
   */
  private val MaxRateTextLength: Int = 256

  /**
   * The longest text a rejection quotes back in full, which is the longest text this type accepts.
   *
   * The seven characters of a folded pair, the space after them and a rate at the ceiling above.
   * Text within it is named character for character, and text beyond it is named through the
   * bounded renderer, so the cost of a rejection is capped by this type rather than chosen by its
   * caller.
   */
  private val MaxQuotedTextLength: Int = 7 + 1 + MaxRateTextLength

  /**
   * The greatest number of characters of rejected text a message can carry.
   *
   * The bounded renderer of a failure writes at most five hundred and twelve characters of a part
   * and then the three of an ellipsis, so this is the cap a message reaches however long the
   * rejected text was.
   */
  private val MaxRenderedMessagePart: Int = 512 + 3

  //-------------------------------------------------------------------------
  /**
   * The rates the cross-rate test crosses, written as quotients of small integers so that the
   * expected cross rate is the same `Double` whichever way round the inputs are written; a decimal
   * literal for the product would assert rounding instead of the cross-rate orientation.
   */
  private val gbpUsd: FxRate = rateOf(GBP, USD, 5d / 4d)

  private val usdGbp: FxRate = rateOf(USD, GBP, 4d / 5d)

  /** The `EUR/USD` rate crossed through `USD` below. */
  private val eurUsd: FxRate = rateOf(EUR, USD, 8d / 7d)

  private val usdEur: FxRate = rateOf(USD, EUR, 7d / 8d)

  /**
   * The cross rate every successful orientation has to produce: its pair is `EUR/GBP`, because
   * [[CurrencyPair.cross]] answers in convention order however its inputs are written, and its
   * rate is the product in the order [[FxRate.crossRate]] forms it.
   */
  private val eurGbp: FxRate = rateOf(EUR, GBP, (8d / 7d) * (4d / 5d))

  /** The identity rate for `GBP`, which no cross can go through. */
  private val gbpGbp: FxRate = rateOf(GBP, GBP, 1d)

  /** The identity rate for `USD`, which no cross can go through. */
  private val usdUsd: FxRate = rateOf(USD, USD, 1d)

  /** A rate sharing no currency with [[gbpUsd]], for the row that has no common currency. */
  private val eurCad: FxRate = rateOf(EUR, CAD, 12d / 5d)

  //-------------------------------------------------------------------------
  /**
   * Arguments of the two-currency factory that describe no rate, with the message each reports:
   * the two constraints share one reason, so the message is what tells them apart. The second row
   * is the boundary of the positivity constraint, zero.
   */
  private val data_ofInvalidCurrencies: TableFor4[Currency, Currency, Double, String] = Table(
    ("base", "counter", "rate", "message"),
    (GBP, USD, -1.5d, "Argument 'rate' must not be negative or zero but has value -1.5"),
    (GBP, USD, 0d, "Argument 'rate' must not be negative or zero but has value 0.0"),
    (GBP, GBP, 2d, "Conversion rate between identical currencies must be one"))

  /**
   * The same three cases through the currency-pair factory, which is the two-currency factory
   * applied to the pair of its arguments and so rejects the same rates with the same wording.
   */
  private val data_ofInvalidPair: TableFor3[CurrencyPair, Double, String] = Table(
    ("pair", "rate", "message"),
    (
      CurrencyPair.of(GBP, USD),
      -1.5d,
      "Argument 'rate' must not be negative or zero but has value -1.5"),
    (
      CurrencyPair.of(GBP, USD),
      0d,
      "Argument 'rate' must not be negative or zero but has value 0.0"),
    (CurrencyPair.of(USD, USD), 2d, "Conversion rate between identical currencies must be one"))

  //-------------------------------------------------------------------------
  /**
   * Text that names a rate, with the rate it names. `USD/EUR 3.00000000` pins that a fraction of
   * zeros names the whole number `3`, `USD/EUR 2` that a rate needs no fractional part, and
   * `cAd/GbP 1.25` that the text is folded before it is matched.
   */
  private val data_parseGood: TableFor4[String, Currency, Currency, Double] = Table(
    ("input", "base", "counter", "rate"),
    ("USD/EUR 205.123", USD, EUR, 205.123d),
    ("USD/EUR 3.00000000", USD, EUR, 3d),
    ("USD/EUR 2", USD, EUR, 2d),
    ("USD/EUR 0.1", USD, EUR, 0.1d),
    ("EUR/USD 0.001", EUR, USD, 0.001d),
    ("EUR/EUR 1", EUR, EUR, 1d),
    ("cAd/GbP 1.25", CAD, GBP, 1.25d))

  /**
   * Text that names no rate, with the message each reports. Text the expression of
   * [[FxRate.parse]] does not match is reported as `Invalid rate: …`; text that matches and is then
   * rejected by a constraint of the type - rows six to eight - is reported as
   * `Unable to parse rate: …`, so each row carries its own wording. `EUR/USD 1e3` is a format
   * failure because the rate group admits no letter, and its message quotes the text as supplied
   * rather than folded.
   */
  private val data_parseBad: TableFor2[String, String] = Table(
    ("input", "message"),
    ("AUD 1.25", "Invalid rate: AUD 1.25"),
    ("AUD/GB 1.25", "Invalid rate: AUD/GB 1.25"),
    ("AUD GBP 1.25", "Invalid rate: AUD GBP 1.25"),
    ("AUD:GBP 1.25", "Invalid rate: AUD:GBP 1.25"),
    ("123/456", "Invalid rate: 123/456"),
    ("EUR/GBP -1.25", "Unable to parse rate: EUR/GBP -1.25"),
    ("EUR/GBP 0", "Unable to parse rate: EUR/GBP 0"),
    ("EUR/EUR 1.25", "Unable to parse rate: EUR/EUR 1.25"),
    ("", "Invalid rate: "),
    ("EUR/USD 1e3", "Invalid rate: EUR/USD 1e3"))

  //-------------------------------------------------------------------------
  /**
   * Currency pairs `GBP/USD 1.25` answers for, with the rate it answers. The second row matters:
   * the inverted pair is answered with the reciprocal, written as the division the implementation
   * performs so that the row asserts the reciprocal and not a decimal that rounds to it.
   */
  private val data_fxRateFound: TableFor3[Currency, Currency, Double] = Table(
    ("base", "counter", "rate"),
    (GBP, USD, 1.25d),
    (USD, GBP, 1d / 1.25d))

  /**
   * Currency pairs `GBP/USD 1.25` cannot answer for: a rate holds one pair, so a question about a
   * pair it does not hold has no answer, and the message names the pair that was asked about.
   */
  private val data_fxRateMissing: TableFor3[Currency, Currency, String] = Table(
    ("base", "counter", "message"),
    (GBP, AUD, "No FX rate found for GBP/AUD"))

  /**
   * Pairs the pair-form lookup answers for. Three of the five rows are the identity case: a
   * currency and itself converts at one whether or not this rate mentions that currency, which is
   * why `AUD/AUD` is answered by a `GBP/USD` rate.
   */
  private val data_fxRatePairFound: TableFor2[CurrencyPair, Double] = Table(
    ("pair", "rate"),
    (CurrencyPair.of(GBP, USD), 1.25d),
    (CurrencyPair.of(USD, GBP), 1d / 1.25d),
    (CurrencyPair.of(GBP, GBP), 1d),
    (CurrencyPair.of(USD, USD), 1d),
    (CurrencyPair.of(AUD, AUD), 1d))

  /**
   * Pairs the pair-form lookup cannot answer for: sharing one currency with the rate is not
   * enough, which is the point of the four rows that do, and the last row holds neither.
   */
  private val data_fxRatePairMissing: TableFor2[CurrencyPair, String] = Table(
    ("pair", "message"),
    (CurrencyPair.of(AUD, GBP), "No FX rate found for AUD/GBP"),
    (CurrencyPair.of(GBP, AUD), "No FX rate found for GBP/AUD"),
    (CurrencyPair.of(AUD, USD), "No FX rate found for AUD/USD"),
    (CurrencyPair.of(USD, AUD), "No FX rate found for USD/AUD"),
    (CurrencyPair.of(EUR, AUD), "No FX rate found for EUR/AUD"))

  //-------------------------------------------------------------------------
  /**
   * Pairs of rates that cross, with the rate the cross has to produce. The same `EUR/GBP` rate has
   * to come out of `EUR/USD` crossed with `USD/GBP`, of either written the other way round, and of
   * the two supplied in the other order; the expected [[eurGbp]] carries the product in the order
   * [[FxRate.crossRate]] forms it, so a reassociated multiplication or an inverted operand fails
   * here rather than agreeing to within rounding.
   */
  private val data_crossRate: TableFor3[FxRate, FxRate, FxRate] = Table(
    ("first", "second", "expected"),
    (eurUsd, usdGbp, eurGbp),
    (eurUsd, gbpUsd, eurGbp),
    (usdEur, usdGbp, eurGbp),
    (usdEur, gbpUsd, eurGbp),
    (gbpUsd, usdEur, eurGbp),
    (gbpUsd, eurUsd, eurGbp),
    (usdGbp, usdEur, eurGbp),
    (usdGbp, eurUsd, eurGbp))

  /**
   * Pairs of rates that do not cross: one rate is an identity, so its two currencies are one and
   * there is no third; the two rates name the same two currencies, in either order, so again there
   * is no third; and the two rates share no currency at all.
   */
  private val data_crossRateInvalid: TableFor3[FxRate, FxRate, String] = Table(
    ("first", "second", "message"),
    (gbpGbp, gbpUsd, "Unable to cross when no unique common currency: GBP/GBP and GBP/USD"),
    (usdUsd, gbpUsd, "Unable to cross when no unique common currency: USD/USD and GBP/USD"),
    (gbpUsd, gbpUsd, "Unable to cross when no unique common currency: GBP/USD and GBP/USD"),
    (gbpUsd, usdGbp, "Unable to cross when no unique common currency: GBP/USD and USD/GBP"),
    (gbpUsd, eurCad, "Unable to cross when no unique common currency: GBP/USD and EUR/CAD"))

  //-------------------------------------------------------------------------
  test("test_of_CurrencyCurrencyDouble") {
    val test = rateOf(GBP, USD, 1.5d)
    test.pair shouldBe CurrencyPair.of(GBP, USD)
    test.rate shouldBe 1.5d
    test.fxRate(GBP, USD) should haveValue(1.5d)
    test.toString shouldBe "GBP/USD 1.5"
  }

  test("test_of_CurrencyCurrencyDouble_reverseStandardOrder") {
    // `of` keeps the pair as supplied; `toConventional` is the member that reorients it
    val test = rateOf(USD, GBP, 0.8d)
    test.pair shouldBe CurrencyPair.of(USD, GBP)
    test.rate shouldBe 0.8d
    test.fxRate(USD, GBP) should haveValue(0.8d)
    test.toString shouldBe "USD/GBP 0.8"
  }

  test("test_of_CurrencyCurrencyDouble_same") {
    // the text form writes a whole-number rate without a fractional part, so this is "USD/USD 1"
    val test = rateOf(USD, USD, 1d)
    test.pair shouldBe CurrencyPair.of(USD, USD)
    test.pair.isIdentity shouldBe true
    test.rate shouldBe 1d
    test.fxRate(USD, USD) should haveValue(1d)
    test.toString shouldBe "USD/USD 1"
  }

  test("test_of_CurrencyCurrencyDouble_invalid") {
    forAll(data_ofInvalidCurrencies) {
      (base: Currency, counter: Currency, rate: Double, message: String) =>
        val outcome: ResultNec[FxRate] = FxRate.of(base, counter, rate)
        outcome should beFailureWith(FailureReason.INVALID)
        outcome should haveFailureMessageMatching(Regex.quote(message))
    }

    // the two constraints are checked in one expression and accumulate, so arguments that break
    // both are told about both, in the order the checks are written
    val both: List[Failure] = failuresOf(FxRate.of(GBP, GBP, -1d))
    both.map(failure => failure.reason) shouldBe
      List(FailureReason.INVALID, FailureReason.INVALID)
    both.map(failure => failure.message) shouldBe List(
      "Argument 'rate' must not be negative or zero but has value -1.0",
      "Conversion rate between identical currencies must be one")
  }

  test("test_of_CurrencyCurrencyDouble_null") {
    // both currencies are required `Currency` values, so a code supplied as text or an argument
    // omitted altogether fails to compile; the valid call is asserted to compile as well, so the
    // proofs cannot be passing for an unrelated reason
    assertDoesNotCompile("""FxRate.of("GBP", USD, 1.5d)""")
    assertDoesNotCompile("""FxRate.of(GBP, "USD", 1.5d)""")
    assertDoesNotCompile("""FxRate.of(GBP, USD)""")
    assertCompiles("""FxRate.of(GBP, USD, 1.5d)""")
  }

  //-------------------------------------------------------------------------
  test("test_of_CurrencyPairDouble") {
    val test = rateOf(CurrencyPair.of(GBP, USD), 1.5d)
    test.pair shouldBe CurrencyPair.of(GBP, USD)
    test.rate shouldBe 1.5d
    test.fxRate(GBP, USD) should haveValue(1.5d)
    test.toString shouldBe "GBP/USD 1.5"
  }

  test("test_of_CurrencyPairDouble_reverseStandardOrder") {
    val test = rateOf(CurrencyPair.of(USD, GBP), 0.8d)
    test.pair shouldBe CurrencyPair.of(USD, GBP)
    test.rate shouldBe 0.8d
    test.fxRate(USD, GBP) should haveValue(0.8d)
    test.toString shouldBe "USD/GBP 0.8"
  }

  test("test_of_CurrencyPairDouble_same") {
    val test = rateOf(CurrencyPair.of(USD, USD), 1d)
    test.pair shouldBe CurrencyPair.of(USD, USD)
    test.pair.isIdentity shouldBe true
    test.rate shouldBe 1d
    test.fxRate(USD, USD) should haveValue(1d)
    test.toString shouldBe "USD/USD 1"
  }

  test("test_of_CurrencyPairDouble_invalid") {
    forAll(data_ofInvalidPair) { (pair: CurrencyPair, rate: Double, message: String) =>
      val outcome: ResultNec[FxRate] = FxRate.of(pair, rate)
      outcome should beFailureWith(FailureReason.INVALID)
      outcome should haveFailureMessageMatching(Regex.quote(message))
    }

    // this factory is the one the two-currency form delegates to, so it accumulates identically
    val both: List[Failure] = failuresOf(FxRate.of(CurrencyPair.of(GBP, GBP), -1d))
    both.map(failure => failure.message) shouldBe List(
      "Argument 'rate' must not be negative or zero but has value -1.0",
      "Conversion rate between identical currencies must be one")
  }

  test("test_of_CurrencyPairDouble_null") {
    assertDoesNotCompile("""FxRate.of(1.5d)""")
    assertDoesNotCompile("""FxRate.of("GBP/USD", 1.5d)""")
    assertDoesNotCompile("""FxRate.of(CurrencyPair.of(GBP, USD))""")
    assertCompiles("""FxRate.of(CurrencyPair.of(GBP, USD), 1.5d)""")
  }

  //-------------------------------------------------------------------------
  test("test_toConventional") {
    // a pair written against the market convention is reoriented and its rate inverted, so
    // `USD/GBP 0.8` is the same rate as `GBP/USD 1.25`; which direction is conventional is decided
    // by `CurrencyPair.isConventional`
    rateOf(USD, GBP, 0.8d).toConventional shouldBe rateOf(GBP, USD, 1.25d)
    rateOf(USD, GBP, 0.8d).toConventional.pair shouldBe CurrencyPair.of(GBP, USD)
    rateOf(USD, GBP, 0.8d).toConventional.rate shouldBe 1d / 0.8d

    rateOf(GBP, USD, 1.25d).toConventional shouldBe rateOf(GBP, USD, 1.25d)
    rateOf(GBP, USD, 1.25d).toConventional.toConventional shouldBe rateOf(GBP, USD, 1.25d)

    rateOf(GBP, GBP, 1d).toConventional shouldBe rateOf(GBP, GBP, 1d)
  }

  test("test_toConventional_infinite") {
    // the conventional direction of an infinite rate written the other way round would carry the
    // reciprocal of infinity, which is not a rate, so it is refused with the wording of the
    // supplied-zero rows above
    val infinite = rateOf(USD, EUR, Double.PositiveInfinity)
    infinite.pair.isConventional shouldBe false
    intercept[IllegalArgumentException](infinite.toConventional).getMessage shouldBe
      ZeroRateMessage

    val conventional = rateOf(EUR, USD, Double.PositiveInfinity)
    conventional.pair.isConventional shouldBe true
    conventional.toConventional shouldBe conventional
    conventional.toConventional.rate shouldBe Double.PositiveInfinity
  }

  //-------------------------------------------------------------------------
  test("test_parse_String_good") {
    forAll(data_parseGood) { (input: String, base: Currency, counter: Currency, rate: Double) =>
      FxRate.parse(input) should haveValue(rateOf(base, counter, rate))
    }
  }

  test("test_parse_String_bad") {
    forAll(data_parseBad) { (input: String, message: String) =>
      val outcome: FailureOr[FxRate] = FxRate.parse(input)
      outcome should beFailureWith(FailureReason.PARSING)
      outcome should haveFailureMessageMatching(Regex.quote(message))
    }
  }

  //-------------------------------------------------------------------------
  test("test_inverse") {
    val test = rateOf(GBP, USD, 1.25d)
    test.inverse shouldBe rateOf(USD, GBP, 0.8d)
    test.inverse.pair shouldBe CurrencyPair.of(USD, GBP)
    // the reciprocal is the division the implementation performs, asserted as that division
    // rather than as a decimal literal that rounds to the same value
    test.inverse.rate shouldBe 1d / 1.25d
  }

  test("test_inverse_same") {
    val test = rateOf(GBP, GBP, 1d)
    test.inverse shouldBe rateOf(GBP, GBP, 1d)
    test.inverse.rate shouldBe 1d
  }

  test("test_inverse_infinite") {
    // an infinite rate is admitted - it is greater than zero - but its reciprocal is zero, which
    // the single creation route refuses, so `inverse` throws rather than growing a failure channel
    // for an edge reachable only from a rate a caller supplied. The reciprocal of the smallest
    // positive rate is that infinity, while a rate that is not a number inverts to one
    val infinite = rateOf(EUR, USD, Double.PositiveInfinity)
    infinite.rate shouldBe Double.PositiveInfinity
    intercept[IllegalArgumentException](infinite.inverse).getMessage shouldBe ZeroRateMessage

    val denormal = rateOf(EUR, USD, Double.MinPositiveValue)
    denormal.inverse.pair shouldBe CurrencyPair.of(USD, EUR)
    denormal.inverse.rate shouldBe Double.PositiveInfinity
    intercept[IllegalArgumentException](denormal.inverse.inverse).getMessage shouldBe
      ZeroRateMessage

    val notANumber = rateOf(EUR, USD, Double.NaN)
    notANumber.inverse.pair shouldBe CurrencyPair.of(USD, EUR)
    notANumber.inverse.rate.isNaN shouldBe true
  }

  //-------------------------------------------------------------------------
  test("test_fxRate_forBase") {
    // the two-currency lookup, which is the single abstract member of `FxRateProvider` and what
    // makes a rate usable wherever a provider is expected
    val test = rateOf(GBP, USD, 1.25d)
    forAll(data_fxRateFound) { (base: Currency, counter: Currency, rate: Double) =>
      test.fxRate(base, counter) should haveValue(rate)
    }
    forAll(data_fxRateMissing) { (base: Currency, counter: Currency, message: String) =>
      val outcome: FailureOr[Double] = test.fxRate(base, counter)
      outcome should beFailureWith(FailureReason.CURRENCY_CONVERSION)
      outcome should haveFailureMessageMatching(Regex.quote(message))
    }
  }

  test("test_fxRate_forPair") {
    // the ordered-pair overload, which is the lookup above applied to the two currencies of the
    // pair
    val test = rateOf(GBP, USD, 1.25d)
    forAll(data_fxRatePairFound) { (pair: CurrencyPair, rate: Double) =>
      test.fxRate(pair) should haveValue(rate)
    }
    forAll(data_fxRatePairMissing) { (pair: CurrencyPair, message: String) =>
      val outcome: FailureOr[Double] = test.fxRate(pair)
      outcome should beFailureWith(FailureReason.CURRENCY_CONVERSION)
      outcome should haveFailureMessageMatching(Regex.quote(message))
    }
  }

  //-------------------------------------------------------------------------
  test("test_convert_double") {
    val test = rateOf(GBP, USD, 1.25d)
    // the conversion is the amount multiplied by the rate of the lookup, so it succeeds exactly
    // where the lookup does and carries its failure where it does not
    test.convert(100d, GBP, USD) should haveValue(125d)
    test.convert(100d, USD, GBP) should haveValue(100d / 1.25d)

    val missing: FailureOr[Double] = test.convert(100d, GBP, AUD)
    missing should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    missing should haveFailureMessageMatching(Regex.quote("No FX rate found for GBP/AUD"))
  }

  test("test_convert_Decimal") {
    val test = rateOf(GBP, USD, 1.25d)
    val hundred = decimalOf(Decimal.of(100L))
    // the exact-decimal conversion looks the rate up as a `Double` and multiplies the amount by
    // it through the decimal's own multiplication, so the result is an exact decimal rather than a
    // rounded binary value
    test.convert(hundred, GBP, USD) should haveValue(decimalOf(Decimal.of(125L)))
    test.convert(hundred, USD, GBP) should haveValue(decimalOf(Decimal.of(100d / 1.25d)))

    val missing: FailureOr[Decimal] = test.convert(hundred, GBP, AUD)
    missing should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    missing should haveFailureMessageMatching(Regex.quote("No FX rate found for GBP/AUD"))
  }

  //-------------------------------------------------------------------------
  test("test_crossRate") {
    forAll(data_crossRate) { (first: FxRate, second: FxRate, expected: FxRate) =>
      first.crossRate(second) should haveValue(expected)
    }

    // the cross pair is in market convention order whichever way round the two inputs are written
    eurUsd.crossRate(usdGbp).map(cross => cross.pair) shouldBe
      Right(CurrencyPair.of(EUR, GBP))
    gbpUsd.crossRate(usdEur).map(cross => cross.pair) shouldBe
      Right(CurrencyPair.of(EUR, GBP))

    forAll(data_crossRateInvalid) { (first: FxRate, second: FxRate, message: String) =>
      val outcome: FailureOr[FxRate] = first.crossRate(second)
      outcome should beFailureWith(FailureReason.CURRENCY_CONVERSION)
      outcome should haveFailureMessageMatching(Regex.quote(message))
    }
  }

  /**
   * Asserts how rejected text is quoted back, and that the failure renders bounded and on one line.
   *
   * The property the quotation has is that text within the length this type can accept is named
   * in full and text beyond it is named bounded, so a rejection cannot be made to cost more than
   * the ceiling however long the input is. Both sides of that boundary are asserted here, and the
   * rendering - which bounds and escapes every part it writes - is asserted on top of it, because
   * the two bounds are independent: one caps what the failure carries, the other caps what a
   * reader of lines is shown.
   */
  test("parsing bounds the text it quotes, and the failure renders bounded and on one line") {
    val payload = "H" * 10000
    val bounded: FailureOr[FxRate] = FxRate.parse(payload)
    bounded should beFailureWith(FailureReason.PARSING)
    val failure = bounded.left.toOption.getOrElse(fail("expected a failure"))
    failure.message should startWith("Invalid rate: HHH")
    failure.message should endWith("...")
    failure.message.length shouldBe "Invalid rate: ".length + MaxRenderedMessagePart
    val rendered = Show[Failure].show(failure)
    rendered.length should be < 1000
    rendered should startWith("PARSING: Invalid rate: HHH")
    rendered should endWith("...")

    // the rate part admits digits, so a rate written with ten thousand zeroes after the point
    // used to match and then be rejected as a zero rate; it is now past the ceiling on the rate,
    // which reports through that same second wording, and the text is quoted bounded
    val longZero = s"EUR/GBP 0.${"0" * 10000}"
    val matched: FailureOr[FxRate] = FxRate.parse(longZero)
    matched should beFailureWith(FailureReason.PARSING)
    val matchedMessage: String =
      matched.left.toOption.map(failure => failure.message).getOrElse(fail("expected a failure"))
    matchedMessage should startWith("Unable to parse rate: EUR/GBP 0.000")
    matchedMessage should endWith("...")
    matchedMessage.length shouldBe "Unable to parse rate: ".length + MaxRenderedMessagePart
    Show[Failure]
      .show(matched.left.toOption.getOrElse(fail("expected a failure")))
      .length should be < 1000

    // a rejection whose text could have named a rate is quoted in full, whatever it holds
    val realistic: String = s"EUR/GBP 0.${"0" * 100}"
    FxRate.parse(realistic).left.toOption.map(failure => failure.message) shouldBe
      Some(s"Unable to parse rate: $realistic")

    // and the boundary between the two is the longest text this type accepts - the seven
    // characters of a folded pair, the space after them and a rate at the ceiling, which is 264
    // characters. Either side of it is asserted through a raw line break, because that is what
    // tells the two quotations apart at a length where they are otherwise the same characters:
    // within the bound the text is handed back untouched, so the break survives into `message`
    // and is escaped only when the failure is written out, while beyond the bound the text goes
    // through the renderer, which escapes it there and then
    val atQuotationBound: String = "EUR\n" + ("0" * (MaxQuotedTextLength - 4))
    atQuotationBound.length shouldBe MaxQuotedTextLength
    FxRate.parse(atQuotationBound).left.toOption.map(failure => failure.message) shouldBe
      Some(s"Invalid rate: $atQuotationBound")
    val pastQuotationBound: String = "EUR\n" + ("0" * (MaxQuotedTextLength - 3))
    pastQuotationBound.length shouldBe MaxQuotedTextLength + 1
    val pastMessage: String = FxRate
      .parse(pastQuotationBound)
      .left
      .toOption
      .map(failure => failure.message)
      .getOrElse(fail("expected a failure"))
    pastMessage should not be s"Invalid rate: $pastQuotationBound"
    pastMessage should not include "\n"
    pastMessage should include("\\n")

    // whatever a sender chooses, the message a rejection carries is capped: a megabyte of text is
    // named in a few hundred characters, so the cost of a rejection is bounded by this type and
    // not by the length of its input (CWE-400/CWE-770)
    val megabyte: String = "H" * (1024 * 1024)
    val cappedMessage: String = FxRate
      .parse(megabyte)
      .left
      .toOption
      .map(failure => failure.message)
      .getOrElse(fail("expected a failure"))
    cappedMessage.length shouldBe "Invalid rate: ".length + MaxRenderedMessagePart

    val injected = FxRate.parse("EUR\nUSD 1.25")
    injected should beFailureWith(FailureReason.PARSING)
    val injectedFailure = injected.left.toOption.getOrElse(fail("expected a failure"))
    injectedFailure.message shouldBe "Invalid rate: EUR\nUSD 1.25"
    val injectedRendering = Show[Failure].show(injectedFailure)
    injectedRendering should not include "\n"
    injectedRendering should not include "\r"
    injectedRendering shouldBe "PARSING: Invalid rate: EUR\\nUSD 1.25"

    FxRate.parse("AUD 1.25").left.toOption.map(failure => failure.message) shouldBe
      Some("Invalid rate: AUD 1.25")
    FxRate.parse("EUR/GBP 0").left.toOption.map(failure => failure.message) shouldBe
      Some("Unable to parse rate: EUR/GBP 0")
  }

  /**
   * Asserts the accepted grammar at every edge the shape of the text can be at.
   *
   * The pair is folded and matched against an expression while the rate is checked by a scan over
   * the characters it admits, so these cases are where the two halves of the grammar meet. Each
   * is a case the whole-text expression that preceded this decided on its own, and each has to
   * read the same way now: an empty rate part is not one character of the rate class and so is a
   * shape failure; a second space is a character the rate class does not hold and so is a shape
   * failure rather than a second part; a letter anywhere in the rate part is likewise refused by
   * the shape rather than by the reading that follows it, which is what keeps `EUR/USD 1e3` an
   * invalid rate and not an unreadable one.
   *
   * The last case is why the pair test is an upper bound rather than an equality: `\ufb01` folds
   * to `FI`, so `\ufb01M/USD` is six characters before the space that fold to the seven of
   * `FIM/USD`, a pair this library defines. Folding the pair alone has to keep that working, and
   * it is the case that would break first if the fold were applied to a part of the text chosen
   * by position rather than by the separator.
   */
  test("the accepted grammar is unchanged at every edge of the shape of the text") {
    // an empty rate part: one character of the rate class is required
    FxRate.parse("EUR/USD ").left.toOption.map(failure => failure.message) shouldBe
      Some("Invalid rate: EUR/USD ")

    // a second space is not a character of the rate class
    FxRate.parse("EUR/USD 1 2").left.toOption.map(failure => failure.message) shouldBe
      Some("Invalid rate: EUR/USD 1 2")
    FxRate.parse("EUR/USD 1.25 ").left.toOption.map(failure => failure.message) shouldBe
      Some("Invalid rate: EUR/USD 1.25 ")

    // a letter anywhere in the rate part, in either case, is a shape failure
    List("EUR/USD X", "EUR/USD 1x25", "EUR/USD 1.25d", "EUR/USD 1E3", "EUR/USD x1.25").foreach {
      input =>
        val outcome: FailureOr[FxRate] = FxRate.parse(input)
        outcome should beFailureWith(FailureReason.PARSING)
        outcome.left.toOption.map(failure => failure.message) shouldBe Some(s"Invalid rate: $input")
    }

    // every character the rate class does hold is accepted by the shape, including the signs and
    // points that then name no number - which is the other wording, reached through the reading
    FxRate.parse("EUR/USD +1.25").map(rate => rate.toString) shouldBe Right("EUR/USD 1.25")
    FxRate.parse("EUR/USD -.+-").left.toOption.map(failure => failure.message) shouldBe
      Some("Unable to parse rate: EUR/USD -.+-")

    // a pair that grows under folding still reaches the expression and still names its rate
    FxRate.parse("\ufb01M/USD 1.25").map(rate => rate.toString) shouldBe Right("FIM/USD 1.25")
    // and one that grows past seven characters cannot name a pair, so it is a shape failure
    FxRate.parse("\ufb01IM/USD 1.25").left.toOption.map(failure => failure.message) shouldBe
      Some("Invalid rate: \ufb01IM/USD 1.25")
  }

  /**
   * Asserts the ceiling on the rate part from both sides of it.
   *
   * The ceiling is a deliberate narrowing rather than a bug fix: it was calibrated on the longest
   * exact decimal spelling of a double, at 767 significant digits, and is now the 256 characters
   * [[CurrencyAmount]], [[Money]] and [[BigMoney]] read their numeral within, so one bound holds
   * across the four types of this package that read a number out of text. What it refuses is text
   * whose extra digits cannot change the value it names, and it reports through the wording a
   * text that names no rate has always reported rather than through a wording of its own.
   */
  test("the rate part is bounded, and reports through the wording of an unreadable rate") {
    // a rate of exactly the ceiling is read exactly as it always was, zeroes and all
    val atCeiling: String = "1." + ("0" * (MaxRateTextLength - 2))
    atCeiling.length shouldBe MaxRateTextLength
    FxRate.parse(s"EUR/GBP $atCeiling").map(rate => rate.toString) shouldBe Right("EUR/GBP 1")

    // one character more names no rate, through that same wording and with no fold, no match and
    // no numeric reading
    val pastCeiling: String = "1." + ("0" * (MaxRateTextLength - 1))
    pastCeiling.length shouldBe MaxRateTextLength + 1
    val refused: FailureOr[FxRate] = FxRate.parse(s"EUR/GBP $pastCeiling")
    refused should beFailureWith(FailureReason.PARSING)
    refused.left.toOption.map(failure => failure.message) shouldBe
      Some(s"Unable to parse rate: EUR/GBP $pastCeiling")
  }

  //-------------------------------------------------------------------------
  test("test_equals_hashCode") {
    val a1 = rateOf(AUD, GBP, 1.25d)
    val a2 = rateOf(AUD, GBP, 1.25d)
    val b = rateOf(USD, GBP, 1.25d)
    val c = rateOf(USD, GBP, 1.35d)

    // `equals` is called as a method so that the whole matrix, including the reflexive cases, can
    // be stated - `==` between two identical expressions would be flagged in this build. `b` and
    // `c` differ in nothing but their rate
    a1.equals(a1) shouldBe true
    a1.equals(a2) shouldBe true
    a1.equals(b) shouldBe false
    a1.equals(c) shouldBe false

    b.equals(a1) shouldBe false
    b.equals(a2) shouldBe false
    b.equals(b) shouldBe true
    b.equals(c) shouldBe false

    c.equals(a1) shouldBe false
    c.equals(a2) shouldBe false
    c.equals(b) shouldBe false
    c.equals(c) shouldBe true

    a1.hashCode shouldBe a2.hashCode

    // the same statements through the `Hash` the library reads equality and hashing from; the
    // companion publishes no `Order`, because rates of different pairs do not order
    Hash[FxRate].eqv(a1, a2) shouldBe true
    Hash[FxRate].eqv(a1, b) shouldBe false
    Hash[FxRate].eqv(b, c) shouldBe false
    Hash[FxRate].hash(a1) shouldBe Hash[FxRate].hash(a2)
  }

  test("test_equals_bad") {
    val test = rateOf(AUD, GBP, 1.25d)

    test.equals(ANOTHER_TYPE) shouldBe false
    ANOTHER_TYPE.equals(test) shouldBe false
  }

  //-----------------------------------------------------------------------
  test("test_serialization") {
    val test = rateOf(GBP, USD, 1.25d)
    val encoded: Json = test.asJson
    encoded.noSpaces shouldBe """{"pair":"GBP/USD","rate":1.25}"""
    encoded.as[FxRate] shouldBe Right(test)

    val identityRate = rateOf(GBP, GBP, 1d)
    val encodedIdentity: Json = identityRate.asJson
    encodedIdentity.noSpaces shouldBe """{"pair":"GBP/GBP","rate":1.0}"""
    encodedIdentity.as[FxRate] shouldBe Right(identityRate)

    // decoding routes the two fields through the same factory a caller's arguments go through, so
    // a document naming a rate the factory rejects is a decoding failure
    Json
      .obj("pair" -> Json.fromString("GBP/USD"), "rate" -> Json.fromDoubleOrNull(0d))
      .as[FxRate]
      .isLeft shouldBe true
    Json
      .obj("pair" -> Json.fromString("GBP/GBP"), "rate" -> Json.fromDoubleOrNull(1.25d))
      .as[FxRate]
      .isLeft shouldBe true
  }

  test("test_serialization_derivedShape") {
    // the encoded object holds exactly the keys `pair` and `rate`, in that order and nothing
    // else, and decoding the encoded form returns the same value
    val test = rateOf(EUR, USD, 1.6d)
    test.asJson.asObject.map(obj => obj.keys.toList) shouldBe Some(List("pair", "rate"))
    test.asJson.noSpaces shouldBe """{"pair":"EUR/USD","rate":1.6}"""
    test.asJson.as[FxRate] shouldBe Right(test)

    // a double goes through the shared double codec, which writes the three values JSON cannot
    // express as tagged strings; both non-finite rates survive the round trip
    val infinite = rateOf(EUR, USD, Double.PositiveInfinity)
    infinite.asJson.noSpaces shouldBe """{"pair":"EUR/USD","rate":"Infinity"}"""
    infinite.asJson.as[FxRate] shouldBe Right(infinite)

    val notANumber = rateOf(EUR, USD, Double.NaN)
    notANumber.asJson.noSpaces shouldBe """{"pair":"EUR/USD","rate":"NaN"}"""
    notANumber.asJson.as[FxRate] shouldBe Right(notANumber)

    test.asJson.hcursor.get[String]("pair") shouldBe Right("EUR/USD")
  }

  /**
   * Asserts the two non-finite rates the type admits, over the shared edge generator.
   *
   * `FxRate.of` applies `!(rate <= 0.0)`, so a rate that is not a number passes - `NaN <= 0.0` is
   * false - and a positive infinity passes because it exceeds zero, while both signed zeros and a
   * negative infinity are rejected. Of every rate drawn this asserts the round trip through the
   * document form, and equality and hashing against a rate rebuilt through the factory - which for
   * a NaN rate holds because the equality compares through `java.lang.Double.compare`. The
   * generator is the one the typeclass law suite runs its `FxRate` rule sets over.
   */
  test("every rate the type admits survives its document form and hashes with its equal") {
    forAll(genEdgeFxRate, minSuccessful(200)) { (rate: FxRate) =>
      val rebuilt: FxRate = unwrap(FxRate.of(rate.pair, rate.rate))
      rate.asJson.as[FxRate] shouldBe Right(rate)
      rate.equals(rebuilt) shouldBe true
      Hash[FxRate].eqv(rate, rebuilt) shouldBe true
      Hash[FxRate].hash(rate) shouldBe Hash[FxRate].hash(rebuilt)
      Show[FxRate].show(rate) shouldBe rate.toString
    }

    val notANumber: FxRate = rateOf(GBP, USD, Double.NaN)
    val infinite: FxRate = rateOf(GBP, USD, Double.PositiveInfinity)
    notANumber.asJson.noSpaces shouldBe """{"pair":"GBP/USD","rate":"NaN"}"""
    infinite.asJson.noSpaces shouldBe """{"pair":"GBP/USD","rate":"Infinity"}"""

    // a rate that is not a number equals itself, where the primitive comparison would not
    (Double.NaN == Double.NaN) shouldBe false
    notANumber.equals(rateOf(GBP, USD, Double.NaN)) shouldBe true
    Hash[FxRate].eqv(notANumber, rateOf(GBP, USD, Double.NaN)) shouldBe true
    notANumber.hashCode shouldBe rateOf(GBP, USD, Double.NaN).hashCode
    Hash[FxRate].eqv(notANumber, infinite) shouldBe false

    // and the three edge values the domain excludes
    FxRate.of(GBP, USD, Double.NegativeInfinity).isLeft shouldBe true
    FxRate.of(GBP, USD, 0.0d).isLeft shouldBe true
    FxRate.of(GBP, USD, -0.0d).isLeft shouldBe true
  }

  //-----------------------------------------------------------------------
  test("coverage") {
    val test = rateOf(GBP, USD, 1.25d)
    val same = rateOf(GBP, USD, 1.25d)
    val other = rateOf(GBP, USD, 1.35d)

    // inequality is stated through `eqv`: the instance guarantees that equal rates hash equally
    // and says nothing about unequal ones, so the hashes of `test` and `other` are deliberately
    // left uncompared - two distinct rates may legally collide
    Hash[FxRate].eqv(test, same) shouldBe true
    Hash[FxRate].eqv(test, other) shouldBe false
    Hash[FxRate].hash(test) shouldBe Hash[FxRate].hash(same)

    Show[FxRate].show(test) shouldBe test.toString
    Show[FxRate].show(test) shouldBe "GBP/USD 1.25"
    Show[FxRate].show(other) shouldBe "GBP/USD 1.35"

    // a whole-number rate is written without a fractional part
    val integral = rateOf(EUR, USD, 5d)
    integral.toString shouldBe "EUR/USD 5"
    Show[FxRate].show(integral) shouldBe "EUR/USD 5"

    FxRate.parse(Show[FxRate].show(test)) should haveValue(test)
    FxRate.parse(Show[FxRate].show(integral)) should haveValue(integral)

    // a rate from the factory and the same rate read back out of its own text are equal, so they
    // are required to hash equally however differently they were reached
    val reparsed: FxRate = FxRate
      .parse(Show[FxRate].show(test))
      .fold(
        failure => fail(s"Expected a rate but parsing failed with: ${failure.message}"),
        rate => rate)
    Hash[FxRate].eqv(test, reparsed) shouldBe true
    Hash[FxRate].hash(test) shouldBe Hash[FxRate].hash(reparsed)
  }

  //-------------------------------------------------------------------------
  /**
   * Reads the rate out of an outcome expected to have produced one, reporting a fixture that fails
   * to build as a test failure naming every reason it failed.
   */
  private def unwrap(outcome: ResultNec[FxRate]): FxRate =
    outcome.fold(
      failures =>
        fail(
          "Expected a rate but the factory failed with: " +
            failures.toChain.toList.map(failure => failure.message).mkString(", ")),
      rate => rate)

  /** Builds a rate from two currencies and a rate, failing the test if they describe none. */
  private def rateOf(base: Currency, counter: Currency, rate: Double): FxRate =
    unwrap(FxRate.of(base, counter, rate))

  /** Builds a rate from a currency pair and a rate, failing the test if they describe none. */
  private def rateOf(pair: CurrencyPair, rate: Double): FxRate = unwrap(FxRate.of(pair, rate))

  /**
   * Reads the failures out of an outcome expected to have produced no rate: the matchers hold when
   * '''some''' failure satisfies what was asked, so asserting that two broken constraints are both
   * reported, in order, needs the chain itself.
   */
  private def failuresOf(outcome: ResultNec[FxRate]): List[Failure] =
    outcome.fold(
      failures => failures.toChain.toList,
      rate => fail(s"Expected a failure but the factory built the rate $rate"))

  /**
   * Reads the decimal out of an outcome expected to have produced one. It is a second helper rather
   * than an overload of [[unwrap]] because the two error channels are the same type after erasure.
   */
  private def decimalOf(outcome: FailureOr[Decimal]): Decimal =
    outcome.fold(
      failure => fail(s"Expected a decimal but it failed with: ${failure.message}"),
      value => value)
}
