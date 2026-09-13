/*
 * Copyright (C) 2021 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.math.BigDecimal
import java.math.RoundingMode

import scala.util.matching.Regex

import cats.Hash
import cats.Order
import cats.Show

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.currency.Currency.AUD
import com.opengamma.strata.basics.currency.Currency.BHD
import com.opengamma.strata.basics.currency.Currency.GBP
import com.opengamma.strata.basics.currency.Currency.JPY
import com.opengamma.strata.basics.currency.Currency.RON
import com.opengamma.strata.basics.currency.Currency.USD
import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[BigMoney]].
 *
 * Two values are equal when their currencies and their exact [[Decimal]] amounts are equal, so no
 * tolerance enters any equality assertion below. Where a failure message is pinned through
 * `Regex.quote`, the comparison is the literal text a log line or a user-facing message carries
 * rather than a pattern that happens to match it.
 *
 * One further test, named descriptively rather than after a method, states the bound this port
 * puts on the work a rejection costs - `parsing decides the shape of the text before it builds
 * anything from it`.
 */
final class BigMoneySpec extends AnyFunSuite with Matchers {

  private val CCY_AUD: Currency = AUD

  private val CCY_RON: Currency = RON

  private val CCY_BHD: Currency = BHD

  private val AMT_100: Double = 100d

  private val AMT_100_12: Double = 100.12d

  private val AMT_100_1249: Double = 100.1249d

  private val AMT_200_2345: Double = 200.2345d

  private val CCYAMT: CurrencyAmount = unwrap(CurrencyAmount.of(CCY_RON, AMT_200_2345))

  private val MONEY_100_RON: BigMoney = unwrap(BigMoney.of(CCY_RON, BigDecimal.valueOf(100L)))

  private val MONEY_200_2345_RON: BigMoney = unwrap(BigMoney.of(CCYAMT))

  private val MONEY_100_AUD: BigMoney = bigMoneyOf(CCY_AUD, AMT_100)

  private val MONEY_100_1249_AUD: BigMoney = bigMoneyOf(CCY_AUD, AMT_100_1249)

  private val MONEY_200_AUD: BigMoney = bigMoneyOf(CCY_AUD, 200d)

  private val MONEY_200_2345_RON_ALTERNATIVE: BigMoney = bigMoneyOf(CCY_RON, AMT_200_2345)

  private val MONEY_100_12_BHD: BigMoney = bigMoneyOf(CCY_BHD, AMT_100_12)

  private val MONEY_100_1249_BHD: BigMoney = bigMoneyOf(CCY_BHD, AMT_100_1249)

  private val ANOTHER_TYPE: Any = ""

  //-------------------------------------------------------------------------
  private val AddMismatchMessage: String = "Unable to add amounts in different currencies"

  private val SubtractMismatchMessage: String =
    "Unable to subtract amounts in different currencies"

  /** One wording covers all four comparison predicates. */
  private val CompareMismatchMessage: String =
    "Unable to compare amounts in different currencies"

  private val NonUnitRateMessage: String = "FX rate must be 1 when no conversion required"

  //-------------------------------------------------------------------------
  /**
   * Every route into the type rounds the amount to twelve decimal places, half up: an amount
   * naming more places is shortened rather than rejected, where [[Money]] rounds the same amount
   * to the minor units its currency quotes.
   */
  test("testOfCurrencyAndAmount") {
    MONEY_100_AUD.currency shouldBe CCY_AUD
    MONEY_100_AUD.getValue shouldBe decimalOf(AMT_100)
    MONEY_100_AUD.getAmount shouldBe BigDecimal.valueOf(10000L, 2)
    MONEY_100_1249_AUD.currency shouldBe CCY_AUD
    MONEY_100_1249_AUD.getValue shouldBe decimalOf(AMT_100_1249)
    MONEY_100_1249_AUD.getAmount shouldBe BigDecimal.valueOf(1001249L, 4)
    MONEY_100_12_BHD.currency shouldBe CCY_BHD
    MONEY_100_12_BHD.getValue shouldBe decimalOf(AMT_100_12)
    MONEY_100_12_BHD.getAmount shouldBe BigDecimal.valueOf(100120L, 3)
    MONEY_100_1249_BHD.currency shouldBe CCY_BHD
    MONEY_100_1249_BHD.getValue shouldBe decimalOf(AMT_100_1249)
    MONEY_100_1249_BHD.getAmount shouldBe BigDecimal.valueOf(1001249L, 4)

    MONEY_100_RON.currency shouldBe CCY_RON
    MONEY_100_RON.getValue shouldBe decimalOf(AMT_100)
    unwrap(BigMoney.of(CCY_RON, AMT_100)) shouldBe MONEY_100_RON
    BigMoney.of(CCY_RON, decimalOf("100")) shouldBe MONEY_100_RON
    BigMoney.of(CCY_RON, Decimal.ZERO) shouldBe BigMoney.zero(CCY_RON)

    BigMoney.of(GBP, decimalOf("1.123456789012345")).getValue shouldBe decimalOf("1.123456789012")
    BigMoney.of(GBP, decimalOf("1.0000000000005")).getValue shouldBe decimalOf("1.000000000001")
    unwrap(BigMoney.of(GBP, 1.123456789012345d)).getValue shouldBe decimalOf("1.123456789012")
    unwrap(BigMoney.of(GBP, new BigDecimal("2.0000000000005"))).getValue shouldBe
      decimalOf("2.000000000001")

    Money.of(GBP, decimalOf("1.123456789012345")).amount shouldBe decimalOf("1.12")

    BigMoney.of(GBP, Double.NaN) should beFailureWith(FailureReason.INVALID)
  }

  /**
   * Widening a [[Money]] is total and loses nothing: its amount is already at the minor units of
   * its currency, and no currency of the reference data quotes more than three digits.
   */
  test("testOfCurrencyAmount") {
    MONEY_200_2345_RON.currency shouldBe CCY_RON
    MONEY_200_2345_RON.getValue shouldBe decimalOf(AMT_200_2345)

    val money: Money = Money.of(CCY_RON, decimalOf(AMT_200_2345))
    BigMoney.of(money).getValue shouldBe decimalOf("200.23")
    BigMoney.of(money) shouldBe bigMoneyOf(CCY_RON, 200.23d)

    CurrencyAmount
      .of(CCY_RON, Double.PositiveInfinity)
      .flatMap(amount => BigMoney.of(amount)) should beFailureWith(FailureReason.INVALID)
  }

  //-------------------------------------------------------------------------
  /**
   * A sum never needs re-rounding: both operands already have at most twelve decimal places, and
   * so does their exact sum. [[BigMoney.map]] is the route that can reach a finer amount.
   */
  test("testPlus") {
    val a: BigMoney = bigMoneyOf(GBP, 1.23d)
    val b: BigMoney = bigMoneyOf(GBP, 2.34d)
    a.plus(b) should haveValue(bigMoneyOf(GBP, 3.57d))
    b.plus(a) should haveValue(bigMoneyOf(GBP, 3.57d))

    BigMoney
      .of(GBP, decimalOf("0.000000000001"))
      .plus(BigMoney.of(GBP, decimalOf("0.000000000002"))) should
      haveValue(BigMoney.of(GBP, decimalOf("0.000000000003")))

    val mismatch: FailureOr[BigMoney] = a.plus(bigMoneyOf(USD, 1d))
    mismatch should beFailureWith(FailureReason.INVALID)
    mismatch should haveFailureMessageMatching(Regex.quote(AddMismatchMessage))
  }

  test("testMinus") {
    val a: BigMoney = bigMoneyOf(GBP, 1.23d)
    val b: BigMoney = bigMoneyOf(GBP, 0.34d)
    a.minus(b) should haveValue(bigMoneyOf(GBP, 0.89d))
    b.minus(a) should haveValue(bigMoneyOf(GBP, -0.89d))

    val mismatch: FailureOr[BigMoney] = a.minus(bigMoneyOf(USD, 1d))
    mismatch should beFailureWith(FailureReason.INVALID)
    mismatch should haveFailureMessageMatching(Regex.quote(SubtractMismatchMessage))
  }

  /** A whole multiplier cannot move an amount past the scale it came with. */
  test("testMultipliedBy") {
    val a: BigMoney = bigMoneyOf(GBP, 1.23d)
    a.multipliedBy(2L) shouldBe bigMoneyOf(GBP, 2.46d)

    BigMoney.of(GBP, decimalOf("1.000000000001")).multipliedBy(3L).getValue shouldBe
      decimalOf("3.000000000003")
    a.multipliedBy(0L) shouldBe BigMoney.zero(GBP)
  }

  /**
   * Mapping is where the twelve-place rounding is observable: a third of `GBP 10` has an
   * unbounded fraction, and the amount produced is that fraction rounded to twelve places.
   */
  test("testMapAmount") {
    val a: BigMoney = bigMoneyOf(GBP, 1.23d)
    a.map(amount => amount.multipliedBy(decimalOf("10"))) shouldBe
      BigMoney.of(GBP, decimalOf("12.30"))
    a.mapAmount(amount => amount.multiply(BigDecimal.TEN)) should
      haveValue(BigMoney.of(GBP, decimalOf("12.30")))

    bigMoneyOf(GBP, 10d).map(amount => amount.dividedBy(decimalOf("3"))).getValue shouldBe
      decimalOf("3.333333333333")

    a.mapAmount(amount => amount.multiply(new BigDecimal("1E+30"))) should
      beFailureWith(FailureReason.INVALID)
  }

  //-------------------------------------------------------------------------
  /**
   * Each of the four predicates answers with an outcome rather than a `Boolean`, because it
   * refuses a pair in two different currencies, so a `true` is `haveValue(true)` - an outcome
   * carrying a failure is neither `true` nor `false`.
   */
  test("testCompare") {
    bigMoneyOf(GBP, 1.000009d).isGreaterThan(bigMoneyOf(GBP, 1d)) should haveValue(true)
    bigMoneyOf(GBP, 1.000009d).isGreaterThanEqualTo(bigMoneyOf(GBP, 1d)) should haveValue(true)
    bigMoneyOf(GBP, 1d).isGreaterThanEqualTo(bigMoneyOf(GBP, 1d)) should haveValue(true)
    bigMoneyOf(GBP, 1d).isGreaterThan(bigMoneyOf(GBP, 1.000009d)) should haveValue(false)
    bigMoneyOf(GBP, 1d).isGreaterThanEqualTo(bigMoneyOf(GBP, 1.000009d)) should haveValue(false)

    bigMoneyOf(GBP, 9.99999999d).isLessThan(bigMoneyOf(GBP, 10d)) should haveValue(true)
    bigMoneyOf(GBP, 9.99999999d).isLessThanEqualTo(bigMoneyOf(GBP, 10d)) should haveValue(true)
    bigMoneyOf(GBP, 10d).isLessThanEqualTo(bigMoneyOf(GBP, 10d)) should haveValue(true)
    bigMoneyOf(GBP, 10d).isLessThan(bigMoneyOf(GBP, 9.999999999d)) should haveValue(false)
    bigMoneyOf(GBP, 10d).isLessThanEqualTo(bigMoneyOf(GBP, 9.999999999d)) should haveValue(false)

    bigMoneyOf(GBP, 1d).isGreaterThan(bigMoneyOf(GBP, 1d)) should haveValue(false)
    bigMoneyOf(GBP, 1d).isLessThan(bigMoneyOf(GBP, 1d)) should haveValue(false)
  }

  /**
   * `GBP 1` and `USD 1` stand in no order without an exchange rate, so all four predicates
   * decline rather than compare the bare numbers. [[BigMoney.order]] has the other answer: it
   * orders any two values, mixed currencies included, which `testCompareTo` asserts.
   */
  test("testCompareWithDifferentCurrencies") {
    val greaterThan: FailureOr[Boolean] =
      bigMoneyOf(GBP, 1.000009d).isGreaterThan(bigMoneyOf(USD, 1d))
    greaterThan should beFailureWith(FailureReason.INVALID)
    greaterThan should haveFailureMessageMatching(Regex.quote(CompareMismatchMessage))

    val greaterThanEqualTo: FailureOr[Boolean] =
      bigMoneyOf(GBP, 1.000009d).isGreaterThanEqualTo(bigMoneyOf(USD, 1d))
    greaterThanEqualTo should beFailureWith(FailureReason.INVALID)
    greaterThanEqualTo should haveFailureMessageMatching(Regex.quote(CompareMismatchMessage))

    val lessThanEqualTo: FailureOr[Boolean] =
      bigMoneyOf(GBP, 10d).isLessThanEqualTo(bigMoneyOf(USD, 9.999999d))
    lessThanEqualTo should beFailureWith(FailureReason.INVALID)
    lessThanEqualTo should haveFailureMessageMatching(Regex.quote(CompareMismatchMessage))

    val lessThan: FailureOr[Boolean] = bigMoneyOf(GBP, 10d).isLessThan(bigMoneyOf(USD, 9.999999d))
    lessThan should beFailureWith(FailureReason.INVALID)
    lessThan should haveFailureMessageMatching(Regex.quote(CompareMismatchMessage))
  }

  //-------------------------------------------------------------------------
  /**
   * [[BigMoney.roundToScale]] applies the mode it is asked for rather than the half up the type
   * applies to its own amounts, and a scale of twelve or more leaves the amount alone, since it
   * has no digit beyond the twelfth to lose.
   */
  test("testRoundingWithPositiveScale") {
    bigMoneyOf(GBP, 1.441d).roundToScale(2, RoundingMode.CEILING) shouldBe bigMoneyOf(GBP, 1.45d)
    bigMoneyOf(GBP, 1.441d).roundToScale(2, RoundingMode.UP) shouldBe bigMoneyOf(GBP, 1.45d)
    bigMoneyOf(GBP, 1.446d).roundToScale(2, RoundingMode.HALF_UP) shouldBe bigMoneyOf(GBP, 1.45d)

    bigMoneyOf(GBP, 1.449d).roundToScale(2, RoundingMode.FLOOR) shouldBe bigMoneyOf(GBP, 1.44d)
    bigMoneyOf(GBP, 1.449d).roundToScale(2, RoundingMode.DOWN) shouldBe bigMoneyOf(GBP, 1.44d)
    bigMoneyOf(GBP, 1.444d).roundToScale(2, RoundingMode.HALF_DOWN) shouldBe
      bigMoneyOf(GBP, 1.44d)

    val fine: BigMoney = BigMoney.of(GBP, decimalOf("1.000000000001"))
    fine.roundToScale(12, RoundingMode.UP) shouldBe fine
    fine.roundToScale(18, RoundingMode.UP) shouldBe fine
  }

  /**
   * A negative scale reaches the whole part: `-3` rounds to a multiple of a thousand, `-2` to a
   * hundred and `-1` to ten. A scale clamped at zero would return each amount unchanged and so
   * would satisfy none of these rows, while the positive-scale rows above would still pass.
   */
  test("testRoundingWithNegativeScale") {
    bigMoneyOf(GBP, 780001d).roundToScale(-3, RoundingMode.CEILING) shouldBe
      bigMoneyOf(GBP, 781000d)
    bigMoneyOf(GBP, 780001d).roundToScale(-2, RoundingMode.UP) shouldBe bigMoneyOf(GBP, 780100d)
    bigMoneyOf(GBP, 780005d).roundToScale(-1, RoundingMode.HALF_UP) shouldBe
      bigMoneyOf(GBP, 780010d)

    bigMoneyOf(GBP, 780999d).roundToScale(-3, RoundingMode.FLOOR) shouldBe
      bigMoneyOf(GBP, 780000d)
    bigMoneyOf(GBP, 780699d).roundToScale(-2, RoundingMode.DOWN) shouldBe
      bigMoneyOf(GBP, 780600d)
    bigMoneyOf(GBP, 780234d).roundToScale(-1, RoundingMode.HALF_DOWN) shouldBe
      bigMoneyOf(GBP, 780230d)
  }

  //-------------------------------------------------------------------------
  /** A zero returns itself from all three sign-changing methods. */
  test("testZeroPositiveNegative") {
    val zero: BigMoney = BigMoney.zero(GBP)
    val positive: BigMoney = bigMoneyOf(GBP, 200.23d)
    val negative: BigMoney = bigMoneyOf(GBP, -200.23d)

    zero.isZero shouldBe true
    zero.isPositive shouldBe false
    zero.isNegative shouldBe false
    zero.negated shouldBe zero
    zero.positive shouldBe zero
    zero.negative shouldBe zero

    positive.isZero shouldBe false
    positive.isPositive shouldBe true
    positive.isNegative shouldBe false
    positive.negated shouldBe negative
    positive.positive shouldBe positive
    positive.negative shouldBe negative

    negative.isZero shouldBe false
    negative.isPositive shouldBe false
    negative.isNegative shouldBe true
    negative.negated shouldBe positive
    negative.positive shouldBe positive
    negative.negative shouldBe negative
  }


  //-------------------------------------------------------------------------
  /**
   * [[BigMoney.of]] and [[CurrencyAmount.toBigMoney]] are one conversion under two names, and
   * both answer with an outcome, because a `Double` amount may be one no decimal holds.
   */
  test("testToCurrencyAmount") {
    val base: BigMoney = bigMoneyOf(GBP, 200.23d)
    base.toCurrencyAmount shouldBe unwrap(CurrencyAmount.of(GBP, 200.23d))
    base.toCurrencyAmount.toBigMoney should haveValue(base)
    BigMoney.of(base.toCurrencyAmount) should haveValue(base)
  }

  //-------------------------------------------------------------------------
  /**
   * The amount is multiplied by the rate in exact decimal arithmetic and the product is rounded
   * to twelve places, not to the minor units of the result currency, so a rate with more digits
   * than that currency quotes is applied in full.
   */
  test("testConvertedToWithExplicitRate") {
    MONEY_200_2345_RON.convertedTo(CCY_RON, decimalOf("1")) should
      haveValue(bigMoneyOf(CCY_RON, 200.2345d))
    MONEY_100_AUD.convertedTo(CCY_RON, decimalOf("2.6031")) should
      haveValue(bigMoneyOf(CCY_RON, 260.31d))
    MONEY_100_AUD.convertedTo(CCY_RON, new BigDecimal("2.6031")) should
      haveValue(bigMoneyOf(CCY_RON, 260.31d))

    MONEY_100_AUD.convertedTo(CCY_RON, decimalOf("2.60315")) should
      haveValue(bigMoneyOf(CCY_RON, 260.315d))

    MONEY_100_AUD.convertedTo(CCY_RON, new BigDecimal("1E+30")) should
      beFailureWith(FailureReason.INVALID)
  }

  /**
   * A conversion into the value's own currency performs no arithmetic, so a rate of one returns
   * the value as it stands and any other rate is refused rather than silently applied, which
   * keeps a caller from scaling an amount through a conversion that does not happen. The rate is
   * compared with one to a tolerance of `1e-8`, so a rate a billionth away is accepted and a rate
   * a ten-millionth away is refused.
   */
  test("testConvertedToWithExplicitRateForSameCurrency") {
    val refused: FailureOr[BigMoney] = MONEY_200_2345_RON.convertedTo(CCY_RON, decimalOf("1.1"))
    refused should beFailureWith(FailureReason.INVALID)
    refused should haveFailureMessageMatching(Regex.quote(NonUnitRateMessage))

    MONEY_200_2345_RON.convertedTo(CCY_RON, decimalOf("1.000000001")) should
      haveValue(MONEY_200_2345_RON)
    MONEY_200_2345_RON.convertedTo(CCY_RON, decimalOf("0.999999999")) should
      haveValue(MONEY_200_2345_RON)

    MONEY_200_2345_RON.convertedTo(CCY_RON, decimalOf("1.0000001")) should
      beFailureWith(FailureReason.INVALID)
    MONEY_200_2345_RON.convertedTo(CCY_RON, new BigDecimal("1.1")) should
      haveFailureMessageMatching(Regex.quote(NonUnitRateMessage))
  }

  /**
   * A value already in the requested currency is returned without the provider being consulted,
   * so the identity conversion succeeds even under a provider holding no rate; a cross-currency
   * conversion reports the provider's own failure rather than inventing a rate.
   */
  test("testConvertedToWithRateProvider") {
    val provider: FxRateProvider = FxRateProvider.fromFunction((_, _) => Right(2.5d))
    MONEY_100_AUD.convertedTo(CCY_RON, provider) should haveValue(bigMoneyOf(CCY_RON, 250d))
    MONEY_200_2345_RON.convertedTo(CCY_RON, provider) should haveValue(MONEY_200_2345_RON)

    MONEY_200_2345_RON.convertedTo(CCY_RON, FxRateProvider.noConversion()) should
      haveValue(MONEY_200_2345_RON)

    MONEY_100_AUD.convertedTo(CCY_RON, FxRateProvider.noConversion()) should
      beFailureWith(FailureReason.CURRENCY_CONVERSION)
  }

  /**
   * [[BigMoney.toMoney]] narrows to the minor units of the amount's own currency - three digits
   * for the Bahraini dinar, none for the yen - and rounds a tie there '''half up''': `GBP 1.005`
   * narrows to `GBP 1.01`. That tie exists only because the amount is held exactly; the nearest
   * `Double` to `1.005` lies below it, at `1.00499999999999989…`, and would round down.
   */
  test("testTo") {
    MONEY_100_AUD.toMoney.toBigMoney shouldBe MONEY_100_AUD
    MONEY_200_AUD.toMoney.toBigMoney shouldBe MONEY_200_AUD

    BigMoney.of(MONEY_100_AUD.toMoney) shouldBe MONEY_100_AUD.toMoney.toBigMoney
    Money.of(MONEY_100_AUD) shouldBe MONEY_100_AUD.toMoney

    val tie: BigMoney = BigMoney.of(GBP, decimalOf("1.005"))
    tie.toMoney.amount shouldBe decimalOf("1.01")
    tie.toMoney.getAmount shouldBe new BigDecimal("1.01")
    tie.toMoney.toString shouldBe "GBP 1.01"

    BigMoney.of(CCY_BHD, decimalOf("1234.567890123457")).toMoney.amount shouldBe
      decimalOf("1234.568")
    BigMoney.of(JPY, decimalOf("1234.567890123457")).toMoney.amount shouldBe decimalOf("1235")

    BigMoney.of(MONEY_100_12_BHD.toMoney) shouldBe MONEY_100_12_BHD
  }

  //-------------------------------------------------------------------------
  /**
   * Values order by currency and then by amount, and only the sign of a comparison is part of the
   * contract. The order agrees with equality exactly - `compare` is zero precisely when the two
   * values are equal - and it answers for two values in different currencies, unlike the four
   * predicates above, because a total order is what sorting a mixed collection needs.
   */
  test("testCompareTo") {
    MONEY_100_AUD.compareTo(MONEY_200_2345_RON) should be < 0
    MONEY_200_2345_RON.compareTo(MONEY_100_AUD) should be > 0
    MONEY_200_2345_RON.compareTo(MONEY_200_2345_RON_ALTERNATIVE) shouldBe 0

    MONEY_100_AUD.compareTo(MONEY_200_AUD) should be < 0
    MONEY_200_AUD.compareTo(MONEY_100_AUD) should be > 0
    MONEY_100_AUD.compareTo(MONEY_100_RON) should be < 0

    Order[BigMoney].compare(MONEY_100_AUD, MONEY_200_2345_RON) should be < 0
    Order[BigMoney].compare(MONEY_200_2345_RON, MONEY_200_2345_RON_ALTERNATIVE) shouldBe 0
    Order[BigMoney].eqv(MONEY_200_2345_RON, MONEY_200_2345_RON_ALTERNATIVE) shouldBe true
    Order[BigMoney].eqv(MONEY_100_AUD, MONEY_100_RON) shouldBe false
    Order[BigMoney].max(MONEY_100_AUD, MONEY_200_AUD) shouldBe MONEY_200_AUD
  }

  /**
   * Equality is that of the currency and the amount, exactly: the amount is a normalised
   * [[Decimal]] that compares as a number, so `200` and `200.00` are one value and no tolerance
   * or special case enters the comparison. [[Hash]] answers as the methods do.
   */
  test("testEqualsHashCode") {
    MONEY_200_2345_RON.equals(MONEY_200_2345_RON) shouldBe true
    MONEY_200_2345_RON.equals(MONEY_200_2345_RON_ALTERNATIVE) shouldBe true
    MONEY_200_2345_RON_ALTERNATIVE.equals(MONEY_200_2345_RON) shouldBe true
    MONEY_200_2345_RON.equals(MONEY_100_AUD) shouldBe false
    MONEY_200_2345_RON.equals(MONEY_200_AUD) shouldBe false
    MONEY_200_2345_RON.equals(MONEY_100_RON) shouldBe false
    MONEY_200_2345_RON.equals(MONEY_100_12_BHD) shouldBe false
    MONEY_200_2345_RON.equals(ANOTHER_TYPE) shouldBe false
    ANOTHER_TYPE.equals(MONEY_200_2345_RON) shouldBe false
    MONEY_200_2345_RON.hashCode shouldBe MONEY_200_2345_RON_ALTERNATIVE.hashCode

    BigMoney.of(CCY_AUD, decimalOf("200")) shouldBe MONEY_200_AUD
    BigMoney.of(CCY_AUD, decimalOf("200.00")) shouldBe MONEY_200_AUD
    BigMoney.of(CCY_AUD, decimalOf("200.00")).hashCode shouldBe MONEY_200_AUD.hashCode

    Hash[BigMoney].eqv(MONEY_200_2345_RON, MONEY_200_2345_RON_ALTERNATIVE) shouldBe true
    Hash[BigMoney].eqv(MONEY_200_2345_RON, MONEY_100_RON) shouldBe false
    Hash[BigMoney].hash(MONEY_200_2345_RON) shouldBe
      Hash[BigMoney].hash(MONEY_200_2345_RON_ALTERNATIVE)
  }

  //-------------------------------------------------------------------------
  /**
   * The text form is the currency code and the amount padded to at least the digits the currency
   * quotes, with any further place shown in full rather than truncated.
   */
  test("testToString") {
    MONEY_200_AUD.toString shouldBe "AUD 200.00"
    MONEY_200_2345_RON.toString shouldBe "RON 200.2345"
    MONEY_100_12_BHD.toString shouldBe "BHD 100.120"
    MONEY_100_1249_BHD.toString shouldBe "BHD 100.1249"

    Show[BigMoney].show(MONEY_200_AUD) shouldBe "AUD 200.00"
    Show[BigMoney].show(MONEY_200_2345_RON) shouldBe MONEY_200_2345_RON.toString
    BigMoney.parse(MONEY_200_AUD.toString) should haveValue(MONEY_200_AUD)
    BigMoney.parse(MONEY_200_2345_RON.toString) should haveValue(MONEY_200_2345_RON)
  }

  //-------------------------------------------------------------------------
  /**
   * [[BigMoney.parse]] normalises rather than rejects: text naming more than twelve decimal
   * places is read as the amount rounded to twelve. The currency code is case insensitive, since
   * it is resolved through [[Currency.parse]].
   */
  test("testParse") {
    BigMoney.parse("RON 200.2345") should haveValue(MONEY_200_2345_RON)
    unwrap(BigMoney.parse("AUD 1.123456789012345")) shouldBe
      unwrap(BigMoney.parse("AUD 1.123456789012"))
    unwrap(BigMoney.parse("AUD 1.123456789012345")).getValue shouldBe decimalOf("1.123456789012")
    BigMoney.parse("ron 200.2345") should haveValue(MONEY_200_2345_RON)
  }

  /**
   * `200.23 RON` falls into exactly two space-separated parts, so it passes the shape check and
   * is rejected for the currency its first part does not name.
   */
  test("testParseWrongFormat") {
    val outcome: FailureOr[BigMoney] = BigMoney.parse("200.23 RON")
    outcome should beFailureWith(FailureReason.PARSING)
    outcome should haveFailureMessageMatching(Regex.quote("Unable to parse amount: 200.23 RON"))
  }

  /**
   * `$100` holds no space at all, so it is rejected on its shape, with the other of the two
   * wordings, before anything is read from it. The expected text is written as a pattern, with
   * the currency symbol in a character class rather than quoted whole.
   */
  test("testParseWrongElementsNumber") {
    val outcome: FailureOr[BigMoney] = BigMoney.parse("$100")
    outcome should beFailureWith(FailureReason.PARSING)
    outcome should haveFailureMessageMatching("Unable to parse amount, invalid format: [$]100")
  }

  /**
   * Asserts that the shape of the text is decided before anything is built from it.
   *
   * No counterpart in the Java test class, and none in the port until now: the implementation
   * being ported - and this port's first reading of it - split the text on every separator it
   * held and then looked at how many pieces there were, so the work of rejecting text was
   * proportional to the separators in it and text of two hundred thousand spaces built two
   * hundred thousand strings for the sole purpose of counting them (CWE-400/CWE-770). The parse
   * now locates the single separator by index and takes at most the two substrings the grammar
   * has.
   *
   * The point of this case is that the change is invisible in every observable respect, so the
   * six shapes either side of the boundary are asserted to read exactly as the split read them:
   * one separator is the two-part form, a trailing separator is the two-part form with an empty
   * amount - which is refused for naming no decimal rather than for its shape - and no separator
   * or two separators is the malformed form. The hostile payload closes it by asserting that
   * such text reaches the same wording, which is what says the shape was decided rather than
   * derived from a decomposition.
   */
  test("parsing decides the shape of the text before it builds anything from it") {
    // one separator: the two-part form, read as it always was
    BigMoney.parse("RON 200.2345").map(value => value.toString) should haveValue("RON 200.2345")

    // a trailing separator is still two parts, the second of them empty, so the text is refused
    // for naming no decimal - the right-shape wording - and not for its shape
    val trailing: FailureOr[BigMoney] = BigMoney.parse("RON ")
    trailing should beFailureWith(FailureReason.PARSING)
    trailing.left.toOption.map(failure => failure.message) shouldBe Some("Unable to parse amount: RON ")

    // a leading separator is likewise two parts, the first of them naming no currency
    val leading: FailureOr[BigMoney] = BigMoney.parse(" 1")
    leading.left.toOption.map(failure => failure.message) shouldBe Some("Unable to parse amount:  1")

    // no separator, and every arrangement of two, is the malformed form
    val malformed: List[String] = List("RON", "RON 1 2", " RON 1", "RON  1")
    malformed.foreach { input =>
      val outcome: FailureOr[BigMoney] = BigMoney.parse(input)
      outcome should beFailureWith(FailureReason.PARSING)
      outcome.left.toOption.map(failure => failure.message) shouldBe
        Some(s"Unable to parse amount, invalid format: $input")
    }

    // and text written to be hostile - two hundred thousand separators, which is two hundred
    // thousand and one parts - reaches that same wording, naming the text in full as every
    // rejection of this port does; bounding what a reader sees belongs to the rendering of the
    // failure, which the neighbouring suites pin.
    val hostile: String = " " * 200000
    val refused: FailureOr[BigMoney] = BigMoney.parse(hostile)
    refused should beFailureWith(FailureReason.PARSING)
    refused.left.toOption.map(failure => failure.message) shouldBe
      Some(s"Unable to parse amount, invalid format: $hostile")
  }

  //-------------------------------------------------------------------------
  /** Returns the value an outcome carries, failing the test with its reason if it carries none. */
  private def unwrap[A](outcome: FailureOr[A]): A =
    outcome.fold(
      failure => fail(s"Expected a value but the operation failed with: ${failure.message}"),
      value => value)

  /** Builds a value, failing the test if the arguments describe none. */
  private def bigMoneyOf(currency: Currency, amount: Double): BigMoney =
    unwrap(BigMoney.of(currency, amount))

  /** Builds a decimal from text, which states the exact digits a row asserts. */
  private def decimalOf(text: String): Decimal = unwrap(Decimal.of(text))

  /** Builds a decimal from a `Double`, reading the decimal its shortest text names. */
  private def decimalOf(value: Double): Decimal = unwrap(Decimal.of(value))

}
