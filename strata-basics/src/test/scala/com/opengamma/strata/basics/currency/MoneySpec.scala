/*
 * Copyright (C) 2017 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.math.BigDecimal

import scala.util.matching.Regex

import cats.Hash
import cats.Order
import cats.Show

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.currency.Currency.GBP
import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.FixedScaleDecimal
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[Money]].
 *
 * A money value is never off-scale: every route into the type - the four factories, the text form,
 * the arithmetic, both mapping methods and both conversions - passes through one private creation
 * step that rounds the amount to the minor units of the currency, half up, so a result finer than
 * the currency's quote is rounded and not only a construction is. The rounding is exercised over
 * all three minor-unit widths the reference data holds - none for the yen, two for the Australian
 * dollar and the Romanian leu, three for the Bahraini dinar - so the width is read from the
 * currency rather than assumed.
 *
 * One further test, named descriptively rather than after a method, states the bound this port
 * puts on the work a rejection costs - `parsing decides the shape of the text before it builds
 * anything from it`.
 *
 * A conversion into the currency a value already has requires a rate of one within `1e-8`; `Order`
 * is the currency code and then the amount, and returns zero exactly when the two values are
 * equal. Factories, arithmetic, the text route and the conversions report failure as a `Left`
 * value, so every fixture here is unwrapped once through [[moneyOf]] or [[unwrap]].
 */
final class MoneySpec extends AnyFunSuite with Matchers {

  /** The Australian dollar, which quotes two minor unit digits. */
  private val CCY_AUD: Currency = Currency.AUD

  /** The Romanian leu, which quotes two minor unit digits. */
  private val CCY_RON: Currency = Currency.RON

  /** The Bahraini dinar, which quotes three minor unit digits. */
  private val CCY_BHD: Currency = Currency.BHD

  /** An amount at the two-digit scale. */
  private val AMT_100_12: Double = 100.12d

  /** The same amount rounded up at the two-digit scale, which `AMT_100_125` rounds to there. */
  private val AMT_100_13: Double = 100.13d

  /** An amount one digit finer than the dinar quotes, to be rounded at both scales. */
  private val AMT_100_1249: Double = 100.1249d

  /** An amount exactly at the dinar's scale, and a tie to be rounded up at the dollar's. */
  private val AMT_100_125: Double = 100.125d

  /** The amount of the leu fixtures. */
  private val AMT_200_23: Double = 200.23d

  /** The inexact amount the [[Money.of]] overload taking one is built from. */
  private val CCYAMT: CurrencyAmount = unwrap(CurrencyAmount.of(CCY_RON, AMT_200_23))

  /** `RON 100.00`, built through the `BigDecimal` factory. */
  private val MONEY_100_RON: Money = unwrap(Money.of(CCY_RON, BigDecimal.valueOf(100L)))

  /** `RON 200.23`, built from the inexact amount. */
  private val MONEY_200_23_RON: Money = unwrap(Money.of(CCYAMT))

  private val MONEY_100_12_AUD: Money = moneyOf(CCY_AUD, AMT_100_12)

  /** `AUD 200.00`, whose amount is a whole number and whose text form is nonetheless padded. */
  private val MONEY_200_AUD: Money = moneyOf(CCY_AUD, 200d)

  /** `RON 200.23` built by the other route, for the equality and comparison matrices. */
  private val MONEY_200_23_RON_ALTERNATIVE: Money = moneyOf(CCY_RON, AMT_200_23)

  /** `BHD 100.120`, an amount coarser than the dinar quotes, which is padded rather than moved. */
  private val MONEY_100_120_BHD: Money = moneyOf(CCY_BHD, AMT_100_12)

  /** `BHD 100.125`, an amount exactly at the dinar's scale. */
  private val MONEY_100_125_BHD: Money = moneyOf(CCY_BHD, AMT_100_125)

  /** A value of a type unrelated to a money value, for the equality assertion that needs one. */
  private val ANOTHER_TYPE: Any = ""

  private val DifferentCurrenciesAddMessage: String =
    "Unable to add amounts in different currencies"

  private val DifferentCurrenciesSubtractMessage: String =
    "Unable to subtract amounts in different currencies"

  private val NonUnitRateMessage: String = "FX rate must be 1 when no conversion required"

  //-------------------------------------------------------------------------
  /**
   * Asserts the three number-taking factories and the rounding every one of them applies.
   *
   * The two accessors are two renderings of one amount, so both are asserted for each fixture. The
   * rounding is then stated over each of the three minor-unit widths the currency data holds; the
   * yen's width of zero is the case in which it reaches the whole part of the amount, so
   * `JPY 100.5` rounds away from zero to `JPY 101`. The [[Decimal]] factory is the one route that
   * is total, because a decimal is already a value money can hold.
   */
  test("testOfCurrencyAndAmount") {
    MONEY_200_AUD.currency shouldBe CCY_AUD
    MONEY_200_AUD.getValue should haveValue(fixedScale(decimal(200d), 2))
    MONEY_200_AUD.getAmount shouldBe BigDecimal.valueOf(20000L, 2)
    MONEY_100_12_AUD.currency shouldBe CCY_AUD
    MONEY_100_12_AUD.getValue should haveValue(fixedScale(decimal(AMT_100_12), 2))
    MONEY_100_12_AUD.getAmount shouldBe BigDecimal.valueOf(10012L, 2)
    MONEY_100_120_BHD.currency shouldBe CCY_BHD
    MONEY_100_120_BHD.getValue should haveValue(fixedScale(decimal(AMT_100_12), 3))
    MONEY_100_120_BHD.getAmount shouldBe BigDecimal.valueOf(100120L, 3)
    MONEY_100_125_BHD.currency shouldBe CCY_BHD
    MONEY_100_125_BHD.getValue should haveValue(fixedScale(decimal(AMT_100_125), 3))
    MONEY_100_125_BHD.getAmount shouldBe BigDecimal.valueOf(100125L, 3)

    // a digit finer than the currency quotes is dropped, and a tie goes away from zero
    moneyOf(CCY_AUD, AMT_100_1249).getValue should haveValue(fixedScale(decimal(AMT_100_12), 2))
    moneyOf(CCY_AUD, AMT_100_125).getValue should haveValue(fixedScale(decimal(AMT_100_13), 2))
    moneyOf(CCY_BHD, AMT_100_1249).getValue should haveValue(fixedScale(decimal(AMT_100_125), 3))
    moneyOf(CCY_BHD, AMT_100_125).getValue should haveValue(fixedScale(decimal(AMT_100_125), 3))

    // a currency that quotes no minor unit digit, where rounding reaches the whole part
    val yen: Money = moneyOf(Currency.JPY, 100.5d)
    yen.getValue should haveValue(fixedScale(decimal(101d), 0))
    yen.getAmount shouldBe BigDecimal.valueOf(101L, 0)
    moneyOf(Currency.JPY, 100.4d).getValue should haveValue(fixedScale(decimal(100d), 0))
    moneyOf(Currency.JPY, 100.4d).toString shouldBe "JPY 100"

    // the `BigDecimal` factory, given a whole number and then an amount finer than the quote
    MONEY_100_RON.currency shouldBe CCY_RON
    MONEY_100_RON.getValue should haveValue(fixedScale(decimal(100d), 2))
    MONEY_100_RON.getAmount shouldBe BigDecimal.valueOf(10000L, 2)
    Money.of(CCY_RON, new BigDecimal("200.2345")) should haveValue(MONEY_200_23_RON)

    // the decimal factory, which is total and rounds like the rest
    Money.of(CCY_AUD, decimal(100.129d)) shouldBe moneyOf(CCY_AUD, AMT_100_13)
    Money.of(CCY_AUD, decimal(AMT_100_12)) shouldBe MONEY_100_12_AUD

    // a number no decimal holds is reported rather than rounded
    Money.of(CCY_AUD, Double.PositiveInfinity) should beFailureWith(FailureReason.INVALID)
  }

  /**
   * Asserts the factory taking an inexact amount, which is the route that carries an amount of
   * arbitrary precision into the type: it rounds like the others, and its one failure is an amount
   * that is not finite, which [[CurrencyAmount]] admits and no decimal holds.
   */
  test("testOfCurrencyAmount") {
    MONEY_200_23_RON.currency shouldBe CCY_RON
    MONEY_200_23_RON.getValue should haveValue(fixedScale(decimal(AMT_200_23), 2))

    // the inexact amount is rounded to the currency's minor units, as every route is
    val finer: CurrencyAmount = unwrap(CurrencyAmount.of(CCY_RON, 200.2345d))
    Money.of(finer) should haveValue(MONEY_200_23_RON)

    // an infinite amount is one this type does not hold, and it is reported
    val infinite: CurrencyAmount = unwrap(CurrencyAmount.of(CCY_RON, Double.PositiveInfinity))
    Money.of(infinite) should beFailureWith(FailureReason.INVALID)
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts addition, in both directions and in both outcomes: two money values of different
   * currencies have no sum, and that is reported as a `Left` carrying the message pinned below.
   */
  test("testPlus") {
    val a: Money = moneyOf(GBP, 1.23d)
    val b: Money = moneyOf(GBP, 2.34d)
    a.plus(b) should haveValue(moneyOf(GBP, 3.57d))
    b.plus(a) should haveValue(moneyOf(GBP, 3.57d))

    // the sum is at the currency's scale, as every amount of this type is
    unwrap(a.plus(b)).getValue should haveValue(fixedScale(decimal(3.57d), 2))

    val mismatch: FailureOr[Money] = a.plus(moneyOf(Currency.USD, 1d))
    mismatch should beFailureWith(FailureReason.INVALID)
    mismatch should haveFailureMessageMatching(Regex.quote(DifferentCurrenciesAddMessage))
  }

  /** Asserts subtraction, in both directions and in both outcomes, including a negative result. */
  test("testMinus") {
    val a: Money = moneyOf(GBP, 1.23d)
    val b: Money = moneyOf(GBP, 0.34d)
    a.minus(b) should haveValue(moneyOf(GBP, 0.89d))
    b.minus(a) should haveValue(moneyOf(GBP, -0.89d))

    val mismatch: FailureOr[Money] = a.minus(moneyOf(Currency.USD, 1d))
    mismatch should beFailureWith(FailureReason.INVALID)
    mismatch should haveFailureMessageMatching(Regex.quote(DifferentCurrenciesSubtractMessage))
  }

  /**
   * Asserts multiplication by a whole number, which is total in signature: multiplying an amount
   * at the currency's scale by a whole number cannot introduce a finer digit, so this is the one
   * arithmetic route whose result is at that scale for a reason other than the rounding. The
   * multiplier carries the `L` suffix because this build treats an implicit numeric widening as an
   * error.
   */
  test("testMultipliedBy") {
    val a: Money = moneyOf(GBP, 1.23d)
    a.multipliedBy(2L) shouldBe moneyOf(GBP, 2.46d)
    a.multipliedBy(3L) shouldBe moneyOf(GBP, 3.69d)
    a.multipliedBy(0L) shouldBe Money.zero(GBP)
    a.multipliedBy(-1L) shouldBe moneyOf(GBP, -1.23d)
    a.multipliedBy(2L).getValue should haveValue(fixedScale(decimal(2.46d), 2))
  }

  /**
   * Asserts both mapping methods, including a result finer than the currency quotes.
   *
   * A factor of `0.3333` gives the exact product `0.409959`, four digits finer than sterling
   * quotes, and both methods answer `GBP 0.41`: an implementation that rounded only on
   * construction would fail here and nowhere else. `mapAmount` answers with an outcome because a
   * function on `BigDecimal` can return a number needing more than eighteen digits, which the
   * `1E+30` row reports as a `Left`.
   */
  test("testMapAmount") {
    val a: Money = moneyOf(GBP, 1.23d)
    a.map(amount => amount.multipliedBy(decimal(10d))) shouldBe moneyOf(GBP, 12.3d)
    a.mapAmount(amount => amount.multiply(BigDecimal.TEN)) should haveValue(moneyOf(GBP, 12.3d))

    // a result four digits finer than the currency quotes is rounded, by both routes
    a.map(amount => amount.multipliedBy(decimal(0.3333d))) shouldBe moneyOf(GBP, 0.41d)
    a.mapAmount(amount => amount.multiply(new BigDecimal("0.3333"))) should
      haveValue(moneyOf(GBP, 0.41d))
    a.map(amount => amount.multipliedBy(decimal(0.3333d))).getValue should
      haveValue(fixedScale(decimal(0.41d), 2))

    // the currency is carried through, and a currency of another width rounds to its own scale
    val dinar: Money = moneyOf(CCY_BHD, 1.23d)
    dinar.map(amount => amount.multipliedBy(decimal(0.3333d))) shouldBe moneyOf(CCY_BHD, 0.41d)
    dinar.map(amount => amount.multipliedBy(decimal(0.3333d))).currency shouldBe CCY_BHD

    // a result no decimal holds is reported rather than raised or truncated
    val oversized: FailureOr[Money] = a.mapAmount(amount => amount.multiply(new BigDecimal("1E+30")))
    oversized should beFailureWith(FailureReason.INVALID)
    oversized should haveFailureMessageMatching(
      Regex.quote("Decimal value must not exceed 18 digits of precision at scale 0: 1.23E+30"))
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the sign predicates and the three sign-changing methods. Zero is neither positive nor
   * negative and its negation is the value itself, so the sign of a zero cannot depend on how that
   * zero was reached.
   */
  test("testZeroPositiveNegative") {
    val zero: Money = Money.zero(GBP)
    val positive: Money = moneyOf(GBP, 200.23d)
    val negative: Money = moneyOf(GBP, -200.23d)

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
   * Asserts the conversion to an inexact amount and the round trip back, which answers with an
   * outcome because an amount may be infinite and no decimal is.
   *
   * Widening to [[BigMoney]] keeps every digit, since no currency quotes more than twelve places,
   * so that round trip is an identity; narrowing rounds half up, so the twelve-place tie `1.005`
   * moves to `1.01` at sterling's two places.
   */
  test("testToCurrencyAmount") {
    val base: Money = moneyOf(GBP, 200.23d)
    base.toCurrencyAmount shouldBe unwrap(CurrencyAmount.of(GBP, 200.23d))
    base.toCurrencyAmount.toMoney should haveValue(base)
    Money.of(base.toCurrencyAmount) should haveValue(base)

    base.toCurrencyAmount.currency shouldBe GBP
    MONEY_200_AUD.toCurrencyAmount shouldBe unwrap(CurrencyAmount.of(CCY_AUD, 200d))

    base.toBigMoney shouldBe BigMoney.of(base)
    base.toBigMoney.amount shouldBe base.amount
    Money.of(base.toBigMoney) shouldBe base
    base.toBigMoney.toMoney shouldBe base

    val finer: BigMoney = unwrap(BigMoney.of(GBP, new BigDecimal("1.005")))
    Money.of(finer).amount shouldBe finer.toMoney.amount
    Money.of(finer) shouldBe moneyOf(GBP, 1.01d)
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts conversion at a rate given explicitly, through both rate overloads, which have to
   * agree.
   *
   * `100.12` multiplied by `2.6` is exactly `260.312` and the leu quotes two digits, so the answer
   * is `RON 260.31`: the product is rounded to the minor units of the '''result''' currency, which
   * the dinar and yen rows state by converting into currencies of other widths.
   */
  test("testConvertedToWithExplicitRate") {
    MONEY_200_23_RON.convertedTo(CCY_RON, decimal(1d)) should haveValue(moneyOf(CCY_RON, AMT_200_23))
    MONEY_100_12_AUD.convertedTo(CCY_RON, decimal(2.6d)) should haveValue(moneyOf(CCY_RON, 260.31d))
    MONEY_100_12_AUD.convertedTo(CCY_RON, decimal(2.6d).toBigDecimal) should
      haveValue(moneyOf(CCY_RON, 260.31d))

    MONEY_100_12_AUD.convertedTo(CCY_BHD, decimal(2.6d)) should haveValue(moneyOf(CCY_BHD, 260.312d))
    MONEY_100_12_AUD.convertedTo(Currency.JPY, decimal(2.6d)) should
      haveValue(moneyOf(Currency.JPY, 260d))

    // a rate no decimal holds is reported by the `BigDecimal` overload, which reads it as one
    MONEY_100_12_AUD.convertedTo(CCY_RON, new BigDecimal("1e19")) should
      beFailureWith(FailureReason.INVALID)
  }

  /**
   * Asserts the rule for a conversion into the currency the value already has: it needs no
   * arithmetic, so a rate of one returns the value and any other rate is reported rather than
   * silently applied, which keeps a caller from scaling an amount through a conversion that does
   * not happen. The comparison against one is fuzzy with a tolerance of `1e-8`, so the boundary is
   * asserted on both sides.
   */
  test("testConvertedToWithExplicitRateForSameCurrency") {
    val rejected: FailureOr[Money] = MONEY_200_23_RON.convertedTo(CCY_RON, decimal(1.1d))
    rejected should beFailureWith(FailureReason.INVALID)
    rejected should haveFailureMessageMatching(Regex.quote(NonUnitRateMessage))

    MONEY_200_23_RON.convertedTo(CCY_RON, decimal(1.000000001d)) should
      haveValue(MONEY_200_23_RON)
    MONEY_200_23_RON.convertedTo(CCY_RON, decimal(0.999999999d)) should
      haveValue(MONEY_200_23_RON)

    MONEY_200_23_RON.convertedTo(CCY_RON, decimal(1.0000001d)) should
      beFailureWith(FailureReason.INVALID)

    // the same rule through the `BigDecimal` overload, which reads its rate as a decimal first
    MONEY_200_23_RON.convertedTo(CCY_RON, BigDecimal.valueOf(11L, 1)) should
      beFailureWith(FailureReason.INVALID)
    MONEY_200_23_RON.convertedTo(CCY_RON, BigDecimal.valueOf(1L)) should
      haveValue(MONEY_200_23_RON)
  }

  /**
   * Asserts conversion through a provider of rates, whose lookup answers with an outcome and is
   * therefore built through [[FxRateProvider.fromFunction]].
   *
   * A conversion into the currency the value already has returns the value '''unchanged''' - the
   * flat rate is not applied to it, and a provider holding no rate at all still answers it,
   * because it is never consulted for one.
   */
  test("testConvertedToWithRateProvider") {
    val provider: FxRateProvider = FxRateProvider.fromFunction((_, _) => Right(2.5d))
    MONEY_100_12_AUD.convertedTo(CCY_RON, provider) should haveValue(moneyOf(CCY_RON, 250.30d))
    MONEY_200_23_RON.convertedTo(CCY_RON, provider) should haveValue(MONEY_200_23_RON)

    // the same two conversions through the contract, from a reference of the trait's type
    val convertible: FxConvertible[Money] = MONEY_100_12_AUD
    convertible.convertedTo(CCY_RON, provider) should haveValue(moneyOf(CCY_RON, 250.30d))
    convertible.convertedTo(CCY_AUD, provider) should haveValue(MONEY_100_12_AUD)

    // a provider that supplies no rate for any pair: the cross-currency conversion is its failure
    val empty: FxRateProvider = FxRateProvider.noConversion()
    MONEY_100_12_AUD.convertedTo(CCY_RON, empty) should
      beFailureWith(FailureReason.CURRENCY_CONVERSION)
    MONEY_100_12_AUD.convertedTo(CCY_AUD, empty) should haveValue(MONEY_100_12_AUD)
  }

  /**
   * Asserts the comparison of money values, by currency and then by amount.
   *
   * The currency comparison is the platform's `String.compareTo`, whose magnitude is unspecified,
   * so the '''sign''' is what is asserted. Both fields compare as zero only when they are equal, so
   * `compare` returns zero exactly when the two values are equal.
   */
  test("testCompareTo") {
    MONEY_100_12_AUD.compareTo(MONEY_200_23_RON) should be < 0
    MONEY_200_23_RON.compareTo(MONEY_100_12_AUD) should be > 0
    MONEY_200_23_RON.compareTo(MONEY_200_23_RON_ALTERNATIVE) shouldBe 0

    moneyOf(GBP, 1.23d).compareTo(moneyOf(GBP, 2.34d)) should be < 0
    moneyOf(GBP, 2.34d).compareTo(moneyOf(GBP, 1.23d)) should be > 0
    moneyOf(GBP, -1d).compareTo(Money.zero(GBP)) should be < 0

    // the instance agrees with the method, and the ordering agrees with equality
    Order[Money].compare(MONEY_100_12_AUD, MONEY_200_23_RON) should be < 0
    Order[Money].compare(MONEY_200_23_RON, MONEY_200_23_RON_ALTERNATIVE) shouldBe 0
    Order[Money].eqv(MONEY_200_23_RON, MONEY_200_23_RON_ALTERNATIVE) shouldBe true
    Order[Money].eqv(MONEY_100_12_AUD, MONEY_200_23_RON) shouldBe false
  }

  /**
   * Asserts the equality and hashing matrix, through `equals`/`hashCode` and through the `Hash`
   * instance a generic caller - a set, a map key, a law suite, a codec property - consults.
   *
   * The amount is a [[Decimal]], which is normalised and compares as a number, so no special
   * casing is needed for it: two amounts written at different widths - `RON 200.23` and
   * `RON 200.2300` - are one value and not two, which the last rows assert.
   */
  test("testEqualsHashCode") {
    MONEY_200_23_RON.equals(MONEY_200_23_RON) shouldBe true
    MONEY_200_23_RON.equals(MONEY_200_23_RON_ALTERNATIVE) shouldBe true
    MONEY_200_23_RON_ALTERNATIVE.equals(MONEY_200_23_RON) shouldBe true
    MONEY_200_23_RON.hashCode shouldBe MONEY_200_23_RON_ALTERNATIVE.hashCode

    // the currency and the amount are both part of the identity of a value
    MONEY_200_23_RON.equals(MONEY_100_12_AUD) shouldBe false
    MONEY_200_23_RON.equals(MONEY_200_AUD) shouldBe false
    MONEY_200_23_RON.equals(MONEY_100_RON) shouldBe false
    MONEY_200_23_RON.equals(MONEY_100_120_BHD) shouldBe false

    MONEY_200_23_RON.equals(ANOTHER_TYPE) shouldBe false
    ANOTHER_TYPE.equals(MONEY_200_23_RON) shouldBe false

    // the same matrix through the `Hash` instance
    Hash[Money].eqv(MONEY_200_23_RON, MONEY_200_23_RON_ALTERNATIVE) shouldBe true
    Hash[Money].hash(MONEY_200_23_RON) shouldBe Hash[Money].hash(MONEY_200_23_RON_ALTERNATIVE)
    Hash[Money].eqv(MONEY_200_23_RON, MONEY_100_RON) shouldBe false
    Hash[Money].eqv(MONEY_200_23_RON, MONEY_200_AUD) shouldBe false

    // a decimal is normalised, so the width an amount was written at is not part of its identity
    val padded: Money = unwrap(Money.of(CCY_RON, new BigDecimal("200.2300")))
    padded shouldBe MONEY_200_23_RON
    Hash[Money].hash(padded) shouldBe Hash[Money].hash(MONEY_200_23_RON)
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the text form, which is the currency code, a space and the amount padded to at least
   * the currency's minor unit width - so `AUD 200.00` carries two digits it was not given, and
   * `BHD 100.125` carries the dinar's three. The text is read back by [[Money.parse]].
   */
  test("testToString") {
    MONEY_200_AUD.toString shouldBe "AUD 200.00"
    MONEY_200_23_RON.toString shouldBe "RON 200.23"
    MONEY_100_125_BHD.toString shouldBe "BHD 100.125"

    Show[Money].show(MONEY_200_AUD) shouldBe "AUD 200.00"
    Show[Money].show(MONEY_100_125_BHD) shouldBe MONEY_100_125_BHD.toString
    Money.parse(MONEY_100_125_BHD.toString) should haveValue(MONEY_100_125_BHD)
  }

  /**
   * Asserts the route from text, including the rounding it applies: `RON 200.2345` names the same
   * value as `RON 200.23`, because the leu quotes two digits and the text route rounds like every
   * other route.
   */
  test("testParse") {
    Money.parse("RON 200.23") should haveValue(MONEY_200_23_RON)
    Money.parse("RON 200.2345") should haveValue(MONEY_200_23_RON)

    // the case of the code is tolerated, and a currency of another width rounds to its own
    Money.parse("ron 200.2345") should haveValue(MONEY_200_23_RON)
    Money.parse("BHD 100.1249") should haveValue(MONEY_100_125_BHD)
  }

  /**
   * Asserts text of the right shape that names no value: it falls into the two parts the form has
   * and is read, and the first names no currency, so the failure carries the message pinned here.
   */
  test("testParseWrongFormat") {
    val outcome: FailureOr[Money] = Money.parse("200.23 RON")
    outcome should beFailureWith(FailureReason.PARSING)
    outcome should haveFailureMessageMatching(Regex.quote("Unable to parse amount: 200.23 RON"))
  }

  /**
   * Asserts text whose shape does not admit a money value at all: it holds no space, so it never
   * falls into the two parts the form has. The expectation is matched as a regular expression, so
   * the dollar sign is written as the character class `[$]`.
   */
  test("testParseWrongElementsNumber") {
    val outcome: FailureOr[Money] = Money.parse("$100")
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
    Money.parse("GBP 12.34").map(money => money.toString) should haveValue("GBP 12.34")

    // a trailing separator is still two parts, the second of them empty, so the text is refused
    // for naming no decimal - the right-shape wording - and not for its shape
    val trailing: FailureOr[Money] = Money.parse("GBP ")
    trailing should beFailureWith(FailureReason.PARSING)
    trailing.left.toOption.map(failure => failure.message) shouldBe Some("Unable to parse amount: GBP ")

    // a leading separator is likewise two parts, the first of them naming no currency
    val leading: FailureOr[Money] = Money.parse(" 1")
    leading.left.toOption.map(failure => failure.message) shouldBe Some("Unable to parse amount:  1")

    // no separator, and every arrangement of two, is the malformed form
    val malformed: List[String] = List("GBP", "GBP 1 2", " GBP 1", "GBP  1")
    malformed.foreach { input =>
      val outcome: FailureOr[Money] = Money.parse(input)
      outcome should beFailureWith(FailureReason.PARSING)
      outcome.left.toOption.map(failure => failure.message) shouldBe
        Some(s"Unable to parse amount, invalid format: $input")
    }

    // and text written to be hostile - two hundred thousand separators, which is two hundred
    // thousand and one parts - reaches that same wording, naming the text in full as every
    // rejection of this port does; bounding what a reader sees belongs to the rendering of the
    // failure, which the neighbouring suites pin.
    val hostile: String = " " * 200000
    val refused: FailureOr[Money] = Money.parse(hostile)
    refused should beFailureWith(FailureReason.PARSING)
    refused.left.toOption.map(failure => failure.message) shouldBe
      Some(s"Unable to parse amount, invalid format: $hostile")
  }

  //-------------------------------------------------------------------------
  /**
   * Reads the value out of an outcome expected to have produced one, reporting a fixture that
   * fails to build as a test failure naming the reason.
   */
  private def unwrap[A](outcome: FailureOr[A]): A =
    outcome.fold(
      failure => fail(s"Expected a value but the factory failed with: ${failure.message}"),
      value => value)

  /**
   * Reads the value out of an accumulating outcome expected to have produced one.
   *
   * [[FixedScaleDecimal.of]] accumulates its checks, so its outcome holds a chain of failures.
   * Every scale this suite pairs a decimal with is a currency's minor unit width, well within the
   * range that factory admits, so a failure here is a mistake in this spec.
   */
  private def unwrapNec[A](outcome: ResultNec[A]): A =
    outcome.fold(
      failures =>
        fail(
          "Expected a value but the factory failed with: " +
            failures.toChain.toList.map(_.message).mkString("; ")),
      value => value)

  /** Builds a money value, failing the test if the currency and amount describe none. */
  private def moneyOf(currency: Currency, amount: Double): Money =
    unwrap(Money.of(currency, amount))

  /**
   * Builds the decimal a `Double` names, which is the one its shortest text spells - so a whole
   * value such as `200d` gives the same decimal as the whole number `200` would.
   */
  private def decimal(value: Double): Decimal = unwrap(Decimal.of(value))

  /** Pairs a decimal with the scale [[Money.getValue]] presents it at. */
  private def fixedScale(amount: Decimal, scale: Int): FixedScaleDecimal =
    unwrapNec(FixedScaleDecimal.of(amount, scale))
}
