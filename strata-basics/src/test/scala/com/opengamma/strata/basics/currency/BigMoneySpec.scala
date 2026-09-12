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
 * Test [[BigMoney]], ported from the Java `BigMoneyTest`.
 *
 * The original held twenty-two test methods and no parameterized test, and this suite holds
 * exactly twenty-two tests under the same names, in the same order, so that a reader comparing
 * the two files finds every method and no method that was never there and the migration
 * manifest's join on suite class and test name is exact in both directions. The names are the
 * original's camel case rather than the `test_` spelling the rest of this package uses, because
 * the original spelled them that way and the manifest joins on the name as written.
 *
 * ===Why this suite is not `MoneySpec` with a wider scale===
 *
 * [[BigMoney]] and [[Money]] hold the same two fields and differ in five places: the amount is
 * kept to twelve decimal places rather than to the minor units of the currency,
 * [[BigMoney.getValue]] answers the amount on its own, the four comparison predicates exist only
 * here, and so do [[BigMoney.roundToScale]] and [[BigMoney.toMoney]]. Those five are where a port
 * that had copied the narrower type would still pass a careless test, so they are the ones this
 * suite presses hardest:
 *
 *   - `testOfCurrencyAndAmount` pins the twelve places by building from an amount that names
 *     more, in each of the three factories, and states the contrast with the sibling type in
 *     one line;
 *   - `testRoundingWithNegativeScale` rounds the '''whole''' part, which a port that clamped a
 *     scale at zero would silently pass under the positive-scale rows alone;
 *   - `testTo` narrows an amount that is a tie at the minor unit, which a port that ignored the
 *     currency's scale would pass on any amount that did not actually need rounding.
 *
 * ===Failure is a value, so the fixtures are unwrapped===
 *
 * Three of the five factories of the type report a number no decimal holds rather than raising
 * it, so every fixture here is built through [[bigMoneyOf]] or [[decimalOf]], which unwrap the
 * outcome once through [[unwrap]] and report a fixture that could not be built as a failed test
 * naming the cause. The original could write `BigMoney.of(CCY_RON, AMT_200_2345)` as a constant
 * because its factory threw.
 *
 * The eight rejections the original asserted as `IllegalArgumentException` are `Left` values
 * here, and they are asserted in the two ways the AAP distinguishes: the reason is compared as a
 * value of the closed family of reasons, and the message is pinned - through `Regex.quote`, so
 * that the comparison is the literal text and not a pattern that happens to match it - wherever
 * the wording is what a log line or a user-facing message carries. The four currency-mismatch
 * comparisons, the non-unit rate and the two parse rejections all carry their original wording;
 * the second parse test keeps the original's own regular expression verbatim, escaped as the
 * original escaped it.
 *
 * ===What this suite does not assert===
 *
 * The property-based round trip of the codec belongs to `json.JsonRoundTripSpec`, the laws of
 * the ordering, hashing and rendering instances to `TypeclassLawsSpec`, the sweep over the
 * construction surface of every validated type - including the documented arithmetic overflow of
 * [[Decimal]], which is the one edge of this type that still raises - to `SmartConstructorSpec`,
 * the compile-time proof that the type publishes no `apply` and no `copy` to `ApiSurfaceSpec`,
 * and the fixture-driven numerical parity of the twelve-digit rounding against the Java baseline
 * to `parity.CurrencyMathParitySpec`. The original had neither a `coverage` method nor a
 * `test_serialization` method, so this suite has no test of either, which is what keeps it at
 * twenty-two.
 *
 * @see [[BigMoney]] for the type under test
 * @see [[Money]] for the sibling rounded to the currency's minor units
 * @see [[FxRateProvider]] for the source of the rates the conversions use
 */
final class BigMoneySpec extends AnyFunSuite with Matchers {

  /** The first currency of the original, which quotes two minor unit digits. */
  private val CCY_AUD: Currency = AUD

  /** The second currency of the original, which also quotes two minor unit digits. */
  private val CCY_RON: Currency = RON

  /** The third currency of the original, kept for its three minor unit digits. */
  private val CCY_BHD: Currency = BHD

  /** A whole amount, written with the `d` suffix this build requires of a `Double`. */
  private val AMT_100: Double = 100d

  /** An amount within the two digits the Australian dollar and the leu quote. */
  private val AMT_100_12: Double = 100.12d

  /** An amount finer than the minor units of any currency of the reference data. */
  private val AMT_100_1249: Double = 100.1249d

  /** The amount of the fixture built from a [[CurrencyAmount]]. */
  private val AMT_200_2345: Double = 200.2345d

  /** The currency amount the `of(CurrencyAmount)` fixture is built from. */
  private val CCYAMT: CurrencyAmount = unwrap(CurrencyAmount.of(CCY_RON, AMT_200_2345))

  /** `RON 100`, built through the `BigDecimal` factory as the original built it. */
  private val MONEY_100_RON: BigMoney = unwrap(BigMoney.of(CCY_RON, BigDecimal.valueOf(100L)))

  /** `RON 200.2345`, built from a [[CurrencyAmount]]. */
  private val MONEY_200_2345_RON: BigMoney = unwrap(BigMoney.of(CCYAMT))

  /** `AUD 100`, built through the `Double` factory. */
  private val MONEY_100_AUD: BigMoney = bigMoneyOf(CCY_AUD, AMT_100)

  /** `AUD 100.1249`, whose amount is finer than the two digits the currency quotes. */
  private val MONEY_100_1249_AUD: BigMoney = bigMoneyOf(CCY_AUD, AMT_100_1249)

  /** `AUD 200`, the second amount of the equality and ordering matrices. */
  private val MONEY_200_AUD: BigMoney = bigMoneyOf(CCY_AUD, 200d)

  /** A second `RON 200.2345`, built afresh, for the equality and ordering matrices. */
  private val MONEY_200_2345_RON_ALTERNATIVE: BigMoney = bigMoneyOf(CCY_RON, AMT_200_2345)

  /** `BHD 100.12`, whose amount is inside the three digits the dinar quotes. */
  private val MONEY_100_12_BHD: BigMoney = bigMoneyOf(CCY_BHD, AMT_100_12)

  /** `BHD 100.1249`, whose amount is finer than the three digits the dinar quotes. */
  private val MONEY_100_1249_BHD: BigMoney = bigMoneyOf(CCY_BHD, AMT_100_1249)

  /**
   * A value of a type unrelated to a monetary value, for the equality assertion that needs one.
   *
   * Held at the type `Any` and valued as the original valued it, so that the assertion reads as
   * a comparison against a foreign value rather than as one the compiler could reject.
   */
  private val ANOTHER_TYPE: Any = ""

  //-------------------------------------------------------------------------
  /** The wording reported when two values of different currencies are added. */
  private val AddMismatchMessage: String = "Unable to add amounts in different currencies"

  /** The wording reported when two values of different currencies are subtracted. */
  private val SubtractMismatchMessage: String =
    "Unable to subtract amounts in different currencies"

  /**
   * The wording reported when two values of different currencies are compared.
   *
   * One wording covers all four predicates, as it did in the original, which is why the original
   * declared it once as a local and asserted it four times.
   */
  private val CompareMismatchMessage: String =
    "Unable to compare amounts in different currencies"

  /**
   * The wording reported when a rate other than one is given for a conversion that converts
   * nothing.
   */
  private val NonUnitRateMessage: String = "FX rate must be 1 when no conversion required"

  //-------------------------------------------------------------------------
  /**
   * Asserts the three factories over a currency and an amount, and the twelve-place invariant.
   *
   * The original's assertions are the first block: the currency, the amount as a [[Decimal]] and
   * the amount as a `BigDecimal` padded to the minor units of the currency, over four fixtures
   * that between them cover a whole amount, an amount finer than the currency's minor units, and
   * a currency quoting three digits rather than two.
   *
   * The second block is what the original could not state, because its constant amounts were all
   * within twelve places: '''every''' route into this type rounds the amount to twelve decimal
   * places, half up. It is stated for each of the three factories - the `Double`, the
   * `BigDecimal` and the [[Decimal]] one - and once against [[Money]], whose factory rounds the
   * same amount to the two digits sterling quotes instead. That one line is the whole difference
   * between the two types, and having it here saves the next reader a lookup.
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

    // the three factories reach one value from the three spellings of one amount, which is what
    // makes the `BigDecimal`-built fixture of the original comparable with the rest
    MONEY_100_RON.currency shouldBe CCY_RON
    MONEY_100_RON.getValue shouldBe decimalOf(AMT_100)
    unwrap(BigMoney.of(CCY_RON, AMT_100)) shouldBe MONEY_100_RON
    BigMoney.of(CCY_RON, decimalOf("100")) shouldBe MONEY_100_RON
    BigMoney.of(CCY_RON, Decimal.ZERO) shouldBe BigMoney.zero(CCY_RON)

    // twelve decimal places is the invariant of the type, applied by every route into it: an
    // amount naming more places is rounded half up to twelve rather than rejected
    BigMoney.of(GBP, decimalOf("1.123456789012345")).getValue shouldBe decimalOf("1.123456789012")
    BigMoney.of(GBP, decimalOf("1.0000000000005")).getValue shouldBe decimalOf("1.000000000001")
    unwrap(BigMoney.of(GBP, 1.123456789012345d)).getValue shouldBe decimalOf("1.123456789012")
    unwrap(BigMoney.of(GBP, new BigDecimal("2.0000000000005"))).getValue shouldBe
      decimalOf("2.000000000001")

    // the one line that separates this type from its sibling: the same amount rounded to the two
    // digits sterling quotes rather than to twelve places
    Money.of(GBP, decimalOf("1.123456789012345")).amount shouldBe decimalOf("1.12")

    // a number no decimal holds is reported rather than raised, which is the divergence of the
    // two failable factories from the total ones the original published
    BigMoney.of(GBP, Double.NaN) should beFailureWith(FailureReason.INVALID)
  }

  /**
   * Asserts the factories that widen a neighbouring type.
   *
   * The original asserted `of(CurrencyAmount)` through its fixture. The `of(Money)` route is
   * asserted alongside it here, because it is the other widening factory and it is total: the
   * amount of a money value is already at the minor units of its currency, and no currency of
   * the reference data quotes more than three digits, so widening always succeeds and loses
   * nothing.
   */
  test("testOfCurrencyAmount") {
    MONEY_200_2345_RON.currency shouldBe CCY_RON
    MONEY_200_2345_RON.getValue shouldBe decimalOf(AMT_200_2345)

    // widening a money value keeps the amount it holds, which is already rounded to the minor
    // units of its currency, so the widened value is the one the narrower amount names
    val money: Money = Money.of(CCY_RON, decimalOf(AMT_200_2345))
    BigMoney.of(money).getValue shouldBe decimalOf("200.23")
    BigMoney.of(money) shouldBe bigMoneyOf(CCY_RON, 200.23d)

    // an amount outside the real numbers is admitted by `CurrencyAmount` and is not a value of
    // this type, so this factory reports where the original's was total
    CurrencyAmount
      .of(CCY_RON, Double.PositiveInfinity)
      .flatMap(amount => BigMoney.of(amount)) should beFailureWith(FailureReason.INVALID)
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts addition, in both directions, and the currency mismatch it reports.
   *
   * The two happy-path rows are the original's. The mismatch is not: the original had no such
   * row in this method, because its `plus` threw and the throw was covered elsewhere. Whether
   * two amounts share a currency is a property of the values a caller holds rather than of the
   * calling code, so it is a reported failure here and belongs with the operation that reports
   * it - asserted by reason and by the wording a log line carries.
   *
   * One thing the brief for this file expected cannot be written, and the reason is worth
   * recording: a sum cannot need re-rounding. Both operands are values of this type, so each
   * already has at most twelve decimal places, and the exact sum of two such numbers has at
   * most twelve as well. The rounding every route performs is therefore a no-op for `plus` and
   * `minus`, and the route that does reach a finer amount is [[BigMoney.map]], where
   * `testMapAmount` asserts it.
   */
  test("testPlus") {
    val a: BigMoney = bigMoneyOf(GBP, 1.23d)
    val b: BigMoney = bigMoneyOf(GBP, 2.34d)
    a.plus(b) should haveValue(bigMoneyOf(GBP, 3.57d))
    b.plus(a) should haveValue(bigMoneyOf(GBP, 3.57d))

    // the sum of two amounts at twelve places is itself at twelve places, so the invariant holds
    // without the rounding having anything to do
    BigMoney
      .of(GBP, decimalOf("0.000000000001"))
      .plus(BigMoney.of(GBP, decimalOf("0.000000000002"))) should
      haveValue(BigMoney.of(GBP, decimalOf("0.000000000003")))

    val mismatch: FailureOr[BigMoney] = a.plus(bigMoneyOf(USD, 1d))
    mismatch should beFailureWith(FailureReason.INVALID)
    mismatch should haveFailureMessageMatching(Regex.quote(AddMismatchMessage))
  }

  /**
   * Asserts subtraction, in both directions, and the currency mismatch it reports.
   *
   * The two happy-path rows are the original's, the second of them producing a negative amount.
   * The mismatch row is added for the reason `testPlus` gives, with the wording this operation
   * reports, which differs from the wording of addition by one word.
   */
  test("testMinus") {
    val a: BigMoney = bigMoneyOf(GBP, 1.23d)
    val b: BigMoney = bigMoneyOf(GBP, 0.34d)
    a.minus(b) should haveValue(bigMoneyOf(GBP, 0.89d))
    b.minus(a) should haveValue(bigMoneyOf(GBP, -0.89d))

    val mismatch: FailureOr[BigMoney] = a.minus(bigMoneyOf(USD, 1d))
    mismatch should beFailureWith(FailureReason.INVALID)
    mismatch should haveFailureMessageMatching(Regex.quote(SubtractMismatchMessage))
  }

  /**
   * Asserts multiplication by a whole number.
   *
   * The original's row is the first. The multiplier is a `Long` here, written with the suffix
   * this build requires, because an implicit numeric widening is an error in it; the value the
   * original passed as an `int` literal is the same value.
   *
   * The second row multiplies an amount that is already at the twelfth decimal place, which is
   * the widest operand the type admits, and shows that the product keeps those places rather
   * than losing them - a whole multiplier cannot move an amount past the scale it came with.
   */
  test("testMultipliedBy") {
    val a: BigMoney = bigMoneyOf(GBP, 1.23d)
    a.multipliedBy(2L) shouldBe bigMoneyOf(GBP, 2.46d)

    BigMoney.of(GBP, decimalOf("1.000000000001")).multipliedBy(3L).getValue shouldBe
      decimalOf("3.000000000003")
    a.multipliedBy(0L) shouldBe BigMoney.zero(GBP)
  }

  /**
   * Asserts the two mapping methods over the amount, and the re-rounding of their results.
   *
   * The original's two rows are the first two: the [[Decimal]]-valued [[BigMoney.map]], which is
   * total, and the `BigDecimal`-valued [[BigMoney.mapAmount]], which reports a result no decimal
   * holds and therefore answers with an outcome rather than a value.
   *
   * The third row is where the re-rounding of a result is actually observable, which no
   * operation asserted above can show: a third of `GBP 10` has an unbounded fraction, and the
   * value produced is that fraction rounded to twelve places. The fourth states the failure
   * channel of `mapAmount` with a function whose result is too large for any decimal, which is
   * the one way that method can fail.
   */
  test("testMapAmount") {
    val a: BigMoney = bigMoneyOf(GBP, 1.23d)
    a.map(amount => amount.multipliedBy(decimalOf("10"))) shouldBe
      BigMoney.of(GBP, decimalOf("12.30"))
    a.mapAmount(amount => amount.multiply(BigDecimal.TEN)) should
      haveValue(BigMoney.of(GBP, decimalOf("12.30")))

    // a function of arbitrary precision cannot break the invariant of the type
    bigMoneyOf(GBP, 10d).map(amount => amount.dividedBy(decimalOf("3"))).getValue shouldBe
      decimalOf("3.333333333333")

    // and a result no decimal holds is reported rather than raised
    a.mapAmount(amount => amount.multiply(new BigDecimal("1E+30"))) should
      beFailureWith(FailureReason.INVALID)
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the four comparison predicates over two values of one currency.
   *
   * These four are published by this type and not by [[Money]], so there is no sibling test to
   * copy them from. Each returns an outcome rather than a `Boolean`, because it refuses a pair in
   * two different currencies, so a `true` of the original is `haveValue(true)` here and a `false`
   * is `haveValue(false)` - a distinction that matters, since an outcome carrying a failure is
   * neither.
   *
   * The ten rows of the original are asserted first, in its order, and the two rows it left
   * implicit follow: the strict predicates over two equal amounts, which are the cases that
   * separate them from their inclusive counterparts.
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

    // the equal case of the two strict predicates, which the original asserted only by negation
    bigMoneyOf(GBP, 1d).isGreaterThan(bigMoneyOf(GBP, 1d)) should haveValue(false)
    bigMoneyOf(GBP, 1d).isLessThan(bigMoneyOf(GBP, 1d)) should haveValue(false)
  }

  /**
   * Asserts that the four predicates refuse a pair in two different currencies.
   *
   * `GBP 1` and `USD 1` stand in no order without an exchange rate, so the predicates decline
   * rather than compare the bare numbers - which is what the original did, by throwing. All four
   * are asserted here where the original asserted `isLessThanEqualTo` twice and `isLessThan` not
   * at all; the fourth row covers the predicate that duplication left out.
   *
   * Ordering through [[BigMoney.order]] is the other question and has the other answer: it
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
   * Asserts rounding to a positive scale, in the six modes the original covered.
   *
   * The rows are the original's, unchanged, and they are the point of the method: the mode
   * asked for is applied rather than the half up this type applies to its own amounts, so a
   * caller reaches `CEILING`, `FLOOR`, `DOWN` and `HALF_DOWN` through it. Three rows round away
   * from zero and three towards it, over amounts chosen so that each mode's answer differs from
   * at least one other's.
   */
  test("testRoundingWithPositiveScale") {
    // round up
    bigMoneyOf(GBP, 1.441d).roundToScale(2, RoundingMode.CEILING) shouldBe bigMoneyOf(GBP, 1.45d)
    bigMoneyOf(GBP, 1.441d).roundToScale(2, RoundingMode.UP) shouldBe bigMoneyOf(GBP, 1.45d)
    bigMoneyOf(GBP, 1.446d).roundToScale(2, RoundingMode.HALF_UP) shouldBe bigMoneyOf(GBP, 1.45d)

    // round down
    bigMoneyOf(GBP, 1.449d).roundToScale(2, RoundingMode.FLOOR) shouldBe bigMoneyOf(GBP, 1.44d)
    bigMoneyOf(GBP, 1.449d).roundToScale(2, RoundingMode.DOWN) shouldBe bigMoneyOf(GBP, 1.44d)
    bigMoneyOf(GBP, 1.444d).roundToScale(2, RoundingMode.HALF_DOWN) shouldBe
      bigMoneyOf(GBP, 1.44d)

    // a scale of twelve or more leaves the amount alone, which is a consequence of the invariant
    // rather than a case of this method: the amount has no digit beyond the twelfth to lose
    val fine: BigMoney = BigMoney.of(GBP, decimalOf("1.000000000001"))
    fine.roundToScale(12, RoundingMode.UP) shouldBe fine
    fine.roundToScale(18, RoundingMode.UP) shouldBe fine
  }

  /**
   * Asserts rounding to a negative scale, which rounds the whole part.
   *
   * A scale of `-3` rounds to a multiple of a thousand, `-2` to a hundred and `-1` to ten, and
   * the six rows are the original's. This is the test that separates a port which rounds a scale
   * from one which clamps it at zero: every row here would be satisfied by a value returned
   * unchanged if the negative scale were ignored, and none of them is.
   *
   * The result is a value of this type, so it passes through the half-up rounding at twelve
   * places that every route performs - which cannot change a number whose fraction is already
   * empty. That is why the expectations are whole amounts and are written as such.
   */
  test("testRoundingWithNegativeScale") {
    // round up
    bigMoneyOf(GBP, 780001d).roundToScale(-3, RoundingMode.CEILING) shouldBe
      bigMoneyOf(GBP, 781000d)
    bigMoneyOf(GBP, 780001d).roundToScale(-2, RoundingMode.UP) shouldBe bigMoneyOf(GBP, 780100d)
    bigMoneyOf(GBP, 780005d).roundToScale(-1, RoundingMode.HALF_UP) shouldBe
      bigMoneyOf(GBP, 780010d)

    // round down
    bigMoneyOf(GBP, 780999d).roundToScale(-3, RoundingMode.FLOOR) shouldBe
      bigMoneyOf(GBP, 780000d)
    bigMoneyOf(GBP, 780699d).roundToScale(-2, RoundingMode.DOWN) shouldBe
      bigMoneyOf(GBP, 780600d)
    bigMoneyOf(GBP, 780234d).roundToScale(-1, RoundingMode.HALF_DOWN) shouldBe
      bigMoneyOf(GBP, 780230d)
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the three sign predicates and the three sign-changing methods.
   *
   * The original's three blocks are kept as they stand - a zero, a positive amount and its
   * negation - including the row it asserted twice, `zero.negated`, which is asserted once here
   * and complemented by the `zero.negative` row its duplication had displaced. A zero returns
   * itself from all three sign-changing methods, so the sign of a zero cannot depend on how it
   * was written.
   */
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
   * Asserts the conversion to a [[CurrencyAmount]] and the round trip back.
   *
   * The original wrote the return leg as `toCurrencyAmount().toBigMoney()`. That method does not
   * exist on the amount type of this port - widening is the business of the type being widened
   * to, so it is [[BigMoney.of]] - and the identity asserted is the same one: an amount whose
   * value is within twelve places survives the trip through the `Double`-valued type unchanged.
   */
  test("testToCurrencyAmount") {
    val base: BigMoney = bigMoneyOf(GBP, 200.23d)
    base.toCurrencyAmount shouldBe unwrap(CurrencyAmount.of(GBP, 200.23d))
    BigMoney.of(base.toCurrencyAmount) should haveValue(base)
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts conversion at a rate the caller supplies, in both rate forms.
   *
   * The original's three rows are kept: the identity conversion at a rate of one, and the same
   * cross-currency conversion through the [[Decimal]] rate and through the `BigDecimal` one,
   * which is the pair of overloads the type publishes. The amount is multiplied by the rate, in
   * that order, and the product is rounded to twelve places rather than to the minor units of
   * the result currency - `AUD 100` at `2.6031` is `RON 260.31` and keeps every digit an exact
   * product would have.
   */
  test("testConvertedToWithExplicitRate") {
    MONEY_200_2345_RON.convertedTo(CCY_RON, decimalOf("1")) should
      haveValue(bigMoneyOf(CCY_RON, 200.2345d))
    MONEY_100_AUD.convertedTo(CCY_RON, decimalOf("2.6031")) should
      haveValue(bigMoneyOf(CCY_RON, 260.31d))
    MONEY_100_AUD.convertedTo(CCY_RON, new BigDecimal("2.6031")) should
      haveValue(bigMoneyOf(CCY_RON, 260.31d))

    // the product is exact decimal arithmetic, so a rate with more digits than the currency
    // quotes is applied in full and only the twelfth place bounds the result
    MONEY_100_AUD.convertedTo(CCY_RON, decimalOf("2.60315")) should
      haveValue(bigMoneyOf(CCY_RON, 260.315d))

    // a rate no decimal holds is reported rather than raised, which is the one failure the
    // `BigDecimal` overload adds to the one below it
    MONEY_100_AUD.convertedTo(CCY_RON, new BigDecimal("1E+30")) should
      beFailureWith(FailureReason.INVALID)
  }

  /**
   * Asserts the rate a conversion into the value's own currency admits.
   *
   * Such a conversion performs no arithmetic, so a rate of one is accepted and the value is
   * returned as it stands, while any other rate is refused rather than silently applied - which
   * keeps a caller from scaling an amount by passing a rate for a conversion that does not
   * happen. The original asserted the refusal at `1.1` with the wording asserted here.
   *
   * The tolerance is the `1e-8` literal the original compared with, so it is asserted on both
   * sides: a rate a billionth away from one is within it and is accepted, and a rate a
   * ten-millionth away is outside it and is refused. A port that had compared for exact equality
   * would fail the first pair, and one that had widened the tolerance would fail the second.
   */
  test("testConvertedToWithExplicitRateForSameCurrency") {
    val refused: FailureOr[BigMoney] = MONEY_200_2345_RON.convertedTo(CCY_RON, decimalOf("1.1"))
    refused should beFailureWith(FailureReason.INVALID)
    refused should haveFailureMessageMatching(Regex.quote(NonUnitRateMessage))

    // within the tolerance, in both directions: the value is returned unchanged
    MONEY_200_2345_RON.convertedTo(CCY_RON, decimalOf("1.000000001")) should
      haveValue(MONEY_200_2345_RON)
    MONEY_200_2345_RON.convertedTo(CCY_RON, decimalOf("0.999999999")) should
      haveValue(MONEY_200_2345_RON)

    // outside it: refused, with the same wording, through either rate form
    MONEY_200_2345_RON.convertedTo(CCY_RON, decimalOf("1.0000001")) should
      beFailureWith(FailureReason.INVALID)
    MONEY_200_2345_RON.convertedTo(CCY_RON, new BigDecimal("1.1")) should
      haveFailureMessageMatching(Regex.quote(NonUnitRateMessage))
  }

  /**
   * Asserts conversion through a provider of rates.
   *
   * The original passed a lambda where this port takes an [[FxRateProvider]] built by
   * [[FxRateProvider.fromFunction]]: the trait declares more than one method, so it is not a
   * single-abstract-method type and a function is turned into one explicitly. The two rows of
   * the original follow - a cross-currency conversion at a flat rate of `2.5`, and the identity
   * conversion, which returns the value unchanged.
   *
   * Two rows are added, both about a provider that holds no rate at all. The identity
   * conversion succeeds under it, because a value already in the requested currency does not
   * consult the provider - the behaviour the original had and the reason its second row passes
   * whatever the provider does - while the cross-currency conversion reports the provider's own
   * failure, which is where a missing rate surfaces.
   */
  test("testConvertedToWithRateProvider") {
    val provider: FxRateProvider = FxRateProvider.fromFunction((_, _) => Right(2.5d))
    MONEY_100_AUD.convertedTo(CCY_RON, provider) should haveValue(bigMoneyOf(CCY_RON, 250d))
    MONEY_200_2345_RON.convertedTo(CCY_RON, provider) should haveValue(MONEY_200_2345_RON)

    // a value already in the requested currency is returned without the provider being asked
    MONEY_200_2345_RON.convertedTo(CCY_RON, FxRateProvider.noConversion()) should
      haveValue(MONEY_200_2345_RON)

    // and a provider holding no rate for the pair makes its failure the failure of the
    // conversion, rather than the conversion inventing a rate of its own
    MONEY_100_AUD.convertedTo(CCY_RON, FxRateProvider.noConversion()) should
      beFailureWith(FailureReason.CURRENCY_CONVERSION)
  }

  /**
   * Asserts the narrowing to [[Money]].
   *
   * The original's row is the round trip of a whole amount, which narrows and widens without
   * changing. That row alone would be satisfied by a port that ignored the currency's minor
   * units entirely, so the narrowing is asserted where it has a decision to make: `GBP 1.005`
   * is an exact tie at the second decimal place and rounds '''half up''' to `GBP 1.01`. That
   * answer depends on the amount being held exactly: the same rounding performed in binary
   * floating point - scaling by a hundred and rounding the result - answers `1.00` instead,
   * because the nearest `Double` to `1.005` lies below the tie at `1.00499999999999989…`.
   *
   * Two further rows narrow to the widths other currencies quote: three digits for the Bahraini
   * dinar and none for the yen. Those are the rows a port that had carried this type's own
   * twelve places into the narrower type would fail.
   */
  test("testTo") {
    BigMoney.of(MONEY_100_AUD.toMoney) shouldBe MONEY_100_AUD
    BigMoney.of(MONEY_200_AUD.toMoney) shouldBe MONEY_200_AUD

    // a genuine narrowing: the tie at the minor unit rounds away from zero
    val tie: BigMoney = BigMoney.of(GBP, decimalOf("1.005"))
    tie.toMoney.amount shouldBe decimalOf("1.01")
    tie.toMoney.getAmount shouldBe new BigDecimal("1.01")
    tie.toMoney.toString shouldBe "GBP 1.01"

    // the width narrowed to is the currency's own, not a constant of this type
    BigMoney.of(CCY_BHD, decimalOf("1234.567890123457")).toMoney.amount shouldBe
      decimalOf("1234.568")
    BigMoney.of(JPY, decimalOf("1234.567890123457")).toMoney.amount shouldBe decimalOf("1235")

    // narrowing an amount already within the minor units of its currency changes nothing, which
    // is what makes the round trip of the original's row an identity
    BigMoney.of(MONEY_100_12_BHD.toMoney) shouldBe MONEY_100_12_BHD
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the ordering, which is by currency and then by amount.
   *
   * Only the sign of a comparison is part of the contract, and this is the one assertion of the
   * original that had to change shape: it compared against `-1`, which its implementation
   * produced because the comparison chain it used narrowed the magnitude, while this port
   * returns the value the underlying comparison gives - so the sign is asserted and the
   * magnitude is not.
   *
   * The ordering agrees with equality exactly: `compare` returns zero precisely when the two
   * values are equal, because both fields compare as zero only when they are equal and no
   * secondary comparison is needed to break a tie. Unlike the four predicates of
   * `testCompareWithDifferentCurrencies`, it answers for two values in different currencies,
   * because a total order is what sorting a mixed collection needs.
   */
  test("testCompareTo") {
    MONEY_100_AUD.compareTo(MONEY_200_2345_RON) should be < 0
    MONEY_200_2345_RON.compareTo(MONEY_100_AUD) should be > 0
    MONEY_200_2345_RON.compareTo(MONEY_200_2345_RON_ALTERNATIVE) shouldBe 0

    // the currency decides first, and the amount only among values of one currency
    MONEY_100_AUD.compareTo(MONEY_200_AUD) should be < 0
    MONEY_200_AUD.compareTo(MONEY_100_AUD) should be > 0
    MONEY_100_AUD.compareTo(MONEY_100_RON) should be < 0

    // the instance a generic caller consults agrees with the method, and its zero coincides
    // with its equality
    Order[BigMoney].compare(MONEY_100_AUD, MONEY_200_2345_RON) should be < 0
    Order[BigMoney].compare(MONEY_200_2345_RON, MONEY_200_2345_RON_ALTERNATIVE) shouldBe 0
    Order[BigMoney].eqv(MONEY_200_2345_RON, MONEY_200_2345_RON_ALTERNATIVE) shouldBe true
    Order[BigMoney].eqv(MONEY_100_AUD, MONEY_100_RON) shouldBe false
    Order[BigMoney].max(MONEY_100_AUD, MONEY_200_AUD) shouldBe MONEY_200_AUD
  }

  /**
   * Asserts the equality and hashing matrix of the original, and the instance that carries it.
   *
   * Equality is that of the two fields, and no bit-level special case is involved: the amount is
   * a normalised [[Decimal]] that compares as a number, which is why two spellings of one amount
   * are one value here. [[CurrencyAmount]] needs to compare bit patterns because its amount is a
   * `Double`, where a value that is not a number would otherwise differ from itself; this type
   * has nothing to defend against, so the equality synthesised from its fields is already the
   * right one.
   *
   * The original also asserted that a value does not equal the absent reference of the language
   * it was written in. That case is subsumed by the types of this port, where a reference to a
   * monetary value cannot be absent, so it is recorded here rather than written; the
   * foreign-type half of the row remains expressible and is asserted in both directions.
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

    // two spellings of one amount are one value, because the amount is normalised
    BigMoney.of(CCY_AUD, decimalOf("200")) shouldBe MONEY_200_AUD
    BigMoney.of(CCY_AUD, decimalOf("200.00")) shouldBe MONEY_200_AUD
    BigMoney.of(CCY_AUD, decimalOf("200.00")).hashCode shouldBe MONEY_200_AUD.hashCode

    // the same matrix through the instance the port publishes, which a generic caller consults
    // and which must agree with the methods above
    Hash[BigMoney].eqv(MONEY_200_2345_RON, MONEY_200_2345_RON_ALTERNATIVE) shouldBe true
    Hash[BigMoney].eqv(MONEY_200_2345_RON, MONEY_100_RON) shouldBe false
    Hash[BigMoney].hash(MONEY_200_2345_RON) shouldBe
      Hash[BigMoney].hash(MONEY_200_2345_RON_ALTERNATIVE)
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the text form, which is the currency code and the amount padded to the minor units.
   *
   * The original's two rows are the first two, and they are the two halves of the rule: an
   * amount with fewer digits than the currency quotes is padded, and one with more is shown in
   * full rather than truncated. The rows over the Bahraini dinar state the same rule at a width
   * of three digits, which is the only other width the reference data uses above zero.
   *
   * The rendering instance and `parse` are asserted against the same text, because those three
   * identities together are what a document, a log line and a test expectation written before
   * the port depend on.
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
   * Asserts the text that names a value, including text naming more places than are kept.
   *
   * The original's two rows are the first two, the second of them stating that `parse`
   * normalises rather than rejects: text naming fifteen decimal places is read as the amount
   * rounded to twelve, which is the same amount the twelve-place text names. The third row
   * writes that amount out, so a reader does not have to derive it from the equality of two
   * parses, and the fourth states the case tolerance the original had, which comes from the
   * currency code being resolved through [[Currency.parse]].
   */
  test("testParse") {
    BigMoney.parse("RON 200.2345") should haveValue(MONEY_200_2345_RON)
    unwrap(BigMoney.parse("AUD 1.123456789012345")) shouldBe
      unwrap(BigMoney.parse("AUD 1.123456789012"))
    unwrap(BigMoney.parse("AUD 1.123456789012345")).getValue shouldBe decimalOf("1.123456789012")
    BigMoney.parse("ron 200.2345") should haveValue(MONEY_200_2345_RON)
  }

  /**
   * Asserts text of the right shape whose parts name no value.
   *
   * `200.23 RON` falls into exactly two space-separated parts, so it passes the shape check and
   * is rejected for naming no currency in its first part - which is the wording asserted here,
   * exactly as the original asserted it. The pattern is the literal message escaped through
   * `Regex.quote`, because the matcher matches a whole message against a regular expression and
   * an unescaped `.` would match a character the message does not hold.
   */
  test("testParseWrongFormat") {
    val outcome: FailureOr[BigMoney] = BigMoney.parse("200.23 RON")
    outcome should beFailureWith(FailureReason.PARSING)
    outcome should haveFailureMessageMatching(Regex.quote("Unable to parse amount: 200.23 RON"))
  }

  /**
   * Asserts text that does not fall into exactly two space-separated parts.
   *
   * `$100` holds no space at all, so it is rejected on its shape before anything is read from
   * it, with the other of the two wordings. The pattern is the original's own, kept verbatim
   * including the character class it used to escape the currency symbol, since the original
   * asserted this row by pattern rather than by literal text.
   */
  test("testParseWrongElementsNumber") {
    val outcome: FailureOr[BigMoney] = BigMoney.parse("$100")
    outcome should beFailureWith(FailureReason.PARSING)
    outcome should haveFailureMessageMatching("Unable to parse amount, invalid format: [$]100")
  }

  //-------------------------------------------------------------------------
  /**
   * Reads the value out of an outcome that is expected to have produced one.
   *
   * This is the single unwrapping helper of the suite, and it exists because the public routes
   * into [[BigMoney]] and [[Decimal]] report what was wrong with their arguments as a value. The
   * outcome is folded rather than opened by a partial accessor, so a fixture that fails to build
   * is reported as a test failure naming the reason instead of raising an error from somewhere
   * else in the suite.
   *
   * @param outcome  the outcome expected to carry a value
   * @tparam A  the type of the value the outcome carries
   * @return the value it carries
   */
  private def unwrap[A](outcome: FailureOr[A]): A =
    outcome.fold(
      failure => fail(s"Expected a value but the operation failed with: ${failure.message}"),
      value => value)

  /**
   * Builds a monetary value from a currency and a `Double` amount, failing the test if the
   * arguments describe none.
   *
   * @param currency  the currency the value is in
   * @param amount  the amount of that currency, expected to be one the type admits
   * @return the monetary value
   */
  private def bigMoneyOf(currency: Currency, amount: Double): BigMoney =
    unwrap(BigMoney.of(currency, amount))

  /**
   * Builds a decimal from its canonical text, failing the test if the text names none.
   *
   * The text route is used wherever the exact digits matter - the twelve-place invariant, the
   * narrowing rows and the rates - because it names the digits asserted rather than leaving them
   * to be recovered from a binary value. An amount such as `1.0000000000005`, whose rounding at
   * the twelfth place is the whole point of the row it appears in, is stated as the text of the
   * number wanted and not as the nearest `Double` to it.
   *
   * @param text  the decimal as text, expected to name a decimal
   * @return the decimal
   */
  private def decimalOf(text: String): Decimal = unwrap(Decimal.of(text))

  /**
   * Builds a decimal from a `Double`, failing the test if the value names none.
   *
   * This is the route the original's amounts take, since they were `double` constants, and it
   * reads the decimal the shortest text of the value names.
   *
   * @param value  the value, expected to be one a decimal holds
   * @return the decimal
   */
  private def decimalOf(value: Double): Decimal = unwrap(Decimal.of(value))

}
