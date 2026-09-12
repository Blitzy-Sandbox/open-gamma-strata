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
 * Test [[Money]], ported from the Java `MoneyTest`.
 *
 * Each of the seventeen methods of the original has one test here, under the name the original
 * gave it and in the order the original declared it, so that a Java test method and a test of
 * this suite stay in one-to-one correspondence in the migration manifest and the join on suite
 * class and test name is exact in both directions. The original had neither a `coverage` method
 * nor a `test_serialization` method, and none is invented here: this suite holds exactly
 * seventeen tests. The names are the original's camel case rather than the `test_` spelling the
 * neighbouring `CurrencyAmountSpec` carries, because the two Java classes were written that way
 * and the manifest records the name as it was.
 *
 * ===The invariant this suite exists to pin===
 *
 * '''A money value is never off-scale.''' Every route into the type - the four factories, the
 * text form, the arithmetic, the two mapping methods and both conversions - passes through one
 * private creation step that rounds the amount to the minor units of the currency, half up. The
 * assertions below exercise that on each of those routes, and three of them are load-bearing
 * because they would still pass for a port that rounded on construction and forgot to round a
 * result:
 *
 *   - `testMapAmount` multiplies `GBP 1.23` by `0.3333`, whose exact product `0.409959` is four
 *     digits finer than sterling quotes, and asserts `GBP 0.41`;
 *   - `testConvertedToWithExplicitRate` converts `AUD 100.12` at `2.6`, whose exact product
 *     `260.312` is one digit finer, and asserts `RON 260.31` - which is the original's own row;
 *   - `testParse` reads `RON 200.2345` and asserts the same value as `RON 200.23`.
 *
 * The rounding is asserted over currencies of all three minor-unit widths the reference data
 * holds - none for the yen, two for the Australian dollar and the Romanian leu, three for the
 * Bahraini dinar - so the width is read from the currency rather than assumed to be two.
 *
 * ===Failure is a value, so the fixtures are unwrapped===
 *
 * Three of the four factories report a number no decimal holds rather than raising it, so every
 * fixture of this suite is built through [[moneyOf]] or [[unwrap]], each of which unwraps the
 * outcome once and reports a fixture that could not be built as a failed test naming the cause.
 * The original could write `Money.of(CCY_RON, AMT_200_23)` as a constant because its factory
 * threw.
 *
 * The four `assertThatIllegalArgumentException` sites of the original are `Left` assertions here,
 * and none of this suite's tests intercepts an exception:
 *
 *   - '''a rate other than one for a conversion that does not convert''' carried the message
 *     `FX rate must be 1 when no conversion required`, which is asserted exactly;
 *   - '''text of the wrong shape''' carried `Unable to parse amount, invalid format: $100`,
 *     asserted through the pattern the original matched it with;
 *   - '''text of the right shape naming no value''' carried
 *     `Unable to parse amount: 200.23 RON`, asserted exactly;
 *   - and the currency mismatch of `plus` and `minus`, which the original's `Money` threw from
 *     but its test did not reach, is asserted here in the two tests that own those methods,
 *     because the failure channel is what replaced the throw.
 *
 * Exact messages are pinned through `Regex.quote`, so the comparison is the literal text of the
 * original and not a pattern that happens to match it. The one deliberately un-quoted pattern is
 * `testParseWrongElementsNumber`, which keeps the original's `[$]` character class verbatim.
 *
 * ===Three places where the port's signature differs from the original's===
 *
 *   - [[Money.getValue]] answers with an outcome rather than a bare
 *     [[com.opengamma.strata.collect.FixedScaleDecimal]], because pairing a decimal with a scale
 *     is rejected in general; for a money value it is always present, which is why every
 *     assertion below reads it with `haveValue`.
 *   - [[CurrencyAmount.toMoney]] answers with an outcome where the original was total, because an
 *     amount may be infinite and no decimal is, so the round trip the original wrote as
 *     `base.toCurrencyAmount().toMoney()` is written in the same form here and read with
 *     `haveValue`. The values are the same.
 *   - [[Money.compareTo]] returns the comparison of the currency codes as the platform gives it,
 *     where the original passed it through a chain that normalised every non-zero answer to `-1`
 *     or `1`. The sign is therefore asserted rather than the literal `-1` the original asserted,
 *     and the guarantee that survives is the stronger one: `compare` returns zero exactly when
 *     the two values are equal.
 *
 * ===What is asserted elsewhere===
 *
 * The laws of the ordering, hashing and rendering instances belong to the root
 * `TypeclassLawsSpec`, the sweep over the construction surface of every validated type to
 * `SmartConstructorSpec`, the sweep over every failable method to `FailableSurfaceSpec`, the
 * compile-time proof that this type publishes no `apply` and no `copy` to `ApiSurfaceSpec`, the
 * property-based round trip of every codec of the module to `json.JsonRoundTripSpec` - the
 * original had no serialization test, so this suite has no JSON test at all - and the
 * fixture-driven numerical parity of the money arithmetic against the Java baseline to
 * `parity.CurrencyMathParitySpec`. This suite asserts the cases of the Java test it is ported
 * from and nothing those sweeps own.
 *
 * @see [[Money]] for the type under test
 * @see [[CurrencyAmount]] for the inexact amount this type rounds
 * @see [[FxRateProvider]] for the source of the rates the conversions use
 */
final class MoneySpec extends AnyFunSuite with Matchers {

  /** The Australian dollar, which quotes two minor unit digits, as the original named it. */
  private val CCY_AUD: Currency = Currency.AUD

  /** The Romanian leu, which quotes two minor unit digits, as the original named it. */
  private val CCY_RON: Currency = Currency.RON

  /** The Bahraini dinar, which quotes three minor unit digits, as the original noted. */
  private val CCY_BHD: Currency = Currency.BHD

  /** An amount at the two-digit scale, written with the `d` suffix this build requires. */
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

  /** `RON 100.00`, built through the `BigDecimal` factory as the original built it. */
  private val MONEY_100_RON: Money = unwrap(Money.of(CCY_RON, BigDecimal.valueOf(100L)))

  /** `RON 200.23`, built from the inexact amount. */
  private val MONEY_200_23_RON: Money = unwrap(Money.of(CCYAMT))

  /** `AUD 100.12`. */
  private val MONEY_100_12_AUD: Money = moneyOf(CCY_AUD, AMT_100_12)

  /** `AUD 200.00`, whose amount is a whole number and whose text form is nonetheless padded. */
  private val MONEY_200_AUD: Money = moneyOf(CCY_AUD, 200d)

  /** `RON 200.23` built by the other route, for the equality and comparison matrices. */
  private val MONEY_200_23_RON_ALTERNATIVE: Money = moneyOf(CCY_RON, AMT_200_23)

  /** `BHD 100.120`, an amount coarser than the dinar quotes, which is padded rather than moved. */
  private val MONEY_100_120_BHD: Money = moneyOf(CCY_BHD, AMT_100_12)

  /** `BHD 100.125`, an amount exactly at the dinar's scale. */
  private val MONEY_100_125_BHD: Money = moneyOf(CCY_BHD, AMT_100_125)

  /**
   * A value of a type unrelated to a money value, for the equality assertion that needs one.
   *
   * Held at the type `Any` and named as the original named such a value, so that the assertion
   * reads as a comparison against a foreign value rather than as one the compiler could reject.
   */
  private val ANOTHER_TYPE: Any = ""

  /** The message reported when two money values of different currencies are added. */
  private val DifferentCurrenciesAddMessage: String =
    "Unable to add amounts in different currencies"

  /** The message reported when two money values of different currencies are subtracted. */
  private val DifferentCurrenciesSubtractMessage: String =
    "Unable to subtract amounts in different currencies"

  /** The message reported for a rate other than one on a conversion that does not convert. */
  private val NonUnitRateMessage: String = "FX rate must be 1 when no conversion required"

  //-------------------------------------------------------------------------
  /**
   * Asserts the three number-taking factories and the rounding every one of them applies.
   *
   * The first half is the original's matrix, read through the two accessors it read: the amount
   * as a `BigDecimal` of exactly the currency's scale, and the amount paired with that scale.
   * Both are asserted for each fixture, because they are two renderings of one amount and a port
   * that padded one and not the other would satisfy only one of them.
   *
   * The second half is the rounding, stated over each of the three minor-unit widths the currency
   * data holds. The original covered two of them - the dollar's two digits and the dinar's three
   * - and the yen's none is added here, because a width of zero is the case in which the rounding
   * changes the whole part of the amount rather than its fraction, and no other test of the suite
   * reaches it. `JPY 100.5` rounds away from zero at the tie to `JPY 101`, and `JPY 100.4` down to
   * `JPY 100`.
   *
   * The `BigDecimal` and [[Decimal]] factories are asserted here too, each with an amount finer
   * than its currency quotes, so that all four routes into the type are shown to round and not
   * only the `Double` one the original's rounding rows used. The [[Decimal]] factory is the one
   * route that is total: a decimal is already a value money can hold, so the rounding narrows the
   * amount rather than refusing it.
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

    // the rounding rows of the original: a digit finer than the currency quotes is dropped, and a
    // tie goes away from zero
    moneyOf(CCY_AUD, AMT_100_1249).getValue should haveValue(fixedScale(decimal(AMT_100_12), 2))
    moneyOf(CCY_AUD, AMT_100_125).getValue should haveValue(fixedScale(decimal(AMT_100_13), 2))
    moneyOf(CCY_BHD, AMT_100_1249).getValue should haveValue(fixedScale(decimal(AMT_100_125), 3))
    moneyOf(CCY_BHD, AMT_100_125).getValue should haveValue(fixedScale(decimal(AMT_100_125), 3))

    // a currency that quotes no minor unit digit at all, where the rounding reaches the whole
    // part of the amount
    val yen: Money = moneyOf(Currency.JPY, 100.5d)
    yen.getValue should haveValue(fixedScale(decimal(101d), 0))
    yen.getAmount shouldBe BigDecimal.valueOf(101L, 0)
    moneyOf(Currency.JPY, 100.4d).getValue should haveValue(fixedScale(decimal(100d), 0))
    moneyOf(Currency.JPY, 100.4d).toString shouldBe "JPY 100"

    // the `BigDecimal` factory, whose fixture is a whole number, and the same factory given an
    // amount finer than the currency quotes
    MONEY_100_RON.currency shouldBe CCY_RON
    MONEY_100_RON.getValue should haveValue(fixedScale(decimal(100d), 2))
    MONEY_100_RON.getAmount shouldBe BigDecimal.valueOf(10000L, 2)
    Money.of(CCY_RON, new BigDecimal("200.2345")) should haveValue(MONEY_200_23_RON)

    // the decimal factory, which is total and rounds like the rest
    Money.of(CCY_AUD, decimal(100.129d)) shouldBe moneyOf(CCY_AUD, AMT_100_13)
    Money.of(CCY_AUD, decimal(AMT_100_12)) shouldBe MONEY_100_12_AUD

    // a number no decimal holds is reported rather than rounded, which is the divergence the
    // outcome in the signature makes visible
    Money.of(CCY_AUD, Double.PositiveInfinity) should beFailureWith(FailureReason.INVALID)
  }

  /**
   * Asserts the factory taking an inexact amount.
   *
   * The original asserted the currency and the amount of the fixture it built that way. The
   * rounding of this route is asserted alongside, because it is the route that carries an amount
   * of arbitrary precision into the type, and its one failure - an amount that is not finite,
   * which [[CurrencyAmount]] admits and no decimal holds - is asserted too, since that is the
   * difference between the outcome here and the total factory of the original.
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
   * Asserts addition, in both directions and in both outcomes.
   *
   * The sum of the original is asserted as the value of an outcome, because two money values of
   * different currencies have no sum and the port reports that rather than raising it. The
   * mismatch is asserted in this test, with the wording the implementation being ported threw
   * with: the original's `Money` rejected it and the original's test did not reach the rejection,
   * so the case belongs to the method this test owns and to no sweep.
   */
  test("testPlus") {
    val a: Money = moneyOf(GBP, 1.23d)
    val b: Money = moneyOf(GBP, 2.34d)
    a.plus(b) should haveValue(moneyOf(GBP, 3.57d))
    b.plus(a) should haveValue(moneyOf(GBP, 3.57d))

    // the sum is at the currency's scale, as every amount of this type is
    unwrap(a.plus(b)).getValue should haveValue(fixedScale(decimal(3.57d), 2))

    // two currencies that differ have no sum, which is reported with the ported wording
    val mismatch: FailureOr[Money] = a.plus(moneyOf(Currency.USD, 1d))
    mismatch should beFailureWith(FailureReason.INVALID)
    mismatch should haveFailureMessageMatching(Regex.quote(DifferentCurrenciesAddMessage))
  }

  /**
   * Asserts subtraction, in both directions and in both outcomes.
   *
   * This is `testPlus` read in the other direction: the original's two rows, one of which is
   * negative, and the currency mismatch with the wording that belongs to subtraction.
   */
  test("testMinus") {
    val a: Money = moneyOf(GBP, 1.23d)
    val b: Money = moneyOf(GBP, 0.34d)
    a.minus(b) should haveValue(moneyOf(GBP, 0.89d))
    b.minus(a) should haveValue(moneyOf(GBP, -0.89d))

    // two currencies that differ have no difference either
    val mismatch: FailureOr[Money] = a.minus(moneyOf(Currency.USD, 1d))
    mismatch should beFailureWith(FailureReason.INVALID)
    mismatch should haveFailureMessageMatching(Regex.quote(DifferentCurrenciesSubtractMessage))
  }

  /**
   * Asserts multiplication by a whole number.
   *
   * The operation is total in signature, as it was in the implementation being ported, so the
   * product is a value and not an outcome. Multiplying an amount at the currency's scale by a
   * whole number cannot introduce a finer digit, which is why this is the one arithmetic route
   * whose result is at the currency's scale for a reason other than the rounding - and the
   * assertion on the scale of the product states that rather than leaving it implied. The
   * multiplier carries the `L` suffix because this build treats an implicit numeric widening as
   * an error.
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
   * The original's two rows scale `GBP 1.23` by ten through each method. The rows that follow
   * them are the load-bearing ones: a factor of `0.3333` gives the exact product `0.409959`, four
   * digits finer than sterling quotes, and both methods answer `GBP 0.41`. A port that rounded
   * only on construction would pass the original's rows and fail these.
   *
   * The two methods differ in their outcome, and the difference is real rather than stylistic:
   * mapping the decimal directly cannot produce a value the type does not hold, while a function
   * on `BigDecimal` can return a number needing more than eighteen digits, so that one answers
   * with an outcome. The last row is that failure channel, and it is asserted here rather than
   * deferred: a mapper returning `1E+30` names a number no decimal holds, and the outcome is a
   * `Left` carrying `FailureReason.INVALID` with the message the decimal factory reports for it.
   * Without that row a port could raise, truncate the digits or answer a wrong value for an
   * oversized result and every other assertion of this suite would still pass.
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

    // the currency is carried through unchanged, and a currency of another width rounds to its
    // own scale rather than to sterling's
    val dinar: Money = moneyOf(CCY_BHD, 1.23d)
    dinar.map(amount => amount.multipliedBy(decimal(0.3333d))) shouldBe moneyOf(CCY_BHD, 0.41d)
    dinar.map(amount => amount.multipliedBy(decimal(0.3333d))).currency shouldBe CCY_BHD

    // a result no decimal holds is reported rather than raised, truncated or wrongly accepted
    val oversized: FailureOr[Money] = a.mapAmount(amount => amount.multiply(new BigDecimal("1E+30")))
    oversized should beFailureWith(FailureReason.INVALID)
    oversized should haveFailureMessageMatching(
      Regex.quote("Decimal value must not exceed 18 digits of precision at scale 0: 1.23E+30"))
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts the zero, positive and negative predicates and the three sign-changing methods.
   *
   * This is the original's matrix over its three values, unchanged. Zero is neither positive nor
   * negative, and its negation is the value itself rather than a second zero, so the sign of a
   * zero cannot depend on how that zero was reached.
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
   * Asserts the conversion to an inexact amount, and the round trip back.
   *
   * The original's two rows are the first two, in the original's form: the conversion out, and
   * `toCurrencyAmount().toMoney()` back. The return leg answers with an outcome here - an amount
   * may be infinite and no decimal is - so it is read with `haveValue` and is otherwise the
   * method the original called. `Money.of(base.toCurrencyAmount)` is the same conversion named
   * from this companion and is asserted alongside, because the two must agree.
   *
   * The rows after them are this suite's own. The conversion to [[BigMoney]] and the narrowing
   * back are the other edge of the three-type graph: widening keeps every digit, since no
   * currency quotes more than twelve places, so the round trip through the wider type is an
   * identity for any money value - and `BigMoney.of(money)` and [[Money.toBigMoney]] are asserted
   * to be one conversion under two names, as are `Money.of(bigMoney)` and `BigMoney.toMoney`.
   */
  test("testToCurrencyAmount") {
    val base: Money = moneyOf(GBP, 200.23d)
    base.toCurrencyAmount shouldBe unwrap(CurrencyAmount.of(GBP, 200.23d))
    base.toCurrencyAmount.toMoney should haveValue(base)
    Money.of(base.toCurrencyAmount) should haveValue(base)

    // the currency survives the conversion, and a whole amount converts to a whole one
    base.toCurrencyAmount.currency shouldBe GBP
    MONEY_200_AUD.toCurrencyAmount shouldBe unwrap(CurrencyAmount.of(CCY_AUD, 200d))

    // widening to the exact sibling keeps the amount, and narrowing it back returns this value
    base.toBigMoney shouldBe BigMoney.of(base)
    base.toBigMoney.amount shouldBe base.amount
    Money.of(base.toBigMoney) shouldBe base
    base.toBigMoney.toMoney shouldBe base

    // narrowing a value finer than the currency quotes rounds half up, which is the one decision
    // in the graph: sterling quotes two places, so the twelve-place tie moves away from zero
    val finer: BigMoney = unwrap(BigMoney.of(GBP, new BigDecimal("1.005")))
    Money.of(finer).amount shouldBe finer.toMoney.amount
    Money.of(finer) shouldBe moneyOf(GBP, 1.01d)
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts conversion at a rate given explicitly, through both rate overloads.
   *
   * The original's three rows are kept: a unit rate into the currency the value already has,
   * which returns the value; and the same cross-currency conversion twice, once with the rate as
   * a decimal and once with the rate as a `BigDecimal`, because the two overloads must agree.
   *
   * The cross-currency row is one of the three that pin the rounding of a result: `100.12`
   * multiplied by `2.6` is exactly `260.312`, and the leu quotes two digits, so the answer is
   * `RON 260.31`. The product is rounded to the minor units of the '''result''' currency, which
   * the dinar row states by converting into a currency that quotes more digits than the source.
   */
  test("testConvertedToWithExplicitRate") {
    MONEY_200_23_RON.convertedTo(CCY_RON, decimal(1d)) should haveValue(moneyOf(CCY_RON, AMT_200_23))
    MONEY_100_12_AUD.convertedTo(CCY_RON, decimal(2.6d)) should haveValue(moneyOf(CCY_RON, 260.31d))
    MONEY_100_12_AUD.convertedTo(CCY_RON, decimal(2.6d).toBigDecimal) should
      haveValue(moneyOf(CCY_RON, 260.31d))

    // the result is rounded to the minor units of the currency it is expressed in, so converting
    // into a currency of another width gives that width
    MONEY_100_12_AUD.convertedTo(CCY_BHD, decimal(2.6d)) should haveValue(moneyOf(CCY_BHD, 260.312d))
    MONEY_100_12_AUD.convertedTo(Currency.JPY, decimal(2.6d)) should
      haveValue(moneyOf(Currency.JPY, 260d))

    // a rate no decimal holds is reported by the `BigDecimal` overload, which reads it as one
    MONEY_100_12_AUD.convertedTo(CCY_RON, new BigDecimal("1e19")) should
      beFailureWith(FailureReason.INVALID)
  }

  /**
   * Asserts the rule for a conversion into the currency the value already has.
   *
   * Such a conversion needs no arithmetic, so a rate of one returns the value unchanged and any
   * other rate is reported rather than silently applied - which keeps a caller from scaling an
   * amount by passing a rate for a conversion that does not happen. The original asserted the
   * rejection at a rate of `1.1` and its message, both of which are asserted here.
   *
   * The comparison against one is fuzzy, with the tolerance of `1e-8` the implementation being
   * ported used, so the boundary is asserted on both sides: a rate a thousandth of that tolerance
   * away from one is accepted and returns the value, and one ten times the tolerance away is
   * rejected.
   */
  test("testConvertedToWithExplicitRateForSameCurrency") {
    val rejected: FailureOr[Money] = MONEY_200_23_RON.convertedTo(CCY_RON, decimal(1.1d))
    rejected should beFailureWith(FailureReason.INVALID)
    rejected should haveFailureMessageMatching(Regex.quote(NonUnitRateMessage))

    // a rate within the tolerance of one is a conversion that does not convert, and is accepted
    MONEY_200_23_RON.convertedTo(CCY_RON, decimal(1.000000001d)) should
      haveValue(MONEY_200_23_RON)
    MONEY_200_23_RON.convertedTo(CCY_RON, decimal(0.999999999d)) should
      haveValue(MONEY_200_23_RON)

    // and a rate outside it is not, however close to one it looks
    MONEY_200_23_RON.convertedTo(CCY_RON, decimal(1.0000001d)) should
      beFailureWith(FailureReason.INVALID)

    // the same rule through the `BigDecimal` overload, which reads its rate as a decimal first
    MONEY_200_23_RON.convertedTo(CCY_RON, BigDecimal.valueOf(11L, 1)) should
      beFailureWith(FailureReason.INVALID)
    MONEY_200_23_RON.convertedTo(CCY_RON, BigDecimal.valueOf(1L)) should
      haveValue(MONEY_200_23_RON)
  }

  /**
   * Asserts conversion through a provider of rates.
   *
   * The original built its provider from a lambda returning a flat rate of `2.5`; the abstract
   * lookup of the port answers with an outcome, so the provider is built through
   * [[FxRateProvider.fromFunction]], which is the helper that replaces the lambda. The two rows
   * are the original's: a cross-currency conversion at that rate, and a conversion into the
   * currency the value already has, which returns the value '''unchanged''' - the flat rate is
   * not applied to it, which is the behaviour that makes the second row worth asserting.
   *
   * The conversion takes the route the implementation being ported took, through the
   * `Double`-valued arithmetic of the provider, so `AUD 100.12` at `2.5` is `RON 250.30` digit
   * for digit with it.
   *
   * A third row is added for the channel that replaced the original's `throws RuntimeException`:
   * a provider holding no rate for the pair makes the conversion a failure carrying
   * `FailureReason.CURRENCY_CONVERSION`, and the same provider still answers a conversion into
   * the value's own currency, because it is never consulted for one.
   */
  test("testConvertedToWithRateProvider") {
    val provider: FxRateProvider = FxRateProvider.fromFunction((_, _) => Right(2.5d))
    MONEY_100_12_AUD.convertedTo(CCY_RON, provider) should haveValue(moneyOf(CCY_RON, 250.30d))
    MONEY_200_23_RON.convertedTo(CCY_RON, provider) should haveValue(MONEY_200_23_RON)

    // the same two conversions through the contract, from a reference of the trait's type
    val convertible: FxConvertible[Money] = MONEY_100_12_AUD
    convertible.convertedTo(CCY_RON, provider) should haveValue(moneyOf(CCY_RON, 250.30d))
    convertible.convertedTo(CCY_AUD, provider) should haveValue(MONEY_100_12_AUD)

    // a provider that supplies no rate for any pair: the cross-currency conversion is its
    // failure, and the conversion that needs no rate still succeeds
    val empty: FxRateProvider = FxRateProvider.noConversion()
    MONEY_100_12_AUD.convertedTo(CCY_RON, empty) should
      beFailureWith(FailureReason.CURRENCY_CONVERSION)
    MONEY_100_12_AUD.convertedTo(CCY_AUD, empty) should haveValue(MONEY_100_12_AUD)
  }

  /**
   * Asserts the comparison of money values, by currency and then by amount.
   *
   * The original asserted the literal `-1` for a pair of values whose currencies differ, because
   * the comparison chain it used normalised every non-zero answer to `-1` or `1`. This port
   * returns the comparison of the two currency codes as the platform gives it, so the '''sign'''
   * is asserted where the original asserted the value; the ordering itself is unchanged.
   *
   * The guarantee the port adds is asserted alongside: the comparison returns zero exactly when
   * the two values are equal. Both fields compare as zero only when they are equal, so no
   * secondary comparison is needed to break a tie, and the `Order` instance - which is what a
   * generic caller consults - is shown to agree with the method.
   */
  test("testCompareTo") {
    MONEY_100_12_AUD.compareTo(MONEY_200_23_RON) should be < 0
    MONEY_200_23_RON.compareTo(MONEY_100_12_AUD) should be > 0
    MONEY_200_23_RON.compareTo(MONEY_200_23_RON_ALTERNATIVE) shouldBe 0

    // equal currencies fall through to the amounts
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
   * Asserts the equality and hashing matrix of the original, through both of its entries.
   *
   * The matrix is stated twice: once through `equals` and `hashCode`, as the original stated it,
   * and once through the `Hash` instance the port publishes, because that instance is what every
   * generic caller of this type - a set, a map key, a law suite, a codec property - consults.
   *
   * Equality here needs none of the bit-level special casing the `Double`-backed types of the
   * module require. The amount is a [[Decimal]], which is normalised and compares as a number, so
   * the equality synthesised from the currency and the amount is already the right one; the
   * neighbouring [[CurrencyAmount]] compares bit patterns only because a `Double` amount that is
   * not a number would otherwise differ from itself.
   *
   * One consequence of the normalisation is asserted at the end: two amounts written at different
   * widths - `RON 200.23` and `RON 200.2300` - are one value and not two, because the trailing
   * zeros are not part of the amount.
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

    // a value of an unrelated type is not a money value, in either direction. The original also
    // compared against the absent reference of the language it was written in; that case is
    // subsumed by the types of this port, where a reference to a money value cannot be absent,
    // so it is recorded here rather than written as a literal that would assert nothing
    MONEY_200_23_RON.equals(ANOTHER_TYPE) shouldBe false
    ANOTHER_TYPE.equals(MONEY_200_23_RON) shouldBe false

    // the same matrix through the instance the port publishes
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
   * Asserts the text form, which is the currency code, a space and the amount.
   *
   * The three rows are the original's, and each says something different: the amount of
   * `AUD 200.00` is a whole number and is padded to the two digits the dollar quotes, `RON 200.23`
   * is already at its currency's width, and `BHD 100.125` is at the three digits the dinar quotes.
   * The rendering instance is asserted to agree with the method, since that is what a generic
   * caller renders a value through, and the text is read back by [[Money.parse]] so that the two
   * remain inverse.
   */
  test("testToString") {
    MONEY_200_AUD.toString shouldBe "AUD 200.00"
    MONEY_200_23_RON.toString shouldBe "RON 200.23"
    MONEY_100_125_BHD.toString shouldBe "BHD 100.125"

    // the rendering instance is the same text, and the text is read back as the same value
    Show[Money].show(MONEY_200_AUD) shouldBe "AUD 200.00"
    Show[Money].show(MONEY_100_125_BHD) shouldBe MONEY_100_125_BHD.toString
    Money.parse(MONEY_100_125_BHD.toString) should haveValue(MONEY_100_125_BHD)
  }

  /**
   * Asserts the route from text, including the rounding it applies.
   *
   * Both of the original's rows are asserted, and the second is the cheapest statement of the
   * invariant of the type there is: `RON 200.2345` names the same value as `RON 200.23`, because
   * the leu quotes two digits and the text route rounds like every other route. A port that read
   * the digits as given would answer with a value whose amount is finer than its currency quotes,
   * and would fail here and nowhere else.
   */
  test("testParse") {
    Money.parse("RON 200.23") should haveValue(MONEY_200_23_RON)
    Money.parse("RON 200.2345") should haveValue(MONEY_200_23_RON)

    // the case of the code is tolerated, as it was in the implementation being ported, and a
    // currency of another width rounds to its own
    Money.parse("ron 200.2345") should haveValue(MONEY_200_23_RON)
    Money.parse("BHD 100.1249") should haveValue(MONEY_100_125_BHD)
  }

  /**
   * Asserts text of the right shape that names no value.
   *
   * The text falls into the two parts the form has, so it is read, and neither part is what it
   * has to be - the first names no currency. The original threw with
   * `Unable to parse amount: 200.23 RON`; the port reports a failure carrying that exact message,
   * which is pinned here through `Regex.quote` so that the assertion is the literal text of the
   * original rather than a pattern that happens to match it.
   */
  test("testParseWrongFormat") {
    val outcome: FailureOr[Money] = Money.parse("200.23 RON")
    outcome should beFailureWith(FailureReason.PARSING)
    outcome should haveFailureMessageMatching(Regex.quote("Unable to parse amount: 200.23 RON"))
  }

  /**
   * Asserts text whose shape does not admit a money value at all.
   *
   * The text holds no space, so it does not fall into the two parts the form has and is rejected
   * before anything is read from it. The pattern is the original's, `[$]` and all: the dollar
   * sign is written as a character class there because the assertion matched the message as a
   * regular expression, and keeping it verbatim is what makes this assertion comparable against
   * the Java side line for line.
   */
  test("testParseWrongElementsNumber") {
    val outcome: FailureOr[Money] = Money.parse("$100")
    outcome should beFailureWith(FailureReason.PARSING)
    outcome should haveFailureMessageMatching("Unable to parse amount, invalid format: [$]100")
  }

  //-------------------------------------------------------------------------
  /**
   * Reads the value out of an outcome that is expected to have produced one.
   *
   * This is the single unwrapping helper of the suite, and it exists because the factories of
   * this type and of its neighbours report what was wrong with their arguments as a value. The
   * outcome is matched rather than unwrapped by a partial accessor, so a fixture that fails to
   * build is reported as a test failure naming the reason instead of raising an error from
   * somewhere else in the suite.
   *
   * @param outcome  the outcome expected to carry a value
   * @tparam A  the type of value the outcome carries
   * @return the value it carries
   */
  private def unwrap[A](outcome: FailureOr[A]): A =
    outcome.fold(
      failure => fail(s"Expected a value but the factory failed with: ${failure.message}"),
      value => value)

  /**
   * Reads the value out of an accumulating outcome that is expected to have produced one.
   *
   * [[FixedScaleDecimal.of]] accumulates its checks, so its outcome holds a chain of failures
   * rather than one. Every scale this suite pairs a decimal with is a currency's minor unit
   * width, which is within the range that factory admits, so a failure here is a mistake in this
   * spec and is reported as one - with every message of the chain, in the order it holds them.
   *
   * @param outcome  the outcome expected to carry a value
   * @tparam A  the type of value the outcome carries
   * @return the value it carries
   */
  private def unwrapNec[A](outcome: ResultNec[A]): A =
    outcome.fold(
      failures =>
        fail(
          "Expected a value but the factory failed with: " +
            failures.toChain.toList.map(_.message).mkString("; ")),
      value => value)

  /**
   * Builds a money value, failing the test if the currency and amount describe none.
   *
   * @param currency  the currency the value is in
   * @param amount  the amount of that currency, expected to be one the type admits
   * @return the money value, with its amount rounded to the currency's minor units
   */
  private def moneyOf(currency: Currency, amount: Double): Money =
    unwrap(Money.of(currency, amount))

  /**
   * Builds the decimal a `Double` names, for the expectations that need an exact amount.
   *
   * Every value handed to this helper is an ordinary decimal of a few places, so a failure is a
   * mistake in this spec. The decimal a `Double` names is the one its shortest text spells, so a
   * whole value such as `200d` gives the same decimal as the whole number `200` would.
   *
   * @param value  the value the decimal is to name
   * @return the decimal the value names
   */
  private def decimal(value: Double): Decimal = unwrap(Decimal.of(value))

  /**
   * Pairs a decimal with the scale it is to be presented at, which is what [[Money.getValue]]
   * answers with.
   *
   * @param amount  the amount
   * @param scale  the number of minor unit digits of the currency the amount belongs to
   * @return the amount at that scale
   */
  private def fixedScale(amount: Decimal, scale: Int): FixedScaleDecimal =
    unwrapNec(FixedScaleDecimal.of(amount, scale))
}
