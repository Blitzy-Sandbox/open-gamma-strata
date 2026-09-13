/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.testkit

import org.scalactic.TripleEqualsSupport.Spread

import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.CurrencyAmount

/**
 * Matchers for [[CurrencyAmount]], for use in the specs of this module and of every module built
 * on it. An amount pairs a number with the currency it is denominated in, and a spec holding one
 * usually asserts on both halves, the number against a tolerance because it came out of floating
 * point arithmetic:
 *
 *   - `haveCurrency(expected)` - the amount is denominated in that currency.
 *   - `haveAmount(expected)` - the number is exactly that value.
 *   - `haveAmount(expected, tolerance)` - the number is within that tolerance of `expected`.
 *   - `haveAmount(expected +- tolerance)` - the same check, written with the tolerance
 *     operator of the test framework.
 *   - `beCloseTo(expected, tolerance)` - the currency equals that of another amount and the
 *     number is within tolerance of its number.
 *
 * Import the members of the companion, or mix in this trait; `and`, `or`, `should not` and
 * `shouldNot` compose these matchers as they do any other:
 *
 * {{{
 * import com.opengamma.strata.basics.testkit.CurrencyAmountMatchers._
 *
 * val result: CurrencyAmount = someMethodCall()
 * result should haveCurrency(Currency.USD)
 * result should not (haveCurrency(Currency.GBP))
 * result should (haveCurrency(Currency.USD) and haveAmount(123.45, 1e-6))
 * result should haveAmount(100.0)
 * result should haveAmount(123.45 +- 1e-6)
 * result should beCloseTo(expected, 1e-6)
 * }}}
 *
 * A spec that also asserts on the outcome of an operation that can fail imports
 * `com.opengamma.strata.collect.testkit.ResultMatchers._` beside this import and applies these
 * matchers to the value projected out of the `Either`; no matcher here reaches inside an outcome.
 *
 * Every amount is a `Double`, and this build treats the widening of a whole number to a decimal
 * as an error, so a call site writes `haveAmount(2560.0)`; there is deliberately no whole number
 * overload, which would double this vocabulary and make `haveAmount(1, 2)` ambiguous with the
 * tolerant form. `beCloseTo` is the tolerant comparison of two whole amounts and is deliberately
 * not equality, for which the test framework already provides `equal` and `===`.
 *
 * The published names are a contract: the specs of the sibling `currency` package import them.
 *
 * @see [[com.opengamma.strata.collect.testkit.ResultMatchers]] for the matchers over the
 *   outcome of an operation that can fail, which compose with these at the call site
 */
trait CurrencyAmountMatchers {

  /**
   * Matches an amount denominated in the specified currency. A currency is a value of a closed
   * family, so this is an ordinary comparison of two values and no tolerance applies; a failure
   * reports the expected and the actual currency.
   *
   * @param expected  the currency the amount is expected to be denominated in
   * @return the matcher for an amount in that currency
   */
  def haveCurrency(expected: Currency): Matcher[CurrencyAmount] =
    new Matcher[CurrencyAmount] {
      override def apply(left: CurrencyAmount): MatchResult =
        MatchResult(
          left.currency == expected,
          s"Expected CurrencyAmount with currency: <$expected> but was: <${left.currency}>",
          s"Expected CurrencyAmount with a currency other than <$expected> but was: <${left.currency}>"
        )
    }

  /**
   * Matches an amount whose number is exactly the specified value, where exactly means
   * `java.lang.Double.compare(actual, expected) == 0` - the comparison [[CurrencyAmount]] itself
   * performs for equality, and not the one `==` performs on a decimal. Every `NaN` equals every
   * other `NaN`, because the payload is canonicalised, and `-0.0` does not match `0.0`; it is not
   * raw bit equality, and neither case is reachable through an amount, whose construction rejects
   * a `NaN` and normalises a `-0.0` away.
   *
   * Use this form only for a number arithmetic cannot have perturbed - an amount carried through
   * unchanged, a whole number, a zero. For anything a calculation produced, prefer
   * [[haveAmount(expected:Double,tolerance:Double)* haveAmount(expected, tolerance)]].
   *
   * @param expected  the number the amount is expected to hold
   * @return the matcher for an amount holding exactly that number
   */
  def haveAmount(expected: Double): Matcher[CurrencyAmount] =
    new Matcher[CurrencyAmount] {
      override def apply(left: CurrencyAmount): MatchResult =
        MatchResult(
          isExactly(left.amount, expected),
          s"Expected CurrencyAmount with amount: <$expected> but was: <${left.amount}> in: <$left>",
          s"Expected CurrencyAmount with an amount other than <$expected> but was: <${left.amount}> in: <$left>"
        )
    }

  /**
   * Matches an amount whose number is within the specified tolerance of the specified value. The
   * bound is inclusive - a difference of exactly the tolerance matches - and the comparison takes
   * three cases in this order:
   *
   *   1. the two numbers being exactly equal matches, whatever they are, which is what lets
   *      an infinite amount be compared against the same infinity;
   *   2. otherwise either of them being infinite or not a number does not match, since no
   *      finite tolerance spans a difference that is not finite;
   *   3. otherwise the absolute difference is compared against the tolerance.
   *
   * A zero tolerance therefore reduces exactly to
   * [[haveAmount(expected:Double)* haveAmount(expected)]], and a negative tolerance is a broken
   * precondition of the call rather than a reported failure: it is rejected when the matcher is
   * built, not when it is applied. A failure carries the tolerance and the observed difference
   * beside the expected and actual numbers.
   *
   * @param expected  the number the amount is expected to be close to
   * @param tolerance  the largest difference that still matches, zero or greater
   * @return the matcher for an amount within tolerance of that number
   * @throws java.lang.IllegalArgumentException if the tolerance is negative
   */
  def haveAmount(expected: Double, tolerance: Double): Matcher[CurrencyAmount] = {
    requireTolerance(tolerance)
    new Matcher[CurrencyAmount] {
      override def apply(left: CurrencyAmount): MatchResult =
        MatchResult(
          isCloseTo(left.amount, expected, tolerance),
          s"Expected CurrencyAmount with amount: <$expected> within tolerance: <$tolerance> " +
            s"but was: <${left.amount}> in: <$left>, differing by: <${difference(left.amount, expected)}>",
          s"Expected CurrencyAmount with an amount further than <$tolerance> from <$expected> " +
            s"but was: <${left.amount}> in: <$left>, differing by: <${difference(left.amount, expected)}>"
        )
    }
  }

  /**
   * Matches an amount whose number is within the specified spread, written with the tolerance
   * operator of the test framework. The spread is taken apart into its centre and its tolerance
   * and handed to
   * [[haveAmount(expected:Double,tolerance:Double)* haveAmount(expected, tolerance)]], so the
   * comparison, the inclusive bound, the rejection of a negative tolerance and the diagnostic are
   * that matcher's - in particular the comparison is the one documented there and not the one
   * `Spread` itself performs, which reaches the same verdict for every finite pair but differs
   * where a number is infinite.
   *
   * @param spread  the expected number with the tolerance around it, as `expected +- tolerance`
   * @return the matcher for an amount within that spread
   * @throws java.lang.IllegalArgumentException if the tolerance of the spread is negative
   */
  def haveAmount(spread: Spread[Double]): Matcher[CurrencyAmount] =
    haveAmount(spread.pivot, spread.tolerance)

  /**
   * Matches an amount denominated in the same currency as the specified amount and within the
   * specified tolerance of its number. The currency is checked first and the number second, so an
   * amount that differs in both is reported as a currency mismatch, the more informative of the
   * two answers; each half is checked by the matcher that publishes it, so both comparisons are
   * those documented above and a negative tolerance is rejected here too, when the matcher is
   * built.
   *
   * This is the tolerant comparison of two whole amounts, deliberately distinct from their
   * equality: `amount should equal (expected)` compares both halves exactly, which is right for an
   * amount arithmetic cannot have perturbed and wrong for one it has.
   *
   * @param expected  the amount this one is expected to be close to
   * @param tolerance  the largest difference of the two numbers that still matches, zero or
   *   greater
   * @return the matcher for an amount in the same currency and within tolerance
   * @throws java.lang.IllegalArgumentException if the tolerance is negative
   */
  def beCloseTo(expected: CurrencyAmount, tolerance: Double): Matcher[CurrencyAmount] = {
    val currencyMatcher = haveCurrency(expected.currency)
    val amountMatcher = haveAmount(expected.amount, tolerance)
    new Matcher[CurrencyAmount] {
      override def apply(left: CurrencyAmount): MatchResult = {
        val currencyResult = currencyMatcher(left)
        if (!currencyResult.matches) {
          currencyResult
        } else {
          val amountResult = amountMatcher(left)
          if (!amountResult.matches) {
            amountResult
          } else {
            MatchResult(
              true,
              s"Expected CurrencyAmount within tolerance: <$tolerance> of <$expected> but was: <$left>",
              s"Expected CurrencyAmount further than <$tolerance> from <$expected> but was: <$left>"
            )
          }
        }
      }
    }
  }

  //-------------------------------------------------------------------------
  /**
   * Compares two numbers exactly, as `java.lang.Double.compare(actual, expected) == 0`: the
   * comparison [[CurrencyAmount]] uses for its own equality, and not the one `==` performs on a
   * decimal. Every `NaN` equals every other `NaN`, because the payload is canonicalised, while
   * `-0.0` and `0.0` stay distinct, so this is not raw bit equality, which would tell two `NaN`
   * payloads apart. Neither case arises for an actual amount - the factory rejects a `NaN` and
   * normalises a `-0.0` away - and exactness is the first question the tolerant comparison asks,
   * which is what lets an infinity match itself.
   *
   * @param actual  the number the amount holds
   * @param expected  the number it is compared against
   * @return true when `java.lang.Double.compare` reports the two equal - equal finite numbers, the
   *   same infinity, or two `NaN`s whatever their payloads - and false for `-0.0` against `0.0`
   */
  private def isExactly(actual: Double, expected: Double): Boolean =
    java.lang.Double.compare(actual, expected) == 0

  /**
   * Compares two numbers within a tolerance, in three ordered cases: two numbers that are already
   * exactly equal match; a number that is infinite or not a number does not match anything it is
   * not exactly equal to, since no finite tolerance spans a difference that is not finite; any
   * other pair matches when the absolute difference is no greater than the tolerance, the bound
   * being inclusive. Written this way the comparison is total - every pair of numbers reaches a
   * verdict without arithmetic on a value outside the real numbers deciding it - and a zero
   * tolerance reduces it to [[isExactly]].
   *
   * @param actual  the number the amount holds
   * @param expected  the number it is compared against
   * @param tolerance  the largest difference that still matches, already known to be zero or
   *   greater
   * @return true if the two are equal or differ by no more than the tolerance
   */
  private def isCloseTo(actual: Double, expected: Double, tolerance: Double): Boolean =
    if (isExactly(actual, expected)) {
      true
    } else if (!java.lang.Double.isFinite(actual) || !java.lang.Double.isFinite(expected)) {
      false
    } else {
      math.abs(actual - expected) <= tolerance
    }

  /**
   * Rejects a negative tolerance, which can only be satisfied through the exactness case and is
   * therefore a mistake in the spec rather than a finding in the code. It is a broken precondition
   * of the call, rejected where the matcher is built; nothing about the amount is involved, so it
   * is not reported as a failure value.
   *
   * @param tolerance  the tolerance to check
   * @throws java.lang.IllegalArgumentException if the tolerance is negative
   */
  private def requireTolerance(tolerance: Double): Unit =
    require(tolerance >= 0d, s"Tolerance must be zero or greater but was: $tolerance")

  /**
   * Renders the difference between two numbers for a diagnostic. The difference of a pair the
   * tolerant comparison rejected for not being finite is itself not finite, and is rendered as it
   * comes out: reading `Infinity` or `NaN` as the difference is how the reader of a failure learns
   * which case was hit.
   *
   * @param actual  the number the amount holds
   * @param expected  the number it was compared against
   * @return the absolute difference between them
   */
  private def difference(actual: Double, expected: Double): Double =
    math.abs(actual - expected)
}

/**
 * The matchers of [[CurrencyAmountMatchers]], ready to be imported as
 * `import com.opengamma.strata.basics.testkit.CurrencyAmountMatchers._`.
 */
object CurrencyAmountMatchers extends CurrencyAmountMatchers
