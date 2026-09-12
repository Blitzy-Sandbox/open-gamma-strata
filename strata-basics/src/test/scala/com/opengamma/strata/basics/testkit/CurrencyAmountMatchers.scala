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
 * Matchers for [[CurrencyAmount]], for use in the specs of this module and of every module
 * built on it.
 *
 * An amount pairs a number with the currency it is denominated in, and a spec holding one
 * almost always has something to say about both halves - with the number compared against a
 * tolerance, because it came out of floating point arithmetic. This is the vocabulary for
 * saying it, ported from the fluent assertion helper the original test tree used:
 *
 *   - `haveCurrency(expected)` - the amount is denominated in that currency.
 *   - `haveAmount(expected)` - the amount is exactly that number.
 *   - `haveAmount(expected, tolerance)` - the number is within that tolerance of `expected`.
 *   - `haveAmount(expected +- tolerance)` - the same check, written with the tolerance
 *     operator of the test framework.
 *   - `beCloseTo(expected, tolerance)` - the currency equals that of another amount and the
 *     number is within tolerance of its number.
 *
 * ===Using them===
 *
 * Bring them into scope either by importing the members of the companion, which is what a
 * spec of this port conventionally does, or by mixing in this trait:
 *
 * {{{
 * import com.opengamma.strata.basics.testkit.CurrencyAmountMatchers._
 *
 * class SomethingSpec extends AnyFunSuite with Matchers {
 *   test("conversion") {
 *     val result: CurrencyAmount = someMethodCall()
 *     result should haveCurrency(Currency.USD)
 *     result should (haveCurrency(Currency.USD) and haveAmount(123.45, 1e-6))
 *     result should haveAmount(123.45 +- 1e-6)
 *     result should beCloseTo(expected, 1e-6)
 *   }
 * }
 * }}}
 *
 * The second line is how the chained form of the helper being ported reads here. That helper
 * returned itself from every method so that the two halves of an amount could be asserted in
 * one expression; a matcher composes the same way through `and`, and two `should` statements
 * one after the other say the same thing with a separate diagnostic for each half. Either
 * reading is available, and `should not`, `shouldNot` and `or` work on these matchers as they
 * do on any other.
 *
 * The last line is the whole-value form: `beCloseTo` takes the other amount rather than its
 * two parts, which is what a spec wants when it already holds the expected amount. It is
 * deliberately not spelled as equality, because equality of amounts is exact - the framework
 * already provides `equal` and `===` for that - while this form is the tolerant comparison
 * the ported helper offered under the name of equality.
 *
 * ===Amounts are written as decimals===
 *
 * Every amount here is a `Double`, and this file is compiled in a build that treats the
 * widening of a whole number to a decimal as an error rather than performing it silently. A
 * call site therefore writes `haveAmount(2560.0)` and `haveAmount(0.0)` where the assertions
 * being ported wrote `hasAmount(2560)` and `hasAmount(0)`. There is deliberately no whole
 * number overload to paper over that: it would double every member of this vocabulary, make
 * `haveAmount(1, 2)` ambiguous with the tolerant form for readers, and hide from a spec the
 * one thing an amount always is.
 *
 * ===Composing with the other matchers of this port===
 *
 * There is no object here gathering these matchers together with those of
 * [[com.opengamma.strata.collect.testkit.ResultMatchers]], and none is needed. The helper
 * being ported had one - a class whose only purpose was to re-expose the amount assertion
 * beside the assertions of the other modules - because that assertion style was entered
 * through a single overloaded method name, so a spec could import one definition of it and no
 * more. A matcher carries its own name, so a spec that needs both vocabularies imports both
 * and sees every name in each:
 *
 * {{{
 * import com.opengamma.strata.collect.testkit.ResultMatchers._
 * import com.opengamma.strata.basics.testkit.CurrencyAmountMatchers._
 * }}}
 *
 * That pairing is also the answer for an operation that can fail. An operation of this
 * library reports a failure as a value rather than by abandoning the call stack, so a spec
 * asserting on one asserts the outcome with the matchers above and applies the matchers here
 * to the value projected out of it - there is deliberately no matcher in this file that
 * reaches inside an outcome:
 *
 * {{{
 * val converted: FailureOr[CurrencyAmount] = someConversion()
 * converted should beSuccess
 * converted.value should haveAmount(123.45 +- 1e-6)  // `value` from ScalaTest EitherValues
 * }}}
 *
 * ===A frozen contract===
 *
 * The specs of the sibling `currency` package import these names, so every member is public
 * and no name may drift. This trait, its companion and its members are the whole of that
 * contract; the helpers below are private because they are the shared reading of a comparison
 * and not part of it.
 *
 * @see [[com.opengamma.strata.collect.testkit.ResultMatchers]] for the matchers over the
 *   outcome of an operation that can fail, which compose with these at the call site
 */
trait CurrencyAmountMatchers {

  /**
   * Matches an amount denominated in the specified currency.
   *
   * A currency is a value of a closed family whose members are the single instance of each
   * currency in the reference data, so this is an ordinary comparison of two values and no
   * tolerance applies. On failure the expected and the actual currency are reported in the
   * wording of the assertion being ported, so a failure of a ported spec reads as it did.
   *
   * {{{
   * amount should haveCurrency(Currency.USD)
   * amount should not (haveCurrency(Currency.GBP))
   * }}}
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
   * Matches an amount whose number is exactly the specified value.
   *
   * Exactly means bit for bit, which is the comparison [[CurrencyAmount]] itself performs and
   * therefore the one under which this matcher and the equality of the type always agree. Two
   * consequences follow from that and from neither being the comparison the `==` operator
   * performs on a decimal: a value that is not a number matches itself, and a negative zero
   * does not match a positive zero. The second is unreachable through an amount, whose
   * construction normalises a negative zero away, and the first is unreachable as an actual
   * amount, which may not hold a value that is not a number; both are stated because they are
   * what makes the comparison total.
   *
   * Use this form only for a value that arithmetic cannot have perturbed - an amount carried
   * through unchanged, a whole number, a zero. For anything a calculation produced, prefer
   * [[haveAmount(expected:Double,tolerance:Double)* haveAmount(expected, tolerance)]].
   *
   * {{{
   * amount should haveAmount(100.0)
   * }}}
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
   * Matches an amount whose number is within the specified tolerance of the specified value.
   *
   * The bound is inclusive: a difference of exactly the tolerance matches. The comparison is
   * the one the assertion being ported performed, in its order, so a spec that passed there
   * passes here for the same reason:
   *
   *   1. the two numbers being exactly equal matches, whatever they are, which is what lets
   *      an infinite amount be compared against the same infinity;
   *   2. otherwise either of them being infinite or not a number does not match, since no
   *      finite tolerance spans a difference that is not finite;
   *   3. otherwise the absolute difference is compared against the tolerance.
   *
   * A zero tolerance therefore reduces exactly to
   * [[haveAmount(expected:Double)* haveAmount(expected)]], and a negative tolerance is
   * rejected when the matcher is built - not when it is applied - so the failure is reported
   * against the line of the spec that wrote it, as the tolerance value of the ported
   * assertion also was.
   *
   * The diagnostic of a failure carries the tolerance and the observed difference beside the
   * expected and actual numbers, which is what makes a failure at the scale this port is
   * required to hold to readable at all.
   *
   * {{{
   * amount should haveAmount(123.45, 1e-6)
   * }}}
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
   * operator of the test framework.
   *
   * This is [[haveAmount(expected:Double,tolerance:Double)* haveAmount(expected, tolerance)]]
   * reached through the notation a spec of this framework reads most naturally, and it is that
   * matcher: the spread is taken apart into its centre and its tolerance and handed to it, so
   * the comparison, the inclusive bound, the rejection of a negative tolerance and the
   * diagnostic are the same ones. In particular the comparison is the one of the assertion
   * being ported and not the one the spread itself performs, which reaches the same verdict
   * for every finite pair but differs where a number is infinite.
   *
   * {{{
   * amount should haveAmount(2560.0 +- 1e-6)
   * }}}
   *
   * @param spread  the expected number with the tolerance around it, as `expected +- tolerance`
   * @return the matcher for an amount within that spread
   * @throws java.lang.IllegalArgumentException if the tolerance of the spread is negative
   */
  def haveAmount(spread: Spread[Double]): Matcher[CurrencyAmount] =
    haveAmount(spread.pivot, spread.tolerance)

  /**
   * Matches an amount denominated in the same currency as the specified amount and within the
   * specified tolerance of its number.
   *
   * The two halves are checked in the order the assertion being ported checked them - the
   * currency first, then the number - so an amount that differs in both is reported as a
   * currency mismatch, which is the more informative of the two answers and the one a ported
   * spec expects to read. Each half is checked by the matcher that publishes it, so the
   * comparison of the currency and the inclusive tolerant comparison of the number are
   * exactly those documented above, and a negative tolerance is rejected here too, when the
   * matcher is built.
   *
   * This is the tolerant comparison of two whole amounts, deliberately distinct from their
   * equality: `amount should equal (expected)` compares both halves exactly, which is right
   * for an amount that arithmetic cannot have perturbed and wrong for one it has.
   *
   * {{{
   * amount should beCloseTo(expected, 1e-6)
   * }}}
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
   * Compares two numbers exactly, by bit pattern.
   *
   * This is the comparison [[CurrencyAmount]] uses for its own equality, reached through the
   * comparison of the boxed type rather than through the `==` operator, so that a value that
   * is not a number equals itself and a negative zero differs from a positive zero. It is
   * also the first question the tolerant comparison asks, which is what lets that comparison
   * accept an infinity compared against itself.
   *
   * @param actual  the number the amount holds
   * @param expected  the number it is compared against
   * @return true if the two have the same bit pattern
   */
  private def isExactly(actual: Double, expected: Double): Boolean =
    java.lang.Double.compare(actual, expected) == 0

  /**
   * Compares two numbers within a tolerance, reproducing the comparison of the assertion this
   * file is ported from.
   *
   * The three cases are, in order: two numbers that are already exactly equal match; a number
   * that is infinite or not a number does not match anything it is not exactly equal to,
   * since no finite tolerance spans a difference that is not finite; any other pair matches
   * when the absolute difference is no greater than the tolerance, the bound being inclusive.
   *
   * Written this way the comparison is total - every pair of numbers reaches a verdict, with
   * no arithmetic on a value outside the real numbers deciding it - and a zero tolerance
   * reduces it to [[isExactly]].
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
   * Rejects a negative tolerance.
   *
   * A tolerance below zero cannot be satisfied by any pair of numbers except through the
   * exactness case, so a spec that writes one has made a mistake in the spec rather than
   * found one in the code. It is rejected as a broken precondition of the call, at the point
   * the matcher is built, which is where the tolerance of the ported assertion was rejected
   * too. Nothing about the amount is involved, so this is not reported as a failure value:
   * the data has no say in it.
   *
   * @param tolerance  the tolerance to check
   * @throws java.lang.IllegalArgumentException if the tolerance is negative
   */
  private def requireTolerance(tolerance: Double): Unit =
    require(tolerance >= 0d, s"Tolerance must be zero or greater but was: $tolerance")

  /**
   * Renders the difference between two numbers for a diagnostic.
   *
   * The difference of a pair the tolerant comparison rejected for not being finite is itself
   * not finite, and is rendered as it comes out rather than suppressed: reading `Infinity` or
   * `NaN` as the difference is how the reader of a failure learns which case was hit.
   *
   * @param actual  the number the amount holds
   * @param expected  the number it was compared against
   * @return the absolute difference between them
   */
  private def difference(actual: Double, expected: Double): Double =
    math.abs(actual - expected)
}

/**
 * The matchers of [[CurrencyAmountMatchers]], ready to be imported.
 *
 * This object is what makes the single wildcard import
 *
 * {{{
 * import com.opengamma.strata.basics.testkit.CurrencyAmountMatchers._
 * }}}
 *
 * supply the whole vocabulary. It supplies these matchers and nothing else: the matchers of
 * the test framework itself are brought in by the spec, by extending the framework's own
 * matcher trait, and the matchers over the outcome of an operation that can fail are imported
 * from the module below this one, so that a spec sees exactly one definition of each name.
 */
object CurrencyAmountMatchers extends CurrencyAmountMatchers
