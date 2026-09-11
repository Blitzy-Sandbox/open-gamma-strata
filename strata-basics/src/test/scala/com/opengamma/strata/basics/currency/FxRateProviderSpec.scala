/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[FxRateProvider]], ported from the Java `FxRateProviderTest`.
 *
 * The original holds three methods and so does this suite, under the names it gave them, so
 * that a Java test method and a test of this suite stay in one-to-one correspondence in the
 * migration manifest. Two of them are about the providers the companion offers and the third
 * is about the trait itself; between them they cover everything the trait does that is not
 * the behaviour of some particular implementation of it.
 *
 * ===What this suite is for===
 *
 * [[FxRateProvider.noConversion]] and [[FxRateProvider.minimal]] agree about every pair of
 * two different currencies - neither will convert - and disagree about a currency and itself,
 * where `noConversion` fails as well and `minimal` answers with a rate of one. That single
 * difference is the entire reason both exist, and it is the thing a port can lose without any
 * careless reading of the original noticing: a `minimal` that failed on the identity case, or
 * a `noConversion` that answered one, would still satisfy an assertion that only looked at
 * two different currencies. So the identity case is asserted for '''both''' providers here,
 * and each failure is pinned to the message wording its own factory produces - the two
 * wordings differ in the original and differ here, which is what lets a failure be traced
 * back to the provider that produced it. Neither test may be weakened to the point where the
 * two providers become interchangeable.
 *
 * ===Failure is a value===
 *
 * The four places the original asserted `assertThatIllegalArgumentException` are four
 * assertions here that the outcome is a `Left`, carrying the reason
 * [[com.opengamma.strata.collect.result.FailureReason.CURRENCY_CONVERSION]] compared as a
 * value of the closed family of reasons rather than as text. The absence of a rate is the
 * ordinary, expected outcome of asking a provider about a pair it does not hold, and the
 * ported trait reports it in the type of the lookup, so there is nothing here for a
 * thrown-exception assertion to catch.
 *
 * ===The function literal===
 *
 * The third test built its provider from a lambda, which the original could do because its
 * trait was a Java functional interface whose single method returned a bare `double`. The
 * ported lookup returns `Either[Failure, Double]`, so the provider is built through
 * [[FxRateProvider.fromFunction]] - the explicit form of the same conversion, which states
 * the error channel at the call site instead of relying on the expected type being inferred
 * there.
 *
 * ===What is asserted elsewhere===
 *
 * `FxRateProvider` is a contract with no data of its own, so it is on the excluded side of
 * this port's codec inventory and has no JSON round-trip here, exactly as the original had no
 * serialization test. The typeclass law suites, the sweep that exercises every failable method
 * of the module with a failing input, and the fixture-driven numerical parity of FX
 * conversion each live in their own spec, as does the deferring provider behind
 * [[FxRateProvider.lazily]], which `LazyFxRateProviderSpec` owns.
 *
 * @see [[FxRateProvider]] for the trait and the providers this suite asserts
 */
final class FxRateProviderSpec extends AnyFunSuite with Matchers {

  /**
   * Asserts that the provider that declines to convert declines every lookup.
   *
   * Both cases the original asserted are kept, and the first of them is the one that matters:
   * a rate for a currency and itself is refused as well. That is deliberate and it is the
   * whole reason this provider exists next to `minimal` - a calculation that must not perform
   * FX conversion at all is given this provider precisely so that an unintended conversion
   * becomes a visible failure rather than passing unnoticed as an identity conversion. It must
   * not be "fixed" into answering one; the provider that answers one for that case is
   * `minimal`, asserted below.
   *
   * The message is pinned as well as the reason, because the reason alone is what `minimal`
   * fails with too. Pinning it here and there is what keeps the two providers distinguishable
   * from the failure a caller receives.
   */
  test("noConversion") {
    val test: FxRateProvider = FxRateProvider.noConversion()
    // a currency and itself is refused, which is what distinguishes this provider from `minimal`
    val identical: FailureOr[Double] = test.fxRate(Currency.GBP, Currency.GBP)
    identical should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    identical should haveFailureMessageMatching(
      "FX rate conversion is not supported, requested for GBP/GBP")
    // two different currencies are refused with the same wording, naming the pair asked about
    val different: FailureOr[Double] = test.fxRate(Currency.GBP, Currency.USD)
    different should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    different should haveFailureMessageMatching(
      "FX rate conversion is not supported, requested for GBP/USD")
  }

  /**
   * Asserts that the minimal provider converts between identical currencies and nothing else.
   *
   * This is the other half of the contract `noConversion` states: the identity case succeeds
   * with a rate of one, so a caller that only ever converts an amount into the currency it is
   * already in needs no rates at all, while any genuine conversion still fails.
   *
   * The wording pinned here is the one this factory produces, which differs from the wording
   * of `noConversion` in the original and differs here. Asserting both is what stops the two
   * providers from becoming interchangeable.
   */
  test("minimal") {
    val test: FxRateProvider = FxRateProvider.minimal()
    // a currency and itself is one, without any rate being held; this is the difference from
    // `noConversion`, which refuses this very lookup
    test.fxRate(Currency.GBP, Currency.GBP) should haveValue(1d)
    // any two different currencies still fail, with this factory's own wording
    val different: FailureOr[Double] = test.fxRate(Currency.GBP, Currency.USD)
    different should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    different should haveFailureMessageMatching("FX rate conversion is not supported for GBP/USD")
  }

  /**
   * Asserts that the ordered-pair lookup is the two-currency lookup applied to the pair.
   *
   * The original wrote a lambda for the provider, relying on its trait being a Java functional
   * interface whose single method returned a bare `double`. The ported lookup returns
   * `Either[Failure, Double]`, so a bare function literal no longer describes it and the
   * provider is built through `FxRateProvider.fromFunction`, which states that error channel
   * at the call site.
   *
   * What is under test is the trait's own definition of the pair form rather than anything the
   * function does: the function is the only thing in the provider that can produce a rate, so
   * a rate coming back from the pair lookup is proof that the default reached it, which is the
   * wiring the original asserted here too. Which currency of the pair reaches which parameter
   * is a question this test cannot answer, since the function ignores both - it is asserted
   * where the two directions of a pair carry different rates, in the specs of the providers
   * that hold rates.
   */
  test("emptyMatrixCanHandleTrivialRate") {
    val test: FxRateProvider = FxRateProvider.fromFunction((_, _) => Right(2.5d))
    val rate: FailureOr[Double] = test.fxRate(CurrencyPair.of(Currency.GBP, Currency.USD))
    rate should haveValue(2.5d)
  }
}
