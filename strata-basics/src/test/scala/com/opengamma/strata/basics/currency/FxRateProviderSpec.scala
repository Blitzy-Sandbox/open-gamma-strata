/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.util.concurrent.atomic.AtomicReference

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[FxRateProvider]].
 *
 * [[FxRateProvider.noConversion]] and [[FxRateProvider.minimal]] agree about two different
 * currencies - neither converts - and differ about a currency against itself, which
 * `noConversion` refuses as well and `minimal` answers with a rate of one. A calculation that
 * must perform no FX conversion takes the first, so that an unintended conversion becomes a
 * visible failure rather than passing as an identity one; a caller that only ever converts an
 * amount into the currency it already holds takes the second and needs no rates at all. Each
 * failure is pinned to the wording its own factory produces, so a caller receiving one can tell
 * which of the two providers produced it.
 */
final class FxRateProviderSpec extends AnyFunSuite with Matchers {

  test("noConversion") {
    val test: FxRateProvider = FxRateProvider.noConversion()
    val identical: FailureOr[Double] = test.fxRate(Currency.GBP, Currency.GBP)
    identical should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    identical should haveFailureMessageMatching(
      "FX rate conversion is not supported, requested for GBP/GBP")
    val different: FailureOr[Double] = test.fxRate(Currency.GBP, Currency.USD)
    different should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    different should haveFailureMessageMatching(
      "FX rate conversion is not supported, requested for GBP/USD")
  }

  test("minimal") {
    val test: FxRateProvider = FxRateProvider.minimal()
    test.fxRate(Currency.GBP, Currency.GBP) should haveValue(1d)
    val different: FailureOr[Double] = test.fxRate(Currency.GBP, Currency.USD)
    different should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    different should haveFailureMessageMatching("FX rate conversion is not supported for GBP/USD")
  }

  /**
   * The ordered-pair lookup is the two-currency lookup applied to the pair, and the recording
   * function is the instrument that shows it: it holds `GBP/USD` at one rate and the reverse
   * direction at another and refuses every other pair, so the recorded sequence and the rate
   * together state that the two-currency lookup was reached exactly once, as `(GBP, USD)`
   * rather than reversed, and for no other pair.
   */
  test("emptyMatrixCanHandleTrivialRate") {
    // The ordered pairs the function has been asked about, newest last. An AtomicReference over
    // an immutable List keeps this suite free of mutable fields and of any mutable collection.
    val asked = new AtomicReference[List[(Currency, Currency)]](List.empty)
    val test: FxRateProvider = FxRateProvider.fromFunction { (baseCurrency, counterCurrency) =>
      val askedAbout: (Currency, Currency) = (baseCurrency, counterCurrency)
      // Bound to a wildcard because the new sequence is of no interest here; it is read below.
      val _ = asked.updateAndGet(recorded => recorded :+ askedAbout)
      askedAbout match {
        case (Currency.GBP, Currency.USD) => Right(2.5d)
        case (Currency.USD, Currency.GBP) => Right(0.8d)
        case (base, counter) =>
          Left(Failure.CurrencyConversion(s"No rate held for $base/$counter"))
      }
    }
    val rate: FailureOr[Double] = test.fxRate(CurrencyPair.of(Currency.GBP, Currency.USD))
    asked.get() shouldBe List((Currency.GBP, Currency.USD))
    rate should haveValue(2.5d)
  }
}
