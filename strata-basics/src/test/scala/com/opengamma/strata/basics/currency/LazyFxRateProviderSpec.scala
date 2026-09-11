/*
 * Copyright (C) 2019 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[LazyFxRateProvider]], reached through [[FxRateProvider.lazily]].
 *
 * This is a one-to-one port of the Java test class: its two test methods are the two tests
 * below, under the same names and in the same order, and each of them asserts the same three
 * lookups the original asserted before checking how often the underlying provider was
 * obtained.
 *
 * ===Two numbers are the whole suite===
 *
 * The provider under test exists to put off obtaining the provider behind it, so the only
 * thing worth measuring about it is how many times it does so:
 *
 *   - `testLaziness` expects '''zero'''. A conversion between a currency and itself, and a
 *     rate for a pair of identical currencies, have the same answer for every provider, so
 *     answering them must not obtain the one behind it.
 *   - `testDelegation` expects '''exactly one'''. Three lookups that do depend on the
 *     underlying provider must collapse to a single evaluation of the function supplying it,
 *     because that function is expected to be the expensive part - loading market data, in the
 *     case this type was written for.
 *
 * Everything else here is scaffolding for those two counts.
 *
 * ===Counting without a mocking framework===
 *
 * The original counted with a mocked supplier: it passed a generated stand-in for the
 * `Supplier` interface as the underlying supplier, stubbed that stand-in to yield a provider,
 * and then asserted the framework's no-interactions and exactly-one-invocation verifications
 * over it. Neither the mocking framework nor the `Supplier` interface it stood in for is part
 * of this port, so the count is kept by [[CountingSupplier]] below - a handful of lines holding
 * an `AtomicInteger` and the provider to hand out. The substitution costs nothing and buys
 * three things: the two unchecked-cast suppression annotations the generated stand-in needed in
 * the original are gone, the counter is thread-safe in the same way the memoisation it observes
 * is, and the stub is a value of the function type the factory actually takes rather than a
 * generated proxy, so the compiler checks it.
 *
 * A counter of this shape is the reason this suite holds no mutable field and no mutable
 * collection: an `AtomicInteger` is neither.
 *
 * ===Why each test asserts three lookups rather than one===
 *
 * The three lookups are not repetition. The two-currency rate lookup of the type under test is
 * deliberately defined as the ordered-pair lookup applied to its two arguments, which is what
 * extends the identity answer - and the memoisation - to both forms. A port that short-circuited
 * or memoised only the two-currency form would still satisfy the first two assertions of each
 * test and fail the third, so the `CurrencyPair` assertion is the one that pins the
 * indirection. The conversion is asserted for the same reason: it is a third route to the same
 * rate, and it has its own short-circuit.
 *
 * Both tests read their outcomes through the shared testkit matchers, because every lookup of
 * this API reports a missing rate as a failure on the left of an `Either` rather than by
 * throwing. `haveValue` therefore asserts two things at once - that the lookup succeeded, and
 * that it produced the expected rate - and names the failure it found if it did not, so a
 * failure cannot pass unnoticed here even though neither test expects one.
 *
 * ===What this suite does not own===
 *
 * The original has no serialization case and no coverage case, and this port adds neither: the
 * type under test holds a deferred computation rather than data and has no JSON form. The other
 * providers of the companion - the two that decline to convert, and the conversion from a
 * function - belong to the sibling suite of the trait, and are used here only to build a target
 * for the stub to hand out.
 */
final class LazyFxRateProviderSpec extends AnyFunSuite with Matchers {

  test("testLaziness") {
    // The target would answer every pair with a rate of seven, which no assertion below
    // expects: an identity lookup that wrongly reached it would be wrong in its value as well
    // as in the call count, so this test fails twice over rather than only on the count.
    val supplier = new CountingSupplier(constantProvider(7d))
    val provider = FxRateProvider.lazily(() => supplier.get())

    provider.convert(1d, Currency.USD, Currency.USD) should haveValue(1d)
    provider.fxRate(Currency.USD, Currency.USD) should haveValue(1d)
    // The pair form is the load-bearing case: the two-currency lookup above is defined as this
    // one, so a short-circuit written only for the two-currency form would pass the two
    // assertions above and fail here.
    provider.fxRate(CurrencyPair.of(Currency.USD, Currency.USD)) should haveValue(1d)

    // The port of the original's no-interactions verification: nothing above depends on the
    // underlying provider, so the function supplying it must not have been invoked at all.
    supplier.callCount shouldBe 0
  }

  test("testDelegation") {
    val expectedRate = 3d
    val supplier = new CountingSupplier(constantProvider(expectedRate))
    val provider = FxRateProvider.lazily(() => supplier.get())

    provider.convert(1d, Currency.USD, Currency.EUR) should haveValue(expectedRate)
    provider.fxRate(Currency.USD, Currency.EUR) should haveValue(expectedRate)
    provider.fxRate(CurrencyPair.of(Currency.USD, Currency.EUR)) should haveValue(expectedRate)

    // The port of the original's exactly-one-invocation verification: the three lookups above
    // all depend on the underlying provider, and all three collapse to a single evaluation of
    // the function supplying it, which is the whole of the memoisation contract. The count is
    // asserted as exactly one rather than at least one - a provider that obtained its target
    // again for the second or third lookup would be a working provider and a broken one, and
    // only this number says so.
    supplier.callCount shouldBe 1
  }

  //-------------------------------------------------------------------------
  /**
   * Records how often the provider it holds is asked for.
   *
   * This is what replaces the mocked supplier of the Java test. [[get]] is the function handed
   * to [[FxRateProvider.lazily]] - through the literal `() => supplier.get()`, since the
   * factory takes an ordinary Scala function rather than the `Supplier` the original passed -
   * and [[callCount]] is what the two tests assert about, standing in for the original's
   * no-interactions and exactly-one-invocation verifications respectively.
   *
   * The counter is an `AtomicInteger` because the memoisation it observes is expected to
   * initialise once even when several threads force it at the same time; counting with a
   * mutable field would make this stub the weaker of the two. It also keeps the suite free of
   * mutable state and of any mutable collection, as the rest of this port is.
   *
   * A fresh instance is built by each test. Sharing one would make each count depend on
   * whichever test ran first and would leave the memoisation assertion meaning nothing.
   *
   * @param target  the provider to hand out when asked
   */
  private final class CountingSupplier(target: FxRateProvider) {

    /** The number of times [[get]] has been called. */
    private val calls: AtomicInteger = new AtomicInteger(0)

    /**
     * Returns the provider this stub holds, counting the call.
     *
     * @return the provider this stub holds
     */
    def get(): FxRateProvider = {
      // Bound to a wildcard because the new count is of no interest here; the tests read it
      // through callCount.
      val _ = calls.incrementAndGet()
      target
    }

    /**
     * Returns the number of times [[get]] has been called.
     *
     * @return the call count, zero if the provider has never been asked for
     */
    def callCount: Int = calls.get()
  }

  //-------------------------------------------------------------------------
  /**
   * Returns a provider that answers every pair with the specified rate.
   *
   * This is the target the stub hands out, and it plays the part of the function
   * `(baseCurrency, counterCurrency) -> expectedRate` that the Java test stubbed its mock to
   * return. A constant is enough for both tests: one of them never reaches the target at all,
   * and the other only needs to recognise the rate that came back as having come from here.
   *
   * @param rate  the rate to return for every pair of currencies
   * @return a provider answering every pair with that rate
   */
  private def constantProvider(rate: Double): FxRateProvider =
    FxRateProvider.fromFunction((_, _) => Right(rate))
}
