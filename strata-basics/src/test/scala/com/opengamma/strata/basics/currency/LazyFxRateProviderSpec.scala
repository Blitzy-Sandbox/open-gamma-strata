/*
 * Copyright (C) 2019 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[LazyFxRateProvider]], reached through [[FxRateProvider.lazily]].
 *
 * Two counts are the contract. A lookup between a currency and itself answers without obtaining
 * the underlying provider, so `testLaziness` expects a count of zero. The three lookups that do
 * depend on it collapse to exactly one evaluation of the function supplying it, which is the whole
 * of the memoisation contract, so `testDelegation` asserts exactly one rather than at least one.
 *
 * `fxRate(CurrencyPair)` is the load-bearing route in both tests: the two-currency lookup is
 * defined in terms of it, so a short-circuit or a memoisation written for that form alone is
 * observable only there.
 *
 * The call count is an `AtomicInteger`, and the pairs `testDelegation` records are an
 * `AtomicReference` over an immutable list, so both stay correct if forcing races. Every lookup
 * reports a missing rate as a `Left`, so `haveValue` asserts success and the rate together.
 */
final class LazyFxRateProviderSpec extends AnyFunSuite with Matchers {

  test("testLaziness") {
    // The target answers every pair with a rate no assertion below expects, so a lookup that
    // wrongly reached it fails on its value as well as on the call count.
    val supplier = new CountingSupplier(constantProvider(7d))
    val provider = FxRateProvider.lazily(() => supplier.get())

    provider.convert(1d, Currency.USD, Currency.USD) should haveValue(1d)
    provider.fxRate(Currency.USD, Currency.USD) should haveValue(1d)
    provider.fxRate(CurrencyPair.of(Currency.USD, Currency.USD)) should haveValue(1d)

    supplier.callCount shouldBe 0
  }

  test("testDelegation") {
    val expectedRate = 3d
    val asked = new AtomicReference[List[(Currency, Currency)]](List.empty)
    val target: FxRateProvider = FxRateProvider.fromFunction { (baseCurrency, counterCurrency) =>
      val askedAbout: (Currency, Currency) = (baseCurrency, counterCurrency)
      val _ = asked.updateAndGet(recorded => recorded :+ askedAbout)
      askedAbout match {
        // only USD/EUR carries the expected rate, and the reverse direction carries another, so a
        // lookup that swapped base and counter is wrong in its rate as well as in what it asked
        case (Currency.USD, Currency.EUR) => Right(expectedRate)
        case (Currency.EUR, Currency.USD) => Right(0.25d)
        case (base, counter) =>
          Left(Failure.CurrencyConversion(s"No rate held for $base/$counter"))
      }
    }
    // takes and clears the record, so each assertion below reads one lookup rather than the total
    def takeAsked(): List[(Currency, Currency)] = asked.getAndSet(List.empty)

    val supplier = new CountingSupplier(target)
    val provider = FxRateProvider.lazily(() => supplier.get())

    // 250 at a rate of three pins the product of amount and rate, which an amount of one could
    // not distinguish from the rate alone
    provider.convert(250d, Currency.USD, Currency.EUR) should haveValue(750d)
    takeAsked() shouldBe List((Currency.USD, Currency.EUR))

    provider.fxRate(Currency.USD, Currency.EUR) should haveValue(expectedRate)
    takeAsked() shouldBe List((Currency.USD, Currency.EUR))

    provider.fxRate(CurrencyPair.of(Currency.USD, Currency.EUR)) should haveValue(expectedRate)
    takeAsked() shouldBe List((Currency.USD, Currency.EUR))

    supplier.callCount shouldBe 1
  }

  //-------------------------------------------------------------------------
  /**
   * Hands out the provider it holds and counts how often it is asked for.
   *
   * The count is an `AtomicInteger` because the memoisation it observes initialises once even if
   * several threads force it at the same time. Each test builds its own instance, so neither
   * count depends on which test ran first.
   */
  private final class CountingSupplier(target: FxRateProvider) {

    private val calls: AtomicInteger = new AtomicInteger(0)

    /** Returns the provider this stub holds, counting the call. */
    def get(): FxRateProvider = {
      val _ = calls.incrementAndGet()
      target
    }

    /** Returns the number of calls to [[get]], zero if the provider has never been asked for. */
    def callCount: Int = calls.get()
  }

  //-------------------------------------------------------------------------
  /** Returns a provider that answers every pair with the specified rate. */
  private def constantProvider(rate: Double): FxRateProvider =
    FxRateProvider.fromFunction((_, _) => Right(rate))
}
