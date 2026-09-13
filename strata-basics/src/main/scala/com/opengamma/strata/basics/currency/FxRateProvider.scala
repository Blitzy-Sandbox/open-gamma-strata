/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.result.Failure

/**
 * A provider of FX rates.
 *
 * This provides the ability to obtain an FX rate. The trait does not mandate when the rate
 * applies, however it typically represents the current rate. [[FxMatrix]] and [[FxRate]] are the
 * implementations this module supplies; a host that reads rates from market data supplies its
 * own, which is why the trait is deliberately open rather than a closed family.
 *
 * ===Every lookup can fail, and says so in its type===
 *
 * A provider holds rates for some set of currencies and nothing forces a caller to ask about a
 * pair the provider knows. The outcome of a lookup is therefore an `Either`: a rate is a
 * `Right`, and the absence of one is a `Left` carrying a
 * [[com.opengamma.strata.collect.result.Failure.CurrencyConversion]] whose message names the
 * pair that could not be converted. A caller cannot use a rate without first deciding what to
 * do when there is none, and an implementation reports a missing rate by returning that
 * failure.
 *
 * The same applies to the two conversions below: they are the lookup followed by a
 * multiplication, so they fail exactly when the lookup does and carry the same failure.
 *
 * ===Implementing the trait===
 *
 * The two-currency `fxRate` is the only abstract member; everything else is written in terms of
 * it and is overridden only by an implementation that can answer a question more directly - as
 * the provider behind [[FxRateProvider.lazily]] does, to avoid obtaining its underlying
 * provider for a conversion it can answer on its own.
 *
 * Because there is a single abstract member, a function literal can be written wherever a
 * provider is expected:
 *
 * {{{
 * val flat: FxRateProvider = (base, counter) => Right(2.5d)
 * }}}
 *
 * [[FxRateProvider.fromFunction]] performs the same conversion explicitly, which is the form to
 * prefer where the expected type is not already fixed by the surrounding context.
 *
 * Implementations do not have to be immutable, but calls to the methods must be thread-safe.
 */
trait FxRateProvider {

  /**
   * Gets the FX rate for the specified currency pair.
   *
   * The rate returned is the rate from the base currency to the counter currency as defined by
   * this formula: `(1 * baseCurrency = fxRate * counterCurrency)`.
   *
   * This is the single abstract member of the trait. An implementation is expected to answer
   * for the currencies it holds rates for and to return a
   * [[com.opengamma.strata.collect.result.Failure.CurrencyConversion]] for any other pair,
   * rather than to signal the absence of a rate in some other way.
   *
   * @param baseCurrency  the base currency, to convert from
   * @param counterCurrency  the counter currency, to convert to
   * @return the FX rate for the currency pair, or a failure when no rate is available
   */
  def fxRate(baseCurrency: Currency, counterCurrency: Currency): Either[Failure, Double]

  /**
   * Gets the FX rate for the specified currency pair.
   *
   * The rate returned is the rate from the base currency to the counter currency as defined by
   * this formula: `(1 * baseCurrency = fxRate * counterCurrency)`.
   *
   * This is the ordered-pair form of the lookup above and agrees with it for every pair, since
   * it is defined as that lookup applied to the two currencies of the pair.
   *
   * @param currencyPair  the ordered currency pair defining the rate required
   * @return the FX rate for the currency pair, or a failure when no rate is available
   */
  def fxRate(currencyPair: CurrencyPair): Either[Failure, Double] =
    fxRate(currencyPair.base, currencyPair.counter)

  /**
   * Converts an amount in a currency to an amount in a different currency using this provider.
   *
   * The amount is multiplied by the rate from `fromCurrency` to `toCurrency`, so the conversion
   * succeeds exactly when that rate is available and carries the failure of the lookup when it
   * is not.
   *
   * {{{
   * provider.convert(100d, Currency.GBP, Currency.USD)  // Right(125.0) at a rate of 1.25
   * }}}
   *
   * @param amount  an amount in `fromCurrency`
   * @param fromCurrency  the currency of the amount
   * @param toCurrency  the currency into which the amount should be converted
   * @return the amount converted into `toCurrency`, or a failure when no rate is available
   */
  def convert(amount: Double, fromCurrency: Currency, toCurrency: Currency): Either[Failure, Double] =
    fxRate(fromCurrency, toCurrency).map(rate => amount * rate)

  /**
   * Converts a decimal amount in a currency to an amount in a different currency using this
   * provider.
   *
   * This is the exact-decimal counterpart of the conversion above: the rate is looked up as a
   * `Double` and the amount is multiplied by it through the decimal's own `multipliedBy`, which
   * converts the rate to a decimal and multiplies exactly, rather than the amount being rounded
   * into binary floating point and back. The result therefore carries the exact digits of the
   * product.
   *
   * Decimal multiplication has a domain: the decimal type carries at most 18 significant
   * digits and cannot represent a rate that is not finite. A rate outside that domain is a
   * caller contract violation rather than a missing rate, so it is signalled the way the
   * decimal type signals it - by an `IllegalArgumentException` from the multiplication - and
   * not folded into the failure channel of the lookup. A provider that holds finite rates
   * cannot reach it.
   *
   * @param amount  an amount in `fromCurrency`
   * @param fromCurrency  the currency of the amount
   * @param toCurrency  the currency into which the amount should be converted
   * @return the amount converted into `toCurrency`, or a failure when no rate is available
   * @throws java.lang.IllegalArgumentException if the rate is not finite, or if the product
   *   needs more than 18 digits at scale zero
   */
  def convert(amount: Decimal, fromCurrency: Currency, toCurrency: Currency): Either[Failure, Decimal] =
    fxRate(fromCurrency, toCurrency).map(rate => amount.multipliedBy(rate))
}

/**
 * Provides the ways of obtaining an FX rate provider that do not come from market data.
 *
 * The three providers here are the ones the library itself needs: two that decline to convert,
 * used where a calculation must be given a provider but is not expected to use it, and one that
 * defers to another provider only if a conversion is actually asked for.
 *
 * [[FxRateProvider.noConversion]] and [[FxRateProvider.minimal]] differ in one case and it is
 * the case that matters. Both refuse to convert between two different currencies; they disagree
 * about a currency and itself, where `noConversion` refuses as well and `minimal` answers with a
 * rate of one. A caller that must not perform FX at all therefore takes `noConversion` and gets
 * a failure rather than a silent identity conversion, while a caller that only ever converts an
 * amount into the currency it is already in takes `minimal` and never needs a rate. The
 * wording of the two failure messages differs, so that a failure identifies which of the two
 * providers produced it.
 */
object FxRateProvider {

  /**
   * The provider returned by [[noConversion]].
   *
   * The provider holds no state, so one instance is created here and handed out on every call
   * rather than allocated per invocation.
   */
  private val NoConversionProvider: FxRateProvider =
    fromFunction((baseCurrency, counterCurrency) =>
      Left(
        Failure.CurrencyConversion(
          s"FX rate conversion is not supported, requested for $baseCurrency/$counterCurrency")))

  /**
   * The provider returned by [[minimal]].
   *
   * As above, the provider holds no state and a single instance serves every caller.
   */
  private val MinimalProvider: FxRateProvider =
    fromFunction((baseCurrency, counterCurrency) =>
      if (baseCurrency == counterCurrency) {
        Right(1d)
      } else {
        Left(
          Failure.CurrencyConversion(
            s"FX rate conversion is not supported for $baseCurrency/$counterCurrency"))
      })

  /**
   * Obtains a provider that looks up rates using the specified function.
   *
   * This makes the conversion from a function to a provider explicit, so that a call site does
   * not have to rely on the expected type being inferred at the point of use:
   *
   * {{{
   * val flat = FxRateProvider.fromFunction((base, counter) => Right(2.5d))
   * }}}
   *
   * The function is the implementation of the pair lookup, so the returned provider is as
   * immutable and thread-safe as the function given to it, and every other method of the
   * provider is the trait's own definition in terms of that lookup.
   *
   * @param f  the function returning the rate from the base currency to the counter currency
   * @return a provider that looks up rates using the function
   */
  def fromFunction(f: (Currency, Currency) => Either[Failure, Double]): FxRateProvider =
    new FxRateProvider {
      override def fxRate(baseCurrency: Currency, counterCurrency: Currency): Either[Failure, Double] =
        f(baseCurrency, counterCurrency)
    }

  /**
   * Returns a provider that always fails.
   *
   * The provider declines every lookup, including one for a currency and itself, and the
   * failure it returns is a
   * [[com.opengamma.strata.collect.result.Failure.CurrencyConversion]] reading
   * `FX rate conversion is not supported, requested for GBP/USD`. This is the provider to pass
   * to a calculation that is not permitted to perform FX conversion at all: refusing the
   * identity case as well is what turns an unintended conversion into a failure rather than
   * letting it pass unnoticed.
   *
   * Where the identity case should be allowed, use [[minimal]] instead.
   *
   * @return a provider that always fails
   */
  def noConversion(): FxRateProvider = NoConversionProvider

  /**
   * Returns a provider that provides minimal behaviour.
   *
   * The provider returns a rate of one when the two currencies are the same, and fails for any
   * two different currencies with a
   * [[com.opengamma.strata.collect.result.Failure.CurrencyConversion]] reading
   * `FX rate conversion is not supported for GBP/USD`. This is the provider to pass where a
   * conversion is part of the signature but only ever applied to an amount that is already in
   * the target currency.
   *
   * Where even the identity case should fail, use [[noConversion]] instead.
   *
   * @return a provider that converts between identical currencies only
   */
  def minimal(): FxRateProvider = MinimalProvider

  /**
   * Returns a provider that delays obtaining its underlying provider until one is actually
   * needed.
   *
   * This is typically useful where a provider built from market data '''may''' be needed,
   * but loading that data should be put off until it is certain to be needed. The returned
   * provider answers a conversion between a currency and itself, and a rate for a pair of
   * identical currencies, on its own; any other question forces the function, once, and is
   * delegated to the provider it yields.
   *
   * {{{
   * val provider = FxRateProvider.lazily(() => loadMarketDataProvider())
   * provider.fxRate(Currency.USD, Currency.USD)  // Right(1.0); nothing is loaded
   * provider.fxRate(Currency.GBP, Currency.USD)  // loads once, then delegates
   * }}}
   *
   * The function is invoked at most once however many times the provider is used and from
   * however many threads, and the provider it yields is kept: the memoisation is a `lazy val`,
   * which initialises once and is safe for several threads to force at the same time. The
   * method is spelled `lazily` because `lazy` is a reserved word.
   *
   * @param target  the function supplying the underlying provider
   * @return a provider that obtains the underlying provider only when it is needed
   */
  def lazily(target: () => FxRateProvider): FxRateProvider = new LazyFxRateProvider(target)
}

/**
 * An [[FxRateProvider]] that delays obtaining its underlying provider until one is actually
 * needed.
 *
 * This is typically useful where a provider built from market data '''may''' be needed, but
 * loading that data should be put off until it is certain to be needed. It is reached through
 * [[FxRateProvider.lazily]] rather than constructed directly, which is why it is visible only
 * inside this package.
 *
 * ===What it answers without forcing the function===
 *
 * The point of the provider is that it does not obtain the underlying provider unless the
 * answer actually depends on it, so the two questions that have the same answer for every
 * provider are answered here:
 *
 *   - a conversion of an amount between a currency and itself returns the amount, and
 *   - a rate for a pair of identical currencies is one.
 *
 * The two-currency rate lookup is routed through the pair form rather than implemented
 * separately, which is what extends the second of those to it, and that indirection is
 * deliberate. The decimal conversion inherits the trait's definition, which multiplies the
 * amount by the result of a rate lookup, so it too answers a currency and itself without
 * forcing the function.
 *
 * Anything else - a rate between two different currencies, a conversion between them - forces
 * the function once and delegates.
 *
 * The class is immutable and thread-safe as long as the function it is given is, and holding a
 * deferred computation rather than data is why it is a plain class with reference equality: two
 * providers built from two functions are not interchangeable even where the functions would
 * yield equal providers, since observing that would mean invoking both.
 *
 * @param target  the function supplying the underlying provider
 */
private[currency] final class LazyFxRateProvider(target: () => FxRateProvider) extends FxRateProvider {

  /**
   * The underlying provider, obtained on first use and then kept.
   *
   * A `lazy val` is the whole of the memoisation: it invokes the function the first time this
   * field is read, keeps what it returns, and is initialised exactly once even when several
   * threads read it at the same time. No field of this class is mutable.
   */
  private lazy val underlying: FxRateProvider = target()

  /**
   * Converts an amount in a currency to an amount in a different currency.
   *
   * An amount is returned unchanged when the two currencies are the same, without the
   * underlying provider being obtained. Any other conversion is delegated to it.
   *
   * @param amount  an amount in `fromCurrency`
   * @param fromCurrency  the currency of the amount
   * @param toCurrency  the currency into which the amount should be converted
   * @return the amount converted into `toCurrency`, or a failure when no rate is available
   */
  override def convert(amount: Double, fromCurrency: Currency, toCurrency: Currency): Either[Failure, Double] =
    if (fromCurrency == toCurrency) {
      Right(amount)
    } else {
      underlying.convert(amount, fromCurrency, toCurrency)
    }

  /**
   * Gets the FX rate for the specified currencies.
   *
   * This is deliberately defined as the pair form applied to the two currencies, so that the
   * rate of one for a currency and itself is answered there for both forms of the lookup.
   *
   * @param baseCurrency  the base currency, to convert from
   * @param counterCurrency  the counter currency, to convert to
   * @return the FX rate for the currencies, or a failure when no rate is available
   */
  override def fxRate(baseCurrency: Currency, counterCurrency: Currency): Either[Failure, Double] =
    fxRate(CurrencyPair.of(baseCurrency, counterCurrency))

  /**
   * Gets the FX rate for the specified currency pair.
   *
   * A pair of identical currencies has a rate of one, which is returned without the underlying
   * provider being obtained. Any other pair is delegated to it.
   *
   * @param currencyPair  the ordered currency pair defining the rate required
   * @return the FX rate for the currency pair, or a failure when no rate is available
   */
  override def fxRate(currencyPair: CurrencyPair): Either[Failure, Double] =
    if (currencyPair.isIdentity) {
      Right(1d)
    } else {
      underlying.fxRate(currencyPair)
    }

  /**
   * Returns a description of this provider.
   *
   * The description names the provider only. Rendering the underlying provider would mean
   * obtaining it, which is exactly what this class exists to put off, so a provider that has
   * not been used yet is not forced by being logged.
   *
   * @return a description of this provider
   */
  override def toString: String = "LazyFxRateProvider"
}
