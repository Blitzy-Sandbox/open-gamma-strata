/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import com.opengamma.strata.collect.result.Failure

/**
 * Defines a standard mechanism for converting an object representing one or more monetary
 * amounts to a single currency.
 *
 * The single method allows a monetary object to be converted to a similar object expressed in
 * terms of the specified currency. Any FX conversion that the change of currency requires is
 * performed with rates taken from the [[FxRateProvider]] handed to the method, so a monetary
 * type never has to hold rates of its own or decide where they come from.
 *
 * ===The result type is free===
 *
 * The conversion is permitted to return a type other than the implementing one, which is why
 * the result type `R` is a parameter of the trait rather than the implementing type itself.
 * That freedom is the whole point of the abstraction: expressing a multi-currency object in a
 * single currency generally collapses it into a simpler shape. [[MultiCurrencyAmount]] is the
 * example that motivates the design - it holds an amount per currency and is therefore an
 * `FxConvertible[CurrencyAmount]`, because once every amount has been converted into one
 * currency the amounts sum to a single [[CurrencyAmount]]. A method that returned the
 * implementing type instead could not express that, and would force such a type either to
 * return a one-entry collection or to leave the trait unimplemented.
 *
 * For the same reason the parameter is invariant. The trait describes a conversion that
 * produces exactly the stated type, so a caller holding an `FxConvertible[CurrencyAmount]`
 * relies on the result being a `CurrencyAmount` and nothing wider; making the parameter
 * covariant would weaken that guarantee without any implementation in this module needing it.
 *
 * ===The implementations in this module===
 *
 * Five types convert to themselves, each amount they hold being restated in the requested
 * currency: [[CurrencyAmount]], [[Money]], [[BigMoney]], [[Payment]] and
 * [[CurrencyAmountArray]]. Two collapse to a simpler type, as described above:
 * [[MultiCurrencyAmount]] converts to a [[CurrencyAmount]], and [[MultiCurrencyAmountArray]]
 * to a [[CurrencyAmountArray]].
 *
 * [[AdjustablePayment]] is deliberately absent from that list: it is resolved against reference
 * data to produce a [[Payment]], and it is that [[Payment]] which is convertible. A holder of an
 * adjustable payment therefore resolves first and converts afterwards, rather than converting an
 * amount whose date is still subject to a business day adjustment.
 *
 * ===A missing rate is a value, not an abandoned call===
 *
 * The outcome of a conversion is an `Either`: the converted object is a `Right`, and the absence
 * of a rate the conversion needed is a `Left` carrying the [[Failure]] produced by the provider -
 * typically a [[com.opengamma.strata.collect.result.Failure.CurrencyConversion]] naming the pair
 * that could not be converted. The possibility of failure is therefore part of the signature: a
 * caller cannot use a converted amount without first deciding what to do when the conversion was
 * not possible, and no implementation abandons the call stack in order to report that state.
 *
 * The failure channel is that of the provider, so an implementation is written by mapping over
 * the provider's own result rather than by inspecting it - here `rebuild` stands for whatever
 * the implementing type does with an amount that has already been converted:
 *
 * {{{
 * rateProvider.convert(amount, currency, resultCurrency).map(rebuild)
 * }}}
 *
 * ===Implementing the trait===
 *
 * The trait is open rather than a closed family, and deliberately so. It is implemented from
 * many files - every monetary type listed above lives in its own - and further modules of the
 * library implement it for their own measures, such as cash flows and sensitivities. Sealing it
 * would require every one of those types to be declared in this file, which is neither possible
 * across modules nor desirable within one.
 *
 * An implementation is required to convert an instance of itself to the requested currency using
 * only the rates the supplied provider answers with, to report a rate it needs and cannot obtain
 * as the provider's failure rather than by any other means, and to be safe to call from several
 * threads at once, because a conversion that observes mutable state may be asked for
 * concurrently. Every implementation in this module is immutable, which satisfies the last of
 * those by construction.
 *
 * @tparam R  the result type expressed in a single currency
 */
trait FxConvertible[R] {

  /**
   * Converts this instance to an equivalent amount in the specified currency.
   *
   * The result, which may be of a different type, is expressed in terms of the given currency.
   * Any FX conversion that is required uses rates from the provider, and the conversion fails
   * exactly when a rate it needs is unavailable, carrying the failure the provider reported.
   *
   * An instance holding amounts in a single currency, that currency being the requested one, is
   * returned unchanged without the provider being consulted, so such a conversion succeeds even
   * under a provider that supplies no rates at all. An instance holding several currencies has
   * no such shortcut: it asks the provider for a rate for every amount it holds, including any
   * amount already in the requested currency, and therefore needs a provider that answers for a
   * currency against itself - which is what [[FxRateProvider.minimal]] is for.
   *
   * @param resultCurrency  the currency of the result
   * @param rateProvider  the provider of FX rates
   * @return the converted instance, expressed in the specified currency, or a failure when a
   *   rate required by the conversion is not available
   */
  def convertedTo(resultCurrency: Currency, rateProvider: FxRateProvider): Either[Failure, R]
}
