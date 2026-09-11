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
 * Five types convert to themselves, the currency of each amount they hold being replaced by the
 * requested one: [[CurrencyAmount]], [[Money]], [[BigMoney]], [[Payment]] and
 * [[CurrencyAmountArray]]. Two collapse to a simpler type, as described above:
 * [[MultiCurrencyAmount]] converts to a [[CurrencyAmount]], and [[MultiCurrencyAmountArray]]
 * to a [[CurrencyAmountArray]].
 *
 * [[AdjustablePayment]] is deliberately absent from that list, matching the type being ported:
 * it is resolved against reference data to produce a [[Payment]], and it is that [[Payment]]
 * which is convertible. A holder of an adjustable payment therefore resolves first and converts
 * afterwards, rather than converting an amount whose date is not yet fixed.
 *
 * ===A missing rate is a value, not an abandoned call===
 *
 * The type being ported declared that the conversion could raise a runtime exception when no FX
 * rate could be found, which left the possibility of failure out of the signature entirely. Here
 * the outcome is an `Either`: the converted object is a `Right`, and the absence of a rate the
 * conversion needed is a `Left` carrying the [[Failure]] produced by the provider - typically a
 * [[com.opengamma.strata.collect.result.Failure.CurrencyConversion]] naming the pair that could
 * not be converted. A caller consequently cannot use a converted amount without first deciding
 * what to do when the conversion was not possible, and no implementation needs to abandon the
 * call stack in order to report that state.
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
 * library implement it for their own measures, such as cash flows and sensitivities, as the
 * modules yet to be ported do. Sealing it would require every one of those types to be declared
 * in this file, which is neither possible across modules nor desirable within one.
 *
 * Every implementation in this module is immutable, so a conversion is inherently safe to call
 * from several threads at once. The type being ported required only thread-safety and allowed a
 * mutable implementation; an implementation outside this module is still held to that weaker
 * requirement, because a conversion that observes mutable state may be asked for concurrently.
 *
 * The trait carries no data of its own, only this contract, and so has no serialized form: the
 * types that implement it are serialized individually and this trait is not.
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
   * currency against itself - which is what [[FxRateProvider.minimal]] is for. Both behaviours
   * are those of the types being ported and are preserved by their implementations here.
   *
   * @param resultCurrency  the currency of the result
   * @param rateProvider  the provider of FX rates
   * @return the converted instance, expressed in the specified currency, or a failure when a
   *   rate required by the conversion is not available
   */
  def convertedTo(resultCurrency: Currency, rateProvider: FxRateProvider): Either[Failure, R]
}
