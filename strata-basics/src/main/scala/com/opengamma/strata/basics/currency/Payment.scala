/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.time.LocalDate

import cats.Hash
import cats.Show

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.json.Codecs

/**
 * A single payment of a known amount on a specific date.
 *
 * This type represents a payment whose date and amount are both already known: an amount in one
 * currency, and the date on which it changes hands. It is the settled form of a cash flow, and it
 * is what [[AdjustablePayment]] becomes once its date has been adjusted against reference data.
 *
 * ===The sign carries the direction===
 *
 * A payment has no separate pay-or-receive flag; the sign of the amount is the direction. A
 * '''negative''' amount is money to be paid away and a '''positive''' amount money to be
 * received, so a portfolio of payments is summed without any per-payment inspection of
 * direction. The two factories [[Payment.ofPay]] and [[Payment.ofReceive]] exist for callers
 * that know the direction but hold an amount of unknown sign, and each normalises the sign it
 * needs rather than negating unconditionally - see their own documentation, since the
 * distinction is observable.
 *
 * ===Construction is total, and how the amount is obtained decides where failure lives===
 *
 * A payment is a pair of two values that are already valid, and it imposes no further invariant
 * of its own: any amount [[CurrencyAmount]] admits, paid on any date, is a payment. The type is
 * therefore a plain `case class` with a public constructor, `apply` and `copy`, and building one
 * cannot fail:
 *
 * {{{
 * val payment = Payment(gbp1000, LocalDate.of(2015, 6, 30))
 * val moved = payment.copy(date = LocalDate.of(2015, 7, 1))
 * }}}
 *
 * Failure appears at exactly one door, and it is inherited rather than introduced: the
 * three-argument `Payment.of` takes a raw `Double` and so has to build a [[CurrencyAmount]]
 * first, which refuses a value that is not a number, so that factory returns a
 * [[com.opengamma.strata.collect.FailureOr]]. Every other route in - the two-argument `of`,
 * `ofPay`, `ofReceive`, the constructor and `copy` - takes an amount that has already been
 * checked and is consequently total.
 *
 * The JSON codec published by the companion is the serialized form of a payment.
 *
 * ===Arithmetic, adjustment and conversion===
 *
 * Three operations are defined, each returning a new payment and leaving this one untouched:
 * [[negated]] reverses the direction, [[adjustDate]] moves the date, and [[convertedTo]]
 * re-expresses the amount in another currency. The last is the [[FxConvertible]] implementation
 * of the type and is the only one that can fail, because a conversion between two different
 * currencies needs a rate that the provider may not hold.
 *
 * Both [[adjustDate]] and [[convertedTo]] return '''this''' instance where the operation asks for
 * no change - an adjuster that leaves the date alone, or a conversion into the currency the
 * payment already has. The short-circuit is part of the contract and not merely an optimisation,
 * since it is what lets a payment be pushed through a chain of adjustments without allocating at
 * every step.
 *
 * ===Equality, ordering and text===
 *
 * Equality is the case class's own, which compares the amount by the equality [[CurrencyAmount]]
 * defines - a bit-pattern comparison, under which an amount that is not a number equals itself
 * and a negative zero differs from a positive zero - and the date by its own. No bit-pattern
 * comparison is arranged here, deliberately: the only `Double` a payment holds is inside the
 * amount, and one source of truth for the equality of a double belongs with the type that holds
 * it. The companion publishes a `Hash` and a `Show`, and no `Order`, since whether an amount or
 * a date ranks first depends on what the caller is doing.
 *
 * Instances are immutable and every operation is a pure function of the instance and its
 * arguments, so a payment may be shared freely between threads.
 *
 * @param value  the amount of the payment, signed, negative to pay and positive to receive
 * @param date  the date on which the payment is made, normally a valid business day
 * @see [[AdjustablePayment]] for a payment whose date is still to be adjusted against reference
 *   data
 * @see [[CurrencyAmount]] for the amount held and for what a valid amount is
 * @see [[FxConvertible]] for the conversion contract this type implements
 */
final case class Payment(value: CurrencyAmount, date: LocalDate)
    extends FxConvertible[Payment]
    with NoJavaSerialization {

  /**
   * Gets the currency of the payment.
   *
   * This is `value.currency`. The currency is a property of the amount rather than of the
   * payment, so changing it means converting the payment - see [[convertedTo]].
   *
   * @return the currency of the amount paid
   */
  def getCurrency: Currency = value.currency

  /**
   * Gets the amount of the payment.
   *
   * This is `value.amount`. The amount is signed: negative to pay, positive to receive.
   *
   * @return the signed amount paid, in the currency of the payment
   */
  def getAmount: Double = value.amount

  /**
   * Adjusts the payment date using the specified function.
   *
   * The function is applied to the date and the result becomes the date of the payment returned;
   * the amount is untouched. Where the function returns the date it was given, '''this''' payment
   * is returned rather than an equal copy of it. The function is expected to be pure; it is
   * applied exactly once, to this payment's date.
   *
   * ===The adjuster is a date function===
   *
   * The parameter is the Scala function type and '''only''' that, which keeps every idiomatic
   * call shape available - a placeholder lambda, an explicitly typed lambda, and a method
   * converted to a function - because the parameter type is unambiguous:
   *
   * {{{
   * payment.adjustDate(_.plusDays(1))
   * payment.adjustDate(date => if (date.getDayOfWeek == SATURDAY) date.plusDays(2) else date)
   * }}}
   *
   * There is no overload taking a `TemporalAdjuster`: because a `TemporalAdjuster` is itself a
   * single-abstract-method type, its presence would stop the parameter type of an un-annotated
   * lambda being inferred and stop a method being converted to a function at the call site, so
   * it would cost exactly the two shapes above. Both kinds of adjuster reach this method in one
   * step regardless:
   *
   * {{{
   * // any temporal adjuster, applied through `LocalDate.with` - `with` is a Scala keyword,
   * // hence the back-ticks
   * payment.adjustDate(_.`with`(TemporalAdjusters.lastDayOfMonth))
   *
   * // a DateAdjuster, which this library pairs with the date function it delegates to
   * val adjuster = businessDayAdjustment.resolve(refData)    // Either[Failure, DateAdjuster]
   * adjuster.map(adj => payment.adjustDate(adj.adjust))
   * }}}
   *
   * @param adjuster  the function to apply to the payment date
   * @return the payment with the adjusted date, or this payment where the date is unchanged
   * @throws java.time.DateTimeException if the function cannot adjust the date
   * @throws java.lang.ArithmeticException if the adjustment overflows the range of a date
   */
  def adjustDate(adjuster: LocalDate => LocalDate): Payment = {
    val adjusted = adjuster(date)
    if (adjusted == date) this else copy(date = adjusted)
  }

  /**
   * Returns a payment with the amount negated.
   *
   * A payment to be made becomes one to be received and the other way about, on the same date.
   * The negation is that of [[CurrencyAmount.negated]], so it is unconditional - applying it
   * twice returns the amount it started from - and it is the operation to use where the direction
   * of a known payment is reversed. Where instead the direction is known and the sign of the
   * amount is not, use [[Payment.ofPay]] or [[Payment.ofReceive]].
   *
   * This instance is immutable and unaffected by this method.
   *
   * @return a payment holding the negated amount, on the same date
   */
  def negated: Payment = Payment.of(value.negated, date)

  /**
   * Converts this payment to an equivalent payment in the specified currency.
   *
   * The amount is re-expressed in the requested currency at the rate the provider supplies, and
   * the date is unchanged - a conversion changes the currency of a payment and never when it is
   * made. A payment already in the requested currency is returned as '''this''' instance and the
   * provider is not consulted, so such a conversion succeeds even under
   * [[FxRateProvider.noConversion]].
   *
   * The converted payment is a `Right`, and a rate the provider cannot supply is the `Left` the
   * provider reported, typically a
   * [[com.opengamma.strata.collect.result.Failure.CurrencyConversion]] naming the pair.
   *
   * {{{
   * payment.convertedTo(Currency.EUR, provider)   // Right(Payment(EUR 1600, 2015-06-30))
   * payment.convertedTo(payment.getCurrency, provider)   // Right(this payment), no rate needed
   * }}}
   *
   * @param resultCurrency  the currency of the result
   * @param rateProvider  the provider of FX rates
   * @return this payment expressed in the result currency, or the failure the provider reported
   *   for the rate the conversion needed
   */
  override def convertedTo(
      resultCurrency: Currency,
      rateProvider: FxRateProvider): FailureOr[Payment] =
    if (getCurrency == resultCurrency) {
      Right(this)
    } else {
      value.convertedTo(resultCurrency, rateProvider).map(converted => Payment.of(converted, date))
    }

  /**
   * Returns this payment as text.
   *
   * The two fields are named in declaration order between braces, as in
   * `Payment{value=GBP 1000, date=2015-06-30}`, with the amount rendered by
   * [[CurrencyAmount.toString]] and the date in ISO-8601. It is what the `Show` instance of the
   * companion renders.
   *
   * @return the rendering of this payment
   */
  override def toString: String = s"Payment{value=$value, date=$date}"
}

/**
 * The factories, typeclass instances and JSON form of [[Payment]].
 *
 * The four factories differ only in how the amount reaches them: from a currency and a raw
 * number, from an amount that has already been checked, or from such an amount whose sign is to
 * be normalised in one direction or the other. The constructor and `copy` of the case class are
 * public alongside them, since a payment imposes no invariant of its own, so a caller holding an
 * amount and a date needs no factory at all.
 *
 * Two typeclass instances are published, and exactly two: a `Hash`, which is the single
 * equality-bearing instance of the type - `Hash` extends `Eq`, so declaring an `Eq` as well would
 * leave two instances that could disagree and one of them ambiguous - and a `Show`. There is
 * deliberately no `Order`, since whether an amount or a date ranks first depends on what the
 * caller is doing. A caller that needs one sorts by the field it means, as in
 * `payments.sortBy(_.date)`.
 */
object Payment {

  /**
   * Obtains an instance representing an amount in a currency.
   *
   * Whether the payment is to be paid or received is determined by the sign of the amount, which
   * is taken as given: a negative amount is a payment made and a positive amount one received.
   *
   * This is the one factory that can fail, and it fails for one reason: the amount arrives as a
   * raw `Double` and has to become a [[CurrencyAmount]], which refuses a value that is not a
   * number. The failure is that of [[CurrencyAmount.of]], reported unchanged, so the caller sees
   * the same reason and wording whichever of the two doors it came through. Where an amount is
   * already in hand, the two-argument factory below is total.
   *
   * {{{
   * Payment.of(Currency.GBP, 1000d, date)        // Right(Payment{value=GBP 1000, …})
   * Payment.of(Currency.GBP, Double.NaN, date)   // Left(Failure.Invalid("… must not be NaN"))
   * }}}
   *
   * @param currency  the currency of the payment
   * @param amount  the signed amount of the payment
   * @param date  the date on which the payment is made
   * @return the payment, or the failure describing why the amount is not one this library holds
   */
  def of(currency: Currency, amount: Double, date: LocalDate): FailureOr[Payment] =
    CurrencyAmount.of(currency, amount).map(checked => Payment(checked, date))

  /**
   * Obtains an instance representing an amount.
   *
   * Whether the payment is to be paid or received is determined by the sign of the amount, which
   * is taken as given. Construction cannot fail: the amount has already been checked by the type
   * that holds it, and a payment adds no invariant to it.
   *
   * @param value  the signed amount of the payment
   * @param date  the date on which the payment is made
   * @return the payment
   */
  def of(value: CurrencyAmount, date: LocalDate): Payment = Payment(value, date)

  /**
   * Obtains an instance representing an amount to be paid.
   *
   * The sign of the amount is '''normalised''' to be negative, marking the payment as one made.
   * That is [[CurrencyAmount.negative]] and not [[CurrencyAmount.negated]], and the difference is
   * observable: an amount that is already negative, and a zero, are passed through unchanged,
   * whereas an unconditional negation would turn a negative amount back into a positive one. So
   * `ofPay` applied twice is `ofPay`, which is what makes it safe to use on an amount of unknown
   * sign - the use it exists for. Where the direction of a payment is to be ''reversed'' rather
   * than asserted, use [[Payment.negated]].
   *
   * {{{
   * Payment.ofPay(gbp1000, date).getAmount    // -1000.0
   * Payment.ofPay(gbpM1000, date).getAmount   // -1000.0, unchanged
   * }}}
   *
   * @param value  the amount of the payment, of either sign
   * @param date  the date on which the payment is made
   * @return the payment, holding an amount that is negative or zero
   */
  def ofPay(value: CurrencyAmount, date: LocalDate): Payment = Payment(value.negative, date)

  /**
   * Obtains an instance representing an amount to be received.
   *
   * The sign of the amount is '''normalised''' to be positive, marking the payment as one
   * received. That is [[CurrencyAmount.positive]] and not a negation: an amount that is already
   * positive, and a zero, are passed through unchanged, and only a negative amount is turned
   * round. `ofReceive` applied twice is therefore `ofReceive`, and the note under
   * [[Payment.ofPay]] about reversing a direction applies here in the same way.
   *
   * {{{
   * Payment.ofReceive(gbpM1000, date).getAmount   // 1000.0
   * Payment.ofReceive(gbp1000, date).getAmount    // 1000.0, unchanged
   * }}}
   *
   * @param value  the amount of the payment, of either sign
   * @param date  the date on which the payment is made
   * @return the payment, holding an amount that is positive or zero
   */
  def ofReceive(value: CurrencyAmount, date: LocalDate): Payment = Payment(value.positive, date)

  /**
   * The hashing and equality of payments.
   *
   * Taken from the `equals` and `hashCode` of the case class, which compare and mix the two
   * fields by their own: the amount by the bit-pattern equality [[CurrencyAmount]] defines, so
   * that an amount which is not a number equals itself and a negative zero is distinct from a
   * positive zero, and the date by the equality of the platform. Nothing about the hash depends on
   * where an instance sits in memory, so the hash of a payment is the same in every run of every
   * program.
   *
   * This is the type's only equality-bearing instance; `Eq[Payment]` is obtained from it by
   * subtyping rather than declared separately. There is no `Order`, as the companion's own
   * documentation explains.
   *
   * @return the hashing of payments
   */
  implicit val hash: Hash[Payment] = Hash.fromUniversalHashCode[Payment]

  /**
   * The rendering of payments as text.
   *
   * Renders what [[Payment.toString]] renders, the `Payment{value=GBP 1000, date=2015-06-30}`
   * form, so the two ways of putting a payment into a message agree.
   *
   * @return the rendering of a payment
   */
  implicit val show: Show[Payment] = Show.show(_.toString)

  /**
   * The JSON encoding of payments.
   *
   * The encoding is derived when this file is compiled, so no part of it inspects a class while
   * the program runs. A payment encodes as an object holding its two fields under their own
   * names, in declaration order, each written by the codec its own type publishes - the amount as
   * the object of [[CurrencyAmount]], and the date as an ISO-8601 string:
   *
   * {{{
   * {"value":{"currency":"GBP","amount":1000.0},"date":"2015-06-30"}
   * }}}
   *
   * An amount outside the real numbers is written by the amount's own codec as the tagged string
   * that codec defines - `"Infinity"` for an infinite payment - so every payment this type admits
   * survives a round trip. A payment holds both of its fields, the amount and the date, so
   * nothing is ever omitted here; the encoder is wrapped in the same policy every product encoder
   * of this library carries, which omits a field holding no value, so that the rule holds of
   * every product encoder alike.
   *
   * @return the JSON encoding of a payment
   */
  implicit val encoder: Encoder[Payment] = Codecs.dropNulls(deriveEncoder[Payment])

  /**
   * The JSON decoding of payments.
   *
   * This is the inverse of the encoding above and is likewise derived at compile time. Both fields
   * have to be present. The amount is read by its own decoder, which checks the fields it read
   * before it builds one, so a document naming a currency this library does not hold or an amount
   * that is not a number is a decoding failure carrying that reason rather than a payment that
   * could not have been built in memory. Nothing further is checked here, because a payment adds
   * no invariant of its own to the two values it holds: a payload of the right shape always
   * yields a payment, and an encoded payment decodes back to one equal to it.
   *
   * @return the JSON decoding of a payment
   */
  implicit val decoder: Decoder[Payment] = deriveDecoder[Payment]
}
