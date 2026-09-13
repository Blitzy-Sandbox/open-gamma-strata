/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
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

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.Resolvable
import com.opengamma.strata.basics.date.AdjustableDate
import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * A single payment of a known amount on a date, with business day adjustment rules.
 *
 * This type represents a payment whose amount is known and whose date is known only as an
 * intention: an [[com.opengamma.strata.basics.date.AdjustableDate]], which pairs the date as
 * agreed with the rule for moving it off a weekend or a holiday. It is the form a cash flow
 * takes in a term sheet - "one thousand pounds on the thirtieth, following business day
 * convention over the London calendar" - and [[resolve]] turns it into the [[Payment]] that
 * says which day that actually is.
 *
 * ===The sign carries the direction===
 *
 * As in [[Payment]], there is no separate pay-or-receive flag: the sign of the amount is the
 * direction. A '''negative''' amount is money to be paid away and a '''positive''' amount money
 * to be received, so a collection of payments is summed without inspecting the direction of each
 * one. The factories [[AdjustablePayment.ofPay]] and [[AdjustablePayment.ofReceive]] exist for
 * callers that know the direction but hold an amount of unknown sign, and each ''normalises''
 * the sign rather than negating unconditionally - a distinction that is observable and is
 * documented on those factories.
 *
 * ===Construction is total, and failure is inherited rather than introduced===
 *
 * An adjustable payment is a pair of two values that are already valid, and it imposes no
 * further invariant of its own: any amount [[CurrencyAmount]] admits, on any adjustable date, is
 * an adjustable payment. The type is therefore a plain `case class` with a public constructor,
 * `apply` and `copy`, and building one cannot fail:
 *
 * {{{
 * val payment = AdjustablePayment(gbp1000, AdjustableDate.of(LocalDate.of(2015, 6, 30)))
 * val moved = payment.copy(date = AdjustableDate.of(LocalDate.of(2015, 7, 1)))
 * }}}
 *
 * Failure appears at exactly one door, and it is inherited: the three-argument
 * `AdjustablePayment.of` takes a raw `Double` and so has to build a [[CurrencyAmount]] first,
 * which refuses a value that is not a number, so those two factories return a
 * [[com.opengamma.strata.collect.FailureOr]]. Every other route in - the two-argument `of`,
 * `ofPay`, `ofReceive`, `of` from a [[Payment]], the constructor and `copy` - takes an amount
 * that has already been checked and is consequently total.
 *
 * The JSON codec published by the companion is the serialized form of an adjustable payment.
 *
 * ===Resolution, and why it is the only reference-data call here===
 *
 * [[resolve]] is the [[com.opengamma.strata.basics.Resolvable]] implementation of the type: it
 * adjusts the date against the reference data supplied and pairs the resulting fixed date with
 * this payment's amount, giving a [[Payment]]. It is one line long, and deliberately so - every
 * question about which calendar applies, what the convention does with a Sunday and what a
 * composite calendar identifier means belongs to
 * [[com.opengamma.strata.basics.date.AdjustableDate]] and the types beneath it, none of which
 * this type needs to know about.
 *
 * Resolution answers a value: the resolved payment is a `Right`, and a calendar the
 * reference data cannot supply is a
 * `Left(`[[com.opengamma.strata.collect.result.Failure.MissingData]]`)` naming the identifier.
 * [[com.opengamma.strata.basics.Resolvable.toReader]] is inherited and offers the same
 * resolution as a value awaiting its reference data, so several resolutions compose and the data
 * is supplied once:
 *
 * {{{
 * import cats.syntax.apply._
 *
 * val both = (firstPayment.toReader, secondPayment.toReader).tupled
 * val resolved = both.run(ReferenceData.standard)   // FailureOr[(Payment, Payment)]
 * }}}
 *
 * ===What this type deliberately does not do===
 *
 * There is no `convertedTo`, and this type is '''not''' an [[FxConvertible]]. An amount whose
 * payment date is still to be adjusted is not a sensible thing to convert, because the rate a
 * conversion should use generally depends on a date; a holder therefore resolves first and
 * converts the resulting [[Payment]], which is the convertible type:
 *
 * {{{
 * adjustablePayment.resolve(refData).flatMap(_.convertedTo(Currency.EUR, rateProvider))
 * }}}
 *
 * ===Equality, ordering and text===
 *
 * Equality is the case class's own, which compares the amount by the equality
 * [[CurrencyAmount]] defines - a bit-pattern comparison, under which an amount that is not a
 * number equals itself and a negative zero differs from a positive zero - and the date by the
 * equality [[com.opengamma.strata.basics.date.AdjustableDate]] defines. No bit-pattern
 * comparison is arranged here, deliberately: the only `Double` an adjustable payment holds is
 * inside the amount, and one source of truth for the equality of a double belongs with the type
 * that holds it. The companion publishes a `Hash` and a `Show`, and no `Order`, since whether an
 * amount or a date ranks first depends on what the caller is doing.
 *
 * Instances are immutable and every operation is a pure function of the instance and its
 * arguments, so an adjustable payment may be shared freely between threads.
 *
 * @param value  the amount of the payment, signed, negative to pay and positive to receive
 * @param date  the date on which the payment is made, which should normally adjust to a valid
 *   business day
 * @see [[Payment]] for the fixed-date payment this type resolves to, and for the conversion
 *   this type leaves to it
 * @see [[CurrencyAmount]] for the amount held and for what a valid amount is
 * @see [[com.opengamma.strata.basics.date.AdjustableDate]] for the date held and the adjustment
 *   it carries
 * @see [[com.opengamma.strata.basics.Resolvable]] for the resolution contract this type
 *   implements
 */
final case class AdjustablePayment(value: CurrencyAmount, date: AdjustableDate)
    extends Resolvable[Payment]
    with NoJavaSerialization {

  /**
   * Gets the currency of the payment.
   *
   * This is `value.currency`. The currency is a property of the amount rather than of the
   * payment, so changing it means converting - which this type leaves to the [[Payment]] that
   * [[resolve]] produces, as the class documentation explains.
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
   * Resolves the date on this payment, returning a payment with a fixed date.
   *
   * The adjustable date is adjusted against the reference data supplied, and the fixed date that
   * results is paired with this payment's amount to give a [[Payment]]. The amount is carried
   * across untouched - resolution settles '''when''' a payment is made and never how much it is.
   *
   * The reference data is consulted on every call, so the answer follows the data given rather
   * than any data captured earlier. The resolved payment, by contrast, is bound to the data it
   * was resolved against and does not follow later changes to it, which is the caveat
   * [[com.opengamma.strata.basics.Resolvable]] documents for every resolved form.
   *
   * Reference data that cannot supply the calendar the date's adjustment names yields
   * `Left(`[[com.opengamma.strata.collect.result.Failure.MissingData]]`)` naming the identifier.
   *
   * A date carrying [[com.opengamma.strata.basics.date.BusinessDayAdjustment.NONE]] moves no
   * date, but it is '''not''' exempt from that lookup: it names the no-holidays calendar, and the
   * adjustment resolves the identifier before applying a convention that does nothing with it.
   * The calendar is resolved unconditionally, and the consequence is worth stating, because it
   * is the one way a payment that adjusts nothing can still fail to resolve: the no-holidays
   * calendar is supplied by [[com.opengamma.strata.basics.ReferenceData.standard]], by
   * [[com.opengamma.strata.basics.ReferenceData.minimal]] and by every set built with
   * [[com.opengamma.strata.basics.ReferenceData.of]], which includes the minimal calendars, so
   * resolution succeeds against any of those; only genuinely empty data, which supplies nothing
   * at all, does not.
   *
   * {{{
   * val payment = AdjustablePayment.ofReceive(
   *   gbp1000,
   *   AdjustableDate.of(LocalDate.of(2015, 6, 28), BusinessDayAdjustment.of(FOLLOWING, GBLO)))
   *
   * payment.resolve(ReferenceData.standard)   // Right(Payment{value=GBP 1000, date=2015-06-29})
   * payment.resolve(ReferenceData.empty)      // Left(Failure.MissingData("... GBLO ..."))
   * }}}
   *
   * @param refData  the reference data to use, which supplies the holiday calendar the date's
   *   adjustment names
   * @return the payment with its date fixed, or the failure explaining which reference data was
   *   missing
   */
  override def resolve(refData: ReferenceData): Either[Failure, Payment] =
    date.adjusted(refData).map(adjusted => Payment.of(value, adjusted))

  /**
   * Returns a copy of this payment with the value negated.
   *
   * A payment to be made becomes one to be received and the other way about, on the same
   * adjustable date. The negation is that of [[CurrencyAmount.negated]], so it is unconditional -
   * applying it twice returns the amount it started from - and it is the operation to use where
   * the direction of a known payment is reversed. Where instead the direction is known and the
   * sign of the amount is not, use [[AdjustablePayment.ofPay]] or [[AdjustablePayment.ofReceive]].
   *
   * This instance is immutable and unaffected by this method.
   *
   * @return a payment based on this with the value negated
   */
  def negated: AdjustablePayment = AdjustablePayment.of(value.negated, date)

  /**
   * Returns this payment as text.
   *
   * The two fields are named in declaration order between braces, as in
   * `AdjustablePayment{value=GBP 1000, date=2015-06-30}`, with the amount rendered by
   * [[CurrencyAmount.toString]] and the date by
   * [[com.opengamma.strata.basics.date.AdjustableDate.toString]] - so a date needing no
   * adjustment renders as the bare date and any other renders as
   * `2015-06-28 adjusted by Following using calendar GBLO`. It is what the `Show` instance of
   * the companion renders.
   *
   * @return the rendering of this payment
   */
  override def toString: String = s"AdjustablePayment{value=$value, date=$date}"
}

/**
 * The factories, typeclass instances and JSON form of [[AdjustablePayment]].
 *
 * The nine factories form a grid: three intents - take the sign as given (`of`), force it
 * negative (`ofPay`), force it positive (`ofReceive`) - each in a fixed-date and an
 * adjustable-date form, plus the two `of` overloads that take a currency and a raw number, and
 * one that lifts a fixed [[Payment]]. Every fixed-date form is its adjustable-date counterpart
 * applied to [[com.opengamma.strata.basics.date.AdjustableDate.of]], so the two forms of one
 * intent cannot drift apart. The constructor and `copy` of the case class are public alongside
 * them, since an adjustable payment imposes no invariant of its own, so a caller holding an
 * amount and an adjustable date needs no factory at all.
 *
 * Two typeclass instances are published, and exactly two: a `Hash`, which is the single
 * equality-bearing instance of the type - `Hash` extends `Eq`, so declaring an `Eq` as well
 * would leave two instances that could disagree and one of them ambiguous - and a `Show`. There
 * is deliberately no `Order`, since whether an amount or a date ranks first depends on what the
 * caller is doing. A caller that needs one sorts by the field it means, as in
 * `payments.sortBy(_.date.unadjusted)`.
 */
object AdjustablePayment {

  /**
   * Obtains an instance representing an amount where the date is fixed.
   *
   * Whether the payment is to be paid or received is determined by the sign of the amount, which
   * is taken as given: a negative amount is a payment made and a positive amount one received.
   * The date is taken as it stands, carrying no business day adjustment, so [[resolve]] returns
   * the date given here rather than any neighbouring business day - though it still resolves the
   * no-holidays calendar, as [[resolve]] explains.
   *
   * This is one of the two factories that can fail, and it fails for one reason: the amount
   * arrives as a raw `Double` and has to become a [[CurrencyAmount]], which refuses a value that
   * is not a number. The failure is that of [[CurrencyAmount.of]], reported unchanged, so the
   * caller sees the same reason and wording whichever door it came through. Where an amount is
   * already in hand, the two-argument factories below are total.
   *
   * {{{
   * AdjustablePayment.of(Currency.GBP, 1000d, date)        // Right(AdjustablePayment{...})
   * AdjustablePayment.of(Currency.GBP, Double.NaN, date)   // Left(Failure.Invalid("... NaN"))
   * }}}
   *
   * @param currency  the currency of the payment
   * @param amount  the signed amount of the payment
   * @param date  the date on which the payment is made, taken as it stands
   * @return the adjustable payment, or the failure describing why the amount is not one this
   *   library holds
   */
  def of(currency: Currency, amount: Double, date: LocalDate): FailureOr[AdjustablePayment] =
    of(currency, amount, AdjustableDate.of(date))

  /**
   * Obtains an instance representing an amount where the date is adjustable.
   *
   * Whether the payment is to be paid or received is determined by the sign of the amount, which
   * is taken as given. The date carries its own business day adjustment, which [[resolve]]
   * applies.
   *
   * This factory can fail for the single reason described on the fixed-date overload above: the
   * amount arrives as a raw `Double` and has to become a [[CurrencyAmount]] first.
   *
   * @param currency  the currency of the payment
   * @param amount  the signed amount of the payment
   * @param date  the date on which the payment is made, with its adjustment
   * @return the adjustable payment, or the failure describing why the amount is not one this
   *   library holds
   */
  def of(currency: Currency, amount: Double, date: AdjustableDate): FailureOr[AdjustablePayment] =
    CurrencyAmount.of(currency, amount).map(checked => AdjustablePayment(checked, date))

  /**
   * Obtains an instance representing an amount where the date is fixed.
   *
   * Whether the payment is to be paid or received is determined by the sign of the amount, which
   * is taken as given. The date is taken as it stands, carrying no business day adjustment.
   *
   * Construction cannot fail: the amount has already been checked by the type that holds it, and
   * an adjustable payment adds no invariant to it.
   *
   * @param value  the signed amount of the payment
   * @param date  the date on which the payment is made, taken as it stands
   * @return the adjustable payment
   */
  def of(value: CurrencyAmount, date: LocalDate): AdjustablePayment =
    of(value, AdjustableDate.of(date))

  /**
   * Obtains an instance representing an amount where the date is adjustable.
   *
   * Whether the payment is to be paid or received is determined by the sign of the amount, which
   * is taken as given. The date carries its own business day adjustment, which [[resolve]]
   * applies.
   *
   * Construction cannot fail: this factory is exactly the constructor of the case class, offered
   * under the `of` name the other factories share.
   *
   * @param value  the signed amount of the payment
   * @param date  the date on which the payment is made, with its adjustment
   * @return the adjustable payment
   */
  def of(value: CurrencyAmount, date: AdjustableDate): AdjustablePayment =
    AdjustablePayment(value, date)

  /**
   * Obtains an instance representing an amount to be paid where the date is fixed.
   *
   * The sign of the amount is '''normalised''' to be negative, marking the payment as one made.
   * That is [[CurrencyAmount.negative]] and not [[CurrencyAmount.negated]], and the difference is
   * observable: an amount that is already negative, and a zero, are passed through unchanged,
   * whereas an unconditional negation would turn a negative amount back into a positive one. So
   * `ofPay` applied twice is `ofPay`, which is what makes it safe to use on an amount of unknown
   * sign - the use it exists for. Where the direction of a payment is to be ''reversed'' rather
   * than asserted, use [[AdjustablePayment.negated]].
   *
   * {{{
   * AdjustablePayment.ofPay(gbp1000, date).getAmount    // -1000.0
   * AdjustablePayment.ofPay(gbpM1000, date).getAmount   // -1000.0, unchanged
   * }}}
   *
   * @param value  the amount of the payment, of either sign
   * @param date  the date on which the payment is made, taken as it stands
   * @return the adjustable payment, holding an amount that is negative or zero
   */
  def ofPay(value: CurrencyAmount, date: LocalDate): AdjustablePayment =
    ofPay(value, AdjustableDate.of(date))

  /**
   * Obtains an instance representing an amount to be paid where the date is adjustable.
   *
   * The sign of the amount is '''normalised''' to be negative exactly as described on the
   * fixed-date overload above, so an amount that is already negative and a zero are passed
   * through unchanged. The date carries its own business day adjustment, which [[resolve]]
   * applies.
   *
   * @param value  the amount of the payment, of either sign
   * @param date  the date on which the payment is made, with its adjustment
   * @return the adjustable payment, holding an amount that is negative or zero
   */
  def ofPay(value: CurrencyAmount, date: AdjustableDate): AdjustablePayment =
    AdjustablePayment(value.negative, date)

  /**
   * Obtains an instance representing an amount to be received where the date is fixed.
   *
   * The sign of the amount is '''normalised''' to be positive, marking the payment as one
   * received. That is [[CurrencyAmount.positive]] and not a negation: an amount that is already
   * positive, and a zero, are passed through unchanged, and only a negative amount is turned
   * round. `ofReceive` applied twice is therefore `ofReceive`, and the note under
   * [[AdjustablePayment.ofPay]] about reversing a direction applies here in the same way.
   *
   * {{{
   * AdjustablePayment.ofReceive(gbpM1000, date).getAmount   // 1000.0
   * AdjustablePayment.ofReceive(gbp1000, date).getAmount    // 1000.0, unchanged
   * }}}
   *
   * @param value  the amount of the payment, of either sign
   * @param date  the date on which the payment is made, taken as it stands
   * @return the adjustable payment, holding an amount that is positive or zero
   */
  def ofReceive(value: CurrencyAmount, date: LocalDate): AdjustablePayment =
    ofReceive(value, AdjustableDate.of(date))

  /**
   * Obtains an instance representing an amount to be received where the date is adjustable.
   *
   * The sign of the amount is '''normalised''' to be positive exactly as described on the
   * fixed-date overload above, so an amount that is already positive and a zero are passed
   * through unchanged. The date carries its own business day adjustment, which [[resolve]]
   * applies.
   *
   * @param value  the amount of the payment, of either sign
   * @param date  the date on which the payment is made, with its adjustment
   * @return the adjustable payment, holding an amount that is positive or zero
   */
  def ofReceive(value: CurrencyAmount, date: AdjustableDate): AdjustablePayment =
    AdjustablePayment(value.positive, date)

  /**
   * Obtains an instance based on a [[Payment]].
   *
   * The amount and the date of the fixed payment are taken as they are, the date becoming an
   * adjustable date that carries no adjustment. Resolving the result recovers a payment equal to
   * the one given - the date is not moved - which is what makes this the way to hand a fixed
   * payment to an interface that speaks in adjustable payments:
   *
   * {{{
   * AdjustablePayment.of(payment).resolve(ReferenceData.standard) == Right(payment)
   * }}}
   *
   * The reference data still has to supply the no-holidays calendar that the absent adjustment
   * names, for the reason [[resolve]] gives; [[com.opengamma.strata.basics.ReferenceData.minimal]]
   * is the smallest set that does.
   *
   * The sign of the amount is taken as given, as in the fixed payment.
   *
   * @param payment  the fixed payment
   * @return the adjustable payment
   */
  def of(payment: Payment): AdjustablePayment = of(payment.value, payment.date)

  /**
   * The hashing and equality of adjustable payments.
   *
   * Taken from the `equals` and `hashCode` of the case class, which compare and mix the two
   * fields by their own: the amount by the bit-pattern equality [[CurrencyAmount]] defines, so
   * that an amount which is not a number equals itself and a negative zero is distinct from a
   * positive zero, and the date by the structural equality of
   * [[com.opengamma.strata.basics.date.AdjustableDate]]. Nothing about the hash depends on where
   * an instance sits in memory, so the hash of an adjustable payment is the same in every run of
   * every program.
   *
   * This is the type's only equality-bearing instance; `Eq[AdjustablePayment]` is obtained from
   * it by subtyping rather than declared separately. There is no `Order`, as the companion's own
   * documentation explains.
   *
   * @return the hashing of adjustable payments
   */
  implicit val hash: Hash[AdjustablePayment] = Hash.fromUniversalHashCode[AdjustablePayment]

  /**
   * The rendering of adjustable payments as text.
   *
   * Renders what [[AdjustablePayment.toString]] renders, the
   * `AdjustablePayment{value=GBP 1000, date=2015-06-30}` form, so the two ways of putting an
   * adjustable payment into a message agree.
   *
   * @return the rendering of an adjustable payment
   */
  implicit val show: Show[AdjustablePayment] = Show.show(_.toString)

  /**
   * The JSON encoding of adjustable payments.
   *
   * The encoding is derived when this file is compiled, so no part of it inspects a class while
   * the program runs. An adjustable payment encodes as an object holding its two fields under
   * their own names, in declaration order, each written by the codec its own type publishes -
   * the amount as the object of [[CurrencyAmount]], and the date as the object of
   * [[com.opengamma.strata.basics.date.AdjustableDate]], whose own two fields are the ISO-8601
   * date and the adjustment:
   *
   * {{{
   * {"value":{"currency":"GBP","amount":1000.0},
   *  "date":{"unadjusted":"2015-06-28","adjustment":{"convention":"Following","calendar":"GBLO"}}}
   * }}}
   *
   * An amount outside the real numbers is written by the amount's own codec as the tagged string
   * that codec defines - `"Infinity"` for an infinite payment - so every adjustable payment this
   * type admits survives a round trip. An adjustable payment holds both of its fields, the amount
   * and the date, so nothing is ever omitted here; the encoder is wrapped in the same policy
   * every product encoder of this library carries, which omits a field holding no value, so that
   * the rule holds of every product encoder alike.
   *
   * @return the JSON encoding of an adjustable payment
   */
  implicit val encoder: Encoder[AdjustablePayment] =
    Codecs.dropNulls(deriveEncoder[AdjustablePayment])

  /**
   * The JSON decoding of adjustable payments.
   *
   * This is the inverse of the encoding above and is likewise derived at compile time. Both
   * fields have to be present, and each is read by the decoder its own type publishes: a document
   * naming a currency this library does not hold, or an amount that is not a number, is a
   * decoding failure carrying that reason rather than a payment that could not have been built in
   * memory. A holiday calendar this library knows nothing about is accepted here and fails - if
   * at all - when the payment is resolved, which is where a missing calendar belongs.
   *
   * Nothing further is checked, because an adjustable payment adds no invariant of its own to the
   * two values it holds: a payload of the right shape always yields an adjustable payment, and an
   * encoded adjustable payment decodes back to one equal to it.
   *
   * @return the JSON decoding of an adjustable payment
   */
  implicit val decoder: Decoder[AdjustablePayment] = deriveDecoder[AdjustablePayment]
}
