/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import cats.Hash
import cats.Order
import cats.Show

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.DoubleArrayMath
import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * An amount of a currency.
 *
 * This type holds a `Double` amount together with the currency it is denominated in, so that
 * `GBP 12.34` is one value rather than a number and a currency a caller has to keep side by
 * side. It is deliberately named an amount and not money: it pairs a number with a currency and
 * nothing more, where a name like money would suggest it is suitable for accounting, which it is
 * not. That choice is made for performance - exact decimal arithmetic is markedly slower - and
 * the exact-decimal alternatives are [[Money]] and [[BigMoney]].
 *
 * A `Double` is a 64 bit binary floating point value, and binary floating point cannot represent
 * every decimal fraction: adding `0.1` and `0.2` gives `0.30000000000000004` rather than `0.3`.
 * The error is small, so this type is an appropriate way to carry the result of a calculation,
 * and an inappropriate way to carry a ledger balance.
 *
 * ===What an amount may hold===
 *
 * An amount holds any `Double` except a value that is not a number. The two infinities are
 * accepted, because a calculation that overflows still describes a direction, while a value that
 * is not a number describes nothing at all and would silently poison every amount it was added
 * to. A negative zero is accepted and normalised to a positive zero, so `-0.0` never survives
 * into a value of this type and two amounts that are both zero are always equal.
 *
 * ===The two ways in===
 *
 * [[CurrencyAmount.of]] is the public route and reports a rejected amount as a [[Failure]], so a
 * caller holding text, a document or a user's input decides what to do about a value this type
 * does not admit before it has one. The arithmetic below takes a second, internal route: it
 * normalises and checks exactly as `of` does, but its signature stays total, and a result that is
 * not a number - reachable only by combining infinities, as in `+∞ + −∞` - raises the invariant
 * through [[com.opengamma.strata.collect.ArgCheck]] rather than widening every arithmetic method
 * into a failure channel. That split is deliberate: the data-dependent failures of this type are
 * values, and the numeric edge that only an infinite operand can reach is an invariant.
 *
 * ===Which operations can fail===
 *
 * Adding or subtracting another amount fails when the two currencies differ, because the result
 * would have no currency; parsing fails when the text names no amount; and converting fails when
 * the rate the conversion needs is unavailable, or when a rate other than one is supplied for a
 * conversion into the currency the amount already has. Each of those is a property of the values
 * involved rather than of the calling code, so each is an `Either` and none of them abandons the
 * call stack. Everything else here is total.
 *
 * ===Equality and ordering===
 *
 * Two amounts are equal when their currencies are equal and their amounts have the same bit
 * pattern. That differs from the comparison a plain case class would have synthesised in two
 * places: a value that is not a number equals itself, and a negative zero differs from a positive
 * zero - the second being unreachable here, since construction normalises it away. Amounts order
 * by currency alphabetically and then by amount, and that ordering agrees with equality:
 * `compare` returns zero exactly when the two are equal.
 *
 * There is deliberately no `Monoid` or `Semigroup` for this type. Combining two amounts is
 * exactly what [[plus]] does, and it can fail, which a `Semigroup` has no way to report;
 * [[MultiCurrencyAmount]] is the type that adds amounts of any currencies and therefore the type
 * that carries the additive instance.
 *
 * ===Conversions to the exact-decimal types===
 *
 * [[toMoney]] rounds this amount to the minor units of its currency and [[toBigMoney]] keeps it
 * at scale twelve; `Money.of(amount)` and `BigMoney.of(amount)` are the same two conversions
 * reached from the other side, and [[Money.toCurrencyAmount]] and [[BigMoney.toCurrencyAmount]]
 * convert back. The three types therefore form one graph, and every edge of it can be travelled
 * in either direction.
 *
 * Both members here answer with an outcome, because of what this type admits: an amount may be
 * infinite, and no decimal is, so an infinite amount is not a money value. Reporting that keeps
 * the rejection visible in the signature instead of raising it from a conversion that reads as
 * an accessor.
 *
 * This type is immutable and thread-safe: a value of it can be shared freely, and every
 * operation returns a new value rather than changing the one it was called on.
 *
 * @param currency  the currency, which is `GBP` in the value `GBP 12.34`
 * @param amount  the amount of that currency, which is `12.34` in the value `GBP 12.34`, never
 *   not-a-number and never a negative zero
 * @see [[MultiCurrencyAmount]] for an amount in several currencies at once
 * @see [[Money]] and [[BigMoney]] for the exact-decimal alternatives
 * @see [[FxRateProvider]] for the source of the rates the conversions use
 */
sealed abstract case class CurrencyAmount private (currency: Currency, amount: Double)
    extends FxConvertible[CurrencyAmount]
    with NoJavaSerialization {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means can be
  // stopped is here. The single implementation is the companion's hidden `Impl`.
  JvmClosure.requireSoleImplementation(this, classOf[CurrencyAmount.Impl])

  // The invariant of this type, stated over the number the instance actually holds rather than
  // over the argument a factory was given, because the implementation class carries a public
  // constructor in the class file whatever the source asked for: a class compiled outside this
  // library can call it directly, and identity alone would then admit an amount holding a value
  // that is not a number - the one value this type refuses - or a negative zero, which every
  // route into the type normalises away. Both are what `CurrencyAmount.checkedAmount` establishes.
  JvmClosure.requireInvariant("its amount is a number", !amount.isNaN)
  JvmClosure.requireInvariant(
    "its amount is a positive zero where it is zero, a negative zero being normalised",
    // the two zeroes compare equal, so the bit pattern is what tells them apart: the pattern of a
    // positive zero is zero, and that of a negative zero is not
    amount != 0d || java.lang.Double.doubleToLongBits(amount) == 0L)

  /**
   * Returns a copy of this amount with the specified amount added.
   *
   * The addition is ordinary `Double` arithmetic, this amount plus the other.
   *
   * The two amounts have to be in the same currency, since an amount is a number ''of'' a
   * currency and there is no meaningful sum of amounts of different ones. A mismatch is reported
   * rather than thrown, because whether it happens depends on the values a caller holds:
   *
   * {{{
   * gbp100.plus(gbp50)   // Right(GBP 150)
   * gbp100.plus(usd50)   // Left(Failure.Invalid("Unable to add amounts in different currencies"))
   * }}}
   *
   * @param amountToAdd  the amount to add, in the same currency as this one
   * @return this amount with the other added, or the failure describing the currency mismatch
   * @throws java.lang.IllegalArgumentException if the sum is not a number, which requires the two
   *   amounts to be infinities of opposite sign
   */
  def plus(amountToAdd: CurrencyAmount): FailureOr[CurrencyAmount] =
    if (amountToAdd.currency == currency) {
      Right(plus(amountToAdd.amount))
    } else {
      Left(Failure.Invalid(CurrencyAmount.DifferentCurrenciesAddMessage))
    }

  /**
   * Returns a copy of this amount with the specified value added.
   *
   * The value is taken to be in the currency of this amount, so no currency can disagree and the
   * addition is total. The one numeric edge is the documented invariant of this type: adding an
   * infinity of the opposite sign to an infinite amount produces a value that is not a number,
   * which no amount may hold.
   *
   * @param amountToAdd  the value to add, in the currency of this amount
   * @return this amount with the value added
   * @throws java.lang.IllegalArgumentException if the sum is not a number, which requires this
   *   amount and the value to be infinities of opposite sign
   */
  def plus(amountToAdd: Double): CurrencyAmount =
    CurrencyAmount.create(currency, amount + amountToAdd)

  /**
   * Returns a copy of this amount with the specified amount subtracted.
   *
   * This is [[plus]] read in the other direction and behaves the same way in every respect: the
   * currencies have to agree, the subtraction is ordinary `Double` arithmetic - this amount minus
   * the other - and a difference that is not a number raises the invariant.
   *
   * @param amountToSubtract  the amount to subtract, in the same currency as this one
   * @return this amount with the other subtracted, or the failure describing the currency
   *   mismatch
   * @throws java.lang.IllegalArgumentException if the difference is not a number, which requires
   *   the two amounts to be infinities of the same sign
   */
  def minus(amountToSubtract: CurrencyAmount): FailureOr[CurrencyAmount] =
    if (amountToSubtract.currency == currency) {
      Right(minus(amountToSubtract.amount))
    } else {
      Left(Failure.Invalid(CurrencyAmount.DifferentCurrenciesSubtractMessage))
    }

  /**
   * Returns a copy of this amount with the specified value subtracted.
   *
   * The value is taken to be in the currency of this amount, so the subtraction is total, with
   * the same numeric edge [[plus]] documents.
   *
   * @param amountToSubtract  the value to subtract, in the currency of this amount
   * @return this amount with the value subtracted
   * @throws java.lang.IllegalArgumentException if the difference is not a number, which requires
   *   this amount and the value to be infinities of the same sign
   */
  def minus(amountToSubtract: Double): CurrencyAmount =
    CurrencyAmount.create(currency, amount - amountToSubtract)

  /**
   * Returns a copy of this amount with the amount multiplied by the specified value.
   *
   * The multiplication is written as the amount times the value, so a product that rounds
   * differently under the other order rounds as this order gives it.
   *
   * @param valueToMultiplyBy  the scalar value to multiply the amount by
   * @return this amount with its amount multiplied
   * @throws java.lang.IllegalArgumentException if the product is not a number, which requires an
   *   infinite amount and a zero multiplier or the reverse
   */
  def multipliedBy(valueToMultiplyBy: Double): CurrencyAmount =
    CurrencyAmount.create(currency, amount * valueToMultiplyBy)

  /**
   * Returns a copy of this amount with the specified operation applied to the amount.
   *
   * This is the general form of the arithmetic above, for an operation this type does not offer:
   *
   * {{{
   * base.mapAmount(value => if (value < 0) 0d else value * 3)
   * }}}
   *
   * The currency is carried through unchanged, since the operation is on the number alone.
   *
   * @param mapper  the operation to apply to the amount
   * @return this amount with the operation applied to its amount
   * @throws java.lang.IllegalArgumentException if the operation produces a value that is not a
   *   number
   */
  def mapAmount(mapper: Double => Double): CurrencyAmount =
    CurrencyAmount.create(currency, mapper(amount))

  /**
   * Checks whether the amount is zero.
   *
   * @return true if the amount is zero
   */
  def isZero: Boolean = amount == 0d

  /**
   * Checks whether the amount is greater than zero.
   *
   * Zero and negative amounts are not positive.
   *
   * @return true if the amount is greater than zero
   */
  def isPositive: Boolean = amount > 0d

  /**
   * Checks whether the amount is less than zero.
   *
   * Zero and positive amounts are not negative.
   *
   * @return true if the amount is less than zero
   */
  def isNegative: Boolean = amount < 0d

  /**
   * Returns a copy of this amount with the amount negated.
   *
   * Negating a zero amount gives a zero amount rather than a negative zero, because construction
   * normalises the sign of zero away.
   *
   * @return this amount with its amount negated
   */
  def negated: CurrencyAmount = CurrencyAmount.create(currency, -amount)

  /**
   * Returns a copy of this amount whose amount is not negative.
   *
   * The result is the absolute value of this amount, reached by negating a negative amount and
   * returning this amount itself otherwise.
   *
   * @return this amount with its amount made positive
   */
  def positive: CurrencyAmount = if (amount < 0d) negated else this

  /**
   * Returns a copy of this amount whose amount is not positive.
   *
   * The result is the negation of the absolute value of this amount, reached by negating a
   * positive amount and returning this amount itself otherwise.
   *
   * @return this amount with its amount made negative
   */
  def negative: CurrencyAmount = if (amount > 0d) negated else this

  /**
   * Converts this amount to the equivalent [[Money]].
   *
   * A money value holds its amount as a decimal at the minor units of its currency, so this
   * conversion rounds - half up, away from zero at a tie, at the width the currency quotes - and
   * loses precision for an amount finer than that. `AUD 100.125` becomes `AUD 100.13`, `BHD
   * 100.125` stays `BHD 100.125` because that currency quotes three digits, and `JPY 100.5`
   * becomes `JPY 101` because the yen quotes none.
   *
   * It answers with an outcome because this type admits amounts no decimal holds: the two
   * infinities, and any amount whose magnitude needs more than eighteen digits. Every other
   * amount converts, so a caller holding an amount that came from ordinary arithmetic reads the
   * `Right`.
   *
   * {{{
   * CurrencyAmount.of(Currency.AUD, 100.125d).flatMap(amount => amount.toMoney)   // AUD 100.13
   * }}}
   *
   * @return the equivalent money value rounded to the currency's minor units, or the failure
   *   describing why this amount is not one
   */
  def toMoney: FailureOr[Money] = Money.of(this)

  /**
   * Converts this amount to the equivalent [[BigMoney]].
   *
   * A `BigMoney` holds its amount as a decimal at scale twelve, which is finer than any currency
   * quotes, so this conversion keeps every digit of the amount that a decimal holds and rounds
   * only a fraction longer than twelve places.
   *
   * It answers with an outcome for the reason [[toMoney]] gives: an infinite amount, and one
   * needing more than eighteen digits, are values no decimal holds.
   *
   * @return the equivalent arbitrary-precision value, or the failure describing why this amount
   *   is not one
   */
  def toBigMoney: FailureOr[BigMoney] = BigMoney.of(this)

  /**
   * Converts this amount into the specified currency at the specified rate.
   *
   * The amount is multiplied by the rate, in that order, so `GBP 100` converted into `USD` at
   * `1.6` is `USD 160`.
   *
   * Converting into the currency this amount already has is the one case that needs a decision:
   * such a conversion requires no arithmetic, so a rate of one - within a tolerance of `1e-8` -
   * returns this amount unchanged, and any other rate is reported as a failure rather than
   * silently applied. That keeps a caller from scaling an amount by passing a rate for a
   * conversion that does not happen. A rate that is not a number, and an infinite rate, are both
   * reported as a failure rather than applied: under the comparison used neither is within any
   * finite tolerance of one, since the distance from one to either of them is not a finite
   * quantity and neither of them equals one.
   *
   * {{{
   * gbp100.convertedTo(Currency.USD, 1.6d)   // Right(USD 160)
   * gbp100.convertedTo(Currency.GBP, 1d)     // Right(GBP 100) - this amount, unchanged
   * gbp100.convertedTo(Currency.GBP, 2d)     // Left(Failure.Invalid("FX rate must be 1 …"))
   * }}}
   *
   * @param resultCurrency  the currency of the result
   * @param fxRate  the rate from the currency of this amount to the result currency
   * @return this amount expressed in the result currency, or the failure naming the broken
   *   condition: a conversion into the currency this amount already has requires a rate of one,
   *   compared within `1e-8`
   */
  def convertedTo(resultCurrency: Currency, fxRate: Double): FailureOr[CurrencyAmount] =
    if (currency == resultCurrency) {
      if (DoubleArrayMath.fuzzyEquals(fxRate, 1d, CurrencyAmount.NoConversionTolerance)) {
        Right(this)
      } else {
        Left(Failure.Invalid(CurrencyAmount.NonUnitRateMessage))
      }
    } else {
      CurrencyAmount.of(resultCurrency, amount * fxRate)
    }

  /**
   * Converts this amount into the specified currency, taking the rate from the specified provider.
   *
   * This is the [[FxConvertible]] implementation of this type. An amount already in the requested
   * currency is returned unchanged and the provider is not consulted, so such a conversion
   * succeeds even under a provider that supplies no rates at all - a single-currency amount needs
   * no rate for a conversion into its own currency. Otherwise the provider converts the amount,
   * and the failure it reports when it holds no rate for the pair is the failure of this
   * conversion.
   *
   * @param resultCurrency  the currency of the result
   * @param rateProvider  the provider of FX rates
   * @return this amount expressed in the result currency, or the failure the provider reported
   *   for the rate the conversion needed
   */
  override def convertedTo(
      resultCurrency: Currency,
      rateProvider: FxRateProvider): FailureOr[CurrencyAmount] =
    if (currency == resultCurrency) {
      Right(this)
    } else {
      rateProvider
        .convert(amount, currency, resultCurrency)
        .flatMap(converted => CurrencyAmount.of(resultCurrency, converted))
    }

  /**
   * Checks whether this amount equals another object.
   *
   * Another amount is equal when it holds the same currency and an amount with the same bit
   * pattern. The bit comparison differs from the comparison a case class would have synthesised
   * for two values: one that is not a number, which here equals itself, and a negative zero,
   * which here differs from a positive zero and which construction ensures no amount holds. An
   * object of any other type is not equal.
   *
   * @param obj  the object to compare to
   * @return true if the other object is an amount holding the same currency and the same amount
   */
  override def equals(obj: Any): Boolean = obj match {
    case other: CurrencyAmount =>
      (this eq other) ||
        (currency == other.currency && java.lang.Double.compare(amount, other.amount) == 0)
    case _ => false
  }

  /**
   * Returns a hash code consistent with [[equals]].
   *
   * The mixing is the hash of the currency, multiplied by the usual odd prime, plus the hash of
   * the amount, with the amount hashed by its bit pattern so that two amounts which [[equals]]
   * calls equal always agree here too. Every part of it is a function of the value alone, so the
   * hash of an amount is identical in every run of every program.
   *
   * @return the hash code of the currency and the amount held
   */
  override def hashCode: Int = currency.hashCode * 31 + java.lang.Double.hashCode(amount)

  /**
   * Returns the formatted string form of this amount.
   *
   * The form is the currency code, a space and the amount - `GBP 12.34` - which is the form
   * [[CurrencyAmount.parse]] reads back. An amount that is a whole number is written without a
   * fractional part, so `GBP 100` rather than `GBP 100.0`. The two values outside the real
   * numbers are written as the platform writes them, `Infinity` and `-Infinity`, and
   * [[CurrencyAmount.parse]] reads those two forms back as well, so every value an amount may
   * hold survives the round trip through its text.
   *
   * @return the formatted amount
   */
  override def toString: String = s"$currency ${CurrencyAmount.formatAmount(amount)}"
}

/**
 * Provides the ways of obtaining an amount, the route from text, and the instances for the type.
 *
 * This companion is the only place a [[CurrencyAmount]] is created. Every route into the type
 * either checks the amount and reports what it rejected, as [[of]] and [[parse]] do, or is
 * reached from an amount that already exists, as the arithmetic of the type is - and that
 * arithmetic stays total in signature, raising the documented invariant only for a result no
 * amount may hold. Neither the constructor nor a synthesised `apply` or `copy` is available, and
 * the type is sealed, so an amount that is not a number or a negative zero cannot be built.
 *
 * @see [[CurrencyAmount]] for the type itself and for what an amount may hold
 */
object CurrencyAmount {

  /**
   * The name of the amount field, as the argument name of the checks that reject it.
   *
   * This is the name that appears in `Argument 'amount' must not be NaN`, the message both routes
   * into the type report for a value that is not a number.
   */
  private val AmountField: String = "amount"

  /** Reported when two amounts of different currencies are added. */
  private val DifferentCurrenciesAddMessage: String =
    "Unable to add amounts in different currencies"

  /** Reported when two amounts of different currencies are subtracted. */
  private val DifferentCurrenciesSubtractMessage: String =
    "Unable to subtract amounts in different currencies"

  /** Reported when a rate other than one is supplied for a conversion that does not convert. */
  private val NonUnitRateMessage: String =
    "FX rate must be 1 when no conversion required"

  /** The tolerance within which a rate counts as one for a conversion that does not convert. */
  private val NoConversionTolerance: Double = 1e-8

  /**
   * The index of the space separating the currency code from the amount in the text form.
   *
   * The text form is three letters, a space and the amount, so the shortest text that can name an
   * amount is five characters long and the separator is always at this index. Both facts are read
   * by [[parse]], which is why they are named here rather than written into it twice.
   */
  private val SeparatorIndex: Int = 3

  /**
   * The longest the amount part of the text form may be.
   *
   * The amount is read as a double, and a double has no longest spelling in the way a period or
   * a currency code does - a caller may write any number of digits and the reading rounds them -
   * so, exactly as with an identifier, the only bound available is one this type states. Without
   * one, a sender chooses how much text [[parse]] copies and how much numeric reading it does.
   *
   * The number is chosen from what it takes to write a double exactly rather than from what a
   * machine can hold. The longest exact decimal spelling of a finite double is that of the
   * smallest subnormal, which needs 767 significant digits after a leading zero and a point, and
   * every other value needs fewer; a thousand characters is above all of them, so no spelling
   * that names a double exactly is refused for its size. What is refused is text carrying digits
   * that cannot change the value it names, which is the only thing beyond this bound.
   *
   * [[Money]] and [[BigMoney]] read the same text form and are bounded already, at the 256
   * characters their [[com.opengamma.strata.collect.Decimal]] reads within; this is looser
   * because a double spells values that a decimal of eighteen digits does not.
   */
  private val MaxAmountTextLength: Int = 1024

  /**
   * Obtains a zero amount in the specified currency.
   *
   * A zero amount is one this type always admits, so this is total.
   *
   * @param currency  the currency the amount is in
   * @return the zero amount in that currency
   */
  def zero(currency: Currency): CurrencyAmount = create(currency, 0d)

  /**
   * Obtains an amount in the specified currency.
   *
   * Every `Double` describes an amount except a value that is not a number, which this rejects,
   * and a negative zero, which it normalises to a positive zero. The two infinities are accepted.
   *
   * The failure has a single cause - there is one thing that can be wrong with the arguments - so
   * it is reported as one [[Failure]] rather than as a chain of them, and its message is the one
   * the argument check of this library reports for a value that is not a number.
   *
   * {{{
   * CurrencyAmount.of(Currency.GBP, 12.34d)                     // Right(GBP 12.34)
   * CurrencyAmount.of(Currency.GBP, -0d)                        // Right(GBP 0) - normalised
   * CurrencyAmount.of(Currency.GBP, Double.PositiveInfinity)    // Right(GBP Infinity)
   * CurrencyAmount.of(Currency.GBP, Double.NaN)                 // Left(Failure.Invalid(…))
   * }}}
   *
   * @param currency  the currency the amount is in
   * @param amount  the amount of that currency
   * @return the amount, or the failure describing why the value is not one
   */
  def of(currency: Currency, amount: Double): FailureOr[CurrencyAmount] =
    Validate
      .notNaN(amount, AmountField)
      .toEither
      .left
      .map(Failure.collapse)
      .map(checked => create(currency, checked))

  /**
   * Obtains an amount in the currency with the specified code.
   *
   * The code is resolved exactly, upper case and case sensitive, as [[Currency.of]] resolves it,
   * and then the amount is checked as above. Either step can fail, and the failure of the first
   * is the failure of the lookup.
   *
   * A code outside the closed set of currencies [[Currency]] holds is reported rather than
   * invented, so three upper case letters naming no currency of that set do not name an amount.
   *
   * @param currencyCode  the three letter ISO-4217 currency code, upper case
   * @param amount  the amount of that currency
   * @return the amount, or the failure describing why the code and value do not name one
   */
  def of(currencyCode: String, amount: Double): FailureOr[CurrencyAmount] =
    Currency.of(currencyCode).flatMap(currency => of(currency, amount))

  /**
   * Parses an amount from text of the form `AAA 12.34`.
   *
   * The parsed form is the three letter currency code, a space and the amount, which is the form
   * [[CurrencyAmount.toString]] writes. The shape of the text is checked before anything is read
   * from it, so the two wordings reported are:
   *
   *   - text too short to name an amount, or text whose fourth character is not a space, is
   *     `Unable to parse amount, invalid format: <text>`, and so is text whose amount part
   *     contains a space;
   *   - text of the right shape that nonetheless names no amount - because the code names no
   *     currency, because the digits do not form a number, or because the number is one this type
   *     rejects - is `Unable to parse amount: <text>`.
   *
   * {{{
   * CurrencyAmount.parse("AUD 100.001")   // Right(AUD 100.001)
   * CurrencyAmount.parse("USD -0")        // Right(USD 0) - normalised
   * CurrencyAmount.parse("AUD")           // Left - invalid format, too short
   * CurrencyAmount.parse("123")           // Left - invalid format, no separator
   * CurrencyAmount.parse("AUD aa")        // Left - right shape, names no number
   * CurrencyAmount.parse("GBP NaN")       // Left - names a number this type rejects
   * }}}
   *
   * The case of the currency code is tolerated, because the code is resolved through
   * [[Currency.parse]]. The cause of a rejection is deliberately not carried in the failure: the
   * message - which is what a caller reads, logs and asserts on - names only the text, and
   * keeping the failure to that one message keeps two failures over the same text equal and their
   * encoded form identical.
   *
   * Both wordings name the text as it was given. The text came from outside the library, so
   * bounding it and escaping what it may hold belong to the writing of a failure, which
   * [[com.opengamma.strata.collect.result.Failure.show]] and the text form of a failure perform
   * for every part they write.
   *
   * @param amountStr  the amount as text, in the form `AAA 12.34`
   * @return the amount the text names, or the failure naming the broken condition: the text has
   *   to be a currency code, a space and an amount this type admits
   */
  def parse(amountStr: String): FailureOr[CurrencyAmount] =
    // Every decision about the shape and the size of the text is taken from its length and from
    // the position of a separator within it, and all of them are taken before either part is cut
    // out of it. The order matters: a second separator used to be looked for in the amount part
    // AFTER that part had been copied out, so text shaped as a well-formed prefix followed by a
    // tail of a sender's choosing was copied in full only to be discarded by the very next
    // comparison, and a tail that carried no second separator went on to be read as a number at
    // whatever length it arrived (CWE-400/CWE-770).
    if (amountStr.length <= SeparatorIndex + 1 || amountStr.charAt(SeparatorIndex) != ' ') {
      Left(invalidFormat(amountStr))
    } else if (amountStr.indexOf(' ', SeparatorIndex + 1) >= 0) {
      // a second separator is the shape failure it always was, decided now from an index scan
      // rather than from a copy of the part it was found in
      Left(invalidFormat(amountStr))
    } else if (amountStr.length - (SeparatorIndex + 1) > MaxAmountTextLength) {
      // Text longer than any number can be written with names no amount, which is exactly what
      // the wording below says, so the ceiling reports through it rather than through a wording
      // of its own: what reaches a caller for an oversized amount is what has always reached it
      // for an unreadable one. The text is not read and neither part is copied.
      Left(Failure.Parsing(s"Unable to parse amount: $amountStr"))
    } else {
      val currencyCode = amountStr.substring(0, SeparatorIndex)
      val amountText = amountStr.substring(SeparatorIndex + 1)
      val parsed: Option[CurrencyAmount] = for {
        currency <- Currency.parse(currencyCode).toOption
        parsedAmount <- amountText.toDoubleOption
        value <- of(currency, parsedAmount).toOption
      } yield value
      // the text is rendered rather than interpolated as it stands, which bounds the message
      // and keeps it to one line while leaving an in-bound spelling quoted as it was given
      parsed.toRight(Failure.Parsing(s"Unable to parse amount: $amountStr"))
    }

  /**
   * Creates an amount, normalising it and checking the invariant, which every route funnels
   * through.
   *
   * This is the only instantiation of the type, so the routes above are the only way into it from
   * outside this package. It is what keeps the arithmetic of the type total in signature while the
   * invariant still holds: a caller that adds two ordinary amounts cannot reach the check, and a
   * caller that combines infinities reaches it and is told so.
   *
   * ===Why the currency package may call it===
   *
   * It is visible to the currency package rather than to this file alone because the types of
   * this package that hold amounts as numbers - [[CurrencyAmountArray]],
   * [[MultiCurrencyAmount]], [[MultiCurrencyAmountArray]], [[Money]] and [[BigMoney]] - have to
   * turn a number they hold back into an amount. Reaching that through the zero amount of the
   * currency would allocate a zero amount, a function that closes over the number and then the
   * amount itself, three objects where the one returned is the only one the caller keeps, and a
   * run of a hundred thousand values would pay that for each of them. Calling this directly
   * performs the very same normalisation and the very same check - there is no second definition
   * of what an amount is - and allocates exactly the object it returns.
   *
   * The contract a caller inside this package takes on is only that the number is a number it
   * means as an amount: this method still normalises the sign of zero and still raises the
   * invariant on a value that is not a number, so a caller cannot smuggle a state past it. The
   * public factories remain the only way in from anywhere else, and this is not part of the
   * published API of the module.
   *
   * @param currency  the currency
   * @param amount  the amount, normalised and checked here
   * @return the amount
   * @throws java.lang.IllegalArgumentException if the amount is not a number
   */
  private[currency] def create(currency: Currency, amount: Double): CurrencyAmount =
    new Impl(currency, checkedAmount(amount))

  /**
   * The one implementation of an amount.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it. It is declared rather than written as an anonymous subclass at the instantiation site
   * for two reasons, both about what the class file says: a private member class is one a Java
   * compiler refuses to name, where an anonymous class is public and can be instantiated directly
   * by a caller in another language, and a named class can be compared against, which is what
   * lets [[CurrencyAmount]] refuse in its own constructor to be any other implementation.
   *
   * @param currency  the currency
   * @param amount  the amount, already normalised and checked by [[create]]
   */
  private final class Impl(currency: Currency, amount: Double)
      extends CurrencyAmount(currency, amount)

  /**
   * Normalises a number and checks that it is an amount, answering with the number itself.
   *
   * This is the invariant of the type with the type taken away: it is the very computation
   * [[create]] performs, and [[create]] is written in terms of it, so what an amount may hold is
   * decided in exactly one place in this library and cannot drift between the two.
   *
   * ===Why a number-level form exists===
   *
   * The types of this package that hold amounts as numbers rather than as objects -
   * [[MultiCurrencyAmount]] most of all, whose representation is a map of currency to number -
   * have to apply this invariant to a number they are about to store, and the number is all they
   * want back. Reaching it through [[create]] means building an amount and reading its number
   * again, an object allocated and discarded per entry, which for an aggregation over `E` entries
   * is `E` objects that exist only to be unwrapped. An aggregation is instead one pass that
   * allocates the value it returns, and this is what lets it be that while keeping the check
   * where it belongs.
   *
   * A caller inside this package therefore uses this where it holds a number and [[create]] where
   * it holds an amount; neither is reachable from outside the package, so the public factories
   * remain the only way in from anywhere else.
   *
   * @param amount  the number to normalise and check
   * @return the number, with a negative zero normalised to a positive zero
   * @throws java.lang.IllegalArgumentException if the number is not a number
   */
  private[currency] def checkedAmount(amount: Double): Double = {
    // Adding a positive zero is the whole of the normalisation: `-0.0 + 0.0` is `0.0` while every
    // other value, the infinities included, is left exactly as it was. The addition looks odd
    // because the arithmetic identity is itself the mechanism that removes the sign of zero.
    val normalised = amount + 0d
    ArgCheck.notNaN(normalised, AmountField)
    normalised
  }

  /**
   * Creates an amount for a caller inside this package that already holds a value the type
   * admits.
   *
   * This is [[create]] under a name the rest of the package can reach, and it exists so that a
   * conversion whose answer always exists allocates the amount it returns and nothing else.
   * [[Money.toCurrencyAmount]] and [[BigMoney.toCurrencyAmount]] are its callers: the amount of a
   * money value is a decimal, whose `Double` is finite by construction, so neither has anything
   * to report and neither should build a zero amount on the way to the one it returns.
   *
   * It is `private[currency]` rather than public because it names no failure channel: a caller
   * outside the package has no way to know that the value it holds is one this type admits, and
   * [[of]] is the route that tells it. The invariant is not weakened by it - the value still
   * passes through [[create]], so a negative zero is normalised and a value that is not a number
   * raises the documented invariant exactly as every other route into the type does.
   *
   * @param currency  the currency
   * @param amount  the amount, which the caller has already established is one the type admits
   * @return the amount
   * @throws java.lang.IllegalArgumentException if the amount is not a number, which a caller
   *   holding a decimal cannot reach
   */
  private[currency] def ofTrusted(currency: Currency, amount: Double): CurrencyAmount =
    create(currency, amount)

  /**
   * The failure reported for text whose shape does not admit an amount.
   *
   * The text is quoted as it stands; bounding it and escaping what it may hold belong to the
   * writing of a failure, which the text form of one and [[Failure.show]] perform for every part
   * they write.
   */
  private def invalidFormat(amountStr: String): Failure =
    Failure.Parsing(s"Unable to parse amount, invalid format: $amountStr")

  /**
   * Renders an amount without a fractional part when it is a whole number.
   *
   * A whole number is converted to a `Long` before it is rendered, which is why `GBP 100` is
   * written rather than `GBP 100.0`. The conversion saturates rather than wrapping, so a finite
   * whole number beyond the range of a `Long` renders as that range's end: an amount of `1e20` is
   * written as `9223372036854775807`. An infinite amount is not a whole number by the test above
   * and so takes the other branch, where the platform writes it as `Infinity` or `-Infinity`.
   *
   * @param amount  the amount to render
   * @return the amount as text, without a fractional part when it is a whole number
   */
  private def formatAmount(amount: Double): String =
    if (DoubleArrayMath.isMathematicalInteger(amount)) amount.toLong.toString else amount.toString

  /**
   * The ordering, hashing and equality of amounts.
   *
   * Amounts order by currency alphabetically and then by amount. The amounts are compared by bit
   * pattern, the comparison [[CurrencyAmount.equals]] performs, so the ordering agrees with
   * equality exactly: `compare` returns zero precisely when `eqv` holds, for every value including
   * the ones outside the real numbers. No secondary comparison is needed, since the two fields of
   * the type are exactly the two the comparison reads.
   *
   * This is the only equality-bearing instance of the type. `Order` and `Hash` both extend `Eq`,
   * so a separate `Eq` would be a second answer to the same question; one is declared here and
   * `Eq` is obtained from it by subtyping.
   *
   * @return the ordering and hashing of amounts
   */
  implicit val order: Order[CurrencyAmount] with Hash[CurrencyAmount] =
    new Order[CurrencyAmount] with Hash[CurrencyAmount] {

      private val universal: Hash[CurrencyAmount] = Hash.fromUniversalHashCode[CurrencyAmount]

      override def compare(x: CurrencyAmount, y: CurrencyAmount): Int = {
        val byCurrency = Currency.order.compare(x.currency, y.currency)
        if (byCurrency != 0) byCurrency else java.lang.Double.compare(x.amount, y.amount)
      }

      override def eqv(x: CurrencyAmount, y: CurrencyAmount): Boolean = universal.eqv(x, y)

      override def hash(x: CurrencyAmount): Int = universal.hash(x)
    }

  /**
   * The rendering of amounts as text.
   *
   * Renders what [[CurrencyAmount.toString]] renders, the `GBP 12.34` form that [[parse]] reads
   * back, so the text of an amount is the same however it reaches a message.
   *
   * @return the rendering of an amount
   */
  implicit val show: Show[CurrencyAmount] = Show.show(_.toString)

  // The codec of the double field, brought into scope for the two derivations below and for
  // nothing else: the amount is written under the single policy for a double, which represents
  // the values JSON cannot express as the tagged strings `"NaN"`, `"Infinity"` and `"-Infinity"`.
  // Importing it here is what keeps that choice deliberate and local.
  import Codecs.implicits._

  /**
   * The raw field shape both codecs of this type are derived over.
   *
   * A checked type cannot be derived over directly: its constructor is not public, and decoding
   * it is two steps - read the fields, then hand them to the factory that decides whether they
   * describe a value. This product is the shape those fields have, and it is what makes the
   * derivation possible in both directions: the decoder below derives over it and then checks,
   * and the encoder below derives over it and is contramapped from an amount that is already
   * valid. It exists only for that purpose - it is private and it is never returned, so no caller
   * can hold an unchecked pair. Its field names are the JSON keys, and they are the names of the
   * two fields of [[CurrencyAmount]] itself, which keeps the derived shape and the type from
   * drifting apart.
   *
   * @param currency  the currency, read from its three letter code
   * @param amount  the amount, unchecked, read as a number or as one of the three tagged strings
   */
  private final case class Raw(currency: Currency, amount: Double) extends NoJavaSerialization

  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /**
   * The derived encoder of the raw field shape, used by the encoder below.
   *
   * This sits after the import above deliberately: the derivation picks up the tagged double
   * codec from that import, which is what writes an infinite amount as its tagged string. A
   * derivation site without that import in scope would take circe's plain numeric encoder
   * instead, which has no representation for a value JSON cannot express.
   */
  private val rawEncoder: Encoder[Raw] = deriveEncoder[Raw]

  /**
   * The JSON encoding of amounts.
   *
   * An amount is an object of two fields, the currency as its code and the amount as a number:
   *
   * {{{
   * {"currency":"GBP","amount":100.0}
   * }}}
   *
   * The shape is derived at compile time over the raw product above rather than written out
   * field by field, and the value is contramapped into that product: an amount in memory has
   * already been checked, so nothing further has to be decided on the way out. Deriving it is
   * what keeps the document and the type in step - a field added to one is a field added to the
   * other, with no second list of names to maintain. An infinite amount is written as the tagged
   * string the double policy defines, so every value this type admits survives a round trip. The
   * result is wrapped so that a field holding no value is omitted; this type has no optional
   * field, so the wrapping changes nothing about its output and keeps it inside that one policy.
   *
   * @return the JSON encoding of an amount
   */
  implicit val encoder: Encoder[CurrencyAmount] =
    Codecs.dropNulls(
      rawEncoder.contramap[CurrencyAmount](value => Raw(value.currency, value.amount)))

  /**
   * The JSON decoding of amounts.
   *
   * This is the inverse of the encoding above, and it decides whether the fields describe an
   * amount exactly as a caller's arguments are decided: the payload is read into the raw shape
   * and handed to `of`, so a document naming a code that is not one of the currencies
   * [[Currency]] holds, or an amount that is not a number, is a decoding failure carrying the
   * reason rather than a value this type would not have built.
   *
   * @return the JSON decoding of an amount
   */
  implicit val decoder: Decoder[CurrencyAmount] =
    Codecs.checkedDecoder[Raw, CurrencyAmount](raw => of(raw.currency, raw.amount))(rawDecoder)
}
