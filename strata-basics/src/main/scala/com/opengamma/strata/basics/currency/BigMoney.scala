/*
 * Copyright (C) 2021 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.math.BigDecimal
import java.math.RoundingMode

import cats.Hash
import cats.Order
import cats.Show

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.DoubleArrayMath
import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * An amount of a currency, held exactly to at most twelve decimal places.
 *
 * This is the high-precision sibling of [[Money]]. The two types hold the same pair of fields - a
 * [[Currency]] and an exact [[Decimal]] amount - and differ in what they round that amount to:
 * money is rounded to the minor units the currency quotes, so a sterling balance is two decimal
 * places, while this type keeps twelve regardless of the currency. An amount of `GBP
 * 1.000009` is therefore a money value of `GBP 1.00` and a value of this type of `GBP 1.000009`,
 * which is why an interest accrual, a per-unit price or an unrounded intermediate result belongs
 * here and a balance belongs in [[Money]].
 *
 * ===Twelve decimal places is the invariant, and it is applied everywhere===
 *
 * Every route into this type rounds to twelve decimal places, half up: the factories, and equally
 * the arithmetic, because the implementation being ported reached its private constructor for
 * both and that constructor rounded. So a value read from text naming more digits is shortened -
 * `AUD 1.123456789012345` is `AUD 1.123456789012` - and a product, a sum or a mapped amount that
 * lands on a finer digit is shortened in the same way. The invariant that follows is worth
 * stating on its own: '''the amount of a value of this type never has more than twelve decimal
 * places''' - and it is the one property [[Money]] does not share, since money is narrower still.
 *
 * The twelve places are an upper bound rather than an exact width, because [[Decimal]] normalises
 * away a trailing fractional zero: the amount inside `RON 200.2345` is the decimal `200.2345` at
 * scale four, not a decimal padded to twelve. That is invisible in the text form, where
 * [[toString]] pads to at least the width the currency quotes, and it is why `AUD 100` and `AUD
 * 100.00` name one value rather than two.
 *
 * Rounding to twelve places cannot itself fail: the decimal type holds up to eighteen, so twelve
 * is always available to it. What the decimal type does reject is a number it cannot hold at all -
 * one that is not finite, or one whose significant digits exceed eighteen - and that rejection is
 * what makes three of the five factories below answer with an outcome.
 *
 * ===Which operations can fail===
 *
 * Adding or subtracting another value fails when the two currencies differ, because the result
 * would have no currency, and comparing two values fails for the same reason - the implementation
 * being ported refused all five of those cases. Parsing fails when the text names no value;
 * converting fails when the rate the conversion needs is unavailable, and when a rate other than
 * one is supplied for a conversion into the currency the value already has. Building a value from
 * a `Double`, a `BigDecimal` or a [[CurrencyAmount]] fails when the number is one no decimal
 * holds. Each of those depends on the values involved rather than on the calling code, so each is
 * an `Either` and none of them abandons the call stack.
 *
 * The arithmetic, by contrast, is total in signature, exactly as it was in the implementation
 * being ported. Its one edge belongs to [[Decimal]] rather than to this type: a result beyond
 * eighteen digits is a broken precondition and is raised by the argument checks of
 * `strata-collect`, not reported as a failure of this type. [[roundToScale]] is total for the same
 * reason and with the same caveat, described where it is declared.
 *
 * ===Equality and ordering===
 *
 * Two values are equal when their currencies and their amounts are equal, which is the comparison
 * the implementation being ported performed. No bit-level special case is needed here - the reason
 * [[CurrencyAmount]] needs one is that it holds a `Double`, where a value that is not a number
 * would otherwise differ from itself; an amount held as a [[Decimal]] is normalised and compares
 * as a number, so the equality synthesised from the two fields is already the right one. Values
 * order by currency alphabetically and then by amount, as they did in the implementation being
 * ported, and that ordering agrees with equality: `compare` returns zero exactly when the two
 * values are equal.
 *
 * Ordering through the instance below is one thing and the five failable comparisons are another.
 * `Order` answers for any two values, including two in different currencies, because a total order
 * is what sorting a mixed collection needs; the predicates [[isGreaterThan]] and its three
 * companions refuse a mixed pair, because asking whether one sum of money exceeds another in a
 * different currency has no answer without a rate. Both behaviours are the ones the implementation
 * being ported had, and the choice between them is the caller's.
 *
 * ===Conversions to the neighbouring types===
 *
 * [[BigMoney.of]] accepts a [[CurrencyAmount]] and a [[Money]], and [[toCurrencyAmount]] and
 * [[toMoney]] convert back, so this type is one step from both of its neighbours in both
 * directions. Widening a money value loses nothing, since the minor units of every currency of
 * the reference data are within twelve places; narrowing through [[toMoney]] rounds, and rounds
 * half up, so it is a genuine conversion rather than a reinterpretation of the same digits.
 * [[CurrencyAmount.toBigMoney]] and [[Money.toBigMoney]] are the two widenings named from the
 * other side and produce the same values as the factories here.
 *
 * This type is immutable and thread-safe: a value of it can be shared freely, and every operation
 * returns a new value rather than changing the one it was called on.
 *
 * @param currency  the currency, which is `GBP` in the value `GBP 12.34`
 * @param amount  the amount of that currency, always at twelve decimal places or fewer, which is
 *   `12.34` in the value `GBP 12.34`
 * @see [[Money]] for the amount rounded to the currency's minor units
 * @see [[CurrencyAmount]] for the `Double`-valued amount this type holds exactly
 * @see [[FxRateProvider]] for the source of the rates the conversions use
 */
sealed abstract case class BigMoney private (currency: Currency, amount: Decimal)
    extends FxConvertible[BigMoney] {

  //-------------------------------------------------------------------------
  /**
   * Gets the amount as a `BigDecimal` of at least the currency's scale.
   *
   * The value returned carries the digits of the amount, padded to the number of minor unit
   * digits of the currency when the amount has fewer: `AUD 100` converts to a `BigDecimal`
   * reading `100.00` and `AUD 100.1249` to one reading `100.1249`, since four digits is already
   * more than the two sterling and the Australian dollar quote. Nothing is ever dropped, so this
   * is total - padding a scale upwards always succeeds.
   *
   * The implementation being ported deprecated this accessor in favour of [[getValue]], which
   * carries the amount without encoding a presentation width in it. It is kept, with its name,
   * because callers of the port read the same documentation and expect the same members; new code
   * should prefer [[getValue]] or the [[amount]] the value holds. It is not marked deprecated in
   * Scala: this build turns every warning into an error, so the annotation would make each
   * ordinary use of a member the implementation being ported still published fail to compile.
   *
   * @return the amount, with a scale of at least the currency's minor unit digits and at most
   *   twelve
   */
  def getAmount: BigDecimal = {
    val decimal = amount.toBigDecimal
    val digits = currency.minorUnitDigits
    if (decimal.scale < digits) decimal.setScale(digits) else decimal
  }

  /**
   * Gets the numeric amount of the value.
   *
   * This is the amount as it is held, so `BHD 1.23456` answers the decimal `1.23456` rather than
   * a value padded or narrowed to the three digits that currency quotes. It is where this type
   * and [[Money]] visibly diverge: money answers with its amount paired with the currency's
   * scale, because that scale is a property of every money value, while an amount of at most
   * twelve places has no single presentation width to carry and is returned on its own.
   *
   * @return the amount, at twelve decimal places or fewer
   */
  def getValue: Decimal = amount

  //-------------------------------------------------------------------------
  /**
   * Returns a copy of this value with the specified amount added.
   *
   * The two values have to be in the same currency, since this is an amount ''of'' a currency and
   * there is no meaningful sum of amounts of different ones. A mismatch is reported rather than
   * thrown, because whether it happens depends on the values a caller holds:
   *
   * {{{
   * ron200.plus(ron100)   // Right(RON 300.3594)
   * ron200.plus(aud100)   // Left(Failure.Invalid("Unable to add amounts in different currencies"))
   * }}}
   *
   * The sum is rounded to twelve decimal places, as every route into this type is, because the
   * implementation being ported reached its rounding constructor here too.
   *
   * @param amountToAdd  the amount to add, in the same currency as this value
   * @return this value with the other added, or the failure describing the currency mismatch
   * @throws java.lang.IllegalArgumentException if the sum needs more than eighteen digits, which
   *   is the documented arithmetic precondition of [[Decimal]]
   */
  def plus(amountToAdd: BigMoney): FailureOr[BigMoney] =
    if (amountToAdd.currency == currency) {
      Right(BigMoney.create(currency, amount.plus(amountToAdd.amount)))
    } else {
      Left(Failure.Invalid(BigMoney.DifferentCurrenciesAddMessage))
    }

  /**
   * Returns a copy of this value with the specified amount subtracted.
   *
   * This is [[plus]] read in the other direction and behaves the same way in every respect: the
   * currencies have to agree, the difference is exact decimal arithmetic, and the result is
   * rounded to twelve decimal places.
   *
   * @param amountToSubtract  the amount to subtract, in the same currency as this value
   * @return this value with the other subtracted, or the failure describing the currency mismatch
   * @throws java.lang.IllegalArgumentException if the difference needs more than eighteen digits,
   *   which is the documented arithmetic precondition of [[Decimal]]
   */
  def minus(amountToSubtract: BigMoney): FailureOr[BigMoney] =
    if (amountToSubtract.currency == currency) {
      Right(BigMoney.create(currency, amount.minus(amountToSubtract.amount)))
    } else {
      Left(Failure.Invalid(BigMoney.DifferentCurrenciesSubtractMessage))
    }

  //-------------------------------------------------------------------------
  /**
   * Returns a copy of this value with the amount multiplied by the specified whole number.
   *
   * The multiplier is a whole number because that is what the implementation being ported took.
   * Scaling by a fraction is [[map]] over the amount, or [[convertedTo]] where the fraction is an
   * exchange rate.
   *
   * A whole-number literal needs no suffix, since it is typed as the `Long` the parameter asks
   * for; an `Int` ''value'' has to be converted explicitly, because this build treats an implicit
   * numeric widening as an error. So `value.multipliedBy(3)` compiles and
   * `value.multipliedBy(count)` for an `Int` count is written `value.multipliedBy(count.toLong)`.
   *
   * @param valueToMultiplyBy  the whole number to multiply the amount by
   * @return this value with its amount multiplied
   * @throws java.lang.IllegalArgumentException if the product needs more than eighteen digits,
   *   which is the documented arithmetic precondition of [[Decimal]]
   */
  def multipliedBy(valueToMultiplyBy: Long): BigMoney =
    BigMoney.create(currency, amount.multipliedBy(valueToMultiplyBy))

  /**
   * Returns a copy of this value with the amount replaced by the result of a function.
   *
   * This is how the arithmetic of [[Decimal]] is reached, so that an operation this type does not
   * publish is still a single step:
   *
   * {{{
   * val doubled = value.map(_.multipliedBy(2L))
   * val absolute = value.map(_.abs)
   * }}}
   *
   * The result is rounded to twelve decimal places, so applying a function of arbitrary precision
   * cannot produce a value that breaks the invariant of the type - a third of `GBP 10` is `GBP
   * 3.333333333333` and not a figure with an unbounded fraction.
   *
   * @param mapper  the function to apply to the amount
   * @return this value with the function applied to its amount and the result rounded
   */
  def map(mapper: Decimal => Decimal): BigMoney = BigMoney.create(currency, mapper(amount))

  /**
   * Returns a copy of this value with the amount replaced by the result of a function on
   * `BigDecimal`.
   *
   * This is [[map]] with the amount presented as a `BigDecimal`, and unlike [[map]] it answers
   * with an outcome: a function of arbitrary precision can return a number no decimal holds - one
   * that is not finite, or one whose whole part needs more than eighteen digits - and that is a
   * property of the function and the value rather than of the calling code. The implementation
   * being ported raised in that case, from the decimal conversion this delegates to.
   *
   * The implementation being ported deprecated this in favour of [[map]], whose function works on
   * the decimal directly. It is kept, with its name, for the reason [[getAmount]] gives, and for
   * the same reason it carries no deprecation annotation.
   *
   * @param mapper  the function to apply to the amount, as a `BigDecimal`
   * @return this value with the function applied and the result rounded, or the failure describing
   *   why the result is not an amount
   */
  def mapAmount(mapper: BigDecimal => BigDecimal): FailureOr[BigMoney] =
    amount.mapAsBigDecimal(mapper).map(mapped => BigMoney.create(currency, mapped))

  //-------------------------------------------------------------------------
  /**
   * Checks whether this value is greater than another.
   *
   * The two values have to be in the same currency. The implementation being ported refused a
   * mixed pair here rather than comparing the bare numbers, and refusing is the right answer:
   * `GBP 1` and `USD 1` stand in no order without an exchange rate, and a comparison that
   * silently ignored the currencies would report one as the greater on the strength of its digits
   * alone. The refusal is reported rather than thrown, because whether it happens depends on the
   * values a caller holds - which is the same reason [[plus]] reports it.
   *
   * {{{
   * gbp1_000009.isGreaterThan(gbp1)   // Right(true)
   * gbp1.isGreaterThan(gbp1)          // Right(false)
   * gbp1.isGreaterThan(usd1)          // Left(Failure.Invalid("Unable to compare amounts in different currencies"))
   * }}}
   *
   * Sorting a collection that mixes currencies is a different question and has a different
   * answer: the [[BigMoney.order]] instance orders any two values, by currency and then by amount.
   *
   * @param otherAmount  the value to compare to
   * @return whether this amount is the greater, or the failure describing the currency mismatch
   */
  def isGreaterThan(otherAmount: BigMoney): FailureOr[Boolean] =
    comparedWith(otherAmount)(_.isGreaterThan(_))

  /**
   * Checks whether this value is greater than or equal to another.
   *
   * Two equal amounts satisfy this, which is the difference from [[isGreaterThan]]; the currencies
   * have to agree exactly as they do there, and a mismatch is reported the same way.
   *
   * @param otherAmount  the value to compare to
   * @return whether this amount is the greater or the two are equal, or the failure describing the
   *   currency mismatch
   */
  def isGreaterThanEqualTo(otherAmount: BigMoney): FailureOr[Boolean] =
    comparedWith(otherAmount)(_.isGreaterThanEqualTo(_))

  /**
   * Checks whether this value is less than another.
   *
   * This is [[isGreaterThan]] read in the other direction, with the same requirement that the
   * currencies agree and the same failure when they do not.
   *
   * @param otherAmount  the value to compare to
   * @return whether this amount is the smaller, or the failure describing the currency mismatch
   */
  def isLessThan(otherAmount: BigMoney): FailureOr[Boolean] =
    comparedWith(otherAmount)(_.isLessThan(_))

  /**
   * Checks whether this value is less than or equal to another.
   *
   * Two equal amounts satisfy this, which is the difference from [[isLessThan]]; the currencies
   * have to agree exactly as they do there, and a mismatch is reported the same way.
   *
   * @param otherAmount  the value to compare to
   * @return whether this amount is the smaller or the two are equal, or the failure describing the
   *   currency mismatch
   */
  def isLessThanEqualTo(otherAmount: BigMoney): FailureOr[Boolean] =
    comparedWith(otherAmount)(_.isLessThanEqualTo(_))

  /**
   * Applies a comparison of the two amounts, provided the two currencies agree.
   *
   * The four predicates above differ only in which comparison of [[Decimal]] they delegate to, so
   * the currency check and the wording of its failure are written once here. The implementation
   * being ported repeated both four times and reported one message for all four, which is the
   * message reported here.
   *
   * @param otherAmount  the value being compared to
   * @param predicate  the comparison to apply to the two amounts
   * @return the outcome of the comparison, or the failure describing the currency mismatch
   */
  private def comparedWith(
      otherAmount: BigMoney)(predicate: (Decimal, Decimal) => Boolean): FailureOr[Boolean] =
    if (otherAmount.currency == currency) {
      Right(predicate(amount, otherAmount.amount))
    } else {
      Left(Failure.Invalid(BigMoney.DifferentCurrenciesCompareMessage))
    }

  //-------------------------------------------------------------------------
  /**
   * Returns a copy of this value with the amount rounded to the specified scale.
   *
   * A positive scale is a number of decimal places and a negative scale rounds the whole part, so
   * a scale of `-3` rounds to a multiple of a thousand. The rounding is the one asked for rather
   * than the half up this type applies to its own amounts, which is the point of the member: it is
   * how a caller reaches `CEILING`, `FLOOR`, `DOWN` or `HALF_DOWN` on a monetary amount.
   *
   * {{{
   * gbp1_441.roundToScale(2, RoundingMode.CEILING)      // GBP 1.45
   * gbp1_449.roundToScale(2, RoundingMode.DOWN)         // GBP 1.44
   * gbp780001.roundToScale(-3, RoundingMode.CEILING)    // GBP 781000.00
   * }}}
   *
   * Rounding to a scale of twelve or more leaves the amount as it is. That is not a special case
   * of this member but a consequence of the invariant of the type: the amount already has at most
   * twelve decimal places, and rounding never invents a digit that was not there. Rounding to a
   * ''smaller'' scale loses digits, as it must, and the result then passes through the same
   * half-up rounding at twelve places as every other route into the type, which cannot change it
   * further.
   *
   * The two ways this can raise both belong to [[Decimal]] and are the ways the implementation
   * being ported raised: a scale of `-18` or less, where the edges of the representation make the
   * answer ambiguous, and the `UNNECESSARY` mode applied to an amount that does need rounding.
   * Neither depends on the money value - both are properties of the arguments the caller chose -
   * so both stay raised rather than reported, and the signature stays total as it was.
   *
   * @param desiredScale  the scale to round to, positive for decimal places, negative to round the
   *   whole part, and greater than `-18`
   * @param roundingMode  the rounding to apply
   * @return this value with its amount rounded
   * @throws java.lang.IllegalArgumentException if the scale is `-18` or less
   * @throws java.lang.ArithmeticException if the mode is `UNNECESSARY` and rounding is required
   */
  def roundToScale(desiredScale: Int, roundingMode: RoundingMode): BigMoney =
    BigMoney.create(currency, amount.roundToScale(desiredScale, roundingMode))

  //-------------------------------------------------------------------------
  /**
   * Checks whether the amount is zero.
   *
   * @return true if the amount is zero
   */
  def isZero: Boolean = amount.isZero

  /**
   * Checks whether the amount is positive.
   *
   * A zero amount is not positive, and neither is a negative one.
   *
   * @return true if the amount is greater than zero
   */
  def isPositive: Boolean = amount.unscaledValue > 0L

  /**
   * Checks whether the amount is negative.
   *
   * A zero amount is not negative, and neither is a positive one.
   *
   * @return true if the amount is less than zero
   */
  def isNegative: Boolean = amount.unscaledValue < 0L

  //-------------------------------------------------------------------------
  /**
   * Returns a copy of this value with the amount negated.
   *
   * A zero amount returns this value, which is what the implementation being ported did and what
   * keeps the negation of zero from depending on how that zero was written.
   *
   * @return this value with its amount negated
   */
  def negated: BigMoney = if (isZero) this else BigMoney.create(currency, amount.negated)

  /**
   * Returns a copy of this value whose amount is not negative.
   *
   * @return this value if its amount is zero or positive, and its negation otherwise
   */
  def positive: BigMoney = if (isNegative) negated else this

  /**
   * Returns a copy of this value whose amount is not positive.
   *
   * @return this value if its amount is zero or negative, and its negation otherwise
   */
  def negative: BigMoney = if (isPositive) negated else this

  //-------------------------------------------------------------------------
  /**
   * Converts this value to the equivalent [[CurrencyAmount]].
   *
   * The amount is the nearest `Double` to the exact amount held here, so the conversion loses
   * precision in the direction one would expect of it and the currency is unchanged.
   *
   * This is total, as it was in the implementation being ported, and it is reached through the
   * trusted constructor of that type rather than through `CurrencyAmount.of`, which answers with
   * an outcome: that factory rejects one thing only, a value that is not a number, and the
   * `Double` of a decimal is always finite - so routing through it would put a failure branch that
   * cannot be reached into the signature of an accessor whose answer always exists. The trusted
   * route performs the same normalisation and the same invariant check as every other way into
   * that type, and allocates only the amount returned; the implementation being ported constructed
   * the result once as well.
   *
   * @return the equivalent amount, held as a `Double`
   */
  def toCurrencyAmount: CurrencyAmount = CurrencyAmount.ofTrusted(currency, amount.doubleValue)

  /**
   * Converts this value to the equivalent [[Money]].
   *
   * This narrows the amount to the minor units of the currency, rounding half up, so it is a
   * conversion with a decision in it rather than a reinterpretation of the same digits: `RON
   * 200.2345` becomes `RON 200.23`, `BHD 1234.567890123457` becomes `BHD 1234.568` because that
   * currency quotes three digits, and `JPY 1234.567890123457` becomes `JPY 1235` because the yen
   * quotes none. A value whose amount is already within the currency's minor units is unchanged.
   *
   * It is total, because rounding narrows an amount rather than refusing it, and it is written as
   * the `Money.of` overload that takes a value of this type - the same delegation the
   * implementation being ported wrote - so this method and that factory are one route with two
   * names and can never disagree. [[Money.toBigMoney]] is the widening that reverses it.
   *
   * @return the equivalent money value, rounded to the currency's minor units
   */
  def toMoney: Money = Money.of(this)

  //-------------------------------------------------------------------------
  /**
   * Converts this value into the specified currency at the specified rate.
   *
   * The amount is multiplied by the rate, in that order, so `AUD 100` converted into `RON` at
   * `2.6031` is `RON 260.31`.
   *
   * The rate is read as a decimal first, which fails for a rate no decimal holds - one that is not
   * finite, or one whose whole part needs more than eighteen digits - and the conversion then
   * proceeds exactly as the decimal-rated form below describes.
   *
   * @param resultCurrency  the currency of the result
   * @param fxRate  the rate from the currency of this value to the result currency
   * @return this value expressed in the result currency, or the failure describing why the rate
   *   supplied does not describe that conversion
   * @throws java.lang.IllegalArgumentException if the converted amount needs more than eighteen
   *   digits, which is the documented arithmetic precondition of [[Decimal]]
   */
  def convertedTo(resultCurrency: Currency, fxRate: BigDecimal): FailureOr[BigMoney] =
    Decimal.of(fxRate).flatMap(rate => convertedTo(resultCurrency, rate))

  /**
   * Converts this value into the specified currency at the specified rate.
   *
   * Converting into the currency this value already has is the one case that needs a decision, and
   * the decision is the one the implementation being ported made: such a conversion requires no
   * arithmetic, so a rate of one - within a tolerance of `1e-8`, the literal that implementation
   * used - returns this value unchanged, and any other rate is reported as a failure rather than
   * silently applied. That keeps a caller from scaling an amount by passing a rate for a
   * conversion that does not happen.
   *
   * {{{
   * Decimal.of("2.6031").flatMap(aud100.convertedTo(Currency.RON, _))   // Right(RON 260.31)
   * Decimal.of(1L).flatMap(aud100.convertedTo(Currency.AUD, _))         // Right(AUD 100.00), as is
   * Decimal.of(2L).flatMap(aud100.convertedTo(Currency.AUD, _))         // Left - rate must be one
   * }}}
   *
   * The multiplication is exact decimal arithmetic and the product is rounded to twelve decimal
   * places, as every route into this type is - not to the minor units of the result currency,
   * which is what [[Money]] would do and what [[toMoney]] is for.
   *
   * @param resultCurrency  the currency of the result
   * @param fxRate  the rate from the currency of this value to the result currency
   * @return this value expressed in the result currency, or the failure describing why the rate
   *   supplied does not describe that conversion
   * @throws java.lang.IllegalArgumentException if the converted amount needs more than eighteen
   *   digits, which is the documented arithmetic precondition of [[Decimal]]
   */
  def convertedTo(resultCurrency: Currency, fxRate: Decimal): FailureOr[BigMoney] =
    if (currency == resultCurrency) {
      if (DoubleArrayMath.fuzzyEquals(fxRate.doubleValue, 1d, BigMoney.NoConversionTolerance)) {
        Right(this)
      } else {
        Left(Failure.Invalid(BigMoney.NonUnitRateMessage))
      }
    } else {
      Right(BigMoney.of(resultCurrency, amount.multipliedBy(fxRate)))
    }

  /**
   * Converts this value into the specified currency, taking the rate from the specified provider.
   *
   * This is the [[FxConvertible]] implementation of this type. A value already in the requested
   * currency is returned unchanged and the provider is not consulted, so such a conversion
   * succeeds even under a provider that supplies no rates at all - which is the behaviour of the
   * implementation being ported. Otherwise the provider converts the amount and the failure it
   * reports when it holds no rate for the pair is the failure of this conversion.
   *
   * The conversion goes through the `Double`-valued arithmetic of the provider and the exact
   * amount is then rebuilt from the result, which is the route the implementation being ported
   * took for this type; the outcome therefore agrees with it digit for digit, rather than being
   * the more precise answer an exact multiplication by the rate would give. A caller wanting the
   * exact product has the rated form above.
   *
   * @param resultCurrency  the currency of the result
   * @param rateProvider  the provider of FX rates
   * @return this value expressed in the result currency, or the failure the provider reported for
   *   the rate the conversion needed
   */
  override def convertedTo(
      resultCurrency: Currency,
      rateProvider: FxRateProvider): FailureOr[BigMoney] =
    if (currency == resultCurrency) {
      Right(this)
    } else {
      rateProvider
        .convert(amount.doubleValue, currency, resultCurrency)
        .flatMap(converted => BigMoney.of(resultCurrency, converted))
    }

  //-------------------------------------------------------------------------
  /**
   * Compares this value to another, by currency and then by amount.
   *
   * Currencies compare alphabetically by their codes and equal currencies fall through to the
   * amounts, which is the comparison the implementation being ported performed. Both comparisons
   * return zero exactly when their operands are equal, so this returns zero exactly when the two
   * values are equal and the ordering agrees with equality.
   *
   * Only the sign of the result is part of the contract, as it is for every comparison in Scala.
   * The implementation being ported narrowed the magnitude to `-1` and `1` through the comparison
   * chain it used; this returns the value the underlying comparison gives, so the sign agrees and
   * the magnitude may differ.
   *
   * @param other  the value to compare to
   * @return negative when this value is the smaller, zero when the two are equal, positive
   *   otherwise
   */
  def compareTo(other: BigMoney): Int = {
    val byCurrency = currency.code.compareTo(other.currency.code)
    if (byCurrency != 0) byCurrency else amount.compareTo(other.amount)
  }

  /**
   * Returns the formatted text of this value.
   *
   * The form is the currency code, a space, and the amount shown with at least the number of
   * decimal places the currency quotes - `AUD 200.00`, `RON 200.2345`, `BHD 100.120` - which is
   * the form [[BigMoney.parse]] reads back and the form the implementation being ported wrote. The
   * amount is padded rather than truncated, so an amount finer than the currency's minor units is
   * shown in full; only an amount with fewer digits is padded.
   *
   * @return the formatted value
   */
  override def toString: String = s"$currency ${amount.formatAtLeast(currency.minorUnitDigits)}"
}

/**
 * Provides the ways of obtaining a high-precision monetary value, the route from text, and the
 * instances for the type.
 *
 * This companion is the only place a [[BigMoney]] is created. Every route into the type goes
 * through one private creation step, which rounds the amount to twelve decimal places half up, so
 * the invariant of the type holds for a value built by a factory and for a value produced by
 * arithmetic alike. Neither the constructor nor a generated `apply` or `copy` is available, and
 * the type is sealed, so no value of it can carry an amount of more than twelve decimal places.
 *
 * @see [[BigMoney]] for the type itself and for what a value of it holds
 * @see [[Money]] for the sibling rounded to the currency's minor units
 */
object BigMoney {

  /**
   * The number of decimal places a value of this type keeps.
   *
   * Twelve, which is the width the implementation being ported rounded to and the property that
   * distinguishes this type from [[Money]]. It appears once, in [[create]], so every route into
   * the type rounds to the same width by construction rather than by repetition.
   */
  private val MaximumDecimalPlaces: Int = 12

  /** Reported when two values of different currencies are added. */
  private val DifferentCurrenciesAddMessage: String =
    "Unable to add amounts in different currencies"

  /** Reported when two values of different currencies are subtracted. */
  private val DifferentCurrenciesSubtractMessage: String =
    "Unable to subtract amounts in different currencies"

  /**
   * Reported when two values of different currencies are compared by one of the four predicates.
   *
   * One wording covers all four, as it did in the implementation being ported, so a caller cannot
   * tell from the message which comparison was attempted - only that the currencies disagreed,
   * which is the whole of the problem.
   */
  private val DifferentCurrenciesCompareMessage: String =
    "Unable to compare amounts in different currencies"

  /** Reported when a rate other than one is supplied for a conversion that does not convert. */
  private val NonUnitRateMessage: String =
    "FX rate must be 1 when no conversion required"

  /**
   * The tolerance within which a rate counts as one for a conversion that does not convert.
   *
   * This is the literal the implementation being ported used for the same comparison, so a rate it
   * accepted for a conversion into the currency of the value is accepted here as well.
   */
  private val NoConversionTolerance: Double = 1e-8

  /** The number of parts the text form of a value has, which is the code and the amount. */
  private val TextParts: Int = 2

  /**
   * The separator between the currency code and the amount in the text form.
   *
   * A single space, as [[BigMoney.toString]] writes it and as [[BigMoney.parse]] splits on.
   */
  private val TextSeparator: String = " "

  //-------------------------------------------------------------------------
  /**
   * Obtains a zero value in the specified currency.
   *
   * Zero is an amount every currency admits at every scale, so this is total.
   *
   * @param currency  the currency the value is in
   * @return the zero value in that currency
   */
  def zero(currency: Currency): BigMoney = create(currency, Decimal.ZERO)

  /**
   * Obtains a value from a [[CurrencyAmount]], keeping twelve decimal places of the amount.
   *
   * This answers with an outcome where the implementation being ported was total, and the
   * difference is a real one rather than a stylistic one: an amount is permitted to be infinite -
   * that type accepts the two infinities and rejects only a value that is not a number - while no
   * decimal holds a value that is not finite. An infinite amount is therefore not a value of this
   * type, and saying so in the signature is what keeps the rejection visible instead of raising
   * from an accessor later. An amount whose magnitude needs more than eighteen digits is rejected
   * for the same reason. The implementation being ported raised on both, from the decimal
   * conversion this performs. [[Money.of]] answers with an outcome for the same boundary and for
   * the same reason.
   *
   * {{{
   * CurrencyAmount.of(Currency.RON, 200.2345d).flatMap(BigMoney.of)   // Right(RON 200.2345)
   * }}}
   *
   * @param currencyAmount  the amount to hold exactly
   * @return the value, or the failure describing why the amount is not one
   */
  def of(currencyAmount: CurrencyAmount): FailureOr[BigMoney] =
    Decimal.of(currencyAmount.amount).map(decimal => create(currencyAmount.currency, decimal))

  /**
   * Obtains a value from a [[Money]].
   *
   * This is total and loses nothing: the amount of a money value is already at the minor units of
   * its currency, and no currency of the reference data quotes more than three digits, so it is
   * well within the twelve places this type keeps. Widening therefore always succeeds, and
   * narrowing back through [[BigMoney.toMoney]] returns the money value this was built from.
   *
   * @param money  the money value to widen
   * @return the equivalent high-precision value
   */
  def of(money: Money): BigMoney = create(money.currency, money.amount)

  /**
   * Obtains a value from a currency and a `Double` amount, keeping twelve decimal places.
   *
   * The amount is read as the decimal its shortest text names, so `0.1` is a tenth exactly and not
   * the binary value a tenth is stored as. It fails for a value no decimal holds, which is one
   * that is not finite and one needing more than eighteen digits.
   *
   * {{{
   * BigMoney.of(Currency.GBP, 1.000009d)           // Right(GBP 1.000009)
   * BigMoney.of(Currency.GBP, 1.123456789012345d)  // Right(GBP 1.123456789012) - twelve places
   * BigMoney.of(Currency.GBP, 1d / 0d)             // Left - no decimal is infinite
   * }}}
   *
   * @param currency  the currency the value is in
   * @param amount  the amount of that currency
   * @return the value, or the failure describing why the number is not an amount
   */
  def of(currency: Currency, amount: Double): FailureOr[BigMoney] =
    Decimal.of(amount).map(decimal => create(currency, decimal))

  /**
   * Obtains a value from a currency and a `BigDecimal` amount, keeping twelve decimal places.
   *
   * Precision beyond eighteen digits is truncated towards zero, as it is by every decimal factory
   * of this port, and a value too large for a decimal to hold is reported as a failure. The
   * fraction is then rounded to twelve places half up, so this is the route by which a
   * `BigDecimal` of arbitrary width becomes a value of this type.
   *
   * @param currency  the currency the value is in
   * @param amount  the amount of that currency
   * @return the value, or the failure describing why the number is not an amount
   */
  def of(currency: Currency, amount: BigDecimal): FailureOr[BigMoney] =
    Decimal.of(amount).map(decimal => create(currency, decimal))

  /**
   * Obtains a value from a currency and an exact decimal amount, keeping twelve decimal places.
   *
   * This is total: a decimal is already a number this type can hold, so there is nothing left to
   * reject - the rounding narrows an amount of more than twelve places rather than refusing it. It
   * is consequently the factory the other routes end at, the one the JSON decoder uses, and the
   * one the arithmetic of this type is built on.
   *
   * @param currency  the currency the value is in
   * @param amount  the amount of that currency
   * @return the value, with its amount rounded to twelve decimal places
   */
  def of(currency: Currency, amount: Decimal): BigMoney = create(currency, amount)

  //-------------------------------------------------------------------------
  /**
   * Parses a value from text of the form `RON 200.2345`.
   *
   * The parsed form is the currency code, a space and the amount, which is the form
   * [[BigMoney.toString]] writes. The text is split on the space and has to fall into exactly two
   * parts, as it did in the implementation being ported - so text holding no space, and text
   * holding two, is rejected as malformed before anything is read from it. The two wordings that
   * implementation reported are the two wordings reported here:
   *
   *   - text that does not fall into exactly two space-separated parts is
   *     `Unable to parse amount, invalid format: <text>`;
   *   - text of the right shape that nonetheless names no value - because the code names no
   *     currency, or because the digits name no decimal - is `Unable to parse amount: <text>`.
   *
   * {{{
   * BigMoney.parse("RON 200.2345")          // Right(RON 200.2345)
   * BigMoney.parse("AUD 1.123456789012345") // Right(AUD 1.123456789012) - rounded, as every route is
   * BigMoney.parse("$100")                  // Left - invalid format, one part
   * BigMoney.parse("200.23 RON")            // Left - right shape, names no currency in its first part
   * }}}
   *
   * The case of the currency code is tolerated, as it was in the implementation being ported,
   * because the code is resolved through [[Currency.parse]]. The cause of a rejection is
   * deliberately not carried in the failure: the implementation being ported wrapped it in the
   * exception it threw, but the message - which is what a caller reads, logs and asserts on - named
   * only the text, and keeping the failure to that one message keeps two failures over the same
   * text equal and their serialized form stable.
   *
   * Both wordings name the text as it was given, so each reads as the original's did. The text
   * came from outside the library, so bounding it and escaping what it may hold belong to the
   * writing of a failure, which [[com.opengamma.strata.collect.result.Failure.show]] and the
   * text form of a failure perform for every part they write.
   *
   * @param amountStr  the value as text, in the form `RON 200.2345`
   * @return the value the text names, or the failure describing why it names none
   */
  def parse(amountStr: String): FailureOr[BigMoney] = {
    // The limit of -1 keeps trailing empty parts, which is what the splitter the implementation
    // being ported used did: "RON " is two parts there and has to be two parts here, so that it is
    // rejected for naming no decimal rather than for having the wrong shape.
    val parts = amountStr.split(TextSeparator, -1)
    if (parts.length != TextParts) {
      Left(invalidFormat(amountStr))
    } else {
      val parsed: Option[BigMoney] = for {
        currency <- Currency.parse(parts(0)).toOption
        amount <- Decimal.parse(parts(1)).toOption
      } yield create(currency, amount)
      // the text is rendered rather than interpolated as it stands, which bounds the message and
      // keeps it to one line while leaving an in-bound spelling quoted as it was given
      parsed.toRight(Failure.Parsing(s"Unable to parse amount: $amountStr"))
    }
  }

  //-------------------------------------------------------------------------
  /**
   * Creates a value, rounding the amount, which every route funnels through.
   *
   * This is the only instantiation of the type and it is private, so the routes above are the only
   * way into it from outside this file. Rounding here rather than in each of them is what makes the
   * invariant of the type hold for the results of the arithmetic as well: the implementation being
   * ported reached its rounding constructor from `plus`, `minus`, `multipliedBy`, `map`,
   * `roundToScale` and each of its factories, and so does this.
   *
   * Rounding is to twelve decimal places, half up, and cannot fail: twelve is within the eighteen
   * places a [[Decimal]] holds, so the scale is always available, and an amount already at twelve
   * places or fewer is returned as it stands rather than padded.
   *
   * @param currency  the currency
   * @param amount  the amount, rounded here to twelve decimal places
   * @return the value
   */
  private def create(currency: Currency, amount: Decimal): BigMoney =
    new BigMoney(currency, amount.roundToScale(MaximumDecimalPlaces, RoundingMode.HALF_UP)) {}

  /**
   * The failure reported for text whose shape does not admit a monetary value.
   *
   * The text is quoted as it stands, which is the wording the implementation being ported
   * produced; bounding it and escaping what it may hold belong to the writing of a failure,
   * which the text form of one and [[Failure.show]] perform for every part they write.
   */
  private def invalidFormat(amountStr: String): Failure =
    Failure.Parsing(s"Unable to parse amount, invalid format: $amountStr")

  //-------------------------------------------------------------------------
  /**
   * The ordering, hashing and equality of values of this type.
   *
   * Values order by currency alphabetically and then by amount, which is the comparison the
   * implementation being ported performed and which [[BigMoney.compareTo]] describes. The ordering
   * agrees with equality exactly - `compare` returns zero precisely when `eqv` holds - because both
   * fields compare as zero only when they are equal, so no secondary comparison is needed to break
   * a tie.
   *
   * Equality and hashing are those of the value itself: the currency compares by its code and the
   * amount is a normalised decimal that compares as a number, which is exactly the comparison the
   * implementation being ported made. Nothing bit-level is involved, unlike [[CurrencyAmount]],
   * whose `Double` amount forces it to compare bit patterns so that a value that is not a number
   * still equals itself. No override is written here for that reason, and the hashing is the
   * deterministic hash of the two fields, so the hash of a value is identical in every run of every
   * program - which is what the byte-stability properties of the test suite rely on.
   *
   * This is the only equality-bearing instance of the type. `Order` and `Hash` both extend `Eq`, so
   * a separate `Eq` would be a second answer to the same question; one is declared here and `Eq` is
   * obtained from it by subtyping.
   *
   * @return the ordering and hashing of values of this type
   */
  implicit val order: Order[BigMoney] with Hash[BigMoney] =
    new Order[BigMoney] with Hash[BigMoney] {

      private val universal: Hash[BigMoney] = Hash.fromUniversalHashCode[BigMoney]

      override def compare(x: BigMoney, y: BigMoney): Int = x.compareTo(y)

      override def eqv(x: BigMoney, y: BigMoney): Boolean = universal.eqv(x, y)

      override def hash(x: BigMoney): Int = universal.hash(x)
    }

  /**
   * The rendering of a value, which is the text [[BigMoney.toString]] produces.
   *
   * @return the rendering of a value of this type
   */
  implicit val show: Show[BigMoney] = Show.show(_.toString)

  //-------------------------------------------------------------------------
  /**
   * The raw field shape both codecs of this type are derived over.
   *
   * A type without a public constructor cannot be derived over directly, and decoding one is two
   * steps - read the fields, then hand them to the factory that decides what they describe. This
   * product is the shape those fields have. It exists only for that purpose: it is private and it
   * is never returned, so no caller can hold an unrounded pair. Its field names are the JSON keys,
   * and they are the names of the two fields of [[BigMoney]] itself, which is what keeps the
   * derived shape and the type from drifting apart.
   *
   * @param currency  the currency, read from its three letter code
   * @param amount  the amount, read from the canonical text of a decimal and not yet rounded
   */
  private final case class Raw(currency: Currency, amount: Decimal)

  /** The derived decoder of the raw field shape, used by the decoder below. */
  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /** The derived encoder of the raw field shape, used by the encoder below. */
  private val rawEncoder: Encoder[Raw] = deriveEncoder[Raw]

  /**
   * The JSON encoding of values of this type.
   *
   * A value is an object of two fields, the currency as its code and the amount as the canonical
   * text of a decimal:
   *
   * {{{
   * {"currency":"GBP","amount":"12.34"}
   * }}}
   *
   * The amount is a string rather than a number because that is how every decimal of this port is
   * written: a JSON number is read back as a binary floating point value in most parsers, which is
   * exactly the representation this type exists to avoid - and with twelve decimal places to carry,
   * it is the type for which that matters most. The text is the canonical form of the decimal, so
   * it carries the amount and not a presentation width - `AUD 100.00` is written `"100"` - and the
   * width is recovered from the currency whenever the value is rendered.
   *
   * The shape is derived at compile time over the raw product above rather than written out field
   * by field, which is what every product of this port does, and the value is contramapped into
   * that product: a value in memory is already rounded, so nothing further has to be decided on the
   * way out. The result is wrapped so that a field holding no value would be omitted, which is the
   * policy every product of this port follows - this type has no optional field, so the wrapping
   * changes nothing about its output and exists so that the policy holds without exception.
   *
   * @return the JSON encoding of a value of this type
   */
  implicit val encoder: Encoder[BigMoney] =
    Codecs.dropNulls(rawEncoder.contramap[BigMoney](value => Raw(value.currency, value.amount)))

  /**
   * The JSON decoding of values of this type.
   *
   * This is the inverse of the encoding above: the payload is read into the raw shape and handed to
   * the total factory, which rounds the amount to twelve decimal places. A payload whose amount
   * names more places is therefore accepted and rounded rather than rejected, which is the
   * behaviour of every other route into this type and of the implementation being ported, whose
   * constructor rounded whatever it was given.
   *
   * That choice is consistent with the round trip the test suite asserts: encoding is a function of
   * the value, and a value is always rounded already, so decoding what this encoder wrote returns
   * the value it was given and equal values encode to identical bytes. It is the other direction
   * that normalises - a hand-written document naming `"1.123456789012345"` decodes to
   * `1.123456789012` and re-encodes as that - and that is a deliberate normalisation of input
   * rather than an inconsistency of the codec.
   *
   * The payload reaches the factory through
   * [[com.opengamma.strata.collect.json.Codecs.validatedDecoder]], the one construction gate every
   * checking and every rounding type of this port decodes through, so this type is inside that
   * policy rather than beside it: whatever is added to the gate - a further check, a different way
   * of reporting a rejection - is inherited here without this file being touched, and [[Money]]
   * reads the same two fields through the same gate. The factory the gate reaches is total, so no
   * payload of the right shape is refused at that point, and the rounding to twelve decimal places
   * described above still happens on the way in. What a document can get wrong is its fields: a
   * code naming no currency, or text naming no decimal, is rejected by the codecs of those two
   * types as the fields are read.
   *
   * @return the JSON decoding of a value of this type
   */
  implicit val decoder: Decoder[BigMoney] =
    Codecs.validatedDecoder[Raw, BigMoney](raw => Right(of(raw.currency, raw.amount)))(rawDecoder)
}
