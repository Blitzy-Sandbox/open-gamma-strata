/*
 * Copyright (C) 2017 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.math.BigDecimal

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
import com.opengamma.strata.collect.FixedScaleDecimal
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * An amount of a currency, held exactly and rounded to the minor units of that currency.
 *
 * This is the exact-decimal counterpart of [[CurrencyAmount]]: the amount is a [[Decimal]]
 * rather than a `Double`, so `0.1 + 0.2` is `0.3` and not a value a hundredth of a trillionth
 * away from it, and it is rounded to the number of digits the currency quotes. A balance of
 * `GBP 12.34` is two decimal places because sterling has two minor unit digits, `JPY 123` is
 * none because the yen has none, and `BHD 1.235` is three. Since the rounding is half up, an
 * amount this type holds should be read as the money value its currency can express rather than
 * as an exact figure of unlimited precision.
 *
 * ===Rounding is the invariant, and it is applied everywhere===
 *
 * Every route into this type rounds: the factories, and equally the arithmetic, both of which
 * reach one private creation step that rounds. Rounding the operands as they are built is what an
 * ordinary sum shows - `JPY 0.4` plus `JPY 0.4` is `JPY 0`, because each operand is `JPY 0`
 * before the addition, and `USD 0.005` plus `USD 0.005` is `USD 0.02`, because each is
 * `USD 0.01` first. Rounding the ''result'' shows in any operation that introduces a finer
 * digit: `USD 10` divided by three through [[map]] is `USD 3.33`, and a conversion at a rate
 * that lands between two minor units is rounded to one of them. The invariant that follows from
 * both is worth stating on its own: '''the amount of a money value is always at the scale of its
 * currency''' - never finer. [[getValue]], [[getAmount]] and the text form all rely on it, and it
 * holds for each of the three minor unit digit counts the currency data carries, which are none,
 * two and three.
 *
 * The scale is an upper bound rather than an exact width, because [[Decimal]] normalises away a
 * trailing fractional zero: the amount inside `USD 63.30` is the decimal `63.3` at scale one.
 * That is invisible in every rendering, since [[toString]], [[getValue]] and [[getAmount]] all
 * present the amount at the width of the currency, and it is why `USD 63.3` and `USD 63.30`
 * name one value rather than two.
 *
 * ===Which operations can fail===
 *
 * Adding or subtracting another money value fails when the two currencies differ, because the
 * result would have no currency; parsing fails when the text names no amount; converting fails
 * when the rate the conversion needs is unavailable, and when a rate other than one is supplied
 * for a conversion into the currency the value already has. Building a value from a `Double`, a
 * `BigDecimal` or a [[CurrencyAmount]] fails when the number is one no decimal holds - one that
 * is not finite, or one needing more than eighteen digits. Each of those depends on the values
 * involved rather than on the calling code, so each is an `Either` and none of them abandons the
 * call stack.
 *
 * The arithmetic, by contrast, is total in signature. Its one edge belongs to [[Decimal]] rather
 * than to this type: a result beyond eighteen digits is a broken precondition and is raised by
 * the argument checks of `strata-collect`, not reported as a failure of money.
 *
 * ===Equality and ordering===
 *
 * Two money values are equal when their currencies and their amounts are equal. No bit-level
 * special case is needed here - the reason [[CurrencyAmount]] needs one is that it holds a
 * `Double`, where a value that is not a number would otherwise differ from itself; an amount held
 * as a [[Decimal]] is normalised and compares as a number, so the equality synthesised from the
 * two fields is already the right one. Values order by currency alphabetically and then by
 * amount, and that ordering agrees with equality: `compare` returns zero exactly when the two
 * values are equal.
 *
 * ===Conversions to the neighbouring types===
 *
 * [[Money.of]] accepts a [[CurrencyAmount]] and [[toCurrencyAmount]] converts back, so the
 * inexact and exact representations are one step apart in both directions. [[toBigMoney]] widens
 * this value to [[BigMoney]] - the arbitrary-precision sibling, which keeps twelve decimal places
 * instead of the currency's - and `Money.of(bigMoney)` narrows one back, applying the rounding of
 * this type; [[BigMoney.of]] and [[BigMoney.toMoney]] are the same two conversions named from the
 * other side. Widening loses nothing, since twelve places are more than any currency quotes,
 * while narrowing rounds half up to the minor units and is therefore the direction with a
 * decision in it.
 *
 * This type is immutable and thread-safe: a value of it can be shared freely, and every
 * operation returns a new value rather than changing the one it was called on.
 *
 * @param currency  the currency, which is `GBP` in the value `GBP 12.34`
 * @param amount  the amount of that currency, always at the scale of the currency's minor units
 *   or finer, which is `12.34` in the value `GBP 12.34`
 * @see [[CurrencyAmount]] for the `Double`-valued amount this type rounds
 * @see [[FxRateProvider]] for the source of the rates the conversions use
 */
sealed abstract case class Money private (currency: Currency, amount: Decimal)
    extends FxConvertible[Money]
    with NoJavaSerialization {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means can be
  // stopped is here. The single implementation is the companion's hidden `Impl`.
  JvmClosure.requireSoleImplementation(this, classOf[Money.Impl])

  // The invariant of this type, stated over the amount the instance actually holds rather than
  // over the argument a factory was given, because the implementation class carries a public
  // constructor in the class file whatever the source asked for: a class compiled outside this
  // library can call it directly, and identity alone would then admit an amount finer than the
  // currency quotes - a value that would print and convert as digits the currency has no minor
  // unit for. What `Money.create` establishes is that the amount is rounded to those minor units,
  // so rounding it again is what leaves it alone.
  JvmClosure.requireInvariant(
    "its amount is already rounded to the minor units of its currency",
    currency.roundMinorUnits(amount) == amount)

  /**
   * Gets the amount as a `BigDecimal` of exactly the currency's scale.
   *
   * The scale of the value returned is the number of minor unit digits of the currency rather
   * than the scale of the amount, so the digits shown are the digits the currency quotes:
   * `GBP 13.4` converts to a `BigDecimal` reading `13.40`. Setting a scale that way can in
   * general refuse to drop a digit, but it never has one to drop here, since the amount of a
   * money value is at the currency's scale or finer by construction - so this only ever pads,
   * and it is total.
   *
   * [[getValue]] is the preferred accessor, since it carries the scale alongside the value
   * instead of encoding it in a `BigDecimal`, and the [[amount]] the value holds is the third
   * route to the same digits. This accessor remains published and carries no deprecation
   * annotation.
   *
   * @return the amount, with a scale equal to the currency's minor unit digits
   */
  def getAmount: BigDecimal = amount.toBigDecimal.setScale(currency.minorUnitDigits)

  /**
   * Gets the amount together with the scale it is presented at.
   *
   * The scale is the number of minor unit digits of the currency, so the value returned shows
   * `12.30` for `GBP 12.3` and `123` for `JPY 123`, which is what a [[FixedScaleDecimal]]
   * exists to express.
   *
   * The outcome is always a `Right` for a money value. Pairing a decimal with a scale is
   * rejected only when the scale would drop a digit of the decimal or exceed eighteen, and
   * neither can happen here: the amount is at the currency's scale or finer by construction, and
   * no currency of the built-in data has more than three minor unit digits. The outcome is
   * nevertheless returned rather than unwrapped, because [[FixedScaleDecimal]] publishes no
   * total factory and no constant, so unwrapping would mean inventing a money amount to return
   * in a branch that cannot be reached - and an invented amount of money is a worse answer than
   * an outcome a caller pattern-matches.
   *
   * Callers wanting the amount without an outcome to unwrap have two total accessors: the
   * [[amount]] the value holds, and [[getAmount]] for the same digits as a `BigDecimal`.
   *
   * @return the amount at the currency's scale, which is always present
   */
  def getValue: ResultNec[FixedScaleDecimal] = amount.toFixedScale(currency.minorUnitDigits)

  /**
   * Returns a copy of this value with the specified amount added.
   *
   * The two values have to be in the same currency, since money is an amount ''of'' a currency
   * and there is no meaningful sum of amounts of different ones. A mismatch is reported rather
   * than thrown, because whether it happens depends on the values a caller holds:
   *
   * {{{
   * gbp100.plus(gbp50)   // Right(GBP 150.00)
   * gbp100.plus(usd50)   // Left(Failure.Invalid("Unable to add amounts in different currencies"))
   * }}}
   *
   * The sum is rounded to the currency's minor units, as every route into this type is.
   *
   * @param amountToAdd  the amount to add, in the same currency as this value
   * @return this value with the other added, or the failure describing the currency mismatch
   * @throws java.lang.IllegalArgumentException if the sum needs more than eighteen digits, which
   *   is the documented arithmetic precondition of [[Decimal]]
   */
  def plus(amountToAdd: Money): FailureOr[Money] =
    if (amountToAdd.currency == currency) {
      Right(Money.create(currency, amount.plus(amountToAdd.amount)))
    } else {
      Left(Failure.Invalid(Money.DifferentCurrenciesAddMessage))
    }

  /**
   * Returns a copy of this value with the specified amount subtracted.
   *
   * This is [[plus]] read in the other direction and behaves the same way in every respect: the
   * currencies have to agree, the difference is exact decimal arithmetic, and the result is
   * rounded to the currency's minor units.
   *
   * @param amountToSubtract  the amount to subtract, in the same currency as this value
   * @return this value with the other subtracted, or the failure describing the currency
   *   mismatch
   * @throws java.lang.IllegalArgumentException if the difference needs more than eighteen
   *   digits, which is the documented arithmetic precondition of [[Decimal]]
   */
  def minus(amountToSubtract: Money): FailureOr[Money] =
    if (amountToSubtract.currency == currency) {
      Right(Money.create(currency, amount.minus(amountToSubtract.amount)))
    } else {
      Left(Failure.Invalid(Money.DifferentCurrenciesSubtractMessage))
    }

  /**
   * Returns a copy of this value with the amount multiplied by the specified whole number.
   *
   * The multiplier is a whole number because multiplying money by a whole number is the
   * operation that cannot introduce a digit the currency does not quote. Scaling by a fraction is
   * [[map]] over the amount, or [[convertedTo]] where the fraction is an exchange rate.
   *
   * A whole-number literal needs no suffix, since it is typed as the `Long` the parameter asks
   * for, while an `Int` ''value'' is widened explicitly at the call site: `money.multipliedBy(3)`
   * and, for an `Int` count, `money.multipliedBy(count.toLong)`.
   *
   * @param valueToMultiplyBy  the whole number to multiply the amount by
   * @return this value with its amount multiplied
   * @throws java.lang.IllegalArgumentException if the product needs more than eighteen digits,
   *   which is the documented arithmetic precondition of [[Decimal]]
   */
  def multipliedBy(valueToMultiplyBy: Long): Money =
    Money.create(currency, amount.multipliedBy(valueToMultiplyBy))

  /**
   * Returns a copy of this value with the amount produced by the specified function.
   *
   * This is how the arithmetic of [[Decimal]] is reached, so that an operation this type does
   * not publish is still a single step:
   *
   * {{{
   * val doubled = money.map(_.multipliedBy(2L))
   * val halved = money.map(_.dividedBy(2L))
   * }}}
   *
   * The result is rounded to the currency's minor units, which is what makes this safe to apply
   * a function of arbitrary precision to: halving `GBP 12.35` gives `GBP 6.18` rather than a
   * balance carrying a half penny.
   *
   * @param mapper  the function to apply to the amount
   * @return this value with the function applied to its amount and the result rounded
   */
  def map(mapper: Decimal => Decimal): Money = Money.create(currency, mapper(amount))

  /**
   * Returns a copy of this value with the amount produced by the specified function on
   * `BigDecimal`.
   *
   * This is [[map]] with the amount presented as a `BigDecimal`, and it answers with an outcome
   * where [[map]] is total: a function of arbitrary precision can return a number no decimal
   * holds - one needing more than eighteen digits - and that is a property of the function and
   * the value rather than of the calling code.
   *
   * [[map]] is the preferred member, its function working on the decimal directly. This one
   * remains published and carries no deprecation annotation.
   *
   * @param mapper  the function to apply to the amount, as a `BigDecimal`
   * @return this value with the function applied and the result rounded, or the failure
   *   describing why the result is not an amount
   */
  def mapAmount(mapper: BigDecimal => BigDecimal): FailureOr[Money] =
    amount.mapAsBigDecimal(mapper).map(mapped => Money.create(currency, mapped))

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

  /**
   * Returns a copy of this value with the amount negated.
   *
   * A zero amount returns this value, which keeps the negation of zero from depending on how
   * that zero was written.
   *
   * @return this value with its amount negated
   */
  def negated: Money = if (isZero) this else Money.create(currency, amount.negated)

  /**
   * Returns a copy of this value whose amount is not negative.
   *
   * @return this value if its amount is zero or positive, and its negation otherwise
   */
  def positive: Money = if (isNegative) negated else this

  /**
   * Returns a copy of this value whose amount is not positive.
   *
   * @return this value if its amount is zero or negative, and its negation otherwise
   */
  def negative: Money = if (isPositive) negated else this

  /**
   * Converts this value to the equivalent [[CurrencyAmount]].
   *
   * The amount is the nearest `Double` to the exact amount held here, so the conversion loses
   * precision in the direction one would expect of it and the currency is unchanged.
   *
   * This is total, and it is reached through the trusted constructor of that type rather than
   * through `CurrencyAmount.of`, which answers with an outcome: that factory rejects one thing
   * only, a value that is not a number, and the `Double` of a decimal is always finite - so
   * routing through it would put a failure branch that cannot be reached into the signature of an
   * accessor whose answer always exists. The trusted route performs the same normalisation and
   * the same invariant check as every other way into that type, and allocates only the amount
   * returned.
   *
   * @return the equivalent amount, held as a `Double`
   */
  def toCurrencyAmount: CurrencyAmount = CurrencyAmount.ofTrusted(currency, amount.doubleValue)

  /**
   * Converts this value to the equivalent [[BigMoney]].
   *
   * This widens the amount from the minor units of the currency to scale twelve, which keeps
   * every digit: no currency quotes more than twelve decimal places, so the value is the same
   * number in a representation that can hold finer ones. `RON 200.23` becomes `RON 200.23`, and
   * a later calculation on the widened value is free to produce digits this type would have
   * rounded away.
   *
   * It is total, because widening cannot fail, and it goes through the total factory of
   * [[BigMoney]], so it answers with the same value as `BigMoney.of(money)`. `Money.of(bigMoney)`
   * is the narrowing that reverses it, and the round trip returns this value unchanged for the
   * same reason widening keeps every digit.
   *
   * @return the equivalent arbitrary-precision value
   */
  def toBigMoney: BigMoney = BigMoney.of(this)

  /**
   * Converts this value into the specified currency at the specified rate.
   *
   * The amount is multiplied by the rate, in that order, so `GBP 100` converted into `USD` at
   * `1.6` is `USD 160.00`. The product is rounded to the minor units of the '''result'''
   * currency, so converting into a currency that quotes fewer digits rounds to the digits that
   * currency quotes.
   *
   * The rate is read as a decimal first, which fails for a rate no decimal holds - one that is
   * not finite, or one needing more than eighteen digits - and the conversion then proceeds
   * exactly as the decimal-rated form below describes.
   *
   * @param resultCurrency  the currency of the result
   * @param fxRate  the rate from the currency of this value to the result currency
   * @return this value expressed in the result currency, or the failure naming the broken
   *   condition: the rate has to be a number a decimal holds, and a conversion into the currency
   *   this value already has requires a rate of one
   * @throws java.lang.IllegalArgumentException if the converted amount needs more than eighteen
   *   digits, which is the documented arithmetic precondition of [[Decimal]]
   */
  def convertedTo(resultCurrency: Currency, fxRate: BigDecimal): FailureOr[Money] =
    Decimal.of(fxRate).flatMap(rate => convertedTo(resultCurrency, rate))

  /**
   * Converts this value into the specified currency at the specified rate.
   *
   * Converting into the currency this value already has is the one case that needs a decision:
   * such a conversion requires no arithmetic, so a rate of one - within a tolerance of `1e-8` -
   * returns this value unchanged, and any other rate is reported as a failure rather than
   * silently applied. That keeps a caller from scaling an amount by passing a rate for a
   * conversion that does not happen.
   *
   * {{{
   * Decimal.of("1.6").flatMap(gbp100.convertedTo(Currency.USD, _))   // Right(USD 160.00)
   * Decimal.of(1L).flatMap(gbp100.convertedTo(Currency.GBP, _))      // Right(GBP 100.00), as is
   * Decimal.of(2L).flatMap(gbp100.convertedTo(Currency.GBP, _))      // Left - rate must be one
   * }}}
   *
   * @param resultCurrency  the currency of the result
   * @param fxRate  the rate from the currency of this value to the result currency
   * @return this value expressed in the result currency, or the failure naming the broken
   *   condition: a conversion into the currency this value already has requires a rate of one,
   *   compared within `1e-8`
   * @throws java.lang.IllegalArgumentException if the converted amount needs more than eighteen
   *   digits, which is the documented arithmetic precondition of [[Decimal]]
   */
  def convertedTo(resultCurrency: Currency, fxRate: Decimal): FailureOr[Money] =
    if (currency == resultCurrency) {
      if (DoubleArrayMath.fuzzyEquals(fxRate.doubleValue, 1d, Money.NoConversionTolerance)) {
        Right(this)
      } else {
        Left(Failure.Invalid(Money.NonUnitRateMessage))
      }
    } else {
      Right(Money.of(resultCurrency, amount.multipliedBy(fxRate)))
    }

  /**
   * Converts this value into the specified currency, taking the rate from the specified provider.
   *
   * This is the [[FxConvertible]] implementation of this type. A value already in the requested
   * currency is returned unchanged and the provider is not consulted, so such a conversion
   * succeeds even under a provider that supplies no rates at all. Otherwise the provider converts
   * the amount and the failure it reports when it holds no rate for the pair is the failure of
   * this conversion.
   *
   * The conversion goes through the `Double`-valued arithmetic of the provider and the exact
   * amount is then rebuilt from the result, so the outcome carries the precision of that
   * arithmetic rather than the more precise answer an exact multiplication by the rate would
   * give.
   *
   * @param resultCurrency  the currency of the result
   * @param rateProvider  the provider of FX rates
   * @return this value expressed in the result currency, or the failure the provider reported
   *   for the rate the conversion needed
   */
  override def convertedTo(
      resultCurrency: Currency,
      rateProvider: FxRateProvider): FailureOr[Money] =
    if (currency == resultCurrency) {
      Right(this)
    } else {
      rateProvider
        .convert(amount.doubleValue, currency, resultCurrency)
        .flatMap(converted => Money.of(resultCurrency, converted))
    }

  /**
   * Compares this value to another, by currency and then by amount.
   *
   * Currencies compare alphabetically by their codes and equal currencies fall through to the
   * amounts. Both comparisons return zero exactly when their operands are equal, so this returns
   * zero exactly when the two values are equal and the ordering agrees with equality.
   *
   * @param other  the value to compare to
   * @return negative when this value is the smaller, zero when the two are equal, positive
   *   otherwise
   */
  def compareTo(other: Money): Int = {
    val byCurrency = currency.code.compareTo(other.currency.code)
    if (byCurrency != 0) byCurrency else amount.compareTo(other.amount)
  }

  /**
   * Returns the formatted text of this value.
   *
   * The form is the currency code, a space, and the amount shown with at least the number of
   * decimal places the currency quotes - `GBP 12.34`, `JPY 123`, `BHD 1.235` - which is the form
   * [[Money.parse]] reads back. The amount is padded rather than truncated, so `USD 63.3` is
   * written `USD 63.30`; since the amount is at the currency's scale or finer by construction, no
   * digit of it is ever dropped.
   *
   * @return the formatted value
   */
  override def toString: String = s"$currency ${amount.formatAtLeast(currency.minorUnitDigits)}"
}

/**
 * Provides the ways of obtaining a money value, the route from text, and the instances for the
 * type.
 *
 * This companion is the only place a [[Money]] is created. Every route into the type goes
 * through one private creation step, which rounds the amount to the minor units of the currency,
 * so the invariant of the type holds for a value built by a factory and for a value produced by
 * arithmetic alike. Neither the constructor nor a synthesised `apply` or `copy` is available, and
 * the type is sealed, so no value of it can carry an amount finer than its currency quotes.
 *
 * @see [[Money]] for the type itself and for what a money value holds
 */
object Money {

  /** Reported when two money values of different currencies are added. */
  private val DifferentCurrenciesAddMessage: String =
    "Unable to add amounts in different currencies"

  /** Reported when two money values of different currencies are subtracted. */
  private val DifferentCurrenciesSubtractMessage: String =
    "Unable to subtract amounts in different currencies"

  /** Reported when a rate other than one is supplied for a conversion that does not convert. */
  private val NonUnitRateMessage: String =
    "FX rate must be 1 when no conversion required"

  /** The tolerance within which a rate counts as one for a conversion that does not convert. */
  private val NoConversionTolerance: Double = 1e-8

  /**
   * The separator between the currency code and the amount in the text form.
   *
   * A single space, as [[Money.toString]] writes it and as [[Money.parse]] locates it. The text
   * form has exactly two parts either side of that one separator, which is the whole of the
   * grammar, and the parse tests that shape by index rather than by splitting the text: a
   * constant naming the number of parts would describe an intermediate the parse no longer
   * builds, so none is declared.
   */
  private val TextSeparator: String = " "

  /**
   * The longest the amount part of the text form may be.
   *
   * This is the ceiling [[com.opengamma.strata.collect.Decimal]] already puts on the numeral it
   * reads, restated here so that it can be applied before the numeral is copied out of the text
   * rather than after. Nothing about what is accepted changes: a numeral of this length or less
   * reaches the decimal exactly as before, and one longer was refused by the decimal and is
   * refused here, with the same wording either way. What changes is that a tail of a sender's
   * choosing is no longer copied in order to be measured (CWE-400/CWE-770).
   */
  private val MaxAmountTextLength: Int = 256

  /**
   * Obtains a zero money value in the specified currency.
   *
   * Zero is an amount every currency admits at every scale, so this is total.
   *
   * @param currency  the currency the value is in
   * @return the zero value in that currency
   */
  def zero(currency: Currency): Money = create(currency, Decimal.ZERO)

  /**
   * Obtains a money value from a [[CurrencyAmount]], rounding the amount to the currency's minor
   * units.
   *
   * This answers with an outcome because an amount is permitted to be infinite - that type
   * accepts the two infinities and rejects only a value that is not a number - while no decimal
   * holds a value that is not finite. An infinite amount is therefore not a money value, and
   * saying so in the signature is what keeps the rejection visible instead of raising from an
   * accessor later. An amount whose magnitude needs more than eighteen digits is rejected for the
   * same reason.
   *
   * {{{
   * CurrencyAmount.of(Currency.GBP, 12.345d).flatMap(amount => Money.of(amount))   // GBP 12.35
   * }}}
   *
   * @param currencyAmount  the amount to round into a money value
   * @return the money value, or the failure describing why the amount is not one
   */
  def of(currencyAmount: CurrencyAmount): FailureOr[Money] =
    Decimal.of(currencyAmount.amount).map(decimal => create(currencyAmount.currency, decimal))

  /**
   * Obtains a money value from a [[BigMoney]], narrowing the amount to the currency's minor
   * units.
   *
   * This is total: the amount of a `BigMoney` is already a decimal, so the narrowing rounds it -
   * half up, away from zero at a tie, at the width the currency quotes - rather than refusing it.
   * `RON 200.2345` becomes `RON 200.23`, `BHD 1234.567890123457` becomes `BHD 1234.568` because
   * that currency quotes three digits, and `JPY 1234.567890123457` becomes `JPY 1235` because the
   * yen quotes none. A value already within the minor units of its currency is unchanged, which
   * is what makes widening and narrowing a round trip.
   *
   * [[BigMoney.toMoney]] is this conversion named from the other side and produces the same
   * value, and [[Money.toBigMoney]] is its inverse.
   *
   * {{{
   * Decimal.of("200.2345").map(amount => Money.of(BigMoney.of(Currency.RON, amount)))
   * // Right(RON 200.23) - the decimal factory reads the text, and both conversions are total
   * }}}
   *
   * @param money  the arbitrary-precision value to narrow
   * @return the money value, with its amount rounded to the currency's minor units
   */
  def of(money: BigMoney): Money = create(money.currency, money.amount)

  /**
   * Obtains a money value from a currency and a `Double` amount, rounding to the currency's
   * minor units.
   *
   * The amount is read as the decimal its shortest text names, so `0.1` is a tenth exactly and
   * not the binary value a tenth is stored as. It fails for a value no decimal holds, which is
   * one that is not finite and one needing more than eighteen digits.
   *
   * {{{
   * Money.of(Currency.USD, 63.347d)   // Right(USD 63.35)
   * Money.of(Currency.JPY, 63.347d)   // Right(JPY 63)
   * Money.of(Currency.USD, 1d / 0d)   // Left - no decimal is infinite
   * }}}
   *
   * @param currency  the currency the value is in
   * @param amount  the amount of that currency
   * @return the money value, or the failure describing why the number is not an amount
   */
  def of(currency: Currency, amount: Double): FailureOr[Money] =
    Decimal.of(amount).map(decimal => create(currency, decimal))

  /**
   * Obtains a money value from a currency and a `BigDecimal` amount, rounding to the currency's
   * minor units.
   *
   * Precision beyond eighteen digits is truncated towards zero, as [[Decimal]] truncates it, and
   * a value too large for a decimal to hold is reported as a failure.
   *
   * @param currency  the currency the value is in
   * @param amount  the amount of that currency
   * @return the money value, or the failure describing why the number is not an amount
   */
  def of(currency: Currency, amount: BigDecimal): FailureOr[Money] =
    Decimal.of(amount).map(decimal => create(currency, decimal))

  /**
   * Obtains a money value from a currency and an exact decimal amount, rounding to the
   * currency's minor units.
   *
   * This is total: a decimal is already a value money can hold, so there is nothing left to
   * reject - the rounding narrows the amount rather than refusing it. It is consequently the
   * factory the other routes end at, the one the JSON decoder uses, and the one a neighbouring
   * type narrows through.
   *
   * {{{
   * Money.of(Currency.USD, exactAmount)   // rounded half up to two decimal places
   * Money.of(Currency.BHD, exactAmount)   // rounded half up to three
   * }}}
   *
   * @param currency  the currency the value is in
   * @param amount  the amount of that currency
   * @return the money value, with its amount rounded to the currency's minor units
   */
  def of(currency: Currency, amount: Decimal): Money = create(currency, amount)

  /**
   * Parses a money value from text of the form `GBP 12.34`.
   *
   * The parsed form is the currency code, a space and the amount, which is the form
   * [[Money.toString]] writes. The text has to hold exactly one space, so text holding no space,
   * and text holding two, is rejected as malformed before anything is read from it. The two
   * wordings reported are:
   *
   *   - text that does not fall into exactly two space-separated parts is
   *     `Unable to parse amount, invalid format: <text>`;
   *   - text of the right shape that nonetheless names no value - because the code names no
   *     currency, or because the digits name no decimal - is `Unable to parse amount: <text>`.
   *
   * {{{
   * Money.parse("GBP 12.34")    // Right(GBP 12.34)
   * Money.parse("GBP 12.345")   // Right(GBP 12.35) - rounded, as every route is
   * Money.parse("GBP")          // Left - invalid format, one part
   * Money.parse("GBP 1 2")      // Left - invalid format, three parts
   * Money.parse("ZZZ 1")        // Left - right shape, names no currency
   * Money.parse("GBP NaN")      // Left - right shape, names no decimal
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
   * ===The shape is decided before anything is built from the text===
   *
   * The two parts are taken by locating the one separator rather than by splitting the text into
   * however many pieces it holds. Splitting made the cost of a rejection proportional to the
   * number of separators the text carried - text of a million spaces built a million strings,
   * every one of them to be discarded by the very next comparison - while locating the separator
   * decides the shape from two index scans and allocates at most the two substrings the grammar
   * has, whatever arrives (CWE-400/CWE-770). What the caller observes is unchanged: exactly the
   * text that fell into two space-separated parts before falls into them now, the empty second
   * part of `GBP ` included, and the same wording rejects everything else.
   *
   * @param amountStr  the money value as text, in the form `GBP 12.34`
   * @return the value the text names, or the failure naming the broken condition: the text has to
   *   be a currency code, one space and an amount a decimal holds
   */
  def parse(amountStr: String): FailureOr[Money] = {
    // The first separator, and then the absence of a second one, are the whole of the shape
    // check: text with no separator has one part and text with two has three, and neither is the
    // two parts the form has. A trailing separator is kept, so "GBP " is two parts, the second of
    // them empty, and is rejected for naming no decimal rather than for having the wrong shape.
    val separator = amountStr.indexOf(TextSeparator)
    if (separator < 0 || amountStr.indexOf(TextSeparator, separator + TextSeparator.length) >= 0) {
      Left(invalidFormat(amountStr))
    } else if (amountStr.length - (separator + TextSeparator.length) > MaxAmountTextLength) {
      // the ceiling the decimal would apply, applied before the numeral is copied rather than
      // after: the outcome is the one the decimal's own refusal produced, and the copy is not
      // made
      Left(Failure.Parsing(s"Unable to parse amount: $amountStr"))
    } else {
      val currencyCode = amountStr.substring(0, separator)
      val amountText = amountStr.substring(separator + TextSeparator.length)
      val parsed: Option[Money] = for {
        currency <- Currency.parse(currencyCode).toOption
        amount <- Decimal.parse(amountText).toOption
      } yield create(currency, amount)
      // the text is rendered rather than interpolated as it stands, which bounds the message and
      // keeps it to one line while leaving an in-bound spelling quoted as it was given
      parsed.toRight(Failure.Parsing(s"Unable to parse amount: $amountStr"))
    }
  }

  /**
   * Creates a money value, rounding the amount, which every route funnels through.
   *
   * This is the only instantiation of the type and it is private, so the routes above are the
   * only way into it from outside this file. Rounding here rather than in each of them is what
   * makes the invariant of the type hold for the results of the arithmetic as well: `plus`,
   * `minus`, `multipliedBy`, `map` and every factory reach this one step.
   *
   * Rounding is half up, away from zero at a tie, and is performed by the currency itself -
   * [[Currency.roundMinorUnits]] - so the number of digits kept and the direction of a tie are
   * decided in one place.
   *
   * @param currency  the currency
   * @param amount  the amount, rounded here to the currency's minor units
   * @return the money value
   */
  private def create(currency: Currency, amount: Decimal): Money =
    new Impl(currency, currency.roundMinorUnits(amount))

  /**
   * The one implementation of a money value.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it. It is declared rather than written as an anonymous subclass at the instantiation site
   * for two reasons, both about what the class file says: a private member class is one a Java
   * compiler refuses to name, where an anonymous class is public and can be instantiated directly
   * by a caller in another language, and a named class can be compared against, which is what
   * lets [[Money]] refuse in its own constructor to be any other implementation.
   *
   * @param currency  the currency
   * @param amount  the amount, already rounded to the currency's minor units by [[create]]
   */
  private final class Impl(currency: Currency, amount: Decimal)
      extends Money(currency, amount)

  /**
   * The failure reported for text whose shape does not admit a money value.
   *
   * The text is quoted as it stands; bounding it and escaping what it may hold belong to the
   * writing of a failure, which the text form of one and [[Failure.show]] perform for every part
   * they write.
   */
  private def invalidFormat(amountStr: String): Failure =
    Failure.Parsing(s"Unable to parse amount, invalid format: $amountStr")

  /**
   * The ordering, hashing and equality of money values.
   *
   * Values order by currency alphabetically and then by amount, the comparison
   * [[Money.compareTo]] describes. The ordering agrees with equality exactly - `compare` returns
   * zero precisely when `eqv` holds - because both fields compare as zero only when they are
   * equal, so no secondary comparison is needed to break a tie.
   *
   * Equality and hashing are those of the value itself: the currency compares by its code and
   * the amount is a normalised decimal that compares as a number. Nothing bit-level is involved,
   * as it is in [[CurrencyAmount]], whose `Double` amount forces it to compare bit patterns so
   * that a value that is not a number still equals itself. No override is written here for that
   * reason, and the hashing is the deterministic hash of the two fields, so the hash of a money
   * value is identical in every run of every program.
   *
   * This is the only equality-bearing instance of the type. `Order` and `Hash` both extend `Eq`,
   * so a separate `Eq` would be a second answer to the same question; one is declared here and
   * `Eq` is obtained from it by subtyping.
   *
   * @return the ordering and hashing of money values
   */
  implicit val order: Order[Money] with Hash[Money] =
    new Order[Money] with Hash[Money] {

      private val universal: Hash[Money] = Hash.fromUniversalHashCode[Money]

      override def compare(x: Money, y: Money): Int = x.compareTo(y)

      override def eqv(x: Money, y: Money): Boolean = universal.eqv(x, y)

      override def hash(x: Money): Int = universal.hash(x)
    }

  /**
   * The rendering of a money value, which is the text [[Money.toString]] produces.
   *
   * @return the rendering of a money value
   */
  implicit val show: Show[Money] = Show.show(_.toString)

  /**
   * The raw field shape both codecs of this type are derived over.
   *
   * A type without a public constructor cannot be derived over directly, and decoding one is two
   * steps - read the fields, then hand them to the factory that decides what they describe. This
   * product is the shape those fields have. It exists only for that purpose: it is private and it
   * is never returned, so no caller can hold an unrounded pair. Its field names are the JSON
   * keys, and they are the names of the two fields of [[Money]] itself, which keeps the derived
   * shape and the type from drifting apart.
   *
   * @param currency  the currency, read from its three letter code
   * @param amount  the amount, read unrounded from the canonical text of a decimal
   */
  private final case class Raw(currency: Currency, amount: Decimal) extends NoJavaSerialization

  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  private val rawEncoder: Encoder[Raw] = deriveEncoder[Raw]

  /**
   * The JSON encoding of money values.
   *
   * A value is an object of two fields, the currency as its code and the amount as the canonical
   * text of a decimal:
   *
   * {{{
   * {"currency":"GBP","amount":"12.34"}
   * }}}
   *
   * The amount is a string rather than a number because that is how a [[Decimal]] is written: a
   * JSON number is read back as a binary floating point value in most parsers, which is exactly
   * the representation this type exists to avoid. The text is the canonical form of the decimal,
   * so it carries the amount and not the presentation width - `USD 63.30` is written `"63.3"` -
   * and the width is recovered from the currency on the way back in.
   *
   * The shape is derived at compile time over the raw product above rather than written out field
   * by field, and the value is contramapped into that product: a value in memory is already
   * rounded, so nothing further has to be decided on the way out. The result is wrapped so that a
   * field holding no value is omitted; this type has no optional field, so the wrapping changes
   * nothing about its output and keeps it inside that one policy.
   *
   * @return the JSON encoding of a money value
   */
  implicit val encoder: Encoder[Money] =
    Codecs.dropNulls(rawEncoder.contramap[Money](value => Raw(value.currency, value.amount)))

  /**
   * The JSON decoding of money values.
   *
   * This is the inverse of the encoding above: the payload is read into the raw shape and handed
   * to the total factory, which rounds the amount to the minor units of the currency the payload
   * names. A payload whose amount is finer than its currency quotes is therefore accepted and
   * rounded rather than rejected, as every other route into this type rounds what it is given.
   *
   * The round trip is consequently exact in one direction: encoding is a function of the value,
   * and a value is always rounded already, so decoding what this encoder wrote returns the value
   * it was given and equal values encode to identical bytes. It is the other direction that
   * normalises - a hand-written document naming `"12.345"` in sterling decodes to `GBP 12.35` and
   * re-encodes as `"12.35"` - and that is a deliberate normalisation of input rather than an
   * inconsistency of the codec.
   *
   * The payload reaches the factory through the one construction gate every checking and every
   * rounding type decodes through, so this type is inside that policy rather than beside it:
   * whatever is added to the gate - a further check, a different way of reporting a rejection -
   * is inherited here without this file being touched. The factory the gate reaches is total, so
   * no payload of the right shape is refused at that point, and the rounding of a finer amount
   * described above still happens on the way in. What a document can get wrong is its fields: a
   * code naming no currency, or text naming no decimal, is rejected by the codecs of those two
   * types as the fields are read.
   *
   * @return the JSON decoding of a money value
   */
  implicit val decoder: Decoder[Money] =
    Codecs.validatedDecoder[Raw, Money](raw => Right(of(raw.currency, raw.amount)))(rawDecoder)
}
