/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import scala.collection.mutable

import cats.Hash
import cats.Monoid
import cats.Order
import cats.Show
import cats.syntax.traverse._

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * An amount of money in several currencies at once, such as `[GBP 100, USD 200]`.
 *
 * This is the container that holds a [[CurrencyAmount]] per currency. The amounts it holds are
 * not of equal worth and are not converted into one another while they are held: a value of
 * `[GBP 100, USD 200]` states that one hundred pounds and two hundred dollars are both owed, not
 * that either sum is the other's equivalent. Converting the whole of it into a single currency is
 * a separate, explicit step - [[convertedTo]] - which needs rates and therefore can fail.
 *
 * ===What it holds, and why that shape===
 *
 * Each currency occurs at most once, which makes this a map of currency to amount, and it is
 * held as exactly that: a `SortedMap[Currency, Double]` ordered by currency code. Three
 * consequences of holding a map are worth stating, because the rest of this type follows from
 * them:
 *
 *   - the no-duplicate rule is structural rather than checked, so no value of this type can
 *     exist that breaks it and no operation has to re-validate it;
 *   - the order of the amounts is the alphabetical order of their currency codes, always and
 *     everywhere - in [[getAmounts]], in [[iterator]], in [[toString]] and in the serialized
 *     form - so two values built from the same amounts in different orders are indistinguishable,
 *     down to the bytes they serialize to;
 *   - summing the amounts happens in that same fixed order, which matters because floating point
 *     addition is not associative: a total computed in currency order is reproducible, while one
 *     computed in the order the amounts happened to arrive is not.
 *
 * ===Combining: `of` refuses, `total` adds===
 *
 * The two ways of building a value from a collection of amounts differ in exactly one respect,
 * and the difference is deliberate on both sides:
 *
 * {{{
 * MultiCurrencyAmount.of(List(eur100, eur200, cad100))     // Left - "Currency is duplicated: EUR"
 * MultiCurrencyAmount.total(List(eur100, eur200, cad100))  // [CAD 100, EUR 300]
 * }}}
 *
 * [[MultiCurrencyAmount.of]] treats a repeated currency as a statement that something is wrong
 * with the data - two amounts of the same currency arriving as separate entries usually means a
 * key was lost somewhere upstream - and reports it. [[MultiCurrencyAmount.total]] treats a
 * repeated currency as arithmetic to be done, which is what aggregating a stream of cash flows
 * needs. Neither is the safe default for the other's use, so both are offered.
 *
 * ===Adding values of this type: the additive instance===
 *
 * This type carries a `Monoid`. It can carry one because adding two of these values cannot fail:
 * amounts of a currency present in both are added, amounts of a currency present in one are
 * carried across, and the identity is the value with no amounts at all. [[CurrencyAmount]]
 * deliberately carries no `Semigroup` or `Monoid` for the opposite reason - adding two
 * single-currency amounts fails when their currencies differ, and a `Semigroup` has nowhere to
 * report that.
 *
 * Two facts bound how exactly that instance is associative, and both are properties of the
 * arithmetic rather than of the instance:
 *
 *   - combining `+∞` with `−∞` produces a value that is not a number, which no amount may hold,
 *     so it raises the documented invariant of [[CurrencyAmount]] instead of yielding a value;
 *   - floating point addition is only approximately associative - `(a + b) + c` and
 *     `a + (b + c)` can differ in their last bit - so associativity holds to within the rounding
 *     of double arithmetic rather than exactly.
 *
 * ===Equality and ordering===
 *
 * Two values are equal when they hold the same currencies with amounts of the same bit pattern.
 * The bit comparison is made through the equality of [[CurrencyAmount]], and it differs from the
 * comparison a plain case class would have synthesised for two values: one that is not a number
 * equals itself, and a negative zero differs from a positive zero - the second being unreachable
 * here, because every route into this type normalises it away.
 *
 * There is deliberately no `Order`, because no ordering of these values means anything: neither
 * `[GBP 100]` nor `[USD 100]` is the greater, and comparing them by size or by code would invent
 * an answer. A caller that needs a reproducible sequence of them should sort by whatever
 * identifies them in its own domain.
 *
 * ===Which operations can fail===
 *
 * Reading the amount of a currency this value does not hold fails, because there is no amount to
 * return and zero would be an answer this type has not been told to give - [[getAmountOrZero]]
 * is the member that gives it. Converting into a single currency fails when a rate the conversion
 * needs is unavailable. Building from a collection fails on a repeated currency, and building
 * from a currency and a number fails for a number that is not an amount. Each of those depends on
 * the values involved rather than on the calling code, so each is an `Either`.
 *
 * The arithmetic - [[plus]], [[minus]], [[multipliedBy]], [[negated]], [[mapAmounts]],
 * [[mapCurrencyAmounts]] - is total in signature and shares the single numeric invariant of
 * [[CurrencyAmount]]: an amount that is not a number cannot be held, and an operation that would
 * produce one raises that invariant - literally, by routing the result through
 * [[CurrencyAmount]] itself - rather than widening every operation into a failure channel.
 * Reaching it requires infinite operands or a mapping function that produces a value that is not
 * a number.
 *
 * This type is immutable and thread-safe: a value of it can be shared freely, and every operation
 * returns a new value rather than changing the one it was called on.
 *
 * @param amounts  the amount held per currency, each currency at most once, ordered by currency
 *   code, every amount normalised so that none is a negative zero and none is a value that is not
 *   a number
 * @see [[CurrencyAmount]] for an amount in a single currency
 * @see [[FxRateProvider]] for the source of the rates [[convertedTo]] uses
 * @see [[MultiCurrencyAmount.total]] for the aggregating factory and
 *   [[MultiCurrencyAmount.of]] for the one that refuses a repeated currency
 */
sealed abstract case class MultiCurrencyAmount private (amounts: SortedMap[Currency, Double])
    extends FxConvertible[CurrencyAmount]
    with NoJavaSerialization {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means can be
  // stopped is here. The single implementation is the companion's hidden `Impl`.
  JvmClosure.requireSoleImplementation(this, classOf[MultiCurrencyAmount.Impl])

  // The invariant of this type, stated over the map the instance actually holds rather than over
  // the entries a factory was given, because the implementation class carries a public
  // constructor in the class file whatever the source asked for: a class compiled outside this
  // library can call it directly, and identity alone would then admit a value holding numbers no
  // amount may hold. That a currency appears at most once needs no statement - the representation
  // is a map keyed by currency - so what is stated is what every route adds to it: each number
  // goes through the invariant of an amount, which refuses a value that is not a number and
  // normalises a negative zero, so every number held is one a `CurrencyAmount` could hold.
  //
  // The map is walked once, which is the cost the factory that builds it already pays.
  JvmClosure.requireInvariant(
    "every amount it holds is a number",
    amounts.forall { case (_, amount) => !amount.isNaN })
  JvmClosure.requireInvariant(
    "every amount it holds is a positive zero where it is zero, a negative zero being normalised",
    // the two zeroes compare equal, so the bit pattern is what tells them apart, exactly as it
    // does for the single amount of a `CurrencyAmount`
    amounts.forall { case (_, amount) =>
      amount != 0d || java.lang.Double.doubleToLongBits(amount) == 0L
    })

  /**
   * Gets the amounts held, as a set ordered by currency and then by amount.
   *
   * The ordering is that of [[CurrencyAmount]], which compares the currency first, and since each
   * currency occurs at most once the amounts never have to be compared to break a tie.
   *
   * The set is built from the map this value holds rather than stored, which is what keeps the
   * single representation single. A caller that only wants to walk the amounts should prefer
   * [[iterator]], which builds nothing; a caller that wants the numbers keyed by currency should
   * prefer [[toMap]].
   *
   * @return the amounts held, ordered by currency
   */
  def getAmounts: SortedSet[CurrencyAmount] =
    SortedSet.from(iterator)(MultiCurrencyAmount.currencyAmountOrdering)

  /**
   * Gets the currencies this value holds an amount for.
   *
   * The set iterates in the alphabetical order of the codes, since that is the order of the map
   * behind it, so a report built from it is reproducible.
   *
   * @return the currencies held
   */
  def getCurrencies: Set[Currency] = amounts.keySet

  /**
   * Gets the number of amounts held, which is the number of distinct currencies.
   *
   * @return the number of amounts held
   */
  def size: Int = amounts.size

  /**
   * Checks whether this value holds an amount for the specified currency.
   *
   * An amount of zero counts as held: this answers whether the currency is present, not whether
   * what it holds is non-zero.
   *
   * @param currency  the currency to look for
   * @return true when an amount of that currency is held
   */
  def contains(currency: Currency): Boolean = amounts.contains(currency)

  /**
   * Gets the amount of the specified currency.
   *
   * A currency this value does not hold is reported rather than answered with zero, which is the
   * distinction between this member and [[getAmountOrZero]]: an absent currency may mean a
   * currency that was never involved, and silently reading it as zero would hide that. Whether it
   * happens depends on the value and the currency a caller holds, so it is reported as a failure:
   *
   * {{{
   * multi.getAmount(Currency.GBP)   // Right(GBP 100)
   * multi.getAmount(Currency.CHF)   // Left(Failure.Invalid("Unknown currency CHF"))
   * }}}
   *
   * @param currency  the currency to read the amount of
   * @return the amount of that currency, or the failure naming the currency this value does not
   *   hold
   */
  def getAmount(currency: Currency): FailureOr[CurrencyAmount] =
    amounts
      .get(currency)
      .map(amount => CurrencyAmount.create(currency, amount))
      .toRight(MultiCurrencyAmount.unknownCurrency(currency))

  /**
   * Gets the amount of the specified currency, or zero of it when none is held.
   *
   * This is the total counterpart of [[getAmount]], for a caller whose arithmetic treats an
   * absent currency as nothing owed - totalling one currency across many of these values, for
   * instance.
   *
   * @param currency  the currency to read the amount of
   * @return the amount of that currency, or zero of it
   */
  def getAmountOrZero(currency: Currency): CurrencyAmount =
    amounts
      .get(currency)
      .fold(CurrencyAmount.zero(currency))(amount => CurrencyAmount.create(currency, amount))

  /**
   * Returns a copy of this value with the specified amount of the specified currency added.
   *
   * A currency this value already holds has the amount added to what it holds; a currency it does
   * not hold is added to it. The addition is ordinary `Double` arithmetic performed as what is
   * held plus what is supplied, in that operand order, so the rounding of the result is fixed by
   * this member rather than by the order a caller happens to write.
   *
   * {{{
   * gbp100.plus(Currency.GBP, 50d)   // [GBP 150]
   * gbp100.plus(Currency.USD, 50d)   // [GBP 100, USD 50]
   * }}}
   *
   * @param currency  the currency to add an amount of
   * @param amountToAdd  the amount of that currency to add
   * @return this value with the amount added
   * @throws java.lang.IllegalArgumentException if the amount supplied is not a number, or if the
   *   sum is not one, which requires infinite operands of opposite sign
   */
  def plus(currency: Currency, amountToAdd: Double): MultiCurrencyAmount =
    plus(CurrencyAmount.create(currency, amountToAdd))

  /**
   * Returns a copy of this value with the specified amount added.
   *
   * This is [[plus]] of a currency and a number with the two carried together, and it behaves
   * identically: the currency of the amount is added to or inserted into what this value holds.
   *
   * One amount arriving changes at most one entry, so this is a single merge into the map this
   * value holds rather than a traversal of it: the other entries are carried over by the sharing
   * of the immutable map, and no amount is built for any of them.
   *
   * @param amountToAdd  the amount to add
   * @return this value with the amount added
   * @throws java.lang.IllegalArgumentException if the sum is not a number, which requires
   *   infinite operands of opposite sign
   */
  def plus(amountToAdd: CurrencyAmount): MultiCurrencyAmount =
    MultiCurrencyAmount.instantiate(
      MultiCurrencyAmount.mergedAmount(amounts, amountToAdd.currency, amountToAdd.amount))

  /**
   * Returns a copy of this value with every amount of the specified value added.
   *
   * Each currency of the other value is added to or inserted into this one, so the result holds
   * the union of the two sets of currencies. This is the operation the additive instance of this
   * type combines with, and it is associative to the extent double addition is - see the note on
   * [[MultiCurrencyAmount.monoid]].
   *
   * {{{
   * // [GBP 100, USD 200] plus [EUR 75, USD 50]
   * // is [EUR 75, GBP 100, USD 250]
   * }}}
   *
   * The map this value holds is the map the other value's entries are merged into, one entry at a
   * time, which is what makes each sum `what is held plus what arrives` and what keeps the amounts
   * of a currency only one of the two values holds exactly the numbers they were - the entries of
   * this value are carried over by the sharing of the immutable map rather than read again, and
   * nothing is added to them.
   *
   * @param amountToAdd  the value whose amounts are to be added
   * @return this value with the other value's amounts added
   * @throws java.lang.IllegalArgumentException if any sum is not a number, which requires
   *   infinite operands of opposite sign
   */
  def plus(amountToAdd: MultiCurrencyAmount): MultiCurrencyAmount =
    MultiCurrencyAmount.instantiate(
      MultiCurrencyAmount.mergedInto(amounts, amountToAdd.amounts.iterator))

  /**
   * Returns a copy of this value with the specified amount of the specified currency subtracted.
   *
   * A currency this value already holds has the amount subtracted from what it holds; a currency
   * it does not hold is added to it ''negated'', which keeps subtraction the exact inverse of
   * addition:
   *
   * {{{
   * gbp100.minus(Currency.GBP, 50d)   // [GBP 50]
   * gbp100.minus(Currency.USD, 50d)   // [GBP 100, USD -50]
   * }}}
   *
   * The parameter is named for the addition this is written as, the amount being negated before
   * it is added.
   *
   * @param currency  the currency to subtract an amount of
   * @param amountToAdd  the amount of that currency to subtract
   * @return this value with the amount subtracted
   * @throws java.lang.IllegalArgumentException if the amount supplied is not a number, or if the
   *   difference is not one, which requires infinite operands of the same sign
   */
  def minus(currency: Currency, amountToAdd: Double): MultiCurrencyAmount =
    plus(CurrencyAmount.create(currency, -amountToAdd))

  /**
   * Returns a copy of this value with the specified amount subtracted.
   *
   * The amount is negated and added, so a currency this value does not hold is inserted with the
   * negated amount.
   *
   * @param amountToSubtract  the amount to subtract
   * @return this value with the amount subtracted
   * @throws java.lang.IllegalArgumentException if the difference is not a number, which requires
   *   infinite operands of the same sign
   */
  def minus(amountToSubtract: CurrencyAmount): MultiCurrencyAmount = plus(amountToSubtract.negated)

  /**
   * Returns a copy of this value with every amount of the specified value subtracted.
   *
   * The other value is negated and added, so a currency only it holds appears in the result with
   * its amount negated.
   *
   * @param amountToSubtract  the value whose amounts are to be subtracted
   * @return this value with the other value's amounts subtracted
   * @throws java.lang.IllegalArgumentException if any difference is not a number, which requires
   *   infinite operands of the same sign
   */
  def minus(amountToSubtract: MultiCurrencyAmount): MultiCurrencyAmount =
    plus(amountToSubtract.negated)

  /**
   * Returns a copy of this value with every amount multiplied by the specified factor.
   *
   * The currencies are untouched, so the result holds exactly the currencies this value holds.
   * The multiplication is written as the amount times the factor, in that operand order, so the
   * rounding of a product is fixed by this member rather than by the order a caller happens to
   * write.
   *
   * @param factor  the factor to multiply every amount by
   * @return this value with every amount multiplied
   * @throws java.lang.IllegalArgumentException if any product is not a number, which requires an
   *   infinite amount and a zero factor
   */
  def multipliedBy(factor: Double): MultiCurrencyAmount = mapAmounts(amount => amount * factor)

  /**
   * Returns a copy of this value with every amount negated.
   *
   * An amount of zero negates to zero rather than to a negative zero. That case is handled here
   * even though construction would normalise the sign away in any event, so that the arithmetic
   * and the normalisation each remain correct on their own.
   *
   * @return this value with every amount negated
   */
  def negated: MultiCurrencyAmount = mapAmounts(amount => if (amount == 0d) 0d else -amount)

  /**
   * Returns a copy of this value with the specified operation applied to every amount.
   *
   * This is the general form of the arithmetic above, for an operation this type does not offer:
   *
   * {{{
   * base.mapAmounts(value => value * value)
   * }}}
   *
   * The operation receives one amount at a time and cannot change its currency, so the result
   * holds exactly the currencies this value holds and no amount can merge into another.
   *
   * @param mapper  the operation to apply to every amount
   * @return this value with the operation applied to every amount
   * @throws java.lang.IllegalArgumentException if the operation produces a value that is not a
   *   number
   */
  def mapAmounts(mapper: Double => Double): MultiCurrencyAmount =
    MultiCurrencyAmount.create(amounts.iterator.map { case (currency, amount) =>
      (currency, mapper(amount))
    })

  /**
   * Returns a copy of this value with the specified operation applied to every amount, currency
   * included.
   *
   * The operation is called once per currency held and may return an amount of a ''different''
   * currency, which is the whole difference between this member and [[mapAmounts]]. Two amounts
   * mapped onto the same currency are therefore added together rather than rejected - the result
   * is the total of the mapped amounts:
   *
   * {{{
   * // [GBP 100, USD 200] with every amount mapped into EUR
   * // is [EUR 300], not a failure about a duplicated currency
   * }}}
   *
   * @param operator  the operation to apply to every amount
   * @return the total of the mapped amounts
   * @throws java.lang.IllegalArgumentException if any total is not a number, which requires
   *   infinite operands of opposite sign
   */
  def mapCurrencyAmounts(operator: CurrencyAmount => CurrencyAmount): MultiCurrencyAmount =
    MultiCurrencyAmount.merged(iterator.map(operator))

  /**
   * Returns an iterator over the amounts held, ordered by currency.
   *
   * This is the traversal that materialises nothing, for a caller that walks the amounts once. A
   * caller that wants a collection can ask for one, `iterator.toList` and [[getAmounts]] being
   * the two obvious ways.
   *
   * The iterator reads the map this value holds, which no operation ever modifies, so it cannot
   * observe a change part-way through a traversal.
   *
   * Each amount is built as the traversal reaches it, through the construction path
   * [[CurrencyAmount]] publishes to this package, which performs the very same normalisation and
   * check as its public factories and allocates exactly the amount it returns. A traversal
   * therefore allocates one object per amount handed out and nothing besides. The members of this
   * type that aggregate do not traverse this way at all - they read the numbers of the map
   * directly, since an amount they would build would only be unwrapped again.
   *
   * @return the amounts held, in the alphabetical order of their currency codes
   */
  def iterator: Iterator[CurrencyAmount] =
    amounts.iterator.map { case (currency, amount) =>
      CurrencyAmount.create(currency, amount)
    }

  /**
   * Converts every amount held into the specified currency and totals them.
   *
   * This is the [[FxConvertible]] implementation of this type, and it is the reason that trait is
   * parameterised: converting a value that holds several currencies collapses it into a single
   * [[CurrencyAmount]], not into another value of this type.
   *
   * There are two branches, and they differ in an observable way:
   *
   *   - a value holding exactly one amount converts that amount through
   *     [[CurrencyAmount.convertedTo]], which returns it unchanged when it is already in the
   *     requested currency and consequently succeeds under a provider that holds no rates at
   *     all;
   *   - a value holding no amount, or more than one, asks the provider for a rate for ''every''
   *     amount it holds, including an amount already in the requested currency. Such a
   *     conversion therefore needs a provider that answers for a currency against itself, which
   *     is what [[FxRateProvider.minimal]] is for. A value holding nothing converts to zero of
   *     the requested currency.
   *
   * The converted amounts are totalled in the alphabetical order of their currency codes,
   * starting from zero. Floating point addition is order-sensitive, so stating the order is what
   * makes this total reproducible from one run to the next.
   *
   * The second branch is a single traversal of the map, carrying the total as a number from one
   * amount to the next: each amount is converted and added where it is read, so nothing between
   * the map and the result is held - neither a collection of the entries nor one of the converted
   * numbers. The total is the number an amount is finally built from through
   * [[CurrencyAmount.of]], so a total that is not a number is reported rather than raised: it can
   * only arise from rates and amounts a caller supplied.
   *
   * A single unavailable rate fails the whole conversion, carrying the failure the provider
   * reported, and no later rate is asked for - a partial total would be a number with no meaning.
   * The traversal returns at the amount whose rate was refused, so the number of lookups a failing
   * conversion costs is the number of amounts up to and including that one.
   *
   * @param resultCurrency  the currency to convert every amount into
   * @param rateProvider  the provider of FX rates
   * @return the total of the converted amounts, or the failure the provider reported for the
   *   first rate the conversion needed and could not get
   */
  override def convertedTo(
      resultCurrency: Currency,
      rateProvider: FxRateProvider): FailureOr[CurrencyAmount] =
    if (amounts.size == 1) {
      val (currency, amount) = amounts.head
      CurrencyAmount.create(currency, amount).convertedTo(resultCurrency, rateProvider)
    } else {
      MultiCurrencyAmount.totalConverted(amounts.iterator, resultCurrency, rateProvider, 0d)
    }

  /**
   * Converts this value to a map of currency to amount.
   *
   * The map is the representation this type holds, handed back as it is: it is immutable, it is
   * ordered by currency code, and its values are the normalised amounts described on this type.
   * No copy is made, because there is nothing a caller could do to it that would affect this
   * value.
   *
   * @return the amount held per currency, ordered by currency code
   */
  def toMap: SortedMap[Currency, Double] = amounts

  /**
   * Checks whether this value equals another object.
   *
   * Another value of this type is equal when it holds the same currencies and, for each of them,
   * an amount with the same bit pattern. The bit comparison is the one the equality of
   * [[CurrencyAmount]] makes, and it differs from the comparison a case class would have
   * synthesised for two values: one that is not a number, which here equals itself, and a
   * negative zero, which here differs from a positive zero and which construction ensures no
   * amount holds. An object of any other type is not equal.
   *
   * Because the currencies are held as a map, two values built from the same amounts in different
   * orders hold the same map and are equal without anything having to be sorted here.
   *
   * @param obj  the object to compare to
   * @return true if the other object holds the same currencies with the same amounts
   */
  override def equals(obj: Any): Boolean = obj match {
    case other: MultiCurrencyAmount =>
      (this eq other) ||
        (amounts.size == other.amounts.size &&
          amounts.forall { case (currency, amount) =>
            other.amounts
              .get(currency)
              .exists(otherAmount => java.lang.Double.compare(amount, otherAmount) == 0)
          })
    case _ => false
  }

  /**
   * Returns a hash code consistent with [[equals]].
   *
   * Each entry contributes the hash of its currency and the hash of its amount, mixed with the
   * usual odd prime, and the entries contribute in currency order so that the result does not
   * depend on how the value was built. The amount is hashed by its bit pattern, so two values
   * that [[equals]] calls equal always agree here too. Every part of it is a function of the
   * value alone, so the hash of a value is identical in every run of every program.
   *
   * @return the hash code of the currencies and amounts held
   */
  override def hashCode: Int =
    amounts.iterator.foldLeft(1) { case (accumulated, (currency, amount)) =>
      accumulated * 31 + (currency.hashCode * 31 + java.lang.Double.hashCode(amount))
    }

  /**
   * Returns the formatted string form of this value.
   *
   * The form is the amounts in currency order, separated by commas and enclosed in square
   * brackets - `[GBP 100, USD 200]`. Each amount is written as [[CurrencyAmount.toString]] writes
   * it, so a whole number has no fractional part and a value holding nothing renders as `[]`.
   *
   * @return the formatted amounts
   */
  override def toString: String = iterator.mkString("[", ", ", "]")
}

/**
 * Provides the ways of obtaining a multi-currency amount and the instances for the type.
 *
 * This companion is the only place a [[MultiCurrencyAmount]] is created. Every route into the
 * type either checks its input and reports what it rejected - as [[of]] does for a repeated
 * currency and for a number that is not an amount - or is reached from values that are already
 * amounts, as [[total]] and the arithmetic of the type are. Neither the constructor nor a
 * generated `apply` or `copy` is available, and the type is sealed, so a value holding the same
 * currency twice, an amount that is not a number, or a negative zero cannot be built.
 *
 * One route is visible to the currency package rather than to this file alone - [[create]], which
 * builds a value from a map of currency to number and applies the invariant of an amount to every
 * number it is given. It is what [[MultiCurrencyAmountArray]] reconstructs a single scenario
 * through, and it is not part of the published API of the module. The trusted instantiation behind
 * it stays private to this file, so the checks cannot be stepped around from anywhere.
 *
 * @see [[MultiCurrencyAmount]] for the type itself, the difference between [[of]] and [[total]],
 *   and the bounds on the associativity of [[monoid]]
 */
object MultiCurrencyAmount {

  /**
   * The ordering the map of this type is built with.
   *
   * It is the order [[Currency]] publishes, converted to the ordering the standard library's
   * sorted collections require, so that the order of the amounts in every value of this type,
   * in its rendering and in its serialized form is the one order of currencies this library
   * defines rather than a second one declared here.
   */
  private val currencyOrdering: Ordering[Currency] = Order[Currency].toOrdering

  /**
   * The ordering the set answered by [[MultiCurrencyAmount.getAmounts]] is built with.
   *
   * It is the order [[CurrencyAmount]] publishes - currency first, then amount. Since a value of
   * this type holds each currency at most once, the amounts are never reached.
   */
  private val currencyAmountOrdering: Ordering[CurrencyAmount] = Order[CurrencyAmount].toOrdering

  /**
   * The empty map the single-entry routes of this file start from.
   *
   * It is held rather than built at each use for the reason any empty immutable collection is:
   * there is exactly one of it, it is shared by every use, and building it again would only
   * construct the same value. It also fixes the ordering of every map this type holds at one
   * place - the ordering above - so a map that grows by insertion and a map that is assembled in
   * bulk cannot be ordered differently.
   *
   * The routes that add exactly one entry - [[empty]] itself, and [[of]] of a currency and a
   * number - start from it and insert into it, because one insertion into an empty tree is the
   * whole of the work. The routes that assemble a whole collection do not: they accumulate into a
   * buffer and seal it in one step, for the reason [[distinct]] records.
   */
  private val noAmounts: SortedMap[Currency, Double] =
    SortedMap.empty[Currency, Double](currencyOrdering)

  /**
   * The value that holds no amount at all.
   *
   * This is the identity of [[monoid]]: adding it to any value yields that value. It renders as
   * `[]`, its size is zero, and converting it into any currency gives zero of that currency.
   *
   * @return the value holding no amounts
   */
  val empty: MultiCurrencyAmount = instantiate(noAmounts)

  /**
   * Obtains a value holding a single amount, of the specified currency and number.
   *
   * The number has to be an amount: a value that is not a number is rejected and a negative zero
   * is normalised to a positive zero, exactly as [[CurrencyAmount.of]] decides it, because this
   * factory is that one with the result wrapped. Whether the rejection happens depends on the
   * number a caller holds rather than on the calling code, so it is reported as a failure - the
   * same failure [[CurrencyAmount.of]] reports, so the two routes cannot disagree about what an
   * amount is.
   *
   * {{{
   * MultiCurrencyAmount.of(Currency.GBP, 100d)         // Right([GBP 100])
   * MultiCurrencyAmount.of(Currency.GBP, Double.NaN)   // Left(Failure.Invalid(…))
   * }}}
   *
   * @param currency  the currency of the amount
   * @param amount  the number of that currency
   * @return the value holding that single amount, or the failure describing why the number is not
   *   an amount
   */
  def of(currency: Currency, amount: Double): FailureOr[MultiCurrencyAmount] =
    CurrencyAmount
      .of(currency, amount)
      .map(checked => instantiate(noAmounts.updated(checked.currency, checked.amount)))

  /**
   * Obtains a value from the specified amounts, rejecting a repeated currency.
   *
   * This is the varargs form of the factory below and behaves identically, except that no
   * argument at all yields [[empty]] without anything being examined.
   *
   * {{{
   * MultiCurrencyAmount.of(gbp100, usd200)   // Right([GBP 100, USD 200])
   * MultiCurrencyAmount.of()                 // Right([])
   * }}}
   *
   * @param amounts  the amounts, each of a different currency
   * @return the value holding those amounts, or the failure naming the first currency that
   *   appeared twice
   */
  def of(amounts: CurrencyAmount*): FailureOr[MultiCurrencyAmount] =
    if (amounts.isEmpty) Right(empty) else of(amounts.toList)

  /**
   * Obtains a value from the specified amounts, rejecting a repeated currency.
   *
   * Each amount has to be of a different currency. A repeated currency is a statement about the
   * data rather than about the calling code - two entries of one currency usually mean a key was
   * lost upstream - so it is reported as a failure, and the traversal stops where the repeat was
   * found:
   *
   * {{{
   * MultiCurrencyAmount.of(List(gbp100, usd200))   // Right([GBP 100, USD 200])
   * MultiCurrencyAmount.of(List(gbp100, gbp200))   // Left(Failure.Invalid("Currency is duplicated: GBP"))
   * MultiCurrencyAmount.of(Nil)                    // Right([])
   * }}}
   *
   * [[total]] is the factory to use when a repeated currency should be added up instead. The two
   * are deliberately separate; neither is a safe default for the other's use.
   *
   * The failure carries no attributes, which keeps two failures over the same currency equal and
   * therefore directly comparable, and its message names the currency by its code.
   *
   * @param amounts  the amounts, each of a different currency, consumed once and no further than
   *   the first repeated currency
   * @return the value holding those amounts, or the failure naming the first currency that
   *   appeared twice
   */
  def of(amounts: Iterable[CurrencyAmount]): FailureOr[MultiCurrencyAmount] =
    distinct(amounts.iterator)

  /**
   * Obtains a value from the specified map of currency to number.
   *
   * A map cannot hold a key twice, so no currency can be repeated and the only thing left to
   * decide is whether each number is an amount - which it is unless it is a value that is not a
   * number, rejected here as [[CurrencyAmount.of]] rejects it. That rejection is reported as a
   * failure, consistently with the factory taking a currency and a number above.
   *
   * The entries are examined in the alphabetical order of their currency codes rather than in
   * whatever order the map supplied happens to iterate in, so the failure reported for a map with
   * several unacceptable numbers is always the one for the earliest currency and never depends on
   * the kind of map that was passed.
   *
   * The amounts are then handed to the factory above. The duplicate check it performs cannot fire
   * for a map, and it is taken anyway so that there is one assembly of this type from amounts
   * rather than two that could drift apart.
   *
   * @param map  the number held per currency
   * @return the value holding those amounts, or the failure describing the first number that is
   *   not an amount
   */
  def of(map: Map[Currency, Double]): FailureOr[MultiCurrencyAmount] =
    SortedMap
      .from(map)(currencyOrdering)
      .toList
      .traverse { case (currency, amount) => CurrencyAmount.of(currency, amount) }
      .flatMap(amounts => of(amounts))

  /**
   * Obtains a value from the total of the specified amounts, adding up a repeated currency.
   *
   * Amounts of the same currency are added together, so this is the factory for aggregating a
   * collection whose entries were never keyed by currency - a list of cash flows, the amounts of
   * several legs of a trade, the results of a calculation performed per instrument:
   *
   * {{{
   * MultiCurrencyAmount.total(List(eur100, eur200, cad100))   // [CAD 100, EUR 300]
   * MultiCurrencyAmount.total(Nil)                            // []
   * }}}
   *
   * It cannot fail: there is nothing about a collection of amounts that this refuses, which is
   * why it returns a value rather than an `Either`. [[of]] is the factory that refuses a repeated
   * currency instead.
   *
   * Amounts of one currency are added in the order they arrive, starting from the first of them,
   * so the rounding of a total is fixed by the order of the collection. This member and
   * `Monoid[MultiCurrencyAmount].combineAll` are the two ways of aggregating.
   *
   * @param amounts  the amounts to total, of any currencies, consumed once
   * @return the value holding the total per currency
   * @throws java.lang.IllegalArgumentException if any total is not a number, which requires
   *   infinite operands of opposite sign
   */
  def total(amounts: Iterable[CurrencyAmount]): MultiCurrencyAmount = merged(amounts)

  /**
   * Accumulates amounts of distinct currencies, stopping at the first currency that repeats.
   *
   * The amounts arrive as an iterator and are pulled one at a time, so a repeat stops the
   * traversal where it is found and nothing after it is read - which is what lets the factory
   * above promise that a failing call leaves the rest of a single-use collection untouched.
   * Presence is asked of each currency before it is added, because a later amount would otherwise
   * silently replace an earlier one, which is precisely the condition being reported.
   *
   * The recursion is in tail position and runs as a loop, so a collection of any size is
   * traversed without consuming stack. The map the traversal accumulates into is the map of the
   * value returned, handed to [[instantiate]] once it is sealed: every number in it came out of a
   * [[CurrencyAmount]], so it is already normalised and already within the invariant, and
   * re-deciding it would only build a second map to arrive at the same one.
   *
   * ===How the map is built: accumulate into a buffer, then seal it===
   *
   * This is the shape every route of this file that assembles a ''collection'' uses - this one,
   * [[mergedAmounts]], [[mergedEntries]] and [[create]] - and it is recorded here once:
   *
   *   - the accumulator is a local `mutable.TreeMap` built with [[currencyOrdering]], the very
   *     ordering instance this type holds its maps by, and each entry is written into it with
   *     `update`. Writing an entry therefore costs one node, where inserting into a persistent
   *     red-black tree copies the whole path from the root - so assembling a value of `c`
   *     currencies allocates the `c` nodes it needs rather than the `c log c` a fold of
   *     `updated` allocates, which is the one order of complexity this port had over the Java
   *     original;
   *   - the buffer is then sealed by `SortedMap.from(buffer)(currencyOrdering)`. That reaches
   *     the ordered-entries construction of `scala.collection.immutable.TreeMap`: a
   *     `mutable.TreeMap` ''is'' a `scala.collection.SortedMap`, and its ordering is the same
   *     instance, so the factory recognises the source as already sorted and builds the
   *     immutable tree from its entries in one linear pass instead of re-inserting them one at a
   *     time. Sealing an unsorted source - a `Vector`, an `Array`, a `mutable.HashMap` - would
   *     miss that branch and pay for the per-entry insertions after all, which is why the buffer
   *     is a sorted mutable map and not any cheaper container;
   *   - the buffer is a `val` local to the private method that fills it. It is never returned,
   *     never stored and never named by any signature, so no value of this type and no caller
   *     can reach it, and the map that leaves the method is an immutable one that nothing else
   *     holds a reference to. That is what keeps this type immutable and safely shared while the
   *     assembly of it is not.
   *
   * @param amounts  the amounts to examine, pulled one at a time and no further than the first
   *   repeated currency
   * @return the value holding the accumulated amounts, or the failure naming the repeated currency
   */
  private def distinct(amounts: Iterator[CurrencyAmount]): FailureOr[MultiCurrencyAmount] = {
    val buffer: mutable.TreeMap[Currency, Double] =
      mutable.TreeMap.empty[Currency, Double](currencyOrdering)

    @tailrec
    def accumulate(remaining: Iterator[CurrencyAmount]): FailureOr[MultiCurrencyAmount] =
      if (!remaining.hasNext) {
        Right(instantiate(SortedMap.from(buffer)(currencyOrdering)))
      } else {
        val next = remaining.next()
        // one walk of the buffer decides both questions: the insertion answers what was held for
        // the currency before it, which is nothing for a currency arriving for the first time, so
        // testing membership first and inserting afterwards would walk the tree twice per amount
        if (buffer.put(next.currency, next.amount).isDefined) {
          // the traversal stops here: the outcome is settled, and reading the rest of a
          // single-use collection would consume it for nothing. What the insertion just wrote is
          // discarded with the buffer, which no value of this type is built from on this path
          Left(duplicateCurrency(next.currency))
        } else {
          accumulate(remaining)
        }
      }

    accumulate(amounts)
  }

  /**
   * Assembles a value from amounts of any currencies, adding up those of the same currency.
   *
   * This is the merging aggregation that [[total]] and
   * [[MultiCurrencyAmount.mapCurrencyAmounts]] are written in terms of, so the way a collection of
   * amounts combines is stated once. [[mergedAmounts]] accumulates the collection into one map and
   * [[instantiate]] takes that very map as the map of the value returned, so an aggregation of any
   * number of amounts builds exactly one map and no intermediate amount.
   *
   * @param amounts  the amounts to combine, of any currencies, consumed once
   * @return the value holding the total per currency
   * @throws java.lang.IllegalArgumentException if any total is not a number
   */
  private def merged(amounts: IterableOnce[CurrencyAmount]): MultiCurrencyAmount =
    instantiate(mergedAmounts(amounts.iterator))

  /**
   * Merges the amounts of a collection into one map, adding up a repeated currency.
   *
   * The amounts arrive as an iterator and are pulled one at a time. The recursion is in tail
   * position and runs as a loop, so a collection of any size is aggregated without consuming
   * stack, and the map is assembled in the accumulate-then-seal shape [[distinct]] documents -
   * one node per currency held rather than a copied tree path per amount that arrives.
   *
   * How two amounts of one currency combine is what [[mergedAmount]] states for the single-amount
   * route, and this loop states the same thing over the buffer: a currency the buffer does not
   * hold is written with the number as it stands, since it arrived as the amount of a
   * [[CurrencyAmount]] and is therefore already normalised and already within the invariant; a
   * currency it holds is written `accumulated + arriving`, in that operand order, because
   * floating point addition rounds and the reverse order can differ in the last bit. The sum is
   * the one number that has not been decided yet, so it - and only it - goes through the
   * number-level form of the invariant that [[CurrencyAmount]] publishes to this package, which
   * keeps the invariant of an amount defined in one place in this library.
   *
   * @param amounts  the amounts to merge, of any currencies, consumed once
   * @return the total per currency once the collection is exhausted
   * @throws java.lang.IllegalArgumentException if any total is not a number
   */
  private def mergedAmounts(amounts: Iterator[CurrencyAmount]): SortedMap[Currency, Double] = {
    val buffer: mutable.TreeMap[Currency, Double] =
      mutable.TreeMap.empty[Currency, Double](currencyOrdering)

    @tailrec
    def accumulate(remaining: Iterator[CurrencyAmount]): Unit =
      if (remaining.hasNext) {
        val arriving = remaining.next()
        buffer.update(
          arriving.currency,
          buffer
            .get(arriving.currency)
            .fold(arriving.amount)(held => CurrencyAmount.checkedAmount(held + arriving.amount)))
        accumulate(remaining)
      }

    accumulate(amounts)
    SortedMap.from(buffer)(currencyOrdering)
  }

  /**
   * Merges the entries of a map of currency to number into one map.
   *
   * This is [[mergedAmounts]] over the raw entries of a value of this type rather than over
   * amounts, and it exists so that aggregating whole values - `combineAll` of the additive
   * instance - reads the numbers those values hold directly. Building a [[CurrencyAmount]] per
   * entry only to unwrap it again would allocate two objects for every entry of every input
   * before any addition happened.
   *
   * This is the route for an aggregation that assembles a map from '''nothing''', where the
   * entries of every input are read once whichever shape is used and the buffer is what avoids a
   * copied tree path per entry. Adding ''one'' value to another starts from a map that is already
   * assembled, and [[mergedInto]] is that route.
   *
   * The entries are expected to come from a value of this type, so each number is already an
   * amount and each map already names its currencies once; what a repeated currency across
   * several inputs means is decided exactly as it is for a collection of amounts - the sum in
   * arrival order, checked as it is computed - and the map is assembled in the
   * accumulate-then-seal shape [[distinct]] documents.
   *
   * @param entries  the entries to merge, consumed once
   * @return the total per currency once the entries are exhausted
   * @throws java.lang.IllegalArgumentException if any total is not a number
   */
  private def mergedEntries(entries: Iterator[(Currency, Double)]): SortedMap[Currency, Double] = {
    val buffer: mutable.TreeMap[Currency, Double] =
      mutable.TreeMap.empty[Currency, Double](currencyOrdering)

    @tailrec
    def accumulate(remaining: Iterator[(Currency, Double)]): Unit =
      if (remaining.hasNext) {
        val (currency, amount) = remaining.next()
        buffer.update(
          currency,
          buffer
            .get(currency)
            .fold(amount)(held => CurrencyAmount.checkedAmount(held + amount)))
        accumulate(remaining)
      }

    accumulate(entries)
    SortedMap.from(buffer)(currencyOrdering)
  }

  /**
   * Merges the entries arriving into the map already held, adding up a repeated currency.
   *
   * This is the route [[MultiCurrencyAmount.plus]] of a whole value takes, and it differs from
   * [[mergedEntries]] in where it starts rather than in what it computes. The map held is already
   * assembled and already sorted, so the entries arriving are merged ''into'' it one at a time,
   * each step sharing the nodes the step before it built - which is [[mergedAmount]] repeated,
   * and is why this loop is written in terms of it rather than restating the combination.
   *
   * Accumulating both sides into a buffer instead would read the entries of the value held a
   * second time for nothing: a value of `c` currencies gaining one entry would pay `c + 1`
   * insertions and a seal where this pays one insertion into a map that already exists. The
   * buffer is therefore kept for the routes that assemble a map from nothing and this route is
   * kept over the immutable map, which is the same division [[mergedAmount]] records for one
   * arriving amount.
   *
   * @param held  the map to merge into, which is the map of the value being added to
   * @param arriving  the entries to merge into it, consumed once
   * @return the map with every arriving entry merged into it
   * @throws java.lang.IllegalArgumentException if any total is not a number
   */
  @tailrec
  private def mergedInto(
      held: SortedMap[Currency, Double],
      arriving: Iterator[(Currency, Double)]): SortedMap[Currency, Double] =

    if (!arriving.hasNext) {
      held
    } else {
      val (currency, amount) = arriving.next()
      mergedInto(mergedAmount(held, currency, amount), arriving)
    }

  /**
   * Merges one currency and number into an immutable map, adding to what is held for it.
   *
   * This is the route [[MultiCurrencyAmount.plus]] of a single amount takes, which is one step and
   * nothing else. One arriving amount changes one entry, so the map it merges into is the map the
   * value already holds and the step is a single insertion into it: the other entries are carried
   * over by the sharing of the immutable map, and no buffer is worth assembling for one entry -
   * which is why this step stays written over the immutable map while the routes that assemble a
   * whole collection accumulate into a buffer. The two loops above state the same combination over
   * their buffer, and the way two amounts of one currency combine is the same in both places:
   *
   *   - a currency the map does not hold is inserted with the number as it stands. The number
   *     arrived as the amount of a [[CurrencyAmount]] or out of the map of a value of this type,
   *     so it has already been normalised and already satisfies the invariant, and deciding it
   *     again would change nothing;
   *   - a currency the map holds is given `accumulated + arriving`, in that operand order. The
   *     order matters: floating point addition rounds, so the reverse order can differ in the
   *     last bit, and fixing it here is what makes a total reproducible.
   *
   * The sum is the one number here that has not been decided yet - two infinities of opposite sign
   * add to a value that is not a number - so it is routed through the number-level form of the
   * invariant that [[CurrencyAmount]] publishes to this package, which normalises the number and
   * refuses one that is not an amount without building an amount around it. That keeps the
   * invariant of an amount defined in exactly one place in this library rather than restated here,
   * which is the whole reason the sum is not simply written into the map: an amount that could not
   * exist must not be reachable through an aggregate either. Nothing is allocated for the check,
   * so a step of this aggregation costs the entry of the map and nothing else.
   *
   * @param accumulated  the total per currency up to this step
   * @param currency  the currency of the number arriving
   * @param amount  the number arriving, already normalised and within the invariant
   * @return the map with the number merged into it
   * @throws java.lang.IllegalArgumentException if the total is not a number
   */
  private def mergedAmount(
      accumulated: SortedMap[Currency, Double],
      currency: Currency,
      amount: Double): SortedMap[Currency, Double] =
    accumulated.updated(
      currency,
      accumulated
        .get(currency)
        // the sum is the one number here that has not been decided yet, so it is the one the
        // invariant is applied to - through the number-level form of it, which decides the
        // number without building an amount around it only to read the number back out
        .fold(amount)(held => CurrencyAmount.checkedAmount(held + amount)))

  /**
   * Creates a value from a map of currency to number, normalising and checking every number.
   *
   * This is the route into the type for numbers that have not been decided yet, and it is the one
   * the currency package may call. [[MultiCurrencyAmount.mapAmounts]] reaches it because the
   * operation it applies may produce a value that is no amount, and
   * [[MultiCurrencyAmountArray]] reaches it because reconstructing one scenario of a run means
   * building a value from numbers it holds in primitive arrays - which it can do without
   * assembling a [[CurrencyAmount]] per currency only to have this method unwrap it again.
   *
   * The contract a caller takes on is that the entries name '''distinct''' currencies, and that
   * the numbers are meant as amounts. Neither is weakened here: two entries of one currency would
   * silently lose the first, which is why the requirement is stated rather than checked - a
   * caller reading a map, transposing a run, or mapping the amounts of an existing value has each
   * established it by construction - and the invariant of an amount is still applied to every
   * number, so a caller cannot smuggle a value that is not a number past it. A number that is not
   * an amount fails here with the message [[CurrencyAmount]] reports for it,
   * `Argument 'amount' must not be NaN`.
   *
   * Exactly one map is built: the entries are normalised as they are read, accumulated into a
   * buffer and sealed in the shape [[distinct]] documents, and the sealed map is the map of the
   * value returned. The normalisation and the check are not restated - each number goes through
   * the number-level form of the invariant that [[CurrencyAmount]] publishes to this package, so
   * the invariant of an amount is defined in one place in this library and a value of this type
   * holds exactly the amounts a collection of [[CurrencyAmount]] could hold, while nothing is
   * allocated per entry beyond the node the entry occupies.
   *
   * The entries a caller supplies are in no particular order - a run transposed per currency, a
   * mapping applied to the amounts of an existing value - which is a second reason the buffer is
   * a sorted mutable map: it puts them in currency order as they are written, so the seal finds
   * them ordered however they arrived.
   *
   * @param entries  the amount per currency, naming each currency at most once
   * @return the value holding those amounts, normalised
   * @throws java.lang.IllegalArgumentException if any amount is not a number
   */
  private[currency] def create(entries: IterableOnce[(Currency, Double)]): MultiCurrencyAmount = {
    val buffer: mutable.TreeMap[Currency, Double] =
      mutable.TreeMap.empty[Currency, Double](currencyOrdering)

    // The traversal is a tail-recursive local method rather than a `foreach` for a reason that is
    // about the class file rather than about the loop: a function literal that reads the buffer is
    // lifted into a synthetic '''public''' static method whose parameter list names the buffer's
    // type, where a local method is compiled private and named by nothing. The module is held to a
    // surface that mentions no mutable collection anywhere - it is scanned with `javap` over the
    // compiled classes, not over the sources - so the buffer must not appear even in a signature
    // the compiler wrote.
    @tailrec
    def accumulate(remaining: Iterator[(Currency, Double)]): Unit =
      if (remaining.hasNext) {
        val (currency, amount) = remaining.next()
        // each number is decided as it goes into the buffer by the number-level form of the
        // invariant of an amount: building an amount per entry and reading its number back would
        // allocate an object per entry that nothing keeps
        buffer.update(currency, CurrencyAmount.checkedAmount(amount))
        accumulate(remaining)
      }

    accumulate(entries.iterator)
    instantiate(SortedMap.from(buffer)(currencyOrdering))
  }

  /**
   * Instantiates the type from a map that already holds what a value of it holds.
   *
   * This is the only instantiation of the type, and it is the trusted one: it performs no
   * normalisation and no check, because its argument is required to be a map that a value of this
   * type could already hold - each currency once, ordered by [[currencyOrdering]], and every
   * number an amount, which is to say normalised and not a value that is not a number.
   *
   * Every route into the type reaches it, and each establishes that contract in its own way and
   * says so at its own declaration: [[create]] by normalising and checking each number as it
   * builds the map, [[distinct]] by accumulating numbers that were already amounts, [[merged]]
   * and [[mergedAmount]] by doing the same and checking the one number that is new - a sum - as
   * it is computed, and [[MultiCurrencyAmount.plus]] by merging into the map a value of this type
   * already holds. The routes that assemble a collection hand over a map sealed from a buffer
   * built with [[currencyOrdering]], so the order of the sealed map is that ordering and not one
   * of the buffer's own; each currency occurs once because a map, mutable or not, holds a key
   * once. The split exists so that an aggregation is one pass: the map it produced is the map of
   * the value returned, where routing it through [[create]] would iterate it into a second map to
   * reach a value it already had. It is private to this file, so no caller outside it can take
   * the trusted route by mistake - the currency package is offered [[create]], which decides the
   * numbers it is given.
   *
   * @param amounts  the amount per currency, each currency once, ordered by currency code, every
   *   number already an amount
   * @return the value holding exactly those amounts
   */
  private def instantiate(amounts: SortedMap[Currency, Double]): MultiCurrencyAmount =
    new Impl(amounts)

  /**
   * The one implementation of a multi-currency amount.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it. It is declared rather than written as an anonymous subclass at the instantiation site
   * for two reasons, both about what the class file says: a private member class is one a Java
   * compiler refuses to name, where an anonymous class is public and can be instantiated directly
   * by a caller in another language, and a named class can be compared against, which is what
   * lets [[MultiCurrencyAmount]] refuse in its own constructor to be any other implementation.
   *
   * @param amounts  the amount per currency, holding the contract [[instantiate]] documents -
   *   each currency once, ordered by currency code, every number already an amount
   */
  private final class Impl(amounts: SortedMap[Currency, Double])
      extends MultiCurrencyAmount(amounts)

  /**
   * Converts the entries of a value one at a time and totals them, stopping at the first refusal.
   *
   * This is the traversal behind the second branch of [[MultiCurrencyAmount.convertedTo]]. It is
   * written as a recursion over the entries of the map because the two things that decide whether
   * a conversion is faithful - what the total is added from, and how far the traversal runs before
   * an unavailable rate settles the outcome - are both properties of the traversal itself.
   *
   * The total is carried as a running number from one entry to the next: the entries arrive in
   * the order the map holds them, the alphabetical order of the currency codes, and each converted
   * number is added where it is read. Nothing between the map and the result is built - no
   * collection of the entries, and none of the converted numbers - and the addition is `what has
   * accumulated plus what arrives`, from zero, which is the order and the starting point that fix
   * the rounding of the total.
   *
   * A rate the provider cannot supply returns its failure immediately, so no rate after it is
   * asked for. That is not only a saving: asking for rates a decided outcome does not need would
   * make the number of lookups a failing conversion costs depend on how many amounts happened to
   * follow the one that failed.
   *
   * The recursion is in tail position and runs as a loop, so a value holding any number of
   * currencies converts without consuming stack, and the number it threads is an ordinary
   * argument.
   *
   * @param remaining  the entries still to be converted, pulled one at a time
   * @param resultCurrency  the currency every amount is converted into
   * @param rateProvider  the provider of FX rates, asked once per entry
   * @param accumulated  the total of the entries converted up to this step
   * @return the total of the converted amounts as an amount of the requested currency, or the
   *   failure the provider reported for the first rate it could not supply
   */
  @tailrec
  private def totalConverted(
      remaining: Iterator[(Currency, Double)],
      resultCurrency: Currency,
      rateProvider: FxRateProvider,
      accumulated: Double): FailureOr[CurrencyAmount] =
    if (!remaining.hasNext) {
      // the total is decided by CurrencyAmount rather than here: it is built from rates and
      // amounts a caller supplied, so a total that is no amount is reported and not raised
      CurrencyAmount.of(resultCurrency, accumulated)
    } else {
      val (currency, amount) = remaining.next()
      rateProvider.convert(amount, currency, resultCurrency) match {
        case Right(converted) =>
          totalConverted(remaining, resultCurrency, rateProvider, accumulated + converted)
        case Left(failure) => Left(failure)
      }
    }

  /**
   * The failure reported when a collection names one currency twice.
   *
   * The message names the currency and nothing else. The currency is a value of a closed family
   * rather than text from outside the library, so there is nothing in it to bound or escape, and
   * the failure deliberately carries no attributes, which makes two failures over the same
   * currency equal and directly comparable.
   *
   * @param currency  the currency that appeared twice
   * @return the failure naming the repeated currency
   */
  private def duplicateCurrency(currency: Currency): Failure =
    Failure.Invalid(s"Currency is duplicated: $currency")

  /**
   * The failure reported when an amount is asked for in a currency that is not held.
   *
   * The message names the currency that is not held. As above, the currency is a member of a
   * closed family and the failure carries no attributes.
   *
   * @param currency  the currency that is not held
   * @return the failure naming the currency
   */
  private def unknownCurrency(currency: Currency): Failure =
    Failure.Invalid(s"Unknown currency $currency")

  /**
   * The additive instance of this type: the identity is [[empty]] and combining adds per currency.
   *
   * This type can carry a `Monoid` because combining two of these values cannot fail - amounts of
   * a currency held by both are added, amounts of a currency held by one are carried across, and
   * a value holding nothing leaves its partner unchanged. [[CurrencyAmount]] carries no additive
   * instance for the opposite reason: adding two single-currency amounts fails when the
   * currencies differ, and a `Semigroup` has nowhere to report that.
   *
   * Aggregating a collection is `combineAll`, which is overridden here to make a single pass that
   * adds every amount of every value into one map, rather than folding whole values together one
   * at a time. The two give the same answer to the last bit - for each currency both add the
   * amounts in the order the values arrive - and the pass allocates one result rather than one
   * per value combined.
   *
   * The pass reads the numbers the values hold, through the raw entries of their maps, rather than
   * the amounts they would hand out: an amount built per entry would be unwrapped again by the
   * addition, so aggregating `V` values of `E` entries each would allocate `V × E` objects before
   * the first addition happened. Nothing about the result changes - the same entries arrive in the
   * same order - and what reaches the accumulator is exactly what a traversal would have carried.
   *
   * ===How exactly it is associative===
   *
   * Two properties of the arithmetic bound the associativity of this instance, and both are facts
   * about amounts rather than about the instance:
   *
   *   - an infinite amount combined with an infinite amount of the opposite sign produces a value
   *     that is not a number, which no amount may hold, so it raises the documented invariant of
   *     [[CurrencyAmount]] instead of yielding a value. Associativity therefore holds over finite
   *     amounts;
   *   - floating point addition is only approximately associative, so `(a + b) + c` and
   *     `a + (b + c)` can differ in their last bit. Associativity therefore holds to within the
   *     rounding of double arithmetic rather than under the exact equality of this type.
   *
   * @return the additive instance of this type
   */
  implicit val monoid: Monoid[MultiCurrencyAmount] = new Monoid[MultiCurrencyAmount] {

    override def empty: MultiCurrencyAmount = MultiCurrencyAmount.empty

    override def combine(x: MultiCurrencyAmount, y: MultiCurrencyAmount): MultiCurrencyAmount =
      x.plus(y)

    override def combineAll(as: IterableOnce[MultiCurrencyAmount]): MultiCurrencyAmount =
      instantiate(mergedEntries(as.iterator.flatMap(value => value.amounts.iterator)))
  }

  /**
   * The hashing and equality of these values.
   *
   * This is the only equality-bearing instance of the type: `Hash` extends `Eq`, so a separate
   * `Eq` would be a second answer to the same question. Both are taken from the
   * [[MultiCurrencyAmount.equals]] and [[MultiCurrencyAmount.hashCode]] of the type, which
   * compare the amounts by their bit patterns.
   *
   * No `Order` is offered, because no ordering of these values means anything - neither
   * `[GBP 100]` nor `[USD 100]` is the greater - so an `Order` for this type cannot be summoned.
   *
   * @return the hashing and equality of these values
   */
  implicit val hash: Hash[MultiCurrencyAmount] = Hash.fromUniversalHashCode[MultiCurrencyAmount]

  /**
   * The rendering of these values as text.
   *
   * Renders what [[MultiCurrencyAmount.toString]] renders, the `[GBP 100, USD 200]` form, so the
   * text of a value is the same however it reaches a message.
   *
   * @return the rendering of a value
   */
  implicit val show: Show[MultiCurrencyAmount] = Show.show(_.toString)

  /**
   * The field shape both codecs are derived from, which the decoder reads before validation.
   *
   * Decoding a checked type is two steps: read the fields, then hand them to the factory that
   * decides whether they describe a value. This product is the first step, and encoding is the
   * same two steps in reverse, so both codecs below derive from this one declaration and the JSON
   * shape of a value is stated exactly once. It is private and never returned - the only values of
   * it that exist are the ones the two codecs build.
   *
   * The amounts are carried as a sequence rather than as an object keyed by currency. An object
   * keyed by currency would repeat neither more nor less information, but a sequence of amounts
   * serializes each amount through the codec of [[CurrencyAmount]] itself, so one document names a
   * currency and an amount in exactly one way wherever it appears.
   *
   * @param amounts  the amounts, in the alphabetical order of their currency codes on the way out,
   *   in whatever order a document supplies them on the way in
   */
  private final case class Raw(amounts: Vector[CurrencyAmount]) extends NoJavaSerialization

  /**
   * The name of the field holding the amounts, which is the JSON key of the only field there is.
   *
   * It is named once here because two things below need the same string: the shape derived from
   * [[Raw]] writes and reads the field under it, and the ceiling the decoder applies to the number
   * of amounts a document may state is expressed in terms of it. Naming it keeps the two from
   * drifting apart, since a bound on a field the document does not have would silently bound
   * nothing.
   */
  private val AmountsField: String = "amounts"

  /** The derived decoder of the raw field shape, used by the checking decoder below. */
  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  private val rawEncoder: Encoder.AsObject[Raw] = deriveEncoder[Raw]

  /**
   * The JSON encoding of these values.
   *
   * A value is an object of one field holding the amounts, each amount as the object the codec of
   * [[CurrencyAmount]] writes, ordered by currency code:
   *
   * {{{
   * {"amounts":[{"currency":"GBP","amount":100.0},{"currency":"USD","amount":200.0}]}
   * }}}
   *
   * The order is not a convention of the encoder but a property of the value being encoded: the
   * amounts are held in a map sorted by currency, so two values built from the same amounts in
   * different orders encode to identical bytes without anything being sorted here.
   *
   * The shape is derived over the raw product above rather than written out field by field, and
   * the value is mapped into that product: a value in memory has already been checked, so nothing
   * further has to be decided on the way out. An amount that is infinite is written as a tagged
   * string, because each amount goes through the codec of [[CurrencyAmount]], which applies the
   * non-finite policy of this library. The result is wrapped so that a field holding no value
   * would be omitted; this type has no optional field, so the wrapping changes nothing about its
   * output and exists so that the policy holds without exception.
   *
   * @return the JSON encoding of a value
   */
  implicit val encoder: Encoder[MultiCurrencyAmount] =
    Codecs.dropNulls(rawEncoder.contramap[MultiCurrencyAmount](value => Raw(value.iterator.toVector)))

  /**
   * The JSON decoding of these values.
   *
   * This is the inverse of the encoding above, and it decides whether the amounts describe a value
   * exactly as a caller's arguments are decided: the payload is read into the raw shape and handed
   * to [[of]], so a document naming one currency twice is a decoding failure carrying that reason
   * rather than a value this type would not have built - the last amount silently winning would be
   * the alternative, and it would lose data without saying so.
   *
   * A document may list the amounts in any order, since the value they describe does not depend on
   * it; re-encoding what was decoded puts them in currency order.
   *
   * ===How many amounts a document may state===
   *
   * How long the `amounts` array is, is stated by the document, so the count is read from the
   * payload and measured against `Codecs.MaximumCollectionElements` before a single amount is
   * decoded. A document stating more is a decoding failure naming the ceiling and the field, and
   * nothing is allocated for it - which is the point of measuring first: this factory collapses the
   * amounts into a map keyed by currency, so a refusal issued afterwards would already have paid
   * for reading, sorting and grouping every one of them, and a document naming the same currency a
   * million times costs exactly as much as a legitimate one of that size.
   *
   * The ceiling cannot refuse a value this port wrote. A value of this type holds at most one
   * amount per currency, so the longest array it can write is the number of currencies the
   * reference data names - a figure three orders of magnitude below the ceiling - and
   * `decode(encode(x))` therefore holds for every value that can exist. What the ceiling refuses is
   * a document asking for entries no value of this type could hold.
   *
   * @return the JSON decoding of a value
   */
  implicit val decoder: Decoder[MultiCurrencyAmount] =
    Codecs.boundedFields(AmountsField -> Codecs.MaximumCollectionElements)(
      Codecs.checkedDecoder[Raw, MultiCurrencyAmount](raw => of(raw.amounts))(rawDecoder))
}
