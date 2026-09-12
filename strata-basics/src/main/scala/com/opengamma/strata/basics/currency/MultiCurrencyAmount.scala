/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

import cats.Hash
import cats.Monoid
import cats.Order
import cats.Show
import cats.syntax.traverse._

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.collect.Collections
import com.opengamma.strata.collect.FailureOr
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
 * held as exactly that: a `SortedMap[Currency, Double]` ordered by currency code. The
 * implementation being ported held a sorted ''set'' of [[CurrencyAmount]] instead and checked
 * after the fact that no currency appeared twice, a choice its own comment attributes to the
 * shape of its serialized form rather than to the model. Holding a map instead has three
 * consequences worth stating, because the rest of this type follows from them:
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
 * needs. Both behaviours are those of the implementation being ported, and neither is the safe
 * default for the other's use, so both are kept.
 *
 * ===Adding values of this type: the additive instance===
 *
 * This is the one type in either module of this port that carries a `Monoid`. It can carry one
 * because adding two of these values cannot fail: amounts of a currency present in both are
 * added, amounts of a currency present in one are carried across, and the identity is the value
 * with no amounts at all. [[CurrencyAmount]] deliberately carries no `Semigroup` or `Monoid` for
 * the mirror-image reason - adding two single-currency amounts fails when their currencies
 * differ, and a `Semigroup` has nowhere to report that.
 *
 * Two limitations of that instance are documented rather than hidden, because a reader who
 * "fixes" either of them gets a law suite that fails intermittently:
 *
 *   - '''the laws are checked over finite amounts only'''. Combining `+∞` with `−∞` produces a
 *     value that is not a number, which no amount may hold, so it raises the documented invariant
 *     of [[CurrencyAmount]] rather than producing a value. A generator that emits infinities
 *     therefore falsifies associativity by reaching that invariant, which says nothing about the
 *     instance;
 *   - '''the laws are checked with a tolerant equality''', within `1e-9` relative, rather than
 *     with the exact equality of this type. Floating point addition is only approximately
 *     associative - `(a + b) + c` and `a + (b + c)` can differ in their last bit - so exact
 *     equality falsifies associativity on ordinary finite inputs after a handful of examples.
 *     The instance is as associative as double arithmetic allows, and the tolerance is what
 *     states that precisely.
 *
 * ===Equality and ordering===
 *
 * Two values are equal when they hold the same currencies with amounts of the same bit pattern.
 * The bit comparison is the one the generated bean performed through the equality of
 * [[CurrencyAmount]], and it differs from the comparison a plain case class would have
 * synthesised for two values: one that is not a number equals itself, and a negative zero
 * differs from a positive zero - the second being unreachable here, because every route into
 * this type normalises it away.
 *
 * There is deliberately no `Order`. The type being ported was not comparable, and there is no
 * ordering of these values that means anything: neither `[GBP 100]` nor `[USD 100]` is the
 * greater, and comparing them by size or by code would invent an answer. A caller that needs a
 * reproducible sequence of them should sort by whatever identifies them in its own domain.
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
 * [[mapCurrencyAmounts]] - is total in signature, as it was in the implementation being ported,
 * and shares the single numeric invariant of [[CurrencyAmount]]: an amount that is not a number
 * cannot be held, and an operation that would produce one raises that invariant - literally, by
 * routing the result through [[CurrencyAmount]] itself - rather than widening every operation into
 * a failure channel. Reaching it requires infinite operands or a mapping function that produces a
 * value that is not a number.
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
    extends FxConvertible[CurrencyAmount] {

  //-------------------------------------------------------------------------
  /**
   * Gets the amounts held, as a set ordered by currency and then by amount.
   *
   * This is the accessor the implementation being ported published, and the set it answers with
   * has the same contents and the same iteration order: the ordering is that of
   * [[CurrencyAmount]], which compares the currency first, and since each currency occurs at
   * most once the amounts never have to be compared to break a tie.
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

  //-------------------------------------------------------------------------
  /**
   * Gets the amount of the specified currency.
   *
   * A currency this value does not hold is reported rather than answered with zero, which is the
   * distinction the implementation being ported drew between this member and
   * [[getAmountOrZero]]: an absent currency may mean a currency that was never involved, and
   * silently reading it as zero would hide that. Whether it happens depends on the value and the
   * currency a caller holds, so it is a failure rather than a throw, and its wording is the
   * wording that implementation reported:
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
      .map(amount => MultiCurrencyAmount.currencyAmount(currency, amount))
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
      .fold(CurrencyAmount.zero(currency))(amount =>
        MultiCurrencyAmount.currencyAmount(currency, amount))

  //-------------------------------------------------------------------------
  /**
   * Returns a copy of this value with the specified amount of the specified currency added.
   *
   * A currency this value already holds has the amount added to what it holds; a currency it does
   * not hold is added to it. The addition is ordinary `Double` arithmetic in the order the
   * implementation being ported performed it - what is held plus what is supplied - so the result
   * agrees with it bit for bit.
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
    plus(MultiCurrencyAmount.currencyAmount(currency, amountToAdd))

  /**
   * Returns a copy of this value with the specified amount added.
   *
   * This is [[plus]] of a currency and a number with the two carried together, and it behaves
   * identically: the currency of the amount is added to or inserted into what this value holds.
   *
   * @param amountToAdd  the amount to add
   * @return this value with the amount added
   * @throws java.lang.IllegalArgumentException if the sum is not a number, which requires
   *   infinite operands of opposite sign
   */
  def plus(amountToAdd: CurrencyAmount): MultiCurrencyAmount =
    MultiCurrencyAmount.merged(iterator ++ Iterator.single(amountToAdd))

  /**
   * Returns a copy of this value with every amount of the specified value added.
   *
   * Each currency of the other value is added to or inserted into this one, so the result holds
   * the union of the two sets of currencies. This is the operation the additive instance of this
   * type combines with, and the operation is associative to the extent double addition is - see
   * the note on [[MultiCurrencyAmount.monoid]].
   *
   * {{{
   * // [GBP 100, USD 200] plus [EUR 75, USD 50]
   * // is [EUR 75, GBP 100, USD 250]
   * }}}
   *
   * @param amountToAdd  the value whose amounts are to be added
   * @return this value with the other value's amounts added
   * @throws java.lang.IllegalArgumentException if any sum is not a number, which requires
   *   infinite operands of opposite sign
   */
  def plus(amountToAdd: MultiCurrencyAmount): MultiCurrencyAmount =
    MultiCurrencyAmount.merged(iterator ++ amountToAdd.iterator)

  //-------------------------------------------------------------------------
  /**
   * Returns a copy of this value with the specified amount of the specified currency subtracted.
   *
   * A currency this value already holds has the amount subtracted from what it holds; a currency
   * it does not hold is added to it ''negated'', which is the behaviour the implementation being
   * ported documented and which keeps subtraction the exact inverse of addition:
   *
   * {{{
   * gbp100.minus(Currency.GBP, 50d)   // [GBP 50]
   * gbp100.minus(Currency.USD, 50d)   // [GBP 100, USD -50]
   * }}}
   *
   * The parameter keeps the name it had in the implementation being ported, where subtraction was
   * written as the addition of a negated amount.
   *
   * @param currency  the currency to subtract an amount of
   * @param amountToAdd  the amount of that currency to subtract
   * @return this value with the amount subtracted
   * @throws java.lang.IllegalArgumentException if the amount supplied is not a number, or if the
   *   difference is not one, which requires infinite operands of the same sign
   */
  def minus(currency: Currency, amountToAdd: Double): MultiCurrencyAmount =
    plus(MultiCurrencyAmount.currencyAmount(currency, -amountToAdd))

  /**
   * Returns a copy of this value with the specified amount subtracted.
   *
   * The amount is negated and added, as in the implementation being ported, so a currency this
   * value does not hold is inserted with the negated amount.
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
   * The other value is negated and added, as in the implementation being ported, so a currency
   * only it holds appears in the result with its amount negated.
   *
   * @param amountToSubtract  the value whose amounts are to be subtracted
   * @return this value with the other value's amounts subtracted
   * @throws java.lang.IllegalArgumentException if any difference is not a number, which requires
   *   infinite operands of the same sign
   */
  def minus(amountToSubtract: MultiCurrencyAmount): MultiCurrencyAmount =
    plus(amountToSubtract.negated)

  //-------------------------------------------------------------------------
  /**
   * Returns a copy of this value with every amount multiplied by the specified factor.
   *
   * The currencies are untouched, so the result holds exactly the currencies this value holds.
   * The multiplication is written as the amount times the factor, the order the implementation
   * being ported used, so a product that rounds differently under the other order rounds the same
   * way here.
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
   * An amount of zero negates to zero rather than to a negative zero. The implementation being
   * ported special-cased that, and the special case is kept even though construction would
   * normalise the sign away in any event, so that the arithmetic and the normalisation each
   * remain correct on their own.
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
   * holds exactly the currencies this value holds and no amount can merge into another. The
   * operation is an ordinary function rather than the primitive-specialised interface of the
   * implementation being ported, which is the same thing expressed in this language.
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
   * is the total of the mapped amounts, as the implementation being ported documented and as its
   * use of the merging collector implemented:
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

  //-------------------------------------------------------------------------
  /**
   * Returns an iterator over the amounts held, ordered by currency.
   *
   * This is what the stream of the implementation being ported becomes here, following the
   * convention this port uses for every such member: a Java stream type has no place in the
   * public API of a Scala library, and an `Iterator` is the same thing - a traversal that
   * materialises nothing - expressed in the standard library of this language. A caller that
   * wants a collection can ask for one, `iterator.toList` and [[getAmounts]] being the two
   * obvious ways.
   *
   * The iterator reads the map this value holds, which no operation ever modifies, so it cannot
   * observe a change part-way through a traversal.
   *
   * @return the amounts held, in the alphabetical order of their currency codes
   */
  def iterator: Iterator[CurrencyAmount] =
    amounts.iterator.map { case (currency, amount) =>
      MultiCurrencyAmount.currencyAmount(currency, amount)
    }

  //-------------------------------------------------------------------------
  /**
   * Converts every amount held into the specified currency and totals them.
   *
   * This is the [[FxConvertible]] implementation of this type, and it is the reason that trait is
   * parameterised: converting a value that holds several currencies collapses it into a single
   * [[CurrencyAmount]], not into another value of this type.
   *
   * The route taken is the route of the implementation being ported, in both of its branches,
   * because the two differ in an observable way:
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
   * starting from zero, which is the order and the starting point that implementation used.
   * Floating point addition is order-sensitive, so stating the order is what makes this total
   * reproducible and what lets it be compared against a captured baseline.
   *
   * A single unavailable rate fails the whole conversion, carrying the failure the provider
   * reported, and no later rate is asked for - a partial total would be a number with no meaning.
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
      MultiCurrencyAmount.currencyAmount(currency, amount).convertedTo(resultCurrency, rateProvider)
    } else {
      amounts.toList
        .traverse { case (currency, amount) =>
          rateProvider.convert(amount, currency, resultCurrency)
        }
        .flatMap(converted =>
          CurrencyAmount.of(resultCurrency, converted.foldLeft(0d)((total, next) => total + next)))
    }

  //-------------------------------------------------------------------------
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

  //-------------------------------------------------------------------------
  /**
   * Checks whether this value equals another object.
   *
   * Another value of this type is equal when it holds the same currencies and, for each of them,
   * an amount with the same bit pattern. The bit comparison is the one the generated bean
   * performed through the equality of [[CurrencyAmount]], and it differs from the comparison a
   * case class would have synthesised for two values: one that is not a number, which here equals
   * itself, and a negative zero, which here differs from a positive zero and which construction
   * ensures no amount holds. An object of any other type is not equal.
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
   * usual odd prime in the way the bean this replaces mixed the fields of an amount, and the
   * entries contribute in currency order so that the result does not depend on how the value was
   * built. The amount is hashed by its bit pattern, so two values that [[equals]] calls equal
   * always agree here too. Every part of it is a function of the value alone, so the hash of a
   * value is identical in every run of every program, which is what the byte-stability properties
   * of the test suite rely on.
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
   * brackets - `[GBP 100, USD 200]` - which is what the sorted collection of the implementation
   * being ported rendered and therefore what documents, rendered output and test expectations
   * carry. Each amount is written as [[CurrencyAmount.toString]] writes it, so a whole number has
   * no fractional part and a value holding nothing renders as `[]`.
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
 * @see [[MultiCurrencyAmount]] for the type itself, the difference between [[of]] and [[total]],
 *   and the two documented limitations of [[monoid]]
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
   * It is the order [[CurrencyAmount]] publishes - currency first, then amount - which is the
   * comparator the sorted set of the implementation being ported used. Since a value of this type
   * holds each currency at most once, the amounts are never reached.
   */
  private val currencyAmountOrdering: Ordering[CurrencyAmount] = Order[CurrencyAmount].toOrdering

  //-------------------------------------------------------------------------
  /**
   * The value that holds no amount at all.
   *
   * This is the constant the implementation being ported held, and it is the identity of
   * [[monoid]]: adding it to any value yields that value. It renders as `[]`, its size is zero,
   * and converting it into any currency gives zero of that currency.
   *
   * @return the value holding no amounts
   */
  val empty: MultiCurrencyAmount = create(List.empty[(Currency, Double)])

  //-------------------------------------------------------------------------
  /**
   * Obtains a value holding a single amount, of the specified currency and number.
   *
   * The number has to be an amount: a value that is not a number is rejected and a negative zero
   * is normalised to a positive zero, exactly as [[CurrencyAmount.of]] decides it, because this
   * factory is that one with the result wrapped. The implementation being ported threw for a
   * value that is not a number here; whether that happens depends on the number a caller holds
   * rather than on the calling code, so it is reported as a failure - the same failure
   * [[CurrencyAmount.of]] reports, so the two routes cannot disagree about what an amount is.
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
    CurrencyAmount.of(currency, amount).map(checked => create(List((checked.currency, checked.amount))))

  /**
   * Obtains a value from the specified amounts, rejecting a repeated currency.
   *
   * This is the varargs form of the factory below and behaves identically, except that no
   * argument at all yields [[empty]] without anything being examined, which is the short-circuit
   * the implementation being ported took.
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
   * lost upstream - so it is reported as a failure, with the wording the implementation being
   * ported reported, and the traversal stops where the repeat was found:
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
    distinct(amounts.iterator, SortedMap.empty[Currency, Double](currencyOrdering))

  /**
   * Obtains a value from the specified map of currency to number.
   *
   * A map cannot hold a key twice, so no currency can be repeated and the only thing left to
   * decide is whether each number is an amount - which it is unless it is a value that is not a
   * number, rejected here as [[CurrencyAmount.of]] rejects it. The implementation being ported
   * threw in that case; this reports it, consistently with the factory taking a currency and a
   * number above.
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

  //-------------------------------------------------------------------------
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
   * why it returns a value rather than an `Either` - as the implementation being ported also
   * did. [[of]] is the factory that refuses a repeated currency instead.
   *
   * Amounts of one currency are added in the order they arrive, starting from the first of them,
   * which is the order and the starting point the collector of the implementation being ported
   * used, so a total agrees with it bit for bit. The `Collector` that implementation published
   * for use with a stream has no counterpart here: this member and
   * `Monoid[MultiCurrencyAmount].combineAll` are the two ways of aggregating, and neither needs
   * one.
   *
   * @param amounts  the amounts to total, of any currencies, consumed once
   * @return the value holding the total per currency
   * @throws java.lang.IllegalArgumentException if any total is not a number, which requires
   *   infinite operands of opposite sign
   */
  def total(amounts: Iterable[CurrencyAmount]): MultiCurrencyAmount = merged(amounts)

  //-------------------------------------------------------------------------
  /**
   * Accumulates amounts of distinct currencies, stopping at the first currency that repeats.
   *
   * The amounts arrive as an iterator and are pulled one at a time, so a repeat stops the
   * traversal where it is found and nothing after it is read - which is what lets the factory
   * above promise that a failing call leaves the rest of a single-use collection untouched.
   * Presence is asked of each currency before it is added, because a later amount would otherwise
   * silently replace an earlier one, which is precisely the condition being reported.
   *
   * The recursion is in tail position and compiles to a loop, so a collection of any size is
   * traversed without consuming stack, and the map it threads is an immutable value passed from
   * one step to the next rather than a mutable accumulator. The map is small by nature - it holds
   * at most one entry per currency this library defines - so the path copied by each insertion
   * costs a constant that no realistic input makes matter.
   *
   * @param remaining  the amounts still to be examined
   * @param accumulated  the amounts accepted so far, keyed by currency
   * @return the value holding the accumulated amounts, or the failure naming the repeated currency
   */
  @tailrec
  private def distinct(
      remaining: Iterator[CurrencyAmount],
      accumulated: SortedMap[Currency, Double]): FailureOr[MultiCurrencyAmount] =
    if (!remaining.hasNext) {
      Right(create(accumulated))
    } else {
      val next = remaining.next()
      if (accumulated.contains(next.currency)) {
        Left(duplicateCurrency(next.currency))
      } else {
        distinct(remaining, accumulated.updated(next.currency, next.amount))
      }
    }

  /**
   * Assembles a value from amounts of any currencies, adding up those of the same currency.
   *
   * This is the merging aggregation that [[total]], the three [[MultiCurrencyAmount.plus]]
   * members, the two [[MultiCurrencyAmount.minus]] members that delegate to them and
   * [[MultiCurrencyAmount.mapCurrencyAmounts]] are all written in terms of, so the way amounts
   * combine is stated once. It is the collector of the implementation being ported expressed as a
   * fold: the grouping helper of `strata-collect` is given the currency as the key, the number as
   * the value, and addition as the combination, and it adds the amount arriving to the amount
   * accumulated in that order - which is the order that implementation's merge function used and
   * therefore the order that reproduces its rounding.
   *
   * @param amounts  the amounts to combine, of any currencies, consumed once
   * @return the value holding the total per currency
   * @throws java.lang.IllegalArgumentException if any total is not a number
   */
  private def merged(amounts: IterableOnce[CurrencyAmount]): MultiCurrencyAmount =
    create(
      Collections.toSortedMap[CurrencyAmount, Currency, Double](
        amounts,
        amount => amount.currency,
        amount => amount.amount,
        // the parameter types are written out because the member is overloaded on its arity, and
        // an overloaded call is resolved before the types of its function arguments are inferred
        (accumulated: Double, arriving: Double) => accumulated + arriving))

  /**
   * Creates a value, normalising and checking every amount, which every route funnels through.
   *
   * This is the only instantiation of the type and it is private, so the factories above and the
   * arithmetic of the type are the only ways into it. It is what keeps that arithmetic total in
   * signature while the invariant of an amount still holds: a caller adding ordinary amounts
   * cannot reach the check, and a caller combining infinities reaches it and is told so.
   *
   * The normalisation and the check are not restated here. Each number is routed through
   * [[currencyAmount]], which is [[CurrencyAmount]] normalising and checking it, so the invariant
   * of an amount is defined in exactly one place in this library and a value of this type holds
   * exactly the amounts a collection of [[CurrencyAmount]] could hold. A number that is not an
   * amount consequently fails here with the message that type reports for it,
   * `Argument 'amount' must not be NaN`, which is the message the implementation being ported
   * reported from the same check.
   *
   * The entries must name distinct currencies. Each caller establishes that in its own way -
   * [[distinct]] by refusing a repeat, [[merged]] by adding repeats together, and
   * [[MultiCurrencyAmount.mapAmounts]] by leaving the currencies of an existing value untouched -
   * and nothing outside this file can call this method, so the requirement cannot be violated
   * from elsewhere.
   *
   * @param entries  the amount per currency, naming each currency at most once
   * @return the value holding those amounts, normalised
   * @throws java.lang.IllegalArgumentException if any amount is not a number
   */
  private def create(entries: IterableOnce[(Currency, Double)]): MultiCurrencyAmount =
    new MultiCurrencyAmount(
      SortedMap.from(entries.iterator.map { case (currency, amount) =>
        (currency, currencyAmount(currency, amount).amount)
      })(currencyOrdering)) {}

  /**
   * Turns a currency and a number into the amount of that currency, normalising and checking it.
   *
   * This is [[CurrencyAmount]] doing both jobs rather than this file doing either: adding the
   * number to a zero amount of the currency applies the normalisation of that type - `-0.0` loses
   * its sign while every other value, the infinities included, is left exactly as it was - and
   * reaches its check, which refuses a value that is not a number. The implementation being ported
   * normalised and checked with the same addition, in the constructor of that type, and this
   * routes through that constructor instead of restating it.
   *
   * It is used for both directions, and neither use needs anything else. [[create]] uses it to
   * decide a number arriving from outside, and the members that read the map of an existing value -
   * [[MultiCurrencyAmount.iterator]], [[MultiCurrencyAmount.getAmount]] and
   * [[MultiCurrencyAmount.getAmountOrZero]] - use it to rebuild an amount that has already been
   * decided, where it cannot fail and changes nothing. That second use is written through the
   * total arithmetic of that type rather than through its reporting factory for exactly that
   * reason: there is nothing left to decide, so there is no failure to report and no caller that
   * would have to unwrap one.
   *
   * @param currency  the currency of the amount
   * @param amount  the number of that currency, normalised and checked here
   * @return the amount of that currency
   * @throws java.lang.IllegalArgumentException if the number is not an amount
   */
  private def currencyAmount(currency: Currency, amount: Double): CurrencyAmount =
    CurrencyAmount.zero(currency).plus(amount)

  /**
   * The failure reported when a collection names one currency twice.
   *
   * The wording is that of the implementation being ported, which named the currency and nothing
   * else. The currency is a value of a closed family rather than text from outside the library,
   * so there is nothing in it to bound or escape, and the failure deliberately carries no
   * attributes, which makes two failures over the same currency equal and directly comparable.
   *
   * @param currency  the currency that appeared twice
   * @return the failure naming the repeated currency
   */
  private def duplicateCurrency(currency: Currency): Failure =
    Failure.Invalid(s"Currency is duplicated: $currency")

  /**
   * The failure reported when an amount is asked for in a currency that is not held.
   *
   * The wording is that of the implementation being ported. As above, the currency is a member of
   * a closed family and the failure carries no attributes.
   *
   * @param currency  the currency that is not held
   * @return the failure naming the currency
   */
  private def unknownCurrency(currency: Currency): Failure =
    Failure.Invalid(s"Unknown currency $currency")

  //-------------------------------------------------------------------------
  /**
   * The additive instance of this type: the identity is [[empty]] and combining adds per currency.
   *
   * This is the only `Monoid` in either module of this port, and this type can carry one because
   * combining two of these values cannot fail - amounts of a currency held by both are added,
   * amounts of a currency held by one are carried across, and a value holding nothing leaves its
   * partner unchanged. [[CurrencyAmount]] carries no additive instance for the opposite reason:
   * adding two single-currency amounts fails when the currencies differ, and a `Semigroup` has
   * nowhere to report that.
   *
   * Aggregating a collection is `combineAll`, which is overridden here to make a single pass that
   * adds every amount of every value into one map, rather than folding whole values together one
   * at a time. The two give the same answer to the last bit - for each currency both add the
   * amounts in the order the values arrive - and the pass allocates one result rather than one
   * per value combined.
   *
   * ===The two documented limitations of its laws===
   *
   * The law suite for this instance restricts its generators to '''finite amounts''' and compares
   * with an equality that is tolerant to '''`1e-9` relative''' rather than with the exact
   * equality of this type. Both are deliberate weakenings, recorded in the migration notes of
   * this port, and neither is a convenience of the test:
   *
   *   - an infinite amount combined with an infinite amount of the opposite sign produces a value
   *     that is not a number, which no amount may hold, so it raises the documented invariant of
   *     [[CurrencyAmount]]. A generator emitting infinities falsifies associativity by reaching
   *     that invariant, which says nothing about whether this instance is associative;
   *   - floating point addition is only approximately associative, so `(a + b) + c` and
   *     `a + (b + c)` can differ in their last bit. Under exact equality associativity is
   *     falsified within a handful of examples on wholly ordinary finite inputs. The tolerance
   *     states the strongest thing that is actually true of double arithmetic.
   *
   * A reader tempted to replace the tolerant equality with the exact one, or to widen the
   * generators, should expect an intermittently failing law suite and nothing else: the weakness
   * is in IEEE-754 addition, not in this instance.
   *
   * @return the additive instance of this type
   */
  implicit val monoid: Monoid[MultiCurrencyAmount] = new Monoid[MultiCurrencyAmount] {

    override def empty: MultiCurrencyAmount = MultiCurrencyAmount.empty

    override def combine(x: MultiCurrencyAmount, y: MultiCurrencyAmount): MultiCurrencyAmount =
      x.plus(y)

    override def combineAll(as: IterableOnce[MultiCurrencyAmount]): MultiCurrencyAmount =
      merged(as.iterator.flatMap(value => value.iterator))
  }

  /**
   * The hashing and equality of these values.
   *
   * This is the only equality-bearing instance of the type: `Hash` extends `Eq`, so a separate
   * `Eq` would be a second answer to the same question. Both are taken from the
   * [[MultiCurrencyAmount.equals]] and [[MultiCurrencyAmount.hashCode]] of the type, which
   * compare the amounts by their bit patterns.
   *
   * No `Order` is offered, because the type being ported was not comparable and no ordering of
   * these values means anything - neither `[GBP 100]` nor `[USD 100]` is the greater. The law
   * suite of this port asserts that an `Order` for this type cannot be summoned, so declaring one
   * here would be a visible change rather than an addition.
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

  //-------------------------------------------------------------------------
  /**
   * The field shape both codecs are derived from, which the decoder reads before validation.
   *
   * Decoding a checked type is two steps: read the fields, then hand them to the factory that
   * decides whether they describe a value. This product is the first step, and encoding is the
   * same two steps in reverse, so both codecs below derive from this one declaration and the JSON
   * shape of a value is stated exactly once. It is private and never returned - the only values of
   * it that exist are the ones the two codecs build.
   *
   * The amounts are carried as a sequence rather than as an object keyed by currency, which is the
   * shape this port defines for this type. An object keyed by currency would repeat neither more
   * nor less information, but a sequence of amounts serializes each amount through the codec of
   * [[CurrencyAmount]] itself, so one document names a currency and an amount in exactly one way
   * wherever it appears.
   *
   * @param amounts  the amounts, in the alphabetical order of their currency codes on the way out,
   *   in whatever order a document supplies them on the way in
   */
  private final case class Raw(amounts: Vector[CurrencyAmount])

  /** The derived decoder of the raw field shape, used by the checking decoder below. */
  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /** The derived encoder of the raw field shape, used by the encoder below. */
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
   * different orders encode to identical bytes. That is what makes the byte-stability property of
   * the test suite hold for this type without anything being sorted here.
   *
   * The shape is derived at compile time over the raw product above rather than written out field
   * by field, which is what every product of this port does, and the value is mapped into that
   * product: a value in memory has already been checked, so nothing further has to be decided on
   * the way out. An amount that is infinite is written as the tagged string the double policy of
   * this port defines, because each amount goes through the codec of [[CurrencyAmount]], which
   * applies that policy. The result is wrapped so that a field holding no value would be omitted,
   * which is the policy every product of this port follows - this type has no optional field, so
   * the wrapping changes nothing about its output and exists so that the policy holds without
   * exception.
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
   * @return the JSON decoding of a value
   */
  implicit val decoder: Decoder[MultiCurrencyAmount] =
    Codecs.checkedDecoder[Raw, MultiCurrencyAmount](raw => of(raw.amounts))(rawDecoder)
}

