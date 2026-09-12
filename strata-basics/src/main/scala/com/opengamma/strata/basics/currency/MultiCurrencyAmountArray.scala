/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

import cats.Hash
import cats.Order
import cats.Show
import cats.syntax.traverse._

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.Collections
import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.toNec

/**
 * An array of multi-currency amounts.
 *
 * This represents a run of [[MultiCurrencyAmount]] values - the amounts of a measure across the
 * scenarios of a calculation, for instance - held as a map of [[Currency]] to [[DoubleArray]]
 * rather than as a collection of amounts. A run of a hundred thousand scenarios in three
 * currencies is consequently three currencies and three primitive arrays instead of a hundred
 * thousand small objects, and arithmetic over the whole run is one arithmetic operation per
 * currency.
 *
 * ===What the representation guarantees===
 *
 * Two structural properties hold of every value of this type, and both are established before a
 * value exists rather than checked when one is read:
 *
 *   - the size is zero or greater, and
 *   - every array of values has exactly that many elements, one per index of the run.
 *
 * The second is why a currency and an index always name a number: a currency that is held at all
 * is held for the whole run. The implementation being ported relied on the same two properties -
 * it validated the size in its constructor and the array lengths in its `of` and again when it
 * was deserialized - and here they are the post-condition of the one checking factory, the
 * `of` that reads a map of values per currency, which every route that could break them goes
 * through.
 *
 * ===Zero padding===
 *
 * A currency the run holds is held for every index, so an amount that names no value for a
 * currency at some index contributes zero there rather than a gap. Building from amounts makes
 * that visible:
 *
 * {{{
 * val array = MultiCurrencyAmountArray.of(oneGbp, twoUsd)
 * array.size                          // 2
 * array.getCurrencies                 // Set(GBP, USD)
 * array.getValues(Currency.GBP)       // Right([1.0, 0.0])
 * array.getValues(Currency.USD)       // Right([0.0, 2.0])
 * }}}
 *
 * This is the behaviour of the implementation being ported, which allocated a full-length array
 * for a currency the first time it saw it and wrote only the indices where that currency
 * appeared, leaving the rest at zero. It is worth stating because it is observable: reading index
 * zero back with [[get]] answers `[GBP 1, USD 0]`, naming both currencies, and not `[GBP 1]`.
 *
 * ===Which operations can fail===
 *
 * Reading the values of a currency the run does not hold fails, since there is no array to
 * answer with. Combining two runs fails when they have different sizes, because there is no
 * value to combine at an index only one of them has. Converting fails when a rate the conversion
 * needs is unavailable. Each of those depends on the values a caller holds rather than on the
 * calling code, so each is reported as a [[Failure]] rather than by abandoning the call.
 *
 * Everything else here is total: reading an index, scaling every value, mapping every value,
 * listing the amounts. Two documented invariants can still be raised, exactly as they were by
 * the implementation being ported - an index outside the run is the index exception of the
 * runtime, and reading back a value that is not a number is an amount [[CurrencyAmount]] does not
 * admit.
 *
 * ===Equality===
 *
 * Two runs are equal when they have the same size and hold the same currencies with values that
 * are equal element by element, compared by bit pattern as [[DoubleArray]] compares them. That is
 * the comparison the generated bean of the implementation being ported performed, and it differs
 * from a numeric comparison in two deliberate places the round-trip properties of the test suite
 * rely on: a value that is not a number equals itself, so a run always equals itself, and a
 * negative zero differs from a positive zero. Because the currencies are held in a map, two runs
 * built from the same data in different orders are equal without anything having to be sorted
 * when they are compared.
 *
 * There is deliberately no ordering. The implementation being ported is not comparable, and a run
 * of multi-currency amounts has no ordering worth inventing.
 *
 * ===Thread safety===
 *
 * An instance is immutable, and so are the map and the arrays it holds, so it is safe to share
 * between any number of threads without synchronisation. Every operation returns a new run rather
 * than changing the one it was called on.
 *
 * @param size  the number of amounts the run holds, zero or greater
 * @param values  the values held per currency, ordered by currency code, each holding exactly
 *   `size` elements
 * @see [[CurrencyAmountArray]] for a run of amounts that are all in one currency, which is what
 *   [[convertedTo]] produces
 * @see [[MultiCurrencyAmount]] for a single multi-currency amount, which is what [[get]] produces
 */
sealed abstract case class MultiCurrencyAmountArray private (
    size: Int,
    values: SortedMap[Currency, DoubleArray])
    extends FxConvertible[CurrencyAmountArray] {

  //-------------------------------------------------------------------------
  /**
   * Gets the set of currencies this run holds values for.
   *
   * The currencies are ordered by their codes, since that is how the run holds them, and the set
   * is the key set of that map handed back as it is - it is immutable, so there is nothing a
   * caller could do to it that would affect this run.
   *
   * @return the currencies this run holds values for, ordered by currency code
   */
  def getCurrencies: Set[Currency] = values.keySet

  /**
   * Gets the values of the specified currency.
   *
   * A currency the run does not hold is reported rather than answered with an array of zeroes:
   * whether the run holds it is a property of the run and the currency a caller has, and reading
   * an absent currency as nothing would hide the difference between a currency that was never
   * involved and one whose values happen to be zero.
   *
   * {{{
   * array.getValues(Currency.GBP)   // Right([1.0, 0.0])
   * array.getValues(Currency.CHF)   // Left(Failure.Invalid("No values available for CHF"))
   * }}}
   *
   * The wording is that of the implementation being ported, so a log or an expectation carrying
   * the message of the exception it raised carries this message unchanged.
   *
   * @param currency  the currency to read the values of
   * @return the values of that currency, or the failure naming the currency this run does not
   *   hold
   */
  def getValues(currency: Currency): FailureOr[DoubleArray] =
    values.get(currency).toRight(MultiCurrencyAmountArray.noValues(currency))

  /**
   * Gets the amount at the specified index.
   *
   * The amount names every currency of the run, taking each currency's value at that position,
   * counting from zero. A currency whose value there is zero is named with a zero amount rather
   * than left out, which is the reconstruction the implementation being ported performed and the
   * visible consequence of the zero padding described on this type:
   *
   * {{{
   * MultiCurrencyAmountArray.of(oneGbp, twoUsd).get(0)   // [GBP 1, USD 0]
   * }}}
   *
   * This is total in signature, as it was there, and it raises that implementation's two
   * documented invariants rather than widening into a failure channel: an index outside the run
   * is the index exception of the runtime, exactly as reading the [[DoubleArray]] of a currency
   * directly would be, and a value that is not a number is an amount [[CurrencyAmount]] does not
   * admit. The second is reachable only from a run built with such a value, and it is the
   * behaviour of the implementation being ported, whose own `get` raised it too. A run holding no
   * currency answers with an empty amount for any index, as it did there, since no array is read.
   *
   * @param index  the zero-based index to retrieve
   * @return the amount at that index, naming every currency of the run
   * @throws java.lang.IndexOutOfBoundsException if the index is outside the run and the run holds
   *   at least one currency
   * @throws java.lang.IllegalArgumentException if a value at that index is not a number
   */
  def get(index: Int): MultiCurrencyAmount =
    MultiCurrencyAmount.total(values.iterator.map { case (currency, currencyValues) =>
      // the total route into an amount: the value has already been decided, so the checking
      // factory of `CurrencyAmount` is not used - it would widen reading an index into a failure
      // channel the implementation being ported did not have
      CurrencyAmount.zero(currency).mapAmount(_ => currencyValues.get(index))
    }.toList)

  /**
   * Returns the amounts of this run, one at a time.
   *
   * This is the member the stream of the implementation being ported becomes. A stream is a lazy
   * sequence that is traversed once, which is what an `Iterator` is in this language, so a call
   * site that streamed the amounts in order to filter, map or fold them reads the same way here:
   *
   * {{{
   * array.iterator.map(_.getAmountOrZero(Currency.GBP)).toList
   * }}}
   *
   * The amounts are produced in index order and each is built only as it is read, through [[get]]
   * and therefore with its invariants, so a caller that stops early never builds the rest.
   * [[toList]] is the eager form, for a caller that wants every amount at once.
   *
   * @return the amounts of this run in index order, built as they are read
   */
  def iterator: Iterator[MultiCurrencyAmount] = Iterator.range(0, size).map(get)

  /**
   * Returns the amounts of this run as a list.
   *
   * This is [[iterator]] traversed to its end, for a caller that wants the amounts as a
   * collection rather than as a traversal. It costs one amount object per index, holding one
   * value per currency - which is exactly the representation this type exists to avoid, and is
   * why it is obtained explicitly rather than being how the type holds its data.
   *
   * @return the amounts of this run in index order
   */
  def toList: List[MultiCurrencyAmount] = iterator.toList

  //-------------------------------------------------------------------------
  /**
   * Returns a run with every value multiplied by the specified factor.
   *
   * The multiplication is delegated to the values of each currency, so it is one operation per
   * currency over its primitive array and the products agree bit for bit with the values
   * multiplied one at a time. A factor of one answers with arrays identical to those of this run,
   * which is what the underlying arrays do.
   *
   * The currencies are unchanged: scaling a run by a plain number does not convert it, and
   * [[convertedTo]] is the member that does.
   *
   * @param factor  the multiplicative factor
   * @return a copy of this run with every value multiplied by the factor
   */
  def multipliedBy(factor: Double): MultiCurrencyAmountArray =
    mapValues(currencyValues => currencyValues.multipliedBy(factor))

  /**
   * Returns a run with the specified operation applied to every value.
   *
   * This is the general form of the arithmetic of this type, for an operation it does not offer
   * as a member of its own:
   *
   * {{{
   * array.mapAmounts(value => if (value < 0d) 0d else value * 3d)
   * }}}
   *
   * The operation is applied to each value of each currency, the currencies taken in the order
   * the run holds them and the values of each in index order, and the currencies themselves are
   * carried through unchanged since the operation is on the numbers alone. It is an ordinary
   * function rather than the primitive-specialised interface of the library being ported, which
   * is the same thing expressed in this language.
   *
   * The operation may produce a value that is not a number, exactly as the arrays of this run may
   * hold one; it is reading such a value back as an amount that raises the invariant of
   * [[CurrencyAmount]], not producing it here.
   *
   * @param mapper  the operation to apply to each value
   * @return a copy of this run with the operation applied to every value
   */
  def mapAmounts(mapper: Double => Double): MultiCurrencyAmountArray =
    mapValues(currencyValues => currencyValues.map(mapper))

  //-------------------------------------------------------------------------
  /**
   * Converts this run into the specified currency, taking the rates from the specified provider.
   *
   * This is the [[FxConvertible]] implementation of this type, and the result is a
   * [[CurrencyAmountArray]] rather than another run of this type: converting collapses the
   * currency dimension, so what is left is one array of values in one currency, exactly as
   * converting a [[MultiCurrencyAmount]] leaves one [[CurrencyAmount]].
   *
   * The arithmetic is the arithmetic of the implementation being ported, in its order: the result
   * starts as zeroes, and each currency's values, multiplied by that currency's rate, are added
   * to it, the currencies taken in the order of their codes. Floating point addition is
   * order-sensitive, so stating the order is what makes the result reproducible and what lets it
   * be compared against a captured baseline.
   *
   * The rate of a currency is asked for exactly once and applied to that currency's whole array,
   * where the implementation being ported asked once per element. For a provider whose answers
   * are a function of the pair - which every provider of this port is - the two produce identical
   * numbers, and asking once is both faster and what guarantees that a whole run is converted at
   * a single rate. A single unavailable rate fails the whole conversion, carrying the failure the
   * provider reported, and no later rate is asked for: a partially converted run would be
   * numbers with no meaning.
   *
   * The rate is asked for even when a currency of the run is the result currency, as it was
   * there, and the providers of this port answer that with one.
   *
   * @param resultCurrency  the currency of the result
   * @param rateProvider  the provider of FX rates
   * @return this run expressed in the result currency, or the failure the provider reported for
   *   the first rate the conversion needed and could not get
   */
  override def convertedTo(
      resultCurrency: Currency,
      rateProvider: FxRateProvider): FailureOr[CurrencyAmountArray] =
    values.toList
      .traverse { case (currency, currencyValues) =>
        rateProvider.fxRate(currency, resultCurrency).map(rate => currencyValues.multipliedBy(rate))
      }
      .map(scaled =>
        CurrencyAmountArray.of(
          resultCurrency,
          scaled.foldLeft(DoubleArray.filled(size))((total, next) => total.plus(next))))

  //-------------------------------------------------------------------------
  /**
   * Returns a run holding the values of this run added to the values of the other run.
   *
   * The values are added element by element and currency by currency, so the amount at each index
   * of the result is the sum of the amounts at that index of the two runs. A currency only one of
   * the runs holds is carried through as it stands, which is the same statement read through the
   * zero padding of this type: the other run holds zero for that currency at every index, and
   * adding zero changes nothing.
   *
   * The two runs have to have the same size, since there is no value to add at an index only one
   * of them has. That is a property of the runs a caller holds, so it is reported:
   *
   * {{{
   * twoLong.plus(twoLong)     // Right(the element-wise sum, over the union of the currencies)
   * twoLong.plus(threeLong)   // Left(Failure.Invalid(
   *                           //   "Sizes must be equal, this size is 2, other size is 3"))
   * }}}
   *
   * The wording is that of the implementation being ported. The result is built through the
   * checking factory of this type, as it was there, so the structural properties of this type are
   * established for the result rather than assumed of it.
   *
   * @param other  the other run of values
   * @return this run with the other added element by element, or the failure describing the size
   *   mismatch
   */
  def plus(other: MultiCurrencyAmountArray): FailureOr[MultiCurrencyAmountArray] =
    sameSize(other).flatMap(_ =>
      MultiCurrencyAmountArray.checkedOne(
        size,
        merged(other.values, (mine, theirs) => mine.plus(theirs), theirs => theirs)))

  /**
   * Returns a run holding the values of this run with the specified amount added.
   *
   * The amount is added to every index, so this shifts the whole run by one multi-currency
   * amount rather than combining two runs. A currency the amount names and the run does not
   * becomes a currency of the result, holding that one value at every index; a currency the run
   * holds and the amount does not is carried through unchanged. There is no size to disagree
   * about, since an amount describes every index.
   *
   * {{{
   * twoLongInGbp.plus(multiOfGbpAndUsd)   // Right(GBP shifted, USD filled with the USD amount)
   * }}}
   *
   * The result is built through the checking factory of this type, as the implementation being
   * ported built it, which is why this reports a failure channel it cannot currently fill: the
   * arrays it assembles all have the size of this run by construction. Keeping the channel is
   * what makes that a property of the factory rather than of a reading of this method, so a check
   * added to the factory needs no change here.
   *
   * @param amount  the amount to add to every index
   * @return this run with the amount added at every index, or the failure the checking factory
   *   reported
   */
  def plus(amount: MultiCurrencyAmount): FailureOr[MultiCurrencyAmountArray] =
    MultiCurrencyAmountArray.checkedOne(
      size,
      mergedWithAmount(
        amount,
        (mine, value) => mine.plus(value),
        value => DoubleArray.filled(size, value)))

  /**
   * Returns a run holding the values of this run with the values of the other run subtracted.
   *
   * This is the element-wise `plus` above read in the other direction, and it behaves the same
   * way in every respect but one: a currency
   * only the other run holds is carried through negated rather than as it stands, since nothing
   * minus that currency's values is their negation. That was the behaviour of the implementation
   * being ported, and it is the zero padding of this type again - this run holds zero for that
   * currency at every index.
   *
   * The sizes have to be equal, for the reason element-wise addition gives, and the failure is
   * worded identically.
   *
   * @param other  the other run of values
   * @return this run with the other subtracted element by element, or the failure describing the
   *   size mismatch
   */
  def minus(other: MultiCurrencyAmountArray): FailureOr[MultiCurrencyAmountArray] =
    sameSize(other).flatMap(_ =>
      MultiCurrencyAmountArray.checkedOne(
        size,
        merged(
          other.values,
          (mine, theirs) => mine.minus(theirs),
          theirs => theirs.multipliedBy(-1d))))

  /**
   * Returns a run holding the values of this run with the specified amount subtracted.
   *
   * This is the `plus` of an amount above read in the other direction and behaves the same way in
   * every respect: the amount is subtracted at
   * every index, a currency only the amount names becomes a currency of the result - holding the
   * negation of that value at every index - and a currency only the run holds is carried through
   * unchanged.
   *
   * @param amount  the amount to subtract at every index
   * @return this run with the amount subtracted at every index, or the failure the checking
   *   factory reported
   */
  def minus(amount: MultiCurrencyAmount): FailureOr[MultiCurrencyAmountArray] =
    MultiCurrencyAmountArray.checkedOne(
      size,
      mergedWithAmount(
        amount,
        (mine, value) => mine.minus(value),
        value => DoubleArray.filled(size, -value)))

  //-------------------------------------------------------------------------
  /**
   * Applies an operation to the values of every currency, keeping the currencies and the size.
   *
   * This is the shape of the two total arithmetic members above. The operation is applied per
   * currency, in the order of the currency codes, and it may not change the length of an array -
   * every operation that reaches it is element-wise - so the structural properties of this type
   * carry over to the result and the unchecked construction is the right one.
   *
   * @param operation  the operation to apply to each currency's values
   * @return this run with the operation applied to the values of every currency
   */
  private def mapValues(operation: DoubleArray => DoubleArray): MultiCurrencyAmountArray =
    MultiCurrencyAmountArray.create(
      size,
      SortedMap.from(values.iterator.map { case (currency, currencyValues) =>
        (currency, operation(currencyValues))
      })(MultiCurrencyAmountArray.currencyOrdering))

  /**
   * Combines the values of this run with those of another run, currency by currency.
   *
   * The union of the two sets of currencies is taken: a currency both runs hold is combined with
   * the first operation, a currency only this run holds is carried through as it stands, and a
   * currency only the other run holds goes through the second operation. That is exactly the
   * three-way choice the implementation being ported made, and the two operations are what
   * distinguish addition from subtraction.
   *
   * The currencies of this run are taken first, in the order of their codes, then those only the
   * other run holds, likewise ordered; the result is sorted regardless, so the traversal order is
   * chosen for determinism of the failures a caller may see rather than for the result itself.
   *
   * @param otherValues  the values of the other run, which has the same size as this one
   * @param onBoth  the operation applied to the values of a currency both runs hold
   * @param onOtherOnly  the operation applied to the values of a currency only the other run
   *   holds
   * @return the combined values per currency
   */
  private def merged(
      otherValues: SortedMap[Currency, DoubleArray],
      onBoth: (DoubleArray, DoubleArray) => DoubleArray,
      onOtherOnly: DoubleArray => DoubleArray): SortedMap[Currency, DoubleArray] = {
    val fromThis = values.iterator.map { case (currency, mine) =>
      (currency, otherValues.get(currency).fold(mine)(theirs => onBoth(mine, theirs)))
    }
    val fromOtherOnly = otherValues.iterator.collect {
      case (currency, theirs) if !values.contains(currency) => (currency, onOtherOnly(theirs))
    }
    SortedMap.from(fromThis ++ fromOtherOnly)(MultiCurrencyAmountArray.currencyOrdering)
  }

  /**
   * Combines the values of this run with a single multi-currency amount, currency by currency.
   *
   * This is [[merged]] for the case where the other operand describes every index with one value
   * per currency rather than one array per currency, which is the shape the two members that take
   * an amount need. The three-way choice is the same one, and a currency only the amount names
   * produces a full-length array through the second operation - filled with that value for
   * addition, with its negation for subtraction - which is how the implementation being ported
   * introduced such a currency.
   *
   * @param amount  the amount to combine with this run
   * @param onBoth  the operation applied to the values of a currency both operands name
   * @param onAmountOnly  the operation producing the values of a currency only the amount names
   * @return the combined values per currency
   */
  private def mergedWithAmount(
      amount: MultiCurrencyAmount,
      onBoth: (DoubleArray, Double) => DoubleArray,
      onAmountOnly: Double => DoubleArray): SortedMap[Currency, DoubleArray] = {
    val amountsByCurrency = amount.toMap
    val fromThis = values.iterator.map { case (currency, mine) =>
      (currency, amountsByCurrency.get(currency).fold(mine)(value => onBoth(mine, value)))
    }
    val fromAmountOnly = amountsByCurrency.iterator.collect {
      case (currency, value) if !values.contains(currency) => (currency, onAmountOnly(value))
    }
    SortedMap.from(fromThis ++ fromAmountOnly)(MultiCurrencyAmountArray.currencyOrdering)
  }

  /**
   * Checks that the other run has the same size as this one.
   *
   * Both members that combine two runs share this, so the two report the same reason in the same
   * wording - that of the implementation being ported.
   *
   * @param other  the other run
   * @return nothing when the two runs have the same size, otherwise the failure describing the
   *   mismatch
   */
  private def sameSize(other: MultiCurrencyAmountArray): FailureOr[Unit] =
    if (other.size == size) {
      Right(())
    } else {
      Left(MultiCurrencyAmountArray.differentSizes(size, other.size))
    }

  //-------------------------------------------------------------------------
  /**
   * Checks whether this run equals another object.
   *
   * Another run is equal when it has the same size and holds the same currencies with values that
   * are equal element by element. The values are compared by [[DoubleArray]], whose equality is
   * bit for bit, so a value that is not a number equals itself and a negative zero differs from a
   * positive zero - the comparison the generated bean of the implementation being ported
   * performed. The map comparison is by content, so it does not matter in what order either run
   * was built. An object of any other type is not equal.
   *
   * @param obj  the object to compare to
   * @return true if the other object is a run of the same size holding the same values per
   *   currency
   */
  override def equals(obj: Any): Boolean = obj match {
    case other: MultiCurrencyAmountArray =>
      (this eq other) || (size == other.size && values == other.values)
    case _ => false
  }

  /**
   * Returns a hash code consistent with [[equals]].
   *
   * The mixing is that of the bean this replaces - a seed, then each field in declaration order -
   * with the map contributing the hash of its entries, each of which is the hash of a currency
   * code and the hash of a [[DoubleArray]], computed from the bit patterns of its elements.
   * Everything it reads is a function of the value alone, so the hash of a run is identical in
   * every run of every program, which is what the byte-stability and round-trip properties of the
   * test suite depend on, and it agrees with the equality above.
   *
   * @return the hash code of the size and the values held
   */
  override def hashCode: Int =
    (MultiCurrencyAmountArray.HashSeed * 31 + size) * 31 + values.hashCode

  /**
   * Returns this run as text.
   *
   * The form is the one the generated bean produced, the two fields named in declaration order
   * between braces, with the values rendered as the sorted map of the library being ported
   * rendered them:
   *
   * {{{
   * MultiCurrencyAmountArray{size=2, values={GBP=[1.0, 0.0], USD=[0.0, 2.0]}}
   * }}}
   *
   * It is kept exactly so that ported code and the logs it writes read as they did before, and it
   * is what the `Show` instance of the companion renders. Every value of every array appears, so
   * the text of a long run is long - it describes the whole value rather than summarising it, as
   * the text of the implementation being ported did.
   *
   * @return the rendering of this run
   */
  override def toString: String =
    s"MultiCurrencyAmountArray{size=$size, values=$renderedValues}"

  /**
   * Renders the values as the sorted map of the library being ported rendered them.
   *
   * That library wrote a map as its entries between braces, each entry as its key, an equals sign
   * and its value, separated by commas - which is not how a map of this language renders itself,
   * hence rendering it here rather than interpolating it.
   *
   * @return the rendering of the values per currency
   */
  private def renderedValues: String =
    values.iterator
      .map { case (currency, currencyValues) => s"$currency=$currencyValues" }
      .mkString("{", ", ", "}")
}

/**
 * Provides the ways of obtaining a run of multi-currency amounts, and the instances for the type.
 *
 * This companion is the only place a [[MultiCurrencyAmountArray]] is created. Neither the
 * constructor nor a generated `apply` or `copy` is available and the type is sealed, so a run
 * whose arrays disagree about their length, or whose size does not match them, cannot exist: the
 * factory that reads such input reports it instead.
 *
 * ===The four ways in===
 *
 * Three of them build the values themselves and are total, because a collection of amounts, or a
 * function producing them, always describes a run - the currencies are simply padded with zero
 * wherever an amount does not name them:
 *
 *   - `of(amounts*)` and `of(amounts)` read amounts that already exist,
 *   - `of(size, valueFunction)` produces them from the index.
 *
 * The fourth reads the representation directly, one array per currency, and is therefore the one
 * that has something to check - that the arrays agree about their length:
 *
 *   - `of(values)` reports what it rejects as a chain of [[Failure]].
 *
 * [[total]] is the aggregating factory: it combines runs of single-currency amounts into one run,
 * adding the arrays of a currency that appears more than once. It inherits the failure channel of
 * the fourth, since arrays of different lengths cannot be combined or held together.
 *
 * ===Instances===
 *
 * Two typeclass instances are published, and exactly two: a `Hash`, which is the single
 * equality-bearing instance of the type - `Hash` extends `Eq`, so declaring an `Eq` as well would
 * leave two instances that could disagree and one of them ambiguous - and a `Show`. There is
 * deliberately no `Order`, matching the implementation being ported, which is not comparable.
 *
 * @see [[MultiCurrencyAmountArray]] for the type itself, the two structural properties every run
 *   has, and the zero padding the three total factories apply
 */
object MultiCurrencyAmountArray {

  /**
   * The seed of the hash code, standing in for the identity hash of the class that the generated
   * bean used, so that hashing is reproducible across runs. Any constant would do; the hash of
   * the type name is used because it is stable, specified by the platform, and distinct from the
   * seed of every other type of this port.
   */
  private val HashSeed: Int = "MultiCurrencyAmountArray".hashCode

  /**
   * The name of the size, as the argument name of the checks that read it.
   *
   * This is the name the implementation being ported gave the same property, so the message
   * reported for a size the type does not admit is worded as it was there.
   */
  private val SizeField: String = "size"

  /**
   * The ordering the values of every run are held in.
   *
   * It is the cats `Order` of [[Currency]] turned into an `Ordering`, so the order of the
   * currencies of a run, of the entries of its JSON object, of its rendering and of the rates
   * asked for by a conversion all come from the one source of truth for comparing currencies -
   * their codes. The implementation being ported held the same map sorted the same way.
   */
  private val currencyOrdering: Ordering[Currency] = Order[Currency].toOrdering

  /**
   * The empty set of totals, which is where [[total]] starts.
   *
   * It is held once rather than built per call, and its ordering is the ordering above so that
   * the aggregation is performed and reported in currency order.
   */
  private val noTotals: FailureOr[SortedMap[Currency, CurrencyAmountArray]] =
    Right(SortedMap.empty[Currency, CurrencyAmountArray](currencyOrdering))

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance from the specified multi-currency amounts.
   *
   * The amounts are taken in the order they are given, one per index, and the size of the run is
   * how many there are. This is the varargs form of the `of` that takes a collection of amounts,
   * below, and behaves identically, including its zero padding:
   *
   * {{{
   * MultiCurrencyAmountArray.of(oneGbp, twoUsd)   // size 2, GBP [1.0, 0.0], USD [0.0, 2.0]
   * MultiCurrencyAmountArray.of()                 // size 0, no currencies
   * }}}
   *
   * @param amounts  the amounts, one per index of the run
   * @return the run holding those amounts
   */
  def of(amounts: MultiCurrencyAmount*): MultiCurrencyAmountArray = fromAmounts(amounts.toVector)

  /**
   * Obtains an instance from the specified collection of multi-currency amounts.
   *
   * The amounts are taken in the order the collection presents them, one per index, and the size
   * of the run is how many there are. This is total: a collection of amounts always describes a
   * run, and an empty collection describes the run of size zero.
   *
   * Every currency any amount names becomes a currency of the run, holding that amount's value
   * at the indices where the amount names it and zero at every other index - the zero padding
   * described on this type, which is what the implementation being ported did by allocating a
   * full-length array for a currency the first time it saw it:
   *
   * {{{
   * MultiCurrencyAmountArray.of(List(oneGbp, twoUsd))
   *   // size 2, GBP [1.0, 0.0], USD [0.0, 2.0]
   * }}}
   *
   * The collection is read once, into an indexed sequence, so a lazy or single-use collection is
   * traversed exactly once even though the values of each currency are then built per currency.
   * The implementation being ported took a `List`, which is one of the collections accepted here.
   *
   * @param amounts  the amounts, one per index of the run
   * @return the run holding those amounts
   */
  def of(amounts: Iterable[MultiCurrencyAmount]): MultiCurrencyAmountArray =
    fromAmounts(amounts.toVector)

  /**
   * Obtains an instance using a function to create the amounts.
   *
   * The function is passed each index, counting from zero, and returns the amount at that index.
   * The amounts are then read exactly as the collection factory above reads them, zero padding
   * included:
   *
   * {{{
   * MultiCurrencyAmountArray.of(3, index => multiOf(index.toDouble))
   * }}}
   *
   * The function is evaluated exactly once per index, in index order. That is worth stating
   * because the values are held per currency: producing each currency's array by calling the
   * function again would evaluate it once per currency and index, which would be wrong for a
   * function that counts its calls, reads a sequence of inputs or is expensive. The amounts are
   * therefore materialised once, in order, and the arrays are built from them - the same single
   * pass over the input the implementation being ported made.
   *
   * A negative size is refused as a caller contract rather than reported as a failure: there is
   * no data a caller could hold that makes a run of minus one amounts meaningful, so this is an
   * invariant of the call and not a property of its input. That is the contract the
   * implementation being ported declared - its size was declared as a property that must not be
   * negative, and this factory was documented as raising an argument exception for a size it does
   * not admit - but not what its code did: the constructor written by hand for that class skipped
   * the validation its declaration asked for, so a negative size there produced a run whose size
   * contradicted its values and whose every index was unreadable. Enforcing the declared contract
   * is the one behaviour of this factory that differs from that code, it is recorded as a
   * divergence with the port, and it is what lets the two structural properties of this type hold
   * without exception - the decoder refuses a negative size for the same reason.
   *
   * A size of zero is admitted and describes the run of size zero, as it did there; the exception
   * the documentation of that factory promised for a size of zero was never raised by it, and is
   * not raised here.
   *
   * @param size  the number of amounts, zero or greater
   * @param valueFunction  the function used to obtain the amount at each index
   * @return the run holding the amounts the function produced
   * @throws java.lang.IllegalArgumentException if the size is negative
   */
  def of(size: Int, valueFunction: Int => MultiCurrencyAmount): MultiCurrencyAmountArray = {
    ArgCheck.notNegative(size, SizeField)
    fromAmounts(Vector.tabulate(size)(valueFunction))
  }

  /**
   * Obtains an instance from a map of values per currency.
   *
   * This is the factory that reads the representation of the type directly, and the one that has
   * something to decide: every array has to hold the same number of elements, since that number
   * is the size of the run and a currency is held for the whole run or not at all. The size is
   * taken from the array of the first currency in code order, and every other array is compared
   * against it:
   *
   * {{{
   * MultiCurrencyAmountArray.of(Map(gbp -> twoValues, usd -> twoValues))   // Right(size 2)
   * MultiCurrencyAmountArray.of(Map(gbp -> twoValues, usd -> threeValues))
   *   // Left(Failure.Invalid("Arrays must have the same size but found sizes 2 and 3"))
   * MultiCurrencyAmountArray.of(Map.empty)                                 // Right(size 0)
   * }}}
   *
   * An empty map describes the run of size zero, as it did in the implementation being ported,
   * which documented exactly that; a run of non-zero size holding no currency is built with one
   * of the other factories.
   *
   * Every array that disagrees is reported, not only the first, so a caller reads one chain
   * naming each of them. The wording of each is that of the implementation being ported, which
   * reported the size it had settled on and the size it found; which array is the reference is
   * settled by the order of the currency codes rather than by the iteration order of the map
   * handed in, so the same map always produces the same reasons in the same order - the
   * implementation being ported read an unordered map here and its message depended on that
   * order.
   *
   * @param values  the values per currency, each array holding one value per index of the run
   * @return the run holding those values, or the failures describing the arrays that disagree
   *   about their length
   */
  def of(values: Map[Currency, DoubleArray]): ResultNec[MultiCurrencyAmountArray] = {
    val sorted = sortedValues(values)
    // the size of the first array in currency order, and zero when there is no array at all
    checked(sorted.headOption.fold(0) { case (_, currencyValues) => currencyValues.size }, sorted)
  }

  /**
   * Returns the run representing the total of the specified runs of single-currency amounts.
   *
   * Each [[CurrencyAmountArray]] contributes its values under its own currency, and a currency
   * that appears more than once has its runs added element by element, in the order the input
   * presents them - the aggregation the implementation being ported performed with a collector,
   * whose role this member takes over. Floating point addition is order-sensitive, so stating
   * that order is what makes the total reproducible.
   *
   * {{{
   * MultiCurrencyAmountArray.total(List(gbpRun, usdRun))          // Right(GBP and USD)
   * MultiCurrencyAmountArray.total(List(gbpRun, gbpRun))          // Right(GBP doubled)
   * MultiCurrencyAmountArray.total(Nil)                           // Right(size 0)
   * MultiCurrencyAmountArray.total(List(twoLongRun, threeLongRun)) // Left(the size mismatch)
   * }}}
   *
   * All the runs have to have the same length: two runs of one currency cannot be added
   * element-wise if they differ, and runs of different currencies cannot be held together in one
   * value of this type if they differ. The first is reported by [[CurrencyAmountArray.plus]] and
   * the second by the checking factory above, so a mixture of lengths is reported whichever pair
   * meets first. An empty input describes the run of size zero.
   *
   * @param arrays  the runs to total, in any number and any order
   * @return the total of those runs, or the failures describing the lengths that disagree
   */
  def total(arrays: Iterable[CurrencyAmountArray]): ResultNec[MultiCurrencyAmountArray] = {
    val totalled = arrays.foldLeft(noTotals) { (accumulated, next) =>
      accumulated.flatMap { totals =>
        totals.get(next.currency) match {
          case Some(existing) =>
            existing.plus(next).map(summed => totals.updated(next.currency, summed))
          case None =>
            Right(totals.updated(next.currency, next))
        }
      }
    }
    toNec(totalled).flatMap(totals =>
      of(totals.iterator.map { case (currency, array) => (currency, array.values) }.toMap))
  }

  //-------------------------------------------------------------------------
  /**
   * Builds a run from amounts that have already been read into an indexed sequence.
   *
   * This is where the three total factories meet, and it is where the size of the run is settled:
   * one index per amount. It needs no check - [[arraysOf]] produces one array of exactly that
   * length per currency - so it is the unchecked construction.
   *
   * @param amounts  the amounts, one per index, in index order
   * @return the run holding those amounts
   */
  private def fromAmounts(amounts: Vector[MultiCurrencyAmount]): MultiCurrencyAmountArray =
    create(amounts.size, arraysOf(amounts))

  /**
   * Turns amounts into one full-length array of values per currency.
   *
   * This is the zero padding of the three total factories, written as the pure counterpart of the
   * buffer the implementation being ported allocated per currency and wrote into: the currencies
   * of all the amounts are collected first, and then each currency's array is produced from the
   * amounts by index. `getAmountOrZero` is what makes it faithful - it is the accessor of
   * [[MultiCurrencyAmount]] that reads an absent currency as zero of it, which is exactly what an
   * untouched element of that buffer held.
   *
   * Each array is built in one pass over the amounts with no buffer being handed out, which is
   * what the copy-safe construction of [[DoubleArray]] requires: the escape hatches the
   * implementation being ported used to wrap its buffers are not available outside that module.
   *
   * @param amounts  the amounts, one per index, in index order
   * @return one array per currency any amount names, each holding one value per amount
   */
  private def arraysOf(amounts: Vector[MultiCurrencyAmount]): SortedMap[Currency, DoubleArray] = {
    val currencies = amounts.foldLeft(SortedSet.empty[Currency](currencyOrdering))((seen, amount) =>
      seen ++ amount.getCurrencies)
    SortedMap.from(currencies.iterator.map(currency =>
      (
        currency,
        DoubleArray.tabulate(amounts.size)(index =>
          amounts(index).getAmountOrZero(currency).amount))))(currencyOrdering)
  }

  /**
   * Builds a run after establishing both of its structural properties, reporting what it rejects.
   *
   * This is the single checking route into the type. It is what the `of` that reads a map of
   * values per currency performs, what the four arithmetic members that combine values route
   * their results through -
   * as the implementation being ported routed its own through its `of` - and what the decoder
   * hands a payload to, which is why a document may not declare a size its arrays do not have.
   *
   * Both classes of reason are accumulated rather than sequenced, so a caller reads every reason
   * its input gives: the size being negative, and each array whose length is not that size. The
   * arrays are examined in currency order, so the reasons are ordered the same way for the same
   * input.
   *
   * @param size  the size the run is to have, zero or greater
   * @param values  the values per currency, ordered by currency code
   * @return the run, or the failures describing the size and the arrays that disagree with it
   */
  private def checked(
      size: Int,
      values: SortedMap[Currency, DoubleArray]): ResultNec[MultiCurrencyAmountArray] = {
    val negativeSize = Validate.notNegative(size, SizeField).toEither.left.toOption
    val differingLengths = Collections.toNonEmptyChain(
      values.iterator
        .collect {
          case (_, currencyValues) if currencyValues.size != size => currencyValues.size
        }
        .map(found => differentArraySizes(size, found)))
    Collections
      .concatNonEmptyChains(List(negativeSize, differingLengths).flatten)
      .toLeft(create(size, values))
  }

  /**
   * Builds a run as [[checked]] does, reporting a single failure rather than a chain.
   *
   * The four arithmetic members that combine values report one reason, since the size mismatch
   * they check for themselves is one reason; routing their results through this keeps the shape
   * of what they return while the structural properties of the result are still established by
   * the one checking route. The chain is collapsed by [[Failure.collapse]], which joins the
   * reasons of several failures into one - it is reached only if an arithmetic member ever
   * assembles arrays of differing lengths, which its own size check prevents.
   *
   * @param size  the size the run is to have
   * @param values  the values per currency, ordered by currency code
   * @return the run, or the single failure describing why those values do not describe one
   */
  private def checkedOne(
      size: Int,
      values: SortedMap[Currency, DoubleArray]): FailureOr[MultiCurrencyAmountArray] =
    checked(size, values).left.map(Failure.collapse)

  /**
   * Builds a run without checking anything, which is the only call of the constructor.
   *
   * Every route into the type ends here, and each of them has already established the two
   * structural properties: the total factories build one array of the run's length per currency,
   * the element-wise arithmetic preserves the lengths of the arrays it is given, and [[checked]]
   * has just examined them. Keeping the construction in one place is what makes that reviewable.
   *
   * @param size  the size of the run
   * @param values  the values per currency, each holding exactly `size` elements
   * @return the run
   */
  private def create(
      size: Int,
      values: SortedMap[Currency, DoubleArray]): MultiCurrencyAmountArray =
    new MultiCurrencyAmountArray(size, values) {}

  /**
   * Sorts values by currency code, whatever collection they arrive in.
   *
   * Every run holds its values in code order, so this is applied to the map a caller hands to the
   * checking factory and to the map a document decodes into. Doing it before anything is examined
   * is what makes the reasons reported deterministic, and doing it in one place is what makes the
   * ordering of a run a property of this companion rather than of each route into it.
   *
   * @param values  the values per currency, in any order
   * @return the same values, ordered by currency code
   */
  private def sortedValues(
      values: Map[Currency, DoubleArray]): SortedMap[Currency, DoubleArray] =
    SortedMap.from(values)(currencyOrdering)

  //-------------------------------------------------------------------------
  /**
   * The failure reported when a currency's values are read from a run that does not hold it.
   *
   * The wording is that of the implementation being ported, so a log or an expectation carrying
   * the message of the exception it raised carries this message unchanged.
   *
   * @param currency  the currency the run does not hold
   * @return the failure naming that currency
   */
  private def noValues(currency: Currency): Failure =
    Failure.Invalid(s"No values available for $currency")

  /**
   * The failure reported when two runs that have to be combined have different sizes.
   *
   * The wording is that of the implementation being ported, and it is the wording
   * [[CurrencyAmountArray]] reports for the same disagreement, as it was there.
   *
   * @param thisSize  the size of the run the operation was called on
   * @param otherSize  the size of the other run
   * @return the failure describing the mismatch
   */
  private def differentSizes(thisSize: Int, otherSize: Int): Failure =
    Failure.Invalid(s"Sizes must be equal, this size is $thisSize, other size is $otherSize")

  /**
   * The failure reported when the arrays of a map do not all hold the same number of values.
   *
   * The wording is that of the implementation being ported, which named the length it had settled
   * on and the length it found.
   *
   * @param expected  the length the run is to have
   * @param found  the length of the array that disagrees with it
   * @return the failure describing the disagreement
   */
  private def differentArraySizes(expected: Int, found: Int): Failure =
    Failure.Invalid(s"Arrays must have the same size but found sizes $expected and $found")

  //-------------------------------------------------------------------------
  /**
   * The hashing and equality of runs of multi-currency amounts.
   *
   * Taken from the `equals` and `hashCode` of the type, which compare the size and then the
   * values of each currency element by element on their bit patterns. This is the type's only
   * equality-bearing instance, and `Eq[MultiCurrencyAmountArray]` is obtained from it by
   * subtyping.
   *
   * @return the hashing of runs of multi-currency amounts
   */
  implicit val hash: Hash[MultiCurrencyAmountArray] =
    Hash.fromUniversalHashCode[MultiCurrencyAmountArray]

  /**
   * The rendering of runs of multi-currency amounts as text.
   *
   * Renders what `toString` renders, which is the form of the implementation being ported.
   *
   * @return the rendering of a run
   */
  implicit val show: Show[MultiCurrencyAmountArray] = Show.show(_.toString)

  //-------------------------------------------------------------------------
  // The codec of the arrays, brought into scope for the two derivations below and for nothing
  // else. It has to be taken from here rather than from the JSON library, which has no instance
  // for an array of this port at all and whose instance for a double cannot express a value that
  // is not a number; importing them at this point is what makes that choice deliberate and local,
  // as the codec support of `strata-collect` intends.
  import Codecs.implicits._

  /**
   * The raw field shape both codecs of this type are derived over.
   *
   * A type whose constructor is private cannot be derived over directly, so this product is the
   * shape of its two fields and is what makes the derivation possible in both directions: the
   * decoder below derives over it and then builds through the checking factory, and the encoder
   * below derives over it and is contramapped from a run that already exists. It exists only for
   * that purpose - it is private and it is never returned, so no caller can hold an unchecked
   * pair. Its field names are the JSON keys, and they are the names of the two fields of
   * [[MultiCurrencyAmountArray]] itself, which is what keeps the derived shape and the type from
   * drifting apart.
   *
   * The values are typed as an unordered map here rather than as the sorted map the type holds,
   * for two reasons: a document may present the currencies in any order, and decoding into a
   * sorted map would require this file to place an ordering of currencies in implicit scope,
   * where it could be picked up by an unrelated derivation. The map a run hands over is already
   * sorted, and a map is encoded in its own iteration order, so the output is ordered by currency
   * code without this shape having to say so.
   *
   * @param size  the size of the run
   * @param values  the values per currency, each currency read from its three letter code and
   *   each array read as a JSON array whose elements are numbers or one of the three tagged
   *   strings
   */
  private final case class Raw(size: Int, values: Map[Currency, DoubleArray])

  /** The derived decoder of the raw field shape, used by the checking decoder below. */
  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /**
   * The derived encoder of the raw field shape, used by the encoder below.
   *
   * This sits after the import above deliberately: the derivation picks up the array codec of
   * this port from that import, which is what writes an infinite element as its tagged string. A
   * derivation site without that import in scope would not compile at all, the JSON library
   * having no encoder for the array type.
   */
  private val rawEncoder: Encoder.AsObject[Raw] = deriveEncoder[Raw]

  /**
   * The JSON encoding of runs of multi-currency amounts.
   *
   * A run is an object of two fields, its size and its values as an object keyed by currency
   * code:
   *
   * {{{
   * {"size":2,"values":{"GBP":[1.0,0.0],"USD":[0.0,"Infinity"]}}
   * }}}
   *
   * The currencies appear in the order of their codes. That is not a convention of the encoder
   * but a property of the value being encoded - a run holds its values in a map sorted by
   * currency - so two runs built from the same data in different orders encode to identical
   * bytes, which is what makes the byte-stability property of the test suite hold for this type
   * without anything being sorted here.
   *
   * The shape is derived when this file is compiled rather than written out field by field, which
   * is what every product of this port does, and no part of it inspects a class while the program
   * runs. Every element of every array goes through the single policy of this port for a double,
   * so a value that is not a number and the two infinities appear as the strings `"NaN"`,
   * `"Infinity"` and `"-Infinity"` while a finite value is written as a JSON number, exactly to
   * the bit - which is what lets every run this type admits survive a round trip. The result is
   * wrapped so that a field holding no value would be omitted, the policy every product of this
   * port follows; this type has no optional field, so the wrapping changes nothing about its
   * output and exists so that the policy holds without exception.
   *
   * @return the JSON encoding of a run
   */
  implicit val encoder: Encoder[MultiCurrencyAmountArray] =
    Codecs.dropNulls(
      rawEncoder.contramap[MultiCurrencyAmountArray](value => Raw(value.size, value.values)))

  /**
   * The JSON decoding of runs of multi-currency amounts.
   *
   * This is the inverse of the encoding above and is likewise derived at compile time. Both
   * fields have to be present, and the payload is read into the raw shape and handed to the
   * checking factory, so the only route into the type from a document is the route a caller
   * takes: a document declaring a size its arrays do not have, or a negative size, is a decoding
   * failure carrying those reasons rather than a run this type would not have built. What a
   * document can get wrong inside a field is refused by the field codecs, which report a currency
   * code this port does not hold and an element that is neither a number nor one of the three
   * tagged strings.
   *
   * The size is taken from the payload rather than derived from the arrays, which is what lets a
   * run of non-zero size holding no currency - a legitimate value of this type, built by the
   * factories that take a size - survive a round trip.
   *
   * A document may list the currencies in any order, since the run they describe does not depend
   * on it; re-encoding what was decoded puts them in code order.
   *
   * @return the JSON decoding of a run
   */
  implicit val decoder: Decoder[MultiCurrencyAmountArray] =
    Codecs.validatedDecoder[Raw, MultiCurrencyAmountArray](raw =>
      checked(raw.size, sortedValues(raw.values)))(rawDecoder)
}
