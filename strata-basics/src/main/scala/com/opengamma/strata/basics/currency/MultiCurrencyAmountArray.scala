/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

import cats.Hash
import cats.Order
import cats.Show

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.Collections
import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.NoJavaSerialization
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
 * Three properties hold of every value of this type, and each is established before a value
 * exists rather than checked when one is read:
 *
 *   - the size is zero or greater,
 *   - every array of values has exactly that many elements, one per index of the run, and
 *   - every element of every array is a value [[CurrencyAmount]] holds.
 *
 * The second is why a currency and an index always name a number: a currency that is held at all
 * is held for the whole run. The third is why that number is always an amount: the elements of a
 * run are amounts kept as numbers, so a value that is not a number is no more an element than it
 * is an amount - the two infinities are held, as an amount holds them, and only a not-a-number
 * value is refused. All three are the post-condition of the one checking factory - the `of` that
 * reads a map of values per currency - which every route that could break them goes through, and
 * of the one construction point behind it.
 *
 * Which channel reports a refused element is the channel the route in question already has,
 * which is the policy of this port for a numeric-domain edge: the checking factory, [[total]],
 * the four members that add or subtract, [[convertedTo]] and the decoder report
 * `Argument 'values' for GBP must not be NaN at index N` as a [[Failure]] - one per offending
 * currency, in currency order, beside any other reason the same input gives - while the routes
 * that are total in signature, [[multipliedBy]] and [[mapAmounts]], raise that same wording as a
 * documented invariant.
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
 * The padding is worth stating because it is observable: reading index zero back with [[get]]
 * answers `[GBP 1, USD 0]`, naming both currencies, and not `[GBP 1]`.
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
 * listing the amounts. One documented invariant can be raised by reading - an index outside the
 * run is the index exception of the runtime - and two more by the arithmetic that is total in
 * signature, where scaling or mapping produces a value that is not a number. Reading an index
 * cannot raise the invariant of [[CurrencyAmount]]: the element invariant above holds of every
 * value of this type, so every number a currency and an index name is already an amount.
 *
 * ===Equality===
 *
 * Two runs are equal when they have the same size and hold the same currencies with values that
 * are equal element by element, compared by bit pattern as [[DoubleArray]] compares them. That
 * differs from a numeric comparison in one deliberate place the round-trip properties of the test
 * suite rely on: a negative zero differs from a positive zero. The other place a bit comparison
 * differs, a value that is not a number comparing equal to itself, is unreachable here - the
 * element invariant above admits no such value - and it is [[DoubleArray]] itself, which does
 * admit one, that the property rests on. Because the currencies are held in a map, two runs built
 * from the same data in different orders are equal without anything having to be sorted when they
 * are compared.
 *
 * There is deliberately no ordering: a run of multi-currency amounts has no ordering worth
 * inventing.
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
    extends FxConvertible[CurrencyAmountArray]
    with NoJavaSerialization {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means can be
  // stopped is here. The single implementation is the companion's hidden `Impl`.
  JvmClosure.requireSoleImplementation(this, classOf[MultiCurrencyAmountArray.Impl])

  // The invariant of this type, stated over the fields the instance actually holds rather than
  // over the arguments a factory was given, because the implementation class carries a public
  // constructor in the class file whatever the source asked for: a class compiled outside this
  // library can call it directly, and identity alone would then admit a run whose size is not the
  // length of the arrays it holds - so that reading one scenario of it would reach past the end of
  // an array, or would read fewer currencies than the run has. These are the two structural
  // properties `MultiCurrencyAmountArray.checked` establishes and every other route into the type
  // holds by construction. The third is the element invariant: the elements of a run are amounts
  // kept as numbers, so a value that is not a number is no more an element of a run than it is an
  // amount, and identity alone would admit a run whose every later reader failed on it.
  //
  // The map is walked once per statement, over one entry per currency, and the third walks each
  // array on its bit patterns - all of which is less than the factory that builds the arrays has
  // already cost.
  JvmClosure.requireInvariant("its size is not negative", size >= 0)
  JvmClosure.requireInvariant(
    "it holds exactly one value per index of the run for each of its currencies",
    values.forall { case (_, currencyValues) => currencyValues.size == size })
  JvmClosure.requireInvariant(
    "every value of every currency it holds is a number",
    values.forall { case (_, currencyValues) => currencyValues.indexOf(Double.NaN) < 0 })

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
   * than left out, which is the visible consequence of the zero padding described on this type:
   *
   * {{{
   * MultiCurrencyAmountArray.of(oneGbp, twoUsd).get(0)   // [GBP 1, USD 0]
   * }}}
   *
   * This is total in signature, and it raises one documented invariant rather than widening into a
   * failure channel: an index outside the run is the index exception of the runtime, exactly as
   * reading the [[DoubleArray]] of a currency directly would be. The invariant of
   * [[CurrencyAmount]] cannot be raised from here, although the amount is built through the
   * construction path that performs it: the element invariant of this type holds of every value of
   * the type, so every number a currency and an index name is already an amount. A run holding no
   * currency answers with an empty amount for any index, since no array is read.
   *
   * ===What reconstructing an index costs===
   *
   * The currencies of the run are already distinct and already in code order, and each of their
   * values at the index is already a number, so there is nothing here to merge and nothing to
   * decide beyond the invariant of an amount. The raw pairs are therefore handed to the
   * package-private checked-map constructor of [[MultiCurrencyAmount]], which normalises and
   * checks each number as it goes into the one map the returned value holds: reading an index
   * builds the value it answers with and nothing else. Routing the pairs through the aggregating
   * factory instead would allocate a [[CurrencyAmount]] per currency for that factory to unwrap
   * again, and would re-merge entries that cannot collide, building a second map to reach a value
   * the first one already described.
   *
   * The invariant of an amount is applied by exactly the same computation either way - it is the
   * check the element invariant of this type has already established every value passes - and a
   * negative zero held in an array is normalised to a positive zero in the amount.
   *
   * @param index  the zero-based index to retrieve
   * @return the amount at that index, naming every currency of the run
   * @throws java.lang.IndexOutOfBoundsException if the index is outside the run and the run holds
   *   at least one currency
   */
  def get(index: Int): MultiCurrencyAmount =
    MultiCurrencyAmount.create(values.iterator.map { case (currency, currencyValues) =>
      // the numbers are read straight out of the arrays and the currencies are distinct and
      // sorted already, so the checked-map constructor of the amount is handed exactly the
      // entries of the value it returns: no amount object is built for it to unwrap, and no
      // merge is performed over entries that cannot collide
      (currency, currencyValues.get(index))
    })

  /**
   * Returns the amounts of this run, one at a time.
   *
   * The amounts are handed back as a lazy sequence that is traversed once, so a caller may
   * filter, map or fold them without ever holding the whole run of amounts:
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
   * This is total in signature, as it was in the implementation being ported, and it raises the
   * element invariant of this type for a product that is not a number - a factor of zero applied
   * to an infinite value, or a factor that is itself not a number - naming the currency and the
   * index of the first such product. That is the policy of this port for a numeric-domain edge
   * reached from the values a caller chose, and it is what the arithmetic of [[CurrencyAmount]]
   * does for the sum of two opposite infinities.
   *
   * @param factor  the multiplicative factor
   * @return a copy of this run with every value multiplied by the factor
   * @throws java.lang.IllegalArgumentException if a product is not a number
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
   * carried through unchanged since the operation is on the numbers alone.
   *
   * The operation may produce a value that is not a number, which is not a value the arrays of a
   * run hold: this is total in signature and raises the element invariant of this type for such
   * an image, naming the currency and the index, as [[multipliedBy]] does for such a product.
   * The refusal happens here, where the value was produced, rather than later where something
   * read it back.
   *
   * @param mapper  the operation to apply to each value
   * @return a copy of this run with the operation applied to every value
   * @throws java.lang.IllegalArgumentException if the operation produces a value that is not a
   *   number
   */
  def mapAmounts(mapper: Double => Double): MultiCurrencyAmountArray =
    mapValues(currencyValues => currencyValues.map(mapper))

  /**
   * Converts this run into the specified currency, taking the rates from the specified provider.
   *
   * This is the [[FxConvertible]] implementation of this type, and the result is a
   * [[CurrencyAmountArray]] rather than another run of this type: converting collapses the
   * currency dimension, so what is left is one array of values in one currency, exactly as
   * converting a [[MultiCurrencyAmount]] leaves one [[CurrencyAmount]].
   *
   * The arithmetic has a stated order: the value at an index starts at zero, and each currency's
   * value at that index, multiplied by that currency's rate, is added to it, the currencies taken
   * in the order of their codes. Floating point addition is order-sensitive, so stating the order
   * is what makes the result reproducible. That is also why the starting zero is stated: adding
   * into it is what turns a product of `-0.0` into a positive zero.
   *
   * The rate of a currency is asked for exactly once and applied to that currency's whole array,
   * which is both cheaper than one question per element and what guarantees that a whole run is
   * converted at a single rate. A single unavailable rate fails the whole conversion, carrying
   * the failure the provider reported, and no later rate is asked for: a partially converted run
   * would be numbers with no meaning.
   *
   * A currency of the run that is already the result currency is asked about like any other
   * rather than passed through unconverted, so the rate of a currency against itself is the
   * provider's to answer.
   *
   * ===What a conversion allocates===
   *
   * Exactly one full-length array, the one the result is made of. The rates are collected first,
   * one per currency, and the sum is then computed index by index into that single array - so a
   * run of a hundred thousand scenarios in five currencies writes a hundred thousand values once
   * rather than the six hundred thousand that scaling each currency's array and folding the
   * results onto a zeroed array costs, and nothing full-length is allocated only to be added into
   * something else and dropped. What is held between the two halves is one small structure per
   * currency of the run, holding that currency's rate beside the array the run already holds.
   *
   * The sum at an index can be a value no amount holds - opposed infinities in two currencies,
   * or a rate that is not a number, which an [[FxRate]] holds because it is neither negative nor
   * zero - and a run of single-currency amounts does not hold one either. The converted values
   * therefore go through the checked construction of [[CurrencyAmountArray]], so such a
   * conversion is reported in the failure channel this member already has rather than raised out
   * of it.
   *
   * @param resultCurrency  the currency of the result
   * @param rateProvider  the provider of FX rates
   * @return this run expressed in the result currency, the failure the provider reported for the
   *   first rate the conversion needed and could not get, or the failure describing a converted
   *   value that is not a number
   */
  override def convertedTo(
      resultCurrency: Currency,
      rateProvider: FxRateProvider): FailureOr[CurrencyAmountArray] =
    ratesOf(values.iterator, resultCurrency, rateProvider, Vector.empty)
      .flatMap(prepared =>
        CurrencyAmountArray.checkedOne(resultCurrency, converted(prepared)))

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
   * The result is built through the checking factory of this type, so the two structural
   * properties are established for the result rather than inherited from the operands.
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
   * The result is built through the checking factory of this type, and the failure channel this
   * reports is that factory's: the arrays assembled here all have the size of this run by
   * construction, so no input of this method fills the channel. Keeping it is what makes the
   * check a property of the factory rather than of a reading of this method, so a check added to
   * the factory needs no change here.
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
   * way in every respect but one: a currency only the other run holds is carried through negated
   * rather than as it stands, since nothing minus that currency's values is their negation. That
   * is the zero padding of this type again - this run holds zero for that currency at every
   * index.
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

  /**
   * Applies an operation to the values of every currency, keeping the currencies and the size.
   *
   * This is the shape of the two total arithmetic members above. The operation is applied per
   * currency, in the order of the currency codes, and it may not change the length of an array -
   * every operation that reaches it is element-wise - so the size and the array lengths of this
   * type carry over to the result. What does not carry over is the element invariant: the
   * operation produces numbers of its own, so the construction it goes through establishes that
   * invariant and raises it, which is what the two members above document.
   *
   * @param operation  the operation to apply to each currency's values
   * @return this run with the operation applied to the values of every currency
   * @throws java.lang.IllegalArgumentException if the operation produces a value that is not a
   *   number
   */
  private def mapValues(operation: DoubleArray => DoubleArray): MultiCurrencyAmountArray =
    MultiCurrencyAmountArray.create(
      size,
      SortedMap.from(values.iterator.map { case (currency, currencyValues) =>
        (currency, operation(currencyValues))
      })(MultiCurrencyAmountArray.currencyOrdering))

  /**
   * Collects the rate of each currency of this run, stopping at the first one that is refused.
   *
   * This is the first half of [[convertedTo]], and it is where every question is put to the
   * provider: each currency of the run is asked for exactly once, in the order of the currency
   * codes, and the rate is kept beside the array the run already holds for that currency so that
   * the second half needs no further lookup. Nothing full-length is built here - an entry is a
   * rate and a reference to an array this run holds - so the whole of what a conversion holds
   * between its two halves is one small entry per currency.
   *
   * A refused rate returns immediately and no currency after it is asked about, which is the
   * promise [[convertedTo]] makes: asking for rates that a decided outcome does not need would
   * make the number of lookups a failing conversion costs depend on how many currencies happened
   * to follow the one that failed. Writing it as a recursion over the entries rather than as a
   * traversal of them is what makes that unconditional, since a traversal decides for itself how
   * much of its input it reads.
   *
   * The recursion is in tail position and runs as a loop, so a run of any number of currencies is
   * prepared without consuming stack.
   *
   * @param remaining  the currencies of this run still to be asked about, in code order
   * @param resultCurrency  the currency every value is to be converted into
   * @param rateProvider  the provider of FX rates, asked once per currency
   * @param prepared  the rate and values of the currencies already asked about, in code order
   * @return the rate and values of every currency in code order, or the failure the provider
   *   reported for the first rate it could not supply
   */
  @tailrec
  private def ratesOf(
      remaining: Iterator[(Currency, DoubleArray)],
      resultCurrency: Currency,
      rateProvider: FxRateProvider,
      prepared: Vector[(Double, DoubleArray)]): FailureOr[Vector[(Double, DoubleArray)]] =
    if (!remaining.hasNext) {
      Right(prepared)
    } else {
      val (currency, currencyValues) = remaining.next()
      rateProvider.fxRate(currency, resultCurrency) match {
        case Right(rate) =>
          ratesOf(remaining, resultCurrency, rateProvider, prepared :+ ((rate, currencyValues)))
        case Left(failure) => Left(failure)
      }
    }

  /**
   * Computes the converted values of the whole run into one array.
   *
   * This is the second half of [[convertedTo]], and it allocates exactly one full-length array:
   * the values of the result are produced index by index, each as the sum over the currencies at
   * that index, so no currency's contribution is ever materialised as an array of its own. The
   * two small buffers are the rates and the arrays of the entries prepared by [[ratesOf]], laid
   * out by position so that the loop over the currencies of an index reads them primitively;
   * they are locals of this method, they hold no value of the result, and neither they nor any
   * backing array of a [[DoubleArray]] leaves it.
   *
   * @param prepared  the rate and values of every currency of this run, in code order
   * @return the values of this run converted and added together, one per index
   */
  private def converted(prepared: Vector[(Double, DoubleArray)]): DoubleArray = {
    val rates: Array[Double] = Array.tabulate(prepared.size)(position => prepared(position)._1)
    val arrays: Array[DoubleArray] =
      Array.tabulate(prepared.size)(position => prepared(position)._2)
    DoubleArray.tabulate(size)(index => convertedAt(rates, arrays, index, 0, 0d))
  }

  /**
   * Adds up the converted values of every currency at one index of the run.
   *
   * The accumulation starts at zero, it takes the currencies by position - which is the order of
   * their codes - and each term is that currency's value at the index multiplied by that
   * currency's rate, in that operand order. Floating point addition and multiplication both round,
   * so each of those three things is part of the number this produces and none of them is
   * incidental.
   *
   * The recursion is in tail position and runs as a loop over two primitive-indexed buffers, so
   * an index costs no allocation at all and a run of any number of currencies converts without
   * consuming stack.
   *
   * @param rates  the rate of each currency, by position in code order
   * @param arrays  the values of each currency, by position in code order
   * @param index  the index of the run being converted
   * @param position  the position of the currency whose term is added next
   * @param accumulated  the sum of the terms of the currencies before that position
   * @return the converted value of the run at that index
   */
  @tailrec
  private def convertedAt(
      rates: Array[Double],
      arrays: Array[DoubleArray],
      index: Int,
      position: Int,
      accumulated: Double): Double =
    if (position == rates.length) {
      accumulated
    } else {
      convertedAt(
        rates,
        arrays,
        index,
        position + 1,
        accumulated + arrays(position).get(index) * rates(position))
    }

  /**
   * Combines the values of this run with those of another run, currency by currency.
   *
   * The union of the two sets of currencies is taken: a currency both runs hold is combined with
   * the first operation, a currency only this run holds is carried through as it stands, and a
   * currency only the other run holds goes through the second operation. The two operations are
   * what distinguish addition from subtraction.
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
   * addition, with its negation for subtraction.
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
   * wording.
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

  /**
   * Checks whether this run equals another object.
   *
   * Another run is equal when it has the same size and holds the same currencies with values that
   * are equal element by element. The values are compared by [[DoubleArray]], whose equality is
   * bit for bit, so a value that is not a number equals itself and a negative zero differs from a
   * positive zero. The map comparison is by content, so it does not matter in what order either
   * run was built. An object of any other type is not equal.
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
   * The mixing is a seed, then each field in declaration order, with the map contributing the
   * hash of its entries, each of which is the hash of a currency code and the hash of a
   * [[DoubleArray]], computed from the bit patterns of its elements. Everything it reads is a
   * function of the value alone, so the hash of a run is identical in every execution of every
   * program, and it agrees with the equality above.
   *
   * @return the hash code of the size and the values held
   */
  override def hashCode: Int =
    (MultiCurrencyAmountArray.HashSeed * 31 + size) * 31 + values.hashCode

  /**
   * Returns this run as text.
   *
   * The form is the two fields named in declaration order between braces, the values written as a
   * braced list of one entry per currency:
   *
   * {{{
   * MultiCurrencyAmountArray{size=2, values={GBP=[1.0, 0.0], USD=[0.0, 2.0]}}
   * }}}
   *
   * It is what the `Show` instance of the companion renders. Every value of every array appears,
   * so the text of a long run is long - it describes the whole value rather than summarising it.
   *
   * @return the rendering of this run
   */
  override def toString: String =
    s"MultiCurrencyAmountArray{size=$size, values=$renderedValues}"

  /**
   * Renders the values per currency as a braced list of entries.
   *
   * Each entry is its currency, an equals sign and that currency's values, the entries separated
   * by a comma and a space and taken in currency order. That is not how a map of this language
   * renders itself, hence rendering it here rather than interpolating the map.
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
 * This companion is the only place a [[MultiCurrencyAmountArray]] is created. The constructor is
 * private, no `apply` or `copy` is published and the type is sealed, so a run whose arrays
 * disagree about their length, or whose size does not match them, cannot exist: the factory that
 * reads such input reports it instead.
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
 * that has something to check - that the arrays agree about their length, and that every value
 * in them is a value an amount holds:
 *
 *   - `of(values)` reports what it rejects as a chain of [[Failure]].
 *
 * [[total]] is the aggregating factory: it combines runs of single-currency amounts into one run,
 * adding the arrays of a currency that appears more than once. It inherits the failure channel of
 * the fourth, since arrays of different lengths cannot be combined or held together and since
 * adding two arrays can produce a value no amount holds.
 *
 * ===Instances===
 *
 * Two typeclass instances are published, and exactly two: a `Hash`, which is the single
 * equality-bearing instance of the type - `Hash` extends `Eq`, so declaring an `Eq` as well would
 * leave two instances that could disagree and one of them ambiguous - and a `Show`. There is
 * deliberately no `Order`, a run of multi-currency amounts having no ordering worth inventing.
 *
 * @see [[MultiCurrencyAmountArray]] for the type itself, the two structural properties every run
 *   has, and the zero padding the three total factories apply
 */
object MultiCurrencyAmountArray {

  /**
   * The seed of the hash code, which makes hashing reproducible from one execution to the next.
   * Any constant would do; the hash of the type name is taken because it is stable, is specified
   * by the platform, and distinguishes this type's seed from that of any other type deriving one
   * from its own name.
   */
  private val HashSeed: Int = "MultiCurrencyAmountArray".hashCode

  /**
   * The name of the size, as the argument name of the checks that read it, so a size this type
   * does not admit is reported against the name `size`.
   */
  private val SizeField: String = "size"

  /**
   * The name of the values, as the argument name of the check that rejects an element.
   *
   * This is the name of the field itself, so the message reported for a value the type does not
   * admit names the thing a caller passed - `Argument 'values' for GBP must not be NaN at
   * index 1`.
   */
  private val ValuesField: String = "values"

  /**
   * The ordering the values of every run are held in.
   *
   * It is the cats `Order` of [[Currency]] turned into an `Ordering`, so the order of the
   * currencies of a run, of the entries of its JSON object, of its rendering and of the rates
   * asked for by a conversion all come from the one source of truth for comparing currencies -
   * their codes.
   */
  private val currencyOrdering: Ordering[Currency] = Order[Currency].toOrdering

  /**
   * The empty grouping of runs by currency, which is where [[total]] starts.
   *
   * It is held once rather than built per call, and its ordering is the ordering above so that
   * the aggregation is performed and reported in currency order.
   */
  private val noGroups: SortedMap[Currency, Vector[CurrencyAmountArray]] =
    SortedMap.empty[Currency, Vector[CurrencyAmountArray]](currencyOrdering)

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
   * described on this type:
   *
   * {{{
   * MultiCurrencyAmountArray.of(List(oneGbp, twoUsd))
   *   // size 2, GBP [1.0, 0.0], USD [0.0, 2.0]
   * }}}
   *
   * The collection is read once, into an indexed sequence, so a lazy or single-use collection is
   * traversed exactly once even though the values of each currency are then built per currency.
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
   * therefore materialised once, in order, and the arrays are built from them in a single pass
   * over that sequence.
   *
   * A negative size is refused as a caller contract rather than reported as a failure: there is
   * no data a caller could hold that makes a run of minus one amounts meaningful, so this is an
   * invariant of the call and not a property of its input. Refusing it is what lets the two
   * structural properties of this type hold without exception - the decoder refuses a negative
   * size for the same reason.
   *
   * A size of zero is admitted and describes the run of size zero.
   *
   * @param size  the number of amounts, zero or greater
   * @param valueFunction  the function that produces the amount at each index
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
   * An empty map describes the run of size zero; a run of non-zero size holding no currency is
   * built with one of the other factories.
   *
   * Every array that disagrees is reported, not only the first, so a caller reads one chain
   * naming each of them. Each reason names the size settled on and the size found. Which array is
   * the reference is settled by the order of the currency codes rather than by the iteration
   * order of the map handed in, so the same map always produces the same reasons in the same
   * order whatever collection it arrives in.
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
   * presents them. Floating point addition is order-sensitive, so stating that order is what
   * makes the total reproducible.
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
   * value of this type if they differ. The two are reported differently, and which of them a
   * mixture of lengths meets first is decided by the currencies involved rather than by the
   * order of the input:
   *
   *   - two runs of the '''same''' currency whose lengths differ are the first reason, reported
   *     in the wording [[CurrencyAmountArray]] reports for that disagreement - which names the
   *     length of the first run of that currency and the length of the one that arrived - as the
   *     single reason the whole call fails with, ahead of any comparison across currencies;
   *   - runs of '''different''' currencies whose lengths differ are the second, reported by the
   *     checking factory above, one reason per run that disagrees with the length of the first
   *     run in currency order, in currency order.
   *
   * A third reason comes from the addition itself rather than from the lengths: two runs of one
   * currency holding opposite infinities at an index sum to a value no amount holds, which the
   * checking factory reports naming that currency and index. It is reported alongside the second
   * class of reason, since both are found by the same factory over the same assembled map.
   *
   * An empty input describes the run of size zero.
   *
   * ===How the total is computed===
   *
   * The input is read once, and it is read only as far as the outcome needs. The runs of each
   * currency are collected by reference as they arrive, in input order, and the length of each is
   * compared with the length of the first run of its currency as it is collected, so a
   * disagreement returns at once and nothing after it is read - which is what keeps a very large
   * or lazily generated input from being consumed in full after its first reason has already
   * decided the answer.
   *
   * Each currency's values are then added up once, element by element and left to right in input
   * order, into the one array that currency contributes to the result. That is where the order of
   * the addition is settled - floating point addition is order-sensitive, so stating it is what
   * makes the total reproducible - and it is why a currency offered M runs costs one array and
   * not M-1 transient ones: adding the runs pairwise as they arrive would allocate a complete
   * array for every arrival after the first, each one read once and dropped. A currency offered a
   * single run contributes that run's values as they stand, with nothing computed at all.
   *
   * The result is assembled in currency order as it is computed, so the map handed to the
   * checking factory is already the map of the run: it is passed there directly rather than being
   * iterated into an unordered map for that factory to sort again.
   *
   * @param arrays  the runs to total, in any number and any order
   * @return the total of those runs, or the failures describing the lengths that disagree
   */
  def total(arrays: Iterable[CurrencyAmountArray]): ResultNec[MultiCurrencyAmountArray] =
    toNec(grouped(arrays.iterator, noGroups)).flatMap { groups =>
      val totals = SortedMap.from(groups.iterator.map { case (currency, runs) =>
        (currency, summedValues(runs))
      })(currencyOrdering)
      // the size of the first array in currency order, and zero when there is no array at all,
      // which is the size the factory that reads a map of values settles on for the same input
      checked(totals.headOption.fold(0) { case (_, currencyValues) => currencyValues.size }, totals)
    }

  /**
   * Collects the runs offered to [[total]] by currency, stopping at the first length that differs.
   *
   * Nothing is added up here: a run is kept as the reference it arrived as, appended to the runs
   * its currency has already been offered, so a pass over the input costs one small entry per
   * currency and one per run rather than a full-length array per arrival. The runs of a currency
   * are kept in the order the input presented them, which is the order they are added up in, and
   * the currencies are kept in code order, which is the order the reasons of a rejection and the
   * currencies of the result appear in.
   *
   * The length of an arriving run is compared with the length of the first run of its currency,
   * and a disagreement returns immediately with the reason [[CurrencyAmountArray.plus]] reports
   * for it, in its wording: the comparison is the one that member performs, made here so that it
   * can be made without adding anything up. Returning at once is what the promise of [[total]]
   * rests on - the outcome is already decided, so no further element of the input is pulled.
   *
   * The recursion is in tail position and runs as a loop, so an input of any length is collected
   * without consuming stack.
   *
   * @param remaining  the runs still to be collected, pulled one at a time
   * @param accumulated  the runs already collected, by currency in code order and in input order
   * @return the runs of every currency, or the failure describing the first length that differs
   *   from the length of the first run of its currency
   */
  @tailrec
  private def grouped(
      remaining: Iterator[CurrencyAmountArray],
      accumulated: SortedMap[Currency, Vector[CurrencyAmountArray]])
      : FailureOr[SortedMap[Currency, Vector[CurrencyAmountArray]]] =
    if (!remaining.hasNext) {
      Right(accumulated)
    } else {
      val next = remaining.next()
      accumulated.get(next.currency) match {
        case Some(runs) if runs.head.size != next.size =>
          // the size check of `CurrencyAmountArray.plus`, performed without the addition: the
          // first run of the currency is the one the sum would have accumulated into, so it is
          // its size that is named first, exactly as that member names it
          Left(differentSizes(runs.head.size, next.size))
        case Some(runs) => grouped(remaining, accumulated.updated(next.currency, runs :+ next))
        case None => grouped(remaining, accumulated.updated(next.currency, Vector(next)))
      }
    }

  /**
   * Adds up the runs of one currency into the one array that currency contributes.
   *
   * A single run contributes the values it already holds, with nothing computed and nothing
   * allocated. Several runs are added element by element in one pass per index, so the currency
   * costs exactly one array however many runs it was offered.
   *
   * The addition starts at the first run's value rather than at zero and takes the runs in input
   * order. Both are part of the number produced, since floating point addition rounds and adding
   * into a zero would normalise a negative zero away.
   *
   * @param runs  the runs of one currency, in input order, all of the same length
   * @return the values of that currency in the total
   */
  private def summedValues(runs: Vector[CurrencyAmountArray]): DoubleArray =
    if (runs.size == 1) {
      runs.head.values
    } else {
      val values: Array[DoubleArray] =
        Array.tabulate(runs.size)(position => runs(position).values)
      DoubleArray.tabulate(values(0).size)(index =>
        summedAt(values, index, 1, values(0).get(index)))
    }

  /**
   * Adds up the values of several runs of one currency at one index.
   *
   * The runs are taken by position, which is the order the input presented them, and each value
   * is added to what has accumulated - `accumulated + arriving`, in that operand order, which is
   * part of the number produced because floating point addition rounds.
   *
   * The recursion is in tail position and runs as a loop over a primitive-indexed buffer, so an
   * index costs no allocation at all.
   *
   * @param values  the values of each run of the currency, by position in input order
   * @param index  the index being added up
   * @param position  the position of the run whose value is added next
   * @param accumulated  the sum of the values of the runs before that position
   * @return the total of the runs at that index
   */
  @tailrec
  private def summedAt(
      values: Array[DoubleArray],
      index: Int,
      position: Int,
      accumulated: Double): Double =
    if (position == values.length) {
      accumulated
    } else {
      summedAt(values, index, position + 1, accumulated + values(position).get(index))
    }

  /**
   * Builds a run from amounts that have already been read into an indexed sequence.
   *
   * This is where the three total factories meet, and it is where the size of the run is settled:
   * one index per amount. It has nothing of its own to check: [[arraysOf]] produces one array of
   * exactly that length per currency, and every number in those arrays came out of a
   * [[MultiCurrencyAmount]] - which holds amounts - or is the padded zero, so the element
   * invariant of the construction point cannot refuse one of them.
   *
   * @param amounts  the amounts, one per index, in index order
   * @return the run holding those amounts
   */
  private def fromAmounts(amounts: Vector[MultiCurrencyAmount]): MultiCurrencyAmountArray =
    create(amounts.size, arraysOf(amounts))

  /**
   * Turns amounts into one full-length array of values per currency.
   *
   * This is the zero padding of the three total factories: the currencies of all the amounts are
   * collected first, and then each currency's array is produced from the amounts by index, an
   * amount that does not name the currency contributing `0.0` at its index.
   *
   * The number of each cell is read straight out of the map the amount holds, which
   * [[MultiCurrencyAmount.toMap]] hands back without copying, and which holds numbers that are
   * already normalised amounts. That is what keeps the transposition to the arrays it produces
   * free of intermediate objects: asking the amount for a [[CurrencyAmount]] per cell would
   * allocate one domain object for every currency and every index, C×N of them for C currencies
   * and N amounts, each built only to have its number read back out and then discarded.
   *
   * Each array is built in one pass over the amounts with no buffer being handed out, which is
   * what the copy-safe construction of [[DoubleArray]] requires - it copies whatever it is given
   * and hands back nothing that aliases its own storage.
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
          // the padded read, performed on the map itself: an amount that does not name the
          // currency contributes zero, and nothing is allocated to find that out
          amounts(index).toMap.getOrElse(currency, 0d)))))(currencyOrdering)
  }

  /**
   * Builds a run after establishing all three of its properties, reporting what it rejects.
   *
   * This is the single checking route into the type. It is what the `of` that reads a map of
   * values per currency performs, what [[total]] finishes with, what the four arithmetic members
   * that combine values route their results through, and what the decoder hands a payload to,
   * which is why a document may neither declare a size its arrays do not have nor carry an element
   * this type does not hold.
   *
   * All three classes of reason are accumulated rather than sequenced, so a caller reads every
   * reason its input gives: the size being negative, each array whose length is not that size,
   * and each currency holding a value that is not a number. The arrays are examined in currency
   * order in both of the per-currency passes, so the reasons are ordered the same way for the
   * same input, and a currency holding several such values is reported once - at the first of
   * them, which is the one a caller looks at.
   *
   * The element examination is performed here rather than being left to the raise of [[create]]
   * because every route through this one has a failure channel: a caller reading a map of values
   * from a document, a file or a wire did not write those numbers, so a run it cannot have is an
   * answer to be read and not an exception thrown past it. [[create]] still performs the
   * invariant - it answers to the routes that have no channel - and it is reached from here only
   * when nothing was found, `toLeft` taking its argument by name.
   *
   * @param size  the size the run is to have, zero or greater
   * @param values  the values per currency, ordered by currency code
   * @return the run, or the failures describing the size, the arrays that disagree with it and
   *   the values that are not numbers
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
    val notANumbers = Collections.toNonEmptyChain(notANumberIndices(values).map {
      case (currency, index) => notANumber(currency, index)
    })
    Collections
      .concatNonEmptyChains(List(negativeSize, differingLengths, notANumbers).flatten)
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
   * @return the run, or the single failure naming the broken constraint: the size must be zero or
   *   greater, and every array must hold exactly that many values
   */
  private def checkedOne(
      size: Int,
      values: SortedMap[Currency, DoubleArray]): FailureOr[MultiCurrencyAmountArray] =
    checked(size, values).left.map(Failure.collapse)

  /**
   * Builds a run, establishing the element invariant, which is the only call of the constructor.
   *
   * Every route into the type ends here, and each of them has already established the two
   * structural properties: the total factories build one array of the run's length per currency,
   * the element-wise arithmetic preserves the lengths of the arrays it is given, and [[checked]]
   * has just examined them. The third property, that every element is a value
   * [[CurrencyAmount]] holds, is a property of the numbers rather than of the route, and it is
   * established here so that it holds for every route at once - including the two arithmetic
   * members that are total in signature and produce numbers of their own, which have no channel
   * to report it in. Keeping both the construction and that invariant in one place is what makes
   * the type's promise reviewable at a single site.
   *
   * The check is a fail-fast [[ArgCheck]], and the message - which names the currency and the
   * index, since a run says nothing about which of its numbers is wrong - is taken by name, so
   * the happy path allocates nothing for it. `hasNext` inspects the scan without consuming its
   * first entry, so the message can still take that entry from the same iterator. A route with a
   * failure channel of its own does not reach the raise: [[checked]] performs the same
   * examination first and reports it.
   *
   * @param size  the size of the run
   * @param values  the values per currency, each holding exactly `size` elements
   * @return the run
   * @throws java.lang.IllegalArgumentException if a value of any currency is not a number
   */
  private def create(
      size: Int,
      values: SortedMap[Currency, DoubleArray]): MultiCurrencyAmountArray = {
    val offending: Iterator[(Currency, Int)] = notANumberIndices(values)
    ArgCheck.isTrue(
      !offending.hasNext, {
        val (currency, index) = offending.next()
        notANumberMessage(currency, index)
      })
    new Impl(size, values)
  }

  /**
   * Finds, per currency, the index of the first value that is not a number.
   *
   * The currencies are examined in the order the run holds them, which is the order of their
   * codes, and a currency whose values are all numbers contributes nothing - so the result is
   * empty for the values of every run this type has, and holds one entry per offending currency
   * otherwise. It is an iterator rather than a collection because both of its callers need only
   * as much of it as their answer depends on: the raise of [[create]] needs the first entry and
   * the accumulation of [[checked]] needs all of them.
   *
   * The scan of each array is `DoubleArray.indexOf`, which compares bit patterns: that finds a
   * not-a-number value however it arose, where an ordinary comparison finds none, and it is one
   * pass over the primitive array with nothing boxed.
   *
   * @param values  the values per currency, ordered by currency code
   * @return the currency and index of the first offending value of each offending currency, in
   *   currency order
   */
  private def notANumberIndices(
      values: SortedMap[Currency, DoubleArray]): Iterator[(Currency, Int)] =
    values.iterator
      .map { case (currency, currencyValues) =>
        (currency, currencyValues.indexOf(Double.NaN))
      }
      .filter { case (_, index) => index >= 0 }

  /**
   * The one implementation of a run of multi-currency amounts.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it. It is declared rather than written as an anonymous subclass at the instantiation site
   * for two reasons, both about what the class file says: a private member class is one a Java
   * compiler refuses to name, where an anonymous class is public and can be instantiated directly
   * by a caller in another language, and a named class can be compared against, which is what
   * lets [[MultiCurrencyAmountArray]] refuse in its own constructor to be any other
   * implementation.
   *
   * @param size  the size of the run
   * @param values  the values per currency, each holding exactly `size` elements, as every route
   *   into [[create]] has already established
   */
  private final class Impl(size: Int, values: SortedMap[Currency, DoubleArray])
      extends MultiCurrencyAmountArray(size, values)

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

  /**
   * The failure reported when a currency's values are read from a run that does not hold it.
   *
   * @param currency  the currency the run does not hold
   * @return the failure naming that currency
   */
  private def noValues(currency: Currency): Failure =
    Failure.Invalid(s"No values available for $currency")

  /**
   * The failure reported when two runs that have to be combined have different sizes.
   *
   * It is the wording [[CurrencyAmountArray]] reports for the same disagreement, so the two types
   * describe a mismatched pair of runs identically.
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
   * It names the length the run is to have and the length of the array that disagrees with it.
   *
   * @param expected  the length the run is to have
   * @param found  the length of the array that disagrees with it
   * @return the failure describing the disagreement
   */
  private def differentArraySizes(expected: Int, found: Int): Failure =
    Failure.Invalid(s"Arrays must have the same size but found sizes $expected and $found")

  /**
   * The wording of the element invariant, which is defined here and nowhere else.
   *
   * Both channels that report a refused element read it from here - the raise of [[create]] and
   * the failures of [[checked]] - so the two cannot drift apart. It carries the whole location of
   * the value: the currency, rendered as its code, because a run holds one array per currency and
   * the arrays are otherwise indistinguishable to a reader of the message, and the index within
   * that currency's array, because a run of a hundred thousand values says nothing about which of
   * them is wrong.
   *
   * @param currency  the currency whose values hold the offending value
   * @param index  the index of that value within the currency's array
   * @return the message naming the argument, the currency and that index
   */
  private def notANumberMessage(currency: Currency, index: Int): String =
    s"Argument '$ValuesField' for $currency must not be NaN at index $index"

  /**
   * The failure reported for a value that is not a number.
   *
   * The reason is the invalid-argument reason of this port, as it is for the two structural
   * rejections of this type, and the message is the one wording above.
   *
   * @param currency  the currency whose values hold the offending value
   * @param index  the index of that value within the currency's array
   * @return the failure describing that value
   */
  private def notANumber(currency: Currency, index: Int): Failure =
    Failure.Invalid(notANumberMessage(currency, index))

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
   * Renders what `toString` renders.
   *
   * @return the rendering of a run
   */
  implicit val show: Show[MultiCurrencyAmountArray] = Show.show(_.toString)

  // The codec of the arrays, brought into scope for the two derivations below and for nothing
  // else. It has to be taken from here because the JSON library has no instance for
  // [[DoubleArray]] and its instance for a double cannot express a value that is not a number;
  // importing at this point is what keeps that choice local, as the codec support of
  // `strata-collect` intends.
  import Codecs.implicits._

  /**
   * The raw field shape both codecs of this type are derived over.
   *
   * A type whose constructor is private has no shape a codec can be derived over, so this product
   * states its two fields and makes the derivation possible in both directions: the decoder below
   * derives over it and then builds through the checking factory, and the encoder below derives
   * over it and is contramapped from a run that already exists. It exists only for that purpose -
   * it is private and it is never returned, so no caller can hold an unchecked pair. Its field
   * names are the JSON keys, and they are the names of the two fields of
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
      extends NoJavaSerialization

  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /**
   * The derived encoder of the raw field shape.
   *
   * It sits after the import above deliberately: the derivation reads the array codec of this
   * library from that import, and that codec is what writes an infinite element as its tagged
   * string.
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
   * bytes without anything being sorted here.
   *
   * The shape is derived at compile time rather than written out field by field, and no part of
   * it inspects a class while the program runs. Every element of every array goes through the one
   * policy this library has for a double, so a value that is not a number and the two infinities
   * appear as the strings `"NaN"`, `"Infinity"` and `"-Infinity"` while a finite value is written
   * as a JSON number, exactly to the bit - which is what lets every run this type admits survive
   * a round trip. The result is wrapped so that a field holding no value would be omitted, the
   * policy every product of this library follows; this type has no optional field, so the
   * wrapping changes nothing about its output and exists so that the policy holds without
   * exception.
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
   * takes: a document declaring a size its arrays do not have, a negative size, or an element
   * that decodes to a value this type does not hold - the tagged string `"NaN"`, which the policy
   * of this port for a double reads as a not-a-number value - is a decoding failure carrying
   * those reasons rather than a run this type would not have built. The last of the three is why
   * a payload carrying `"NaN"` is refused here rather than read into a run whose every later
   * reader fails on it. What a document can get wrong inside a field is refused by the field
   * codecs, which report a currency code this port does not hold and an element that is neither a
   * number nor one of the three tagged strings.
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
