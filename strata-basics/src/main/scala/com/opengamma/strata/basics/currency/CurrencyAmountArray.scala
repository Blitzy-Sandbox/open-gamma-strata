/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import cats.Hash
import cats.Show
import cats.syntax.apply._

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.collect.Collections
import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * An array of currency amounts with the same currency.
 *
 * This represents a run of [[CurrencyAmount]] values that are all in one currency - the amounts
 * of a measure across the scenarios of a calculation, for instance. The currency is held once
 * and the numbers are held together in a single [[DoubleArray]], rather than as one amount per
 * element, so that a run of a hundred thousand scenarios is one currency and one primitive
 * array instead of a hundred thousand small objects, and so that arithmetic over the whole run
 * is a single arithmetic operation on that array.
 *
 * ===Construction===
 *
 * The three ways in are the three the type being ported offered, and they differ in what they
 * have to check:
 *
 *   - `of(currency, values)` is total. A currency and an array of numbers always describe a run
 *     of amounts, so there is nothing for a factory to reject - including a value that is not a
 *     number, which this type admits inside its array exactly as the type being ported admitted
 *     it.
 *   - `of(amounts)` reads a collection of amounts, which has to hold at least one amount and
 *     has to name a single currency, so it reports what it rejects.
 *   - `of(size, valueFunction)` builds the amounts from a function of the index, which likewise
 *     has to produce at least one amount and has to produce them all in one currency.
 *
 * The two checking factories report their reasons as a chain of [[Failure]] rather than by
 * abandoning the call, which is the convention of this port for a rejection that depends on the
 * data a caller holds. Neither a public `apply` nor a `copy` exists - the constructor is
 * private and the type is sealed - so the only route to a value is one of those three, and an
 * array whose amounts disagree about their currency cannot be built at all.
 *
 * ===Which operations can fail===
 *
 * Adding or subtracting another array fails when the two arrays have different sizes or
 * different currencies, and adding or subtracting a single amount fails when its currency is
 * not the currency of the array: in each case the result would have no meaning, and whether it
 * happens is a property of the values involved. Converting fails when the rate the conversion
 * needs is unavailable. Everything else here - reading an element, scaling every element,
 * mapping every element - is total.
 *
 * ===Equality===
 *
 * Two arrays are equal when their currencies are equal and their values are equal element by
 * element, compared by bit pattern as [[DoubleArray]] compares them. That is the comparison the
 * generated bean of the type being ported performed, and it differs from a numeric comparison
 * in two deliberate places the round-trip properties of the test suite rely on: an element that
 * is not a number equals itself, so an array always equals itself, and a negative zero differs
 * from a positive zero.
 *
 * There is deliberately no ordering. The type being ported is not comparable, and a run of
 * amounts has no ordering worth inventing.
 *
 * ===Thread safety===
 *
 * An instance is immutable, and so is the array of values it holds, so it is safe to share
 * between any number of threads without synchronisation. Every operation returns a new array
 * rather than changing the one it was called on.
 *
 * @param currency  the currency, which every amount of the array is in
 * @param values  the values of the amounts, in order
 * @see [[MultiCurrencyAmountArray]] for a run of amounts in several currencies at once
 * @see [[CurrencyAmount]] for a single amount
 */
sealed abstract case class CurrencyAmountArray private (currency: Currency, values: DoubleArray)
    extends FxConvertible[CurrencyAmountArray] {

  //-------------------------------------------------------------------------
  /**
   * Gets the size of the array.
   *
   * @return the number of amounts the array holds
   */
  def size: Int = values.size

  /**
   * Gets the amount at the specified index.
   *
   * The amount pairs the currency of the array with the value at that position, counting from
   * zero, so reading an element of a run of amounts costs one small object and no copy of the
   * values:
   *
   * {{{
   * CurrencyAmountArray.of(Currency.GBP, DoubleArray.of(1d, 2d)).get(1)   // GBP 2
   * }}}
   *
   * This is total in signature, as it was in the type being ported, and it raises that type's
   * two documented invariants rather than widening into a failure channel: an index outside the
   * array is the index exception of the runtime, exactly as reading the [[DoubleArray]] directly
   * would be, and an element that is not a number is an amount [[CurrencyAmount]] does not
   * admit. The second is reachable only from an array built with such an element by the total
   * factory, and it is the behaviour of the type being ported, whose own `get` raised it too.
   *
   * @param index  the zero-based index to retrieve
   * @return the amount at that index
   * @throws java.lang.IndexOutOfBoundsException if the index is outside the array
   * @throws java.lang.IllegalArgumentException if the value at that index is not a number
   */
  def get(index: Int): CurrencyAmount = amountOf(values.get(index))

  /**
   * Returns the amounts of this array, one at a time.
   *
   * This is the member the stream of the type being ported becomes. A stream is a lazy sequence
   * that is traversed once, which is what an `Iterator` is in this language, so a call site that
   * streamed the amounts in order to filter, map or fold them reads the same way here:
   *
   * {{{
   * array.iterator.filter(_.isPositive).toList
   * }}}
   *
   * The amounts are produced in index order and each is built only as it is read, so a caller
   * that stops early never builds the rest. [[toList]] is the eager form, for a caller that
   * wants every amount at once.
   *
   * @return the amounts of this array in index order, built as they are read
   * @throws java.lang.IllegalArgumentException if a value read from the array is not a number,
   *   raised as the amount is built rather than when the iterator is obtained
   */
  def iterator: Iterator[CurrencyAmount] = values.iterator.map(amountOf)

  /**
   * Returns the amounts of this array as a list.
   *
   * This is [[iterator]] traversed to its end, for a caller that wants the amounts as a
   * collection rather than as a traversal. The list holds one amount per value of the array and
   * is built once, so it costs an object per element - which is exactly the representation this
   * type exists to avoid, and is why it is obtained explicitly rather than being how the type
   * holds its data.
   *
   * @return the amounts of this array in index order
   * @throws java.lang.IllegalArgumentException if a value of the array is not a number
   */
  def toList: List[CurrencyAmount] = iterator.toList

  /**
   * Returns an array with every amount multiplied by the specified factor.
   *
   * The multiplication is delegated to the values themselves, so it is one operation over the
   * primitive array and the products agree bit for bit with the values multiplied one at a time.
   * A factor of one answers with this instance, which is what the underlying array does.
   *
   * The currency is unchanged: scaling a run of amounts by a plain number does not convert it,
   * and [[convertedTo]] is the member that does.
   *
   * @param factor  the multiplicative factor
   * @return a copy of this array with every amount multiplied by the factor
   */
  def multipliedBy(factor: Double): CurrencyAmountArray =
    CurrencyAmountArray.create(currency, values.multipliedBy(factor))

  /**
   * Returns an array with the specified operation applied to every amount.
   *
   * This is the general form of the arithmetic of this type, for an operation it does not offer
   * as a member of its own:
   *
   * {{{
   * array.mapAmounts(value => if (value < 0d) 0d else value * 3d)
   * }}}
   *
   * The operation is applied to each value in index order and the currency is carried through
   * unchanged, since the operation is on the numbers alone. It is an ordinary function rather
   * than the primitive-specialised interface of the library being ported, which is the same
   * thing expressed in this language.
   *
   * @param mapper  the operation to apply to each amount
   * @return a copy of this array with the operation applied to every amount
   */
  def mapAmounts(mapper: Double => Double): CurrencyAmountArray =
    CurrencyAmountArray.create(currency, values.map(mapper))

  //-------------------------------------------------------------------------
  /**
   * Converts this array into the specified currency, taking the rate from the specified provider.
   *
   * This is the [[FxConvertible]] implementation of this type. An array already in the requested
   * currency is returned unchanged and the provider is not consulted, so such a conversion
   * succeeds even under a provider that supplies no rates at all - the behaviour of the type
   * being ported, and the reason a single-currency run needs no rate for a conversion into its
   * own currency.
   *
   * Otherwise the rate is looked up exactly once, for the pair of the two currencies, and every
   * value is multiplied by that one rate. Looking it up once rather than once per element is
   * what the type being ported did: it is faster, and it means the whole run is converted at a
   * single rate even under a provider whose answers could vary between calls.
   *
   * @param resultCurrency  the currency of the result
   * @param rateProvider  the provider of FX rates
   * @return this array expressed in the result currency, or the failure the provider reported
   *   for the rate the conversion needed
   */
  override def convertedTo(
      resultCurrency: Currency,
      rateProvider: FxRateProvider): FailureOr[CurrencyAmountArray] =
    if (currency == resultCurrency) {
      Right(this)
    } else {
      rateProvider
        .fxRate(currency, resultCurrency)
        .map(rate => CurrencyAmountArray.create(resultCurrency, values.multipliedBy(rate)))
    }

  //-------------------------------------------------------------------------
  /**
   * Returns an array holding the values of this array added to the values of the other array.
   *
   * The amounts are added element by element, so the amount at each index of the result is the
   * sum of the amounts at that index of the two arrays. The addition is delegated to the values
   * themselves, one operation over the two primitive arrays, so the sums agree bit for bit with
   * the amounts added one at a time.
   *
   * The two arrays have to have the same size, since there is no amount to add to an index only
   * one of them holds, and they have to be in the same currency, since the sum of amounts of
   * different currencies has no currency. Both are properties of the arrays a caller holds, so
   * both are reported:
   *
   * {{{
   * twoGbp.plus(twoGbp)   // Right(the element-wise sum)
   * twoGbp.plus(threeGbp) // Left(Failure.Invalid("Sizes must be equal, this size is 2, other size is 3"))
   * twoGbp.plus(twoUsd)   // Left(Failure.Invalid("Currencies must be equal, …"))
   * }}}
   *
   * The size is checked before the currency, which is the order of the type being ported, so an
   * array that differs in both is reported as differing in size.
   *
   * @param other  the other array of amounts
   * @return this array with the other added element by element, or the failure describing the
   *   size or currency mismatch
   */
  def plus(other: CurrencyAmountArray): FailureOr[CurrencyAmountArray] =
    matching(other).map(_ => CurrencyAmountArray.create(currency, values.plus(other.values)))

  /**
   * Returns an array holding the values of this array with the specified amount added.
   *
   * The amount is added to every element, so this shifts the whole run by one amount rather
   * than combining two runs. The currency of the amount has to be the currency of the array,
   * for the reason the element-wise `plus` above gives, and there is no size to disagree about.
   *
   * Adding a zero amount answers with this instance, which is what the underlying array does.
   *
   * @param amount  the amount to add to every element
   * @return this array with the amount added to every element, or the failure describing the
   *   currency mismatch
   */
  def plus(amount: CurrencyAmount): FailureOr[CurrencyAmountArray] =
    sameCurrency(amount.currency)
      .map(_ => CurrencyAmountArray.create(currency, values.plus(amount.amount)))

  /**
   * Returns an array holding the values of this array with the values of the other subtracted.
   *
   * This is the element-wise `plus` above read in the other direction and behaves the same way
   * in every respect: the amounts are
   * subtracted element by element, the sizes have to be equal, the currencies have to be equal,
   * and the size is checked first.
   *
   * @param other  the other array of amounts
   * @return this array with the other subtracted element by element, or the failure describing
   *   the size or currency mismatch
   */
  def minus(other: CurrencyAmountArray): FailureOr[CurrencyAmountArray] =
    matching(other).map(_ => CurrencyAmountArray.create(currency, values.minus(other.values)))

  /**
   * Returns an array holding the values of this array with the specified amount subtracted.
   *
   * The amount is subtracted from every element, and its currency has to be the currency of the
   * array. Subtracting a zero amount answers with this instance.
   *
   * @param amount  the amount to subtract from every element
   * @return this array with the amount subtracted from every element, or the failure describing
   *   the currency mismatch
   */
  def minus(amount: CurrencyAmount): FailureOr[CurrencyAmountArray] =
    sameCurrency(amount.currency)
      .map(_ => CurrencyAmountArray.create(currency, values.minus(amount.amount)))

  //-------------------------------------------------------------------------
  /**
   * Pairs a value of this array with the currency of this array.
   *
   * This is the route every member producing amounts takes, and it is deliberately the total
   * one: the zero amount of the currency has its number replaced by the value, which is exactly
   * the construction the `get` of the type being ported performed. The checking factory of
   * [[CurrencyAmount]] is not used, because it would widen reading an element into a failure
   * channel that the type being ported did not have and that the inventory of this port does
   * not list. A value that is not a number consequently raises the documented invariant of
   * [[CurrencyAmount]], as it did there.
   *
   * @param value  the value to pair with the currency of this array
   * @return the amount holding that value in the currency of this array
   * @throws java.lang.IllegalArgumentException if the value is not a number
   */
  private def amountOf(value: Double): CurrencyAmount =
    CurrencyAmount.zero(currency).mapAmount(_ => value)

  /**
   * Checks that the other array can be combined with this one element by element.
   *
   * The size is examined first and the currency second, which is the order of the type being
   * ported: an array that differs in both is reported as differing in size, and the currency
   * check is not reached. Both members that combine two arrays share this, so the two report
   * the same reasons in the same order.
   *
   * @param other  the other array
   * @return nothing when the two arrays agree in size and currency, otherwise the failure
   *   describing the first disagreement
   */
  private def matching(other: CurrencyAmountArray): FailureOr[Unit] =
    if (other.size != size) {
      Left(CurrencyAmountArray.differentSizes(size, other.size))
    } else {
      sameCurrency(other.currency)
    }

  /**
   * Checks that the specified currency is the currency of this array.
   *
   * @param otherCurrency  the currency to compare with the currency of this array
   * @return nothing when the currencies are equal, otherwise the failure describing the mismatch
   */
  private def sameCurrency(otherCurrency: Currency): FailureOr[Unit] =
    if (otherCurrency == currency) {
      Right(())
    } else {
      Left(CurrencyAmountArray.differentCurrencies(currency, otherCurrency))
    }

  //-------------------------------------------------------------------------
  /**
   * Checks whether this array equals another object.
   *
   * Another array is equal when it holds the same currency and values that are equal element by
   * element. The values are compared by [[DoubleArray]], whose equality is bit for bit, so an
   * element that is not a number equals itself and a negative zero differs from a positive
   * zero - the comparison the generated bean of the type being ported performed. An object of
   * any other type is not equal.
   *
   * @param obj  the object to compare to
   * @return true if the other object is an array holding the same currency and the same values
   */
  override def equals(obj: Any): Boolean = obj match {
    case other: CurrencyAmountArray =>
      (this eq other) || (currency == other.currency && values == other.values)
    case _ => false
  }

  /**
   * Returns a hash code consistent with [[equals]].
   *
   * The mixing is that of the bean this replaces - a seed, then each field in declaration
   * order - with the values contributing the hash of [[DoubleArray]], which is computed from
   * the bit patterns of its elements and therefore agrees with the equality above. The seed is
   * the hash of the type's own name rather than the identity hash of its class, which the
   * generated bean used: that makes the hash of an array a function of the array alone,
   * identical in every run of every program, which is what the byte-stability and round-trip
   * properties of the test suite depend on.
   *
   * @return the hash code of the currency and values held
   */
  override def hashCode: Int =
    (CurrencyAmountArray.HashSeed * 31 + currency.hashCode) * 31 + values.hashCode

  /**
   * Returns this array as text.
   *
   * The form is the one the generated bean produced, the two fields named in declaration order
   * between braces, as in `CurrencyAmountArray{currency=GBP, values=[1.0, 2.0]}`. It is kept
   * exactly so that ported code and the logs it writes read as they did before, and it is what
   * the `Show` instance of the companion renders. Every value of the array appears, so the text
   * of a long run is long - it describes the whole value rather than summarising it, as the
   * text of the type being ported did.
   *
   * @return the rendering of this array
   */
  override def toString: String = s"CurrencyAmountArray{currency=$currency, values=$values}"
}

/**
 * Provides the ways of obtaining an array of amounts, and the instances for the type.
 *
 * This companion is the only place a [[CurrencyAmountArray]] is created. Neither the
 * constructor nor a generated `apply` or `copy` is available and the type is sealed, so an
 * array assembled from amounts of several currencies, or from no amounts at all, cannot exist:
 * the two factories that read such input report it instead.
 *
 * Two typeclass instances are published, and exactly two: a `Hash`, which is the single
 * equality-bearing instance of the type - `Hash` extends `Eq`, so declaring an `Eq` as well
 * would leave two instances that could disagree and one of them ambiguous - and a `Show`. There
 * is deliberately no `Order`, matching the type being ported, which is not comparable.
 *
 * @see [[CurrencyAmountArray]] for the type itself and for what an array may hold
 */
object CurrencyAmountArray {

  /**
   * The seed of the hash code, standing in for the identity hash of the class that the
   * generated bean used, so that hashing is reproducible across runs. Any constant would do;
   * the hash of the type name is used because it is stable, specified by the platform, and
   * distinct from the seed of every other type of this port.
   */
  private val HashSeed: Int = "CurrencyAmountArray".hashCode

  /**
   * The name of the collection of amounts, as the argument name of the check that rejects it.
   *
   * This is the name that appears in `Argument iterable 'amounts' must not be empty`, the
   * message reported for a collection holding no amount.
   */
  private val AmountsField: String = "amounts"

  /**
   * The name of the size, as the argument name of the check that rejects it.
   *
   * This is the name the type being ported gave the same argument, so the message reported for
   * a size the type does not admit is worded as it was there.
   */
  private val SizeField: String = "size"

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance from the specified currency and array of values.
   *
   * This is total. A currency and an array of numbers always describe a run of amounts, so
   * there is nothing to reject - the only check the type being ported made was that neither
   * reference was absent, a state the types here cannot express. An array holding a value that
   * is not a number is admitted exactly as it was there, and reading that element back is what
   * raises the invariant of [[CurrencyAmount]].
   *
   * {{{
   * CurrencyAmountArray.of(Currency.GBP, DoubleArray.of(1d, 2d, 3d))
   * }}}
   *
   * @param currency  the currency of the values
   * @param values  the values, which the array holds without copying, since the array of
   *   values is itself immutable
   * @return an instance with the specified currency and values
   */
  def of(currency: Currency, values: DoubleArray): CurrencyAmountArray = create(currency, values)

  /**
   * Obtains an instance from the specified collection of amounts.
   *
   * The amounts have to be in one currency, which becomes the currency of the array, and there
   * has to be at least one of them - an array of amounts with no currency to name is not a
   * value this type has. The values are taken in the order the collection presents them:
   *
   * {{{
   * CurrencyAmountArray.of(List(gbp1, gbp2))   // Right(GBP [1.0, 2.0])
   * CurrencyAmountArray.of(List(gbp1, usd2))   // Left(one failure naming both currencies)
   * CurrencyAmountArray.of(Nil)                // Left(one failure naming the empty collection)
   * }}}
   *
   * Both conditions are properties of the collection a caller holds rather than of the calling
   * code, so both are reported as failures. The type being ported reported neither as a value:
   * a collection of several currencies raised an argument exception from inside a stream
   * reduction, and an empty collection raised a no-such-element exception from reading the
   * result of that reduction - a failure it never described, since nothing there named the
   * empty case. Here it is named.
   *
   * The two checks are combined rather than sequenced, so the outcome is the shape every
   * checked type of this port has and a caller reads one chain of reasons. In practice the two
   * are mutually exclusive - a collection that holds no amount names no currency to disagree
   * about - so the chain holds one failure; combining them is what keeps that a property of
   * these particular checks rather than of the code that reports them.
   *
   * @param amounts  the amounts, at least one, all in the same currency
   * @return an instance holding the amounts, or the failures describing why they do not
   *   describe one
   */
  def of(amounts: Iterable[CurrencyAmount]): ResultNec[CurrencyAmountArray] = {
    // the collection is read into a list once: it may be a lazy or a single-use collection,
    // and it is traversed three times below - for the emptiness check, for its currencies and
    // for its values
    val requested = amounts.toList
    val present = Validate.notEmpty(requested, AmountsField)
    // the currencies are de-duplicated first, so the check is that the amounts name one
    // currency rather than that there is one amount, and at most two of them are ever read
    val single = Validate.fromResult(
      Collections.ensureOnlyOne(requested.iterator.map(_.currency).distinct))
    (present, single)
      .mapN((checked, _) =>
        // reached only when both checks passed, so `checked` holds at least one amount and
        // every amount in it is in the currency the second check found: the currency of the
        // first amount is therefore the currency of all of them
        create(checked.head.currency, DoubleArray.copyOf(requested.map(_.amount))))
      .toEither
  }

  /**
   * Obtains an instance using a function to create the amounts.
   *
   * The function is passed each index, counting from zero, and returns the amount at that
   * index. The size has to be at least one and every amount has to be in the currency of the
   * amount at index zero, which becomes the currency of the array:
   *
   * {{{
   * CurrencyAmountArray.of(3, i => gbp(i.toDouble))          // Right(GBP [0.0, 1.0, 2.0])
   * CurrencyAmountArray.of(0, i => gbp(i.toDouble))          // Left(the size failure)
   * CurrencyAmountArray.of(2, i => if (i == 0) gbp1 else usd2) // Left(the currency failure)
   * }}}
   *
   * The function is evaluated exactly once per index, in index order, so a function that counts
   * its calls or reads a sequence of inputs sees each index once. It is evaluated for every
   * index even when an early amount is in the wrong currency, where the type being ported
   * stopped at the first such amount; that is what allows every currency that disagrees to be
   * reported rather than only the first, and it is the one behaviour of this factory that
   * differs from it. A currency that disagrees at several indices is reported once, since the
   * failure names the currencies rather than the positions.
   *
   * The size is checked before the function is evaluated at all, exactly as it was there, so a
   * size the type does not admit is reported without the function being called. The two
   * conditions are consequently sequenced rather than combined: a rejected size leaves no
   * amounts to examine, so there is no second set of reasons to accumulate alongside it.
   *
   * A caller that has the currency in hand and needs only the numbers builds the values with
   * `DoubleArray.tabulate` and uses `of(currency, values)` instead, which cannot fail.
   *
   * @param size  the number of amounts, at least one
   * @param valueFunction  the function used to obtain the amount at each index
   * @return an instance holding the amounts the function produced, or the failures describing
   *   why they do not describe one
   */
  def of(size: Int, valueFunction: Int => CurrencyAmount): ResultNec[CurrencyAmountArray] =
    Validate
      .notNegativeOrZero(size, SizeField)
      .toEither
      .flatMap { checkedSize =>
        // one evaluation per index, in index order, and the amounts are held while their
        // currencies are examined so that no index is evaluated twice
        val produced = Vector.tabulate(checkedSize)(valueFunction)
        // the size is at least one, so there is an amount at index zero and its currency is the
        // currency every other amount is required to be in
        val currency = produced.head.currency
        val differing = produced.iterator.map(_.currency).filter(_ != currency).distinct
        Collections
          .toNonEmptyChain(differing.map(other => differingCurrencies(currency, other)))
          .toLeft(create(currency, DoubleArray.copyOf(produced.map(_.amount))))
      }

  //-------------------------------------------------------------------------
  /**
   * Builds an instance without checking anything, which is the only call of the constructor.
   *
   * Every route into the type ends here, and each of them has already established what it has
   * to: the total factory has nothing to establish, and the two checking factories have found
   * that the amounts name one currency. Keeping the construction in one place is what makes
   * that reviewable.
   *
   * @param currency  the currency of the values
   * @param values  the values
   * @return the array of amounts
   */
  private def create(currency: Currency, values: DoubleArray): CurrencyAmountArray =
    new CurrencyAmountArray(currency, values) {}

  /**
   * The failure reported when two arrays that have to be combined differ in size.
   *
   * The wording is that of the type being ported, so a log or an expectation carrying the
   * message of the exception it raised carries this message unchanged.
   *
   * @param thisSize  the size of the array the operation was called on
   * @param otherSize  the size of the other array
   * @return the failure describing the mismatch
   */
  private def differentSizes(thisSize: Int, otherSize: Int): Failure =
    Failure.Invalid(s"Sizes must be equal, this size is $thisSize, other size is $otherSize")

  /**
   * The failure reported when an array is combined with amounts of another currency.
   *
   * The wording is that of the type being ported, and it is shared by the members that combine
   * two arrays and the members that combine an array with a single amount, exactly as it was
   * shared there.
   *
   * @param thisCurrency  the currency of the array the operation was called on
   * @param otherCurrency  the currency of the other array or amount
   * @return the failure describing the mismatch
   */
  private def differentCurrencies(thisCurrency: Currency, otherCurrency: Currency): Failure =
    Failure.Invalid(
      s"Currencies must be equal, this currency is $thisCurrency, " +
        s"other currency is $otherCurrency")

  /**
   * The failure reported when the amounts offered to a factory are not all in one currency.
   *
   * The wording is that of the type being ported, which named the currency of the first amount
   * and the currency that disagreed with it.
   *
   * @param first  the currency of the amount at index zero
   * @param other  the currency that disagrees with it
   * @return the failure describing the disagreement
   */
  private def differingCurrencies(first: Currency, other: Currency): Failure =
    Failure.Invalid(s"Currencies differ: $first and $other")

  //-------------------------------------------------------------------------
  /**
   * The hashing and equality of arrays of amounts.
   *
   * Taken from the `equals` and `hashCode` of the type, which compare the currency and then the
   * values element by element on their bit patterns. This is the type's only equality-bearing
   * instance, and `Eq[CurrencyAmountArray]` is obtained from it by subtyping.
   *
   * @return the hashing of arrays of amounts
   */
  implicit val hash: Hash[CurrencyAmountArray] = Hash.fromUniversalHashCode[CurrencyAmountArray]

  /**
   * The rendering of arrays of amounts as text.
   *
   * Renders what `toString` renders, which is the form of the type being ported.
   *
   * @return the rendering of an array of amounts
   */
  implicit val show: Show[CurrencyAmountArray] = Show.show(_.toString)

  //-------------------------------------------------------------------------
  // The codec of the values field, brought into scope for the two derivations below and for
  // nothing else. The array has to be taken from here rather than from the JSON library, which
  // has no instance for it at all and whose instance for a double cannot express a value that
  // is not a number; importing them at this point is what makes that choice deliberate and
  // local, as the codec support of `strata-collect` intends.
  import Codecs.implicits._

  /**
   * The raw field shape both codecs of this type are derived over.
   *
   * A type whose constructor is private cannot be derived over directly, so this product is the
   * shape of its two fields and is what makes the derivation possible in both directions: the
   * decoder below derives over it and then builds through the factory, and the encoder below
   * derives over it and is contramapped from an array that already exists. It exists only for
   * that purpose - it is private and it is never returned, so no caller can hold an unchecked
   * pair. Its field names are the JSON keys, and they are the names of the two fields of
   * [[CurrencyAmountArray]] itself, which is what keeps the derived shape and the type from
   * drifting apart.
   *
   * @param currency  the currency, read from its three letter code
   * @param values  the values, read as a JSON array whose elements are numbers or one of the
   *   three tagged strings
   */
  private final case class Raw(currency: Currency, values: DoubleArray)

  /** The derived decoder of the raw field shape, used by the decoder below. */
  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /**
   * The derived encoder of the raw field shape, used by the encoder below.
   *
   * This sits after the import above deliberately: the derivation picks up the array codec of
   * this port from that import, which is what writes an infinite element as its tagged string.
   * A derivation site without that import in scope would not compile at all, the JSON library
   * having no encoder for the array type.
   */
  private val rawEncoder: Encoder[Raw] = deriveEncoder[Raw]

  /**
   * The JSON encoding of arrays of amounts.
   *
   * An array is an object of two fields, the currency as its code and the values as a JSON
   * array:
   *
   * {{{
   * {"currency":"GBP","values":[1.0,2.0,"Infinity"]}
   * }}}
   *
   * The shape is derived when this file is compiled rather than written out field by field,
   * which is what every product of this port does, and no part of it inspects a class while the
   * program runs. Every element goes through the single policy of this port for a double, so a
   * value that is not a number and the two infinities appear as the strings `"NaN"`,
   * `"Infinity"` and `"-Infinity"` while a finite value is written as a JSON number, exactly to
   * the bit - which is what lets every array this type admits survive a round trip. The result
   * is wrapped so that a field holding no value would be omitted, the policy every product of
   * this port follows; this type has no optional field, so the wrapping changes nothing about
   * its output and exists so that the policy holds without exception.
   *
   * @return the JSON encoding of an array of amounts
   */
  implicit val encoder: Encoder[CurrencyAmountArray] =
    Codecs.dropNulls(
      rawEncoder.contramap[CurrencyAmountArray](value => Raw(value.currency, value.values)))

  /**
   * The JSON decoding of arrays of amounts.
   *
   * This is the inverse of the encoding above and is likewise derived at compile time. Both
   * fields have to be present, and the payload is read into the raw shape and handed to the
   * factory, so the only route into the type from a document is the same route a caller takes.
   * That factory is the total one - a currency and an array of values always describe a run of
   * amounts - so nothing beyond the payload itself is refused here; what a document can get
   * wrong is refused by the field codecs, which report a currency code this port does not hold
   * and an element that is neither a number nor one of the three tagged strings. The failure
   * channel is kept rather than elided so that this type decodes exactly as every other checked
   * product of this port does, and so that a check added to the factory needs no change here.
   *
   * @return the JSON decoding of an array of amounts
   */
  implicit val decoder: Decoder[CurrencyAmountArray] =
    Codecs.validatedDecoder[Raw, CurrencyAmountArray](raw => Right(of(raw.currency, raw.values)))(
      rawDecoder)
}
