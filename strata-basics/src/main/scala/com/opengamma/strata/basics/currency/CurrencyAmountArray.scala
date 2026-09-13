/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import scala.collection.AbstractIndexedSeqView

import cats.Hash
import cats.Show
import cats.syntax.apply._

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
 * There are three ways in, and they differ in what they have to check:
 *
 *   - `of(currency, values)` is total in signature. A currency and an array of numbers describe
 *     a run of amounts whenever every number is one an amount holds, which is the element
 *     invariant below.
 *   - `of(amounts)` reads a collection of amounts, which has to hold at least one amount and
 *     has to name a single currency, so it reports what it rejects.
 *   - `of(size, valueFunction)` builds the amounts from a function of the index, which likewise
 *     has to produce at least one amount and has to produce them all in one currency.
 *
 * The two checking factories report their reasons as a chain of [[Failure]] rather than by
 * abandoning the call, since each rejection depends on the data a caller holds. Neither a public
 * `apply` nor a `copy` exists - the constructor is private and the type is sealed - so the only
 * route to a value is one of those three, and an array whose amounts disagree about their
 * currency cannot be built at all.
 *
 * ===The element invariant===
 *
 * Every value an array holds is a value [[CurrencyAmount]] holds: the elements of a run are
 * amounts kept as numbers, so a value that is not a number is no more an element of a run than
 * it is an amount. The two infinities are numbers an amount holds and are therefore elements a
 * run holds; only a not-a-number value is refused. The invariant holds of every value of this
 * type rather than of the values of some of its routes, and reading an element - [[get]],
 * [[iterator]], [[toList]] - cannot raise it.
 *
 * It costs one pass over the values, and exactly one: the invariant is established by whichever
 * route takes numbers in from outside - each examines them once, then raises or reports what it
 * found and hands the values on to the single construction point, which examines nothing further.
 * A route whose numbers are the amounts of [[CurrencyAmount]] values examines nothing at all,
 * since the invariant of that type has already established of every one of them exactly what this
 * one asks. Restating the invariant a second time where the values are handed over would double
 * the cost of every element-wise operation on a run of a hundred thousand scenarios - the pass
 * that establishes it is as long as the pass that does the arithmetic - which is why it is stated
 * once and where the numbers arrive.
 *
 * Which channel reports a refused element is the channel the route in question already has,
 * which is the policy of this port for a numeric-domain edge:
 *
 *   - the routes that already report failures - the decoder, the four members that add or
 *     subtract, and [[convertedTo]] - report `Argument 'values' must not be NaN at index N`
 *     as a [[Failure]], so a document carrying the tagged string `"NaN"`, the sum of two
 *     opposite infinities and a conversion at a rate that is not a number are all answers a
 *     caller reads rather than exceptions thrown past it;
 *   - the routes that are total in signature - `of(currency, values)`, [[multipliedBy]] and
 *     [[mapAmounts]] - raise that same wording as a documented invariant, exactly as the
 *     arithmetic of [[CurrencyAmount]] raises its own invariant for the sum of two opposite
 *     infinities rather than widening into a failure channel the type being ported did not have.
 *
 * ===Which operations can fail===
 *
 * Adding or subtracting another array fails when the two arrays have different sizes or
 * different currencies, and adding or subtracting a single amount fails when its currency is
 * not the currency of the array: in each case the result would have no meaning, and whether it
 * happens is a property of the values involved. Each of the four also fails where the sum or
 * difference holds a value that is not a number, which the element invariant above refuses.
 * Converting fails when the rate the conversion needs is unavailable, and where the rate on
 * offer turns a value into one that is not a number. Reading an element is total. Scaling and
 * mapping every element are total in signature and raise the element invariant for a product or
 * an image that is not a number.
 *
 * ===Equality===
 *
 * Two arrays are equal when their currencies are equal and their values are equal element by
 * element, compared by bit pattern as [[DoubleArray]] compares them. That differs from a numeric
 * comparison in one place the round-trip properties of the test suite rely on: a negative zero
 * differs from a positive zero. The other place a bit comparison differs, an element that is not
 * a number comparing equal to itself, is unreachable here - the element invariant above admits no
 * such element - and it is [[DoubleArray]] itself, which does admit one, that the property rests
 * on.
 *
 * There is deliberately no ordering: a run of amounts has none worth inventing.
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
    extends FxConvertible[CurrencyAmountArray]
    with NoJavaSerialization {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means can be
  // stopped is here. The single implementation is the companion's hidden `Impl`.
  JvmClosure.requireSoleImplementation(this, classOf[CurrencyAmountArray.Impl])

  // The element invariant of this type - every element of its values is a number - is deliberately
  // not restated here, and this is the one statement of why.
  //
  // A class body is where the construction closure states an invariant, because the implementation
  // class carries a public constructor in the class file whatever the source asked for: a class
  // compiled outside this library can call it directly, and `requireSoleImplementation` above
  // admits exactly that class, so an invariant restated here is the only thing that also holds of
  // an instance forged that way. Where an invariant costs a constant, or costs one step per
  // currency of a value, restating it is free and every type of this package restates it -
  // `CurrencyAmount` its own two, `MultiCurrencyAmountArray` its size and its per-currency lengths.
  //
  // This one costs a pass over the whole run. A constructor runs for every value built, so
  // restating it here would examine every element of every array a second time - and a third time
  // where the route that reports failures has to examine them itself to say which element it
  // refused - doubling the cost of `of` and of each element-wise operation on a run of a hundred
  // thousand scenarios, for a route the supported API does not have. It is therefore established
  // once, by whichever route takes the numbers in, and `CurrencyAmountArray.trusted` is the
  // construction point they all reach once they have: `create` raises it, `checked` reports it, and
  // the two factories that read amounts rely on the invariant of `CurrencyAmount` instead. The
  // invariant consequently holds of every value this library builds; what a forged instance would
  // hold is not examined here, and a value that is not a number in one would be refused by
  // `CurrencyAmount` as soon as any element of it were read as an amount.

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
   * This is total in signature, and it raises one documented invariant rather than widening into
   * a failure channel: an index outside the array is the index exception of the runtime, exactly
   * as reading the [[DoubleArray]] directly would be. The invariant of [[CurrencyAmount]] cannot
   * be raised from here, although the amount is built through the construction path that performs
   * it: the element invariant of this type holds of every value of the type, so every value of the
   * array is already a value an amount holds.
   *
   * @param index  the zero-based index to retrieve
   * @return the amount at that index
   * @throws java.lang.IndexOutOfBoundsException if the index is outside the array
   */
  def get(index: Int): CurrencyAmount = amountOf(values.get(index))

  /**
   * Returns the amounts of this array, one at a time.
   *
   * The result is a lazy sequence traversed once, for a caller that filters, maps or folds the
   * amounts in order:
   *
   * {{{
   * array.iterator.filter(_.isPositive).toList
   * }}}
   *
   * The amounts are produced in index order and each is built only as it is read, so a caller
   * that stops early never builds the rest. [[toList]] is the eager form, for a caller that
   * wants every amount at once.
   *
   * The traversal is over the indices of the array, each value being read with the primitive
   * accessor of [[DoubleArray]] as its amount is built, rather than over the generic iterator
   * of the values: a generic iterator of numbers would box every element of the run and unbox
   * it again on the way into the amount, which is precisely the cost of one object per number
   * that this type exists to avoid. Reading by index is also what makes this and [[get]] one
   * behaviour rather than two - the amount at an index is the same amount either way.
   *
   * The indices themselves are not carried through a function of the index either. A traversal
   * written as a mapped range of indices would hand each index to a function whose argument is
   * an amount rather than a number, a shape with no primitive specialisation in this language, so
   * every index would be boxed on its way into the function - the same cost as boxing the values,
   * moved to the other operand. The traversal is therefore an indexed view whose element accessor
   * takes the index as a primitive, and the iterator of that view reads it straight through: a
   * consumed element costs the one amount it answers with and nothing else.
   *
   * The traversal raises nothing. Each amount is built through the construction path of
   * [[CurrencyAmount]], which performs that type's whole invariant, and every value of the array
   * already satisfies it: the element invariant of this type was established before the array
   * existed.
   *
   * @return the amounts of this array in index order, built as they are read
   */
  def iterator: Iterator[CurrencyAmount] = new CurrencyAmountArray.Amounts(this).iterator

  /**
   * Returns the amounts of this array as a list.
   *
   * This is [[iterator]] traversed to its end, for a caller that wants the amounts as a
   * collection rather than as a traversal. The list holds one amount per value of the array and
   * is built once, so it costs an object per element - which is exactly the representation this
   * type exists to avoid, and is why it is obtained explicitly rather than being how the type
   * holds its data. It raises nothing, for the reason [[iterator]] gives.
   *
   * @return the amounts of this array in index order
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
   * This is total in signature, as it was in the type being ported, and it raises the element
   * invariant of this type for a product that is not a number - a factor of zero applied to an
   * infinite element, or a factor that is itself not a number. That is the policy of this port
   * for a numeric-domain edge reached from the values a caller chose: the arithmetic of
   * [[CurrencyAmount]] raises its own invariant for the sum of two opposite infinities in exactly
   * the same way.
   *
   * @param factor  the multiplicative factor
   * @return a copy of this array with every amount multiplied by the factor
   * @throws java.lang.IllegalArgumentException if a product is not a number
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
   * unchanged, since the operation is on the numbers alone. It is total: every number is a value
   * the array may hold.
   *
   * The operation may produce a value that is not a number, which is a value the elements of a
   * run do not include: this is total in signature and raises the element invariant of this type
   * for such an image, as [[multipliedBy]] does for such a product. The refusal happens here,
   * where the value was produced, rather than later where something read it back.
   *
   * @param mapper  the operation to apply to each amount
   * @return a copy of this array with the operation applied to every amount
   * @throws java.lang.IllegalArgumentException if the operation produces a value that is not a
   *   number
   */
  def mapAmounts(mapper: Double => Double): CurrencyAmountArray =
    CurrencyAmountArray.create(currency, values.map(mapper))

  /**
   * Converts this array into the specified currency, taking the rate from the specified provider.
   *
   * This is the [[FxConvertible]] implementation of this type. An array already in the requested
   * currency is returned unchanged and the provider is not consulted, so such a conversion
   * succeeds even under a provider that supplies no rates at all - a single-currency run needs no
   * rate for a conversion into its own currency.
   *
   * Otherwise the rate is looked up exactly once, for the pair of the two currencies, and every
   * value is multiplied by that one rate. Looking it up once rather than once per element is
   * faster, and it means the whole run is converted at a single rate even under a provider whose
   * answers could vary between calls.
   *
   * A rate that is not a number is a rate an [[FxRate]] holds - it is not negative and not zero,
   * which is all that type asks of one - and applying it would produce values the elements of a
   * run do not include. The converted values therefore go through the checked construction of
   * this type, so such a conversion is reported in the failure channel this member already has
   * rather than raised out of it.
   *
   * @param resultCurrency  the currency of the result
   * @param rateProvider  the provider of FX rates
   * @return this array expressed in the result currency, the failure the provider reported for
   *   the rate the conversion needed, or the failure describing a converted value that is not a
   *   number
   */
  override def convertedTo(
      resultCurrency: Currency,
      rateProvider: FxRateProvider): FailureOr[CurrencyAmountArray] =
    if (currency == resultCurrency) {
      Right(this)
    } else {
      rateProvider
        .fxRate(currency, resultCurrency)
        .flatMap(rate =>
          CurrencyAmountArray.checkedOne(resultCurrency, values.multipliedBy(rate)))
    }

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
   * The size is checked before the currency, so an array that differs in both is reported as
   * differing in size.
   *
   * The sums themselves go through the checked construction of this type, which is reached only
   * once the two arrays agree: a sum that is not a number - an element of `+∞` added to an
   * element of `-∞` - is a value the elements of a run do not include, and it is reported in the
   * failure channel this member already has rather than raised out of it.
   *
   * @param other  the other array of amounts
   * @return this array with the other added element by element, or the failure describing the
   *   size or currency mismatch or the sum that is not a number
   */
  def plus(other: CurrencyAmountArray): FailureOr[CurrencyAmountArray] =
    matching(other).flatMap(_ =>
      CurrencyAmountArray.checkedOne(currency, values.plus(other.values)))

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
   *   currency mismatch or the sum that is not a number
   */
  def plus(amount: CurrencyAmount): FailureOr[CurrencyAmountArray] =
    sameCurrency(amount.currency)
      .flatMap(_ => CurrencyAmountArray.checkedOne(currency, values.plus(amount.amount)))

  /**
   * Returns an array holding the values of this array with the values of the other subtracted.
   *
   * This is the element-wise `plus` above read in the other direction and behaves the same way
   * in every respect: the amounts are
   * subtracted element by element, the sizes have to be equal, the currencies have to be equal,
   * the size is checked first, and a difference that is not a number - an element of `+∞` less
   * an element of `+∞` - is reported by the checked construction of this type.
   *
   * @param other  the other array of amounts
   * @return this array with the other subtracted element by element, or the failure describing
   *   the size or currency mismatch or the difference that is not a number
   */
  def minus(other: CurrencyAmountArray): FailureOr[CurrencyAmountArray] =
    matching(other).flatMap(_ =>
      CurrencyAmountArray.checkedOne(currency, values.minus(other.values)))

  /**
   * Returns an array holding the values of this array with the specified amount subtracted.
   *
   * The amount is subtracted from every element, and its currency has to be the currency of the
   * array. Subtracting a zero amount answers with this instance.
   *
   * @param amount  the amount to subtract from every element
   * @return this array with the amount subtracted from every element, or the failure describing
   *   the currency mismatch or the difference that is not a number
   */
  def minus(amount: CurrencyAmount): FailureOr[CurrencyAmountArray] =
    sameCurrency(amount.currency)
      .flatMap(_ => CurrencyAmountArray.checkedOne(currency, values.minus(amount.amount)))

  /**
   * Pairs a value of this array with the currency of this array.
   *
   * This is the route every member producing amounts takes, and it is deliberately the total
   * one: it goes through the trusted construction path of [[CurrencyAmount]], the one its own
   * arithmetic goes through, which performs that type's whole invariant - a negative zero is
   * normalised to a positive zero and a value that is not a number is refused - and allocates
   * exactly the amount it returns. The checking factory of [[CurrencyAmount]] is not used,
   * because it would widen reading an element into a failure channel this type does not have.
   * Nothing is lost by that: the element invariant of this type is the invariant of
   * [[CurrencyAmount]] applied to every value before the array exists, so the check performed here
   * can no longer refuse a value - it is the same check, reached with a value already known to
   * pass it.
   *
   * Building the amount directly rather than adding the value to a zero amount is what keeps
   * reading a run of amounts to one object per amount read: the other route allocates a zero
   * amount and a function capturing the value on top of the amount it answers with, three
   * objects in place of one.
   *
   * @param value  the value to pair with the currency of this array, which the element invariant
   *   of this type has already established is one an amount holds
   * @return the amount holding that value in the currency of this array
   */
  private def amountOf(value: Double): CurrencyAmount = CurrencyAmount.create(currency, value)

  /**
   * Checks that the other array can be combined with this one element by element.
   *
   * The size is examined first and the currency second: an array that differs in both is
   * reported as differing in size, and the currency check is not reached. Both members that
   * combine two arrays share this, so the two report the same reasons in the same order.
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

  /**
   * Checks whether this array equals another object.
   *
   * Another array is equal when it holds the same currency and values that are equal element by
   * element. The values are compared by [[DoubleArray]], whose equality is bit for bit, so an
   * element that is not a number equals itself and a negative zero differs from a positive zero.
   * An object of any other type is not equal.
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
   * The mixing is a seed, then each field in declaration order, with the values contributing the
   * hash of [[DoubleArray]], which is computed from the bit patterns of its elements and
   * therefore agrees with the equality above. The seed is the hash of the type's own name rather
   * than the identity hash of its class, which makes the hash of an array a function of the array
   * alone, identical in every run of every program.
   *
   * @return the hash code of the currency and values held
   */
  override def hashCode: Int =
    (CurrencyAmountArray.HashSeed * 31 + currency.hashCode) * 31 + values.hashCode

  /**
   * Returns this array as text.
   *
   * The form is the two fields named in declaration order between braces, as in
   * `CurrencyAmountArray{currency=GBP, values=[1.0, 2.0]}`, and it is what the `Show` instance of
   * the companion renders. Every value of the array appears, so the text of a long run is long:
   * it describes the whole value rather than summarising it.
   *
   * @return the rendering of this array
   */
  override def toString: String = s"CurrencyAmountArray{currency=$currency, values=$values}"
}

/**
 * Provides the ways of obtaining an array of amounts, and the instances for the type.
 *
 * This companion is the only place a [[CurrencyAmountArray]] is created. Neither the
 * constructor nor a synthesised `apply` or `copy` is available and the type is sealed, so an
 * array assembled from amounts of several currencies, or from no amounts at all, cannot exist:
 * the two factories that read such input report it instead.
 *
 * Two typeclass instances are published, and exactly two: a `Hash`, which is the single
 * equality-bearing instance of the type - `Hash` extends `Eq`, so declaring an `Eq` as well
 * would leave two instances that could disagree and one of them ambiguous - and a `Show`. There
 * is deliberately no `Order`.
 *
 * @see [[CurrencyAmountArray]] for the type itself and for what an array may hold
 */
object CurrencyAmountArray {

  /**
   * The seed of the hash code, which makes hashing reproducible across runs rather than
   * dependent on the identity hash of a class. Any constant would do; the hash of the type name
   * is used because it is stable, specified by the platform, and distinct from the seed of every
   * other type that hashes this way.
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
   * This is the name that appears in the message reported for a size the type does not admit.
   */
  private val SizeField: String = "size"

  /**
   * The name of the values, as the argument name of the check that rejects an element.
   *
   * This is the name of the field itself, so the message reported for an element the type does
   * not admit names the thing a caller passed - `Argument 'values' must not be NaN at index 1`.
   */
  private val ValuesField: String = "values"

  /**
   * Obtains an instance from the specified currency and array of values.
   *
   * This is total in signature. A currency and an array of numbers describe a run of amounts
   * whenever every number is one an amount holds, which is the element invariant of this type.
   * The two infinities are admitted, as [[CurrencyAmount]] admits them; a value that is not a
   * number is refused here, where the array is handed over, rather than later where an element of
   * it is read:
   *
   * {{{
   * CurrencyAmountArray.of(Currency.GBP, DoubleArray.of(1d, 2d, 3d))
   * CurrencyAmountArray.of(Currency.GBP, DoubleArray.of(1d, Double.NaN))
   *   // raises: Argument 'values' must not be NaN at index 1
   * }}}
   *
   * A caller that holds values it has not examined and wants the refusal as a value rather than
   * as an exception builds the amounts and uses `of(amounts)`, whose input is amounts rather
   * than numbers and which therefore cannot carry one that is not a number at all.
   *
   * @param currency  the currency of the values
   * @param values  the values, which the array holds without copying, since the array of
   *   values is itself immutable
   * @return an instance with the specified currency and values
   * @throws java.lang.IllegalArgumentException if a value is not a number
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
   * code, so both are reported as failures.
   *
   * The two checks are combined rather than sequenced, so a caller reads one chain of reasons. In
   * practice the two are mutually exclusive - a collection that holds no amount names no currency
   * to disagree about - so the chain holds one failure; combining them is what keeps that a
   * property of these particular checks rather than of the code that reports them.
   *
   * @param amounts  the amounts, at least one, all in the same currency
   * @return an instance holding the amounts, or the failures naming the broken condition: the
   *   collection has to hold at least one amount, and every amount in it has to be in one
   *   currency
   */
  def of(amounts: Iterable[CurrencyAmount]): ResultNec[CurrencyAmountArray] = {
    // the collection is read into an indexed vector once: it may be a lazy or a single-use
    // collection, and it is read three times below - for the emptiness check, for its
    // currencies and for its values. It is held indexed rather than linked because the values
    // are taken by index: that is what lets the primitive array of the result be filled
    // straight from the amounts, with no collection of boxed numbers in between
    val requested: Vector[CurrencyAmount] = amounts.toVector
    val present = Validate.notEmpty(requested, AmountsField)
    // the currencies are de-duplicated first, so the check is that the amounts name one
    // currency rather than that there is one amount, and at most two of them are ever read
    val single = Validate.fromResult(
      Collections.ensureOnlyOne(requested.iterator.map(_.currency).distinct))
    (present, single)
      .mapN((checked, _) =>
        // reached only when both checks passed, so `checked` - which is the vector itself,
        // carried through the check at the wider type the check declares - holds at least one
        // amount and every amount in it is in the currency the second check found: the
        // currency of the first amount is therefore the currency of all of them.
        //
        // The values are tabulated by index off the vector rather than mapped into a
        // collection of numbers and copied: the function handed to `tabulate` is an
        // `Int => Double`, the primitive specialisation of a one-argument function, so each
        // amount's number is written straight into the array of the result and none of them is
        // boxed on the way.
        //
        // The values are handed to the construction point directly, because there is nothing
        // left to examine: every one of them is the amount of a `CurrencyAmount`, and the
        // invariant of that type - stated in its own class body, so it holds of every instance
        // of it that exists - is that its amount is a number. Examining them here would be
        // examining `CurrencyAmount`'s invariant a second time, once per element of the run
        trusted(
          checked.head.currency,
          DoubleArray.tabulate(requested.size)(index => requested(index).amount)))
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
   * index even when an early amount is in the wrong currency, which is what allows every currency
   * that disagrees to be reported rather than only the first. A currency that disagrees at
   * several indices is reported once, since the failure names the currencies rather than the
   * positions.
   *
   * The size is checked before the function is evaluated at all, so a size the type does not
   * admit is reported without the function being called. The two conditions are consequently
   * sequenced rather than combined: a rejected size leaves no amounts to examine, so there is no
   * second set of reasons to accumulate alongside it.
   *
   * A caller that has the currency in hand and needs only the numbers builds the values with
   * `DoubleArray.tabulate` and uses `of(currency, values)` instead, which reports nothing - it
   * has no currency to disagree about and no emptiness to reject, and the one thing it does
   * refuse, a value that is not a number, it refuses as the documented invariant of that
   * factory.
   *
   * @param size  the number of amounts, at least one
   * @param valueFunction  the function that produces the amount at each index
   * @return an instance holding the amounts the function produced, or the failures naming the
   *   broken condition: the size has to be at least one, and every amount produced has to be in
   *   the currency of the amount at index zero
   */
  def of(size: Int, valueFunction: Int => CurrencyAmount): ResultNec[CurrencyAmountArray] =
    Validate
      .notNegativeOrZero(size, SizeField)
      .toEither
      .flatMap { checkedSize =>
        // one evaluation per index, in index order, and the amounts are held indexed while
        // their currencies are examined so that no index is evaluated twice and so that their
        // values can afterwards be read by index
        val produced: Vector[CurrencyAmount] = Vector.tabulate(checkedSize)(valueFunction)
        // the size is at least one, so there is an amount at index zero and its currency is the
        // currency every other amount is required to be in
        val currency = produced.head.currency
        val differing = produced.iterator.map(_.currency).filter(_ != currency).distinct
        Collections
          .toNonEmptyChain(differing.map(other => differingCurrencies(currency, other)))
          // the values are tabulated by index off the amounts already in hand, for the reason
          // the collection form above gives: an `Int => Double` is the primitive
          // specialisation of a one-argument function, so no number is boxed between the
          // amount that holds it and the array of the result. They go to the construction point
          // directly for the reason that form gives too - each is the amount of a
          // `CurrencyAmount` and is therefore already a number. The array is built only when the
          // outcome carries no failure, `toLeft` taking its argument by name
          .toLeft(
            trusted(currency, DoubleArray.tabulate(checkedSize)(index => produced(index).amount)))
      }

  /**
   * Builds an instance from values whose element invariant has already been established, which is
   * the only call of the constructor.
   *
   * Every route into the type ends here - the total factory, both checking factories, the
   * arithmetic, the mapping, the conversion and the decoder - and each of them arrives having
   * established what only it can: the two factories that read amounts have found that those
   * amounts name one currency, and every route has established the element invariant, either by
   * examining the numbers it was given ([[create]] and [[checked]]) or by taking them from
   * [[CurrencyAmount]] values that already satisfy it. Nothing is examined here.
   *
   * That the examination happens in the three callers rather than in this one method is the whole
   * of the difference between one pass over a run and two. A single construction point that
   * examined its own arguments would examine them again after the route that has to report a
   * refusal - rather than raise it - had already examined them to find the element to name, and
   * would examine the values of a run assembled from amounts that cannot carry a refused element
   * at all. What the single point is for is the constructor call and the invariant's wording,
   * which [[notANumberMessage]] states once for both channels, and those it still holds.
   *
   * @param currency  the currency of the values
   * @param values  the values, of which the element invariant of this type has been established
   * @return the array of amounts
   */
  private def trusted(currency: Currency, values: DoubleArray): CurrencyAmountArray =
    new Impl(currency, values)

  /**
   * Builds an instance, establishing the element invariant by raising it.
   *
   * This is the construction route of the members whose signature has nowhere to report a refused
   * element: the direct factory, the scaling and the mapping. The check is a fail-fast
   * [[ArgCheck]] for that reason, and the message is taken by name so that the happy path
   * allocates nothing for it. A route that has a failure channel of its own does not reach the
   * raise and does not reach this method either: [[checked]] and [[checkedOne]] perform the same
   * examination and report what it finds.
   *
   * The scan itself is `DoubleArray.indexOf`, which compares bit patterns: that finds a
   * not-a-number value however it arose, where an ordinary comparison finds none, and it is one
   * pass over the primitive array with nothing boxed and nothing allocated. It is performed once
   * per value built - the index it finds is read once for the decision and once for the message -
   * and [[trusted]], which the construction itself goes through, performs no second pass.
   *
   * @param currency  the currency of the values
   * @param values  the values
   * @return the array of amounts
   * @throws java.lang.IllegalArgumentException if a value is not a number
   */
  private def create(currency: Currency, values: DoubleArray): CurrencyAmountArray = {
    val notANumberAt: Int = values.indexOf(Double.NaN)
    ArgCheck.isTrue(notANumberAt < 0, notANumberMessage(notANumberAt))
    trusted(currency, values)
  }

  /**
   * Establishes the element invariant as [[create]] does, reporting a refused element as a chain
   * of failures rather than raising it.
   *
   * This is the route of the decoder, which has the failure channel every decoder of this port
   * has: a document whose values hold the tagged string `"NaN"` describes a run this type does
   * not have, and the only thing to do with it is to say so. Reading it into a value and letting
   * something later fail on it is precisely the defect this invariant closes.
   *
   * The examination is performed here rather than by [[create]], and the value is built by
   * [[trusted]] rather than by that method, so that a route which reports a refused element
   * examines the run exactly once: the index this scan finds decides the answer and, where the
   * answer is a failure, names the element in it. Routing through [[create]] instead would examine
   * every element a second time to raise an invariant this method has just established cannot be
   * broken. The value is the by-name argument of `cond`, so it is built only when the examination
   * found nothing.
   *
   * @param currency  the currency of the values
   * @param values  the values
   * @return the array of amounts, or the failure naming the index of the first value that is not
   *   a number
   */
  private def checked(currency: Currency, values: DoubleArray): ResultNec[CurrencyAmountArray] = {
    val notANumberAt: Int = values.indexOf(Double.NaN)
    Validate.toResult(
      Validate.cond(notANumberAt < 0, trusted(currency, values), notANumber(notANumberAt)))
  }

  /**
   * Builds an instance as [[checked]] does, reporting a single failure rather than a chain.
   *
   * The four members that add or subtract, and the two conversions - this type's own and the one
   * [[MultiCurrencyAmountArray]] performs, which collapses its currencies into a run of this
   * type - each report one reason, so routing their results through this keeps the shape of what
   * they return while the element invariant is still established by the one examination
   * [[checked]] performs.
   * The chain is collapsed by [[Failure.collapse]], which joins the reasons of several failures
   * into one; the examination reports at most one, so the collapse is the shape change alone.
   *
   * It is `private[currency]` rather than private to this companion for the conversion of
   * [[MultiCurrencyAmountArray]]: that member returns a failure channel, and its sum over the
   * currencies of a run can produce a value this type does not hold - opposed infinities in two
   * currencies, or a rate that is not a number - which has to be reported there rather than
   * raised out of it. It is not part of the published API of the module: a caller outside this
   * package reaches the type through the three factories.
   *
   * @param currency  the currency of the values
   * @param values  the values
   * @return the array of amounts, or the single failure naming the index of the first value that
   *   is not a number
   */
  private[currency] def checkedOne(
      currency: Currency,
      values: DoubleArray): FailureOr[CurrencyAmountArray] =
    checked(currency, values).left.map(Failure.collapse)

  /**
   * The one implementation of an array of amounts.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it. It is declared rather than written as an anonymous subclass at the instantiation site
   * for two reasons, both about what the class file says: a private member class is one a Java
   * compiler refuses to name, where an anonymous class is public and can be instantiated directly
   * by a caller in another language, and a named class can be compared against, which is what
   * lets [[CurrencyAmountArray]] refuse in its own constructor to be any other implementation.
   *
   * @param currency  the currency of the values
   * @param values  the values, of which every route into [[trusted]] has already established the
   *   element invariant of this type
   */
  private final class Impl(currency: Currency, values: DoubleArray)
      extends CurrencyAmountArray(currency, values)

  /**
   * The amounts of an array, as an indexed view that builds each amount as it is read.
   *
   * This is what [[CurrencyAmountArray.iterator]] and therefore
   * [[CurrencyAmountArray.toList]] traverse, and it exists for one reason: the standard library
   * iterator of an indexed view asks the view for its element with the index as a '''primitive''',
   * `apply(index: Int)`, and answers with the element itself. Nothing in the traversal is
   * therefore boxed - not the value, which is read from the [[DoubleArray]] through its primitive
   * accessor, and not the index, which a traversal written as a mapped range of indices would box
   * on its way into the mapping function, since a function from a number to an object has no
   * primitive specialisation in this language. A consumed element costs exactly the one amount it
   * answers with.
   *
   * It is a ''view'' rather than a sequence of amounts: nothing is materialised when it is built,
   * each amount is produced by the read that asks for it, and a caller that stops early never
   * builds the rest - which is the promise [[CurrencyAmountArray.iterator]] makes. The amount at
   * an index is [[CurrencyAmountArray.get]], so a traversal and a direct read are one behaviour:
   * each amount is built through the construction path of [[CurrencyAmount]], and since the
   * element invariant of the array has already established that every value is one an amount
   * holds, neither route can refuse an element.
   *
   * The view refuses Java serialization for the same reason the array it reads does: the view of
   * a sequence is `Serializable` in the standard library, so a stream naming this class would
   * otherwise be read back into a view over whatever the stream supplied as its array, bypassing
   * [[CurrencyAmountArray.of]]. It is the only class of these modules that takes part in Java
   * serialization by inheriting from the standard library rather than from the compiler's product
   * encoding, and refusing here keeps that route closed without an exception in the gate.
   *
   * @param array  the array whose amounts this view presents
   */
  private final class Amounts(array: CurrencyAmountArray)
      extends AbstractIndexedSeqView[CurrencyAmount]
      with NoJavaSerialization {

    override def length: Int = array.size

    override def apply(index: Int): CurrencyAmount = array.get(index)
  }

  /**
   * The failure reported when two arrays that have to be combined differ in size.
   *
   * The message names both sizes, the one of the array the operation was called on first.
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
   * One wording serves the members that combine two arrays and the members that combine an array
   * with a single amount, so both report the mismatch identically.
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
   * The message names the currency of the amount at index zero and the currency that disagrees
   * with it.
   *
   * @param first  the currency of the amount at index zero
   * @param other  the currency that disagrees with it
   * @return the failure describing the disagreement
   */
  private def differingCurrencies(first: Currency, other: Currency): Failure =
    Failure.Invalid(s"Currencies differ: $first and $other")

  /**
   * The wording of the element invariant, which is defined here and nowhere else.
   *
   * Both channels that report a refused element read it from here - the raise of [[create]] and
   * the failure of [[checked]] - so the two cannot drift apart: a caller that reads the message
   * of the exception and a caller that reads the message of the failure read the same sentence.
   * It names the index because an array of a hundred thousand values says nothing about which of
   * them is wrong, and the index is what the scan already found.
   *
   * @param index  the index of the value that is not a number
   * @return the message naming the argument and that index
   */
  private def notANumberMessage(index: Int): String =
    s"Argument '$ValuesField' must not be NaN at index $index"

  /**
   * The failure reported for an element that is not a number.
   *
   * The reason is the invalid-argument reason of this port, as it is for every other structural
   * rejection of this type, and the message is the one wording above.
   *
   * @param index  the index of the value that is not a number
   * @return the failure describing that element
   */
  private def notANumber(index: Int): Failure = Failure.Invalid(notANumberMessage(index))

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
   * Renders the braced two-field form `toString` renders.
   *
   * @return the rendering of an array of amounts
   */
  implicit val show: Show[CurrencyAmountArray] = Show.show(_.toString)

  // The codec of the values field, brought into scope for the two derivations below and for
  // nothing else. The array codec has to be taken from here rather than from the JSON library,
  // which has no instance for the array type at all and whose instance for a double cannot
  // express a value that is not a number; importing it at this point is what keeps that choice
  // deliberate and local.
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
   * [[CurrencyAmountArray]] itself, which keeps the derived shape and the type from drifting
   * apart.
   *
   * @param currency  the currency, read from its three letter code
   * @param values  the values, read as a JSON array whose elements are numbers or one of the
   *   three tagged strings
   */
  private final case class Raw(currency: Currency, values: DoubleArray)
      extends NoJavaSerialization

  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /**
   * The derived encoder of the raw field shape, used by the encoder below.
   *
   * This sits after the import above deliberately: the derivation picks up the array codec from
   * that import, which is what writes an infinite element as its tagged string. A derivation site
   * without that import in scope has no encoder for the array type at all.
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
   * The shape is derived from the two fields rather than written out field by field, and no part
   * of it inspects a class while the program runs. Every element is written under the single
   * policy for a double, so a value that is not a number and the two infinities appear as the
   * strings `"NaN"`, `"Infinity"` and `"-Infinity"` while a finite value is written as a JSON
   * number, exactly to the bit - which is what lets every array this type admits survive a round
   * trip. The result is wrapped so that a field holding no value is omitted; this type has no
   * optional field, so the wrapping changes nothing about its output and keeps it inside that one
   * policy.
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
   * checked construction of this type, so the only route into the type from a document
   * establishes what every other route establishes:
   *
   *   - a currency code this port does not hold and an element that is neither a number nor one
   *     of the three tagged strings are refused by the field codecs;
   *   - an element that decodes to a value the type does not admit - the tagged string `"NaN"`,
   *     which the policy of this port for a double reads as a not-a-number value - is refused
   *     here, as a decoding failure carrying `Argument 'values' must not be NaN at index N`.
   *
   * The second is the reason this decoder reports rather than calling the total factory: a
   * document is data a caller did not write, so a payload describing a run this type does not
   * have is an answer to be read, never an exception thrown from underneath a decode, and never
   * a value that reads back as a run until something touches the offending element.
   *
   * @return the JSON decoding of an array of amounts
   */
  implicit val decoder: Decoder[CurrencyAmountArray] =
    Codecs.validatedDecoder[Raw, CurrencyAmountArray](raw => checked(raw.currency, raw.values))(
      rawDecoder)
}
