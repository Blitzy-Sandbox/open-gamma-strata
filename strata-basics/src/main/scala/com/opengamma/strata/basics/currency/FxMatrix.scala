/*
 * Copyright (C) 2011 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.util.Arrays

import scala.annotation.tailrec
import scala.collection.immutable.ListSet
import scala.collection.immutable.Set

import cats.Hash
import cats.Show
import cats.syntax.apply._
import cats.syntax.traverse._

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.ValidatedFailures
import com.opengamma.strata.collect.array.DoubleMatrix
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * A matrix of foreign exchange rates.
 *
 * This provides a matrix of foreign exchange rates, such that the rate can be queried for any
 * available pair. For example, if the matrix contains the currencies `USD`, `EUR` and `GBP`, then
 * six rates can be queried, `EUR/USD`, `GBP/USD`, `EUR/GBP` and the three inverse rates.
 *
 * ===What the two fields mean===
 *
 * The currencies are held in a `Vector` and their positions in it are their positions in the
 * matrix: the currency at position `i` of [[currencies]] is the currency of row `i` and column
 * `i` of [[rates]]. The implementation being ported kept an insertion-ordered map of currency to
 * index for the same purpose, and its own documentation records why the order is part of the
 * value rather than an implementation detail - it is the order [[toString]] writes. Holding the
 * currencies in a vector makes the position the index, so no map of indices is part of the value
 * and the order cannot disagree with it.
 *
 * If currencies `c1` and `c2` occupy positions `i` and `j`, then `rates.get(i, j)` is such that
 * one unit of `c1` is worth that many units of `c2`. So with `EUR` at position 0 and `USD` at
 * position 1, `rates.get(0, 1)` is around 1.40 and `rates.get(1, 0)` around 0.7142; the second is
 * computed from the first when the matrix is built, and every element of the matrix is meaningful.
 *
 * ===Construction, and where the builder went===
 *
 * The implementation being ported built a matrix through a mutable builder that held a growing
 * map of currencies, a resizable array of rates, and a set of ''pending'' rates - rates offered
 * for two currencies neither of which was in the matrix yet, which could not be placed until some
 * later rate connected one of them. The builder is not ported: this type is built by
 * [[FxMatrix.of]], [[FxMatrix.ofRates]] and [[withRate]], each of which answers with a new matrix
 * and never mutates one.
 *
 * The pending rates survive that change, because they are behaviour rather than state: a caller
 * that offers a whole collection of rates may legitimately offer them in an order in which some
 * rate arrives before the rate that connects it. That tolerance now lives inside
 * [[FxMatrix.ofRates]] and [[withRates]], as a local part of their fold, and is no longer
 * something a half-built value carries: a single rate offered through [[withRate]] to a matrix it
 * has no currency in common with is a failure at that point, because there is no later offer for
 * it to wait for. What was an exception from the builder's `build` is now the failure those folds
 * report for a rate they could never place.
 *
 * ===Rates are taken as they are given===
 *
 * No factory here judges a rate. The builder being ported validated nothing about the numbers it
 * was given, and neither does this: a rate of zero is accepted and the reciprocal recorded for it
 * is infinite, which is a matrix the captured baseline of this port deliberately contains. The
 * checks this type does make, and makes only where a matrix arrives from outside the library
 * through [[FxMatrix.fromMatrix]], are structural - the currencies are distinct, the matrix is
 * square and of their number, and its diagonal is one.
 *
 * ===Equality===
 *
 * Two matrices are equal when they hold the same currencies ''in the same order'' and rates that
 * agree bit for bit. The order is part of the value, so two matrices holding the same rates for
 * the same currencies in a different order are not equal, exactly as they were not for the
 * implementation being ported, whose equality read an ordered map. Rates are compared through
 * [[com.opengamma.strata.collect.array.DoubleMatrix]], whose equality is by bit pattern, so a
 * rate that is not a number equals itself and the two zeroes are distinct.
 *
 * This class is immutable and thread-safe.
 *
 * @param currencies  the currencies of this matrix, in the order that is their position in it
 * @param rates  the matrix of rates, square and of the number of currencies, with a unit diagonal
 */
sealed abstract case class FxMatrix private (currencies: Vector[Currency], rates: DoubleMatrix)
    extends FxRateProvider {

  /**
   * The position of each currency within this matrix.
   *
   * This is derived from [[currencies]] - the position of a currency in the vector is its index -
   * and is held so that a lookup costs the same whatever the matrix holds, which matters because
   * a rate query performs two of them and a conversion of a multi-currency amount performs two
   * per amount. Being derived, it is not part of the value: it takes no part in equality, in
   * hashing or in the JSON form, and it is built on first use rather than on construction so that
   * a matrix built as one step of a fold does not pay for a lookup table no caller will read.
   */
  private lazy val indexByCurrency: Map[Currency, Int] = currencies.zipWithIndex.toMap

  //-------------------------------------------------------------------------
  /**
   * Returns the set of currencies held within this matrix.
   *
   * The set iterates in the order the currencies occupy in this matrix, which is the order
   * [[currencies]] holds them in and the order the implementation being ported iterated its own
   * insertion-ordered set in. It is therefore an insertion-ordered `ListSet` rather than a hashed
   * set, so the order is a property of the returned value and not an accident of how few elements
   * it holds. Where the order is what a caller needs, [[currencies]] states it in a type that
   * cannot lose it.
   *
   * @return the currencies in this matrix, iterating in matrix order
   */
  def getCurrencies: Set[Currency] = ListSet.from(currencies)

  //-------------------------------------------------------------------------
  /**
   * Gets the FX rate for the specified currency pair.
   *
   * The rate returned is the rate from the base currency to the counter currency as defined by
   * this formula: `(1 * baseCurrency = fxRate * counterCurrency)`.
   *
   * Two identical currencies convert at one whether or not this matrix mentions them, and that
   * case is answered before the matrix is consulted, which is what lets the empty matrix answer a
   * rate at all. Any other pair is read from the matrix when it holds both currencies, and is a
   * [[com.opengamma.strata.collect.result.Failure.CurrencyConversion]] naming the pair and the
   * currencies this matrix does hold when it does not. The implementation being ported raised an
   * exception with that same wording.
   *
   * {{{
   * val matrix = FxMatrix.of(Currency.GBP, Currency.USD, 1.6d)
   * matrix.fxRate(Currency.GBP, Currency.USD)   // Right(1.6)
   * matrix.fxRate(Currency.USD, Currency.GBP)   // Right(0.625)
   * matrix.fxRate(Currency.EUR, Currency.EUR)   // Right(1.0) - not held, still answered
   * matrix.fxRate(Currency.USD, Currency.EUR)   // Left(Failure.CurrencyConversion(…))
   * }}}
   *
   * @param baseCurrency  the base currency, to convert from
   * @param counterCurrency  the counter currency, to convert to
   * @return the FX rate for the currency pair, or the failure naming the pair this matrix cannot
   *   convert
   */
  override def fxRate(baseCurrency: Currency, counterCurrency: Currency): FailureOr[Double] =
    if (baseCurrency == counterCurrency) {
      Right(1d)
    } else {
      (indexOf(baseCurrency), indexOf(counterCurrency)) match {
        case (Some(baseIndex), Some(counterIndex)) => Right(rates.get(baseIndex, counterIndex))
        case _ => Left(FxMatrix.noRateFound(baseCurrency, counterCurrency, currencies))
      }
    }

  /**
   * Converts an amount into an amount in the specified currency using the rates in this matrix.
   *
   * An amount already in the requested currency is returned unchanged, and the matrix is not
   * consulted for it. Otherwise the amount is multiplied by the rate from its own currency to the
   * requested one, so the conversion carries the failure of that lookup when this matrix holds no
   * such rate.
   *
   * The product is handed to [[CurrencyAmount.of]] rather than assembled directly, which is what
   * the implementation being ported did and is why a product that is not a number - reachable
   * only by multiplying a zero rate by an infinite amount, or the reverse - is reported rather
   * than carried.
   *
   * @param amount  the amount to be converted
   * @param targetCurrency  the currency to convert the amount to
   * @return the amount converted to the requested currency, or the failure describing why it
   *   could not be
   */
  def convert(amount: CurrencyAmount, targetCurrency: Currency): FailureOr[CurrencyAmount] =
    if (amount.currency == targetCurrency) {
      Right(amount)
    } else {
      convert(amount.amount, amount.currency, targetCurrency)
        .flatMap(converted => CurrencyAmount.of(targetCurrency, converted))
    }

  /**
   * Converts a multi-currency amount into an amount in the specified currency using the rates in
   * this matrix.
   *
   * Every amount held is converted into the requested currency and the results are totalled, so
   * the answer is a single amount. An amount already in the requested currency needs no rate,
   * because [[fxRate]] answers a currency against itself without consulting the matrix; a value
   * holding nothing at all totals to zero of the requested currency, as it did for the
   * implementation being ported, which is why that conversion succeeds even for a currency this
   * matrix does not hold.
   *
   * The conversion is performed on the numbers rather than on amounts, as the implementation
   * being ported performed it - it noted that this avoids building an intermediate amount per
   * currency - and the terms are totalled in the alphabetical order of their currency codes,
   * which is the order the value holds them in and the order that implementation's sorted set
   * yielded them in. Floating-point addition is order-sensitive, so stating the order is what
   * makes this total reproducible and what lets it be compared against a captured baseline.
   *
   * A single unavailable rate fails the whole conversion, carrying the failure the lookup
   * reported: a total assembled from some of the amounts would be a number with no meaning.
   *
   * @param amount  the multi-currency amount to be converted
   * @param targetCurrency  the currency to convert all entries to
   * @return the total amount in the requested currency, or the failure describing why the
   *   conversion could not be performed
   */
  def convert(amount: MultiCurrencyAmount, targetCurrency: Currency): FailureOr[CurrencyAmount] =
    amount.amounts.toList
      .traverse { case (currency, value) => convert(value, currency, targetCurrency) }
      .flatMap(converted =>
        CurrencyAmount.of(targetCurrency, converted.foldLeft(0d)((total, next) => total + next)))

  //-------------------------------------------------------------------------
  /**
   * Returns a matrix with the rate for the specified currency pair added or updated.
   *
   * This is the currency-pair form of the method below, applied to the two currencies of the
   * pair, so the two forms place a rate identically.
   *
   * @param currencyPair  the currency pair the rate is for
   * @param rate  the FX rate between the base currency of the pair and the counter currency. The
   *   rate indicates the value of one unit of the base currency in terms of the counter currency.
   * @return the matrix holding this matrix's rates and that rate, or the failure reported for a
   *   pair this matrix has no currency in common with
   */
  def withRate(currencyPair: CurrencyPair, rate: Double): FailureOr[FxMatrix] =
    withRate(currencyPair.base, currencyPair.counter, rate)

  /**
   * Returns a matrix with the rate for the specified currencies added or updated.
   *
   * An invocation with `matrix.withRate(GBP, USD, 1.6)` states that one pound sterling is worth
   * 1.6 US dollars. It is equivalent to `matrix.withRate(USD, GBP, 1 / 1.6)` in every case except
   * the one where the matrix already holds both currencies, where the two differ - see below.
   *
   * There are four outcomes, which are the four the implementation being ported described:
   *
   *   - '''This matrix is empty.''' The two currencies and the rate become the initial pair, and
   *     the reciprocal rate is recorded with them.
   *   - '''One of the currencies is held and the other is not.''' The new currency is added at the
   *     end, so it takes the last position, and its rate against every currency already held is
   *     computed from the rate given and the rates already held.
   *   - '''Both currencies are held.''' The rate is an update. The first currency is the
   *     ''reference'' and the second is the currency ''restated'' against it: every rate involving
   *     the second currency is recomputed from the new rate and the reference currency's existing
   *     rates, and the rate of the second currency against itself stays one.
   *   - '''Neither currency is held.''' There is no currency in common, so no cross rate can be
   *     computed, and this is a
   *     [[com.opengamma.strata.collect.result.Failure.CurrencyConversion]]. Where a whole
   *     collection of rates is being placed and a later rate would supply the connection, use
   *     [[withRates]] or [[FxMatrix.ofRates]], which tolerate that order.
   *
   * ===The update is deliberately not symmetric===
   *
   * Because one currency of an update is the reference and the other is restated against it, the
   * result of `matrix.withRate(USD, EUR, 1.23)` differs from that of
   * `matrix.withRate(EUR, USD, 1 / 1.23)` whenever the matrix holds a third currency. Both agree
   * about the `USD/EUR` rate itself and disagree about what that rate implies for the rest of the
   * matrix: restating `EUR` against `USD` leaves every rate of `USD` against a third currency
   * alone and changes those of `EUR`, and the other way round. That asymmetry is the behaviour of
   * the implementation being ported, is documented there at length for the same reason, and is
   * reproduced here rather than smoothed away - a symmetric rule would answer differently for
   * every matrix of three or more currencies.
   *
   * @param ccy1  the first currency of the pair, the reference currency of an update
   * @param ccy2  the second currency of the pair, the currency restated by an update
   * @param rate  the FX rate between the first currency and the second currency. The rate
   *   indicates the value of one unit of the first currency in terms of the second currency.
   * @return the matrix holding this matrix's rates and that rate, or the failure reported for a
   *   pair this matrix has no currency in common with
   */
  def withRate(ccy1: Currency, ccy2: Currency, rate: Double): FailureOr[FxMatrix] =
    FxMatrix.placed(FxMatrix.stepped(this, FxMatrix.NoPendingRates, ccy1, ccy2, rate))

  /**
   * Returns a matrix with the rates for the specified currency pairs added or updated.
   *
   * The rates are placed one at a time, in the order they are given, each as [[withRate]] would
   * place it, so a pair this matrix already holds both currencies of is an update and the
   * asymmetry described there applies to it. A `Map` of pair to rate is an `Iterable` of those
   * entries, so one may be passed directly; its iteration order is the order the rates are
   * placed in, which is why an ordered map is what a caller should reach for when the order
   * matters to them.
   *
   * ===Rates offered before the rate that connects them===
   *
   * A rate for two currencies neither of which is held yet cannot be placed at the point it is
   * offered, because no cross rate can be computed for it. Rather than failing there, this holds
   * it back and retries it each time a later rate brings a new currency in, which is what lets a
   * collection be given in any order:
   *
   * {{{
   * FxMatrix.empty.withRates(Vector(
   *   CurrencyPair.of(Currency.GBP, Currency.USD) -> 1.6d,
   *   CurrencyPair.of(Currency.EUR, Currency.CHF) -> 1.2d,   // held back
   *   CurrencyPair.of(Currency.USD, Currency.EUR) -> 0.7d))  // places EUR, then EUR/CHF
   * }}}
   *
   * A rate still held back once every rate has been offered is one that could never be placed,
   * and it is the failure of this method - the state the implementation being ported reported
   * from `build`, with the same wording, listing the rates it could not place.
   *
   * The retry is performed in the same shape that implementation performed it, which is what
   * makes the ''positions'' of the currencies in the result the same: after each rate that brings
   * in a new currency, the rates held back are examined as they stand and every one of them that
   * has become placeable is placed, and that examination is repeated until a pass places nothing.
   * A rate that becomes placeable part way through a pass therefore waits for the next pass. The
   * order the currencies end up in is observable - through [[currencies]], [[toString]], equality
   * and the JSON form - so this is not an internal detail that could be reorganised.
   *
   * @param rateEntries  the currency pairs and rates to place, in the order to place them
   * @return the matrix holding this matrix's rates and those rates, or the failure listing the
   *   rates that could never be placed
   */
  def withRates(rateEntries: Iterable[(CurrencyPair, Double)]): FailureOr[FxMatrix] =
    FxMatrix.placed(
      rateEntries.foldLeft((this, FxMatrix.NoPendingRates)) { case ((matrix, pending), (pair, rate)) =>
        FxMatrix.stepped(matrix, pending, pair.base, pair.counter, rate)
      })

  //-------------------------------------------------------------------------
  /**
   * Merges the entries from the other matrix into this one.
   *
   * The other matrix should have at least one currency in common with this one; the first
   * currency of this matrix that the other also holds is the one the merge works through. The
   * currencies of the other matrix that this one does not hold are added one at a time, in the
   * order the other matrix holds them, each at the rate the other matrix records between it and
   * that common currency. The rates in the result are therefore coherent with the rates already
   * in this matrix.
   *
   * Note that where the other matrix has more than one currency in common with this one and the
   * rates between those shared currencies differ from the rates here, the rates the result holds
   * for the added currencies will differ from the rates the other matrix held for them. That
   * follows from every added rate being derived through one common currency, it is the behaviour
   * of the implementation being ported, and it is why the rates of the other matrix are described
   * as merged into this one rather than combined with it.
   *
   * Two matrices with no currency in common cannot be merged, and that is a
   * [[com.opengamma.strata.collect.result.Failure.CurrencyConversion]] naming both sets of
   * currencies - the state the implementation being ported raised an exception for. Merging a
   * matrix that adds nothing answers with this matrix.
   *
   * @param other  the matrix to be merged into this one
   * @return a new matrix containing the rates from this matrix plus any rates for additional
   *   currencies from the other matrix, or the failure reported for two matrices with no currency
   *   in common
   */
  def merge(other: FxMatrix): FailureOr[FxMatrix] =
    currencies.iterator
      .flatMap(currency => other.indexOf(currency).map(index => (currency, index)))
      .nextOption() match {
      case None => Left(FxMatrix.noCommonCurrency(currencies, other.currencies))
      case Some((commonCurrency, commonIndex)) =>
        // The currencies of the other matrix are walked in its own order and each is tested
        // against the matrix as it stands at that point, which is how the implementation being
        // ported tested them. Each rate is read from the other matrix, between the common
        // currency and the currency being added, and placed here through the ordinary placement
        // of a rate - which for a currency this matrix does not hold yet is the addition of a new
        // currency and cannot fail. Threading the outcome rather than assuming that is what keeps
        // the method total.
        other.currencies.zipWithIndex.foldLeft[FailureOr[FxMatrix]](Right(this)) {
          case (merged, (currency, index)) =>
            merged.flatMap { matrix =>
              if (currency == commonCurrency || matrix.holds(currency)) {
                Right(matrix)
              } else {
                matrix.withRate(commonCurrency, currency, other.rates.get(commonIndex, index))
              }
            }
        }
    }

  //-------------------------------------------------------------------------
  /**
   * Checks whether this matrix equals another object.
   *
   * Another matrix is equal when it holds the same currencies in the same positions and rates
   * that agree bit for bit. The currency order is read because it is part of the value: the
   * implementation being ported compared an ordered map of currency to position, so two matrices
   * that hold the same rates for the same currencies in a different order were not equal there
   * either. The rates are compared by
   * [[com.opengamma.strata.collect.array.DoubleMatrix]] itself, whose comparison is of the bit
   * pattern of each element, so a rate that is not a number equals itself and a zero rate differs
   * from a negative zero rate - the comparison the generated bean performed. An object of any
   * other type is not equal.
   *
   * @param obj  the object to compare to
   * @return true if the other object is a matrix holding the same currencies in the same order
   *   and the same rates
   */
  override def equals(obj: Any): Boolean = obj match {
    case other: FxMatrix =>
      (this eq other) || (currencies == other.currencies && rates == other.rates)
    case _ => false
  }

  /**
   * Returns a hash code consistent with `equals`.
   *
   * The two fields the comparison reads are the two folded in here, and the rates contribute the
   * hash [[com.opengamma.strata.collect.array.DoubleMatrix]] computes from their bit patterns, so
   * two matrices that are equal hash alike including when they hold rates outside the real
   * numbers.
   *
   * @return the hash code of the currencies and rates held
   */
  override def hashCode: Int = 31 * currencies.hashCode + rates.hashCode

  /**
   * Returns a description of this matrix.
   *
   * The form is that of the implementation being ported, down to its two separators, which are
   * not the same one: the currencies are written in matrix order separated by a comma and a
   * space, then a space, a colon and a space, then the rows of the matrix separated by a bare
   * comma, each row rendered as a bracketed list of its elements. The empty matrix therefore
   * renders as `FxMatrix[ : ]`, with both lists empty.
   *
   * {{{
   * FxMatrix.of(Currency.GBP, Currency.USD, 1.6d).toString
   * // FxMatrix[GBP, USD : [1.0, 1.6],[0.625, 1.0]]
   * }}}
   *
   * Each row is rendered by the platform's own rendering of an array of doubles, which is what
   * that implementation used and what fixes the spacing within a row and the way each element is
   * written. The rows are read as independent copies, so nothing of this matrix is exposed by
   * describing it.
   *
   * @return the rendering of this matrix
   */
  override def toString: String =
    "FxMatrix[" + currencies.mkString(", ") + " : " +
      Vector.tabulate(rates.rowCount)(row => Arrays.toString(rates.rowArray(row))).mkString(",") +
      "]"

  //-------------------------------------------------------------------------
  /**
   * The position of a currency within this matrix, if it holds it.
   *
   * This is the lookup every rate query and every placement of a rate is written in terms of, and
   * answering with an option rather than with a position and a separate test is what makes those
   * total: a position obtained here is known to index both the currencies and the rates.
   *
   * @param currency  the currency to locate
   * @return the position of the currency, or nothing if this matrix does not hold it
   */
  private def indexOf(currency: Currency): Option[Int] = indexByCurrency.get(currency)

  /**
   * Whether this matrix holds a currency.
   *
   * @param currency  the currency to test for
   * @return true if this matrix holds a rate for that currency
   */
  private def holds(currency: Currency): Boolean = indexByCurrency.contains(currency)
}

/**
 * The ways of obtaining a matrix of FX rates, the placement of a rate into one, and the typeclass
 * instances and JSON codecs of the type.
 *
 * ===Construction===
 *
 * A matrix is obtained empty, from a single rate, or from a collection of rates:
 *
 *   - [[FxMatrix.empty]] holds no currency and no rate, and still answers a rate of one for a
 *     currency against itself.
 *   - [[FxMatrix.of]] for a pair or two currencies and a rate is the single-rate matrix. Neither
 *     form can fail: an empty matrix has no currency for the rate to be disconnected from, and no
 *     rate is rejected for its value.
 *   - [[FxMatrix.of]] for a collection of [[FxRate]] values, and [[FxMatrix.ofRates]] for a
 *     collection of pairs and rates, place every rate in turn. Both tolerate a rate offered
 *     before the rate that connects it and both report the rates they could never place.
 *   - [[FxMatrix.fromMatrix]] adopts currencies and a matrix of rates that have been stated
 *     elsewhere, which is the route the JSON decoder takes, and is the one factory here that
 *     checks the shape of what it is given.
 *
 * [[FxMatrix.ofRates]] rather than [[FxMatrix.of]] is the factory for a collection whose rates are
 * not all greater than zero: a rate of zero is one a matrix accepts but not one [[FxRate]] admits,
 * so such a collection cannot be expressed as [[FxRate]] values. The two collection factories are
 * otherwise the same factory, the rate form being defined in terms of the pair form.
 *
 * The `Collector` factories of the implementation being ported, which collected a stream of
 * entries or of pairs into a matrix, are not ported: a collection is placed by these factories
 * and a stream by draining it into one. Its mutable builder is not ported either, and the mutating
 * calls that returned one are replaced by the methods of [[FxMatrix]] that answer with a new
 * matrix - so `toBuilder` has no counterpart here, and code that reached for it to add rates to an
 * existing matrix reaches for [[FxMatrix.withRate]] or [[FxMatrix.withRates]] instead.
 *
 * ===Where the placement of a rate lives===
 *
 * The four outcomes of offering a rate - the initial pair, a new currency, an update, and a rate
 * with no currency in common - are implemented by the private members below rather than by the
 * public methods, because the collection factories need them in a form that also carries the
 * rates held back. Each of them answers with a new matrix, and the rates held back are an ordinary
 * value threaded through a fold, so nothing of a part-built matrix is observable and no
 * half-constructed value exists to be handed out.
 */
object FxMatrix {

  /**
   * The rates offered to a fold that could not be placed when they were offered, in the order
   * they were offered in.
   *
   * The implementation being ported held these in a hashed map keyed by currency pair, and a rate
   * offered twice for one pair therefore replaced the earlier one. That is reproduced here -
   * [[parked]] replaces the rate of a pair already held back and keeps its position - with the
   * one difference that the order here is the order the rates were offered in rather than the
   * order of a hash table, which is what makes a fold over a collection of rates reproducible.
   */
  private type PendingRates = Vector[(CurrencyPair, Double)]

  /** No rates held back, the state every fold starts from. */
  private val NoPendingRates: PendingRates = Vector.empty

  /**
   * The greatest number of rates held back that the failure reporting them lists individually.
   *
   * How many rates a caller offers is the caller's choice, so the number that could never be
   * placed is too, and a message that listed all of them could be made large by offering a large
   * collection. The listing is therefore bounded and says how many rates it left out. The
   * currencies of a matrix need no such bound, being drawn from the closed family of currencies
   * this port holds.
   */
  private val MaxListedRates: Int = 8

  //-------------------------------------------------------------------------
  /**
   * An empty FX matrix containing neither currencies nor rates.
   *
   * The matrix holds no currency and its rates are the empty matrix, so it answers a rate of one
   * for any currency against itself, fails for every other pair, and totals a multi-currency
   * amount holding nothing to zero of any currency. It is the matrix every fold over a collection
   * of rates starts from.
   *
   * @return an empty matrix
   */
  val empty: FxMatrix = create(Vector.empty, DoubleMatrix.EMPTY)

  /**
   * Obtains an instance containing a single FX rate.
   *
   * An invocation with `FxMatrix.of(CurrencyPair.of(GBP, USD), 1.6)` states that one pound
   * sterling is worth 1.6 US dollars. The matrix can also be queried for the reverse rate, from
   * `USD` to `GBP`, which is recorded as the reciprocal of the rate given.
   *
   * @param currencyPair  the currency pair to be added
   * @param rate  the FX rate between the base currency of the pair and the counter currency. The
   *   rate indicates the value of one unit of the base currency in terms of the counter currency.
   * @return a matrix containing the single FX rate
   */
  def of(currencyPair: CurrencyPair, rate: Double): FxMatrix =
    of(currencyPair.base, currencyPair.counter, rate)

  /**
   * Obtains an instance containing a single FX rate.
   *
   * An invocation with `FxMatrix.of(GBP, USD, 1.6)` states that one pound sterling is worth 1.6 US
   * dollars. The matrix can also be queried for the reverse rate, from `USD` to `GBP`.
   *
   * The two currencies take positions zero and one in that order, so the order they are given in
   * is observable in the result. A rate given for one currency against itself yields a matrix of
   * that one currency at a rate of one, the rate given being unobservable - which is what the
   * implementation being ported produced for the same arguments, since its matrix was trimmed to
   * the number of distinct currencies it held.
   *
   * @param ccy1  the first currency of the pair
   * @param ccy2  the second currency of the pair
   * @param rate  the FX rate between the first currency and the second currency. The rate
   *   indicates the value of one unit of the first currency in terms of the second currency.
   * @return a matrix containing the single FX rate
   */
  def of(ccy1: Currency, ccy2: Currency, rate: Double): FxMatrix = addInitial(ccy1, ccy2, rate)

  /**
   * Obtains an instance containing the specified FX rates.
   *
   * The rates are placed one at a time, in the order the collection yields them, exactly as
   * [[FxMatrix.withRates]] places them - including its tolerance of a rate offered before the
   * rate that connects it, and including the treatment of a second rate for a pair already placed
   * as an update to it.
   *
   * {{{
   * FxMatrix.of(Vector(gbpUsd, eurUsd, eurChf))  // Right(FxMatrix[GBP, USD, EUR, CHF : …])
   * }}}
   *
   * A rate of zero is a rate a matrix accepts and a rate [[FxRate]] rejects, so a collection that
   * holds one cannot be expressed here; [[FxMatrix.ofRates]] takes pairs and rates directly and
   * places such a collection.
   *
   * @param fxRates  the rates to place, in the order to place them
   * @return the matrix holding those rates, or the failure listing the rates that could never be
   *   placed
   */
  def of(fxRates: Iterable[FxRate]): FailureOr[FxMatrix] =
    ofRates(fxRates.map(rate => (rate.pair, rate.rate)))

  /**
   * Obtains an instance containing the specified rates, stated as currency pairs and rates.
   *
   * This is [[FxMatrix.of]] for a collection whose rates are numbers rather than [[FxRate]]
   * values, and it is the factory that corresponds to the `addRates` method of the builder being
   * ported: every rate is taken as it is given, a rate of zero included. A `Map` of pair to rate
   * is an `Iterable` of those entries and may be passed directly, its iteration order being the
   * order the rates are placed in.
   *
   * {{{
   * FxMatrix.ofRates(Vector(
   *   CurrencyPair.of(Currency.GBP, Currency.USD) -> 1.6d,
   *   CurrencyPair.of(Currency.JPY, Currency.CAD) -> 0.0d,     // held back, and legal
   *   CurrencyPair.of(Currency.JPY, Currency.USD) -> 0.008d))  // places JPY, then JPY/CAD
   * }}}
   *
   * @param rateEntries  the currency pairs and rates to place, in the order to place them
   * @return the matrix holding those rates, or the failure listing the rates that could never be
   *   placed
   */
  def ofRates(rateEntries: Iterable[(CurrencyPair, Double)]): FailureOr[FxMatrix] =
    empty.withRates(rateEntries)

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance from currencies and a matrix of the rates between them.
   *
   * This adopts a matrix that has been stated elsewhere rather than computing one, and it is the
   * route a matrix takes when it is decoded from a document. The currency at each position of the
   * vector is the currency of that row and column of the matrix, so the order of the vector is
   * the order the result holds its currencies in.
   *
   * Three things are checked, and they accumulate, so a caller is told everything that is wrong
   * with what they supplied rather than the first thing:
   *
   *   - the currencies are distinct, since a currency at two positions would make the rate
   *     between a pair ambiguous;
   *   - the matrix is square and holds one row and column per currency;
   *   - every rate of a currency against itself, which is the diagonal, is one.
   *
   * Nothing else is checked. In particular a rate is not required to be the reciprocal of the
   * rate in the opposite direction, and three rates are not required to triangulate, because the
   * builder being ported accepted whatever rates it was given and a matrix built by placing rates
   * into it can hold rates that satisfy neither property. A matrix this method rejected for such
   * a reason would therefore be a matrix that could be built but not decoded.
   *
   * @param currencies  the currencies, in the order they occupy in the matrix
   * @param rates  the matrix of rates, in the orientation described on [[FxMatrix]]
   * @return the matrix, or every reason the currencies and rates do not describe one
   */
  def fromMatrix(currencies: Vector[Currency], rates: DoubleMatrix): ResultNec[FxMatrix] =
    (checkedDistinct(currencies), checkedShape(currencies.size, rates), checkedDiagonal(rates))
      .mapN((_, _, _) => create(currencies, rates))
      .toEither

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance from currencies and rates that are known to describe one.
   *
   * This is the one place a matrix is instantiated, so every factory and every placement of a
   * rate arrives here. The shape of the rates is checked against the currencies, which is a
   * caller-contract check rather than a decision about data - the arguments come from this file
   * in every case, since the constructor of the type is not public - and it is stated because a
   * matrix whose rates did not match its currencies would answer a rate for a position that does
   * not exist. The two remaining invariants, distinct currencies and a unit diagonal, hold of
   * every matrix these private members build and are checked by [[fromMatrix]], which is where a
   * matrix stated outside the library arrives.
   *
   * @param currencies  the currencies, in the order they occupy in the matrix
   * @param rates  the matrix of rates
   * @return the matrix
   * @throws java.lang.IllegalArgumentException if the rates are not a square matrix of the number
   *   of currencies
   */
  private def create(currencies: Vector[Currency], rates: DoubleMatrix): FxMatrix = {
    ArgCheck.isTrue(
      rates.isSquare && rates.rowCount == currencies.size,
      s"An FX matrix of ${currencies.size} currencies requires a square matrix of that many " +
        s"rates, but the matrix given is ${rates.rowCount} by ${rates.columnCount}")
    new FxMatrix(currencies, rates) {}
  }

  //-------------------------------------------------------------------------
  /**
   * Places a rate into an empty matrix, as the initial pair of currencies.
   *
   * The two currencies take positions zero and one, their rate is recorded at the first position
   * of the second, its reciprocal at the second position of the first, and both rates of a
   * currency against itself are one. This is the body of the corresponding step of the builder
   * being ported, in the same order, so the four elements are the four numbers it stored.
   *
   * Two identical currencies describe one currency rather than a pair, and the rate given is then
   * unobservable: the result is the one-currency matrix holding a rate of one. The implementation
   * being ported reached the same result by a different route, storing the rate and then trimming
   * its matrix to the single currency its map held.
   *
   * @param ccy1  the first currency of the pair
   * @param ccy2  the second currency of the pair
   * @param rate  the rate of the first currency in terms of the second
   * @return the matrix of those two currencies
   */
  private def addInitial(ccy1: Currency, ccy2: Currency, rate: Double): FxMatrix =
    if (ccy1 == ccy2) {
      create(Vector(ccy1), DoubleMatrix.of(1, 1, 1d))
    } else {
      create(Vector(ccy1, ccy2), DoubleMatrix.of(2, 2, 1d, rate, 1d / rate, 1d))
    }

  /**
   * Places a rate that brings a new currency into a matrix.
   *
   * The new currency takes the position after the currencies already held, and its rate against
   * each of them is computed from the rate given and the rate that currency already has against
   * the reference currency - the currency of the offered pair the matrix already held. The rate
   * given is stated in the direction from the reference currency to the new one, which is why the
   * callers that hold the pair the other way round invert it before arriving here rather than
   * this deciding which way round it is: the inversion is one division, and performing it at the
   * call site is what keeps the reference position and the rate consistent with each other.
   *
   * The arithmetic is the arithmetic of the builder being ported, operand for operand - the cross
   * rate is the rate multiplied by the existing rate in that order, and the opposite rate is one
   * divided by that same product. Floating-point multiplication and division are neither exact
   * nor associative, so a rearranged expression would agree with the original only to within
   * rounding, and the captured baseline of this port records the exact value.
   *
   * @param matrix  the matrix to place the rate into, which holds the reference currency
   * @param indexRef  the position of the reference currency within that matrix
   * @param other  the currency to add, which the matrix does not hold
   * @param updatedRate  the rate of the reference currency in terms of the currency to add
   * @return the matrix holding the additional currency and every rate for it
   */
  private def addNew(matrix: FxMatrix, indexRef: Int, other: Currency, updatedRate: Double): FxMatrix = {
    val previous = matrix.rates
    val indexOther = matrix.currencies.size
    val size = indexOther + 1
    create(
      matrix.currencies :+ other,
      DoubleMatrix.tabulate(size, size) { (row, column) =>
        if (row == indexOther) {
          if (column == indexOther) 1d else 1d / (updatedRate * previous.get(column, indexRef))
        } else if (column == indexOther) {
          updatedRate * previous.get(row, indexRef)
        } else {
          previous.get(row, column)
        }
      })
  }

  /**
   * Places a rate for two currencies a matrix already holds, restating the second against the
   * first.
   *
   * The currency at the first position is the reference and the currency at the second is
   * restated: every rate involving the restated currency is recomputed from the rate given and
   * the reference currency's existing rates, and the rate of the restated currency against itself
   * is left at one - the element the loop of the builder being ported skipped, for the same
   * reason.
   *
   * Every rate read here is read from the matrix as it stood before the update. The builder being
   * ported wrote its rates in place while it read them, which reaches the same numbers because no
   * element it wrote was read again afterwards, and stating the two matrices separately is what
   * makes that independence evident rather than incidental.
   *
   * This is the asymmetric operation documented on [[FxMatrix.withRate]]: which of the two
   * currencies is the reference decides which rates of the rest of the matrix move.
   *
   * @param matrix  the matrix to place the rate into, which holds both currencies
   * @param index1  the position of the reference currency
   * @param index2  the position of the currency being restated
   * @param rate  the rate of the reference currency in terms of the restated currency
   * @return the matrix holding the restated rates
   */
  private def updated(matrix: FxMatrix, index1: Int, index2: Int, rate: Double): FxMatrix = {
    val previous = matrix.rates
    val size = matrix.currencies.size
    create(
      matrix.currencies,
      DoubleMatrix.tabulate(size, size) { (row, column) =>
        if (row == index2) {
          if (column == index2) previous.get(row, column) else 1d / (rate * previous.get(column, index1))
        } else if (column == index2) {
          rate * previous.get(row, index1)
        } else {
          previous.get(row, column)
        }
      })
  }

  //-------------------------------------------------------------------------
  /**
   * Offers one rate to a matrix, holding it back if it cannot be placed yet.
   *
   * This is the dispatch of the four outcomes described on [[FxMatrix.withRate]], and it is the
   * single step every factory and every placement of a rate is built from. It does not retry the
   * rates already held back; [[retried]] does that, and [[stepped]] is the two together.
   *
   * @param matrix  the matrix to offer the rate to
   * @param pending  the rates already held back
   * @param ccy1  the first currency of the pair, the reference currency of an update
   * @param ccy2  the second currency of the pair, the currency restated by an update
   * @param rate  the rate of the first currency in terms of the second
   * @return the matrix after the offer, and the rates held back after it
   */
  private def offered(
      matrix: FxMatrix,
      pending: PendingRates,
      ccy1: Currency,
      ccy2: Currency,
      rate: Double): (FxMatrix, PendingRates) =
    if (matrix.currencies.isEmpty) {
      (addInitial(ccy1, ccy2, rate), pending)
    } else {
      (matrix.indexOf(ccy1), matrix.indexOf(ccy2)) match {
        case (Some(index1), Some(index2)) => (updated(matrix, index1, index2, rate), pending)
        case (Some(index1), None) => (addNew(matrix, index1, ccy2, rate), pending)
        case (None, Some(index2)) => (addNew(matrix, index2, ccy1, 1d / rate), pending)
        case (None, None) => (matrix, parked(pending, CurrencyPair.of(ccy1, ccy2), rate))
      }
    }

  /**
   * Places every rate held back that the matrix has since come to reach, repeatedly, until a pass
   * places nothing.
   *
   * The shape of this is the shape of the corresponding loop of the builder being ported, and the
   * shape is what makes the positions of the currencies in the result the same: the rates held
   * back are examined against the matrix as it stands at the start of a pass, every one of them
   * that can be placed is then placed in the order they were offered in, and the examination is
   * repeated. A rate that becomes placeable part way through a pass is therefore placed by the
   * next pass rather than immediately, which is the difference between this and a depth-first
   * placement and would reorder the currencies of a matrix built from rates that connect in a
   * chain.
   *
   * Each rate of a pass is placed through [[offered]] rather than directly as the addition of a
   * new currency, which differs from that implementation in one state it could reach: where two
   * rates held back name the same new currency - against two different currencies already held,
   * or as a pair and its mirror image - the second of them finds both of its currencies present
   * by the time it is placed. That implementation added the currency a second time in that state,
   * recording a position outside the matrix it went on to build, so every rate query for that
   * currency then failed on the index; here the second rate is an update to the first, which is
   * what the same rate offered directly would have been.
   *
   * The recursion ends because every pass that places anything places at least one rate held
   * back, and a rate a pass places cannot be held back again - a rate is only placed when the
   * matrix holds one of its currencies, which is exactly when it is not held back.
   *
   * @param matrix  the matrix to place the rates into
   * @param pending  the rates held back
   * @return the matrix after every placeable rate has been placed, and the rates still held back
   */
  @tailrec
  private def retried(matrix: FxMatrix, pending: PendingRates): (FxMatrix, PendingRates) = {
    val (placeable, blocked) = pending.partition { case (pair, _) =>
      matrix.holds(pair.base) || matrix.holds(pair.counter)
    }
    if (placeable.isEmpty) {
      (matrix, blocked)
    } else {
      val (advanced, stillPending) = placeable.foldLeft((matrix, blocked)) {
        case ((offeredTo, held), (pair, rate)) => offered(offeredTo, held, pair.base, pair.counter, rate)
      }
      retried(advanced, stillPending)
    }
  }

  /**
   * Offers one rate to a matrix and then places every rate held back that has become placeable.
   *
   * This is the whole of placing a rate, and it is what the corresponding method of the builder
   * being ported did: the rate is offered, and the rates held back are retried afterwards. That
   * implementation retried them only after a rate that brought in a new currency, which is the
   * only case that can make one placeable; retrying after every offer reaches the same state,
   * because a pass over rates held back against an unchanged set of currencies places nothing,
   * and it keeps this member to one description of what happens.
   *
   * @param matrix  the matrix to offer the rate to
   * @param pending  the rates already held back
   * @param ccy1  the first currency of the pair, the reference currency of an update
   * @param ccy2  the second currency of the pair, the currency restated by an update
   * @param rate  the rate of the first currency in terms of the second
   * @return the matrix after the offer and the retry, and the rates still held back
   */
  private def stepped(
      matrix: FxMatrix,
      pending: PendingRates,
      ccy1: Currency,
      ccy2: Currency,
      rate: Double): (FxMatrix, PendingRates) = {
    val (advanced, offeredPending) = offered(matrix, pending, ccy1, ccy2, rate)
    retried(advanced, offeredPending)
  }

  /**
   * Holds a rate back, to be retried when a later rate connects it.
   *
   * A rate offered for a pair already held back replaces it and keeps its position, which is what
   * the hashed map of the implementation being ported did for a repeated key.
   *
   * @param pending  the rates already held back
   * @param pair  the currency pair of the rate to hold back
   * @param rate  the rate of the base currency of that pair in terms of its counter currency
   * @return the rates held back, including this one
   */
  private def parked(pending: PendingRates, pair: CurrencyPair, rate: Double): PendingRates = {
    val at = pending.indexWhere { case (parkedPair, _) => parkedPair == pair }
    if (at < 0) pending :+ (pair -> rate) else pending.updated(at, pair -> rate)
  }

  /**
   * Answers with the matrix a fold reached, or with the failure listing the rates it could never
   * place.
   *
   * This is the point the state the implementation being ported reported from `build` is decided:
   * a rate still held back once every rate has been offered is a rate that has no currency in
   * common with the matrix and never will, so the fold has failed rather than partly succeeded.
   *
   * @param outcome  the matrix a fold reached and the rates it still holds back
   * @return the matrix, or the failure listing the rates that could never be placed
   */
  private def placed(outcome: (FxMatrix, PendingRates)): FailureOr[FxMatrix] = {
    val (matrix, pending) = outcome
    if (pending.isEmpty) Right(matrix) else Left(unplaceableRates(pending))
  }

  //-------------------------------------------------------------------------
  /**
   * Checks that the currencies of a matrix are distinct.
   *
   * @param currencies  the currencies to check
   * @return a passing outcome, or the failure naming the first currency that repeats
   */
  private def checkedDistinct(currencies: Vector[Currency]): ValidatedFailures[Unit] =
    currencies.diff(currencies.distinct).headOption match {
      case None => Validate.valid(())
      case Some(currency) =>
        Validate.invalid(
          Failure.Invalid(
            s"Expected the currencies of an FX matrix to be distinct, but $currency appears " +
              s"more than once in ${renderCurrencies(currencies)}"))
    }

  /**
   * Checks that the rates of a matrix are square and hold one row and column per currency.
   *
   * @param count  the number of currencies
   * @param rates  the matrix of rates to check
   * @return a passing outcome, or the failure describing the shape that was given
   */
  private def checkedShape(count: Int, rates: DoubleMatrix): ValidatedFailures[Unit] =
    Validate.isTrue(
      rates.isSquare && rates.rowCount == count,
      s"Expected the rates of an FX matrix of $count currencies to be a $count by $count " +
        s"matrix, but the matrix given is ${rates.rowCount} by ${rates.columnCount}")

  /**
   * Checks that the rate of every currency of a matrix against itself is one.
   *
   * The diagonal is walked only as far as both dimensions of the matrix reach, so this is
   * answerable for a matrix of any shape and accumulates alongside the check of that shape rather
   * than depending on it having passed.
   *
   * @param rates  the matrix of rates to check
   * @return a passing outcome, or the failure naming the first position of the diagonal that does
   *   not hold one
   */
  private def checkedDiagonal(rates: DoubleMatrix): ValidatedFailures[Unit] = {
    val reach = math.min(rates.rowCount, rates.columnCount)

    @tailrec
    def firstNonUnit(index: Int): Option[Int] =
      if (index >= reach) {
        None
      } else if (rates.get(index, index) != 1d) {
        Some(index)
      } else {
        firstNonUnit(index + 1)
      }

    firstNonUnit(0) match {
      case None => Validate.valid(())
      case Some(index) =>
        Validate.invalid(
          Failure.Invalid(
            s"Expected the rate of every currency of an FX matrix against itself to be one, but " +
              s"the rate at position $index is ${rates.get(index, index)}"))
    }
  }

  //-------------------------------------------------------------------------
  /**
   * The failure reported for a pair no rate of this matrix can convert.
   *
   * The wording is the wording the implementation being ported raised, naming the pair asked for
   * and the currencies the matrix holds, so a caller can see at once whether the pair is outside
   * the matrix or the matrix is not the one they meant to query.
   *
   * @param baseCurrency  the base currency asked for
   * @param counterCurrency  the counter currency asked for
   * @param currencies  the currencies the matrix holds, in matrix order
   * @return the failure describing the absent rate
   */
  private def noRateFound(
      baseCurrency: Currency,
      counterCurrency: Currency,
      currencies: Vector[Currency]): Failure =
    Failure.CurrencyConversion(
      s"No FX rate found for $baseCurrency/$counterCurrency, matrix only contains rates for " +
        renderCurrencies(currencies))

  /**
   * The failure reported for two matrices that cannot be merged.
   *
   * @param currencies  the currencies of the matrix being merged into, in matrix order
   * @param otherCurrencies  the currencies of the matrix being merged in, in matrix order
   * @return the failure describing the two sets of currencies
   */
  private def noCommonCurrency(
      currencies: Vector[Currency],
      otherCurrencies: Vector[Currency]): Failure =
    Failure.CurrencyConversion(
      s"There are no currencies in common between ${renderCurrencies(currencies)} and " +
        renderCurrencies(otherCurrencies))

  /**
   * The failure reported for rates a fold could never place.
   *
   * The wording is the wording the implementation being ported raised from `build`, and the rates
   * are rendered as it rendered its map of them, except that the listing is bounded as described
   * on [[MaxListedRates]].
   *
   * @param pending  the rates that could never be placed
   * @return the failure listing those rates
   */
  private def unplaceableRates(pending: PendingRates): Failure =
    Failure.CurrencyConversion(
      s"Received rates with no currencies in common with other: ${renderRates(pending)}")

  /**
   * Renders the currencies of a matrix, in matrix order.
   *
   * The form is the form the implementation being ported wrote a set of currencies in, a
   * bracketed list separated by a comma and a space. The listing is unbounded because the number
   * of currencies is: they are drawn from the closed family of currencies this port holds, so the
   * longest list any matrix can produce is that family.
   *
   * @param currencies  the currencies to render
   * @return the rendering of those currencies
   */
  private def renderCurrencies(currencies: Vector[Currency]): String =
    currencies.mkString("[", ", ", "]")

  /**
   * Renders rates held back, bounded in length.
   *
   * The form is the form the implementation being ported wrote its map of them in, a braced list
   * of pair and rate separated by an equals sign, which for the single rate that is the common
   * case is identical to what it wrote. Beyond [[MaxListedRates]] rates the listing states how
   * many it left out rather than growing with the collection it was given.
   *
   * @param pending  the rates to render
   * @return the bounded rendering of those rates
   */
  private def renderRates(pending: PendingRates): String = {
    val listed = pending.take(MaxListedRates).map { case (pair, rate) => s"$pair=$rate" }
    val omitted = pending.size - listed.size
    val marker = if (omitted > 0) s", and $omitted more" else ""
    listed.mkString("{", ", ", marker + "}")
  }

  //-------------------------------------------------------------------------
  /**
   * The hashing and equality of matrices.
   *
   * This is the only equality-bearing instance of the type, and it is a `Hash` rather than an
   * `Order`: the implementation being ported did not order matrices, and no ordering of them
   * would be meaningful. `Hash` extends `Eq`, so a separate `Eq` would be a second answer to the
   * same question; one is declared here and `Eq` is obtained from it by subtyping.
   *
   * The instance defers to [[FxMatrix.equals]] and [[FxMatrix.hashCode]], which compare the
   * currencies in order and the rates by bit pattern, so `eqv` agrees with `equals` for every
   * matrix including one holding rates outside the real numbers.
   *
   * @return the hashing and equality of matrices
   */
  implicit val hash: Hash[FxMatrix] = Hash.fromUniversalHashCode[FxMatrix]

  /**
   * The rendering of matrices.
   *
   * The rendering is [[FxMatrix.toString]], which is the form the implementation being ported
   * wrote, so the two agree exactly.
   *
   * @return the rendering of matrices
   */
  implicit val show: Show[FxMatrix] = Show.show(_.toString)

  //-------------------------------------------------------------------------
  // The rates of the matrix go through the single policy this port has for a double, which writes
  // the values JSON cannot express as tagged strings. That is what carries a rate of zero and the
  // infinite reciprocal recorded for it through a round trip, and the import is what makes the
  // choice deliberate and local, as the codec support of `strata-collect` intends.
  import Codecs.implicits._

  /**
   * The field shape both codecs are derived from, which the decoder reads before validation.
   *
   * Decoding a matrix is two steps: read the fields, then hand them to the factory that decides
   * whether they describe a matrix. This product is the first step, and encoding is the same two
   * steps in reverse, so both codecs below derive from this one declaration and the JSON shape of
   * a matrix is stated exactly once. It is private and never returned - the only values of it
   * that exist are the ones the two codecs build. Its field names are the JSON keys and they are
   * the names of the two fields of [[FxMatrix]] itself.
   *
   * @param currencies  the currencies, in the order they occupy in the matrix
   * @param rates  the matrix of rates, as an array of rows of tagged doubles
   */
  private final case class Raw(currencies: Vector[Currency], rates: DoubleMatrix)

  /** The derived decoder of the raw field shape, used by the validating decoder below. */
  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /** The derived encoder of the raw field shape, used by the encoder below. */
  private val rawEncoder: Encoder.AsObject[Raw] = deriveEncoder[Raw]

  /**
   * The JSON encoding of matrices.
   *
   * A matrix is an object of two fields, the currencies as their text forms in matrix order and
   * the rates as an array of rows:
   *
   * {{{
   * {"currencies":["GBP","USD"],"rates":[[1.0,1.6],[0.625,1.0]]}
   * }}}
   *
   * The order of the currencies is written because it is part of the value: two matrices holding
   * the same rates for the same currencies in a different order are different matrices and encode
   * to different documents. Every rate is written through the tagged-double policy of this port,
   * so a rate of zero and the infinite reciprocal recorded for it both survive a round trip.
   *
   * Both halves of the codec are assembled by the same compile-time derivation over the same raw
   * shape, which is what keeps them from drifting apart: the field names, their order and their
   * element codecs are stated once, in [[Raw]], and the encoder reaches them by mapping a matrix
   * onto that shape. Deriving from [[FxMatrix]] itself is not possible - the constructor of a
   * validated type is not public, so there is no public shape to derive from.
   *
   * The result is wrapped so that a field holding no value would be omitted, which is the policy
   * every product of this port follows - this type has no optional field, so the wrapping changes
   * nothing about its output and exists so that the policy holds without exception.
   *
   * @return the JSON encoding of a matrix
   */
  implicit val encoder: Encoder[FxMatrix] =
    Codecs.dropNulls(rawEncoder.contramap[FxMatrix](matrix => Raw(matrix.currencies, matrix.rates)))

  /**
   * The JSON decoding of matrices.
   *
   * This is the inverse of the encoding above, and it decides whether the fields describe a
   * matrix exactly as a caller's arguments are decided: the payload is read into the raw shape
   * and handed to [[FxMatrix.fromMatrix]], so a document whose currencies repeat, whose matrix is
   * not square or is not of the number of currencies, or whose diagonal is not one, is a decoding
   * failure carrying every reason rather than a value this type would not have built. A document
   * whose rates are merely unusual - not reciprocal, not triangulating - decodes, because such a
   * matrix can be built by placing rates into one.
   *
   * @return the JSON decoding of a matrix
   */
  implicit val decoder: Decoder[FxMatrix] =
    Codecs.validatedDecoder[Raw, FxMatrix](raw => fromMatrix(raw.currencies, raw.rates))(rawDecoder)
}

