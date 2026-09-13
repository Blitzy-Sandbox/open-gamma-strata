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
import scala.collection.immutable.VectorMap

import cats.Hash
import cats.Show
import cats.syntax.apply._

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.NoJavaSerialization
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
 * `i` of [[rates]]. That order is part of the value rather than an internal detail, because a
 * rate is read by the positions of its two currencies: the same numbers under a different
 * currency order describe different pairs. It is observable four ways - through [[currencies]],
 * through [[toString]], through equality and through the encoded form. Holding the currencies in
 * a vector makes the position the index, so no map of indices is part of the value and the order
 * cannot disagree with it.
 *
 * If currencies `c1` and `c2` occupy positions `i` and `j`, then `rates.get(i, j)` is such that
 * one unit of `c1` is worth that many units of `c2`. So with `EUR` at position 0 and `USD` at
 * position 1, `rates.get(0, 1)` is around 1.40 and `rates.get(1, 0)` around 0.7142; the second is
 * computed from the first when the matrix is built, and every element of the matrix is meaningful.
 *
 * ===Construction===
 *
 * A matrix is built by [[FxMatrix.of]], [[FxMatrix.ofRates]] and [[withRate]], each of which
 * answers with a new matrix and never changes the one it was given.
 *
 * A caller offering a whole collection of rates may offer them in an order in which some rate
 * arrives before the rate that connects it. [[FxMatrix.ofRates]] and [[withRates]] tolerate that:
 * such a rate is held back inside their fold and retried as later rates bring currencies in, and
 * a rate still held back when the collection is exhausted is the failure they report. A single
 * rate offered through [[withRate]] for two currencies the matrix holds neither of fails at that
 * point instead, because there is no later offer for it to wait for.
 *
 * ===Rates are taken as they are given===
 *
 * No factory here judges a rate. A rate of zero is accepted, the reciprocal recorded for it is
 * infinite, and both survive an encode and decode round trip. No rate is required to be the
 * reciprocal of the rate in the opposite direction and no three rates are required to
 * triangulate. The checks this type does make, and makes only where a matrix arrives from outside
 * the library through [[FxMatrix.fromMatrix]], are structural - the currencies are distinct, the
 * matrix is square and of their number, and its diagonal is one.
 *
 * ===Equality===
 *
 * Two matrices are equal when they hold the same currencies ''in the same order'' and rates that
 * agree bit for bit. The order is part of the value, so two matrices holding the same rates for
 * the same currencies in a different order are not equal. Rates are compared through
 * [[com.opengamma.strata.collect.array.DoubleMatrix]], whose equality is by bit pattern rather
 * than numeric, so a rate that is not a number equals itself - a matrix holding such a rate is
 * still equal to itself, and still hashes to one value - and the two zeroes are distinct.
 *
 * This class is immutable and thread-safe.
 *
 * @param currencies  the currencies of this matrix, in the order that is their position in it
 * @param rates  the matrix of rates, square and of the number of currencies, with a unit diagonal
 */
sealed abstract case class FxMatrix private (currencies: Vector[Currency], rates: DoubleMatrix)
    extends FxRateProvider
    with NoJavaSerialization {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means can be
  // stopped is here. The single implementation is the companion's hidden `Impl`.
  JvmClosure.requireSoleImplementation(this, classOf[FxMatrix.Impl])

  // The invariant of this type, stated over the fields the instance actually holds rather than
  // over the arguments a factory was given, because the implementation class carries a public
  // constructor in the class file whatever the source asked for: a class compiled outside this
  // library can call it directly, and identity alone would then admit a matrix whose rates do not
  // describe its currencies at all - a currency at two positions, so that the rate of a pair is
  // ambiguous; rates that do not reach a position a lookup can ask for; or a currency worth
  // something other than itself.
  //
  // The three conditions are those of `FxMatrix.fromMatrix`, which is where a matrix stated
  // outside this library arrives, and they are the whole of what this type checks: as the note
  // above records, no rate is required to be the reciprocal of the rate in the opposite direction
  // and no three rates are required to triangulate, because a matrix built by placing rates into
  // it can hold rates satisfying neither.
  JvmClosure.requireInvariant(
    "its currencies are distinct",
    currencies.distinct.size == currencies.size)
  JvmClosure.requireInvariant(
    "its rates are a square matrix of one row and column per currency",
    rates.isSquare && rates.rowCount == currencies.size)
  JvmClosure.requireInvariant(
    "the rate of every one of its currencies against itself is one",
    // the diagonal is walked only as far as both dimensions reach, so this is answerable for a
    // matrix of any shape rather than depending on the check above having passed
    (0 until math.min(rates.rowCount, rates.columnCount))
      .forall(index => rates.get(index, index) == 1d))

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

  /**
   * The currencies of this matrix as an insertion-ordered set, which is what [[getCurrencies]]
   * answers with.
   *
   * This is derived from [[currencies]] and held for the same reason the lookup above is held: a
   * matrix cannot change, so the set derived from it cannot either, and answering with a set that
   * is already held costs a caller nothing. It is built on first use, so a matrix built as one
   * step of a fold pays nothing for a set no caller reads, and every caller thereafter reads the
   * same value. Being derived, it takes no part in equality, in hashing or in the JSON form -
   * those read the currencies in order, which is the value this set is a projection of.
   */
  private lazy val currencySet: Set[Currency] = ListSet.from(currencies)

  /**
   * Returns the set of currencies held within this matrix.
   *
   * The set iterates in the order the currencies occupy in this matrix, which is the order
   * [[currencies]] holds them in. It is therefore an insertion-ordered `ListSet` rather than a
   * hashed set, so the order is a property of the returned value and not an accident of how few
   * elements it holds. Where the order is what a caller needs, [[currencies]] states it in a type
   * that cannot lose it.
   *
   * The value answered is the one held on this matrix rather than one built per call, so repeated
   * calls answer the same set and reading the currencies of a matrix allocates nothing - see
   * [[currencySet]], which also records that a derived value takes no part in equality, in hashing
   * or in the JSON form.
   *
   * @return the currencies in this matrix, iterating in matrix order
   */
  def getCurrencies: Set[Currency] = currencySet

  /**
   * Gets the FX rate for the specified currency pair.
   *
   * The rate returned is the rate from the base currency to the counter currency as defined by
   * this formula: `(1 * baseCurrency = fxRate * counterCurrency)`.
   *
   * Two identical currencies convert at one whether or not this matrix mentions them, and that
   * case is answered before the matrix is consulted, which is what lets the empty matrix answer a
   * rate at all. Any other pair is read from the matrix when it holds both currencies, and is a
   * failure naming the pair and the currencies this matrix does hold when it does not. A pair
   * this matrix holds is answered by two position lookups and one read of the rates.
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
   * The product is handed to [[CurrencyAmount.of]] rather than assembled directly, which is why
   * a product that is not a number - reachable only by multiplying a zero rate by an infinite
   * amount, or the reverse - is reported rather than carried.
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
   * holding nothing at all totals to zero of the requested currency, which is why that conversion
   * succeeds even for a currency this matrix does not hold.
   *
   * The conversion is performed on the numbers rather than on amounts, which avoids building an
   * intermediate amount per currency, and the terms are totalled in the alphabetical order of
   * their currency codes, which is the order the value holds them in. Floating-point addition is
   * order-sensitive, so stating the order is what makes this total reproducible.
   *
   * The total is accumulated as a number, in one pass over the amounts held: the running total is
   * a parameter of the loop below, so it stays a primitive and neither the amounts converted nor
   * the total are collected into anything. A pass that collected the converted numbers first
   * would box every one of them.
   *
   * A single unavailable rate fails the whole conversion, carrying the failure the lookup
   * reported: a total assembled from some of the amounts would be a number with no meaning. The
   * pass therefore ends at the first amount whose rate is missing and no later amount is looked
   * up, which is the failure a caller is told about and is why offering amounts in several
   * unavailable currencies reports the first of them in currency order.
   *
   * @param amount  the multi-currency amount to be converted
   * @param targetCurrency  the currency to convert all entries to
   * @return the total amount in the requested currency, or the failure describing why the
   *   conversion could not be performed
   */
  def convert(amount: MultiCurrencyAmount, targetCurrency: Currency): FailureOr[CurrencyAmount] = {
    // The amounts are walked through their iterator, which yields them in the currency order the
    // value holds them in, and the total is threaded as a parameter rather than accumulated into
    // a collection. The recursion is in tail position, so it runs as a loop over a primitive.
    @tailrec
    def totalled(entries: Iterator[(Currency, Double)], total: Double): FailureOr[Double] =
      if (entries.hasNext) {
        val (currency, value) = entries.next()
        convert(value, currency, targetCurrency) match {
          case Right(converted) => totalled(entries, total + converted)
          case Left(failure) => Left(failure)
        }
      } else {
        Right(total)
      }

    totalled(amount.amounts.iterator, 0d)
      .flatMap(total => CurrencyAmount.of(targetCurrency, total))
  }

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
   * There are four outcomes:
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
   * alone and changes those of `EUR`, and the other way round. The asymmetry is stated rather
   * than smoothed away, because a symmetric rule would answer differently for every matrix of
   * three or more currencies and a caller placing an update chooses which currency moves by
   * choosing the order of the arguments.
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
   * and it is the failure of this method, which lists the rates it could not place.
   *
   * The retry proceeds in passes, and the shape of those passes is what settles the ''positions''
   * of the currencies in the result: after each rate that brings in a new currency, the rates
   * held back are examined as they stand and every one of them that has become placeable is
   * placed, and that examination is repeated until a pass places nothing. A rate that becomes
   * placeable part way through a pass therefore waits for the next pass. The order the currencies
   * end up in is observable - through [[currencies]], [[toString]], equality and the JSON form -
   * so this is not an internal detail that could be reorganised.
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
   * follows from every added rate being derived through one common currency, and it is why the
   * rates of the other matrix are described as merged into this one rather than combined with it.
   *
   * Two matrices that share no currency cannot be merged, because no cross rate can be computed
   * between them, and that is the one failure of this method; it names both sets of currencies.
   * Merging a matrix that adds nothing answers with this matrix.
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
        // against the matrix as it stands at that point, so a currency added by an earlier step
        // is not added twice. Each rate is read from the other matrix, between the common
        // currency and the currency being added, and placed here through the ordinary placement
        // of a rate - which for a currency this matrix does not hold yet is the addition of a new
        // currency and cannot fail. Threading the outcome through the fold rather than discarding
        // it is what keeps the method total.
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

  /**
   * Checks whether this matrix equals another object.
   *
   * Another matrix is equal when it holds the same currencies in the same positions and rates
   * that agree bit for bit. The currency order is read because it is part of the value: two
   * matrices that hold the same rates for the same currencies in a different order index those
   * rates differently and are not equal. The rates are compared by
   * [[com.opengamma.strata.collect.array.DoubleMatrix]] itself, whose comparison is of the bit
   * pattern of each element rather than numeric, so a rate that is not a number equals itself -
   * which is what makes a matrix holding one equal to itself - and a zero rate differs from a
   * negative zero rate. An object of any other type is not equal.
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
   * The two separators of the form are not the same one: the currencies are written in matrix
   * order separated by a comma and a space, then a space, a colon and a space, then the rows of
   * the matrix separated by a bare comma, each row rendered as a bracketed list of its elements.
   * The empty matrix therefore renders as `FxMatrix[ : ]`, with both lists empty.
   *
   * {{{
   * FxMatrix.of(Currency.GBP, Currency.USD, 1.6d).toString
   * // FxMatrix[GBP, USD : [1.0, 1.6],[0.625, 1.0]]
   * }}}
   *
   * Each row is rendered by the platform's own rendering of an array of doubles, which is what
   * fixes the spacing within a row and the way each element is written. The rows are read as
   * independent copies, so nothing of this matrix is exposed by describing it.
   *
   * @return the rendering of this matrix
   */
  override def toString: String =
    "FxMatrix[" + currencies.mkString(", ") + " : " +
      Vector.tabulate(rates.rowCount)(row => Arrays.toString(rates.rowArray(row))).mkString(",") +
      "]"

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
 * otherwise one factory stated twice: each folds the placement below over its own collection, in
 * the order that collection yields, so they answer with the same matrix for the same pairs and
 * rates. Neither is written in terms of the other, because a collection of [[FxRate]] values
 * projected onto pairs and rates is a second collection of the same length that placing the values
 * as they are read does not need.
 *
 * A lazy sequence of pairs and rates is placed by draining it into one of the collection
 * factories, and rates are added to a matrix that already exists through [[FxMatrix.withRate]]
 * and [[FxMatrix.withRates]], which answer with a new matrix.
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
   * The rates offered to a fold that could not be placed when they were offered, keyed by their
   * currency pair and iterating in the order they were offered in.
   *
   * Two properties are needed of the structure holding them. A rate offered twice for one pair
   * has to replace the rate held for that pair, because a caller may restate a rate that is still
   * held back; and holding one back has to cost a single keyed write, because how many rates are
   * held back is the caller's choice and a scan per offer would make a collection of disconnected
   * rates cost time proportional to the square of its size. `VectorMap` is the structure of the
   * standard library that has both: a write for a key it already holds keeps that key's position,
   * a write for a new key appends, and iteration is in that order. Offer order is what makes a
   * fold over a collection of rates reproducible, and it is the order the failure listing them
   * reads in.
   */
  private type PendingRates = VectorMap[CurrencyPair, Double]

  /** No rates held back, the state every fold starts from. */
  private val NoPendingRates: PendingRates = VectorMap.empty

  /**
   * The greatest number of rates held back that the failure reporting them lists individually.
   *
   * How many rates a caller offers is the caller's choice, so the number that could never be
   * placed is too, and a message that listed all of them could be made large by offering a large
   * collection. The listing is therefore bounded and says how many rates it left out. The
   * currencies of a matrix need no such bound, being drawn from the closed family of currencies
   * [[Currency]] holds.
   */
  private val MaxListedRates: Int = 8

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
   * is observable in the result. A rate given for one currency against itself yields the matrix
   * of that one currency at a rate of one, the rate given being unobservable, because a matrix
   * holds one position per distinct currency.
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
   * The pair and rate each value holds are read as the rate is placed, rather than the collection
   * being projected onto pairs and rates and that projection placed. The two describe the same
   * placement - this is the same fold over the same rates in the same order, so the result is the
   * result [[FxMatrix.ofRates]] answers for the pairs and rates of these values - and reading them
   * one at a time is what keeps a collection of rates from being copied into a second collection
   * of one tuple per rate, each holding a number that would have to be boxed to live in it. The
   * collection is traversed exactly once, so a collection that admits only one traversal is a
   * collection this factory accepts.
   *
   * @param fxRates  the rates to place, in the order to place them
   * @return the matrix holding those rates, or the failure listing the rates that could never be
   *   placed
   */
  def of(fxRates: Iterable[FxRate]): FailureOr[FxMatrix] =
    placed(fxRates.foldLeft((empty, NoPendingRates)) { case ((matrix, pending), fxRate) =>
      stepped(matrix, pending, fxRate.pair.base, fxRate.pair.counter, fxRate.rate)
    })

  /**
   * Obtains an instance containing the specified rates, stated as currency pairs and rates.
   *
   * This is [[FxMatrix.of]] for a collection whose rates are numbers rather than [[FxRate]]
   * values: every rate is taken as it is given, a rate of zero included. A `Map` of pair to rate
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
   * rate in the opposite direction, and three rates are not required to triangulate: placing
   * rates into a matrix accepts whatever rates it is given, a rate of zero included, so a matrix
   * refused for either reason here would be a matrix that could be built but not decoded.
   *
   * @param currencies  the currencies, in the order they occupy in the matrix
   * @param rates  the matrix of rates, in the orientation described on [[FxMatrix]]
   * @return the matrix, or every broken constraint: the currencies must be distinct, the rates
   *   must be a square matrix holding one row and column per currency, and every rate of a
   *   currency against itself must be one
   */
  def fromMatrix(currencies: Vector[Currency], rates: DoubleMatrix): ResultNec[FxMatrix] =
    (checkedDistinct(currencies), checkedShape(currencies.size, rates), checkedDiagonal(rates))
      .mapN((_, _, _) => create(currencies, rates))
      .toEither

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
    new Impl(currencies, rates)
  }

  /**
   * The one implementation of a matrix.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it. It is declared rather than written as an anonymous subclass at the instantiation site
   * for two reasons, both about what the class file says: a private member class is one a Java
   * compiler refuses to name, where an anonymous class is public and can be instantiated directly
   * by a caller in another language, and a named class can be compared against, which is what
   * lets [[FxMatrix]] refuse in its own constructor to be any other implementation.
   *
   * @param currencies  the currencies, in the order they occupy in the matrix
   * @param rates  the matrix of rates, whose shape [[create]] has already checked against the
   *   currencies
   */
  private final class Impl(currencies: Vector[Currency], rates: DoubleMatrix)
      extends FxMatrix(currencies, rates)

  /**
   * Places a rate into an empty matrix, as the initial pair of currencies.
   *
   * The two currencies take positions zero and one, their rate is recorded at the first position
   * of the second, its reciprocal at the second position of the first, and both rates of a
   * currency against itself are one - the four elements of a two-by-two matrix.
   *
   * Two identical currencies describe one currency rather than a pair, and the rate given is then
   * unobservable: the result is the one-currency matrix holding a rate of one.
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
   * The arithmetic is stated operand for operand - the cross rate is the rate multiplied by the
   * existing rate in that order, and the opposite rate is one divided by that same product.
   * Floating-point multiplication and division are neither exact nor associative, so a rearranged
   * expression would agree with this one only to within rounding, and the rates this matrix holds
   * are the numbers these expressions produce.
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
   * is left at one, since a currency is worth one of itself whatever the update says.
   *
   * Every rate read here is read from the matrix as it stood before the update, so no element of
   * the result is computed from another element of the result.
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

  /**
   * Offers one rate to a matrix, holding it back if it cannot be placed yet.
   *
   * This is the dispatch of the four outcomes described on [[FxMatrix.withRate]], and it is the
   * single step every factory and every placement of a rate is built from. It does not retry the
   * rates already held back; [[retried]] does that, and [[stepped]] is this offer followed by
   * that retry in the one case that can make a rate held back placeable - an offer that brought a
   * currency into the matrix. Of the four outcomes only the initial pair and the addition of a
   * new currency do so: an update returns a matrix of the same currencies, and a rate held back
   * returns the matrix it was offered to.
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
   * The shape of the passes is what settles the positions of the currencies in the result: the
   * rates held back are examined against the matrix as it stands at the start of a pass, every
   * one of them that can be placed is then placed in the order they were offered in, and the
   * examination is repeated. A rate that becomes placeable part way through a pass is therefore
   * placed by the next pass rather than immediately, which is what distinguishes this from a
   * depth-first placement: the two order the currencies of a matrix built from rates that connect
   * in a chain differently.
   *
   * Each rate of a pass is placed through [[offered]] rather than directly as the addition of a
   * new currency, which is what makes one state well defined: where two rates held back name the
   * same new currency - against two different currencies already held, or as a pair and the same
   * pair the other way round - the second of them finds both of its currencies present by the
   * time it is placed, and is an update to the first, exactly as the same rate offered directly
   * would have been.
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
   * Offers one rate to a matrix and then, where the offer brought a currency in, places every
   * rate held back that has become placeable.
   *
   * This is the whole of placing a rate: the rate is offered, and the rates held back are retried
   * exactly when a currency arrived, which is the only thing that can make a held-back rate
   * placeable. A rate held back is held back because the matrix contains neither of its
   * currencies, so a pass over the rates held back against a set of currencies that has not
   * changed since the previous pass places nothing; performing that pass after every offer would
   * therefore reach the same state while examining every rate held back once per offer, which is
   * time proportional to the square of the number of rates a caller offered - the amplification
   * this member exists to avoid.
   *
   * Whether a currency arrived is read from the number of currencies, which is the whole of the
   * test: of the four outcomes of an offer only the initial pair and the addition of a new
   * currency change [[FxMatrix.currencies]], an update leaves it as it stands, and a rate held
   * back leaves the matrix itself untouched. The retry is also skipped when nothing is held back,
   * which is the common case of a collection whose rates connect in the order they are offered.
   *
   * @param matrix  the matrix to offer the rate to
   * @param pending  the rates already held back
   * @param ccy1  the first currency of the pair, the reference currency of an update
   * @param ccy2  the second currency of the pair, the currency restated by an update
   * @param rate  the rate of the first currency in terms of the second
   * @return the matrix after the offer and any retry, and the rates still held back
   */
  private def stepped(
      matrix: FxMatrix,
      pending: PendingRates,
      ccy1: Currency,
      ccy2: Currency,
      rate: Double): (FxMatrix, PendingRates) = {
    val (advanced, offeredPending) = offered(matrix, pending, ccy1, ccy2, rate)
    val currencyArrived = advanced.currencies.size > matrix.currencies.size
    if (currencyArrived && offeredPending.nonEmpty) {
      retried(advanced, offeredPending)
    } else {
      (advanced, offeredPending)
    }
  }

  /**
   * Holds a rate back, to be retried when a later rate connects it.
   *
   * This is one keyed write into the rates held back: a rate offered for a pair already held back
   * takes the place of the rate held for it and keeps that pair's position in the offer order,
   * and a rate for a pair not held back is appended after the rates already held. Neither case
   * examines the rates already held back, so offering a rate that cannot be placed costs the same
   * whether one rate is held back or thousands - which is what bounds the work a collection of
   * mutually disconnected rates can demand.
   *
   * @param pending  the rates already held back
   * @param pair  the currency pair of the rate to hold back
   * @param rate  the rate of the base currency of that pair in terms of its counter currency
   * @return the rates held back, including this one
   */
  private def parked(pending: PendingRates, pair: CurrencyPair, rate: Double): PendingRates =
    pending.updated(pair, rate)

  /**
   * Answers with the matrix a fold reached, or with the failure listing the rates it could never
   * place.
   *
   * This is where the outcome of a fold is decided: a rate still held back once every rate has
   * been offered is a rate that has no currency in common with the matrix and never will, so the
   * fold has failed rather than partly succeeded.
   *
   * @param outcome  the matrix a fold reached and the rates it still holds back
   * @return the matrix, or the failure listing the rates that could never be placed
   */
  private def placed(outcome: (FxMatrix, PendingRates)): FailureOr[FxMatrix] = {
    val (matrix, pending) = outcome
    if (pending.isEmpty) Right(matrix) else Left(unplaceableRates(pending))
  }

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

  /**
   * The failure reported for a pair no rate of this matrix can convert.
   *
   * It names the pair asked for and the currencies the matrix holds, so a caller can see at once
   * whether the pair is outside the matrix or the matrix is not the one they meant to query.
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
   * It lists those rates, bounded as described on [[MaxListedRates]].
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
   * The form is a bracketed list separated by a comma and a space. The listing is unbounded
   * because the number of currencies is bounded: they are drawn from the closed family of
   * currencies [[Currency]] holds, so the longest list any matrix can produce is that family.
   *
   * @param currencies  the currencies to render
   * @return the rendering of those currencies
   */
  private def renderCurrencies(currencies: Vector[Currency]): String =
    currencies.mkString("[", ", ", "]")

  /**
   * Renders rates held back, bounded in length.
   *
   * The form is a braced list of pair and rate separated by an equals sign. Beyond
   * [[MaxListedRates]] rates the listing states how many it left out rather than growing with the
   * collection it was given.
   *
   * The rates are read in the order they were offered in, which is the order the rates held back
   * iterate in, and only as far as the bound: the rendering walks an iterator rather than taking
   * a prefix of the rates held back, so describing a large collection of rates that could never
   * be placed builds the bounded listing and nothing else.
   *
   * @param pending  the rates to render
   * @return the bounded rendering of those rates
   */
  private def renderRates(pending: PendingRates): String = {
    val listed =
      pending.iterator.take(MaxListedRates).map { case (pair, rate) => s"$pair=$rate" }.toVector
    val omitted = pending.size - listed.size
    val marker = if (omitted > 0) s", and $omitted more" else ""
    listed.mkString("{", ", ", marker + "}")
  }

  /**
   * The hashing and equality of matrices.
   *
   * This is the only equality-bearing instance of the type, and it is a `Hash` rather than an
   * `Order`: no ordering of matrices of rates would be meaningful. `Hash` extends `Eq`, so a
   * separate `Eq` would be a second answer to the same question; one is declared here and `Eq` is
   * obtained from it by subtyping.
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
   * The rendering is [[FxMatrix.toString]], so the two agree exactly.
   *
   * @return the rendering of matrices
   */
  implicit val show: Show[FxMatrix] = Show.show(_.toString)

  // The rates of the matrix go through the one policy this library has for a double, which writes
  // the values JSON cannot express as tagged strings. That is what carries a rate of zero and the
  // infinite reciprocal recorded for it through a round trip, and the import is what keeps the
  // choice local, as the codec support of `strata-collect` intends.
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
      extends NoJavaSerialization

  /**
   * The name of the field holding the currencies, which is the JSON key the derivation uses.
   *
   * It is named once here because the ceiling the decoder applies to the number of currencies a
   * document may state is expressed in terms of it, and a bound naming a field the document does
   * not hold would silently bound nothing.
   */
  private val CurrenciesField: String = "currencies"

  /** The derived decoder of the raw field shape, used by the validating decoder below. */
  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

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
   * to different documents. Every rate is written through the tagged-double policy of this
   * library, so a rate of zero and the infinite reciprocal recorded for it both survive a round
   * trip.
   *
   * Both halves of the codec are assembled by the same compile-time derivation over the same raw
   * shape, which is what keeps them from drifting apart: the field names, their order and their
   * element codecs are stated once, in [[Raw]], and the encoder reaches them by mapping a matrix
   * onto that shape. Deriving from [[FxMatrix]] itself is not possible - the constructor of a
   * validated type is not public, so there is no public shape to derive from.
   *
   * The result is wrapped so that a field holding no value would be omitted, which is the policy
   * every product of this library follows - this type has no optional field, so the wrapping
   * changes nothing about its output and exists so that the policy holds without exception.
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
   * ===How large a matrix a document may state===
   *
   * Both fields of the payload are sized by the document, and both are measured before anything is
   * read from them. The `currencies` array is measured here, against
   * `Codecs.MaximumCollectionElements`; the `rates` field needs no bound of its own, because it is
   * a [[com.opengamma.strata.collect.array.DoubleMatrix]] and its codec already refuses a payload
   * beyond `Codecs.MaximumMatrixRows`, `Codecs.MaximumMatrixColumns` or
   * `Codecs.MaximumMatrixElements` before it reads a row - bounding it a second time here would
   * state the same ceiling in two places and refuse nothing the matrix codec does not.
   *
   * Measuring the currencies first is what keeps the cost of a hostile document proportional to
   * nothing: the currencies are decoded name by name through the named-family lookup and then
   * checked for distinctness, so a document naming a million of them would pay for a million
   * lookups before the structural check that a matrix of ''n'' currencies must be ''n'' by ''n''
   * rejected it. A document beyond the ceiling is a decoding failure naming the ceiling and the
   * field, and no currency is looked up for it.
   *
   * The ceiling cannot refuse a matrix this port wrote. A matrix holds one row and one column per
   * currency, so a matrix of more currencies than the ceiling would need a square of rates ten
   * thousand times larger than the matrix codec reads - the rates of such a matrix could not be
   * written, let alone read back - and the widest matrix the reference data can populate is the
   * number of currencies it names, three orders of magnitude below the ceiling.
   *
   * @return the JSON decoding of a matrix
   */
  implicit val decoder: Decoder[FxMatrix] =
    Codecs.boundedFields(CurrenciesField -> Codecs.MaximumCollectionElements)(
      Codecs.validatedDecoder[Raw, FxMatrix](raw => fromMatrix(raw.currencies, raw.rates))(rawDecoder))
}
