/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import scala.collection.immutable.HashMap

/**
 * The conventional currency pair reference data of the module, expressed as immutable Scala data.
 *
 * This object is the currency pair table itself, compiled into the module. It has 92 rows and
 * three columns: the base [[Currency]] of a pair quoted in standard market convention, its
 * counter currency, and the number of fractional digits a typical rate for that pair is quoted
 * to. Nothing is read at run time, so the table is complete once this object initialises and
 * cannot be absent or partial.
 *
 * It holds data only and no behaviour. `CurrencyPair` is its consumer and reads [[rows]],
 * [[rateDigitsByCurrencies]] and [[rateDigitsByBase]] to answer `isConventional` and
 * `getRateDigits`. The first two member names are the published contract of this object and are
 * kept stable - the reference-data manifest test reads both of them, `rateDigitsByCurrencies` down
 * to its size and the shape of its keys - while the third is the lookup view the module's own
 * predicates descend, added beside them rather than in place of either.
 *
 * That contract is offered to the module and to nothing outside it, which is why the object is
 * qualified `private[basics]`: these rows are reference data that the module's own types consume,
 * not an extension point a caller elsewhere should be able to reach for and then depend on, and
 * `CurrencyData` carries the same qualifier for the same reason. The restriction reaches every
 * part of the module, so any type of the module may read the table directly.
 *
 * ===Each row is the pair in standard market convention===
 *
 * A market convention determines that a rate between two currencies is quoted one way round and
 * not the other: `EUR/USD` is quoted, `USD/EUR` is not. Each row of this table is therefore the
 * conventional direction of its pair, and the inverse direction is '''deliberately absent'''.
 * That asymmetry is the data rather than an omission, and it is what makes the convention
 * decidable: a pair present in the table is conventional, a pair whose inverse is present is not,
 * and a pair with neither direction present falls back to the priority ordering held in
 * [[CurrencyData]]. Storing both directions would answer the first two questions the same way and
 * so would silently destroy the distinction, which is why no pair and its inverse are both
 * present.
 *
 * ===Keyed by the two currencies, not by the pair===
 *
 * The table is keyed by the base and counter [[Currency]] of the conventional pair rather than by
 * a `CurrencyPair`, even though a `CurrencyPair` is what the consumer looks up. Keying it by the
 * pair type would make this object depend on the very type that depends on it, leaving a cycle to
 * be resolved while these objects initialise - a hazard at run time whose outcome depends on which
 * object a program happens to touch first. Keying by the components keeps initialisation strictly
 * one way: [[CurrencyData]] to [[Currency]] to this object to `CurrencyPair`.
 *
 * ===Invariants===
 *
 * These hold row for row, and a consumer may rely on each of them:
 *
 *  - [[rows]] holds exactly 92 entries, in the published order, with one number of fractional
 *    digits for each pair.
 *  - The 92 keys are distinct, and no key has its inverse also present.
 *  - [[rateDigitsByCurrencies]] and [[rateDigitsByBase]] are two views of those same 92 rows and
 *    answer identically for every pair of currencies, because both are derived from [[rows]].
 *  - `rateDigits` is 0 for 1 row, 2 for 14 rows, 3 for 10 rows, 4 for 61 rows and 5 for 6 rows.
 *  - The 54 currencies the table names are all members of [[Currency]], each reached through its
 *    named constant rather than through a code, so a currency that did not exist would be a
 *    compile error rather than a lookup that fails later.
 *
 * No pair may be added to, removed from or edited in this table: it is published reference data,
 * and a new market convention is not established here.
 *
 * ===Thread safety===
 *
 * Every member is an immutable value computed once while this object initialises, and nothing here
 * changes afterwards. They may be shared freely between threads.
 *
 * @see [[CurrencyData]] for the currency table and the market convention priority ordering
 */
private[basics] object CurrencyPairData {

  /**
   * The 92 conventional currency pairs with the number of fractional digits of a typical rate, in
   * the published order.
   *
   * Each entry is the base currency, the counter currency and the rate digits of the pair, read in
   * the conventional direction described above. The order is part of the meaning of the table and
   * is observable through any iteration a consumer performs, so it is kept exactly as published
   * rather than re-sorted. The comments mark the groupings the pairs are published in.
   */
  val rows: Vector[(Currency, Currency, Int)] = Vector(
    // Major currencies
    (Currency.EUR, Currency.AUD, 5),
    (Currency.EUR, Currency.CAD, 5),
    (Currency.EUR, Currency.CHF, 5),
    (Currency.EUR, Currency.GBP, 5),
    (Currency.EUR, Currency.JPY, 2),
    (Currency.EUR, Currency.NZD, 4),
    (Currency.EUR, Currency.USD, 4),

    (Currency.GBP, Currency.AUD, 4),
    (Currency.GBP, Currency.CAD, 4),
    (Currency.GBP, Currency.CHF, 4),
    (Currency.GBP, Currency.JPY, 3),
    (Currency.GBP, Currency.NZD, 4),
    (Currency.GBP, Currency.USD, 4),

    (Currency.AUD, Currency.CAD, 4),
    (Currency.AUD, Currency.CHF, 4),
    (Currency.AUD, Currency.JPY, 3),
    (Currency.AUD, Currency.NZD, 4),
    (Currency.AUD, Currency.USD, 4),

    (Currency.NZD, Currency.CAD, 4),
    (Currency.NZD, Currency.CHF, 4),
    (Currency.NZD, Currency.JPY, 3),
    (Currency.NZD, Currency.USD, 4),

    (Currency.USD, Currency.CAD, 4),
    (Currency.USD, Currency.CHF, 4),
    (Currency.USD, Currency.JPY, 2),

    (Currency.CAD, Currency.CHF, 4),
    (Currency.CAD, Currency.JPY, 3),

    (Currency.CHF, Currency.JPY, 3),

    // Scandinavian currencies
    (Currency.EUR, Currency.DKK, 4),
    (Currency.GBP, Currency.DKK, 4),
    (Currency.USD, Currency.DKK, 4),
    (Currency.DKK, Currency.NOK, 4),
    (Currency.DKK, Currency.SEK, 4),

    (Currency.EUR, Currency.NOK, 4),
    (Currency.GBP, Currency.NOK, 4),
    (Currency.USD, Currency.NOK, 4),
    (Currency.NOK, Currency.SEK, 4),

    (Currency.USD, Currency.SEK, 4),
    (Currency.GBP, Currency.SEK, 4),
    (Currency.EUR, Currency.SEK, 4),

    // Other European currencies
    (Currency.EUR, Currency.BGN, 4),
    (Currency.USD, Currency.BGN, 4),

    (Currency.EUR, Currency.CZK, 3),
    (Currency.USD, Currency.CZK, 4),

    (Currency.EUR, Currency.HRK, 4),
    (Currency.USD, Currency.HRK, 4),

    (Currency.EUR, Currency.HUF, 2),
    (Currency.USD, Currency.HUF, 2),

    (Currency.EUR, Currency.ISK, 2),
    (Currency.USD, Currency.ISK, 2),

    (Currency.EUR, Currency.PLN, 4),
    (Currency.USD, Currency.PLN, 4),

    (Currency.EUR, Currency.RON, 4),
    (Currency.USD, Currency.RON, 4),

    (Currency.EUR, Currency.TRY, 4),
    (Currency.USD, Currency.TRY, 4),

    (Currency.EUR, Currency.UAH, 4),
    (Currency.USD, Currency.UAH, 4),

    // Other currencies
    (Currency.USD, Currency.AED, 4),
    (Currency.USD, Currency.ARS, 4),
    (Currency.USD, Currency.BHD, 5),
    (Currency.USD, Currency.BRL, 4),
    (Currency.USD, Currency.CLP, 2),
    (Currency.USD, Currency.CNH, 4),
    (Currency.USD, Currency.CNY, 4),
    (Currency.USD, Currency.COP, 2),
    (Currency.USD, Currency.EGP, 4),
    (Currency.USD, Currency.HKD, 4),
    (Currency.USD, Currency.IDR, 2),
    (Currency.USD, Currency.ILS, 4),
    (Currency.USD, Currency.INR, 4),
    (Currency.USD, Currency.KRW, 2),
    (Currency.USD, Currency.KZT, 2),
    (Currency.USD, Currency.MAD, 4),
    (Currency.USD, Currency.MXN, 4),
    (Currency.USD, Currency.MYR, 4),
    (Currency.USD, Currency.OMR, 5),
    (Currency.USD, Currency.PEN, 4),
    (Currency.USD, Currency.PHP, 3),
    (Currency.USD, Currency.PKR, 3),
    (Currency.USD, Currency.QAR, 4),
    (Currency.USD, Currency.SGD, 4),
    (Currency.USD, Currency.RUB, 4),
    (Currency.USD, Currency.SAR, 4),
    (Currency.USD, Currency.THB, 3),
    (Currency.USD, Currency.TWD, 3),
    (Currency.USD, Currency.VND, 0),
    (Currency.USD, Currency.ZAR, 4),

    // X currencies
    (Currency.XAG, Currency.USD, 4),
    (Currency.XAU, Currency.USD, 2),
    (Currency.XPD, Currency.USD, 2),
    (Currency.XPT, Currency.USD, 2))

  /**
   * The rate digits of each conventional pair, keyed by its base and counter currency.
   *
   * Derived from [[rows]], so the two can never disagree. The keys are distinct, so the map holds
   * the same 92 entries. A key present here is a conventional pair; a pair whose inverse is a key
   * here is the non conventional direction of a configured pair; a pair with neither direction
   * present is not configured at all. Deciding between those three cases, and the fall back to the
   * minor unit digits of the two currencies for an unconfigured pair, belongs to `CurrencyPair`
   * and deliberately has no counterpart here - this object answers only what the reference data
   * states.
   */
  val rateDigitsByCurrencies: Map[(Currency, Currency), Int] =
    rows.iterator.map { case (base, counter, rateDigits) => (base, counter) -> rateDigits }.toMap

  /**
   * The rate digits of each conventional pair, keyed by base currency and then by counter
   * currency, held for lookup rather than for iteration.
   *
   * This is the view `CurrencyPair.isConventional` and `CurrencyPair.getRateDigits` descend, and it
   * exists because those two are pure predicates called in loops - a conversion walking a matrix of
   * currencies asks them once per cell - and a lookup that allocates cannot be hoisted out of such
   * a loop by the JIT. Reaching [[rateDigitsByCurrencies]] costs a `Tuple2` for the composite key
   * and the `Some` that `get` wraps its answer in, on every call; descending this view costs
   * neither, because a nested lookup is keyed by a currency each consumer already holds and
   * `getOrElse` answers with the value itself rather than with an `Option` of it. Nothing is boxed
   * per call either: the digits were boxed once, when this table was built, and the "not
   * configured" answer the consumer supplies is a small enough integer to be served by the cache
   * of `java.lang.Integer`.
   *
   * The type is written as `HashMap` rather than as `Map` deliberately, at both levels, and that
   * is a statement about the representation rather than about the interface: the `Map` factory
   * answers with one of `Map1`…`Map4` for a handful of entries and with a `HashMap` beyond that,
   * so a `Map`-typed member would be a trie for the base currencies and a linear scan of up to
   * four keys for most of the inner tables. Naming `HashMap` fixes one representation, so a
   * lookup costs the same for the base currency with thirty counter currencies as for one with a
   * single counter, and it is also exactly what the derivation below produces - `HashMap.updated`
   * answers a `HashMap` - so the type states what is there rather than widening it.
   *
   * ===Why this is added rather than substituted===
   *
   * [[rateDigitsByCurrencies]] stays exactly as it is, with the same type and the same 92 entries:
   * it is what the reference-data manifest test reads, keys included, to verify that the
   * transcription of the published table is faithful, and that test is the reason this object can
   * be trusted at all. A lookup view is not a reason to change what the data publishes, so the
   * fast path is added beside the published table - the same relationship `CountryData` holds
   * between its published code tables and its hash indexes - and both views are derived from
   * [[rows]], so no second transcription exists that could disagree with the first.
   *
   * Like the table above it is keyed by the two currencies rather than by a `CurrencyPair`, for
   * the reason recorded on this object: keying by the pair type would make this object depend on
   * the very type that depends on it, leaving a cycle to be resolved while the two initialise.
   * The nesting is the shape the consumer's questions have - is ''this'' direction configured,
   * and then is the ''inverse'' configured - so each question is one descent from a base currency.
   *
   * The qualifier is stated on this member, where the two above leave it to the object, to record
   * that it is the module's own fast path and not part of the contract the manifest verifies: a
   * consumer outside this module could not reach it in any event, since the object itself is
   * `private[basics]`.
   */
  private[basics] val rateDigitsByBase: HashMap[Currency, HashMap[Currency, Int]] =
    rows.iterator.foldLeft(HashMap.empty[Currency, HashMap[Currency, Int]]) {
      case (accumulated, (base, counter, rateDigits)) =>
        accumulated.updated(
          base,
          accumulated
            .getOrElse(base, HashMap.empty[Currency, Int])
            .updated(counter, rateDigits))
    }
}
