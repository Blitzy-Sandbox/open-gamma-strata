/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

/**
 * The conventional currency pair reference data of the module, expressed as immutable Scala data.
 *
 * This object carries the currency pair table transcribed verbatim from the Java currency pair
 * configuration, which was a classpath resource loaded at class initialisation time in the
 * implementation being ported. It is compiled data here, so there is no resource to locate, no
 * registry to populate and no load failure to recover from - the Java loader swallowed a load
 * failure and returned an empty table, silently turning every pair into an unconfigured one, and
 * that failure mode does not exist for data the compiler has already read.
 *
 * It holds data only and no behaviour. `CurrencyPair` is its consumer and reads [[rows]] and
 * [[rateDigitsByCurrencies]] to answer `isConventional` and `getRateDigits`. Those two member
 * names are the published contract of this object and are kept stable.
 *
 * That contract is offered to the module and to nothing outside it, which is why the object is
 * qualified `private[basics]`: these rows are a transcription that the module's own types consume,
 * not an extension point a caller elsewhere should be able to reach for and then depend on, and
 * `CurrencyData` carries the same qualifier for the same reason. The restriction still reaches
 * every part of the module, so `ReferenceDataManifestSpec` reads the table directly to make the
 * comparison described under the invariants below.
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
 * so would silently destroy the distinction. The Java configuration stated the same rule from the
 * other side, declaring it an error to define two sections for the same effective pair.
 *
 * ===Keyed by the two currencies, not by the pair===
 *
 * The table is keyed by the base and counter [[Currency]] of the conventional pair rather than by
 * a `CurrencyPair`, even though a `CurrencyPair` is what the consumer looks up. Keying it by the
 * pair type would make this object depend on the very type that depends on it, leaving a cycle to
 * be resolved while these objects initialise; harmless to compile, since the module compiles as a
 * whole, but a hazard at run time whose outcome depends on which object a program happens to touch
 * first. The Java implementation met the same class of problem in its currency table and solved it
 * the same way, holding a triangulation currency as a plain code rather than as a currency.
 * Keying by the components keeps initialisation strictly one way: [[CurrencyData]] to
 * [[Currency]] to this object to `CurrencyPair`.
 *
 * ===Invariants===
 *
 * All of the following are asserted row for row against the Java captured reference data manifest
 * by `ReferenceDataManifestSpec`, so a self consistent but mistranscribed row cannot pass:
 *
 *  - [[rows]] holds exactly 92 entries, in the declaration order of the Java configuration, with
 *    the same number of fractional digits for each pair.
 *  - The 92 keys are distinct, and no key has its inverse also present.
 *  - `rateDigits` is 0 for 1 row, 2 for 14 rows, 3 for 10 rows, 4 for 61 rows and 5 for 6 rows.
 *  - The 54 currencies the table names are all members of [[Currency]], each reached through its
 *    named constant rather than through a code, so a currency that did not exist would be a
 *    compile error rather than a lookup that fails later.
 *
 * No pair may be added to, removed from or edited in this table: it is a transcription of existing
 * reference data, and introducing a new market convention is outside the scope of the port.
 *
 * ===Thread safety===
 *
 * Both members are immutable values computed once while this object initialises, and nothing here
 * changes afterwards. They may be shared freely between threads.
 *
 * @see [[CurrencyData]] for the currency table and the market convention priority ordering
 */
private[basics] object CurrencyPairData {

  /**
   * The 92 transcribed conventional currency pairs with the number of fractional digits of a
   * typical rate, in the declaration order of the Java currency pair configuration.
   *
   * Each entry is the base currency, the counter currency and the rate digits of the pair, read in
   * the conventional direction described above. The order is observable through any iteration a
   * consumer performs, so it is kept stable and deterministic rather than re-sorted, and the
   * comments mark the groupings the Java configuration carried.
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
}
