/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import com.opengamma.strata.collect.Named

/**
 * An index of an observable value, such as an interest rate, a price level or an exchange rate.
 *
 * An index is a published figure that a financial instrument refers to rather than carries: a
 * swap leg pays what `GBP-LIBOR-3M` fixed at, and the instrument records the index while the
 * figure itself comes from market data. Every index is identified by a name - `GBP-LIBOR-3M`,
 * `EUR-ESTR`, `GB-RPI`, `EUR/USD-ECB` - and that name is the index's identity throughout this
 * library, in text, in JSON and as a map key.
 *
 * ===Why the hierarchy is closed, and why it is one file===
 *
 * The set of indices is reference data, not an extension point: an application refers to the
 * indices the market publishes, and this library knows what those are. The hierarchy is
 * therefore `sealed`, so that a match over it is checked for exhaustiveness by the compiler and
 * no index can exist that this library did not publish. Scala requires every direct subtype of
 * a sealed type to be declared in the same file as the type, which is why this one file holds
 * the whole hierarchy - the three abstractions below and every concrete family - rather than
 * one file per family. The named constants of each family, and the data tables its members are
 * built from, live in their own files, because those only refer to members and do not extend
 * anything.
 *
 * The abstractions are three, and they exist because different code needs different guarantees:
 *
 *   - `Index` is any index at all, which is what an instrument refers to.
 *   - [[RateIndex]] is an index of an interest rate, which is what code computing interest
 *     needs: it excludes price and exchange-rate indices.
 *   - [[FloatingRateIndex]] is an index whose figure is a floating rate, which is what code
 *     resolving a [[FloatingRate]] name to a concrete index needs.
 *
 * No member of any family is declared yet: the members are created in the companions below
 * from the published index data, and those tables are ported together with the concrete fields
 * of each family - the currency, the calendars, the offsets and the day counts an index carries.
 * Until then each family is a closed set with nothing in it, so a lookup by name finds nothing
 * and a match over the hierarchy has no case to write.
 *
 * @see [[FloatingRate]] for the abstraction over an index and the family it belongs to
 * @see [[FloatingRateName]] for the family identifier a floating rate index reports
 */
sealed trait Index extends Named

/**
 * An index of an interest rate.
 *
 * This is the subset of [[Index]] whose figure is an interest rate, which is every Ibor index
 * and every overnight index. Code that computes interest takes this type rather than `Index`,
 * so that a price index or an exchange-rate index cannot reach it.
 */
sealed trait RateIndex extends Index

/**
 * An index whose figure is a floating rate.
 *
 * This is the subset of [[Index]] that a [[FloatingRate]] name can resolve to: the Ibor,
 * overnight and price indices, but not the exchange-rate indices. It is the type
 * [[FloatingRate.tryParse]] answers with when the text it was given names a concrete index
 * rather than a family.
 */
sealed trait FloatingRateIndex extends Index with FloatingRate

//-------------------------------------------------------------------------
/**
 * An Ibor-like index, whose rate is published for a fixed tenor.
 *
 * An Ibor index fixes a rate for borrowing over a period that starts shortly after the fixing
 * and runs for the index's tenor, as `GBP-LIBOR-3M` fixes a three-month rate. A member carries
 * the currency it is quoted in, the calendars and times its fixing is published against, the
 * offsets from fixing to effective and maturity dates, and the day count its rate is quoted on.
 */
sealed abstract class IborIndex extends RateIndex with FloatingRateIndex

/**
 * Holds the members of the Ibor index family and the lookup of a member by name.
 */
object IborIndex {

  /**
   * The members of the family, by name.
   *
   * The members are created from the published Ibor index data, which is ported with the
   * concrete fields of the family, so this table is empty until then.
   */
  private val byName: Map[String, IborIndex] = Map.empty

  /**
   * Looks up an Ibor index by name, answering with nothing when no member has that name.
   *
   * @param name  the index name, such as `GBP-LIBOR-3M`
   * @return the index of that name, or nothing when the family has no such member
   */
  def valueOf(name: String): Option[IborIndex] = byName.get(name)
}

//-------------------------------------------------------------------------
/**
 * An overnight index, whose rate is published for a single business day.
 *
 * An overnight index fixes a rate for borrowing over one day, as `EUR-ESTR` and `USD-SOFR` do.
 * A rate for a longer period is derived from the daily fixings, by compounding or by averaging,
 * which is what distinguishes the two kinds of overnight [[FloatingRateType]].
 */
sealed abstract class OvernightIndex extends RateIndex with FloatingRateIndex

/**
 * Holds the members of the overnight index family and the lookup of a member by name.
 */
object OvernightIndex {

  /**
   * The members of the family, by name.
   *
   * The members are created from the published overnight index data, which is ported with the
   * concrete fields of the family, so this table is empty until then.
   */
  private val byName: Map[String, OvernightIndex] = Map.empty

  /**
   * Looks up an overnight index by name, answering with nothing when no member has that name.
   *
   * @param name  the index name, such as `EUR-ESTR`
   * @return the index of that name, or nothing when the family has no such member
   */
  def valueOf(name: String): Option[OvernightIndex] = byName.get(name)
}

//-------------------------------------------------------------------------
/**
 * An index of a price level, such as a measure of inflation.
 *
 * A price index is published monthly rather than daily, as `GB-RPI` and `US-CPI-U` are, so an
 * observation of one names a month rather than a date. A member carries the currency and the
 * region whose prices it measures.
 */
sealed abstract class PriceIndex extends FloatingRateIndex

/**
 * Holds the members of the price index family and the lookup of a member by name.
 */
object PriceIndex {

  /**
   * The members of the family, by name.
   *
   * The members are created from the published price index data, which is ported with the
   * concrete fields of the family, so this table is empty until then.
   */
  private val byName: Map[String, PriceIndex] = Map.empty

  /**
   * Looks up a price index by name, answering with nothing when no member has that name.
   *
   * @param name  the index name, such as `GB-RPI`
   * @return the index of that name, or nothing when the family has no such member
   */
  def valueOf(name: String): Option[PriceIndex] = byName.get(name)
}

//-------------------------------------------------------------------------
/**
 * An index of a foreign exchange rate between two currencies.
 *
 * An FX index fixes the rate at which one currency converts to another, as `EUR/USD-ECB` does.
 * It is an [[Index]] but not a [[RateIndex]] or a [[FloatingRateIndex]], because the figure it
 * publishes is an exchange rate rather than an interest rate - which is why a floating rate
 * name never resolves to one.
 */
sealed abstract class FxIndex extends Index

/**
 * Holds the members of the FX index family and the lookup of a member by name.
 */
object FxIndex {

  /**
   * The members of the family, by name.
   *
   * The members are created from the published FX index data, which is ported with the
   * concrete fields of the family, so this table is empty until then.
   */
  private val byName: Map[String, FxIndex] = Map.empty

  /**
   * Looks up an FX index by name, answering with nothing when no member has that name.
   *
   * @param name  the index name, such as `EUR/USD-ECB`
   * @return the index of that name, or nothing when the family has no such member
   */
  def valueOf(name: String): Option[FxIndex] = byName.get(name)
}
