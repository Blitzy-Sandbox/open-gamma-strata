/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import com.opengamma.strata.collect.ArgCheck

/**
 * Constants for the standard foreign exchange indices.
 *
 * An FX index is an agreed mechanism for determining a rate of exchange, published daily by an
 * administrator: the reference rates of the European Central Bank, or the closing spot rates of
 * the London market. Each constant here names one member of [[FxIndex]].
 *
 * The values are the same objects as the members of the [[FxIndex]] companion, so a constant
 * taken from here and the member of the same name are indistinguishable - including by `eq`, by
 * `==` and in a pattern match. What each one carries is what an FX index carries and no more: the
 * pair of currencies the rate is quoted for, the calendar of the dates it is fixed on, and the
 * offset from a fixing date to the date a conversion settles. An FX index is an [[Index]] and
 * deliberately not a [[FloatingRateIndex]], so no constant here has a currency, a day count or a
 * flag saying whether it is still published; see [[FxIndex]] for why the symmetry with the other
 * three families of the hierarchy stops there.
 *
 * The family is closed and no member of it can be substituted, so each constant is resolved
 * directly against the family rather than through any indirection.
 *
 * ===Eight constants for sixteen published indices===
 *
 * Only the European Central Bank rates and the London closing rates are named here. The eight
 * remaining published indices - the local rates of the definitions annex, such as
 * `USD/KRW-KFTC18-KRW02` - have no constant, and are reached through [[FxIndex.valueOf]],
 * [[FxIndex.of]] and [[FxIndex.values]], which own the contract of the family; this object adds
 * nothing to it.
 *
 * ===A pair of currencies does not name a constant===
 *
 * Two administrators publish the euro against the dollar, so `EUR_USD_ECB` and `EUR_USD_WM` are
 * two distinct indices quoting one pair of currencies. Every constant is therefore resolved by the
 * name of its index and never by its pair, which is what keeps the two apart: obtaining an index
 * from a pair is [[FxIndex.of]], which has to choose between the candidates of a pair and chooses
 * by name, so it answers `EUR/USD` with the European Central Bank rate whichever of the two the
 * caller had in mind.
 *
 * @see [[FxIndex]] for the family these constants are members of, and for its lookups
 */
object FxIndices {

  /**
   * Obtains the published FX index of the given name, failing fast when the family has no member
   * of that name.
   *
   * The lookup is [[FxIndex.valueOf]], the exact, alias-aware lookup of the family, rather than
   * [[FxIndex.of]]. The latter also reads text as a pair of currencies and then chooses between
   * the indices quoting that pair by name, which for the two euro/dollar rates would answer with
   * an index other than the one the name asked for; it also reports an absent value through
   * `Either`, which is the wrong channel for what can go wrong here.
   *
   * An absent value cannot be caused by a caller: it would mean that this holder and the
   * published index data of the family disagree about the name of an index, which is a defect in
   * this library rather than a data-dependent failure. It is reported through the module's single
   * sanctioned fail-fast channel, [[com.opengamma.strata.collect.ArgCheck]], so that every breach
   * of an invariant of this library reports the same kind of error. The second statement is
   * unreachable and exists only because the first is declared to return no value, while this
   * operation must produce an index.
   *
   * @param name  the name of the published index, such as `EUR/USD-ECB`
   * @return the index of that name
   * @throws IllegalArgumentException if the family publishes no index of that name
   */
  private def builtIn(name: String): FxIndex =
    FxIndex.valueOf(name).getOrElse {
      val message = s"Unknown built-in FX index: $name"
      ArgCheck.isTrue(false, message)
      sys.error(message)
    }

  /**
   * The FX index for conversion from EUR to CHF, as defined by the European Central Bank
   * "Euro foreign exchange reference rates".
   */
  val EUR_CHF_ECB: FxIndex = builtIn("EUR/CHF-ECB")

  /**
   * The FX index for conversion from EUR to GBP, as defined by the European Central Bank
   * "Euro foreign exchange reference rates".
   */
  val EUR_GBP_ECB: FxIndex = builtIn("EUR/GBP-ECB")

  /**
   * The FX index for conversion from EUR to JPY, as defined by the European Central Bank
   * "Euro foreign exchange reference rates".
   */
  val EUR_JPY_ECB: FxIndex = builtIn("EUR/JPY-ECB")

  /**
   * The FX index for conversion from EUR to USD, as defined by the European Central Bank
   * "Euro foreign exchange reference rates".
   */
  val EUR_USD_ECB: FxIndex = builtIn("EUR/USD-ECB")

  /**
   * The FX index for conversion from USD to CHF, as defined by the WM company
   * "Closing Spot rates".
   */
  val USD_CHF_WM: FxIndex = builtIn("USD/CHF-WM")

  /**
   * The FX index for conversion from GBP to USD, as defined by the WM company
   * "Closing Spot rates".
   */
  val GBP_USD_WM: FxIndex = builtIn("GBP/USD-WM")

  /**
   * The FX index for conversion from EUR to USD, as defined by the WM company
   * "Closing Spot rates".
   *
   * The rate is the euro against the dollar, as both the index name `EUR/USD-WM` and the published
   * data say.
   */
  val EUR_USD_WM: FxIndex = builtIn("EUR/USD-WM")

  /**
   * The FX index for conversion from USD to JPY, as defined by the WM company
   * "Closing Spot rates".
   */
  val USD_JPY_WM: FxIndex = builtIn("USD/JPY-WM")
}
