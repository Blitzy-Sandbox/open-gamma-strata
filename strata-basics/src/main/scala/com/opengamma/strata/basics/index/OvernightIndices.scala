/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import com.opengamma.strata.collect.ArgCheck

/**
 * Constants for the standard Overnight rate indices, published under the identifiers the ported
 * library used.
 *
 * Each constant is the standard definition of the index it names, carrying the currency the rate
 * is quoted in, the calendar of the days the rate fixes on, the offsets from a fixing date to the
 * publication and effective dates, and the day counts the rate and a conventional fixed leg
 * against it accrue on. A floating rate that has a constant here is fully supported by this
 * library, with example holiday calendar data behind its fixing calendar.
 *
 * ===Constants, not a registry===
 *
 * The holder being ported indirected every constant through an extended enum so that
 * configuration found on the classpath could replace a member. That indirection is gone: the
 * [[OvernightIndex]] family is closed, its members are created once from the index data
 * transcribed into this module, and each constant here is simply a name looked up in that family.
 * A constant and the member it names are therefore the same object - indistinguishable by `eq`, by
 * `==` and in a pattern match - and nothing outside this build can substitute either.
 *
 * ===Twenty-one constants over thirty-five members===
 *
 * The family publishes thirty-five indices, and this holder declares twenty-one constants naming
 * twenty of them - `EUR_ESTR` and `EUR_ESTER` name the same member under its current and its
 * retired spelling - which is exactly the set, and exactly the pairing, that the original holder
 * declared. The fifteen members with no constant here (the Swedish, Danish, Singaporean, Chilean,
 * Colombian, Czech, Hong Kong, Hungarian, Indonesian, Israeli, Indian, Russian, Saudi and Turkish
 * rates, Singapore contributing two) are reached through [[OvernightIndex.valueOf]] or
 * [[OvernightIndex.parse]] under the name the index data declares, exactly as they were reached in
 * the original. Nothing is added here and nothing is dropped: publishing a further index is a
 * change to the index data, never to this file.
 *
 * ===Two retired names===
 *
 * `CHF_TOIS` and `EUR_ESTER` name rates that are no longer current, and both were marked
 * deprecated in the original. They are kept, because a trade or a stored document written against
 * either name still has to resolve, and the note on each says what replaced it. Those notes are
 * carried as documentation rather than as a `@deprecated` annotation: this build compiles warnings
 * as errors, so annotating the two constants would turn every reference to them - from a test, or
 * from a later port of a module that reads a historic trade - into a build failure, which would
 * remove the names rather than deprecate them.
 *
 * @see [[OvernightIndex]] for the family itself, its members and the lookup of one by name
 * @see [[OvernightIndexData]] for the transcribed data the members are built from
 */
object OvernightIndices {

  /**
   * Looks up a member of the [[OvernightIndex]] family by name, refusing a name the family does
   * not have.
   *
   * The lookup is the alias-aware one of the family, so a constant may be declared under a retired
   * spelling and still resolve to the member that replaced it. That is how `EUR_ESTER` is declared
   * below, and it is the one constant here whose name is not a row of the index data.
   *
   * An absent value cannot be caused by a caller. Every name below is fixed text in this file and
   * every member is built from data transcribed into this module, so a name that does not resolve
   * means the two disagree - a defect in this library rather than a data-dependent failure, and
   * one that has to surface at once and loudly. It is therefore raised through the module's single
   * fail-fast channel, [[com.opengamma.strata.collect.ArgCheck]], which is what the static
   * initialiser being ported did when its own lookup failed. The second statement is unreachable
   * and exists only because the check is declared to return no value, while this method has to
   * return an index.
   *
   * @param name  the name of the index to look up, such as `GBP-SONIA`
   * @return the index published under that name
   * @throws IllegalArgumentException if the family publishes no index of that name
   */
  private def builtIn(name: String): OvernightIndex =
    OvernightIndex.valueOf(name).getOrElse {
      val message = s"Unknown built-in Overnight index: $name"
      ArgCheck.isTrue(false, message)
      sys.error(message)
    }

  //-------------------------------------------------------------------------
  /**
   * The SONIA index for GBP.
   *
   * The Sterling Overnight Index Average (SONIA) index.
   */
  val GBP_SONIA: OvernightIndex = builtIn("GBP-SONIA")

  /**
   * The SARON index for CHF.
   *
   * The Swiss Average Overnight Rate (SARON) index.
   */
  val CHF_SARON: OvernightIndex = builtIn("CHF-SARON")

  /**
   * The TOIS index for CHF.
   *
   * The Tomorrow/Next Overnight Indexed Swaps (TOIS) index, which is a "Tomorrow/Next" index.
   *
   * Retired: not published as of 2017-12-29. The index remains resolvable for historic data; see
   * the note on retired names on this object.
   */
  val CHF_TOIS: OvernightIndex = builtIn("CHF-TOIS")

  /**
   * The EONIA index for EUR.
   *
   * The Euro OverNight Index Average (EONIA) index.
   */
  val EUR_EONIA: OvernightIndex = builtIn("EUR-EONIA")

  /**
   * The ESTR index for EUR.
   *
   * The Euro Short-Term Rate (€STR) index. This was first published on 2019-10-02.
   */
  val EUR_ESTR: OvernightIndex = builtIn("EUR-ESTR")

  /**
   * The ESTR index for EUR, under the old ESTER name.
   *
   * The index was renamed. This constant is looked up under the retired spelling, which the family
   * accepts as an alternate name, so it resolves to the very same index as `EUR_ESTR` - the two
   * constants are one object, and the name this one reports is the current `EUR-ESTR`.
   *
   * Retired: use `EUR_ESTR` instead; see the note on retired names on this object.
   */
  val EUR_ESTER: OvernightIndex = builtIn("EUR-ESTER")

  /**
   * The TONAR index for JPY.
   *
   * The Tokyo Overnight Average Rate (TONAR) index.
   */
  val JPY_TONAR: OvernightIndex = builtIn("JPY-TONAR")

  /**
   * The Fed Fund index for USD.
   *
   * The Federal Funds Rate index.
   */
  val USD_FED_FUND: OvernightIndex = builtIn("USD-FED-FUND")

  /**
   * The SOFR index for USD.
   *
   * The Secured Overnight Financing Rate (SOFR) index.
   */
  val USD_SOFR: OvernightIndex = builtIn("USD-SOFR")

  /**
   * The AMERIBOR index for USD.
   *
   * The AMERIBOR index.
   */
  val USD_AMERIBOR: OvernightIndex = builtIn("USD-AMERIBOR")

  /**
   * The AONIA index for AUD.
   *
   * AONIA is an "Overnight" index.
   */
  val AUD_AONIA: OvernightIndex = builtIn("AUD-AONIA")

  /**
   * The CDI index for BRL.
   *
   * The "Brazil Certificates of Interbank Deposit" index.
   */
  val BRL_CDI: OvernightIndex = builtIn("BRL-CDI")

  /**
   * The CORRA index for CAD.
   *
   * The "Canadian Overnight Repo Rate Average" index.
   */
  val CAD_CORRA: OvernightIndex = builtIn("CAD-CORRA")

  /**
   * The TN index for DKK.
   *
   * The "Tomorrow/Next-renten" index.
   */
  val DKK_TNR: OvernightIndex = builtIn("DKK-TNR")

  /**
   * The NOWA index for NOK.
   *
   * The "Norwegian Overnight Weighted Average" index.
   */
  val NOK_NOWA: OvernightIndex = builtIn("NOK-NOWA")

  /**
   * The NZIONA index for NZD.
   *
   * The "New Zealand Overnight" index.
   */
  val NZD_NZIONA: OvernightIndex = builtIn("NZD-NZIONA")

  /**
   * The PLONIA index for PLN.
   *
   * The "Polish Overnight" index.
   */
  val PLN_POLONIA: OvernightIndex = builtIn("PLN-POLONIA")

  /**
   * The POLSTR index for PLN.
   *
   * The "Polish Short Term Rate".
   */
  val PLN_POLSTR: OvernightIndex = builtIn("PLN-POLSTR")

  /**
   * The SIOR index for SEK.
   *
   * The "STIBOR T/N" index.
   */
  val SEK_SIOR: OvernightIndex = builtIn("SEK-SIOR")

  /**
   * The THOR index for THB.
   *
   * The "Thai Overnight Repurchase Rate" index.
   */
  val THB_THOR: OvernightIndex = builtIn("THB-THOR")

  /**
   * The SABOR index for ZAR.
   *
   * The "South African Benchmark Overnight Rate" index.
   */
  val ZAR_SABOR: OvernightIndex = builtIn("ZAR-SABOR")
}
