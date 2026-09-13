/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import com.opengamma.strata.collect.ArgCheck

/**
 * The published price indices, one named constant per index.
 *
 * Each constant below names one of the standard price indices of the library and answers with
 * the definition of that index: the currency its level is quoted in, the region whose price
 * level it measures, whether it is still published and how often it is published. The
 * definitions themselves are not held here - they belong to the closed [[PriceIndex]] family in
 * `Index.scala`, which creates every member of the family from the published index data. This
 * object is a directory of that family and nothing more, so a constant and the index a caller
 * reaches by name are one and the same value:
 *
 * {{{
 * PriceIndex.valueOf("GB-RPI").contains(PriceIndices.GB_RPI)  // true
 * }}}
 *
 * A price index publishes a level rather than a rate per tenor, so where an Ibor or an overnight
 * index carries a tenor and a fixing calendar these constants carry neither, and their day count
 * is the one-to-one convention the [[PriceIndex]] type itself fixes rather than a column of the
 * data. Four of the names below, `GB-RPI`, `EU-EXT-CPI`, `US-CPI-U` and `FR-EXT-CPI`, are also
 * spelled by a constant of `FloatingRateNames`; those are values of a different type resolved
 * from a different table, and the two are deliberately kept apart. Text naming one of them
 * resolves to the index rather than to the family, because `FloatingRate.tryParse` probes the
 * index families first.
 *
 * The nine constants cover the published index data exactly - nine constants for nine rows - and
 * are declared in published order. No alternate name resolves to a price index, so `UK-HICP`,
 * `UK-RPI`, `UK-RPIX`, `SWF-CPI`, `EUR-AI-CPI`, `JPY-CPI-EXF` and `USA-CPI-U` are not names of
 * anything; adding an index, or introducing one of those names, would be new behaviour.
 *
 * The constants resolve against the closed family while this object is initialised: no file is
 * read, nothing is looked up at run time and no index can be substituted. The lookup of an index
 * by name, and the enumeration of the family, are the business of `PriceIndex.valueOf` and
 * `PriceIndex.values`, which this object does not duplicate.
 *
 * All members are immutable values initialised once, so this object is thread-safe.
 */
object PriceIndices {

  /**
   * Resolves one of the indices this object publishes, failing fast if the family has no such
   * member.
   *
   * The name passed in is a literal of this file, never caller input, so a missing member is not
   * a data-dependent failure to be reported through `Either`: it means this directory and the
   * published index data of `Index.scala` disagree, which is a defect in the library that has to
   * surface at once and loudly. The failure is raised through the module's single sanctioned
   * fail-fast channel, [[com.opengamma.strata.collect.ArgCheck]], so that every invariant breach
   * in the library reports the same kind of error.
   *
   * The second statement is unreachable: it exists only because the check is declared to return
   * no value, while this operation has to produce an index.
   *
   * @param name  the name of the index to resolve, such as `GB-RPI`
   * @return the index of that name
   * @throws IllegalArgumentException if the price index family has no member of that name
   */
  private def builtIn(name: String): PriceIndex =
    PriceIndex.valueOf(name).getOrElse {
      val message = s"Unknown built-in Price index: $name"
      ArgCheck.isTrue(false, message)
      sys.error(message)
    }

  /**
   * The harmonized consumer price index for the United Kingdom,
   * "Non-revised Harmonised Index of Consumer Prices".
   */
  val GB_HICP: PriceIndex = builtIn("GB-HICP")

  /**
   * The retail price index for the United Kingdom,
   * "Non-revised Retail Price Index All Items in the United Kingdom".
   */
  val GB_RPI: PriceIndex = builtIn("GB-RPI")

  /**
   * The retail price index for the United Kingdom excluding mortgage interest payments,
   * "Non-revised Retail Price Index Excluding Mortgage Interest Payments in the United Kingdom".
   */
  val GB_RPIX: PriceIndex = builtIn("GB-RPIX")

  /**
   * The consumer price index for Switzerland,
   * "Non-revised Consumer Price Index".
   */
  val CH_CPI: PriceIndex = builtIn("CH-CPI")

  /**
   * The consumer price index for Europe,
   * "Non-revised Harmonised Index of Consumer Prices All Items".
   */
  val EU_AI_CPI: PriceIndex = builtIn("EU-AI-CPI")

  /**
   * The consumer price index for Europe,
   * "Non-revised Harmonised Index of Consumer Prices Excluding Tobacco".
   */
  val EU_EXT_CPI: PriceIndex = builtIn("EU-EXT-CPI")

  /**
   * The consumer price index for Japan excluding fresh food,
   * "Non-revised Consumer Price Index Nationwide General Excluding Fresh Food".
   */
  val JP_CPI_EXF: PriceIndex = builtIn("JP-CPI-EXF")

  /**
   * The consumer price index for US Urban consumers,
   * "Non-revised index of Consumer Prices for All Urban Consumers (CPI-U) before seasonal
   * adjustment".
   */
  val US_CPI_U: PriceIndex = builtIn("US-CPI-U")

  /**
   * The consumer price index for France,
   * "Non-revised Harmonised Index of Consumer Prices Excluding Tobacco".
   */
  val FR_EXT_CPI: PriceIndex = builtIn("FR-EXT-CPI")
}
