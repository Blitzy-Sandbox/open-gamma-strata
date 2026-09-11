/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.location.Country
import com.opengamma.strata.basics.schedule.Frequency

/**
 * A single row of transcribed price index reference data.
 *
 * Each row corresponds to one entry of the price index configuration that the Java implementation
 * read from the classpath, and carries exactly the five columns that configuration declared: the
 * index name, the currency the index is quoted in, the region whose price level it measures,
 * whether the index is still published, and how often it is published.
 *
 * The row is data and nothing else. It deliberately does not carry the day count of a price index:
 * the Java configuration held no such column and the Java index type supplied a fixed default,
 * so the day count belongs with the index family in `Index.scala` rather than with this
 * transcription. Adding a column here that the original data does not have would be a change of
 * behaviour, not a port of it.
 *
 * @param name                  the unique name of the index, such as `GB-RPI`
 * @param currency              the currency the index is quoted in
 * @param region                the region whose price level the index measures
 * @param active                whether the index is currently published, false once it has been
 *                              discontinued
 * @param publicationFrequency  the frequency at which the index is published
 */
private[basics] final case class PriceIndexRow(
    name: String,
    currency: Currency,
    region: Country,
    active: Boolean,
    publicationFrequency: Frequency)

/**
 * The price index reference data of the module, expressed as immutable Scala data.
 *
 * This object carries the price index table transcribed verbatim from the price index
 * configuration that the Java implementation located on the classpath and parsed on first use.
 * Nothing is located, parsed or cached at run time here: the nine rows below are Scala literals
 * fixed at compile time, so the table cannot fail to initialise, needs no classpath, recovers
 * from no load failure and holds no hidden global state. Where the Java loader logged a severe
 * error and handed back an empty map when a row would not parse, there is no such failure mode to
 * recover from, because a row that does not typecheck does not compile.
 *
 * It holds data only and no behaviour: it constructs no [[PriceIndex]], because creating the
 * members of that family - and giving them the day count, the lookups and the observation
 * machinery an index needs - belongs to `Index.scala`, which reads [[rows]] to do it. The
 * dependency runs one way, from this object towards the currency, country and frequency types it
 * names, and from `Index.scala` towards this object; nothing in this file refers to any other
 * member of the index package.
 *
 * Invariants, all pinned row for row against the Java captured reference data manifest by
 * `ReferenceDataManifestSpec`, so a self consistent but mistranscribed row cannot pass:
 *
 *  - [[rows]] holds exactly nine entries with distinct names, in the declaration order of the
 *    original data: the three sterling indices, then the Swiss, the two European, the Japanese,
 *    the United States and the French index.
 *  - Every row is active, and every row is published monthly. Both are properties of the data as
 *    it stands rather than constraints on the type: the row type admits a discontinued index and
 *    any publication frequency, as the original column did, so an index that is later
 *    discontinued is a change of one literal.
 *  - The three sterling indices measure the United Kingdom, the Swiss index Switzerland, the two
 *    European indices the `EU` region rather than any member state, the Japanese index Japan and
 *    the United States index the United States. The French index is the one row whose region is
 *    not the region of its currency: it is quoted in euro and measures France.
 *
 * There is no alias table for this family. The Java configuration declared an alternate name
 * section for price indices, but every entry in it was commented out, so no alternate name
 * resolved to an index and none is introduced here - reviving one would add behaviour the
 * original did not have. There is likewise no upper case key space in [[byName]]: registering
 * each name twice was how the Java registry answered a case insensitive lookup, and that
 * responsibility now belongs to the named enum support in `strata-collect`, which derives the
 * upper case view from the canonical one.
 *
 * No index may be added to, removed from or edited in this table: it is a transcription of
 * existing reference data, and introducing a new index is outside the scope of the port.
 *
 * All members are immutable values, so this object is thread-safe.
 */
private[basics] object PriceIndexData {

  /**
   * The nine transcribed price index rows, in the declaration order of the original data.
   *
   * The order is observable through any iteration a consumer performs - notably the order in
   * which `Index.scala` creates the members of the price index family - so it is kept stable and
   * deterministic rather than re-sorted. The columns appear in the order the original data
   * declared them, so the literal list can be diffed line for line against it.
   */
  val rows: Vector[PriceIndexRow] = Vector(
    PriceIndexRow("GB-HICP", Currency.GBP, Country.GB, active = true, publicationFrequency = Frequency.P1M),
    PriceIndexRow("GB-RPI", Currency.GBP, Country.GB, active = true, publicationFrequency = Frequency.P1M),
    PriceIndexRow("GB-RPIX", Currency.GBP, Country.GB, active = true, publicationFrequency = Frequency.P1M),
    PriceIndexRow("CH-CPI", Currency.CHF, Country.CH, active = true, publicationFrequency = Frequency.P1M),
    PriceIndexRow("EU-AI-CPI", Currency.EUR, Country.EU, active = true, publicationFrequency = Frequency.P1M),
    PriceIndexRow("EU-EXT-CPI", Currency.EUR, Country.EU, active = true, publicationFrequency = Frequency.P1M),
    PriceIndexRow("JP-CPI-EXF", Currency.JPY, Country.JP, active = true, publicationFrequency = Frequency.P1M),
    PriceIndexRow("US-CPI-U", Currency.USD, Country.US, active = true, publicationFrequency = Frequency.P1M),
    PriceIndexRow("FR-EXT-CPI", Currency.EUR, Country.FR, active = true, publicationFrequency = Frequency.P1M))

  /**
   * The transcribed rows keyed by the name of the index.
   *
   * Derived from [[rows]] rather than transcribed a second time, since a second transcription
   * would be a second opportunity to be wrong, and the names are distinct so the derivation loses
   * nothing. The keys are the canonical names alone: a lookup that folds case, or that follows an
   * alternate name, is the business of the named enum support rather than of this table.
   */
  val byName: Map[String, PriceIndexRow] = rows.iterator.map(row => row.name -> row).toMap
}
