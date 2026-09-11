/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

/**
 * A single row of transcribed currency reference data.
 *
 * Each row corresponds to one ISO-4217 currency entry of the Java currency configuration that this
 * module replaces, and carries exactly the three properties that configuration defined:
 * the number of minor unit digits, the triangulation currency and the historic flag.
 *
 * The triangulation currency is held as a plain three letter `String` code rather than as a
 * [[Currency]]. This is deliberate and load bearing: `Currency` builds its instances *from* this
 * table, so storing a `Currency` here would make the two objects mutually dependent at
 * construction time. The Java implementation hit the same problem and documented it: its
 * `triangulationCurrency` field is a `String` because the load order of USD and EUR cannot be
 * guaranteed. `Currency` resolves the code to a `Currency` instance lazily instead.
 *
 * @param code                       the ISO-4217 three letter currency code, upper case
 * @param minorUnitDigits            the number of digits of minor units, such as 2 for cents in the dollar
 * @param triangulationCurrencyCode  the three letter code of the currency to triangulate quotes through
 * @param historic                   whether the currency is no longer in active use
 */
private[basics] final case class CurrencyRow(
    code: String,
    minorUnitDigits: Int,
    triangulationCurrencyCode: String,
    historic: Boolean)

/**
 * The currency reference data of the module, expressed as immutable Scala data.
 *
 * This object carries the currency table transcribed verbatim from the Java currency
 * configuration, together with the market convention priority ordering transcribed from the Java
 * currency priority data. Both were runtime classpath resources in the Java implementation; they
 * are compiled data here, so there is no registry to populate, no resource to locate and no load
 * failure to recover from.
 *
 * It holds data only and no behaviour. [[Currency]] builds its 74 instances from [[rows]] and
 * [[byCode]], exposes the [[nonHistoricCodes]] subset as its configured currencies, and
 * `CurrencyPair` reads [[marketConventionPriority]] to decide the base currency of a conventional
 * pair. Those member names are the published contract of this object and are kept stable.
 *
 * Invariants, all asserted row for row against the Java captured reference data manifest by
 * `ReferenceDataManifestSpec`, so a self consistent but mistranscribed row cannot pass:
 *
 *  - [[rows]] holds exactly 74 entries with distinct codes, in the order the Java configuration
 *    declared them: the active currencies alphabetically, then the metal and unapplicable
 *    currencies, then the legacy currencies replaced by the euro.
 *  - 55 rows are active and 19 are historic. The active subset is exactly the set of currencies
 *    that the Java implementation exposed as named constants and returned from
 *    `getAvailableCurrencies`. All 74 remain resolvable, so a historic currency resolves with its
 *    real minor units and triangulation currency.
 *  - `minorUnitDigits` is 0 for 12 rows, 2 for 60 rows and 3 for 2 rows.
 *  - Every `triangulationCurrencyCode` is `EUR` or `USD`, and is itself one of the 74 codes.
 *
 * No currency may be added to, removed from or edited in this table: it is a transcription of
 * existing reference data, and introducing a new currency is outside the scope of the port.
 */
private[basics] object CurrencyData {

  /**
   * The 74 transcribed currency rows, in the declaration order of the Java currency configuration.
   *
   * The order is observable through any iteration a consumer performs, so it is kept stable and
   * deterministic rather than re-sorted. The trailing comment on each row is the description the
   * Java configuration carried for that currency.
   */
  val rows: Vector[CurrencyRow] = Vector(
    // active currencies
    CurrencyRow("AED", 2, "USD", historic = false),          // United Arab Emirates
    CurrencyRow("ARS", 2, "USD", historic = false),          // Argentina
    CurrencyRow("AUD", 2, "USD", historic = false),          // Australia
    CurrencyRow("BGN", 2, "USD", historic = false),          // Bulgaria
    CurrencyRow("BHD", 3, "USD", historic = false),          // Bahrain
    CurrencyRow("BRL", 2, "USD", historic = false),          // Brazil
    CurrencyRow("CAD", 2, "USD", historic = false),          // Canada
    CurrencyRow("CHF", 2, "USD", historic = false),          // Switzerland
    CurrencyRow("CLP", 0, "USD", historic = false),          // Chile
    CurrencyRow("CNH", 2, "USD", historic = false),          // China
    CurrencyRow("CNY", 2, "USD", historic = false),          // China
    CurrencyRow("COP", 2, "USD", historic = false),          // Colombia
    CurrencyRow("CZK", 2, "USD", historic = false),          // Czech
    CurrencyRow("DKK", 2, "USD", historic = false),          // Denmark
    CurrencyRow("EGP", 2, "USD", historic = false),          // Egypt
    CurrencyRow("EUR", 2, "USD", historic = false),          // Euro
    CurrencyRow("GBP", 2, "USD", historic = false),          // United Kingdom
    CurrencyRow("HKD", 2, "USD", historic = false),          // Hong Kong
    CurrencyRow("HRK", 2, "USD", historic = false),          // Croatia
    CurrencyRow("HUF", 2, "USD", historic = false),          // Hungary
    CurrencyRow("IDR", 2, "USD", historic = false),          // Indonesia
    CurrencyRow("ILS", 2, "USD", historic = false),          // Israel
    CurrencyRow("INR", 2, "USD", historic = false),          // India
    CurrencyRow("ISK", 0, "USD", historic = false),          // Iceland
    CurrencyRow("JPY", 0, "USD", historic = false),          // Japan
    CurrencyRow("KRW", 0, "USD", historic = false),          // South Korea
    CurrencyRow("KZT", 2, "USD", historic = false),          // Kazakhstan
    CurrencyRow("MAD", 2, "USD", historic = false),          // Morocco
    CurrencyRow("MXN", 2, "USD", historic = false),          // Mexico
    CurrencyRow("MYR", 2, "USD", historic = false),          // Malaysia
    CurrencyRow("NOK", 2, "USD", historic = false),          // Norway
    CurrencyRow("NZD", 2, "USD", historic = false),          // New Zealand
    CurrencyRow("OMR", 3, "USD", historic = false),          // Oman
    CurrencyRow("PEN", 2, "USD", historic = false),          // Peru
    CurrencyRow("PHP", 2, "USD", historic = false),          // Philippines
    CurrencyRow("PKR", 2, "USD", historic = false),          // Pakistan
    CurrencyRow("PLN", 2, "USD", historic = false),          // Poland
    CurrencyRow("QAR", 2, "USD", historic = false),          // Qatar
    CurrencyRow("RON", 2, "USD", historic = false),          // Romania
    CurrencyRow("RUB", 2, "USD", historic = false),          // Russia
    CurrencyRow("SAR", 2, "USD", historic = false),          // Saudi Arabia
    CurrencyRow("SEK", 2, "USD", historic = false),          // Sweden
    CurrencyRow("SGD", 2, "USD", historic = false),          // Singapore
    CurrencyRow("THB", 2, "USD", historic = false),          // Thailand
    CurrencyRow("TRY", 2, "USD", historic = false),          // Turkey
    CurrencyRow("TWD", 2, "USD", historic = false),          // Taiwan
    CurrencyRow("UAH", 2, "USD", historic = false),          // Ukraine
    CurrencyRow("USD", 2, "USD", historic = false),          // USA
    CurrencyRow("VND", 0, "USD", historic = false),          // Vietnam
    CurrencyRow("ZAR", 2, "USD", historic = false),          // South Africa
    // X currencies
    CurrencyRow("XXX", 0, "USD", historic = false),          // No applicable currency
    CurrencyRow("XAG", 2, "USD", historic = false),          // Silver (troy ounce)
    CurrencyRow("XAU", 0, "USD", historic = false),          // Gold (troy ounce)
    CurrencyRow("XPD", 0, "USD", historic = false),          // Paladium (troy ounce)
    CurrencyRow("XPT", 0, "USD", historic = false),          // Platinum (troy ounce)
    // historic EUR currencies
    CurrencyRow("ATS", 2, "EUR", historic = true),           // Austria
    CurrencyRow("BEF", 2, "EUR", historic = true),           // Belgium
    CurrencyRow("CYP", 2, "EUR", historic = true),           // Cyprus
    CurrencyRow("DEM", 2, "EUR", historic = true),           // Germany
    CurrencyRow("EEK", 2, "EUR", historic = true),           // Estonia
    CurrencyRow("ESP", 0, "EUR", historic = true),           // Spain
    CurrencyRow("FIM", 2, "EUR", historic = true),           // Finland
    CurrencyRow("FRF", 2, "EUR", historic = true),           // France
    CurrencyRow("GRD", 2, "EUR", historic = true),           // Greece
    CurrencyRow("IEP", 2, "EUR", historic = true),           // Ireland
    CurrencyRow("ITL", 0, "EUR", historic = true),           // Italy
    CurrencyRow("LTL", 2, "EUR", historic = true),           // Lithuania
    CurrencyRow("LUF", 2, "EUR", historic = true),           // Luxembourg
    CurrencyRow("LVL", 2, "EUR", historic = true),           // Latvia
    CurrencyRow("MTL", 2, "EUR", historic = true),           // Malta
    CurrencyRow("NLG", 2, "EUR", historic = true),           // Netherlands
    CurrencyRow("PTE", 0, "EUR", historic = true),           // Portugal
    CurrencyRow("SIT", 2, "EUR", historic = true),           // Slovenia
    CurrencyRow("SKK", 2, "EUR", historic = true))           // Slovakia

  /**
   * The transcribed rows keyed by their ISO-4217 code.
   *
   * Derived from [[rows]], so the two can never disagree. Codes are distinct, so the map holds the
   * same 74 entries.
   */
  val byCode: Map[String, CurrencyRow] = rows.iterator.map(row => row.code -> row).toMap

  /**
   * The 74 currency codes, in the order of [[rows]].
   */
  val codes: Vector[String] = rows.map(_.code)

  /**
   * The 55 codes of currencies that are in active use.
   *
   * These are the currencies that the Java implementation defined as named constants and returned
   * from `getAvailableCurrencies`. Derived from [[rows]] and therefore in the same order.
   */
  val nonHistoricCodes: Vector[String] = rows.filterNot(_.historic).map(_.code)

  /**
   * The 19 codes of currencies that are no longer in active use.
   *
   * These remain resolvable with their real minor units and triangulation currency, matching the
   * Java implementation, where they were pre-seeded rather than treated as unknown codes. Derived
   * from [[rows]] and therefore in the same order.
   */
  val historicCodes: Vector[String] = rows.filter(_.historic).map(_.code)

  /**
   * The market convention priority ordering, highest priority first.
   *
   * The ordering itself is the data, so it is held as an ordered `Vector` rather than a set. It
   * decides the base currency of a conventional currency pair when the pair is not explicitly
   * configured: the currency appearing earlier is the base currency, a currency listed here
   * outranks one that is not listed at all, and two unlisted currencies fall back to
   * lexicographical ordering.
   */
  val marketConventionPriority: Vector[String] =
    Vector("XAU", "EUR", "GBP", "AUD", "NZD", "USD", "CAD", "CHF", "JPY")

  /**
   * The market convention priority of each listed currency, as a zero based index where a lower
   * value means a higher priority.
   *
   * Derived from [[marketConventionPriority]] by position, so the vector and this map can never
   * disagree. The Java implementation numbered the same ordering from one; only the relative order
   * is observable, so the base of the index is immaterial.
   */
  val marketConventionPriorityIndex: Map[String, Int] = marketConventionPriority.zipWithIndex.toMap
}
