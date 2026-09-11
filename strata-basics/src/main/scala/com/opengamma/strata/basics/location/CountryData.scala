/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.location

import scala.collection.immutable.{SortedMap, SortedSet}

/**
 * The ISO-3166 country code table, expressed as compile-time data.
 *
 * This object replaces the classpath resource that the Java implementation read at
 * `Country.java:45-50`, where a memoised Guava supplier loaded
 * `com/opengamma/strata/basics/location/country` through `ResourceLocator` and `PropertiesFile`
 * and parsed it into an `ImmutableBiMap`. Nothing is loaded, parsed or looked up at run time
 * here: the 251 rows below are Scala literals fixed at compile time, so the table cannot fail to
 * initialise, needs no classpath and holds no hidden global state.
 *
 * The rows are transcribed in the order of the original data, which is ascending by alpha-2 code,
 * so that the literal list can be diffed line for line against it. [[alpha3ToAlpha2]] re-sorts
 * them by alpha-3 key, as its type requires.
 *
 * The relation is a bijection: the 251 alpha-3 keys and the 251 alpha-2 values are each distinct.
 * That is what made the Java `ImmutableBiMap.inverse()` well defined, and it is why
 * [[alpha2ToAlpha3]] is derived from [[alpha3ToAlpha2]] rather than transcribed a second time -
 * a second transcription would be a second opportunity to be wrong. The same reasoning applies to
 * [[alpha3Codes]] and [[alpha2Codes]], which are views of the single transcription.
 *
 * Two historical rows are deliberately retained because the Java table contains them and the
 * alpha-3 lookups are specified against that table: `ANT -> AN` (Netherlands Antilles) and
 * `SCG -> CS` (Serbia and Montenegro). Just as deliberately, `EU` is absent - it is not an
 * ISO-3166 alpha-2 value in the source data, so the `Country` constant for Europe has no alpha-3
 * code, exactly as in Java.
 *
 * Every row is pinned against the Java-captured reference-data manifest by
 * `ReferenceDataManifestSpec`, which compares this table with the manifest row for row, so a
 * mistranscribed row is caught independently of any self-consistency check.
 *
 * All members are immutable values, and this object is therefore thread-safe.
 */
object CountryData {

  /**
   * The 251 alpha-3 to alpha-2 rows, in the order of the original data.
   *
   * Held privately as a sequence of literal pairs so that the public maps and sets below are all
   * derived from one transcription. Callers use the named members instead.
   *
   * This is a temporary container rather than a retained value: it is a method, evaluated exactly
   * once by [[alpha3ToAlpha2]], so the Vector and its 251 tuples become unreachable as soon as
   * that map is built and are reclaimed with the rest of the initialisation garbage. Nothing after
   * that point needs them - [[alpha2ToAlpha3]], [[alpha3Codes]] and [[alpha2Codes]] are all
   * derived from the map, not from these rows - and holding them in a value would keep them alive
   * for the lifetime of the class loader for no reader.
   *
   * @return the alpha-3 to alpha-2 pairs of the original data, in its own order
   */
  private def rows: Vector[(String, String)] = Vector(
    "AND" -> "AD",
    "ARE" -> "AE",
    "AFG" -> "AF",
    "ATG" -> "AG",
    "AIA" -> "AI",
    "ALB" -> "AL",
    "ARM" -> "AM",
    "ANT" -> "AN",
    "AGO" -> "AO",
    "ATA" -> "AQ",
    "ARG" -> "AR",
    "ASM" -> "AS",
    "AUT" -> "AT",
    "AUS" -> "AU",
    "ABW" -> "AW",
    "ALA" -> "AX",
    "AZE" -> "AZ",
    "BIH" -> "BA",
    "BRB" -> "BB",
    "BGD" -> "BD",
    "BEL" -> "BE",
    "BFA" -> "BF",
    "BGR" -> "BG",
    "BHR" -> "BH",
    "BDI" -> "BI",
    "BEN" -> "BJ",
    "BLM" -> "BL",
    "BMU" -> "BM",
    "BRN" -> "BN",
    "BOL" -> "BO",
    "BES" -> "BQ",
    "BRA" -> "BR",
    "BHS" -> "BS",
    "BTN" -> "BT",
    "BVT" -> "BV",
    "BWA" -> "BW",
    "BLR" -> "BY",
    "BLZ" -> "BZ",
    "CAN" -> "CA",
    "CCK" -> "CC",
    "COD" -> "CD",
    "CAF" -> "CF",
    "COG" -> "CG",
    "CHE" -> "CH",
    "CIV" -> "CI",
    "COK" -> "CK",
    "CHL" -> "CL",
    "CMR" -> "CM",
    "CHN" -> "CN",
    "COL" -> "CO",
    "CRI" -> "CR",
    "SCG" -> "CS",
    "CUB" -> "CU",
    "CPV" -> "CV",
    "CUW" -> "CW",
    "CXR" -> "CX",
    "CYP" -> "CY",
    "CZE" -> "CZ",
    "DEU" -> "DE",
    "DJI" -> "DJ",
    "DNK" -> "DK",
    "DMA" -> "DM",
    "DOM" -> "DO",
    "DZA" -> "DZ",
    "ECU" -> "EC",
    "EST" -> "EE",
    "EGY" -> "EG",
    "ESH" -> "EH",
    "ERI" -> "ER",
    "ESP" -> "ES",
    "ETH" -> "ET",
    "FIN" -> "FI",
    "FJI" -> "FJ",
    "FLK" -> "FK",
    "FSM" -> "FM",
    "FRO" -> "FO",
    "FRA" -> "FR",
    "GAB" -> "GA",
    "GBR" -> "GB",
    "GRD" -> "GD",
    "GEO" -> "GE",
    "GUF" -> "GF",
    "GGY" -> "GG",
    "GHA" -> "GH",
    "GIB" -> "GI",
    "GRL" -> "GL",
    "GMB" -> "GM",
    "GIN" -> "GN",
    "GLP" -> "GP",
    "GNQ" -> "GQ",
    "GRC" -> "GR",
    "SGS" -> "GS",
    "GTM" -> "GT",
    "GUM" -> "GU",
    "GNB" -> "GW",
    "GUY" -> "GY",
    "HKG" -> "HK",
    "HMD" -> "HM",
    "HND" -> "HN",
    "HRV" -> "HR",
    "HTI" -> "HT",
    "HUN" -> "HU",
    "IDN" -> "ID",
    "IRL" -> "IE",
    "ISR" -> "IL",
    "IMN" -> "IM",
    "IND" -> "IN",
    "IOT" -> "IO",
    "IRQ" -> "IQ",
    "IRN" -> "IR",
    "ISL" -> "IS",
    "ITA" -> "IT",
    "JEY" -> "JE",
    "JAM" -> "JM",
    "JOR" -> "JO",
    "JPN" -> "JP",
    "KEN" -> "KE",
    "KGZ" -> "KG",
    "KHM" -> "KH",
    "KIR" -> "KI",
    "COM" -> "KM",
    "KNA" -> "KN",
    "PRK" -> "KP",
    "KOR" -> "KR",
    "KWT" -> "KW",
    "CYM" -> "KY",
    "KAZ" -> "KZ",
    "LAO" -> "LA",
    "LBN" -> "LB",
    "LCA" -> "LC",
    "LIE" -> "LI",
    "LKA" -> "LK",
    "LBR" -> "LR",
    "LSO" -> "LS",
    "LTU" -> "LT",
    "LUX" -> "LU",
    "LVA" -> "LV",
    "LBY" -> "LY",
    "MAR" -> "MA",
    "MCO" -> "MC",
    "MDA" -> "MD",
    "MNE" -> "ME",
    "MAF" -> "MF",
    "MDG" -> "MG",
    "MHL" -> "MH",
    "MKD" -> "MK",
    "MLI" -> "ML",
    "MMR" -> "MM",
    "MNG" -> "MN",
    "MAC" -> "MO",
    "MNP" -> "MP",
    "MTQ" -> "MQ",
    "MRT" -> "MR",
    "MSR" -> "MS",
    "MLT" -> "MT",
    "MUS" -> "MU",
    "MDV" -> "MV",
    "MWI" -> "MW",
    "MEX" -> "MX",
    "MYS" -> "MY",
    "MOZ" -> "MZ",
    "NAM" -> "NA",
    "NCL" -> "NC",
    "NER" -> "NE",
    "NFK" -> "NF",
    "NGA" -> "NG",
    "NIC" -> "NI",
    "NLD" -> "NL",
    "NOR" -> "NO",
    "NPL" -> "NP",
    "NRU" -> "NR",
    "NIU" -> "NU",
    "NZL" -> "NZ",
    "OMN" -> "OM",
    "PAN" -> "PA",
    "PER" -> "PE",
    "PYF" -> "PF",
    "PNG" -> "PG",
    "PHL" -> "PH",
    "PAK" -> "PK",
    "POL" -> "PL",
    "SPM" -> "PM",
    "PCN" -> "PN",
    "PRI" -> "PR",
    "PSE" -> "PS",
    "PRT" -> "PT",
    "PLW" -> "PW",
    "PRY" -> "PY",
    "QAT" -> "QA",
    "REU" -> "RE",
    "ROU" -> "RO",
    "SRB" -> "RS",
    "RUS" -> "RU",
    "RWA" -> "RW",
    "SAU" -> "SA",
    "SLB" -> "SB",
    "SYC" -> "SC",
    "SDN" -> "SD",
    "SWE" -> "SE",
    "SGP" -> "SG",
    "SHN" -> "SH",
    "SVN" -> "SI",
    "SJM" -> "SJ",
    "SVK" -> "SK",
    "SLE" -> "SL",
    "SMR" -> "SM",
    "SEN" -> "SN",
    "SOM" -> "SO",
    "SUR" -> "SR",
    "SSD" -> "SS",
    "STP" -> "ST",
    "SLV" -> "SV",
    "SXM" -> "SX",
    "SYR" -> "SY",
    "SWZ" -> "SZ",
    "TCA" -> "TC",
    "TCD" -> "TD",
    "ATF" -> "TF",
    "TGO" -> "TG",
    "THA" -> "TH",
    "TJK" -> "TJ",
    "TKL" -> "TK",
    "TLS" -> "TL",
    "TKM" -> "TM",
    "TUN" -> "TN",
    "TON" -> "TO",
    "TUR" -> "TR",
    "TTO" -> "TT",
    "TUV" -> "TV",
    "TWN" -> "TW",
    "TZA" -> "TZ",
    "UKR" -> "UA",
    "UGA" -> "UG",
    "UMI" -> "UM",
    "USA" -> "US",
    "URY" -> "UY",
    "UZB" -> "UZ",
    "VAT" -> "VA",
    "VCT" -> "VC",
    "VEN" -> "VE",
    "VGB" -> "VG",
    "VIR" -> "VI",
    "VNM" -> "VN",
    "VUT" -> "VU",
    "WLF" -> "WF",
    "WSM" -> "WS",
    "YEM" -> "YE",
    "MYT" -> "YT",
    "ZAF" -> "ZA",
    "ZMB" -> "ZM",
    "ZWE" -> "ZW"
  )

  /**
   * The ISO-3166 alpha-3 country codes mapped to their alpha-2 equivalents, keyed by alpha-3 code.
   *
   * Contains all 251 rows of the original data. `Country.of3Char` resolves a three-letter code
   * through this map.
   */
  val alpha3ToAlpha2: SortedMap[String, String] = SortedMap.from(rows)

  /**
   * The ISO-3166 alpha-2 country codes mapped to their alpha-3 equivalents, keyed by alpha-2 code.
   *
   * The inverse of [[alpha3ToAlpha2]], derived from it and therefore guaranteed to agree with it.
   * The relation is a bijection, so the inverse is total over [[alpha2Codes]] and unambiguous,
   * which is what the Java `ImmutableBiMap.inverse()` provided. `Country.code3Char` reads this
   * map, and a two-letter code that is absent from it - `EU` being the notable case - has no
   * alpha-3 equivalent to return.
   */
  val alpha2ToAlpha3: SortedMap[String, String] =
    SortedMap.from(alpha3ToAlpha2.iterator.map { case (alpha3, alpha2) => alpha2 -> alpha3 })

  /**
   * The 251 known ISO-3166 alpha-3 country codes, in ascending order.
   *
   * The key view of [[alpha3ToAlpha2]].
   */
  val alpha3Codes: SortedSet[String] = SortedSet.from(alpha3ToAlpha2.keys)

  /**
   * The 251 ISO-3166 alpha-2 country codes that have an alpha-3 equivalent, in ascending order.
   *
   * The value view of [[alpha3ToAlpha2]]. Note that this is the set of codes present in the
   * original data, not the set of codes a `Country` may carry: any two-letter code is a valid
   * country code, so this set is the domain of the alpha-3 conversions only.
   */
  val alpha2Codes: SortedSet[String] = SortedSet.from(alpha3ToAlpha2.values)
}
