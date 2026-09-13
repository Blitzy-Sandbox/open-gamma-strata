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
 * The table holds 251 rows, each mapping one ISO-3166-1 alpha-3 three letter code to the
 * ISO-3166-1 alpha-2 two letter code that names the same country - `GBR -> GB` reads as "the
 * country whose three letter code is GBR is the country whose two letter code is GB". The rows
 * are Scala literals fixed at compile time, so nothing is loaded, parsed or resolved while the
 * program runs: the table cannot fail to initialise and holds no hidden global state.
 *
 * The rows are written in ascending order of alpha-2 code, so that the literal list can be read
 * and checked in one order. [[alpha3ToAlpha2]] re-sorts them by alpha-3 key, as its type
 * requires.
 *
 * The relation is a bijection: the 251 alpha-3 keys and the 251 alpha-2 values are each distinct.
 * That is what makes the inverse well defined, and it is why [[alpha2ToAlpha3]] is derived from
 * [[alpha3ToAlpha2]] rather than transcribed a second time - a second transcription would be a
 * second opportunity to be wrong. The same reasoning applies to [[alpha3Codes]] and
 * [[alpha2Codes]], which are views of the single transcription.
 *
 * Two historical rows are held deliberately, because the alpha-3 lookups are defined over the
 * whole table: `ANT -> AN` (Netherlands Antilles) and `SCG -> CS` (Serbia and Montenegro). Just
 * as deliberately, `EU` is absent: it is not an ISO-3166 alpha-2 value, so the `Country`
 * constant for Europe has no alpha-3 code.
 *
 * The published members keep their sorted types because their order is part of what they publish:
 * `Country.availableCountries` iterates [[alpha2Codes]] in it, and a reader listing or checking
 * the codes reads them in it.
 *
 * The two directions a caller actually looks up - alpha-3 to alpha-2 and back - are therefore
 * served by [[alpha2CodeOf]] and [[alpha3CodeOf]], which read private hash indexes built from the
 * published maps and sitting beside them. The published tables carry the order; the indexes
 * answer in constant time; neither can disagree with the other, because both come from the one
 * transcription.
 *
 * All members are immutable values, and this object is therefore thread-safe.
 */
object CountryData {

  /**
   * The 251 alpha-3 to alpha-2 rows, in ascending order of alpha-2 code.
   *
   * Held privately as a sequence of literal pairs so that the public maps and sets below are all
   * derived from one transcription. Callers use the named members instead.
   *
   * This is a temporary container rather than a retained value: it is a method, evaluated exactly
   * once by [[alpha3ToAlpha2]], so the Vector and its 251 tuples become unreachable once that map
   * is built and are reclaimed with the rest of the initialisation garbage. Nothing after that
   * point needs them - [[alpha2ToAlpha3]], [[alpha3Codes]] and [[alpha2Codes]] are all derived
   * from the map, not from these rows - and holding them in a value would keep them alive for the
   * lifetime of the class loader for no reader.
   *
   * @return the alpha-3 to alpha-2 pairs, in ascending order of alpha-2 code
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
   * Contains all 251 rows, in ascending order of alpha-3 code. This is the published,
   * order-bearing form of the table, and it is what a reader checking the codes against the
   * standard reads. `Country.of3Char` resolves a three-letter code through [[alpha2CodeOf]],
   * which answers from a hash index of these same 251 rows, so a lookup pays no tree descent
   * while this table keeps its order.
   */
  val alpha3ToAlpha2: SortedMap[String, String] = SortedMap.from(rows)

  /**
   * The ISO-3166 alpha-2 country codes mapped to their alpha-3 equivalents, keyed by alpha-2 code.
   *
   * The inverse of [[alpha3ToAlpha2]], derived from it and therefore guaranteed to agree with it.
   * The relation is a bijection, so the inverse is total over [[alpha2Codes]] and unambiguous.
   * This is the published, order-bearing form of that inverse; `Country.code3Char` asks
   * [[alpha3CodeOf]], which answers from a hash index of these same rows, and a two-letter code
   * that is absent from them - `EU` being the notable case - has no alpha-3 equivalent to return.
   */
  val alpha2ToAlpha3: SortedMap[String, String] =
    SortedMap.from(alpha3ToAlpha2.iterator.map { case (alpha3, alpha2) => alpha2 -> alpha3 })

  /**
   * The 251 known ISO-3166 alpha-3 country codes, in ascending order.
   *
   * The key view of [[alpha3ToAlpha2]]. This set is an ordered view for iteration and for
   * comparison - listing the codes, and checking them against the standard - and a sorted set is
   * the right shape for that. It is deliberately not a membership table: asking whether a
   * three-letter code is known is `alpha2CodeOf(code).isDefined`, which is a single hash probe,
   * so no further table is added here for a question already answered in constant time.
   */
  val alpha3Codes: SortedSet[String] = SortedSet.from(alpha3ToAlpha2.keys)

  /**
   * The 251 ISO-3166 alpha-2 country codes that have an alpha-3 equivalent, in ascending order.
   *
   * The value view of [[alpha3ToAlpha2]]. Note that this is the set of codes the table holds, not
   * the set of codes a `Country` may carry: any two-letter code is a valid country code, so this
   * set is the domain of the alpha-3 conversions only.
   *
   * Like [[alpha3Codes]], this is an ordered view for iteration and for comparison, and its order
   * is load-bearing: `Country.availableCountries` iterates it to build a set ordered by code. A
   * membership test belongs on the hash index instead - `alpha3CodeOf(code).isDefined` - so this
   * set is never the thing a lookup descends, and no further table is added for it.
   */
  val alpha2Codes: SortedSet[String] = SortedSet.from(alpha3ToAlpha2.values)

  /**
   * The alpha-3 to alpha-2 rows as a hash table, held for lookup rather than for iteration.
   *
   * [[alpha3ToAlpha2]] is a `SortedMap`, which is a red-black tree while the program runs, so
   * resolving a code through it is a descent of several comparisons over the 251 rows rather than
   * one hash probe. This index answers the alpha-3 direction in constant time while leaving the
   * published table exactly as it is - the order of that table is part of what it publishes, so
   * the fast path is added beside it rather than in place of it.
   *
   * It is derived from the already-built [[alpha3ToAlpha2]], not from a second pass over [[rows]],
   * for the reason recorded above: a second transcription is a second opportunity to be wrong.
   * `iterator.toMap` over a map of this size yields a `scala.collection.immutable.HashMap` of the
   * same 251 entries, so the index cannot hold anything the published table does not.
   *
   * Being private, it is invisible outside this object, and [[alpha2CodeOf]] is its only reader.
   */
  private val alpha3ToAlpha2Index: Map[String, String] = alpha3ToAlpha2.iterator.toMap

  /**
   * The alpha-2 to alpha-3 rows as a hash table, held for lookup rather than for iteration.
   *
   * The counterpart of [[alpha3ToAlpha2Index]] for the other direction, derived from the published
   * [[alpha2ToAlpha3]] by the same reasoning and with the same relationship to it: same entries,
   * no order, constant-time `get`. [[alpha3CodeOf]] is its only reader.
   */
  private val alpha2ToAlpha3Index: Map[String, String] = alpha2ToAlpha3.iterator.toMap

  /**
   * Returns the alpha-2 code equivalent to the supplied ISO-3166 alpha-3 code.
   *
   * This is the lookup behind `Country.of3Char`, and it answers exactly what
   * `alpha3ToAlpha2.get` answers - the same value for each of the 251 known codes and nothing for
   * any other - in constant time. The argument is taken as given: nothing here checks the shape
   * of the code, because the caller has already rejected anything that is not three upper-case
   * letters, and an unknown but well formed code is a miss rather than an error.
   *
   * It is visible to this package and no wider, so the published surface of this object is the
   * four tables above and nothing else.
   *
   * @param alpha3Code  the three letter country code to resolve
   * @return the equivalent two letter code, or none if the data holds no row for it
   */
  private[location] def alpha2CodeOf(alpha3Code: String): Option[String] =
    alpha3ToAlpha2Index.get(alpha3Code)

  /**
   * Returns the alpha-3 code equivalent to the supplied ISO-3166 alpha-2 code.
   *
   * This is the lookup behind `Country.code3Char`, and it answers exactly what
   * `alpha2ToAlpha3.get` answers in constant time. A well formed two-letter code that the data
   * does not hold - `EU`, and every code outside the standard - is a miss, which is the caller's
   * cue that the country has no three letter form.
   *
   * @param alpha2Code  the two letter country code to resolve
   * @return the equivalent three letter code, or none if the data holds no row for it
   */
  private[location] def alpha3CodeOf(alpha2Code: String): Option[String] =
    alpha2ToAlpha3Index.get(alpha2Code)
}
