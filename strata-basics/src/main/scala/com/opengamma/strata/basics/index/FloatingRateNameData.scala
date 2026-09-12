/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import java.util.Locale

import scala.collection.immutable.ListMap

import com.opengamma.strata.basics.currency.Currency

/**
 * A single transcribed row of the floating rate name reference data.
 *
 * A row is one published entry of that data: the external name a floating rate is known by, the
 * name of the index family that external name resolves to, the kind of rate the family describes,
 * and the non-standard fixing date offset the entry declares, where it declares one.
 *
 * The external name is the published identifier, and it is the contract with the outside world:
 * it is the text that arrives in an FpML message or an ISDA definition, so it is transcribed
 * exactly as published, including the spaces that several names contain - `USD-Federal Funds`,
 * `DKK-CIBOR-Reference Banks` and `EUR-EuroSTR-OIS Compound` among them. Two external names even
 * contain an equals sign, `CNY-CNREPOFIX=CFXS-Reuters` and `HKD-HIBOR-HIBOR=`, which the parser of
 * the Java implementation admitted because it split a published line on the first ` = ` rather
 * than on the first `=`. Nothing here may be tidied: a name that is corrected stops matching the
 * messages it exists to match.
 *
 * Many external names share one index name. The 159 Ibor rows resolve onto 31 index families -
 * five spellings of CHF LIBOR all name `CHF-LIBOR-` - which is deliberate, and is how a family
 * recognises its own variants: the canonical row of a family is the one whose external name equals
 * its index name, and every other row of the family is a variant that normalises onto it.
 *
 * @param externalName  the published name of the floating rate, such as `GBP-LIBOR-BBA`, exactly
 *   as published
 * @param indexName  the name of the index family, already carrying the trailing `-` of an Ibor
 *   family so that a tenor completes it, as in `GBP-LIBOR-` plus `3M`
 * @param rateType  the kind of rate the family describes, which decides the kind of index the
 *   family converts to
 * @param fixingDateOffsetDays  the number of days of the non-standard fixing date offset the row
 *   declares, or nothing when the row declares none and the offset of the index applies
 */
final case class FloatingRateNameRow(
    externalName: String,
    indexName: String,
    rateType: FloatingRateType,
    fixingDateOffsetDays: Option[Int])

/**
 * The floating rate name reference data of this module, expressed as immutable Scala data.
 *
 * This object carries the whole of the published floating rate name table: 351 name rows over
 * four kinds of rate, the three rows that declare a non-standard fixing date offset, and the two
 * tables that name the default Ibor and Overnight rate of a currency - 404 rows in total. In the
 * Java implementation that table was a classpath resource, read and parsed at class-initialisation
 * time by a loader that caught and logged any failure and left the registry empty when one
 * occurred. Here the rows are Scala literals fixed at compile time: there is no resource to
 * locate, no text to parse, no registry to populate and no load failure to recover from, so the
 * table cannot be absent, partial or overridden at run time.
 *
 * ===Sections and counts===
 *
 * The published data is divided into seven sections, and this object keeps that division because
 * the section a row belongs to is what decides how the row is read:
 *
 *  - 159 Ibor rows, in [[iborRows]]. The published value of an Ibor row is the stem of an index
 *    name, and the loader of the Java implementation appended a `-` to it, because an Ibor index
 *    name is completed by a tenor. [[iborRows]] carries the appended form, so no consumer repeats
 *    that step.
 *  - 3 fixing date offset rows, in [[iborFixingDateOffsets]], which also reach [[iborRows]] as the
 *    `fixingDateOffsetDays` of the three rows they name.
 *  - 156 Overnight compounded rows, in [[overnightCompoundedRows]], 6 Overnight averaged rows, in
 *    [[overnightAveragedRows]], and 30 price rows, in [[priceRows]]. The published value of a row
 *    of these three sections is a complete index name and is transcribed unchanged.
 *  - 23 default Ibor rows and 27 default Overnight rows, in [[currencyDefaultIbor]] and
 *    [[currencyDefaultOvernight]].
 *
 * Rows are held in their published order, within each section and across the sections, so that the
 * literals below can be read against the published data line for line. The published comments that
 * identify the ISDA 2021 and FpML codes of a family are carried over with the rows they annotate,
 * for the same reason.
 *
 * ===Invariants, and what pins them===
 *
 * Every row of every table is asserted against the Java-captured reference data manifest by
 * `ReferenceDataManifestSpec`, section by section, so a row that is self-consistent but
 * mistranscribed is caught independently of any check this object could make on itself. The
 * invariants are:
 *
 *  - the section sizes above, 159 + 3 + 156 + 6 + 30 + 23 + 27 = 404 rows;
 *  - the 351 name rows have 351 distinct external names, within each section and across all four,
 *    which is what makes [[byExternalName]] lossless and total over [[rows]];
 *  - every Ibor index name ends with `-` and no other index name does;
 *  - every value of [[currencyDefaultIbor]] and [[currencyDefaultOvernight]] is the external name
 *    of a row of [[rows]], which is what lets a default be resolved through [[byExternalName]].
 *
 * Two published rows look like mistakes and are not. `USD-EFFECTIVE` is an Overnight averaged row
 * whose index name is `USD-FED-FUND`, while every other row of that section names
 * `USD-FED-FUND-AVG`; the conversion to an Overnight index strips a trailing `-AVG`, so the two
 * index names denote the same index and both spellings are needed. And `SAR-SAIBOR-OIS Compound`
 * is an Overnight compounded row naming `SAR-SAIBOR`, which is also the index name of an Ibor
 * family, because the published Saudi rate covers both a term and an overnight rate under one
 * name.
 *
 * ===What this object does not do===
 *
 * It holds data and no behaviour. It constructs no [[FloatingRateName]] and refers to no index
 * type: `FloatingRateName` is what reads these tables and builds its members from them, and the
 * dependency runs in that direction only, so that neither object can be caught part-initialised by
 * the other. For the same reason the currency default tables map a [[Currency]] to the external
 * name of a rate rather than to a rate, leaving the resolution to the consumer that owns the
 * members. The lookup routes it does offer - [[fixingDateOffsetOf]],
 * [[defaultIborExternalNameOf]] and [[defaultOvernightExternalNameOf]], all three visible to this
 * package alone - construct nothing either: each answers a key with the published text of a row,
 * and each exists because the table published beside it is ordered for the manifest comparison and
 * an ordered map answers a key by walking to it.
 *
 * There is also no table of alternate, lenient or external-group names here, and that is not an
 * omission: the configuration of the Java implementation declares none for this family. Its 404
 * published rows are the whole of the name space, and the upper-case spelling of each external
 * name resolves through the key space that `NamedEnum` builds for a named family rather than
 * through a table transcribed here.
 *
 * All members are immutable values, and this object is therefore thread-safe.
 */
object FloatingRateNameData {

  // The name sections are transcribed in several methods rather than one. The split has to be
  // into methods to have any effect on the generated code: the initialiser of every `val` of an
  // object is emitted into that object's single constructor method, so chunking into values would
  // leave one method holding every row and approaching the per-method size limit of the virtual
  // machine. Each method below stays well inside that limit, and every boundary falls between two
  // published families, so each method is still a contiguous run of published rows. Keep them
  // separate.

  /**
   * The Ibor rows of the CHF LIBOR, EUR LIBOR, EUR EURIBOR, GBP LIBOR, JPY LIBOR, JPY TIBOR and
   * JPY Euroyen TIBOR families, in published order.
   *
   * @return the 45 rows, each mapping a published external name to its published value
   */
  private def iborPairs1: Vector[(String, String)] = Vector(
    // "CHF-LIBOR" is the ISDA 2021 code
    "CHF-LIBOR" -> "CHF-LIBOR",
    "CHF-LIBOR-BBA" -> "CHF-LIBOR",
    "CHF-LIBOR-BBA-Bloomberg" -> "CHF-LIBOR",
    "CHF-LIBOR-ISDA" -> "CHF-LIBOR",
    "CHF-LIBOR-Reference Banks" -> "CHF-LIBOR",

    // "EUR-LIBOR" is the ISDA 2021 code
    "EUR-LIBOR" -> "EUR-LIBOR",
    "EUR-LIBOR-BBA" -> "EUR-LIBOR",
    "EUR-LIBOR-BBA-Bloomberg" -> "EUR-LIBOR",
    "EUR-LIBOR-Reference Banks" -> "EUR-LIBOR",

    // "EUR-EURIBOR" is the ISDA 2021 code
    "EUR-EURIBOR" -> "EUR-EURIBOR",
    "EUR-EURIBOR-Act/365" -> "EUR-EURIBOR",
    "EUR-EURIBOR-Act/365-Bloomberg" -> "EUR-EURIBOR",
    "EUR-EURIBOR-Reference Banks" -> "EUR-EURIBOR",
    "EUR-EURIBOR-Reuters" -> "EUR-EURIBOR",
    "EUR-EURIBOR-Telerate" -> "EUR-EURIBOR",

    // "GBP-LIBOR" is the ISDA 2021 code
    "GBP-LIBOR" -> "GBP-LIBOR",
    "GBP-LIBOR-BBA" -> "GBP-LIBOR",
    "GBP-LIBOR-BBA-Bloomberg" -> "GBP-LIBOR",
    "GBP-LIBOR-ISDA" -> "GBP-LIBOR",
    "GBP-LIBOR-Reference Banks" -> "GBP-LIBOR",

    // "JPY-LIBOR" is the ISDA 2021 code
    "JPY-LIBOR" -> "JPY-LIBOR",
    "JPY-LIBOR-BBA" -> "JPY-LIBOR",
    "JPY-LIBOR-BBA-Bloomberg" -> "JPY-LIBOR",
    "JPY-LIBOR-FRASETT" -> "JPY-LIBOR",
    "JPY-LIBOR-ISDA" -> "JPY-LIBOR",
    "JPY-LIBOR-Reference Banks" -> "JPY-LIBOR",

    // "JPY-TIBOR" is the ISDA 2021 code
    "JPY-TIBOR-JAPAN" -> "JPY-TIBOR-JAPAN",
    "JPY-TIBORJAPAN" -> "JPY-TIBOR-JAPAN",
    "JPY-TIBOR-TIBM" -> "JPY-TIBOR-JAPAN",
    "JPY-TIBOR-TIBM (5 Banks)" -> "JPY-TIBOR-JAPAN",
    "JPY-TIBOR-TIBM (10 Banks)" -> "JPY-TIBOR-JAPAN",
    "JPY-TIBOR-TIBM (All Banks)" -> "JPY-TIBOR-JAPAN",
    "JPY-TIBOR-TIBM (All Banks)-Bloomberg" -> "JPY-TIBOR-JAPAN",
    "JPY-TIBOR-TIBM-Reference Banks" -> "JPY-TIBOR-JAPAN",
    "JPY-TIBOR-17096" -> "JPY-TIBOR-JAPAN",
    "JPY-TIBOR-17097" -> "JPY-TIBOR-JAPAN",
    "JPY-DTIBOR" -> "JPY-TIBOR-JAPAN",
    "JPY-D-TIBOR" -> "JPY-TIBOR-JAPAN",
    "JPY-TIBOR" -> "JPY-TIBOR-JAPAN",

    // "JPY-Euroyen TIBOR" is the ISDA 2021 code
    "JPY-TIBOR-EUROYEN" -> "JPY-TIBOR-EUROYEN",
    "JPY-TIBOREUROYEN" -> "JPY-TIBOR-EUROYEN",
    "JPY-TIBOR-ZTIBOR" -> "JPY-TIBOR-EUROYEN",
    "JPY-ZTIBOR" -> "JPY-TIBOR-EUROYEN",
    "JPY-Z-TIBOR" -> "JPY-TIBOR-EUROYEN",
    "JPY-Euroyen TIBOR" -> "JPY-TIBOR-EUROYEN"
  )

  /**
   * The Ibor rows of the USD LIBOR, USD BSBY, USD AMERIBOR Term, AUD BBSW, CAD CDOR, CNY REPO, CZK
   * PRIBOR, DKK CIBOR and HKD HIBOR families, in published order.
   *
   * @return the 43 rows, each mapping a published external name to its published value
   */
  private def iborPairs2: Vector[(String, String)] = Vector(
    // "USD-LIBOR" is the ISDA 2021 code
    "USD-LIBOR" -> "USD-LIBOR",
    "USD-LIBOR-BBA" -> "USD-LIBOR",
    "USD-LIBOR-BBA-Bloomberg" -> "USD-LIBOR",
    "USD-LIBOR-ISDA" -> "USD-LIBOR",
    "USD-LIBOR-LIBO" -> "USD-LIBOR",
    "USD-LIBOR-Reference Banks" -> "USD-LIBOR",

    // "USD-BSBY" is the ISDA 2021 code
    "USD-BSBY" -> "USD-BSBY",
    "USD-BSBY-Bloomberg" -> "USD-BSBY",

    // "USD-AMERIBOR Term" is the ISDA 2021 code
    "USD-AMERIBORTERM" -> "USD-AMERIBORTERM",
    "USD-AMERIBOR Term" -> "USD-AMERIBORTERM",

    // "AUD-BBSW" is the ISDA 2021 code
    "AUD-BBSW" -> "AUD-BBSW",
    "AUD-BBR-AUBBSW" -> "AUD-BBSW",
    "AUD-BBR-BBSW" -> "AUD-BBSW",
    "AUD-BBR-BBSW-Bloomberg" -> "AUD-BBSW",
    "AUD-BBR" -> "AUD-BBSW",
    "AUD-BANKBILL" -> "AUD-BBSW",

    // "CAD-CDOR" is the ISDA 2021 code
    "CAD-CDOR" -> "CAD-CDOR",
    "CAD-BA-CDOR" -> "CAD-CDOR",
    "CAD-BA-CDOR-Bloomberg" -> "CAD-CDOR",
    "CAD-BA" -> "CAD-CDOR",
    "CAD-CBA" -> "CAD-CDOR",

    // "CNY-CNREPOFIX" may be the ISDA 2021 code (sources differ)
    "CNY-REPO" -> "CNY-REPO",
    "CNY-CNREPOFIX=CFXS-Reuters" -> "CNY-REPO",
    "CNY-CFRR" -> "CNY-REPO",
    "CNY-REPOFIX" -> "CNY-REPO",
    "CNY-CNREPOFIX" -> "CNY-REPO",

    // "CZK-PRIBOR" is the ISDA 2021 code
    "CZK-PRIBOR" -> "CZK-PRIBOR",
    "CZK-PRIBOR-PRBO" -> "CZK-PRIBOR",
    "CZK-PRIBOR-Reference Banks" -> "CZK-PRIBOR",

    // "DKK-CIBOR"/"DKK-CIBOR2" are the ISDA 2021 codes
    // We model the index, not the floating rate, thus CIBOR and CIBOR2 are the same
    "DKK-CIBOR" -> "DKK-CIBOR",
    "DKK-CIBOR-DKNA13" -> "DKK-CIBOR",
    "DKK-CIBOR-DKNA13-Bloomberg" -> "DKK-CIBOR",
    "DKK-CIBOR-Reference Banks" -> "DKK-CIBOR",
    "DKK-CIBOR2-DKNA13" -> "DKK-CIBOR",
    "DKK-CIBOR2" -> "DKK-CIBOR",

    // "HKD-HIBOR" is the ISDA 2021 code
    "HKD-HIBOR" -> "HKD-HIBOR",
    "HKD-HIBOR-HIBOR" -> "HKD-HIBOR",
    "HKD-HIBOR-HIBOR=" -> "HKD-HIBOR",
    "HKD-HIBOR-HIBOR-Bloomberg" -> "HKD-HIBOR",
    "HKD-HIBOR-HKAB" -> "HKD-HIBOR",
    "HKD-HIBOR-HKAB-Bloomberg" -> "HKD-HIBOR",
    "HKD-HIBOR-ISDC" -> "HKD-HIBOR",
    "HKD-HIBOR-Reference Banks" -> "HKD-HIBOR"
  )

  /**
   * The Ibor rows of the HUF BUBOR, ILS TELBOR, KRW CD, MXN TIIE, MYR KLIBOR, NOK NIBOR, NZD BKBM
   * and PLN WIBOR families, in published order.
   *
   * @return the 46 rows, each mapping a published external name to its published value
   */
  private def iborPairs3: Vector[(String, String)] = Vector(
    // "HUF-BUBOR" is the ISDA 2021 code
    "HUF-BUBOR" -> "HUF-BUBOR",
    "HUF-BUBOR-Reference Banks" -> "HUF-BUBOR",
    "HUF-BUBOR-Reuters" -> "HUF-BUBOR",

    // "ILS-TELBOR" is the ISDA 2021 code
    "ILS-TELBOR" -> "ILS-TELBOR",
    "ILS-TELBOR01-Reuters" -> "ILS-TELBOR",
    "ILS-TELBOR-Reference Banks" -> "ILS-TELBOR",

    // "KRW-CD 91D" is the ISDA 2021 code
    "KRW-CD" -> "KRW-CD",
    "KRW-CD-3220" -> "KRW-CD",
    "KRW-CD-KSDA" -> "KRW-CD",
    "KRW-CD-KSDA-Bloomberg" -> "KRW-CD",
    "KRW-CDR" -> "KRW-CD",
    "KRW-CD 91D" -> "KRW-CD",

    // "MXN-TIIE" is the ISDA 2021 code
    "MXN-TIIE" -> "MXN-TIIE",
    "MXN-TIIE-Banxico" -> "MXN-TIIE",
    "MXN-TIIE-Banxico-Bloomberg" -> "MXN-TIIE",
    "MXN-TIIE-Banxico-Reference Banks" -> "MXN-TIIE",

    // "MYR-KLIBOR" is the ISDA 2021 code
    "MYR-KLIBOR" -> "MYR-KLIBOR",
    "MYR-KLIBOR-BNM" -> "MYR-KLIBOR",
    "MYR-KLIBOR-Reference Banks" -> "MYR-KLIBOR",
    "MYR-Quarterly Swap Rate-11:00-TRADITION" -> "MYR-KLIBOR",
    "MYR-Quarterly Swap Rate-TRADITION-Reference Banks" -> "MYR-KLIBOR",

    // "NOK-NIBOR" is the ISDA 2021 code
    "NOK-NIBOR" -> "NOK-NIBOR",
    "NOK-NIBOR-NIBR" -> "NOK-NIBOR",
    "NOK-NIBOR-NIBR-Reference Banks" -> "NOK-NIBOR",
    "NOK-NIBOR-Reference Banks" -> "NOK-NIBOR",
    "NOK-NIBOR-OIBOR" -> "NOK-NIBOR",
    "NOK-NIBR" -> "NOK-NIBOR",

    // "NZD-BKBM FRA"/"NZD-BKBM Bid" are the ISDA 2021 codes
    // "NZD-BBR FRA" mentioned by another source
    // "FRA" seems to be the main one, but including "Bid" as well
    "NZD-BKBM" -> "NZD-BKBM",
    "NZD-BBR" -> "NZD-BKBM",
    "NZD-BBR-BID" -> "NZD-BKBM",
    "NZD-BBR-FRA" -> "NZD-BKBM",
    "NZD-BBR-ISDC" -> "NZD-BKBM",
    "NZD-BBR-Reference Banks" -> "NZD-BKBM",
    "NZD-BBR-Telerate" -> "NZD-BKBM",
    "NZD-BANKBILL" -> "NZD-BKBM",
    "NZD-BKBM Bid" -> "NZD-BKBM",
    "NZD-BKBM FRA" -> "NZD-BKBM",
    "NZD-BBR FRA" -> "NZD-BKBM",

    // "PLN-WIBOR" is the ISDA 2021 code
    "PLN-WIBOR" -> "PLN-WIBOR",
    "PLN-WIBOR-Reference Banks" -> "PLN-WIBOR",
    "PLN-WIBOR-WIBO" -> "PLN-WIBOR",
    "PLN-WIBR" -> "PLN-WIBOR",
    "PLZ-WIBOR" -> "PLN-WIBOR",
    "PLZ-WIBOR-Reference Banks" -> "PLN-WIBOR",
    "PLZ-WIBOR-WIBO" -> "PLN-WIBOR",
    "PLZ-WIBR" -> "PLN-WIBOR"
  )

  /**
   * The Ibor rows of the SAR SAIBOR, SEK STIBOR, SGD SIBOR, SGD SOR, THB THBFIX, TWD TAIBOR and
   * ZAR JIBAR families, in published order.
   *
   * @return the 25 rows, each mapping a published external name to its published value
   */
  private def iborPairs4: Vector[(String, String)] = Vector(
    // "SAR-SAIBOR" is the ISDA 2021 code
    "SAR-SAIBOR" -> "SAR-SAIBOR",
    "SAR-SRIOR-SUAA" -> "SAR-SAIBOR",
    "SAR-SRIOR-Reference Banks" -> "SAR-SAIBOR",

    // "SEK-STIBOR" is the ISDA 2021 code
    "SEK-STIBOR" -> "SEK-STIBOR",
    "SEK-STIBOR-Bloomberg" -> "SEK-STIBOR",
    "SEK-STIBOR-Reference Banks" -> "SEK-STIBOR",
    "SEK-STIBOR-SIDE" -> "SEK-STIBOR",

    // "SGD-SIBOR" is the ISDA 2021 code
    "SGD-SIBOR" -> "SGD-SIBOR",
    "SGD-SIBOR-Reference Banks" -> "SGD-SIBOR",
    "SGD-SIBOR-Reuters" -> "SGD-SIBOR",
    "SGD-SIBOR-Telerate" -> "SGD-SIBOR",

    // "SGD-SOR" is the ISDA 2021 code
    "SGD-SOR" -> "SGD-SOR",
    "SGD-SOR-Reuters" -> "SGD-SOR",
    "SGD-SOR-Telerate" -> "SGD-SOR",
    "SGD-SOR-VWAP" -> "SGD-SOR",
    "SGD-SOR-VWAP-Reference Banks" -> "SGD-SOR",

    // "THB-THBFIX" is the ISDA 2021 code
    "THB-THBFIX" -> "THB-THBFIX",
    "THB-THBFIX-Reuters" -> "THB-THBFIX",
    "THB-THBFIX-Reference Banks" -> "THB-THBFIX",

    // "TWD-TAIBOR" is the ISDA 2021 code
    "TWD-TAIBOR" -> "TWD-TAIBOR",
    "TWD-TAIBOR-Reuters" -> "TWD-TAIBOR",
    "TWD-TAIBOR-Bloomberg" -> "TWD-TAIBOR",

    // "ZAR-JIBAR" is the ISDA 2021 code
    "ZAR-JIBAR" -> "ZAR-JIBAR",
    "ZAR-JIBAR-Reference Banks" -> "ZAR-JIBAR",
    "ZAR-JIBAR-SAFEX" -> "ZAR-JIBAR"
  )

  /**
   * The 159 Ibor rows, in published order.
   *
   * @return the published external name to index name stem pairs of the Ibor section
   */
  private def iborPairs: Vector[(String, String)] =
    iborPairs1 ++ iborPairs2 ++ iborPairs3 ++ iborPairs4

  /**
   * The Overnight compounded rows of the CHF SARON, CHF TOIS, EUR EONIA, EUR ESTR, GBP SONIA, GBP
   * SONIA ICE Term, GBP SONIA Refinitiv Term, JPY TONAR and JPY TORF families, in published order.
   *
   * @return the 40 rows, each mapping a published external name to its published value
   */
  private def overnightCompoundedPairs1: Vector[(String, String)] = Vector(
    // "CHF-SARON-OIS-COMPOUND" is the FpML/ISDA code
    // "CHF-SARON"/"CHF-SARON-OIS Compound" are the ISDA 2021 codes
    "CHF-SARON" -> "CHF-SARON",
    "CHF-SARON-COMPOUND" -> "CHF-SARON",
    "CHF-SARON-OIS-COMPOUND" -> "CHF-SARON",
    "CHF-SARON-OIS Compound" -> "CHF-SARON",

    // "CHF-TOIS-OIS-COMPOUND" is the FpML/ISDA code
    // Not present in ISDA 2021
    "CHF-TOIS" -> "CHF-TOIS",
    "CHF-TOIS-COMPOUND" -> "CHF-TOIS",
    "CHF-TOIS-OIS-COMPOUND" -> "CHF-TOIS",

    // "EUR-EONIA-OIS-COMPOUND" is the FpML/ISDA code
    // "EUR-EONIA"/"EUR-EONIA-OIS Compound" are the ISDA 2021 codes, soon to be phased out
    "EUR-EONIA" -> "EUR-EONIA",
    "EUR-EONIA-COMPOUND" -> "EUR-EONIA",
    "EUR-EONIA-OIS-COMPOUND" -> "EUR-EONIA",
    "EUR-EONIA-OIS Compound" -> "EUR-EONIA",
    "EUR-EONIA-OIS-COMPOUND-Bloomberg" -> "EUR-EONIA",

    // "EUR-ESTR-COMPOUND" is the FpML/ISDA code
    // "EUR-EuroSTR"/"EUR-EuroSTR-OIS Compound" are the ISDA 2021 codes
    "EUR-ESTR" -> "EUR-ESTR",
    "EUR-ESTR-COMPOUND" -> "EUR-ESTR",
    "EUR-ESTR-OIS-COMPOUND" -> "EUR-ESTR",
    "EUR-ESTER" -> "EUR-ESTR",
    "EUR-ESTER-COMPOUND" -> "EUR-ESTR",
    "EUR-ESTER-OIS-COMPOUND" -> "EUR-ESTR",
    "EUR-EuroSTR" -> "EUR-ESTR",
    "EUR-EuroSTR-COMPOUND" -> "EUR-ESTR",
    "EUR-EuroSTR-OIS-COMPOUND" -> "EUR-ESTR",
    "EUR-EuroSTR-OIS Compound" -> "EUR-ESTR",

    // "GBP-SONIA-COMPOUND" is the FpML/ISDA code
    // "GBP-SONIA"/"GBP-SONIA-OIS Compound" are the ISDA 2021 codes
    "GBP-SONIA" -> "GBP-SONIA",
    "GBP-WMBA-SONIA-COMPOUND" -> "GBP-SONIA",
    "GBP-SONIA-COMPOUND" -> "GBP-SONIA",
    "GBP-SONIA-OIS-COMPOUND" -> "GBP-SONIA",
    "GBP-SONIA-OIS Compound" -> "GBP-SONIA",
    "GBP-SONIA-WMBA-COMPOUND" -> "GBP-SONIA",

    "GBP-SONIAICETERM" -> "GBP-SONIAICETERM",
    "GBP-SONIA ICE Term" -> "GBP-SONIAICETERM",
    "GBP-SONIAREFINITIVTERM" -> "GBP-SONIAREFINITIVTERM",
    "GBP-SONIA Refinitiv Term" -> "GBP-SONIAREFINITIVTERM",

    // "JPY-TONA-OIS-COMPOUND" is the FpML/ISDA code
    // "JPY-TONA"/"JPY-TONA-OIS Compound" are the ISDA 2021 codes
    "JPY-TONAR" -> "JPY-TONAR",
    "JPY-TONA-COMPOUND" -> "JPY-TONAR",
    "JPY-TONA-OIS-COMPOUND" -> "JPY-TONAR",
    "JPY-TONA-OIS Compound" -> "JPY-TONAR",
    "JPY-MUTAN" -> "JPY-TONAR",
    "JPY-TONA" -> "JPY-TONAR",

    "JPY-TORF" -> "JPY-TORF",
    "JPY-TORF QUICK" -> "JPY-TORF"
  )

  /**
   * The Overnight compounded rows of the USD Fed Fund, USD SOFR, USD SOFR CME Term, USD AMERIBOR,
   * AUD AONIA, BRL CDI, CAD CORRA, CLP TNA, COP OIBR, CZK CZEONIA and DKK TNR families, in
   * published order.
   *
   * @return the 45 rows, each mapping a published external name to its published value
   */
  private def overnightCompoundedPairs2: Vector[(String, String)] = Vector(
    // "USD-Federal Funds-H.15-OIS-COMPOUND" is the FpML/ISDA code
    // "USD-Federal Funds-OIS Compound" is the ISDA 2021 code
    "USD-FED-FUND" -> "USD-FED-FUND",
    "USD-FED-FUNDS" -> "USD-FED-FUND",
    "USD-FEDFUND" -> "USD-FED-FUND",
    "USD-FEDFUNDS" -> "USD-FED-FUND",
    "USD-Federal Funds-H.15-OIS-COMPOUND" -> "USD-FED-FUND",
    "USD-FEDFUND-H.15-OIS-COMPOUND" -> "USD-FED-FUND",
    "USD-Federal Funds-OIS Compound" -> "USD-FED-FUND",

    // "USD-SOFR-COMPOUND" is the FpML/ISDA code
    // "USD-SOFR"/"USD-SOFR-OIS Compound" are the ISDA 2021 codea
    "USD-SOFR" -> "USD-SOFR",
    "USD-SOFR-COMPOUND" -> "USD-SOFR",
    "USD-SOFR-OIS-COMPOUND" -> "USD-SOFR",
    "USD-SOFR-OIS Compound" -> "USD-SOFR",

    "USD-SOFRCMETERM" -> "USD-SOFRCMETERM",
    "USD-SOFR CME Term" -> "USD-SOFRCMETERM",

    // "USD-AMERIBOR" is the ISDA 2021 code
    "USD-AMERIBOR" -> "USD-AMERIBOR",

    // "AUD-AONIA-OIS-COMPOUND" is the FpML/ISDA code
    // "AUD-AONIA"/"AUD-AONIA-OIS Compound" are the ISDA 2021 codes
    "AUD-AONIA" -> "AUD-AONIA",
    "AUD-AONIA-COMPOUND" -> "AUD-AONIA",
    "AUD-AONIA-OIS-COMPOUND" -> "AUD-AONIA",
    "AUD-AONIA-OIS Compound" -> "AUD-AONIA",
    "AUD-CASH" -> "AUD-AONIA",
    "AUD-OCR" -> "AUD-AONIA",

    // "BRL-CDI" is the FpML/ISDA code and ISDA 2021 code
    "BRL-CDI" -> "BRL-CDI",

    // "CAD-CORRA-OIS-COMPOUND" is the FpML/ISDA code
    // "CAD-CORRA"/"CAD-CORRA-OIS Compound" are the ISDA 2021 codes
    "CAD-CORRA" -> "CAD-CORRA",
    "CAD-CORRA-COMPOUND" -> "CAD-CORRA",
    "CAD-CORRA-OIS-COMPOUND" -> "CAD-CORRA",
    "CAD-CORRA-OIS Compound" -> "CAD-CORRA",

    // "CLP-TNA" is the FpML/ISDA code
    // "CLP-ICP" is the ISDA 2021 code
    "CLP-TNA" -> "CLP-TNA",
    "CLP-CLICP" -> "CLP-TNA",
    "CLP-CLICP-Bloomberg" -> "CLP-TNA",
    "CL-CLICP-Bloomberg" -> "CLP-TNA",
    "CLP-ICP" -> "CLP-TNA",

    // "COP-IBR-OIS-COMPOUND" is the FpML/ISDA code
    // "COP-IBR-OIS Compound" is the ISDA 2021 code
    "COP-OIBR" -> "COP-OIBR",
    "COP-IBR-COMPOUND" -> "COP-OIBR",
    "COP-IBR-OIS-COMPOUND" -> "COP-OIBR",
    "COP-IBR-OIS Compound" -> "COP-OIBR",
    "COP-IBR" -> "COP-OIBR",

    // "CZK-CZEONIA"/"CZK-CZEONIA-OIS Compound" are the ISDA 2021 codes
    "CZK-CZEONIA" -> "CZK-CZEONIA",
    "CZK-CZEONIA-OIS Compound" -> "CZK-CZEONIA",

    // "DKK-DKKOIS-OIS-COMPOUND" is the FpML/ISDA code
    // "DKK-Tom Next-OIS Compound" is the ISDA 2021 code
    "DKK-TNR" -> "DKK-TNR",
    "DKK-DKKOIS-COMPOUND" -> "DKK-TNR",
    "DKK-DKKOIS-OIS-COMPOUND" -> "DKK-TNR",
    "DKK-DKKOIS" -> "DKK-TNR",
    "DKK-CITA" -> "DKK-TNR",
    "DKK-CITA-OIS-COMPOUND" -> "DKK-TNR",
    "DKK-Tom Next" -> "DKK-TNR",
    "DKK-Tom Next-OIS Compound" -> "DKK-TNR"
  )

  /**
   * The Overnight compounded rows of the DKK DESTR, HKD HONIA, HUF HUFONIA, IDR INDONIA, ILS
   * OTELBOR, INR OMIBOR, NOK NOWA, NZD NZIONA, PLN POLONIA, PLN POLSTR and RUB RUONIA families, in
   * published order.
   *
   * @return the 40 rows, each mapping a published external name to its published value
   */
  private def overnightCompoundedPairs3: Vector[(String, String)] = Vector(
    // "DKK-DESTR"/"DKK-DESTR-OIS Compound" are the ISDA 2021 codes
    "DKK-DESTR" -> "DKK-DESTR",
    "DKK-DESTR-OIS" -> "DKK-DESTR",
    "DKK-DESTR-OIS Compound" -> "DKK-DESTR",
    "DKK-DESTR-OIS COMPOUND" -> "DKK-DESTR",

    // "HKD-HONIX-OIS-COMPOUND" is the FpML/ISDA code
    // "HKD-HONIA"/"HKD-HONIA-OIS Compound" are the ISDA 2021 codes
    "HKD-HONIA" -> "HKD-HONIA",
    "HKD-HONIA-COMPOUND" -> "HKD-HONIA",
    "HKD-HONIA-OIS-COMPOUND" -> "HKD-HONIA",
    "HKD-HONIA-OIS Compound" -> "HKD-HONIA",
    "HKD-HONIX" -> "HKD-HONIA",
    "HKD-HONIX-COMPOUND" -> "HKD-HONIA",
    "HKD-HONIX-OIS-COMPOUND" -> "HKD-HONIA",

    // "HUF-HUFONIA" is the ISDA 2021 code
    "HUF-HUFONIA" -> "HUF-HUFONIA",

    // No FpML/ISDA code
    "IDR-INDONIA" -> "IDR-INDONIA",

    // No FpML/ISDA code
    // "ILS-TELBOR" is the ISDA 2021 code (probably covers Overnight and Ibor)
    "ILS-OTELBOR" -> "ILS-OTELBOR",
    "ILS-TELBOR-OIS Compound" -> "ILS-OTELBOR",

    // "INR-MIBOR-OIS-COMPOUND" is the FpML/ISDA code
    // "INR-MIBOR-OIS Compound" is the ISDA 2021 code
    "INR-OMIBOR" -> "INR-OMIBOR",
    "INR-MIBOR-COMPOUND" -> "INR-OMIBOR",
    "INR-MIBOR-OIS-COMPOUND" -> "INR-OMIBOR",
    "INR-MIBOR-OIS Compound" -> "INR-OMIBOR",
    "INR-FBIL-MIBOR-OIS-COMPOUND" -> "INR-OMIBOR",

    // "NOK-NOWA"/"NOK-NOWA-OIS Compound" are the ISDA 2021 codes
    "NOK-NOWA" -> "NOK-NOWA",
    "NOK-NOWA-OIS Compound" -> "NOK-NOWA",

    // "NZD-NZIONA-OIS-COMPOUND" is the FpML/ISDA code
    // "NZD-NZIONA"/"NZD-NZIONA-OIS Compound" are the ISDA 2021 codes
    "NZD-NZIONA" -> "NZD-NZIONA",
    "NZD-NZIONA-COMPOUND" -> "NZD-NZIONA",
    "NZD-NZIONA-OIS-COMPOUND" -> "NZD-NZIONA",
    "NZD-NZIONA-OIS Compound" -> "NZD-NZIONA",
    "NZD-CASH" -> "NZD-NZIONA",
    "NZD-OCR" -> "NZD-NZIONA",

    // "PLN-POLONIA-OIS-COMPOUND" is the FpML/ISDA code
    // "PLN-POLONIA"/"PLN-POLONIA-OIS Compound" are the ISDA 2021 codes
    "PLN-POLONIA" -> "PLN-POLONIA",
    "PLN-POLONIA-COMPOUND" -> "PLN-POLONIA",
    "PLN-POLONIA-OIS-COMPOUND" -> "PLN-POLONIA",
    "PLN-POLONIA-OIS Compound" -> "PLN-POLONIA",

    // "PLN-POLSTR-OIS-COMPOUND" is the FpML/ISDA code
    // "PLN-POLSTR"/"PLN-POLSTR-OIS Compound" are the ISDA 2021 codes
    "PLN-POLSTR" -> "PLN-POLSTR",
    "PLN-POLSTR-COMPOUND" -> "PLN-POLSTR",
    "PLN-POLSTR-OIS-COMPOUND" -> "PLN-POLSTR",
    "PLN-POLSTR-OIS Compound" -> "PLN-POLSTR",

    // "RUB-RUONIA-OIS-COMPOUND" is the FpML/ISDA code
    // "RUB-RUONIA"/"RUB-RUONIA-OIS Compound" are the ISDA 2021 codes
    "RUB-RUONIA" -> "RUB-RUONIA",
    "RUB-RUONIA-COMPOUND" -> "RUB-RUONIA",
    "RUB-RUONIA-OIS-COMPOUND" -> "RUB-RUONIA",
    "RUB-RUONIA-OIS Compound" -> "RUB-RUONIA"
  )

  /**
   * The Overnight compounded rows of the SAR OSAIBOR, SAR SAIBOR, SEK SIOR, SEK SWESTR, SGD SONAR,
   * SGD SORA, THB THOR, TRY TLREF and ZAR SABOR families, in published order.
   *
   * @return the 31 rows, each mapping a published external name to its published value
   */
  private def overnightCompoundedPairs4: Vector[(String, String)] = Vector(
    // No FpML/ISDA code
    // "SAR-SAIBOR" is the ISDA 2021 code for Term and Overnight rates
    "SAR-OSAIBOR" -> "SAR-OSAIBOR",
    "SAR-SAIBOR-OIS Compound" -> "SAR-SAIBOR",

    // "SEK-STIBOR-OIS Compound" is the ISDA 2021 code
    "SEK-SIOR" -> "SEK-SIOR",
    "SEK-SIOR-COMPOUND" -> "SEK-SIOR",
    "SEK-SIOR-OIS-COMPOUND" -> "SEK-SIOR",
    "SEK-STINA" -> "SEK-SIOR",
    "SEK-STINA-COMPOUND" -> "SEK-SIOR",
    "SEK-STINA-OIS-COMPOUND" -> "SEK-SIOR",
    "SEK-STIBOR-OIS Compound" -> "SEK-SIOR",

    // "SEK-SWESTR"/"SEK-SWESTR-OIS Compound" are the ISDA 2021 codes
    "SEK-SWESTR" -> "SEK-SWESTR",
    "SEK-SWESTR-OIS" -> "SEK-SWESTR",
    "SEK-SWESTR-OIS Compound" -> "SEK-SWESTR",
    "SEK-SWESTR-OIS COMPOUND" -> "SEK-SWESTR",

    // "SGD-SONAR-OIS-VWAP-COMPOUND" is the FpML/ISDA code
    "SGD-SONAR" -> "SGD-SONAR",
    "SGD-SONAR-COMPOUND" -> "SGD-SONAR",
    "SGD-SONAR-OIS-COMPOUND" -> "SGD-SONAR",
    "SGD-SONAR-OIS-VWAP-COMPOUND" -> "SGD-SONAR",

    // "SGD-SORA-COMPOUND" is the FpML/ISDA code
    // "SGD-SORA"/"SGD-SORA-OIS Compound" are the ISDA 2021 codes
    "SGD-SORA" -> "SGD-SORA",
    "SGD-SORA-COMPOUND" -> "SGD-SORA",
    "SGD-SORA-OIS-COMPOUND" -> "SGD-SORA",
    "SGD-SORA-OIS Compound" -> "SGD-SORA",

    // "THB-THOR/THB-THOR-OIS COMPOUND" is the ISDA 2021 code
    "THB-THOR" -> "THB-THOR",
    "THB-THOR-OIS-COMPOUND" -> "THB-THOR",

    // "TRY-TLREF-OIS-COMPOUND" is the FpML/ISDA code
    // "TRY-TLREF-OIS Compound" is the ISDA 2021 code
    "TRY-TLREF" -> "TRY-TLREF",
    "TRY-TLREF-COMPOUND" -> "TRY-TLREF",
    "TRY-TLREF-OIS-COMPOUND" -> "TRY-TLREF",
    "TRY-TLREF-OIS Compound" -> "TRY-TLREF",

    // "ZAR-DEPOSIT-SAFEX " is the FpML/ISDA code
    "ZAR-SABOR" -> "ZAR-SABOR",
    "ZAR-DEPOSIT-SAFEX" -> "ZAR-SABOR",
    "ZAR-DEPOSIT-Reference Banks" -> "ZAR-SABOR",
    "ZAR-DEPOSIT" -> "ZAR-SABOR"
  )

  /**
   * The 156 Overnight compounded rows, in published order.
   *
   * @return the published external name to index name pairs of the Overnight compounded section
   */
  private def overnightCompoundedPairs: Vector[(String, String)] =
    overnightCompoundedPairs1 ++ overnightCompoundedPairs2 ++ overnightCompoundedPairs3 ++
      overnightCompoundedPairs4

  /**
   * The 6 Overnight averaged rows, in published order.
   *
   * The section names the US Fed Fund averaged rate and its variants. `USD-EFFECTIVE` names
   * `USD-FED-FUND` where the other five rows name `USD-FED-FUND-AVG`, exactly as published.
   *
   * @return the published external name to index name pairs of the Overnight averaged section
   */
  private def overnightAveragedPairs: Vector[(String, String)] = Vector(
    // "USD-Federal Funds" is an ISDA 2021 code, assume it links here
    "USD-FED-FUND-AVG" -> "USD-FED-FUND-AVG",
    "USD-Federal Funds-H.15" -> "USD-FED-FUND-AVG",
    "USD-Federal Funds-H.15-Bloomberg" -> "USD-FED-FUND-AVG",
    "USD-BASIS-FEDFUNDS" -> "USD-FED-FUND-AVG",
    "USD-EFFECTIVE" -> "USD-FED-FUND",
    "USD-Federal Funds" -> "USD-FED-FUND-AVG"
  )

  /**
   * The 30 price rows, in published order.
   *
   * The section names the inflation indices of the sterling, Swiss franc, euro, yen, dollar and
   * French markets, each with the spellings the published data admits for it.
   *
   * @return the published external name to index name pairs of the price section
   */
  private def pricePairs: Vector[(String, String)] = Vector(
    "GB-HICP" -> "GB-HICP",
    "UK-HICP" -> "GB-HICP",
    "GBP-HICP" -> "GB-HICP",

    "GB-RPI" -> "GB-RPI",
    "UK-RPI" -> "GB-RPI",
    "GBP-RPI" -> "GB-RPI",

    "GB-RPIX" -> "GB-RPIX",
    "UK-RPIX" -> "GB-RPIX",
    "GBP-RPIX" -> "GB-RPIX",

    "CH-CPI" -> "CH-CPI",
    "SWF-CPI" -> "CH-CPI",
    "CHF-CPI" -> "CH-CPI",

    "EU-AI-CPI" -> "EU-AI-CPI",
    "EUR-AI-CPI" -> "EU-AI-CPI",

    "EU-EXT-CPI" -> "EU-EXT-CPI",
    "EU-CPI" -> "EU-EXT-CPI",
    "EUR-EXT-CPI" -> "EU-EXT-CPI",
    "EUR-CPI" -> "EU-EXT-CPI",

    "JP-CPI-EXF" -> "JP-CPI-EXF",
    "JP-CPI" -> "JP-CPI-EXF",
    "JPY-CPI-EXF" -> "JP-CPI-EXF",
    "JPY-CPI" -> "JP-CPI-EXF",

    "US-CPI-U" -> "US-CPI-U",
    "US-CPI" -> "US-CPI-U",
    "USA-CPI-U" -> "US-CPI-U",
    "USD-CPI-U" -> "US-CPI-U",
    "USD-CPI" -> "US-CPI-U",

    "FR-EXT-CPI" -> "FR-EXT-CPI",
    "FR-CPI" -> "FR-EXT-CPI",
    "FRC-EXT-CPI" -> "FR-EXT-CPI"
  )

  /**
   * The 3 rows that declare a non-standard fixing date offset, in published order.
   *
   * @return the published external name to offset-in-days pairs of the fixing date offset section
   */
  private def iborFixingDateOffsetPairs: Vector[(String, Int)] = Vector(
    "DKK-CIBOR-DKNA13" -> 0,
    "DKK-CIBOR-DKNA13-Bloomberg" -> 0,
    "DKK-CIBOR-Reference Banks" -> 0
  )

  /**
   * The 23 default Ibor rows, in published order.
   *
   * @return the currency to external name pairs of the default Ibor section
   */
  private def currencyDefaultIborPairs: Vector[(Currency, String)] = Vector(
    Currency.CHF -> "CHF-LIBOR",
    Currency.EUR -> "EUR-EURIBOR",
    Currency.GBP -> "GBP-LIBOR",
    Currency.JPY -> "JPY-LIBOR",
    Currency.USD -> "USD-LIBOR",
    Currency.AUD -> "AUD-BBSW",
    Currency.CAD -> "CAD-CDOR",
    Currency.CNY -> "CNY-REPO",
    Currency.CZK -> "CZK-PRIBOR",
    Currency.DKK -> "DKK-CIBOR",
    Currency.HKD -> "HKD-HIBOR",
    Currency.HUF -> "HUF-BUBOR",
    Currency.KRW -> "KRW-CD",
    Currency.MXN -> "MXN-TIIE",
    Currency.MYR -> "MYR-KLIBOR",
    Currency.NOK -> "NOK-NIBOR",
    Currency.NZD -> "NZD-BKBM",
    Currency.PLN -> "PLN-WIBOR",
    Currency.SEK -> "SEK-STIBOR",
    Currency.SGD -> "SGD-SIBOR",
    Currency.THB -> "THB-THBFIX",
    Currency.TWD -> "TWD-TAIBOR",
    Currency.ZAR -> "ZAR-JIBAR"
  )

  /**
   * The 27 default Overnight rows, in published order.
   *
   * @return the currency to external name pairs of the default Overnight section
   */
  private def currencyDefaultOvernightPairs: Vector[(Currency, String)] = Vector(
    Currency.CHF -> "CHF-SARON",
    Currency.EUR -> "EUR-ESTR",
    Currency.GBP -> "GBP-SONIA",
    Currency.JPY -> "JPY-TONAR",
    Currency.USD -> "USD-SOFR",
    Currency.AUD -> "AUD-AONIA",
    Currency.BRL -> "BRL-CDI",
    Currency.CAD -> "CAD-CORRA",
    Currency.CLP -> "CLP-TNA",
    Currency.COP -> "COP-OIBR",
    Currency.CZK -> "CZK-CZEONIA",
    Currency.DKK -> "DKK-TNR",
    Currency.HKD -> "HKD-HONIA",
    Currency.HUF -> "HUF-HUFONIA",
    Currency.IDR -> "IDR-INDONIA",
    Currency.ILS -> "ILS-OTELBOR",
    Currency.INR -> "INR-OMIBOR",
    Currency.NOK -> "NOK-NOWA",
    Currency.NZD -> "NZD-NZIONA",
    Currency.PLN -> "PLN-POLONIA",
    Currency.RUB -> "RUB-RUONIA",
    Currency.SAR -> "SAR-OSAIBOR",
    Currency.SEK -> "SEK-SIOR",
    Currency.SGD -> "SGD-SORA",
    Currency.THB -> "THB-THOR",
    Currency.TRY -> "TRY-TLREF",
    Currency.ZAR -> "ZAR-SABOR"
  )

  /**
   * Finds the non-standard fixing date offset that the fixing date offset section declares for an
   * external name, matching the name without regard to case.
   *
   * The loader of the Java implementation registered every row twice, under its published external
   * name and under the upper-case form of that name, and then applied this section to the
   * upper-case entry alone. Of the three names the section declares, two are already upper case,
   * so for them the two entries were the same entry; the third, `DKK-CIBOR-Reference Banks`,
   * carried the offset under its upper-case spelling only. The key space of this port has one row
   * per external name - the upper-case spelling of a name resolves to that same row through the key
   * space `NamedEnum` builds - so the offset belongs to the row itself, and matching without regard
   * to case is what places it there. The three rows are the Danish CIBOR variants that fix on the
   * day of the period rather than two days before it.
   *
   * The match is made through `iborFixingDateOffsetsByUpperCaseName`, the table of the same three
   * offsets built once under upper-case keys, so a lookup is one direct lookup in a table of three
   * entries rather than a reconstruction of the section followed by a scan of it. Normalising the
   * argument the same way the table was keyed is how case is discounted here, which mirrors the key
   * normalisation the Java loader itself performed on the entry it read this section into; the
   * standard library returns the argument unchanged when it is already upper case, so only a name
   * that is not produces a second string.
   *
   * This is the lookup route for the fixing date offset section, and it is visible to the package
   * rather than to this object alone so that a consumer of the section asks a question of it
   * instead of searching [[iborFixingDateOffsets]]: the published table is ordered for comparison
   * with the reference data manifest, and answering a single name from it walks its insertion
   * chain, while this route is one hit in a plain map. The case-insensitive key space it reads
   * covers every published key, the one name that is not already upper case,
   * `DKK-CIBOR-Reference Banks`, included.
   *
   * @param externalName  the published external name of an Ibor row
   * @return the offset in days declared for that name, or nothing when none is declared
   */
  private[index] def fixingDateOffsetOf(externalName: String): Option[Int] =
    iborFixingDateOffsetsByUpperCaseName.get(externalName.toUpperCase(Locale.ENGLISH))

  /**
   * Builds the rows of a section whose published values are complete index names.
   *
   * The three Overnight and price sections declare no fixing date offset, so every row they
   * produce carries none.
   *
   * @param pairs  the published external name to index name pairs of the section, in published order
   * @param rateType  the kind of rate the section describes
   * @return the rows of the section, in published order
   */
  private def rowsOf(
      pairs: Vector[(String, String)],
      rateType: FloatingRateType): Vector[FloatingRateNameRow] =
    pairs.map {
      case (externalName, indexName) =>
        FloatingRateNameRow(
          externalName = externalName,
          indexName = indexName,
          rateType = rateType,
          fixingDateOffsetDays = None)
    }

  /**
   * The 3 non-standard fixing date offsets, keyed by the published external name they apply to.
   *
   * Published as a table of its own so that it can be compared with the corresponding section of
   * the reference data manifest, and consumed by [[iborRows]], where the same three offsets appear
   * as the `fixingDateOffsetDays` of the rows they name. The order is the published order, and it
   * stays published order because that order is part of what the manifest compares; an ordered map
   * answers a single key by walking its insertion chain, so ask [[fixingDateOffsetOf]] for one
   * name and read this table when the whole ordered section is what is wanted.
   *
   * This value is declared before the private `iborFixingDateOffsetsByUpperCaseName` lookup table
   * and before [[iborRows]] because both are built from it.
   */
  val iborFixingDateOffsets: Map[String, Int] = ListMap.from(iborFixingDateOffsetPairs)

  /**
   * The 3 non-standard fixing date offsets of [[iborFixingDateOffsets]], keyed by the upper-case
   * form of the external name each applies to.
   *
   * This is the table [[fixingDateOffsetOf]] reads, and it exists so that the 159 lookups made
   * while [[iborRows]] initialises are 159 direct lookups in one table of three entries rather than
   * 159 reconstructions of the section and linear scans of the result. It is derived from the
   * already-initialised [[iborFixingDateOffsets]] rather than from a second transcription of the
   * three rows, so the published table and the lookup table cannot disagree; it is a plain map
   * rather than an ordered one because order is a property of the published table alone and no
   * consumer reads this one.
   *
   * Upper-case keys - folded with `Locale.ENGLISH`, so the folding is independent of the default
   * locale of the host - are what make the lookup case-insensitive, as [[fixingDateOffsetOf]]
   * explains. Being private, this table is invisible to the reference data manifest comparison,
   * which reads [[iborFixingDateOffsets]].
   *
   * This value is declared after [[iborFixingDateOffsets]], which it is derived from, and before
   * [[iborRows]], which reads it through [[fixingDateOffsetOf]]: the vals of an object initialise
   * in textual order, so either declaration moved out of that order would leave this table empty
   * while the rows were built and silently strip the three offsets.
   */
  private val iborFixingDateOffsetsByUpperCaseName: Map[String, Int] =
    iborFixingDateOffsets.iterator.map {
      case (externalName, days) => externalName.toUpperCase(Locale.ENGLISH) -> days
    }.toMap

  /**
   * The 159 Ibor rows, in published order.
   *
   * The index name of each row is the published value with a `-` appended, which is the form the
   * Java loader produced and the form a tenor completes: `GBP-LIBOR-` plus `3M` names the index
   * `GBP-LIBOR-3M`. The three rows named by [[iborFixingDateOffsets]] carry their declared offset;
   * the other 156 carry none and so defer to the offset of the index they name.
   */
  val iborRows: Vector[FloatingRateNameRow] =
    iborPairs.map {
      case (externalName, indexNameStem) =>
        FloatingRateNameRow(
          externalName = externalName,
          indexName = indexNameStem + "-",
          rateType = FloatingRateType.Ibor,
          fixingDateOffsetDays = fixingDateOffsetOf(externalName))
    }

  /**
   * The 156 Overnight compounded rows, in published order.
   *
   * Compounding is the usual accrual of an Overnight rate, which is why this is by far the largest
   * of the Overnight sections.
   */
  val overnightCompoundedRows: Vector[FloatingRateNameRow] =
    rowsOf(overnightCompoundedPairs, FloatingRateType.OvernightCompounded)

  /**
   * The 6 Overnight averaged rows, in published order.
   */
  val overnightAveragedRows: Vector[FloatingRateNameRow] =
    rowsOf(overnightAveragedPairs, FloatingRateType.OvernightAveraged)

  /**
   * The 30 price rows, in published order.
   */
  val priceRows: Vector[FloatingRateNameRow] =
    rowsOf(pricePairs, FloatingRateType.Price)

  /**
   * All 351 name rows, in published order: the Ibor rows, then the Overnight compounded rows, then
   * the Overnight averaged rows, then the price rows.
   *
   * This is the order in which the Java loader processed the sections, and it is the order in which
   * a consumer that builds one member per row should build them, so that the member list of the
   * family is the published list.
   */
  val rows: Vector[FloatingRateNameRow] =
    iborRows ++ overnightCompoundedRows ++ overnightAveragedRows ++ priceRows

  /**
   * The 351 name rows, keyed by their external name.
   *
   * The external names of [[rows]] are distinct, so this map holds every row and loses none. It is
   * the lookup a consumer uses to resolve a published name, and the lookup the currency default
   * tables are resolved through, their values being external names.
   */
  val byExternalName: Map[String, FloatingRateNameRow] =
    rows.iterator.map(row => row.externalName -> row).toMap

  /**
   * The default Ibor rate of each of 23 currencies, as the external name of a row of [[rows]], in
   * published order.
   *
   * The value is an external name rather than an index name, and the two differ for some rows:
   * `CNY` defaults to the external name `CNY-REPO` and `THB` to `THB-THBFIX`. Resolve a value
   * through [[byExternalName]]; do not read it as an index name.
   *
   * A currency absent from this table has no published default Ibor rate, which is a value the
   * consumer reports rather than a case this table can fill.
   *
   * The order is the published order and stays that way because the reference data manifest
   * compares the section in order, which also means that answering one currency from here walks
   * the insertion chain as far as that currency sits along it. Ask [[defaultIborExternalNameOf]]
   * for a single currency; read this table when the ordered section itself is what is wanted.
   */
  val currencyDefaultIbor: Map[Currency, String] = ListMap.from(currencyDefaultIborPairs)

  /**
   * The 23 rows of [[currencyDefaultIbor]], keyed the same way but held in a plain map.
   *
   * This is the table [[defaultIborExternalNameOf]] reads, and it exists for the same reason
   * `iborFixingDateOffsetsByUpperCaseName` exists: the published table is ordered, and an ordered
   * map answers a key by walking its insertion chain, so the cost of a lookup there is the
   * position of the key rather than the size of the table. It is derived from the
   * already-initialised [[currencyDefaultIbor]] rather than from a second transcription of the 23
   * rows, so the published table and the lookup table cannot disagree, and it is derived through
   * `iterator` because `toMap` on an ordered map answers with that same ordered map and would
   * leave the chain in place. Being private, it is invisible to the reference data manifest
   * comparison, which reads [[currencyDefaultIbor]].
   *
   * This value is declared after [[currencyDefaultIbor]], which it is derived from: the vals of an
   * object initialise in textual order, so a declaration moved above its table would silently
   * leave this one empty.
   */
  private val currencyDefaultIborByCurrency: Map[Currency, String] =
    currencyDefaultIbor.iterator.toMap

  /**
   * Finds the external name of the published default Ibor rate of a currency.
   *
   * The answer is an external name and not an index name, as [[currencyDefaultIbor]] explains, so
   * resolve it through [[byExternalName]] to reach the row it names.
   *
   * @param currency  the currency to find the default Ibor rate of
   * @return the external name of the default Ibor rate of that currency, or nothing when the
   *   published data declares none for it
   */
  private[index] def defaultIborExternalNameOf(currency: Currency): Option[String] =
    currencyDefaultIborByCurrency.get(currency)

  /**
   * The default Overnight rate of each of 27 currencies, as the external name of a row of [[rows]],
   * in published order.
   *
   * As with [[currencyDefaultIbor]], the value is an external name to be resolved through
   * [[byExternalName]], and a currency absent from the table has no published default Overnight
   * rate. The order is the published order for the same reason, and a single currency is likewise
   * answered by [[defaultOvernightExternalNameOf]] rather than from here.
   */
  val currencyDefaultOvernight: Map[Currency, String] = ListMap.from(currencyDefaultOvernightPairs)

  /**
   * The 27 rows of [[currencyDefaultOvernight]], keyed the same way but held in a plain map.
   *
   * This is the table [[defaultOvernightExternalNameOf]] reads, and it stands to
   * [[currencyDefaultOvernight]] exactly as `currencyDefaultIborByCurrency` stands to
   * [[currencyDefaultIbor]]: derived from the published table through `iterator`, so that the two
   * cannot disagree and so that the insertion chain of the ordered map is not carried over, and
   * private, so that the reference data manifest comparison continues to read the published table.
   *
   * This value is declared after [[currencyDefaultOvernight]], which it is derived from, for the
   * initialisation-order reason given on `currencyDefaultIborByCurrency`.
   */
  private val currencyDefaultOvernightByCurrency: Map[Currency, String] =
    currencyDefaultOvernight.iterator.toMap

  /**
   * Finds the external name of the published default Overnight rate of a currency.
   *
   * The answer is an external name and not an index name, so resolve it through
   * [[byExternalName]] to reach the row it names.
   *
   * @param currency  the currency to find the default Overnight rate of
   * @return the external name of the default Overnight rate of that currency, or nothing when the
   *   published data declares none for it
   */
  private[index] def defaultOvernightExternalNameOf(currency: Currency): Option[String] =
    currencyDefaultOvernightByCurrency.get(currency)
}
