/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.CurrencyPair
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.basics.date.HolidayCalendarIds

/**
 * A single row of transcribed FX index reference data.
 *
 * Each row corresponds to one entry of the FX index reference data that the Java implementation
 * located on the classpath and parsed on first use, and carries exactly the six columns that data
 * declared: the index name, the base currency, the counter currency, the fixing calendar, the
 * number of business days from fixing to maturity, and the calendar those business days are
 * counted in.
 *
 * ===The two currency columns===
 *
 * The base and counter columns are held as one [[CurrencyPair]] rather than as two separate
 * fields, because a pair is what the Java parser built from them and what an FX index is quoted
 * against. The correspondence is exact and mechanical - the `Base Currency` column is
 * `currencyPair.base` and the `Counter Currency` column is `currencyPair.counter` - so the
 * captured reference data manifest is still compared against this table column for column.
 *
 * ===Data, not behaviour===
 *
 * The row carries no derived value. In particular it does not carry the fixing date offset of an
 * index: the original data declared no such column, and the Java index type computed the offset
 * from the maturity days and the two calendars while building the index, so that computation
 * belongs with the index family in `Index.scala` rather than with this transcription. For the
 * same reason the maturity days and the maturity calendar are kept apart here rather than
 * composed into one days adjustment: composing them is the index family's work.
 *
 * @param name              the unique name of the index, such as `EUR/USD-ECB`; the name is the
 *                          identity of the index in text, in JSON and to a user, so it is
 *                          reproduced exactly as the original data spelled it, punctuation
 *                          included
 * @param currencyPair      the currency pair the index is quoted for, whose base and counter are
 *                          the two currency columns of the original data
 * @param fixingCalendar    the calendar that determines the days the index is fixed on
 * @param maturityDays      the number of business days between the fixing date and the maturity
 *                          date; two for most of these indices, one for the Chilean peso index
 *                          and zero for the Colombian peso index
 * @param maturityCalendar  the calendar those business days are counted in
 */
final case class FxIndexRow(
    name: String,
    currencyPair: CurrencyPair,
    fixingCalendar: HolidayCalendarId,
    maturityDays: Int,
    maturityCalendar: HolidayCalendarId)

/**
 * The FX index reference data of the module, expressed as immutable Scala data.
 *
 * This object carries the FX index table transcribed verbatim from the reference data that the
 * Java implementation located on the classpath and parsed on first use. Nothing is located,
 * parsed or cached at run time here: the sixteen rows below are Scala literals fixed at compile
 * time, so the table cannot fail to initialise, needs no classpath, recovers from no load failure
 * and holds no hidden global state. Where the Java loader logged a severe error and handed back an
 * empty map when a row would not parse - silently leaving the whole family without members - there
 * is no such failure mode to recover from, because a row that does not typecheck does not compile.
 *
 * It holds data only and no behaviour: it constructs no [[FxIndex]], because creating the members
 * of that family, and giving them the lookups, the derived offsets and the observation machinery
 * an index needs, belongs to `Index.scala`, which reads [[rows]] to do it. The dependency runs one
 * way, from this object towards the currency and calendar types it names, and from `Index.scala`
 * towards this object; nothing in this file refers to any other member of the index package.
 *
 * ===This table is the domain of the family===
 *
 * For the other index families the transcribed table is the set of members a name can be looked
 * up in, and an unconfigured input is simply a name that resolves to nothing. For FX indices it is
 * more than that. The Java implementation, asked for an index for a currency pair it had no row
 * for, minted one on the spot from the pair's default calendar and a two day maturity offset; that
 * fallback is deliberately not ported, so the family is closed and this table alone fixes which
 * pairs an index exists for. Adding or omitting a row therefore does not merely change a lookup,
 * it changes the public API - which is why the table is pinned row for row against the captured
 * reference data manifest and why no index may be added to, removed from or edited in it.
 *
 * ===Invariants===
 *
 * All of the following are pinned against the Java captured reference data manifest, so a self
 * consistent but mistranscribed row cannot pass:
 *
 *  - [[rows]] holds exactly sixteen entries with distinct names, in the declaration order of the
 *    original data: the four European Central Bank rates, the four WM/Reuters rates, then the
 *    eight rates named after the industry definitions that publish them.
 *  - Every row's maturity is two business days except the Colombian peso index, which matures on
 *    the fixing date itself, and the Chilean peso index, which matures one business day later.
 *  - Eight of the sixteen fixing calendars are composite or name a calendar whose holidays this
 *    library does not ship. Seven identifiers - the Chilean, Chinese, Colombian, Indian, Korean,
 *    Singaporean and Taiwanese ones - have no built-in calendar here because they had none in the
 *    original either, so those indices resolve against `ReferenceData.standard` with a missing
 *    data failure, exactly as they did there. Nothing is invented to fill the gap.
 *  - Two rows share a currency pair: the euro/dollar rate is published both by the European
 *    Central Bank and by WM/Reuters. This is why an index cannot be identified by its pair alone,
 *    and why the family resolves a pair to the matching row of lowest name; that selection rule is
 *    the family's and lives in `Index.scala`, not here.
 *
 * ===What this table deliberately omits===
 *
 * There is no upper case key space in [[byName]]. Registering each name a second time in upper
 * case was how the Java registry answered a case insensitive lookup, and that responsibility now
 * belongs to the named enum support in `strata-collect`, which derives the upper case view from
 * the canonical one.
 *
 * There is likewise no alternate name table. The original declared exactly one alternate FX index
 * name, superseded by the index now named for the benchmark administrator that publishes it, and
 * an alternate name is a property of the family's lookup rather than of a row of data, so it lives
 * with the family in `Index.scala`.
 *
 * All members are immutable values, so this object is thread-safe.
 */
object FxIndexData {

  /**
   * The sixteen transcribed FX index rows, in the declaration order of the original data.
   *
   * The order is observable through any iteration a consumer performs - notably the order in which
   * `Index.scala` creates the members of the FX index family, and the order within each group of
   * [[byCurrencyPair]] - so it is kept stable and deterministic rather than re-sorted. The columns
   * appear in the order the original data declared them, so the literal list can be diffed line
   * for line against it.
   *
   * A calendar that the library ships and names is written as its named constant, and one it does
   * not name - because the original named none either - is written as the total identifier factory
   * applied to the text of the column. That factory also builds the composite identifiers, and it
   * normalises them exactly as the original did: the parts are deduplicated and sorted by name, so
   * the column text `EUTA+CHZU` yields the identifier named `CHZU+EUTA`. The literals below are
   * the column text, and normalisation is the identifier's business, which is precisely how the
   * Java parser handed each column to it.
   */
  val rows: Vector[FxIndexRow] = Vector(
    row("EUR/CHF-ECB", Currency.EUR, Currency.CHF, HolidayCalendarIds.EUTA, 2, calendar("EUTA+CHZU")),
    row("EUR/GBP-ECB", Currency.EUR, Currency.GBP, HolidayCalendarIds.EUTA, 2, calendar("EUTA+GBLO")),
    row("EUR/JPY-ECB", Currency.EUR, Currency.JPY, HolidayCalendarIds.EUTA, 2, calendar("EUTA+JPTO")),
    row("EUR/USD-ECB", Currency.EUR, Currency.USD, HolidayCalendarIds.EUTA, 2, calendar("EUTA+USNY")),
    row("USD/CHF-WM", Currency.USD, Currency.CHF, HolidayCalendarIds.USNY, 2, calendar("USNY+CHZU")),
    row("EUR/USD-WM", Currency.EUR, Currency.USD, HolidayCalendarIds.USNY, 2, calendar("USNY+EUTA")),
    row("GBP/USD-WM", Currency.GBP, Currency.USD, HolidayCalendarIds.USNY, 2, calendar("USNY+GBLO")),
    row("USD/JPY-WM", Currency.USD, Currency.JPY, HolidayCalendarIds.USNY, 2, calendar("USNY+JPTO")),
    row("USD/CLP-DOLAR-OBS-CLP10", Currency.USD, Currency.CLP, calendar("CLSA"), 1, calendar("CLSA")),
    row("USD/CNY-SAEC-CNY01", Currency.USD, Currency.CNY, calendar("CNBE"), 2, calendar("CNBE")),
    row("USD/COP-TRM-COP02", Currency.USD, Currency.COP, calendar("COBO"), 0, calendar("COBO")),
    row("USD/INR-FBIL-INR01", Currency.USD, Currency.INR, calendar("INMU"), 2, calendar("INMU")),
    row("USD/KRW-KFTC18-KRW02", Currency.USD, Currency.KRW, calendar("KRSE"), 2, calendar("KRSE")),
    row("USD/SGD-VWAP-SGD3", Currency.USD, Currency.SGD, calendar("SGSI"), 2, calendar("SGSI")),
    row("USD/THB-VWAP-THB01", Currency.USD, Currency.THB, calendar("SGSI+THBA"), 2, calendar("SGSI+THBA")),
    row("USD/TWD-TAIFX1-TWD03", Currency.USD, Currency.TWD, calendar("TWTA"), 2, calendar("TWTA")))

  /**
   * The transcribed rows keyed by the name of the index.
   *
   * Derived from [[rows]] rather than transcribed a second time, since a second transcription
   * would be a second opportunity to be wrong, and the names are distinct so the derivation loses
   * nothing. The keys are the canonical names alone: a lookup that folds case, or that follows an
   * alternate name, is the business of the named enum support rather than of this table.
   */
  val byName: Map[String, FxIndexRow] = rows.iterator.map(entry => entry.name -> entry).toMap

  /**
   * The transcribed rows grouped by the currency pair the index is quoted for.
   *
   * A pair does not identify a row - the euro/dollar rate is published by two administrators and
   * so appears twice - which is why the value is a sequence rather than a single row, and why the
   * family has to choose between the candidates when it is asked for the index of a pair. That
   * choice is the family's and is made in `Index.scala`; this view only presents the candidates.
   *
   * Within each group the rows keep the order of [[rows]], so the candidates for a pair are always
   * presented in the order the original data declared them. That ordering is built here by folding
   * the rows in order and appending to the group each one belongs to, rather than by grouping, so
   * that it is a property of this code rather than of an unspecified library behaviour.
   */
  val byCurrencyPair: Map[CurrencyPair, Vector[FxIndexRow]] =
    rows.foldLeft(Map.empty[CurrencyPair, Vector[FxIndexRow]])((grouped, entry) =>
      grouped.updated(entry.currencyPair, grouped.getOrElse(entry.currencyPair, Vector.empty) :+ entry))

  //-------------------------------------------------------------------------
  /**
   * Builds one row from the six columns of the original data.
   *
   * The base and counter currencies are separate parameters, so that the table above reads as the
   * original data read - six columns in their declared order - while the row type holds the single
   * currency pair that the rest of the module works with. Pairing the two currencies in one place
   * mirrors the Java parser, which likewise combined the two columns as it read them, and a pair of
   * currencies is always meaningful, so this cannot fail.
   *
   * @param name              the unique name of the index
   * @param baseCurrency      the `Base Currency` column
   * @param counterCurrency   the `Counter Currency` column
   * @param fixingCalendar    the `Fixing Calendar` column
   * @param maturityDays      the `Maturity Days` column
   * @param maturityCalendar  the `Maturity Calendar` column
   * @return the transcribed row
   */
  private def row(
      name: String,
      baseCurrency: Currency,
      counterCurrency: Currency,
      fixingCalendar: HolidayCalendarId,
      maturityDays: Int,
      maturityCalendar: HolidayCalendarId): FxIndexRow =
    FxIndexRow(
      name,
      CurrencyPair.of(baseCurrency, counterCurrency),
      fixingCalendar,
      maturityDays,
      maturityCalendar)

  /**
   * Obtains a calendar identifier from the text of a calendar column.
   *
   * This is the total identifier factory under a shorter name, so that each row of the table fits
   * on one line and stays diffable against the original data. Any name is accepted, whether or not
   * this library ships the calendar it names, and a name joining several calendars is normalised
   * by the factory; see [[HolidayCalendarId.of]] for both. An identifier naming a calendar that is
   * not shipped fails to resolve against reference data at the point of use, which is the
   * behaviour of the original and is why such an identifier is admitted here rather than rejected.
   *
   * @param uniqueName  the text of the calendar column
   * @return the calendar identifier
   */
  private def calendar(uniqueName: String): HolidayCalendarId = HolidayCalendarId.of(uniqueName)
}
