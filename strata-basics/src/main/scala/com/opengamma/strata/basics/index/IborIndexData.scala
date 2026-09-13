/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import java.time.LocalTime
import java.time.ZoneId

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.Currency.{
  AUD, CAD, CHF, CNY, CZK, DKK, EUR, GBP, HKD, HUF, ILS, JPY, KRW, MXN, MYR, NOK, NZD, PLN, SAR,
  SEK, SGD, THB, TWD, USD, ZAR
}
import com.opengamma.strata.basics.date.DayCount
import com.opengamma.strata.basics.date.DayCounts.{ACT_360, ACT_365F, ACT_ACT_ISDA, THIRTY_U_360}
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.basics.date.HolidayCalendarIds.{
  AUSY, CATO, CZPR, DKCO, EUTA, GBLO, HUBU, JPTO, MXMC, NOOS, PLWA, SEST, THBA, USGS, USNY, ZAJO
}
import com.opengamma.strata.basics.date.Tenor
import com.opengamma.strata.basics.date.Tenor.{
  TENOR_1W, TENOR_2W, TENOR_4W, TENOR_13W, TENOR_26W, TENOR_1M, TENOR_2M, TENOR_3M, TENOR_4M,
  TENOR_5M, TENOR_6M, TENOR_7M, TENOR_8M, TENOR_9M, TENOR_10M, TENOR_11M, TENOR_12M
}
import com.opengamma.strata.collect.NoJavaSerialization

/**
 * A single row of transcribed Ibor index reference data.
 *
 * Each row is one published entry of the Ibor index reference data, and carries exactly the
 * thirteen columns that data declares, in the order it declares them: the index name, the currency
 * the rate is quoted in, whether the rate is still published, the day count the rate accrues on,
 * the calendar the rate is fixed against, the number of business days from fixing to effective
 * date, the calendar those days are counted in, the calendar the effective and maturity dates are
 * adjusted against, the tenor of the rate, the text of the tenor convention, the time of day the
 * rate is fixed at, the time zone that time is expressed in, and the day count of the fixed leg a
 * swap against this rate conventionally uses.
 *
 * ===Data, not behaviour===
 *
 * A row carries no derived value. The three date offsets an index exposes - from fixing date to
 * effective date, from effective date back to fixing date, and from effective date to maturity -
 * are not columns of this data: each is computed from the offset days, the tenor, the tenor
 * convention and the calendars, and that computation belongs with the index family in
 * `Index.scala`, which reads these rows to create its members. The columns are therefore kept
 * apart here, as the published data keeps them apart.
 *
 * ===Why the tenor convention stays text===
 *
 * [[tenorConvention]] is deliberately a `String` and not a parsed convention. The single column it
 * transcribes has two readings: as a period addition convention, which is absent when the text
 * names no such convention, and as a business day convention, which is modified following when the
 * period addition convention moves to the end of a month and following otherwise. Both readings
 * are needed to build an index, and each admits text the other rejects - `LastBusinessDay` names
 * only the first, `ModifiedFollowing` only the second - so resolving the column here to either type
 * would discard the other reading. The text is therefore carried verbatim and interpreted where
 * both readings are performed, in `Index.scala`.
 *
 * ===Why both day counts are typed===
 *
 * [[dayCount]] and [[fixedLegDayCount]], by contrast, are resolved to [[DayCount]] here, because
 * each column has exactly one reading and one type. Resolving them also folds away a spelling
 * difference that is not a difference in data: the day count of the fixed leg is spelled in upper
 * case on the Czech koruna rows of the published data and in mixed case everywhere else, and both
 * spellings name one convention, so both transcribe to the same constant.
 *
 * @param name                   the unique name of the index, such as `GBP-LIBOR-3M`; the name is
 *                               the identity of the index in text, in JSON and to a user, so it
 *                               is reproduced exactly as the published data spells it
 * @param currency               the currency the rate is quoted in
 * @param active                 whether the rate is still published; an inactive rate is retained
 *                               because trades that reference it outlive its publication
 * @param dayCount               the day count the rate accrues on
 * @param fixingCalendar         the calendar that determines the days the rate is fixed on
 * @param offsetDays             the number of business days between the fixing date and the
 *                               effective date; zero, one or two on this data
 * @param offsetCalendar         the calendar those business days are counted in
 * @param effectiveDateCalendar  the calendar the effective and maturity dates are adjusted
 *                               against, which is composite on four of these families
 * @param tenor                  the tenor of the rate, which the maturity date is derived from
 * @param tenorConvention        the text of the tenor convention column, carried verbatim because
 *                               it has two readings; see above
 * @param fixingTime             the local time of day the rate is fixed at
 * @param fixingZone             the time zone [[fixingTime]] is expressed in
 * @param fixedLegDayCount       the day count of the fixed leg a swap against this rate
 *                               conventionally uses
 *
 * The row is the shape the table below is written in rather than a value of the published API: a
 * caller reads this data as the index family the companion of `Index.scala` builds from it, never
 * as rows. It is therefore visible within `com.opengamma.strata.basics` and no further - the same
 * visibility `PriceIndexRow` has, and for the same reason - so that the row type adds nothing to
 * the published surface of the module.
 */
private[basics] final case class IborIndexRow(
    name: String,
    currency: Currency,
    active: Boolean,
    dayCount: DayCount,
    fixingCalendar: HolidayCalendarId,
    offsetDays: Int,
    offsetCalendar: HolidayCalendarId,
    effectiveDateCalendar: HolidayCalendarId,
    tenor: Tenor,
    tenorConvention: String,
    fixingTime: LocalTime,
    fixingZone: ZoneId,
    fixedLegDayCount: DayCount)
    extends NoJavaSerialization

/**
 * The Ibor index reference data of the module, expressed as immutable Scala data.
 *
 * This object carries the published Ibor index table verbatim: the 271 rows below are Scala
 * literals fixed at compile time, each holding the thirteen columns of [[IborIndexRow]]. Nothing
 * is located, parsed or cached at run time, so the table cannot fail to initialise, cannot be
 * partial, cannot be overridden at run time and holds no hidden global state.
 *
 * It holds data only and no behaviour: it constructs no [[IborIndex]], because creating the
 * members of that family, deriving their date offsets and giving them the lookups and observation
 * machinery an index needs, belongs to `Index.scala`, which reads [[rows]] to do it. The
 * dependency runs one way - from this object towards the currency, day count, calendar and tenor
 * types it names, and from `Index.scala` towards this object - and nothing in this file refers to
 * any other member of the index package, which is what lets the index family be built without an
 * initialisation cycle.
 *
 * ===Invariants===
 *
 * The table holds all of the following, and a consumer may rely on them:
 *
 *  - [[rows]] holds exactly 271 entries with distinct names, in the declaration order of the
 *    published data: 35 benchmark families, each in the order its tenors are declared.
 *  - 162 of the rows are active and 109 are retained for trades that outlive publication.
 *  - The rows name 25 currencies, 17 tenors from one week to twelve months, and four day counts.
 *  - The tenor convention column holds exactly four texts: `Following`, `LastBusinessDay`,
 *    `ModifiedFollowing` and `ModifiedFollowingBiMonthly`.
 *  - The offset from fixing to effective date is zero, one or two business days.
 *  - Nine of the calendars named here - the Chinese, Hong Kong, Israeli, Korean, Malaysian, New
 *    Zealand bank, Saudi, Singaporean and Taiwanese ones - have no built-in calendar in this
 *    library. Such an identifier is transcribed as it stands and nothing is invented to fill the
 *    gap; resolving it against reference data fails with missing data at the point of use.
 *  - Four families adjust their effective dates against a composite calendar. The literals below
 *    hold the text of the column and leave its normalisation to the identifier factory, so the
 *    column text `SGSI+GBLO` yields the identifier named `GBLO+SGSI`.
 *
 * ===What this table deliberately omits===
 *
 * There is no upper case key space in [[byName]]: a case insensitive lookup belongs to the named
 * enum support in `strata-collect`, which derives the upper case view from the canonical one, so
 * this table holds the canonical names alone.
 *
 * There is likewise no alternate name table. Exactly one alternate Ibor index name is published,
 * for the won certificate of deposit rate whose tenor is spelled in weeks by the index and in
 * months by the market, and an alternate name is a property of the family's lookup rather than of
 * a row of data, so it lives with the family in `Index.scala`.
 *
 * All members are immutable values, so this object is thread-safe.
 *
 * The table is visible within `com.opengamma.strata.basics` and no further, as `PriceIndexData`
 * is: it is the transcribed reference data the index family is built from, and the family is what
 * a caller uses.
 */
private[basics] object IborIndexData {

  /**
   * The 271 transcribed Ibor index rows, in the declaration order of the published data.
   *
   * The order is observable through any iteration a consumer performs - notably the order in
   * which `Index.scala` creates the members of the Ibor index family - so it is kept stable and
   * deterministic rather than re-sorted. Within each of the 35 benchmark families the rows keep
   * the order their tenors were declared in, which is ascending but not by a single unit, since a
   * family may declare weeks before months.
   *
   * The value is the concatenation of one group per benchmark family; the groups are defined
   * below and are listed here in the order the published data declares them.
   */
  val rows: Vector[IborIndexRow] =
    gbpLibor ++ gbpSoniaIceTerm ++ gbpSoniaRefinitivTerm ++ chfLibor ++ eurLibor ++ jpyLibor ++
      jpyTorf ++ usdLibor ++ usdBsby ++ usdSofrCmeTerm ++ usdAmeriborTerm ++ eurEuribor ++
      jpyTiborJapan ++ jpyTiborEuroyen ++ audBbsw ++ cadCdor ++ cnyRepo ++ czkPribor ++
      dkkCibor ++ hkdHibor ++ hufBubor ++ ilsTelbor ++ krwCd ++ mxnTiie ++ myrKlibor ++
      nokNibor ++ nzdBkbm ++ plnWibor ++ sarSaibor ++ sekStibor ++ sgdSibor ++ sgdSor ++
      thbThbFix ++ twdTaibor ++ zarJibar

  /**
   * The transcribed rows keyed by the name of the index.
   *
   * Derived from [[rows]] rather than transcribed a second time, since a second transcription
   * would be a second opportunity to be wrong, and the names are distinct so the derivation loses
   * nothing. The keys are the canonical names alone: a lookup that folds case, or that follows an
   * alternate name, is the business of the named enum support rather than of this table.
   */
  val byName: Map[String, IborIndexRow] =
    rows.iterator.map(row => row.name -> row).toMap

  // The table is defined in one group per benchmark family rather than as a single literal, and
  // the groups are deliberately not merged. A literal of 271 rows of thirteen columns would become
  // one very large initialiser method, which risks the method size limit of the virtual machine;
  // and a group per family keeps each literal small enough to read, follows the grouping of the
  // published data, and lets one family be read against that data on its own. Each group is a
  // method rather than a field so that nothing but the concatenated table is retained.

  /** The fourteen sterling LIBOR rows, `GBP-LIBOR-1W` to `GBP-LIBOR-12M`, six of them active. */
  private def gbpLibor: Vector[IborIndexRow] = Vector(
    IborIndexRow("GBP-LIBOR-1W", GBP, active = true, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_1W, "Following",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("GBP-LIBOR-2W", GBP, active = false, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_2W, "Following",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("GBP-LIBOR-1M", GBP, active = true, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_1M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("GBP-LIBOR-2M", GBP, active = true, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_2M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("GBP-LIBOR-3M", GBP, active = true, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_3M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("GBP-LIBOR-4M", GBP, active = false, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_4M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("GBP-LIBOR-5M", GBP, active = false, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_5M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("GBP-LIBOR-6M", GBP, active = true, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_6M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("GBP-LIBOR-7M", GBP, active = false, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_7M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("GBP-LIBOR-8M", GBP, active = false, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_8M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("GBP-LIBOR-9M", GBP, active = false, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_9M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("GBP-LIBOR-10M", GBP, active = false, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_10M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("GBP-LIBOR-11M", GBP, active = false, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_11M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("GBP-LIBOR-12M", GBP, active = true, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_12M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F))

  /**
   * The four sterling ICE term SONIA rows, `GBP-SONIAICETERM-1M` to `GBP-SONIAICETERM-12M`, all of
   * them active.
   */
  private def gbpSoniaIceTerm: Vector[IborIndexRow] = Vector(
    IborIndexRow("GBP-SONIAICETERM-1M", GBP, active = true, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_1M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("GBP-SONIAICETERM-3M", GBP, active = true, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_3M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("GBP-SONIAICETERM-6M", GBP, active = true, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_6M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("GBP-SONIAICETERM-12M", GBP, active = true, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_12M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F))

  /**
   * The four sterling Refinitiv term SONIA rows, `GBP-SONIAREFINITIVTERM-1M` to
   * `GBP-SONIAREFINITIVTERM-12M`, all of them active.
   */
  private def gbpSoniaRefinitivTerm: Vector[IborIndexRow] = Vector(
    IborIndexRow("GBP-SONIAREFINITIVTERM-1M", GBP, active = true, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_1M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("GBP-SONIAREFINITIVTERM-3M", GBP, active = true, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_3M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("GBP-SONIAREFINITIVTERM-6M", GBP, active = true, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_6M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("GBP-SONIAREFINITIVTERM-12M", GBP, active = true, ACT_365F, GBLO, 0,
      GBLO, GBLO, TENOR_12M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F))

  /**
   * The fourteen Swiss franc LIBOR rows, `CHF-LIBOR-1W` to `CHF-LIBOR-12M`, six of them active.
   */
  private def chfLibor: Vector[IborIndexRow] = Vector(
    IborIndexRow("CHF-LIBOR-1W", CHF, active = true, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+CHZU"), TENOR_1W, "Following",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("CHF-LIBOR-2W", CHF, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+CHZU"), TENOR_2W, "Following",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("CHF-LIBOR-1M", CHF, active = true, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+CHZU"), TENOR_1M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("CHF-LIBOR-2M", CHF, active = true, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+CHZU"), TENOR_2M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("CHF-LIBOR-3M", CHF, active = true, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+CHZU"), TENOR_3M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("CHF-LIBOR-4M", CHF, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+CHZU"), TENOR_4M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("CHF-LIBOR-5M", CHF, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+CHZU"), TENOR_5M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("CHF-LIBOR-6M", CHF, active = true, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+CHZU"), TENOR_6M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("CHF-LIBOR-7M", CHF, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+CHZU"), TENOR_7M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("CHF-LIBOR-8M", CHF, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+CHZU"), TENOR_8M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("CHF-LIBOR-9M", CHF, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+CHZU"), TENOR_9M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("CHF-LIBOR-10M", CHF, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+CHZU"), TENOR_10M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("CHF-LIBOR-11M", CHF, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+CHZU"), TENOR_11M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("CHF-LIBOR-12M", CHF, active = true, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+CHZU"), TENOR_12M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360))

  /** The fourteen euro LIBOR rows, `EUR-LIBOR-1W` to `EUR-LIBOR-12M`, six of them active. */
  private def eurLibor: Vector[IborIndexRow] = Vector(
    IborIndexRow("EUR-LIBOR-1W", EUR, active = true, ACT_360, GBLO, 2,
      EUTA, EUTA, TENOR_1W, "Following",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("EUR-LIBOR-2W", EUR, active = false, ACT_360, GBLO, 2,
      EUTA, EUTA, TENOR_2W, "Following",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("EUR-LIBOR-1M", EUR, active = true, ACT_360, GBLO, 2,
      EUTA, EUTA, TENOR_1M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("EUR-LIBOR-2M", EUR, active = true, ACT_360, GBLO, 2,
      EUTA, EUTA, TENOR_2M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("EUR-LIBOR-3M", EUR, active = true, ACT_360, GBLO, 2,
      EUTA, EUTA, TENOR_3M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("EUR-LIBOR-4M", EUR, active = false, ACT_360, GBLO, 2,
      EUTA, EUTA, TENOR_4M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("EUR-LIBOR-5M", EUR, active = false, ACT_360, GBLO, 2,
      EUTA, EUTA, TENOR_5M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("EUR-LIBOR-6M", EUR, active = true, ACT_360, GBLO, 2,
      EUTA, EUTA, TENOR_6M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("EUR-LIBOR-7M", EUR, active = false, ACT_360, GBLO, 2,
      EUTA, EUTA, TENOR_7M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("EUR-LIBOR-8M", EUR, active = false, ACT_360, GBLO, 2,
      EUTA, EUTA, TENOR_8M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("EUR-LIBOR-9M", EUR, active = false, ACT_360, GBLO, 2,
      EUTA, EUTA, TENOR_9M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("EUR-LIBOR-10M", EUR, active = false, ACT_360, GBLO, 2,
      EUTA, EUTA, TENOR_10M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("EUR-LIBOR-11M", EUR, active = false, ACT_360, GBLO, 2,
      EUTA, EUTA, TENOR_11M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360),
    IborIndexRow("EUR-LIBOR-12M", EUR, active = true, ACT_360, GBLO, 2,
      EUTA, EUTA, TENOR_12M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), THIRTY_U_360))

  /** The fourteen yen LIBOR rows, `JPY-LIBOR-1W` to `JPY-LIBOR-12M`, six of them active. */
  private def jpyLibor: Vector[IborIndexRow] = Vector(
    IborIndexRow("JPY-LIBOR-1W", JPY, active = true, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+JPTO"), TENOR_1W, "Following",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("JPY-LIBOR-2W", JPY, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+JPTO"), TENOR_2W, "Following",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("JPY-LIBOR-1M", JPY, active = true, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+JPTO"), TENOR_1M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("JPY-LIBOR-2M", JPY, active = true, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+JPTO"), TENOR_2M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("JPY-LIBOR-3M", JPY, active = true, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+JPTO"), TENOR_3M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("JPY-LIBOR-4M", JPY, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+JPTO"), TENOR_4M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("JPY-LIBOR-5M", JPY, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+JPTO"), TENOR_5M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("JPY-LIBOR-6M", JPY, active = true, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+JPTO"), TENOR_6M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("JPY-LIBOR-7M", JPY, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+JPTO"), TENOR_7M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("JPY-LIBOR-8M", JPY, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+JPTO"), TENOR_8M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("JPY-LIBOR-9M", JPY, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+JPTO"), TENOR_9M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("JPY-LIBOR-10M", JPY, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+JPTO"), TENOR_10M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("JPY-LIBOR-11M", JPY, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+JPTO"), TENOR_11M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("JPY-LIBOR-12M", JPY, active = true, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+JPTO"), TENOR_12M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F))

  /** The three yen term risk free rows, `JPY-TORF-1M` to `JPY-TORF-6M`, all of them active. */
  private def jpyTorf: Vector[IborIndexRow] = Vector(
    IborIndexRow("JPY-TORF-1M", JPY, active = true, ACT_365F, JPTO, 2,
      JPTO, JPTO, TENOR_1M, "LastBusinessDay",
      LocalTime.of(15, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TORF-3M", JPY, active = true, ACT_365F, JPTO, 2,
      JPTO, JPTO, TENOR_3M, "LastBusinessDay",
      LocalTime.of(15, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TORF-6M", JPY, active = true, ACT_365F, JPTO, 2,
      JPTO, JPTO, TENOR_6M, "LastBusinessDay",
      LocalTime.of(15, 0), ZoneId.of("Asia/Tokyo"), ACT_365F))

  /** The fourteen dollar LIBOR rows, `USD-LIBOR-1W` to `USD-LIBOR-12M`, six of them active. */
  private def usdLibor: Vector[IborIndexRow] = Vector(
    IborIndexRow("USD-LIBOR-1W", USD, active = true, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+USNY"), TENOR_1W, "Following",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_360),
    IborIndexRow("USD-LIBOR-2W", USD, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+USNY"), TENOR_2W, "Following",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_360),
    IborIndexRow("USD-LIBOR-1M", USD, active = true, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+USNY"), TENOR_1M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_360),
    IborIndexRow("USD-LIBOR-2M", USD, active = true, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+USNY"), TENOR_2M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_360),
    IborIndexRow("USD-LIBOR-3M", USD, active = true, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+USNY"), TENOR_3M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_360),
    IborIndexRow("USD-LIBOR-4M", USD, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+USNY"), TENOR_4M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_360),
    IborIndexRow("USD-LIBOR-5M", USD, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+USNY"), TENOR_5M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_360),
    IborIndexRow("USD-LIBOR-6M", USD, active = true, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+USNY"), TENOR_6M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_360),
    IborIndexRow("USD-LIBOR-7M", USD, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+USNY"), TENOR_7M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_360),
    IborIndexRow("USD-LIBOR-8M", USD, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+USNY"), TENOR_8M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_360),
    IborIndexRow("USD-LIBOR-9M", USD, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+USNY"), TENOR_9M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_360),
    IborIndexRow("USD-LIBOR-10M", USD, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+USNY"), TENOR_10M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_360),
    IborIndexRow("USD-LIBOR-11M", USD, active = false, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+USNY"), TENOR_11M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_360),
    IborIndexRow("USD-LIBOR-12M", USD, active = true, ACT_360, GBLO, 2,
      GBLO, calendar("GBLO+USNY"), TENOR_12M, "LastBusinessDay",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_360))

  /**
   * The four dollar short term bank yield rows, `USD-BSBY-1M` to `USD-BSBY-12M`, all of them
   * active.
   */
  private def usdBsby: Vector[IborIndexRow] = Vector(
    IborIndexRow("USD-BSBY-1M", USD, active = true, ACT_360, USGS, 2,
      USGS, USGS, TENOR_1M, "LastBusinessDay",
      LocalTime.of(8, 0), ZoneId.of("America/New_York"), ACT_360),
    IborIndexRow("USD-BSBY-3M", USD, active = true, ACT_360, USGS, 2,
      USGS, USGS, TENOR_3M, "LastBusinessDay",
      LocalTime.of(8, 0), ZoneId.of("America/New_York"), ACT_360),
    IborIndexRow("USD-BSBY-6M", USD, active = true, ACT_360, USGS, 2,
      USGS, USGS, TENOR_6M, "LastBusinessDay",
      LocalTime.of(8, 0), ZoneId.of("America/New_York"), ACT_360),
    IborIndexRow("USD-BSBY-12M", USD, active = true, ACT_360, USGS, 2,
      USGS, USGS, TENOR_12M, "LastBusinessDay",
      LocalTime.of(8, 0), ZoneId.of("America/New_York"), ACT_360))

  /**
   * The four dollar CME term SOFR rows, `USD-SOFRCMETERM-1M` to `USD-SOFRCMETERM-12M`, all of them
   * active.
   */
  private def usdSofrCmeTerm: Vector[IborIndexRow] = Vector(
    IborIndexRow("USD-SOFRCMETERM-1M", USD, active = true, ACT_360, USGS, 2,
      USGS, USGS, TENOR_1M, "LastBusinessDay",
      LocalTime.of(5, 0), ZoneId.of("America/New_York"), ACT_360),
    IborIndexRow("USD-SOFRCMETERM-3M", USD, active = true, ACT_360, USGS, 2,
      USGS, USGS, TENOR_3M, "LastBusinessDay",
      LocalTime.of(5, 0), ZoneId.of("America/New_York"), ACT_360),
    IborIndexRow("USD-SOFRCMETERM-6M", USD, active = true, ACT_360, USGS, 2,
      USGS, USGS, TENOR_6M, "LastBusinessDay",
      LocalTime.of(5, 0), ZoneId.of("America/New_York"), ACT_360),
    IborIndexRow("USD-SOFRCMETERM-12M", USD, active = true, ACT_360, USGS, 2,
      USGS, USGS, TENOR_12M, "LastBusinessDay",
      LocalTime.of(5, 0), ZoneId.of("America/New_York"), ACT_360))

  /**
   * The two dollar term AMERIBOR rows, `USD-AMERIBORTERM-1M` to `USD-AMERIBORTERM-3M`, all of them
   * active.
   */
  private def usdAmeriborTerm: Vector[IborIndexRow] = Vector(
    IborIndexRow("USD-AMERIBORTERM-1M", USD, active = true, ACT_360, USNY, 2,
      USNY, USNY, TENOR_1M, "LastBusinessDay",
      LocalTime.of(18, 30), ZoneId.of("America/Chicago"), ACT_360),
    IborIndexRow("USD-AMERIBORTERM-3M", USD, active = true, ACT_360, USNY, 2,
      USNY, USNY, TENOR_3M, "LastBusinessDay",
      LocalTime.of(18, 30), ZoneId.of("America/Chicago"), ACT_360))

  /**
   * The fourteen euro EURIBOR rows, `EUR-EURIBOR-1W` to `EUR-EURIBOR-12M`, five of them active.
   */
  private def eurEuribor: Vector[IborIndexRow] = Vector(
    IborIndexRow("EUR-EURIBOR-1W", EUR, active = true, ACT_360, EUTA, 2,
      EUTA, EUTA, TENOR_1W, "Following",
      LocalTime.of(11, 0), ZoneId.of("Europe/Brussels"), THIRTY_U_360),
    IborIndexRow("EUR-EURIBOR-2W", EUR, active = false, ACT_360, EUTA, 2,
      EUTA, EUTA, TENOR_2W, "Following",
      LocalTime.of(11, 0), ZoneId.of("Europe/Brussels"), THIRTY_U_360),
    IborIndexRow("EUR-EURIBOR-1M", EUR, active = true, ACT_360, EUTA, 2,
      EUTA, EUTA, TENOR_1M, "LastBusinessDay",
      LocalTime.of(11, 0), ZoneId.of("Europe/Brussels"), THIRTY_U_360),
    IborIndexRow("EUR-EURIBOR-2M", EUR, active = false, ACT_360, EUTA, 2,
      EUTA, EUTA, TENOR_2M, "LastBusinessDay",
      LocalTime.of(11, 0), ZoneId.of("Europe/Brussels"), THIRTY_U_360),
    IborIndexRow("EUR-EURIBOR-3M", EUR, active = true, ACT_360, EUTA, 2,
      EUTA, EUTA, TENOR_3M, "LastBusinessDay",
      LocalTime.of(11, 0), ZoneId.of("Europe/Brussels"), THIRTY_U_360),
    IborIndexRow("EUR-EURIBOR-4M", EUR, active = false, ACT_360, EUTA, 2,
      EUTA, EUTA, TENOR_4M, "LastBusinessDay",
      LocalTime.of(11, 0), ZoneId.of("Europe/Brussels"), THIRTY_U_360),
    IborIndexRow("EUR-EURIBOR-5M", EUR, active = false, ACT_360, EUTA, 2,
      EUTA, EUTA, TENOR_5M, "LastBusinessDay",
      LocalTime.of(11, 0), ZoneId.of("Europe/Brussels"), THIRTY_U_360),
    IborIndexRow("EUR-EURIBOR-6M", EUR, active = true, ACT_360, EUTA, 2,
      EUTA, EUTA, TENOR_6M, "LastBusinessDay",
      LocalTime.of(11, 0), ZoneId.of("Europe/Brussels"), THIRTY_U_360),
    IborIndexRow("EUR-EURIBOR-7M", EUR, active = false, ACT_360, EUTA, 2,
      EUTA, EUTA, TENOR_7M, "LastBusinessDay",
      LocalTime.of(11, 0), ZoneId.of("Europe/Brussels"), THIRTY_U_360),
    IborIndexRow("EUR-EURIBOR-8M", EUR, active = false, ACT_360, EUTA, 2,
      EUTA, EUTA, TENOR_8M, "LastBusinessDay",
      LocalTime.of(11, 0), ZoneId.of("Europe/Brussels"), THIRTY_U_360),
    IborIndexRow("EUR-EURIBOR-9M", EUR, active = false, ACT_360, EUTA, 2,
      EUTA, EUTA, TENOR_9M, "LastBusinessDay",
      LocalTime.of(11, 0), ZoneId.of("Europe/Brussels"), THIRTY_U_360),
    IborIndexRow("EUR-EURIBOR-10M", EUR, active = false, ACT_360, EUTA, 2,
      EUTA, EUTA, TENOR_10M, "LastBusinessDay",
      LocalTime.of(11, 0), ZoneId.of("Europe/Brussels"), THIRTY_U_360),
    IborIndexRow("EUR-EURIBOR-11M", EUR, active = false, ACT_360, EUTA, 2,
      EUTA, EUTA, TENOR_11M, "LastBusinessDay",
      LocalTime.of(11, 0), ZoneId.of("Europe/Brussels"), THIRTY_U_360),
    IborIndexRow("EUR-EURIBOR-12M", EUR, active = true, ACT_360, EUTA, 2,
      EUTA, EUTA, TENOR_12M, "LastBusinessDay",
      LocalTime.of(11, 0), ZoneId.of("Europe/Brussels"), THIRTY_U_360))

  /**
   * The thirteen yen Japanese TIBOR rows, `JPY-TIBOR-JAPAN-1W` to `JPY-TIBOR-JAPAN-12M`, five of
   * them active.
   */
  private def jpyTiborJapan: Vector[IborIndexRow] = Vector(
    IborIndexRow("JPY-TIBOR-JAPAN-1W", JPY, active = true, ACT_365F, JPTO, 2,
      JPTO, JPTO, TENOR_1W, "Following",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-JAPAN-1M", JPY, active = true, ACT_365F, JPTO, 2,
      JPTO, JPTO, TENOR_1M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-JAPAN-2M", JPY, active = false, ACT_365F, JPTO, 2,
      JPTO, JPTO, TENOR_2M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-JAPAN-3M", JPY, active = true, ACT_365F, JPTO, 2,
      JPTO, JPTO, TENOR_3M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-JAPAN-4M", JPY, active = false, ACT_365F, JPTO, 2,
      JPTO, JPTO, TENOR_4M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-JAPAN-5M", JPY, active = false, ACT_365F, JPTO, 2,
      JPTO, JPTO, TENOR_5M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-JAPAN-6M", JPY, active = true, ACT_365F, JPTO, 2,
      JPTO, JPTO, TENOR_6M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-JAPAN-7M", JPY, active = false, ACT_365F, JPTO, 2,
      JPTO, JPTO, TENOR_7M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-JAPAN-8M", JPY, active = false, ACT_365F, JPTO, 2,
      JPTO, JPTO, TENOR_8M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-JAPAN-9M", JPY, active = false, ACT_365F, JPTO, 2,
      JPTO, JPTO, TENOR_9M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-JAPAN-10M", JPY, active = false, ACT_365F, JPTO, 2,
      JPTO, JPTO, TENOR_10M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-JAPAN-11M", JPY, active = false, ACT_365F, JPTO, 2,
      JPTO, JPTO, TENOR_11M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-JAPAN-12M", JPY, active = true, ACT_365F, JPTO, 2,
      JPTO, JPTO, TENOR_12M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F))

  /**
   * The thirteen yen euroyen TIBOR rows, `JPY-TIBOR-EUROYEN-1W` to `JPY-TIBOR-EUROYEN-12M`, none of
   * them still published.
   */
  private def jpyTiborEuroyen: Vector[IborIndexRow] = Vector(
    IborIndexRow("JPY-TIBOR-EUROYEN-1W", JPY, active = false, ACT_360, JPTO, 2,
      JPTO, JPTO, TENOR_1W, "Following",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-EUROYEN-1M", JPY, active = false, ACT_360, JPTO, 2,
      JPTO, JPTO, TENOR_1M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-EUROYEN-2M", JPY, active = false, ACT_360, JPTO, 2,
      JPTO, JPTO, TENOR_2M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-EUROYEN-3M", JPY, active = false, ACT_360, JPTO, 2,
      JPTO, JPTO, TENOR_3M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-EUROYEN-4M", JPY, active = false, ACT_360, JPTO, 2,
      JPTO, JPTO, TENOR_4M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-EUROYEN-5M", JPY, active = false, ACT_360, JPTO, 2,
      JPTO, JPTO, TENOR_5M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-EUROYEN-6M", JPY, active = false, ACT_360, JPTO, 2,
      JPTO, JPTO, TENOR_6M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-EUROYEN-7M", JPY, active = false, ACT_360, JPTO, 2,
      JPTO, JPTO, TENOR_7M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-EUROYEN-8M", JPY, active = false, ACT_360, JPTO, 2,
      JPTO, JPTO, TENOR_8M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-EUROYEN-9M", JPY, active = false, ACT_360, JPTO, 2,
      JPTO, JPTO, TENOR_9M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-EUROYEN-10M", JPY, active = false, ACT_360, JPTO, 2,
      JPTO, JPTO, TENOR_10M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-EUROYEN-11M", JPY, active = false, ACT_360, JPTO, 2,
      JPTO, JPTO, TENOR_11M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F),
    IborIndexRow("JPY-TIBOR-EUROYEN-12M", JPY, active = false, ACT_360, JPTO, 2,
      JPTO, JPTO, TENOR_12M, "LastBusinessDay",
      LocalTime.of(13, 0), ZoneId.of("Asia/Tokyo"), ACT_365F))

  /**
   * The six Australian dollar bank bill swap rows, `AUD-BBSW-1M` to `AUD-BBSW-6M`, all of them
   * active.
   */
  private def audBbsw: Vector[IborIndexRow] = Vector(
    IborIndexRow("AUD-BBSW-1M", AUD, active = true, ACT_365F, AUSY, 0,
      AUSY, AUSY, TENOR_1M, "ModifiedFollowingBiMonthly",
      LocalTime.of(10, 30), ZoneId.of("Australia/Sydney"), ACT_365F),
    IborIndexRow("AUD-BBSW-2M", AUD, active = true, ACT_365F, AUSY, 0,
      AUSY, AUSY, TENOR_2M, "ModifiedFollowingBiMonthly",
      LocalTime.of(10, 30), ZoneId.of("Australia/Sydney"), ACT_365F),
    IborIndexRow("AUD-BBSW-3M", AUD, active = true, ACT_365F, AUSY, 0,
      AUSY, AUSY, TENOR_3M, "ModifiedFollowingBiMonthly",
      LocalTime.of(10, 30), ZoneId.of("Australia/Sydney"), ACT_365F),
    IborIndexRow("AUD-BBSW-4M", AUD, active = true, ACT_365F, AUSY, 0,
      AUSY, AUSY, TENOR_4M, "ModifiedFollowingBiMonthly",
      LocalTime.of(10, 30), ZoneId.of("Australia/Sydney"), ACT_365F),
    IborIndexRow("AUD-BBSW-5M", AUD, active = true, ACT_365F, AUSY, 0,
      AUSY, AUSY, TENOR_5M, "ModifiedFollowingBiMonthly",
      LocalTime.of(10, 30), ZoneId.of("Australia/Sydney"), ACT_365F),
    IborIndexRow("AUD-BBSW-6M", AUD, active = true, ACT_365F, AUSY, 0,
      AUSY, AUSY, TENOR_6M, "ModifiedFollowingBiMonthly",
      LocalTime.of(10, 30), ZoneId.of("Australia/Sydney"), ACT_365F))

  /**
   * The five Canadian dollar offered rate rows, `CAD-CDOR-1M` to `CAD-CDOR-12M`, three of them
   * active.
   */
  private def cadCdor: Vector[IborIndexRow] = Vector(
    IborIndexRow("CAD-CDOR-1M", CAD, active = true, ACT_365F, CATO, 0,
      CATO, CATO, TENOR_1M, "ModifiedFollowing",
      LocalTime.of(10, 15), ZoneId.of("America/Toronto"), ACT_365F),
    IborIndexRow("CAD-CDOR-2M", CAD, active = true, ACT_365F, CATO, 0,
      CATO, CATO, TENOR_2M, "ModifiedFollowing",
      LocalTime.of(10, 15), ZoneId.of("America/Toronto"), ACT_365F),
    IborIndexRow("CAD-CDOR-3M", CAD, active = true, ACT_365F, CATO, 0,
      CATO, CATO, TENOR_3M, "ModifiedFollowing",
      LocalTime.of(10, 15), ZoneId.of("America/Toronto"), ACT_365F),
    IborIndexRow("CAD-CDOR-6M", CAD, active = false, ACT_365F, CATO, 0,
      CATO, CATO, TENOR_6M, "ModifiedFollowing",
      LocalTime.of(10, 15), ZoneId.of("America/Toronto"), ACT_365F),
    IborIndexRow("CAD-CDOR-12M", CAD, active = false, ACT_365F, CATO, 0,
      CATO, CATO, TENOR_12M, "ModifiedFollowing",
      LocalTime.of(10, 15), ZoneId.of("America/Toronto"), ACT_365F))

  /** The single renminbi repo row, `CNY-REPO-1W`; it is active. */
  private def cnyRepo: Vector[IborIndexRow] = Vector(
    IborIndexRow("CNY-REPO-1W", CNY, active = true, ACT_365F, calendar("CNBE"), 0,
      calendar("CNBE"), calendar("CNBE"), TENOR_1W, "Following",
      LocalTime.of(11, 30), ZoneId.of("Asia/Shanghai"), ACT_365F))

  /**
   * The eight Czech koruna PRIBOR rows, `CZK-PRIBOR-1W` to `CZK-PRIBOR-12M`, all of them active.
   */
  private def czkPribor: Vector[IborIndexRow] = Vector(
    IborIndexRow("CZK-PRIBOR-1W", CZK, active = true, ACT_360, CZPR, 2,
      CZPR, CZPR, TENOR_1W, "Following",
      LocalTime.of(11, 0), ZoneId.of("Europe/Prague"), ACT_360),
    IborIndexRow("CZK-PRIBOR-2W", CZK, active = true, ACT_360, CZPR, 2,
      CZPR, CZPR, TENOR_2W, "Following",
      LocalTime.of(11, 0), ZoneId.of("Europe/Prague"), ACT_360),
    IborIndexRow("CZK-PRIBOR-1M", CZK, active = true, ACT_360, CZPR, 2,
      CZPR, CZPR, TENOR_1M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Prague"), ACT_360),
    IborIndexRow("CZK-PRIBOR-2M", CZK, active = true, ACT_360, CZPR, 2,
      CZPR, CZPR, TENOR_2M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Prague"), ACT_360),
    IborIndexRow("CZK-PRIBOR-3M", CZK, active = true, ACT_360, CZPR, 2,
      CZPR, CZPR, TENOR_3M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Prague"), ACT_360),
    IborIndexRow("CZK-PRIBOR-6M", CZK, active = true, ACT_360, CZPR, 2,
      CZPR, CZPR, TENOR_6M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Prague"), ACT_360),
    IborIndexRow("CZK-PRIBOR-9M", CZK, active = true, ACT_360, CZPR, 2,
      CZPR, CZPR, TENOR_9M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Prague"), ACT_360),
    IborIndexRow("CZK-PRIBOR-12M", CZK, active = true, ACT_360, CZPR, 2,
      CZPR, CZPR, TENOR_12M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Prague"), ACT_360))

  /**
   * The fourteen Danish krone CIBOR rows, `DKK-CIBOR-1W` to `DKK-CIBOR-12M`, eight of them active.
   */
  private def dkkCibor: Vector[IborIndexRow] = Vector(
    IborIndexRow("DKK-CIBOR-1W", DKK, active = true, ACT_360, DKCO, 2,
      DKCO, DKCO, TENOR_1W, "Following",
      LocalTime.of(11, 0), ZoneId.of("Europe/Copenhagen"), THIRTY_U_360),
    IborIndexRow("DKK-CIBOR-2W", DKK, active = true, ACT_360, DKCO, 2,
      DKCO, DKCO, TENOR_2W, "Following",
      LocalTime.of(11, 0), ZoneId.of("Europe/Copenhagen"), THIRTY_U_360),
    IborIndexRow("DKK-CIBOR-1M", DKK, active = true, ACT_360, DKCO, 2,
      DKCO, DKCO, TENOR_1M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Copenhagen"), THIRTY_U_360),
    IborIndexRow("DKK-CIBOR-2M", DKK, active = true, ACT_360, DKCO, 2,
      DKCO, DKCO, TENOR_2M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Copenhagen"), THIRTY_U_360),
    IborIndexRow("DKK-CIBOR-3M", DKK, active = true, ACT_360, DKCO, 2,
      DKCO, DKCO, TENOR_3M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Copenhagen"), THIRTY_U_360),
    IborIndexRow("DKK-CIBOR-4M", DKK, active = false, ACT_360, DKCO, 2,
      DKCO, DKCO, TENOR_4M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Copenhagen"), THIRTY_U_360),
    IborIndexRow("DKK-CIBOR-5M", DKK, active = false, ACT_360, DKCO, 2,
      DKCO, DKCO, TENOR_5M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Copenhagen"), THIRTY_U_360),
    IborIndexRow("DKK-CIBOR-6M", DKK, active = true, ACT_360, DKCO, 2,
      DKCO, DKCO, TENOR_6M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Copenhagen"), THIRTY_U_360),
    IborIndexRow("DKK-CIBOR-7M", DKK, active = false, ACT_360, DKCO, 2,
      DKCO, DKCO, TENOR_7M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Copenhagen"), THIRTY_U_360),
    IborIndexRow("DKK-CIBOR-8M", DKK, active = false, ACT_360, DKCO, 2,
      DKCO, DKCO, TENOR_8M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Copenhagen"), THIRTY_U_360),
    IborIndexRow("DKK-CIBOR-9M", DKK, active = true, ACT_360, DKCO, 2,
      DKCO, DKCO, TENOR_9M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Copenhagen"), THIRTY_U_360),
    IborIndexRow("DKK-CIBOR-10M", DKK, active = false, ACT_360, DKCO, 2,
      DKCO, DKCO, TENOR_10M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Copenhagen"), THIRTY_U_360),
    IborIndexRow("DKK-CIBOR-11M", DKK, active = false, ACT_360, DKCO, 2,
      DKCO, DKCO, TENOR_11M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Copenhagen"), THIRTY_U_360),
    IborIndexRow("DKK-CIBOR-12M", DKK, active = true, ACT_360, DKCO, 2,
      DKCO, DKCO, TENOR_12M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Copenhagen"), THIRTY_U_360))

  /**
   * The fourteen Hong Kong dollar HIBOR rows, `HKD-HIBOR-1W` to `HKD-HIBOR-12M`, seven of them
   * active.
   */
  private def hkdHibor: Vector[IborIndexRow] = Vector(
    IborIndexRow("HKD-HIBOR-1W", HKD, active = true, ACT_365F, calendar("HKHK"), 0,
      calendar("HKHK"), calendar("HKHK"), TENOR_1W, "Following",
      LocalTime.of(11, 55), ZoneId.of("Asia/Hong_Kong"), ACT_365F),
    IborIndexRow("HKD-HIBOR-2W", HKD, active = true, ACT_365F, calendar("HKHK"), 0,
      calendar("HKHK"), calendar("HKHK"), TENOR_2W, "Following",
      LocalTime.of(11, 55), ZoneId.of("Asia/Hong_Kong"), ACT_365F),
    IborIndexRow("HKD-HIBOR-1M", HKD, active = true, ACT_365F, calendar("HKHK"), 0,
      calendar("HKHK"), calendar("HKHK"), TENOR_1M, "ModifiedFollowing",
      LocalTime.of(11, 55), ZoneId.of("Asia/Hong_Kong"), ACT_365F),
    IborIndexRow("HKD-HIBOR-2M", HKD, active = true, ACT_365F, calendar("HKHK"), 0,
      calendar("HKHK"), calendar("HKHK"), TENOR_2M, "ModifiedFollowing",
      LocalTime.of(11, 55), ZoneId.of("Asia/Hong_Kong"), ACT_365F),
    IborIndexRow("HKD-HIBOR-3M", HKD, active = true, ACT_365F, calendar("HKHK"), 0,
      calendar("HKHK"), calendar("HKHK"), TENOR_3M, "ModifiedFollowing",
      LocalTime.of(11, 55), ZoneId.of("Asia/Hong_Kong"), ACT_365F),
    IborIndexRow("HKD-HIBOR-4M", HKD, active = false, ACT_365F, calendar("HKHK"), 0,
      calendar("HKHK"), calendar("HKHK"), TENOR_4M, "ModifiedFollowing",
      LocalTime.of(11, 55), ZoneId.of("Asia/Hong_Kong"), ACT_365F),
    IborIndexRow("HKD-HIBOR-5M", HKD, active = false, ACT_365F, calendar("HKHK"), 0,
      calendar("HKHK"), calendar("HKHK"), TENOR_5M, "ModifiedFollowing",
      LocalTime.of(11, 55), ZoneId.of("Asia/Hong_Kong"), ACT_365F),
    IborIndexRow("HKD-HIBOR-6M", HKD, active = true, ACT_365F, calendar("HKHK"), 0,
      calendar("HKHK"), calendar("HKHK"), TENOR_6M, "ModifiedFollowing",
      LocalTime.of(11, 55), ZoneId.of("Asia/Hong_Kong"), ACT_365F),
    IborIndexRow("HKD-HIBOR-7M", HKD, active = false, ACT_365F, calendar("HKHK"), 0,
      calendar("HKHK"), calendar("HKHK"), TENOR_7M, "ModifiedFollowing",
      LocalTime.of(11, 55), ZoneId.of("Asia/Hong_Kong"), ACT_365F),
    IborIndexRow("HKD-HIBOR-8M", HKD, active = false, ACT_365F, calendar("HKHK"), 0,
      calendar("HKHK"), calendar("HKHK"), TENOR_8M, "ModifiedFollowing",
      LocalTime.of(11, 55), ZoneId.of("Asia/Hong_Kong"), ACT_365F),
    IborIndexRow("HKD-HIBOR-9M", HKD, active = false, ACT_365F, calendar("HKHK"), 0,
      calendar("HKHK"), calendar("HKHK"), TENOR_9M, "ModifiedFollowing",
      LocalTime.of(11, 55), ZoneId.of("Asia/Hong_Kong"), ACT_365F),
    IborIndexRow("HKD-HIBOR-10M", HKD, active = false, ACT_365F, calendar("HKHK"), 0,
      calendar("HKHK"), calendar("HKHK"), TENOR_10M, "ModifiedFollowing",
      LocalTime.of(11, 55), ZoneId.of("Asia/Hong_Kong"), ACT_365F),
    IborIndexRow("HKD-HIBOR-11M", HKD, active = false, ACT_365F, calendar("HKHK"), 0,
      calendar("HKHK"), calendar("HKHK"), TENOR_11M, "ModifiedFollowing",
      LocalTime.of(11, 55), ZoneId.of("Asia/Hong_Kong"), ACT_365F),
    IborIndexRow("HKD-HIBOR-12M", HKD, active = true, ACT_365F, calendar("HKHK"), 0,
      calendar("HKHK"), calendar("HKHK"), TENOR_12M, "ModifiedFollowing",
      LocalTime.of(11, 55), ZoneId.of("Asia/Hong_Kong"), ACT_360))

  /** The fourteen forint BUBOR rows, `HUF-BUBOR-1W` to `HUF-BUBOR-12M`, eight of them active. */
  private def hufBubor: Vector[IborIndexRow] = Vector(
    IborIndexRow("HUF-BUBOR-1W", HUF, active = true, ACT_360, HUBU, 2,
      HUBU, HUBU, TENOR_1W, "Following",
      LocalTime.of(11, 0), ZoneId.of("Europe/Budapest"), ACT_365F),
    IborIndexRow("HUF-BUBOR-2W", HUF, active = true, ACT_360, HUBU, 2,
      HUBU, HUBU, TENOR_2W, "Following",
      LocalTime.of(11, 0), ZoneId.of("Europe/Budapest"), ACT_365F),
    IborIndexRow("HUF-BUBOR-1M", HUF, active = true, ACT_360, HUBU, 2,
      HUBU, HUBU, TENOR_1M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Budapest"), ACT_365F),
    IborIndexRow("HUF-BUBOR-2M", HUF, active = true, ACT_360, HUBU, 2,
      HUBU, HUBU, TENOR_2M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Budapest"), ACT_365F),
    IborIndexRow("HUF-BUBOR-3M", HUF, active = true, ACT_360, HUBU, 2,
      HUBU, HUBU, TENOR_3M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Budapest"), ACT_365F),
    IborIndexRow("HUF-BUBOR-4M", HUF, active = false, ACT_360, HUBU, 2,
      HUBU, HUBU, TENOR_4M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Budapest"), ACT_365F),
    IborIndexRow("HUF-BUBOR-5M", HUF, active = false, ACT_360, HUBU, 2,
      HUBU, HUBU, TENOR_5M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Budapest"), ACT_365F),
    IborIndexRow("HUF-BUBOR-6M", HUF, active = true, ACT_360, HUBU, 2,
      HUBU, HUBU, TENOR_6M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Budapest"), ACT_365F),
    IborIndexRow("HUF-BUBOR-7M", HUF, active = false, ACT_360, HUBU, 2,
      HUBU, HUBU, TENOR_7M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Budapest"), ACT_365F),
    IborIndexRow("HUF-BUBOR-8M", HUF, active = false, ACT_360, HUBU, 2,
      HUBU, HUBU, TENOR_8M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Budapest"), ACT_365F),
    IborIndexRow("HUF-BUBOR-9M", HUF, active = true, ACT_360, HUBU, 2,
      HUBU, HUBU, TENOR_9M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Budapest"), ACT_365F),
    IborIndexRow("HUF-BUBOR-10M", HUF, active = false, ACT_360, HUBU, 2,
      HUBU, HUBU, TENOR_10M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Budapest"), ACT_365F),
    IborIndexRow("HUF-BUBOR-11M", HUF, active = false, ACT_360, HUBU, 2,
      HUBU, HUBU, TENOR_11M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Budapest"), ACT_365F),
    IborIndexRow("HUF-BUBOR-12M", HUF, active = true, ACT_360, HUBU, 2,
      HUBU, HUBU, TENOR_12M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Budapest"), ACT_365F))

  /** The four shekel TELBOR rows, `ILS-TELBOR-1M` to `ILS-TELBOR-12M`, all of them active. */
  private def ilsTelbor: Vector[IborIndexRow] = Vector(
    IborIndexRow("ILS-TELBOR-1M", ILS, active = true, ACT_365F, calendar("ILTA"), 0,
      calendar("ILTA"), calendar("ILTA"), TENOR_1M, "ModifiedFollowing",
      LocalTime.of(12, 0), ZoneId.of("Asia/Jerusalem"), ACT_365F),
    IborIndexRow("ILS-TELBOR-3M", ILS, active = true, ACT_365F, calendar("ILTA"), 0,
      calendar("ILTA"), calendar("ILTA"), TENOR_3M, "ModifiedFollowing",
      LocalTime.of(12, 0), ZoneId.of("Asia/Jerusalem"), ACT_365F),
    IborIndexRow("ILS-TELBOR-6M", ILS, active = true, ACT_365F, calendar("ILTA"), 0,
      calendar("ILTA"), calendar("ILTA"), TENOR_6M, "ModifiedFollowing",
      LocalTime.of(12, 0), ZoneId.of("Asia/Jerusalem"), ACT_365F),
    IborIndexRow("ILS-TELBOR-12M", ILS, active = true, ACT_365F, calendar("ILTA"), 0,
      calendar("ILTA"), calendar("ILTA"), TENOR_12M, "ModifiedFollowing",
      LocalTime.of(12, 0), ZoneId.of("Asia/Jerusalem"), ACT_365F))

  /** The single won certificate of deposit row, `KRW-CD-13W`; it is active. */
  private def krwCd: Vector[IborIndexRow] = Vector(
    IborIndexRow("KRW-CD-13W", KRW, active = true, ACT_365F, calendar("KRSE"), 1,
      calendar("KRSE"), calendar("KRSE"), TENOR_13W, "Following",
      LocalTime.of(15, 30), ZoneId.of("Asia/Seoul"), ACT_365F))

  /** The three Mexican peso TIIE rows, `MXN-TIIE-4W` to `MXN-TIIE-26W`, all of them active. */
  private def mxnTiie: Vector[IborIndexRow] = Vector(
    IborIndexRow("MXN-TIIE-4W", MXN, active = true, ACT_360, MXMC, 1,
      MXMC, MXMC, TENOR_4W, "Following",
      LocalTime.of(14, 0), ZoneId.of("America/Mexico_City"), ACT_360),
    IborIndexRow("MXN-TIIE-13W", MXN, active = true, ACT_360, MXMC, 1,
      MXMC, MXMC, TENOR_13W, "Following",
      LocalTime.of(14, 0), ZoneId.of("America/Mexico_City"), ACT_360),
    IborIndexRow("MXN-TIIE-26W", MXN, active = true, ACT_360, MXMC, 1,
      MXMC, MXMC, TENOR_26W, "Following",
      LocalTime.of(14, 0), ZoneId.of("America/Mexico_City"), ACT_360))

  /** The three ringgit KLIBOR rows, `MYR-KLIBOR-1M` to `MYR-KLIBOR-6M`, all of them active. */
  private def myrKlibor: Vector[IborIndexRow] = Vector(
    IborIndexRow("MYR-KLIBOR-1M", MYR, active = true, ACT_365F, calendar("MYKL"), 0,
      calendar("MYKL"), calendar("MYKL"), TENOR_1M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Asia/Kuala_Lumpur"), ACT_365F),
    IborIndexRow("MYR-KLIBOR-3M", MYR, active = true, ACT_365F, calendar("MYKL"), 0,
      calendar("MYKL"), calendar("MYKL"), TENOR_3M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Asia/Kuala_Lumpur"), ACT_365F),
    IborIndexRow("MYR-KLIBOR-6M", MYR, active = true, ACT_365F, calendar("MYKL"), 0,
      calendar("MYKL"), calendar("MYKL"), TENOR_6M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Asia/Kuala_Lumpur"), ACT_365F))

  /**
   * The ten Norwegian krone NIBOR rows, `NOK-NIBOR-1W` to `NOK-NIBOR-12M`, five of them active.
   */
  private def nokNibor: Vector[IborIndexRow] = Vector(
    IborIndexRow("NOK-NIBOR-1W", NOK, active = true, ACT_360, NOOS, 2,
      NOOS, NOOS, TENOR_1W, "Following",
      LocalTime.of(12, 0), ZoneId.of("Europe/Oslo"), THIRTY_U_360),
    IborIndexRow("NOK-NIBOR-2W", NOK, active = false, ACT_360, NOOS, 2,
      NOOS, NOOS, TENOR_2W, "Following",
      LocalTime.of(12, 0), ZoneId.of("Europe/Oslo"), THIRTY_U_360),
    IborIndexRow("NOK-NIBOR-1M", NOK, active = true, ACT_360, NOOS, 2,
      NOOS, NOOS, TENOR_1M, "ModifiedFollowing",
      LocalTime.of(12, 0), ZoneId.of("Europe/Oslo"), THIRTY_U_360),
    IborIndexRow("NOK-NIBOR-2M", NOK, active = true, ACT_360, NOOS, 2,
      NOOS, NOOS, TENOR_2M, "ModifiedFollowing",
      LocalTime.of(12, 0), ZoneId.of("Europe/Oslo"), THIRTY_U_360),
    IborIndexRow("NOK-NIBOR-3M", NOK, active = true, ACT_360, NOOS, 2,
      NOOS, NOOS, TENOR_3M, "ModifiedFollowing",
      LocalTime.of(12, 0), ZoneId.of("Europe/Oslo"), THIRTY_U_360),
    IborIndexRow("NOK-NIBOR-4M", NOK, active = false, ACT_360, NOOS, 2,
      NOOS, NOOS, TENOR_4M, "ModifiedFollowing",
      LocalTime.of(12, 0), ZoneId.of("Europe/Oslo"), THIRTY_U_360),
    IborIndexRow("NOK-NIBOR-5M", NOK, active = false, ACT_360, NOOS, 2,
      NOOS, NOOS, TENOR_5M, "ModifiedFollowing",
      LocalTime.of(12, 0), ZoneId.of("Europe/Oslo"), THIRTY_U_360),
    IborIndexRow("NOK-NIBOR-6M", NOK, active = true, ACT_360, NOOS, 2,
      NOOS, NOOS, TENOR_6M, "ModifiedFollowing",
      LocalTime.of(12, 0), ZoneId.of("Europe/Oslo"), THIRTY_U_360),
    IborIndexRow("NOK-NIBOR-9M", NOK, active = false, ACT_360, NOOS, 2,
      NOOS, NOOS, TENOR_9M, "ModifiedFollowing",
      LocalTime.of(12, 0), ZoneId.of("Europe/Oslo"), THIRTY_U_360),
    IborIndexRow("NOK-NIBOR-12M", NOK, active = false, ACT_360, NOOS, 2,
      NOOS, NOOS, TENOR_12M, "ModifiedFollowing",
      LocalTime.of(12, 0), ZoneId.of("Europe/Oslo"), THIRTY_U_360))

  /**
   * The six New Zealand dollar bank bill rows, `NZD-BKBM-1M` to `NZD-BKBM-6M`, all of them active.
   */
  private def nzdBkbm: Vector[IborIndexRow] = Vector(
    IborIndexRow("NZD-BKBM-1M", NZD, active = true, ACT_365F, calendar("NZBD"), 0,
      calendar("NZBD"), calendar("NZBD"), TENOR_1M, "ModifiedFollowing",
      LocalTime.of(10, 45), ZoneId.of("Pacific/Auckland"), ACT_365F),
    IborIndexRow("NZD-BKBM-2M", NZD, active = true, ACT_365F, calendar("NZBD"), 0,
      calendar("NZBD"), calendar("NZBD"), TENOR_2M, "ModifiedFollowing",
      LocalTime.of(10, 45), ZoneId.of("Pacific/Auckland"), ACT_365F),
    IborIndexRow("NZD-BKBM-3M", NZD, active = true, ACT_365F, calendar("NZBD"), 0,
      calendar("NZBD"), calendar("NZBD"), TENOR_3M, "ModifiedFollowing",
      LocalTime.of(10, 45), ZoneId.of("Pacific/Auckland"), ACT_365F),
    IborIndexRow("NZD-BKBM-4M", NZD, active = true, ACT_365F, calendar("NZBD"), 0,
      calendar("NZBD"), calendar("NZBD"), TENOR_4M, "ModifiedFollowing",
      LocalTime.of(10, 45), ZoneId.of("Pacific/Auckland"), ACT_365F),
    IborIndexRow("NZD-BKBM-5M", NZD, active = true, ACT_365F, calendar("NZBD"), 0,
      calendar("NZBD"), calendar("NZBD"), TENOR_5M, "ModifiedFollowing",
      LocalTime.of(10, 45), ZoneId.of("Pacific/Auckland"), ACT_365F),
    IborIndexRow("NZD-BKBM-6M", NZD, active = true, ACT_365F, calendar("NZBD"), 0,
      calendar("NZBD"), calendar("NZBD"), TENOR_6M, "ModifiedFollowing",
      LocalTime.of(10, 45), ZoneId.of("Pacific/Auckland"), ACT_365F))

  /** The seven zloty WIBOR rows, `PLN-WIBOR-1W` to `PLN-WIBOR-12M`, five of them active. */
  private def plnWibor: Vector[IborIndexRow] = Vector(
    IborIndexRow("PLN-WIBOR-1W", PLN, active = true, ACT_365F, PLWA, 2,
      PLWA, PLWA, TENOR_1W, "Following",
      LocalTime.of(11, 0), ZoneId.of("Europe/Warsaw"), ACT_ACT_ISDA),
    IborIndexRow("PLN-WIBOR-2W", PLN, active = false, ACT_365F, PLWA, 2,
      PLWA, PLWA, TENOR_2W, "Following",
      LocalTime.of(11, 0), ZoneId.of("Europe/Warsaw"), ACT_ACT_ISDA),
    IborIndexRow("PLN-WIBOR-1M", PLN, active = true, ACT_365F, PLWA, 2,
      PLWA, PLWA, TENOR_1M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Warsaw"), ACT_ACT_ISDA),
    IborIndexRow("PLN-WIBOR-3M", PLN, active = true, ACT_365F, PLWA, 2,
      PLWA, PLWA, TENOR_3M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Warsaw"), ACT_ACT_ISDA),
    IborIndexRow("PLN-WIBOR-6M", PLN, active = true, ACT_365F, PLWA, 2,
      PLWA, PLWA, TENOR_6M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Warsaw"), ACT_ACT_ISDA),
    IborIndexRow("PLN-WIBOR-9M", PLN, active = false, ACT_365F, PLWA, 2,
      PLWA, PLWA, TENOR_9M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Warsaw"), ACT_ACT_ISDA),
    IborIndexRow("PLN-WIBOR-12M", PLN, active = true, ACT_365F, PLWA, 2,
      PLWA, PLWA, TENOR_12M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Warsaw"), ACT_ACT_ISDA))

  /**
   * The five Saudi riyal SAIBOR rows, `SAR-SAIBOR-1W` to `SAR-SAIBOR-12M`, all of them active.
   */
  private def sarSaibor: Vector[IborIndexRow] = Vector(
    IborIndexRow("SAR-SAIBOR-1W", SAR, active = true, ACT_360, calendar("SARI"), 0,
      calendar("SARI"), calendar("SARI"), TENOR_1W, "Following",
      LocalTime.of(11, 0), ZoneId.of("Asia/Riyadh"), ACT_360),
    IborIndexRow("SAR-SAIBOR-1M", SAR, active = true, ACT_360, calendar("SARI"), 0,
      calendar("SARI"), calendar("SARI"), TENOR_1M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Asia/Riyadh"), ACT_360),
    IborIndexRow("SAR-SAIBOR-3M", SAR, active = true, ACT_360, calendar("SARI"), 0,
      calendar("SARI"), calendar("SARI"), TENOR_3M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Asia/Riyadh"), ACT_360),
    IborIndexRow("SAR-SAIBOR-6M", SAR, active = true, ACT_360, calendar("SARI"), 0,
      calendar("SARI"), calendar("SARI"), TENOR_6M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Asia/Riyadh"), ACT_360),
    IborIndexRow("SAR-SAIBOR-12M", SAR, active = true, ACT_360, calendar("SARI"), 0,
      calendar("SARI"), calendar("SARI"), TENOR_12M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Asia/Riyadh"), ACT_360))

  /**
   * The seven Swedish krona STIBOR rows, `SEK-STIBOR-1W` to `SEK-STIBOR-12M`, five of them active.
   */
  private def sekStibor: Vector[IborIndexRow] = Vector(
    IborIndexRow("SEK-STIBOR-1W", SEK, active = true, ACT_360, SEST, 2,
      SEST, SEST, TENOR_1W, "Following",
      LocalTime.of(11, 0), ZoneId.of("Europe/Stockholm"), THIRTY_U_360),
    IborIndexRow("SEK-STIBOR-1M", SEK, active = true, ACT_360, SEST, 2,
      SEST, SEST, TENOR_1M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Stockholm"), THIRTY_U_360),
    IborIndexRow("SEK-STIBOR-2M", SEK, active = true, ACT_360, SEST, 2,
      SEST, SEST, TENOR_2M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Stockholm"), THIRTY_U_360),
    IborIndexRow("SEK-STIBOR-3M", SEK, active = true, ACT_360, SEST, 2,
      SEST, SEST, TENOR_3M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Stockholm"), THIRTY_U_360),
    IborIndexRow("SEK-STIBOR-6M", SEK, active = true, ACT_360, SEST, 2,
      SEST, SEST, TENOR_6M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Stockholm"), THIRTY_U_360),
    IborIndexRow("SEK-STIBOR-9M", SEK, active = false, ACT_360, SEST, 2,
      SEST, SEST, TENOR_9M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Stockholm"), THIRTY_U_360),
    IborIndexRow("SEK-STIBOR-12M", SEK, active = false, ACT_360, SEST, 2,
      SEST, SEST, TENOR_12M, "ModifiedFollowing",
      LocalTime.of(11, 0), ZoneId.of("Europe/Stockholm"), THIRTY_U_360))

  /**
   * The six Singapore dollar SIBOR rows, `SGD-SIBOR-1M` to `SGD-SIBOR-12M`, four of them active.
   */
  private def sgdSibor: Vector[IborIndexRow] = Vector(
    IborIndexRow("SGD-SIBOR-1M", SGD, active = true, ACT_365F, calendar("SGSI"), 2,
      calendar("SGSI"), calendar("SGSI"), TENOR_1M, "ModifiedFollowing",
      LocalTime.of(11, 30), ZoneId.of("Asia/Singapore"), ACT_365F),
    IborIndexRow("SGD-SIBOR-2M", SGD, active = false, ACT_365F, calendar("SGSI"), 2,
      calendar("SGSI"), calendar("SGSI"), TENOR_2M, "ModifiedFollowing",
      LocalTime.of(11, 30), ZoneId.of("Asia/Singapore"), ACT_365F),
    IborIndexRow("SGD-SIBOR-3M", SGD, active = true, ACT_365F, calendar("SGSI"), 2,
      calendar("SGSI"), calendar("SGSI"), TENOR_3M, "ModifiedFollowing",
      LocalTime.of(11, 30), ZoneId.of("Asia/Singapore"), ACT_365F),
    IborIndexRow("SGD-SIBOR-6M", SGD, active = true, ACT_365F, calendar("SGSI"), 2,
      calendar("SGSI"), calendar("SGSI"), TENOR_6M, "ModifiedFollowing",
      LocalTime.of(11, 30), ZoneId.of("Asia/Singapore"), ACT_365F),
    IborIndexRow("SGD-SIBOR-9M", SGD, active = false, ACT_365F, calendar("SGSI"), 2,
      calendar("SGSI"), calendar("SGSI"), TENOR_9M, "ModifiedFollowing",
      LocalTime.of(11, 30), ZoneId.of("Asia/Singapore"), ACT_365F),
    IborIndexRow("SGD-SIBOR-12M", SGD, active = true, ACT_365F, calendar("SGSI"), 2,
      calendar("SGSI"), calendar("SGSI"), TENOR_12M, "ModifiedFollowing",
      LocalTime.of(11, 30), ZoneId.of("Asia/Singapore"), ACT_365F))

  /**
   * The seven Singapore dollar swap offer rate rows, `SGD-SOR-1W` to `SGD-SOR-12M`, three of them
   * active.
   */
  private def sgdSor: Vector[IborIndexRow] = Vector(
    IborIndexRow("SGD-SOR-1W", SGD, active = false, ACT_365F, calendar("SGSI+GBLO"), 2,
      calendar("SGSI+GBLO"), calendar("SGSI+GBLO"), TENOR_1W, "Following",
      LocalTime.of(12, 0), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("SGD-SOR-1M", SGD, active = true, ACT_365F, calendar("SGSI+GBLO"), 2,
      calendar("SGSI+GBLO"), calendar("SGSI+GBLO"), TENOR_1M, "ModifiedFollowing",
      LocalTime.of(12, 0), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("SGD-SOR-2M", SGD, active = false, ACT_365F, calendar("SGSI+GBLO"), 2,
      calendar("SGSI+GBLO"), calendar("SGSI+GBLO"), TENOR_2M, "ModifiedFollowing",
      LocalTime.of(12, 0), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("SGD-SOR-3M", SGD, active = true, ACT_365F, calendar("SGSI+GBLO"), 2,
      calendar("SGSI+GBLO"), calendar("SGSI+GBLO"), TENOR_3M, "ModifiedFollowing",
      LocalTime.of(12, 0), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("SGD-SOR-6M", SGD, active = true, ACT_365F, calendar("SGSI+GBLO"), 2,
      calendar("SGSI+GBLO"), calendar("SGSI+GBLO"), TENOR_6M, "ModifiedFollowing",
      LocalTime.of(12, 0), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("SGD-SOR-9M", SGD, active = false, ACT_365F, calendar("SGSI+GBLO"), 2,
      calendar("SGSI+GBLO"), calendar("SGSI+GBLO"), TENOR_9M, "ModifiedFollowing",
      LocalTime.of(12, 0), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("SGD-SOR-12M", SGD, active = false, ACT_365F, calendar("SGSI+GBLO"), 2,
      calendar("SGSI+GBLO"), calendar("SGSI+GBLO"), TENOR_12M, "ModifiedFollowing",
      LocalTime.of(12, 0), ZoneId.of("Europe/London"), ACT_365F))

  /** The seven baht THBFIX rows, `THB-THBFIX-1W` to `THB-THBFIX-12M`, five of them active. */
  private def thbThbFix: Vector[IborIndexRow] = Vector(
    IborIndexRow("THB-THBFIX-1W", THB, active = true, ACT_365F, THBA, 2,
      THBA, THBA, TENOR_1W, "Following",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("THB-THBFIX-1M", THB, active = true, ACT_365F, THBA, 2,
      THBA, THBA, TENOR_1M, "ModifiedFollowing",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("THB-THBFIX-2M", THB, active = false, ACT_365F, THBA, 2,
      THBA, THBA, TENOR_2M, "ModifiedFollowing",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("THB-THBFIX-3M", THB, active = true, ACT_365F, THBA, 2,
      THBA, THBA, TENOR_3M, "ModifiedFollowing",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("THB-THBFIX-6M", THB, active = true, ACT_365F, THBA, 2,
      THBA, THBA, TENOR_6M, "ModifiedFollowing",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("THB-THBFIX-9M", THB, active = false, ACT_365F, THBA, 2,
      THBA, THBA, TENOR_9M, "ModifiedFollowing",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F),
    IborIndexRow("THB-THBFIX-12M", THB, active = true, ACT_365F, THBA, 2,
      THBA, THBA, TENOR_12M, "ModifiedFollowing",
      LocalTime.of(11, 55), ZoneId.of("Europe/London"), ACT_365F))

  /**
   * The seven Taiwan dollar TAIBOR rows, `TWD-TAIBOR-1W` to `TWD-TAIBOR-12M`, all of them active.
   */
  private def twdTaibor: Vector[IborIndexRow] = Vector(
    IborIndexRow("TWD-TAIBOR-1W", TWD, active = true, ACT_365F, calendar("TWTA"), 2,
      calendar("TWTA"), calendar("TWTA"), TENOR_1W, "Following",
      LocalTime.of(11, 30), ZoneId.of("Asia/Taipei"), ACT_365F),
    IborIndexRow("TWD-TAIBOR-1M", TWD, active = true, ACT_365F, calendar("TWTA"), 2,
      calendar("TWTA"), calendar("TWTA"), TENOR_1M, "ModifiedFollowing",
      LocalTime.of(11, 30), ZoneId.of("Asia/Taipei"), ACT_365F),
    IborIndexRow("TWD-TAIBOR-2M", TWD, active = true, ACT_365F, calendar("TWTA"), 2,
      calendar("TWTA"), calendar("TWTA"), TENOR_2M, "ModifiedFollowing",
      LocalTime.of(11, 30), ZoneId.of("Asia/Taipei"), ACT_365F),
    IborIndexRow("TWD-TAIBOR-3M", TWD, active = true, ACT_365F, calendar("TWTA"), 2,
      calendar("TWTA"), calendar("TWTA"), TENOR_3M, "ModifiedFollowing",
      LocalTime.of(11, 30), ZoneId.of("Asia/Taipei"), ACT_365F),
    IborIndexRow("TWD-TAIBOR-6M", TWD, active = true, ACT_365F, calendar("TWTA"), 2,
      calendar("TWTA"), calendar("TWTA"), TENOR_6M, "ModifiedFollowing",
      LocalTime.of(11, 30), ZoneId.of("Asia/Taipei"), ACT_365F),
    IborIndexRow("TWD-TAIBOR-9M", TWD, active = true, ACT_365F, calendar("TWTA"), 2,
      calendar("TWTA"), calendar("TWTA"), TENOR_9M, "ModifiedFollowing",
      LocalTime.of(11, 30), ZoneId.of("Asia/Taipei"), ACT_365F),
    IborIndexRow("TWD-TAIBOR-12M", TWD, active = true, ACT_365F, calendar("TWTA"), 2,
      calendar("TWTA"), calendar("TWTA"), TENOR_12M, "ModifiedFollowing",
      LocalTime.of(11, 30), ZoneId.of("Asia/Taipei"), ACT_365F))

  /** The five rand JIBAR rows, `ZAR-JIBAR-1M` to `ZAR-JIBAR-12M`, four of them active. */
  private def zarJibar: Vector[IborIndexRow] = Vector(
    IborIndexRow("ZAR-JIBAR-1M", ZAR, active = true, ACT_365F, ZAJO, 0,
      ZAJO, ZAJO, TENOR_1M, "ModifiedFollowing",
      LocalTime.of(10, 0), ZoneId.of("Africa/Johannesburg"), ACT_365F),
    IborIndexRow("ZAR-JIBAR-3M", ZAR, active = true, ACT_365F, ZAJO, 0,
      ZAJO, ZAJO, TENOR_3M, "ModifiedFollowing",
      LocalTime.of(10, 0), ZoneId.of("Africa/Johannesburg"), ACT_365F),
    IborIndexRow("ZAR-JIBAR-6M", ZAR, active = true, ACT_365F, ZAJO, 0,
      ZAJO, ZAJO, TENOR_6M, "ModifiedFollowing",
      LocalTime.of(10, 0), ZoneId.of("Africa/Johannesburg"), ACT_365F),
    IborIndexRow("ZAR-JIBAR-9M", ZAR, active = false, ACT_365F, ZAJO, 0,
      ZAJO, ZAJO, TENOR_9M, "ModifiedFollowing",
      LocalTime.of(10, 0), ZoneId.of("Africa/Johannesburg"), ACT_365F),
    IborIndexRow("ZAR-JIBAR-12M", ZAR, active = true, ACT_365F, ZAJO, 0,
      ZAJO, ZAJO, TENOR_12M, "ModifiedFollowing",
      LocalTime.of(10, 0), ZoneId.of("Africa/Johannesburg"), ACT_365F))

  /**
   * Obtains a calendar identifier from the text of a calendar column.
   *
   * This is the total identifier factory under a shorter name, used for the columns whose text
   * this library names no constant for and for the composite columns. Any name is accepted,
   * whether or not this library ships the calendar it names, and a name joining several calendars
   * is normalised by the factory; see [[HolidayCalendarId.of]] for both. An identifier naming a
   * calendar that is not shipped is admitted here rather than rejected, and fails to resolve
   * against reference data at the point of use.
   *
   * @param uniqueName  the text of the calendar column
   * @return the calendar identifier
   */
  private def calendar(uniqueName: String): HolidayCalendarId =
    HolidayCalendarId.of(uniqueName)
}
