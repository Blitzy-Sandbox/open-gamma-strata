/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.date.DayCount
import com.opengamma.strata.basics.date.DayCounts
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.basics.date.HolidayCalendarIds
import com.opengamma.strata.basics.date.StandardHolidayCalendars

/**
 * A single row of transcribed Overnight index reference data.
 *
 * Each row corresponds to one entry of the Overnight index reference data that the Java
 * implementation located on the classpath and parsed on first use, and carries exactly the eight
 * columns that data declared, in the order it declared them: the index name, the currency the
 * index is quoted in, whether the index is still published, the day count of the index, the
 * calendar that determines the days the index is fixed on, the number of days from a fixing date
 * to the date the fixing is published on, the number of days from a fixing date to the date the
 * deposit implied by the fixing starts on, and the day count a fixed leg conventionally uses
 * against this index.
 *
 * ===Data, not behaviour===
 *
 * The row carries no derived value and no calculation. In particular it does not carry the
 * publication, effective or maturity date of an observation: those are functions of a fixing date,
 * of the two offsets below and of the resolved fixing calendar, and computing them is the work of
 * the index family in `Index.scala`, which reads these rows to build its members. Nor does it
 * carry a fixing time or zone, because the original data declared no such column for an Overnight
 * index.
 *
 * ===The two day count columns are independent===
 *
 * The day count of the index and the day count of a conventional fixed leg quoted against it are
 * separate columns and are not always equal, so they are held as two fields and never collapsed
 * into one. They coincide on thirty-four of the thirty-five rows and differ on the Norwegian row,
 * whose index accrues on an actual/actual year basis while a fixed leg against it accrues on
 * actual/360.
 *
 * @param name                   the unique name of the index, such as `GBP-SONIA`; the name is the
 *                               identity of the index in text, in JSON and to a user, so it is
 *                               reproduced exactly as the original data spelled it, punctuation
 *                               included
 * @param currency               the currency the index is quoted in
 * @param active                 whether the index is currently published, false once it has been
 *                               discontinued
 * @param dayCount               the day count the index itself accrues on
 * @param fixingCalendar         the calendar that determines the days the index is fixed on
 * @param publicationOffsetDays  the number of days from a fixing date to the date the fixing is
 *                               published on, zero when the rate is published on the fixing date
 *                               itself and one when it is published the following business day
 * @param effectiveOffsetDays    the number of days from a fixing date to the date the deposit
 *                               implied by the fixing starts on, zero for an overnight rate and
 *                               one or two for a tomorrow/next rate
 * @param fixedLegDayCount       the day count a fixed leg conventionally uses against this index,
 *                               which is the day count of the index on every row but the Norwegian
 *                               one
 *
 * The row is a transcription of one line of the reference data this module was built from, so it
 * is visible within `com.opengamma.strata.basics` and no further - the same visibility
 * `PriceIndexRow` has, and for the same reason. It is the shape the table below is written in
 * rather than a value of the published API: a caller reads this data as the index family the
 * companion of `Index.scala` builds from it, never as rows, so publishing the row type would add
 * a type to the module's surface that nothing outside it can use and that the port's construction
 * and codec inventories would then have to account for.
 */
private[basics] final case class OvernightIndexRow(
    name: String,
    currency: Currency,
    active: Boolean,
    dayCount: DayCount,
    fixingCalendar: HolidayCalendarId,
    publicationOffsetDays: Int,
    effectiveOffsetDays: Int,
    fixedLegDayCount: DayCount)

/**
 * The Overnight index reference data of the module, expressed as immutable Scala data.
 *
 * This object carries the Overnight index table transcribed verbatim from the reference data that
 * the Java implementation located on the classpath and parsed on first use. Nothing is located,
 * parsed or cached at run time here: the thirty-five rows below are Scala literals fixed at compile
 * time, so the table cannot fail to initialise, needs no classpath, recovers from no load failure
 * and holds no hidden global state. Where the Java loader logged a severe error and handed back an
 * empty map when a row would not parse - silently leaving the whole family without members - there
 * is no such failure mode to recover from, because a row that does not typecheck does not compile.
 *
 * It holds data only and no behaviour: it constructs no [[OvernightIndex]], because creating the
 * members of that family, and giving them the lookups, the date calculations and the observation
 * machinery an index needs, belongs to `Index.scala`, which reads [[rows]] to do it. The dependency
 * runs one way, from this object towards the currency, day count and calendar types it names, and
 * from `Index.scala` towards this object; nothing in this file refers to any other member of the
 * index package.
 *
 * ===Invariants===
 *
 * All of the following are pinned against the Java captured reference data manifest, so a self
 * consistent but mistranscribed row cannot pass:
 *
 *  - [[rows]] holds exactly thirty-five entries with distinct names, in the declaration order of
 *    the original data: the nine major currency rates first, then the remaining rates grouped by
 *    currency in alphabetical order of currency code.
 *  - Every row but one is active. The Swiss tomorrow/next rate is the single discontinued index and
 *    is kept rather than deleted, because a rate that is no longer published is still needed to
 *    value a trade that references it.
 *  - Thirty-three rows accrue on actual/360 or actual/365 fixed - fourteen on the former and
 *    nineteen on the latter. The two exceptions are the Brazilian rate, which accrues on the
 *    business days of the Brazilian calendar, and the Norwegian rate, which accrues on an
 *    actual/actual year basis while its fixed leg accrues on actual/360.
 *  - The two offsets are transcribed per row and follow no pattern that may be assumed: most rates
 *    publish on the fixing date or the next business day and are effective on the fixing date,
 *    while the Danish rates offset both by a day, the Swedish and the Swiss tomorrow/next rates are
 *    effective a day after the fixing, and the Thai rate is effective two days after it.
 *  - Ten distinct fixing calendars, on eleven of the rows, name a calendar whose holidays this
 *    library does not ship - the Chilean, Colombian, Hong Kong, Indonesian, Israeli, Indian,
 *    Russian, Saudi, Singaporean and Turkish ones, the Singaporean one twice - because the original
 *    shipped none either, so those indices resolve against `ReferenceData.standard` with a missing
 *    data failure, exactly as they did there. Nothing is invented to fill the gap.
 *  - One fixing calendar is composite: the New Zealand rate fixes on the days that are business
 *    days in both Auckland and Wellington.
 *
 * ===What this table deliberately omits===
 *
 * There is no upper case key space in [[byName]]. Registering each name a second time in upper
 * case was how the Java registry answered a case insensitive lookup, and that responsibility now
 * belongs to the named enum support in `strata-collect`, which derives the upper case view from the
 * canonical one.
 *
 * There is likewise no alternate name table, although this family has the largest one: the ten
 * alternate names that resolve to six of the rows below - among them the former names of the euro
 * and the Japanese overnight rates, and four spellings of the United States federal funds rate -
 * are a property of the family's lookup rather than of a row of data, so they live with the family
 * in `Index.scala`.
 *
 * No index may be added to, removed from or edited in this table: it is a transcription of existing
 * reference data, and introducing a new index is outside the scope of the port.
 *
 * All members are immutable values, so this object is thread-safe.
 *
 * The table is visible within `com.opengamma.strata.basics` and no further, as `PriceIndexData`
 * is: it is the transcribed reference data the index family is built from, and the family is what
 * a caller uses.
 */
private[basics] object OvernightIndexData {

  //-------------------------------------------------------------------------
  // The four day counts and the calendar identifiers the rows below are built from.
  //
  // The day counts are bound to names before the table rather than after it, because the fields of
  // an object are initialised in the order they are declared, so a value the table reads has to be
  // declared above it. Each name is the day count column value it stands for, written once here
  // instead of once in each of the seventy day count cells of the table, which keeps every row on
  // one line and diffable against the original data.
  //-------------------------------------------------------------------------

  /**
   * The 'Act/360' day count, which is the accrual basis of fourteen of the rows and the fixed leg
   * basis of fifteen of them - the fifteenth being the Norwegian row, whose two day counts differ.
   */
  private val act360: DayCount = DayCounts.ACT_360

  /** The 'Act/365F' day count, which is both day counts of nineteen of the rows. */
  private val act365F: DayCount = DayCounts.ACT_365F

  /** The 'Act/Act Year' day count, the accrual basis of the Norwegian rate alone. */
  private val actActYear: DayCount = DayCounts.ACT_ACT_YEAR

  /**
   * The 'Bus/252 BRBD' day count, the accrual basis of the Brazilian rate and of a fixed leg
   * quoted against it.
   *
   * This day count is not a singleton of the family: it counts the business days of a particular
   * calendar, which is part of its identity and appears in its name, so it is built from the
   * built-in Brazilian calendar by the total factory [[DayCount.ofBus252]]. It is built once and
   * shared by both day count columns of that row, so the two columns hold the same instance rather
   * than two equal ones.
   *
   * The calendar itself is generated when this value is first read, which is when this object is
   * initialised. That cost is paid once per process and is the behaviour of the implementation
   * being ported, whose Brazilian day count likewise resolved its calendar the first time it was
   * needed.
   */
  private val bus252Brbd: DayCount = DayCount.ofBus252(StandardHolidayCalendars.BRBD)

  /**
   * Obtains a calendar identifier from the text of the fixing calendar column.
   *
   * This is the total identifier factory under a shorter name, used for the calendars this library
   * does not name with a constant, so that each row of the table fits on one line and stays
   * diffable against the original data. Any name is accepted, whether or not this library ships
   * the calendar it names, and a name joining several calendars is normalised by the factory; see
   * [[HolidayCalendarId.of]] for both. An identifier naming a calendar that is not shipped fails
   * to resolve against reference data at the point of use, which is the behaviour of the original
   * and is why such an identifier is admitted here rather than rejected.
   *
   * @param uniqueName  the text of the fixing calendar column
   * @return the calendar identifier
   */
  private def calendar(uniqueName: String): HolidayCalendarId = HolidayCalendarId.of(uniqueName)

  //-------------------------------------------------------------------------
  /**
   * The thirty-five transcribed Overnight index rows, in the declaration order of the original
   * data.
   *
   * The order is observable through any iteration a consumer performs - notably the order in which
   * `Index.scala` creates the members of the Overnight index family - so it is kept stable and
   * deterministic rather than re-sorted. The columns appear in the order the original data declared
   * them, so the literal list can be diffed line for line against it.
   */
  val rows: Vector[OvernightIndexRow] = Vector(
    OvernightIndexRow("GBP-SONIA", Currency.GBP, active = true, act365F, HolidayCalendarIds.GBLO, 1, 0, act365F),
    OvernightIndexRow("CHF-SARON", Currency.CHF, active = true, act360, HolidayCalendarIds.CHZU, 0, 0, act360),
    OvernightIndexRow("CHF-TOIS", Currency.CHF, active = false, act360, HolidayCalendarIds.CHZU, 0, 1, act360),
    OvernightIndexRow("EUR-EONIA", Currency.EUR, active = true, act360, HolidayCalendarIds.EUTA, 0, 0, act360),
    OvernightIndexRow("EUR-ESTR", Currency.EUR, active = true, act360, HolidayCalendarIds.EUTA, 1, 0, act360),
    OvernightIndexRow("JPY-TONAR", Currency.JPY, active = true, act365F, HolidayCalendarIds.JPTO, 1, 0, act365F),
    OvernightIndexRow("USD-FED-FUND", Currency.USD, active = true, act360, HolidayCalendarIds.USNY, 1, 0, act360),
    OvernightIndexRow("USD-SOFR", Currency.USD, active = true, act360, HolidayCalendarIds.USGS, 1, 0, act360),
    OvernightIndexRow("USD-AMERIBOR", Currency.USD, active = true, act360, HolidayCalendarIds.USNY, 0, 0, act360),
    // the standard name of each remaining rate, by currency
    OvernightIndexRow("AUD-AONIA", Currency.AUD, active = true, act365F, HolidayCalendarIds.AUSY, 0, 0, act365F),
    OvernightIndexRow("BRL-CDI", Currency.BRL, active = true, bus252Brbd, HolidayCalendarIds.BRBD, 1, 0, bus252Brbd),
    OvernightIndexRow("CAD-CORRA", Currency.CAD, active = true, act365F, HolidayCalendarIds.CATO, 1, 0, act365F),
    OvernightIndexRow("CLP-TNA", Currency.CLP, active = true, act360, calendar("CLSA"), 0, 0, act360),
    OvernightIndexRow("COP-OIBR", Currency.COP, active = true, act360, calendar("COBO"), 0, 0, act360),
    OvernightIndexRow("CZK-CZEONIA", Currency.CZK, active = true, act360, HolidayCalendarIds.CZPR, 1, 0, act360),
    OvernightIndexRow("DKK-TNR", Currency.DKK, active = true, act360, HolidayCalendarIds.DKCO, 1, 1, act360),
    OvernightIndexRow("DKK-DESTR", Currency.DKK, active = true, act360, HolidayCalendarIds.DKCO, 1, 1, act360),
    OvernightIndexRow("HKD-HONIA", Currency.HKD, active = true, act365F, calendar("HKHK"), 0, 0, act365F),
    OvernightIndexRow("HUF-HUFONIA", Currency.HUF, active = true, act365F, HolidayCalendarIds.HUBU, 1, 0, act365F),
    // has replaced the overnight Jakarta interbank rate
    OvernightIndexRow("IDR-INDONIA", Currency.IDR, active = true, act365F, calendar("IDJA"), 1, 0, act365F),
    // the Tel Aviv rate, prefixed by O for uniqueness against the term rates of the same name
    OvernightIndexRow("ILS-OTELBOR", Currency.ILS, active = true, act365F, calendar("ILTA"), 1, 0, act365F),
    // the Mumbai rate, prefixed by O for uniqueness against the term rates of the same name
    OvernightIndexRow("INR-OMIBOR", Currency.INR, active = true, act365F, calendar("INMU"), 0, 0, act365F),
    OvernightIndexRow("NOK-NOWA", Currency.NOK, active = true, actActYear, HolidayCalendarIds.NOOS, 0, 0, act360),
    OvernightIndexRow("NZD-NZIONA", Currency.NZD, active = true, act365F, calendar("NZAU+NZWE"), 0, 0, act365F),
    OvernightIndexRow("PLN-POLONIA", Currency.PLN, active = true, act365F, HolidayCalendarIds.PLWA, 0, 0, act365F),
    OvernightIndexRow("PLN-POLSTR", Currency.PLN, active = true, act365F, HolidayCalendarIds.PLWA, 0, 0, act365F),
    OvernightIndexRow("RUB-RUONIA", Currency.RUB, active = true, act365F, calendar("RUMO"), 1, 0, act365F),
    // the Riyadh rate, prefixed by O for uniqueness against the term rates of the same name
    OvernightIndexRow("SAR-OSAIBOR", Currency.SAR, active = true, act365F, calendar("SARI"), 1, 0, act365F),
    // the same rate as the tomorrow/next Stockholm interbank rate
    OvernightIndexRow("SEK-SIOR", Currency.SEK, active = true, act360, HolidayCalendarIds.SEST, 0, 1, act360),
    OvernightIndexRow("SEK-SWESTR", Currency.SEK, active = true, act360, HolidayCalendarIds.SEST, 0, 1, act360),
    OvernightIndexRow("SGD-SONAR", Currency.SGD, active = true, act365F, calendar("SGSI"), 0, 0, act365F),
    // the alternative reference rate for the Singapore dollar
    OvernightIndexRow("SGD-SORA", Currency.SGD, active = true, act365F, calendar("SGSI"), 1, 0, act365F),
    OvernightIndexRow("THB-THOR", Currency.THB, active = true, act365F, HolidayCalendarIds.THBA, 0, 2, act365F),
    // not the same rate as the overnight Turkish lira interbank rate, and more widely used
    OvernightIndexRow("TRY-TLREF", Currency.TRY, active = true, act365F, calendar("TRIS"), 1, 0, act365F),
    OvernightIndexRow("ZAR-SABOR", Currency.ZAR, active = true, act365F, HolidayCalendarIds.ZAJO, 0, 0, act365F))

  /**
   * The transcribed rows keyed by the name of the index.
   *
   * Derived from [[rows]] rather than transcribed a second time, since a second transcription would
   * be a second opportunity to be wrong, and the names are distinct so the derivation loses
   * nothing. The keys are the canonical names alone: a lookup that folds case, or that follows an
   * alternate name, is the business of the named enum support rather than of this table.
   */
  val byName: Map[String, OvernightIndexRow] = rows.iterator.map(row => row.name -> row).toMap
}
