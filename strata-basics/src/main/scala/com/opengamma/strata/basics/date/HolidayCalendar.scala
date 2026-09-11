/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.DayOfWeek
import java.time.LocalDate

import scala.annotation.tailrec
import scala.collection.immutable.Set
import scala.collection.immutable.SortedSet

import cats.Hash
import cats.Order
import cats.Show

import io.circe.ACursor
import io.circe.Codec
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.ReferenceDataId
import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * A holiday calendar, telling business days apart from holidays.
 *
 * A calendar answers one question - whether a date is a business day - and everything else it
 * offers is built from that answer: the next or previous business day, a shift by a number of
 * business days, the count of business days in a range. Weekends are holidays like any other,
 * because which days form the weekend is itself a property of the centre being modelled.
 *
 * A calendar is located by its [[HolidayCalendarId]] in the
 * [[com.opengamma.strata.basics.ReferenceData]] a caller supplies, so that an application whose
 * own holidays are what matter can supply its own calendar under a well-known identifier. The
 * identifier is part of the calendar, which is why combining two calendars also combines their
 * identifiers and a resolved calendar can always say what it is.
 *
 * The family is closed, and every member of it is in this file: the calendar with no holidays,
 * the three weekend-only calendars, the calendar built from a list of dates, and the two
 * composites that read a pair of calendars together. That is what lets a caller match on a
 * calendar exhaustively, and it is why no registry of calendar implementations is needed.
 *
 * ===Composition===
 *
 * [[combinedWith]] observes the holidays of both calendars, which is what a cash flow settled in
 * two centres requires; [[linkedWith]] observes only the holidays both agree on, which is what a
 * date defined by one centre but published by another requires. Both are total and both keep the
 * identifiers in step with the values, so a combined calendar is named as its identifier is.
 *
 * ===Range of dates===
 *
 * Every method here is total over the dates a calendar knows about, and the two methods that
 * take a range require that range to be the right way round. A date whose year lies outside 0 to
 * 9999 is rejected by the calendars that hold holiday data, because a calendar cannot answer for
 * a date it could never have been given data for; that rejection is a fault in the calling code
 * rather than a property of market data, so it fails fast through
 * [[com.opengamma.strata.collect.ArgCheck]] exactly as the library being ported did.
 *
 * Every implementation is immutable and safe to share between threads.
 */
sealed trait HolidayCalendar extends Named {

  /**
   * The identifier of this calendar.
   *
   * @return the identifier
   */
  def id: HolidayCalendarId

  /**
   * The name of this calendar, which is the name of its identifier.
   *
   * @return the name, such as `GBLO` or `GBLO+USNY`
   */
  override def name: String = id.name

  /**
   * Checks whether the specified date is a holiday.
   *
   * This is the single question a calendar answers; weekends are holidays.
   *
   * @param date  the date to check
   * @return true where the date is a holiday
   * @throws IllegalArgumentException where the date is outside the range this calendar supports
   */
  def isHoliday(date: LocalDate): Boolean

  /**
   * Checks whether the specified date is a business day.
   *
   * @param date  the date to check
   * @return true where the date is a business day
   * @throws IllegalArgumentException where the date is outside the range this calendar supports
   */
  def isBusinessDay(date: LocalDate): Boolean = !isHoliday(date)

  /**
   * Returns an adjuster that shifts a date by a number of business days.
   *
   * The adjuster is a [[DateAdjuster]], which is also a `java.time.temporal.TemporalAdjuster`,
   * so it may be applied either by calling it or by handing it to a date:
   *
   * {{{
   * val threeDaysLater = calendar.adjustBy(3).adjust(date)
   * val twoDaysEarlier = date.`with`(calendar.adjustBy(-2))
   * }}}
   *
   * @param amount  the number of business days to shift by, which may be negative
   * @return the adjuster that applies that shift
   */
  def adjustBy(amount: Int): DateAdjuster = DateAdjuster(date => shift(date, amount))

  /**
   * Finds the next business day, returning the date itself where it is one already.
   *
   * @param date  the date to adjust
   * @return the date itself where it is a business day, otherwise the next business day
   * @throws IllegalArgumentException where the calculation leaves the range this calendar
   *   supports
   */
  def nextOrSame(date: LocalDate): LocalDate = HolidayCalendar.businessDayFrom(this, date, 1)

  /**
   * Finds the next business day, always returning a later date.
   *
   * @param date  the date to adjust
   * @return the first business day after the date
   * @throws IllegalArgumentException where the calculation leaves the range this calendar
   *   supports
   */
  def next(date: LocalDate): LocalDate = nextOrSame(LocalDateUtils.plusDays(date, 1))

  /**
   * Finds the previous business day, returning the date itself where it is one already.
   *
   * @param date  the date to adjust
   * @return the date itself where it is a business day, otherwise the previous business day
   * @throws IllegalArgumentException where the calculation leaves the range this calendar
   *   supports
   */
  def previousOrSame(date: LocalDate): LocalDate = HolidayCalendar.businessDayFrom(this, date, -1)

  /**
   * Finds the previous business day, always returning an earlier date.
   *
   * @param date  the date to adjust
   * @return the first business day before the date
   * @throws IllegalArgumentException where the calculation leaves the range this calendar
   *   supports
   */
  def previous(date: LocalDate): LocalDate = previousOrSame(LocalDateUtils.plusDays(date, -1))

  /**
   * Finds the next business day within the same month, or the last business day of the month
   * where the next business day would fall in the month after.
   *
   * This is the adjustment the modified-following business day convention makes, so the result
   * can be earlier than the date given.
   *
   * @param date  the date to adjust
   * @return the adjusted date
   * @throws IllegalArgumentException where the calculation leaves the range this calendar
   *   supports
   */
  def nextSameOrLastInMonth(date: LocalDate): LocalDate = {
    val candidate = nextOrSame(date)
    if (candidate.getMonthValue != date.getMonthValue) previous(date) else candidate
  }

  /**
   * Checks whether the specified date is the last business day of its month.
   *
   * @param date  the date to check
   * @return true where the date is a business day and the next one falls in another month
   * @throws IllegalArgumentException where the date is outside the range this calendar supports
   */
  def isLastBusinessDayOfMonth(date: LocalDate): Boolean =
    isBusinessDay(date) && next(date).getMonthValue != date.getMonthValue

  /**
   * Calculates the last business day of the month the specified date falls in.
   *
   * @param date  the date to examine
   * @return the last business day of that month
   * @throws IllegalArgumentException where the date is outside the range this calendar supports
   */
  def lastBusinessDayOfMonth(date: LocalDate): LocalDate =
    previousOrSame(date.withDayOfMonth(date.lengthOfMonth))

  /**
   * Shifts the specified date by a number of business days.
   *
   * A positive amount chooses later business days and a negative amount earlier ones; zero
   * returns the date unchanged, business day or not.
   *
   * @param date  the date to shift
   * @param amount  the number of business days to shift by
   * @return the shifted date
   * @throws IllegalArgumentException where the calculation leaves the range this calendar
   *   supports
   */
  def shift(date: LocalDate, amount: Int): LocalDate = HolidayCalendar.shifted(this, date, amount)

  /**
   * Counts the business days in the specified range.
   *
   * @param startInclusive  the start date, included in the count
   * @param endExclusive  the end date, excluded from the count
   * @return the number of business days in the range, zero where the dates are equal
   * @throws IllegalArgumentException where the end date is before the start date, or either date
   *   is outside the range this calendar supports
   */
  def daysBetween(startInclusive: LocalDate, endExclusive: LocalDate): Int =
    businessDays(startInclusive, endExclusive).size

  /**
   * Returns the business days in the specified range.
   *
   * The dates are produced lazily and in ascending order, so a caller may stop reading at any
   * point without the remainder being computed.
   *
   * @param startInclusive  the start date, included in the result
   * @param endExclusive  the end date, excluded from the result
   * @return the business days of the range, in ascending order
   * @throws IllegalArgumentException where the end date is before the start date
   */
  def businessDays(startInclusive: LocalDate, endExclusive: LocalDate): Iterator[LocalDate] = {
    HolidayCalendar.checkInOrder(startInclusive, endExclusive)
    LocalDateUtils.dates(startInclusive, endExclusive).filter(date => isBusinessDay(date))
  }

  /**
   * Returns the holidays in the specified range.
   *
   * The dates are produced lazily and in ascending order, and weekends are holidays.
   *
   * @param startInclusive  the start date, included in the result
   * @param endExclusive  the end date, excluded from the result
   * @return the holidays of the range, in ascending order
   * @throws IllegalArgumentException where the end date is before the start date
   */
  def holidays(startInclusive: LocalDate, endExclusive: LocalDate): Iterator[LocalDate] = {
    HolidayCalendar.checkInOrder(startInclusive, endExclusive)
    LocalDateUtils.dates(startInclusive, endExclusive).filter(date => isHoliday(date))
  }

  /**
   * Combines this calendar with another, observing the holidays of both.
   *
   * A day is a business day under the result only where it is a business day under both
   * calendars. Combining a calendar with itself, or with the calendar that has no holidays,
   * changes nothing.
   *
   * The result reads both calendars on every query, which is what suits a combination built for
   * one calculation. A combination meant to be held for the lifetime of an application is better
   * built by [[ImmutableHolidayCalendar.combined]], which merges the holiday data once.
   *
   * @param other  the other calendar
   * @return the combined calendar
   */
  def combinedWith(other: HolidayCalendar): HolidayCalendar =
    if (this == other || other == NoHolidays) this else HolidayCalendar.Combined(this, other)

  /**
   * Links this calendar to another, observing only the holidays both agree on.
   *
   * A day is a business day under the result where it is a business day under either calendar,
   * so linking anything to the calendar that has no holidays makes every day a business day.
   *
   * @param other  the other calendar
   * @return the linked calendar
   */
  def linkedWith(other: HolidayCalendar): HolidayCalendar =
    if (this == other) this
    else if (this == NoHolidays || other == NoHolidays) NoHolidays
    else HolidayCalendar.Linked(this, other)

  override def toString: String = s"HolidayCalendar[$name]"
}

/**
 * The composites of the calendar family, its date arithmetic, its type class instances and its
 * JSON form.
 *
 * ===Initialisation order (read before editing)===
 *
 * This object closes a loop that the design intends: the JSON form of a calendar names the
 * built-in calendars, the built-in calendars are assembled by [[StandardHolidayCalendars]] from
 * generated holiday data, and that data is turned into calendars by
 * [[ImmutableHolidayCalendar.of]] here. `HolidayCalendarId` and `HolidayCalendar` are likewise
 * mutually dependent, an identifier naming a calendar and a calendar carrying an identifier.
 *
 * The loop is safe only because every reference to [[StandardHolidayCalendars]] and to
 * `ReferenceData.standard` in this file sits inside the body of a method or of a `lazy val`,
 * never in a strict `val` that would be evaluated while this object is initialising. Anything
 * added here must keep that property.
 */
object HolidayCalendar {

  /** The JSON key of a calendar written out with its own holiday dates. */
  private val ImmutableKey: String = "Immutable"

  /** The JSON key of a calendar that observes the holidays of both of its parts. */
  private val CombinedKey: String = "Combined"

  /** The JSON key of a calendar that observes only the holidays both of its parts agree on. */
  private val LinkedKey: String = "Linked"

  /** The JSON field holding the identifier of a calendar. */
  private val IdField: String = "id"

  /** The JSON field holding the days of the week that are holidays. */
  private val WeekendDaysField: String = "weekendDays"

  /** The JSON field holding the first year the holiday data covers. */
  private val StartYearField: String = "startYear"

  /** The JSON field holding the holiday dates. */
  private val HolidaysField: String = "holidays"

  /** The JSON field holding the weekend dates that are business days regardless. */
  private val WorkingWeekendDaysField: String = "workingWeekendDays"

  /** The JSON field holding the first part of a composite calendar. */
  private val FirstField: String = "a"

  /** The JSON field holding the second part of a composite calendar. */
  private val SecondField: String = "b"

  /** The rejection of a document that is none of the shapes a calendar takes. */
  private val UnknownShapeMessage: String =
    "A holiday calendar must be the name of a calendar, or an object holding exactly one of " +
      s"'$ImmutableKey', '$CombinedKey' or '$LinkedKey'"

  //-------------------------------------------------------------------------
  /**
   * A calendar that observes the holidays of both of its parts.
   *
   * A day is a business day only where both parts agree that it is one. The parts are read on
   * every query rather than merged, which is what makes this the right composite for a
   * combination built for a single calculation; [[ImmutableHolidayCalendar.combined]] is the one
   * to hold for the lifetime of an application.
   *
   * Equality is structural, so two combinations of the same pair of calendars in the same order
   * are the same value.
   *
   * @param calendar1  the first calendar
   * @param calendar2  the second calendar
   */
  final case class Combined(calendar1: HolidayCalendar, calendar2: HolidayCalendar) extends HolidayCalendar {

    override def id: HolidayCalendarId = calendar1.id.combinedWith(calendar2.id)

    override def isHoliday(date: LocalDate): Boolean =
      calendar1.isHoliday(date) || calendar2.isHoliday(date)

    override def toString: String = s"HolidayCalendar[$name]"
  }

  /**
   * A calendar that observes only the holidays both of its parts agree on.
   *
   * A day is a business day where either part says it is one, so the holidays of the result are
   * the days on which every part is closed.
   *
   * Equality is structural, as for [[Combined]].
   *
   * @param calendar1  the first calendar
   * @param calendar2  the second calendar
   */
  final case class Linked(calendar1: HolidayCalendar, calendar2: HolidayCalendar) extends HolidayCalendar {

    override def id: HolidayCalendarId = calendar1.id.linkedWith(calendar2.id)

    override def isHoliday(date: LocalDate): Boolean =
      calendar1.isHoliday(date) && calendar2.isHoliday(date)

    override def toString: String = s"HolidayCalendar[$name]"
  }

  //-------------------------------------------------------------------------
  // The four calendars whose content follows from their name are declared beside this object
  // rather than inside it, because Scala requires every direct subtype of a sealed type to be
  // declared in the same file and the file reads better with them at the top level - the library
  // being ported also had one top-level type per implementation. These aliases make each of them
  // reachable through this companion as well, so `HolidayCalendar.SatSun` and `SatSun` name the
  // same object and a call site may spell either. They introduce no new entity.

  /** The calendar for which every day is a business day, also reachable as `NoHolidays`. */
  val NoHolidays: com.opengamma.strata.basics.date.NoHolidays.type =
    com.opengamma.strata.basics.date.NoHolidays

  /** The calendar whose holidays are Saturday and Sunday, also reachable as `SatSun`. */
  val SatSun: com.opengamma.strata.basics.date.SatSun.type =
    com.opengamma.strata.basics.date.SatSun

  /** The calendar whose holidays are Friday and Saturday, also reachable as `FriSat`. */
  val FriSat: com.opengamma.strata.basics.date.FriSat.type =
    com.opengamma.strata.basics.date.FriSat

  /** The calendar whose holidays are Thursday and Friday, also reachable as `ThuFri`. */
  val ThuFri: com.opengamma.strata.basics.date.ThuFri.type =
    com.opengamma.strata.basics.date.ThuFri

  //-------------------------------------------------------------------------
  /**
   * The ordering of dates used by the range checks of this file.
   *
   * `LocalDate` implements `Comparable` over `ChronoLocalDate` rather than over itself, so no
   * ordering of dates can be derived by the compiler and one has to be named. It is deliberately
   * not implicit: it is handed to the check that needs it, so that it cannot become an ambient
   * ordering for every date comparison in this package.
   */
  private val dateOrder: Order[LocalDate] = Order.from((first, second) => first.compareTo(second))

  /**
   * Finds the first business day at or after the date, stepping by one day at a time.
   *
   * Written as a tail-recursive function taking the date as a parameter, so that the search
   * needs no mutable state and compiles to a loop.
   *
   * @param calendar  the calendar to search
   * @param date  the date to start from
   * @param step  the number of days to step by, 1 to search forwards and -1 backwards
   * @return the first business day found
   */
  @tailrec
  private def businessDayFrom(calendar: HolidayCalendar, date: LocalDate, step: Int): LocalDate =
    if (calendar.isBusinessDay(date)) date
    else businessDayFrom(calendar, LocalDateUtils.plusDays(date, step), step)

  /**
   * Shifts the date by a number of business days, one business day at a time.
   *
   * @param calendar  the calendar to shift against
   * @param date  the date to shift
   * @param amount  the number of business days left to shift by
   * @return the shifted date
   */
  @tailrec
  private def shifted(calendar: HolidayCalendar, date: LocalDate, amount: Int): LocalDate =
    if (amount > 0) shifted(calendar, calendar.next(date), amount - 1)
    else if (amount < 0) shifted(calendar, calendar.previous(date), amount + 1)
    else date

  /**
   * Checks that a range is not inverted.
   *
   * Supplying the end of a range before its start is a mistake in the calling code rather than a
   * property of the data, so it fails fast, as the library being ported did, and with the same
   * message.
   *
   * Visible within this package rather than to this companion alone, because the calendar that
   * counts business days from its stored months makes the same check and is a separate class
   * from the trait this object accompanies.
   *
   * @param startInclusive  the start date
   * @param endExclusive  the end date
   * @throws IllegalArgumentException where the end date is before the start date
   */
  private[date] def checkInOrder(startInclusive: LocalDate, endExclusive: LocalDate): Unit =
    ArgCheck.inOrderOrEqual(startInclusive, endExclusive, "startInclusive", "endExclusive")(dateOrder)

  //-------------------------------------------------------------------------
  /**
   * The hashing and equality of holiday calendars.
   *
   * Taken from the `equals` and `hashCode` of the members, which is what keeps the two notions of
   * equality the family needs: a calendar carrying holiday data, and a weekend calendar, are
   * equal to another of their kind with the same identifier, because a calendar is identified by
   * what it claims to be rather than by the dates it happens to hold; a composite is equal
   * structurally, to the same pair of calendars read the same way. This is the family's only
   * equality-bearing instance, and `Eq[HolidayCalendar]` is obtained from it by subtyping.
   *
   * There is deliberately no ordering of calendars: the library being ported defines none, and
   * nothing about a calendar makes one larger than another.
   *
   * @return the hashing of holiday calendars
   */
  implicit val hash: Hash[HolidayCalendar] = Hash.fromUniversalHashCode[HolidayCalendar]

  /**
   * The rendering of holiday calendars as text.
   *
   * Renders what `toString` renders, which is the form of the library being ported -
   * `HolidayCalendar[GBLO]` - so the two ways of putting a calendar into a message agree.
   *
   * @return the rendering of a holiday calendar
   */
  implicit val show: Show[HolidayCalendar] = Show.show(calendar => calendar.toString)

  //-------------------------------------------------------------------------
  /** The decoder of a list of holiday dates, in the ISO-8601 form. */
  private val dateListDecoder: Decoder[List[LocalDate]] = Decoder.decodeList(Decoder.decodeLocalDate)

  /** The decoder of a list of days of the week, by constant name. */
  private val dayOfWeekListDecoder: Decoder[List[DayOfWeek]] = Decoder.decodeList(Codecs.dayOfWeekCodec)

  /**
   * The calendars of this file whose whole content follows from their name.
   *
   * These four are read back by name without consulting reference data, which is what lets a
   * document naming `Sat/Sun` be read by an application that supplies no calendars at all. They
   * are also entries of [[StandardHolidayCalendars]], so this is a short cut through the general
   * path rather than a second answer: `ReferenceData.standard` maps the same names to the same
   * values.
   *
   * Computed on first use, so that reading this file's JSON support cannot depend on the order in
   * which these objects and this one happen to be initialised.
   */
  private lazy val constantsByName: Map[String, HolidayCalendar] =
    List[HolidayCalendar](NoHolidays, SatSun, FriSat, ThuFri)
      .map(calendar => calendar.name -> calendar)
      .toMap

  /**
   * Checks whether a calendar is one of those built into this library.
   *
   * A built-in calendar is written as its name, because the name is enough to find the same
   * calendar again. The test is equality against the built-in set, so it answers for the value
   * rather than for the object: a calendar equal to the built-in `GBLO` is written as `GBLO`
   * however it was obtained, which is what makes the encoding a function of the value and
   * therefore stable.
   *
   * This is the one place in the JSON support that reads the built-in set, and it does so from
   * inside a method body, as the note on this object requires.
   *
   * @param calendar  the calendar to test
   * @return true where the calendar is one of the built-in set
   */
  private def isBuiltIn(calendar: HolidayCalendar): Boolean =
    StandardHolidayCalendars.all.exists { case (_, value) => value == calendar }

  /**
   * Writes the dates of a calendar as a JSON array in ascending order.
   *
   * The order is what makes the document stable: two calendars that are equal hold the same
   * dates, and sorted dates put them in the same place.
   *
   * @param dates  the dates to write, already in ascending order
   * @return the JSON array of dates
   */
  private def encodeDates(dates: Iterable[LocalDate]): Json =
    Json.fromValues(dates.toList.map(date => Encoder.encodeLocalDate(date)))

  /**
   * Writes a calendar that carries its own holiday data.
   *
   * The holiday dates and the weekend dates declared to be business days are the sets the
   * calendar reports, so the array of monthly bit masks it holds internally never appears in a
   * document. The first year covered is written for the reader; it is not read back, because the
   * range a calendar supports follows from its holidays and is worked out again when the
   * calendar is rebuilt.
   *
   * @param calendar  the calendar to write
   * @return the fields of the calendar
   */
  private def encodeImmutable(calendar: ImmutableHolidayCalendar): Json =
    Json.obj(
      IdField -> Json.fromString(calendar.id.name),
      WeekendDaysField -> Json.fromValues(
        calendar.weekendDays.toList.sortBy(day => day.getValue).map(day => Codecs.dayOfWeekCodec(day))),
      StartYearField -> Json.fromInt(calendar.startYear),
      HolidaysField -> encodeDates(calendar.holidays),
      WorkingWeekendDaysField -> encodeDates(calendar.workingDays))

  /**
   * Writes the two parts of a composite calendar.
   *
   * @param first  the first part
   * @param second  the second part
   * @return the fields naming both parts
   */
  private def encodePair(first: HolidayCalendar, second: HolidayCalendar): Json =
    Json.obj(FirstField -> encodeCalendar(first), SecondField -> encodeCalendar(second))

  /**
   * Writes a holiday calendar.
   *
   * A calendar that this library defines - the four constants of this file and every built-in
   * national calendar - is written as its name, because that name locates the same calendar
   * again through reference data. Any other calendar is written as an object of one field naming
   * its kind, so that a calendar an application built itself survives the round trip with its
   * holidays intact.
   *
   * The last case covers the constants of this file, whose content follows from their identifier.
   * The family is sealed and every member of it is declared here, so the kinds a document can
   * take are exactly the four written below.
   *
   * @param calendar  the calendar to write
   * @return the JSON document of the calendar
   */
  private def encodeCalendar(calendar: HolidayCalendar): Json = calendar match {
    case immutable: ImmutableHolidayCalendar =>
      if (isBuiltIn(immutable)) Json.fromString(immutable.name)
      else Json.obj(ImmutableKey -> encodeImmutable(immutable))
    case Combined(first, second) => Json.obj(CombinedKey -> encodePair(first, second))
    case Linked(first, second) => Json.obj(LinkedKey -> encodePair(first, second))
    case constant => Json.fromString(constant.name)
  }

  /**
   * Reads a calendar named by a document.
   *
   * The four constants of this file answer for themselves, and every other name is resolved
   * against the built-in reference data - which resolves a composite name such as `GBLO+USNY`
   * part by part, as resolving that identifier always does. A name that resolves to nothing is a
   * decoding failure carrying the reason the identifier could not be resolved.
   *
   * @param text  the name read from the document
   * @param cursor  the position in the document, for the failure message
   * @return the calendar of that name, or the failure explaining why there is none
   */
  private def decodeName(text: String, cursor: HCursor): Decoder.Result[HolidayCalendar] =
    constantsByName.get(text) match {
      case Some(calendar) => Right(calendar)
      case None =>
        HolidayCalendarId
          .of(text)
          .resolve(ReferenceData.standard)
          .left
          .map(failure => DecodingFailure(failure.message, cursor.history))
    }

  /**
   * Reads a calendar that carries its own holiday data.
   *
   * The fields are handed to the ordinary factory, so a document describes a calendar exactly as
   * a caller does: the holidays are sorted and deduplicated, the range follows from them, and the
   * weekend dates declared to be business days override both. The weekend overrides are optional,
   * because a calendar that has none writes an empty array and a hand-written document may omit
   * the field altogether.
   */
  private val immutableDecoder: Decoder[ImmutableHolidayCalendar] = Decoder.instance { cursor =>
    for {
      name <- cursor.get[String](IdField)
      weekendDays <- cursor.get[List[DayOfWeek]](WeekendDaysField)(dayOfWeekListDecoder)
      holidays <- cursor.get[List[LocalDate]](HolidaysField)(dateListDecoder)
      workingDays <- cursor.getOrElse[List[LocalDate]](WorkingWeekendDaysField)(List.empty)(dateListDecoder)
    } yield ImmutableHolidayCalendar.of(HolidayCalendarId.of(name), holidays, weekendDays, workingDays)
  }

  /**
   * Reads the two parts of a composite calendar.
   *
   * Each part is read by the codec of the family, so a part may itself be a name, a calendar
   * carrying its own data, or a further composite.
   *
   * @param cursor  the position of the object holding the two parts
   * @return the two parts, or the first failure encountered
   */
  private def decodePair(cursor: ACursor): Decoder.Result[(HolidayCalendar, HolidayCalendar)] =
    for {
      first <- cursor.get[HolidayCalendar](FirstField)(codec)
      second <- cursor.get[HolidayCalendar](SecondField)(codec)
    } yield (first, second)

  /**
   * Reads a holiday calendar.
   *
   * This is the inverse of the encoding above: a string is a name, and an object holding exactly
   * one field is a calendar of the kind that field names. Anything else is rejected, as is an
   * object holding no field or several.
   *
   * @param cursor  the position in the document
   * @return the calendar, or the failure explaining why the document describes none
   */
  private def decodeCalendar(cursor: HCursor): Decoder.Result[HolidayCalendar] =
    cursor.value.asString match {
      case Some(text) => decodeName(text, cursor)
      case None =>
        cursor.keys.map(keys => keys.toList) match {
          case Some(ImmutableKey :: Nil) =>
            cursor.downField(ImmutableKey).as[ImmutableHolidayCalendar](immutableDecoder)
          case Some(CombinedKey :: Nil) =>
            decodePair(cursor.downField(CombinedKey)).map { case (first, second) => Combined(first, second) }
          case Some(LinkedKey :: Nil) =>
            decodePair(cursor.downField(LinkedKey)).map { case (first, second) => Linked(first, second) }
          case _ => Left(DecodingFailure(UnknownShapeMessage, cursor.history))
        }
    }

  /**
   * The JSON form of holiday calendars.
   *
   * A calendar this library defines is written as its name and read back through reference data,
   * so a document holds `"GBLO"`, `"Sat/Sun"` or `"GBLO+USNY"`; a calendar an application built
   * itself is written structurally, with its holidays:
   *
   * {{{
   * "GBLO"
   * {"Immutable":{"id":"XCAL","weekendDays":["SATURDAY","SUNDAY"],"startYear":2020,
   *               "holidays":["2020-01-01"],"workingWeekendDays":[]}}
   * {"Combined":{"a":"GBLO","b":"USNY"}}
   * {"Linked":{"a":"GBLO","b":"USNY"}}
   * }}}
   *
   * The form is this port's own. The library being ported serialized a calendar as its internal
   * bit masks, and no compatibility with that is claimed or tested: the dates a calendar declares
   * are what a document carries, which is both readable and independent of how a calendar stores
   * them.
   *
   * Written out by hand rather than derived, because the family mixes a name-based form with a
   * structural one and because the type carrying holiday data has no public constructor to derive
   * from. Nothing here reads a class or a member by reflection.
   *
   * Equal calendars encode to identical bytes: the choice between the two forms is made by
   * equality against the built-in set, dates and days of the week are written in ascending order,
   * and the fields are written in a fixed order.
   *
   * @return the codec for holiday calendars
   */
  implicit val codec: Codec[HolidayCalendar] =
    Codec.from(Decoder.instance(decodeCalendar), Encoder.instance(encodeCalendar))
}


/**
 * The calendar for which every day is a business day.
 *
 * It is the identity of [[HolidayCalendar.combinedWith]] and the absorbing element of
 * [[HolidayCalendar.linkedWith]], and it is what a caller supplies to say that no holiday
 * calendar applies. Its date arithmetic is plain calendar arithmetic, which is why each method
 * is given directly here rather than searching day by day for a business day it would always
 * find at once - the same overrides the library being ported made, for the same reason.
 */
object NoHolidays extends HolidayCalendar {

  override val id: HolidayCalendarId = HolidayCalendarIds.NO_HOLIDAYS

  override def isHoliday(date: LocalDate): Boolean = false

  override def isBusinessDay(date: LocalDate): Boolean = true

  override def nextOrSame(date: LocalDate): LocalDate = date

  override def next(date: LocalDate): LocalDate = LocalDateUtils.plusDays(date, 1)

  override def previousOrSame(date: LocalDate): LocalDate = date

  override def previous(date: LocalDate): LocalDate = LocalDateUtils.plusDays(date, -1)

  override def nextSameOrLastInMonth(date: LocalDate): LocalDate = date

  override def shift(date: LocalDate, amount: Int): LocalDate = LocalDateUtils.plusDays(date, amount)

  /**
   * Counts the days in the specified range, every one of which is a business day.
   *
   * As in the library being ported, this is the plain difference of the two dates and does not
   * require them to be the right way round: a range given backwards yields a negative count
   * rather than failing.
   *
   * @param startInclusive  the start date, included in the count
   * @param endExclusive  the end date, excluded from the count
   * @return the number of days in the range
   */
  override def daysBetween(startInclusive: LocalDate, endExclusive: LocalDate): Int =
    Math.toIntExact(LocalDateUtils.daysBetween(startInclusive, endExclusive))

  override def combinedWith(other: HolidayCalendar): HolidayCalendar = other
}

/**
 * A calendar whose only holidays are its two weekend days.
 *
 * The three instances this library publishes are the only ones that exist, because a weekend
 * calendar carries no data beyond the pair of days its name already gives: `Sat/Sun`, `Fri/Sat`
 * and `Thu/Fri`. That is exactly the set the library being ported exposed, and it exposed no
 * constructor either; a centre with any other weekend is described by an
 * [[ImmutableHolidayCalendar]], which takes the weekend as data.
 *
 * Equality is identity, and, as in the library being ported, a weekend calendar is never equal to
 * a calendar carrying holiday data even where the two share an identifier.
 *
 * @param id  the identifier of this calendar
 * @param weekendDays  the two days of the week that are holidays
 */
sealed abstract class WeekendHolidayCalendar private[date] (
    override val id: HolidayCalendarId,
    val weekendDays: Set[DayOfWeek])
    extends HolidayCalendar {

  override def isHoliday(date: LocalDate): Boolean = weekendDays.contains(date.getDayOfWeek)
}

/** The calendar whose holidays are Saturday and Sunday. */
object SatSun
    extends WeekendHolidayCalendar(
      HolidayCalendarIds.SAT_SUN,
      Set(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY))

/** The calendar whose holidays are Friday and Saturday. */
object FriSat
    extends WeekendHolidayCalendar(
      HolidayCalendarIds.FRI_SAT,
      Set(DayOfWeek.FRIDAY, DayOfWeek.SATURDAY))

/** The calendar whose holidays are Thursday and Friday. */
object ThuFri
    extends WeekendHolidayCalendar(
      HolidayCalendarIds.THU_FRI,
      Set(DayOfWeek.THURSDAY, DayOfWeek.FRIDAY))


/**
 * A calendar built from a list of holiday dates and a weekend.
 *
 * This is the calendar an application supplies for a centre whose holidays it publishes itself,
 * and the one the built-in national calendars are expressed as. Construction normalises what it
 * is given, as the library being ported did: the holidays are sorted and deduplicated, the years
 * they span become the range the calendar knows about, and dates named as working days override
 * both the holidays and the weekend - they are applied last, and a working day outside the
 * holiday range is ignored because there is nothing there to override.
 *
 * Outside the range of years spanned by its holidays the calendar can only apply its weekend,
 * which is what the library being ported does rather than declaring those years free of
 * holidays. A date whose year lies outside 0 to 9999 is rejected, because no calendar could hold
 * data for it.
 *
 * Equality is the identifier alone, again as in the library being ported: two calendars claiming
 * to be `GBLO` are the same calendar as far as a schedule is concerned, and comparing their
 * holiday lists would make a calendar resolved from one set of reference data unequal to the same
 * calendar resolved from another. A calendar of this kind is never equal to a weekend calendar,
 * whatever the identifiers say.
 *
 * ===How the holidays are stored===
 *
 * A calendar is asked whether a date is a business day once per day of every schedule that
 * touches it, so the answer has to be cheap. The dates are therefore held as one `Int` per month
 * from January of the first year covered, with '''bit set meaning business day''' - the layout of
 * the library being ported, kept because it makes the whole family of questions a handful of
 * machine instructions: whether a date is a business day is one bit test, the next and previous
 * business days are `Integer.numberOfTrailingZeros` and `Integer.numberOfLeadingZeros`, and the
 * count of business days in a range is `Integer.bitCount` over the months it spans. Bits beyond
 * the length of a month are left unset, so they read as holidays and no month-length arithmetic
 * is needed.
 *
 * The array is allocated once, during construction, and is never handed out: [[holidays]],
 * [[weekendDays]] and [[workingDays]] each reconstruct a set from it, so the representation
 * cannot be reached, let alone written to, by a caller.
 *
 * Each method that reads the array tests the month index rather than catching the failure of an
 * out-of-bounds read, and a date the array does not cover falls back to the general
 * implementation of [[HolidayCalendar]], which applies the weekend alone. That is the same
 * two-case behaviour as the library being ported, arrived at by testing instead of by catching.
 */
final class ImmutableHolidayCalendar private (
    override val id: HolidayCalendarId,
    private val weekends: Int,
    val startYear: Int,
    private val lookup: Array[Int])
    extends HolidayCalendar {

  /**
   * The year after the last one the holiday data covers.
   *
   * Together with [[startYear]] this is the range of years within which the calendar answers
   * from its holiday data; outside it the weekend alone is applied. Where a calendar was built
   * with no holidays at all the range is empty and every query applies the weekend.
   *
   * @return the exclusive end of the range of years covered
   */
  def endYearExclusive: Int = startYear + lookup.length / 12

  /**
   * The days of the week that are holidays.
   *
   * @return the weekend days, which may be empty
   */
  def weekendDays: Set[DayOfWeek] =
    DayOfWeek.values().iterator.filter(day => isWeekendDay(day)).toSet

  /**
   * The holiday dates of this calendar, excluding the dates that are holidays only because they
   * fall at a weekend.
   *
   * The dates are recovered from the stored months, so this is the set the calendar was built
   * with, sorted and deduplicated. It is the set a document holds and the set a test compares,
   * and it is what makes the storage an implementation detail rather than part of the contract.
   *
   * @return the holiday dates, in ascending order
   */
  def holidays: SortedSet[LocalDate] = holidaysAndWorkingDays._1

  /**
   * The dates that are business days even though they fall at a weekend.
   *
   * These are the working-day overrides the calendar was built with, restricted - as
   * construction restricts them - to the range of years the calendar covers.
   *
   * @return the weekend dates that are business days, in ascending order
   */
  def workingDays: SortedSet[LocalDate] = holidaysAndWorkingDays._2

  override def isHoliday(date: LocalDate): Boolean = {
    val index = monthIndex(date)
    if (covers(index)) (lookup(index) & dayOfMonthBit(date)) == 0 else isHolidayOutsideRange(date)
  }

  override def shift(date: LocalDate, amount: Int): LocalDate =
    if (amount > 0) {
      // day-of-month: one-based here, which is the zero-based day-of-month of the following day,
      // so the search starts from the day after the one given
      shiftNext(date.getYear, date.getMonthValue, date.getDayOfMonth, amount)
        .getOrElse(shiftOutsideRange(date, amount))
    } else if (amount < 0) {
      // day-of-month: minus one, so the search starts from the day before the one given
      shiftPrevious(date.getYear, date.getMonthValue, date.getDayOfMonth - 1, amount)
        .getOrElse(shiftOutsideRange(date, amount))
    } else {
      date
    }

  override def next(date: LocalDate): LocalDate =
    shiftNext(date.getYear, date.getMonthValue, date.getDayOfMonth, 1).getOrElse(super.next(date))

  override def previous(date: LocalDate): LocalDate =
    shiftPrevious(date.getYear, date.getMonthValue, date.getDayOfMonth - 1, -1)
      .getOrElse(previousOutsideRange(date))

  override def nextSameOrLastInMonth(date: LocalDate): LocalDate = {
    val index = monthIndex(date)
    if (covers(index)) {
      val monthData = lookup(index)
      // shift the day given into bit 0, dropping the days before it
      val remaining = monthData >>> (date.getDayOfMonth - 1)
      val dayOfMonth =
        if (remaining == 0) {
          // no business day left this month, so the most significant bit is the last one
          32 - Integer.numberOfLeadingZeros(monthData)
        } else {
          // the least significant bit is the next business day, or the day itself
          date.getDayOfMonth + Integer.numberOfTrailingZeros(remaining)
        }
      date.withDayOfMonth(dayOfMonth)
    } else {
      super.nextSameOrLastInMonth(date)
    }
  }

  override def isLastBusinessDayOfMonth(date: LocalDate): Boolean = {
    val index = monthIndex(date)
    if (covers(index)) {
      // with the day given in bit 0, a remainder of exactly 1 means no later business day
      (lookup(index) >>> (date.getDayOfMonth - 1)) == 1
    } else {
      checkSupported(date)
      super.isLastBusinessDayOfMonth(date)
    }
  }

  override def lastBusinessDayOfMonth(date: LocalDate): LocalDate = {
    val index = monthIndex(date)
    if (covers(index)) {
      date.withDayOfMonth(32 - Integer.numberOfLeadingZeros(lookup(index)))
    } else {
      checkSupported(date)
      super.lastBusinessDayOfMonth(date)
    }
  }

  override def daysBetween(startInclusive: LocalDate, endExclusive: LocalDate): Int = {
    HolidayCalendar.checkInOrder(startInclusive, endExclusive)
    val startIndex = monthIndex(startInclusive)
    val endIndex = monthIndex(endExclusive)
    if (covers(startIndex) && covers(endIndex)) {
      // business days of the first month from the start date inclusive
      val fromStart = Integer.bitCount(lookup(startIndex) >>> (startInclusive.getDayOfMonth - 1))
      // business days of the last month from the end date inclusive, which are not in the range
      val beyondEnd = Integer.bitCount(lookup(endIndex) >>> (endExclusive.getDayOfMonth - 1))
      if (startIndex == endIndex) {
        fromStart - beyondEnd
      } else {
        val untilEnd = Integer.bitCount(lookup(endIndex)) - beyondEnd
        fromStart + untilEnd + businessDaysOfMonths(startIndex + 1, endIndex, 0)
      }
    } else {
      daysBetweenOutsideRange(startInclusive, endExclusive)
    }
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: ImmutableHolidayCalendar => id == that.id
    case _ => false
  }

  override def hashCode: Int = id.hashCode

  override def toString: String = s"HolidayCalendar[$name]"

  //-------------------------------------------------------------------------
  /**
   * Returns the index in the stored months of the month a date falls in.
   *
   * The arithmetic is that of the library being ported, including its behaviour for a date far
   * outside the range: the product can overflow, and an index that overflows is no more within
   * the bounds of the array than one that merely lies outside them, so such a date takes the
   * same path as any other uncovered date and is then rejected by its year.
   *
   * @param date  the date to locate
   * @return the index of the month, which may be outside the stored months
   */
  private def monthIndex(date: LocalDate): Int = monthIndexOf(date.getYear, date.getMonthValue)

  /**
   * Returns the index in the stored months of a year and month.
   *
   * @param year  the year
   * @param month  the month-of-year, from 1 to 12
   * @return the index of the month, which may be outside the stored months
   */
  private def monthIndexOf(year: Int, month: Int): Int = (year - startYear) * 12 + month - 1

  /**
   * Checks whether a month index is one of the stored months.
   *
   * @param index  the index to check
   * @return true where the index identifies a stored month
   */
  private def covers(index: Int): Boolean = index >= 0 && index < lookup.length

  /**
   * Returns the bit within a stored month that holds the day-of-month of a date.
   *
   * @param date  the date
   * @return the bit of that day-of-month
   */
  private def dayOfMonthBit(date: LocalDate): Int = 1 << (date.getDayOfMonth - 1)

  /**
   * Checks whether a day of the week is part of the weekend of this calendar.
   *
   * @param day  the day of the week
   * @return true where the day is a weekend day
   */
  private def isWeekendDay(day: DayOfWeek): Boolean =
    (weekends & ImmutableHolidayCalendar.weekendBit(day)) != 0

  /**
   * Checks that a date is one this calendar can be asked about at all.
   *
   * @param date  the date to check
   * @throws IllegalArgumentException where the year of the date is outside 0 to 9999
   */
  private def checkSupported(date: LocalDate): Unit =
    ArgCheck.isTrue(
      date.getYear >= 0 && date.getYear < 10000,
      s"Date is outside the accepted range (year 0000 to 10,000): $date")

  /**
   * Determines whether a date the holiday data does not cover is a holiday.
   *
   * Only the weekend can be applied, because no holidays are known for that year.
   *
   * @param date  the date to check
   * @return true where the date falls at the weekend
   * @throws IllegalArgumentException where the year of the date is outside 0 to 9999
   */
  private def isHolidayOutsideRange(date: LocalDate): Boolean = {
    checkSupported(date)
    isWeekendDay(date.getDayOfWeek)
  }

  /**
   * Shifts a date the holiday data does not cover, applying the weekend alone.
   *
   * @param date  the date to shift
   * @param amount  the number of business days to shift by
   * @return the shifted date
   * @throws IllegalArgumentException where the year of the date is outside 0 to 9999
   */
  private def shiftOutsideRange(date: LocalDate, amount: Int): LocalDate = {
    checkSupported(date)
    super.shift(date, amount)
  }

  /**
   * Finds the business day before a date the holiday data does not cover.
   *
   * @param date  the date to adjust
   * @return the previous business day
   * @throws IllegalArgumentException where the year of the date is outside 0 to 9999
   */
  private def previousOutsideRange(date: LocalDate): LocalDate = {
    checkSupported(date)
    super.previous(date)
  }

  /**
   * Counts the business days of a range the holiday data does not cover, applying the weekend
   * alone.
   *
   * @param startInclusive  the start date, included in the count
   * @param endExclusive  the end date, excluded from the count
   * @return the number of business days in the range
   * @throws IllegalArgumentException where the year of either date is outside 0 to 9999
   */
  private def daysBetweenOutsideRange(startInclusive: LocalDate, endExclusive: LocalDate): Int = {
    ArgCheck.isTrue(
      startInclusive.getYear >= 0 && startInclusive.getYear < 10000 &&
        endExclusive.getYear >= 0 && endExclusive.getYear < 10000,
      s"Dates are outside the accepted range (year 0000 to 10,000): $startInclusive, $endExclusive")
    super.daysBetween(startInclusive, endExclusive)
  }

  /**
   * Finds the business day a number of business days after a day-of-month.
   *
   * Each step moves to the least significant set bit at or after the day reached so far, and a
   * month with no business day left is left by recursing into the next one, carrying the steps
   * still to take. The recursion is tail recursion, so the search needs no mutable state.
   *
   * A month outside the stored months yields no date rather than failing, which is what lets the
   * caller fall back to the general implementation for a date this calendar holds no data for.
   *
   * @param year  the year reached
   * @param month  the month-of-year reached, from 1 to 12
   * @param dayOfMonth0  the zero-based day-of-month to search from
   * @param amount  the number of business days still to take, positive until the last step
   * @return the shifted date, or empty where the search left the stored months
   */
  @tailrec
  private def shiftNext(year: Int, month: Int, dayOfMonth0: Int, amount: Int): Option[LocalDate] = {
    val index = monthIndexOf(year, month)
    if (!covers(index)) {
      None
    } else if (amount <= 0) {
      Some(LocalDate.of(year, month, dayOfMonth0))
    } else {
      // shift the day reached into bit 0, dropping the days before it
      val remaining = lookup(index) >>> dayOfMonth0
      if (remaining == 0) {
        if (month == 12) shiftNext(year + 1, 1, 0, amount) else shiftNext(year, month + 1, 0, amount)
      } else {
        shiftNext(year, month, dayOfMonth0 + Integer.numberOfTrailingZeros(remaining) + 1, amount - 1)
      }
    }
  }

  /**
   * Finds the business day a number of business days before a day-of-month.
   *
   * The mirror of [[shiftNext]]: each step moves to the most significant set bit at or before the
   * day reached so far, and a month with no earlier business day is left by recursing into the
   * one before it.
   *
   * @param year  the year reached
   * @param month  the month-of-year reached, from 1 to 12
   * @param dayOfMonth  the one-based day-of-month to search from, which may be zero or negative
   * @param amount  the number of business days still to take, negative until the last step
   * @return the shifted date, or empty where the search left the stored months
   */
  @tailrec
  private def shiftPrevious(year: Int, month: Int, dayOfMonth: Int, amount: Int): Option[LocalDate] = {
    val index = monthIndexOf(year, month)
    if (!covers(index)) {
      None
    } else if (amount >= 0) {
      Some(LocalDate.of(year, month, dayOfMonth + 1))
    } else {
      // shift the day reached into bit 31, dropping the days after it
      val remaining = lookup(index) << (32 - dayOfMonth)
      if (remaining == 0 || dayOfMonth <= 0) {
        if (month == 1) shiftPrevious(year - 1, 12, 31, amount)
        else shiftPrevious(year, month - 1, 31, amount)
      } else {
        shiftPrevious(year, month, dayOfMonth - (Integer.numberOfLeadingZeros(remaining) + 1), amount + 1)
      }
    }
  }

  /**
   * Counts the business days of whole stored months.
   *
   * @param index  the index of the first month to count
   * @param endIndex  the index of the month to stop before
   * @param counted  the business days counted so far
   * @return the total number of business days in those months
   */
  @tailrec
  private def businessDaysOfMonths(index: Int, endIndex: Int, counted: Int): Int =
    if (index >= endIndex) counted
    else businessDaysOfMonths(index + 1, endIndex, counted + Integer.bitCount(lookup(index)))

  /**
   * Recovers the holiday dates and the weekend dates declared to be business days.
   *
   * Both sets come from one scan of the stored months, because each day of each month decides
   * which of them it belongs to, if either: a day that is not a business day and does not fall at
   * a weekend is a holiday, and a day that is a business day and does fall at a weekend is a
   * working-day override. A day that is neither is an ordinary business day or an ordinary
   * weekend, and is carried by the weekend alone.
   *
   * Where the calendar holds no months there is nothing to scan and both sets are empty.
   *
   * @return the holiday dates and the working weekend dates, both in ascending order
   */
  private def holidaysAndWorkingDays: (SortedSet[LocalDate], SortedSet[LocalDate]) = {
    val empty = SortedSet.empty[LocalDate](ImmutableHolidayCalendar.dateOrdering)
    if (lookup.isEmpty) {
      (empty, empty)
    } else {
      val (foundHolidays, foundWorkingDays) =
        lookup.indices.foldLeft((Vector.empty[LocalDate], Vector.empty[LocalDate])) {
          case ((holidaysSoFar, workingDaysSoFar), index) =>
            val (monthHolidays, monthWorkingDays) = monthDates(index)
            (holidaysSoFar ++ monthHolidays, workingDaysSoFar ++ monthWorkingDays)
        }
      (empty ++ foundHolidays, empty ++ foundWorkingDays)
    }
  }

  /**
   * Recovers the holiday dates and working weekend dates of one stored month.
   *
   * @param index  the index of the month to scan
   * @return the holiday dates and the working weekend dates of that month, in ascending order
   */
  private def monthDates(index: Int): (Vector[LocalDate], Vector[LocalDate]) = {
    val firstOfMonth = LocalDate.of(startYear, 1, 1).plusMonths(index.toLong)
    val monthData = lookup(index)
    val firstBitOfWeek = firstOfMonth.getDayOfWeek.getValue - 1
    (0 until firstOfMonth.lengthOfMonth)
      .foldLeft((Vector.empty[LocalDate], Vector.empty[LocalDate])) {
        case ((monthHolidays, monthWorkingDays), dayOffset) =>
          val businessDay = (monthData & (1 << dayOffset)) != 0
          val weekendDay = (weekends & (1 << ((firstBitOfWeek + dayOffset) % 7))) != 0
          if (businessDay == weekendDay) {
            val date = firstOfMonth.withDayOfMonth(dayOffset + 1)
            if (businessDay) (monthHolidays, monthWorkingDays :+ date)
            else (monthHolidays :+ date, monthWorkingDays)
          } else {
            (monthHolidays, monthWorkingDays)
          }
      }
  }
}


/**
 * Builds calendars from published holiday dates.
 *
 * Every factory here is total: a list of dates and a weekend always describe a calendar, so
 * there is nothing to reject and no error channel is needed. What the factories do instead is
 * normalise - sort and deduplicate the dates, derive the range of years they span, and apply the
 * working-day overrides last - which is why two calendars built from the same dates in different
 * orders are identical rather than merely equal.
 */
object ImmutableHolidayCalendar {

  /**
   * The repeating seven-bit pattern that clears one day of the week from a month.
   *
   * Bits 0, 7, 14, 21 and 28 are set, so shifting the pattern left by the offset between a
   * weekend day and the first of the month selects every occurrence of that day within the
   * month, and clearing those bits removes the whole of that weekday in one operation. This is
   * the binary literal the library being ported used, written in hexadecimal because Scala has
   * no binary literals.
   */
  private val WeekendPattern: Int = 0x10204081

  /**
   * The ordering of dates used to sort the holidays of a calendar.
   *
   * `LocalDate` implements `Comparable` over `ChronoLocalDate` rather than over itself, so no
   * ordering can be derived by the compiler and one has to be named. It is handed to each sorted
   * set explicitly rather than being implicit, so that it cannot become an ambient ordering for
   * every date comparison in this package.
   */
  private val dateOrdering: Ordering[LocalDate] =
    Ordering.fromLessThan((first, second) => first.isBefore(second))

  /**
   * The stored months of a calendar built with no holidays at all.
   *
   * A single shared value, which is safe because an empty array has nothing that could be
   * written to it and every calendar built this way applies its weekend alone.
   */
  private val emptyLookup: Array[Int] = Array.emptyIntArray

  /**
   * Obtains a calendar from a list of holidays and a two-day weekend.
   *
   * The two days may be the same, in which case the weekend is one day long.
   *
   * @param id  the identifier of the calendar
   * @param holidays  the holiday dates, in any order and with any duplicates
   * @param firstWeekendDay  the first day of the weekend
   * @param secondWeekendDay  the second day of the weekend, which may be the first again
   * @return the calendar
   */
  def of(
      id: HolidayCalendarId,
      holidays: Iterable[LocalDate],
      firstWeekendDay: DayOfWeek,
      secondWeekendDay: DayOfWeek): ImmutableHolidayCalendar =

    of(id, holidays, Set(firstWeekendDay, secondWeekendDay), Nil)

  /**
   * Obtains a calendar from a list of holidays and a weekend.
   *
   * The weekend may be empty, in which case the holidays have to name every non-business day
   * themselves - which is how a centre that works a six-day week is described.
   *
   * @param id  the identifier of the calendar
   * @param holidays  the holiday dates, in any order and with any duplicates
   * @param weekendDays  the days of the week that are holidays, which may be empty
   * @return the calendar
   */
  def of(
      id: HolidayCalendarId,
      holidays: Iterable[LocalDate],
      weekendDays: Iterable[DayOfWeek]): ImmutableHolidayCalendar =

    of(id, holidays, weekendDays, Nil)

  /**
   * Obtains a calendar from a list of holidays, a weekend, and the dates that are business days
   * in spite of them.
   *
   * The working days are applied last and override both the holidays and the weekend, so a date
   * named as both a holiday and a working day is a business day. A working day outside the range
   * of years the holidays span is ignored, because outside that range the calendar applies its
   * weekend and holds no data an override could apply to - the behaviour of the library being
   * ported.
   *
   * @param id  the identifier of the calendar
   * @param holidays  the holiday dates, in any order and with any duplicates
   * @param weekendDays  the days of the week that are holidays, which may be empty
   * @param workingDays  the dates that are business days whatever the holidays and weekend say
   * @return the calendar
   */
  def of(
      id: HolidayCalendarId,
      holidays: Iterable[LocalDate],
      weekendDays: Iterable[DayOfWeek],
      workingDays: Iterable[LocalDate]): ImmutableHolidayCalendar = {

    val sortedHolidays = SortedSet.empty[LocalDate](dateOrdering) ++ holidays
    val weekendSet = weekendDays.toSet
    val weekends = weekendSet.foldLeft(0)((mask, day) => mask | weekendBit(day))
    if (sortedHolidays.isEmpty) {
      new ImmutableHolidayCalendar(id, weekends, 0, emptyLookup)
    } else {
      val startYear = sortedHolidays.head.getYear
      val endYearExclusive = sortedHolidays.last.getYear + 1
      val lookup = buildLookup(sortedHolidays, weekendSet, startYear, endYearExclusive, workingDays)
      new ImmutableHolidayCalendar(id, weekends, startYear, lookup)
    }
  }

  /**
   * Obtains a calendar that merges the holidays of two calendars.
   *
   * Where [[HolidayCalendar.combinedWith]] reads both calendars on every query, this merges their
   * holiday data once and returns a calendar that answers as quickly as either of its parts. It
   * is the combination to build up front, for a value an application will hold and use
   * repeatedly; it is relatively slow to build, which is why it is not what `combinedWith` does.
   *
   * Merging the same calendar with itself yields that calendar. Two calendars whose ranges of
   * years overlap are merged by intersecting their stored months, which is why the result covers
   * the union of the two ranges - and why a year one of them holds no data for, but which lies
   * within the merged range, is a year of the merged calendar in which every day the other
   * calendar knows nothing about is a holiday. That is the behaviour of the library being ported,
   * reproduced here deliberately. Two calendars whose ranges do not overlap at all are merged
   * from their dates instead, because intersecting months that do not meet would lose them.
   *
   * @param calendar1  the first calendar
   * @param calendar2  the second calendar
   * @return the merged calendar, identified by the combination of the two identifiers
   */
  def combined(
      calendar1: ImmutableHolidayCalendar,
      calendar2: ImmutableHolidayCalendar): ImmutableHolidayCalendar =

    if (calendar1 eq calendar2) {
      calendar1
    } else {
      val combinedId = calendar1.id.combinedWith(calendar2.id)
      if (calendar1.endYearExclusive < calendar2.startYear ||
        calendar2.endYearExclusive < calendar1.startYear) {
        // the ranges do not meet, so the calendars are rebuilt from the dates they declare
        of(
          combinedId,
          calendar1.holidays ++ calendar2.holidays,
          calendar1.weekendDays ++ calendar2.weekendDays,
          calendar1.workingDays ++ calendar2.workingDays)
      } else {
        // the ranges meet, so the months are intersected, using the earlier start as the base
        val firstIsLower = calendar1.startYear <= calendar2.startYear
        val base = if (firstIsLower) calendar1 else calendar2
        val other = if (firstIsLower) calendar2 else calendar1
        val offset = (other.startYear - base.startYear) * 12
        val size = Math.max(base.lookup.length, other.lookup.length + offset)
        val merged = Array.tabulate(size) { index =>
          // a month the base calendar does not reach contributes no business day, exactly as
          // extending its array with zeroes and intersecting would
          val baseMonth = if (index < base.lookup.length) base.lookup(index) else 0
          val otherIndex = index - offset
          // intersected, because a set bit is a business day and a day has to be one in both
          if (otherIndex >= 0 && otherIndex < other.lookup.length) baseMonth & other.lookup(otherIndex)
          else baseMonth
        }
        // unioned, because a set bit here is a weekend day and either weekend closes the day
        val weekends = calendar1.weekends | calendar2.weekends
        new ImmutableHolidayCalendar(combinedId, weekends, base.startYear, merged)
      }
    }

  //-------------------------------------------------------------------------
  /**
   * Returns the bit that marks a day of the week as part of a weekend.
   *
   * @param day  the day of the week
   * @return the bit of that day
   */
  private def weekendBit(day: DayOfWeek): Int = 1 << (day.getValue - 1)

  /**
   * Builds the stored months of a calendar, one `Int` per month with a set bit for each business
   * day.
   *
   * Each month starts as every one of its days being a business day, which also leaves the bits
   * beyond the end of the month unset so that they read as holidays. The weekend is then cleared
   * a whole weekday at a time, the holidays are cleared, and the working days are set last so
   * that they override both. The array is produced by tabulating over the months, so the
   * construction holds no mutable state.
   *
   * @param holidays  the holiday dates, sorted and deduplicated
   * @param weekendDays  the days of the week that are holidays
   * @param startYear  the first year to cover, which is the year of the earliest holiday
   * @param endYearExclusive  the year after the last one to cover
   * @param workingDays  the dates that are business days whatever the holidays and weekend say
   * @return the stored months, one for each month of the range
   */
  private def buildLookup(
      holidays: Iterable[LocalDate],
      weekendDays: Set[DayOfWeek],
      startYear: Int,
      endYearExclusive: Int,
      workingDays: Iterable[LocalDate]): Array[Int] = {

    val firstOfRange = LocalDate.of(startYear, 1, 1)
    val holidayMasks = masksByMonth(holidays, startYear, endYearExclusive)
    val workingMasks = masksByMonth(workingDays, startYear, endYearExclusive)
    Array.tabulate((endYearExclusive - startYear) * 12) { index =>
      val firstOfMonth = firstOfRange.plusMonths(index.toLong)
      val firstDayOfWeek = firstOfMonth.getDayOfWeek.getValue
      // one set bit per day of the month, and no set bit beyond its end
      val everyDay = (1 << firstOfMonth.lengthOfMonth) - 1
      val withoutWeekends = weekendDays.foldLeft(everyDay) { (mask, weekendDay) =>
        val daysDifference = weekendDay.getValue - firstDayOfWeek
        val offset = if (daysDifference < 0) daysDifference + 7 else daysDifference
        mask & ~(WeekendPattern << offset)
      }
      (withoutWeekends & ~holidayMasks.getOrElse(index, 0)) | workingMasks.getOrElse(index, 0)
    }
  }

  /**
   * Collects a set of dates into one bit mask per month of the range.
   *
   * Dates outside the range are dropped, which is what ignores a working day the calendar holds
   * no data for. The holidays define the range and so are never dropped by this.
   *
   * @param dates  the dates to collect
   * @param startYear  the first year of the range
   * @param endYearExclusive  the year after the last one of the range
   * @return the bit mask of each month that holds at least one of the dates
   */
  private def masksByMonth(
      dates: Iterable[LocalDate],
      startYear: Int,
      endYearExclusive: Int): Map[Int, Int] =

    dates.foldLeft(Map.empty[Int, Int]) { (masks, date) =>
      if (date.getYear < startYear || date.getYear >= endYearExclusive) {
        masks
      } else {
        val index = (date.getYear - startYear) * 12 + date.getMonthValue - 1
        masks.updated(index, masks.getOrElse(index, 0) | (1 << (date.getDayOfMonth - 1)))
      }
    }

  //-------------------------------------------------------------------------
  /**
   * The JSON form of a calendar built from holiday dates.
   *
   * This is the codec of the whole family narrowed to this one member, so a calendar written
   * through it is written exactly as it would be as a [[HolidayCalendar]] - as its name where it
   * is one of the built-in calendars, and structurally otherwise - and a document that describes
   * a calendar of another kind, such as the bare name `Sat/Sun`, is rejected rather than widened.
   *
   * Computed on first use, because it reads the codec of the family and the two objects must not
   * depend on the order in which they are initialised.
   *
   * @return the codec for calendars built from holiday dates
   */
  implicit lazy val codec: Codec[ImmutableHolidayCalendar] = {
    val encoder: Encoder[ImmutableHolidayCalendar] =
      Encoder.instance(calendar => HolidayCalendar.codec(calendar))
    val decoder: Decoder[ImmutableHolidayCalendar] = Decoder.instance { cursor =>
      HolidayCalendar.codec(cursor).flatMap {
        case immutable: ImmutableHolidayCalendar => Right(immutable)
        case other =>
          Left(DecodingFailure(s"Expected a calendar built from holiday dates, but found: $other", cursor.history))
      }
    }
    Codec.from(decoder, encoder)
  }
}

/**
 * The holiday calendars that carry no market convention of their own, and the ways of reaching a
 * calendar by name.
 *
 * The four constants here are the calendars whose contents follow from their names, so that no
 * application has anything to disagree with: every day a business day, or a weekend and nothing
 * else. The calendars of actual centres are reference data and are reached through
 * [[com.opengamma.strata.basics.ReferenceData]] instead, which is what [[of]] does on a caller's
 * behalf for the calendars built into this library.
 */
object HolidayCalendars {

  /** The separator that combines two calendar names. */
  private val CombineSeparator: String = "+"

  /** The two days that a calendar defaulted by [[defaultingReferenceData]] treats as its weekend. */
  private val DefaultWeekendDays: Set[DayOfWeek] = Set(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY)

  /** The calendar for which every day is a business day. */
  val NO_HOLIDAYS: HolidayCalendar = NoHolidays

  /** The calendar whose only holidays are Saturday and Sunday. */
  val SAT_SUN: HolidayCalendar = SatSun

  /** The calendar whose only holidays are Friday and Saturday. */
  val FRI_SAT: HolidayCalendar = FriSat

  /** The calendar whose only holidays are Thursday and Friday. */
  val THU_FRI: HolidayCalendar = ThuFri

  //-------------------------------------------------------------------------
  /**
   * Obtains a calendar built into this library by name.
   *
   * Two or more names may be joined with `'+'`, so that `GBLO+USNY` is the calendar that observes
   * the holidays of London and New York; each part is looked up in turn and the parts are
   * combined. A name this library does not know is reported as a failure rather than raised,
   * which is the port's treatment of every lookup that depends on its argument.
   *
   * An application should generally not call this. A calendar is reference data, so code that
   * adjusts dates should name it with a [[HolidayCalendarId]] and resolve that identifier against
   * the [[com.opengamma.strata.basics.ReferenceData]] it was given, which lets the application
   * supply its own holidays. This method is the short cut for a demonstration, a test, or a
   * program that has decided to use the built-in data.
   *
   * @param uniqueName  the name of the calendar, optionally several joined with `'+'`
   * @return the calendar of that name, or the failure naming the part that is unknown
   */
  def of(uniqueName: String): Either[Failure, HolidayCalendar] =
    if (uniqueName.contains(CombineSeparator)) {
      splitOnCombine(uniqueName).foldLeft(Right(NO_HOLIDAYS): Either[Failure, HolidayCalendar]) {
        (combined, part) =>
          for {
            calendar <- combined
            next <- of(part)
          } yield calendar.combinedWith(next)
      }
    } else {
      lookup(uniqueName).toRight(
        Failure
          .Parsing(s"HolidayCalendar name not found: $uniqueName")
          .withAttribute("name", uniqueName))
    }

  /**
   * Decorates reference data so that every holiday calendar identifier resolves to a calendar.
   *
   * An identifier the underlying data does not hold resolves to a calendar of that identifier
   * whose only holidays are Saturday and Sunday, so a calculation can proceed against data that
   * is incomplete - which is what makes this useful for exploratory and test code, and what makes
   * it unsuitable for production, where a missing calendar is a fault worth reporting.
   *
   * The defaulted calendar carries the identifier that was '''asked for''', so a request for
   * `GBXX` yields a calendar named `GBXX` rather than the shared weekend calendar, and that
   * identity survives into its equality, its combined names and its JSON form. A composite
   * identifier is deliberately left unresolved, so that resolving it falls through to its parts
   * and each part is defaulted in turn.
   *
   * @param underlying  the reference data to decorate
   * @return reference data that answers for every holiday calendar identifier
   */
  def defaultingReferenceData(underlying: ReferenceData): ReferenceData =
    new DefaultingReferenceData(underlying)

  //-------------------------------------------------------------------------
  /**
   * Finds a calendar built into this library by name.
   *
   * The built-in set is read here rather than held in a field of this object, because the
   * calendars are assembled from generated data that is itself built from calendars; the note on
   * [[HolidayCalendar]] explains why that loop is only safe from inside a method body.
   *
   * @param uniqueName  the name of the calendar
   * @return the calendar of that name, or empty where this library does not define one
   */
  private def lookup(uniqueName: String): Option[HolidayCalendar] =
    StandardHolidayCalendars.all.collectFirst {
      case (id: HolidayCalendarId, calendar: HolidayCalendar) if id.name == uniqueName => calendar
    }

  /**
   * Splits a combined name into its parts.
   *
   * Empty parts are kept, so a name such as `GBLO+` names an unknown calendar and is reported as
   * such rather than silently being the London calendar - the behaviour of the library being
   * ported. Written as a tail-recursive function, which also avoids treating the separator as a
   * regular expression.
   *
   * @param uniqueName  the combined name
   * @return the parts of the name, in the order they were written
   */
  private def splitOnCombine(uniqueName: String): List[String] = {
    @tailrec
    def loop(from: Int, parts: List[String]): List[String] = {
      val index = uniqueName.indexOf(CombineSeparator, from)
      if (index < 0) (uniqueName.substring(from) :: parts).reverse
      else loop(index + CombineSeparator.length, uniqueName.substring(from, index) :: parts)
    }

    loop(0, Nil)
  }

  /**
   * Reference data that answers for every holiday calendar identifier.
   *
   * The implementation of [[defaultingReferenceData]], kept private because the decoration is
   * what a caller wants rather than the type of it.
   *
   * @param underlying  the reference data being decorated
   */
  private final class DefaultingReferenceData(underlying: ReferenceData) extends ReferenceData {

    override def findValue[T](id: ReferenceDataId[T]): Option[T] =
      underlying.findValue(id).orElse(defaultValue(id))

    override def containsValue(id: ReferenceDataId[_]): Boolean =
      underlying.containsValue(id) || id.isInstanceOf[HolidayCalendarId]

    override def combinedWith(other: ReferenceData): ReferenceData =
      new DefaultingReferenceData(underlying.combinedWith(other))

    /**
     * Returns the calendar supplied for an identifier the underlying data does not hold.
     *
     * The cast is safe and is confined to this method: the value is produced only for a
     * [[HolidayCalendarId]], whose type parameter is [[HolidayCalendar]], so the value produced
     * is of the type the identifier asks for. A composite identifier yields nothing, which is
     * what sends its resolution to its parts.
     *
     * @tparam T  the type of data the identifier refers to
     * @param id  the identifier that was not found
     * @return the defaulted calendar, or empty where the identifier names something else
     */
    private def defaultValue[T](id: ReferenceDataId[T]): Option[T] = id match {
      case calendarId: HolidayCalendarId if !calendarId.isComposite =>
        Some(ImmutableHolidayCalendar.of(calendarId, Nil, DefaultWeekendDays).asInstanceOf[T])
      case _ => None
    }
  }
}

