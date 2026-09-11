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

import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.Named

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
   */
  def isHoliday(date: LocalDate): Boolean

  /**
   * Checks whether the specified date is a business day.
   *
   * @param date  the date to check
   * @return true where the date is a business day
   */
  def isBusinessDay(date: LocalDate): Boolean = !isHoliday(date)

  /**
   * Finds the next business day, returning the date itself where it is one already.
   *
   * @param date  the date to adjust
   * @return the date itself where it is a business day, otherwise the next business day
   */
  def nextOrSame(date: LocalDate): LocalDate = HolidayCalendar.businessDayFrom(this, date, 1)

  /**
   * Finds the next business day, always returning a later date.
   *
   * @param date  the date to adjust
   * @return the first business day after the date
   */
  def next(date: LocalDate): LocalDate = nextOrSame(LocalDateUtils.plusDays(date, 1))

  /**
   * Finds the previous business day, returning the date itself where it is one already.
   *
   * @param date  the date to adjust
   * @return the date itself where it is a business day, otherwise the previous business day
   */
  def previousOrSame(date: LocalDate): LocalDate = HolidayCalendar.businessDayFrom(this, date, -1)

  /**
   * Finds the previous business day, always returning an earlier date.
   *
   * @param date  the date to adjust
   * @return the first business day before the date
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
   */
  def isLastBusinessDayOfMonth(date: LocalDate): Boolean =
    isBusinessDay(date) && next(date).getMonthValue != date.getMonthValue

  /**
   * Calculates the last business day of the month the specified date falls in.
   *
   * @param date  the date to examine
   * @return the last business day of that month
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
   */
  def shift(date: LocalDate, amount: Int): LocalDate = HolidayCalendar.shifted(this, date, amount)

  /**
   * Counts the business days in the specified range.
   *
   * @param startInclusive  the start date, included in the count
   * @param endExclusive  the end date, excluded from the count
   * @return the number of business days in the range, zero where the dates are equal
   * @throws IllegalArgumentException where the end date is before the start date
   */
  def daysBetween(startInclusive: LocalDate, endExclusive: LocalDate): Int =
    businessDays(startInclusive, endExclusive).size

  /**
   * Returns the business days in the specified range.
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
 * The composites of the calendar family, and the shared date arithmetic behind its methods.
 */
object HolidayCalendar {

  /**
   * A calendar that observes the holidays of both of its parts.
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
   * @param calendar1  the first calendar
   * @param calendar2  the second calendar
   */
  final case class Linked(calendar1: HolidayCalendar, calendar2: HolidayCalendar) extends HolidayCalendar {

    override def id: HolidayCalendarId = calendar1.id.linkedWith(calendar2.id)

    override def isHoliday(date: LocalDate): Boolean =
      calendar1.isHoliday(date) && calendar2.isHoliday(date)

    override def toString: String = s"HolidayCalendar[$name]"
  }

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
   * property of the data, so it fails fast, as the original did.
   *
   * @param startInclusive  the start date
   * @param endExclusive  the end date
   * @throws IllegalArgumentException where the end date is before the start date
   */
  private def checkInOrder(startInclusive: LocalDate, endExclusive: LocalDate): Unit =
    ArgCheck.isTrue(
      !endExclusive.isBefore(startInclusive),
      s"Invalid dates, startInclusive must be on or before endExclusive, but was $startInclusive and $endExclusive")
}

/**
 * The calendar for which every day is a business day.
 *
 * It is the identity of [[HolidayCalendar.combinedWith]] and the absorbing element of
 * [[HolidayCalendar.linkedWith]], and its date arithmetic is plain calendar arithmetic, which is
 * why each method is given directly here rather than searching day by day for a business day it
 * would always find at once.
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

  override def daysBetween(startInclusive: LocalDate, endExclusive: LocalDate): Int =
    Math.toIntExact(LocalDateUtils.daysBetween(startInclusive, endExclusive))

  override def combinedWith(other: HolidayCalendar): HolidayCalendar = other
}

/**
 * A calendar whose only holidays are its two weekend days.
 *
 * The three instances this library publishes are the only ones that exist, because a weekend
 * calendar carries no data beyond the pair of days its name already gives: `Sat/Sun`, `Fri/Sat`
 * and `Thu/Fri`. Equality is therefore identity, and the identifier is the name.
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
 * is given, as the original did: the holidays are sorted and deduplicated, the years they span
 * become the range the calendar knows about, and dates named as working days override both the
 * holidays and the weekend - they are applied last, and a working day outside the holiday range
 * is ignored because there is nothing there to override.
 *
 * Outside the year range spanned by its holidays the calendar can only apply its weekend, which
 * is what the original does rather than declaring those years free of holidays; a date whose year
 * is outside the range of years `java.time` accepts as data is rejected. Equality is the
 * identifier alone, again as in the original: two calendars claiming to be `GBLO` are the same
 * calendar as far as a schedule is concerned, and comparing their holiday lists would make a
 * calendar resolved from one set of reference data unequal to the same calendar resolved from
 * another.
 *
 * @param id  the identifier of this calendar
 * @param holidays  the holiday dates, sorted and deduplicated
 * @param weekendDays  the days of the week that are holidays
 * @param workingDays  the dates that are business days whatever the holidays and weekend say
 */
sealed abstract case class ImmutableHolidayCalendar private (
    override val id: HolidayCalendarId,
    holidays: SortedSet[LocalDate],
    weekendDays: Set[DayOfWeek],
    workingDays: Set[LocalDate])
    extends HolidayCalendar {

  /** The first year the holiday data covers, or zero where there are no holidays. */
  private val startYear: Int = holidays.headOption.map(date => date.getYear).getOrElse(0)

  /** The last year the holiday data covers, or minus one where there are no holidays. */
  private val endYear: Int = holidays.lastOption.map(date => date.getYear).getOrElse(-1)

  override def isHoliday(date: LocalDate): Boolean =
    if (date.getYear >= startYear && date.getYear <= endYear) {
      !workingDays.contains(date) &&
      (holidays.contains(date) || weekendDays.contains(date.getDayOfWeek))
    } else {
      isHolidayOutsideDataRange(date)
    }

  /**
   * Determines whether a date the holiday data does not cover is a holiday.
   *
   * Only the weekend can be applied, since no holidays are known for that year.
   *
   * @param date  the date to check
   * @return true where the date falls on the weekend
   * @throws IllegalArgumentException where the year of the date is outside 0 to 9999
   */
  private def isHolidayOutsideDataRange(date: LocalDate): Boolean = {
    ArgCheck.isTrue(
      date.getYear >= 0 && date.getYear < 10000,
      s"Date is outside the accepted range (year 0000 to 10,000): $date")
    weekendDays.contains(date.getDayOfWeek)
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: ImmutableHolidayCalendar => id == that.id
    case _ => false
  }

  override def hashCode: Int = id.hashCode

  override def toString: String = s"HolidayCalendar[$name]"
}

/**
 * Builds calendars from published holiday dates.
 */
object ImmutableHolidayCalendar {

  /** Orders dates by the day they fall on, so that the holidays of a calendar are sorted. */
  private implicit val dateOrdering: Ordering[LocalDate] = Ordering.by(date => date.toEpochDay)

  /**
   * Obtains a calendar from a list of holidays and a weekend.
   *
   * @param id  the identifier of the calendar
   * @param holidays  the holiday dates, in any order and with any duplicates
   * @param weekendDays  the days of the week that are holidays
   * @return the calendar
   */
  def of(
      id: HolidayCalendarId,
      holidays: Iterable[LocalDate],
      weekendDays: Set[DayOfWeek]): ImmutableHolidayCalendar =

    of(id, holidays, weekendDays, Set.empty[LocalDate])

  /**
   * Obtains a calendar from a list of holidays, a weekend, and the dates that are business days
   * in spite of them.
   *
   * @param id  the identifier of the calendar
   * @param holidays  the holiday dates, in any order and with any duplicates
   * @param weekendDays  the days of the week that are holidays
   * @param workingDays  the dates that are business days whatever the holidays and weekend say
   * @return the calendar
   */
  def of(
      id: HolidayCalendarId,
      holidays: Iterable[LocalDate],
      weekendDays: Set[DayOfWeek],
      workingDays: Iterable[LocalDate]): ImmutableHolidayCalendar =

    new ImmutableHolidayCalendar(
      id,
      SortedSet.empty[LocalDate] ++ holidays,
      weekendDays,
      workingDays.toSet) {}
}

/**
 * The holiday calendars that carry no market convention of their own.
 *
 * These four are the calendars whose contents follow from their names, so that no application has
 * anything to disagree with: every day a business day, or a weekend and nothing else. The
 * calendars of actual centres are reference data and are reached through
 * [[com.opengamma.strata.basics.ReferenceData]] instead.
 */
object HolidayCalendars {

  /** The calendar for which every day is a business day. */
  val NO_HOLIDAYS: HolidayCalendar = NoHolidays

  /** The calendar whose only holidays are Saturday and Sunday. */
  val SAT_SUN: HolidayCalendar = SatSun

  /** The calendar whose only holidays are Friday and Saturday. */
  val FRI_SAT: HolidayCalendar = FriSat

  /** The calendar whose only holidays are Thursday and Friday. */
  val THU_FRI: HolidayCalendar = ThuFri
}
