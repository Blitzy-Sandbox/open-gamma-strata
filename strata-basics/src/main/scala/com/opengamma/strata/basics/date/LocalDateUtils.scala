/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.{LocalDate, Year}

/**
 * Internal date arithmetic helpers for working with `LocalDate`.
 *
 * This is an implementation detail of this module rather than part of its public API, so it is
 * visible only inside `com.opengamma.strata.basics`, where the `date`, `schedule` and `index`
 * packages build on it.
 *
 * Every member is a total, referentially transparent function of its arguments: there is no
 * state, no lookup of ambient data and no failure mode, so no error channel is needed. The
 * helpers compute their answers directly rather than delegating to the JDK equivalents, because
 * they sit on the hot path of day-count year fractions and holiday-calendar scans, where they
 * are evaluated once per day of a schedule. Each fast path is exact rather than approximate - a
 * cheaper route to precisely the answer the JDK would give - which matters because [[doy]] and
 * [[daysBetween]] feed the day-count year fractions.
 */
private[basics] object LocalDateUtils {

  /**
   * Day-of-year of the last day of the month preceding each month, in a standard year.
   *
   * The table has length 13 with element zero ignored, so a month value of 1 to 12 indexes it
   * directly. `Array[Int]` keeps each lookup a single unboxed load; the table is private, is
   * never handed out and is never written to after construction, so the mutability of the
   * underlying primitive array is not observable.
   */
  private val standardMonthOffsets: Array[Int] =
    Array(0, 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)

  /**
   * Day-of-year of the last day of the month preceding each month, in a leap year.
   *
   * Indexed exactly as [[standardMonthOffsets]], and subject to the same restrictions.
   */
  private val leapMonthOffsets: Array[Int] =
    Array(0, 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)

  /**
   * Finds the day-of-year of a date.
   *
   * This is faster than the JDK method, and agrees with it for every date: the day-of-year is
   * the day-of-month offset by the length of the preceding months, which the two tables hold
   * for the standard and leap cases.
   *
   * @param date  the date to query
   * @return the day-of-year, from 1 to 365, or 366 in a leap year
   */
  def doy(date: LocalDate): Int = {
    val lookup = if (date.isLeapYear) leapMonthOffsets else standardMonthOffsets
    lookup(date.getMonthValue) + date.getDayOfMonth
  }

  /**
   * Adds a number of days to a date.
   *
   * This is faster than the JDK method, and agrees with it for every date and offset. Adding
   * zero returns the input date itself. Otherwise the offset is applied to the day-of-month, and
   * when the result is small enough to be guaranteed to land in the same month or the one after
   * it, the date is built directly instead of being routed through the epoch day: the 59th of
   * January is the 28th of February and the 59th of February is the 31st of March, so 59 is the
   * largest day-of-month that cannot overshoot the following month. Every other offset falls
   * back to exact epoch-day arithmetic, which overflows only for a result outside the supported
   * range of `LocalDate`, exactly as the JDK method does.
   *
   * @param date  the date to add to
   * @param daysToAdd  the number of days to add, which may be negative
   * @return the date the given number of days after the input date
   */
  def plusDays(date: LocalDate, daysToAdd: Int): LocalDate =
    if (daysToAdd == 0) {
      date
    } else {
      // Widened before the addition rather than after it, as the conversion is explicit here;
      // the fast path is unaffected, because a sum too large for an Int is also far above 59.
      val dom = date.getDayOfMonth.toLong + daysToAdd
      if (dom > 0 && dom <= 59) {
        val monthLen = date.lengthOfMonth
        val month = date.getMonthValue
        val year = date.getYear
        if (dom <= monthLen) {
          LocalDate.of(year, month, dom.toInt)
        } else if (month < 12) {
          LocalDate.of(year, month + 1, (dom - monthLen).toInt)
        } else {
          LocalDate.of(year + 1, 1, (dom - monthLen).toInt)
        }
      } else {
        LocalDate.ofEpochDay(Math.addExact(date.toEpochDay, daysToAdd.toLong))
      }
    }

  /**
   * Returns the number of days between two dates.
   *
   * This is faster than the JDK method, and agrees with it for every pair of dates. Two dates in
   * the same year are compared by day-of-year, and two dates in consecutive years by the days
   * remaining in the first year plus the day-of-year of the second; any wider gap falls back to
   * the difference of the epoch days. The result is exact for any ordering of the arguments, and
   * is negative when the second date precedes the first.
   *
   * The result is a `Long` because it is the difference of two epoch days; callers that need an
   * `Int` narrow it themselves, so that an out-of-range difference cannot be silently truncated.
   *
   * @param firstDate  the first date
   * @param secondDate  the second date, normally on or after the first
   * @return the number of days from the first date to the second
   */
  def daysBetween(firstDate: LocalDate, secondDate: LocalDate): Long = {
    val firstYear = firstDate.getYear
    val secondYear = secondDate.getYear
    if (firstYear == secondYear) {
      (doy(secondDate) - doy(firstDate)).toLong
    } else if (firstYear + 1 == secondYear) {
      ((firstDate.lengthOfYear - doy(firstDate)) + doy(secondDate)).toLong
    } else {
      secondDate.toEpochDay - firstDate.toEpochDay
    }
  }

  /**
   * Returns the dates in a range, from the start date inclusive to the end date exclusive.
   *
   * The dates are produced in ascending order, one day apart, and the iterator is lazy: no date
   * beyond the one being consumed is computed, and no collection is built, so a caller filters
   * and counts the range with the ordinary collection operations. An end date on or before the
   * start date yields no dates at all.
   *
   * The returned iterator is single-use, as every iterator is; call this method again for a
   * second traversal of the same range.
   *
   * @param startInclusive  the first date of the range
   * @param endExclusive  the date immediately after the last date of the range
   * @return an iterator over each date in the range
   */
  def dates(startInclusive: LocalDate, endExclusive: LocalDate): Iterator[LocalDate] =
    Iterator.iterate(startInclusive)(plusDays(_, 1)).takeWhile(_.isBefore(endExclusive))

  /**
   * Checks whether a year is a leap year, according to the ISO proleptic calendar system.
   *
   * @param year  the year to check, which may be negative
   * @return true if the year has 366 days
   */
  def isLeapYear(year: Int): Boolean = Year.isLeap(year.toLong)

}
