/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.DayOfWeek
import java.time.LocalDate
import java.time.MonthDay

import scala.collection.immutable.List
import scala.collection.immutable.Set
import scala.collection.immutable.SortedMap

/**
 * The Thai bank holidays, held as explicit dates rather than derived from rules.
 *
 * This object is the whole of the Thai holiday data of this library: the single built-in
 * calendar whose holidays are published date by date, section `THBA`. Every row is
 * recorded here exactly as published:
 *
 *  - 75 year rows, one for every year from 2005 to 2079 inclusive;
 *  - 1220 holiday dates in total, each recorded as the month-day pair given for its year;
 *  - the weekend is Saturday and Sunday;
 *  - no working-day override is declared, so weekends are never reinstated as business days.
 *
 * Two aspects of the published data are deliberately preserved rather than tidied up.
 * First, the rows are '''not''' filtered against the weekend: a published holiday that
 * happens to fall on a Saturday or Sunday stays in the table, and exactly one does
 * (2031-05-04, a Sunday). Second, the dates within each row keep their published order,
 * which is ascending, so [[thbaHolidays]] is ascending overall.
 *
 * This object is intentionally data only. It never builds a [[HolidayCalendar]], because
 * the built-in calendar values are assembled by `StandardHolidayCalendars`, which reads
 * this table - constructing a calendar here would create an initialisation cycle between
 * the two. Consumers combine [[thbaHolidays]] with [[thbaWeekendDays]] to build the
 * calendar value itself.
 *
 * All members are immutable and safe to share between threads.
 */
object HolidayCalendarData {

  /**
   * Creates a month-day pair.
   *
   * The rows below are written through this helper so that each year stays on a line or
   * two of dates and remains readable.
   *
   * @param month  the month-of-year, from 1 (January) to 12 (December)
   * @param day  the day-of-month, valid for the month of the year it is recorded against
   * @return the month-day
   */
  private def md(month: Int, day: Int): MonthDay = MonthDay.of(month, day)

  // The table is split into three methods at 25-year boundaries. The split has to be into
  // methods rather than values to have any effect on the generated code at all: the
  // initialiser of every `val` in an object is emitted into that object's single
  // constructor method, so chunking into values would not divide the bytecode. Each method
  // below is compiled on its own and stays well inside the per-method size limit of the
  // virtual machine, and the boundaries fall on whole years so that each method still
  // corresponds to a contiguous run of published rows.

  /**
   * The published holidays for 2005 through 2029, in year order.
   *
   * @return the year to holiday month-day mapping, each row in its published order
   */
  private def thbaRows2005To2029: List[(Int, List[MonthDay])] = List(
    2005 -> List(
      md(1, 3), md(2, 23), md(4, 13), md(4, 14), md(4, 15), md(5, 2), md(5, 5), md(5, 23), md(7, 1), md(7, 22),
      md(8, 12), md(10, 24), md(12, 5), md(12, 12)
    ),
    2006 -> List(
      md(1, 2), md(2, 13), md(4, 6), md(4, 13), md(4, 14), md(5, 1), md(5, 5), md(5, 15), md(7, 10), md(8, 14),
      md(10, 23), md(12, 5), md(12, 11)
    ),
    2007 -> List(
      md(1, 1), md(1, 2), md(3, 5), md(4, 6), md(4, 13), md(4, 16), md(5, 1), md(5, 7), md(6, 1), md(7, 30),
      md(8, 13), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2008 -> List(
      md(1, 1), md(2, 20), md(4, 7), md(4, 14), md(4, 15), md(5, 1), md(5, 5), md(5, 20), md(7, 1), md(7, 18),
      md(8, 12), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2009 -> List(
      md(1, 1), md(2, 9), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 1), md(5, 5), md(5, 11), md(7, 1),
      md(7, 8), md(8, 12), md(10, 23), md(12, 7), md(12, 10), md(12, 31)
    ),
    2010 -> List(
      md(1, 1), md(3, 1), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 3), md(5, 5), md(5, 27), md(7, 1),
      md(7, 27), md(8, 12), md(10, 25), md(12, 6), md(12, 10), md(12, 31)
    ),
    2011 -> List(
      md(1, 3), md(2, 17), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 2), md(5, 5), md(5, 17), md(7, 1),
      md(7, 18), md(8, 12), md(10, 24), md(12, 5), md(12, 12)
    ),
    2012 -> List(
      md(1, 2), md(3, 8), md(4, 6), md(4, 13), md(4, 16), md(5, 1), md(5, 7), md(6, 4), md(8, 3), md(8, 13),
      md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2013 -> List(
      md(1, 1), md(2, 25), md(4, 8), md(4, 15), md(4, 16), md(5, 1), md(5, 6), md(5, 27), md(7, 1), md(7, 23),
      md(8, 12), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2014 -> List(
      md(1, 1), md(2, 14), md(4, 7), md(4, 14), md(4, 15), md(5, 1), md(5, 5), md(5, 14), md(7, 1), md(7, 14),
      md(8, 12), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2015 -> List(
      md(1, 1), md(3, 5), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 1), md(5, 5), md(6, 2), md(7, 1),
      md(7, 31), md(8, 12), md(10, 23), md(12, 7), md(12, 10), md(12, 31)
    ),
    2016 -> List(
      md(1, 1), md(2, 22), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 2), md(5, 5), md(5, 6), md(5, 20),
      md(7, 1), md(7, 18), md(7, 19), md(8, 12), md(10, 24), md(12, 5), md(12, 12)
    ),
    2017 -> List(
      md(1, 2), md(1, 3), md(2, 13), md(4, 6), md(4, 13), md(4, 14), md(5, 1), md(5, 10), md(7, 10), md(7, 28),
      md(8, 14), md(10, 13), md(10, 23), md(10, 26), md(12, 5), md(12, 11)
    ),
    2018 -> List(
      md(1, 1), md(1, 2), md(3, 1), md(4, 6), md(4, 13), md(4, 16), md(5, 1), md(5, 29), md(7, 27), md(7, 30),
      md(8, 13), md(10, 15), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2019 -> List(
      md(1, 1), md(2, 19), md(4, 8), md(4, 15), md(4, 16), md(5, 1), md(5, 20), md(6, 3), md(7, 16), md(7, 29),
      md(8, 12), md(10, 14), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2020 -> List(
      md(1, 1), md(2, 10), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 1), md(5, 6), md(6, 3), md(7, 6),
      md(7, 28), md(8, 12), md(10, 13), md(10, 23), md(12, 7), md(12, 10), md(12, 31)
    ),
    2021 -> List(
      md(1, 1), md(2, 26), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 3), md(5, 26), md(6, 3), md(7, 26),
      md(7, 28), md(8, 12), md(10, 13), md(10, 25), md(12, 6), md(12, 10), md(12, 31)
    ),
    2022 -> List(
      md(1, 3), md(2, 16), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 2), md(5, 16), md(6, 3), md(7, 13),
      md(7, 28), md(8, 12), md(10, 13), md(10, 24), md(12, 5), md(12, 12)
    ),
    2023 -> List(
      md(1, 2), md(3, 6), md(4, 6), md(4, 13), md(4, 14), md(5, 1), md(5, 4), md(5, 5), md(6, 5), md(7, 28),
      md(8, 1), md(8, 14), md(10, 13), md(10, 23), md(12, 5), md(12, 11), md(12, 29)
    ),
    2024 -> List(
      md(1, 1), md(1, 2), md(2, 26), md(4, 8), md(4, 15), md(4, 16), md(5, 1), md(5, 6), md(5, 22), md(6, 3),
      md(7, 22), md(7, 29), md(8, 12), md(10, 14), md(10, 23), md(12, 5), md(12, 10), md(12, 30), md(12, 31)
    ),
    2025 -> List(
      md(1, 1), md(2, 12), md(4, 7), md(4, 14), md(4, 15), md(5, 1), md(5, 5), md(5, 12), md(6, 3), md(7, 10),
      md(7, 28), md(8, 11), md(8, 12), md(10, 13), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2026 -> List(
      md(1, 1), md(1, 2), md(3, 3), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 1), md(5, 4), md(6, 1),
      md(6, 3), md(7, 28), md(7, 29), md(8, 12), md(10, 13), md(10, 23), md(12, 7), md(12, 10), md(12, 31)
    ),
    2027 -> List(
      md(1, 1), md(2, 22), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 3), md(5, 4), md(5, 20), md(6, 3),
      md(7, 19), md(7, 28), md(8, 12), md(10, 13), md(10, 25), md(12, 6), md(12, 10), md(12, 31)
    ),
    2028 -> List(
      md(1, 3), md(2, 10), md(4, 6), md(4, 13), md(4, 14), md(5, 1), md(5, 4), md(5, 8), md(6, 5), md(7, 6),
      md(7, 28), md(8, 14), md(10, 13), md(10, 23), md(12, 5), md(12, 11)
    ),
    2029 -> List(
      md(1, 1), md(1, 2), md(2, 27), md(4, 6), md(4, 13), md(4, 16), md(5, 1), md(5, 4), md(5, 28), md(6, 4),
      md(7, 25), md(7, 30), md(8, 13), md(10, 15), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    )
  )

  /**
   * The published holidays for 2030 through 2054, in year order.
   *
   * @return the year to holiday month-day mapping, each row in its published order
   */
  private def thbaRows2030To2054: List[(Int, List[MonthDay])] = List(
    2030 -> List(
      md(1, 1), md(2, 18), md(4, 8), md(4, 15), md(4, 16), md(5, 1), md(5, 6), md(5, 16), md(6, 3), md(7, 15),
      md(7, 29), md(8, 12), md(10, 14), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2031 -> List(
      md(1, 1), md(3, 7), md(4, 7), md(4, 14), md(4, 15), md(5, 1), md(5, 4), md(6, 3), md(7, 28), md(8, 4),
      md(8, 12), md(10, 13), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2032 -> List(
      md(1, 1), md(1, 2), md(2, 25), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 3), md(5, 4), md(5, 24),
      md(6, 3), md(7, 22), md(7, 28), md(8, 12), md(10, 13), md(10, 25), md(12, 6), md(12, 10), md(12, 31)
    ),
    2033 -> List(
      md(1, 3), md(2, 14), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 2), md(5, 4), md(5, 13), md(6, 3),
      md(7, 11), md(7, 28), md(8, 12), md(10, 13), md(10, 24), md(12, 5), md(12, 12)
    ),
    2034 -> List(
      md(1, 2), md(3, 6), md(4, 6), md(4, 13), md(4, 14), md(5, 1), md(6, 1), md(6, 5), md(7, 28), md(7, 31),
      md(8, 14), md(10, 13), md(10, 23), md(12, 5), md(12, 11)
    ),
    2035 -> List(
      md(1, 1), md(1, 2), md(2, 22), md(4, 6), md(4, 13), md(4, 16), md(5, 1), md(5, 21), md(6, 4), md(7, 20),
      md(7, 30), md(8, 13), md(10, 15), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2036 -> List(
      md(1, 1), md(2, 12), md(4, 7), md(4, 14), md(4, 15), md(5, 1), md(5, 12), md(6, 3), md(7, 8), md(7, 28),
      md(8, 12), md(10, 13), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2037 -> List(
      md(1, 1), md(1, 2), md(3, 2), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 1), md(5, 29), md(6, 3),
      md(7, 27), md(7, 28), md(8, 12), md(10, 13), md(10, 23), md(12, 7), md(12, 10), md(12, 31)
    ),
    2038 -> List(
      md(1, 1), md(2, 19), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 3), md(5, 18), md(6, 3), md(7, 16),
      md(7, 28), md(8, 12), md(10, 13), md(10, 25), md(12, 6), md(12, 10), md(12, 31)
    ),
    2039 -> List(
      md(1, 3), md(2, 8), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 2), md(5, 9), md(6, 3), md(7, 5),
      md(7, 28), md(8, 12), md(10, 13), md(10, 24), md(12, 5), md(12, 12)
    ),
    2040 -> List(
      md(1, 2), md(2, 27), md(4, 6), md(4, 13), md(4, 16), md(5, 1), md(5, 25), md(6, 4), md(7, 23), md(7, 30),
      md(8, 13), md(10, 15), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2041 -> List(
      md(1, 1), md(2, 15), md(4, 8), md(4, 15), md(4, 16), md(5, 1), md(5, 14), md(6, 3), md(7, 15), md(7, 29),
      md(8, 12), md(10, 14), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2042 -> List(
      md(1, 1), md(3, 6), md(4, 7), md(4, 14), md(4, 15), md(5, 1), md(6, 3), md(7, 28), md(8, 1), md(8, 12),
      md(10, 13), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2043 -> List(
      md(1, 1), md(1, 2), md(2, 24), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 1), md(5, 25), md(6, 3),
      md(7, 21), md(7, 28), md(8, 12), md(10, 13), md(10, 23), md(12, 7), md(12, 10), md(12, 31)
    ),
    2044 -> List(
      md(1, 1), md(2, 15), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 2), md(5, 11), md(6, 3), md(7, 11),
      md(7, 28), md(8, 12), md(10, 13), md(10, 24), md(12, 5), md(12, 12)
    ),
    2045 -> List(
      md(1, 2), md(3, 2), md(4, 6), md(4, 13), md(4, 14), md(5, 1), md(5, 30), md(6, 5), md(7, 28), md(8, 14),
      md(10, 13), md(10, 23), md(12, 5), md(12, 11)
    ),
    2046 -> List(
      md(1, 1), md(1, 2), md(2, 20), md(4, 6), md(4, 13), md(4, 16), md(5, 1), md(5, 21), md(6, 4), md(7, 17),
      md(7, 30), md(8, 13), md(10, 15), md(10, 23), md(12, 5), md(12, 10)
    ),
    2047 -> List(
      md(1, 1), md(2, 11), md(4, 8), md(4, 15), md(4, 16), md(5, 1), md(5, 8), md(6, 3), md(7, 8), md(7, 29),
      md(8, 12), md(10, 14), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2048 -> List(
      md(1, 1), md(2, 28), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 1), md(5, 27), md(6, 3), md(7, 27),
      md(7, 28), md(8, 12), md(10, 13), md(10, 23), md(12, 7), md(12, 10), md(12, 31)
    ),
    2049 -> List(
      md(1, 1), md(2, 17), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 3), md(5, 17), md(6, 3), md(7, 14),
      md(7, 28), md(8, 12), md(10, 13), md(10, 25), md(12, 6), md(12, 10), md(12, 31)
    ),
    2050 -> List(
      md(1, 3), md(3, 7), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 2), md(6, 3), md(6, 6), md(7, 28),
      md(8, 2), md(8, 12), md(10, 13), md(10, 24), md(12, 5), md(12, 12)
    ),
    2051 -> List(
      md(1, 2), md(2, 27), md(4, 6), md(4, 13), md(4, 14), md(5, 1), md(5, 24), md(6, 5), md(7, 24), md(7, 28),
      md(8, 14), md(10, 13), md(10, 23), md(12, 5), md(12, 11)
    ),
    2052 -> List(
      md(1, 1), md(1, 2), md(2, 14), md(4, 8), md(4, 15), md(4, 16), md(5, 1), md(5, 13), md(6, 3), md(7, 11),
      md(7, 29), md(8, 12), md(10, 14), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2053 -> List(
      md(1, 1), md(3, 4), md(4, 7), md(4, 14), md(4, 15), md(5, 1), md(6, 2), md(6, 3), md(7, 28), md(7, 30),
      md(8, 12), md(10, 13), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2054 -> List(
      md(1, 1), md(1, 2), md(2, 23), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 1), md(5, 21), md(6, 3),
      md(7, 20), md(7, 28), md(8, 12), md(10, 13), md(10, 23), md(12, 7), md(12, 10), md(12, 31)
    )
  )

  /**
   * The published holidays for 2055 through 2079, in year order.
   *
   * @return the year to holiday month-day mapping, each row in its published order
   */
  private def thbaRows2055To2079: List[(Int, List[MonthDay])] = List(
    2055 -> List(
      md(1, 1), md(2, 11), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 3), md(5, 10), md(6, 3), md(7, 8),
      md(7, 28), md(8, 12), md(10, 13), md(10, 25), md(12, 6), md(12, 10), md(12, 31)
    ),
    2056 -> List(
      md(1, 3), md(2, 29), md(4, 6), md(4, 13), md(4, 14), md(5, 1), md(5, 29), md(6, 5), md(7, 27), md(7, 28),
      md(8, 14), md(10, 13), md(10, 23), md(12, 5), md(12, 11)
    ),
    2057 -> List(
      md(1, 1), md(1, 2), md(2, 19), md(4, 6), md(4, 13), md(4, 16), md(5, 1), md(5, 18), md(6, 4), md(7, 16),
      md(7, 30), md(8, 13), md(10, 15), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2058 -> List(
      md(1, 1), md(2, 8), md(4, 8), md(4, 15), md(4, 16), md(5, 1), md(5, 7), md(6, 3), md(7, 5), md(7, 29),
      md(8, 12), md(10, 14), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2059 -> List(
      md(1, 1), md(2, 26), md(4, 7), md(4, 14), md(4, 15), md(5, 1), md(5, 26), md(6, 3), md(7, 24), md(7, 28),
      md(8, 12), md(10, 13), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2060 -> List(
      md(1, 1), md(1, 2), md(2, 16), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 3), md(5, 14), md(6, 3),
      md(7, 12), md(7, 28), md(8, 12), md(10, 13), md(10, 25), md(12, 6), md(12, 10), md(12, 31)
    ),
    2061 -> List(
      md(1, 3), md(3, 7), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 2), md(6, 2), md(6, 3), md(7, 28),
      md(8, 1), md(8, 12), md(10, 13), md(10, 24), md(12, 5), md(12, 12)
    ),
    2062 -> List(
      md(1, 2), md(2, 24), md(4, 6), md(4, 13), md(4, 14), md(5, 1), md(5, 23), md(6, 5), md(7, 21), md(7, 28),
      md(8, 14), md(10, 13), md(10, 23), md(12, 5), md(12, 11)
    ),
    2063 -> List(
      md(1, 1), md(1, 2), md(2, 13), md(4, 6), md(4, 13), md(4, 16), md(5, 1), md(5, 14), md(6, 4), md(7, 10),
      md(7, 30), md(8, 13), md(10, 15), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2064 -> List(
      md(1, 1), md(3, 3), md(4, 7), md(4, 14), md(4, 15), md(5, 1), md(5, 30), md(6, 3), md(7, 28), md(8, 12),
      md(10, 13), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2065 -> List(
      md(1, 1), md(1, 2), md(2, 20), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 1), md(5, 19), md(6, 3),
      md(7, 17), md(7, 28), md(8, 12), md(10, 13), md(10, 23), md(12, 7), md(12, 10), md(12, 31)
    ),
    2066 -> List(
      md(1, 1), md(2, 9), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 3), md(5, 10), md(6, 3), md(7, 6),
      md(7, 28), md(8, 12), md(10, 13), md(10, 25), md(12, 6), md(12, 10), md(12, 31)
    ),
    2067 -> List(
      md(1, 3), md(2, 28), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 2), md(5, 27), md(6, 3), md(7, 26),
      md(7, 28), md(8, 12), md(10, 13), md(10, 24), md(12, 5), md(12, 12)
    ),
    2068 -> List(
      md(1, 2), md(2, 20), md(4, 6), md(4, 13), md(4, 16), md(5, 1), md(5, 16), md(6, 4), md(7, 16), md(7, 30),
      md(8, 13), md(10, 15), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2069 -> List(
      md(1, 1), md(3, 7), md(4, 8), md(4, 15), md(4, 16), md(5, 1), md(6, 3), md(7, 29), md(8, 2), md(8, 12),
      md(10, 14), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2070 -> List(
      md(1, 1), md(2, 25), md(4, 7), md(4, 14), md(4, 15), md(5, 1), md(5, 26), md(6, 3), md(7, 22), md(7, 28),
      md(8, 12), md(10, 13), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2071 -> List(
      md(1, 1), md(1, 2), md(2, 16), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 1), md(5, 13), md(6, 3),
      md(7, 13), md(7, 28), md(8, 12), md(10, 13), md(10, 23), md(12, 7), md(12, 10), md(12, 31)
    ),
    2072 -> List(
      md(1, 1), md(3, 3), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 2), md(5, 31), md(6, 3), md(7, 28),
      md(8, 1), md(8, 12), md(10, 13), md(10, 24), md(12, 5), md(12, 12)
    ),
    2073 -> List(
      md(1, 2), md(2, 22), md(4, 6), md(4, 13), md(4, 14), md(5, 1), md(5, 22), md(6, 5), md(7, 19), md(7, 28),
      md(8, 14), md(10, 13), md(10, 23), md(12, 5), md(12, 11)
    ),
    2074 -> List(
      md(1, 1), md(1, 2), md(2, 12), md(4, 6), md(4, 13), md(4, 16), md(5, 1), md(5, 10), md(6, 4), md(7, 9),
      md(7, 30), md(8, 13), md(10, 15), md(10, 23), md(12, 5), md(12, 10)
    ),
    2075 -> List(
      md(1, 1), md(3, 1), md(4, 8), md(4, 15), md(4, 16), md(5, 1), md(5, 29), md(6, 3), md(7, 29), md(8, 12),
      md(10, 14), md(10, 23), md(12, 5), md(12, 10), md(12, 31)
    ),
    2076 -> List(
      md(1, 1), md(2, 19), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 1), md(5, 18), md(6, 3), md(7, 15),
      md(7, 28), md(8, 12), md(10, 13), md(10, 23), md(12, 7), md(12, 10), md(12, 31)
    ),
    2077 -> List(
      md(1, 1), md(2, 8), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 3), md(5, 6), md(6, 3), md(7, 5),
      md(7, 28), md(8, 12), md(10, 13), md(10, 25), md(12, 6), md(12, 10), md(12, 31)
    ),
    2078 -> List(
      md(1, 3), md(2, 25), md(4, 6), md(4, 13), md(4, 14), md(4, 15), md(5, 2), md(5, 25), md(6, 3), md(7, 25),
      md(7, 28), md(8, 12), md(10, 13), md(10, 24), md(12, 5), md(12, 12)
    ),
    2079 -> List(
      md(1, 2), md(2, 16), md(4, 6), md(4, 13), md(4, 14), md(5, 1), md(5, 15), md(6, 5), md(7, 13), md(7, 28),
      md(8, 14), md(10, 13), md(10, 23), md(12, 5), md(12, 11)
    )
  )

  /**
   * The Thai bank holidays, keyed by year.
   *
   * Every year from 2005 to 2079 inclusive is present and maps to a non-empty list of
   * month-day pairs in ascending order. The map is sorted by year, so iterating it yields
   * the rows in the order they are published.
   */
  val thba: SortedMap[Int, List[MonthDay]] =
    SortedMap.from(
      thbaRows2005To2029 ::: thbaRows2030To2054 ::: thbaRows2055To2079
    )

  /**
   * The weekend for the Thai bank calendar.
   *
   * The published section declares `Sat,Sun`, so Saturday and Sunday are non-business days
   * for every year, including the years outside the range covered by [[thba]].
   */
  val thbaWeekendDays: Set[DayOfWeek] = Set(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY)

  /**
   * The Thai bank holidays as absolute dates, in ascending order.
   *
   * Each month-day of [[thba]] is resolved against the year it is recorded under. Because
   * [[thba]] is sorted by year and every row is itself ascending, the result is strictly
   * ascending and free of duplicates. The dates are returned exactly as published, so a
   * holiday falling on a weekend is included rather than dropped.
   *
   * @return the 1220 published holiday dates, ascending
   */
  def thbaHolidays: List[LocalDate] = resolvedThbaHolidays

  // Resolving the rows allocates over a thousand dates, and the result is immutable and
  // independent of any argument, so it is computed once on first use and shared from then
  // on. The public accessor above stays a method, which keeps callers free of the
  // initialisation order of this object.
  private lazy val resolvedThbaHolidays: List[LocalDate] =
    thba.toList.flatMap { case (year, monthDays) => monthDays.map(_.atYear(year)) }
}
