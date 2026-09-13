/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.DayOfWeek.SATURDAY
import java.time.DayOfWeek.SUNDAY
import java.time.LocalDate
import java.time.MonthDay

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableFor3

import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[GlobalHolidayCalendars]].
 *
 * [[GlobalHolidayCalendars]] is a rule engine rather than a data file: each of its twenty-five
 * generators expresses the legislation of one financial centre as a function of the year, and
 * nothing in a generator says which dates come out. The expected dates below were established by
 * hand from the published schedules, Hansard records and government sources cited alongside them -
 * the only independent evidence that the rules produce the intended calendars.
 *
 * Every year-row is checked for '''every day of that year''': the calendar must report a holiday
 * on exactly the dates the row names and on the weekend, and a business day on every other date,
 * so the absence of a holiday is asserted as strongly as its presence.
 *
 * The calendars come from the generators directly rather than from `StandardHolidayCalendars`,
 * which holds the same calendars memoised and would only prove that its table names the right
 * generators. Each fixture is a `lazy val` because regenerating a century and a half of holidays
 * inside a table row would be pathologically slow.
 */
final class GlobalHolidayCalendarsSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  //-------------------------------------------------------------------------
  /** A month (1 to 12) and a day: a row states its year once, so it cannot disagree with itself. */
  private def md(month: Int, day: Int): MonthDay = MonthDay.of(month, day)

  /** Resolves the month-day pairs of a row against its year, in the order they are written. */
  private def mds(year: Int, monthDays: MonthDay*): List[LocalDate] =
    monthDays.iterator.map(monthDay => monthDay.atYear(year)).toList

  /** Every day of a year, bounded by the length of the year so that a leap day is included. */
  private def daysOfYear(year: Int): Iterator[LocalDate] = {
    val firstDay = LocalDate.of(year, 1, 1)
    Iterator.iterate(firstDay)(day => day.plusDays(1)).take(firstDay.lengthOfYear)
  }

  /**
   * Whether a date falls at the Saturday-Sunday weekend. Twenty-four of the twenty-five calendars
   * close at that weekend and their rows need not list weekend days, so the weekend is added back
   * before the expectation is complete; Budapest is the exception and has its own assertion.
   */
  private def isWeekend(day: LocalDate): Boolean =
    day.getDayOfWeek == SATURDAY || day.getDayOfWeek == SUNDAY

  /**
   * Asserts a calendar over a whole year: a date is a holiday if and only if the row names it or
   * it falls at the weekend, so the absence of a holiday is asserted as strongly as its presence.
   * The date is carried in the clue so that a failure names the day that disagreed.
   */
  private def assertHolidays(
      calendar: HolidayCalendar,
      year: Int,
      holidays: List[LocalDate]): Assertion = {

    daysOfYear(year).foreach { day =>
      withClue(s"$day: ") {
        calendar.isHoliday(day) shouldBe (holidays.contains(day) || isWeekend(day))
      }
    }
    succeed
  }

  /**
   * Asserts the Budapest calendar over a whole year. Budapest is the one centre whose weekend is a
   * single day: its calendar declares Sunday as its only weekend day and lists every Saturday that
   * is not worked as a holiday, because a Hungarian holiday falling midweek is bridged by working
   * the Saturday that year's decree names. The expectation is therefore the holidays and the
   * weekend '''less''' the working days - a Saturday named as a working day is a business day, and
   * a bridged Monday or Friday is a holiday.
   */
  private def assertBudapest(
      year: Int,
      holidays: List[LocalDate],
      workDays: List[LocalDate]): Assertion = {

    daysOfYear(year).foreach { day =>
      withClue(s"$day: ") {
        HUBU.isHoliday(day) shouldBe
          ((holidays.contains(day) || isWeekend(day)) && !workDays.contains(day))
      }
    }
    succeed
  }

  //-------------------------------------------------------------------------
  /**
   * Easter Sunday from 1900 to 2099, as published, in columns of day, month and year; the table opens
   * with 1900 twice and the duplicate asserts that year a second time. The anonymous Gregorian
   * algorithm behind [[GlobalHolidayCalendars.easter]] feeds Good Friday, Easter Monday, Ascension,
   * Whitsun, Corpus Christi and Carnival across most of the European and Latin American calendars,
   * so an error here would cascade through the calendars below.
   */
  private val data_easter: TableFor3[Int, Int, Int] = Table(
    ("day", "month", "year"),
    (15, 4, 1900),
    (15, 4, 1900),
    (7, 4, 1901),
    (30, 3, 1902),
    (12, 4, 1903),
    (3, 4, 1904),
    (23, 4, 1905),
    (15, 4, 1906),
    (31, 3, 1907),
    (19, 4, 1908),
    (11, 4, 1909),
    (27, 3, 1910),
    (16, 4, 1911),
    (7, 4, 1912),
    (23, 3, 1913),
    (12, 4, 1914),
    (4, 4, 1915),
    (23, 4, 1916),
    (8, 4, 1917),
    (31, 3, 1918),
    (20, 4, 1919),
    (4, 4, 1920),
    (27, 3, 1921),
    (16, 4, 1922),
    (1, 4, 1923),
    (20, 4, 1924),
    (12, 4, 1925),
    (4, 4, 1926),
    (17, 4, 1927),
    (8, 4, 1928),
    (31, 3, 1929),
    (20, 4, 1930),
    (5, 4, 1931),
    (27, 3, 1932),
    (16, 4, 1933),
    (1, 4, 1934),
    (21, 4, 1935),
    (12, 4, 1936),
    (28, 3, 1937),
    (17, 4, 1938),
    (9, 4, 1939),
    (24, 3, 1940),
    (13, 4, 1941),
    (5, 4, 1942),
    (25, 4, 1943),
    (9, 4, 1944),
    (1, 4, 1945),
    (21, 4, 1946),
    (6, 4, 1947),
    (28, 3, 1948),
    (17, 4, 1949),
    (9, 4, 1950),
    (25, 3, 1951),
    (13, 4, 1952),
    (5, 4, 1953),
    (18, 4, 1954),
    (10, 4, 1955),
    (1, 4, 1956),
    (21, 4, 1957),
    (6, 4, 1958),
    (29, 3, 1959),
    (17, 4, 1960),
    (2, 4, 1961),
    (22, 4, 1962),
    (14, 4, 1963),
    (29, 3, 1964),
    (18, 4, 1965),
    (10, 4, 1966),
    (26, 3, 1967),
    (14, 4, 1968),
    (6, 4, 1969),
    (29, 3, 1970),
    (11, 4, 1971),
    (2, 4, 1972),
    (22, 4, 1973),
    (14, 4, 1974),
    (30, 3, 1975),
    (18, 4, 1976),
    (10, 4, 1977),
    (26, 3, 1978),
    (15, 4, 1979),
    (6, 4, 1980),
    (19, 4, 1981),
    (11, 4, 1982),
    (3, 4, 1983),
    (22, 4, 1984),
    (7, 4, 1985),
    (30, 3, 1986),
    (19, 4, 1987),
    (3, 4, 1988),
    (26, 3, 1989),
    (15, 4, 1990),
    (31, 3, 1991),
    (19, 4, 1992),
    (11, 4, 1993),
    (3, 4, 1994),
    (16, 4, 1995),
    (7, 4, 1996),
    (30, 3, 1997),
    (12, 4, 1998),
    (4, 4, 1999),
    (23, 4, 2000),
    (15, 4, 2001),
    (31, 3, 2002),
    (20, 4, 2003),
    (11, 4, 2004),
    (27, 3, 2005),
    (16, 4, 2006),
    (8, 4, 2007),
    (23, 3, 2008),
    (12, 4, 2009),
    (4, 4, 2010),
    (24, 4, 2011),
    (8, 4, 2012),
    (31, 3, 2013),
    (20, 4, 2014),
    (5, 4, 2015),
    (27, 3, 2016),
    (16, 4, 2017),
    (1, 4, 2018),
    (21, 4, 2019),
    (12, 4, 2020),
    (4, 4, 2021),
    (17, 4, 2022),
    (9, 4, 2023),
    (31, 3, 2024),
    (20, 4, 2025),
    (5, 4, 2026),
    (28, 3, 2027),
    (16, 4, 2028),
    (1, 4, 2029),
    (21, 4, 2030),
    (13, 4, 2031),
    (28, 3, 2032),
    (17, 4, 2033),
    (9, 4, 2034),
    (25, 3, 2035),
    (13, 4, 2036),
    (5, 4, 2037),
    (25, 4, 2038),
    (10, 4, 2039),
    (1, 4, 2040),
    (21, 4, 2041),
    (6, 4, 2042),
    (29, 3, 2043),
    (17, 4, 2044),
    (9, 4, 2045),
    (25, 3, 2046),
    (14, 4, 2047),
    (5, 4, 2048),
    (18, 4, 2049),
    (10, 4, 2050),
    (2, 4, 2051),
    (21, 4, 2052),
    (6, 4, 2053),
    (29, 3, 2054),
    (18, 4, 2055),
    (2, 4, 2056),
    (22, 4, 2057),
    (14, 4, 2058),
    (30, 3, 2059),
    (18, 4, 2060),
    (10, 4, 2061),
    (26, 3, 2062),
    (15, 4, 2063),
    (6, 4, 2064),
    (29, 3, 2065),
    (11, 4, 2066),
    (3, 4, 2067),
    (22, 4, 2068),
    (14, 4, 2069),
    (30, 3, 2070),
    (19, 4, 2071),
    (10, 4, 2072),
    (26, 3, 2073),
    (15, 4, 2074),
    (7, 4, 2075),
    (19, 4, 2076),
    (11, 4, 2077),
    (3, 4, 2078),
    (23, 4, 2079),
    (7, 4, 2080),
    (30, 3, 2081),
    (19, 4, 2082),
    (4, 4, 2083),
    (26, 3, 2084),
    (15, 4, 2085),
    (31, 3, 2086),
    (20, 4, 2087),
    (11, 4, 2088),
    (3, 4, 2089),
    (16, 4, 2090),
    (8, 4, 2091),
    (30, 3, 2092),
    (12, 4, 2093),
    (4, 4, 2094),
    (24, 4, 2095),
    (15, 4, 2096),
    (31, 3, 2097),
    (20, 4, 2098),
    (12, 4, 2099))

  test("test_easter") {
    forAll(data_easter) { (day: Int, month: Int, year: Int) =>
      GlobalHolidayCalendars.easter(year) shouldBe LocalDate.of(year, month, day)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val GBLO: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateLondon()

  /**
   * The rows before 1971 predate the Banking and Financial Dealings Act, when holidays were set
   * year by year, and the Hansard citations below are the provenance of dates no rule produces. From
   * 1971 the rules apply and the later rows are the published schedule, including the Diamond
   * Jubilee in 2012, the Platinum Jubilee and state funeral in 2022 and the coronation in 2023.
   */
  private val data_gblo: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    // Whitsun, Last Mon Aug - http://hansard.millbanksystems.com/commons/1964/mar/04/staggered-holidays
    (1965, mds(1965, md(4, 16), md(4, 19), md(6, 7), md(8, 30), md(12, 27), md(12, 28))),
    // Whitsun May - http://hansard.millbanksystems.com/commons/1964/mar/04/staggered-holidays
    // 29th Aug - http://hansard.millbanksystems.com/written_answers/1965/nov/25/august-bank-holiday
    (1966, mds(1966, md(4, 8), md(4, 11), md(5, 30), md(8, 29), md(12, 26), md(12, 27))),
    // 29th May, 28th Aug - http://hansard.millbanksystems.com/written_answers/1965/jun/03/bank-holidays-1967-and-1968
    (1967, mds(1967, md(3, 24), md(3, 27), md(5, 29), md(8, 28), md(12, 25), md(12, 26))),
    // 3rd Jun, 2nd Sep - http://hansard.millbanksystems.com/written_answers/1965/jun/03/bank-holidays-1967-and-1968
    (1968, mds(1968, md(4, 12), md(4, 15), md(6, 3), md(9, 2), md(12, 25), md(12, 26))),
    // 26th May, 1st Sep - http://hansard.millbanksystems.com/written_answers/1967/mar/21/bank-holidays-1969-dates
    (1969, mds(1969, md(4, 4), md(4, 7), md(5, 26), md(9, 1), md(12, 25), md(12, 26))),
    // 25th May, 31st Aug - http://hansard.millbanksystems.com/written_answers/1967/jul/28/bank-holidays
    (1970, mds(1970, md(3, 27), md(3, 30), md(5, 25), md(8, 31), md(12, 25), md(12, 28))),
    (1971, mds(1971, md(4, 9), md(4, 12), md(5, 31), md(8, 30), md(12, 27), md(12, 28))),
    (2009, mds(2009, md(1, 1), md(4, 10), md(4, 13), md(5, 4), md(5, 25), md(8, 31), md(12, 25), md(12, 28))),
    (2010, mds(2010, md(1, 1), md(4, 2), md(4, 5), md(5, 3), md(5, 31), md(8, 30), md(12, 27), md(12, 28))),
    // https://www.gov.uk/bank-holidays
    (2012, mds(2012, md(1, 2), md(4, 6), md(4, 9), md(5, 7), md(6, 4), md(6, 5), md(8, 27), md(12, 25), md(12, 26))),
    (2013, mds(2013, md(1, 1), md(3, 29), md(4, 1), md(5, 6), md(5, 27), md(8, 26), md(12, 25), md(12, 26))),
    (2014, mds(2014, md(1, 1), md(4, 18), md(4, 21), md(5, 5), md(5, 26), md(8, 25), md(12, 25), md(12, 26))),
    (2015, mds(2015, md(1, 1), md(4, 3), md(4, 6), md(5, 4), md(5, 25), md(8, 31), md(12, 25), md(12, 28))),
    (2016, mds(2016, md(1, 1), md(3, 25), md(3, 28), md(5, 2), md(5, 30), md(8, 29), md(12, 26), md(12, 27))),
    (2020, mds(2020, md(1, 1), md(4, 10), md(4, 13), md(5, 8), md(5, 25), md(8, 31), md(12, 25), md(12, 28))),
    (
      2022,
      mds(
        2022,
        md(1, 3),
        md(4, 15),
        md(4, 18),
        md(5, 2),
        md(6, 2),
        md(6, 3),
        md(8, 29),
        md(9, 19),
        md(12, 26),
        md(12, 27))),
    (2023, mds(2023, md(1, 2), md(4, 7), md(4, 10), md(5, 1), md(5, 8), md(5, 29), md(8, 28), md(12, 25), md(12, 26))))

  test("test_gblo") {
    forAll(data_gblo) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(GBLO, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val FRPA: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateParis()

  /** France does not move a holiday off a weekend, so no substitute day may be closed for one. */
  private val data_frpa: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    (2003, mds(2003, md(1, 1), md(4, 18), md(4, 21), md(5, 1), md(5, 8), md(5, 29),
      md(6, 9), md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))),
    (2004, mds(2004, md(1, 1), md(4, 9), md(4, 12), md(5, 1), md(5, 8), md(5, 20), md(5, 31),
      md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))),
    (2005, mds(2005, md(1, 1), md(3, 25), md(3, 28), md(5, 1), md(5, 5), md(5, 8),
      md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))),
    (2006, mds(2006, md(1, 1), md(4, 14), md(4, 17), md(5, 1), md(5, 8), md(5, 25),
      md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))),
    (2007, mds(2007, md(1, 1), md(4, 6), md(4, 9), md(5, 1), md(5, 8), md(5, 17),
      md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))),
    (2008, mds(2008, md(1, 1), md(3, 21), md(3, 24), md(5, 1), md(5, 8), md(5, 12), md(5, 24),
      md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))),
    (2012, mds(2012, md(1, 1), md(4, 6), md(4, 9), md(5, 1), md(5, 8), md(5, 17),
      md(5, 28), md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))),
    (2013, mds(2013, md(1, 1), md(3, 29), md(4, 1), md(5, 1), md(5, 8), md(5, 9), md(5, 20),
      md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))),
    (2014, mds(2014, md(1, 1), md(4, 18), md(4, 21), md(5, 1), md(5, 8), md(5, 29),
      md(6, 9), md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))),
    (2015, mds(2015, md(1, 1), md(4, 3), md(4, 6), md(5, 1), md(5, 8), md(5, 14), md(5, 25),
      md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))),
    (2016, mds(2016, md(1, 1), md(3, 25), md(3, 28), md(5, 1), md(5, 5), md(5, 8), md(5, 16),
      md(7, 14), md(8, 15), md(11, 1), md(11, 11), md(12, 25), md(12, 26))))

  test("test_frpa") {
    forAll(data_frpa) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(FRPA, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val DEFR: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateFrankfurt()

  /** The 2017 row alone carries Reformation Day on the 31st of October, a one-off national holiday. */
  private val data_defr: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    // dates not shifted if fall on a weekend
    (2014, mds(2014, md(1, 1), md(4, 18), md(4, 21), md(5, 1), md(5, 29), md(6, 9), md(6, 19),
      md(10, 3), md(12, 24), md(12, 25), md(12, 26), md(12, 31))),
    (2015, mds(2015, md(1, 1), md(4, 3), md(4, 6), md(5, 1), md(5, 14), md(5, 25), md(6, 4),
      md(10, 3), md(12, 24), md(12, 25), md(12, 26), md(12, 31))),
    (2016, mds(2016, md(1, 1), md(3, 25), md(3, 28), md(5, 1), md(5, 5), md(5, 16), md(5, 26),
      md(10, 3), md(12, 25), md(12, 26), md(12, 31))),
    (2017, mds(2017, md(1, 1), md(4, 14), md(4, 17), md(5, 1), md(5, 25), md(6, 5), md(6, 15),
      md(10, 3), md(10, 31), md(12, 25), md(12, 26), md(12, 31))))

  test("test_defr") {
    forAll(data_defr) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(DEFR, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val CHZU: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateZurich()

  private val data_chzu: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    // dates not shifted if fall on a weekend
    (2012, mds(2012, md(1, 1), md(1, 2), md(4, 6), md(4, 9), md(5, 1), md(5, 17), md(5, 28),
      md(8, 1), md(12, 25), md(12, 26))),
    (2013, mds(2013, md(1, 1), md(1, 2), md(3, 29), md(4, 1), md(5, 1), md(5, 9), md(5, 20),
      md(8, 1), md(12, 25), md(12, 26))),
    (2014, mds(2014, md(1, 1), md(1, 2), md(4, 18), md(4, 21), md(5, 1), md(5, 29), md(6, 9),
      md(8, 1), md(12, 25), md(12, 26))),
    (2015, mds(2015, md(1, 1), md(1, 2), md(4, 3), md(4, 6), md(5, 1), md(5, 14), md(5, 25),
      md(8, 1), md(12, 25), md(12, 26))),
    (2016, mds(2016, md(1, 1), md(1, 2), md(3, 25), md(3, 28), md(5, 1), md(5, 5), md(5, 16),
      md(8, 1), md(12, 25), md(12, 26))))

  test("test_chzu") {
    forAll(data_chzu) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(CHZU, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val EUTA: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateEuropeanTarget()

  /**
   * TARGET did not exist before 1997, so its generator covers 1997 to 2099 rather than 1950 to 2099
   * and every row lies inside that range. Through the testing phase of 1997 and 1998 it closed only
   * on New Year's Day and Christmas Day; 1999 and 2001 add New Year's Eve, and the settled schedule -
   * those two plus Good Friday, Easter Monday, Labour Day and the 26th of December - runs from 2000.
   */
  private val data_euta: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    (1997, mds(1997, md(1, 1), md(12, 25))),
    (1998, mds(1998, md(1, 1), md(12, 25))),
    (1999, mds(1999, md(1, 1), md(12, 25), md(12, 31))),
    (2000, mds(2000, md(1, 1), md(4, 21), md(4, 24), md(5, 1), md(12, 25), md(12, 26))),
    (2001, mds(2001, md(1, 1), md(4, 13), md(4, 16), md(5, 1), md(12, 25), md(12, 26), md(12, 31))),
    (2002, mds(2002, md(1, 1), md(3, 29), md(4, 1), md(5, 1), md(12, 25), md(12, 26))),
    (2003, mds(2003, md(1, 1), md(4, 18), md(4, 21), md(5, 1), md(12, 25), md(12, 26))),
    // http://www.ecb.europa.eu/home/html/holidays.en.html
    (2014, mds(2014, md(1, 1), md(4, 18), md(4, 21), md(5, 1), md(12, 25), md(12, 26))),
    (2015, mds(2015, md(1, 1), md(4, 3), md(4, 6), md(5, 1), md(12, 25), md(12, 26))))

  test("test_euta") {
    forAll(data_euta) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(EUTA, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val USGS: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateUsGovtSecurities()

  /**
   * The SIFMA recommendations, and the most exacting rows here because more than one weekend rule
   * applies at once: Independence Day and Christmas Day shift both ways - Sunday to the following
   * Monday, Saturday to the preceding Friday - while New Year's Day and Veterans Day are not observed
   * at all when they fall on a Saturday, and Good Friday is observed though it is not federal.
   */
  private val data_usgs: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    // http://www.sifma.org/uploadedfiles/research/statistics/statisticsfiles/misc-us-historical-holiday-market-recommendations-sifma.pdf?n=53384
    (1996, mds(1996, md(1, 1), md(1, 15), md(2, 19), md(4, 5), md(5, 27), md(7, 4),
      md(9, 2), md(10, 14), md(11, 11), md(11, 28), md(12, 25))),
    (1997, mds(1997, md(1, 1), md(1, 20), md(2, 17), md(3, 28), md(5, 26), md(7, 4),
      md(9, 1), md(10, 13), md(11, 11), md(11, 27), md(12, 25))),
    (1998, mds(1998, md(1, 1), md(1, 19), md(2, 16), md(4, 10), md(5, 25), md(7, 3),
      md(9, 7), md(10, 12), md(11, 11), md(11, 26), md(12, 25))),
    (1999, mds(1999, md(1, 1), md(1, 18), md(2, 15), md(4, 2), md(5, 31), md(7, 5),
      md(9, 6), md(10, 11), md(11, 11), md(11, 25), md(12, 24))),
    (2000, mds(2000, md(1, 17), md(2, 21), md(4, 21), md(5, 29), md(7, 4),
      md(9, 4), md(10, 9), md(11, 23), md(12, 25))),
    (2001, mds(2001, md(1, 1), md(1, 15), md(2, 19), md(4, 13), md(5, 28), md(7, 4),
      md(9, 3), md(10, 8), md(11, 12), md(11, 22), md(12, 25))),
    (2002, mds(2002, md(1, 1), md(1, 21), md(2, 18), md(3, 29), md(5, 27), md(7, 4),
      md(9, 2), md(10, 14), md(11, 11), md(11, 28), md(12, 25))),
    (2003, mds(2003, md(1, 1), md(1, 20), md(2, 17), md(4, 18), md(5, 26), md(7, 4),
      md(9, 1), md(10, 13), md(11, 11), md(11, 27), md(12, 25))),
    (2004, mds(2004, md(1, 1), md(1, 19), md(2, 16), md(4, 9), md(5, 31), md(7, 5),
      md(9, 6), md(10, 11), md(11, 11), md(11, 25), md(12, 24))),
    (2005, mds(2005, md(1, 17), md(2, 21), md(3, 25), md(5, 30), md(7, 4),
      md(9, 5), md(10, 10), md(11, 11), md(11, 24), md(12, 26))),
    (2006, mds(2006, md(1, 2), md(1, 16), md(2, 20), md(4, 14), md(5, 29), md(7, 4),
      md(9, 4), md(10, 9), md(11, 23), md(12, 25))),
    (2007, mds(2007, md(1, 1), md(1, 15), md(2, 19), md(4, 6), md(5, 28), md(7, 4),
      md(9, 3), md(10, 8), md(11, 12), md(11, 22), md(12, 25))),
    (2008, mds(2008, md(1, 1), md(1, 21), md(2, 18), md(3, 21), md(5, 26), md(7, 4),
      md(9, 1), md(10, 13), md(11, 11), md(11, 27), md(12, 25))),
    (2009, mds(2009, md(1, 1), md(1, 19), md(2, 16), md(4, 10), md(5, 25), md(7, 3),
      md(9, 7), md(10, 12), md(11, 11), md(11, 26), md(12, 25))),
    (2010, mds(2010, md(1, 1), md(1, 18), md(2, 15), md(4, 2), md(5, 31), md(7, 5),
      md(9, 6), md(10, 11), md(11, 11), md(11, 25), md(12, 24))),
    (2011, mds(2011, md(1, 17), md(2, 21), md(4, 22), md(5, 30), md(7, 4),
      md(9, 5), md(10, 10), md(11, 11), md(11, 24), md(12, 26))),
    (2012, mds(2012, md(1, 2), md(1, 16), md(2, 20), md(4, 6), md(5, 28), md(7, 4),
      md(9, 3), md(10, 8), md(10, 30), md(11, 12), md(11, 22), md(12, 25))),
    (2013, mds(2013, md(1, 1), md(1, 21), md(2, 18), md(3, 29), md(5, 27), md(7, 4),
      md(9, 2), md(10, 14), md(11, 11), md(11, 28), md(12, 25))),
    (2014, mds(2014, md(1, 1), md(1, 20), md(2, 17), md(4, 18), md(5, 26), md(7, 4),
      md(9, 1), md(10, 13), md(11, 11), md(11, 27), md(12, 25))),
    (2015, mds(2015, md(1, 1), md(1, 19), md(2, 16), md(4, 3), md(5, 25), md(7, 3),
      md(9, 7), md(10, 12), md(11, 11), md(11, 26), md(12, 25))))

  test("test_usgs") {
    forAll(data_usgs) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(USGS, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val USNY: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateUsNewYork()

  /**
   * New York observes the federal holidays without Good Friday, which distinguishes these rows from
   * the government securities rows above; the 2022 row carries Juneteenth on the 20th of June.
   */
  private val data_usny: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    // http://www.cs.ny.gov/attendance_leave/2012_legal_holidays.cfm
    (2008, mds(2008, md(1, 1), md(1, 21), md(2, 18), md(5, 26), md(7, 4),
      md(9, 1), md(10, 13), md(11, 11), md(11, 27), md(12, 25))),
    (2009, mds(2009, md(1, 1), md(1, 19), md(2, 16), md(5, 25), md(7, 4),
      md(9, 7), md(10, 12), md(11, 11), md(11, 26), md(12, 25))),
    (2010, mds(2010, md(1, 1), md(1, 18), md(2, 15), md(5, 31), md(7, 5),
      md(9, 6), md(10, 11), md(11, 11), md(11, 25), md(12, 25))),
    (2011, mds(2011, md(1, 1), md(1, 17), md(2, 21), md(5, 30), md(7, 4),
      md(9, 5), md(10, 10), md(11, 11), md(11, 24), md(12, 26))),
    (2012, mds(2012, md(1, 2), md(1, 16), md(2, 20), md(5, 28), md(7, 4),
      md(9, 3), md(10, 8), md(11, 12), md(11, 22), md(12, 25))),
    (2013, mds(2013, md(1, 1), md(1, 21), md(2, 18), md(5, 27), md(7, 4),
      md(9, 2), md(10, 14), md(11, 11), md(11, 28), md(12, 25))),
    (2014, mds(2014, md(1, 1), md(1, 20), md(2, 17), md(5, 26), md(7, 4),
      md(9, 1), md(10, 13), md(11, 11), md(11, 27), md(12, 25))),
    (2015, mds(2015, md(1, 1), md(1, 19), md(2, 16), md(5, 25), md(7, 4),
      md(9, 7), md(10, 12), md(11, 11), md(11, 26), md(12, 25))),
    (2021, mds(2021, md(1, 1), md(1, 18), md(2, 15), md(5, 31), md(7, 5),
      md(9, 6), md(10, 11), md(11, 11), md(11, 25), md(12, 25))),
    (2022, mds(2022, md(1, 1), md(1, 17), md(2, 21), md(5, 30), md(6, 20), md(7, 4),
      md(9, 5), md(10, 10), md(11, 11), md(11, 24), md(12, 26))))

  test("test_usny") {
    forAll(data_usny) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(USNY, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val NYFD: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateNewYorkFed()

  /**
   * The Fed drops a holiday that falls on a Saturday rather than moving it, which is why the 2004
   * and 2010 rows have no Christmas Day and the 2009 and 2015 rows no Independence Day.
   */
  private val data_nyfd: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    // http://www.ny.frb.org/aboutthefed/holiday_schedule.html
    // http://web.archive.org/web/20080403230805/http://www.ny.frb.org/aboutthefed/holiday_schedule.html
    // http://web.archive.org/web/20100827003740/http://www.ny.frb.org/aboutthefed/holiday_schedule.html
    // http://web.archive.org/web/20031007222458/http://www.ny.frb.org/aboutthefed/holiday_schedule.html
    // http://www.federalreserve.gov/aboutthefed/k8.htm
    (2003, mds(2003, md(1, 1), md(1, 20), md(2, 17), md(5, 26), md(7, 4),
      md(9, 1), md(10, 13), md(11, 11), md(11, 27), md(12, 25))),
    (2004, mds(2004, md(1, 1), md(1, 19), md(2, 16), md(5, 31), md(7, 5),
      md(9, 6), md(10, 11), md(11, 11), md(11, 25))),
    (2005, mds(2005, md(1, 17), md(2, 21), md(5, 30), md(7, 4),
      md(9, 5), md(10, 10), md(11, 11), md(11, 24), md(12, 26))),
    (2006, mds(2006, md(1, 2), md(1, 16), md(2, 20), md(5, 29), md(7, 4),
      md(9, 4), md(10, 9), md(11, 23), md(12, 25))),
    (2007, mds(2007, md(1, 1), md(1, 15), md(2, 19), md(5, 28), md(7, 4),
      md(9, 3), md(10, 8), md(11, 12), md(11, 22), md(12, 25))),
    (2008, mds(2008, md(1, 1), md(1, 21), md(2, 18), md(5, 26), md(7, 4),
      md(9, 1), md(10, 13), md(11, 11), md(11, 27), md(12, 25))),
    (2009, mds(2009, md(1, 1), md(1, 19), md(2, 16), md(5, 25),
      md(9, 7), md(10, 12), md(11, 11), md(11, 26), md(12, 25))),
    (2010, mds(2010, md(1, 1), md(1, 18), md(2, 15), md(5, 31), md(7, 5),
      md(9, 6), md(10, 11), md(11, 11), md(11, 25))),
    (2011, mds(2011, md(1, 17), md(2, 21), md(5, 30), md(7, 4),
      md(9, 5), md(10, 10), md(11, 11), md(11, 24), md(12, 26))),
    (2012, mds(2012, md(1, 2), md(1, 16), md(2, 20), md(5, 28), md(7, 4),
      md(9, 3), md(10, 8), md(11, 12), md(11, 22), md(12, 25))),
    (2013, mds(2013, md(1, 1), md(1, 21), md(2, 18), md(5, 27), md(7, 4),
      md(9, 2), md(10, 14), md(11, 11), md(11, 28), md(12, 25))),
    (2014, mds(2014, md(1, 1), md(1, 20), md(2, 17), md(5, 26), md(7, 4),
      md(9, 1), md(10, 13), md(11, 11), md(11, 27), md(12, 25))),
    (2015, mds(2015, md(1, 1), md(1, 19), md(2, 16), md(5, 25),
      md(9, 7), md(10, 12), md(11, 11), md(11, 26), md(12, 25))),
    (2016, mds(2016, md(1, 1), md(1, 18), md(2, 15), md(5, 30), md(7, 4),
      md(9, 5), md(10, 10), md(11, 11), md(11, 24), md(12, 26))),
    (2017, mds(2017, md(1, 2), md(1, 16), md(2, 20), md(5, 29), md(7, 4),
      md(9, 4), md(10, 9), md(11, 23), md(12, 25))),
    (2018, mds(2018, md(1, 1), md(1, 15), md(2, 19), md(5, 28), md(7, 4),
      md(9, 3), md(10, 8), md(11, 12), md(11, 22), md(12, 25))))

  test("test_nyfd") {
    forAll(data_nyfd) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(NYFD, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val NYSE: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateNewYorkStockExchange()

  /**
   * The exchange observes Good Friday but neither Columbus Day nor Veterans Day; the 2012 row
   * carries the 30th of October, the Hurricane Sandy closure.
   */
  private val data_nyse: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    // https://www.nyse.com/markets/hours-calendars
    // http://web.archive.org/web/20110320011340/http://www.nyse.com/about/newsevents/1176373643795.html?sa_campaign=/internal_ads/homepage/08262008holidays
    // http://web.archive.org/web/20080901164729/http://www.nyse.com/about/newsevents/1176373643795.html?sa_campaign=/internal_ads/homepage/08262008holidays
    (2008, mds(2008, md(1, 1), md(1, 21), md(2, 18), md(3, 21), md(5, 26), md(7, 4),
      md(9, 1), md(11, 27), md(12, 25))),
    (2009, mds(2009, md(1, 1), md(1, 19), md(2, 16), md(4, 10), md(5, 25), md(7, 3),
      md(9, 7), md(11, 26), md(12, 25))),
    (2010, mds(2010, md(1, 1), md(1, 18), md(2, 15), md(4, 2), md(5, 31), md(7, 5),
      md(9, 6), md(11, 25), md(12, 24))),
    (2011, mds(2011, md(1, 1), md(1, 17), md(2, 21), md(4, 22), md(5, 30), md(7, 4),
      md(9, 5), md(11, 24), md(12, 26))),
    (2012, mds(2012, md(1, 2), md(1, 16), md(2, 20), md(4, 6), md(5, 28), md(7, 4),
      md(9, 3), md(10, 30), md(11, 22), md(12, 25))),
    (2013, mds(2013, md(1, 1), md(1, 21), md(2, 18), md(3, 29), md(5, 27), md(7, 4),
      md(9, 2), md(11, 28), md(12, 25))),
    (2014, mds(2014, md(1, 1), md(1, 20), md(2, 17), md(4, 18), md(5, 26), md(7, 4),
      md(9, 1), md(11, 27), md(12, 25))),
    (2015, mds(2015, md(1, 1), md(1, 19), md(2, 16), md(4, 3), md(5, 25), md(7, 3),
      md(9, 7), md(11, 26), md(12, 25))))

  test("test_nyse") {
    forAll(data_nyse) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(NYSE, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val JPTO: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateTokyo()

  /**
   * Japan has two rules no other calendar here has: a holiday that falls on a Sunday is observed on
   * the following Monday, and a day between two holidays becomes a citizens' holiday itself - the
   * source of the 22nd of September 2015, between Respect for the Aged Day and the autumn equinox.
   * The one-off moves are here too: the 2019 enthronement year with its extended Golden Week and its
   * 22nd of October, and the Olympic moves of the marine, mountain and sports days.
   */
  private val data_jpto: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    // https://www.boj.or.jp/en/about/outline/holi.htm/
    // http://web.archive.org/web/20110513190217/http://www.boj.or.jp/en/about/outline/holi.htm/
    // https://www.japanspecialist.co.uk/travel-tips/national-holidays-in-japan/
    (1999, mds(1999, md(1, 1), md(1, 2), md(1, 3), md(1, 15), md(2, 11), md(3, 22), md(4, 29), md(5, 3), md(5, 4), md(5, 5),
      md(7, 20), md(9, 15), md(9, 23), md(10, 11), md(11, 3), md(11, 23), md(12, 23), md(12, 31))),
    (2000, mds(2000, md(1, 1), md(1, 2), md(1, 3), md(1, 10), md(2, 11), md(3, 20), md(4, 29), md(5, 3), md(5, 4), md(5, 5),
      md(7, 20), md(9, 15), md(9, 23), md(10, 9), md(11, 3), md(11, 23), md(12, 23), md(12, 31))),
    (2001, mds(2001, md(1, 1), md(1, 2), md(1, 3), md(1, 8), md(2, 12), md(3, 20), md(4, 30), md(5, 3), md(5, 4), md(5, 5),
      md(7, 20), md(9, 15), md(9, 24), md(10, 8), md(11, 3), md(11, 23), md(12, 24), md(12, 31))),
    (2002, mds(2002, md(1, 1), md(1, 2), md(1, 3), md(1, 14), md(2, 11), md(3, 21), md(4, 29), md(5, 3), md(5, 4), md(5, 6),
      md(7, 20), md(9, 16), md(9, 23), md(10, 14), md(11, 4), md(11, 23), md(12, 23), md(12, 31))),
    (2003, mds(2003, md(1, 1), md(1, 2), md(1, 3), md(1, 13), md(2, 11), md(3, 21), md(4, 29), md(5, 3), md(5, 4), md(5, 5),
      md(7, 21), md(9, 15), md(9, 23), md(10, 13), md(11, 3), md(11, 24), md(12, 23), md(12, 31))),
    (2004, mds(2004, md(1, 1), md(1, 2), md(1, 3), md(1, 12), md(2, 11), md(3, 20), md(4, 29), md(5, 3), md(5, 4), md(5, 5),
      md(7, 19), md(9, 20), md(9, 23), md(10, 11), md(11, 3), md(11, 23), md(12, 23), md(12, 31))),
    (2005, mds(2005, md(1, 1), md(1, 2), md(1, 3), md(1, 10), md(2, 11), md(3, 21), md(4, 29), md(5, 3), md(5, 4), md(5, 5),
      md(7, 18), md(9, 19), md(9, 23), md(10, 10), md(11, 3), md(11, 23), md(12, 23), md(12, 31))),
    (2006, mds(2006, md(1, 1), md(1, 2), md(1, 3), md(1, 9), md(2, 11), md(3, 21), md(4, 29), md(5, 3), md(5, 4), md(5, 5),
      md(7, 17), md(9, 18), md(9, 23), md(10, 9), md(11, 3), md(11, 23), md(12, 23), md(12, 31))),
    (2011, mds(2011, md(1, 1), md(1, 2), md(1, 3), md(1, 10), md(2, 11), md(3, 21), md(4, 29), md(5, 3), md(5, 4), md(5, 5),
      md(7, 18), md(9, 19), md(9, 23), md(10, 10), md(11, 3), md(11, 23), md(12, 23), md(12, 31))),
    (2012, mds(2012, md(1, 1), md(1, 2), md(1, 3), md(1, 9), md(2, 11), md(3, 20), md(4, 30), md(5, 3), md(5, 4), md(5, 5),
      md(7, 16), md(9, 17), md(9, 22), md(10, 8), md(11, 3), md(11, 23), md(12, 24), md(12, 31))),
    (2013, mds(2013, md(1, 1), md(1, 2), md(1, 3), md(1, 14), md(2, 11), md(3, 20), md(4, 29),
      md(5, 3), md(5, 4), md(5, 5), md(5, 6),
      md(7, 15), md(9, 16), md(9, 23), md(10, 14), md(11, 4), md(11, 23), md(12, 23), md(12, 31))),
    (2014, mds(2014, md(1, 1), md(1, 2), md(1, 3), md(1, 13), md(2, 11), md(3, 21), md(4, 29),
      md(5, 3), md(5, 4), md(5, 5), md(5, 6),
      md(7, 21), md(9, 15), md(9, 23), md(10, 13), md(11, 3), md(11, 24), md(12, 23), md(12, 31))),
    (2015, mds(2015, md(1, 1), md(1, 2), md(1, 3), md(1, 12), md(2, 11), md(3, 21), md(4, 29),
      md(5, 3), md(5, 4), md(5, 5), md(5, 6),
      md(7, 20), md(9, 21), md(9, 22), md(9, 23), md(10, 12), md(11, 3), md(11, 23), md(12, 23), md(12, 31))),
    (2018, mds(2018, md(1, 1), md(1, 2), md(1, 3), md(1, 8), md(2, 12), md(3, 21), md(4, 30),
      md(5, 3), md(5, 4), md(5, 5), md(7, 16), md(8, 11), md(9, 17), md(9, 24),
      md(10, 8), md(11, 3), md(11, 23), md(12, 23), md(12, 24), md(12, 31))),
    (2019, mds(2019, md(1, 1), md(1, 2), md(1, 3), md(1, 14), md(2, 11), md(3, 21), md(4, 29), md(4, 30),
      md(5, 1), md(5, 2), md(5, 3), md(5, 4), md(5, 5), md(5, 6), md(7, 15), md(8, 12), md(9, 16), md(9, 23),
      md(10, 14), md(10, 22), md(11, 4), md(11, 23), md(12, 31))),
    (2020, mds(2020, md(1, 1), md(1, 2), md(1, 3), md(1, 13), md(2, 11), md(2, 24), md(3, 20), md(4, 29),
      md(5, 3), md(5, 4), md(5, 5), md(5, 6), md(7, 23), md(7, 24), md(8, 10), md(9, 21), md(9, 22),
      md(11, 3), md(11, 23), md(12, 31))),
    (2021, mds(2021, md(1, 1), md(1, 11), md(2, 11), md(2, 23), md(3, 20), md(4, 29),
      md(5, 3), md(5, 4), md(5, 5), md(7, 22), md(7, 23), md(8, 9), md(9, 20),
      md(9, 23), md(11, 3), md(11, 23), md(12, 31))))

  test("test_jpto") {
    forAll(data_jpto) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(JPTO, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val AUSY: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateSydney()

  /**
   * New South Wales observes the Easter weekend in full - Good Friday, Easter Saturday, Easter Sunday
   * and Easter Monday - which is why rows such as 2014 list four consecutive days including weekend
   * days. The 2022 row carries the national day of mourning on the 22nd of September, the 2026 row
   * Anzac Day observed on Monday the 27th of April.
   */
  private val data_ausy: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    (2012, mds(2012, md(1, 1), md(1, 2), md(1, 26), md(4, 6), md(4, 7), md(4, 8), md(4, 9),
      md(4, 25), md(6, 11), md(8, 6), md(10, 1), md(12, 25), md(12, 26))),
    (2013, mds(2013, md(1, 1), md(1, 26), md(1, 28), md(3, 29), md(3, 30), md(3, 31), md(4, 1),
      md(4, 25), md(6, 10), md(8, 5), md(10, 7), md(12, 25), md(12, 26))),
    (2014, mds(2014, md(1, 1), md(1, 26), md(1, 27), md(4, 18), md(4, 19), md(4, 20), md(4, 21),
      md(4, 25), md(6, 9), md(8, 4), md(10, 6), md(12, 25), md(12, 26))),
    (2015, mds(2015, md(1, 1), md(1, 26), md(4, 3), md(4, 4), md(4, 5), md(4, 6), md(4, 25),
      md(6, 8), md(8, 3), md(10, 5), md(12, 25), md(12, 26), md(12, 27), md(12, 28))),
    (2016, mds(2016, md(1, 1), md(1, 26), md(3, 25), md(3, 26), md(3, 27), md(3, 28),
      md(4, 25), md(6, 13), md(8, 1), md(10, 3), md(12, 25), md(12, 26), md(12, 27))),
    (2017, mds(2017, md(1, 1), md(1, 2), md(1, 26), md(4, 14), md(4, 15), md(4, 16), md(4, 17),
      md(4, 25), md(6, 12), md(8, 7), md(10, 2), md(12, 25), md(12, 26))),
    (2022, mds(2022, md(1, 3), md(1, 26), md(4, 15), md(4, 18),
      md(4, 25), md(6, 13), md(8, 1), md(9, 22), md(10, 3), md(12, 26), md(12, 27))),
    (2026, mds(2026, md(1, 1), md(1, 26), md(4, 3), md(4, 6),
      md(4, 27), md(6, 8), md(8, 3), md(10, 5), md(12, 25), md(12, 28))))

  test("test_ausy") {
    forAll(data_ausy) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(AUSY, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val BRBD: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateBrazil()

  /**
   * Carnival is the pair of days before Ash Wednesday, forty-eight and forty-seven days before Easter,
   * which is why each row opens with two consecutive February or March dates. The 2024 row carries
   * Black Consciousness Day on the 20th of November, which applies from 2024 onwards.
   */
  private val data_brbd: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    // http://www.planalto.gov.br/ccivil_03/leis/2002/L10607.htm
    (2013, mds(2013, md(1, 1), md(2, 11), md(2, 12), md(3, 29), md(4, 21), md(5, 1),
      md(5, 30), md(9, 7), md(10, 12), md(11, 2), md(11, 15), md(12, 25))),
    (2014, mds(2014, md(1, 1), md(3, 3), md(3, 4), md(4, 18), md(4, 21), md(5, 1),
      md(6, 19), md(9, 7), md(10, 12), md(11, 2), md(11, 15), md(12, 25))),
    (2015, mds(2015, md(1, 1), md(2, 16), md(2, 17), md(4, 3), md(4, 21), md(5, 1),
      md(6, 4), md(9, 7), md(10, 12), md(11, 2), md(11, 15), md(12, 25))),
    (2016, mds(2016, md(1, 1), md(2, 8), md(2, 9), md(3, 25), md(4, 21), md(5, 1),
      md(5, 26), md(9, 7), md(10, 12), md(11, 2), md(11, 15), md(12, 25))),
    (2024, mds(2024, md(1, 1), md(2, 12), md(2, 13), md(3, 29), md(4, 21), md(5, 1),
      md(5, 30), md(9, 7), md(10, 12), md(11, 2), md(11, 15), md(11, 20), md(12, 25))))

  test("test_brbd") {
    forAll(data_brbd) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(BRBD, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val CAMO: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateMontreal()

  /**
   * Quebec observes the National Holiday on the 24th of June and Good Friday rather than Easter
   * Monday; the 2022 row carries Truth and Reconciliation Day on the 30th of September.
   */
  private val data_camo: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    // https://www.bankofcanada.ca/about/contact-information/bank-of-canada-holiday-schedule/
    // also indicate day after new year and boxing day, but no other sources for this
    (2017, mds(2017, md(1, 2), md(4, 14),
      md(5, 22), md(6, 26), md(7, 3), md(9, 4), md(10, 9), md(12, 25))),
    (2018, mds(2018, md(1, 1), md(3, 30),
      md(5, 21), md(6, 25), md(7, 2), md(9, 3), md(10, 8), md(12, 25))),
    (2022, mds(2022, md(1, 3), md(4, 15),
      md(5, 23), md(6, 24), md(7, 1), md(9, 5), md(9, 30), md(10, 10), md(12, 26))))

  test("test_camo") {
    forAll(data_camo) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(CAMO, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val CATO: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateToronto()

  /**
   * Ontario observes Family Day, Victoria Day, the Civic Holiday and Remembrance Day, and moves
   * Christmas Day and Boxing Day off the weekend as a pair, which is why the December dates differ
   * from row to row rather than staying on the 25th and 26th. The 2025 row carries Truth and
   * Reconciliation Day on the 30th of September.
   */
  private val data_cato: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    (2009, mds(2009, md(1, 1), md(2, 16), md(4, 10),
      md(5, 18), md(7, 1), md(8, 3), md(9, 7), md(10, 12), md(11, 11), md(12, 25), md(12, 28))),
    (2010, mds(2010, md(1, 1), md(2, 15), md(4, 2),
      md(5, 24), md(7, 1), md(8, 2), md(9, 6), md(10, 11), md(11, 11), md(12, 27), md(12, 28))),
    (2011, mds(2011, md(1, 3), md(2, 21), md(4, 22),
      md(5, 23), md(7, 1), md(8, 1), md(9, 5), md(10, 10), md(11, 11), md(12, 26), md(12, 27))),
    (2012, mds(2012, md(1, 2), md(2, 20), md(4, 6),
      md(5, 21), md(7, 2), md(8, 6), md(9, 3), md(10, 8), md(11, 12), md(12, 25), md(12, 26))),
    (2013, mds(2013, md(1, 1), md(2, 18), md(3, 29),
      md(5, 20), md(7, 1), md(8, 5), md(9, 2), md(10, 14), md(11, 11), md(12, 25), md(12, 26))),
    (2014, mds(2014, md(1, 1), md(2, 17), md(4, 18),
      md(5, 19), md(7, 1), md(8, 4), md(9, 1), md(10, 13), md(11, 11), md(12, 25), md(12, 26))),
    (2015, mds(2015, md(1, 1), md(2, 16), md(4, 3),
      md(5, 18), md(7, 1), md(8, 3), md(9, 7), md(10, 12), md(11, 11), md(12, 25), md(12, 28))),
    (2016, mds(2016, md(1, 1), md(2, 15), md(3, 25),
      md(5, 23), md(7, 1), md(8, 1), md(9, 5), md(10, 10), md(11, 11), md(12, 26), md(12, 27))),
    (2025, mds(2025, md(1, 1), md(2, 17), md(4, 18), md(5, 19),
      md(7, 1), md(8, 4), md(9, 1), md(9, 30), md(10, 13), md(11, 11), md(12, 25), md(12, 26))))

  test("test_cato") {
    forAll(data_cato) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(CATO, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val CZPR: ImmutableHolidayCalendar = GlobalHolidayCalendars.generatePrague()

  /**
   * The Czech holidays are fixed dates that are not moved off the weekend, plus Easter Monday
   * throughout and Good Friday from 2016 - which the 2016 and 2017 rows carry and the rest do not.
   */
  private val data_czpr: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    // official data from Czech National Bank
    // https://www.cnb.cz/en/public/media_service/schedules/media_svatky.html
    (2008, mds(2008, md(1, 1), md(3, 24), md(5, 1), md(5, 8),
      md(7, 5), md(7, 6), md(9, 28), md(10, 28), md(11, 17), md(12, 24), md(12, 25), md(12, 26))),
    (2009, mds(2009, md(1, 1), md(4, 13), md(5, 1), md(5, 8),
      md(7, 5), md(7, 6), md(9, 28), md(10, 28), md(11, 17), md(12, 24), md(12, 25), md(12, 26))),
    (2010, mds(2010, md(1, 1), md(4, 5), md(5, 1), md(5, 8),
      md(7, 5), md(7, 6), md(9, 28), md(10, 28), md(11, 17), md(12, 24), md(12, 25), md(12, 26))),
    (2011, mds(2011, md(1, 1), md(4, 25), md(5, 1), md(5, 8),
      md(7, 5), md(7, 6), md(9, 28), md(10, 28), md(11, 17), md(12, 24), md(12, 25), md(12, 26))),
    (2012, mds(2012, md(1, 1), md(4, 9), md(5, 1), md(5, 8),
      md(7, 5), md(7, 6), md(9, 28), md(10, 28), md(11, 17), md(12, 24), md(12, 25), md(12, 26))),
    (2013, mds(2013, md(1, 1), md(4, 1), md(5, 1), md(5, 8),
      md(7, 5), md(7, 6), md(9, 28), md(10, 28), md(11, 17), md(12, 24), md(12, 25), md(12, 26))),
    (2014, mds(2014, md(1, 1), md(4, 21), md(5, 1), md(5, 8),
      md(7, 5), md(7, 6), md(9, 28), md(10, 28), md(11, 17), md(12, 24), md(12, 25), md(12, 26))),
    (2015, mds(2015, md(1, 1), md(4, 6), md(5, 1), md(5, 8),
      md(7, 5), md(7, 6), md(9, 28), md(10, 28), md(11, 17), md(12, 24), md(12, 25), md(12, 26))),
    (2016, mds(2016, md(1, 1), md(3, 25), md(3, 28), md(5, 1), md(5, 8),
      md(7, 5), md(7, 6), md(9, 28), md(10, 28), md(11, 17), md(12, 24), md(12, 25), md(12, 26))),
    (2017, mds(2017, md(1, 1), md(4, 14), md(4, 17), md(5, 1), md(5, 8),
      md(7, 5), md(7, 6), md(9, 28), md(10, 28), md(11, 17), md(12, 24), md(12, 25), md(12, 26))))

  test("test_czpr") {
    forAll(data_czpr) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(CZPR, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val DKCO: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateCopenhagen()

  /**
   * Denmark observes Maundy Thursday, Great Prayer Day - the fourth Friday after Easter - and the day
   * after Ascension, none of which appears in any other calendar here, which is why each row carries
   * spring dates that look unrelated to Easter until they are counted from it.
   */
  private val data_dkco: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    // official data from Danish Bankers association via web archive
    (2013, mds(2013, md(1, 1), md(3, 28), md(3, 29), md(4, 1),
      md(4, 26), md(5, 9), md(5, 10), md(5, 20), md(6, 5), md(12, 24), md(12, 25), md(12, 26), md(12, 31))),
    (2014, mds(2014, md(1, 1), md(4, 17), md(4, 18), md(4, 21),
      md(5, 16), md(5, 29), md(5, 30), md(6, 5), md(6, 9), md(12, 24), md(12, 25), md(12, 26), md(12, 31))),
    (2015, mds(2015, md(1, 1), md(4, 2), md(4, 3), md(4, 6),
      md(5, 1), md(5, 14), md(5, 15), md(5, 25), md(6, 5), md(12, 24), md(12, 25), md(12, 26), md(12, 31))),
    (2016, mds(2016, md(1, 1), md(3, 24), md(3, 25), md(3, 28),
      md(4, 22), md(5, 5), md(5, 6), md(5, 16), md(6, 5), md(12, 24), md(12, 25), md(12, 26), md(12, 31))))

  test("test_dkco") {
    forAll(data_dkco) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(DKCO, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val HUBU: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateBudapest()

  /**
   * The Budapest expectations need a third column: the second lists what the calendar must close, the
   * third the compensating working Saturdays it must open in spite of the second column and the
   * weekend. Which Saturday is worked is set by decree each year, so it cannot be derived.
   */
  private val data_hubu: TableFor3[Int, List[LocalDate], List[LocalDate]] = Table(
    ("year", "holidays", "workDays"),
    // http://www.mnb.hu/letoltes/bubor2.xls
    // http://holidays.kayaposoft.com/public_holidays.php?year=2013&country=hun&region=#
    (
      2012,
      mds(2012, md(3, 15), md(3, 16), md(4, 9), md(4, 30), md(5, 1), md(5, 28),
        md(8, 20), md(10, 22), md(10, 23), md(11, 1), md(11, 2), md(12, 24), md(12, 25), md(12, 26), md(12, 31)),
      List(date(2012, 3, 24), date(2012, 5, 5), date(2012, 10, 27),
        date(2012, 11, 10), date(2012, 12, 15), date(2012, 12, 29))),
    (
      2013,
      mds(2013, md(1, 1), md(3, 15), md(4, 1), md(5, 1), md(5, 20),
        md(8, 19), md(8, 20), md(10, 23), md(11, 1), md(12, 24), md(12, 25), md(12, 26), md(12, 27)),
      List(date(2013, 8, 24), date(2013, 12, 7), date(2013, 12, 21))),
    (
      2014,
      mds(2014, md(1, 1), md(3, 15), md(4, 21), md(5, 1), md(5, 2),
        md(6, 9), md(8, 20), md(10, 23), md(10, 24), md(12, 24), md(12, 25), md(12, 26)),
      List(date(2014, 5, 10), date(2014, 10, 18))),
    (
      2015,
      mds(2015, md(1, 1), md(1, 2), md(3, 15), md(4, 6), md(5, 1), md(5, 25),
        md(8, 20), md(8, 21), md(10, 23), md(12, 24), md(12, 25), md(12, 26)),
      List(date(2015, 1, 10), date(2015, 8, 8), date(2015, 12, 12))),
    (
      2016,
      mds(2016, md(1, 1), md(3, 14), md(3, 15), md(3, 28), md(5, 1), md(5, 16),
        md(10, 31), md(11, 1), md(12, 24), md(12, 25), md(12, 26)),
      List(date(2016, 3, 5), date(2016, 10, 15))),
    (
      2020,
      mds(2020, md(1, 1), md(3, 15), md(4, 10), md(4, 13), md(5, 1), md(6, 1),
        md(8, 20), md(8, 21), md(10, 23), md(12, 24), md(12, 25), md(12, 26)),
      List(date(2020, 8, 29), date(2020, 12, 12))))

  test("test_hubu") {
    forAll(data_hubu) { (year: Int, holidays: List[LocalDate], workDays: List[LocalDate]) =>
      assertBudapest(year, holidays, workDays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val MXMC: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateMexicoCity()

  /**
   * Three Mexican holidays are the first Monday of February, the third Monday of March and the third
   * Monday of November, which is why those dates move by a week from row to row while the fixed
   * dates do not. The 2024 row carries the presidential inauguration on the 1st of October.
   */
  private val data_mxmc: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    // http://www.banxico.org.mx/SieInternet/consultarDirectorioInternetAction.do?accion=consultarCuadro&idCuadro=CF111&locale=en
    (2012, mds(2012, md(1, 1), md(2, 6), md(3, 19), md(4, 5), md(4, 6),
      md(5, 1), md(9, 16), md(11, 2), md(11, 19), md(12, 12), md(12, 25))),
    (2013, mds(2013, md(1, 1), md(2, 4), md(3, 18), md(3, 28), md(3, 29),
      md(5, 1), md(9, 16), md(11, 2), md(11, 18), md(12, 12), md(12, 25))),
    (2014, mds(2014, md(1, 1), md(2, 3), md(3, 17), md(4, 17), md(4, 18),
      md(5, 1), md(9, 16), md(11, 2), md(11, 17), md(12, 12), md(12, 25))),
    (2015, mds(2015, md(1, 1), md(2, 2), md(3, 16), md(4, 2), md(4, 3),
      md(5, 1), md(9, 16), md(11, 2), md(11, 16), md(12, 12), md(12, 25))),
    (2016, mds(2016, md(1, 1), md(2, 1), md(3, 21), md(3, 24), md(3, 25),
      md(5, 1), md(9, 16), md(11, 2), md(11, 21), md(12, 12), md(12, 25))),
    (2024, mds(2024, md(1, 1), md(2, 5), md(3, 18), md(3, 28), md(3, 29),
      md(5, 1), md(9, 16), md(10, 1), md(11, 2), md(11, 18), md(12, 12), md(12, 25))))

  test("test_mxmc") {
    forAll(data_mxmc) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(MXMC, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val NOOS: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateOslo()

  /**
   * Norway does not move a holiday off the weekend, so the 2011, 2016 and 2017 rows are the
   * interesting ones: New Year's Day, Labour Day, Constitution Day or Christmas Eve fall at a weekend
   * in those years, simply do not appear, and get no substitute.
   */
  private val data_noos: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    // official data from Oslo Bors via web archive
    (2009, mds(2009, md(1, 1), md(4, 9), md(4, 10), md(4, 13),
      md(5, 1), md(5, 21), md(6, 1), md(12, 24), md(12, 25), md(12, 31))),
    (2011, mds(2011, md(4, 21), md(4, 22), md(4, 25),
      md(5, 17), md(6, 2), md(6, 13), md(12, 26))),
    (2012, mds(2012, md(4, 5), md(4, 6), md(4, 9),
      md(5, 1), md(5, 17), md(5, 28), md(12, 24), md(12, 25), md(12, 26), md(12, 31))),
    (2013, mds(2013, md(1, 1), md(3, 28), md(3, 29), md(4, 1),
      md(5, 1), md(5, 9), md(5, 17), md(5, 20), md(12, 24), md(12, 25), md(12, 26), md(12, 31))),
    (2014, mds(2014, md(1, 1), md(4, 17), md(4, 18), md(4, 21),
      md(5, 1), md(5, 17), md(5, 29), md(6, 9), md(12, 24), md(12, 25), md(12, 26), md(12, 31))),
    (2015, mds(2015, md(1, 1), md(4, 2), md(4, 3), md(4, 6),
      md(5, 1), md(5, 14), md(5, 25), md(12, 24), md(12, 25), md(12, 31))),
    (2016, mds(2016, md(1, 1), md(3, 24), md(3, 25), md(3, 28),
      md(5, 5), md(5, 16), md(5, 17), md(12, 26))),
    (2017, mds(2017, md(4, 13), md(4, 14), md(4, 17),
      md(5, 1), md(5, 17), md(5, 25), md(6, 5), md(12, 25), md(12, 26))))

  test("test_noos") {
    forAll(data_noos) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(NOOS, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val NZAU: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateAuckland()

  /**
   * New Zealand moves a public holiday that falls at a weekend to the following Monday, or to the
   * Tuesday where the Monday is already taken, and adds the province's anniversary day - for Auckland
   * the Monday nearest the 29th of January, the only difference from Wellington's rows.
   */
  private val data_nzau: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    // https://www.govt.nz/browse/work/public-holidays-and-work/public-holidays-and-anniversary-dates/
    // https://www.employment.govt.nz/leave-and-holidays/public-holidays/public-holidays-and-anniversary-dates/dates-for-previous-years/
    (2015, mds(2015, md(1, 1), md(1, 2), md(1, 26), md(2, 6), md(4, 3), md(4, 6),
      md(4, 27), md(6, 1), md(10, 26), md(12, 25), md(12, 28))),
    (2016, mds(2016, md(1, 1), md(1, 4), md(2, 1), md(2, 8), md(3, 25), md(3, 28),
      md(4, 25), md(6, 6), md(10, 24), md(12, 26), md(12, 27))),
    (2017, mds(2017, md(1, 2), md(1, 3), md(1, 30), md(2, 6), md(4, 14), md(4, 17),
      md(4, 25), md(6, 5), md(10, 23), md(12, 25), md(12, 26))),
    (2018, mds(2018, md(1, 1), md(1, 2), md(1, 29), md(2, 6), md(3, 30), md(4, 2),
      md(4, 25), md(6, 4), md(10, 22), md(12, 25), md(12, 26))))

  test("test_nzau") {
    forAll(data_nzau) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(NZAU, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val NZWE: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateWellington()

  /** Wellington's anniversary day is the Monday nearest the 22nd of January, unlike Auckland's. */
  private val data_nzwe: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    // https://www.govt.nz/browse/work/public-holidays-and-work/public-holidays-and-anniversary-dates/
    // https://www.employment.govt.nz/leave-and-holidays/public-holidays/public-holidays-and-anniversary-dates/dates-for-previous-years/
    (2015, mds(2015, md(1, 1), md(1, 2), md(1, 19), md(2, 6), md(4, 3), md(4, 6),
      md(4, 27), md(6, 1), md(10, 26), md(12, 25), md(12, 28))),
    (2016, mds(2016, md(1, 1), md(1, 4), md(1, 25), md(2, 8), md(3, 25), md(3, 28),
      md(4, 25), md(6, 6), md(10, 24), md(12, 26), md(12, 27))),
    (2017, mds(2017, md(1, 2), md(1, 3), md(1, 23), md(2, 6), md(4, 14), md(4, 17),
      md(4, 25), md(6, 5), md(10, 23), md(12, 25), md(12, 26))),
    (2018, mds(2018, md(1, 1), md(1, 2), md(1, 22), md(2, 6), md(3, 30), md(4, 2),
      md(4, 25), md(6, 4), md(10, 22), md(12, 25), md(12, 26))))

  test("test_nzwe") {
    forAll(data_nzwe) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(NZWE, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  /** `NZBD` has no `HolidayCalendarIds` constant, so it is reached through its generator alone. */
  private lazy val NZBD: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateNewZealand()

  /**
   * The public holidays of the whole country, so no provincial anniversary day appears - the only
   * respect in which these rows differ from the provincial calendars above. The 2025 row carries
   * Matariki on the 20th of June.
   */
  private val data_nzbd: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    // https://www.govt.nz/browse/work/public-holidays-and-work/public-holidays-and-anniversary-dates/
    // https://www.employment.govt.nz/leave-and-holidays/public-holidays/public-holidays-and-anniversary-dates/dates-for-previous-years/
    (2015, mds(2015, md(1, 1), md(1, 2), md(2, 6), md(4, 3), md(4, 6),
      md(4, 27), md(6, 1), md(10, 26), md(12, 25), md(12, 28))),
    (2016, mds(2016, md(1, 1), md(1, 4), md(2, 8), md(3, 25), md(3, 28),
      md(4, 25), md(6, 6), md(10, 24), md(12, 26), md(12, 27))),
    (2017, mds(2017, md(1, 2), md(1, 3), md(2, 6), md(4, 14), md(4, 17),
      md(4, 25), md(6, 5), md(10, 23), md(12, 25), md(12, 26))),
    (2018, mds(2018, md(1, 1), md(1, 2), md(2, 6), md(3, 30), md(4, 2),
      md(4, 25), md(6, 4), md(10, 22), md(12, 25), md(12, 26))),
    (2025, mds(2025, md(1, 1), md(1, 2), md(2, 6), md(4, 18),
      md(4, 21), md(4, 25), md(6, 2), md(6, 20), md(10, 27), md(12, 25), md(12, 26))))

  test("test_nzbd") {
    forAll(data_nzbd) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(NZBD, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val PLWA: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateWarsaw()

  /**
   * Poland observes Epiphany from 2011 and Corpus Christi on the Thursday sixty days after Easter,
   * and the exchange closes on Christmas Eve every year but on New Year's Eve only when it falls on a
   * Monday, a Thursday or a Friday - hence no 31st of December in the 2013 and 2014 rows. The 2018 row
   * carries the centenary of independence on the 12th of November.
   */
  private val data_plwa: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    // based on government law data and stock exchange holidays
    (2013, mds(2013, md(1, 1), md(4, 1),
      md(5, 1), md(5, 3), md(5, 30), md(8, 15), md(11, 1), md(11, 11), md(12, 24), md(12, 25), md(12, 26))),
    (2014, mds(2014, md(1, 1), md(1, 6), md(4, 21),
      md(5, 1), md(6, 19), md(8, 15), md(11, 11), md(12, 24), md(12, 25), md(12, 26))),
    (2015, mds(2015, md(1, 1), md(1, 6), md(4, 6),
      md(5, 1), md(6, 4), md(11, 11), md(12, 24), md(12, 25), md(12, 31))),
    (2016, mds(2016, md(1, 1), md(1, 6), md(3, 28),
      md(5, 3), md(5, 26), md(8, 15), md(11, 1), md(11, 11), md(12, 26))),
    (2017, mds(2017, md(1, 6), md(4, 17),
      md(5, 1), md(5, 3), md(6, 15), md(8, 15), md(11, 1), md(12, 25), md(12, 26))),
    (2018, mds(2018, md(1, 1), md(1, 6), md(4, 1), md(4, 2), md(5, 1), md(5, 3),
      md(5, 20), md(5, 31), md(8, 15), md(11, 1), md(11, 11), md(11, 12), md(12, 24), md(12, 25), md(12, 26),
      md(12, 31))))

  test("test_plwa") {
    forAll(data_plwa) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(PLWA, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val SEST: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateStockholm()

  /**
   * Sweden observes Midsummer Eve - the Friday between the 19th and the 25th of June - and the fixing
   * calendar closes on Christmas Eve and New Year's Eve, none of which is moved off a weekend, which
   * is why the 2016 row is the shortest of the three.
   */
  private val data_sest: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    // official data from published fixing dates
    (2014, mds(2014, md(1, 1), md(1, 6), md(4, 18), md(4, 21),
      md(5, 1), md(5, 29), md(6, 6), md(6, 20), md(12, 24), md(12, 25), md(12, 26), md(12, 31))),
    (2015, mds(2015, md(1, 1), md(1, 6), md(4, 3), md(4, 6),
      md(5, 1), md(5, 14), md(6, 19), md(12, 24), md(12, 25), md(12, 31))),
    (2016, mds(2016, md(1, 1), md(1, 6), md(3, 25), md(3, 28),
      md(5, 5), md(6, 6), md(6, 24), md(12, 26))))

  test("test_sest") {
    forAll(data_sest) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(SEST, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  private lazy val ZAJO: ImmutableHolidayCalendar = GlobalHolidayCalendars.generateJohannesburg()

  /**
   * South Africa moves a holiday that falls on a Sunday to the following Monday and adds an election
   * day when one is held, which is where the 3rd of August 2016 comes from and why the 2017 row names
   * the 25th of September rather than the 24th. That row lists the 16th of December twice; the
   * duplicate changes nothing, because the expectation is membership of the list.
   */
  private val data_zajo: TableFor2[Int, List[LocalDate]] = Table(
    ("year", "holidays"),
    // http://www.gov.za/about-sa/public-holidays
    // https://web.archive.org/web/20151230214958/http://www.gov.za/about-sa/public-holidays
    (2015, mds(2015, md(1, 1), md(3, 21), md(4, 3), md(4, 6), md(4, 27), md(5, 1),
      md(6, 16), md(8, 10), md(9, 24), md(12, 16), md(12, 25), md(12, 26))),
    (2016, mds(2016, md(1, 1), md(3, 21), md(3, 25), md(3, 28), md(4, 27), md(5, 2),
      md(6, 16), md(8, 3), md(8, 9), md(9, 24), md(12, 16), md(12, 26), md(12, 27))),
    (2017, mds(2017, md(1, 1), md(1, 2), md(3, 21), md(4, 14), md(4, 17), md(4, 27), md(5, 1),
      md(6, 16), md(8, 9), md(9, 25), md(12, 16), md(12, 16), md(12, 25), md(12, 26))))

  test("test_zajo") {
    forAll(data_zajo) { (year: Int, holidays: List[LocalDate]) =>
      assertHolidays(ZAJO, year, holidays)
    }
  }

  //-------------------------------------------------------------------------
  /**
   * Merging two generated calendars keeps every holiday of both, for ninety years of dates.
   * [[ImmutableHolidayCalendar.combined]] merges the stored months of its two arguments rather than
   * reading both on every query, so what it produces has to be checked against the union of the two
   * calendars it came from. Tokyo and New York are a demanding pair: their holidays barely overlap,
   * so almost every holiday of the merged calendar comes from exactly one side.
   */
  test("test_combinedWith") {
    val combined = ImmutableHolidayCalendar.combined(JPTO, USNY)
    Iterator
      .iterate(LocalDate.of(1950, 1, 1))(day => day.plusDays(1))
      .takeWhile(day => day.getYear < 2040)
      .foreach { day =>
        withClue(s"Date: $day: ") {
          combined.isHoliday(day) shouldBe (JPTO.isHoliday(day) || USNY.isHoliday(day))
        }
      }
    succeed
  }

  //-------------------------------------------------------------------------
  /**
   * The Christmas and Boxing Day bumps, pinned across the four ways the pair can fall: no bump,
   * Christmas bumped over a whole weekend, Christmas bumped over a Sunday, and Boxing Day alone
   * bumped. Both helpers are shared by most of the calendars that observe Christmas.
   */
  test("test_christmas") {
    // christmas on Friday
    GlobalHolidayCalendars.christmasBumpedSatSun(2020) shouldBe LocalDate.of(2020, 12, 25)
    GlobalHolidayCalendars.boxingDayBumpedSatSun(2020) shouldBe LocalDate.of(2020, 12, 28)
    // christmas on Saturday
    GlobalHolidayCalendars.christmasBumpedSatSun(2021) shouldBe LocalDate.of(2021, 12, 27)
    GlobalHolidayCalendars.boxingDayBumpedSatSun(2021) shouldBe LocalDate.of(2021, 12, 28)
    // christmas on Sunday
    GlobalHolidayCalendars.christmasBumpedSatSun(2022) shouldBe LocalDate.of(2022, 12, 27)
    GlobalHolidayCalendars.boxingDayBumpedSatSun(2022) shouldBe LocalDate.of(2022, 12, 26)
    // christmas on Monday
    GlobalHolidayCalendars.christmasBumpedSatSun(2023) shouldBe LocalDate.of(2023, 12, 25)
    GlobalHolidayCalendars.boxingDayBumpedSatSun(2023) shouldBe LocalDate.of(2023, 12, 26)
  }
}
