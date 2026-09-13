/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.DayOfWeek
import java.time.DayOfWeek.FRIDAY
import java.time.DayOfWeek.MONDAY
import java.time.DayOfWeek.SATURDAY
import java.time.DayOfWeek.SUNDAY
import java.time.DayOfWeek.THURSDAY
import java.time.DayOfWeek.TUESDAY
import java.time.DayOfWeek.WEDNESDAY
import java.time.LocalDate
import java.time.temporal.TemporalAdjusters.dayOfWeekInMonth
import java.time.temporal.TemporalAdjusters.firstInMonth
import java.time.temporal.TemporalAdjusters.lastInMonth
import java.time.temporal.TemporalAdjusters.nextOrSame
import java.time.temporal.TemporalAdjusters.previous

import scala.collection.immutable.SortedSet

/**
 * The holiday calendars of some of the world's financial centres, expressed as the rules that
 * produce them.
 *
 * A holiday calendar is a set of dates, but writing one down as a set of dates is not how it
 * comes to exist. It comes from legislation - Easter Monday is a holiday, the August bank
 * holiday is the last Monday in August, Christmas moves to the 27th when it falls at a weekend
 * - and the legislation is what this object holds. Each calendar is produced by a function of
 * one integer year that returns that year's holidays, applied across the whole range of years
 * the calendar covers, so a rule appears once rather than once per year and the reason a date
 * is a holiday can be read next to the date itself.
 *
 * The data here was identified through direct research and is not derived from a vendor of
 * holiday calendar data. It may or may not be sufficient for a production need, and it is the
 * caller's responsibility to decide: an application that has its own holidays supplies them as
 * its own reference data, which is exactly what
 * [[com.opengamma.strata.basics.ReferenceData]] exists to let it do.
 *
 * ===Range of years, and what happens outside it===
 *
 * Every calendar but one covers 1950 to 2099 inclusive. The exception is the European TARGET
 * calendar, which covers 1997 to 2099 because TARGET did not exist before 1997, and whose
 * first three years carry only the two days the testing-phase system observed.
 *
 * Outside its range a calendar answers from its weekend alone, because
 * [[ImmutableHolidayCalendar]] derives the range it holds data for from the earliest and
 * latest holiday it was built with. A query about 1949 or 2100 therefore succeeds and reports
 * only Saturday and Sunday, rather than failing.
 *
 * ===Purity and memoisation===
 *
 * Every generator is a pure function of no arguments, and every helper returns the dates it
 * contributes:
 *
 *   - [[usCommon]] and [[newZealand]] return the holidays shared by the calendars of their
 *     region, which each caller combines with the holidays particular to its own calendar;
 *   - [[citizensDay]] takes the holidays the earlier rules established and returns them,
 *     extended where the rule applies, because the rule reads what is already established;
 *   - [[addDateWithHungarianBridging]] returns both the holidays and the working Saturdays a
 *     bridged holiday implies, as a pair;
 *   - [[addHungarianSaturdays]] returns the transformed holidays as the ordered set the
 *     calendar is built from, since it both removes the weekend days and adds the Saturdays
 *     that are not working Saturdays;
 *   - [[removeSatSun]] filters lazily, leaving the dates it keeps uncollected.
 *
 * A generator is called at most once per calendar, because [[StandardHolidayCalendars]] holds
 * the calendar each one produces, so the dates a caller sees cannot fall out of step with the
 * rules that produce them.
 *
 * ===Determinism===
 *
 * Every generator is deterministic and depends on nothing outside its own rules, so two calls
 * produce equal calendars. Each one produces its dates as an iterator and collects them, once,
 * into the sorted set the calendar is built from, so a calendar does not depend on the order
 * its rules happened to be written in, and duplicates - which the rules do produce, New Year's
 * Eve bumped from a Sunday landing on a New Year's Day that is already a holiday, for
 * instance - collapse rather than being carried. Tokyo is the one generator whose
 * years are accumulated into a set as they are computed rather than afterwards, because its
 * citizens' day rule reads the holidays the earlier rules established - see [[citizensDay]].
 *
 * That single ordering is the whole cost of ordering a calendar. Dropping the weekend days does
 * not depend on the order of the dates, so it happens before the set is built rather than after
 * it, and the ordered set is handed to [[ImmutableHolidayCalendar.ofNormalized]], which does not
 * order it again - see [[weekdaysOnly]].
 *
 * @see [[StandardHolidayCalendars]] for the built-in calendars these rules produce
 * @see [[HolidayCalendarData]] for the built-in calendar whose dates are published rather than
 *   derived from rules
 */
private[date] object GlobalHolidayCalendars {

  /**
   * The weekend of every calendar here but Budapest.
   *
   * A single shared set, rather than one built per generator, because the weekend of a calendar
   * is read once when it is built and the set is immutable. Budapest works a Saturday some weeks
   * and so cannot use it - see [[generateBudapest]].
   */
  private val satSunWeekend: Set[DayOfWeek] = Set(SATURDAY, SUNDAY)

  /**
   * Collects dates into the ordered set a calendar is built from, dropping the weekend days.
   *
   * The dates a rule produces are unordered and contain duplicates, so they have to be ordered
   * and deduplicated once; the weekend days have to be dropped, because a holiday falling on a
   * Saturday or a Sunday is not observed. Dropping them does not depend on the order of the
   * dates, so it happens first, lazily, and the ordering is then the only pass over the whole
   * calendar. The result is handed straight to [[ImmutableHolidayCalendar.ofNormalized]], which
   * orders nothing.
   *
   * The ordering applied is [[ImmutableHolidayCalendar.dateOrdering]], the one ordering of dates
   * this package has, which is what makes the result acceptable to that factory.
   *
   * @param dates  the dates, in any order and with any duplicates, including any at a weekend
   * @return the dates that fall on a weekday, sorted and deduplicated
   */
  private def weekdaysOnly(dates: IterableOnce[LocalDate]): SortedSet[LocalDate] =
    ImmutableHolidayCalendar.sortedDates(removeSatSun(dates))

  /**
   * Drops the weekend days from dates that are ordered already.
   *
   * This is [[weekdaysOnly]] for a caller that holds the ordered set rather than the dates it was
   * built from - which is the Tokyo calendar, whose rules accumulate into one because the
   * citizens' day rule reads the holidays the earlier rules established. Filtering a sorted set
   * yields a sorted set, in one pass over it and with no date compared against another, so
   * ordering that calendar's dates a second time is avoided rather than moved: the accumulation
   * its rules require is the only ordering pass it makes.
   *
   * @param dates  the dates, ordered by [[ImmutableHolidayCalendar.dateOrdering]]
   * @return the dates that fall on a weekday, in the same order
   */
  private def weekdaysOf(dates: SortedSet[LocalDate]): SortedSet[LocalDate] =
    dates.filterNot(date => isWeekend(date))

  /**
   * Checks whether a date falls at a Saturday/Sunday weekend.
   *
   * @param date  the date
   * @return true where the date is a Saturday or a Sunday
   */
  private def isWeekend(date: LocalDate): Boolean =
    date.getDayOfWeek == SATURDAY || date.getDayOfWeek == SUNDAY

  /**
   * Builds a calendar whose weekend is Saturday and Sunday from the dates its rules produced.
   *
   * Twenty-four of the twenty-five calendars here end this way, so the weekend, the single
   * ordering pass and the absence of working-day overrides are stated here once rather than
   * twenty-four times. Budapest is the exception and builds its own calendar, because its
   * weekend is Sunday alone.
   *
   * @param id  the identifier of the calendar
   * @param dates  the dates the rules of the calendar produced, in any order, with any
   *   duplicates, and including any that fall at a weekend
   * @return the calendar
   */
  private def satSunCalendar(
      id: HolidayCalendarId,
      dates: IterableOnce[LocalDate]): ImmutableHolidayCalendar =

    ImmutableHolidayCalendar.ofNormalized(id, weekdaysOnly(dates), satSunWeekend, Nil)

  // common law (including before 1871) good friday and christmas day (unadjusted for weekends)
  // from 1871 easter monday, whit monday, first Mon in Aug and boxing day
  // from 1965 to 1970, first in Aug moved to Mon after last Sat in Aug
  // from 1971, whitsun moved to last Mon in May, last Mon in Aug
  // from 1974, added new year
  // from 1978, added first Mon in May
  // 1965, Whitsun, Last Mon Aug - http://hansard.millbanksystems.com/commons/1964/mar/04/staggered-holidays
  // 1966, Whitsun May - http://hansard.millbanksystems.com/commons/1964/mar/04/staggered-holidays
  // 1966, 29th Aug - http://hansard.millbanksystems.com/written_answers/1965/nov/25/august-bank-holiday
  // 1967, 29th May, 28th Aug - http://hansard.millbanksystems.com/written_answers/1965/jun/03/bank-holidays-1967-and-1968
  // 1968, 3rd Jun, 2nd Sep - http://hansard.millbanksystems.com/written_answers/1965/jun/03/bank-holidays-1967-and-1968
  // 1969, 26th May, 1st Sep - http://hansard.millbanksystems.com/written_answers/1967/mar/21/bank-holidays-1969-dates
  // 1970, 25th May, 31st Aug - http://hansard.millbanksystems.com/written_answers/1967/jul/28/bank-holidays
  // 2022, 2nd and 3rd Jun - https://www.gov.uk/government/news/extra-bank-holiday-to-mark-the-queens-platinum-jubilee-in-2022
  // 2022, 19th Sep - https://www.gov.uk/government/news/bank-holiday-announced-for-her-majesty-queen-elizabeth-iis-state-funeral-on-monday-19-september
  /**
   * Generates the London holiday calendar, `GBLO`.
   *
   * @return the calendar of London bank holidays from 1950 to 2099
   */
  def generateLondon(): ImmutableHolidayCalendar =
    satSunCalendar(
      HolidayCalendarIds.GBLO,
      (1950 to 2099).iterator.flatMap(londonYear) ++
        Iterator(
          date(1999, 12, 31), // millennium
          date(2011, 4, 29), // royal wedding
          date(2023, 5, 8))) // king's coronation

  /**
   * Calculates the London bank holidays of one year.
   *
   * @param year  the year
   * @return the holidays of that year, before the weekend days are removed
   */
  private def londonYear(year: Int): List[LocalDate] =
    List.concat(
      // new year
      if (year >= 1974) List(bumpToMon(first(year, 1))) else Nil,
      // easter
      List(easter(year).minusDays(2), easter(year).plusDays(1)),
      // early May
      if (year == 1995 || year == 2020) {
        // ve day
        List(date(year, 5, 8))
      } else if (year >= 1978) {
        List(first(year, 5).`with`(firstInMonth(MONDAY)))
      } else {
        Nil
      },
      // spring
      if (year == 2002) {
        // golden jubilee
        List(date(2002, 6, 3), date(2002, 6, 4))
      } else if (year == 2012) {
        // diamond jubilee
        List(date(2012, 6, 4), date(2012, 6, 5))
      } else if (year == 2022) {
        // platinum jubilee
        List(date(2022, 6, 2), date(2022, 6, 3))
      } else if (year == 1967 || year == 1970) {
        List(first(year, 5).`with`(lastInMonth(MONDAY)))
      } else if (year < 1971) {
        // whitsun
        List(easter(year).plusDays(50))
      } else {
        List(first(year, 5).`with`(lastInMonth(MONDAY)))
      },
      // summer
      if (year < 1965) {
        List(first(year, 8).`with`(firstInMonth(MONDAY)))
      } else if (year < 1971) {
        List(first(year, 8).`with`(lastInMonth(SATURDAY)).plusDays(2))
      } else {
        List(first(year, 8).`with`(lastInMonth(MONDAY)))
      },
      // queen's funeral
      if (year == 2022) List(date(2022, 9, 19)) else Nil,
      // christmas
      List(christmasBumpedSatSun(year), boxingDayBumpedSatSun(year)))

  // http://www.legifrance.gouv.fr/affichCodeArticle.do?idArticle=LEGIARTI000006902611&cidTexte=LEGITEXT000006072050
  // http://jollyday.sourceforge.net/data/fr.html
  // Euronext holidays only New Year, Good Friday, Easter Monday, Labour Day, Christmas Day, Boxing Day
  // New Years Eve is holiday for cash markets and derivatives in 2015
  // https://www.euronext.com/en/holidays-and-hours
  // https://www.euronext.com/en/trading/nyse-euronext-trading-calendar/archives
  // the Monday before a Tuesday holiday, and the Friday after a Thursday holiday, are not holidays here
  /**
   * Generates the Paris holiday calendar, `FRPA`.
   *
   * @return the calendar of Paris bank holidays from 1950 to 2099
   */
  def generateParis(): ImmutableHolidayCalendar =
    satSunCalendar(
      HolidayCalendarIds.FRPA,
      (1950 to 2099).iterator.flatMap(parisYear) ++
        Iterator(date(1999, 12, 31))) // millennium

  /**
   * Calculates the Paris bank holidays of one year.
   *
   * @param year  the year
   * @return the holidays of that year, before the weekend days are removed
   */
  private def parisYear(year: Int): List[LocalDate] =
    List.concat(
      List(
        date(year, 1, 1), // new year
        easter(year).minusDays(2), // good friday
        easter(year).plusDays(1), // easter monday
        date(year, 5, 1), // labour day
        date(year, 5, 8), // victory in europe
        easter(year).plusDays(39)), // ascension day
      if (year <= 2004 || year >= 2008) {
        List(easter(year).plusDays(50)) // whit monday
      } else {
        Nil
      },
      List(
        date(year, 7, 14), // bastille
        date(year, 8, 15), // assumption of mary
        date(year, 11, 1), // all saints
        date(year, 11, 11), // armistice day
        date(year, 12, 25), // christmas day
        date(year, 12, 26))) // saint stephen

  // https://www.feiertagskalender.ch/index.php?geo=3122&klasse=3&jahr=2017&hl=en
  // http://jollyday.sourceforge.net/data/de.html
  // http://en.boerse-frankfurt.de/basics-marketplaces-tradingcalendar2019
  /**
   * Generates the Frankfurt holiday calendar, `DEFR`.
   *
   * @return the calendar of Frankfurt bank holidays from 1950 to 2099
   */
  def generateFrankfurt(): ImmutableHolidayCalendar =
    satSunCalendar(
      HolidayCalendarIds.DEFR,
      (1950 to 2099).iterator.flatMap(frankfurtYear) ++
        Iterator(date(2017, 10, 31))) // reformation day

  /**
   * Calculates the Frankfurt bank holidays of one year.
   *
   * @param year  the year
   * @return the holidays of that year, before the weekend days are removed
   */
  private def frankfurtYear(year: Int): List[LocalDate] =
    List.concat(
      List(
        date(year, 1, 1), // new year
        easter(year).minusDays(2), // good friday
        easter(year).plusDays(1), // easter monday
        date(year, 5, 1), // labour day
        easter(year).plusDays(39), // ascension day
        easter(year).plusDays(50), // whit monday
        easter(year).plusDays(60)), // corpus christi
      if (year >= 2000) {
        List(date(year, 10, 3)) // german unity
      } else {
        Nil
      },
      if (year <= 1994) {
        // Wed before the Sunday that is 2 weeks before first advent, which is 4th Sunday before Christmas
        List(date(year, 12, 25).`with`(previous(SUNDAY)).minusWeeks(6).minusDays(4)) // repentance
      } else {
        Nil
      },
      List(
        date(year, 12, 24), // christmas eve
        date(year, 12, 25), // christmas day
        date(year, 12, 26), // saint stephen
        date(year, 12, 31))) // new year

  // http://jollyday.sourceforge.net/data/ch.html
  // https://github.com/lballabio/quantlib/blob/master/QuantLib/ql/time/calendars/switzerland.cpp
  // http://www.six-swiss-exchange.com/funds/trading/trading_and_settlement_calendar_en.html
  // http://www.six-swiss-exchange.com/swx_messages/online/swx7299e.pdf
  /**
   * Generates the Zurich holiday calendar, `CHZU`.
   *
   * @return the calendar of Zurich bank holidays from 1950 to 2099
   */
  def generateZurich(): ImmutableHolidayCalendar =
    satSunCalendar(
      HolidayCalendarIds.CHZU,
      (1950 to 2099).iterator.flatMap(zurichYear) ++
        Iterator(
          date(1999, 12, 31), // millennium
          date(2000, 1, 3))) // millennium

  /**
   * Calculates the Zurich bank holidays of one year.
   *
   * @param year  the year
   * @return the holidays of that year, before the weekend days are removed
   */
  private def zurichYear(year: Int): List[LocalDate] =
    List(
      date(year, 1, 1), // new year
      date(year, 1, 2), // saint berchtoldstag
      easter(year).minusDays(2), // good friday
      easter(year).plusDays(1), // easter monday
      date(year, 5, 1), // labour day
      easter(year).plusDays(39), // ascension day
      easter(year).plusDays(50), // whit monday
      date(year, 8, 1), // national day
      date(year, 12, 25), // christmas day
      date(year, 12, 26)) // saint stephen

  // 1997 - 1998 (testing phase), Jan 1, christmas day
  // https://www.ecb.europa.eu/pub/pdf/other/tagien.pdf
  // in 1999, Jan 1, christmas day, Dec 26, Dec 31
  // http://www.ecb.europa.eu/press/pr/date/1999/html/pr990715_1.en.html
  // http://www.ecb.europa.eu/press/pr/date/1999/html/pr990331.en.html
  // in 2000, Jan 1, good friday, easter monday, May 1, christmas day, Dec 26
  // http://www.ecb.europa.eu/press/pr/date/1999/html/pr990715_1.en.html
  // in 2001, Jan 1, good friday, easter monday, May 1, christmas day, Dec 26, Dec 31
  // http://www.ecb.europa.eu/press/pr/date/2000/html/pr000525_2.en.html
  // from 2002, Jan 1, good friday, easter monday, May 1, christmas day, Dec 26
  // http://www.ecb.europa.eu/press/pr/date/2000/html/pr001214_4.en.html
  /**
   * Generates the European TARGET holiday calendar, `EUTA`.
   *
   * The range of years starts at 1997, when the system this calendar describes began its
   * testing phase, rather than at 1950 as every other calendar here does.
   *
   * @return the calendar of European TARGET settlement holidays from 1997 to 2099
   */
  def generateEuropeanTarget(): ImmutableHolidayCalendar =
    satSunCalendar(HolidayCalendarIds.EUTA, (1997 to 2099).iterator.flatMap(europeanTargetYear))

  /**
   * Calculates the European TARGET settlement holidays of one year.
   *
   * @param year  the year, from 1997 onwards
   * @return the holidays of that year, before the weekend days are removed
   */
  private def europeanTargetYear(year: Int): List[LocalDate] =
    List.concat(
      if (year >= 2000) {
        List(
          date(year, 1, 1),
          easter(year).minusDays(2),
          easter(year).plusDays(1),
          date(year, 5, 1),
          date(year, 12, 25),
          date(year, 12, 26))
      } else { // 1997 to 1999
        List(
          date(year, 1, 1),
          date(year, 12, 25))
      },
      if (year == 1999 || year == 2001) {
        List(date(year, 12, 31))
      } else {
        Nil
      })

  /**
   * Calculates the holidays common to the United States calendars for one year.
   *
   * The dates returned are the holidays the United States calendars here have in common, as
   * varied by the arguments; each caller combines them with the holidays particular to its own
   * calendar.
   *
   * @param year  the year
   * @param bumpBack  true if Independence Day and Christmas Day move back to Friday when they
   *   fall on a Saturday, false if a Saturday is observed on the day itself
   * @param columbusVeteran  true if Columbus Day and Veterans Day are observed
   * @param mlkStartYear  the first year in which Martin Luther King Day is observed
   * @return the common holidays of that year, before the weekend days are removed
   */
  private def usCommon(
      year: Int,
      bumpBack: Boolean,
      columbusVeteran: Boolean,
      mlkStartYear: Int): List[LocalDate] =

    List.concat(
      // new year, adjusted if Sunday
      List(bumpSunToMon(date(year, 1, 1))),
      // martin luther king
      if (year >= mlkStartYear) {
        List(date(year, 1, 1).`with`(dayOfWeekInMonth(3, MONDAY)))
      } else {
        Nil
      },
      // washington
      if (year < 1971) {
        List(bumpSunToMon(date(year, 2, 22)))
      } else {
        List(date(year, 2, 1).`with`(dayOfWeekInMonth(3, MONDAY)))
      },
      // memorial
      if (year < 1971) {
        List(bumpSunToMon(date(year, 5, 30)))
      } else {
        List(date(year, 5, 1).`with`(lastInMonth(MONDAY)))
      },
      // juneteenth, from 2022
      if (year >= 2022) {
        List(bumpToFriOrMon(date(year, 6, 19)))
      } else {
        Nil
      },
      // labor day
      List(date(year, 9, 1).`with`(firstInMonth(MONDAY))),
      // columbus day
      if (columbusVeteran) {
        if (year < 1971) {
          List(bumpSunToMon(date(year, 10, 12)))
        } else {
          List(date(year, 10, 1).`with`(dayOfWeekInMonth(2, MONDAY)))
        }
      } else {
        Nil
      },
      // veterans day
      if (columbusVeteran) {
        if (year >= 1971 && year < 1978) {
          List(date(year, 10, 1).`with`(dayOfWeekInMonth(4, MONDAY)))
        } else {
          List(bumpSunToMon(date(year, 11, 11)))
        }
      } else {
        Nil
      },
      // thanksgiving
      List(date(year, 11, 1).`with`(dayOfWeekInMonth(4, THURSDAY))),
      // independence day & christmas day
      if (bumpBack) {
        List(bumpToFriOrMon(date(year, 7, 4)), bumpToFriOrMon(date(year, 12, 25)))
      } else {
        List(bumpSunToMon(date(year, 7, 4)), bumpSunToMon(date(year, 12, 25)))
      })

  // https://www.sifma.org/resources/general/holiday-schedule/
  /**
   * Generates the United States government securities holiday calendar, `USGS`.
   *
   * @return the calendar of United States government securities holidays from 1950 to 2099
   */
  def generateUsGovtSecurities(): ImmutableHolidayCalendar =
    satSunCalendar(
      HolidayCalendarIds.USGS,
      (1950 to 2099).iterator.flatMap(usGovtSecuritiesYear) ++
        Iterator(date(2018, 12, 5))) // Death of George H.W. Bush

  /**
   * Calculates the United States government securities holidays of one year.
   *
   * @param year  the year
   * @return the holidays of that year, before the weekend days are removed
   */
  private def usGovtSecuritiesYear(year: Int): List[LocalDate] =
    List.concat(
      usCommon(year, bumpBack = true, columbusVeteran = true, mlkStartYear = 1986),
      // good friday, in 1999/2007 only a partial holiday
      List(easter(year).minusDays(2)),
      // hurricane sandy
      if (year == 2012) {
        List(date(year, 10, 30))
      } else {
        Nil
      })

  // http://www.cs.ny.gov/attendance_leave/2012_legal_holidays.cfm
  // http://www.cs.ny.gov/attendance_leave/2013_legal_holidays.cfm
  // ignore election day and lincoln day
  /**
   * Generates the New York State holiday calendar, `USNY`.
   *
   * @return the calendar of New York State holidays from 1950 to 2099
   */
  def generateUsNewYork(): ImmutableHolidayCalendar =
    satSunCalendar(
      HolidayCalendarIds.USNY,
      (1950 to 2099).iterator
        .flatMap(year => usCommon(year, bumpBack = false, columbusVeteran = true, mlkStartYear = 1986)))

  // https://www.newyorkfed.org/aboutthefed/holiday_schedule.html
  /**
   * Generates the Federal Reserve Bank of New York holiday calendar, `NYFD`.
   *
   * @return the calendar of New York Federal Reserve holidays from 1950 to 2099
   */
  def generateNewYorkFed(): ImmutableHolidayCalendar =
    satSunCalendar(
      HolidayCalendarIds.NYFD,
      (1950 to 2099).iterator
        .flatMap(year => usCommon(year, bumpBack = false, columbusVeteran = true, mlkStartYear = 1986)))

  // https://www.nyse.com/markets/hours-calendars
  // http://www1.nyse.com/pdfs/closings.pdf
  /**
   * Generates the New York Stock Exchange holiday calendar, `NYSE`.
   *
   * Alongside the rule-derived holidays this calendar carries the exchange's one-off
   * closures - snow, blackouts, the 1968 paperwork crisis, state funerals - which are dates
   * rather than rules and are therefore listed.
   *
   * @return the calendar of New York Stock Exchange holidays from 1950 to 2099
   */
  def generateNewYorkStockExchange(): ImmutableHolidayCalendar =
    satSunCalendar(
      HolidayCalendarIds.NYSE,
      (1950 to 2099).iterator.flatMap(newYorkStockExchangeYear) ++
        nyseLincolnColumbusVeterans.iterator ++
        nyseElectionDays.iterator ++
        nyseSpecialDays.iterator)

  /**
   * Calculates the rule-derived New York Stock Exchange holidays of one year.
   *
   * @param year  the year
   * @return the holidays of that year, before the weekend days are removed
   */
  private def newYorkStockExchangeYear(year: Int): List[LocalDate] =
    List.concat(
      usCommon(year, bumpBack = true, columbusVeteran = false, mlkStartYear = 1998),
      // good friday
      List(easter(year).minusDays(2)))

  /**
   * The Lincoln, Columbus and Veterans days the exchange observed up to 1953.
   *
   * Lincoln day was observed 1896-1953, Columbus day 1909-1953 and Veterans day 1934-1953; the
   * calendar starts in 1950, so only the last four years of each fall in range.
   *
   * @return the dates observed from 1950 to 1953
   */
  private def nyseLincolnColumbusVeterans: List[LocalDate] =
    (1950 to 1953).iterator
      .flatMap(year => List(date(year, 2, 12), date(year, 10, 12), date(year, 11, 11)))
      .toList

  /**
   * The election days the exchange closed for.
   *
   * Election day is the Tuesday after the first Monday of November, observed every year to
   * 1968 and in three presidential years after that.
   *
   * @return the election days observed
   */
  private def nyseElectionDays: List[LocalDate] =
    List.concat(
      // election day, Tue after first Monday of November
      (1950 to 1968).iterator.map(year => date(year, 11, 1).`with`(nextOrSame(MONDAY)).plusDays(1)).toList,
      List(
        date(1972, 11, 7),
        date(1976, 11, 2),
        date(1980, 11, 4)))

  /**
   * The one-off days on which the exchange did not trade.
   *
   * @return the special closures, each with the reason it was declared
   */
  private def nyseSpecialDays: List[LocalDate] =
    List(
      date(1955, 12, 24), // Christmas Eve
      date(1956, 12, 24), // Christmas Eve
      date(1958, 12, 26), // Day after Christmas
      date(1961, 5, 29), // Decoration day
      date(1963, 11, 25), // Death of John F Kennedy
      date(1965, 12, 24), // Christmas Eve
      date(1968, 2, 12), // Lincoln birthday
      date(1968, 4, 9), // Death of Martin Luther King
      date(1968, 6, 12), // Paperwork crisis
      date(1968, 6, 19), // Paperwork crisis
      date(1968, 6, 26), // Paperwork crisis
      date(1968, 7, 3), // Paperwork crisis
      date(1968, 7, 5), // Day after independence
      date(1968, 7, 10), // Paperwork crisis
      date(1968, 7, 17), // Paperwork crisis
      date(1968, 7, 24), // Paperwork crisis
      date(1968, 7, 31), // Paperwork crisis
      date(1968, 8, 7), // Paperwork crisis
      date(1968, 8, 13), // Paperwork crisis
      date(1968, 8, 21), // Paperwork crisis
      date(1968, 8, 28), // Paperwork crisis
      date(1968, 9, 4), // Paperwork crisis
      date(1968, 9, 11), // Paperwork crisis
      date(1968, 9, 18), // Paperwork crisis
      date(1968, 9, 25), // Paperwork crisis
      date(1968, 10, 2), // Paperwork crisis
      date(1968, 10, 9), // Paperwork crisis
      date(1968, 10, 16), // Paperwork crisis
      date(1968, 10, 23), // Paperwork crisis
      date(1968, 10, 30), // Paperwork crisis
      date(1968, 11, 6), // Paperwork crisis
      date(1968, 11, 13), // Paperwork crisis
      date(1968, 11, 20), // Paperwork crisis
      date(1968, 11, 27), // Paperwork crisis
      date(1968, 12, 4), // Paperwork crisis
      date(1968, 12, 11), // Paperwork crisis
      date(1968, 12, 18), // Paperwork crisis
      date(1968, 12, 25), // Paperwork crisis
      date(1968, 12, 31), // Paperwork crisis
      date(1969, 2, 10), // Snow
      date(1969, 3, 31), // Death of Dwight Eisenhower
      date(1969, 7, 21), // Lunar exploration
      date(1972, 12, 28), // Death of Harry Truman
      date(1973, 1, 25), // Death of Lyndon Johnson
      date(1977, 7, 14), // Blackout
      date(1985, 9, 27), // Hurricane Gloria
      date(1994, 4, 27), // Death of Richard Nixon
      date(2001, 9, 11), // 9/11 attack
      date(2001, 9, 12), // 9/11 attack
      date(2001, 9, 13), // 9/11 attack
      date(2001, 9, 14), // 9/11 attack
      date(2004, 6, 11), // Death of Ronald Reagan
      date(2007, 1, 2), // Death of Gerald Ford
      date(2012, 10, 30), // Hurricane Sandy
      date(2018, 12, 5), // Death of George H.W. Bush
      date(2025, 1, 9)) // Death of Jimmy Carter


  // https://www.boj.or.jp/en/about/outline/holi.htm/
  // http://web.archive.org/web/20110513190217/http://www.boj.or.jp/en/about/outline/holi.htm/
  // http://web.archive.org/web/20130502031733/http://www.boj.or.jp/en/about/outline/holi.htm
  // http://www8.cao.go.jp/chosei/shukujitsu/gaiyou.html (law)
  // http://www.nao.ac.jp/faq/a0301.html (equinox)
  // http://eco.mtk.nao.ac.jp/koyomi/faq/holiday.html.en
  // https://www.jpx.co.jp/english/announce/market-holidays.html
  // https://www.loc.gov/law/foreign-news/article/japan-three-holidays-to-be-moved-to-ease-2020-olympic-ceremony-traffic/
  // https://www.nippon.com/en/japan-data/h00738/
  /**
   * Generates the Tokyo holiday calendar, `JPTO`.
   *
   * The years are accumulated in order rather than independently, because the citizens' day
   * rule reads the holidays the earlier rules established - see [[citizensDay]]. That
   * accumulation is the ordered set the calendar is built from, so this is the one generator
   * that does not order its dates through [[weekdaysOnly]]: it drops the weekend days from the
   * set it already has - see [[weekdaysOf]] - and hands the result straight to the factory, so
   * it too orders the calendar once.
   *
   * @return the calendar of Tokyo bank holidays from 1950 to 2099
   */
  def generateTokyo(): ImmutableHolidayCalendar = {
    val generated =
      (1950 to 2099).foldLeft(SortedSet.empty[LocalDate](ImmutableHolidayCalendar.dateOrdering))(tokyoYear)
    val withOneOffDates = generated ++
      List(
        date(1959, 4, 10), // marriage akihito
        date(1989, 2, 24), // funeral showa
        date(1990, 11, 12), // enthrone akihito
        date(1993, 6, 9), // marriage naruhito
        date(2019, 4, 30), // abdication
        date(2019, 5, 1), // accession
        date(2019, 5, 2), // accession
        date(2019, 10, 22)) // enthronement
    ImmutableHolidayCalendar.ofNormalized(
      HolidayCalendarIds.JPTO,
      weekdaysOf(withOneOffDates),
      satSunWeekend,
      Nil)
  }

  /**
   * Adds the Tokyo bank holidays of one year to those already established.
   *
   * The order of the three steps is load-bearing and cannot be rearranged: the citizens' day
   * rule asks whether the holidays accumulated before it contain the two dates its day sits
   * between, and the second application of that rule can see the date the first one added.
   *
   * @param holidays  the holidays established by the earlier years and the earlier rules
   * @param year  the year
   * @return the holidays extended with those of that year
   */
  private def tokyoYear(holidays: SortedSet[LocalDate], year: Int): SortedSet[LocalDate] = {
    val throughAutumnEquinox = holidays ++ tokyoYearThroughAutumnEquinox(year)
    val withFirstCitizensDay = citizensDay(throughAutumnEquinox, date(year, 9, 20), date(year, 9, 22))
    val withCitizensDays = citizensDay(withFirstCitizensDay, date(year, 9, 21), date(year, 9, 23))
    withCitizensDays ++ tokyoYearFromHealthSports(year)
  }

  /**
   * Calculates the Tokyo bank holidays of one year up to and including the autumn equinox.
   *
   * @param year  the year
   * @return the holidays of that year declared before the citizens' day rule applies
   */
  private def tokyoYearThroughAutumnEquinox(year: Int): List[LocalDate] =
    List.concat(
      // new year
      List(
        date(year, 1, 1),
        date(year, 1, 2),
        date(year, 1, 3)),
      // coming of age
      if (year >= 2000) {
        List(date(year, 1, 1).`with`(dayOfWeekInMonth(2, MONDAY)))
      } else {
        List(bumpSunToMon(date(year, 1, 15)))
      },
      // national foundation
      if (year >= 1967) {
        List(bumpSunToMon(date(year, 2, 11)))
      } else {
        Nil
      },
      // vernal equinox (from 1948), 20th or 21st (predictions/facts 2000 to 2030)
      if (year == 2000 || year == 2001 || year == 2004 || year == 2005 || year == 2008 || year == 2009 ||
        year == 2012 || year == 2013 || year == 2016 || year == 2017 ||
        year == 2020 || year == 2021 || year == 2024 || year == 2025 || year == 2026 || year == 2028 ||
        year == 2029 || year == 2030) {
        List(bumpSunToMon(date(year, 3, 20)))
      } else {
        List(bumpSunToMon(date(year, 3, 21)))
      },
      // showa (from 2007 onwards), greenery (from 1989 to 2006), emperor (before 1989)
      // http://news.bbc.co.uk/1/hi/world/asia-pacific/4543461.stm
      List(bumpSunToMon(date(year, 4, 29))),
      // constitution (from 1948)
      // greenery (from 2007 onwards), holiday between two other holidays before that (from 1985)
      // children (from 1948)
      if (year >= 1985) {
        List.concat(
          List(
            bumpSunToMon(date(year, 5, 3)),
            bumpSunToMon(date(year, 5, 4)),
            bumpSunToMon(date(year, 5, 5))),
          if (year >= 2007 && (date(year, 5, 3).getDayOfWeek == SUNDAY || date(year, 5, 4).getDayOfWeek == SUNDAY)) {
            List(date(year, 5, 6))
          } else {
            Nil
          })
      } else {
        List(
          bumpSunToMon(date(year, 5, 3)),
          bumpSunToMon(date(year, 5, 5)))
      },
      // marine
      if (year == 2021) {
        // moved because of the Olympics
        List(date(year, 7, 22))
      } else if (year == 2020) {
        // moved because of the Olympics (day prior to opening ceremony)
        List(date(year, 7, 23))
      } else if (year >= 2003) {
        List(date(year, 7, 1).`with`(dayOfWeekInMonth(3, MONDAY)))
      } else if (year >= 1996) {
        List(bumpSunToMon(date(year, 7, 20)))
      } else {
        Nil
      },
      // mountain
      if (year == 2021) {
        // moved because of the Olympics
        List(date(year, 8, 9))
      } else if (year == 2020) {
        // moved because of the Olympics (day after closing ceremony)
        List(date(year, 8, 10))
      } else if (year >= 2016) {
        List(bumpSunToMon(date(year, 8, 11)))
      } else {
        Nil
      },
      // aged
      if (year >= 2003) {
        List(date(year, 9, 1).`with`(dayOfWeekInMonth(3, MONDAY)))
      } else if (year >= 1966) {
        List(bumpSunToMon(date(year, 9, 15)))
      } else {
        Nil
      },
      // autumn equinox (from 1948), 22nd or 23rd (predictions/facts 2000 to 2030)
      if (year == 2012 || year == 2016 || year == 2020 || year == 2024 || year == 2028) {
        List(bumpSunToMon(date(year, 9, 22)))
      } else {
        List(bumpSunToMon(date(year, 9, 23)))
      })

  /**
   * Calculates the Tokyo bank holidays of one year from the health-sports day onwards.
   *
   * @param year  the year
   * @return the holidays of that year declared after the citizens' day rule applies
   */
  private def tokyoYearFromHealthSports(year: Int): List[LocalDate] =
    List.concat(
      // health-sports
      if (year == 2021) {
        // moved because of the Olympics
        List(date(year, 7, 23))
      } else if (year == 2020) {
        // moved because of the Olympics (day of opening ceremony)
        List(date(year, 7, 24))
      } else if (year >= 2000) {
        List(date(year, 10, 1).`with`(dayOfWeekInMonth(2, MONDAY)))
      } else if (year >= 1966) {
        List(bumpSunToMon(date(year, 10, 10)))
      } else {
        Nil
      },
      // culture (from 1948)
      List(bumpSunToMon(date(year, 11, 3))),
      // labor (from 1948)
      List(bumpSunToMon(date(year, 11, 23))),
      // emperor's birthday - 23 December from 1990 to 2018, 23 February from 2020
      if (year >= 1990 && year < 2019) {
        List(bumpSunToMon(date(year, 12, 23)))
      } else if (year >= 2020) {
        List(bumpSunToMon(date(year, 2, 23)))
      } else {
        Nil
      },
      // new years eve - bank of Japan, but not national holiday
      List(bumpSunToMon(date(year, 12, 31))))

  /**
   * Adds the day between two holidays that the citizens' day rule declares.
   *
   * The rule reads the holidays the earlier rules established, which is why this takes them and
   * returns them rather than returning the date it contributes: whether the rule applies at all
   * depends on both of the named dates already being holidays. The day between is declared only
   * where `date1` falls on a Monday, a Tuesday or a Wednesday, so it never falls at a weekend.
   *
   * @param holidays  the holidays the earlier rules established
   * @param date1  the earlier of the two dates the new holiday would sit between
   * @param date2  the later of the two dates the new holiday would sit between
   * @return the holidays, extended with the day after `date1` where the rule applies
   */
  private def citizensDay(
      holidays: SortedSet[LocalDate],
      date1: LocalDate,
      date2: LocalDate): SortedSet[LocalDate] =

    if (holidays.contains(date1) && holidays.contains(date2)) {
      if (date1.getDayOfWeek == MONDAY || date1.getDayOfWeek == TUESDAY || date1.getDayOfWeek == WEDNESDAY) {
        holidays + date1.plusDays(1)
      } else {
        holidays
      }
    } else {
      holidays
    }

  // https://www.cnesst.gouv.qc.ca/en/working-conditions/leave/statutory-holidays/list-paid-statutory-holidays
  // https://www.canada.ca/en/revenue-agency/services/tax/public-holidays.html
  /**
   * Generates the Montreal holiday calendar, `CAMO`.
   *
   * @return the calendar of Montreal bank holidays from 1950 to 2099
   */
  def generateMontreal(): ImmutableHolidayCalendar =
    satSunCalendar(HolidayCalendarId.of("CAMO"), (1950 to 2099).iterator.flatMap(montrealYear))

  /**
   * Calculates the Montreal bank holidays of one year.
   *
   * @param year  the year
   * @return the holidays of that year, before the weekend days are removed
   */
  private def montrealYear(year: Int): List[LocalDate] =
    List.concat(
      List(
        // new year
        bumpToMon(date(year, 1, 1)),
        // good friday
        easter(year).minusDays(2),
        // patriots
        date(year, 5, 25).`with`(previous(MONDAY)),
        // fete nationale quebec
        bumpToMon(date(year, 6, 24)),
        // canada
        bumpToMon(date(year, 7, 1)),
        // labour
        first(year, 9).`with`(dayOfWeekInMonth(1, MONDAY))),
      // national day for truth and reconciliation
      if (year >= 2021) {
        List(date(year, 9, 30))
      } else {
        Nil
      },
      List(
        // thanksgiving
        first(year, 10).`with`(dayOfWeekInMonth(2, MONDAY)),
        // christmas
        bumpToMon(date(year, 12, 25))))

  // http://www.labour.gov.on.ca/english/es/pubs/guide/publicholidays.php
  // http://www.cra-arc.gc.ca/tx/hldys/menu-eng.html
  // http://www.tmxmoney.com/en/investor_tools/market_hours.html
  // http://www.statutoryholidayscanada.com/
  // http://www.osc.gov.on.ca/en/SecuritiesLaw_csa_20151209_13-315_sra-closed-dates.htm
  /**
   * Generates the Toronto holiday calendar, `CATO`.
   *
   * @return the calendar of Toronto bank holidays from 1950 to 2099
   */
  def generateToronto(): ImmutableHolidayCalendar =
    satSunCalendar(HolidayCalendarId.of("CATO"), (1950 to 2099).iterator.flatMap(torontoYear))

  /**
   * Calculates the Toronto bank holidays of one year.
   *
   * @param year  the year
   * @return the holidays of that year, before the weekend days are removed
   */
  private def torontoYear(year: Int): List[LocalDate] =
    List.concat(
      // new year (public)
      List(bumpToMon(date(year, 1, 1))),
      // family (public)
      if (year >= 2008) {
        List(first(year, 2).`with`(dayOfWeekInMonth(3, MONDAY)))
      } else {
        Nil
      },
      List(
        // good friday (public)
        easter(year).minusDays(2),
        // victoria (public)
        date(year, 5, 25).`with`(previous(MONDAY)),
        // canada (public)
        bumpToMon(date(year, 7, 1)),
        // civic
        first(year, 8).`with`(dayOfWeekInMonth(1, MONDAY)),
        // labour (public)
        first(year, 9).`with`(dayOfWeekInMonth(1, MONDAY))),
      // national day for truth and reconciliation
      if (year >= 2021) {
        List(date(year, 9, 30))
      } else {
        Nil
      },
      List(
        // thanksgiving (public)
        first(year, 10).`with`(dayOfWeekInMonth(2, MONDAY)),
        // remembrance
        bumpToMon(date(year, 11, 11)),
        // christmas (public)
        christmasBumpedSatSun(year),
        // boxing (public)
        boxingDayBumpedSatSun(year)))

  // http://www.finansraadet.dk/Bankkunde/Pages/bankhelligdage.aspx
  // web archive history of those pages
  /**
   * Generates the Copenhagen holiday calendar, `DKCO`.
   *
   * @return the calendar of Copenhagen bank holidays from 1950 to 2099
   */
  def generateCopenhagen(): ImmutableHolidayCalendar =
    satSunCalendar(HolidayCalendarId.of("DKCO"), (1950 to 2099).iterator.flatMap(copenhagenYear))

  /**
   * Calculates the Copenhagen bank holidays of one year.
   *
   * @param year  the year
   * @return the holidays of that year, before the weekend days are removed
   */
  private def copenhagenYear(year: Int): List[LocalDate] =
    List(
      // new year
      date(year, 1, 1),
      // maundy thursday
      easter(year).minusDays(3),
      // good friday
      easter(year).minusDays(2),
      // easter monday
      easter(year).plusDays(1),
      // prayer day (Friday)
      easter(year).plusDays(26),
      // ascension (Thursday)
      easter(year).plusDays(39),
      // ascension + 1 (Friday)
      easter(year).plusDays(40),
      // whit monday
      easter(year).plusDays(50),
      // constitution
      date(year, 6, 5),
      // christmas eve
      date(year, 12, 24),
      // christmas
      date(year, 12, 25),
      // boxing
      date(year, 12, 26),
      // new years eve
      date(year, 12, 31))

  // http://www.oslobors.no/ob_eng/Oslo-Boers/About-Oslo-Boers/Opening-hours
  // http://www.oslobors.no/Oslo-Boers/Om-Oslo-Boers/AApningstider
  // web archive history of those pages
  /**
   * Generates the Oslo holiday calendar, `NOOS`.
   *
   * @return the calendar of Oslo bank holidays from 1950 to 2099
   */
  def generateOslo(): ImmutableHolidayCalendar =
    satSunCalendar(HolidayCalendarId.of("NOOS"), (1950 to 2099).iterator.flatMap(osloYear))

  /**
   * Calculates the Oslo bank holidays of one year.
   *
   * @param year  the year
   * @return the holidays of that year, before the weekend days are removed
   */
  private def osloYear(year: Int): List[LocalDate] =
    List(
      // new year
      date(year, 1, 1),
      // maundy thursday
      easter(year).minusDays(3),
      // good friday
      easter(year).minusDays(2),
      // easter monday
      easter(year).plusDays(1),
      // labour
      date(year, 5, 1),
      // constitution
      date(year, 5, 17),
      // ascension
      easter(year).plusDays(39),
      // whit monday
      easter(year).plusDays(50),
      // christmas eve
      date(year, 12, 24),
      // christmas
      date(year, 12, 25),
      // boxing
      date(year, 12, 26),
      // new years eve
      date(year, 12, 31))


  // https://www.nzfma.org/Site/practices_standards/market_conventions.aspx
  /**
   * Generates the Auckland holiday calendar, `NZAU`.
   *
   * @return the calendar of Auckland bank holidays from 1950 to 2099
   */
  def generateAuckland(): ImmutableHolidayCalendar =
    satSunCalendar(HolidayCalendarId.of("NZAU"), (1950 to 2099).iterator.flatMap(aucklandYear))

  /**
   * Calculates the Auckland bank holidays of one year.
   *
   * @param year  the year
   * @return the holidays of that year, before the weekend days are removed
   */
  private def aucklandYear(year: Int): List[LocalDate] =
    List.concat(
      newZealand(year),
      // auckland anniversary day
      List(date(year, 1, 29).minusDays(3).`with`(nextOrSame(MONDAY))))

  // https://www.nzfma.org/Site/practices_standards/market_conventions.aspx
  /**
   * Generates the Wellington holiday calendar, `NZWE`.
   *
   * @return the calendar of Wellington bank holidays from 1950 to 2099
   */
  def generateWellington(): ImmutableHolidayCalendar =
    satSunCalendar(HolidayCalendarId.of("NZWE"), (1950 to 2099).iterator.flatMap(wellingtonYear))

  /**
   * Calculates the Wellington bank holidays of one year.
   *
   * @param year  the year
   * @return the holidays of that year, before the weekend days are removed
   */
  private def wellingtonYear(year: Int): List[LocalDate] =
    List.concat(
      newZealand(year),
      // wellington anniversary day
      List(date(year, 1, 22).minusDays(3).`with`(nextOrSame(MONDAY))))

  // https://www.nzfma.org/Site/practices_standards/market_conventions.aspx
  /**
   * Generates the New Zealand bank holiday calendar, `NZBD`.
   *
   * This is an artificial, non-ISDA definition, named after `BRBD` for Brazil. It is needed
   * because the `NZD-BBR` index is published on both the Wellington and the Auckland
   * anniversary days, so neither city's calendar describes it and the national holidays alone
   * do. The identifier is built here rather than taken from [[HolidayCalendarIds]], which does
   * not declare it.
   *
   * @return the calendar of New Zealand national bank holidays from 1950 to 2099
   */
  def generateNewZealand(): ImmutableHolidayCalendar = {
    satSunCalendar(HolidayCalendarId.of("NZBD"), (1950 to 2099).iterator.flatMap(newZealand))
  }

  /**
   * Calculates the holidays common to the New Zealand calendars for one year.
   *
   * The dates returned are the national holidays; each caller adds its own city's anniversary
   * day.
   *
   * @param year  the year
   * @return the national holidays of that year, before the weekend days are removed
   */
  private def newZealand(year: Int): List[LocalDate] = {
    // new year and day after
    val newYear = bumpToMon(date(year, 1, 1))
    List.concat(
      List(newYear, bumpToMon(newYear.plusDays(1))),
      // waitangi day
      // https://www.employment.govt.nz/leave-and-holidays/public-holidays/public-holidays-and-anniversary-dates/
      if (year >= 2014) {
        List(bumpToMon(date(year, 2, 6)))
      } else {
        List(date(year, 2, 6))
      },
      List(
        // good friday
        easter(year).minusDays(2),
        // easter monday
        easter(year).plusDays(1)),
      // anzac day
      // https://www.employment.govt.nz/leave-and-holidays/public-holidays/public-holidays-and-anniversary-dates/
      if (year >= 2014) {
        List(bumpToMon(date(year, 4, 25)))
      } else {
        List(date(year, 4, 25))
      },
      // queen's birthday
      List(first(year, 6).`with`(firstInMonth(MONDAY))),
      // matariki day
      // https://www.legislation.govt.nz/act/public/2022/0014/latest/whole.html#LMS557893
      matarikiDay(year),
      // queen's funeral
      if (year == 2022) {
        List(date(year, 9, 26))
      } else {
        Nil
      },
      List(
        // labour day
        first(year, 10).`with`(dayOfWeekInMonth(4, MONDAY)),
        // christmas
        christmasBumpedSatSun(year),
        boxingDayBumpedSatSun(year)))
  }

  /**
   * Calculates Matariki day, the New Zealand public holiday observed from 2022.
   *
   * The date follows the Maori lunar calendar and so cannot be derived from a rule of the
   * Gregorian year; the legislation that created the holiday therefore lists it, for 2022 to
   * 2052 inclusive, and this is that list. A year outside that range has no Matariki day here,
   * which is the extent of the legislated list rather than a claim that the holiday ends in
   * 2052.
   *
   * @param year  the year
   * @return Matariki day of that year, or nothing where the legislation does not name one
   */
  private def matarikiDay(year: Int): List[LocalDate] =
    if (year >= 2022 && year <= 2052) {
      year match {
        case 2022 | 2033 | 2044 => List(date(year, 6, 24))
        case 2023 | 2028 => List(date(year, 7, 14))
        case 2024 => List(date(year, 6, 28))
        case 2025 => List(date(year, 6, 20))
        case 2026 | 2037 => List(date(year, 7, 10))
        case 2027 | 2038 | 2049 => List(date(year, 6, 25))
        case 2029 | 2040 => List(date(year, 7, 6))
        case 2030 | 2052 => List(date(year, 6, 21))
        case 2031 | 2042 => List(date(year, 7, 11))
        case 2032 => List(date(year, 7, 2))
        case 2034 | 2045 => List(date(year, 7, 7))
        case 2035 | 2046 => List(date(year, 6, 29))
        case 2036 => List(date(year, 7, 18))
        case 2039 | 2050 => List(date(year, 7, 15))
        case 2041 | 2047 => List(date(year, 7, 19))
        case 2043 | 2048 => List(date(year, 7, 3))
        case 2051 => List(date(year, 6, 30))
        case _ => Nil
      }
    } else {
      Nil
    }

  // http://isap.sejm.gov.pl/DetailsServlet?id=WDU19510040028 and linked pages
  // https://www.gpw.pl/dni_bez_sesji_en
  // http://jollyday.sourceforge.net/data/pl.html
  // https://www.gpw.pl/session-details
  // https://www.gpw.pl/news?cmn_id=107609&title=No+exchange+trading+session+on+12+November+2018
  // https://www.gpw.pl/news?cmn_id=107794&title=December+24%2C+2018+-+Closing+day
  /**
   * Generates the Warsaw holiday calendar, `PLWA`.
   *
   * The holiday law dates from 1951, and its rules are applied from 1950 so that this calendar
   * covers the same range of years as the others.
   *
   * @return the calendar of Warsaw bank holidays from 1950 to 2099
   */
  def generateWarsaw(): ImmutableHolidayCalendar = {
    satSunCalendar(
      HolidayCalendarId.of("PLWA"),
      (1950 to 2099).iterator.flatMap(warsawYear) ++
        // 100th independence day anniversary
        Iterator(date(2018, 11, 12)))
  }

  /**
   * Calculates the Warsaw bank holidays of one year.
   *
   * @param year  the year
   * @return the holidays of that year, before the weekend days are removed
   */
  private def warsawYear(year: Int): List[LocalDate] =
    List.concat(
      // new year
      List(date(year, 1, 1)),
      // epiphany
      if (year < 1961 || year >= 2011) {
        List(date(year, 1, 6))
      } else {
        Nil
      },
      List(
        // easter monday
        easter(year).plusDays(1),
        // state
        date(year, 5, 1)),
      // constitution
      if (year >= 1990) {
        List(date(year, 5, 3))
      } else {
        Nil
      },
      // rebirth/national
      if (year < 1990) {
        List(date(year, 7, 22))
      } else {
        Nil
      },
      // corpus christi
      List(easter(year).plusDays(60)),
      // assumption
      if (year < 1961 || year >= 1989) {
        List(date(year, 8, 15))
      } else {
        Nil
      },
      // all saints
      List(date(year, 11, 1)),
      // independence
      if (year >= 1990) {
        List(date(year, 11, 11))
      } else {
        Nil
      },
      List(
        // christmas (exchange)
        date(year, 12, 24),
        // christmas
        date(year, 12, 25),
        // boxing
        date(year, 12, 26)),
      // new years eve (exchange, rule based on sample data)
      warsawNewYearsEve(year))

  /**
   * Calculates the Warsaw exchange closure on New Year's Eve.
   *
   * The exchange closed on New Year's Eve when it fell on a Monday, a Thursday or a Friday;
   * the rule is inferred from the published session data rather than stated in law.
   *
   * @param year  the year
   * @return New Year's Eve where the exchange was closed, or nothing
   */
  private def warsawNewYearsEve(year: Int): List[LocalDate] = {
    val nyeve = date(year, 12, 31)
    if (nyeve.getDayOfWeek == MONDAY || nyeve.getDayOfWeek == THURSDAY || nyeve.getDayOfWeek == FRIDAY) {
      List(nyeve)
    } else {
      Nil
    }
  }

  // the dates on which the STIBOR fixing occurred
  // http://www.riksbank.se/en/Interest-and-exchange-rates/search-interest-rates-exchange-rates/?g5-SEDP1MSTIBOR=on&from=2016-01-01&to=2016-10-05&f=Day&cAverage=Average&s=Comma#search
  /**
   * Generates the Stockholm holiday calendar, `SEST`.
   *
   * @return the calendar of Stockholm bank holidays from 1950 to 2099
   */
  def generateStockholm(): ImmutableHolidayCalendar =
    satSunCalendar(HolidayCalendarId.of("SEST"), (1950 to 2099).iterator.flatMap(stockholmYear))

  /**
   * Calculates the Stockholm bank holidays of one year.
   *
   * @param year  the year
   * @return the holidays of that year, before the weekend days are removed
   */
  private def stockholmYear(year: Int): List[LocalDate] =
    List.concat(
      List(
        // new year
        date(year, 1, 1),
        // epiphany
        date(year, 1, 6),
        // good friday
        easter(year).minusDays(2),
        // easter monday
        easter(year).plusDays(1),
        // labour
        date(year, 5, 1),
        // ascension
        easter(year).plusDays(39),
        // midsummer friday
        date(year, 6, 19).`with`(nextOrSame(FRIDAY))),
      // national
      if (year > 2005) {
        List(date(year, 6, 6))
      } else {
        Nil
      },
      List(
        // christmas
        date(year, 12, 24),
        // christmas
        date(year, 12, 25),
        // boxing
        date(year, 12, 26),
        // new years eve (fixings, rule based on sample data)
        date(year, 12, 31)))

  // http://www.rba.gov.au/schedules-events/bank-holidays/bank-holidays-2016.html
  // http://www.rba.gov.au/schedules-events/bank-holidays/bank-holidays-2017.html
  // web archive history of those pages
  /**
   * Generates the Sydney holiday calendar, `AUSY`.
   *
   * @return the calendar of Sydney bank holidays from 1950 to 2099
   */
  def generateSydney(): ImmutableHolidayCalendar =
    satSunCalendar(HolidayCalendarId.of("AUSY"), (1950 to 2099).iterator.flatMap(sydneyYear))

  /**
   * Calculates the Sydney bank holidays of one year.
   *
   * @param year  the year
   * @return the holidays of that year, before the weekend days are removed
   */
  private def sydneyYear(year: Int): List[LocalDate] =
    List.concat(
      List(
        // new year
        bumpToMon(date(year, 1, 1)),
        // australia day
        bumpToMon(date(year, 1, 26)),
        // good friday
        easter(year).minusDays(2),
        // easter monday
        easter(year).plusDays(1)),
      // anzac day
      if (year >= 2026) {
        List(bumpToMon(date(year, 4, 25)))
      } else {
        List(date(year, 4, 25))
      },
      List(
        // queen's birthday
        first(year, 6).`with`(dayOfWeekInMonth(2, MONDAY)),
        // bank holiday
        first(year, 8).`with`(dayOfWeekInMonth(1, MONDAY))),
      // queen's funeral
      if (year == 2022) {
        List(date(year, 9, 22))
      } else {
        Nil
      },
      List(
        // labour day
        first(year, 10).`with`(dayOfWeekInMonth(1, MONDAY)),
        // christmas
        christmasBumpedSatSun(year),
        // boxing
        boxingDayBumpedSatSun(year)))

  // http://www.gov.za/about-sa/public-holidays
  // http://www.gov.za/sites/www.gov.za/files/Act36of1994.pdf
  // http://www.gov.za/sites/www.gov.za/files/Act48of1995.pdf
  // 27th Dec when Tue http://www.gov.za/sites/www.gov.za/files/34881_proc72.pdf
  /**
   * Generates the Johannesburg holiday calendar, `ZAJO`.
   *
   * The rules are those of the act of 7 December 1994, and they are what this calendar models
   * across the whole of its range: the years before 1995, which the act of 1952 governed, carry
   * the 1994 rules too.
   *
   * @return the calendar of Johannesburg bank holidays from 1950 to 2099
   */
  def generateJohannesburg(): ImmutableHolidayCalendar =
    satSunCalendar(
      HolidayCalendarId.of("ZAJO"),
      (1950 to 2099).iterator.flatMap(johannesburgYear) ++ johannesburgElectionDays.iterator)

  /**
   * Calculates the Johannesburg bank holidays of one year.
   *
   * @param year  the year
   * @return the holidays of that year, before the weekend days are removed
   */
  private def johannesburgYear(year: Int): List[LocalDate] =
    // from 1995 (act of 7 Dec 1994)
    // these rules are the ones applied across the whole range, 1950 to 2099
    List(
      // new year
      bumpSunToMon(date(year, 1, 1)),
      // human rights day
      bumpSunToMon(date(year, 3, 21)),
      // good friday
      easter(year).minusDays(2),
      // family day (easter monday)
      easter(year).plusDays(1),
      // freedom day
      bumpSunToMon(date(year, 4, 27)),
      // workers day
      bumpSunToMon(date(year, 5, 1)),
      // youth day
      bumpSunToMon(date(year, 6, 16)),
      // national women's day
      bumpSunToMon(date(year, 8, 9)),
      // heritage day
      bumpSunToMon(date(year, 9, 24)),
      // day of reconciliation
      bumpSunToMon(date(year, 12, 16)),
      // christmas
      christmasBumpedSun(year),
      // goodwill
      boxingDayBumpedSun(year))

  /**
   * The one-off Johannesburg holidays, mostly election days.
   *
   * @return the dates declared by proclamation rather than by the holiday rules
   */
  private def johannesburgElectionDays: List[LocalDate] =
    List(
      // http://www.gov.za/sites/www.gov.za/files/40125_proc%2045.pdf
      date(2016, 8, 3),
      // http://www.gov.za/sites/www.gov.za/files/37376_proc13.pdf
      date(2014, 5, 7),
      // http://www.gov.za/sites/www.gov.za/files/34127_proc27.pdf
      date(2011, 5, 18),
      // http://www.gov.za/sites/www.gov.za/files/32039_17.pdf
      date(2009, 4, 22),
      // http://www.gov.za/sites/www.gov.za/files/30900_7.pdf (moved human rights day)
      date(2008, 5, 2),
      // http://www.gov.za/sites/www.gov.za/files/28442_0.pdf
      date(2006, 3, 1),
      // http://www.gov.za/sites/www.gov.za/files/26075.pdf
      date(2004, 4, 14),
      // http://www.gov.za/sites/www.gov.za/files/20032_0.pdf
      date(1999, 12, 31),
      date(2000, 1, 1),
      date(2000, 1, 2))


  // http://www.magyarkozlony.hu/dokumentumok/b0d596a3e6ce15a2350a9e138c058a78dd8622d0/megtekintes (article 148)
  // http://www.mfa.gov.hu/NR/rdonlyres/18C1949E-D740-45E0-923A-BDFC81EC44C8/0/ListofHolidays2016.pdf
  // http://jollyday.sourceforge.net/data/hu.html
  // https://englishhungary.wordpress.com/2012/01/15/bridge-days/
  // http://www.ucmsgroup.hu/newsletter/public-holiday-and-related-work-schedule-changes-in-2015/
  // http://www.ucmsgroup.hu/newsletter/public-holiday-and-related-work-schedule-changes-in-2014/
  // https://www.bse.hu/Products-and-Services/Trading-information/tranding-calendar-2019
  // https://www.bse.hu/Products-and-Services/Trading-information/trading-calendar-2020
  // https://www.bse.hu/Products-and-Services/Trading-information/trading-calendar-2021
  // https://www.bse.hu/Products-and-Services/Trading-information/trading-calendar-2022
  /**
   * Generates the Budapest holiday calendar, `HUBU`.
   *
   * Two things set this calendar apart from every other one here, and both are load-bearing.
   *
   * First, its weekend is '''Sunday alone'''. Hungary bridges a holiday that falls on a
   * Tuesday or a Thursday by taking the intervening Monday or Friday off and working the
   * neighbouring Saturday instead, so a Saturday is a business day some weeks and a holiday in
   * most, and no weekend rule can express that. The calendar therefore declares Sunday as its
   * only weekend day and lists every non-working Saturday from 1950 to 2099 explicitly - some
   * 7,800 dates - which is what [[addHungarianSaturdays]] does.
   *
   * Second, the working Saturdays are accumulated across the whole range before any of them is
   * applied. A bridged holiday displaces work to a Saturday one or two weeks away, which can
   * fall in a different month or year from the holiday that caused it, so the set has to be
   * complete before the Saturdays are listed. The years are computed independently and their
   * holidays and working Saturdays combined, which is safe because no rule here reads what
   * another year produced. The working Saturdays are collected into a plain set rather than an
   * ordered one, because the only question ever asked of them is whether a given Saturday is
   * one of them; the holidays, which do have to be ordered, are ordered once by
   * [[addHungarianSaturdays]].
   *
   * @return the calendar of Budapest bank holidays from 1950 to 2099
   */
  def generateBudapest(): ImmutableHolidayCalendar = {
    val byYear = (1950 to 2099).map(budapestYear)
    val workDays = byYear.iterator.flatMap(_._2).toSet
    ImmutableHolidayCalendar.ofNormalized(
      HolidayCalendarId.of("HUBU"),
      addHungarianSaturdays(byYear.iterator.flatMap(_._1), workDays),
      Set(SUNDAY),
      Nil)
  }

  /**
   * Calculates the Budapest holidays and working Saturdays of one year.
   *
   * @param year  the year
   * @return the holidays of that year and the Saturdays that are working days because of them
   */
  private def budapestYear(year: Int): (List[LocalDate], List[LocalDate]) = {
    // new year
    val newYear = addDateWithHungarianBridging(date(year, 1, 1), -1, 1)
    // national day
    // in 2022 the working saturday was 2 weeks after, in 2021 it was 1 week after
    // logic is determined yearly by government decree
    val nationalDayTuesRelativeWeeks = if (year == 2022) 1 else -2
    val springNationalDay = addDateWithHungarianBridging(date(year, 3, 15), nationalDayTuesRelativeWeeks, 1)
    // good friday
    val goodFriday = if (year >= 2017) List(easter(year).minusDays(2)) else Nil
    // easter monday
    val easterMonday = List(easter(year).plusDays(1))
    // labour day
    val labourDay = addDateWithHungarianBridging(date(year, 5, 1), 0, 1)
    // pentecost monday
    val pentecostMonday = List(easter(year).plusDays(50))
    // state foundation day
    // in 2015 the working saturday was 2 weeks before, in 2020 it was 1 week after
    // logic is determined yearly by government decree
    val foundationDayThuRelativeWeeks = if (year == 2020) 1 else -2
    val foundationDay = addDateWithHungarianBridging(date(year, 8, 20), 0, foundationDayThuRelativeWeeks)
    // national day
    val autumnNationalDay = addDateWithHungarianBridging(date(year, 10, 23), 0, -1)
    // all saints day
    val allSaintsDay = addDateWithHungarianBridging(date(year, 11, 1), -3, 1)
    // christmas
    val christmas = List(date(year, 12, 24), date(year, 12, 25), date(year, 12, 26))
    val christmasBridging = budapestChristmasBridging(year)
    (List.concat(
      newYear._1,
      springNationalDay._1,
      goodFriday,
      easterMonday,
      labourDay._1,
      pentecostMonday,
      foundationDay._1,
      autumnNationalDay._1,
      allSaintsDay._1,
      christmas,
      christmasBridging._1),
      List.concat(
        newYear._2,
        springNationalDay._2,
        labourDay._2,
        foundationDay._2,
        autumnNationalDay._2,
        allSaintsDay._2,
        christmasBridging._2))
  }

  /**
   * Calculates the extra Budapest days off, and working Saturdays, around Christmas.
   *
   * Christmas is bridged like the other Hungarian holidays, but by a rule of its own that
   * depends on the day of the week the 25th falls on. Christmas Eve is already a holiday in
   * every case, which is why the bridging repeats it rather than adding it.
   *
   * @param year  the year
   * @return the extra holidays and the working Saturdays that pay for them
   */
  private def budapestChristmasBridging(year: Int): (List[LocalDate], List[LocalDate]) =
    if (date(year, 12, 25).getDayOfWeek == TUESDAY) {
      (List(date(year, 12, 24)), List(date(year, 12, 15)))
    } else if (date(year, 12, 25).getDayOfWeek == WEDNESDAY) {
      (List(date(year, 12, 24), date(year, 12, 27)), List(date(year, 12, 7), date(year, 12, 21)))
    } else if (date(year, 12, 25).getDayOfWeek == THURSDAY) {
      (List(date(year, 12, 24)), Nil)
    } else if (date(year, 12, 25).getDayOfWeek == FRIDAY) {
      (List(date(year, 12, 24)), List(date(year, 12, 12)))
    } else {
      (Nil, Nil)
    }

  /**
   * Calculates the days off, and the working Saturday, that a bridged Hungarian holiday
   * implies.
   *
   * A holiday on a Monday, a Wednesday or a Friday is taken as it stands. One on a Tuesday
   * takes the Monday before it as well, and one on a Thursday the Friday after it, with a
   * Saturday becoming a working day in return; which Saturday is decided yearly by government
   * decree, which the relative-week arguments carry. A holiday at a weekend gives nothing,
   * since Saturday and Sunday are already handled - Sunday by the calendar's weekend and
   * Saturday by [[addHungarianSaturdays]].
   *
   * These rules follow the published Hungarian holiday dates, which is why the Saturday worked
   * in return is supplied per year rather than computed from the holiday.
   *
   * @param date  the date of the holiday
   * @param relativeWeeksTue  the number of weeks between the bridged Monday and the Saturday
   *   worked in return, when the holiday falls on a Tuesday
   * @param relativeWeeksThu  the number of weeks between the bridged Friday and the Saturday
   *   worked in return, when the holiday falls on a Thursday
   * @return the holidays the bridging declares and the Saturdays it makes working days
   */
  private def addDateWithHungarianBridging(
      date: LocalDate,
      relativeWeeksTue: Int,
      relativeWeeksThu: Int): (List[LocalDate], List[LocalDate]) = {

    val dow: DayOfWeek = date.getDayOfWeek
    dow match {
      case MONDAY | WEDNESDAY | FRIDAY =>
        (List(date), Nil)
      case TUESDAY =>
        // a Saturday becomes a workday in return
        (List(date.minusDays(1), date), List(date.plusDays(4).plusWeeks(relativeWeeksTue.toLong)))
      case THURSDAY =>
        // a Saturday becomes a workday in return
        (List(date.plusDays(1), date), List(date.plusDays(2).plusWeeks(relativeWeeksThu.toLong)))
      case _ =>
        (Nil, Nil)
    }
  }

  /**
   * Replaces the weekend days of the Budapest holidays with the Saturdays that are not worked.
   *
   * Every Saturday and Sunday is first removed from the holidays - a holiday that fell at a
   * weekend is not observed - and then every Saturday from 1950 to 2099 that is not a working
   * Saturday is added as a holiday. Sunday needs no listing, because it is the calendar's
   * weekend day.
   *
   * The range ends on the last Saturday strictly before the last day of 2099.
   *
   * Both the removal and the addition are lazy, so the holidays and the Saturdays are ordered
   * together in one pass and the result is the ordered set the calendar is built from; nothing
   * orders it again.
   *
   * @param holidays  the holidays declared by the Hungarian rules, in any order
   * @param workDays  the Saturdays that are working days, tested for membership alone
   * @return the holidays of the calendar, sorted and deduplicated
   */
  private def addHungarianSaturdays(
      holidays: IterableOnce[LocalDate],
      workDays: Set[LocalDate]): SortedSet[LocalDate] = {

    val endDate = LocalDate.of(2099, 12, 31)
    val saturdays = Iterator
      .iterate(LocalDate.of(1950, 1, 7))(saturday => saturday.plusDays(7))
      .takeWhile(saturday => saturday.isBefore(endDate))
      .filterNot(saturday => workDays.contains(saturday))
    // remove all saturdays and sundays from the rule-derived holidays, then order once
    ImmutableHolidayCalendar.sortedDates(removeSatSun(holidays) ++ saturdays)
  }

  // dates of published fixings - https://twitter.com/Banxico
  // http://www.banxico.org.mx/SieInternet/consultarDirectorioInternetAction.do?accion=consultarCuadro&idCuadro=CF111&locale=en
  // http://www.gob.mx/cms/uploads/attachment/file/161094/calendario_vacaciones2016.pdf
  // https://comunicacionsocial.diputados.gob.mx/index.php/boletines/la-camara-de-diputados-declaro-el-1-de-octubre-de-cada-seis-a-os-como-dia-de-descanso-obligatorio
  /**
   * Generates the Mexico City holiday calendar, `MXMC`.
   *
   * @return the calendar of Mexico City bank holidays from 1950 to 2099
   */
  def generateMexicoCity(): ImmutableHolidayCalendar =
    satSunCalendar(HolidayCalendarIds.MXMC, (1950 to 2099).iterator.flatMap(mexicoCityYear))

  /**
   * Calculates the Mexico City bank holidays of one year.
   *
   * @param year  the year
   * @return the holidays of that year, before the weekend days are removed
   */
  private def mexicoCityYear(year: Int): List[LocalDate] =
    List.concat(
      List(
        // new year
        date(year, 1, 1),
        // constitution
        first(year, 2).`with`(firstInMonth(MONDAY)),
        // president
        first(year, 3).`with`(firstInMonth(MONDAY)).plusWeeks(2),
        // maundy thursday
        easter(year).minusDays(3),
        // good friday
        easter(year).minusDays(2),
        // labour
        date(year, 5, 1),
        // independence
        date(year, 9, 16)),
      // inauguration day - occurring once in every 6 years (2024, 2030, etc).
      if (year >= 2024 && (year + 4) % 6 == 0) {
        List(date(year, 10, 1))
      } else {
        Nil
      },
      List(
        // dead
        date(year, 11, 2),
        // revolution
        first(year, 11).`with`(firstInMonth(MONDAY)).plusWeeks(2),
        // guadalupe
        date(year, 12, 12),
        // christmas
        date(year, 12, 25)))

  // http://www.planalto.gov.br/ccivil_03/leis/l0662.htm
  // http://www.planalto.gov.br/ccivil_03/Leis/L6802.htm
  // http://www.planalto.gov.br/ccivil_03/leis/2002/L10607.htm
  // https://www.planalto.gov.br/ccivil_03/_ato2023-2026/2023/lei/l14759.htm
  /**
   * Generates the Brazilian bank holiday calendar, `BRBD`.
   *
   * A date is a holiday here only where it is a holiday in Sao Paulo, Rio de Janeiro and
   * Brasilia alike. The base law is from 1949, reworded in 2002.
   *
   * @return the calendar of Brazilian bank holidays from 1950 to 2099
   */
  def generateBrazil(): ImmutableHolidayCalendar = {
    satSunCalendar(HolidayCalendarId.of("BRBD"), (1950 to 2099).iterator.flatMap(brazilYear))
  }

  /**
   * Calculates the Brazilian bank holidays of one year.
   *
   * @param year  the year
   * @return the holidays of that year, before the weekend days are removed
   */
  private def brazilYear(year: Int): List[LocalDate] =
    List.concat(
      List(
        // new year
        date(year, 1, 1),
        // carnival
        easter(year).minusDays(48),
        easter(year).minusDays(47),
        // tiradentes
        date(year, 4, 21),
        // good friday
        easter(year).minusDays(2),
        // labour
        date(year, 5, 1),
        // corpus christi
        easter(year).plusDays(60),
        // independence
        date(year, 9, 7)),
      // Our Lady of Aparecida
      if (year >= 1980) {
        List(date(year, 10, 12))
      } else {
        Nil
      },
      List(
        // dead
        date(year, 11, 2),
        // republic
        date(year, 11, 15)),
      // Dia Nacional de Zumbi e da Consciencia Negra
      if (year >= 2024) {
        List(date(year, 11, 20))
      } else {
        Nil
      },
      // christmas
      List(date(year, 12, 25)))

  // https://www.cnb.cz/en/public/media_service/schedules/media_svatky.html
  /**
   * Generates the Prague holiday calendar, `CZPR`.
   *
   * The dates are fixed - there is no moving of a Sunday to a Monday or similar.
   *
   * @return the calendar of Prague bank holidays from 1950 to 2099
   */
  def generatePrague(): ImmutableHolidayCalendar = {
    satSunCalendar(HolidayCalendarId.of("CZPR"), (1950 to 2099).iterator.flatMap(pragueYear))
  }

  /**
   * Calculates the Prague bank holidays of one year.
   *
   * @param year  the year
   * @return the holidays of that year, before the weekend days are removed
   */
  private def pragueYear(year: Int): List[LocalDate] =
    List.concat(
      // new year
      List(date(year, 1, 1)),
      // good friday
      if (year > 2015) {
        List(easter(year).minusDays(2))
      } else {
        Nil
      },
      List(
        // easter monday
        easter(year).plusDays(1),
        // may day
        date(year, 5, 1),
        // liberation from fascism
        date(year, 5, 8),
        // cyril and methodius
        date(year, 7, 5),
        // jan hus
        date(year, 7, 6),
        // statehood
        date(year, 9, 28),
        // republic
        date(year, 10, 28),
        // freedom and democracy
        date(year, 11, 17),
        // christmas eve
        date(year, 12, 24),
        // christmas
        date(year, 12, 25),
        // boxing
        date(year, 12, 26)))

  /**
   * Obtains a date from a year, a month and a day.
   *
   * @param year  the year
   * @param month  the month, from 1 (January) to 12 (December)
   * @param day  the day of the month
   * @return the date
   */
  private def date(year: Int, month: Int, day: Int): LocalDate =
    LocalDate.of(year, month, day)

  /**
   * Moves a date that falls at a weekend forward to the following Monday.
   *
   * @param date  the date
   * @return the date, or the following Monday where it fell on a Saturday or a Sunday
   */
  private def bumpToMon(date: LocalDate): LocalDate =
    if (date.getDayOfWeek == SATURDAY) {
      date.plusDays(2)
    } else if (date.getDayOfWeek == SUNDAY) {
      date.plusDays(1)
    } else {
      date
    }

  /**
   * Moves a date that falls on a Sunday forward to the following Monday.
   *
   * @param date  the date
   * @return the date, or the following Monday where it fell on a Sunday
   */
  private def bumpSunToMon(date: LocalDate): LocalDate =
    if (date.getDayOfWeek == SUNDAY) {
      date.plusDays(1)
    } else {
      date
    }

  /**
   * Moves a date that falls at a weekend to the nearer working day.
   *
   * @param date  the date
   * @return the date, or the preceding Friday where it fell on a Saturday, or the following
   *   Monday where it fell on a Sunday
   */
  private def bumpToFriOrMon(date: LocalDate): LocalDate =
    if (date.getDayOfWeek == SATURDAY) {
      date.minusDays(1)
    } else if (date.getDayOfWeek == SUNDAY) {
      date.plusDays(1)
    } else {
      date
    }

  /**
   * Calculates the observed Christmas Day, moved to the 27th when it falls at a weekend.
   *
   * @param year  the year
   * @return the date on which Christmas Day is observed
   */
  def christmasBumpedSatSun(year: Int): LocalDate = {
    val base = LocalDate.of(year, 12, 25)
    if (base.getDayOfWeek == SATURDAY || base.getDayOfWeek == SUNDAY) {
      LocalDate.of(year, 12, 27)
    } else {
      base
    }
  }

  /**
   * Calculates the observed Christmas Day, moved to the 26th when it falls on a Sunday.
   *
   * @param year  the year
   * @return the date on which Christmas Day is observed
   */
  private def christmasBumpedSun(year: Int): LocalDate = {
    val base = LocalDate.of(year, 12, 25)
    if (base.getDayOfWeek == SUNDAY) {
      LocalDate.of(year, 12, 26)
    } else {
      base
    }
  }

  /**
   * Calculates the observed Boxing Day, moved to the 28th when it falls at a weekend.
   *
   * @param year  the year
   * @return the date on which Boxing Day is observed
   */
  def boxingDayBumpedSatSun(year: Int): LocalDate = {
    val base = LocalDate.of(year, 12, 26)
    if (base.getDayOfWeek == SATURDAY || base.getDayOfWeek == SUNDAY) {
      LocalDate.of(year, 12, 28)
    } else {
      base
    }
  }

  /**
   * Calculates the observed Boxing Day, moved to the 27th when it falls on a Monday - which is
   * to say when Christmas Day fell on a Sunday and took the Monday.
   *
   * @param year  the year
   * @return the date on which Boxing Day is observed
   */
  private def boxingDayBumpedSun(year: Int): LocalDate = {
    val base = LocalDate.of(year, 12, 26)
    if (base.getDayOfWeek == MONDAY) {
      LocalDate.of(year, 12, 27)
    } else {
      base
    }
  }

  /**
   * Obtains the first day of a month.
   *
   * @param year  the year
   * @param month  the month, from 1 (January) to 12 (December)
   * @return the first day of that month
   */
  private def first(year: Int, month: Int): LocalDate =
    LocalDate.of(year, month, 1)

  /**
   * Removes the dates that fall at a weekend.
   *
   * A holiday that falls on a Saturday or a Sunday is not observed, and the calendars here
   * treat the weekend as a weekend rather than as a holiday, so such a date is dropped rather
   * than carried. What is left is returned in the order of the argument.
   *
   * The filtering is lazy and the dates are not collected, because the caller is about to
   * collect them into the ordered set of the calendar - see [[weekdaysOnly]] - and materialising
   * a whole calendar's dates in between would allocate thousands of them for nothing.
   *
   * @param dates  the dates
   * @return an iterator over the dates that fall on a weekday
   */
  private def removeSatSun(dates: IterableOnce[LocalDate]): Iterator[LocalDate] =
    dates.iterator.filterNot(d => isWeekend(d))

  /**
   * Calculates Easter Sunday of a year, by Delambre's algorithm.
   *
   * This is the anonymous Gregorian computus: every division is integer division, and the
   * intermediate quantities carry the single-letter names the algorithm is published with
   * rather than descriptive ones, because they have no meaning to describe.
   *
   * @param year  the year
   * @return Easter Sunday of that year
   */
  def easter(year: Int): LocalDate = {
    val a = year % 19
    val b = year / 100
    val c = year % 100
    val d = b / 4
    val e = b % 4
    val f = (b + 8) / 25
    val g = (b - f + 1) / 3
    val h = (19 * a + b - d - g + 15) % 30
    val i = c / 4
    val k = c % 4
    val l = (32 + 2 * e + 2 * i - h - k) % 7
    val m = (a + 11 * h + 22 * l) / 451
    val month = (h + l - 7 * m + 114) / 31
    val day = ((h + l - 7 * m + 114) % 31) + 1
    LocalDate.of(year, month, day)
  }

}
