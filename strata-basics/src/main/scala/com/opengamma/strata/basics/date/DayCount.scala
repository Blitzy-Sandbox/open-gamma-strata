/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate
import java.util.Locale

import scala.annotation.tailrec
import scala.util.matching.Regex

import cats.Hash
import cats.Order
import cats.Show
import cats.data.EitherNec
import cats.data.NonEmptyChain
import cats.data.NonEmptyList

import io.circe.Codec
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.schedule.Frequency
import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.Failure

/**
 * A convention defining how to calculate fractions of a year.
 *
 * The purpose of this convention is to define how to convert dates into numeric year
 * fractions, which is what allows interest accrued over a stretch of time to be calculated.
 * Given two dates, [[yearFraction]] answers with the fraction of a year between them
 * according to the rule this convention embodies, and [[days]] answers with the numerator of
 * that division - the count of days the rule attributes to the period, which for a `30/360`
 * convention is not the actual number of days at all.
 *
 * ===A closed family===
 *
 * The twenty-one standard conventions are declared in the companion of this type, and every
 * one of them is a `case object`: the type is `sealed`, its constructor is visible only inside
 * this package, and a `match` over a day count is therefore checked for exhaustiveness by the
 * compiler. They are reached in three ways, all of which yield the same objects:
 *
 * {{{
 * DayCount.ACT_365F              // the member itself
 * DayCounts.ACT_365F             // the identifier the ported library used
 * DayCount.parse("ACT/365")      // text, leniently resolved
 * }}}
 *
 * The one member that is not a singleton is [[DayCount.Bus252]], the `Bus/252` convention used
 * in Brazil, which counts business days against a holiday calendar and so exists once per
 * calendar rather than once for the family. It is built by [[DayCount.ofBus252]] and carries
 * the '''resolved''' calendar, which keeps [[yearFraction]] and [[days]] pure functions of
 * their arguments.
 *
 * ===Order of the arguments===
 *
 * [[yearFraction]] and [[days]] require their dates in time-line order and refuse a pair that
 * is not, because a negative accrual is a mistake at the call site rather than a result. Where
 * the direction of a period is genuinely unknown, [[relativeYearFraction]] answers for either
 * order, negating its result when the second date precedes the first. This is the split the
 * implementation being ported made, and both halves of it are preserved here.
 *
 * ===Schedule information===
 *
 * Four of the twenty-one conventions cannot answer from two dates alone: `Act/Act ICMA` and
 * `Act/365L` need the schedule the period belongs to, `30U/360` needs to know whether the
 * end-of-month convention applies, and `30E/360 ISDA` needs the maturity date of the schedule.
 * Those facts are supplied by a [[DayCount.ScheduleInfo]], and the two-argument overloads pass
 * [[DayCount.ScheduleInfo.simple]], which carries none of them. Asking one of those four
 * conventions for a year fraction without the information it reads is a breach of this
 * contract and is refused, exactly as it was refused before; see
 * [[DayCount.ScheduleInfo]] for why that is a refusal rather than a reported failure.
 *
 * ===What this replaces===
 *
 * Four Java types fold into this file: the interface, its constants holder, the enum holding
 * the standard implementations and the name lookup that created `Bus/252` conventions on
 * demand. The registry that discovered implementations while the program ran is gone, together
 * with the configuration resource it read, the caches it held and the Java serialization and
 * string-conversion annotations the types carried. What that resource ''declared'' survives in
 * full: the two groups of external names and the ordered lenient rewrites are transcribed into
 * the companion as Scala data, so text that resolved before resolves now.
 *
 * Every member is immutable and safe to share between threads.
 *
 * @param name  the unique name of the convention, which is its identity in text and on the wire
 */
sealed abstract class DayCount private[date] (val name: String) extends Named {

  /**
   * Gets the year fraction between the specified dates.
   *
   * Given two dates, this returns the fraction of a year between them according to this
   * convention. The dates must be in time-line order.
   *
   * This uses [[DayCount.ScheduleInfo.simple]], which has the end-of-month convention set to
   * true and carries no schedule dates or frequency. The four conventions that read schedule
   * information therefore refuse this overload; the three-argument form is the one to call for
   * them.
   *
   * @param firstDate  the first date
   * @param secondDate  the second date, on or after the first date
   * @return the year fraction, zero or greater
   * @throws IllegalArgumentException if the dates are not in time-line order, or if this
   *   convention requires schedule information that [[DayCount.ScheduleInfo.simple]] does not
   *   carry
   */
  def yearFraction(firstDate: LocalDate, secondDate: LocalDate): Double =
    yearFraction(firstDate, secondDate, DayCount.ScheduleInfo.simple)

  /**
   * Gets the year fraction between the specified dates.
   *
   * Given two dates, this returns the fraction of a year between them according to this
   * convention. The dates must be in time-line order.
   *
   * @param firstDate  the first date
   * @param secondDate  the second date, on or after the first date
   * @param scheduleInfo  the schedule information, read only by the four conventions that
   *   need it
   * @return the year fraction, zero or greater
   * @throws IllegalArgumentException if the dates are not in time-line order, or if this
   *   convention requires schedule information the argument does not carry
   */
  def yearFraction(firstDate: LocalDate, secondDate: LocalDate, scheduleInfo: DayCount.ScheduleInfo): Double = {
    DayCount.checkInOrder(firstDate, secondDate)
    calculateYearFraction(firstDate, secondDate, scheduleInfo)
  }

  /**
   * Gets the relative year fraction between the specified dates.
   *
   * Given two dates, this returns the fraction of a year between them according to this
   * convention. Unlike [[yearFraction]] the dates may be supplied in either order, and the
   * result is negative when the first date is after the second.
   *
   * This uses [[DayCount.ScheduleInfo.simple]], with the consequence described on
   * [[yearFraction]].
   *
   * @param firstDate  the first date
   * @param secondDate  the second date, which may be before the first date
   * @return the year fraction, which may be negative
   * @throws IllegalArgumentException if this convention requires schedule information that
   *   [[DayCount.ScheduleInfo.simple]] does not carry
   */
  def relativeYearFraction(firstDate: LocalDate, secondDate: LocalDate): Double =
    relativeYearFraction(firstDate, secondDate, DayCount.ScheduleInfo.simple)

  /**
   * Gets the relative year fraction between the specified dates.
   *
   * Given two dates, this returns the fraction of a year between them according to this
   * convention. Unlike [[yearFraction]] the dates may be supplied in either order, and the
   * result is negative when the first date is after the second.
   *
   * Dates out of order are the point of this method, so the order check of [[yearFraction]] is
   * deliberately bypassed: the calculation is reached directly, with the arguments swapped and
   * the result negated. This is what the implementation being ported did, and it is why
   * `relativeYearFraction(second, first)` answers where `yearFraction(second, first)` refuses.
   *
   * @param firstDate  the first date
   * @param secondDate  the second date, which may be before the first date
   * @param scheduleInfo  the schedule information, read only by the four conventions that
   *   need it
   * @return the year fraction, which may be negative
   * @throws IllegalArgumentException if this convention requires schedule information the
   *   argument does not carry
   */
  def relativeYearFraction(
      firstDate: LocalDate,
      secondDate: LocalDate,
      scheduleInfo: DayCount.ScheduleInfo): Double =

    if (secondDate.isBefore(firstDate)) {
      -calculateYearFraction(secondDate, firstDate, scheduleInfo)
    } else {
      calculateYearFraction(firstDate, secondDate, scheduleInfo)
    }

  /**
   * Calculates the number of days between the specified dates using the rules of this day
   * count.
   *
   * A day count is typically defined as a count of days divided by an estimate of the length
   * of a year, and this method returns the count of days - the numerator of that division. The
   * `Act/Act` conventions return the actual number of days between the two dates, while
   * `30/360 ISDA` returns a value based on months of thirty days, and `1/1` returns one
   * whatever the dates.
   *
   * @param firstDate  the first date
   * @param secondDate  the second date, on or after the first date
   * @return the number of days, as determined by this day count
   * @throws IllegalArgumentException if the dates are not in time-line order
   */
  def days(firstDate: LocalDate, secondDate: LocalDate): Int = {
    DayCount.checkInOrder(firstDate, secondDate)
    calculateDays(firstDate, secondDate)
  }

  /**
   * Calculates the year fraction from arguments already known to be in order.
   *
   * This is where each convention states its rule, and it is reached only through
   * [[yearFraction]], which checks the order of the dates, or through
   * [[relativeYearFraction]], which establishes it by swapping them. Splitting the check from
   * the calculation this way is what lets the relative form answer for a reversed pair without
   * the check rejecting it first - the same two-layer split the implementation being ported
   * used.
   *
   * Visible throughout this package rather than to the subtype alone, because `30U/360`
   * delegates to the calculation of '''another''' member of the family depending on the
   * end-of-month flag, which Scala's unqualified `protected` would not permit.
   *
   * @param firstDate  the first date
   * @param secondDate  the second date, on or after the first date
   * @param scheduleInfo  the schedule information
   * @return the year fraction
   * @throws IllegalArgumentException if this convention requires schedule information the
   *   argument does not carry
   */
  protected[date] def calculateYearFraction(
      firstDate: LocalDate,
      secondDate: LocalDate,
      scheduleInfo: DayCount.ScheduleInfo): Double

  /**
   * Calculates the number of days from arguments already known to be in order.
   *
   * The counterpart of [[calculateYearFraction]], reached only through [[days]].
   *
   * @param firstDate  the first date
   * @param secondDate  the second date, on or after the first date
   * @return the number of days, as determined by this day count
   */
  protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int

  /**
   * Renders this convention as its unique name.
   *
   * The name is the only text form of a day count: it is what the `Show` instance produces,
   * what the codec writes, and what [[DayCount.parse]] reads back.
   *
   * @return the unique name
   */
  override def toString: String = name
}

/**
 * The twenty-one standard day counts, the `Bus/252` family, and the name lookup, typeclass
 * instances and JSON support of the whole family.
 *
 * The members are declared in the order the enum being ported declared them, and [[values]]
 * preserves that order. Each is a `case object` whose calculation is transcribed statement for
 * statement from the implementation being ported, because the numbers these conventions produce
 * are the contract: a rewritten formula that is algebraically equal can still differ in the last
 * bits of a `Double`, and those bits are compared against a captured Java baseline.
 */
object DayCount {

  /** The prefix that identifies a business-day convention bound to a holiday calendar. */
  private val Bus252Prefix: String = "Bus/252 "

  /** The number of characters of [[Bus252Prefix]], matched case-insensitively when parsing. */
  private val Bus252PrefixLength: Int = Bus252Prefix.length

  /** The number of business days a `Bus/252` convention treats as a year. */
  private val Bus252DaysPerYear: Double = 252d

  /** The message reporting dates that are not in time-line order. */
  private val DatesOutOfOrderMessage: String = "Dates must be in time-line order"

  /** The message reporting that the end date of the schedule was needed and is absent. */
  private val ScheduleEndDateRequired: String = "The end date of the schedule is required"

  /** The message reporting that the end date of the schedule period was needed and is absent. */
  private val PeriodEndDateRequired: String = "The end date of the schedule period is required"

  /** The message reporting that the frequency of the schedule was needed and is absent. */
  private val FrequencyRequired: String = "The frequency of the schedule is required"

  //-------------------------------------------------------------------------
  /**
   * Information about the schedule that some day counts need in order to calculate.
   *
   * Four of the twenty-one standard conventions read this: `Act/Act ICMA` reads the end date of
   * the schedule, the end date of the schedule period containing the first date, the frequency
   * and the end-of-month flag; `Act/365L` reads the period end date and the frequency; `30U/360`
   * reads the end-of-month flag; and `30E/360 ISDA` reads the end date of the schedule. The other
   * seventeen ignore it entirely.
   *
   * ===Total accessors===
   *
   * Every accessor here is '''total'''. The interface being ported declared four of them to throw
   * `UnsupportedOperationException` by default, so that an implementation supplying only some of
   * the facts could leave the rest to raise; here each of those four answers with an `Option` and
   * the default is `None`, which is the same statement of "this schedule does not know" made as a
   * value. An implementation therefore overrides only what it knows, and no implementation has to
   * raise.
   *
   * Note what this does '''not''' change. A day count that reads a fact which is absent still
   * refuses to produce a number, and it refuses by raising `IllegalArgumentException` through
   * `ArgCheck` rather than by returning a failure. The reason is the classification the port
   * applies throughout: a failure that depends on the ''data'' of the arguments is reported as a
   * value, while a caller handing a convention a schedule that cannot answer what that convention
   * is defined in terms of has broken the contract of the call, which is what the original
   * `UnsupportedOperationException` said as well. Keeping it a refusal is also what lets
   * [[DayCount.yearFraction]] return a plain `Double`, so that arithmetic over year fractions -
   * the overwhelmingly common case, and every case for seventeen of the members - needs no error
   * channel at all.
   *
   * The schedule type of this library implements this trait with `Some` values throughout, so
   * calculating against a real schedule is the case where nothing is absent.
   */
  trait ScheduleInfo {

    /**
     * Gets the start date of the schedule.
     *
     * The first date of the schedule, adjusted for business days where the schedule adjusts.
     * No standard day count reads it; it is part of this contract because the interface being
     * ported declared it and an implementation may be asked for it by code outside this library.
     *
     * @return the start date of the schedule, or `None` where the schedule is unknown
     */
    def startDate: Option[LocalDate] = None

    /**
     * Gets the end date of the schedule.
     *
     * The last date of the schedule - the maturity date - adjusted for business days where the
     * schedule adjusts. Read by `Act/Act ICMA` and by `30E/360 ISDA`.
     *
     * @return the end date of the schedule, or `None` where the schedule is unknown
     */
    def endDate: Option[LocalDate] = None

    /**
     * Gets the end date of the schedule period containing the specified date.
     *
     * This is the next coupon date as seen from the date supplied. Read by `Act/Act ICMA` and by
     * `Act/365L`.
     *
     * A schedule answers `None` for a date that lies in none of its periods, where the interface
     * being ported raised: a date outside the schedule is data rather than a broken call, and the
     * day count that reads this refuses on its own behalf if it cannot proceed.
     *
     * @param date  the date to find the period end date for
     * @return the end date of the period containing the date, or `None` where there is none
     */
    def periodEndDate(date: LocalDate): Option[LocalDate] = None

    /**
     * Gets the periodic frequency of the schedule.
     *
     * Read by `Act/Act ICMA`, which divides by it, and by `Act/365L`, which asks only whether it
     * is annual.
     *
     * @return the periodic frequency of the schedule, or `None` where the schedule is unknown
     */
    def frequency: Option[Frequency] = None

    /**
     * Checks whether the end-of-month convention is in use.
     *
     * Read by `30U/360`, which chooses between two day-of-month rules by it, and by
     * `Act/Act ICMA`, which rolls its nominal periods to the end of the month when it holds.
     *
     * This is the one accessor that is not optional, and it defaults to `true`, both of which
     * follow the interface being ported.
     *
     * @return true if the end-of-month convention is in use
     */
    def isEndOfMonthConvention: Boolean = true
  }

  /**
   * The schedule information that carries nothing, used by the two-argument overloads.
   *
   * Every optional accessor answers `None` and the end-of-month convention is in use, which is
   * exactly the all-defaults instance the constants holder being ported published for the same
   * purpose. Seventeen of the twenty-one conventions calculate against it without reading it at
   * all; the four that read something refuse, which is what makes
   * `DayCounts.ACT_ACT_ICMA.yearFraction(a, b)` a call that cannot succeed - as it could not
   * before.
   */
  object ScheduleInfo {

    /** The instance carrying no schedule facts, with the end-of-month convention in use. */
    val simple: ScheduleInfo = new ScheduleInfo {}
  }

  //-------------------------------------------------------------------------
  /**
   * Checks that two dates are in time-line order, refusing the call when they are not.
   *
   * Shared by [[DayCount.yearFraction]] and [[DayCount.days]], which is where the implementation
   * being ported made the same check with the same message. Equal dates pass: a period of no
   * length has a year fraction, and it is zero for every convention but `1/1`.
   *
   * @param firstDate  the first date
   * @param secondDate  the second date, expected on or after the first date
   * @throws IllegalArgumentException if the second date is before the first
   */
  private def checkInOrder(firstDate: LocalDate, secondDate: LocalDate): Unit =
    ArgCheck.isTrue(!secondDate.isBefore(firstDate), DatesOutOfOrderMessage)

  /**
   * Extracts a schedule fact a convention reads, refusing the call when it is absent.
   *
   * This is the single point where the absence of schedule information becomes the refusal
   * described on [[ScheduleInfo]], and it is reached only from the four conventions that read
   * something. The value is known to be present when it is taken, because the check above it
   * raises otherwise.
   *
   * @param value  the fact, where the schedule carries it
   * @param message  the message naming what is missing, matching the text the ported interface
   *   raised
   * @tparam A  the type of the fact
   * @return the fact
   * @throws IllegalArgumentException if the fact is absent
   */
  private def required[A](value: Option[A], message: String): A = {
    ArgCheck.isTrue(value.isDefined, message)
    value.get
  }

  /**
   * The number of events per year of a frequency, refusing a frequency that has no exact number.
   *
   * `Act/Act ICMA` divides by this, and the frequency reaching it is the frequency of the
   * schedule being accrued over, so a frequency such as `P5M` - which no whole number of periods
   * fills a year with - is a schedule that cannot be accrued by this convention. The frequency
   * type reports that as a failure, because for its own callers it depends on data; here it is a
   * breach of this convention's contract, so it is raised with the message the frequency
   * produced, which is what the ported implementation did.
   *
   * @param freq  the frequency of the schedule
   * @return the number of events per year
   * @throws IllegalArgumentException if the frequency has no exact number of events per year
   */
  private def eventsPerYearOf(freq: Frequency): Int =
    freq.eventsPerYear match {
      case Right(events) => events
      case Left(failure) => required[Int](None, failure.message)
    }

  /**
   * The actual number of days between two dates, which is the day count of every `Act/` rule.
   *
   * Narrowed to an `Int` exactly, as the implementation being ported did: the difference of two
   * dates this library can represent exceeds an `Int` only for dates millions of years apart,
   * and an overflow is raised rather than silently truncated.
   *
   * @param firstDate  the first date
   * @param secondDate  the second date, on or after the first date
   * @return the actual number of days between the dates
   */
  private def actualDayCount(firstDate: LocalDate, secondDate: LocalDate): Int =
    Math.toIntExact(LocalDateUtils.daysBetween(firstDate, secondDate))

  /**
   * The day count of the `30/360` family, given the two dates with their days-of-month already
   * adjusted by the rule of the particular convention.
   *
   * Months are thirty days and years are three hundred and sixty, so the count is
   * `360 * deltaYear + 30 * deltaMonth + deltaDay`. This is the ported helper, which divides by
   * nothing: the conventions that want a fraction divide the result themselves.
   *
   * @param y1  the year of the first date
   * @param m1  the month of the first date
   * @param d1  the adjusted day-of-month of the first date
   * @param y2  the year of the second date
   * @param m2  the month of the second date, which one convention increments
   * @param d2  the adjusted day-of-month of the second date
   * @return the number of days between the two adjusted dates
   */
  private def thirty360Days(y1: Int, m1: Int, d1: Int, y2: Int, m2: Int, d2: Int): Int =
    360 * (y2 - y1) + 30 * (m2 - m1) + (d2 - d1)

  /**
   * Checks whether a date is the last day of February, in a leap year or not.
   *
   * The end-of-February rules of `30U/360 EOM`, `30/360 PSA` and `30E/360 ISDA` all turn on this.
   *
   * @param date  the date to test
   * @return true if the date is the last day of February
   */
  private def lastDayOfFebruary(date: LocalDate): Boolean =
    date.getMonthValue == 2 && date.getDayOfMonth == date.lengthOfMonth

  /**
   * Counts the leap days in the half-open period between two dates, which the `NL/` rules remove.
   *
   * The search walks from the first leap day after the first date to the first one after the
   * second, counting those that do not pass it; a leap day on the second date itself '''is'''
   * counted, which is the comparison the ported loop made. Written as a tail-recursive function,
   * so no mutable counter is needed.
   *
   * @param firstDate  the first date
   * @param secondDate  the second date, on or after the first date
   * @return the number of occurrences of February 29 in the period
   */
  private def numberOfLeapDays(firstDate: LocalDate, secondDate: LocalDate): Int = {
    @tailrec
    def count(candidate: LocalDate, found: Int): Int =
      if (candidate.isAfter(secondDate)) found
      else count(DateAdjusters.nextLeapDay(candidate), found + 1)

    count(DateAdjusters.nextLeapDay(firstDate), 0)
  }

  //-------------------------------------------------------------------------
  /**
   * The '1/1' day count, which always returns a day count of 1.
   *
   * The result is always one, whatever the dates. Also known as 'One/One'; defined by the 2006
   * ISDA definitions 4.16a.
   */
  case object ONE_ONE extends DayCount("1/1") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double = 1d

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int = 1
  }

  /**
   * The 'Act/Act ISDA' day count, which divides the actual number of days in a leap year by 366
   * and the actual number of days in a standard year by 365.
   *
   * The result is calculated in two parts. The actual number of days in the requested period that
   * fall in a leap year is divided by 366, the actual number that fall in a standard year is
   * divided by 365, and the result is the sum of the two. The first day of the period is included
   * and the last is excluded.
   *
   * Also known as 'Actual/Actual'; defined by the 2006 ISDA definitions 4.16b.
   */
  case object ACT_ACT_ISDA extends DayCount("Act/Act ISDA") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double = {

      val y1 = firstDate.getYear
      val y2 = secondDate.getYear
      val firstYearLength = firstDate.lengthOfYear.toDouble
      if (y1 == y2) {
        val actualDays = (LocalDateUtils.doy(secondDate) - LocalDateUtils.doy(firstDate)).toDouble
        actualDays / firstYearLength
      } else {
        val firstRemainderOfYear = firstYearLength - LocalDateUtils.doy(firstDate).toDouble + 1d
        val secondRemainderOfYear = (LocalDateUtils.doy(secondDate) - 1).toDouble
        val secondYearLength = secondDate.lengthOfYear.toDouble
        firstRemainderOfYear / firstYearLength +
          secondRemainderOfYear / secondYearLength +
          (y2 - y1 - 1).toDouble
      }
    }

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int =
      actualDayCount(firstDate, secondDate)
  }

  /**
   * The 'Act/Act ICMA' day count, which divides the actual number of days by the actual number of
   * days in the coupon period multiplied by the frequency.
   *
   * The result is calculated as follows. First, the underlying schedule period is obtained,
   * treating the first date as the start of the schedule period. Second, if the period is a stub
   * then nominal regular periods are created matching the schedule frequency, working forwards or
   * backwards from the known regular schedule date, with an end-of-month flag used to handle
   * month-ends; if the period is not a stub then the schedule period is treated as a nominal
   * period. Third, the result is the sum of a calculation for each nominal period: the actual days
   * between the first and second date are allocated to the matching nominal period, and each
   * calculation divides the actual number of days in the nominal period - which can be zero in the
   * case of a long stub - by the length of the nominal period multiplied by the frequency. The
   * first day of the period is included and the last is excluded.
   *
   * Because the nominal periods are determined ignoring business day adjustments, this day count
   * is recommended for use by bonds rather than swaps.
   *
   * The two-argument `yearFraction(firstDate, secondDate)` cannot be used with this convention,
   * because schedule information is required.
   *
   * Also known as 'Actual/Actual ICMA' or 'Actual/Actual (Bond)'; defined by the 2006 ISDA
   * definitions 4.16c and ICMA rule 251.1(iii) and 251.3 as later clarified by ISDA 'EMU and
   * market conventions'.
   */
  case object ACT_ACT_ICMA extends DayCount("Act/Act ICMA") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double =

      // avoid using the schedule information in this case
      if (firstDate == secondDate) {
        0d
      } else {
        // the calculation is based on the schedule period, the first date being assumed to be the
        // start of that period
        val scheduleEndDate = required(scheduleInfo.endDate, ScheduleEndDateRequired)
        val nextCouponDate = required(scheduleInfo.periodEndDate(firstDate), PeriodEndDateRequired)
        val freq = required(scheduleInfo.frequency, FrequencyRequired)
        val eom = scheduleInfo.isEndOfMonthConvention
        if (nextCouponDate == scheduleEndDate) {
          // the final period, which also covers single period schedules
          finalPeriod(firstDate, secondDate, freq, eom)
        } else {
          // the initial period, and every other case where the previous coupon date has to be
          // determined, whether that date is real or nominal
          initPeriod(firstDate, secondDate, nextCouponDate, freq, eom)
        }
      }

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int =
      actualDayCount(firstDate, secondDate)

    /**
     * Sums the calculation over nominal periods counted backwards from the coupon date.
     *
     * The nominal periods are stepped back one frequency at a time until one begins on or before
     * the start date, and each step contributes the days of the requested period that fall inside
     * it. The accumulation is a parameter of a tail-recursive function rather than a mutable
     * local, and the additions happen in the order the ported loop performed them, which keeps the
     * result identical bit for bit.
     *
     * @param startDate  the start of the period being measured
     * @param endDate  the end of the period being measured
     * @param couponDate  the known regular schedule date the nominal periods are counted from
     * @param freq  the frequency of the schedule
     * @param eom  whether the end-of-month convention is in use
     * @return the year fraction of the period
     */
    private def initPeriod(
        startDate: LocalDate,
        endDate: LocalDate,
        couponDate: LocalDate,
        freq: Frequency,
        eom: Boolean): Double = {

      @tailrec
      def accrue(currentNominal: LocalDate, prevNominal: LocalDate, accrued: Double): Double =
        if (prevNominal.isAfter(startDate)) {
          accrue(
            prevNominal,
            endOfMonth(couponDate, freq.subtractFrom(prevNominal), eom),
            accrued + calc(prevNominal, currentNominal, startDate, endDate, freq))
        } else {
          accrued + calc(prevNominal, currentNominal, startDate, endDate, freq)
        }

      accrue(couponDate, endOfMonth(couponDate, freq.subtractFrom(couponDate), eom), 0d)
    }

    /**
     * Sums the calculation over nominal periods counted forwards from the coupon date.
     *
     * The mirror of [[initPeriod]], used when the period being measured is the final one of the
     * schedule: the nominal periods step forward one frequency at a time until one ends on or
     * after the end date.
     *
     * @param couponDate  the known regular schedule date the nominal periods are counted from,
     *   which is also the start of the period being measured
     * @param endDate  the end of the period being measured
     * @param freq  the frequency of the schedule
     * @param eom  whether the end-of-month convention is in use
     * @return the year fraction of the period
     */
    private def finalPeriod(couponDate: LocalDate, endDate: LocalDate, freq: Frequency, eom: Boolean): Double = {
      @tailrec
      def accrue(curNominal: LocalDate, nextNominal: LocalDate, accrued: Double): Double =
        if (nextNominal.isBefore(endDate)) {
          accrue(
            nextNominal,
            endOfMonth(couponDate, freq.addTo(nextNominal), eom),
            accrued + calc(curNominal, nextNominal, curNominal, endDate, freq))
        } else {
          accrued + calc(curNominal, nextNominal, curNominal, endDate, freq)
        }

      accrue(couponDate, endOfMonth(couponDate, freq.addTo(couponDate), eom), 0d)
    }

    /**
     * Applies the end-of-month convention to a nominal date.
     *
     * Where the convention is in use and the date the nominal periods are counted from is the last
     * day of its month, each nominal date is moved to the last day of its own month; otherwise the
     * nominal date stands as the frequency produced it.
     *
     * @param base  the date the nominal periods are counted from
     * @param candidate  the nominal date the frequency produced
     * @param eom  whether the end-of-month convention is in use
     * @return the nominal date, rolled to the end of its month where the convention applies
     */
    private def endOfMonth(base: LocalDate, candidate: LocalDate, eom: Boolean): LocalDate =
      if (eom && base.getDayOfMonth == base.lengthOfMonth) {
        candidate.withDayOfMonth(candidate.lengthOfMonth)
      } else {
        candidate
      }

    /**
     * The contribution of one nominal period to the year fraction.
     *
     * The days of the requested period that fall inside the nominal period are divided by the
     * length of the nominal period multiplied by the number of events per year. A nominal period
     * that ends before the requested period starts contributes nothing, which is the case a long
     * stub produces.
     *
     * @param prevNominal  the start of the nominal period
     * @param curNominal  the end of the nominal period
     * @param start  the start of the period being measured
     * @param end  the end of the period being measured
     * @param freq  the frequency of the schedule
     * @return the contribution of the nominal period
     */
    private def calc(
        prevNominal: LocalDate,
        curNominal: LocalDate,
        start: LocalDate,
        end: LocalDate,
        freq: Frequency): Double =

      if (end.isAfter(prevNominal)) {
        val curNominalEpochDay = curNominal.toEpochDay
        val prevNominalEpochDay = prevNominal.toEpochDay
        val startEpochDay = start.toEpochDay
        val endEpochDay = end.toEpochDay
        val periodDays = (curNominalEpochDay - prevNominalEpochDay).toDouble
        val actualDays =
          (Math.min(endEpochDay, curNominalEpochDay) - Math.max(startEpochDay, prevNominalEpochDay)).toDouble
        actualDays / (eventsPerYearOf(freq).toDouble * periodDays)
      } else {
        0d
      }
  }

  /**
   * The 'Act/Act AFB' day count, which divides the actual number of days by 366 if a leap day is
   * contained, or by 365 if not, with additional rules for periods over one year.
   *
   * The result is a simple division. The numerator is the actual number of days in the requested
   * period and the denominator is 366 if the period contains February 29, or 365 if it does not.
   * The first day of the period is included and the last is excluded.
   *
   * Also known as 'Actual/Actual AFB' or 'Actual/Actual (Euro)'; defined by the Association
   * Francaise des Banques in September 1994 as 'Base Exact/Exact'.
   *
   * This library implements the day count from the original French documentation rather than from
   * the later ISDA clarification, whose roll-back rule gives one day two days of interest and the
   * next none. The interpretation taken rolls a period ending on the ''29th'' of February back to
   * the 28th, or to the 29th in a leap year, which gives:
   *
   * {{{
   * 2004-02-28 to 2008-02-27 = 3 + 365 / 366
   * 2004-02-28 to 2008-02-28 = 4
   * 2004-02-28 to 2008-02-29 = 4 + 1 / 366
   * }}}
   */
  case object ACT_ACT_AFB extends DayCount("Act/Act AFB") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double = {

      // No initial check of a period of one year or less is needed under this interpretation of
      // the end-of-February rule: whole years are counted back from the second date, and the
      // remainder - the first date up to but excluding the resulting date - is divided by the
      // length of the year the remainder falls in.
      @tailrec
      def accrue(years: Int, end: LocalDate, start: LocalDate): Double =
        if (start.isBefore(firstDate)) {
          val actualDays = LocalDateUtils.daysBetween(firstDate, end).toDouble
          val nextLeap = DateAdjusters.nextOrSameLeapDay(firstDate)
          years.toDouble + (actualDays / (if (nextLeap.isBefore(end)) 366d else 365d))
        } else {
          accrue(years + 1, start, secondDate.minusYears((years + 2).toLong))
        }

      accrue(0, secondDate, secondDate.minusYears(1L))
    }

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int =
      actualDayCount(firstDate, secondDate)
  }

  /**
   * The 'Act/Act Year' day count, which divides the actual number of days by the number of days in
   * the year from the start date.
   *
   * The result is calculated in two parts - a number of whole years and the remaining part. If the
   * period is over one year, whole years are added to the start date to reduce the remaining period
   * to less than a year; where the start date is February 29 the last valid day in February is
   * chosen each time a year is added. The remaining period is then a simple division: the actual
   * number of days in it over the actual number of days in the year from the adjusted start date.
   * The first day of the period is included and the last is excluded.
   *
   * For the period 2016-01-10 to 2016-01-20 the numerator is 10, as there are ten days between the
   * dates, and the denominator is 366, as there are 366 days between 2016-01-10 and 2017-01-10.
   *
   * This is a variation of 'Act/Act ICMA': calling that convention with a yearly frequency, a next
   * coupon date one year after the start date and the end-of-month flag unset gives the same result
   * for periods of less than a year.
   */
  case object ACT_ACT_YEAR extends DayCount("Act/Act Year") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double = {

      @tailrec
      def accrue(startDate: LocalDate, yearsAdded: Int): Double =
        if (secondDate.compareTo(startDate.plusYears(1L)) > 0) {
          accrue(firstDate.plusYears((yearsAdded + 1).toLong), yearsAdded + 1)
        } else {
          val actualDays = LocalDateUtils.daysBetween(startDate, secondDate).toDouble
          val actualDaysInYear = LocalDateUtils.daysBetween(startDate, startDate.plusYears(1L)).toDouble
          yearsAdded.toDouble + (actualDays / actualDaysInYear)
        }

      accrue(firstDate, 0)
    }

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int =
      actualDayCount(firstDate, secondDate)
  }

  /**
   * The 'Act/365 Actual' day count, which divides the actual number of days by 366 if a leap day is
   * contained, or by 365 if not.
   *
   * The result is a simple division. The numerator is the actual number of days in the requested
   * period and the denominator is 366 if the period contains February 29, or 365 if it does not.
   * The first day of the period is excluded and the last is included.
   *
   * Also known as 'Act/365A'.
   */
  case object ACT_365_ACTUAL extends DayCount("Act/365 Actual") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double = {

      val actualDays = LocalDateUtils.daysBetween(firstDate, secondDate)
      val nextLeap = DateAdjusters.nextLeapDay(firstDate)
      actualDays.toDouble / (if (nextLeap.isAfter(secondDate)) 365d else 366d)
    }

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int =
      actualDayCount(firstDate, secondDate)
  }

  /**
   * The 'Act/365L' day count, which divides the actual number of days by 365 or 366.
   *
   * The result is a simple division. The numerator is the actual number of days in the requested
   * period and the denominator is determined by examining the frequency and the period end date -
   * the date of the next coupon. If the frequency is annual then the denominator is 366 if the
   * period contains February 29, or 365 if it does not, with the first day of the period excluded
   * and the last included. If the frequency is not annual, the denominator is 366 if the period end
   * date is in a leap year, or 365 if it is not.
   *
   * The two-argument `yearFraction(firstDate, secondDate)` cannot be used with this convention,
   * because schedule information is required.
   *
   * Also known as 'Act/365 Leap year'; defined by the 2006 ISDA definitions 4.16i and ICMA rule
   * 251.1(i) part 2 as later clarified by ICMA and the Swiss Exchange.
   */
  case object ACT_365L extends DayCount("Act/365L") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double = {

      val actualDays = LocalDateUtils.daysBetween(firstDate, secondDate)
      // avoid using the schedule information in this case
      if (firstDate == secondDate) {
        0d
      } else {
        // the calculation is based on the end of the schedule period - the next coupon date - and
        // on whether the frequency is annual
        val nextCouponDate = required(scheduleInfo.periodEndDate(firstDate), PeriodEndDateRequired)
        if (required(scheduleInfo.frequency, FrequencyRequired).isAnnual) {
          val nextLeap = DateAdjusters.nextLeapDay(firstDate)
          actualDays.toDouble / (if (nextLeap.isAfter(nextCouponDate)) 365d else 366d)
        } else {
          actualDays.toDouble / (if (nextCouponDate.isLeapYear) 366d else 365d)
        }
      }
    }

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int =
      actualDayCount(firstDate, secondDate)
  }


  /**
   * The 'Act/360' day count, which divides the actual number of days by 360.
   *
   * The result is a simple division. The numerator is the actual number of days in the requested
   * period and the denominator is always 360.
   *
   * Also known as 'Actual/360' or 'French'; defined by the 2006 ISDA definitions 4.16e and ICMA
   * rule 251.1(i) part 1.
   */
  case object ACT_360 extends DayCount("Act/360") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double =

      LocalDateUtils.daysBetween(firstDate, secondDate).toDouble / 360d

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int =
      actualDayCount(firstDate, secondDate)
  }

  /**
   * The 'Act/364' day count, which divides the actual number of days by 364.
   *
   * The result is a simple division. The numerator is the actual number of days in the requested
   * period and the denominator is always 364.
   *
   * Also known as 'Actual/364'.
   */
  case object ACT_364 extends DayCount("Act/364") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double =

      LocalDateUtils.daysBetween(firstDate, secondDate).toDouble / 364d

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int =
      actualDayCount(firstDate, secondDate)
  }

  /**
   * The 'Act/365F' day count, which divides the actual number of days by 365 (fixed).
   *
   * The result is a simple division. The numerator is the actual number of days in the requested
   * period and the denominator is always 365.
   *
   * Also known as 'Act/365', 'Actual/365 Fixed' or 'English'; defined by the 2006 ISDA definitions
   * 4.16d.
   */
  case object ACT_365F extends DayCount("Act/365F") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double =

      LocalDateUtils.daysBetween(firstDate, secondDate).toDouble / 365d

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int =
      actualDayCount(firstDate, secondDate)
  }

  /**
   * The 'Act/365.25' day count, which divides the actual number of days by 365.25.
   *
   * The result is a simple division. The numerator is the actual number of days in the requested
   * period and the denominator is always 365.25.
   */
  case object ACT_365_25 extends DayCount("Act/365.25") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double =

      LocalDateUtils.daysBetween(firstDate, secondDate).toDouble / 365.25d

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int =
      actualDayCount(firstDate, secondDate)
  }

  /**
   * The 'NL/360' day count, which divides the actual number of days omitting leap days by 360.
   *
   * The result is a simple division. The numerator is the actual number of days in the requested
   * period minus the number of occurrences of February 29 and the denominator is always 360. The
   * first day of the period is excluded and the last is included.
   *
   * Also known as 'NoLeap/360', 'Actual/360 No Leap' or 'Actual (no leap year)/360'.
   */
  case object NL_360 extends DayCount("NL/360") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double = {

      val actualDays = LocalDateUtils.daysBetween(firstDate, secondDate)
      val leapDays = numberOfLeapDays(firstDate, secondDate)
      (actualDays - leapDays.toLong).toDouble / 360d
    }

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int = {
      val leapDays = numberOfLeapDays(firstDate, secondDate)
      actualDayCount(firstDate, secondDate) - leapDays
    }
  }

  /**
   * The 'NL/365' day count, which divides the actual number of days omitting leap days by 365.
   *
   * The result is a simple division. The numerator is the actual number of days in the requested
   * period minus the number of occurrences of February 29 and the denominator is always 365. The
   * first day of the period is excluded and the last is included.
   *
   * Also known as 'NoLeap/365', 'Actual/365 No Leap' or 'Actual (no leap year)/365'.
   */
  case object NL_365 extends DayCount("NL/365") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double = {

      val actualDays = LocalDateUtils.daysBetween(firstDate, secondDate)
      val leapDays = numberOfLeapDays(firstDate, secondDate)
      (actualDays - leapDays.toLong).toDouble / 365d
    }

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int = {
      val leapDays = numberOfLeapDays(firstDate, secondDate)
      actualDayCount(firstDate, secondDate) - leapDays
    }
  }

  /**
   * The '30/360 ISDA' day count, which treats an input day-of-month of 31 specially.
   *
   * The result is `(360 * deltaYear + 30 * deltaMonth + deltaDay) / 360`, the day difference being
   * calculated once the day-of-month adjustments have occurred. If the second day-of-month is 31
   * and the first day-of-month is 30 or 31, the second day-of-month is changed to 30. If the first
   * day-of-month is 31, it is changed to 30.
   *
   * Also known as '30/360 U.S. Municipal' or '30/360 Bond Basis'; defined by the 2006 ISDA
   * definitions 4.16f.
   */
  case object THIRTY_360_ISDA extends DayCount("30/360 ISDA") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double =

      calculateDays(firstDate, secondDate).toDouble / 360d

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int = {
      val dom1 = firstDate.getDayOfMonth
      val dom2 = secondDate.getDayOfMonth
      val d1 = if (dom1 == 31) 30 else dom1
      val d2 = if (dom2 == 31 && d1 == 30) 30 else dom2
      thirty360Days(
        firstDate.getYear,
        firstDate.getMonthValue,
        d1,
        secondDate.getYear,
        secondDate.getMonthValue,
        d2)
    }
  }

  /**
   * The '30U/360' day count, which treats an input day-of-month of 31 and the end of February
   * specially, following the end-of-month flag of the schedule.
   *
   * The result is `(360 * deltaYear + 30 * deltaMonth + deltaDay) / 360`, the day difference being
   * calculated once the day-of-month adjustments have occurred. If the schedule uses the
   * end-of-month convention and both dates are the last day of February, the second day-of-month is
   * changed to 30; if it uses the convention and the first date is the last day of February, the
   * first day-of-month is changed to 30. If the second day-of-month is 31 and the first
   * day-of-month is 30 or 31, the second day-of-month is changed to 30. If the first day-of-month
   * is 31, it is changed to 30.
   *
   * This is the one convention whose rule depends on the end-of-month flag of the
   * [[DayCount.ScheduleInfo]], which defaults to true. It is identical to [[THIRTY_U_360_EOM]] when
   * the convention applies and to [[THIRTY_360_ISDA]] when it does not, and it delegates to
   * whichever of the two applies rather than restating either rule.
   *
   * Also known as '30/360 US', '30US/360' or '30/360 SIA'. The US 30/360 day count appears to have
   * started with the two rules of '30/360 ISDA', the last day of February rules being added later.
   */
  case object THIRTY_U_360 extends DayCount("30U/360") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double =

      if (scheduleInfo.isEndOfMonthConvention) {
        THIRTY_U_360_EOM.calculateYearFraction(firstDate, secondDate, scheduleInfo)
      } else {
        THIRTY_360_ISDA.calculateYearFraction(firstDate, secondDate, scheduleInfo)
      }

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int =
      THIRTY_360_ISDA.days(firstDate, secondDate)
  }

  /**
   * The '30U/360 EOM' day count, which treats an input day-of-month of 31 and the end of February
   * specially, with the end-of-month rule always applied.
   *
   * The result is `(360 * deltaYear + 30 * deltaMonth + deltaDay) / 360`, the day difference being
   * calculated once the day-of-month adjustments have occurred. If both dates are the last day of
   * February, the second day-of-month is changed to 30; if the first date is the last day of
   * February, the first day-of-month is changed to 30. If the second day-of-month is 31 and the
   * first day-of-month is 30 or 31, the second day-of-month is changed to 30. If the first
   * day-of-month is 31, it is changed to 30.
   *
   * This day count does not depend on the end-of-month flag of the [[DayCount.ScheduleInfo]] and is
   * the same as [[THIRTY_U_360]] when that flag is set. It exists to be explicit about the rule
   * applying; in most cases '30U/360' should be preferred.
   *
   * @see [[THIRTY_U_360]]
   */
  case object THIRTY_U_360_EOM extends DayCount("30U/360 EOM") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double =

      calculateDays(firstDate, secondDate).toDouble / 360d

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int = {
      val dom1 = firstDate.getDayOfMonth
      val dom2 = secondDate.getDayOfMonth
      val firstIsEndOfFebruary = lastDayOfFebruary(firstDate)
      val febD1 = if (firstIsEndOfFebruary) 30 else dom1
      val febD2 = if (firstIsEndOfFebruary && lastDayOfFebruary(secondDate)) 30 else dom2
      // now apply the '30/360 ISDA' rules
      val d1 = if (febD1 == 31) 30 else febD1
      val d2 = if (febD2 == 31 && d1 == 30) 30 else febD2
      thirty360Days(
        firstDate.getYear,
        firstDate.getMonthValue,
        d1,
        secondDate.getYear,
        secondDate.getMonthValue,
        d2)
    }
  }

  /**
   * The '30/360 PSA' day count, which treats an input day-of-month of 31 and the last day of a
   * month specially.
   *
   * The result is `(360 * deltaYear + 30 * deltaMonth + deltaDay) / 360`, the day difference being
   * calculated once the day-of-month adjustments have occurred. If the first date is the last day
   * of its month, the first day-of-month is changed to 30. If the second day-of-month is 31 and the
   * first day-of-month is 30 or 31, the second day-of-month is changed to 30.
   *
   * PSA is the Public Securities Association, later the Bond Market Association.
   */
  case object THIRTY_360_PSA extends DayCount("30/360 PSA") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double =

      calculateDays(firstDate, secondDate).toDouble / 360d

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int = {
      val dom1 = firstDate.getDayOfMonth
      val dom2 = secondDate.getDayOfMonth
      val d1 = if (dom1 == firstDate.lengthOfMonth) 30 else dom1
      val d2 = if (dom2 == 31 && d1 == 30) 30 else dom2
      thirty360Days(
        firstDate.getYear,
        firstDate.getMonthValue,
        d1,
        secondDate.getYear,
        secondDate.getMonthValue,
        d2)
    }
  }

  /**
   * The '30E/360 ISDA' day count, which treats an input day-of-month of 31 and the end of February
   * specially, the maturity date excepted.
   *
   * The result is `(360 * deltaYear + 30 * deltaMonth + deltaDay) / 360`, the day difference being
   * calculated once the day-of-month adjustments have occurred. If the first day-of-month is 31 it
   * is changed to 30, and if the second day-of-month is 31 it is changed to 30. If the first date
   * is the last day of February, the first day-of-month is changed to 30. If the second date is the
   * last day of February '''and is not the maturity date''', the second day-of-month is changed to
   * 30.
   *
   * The two-argument `yearFraction(firstDate, secondDate)` cannot be used with this convention
   * where the second date is the last day of February, because the maturity date of the schedule is
   * required in order to apply the last rule. Every other pair of dates - including any pair whose
   * second day-of-month is 31, which short-circuits the rule - needs no schedule information at
   * all, and that is the behaviour of the implementation being ported rather than a relaxation of
   * it.
   *
   * The day count, as opposed to the year fraction, applies the last rule unconditionally: it has
   * no schedule to consult, exactly as before.
   *
   * Also known as '30E/360 German' or 'German'; defined by the 2006 ISDA definitions 4.16h.
   */
  case object THIRTY_E_360_ISDA extends DayCount("30E/360 ISDA") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double = {

      val dom1 = firstDate.getDayOfMonth
      val dom2 = secondDate.getDayOfMonth
      val d1 = if (dom1 == 31 || lastDayOfFebruary(firstDate)) 30 else dom1
      // the end date of the schedule is read only where the second date is the last day of
      // February and is not already being changed, which is the short-circuit the ported
      // expression performed and the only case in which this convention needs a schedule
      val d2 =
        if (dom2 == 31) {
          30
        } else if (lastDayOfFebruary(secondDate) &&
          secondDate != required(scheduleInfo.endDate, ScheduleEndDateRequired)) {
          30
        } else {
          dom2
        }
      thirty360Days(
        firstDate.getYear,
        firstDate.getMonthValue,
        d1,
        secondDate.getYear,
        secondDate.getMonthValue,
        d2).toDouble / 360d
    }

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int = {
      val dom1 = firstDate.getDayOfMonth
      val dom2 = secondDate.getDayOfMonth
      val d1 = if (dom1 == 31 || lastDayOfFebruary(firstDate)) 30 else dom1
      val d2 = if (dom2 == 31 || lastDayOfFebruary(secondDate)) 30 else dom2
      thirty360Days(
        firstDate.getYear,
        firstDate.getMonthValue,
        d1,
        secondDate.getYear,
        secondDate.getMonthValue,
        d2)
    }
  }

  /**
   * The '30E/360' day count, which treats an input day-of-month of 31 specially.
   *
   * The result is `(360 * deltaYear + 30 * deltaMonth + deltaDay) / 360`, the day difference being
   * calculated once the day-of-month adjustments have occurred. If the first day-of-month is 31 it
   * is changed to 30, and if the second day-of-month is 31 it is changed to 30.
   *
   * Also known as '30/360 ISMA', '30/360 European', '30S/360 Special German' or 'Eurobond';
   * defined by the 2006 ISDA definitions 4.16g and ICMA rule 251.1(ii) and 252.2.
   */
  case object THIRTY_E_360 extends DayCount("30E/360") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double =

      calculateDays(firstDate, secondDate).toDouble / 360d

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int = {
      val dom1 = firstDate.getDayOfMonth
      val dom2 = secondDate.getDayOfMonth
      val d1 = if (dom1 == 31) 30 else dom1
      val d2 = if (dom2 == 31) 30 else dom2
      thirty360Days(
        firstDate.getYear,
        firstDate.getMonthValue,
        d1,
        secondDate.getYear,
        secondDate.getMonthValue,
        d2)
    }
  }

  /**
   * The '30E+/360' day count, which treats an input day-of-month of 31 specially, rolling the
   * second date into the following month.
   *
   * The result is `(360 * deltaYear + 30 * deltaMonth + deltaDay) / 360`, the day and month
   * differences being calculated once the adjustments have occurred. If the first day-of-month is
   * 31 it is changed to 30. If the second day-of-month is 31 it is changed to 1 and the second
   * month is incremented - the nature of the calculation means that a December date needs no
   * adjustment from month 13 to January of the following year.
   */
  case object THIRTY_EPLUS_360 extends DayCount("30E+/360") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double =

      calculateDays(firstDate, secondDate).toDouble / 360d

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int = {
      val dom1 = firstDate.getDayOfMonth
      val dom2 = secondDate.getDayOfMonth
      val month1 = firstDate.getMonthValue
      val rawMonth2 = secondDate.getMonthValue
      val d1 = if (dom1 == 31) 30 else dom1
      val d2 = if (dom2 == 31) 1 else dom2
      val month2 = if (dom2 == 31) rawMonth2 + 1 else rawMonth2
      thirty360Days(firstDate.getYear, month1, d1, secondDate.getYear, month2, d2)
    }
  }

  /**
   * The '30E/365' day count, which treats the last day of a month specially and divides by 365.
   *
   * The result is `(360 * deltaYear + 30 * deltaMonth + deltaDay) / 365`, the day difference being
   * calculated once the day-of-month adjustments have occurred. If the first day-of-month is the
   * last day of its month it is changed to 30, and the same rule is applied to the second
   * day-of-month.
   *
   * Also known as '30/365 German'.
   */
  case object THIRTY_E_365 extends DayCount("30E/365") {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double =

      calculateDays(firstDate, secondDate).toDouble / 365d

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int = {
      val dom1 = firstDate.getDayOfMonth
      val dom2 = secondDate.getDayOfMonth
      val d1 = if (dom1 == firstDate.lengthOfMonth) 30 else dom1
      val d2 = if (dom2 == secondDate.lengthOfMonth) 30 else dom2
      thirty360Days(
        firstDate.getYear,
        firstDate.getMonthValue,
        d1,
        secondDate.getYear,
        secondDate.getMonthValue,
        d2)
    }
  }

  //-------------------------------------------------------------------------
  /**
   * The 'Bus/252' day count, which counts business days against a holiday calendar and divides by
   * 252.
   *
   * The result is a simple division. The numerator is the number of business days of the calendar
   * in the period, the first day included and the last excluded, and the denominator is always 252
   * - the conventional number of business days in a year. This day count is typically used in
   * Brazil.
   *
   * ===One instance per calendar===
   *
   * Unlike the twenty-one standard conventions this one is not a singleton: the calendar is part of
   * the convention and appears in its name, so `Bus/252 BRBD` and `Bus/252 GBLO` are two day counts
   * of the same kind. Instances are created only by [[DayCount.ofBus252]] - the constructor is
   * visible to the companion alone, and this is not a `case class`, so there is no `apply` and no
   * `copy` to bypass it with.
   *
   * The calendar held here is '''resolved''': the convention carries the calendar itself rather than
   * an identifier to be looked up, which is what keeps [[DayCount.yearFraction]] and
   * [[DayCount.days]] pure functions of their arguments. The implementation being ported resolved
   * the identifier against the standard reference data from inside its own factory; this port
   * requires the caller to supply the calendar or the reference data, and that removal of an
   * ambient lookup is recorded as a deliberate divergence in `SCALA_MIGRATION.md`.
   *
   * ===Equality===
   *
   * Two instances are equal when their names are equal, which is equality by the '''name''' of the
   * calendar rather than by its holidays - the comparison the ported implementation made, and the
   * one consistent with the `Order`, `Hash` and `Show` instances of this family, all of which are
   * derived from the name. Two instances built over different calendars that share an identifier
   * are therefore equal, as are the calendars themselves.
   *
   * @param calendar  the resolved holiday calendar whose business days are counted
   */
  final class Bus252 private[DayCount] (val calendar: HolidayCalendar)
      extends DayCount(Bus252Prefix + calendar.name) {

    override protected[date] def calculateYearFraction(
        firstDate: LocalDate,
        secondDate: LocalDate,
        scheduleInfo: ScheduleInfo): Double =

      calendar.daysBetween(firstDate, secondDate).toDouble / Bus252DaysPerYear

    override protected[date] def calculateDays(firstDate: LocalDate, secondDate: LocalDate): Int =
      calendar.daysBetween(firstDate, secondDate)

    override def equals(obj: Any): Boolean = obj match {
      case other: Bus252 => other.name == name
      case _ => false
    }

    override def hashCode: Int = name.hashCode
  }

  /**
   * Obtains the 'Bus/252' day count for a resolved holiday calendar.
   *
   * The calendar is stored in the day count and named by it, so the result is named
   * `Bus/252 BRBD` for the Brazilian calendar. This factory is total: any calendar, built-in or
   * supplied by an application, yields a day count.
   *
   * {{{
   * DayCount.ofBus252(StandardHolidayCalendars.BRBD).name == "Bus/252 BRBD"
   * }}}
   *
   * @param calendar  the resolved holiday calendar
   * @return the day count counting the business days of that calendar
   */
  def ofBus252(calendar: HolidayCalendar): DayCount = new Bus252(calendar)

  /**
   * Obtains the 'Bus/252' day count for a calendar identifier, resolved against the reference data
   * supplied.
   *
   * This is the explicit form of the factory the implementation being ported provided: there the
   * identifier was resolved against the standard reference data reached from inside the factory,
   * which made the result depend on ambient state; here the caller passes the reference data it is
   * working with, so an application's own holidays are used where it has them.
   *
   * {{{
   * DayCount.ofBus252(HolidayCalendarIds.BRBD, ReferenceData.standard)
   * }}}
   *
   * @param id  the identifier of the holiday calendar
   * @param refData  the reference data to resolve the identifier against
   * @return the day count, or the failure explaining why the identifier could not be resolved
   */
  def ofBus252(id: HolidayCalendarId, refData: ReferenceData): Either[Failure, DayCount] =
    id.resolve(refData).map(calendar => ofBus252(calendar))

  //-------------------------------------------------------------------------
  /**
   * The twenty-one standard day counts, in declaration order.
   *
   * The order is the declaration order of the enum being ported, which is also the order in which
   * the members claim their lookup keys and the order a report over the family follows. It is not
   * the order the `Order` instance below imposes, which is alphabetical by name.
   *
   * The `Bus/252` conventions are deliberately absent: there is one of them per holiday calendar
   * rather than one per family, so the set is open and cannot be enumerated. The library being
   * ported drew the same line, listing the standard constants from one provider and creating
   * `Bus/252` conventions on demand from another.
   *
   * @return the twenty-one standard day counts, in declaration order
   */
  val values: NonEmptyList[DayCount] =
    NonEmptyList.of(
      ONE_ONE,
      ACT_ACT_ISDA,
      ACT_ACT_ICMA,
      ACT_ACT_AFB,
      ACT_ACT_YEAR,
      ACT_365_ACTUAL,
      ACT_365L,
      ACT_360,
      ACT_364,
      ACT_365F,
      ACT_365_25,
      NL_360,
      NL_365,
      THIRTY_360_ISDA,
      THIRTY_U_360,
      THIRTY_U_360_EOM,
      THIRTY_360_PSA,
      THIRTY_E_360_ISDA,
      THIRTY_E_360,
      THIRTY_EPLUS_360,
      THIRTY_E_365
    )

  //-------------------------------------------------------------------------
  /** The label this family gives itself when it rejects text. */
  private val FamilyName: String = "DayCount"

  /**
   * The spellings this family publishes for the FpML protocol, each mapped to a canonical name.
   *
   * These are the fourteen rows of the FpML group of external names that the configuration resource
   * of the ported library declared, transcribed unchanged. They take part in no lookup - reading
   * `ACT/360` as a day count is the business of the lenient patterns below, which happen to accept
   * it - and exist so that a caller writing or reading that protocol can map between the two
   * vocabularies explicitly, through `NamedEnum.externalNames`.
   *
   * Note the row for `BUS/252`, which names a day count that is not a member of [[values]]: the
   * name lookup resolves an external row against the members of the family, so this row is
   * published by `externalNamesRaw` and absent from the resolved view. `DayCount.parse("BUS/252")`
   * reaches the Brazilian convention by the lenient route instead.
   */
  private val FpMLNames: Map[String, String] =
    Map(
      "1/1" -> "1/1",
      "30/360" -> "30/360 ISDA",
      "30E/360" -> "30E/360",
      "30E/360.ISDA" -> "30E/360 ISDA",
      "ACT/360" -> "Act/360",
      "ACT/365.FIXED" -> "Act/365F",
      "ACT/365" -> "Act/365F",
      "ACT/365L" -> "Act/365L",
      "ACT/ACT.AFB" -> "Act/Act AFB",
      "ACT/ACT.ICMA" -> "Act/Act ICMA",
      "ACT/ACT.ISMA" -> "Act/Act ICMA",
      "ACT/ACT.ISDA" -> "Act/Act ISDA",
      "ACT/365.ISDA" -> "Act/Act ISDA",
      "BUS/252" -> "Bus/252 BRBD"
    )

  /**
   * The spellings this family publishes for the SWIFT message standard, each mapped to a canonical
   * name.
   *
   * These are the eight rows of the SWIFT group of external names that the configuration resource
   * of the ported library declared, transcribed unchanged. The group disagrees with the FpML one
   * about two spellings - `30E/360` and `ACT/365` name different conventions in the two
   * vocabularies - which is exactly why each group is published separately rather than merged into
   * one table of aliases.
   */
  private val SwiftNames: Map[String, String] =
    Map(
      "30E/360" -> "30E/360 ISDA",
      "360/360" -> "30U/360",
      "ACT/360" -> "Act/360",
      "ACT/365" -> "Act/Act ISDA",
      "AFI/365" -> "Act/365F",
      "EBD/360" -> "30E/360",
      "EXA/EXA" -> "Act/Act AFB",
      "ICM/ACT" -> "Act/Act ICMA"
    )

  /**
   * The lenient rewrites of this family, in the order they are applied.
   *
   * These are the sixty-seven rows of the lenient patterns that the configuration resource of the
   * ported library declared, in the order that resource listed them, and the order is part of the
   * data: [[parse]] folds its input to upper case and then applies every pattern in turn, a pattern
   * whose expression matches the whole of the current text replacing that text, so a later pattern
   * sees what an earlier one produced. Reordering these rows would change which text resolves and
   * to what.
   *
   * The chain is what lets the long and short spellings of `Actual`, the bracketed and dotted
   * qualifier forms, the screaming-snake spellings of the constant identifiers and a shelf of
   * market nicknames all reach the same member:
   *
   * {{{
   * parse("Actual/Actual (ISDA)")  // Act/Act ISDA - brackets removed, then Actual expanded
   * parse("A/A ISMA")              // Act/Act ICMA - abbreviation expanded, then ISMA renamed
   * parse("ACT_365F")              // Act/365F     - by constant identifier
   * parse("English")               // Act/365F     - by nickname
   * parse("Bus/252")               // Bus/252 BRBD - defaulted to the Brazilian calendar
   * }}}
   *
   * Each expression is written here in the mixed case of the original row and matched insensitively
   * to case, because [[parse]] has already folded its input to upper case by the time they are
   * applied.
   */
  private val LenientPatterns: List[(Regex, String)] =
    List(
      // convert actual
      "ACTUAL/ACTUAL(.*)".r -> "Act/Act$1",
      "ACTUAL/(.*)".r -> "Act/$1",
      "ACT/ACT(.*)".r -> "Act/Act$1",
      "ACT/(.*)".r -> "Act/$1",
      "A/A(.*)".r -> "Act/Act$1",
      "A/(.*)".r -> "Act/$1",
      // remove brackets
      "(.*)[(](.*)[)]".r -> "$1$2",
      // replace dot with space
      "(.*)[.]([A-Z])(.*)".r -> "$1 $2$3",
      // replace ISMA with ICMA
      "(.*) ISMA".r -> "$1 ICMA",
      // Act/Act oddities
      "Act/Act".r -> "Act/Act ISDA",
      "Act/365 ISDA".r -> "Act/Act ISDA",
      "Act/Act Historical".r -> "Act/Act ISDA",
      "Act/Act Bond".r -> "Act/Act ICMA",
      "ISMA-99".r -> "Act/Act ICMA",
      "Act/Act Euro".r -> "Act/Act AFB",
      "Act/Act YEAR".r -> "Act/Act Year",
      // Act/36x oddities
      "Act/365 ACTUAL".r -> "Act/365 Actual",
      "Act/365A".r -> "Act/365 Actual",
      "Act/365 Leap year".r -> "Act/365L",
      "ISMA-Year".r -> "Act/365L",
      "French".r -> "Act/360",
      "Act/365".r -> "Act/365F",
      "Act/365 Fixed".r -> "Act/365F",
      "Act/Fixed 365".r -> "Act/365F",
      "English".r -> "Act/365F",
      "NL360".r -> "NL/360",
      "Act/360 No leap year".r -> "NL/360",
      "Act/NL".r -> "NL/365",
      "NL365".r -> "NL/365",
      "Act/365 No leap year".r -> "NL/365",
      // enum style
      "ONE_ONE".r -> "1/1",
      "ACT_ACT_ISDA".r -> "Act/Act ISDA",
      "ACT_ACT_ICMA".r -> "Act/Act ICMA",
      "ACT_ACT_AFB".r -> "Act/Act AFB",
      "ACT_ACT_YEAR".r -> "Act/Act Year",
      "ACT_365_ACTUAL".r -> "Act/365 Actual",
      "ACT_365L".r -> "Act/365L",
      "ACT_360".r -> "Act/360",
      "ACT_364".r -> "Act/364",
      "ACT_365F".r -> "Act/365F",
      "ACT_365_25".r -> "Act/365.25",
      "NL_360".r -> "NL/360",
      "NL_365".r -> "NL/365",
      "THIRTY_360_ISDA".r -> "30/360 ISDA",
      "THIRTY_U_360".r -> "30U/360",
      "THIRTY_U_360_EOM".r -> "30U/360 EOM",
      "THIRTY_360_PSA".r -> "30/360 PSA",
      "THIRTY_E_360_ISDA".r -> "30E/360 ISDA",
      "THIRTY_E_360".r -> "30E/360",
      "THIRTY_EPLUS_360".r -> "30E+/360",
      "THIRTY_E_365".r -> "30E/365",
      // 30/360 oddities
      "30/360".r -> "30/360 ISDA",
      "Eurobond Basis".r -> "30E/360",
      "30S/360".r -> "30E/360",
      "Special German".r -> "30E/360",
      "30/360 ICMA".r -> "30E/360",
      "30/360 German".r -> "30E/360 ISDA",
      "German".r -> "30E/360 ISDA",
      "30/360 US".r -> "30U/360",
      "30US/360".r -> "30U/360",
      "360/360".r -> "30U/360",
      "Bond Basis".r -> "30U/360",
      "US".r -> "30U/360",
      "ISMA-30/360".r -> "30U/360",
      "30/360 SIA".r -> "30U/360",
      "30/365 German".r -> "30E/365",
      // Bus/252, defaulted to Brazil
      "Bus/252".r -> "Bus/252 BRBD"
    )

  /**
   * The lenient expressions, copied to be insensitive to case.
   *
   * The rewrites are applied to text that [[parse]] has already folded to upper case, so an
   * expression written in the mixed case of its original row - and a character class written as
   * upper case - could never match without this. The copy is made by prefixing the inline flag to
   * the source of each expression, which is how the name lookup of this library treats the same
   * table.
   */
  private val LenientMatchers: List[(Regex, String)] =
    LenientPatterns.map {
      case (expression, replacement) => (("(?i)" + expression.pattern.pattern()).r, replacement)
    }

  /**
   * The name lookup for the twenty-one standard day counts.
   *
   * This instance is built from [[values]] and the three transcribed tables alone. The family
   * declares no alternate spelling, because the resource of the ported library declared none for
   * it: every spelling other than the twenty-one canonical names is reached through the lenient
   * patterns, and the two external groups are published rather than looked up. Nothing is read from
   * a class or from the class path, so the name space of the standard family is fixed when this
   * file is compiled.
   *
   * It covers the standard members only. [[valueOf]] and [[parse]] wrap it with the `Bus/252`
   * lookup, which no closed family can express, in the two places the registry being replaced
   * consulted its second provider.
   *
   * @return the name lookup for the twenty-one standard day counts
   */
  implicit val namedEnum: NamedEnum[DayCount] =
    NamedEnum.of(values, Map.empty, LenientPatterns, Map("FpML" -> FpMLNames, "SWIFT" -> SwiftNames), FamilyName)

  //-------------------------------------------------------------------------
  /**
   * Obtains the day count with the specified canonical name, if one exists.
   *
   * The match is exact against the canonical names and against those names folded to upper case, so
   * `Act/365F` and `ACT/365F` resolve while `act/365f` does not. No lenient pattern is applied. A
   * `Bus/252` name is resolved as well - its prefix insensitively to case and the rest of it
   * against the calendars built into this library - so `Bus/252 GBLO` and `BUS/252 GBLO` both name
   * a day count, and a name whose calendar this library does not define answers with `None`.
   *
   * This is the exact lookup the registry being replaced performed over its two providers, with the
   * one difference that a `Bus/252` name holding an unknown calendar is an empty answer here where
   * the registry raised. Use [[parse]] to accept text whose shape is not known in advance, or to be
   * told '''why''' a name did not resolve.
   *
   * @param name  the name to look up
   * @return the day count with that name, or `None` when no day count has it
   */
  def valueOf(name: String): Option[DayCount] =
    namedEnum
      .valueOf(name)
      .orElse(bus252(name, calendarName => HolidayCalendars.of(calendarName)).flatMap(result => result.toOption))

  /**
   * Parses a day count from text, applying the leniency this family declares.
   *
   * The exact lookup of [[valueOf]] is tried first. Failing that, the text is folded to upper case
   * and the sixty-seven lenient patterns are applied in order before the exact lookup is tried once
   * more, so a long spelling, a bracketed qualifier, a screaming-snake identifier or a market
   * nickname all resolve:
   *
   * {{{
   * parse("Act/365F")             // Right(ACT_365F)     - the canonical name
   * parse("ACT/360")              // Right(ACT_360)      - folded and rewritten
   * parse("Actual/Actual (ISDA)") // Right(ACT_ACT_ISDA) - brackets removed, then expanded
   * parse("Bus/252")              // Right(Bus/252 BRBD) - defaulted to the Brazilian calendar
   * parse("Bus/252 GBLO")         // Right(Bus/252 GBLO) - resolved against the built-in calendars
   * parse("Rubbish")              // Left - text this family has never accepted
   * }}}
   *
   * A `Bus/252` name is resolved against the calendars '''built into this library''', which are
   * constant data rather than ambient state; the overload taking reference data resolves against
   * whatever calendars the caller supplies, which is the form to use for an application's own
   * calendars. A name whose calendar cannot be resolved fails with the reason the calendar lookup
   * gave, rather than with the reason this family gives for text it does not recognise.
   *
   * Where the type being ported raised an error for unrecognised text, this method reports it as a
   * value: the result is `Left` of a chain holding one
   * [[com.opengamma.strata.collect.result.Failure]] whose reason is `PARSING` and whose message
   * names both this family and the text that could not be resolved.
   *
   * @param name  the text to parse
   * @return the day count the text names, or the failure describing why it names none
   */
  def parse(name: String): EitherNec[Failure, DayCount] =
    parseWith(name, calendarName => HolidayCalendars.of(calendarName))

  /**
   * Parses a day count from text, resolving a `Bus/252` calendar against the reference data
   * supplied.
   *
   * The algorithm is that of [[parse]] in every respect but one: the calendar of a `Bus/252` name is
   * resolved against the reference data given rather than against the calendars built into this
   * library, so an application that supplies its own holidays can read back a day count that names
   * them. Every other name resolves identically, and reference data is not consulted for it.
   *
   * {{{
   * parse("Bus/252 XCAL", refData)  // Right, where refData holds a calendar named XCAL
   * parse("Bus/252 XCAL", ReferenceData.standard)  // Left - the standard data has no XCAL
   * }}}
   *
   * @param name  the text to parse
   * @param refData  the reference data used to resolve the calendar of a `Bus/252` name
   * @return the day count the text names, or the failure describing why it names none
   */
  def parse(name: String, refData: ReferenceData): EitherNec[Failure, DayCount] =
    parseWith(name, calendarName => HolidayCalendarId.of(calendarName).resolve(refData))

  /**
   * The two-stage lookup the registry being replaced performed, given a way of resolving a calendar.
   *
   * Stage one is the exact lookup over both providers - the closed family and `Bus/252` - and stage
   * two folds the text to upper case, applies every lenient rewrite in order and repeats the exact
   * lookup. Text that survives both stages unresolved is reported with the failure this family
   * gives, unless a `Bus/252` name was recognised and its calendar was not, in which case the
   * calendar's own failure is reported instead: that is the more specific answer, and it is the
   * error the ported implementation raised from the same place.
   *
   * @param name  the text to parse
   * @param resolveCalendar  resolves the calendar part of a `Bus/252` name
   * @return the day count the text names, or the failure describing why it names none
   */
  private def parseWith(
      name: String,
      resolveCalendar: String => Either[Failure, HolidayCalendar]): EitherNec[Failure, DayCount] =

    exact(name, resolveCalendar) match {
      case Some(result) => result
      case None =>
        exact(rewriteLeniently(name.toUpperCase(Locale.ENGLISH)), resolveCalendar)
          .getOrElse(Left(NonEmptyChain.one(nameNotFound(name))))
    }

  /**
   * The exact lookup over the two providers, in the order the registry consulted them.
   *
   * The closed family answers first, and only text it does not claim reaches the `Bus/252` lookup.
   * An empty answer means no provider recognised the text at all, which is what sends [[parseWith]]
   * on to the lenient stage; an answer that is present but failed means a provider recognised the
   * text and could not complete it, which is reported as it stands.
   *
   * @param name  the text to look up
   * @param resolveCalendar  resolves the calendar part of a `Bus/252` name
   * @return the outcome of the provider that claimed the text, or `None` where none did
   */
  private def exact(
      name: String,
      resolveCalendar: String => Either[Failure, HolidayCalendar]): Option[EitherNec[Failure, DayCount]] =

    namedEnum.valueOf(name) match {
      case Some(dayCount) => Some(Right(dayCount))
      case None => bus252(name, resolveCalendar)
    }

  /**
   * The `Bus/252` lookup: the prefix matched insensitively to case, the rest resolved as a calendar.
   *
   * The prefix test is the one the ported lookup made - eight characters compared without regard to
   * case - so `Bus/252 `, `BUS/252 ` and any mixture claim the text, and shorter text does not. The
   * name of the resulting day count is rebuilt from the '''resolved''' calendar rather than copied
   * from the input, so `BUS/252 EUTA` yields a day count named `Bus/252 EUTA`.
   *
   * @param name  the text to look up
   * @param resolveCalendar  resolves the calendar part of the name
   * @return the day count or the calendar's failure where the text carries the prefix, `None`
   *   otherwise
   */
  private def bus252(
      name: String,
      resolveCalendar: String => Either[Failure, HolidayCalendar]): Option[EitherNec[Failure, DayCount]] =

    if (name.regionMatches(true, 0, Bus252Prefix, 0, Bus252PrefixLength)) {
      Some(
        resolveCalendar(name.substring(Bus252PrefixLength))
          .left
          .map(failure => NonEmptyChain.one(failure))
          .map(calendar => ofBus252(calendar)))
    } else {
      None
    }

  /**
   * Applies every lenient rewrite to the specified text, in order.
   *
   * An expression that matches the whole of the text replaces it, and the expression after it is
   * applied to the replacement, so the rewrites chain and the replacement may refer back to the
   * groups the expression captured. One matcher serves both purposes for each rule, deciding
   * whether the rule applies and then performing the replacement, as the ported loop did.
   *
   * The whole table is applied to text of any length, which is what the ported algorithm did and
   * what this family needs: the text reaching here may be a `Bus/252` name carrying a composite
   * calendar name of no fixed size, and every expression in the table is anchored to a literal
   * shape with no nested quantifier, so one pass costs one match per rule.
   *
   * @param name  the text to rewrite, already folded to upper case
   * @return the text that survives every rewrite
   */
  private def rewriteLeniently(name: String): String =
    LenientMatchers.foldLeft(name) {
      case (current, (expression, replacement)) =>
        val matcher = expression.pattern.matcher(current)
        if (matcher.matches()) {
          matcher.replaceFirst(replacement)
        } else {
          current
        }
    }

  /**
   * The failure reported for text that names no day count.
   *
   * The text is rendered through `Failure.describeInput` rather than interpolated as it stands, so
   * the message is bounded in length and holds no character that could forge a line of a log
   * carrying it. The wording is that of the name lookup of this library, so a caller cannot tell
   * whether the standard family or the wrapping in this file rejected the text.
   *
   * @param name  the text that was rejected, as it was supplied
   * @return the failure naming this family and the rendering of the text
   */
  private def nameNotFound(name: String): Failure =
    Failure.Parsing(s"$FamilyName name not found: ${Failure.describeInput(name)}")

  //-------------------------------------------------------------------------
  /**
   * The ordering and hashing of day counts.
   *
   * This is the only equality-bearing instance of the type: `Order` extends `Eq` and `Hash` extends
   * `Eq`, so summoning any of the three yields this one value and the three can never disagree.
   * Comparison is over `name`, which makes the ordering alphabetical rather than the declaration
   * order of [[values]], and equality follows it - the twenty-one names are distinct, and a
   * `Bus/252` day count is named for its calendar, so two day counts compare equal exactly when the
   * `equals` of the family says they are equal.
   *
   * @return the ordering of day counts by name, which is also their hashing
   */
  implicit val order: Order[DayCount] with Hash[DayCount] = NamedEnum.orderByName

  /**
   * The rendering of day counts as text.
   *
   * A day count renders as its canonical name, which is what `toString` produces as well, so the
   * two ways of putting a day count into a message agree.
   *
   * @return the rendering of a day count as its canonical name
   */
  implicit val show: Show[DayCount] = NamedEnum.showByName

  //-------------------------------------------------------------------------
  /** The single field naming the structural form of a calendar-bearing day count. */
  private val Bus252Key: String = "Bus252"

  /** The field holding the name of a calendar-bearing day count. */
  private val NameField: String = "name"

  /** The field holding the calendar of a calendar-bearing day count. */
  private val CalendarField: String = "calendar"

  /** Separates the messages of accumulated failures in a single decoding failure. */
  private val FailureMessageSeparator: String = "; "

  /** The message reporting a document that describes no day count. */
  private val UnknownShapeMessage: String =
    s"A day count is either the string of its name or an object of one field named '$Bus252Key'"

  /**
   * The JSON codec for day counts.
   *
   * One of the two hand-written codecs of this port, and hand-written for the same reason as the
   * other: the family mixes a name-based form with a structural one, and the structural form has no
   * public constructor to derive from. Nothing here reads a class or a member by reflection.
   *
   * A standard day count is written as the bare string of its canonical name, which is the form the
   * type being ported wrote through its string conversion:
   *
   * {{{
   * "Act/365F"
   * }}}
   *
   * A `Bus/252` day count is written structurally instead, because its calendar is part of it and a
   * calendar an application built itself has to survive the round trip with its holidays intact:
   *
   * {{{
   * {"Bus252":{"name":"Bus/252 BRBD","calendar":"BRBD"}}
   * {"Bus252":{"name":"Bus/252 XCAL","calendar":{"Immutable":{"id":"XCAL", ...}}}}
   * }}}
   *
   * The nested calendar is written by the codec of [[HolidayCalendar]], which writes a calendar
   * built into this library as its name and any other calendar as its dates.
   *
   * Reading accepts either form. A string is resolved through [[parse]], so every name a caller can
   * write - a canonical name, a lenient spelling, or a hand-written `Bus/252 GBLO` - is read, with
   * the calendar of a `Bus/252` name resolved against the calendars built into this library. An
   * object is read as a `Bus/252` day count whose calendar is taken from the document, and its
   * declared name is '''checked''' against the calendar rather than trusted: a document naming one
   * calendar and carrying another describes no day count this library can build, and is reported as
   * a decoding failure.
   *
   * Two equal values encode to identical bytes: the choice between the two forms follows from the
   * type of the value, the two fields of the structural form are written in a fixed order, and the
   * calendar codec is itself stable.
   *
   * @return the codec reading and writing a day count
   */
  implicit val codec: Codec[DayCount] =
    Codec.from(Decoder.instance(cursor => decodeDayCount(cursor)), Encoder.instance(value => encodeDayCount(value)))

  /**
   * Writes a day count.
   *
   * @param value  the day count to write
   * @return the JSON document of the day count
   */
  private def encodeDayCount(value: DayCount): Json = value match {
    case bus252DayCount: Bus252 =>
      Json.obj(
        Bus252Key -> Json.obj(
          NameField -> Json.fromString(bus252DayCount.name),
          CalendarField -> Encoder[HolidayCalendar].apply(bus252DayCount.calendar)))
    case standard => Json.fromString(standard.name)
  }

  /**
   * Reads a day count.
   *
   * A string is a name and an object holding exactly one field named for the calendar-bearing form
   * is that form. Anything else is rejected, as is an object holding no field or several.
   *
   * @param cursor  the position in the document
   * @return the day count, or the failure explaining why the document describes none
   */
  private def decodeDayCount(cursor: HCursor): Decoder.Result[DayCount] =
    cursor.value.asString match {
      case Some(text) =>
        parse(text).left.map(failures =>
          DecodingFailure(
            failures.toNonEmptyList.toList.map(failure => failure.message).mkString(FailureMessageSeparator),
            cursor.history))
      case None =>
        cursor.keys.map(keys => keys.toList) match {
          case Some(Bus252Key :: Nil) =>
            val fields = cursor.downField(Bus252Key)
            for {
              name <- fields.get[String](NameField)
              calendar <- fields.get[HolidayCalendar](CalendarField)
              dayCount <- checkedBus252(name, calendar, cursor)
            } yield dayCount
          case _ => Left(DecodingFailure(UnknownShapeMessage, cursor.history))
        }
    }

  /**
   * Builds a calendar-bearing day count from a document, checking its declared name.
   *
   * The name of such a day count follows from its calendar, so the two fields of the document are
   * not independent: a document is accepted only where its name is the one the calendar it carries
   * produces. Without the check a document could name `Bus/252 BRBD` and carry a calendar of London
   * holidays, and the value read back would count the wrong days under a name that hid it.
   *
   * @param name  the name the document declares
   * @param calendar  the calendar the document carries
   * @param cursor  the position in the document, for the failure message
   * @return the day count, or the failure explaining why the two fields disagree
   */
  private def checkedBus252(name: String, calendar: HolidayCalendar, cursor: HCursor): Decoder.Result[DayCount] = {
    val expectedName = Bus252Prefix + calendar.id.name
    if (name == expectedName) {
      Right(ofBus252(calendar))
    } else {
      Left(
        DecodingFailure(
          s"A day count named '${Failure.describeInput(name)}' does not match the calendar it " +
            s"carries, which would be named '$expectedName'",
          cursor.history))
    }
  }
}

/**
 * Constants for the standard day count conventions, published under the identifiers the ported
 * library used.
 *
 * The purpose of each convention is to define how to convert dates into numeric year fractions,
 * which is of use when calculating interest accrued over time.
 *
 * Every constant here is one of the members of [[DayCount]], exposed under the name the original
 * constants holder gave it so that a call site reading `DayCounts.ACT_365F` ports across
 * unchanged. The values are the same objects as the members of the companion, so a constant taken
 * from here and the matching member are indistinguishable - including by `eq`, by `==` and in a
 * pattern match.
 *
 * Unlike the holder being ported, these constants are not indirected through a registry: each one
 * names its member directly, because the family is closed and no configuration can replace a
 * member of it. The `Bus/252` conventions have no constant here, as they had none there: they are
 * built by [[DayCount.ofBus252]] from the calendar they count.
 */
object DayCounts {

  /**
   * The '1/1' day count, which always returns a day count of 1.
   *
   * The result is always one.
   *
   * Also known as 'One/One'. Defined by the 2006 ISDA definitions 4.16a.
   */
  val ONE_ONE: DayCount = DayCount.ONE_ONE

  /**
   * The 'Act/Act ISDA' day count, which divides the actual number of days in a leap year by 366
   * and the actual number of days in a standard year by 365.
   *
   * The result is calculated in two parts. The actual number of days in the requested period that
   * fall in a leap year is divided by 366. The actual number of days in the requested period that
   * fall in a standard year is divided by 365. The result is the sum of the two. The first day in
   * the period is included, the last day is excluded.
   *
   * Also known as 'Actual/Actual'. Defined by the 2006 ISDA definitions 4.16b.
   */
  val ACT_ACT_ISDA: DayCount = DayCount.ACT_ACT_ISDA

  /**
   * The 'Act/Act ICMA' day count, which divides the actual number of days by the actual number of
   * days in the coupon period multiplied by the frequency.
   *
   * First, the underlying schedule period is obtained treating the first date as the start of the
   * schedule period. Second, if the period is a stub then nominal regular periods are created
   * matching the schedule frequency, working forwards or backwards from the known regular schedule
   * date, an end-of-month flag being used to handle month-ends; if the period is not a stub then
   * the schedule period is treated as a nominal period. Third, the result is the sum of a
   * calculation for each nominal period, each a division whose numerator is the actual number of
   * days in the nominal period - which could be zero in the case of a long stub - and whose
   * denominator is the length of the nominal period multiplied by the frequency. The first day in
   * the period is included, the last day is excluded.
   *
   * Because the nominal periods are determined ignoring business day adjustments, this day count
   * is recommended for use by bonds, not swaps.
   *
   * The two-argument `yearFraction(firstDate, secondDate)` cannot be used with this convention,
   * because schedule information is required.
   *
   * Also known as 'Actual/Actual ICMA' or 'Actual/Actual (Bond)'. Defined by the 2006 ISDA
   * definitions 4.16c and ICMA rule 251.1(iii) and 251.3 as later clarified by ISDA 'EMU and
   * market conventions'.
   */
  val ACT_ACT_ICMA: DayCount = DayCount.ACT_ACT_ICMA

  /**
   * The 'Act/Act AFB' day count, which divides the actual number of days by 366 if a leap day is
   * contained, or by 365 if not, with additional rules for periods over one year.
   *
   * The result is a simple division. The numerator is the actual number of days in the requested
   * period. The denominator is determined by examining the period end date, the date of the next
   * coupon: it is 366 if the schedule period contains February 29th, and 365 if it does not. The
   * first day in the schedule period is included, the last day is excluded.
   *
   * Also known as 'Actual/Actual AFB' or 'Actual/Actual (Euro)'. Defined by the Association
   * Francaise des Banques in September 1994 as 'Base Exact/Exact' in 'Definitions Communes
   * plusieurs Additifs Techniques'.
   *
   * This library implements the day count based on the original French documentation without the
   * ISDA clarification, whose roll-back rule has the strange effect that one day receives two days
   * of interest and the next receives none. The rule is interpreted here as rolling a period that
   * ends on the ''29th'' of February back to the 28th, or to the 29th in a leap year, which can be
   * argued to be closer to the original French than the ISDA "clarification".
   */
  val ACT_ACT_AFB: DayCount = DayCount.ACT_ACT_AFB

  /**
   * The 'Act/Act Year' day count, which divides the actual number of days by the number of days in
   * the year from the start date.
   *
   * The result is calculated in two parts - a number of whole years and the remaining part. If the
   * period is over one year, a number of years is added to the start date to reduce the remaining
   * period to less than a year; if the start date is February 29th then each time a year is added
   * the last valid day in February is chosen. The remaining period is then a simple division whose
   * numerator is the actual number of days in it and whose denominator is the actual number of days
   * in the year from the adjusted start date. The first day in the period is included, the last day
   * is excluded, and the result is the number of whole years plus the result of the division.
   *
   * For the period 2016-01-10 to 2016-01-20 the numerator is 10, as there are 10 days between the
   * dates, and the denominator is 366, as there are 366 days between 2016-01-10 and 2017-01-10.
   *
   * This is a variation of the 'Act/Act ICMA' day count. If 'Act/Act ICMA' is called with a
   * frequency of yearly, the next coupon date equal to the start date plus one year and the
   * end-of-month flag set to false, then the result is the same for periods less than a year.
   */
  val ACT_ACT_YEAR: DayCount = DayCount.ACT_ACT_YEAR

  /**
   * The 'Act/365 Actual' day count, which divides the actual number of days by 366 if a leap day is
   * contained, or by 365 if not.
   *
   * The result is a simple division. The numerator is the actual number of days in the requested
   * period. The denominator is 366 if the period contains February 29th, and 365 if it does not.
   * The first day in the period is excluded, the last day is included.
   *
   * Also known as 'Act/365A'.
   */
  val ACT_365_ACTUAL: DayCount = DayCount.ACT_365_ACTUAL

  /**
   * The 'Act/365L' day count, which divides the actual number of days by 365 or 366.
   *
   * The result is a simple division. The numerator is the actual number of days in the requested
   * period. The denominator is determined by examining the frequency and the period end date, the
   * date of the next coupon. If the frequency is annual then the denominator is 366 if the period
   * contains February 29th and 365 if it does not, the first day in the period being excluded and
   * the last day included. If the frequency is not annual, the denominator is 366 if the period end
   * date is in a leap year and 365 if it is not.
   *
   * The two-argument `yearFraction(firstDate, secondDate)` cannot be used with this convention,
   * because schedule information is required.
   *
   * Also known as 'Act/365 Leap year'. Defined by the 2006 ISDA definitions 4.16i and ICMA rule
   * 251.1(i) part 2 as later clarified by ICMA and the Swiss Exchange.
   */
  val ACT_365L: DayCount = DayCount.ACT_365L

  /**
   * The 'Act/360' day count, which divides the actual number of days by 360.
   *
   * The result is a simple division. The numerator is the actual number of days in the requested
   * period. The denominator is always 360.
   *
   * Also known as 'Actual/360' or 'French'. Defined by the 2006 ISDA definitions 4.16e and ICMA
   * rule 251.1(i) part 1.
   */
  val ACT_360: DayCount = DayCount.ACT_360

  /**
   * The 'Act/364' day count, which divides the actual number of days by 364.
   *
   * The result is a simple division. The numerator is the actual number of days in the requested
   * period. The denominator is always 364.
   *
   * Also known as 'Actual/364'.
   */
  val ACT_364: DayCount = DayCount.ACT_364

  /**
   * The 'Act/365F' day count, which divides the actual number of days by 365 (fixed).
   *
   * The result is a simple division. The numerator is the actual number of days in the requested
   * period. The denominator is always 365.
   *
   * Also known as 'Act/365', 'Actual/365 Fixed' or 'English'. Defined by the 2006 ISDA definitions
   * 4.16d.
   */
  val ACT_365F: DayCount = DayCount.ACT_365F

  /**
   * The 'Act/365.25' day count, which divides the actual number of days by 365.25.
   *
   * The result is a simple division. The numerator is the actual number of days in the requested
   * period. The denominator is always 365.25.
   */
  val ACT_365_25: DayCount = DayCount.ACT_365_25

  /**
   * The 'NL/360' day count, which divides the actual number of days omitting leap days by 360.
   *
   * The result is a simple division. The numerator is the actual number of days in the requested
   * period minus the number of occurrences of February 29th. The denominator is always 360. The
   * first day in the period is excluded, the last day is included.
   *
   * Also known as 'NoLeap/360', 'Actual/360 No Leap' or 'Actual (no leap year)/360'.
   */
  val NL_360: DayCount = DayCount.NL_360

  /**
   * The 'NL/365' day count, which divides the actual number of days omitting leap days by 365.
   *
   * The result is a simple division. The numerator is the actual number of days in the requested
   * period minus the number of occurrences of February 29th. The denominator is always 365. The
   * first day in the period is excluded, the last day is included.
   *
   * Also known as 'NoLeap/365', 'Actual/365 No Leap' or 'Actual (no leap year)/365'.
   */
  val NL_365: DayCount = DayCount.NL_365

  /**
   * The '30/360 ISDA' day count, which treats input day-of-month 31 specially.
   *
   * The result is calculated as `(360 * deltaYear + 30 * deltaMonth + deltaDay) / 360`, the day
   * difference being calculated once day-of-month adjustments have occurred. If the second
   * day-of-month is 31 and the first day-of-month is 30 or 31, the second day-of-month is changed
   * to 30. If the first day-of-month is 31, it is changed to 30.
   *
   * Also known as '30/360 U.S. Municipal' or '30/360 Bond Basis'. Defined by the 2006 ISDA
   * definitions 4.16f.
   */
  val THIRTY_360_ISDA: DayCount = DayCount.THIRTY_360_ISDA

  /**
   * The '30U/360' day count, which treats input day-of-month 31 and end of February specially.
   *
   * The result is calculated as `(360 * deltaYear + 30 * deltaMonth + deltaDay) / 360`, the day
   * difference being calculated once day-of-month adjustments have occurred. If the schedule uses
   * the end-of-month convention and both dates are the last day of February, the second
   * day-of-month is changed to 30. If the schedule uses the end-of-month convention and the first
   * date is the last day of February, the first day-of-month is changed to 30. If the second
   * day-of-month is 31 and the first day-of-month is 30 or 31, the second day-of-month is changed
   * to 30. If the first day-of-month is 31, it is changed to 30.
   *
   * This day count has different rules depending on whether the end-of-month rule applies or not,
   * which is set in the schedule information and defaults to true. The '30U/360 EOM' rule is
   * identical to this rule when the end-of-month convention applies, and the '30/360 ISDA' rule is
   * identical to it when the convention does not apply.
   *
   * Also known as '30/360 US', '30US/360' or '30/360 SIA'.
   */
  val THIRTY_U_360: DayCount = DayCount.THIRTY_U_360

  /**
   * The '30U/360 EOM' day count, which treats input day-of-month 31 and end of February specially.
   *
   * The result is calculated as `(360 * deltaYear + 30 * deltaMonth + deltaDay) / 360`, the day
   * difference being calculated once day-of-month adjustments have occurred. If both dates are the
   * last day of February, the second day-of-month is changed to 30. If the first date is the last
   * day of February, the first day-of-month is changed to 30. If the second day-of-month is 31 and
   * the first day-of-month is 30 or 31, the second day-of-month is changed to 30. If the first
   * day-of-month is 31, it is changed to 30.
   *
   * This day count is not dependent on the end-of-month flag of the schedule information. It is the
   * same as '30U/360' when the end-of-month convention applies, and would typically be used to be
   * explicit about that rule applying; in most cases '30U/360' should be used in preference.
   *
   * @see [[THIRTY_U_360]]
   */
  val THIRTY_U_360_EOM: DayCount = DayCount.THIRTY_U_360_EOM

  /**
   * The '30/360 PSA' day count, which treats input day-of-month 31 and end of February specially.
   *
   * The result is calculated as `(360 * deltaYear + 30 * deltaMonth + deltaDay) / 360`, the day
   * difference being calculated once day-of-month adjustments have occurred. If the first date is
   * the last day of its month, the first day-of-month is changed to 30. If the second day-of-month
   * is 31 and the first day-of-month is 30 or 31, the second day-of-month is changed to 30.
   *
   * PSA is the Public Securities Association, BMA is the Bond Market Association.
   */
  val THIRTY_360_PSA: DayCount = DayCount.THIRTY_360_PSA

  /**
   * The '30E/360 ISDA' day count, which treats input day-of-month 31 and end of February specially.
   *
   * The result is calculated as `(360 * deltaYear + 30 * deltaMonth + deltaDay) / 360`, the day
   * difference being calculated once day-of-month adjustments have occurred. If the first
   * day-of-month is 31, it is changed to 30. If the second day-of-month is 31, it is changed to 30.
   * If the first date is the last day of February, the first day-of-month is changed to 30. If the
   * second date is the last day of February and it is not the maturity date, the second
   * day-of-month is changed to 30.
   *
   * The two-argument `yearFraction(firstDate, secondDate)` cannot be used with this convention
   * where the second date is the last day of February, because the maturity date of the schedule is
   * then required.
   *
   * Also known as '30E/360 German' or 'German'. Defined by the 2006 ISDA definitions 4.16h.
   */
  val THIRTY_E_360_ISDA: DayCount = DayCount.THIRTY_E_360_ISDA

  /**
   * The '30E/360' day count, which treats input day-of-month 31 specially.
   *
   * The result is calculated as `(360 * deltaYear + 30 * deltaMonth + deltaDay) / 360`, the day
   * difference being calculated once day-of-month adjustments have occurred. If the first
   * day-of-month is 31, it is changed to 30. If the second day-of-month is 31, it is changed to 30.
   *
   * Also known as '30/360 ISMA', '30/360 European', '30S/360 Special German' or 'Eurobond'. Defined
   * by the 2006 ISDA definitions 4.16g and ICMA rule 251.1(ii) and 252.2.
   */
  val THIRTY_E_360: DayCount = DayCount.THIRTY_E_360

  /**
   * The '30E+/360' day count, which treats input day-of-month 31 specially.
   *
   * The result is calculated as `(360 * deltaYear + 30 * deltaMonth + deltaDay) / 360`, the day and
   * month differences being calculated once adjustments have occurred. If the first day-of-month is
   * 31, it is changed to 30. If the second day-of-month is 31, it is changed to 1 and the second
   * month is incremented.
   */
  val THIRTY_EPLUS_360: DayCount = DayCount.THIRTY_EPLUS_360

  /**
   * The '30E/365' day count, which treats input day-of-month 31 and end of February specially.
   *
   * The result is calculated as `(360 * deltaYear + 30 * deltaMonth + deltaDay) / 365`, the day
   * difference being calculated once day-of-month adjustments have occurred. If the first
   * day-of-month is the last day-of-month, it is changed to 30. If the second day-of-month is the
   * last day-of-month, it is changed to 30.
   *
   * Also known as '30/365 German'.
   */
  val THIRTY_E_365: DayCount = DayCount.THIRTY_E_365
}

