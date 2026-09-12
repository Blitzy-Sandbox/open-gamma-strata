/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.schedule

import java.time.DayOfWeek
import java.time.LocalDate
import java.time.temporal.TemporalAdjuster
import java.time.temporal.TemporalAdjusters

import scala.util.matching.Regex

import cats.Hash
import cats.Order
import cats.Show
import cats.data.EitherNec
import cats.data.NonEmptyList

import io.circe.Codec

import com.opengamma.strata.basics.date.HolidayCalendar
import com.opengamma.strata.basics.date.StandardHolidayCalendars
import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.Failure

/**
 * A convention defining how to roll dates.
 *
 * A [[PeriodicSchedule periodic schedule]] is determined using a periodic frequency. When
 * applying the frequency, the roll convention is used to fine tune the dates. This might involve
 * selecting the last day of the month, or the third Wednesday.
 *
 * To get the next date in the schedule, take the base date and the [[Frequency periodic
 * frequency]]. Once this date is calculated, the roll convention is applied to produce the next
 * schedule date.
 *
 * A convention is pure: [[adjust]] is a function of the date it is given and of the fixed data
 * the convention carries, so the same date always produces the same result. Nothing is read from
 * reference data, from configuration or from the class path.
 *
 * ===A closed family===
 *
 * The family has exactly 45 members, every one of them declared in this file:
 *
 *  - the eight rule-based conventions `None`, `EOM`, `IMM`, `IMMCAD`, `IMMAUD`, `IMMNZD`, `SFE`
 *    and `TBILL`;
 *  - the thirty day-of-month conventions `Day1` to `Day30`;
 *  - the seven day-of-week conventions `DayMon` to `DaySun`.
 *
 * The type is `sealed`, its constructor is not visible outside this package, the two
 * parameterised implementations are private to the companion, and the name lookup is built from
 * those 45 members alone, so nothing can add a forty-sixth. A `match` over a convention is
 * therefore checked for exhaustiveness by the compiler.
 *
 * The members are reached in three ways, all of which yield the same objects:
 *
 * {{{
 * RollConvention.EOM                  // the member itself
 * RollConventions.DAY_15              // the identifier the ported library used
 * RollConvention.parse("Day_31")      // text, leniently resolved - to EOM
 * }}}
 *
 * ===What this replaces===
 *
 * The type being ported was an interface whose implementations were discovered while the program
 * ran: a registry read the constants of an enum reflectively, merged in the day-based members
 * supplied by a second lookup class, and merged in whatever external spellings and lenient
 * rewrites it found declared in a configuration resource on the class path. The public constants
 * were indirected through that registry so that configuration could replace them. None of that
 * machinery survives. What the configuration ''declared'' does survive in full: the 44 rows of
 * the FpML group of external names and the 11 ordered lenient rewrites are transcribed into this
 * file as Scala data and handed to the shared name lookup, so text that resolved before resolves
 * now.
 *
 * ===Divergences from the ported type===
 *
 * These are the deliberate differences, recorded here because they belong in the migration note:
 *
 *  - '''Fixed calendars instead of an ambient lookup.''' The ported `IMMCAD`, `IMMAUD` and
 *    `TBILL` captured their holiday calendars from standard reference data while their class
 *    initialised, falling back to a Saturday/Sunday calendar if the lookup missed. The members
 *    here hold the built-in calendar values of
 *    [[com.opengamma.strata.basics.date.StandardHolidayCalendars]] directly - `GBLO`, `CATO`
 *    combined with `CAMO`, `AUSY` and `USNY`. They are the same fixed calendars, reached as data
 *    rather than through a lookup, so no fallback is needed and no reference data appears in this
 *    file. [[adjust]] keeps the signature it had: a date in, a date out, with no reference data
 *    parameter and no error channel.
 *  - '''`NONE` carries the name `None`.''' The member whose canonical name is `None` is declared
 *    as `NONE`, because a member named `None` inside the companion would shadow `scala.None`
 *    throughout it. `NONE` is also the identifier the ported constants holder used, so the
 *    rename is only of the Scala member, never of the name: `RollConvention.NONE.name` is
 *    `"None"` and that is the text the codec writes and [[RollConvention.parse]] reads.
 *  - '''Rejection is reported rather than raised.''' The ported `of` and `ofDayOfMonth` raised an
 *    error for text or a number they did not accept. [[RollConvention.parse]] and
 *    [[RollConvention.ofDayOfMonth]] report it instead, as a
 *    [[com.opengamma.strata.collect.result.Failure]] on the left of an `Either`.
 *  - '''Accessors are renamed to Scala form.''' `getName` is [[name]] and `getDayOfMonth` is
 *    [[dayOfMonth]]. The values they answer with are unchanged.
 *  - '''Java serialization is gone.''' No member is serializable, and the resolution hooks that
 *    served it are not ported. JSON is the wire form, through the codec on the companion.
 *
 * Every member is immutable and safe to share between threads.
 *
 * @param name  the unique name of the convention, which is its identity in text and on the wire
 */
sealed abstract class RollConvention private[schedule] (val name: String) extends Named {

  /**
   * Adjusts the date according to the rules of this roll convention.
   *
   * See the description of each member to understand the rule applied. Every rule is total over
   * the dates `java.time` can represent, and the three conventions that consult a holiday
   * calendar reject only what that calendar rejects, which is a year outside 0 to 9999.
   *
   * It is recommended to use [[next]] and [[previous]] rather than calling this directly, since
   * those two combine the adjustment with the periodic frequency of a schedule.
   *
   * @param date  the date to adjust
   * @return the adjusted date
   */
  def adjust(date: LocalDate): LocalDate

  /**
   * Checks whether the date matches the rules of this roll convention.
   *
   * The default is the general test - a date matches when adjusting it changes nothing - and the
   * two day-based families narrow it: a day-of-month convention also matches the last day of
   * February where its own day-of-month is later than February has days, and a day-of-week
   * convention matches on its day of the week alone.
   *
   * @param date  the date to check
   * @return true if the date matches this convention
   */
  def matches(date: LocalDate): Boolean = date == adjust(date)

  //-------------------------------------------------------------------------
  /**
   * Calculates the next date in the sequence after the specified date.
   *
   * The periodic frequency is added to the date and the result is adjusted by the rule of this
   * convention. Where that lands on or before the date supplied - which a frequency shorter than
   * the convention's own cycle does, a one-day frequency under a monthly convention being the
   * common case - a month is added to the date instead and the adjustment is applied to that, so
   * the result is always after the date supplied.
   *
   * This implementation suits every month-based convention. The day-of-week family overrides it,
   * because adding a week-based frequency and then rolling forward to the required day of the
   * week always moves past the date supplied without the correction.
   *
   * @param date  the date to adjust
   * @param periodicFrequency  the periodic frequency of the schedule
   * @return the adjusted date, always after the date supplied
   */
  def next(date: LocalDate, periodicFrequency: Frequency): LocalDate = {
    val calculated = adjust(periodicFrequency.addTo(date))
    if (calculated.isAfter(date)) calculated else adjust(date.plusMonths(1L))
  }

  /**
   * Calculates the previous date in the sequence before the specified date.
   *
   * This is the mirror of [[next]]: the periodic frequency is subtracted and the result adjusted,
   * and where that lands on or after the date supplied a month is subtracted from the date
   * instead, so the result is always before the date supplied.
   *
   * @param date  the date to adjust
   * @param periodicFrequency  the periodic frequency of the schedule
   * @return the adjusted date, always before the date supplied
   */
  def previous(date: LocalDate, periodicFrequency: Frequency): LocalDate = {
    val calculated = adjust(periodicFrequency.subtractFrom(date))
    if (calculated.isBefore(date)) calculated else adjust(date.minusMonths(1L))
  }

  //-------------------------------------------------------------------------
  /**
   * The day-of-month that this roll convention implies, zero where it implies none.
   *
   * A day-of-month convention answers with its own day, `EOM` answers with 31 - the two agree in
   * every month, because the conventions for 29, 30 and 31 all roll to the end of February - and
   * every other convention answers with zero.
   *
   * This is the accessor the ported type called `getDayOfMonth`.
   *
   * @return the day-of-month implied, zero if not applicable
   */
  def dayOfMonth: Int = 0

  /**
   * Renders this convention as its unique name.
   *
   * The name is the only text form of a convention: it is what the `Show` instance produces,
   * what the codec writes, and what [[RollConvention.parse]] reads back.
   *
   * @return the unique name
   */
  override def toString: String = name
}

/**
 * The 45 roll conventions, together with their name lookup, factories and typeclass instances.
 *
 * The eight rule-based members are `case object`s, so each is a singleton whose identity is its
 * own and whose pattern match needs no extractor. The thirty day-of-month members and the seven
 * day-of-week members are instances of two implementations private to this object, built once
 * here and published only through [[values]], the factories and the constants of
 * [[RollConventions]] - which is what makes them singletons too, so that reference equality,
 * `==` and equality by name all agree for every member of the family.
 */
object RollConvention {

  /** The first day-of-month a convention can name. */
  private val FirstDayOfMonth: Int = 1

  /**
   * The highest day-of-month with a convention of its own, the 31st being `EOM`.
   *
   * The conventions for 29, 30 and 31 all roll to the end of February, so a convention for the
   * 31st would differ from `EOM` in no month, which is why the family stops at 30 and the ported
   * library mapped 31 to `EOM` - in its FpML table as well as in its factory.
   */
  private val HighestDayOfMonthMember: Int = 30

  /** The day-of-month that names `EOM` rather than a day-of-month convention. */
  private val EndOfMonthDayOfMonth: Int = 31

  /** The month value of February, the only month shorter than the highest conventions. */
  private val FebruaryMonthValue: Int = 2

  /**
   * The lowest day-of-month whose convention rolls to the end of February.
   *
   * A convention for the 29th, 30th or 31st cannot be honoured in every February, so in February
   * all three select the last day of the month - the 28th, or the 29th in a leap year.
   */
  private val LowestDayOfMonthRolledInFebruary: Int = 29

  /** The day-of-month that the New Zealand dollar convention rolls forward from. */
  private val ImmNzdDayOfMonth: Int = 9

  /**
   * The number of London banking days before the third Wednesday that `IMMCAD` selects.
   *
   * Negative because it is a shift backwards through the London calendar.
   */
  private val ImmCadLondonBankingDays: Int = -2

  //-------------------------------------------------------------------------
  /** The third Wednesday of the month of the date, the date the IMM conventions are built on. */
  private val ThirdWednesday: TemporalAdjuster =
    TemporalAdjusters.dayOfWeekInMonth(3, DayOfWeek.WEDNESDAY)

  /** The second Friday of the month of the date. */
  private val SecondFriday: TemporalAdjuster =
    TemporalAdjusters.dayOfWeekInMonth(2, DayOfWeek.FRIDAY)

  /** The date itself when it is a Wednesday, otherwise the Wednesday after it. */
  private val NextOrSameWednesday: TemporalAdjuster =
    TemporalAdjusters.nextOrSame(DayOfWeek.WEDNESDAY)

  /** The date itself when it is a Monday, otherwise the Monday after it. */
  private val NextOrSameMonday: TemporalAdjuster =
    TemporalAdjusters.nextOrSame(DayOfWeek.MONDAY)

  //-------------------------------------------------------------------------
  /**
   * The 'None' roll convention, which makes no adjustment.
   *
   * When calculating a schedule, there is no further adjustment after the periodic frequency is
   * added or subtracted, so the schedule keeps the day-of-month of its start date and shortens
   * only where a month is too short to hold it.
   *
   * The canonical name of this member is `None`; the Scala member is `NONE` so that it cannot
   * shadow `scala.None` inside this object, and `NONE` is also the identifier the ported
   * constants holder published it under.
   */
  case object NONE extends RollConvention("None") {
    override def adjust(date: LocalDate): LocalDate = date
  }

  /**
   * The 'EOM' roll convention, which adjusts the date to the end of the month.
   *
   * The date is moved to the last valid day of its month, so the year and month of the result
   * are those of the date supplied. This convention is intended for use with periods that are a
   * multiple of months.
   */
  case object EOM extends RollConvention("EOM") {
    override def adjust(date: LocalDate): LocalDate = date.withDayOfMonth(date.lengthOfMonth)

    /**
     * The day-of-month implied, which is 31.
     *
     * `EOM` is equivalent to the 31st in FpML in most cases, because the conventions for 29 and
     * 30 also have to adjust to the end of February.
     */
    override def dayOfMonth: Int = EndOfMonthDayOfMonth
  }

  /**
   * The 'IMM' roll convention, which adjusts the date to the third Wednesday.
   *
   * The date is moved to the third Wednesday of its month, so the year and month of the result
   * are those of the date supplied. This convention is intended for use with periods that are a
   * multiple of months.
   */
  case object IMM extends RollConvention("IMM") {
    override def adjust(date: LocalDate): LocalDate = date.`with`(ThirdWednesday)
  }

  /**
   * The 'IMMCAD' roll convention, which adjusts the date to two days before the third Wednesday.
   *
   * The date is moved to two London banking days before the third Wednesday of its month, and
   * then earlier again to a business day of the combined Montreal and Toronto calendars. The
   * year and month of the result are those of the date supplied. This convention is intended for
   * use with periods that are a multiple of months.
   *
   * The two calendars are the built-in `GBLO` calendar and the built-in `CATO` and `CAMO`
   * calendars combined, held as `lazy val`s: the built-in calendars are themselves generated
   * lazily, so deferring these keeps the cost of generating three calendars off the
   * initialisation of this family and leaves the order in which the two files initialise
   * immaterial. Where the ported convention resolved the same three calendar identifiers against
   * standard reference data as its class initialised, this one names the calendar values
   * themselves - the same fixed calendars, without a lookup.
   *
   * Note that no `GBLO`, `CATO` or `CAMO` holiday currently falls where it would change the
   * result of this rule.
   */
  case object IMMCAD extends RollConvention("IMMCAD") {

    /** The London calendar, through which the two banking days are counted back. */
    private lazy val london: HolidayCalendar = StandardHolidayCalendars.GBLO

    /** The Toronto and Montreal calendars combined, as the ported convention combined them. */
    private lazy val canada: HolidayCalendar =
      StandardHolidayCalendars.CATO.combinedWith(StandardHolidayCalendars.CAMO)

    override def adjust(date: LocalDate): LocalDate = {
      val thirdWednesday = date.`with`(ThirdWednesday)
      canada.previousOrSame(london.shift(thirdWednesday, ImmCadLondonBankingDays))
    }
  }

  /**
   * The 'IMMAUD' roll convention, which adjusts the date to the day before the second Friday.
   *
   * The date is moved to the second Friday of its month and then to the previous Sydney business
   * day, which is the Thursday before in a week with no Sydney holiday. The year and month of
   * the result are those of the date supplied. This convention is intended for use with periods
   * that are a multiple of months.
   *
   * The Sydney calendar is the built-in `AUSY` calendar, held as a `lazy val` for the reason
   * given on [[IMMCAD]]. Note that no `AUSY` holiday currently falls where it would change the
   * result of this rule.
   */
  case object IMMAUD extends RollConvention("IMMAUD") {

    /** The Sydney calendar, through which the single business day is counted back. */
    private lazy val sydney: HolidayCalendar = StandardHolidayCalendars.AUSY

    override def adjust(date: LocalDate): LocalDate = sydney.previous(date.`with`(SecondFriday))
  }

  /**
   * The 'IMMNZD' roll convention, which adjusts the date to the first Wednesday on or after the
   * ninth day of the month.
   *
   * The date is moved to the ninth day of its month and then forward to a Wednesday, so a ninth
   * that is already a Wednesday is the result. The year and month of the result are those of the
   * date supplied. This convention is intended for use with periods that are a multiple of
   * months.
   */
  case object IMMNZD extends RollConvention("IMMNZD") {
    override def adjust(date: LocalDate): LocalDate =
      date.withDayOfMonth(ImmNzdDayOfMonth).`with`(NextOrSameWednesday)
  }

  /**
   * The 'SFE' roll convention, which adjusts the date to the second Friday.
   *
   * The date is moved to the second Friday of its month, so the year and month of the result are
   * those of the date supplied. No holiday calendar takes part, which is what distinguishes this
   * convention from [[IMMAUD]]. This convention is intended for use with periods that are a
   * multiple of months.
   */
  case object SFE extends RollConvention("SFE") {
    override def adjust(date: LocalDate): LocalDate = date.`with`(SecondFriday)
  }

  /**
   * The 'TBILL' roll convention, which adjusts the date to the next Monday.
   *
   * The date is moved forward to a Monday, and then forward again to a New York business day
   * where that Monday is a New York holiday - so a week whose Monday is a public holiday rolls
   * to the Tuesday. Unlike the month-based conventions, the result may fall in the month after
   * the date supplied.
   *
   * The New York calendar is the built-in `USNY` calendar, held as a `lazy val` for the reason
   * given on [[IMMCAD]].
   */
  case object TBILL extends RollConvention("TBILL") {

    /** The New York calendar, which moves a holiday Monday forward. */
    private lazy val newYork: HolidayCalendar = StandardHolidayCalendars.USNY

    override def adjust(date: LocalDate): LocalDate =
      newYork.nextOrSame(date.`with`(NextOrSameMonday))
  }

  //-------------------------------------------------------------------------
  /**
   * The implementation of the day-of-month conventions, `Day1` to `Day30`.
   *
   * The class is private to this object, so no code outside this file can name it or construct
   * one: the thirty instances the family has are built once into [[DomValues]] and reached only
   * through [[values]], [[ofDayOfMonth]] and the constants of [[RollConventions]]. That makes
   * every day-of-month convention a singleton, which is what the ported implementation relied on
   * for its equality - it defined none - and what keeps reference equality, `==` and equality by
   * name in agreement here.
   *
   * @param dayOfMonth  the day-of-month this convention selects, from 1 to 30
   */
  private final class Dom(override val dayOfMonth: Int) extends RollConvention(s"Day$dayOfMonth") {

    /**
     * Adjusts the date to this day-of-month, or to the end of February where February is too
     * short to hold it.
     *
     * The February rule applies to the conventions for the 29th and the 30th, which is why they
     * and `EOM` agree in that month.
     *
     * @param date  the date to adjust
     * @return the date within the month of the date supplied
     */
    override def adjust(date: LocalDate): LocalDate =
      if (dayOfMonth >= LowestDayOfMonthRolledInFebruary && date.getMonthValue == FebruaryMonthValue) {
        date.withDayOfMonth(date.lengthOfMonth)
      } else {
        date.withDayOfMonth(dayOfMonth)
      }

    /**
     * Checks whether the date is this day-of-month, or the last day of a February too short to
     * hold it.
     *
     * The second clause is what makes the 29th and 30th conventions match the 28th of February
     * in a common year and the 29th in a leap year, in step with [[adjust]].
     *
     * @param date  the date to check
     * @return true if the date matches this convention
     */
    override def matches(date: LocalDate): Boolean =
      date.getDayOfMonth == dayOfMonth ||
        (date.getMonthValue == FebruaryMonthValue &&
          dayOfMonth >= date.lengthOfMonth &&
          date.getDayOfMonth == date.lengthOfMonth)
  }

  /**
   * The implementation of the day-of-week conventions, `DayMon` to `DaySun`.
   *
   * Private to this object for the same reason as [[Dom]]: the seven instances are built once
   * into [[DowValues]], so each is a singleton.
   *
   * Both [[next]] and [[previous]] are overridden, because the correction the month-based default
   * applies is unnecessary here - adding a frequency and then rolling forward to the required day
   * of the week always lands after the date supplied, and subtracting and rolling backwards
   * always lands before it.
   *
   * @param dayOfWeek  the day-of-week this convention selects
   * @param conventionName  the unique name of this convention, `Day` followed by the three-letter
   *   form of the day of the week
   */
  private final class Dow(dayOfWeek: DayOfWeek, conventionName: String)
      extends RollConvention(conventionName) {

    /** The date itself when it falls on this day of the week, otherwise the next such day. */
    private val nextOrSame: TemporalAdjuster = TemporalAdjusters.nextOrSame(dayOfWeek)

    /** The date itself when it falls on this day of the week, otherwise the previous such day. */
    private val previousOrSame: TemporalAdjuster = TemporalAdjusters.previousOrSame(dayOfWeek)

    /**
     * Adjusts the date forward to this day of the week, by up to six days.
     *
     * @param date  the date to adjust
     * @return the date itself, or the next date falling on this day of the week
     */
    override def adjust(date: LocalDate): LocalDate = date.`with`(nextOrSame)

    /**
     * Checks whether the date falls on this day of the week.
     *
     * @param date  the date to check
     * @return true if the date falls on this day of the week
     */
    override def matches(date: LocalDate): Boolean = date.getDayOfWeek == dayOfWeek

    /**
     * Calculates the next date, by adding the frequency and rolling forward to this day of the
     * week.
     *
     * @param date  the date to adjust
     * @param periodicFrequency  the periodic frequency of the schedule
     * @return the adjusted date
     */
    override def next(date: LocalDate, periodicFrequency: Frequency): LocalDate =
      periodicFrequency.addTo(date).`with`(nextOrSame)

    /**
     * Calculates the previous date, by subtracting the frequency and rolling backwards to this
     * day of the week.
     *
     * @param date  the date to adjust
     * @param periodicFrequency  the periodic frequency of the schedule
     * @return the adjusted date
     */
    override def previous(date: LocalDate, periodicFrequency: Frequency): LocalDate =
      periodicFrequency.subtractFrom(date).`with`(previousOrSame)
  }

  //-------------------------------------------------------------------------
  /**
   * The thirty day-of-month conventions, indexed by day-of-month less one.
   *
   * Built once, by tabulating the days rather than by filling an array in a loop as the ported
   * implementation did, so the table holds no mutable state at any point in its construction.
   */
  private val DomValues: Vector[RollConvention] =
    Vector.tabulate(HighestDayOfMonthMember)(index => new Dom(index + FirstDayOfMonth))

  /**
   * The unique names of the seven day-of-week conventions, in the order of `java.time.DayOfWeek`.
   *
   * The ported implementation sliced these names out of one string in six-character chunks; they
   * are written out here, so that the name of each convention is legible at the point it is
   * declared.
   */
  private val DowNames: Vector[String] =
    Vector("DayMon", "DayTue", "DayWed", "DayThu", "DayFri", "DaySat", "DaySun")

  /**
   * The seven day-of-week conventions, indexed by the value of the day of the week less one.
   *
   * The index is the `java.time.DayOfWeek` numbering, Monday being 1, which is what lets
   * [[ofDayOfWeek]] select a convention without a lookup.
   */
  private val DowValues: Vector[RollConvention] =
    DowNames.zipWithIndex.map {
      case (conventionName, index) => new Dow(DayOfWeek.of(index + 1), conventionName)
    }

  //-------------------------------------------------------------------------
  /**
   * The complete set of roll conventions, in declaration order.
   *
   * The order is the eight rule-based conventions, then `Day1` to `Day30`, then `DayMon` to
   * `DaySun` - the order in which the ported library's two providers contributed them, which is
   * also the order in which the members claim their lookup keys and the order a report over the
   * family follows. It is not the order the `Order` instance below imposes, which is alphabetical
   * by name. The list is non-empty by construction and holds exactly 45 members.
   *
   * @return the 45 conventions, in declaration order
   */
  val values: NonEmptyList[RollConvention] =
    NonEmptyList.of(NONE, EOM, IMM, IMMCAD, IMMAUD, IMMNZD, SFE, TBILL) ++
      DomValues.toList ++ DowValues.toList

  //-------------------------------------------------------------------------
  /**
   * The spellings this family publishes for the FpML protocol, each mapped to a canonical name.
   *
   * These are the 44 rows of the FpML group of external names that the configuration resource of
   * the ported library declared, transcribed unchanged: the six rule-based conventions FpML
   * names, the day-of-month numbers 1 to 30, the number 31 mapped to `EOM` - FpML has no
   * end-of-month spelling of its own - and the three-letter days of the week.
   *
   * Two conventions of this family are deliberately absent, because the resource did not declare
   * them: `IMMCAD` and `TBILL`, neither of which FpML defines. Comparing the size of this table
   * against the captured reference-data manifest is what holds that count to 44.
   *
   * The rows take part in no lookup - `MON` and `31` are resolved by the lenient patterns below,
   * which happen to accept them - and exist so that a caller writing or reading that protocol can
   * map between the two vocabularies explicitly, through `NamedEnum.externalNames`.
   */
  private val FpMLNames: Map[String, String] =
    Map(
      "NONE" -> "None",
      "EOM" -> "EOM",
      "IMM" -> "IMM",
      "IMMAUD" -> "IMMAUD",
      "IMMNZD" -> "IMMNZD",
      "SFE" -> "SFE",
      "1" -> "Day1",
      "2" -> "Day2",
      "3" -> "Day3",
      "4" -> "Day4",
      "5" -> "Day5",
      "6" -> "Day6",
      "7" -> "Day7",
      "8" -> "Day8",
      "9" -> "Day9",
      "10" -> "Day10",
      "11" -> "Day11",
      "12" -> "Day12",
      "13" -> "Day13",
      "14" -> "Day14",
      "15" -> "Day15",
      "16" -> "Day16",
      "17" -> "Day17",
      "18" -> "Day18",
      "19" -> "Day19",
      "20" -> "Day20",
      "21" -> "Day21",
      "22" -> "Day22",
      "23" -> "Day23",
      "24" -> "Day24",
      "25" -> "Day25",
      "26" -> "Day26",
      "27" -> "Day27",
      "28" -> "Day28",
      "29" -> "Day29",
      "30" -> "Day30",
      "31" -> "EOM",
      "MON" -> "DayMon",
      "TUE" -> "DayTue",
      "WED" -> "DayWed",
      "THU" -> "DayThu",
      "FRI" -> "DayFri",
      "SAT" -> "DaySat",
      "SUN" -> "DaySun"
    )

  /**
   * The lenient rewrites of this family, in the order they are applied.
   *
   * These are the 11 rows of the lenient patterns that the configuration resource of the ported
   * library declared, in the order that resource listed them, and the order is part of the data:
   * [[parse]] folds its input to upper case and then applies every pattern in turn, a pattern
   * whose expression matches the whole of the current text replacing that text, so a later
   * pattern sees what an earlier one produced.
   *
   * Here the order is load-bearing rather than incidental. The row for 31 is declared before the
   * row for 30 and both before the row that captures a one- or two-digit day, so text naming the
   * 31st reaches `EOM` and is never rewritten to a `Day31` that no member carries. Reordering
   * these rows would change which text resolves and to what.
   *
   * The chain is what lets a bare number, a name with or without an underscore, and the
   * screaming-snake spellings of the constant identifiers all reach the same member:
   *
   * {{{
   * parse("31")      // EOM    - by number
   * parse("Day_31")  // EOM    - by spelling, underscore and all
   * parse("15")      // Day15
   * parse("DAY_MON") // DayMon - by constant identifier
   * parse("thu")     // DayThu
   * }}}
   *
   * The replacement of the third row refers back to the group its expression captured, which is
   * the digits of the day, and the replacements of the others are literal names. Each expression
   * is matched insensitively to case by the name lookup, which is why they are written here in
   * the mixed case of the original rows rather than folded by hand.
   */
  private val LenientPatterns: List[(Regex, String)] =
    List(
      "(Day_?)?31".r -> "EOM",
      "(Day_?)?30".r -> "Day30",
      "(Day_?)?([1-2]?[0-9])".r -> "Day$2",
      "NONE".r -> "None",
      "(Day_?)?MON".r -> "DayMon",
      "(Day_?)?TUE".r -> "DayTue",
      "(Day_?)?WED".r -> "DayWed",
      "(Day_?)?THU".r -> "DayThu",
      "(Day_?)?FRI".r -> "DayFri",
      "(Day_?)?SAT".r -> "DaySat",
      "(Day_?)?SUN".r -> "DaySun"
    )

  /**
   * The name lookup for this family.
   *
   * This instance is the single route from text to a convention, and it is built from [[values]]
   * and the two transcribed tables alone. The family declares no alternate spelling: the ported
   * library registered every day-based convention under its name and under that name folded to
   * upper case, and the shared lookup derives both keys from [[values]] for every member, so
   * `Day15` and `DAY15` resolve without a table and a table would only repeat what is already
   * derived. Nothing is read from a class or from the class path, so the name space of the family
   * is fixed when this file is compiled.
   *
   * The instance also carries the tables themselves - `lenientPatterns` and `externalNamesRaw` -
   * which is how a caller or a specification reads the transcribed data back without this object
   * having to publish it twice.
   *
   * @return the name lookup for the 45 conventions
   */
  implicit val namedEnum: NamedEnum[RollConvention] =
    NamedEnum.of(values, Map.empty, LenientPatterns, Map("FpML" -> FpMLNames), "RollConvention")

  //-------------------------------------------------------------------------
  /**
   * Obtains the convention with the specified canonical name, if one exists.
   *
   * The match is exact against the canonical names and against those names folded to upper case,
   * so `Day15` and `DAY15` resolve while `day15` does not. No lenient pattern is applied. Use
   * [[parse]] to accept text whose shape is not known in advance.
   *
   * @param name  the name to look up
   * @return the convention with that name, or `None` when no convention has it
   */
  def valueOf(name: String): Option[RollConvention] = namedEnum.valueOf(name)

  /**
   * Parses a convention from text, applying the leniency this family declares.
   *
   * The exact lookup of [[valueOf]] is tried first. Failing that, the text is folded to upper
   * case and the 11 lenient patterns are applied in order before the exact lookup is tried once
   * more, so a bare number, an underscored spelling and a constant identifier all resolve:
   *
   * {{{
   * parse("Day15")   // Right(Day15) - the canonical name
   * parse("15")      // Right(Day15) - by number
   * parse("Day_31")  // Right(EOM)   - the 31st is the end of the month
   * parse("none")    // Right(None)
   * parse("Rubbish") // Left - text this family has never accepted
   * }}}
   *
   * Where the type being ported signalled unrecognised text by raising an error, this method
   * reports it as a value: the result is `Left` of a chain holding one
   * [[com.opengamma.strata.collect.result.Failure]] whose reason is `PARSING` and whose message
   * names both this family and the text that could not be resolved.
   *
   * @param name  the text to parse
   * @return the convention the text names, or the failure describing why it names none
   */
  def parse(name: String): EitherNec[Failure, RollConvention] = namedEnum.parse(name)

  //-------------------------------------------------------------------------
  /**
   * Obtains the convention for the specified day-of-month.
   *
   * The convention adjusts a date to that day-of-month, keeping its year and month. Where the
   * month is shorter than the day requested the last day of the month is chosen, which is why
   * passing 31 yields [[EOM]] rather than a convention of its own.
   *
   * The ported factory raised an error for a day outside 1 to 31; this one reports it, as a
   * [[com.opengamma.strata.collect.result.Failure]] whose reason is `INVALID` and whose message
   * is the one the ported factory used.
   *
   * {{{
   * ofDayOfMonth(15) // Right(Day15)
   * ofDayOfMonth(31) // Right(EOM)
   * ofDayOfMonth(32) // Left(Failure.Invalid("Invalid day-of-month: 32"))
   * }}}
   *
   * @param dayOfMonth  the day-of-month, from 1 to 31
   * @return the convention for that day-of-month, or the failure describing why there is none
   */
  def ofDayOfMonth(dayOfMonth: Int): Either[Failure, RollConvention] =
    if (dayOfMonth == EndOfMonthDayOfMonth) {
      Right(EOM)
    } else if (dayOfMonth < FirstDayOfMonth || dayOfMonth > HighestDayOfMonthMember) {
      Left(Failure.Invalid(s"Invalid day-of-month: $dayOfMonth"))
    } else {
      Right(DomValues(dayOfMonth - FirstDayOfMonth))
    }

  /**
   * Obtains the convention for a day-of-month already known to be valid.
   *
   * This is the total form of [[ofDayOfMonth]], for the callers within this package that derive
   * their argument from a date: `LocalDate.getDayOfMonth` is between 1 and 31 by construction, as
   * is the greater of two such values, so no caller of this method can supply a number the family
   * has no convention for, and threading an `Either` through those call sites would describe a
   * failure that cannot arise.
   *
   * The contract is nonetheless checked rather than assumed, through
   * [[com.opengamma.strata.collect.ArgCheck]]: a number outside 1 to 31 is a programming error in
   * this package, and it fails immediately and loudly rather than silently selecting the wrong
   * convention. Because the check belongs to `ArgCheck`, this file raises nothing itself.
   *
   * It is not part of the public surface of this family; public callers use [[ofDayOfMonth]].
   *
   * @param dayOfMonth  the day-of-month, from 1 to 31
   * @return the convention for that day-of-month
   * @throws IllegalArgumentException if the day-of-month is outside 1 to 31
   */
  private[schedule] def ofDayOfMonthUnsafe(dayOfMonth: Int): RollConvention = {
    ArgCheck.inRangeInclusive(dayOfMonth, FirstDayOfMonth, EndOfMonthDayOfMonth, "dayOfMonth")
    if (dayOfMonth == EndOfMonthDayOfMonth) EOM else DomValues(dayOfMonth - FirstDayOfMonth)
  }

  /**
   * Obtains the convention for the specified day-of-week.
   *
   * The convention adjusts a date to that day of the week, and is intended for use with periods
   * that are a multiple of weeks. It is total: every one of the seven days has a convention, so
   * there is nothing to report.
   *
   * In `adjust`, a date that is not the required day of the week is moved forward to the next
   * occurrence of it, up to six days later. In `next`, the day of the week is selected after the
   * frequency is added; in `previous`, it is selected backwards after the frequency is
   * subtracted.
   *
   * @param dayOfWeek  the day-of-week
   * @return the convention for that day-of-week
   */
  def ofDayOfWeek(dayOfWeek: DayOfWeek): RollConvention = DowValues(dayOfWeek.getValue - 1)

  //-------------------------------------------------------------------------
  /**
   * The ordering and hashing of conventions.
   *
   * This is the only equality-bearing instance of the type: `Order` extends `Eq` and `Hash`
   * extends `Eq`, so summoning any of the three yields this one value and the three can never
   * disagree. Comparison is over `name`, which makes the ordering alphabetical rather than the
   * declaration order of [[values]], and equality follows it - the 45 names are distinct, so two
   * conventions compare equal if, and only if, they are the same convention. Since every member
   * is a singleton, this agrees with `==` and with reference equality as well.
   *
   * @return the ordering of conventions by name, which is also their hashing
   */
  implicit val order: Order[RollConvention] with Hash[RollConvention] = NamedEnum.orderByName

  /**
   * The rendering of conventions as text.
   *
   * A convention renders as its canonical name, which is what `toString` produces as well, so
   * the two ways of putting a convention into a message agree.
   *
   * @return the rendering of a convention as its canonical name
   */
  implicit val show: Show[RollConvention] = NamedEnum.showByName

  /**
   * The JSON codec for conventions.
   *
   * A convention is written as the bare string of its canonical name - `"EOM"`, `"Day15"`,
   * `"DayMon"` - and never as an object, which is the single-string form the type being ported
   * wrote through its string conversion, so a document written by either side names the same
   * convention. Decoding goes through [[parse]], so the leniency of the two is identical and
   * unresolvable text is reported as a decoding failure rather than raised.
   *
   * @return the codec reading and writing a convention as its canonical name
   */
  implicit val codec: Codec[RollConvention] = Codecs.namedEnumCodec
}

/**
 * Constants for the standard roll conventions, published under the identifiers the ported
 * library used.
 *
 * The purpose of a roll convention is to define how to roll dates when building a schedule. The
 * standard approach to building a schedule is based on unadjusted dates, which do not have a
 * business day convention applied. To get the next date in the schedule, take the base date and
 * the [[Frequency periodic frequency]]. Once this date is calculated, the roll convention is
 * applied to produce the next schedule date.
 *
 * In most cases the specific values for day-of-month and day-of-week are not needed. A one month
 * periodic frequency will naturally select the same day-of-month as the input date, thus the
 * day-of-month does not need to be additionally specified.
 *
 * Every one of these 45 constants is a member of [[RollConvention]], exposed under the name the
 * original constants holder gave it so that a call site reading `RollConventions.DAY_15` ports
 * across unchanged. The values are the same objects as the members of the companion, so a
 * constant taken from here and the matching member are indistinguishable - including by `eq`, by
 * `==` and in a pattern match.
 *
 * Unlike the holder being ported, these constants are not indirected through a registry: each
 * one names its member directly, because the family is closed and no configuration can replace a
 * member of it.
 */
object RollConventions {

  /**
   * The 'None' roll convention.
   *
   * The input date will not be adjusted.
   *
   * When calculating a schedule, there will be no further adjustment after the periodic
   * frequency is added or subtracted.
   */
  val NONE: RollConvention = RollConvention.NONE

  /**
   * The 'EOM' roll convention which adjusts the date to the end of the month.
   *
   * The input date will be adjusted to ensure it is the last valid day of the month. The year
   * and month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val EOM: RollConvention = RollConvention.EOM

  /**
   * The 'IMM' roll convention which adjusts the date to the third Wednesday.
   *
   * The input date will be adjusted to ensure it is the third Wednesday of the month. The year
   * and month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val IMM: RollConvention = RollConvention.IMM

  /**
   * The 'IMMCAD' roll convention which adjusts the date two days before the third Wednesday.
   *
   * The input date will be adjusted to ensure it is two GBLO business days before the third
   * Wednesday of the month. The date is further adjusted earlier by a combination of the CATO
   * and CAMO calendars. The built-in calendars are used directly, rather than resolved from
   * reference data as the ported convention did. (Note that all current GBLO, CATO and CAMO
   * holiday dates will not impact the result.) The year and month of the result date will be the
   * same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val IMMCAD: RollConvention = RollConvention.IMMCAD

  /**
   * The 'IMMAUD' roll convention which adjusts the date to the Thursday before the second
   * Friday.
   *
   * The input date will be adjusted to ensure it is the Thursday before the second Friday of the
   * month. The built-in AUSY calendar is used to subtract the day, rather than resolved from
   * reference data as the ported convention did. (Note that all current AUSY holiday dates will
   * not impact the result.) The year and month of the result date will be the same as the input
   * date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val IMMAUD: RollConvention = RollConvention.IMMAUD

  /**
   * The 'IMMNZD' roll convention which adjusts the date to the first Wednesday on or after the
   * ninth day of the month.
   *
   * The input date will be adjusted to the ninth day of the month, and then it will be adjusted
   * to be a Wednesday. If the ninth is a Wednesday, then that is returned. The year and month of
   * the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val IMMNZD: RollConvention = RollConvention.IMMNZD

  /**
   * The 'SFE' roll convention which adjusts the date to the second Friday.
   *
   * The input date will be adjusted to ensure it is the second Friday of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val SFE: RollConvention = RollConvention.SFE

  /**
   * The 'TBILL' roll convention which adjusts the date to next Monday.
   *
   * The input date will be adjusted to ensure it is the next Monday. The built-in USNY calendar
   * is used in case the Monday is a holiday, rather than resolved from reference data as the
   * ported convention did.
   */
  val TBILL: RollConvention = RollConvention.TBILL

  /**
   * The 'Day1' roll convention which adjusts the date to day-of-month 1.
   *
   * The input date will be adjusted to ensure it is the 1st day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_1: RollConvention = RollConvention.ofDayOfMonthUnsafe(1)

  /**
   * The 'Day2' roll convention which adjusts the date to day-of-month 2.
   *
   * The input date will be adjusted to ensure it is the 2nd day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_2: RollConvention = RollConvention.ofDayOfMonthUnsafe(2)

  /**
   * The 'Day3' roll convention which adjusts the date to day-of-month 3.
   *
   * The input date will be adjusted to ensure it is the 3rd day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_3: RollConvention = RollConvention.ofDayOfMonthUnsafe(3)

  /**
   * The 'Day4' roll convention which adjusts the date to day-of-month 4.
   *
   * The input date will be adjusted to ensure it is the 4th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_4: RollConvention = RollConvention.ofDayOfMonthUnsafe(4)

  /**
   * The 'Day5' roll convention which adjusts the date to day-of-month 5.
   *
   * The input date will be adjusted to ensure it is the 5th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_5: RollConvention = RollConvention.ofDayOfMonthUnsafe(5)

  /**
   * The 'Day6' roll convention which adjusts the date to day-of-month 6.
   *
   * The input date will be adjusted to ensure it is the 6th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_6: RollConvention = RollConvention.ofDayOfMonthUnsafe(6)

  /**
   * The 'Day7' roll convention which adjusts the date to day-of-month 7.
   *
   * The input date will be adjusted to ensure it is the 7th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_7: RollConvention = RollConvention.ofDayOfMonthUnsafe(7)

  /**
   * The 'Day8' roll convention which adjusts the date to day-of-month 8.
   *
   * The input date will be adjusted to ensure it is the 8th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_8: RollConvention = RollConvention.ofDayOfMonthUnsafe(8)

  /**
   * The 'Day9' roll convention which adjusts the date to day-of-month 9.
   *
   * The input date will be adjusted to ensure it is the 9th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_9: RollConvention = RollConvention.ofDayOfMonthUnsafe(9)

  /**
   * The 'Day10' roll convention which adjusts the date to day-of-month 10.
   *
   * The input date will be adjusted to ensure it is the 10th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_10: RollConvention = RollConvention.ofDayOfMonthUnsafe(10)

  /**
   * The 'Day11' roll convention which adjusts the date to day-of-month 11.
   *
   * The input date will be adjusted to ensure it is the 11th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_11: RollConvention = RollConvention.ofDayOfMonthUnsafe(11)

  /**
   * The 'Day12' roll convention which adjusts the date to day-of-month 12.
   *
   * The input date will be adjusted to ensure it is the 12th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_12: RollConvention = RollConvention.ofDayOfMonthUnsafe(12)

  /**
   * The 'Day13' roll convention which adjusts the date to day-of-month 13.
   *
   * The input date will be adjusted to ensure it is the 13th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_13: RollConvention = RollConvention.ofDayOfMonthUnsafe(13)

  /**
   * The 'Day14' roll convention which adjusts the date to day-of-month 14.
   *
   * The input date will be adjusted to ensure it is the 14th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_14: RollConvention = RollConvention.ofDayOfMonthUnsafe(14)

  /**
   * The 'Day15' roll convention which adjusts the date to day-of-month 15.
   *
   * The input date will be adjusted to ensure it is the 15th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_15: RollConvention = RollConvention.ofDayOfMonthUnsafe(15)

  /**
   * The 'Day16' roll convention which adjusts the date to day-of-month 16.
   *
   * The input date will be adjusted to ensure it is the 16th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_16: RollConvention = RollConvention.ofDayOfMonthUnsafe(16)

  /**
   * The 'Day17' roll convention which adjusts the date to day-of-month 17.
   *
   * The input date will be adjusted to ensure it is the 17th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_17: RollConvention = RollConvention.ofDayOfMonthUnsafe(17)

  /**
   * The 'Day18' roll convention which adjusts the date to day-of-month 18.
   *
   * The input date will be adjusted to ensure it is the 18th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_18: RollConvention = RollConvention.ofDayOfMonthUnsafe(18)

  /**
   * The 'Day19' roll convention which adjusts the date to day-of-month 19.
   *
   * The input date will be adjusted to ensure it is the 19th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_19: RollConvention = RollConvention.ofDayOfMonthUnsafe(19)

  /**
   * The 'Day20' roll convention which adjusts the date to day-of-month 20.
   *
   * The input date will be adjusted to ensure it is the 20th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_20: RollConvention = RollConvention.ofDayOfMonthUnsafe(20)

  /**
   * The 'Day21' roll convention which adjusts the date to day-of-month 21.
   *
   * The input date will be adjusted to ensure it is the 21st day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_21: RollConvention = RollConvention.ofDayOfMonthUnsafe(21)

  /**
   * The 'Day22' roll convention which adjusts the date to day-of-month 22.
   *
   * The input date will be adjusted to ensure it is the 22nd day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_22: RollConvention = RollConvention.ofDayOfMonthUnsafe(22)

  /**
   * The 'Day23' roll convention which adjusts the date to day-of-month 23.
   *
   * The input date will be adjusted to ensure it is the 23rd day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_23: RollConvention = RollConvention.ofDayOfMonthUnsafe(23)

  /**
   * The 'Day24' roll convention which adjusts the date to day-of-month 24.
   *
   * The input date will be adjusted to ensure it is the 24th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_24: RollConvention = RollConvention.ofDayOfMonthUnsafe(24)

  /**
   * The 'Day25' roll convention which adjusts the date to day-of-month 25.
   *
   * The input date will be adjusted to ensure it is the 25th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_25: RollConvention = RollConvention.ofDayOfMonthUnsafe(25)

  /**
   * The 'Day26' roll convention which adjusts the date to day-of-month 26.
   *
   * The input date will be adjusted to ensure it is the 26th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_26: RollConvention = RollConvention.ofDayOfMonthUnsafe(26)

  /**
   * The 'Day27' roll convention which adjusts the date to day-of-month 27.
   *
   * The input date will be adjusted to ensure it is the 27th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_27: RollConvention = RollConvention.ofDayOfMonthUnsafe(27)

  /**
   * The 'Day28' roll convention which adjusts the date to day-of-month 28.
   *
   * The input date will be adjusted to ensure it is the 28th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_28: RollConvention = RollConvention.ofDayOfMonthUnsafe(28)

  /**
   * The 'Day29' roll convention which adjusts the date to day-of-month 29.
   *
   * The input date will be adjusted to ensure it is the 29th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * February is shorter than this day-of-month, so in February the last day of the month is
   * selected instead - the 28th, or the 29th in a leap year.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_29: RollConvention = RollConvention.ofDayOfMonthUnsafe(29)

  /**
   * The 'Day30' roll convention which adjusts the date to day-of-month 30.
   *
   * The input date will be adjusted to ensure it is the 30th day of the month. The year and
   * month of the result date will be the same as the input date.
   *
   * February is shorter than this day-of-month, so in February the last day of the month is
   * selected instead - the 28th, or the 29th in a leap year.
   *
   * This convention is intended for use with periods that are a multiple of months.
   */
  val DAY_30: RollConvention = RollConvention.ofDayOfMonthUnsafe(30)

  /**
   * The 'DayMon' roll convention which adjusts the date to be Monday.
   *
   * The input date will be adjusted to ensure it is a Monday.
   *
   * This convention is intended for use with periods that are a multiple of weeks.
   */
  val DAY_MON: RollConvention = RollConvention.ofDayOfWeek(DayOfWeek.MONDAY)

  /**
   * The 'DayTue' roll convention which adjusts the date to be Tuesday.
   *
   * The input date will be adjusted to ensure it is a Tuesday.
   *
   * This convention is intended for use with periods that are a multiple of weeks.
   */
  val DAY_TUE: RollConvention = RollConvention.ofDayOfWeek(DayOfWeek.TUESDAY)

  /**
   * The 'DayWed' roll convention which adjusts the date to be Wednesday.
   *
   * The input date will be adjusted to ensure it is a Wednesday.
   *
   * This convention is intended for use with periods that are a multiple of weeks.
   */
  val DAY_WED: RollConvention = RollConvention.ofDayOfWeek(DayOfWeek.WEDNESDAY)

  /**
   * The 'DayThu' roll convention which adjusts the date to be Thursday.
   *
   * The input date will be adjusted to ensure it is a Thursday.
   *
   * This convention is intended for use with periods that are a multiple of weeks.
   */
  val DAY_THU: RollConvention = RollConvention.ofDayOfWeek(DayOfWeek.THURSDAY)

  /**
   * The 'DayFri' roll convention which adjusts the date to be Friday.
   *
   * The input date will be adjusted to ensure it is a Friday.
   *
   * This convention is intended for use with periods that are a multiple of weeks.
   */
  val DAY_FRI: RollConvention = RollConvention.ofDayOfWeek(DayOfWeek.FRIDAY)

  /**
   * The 'DaySat' roll convention which adjusts the date to be Saturday.
   *
   * The input date will be adjusted to ensure it is a Saturday.
   *
   * This convention is intended for use with periods that are a multiple of weeks.
   */
  val DAY_SAT: RollConvention = RollConvention.ofDayOfWeek(DayOfWeek.SATURDAY)

  /**
   * The 'DaySun' roll convention which adjusts the date to be Sunday.
   *
   * The input date will be adjusted to ensure it is a Sunday.
   *
   * This convention is intended for use with periods that are a multiple of weeks.
   */
  val DAY_SUN: RollConvention = RollConvention.ofDayOfWeek(DayOfWeek.SUNDAY)
}
