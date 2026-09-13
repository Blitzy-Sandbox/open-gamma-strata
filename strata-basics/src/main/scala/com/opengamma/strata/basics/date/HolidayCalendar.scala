/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.DayOfWeek
import java.time.LocalDate
import java.time.Month
import java.time.Year

import scala.annotation.tailrec
import scala.collection.immutable.Set
import scala.collection.immutable.SortedSet

import cats.Hash
import cats.Order
import cats.Show

import io.circe.ACursor
import io.circe.Codec
import io.circe.CursorOp
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.NoJavaSerialization
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
 * calendar exhaustively.
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
 * [[com.opengamma.strata.collect.ArgCheck]]. Within those years, a date outside the range the
 * holiday data spans is answered from the weekend alone.
 *
 * Every implementation is immutable and safe to share between threads.
 *
 * ===Serialization===
 *
 * A calendar is written as JSON through the codec this file publishes and in no other form. Its
 * members are `case class`es and `case object`s, which the compiler makes `java.io.Serializable`
 * whether or not the library wants it, so the root mixes in
 * [[com.opengamma.strata.collect.NoJavaSerialization]]: every calendar - the weekend singletons,
 * the two composites and the packed [[ImmutableHolidayCalendar]] - refuses to be written to or
 * read from an object stream, and holiday data assembled by a stream rather than by a factory
 * cannot be presented as a calendar of this library.
 */
sealed trait HolidayCalendar extends Named with NoJavaSerialization {

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
   * The number of calendars this one reads through, which is zero unless it is a composite.
   *
   * A composite reads a pair of calendars on every query, and either of those may itself be a
   * composite, so answering a question about a date is a walk of a tree of calendars whose
   * height is this number. Every recursive operation of the family - deciding whether a date is
   * a holiday, composing the identifier, writing the calendar out and reading one back - walks
   * that same tree, so its height is what bounds the stack each of them needs. It is held on
   * each composite as it is built, from the heights of its two parts, and
   * [[HolidayCalendar.MaxCompositeDepth]] is the greatest height there can be.
   *
   * Visible within this package rather than published: it describes how a calendar is put
   * together rather than what it answers, and no caller outside these types needs it. It is not
   * part of the value either - two composites of equal parts have equal heights by construction.
   *
   * @return zero for a calendar that answers for itself, otherwise the height of the composite
   */
  private[date] def compositeDepth: Int = 0

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
   * The number of days is judged here rather than when the adjuster is applied, so that a
   * shift no calendar can perform is refused where it is named - see [[shift]] for what the
   * limit is and why there is one.
   *
   * @param amount  the number of business days to shift by, which may be negative
   * @return the adjuster that applies that shift
   * @throws IllegalArgumentException where the shift is larger than
   *   [[HolidayCalendar.MaxBusinessDayShift]] business days in either direction
   */
  def adjustBy(amount: Int): DateAdjuster = {
    HolidayCalendar.checkShift(amount)
    DateAdjuster(date => shift(date, amount))
  }

  /**
   * Finds the next business day, returning the date itself where it is one already.
   *
   * The search walks forwards one day at a time and must find a business day within
   * [[HolidayCalendar.MaxConsecutiveHolidays]] days; see [[HolidayCalendar.MaxConsecutiveHolidays]]
   * for what a calendar that cannot is, and why such a search is refused rather than continued.
   *
   * @param date  the date to adjust
   * @return the date itself where it is a business day, otherwise the next business day
   * @throws IllegalArgumentException where no business day is reached within
   *   [[HolidayCalendar.MaxConsecutiveHolidays]] days, or where the calculation leaves the range
   *   this calendar supports
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
   * The search walks backwards one day at a time and must find a business day within
   * [[HolidayCalendar.MaxConsecutiveHolidays]] days, as [[nextOrSame]] must forwards.
   *
   * @param date  the date to adjust
   * @return the date itself where it is a business day, otherwise the previous business day
   * @throws IllegalArgumentException where no business day is reached within
   *   [[HolidayCalendar.MaxConsecutiveHolidays]] days, or where the calculation leaves the range
   *   this calendar supports
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
   * ===How large a shift may be===
   *
   * Shifting by business days is a walk: each day of it asks this calendar a question, so the
   * work the call does is proportional to the number of days asked for, and the number is an
   * `Int`. A request for two thousand million business days would therefore occupy a processor
   * for the best part of a minute to answer with a date some six million years away, which is
   * not a date any rule of finance names - and a calculation whose cost is decided by a number
   * that reached the program from outside it is a calculation that can be made to cost
   * anything. The number of days is accordingly limited to
   * [[HolidayCalendar.MaxBusinessDayShift]] in either direction, which is more than three
   * hundred years of business days and beyond any convention, and a request beyond it is
   * refused before the walk begins. The limit is a precondition on the caller, in the same
   * sense as the range of dates a calendar can answer about, so it is stated by raising rather
   * than reported; the types that carry a day count as '''data''' - [[DaysAdjustment]] above
   * all - judge it against the same limit when they are built, so a document cannot reach this.
   *
   * The calendar of no holidays has no such limit, because it adds days arithmetically rather
   * than by walking them.
   *
   * @param date  the date to shift
   * @param amount  the number of business days to shift by
   * @return the shifted date
   * @throws IllegalArgumentException where the shift is larger than
   *   [[HolidayCalendar.MaxBusinessDayShift]] business days in either direction, or where the
   *   calculation leaves the range this calendar supports
   */
  def shift(date: LocalDate, amount: Int): LocalDate = {
    HolidayCalendar.checkShift(amount)
    HolidayCalendar.shifted(this, date, amount)
  }

  /**
   * Counts the business days in the specified range.
   *
   * The days are counted as a `Long` and the total is narrowed to an `Int` only at the end: a
   * range wide enough to hold more business days than an `Int` can express is reported as an
   * arithmetic failure rather than silently counted modulo two to the thirty-two. Such a range
   * is reachable only on a calendar with no stored holiday data, a calendar that holds data
   * covering at most the ten thousand years it can be asked about.
   *
   * @param startInclusive  the start date, included in the count
   * @param endExclusive  the end date, excluded from the count
   * @return the number of business days in the range, zero where the dates are equal
   * @throws IllegalArgumentException where the end date is before the start date, or either date
   *   is outside the range this calendar supports
   * @throws ArithmeticException where the range holds more business days than an `Int` can hold
   */
  def daysBetween(startInclusive: LocalDate, endExclusive: LocalDate): Int =
    Math.toIntExact(businessDays(startInclusive, endExclusive).foldLeft(0L)((counted, _) => counted + 1L))

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
   * Combining is what makes a calendar a composite, and a composite may be combined again, so
   * the result is limited to [[HolidayCalendar.MaxCompositeDepth]] calendars deep - see
   * [[HolidayCalendar.MaxCompositeDepth]] for why there is a limit at all.
   *
   * @param other  the other calendar
   * @return the combined calendar
   * @throws IllegalArgumentException where the result would read through more than
   *   [[HolidayCalendar.MaxCompositeDepth]] calendars
   */
  def combinedWith(other: HolidayCalendar): HolidayCalendar =
    if (this == other || other == NoHolidays) this else HolidayCalendar.Combined(this, other)

  /**
   * Links this calendar to another, observing only the holidays both agree on.
   *
   * A day is a business day under the result where it is a business day under either calendar,
   * so linking anything to the calendar that has no holidays makes every day a business day.
   *
   * Linking makes a composite exactly as combining does, and is limited in the same way, to
   * [[HolidayCalendar.MaxCompositeDepth]] calendars deep.
   *
   * @param other  the other calendar
   * @return the linked calendar
   * @throws IllegalArgumentException where the result would read through more than
   *   [[HolidayCalendar.MaxCompositeDepth]] calendars
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
 * ===Initialisation order===
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

  // The three limits of the family.
  //
  // Each of them bounds work that a value arriving from outside the program would otherwise
  // decide the cost of: how far a shift walks, how far a search walks, and how deep a
  // composite reads. They are published because each is part of the stated contract of a
  // public method, so a caller can check a figure it holds against the same limit the method
  // applies rather than discovering it by being refused. Every one of them is far beyond any
  // convention of finance, so no legitimate use of this library comes near one.

  /**
   * The greatest number of business days a single shift may name, in either direction.
   *
   * Three hundred and eighty years of business days. [[HolidayCalendar.shift]] walks the days
   * it is asked for, so this is what bounds the cost of a shift; the reasoning is given there.
   * [[DaysAdjustment]] holds a day count as data and judges it against this same figure when
   * it is built, which is what keeps a shift this large out of reach of a document.
   */
  val MaxBusinessDayShift: Int = 100000

  /**
   * The greatest number of consecutive days a search for a business day may cross.
   *
   * Ten years of closure. [[HolidayCalendar.nextOrSame]] and [[HolidayCalendar.previousOrSame]]
   * walk one day at a time until they find a business day, and some calendars have none to
   * find: a weekend that closes all seven days of the week has no business day at all, and
   * neither does a composite whose parts between them close every day - `Sat/Sun` combined with
   * a calendar whose weekend is Monday to Friday, for instance, which is two perfectly ordinary
   * calendars read together. Such a search cannot succeed, so it is stopped and refused rather
   * than walked to the end of the range of dates, which is where it would otherwise end up
   * after some millions of days and a misleading message about the year it reached.
   *
   * The figure is what separates a calendar with no business day from one that is merely shut
   * for a long time. No centre closes for ten years, and a calendar built from published
   * holidays never states such a run, so a search that crosses one is describing a calendar
   * that is shut indefinitely.
   */
  val MaxConsecutiveHolidays: Int = 3653

  /**
   * The greatest number of calendars a composite may read through.
   *
   * A composite reads two calendars, either of which may be a composite of its own, so
   * combining without limit builds a tree of any height - and every recursive operation of
   * this family walks it: deciding whether a date is a holiday, composing the identifier,
   * writing the calendar out, reading one back. A tree tall enough exhausts the stack, which
   * ends the calling thread rather than the calculation, and nothing about combining calendars
   * requires any height: a payment settling in every centre this library knows about reads
   * some thirty calendars, and a composite identifier names each part once. This limit is
   * therefore generous by a factor of four over anything real, while keeping every walk of the
   * tree within a stack depth that cannot fail.
   *
   * It is applied wherever a composite comes into being: [[HolidayCalendar.combinedWith]] and
   * [[HolidayCalendar.linkedWith]], the constructors of [[HolidayCalendar.Combined]] and
   * [[HolidayCalendar.Linked]] themselves, the reading of a calendar from a document, and
   * [[HolidayCalendarId.resolve]], which refuses a composite identifier naming more parts than
   * this - in its own failure channel, so that resolving an identifier still answers rather
   * than raising.
   */
  val MaxCompositeDepth: Int = 128

  /**
   * Checks that a shift names a number of business days a calendar can walk.
   *
   * The message is built only where the check fails, the argument being taken by name, so the
   * limit costs one comparison on the path every shift takes.
   *
   * Visible to the package rather than to this object alone, because the one implementation
   * that overrides [[HolidayCalendar.shift]] - [[ImmutableHolidayCalendar]] - answers from its
   * own stored months without reaching the inherited method, and has to apply the same limit
   * for it to hold of every calendar rather than only of those holding no data.
   *
   * @param amount  the number of business days the shift names
   * @throws IllegalArgumentException where the magnitude exceeds [[MaxBusinessDayShift]]
   */
  private[date] def checkShift(amount: Int): Unit =
    // measured in `Long` arithmetic, because the magnitude of the smallest `Int` is not an
    // `Int`: negating it overflows back to itself, and the comparison would then pass
    ArgCheck.isTrue(
      Math.abs(amount.toLong) <= MaxBusinessDayShift.toLong,
      s"A shift of more than $MaxBusinessDayShift business days is outside the accepted range, " +
        s"but was: $amount")

  /**
   * Returns the height of a composite of two calendars, having checked that it is allowed.
   *
   * The height is one more than the taller of its two parts, and it is computed from heights
   * the parts already hold, so building a composite costs one comparison however tall it is -
   * and the check below therefore cannot be outrun by building the tree from the bottom up.
   *
   * @param first  the first part of the composite
   * @param second  the second part of the composite
   * @return the height of the composite
   * @throws IllegalArgumentException where the composite would be taller than
   *   [[MaxCompositeDepth]]
   */
  private def checkedCompositeDepth(first: HolidayCalendar, second: HolidayCalendar): Int = {
    val depth = compositeDepthOf(first, second)
    ArgCheck.isTrue(
      !exceedsCompositeDepth(first, second),
      s"A holiday calendar cannot read through more than $MaxCompositeDepth calendars, " +
        s"but the combination would read through: $depth")
    depth
  }

  /**
   * Returns the height a composite of two calendars would have.
   *
   * @param first  the first part of the composite
   * @param second  the second part of the composite
   * @return the height the composite would have, whether or not it is allowed
   */
  private[date] def compositeDepthOf(first: HolidayCalendar, second: HolidayCalendar): Int =
    1 + Math.max(first.compositeDepth, second.compositeDepth)

  /**
   * Tests whether combining two calendars would exceed [[MaxCompositeDepth]].
   *
   * The predicate behind the check on the constructors of [[Combined]] and [[Linked]], read
   * directly by callers that have to answer in a failure channel rather than raise - notably
   * [[HolidayCalendarId.resolve]], which combines calendars a caller supplied and so cannot
   * know their heights from the identifier alone.
   *
   * @param first  the first part of the composite
   * @param second  the second part of the composite
   * @return true where the composite would be taller than [[MaxCompositeDepth]]
   */
  private[date] def exceedsCompositeDepth(first: HolidayCalendar, second: HolidayCalendar): Boolean =
    compositeDepthOf(first, second) > MaxCompositeDepth

  /** The JSON key of a calendar written out with its own holiday dates. */
  private val ImmutableKey: String = "Immutable"

  private val CombinedKey: String = "Combined"

  private val LinkedKey: String = "Linked"

  private val IdField: String = "id"

  private val WeekendDaysField: String = "weekendDays"

  private val StartYearField: String = "startYear"

  private val HolidaysField: String = "holidays"

  private val WorkingWeekendDaysField: String = "workingWeekendDays"

  private val FirstField: String = "a"

  private val SecondField: String = "b"

  private val UnknownShapeMessage: String =
    "A holiday calendar must be the name of a calendar, or an object holding exactly one of " +
      s"'$ImmutableKey', '$CombinedKey' or '$LinkedKey'"

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

    /**
     * The height of this combination, worked out and judged as it is built.
     *
     * Held as a value of the instance rather than computed on demand, which is what makes the
     * limit on [[HolidayCalendar.MaxCompositeDepth]] impossible to outrun: the height of each
     * part is already known when a combination is built, so building one costs a single
     * comparison and a combination taller than the limit cannot come into being - not through
     * [[HolidayCalendar.combinedWith]], not through this constructor, and not through the
     * `copy` a case class provides. It is evaluated during construction precisely so the
     * judgement happens there, and it is a function of the two parts, so it cannot make two
     * equal combinations behave differently.
     *
     * @return the number of calendars this combination reads through
     */
    private[date] override val compositeDepth: Int = checkedCompositeDepth(calendar1, calendar2)

    /**
     * The identifier of this combination, composed from the identifiers of its two parts.
     *
     * Composed at most once and then held on the instance. Composing it is not free - the two
     * names are joined and the joined name is normalised, which parses it, builds an identifier
     * for each part, and sorts and deduplicates those parts - while the answer cannot change,
     * because the parts of a combination are fixed when it is built. Holding it therefore takes
     * that parsing, sorting and allocation off every read after the first, and `id`, `name`,
     * `toString`, `Show` and the JSON form of a composite calendar are all reads of it.
     *
     * Deferred rather than computed in the constructor because a combination built to answer a
     * run of `isHoliday` questions - which is what [[HolidayCalendar.combinedWith]] is for, and
     * what resolving a composite identifier does on the way to one date adjustment - never asks
     * for its identifier, and such a combination should pay nothing for one.
     *
     * The field is not part of the value: equality and hashing are the case class's, over the two
     * parts alone, so holding a composed identifier cannot make two equal combinations behave
     * differently.
     *
     * @return the identifier of the combination, such as `GBLO+USNY`
     */
    override lazy val id: HolidayCalendarId = calendar1.id.combinedWith(calendar2.id)

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

    /**
     * The height of this link, worked out and judged as it is built, as [[Combined.compositeDepth]]
     * is.
     *
     * @return the number of calendars this link reads through
     */
    private[date] override val compositeDepth: Int = checkedCompositeDepth(calendar1, calendar2)

    /**
     * The identifier of this link, composed from the identifiers of its two parts.
     *
     * Composed at most once and then held on the instance, for the reasons given on
     * [[Combined.id]]: the answer is fixed when the link is built, composing it parses and
     * normalises a joined name, and a link built to answer `isHoliday` questions never reads it.
     *
     * @return the identifier of the link, such as `GBLO~USNY`
     */
    override lazy val id: HolidayCalendarId = calendar1.id.linkedWith(calendar2.id)

    override def isHoliday(date: LocalDate): Boolean =
      calendar1.isHoliday(date) && calendar2.isHoliday(date)

    override def toString: String = s"HolidayCalendar[$name]"
  }

  // The four calendars whose content follows from their name are declared beside this object
  // rather than inside it, because Scala requires every direct subtype of a sealed type to be
  // declared in the same file. These aliases make each of them reachable through this companion
  // as well, so `HolidayCalendar.SatSun` and `SatSun` name the same object and a call site may
  // spell either. They introduce no new entity.

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

  /**
   * The ordering of dates used by the range checks of this file.
   *
   * `LocalDate` implements `Comparable` over `ChronoLocalDate` rather than over itself, so no
   * ordering of dates is derivable implicitly and one has to be named. It is deliberately not
   * implicit: it is handed to the check that needs it, so that it cannot become an ambient
   * ordering for every date comparison in this package.
   */
  private val dateOrder: Order[LocalDate] = Order.from((first, second) => first.compareTo(second))

  /**
   * Finds the first business day at or after the date, stepping by one day at a time.
   *
   * Written as a tail-recursive function taking the date as a parameter, so that the search
   * needs no mutable state and runs as a loop.
   *
   * The search is required to make progress: it crosses at most
   * [[HolidayCalendar.MaxConsecutiveHolidays]] days without finding a business day, and is
   * refused beyond that. Some calendars have no business day to find - a weekend closing all
   * seven days, or a composite whose parts between them close every day of the week - and such
   * a search would otherwise walk millions of days to the end of the range of dates a calendar
   * can answer about and then report the year it reached, which says nothing about what is
   * actually wrong. Refusing it names the calendar and the date instead, and costs one
   * comparison per day crossed.
   *
   * @param calendar  the calendar to search
   * @param date  the date to start from
   * @param step  the number of days to step by, 1 to search forwards and -1 backwards
   * @return the first business day found
   * @throws IllegalArgumentException where no business day is found within
   *   [[HolidayCalendar.MaxConsecutiveHolidays]] days
   */
  private def businessDayFrom(calendar: HolidayCalendar, date: LocalDate, step: Int): LocalDate =
    businessDaySearch(calendar, date, step, date, MaxConsecutiveHolidays)

  /**
   * Carries out the search of [[businessDayFrom]], counting the days it has left to cross.
   *
   * Written as a tail-recursive function taking the date and the remaining allowance as
   * parameters, so that the search needs no mutable state and compiles to a loop.
   *
   * @param calendar  the calendar to search
   * @param date  the date reached
   * @param step  the number of days to step by, 1 to search forwards and -1 backwards
   * @param from  the date the search began at, named by the refusal
   * @param remaining  the number of further days the search may cross
   * @return the first business day found
   * @throws IllegalArgumentException where the allowance runs out first
   */
  @tailrec
  private def businessDaySearch(
      calendar: HolidayCalendar,
      date: LocalDate,
      step: Int,
      from: LocalDate,
      remaining: Int): LocalDate =

    if (calendar.isBusinessDay(date)) {
      date
    } else {
      // the message is built only where the search is refused, the argument being taken by name
      ArgCheck.isTrue(
        remaining > 0,
        s"Holiday calendar '${calendar.name}' has no business day within " +
          s"$MaxConsecutiveHolidays days of $from, so it has no business day at all")
      businessDaySearch(calendar, LocalDateUtils.plusDays(date, step), step, from, remaining - 1)
    }

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
   * property of the data, so it fails fast.
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

  /**
   * Checks whether a value is one of the three calendars whose only holidays are their weekend.
   *
   * Those three are objects, so membership of the set is reference identity against each of
   * them; there is nothing else a weekend calendar could be, the family being sealed and closed.
   *
   * @param value  the value to test, which may be anything at all
   * @return true where the value is `Sat/Sun`, `Fri/Sat` or `Thu/Fri`
   */
  private[date] def isWeekendCalendar(value: Any): Boolean = value match {
    case reference: AnyRef => (reference eq SatSun) || (reference eq FriSat) || (reference eq ThuFri)
    case _ => false
  }

  /**
   * Compares a weekend calendar with another value.
   *
   * A weekend calendar is equal to a weekend calendar carrying the same identifier. The three
   * weekend calendars carry three different identifiers, so among them the rule matches one
   * value alone; it is written as the rule rather than as a comparison with `this` so that the
   * asymmetry it deliberately has stays visible - a calendar carrying holiday data is '''not'''
   * equal to a weekend calendar of the same identifier, and neither side claims otherwise.
   *
   * @param self  the weekend calendar being compared
   * @param other  the value it is compared with
   * @return true where the other value is a weekend calendar of the same identifier
   */
  private[date] def weekendCalendarEquals(self: HolidayCalendar, other: Any): Boolean =
    other match {
      case that: HolidayCalendar => isWeekendCalendar(that) && that.id == self.id
      case _ => false
    }

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
   * There is deliberately no ordering of calendars: nothing about a calendar makes one larger
   * than another.
   *
   * @return the hashing of holiday calendars
   */
  implicit val hash: Hash[HolidayCalendar] = Hash.fromUniversalHashCode[HolidayCalendar]

  /**
   * The rendering of holiday calendars as text.
   *
   * Renders what `toString` renders - `HolidayCalendar[GBLO]` - so the two ways of putting a
   * calendar into a message agree.
   *
   * @return the rendering of a holiday calendar
   */
  implicit val show: Show[HolidayCalendar] = Show.show(calendar => calendar.toString)

  /**
   * The decoder of a list of holiday dates, in the ISO-8601 form.
   *
   * How many dates the list holds is stated by the document, so the count is read from the payload
   * and measured against `Codecs.MaximumCollectionElements` before a single date is parsed; a
   * document stating more is a decoding failure naming the ceiling, positioned at the field being
   * read, and nothing is allocated for it. The semantic checks this file performs on these lists
   * cannot serve as that bound: they hold each date to the years a calendar may cover, and a
   * document may state the same in-range date a million times over and pass every one of them,
   * having paid for a million parses and for the sort that collapses them. The figure is the one
   * ceiling this port publishes for a decoded collection, and it is far above every calendar this
   * library defines: the longest holiday list any of them holds is the few thousand dates of the
   * Budapest calendar, which names its Saturdays explicitly across the century and a half its range
   * covers, so none of them is refused by it.
   */
  private val dateListDecoder: Decoder[List[LocalDate]] =
    Codecs.boundedElements("dates in the list", Codecs.MaximumCollectionElements)(
      Decoder.decodeList(Decoder.decodeLocalDate))

  /**
   * The decoder of a list of days of the week, by constant name.
   *
   * Bounded exactly as the list of dates above is, and for the same reason: the list a document
   * states is measured against `Codecs.MaximumCollectionElements` before a name is read, so a
   * document naming a weekend day a million times is refused without the names being looked up.
   * A calendar holds at most seven distinct weekend days, and the encoder writes each of them once,
   * so the ceiling stands an enormous distance from every document this port produces.
   */
  private val dayOfWeekListDecoder: Decoder[List[DayOfWeek]] =
    Codecs.boundedElements("days of the week in the list", Codecs.MaximumCollectionElements)(
      Decoder.decodeList(Codecs.dayOfWeekCodec))

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
   * Checks whether a calendar '''is''' one of the calendars built into this library.
   *
   * A built-in calendar is written as its name, because the name is enough to find the same
   * calendar again through reference data. Every other calendar has to be written structurally,
   * and the test that decides between the two forms is therefore identity against the library's
   * own instance rather than equality with it: a calendar carrying holiday data is equal to
   * another one of the same identifier whatever dates it holds, so an application that supplies
   * its own `GBLO` - including the weekend-only calendar
   * [[HolidayCalendars.defaultingReferenceData]] defaults under a standard identifier - would
   * otherwise be written as the bare name `GBLO` and read back as the library's London calendar,
   * losing every holiday and working day it declared.
   *
   * Only the built-in calendar of the name given is consulted, and only that one is generated,
   * so encoding a calendar costs the calendar it names rather than the whole built-in set.
   *
   * This is the one place in the JSON support that reads the built-in set, and it does so from
   * inside a method body, which is what keeps the initialisation loop of this object safe.
   *
   * @param calendar  the calendar to test
   * @return true where the calendar is the library's own instance of its name
   */
  private def isBuiltIn(calendar: HolidayCalendar): Boolean =
    StandardHolidayCalendars.isBuiltIn(calendar)

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
   * document. Both sets are taken from one scan of those months rather than from two, because
   * each day of each month decides by itself which set it belongs to. The first year covered is
   * written for the reader and is checked rather than used when a document is read back, because
   * the range a calendar supports follows from its holidays and is worked out again when the
   * calendar is rebuilt.
   *
   * @param calendar  the calendar to write
   * @return the fields of the calendar
   */
  private def encodeImmutable(calendar: ImmutableHolidayCalendar): Json = {
    val (holidays, workingDays) = calendar.holidaysAndWorkingDays
    Json.obj(
      IdField -> Json.fromString(calendar.id.name),
      WeekendDaysField -> Json.fromValues(
        calendar.weekendDays.toList.sortBy(day => day.getValue).map(day => Codecs.dayOfWeekCodec(day))),
      StartYearField -> Json.fromInt(calendar.startYear),
      HolidaysField -> encodeDates(holidays),
      WorkingWeekendDaysField -> encodeDates(workingDays))
  }

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
   * @return the calendar of that name, or the failure naming the identifier - or, for a
   *   composite name, the component identifier - that the built-in reference data does not hold
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
   * Checks that every date of a decoded field lies in a year a calendar can hold data for.
   *
   * A calendar holds one `Int` per month from the first year of its holidays to the last, so the
   * years its document names decide how much memory building it takes. A document is not
   * trusted: without this check a pair of dates a million years apart would ask for an array of
   * tens of millions of months, and a year far enough out would make the length of that array
   * overflow. The years a calendar can answer about are 0 to 9999, which caps the array at
   * 120,000 months however hostile the document, so the check is the same one the calendar
   * applies to every date it is asked about - stated here as a decoding failure, because a
   * document is data rather than a caller's contract.
   *
   * @param dates  the dates read from the document
   * @param field  the name of the field they were read from, for the failure message
   * @param cursor  the position in the document, for the failure message
   * @return unit where every date is in range, or the failure naming the first date whose year
   *   falls outside 0 to 9999
   */
  private def checkSupportedYears(
      dates: List[LocalDate],
      field: String,
      cursor: HCursor): Decoder.Result[Unit] =

    dates.find(date => !ImmutableHolidayCalendar.isSupportedYear(date.getYear)) match {
      case Some(date) =>
        Left(
          DecodingFailure(
            s"A holiday calendar cannot hold a date outside the accepted range " +
              s"(year 0000 to 9999), but '$field' holds: $date",
            cursor.history))
      case None => Right(())
    }

  /**
   * Checks the first year a document declares its calendar to cover.
   *
   * The field records the first year of the range the calendar being described covered. It is
   * written for the reader and '''checked''' here rather than taken as an input to building the
   * calendar: the range of a decoded calendar follows from its holidays, exactly as it does for
   * every calendar this library builds, so a range that began earlier than every holiday the
   * document still lists is not recovered from this field and a working day declared in such a
   * year is ignored - the same treatment [[ImmutableHolidayCalendar.of]] gives a working day
   * outside the years its holidays span. See [[immutableDecoder]] for why the field is not
   * allowed to widen the range.
   *
   * Two things are therefore required of it, so that a document whose own fields disagree is
   * refused rather than read: it names a year a calendar can cover, and it is not later than the
   * earliest date the document names, since the first year of a range cannot begin after the
   * dates inside it. The field may be absent, so that a hand-written document need not work out
   * what to put in it.
   *
   * @param declared  the year the document declares, where it declares one
   * @param dates  every date the document names, holidays and working days alike
   * @param cursor  the position in the document, for the failure message
   * @return unit where the year is consistent, or the failure naming the broken condition: the
   *   declared year must lie within 0 to 9999, and must not be later than the year of the
   *   earliest date the document names
   */
  private def checkStartYear(
      declared: Option[Int],
      dates: List[LocalDate],
      cursor: HCursor): Decoder.Result[Unit] =

    declared match {
      case None => Right(())
      case Some(year) if !ImmutableHolidayCalendar.isSupportedYear(year) =>
        Left(
          DecodingFailure(
            s"A holiday calendar cannot start outside the accepted range (year 0000 to 9999), " +
              s"but '$StartYearField' is: $year",
            cursor.history))
      case Some(year) if dates.nonEmpty && year > dates.iterator.map(date => date.getYear).min =>
        Left(
          DecodingFailure(
            s"A holiday calendar cannot start after the earliest date it declares, but " +
              s"'$StartYearField' is $year and that date is: ${dates.minBy(date => date.toEpochDay)}",
            cursor.history))
      case Some(_) => Right(())
    }

  /**
   * Checks that a document leaves some day of the week open.
   *
   * A calendar whose weekend closes every day of the week has no business day in any year
   * outside the ones its holidays cover, and therefore no next or previous business day to
   * answer with; [[ImmutableHolidayCalendar.of]] refuses to build one, and a document
   * describing one is refused here so that it is reported as the document being wrong rather
   * than as the caller of a factory being wrong. The check is stated at this level because a
   * document is data: the same condition reaches a caller of the factory as a raised refusal,
   * which is how every precondition of these calendars is stated.
   *
   * @param weekendDays  the days of the week the document declares to be holidays
   * @param cursor  the position in the document, for the failure message
   * @return unit where some day of the week is left open, or the failure otherwise
   */
  private def checkWeekendDays(
      weekendDays: List[DayOfWeek],
      cursor: HCursor): Decoder.Result[Unit] =

    if (weekendDays.distinct.lengthCompare(ImmutableHolidayCalendar.DaysOfWeek) < 0) {
      Right(())
    } else {
      Left(
        DecodingFailure(
          "A holiday calendar must leave at least one day of the week open, but " +
            s"'$WeekendDaysField' closes every one of them",
          cursor.history))
    }

  /**
   * Reads a calendar that carries its own holiday data.
   *
   * A document describes a calendar as a caller does, and is read by handing its fields to the
   * factory a caller uses: the holidays are sorted and deduplicated, the weekend dates declared
   * to be business days override both the holidays and the weekend, and the range of years
   * follows from the holidays alone - so a working day the document names outside that range is
   * ignored here exactly as [[ImmutableHolidayCalendar.of]] ignores one. The weekend overrides
   * and the first year are both optional, because a calendar with no overrides writes an empty
   * array and a hand-written document may omit either field.
   *
   * The first year the document declares is informational. It says which year the range of the
   * calendar being described began in, it is checked against the dates beside it by
   * [[checkStartYear]], and it does not take part in building the calendar. Letting it - or a
   * working day named outside the holidays - decide the range would let a document reach a
   * calendar no factory of this library can produce: a range beginning before every holiday it
   * holds, in which a weekend date named as a working day is a business day while the same
   * declaration made in code is ignored. The range is part of what a calendar answers with, and
   * a decoded calendar reaches date adjustments, schedule generation, index observations and
   * `Bus/252` day counts, so the document is held to the same semantics as the caller. This is
   * the contract the encoder states as well: see [[encodeImmutable]].
   *
   * The checks come before the calendar is built, which is the point of them: building it
   * allocates one machine word per month of the range, so every date the document names - and
   * the year it declares - is known to lie within the years a calendar may cover before anything
   * is allocated from them.
   */
  private val immutableDecoder: Decoder[ImmutableHolidayCalendar] = Decoder.instance { cursor =>
    for {
      name <- cursor.get[String](IdField)
      weekendDays <- cursor.get[List[DayOfWeek]](WeekendDaysField)(dayOfWeekListDecoder)
      holidays <- cursor.get[List[LocalDate]](HolidaysField)(dateListDecoder)
      workingDays <- cursor.getOrElse[List[LocalDate]](WorkingWeekendDaysField)(List.empty)(dateListDecoder)
      declaredStartYear <- cursor.get[Option[Int]](StartYearField)
      _ <- checkWeekendDays(weekendDays, cursor)
      _ <- checkSupportedYears(holidays, HolidaysField, cursor)
      _ <- checkSupportedYears(workingDays, WorkingWeekendDaysField, cursor)
      _ <- checkStartYear(declaredStartYear, holidays ::: workingDays, cursor)
    } yield ImmutableHolidayCalendar.of(HolidayCalendarId.of(name), holidays, weekendDays, workingDays)
  }

  /**
   * The refusal of a document that nests composite calendars deeper than the family allows.
   *
   * @param depth  the depth the reader had already reached
   * @param history  the position within the document
   * @return the failure describing the refusal
   */
  private def tooDeepFailure(depth: Int, history: List[CursorOp]): DecodingFailure =
    DecodingFailure(
      s"A holiday calendar cannot read through more than $MaxCompositeDepth calendars, " +
        s"but the document nests them at least $depth deep",
      history)

  /**
   * Reads the two parts of a composite calendar.
   *
   * Each part is read by the reader of the family at one level deeper, so a part may itself be
   * a name, a calendar carrying its own data, or a further composite - and the reader stops
   * descending where a composite would exceed [[MaxCompositeDepth]]. Counting the descent is
   * what keeps '''reading''' a document within a stack depth that cannot fail: the limit the
   * constructors of [[Combined]] and [[Linked]] apply would otherwise be reached only after the
   * whole of a hostile document had been read, which is after the reading of it has already
   * exhausted the stack.
   *
   * @param cursor  the position of the object holding the two parts
   * @param depth  the number of composites already entered
   * @return the two parts, or the first failure the two fields report, either a missing `a` or
   *   `b` field or a part that is not itself a calendar
   */
  private def decodePair(cursor: ACursor, depth: Int): Decoder.Result[(HolidayCalendar, HolidayCalendar)] = {
    val part: Decoder[HolidayCalendar] = Decoder.instance(nested => decodeCalendar(nested, depth))
    for {
      first <- cursor.get[HolidayCalendar](FirstField)(part)
      second <- cursor.get[HolidayCalendar](SecondField)(part)
    } yield (first, second)
  }

  /**
   * Reads a holiday calendar.
   *
   * This is the inverse of the encoding above: a string is a name, and an object holding exactly
   * one field is a calendar of the kind that field names. Anything else is rejected, as is an
   * object holding no field or several.
   *
   * @param cursor  the position in the document
   * @param depth  the number of composites already entered, zero at the top of a document
   * @return the calendar, or the failure naming the broken condition: a document must be a
   *   calendar name, or an object holding exactly one of the `Immutable`, `Combined` and
   *   `Linked` fields
   */
  private def decodeCalendar(cursor: HCursor, depth: Int): Decoder.Result[HolidayCalendar] =
    cursor.value.asString match {
      case Some(text) => decodeName(text, cursor)
      case None =>
        cursor.keys.map(keys => keys.toList) match {
          case Some(ImmutableKey :: Nil) =>
            cursor.downField(ImmutableKey).as[ImmutableHolidayCalendar](immutableDecoder)
          case Some(CombinedKey :: Nil) =>
            if (depth >= MaxCompositeDepth) Left(tooDeepFailure(depth + 1, cursor.history))
            else decodePair(cursor.downField(CombinedKey), depth + 1).map {
              case (first, second) => Combined(first, second)
            }
          case Some(LinkedKey :: Nil) =>
            if (depth >= MaxCompositeDepth) Left(tooDeepFailure(depth + 1, cursor.history))
            else decodePair(cursor.downField(LinkedKey), depth + 1).map {
              case (first, second) => Linked(first, second)
            }
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
   * The dates a calendar declares are what a document carries, so a document is both readable
   * and independent of how a calendar stores its holidays.
   *
   * Written out by hand rather than derived, because the family mixes a name-based form with a
   * structural one and because the type carrying holiday data has no public constructor to derive
   * from.
   *
   * Two calendars that hold the same data encode to identical bytes: the choice between the two
   * forms is made by identity against the built-in set, dates and days of the week are written
   * in ascending order, and the fields are written in a fixed order. Note that this is a
   * statement about data rather than about equality - a calendar carrying holiday data is equal
   * to any calendar of the same identifier, so the library's `GBLO` and an application's own
   * `GBLO` are equal values that encode differently, which is exactly what stops the second
   * being read back as the first.
   *
   * ===What a document may not state===
   *
   * Two things are refused that the shapes above would otherwise admit, and both are refused
   * because a document is data rather than calling code. A calendar whose weekend closes every
   * day of the week has no business day to answer with, and is refused by
   * [[checkWeekendDays]]. A composite nested deeper than [[MaxCompositeDepth]] is refused as
   * the reader descends, by [[decodePair]], so that reading it cannot exhaust the stack. Beyond
   * those, the reader is wrapped so that a refusal any factory of the family states by raising
   * - a date in a year no calendar can hold data for, most of all - is reported as a decoding
   * failure at the position it occurred instead of leaving the reading of the document
   * altogether.
   *
   * @return the codec for holiday calendars
   */
  implicit val codec: Codec[HolidayCalendar] =
    Codec.from(
      Codecs.guardedDecoder(Decoder.instance(cursor => decodeCalendar(cursor, 0))),
      Encoder.instance(encodeCalendar))
}


/**
 * The calendar for which every day is a business day.
 *
 * It is the identity of [[HolidayCalendar.combinedWith]] and the absorbing element of
 * [[HolidayCalendar.linkedWith]], and it is what a caller supplies to say that no holiday
 * calendar applies. Its date arithmetic is plain calendar arithmetic, which is why each method
 * is overridden directly here rather than searching day by day for a business day it would
 * always find at once.
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

  /**
   * Shifts the date by a number of days, every day being a business day.
   *
   * This is plain date arithmetic in one step, so - alone in the family - it is '''not''' limited
   * to [[HolidayCalendar.MaxBusinessDayShift]] days: the limit exists because shifting by
   * business days walks them, and nothing here walks anything. That is what keeps
   * [[DaysAdjustment.ofCalendarDays]] total for every count an `Int` can hold, as the factory
   * being ported was, and every calendar-day addition applicable however many days it names.
   *
   * @param date  the date to shift
   * @param amount  the number of days to shift by, which may be negative
   * @return the shifted date
   */
  override def shift(date: LocalDate, amount: Int): LocalDate = LocalDateUtils.plusDays(date, amount)

  /**
   * Returns an adjuster that shifts a date by a number of days.
   *
   * Overridden for the reason [[shift]] is: the addition this calendar performs is arithmetic
   * rather than a walk, so the adjuster it returns accepts every count [[shift]] does. Leaving
   * the inherited implementation would have the adjuster refuse a count that `shift` accepts on
   * the same calendar.
   *
   * @param amount  the number of days to shift by, which may be negative
   * @return the adjuster that applies that shift
   */
  override def adjustBy(amount: Int): DateAdjuster = DateAdjuster(date => shift(date, amount))

  /**
   * Counts the days in the specified range, every one of which is a business day.
   *
   * This is the plain difference of the two dates and does not require them to be the right way
   * round: a range given backwards yields a negative count rather than failing.
   *
   * @param startInclusive  the start date, included in the count
   * @param endExclusive  the end date, excluded from the count
   * @return the number of days in the range
   */
  override def daysBetween(startInclusive: LocalDate, endExclusive: LocalDate): Int =
    Math.toIntExact(LocalDateUtils.daysBetween(startInclusive, endExclusive))

  override def combinedWith(other: HolidayCalendar): HolidayCalendar = other
}

// The three calendars whose only holidays are their two weekend days.
//
// These three instances are the only weekend calendars there are, and no constructor makes
// another, because such a calendar carries no data beyond the pair of days its name already
// gives: `Sat/Sun`, `Fri/Sat` and `Thu/Fri`. A centre with any other weekend is described by an
// `ImmutableHolidayCalendar`, which takes the weekend as data.
//
// Each of them is a member of the sealed family in its own right rather than a subclass of a
// shared weekend type: the family has exactly the seven members declared in this file, and an
// intermediate type would add an eighth. What the three have in common is a pair of days and one
// rule for equality, and both are small enough to state three times; the equality rule itself is
// shared, as `HolidayCalendar.weekendCalendarEquals`.
//
// A weekend calendar is equal to a weekend calendar with the same identifier - which, the set
// being closed, means to itself alone - and hashes as its identifier does, so the hash is the
// same in every run. A weekend calendar is never equal to a calendar carrying holiday data, even
// where the two share an identifier.

/**
 * The calendar whose holidays are Saturday and Sunday.
 *
 * Also reachable as `HolidayCalendars.SAT_SUN` and `HolidayCalendar.SatSun`, which name this
 * same object.
 */
object SatSun extends HolidayCalendar {

  override val id: HolidayCalendarId = HolidayCalendarIds.SAT_SUN

  /**
   * The days of the week that are holidays.
   *
   * @return Saturday and Sunday
   */
  val weekendDays: Set[DayOfWeek] = Set(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY)

  override def isHoliday(date: LocalDate): Boolean = {
    // compared day by day rather than through the set above: this is the question a calendar is
    // asked once per day of every schedule
    val day = date.getDayOfWeek
    day == DayOfWeek.SATURDAY || day == DayOfWeek.SUNDAY
  }

  override def equals(obj: Any): Boolean = HolidayCalendar.weekendCalendarEquals(this, obj)

  override def hashCode: Int = id.hashCode
}

/**
 * The calendar whose holidays are Friday and Saturday.
 *
 * Also reachable as `HolidayCalendars.FRI_SAT` and `HolidayCalendar.FriSat`, which name this
 * same object.
 */
object FriSat extends HolidayCalendar {

  override val id: HolidayCalendarId = HolidayCalendarIds.FRI_SAT

  /**
   * The days of the week that are holidays.
   *
   * @return Friday and Saturday
   */
  val weekendDays: Set[DayOfWeek] = Set(DayOfWeek.FRIDAY, DayOfWeek.SATURDAY)

  override def isHoliday(date: LocalDate): Boolean = {
    val day = date.getDayOfWeek
    day == DayOfWeek.FRIDAY || day == DayOfWeek.SATURDAY
  }

  override def equals(obj: Any): Boolean = HolidayCalendar.weekendCalendarEquals(this, obj)

  override def hashCode: Int = id.hashCode
}

/**
 * The calendar whose holidays are Thursday and Friday.
 *
 * Also reachable as `HolidayCalendars.THU_FRI` and `HolidayCalendar.ThuFri`, which name this
 * same object.
 */
object ThuFri extends HolidayCalendar {

  override val id: HolidayCalendarId = HolidayCalendarIds.THU_FRI

  /**
   * The days of the week that are holidays.
   *
   * @return Thursday and Friday
   */
  val weekendDays: Set[DayOfWeek] = Set(DayOfWeek.THURSDAY, DayOfWeek.FRIDAY)

  override def isHoliday(date: LocalDate): Boolean = {
    val day = date.getDayOfWeek
    day == DayOfWeek.THURSDAY || day == DayOfWeek.FRIDAY
  }

  override def equals(obj: Any): Boolean = HolidayCalendar.weekendCalendarEquals(this, obj)

  override def hashCode: Int = id.hashCode
}


/**
 * A calendar built from a list of holiday dates and a weekend.
 *
 * This is the calendar an application supplies for a centre whose holidays it publishes itself,
 * and the one the built-in national calendars are expressed as. Construction normalises what it
 * is given: the holidays are sorted and deduplicated, the years they span become the range the
 * calendar knows about, and dates named as working days override both the holidays and the
 * weekend - they are applied last, and a working day outside the holiday range is ignored
 * because there is nothing there to override.
 *
 * Outside the range of years spanned by its holidays the calendar applies its weekend alone
 * rather than declaring those years free of holidays. A date whose year lies outside 0 to 9999
 * is rejected, because no calendar could hold data for it.
 *
 * A weekend must leave some day of the week open. Outside the years its holidays cover a
 * calendar answers from its weekend alone, so one whose weekend closed all seven days would have
 * no business day to answer with - no next, no previous, no shift and no schedule - and the
 * working days it might declare would not help, naming individual dates inside those years
 * rather than making any day of the week a business day. Such a request is refused where it is
 * made, rather than left for every search on the calendar to walk to the end of the range of
 * dates and report the year it reached, which says nothing about what is wrong.
 *
 * Equality is the identifier alone, which is worth stating because it surprises: two calendars
 * claiming to be `GBLO` are the same calendar as far as a schedule is concerned, and comparing
 * their holiday lists would make a calendar resolved from one set of reference data unequal to
 * the same calendar resolved from another. A calendar of this kind is never equal to a weekend
 * calendar, whatever the identifiers say.
 *
 * ===How the holidays are stored===
 *
 * A calendar is asked whether a date is a business day once per day of every schedule that
 * touches it, so the answer has to be cheap. The dates are therefore held as one `Int` per month
 * from January of the first year covered, with '''bit set meaning business day''' - a layout
 * that makes the whole family of questions a handful of machine instructions: whether a date is
 * a business day is one bit test, the next and previous business days are
 * `Integer.numberOfTrailingZeros` and `Integer.numberOfLeadingZeros`, and the count of business
 * days in a range is `Integer.bitCount` over the months it spans. Bits beyond the length of a
 * month are left unset, so they read as holidays and no month-length arithmetic is needed.
 *
 * The array is produced once, during construction, by tabulating over the months rather than by
 * writing into it, and it is never handed out: [[holidays]], [[weekendDays]] and [[workingDays]]
 * each reconstruct a set from it, so the representation cannot be reached, let alone written to,
 * by a caller. It is a field of the instance and nothing more - no method of this type answers
 * with it, and none can be generated that does, which is why it is a parameter the class keeps to
 * itself rather than a member each instance supplies. The array a factory hands to the
 * constructor is copied there, so the array the calendar reads is reachable from nowhere else at
 * all, and a caller that kept or reused the one it supplied cannot alter the calendar it built.
 *
 * Each method that reads the array checks the month index rather than catching the failure of an
 * out-of-bounds read, and a date the array does not cover falls back to the general
 * implementation of [[HolidayCalendar]], which applies the weekend alone.
 *
 * ===How construction is closed===
 *
 * `of` rewrites what it is given rather than merely accepting it, so the type is a
 * `sealed abstract case class` with a private constructor. An abstract case class synthesises
 * neither `apply` nor `copy`, so the factories below are the only way to obtain a calendar and no
 * caller can produce one that skipped the normalisation; being sealed, the one concrete subclass
 * the factories themselves use - the companion's hidden implementation class - can be declared in
 * this file alone.
 *
 * The '''identifier''' is the one case parameter, and the packed representation is a second
 * parameter list. That is not a way around the policy but what the policy asks for here: the
 * identifier is the whole of this type's equality, so `unapply` hands back precisely the value
 * equality is defined on - `case ImmutableHolidayCalendar(id)` - while the bit masks stay
 * unreachable, which they would not be if they were case parameters. The holidays, the weekend
 * and the working days are read back through [[holidays]],
 * [[weekendDays]] and [[workingDays]], which reconstruct them from those masks.
 *
 * A second parameter list rather than abstract members the anonymous subclass implements,
 * because an abstract member is a method and a method answering with an array publishes that
 * array to every caller that can name the type, whatever visibility this file writes: the
 * machine has no notion of a member visible to one package, so what the source calls
 * package-visible is public where it matters. A parameter the class keeps to itself is a field,
 * and a field is exactly as private as it is declared. The constructor is therefore the single
 * gate - it copies the data and judges it, as [[ImmutableHolidayCalendar.ownedMonths]]
 * describes - which matters because it is the only gate that cannot be gone round: the
 * anonymous subclass that instantiates a `sealed abstract` class carries a constructor the
 * machine leaves reachable, so a caller in another language can reach it, and reaching it
 * yields a calendar that a factory of this library could have built and no other.
 */
sealed abstract case class ImmutableHolidayCalendar private (override val id: HolidayCalendarId)(
    private[date] val weekends: Int,
    val startYear: Int,
    packedMonths: Array[Int])
    extends HolidayCalendar {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means - which
  // would carry packed months no factory had built, over an array it kept a reference to - can be
  // stopped is here. The single implementation is the companion's hidden `Impl`, and the refusal
  // of Java serialization is inherited from [[HolidayCalendar]].
  JvmClosure.requireSoleImplementation(this, classOf[ImmutableHolidayCalendar.Impl])


  /**
   * The holiday data, as one `Int` of day-of-month bits per month from January of [[startYear]].
   *
   * A set bit is a business day, which is the layout described above. This is the whole of the
   * state of a calendar beyond its identifier, its weekend and its first year, and it is the
   * one field of these types that could be written to, an array being mutable however the
   * reference to it is held. Three things together make that impossible from outside:
   *
   *   - it is owned by '''this''' instance. The array handed to the constructor is copied by
   *     [[ImmutableHolidayCalendar.ownedMonths]], so a caller that retained the array it
   *     supplied - or that supplied one it goes on to alter - cannot alter this calendar;
   *   - it is visible to this instance '''alone'''. Being `private[this]` it is a field and
   *     nothing else: no method that hands the array out is generated, and none is written, so
   *     there is no route by which a caller in any language can obtain the reference. The two
   *     readers below answer with a single month as an `Int`, which is a copy of a machine word
   *     rather than a reference to storage, and they are what lets one calendar be merged with
   *     another without either of them publishing its array;
   *   - it is '''checked''' before it is owned, so a calendar whose months could not have come
   *     from a factory of this library - a first year outside the years a calendar answers
   *     about, a part-month of data, a range reaching past the last year there is - does not
   *     come into being at all.
   *
   * The alternative, an abstract member implemented by each instance, is what a reader would
   * reach for first and is exactly what cannot be used here: an abstract member is a method,
   * and a method returning the array publishes it to every caller of this class whatever
   * visibility the source gives it.
   *
   * @return the packed months of holiday data
   */
  private[this] val months: Array[Int] =
    ImmutableHolidayCalendar.ownedMonths(id, weekends, startYear, packedMonths)

  // The invariant of this type, stated over the fields the instance actually holds rather than
  // over the dates a factory was given, because the class file of the implementation carries a
  // public constructor whatever the source asked for: a class compiled outside this library can
  // reach it directly, and the check above would admit what it built, its runtime class being the
  // one class that check admits.
  //
  // The fields are the packed form rather than the dates, so this is what the normalisation of
  // [[ImmutableHolidayCalendar.ofNormalized]] amounts to once it has been performed. Two parts of
  // it need no stating at all: a bit mask cannot hold a date twice and cannot hold its dates out
  // of order, so the sorting and the deduplication that factory performs are properties of the
  // representation rather than conditions on it. What remains is the shape of the storage the
  // range of years implies and the layout every read of it assumes - one `Int` per month from
  // January of the first year covered, with no bit set beyond the days a month has, so that
  // `numberOfTrailingZeros`, `numberOfLeadingZeros` and `bitCount` answer with days that exist.
  // A calendar assembled around those conditions rather than through a factory would report the
  // thirtieth of February a business day, and would index its own array out of bounds at the
  // edges of its range.
  //
  // The whole of it is one pass over the stored months, which is what the factory itself costs,
  // and there are about thirty calendars built in a run.
  JvmClosure.requireInvariant(
    "its packed weekend names only days of the week",
    (weekends & ~ImmutableHolidayCalendar.EveryDayOfWeek) == 0)
  JvmClosure.requireInvariant(
    "its stored months are whole years, one month at a time",
    months.length % 12 == 0)
  JvmClosure.requireInvariant(
    "the years its stored months cover are years it can answer from them",
    ImmutableHolidayCalendar.isSupportedYear(startYear) &&
      endYearExclusive <= ImmutableHolidayCalendar.EndSupportedYearExclusive)
  JvmClosure.requireInvariant(
    "a calendar storing no months covers no year, so its first year is zero",
    months.nonEmpty || startYear == 0)
  JvmClosure.requireInvariant(
    "no stored month declares a business day beyond the days that month has",
    ImmutableHolidayCalendar.storedMonthsFitTheirMonths(startYear, months, 0))

  /**
   * The year after the last one the holiday data covers.
   *
   * Together with [[startYear]] this is the range of years within which the calendar answers
   * from its holiday data; outside it the weekend alone is applied. Where a calendar was built
   * with no holidays at all the range is empty and every query applies the weekend.
   *
   * @return the exclusive end of the range of years covered
   */
  def endYearExclusive: Int = startYear + months.length / 12

  /**
   * The number of months of holiday data this calendar holds.
   *
   * Visible within this package because merging two calendars reads the shape of both; it says
   * how much data there is and nothing about what is in it.
   *
   * @return the number of stored months
   */
  private[date] def monthCount: Int = months.length

  /**
   * The stored month at an index, or a month in which no day is a business day.
   *
   * Total, so a caller merging two calendars whose ranges differ can ask either of them about
   * any index of the merged range: a month this calendar does not reach contributes no business
   * day, which is exactly what extending its data with empty months would do. Answering with
   * the month rather than with the array is what keeps the storage unreachable while still
   * letting [[ImmutableHolidayCalendar.combined]] intersect two calendars month by month.
   *
   * @param index  the index of the month, counted from January of [[startYear]]
   * @return the day-of-month business-day bits of that month, or zero where it is not stored
   */
  private[date] def monthOrEmpty(index: Int): Int =
    if (index >= 0 && index < months.length) months(index) else 0

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
   * with, sorted and deduplicated. It is the set a document holds, and it is what makes the
   * storage an implementation detail rather than part of the contract.
   *
   * Only the holidays are built: the walk of the stored months reads the two bits that decide
   * the category of each day and keeps a date only where both say holiday, so a working-day
   * override is recognised by its bits and is never turned into a date nor given a list cell on
   * the way to being discarded. See [[datesOfCategory]].
   *
   * @return the holiday dates, in ascending order
   */
  def holidays: SortedSet[LocalDate] =
    ImmutableHolidayCalendar.sortedDates(datesOfCategory(ImmutableHolidayCalendar.HolidayCategory))

  /**
   * The dates that are business days even though they fall at a weekend.
   *
   * These are the working-day overrides the calendar was built with, restricted - as
   * construction restricts them - to the range of years the calendar covers.
   *
   * Only the overrides are built, for the reason given on [[holidays]], and a calendar that
   * declares none - which is every calendar this library generates - therefore walks its months
   * without constructing a single date and answers with the empty set.
   *
   * @return the weekend dates that are business days, in ascending order
   */
  def workingDays: SortedSet[LocalDate] =
    ImmutableHolidayCalendar.sortedDates(datesOfCategory(ImmutableHolidayCalendar.WorkingDayCategory))

  override def isHoliday(date: LocalDate): Boolean = {
    val index = monthIndex(date)
    if (covers(index)) (months(index) & dayOfMonthBit(date)) == 0 else isHolidayOutsideRange(date)
  }

  override def shift(date: LocalDate, amount: Int): LocalDate = {
    // The limit the family states on [[HolidayCalendar.shift]] is checked here as well as there,
    // because this override answers from the stored months without calling the inherited method
    // at all where the shift stays inside them. A calendar covering many years would otherwise
    // walk millions of days inside its own data before the inherited check was ever reached -
    // which is to say the limit would hold for exactly the calendars that hold the least data.
    HolidayCalendar.checkShift(amount)
    shiftWithinStoredMonths(date, amount)
  }

  /**
   * Shifts the date by a number of business days the limit of the family allows.
   *
   * The body of [[shift]], reached once its precondition holds. A shift that stays inside the
   * stored months is answered from them, and one that leaves them is handed to the general
   * walk, which applies the weekend beyond the range of years this calendar holds data for.
   *
   * @param date  the date to shift
   * @param amount  the number of business days to shift by, within the limit of the family
   * @return the shifted date
   */
  private def shiftWithinStoredMonths(date: LocalDate, amount: Int): LocalDate =
    if (amount > 0) {
      // day-of-month: one-based here, which is the zero-based day-of-month of the following day,
      // so the search starts from the day after the one given
      val shifted = shiftNext(date.getYear, date.getMonthValue, date.getDayOfMonth, amount)
      if (leftStoredMonths(shifted)) shiftOutsideRange(date, amount) else shifted
    } else if (amount < 0) {
      // day-of-month: minus one, so the search starts from the day before the one given
      val shifted = shiftPrevious(date.getYear, date.getMonthValue, date.getDayOfMonth - 1, amount)
      if (leftStoredMonths(shifted)) shiftOutsideRange(date, amount) else shifted
    } else {
      date
    }

  override def next(date: LocalDate): LocalDate = {
    val shifted = shiftNext(date.getYear, date.getMonthValue, date.getDayOfMonth, 1)
    if (leftStoredMonths(shifted)) super.next(date) else shifted
  }

  override def previous(date: LocalDate): LocalDate = {
    val shifted = shiftPrevious(date.getYear, date.getMonthValue, date.getDayOfMonth - 1, -1)
    if (leftStoredMonths(shifted)) previousOutsideRange(date) else shifted
  }

  override def nextSameOrLastInMonth(date: LocalDate): LocalDate = {
    val index = monthIndex(date)
    if (covers(index)) {
      val monthData = months(index)
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
      (months(index) >>> (date.getDayOfMonth - 1)) == 1
    } else {
      checkSupported(date)
      super.isLastBusinessDayOfMonth(date)
    }
  }

  override def lastBusinessDayOfMonth(date: LocalDate): LocalDate = {
    val index = monthIndex(date)
    if (covers(index)) {
      date.withDayOfMonth(32 - Integer.numberOfLeadingZeros(months(index)))
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
      val fromStart = Integer.bitCount(months(startIndex) >>> (startInclusive.getDayOfMonth - 1))
      // business days of the last month from the end date inclusive, which are not in the range
      val beyondEnd = Integer.bitCount(months(endIndex) >>> (endExclusive.getDayOfMonth - 1))
      if (startIndex == endIndex) {
        fromStart - beyondEnd
      } else {
        val untilEnd = Integer.bitCount(months(endIndex)) - beyondEnd
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

  /**
   * Returns the index in the stored months of the month a date falls in.
   *
   * @param date  the date to locate
   * @return the index of the month, or [[ImmutableHolidayCalendar.OutsideStoredMonths]] where the
   *   date falls outside them
   */
  private def monthIndex(date: LocalDate): Int = monthIndexOf(date.getYear, date.getMonthValue)

  /**
   * Returns the index in the stored months of a year and month.
   *
   * The distance is measured in `Long` arithmetic and the result is only narrowed once it is
   * known to be an index of the array. That matters for a date far outside the range: the
   * product of an extreme year and twelve overflows an `Int`, and an overflowed product can land
   * back inside the bounds of the array, which would have this calendar answer for a year it
   * holds no data for from the data of some other year. Measured exactly, a month outside the
   * stored months always reports itself as outside them, so - construction having restricted
   * the stored months to years 0 to 9999 - a date that is covered is a date this calendar
   * supports, and the two notions cannot come apart however the calendar was built.
   *
   * @param year  the year
   * @param month  the month-of-year, from 1 to 12
   * @return the index of the month, or [[ImmutableHolidayCalendar.OutsideStoredMonths]] where the
   *   month falls outside them
   */
  private def monthIndexOf(year: Int, month: Int): Int = {
    val index = (year.toLong - startYear.toLong) * 12L + (month - 1).toLong
    if (index < 0L || index >= months.length.toLong) ImmutableHolidayCalendar.OutsideStoredMonths
    else index.toInt
  }

  /**
   * Checks whether a month index is one of the stored months.
   *
   * @param index  the index to check
   * @return true where the index identifies a stored month
   */
  private def covers(index: Int): Boolean = index != ImmutableHolidayCalendar.OutsideStoredMonths

  /**
   * Returns the bit within a stored month that holds the day-of-month of a date.
   *
   * @param date  the date
   * @return the bit of that day-of-month
   */
  private def dayOfMonthBit(date: LocalDate): Int = 1 << (date.getDayOfMonth - 1)

  /**
   * Checks whether a search of the stored months left them without finding a date.
   *
   * The searches answer with a date rather than an optional date, and say that they found none
   * by answering with [[ImmutableHolidayCalendar.DateOutsideStoredMonths]] - which is compared
   * here by '''reference''', not by value. That is sound because no search can produce that
   * date: it lies in a year no calendar may hold data for, construction having restricted the
   * stored months to the years 0 to 9999, so the only way to obtain it is for a search to have
   * returned the very object the companion holds.
   *
   * @param found  the date a search answered with
   * @return true where the search left the stored months
   */
  private def leftStoredMonths(found: LocalDate): Boolean =
    found eq ImmutableHolidayCalendar.DateOutsideStoredMonths

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
   * Each step moves to the least significant set bit at or after the day already reached, and a
   * month with no business day left is left by recursing into the next one, carrying the steps
   * still to take. The recursion is tail recursion, so the search needs no mutable state.
   *
   * A month outside the stored months yields [[ImmutableHolidayCalendar.DateOutsideStoredMonths]]
   * rather than failing, which is what lets the caller fall back to the general implementation
   * for a date this calendar holds no data for. The search returns a date rather than an optional
   * date so that a shift that stays within the stored months - which is what a shift on a
   * calendar of real holiday data does - allocates nothing at all beyond the date it answers
   * with; see [[leftStoredMonths]] for why that is sound.
   *
   * @param year  the year reached
   * @param month  the month-of-year reached, from 1 to 12
   * @param dayOfMonth0  the zero-based day-of-month to search from
   * @param amount  the number of business days still to take, positive until the last step
   * @return the shifted date, or [[ImmutableHolidayCalendar.DateOutsideStoredMonths]] where the
   *   search left the stored months
   */
  @tailrec
  private def shiftNext(year: Int, month: Int, dayOfMonth0: Int, amount: Int): LocalDate = {
    val index = monthIndexOf(year, month)
    if (!covers(index)) {
      ImmutableHolidayCalendar.DateOutsideStoredMonths
    } else if (amount <= 0) {
      LocalDate.of(year, month, dayOfMonth0)
    } else {
      // shift the day reached into bit 0, dropping the days before it
      val remaining = months(index) >>> dayOfMonth0
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
   * The reverse of [[shiftNext]]: each step moves to the most significant set bit at or before
   * the day already reached, and a month with no earlier business day is left by recursing into
   * the one before it.
   *
   * @param year  the year reached
   * @param month  the month-of-year reached, from 1 to 12
   * @param dayOfMonth  the one-based day-of-month to search from, which may be zero or negative
   * @param amount  the number of business days still to take, negative until the last step
   * @return the shifted date, or [[ImmutableHolidayCalendar.DateOutsideStoredMonths]] where the
   *   search left the stored months
   */
  @tailrec
  private def shiftPrevious(year: Int, month: Int, dayOfMonth: Int, amount: Int): LocalDate = {
    val index = monthIndexOf(year, month)
    if (!covers(index)) {
      ImmutableHolidayCalendar.DateOutsideStoredMonths
    } else if (amount >= 0) {
      LocalDate.of(year, month, dayOfMonth + 1)
    } else {
      // shift the day reached into bit 31, dropping the days after it
      val remaining = months(index) << (32 - dayOfMonth)
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
   * @param counted  the business days counted in the months already visited
   * @return the total number of business days in those months
   */
  @tailrec
  private def businessDaysOfMonths(index: Int, endIndex: Int, counted: Int): Int =
    if (index >= endIndex) counted
    else businessDaysOfMonths(index + 1, endIndex, counted + Integer.bitCount(months(index)))

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
   * Visible within this package because the two callers that need both sets - writing a calendar
   * out, and merging two calendars whose years do not meet - should scan the months once rather
   * than read [[holidays]] and [[workingDays]] in turn and scan them twice. Those two accessors
   * do not go through this, each scanning for its own category alone. The result is deliberately
   * not retained: the sets are large, and a calendar that is asked for them once should not hold
   * them for the rest of its life.
   *
   * @return the holiday dates and the working weekend dates, both in ascending order
   */
  private[date] def holidaysAndWorkingDays: (SortedSet[LocalDate], SortedSet[LocalDate]) = {
    val (holidayDates, workingDates) = exceptionalDates.partition(date => isHoliday(date))
    (ImmutableHolidayCalendar.sortedDates(holidayDates), ImmutableHolidayCalendar.sortedDates(workingDates))
  }

  /**
   * Recovers every date of the stored months that the weekend alone does not account for.
   *
   * These are the dates that carry information beyond the weekend: a day that is not a business
   * day and does not fall at a weekend is a holiday, and a day that is a business day and does
   * fall at a weekend is a working-day override. A day where the two disagree is an ordinary
   * business day or an ordinary weekend, is carried by the weekend alone, and is not returned.
   * Which of the two kinds a returned date is follows from [[isHoliday]], which reads the same
   * bit, so the caller this exists for - [[holidaysAndWorkingDays]], which needs both kinds -
   * splits one walk in two. A caller that wants one kind alone asks [[datesOfCategory]] for that
   * kind instead, and so never builds the other.
   *
   * The whole of the stored months is walked once, from the last month to the first and within
   * each month from the last day to the first, prepending each date that is found. Walking
   * backwards is what makes the result ascending without a reversal, and prepending is what makes
   * the walk allocate one list cell per date returned rather than a collection per month. Both
   * loops are tail recursive over `Int` indices, so the walk holds no mutable state and needs no
   * stack however many years the calendar covers.
   *
   * Where the calendar holds no months there is nothing to walk and the result is empty.
   *
   * @return every holiday and working-day override of the stored months, in ascending order
   */
  private def exceptionalDates: List[LocalDate] = {
    @tailrec
    def loop(index: Int, found: List[LocalDate]): List[LocalDate] =
      if (index < 0) found else loop(index - 1, exceptionalDatesOfMonth(index, found))
    loop(months.length - 1, Nil)
  }

  /**
   * Prepends the exceptional dates of one stored month to the dates already found.
   *
   * @param index  the index of the month to scan
   * @param later  the dates found in the months after this one, in ascending order
   * @return the exceptional dates of this month followed by those dates, in ascending order
   */
  private def exceptionalDatesOfMonth(index: Int, later: List[LocalDate]): List[LocalDate] = {
    // the month of an index, worked out arithmetically rather than by adding months to the first
    // day of the first year, which allocated a date per month more than this does
    val firstOfMonth = LocalDate.of(startYear + index / 12, index % 12 + 1, 1)
    val monthData = months(index)
    val firstBitOfWeek = firstOfMonth.getDayOfWeek.getValue - 1
    @tailrec
    def loop(dayOffset: Int, found: List[LocalDate]): List[LocalDate] =
      if (dayOffset < 0) {
        found
      } else {
        val businessDay = (monthData & (1 << dayOffset)) != 0
        val weekendDay = (weekends & (1 << ((firstBitOfWeek + dayOffset) % 7))) != 0
        // a day the weekend accounts for adds nothing; the rest is a holiday or an override
        val next = if (businessDay == weekendDay) firstOfMonth.withDayOfMonth(dayOffset + 1) :: found else found
        loop(dayOffset - 1, next)
      }
    loop(firstOfMonth.lengthOfMonth - 1, later)
  }

  /**
   * Recovers the dates of one exceptional category alone from the stored months.
   *
   * Two bits decide the category of a day between them: its bit in its stored month, which is
   * set for a business day, and the bit of its day of the week in the weekend of the calendar. A
   * holiday is a day that is neither - both bits clear - and a working-day override is a day
   * that is both. A day whose two bits disagree is an ordinary business day or an ordinary
   * weekend, is carried by the weekend alone, and belongs to neither category.
   *
   * Both bits are compared against the category asked for, which is what lets this build only
   * the dates the caller wants: a date of the other category is recognised by its bits and never
   * becomes a `LocalDate` nor occupies a list cell. A caller that wants both categories reads
   * [[holidaysAndWorkingDays]] instead, which partitions one walk rather than making two.
   *
   * The stored months are walked once, from the last month to the first and within each month
   * from the last day to the first, prepending each date kept. Walking backwards is what makes
   * the result ascending without a reversal, and prepending is what makes the walk allocate one
   * list cell per date returned rather than a collection per month. Both loops are tail
   * recursive over `Int` indices, so the walk holds no mutable state and needs no stack however
   * many years the calendar covers.
   *
   * Where the calendar holds no months there is nothing to walk and the result is empty.
   *
   * @param category  the value both stored bits take for the category wanted, which is
   *   [[ImmutableHolidayCalendar.HolidayCategory]] for the holidays and
   *   [[ImmutableHolidayCalendar.WorkingDayCategory]] for the working-day overrides
   * @return the dates of that category, in ascending order
   */
  private def datesOfCategory(category: Boolean): List[LocalDate] = {
    @tailrec
    def loop(index: Int, found: List[LocalDate]): List[LocalDate] =
      if (index < 0) found else loop(index - 1, datesOfCategoryInMonth(index, category, found))
    loop(months.length - 1, Nil)
  }

  /**
   * Prepends the dates of one exceptional category found in one stored month to the dates
   * already found.
   *
   * @param index  the index of the month to scan
   * @param category  the value both stored bits take for the category wanted
   * @param later  the dates found in the months after this one, in ascending order
   * @return the dates of that category in this month followed by those dates, in ascending order
   */
  private def datesOfCategoryInMonth(
      index: Int,
      category: Boolean,
      later: List[LocalDate]): List[LocalDate] = {

    // the month of an index, worked out arithmetically rather than by adding months to the first
    // day of the first year, which allocated a date per month more than this does
    val firstOfMonth = LocalDate.of(startYear + index / 12, index % 12 + 1, 1)
    val monthData = months(index)
    val firstBitOfWeek = firstOfMonth.getDayOfWeek.getValue - 1
    @tailrec
    def loop(dayOffset: Int, found: List[LocalDate]): List[LocalDate] =
      if (dayOffset < 0) {
        found
      } else {
        val businessDay = (monthData & (1 << dayOffset)) != 0
        val weekendDay = (weekends & (1 << ((firstBitOfWeek + dayOffset) % 7))) != 0
        // the date is constructed only where it is kept: both bits have to be the category asked
        // for, so a day of the other category costs the two bit checks and nothing else
        val next =
          if (businessDay == category && weekendDay == category) firstOfMonth.withDayOfMonth(dayOffset + 1) :: found
          else found
        loop(dayOffset - 1, next)
      }
    loop(firstOfMonth.lengthOfMonth - 1, later)
  }
}


/**
 * Builds calendars from published holiday dates, and carries their typeclass instances.
 *
 * Every factory here answers with a calendar rather than with a failure: a list of dates and a
 * weekend describe a calendar whatever order the dates arrive in, so there is no error channel.
 * What the factories do instead is normalise - sort and deduplicate the dates, derive the range
 * of years they span, and apply the working-day overrides last - which is why two calendars
 * built from the same dates in different orders are identical rather than merely equal. The one
 * thing they refuse is a holiday in a year no calendar may hold data for, which is a fault in
 * the calling code and fails fast.
 *
 * The `Hash` and `Show` declared below the builders are the family's instances restated at the
 * type of this member, which the invariance of those typeclasses requires; [[HolidayCalendar]]
 * keeps its own pair for a value typed as the family, and both pairs read the same `equals` and
 * the same `toString`.
 */
object ImmutableHolidayCalendar {

  /**
   * The repeating seven-bit pattern that clears one day of the week from a month.
   *
   * Bits 0, 7, 14, 21 and 28 are set, so shifting the pattern left by the offset between a
   * weekend day and the first of the month selects every occurrence of that day within the
   * month, and clearing those bits removes the whole of that weekday in one operation. It is
   * written in hexadecimal because Scala 2 has no binary literal.
   */
  private val WeekendPattern: Int = 0x10204081

  /**
   * The bits a packed weekend may set, which are the seven days of a week.
   *
   * One bit per day of the week in the layout [[weekendBit]] defines, so this is every bit that
   * function can produce and no other. It is what the invariant of a calendar tests its packed
   * weekend against: a mask holding anything above those seven bits describes no day, and the
   * weekend is read back a day at a time, so a bit belonging to no day would be carried silently.
   */
  private val EveryDayOfWeek: Int = 0x7f

  /**
   * The value both stored bits of a day take where that day is a holiday.
   *
   * A holiday is a day the calendar does not call a business day and does not call a weekend
   * day, so neither its bit in its stored month nor the bit of its day of the week in the
   * weekend is set. Named rather than written as a bare boolean so that the recovery of one
   * category reads as the category it asks for.
   */
  private val HolidayCategory: Boolean = false

  /**
   * The value both stored bits of a day take where that day is a working-day override.
   *
   * An override is a day the calendar calls a business day and also calls a weekend day, so both
   * bits are set. The counterpart of [[HolidayCategory]].
   */
  private val WorkingDayCategory: Boolean = true

  /**
   * The ordering of dates applied when the holidays of a calendar are sorted.
   *
   * `LocalDate` implements `Comparable` over `ChronoLocalDate` rather than over itself, so no
   * ordering is derivable implicitly and one has to be named. It is handed to each sorted set
   * explicitly rather than being implicit, so that it cannot become an ambient ordering for
   * every date comparison in this package.
   */
  private[date] val dateOrdering: Ordering[LocalDate] =
    Ordering.fromLessThan((first, second) => first.isBefore(second))

  /**
   * The stored months of a calendar built with no holidays at all.
   *
   * A single shared value, which is safe because an empty array has nothing that could be
   * written to it and every calendar built this way applies its weekend alone.
   */
  private val emptyLookup: Array[Int] = Array.emptyIntArray

  /**
   * Builds a calendar over its packed representation, which is the one route into the type.
   *
   * [[ImmutableHolidayCalendar]] is a sealed abstract case class whose only case parameter is the
   * identifier, the packed weekend, the first year covered and the packed months being a second
   * parameter list the class keeps to itself. An instance is therefore an [[Impl]], the hidden
   * subclass declared here that adds nothing, and every published factory ends here, so there is
   * exactly one place where a calendar comes into being and exactly one place that has to be read
   * to know what a calendar holds.
   *
   * Everything that decides whether those four values describe a calendar at all is applied by
   * the constructor rather than here, and the array is copied there as well - see
   * [[ownedMonths]]. That is deliberate: the constructor of a class is the only gate that no
   * route into the type can go round, and a route round this method exists, because the
   * anonymous subclass a `sealed abstract` class is instantiated through carries a constructor
   * that the machine does not hide however this file spells its visibility. A caller reaching
   * that constructor from another language gets the same checks and the same copy as a caller
   * of [[of]] does, so the calendars that exist are the calendars a factory could have built.
   *
   * @param id  the identifier of the calendar
   * @param packedWeekends  the days of the week that are holidays, packed one bit per day
   * @param firstYear  the first year the holiday data covers, zero where there is none
   * @param packedMonths  the holiday data, one `Int` of day-of-month bits per month from January
   *   of `firstYear`, a set bit meaning business day
   * @return the calendar over that representation
   */
  private def create(
      id: HolidayCalendarId,
      packedWeekends: Int,
      firstYear: Int,
      packedMonths: Array[Int]): ImmutableHolidayCalendar =
    new Impl(id, packedWeekends, firstYear, packedMonths)

  /** The number of days there are in a week, which is how many a weekend may close. */
  private[date] val DaysOfWeek: Int = 7

  /** The number of months there are in a year, the unit the holiday data is stored in. */
  private val MonthsOfYear: Int = 12

  /** The packed weekend that closes every day of the week, which no calendar may declare. */
  private val EveryDayClosed: Int = (1 << DaysOfWeek) - 1

  /**
   * Checks the packed representation of a calendar and hands back the copy it will own.
   *
   * This is the one gate every calendar passes through, and it is called from the constructor
   * so that it cannot be avoided - by a factory of this file, by the anonymous subclass the
   * factories instantiate, or by a caller in another language reaching that subclass directly.
   * Five things are required of the representation, and each of them is something no factory
   * of this library could produce:
   *
   *   - '''some day of the week is open'''. A weekend closing all seven days leaves a calendar
   *     with no business day at all: [[HolidayCalendar.nextOrSame]] would have nothing to
   *     answer with, and neither would any shift, adjustment, schedule or index observation
   *     built on one. Such a thing is not a calendar, and the working days a calendar may
   *     declare do not make it one - they name individual dates within the years its holidays
   *     cover, while a calendar is asked about dates in any year at all;
   *   - '''the weekend names days of the week and nothing else'''. Seven days have bits, and a
   *     bit above them names no day: it would be carried by equality, printed by nothing, and
   *     read by nothing, so two calendars closing the same days could compare unequal;
   *   - '''the data is whole months''', from January of the first year covered, because that is
   *     what the layout means and what indexing it assumes;
   *   - '''the years covered are years a calendar answers about''', 0 to 9999, and a calendar
   *     covering no years starts at year zero. A calendar answers a question about a date
   *     inside its range from its data and one outside it from its weekend, so data for a year
   *     it would refuse to be asked about could never be read, and a first year outside that
   *     range would make the arithmetic that indexes the data place a date in a month belonging
   *     to some other year. An empty range has no first year to speak of, and every factory
   *     writes zero there, so anything else is a second representation of one calendar;
   *   - '''every month names days that month has'''. A bit above the length of its month names
   *     a day that does not exist - the 30th of February, the 32nd of January - and the bit is
   *     not inert: the search of the stored months answers with the day the bit sits at, and
   *     constructing that date fails far from here, in a caller that asked a calendar a
   *     perfectly ordinary question. February is checked against the year it belongs to, so a
   *     leap day is a day in a leap year and not in any other.
   *
   * The copy is what makes the calendar immutable in fact rather than by convention: the array
   * a caller supplies stays the caller's, and the array the calendar reads is reachable from
   * nowhere else. It costs one machine word per month of the range, once, against a calendar
   * that is then queried for the lifetime of the program - and an empty range needs no copy at
   * all, an empty array having nothing that could be written to it.
   *
   * @param id  the identifier of the calendar, for the failure messages
   * @param packedWeekends  the days of the week that are holidays, packed one bit per day
   * @param firstYear  the first year the holiday data covers, zero where there is none
   * @param packedMonths  the holiday data as supplied
   * @return the copy of the holiday data the calendar owns
   * @throws IllegalArgumentException where the representation is one no calendar may have
   */
  private def ownedMonths(
      id: HolidayCalendarId,
      packedWeekends: Int,
      firstYear: Int,
      packedMonths: Array[Int]): Array[Int] = {

    ArgCheck.isTrue(
      (packedWeekends & EveryDayClosed) != EveryDayClosed,
      s"Holiday calendar '${id.name}' must leave at least one day of the week open, but its " +
        s"weekend closes every one of them, so it has no business day")
    ArgCheck.isTrue(
      (packedWeekends & ~EveryDayClosed) == 0,
      s"Holiday calendar '${id.name}' must close days of the week and nothing else, but its " +
        s"weekend names something beyond the seven days: $packedWeekends")
    ArgCheck.isTrue(
      packedMonths.length % MonthsOfYear == 0,
      s"Holiday calendar '${id.name}' must hold whole years of months, but holds: " +
        s"${packedMonths.length} months")
    val yearsCovered = packedMonths.length / MonthsOfYear
    ArgCheck.isTrue(
      isSupportedYear(firstYear) &&
        firstYear.toLong + yearsCovered.toLong <= EndSupportedYearExclusive.toLong,
      s"Holiday calendar '${id.name}' cannot hold holidays outside the accepted range " +
        s"(year 0000 to 9999), but holds $yearsCovered years from: $firstYear")
    ArgCheck.isTrue(
      yearsCovered > 0 || firstYear == 0,
      s"Holiday calendar '${id.name}' covering no years must start at year 0, but starts " +
        s"at: $firstYear")
    val impossibleMonth = firstMonthNamingAMissingDay(firstYear, packedMonths)
    ArgCheck.isTrue(
      impossibleMonth.isEmpty,
      // built only where there is an offending month, the argument being taken by name, so the
      // alternative below stands for a case the check itself has already excluded
      impossibleMonth match {
        case Some(index) =>
          s"Holiday calendar '${id.name}' cannot name a day its month does not have, but the " +
            s"month ${monthName(firstYear, index)} does: ${packedMonths(index)}"
        case None => s"Holiday calendar '${id.name}' names only days its months have"
      })
    if (packedMonths.length == 0) emptyLookup else packedMonths.clone()
  }

  /**
   * Finds the first stored month naming a day that month does not have.
   *
   * The bits of a month are days of the month, so a month of `n` days may use the lowest `n` of
   * them and no more. Every month is tested, and the index of the first offender is handed back
   * for the message - one integer comparison per month of the range, once, on the same pass
   * that the copy is made for, against a calendar queried for the lifetime of the program.
   *
   * @param firstYear  the first year the holiday data covers
   * @param packedMonths  the holiday data as supplied, whole years of months
   * @return the index of the first month naming a day it does not have, or none where every
   *   month names only its own days
   */
  private def firstMonthNamingAMissingDay(firstYear: Int, packedMonths: Array[Int]): Option[Int] =
    Iterator
      .range(0, packedMonths.length)
      .find(index => (packedMonths(index) & ~daysOfMonthMask(firstYear, index)) != 0)

  /**
   * The bits a stored month may use, which is one bit per day the month has.
   *
   * Day of month `d` is held in bit `d - 1`, so a month of `n` days may use bits 0 to `n - 1`.
   * A 31-day month therefore masks to the lowest 31 bits and never to the highest one, which
   * would stand for a 32nd day.
   *
   * @param firstYear  the first year the holiday data covers
   * @param index  the index of the month within the holiday data
   * @return the bits that month may use
   */
  private def daysOfMonthMask(firstYear: Int, index: Int): Int =
    (1 << daysOfMonth(firstYear + index / MonthsOfYear, index % MonthsOfYear + 1)) - 1

  /**
   * The number of days in a month of a year.
   *
   * Answered arithmetically rather than by building a date, because it is asked once per month
   * of a calendar's range - which is a hundred and twenty thousand times for a calendar
   * covering every year a calendar may cover.
   *
   * @param year  the year the month belongs to
   * @param month  the month of the year, 1 to 12
   * @return the number of days in that month
   */
  private def daysOfMonth(year: Int, month: Int): Int =
    month match {
      case 2 => if (LocalDateUtils.isLeapYear(year)) 29 else 28
      case 4 | 6 | 9 | 11 => 30
      case _ => 31
    }

  /**
   * The one implementation of a packed calendar.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it. It is declared rather than written as an anonymous subclass at the instantiation site
   * for two reasons, both about what the class file says: a private member class is one a Java
   * compiler refuses to name, where an anonymous class is public and can be instantiated directly
   * by a caller in another language, and a named class can be compared against, which is what
   * lets [[ImmutableHolidayCalendar]] refuse in its own constructor to be any other
   * implementation.
   *
   * It adds nothing: the packed weekend, the first year and the packed months are handed to the
   * second parameter list of the class, which checks them and takes its own copy of the array -
   * see [[ownedMonths]] - so nothing about the representation is reachable through this class
   * either.
   *
   * @param id  the identifier of the calendar
   * @param packedWeekends  the days of the week that are holidays, packed one bit per day
   * @param firstYear  the first year the holiday data covers, zero where there is none
   * @param packedMonths  the holiday data, one `Int` of day-of-month bits per month from January
   *   of `firstYear`, a set bit meaning business day
   */
  private final class Impl(
      id: HolidayCalendarId,
      packedWeekends: Int,
      firstYear: Int,
      packedMonths: Array[Int])
      extends ImmutableHolidayCalendar(id)(packedWeekends, firstYear, packedMonths)

  /**
   * Names a stored month for a failure message, as a year and a month of that year.
   *
   * @param firstYear  the first year the holiday data covers
   * @param index  the index of the month within the holiday data
   * @return the month, written as `YYYY-MM`
   */
  private def monthName(firstYear: Int, index: Int): String =
    f"${firstYear + index / MonthsOfYear}%04d-${index % MonthsOfYear + 1}%02d"

  /** The month index that says a year and month fall outside the stored months of a calendar. */
  private val OutsideStoredMonths: Int = -1

  /**
   * The date that says a search of the stored months left them without finding a business day.
   *
   * The earliest date there is, which is therefore a date no calendar can hold data for - the
   * years a calendar may cover are 0 to 9999 - so it cannot be confused with a date a search
   * found. It is compared by reference wherever it is read, never by value.
   */
  private val DateOutsideStoredMonths: LocalDate = LocalDate.MIN

  private val FirstSupportedYear: Int = 0

  private val EndSupportedYearExclusive: Int = 10000

  /**
   * Checks whether a year is one a calendar may hold holiday data for.
   *
   * The range is 0 to 9999, the years a calendar answers questions about at all, so a calendar
   * can never be built holding data it would refuse to read back. It also bounds what building a
   * calendar costs: the stored months span at most ten thousand years, so at most 120,000
   * machine words, whoever supplied the dates.
   *
   * @param year  the year to check
   * @return true where a calendar may hold data for the year
   */
  private[date] def isSupportedYear(year: Int): Boolean =
    year >= FirstSupportedYear && year < EndSupportedYearExclusive

  /**
   * Sorts and deduplicates dates into the ordered set the factories of this object expect.
   *
   * The one ordering of dates used inside this package is applied here, so that every ordered
   * set handed to [[ofNormalized]] is ordered the same way. Visible within the package because
   * the generated calendars are assembled date by date and hand their result straight to that
   * factory.
   *
   * @param dates  the dates, in any order and with any duplicates
   * @return the dates, sorted and deduplicated
   */
  private[date] def sortedDates(dates: IterableOnce[LocalDate]): SortedSet[LocalDate] =
    SortedSet.from(dates)(dateOrdering)

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
   * @throws IllegalArgumentException where a holiday falls outside the years 0 to 9999
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
   * themselves - which is how a centre that works a six-day week is described. It may not name
   * all seven days, which would leave the calendar with no business day at all.
   *
   * @param id  the identifier of the calendar
   * @param holidays  the holiday dates, in any order and with any duplicates
   * @param weekendDays  the days of the week that are holidays, which may be empty and may not
   *   be all seven
   * @return the calendar
   * @throws IllegalArgumentException where a holiday falls outside the years 0 to 9999, or where
   *   the weekend closes every day of the week
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
   * weekend and holds no data an override could apply to.
   *
   * Every holiday must fall in a year a calendar can be asked about, which is 0 to 9999. That is
   * a precondition on the caller rather than a property of market data - a calendar that held a
   * date outside those years could not read it back, and the range of years its holidays span is
   * what its storage is allocated from - so it fails fast. Working days outside the range the
   * holidays span are ignored rather than rejected, as described above.
   *
   * The weekend must leave some day of the week open, for the reason the class documentation
   * gives: a calendar closed on all seven days has no business day in any year outside its
   * holidays, and the working days named here do not change that. This too is a precondition on
   * the caller and fails fast; a '''document''' describing such a calendar is refused in the
   * error channel by the reader of this family instead.
   *
   * @param id  the identifier of the calendar
   * @param holidays  the holiday dates, in any order and with any duplicates
   * @param weekendDays  the days of the week that are holidays, which may be empty and may not
   *   be all seven
   * @param workingDays  the dates that are business days whatever the holidays and weekend say
   * @return the calendar
   * @throws IllegalArgumentException where a holiday falls outside the years 0 to 9999, or where
   *   the weekend closes every day of the week
   */
  def of(
      id: HolidayCalendarId,
      holidays: Iterable[LocalDate],
      weekendDays: Iterable[DayOfWeek],
      workingDays: Iterable[LocalDate]): ImmutableHolidayCalendar =

    ofNormalized(id, sortedDates(holidays), weekendDays, workingDays)

  /**
   * Obtains a calendar from holidays that are already sorted and deduplicated.
   *
   * This is [[of]] without its first step, for a caller that has an ordered set of dates in hand
   * already - which is what the generated calendars have, since they accumulate their rules into
   * one. Sorting such a set again is the whole of what this avoids, and it is worth avoiding: a
   * generated calendar holds thousands of dates and there are twenty-six of them.
   *
   * Visible within this package alone, because the set has to be ordered by
   * [[ImmutableHolidayCalendar.dateOrdering]] for the range of the calendar to be read off its
   * ends, and only this package can be held to that. Use [[sortedDates]] to build one.
   *
   * @param id  the identifier of the calendar
   * @param holidays  the holiday dates, sorted and deduplicated by [[dateOrdering]]
   * @param weekendDays  the days of the week that are holidays, which may be empty and may not
   *   be all seven
   * @param workingDays  the dates that are business days whatever the holidays and weekend say
   * @return the calendar
   * @throws IllegalArgumentException where a holiday falls outside the years 0 to 9999, or where
   *   the weekend closes every day of the week
   */
  private[date] def ofNormalized(
      id: HolidayCalendarId,
      holidays: SortedSet[LocalDate],
      weekendDays: Iterable[DayOfWeek],
      workingDays: Iterable[LocalDate]): ImmutableHolidayCalendar = {

    val weekendSet = weekendDays.toSet
    val weekends = weekendSet.foldLeft(0)((mask, day) => mask | weekendBit(day))
    if (holidays.isEmpty) {
      create(id, weekends, 0, emptyLookup)
    } else {
      val startYear = holidays.head.getYear
      val endYearExclusive = holidays.last.getYear + 1
      checkSupportedYears(id, startYear, holidays.last.getYear)
      val lookup = buildLookup(holidays, weekendSet, startYear, endYearExclusive, workingDays)
      create(id, weekends, startYear, lookup)
    }
  }

  /**
   * Checks that the years a calendar's holidays span are years it may hold data for.
   *
   * Both ends are checked, because both are needed: the earliest holiday becomes the first year
   * stored and the latest the last, and the number of months between them is what the storage of
   * the calendar is allocated from. Restricting them to 0 to 9999 keeps that allocation bounded
   * by 120,000 months and keeps the arithmetic that indexes it well inside an `Int`, whatever
   * dates a caller - or a document - supplies.
   *
   * @param id  the identifier of the calendar, for the failure message
   * @param firstYear  the year of the earliest holiday
   * @param lastYear  the year of the latest holiday
   * @throws IllegalArgumentException where either year is outside 0 to 9999
   */
  private def checkSupportedYears(id: HolidayCalendarId, firstYear: Int, lastYear: Int): Unit =
    ArgCheck.isTrue(
      isSupportedYear(firstYear) && isSupportedYear(lastYear),
      s"Holiday calendar '${id.name}' cannot hold holidays outside the accepted range " +
        s"(year 0000 to 9999), but its holidays span: $firstYear to $lastYear")

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
   * calendar knows nothing about is a holiday. Two calendars whose ranges do not overlap at all
   * are merged from their dates instead, because intersecting months that do not meet would lose
   * them.
   *
   * The weekend of the merged calendar closes every day either of them closes, so two calendars
   * that between them close all seven days describe a merged calendar with no business day at
   * all - `Sat/Sun` and a calendar whose weekend is Monday to Friday, for instance, each of them
   * ordinary on its own. No such calendar exists, for the reason the class documentation gives,
   * so the merge is refused rather than answered with one. The lazy composite
   * [[HolidayCalendar.combinedWith]] builds instead is not refused, holding its two calendars
   * apart rather than merging their weekends; a search of '''it''' is what reports that the pair
   * has no business day, when one is made.
   *
   * @param calendar1  the first calendar
   * @param calendar2  the second calendar
   * @return the merged calendar, identified by the combination of the two identifiers
   * @throws IllegalArgumentException where the two weekends between them close every day of the
   *   week, so that the merged calendar would have no business day
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
        // the ranges do not meet, so the calendars are rebuilt from the dates they declare. Each
        // calendar reports its holidays and its working days from one scan of its own months, and
        // both sets are already ordered, so the merged calendar is built without sorting again
        val (holidays1, workingDays1) = calendar1.holidaysAndWorkingDays
        val (holidays2, workingDays2) = calendar2.holidaysAndWorkingDays
        ofNormalized(
          combinedId,
          holidays1 ++ holidays2,
          calendar1.weekendDays ++ calendar2.weekendDays,
          workingDays1 ++ workingDays2)
      } else {
        // the ranges meet, so the months are intersected, using the earlier start as the base.
        // The merged range is the union of two ranges that are already within the years a
        // calendar may hold data for, so it is within them too and needs no further check
        val firstIsLower = calendar1.startYear <= calendar2.startYear
        val base = if (firstIsLower) calendar1 else calendar2
        val other = if (firstIsLower) calendar2 else calendar1
        val offset = (other.startYear - base.startYear) * MonthsOfYear
        val size = Math.max(base.monthCount, other.monthCount + offset)
        // each calendar is asked about one month at a time rather than for its stored data, so
        // neither of them publishes the array it holds. A month the base calendar does not
        // reach answers as an empty month, which is exactly what extending its data with empty
        // months and intersecting would give
        val merged = Array.tabulate(size) { index =>
          val baseMonth = base.monthOrEmpty(index)
          val otherIndex = index - offset
          // intersected, because a set bit is a business day and a day has to be one in both
          if (otherIndex >= 0 && otherIndex < other.monthCount) baseMonth & other.monthOrEmpty(otherIndex)
          else baseMonth
        }
        // unioned, because a set bit here is a weekend day and either weekend closes the day
        val weekends = calendar1.weekends | calendar2.weekends
        create(combinedId, weekends, base.startYear, merged)
      }
    }

  /**
   * Returns the bit that marks a day of the week as part of a weekend.
   *
   * @param day  the day of the week
   * @return the bit of that day
   */
  private def weekendBit(day: DayOfWeek): Int = 1 << (day.getValue - 1)

  /**
   * Whether every stored month declares business days only among the days that month has.
   *
   * This is the layout [[buildLookup]] produces - each month starts as `(1 << lengthOfMonth) - 1`
   * and is only ever cleared from there, and the working days that are set back are real dates of
   * the month - and it is the layout every read of the array assumes: a day is found by counting
   * trailing or leading zeroes of a month, so a bit set above the end of a month would be
   * answered with as a day that does not exist, and the count of business days in a range would
   * include it. It is checked by the invariant a calendar states in its own constructor, which is
   * where a calendar assembled by any other route arrives.
   *
   * The walk is one pass over the stored months carrying its index as a parameter, so it holds no
   * mutable state and compiles to a jump; the length of each month is read from the platform's
   * own calendar rather than from a date built for the purpose, so the pass allocates nothing.
   *
   * @param startYear  the first year the stored months cover
   * @param lookup  the stored months, one `Int` of day-of-month bits per month
   * @param index  the month to test, the walk starting from zero
   * @return true where no month sets a bit above the days it has
   */
  @tailrec
  private def storedMonthsFitTheirMonths(startYear: Int, lookup: Array[Int], index: Int): Boolean =
    if (index >= lookup.length) {
      true
    } else {
      val monthOfYear = Month.of(index % 12 + 1)
      val leapYear = Year.isLeap((startYear + index / 12).toLong)
      // one set bit per day of the month, which is what the month starts as when it is built
      val everyDay = (1 << monthOfYear.length(leapYear)) - 1
      (lookup(index) & ~everyDay) == 0 && storedMonthsFitTheirMonths(startYear, lookup, index + 1)
    }

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

  /**
   * The hashing and equality of calendars built from holiday dates.
   *
   * Taken from the `equals` and `hashCode` of the class, which compare and hash the `id` and
   * '''nothing else''': two calendars of one identifier are equal however their holidays differ,
   * because a calendar is identified by what it claims to be rather than by the dates it happens
   * to hold. It therefore agrees with [[HolidayCalendar.hash]], which reads the same `equals` for
   * a value typed as the family.
   *
   * It is declared here as well as on the family because `cats.Hash` is '''invariant''':
   * `Hash[HolidayCalendar]` is not a `Hash[ImmutableHolidayCalendar]`, so a caller holding a value
   * typed as this member - which every factory of this object returns - could not summon one from
   * the family's instance. The two declarations cannot be ambiguous with each other, a summon at
   * each type being answerable only by the instance declared at it.
   *
   * @return the hashing of calendars built from holiday dates
   */
  implicit val hash: Hash[ImmutableHolidayCalendar] =
    Hash.fromUniversalHashCode[ImmutableHolidayCalendar]

  /**
   * The rendering of calendars built from holiday dates as text.
   *
   * Renders what `toString` renders - `HolidayCalendar[GBLO]` - so the two ways of putting a
   * calendar into a message agree whether the value is typed as the member or as the family.
   * Declared here for the same reason the hashing above is: `cats.Show` is invariant as well.
   *
   * @return the rendering of a calendar built from holiday dates
   */
  implicit val show: Show[ImmutableHolidayCalendar] = Show.show(calendar => calendar.toString)

  /**
   * The JSON form of a calendar built from holiday dates, visible within this package only.
   *
   * This is the codec of the whole family narrowed to this one member, so a calendar written
   * through it is written exactly as it would be as a [[HolidayCalendar]] - as its name where it
   * is one of the built-in calendars, and structurally otherwise - and a document that describes
   * a calendar of another kind, such as the bare name `Sat/Sun`, is rejected rather than widened.
   *
   * Computed on first use, because it reads the codec of the family and the two objects must not
   * depend on the order in which they are initialised.
   *
   * The published codec of this family is [[HolidayCalendar]]'s, and this narrowing adds nothing
   * a caller cannot already do through it. It is `private[date]` rather than `private` so that
   * the rest of this package may summon it for a value typed as the member rather than as the
   * family; outside this package it is not summonable at all.
   *
   * @return the codec for calendars built from holiday dates
   */
  private[date] implicit lazy val codec: Codec[ImmutableHolidayCalendar] = {
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

  private val CombineSeparator: String = "+"

  /** The calendar for which every day is a business day. */
  val NO_HOLIDAYS: HolidayCalendar = NoHolidays

  /** The calendar whose only holidays are Saturday and Sunday. */
  val SAT_SUN: HolidayCalendar = SatSun

  /** The calendar whose only holidays are Friday and Saturday. */
  val FRI_SAT: HolidayCalendar = FriSat

  /** The calendar whose only holidays are Thursday and Friday. */
  val THU_FRI: HolidayCalendar = ThuFri

  /**
   * Obtains a calendar built into this library by name.
   *
   * Two or more names may be joined with `'+'`, so that `GBLO+USNY` is the calendar that observes
   * the holidays of London and New York; each part is looked up in turn and the parts are
   * combined. A name this library does not know is reported as a failure rather than raised,
   * because whether a name is known depends on the text a caller supplies.
   *
   * An application should generally not call this. A calendar is reference data, so code that
   * adjusts dates should name it with a [[HolidayCalendarId]] and resolve that identifier against
   * the [[com.opengamma.strata.basics.ReferenceData]] it was given, which lets the application
   * supply its own holidays. This method is the short cut for a demonstration, a test, or a
   * program that has decided to use the built-in data.
   *
   * The failure reports the name it was handed as that name stands, whole and unaltered, both in
   * its message `HolidayCalendar name not found: <name>` and in its `name` attribute. The wording
   * is the one every named family of this library produces, so a caller may match on it; and the
   * name arrives from outside this library, because `of` accepts whatever text a caller has, so
   * the caller correcting it needs the whole of what was refused rather than a shortened or
   * rewritten account of it. Nothing constrains that text, and bounding it and escaping what it
   * may hold belong to the writing of a failure rather than to the reporting of one.
   *
   * The property is per part of a composite name, because each part is resolved by this same
   * method and a part's failure is returned as it stands: `GBLO+NOSUCH` fails with the failure
   * raised for `NOSUCH`, naming that part alone and never the parts that resolved.
   *
   * @param uniqueName  the name of the calendar, optionally several joined with `'+'`
   * @return the calendar of that name, or the failure naming the part of the name that no
   *   built-in calendar carries
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
    HolidaySafeReferenceData(underlying)

  /**
   * Finds a calendar built into this library by name.
   *
   * Every built-in calendar is filed under two keys: the canonical name it carries, and the
   * English upper-case of that name. `GBLO`, `Sat/Sun` and `SAT/SUN` therefore all name a
   * calendar, and `NOHOLIDAYS`, `FRI/SAT` and `THU/FRI` name the three remaining constants of
   * this file. The match is exact within those two key spaces, so a name in any other case is
   * unknown here.
   *
   * The built-in set is read here rather than held in a field of this object, because the
   * calendars are assembled from generated data that is itself built from calendars, and reading
   * it from inside a method body is what keeps that loop safe. The lookup itself is by name and
   * generates only the calendar it finds.
   *
   * @param uniqueName  the name of the calendar
   * @return the calendar of that name, or empty where this library does not define one
   */
  private def lookup(uniqueName: String): Option[HolidayCalendar] =
    StandardHolidayCalendars.byRegisteredName(uniqueName)

  /**
   * Splits a combined name into its parts.
   *
   * Empty parts are kept, so a name such as `GBLO+` names an unknown calendar and is reported as
   * such rather than silently being the London calendar. Written as a tail-recursive function,
   * which also avoids treating the separator as a regular expression.
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
}
