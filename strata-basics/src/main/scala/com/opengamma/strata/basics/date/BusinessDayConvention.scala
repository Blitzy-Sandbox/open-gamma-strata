/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.DayOfWeek
import java.time.LocalDate

import cats.Hash
import cats.Order
import cats.Show
import cats.data.EitherNec
import cats.data.NonEmptyList

import io.circe.Codec

import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.Failure

/**
 * A convention defining how to adjust a date if it falls on a day other than a business day.
 *
 * The purpose of this convention is to define how to handle non-business days. When processing
 * dates in finance it is typically intended that non-business days, such as weekends and
 * holidays, are converted to a nearby valid business day. The convention, in conjunction with a
 * [[HolidayCalendar]], defines exactly how the adjustment should be made: the convention holds
 * the rule and the calendar holds the days, so the same convention adjusts a sterling date
 * against a London calendar and a dollar date against a New York one.
 *
 * A convention is pure: [[adjust]] is a function of the date and the calendar it is given and of
 * nothing else, so the same pair of arguments always produces the same date. Reference data is
 * never consulted here - the caller resolves a [[HolidayCalendarId]] into a calendar and passes
 * the result in, which is what makes the adjustment reproducible.
 *
 * ===A closed family===
 *
 * The family has exactly seven members and every one of them is declared in this file. The type
 * is `sealed`, the constructor is not visible outside this package, and the name lookup is built
 * from those seven members alone, so nothing can add an eighth. A `match` over a convention is
 * therefore exhaustive once the seven are covered.
 *
 * The seven are reached in three ways, all of which yield the same objects:
 *
 * {{{
 * BusinessDayConvention.ModifiedFollowing        // the member itself
 * BusinessDayConventions.MODIFIED_FOLLOWING      // the screaming-snake constant
 * BusinessDayConvention.parse("MODFOLLOWING")    // text, leniently resolved
 * }}}
 *
 * The name space of the family is fixed by this file. The seven canonical names, the two groups
 * of external spellings that the family publishes and the ordered list of lenient rewrite
 * patterns are all held as data in the companion and handed to the shared name lookup; nothing is
 * read from outside the program, so no configuration can change the family or its names. Text
 * reaches a convention through [[BusinessDayConvention.parse]], which reports text it cannot
 * resolve as a [[com.opengamma.strata.collect.result.Failure]] on the left of an `EitherNec`.
 *
 * Every member is immutable and safe to share between threads.
 *
 * @param name  the unique name of the convention, which is its identity in text and on the wire
 */
sealed abstract class BusinessDayConvention private[date] (val name: String)
    extends Named
    with NoJavaSerialization {

  // The closure of this family, run for every member as it is constructed: `sealed` and a
  // constructor private to the package are enforced against Scala and leave nothing in the class
  // file, so a subtype compiled by other means - which would be an eighth convention, outside the
  // seven this type publishes - is refused here instead.
  JvmClosure.requireDeclaredMember(this, classOf[BusinessDayConvention])

  /**
   * Adjusts the date as necessary if it is not a business day.
   *
   * If the date is a business day it is returned unaltered. If it is not, the rule of this
   * convention is applied against the calendar supplied. The result is always a date, and for
   * every convention other than [[BusinessDayConvention.NoAdjust]] it is always a business day of
   * the calendar supplied.
   *
   * @param date  the date to adjust
   * @param calendar  the calendar that defines holidays and business days
   * @return the adjusted date
   * @throws IllegalArgumentException where the calendar cannot answer for the date, which it
   *   rejects only for a year outside 0 to 9999
   */
  def adjust(date: LocalDate, calendar: HolidayCalendar): LocalDate

  /**
   * Renders this convention as its unique name.
   *
   * The name is the only text form of a convention: it is what the `Show` instance produces, what
   * the codec writes, and what [[BusinessDayConvention.parse]] reads back.
   *
   * @return the unique name
   */
  override def toString: String = name
}

/**
 * The seven business day conventions, together with their name lookup and typeclass instances.
 *
 * Each member is a `case object`, so it is a singleton whose identity is its own and whose
 * pattern match needs no extractor. [[values]] lists them in the order they are declared here.
 */
object BusinessDayConvention {

  /**
   * The day of the month that divides it in two for the bi-monthly convention.
   *
   * The first half of a month is the 1st to the 15th and the second half the 16th onwards, so a
   * date on or before this day may not be adjusted past it.
   */
  private val MidMonthDay: Int = 15

  /**
   * The 'NoAdjust' convention which makes no adjustment.
   *
   * The input date is returned even when it is not a business day, which makes this the identity
   * of the family and the convention to use where a date is already known to be acceptable. It is
   * the only member that can return a holiday, and the only one that never consults the calendar.
   */
  case object NoAdjust extends BusinessDayConvention("NoAdjust") {
    override def adjust(date: LocalDate, calendar: HolidayCalendar): LocalDate = date
  }

  /**
   * The 'Following' convention which adjusts to the next business day.
   *
   * If the input date is not a business day then the date is adjusted, and the adjusted date is
   * the next business day. The month is allowed to change, which is what distinguishes this from
   * [[ModifiedFollowing]].
   */
  case object Following extends BusinessDayConvention("Following") {
    override def adjust(date: LocalDate, calendar: HolidayCalendar): LocalDate =
      calendar.nextOrSame(date)
  }

  /**
   * The 'ModifiedFollowing' convention which adjusts to the next business day without crossing
   * month end.
   *
   * If the input date is not a business day then the date is adjusted. The adjusted date is the
   * next business day unless that day is in a different calendar month, in which case the
   * previous business day is returned.
   */
  case object ModifiedFollowing extends BusinessDayConvention("ModifiedFollowing") {
    override def adjust(date: LocalDate, calendar: HolidayCalendar): LocalDate =
      calendar.nextSameOrLastInMonth(date)
  }

  /**
   * The 'ModifiedFollowingBiMonthly' convention which adjusts to the next business day without
   * crossing mid-month or month end.
   *
   * If the input date is not a business day then the date is adjusted. The month is divided into
   * two parts, the 1st to the 15th and the 16th onwards. The adjusted date is the next business
   * day unless that day is in a different half-month, in which case the previous business day is
   * returned.
   *
   * The half-month test is deliberately not symmetric: crossing from the first half into the
   * second is a crossing, while a date in the second half is only ever held back by the month
   * boundary. The month itself is compared by its number.
   */
  case object ModifiedFollowingBiMonthly
      extends BusinessDayConvention("ModifiedFollowingBiMonthly") {

    override def adjust(date: LocalDate, calendar: HolidayCalendar): LocalDate = {
      val adjusted = calendar.nextOrSame(date)
      if (adjusted.getMonthValue != date.getMonthValue ||
        (adjusted.getDayOfMonth > MidMonthDay && date.getDayOfMonth <= MidMonthDay)) {
        calendar.previous(date)
      } else {
        adjusted
      }
    }
  }

  /**
   * The 'Preceding' convention which adjusts to the previous business day.
   *
   * If the input date is not a business day then the date is adjusted, and the adjusted date is
   * the previous business day. The month is allowed to change, which is what distinguishes this
   * from [[ModifiedPreceding]].
   */
  case object Preceding extends BusinessDayConvention("Preceding") {
    override def adjust(date: LocalDate, calendar: HolidayCalendar): LocalDate =
      calendar.previousOrSame(date)
  }

  /**
   * The 'ModifiedPreceding' convention which adjusts to the previous business day without
   * crossing month start.
   *
   * If the input date is not a business day then the date is adjusted. The adjusted date is the
   * previous business day unless that day is in a different calendar month, in which case the
   * next business day is returned.
   *
   * The month is compared by its month-of-year value rather than by its number. The two
   * comparisons differ only for dates a year or more apart, which no single adjustment can
   * produce.
   */
  case object ModifiedPreceding extends BusinessDayConvention("ModifiedPreceding") {
    override def adjust(date: LocalDate, calendar: HolidayCalendar): LocalDate = {
      val adjusted = calendar.previousOrSame(date)
      if (adjusted.getMonth != date.getMonth) calendar.next(date) else adjusted
    }
  }

  /**
   * The 'Nearest' convention which adjusts Sunday and Monday forward, and other days backward.
   *
   * If the input date is not a business day then the date is adjusted. If the input is a Sunday
   * or a Monday then the next business day is returned; otherwise the previous business day is
   * returned.
   *
   * Despite the name the result may not be the business day that is actually nearest: the choice
   * of direction is made from the day of the week of the input alone, before the calendar is
   * searched, so a Monday holiday in a week whose Tuesday is also a holiday adjusts forward past
   * both. Dates defined to fall on the nearest business day rely on that rule.
   */
  case object Nearest extends BusinessDayConvention("Nearest") {
    override def adjust(date: LocalDate, calendar: HolidayCalendar): LocalDate =
      if (calendar.isBusinessDay(date)) {
        date
      } else if (date.getDayOfWeek == DayOfWeek.SUNDAY || date.getDayOfWeek == DayOfWeek.MONDAY) {
        calendar.next(date)
      } else {
        calendar.previous(date)
      }
  }

  /**
   * The complete set of business day conventions, in declaration order.
   *
   * The order is the order the members are declared in above, which is also the order in which
   * they claim their lookup keys and the order a report over the family follows. It is not the
   * order the `Order` instance below imposes, which is alphabetical by name. The list is
   * non-empty by construction, which is what lets every operation over the family be written
   * without a case for a family that has no members.
   *
   * @return the seven conventions, in declaration order
   */
  val values: NonEmptyList[BusinessDayConvention] =
    NonEmptyList.of(
      NoAdjust,
      Following,
      ModifiedFollowing,
      ModifiedFollowingBiMonthly,
      Preceding,
      ModifiedPreceding,
      Nearest
    )

  /**
   * The five spellings this family publishes for the FpML protocol, each mapped to a canonical
   * name.
   *
   * The group takes part in no lookup - reading `MODFOLLOWING` as a convention is the business of
   * the lenient patterns below, which happen to accept it - and exists so that a caller writing
   * or reading that protocol can map between the two vocabularies explicitly, through
   * `NamedEnum.externalNames`.
   *
   * Note that `NONE` names the convention that makes no adjustment, which this library calls
   * `NoAdjust`; the two vocabularies disagree about that one word, which is exactly why the
   * mapping is held as data rather than derived from a rule over the names.
   */
  private val FpMLNames: Map[String, String] =
    Map(
      "NONE" -> "NoAdjust",
      "FOLLOWING" -> "Following",
      "MODFOLLOWING" -> "ModifiedFollowing",
      "PRECEDING" -> "Preceding",
      "NEAREST" -> "Nearest"
    )

  /**
   * The three spellings this family publishes for the SWIFT message standard, each mapped to a
   * canonical name.
   *
   * The group is smaller than the FpML one because the standard defines only three of the seven
   * conventions, and `MODIFIEDF` is a spelling no lenient pattern accepts, so this table is the
   * only route from it to a convention.
   */
  private val SwiftNames: Map[String, String] =
    Map(
      "FOLLOWING" -> "Following",
      "MODIFIEDF" -> "ModifiedFollowing",
      "PRECEDING" -> "Preceding"
    )

  /**
   * The eleven lenient rewrites of this family, in the order they are applied.
   *
   * The order is part of the data: [[parse]] folds its input to upper case and then applies every
   * pattern in turn, a pattern whose expression matches the whole of the current text replacing
   * that text before the next pattern is tried, so a later pattern sees what an earlier one
   * produced. Reordering these rows would change which text resolves and to what.
   *
   * The chain is what lets abbreviations, the screaming-snake spellings of the constant
   * identifiers, and the spaced and hyphen-free spellings of a name all reach the same member:
   *
   * {{{
   * parse("MF")                 // ModifiedFollowing - by abbreviation
   * parse("MODIFIED_FOLLOWING") // ModifiedFollowing - by constant identifier
   * parse("Mod Follow")         // ModifiedFollowing - by shortened spelling
   * parse("MP")                 // ModifiedPreceding - rewritten twice, to the same text
   * }}}
   *
   * Each expression is matched insensitively to case by the name lookup, which is why the rows
   * are written here in mixed case rather than folded by hand. They are held as the text of each
   * expression: the name lookup prepares each expression once, insensitively to case, and only
   * when this family first parses a name.
   */
  private val LenientSources: List[(String, String)] =
    List(
      "F" -> "Following",
      "Follow" -> "Following",
      "M" -> "ModifiedFollowing",
      "MF" -> "ModifiedFollowing",
      "Mod(ified)?[_ ]?(Follow(ing)?)?" -> "ModifiedFollowing",
      "P" -> "Preceding",
      "MP" -> "ModifiedPreceding",
      "Mod(ified)?[_ ]?Preceding" -> "ModifiedPreceding",
      "Mod(ified)?[_ ]?(Follow(ing)?)?[_ ]?Bi[_ ]?Monthly" -> "ModifiedFollowingBiMonthly",
      "None" -> "NoAdjust",
      "NO_ADJUST" -> "NoAdjust"
    )

  /**
   * The name lookup for this family.
   *
   * This instance is the single route from text to a convention, and it is built from [[values]]
   * and the three tables above alone. The family declares no alternate spelling: every spelling
   * other than the seven canonical names is reached through the lenient patterns, and the two
   * external groups are published rather than looked up.
   *
   * The instance also carries the tables themselves - `lenientSources`, `externalNamesRaw` and
   * `alternateNames` - which is how a caller reads that data back without this object having to
   * publish it twice.
   *
   * @return the name lookup for the seven conventions
   */
  implicit val namedEnum: NamedEnum[BusinessDayConvention] =
    NamedEnum.ofSources(
      values,
      Map.empty,
      LenientSources,
      Map("FpML" -> FpMLNames, "SWIFT" -> SwiftNames),
      "BusinessDayConvention")

  /**
   * Obtains the convention with the specified canonical name, if one exists.
   *
   * The match is exact against the canonical names and against those names folded to upper case,
   * so `ModifiedFollowing` and `MODIFIEDFOLLOWING` resolve while `modifiedfollowing` does not. No
   * lenient pattern is applied. Use [[parse]] to accept text whose shape is not known in advance.
   *
   * @param name  the name to look up
   * @return the convention with that name, or `None` when no convention has it
   */
  def valueOf(name: String): Option[BusinessDayConvention] = namedEnum.valueOf(name)

  /**
   * Parses a convention from text, applying the leniency this family declares.
   *
   * The exact lookup of [[valueOf]] is tried first. Failing that, the text is folded to upper case
   * and the lenient patterns are applied in order before the exact lookup is tried once more, so
   * an abbreviation, a screaming-snake identifier or a spaced spelling all resolve:
   *
   * {{{
   * parse("Following")           // Right(Following) - the canonical name
   * parse("following")           // Right(Following) - folded to upper case
   * parse("F")                   // Right(Following) - by abbreviation
   * parse("MODFOLLOWINGBIMONTHLY") // Right(ModifiedFollowingBiMonthly)
   * parse("Rubbish")             // Left - text this family has never accepted
   * }}}
   *
   * Text that no canonical name and no lenient rewrite reaches is reported as a value on the left
   * of the result rather than raised, so a caller parsing text it did not write handles the
   * outcome in the same way it handles a convention.
   *
   * @param name  the text to parse
   * @return the convention the text names, or the failure reporting that the text matches none of
   *   the seven canonical names, neither as written, nor folded to upper case, nor after the
   *   lenient rewrites have been applied in order
   */
  def parse(name: String): EitherNec[Failure, BusinessDayConvention] = namedEnum.parse(name)

  /**
   * The ordering and hashing of conventions.
   *
   * This is the only equality-bearing instance of the type: `Order` extends `Eq` and `Hash`
   * extends `Eq`, so summoning any of the three yields this one value and the three can never
   * disagree. Comparison is over `name`, which makes the ordering alphabetical rather than the
   * declaration order of [[values]], and equality follows it - the seven names are distinct, so
   * two conventions compare equal if, and only if, they are the same convention.
   *
   * @return the ordering of conventions by name, which is also their hashing
   */
  implicit val order: Order[BusinessDayConvention] with Hash[BusinessDayConvention] =
    NamedEnum.orderByName

  /**
   * The rendering of conventions as text.
   *
   * A convention renders as its canonical name, which is what `toString` produces as well, so the
   * two ways of putting a convention into a message agree.
   *
   * @return the rendering of a convention as its canonical name
   */
  implicit val show: Show[BusinessDayConvention] = NamedEnum.showByName

  /**
   * The JSON codec for conventions.
   *
   * A convention is written as the bare string of its canonical name - `"ModifiedFollowing"` -
   * and never as an object. Decoding goes through [[parse]], so a document is read with exactly
   * the leniency that method applies and unresolvable text is reported as a decoding failure
   * rather than raised.
   *
   * @return the codec reading and writing a convention as its canonical name
   */
  implicit val codec: Codec[BusinessDayConvention] = Codecs.namedEnumCodec
}

/**
 * Constants for the standard business day conventions, published under their screaming-snake
 * identifiers.
 *
 * The purpose of each convention is to define how to handle non-business days. When processing
 * dates in finance it is typically intended that non-business days, such as weekends and
 * holidays, are converted to a nearby valid business day. The convention, in conjunction with a
 * [[HolidayCalendar]], defines exactly how the adjustment should be made.
 *
 * Every constant here is one of the seven members of [[BusinessDayConvention]] under a second
 * name, and each names its member directly. The values are therefore the same objects as the
 * members of the companion, so a constant taken from here and the matching member are
 * indistinguishable - including by `eq`, by `==` and in a pattern match.
 */
object BusinessDayConventions {

  /**
   * The 'NoAdjust' convention which makes no adjustment.
   *
   * The input date will not be adjusted even if it is not a business day.
   */
  val NO_ADJUST: BusinessDayConvention = BusinessDayConvention.NoAdjust

  /**
   * The 'Following' convention which adjusts to the next business day.
   *
   * If the input date is not a business day then the date is adjusted. The adjusted date is the
   * next business day.
   */
  val FOLLOWING: BusinessDayConvention = BusinessDayConvention.Following

  /**
   * The 'ModifiedFollowing' convention which adjusts to the next business day without crossing
   * month end.
   *
   * If the input date is not a business day then the date is adjusted. The adjusted date is the
   * next business day unless that day is in a different calendar month, in which case the
   * previous business day is returned.
   */
  val MODIFIED_FOLLOWING: BusinessDayConvention = BusinessDayConvention.ModifiedFollowing

  /**
   * The 'ModifiedFollowingBiMonthly' convention which adjusts to the next business day without
   * crossing mid-month or month end.
   *
   * If the input date is not a business day then the date is adjusted. The month is divided into
   * two parts, the first half, the 1st to 15th and the 16th onwards. The adjusted date is the
   * next business day unless that day is in a different half-month, in which case the previous
   * business day is returned.
   */
  val MODIFIED_FOLLOWING_BI_MONTHLY: BusinessDayConvention =
    BusinessDayConvention.ModifiedFollowingBiMonthly

  /**
   * The 'Preceding' convention which adjusts to the previous business day.
   *
   * If the input date is not a business day then the date is adjusted. The adjusted date is the
   * previous business day.
   */
  val PRECEDING: BusinessDayConvention = BusinessDayConvention.Preceding

  /**
   * The 'ModifiedPreceding' convention which adjusts to the previous business day without
   * crossing month start.
   *
   * If the input date is not a business day then the date is adjusted. The adjusted date is the
   * previous business day unless that day is in a different calendar month, in which case the
   * next business day is returned.
   */
  val MODIFIED_PRECEDING: BusinessDayConvention = BusinessDayConvention.ModifiedPreceding

  /**
   * The 'Nearest' convention which adjusts Sunday and Monday forward, and other days backward.
   *
   * If the input date is not a business day then the date is adjusted. If the input is Sunday or
   * Monday then the next business day is returned. Otherwise the previous business day is
   * returned.
   *
   * Note that despite the name, the algorithm may not return the business day that is actually
   * nearest.
   */
  val NEAREST: BusinessDayConvention = BusinessDayConvention.Nearest
}
