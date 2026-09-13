/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate
import java.time.Period

import cats.Hash
import cats.Order
import cats.Show
import cats.data.EitherNec
import cats.data.NonEmptyList

import _root_.io.circe.Codec

import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.Failure

/**
 * A convention defining how a period is added to a date.
 *
 * Adding three months to a date looks like plain calendar arithmetic until the date is the
 * last day of its month, and then it stops being obvious. Adding one month to the 30th of
 * June gives the 30th of July by arithmetic, yet the 30th of June was a month end and the
 * 30th of July is not, so a schedule built that way loses the month-end alignment that the
 * instrument was quoted on. This type is the decision of what to do about that: it is applied
 * wherever a period is added to a date in this library, and it says whether the end date is
 * taken as arithmetic produced it, or is pulled out to the end of its month, or is pulled out
 * to the last working day of its month.
 *
 * ===The three conventions===
 *
 * Each convention first adds the period using ordinary date arithmetic, then applies its own
 * rule to the result. Ordinary arithmetic already shortens a day-of-month that the target
 * month does not have - the 31st of January plus one month is the 28th of February - so the
 * conventions differ from each other only when the base date is the last day, or the last
 * working day, of its own month:
 *
 * {{{
 * // 30 June 2014 was a Monday and the last business day of June; 31 August 2014 was a Sunday
 * PeriodAdditionConventions.NONE
 *   .adjust(LocalDate.of(2014, 6, 30), Period.ofMonths(2), HolidayCalendars.SAT_SUN)  // 2014-08-30
 * PeriodAdditionConventions.LAST_DAY
 *   .adjust(LocalDate.of(2014, 6, 30), Period.ofMonths(2), HolidayCalendars.SAT_SUN)  // 2014-08-31
 * PeriodAdditionConventions.LAST_BUSINESS_DAY
 *   .adjust(LocalDate.of(2014, 6, 30), Period.ofMonths(2), HolidayCalendars.SAT_SUN)  // 2014-08-29
 *
 * // 11 July 2014 was neither, so all three conventions agree
 * PeriodAdditionConventions.LAST_BUSINESS_DAY
 *   .adjust(LocalDate.of(2014, 7, 11), Period.ofMonths(1), HolidayCalendars.SAT_SUN)  // 2014-08-11
 * }}}
 *
 * A holiday calendar is passed to every convention even though only the last of the three
 * consults it, because a caller chooses the convention from data and cannot know in advance
 * which one it will hold. The argument the first two conventions leave unread is therefore
 * deliberate.
 *
 * ===Month-based periods===
 *
 * Both end-of-month rules are meaningful only for a period measured in months and years: the
 * end of a month has no bearing on a period of eleven days. [[PeriodAdditionConvention]]
 * therefore reports, through `isMonthBased`, whether it requires such a period, and the
 * adjustment types that carry a convention - [[PeriodAdjustment]] and [[TenorAdjustment]] -
 * check that flag against the period they were given when they are built, so an inconsistent
 * pairing is rejected at construction rather than silently producing a date nobody expected.
 *
 * ===A closed family===
 *
 * The three conventions declared in the companion of this class are the whole family. The
 * class is `sealed`, its constructor is visible only inside this package, and each convention
 * exists exactly once as a value in the companion, so no further convention can come into
 * being - not by subclassing from another file, and not by introducing one while the program
 * runs. The members are fixed in this file, so a match over the family is exhaustive once it
 * covers the three, and text that names no member is rejected by `parse` as a value.
 *
 * ===Names, and the identifiers that carry them===
 *
 * The name of a convention - `None`, `LastDay`, `LastBusinessDay` - is its identity in text
 * and in JSON. The first member is reached through the identifier `NONE` rather than `None`,
 * because `None` is already the empty `Option` of the standard library and a member of that
 * name in this companion would shadow it for every file that imported it. The name string,
 * not the identifier, is the contract here: `PeriodAdditionConventions.NONE.name` is
 * `"None"`.
 *
 * ===Failure===
 *
 * Adding a period cannot fail on the data it is given, so `adjust` is total in its signature.
 * The one error it can propagate belongs to the holiday calendar rather than to this type:
 * asking a calendar about a date whose year lies outside 0 to 9999 is a broken precondition of
 * the call, and the calendar raises `IllegalArgumentException` for it. Resolving a name is the
 * other direction - text arrives as data, so `parse` reports an unrecognised name as a failure
 * value.
 *
 * ===Thread safety===
 *
 * Every convention is an immutable object holding nothing but its name, and `adjust` is a pure
 * function of its arguments, so the members and this type are safe to share without
 * synchronization.
 *
 * @param name  the unique name of the convention, as it appears in text and in JSON
 */
sealed abstract class PeriodAdditionConvention private[date] (val name: String)
    extends Named
    with NoJavaSerialization {

  // The closure of this family, run for every member as it is constructed: `sealed` and a
  // constructor private to the package are enforced against Scala and leave nothing in the class
  // file, so a subtype compiled by other means - which would be a fourth convention, outside the
  // three this type publishes - is refused here instead.
  JvmClosure.requireDeclaredMember(this, classOf[PeriodAdditionConvention])

  /**
   * Returns the name of this convention, which is how a convention renders as text.
   *
   * This is the representation `parse` reads back, so a rendered name resolves to the
   * convention it was rendered from.
   *
   * @return the unique name of this convention
   */
  override def toString: String = name

  /**
   * Adjusts the base date, adding the period and applying the convention rule.
   *
   * The adjustment occurs in two steps. First, the period is added to the base date using
   * ordinary date arithmetic to create the end date. Second, that end date is adjusted by the
   * rule of this convention, which may move it to the end of its month or to the last business
   * day of its month, and which may leave it exactly as arithmetic produced it.
   *
   * The result is the unadjusted end date of the period: a business day adjustment, where the
   * caller has one, is applied afterwards and separately.
   *
   * @param baseDate  the base date to add to
   * @param period  the period to add
   * @param calendar  the holiday calendar to use, consulted only by the conventions whose rule
   *   depends on which days are business days
   * @return the adjusted date
   * @throws IllegalArgumentException where the rule of this convention consults the calendar
   *   about a date whose year lies outside 0 to 9999
   */
  def adjust(baseDate: LocalDate, period: Period, calendar: HolidayCalendar): LocalDate

  /**
   * Checks whether the convention requires a month-based period.
   *
   * A month-based period contains only months and/or years, and not days. The two
   * end-of-month conventions require one, because their rule is expressed in terms of the
   * month the date falls in; the convention that adds the period unchanged does not.
   *
   * @return true if the convention requires a month-based period
   */
  def isMonthBased: Boolean
}

/**
 * The three period addition conventions, and the lookup from a name to one of them.
 *
 * The members are declared here and nowhere else, which is what closes the family. Each is
 * published again by [[PeriodAdditionConventions]], so a call site may reach a convention
 * through either object and obtain the same value.
 */
object PeriodAdditionConvention {

  /**
   * No specific rule applies.
   *
   * Given a date, the specified period is added using standard date arithmetic. The business
   * day adjustment is applied to produce the final result.
   *
   * For example, adding a period of 1 month to June 30th will result in July 30th.
   *
   * This is the member named `None`. It is reached as `NONE` because the identifier `None`
   * belongs to the empty `Option` of the standard library, as explained on
   * [[PeriodAdditionConvention]].
   */
  case object NONE extends PeriodAdditionConvention("None") {

    override def adjust(baseDate: LocalDate, period: Period, calendar: HolidayCalendar): LocalDate =
      baseDate.plus(period)

    override def isMonthBased: Boolean = false
  }

  /**
   * Convention applying a last day of month rule, ''ignoring business days''.
   *
   * Given a date, the specified period is added using standard date arithmetic, shifting to
   * the end-of-month if the base date is the last day of the month. The business day
   * adjustment is applied to produce the final result. Note that this rule is based on the
   * last day of the month, not the last business day of the month.
   *
   * For example, adding a period of 1 month to June 30th will result in July 31st.
   */
  case object LAST_DAY extends PeriodAdditionConvention("LastDay") {

    override def adjust(baseDate: LocalDate, period: Period, calendar: HolidayCalendar): LocalDate = {
      val endDate = baseDate.plus(period)
      if (baseDate.getDayOfMonth == baseDate.lengthOfMonth) {
        endDate.withDayOfMonth(endDate.lengthOfMonth)
      } else {
        endDate
      }
    }

    override def isMonthBased: Boolean = true
  }

  /**
   * Convention applying a last ''business'' day of month rule.
   *
   * Given a date, the specified period is added using standard date arithmetic, shifting to
   * the last business day of the month if the base date is the last business day of the month.
   * The business day adjustment is applied to produce the final result.
   *
   * For example, adding a period of 1 month to June 29th results in July 31st where June
   * 30th is not a valid business day and July 31st is.
   *
   * This is the one convention that consults the calendar it is given, on both sides of its
   * rule: the calendar decides whether the base date is the last business day of its month,
   * and the calendar produces the last business day of the end date's month.
   */
  case object LAST_BUSINESS_DAY extends PeriodAdditionConvention("LastBusinessDay") {

    override def adjust(baseDate: LocalDate, period: Period, calendar: HolidayCalendar): LocalDate = {
      val endDate = baseDate.plus(period)
      if (calendar.isLastBusinessDayOfMonth(baseDate)) {
        calendar.lastBusinessDayOfMonth(endDate)
      } else {
        endDate
      }
    }

    override def isMonthBased: Boolean = true
  }

  /**
   * The complete set of period addition conventions, in declaration order.
   *
   * The order is the order the members are declared above, which is the order a report over
   * the family follows. It is not the order the `Order` instance below imposes, which is
   * alphabetical by name. The list is non-empty by construction, which is what lets every
   * operation over the family be written without a case for a family that has no members.
   *
   * @return the three conventions, in declaration order
   */
  val values: NonEmptyList[PeriodAdditionConvention] =
    NonEmptyList.of(
      NONE,
      LAST_DAY,
      LAST_BUSINESS_DAY
    )

  /**
   * The lenient rewrites of the family, in the order they are applied.
   *
   * These three rows are the whole of the leniency this family declares. Each expression is
   * matched against the whole of the text, after the text has been folded to upper case, and
   * the row that matches rewrites it to a canonical name that is then looked up. They exist
   * so that the identifier of a constant - `NONE`, `LAST_DAY`, `LAST_BUSINESS_DAY` - is
   * accepted wherever the canonical name is and, because the fold to upper case happens
   * first, they accept every spelling of those identifiers whatever its case.
   *
   * The rows are disjoint in practice: an expression matches only the whole of the text, so
   * `LAST_DAY` cannot claim `LAST_BUSINESS_DAY`, and a replacement produced by one row matches
   * none of the rows after it. The chain therefore performs at most one rewrite, whichever
   * order a future row is added in.
   *
   * The rows carry the source text of each expression rather than a built expression, and are
   * handed to the name lookup in that form, which builds each of them once - insensitively to
   * case, and only when this family first parses a name.
   */
  private val LenientSources: List[(String, String)] =
    List(
      "NONE" -> "None",
      "LAST_DAY" -> "LastDay",
      "LAST_BUSINESS_DAY" -> "LastBusinessDay"
    )

  /**
   * The name lookup for this family.
   *
   * This instance is the single route from text to a convention, and it is built from `values`
   * and `LenientSources` alone. Of the three tables a named family may declare, this family
   * declares only the lenient rewrites: it names no alternate spelling of any convention and
   * no group of names published for an external protocol. The whole name space of the family
   * is therefore its three canonical names, those names folded to upper case, and whatever the
   * three rewrites reach.
   *
   * @return the name lookup for the three conventions
   */
  implicit val namedEnum: NamedEnum[PeriodAdditionConvention] =
    NamedEnum.ofSources(
      values = values,
      alternates = Map.empty,
      lenient = LenientSources,
      externals = Map.empty,
      familyName = "PeriodAdditionConvention")

  /**
   * Obtains the convention with the specified canonical name, if one exists.
   *
   * The match is exact: the canonical names as they are declared, and those names folded to
   * upper case. No lenient rewrite is applied, so the identifier of a constant does not
   * resolve here even though `parse` accepts it:
   *
   * {{{
   * valueOf("LastDay")   // Some(LAST_DAY) - the canonical name
   * valueOf("LASTDAY")   // Some(LAST_DAY) - the canonical name in upper case
   * valueOf("LAST_DAY")  // None - the constant identifier, which only parse accepts
   * }}}
   *
   * @param name  the name to look up
   * @return the convention with that name, or `None` when no convention has it
   */
  def valueOf(name: String): Option[PeriodAdditionConvention] = namedEnum.valueOf(name)

  /**
   * Parses a convention from text, applying the leniency this family declares.
   *
   * The exact lookup of `valueOf` is tried first. When that finds nothing, the text is folded
   * to upper case, the three rewrites above are applied in order, and the exact lookup is tried
   * once more on the result. That is what makes both the canonical name of a convention and the
   * identifier of the matching constant resolve, in any case:
   *
   * {{{
   * parse("LastBusinessDay")     // Right(LAST_BUSINESS_DAY) - the canonical name
   * parse("lastbusinessday")     // Right(LAST_BUSINESS_DAY) - folded to upper case
   * parse("LAST_BUSINESS_DAY")   // Right(LAST_BUSINESS_DAY) - through a lenient rewrite
   * parse("last_business_day")   // Right(LAST_BUSINESS_DAY) - folded, then rewritten
   * parse("Last Business Day")   // Left - never a name of this family
   * }}}
   *
   * Text that neither the exact lookup nor a rewrite resolves is reported as a value rather
   * than raised: the result is `Left` of a chain holding one parsing [[Failure]], which names
   * both this family and the text that could not be resolved.
   *
   * @param name  the text to parse
   * @return the convention the text names, or the failure naming this family and text that
   *   matches none of its three canonical names, upper-cased or rewritten
   */
  def parse(name: String): EitherNec[Failure, PeriodAdditionConvention] = namedEnum.parse(name)

  /**
   * The ordering and hashing of conventions.
   *
   * This is the only equality-bearing instance of the type: `Order` extends `Eq` and `Hash`
   * extends `Eq`, so summoning any of the three yields this one value and the three can never
   * disagree. Comparison is over `name`, which makes the ordering alphabetical rather than the
   * declaration order of `values`, and equality follows it - names are unique across the
   * family, so two conventions compare equal if, and only if, they are the same convention.
   *
   * @return the ordering of conventions by name, which is also their hashing
   */
  implicit val order: Order[PeriodAdditionConvention] with Hash[PeriodAdditionConvention] =
    NamedEnum.orderByName

  /**
   * The rendering of conventions as text.
   *
   * A convention renders as its canonical name, which is what `toString` produces as well, so
   * the two ways of putting a convention into a message agree.
   *
   * @return the rendering of a convention as its canonical name
   */
  implicit val show: Show[PeriodAdditionConvention] = NamedEnum.showByName

  /**
   * The JSON codec for conventions.
   *
   * A convention is written as the bare string of its canonical name - `"LastBusinessDay"` -
   * and never as an object. Being a `Codec`, this single instance serves as the encoder and as
   * the decoder, so the two halves cannot drift apart. Decoding goes through `parse`, so a
   * document is accepted whatever the case of the name it holds and whichever of the family's
   * spellings it uses, and text resolving to no convention is reported as a decoding failure
   * rather than raised. The codec is built from the `namedEnum` instance above and inspects no
   * type while the program runs.
   *
   * @return the codec reading and writing a convention as its canonical name
   */
  implicit val codec: Codec[PeriodAdditionConvention] = Codecs.namedEnumCodec
}

/**
 * Constants and implementations for standard period addition conventions.
 *
 * The purpose of each convention is to define how to handle the addition of a period. The
 * default implementations include two different end-of-month rules. The convention is generally
 * only applicable for month-based periods.
 *
 * Every constant here is one of the members of [[PeriodAdditionConvention]], published under
 * the identifier a call site reads it by - `PeriodAdditionConventions.LAST_DAY`. The values are
 * the same objects, so a constant taken from here and the matching member of the companion are
 * indistinguishable.
 */
object PeriodAdditionConventions {

  /**
   * No specific rule applies.
   *
   * Given a date, the specified period is added using standard date arithmetic. The business
   * day adjustment is applied to produce the final result.
   *
   * For example, adding a period of 1 month to June 30th will result in July 30th.
   */
  val NONE: PeriodAdditionConvention = PeriodAdditionConvention.NONE

  /**
   * Convention applying a last day of month rule, ''ignoring business days''.
   *
   * Given a date, the specified period is added using standard date arithmetic, shifting to
   * the end-of-month if the base date is the last day of the month. The business day
   * adjustment is applied to produce the final result. Note that this rule is based on the
   * last day of the month, not the last business day of the month.
   *
   * For example, adding a period of 1 month to June 30th will result in July 31st.
   */
  val LAST_DAY: PeriodAdditionConvention = PeriodAdditionConvention.LAST_DAY

  /**
   * Convention applying a last ''business'' day of month rule.
   *
   * Given a date, the specified period is added using standard date arithmetic, shifting to
   * the last business day of the month if the base date is the last business day of the month.
   * The business day adjustment is applied to produce the final result.
   *
   * For example, adding a period of 1 month to June 29th results in July 31st where June
   * 30th is not a valid business day and July 31st is.
   */
  val LAST_BUSINESS_DAY: PeriodAdditionConvention = PeriodAdditionConvention.LAST_BUSINESS_DAY
}
