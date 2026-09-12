/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate

import cats.Hash
import cats.Show

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.Resolvable
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * An adjustment that alters a date by adding a period of days.
 *
 * When processing dates in finance, the rules for adjusting a date by a number of days can be
 * complex. This type represents those rules, which always operate in two steps - addition
 * followed by adjustment - and it is the pairing of those two steps that makes it more than a
 * day count: the days added and the days the result must fall on need not be counted against the
 * same calendar.
 *
 * ===Approach 1 - calendar days addition===
 *
 * This approach is triggered by the [[DaysAdjustment.ofCalendarDays]] factories. When adding a
 * number of days to a date the addition is simple, no holidays or weekends apply. For example,
 * two days after Friday 15th August would be Sunday 17th, even though this is typically a
 * weekend.
 *
 * In step one, the number of days is added without skipping any dates. In step two, the result of
 * step one is optionally adjusted to be a business day using a [[BusinessDayAdjustment]].
 *
 * ===Approach 2 - business days addition===
 *
 * With this approach the days to be added are treated as business days. For example, two days
 * after Friday 15th August would be Tuesday 19th, assuming a Saturday/Sunday weekend and no
 * other applicable holidays.
 *
 * This approach is triggered by the [[DaysAdjustment.ofBusinessDays]] factories. The distinction
 * between business days, holidays and weekends is made using the holiday calendar this adjustment
 * names. In step one, the number of days is added using [[HolidayCalendar.shift]]. In step two,
 * the result of step one is optionally adjusted to be a business day using a
 * [[BusinessDayAdjustment]].
 *
 * At first glance, step two may seem pointless, as the result of step one will always be a valid
 * business day. However, the step two adjustment allows the possibility of applying a '''different
 * holiday calendar'''. For example, a rule might have two parts: "first add 2 London business
 * days, and then adjust the result to be a valid New York business day using the
 * 'ModifiedFollowing' convention". Note that the holiday calendar differs in the two parts of the
 * rule, which is the whole reason both fields exist:
 *
 * {{{
 * val rule = DaysAdjustment.ofBusinessDays(
 *   2,
 *   HolidayCalendarIds.GBLO,
 *   BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.USNY))
 * val settlement = rule.adjust(tradeDate, ReferenceData.standard)
 * }}}
 *
 * ===Which field does the addition===
 *
 * The [[calendar]] field decides which of the two approaches applies, and it does so by its
 * value rather than by a flag: where it is the no-holidays identifier, addition is simple date
 * arithmetic; where it is anything else, addition walks that calendar's business days. There is
 * therefore no such thing as a business-day addition of zero days - asking for one names no day
 * at all - and [[DaysAdjustment.ofBusinessDays]] handles that case specially, as described there.
 *
 * ===Reference data is supplied, not looked up===
 *
 * Both calendars are held as identifiers rather than as calendars, which is what lets an
 * adjustment be written down, stored and passed around by code that holds no holiday data. The
 * data is supplied at the moment the adjustment is applied, so
 * [[com.opengamma.strata.basics.ReferenceData]] is a parameter of both [[adjust]] and [[resolve]]
 * and nothing is read from ambient state.
 *
 * The two forms differ only in when the lookup happens. [[adjust]] resolves and adjusts one date;
 * [[resolve]] resolves both calendars once and returns a [[DateAdjuster]] holding them, so a run
 * of dates costs one resolution rather than one per date. A schedule of dates should therefore
 * resolve once - the difference matters most where a calendar is composite, such as `GBLO+USNY`,
 * because resolving such an identifier assembles it from its parts every time. `toReader`, from
 * [[com.opengamma.strata.basics.Resolvable]], is the same resolution expressed as a value
 * awaiting its data, for composing several of these before any data is available.
 *
 * A resolved adjuster is bound to the calendars it was resolved against and does not follow later
 * changes to the reference data, which is the caveat `Resolvable` documents for every resolved
 * form.
 *
 * ===Failure is returned, not thrown===
 *
 * The Java original returned a bare date and threw `ReferenceDataNotFoundException` where a
 * calendar was absent from the reference data. Here both methods answer with
 * `Either[Failure, _]`, reporting the `Failure.MissingData` that
 * [[HolidayCalendarId.resolve]] produces, naming the identifier that could not be found. Nothing
 * else about applying an adjustment can fail: once the calendars are in hand, every convention
 * answers for every date they answer for.
 *
 * ===Construction===
 *
 * Construction is total: every combination of a day count, a calendar identifier and a business
 * day adjustment describes an adjustment, which is exactly what the type being ported held - its
 * factories validated nothing beyond non-nullness, and the types here state that on their own. No
 * failure mode is invented for the sake of the shape, so the factories return a `DaysAdjustment`
 * rather than an `Either` of one, and a caller assembling an adjustment from fields it already
 * holds has no failure to handle.
 *
 * What the factories do have is meaning that a raw constructor would lose - which calendar
 * performs the addition, and the zero-day case of [[DaysAdjustment.ofBusinessDays]] - so they are
 * the only way to build one: the constructor of this `sealed abstract case class` is private and
 * neither `apply` nor `copy` exists. Pattern matching and `unapply` are unaffected, and a
 * modified instance is obtained by naming the change through a factory.
 *
 * This type is immutable and thread-safe.
 *
 * @param days  the number of days to be added, which may be negative, and which is added using
 *   the calendar below to determine the addition type
 * @param calendar  the identifier of the holiday calendar that defines the meaning of a day when
 *   performing the addition; where it is `NoHolidays` the addition is simple date arithmetic,
 *   and where it is anything else the addition effectively repeatedly finds the next business day
 * @param adjustment  the business day adjustment applied to the result of the addition, which is
 *   expected to name a different calendar where the addition itself used one
 * @see [[BusinessDayAdjustment]] for the second step of the calculation
 * @see [[PeriodAdditionConvention]] for the corresponding rules when adding months or years
 */
sealed abstract case class DaysAdjustment private (
    days: Int,
    calendar: HolidayCalendarId,
    adjustment: BusinessDayAdjustment)
    extends Resolvable[DateAdjuster] {

  /**
   * Adjusts the date, adding the period in days using the holiday calendar and then applying the
   * business day adjustment.
   *
   * The calculation is performed in two steps. Step one uses [[HolidayCalendar.shift]] to add the
   * number of days; where the holiday calendar is `NoHolidays` this effectively adds calendar
   * days. Step two uses [[BusinessDayAdjustment.adjust]] to adjust the result of step one.
   *
   * Both calendars are resolved from the reference data supplied, the addition calendar first.
   * Where a caller has many dates to adjust, [[resolve]] performs those lookups once instead of
   * once per date.
   *
   * @param date  the date to adjust
   * @param refData  the reference data, used to find the holiday calendars
   * @return the adjusted date, or `Left(Failure.MissingData)` where the reference data does not
   *   supply one of the calendars this adjustment names
   */
  def adjust(date: LocalDate, refData: ReferenceData): Either[Failure, LocalDate] =
    for {
      additionCalendar <- calendar.resolve(refData)
      adjusted <- adjustment.adjust(additionCalendar.shift(date, days), refData)
    } yield adjusted

  /**
   * Resolves this adjustment using the specified reference data, returning an adjuster.
   *
   * This returns a [[DateAdjuster]] that performs the same calculation as this adjustment. The
   * calendars are looked up from the reference data once, here, and bound into the result, so the
   * adjuster returned performs no further lookup however many dates are put through it and there
   * is no need to supply the reference data again.
   *
   * The adjuster is effectively [[normalized]]: where the addition calendar is `NoHolidays` the
   * addition is performed as plain date arithmetic rather than by asking a calendar that has no
   * holidays to shift a date, which is the same answer computed without the indirection. That
   * shortcut is also why an adjustment adding calendar days resolves against reference data that
   * holds no `NoHolidays` entry, where [[adjust]] on the same adjustment would report it missing;
   * both behaviours are those of the type being ported, and the reference data this library
   * builds - [[com.opengamma.strata.basics.ReferenceData.standard]], `minimal`, and anything from
   * `ReferenceData.of` - always holds that entry, so the two paths answer alike for it.
   *
   * The adjuster is bound to the calendars as they stood at this moment and will not follow later
   * changes to the reference data, so care is needed when placing one in a cache or a persistence
   * layer. The unresolved adjustment has no such caveat, which is why both forms exist.
   *
   * @param refData  the reference data, used to find the holiday calendars
   * @return the adjuster bound to specific holiday calendars, or `Left(Failure.MissingData)`
   *   where the reference data does not supply one of the calendars this adjustment names
   */
  override def resolve(refData: ReferenceData): Either[Failure, DateAdjuster] = {
    val adjustmentConvention = adjustment.convention
    adjustment.calendar.resolve(refData).flatMap { adjustmentCalendar =>
      if (calendar == HolidayCalendarIds.NO_HOLIDAYS) {
        // Resolution happens here, outside the adjuster, so that the adjuster returned holds the
        // calendar rather than the reference data and performs no lookup of its own.
        Right(
          DateAdjuster(date =>
            adjustmentConvention.adjust(LocalDateUtils.plusDays(date, days), adjustmentCalendar)))
      } else {
        calendar.resolve(refData).map { additionCalendar =>
          DateAdjuster(date =>
            adjustmentConvention.adjust(additionCalendar.shift(date, days), adjustmentCalendar))
        }
      }
    }
  }

  /**
   * Gets the holiday calendar that will be applied to the result.
   *
   * This adjustment may contain more than one holiday calendar. This method returns the calendar
   * used last, which is the adjustment's own calendar unless that names no holidays, in which
   * case the addition calendar had the last word. The adjusted date is always a valid business
   * day according to the calendar returned.
   *
   * @return the identifier of the result holiday calendar
   */
  def resultCalendar: HolidayCalendarId = {
    val adjustmentCalendar = adjustment.calendar
    if (adjustmentCalendar == HolidayCalendarIds.NO_HOLIDAYS) calendar else adjustmentCalendar
  }

  /**
   * Normalizes the adjustment.
   *
   * Two adjustments can describe the same calculation while holding different fields, and this
   * method chooses one representative of each such pair:
   *
   *   - where the number of days is zero, the addition calendar is dropped, since adding no days
   *     is the same calculation whichever calendar is asked to do it;
   *   - where the number of days is non-zero and the addition calendar equals the adjustment's
   *     calendar, the adjustment is dropped, since a date reached by walking a calendar's
   *     business days already falls on one of them.
   *
   * Anything else is returned unchanged. The result is always equal in behaviour to the input and
   * is often equal to it outright; normalising an already normalised adjustment returns it as it
   * stands.
   *
   * This cannot fail - it rebuilds an adjustment from fields this adjustment already holds.
   *
   * @return the normalized adjustment
   */
  def normalized: DaysAdjustment =
    if (days == 0) {
      if (calendar == HolidayCalendarIds.NO_HOLIDAYS) {
        this
      } else {
        DaysAdjustment.ofCalendarDays(days, adjustment)
      }
    } else if (calendar == adjustment.calendar) {
      DaysAdjustment.ofBusinessDays(days, calendar)
    } else {
      this
    }

  /**
   * Returns a string describing the adjustment.
   *
   * The rendering is that of the type being ported, character for character. It names the number
   * of days and what kind of day they are, singular or plural as the count requires, then the
   * addition calendar where there is one, then the trailing adjustment where there is one:
   *
   * {{{
   * 0 calendar days
   * 1 calendar day then apply Following using calendar Sat/Sun
   * 3 business days using calendar Sat/Sun
   * 1 business day using calendar Sat/Sun then apply Following using calendar WedThu
   * }}}
   *
   * Note that a zero-day business-day addition renders in the calendar-day form, because
   * [[DaysAdjustment.ofBusinessDays]] builds it that way; the calendar it was given is still
   * there, in the trailing adjustment.
   *
   * @return the descriptive string
   */
  override def toString: String = {
    val plural = if (days == 1) "" else "s"
    val addition =
      if (calendar == HolidayCalendarIds.NO_HOLIDAYS) {
        s"$days calendar day$plural"
      } else {
        s"$days business day$plural using calendar ${calendar.name}"
      }
    if (adjustment == BusinessDayAdjustment.NONE) {
      addition
    } else {
      s"$addition then apply $adjustment"
    }
  }
}

/**
 * Companion of [[DaysAdjustment]], holding the no-adjustment constant, the four factories and the
 * typeclass and JSON instances.
 */
object DaysAdjustment {

  // The only construction of the type. The constructor of a `sealed abstract case class` is
  // reachable only from inside this file, and this is the sole place in the file that reaches it,
  // so every instance anywhere was built by one of the factories below and carries the addition
  // calendar that factory decided on.
  private def create(
      days: Int,
      calendar: HolidayCalendarId,
      adjustment: BusinessDayAdjustment): DaysAdjustment =
    new DaysAdjustment(days, calendar, adjustment) {}

  /**
   * An instance that performs no adjustment.
   *
   * It adds no days, names no addition calendar and carries the no-adjustment
   * [[BusinessDayAdjustment]], so it returns every date unaltered and is the identity of this
   * type. It is the adjustment to use where the surrounding structure requires one and the date
   * is to be taken as it is.
   *
   * The constant is built eagerly, which is safe because nothing in it is: both fields hold only
   * a name, so building it generates no holiday calendar and touches no reference data.
   */
  val NONE: DaysAdjustment =
    create(0, HolidayCalendarIds.NO_HOLIDAYS, BusinessDayAdjustment.NONE)

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance that can adjust a date by a specific number of calendar days.
   *
   * When adjusting a date, the specified number of calendar days is added. Holidays and weekends
   * are not taken into account in the calculation, so two days after a Friday is the Sunday.
   *
   * No business day adjustment is applied to the result of the addition, so the result may fall
   * on a weekend or a holiday. Use [[DaysAdjustment.ofCalendarDays(numberOfDays:Int,adjustment:com\.opengamma\.strata\.basics\.date\.BusinessDayAdjustment)* the two-argument form]]
   * to adjust it.
   *
   * @param numberOfDays  the number of days, which may be negative
   * @return the days adjustment
   */
  def ofCalendarDays(numberOfDays: Int): DaysAdjustment =
    create(numberOfDays, HolidayCalendarIds.NO_HOLIDAYS, BusinessDayAdjustment.NONE)

  /**
   * Obtains an instance that can adjust a date by a specific number of calendar days.
   *
   * When adjusting a date, the specified number of calendar days is added. Holidays and weekends
   * are not taken into account in the addition; the business day adjustment is then applied to
   * the result of that addition, so the final date is a business day of whichever calendar the
   * adjustment names.
   *
   * @param numberOfDays  the number of days, which may be negative
   * @param adjustment  the business day adjustment to apply to the result of the addition
   * @return the days adjustment
   */
  def ofCalendarDays(numberOfDays: Int, adjustment: BusinessDayAdjustment): DaysAdjustment =
    create(numberOfDays, HolidayCalendarIds.NO_HOLIDAYS, adjustment)

  /**
   * Obtains an instance that can adjust a date by a specific number of business days.
   *
   * When adjusting a date, the specified number of business days is added. This is equivalent to
   * repeatedly finding the next business day, so two days after a Friday is the Tuesday where the
   * weekend is Saturday and Sunday. If the input is a holiday, the first business day counted is
   * the next business day.
   *
   * No separate business day adjustment is applied to the result of the addition, because the
   * addition itself lands on a business day of the calendar named here.
   *
   * '''The zero-day case.''' Adding zero business days names no day, so this factory reads the
   * request as the one thing it can mean - "the next business day of this calendar, or this date
   * if it already is one" - and builds `(0, NoHolidays, Following using this calendar)` rather
   * than `(0, this calendar, no adjustment)`. The two differ in what they hold and agree in what
   * they compute for a business day, while only the former also moves a holiday forwards, which
   * is the behaviour the type being ported had and which the spot lags and index conventions
   * built on this type depend on. It is also why such an instance renders in the calendar-day
   * form, and why its [[DaysAdjustment.calendar]] is `NoHolidays` while its [[resultCalendar]] is
   * the calendar given here.
   *
   * @param numberOfDays  the number of days, which may be negative
   * @param holidayCalendar  the identifier of the calendar that defines holidays and business days
   * @return the days adjustment
   */
  def ofBusinessDays(numberOfDays: Int, holidayCalendar: HolidayCalendarId): DaysAdjustment =
    if (numberOfDays == 0) {
      create(
        0,
        HolidayCalendarIds.NO_HOLIDAYS,
        BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, holidayCalendar))
    } else {
      create(numberOfDays, holidayCalendar, BusinessDayAdjustment.NONE)
    }

  /**
   * Obtains an instance that can adjust a date by a specific number of business days.
   *
   * When adjusting a date, the specified number of business days is added using the calendar
   * named here, and the business day adjustment is then applied to the result of that addition.
   * The adjustment is expected to name a different calendar - that is the case this form exists
   * for, as the class-level documentation describes - since adjusting against the calendar the
   * addition already walked changes nothing.
   *
   * Unlike the two-argument form, this factory holds exactly the fields it is given, including a
   * day count of zero, so it is also the way to rebuild an adjustment from the fields of an
   * existing one.
   *
   * @param numberOfDays  the number of days, which may be negative
   * @param holidayCalendar  the identifier of the calendar that defines holidays and business days
   * @param adjustment  the business day adjustment to apply to the result of the addition
   * @return the days adjustment
   */
  def ofBusinessDays(
      numberOfDays: Int,
      holidayCalendar: HolidayCalendarId,
      adjustment: BusinessDayAdjustment): DaysAdjustment =
    create(numberOfDays, holidayCalendar, adjustment)

  //-------------------------------------------------------------------------
  /**
   * The hashing and equality of adjustments.
   *
   * Two adjustments are equal when all three fields are equal, which is the equality the case
   * class derives and the equality of the bean being ported: the day count, the name of the
   * addition calendar and the convention and calendar of the trailing adjustment. No field holds
   * a `Double`, so there is no bit-pattern comparison to arrange, and two adjustments that
   * compute the same dates by holding different fields are deliberately '''not''' equal -
   * [[DaysAdjustment.normalized]] is how a caller asks for the representative form before
   * comparing.
   *
   * This is the type's only equality-bearing instance; `Eq[DaysAdjustment]` is obtained from it
   * by subtyping rather than declared separately. There is no `Order`: the bean being ported is
   * not `Comparable`, and an ordering over day counts and calendars would be this port's
   * invention.
   *
   * @return the hashing of adjustments, which is also their equality
   */
  implicit val hash: Hash[DaysAdjustment] = Hash.fromUniversalHashCode

  /**
   * The rendering of adjustments as text.
   *
   * Renders what [[DaysAdjustment.toString]] renders, which is the form of the library being
   * ported, so the two ways of putting an adjustment into a message agree.
   *
   * @return the rendering of an adjustment
   */
  implicit val show: Show[DaysAdjustment] = Show.show(_.toString)

  //-------------------------------------------------------------------------
  /**
   * The raw field shape the JSON codec is derived from.
   *
   * The constructor of the type is private, so the codec cannot be derived from the type itself.
   * This product holds the same three fields under the same names and in the same order, and the
   * codec is derived from it: encoding takes an adjustment apart into these fields, decoding
   * reads them and hands them to the factory that holds them as given. It exists only for those
   * two purposes - it is private, it is never returned, and nothing but the codec below builds
   * one - and its field names and declaration order are therefore the wire shape.
   *
   * @param days  the number of days to be added
   * @param calendar  the identifier of the calendar performing the addition
   * @param adjustment  the business day adjustment applied to the result
   */
  private final case class Raw(
      days: Int,
      calendar: HolidayCalendarId,
      adjustment: BusinessDayAdjustment)

  /** The derived encoder of the raw field shape, used by the encoder below. */
  private val rawEncoder: Encoder[Raw] = deriveEncoder[Raw]

  /** The derived decoder of the raw field shape, used by the decoder below. */
  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /**
   * The JSON encoding of adjustments.
   *
   * The encoding is derived when this file is compiled, so no part of it inspects a class while
   * the program runs. An instance encodes as an object holding its three fields under the names
   * the bean being ported declared, in declaration order:
   *
   * {{{
   * {"days":2,"calendar":"GBLO","adjustment":{"convention":"ModifiedFollowing","calendar":"USNY"}}
   * }}}
   *
   * The day count is a JSON number, the calendar identifier the bare string of its normalised
   * name - including a composite name such as `GBLO+USNY` - and the adjustment the object its own
   * codec writes. Two adjustments that are equal therefore encode to identical bytes.
   *
   * No field is optional, so there is no absent value to drop; the encoder is wrapped in the
   * single policy of this port for products all the same, so that the rule holds of every product
   * encoder without a reader having to check which products have optional fields today.
   *
   * @return the JSON encoding of an adjustment
   */
  implicit val encoder: Encoder[DaysAdjustment] =
    Codecs.dropNulls(
      rawEncoder.contramap[DaysAdjustment](value => Raw(value.days, value.calendar, value.adjustment)))

  /**
   * The JSON decoding of adjustments.
   *
   * This is the inverse of the encoding above and is likewise derived at compile time. All three
   * fields have to be present. The fields are handed to
   * [[DaysAdjustment.ofBusinessDays(numberOfDays:Int,holidayCalendar:com\.opengamma\.strata\.basics\.date\.HolidayCalendarId,adjustment:com\.opengamma\.strata\.basics\.date\.BusinessDayAdjustment)* the three-argument factory]],
   * which holds them exactly as given - deliberately, rather than to the two-argument form, whose
   * zero-day case would rewrite a document describing `(0, some calendar, no adjustment)` into a
   * different adjustment and break the round trip.
   *
   * Construction cannot fail, so nothing beyond the shape of the payload is checked here: a
   * payload of the right shape always yields an adjustment, and an encoded adjustment decodes
   * back to one equal to it. What the fields describe is checked where it can be - the convention
   * against its closed family, and the calendar identifiers against the reference data, when the
   * adjustment is applied.
   *
   * @return the JSON decoding of an adjustment
   */
  implicit val decoder: Decoder[DaysAdjustment] =
    rawDecoder.map(raw => ofBusinessDays(raw.days, raw.calendar, raw.adjustment))
}
