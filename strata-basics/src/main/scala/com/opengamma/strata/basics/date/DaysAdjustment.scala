/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate

import cats.Hash
import cats.Show
import cats.syntax.apply._

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.Resolvable
import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.ValidatedFailures
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
 * Step two is applied whatever the day count is, so an adjustment of zero days is not the
 * identity: it adds nothing and then still applies its [[adjustment]], moving a date that is not
 * a business day of the calendar that adjustment names. Only an adjustment that also carries the
 * no-adjustment [[BusinessDayAdjustment]] - [[DaysAdjustment.NONE]] - returns every date
 * unaltered.
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
 * ===Failure is reported as a value===
 *
 * [[adjust]] and [[resolve]] both answer `Either[Failure, _]`, and calendar resolution is the
 * one thing either of them can fail at: where the reference data does not supply a calendar this
 * adjustment names, [[HolidayCalendarId.resolve]] reports a failure naming that identifier.
 * Nothing else about applying an adjustment can fail - once the calendars are in hand, every
 * convention answers for every date they answer for.
 *
 * ===Construction===
 *
 * An adjustment is a '''validated''' value. [[DaysAdjustment.of]] is the factory that judges the
 * three fields, answering `ResultNec` - the adjustment or a non-empty chain of failures - and it
 * is the one place the pairing of the day count with the addition calendar is checked:
 *
 * {{{
 * DaysAdjustment.of(2, HolidayCalendarIds.GBLO, BusinessDayAdjustment.NONE)  // Right
 * DaysAdjustment.of(0, HolidayCalendarIds.GBLO, BusinessDayAdjustment.NONE)  // Left - see below
 * }}}
 *
 * The condition is the one this class states above: '''a business-day addition of zero days names
 * no day at all'''. An addition calendar other than the no-holidays identifier is what makes the
 * addition walk business days, so pairing one with a day count of zero names no day, and `of`
 * reports it rather than building a value whose calendar is never consulted. Nothing else about
 * the three fields can be wrong - a day count is any integer, and a calendar is a name rather
 * than a resolved calendar - so that single condition is the whole of the check, and it is
 * reported through the accumulating failure channel.
 *
 * The four named factories are '''total''', and they are total because each of them lands in the
 * part of the field space `of` accepts rather than because construction is unchecked:
 * [[DaysAdjustment.ofCalendarDays]] fixes the addition calendar to the no-holidays identifier,
 * and both [[DaysAdjustment.ofBusinessDays]] forms answer a zero-day request by dropping the
 * addition calendar. Every value any factory builds is therefore accepted by `of`, and every
 * triple `of` rejects is built by none of them, so the two construction routes cannot drift
 * apart.
 *
 * What the factories have that a raw constructor would lose is meaning - which calendar performs
 * the addition, and the zero-day case of [[DaysAdjustment.ofBusinessDays]] - and together with
 * `of` they are the only way to build one: the constructor of this `sealed abstract case class` is
 * private and neither `apply` nor `copy` exists. Pattern matching and `unapply` are unaffected,
 * and a modified instance is obtained by naming the change through a factory.
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
    extends Resolvable[DateAdjuster]
    with NoJavaSerialization {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means - which
  // would carry a day count and a calendar pairing no factory had checked - can be stopped is
  // here. The single implementation is the companion's hidden `Impl`.
  JvmClosure.requireSoleImplementation(this, classOf[DaysAdjustment.Impl])

  // The invariant of this type, stated over the fields the instance actually holds rather than
  // over the arguments a factory was given, because the class file of the implementation carries
  // a public constructor whatever the source asked for: a class compiled outside this library can
  // reach it directly, and the check above would admit what it built, its runtime class being the
  // one class that check admits. What is left to state is the one condition `of` checks and the
  // named factories land on - a business-day addition of zero days names no day, so a day count
  // of zero goes with the no-holidays addition calendar and with no other. Nothing else about the
  // three fields can be wrong, as the documentation of `of` explains.
  JvmClosure.requireInvariant(
    "a day count of zero is added by the calendar that declares no holidays",
    days != 0 || calendar == HolidayCalendarIds.NO_HOLIDAYS)

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
   * @param refData  the reference data supplying the holiday calendars this adjustment names
   * @return the adjusted date, or a failure naming the calendar identifier the reference data
   *   does not supply
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
   * Where the addition calendar is `NoHolidays` the addition is performed as plain date
   * arithmetic rather than by asking a calendar that has no holidays to shift a date, which is
   * the same answer computed without the indirection. That shortcut is also why an adjustment
   * adding calendar days resolves against reference data that holds no `NoHolidays` entry, where
   * [[adjust]] on the same adjustment reports it missing; the reference data this library builds
   * - [[com.opengamma.strata.basics.ReferenceData.standard]], `minimal`, and anything from
   * `ReferenceData.of` - always holds that entry, so the two paths answer alike for it.
   *
   * The adjuster is bound to the calendars as they stood at this moment and will not follow later
   * changes to the reference data, so care is needed when placing one in a cache or a persistence
   * layer. The unresolved adjustment has no such caveat, which is why both forms exist.
   *
   * @param refData  the reference data supplying the holiday calendars this adjustment names
   * @return the adjuster bound to specific holiday calendars, or a failure naming the calendar
   *   identifier the reference data does not supply
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
   * The first of those two rewrites is '''already done''' by the time any adjustment exists:
   * every factory drops the addition calendar of a zero-day addition and [[DaysAdjustment.of]]
   * refuses that pairing outright, so a zero-day adjustment always names the no-holidays calendar
   * already and is returned as it stands. The rule is stated above because it is still the rule -
   * it is simply enforced at construction rather than repaired here.
   *
   * This cannot fail - it rebuilds an adjustment from fields this adjustment already holds.
   *
   * @return the normalized adjustment
   */
  def normalized: DaysAdjustment =
    if (days == 0) {
      this
    } else if (calendar == adjustment.calendar) {
      DaysAdjustment.ofBusinessDays(days, calendar)
    } else {
      this
    }

  /**
   * Returns a string describing the adjustment.
   *
   * The rendering names the number of days and what kind of day they are, singular or plural as
   * the count requires, then the addition calendar where there is one, then the trailing
   * adjustment where there is one:
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
   * The addition calendar is rendered by asking the identifier for its own text form rather
   * than by taking its name, which is what keeps this rendering bounded and on a single line:
   * an identifier accepts any text, including text that reached the library from outside it,
   * and [[HolidayCalendarId.toString]] is where that text is made safe to write out. For every
   * identifier of a realistic shape the two are the same characters, so the renderings above
   * are unchanged.
   *
   * @return the descriptive string
   */
  override def toString: String = {
    val plural = if (days == 1) "" else "s"
    val addition =
      if (calendar == HolidayCalendarIds.NO_HOLIDAYS) {
        s"$days calendar day$plural"
      } else {
        s"$days business day$plural using calendar $calendar"
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
    new Impl(days, calendar, adjustment)

  /**
   * The one implementation of a days adjustment.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it. It is declared rather than written as an anonymous subclass at the instantiation site
   * for two reasons, both about what the class file says: a private member class is one a Java
   * compiler refuses to name, where an anonymous class is public and can be instantiated directly
   * by a caller in another language, and a named class can be compared against, which is what
   * lets [[DaysAdjustment]] refuse in its own constructor to be any other implementation.
   *
   * @param days  the number of days to add, as the factory settled it
   * @param calendar  the addition calendar the factory decided on
   * @param adjustment  the business day adjustment applied to the result of the addition
   */
  private final class Impl(
      days: Int,
      calendar: HolidayCalendarId,
      adjustment: BusinessDayAdjustment)
      extends DaysAdjustment(days, calendar, adjustment)

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

  /**
   * Obtains an instance from the number of days, the addition calendar and the trailing
   * adjustment, rejecting a day count of zero paired with an addition calendar other than the
   * no-holidays identifier.
   *
   * This is the validated factory of the type, and it is the route to reach for where the three
   * fields are held already - read off another adjustment, decoded from a document, or computed
   * from data - because it is the one that judges them. The named factories below are the route
   * to reach for where the '''kind''' of addition is being named rather than its fields.
   *
   * One condition is checked, and it is the one the class-level documentation states: the addition
   * calendar decides whether the days are calendar days or business days, so a day count of zero
   * paired with a calendar other than the no-holidays identifier asks for a business-day addition
   * of zero days, which names no day at all. Everything else is accepted: the day count may be
   * negative, either calendar may be composite, and the trailing adjustment may name any
   * convention over any calendar.
   *
   * {{{
   * DaysAdjustment.of(2, HolidayCalendarIds.GBLO, BusinessDayAdjustment.NONE)   // Right
   * DaysAdjustment.of(-2, HolidayCalendarIds.GBLO, BusinessDayAdjustment.NONE)  // Right
   * DaysAdjustment.of(0, HolidayCalendarIds.NO_HOLIDAYS, BusinessDayAdjustment.NONE)  // Right
   * DaysAdjustment.of(0, HolidayCalendarIds.GBLO, BusinessDayAdjustment.NONE)   // Left
   * }}}
   *
   * A caller wanting the interpretable reading of that rejected request - "the next business day
   * of this calendar, or this date if it already is one" - names it through
   * [[DaysAdjustment.ofBusinessDays(numberOfDays:Int,holidayCalendar:com\.opengamma\.strata\.basics\.date\.HolidayCalendarId)* the two-argument business-day factory]],
   * which builds that rule and cannot fail.
   *
   * @param days  the number of days to be added, which may be negative
   * @param calendar  the identifier of the calendar that defines the meaning of a day when
   *   performing the addition
   * @param adjustment  the business day adjustment to apply to the result of the addition
   * @return the days adjustment, or the failure naming the broken condition: a day count of zero
   *   requires the no-holidays identifier as the addition calendar, since an addition of zero
   *   business days names no day
   */
  def of(
      days: Int,
      calendar: HolidayCalendarId,
      adjustment: BusinessDayAdjustment): ResultNec[DaysAdjustment] =
    (checkedAddition(days, calendar), checkedMagnitude(days, calendar))
      .mapN((_, _) => create(days, calendar, adjustment))
      .toEither

  /**
   * Checks that the number of days agrees with the calendar that is to add them.
   *
   * A calendar other than the no-holidays identifier makes the addition walk that calendar's
   * business days, and there is no such thing as walking zero of them - the request names no day,
   * which is why the two-argument business-day factory answers it by naming a rule instead and why
   * [[DaysAdjustment.normalized]] erases the pairing wherever one reaches it.
   *
   * There is nothing worth returning from the check - both values it reads are already in the
   * caller's hands - so its outcome carries `Unit`, which combines with further checks exactly as
   * any other value would. This is the whole validation surface of the type: the three fields are
   * required, which their types state on their own, and nothing else about them can be wrong.
   *
   * The rejected identifier reaches the message as it stands, so a caller correcting its input is
   * handed back exactly what was refused.
   *
   * @param days  the number of days to check
   * @param calendar  the identifier of the calendar the days are counted against
   * @return a passing outcome, or the failure naming the rejected pairing of a zero day count
   *   with an addition calendar other than the no-holidays identifier
   */
  private def checkedAddition(
      days: Int,
      calendar: HolidayCalendarId): ValidatedFailures[Unit] =
    Validate.isFalse(
      days == 0 && calendar != HolidayCalendarIds.NO_HOLIDAYS,
      s"A business day addition of zero days names no day, so 'calendar' must be " +
        s"'${HolidayCalendarIds.NO_HOLIDAYS.name}' when 'days' is zero but was " +
        s"'${calendar.name}'")

  /**
   * Checks that a business-day addition names a number of days a calendar can walk.
   *
   * Adding business days is a walk of them - [[HolidayCalendar.shift]] asks the calendar about
   * each day in turn - so the cost of applying such an adjustment is decided by the day count it
   * holds, and that count is `Int`-wide. An adjustment is '''data''': it is read from a document,
   * held in a convention and passed around, so a count of two thousand million would arrive here
   * from outside the program and then occupy a processor for the best part of a minute every time
   * the adjustment was applied, to name a date some six million years away. The count is
   * therefore judged against the same limit the calendar applies,
   * [[HolidayCalendar.MaxBusinessDayShift]], which is more than three hundred years of business
   * days and beyond every convention of finance - and judged '''here''', where the adjustment is
   * built, so that an adjustment which exists can always be applied and a document naming one
   * that cannot is refused by the reader of this type rather than by the calendar much later.
   *
   * The limit belongs to the business-day addition alone. A calendar-day addition adds its days
   * arithmetically, in one step, whatever their number, so nothing about its cost depends on the
   * count and the no-holidays identifier is accordingly exempt - which is also what keeps
   * [[DaysAdjustment.ofCalendarDays]] total for every count an `Int` can hold, as the factory
   * being ported was.
   *
   * @param days  the number of days to check
   * @param calendar  the identifier of the calendar the days are counted against
   * @return a passing outcome, or the failure describing the count that was rejected
   */
  private def checkedMagnitude(
      days: Int,
      calendar: HolidayCalendarId): ValidatedFailures[Unit] =
    Validate.isFalse(
      exceedsWalkableCount(days, calendar),
      s"An addition of more than ${HolidayCalendar.MaxBusinessDayShift} business days is " +
        s"outside the accepted range, but 'days' was: $days")

  /**
   * Decides whether an addition names more days than the calendar performing it can be walked.
   *
   * The one statement of the limit, read by both routes into this type, so that the field space
   * the validated factory accepts and the field space the named factories build in cannot
   * drift apart. The no-holidays identifier is exempt because an addition against it is
   * arithmetic on the date rather than a walk - [[HolidayCalendar.NoHolidays]] shifts by adding
   * the days in one step - which is the same exemption that calendar makes for itself.
   *
   * @param days  the number of days the addition names
   * @param calendar  the identifier of the calendar that would perform the addition
   * @return true where the count is beyond what that calendar can be walked
   */
  private def exceedsWalkableCount(days: Int, calendar: HolidayCalendarId): Boolean =
    calendar != HolidayCalendarIds.NO_HOLIDAYS &&
      // measured in `Long` arithmetic, because the magnitude of the smallest `Int` is not an
      // `Int`: negating it overflows back to itself, and the comparison would then pass
      Math.abs(days.toLong) > HolidayCalendar.MaxBusinessDayShift.toLong

  /**
   * Checks the same limit as `checkedMagnitude` for the factories that name a kind of addition.
   *
   * The named business-day factories are total in the sense the factories being ported were -
   * they hold the fields they are given rather than judging them - so a count beyond what any
   * calendar can walk is stated by raising, as a precondition of the call, which is the form
   * [[HolidayCalendar.shift]] states the same limit in. It is the one condition they check, and
   * checking it against the same predicate is what keeps them in step with
   * [[DaysAdjustment.of]] in both directions: every adjustment a factory builds is one that
   * factory's validated counterpart accepts, and every set of fields the validated factory
   * accepts is one a named factory builds.
   *
   * @param days  the number of business days the addition names, which is not zero
   * @param calendar  the identifier of the calendar that performs the addition
   * @throws IllegalArgumentException where the magnitude exceeds
   *   [[HolidayCalendar.MaxBusinessDayShift]] for a calendar that has to be walked
   */
  private def checkMagnitude(days: Int, calendar: HolidayCalendarId): Unit =
    ArgCheck.isFalse(
      exceedsWalkableCount(days, calendar),
      s"An addition of more than ${HolidayCalendar.MaxBusinessDayShift} business days is " +
        s"outside the accepted range, but was: $days")

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
   * they compute for a business day, while only the first of the two also moves a holiday
   * forwards, which is what a zero-day request is asking for. It is also why such an instance
   * renders in the calendar-day form, and why its [[DaysAdjustment.calendar]] is `NoHolidays`
   * while its [[resultCalendar]] is the calendar given here.
   *
   * '''How many days may be named.''' The addition walks the business days it is asked for, so
   * the count is limited to [[HolidayCalendar.MaxBusinessDayShift]] in either direction, as
   * `checkedMagnitude` describes, for every calendar that has to be walked - the no-holidays
   * identifier being exempt, its addition adding the days in one step. This factory names a kind
   * of addition rather than judging fields, so it states the limit by raising, exactly as
   * [[HolidayCalendar.shift]] does for the same figure; both routes read one predicate, which is
   * what keeps this factory and [[DaysAdjustment.of]] in step in both directions: no adjustment a
   * factory builds is one `of` would refuse, and no fields `of` accepts are fields a factory
   * would refuse.
   *
   * @param numberOfDays  the number of days, which may be negative
   * @param holidayCalendar  the identifier of the calendar that defines holidays and business days
   * @return the days adjustment
   * @throws IllegalArgumentException where the count exceeds
   *   [[HolidayCalendar.MaxBusinessDayShift]] business days in either direction against a
   *   calendar that has to be walked
   */
  def ofBusinessDays(numberOfDays: Int, holidayCalendar: HolidayCalendarId): DaysAdjustment =
    if (numberOfDays == 0) {
      create(
        0,
        HolidayCalendarIds.NO_HOLIDAYS,
        BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, holidayCalendar))
    } else {
      checkMagnitude(numberOfDays, holidayCalendar)
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
   * This factory holds the fields it is given, with the '''one''' exception the two-argument form
   * also makes: a day count of zero drops the addition calendar, since there is no such thing as
   * an addition of zero business days and the calendar would name a walk that never happens. The
   * adjustment supplied is kept, so the value built is `(0, NoHolidays, adjustment)`, which is
   * what [[DaysAdjustment.normalized]] answers for the fields as given. Every date the two forms
   * compute is the same, because shifting a date by zero days returns the date whichever calendar
   * is asked, so the rule chooses the representative of the pair rather than changing an answer,
   * and it is what keeps every value of this type inside the field space
   * [[DaysAdjustment.of]] accepts.
   *
   * With a non-zero day count this factory is the way to rebuild an adjustment from the fields of
   * an existing one, and [[DaysAdjustment.of]] is the way to do so while reporting a zero day
   * count paired with an addition calendar.
   *
   * The count is limited as it is in the two-argument form, to
   * [[HolidayCalendar.MaxBusinessDayShift]] business days in either direction against a calendar
   * that has to be walked.
   *
   * @param numberOfDays  the number of days, which may be negative
   * @param holidayCalendar  the identifier of the calendar that defines holidays and business days
   * @param adjustment  the business day adjustment to apply to the result of the addition
   * @return the days adjustment
   * @throws IllegalArgumentException where the count exceeds
   *   [[HolidayCalendar.MaxBusinessDayShift]] business days in either direction against a
   *   calendar that has to be walked
   */
  def ofBusinessDays(
      numberOfDays: Int,
      holidayCalendar: HolidayCalendarId,
      adjustment: BusinessDayAdjustment): DaysAdjustment =
    if (numberOfDays == 0) {
      create(0, HolidayCalendarIds.NO_HOLIDAYS, adjustment)
    } else {
      checkMagnitude(numberOfDays, holidayCalendar)
      create(numberOfDays, holidayCalendar, adjustment)
    }

  /**
   * The hashing and equality of adjustments.
   *
   * Two adjustments are equal when all three fields are equal, which is the equality the case
   * class derives: the day count, the name of the addition calendar and the convention and
   * calendar of the trailing adjustment. No field holds a `Double`, so there is no bit-pattern
   * comparison to arrange, and two adjustments that compute the same dates by holding different
   * fields are deliberately '''not''' equal - [[DaysAdjustment.normalized]] is how a caller asks
   * for the representative form before comparing.
   *
   * This is the type's only equality-bearing instance; `Eq[DaysAdjustment]` is obtained from it
   * by subtyping rather than declared separately. There is no `Order`, since no ordering over a
   * day count and two calendar identifiers carries a meaning in the domain.
   *
   * @return the hashing of adjustments, which is also their equality
   */
  implicit val hash: Hash[DaysAdjustment] = Hash.fromUniversalHashCode

  /**
   * The rendering of adjustments as text.
   *
   * Renders what [[DaysAdjustment.toString]] renders, so the two ways of putting an adjustment
   * into a message agree.
   *
   * @return the rendering of an adjustment
   */
  implicit val show: Show[DaysAdjustment] = Show.show(_.toString)

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
      extends NoJavaSerialization

  private val rawEncoder: Encoder[Raw] = deriveEncoder[Raw]

  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /**
   * The JSON encoding of adjustments.
   *
   * The encoding is derived at compile time, so no part of it inspects a class while the program
   * runs. An instance encodes as an object holding its three fields under their own names, in
   * declaration order:
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
   * shared product policy all the same, so that the rule omitting an absent value holds of every
   * product encoder without a reader having to check which products carry an optional field.
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
   * fields have to be present, and they are handed to the '''validated''' factory
   * [[DaysAdjustment.of]] rather than being wrapped unchecked, so a document holding a day count
   * of zero against a named addition calendar is a decoding failure carrying the reason that
   * factory gives. That pairing is also one no value of this type holds, since every factory drops
   * the addition calendar for a zero day count, so the round trip is unaffected: an encoded
   * adjustment decodes back to one equal to it, and the check refuses only documents that no
   * encoder here produces.
   *
   * What the fields describe beyond that pairing is checked where it can be - the convention
   * against its closed family, by its own codec, and the calendar identifiers against the
   * reference data, when the adjustment is applied.
   *
   * @return the JSON decoding of an adjustment
   */
  implicit val decoder: Decoder[DaysAdjustment] =
    Codecs.validatedDecoder[Raw, DaysAdjustment] { raw =>
      of(raw.days, raw.calendar, raw.adjustment)
    }(rawDecoder)
}
