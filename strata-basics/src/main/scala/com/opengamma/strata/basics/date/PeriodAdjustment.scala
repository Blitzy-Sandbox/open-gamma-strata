/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate
import java.time.Period

import cats.Hash
import cats.Show

import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.io.circe.generic.semiauto.deriveDecoder
import _root_.io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.Resolvable
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.ValidatedFailures
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * An adjustment that alters a date by adding a period of calendar days, months and years.
 *
 * This adjustment adds a `java.time.Period` to the input date using an addition convention,
 * followed by an adjustment to ensure the result is a valid business day.
 *
 * Addition is performed using standard calendar addition. It is not possible to add a number of
 * business days using this class. See [[DaysAdjustment]] for an alternative that can handle
 * addition of business days.
 *
 * ===The two steps===
 *
 * In step one, the period is added using the [[PeriodAdditionConvention]] this adjustment
 * carries, which decides what becomes of a base date that is the last day - or the last business
 * day - of its month.
 *
 * In step two, the result of step one is optionally adjusted to be a business day using the
 * [[BusinessDayAdjustment]] this adjustment carries.
 *
 * For example, this class represents a rule such as "the end date is 5 years after the start
 * date, with end-of-month rule based on the last business day of the month, adjusted to be a
 * valid London business day using the 'ModifiedFollowing' convention".
 *
 * {{{
 * val adjustment = PeriodAdjustment.ofLastDay(
 *   Period.ofMonths(1),
 *   BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.SAT_SUN))
 *
 * // 28 February 2014 was the last day of its month, so the addition lands on 31 March 2014
 * // rather than on the 28th, and 31 March 2014 was a Monday and so needs no further adjustment
 * adjustment.flatMap(_.adjust(LocalDate.of(2014, 2, 28), ReferenceData.standard))
 * }}}
 *
 * The order of the two steps matters: the addition convention runs first and the business day
 * convention runs on its result, both against the '''same''' resolved calendar. Applying them
 * the other way round would move a date off the month end before the end-of-month rule had a
 * chance to read it, which is a different - and wrong - answer for every base date that is the
 * last business day of its month.
 *
 * ===Reference data is supplied, not looked up===
 *
 * The calendar of the business day adjustment is held as an identifier rather than as a
 * calendar, which is what lets a whole adjustment be written down, stored and passed around by
 * code that has no holiday data to hand. The data is supplied at the moment the adjustment is
 * applied, so [[com.opengamma.strata.basics.ReferenceData]] is a parameter of both [[adjust]]
 * and [[resolve]] and nothing is read from ambient state.
 *
 * Two forms are offered and they differ only in when the lookup happens. [[adjust]] resolves the
 * calendar and adjusts one date; [[resolve]] resolves the calendar once and returns a
 * [[DateAdjuster]] holding it, so a run of dates costs one lookup rather than one per date:
 *
 * {{{
 * val one = adjustment.adjust(date, ReferenceData.standard)
 * val many = adjustment.resolve(ReferenceData.standard).map(adjuster => dates.map(adjuster.adjust))
 * }}}
 *
 * The two paths compute the same date for the same inputs by construction - they are the same
 * expression with the lookup hoisted - and the resolved form is the one to reach for wherever
 * the calendar is composite, such as `GBLO+USNY`, because assembling a composite calendar is
 * what [[adjust]] would otherwise repeat for every date it is given.
 *
 * A resolved adjuster is bound to the calendar it was resolved against and does not follow
 * later changes to the reference data, which is the caveat
 * [[com.opengamma.strata.basics.Resolvable]] documents for every resolved form.
 *
 * `toReader` is inherited from `Resolvable` and needs no override here. It expresses the same
 * resolution as a value awaiting reference data, which is how several adjustments are composed
 * before any data is available; a reader for one particular date is that reader mapped over the
 * adjuster it produces:
 *
 * {{{
 * val endDate: RefDataReader[LocalDate] = adjustment.toReader.map(_.adjust(startDate))
 * }}}
 *
 * ===Failure is returned===
 *
 * Both [[adjust]] and [[resolve]] answer with `Either[Failure, _]`, and there is exactly one way
 * either of them fails: the reference data supplied holds no calendar for the identifier the
 * business day adjustment names, and the failure [[HolidayCalendarId.resolve]] produces names
 * that identifier. Neither step can fail once the calendar is in hand: calendar addition is
 * defined for every date and every period, and every business day convention answers for every
 * date its calendar answers for.
 *
 * ===Construction is validated===
 *
 * This type carries one invariant: a period holding days cannot be combined with a month-based
 * addition convention. Both end-of-month rules are expressed in terms of the month a date falls
 * in, so a period measured partly in days has no defined meaning under them, and the pairing is
 * rejected rather than silently producing a date nobody intended.
 *
 * The primary constructor is private and no `apply` or `copy` exists, so [[PeriodAdjustment.of]]
 * and its two convention-specific forms are the only ways to obtain an instance, and each of
 * them reports what was wrong with its input rather than interrupting the caller. Every instance
 * in existence therefore satisfies the invariant, and nothing downstream has to re-check it.
 * Pattern matching is unaffected: `unapply` is available, so
 * `case PeriodAdjustment(period, convention, adjustment) => ...` reads the three fields.
 *
 * There is deliberately no field-by-field builder: an instance assembled a field at a time would
 * exist before its invariant had been checked, and the three factories reach every adjustment
 * this type can hold.
 *
 * This type is immutable and thread-safe.
 *
 * @param period  the period to be added, which is added to the input date when the adjustment is
 *   performed
 * @param additionConvention  the addition convention to apply, which refines the added date -
 *   most commonly by moving an end date to the last business day of its month when the start
 *   date was the last business day of its own
 * @param adjustment  the business day adjustment applied to the result of the addition, which is
 *   [[BusinessDayAdjustment.NONE]] where no adjustment is required
 * @see [[DaysAdjustment]] for the adjustment that adds days rather than a calendar period
 * @see [[TenorAdjustment]] for the same rule expressed over a [[Tenor]]
 * @see [[PeriodAdditionConvention]] for the end-of-month rules step one can apply
 */
sealed abstract case class PeriodAdjustment private (
    period: Period,
    additionConvention: PeriodAdditionConvention,
    adjustment: BusinessDayAdjustment)
    extends Resolvable[DateAdjuster]
    with NoJavaSerialization {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means - which
  // would carry a period and an addition convention no factory had checked against one another -
  // can be stopped is here. The single implementation is the companion's hidden `Impl`.
  JvmClosure.requireSoleImplementation(this, classOf[PeriodAdjustment.Impl])

  // The invariant of this type, stated over the fields the instance actually holds rather than
  // over the arguments a factory was given, because the class file of the implementation carries
  // a public constructor whatever the source asked for: a class compiled outside this library can
  // reach it directly, and the check above would admit what it built, its runtime class being the
  // one class that check admits. What is left to state is the one condition the factories check -
  // both end-of-month rules are expressed in terms of the month a date falls in, so a period
  // measured partly in days has no meaning under either of them. It is the factories' own test,
  // written the same way round, so the two reject the same pairings.
  JvmClosure.requireInvariant(
    "a month-based addition convention is paired with a period holding no days",
    !additionConvention.isMonthBased || period.getDays == 0)

  /**
   * Adjusts the date, adding the period and then applying the business day adjustment.
   *
   * The calculation is performed in two steps.
   *
   * Step one adds the period using [[PeriodAdditionConvention.adjust]], which may move the
   * result to the end of its month or to the last business day of its month.
   *
   * Step two adjusts the result of step one with the business day convention of this
   * adjustment's [[BusinessDayAdjustment]].
   *
   * The holiday calendar is resolved once, before either step, and both steps read that one
   * calendar, which is what matters for a composite identifier, whose resolution assembles a
   * calendar from its parts.
   *
   * @param date  the date to adjust
   * @param refData  the reference data that supplies the holiday calendar
   * @return the adjusted date, or the failure reporting that the reference data holds no calendar
   *   for the identifier this adjustment names
   */
  def adjust(date: LocalDate, refData: ReferenceData): Either[Failure, LocalDate] =
    adjustment.calendar.resolve(refData).map { holCal =>
      adjustment.convention.adjust(additionConvention.adjust(date, period, holCal), holCal)
    }

  /**
   * Resolves this adjustment using the specified reference data, returning an adjuster.
   *
   * This returns a [[DateAdjuster]] that performs the same calculation as this adjustment. The
   * holiday calendar is looked up from the reference data once, here, and bound into the result,
   * so the adjuster returned performs no further lookup however many dates are put through it
   * and there is no need to supply the reference data again.
   *
   * The adjuster is bound to the calendar as it stood at this moment and will not follow later
   * changes to the reference data, so care is needed when placing one in a cache or a
   * persistence layer. The unresolved adjustment has no such caveat, which is why both forms
   * exist.
   *
   * @param refData  the reference data that supplies the holiday calendar
   * @return the adjuster bound to a specific holiday calendar, or the failure reporting that the
   *   reference data holds no calendar for the identifier this adjustment names
   */
  override def resolve(refData: ReferenceData): Either[Failure, DateAdjuster] =
    adjustment.calendar.resolve(refData).map { holCal =>
      val businessDayConvention = adjustment.convention
      DateAdjuster(date =>
        businessDayConvention.adjust(additionConvention.adjust(date, period, holCal), holCal))
    }

  /**
   * Returns a string describing the adjustment.
   *
   * The period is always rendered, in the ISO form its own type produces - `P3M`, `P1Y2M3D`,
   * `P0D` - and each of the other two fields is rendered only when it says something: the
   * addition convention when it is not the convention that adds the period unchanged, and the
   * business day adjustment when it is not [[BusinessDayAdjustment.NONE]]. So the adjustment
   * that adds three months to the month end and then rolls forward to a working day renders as
   * `P3M with LastDay then apply Following using calendar Sat/Sun`, while the adjustment that
   * does nothing at all renders as `P0D`.
   *
   * This is the grammar the `Show` instance renders as well, so the two ways of putting an
   * adjustment into a message agree.
   *
   * @return the descriptive string
   */
  override def toString: String = {
    val convention =
      if (additionConvention != PeriodAdditionConventions.NONE) s" with $additionConvention" else ""
    val businessDays =
      if (adjustment != BusinessDayAdjustment.NONE) s" then apply $adjustment" else ""
    s"$period$convention$businessDays"
  }
}

/**
 * Provides the three ways of obtaining a period adjustment, the no-adjustment constant, and the
 * instances for the type.
 *
 * Each factory answers with `ResultNec` - a value or a non-empty chain of failures - because the
 * one invariant of the type is a property of a caller's arguments rather than of its own code.
 * The chain is the accumulating shape every validating factory of this library returns, so a
 * second invariant added here would report alongside the first rather than hiding it, and a
 * caller that only wants the value reaches it with `map` or `flatMap` as it would any other
 * result.
 */
object PeriodAdjustment {

  /**
   * The message reporting a period that holds days under a month-based addition convention.
   *
   * It is held once so that every route into the type, all three factories funnelling through
   * the same check, reports the rejected pairing in identical words.
   */
  private val MonthBasedMessage: String =
    "Period must not contain days when addition convention is month-based"

  /**
   * An instance that performs no adjustment.
   *
   * It pairs a period of zero with the addition convention that adds a period unchanged and the
   * business day adjustment that adjusts nothing, so it returns every date unaltered and is the
   * identity of this type. It is the adjustment to use where the surrounding structure requires
   * one and the date is to be left alone.
   *
   * The constant is built eagerly and directly, without going through [[of]], which is sound on
   * both counts: its convention is not month-based, so the invariant of the type holds of it by
   * inspection, and none of its three parts generates a holiday calendar or touches reference
   * data - the business day adjustment holds only a convention and an identifier.
   */
  val NONE: PeriodAdjustment =
    create(Period.ZERO, PeriodAdditionConventions.NONE, BusinessDayAdjustment.NONE)

  /**
   * Obtains an instance that can adjust a date by the specified period.
   *
   * When adjusting a date, the specified period is added to the input date using the specified
   * addition convention. The business day adjustment then ensures the result is a valid business
   * day.
   *
   * The period and the convention have to agree: a period holding days is rejected for either of
   * the two month-based conventions, which is the one invariant of this type.
   *
   * {{{
   * PeriodAdjustment.of(Period.of(1, 2, 3), PeriodAdditionConventions.NONE, BusinessDayAdjustment.NONE)
   * // Right(P1Y2M3D)
   * PeriodAdjustment.of(Period.of(1, 2, 3), PeriodAdditionConventions.LAST_DAY, BusinessDayAdjustment.NONE)
   * // Left - the period holds days and the convention is month-based
   * }}}
   *
   * @param period  the period to add to the input date
   * @param additionConvention  the convention that performs the addition
   * @param adjustment  the business day adjustment to apply to the result of the addition
   * @return the period adjustment, or the failure naming the broken constraint: a period whose
   *   day amount is not zero cannot be paired with a month-based addition convention
   */
  def of(
      period: Period,
      additionConvention: PeriodAdditionConvention,
      adjustment: BusinessDayAdjustment): ResultNec[PeriodAdjustment] =
    checkedAdditionConvention(period, additionConvention)
      .map(_ => create(period, additionConvention, adjustment))
      .toEither

  /**
   * Obtains an instance that can adjust a date by the specified period using the last day of
   * month convention.
   *
   * When adjusting a date, the specified period is added to the input date, shifting to the end
   * of the month where the input date is the last day of its own month. The business day
   * adjustment then ensures the result is a valid business day.
   *
   * The period must consist only of months and/or years, since the convention applied here is
   * month-based; a period holding days is reported as a failure.
   *
   * @param period  the period to add to the input date
   * @param adjustment  the business day adjustment to apply to the result of the addition
   * @return the period adjustment, or the failure naming the broken constraint: the convention
   *   applied here is month-based, so the day amount of the period must be zero
   */
  def ofLastDay(period: Period, adjustment: BusinessDayAdjustment): ResultNec[PeriodAdjustment] =
    of(period, PeriodAdditionConventions.LAST_DAY, adjustment)

  /**
   * Obtains an instance that can adjust a date by the specified period using the last business
   * day of month convention.
   *
   * When adjusting a date, the specified period is added to the input date, shifting to the last
   * business day of the month where the input date is the last business day of its own month.
   * The business day adjustment then ensures the result is a valid business day.
   *
   * The period must consist only of months and/or years, since the convention applied here is
   * month-based; a period holding days is reported as a failure.
   *
   * @param period  the period to add to the input date
   * @param adjustment  the business day adjustment to apply to the result of the addition
   * @return the period adjustment, or the failure naming the broken constraint: the convention
   *   applied here is month-based, so the day amount of the period must be zero
   */
  def ofLastBusinessDay(
      period: Period,
      adjustment: BusinessDayAdjustment): ResultNec[PeriodAdjustment] =
    of(period, PeriodAdditionConventions.LAST_BUSINESS_DAY, adjustment)

  /**
   * Checks that the period agrees with the addition convention it is paired with.
   *
   * A month-based convention is one whose rule is expressed in terms of the month a date falls
   * in, and a period holding days has no defined meaning under such a rule, so the two cannot be
   * combined: the check passes unless the convention is month-based and the day amount of the
   * period is not zero.
   *
   * There is nothing worth returning from it - both values it reads are already in the caller's
   * hands - so its outcome carries `Unit`, which combines with further checks exactly as any
   * other value would. This is the whole validation surface of the type: the three fields are
   * required, which their types state on their own, and nothing else about them can be wrong.
   *
   * @param period  the period to check
   * @param additionConvention  the convention the period is paired with
   * @return a passing outcome, or the failure naming the rejected pairing of a month-based
   *   convention with a period whose day amount is not zero
   */
  private def checkedAdditionConvention(
      period: Period,
      additionConvention: PeriodAdditionConvention): ValidatedFailures[Unit] =
    Validate.isFalse(additionConvention.isMonthBased && period.getDays != 0, MonthBasedMessage)

  /**
   * Creates an adjustment, which every route into the type funnels through.
   *
   * This is the only instantiation of the type and it is private, so the factories above and the
   * constant are the only ways into it from outside this file. The constructor of a
   * `sealed abstract case class` is reachable only from inside the file that declares it, and
   * the companion's hidden [[Impl]] subclass is how it is reached; that is what leaves the type
   * without a public `apply` or `copy` while keeping the `equals`, `hashCode` and `unapply` a
   * case class provides.
   *
   * The method performs no check of its own, and both of its callers have already established
   * the invariant: [[of]] through [[checkedAdditionConvention]], on behalf of all three
   * factories since the other two delegate to it, and [[NONE]] by inspection. This type has no
   * operation that derives one instance from another - no `with` method, no arithmetic - so
   * there is no further route by which an unchecked pairing could arrive here, which is why the
   * check belongs in the factory rather than being repeated as a fail-fast assertion here.
   *
   * @param period  the period to be added
   * @param additionConvention  the addition convention, already checked against the period
   * @param adjustment  the business day adjustment applied after the addition
   * @return the adjustment
   */
  private def create(
      period: Period,
      additionConvention: PeriodAdditionConvention,
      adjustment: BusinessDayAdjustment): PeriodAdjustment =
    new Impl(period, additionConvention, adjustment)

  /**
   * The one implementation of a period adjustment.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it. It is declared rather than written as an anonymous subclass at the instantiation site
   * for two reasons, both about what the class file says: a private member class is one a Java
   * compiler refuses to name, where an anonymous class is public and can be instantiated directly
   * by a caller in another language, and a named class can be compared against, which is what
   * lets [[PeriodAdjustment]] refuse in its own constructor to be any other implementation.
   *
   * @param period  the period to be added
   * @param additionConvention  the addition convention, already checked against the period
   * @param adjustment  the business day adjustment applied after the addition
   */
  private final class Impl(
      period: Period,
      additionConvention: PeriodAdditionConvention,
      adjustment: BusinessDayAdjustment)
      extends PeriodAdjustment(period, additionConvention, adjustment)

  /**
   * The hashing and equality of adjustments.
   *
   * Taken from the `equals` and `hashCode` of the case class, which compare the three fields by
   * their own equality - the amount of a period, the identity of an addition convention, and the
   * convention and calendar name of a business day adjustment. No field holds a `Double`, so
   * there is no bit-pattern comparison to arrange, and equality is structural over the three
   * fields.
   *
   * Note that period equality is that of `java.time.Period`, which is equality of its three
   * amounts rather than of the length of time they denote: an adjustment adding `P1Y` is not
   * equal to one adding `P12M`.
   *
   * This is the type's only equality-bearing instance, and `Eq[PeriodAdjustment]` is obtained
   * from it by subtyping rather than declared separately. No `Order` is declared, because an
   * ordering over a period, an addition convention and a calendar would have to invent a
   * precedence among the three.
   *
   * @return the hashing of adjustments
   */
  implicit val hash: Hash[PeriodAdjustment] = Hash.fromUniversalHashCode[PeriodAdjustment]

  /**
   * The rendering of adjustments as text.
   *
   * Renders what `toString` renders, so the two ways of putting an adjustment into a message
   * agree.
   *
   * @return the rendering of an adjustment
   */
  implicit val show: Show[PeriodAdjustment] = Show.show(_.toString)

  /**
   * The field shape both codecs are derived from, which the decoder reads before validation.
   *
   * Decoding a validated type is two steps: read the fields, then hand them to the factory that
   * decides whether they describe a value. This product is the first step, and encoding is the
   * same two steps in reverse, so both codecs below derive from this one declaration and the
   * JSON shape of an adjustment is stated exactly once. It is private and never returned - the
   * only values of it that exist are the ones the two codecs build. Its field names are the JSON
   * keys, and they are the names of the three fields of [[PeriodAdjustment]] itself, in their
   * declaration order.
   *
   * @param period  the period, carried as the ISO `P3M` string form by the codec of its own type
   * @param additionConvention  the addition convention, carried as its canonical name
   * @param adjustment  the business day adjustment, carried as the object of its own two fields
   */
  private final case class Raw(
      period: Period,
      additionConvention: PeriodAdditionConvention,
      adjustment: BusinessDayAdjustment)
      extends NoJavaSerialization

  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  private val rawEncoder: Encoder[Raw] = deriveEncoder[Raw]

  /**
   * The JSON encoding of adjustments.
   *
   * An adjustment is an object of three fields, the period as its ISO text form, the addition
   * convention as its canonical name, and the business day adjustment as the object its own
   * codec writes:
   *
   * {{{
   * {"period":"P3M","additionConvention":"LastDay",
   *  "adjustment":{"convention":"Following","calendar":"Sat/Sun"}}
   * }}}
   *
   * Both halves of the codec are assembled by the same compile-time derivation over the same raw
   * shape, which is what keeps them from drifting apart: the field names, their order and their
   * element codecs are stated once, in [[Raw]], and the encoder reaches them by mapping an
   * adjustment onto that shape. Deriving from [[PeriodAdjustment]] itself is not possible - the
   * constructor of a validated type is not public, so there is no public shape to derive from -
   * and writing the fields out by hand instead would state the same contract a second time.
   *
   * No part of the encoding inspects a class while the program runs, and two adjustments that
   * are equal encode to identical bytes. The result is wrapped so that a field holding no value
   * would be omitted, which is the policy every product of this library follows - this type has
   * no optional field, so the wrapping changes nothing about its output and exists so that the
   * policy holds without exception.
   *
   * @return the JSON encoding of an adjustment
   */
  implicit val encoder: Encoder[PeriodAdjustment] =
    Codecs.dropNulls(rawEncoder.contramap[PeriodAdjustment] { value =>
      Raw(value.period, value.additionConvention, value.adjustment)
    })

  /**
   * The JSON decoding of adjustments.
   *
   * This is the inverse of the encoding above, and it decides whether the fields describe an
   * adjustment exactly as a caller's arguments are decided: the payload is read into the raw
   * shape and handed to [[of]], so a document pairing a period that holds days with a
   * month-based addition convention is a decoding failure carrying that reason rather than a
   * value this type would not have built. All three fields have to be present.
   *
   * @return the JSON decoding of an adjustment
   */
  implicit val decoder: Decoder[PeriodAdjustment] =
    Codecs.validatedDecoder[Raw, PeriodAdjustment] { raw =>
      of(raw.period, raw.additionConvention, raw.adjustment)
    }(rawDecoder)
}
