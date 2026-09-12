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

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.Resolvable
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.ValidatedFailures
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * An adjustment that alters a date by adding a tenor.
 *
 * This adjustment adds a [[Tenor]] to the input date using an addition convention, followed by
 * an adjustment to ensure the result is a valid business day.
 *
 * Addition is performed using standard calendar addition. It is not possible to add a number of
 * business days using this class. See [[DaysAdjustment]] for an alternative that can handle
 * addition of business days.
 *
 * ===The two steps===
 *
 * In step one, the tenor's period is added using the [[PeriodAdditionConvention]] held here, so
 * a month-end base date can be carried to the end - or to the last business day - of the target
 * month rather than to the day of the month plain arithmetic would reach.
 *
 * In step two, the result of step one is adjusted to be a business day using the
 * [[BusinessDayAdjustment]] held here. Where no such adjustment is wanted,
 * [[BusinessDayAdjustment.NONE]] is the value that expresses it.
 *
 * For example, a rule represented by this class might be: "the end date is 5 years after the
 * start date, with end-of-month rule based on the last business day of the month, adjusted to be
 * a valid London business day using the 'ModifiedFollowing' convention".
 *
 * {{{
 * // 3 months after 15 August 2014 is 15 November 2014, a Saturday, so Following moves it on
 * val adjustment = BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.SAT_SUN)
 * TenorAdjustment.ofLastDay(Tenor.TENOR_3M, adjustment)
 *   .flatMap(_.adjust(LocalDate.of(2014, 8, 15), ReferenceData.standard))  // Right(2014-11-17)
 * }}}
 *
 * ===One calendar, resolved once===
 *
 * Both steps consult the same holiday calendar: the last-business-day addition convention needs
 * it to decide what the last business day of a month is, and the business day adjustment needs it
 * to decide which days it may land on. The calendar is the one the adjustment names, resolved
 * from the reference data supplied, and it is resolved exactly once per call and then used by
 * both steps - which is the behaviour of the type being ported and also the only reading of it
 * that cannot produce a date computed against two different calendars.
 *
 * ===Reference data is supplied, not looked up===
 *
 * Nothing here reads ambient state. The calendar is held as a [[HolidayCalendarId]] inside the
 * business day adjustment, and the data that resolves it arrives as a parameter of [[adjust]] and
 * [[resolve]], so the same adjustment applied to the same date against the same data always
 * answers the same way. A caller that does not have the data yet composes through `toReader`,
 * inherited from [[Resolvable]], which is this resolution as a
 * [[com.opengamma.strata.basics.RefDataReader]] awaiting its data:
 *
 * {{{
 * import cats.syntax.apply._
 *
 * val start = startAdjustment.toReader.map(_.adjust(tradeDate))
 * val end = tenorAdjustment.toReader.map(_.adjust(tradeDate))
 * (start, end).tupled.run(ReferenceData.standard)
 * }}}
 *
 * ===Failure===
 *
 * Two things can go wrong, and they are reported at the two different moments they belong to.
 * A tenor that the addition convention cannot be applied to is rejected when an adjustment is
 * built, by [[TenorAdjustment.of]] and the two factories beside it, so no instance of this type
 * ever holds that pairing. A calendar the reference data does not supply is reported by
 * [[adjust]] and [[resolve]], as `Left(Failure.MissingData)`, because whether a given body of
 * reference data covers a given calendar is not knowable when the adjustment is written down.
 * Neither case throws, where the type being ported threw from its validator and from its
 * reference data lookup.
 *
 * ===Thread safety===
 *
 * An adjustment is an immutable value holding three immutable values, and both of its methods are
 * pure functions of their arguments, so an instance is safe to share without synchronization. The
 * [[DateAdjuster]] that [[resolve]] returns closes over the calendar it resolved and is likewise
 * immutable, with the caveat every resolved form carries: it holds the calendar as it stood at the
 * moment of resolution and does not follow later changes to the reference data.
 *
 * @param tenor  the tenor to be added; when the adjustment is performed, this tenor will be added
 *   to the input date
 * @param additionConvention  the addition convention to apply, which is used to refine the
 *   adjusted date - most commonly by moving the end date to the last business day of the month
 *   when the start date is the last business day of the month
 * @param adjustment  the business day adjustment that is performed on the result of the addition;
 *   where no adjustment is required, this is [[BusinessDayAdjustment.NONE]]
 * @see [[Tenor]] for the period being added and the text it renders as
 * @see [[PeriodAdditionConvention]] for the rule applied in step one
 * @see [[BusinessDayAdjustment]] for the adjustment applied in step two
 * @see [[DaysAdjustment]] for the equivalent that adds a number of days, business or calendar
 */
sealed abstract case class TenorAdjustment private (
    tenor: Tenor,
    additionConvention: PeriodAdditionConvention,
    adjustment: BusinessDayAdjustment)
    extends Resolvable[DateAdjuster] {

  /**
   * Adjusts the date, adding the tenor and then applying the business day adjustment.
   *
   * The calculation is performed in two steps. Step one adds the tenor's period through
   * [[PeriodAdditionConvention.adjust]]. Step two adjusts the result of step one to a business day
   * through the rule of this adjustment's [[BusinessDayConvention]]. Both steps read the single
   * holiday calendar this adjustment names, resolved here from the reference data supplied.
   *
   * {{{
   * // 1 month after 30 June 2014, the last day of its month, under the last-day convention
   * tenorAdjustment.adjust(LocalDate.of(2014, 6, 30), ReferenceData.standard)  // Right(2014-07-31)
   * }}}
   *
   * @param date  the date to adjust
   * @param refData  the reference data, used to find the holiday calendar
   * @return the adjusted date, or `Left(Failure.MissingData)` where the reference data does not
   *   supply the calendar this adjustment names
   */
  def adjust(date: LocalDate, refData: ReferenceData): Either[Failure, LocalDate] =
    adjustment.calendar.resolve(refData).map { holCal =>
      val convention = adjustment.convention
      convention.adjust(additionConvention.adjust(date, tenor.period, holCal), holCal)
    }

  /**
   * Resolves this adjustment using the specified reference data, returning an adjuster.
   *
   * This returns a [[DateAdjuster]] that performs the same calculation as [[adjust]]. The holiday
   * calendar is looked up from the reference data once, here, and bound into the result together
   * with the tenor's period and the business day convention, so the adjuster returned performs no
   * further lookup however many dates are put through it and there is no need to supply the
   * reference data again.
   *
   * The adjuster is bound to the calendar as it stood at this moment and will not follow later
   * changes to the reference data, so care is needed when placing one in a cache or a persistence
   * layer. The unresolved adjustment has no such caveat, which is why both forms exist.
   *
   * @param refData  the reference data, used to find the holiday calendar
   * @return the adjuster bound to a specific holiday calendar, or `Left(Failure.MissingData)`
   *   where the reference data does not supply the calendar this adjustment names
   */
  override def resolve(refData: ReferenceData): Either[Failure, DateAdjuster] =
    adjustment.calendar.resolve(refData).map { holCal =>
      val convention = adjustment.convention
      val period: Period = tenor.period
      DateAdjuster(date => convention.adjust(additionConvention.adjust(date, period, holCal), holCal))
    }

  //-------------------------------------------------------------------------
  /**
   * Returns a string describing the adjustment.
   *
   * The description is built from the parts that say something, in the grammar of the library
   * being ported, character for character: the tenor alone where neither convention alters the
   * plain addition, the tenor and the addition convention where that convention has a rule of its
   * own, and the business day adjustment appended after `then apply` where one is to be performed:
   *
   * {{{
   * // Tenor.of(Period.of(1, 2, 3)), no addition convention, no business day adjustment
   * "1Y2M3D"
   * // Tenor.TENOR_3M, the last-day convention, Following over the Sat/Sun calendar
   * "3M with LastDay then apply Following using calendar Sat/Sun"
   * }}}
   *
   * The two parts are omitted on exactly the tests the original applied - the addition convention
   * that adds the period unchanged, and the business day adjustment that is
   * [[BusinessDayAdjustment.NONE]] - so an adjustment carrying the no-adjust convention over some
   * other calendar is still described, since it is not that value. This is what the `Show`
   * instance renders.
   *
   * @return the descriptive string
   */
  override def toString: String = {
    val added =
      if (additionConvention == PeriodAdditionConventions.NONE) {
        tenor.name
      } else {
        s"${tenor.name} with $additionConvention"
      }
    if (adjustment == BusinessDayAdjustment.NONE) {
      added
    } else {
      s"$added then apply $adjustment"
    }
  }
}

/**
 * Companion of [[TenorAdjustment]], holding the checking factories and the typeclass and JSON
 * instances.
 *
 * ===Construction===
 *
 * Every adjustment comes from one of the three factories here, each of which checks the pairing
 * of tenor and addition convention and answers with the adjustment or with the reason it does not
 * describe one. There is no public constructor and no `copy`: the type is a case class whose
 * constructor is private and whose declaration is abstract, so the compiler synthesises neither,
 * and the only instantiation of it in the program is the one inside this object, on the far side
 * of the check. Pattern matching is unaffected - `unapply` is synthesised as it is for any case
 * class - so a `case TenorAdjustment(tenor, convention, adjustment) =>` reads the three parts of
 * a value that was checked when it was built.
 *
 * There is deliberately no `NONE` constant here, unlike [[DaysAdjustment]] and
 * [[PeriodAdjustment]]. The type being ported has none either, and there is no tenor that adds
 * nothing: a tenor is always a positive period, so an adjustment that leaves every date alone
 * cannot be expressed as a value of this type.
 */
object TenorAdjustment {

  /**
   * The wording reported when the tenor and the addition convention cannot be paired.
   *
   * This is the message the validator of the type being ported threw with, character for
   * character, so a caller that matched on the text of the original failure still matches. It
   * quotes nothing the caller supplied, so there is no input to bound or to neutralise in it.
   */
  private val MonthBasedTenorMessage: String =
    "Tenor must not contain days when addition convention is month-based"

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance that can adjust a date by the specified tenor.
   *
   * When adjusting a date, the specified tenor is added to the input date using the addition
   * convention supplied. The business day adjustment will then be used to ensure the result is a
   * valid business day.
   *
   * A month-based addition convention - one whose rule is expressed in terms of the month a date
   * falls in - requires a month-based tenor, and this is where that pairing is decided:
   *
   * {{{
   * TenorAdjustment.of(Tenor.TENOR_3M, PeriodAdditionConventions.LAST_DAY, adjustment)  // Right
   * TenorAdjustment.of(Tenor.TENOR_3M, PeriodAdditionConventions.NONE, adjustment)      // Right
   * TenorAdjustment.of(Tenor.TENOR_1W, PeriodAdditionConventions.NONE, adjustment)      // Right
   * TenorAdjustment.of(Tenor.TENOR_1W, PeriodAdditionConventions.LAST_DAY, adjustment)  // Left
   * }}}
   *
   * The last of those is rejected because a week-based tenor is not month-based: the end of a
   * month has no bearing on a period of seven days, so the pairing describes no rule anyone
   * meant. The test is [[Tenor.isMonthBased]] rather than a look at the tenor's day count, which
   * is what the validator being ported applied - so a tenor of weeks is rejected under a
   * month-based convention even though it contains whole weeks rather than loose days.
   *
   * @param tenor  the tenor to add to the input date
   * @param additionConvention  the convention used to perform the addition
   * @param adjustment  the business day adjustment to apply to the result of the addition
   * @return the tenor adjustment, or the reason the arguments do not describe one
   */
  def of(
      tenor: Tenor,
      additionConvention: PeriodAdditionConvention,
      adjustment: BusinessDayAdjustment): ResultNec[TenorAdjustment] =
    Validate.toResult(
      checkedPairing(tenor, additionConvention)
        .map(_ => new TenorAdjustment(tenor, additionConvention, adjustment) {}))

  /**
   * Obtains an instance that can adjust a date by the specified tenor using the last day of month
   * convention.
   *
   * When adjusting a date, the specified tenor is added to the input date. The business day
   * adjustment will then be used to ensure the result is a valid business day.
   *
   * The tenor must be month-based - it must consist only of months and/or years - because the
   * convention this factory applies is, which is checked exactly as in [[of]].
   *
   * @param tenor  the tenor to add to the input date
   * @param adjustment  the business day adjustment to apply to the result of the addition
   * @return the tenor adjustment, or the reason the arguments do not describe one
   */
  def ofLastDay(tenor: Tenor, adjustment: BusinessDayAdjustment): ResultNec[TenorAdjustment] =
    of(tenor, PeriodAdditionConventions.LAST_DAY, adjustment)

  /**
   * Obtains an instance that can adjust a date by the specified tenor using the last business day
   * of month convention.
   *
   * When adjusting a date, the specified tenor is added to the input date. The business day
   * adjustment will then be used to ensure the result is a valid business day.
   *
   * The tenor must be month-based - it must consist only of months and/or years - because the
   * convention this factory applies is, which is checked exactly as in [[of]].
   *
   * @param tenor  the tenor to add to the input date
   * @param adjustment  the business day adjustment to apply to the result of the addition
   * @return the tenor adjustment, or the reason the arguments do not describe one
   */
  def ofLastBusinessDay(tenor: Tenor, adjustment: BusinessDayAdjustment): ResultNec[TenorAdjustment] =
    of(tenor, PeriodAdditionConventions.LAST_BUSINESS_DAY, adjustment)

  //-------------------------------------------------------------------------
  /**
   * Checks that a month-based addition convention has been given a month-based tenor.
   *
   * The condition is the one the validator of the type being ported tested, written the same way
   * round, so the two reject exactly the same pairs. The check has nothing to return, since the
   * tenor and the convention it reads are both already in the caller's hands, so its outcome
   * carries `Unit` and combines with any further check of this type as any other value would -
   * this being, today, the whole validation surface of the type, there is nothing beside it to
   * combine with.
   *
   * @param tenor  the tenor to check against the convention
   * @param additionConvention  the convention the tenor has to suit
   * @return a passing outcome, or the failure the original threw with
   */
  private def checkedPairing(
      tenor: Tenor,
      additionConvention: PeriodAdditionConvention): ValidatedFailures[Unit] =
    Validate.isFalse(additionConvention.isMonthBased && !tenor.isMonthBased, MonthBasedTenorMessage)

  //-------------------------------------------------------------------------
  /**
   * The hashing and equality of adjustments.
   *
   * Taken from the `equals` and `hashCode` of the case class, which compare the three fields by
   * their own equality - the period and name of a tenor, the identity of an addition convention,
   * and the convention and calendar name of a business day adjustment. None of those fields holds
   * a `Double`, so there is no bit-pattern comparison to arrange. This is the type's only
   * equality-bearing instance, and `Eq[TenorAdjustment]` is obtained from it by subtyping rather
   * than declared separately. There is no `Order`: the bean being ported is not `Comparable`, and
   * an ordering over three unrelated parts would be this port's invention.
   *
   * @return the hashing of adjustments
   */
  implicit val hash: Hash[TenorAdjustment] = Hash.fromUniversalHashCode[TenorAdjustment]

  /**
   * The rendering of adjustments as text.
   *
   * Renders what [[TenorAdjustment.toString]] renders, which is the form of the library being
   * ported, so the two ways of putting an adjustment into a message agree.
   *
   * @return the rendering of an adjustment
   */
  implicit val show: Show[TenorAdjustment] = Show.show(_.toString)

  //-------------------------------------------------------------------------
  /**
   * The field shape of an adjustment in JSON, from which both halves of the codec are derived.
   *
   * The names, the order and the element codecs of the three fields are stated once, here, and
   * both the encoder and the decoder below are derived from this one declaration, which is what
   * keeps them from drifting apart. Deriving from [[TenorAdjustment]] itself is not possible - the
   * constructor of a checked type is not public, so there is no public shape to derive from - and
   * writing the fields out by hand twice instead would state the same contract two more times.
   *
   * @param tenor  the tenor, whose own codec carries it as its canonical text, `3M`
   * @param additionConvention  the addition convention, carried as its name, `LastDay`
   * @param adjustment  the business day adjustment, an object of its own two named fields
   */
  private final case class Raw(
      tenor: Tenor,
      additionConvention: PeriodAdditionConvention,
      adjustment: BusinessDayAdjustment)

  /** The derived decoder of the raw field shape, used by the checking decoder below. */
  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /** The derived encoder of the raw field shape, used by the encoder below. */
  private val rawEncoder: Encoder.AsObject[Raw] = deriveEncoder[Raw]

  /**
   * The JSON encoding of adjustments.
   *
   * The encoding is derived when this file is compiled, so no part of it inspects a class while
   * the program runs. An adjustment encodes as an object holding its three fields under the names
   * the Java bean declared, in declaration order:
   *
   * {{{
   * {"tenor":"3M","additionConvention":"LastDay",
   *  "adjustment":{"convention":"Following","calendar":"Sat/Sun"}}
   * }}}
   *
   * The tenor and the convention are written as bare strings by the codecs their own types
   * publish, and the business day adjustment as the object its own codec writes, so two equal
   * adjustments always encode to identical bytes.
   *
   * The result is wrapped so that a field holding no value would be omitted, which is the policy
   * every product of this port follows - this type has no optional field, so the wrapping changes
   * nothing about its output and exists so that the policy holds without exception.
   *
   * @return the JSON encoding of an adjustment
   */
  implicit val encoder: Encoder[TenorAdjustment] =
    Codecs.dropNulls(
      rawEncoder.contramap[TenorAdjustment](adjustment =>
        Raw(adjustment.tenor, adjustment.additionConvention, adjustment.adjustment)))

  /**
   * The JSON decoding of adjustments.
   *
   * This is the inverse of the encoding above and is likewise derived at compile time. All three
   * fields have to be present, and whether they describe an adjustment is decided exactly as a
   * caller's arguments are decided: the payload is read into the raw shape and handed to [[of]],
   * so a document pairing a month-based addition convention with a tenor that is not month-based
   * is a decoding failure carrying that reason rather than a value this type would not have built.
   *
   * @return the JSON decoding of an adjustment
   */
  implicit val decoder: Decoder[TenorAdjustment] =
    Codecs.validatedDecoder[Raw, TenorAdjustment](raw =>
      of(raw.tenor, raw.additionConvention, raw.adjustment))(rawDecoder)
}
