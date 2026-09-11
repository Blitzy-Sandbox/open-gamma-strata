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
 * An adjustment that alters a date if it falls on a day other than a business day.
 *
 * When processing dates in finance, it is typically intended that non-business days, such as
 * weekends and holidays, are converted to a nearby valid business day. This type represents the
 * necessary adjustment.
 *
 * It is a pair and nothing more: a [[BusinessDayConvention]], which decides the direction to move
 * in and how far, and a [[HolidayCalendarId]], which names the days to move off. None of the
 * date arithmetic lives here - it defers in full to the convention - so an adjustment is exactly
 * as expressive as the closed family of conventions is, and a reader who wants to know what a
 * given adjustment computes reads it there.
 *
 * ===Reference data is supplied, not looked up===
 *
 * The calendar is held as an identifier rather than as a calendar, which is what lets an
 * adjustment be written down, stored and passed around by code that has no holiday data to hand.
 * The data is supplied at the moment the adjustment is applied, so
 * [[com.opengamma.strata.basics.ReferenceData]] is a parameter of both [[adjust]] and
 * [[resolve]] and nothing is read from ambient state.
 *
 * Two forms are offered and they differ only in when the lookup happens. [[adjust]] resolves the
 * calendar and adjusts one date; [[resolve]] resolves the calendar once and returns a
 * [[DateAdjuster]] holding it, so a run of dates costs one lookup rather than one per date:
 *
 * {{{
 * val adjustment = BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.GBLO)
 * val one = adjustment.adjust(date, ReferenceData.standard)
 * val many = adjustment.resolve(ReferenceData.standard).map(adjuster => dates.map(adjuster.adjust))
 * }}}
 *
 * A resolved adjuster is bound to the calendar it was resolved against and does not follow later
 * changes to the reference data, which is the caveat [[com.opengamma.strata.basics.Resolvable]]
 * documents for every resolved form.
 *
 * `toReader` is inherited from `Resolvable` and needs no override here. It expresses the same
 * resolution as a value awaiting reference data, which is how several adjustments are composed
 * before any data is available; a reader for one particular date is that reader mapped over the
 * adjuster it produces:
 *
 * {{{
 * val fixing: RefDataReader[LocalDate] = adjustment.toReader.map(_.adjust(tradeDate))
 * }}}
 *
 * ===Failure is returned, not thrown===
 *
 * The Java original returned a bare date and threw `ReferenceDataNotFoundException` where the
 * calendar was absent from the reference data. Here both methods answer with
 * `Either[Failure, _]`, reporting `Failure.MissingData` naming the identifier that could not be
 * found, which is the failure [[HolidayCalendarId.resolve]] produces. Adjustment itself cannot
 * fail once the calendar is in hand: every convention answers for every date the calendar can
 * answer for.
 *
 * ===Construction===
 *
 * Construction is total. Both fields are required and neither can be absent, which the types
 * state on their own, so the Java bean's non-nullness checks have nothing left to check and the
 * ordinary case-class constructor is the whole of the validation. `apply`, `copy` and the
 * factory [[BusinessDayAdjustment.of]] are therefore all public and all equivalent; `of` is kept
 * because it is the name the library being ported used, so ported call sites read unchanged.
 *
 * This type is immutable and thread-safe.
 *
 * @param convention  the convention used to adjust the date if it does not fall on a business
 *   day, which determines whether to move forwards or backwards when it is a holiday
 * @param calendar  the identifier of the calendar that defines holidays and business days, which
 *   is resolved from reference data when the adjustment is applied
 * @see [[BusinessDayConvention]] for the rules an adjustment can apply
 * @see [[HolidayCalendarId]] for the identifier it resolves
 */
final case class BusinessDayAdjustment(
    convention: BusinessDayConvention,
    calendar: HolidayCalendarId)
    extends Resolvable[DateAdjuster] {

  /**
   * Adjusts the date as necessary if it is not a business day.
   *
   * If the date is a business day it is returned unaltered. If it is not, the rule of this
   * adjustment's convention is applied against the calendar this adjustment names, resolved from
   * the reference data supplied.
   *
   * @param date  the date to adjust
   * @param refData  the reference data, used to find the holiday calendar
   * @return the adjusted date, or `Left(Failure.MissingData)` where the reference data does not
   *   supply the calendar this adjustment names
   */
  def adjust(date: LocalDate, refData: ReferenceData): Either[Failure, LocalDate] =
    calendar.resolve(refData).map(holCal => convention.adjust(date, holCal))

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
   * @param refData  the reference data, used to find the holiday calendar
   * @return the adjuster bound to a specific holiday calendar, or `Left(Failure.MissingData)`
   *   where the reference data does not supply the calendar this adjustment names
   */
  override def resolve(refData: ReferenceData): Either[Failure, DateAdjuster] =
    calendar.resolve(refData).map(holCal => DateAdjuster(date => convention.adjust(date, holCal)))

  /**
   * Returns a string describing the adjustment.
   *
   * The adjustment that makes no adjustment at all renders as its convention alone - `NoAdjust` -
   * because naming a calendar that is never consulted would say something untrue about it. Every
   * other adjustment renders as its convention and the name of its calendar, as in
   * `ModifiedFollowing using calendar GBLO+USNY`. Both forms are those of the library being
   * ported, character for character, and they are what the `Show` instance renders.
   *
   * Note that an adjustment carrying the no-adjust convention and some other calendar renders
   * with that calendar, as the original did: it is not [[BusinessDayAdjustment.NONE]], since a
   * caller may later replace the convention and expect the calendar to still be there.
   *
   * @return the descriptive string
   */
  override def toString: String =
    if (this == BusinessDayAdjustment.NONE) {
      convention.toString
    } else {
      s"$convention using calendar ${calendar.name}"
    }
}

/**
 * Companion of [[BusinessDayAdjustment]], holding the no-adjustment constant, the factory and the
 * typeclass and JSON instances.
 */
object BusinessDayAdjustment {

  /**
   * An instance that performs no adjustment.
   *
   * It pairs the no-adjust convention with the no-holidays calendar identifier, so it returns
   * every date unaltered and is the identity of this type. It is the adjustment to use where a
   * date is already known to be acceptable and the surrounding structure requires an adjustment
   * all the same.
   *
   * The constant is built eagerly, which is safe because neither part of it is: the convention is
   * a case object and the identifier holds only its name, so nothing here generates a holiday
   * calendar or touches reference data.
   */
  val NONE: BusinessDayAdjustment =
    BusinessDayAdjustment(BusinessDayConventions.NO_ADJUST, HolidayCalendarIds.NO_HOLIDAYS)

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance using the specified convention and calendar.
   *
   * When adjusting a date, the convention's rule is applied using the calendar named here.
   *
   * This is the factory of the library being ported and is exactly the constructor of the type,
   * which construction being total leaves nothing for it to add. It is kept so that ported call
   * sites and the conventions built on them read as they did.
   *
   * @param convention  the convention used to adjust the date if it does not fall on a business
   *   day
   * @param calendar  the identifier of the calendar that defines holidays and business days
   * @return the adjustment
   */
  def of(convention: BusinessDayConvention, calendar: HolidayCalendarId): BusinessDayAdjustment =
    BusinessDayAdjustment(convention, calendar)

  //-------------------------------------------------------------------------
  /**
   * The hashing and equality of adjustments.
   *
   * Taken from the `equals` and `hashCode` of the case class, which compare the two fields by
   * their own equality - the identity of a convention and the name of a calendar identifier.
   * Neither field holds a `Double`, so there is no bit-pattern comparison to arrange. This is the
   * type's only equality-bearing instance, and `Eq[BusinessDayAdjustment]` is obtained from it by
   * subtyping rather than declared separately. There is no `Order`: the bean being ported is not
   * `Comparable`, and an ordering of conventions and calendars would be this port's invention.
   *
   * @return the hashing of adjustments
   */
  implicit val hash: Hash[BusinessDayAdjustment] = Hash.fromUniversalHashCode[BusinessDayAdjustment]

  /**
   * The rendering of adjustments as text.
   *
   * Renders what `toString` renders, which is the form of the Java original, so the two ways of
   * putting an adjustment into a message agree.
   *
   * @return the rendering of an adjustment
   */
  implicit val show: Show[BusinessDayAdjustment] = Show.show(_.toString)

  //-------------------------------------------------------------------------
  /**
   * The JSON encoding of adjustments.
   *
   * The encoding is derived when this file is compiled, so no part of it inspects a class while
   * the program runs. An instance encodes as an object holding its two fields under the names the
   * Java bean declared, in declaration order:
   *
   * {{{
   * {"convention":"ModifiedFollowing","calendar":"GBLO+USNY"}
   * }}}
   *
   * Both fields are written as bare strings by the codecs their own types publish - the canonical
   * name of the convention and the normalised name of the calendar identifier - so the document
   * is the one the library being ported wrote, and two adjustments that are equal encode to
   * identical bytes whatever order their composite calendar was built in.
   *
   * Neither field is optional, so there is no absent value to drop; the encoder is wrapped in the
   * single policy of this port for products all the same, so that the rule holds of every product
   * encoder without a reader having to check which products have optional fields today.
   *
   * @return the JSON encoding of an adjustment
   */
  implicit val encoder: Encoder[BusinessDayAdjustment] =
    Codecs.dropNulls(deriveEncoder[BusinessDayAdjustment])

  /**
   * The JSON decoding of adjustments.
   *
   * This is the inverse of the encoding above and is likewise derived at compile time. Both fields
   * have to be present. The convention is resolved by the name lookup of its own closed family,
   * which accepts every spelling that family accepts and rejects anything else; the calendar
   * identifier accepts any name, including a composite one and one this library knows nothing
   * about, and fails - if at all - when it is resolved, which is where a missing calendar belongs.
   *
   * Construction cannot fail, so nothing beyond the shape of the payload is checked here: a
   * payload of the right shape always yields an adjustment, and an encoded adjustment decodes
   * back to one equal to it.
   *
   * @return the JSON decoding of an adjustment
   */
  implicit val decoder: Decoder[BusinessDayAdjustment] = deriveDecoder[BusinessDayAdjustment]
}
