/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate

import cats.Hash
import cats.Show
import cats.data.Kleisli

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.basics.RefDataReader
import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * An adjustable date.
 *
 * This type pairs a date that may fall on any day of the calendar with the
 * [[BusinessDayAdjustment]] needed to move it to a business day. It is the form a date takes in
 * a term sheet: a payment is agreed for the first of the month, and the parties have separately
 * agreed what is to happen in those months whose first day is a Saturday. Nothing is adjusted
 * until [[adjusted]] is called, so the date as agreed and the date as it will settle are both
 * recoverable from one value, and the adjustment is applied against the holiday data of the
 * moment rather than against whatever data happened to be loaded when the value was built.
 *
 * A date needing no adjustment at all is still an adjustable date - one carrying
 * [[BusinessDayAdjustment.NONE]] - so a structure holding adjustable dates does not need a
 * second shape for the dates that are already acceptable. The one-argument
 * `AdjustableDate.of` builds exactly that, and its [[adjusted]] returns the date it was given.
 *
 * ===Reference data is supplied, not looked up===
 *
 * The adjustment names its holiday calendar by identifier rather than holding the calendar, so
 * an adjustable date can be written down, stored and passed around by code that holds no
 * holiday data at all. The data is supplied at the moment of adjustment, which is why
 * [[com.opengamma.strata.basics.ReferenceData]] is a parameter of [[adjusted]] and why nothing
 * here reads ambient state:
 *
 * {{{
 * val adjustment = BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.GBLO)
 * val payment = AdjustableDate.of(LocalDate.of(2014, 7, 12), adjustment)
 * val settles = payment.adjusted(ReferenceData.standard)
 * }}}
 *
 * Where the data is not yet in hand, [[toReader]] expresses the same adjustment as a value
 * awaiting it. Such a value composes with `map`, `flatMap` and `mapN` while the data is still
 * unknown, and is run once against the data actually available:
 *
 * {{{
 * import cats.syntax.apply._
 *
 * val start = AdjustableDate.of(LocalDate.of(2014, 7, 12), adjustment)
 * val end = AdjustableDate.of(LocalDate.of(2015, 7, 12), adjustment)
 * val both = (start.toReader, end.toReader).tupled.run(ReferenceData.standard)
 * }}}
 *
 * Both forms resolve the calendar each time they are run, which is worth a thought for a long
 * run of dates sharing one adjustment, and more so where the calendar is composite - `GBLO+USNY`,
 * say - since resolving such an identifier reads each of its parts and combines the results.
 * A caller with many dates and one adjustment should resolve that adjustment once, through
 * `BusinessDayAdjustment.resolve`, and put the dates through the [[DateAdjuster]] it returns;
 * the methods here are for the single date and for the caller composing a handful of them.
 *
 * ===Failure is returned, not thrown===
 *
 * The Java original returned a bare date and threw `ReferenceDataNotFoundException` where the
 * calendar was absent from the reference data. Here [[adjusted]] answers with
 * `Either[Failure, LocalDate]`, reporting the `Failure.MissingData` that
 * [[HolidayCalendarId.resolve]] produces and naming the identifier that could not be found.
 *
 * That is the only way adjusting can fail. The date held here is unconstrained - any date the
 * calendar system can express is a legitimate unadjusted date, including a weekend, a holiday
 * and the 29th of February - and once the calendar is in hand every convention answers for
 * every date, so there is nothing else to report.
 *
 * ===Construction===
 *
 * Construction is total. Both parts are required and neither can be absent, which the types
 * state on their own, so the Java bean's non-nullness checks have nothing left to check and the
 * ordinary case-class constructor is the whole of the validation. `apply`, `copy` and both
 * `AdjustableDate.of` factories are therefore public and equivalent; the factories are kept
 * because they are the names the library being ported used, so ported call sites read
 * unchanged.
 *
 * This type is immutable and thread-safe.
 *
 * @param unadjusted  the unadjusted date, which may be a non-business day; the business day
 *   adjustment is what ensures a business day is produced
 * @param adjustment  the business day adjustment to apply to the unadjusted date, which is
 *   [[BusinessDayAdjustment.NONE]] where the date is to be taken as it stands
 * @see [[BusinessDayAdjustment]] for the adjustment this date carries
 * @see [[HolidayCalendarId]] for the identifier that adjustment resolves
 */
final case class AdjustableDate(
    unadjusted: LocalDate,
    adjustment: BusinessDayAdjustment) {

  /**
   * Adjusts the date using the business day adjustment.
   *
   * This returns the adjusted date, calculated by applying this date's business day adjustment
   * to its unadjusted date. A date that is already a business day of the adjustment's calendar
   * is returned unaltered, as is every date where the adjustment is
   * [[BusinessDayAdjustment.NONE]]; otherwise the convention's rule decides which business day
   * to move to.
   *
   * The calendar is resolved from the reference data supplied on every call, so the answer
   * follows the data given rather than any data captured earlier.
   *
   * @param refData  the reference data to use, which supplies the holiday calendar the
   *   adjustment names
   * @return the adjusted date, or `Left(Failure.MissingData)` where the reference data does not
   *   supply the calendar the adjustment names
   */
  def adjusted(refData: ReferenceData): Either[Failure, LocalDate] =
    adjustment.adjust(unadjusted, refData)

  /**
   * Expresses the adjustment of this date as a function awaiting reference data.
   *
   * [[adjusted]] needs its reference data at the moment it is called. This method returns the
   * same adjustment as a value - a `Kleisli` over `FailureOr` - so that several adjustments and
   * resolutions can be composed while the data is still unknown, the composed reader being run
   * once against the data actually available:
   *
   * {{{
   * import cats.syntax.apply._
   *
   * val dates = (first.toReader, second.toReader).tupled.run(ReferenceData.standard)
   * }}}
   *
   * The import is part of the example: `tupled` is `cats` syntax on the pair of readers rather
   * than a member of either, so the composition above does not compile without it.
   *
   * The reader delegates to [[adjusted]] and so shares its cost: the calendar is resolved each
   * time the reader is run, not once when it is built.
   *
   * The `Kleisli` type arguments are spelled out rather than inferred, over the
   * single-parameter `FailureOr` alias. That alias exists for this position: the build carries
   * no compiler plugin supplying type-lambda syntax, so a failure type applied at the use site
   * could not be written here at all.
   *
   * @return the adjustment of this date as a function from reference data to the adjusted date
   */
  def toReader: RefDataReader[LocalDate] =
    Kleisli[FailureOr, ReferenceData, LocalDate](adjusted)

  /**
   * Returns a string describing the adjustable date.
   *
   * A date carrying [[BusinessDayAdjustment.NONE]] renders as the date alone - `2014-07-11` -
   * because naming an adjustment that adjusts nothing would say something untrue about it.
   * Every other adjustable date renders as its date, the words ` adjusted by ` and its
   * adjustment, as in `2014-07-11 adjusted by Following using calendar Sat/Sun`. Both forms are
   * those of the library being ported, character for character, and they are what the `Show`
   * instance renders.
   *
   * The test is on the value of the adjustment rather than on how this date was built, so a
   * date built with the two-argument factory and the no-adjustment constant renders in the
   * short form, exactly as the original did.
   *
   * @return the descriptive string
   */
  override def toString: String =
    if (adjustment == BusinessDayAdjustment.NONE) {
      unadjusted.toString
    } else {
      s"$unadjusted adjusted by $adjustment"
    }
}

/**
 * Companion of [[AdjustableDate]], holding the factories and the typeclass and JSON instances.
 */
object AdjustableDate {

  /**
   * Obtains an instance with no business day adjustment.
   *
   * This creates an adjustable date from the specified date. No business day adjustment
   * applies, so [[AdjustableDate.adjusted]] returns the date given here whatever reference
   * data it is run against and whether or not that date is a business day anywhere.
   *
   * This is the factory of the library being ported. It is kept so that ported call sites read
   * as they did, and because the intent it states - a date that is to be taken as it stands -
   * reads better than the constructor paired with the no-adjustment constant.
   *
   * @param date  the date, which is taken as both the unadjusted and the adjusted date
   * @return the adjustable date
   */
  def of(date: LocalDate): AdjustableDate =
    AdjustableDate(date, BusinessDayAdjustment.NONE)

  /**
   * Obtains an instance with a business day adjustment.
   *
   * This creates an adjustable date from the unadjusted date and the business day adjustment
   * to apply to it. The adjusted date is reached through [[AdjustableDate.adjusted]], which
   * needs reference data because the adjustment names its holiday calendar rather than holding
   * it.
   *
   * This is the factory of the library being ported and is exactly the constructor of the type,
   * which construction being total leaves nothing for it to add. It is kept so that ported call
   * sites read as they did.
   *
   * @param unadjusted  the unadjusted date, which may be a non-business day
   * @param adjustment  the business day adjustment to apply to the unadjusted date
   * @return the adjustable date
   */
  def of(unadjusted: LocalDate, adjustment: BusinessDayAdjustment): AdjustableDate =
    AdjustableDate(unadjusted, adjustment)

  //-------------------------------------------------------------------------
  /**
   * The hashing and equality of adjustable dates.
   *
   * Taken from the `equals` and `hashCode` of the case class, which compare the two fields by
   * their own equality - the value of a date and the convention and calendar name of an
   * adjustment. Neither field holds a `Double`, so there is no bit-pattern comparison to
   * arrange, and the structural equality the compiler writes is exactly the equality of the
   * bean being ported.
   *
   * Two dates that differ only in their adjustment are therefore unequal even where both adjust
   * to the same day, as they were in the original: what is held is the agreement, and two
   * different agreements that happen to agree today are still two agreements.
   *
   * This is the type's only equality-bearing instance, and `Eq[AdjustableDate]` is obtained
   * from it by subtyping rather than declared separately. There is no `Order`: the bean being
   * ported is not `Comparable`, and ordering by the unadjusted date alone - the only ordering
   * that could be meant - would rank two dates equal that are not.
   *
   * @return the hashing of adjustable dates
   */
  implicit val hash: Hash[AdjustableDate] = Hash.fromUniversalHashCode[AdjustableDate]

  /**
   * The rendering of adjustable dates as text.
   *
   * Renders what [[AdjustableDate.toString]] renders, which is the form of the Java original,
   * so the two ways of putting an adjustable date into a message agree.
   *
   * @return the rendering of an adjustable date
   */
  implicit val show: Show[AdjustableDate] = Show.show(_.toString)

  //-------------------------------------------------------------------------
  /**
   * The JSON encoding of adjustable dates.
   *
   * The encoding is derived when this file is compiled, so no part of it inspects a class while
   * the program runs. An instance encodes as an object holding its two fields under the names
   * the Java bean declared, in declaration order:
   *
   * {{{
   * {"unadjusted":"2024-01-31","adjustment":{"convention":"Following","calendar":"GBLO"}}
   * }}}
   *
   * The date is an ISO-8601 string, written by the codec `circe` itself publishes for
   * `java.time.LocalDate`; the adjustment is the object its own codec writes, whose two fields
   * are in turn bare strings. Nothing here is optional, so no absent value falls out; the
   * encoder is wrapped in the single policy of this port for products all the same, so that the
   * rule holds of every product encoder without a reader having to check which products have
   * optional fields today.
   *
   * Two adjustable dates that are equal encode to identical bytes, since neither field has a
   * representation that depends on how it was built.
   *
   * @return the JSON encoding of an adjustable date
   */
  implicit val encoder: Encoder[AdjustableDate] =
    Codecs.dropNulls(deriveEncoder[AdjustableDate])

  /**
   * The JSON decoding of adjustable dates.
   *
   * This is the inverse of the encoding above and is likewise derived at compile time. Both
   * fields have to be present, and each is read by the decoder its own type publishes: any date
   * the ISO-8601 calendar date form can express is accepted, as is any adjustment whose
   * convention is a member of that closed family. A calendar this library knows nothing about
   * is accepted here and fails - if at all - when the date is adjusted, which is where a
   * missing calendar belongs.
   *
   * Construction cannot fail, so nothing beyond the shape of the payload is checked: a payload
   * of the right shape always yields an adjustable date, and an encoded adjustable date decodes
   * back to one equal to it.
   *
   * @return the JSON decoding of an adjustable date
   */
  implicit val decoder: Decoder[AdjustableDate] = deriveDecoder[AdjustableDate]
}
