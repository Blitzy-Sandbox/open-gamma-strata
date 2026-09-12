/*
 * Copyright (C) 2021 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate

import cats.Hash
import cats.Show
import cats.data.Kleisli
import cats.data.NonEmptyList
import cats.syntax.apply._

import _root_.io.circe.Decoder
import _root_.io.circe.Encoder
import _root_.io.circe.generic.semiauto.deriveDecoder
import _root_.io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.basics.RefDataReader
import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.ValidatedFailures
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * An adjustable list of dates.
 *
 * This type combines a run of unadjusted dates with the single [[BusinessDayAdjustment]] needed
 * to move each of them to a business day. It is [[AdjustableDate]] for a whole set of dates that
 * share one adjustment - the fixing dates of a schedule, the exercise dates of a swaption, the
 * dates named in a term sheet as a list - and it is one value rather than a list of values
 * because the adjustment is a property of the set: naming it once says that these dates are
 * adjusted the same way, which a list of separately adjusted dates only implies.
 *
 * Nothing is adjusted until [[adjusted]] is called, so the dates as agreed and the dates as they
 * will settle are both recoverable from one value, and the adjustment is applied against the
 * holiday data of the moment rather than against whatever data happened to be loaded when the
 * value was built. Where the individual dates are wanted as values in their own right,
 * [[toAdjustableDateList]] hands back exactly that.
 *
 * ===The dates are ordered and distinct===
 *
 * The unadjusted dates are strictly increasing: they are in order and no date appears twice.
 * This is the one invariant of the type and it is checked by every factory, so a value of this
 * type never holds a run of dates that doubles back on itself or names the same day twice. The
 * order is the order a caller supplied, not a sorting of it - a caller holding dates in some
 * other order sorts them before calling, which keeps the rejection meaningful rather than
 * silently accepting input that was not the run it appeared to be.
 *
 * The dates being distinct '''before''' adjustment does not make them distinct after it: two
 * dates a weekend apart adjust onto the same Monday under a following convention. [[adjusted]]
 * therefore removes duplicates from its result, which is the behaviour of the library being
 * ported, so the list it returns can be shorter than [[unadjusted]] and is the set of business
 * days these dates fall on rather than one date per unadjusted date. A caller that needs the
 * correspondence between the two instead of the set adjusts the members of
 * [[toAdjustableDateList]] individually.
 *
 * Since there is always at least one date, [[unadjusted]] is a `cats.data.NonEmptyList` and so
 * are the results of [[adjusted]] and [[toAdjustableDateList]]: the invariant is carried by the
 * type rather than restated as a check by everything that reads one, and `head` is total on all
 * three.
 *
 * ===Reference data is supplied, not looked up===
 *
 * The adjustment names its holiday calendar by identifier rather than holding the calendar, so
 * adjustable dates can be written down, stored and passed around by code that holds no holiday
 * data at all. The data is supplied at the moment of adjustment, which is why
 * [[com.opengamma.strata.basics.ReferenceData]] is a parameter of [[adjusted]] and why nothing
 * here reads ambient state:
 *
 * {{{
 * val adjustment = BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.GBLO)
 * val fixings = AdjustableDates.of(adjustment, List(LocalDate.of(2014, 7, 11), LocalDate.of(2014, 7, 13)))
 * val settling = fixings.flatMap(_.adjusted(ReferenceData.standard))
 * }}}
 *
 * The calendar is resolved once per call to [[adjusted]] and every date of the run is put through
 * the resolved [[DateAdjuster]], so a long run of dates costs one lookup rather than one per
 * date. That is the reason this type adjusts its own dates instead of leaving a caller to map
 * [[AdjustableDate.adjusted]] over [[toAdjustableDateList]], which would resolve the calendar
 * once per date and matters most where the calendar is composite - `GBLO+USNY`, say - since
 * resolving such an identifier reads each of its parts and combines the results.
 *
 * Where the data is not yet in hand, [[toReader]] expresses the same adjustment as a value
 * awaiting it. Such a value composes with `map`, `flatMap` and `mapN` while the data is still
 * unknown, and is run once against the data actually available:
 *
 * {{{
 * import cats.syntax.apply._
 *
 * val both = (fixingDates.toReader, paymentDates.toReader).tupled.run(ReferenceData.standard)
 * }}}
 *
 * ===Failure is returned, not thrown===
 *
 * Construction and adjustment both report their failures as values. [[AdjustableDates.of]]
 * answers with `ResultNec[AdjustableDates]`, reporting `Failure.Invalid` for dates that are not
 * strictly increasing and for a collection holding no date, where the bean being ported raised an
 * argument exception from its validator and a property exception from its constructor.
 * [[adjusted]] answers with `Either[Failure, NonEmptyList[LocalDate]]`, reporting the
 * `Failure.MissingData` that [[HolidayCalendarId.resolve]] produces where the reference data does
 * not supply the calendar the adjustment names.
 *
 * That is the whole of what can fail. Once the calendar is in hand every convention answers for
 * every date, so adjustment itself has nothing further to report.
 *
 * This type is immutable and thread-safe.
 *
 * @param unadjusted  the unadjusted dates, in increasing order and without duplicates, which may
 *   be non-business days; the business day adjustment is what ensures business days are produced
 * @param adjustment  the business day adjustment to apply to each unadjusted date, which is
 *   [[BusinessDayAdjustment.NONE]] where the dates are to be taken as they stand
 * @see [[AdjustableDate]] for the single-date form of the same pairing
 * @see [[BusinessDayAdjustment]] for the adjustment these dates carry
 */
sealed abstract case class AdjustableDates private (
    unadjusted: NonEmptyList[LocalDate],
    adjustment: BusinessDayAdjustment) {

  /**
   * Adjusts the dates using the business day adjustment.
   *
   * This returns the adjusted dates, calculated by applying this value's business day adjustment
   * to each of its unadjusted dates. A date that is already a business day of the adjustment's
   * calendar is returned unaltered, as is every date where the adjustment is
   * [[BusinessDayAdjustment.NONE]]; otherwise the convention's rule decides which business day to
   * move to.
   *
   * '''Duplicates are removed''', as they were in the library being ported: the unadjusted dates
   * are distinct, but two of them can adjust onto the same business day, and the result is the
   * distinct business days in the order they first occur. The list returned can therefore be
   * shorter than [[unadjusted]], and is never empty, since at least one date survives.
   *
   * The calendar is resolved from the reference data supplied on every call, once for the whole
   * run, so the answer follows the data given rather than any data captured earlier.
   *
   * {{{
   * // a Saturday and the Sunday after it, under a following convention over a Sat/Sun calendar
   * dates.adjusted(ReferenceData.standard)  // Right(NonEmptyList(the one Monday))
   * }}}
   *
   * @param refData  the reference data to use, which supplies the holiday calendar the adjustment
   *   names
   * @return the distinct adjusted dates in increasing order, or `Left(Failure.MissingData)` where
   *   the reference data does not supply the calendar the adjustment names
   */
  def adjusted(refData: ReferenceData): Either[Failure, NonEmptyList[LocalDate]] =
    adjustment
      .resolve(refData)
      .map(adjuster => AdjustableDates.distinctDates(unadjusted.map(adjuster.adjust)))

  /**
   * Returns the dates as a list of [[AdjustableDate]], each carrying this value's adjustment.
   *
   * The result is this value taken apart: one adjustable date per unadjusted date, in the same
   * order, every one of them pairing that date with the single adjustment held here. It is the
   * form to use where the dates are to be handled one at a time, or where the correspondence
   * between an unadjusted date and its adjusted date has to survive - which [[adjusted]] does not
   * preserve, since it removes duplicates.
   *
   * Adjusting the members of this list individually resolves the calendar once per date, so
   * [[adjusted]] remains the cheaper route where only the dates are wanted.
   *
   * @return the adjustable dates, one per unadjusted date, in increasing order of unadjusted date
   */
  def toAdjustableDateList: NonEmptyList[AdjustableDate] =
    unadjusted.map(date => AdjustableDate.of(date, adjustment))

  /**
   * Expresses the adjustment of these dates as a function awaiting reference data.
   *
   * [[adjusted]] needs its reference data at the moment it is called. This method returns the
   * same adjustment as a value - a `Kleisli` over `FailureOr` - so that several adjustments and
   * resolutions can be composed while the data is still unknown, the composed reader being run
   * once against the data actually available:
   *
   * {{{
   * import cats.syntax.apply._
   *
   * val dates = (fixingDates.toReader, paymentDates.toReader).tupled.run(ReferenceData.standard)
   * }}}
   *
   * The import is part of the example: `tupled` is `cats` syntax on the pair of readers rather
   * than a member of either, so the composition above does not compile without it.
   *
   * The reader delegates to [[adjusted]] and so shares both its cost - the calendar is resolved
   * each time the reader is run, not once when it is built - and its de-duplication.
   *
   * The `Kleisli` type arguments are spelled out rather than inferred, over the single-parameter
   * `FailureOr` alias. That alias exists for this position: the build carries no compiler plugin
   * supplying type-lambda syntax, so a failure type applied at the use site could not be written
   * here at all.
   *
   * @return the adjustment of these dates as a function from reference data to the adjusted dates
   */
  def toReader: RefDataReader[NonEmptyList[LocalDate]] =
    Kleisli[FailureOr, ReferenceData, NonEmptyList[LocalDate]](adjusted)

  /**
   * Returns a string describing the adjustable dates.
   *
   * Dates carrying [[BusinessDayAdjustment.NONE]] render as the bracketed list alone -
   * `[2014-07-11, 2014-07-13]` - because naming an adjustment that adjusts nothing would say
   * something untrue about it. Every other value renders as that list, the words ` adjusted by `
   * and its adjustment, as in `[2014-07-11, 2014-07-13] adjusted by Following using calendar
   * Sat/Sun`. Both forms are those of the library being ported, character for character, and they
   * are what the `Show` instance renders.
   *
   * The bracketed form is written out here rather than taken from the list, because the standard
   * library renders a list as `List(...)` while the collection of the library being ported
   * rendered it as `[...]`, and it is the latter that callers, stored text and the ported tests
   * read.
   *
   * The test is on the value of the adjustment rather than on how these dates were built, so a
   * value built with an explicit no-adjustment constant renders in the short form, exactly as the
   * original did.
   *
   * @return the descriptive string
   */
  override def toString: String = {
    val dates = unadjusted.toList.mkString("[", ", ", "]")
    if (adjustment == BusinessDayAdjustment.NONE) {
      dates
    } else {
      s"$dates adjusted by $adjustment"
    }
  }
}

/**
 * Provides the ways of obtaining a run of adjustable dates, and the instances for the type.
 *
 * This companion is the only place an [[AdjustableDates]] is created. Neither the constructor nor
 * a generated `apply` or `copy` is available and the type is sealed, so a value holding dates that
 * are out of order, dates that repeat, or no dates at all cannot exist: the factories that read
 * such input report it instead.
 *
 * Five factories are published. Four of them are those of the library being ported - a first date
 * and further dates, or a list, with or without an adjustment - and the fifth takes a
 * `NonEmptyList`, which is the shape the type holds and the shape the other four funnel into.
 * The two that take a first date and further dates cannot be given an empty run, since the first
 * date is a parameter of its own, so they reach the check for order and nothing else; the two
 * taking a list are the only ones that can be handed a collection holding nothing.
 *
 * Two typeclass instances are published, and exactly two: a `Hash`, which is the single
 * equality-bearing instance of the type - `Hash` extends `Eq`, so declaring an `Eq` as well would
 * leave two instances that could disagree and one of them ambiguous - and a `Show`. There is
 * deliberately no `Order`, matching the type being ported, which is not comparable.
 *
 * @see [[AdjustableDates]] for the type itself and for what a value of it holds
 */
object AdjustableDates {

  /**
   * The name of the collection of dates, as the argument name of the check that rejects it.
   *
   * This is the name that appears in `Argument iterable 'unadjusted' must not be empty`, the
   * message reported for a collection holding no date, and it is the name the bean being ported
   * gave the same property, so the message names what the caller passed.
   */
  private val UnadjustedField: String = "unadjusted"

  /**
   * The message reported for dates that are not strictly increasing.
   *
   * This is the wording of the validator of the bean being ported, character for character, and
   * it covers both ways the run can be wrong - a date that is out of order and a date that
   * repeats - because a single test of strict ordering rejects both and reporting them separately
   * would split one condition into two reasons that no caller distinguishes.
   */
  private val OrderMessage: String = "Dates must be in order and without duplicates"

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance with no business day adjustment.
   *
   * This creates an instance from the specified dates. No business day adjustment applies, so
   * [[AdjustableDates.adjusted]] returns the dates given here whatever reference data it is run
   * against and whether or not they are business days anywhere.
   *
   * The dates have to be strictly increasing, which is the one condition this factory reports;
   * there is always at least one of them, since the first is a parameter of its own.
   *
   * {{{
   * AdjustableDates.of(LocalDate.of(2014, 7, 11), LocalDate.of(2014, 7, 13))
   * // Right([2014-07-11, 2014-07-13])
   * }}}
   *
   * @param firstDate  the first date
   * @param remainingDates  the remaining dates, in increasing order and all after the first
   * @return the adjustable dates, or the failure describing why the dates describe none
   */
  def of(firstDate: LocalDate, remainingDates: LocalDate*): ResultNec[AdjustableDates] =
    of(BusinessDayAdjustment.NONE, NonEmptyList(firstDate, remainingDates.toList))

  /**
   * Obtains an instance with no business day adjustment.
   *
   * This creates an instance from the specified dates. No business day adjustment applies, so
   * [[AdjustableDates.adjusted]] returns the dates given here whatever reference data it is run
   * against and whether or not they are business days anywhere.
   *
   * The list has to hold at least one date and the dates have to be strictly increasing; both
   * conditions are reported as failures.
   *
   * {{{
   * AdjustableDates.of(List(LocalDate.of(2014, 7, 11)))  // Right([2014-07-11])
   * AdjustableDates.of(Nil)                              // Left(the emptiness failure)
   * }}}
   *
   * @param dates  the dates, at least one, in increasing order and without duplicates
   * @return the adjustable dates, or the failures describing why the dates describe none
   */
  def of(dates: List[LocalDate]): ResultNec[AdjustableDates] =
    of(BusinessDayAdjustment.NONE, dates)

  /**
   * Obtains an instance with a business day adjustment.
   *
   * This creates an instance from the unadjusted dates and the business day adjustment to apply
   * to each of them. The adjusted dates are reached through [[AdjustableDates.adjusted]], which
   * needs reference data because the adjustment names its holiday calendar rather than holding
   * it.
   *
   * The dates have to be strictly increasing, which is the one condition this factory reports;
   * there is always at least one of them, since the first is a parameter of its own.
   *
   * @param adjustment  the business day adjustment to apply to each unadjusted date
   * @param firstDate  the first date
   * @param remainingDates  the remaining dates, in increasing order and all after the first
   * @return the adjustable dates, or the failure describing why the dates describe none
   */
  def of(
      adjustment: BusinessDayAdjustment,
      firstDate: LocalDate,
      remainingDates: LocalDate*): ResultNec[AdjustableDates] =
    of(adjustment, NonEmptyList(firstDate, remainingDates.toList))

  /**
   * Obtains an instance with a business day adjustment.
   *
   * This creates an instance from the unadjusted dates and the business day adjustment to apply
   * to each of them. The adjusted dates are reached through [[AdjustableDates.adjusted]], which
   * needs reference data because the adjustment names its holiday calendar rather than holding
   * it.
   *
   * The list has to hold at least one date and the dates have to be strictly increasing:
   *
   * {{{
   * AdjustableDates.of(adjustment, List(fri, sun))  // Right([2014-07-11, 2014-07-13] adjusted by ...)
   * AdjustableDates.of(adjustment, List(sun, fri))  // Left(the ordering failure)
   * AdjustableDates.of(adjustment, List(fri, fri))  // Left(the ordering failure - duplicates)
   * AdjustableDates.of(adjustment, Nil)             // Left(the emptiness failure)
   * }}}
   *
   * The two checks are combined rather than sequenced, so the outcome is the shape every checked
   * type of this port has and a caller reads one chain of reasons. In practice the two are
   * mutually exclusive - a collection holding no date holds no pair of dates to be out of order -
   * so the chain holds one failure; combining them is what keeps that a property of these
   * particular checks rather than of the code that reports them.
   *
   * @param adjustment  the business day adjustment to apply to each unadjusted date
   * @param dates  the dates, at least one, in increasing order and without duplicates
   * @return the adjustable dates, or the failures describing why the dates describe none
   */
  def of(adjustment: BusinessDayAdjustment, dates: List[LocalDate]): ResultNec[AdjustableDates] =
    (Validate.notEmpty(dates, UnadjustedField), checkedOrder(dates))
      .mapN((checked, _) =>
        // reached only when the emptiness check passed, so the collection holds at least one
        // date and the first of them is the head of the run
        create(NonEmptyList(checked.head, checked.drop(1).toList), adjustment))
      .toEither

  /**
   * Obtains an instance with a business day adjustment from a non-empty list of dates.
   *
   * This creates an instance from the unadjusted dates and the business day adjustment to apply
   * to each of them, and is the factory the other four funnel into: the dates arrive in the shape
   * the type holds, so the only condition left to report is that they are strictly increasing.
   *
   * It is published rather than kept private because a caller that already holds a
   * `NonEmptyList` - the schedule and adjustment types of this library routinely do - should not
   * have to flatten it into a list and have its emptiness reconsidered.
   *
   * @param adjustment  the business day adjustment to apply to each unadjusted date
   * @param dates  the dates, in increasing order and without duplicates
   * @return the adjustable dates, or the failure describing why the dates describe none
   */
  def of(
      adjustment: BusinessDayAdjustment,
      dates: NonEmptyList[LocalDate]): ResultNec[AdjustableDates] =
    checkedOrder(dates.toList).map(_ => create(dates, adjustment)).toEither

  //-------------------------------------------------------------------------
  /**
   * Checks that the dates are strictly increasing.
   *
   * A run of dates is strictly increasing when each date is before the one after it, which
   * rejects both a date that is out of order and a date that repeats. This reads exactly as the
   * validator of the bean being ported read, and reports the same wording.
   *
   * There is nothing worth returning from it - the dates are already in the caller's hands - so
   * its outcome carries `Unit`, which combines with further checks exactly as any other value
   * would. This is the whole validation surface of the type beyond the emptiness of a collection:
   * both fields are required, which their types state on their own, and nothing else about them
   * can be wrong.
   *
   * @param dates  the dates to check, which may be empty, an empty run being vacuously increasing
   * @return a passing outcome, or the failure describing the run that was rejected
   */
  private def checkedOrder(dates: List[LocalDate]): ValidatedFailures[Unit] =
    Validate.isTrue(strictlyIncreasing(dates), OrderMessage)

  /**
   * Whether each date of the run is strictly before the one after it.
   *
   * The run is paired with itself shifted by one and every pair is tested, which is the whole of
   * the condition. The pairing is lazy, so no intermediate collection of pairs is built, and
   * dropping the first element of a list shares the rest of it rather than copying; the test
   * therefore reads each date once and stops at the first pair that is out of order.
   *
   * A run of one date and an empty run hold no pair and are both increasing, which is what makes
   * the emptiness of a collection a separate check rather than a consequence of this one.
   *
   * @param dates  the dates to test
   * @return true if every date is strictly before its successor
   */
  private def strictlyIncreasing(dates: List[LocalDate]): Boolean =
    dates.lazyZip(dates.drop(1)).forall((earlier, later) => earlier.isBefore(later))

  /**
   * Removes the dates that repeat, keeping the first occurrence of each.
   *
   * This is the de-duplication [[AdjustableDates.adjusted]] performs, which the library being
   * ported performed with a distinct stream. It is expressed over the head and the tail of the
   * run rather than over the flattened list so that the result is non-empty by construction
   * rather than by a check: the head always survives, and the tail contributes its own first
   * occurrences other than the head, which together are exactly the first occurrences of the
   * whole run in their original order.
   *
   * @param dates  the dates to de-duplicate, in the order they were produced
   * @return the distinct dates, in the order of first occurrence
   */
  private def distinctDates(dates: NonEmptyList[LocalDate]): NonEmptyList[LocalDate] =
    NonEmptyList(dates.head, dates.tail.distinct.filter(date => date != dates.head))

  /**
   * Creates a value, which every route into the type funnels through.
   *
   * This is the only instantiation of the type and it is private, so the factories above are the
   * only ways into it from outside this file. The constructor of a `sealed abstract case class`
   * is reachable only from inside the file that declares it, and `new AdjustableDates(...) {}` -
   * an anonymous subclass of the abstract case class - is how it is reached; that is what leaves
   * the type without a public `apply` or `copy` while keeping the `equals`, `hashCode` and
   * `unapply` a case class provides.
   *
   * The method performs no check of its own, and each of its callers has already established the
   * invariant through [[checkedOrder]]. This type has no operation that derives one value from
   * another - no `with` method, no arithmetic, and the two reading methods return dates rather
   * than values of this type - so there is no further route by which an unchecked run could
   * arrive here, which is why the check belongs in the factories rather than being repeated as a
   * fail-fast assertion here.
   *
   * @param unadjusted  the unadjusted dates, already checked to be strictly increasing
   * @param adjustment  the business day adjustment applied to each of them
   * @return the adjustable dates
   */
  private def create(
      unadjusted: NonEmptyList[LocalDate],
      adjustment: BusinessDayAdjustment): AdjustableDates =
    new AdjustableDates(unadjusted, adjustment) {}

  //-------------------------------------------------------------------------
  /**
   * The hashing and equality of adjustable dates.
   *
   * Taken from the `equals` and `hashCode` of the case class, which compare the two fields by
   * their own equality - the dates of a list, in order, and the convention and calendar name of
   * an adjustment. Neither field holds a `Double`, so there is no bit-pattern comparison to
   * arrange, and the structural equality the compiler writes is exactly the equality of the bean
   * being ported, which compared the same two properties.
   *
   * Two values that differ only in their adjustment are therefore unequal even where every date
   * adjusts to the same day, as they were in the original: what is held is the agreement, and two
   * different agreements that happen to agree today are still two agreements.
   *
   * This is the type's only equality-bearing instance, and `Eq[AdjustableDates]` is obtained from
   * it by subtyping rather than declared separately. There is no `Order`: the bean being ported
   * is not `Comparable`, and an ordering over a run of dates and an adjustment would be this
   * port's invention.
   *
   * @return the hashing of adjustable dates
   */
  implicit val hash: Hash[AdjustableDates] = Hash.fromUniversalHashCode[AdjustableDates]

  /**
   * The rendering of adjustable dates as text.
   *
   * Renders what [[AdjustableDates.toString]] renders, which is the form of the library being
   * ported, so the two ways of putting a run of adjustable dates into a message agree.
   *
   * @return the rendering of adjustable dates
   */
  implicit val show: Show[AdjustableDates] = Show.show(_.toString)

  //-------------------------------------------------------------------------
  /**
   * The field shape both codecs are derived from, which the decoder reads before validation.
   *
   * Decoding a validated type is two steps: read the fields, then hand them to the factory that
   * decides whether they describe a value. This product is the first step, and encoding is the
   * same two steps in reverse, so both codecs below derive from this one declaration and the JSON
   * shape of a run of adjustable dates is stated exactly once. It is private and never returned -
   * the only values of it that exist are the ones the two codecs build. Its field names are the
   * JSON keys, and they are the names of the two fields of [[AdjustableDates]] itself, which are
   * the names of the two properties of the bean being ported, in their declaration order.
   *
   * The dates are carried as a plain list rather than as the non-empty list the type holds,
   * because the JSON library publishes a codec for the former and not for the latter, and because
   * a payload can perfectly well carry an empty array - which is exactly the input the factory
   * rejects. Reading it into a shape that could not express it would leave the emptiness to be
   * reported as a malformed document rather than as the reason it is.
   *
   * @param unadjusted  the unadjusted dates, each carried as its ISO date string
   * @param adjustment  the business day adjustment, carried as the object of its own two fields
   */
  private final case class Raw(
      unadjusted: List[LocalDate],
      adjustment: BusinessDayAdjustment)

  /** The derived decoder of the raw field shape, used by the validating decoder below. */
  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /** The derived encoder of the raw field shape, used by the encoder below. */
  private val rawEncoder: Encoder[Raw] = deriveEncoder[Raw]

  /**
   * The JSON encoding of adjustable dates.
   *
   * A value is an object of two fields, the dates as an array of ISO date strings and the
   * business day adjustment as the object its own codec writes:
   *
   * {{{
   * {"unadjusted":["2014-07-11","2014-07-13"],
   *  "adjustment":{"convention":"Following","calendar":"Sat/Sun"}}
   * }}}
   *
   * Both halves of the codec are assembled by the same compile-time derivation over the same raw
   * shape, which is what keeps them from drifting apart: the field names, their order and their
   * element codecs are stated once, in [[Raw]], and the encoder reaches them by mapping a value
   * onto that shape. Deriving from [[AdjustableDates]] itself is not possible - the constructor
   * of a validated type is not public, so there is no public shape to derive from - and writing
   * the fields out by hand instead would state the same contract a second time.
   *
   * No part of the encoding inspects a class while the program runs, and two values that are
   * equal encode to identical bytes: the dates are written in the order the value holds them,
   * which its invariant makes the increasing order, so equal values cannot differ in their
   * arrangement. The result is wrapped so that a field holding no value would be omitted, which
   * is the policy every product of this port follows - this type has no optional field, so the
   * wrapping changes nothing about its output and exists so that the policy holds without
   * exception.
   *
   * @return the JSON encoding of adjustable dates
   */
  implicit val encoder: Encoder[AdjustableDates] =
    Codecs.dropNulls(rawEncoder.contramap[AdjustableDates] { value =>
      Raw(value.unadjusted.toList, value.adjustment)
    })

  /**
   * The JSON decoding of adjustable dates.
   *
   * This is the inverse of the encoding above, and it decides whether the fields describe a value
   * exactly as a caller's arguments are decided: the payload is read into the raw shape and
   * handed to [[AdjustableDates.of]], so a document whose dates are out of order, repeat a date,
   * or hold no date at all is a decoding failure carrying that reason rather than a value this
   * type would not have built. Both fields have to be present.
   *
   * @return the JSON decoding of adjustable dates
   */
  implicit val decoder: Decoder[AdjustableDates] =
    Codecs.validatedDecoder[Raw, AdjustableDates] { raw =>
      of(raw.adjustment, raw.unadjusted)
    }(rawDecoder)
}
