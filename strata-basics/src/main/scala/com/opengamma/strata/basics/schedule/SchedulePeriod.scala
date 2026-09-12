/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.schedule

import java.time.LocalDate
import java.time.Period

import cats.Hash
import cats.Order
import cats.Show
import cats.data.EitherNec
import cats.syntax.apply._

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.basics.date.DateAdjuster
import com.opengamma.strata.basics.date.DayCount
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * A period in a schedule.
 *
 * This consists of a single period (date range) within a schedule. This is typically used as the
 * basis for financial calculations, such as accrual of interest.
 *
 * Two pairs of dates are provided, start/end and unadjustedStart/unadjustedEnd. The period itself
 * runs from [[startDate]] to [[endDate]]. The [[unadjustedStartDate]] and [[unadjustedEndDate]]
 * are the dates used to calculate the start date and the end date when applying business day
 * adjustment.
 *
 * For example, consider a schedule that has periods every three months on the 10th of the month.
 * From time to time, the scheduled date will be a weekend or holiday. In this case, a rule may
 * apply moving the date to a valid business day. If this happens, then the "unadjusted" date is
 * the original date in the periodic schedule and the "adjusted" date is the related valid business
 * day. Note that not all schedules apply a business day adjustment.
 *
 * ===Construction===
 *
 * A period is a '''validated''' value: both pairs of dates have to be in time-line order and
 * neither pair may be degenerate. The two conditions are decided by [[SchedulePeriod.of]], which
 * reports them as failures rather than raising, so a value of this type is known to describe a
 * period of non-zero length in both its adjusted and its unadjusted form. The type is a
 * `sealed abstract case class` with a private constructor, which is what leaves it without a public
 * `apply` or `copy` - there is no route to an instance that skips the factory - while keeping the
 * `equals`, `hashCode` and `unapply` of a case class.
 *
 * ===Divergences from the Java original===
 *
 * These are the points on which this port deliberately differs from
 * `com.opengamma.strata.basics.schedule.SchedulePeriod`, recorded here for `SCALA_MIGRATION.md`:
 *
 *  - '''The validator's exception becomes an accumulating failure.''' The bean checked its two
 *    date pairs in an `@ImmutableValidator` that threw `IllegalArgumentException` at the first
 *    pair out of order. [[SchedulePeriod.of]] returns `EitherNec[Failure, SchedulePeriod]` and
 *    reports '''both''' causes, so a caller correcting a definition sees everything wrong with it
 *    at once rather than one thing per attempt.
 *  - '''`toAdjusted` gains an error channel.''' The method threw `IllegalArgumentException` where
 *    adjustment collapsed the period onto a single day; it now returns that failure as a value.
 *    [[toUnadjusted]] stays total, because the unadjusted pair of a value of this type is already
 *    known to be in order.
 *  - '''`subSchedule` gains an error channel too.''' The method built its definition through the
 *    bean builder of [[PeriodicSchedule]], which threw where the arguments described no definition;
 *    it now returns the definition through [[PeriodicSchedule.of]] and reports those reasons as a
 *    value. What it returns is still the definition rather than a generated schedule, so the
 *    holiday calendar the dates need stays a choice of the caller.
 *  - '''`yearFraction` takes the schedule as its day count contract.''' The method took the
 *    concrete schedule containing this period; it takes
 *    [[com.opengamma.strata.basics.date.DayCount.ScheduleInfo]], which is the only thing the day
 *    count reads from it, so a schedule is still accepted and a bare set of schedule facts is too.
 *  - '''Ordering gains a tie-break.''' The bean's `compareTo` compared the unadjusted start date
 *    and then the unadjusted end date, ignoring the adjusted pair, so two periods that differ only
 *    in their adjusted dates compared equal while being unequal. Cats requires `compare == 0`
 *    exactly where `eqv` holds, so [[SchedulePeriod.order]] continues with the adjusted start date
 *    and then the adjusted end date. The first two keys are unchanged, so any behaviour that
 *    depended on the Java ordering - sorting the periods of a schedule, for instance - is
 *    preserved; only pairs the Java comparison called equal are separated.
 *  - '''No Joda bean, builder or Java serialization.''' The meta-bean, the builder and its
 *    pre-build defaulting, `ImmutableBean`, `Serializable` and `Comparable` are all dropped. The
 *    builder's defaulting of each absent unadjusted date to its adjusted counterpart survives as
 *    the two-argument [[SchedulePeriod.of]], and JSON replaces Java serialization.
 *
 * @param startDate  the start date of this period, used for financial calculations such as
 *   interest accrual; the first date in the schedule period, typically treated as inclusive, and
 *   the adjusted date where the schedule adjusts for business days
 * @param endDate  the end date of this period, used for financial calculations such as interest
 *   accrual; the last date in the schedule period, typically treated as exclusive, and the
 *   adjusted date where the schedule adjusts for business days
 * @param unadjustedStartDate  the start date before any business day adjustment, which is
 *   typically the regular periodic date, and the same as the start date where the schedule does
 *   not adjust for business days
 * @param unadjustedEndDate  the end date before any business day adjustment, which is typically
 *   the regular periodic date, and the same as the end date where the schedule does not adjust for
 *   business days
 */
sealed abstract case class SchedulePeriod private (
    startDate: LocalDate,
    endDate: LocalDate,
    unadjustedStartDate: LocalDate,
    unadjustedEndDate: LocalDate) {

  /**
   * Returns the length of the period.
   *
   * This returns the length of the period, considering the adjusted start and end dates. The
   * calculation does not involve a day count or holiday calendar. The period is calculated using
   * `Period.between` and as such includes the start date and excludes the end date.
   *
   * @return the length of the period
   */
  def length: Period = Period.between(startDate, endDate)

  /**
   * Calculates the number of days in the period.
   *
   * This returns the actual number of days in the period, considering the adjusted start and end
   * dates. The calculation does not involve a day count or holiday calendar. The length includes
   * one date and excludes the other.
   *
   * The count is always positive, because the two dates of a period are in order and distinct, and
   * it overflows only for a period spanning more than about 5.8 million years, where the
   * conversion refuses with `ArithmeticException` exactly as the method being ported did.
   *
   * @return the actual number of days in the period
   */
  def lengthInDays: Int = Math.toIntExact(endDate.toEpochDay - startDate.toEpochDay)

  //-------------------------------------------------------------------------
  /**
   * Calculates the year fraction using the specified day count.
   *
   * Additional information from the schedule is made available to the day count algorithm: the
   * schedule that contains this period is the
   * [[com.opengamma.strata.basics.date.DayCount.ScheduleInfo]] passed here, and the four
   * conventions that read the surrounding schedule - `Act/Act ICMA`, `Act/365 Actual`, `30U/360`
   * and `30E/360 ISDA` - take what they need from it.
   *
   * The result is a plain `Double` rather than a failure-carrying value, which is the signature of
   * the method being ported. A day count that reads a schedule fact the supplied schedule cannot
   * answer refuses the call through `ArgCheck` instead, because handing a convention a schedule
   * that cannot answer what the convention is defined in terms of breaks the contract of the call
   * rather than describing data the library should report on. A real schedule answers everything,
   * so that refusal is not reachable from this method.
   *
   * @param dayCount  the day count convention
   * @param schedule  the schedule that contains this period
   * @return the year fraction, calculated via the day count
   * @throws IllegalArgumentException if the day count reads schedule information the schedule does
   *   not carry
   */
  def yearFraction(dayCount: DayCount, schedule: DayCount.ScheduleInfo): Double =
    dayCount.yearFraction(startDate, endDate, schedule)

  /**
   * Checks if this period is regular according to the specified frequency and roll convention.
   *
   * A schedule period is normally created from a frequency and roll convention. These can
   * therefore be used to determine if the period is regular, which simply means that the period
   * end date can be generated from the start date and vice versa.
   *
   * The check is deliberately symmetric, as it was in the original: both directions have to agree,
   * so a period whose end date rolls forward correctly from its start date but whose start date
   * does not roll back from its end date is '''not''' regular. This is what a schedule uses to
   * classify its first and last periods as stubs, so the two-sided form is load-bearing.
   *
   * The unadjusted dates are compared, because they are the dates a roll convention generates.
   *
   * @param frequency  the frequency
   * @param rollConvention  the roll convention
   * @return true if the period is regular
   */
  def isRegular(frequency: Frequency, rollConvention: RollConvention): Boolean =
    rollConvention.next(unadjustedStartDate, frequency) == unadjustedEndDate &&
      rollConvention.previous(unadjustedEndDate, frequency) == unadjustedStartDate

  /**
   * Checks if this period contains the specified date.
   *
   * The adjusted start and end dates are used in the comparison. The start date is included, the
   * end date is excluded.
   *
   * @param date  the date to check
   * @return true if this period contains the date
   */
  def contains(date: LocalDate): Boolean = !date.isBefore(startDate) && date.isBefore(endDate)

  //-------------------------------------------------------------------------
  /**
   * Creates a sub-schedule within this period.
   *
   * The sub-schedule will have one or more periods. The schedule is bounded by the '''unadjusted'''
   * start and end date of this period, because those are the dates a roll convention generates
   * from: a sub-schedule derived from the adjusted pair would roll from a business day rather than
   * from the periodic date the enclosing schedule was built on. The frequency and roll convention
   * are used to build the unadjusted dates of the sub-schedule, the stub convention handles any
   * remaining time where the new frequency does not divide evenly into this period, and the
   * business day adjustment is the one the sub-schedule applies to every date it produces.
   *
   * What is returned is the '''definition''' of the sub-schedule rather than the schedule itself,
   * as it was in the method being ported: generating the dates needs a holiday calendar, so the
   * caller resolves the definition with [[PeriodicSchedule.createSchedule]] and the reference data
   * of its choosing. That keeps the reference data threaded explicitly, which is the rule this
   * port follows everywhere.
   *
   * The definition is decided by [[PeriodicSchedule.of]], so a set of arguments that describes no
   * definition is reported as a chain of reasons rather than raised - the method being ported threw
   * `ScheduleException` for the same cases. The dates this period contributes are already known to
   * be in order, so the only reasons reachable here come from the four supplied arguments.
   *
   * @param frequency  the frequency of the sub-schedule
   * @param rollConvention  the roll convention to use for rolling
   * @param stubConvention  the stub convention to use for any excess
   * @param adjustment  the business day adjustment to apply to the sub-schedule
   * @return the definition of the sub-schedule, or the failures describing why the arguments
   *   describe none
   */
  def subSchedule(
      frequency: Frequency,
      rollConvention: RollConvention,
      stubConvention: StubConvention,
      adjustment: BusinessDayAdjustment): EitherNec[Failure, PeriodicSchedule] =
    PeriodicSchedule.of(
      unadjustedStartDate,
      unadjustedEndDate,
      frequency,
      adjustment,
      stubConvention,
      rollConvention)

  //-------------------------------------------------------------------------
  /**
   * Converts this period to one where the start and end dates are adjusted using the specified
   * adjuster.
   *
   * The start date of the result will be the start date of this period as altered by the specified
   * adjuster. The end date of the result will be the end date of this period as altered by the
   * specified adjuster. The unadjusted start date and unadjusted end date will be the same as in
   * this period.
   *
   * The adjuster will typically be obtained from
   * [[com.opengamma.strata.basics.date.BusinessDayAdjustment.resolve]].
   *
   * Adjustment can collapse a short period onto a single day, and the pair of dates that results
   * is then no longer a period; that is reported as a failure, where the method being ported threw
   * `IllegalArgumentException`. The variant taking a merge type is how a schedule avoids the
   * collapse for its first and last period.
   *
   * @param adjuster  the adjuster to use
   * @return the adjusted schedule period, or the failures describing why the adjusted dates
   *   describe none
   */
  def toAdjusted(adjuster: DateAdjuster): EitherNec[Failure, SchedulePeriod] =
    rebuilt(adjuster.adjust(startDate), adjuster.adjust(endDate))

  /**
   * Converts this period to an adjusted one, handling a short period that adjusts to nothing.
   *
   * This is the variant [[Schedule.toAdjusted]] uses, and the merge type says what to do when
   * adjustment brings the two dates of this period onto the same day:
   *
   *  - `-1` - keep the start date as it is, which is the rule for the '''first''' period of a
   *    schedule, whose start date is the start of the schedule and therefore must not move onto
   *    the following period.
   *  - `1` - keep the end date as it is, which is the rule for the '''last''' period of a
   *    schedule, whose end date is the end of the schedule.
   *  - `0` - let the collapse be reported, which is the rule for every period in between; the
   *    failure is the answer, because two adjacent dates in the middle of a schedule collapsing
   *    onto one is a definition the caller has to correct.
   *
   * The merge type is tested before the dates, as in the implementation being ported: the
   * overwhelmingly common case is a period in the middle of a schedule, where the comparison of
   * two integers settles the question on its own.
   *
   * This is visible within the schedule package only, because the merge type is a decision a
   * schedule makes about the position of a period within itself and means nothing to a caller
   * holding a period on its own.
   *
   * @param adjuster  the adjuster to use
   * @param mergeType  -1 to keep the start date unadjusted, 0 to report the collapse, 1 to keep
   *   the end date unadjusted
   * @return the adjusted schedule period, or the failures describing why the adjusted dates
   *   describe none
   */
  private[schedule] def toAdjusted(
      adjuster: DateAdjuster,
      mergeType: Int): EitherNec[Failure, SchedulePeriod] = {
    val adjustedStart = adjuster.adjust(startDate)
    val adjustedEnd = adjuster.adjust(endDate)
    // the two branches are mutually exclusive - no merge type is both -1 and 1 - so each date is
    // decided on its own, which is the same outcome as the chained condition being ported
    val resultStart =
      if (mergeType == -1 && adjustedStart == adjustedEnd) startDate else adjustedStart
    val resultEnd =
      if (mergeType == 1 && adjustedStart == adjustedEnd) endDate else adjustedEnd
    rebuilt(resultStart, resultEnd)
  }

  /**
   * Converts this period to one where the start and end dates are set to the unadjusted dates.
   *
   * The start date of the result will be the unadjusted start date of this period. The end date of
   * the result will be the unadjusted end date of this period. The unadjusted start date and
   * unadjusted end date will be the same as in this period.
   *
   * This is '''total''', and it is the one derivation of this type that needs no error channel: the
   * dates it moves into the adjusted positions are the unadjusted dates of a value that exists, so
   * they have already been checked to be in order and distinct, and the pair of dates it produces
   * satisfies both conditions of the type by construction. It therefore builds the value directly
   * rather than passing arguments that cannot be rejected through a factory that could reject
   * them.
   *
   * @return the unadjusted schedule period
   */
  def toUnadjusted: SchedulePeriod =
    if (startDate == unadjustedStartDate && endDate == unadjustedEndDate) {
      this
    } else {
      SchedulePeriod.create(
        unadjustedStartDate,
        unadjustedEndDate,
        unadjustedStartDate,
        unadjustedEndDate)
    }

  /**
   * Rebuilds this period with a new pair of adjusted dates, keeping the unadjusted pair.
   *
   * Shared by the two forms of `toAdjusted`, and the reason both of them have the property their
   * callers rely on: a pair of dates equal to the pair this period already holds yields '''this
   * very instance''', so a schedule adjusting all of its periods can tell by reference whether
   * anything moved and hand back itself when nothing did. Any other pair is decided by the factory
   * of the type, because adjustment can bring the two dates onto the same day.
   *
   * @param resultStart  the start date of the result
   * @param resultEnd  the end date of the result
   * @return this period where the dates are unchanged, otherwise the rebuilt period or the
   *   failures describing why the dates describe none
   */
  private def rebuilt(
      resultStart: LocalDate,
      resultEnd: LocalDate): EitherNec[Failure, SchedulePeriod] =
    if (resultStart == startDate && resultEnd == endDate) {
      Right(this)
    } else {
      SchedulePeriod.of(resultStart, resultEnd, unadjustedStartDate, unadjustedEndDate)
    }

  /**
   * Renders this period as text.
   *
   * A period that applies no business day adjustment - one whose adjusted dates are its unadjusted
   * dates - renders as its two dates, and any other period adds the unadjusted pair, so the two
   * pairs are distinguishable and nothing a value holds is hidden:
   *
   * {{{
   * 2014-06-16 to 2014-07-18
   * 2014-06-16 to 2014-07-18 (unadjusted 2014-06-16 to 2014-07-17)
   * }}}
   *
   * This is the port's own form. The bean being ported rendered the property-by-property text of a
   * Joda bean, which no longer has a counterpart here; the JSON encoding is where the fields are
   * written out under their own names.
   *
   * @return the text form of this period
   */
  override def toString: String =
    if (startDate == unadjustedStartDate && endDate == unadjustedEndDate) {
      s"$startDate to $endDate"
    } else {
      s"$startDate to $endDate (unadjusted $unadjustedStartDate to $unadjustedEndDate)"
    }
}

/**
 * Companion of [[SchedulePeriod]], holding its factories, its typeclass instances and its codecs.
 *
 * The two factories are the only way to obtain a period from outside this file, which is what
 * makes the two conditions of the type - both pairs of dates in order, neither pair degenerate -
 * properties of every value that exists rather than properties a caller is asked to respect.
 *
 * Two typeclass instances are published, and exactly two: an `Order` that is also a `Hash`, which
 * is the single equality-bearing instance of the type - `Order` and `Hash` both extend `Eq`, so
 * declaring an `Eq` as well would leave two instances that could disagree and one of them
 * ambiguous - and a `Show`.
 */
object SchedulePeriod {

  /** The name the first adjusted date is reported under, which is the name of its property. */
  private val StartDateField: String = "startDate"

  /** The name the second adjusted date is reported under. */
  private val EndDateField: String = "endDate"

  /** The name the first unadjusted date is reported under. */
  private val UnadjustedStartDateField: String = "unadjustedStartDate"

  /** The name the second unadjusted date is reported under. */
  private val UnadjustedEndDateField: String = "unadjustedEndDate"

  /**
   * The ordering of dates the two order checks below are performed with.
   *
   * The checking helpers of this port are generic in the type being compared and take its cats
   * ordering, and cats publishes no instance for `java.time.LocalDate` - the class implements
   * `Comparable[ChronoLocalDate]` rather than `Comparable[LocalDate]`, so the ordering derived
   * from a comparable type does not apply to it either. The instance is therefore stated here, as
   * the natural time-line order the class itself defines, and kept private: it exists to serve the
   * checks of this file, and publishing an ordering for a type this module does not own would put
   * an instance into implicit scope for every file that imports anything from here.
   */
  private implicit val dateOrder: Order[LocalDate] =
    Order.from((first, second) => first.compareTo(second))

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance from the adjusted and unadjusted dates.
   *
   * Both pairs of dates have to be in time-line order and distinct: the unadjusted start date
   * strictly before the unadjusted end date, and the start date strictly before the end date. A
   * pair of equal dates is rejected along with a pair in the wrong order, because a period of no
   * length is not a period.
   *
   * The two checks are '''combined rather than sequenced''', so a caller supplying two bad pairs
   * is told about both of them in one chain of reasons instead of correcting one and being sent
   * back for the other. This is the accumulation the validator of the bean being ported could not
   * express: it threw at the first pair it found out of order.
   *
   * @param startDate  the start date, used for financial calculations such as interest accrual
   * @param endDate  the end date, used for financial calculations such as interest accrual
   * @param unadjustedStartDate  the unadjusted start date
   * @param unadjustedEndDate  the unadjusted end date
   * @return the period, or the failures describing why the dates describe none
   */
  def of(
      startDate: LocalDate,
      endDate: LocalDate,
      unadjustedStartDate: LocalDate,
      unadjustedEndDate: LocalDate): EitherNec[Failure, SchedulePeriod] =
    (
      Validate.inOrderNotEqual(
        unadjustedStartDate,
        unadjustedEndDate,
        UnadjustedStartDateField,
        UnadjustedEndDateField),
      Validate.inOrderNotEqual(startDate, endDate, StartDateField, EndDateField))
      .mapN((_, _) => create(startDate, endDate, unadjustedStartDate, unadjustedEndDate))
      .toEither

  /**
   * Obtains an instance from two dates.
   *
   * This factory is used when there is no business day adjustment of schedule dates, so each
   * unadjusted date is the adjusted date it would have been adjusted from. It is the defaulting
   * the builder of the bean being ported performed before validating, expressed as a factory
   * because the port has no builder.
   *
   * Since the one pair of dates fills both roles, a pair that is out of order or degenerate is
   * reported under both pairs of names - once as the unadjusted dates and once as the adjusted
   * dates - which is the same pair of checks the bean ran on the defaulted fields.
   *
   * @param startDate  the start date, used for financial calculations such as interest accrual
   * @param endDate  the end date, used for financial calculations such as interest accrual
   * @return the period, or the failures describing why the dates describe none
   */
  def of(startDate: LocalDate, endDate: LocalDate): EitherNec[Failure, SchedulePeriod] =
    of(startDate, endDate, startDate, endDate)

  /**
   * Creates a value, which every route into the type funnels through.
   *
   * This is the only instantiation of the type and it is private, so the two factories above are
   * the only ways into it from outside this file. The constructor of a `sealed abstract case class`
   * is reachable only from inside the file that declares it, and `new SchedulePeriod(...) {}` - an
   * anonymous subclass of the abstract case class - is how it is reached; that is what leaves the
   * type without a public `apply` or `copy` while keeping the `equals`, `hashCode` and `unapply` a
   * case class provides.
   *
   * The method performs no check of its own. Each of its callers has already established both
   * conditions: [[of]] by the two order checks it accumulates, and
   * [[SchedulePeriod.toUnadjusted]] because the unadjusted pair of an existing value was checked
   * when that value was built. Every other derivation of one period from another -
   * `toAdjusted` in both of its forms - goes through [[of]] instead, precisely because adjustment
   * can produce a pair this type would not accept.
   *
   * @param startDate  the start date, already checked to be before the end date
   * @param endDate  the end date
   * @param unadjustedStartDate  the unadjusted start date, already checked to be before the
   *   unadjusted end date
   * @param unadjustedEndDate  the unadjusted end date
   * @return the period
   */
  private def create(
      startDate: LocalDate,
      endDate: LocalDate,
      unadjustedStartDate: LocalDate,
      unadjustedEndDate: LocalDate): SchedulePeriod =
    new SchedulePeriod(startDate, endDate, unadjustedStartDate, unadjustedEndDate) {}

  //-------------------------------------------------------------------------
  /**
   * The ordering, hashing and equality of schedule periods.
   *
   * Equality and hashing are those of the case class, which compare all four dates, and they are
   * the equality of the bean being ported, which compared the same four properties. No field holds
   * a `Double`, so there is no bit-pattern comparison to arrange.
   *
   * The ordering starts where the comparison being ported started - the unadjusted start date,
   * then the unadjusted end date - and then continues with the adjusted start date and the
   * adjusted end date. The continuation is this port's addition, and it is required rather than
   * chosen: cats asks that a comparison agree with equality, meaning `compare == 0` exactly where
   * `eqv` holds, and the two unadjusted dates alone leave two periods that differ in their
   * adjusted dates - equal periodic dates, different business days - comparing equal while being
   * unequal. Because the two leading keys are untouched, every ordering the Java comparison
   * produced is still produced; the addition only decides pairs that comparison called equal.
   *
   * @return the ordering, hashing and equality of schedule periods
   */
  implicit val order: Order[SchedulePeriod] with Hash[SchedulePeriod] =
    new Order[SchedulePeriod] with Hash[SchedulePeriod] {

      private val universal: Hash[SchedulePeriod] = Hash.fromUniversalHashCode[SchedulePeriod]

      override def compare(x: SchedulePeriod, y: SchedulePeriod): Int = {
        val byUnadjustedStart = x.unadjustedStartDate.compareTo(y.unadjustedStartDate)
        val thenUnadjustedEnd =
          if (byUnadjustedStart != 0) byUnadjustedStart
          else x.unadjustedEndDate.compareTo(y.unadjustedEndDate)
        val thenStart =
          if (thenUnadjustedEnd != 0) thenUnadjustedEnd else x.startDate.compareTo(y.startDate)
        if (thenStart != 0) thenStart else x.endDate.compareTo(y.endDate)
      }

      override def eqv(x: SchedulePeriod, y: SchedulePeriod): Boolean = universal.eqv(x, y)

      override def hash(x: SchedulePeriod): Int = universal.hash(x)
    }

  /**
   * The rendering of schedule periods as text.
   *
   * Renders what [[SchedulePeriod.toString]] renders, so the two ways of putting a period into a
   * message agree.
   *
   * @return the rendering of a schedule period
   */
  implicit val show: Show[SchedulePeriod] = Show.show(_.toString)

  //-------------------------------------------------------------------------
  /**
   * The field shape both codecs are derived from, which the decoder reads before validation.
   *
   * Decoding a validated type is two steps: read the fields, then hand them to the factory that
   * decides whether they describe a value. This product is the first step, and encoding is the
   * same two steps in reverse, so both codecs below derive from this one declaration and the JSON
   * shape of a period is stated exactly once. It is private and never returned - the only values
   * of it that exist are the ones the two codecs build. Its field names are the JSON keys, and
   * they are the names of the four fields of [[SchedulePeriod]] itself, which are the names of the
   * four properties of the bean being ported, in their declaration order.
   *
   * Deriving either codec from [[SchedulePeriod]] directly is not possible: the compile-time
   * derivation reads the public constructor of a product, and a validated type has none - it is an
   * abstract case class whose constructor is private - so there is no public shape to derive from.
   * Writing the four fields out by hand instead would state the same contract a second time.
   *
   * @param startDate  the start date, carried as its ISO date string
   * @param endDate  the end date, carried as its ISO date string
   * @param unadjustedStartDate  the unadjusted start date, carried as its ISO date string
   * @param unadjustedEndDate  the unadjusted end date, carried as its ISO date string
   */
  private final case class Raw(
      startDate: LocalDate,
      endDate: LocalDate,
      unadjustedStartDate: LocalDate,
      unadjustedEndDate: LocalDate)

  /** The derived decoder of the raw field shape, used by the validating decoder below. */
  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /** The derived encoder of the raw field shape, used by the encoder below. */
  private val rawEncoder: Encoder[Raw] = deriveEncoder[Raw]

  /**
   * The JSON encoding of schedule periods.
   *
   * A value is an object of four ISO date strings under the names of its four fields:
   *
   * {{{
   * {"startDate":"2014-07-05",
   *  "endDate":"2014-07-18",
   *  "unadjustedStartDate":"2014-07-04",
   *  "unadjustedEndDate":"2014-07-17"}
   * }}}
   *
   * Both halves of the codec are assembled by the same compile-time derivation over the same raw
   * shape, which is what keeps them from drifting apart, and no part of the encoding inspects a
   * class while the program runs. Two equal values encode to identical bytes: the four fields are
   * written in their declaration order and a date has one ISO form. The result is wrapped so that
   * a field holding no value would be omitted, which is the policy every product of this port
   * follows - all four fields of this type are required, so the wrapping changes nothing about its
   * output and exists so that the policy holds without exception.
   *
   * @return the JSON encoding of schedule periods
   */
  implicit val encoder: Encoder[SchedulePeriod] =
    Codecs.dropNulls(rawEncoder.contramap[SchedulePeriod] { value =>
      Raw(value.startDate, value.endDate, value.unadjustedStartDate, value.unadjustedEndDate)
    })

  /**
   * The JSON decoding of schedule periods.
   *
   * This is the inverse of the encoding above, and it decides whether the fields describe a value
   * exactly as a caller's arguments are decided: the payload is read into the raw shape and handed
   * to [[SchedulePeriod.of]], so a document whose dates are out of order or degenerate is a
   * decoding failure carrying every reason it is, rather than a value this type would not have
   * built. All four fields have to be present.
   *
   * @return the JSON decoding of schedule periods
   */
  implicit val decoder: Decoder[SchedulePeriod] =
    Codecs.validatedDecoder[Raw, SchedulePeriod] { raw =>
      of(raw.startDate, raw.endDate, raw.unadjustedStartDate, raw.unadjustedEndDate)
    }(rawDecoder)
}
