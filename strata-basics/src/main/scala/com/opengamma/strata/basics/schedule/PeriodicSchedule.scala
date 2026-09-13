/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.schedule

import java.time.DateTimeException
import java.time.LocalDate
import java.time.Period
import java.time.temporal.ChronoUnit

import scala.annotation.tailrec

import cats.Hash
import cats.Order
import cats.Show
import cats.data.EitherNec
import cats.data.Kleisli
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.syntax.apply._
import cats.syntax.traverse._

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.basics.RefDataReader
import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.date.AdjustableDate
import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.ValidatedFailures

/**
 * Definition of a periodic schedule.
 *
 * A periodic schedule is determined using a "periodic frequency". This splits the schedule into
 * "regular" periods of a fixed length, such as every 3 months. Any remaining days are allocated to
 * irregular "stubs" at the start and/or end.
 *
 * For example, a 24 month (2 year) swap might be divided into 3 month periods. The 24 month period
 * is the overall schedule and the 3 month period is the periodic frequency.
 *
 * Note that a 23 month swap cannot be split into even 3 month periods. Instead, there will be a 2
 * month "initial" stub at the start, a 2 month "final" stub at the end or both an initial and final
 * stub with a combined length of 2 months.
 *
 * ===Example===
 *
 * This example creates a schedule for a 13 month swap that cannot be split into 3 month periods,
 * with a long initial stub rolling at end-of-month:
 *
 * {{{
 * val definition: EitherNec[Failure, PeriodicSchedule] =
 *   PeriodicSchedule.of(
 *     LocalDate.of(2014, 2, 12),
 *     LocalDate.of(2015, 3, 31),
 *     Frequency.P3M,
 *     BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.EUTA),
 *     StubConvention.LONG_INITIAL,
 *     RollConventions.EOM)
 *
 * val schedule: EitherNec[Failure, Schedule] =
 *   definition.flatMap(_.createSchedule(ReferenceData.standard).left.map(NonEmptyChain.one))
 *
 * // result
 * // period 1: 2014-02-12 to 2014-06-30
 * // period 2: 2014-06-30 to 2014-09-30
 * // period 3: 2014-09-30 to 2014-12-31
 * // period 4: 2014-12-31 to 2015-03-31
 * }}}
 *
 * ===Details about stubs and date rolling===
 *
 * The stubs are specified using a combination of the [[StubConvention]], [[RollConvention]] and
 * dates.
 *
 * The explicit stub dates are checked first. An explicit stub occurs if `firstRegularStartDate` or
 * `lastRegularEndDate` is present and they differ from `startDate` and `endDate`.
 *
 * If explicit stub dates are specified then they are used to lock the initial or final stub. If the
 * stub convention is present, it is matched and validated against the locked stub. For example, if
 * an initial stub is specified by dates and the stub convention is 'ShortInitial', 'LongInitial' or
 * 'SmartInitial' then the convention is considered to be matched, thus the periodic frequency is
 * applied using the implicit stub convention 'None'. If the stub convention does not match the
 * dates, then schedule creation reports a failure. If the stub convention is not present, then the
 * periodic frequency is applied using the implicit stub convention 'None'.
 *
 * If explicit stub dates are not specified then the stub convention is used. The convention selects
 * whether to use the start date or the end date as the beginning of the schedule calculation. The
 * beginning of the calculation must match the roll convention, unless the convention is 'EOM', in
 * which case 'EOM' is only applied if the calculation starts at the end of the month.
 *
 * In all cases, the roll convention is used to fine-tune the dates. If not present or 'None', the
 * convention is effectively implied from the first date of the calculation. All calculated dates
 * will match the roll convention. If this is not possible due to the dates specified then schedule
 * creation reports a failure.
 *
 * It is permitted to have `firstRegularStartDate` equal to `endDate`, or `lastRegularEndDate` equal
 * to `startDate`. In both cases, the effect is to define a schedule that is entirely "stub" and has
 * no regular periods. The resulting schedule will retain the frequency specified here, even though
 * it is not used.
 *
 * The schedule operates primarily on "unadjusted" dates. An unadjusted date can be any day,
 * including non-business days. When the unadjusted schedule has been determined, the appropriate
 * business day adjustment is applied to create a parallel schedule of "adjusted" dates.
 *
 * ===Failures and bounds===
 *
 * A definition that cannot be built, and a definition that cannot create its schedule, are both
 * reported as a failure value rather than as a raised exception. Every such rejection carries the
 * message stating what was rejected and, under its `definition` attribute, the rendered definition
 * that was rejected.
 *
 *  - The seven optional properties are `Option` fields, so the absence of a stub date or a
 *    convention is carried by the type, and the `with*` copies of this type each re-validate.
 *  - '''Date generation is bounded.''' The ceiling is `MaximumPeriodCount` periods, and a
 *    definition asking for more is reported as a failure naming it, through three checks: a span
 *    whose width makes the ceiling '''provably''' unreachable is refused in constant time before
 *    either walk begins, so nothing a generation would have completed is refused; each walk stops
 *    at the ceiling's worth of boundaries, so nothing beyond it is materialised; and the assembled
 *    date list - the schedule's two ends, any dated stub and the rolled boundaries together, one
 *    period fewer than there are dates - is checked against the ceiling exactly.
 *  - '''The business day adjustment of the interior dates is resolved once per generation''',
 *    through [[com.opengamma.strata.basics.date.BusinessDayAdjustment.resolve]], which binds the
 *    holiday calendar a single time rather than once per date, and only where there is an interior
 *    date to adjust.
 *  - '''Date arithmetic at the edges of the representable calendar is reported rather than
 *    raised.''' A roll that steps outside the range `java.time` can represent raises
 *    `DateTimeException` or `ArithmeticException`; those two exceptions, and no others, are caught
 *    around the stepping and reported as an invalid definition.
 *
 * @param startDate  the start date, which is the start of the first schedule period; this is
 *   unadjusted and as such might be a weekend or holiday, and any applicable business day
 *   adjustment is applied when creating the schedule
 * @param endDate  the end date, which is the end of the last schedule period; this is unadjusted
 *   and must be after the start date
 * @param frequency  the regular periodic frequency to use, such as every 3 months
 * @param businessDayAdjustment  the business day adjustment to apply to each date of the calculated
 *   schedule, used for the start and end dates too where those have no adjustment of their own
 * @param startDateBusinessDayAdjustment  the business day adjustment to apply to the start date,
 *   where it differs from `businessDayAdjustment`
 * @param endDateBusinessDayAdjustment  the business day adjustment to apply to the end date, where
 *   it differs from `businessDayAdjustment`
 * @param stubConvention  the convention defining how to handle stubs, where one is declared
 * @param rollConvention  the convention defining how to roll dates, where one is declared; when
 *   absent the convention is implied from the dates and the stub convention
 * @param firstRegularStartDate  the start date of the first regular schedule period, which is the
 *   end date of the initial stub; an unadjusted date on or after `startDate` and on or before
 *   `endDate`
 * @param lastRegularEndDate  the end date of the last regular schedule period, which is the start
 *   date of the final stub; an unadjusted date on or after `firstRegularStartDate` and on or before
 *   `endDate`
 * @param overrideStartDate  the start date of the first schedule period, overriding normal
 *   schedule generation; this supports the FpML 'firstPeriodStartDate' concept and is applied as
 *   a final step
 * @see [[Schedule]] for the schedule a definition creates
 * @see [[StubConvention]] and [[RollConvention]] for the conventions that shape it
 */
sealed abstract case class PeriodicSchedule private (
    startDate: LocalDate,
    endDate: LocalDate,
    frequency: Frequency,
    businessDayAdjustment: BusinessDayAdjustment,
    startDateBusinessDayAdjustment: Option[BusinessDayAdjustment],
    endDateBusinessDayAdjustment: Option[BusinessDayAdjustment],
    stubConvention: Option[StubConvention],
    rollConvention: Option[RollConvention],
    firstRegularStartDate: Option[LocalDate],
    lastRegularEndDate: Option[LocalDate],
    overrideStartDate: Option[AdjustableDate])
    extends NoJavaSerialization {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means - which
  // could hold a definition none of the seven checks of `of` had passed - can be stopped is here.
  // The single implementation is the companion's hidden `Impl`.
  JvmClosure.requireSoleImplementation(this, classOf[PeriodicSchedule.Impl])

  // The invariant of this type, stated over the fields the instance actually holds rather than
  // over the arguments a factory was given, because the class file of the implementation carries a
  // public constructor whatever the source asked for: a caller compiled outside this library can
  // name that constructor directly, and the identity check above would admit a definition none of
  // the seven checks of [[PeriodicSchedule.of]] had passed. Those seven are restated here, in the
  // same order and over the same five date-bearing properties, so a definition reaching this
  // constructor by any other route describes a span that schedule creation can roll out rather
  // than one whose stubs run backwards. The frequency and the three adjustments carry their own
  // invariants and place no constraint on each other, so they take no part, and nothing about
  // whether the frequency divides the term is stated - that is decided by creating the schedule,
  // exactly as it is for a definition a factory built.
  //
  // The effective start - the unadjusted override start date where there is one, the start date
  // otherwise - is the one derived quantity the checks need, and it is written out at each of the
  // two places that read it rather than held in a field, so this adds no state to the type.
  JvmClosure.requireInvariant(
    "its start date falls strictly before its end date",
    startDate.isBefore(endDate))
  JvmClosure.requireInvariant(
    "its override start date, where present, falls strictly before its end date",
    overrideStartDate.forall(override_ => override_.unadjusted.isBefore(endDate)))
  JvmClosure.requireInvariant(
    "its first regular start date, where present, falls on or before its end date",
    firstRegularStartDate.forall(firstRegular => !firstRegular.isAfter(endDate)))
  JvmClosure.requireInvariant(
    "its first regular start date and last regular end date, where both are present, are in " +
      "that order or equal",
    firstRegularStartDate
      .zip(lastRegularEndDate)
      .forall { case (firstRegular, lastRegular) => !firstRegular.isAfter(lastRegular) })
  JvmClosure.requireInvariant(
    "its effective start falls on or before a first regular start date that is present",
    firstRegularStartDate.forall(firstRegular =>
      !overrideStartDate.fold(startDate)(override_ => override_.unadjusted).isAfter(firstRegular)))
  JvmClosure.requireInvariant(
    "its effective start falls on or before a last regular end date that is present",
    lastRegularEndDate.forall(lastRegular =>
      !overrideStartDate.fold(startDate)(override_ => override_.unadjusted).isAfter(lastRegular)))
  JvmClosure.requireInvariant(
    "its last regular end date, where present, falls on or before its end date",
    lastRegularEndDate.forall(lastRegular => !lastRegular.isAfter(endDate)))

  import PeriodicSchedule._

  /**
   * Creates the schedule from this definition, as the two-argument `createSchedule` does with
   * `combinePeriodsIfNecessary` set to false.
   *
   * @param refData  the reference data, used to find the holiday calendars
   * @return the schedule, or the failure naming what this definition breaks: a holiday calendar
   *   the reference data does not supply, a date the roll convention does not match, a remainder
   *   the stub convention disallows, a dated stub on a 'Term' frequency, two dates that collide
   *   once adjusted, a generation of more periods than the ceiling allows, or date arithmetic
   *   outside the range of representable dates
   */
  def createSchedule(refData: ReferenceData): Either[Failure, Schedule] =
    createSchedule(refData, false)

  /**
   * Creates the schedule from this definition.
   *
   * The schedule consists of an optional initial stub, a number of regular periods and an optional
   * final stub.
   *
   * The roll convention, stub convention and additional dates are all used to determine the
   * schedule. If the roll convention is not present it will be defaulted from the stub convention,
   * with 'None' as the default. If there are explicit stub dates then they will be used. If the
   * stub convention is present, then it will be validated against the stub dates. If the stub
   * convention and stub dates are not present, then no stubs are allowed.
   *
   * There is special handling for pre-adjusted start dates to avoid creating incorrect stubs. If
   * all the following conditions hold true, then the unadjusted start date is treated as being the
   * day-of-month implied by the roll convention (the adjusted date is unaffected):
   *
   *  - the `startDateBusinessDayAdjustment` property equals [[BusinessDayAdjustment.NONE]] or the
   *    roll convention is 'EOM';
   *  - the roll convention is numeric or 'EOM';
   *  - applying `businessDayAdjustment` to the day-of-month implied by the roll convention yields
   *    the specified start date.
   *
   * There is additional special handling for pre-adjusted first/last regular dates and the end
   * date. If the following conditions hold true, then the unadjusted date is treated as being the
   * day-of-month implied by the roll convention (the adjusted date is unaffected):
   *
   *  - the roll convention is numeric or 'EOM';
   *  - applying `businessDayAdjustment` to the day-of-month implied by the roll convention yields
   *    the first/last regular date that was specified.
   *
   * Where a business day adjustment maps two adjacent unadjusted dates onto one business day, the
   * period between them is degenerate and no schedule can hold it. Passing `true` for
   * `combinePeriodsIfNecessary` merges such runs into a single boundary instead of reporting a
   * failure; passing `false` reports the failure, which is the behaviour of the two-argument form
   * and of the single-argument form above.
   *
   * @param refData  the reference data, used to find the holiday calendars
   * @param combinePeriodsIfNecessary  determines whether periods should be combined if necessary
   * @return the schedule, or the failure naming what this definition breaks: a holiday calendar
   *   the reference data does not supply, a date the roll convention does not match, a remainder
   *   the stub convention disallows, a dated stub on a 'Term' frequency, two dates that collide
   *   once adjusted and are not being combined, a generation of more periods than the ceiling
   *   allows, or date arithmetic outside the range of representable dates
   */
  def createSchedule(
      refData: ReferenceData,
      combinePeriodsIfNecessary: Boolean): Either[Failure, Schedule] =
    unadjustedSchedule(refData).flatMap { case (unadj, rollConv) =>
      applyBusinessDayAdjustment(unadj, refData).flatMap { adj =>
        assembled(unadj, adj, rollConv, combinePeriodsIfNecessary, refData)
      }
    }

  /**
   * Returns the creation of this schedule as an operation awaiting reference data.
   *
   * This is the single-argument `createSchedule` with its argument left unsupplied, so a caller
   * composes it with the other reference-data operations of this module - the adjustment of a date,
   * the resolution of a calendar - and supplies the data once, at the point where the answer is
   * wanted. The reader fails with the single failure of whichever step could not be completed.
   *
   * @return the schedule creation as a function from reference data to the schedule
   */
  def toReader: RefDataReader[Schedule] =
    Kleisli[FailureOr, ReferenceData, Schedule](refData => createSchedule(refData))

  /**
   * Creates the list of unadjusted dates in the schedule.
   *
   * The unadjusted date list will contain at least two elements, the start date and end date.
   * Between those dates will be the calculated periodic schedule.
   *
   * The roll convention, stub convention and additional dates are all used to determine the
   * schedule. If the roll convention is not present it will be defaulted from the stub convention,
   * with 'None' as the default. If there are explicit stub dates then they will be used. If the
   * stub convention is present, then it will be validated against the stub dates. If the stub
   * convention and stub dates are not present, then no stubs are allowed. If the frequency is
   * 'Term' explicit stub dates are disallowed, and the roll and stub convention are ignored.
   *
   * The special handling for the last business day of the month seen in the reference-data form of
   * `createUnadjustedDates` is '''not''' applied here, because that handling needs the holiday
   * calendars and this form is the one for a caller that has none.
   *
   * @return the schedule of unadjusted dates, or the failure naming the broken constraint: the
   *   date the walk starts from must match the roll convention, a dated stub must not be combined
   *   with a 'Term' frequency, a remainder the walk leaves must be a stub the convention allows,
   *   the dates generated must hold no duplicate, the periods they describe must stay within the
   *   ceiling, and each roll must stay inside the range of representable dates
   */
  def createUnadjustedDates(): Either[Failure, List[LocalDate]] = {
    val regularStart = calculatedFirstRegularStartDate
    val regularEnd = calculatedLastRegularEndDate
    val rollConv = calculatedRollConvention(regularStart, regularEnd)
    generateUnadjustedDates(startDate, regularStart, regularEnd, endDate, rollConv)
      .flatMap(deduplicatedUnadjusted)
  }

  /**
   * Creates the list of unadjusted dates in the schedule, using reference data.
   *
   * This is the no-argument `createUnadjustedDates` with the two pieces of special handling that
   * need the holiday calendars applied: the recovery of a pre-adjusted start date, and the recovery
   * of pre-adjusted first/last regular dates and end date, both described on the two-argument
   * `createSchedule`.
   *
   * @param refData  the reference data, used to find the holiday calendars
   * @return the schedule of unadjusted dates, or the failure naming the broken constraint: the
   *   constraints of the no-argument form above, and a holiday calendar that the reference data
   *   must supply for each adjustment applied while recovering a pre-adjusted date
   */
  def createUnadjustedDates(refData: ReferenceData): Either[Failure, List[LocalDate]] =
    unadjustedDates(refData).flatMap(deduplicatedUnadjusted)

  /**
   * Creates the list of adjusted dates in the schedule.
   *
   * The adjusted date list will contain at least two elements, the start date and end date. Between
   * those dates will be the calculated periodic schedule. Each date will be a valid business day as
   * per the appropriate business day adjustment - which for the first and last dates is the
   * adjustment of the start and end date where one is declared, and for every date between them is
   * `businessDayAdjustment`.
   *
   * @param refData  the reference data, used to find the holiday calendars
   * @return the schedule of dates adjusted to valid business days, or the failure naming the
   *   broken constraint: the constraints of `createUnadjustedDates`, a holiday calendar that the
   *   reference data must supply for each adjustment applied, and adjusted dates that must hold no
   *   duplicate - two unadjusted dates mapping onto one business day describe a period of no
   *   length
   */
  def createAdjustedDates(refData: ReferenceData): Either[Failure, List[LocalDate]] =
    for {
      unadj <- unadjustedDates(refData)
      adj <- applyBusinessDayAdjustment(unadj, refData)
      deduplicated <- deduplicatedAdjusted(unadj, adj)
    } yield deduplicated

  /**
   * Generates the unadjusted dates and the roll convention they were generated with.
   *
   * This is the first six steps of schedule creation, shared by it and by the two members that
   * expose only part of the result. The roll convention is returned alongside the dates because it
   * is derived here - from the calculated regular dates, which themselves depend on the reference
   * data - and it is the convention the resulting [[Schedule]] carries, not the one this definition
   * holds as a property.
   *
   * @param refData  the reference data, used to find the holiday calendars
   * @return the unadjusted dates and the roll convention used, or the failure naming the broken
   *   constraint: a holiday calendar the reference data must supply for each adjustment applied
   *   while recovering a pre-adjusted date, and the generation constraints of
   *   `createUnadjustedDates`
   */
  private def unadjustedSchedule(
      refData: ReferenceData): Either[Failure, (List[LocalDate], RollConvention)] =
    for {
      unadjStart <- calculatedUnadjustedStartDate(refData)
      unadjEnd <- calculatedUnadjustedEndDate(refData)
      regularStart <- calculatedFirstRegularStartDate(unadjStart, refData)
      regularEnd <- calculatedLastRegularEndDate(unadjEnd, refData)
      rollConv = calculatedRollConvention(regularStart, regularEnd)
      unadj <- generateUnadjustedDates(unadjStart, regularStart, regularEnd, unadjEnd, rollConv)
    } yield (unadj, rollConv)

  /** Generates the unadjusted dates using the reference data, discarding the roll convention. */
  private def unadjustedDates(refData: ReferenceData): Either[Failure, List[LocalDate]] =
    unadjustedSchedule(refData).map { case (unadj, _) => unadj }

  /**
   * Builds the schedule from the generated dates, combining coincident boundaries if asked to.
   *
   * The periods are built pairwise from the two lists, so a list of `n` dates yields `n - 1`
   * periods. [[SchedulePeriod.of]] rejects a pair that is degenerate or out of order, and it is the
   * only thing that can go wrong here.
   *
   * Where a pair is rejected, the failure reported is not that rejection. The generation is
   * replayed instead, in this order and through the public members themselves: first the
   * '''no-argument''' `createUnadjustedDates`, which generates from the declared start and end
   * dates and recovers no pre-adjusted date from reference data, then
   * `createAdjustedDates(refData)`; the invalid-period message is reported only where both of
   * those return normally. That matters beyond the duplicate-date messages this branch is usually
   * reached by: the no-argument generation works from different dates than the generation that
   * produced the lists passed here, so it can report a roll convention that the declared start
   * date does not match, a stub the convention disallows or the 'Term' explicit-stubs message, and
   * a definition whose dates are pre-adjusted is exactly the definition on which the two
   * generations differ. Checking the two lists directly would report a different branch, and a
   * different message, for those definitions.
   *
   * A schedule with no periods at all is possible only when combining merged every boundary into
   * one, which requires the adjusted dates to have held duplicates, so it reports through the same
   * chain and lands on the duplicated-adjusted-dates message.
   *
   * @param unadj  the unadjusted dates, before any combining
   * @param adj  the adjusted dates, before any combining
   * @param rollConv  the roll convention the dates were generated with
   * @param combinePeriodsIfNecessary  whether runs of coincident adjusted dates merge into one
   * @param refData  the reference data, used to replay the adjusted-date generation
   * @return the schedule, or the failure naming the broken constraint: each date must fall
   *   strictly before the one after it in both the unadjusted and the adjusted list, and the
   *   periods they build must run from earliest to latest; where a pair does not, the failure is
   *   whichever one the replayed generation reports, and the invalid-period message where it
   *   reports none
   */
  private def assembled(
      unadj: List[LocalDate],
      adj: List[LocalDate],
      rollConv: RollConvention,
      combinePeriodsIfNecessary: Boolean,
      refData: ReferenceData): Either[Failure, Schedule] = {
    val (keptUnadj, keptAdj) =
      if (combinePeriodsIfNecessary) combineCoincident(unadj.zip(adj), Nil, Nil) else (unadj, adj)
    val built = keptAdj
      .zip(keptAdj.drop(1))
      .zip(keptUnadj.zip(keptUnadj.drop(1)))
      .traverse {
        case ((adjStart, adjEnd), (unadjStart, unadjEnd)) =>
          SchedulePeriod.of(adjStart, adjEnd, unadjStart, unadjEnd)
      }
    built.toOption.flatMap(periods => NonEmptyList.fromList(periods)) match {
      case Some(periods) =>
        Schedule.of(periods, frequency, rollConv).left.map(Failure.collapse)
      case None =>
        createUnadjustedDates() match {
          case Left(reported) => Left(reported)
          case Right(_) =>
            createAdjustedDates(refData) match {
              case Left(reported) => Left(reported)
              case Right(_) => Left(failure(InvalidPeriodMessage))
            }
        }
    }
  }

  /**
   * Generates the unadjusted dates of the schedule.
   *
   * The five arguments are the dates the `calculated*` members derived: the unadjusted start and
   * end of the whole schedule, the unadjusted start and end of its regular part, and the roll
   * convention to roll with. Where the regular part differs from the whole, the difference is an
   * explicitly dated stub.
   *
   * Whichever of the four branches below produces the dates, the list they produce is passed
   * through [[boundedDates]], which is where the period ceiling is made exact: the walks bound the
   * boundaries they roll, but the dates a branch answers with also include the schedule's two ends
   * and the date of each explicitly dated stub, and it is the assembled list that decides how many
   * periods the schedule has. This is the single place every route into generation passes through -
   * both forms of `createUnadjustedDates`, `createAdjustedDates` and `createSchedule` reach the
   * dates through here - so applying the ceiling here applies it to all of them.
   *
   * @param start  the calculated unadjusted start date of the schedule
   * @param regStart  the calculated unadjusted start date of the regular part
   * @param regEnd  the calculated unadjusted end date of the regular part
   * @param end  the calculated unadjusted end date of the schedule
   * @param rollConv  the roll convention to roll the regular part with
   * @return the unadjusted dates, in order, or the failure naming the broken constraint: a dated
   *   stub must not be combined with a 'Term' frequency, the dated stubs must be consistent with a
   *   declared stub convention, the date the walk starts from must match the roll convention, a
   *   remainder the walk leaves must be a stub the convention allows, the periods the dates
   *   describe must stay within the ceiling, and each roll must stay inside the range of
   *   representable dates
   */
  private def generateUnadjustedDates(
      start: LocalDate,
      regStart: LocalDate,
      regEnd: LocalDate,
      end: LocalDate,
      rollConv: RollConvention): Either[Failure, List[LocalDate]] = {
    val overrideStart = overrideStartDate.map(_.unadjusted).getOrElse(start)
    val explicitInitStub = start != regStart
    val explicitFinalStub = end != regEnd
    val generated: Either[Failure, List[LocalDate]] =
      if (regStart == end || regEnd == start) {
        // the whole schedule is one stub, so there is nothing to roll
        Right(List(overrideStart, end))
      } else if (frequency.isTerm) {
        // a 'Term' schedule is one period by definition, and a dated stub would contradict it
        if (explicitInitStub || explicitFinalStub) {
          Left(failure(TermExplicitStubsMessage))
        } else {
          Right(List(overrideStart, end))
        }
      } else {
        generateImplicitStubConvention(explicitInitStub, explicitFinalStub, regStart, regEnd)
          .flatMap { stubConv =>
            // special fallback if there is an override start date with a specified roll convention:
            // the override, not the regular start, is the date that matches the convention
            val fallbackToOverride =
              overrideStartDate.isDefined &&
                rollConvention.isDefined &&
                firstRegularStartDate.isEmpty &&
                !rollConv.matches(regStart) &&
                rollConv.matches(overrideStart)
            val calcStart = if (fallbackToOverride) overrideStart else regStart
            if (stubConv.isCalculateBackwards) {
              generateBackwards(
                calcStart,
                regEnd,
                rollConv,
                stubConv,
                overrideStart,
                explicitFinalStub,
                end)
            } else {
              generateForwards(
                calcStart,
                regEnd,
                rollConv,
                stubConv,
                explicitInitStub,
                overrideStart,
                explicitFinalStub,
                end)
            }
          }
      }
    generated.flatMap(boundedDates)
  }

  /**
   * Derives the stub convention to roll the regular part with, given the stubs already dated.
   *
   * An absent stub convention is '''not''' the same as [[StubConvention.NONE]]: 'None' validates
   * that there are no explicit stubs at all, whereas absence means only that the remainder left
   * after the explicit stubs are removed must itself have no stubs. That is why the two cases are
   * distinguished here rather than defaulted together.
   *
   * Where the convention is absent, there are no dated stubs and a roll convention is declared, the
   * convention is inferred from which end of the schedule the roll day matches - matching the end
   * date means rolling backwards from it, matching the start date means rolling forwards - and the
   * smart variants are chosen so that a very short remainder is absorbed rather than kept.
   *
   * @param explicitInitialStub  an initial stub has been defined by dates
   * @param explicitFinalStub  a final stub has been defined by dates
   * @param regStart  the calculated unadjusted start date of the regular part
   * @param regEnd  the calculated unadjusted end date of the regular part
   * @return the stub convention to roll with, or the failure describing why the dated stubs and the
   *   declared convention cannot both hold
   */
  private def generateImplicitStubConvention(
      explicitInitialStub: Boolean,
      explicitFinalStub: Boolean,
      regStart: LocalDate,
      regEnd: LocalDate): Either[Failure, StubConvention] =
    stubConvention match {
      case Some(convention) =>
        convention.toImplicit(toString, explicitInitialStub, explicitFinalStub)
      case None =>
        rollConvention match {
          case Some(roll) if !explicitInitialStub && !explicitFinalStub =>
            if (roll.dayOfMonth == regEnd.getDayOfMonth) {
              Right(StubConvention.SMART_INITIAL)
            } else if (roll.dayOfMonth == regStart.getDayOfMonth) {
              Right(StubConvention.SMART_FINAL)
            } else {
              Right(StubConvention.NONE)
            }
          case _ => Right(StubConvention.NONE)
        }
    }

  /**
   * Generates the schedule of dates backwards from the end, the path an initial stub takes.
   *
   * The walk starts at the regular end date, which must itself match the roll convention, and
   * subtracts the frequency until it reaches or passes the regular start date. Where it passes it,
   * the remainder is a stub, and a convention that wants a long stub absorbs it by deleting the
   * earliest boundary the walk produced.
   *
   * The walk is an iterator taken while it stays after the start date, which yields the boundaries
   * in descending order, and the list is assembled by reversing that and prepending the start. The
   * date the walk stopped on - the first boundary that is not after the start date, which decides
   * whether a remainder is left - is recovered by stepping once more from the earliest boundary
   * kept, or from the end date where the walk kept none; the two are the same date.
   *
   * Both the walk and that extra boundary step go through [[rolledDates]] and [[guardedStep]], so
   * a definition whose dates and frequency would roll outside the range `java.time` represents, or
   * would ask for more boundaries than [[PeriodicSchedule.MaximumPeriodCount]] allows, is reported
   * rather than raised. Before either of them runs, the span this walk is about to traverse - the
   * two dates below, not the whole schedule's - is put to
   * [[PeriodicSchedule.provablyExceedsPeriodCount]], so a span that cannot possibly be walked
   * within the ceiling is refused without stepping at all. It is placed after the roll-convention
   * check above deliberately: the order in which a definition's failures are reported is part of
   * what a caller reads, and a mismatched roll convention is reported first.
   *
   * @param start  the unadjusted start date of the regular part, where the walk stops
   * @param end  the unadjusted end date of the regular part, where the walk starts
   * @param rollConv  the roll convention to roll with
   * @param stubConv  the stub convention that decides whether a remainder is kept or absorbed
   * @param explicitStartDate  the first date of the schedule, which is the override where there is
   *   one and the calculated start date otherwise
   * @param explicitFinalStub  whether a final stub has been defined by dates
   * @param explicitEndDate  the last date of the schedule, used only when there is a dated final
   *   stub
   * @return the unadjusted dates, in order, or the failure naming the broken constraint: the
   *   regular end date must match the roll convention, the span must not provably need more
   *   periods than the ceiling allows, the walk must roll no more boundaries than the ceiling
   *   allows, and each roll must stay inside the range of representable dates
   */
  private def generateBackwards(
      start: LocalDate,
      end: LocalDate,
      rollConv: RollConvention,
      stubConv: StubConvention,
      explicitStartDate: LocalDate,
      explicitFinalStub: Boolean,
      explicitEndDate: LocalDate): Either[Failure, List[LocalDate]] =
    if (!rollConv.matches(end)) {
      Left(failure(rollMismatchMessage(end, rollConv, rollingBackwards = true)))
    } else if (provablyExceedsPeriodCount(start, end, frequency)) {
      Left(failure(TooManyPeriodsMessage))
    } else {
      val tail = if (explicitFinalStub) List(end, explicitEndDate) else List(end)
      val step: LocalDate => LocalDate = date => rollConv.previous(date, frequency)
      for {
        descending <- rolledDates(end, step, _.isAfter(start))
        exhausted <- guardedStep(step(descending.lastOption.getOrElse(end)))
      } yield {
        val stub = exhausted != start
        val rolled = descending.reverse ::: tail
        val absorbed =
          if (stub && rolled.sizeIs > 1 && stubConv.isStubLong(start, rolled.head)) {
            rolled.drop(1)
          } else {
            rolled
          }
        explicitStartDate :: absorbed
      }
    }

  /**
   * Generates the schedule of dates forwards from the start, the path a final stub takes.
   *
   * The walk starts at the regular start date, which must itself match the roll convention, and
   * adds the frequency until it reaches or passes the regular end date. Where it passes it, the
   * remainder is a stub, and a convention that wants a long stub absorbs it by deleting the latest
   * boundary the walk produced.
   *
   * Note the asymmetry with the backwards walk: the regular end date is appended '''inside''' the
   * branch that rolls, so a regular part whose two ends coincide contributes no end date at all,
   * while the date of a dated final stub is appended outside it either way.
   *
   * The walk and its extra boundary step are guarded and bounded as the backwards walk's are, by
   * [[PeriodicSchedule.provablyExceedsPeriodCount]] before the walk and by [[rolledDates]] and
   * [[guardedStep]] within it, and failures are reported in this order: the roll mismatch first,
   * then a span provably beyond the period ceiling, then a step outside the date range or beyond
   * that ceiling, then a remainder the stub convention disallows.
   *
   * @param start  the unadjusted start date of the regular part, where the walk starts
   * @param end  the unadjusted end date of the regular part, where the walk stops
   * @param rollConv  the roll convention to roll with
   * @param stubConv  the stub convention that decides whether a remainder is kept, absorbed or
   *   disallowed
   * @param explicitInitialStub  whether an initial stub has been defined by dates
   * @param explicitStartDate  the first date of the schedule, which is the override where there is
   *   one and the calculated start date otherwise
   * @param explicitFinalStub  whether a final stub has been defined by dates
   * @param explicitEndDate  the last date of the schedule, used only when there is a dated final
   *   stub
   * @return the unadjusted dates, in order, or the failure naming the broken constraint: the
   *   regular start date must match the roll convention, the span must not provably need more
   *   periods than the ceiling allows, the walk must roll no more boundaries than the ceiling
   *   allows, each roll must stay inside the range of representable dates, and a remainder the
   *   walk leaves must be a stub the convention allows
   */
  private def generateForwards(
      start: LocalDate,
      end: LocalDate,
      rollConv: RollConvention,
      stubConv: StubConvention,
      explicitInitialStub: Boolean,
      explicitStartDate: LocalDate,
      explicitFinalStub: Boolean,
      explicitEndDate: LocalDate): Either[Failure, List[LocalDate]] =
    if (!rollConv.matches(start)) {
      Left(failure(rollMismatchMessage(start, rollConv, rollingBackwards = false)))
    } else if (provablyExceedsPeriodCount(start, end, frequency)) {
      Left(failure(TooManyPeriodsMessage))
    } else {
      val head =
        if (explicitInitialStub) List(explicitStartDate, start) else List(explicitStartDate)
      val step: LocalDate => LocalDate = date => rollConv.next(date, frequency)
      val regular: Either[Failure, List[LocalDate]] =
        if (start == end) {
          Right(head)
        } else {
          for {
            interior <- rolledDates(start, step, _.isBefore(end))
            exhausted <- guardedStep(step(interior.lastOption.getOrElse(start)))
            rolled = head ::: interior
            absorbed <-
              if (exhausted != end && rolled.sizeIs > 1) {
                applicableStubConvention(stubConv, rollConv, start, end, explicitFinalStub).map {
                  applicable =>
                    if (applicable.isStubLong(rolled.last, end)) rolled.dropRight(1) else rolled
                }
              } else {
                Right(rolled)
              }
          } yield absorbed :+ end
        }
      regular.map(dates => if (explicitFinalStub) dates :+ explicitEndDate else dates)
    }

  /**
   * Rolls the boundary dates of one walk, refusing an overflow and refusing an oversized schedule.
   *
   * This is the stepping both walks perform, written once. The walk is an `Iterator.iterate` over
   * the step, with two containments:
   *
   *  - '''the date arithmetic is guarded.''' A step that leaves the range `java.time` represents
   *    raises `DateTimeException`, and one whose epoch-day arithmetic overflows raises
   *    `ArithmeticException`; both are data-dependent failures of a member that answers with
   *    `Either`, so they are caught here - and only they are - and reported as the failure value
   *    the rest of this type reports, carrying the `definition` attribute.
   *  - '''the iteration itself is bounded.''' The iterator is taken to one boundary beyond
   *    [[PeriodicSchedule.MaximumPeriodCount]] before `takeWhile` is applied, so materialisation
   *    can never exceed the ceiling however wide the dates and however short the frequency, and a
   *    walk that reaches the bound is reported as a failure naming the limit. The extra boundary
   *    is what distinguishes "stopped because the walk ended" from "stopped because the bound was
   *    reached".
   *
   * This is the middle of the three containments the ceiling has, and the one that makes
   * over-materialisation impossible rather than merely unlikely. Ahead of it,
   * [[PeriodicSchedule.provablyExceedsPeriodCount]] has already refused, in constant time, any
   * span that is provably above the ceiling - conservatively, so it refuses nothing this walk
   * would have completed. Behind it, [[boundedDates]] applies the ceiling to the assembled date
   * list, which is the count the message names. The bound here is what covers everything in
   * between: a span the preflight could prove nothing about still cannot roll more than the
   * ceiling's worth of boundaries, because the iterator is never asked for more.
   *
   * Note that the bound here is stated over the boundaries of '''one''' walk, which is why it is
   * not the whole of the ceiling: a walk that rolls more boundaries than the ceiling always implies
   * more periods than the ceiling, so refusing here is sound, but the converse does not hold and
   * the exact count is therefore taken once the dates are assembled.
   *
   * @param from  the date the walk steps from, which is not itself a boundary of the result
   * @param step  the rolling step, which is one application of the roll convention and frequency
   * @param keep  the test each rolled boundary must satisfy to be part of the walk
   * @return the boundaries the walk produced, in the order the walk produced them, or the failure
   *   naming the broken constraint: the walk must roll no more boundaries than the ceiling allows,
   *   and each step must stay inside the range of representable dates
   */
  private def rolledDates(
      from: LocalDate,
      step: LocalDate => LocalDate,
      keep: LocalDate => Boolean): Either[Failure, List[LocalDate]] =
    guardedStep(
      Iterator
        .iterate(step(from))(step)
        .take(MaximumPeriodCount + 1)
        .takeWhile(keep)
        .toList).flatMap { rolled =>
      if (rolled.sizeIs > MaximumPeriodCount) {
        Left(failure(TooManyPeriodsMessage))
      } else {
        Right(rolled)
      }
    }

  /**
   * Refuses a generated date list that describes more periods than the ceiling allows.
   *
   * This is the ceiling applied '''exactly''', to the number of periods the dates describe rather
   * than to the boundaries one walk rolled. The periods of a schedule are built pairwise from its
   * dates, so a list of `n` dates is a schedule of `n - 1` periods, and a list of more than
   * `MaximumPeriodCount + 1` dates is therefore a schedule of more periods than
   * [[PeriodicSchedule.MaximumPeriodCount]] - which is precisely what
   * [[PeriodicSchedule.TooManyPeriodsMessage]] says cannot be generated.
   *
   * The distinction matters because the dates of a schedule are not only the boundaries a walk
   * rolled: the schedule's own start and end dates are there too, and so is the date of each
   * explicitly dated stub, each of which adds a period the walk never counted. Applying this to
   * the result of [[generateUnadjustedDates]] counts all of them, whichever branch produced them.
   *
   * @param dates  the generated unadjusted dates, in order
   * @return the dates, or the failure reporting that they describe too many periods
   */
  private def boundedDates(dates: List[LocalDate]): Either[Failure, List[LocalDate]] =
    if (dates.sizeIs > MaximumPeriodCount + 1) {
      Left(failure(TooManyPeriodsMessage))
    } else {
      Right(dates)
    }

  /**
   * Evaluates one piece of schedule date arithmetic, reporting an overflow instead of raising it.
   *
   * The argument is taken by name and evaluated once, here, so that the two exceptions the
   * `java.time` arithmetic of a roll can raise at the edges of the supported date range become the
   * failure value this type reports everywhere else. Nothing else is caught: an exception of any
   * other type is a defect rather than a property of the dates, and swallowing it would hide it.
   *
   * @param compute  the date arithmetic to evaluate
   * @tparam A  the type the arithmetic produces
   * @return the value the arithmetic produced, or the failure describing the overflow
   */
  private def guardedStep[A](compute: => A): Either[Failure, A] =
    try {
      Right(compute)
    } catch {
      case _: DateTimeException | _: ArithmeticException =>
        Left(failure(DateRangeMessage))
    }

  /**
   * Decides the stub convention that a forwards walk leaving a remainder is allowed to apply.
   *
   * A convention other than 'None' answers for itself. 'None' declares that there is no stub, so a
   * remainder contradicts it and is rejected - except for one accepted edge case: a month-based
   * schedule rolling on the end of the month whose end date shares the day-of-month of a start
   * date that is itself a month end. There the end date simply does not follow the end-of-month
   * rule of the month it falls in, and the schedule is completed with the smart rules rather than
   * refused.
   *
   * @param stubConv  the stub convention derived for the regular part
   * @param rollConv  the roll convention being rolled with
   * @param start  the unadjusted start date of the regular part
   * @param end  the unadjusted end date of the regular part
   * @param explicitFinalStub  whether a final stub has been defined by dates
   * @return the convention to apply, or the failure describing why the remainder is disallowed
   */
  private def applicableStubConvention(
      stubConv: StubConvention,
      rollConv: RollConvention,
      start: LocalDate,
      end: LocalDate,
      explicitFinalStub: Boolean): Either[Failure, StubConvention] =
    if (stubConv != StubConvention.NONE) {
      Right(stubConv)
    } else if (rollConv == RollConventions.EOM &&
      frequency.isMonthBased &&
      !explicitFinalStub &&
      start.getDayOfMonth == start.lengthOfMonth &&
      end.getDayOfMonth == start.getDayOfMonth) {
      Right(StubConvention.SMART_FINAL)
    } else {
      Left(
        failure(
          s"Period '$start' to '$end' resulted in a disallowed stub " +
            s"with frequency '${frequency.name}'"))
    }

  /**
   * Applies the appropriate business day adjustment to each unadjusted date.
   *
   * The three positions take three different adjustments, which is the whole reason this is not one
   * `map`: the first date is [[calculatedStartDate]] adjusted, so that an override start date and a
   * start-date-specific adjustment are both honoured; the last date is [[calculatedEndDate]]
   * adjusted, honouring an end-date-specific adjustment; and every date between them takes
   * `businessDayAdjustment`. Adjusting the two ends with the plain adjustment instead would
   * silently ignore the two optional ones.
   *
   * The interior adjustment is '''resolved once''' per generation, through
   * [[BusinessDayAdjustment.resolve]], and the resulting
   * [[com.opengamma.strata.basics.date.DateAdjuster]] is applied to every interior date. Adjusting
   * date by date instead looks the calendar up again for each date, and for a composite identifier
   * recombines its components each time, so a long schedule would pay for its calendar once per
   * boundary.
   *
   * The resolution is also '''lazy''': it happens only where there is an interior date to adjust.
   * A schedule of two dates puts nothing through `businessDayAdjustment`, so a definition whose
   * plain adjustment names a calendar the reference data does not supply produces its schedule as
   * long as the two ends can be adjusted.
   *
   * The order in which failures are reported is the order of the three positions: the start date's
   * adjustment first, then the interior one, then the end date's.
   *
   * @param unadj  the unadjusted dates
   * @param refData  the reference data, used to find the holiday calendars
   * @return the adjusted dates, in the order of the dates supplied, or the failure of whichever
   *   adjustment could not find its calendar
   */
  private def applyBusinessDayAdjustment(
      unadj: List[LocalDate],
      refData: ReferenceData): Either[Failure, List[LocalDate]] = {
    val interiorDates = unadj.drop(1).dropRight(1)
    for {
      first <- calculatedStartDate.adjusted(refData)
      interior <-
        if (interiorDates.isEmpty) {
          Right(List.empty[LocalDate])
        } else {
          businessDayAdjustment
            .resolve(refData)
            .map(adjuster => interiorDates.map(date => adjuster.adjust(date)))
        }
      last <- calculatedEndDate.adjusted(refData)
    } yield (first :: interior) :+ last
  }

  /**
   * Gets the applicable roll convention defining how to roll dates.
   *
   * The schedule periods are determined at the high level by repeatedly adding the frequency to the
   * start date, or subtracting it from the end date. The roll convention provides the detailed rule
   * to adjust the day-of-month or day-of-week.
   *
   * The applicable roll convention is always a value. If the roll convention property is not
   * present, it is determined from the stub convention, dates and frequency, defaulting to 'None'
   * if necessary.
   *
   * @return the applicable roll convention
   */
  def calculatedRollConvention: RollConvention =
    calculatedRollConvention(calculatedFirstRegularStartDate, calculatedLastRegularEndDate)

  /**
   * Calculates the applicable roll convention from the calculated regular dates.
   *
   * The two dates are parameters rather than being read from this definition because schedule
   * creation derives them from the reference data first, and the convention has to follow the dates
   * actually being rolled.
   *
   * 'EOM' is advisory rather than mandatory, which is why it is handled separately: the stub
   * convention is asked for a convention preferring the end of the month, and 'EOM' itself is used
   * only where that produces nothing better. An absent or 'None' convention is likewise derived
   * from the stub convention, so that 'None' is used only when nothing else applies. Any other
   * declared convention is returned as it stands.
   *
   * @param calculatedFirstRegStartDate  the calculated unadjusted start date of the regular part
   * @param calculatedLastRegEndDate  the calculated unadjusted end date of the regular part
   * @return the applicable roll convention
   */
  private def calculatedRollConvention(
      calculatedFirstRegStartDate: LocalDate,
      calculatedLastRegEndDate: LocalDate): RollConvention = {
    val stubConv = stubConvention.getOrElse(StubConvention.NONE)
    if (rollConvention.contains(RollConventions.EOM)) {
      val derived =
        stubConv.toRollConvention(
          calculatedFirstRegStartDate,
          calculatedLastRegEndDate,
          frequency,
          preferEndOfMonth = true)
      if (derived == RollConventions.NONE) RollConventions.EOM else derived
    } else if (rollConvention.isEmpty || rollConvention.contains(RollConventions.NONE)) {
      stubConv.toRollConvention(
        calculatedFirstRegStartDate,
        calculatedLastRegEndDate,
        frequency,
        preferEndOfMonth = false)
    } else {
      rollConvention.getOrElse(RollConventions.NONE)
    }
  }

  /**
   * Calculates the applicable unadjusted start date.
   *
   * This applies the de facto rule by which 'EOM' means the last business day of the month for the
   * start date, and the equivalent rule for the numeric roll conventions. A start date that has
   * already been adjusted would otherwise imply a day-of-month that the roll convention does not
   * roll on, producing a stub that the definition never asked for.
   *
   * The recovery is attempted only where a roll convention is declared '''and''' either the start
   * date carries the explicit adjustment [[BusinessDayAdjustment.NONE]] or the convention is 'EOM'.
   * A start date that declares no adjustment of its own does not qualify: the condition is on the
   * adjustment being present and being that constant, not on the schedule's plain adjustment.
   *
   * @param refData  the reference data, used to find the holiday calendars
   * @return the calculated unadjusted start date, or the failure that the adjustment testing the
   *   candidate date names a holiday calendar the reference data does not supply
   */
  private def calculatedUnadjustedStartDate(refData: ReferenceData): Either[Failure, LocalDate] =
    rollConvention match {
      case Some(roll)
          if startDateBusinessDayAdjustment.contains(BusinessDayAdjustment.NONE) ||
            roll == RollConventions.EOM =>
        calculatedUnadjustedDateFromAdjusted(startDate, roll, businessDayAdjustment, refData)
      case _ => Right(startDate)
    }

  /**
   * Calculates the applicable unadjusted end date.
   *
   * This is the same recovery as for the start date, with two differences: it is attempted
   * whenever a roll convention is declared, without the adjustment condition, and the adjustment
   * testing the recovered date is the end date's own - [[calculatedEndDateBusinessDayAdjustment]] -
   * rather than the plain one.
   *
   * @param refData  the reference data, used to find the holiday calendars
   * @return the calculated unadjusted end date, or the failure that the adjustment testing the
   *   candidate date names a holiday calendar the reference data does not supply
   */
  private def calculatedUnadjustedEndDate(refData: ReferenceData): Either[Failure, LocalDate] =
    rollConvention match {
      case Some(roll) =>
        calculatedUnadjustedDateFromAdjusted(
          endDate,
          roll,
          calculatedEndDateBusinessDayAdjustment,
          refData)
      case None => Right(endDate)
    }

  /**
   * Calculates the applicable first regular start date.
   *
   * This is either `firstRegularStartDate` or `startDate`.
   *
   * @return the start date of the first regular period
   */
  def calculatedFirstRegularStartDate: LocalDate = firstRegularStartDate.getOrElse(startDate)

  /**
   * Calculates the first regular start date, recovering a pre-adjusted date where one is declared.
   *
   * Where no first regular start date is declared, the calculated unadjusted start date of the
   * whole schedule stands in for it, so the regular part begins where the schedule does and there
   * is no initial stub.
   *
   * @param unadjStart  the calculated unadjusted start date of the schedule
   * @param refData  the reference data, used to find the holiday calendars
   * @return the calculated start date of the first regular period, or the failure that the
   *   adjustment testing the candidate date names a holiday calendar the reference data does not
   *   supply
   */
  private def calculatedFirstRegularStartDate(
      unadjStart: LocalDate,
      refData: ReferenceData): Either[Failure, LocalDate] =
    (firstRegularStartDate, rollConvention) match {
      case (None, _) => Right(unadjStart)
      case (Some(firstRegular), Some(roll)) =>
        calculatedUnadjustedDateFromAdjusted(firstRegular, roll, businessDayAdjustment, refData)
      case (Some(firstRegular), None) => Right(firstRegular)
    }

  /**
   * Calculates the applicable last regular end date.
   *
   * This is either `lastRegularEndDate` or `endDate`.
   *
   * @return the end date of the last regular period
   */
  def calculatedLastRegularEndDate: LocalDate = lastRegularEndDate.getOrElse(endDate)

  /**
   * Calculates the last regular end date, recovering a pre-adjusted date where one is declared.
   *
   * Where no last regular end date is declared, the calculated unadjusted end date of the whole
   * schedule stands in for it, so the regular part ends where the schedule does and there is no
   * final stub. The adjustment testing a recovered date here is the plain `businessDayAdjustment`;
   * the end date's own adjustment is reserved for the end date itself.
   *
   * @param unadjEnd  the calculated unadjusted end date of the schedule
   * @param refData  the reference data, used to find the holiday calendars
   * @return the calculated end date of the last regular period, or the failure that the adjustment
   *   testing the candidate date names a holiday calendar the reference data does not supply
   */
  private def calculatedLastRegularEndDate(
      unadjEnd: LocalDate,
      refData: ReferenceData): Either[Failure, LocalDate] =
    (lastRegularEndDate, rollConvention) match {
      case (None, _) => Right(unadjEnd)
      case (Some(lastRegular), Some(roll)) =>
        calculatedUnadjustedDateFromAdjusted(lastRegular, roll, businessDayAdjustment, refData)
      case (Some(lastRegular), None) => Right(lastRegular)
    }

  /**
   * Calculates the applicable business day adjustment to apply to the start date.
   *
   * This is either `startDateBusinessDayAdjustment` or `businessDayAdjustment`.
   */
  private def calculatedStartDateBusinessDayAdjustment: BusinessDayAdjustment =
    startDateBusinessDayAdjustment.getOrElse(businessDayAdjustment)

  /**
   * Calculates the applicable business day adjustment to apply to the end date.
   *
   * This is either `endDateBusinessDayAdjustment` or `businessDayAdjustment`.
   */
  private def calculatedEndDateBusinessDayAdjustment: BusinessDayAdjustment =
    endDateBusinessDayAdjustment.getOrElse(businessDayAdjustment)

  /**
   * Calculates the applicable start date.
   *
   * The result combines the start date and the appropriate business day adjustment. Where an
   * override start date is present, it is returned as it stands - it carries its own adjustment.
   *
   * This is '''total''': [[AdjustableDate.of]] accepts any date and any adjustment.
   *
   * @return the calculated start date
   */
  def calculatedStartDate: AdjustableDate =
    overrideStartDate.getOrElse(
      AdjustableDate.of(startDate, calculatedStartDateBusinessDayAdjustment))

  /**
   * Calculates the applicable end date.
   *
   * The result combines the end date and the appropriate business day adjustment.
   *
   * This is '''total''': [[AdjustableDate.of]] accepts any date and any adjustment.
   *
   * @return the calculated end date
   */
  def calculatedEndDate: AdjustableDate =
    AdjustableDate.of(endDate, calculatedEndDateBusinessDayAdjustment)

  /**
   * Returns an instance based on this schedule with the start date replaced.
   *
   * This returns a new instance with the schedule altered to have the specified start date. The
   * specified date is considered to be adjusted, thus `startDateBusinessDayAdjustment` is set to
   * 'None'. The `firstRegularStartDate` and `overrideStartDate` fields are also removed.
   *
   * The stub convention is typically altered to be 'SmartInitial'. The algorithm retains the
   * 'ShortInitial' and 'LongInitial' conventions as is. It examines "Final" conventions to try and
   * set the `lastRegularEndDate` via schedule generation, allowing the stub convention to become
   * 'SmartInitial'; where that generation fails, or produces too few dates to have a regular part,
   * both fields are left untouched.
   *
   * @param adjustedStartDate  the proposed start date, which is considered to be adjusted
   * @return a schedule with the proposed start date, or the failures naming the broken constraint:
   *   the proposed date must not fall after the end date, it must fall strictly before it, and it
   *   must fall on or before a last regular end date that survives the replacement
   */
  def replaceStartDate(adjustedStartDate: LocalDate): EitherNec[Failure, PeriodicSchedule] =
    if (adjustedStartDate.isAfter(endDate)) {
      Left(NonEmptyChain.one(failure(StartDateAfterEndDateMessage)))
    } else {
      val (replacedStubConvention, replacedLastRegularEndDate) = replacedStub
      copyWith(
        startDate = adjustedStartDate,
        startDateBusinessDayAdjustment = Some(BusinessDayAdjustment.NONE),
        stubConvention = replacedStubConvention,
        firstRegularStartDate = None,
        lastRegularEndDate = replacedLastRegularEndDate,
        overrideStartDate = None)
    }

  /**
   * Decides the stub convention and last regular end date a replaced start date implies.
   *
   * There are three cases. An absent convention, 'Both' or 'None' cannot survive the move of the
   * start date, so 'SmartInitial' replaces them. A "Final" convention is turned into 'SmartInitial'
   * too, but only once the boundary between the regular part and the final stub is pinned down:
   * either it is already declared, or it is taken from the penultimate date this definition
   * generates. Any other convention - the initial and smart ones - is retained, because moving the
   * start date is exactly what it already describes.
   *
   * @return the stub convention and last regular end date of the replacement
   */
  private def replacedStub: (Option[StubConvention], Option[LocalDate]) =
    if (stubConvention.isEmpty ||
      stubConvention.contains(StubConvention.BOTH) ||
      stubConvention.contains(StubConvention.NONE)) {
      (Some(StubConvention.SMART_INITIAL), lastRegularEndDate)
    } else if (stubConvention.exists(_.isFinal)) {
      lastRegularEndDate match {
        case Some(_) =>
          // last regular is set, so the final stub convention can be safely changed
          (Some(StubConvention.SMART_INITIAL), lastRegularEndDate)
        case None =>
          // calculate the last regular date so that 'SmartInitial' can be used;
          // if schedule generation fails, make no changes
          createUnadjustedDates().toOption.filter(_.sizeIs > 2) match {
            case Some(dates) => (Some(StubConvention.SMART_INITIAL), Some(dates(dates.size - 2)))
            case None => (stubConvention, lastRegularEndDate)
          }
      }
    } else {
      (stubConvention, lastRegularEndDate)
    }

  /**
   * Returns a copy of this definition with the start date replaced.
   *
   * The copy is re-validated, so the answer carries the failures of any invariant the new value
   * breaks. Each `with*` member changes a single property and funnels through the validating
   * factory, so no sequence of them can reach a definition the factory would have refused.
   *
   * @param startDate  the unadjusted start date of the schedule
   * @return the copy, or the failures naming the broken constraint: the start date must fall
   *   strictly before the end date and, where no override start date is declared, on or before a
   *   declared first regular start date and last regular end date
   */
  def withStartDate(startDate: LocalDate): EitherNec[Failure, PeriodicSchedule] =
    copyWith(startDate = startDate)

  /**
   * Returns a copy of this definition with the end date replaced.
   *
   * @param endDate  the unadjusted end date of the schedule
   * @return the copy, or the failures naming the broken constraint: the end date must fall strictly
   *   after the start date and after the unadjusted date of a declared override start date, and on
   *   or after a declared first regular start date and last regular end date
   */
  def withEndDate(endDate: LocalDate): EitherNec[Failure, PeriodicSchedule] =
    copyWith(endDate = endDate)

  /**
   * Returns a copy of this definition with the business day adjustment replaced.
   *
   * @param businessDayAdjustment  the adjustment applied to each date of the calculated schedule
   * @return the copy, or the failures naming the broken constraint; an adjustment takes part in
   *   none of the date orderings the factory checks, so a definition that held them holds them
   *   still, and whether the adjustment finds its calendar is decided by schedule creation
   */
  def withBusinessDayAdjustment(
      businessDayAdjustment: BusinessDayAdjustment): EitherNec[Failure, PeriodicSchedule] =
    copyWith(businessDayAdjustment = businessDayAdjustment)

  /**
   * Returns a copy of this definition with the start date's business day adjustment replaced.
   *
   * @param startDateBusinessDayAdjustment  the adjustment of the start date, or `None` to fall back
   *   on the schedule's own adjustment
   * @return the copy, or the failures naming the broken constraint; an adjustment takes part in
   *   none of the date orderings the factory checks, so a definition that held them holds them
   *   still
   */
  def withStartDateBusinessDayAdjustment(
      startDateBusinessDayAdjustment: Option[BusinessDayAdjustment])
      : EitherNec[Failure, PeriodicSchedule] =
    copyWith(startDateBusinessDayAdjustment = startDateBusinessDayAdjustment)

  /**
   * Returns a copy of this definition with the end date's business day adjustment replaced.
   *
   * @param endDateBusinessDayAdjustment  the adjustment of the end date, or `None` to fall back on
   *   the schedule's own adjustment
   * @return the copy, or the failures naming the broken constraint; an adjustment takes part in
   *   none of the date orderings the factory checks, so a definition that held them holds them
   *   still
   */
  def withEndDateBusinessDayAdjustment(
      endDateBusinessDayAdjustment: Option[BusinessDayAdjustment])
      : EitherNec[Failure, PeriodicSchedule] =
    copyWith(endDateBusinessDayAdjustment = endDateBusinessDayAdjustment)

  /**
   * Returns a copy of this definition with the stub convention replaced.
   *
   * @param stubConvention  the convention defining how to handle stubs, or `None` to leave it to be
   *   implied from the dates and the roll convention
   * @return the copy, or the failures naming the broken constraint; a convention takes part in none
   *   of the date orderings the factory checks, and whether it agrees with the dates and the dated
   *   stubs is decided by schedule creation
   */
  def withStubConvention(
      stubConvention: Option[StubConvention]): EitherNec[Failure, PeriodicSchedule] =
    copyWith(stubConvention = stubConvention)

  /**
   * Returns a copy of this definition with the roll convention replaced.
   *
   * @param rollConvention  the convention defining how to roll dates, or `None` to leave it to be
   *   implied from the first date of the calculation
   * @return the copy, or the failures naming the broken constraint; a convention takes part in none
   *   of the date orderings the factory checks, and whether the calculated dates match it is
   *   decided by schedule creation
   */
  def withRollConvention(
      rollConvention: Option[RollConvention]): EitherNec[Failure, PeriodicSchedule] =
    copyWith(rollConvention = rollConvention)

  /**
   * Returns a copy of this definition with the first regular start date replaced.
   *
   * @param firstRegularStartDate  the unadjusted start date of the first regular period, or `None`
   *   for a schedule with no dated initial stub
   * @return the copy, or the failures naming the broken constraint: a first regular start date must
   *   fall on or after the effective start - the unadjusted date of a declared override start date,
   *   the start date otherwise - and on or before both the end date and a declared last regular end
   *   date
   */
  def withFirstRegularStartDate(
      firstRegularStartDate: Option[LocalDate]): EitherNec[Failure, PeriodicSchedule] =
    copyWith(firstRegularStartDate = firstRegularStartDate)

  /**
   * Returns a copy of this definition with the last regular end date replaced.
   *
   * @param lastRegularEndDate  the unadjusted end date of the last regular period, or `None` for a
   *   schedule with no dated final stub
   * @return the copy, or the failures naming the broken constraint: a last regular end date must
   *   fall on or after the effective start - the unadjusted date of a declared override start date,
   *   the start date otherwise - and on or after a declared first regular start date, and on or
   *   before the end date
   */
  def withLastRegularEndDate(
      lastRegularEndDate: Option[LocalDate]): EitherNec[Failure, PeriodicSchedule] =
    copyWith(lastRegularEndDate = lastRegularEndDate)

  /**
   * Returns a copy of this definition with the override start date replaced.
   *
   * @param overrideStartDate  the start date of the first schedule period, overriding normal
   *   schedule generation, or `None` to generate it normally
   * @return the copy, or the failures naming the broken constraint: the unadjusted date of an
   *   override start date must fall strictly before the end date and on or before a declared first
   *   regular start date and last regular end date, and it is the effective start that those two
   *   comparisons are made against
   */
  def withOverrideStartDate(
      overrideStartDate: Option[AdjustableDate]): EitherNec[Failure, PeriodicSchedule] =
    copyWith(overrideStartDate = overrideStartDate)

  /**
   * Copies this definition, replacing the properties named by the caller.
   *
   * Every `with*` member above, and [[replaceStartDate]], is one call to this method, and this
   * method is one call to the validating factory of the companion. That is what makes it impossible
   * to derive a definition that the factory would have refused: there is no other route from one
   * definition to another.
   *
   * @return the copy, or the failures naming every one of the seven date orderings of [[of]] that
   *   the new values break
   */
  private def copyWith(
      startDate: LocalDate = this.startDate,
      endDate: LocalDate = this.endDate,
      frequency: Frequency = this.frequency,
      businessDayAdjustment: BusinessDayAdjustment = this.businessDayAdjustment,
      startDateBusinessDayAdjustment: Option[BusinessDayAdjustment] =
        this.startDateBusinessDayAdjustment,
      endDateBusinessDayAdjustment: Option[BusinessDayAdjustment] =
        this.endDateBusinessDayAdjustment,
      stubConvention: Option[StubConvention] = this.stubConvention,
      rollConvention: Option[RollConvention] = this.rollConvention,
      firstRegularStartDate: Option[LocalDate] = this.firstRegularStartDate,
      lastRegularEndDate: Option[LocalDate] = this.lastRegularEndDate,
      overrideStartDate: Option[AdjustableDate] = this.overrideStartDate)
      : EitherNec[Failure, PeriodicSchedule] =
    PeriodicSchedule.of(
      startDate,
      endDate,
      frequency,
      businessDayAdjustment,
      startDateBusinessDayAdjustment,
      endDateBusinessDayAdjustment,
      stubConvention,
      rollConvention,
      firstRegularStartDate,
      lastRegularEndDate,
      overrideStartDate)

  /**
   * Builds the failure this definition reports, attaching itself under the `definition` attribute.
   *
   * Every rejection this type reports carries the definition that was rejected under that
   * attribute, so a report can name the definition without the message having to embed it.
   *
   * ===Why the definition is attached exactly as it renders===
   *
   * The attribute holds the text of [[toString]] as it stands, with nothing dropped, shortened or
   * escaped. That is what the attribute is for: a report has to be able to name the definition that
   * was rejected, and a caller has to be able to compare it with the definition it supplied. Either
   * of those reads a summary rather than the definition if this method alters the text, which is
   * why it does not.
   *
   * Part of that text is nevertheless outside this library's control. A definition embeds a
   * [[BusinessDayAdjustment]], which names a
   * [[com.opengamma.strata.basics.date.HolidayCalendarId]], and that identifier is total in its
   * name: `HolidayCalendarId.of` accepts any text at all, so a calendar name arriving from a
   * document or a caller may hold a line feed, a control character, or several thousand characters
   * of anything. Making such text safe to write out therefore belongs to the writing of a failure
   * rather than to the reporting of one, and this method neither bounds nor escapes the text it
   * attaches.
   *
   * @param message  the message stating what was rejected
   * @return the failure
   */
  private def failure(message: String): Failure =
    Failure.Invalid(message).withAttribute(DefinitionAttribute, toString)

  /** Builds the message reporting that a date does not match the convention being rolled with. */
  private def rollMismatchMessage(
      date: LocalDate,
      rollConv: RollConvention,
      rollingBackwards: Boolean): String = {
    val direction = if (rollingBackwards) "backwards" else "forwards"
    s"Date '$date' does not match roll convention '${rollConv.name}' " +
      s"when starting to roll $direction"
  }

  /**
   * Renders a list of dates as the messages of this type name one.
   *
   * The two messages that name a date list render it through this member: the dates between square
   * brackets, separated by a comma and a space - `[2014-01-01, 2014-02-01]`. Interpolating a `List`
   * directly would produce `List(2014-01-01, 2014-02-01)` instead, which is not the form these
   * messages carry.
   *
   * @param dates  the dates to render, in the order they are to appear
   * @return the dates as a message renders them
   */
  private def dateList(dates: List[LocalDate]): String = dates.mkString("[", ", ", "]")

  /**
   * Reports duplicated unadjusted dates, naming the list as it was before deduplication.
   *
   * Every date in that list was generated by this definition, so the message quotes nothing that
   * reached the library from outside it; the definition the failure carries is attached by
   * [[failure]], under the policy described there.
   */
  private def duplicateUnadjusted(unadj: List[LocalDate]): Option[Failure] =
    Option.when(unadj.distinct.sizeIs < unadj.size)(
      failure(s"Schedule calculation resulted in duplicate unadjusted dates ${dateList(unadj)}"))

  /**
   * Reports duplicated adjusted dates, naming both lists and the adjustment that produced them.
   *
   * The adjustment is quoted as it renders, which embeds an unconstrained holiday calendar name,
   * for the reason [[failure]] gives: the message states what was rejected, and the neutralising
   * of text that arrived from outside happens where a failure is written out.
   */
  private def duplicateAdjusted(
      unadj: List[LocalDate],
      adj: List[LocalDate]): Option[Failure] =
    Option.when(adj.distinct.sizeIs < adj.size)(
      failure(
        s"Schedule calculation resulted in duplicate adjusted dates ${dateList(adj)} " +
          s"from unadjusted dates ${dateList(unadj)} using adjustment '$businessDayAdjustment'"))

  /**
   * Answers with the unadjusted dates, or with the failure that they contain duplicates.
   *
   * A list with no duplicates is its own deduplication, so the dates are returned as they stand.
   */
  private def deduplicatedUnadjusted(unadj: List[LocalDate]): Either[Failure, List[LocalDate]] =
    duplicateUnadjusted(unadj) match {
      case Some(duplicated) => Left(duplicated)
      case None => Right(unadj)
    }

  /** Answers with the adjusted dates, or with the failure that they contain duplicates. */
  private def deduplicatedAdjusted(
      unadj: List[LocalDate],
      adj: List[LocalDate]): Either[Failure, List[LocalDate]] =
    duplicateAdjusted(unadj, adj) match {
      case Some(duplicated) => Left(duplicated)
      case None => Right(adj)
    }

  /**
   * Returns a string describing this definition.
   *
   * The form is the four properties every definition has, followed by those of the seven optional
   * properties that are present, each under the name the codec writes it under.
   *
   * The rendering is deterministic - the properties appear in their declaration order and an absent
   * property contributes nothing - because this is the text that every failure of this type carries
   * under its `definition` attribute.
   *
   * It is a faithful rendering rather than a safe one: the business day adjustments it names embed
   * a holiday calendar name that nothing constrains, so this text can be of any length and can
   * hold any character. That is deliberate, and [[failure]] states why - a caller reads the
   * definition it supplied, and the bounding and escaping of such text happens where a failure is
   * written out, not here.
   *
   * @return the text form of this definition
   */
  override def toString: String = {
    val required = List(
      s"startDate=$startDate",
      s"endDate=$endDate",
      s"frequency=$frequency",
      s"businessDayAdjustment=$businessDayAdjustment")
    val optional = List(
      startDateBusinessDayAdjustment.map(value => s"startDateBusinessDayAdjustment=$value"),
      endDateBusinessDayAdjustment.map(value => s"endDateBusinessDayAdjustment=$value"),
      stubConvention.map(value => s"stubConvention=$value"),
      rollConvention.map(value => s"rollConvention=$value"),
      firstRegularStartDate.map(value => s"firstRegularStartDate=$value"),
      lastRegularEndDate.map(value => s"lastRegularEndDate=$value"),
      overrideStartDate.map(value => s"overrideStartDate=$value")).flatten
    (required ::: optional).mkString("PeriodicSchedule(", ", ", ")")
  }
}

/**
 * Provides the four ways of obtaining a periodic schedule definition, the invariants every one of
 * them checks, and the instances for the type.
 *
 * The factories below cover the minimal shape of the four required properties, a shape stated by a
 * stub convention and an end-of-month preference, a shape stated by the stub and roll conventions,
 * and the full shape of all eleven properties; the `with*` members of the type itself derive one
 * definition from another. Every one of them funnels through [[of]], so the invariants are checked
 * once, in one place, however a definition is arrived at.
 *
 * None of the factories declares a default argument, deliberately: Scala permits defaults on at
 * most one alternative of an overloaded name, and giving them to one of four would make the other
 * three read as the exceptions rather than as peers.
 */
object PeriodicSchedule {

  private val StartDateName: String = "startDate"

  private val EndDateName: String = "endDate"

  /**
   * The name the start date is reported under when it is checked against a regular date.
   *
   * The two checks that compare the start date with a first regular start date or a last regular
   * end date report it as `unadjusted` rather than as `startDate`, and that name is part of the
   * message a caller reads.
   */
  private val UnadjustedName: String = "unadjusted"

  private val OverrideStartDateName: String = "overrideStartDate"

  private val FirstRegularStartDateName: String = "firstRegularStartDate"

  private val LastRegularEndDateName: String = "lastRegularEndDate"

  /** The attribute the rejected definition is attached to every failure under. */
  private val DefinitionAttribute: String = "definition"

  /** The message reporting a dated stub on a schedule whose frequency is 'Term'. */
  private val TermExplicitStubsMessage: String =
    "Explicit stubs must not be specified when using 'Term' frequency"

  /**
   * The message reporting an adjacent pair of generated dates that is not strictly in order.
   *
   * It is reported only where regenerating the dates from the declared start and end dates, and
   * adjusting them again, reports nothing more specific.
   */
  private val InvalidPeriodMessage: String = "Schedule calculation resulted in invalid period"

  /**
   * The greatest number of periods a single schedule generation will produce.
   *
   * Every property of a definition is chosen by its caller, including the frequency and the two
   * dates, so the number of periods a generation is asked for is caller-controlled: without a
   * ceiling, a one-day frequency over a span of centuries would walk and materialise every
   * boundary in it, spending time and heap in proportion to a number the caller supplied. One
   * hundred thousand periods is roughly two hundred and seventy-four years of daily periods - far
   * beyond the longest schedule this library is ever asked to build, and further still beyond
   * anything expressible at the monthly and quarterly frequencies that dominate its use - while
   * bounding the list a generation can materialise to a few megabytes.
   *
   * The ceiling is enforced in three places, which together answer "refused before the work is
   * done", "no more than this is ever materialised" and "the number the message names is the
   * number that is checked":
   *
   *  - [[provablyExceedsPeriodCount]] refuses a span that is '''provably''' above the ceiling
   *    before either walk begins, in constant time and without materialising anything. It refuses
   *    nothing else: the quotient it compares is a lower bound on the steps the walk must take, so
   *    a definition it refuses could not have been generated within the ceiling.
   *  - [[rolledDates]] bounds the walk itself, so however wide the dates and however short the
   *    frequency, the list a walk materialises cannot exceed the ceiling even where the preflight
   *    could prove nothing.
   *  - [[boundedDates]] applies the ceiling to the assembled date list, which is the count this
   *    message is about: `n` dates are `n - 1` periods, and the dates include the schedule's ends
   *    and any dated stub as well as the boundaries the walk rolled.
   */
  private val MaximumPeriodCount: Int = 100000

  /** The message reporting a generation that asks for more periods than the maximum. */
  private val TooManyPeriodsMessage: String =
    s"Schedule calculation resulted in more than $MaximumPeriodCount periods, which is the " +
      "maximum number of periods that can be generated"

  /**
   * The greatest number of days the roll convention's adjustment can add to a stepped date.
   *
   * One generated step is `date.plus(period)` followed by the adjustment of the roll convention,
   * and that adjustment can only move the stepped date '''within its own month''' - the numeric
   * day-of-month conventions, `EOM`, and the IMM-family conventions all answer with a date in the
   * month of the date they were given - or at most six days forward, which is what the day-of-week
   * conventions do. Thirty-one days is therefore an upper bound on the adjustment in either
   * direction, and adding it to the length of the frequency's period gives an upper bound on the
   * distance one step covers.
   */
  private val MaxRollAdjustmentDays: Long = 31L

  /**
   * The greatest number of days the fallback step of a roll convention can cover.
   *
   * Where adding the frequency and adjusting the result lands on or before the date stepped from -
   * which happens when the frequency is shorter than the granularity of the convention, such as a
   * daily frequency rolling on the third Wednesday - the convention steps by one month instead.
   * That substituted step spans at most thirty-one days, so the date being adjusted is at most the
   * greater of this and the period's own length away from the date stepped from, and the bound
   * below takes that greater value.
   */
  private val MaxFallbackStepDays: Long = 31L

  /**
   * Decides whether a walk over the given span is provably asking for more than the maximum.
   *
   * This is the preflight of both walks: a constant-time refusal of a span so wide that no walk
   * over it can stay within [[MaximumPeriodCount]], computed before anything is materialised. It
   * is deliberately '''conservative''' - it answers true only for spans that are provably above
   * the ceiling, and therefore refuses nothing a generation would have accepted:
   *
   *  - `spanDays` is the exact number of days between the two ends of the walk, which is safe over
   *    the whole range of `LocalDate` in a `Long`.
   *  - `maxStepDays` is an upper bound on the distance one generated step can cover: a year spans
   *    at most 366 days and a month at most 31, the fallback step of a convention spans at most
   *    [[MaxFallbackStepDays]], and the adjustment applied afterwards adds at most
   *    [[MaxRollAdjustmentDays]]. The `1` guards a frequency whose components sum to zero, which a
   *    validated [[Frequency]] cannot have but which costs nothing to exclude.
   *  - because every step advances at most `maxStepDays` days, the integer quotient
   *    `spanDays / maxStepDays` is a true '''lower''' bound on the number of steps the walk must
   *    take to cross the span. When that lower bound is itself above the ceiling, the walk would
   *    produce more boundaries than the ceiling allows whatever the roll convention does, so the
   *    definition could not have been generated and is refused here instead of walking.
   *
   * A span whose end is not after its start yields a quotient that is zero or negative, so it is
   * never refused by this: an inverted or empty span is a matter for the checks that own it.
   *
   * It is package-visible rather than private so that the rest of this package can apply the
   * predicate on its own. A refusal from here and a refusal from the walk carry the same message,
   * so nothing else distinguishes them from outside.
   *
   * @param walkStart  the earlier end of the span the walk traverses
   * @param walkEnd  the later end of the span the walk traverses
   * @param frequency  the periodic frequency each step of the walk applies
   * @return true if no walk over this span with this frequency can stay within the maximum
   */
  private[schedule] def provablyExceedsPeriodCount(
      walkStart: LocalDate,
      walkEnd: LocalDate,
      frequency: Frequency): Boolean = {
    val spanDays: Long = ChronoUnit.DAYS.between(walkStart, walkEnd)
    val period: Period = frequency.period
    val periodDays: Long =
      period.getYears.toLong * 366L + period.getMonths.toLong * 31L + period.getDays.toLong
    val maxStepDays: Long =
      math.max(math.max(1L, periodDays), MaxFallbackStepDays) + MaxRollAdjustmentDays
    spanDays / maxStepDays > MaximumPeriodCount.toLong
  }

  /**
   * The message reporting date arithmetic that leaves the range of representable dates.
   *
   * The arithmetic of a roll is total over almost the whole of `LocalDate`, and fails only within
   * one frequency of the two extremes, so this reports a definition whose dates sit at the very
   * edge of what `java.time` can represent rather than anything about the schedule's shape.
   */
  private val DateRangeMessage: String =
    "Schedule calculation moved outside the range of supported dates"

  /** The message reporting a replacement start date that falls after the end date. */
  private val StartDateAfterEndDateMessage: String =
    "Cannot alter leg to have start date after end date"

  /**
   * The ordering of dates the order checks below are performed with.
   *
   * The checking helpers are generic in the type being compared and take its cats ordering, and
   * cats publishes no instance for `java.time.LocalDate` - the class implements
   * `Comparable[ChronoLocalDate]` rather than `Comparable[LocalDate]`, so the ordering derived from
   * a comparable type does not apply to it either. The instance is therefore stated here, as the
   * natural time-line order the class itself defines, and kept private: it exists to serve the
   * checks of this file, and publishing an ordering for a type this module does not own would put
   * an instance into implicit scope for every file that imports anything from here.
   */
  private implicit val dateOrder: Order[LocalDate] =
    Order.from((first, second) => first.compareTo(second))

  /**
   * Obtains an instance from the four properties every definition has.
   *
   * The business day adjustment is used for all dates. There are no stub dates and no conventions,
   * so the stub convention is implied to be 'None' and the roll convention is implied from the
   * first date of the calculation - which means the schedule must divide evenly by the frequency.
   *
   * This is the minimal shape of a definition; the `with*` members of the result add the optional
   * properties one at a time.
   *
   * @param unadjustedStartDate  the start date, which is the start of the first schedule period
   * @param unadjustedEndDate  the end date, which is the end of the last schedule period
   * @param frequency  the regular periodic frequency
   * @param businessDayAdjustment  the business day adjustment to apply
   * @return the definition, or the failure naming the broken constraint: the start date must fall
   *   strictly before the end date, which is the only ordering these four arguments can break
   */
  def of(
      unadjustedStartDate: LocalDate,
      unadjustedEndDate: LocalDate,
      frequency: Frequency,
      businessDayAdjustment: BusinessDayAdjustment): EitherNec[Failure, PeriodicSchedule] =
    of(
      unadjustedStartDate,
      unadjustedEndDate,
      frequency,
      businessDayAdjustment,
      None,
      None,
      None,
      None,
      None,
      None,
      None)

  /**
   * Obtains an instance based on a stub convention and end-of-month flag.
   *
   * The business day adjustment is used for all dates. The stub convention is used to determine
   * whether there are any stubs. If the end-of-month flag is true, then in any case of ambiguity
   * the end-of-month will be chosen.
   *
   * @param unadjustedStartDate  the start date, which is the start of the first schedule period
   * @param unadjustedEndDate  the end date, which is the end of the last schedule period
   * @param frequency  the regular periodic frequency
   * @param businessDayAdjustment  the business day adjustment to apply
   * @param stubConvention  the convention defining how to handle stubs
   * @param preferEndOfMonth  whether to prefer the end-of-month when rolling
   * @return the definition, or the failure naming the broken constraint: the start date must fall
   *   strictly before the end date; whether the stub convention agrees with the dates is decided by
   *   schedule creation rather than here
   */
  def of(
      unadjustedStartDate: LocalDate,
      unadjustedEndDate: LocalDate,
      frequency: Frequency,
      businessDayAdjustment: BusinessDayAdjustment,
      stubConvention: StubConvention,
      preferEndOfMonth: Boolean): EitherNec[Failure, PeriodicSchedule] =
    of(
      unadjustedStartDate,
      unadjustedEndDate,
      frequency,
      businessDayAdjustment,
      None,
      None,
      Some(stubConvention),
      if (preferEndOfMonth) Some(RollConventions.EOM) else None,
      None,
      None,
      None)

  /**
   * Obtains an instance based on roll and stub conventions.
   *
   * The business day adjustment is used for all dates. The stub convention is used to determine
   * whether there are any stubs. The roll convention is used to fine tune each rolled date.
   *
   * @param unadjustedStartDate  the start date, which is the start of the first schedule period
   * @param unadjustedEndDate  the end date, which is the end of the last schedule period
   * @param frequency  the regular periodic frequency
   * @param businessDayAdjustment  the business day adjustment to apply
   * @param stubConvention  the convention defining how to handle stubs
   * @param rollConvention  the convention defining how to roll dates
   * @return the definition, or the failure naming the broken constraint: the start date must fall
   *   strictly before the end date; whether the two conventions agree with the dates is decided by
   *   schedule creation rather than here
   */
  def of(
      unadjustedStartDate: LocalDate,
      unadjustedEndDate: LocalDate,
      frequency: Frequency,
      businessDayAdjustment: BusinessDayAdjustment,
      stubConvention: StubConvention,
      rollConvention: RollConvention): EitherNec[Failure, PeriodicSchedule] =
    of(
      unadjustedStartDate,
      unadjustedEndDate,
      frequency,
      businessDayAdjustment,
      None,
      None,
      Some(stubConvention),
      Some(rollConvention),
      None,
      None,
      None)

  /**
   * Obtains an instance from all eleven properties.
   *
   * This is the funnel every other route into the type passes through - the three factories above,
   * the `with*` members of the type, [[PeriodicSchedule.replaceStartDate]] and the decoder below.
   *
   * The seven optional properties have to be named, `None` included. There is no default for them,
   * for the reason given on this object, and the four-argument factory above is the shape a caller
   * that wants none of them reaches for.
   *
   * ===What is checked===
   *
   * Seven invariants, '''accumulated rather than sequenced''', so that a caller supplying several
   * badly ordered dates is told about all of them at once instead of correcting one and being sent
   * back for the next.
   *
   *  1. the start date falls strictly before the end date;
   *  1. an override start date, where present, falls strictly before the end date;
   *  1. a first regular start date, where present, falls on or before the end date;
   *  1. a first regular start date and a last regular end date, where both are present, are in that
   *     order or equal;
   *  1. the effective start - the override start date where there is one, the start date
   *     otherwise - falls on or before a first regular start date that is present;
   *  1. the same effective start falls on or before a last regular end date that is present;
   *  1. a last regular end date, where present, falls on or before the end date.
   *
   * The two checks against the effective start report it under the name of whichever property
   * supplied it, which for the start date is the name `unadjusted`.
   *
   * Note what is '''not''' checked: nothing about whether the frequency divides the term, whether
   * the conventions agree with the dates, or whether a stub is allowed. Those depend on rolling the
   * schedule out, so they are decided by schedule creation and reported by it.
   *
   * @param startDate  the start date, which is the start of the first schedule period
   * @param endDate  the end date, which is the end of the last schedule period
   * @param frequency  the regular periodic frequency
   * @param businessDayAdjustment  the business day adjustment to apply
   * @param startDateBusinessDayAdjustment  the business day adjustment of the start date, if any
   * @param endDateBusinessDayAdjustment  the business day adjustment of the end date, if any
   * @param stubConvention  the convention defining how to handle stubs, if any
   * @param rollConvention  the convention defining how to roll dates, if any
   * @param firstRegularStartDate  the start date of the first regular period, if any
   * @param lastRegularEndDate  the end date of the last regular period, if any
   * @param overrideStartDate  the overriding start date of the first period, if any
   * @return the definition, or the failures naming every one of the seven orderings above that the
   *   arguments break
   */
  def of(
      startDate: LocalDate,
      endDate: LocalDate,
      frequency: Frequency,
      businessDayAdjustment: BusinessDayAdjustment,
      startDateBusinessDayAdjustment: Option[BusinessDayAdjustment],
      endDateBusinessDayAdjustment: Option[BusinessDayAdjustment],
      stubConvention: Option[StubConvention],
      rollConvention: Option[RollConvention],
      firstRegularStartDate: Option[LocalDate],
      lastRegularEndDate: Option[LocalDate],
      overrideStartDate: Option[AdjustableDate]): EitherNec[Failure, PeriodicSchedule] =
    validated(startDate, endDate, firstRegularStartDate, lastRegularEndDate, overrideStartDate)
      .map(_ =>
        create(
          startDate,
          endDate,
          frequency,
          businessDayAdjustment,
          startDateBusinessDayAdjustment,
          endDateBusinessDayAdjustment,
          stubConvention,
          rollConvention,
          firstRegularStartDate,
          lastRegularEndDate,
          overrideStartDate))
      .toEither

  /**
   * Runs the seven order invariants of a definition, accumulating every one that does not hold.
   *
   * Only the five date-bearing properties take part; the frequency and the three adjustments carry
   * their own invariants and place no constraint on each other.
   *
   * @param startDate  the start date of the schedule
   * @param endDate  the end date of the schedule
   * @param firstRegularStartDate  the start date of the first regular period, if any
   * @param lastRegularEndDate  the end date of the last regular period, if any
   * @param overrideStartDate  the overriding start date of the first period, if any
   * @return the passing outcome, or every failure the dates produce
   */
  private def validated(
      startDate: LocalDate,
      endDate: LocalDate,
      firstRegularStartDate: Option[LocalDate],
      lastRegularEndDate: Option[LocalDate],
      overrideStartDate: Option[AdjustableDate]): ValidatedFailures[Unit] = {
    val (effectiveStart, effectiveStartName) =
      overrideStartDate.fold((startDate, UnadjustedName))(override_ =>
        (override_.unadjusted, OverrideStartDateName))
    (
      before(startDate, endDate, StartDateName, EndDateName),
      whenPresent(overrideStartDate)(override_ =>
        before(override_.unadjusted, endDate, OverrideStartDateName, EndDateName)),
      whenPresent(firstRegularStartDate)(firstRegular =>
        notAfter(firstRegular, endDate, FirstRegularStartDateName, EndDateName)),
      whenPresent(firstRegularStartDate.zip(lastRegularEndDate)) {
        case (firstRegular, lastRegular) =>
          notAfter(firstRegular, lastRegular, FirstRegularStartDateName, LastRegularEndDateName)
      },
      whenPresent(firstRegularStartDate)(firstRegular =>
        notAfter(effectiveStart, firstRegular, effectiveStartName, FirstRegularStartDateName)),
      whenPresent(lastRegularEndDate)(lastRegular =>
        notAfter(effectiveStart, lastRegular, effectiveStartName, LastRegularEndDateName)),
      whenPresent(lastRegularEndDate)(lastRegular =>
        notAfter(lastRegular, endDate, LastRegularEndDateName, EndDateName))
    ).mapN((_, _, _, _, _, _, _) => ())
  }

  /** Checks that the first date falls strictly before the second, discarding the dates checked. */
  private def before(
      obj1: LocalDate,
      obj2: LocalDate,
      name1: String,
      name2: String): ValidatedFailures[Unit] =
    Validate.inOrderNotEqual(obj1, obj2, name1, name2).map(_ => ())

  /** Checks that the first date falls on or before the second, discarding the dates checked. */
  private def notAfter(
      obj1: LocalDate,
      obj2: LocalDate,
      name1: String,
      name2: String): ValidatedFailures[Unit] =
    Validate.inOrderOrEqual(obj1, obj2, name1, name2).map(_ => ())

  /** Runs a check against a property that may be absent, passing where it is. */
  private def whenPresent[A](value: Option[A])(
      check: A => ValidatedFailures[Unit]): ValidatedFailures[Unit] =
    value.fold(Validate.valid(()))(check)

  /**
   * Creates a value, which every route into the type funnels through.
   *
   * This is the only instantiation of the type and it is private, so [[of]] is the only way into it
   * from outside this file. The constructor of a `sealed abstract case class` is reachable only
   * from inside the file that declares it, and [[Impl]] - a subclass of the abstract case class,
   * declared and hidden here - is how it is reached; that is what leaves the type without a public
   * `apply` or `copy` while keeping the `equals`, `hashCode` and `unapply` a case class provides.
   *
   * The method performs no check of its own, because its one caller has already run all seven.
   */
  private def create(
      startDate: LocalDate,
      endDate: LocalDate,
      frequency: Frequency,
      businessDayAdjustment: BusinessDayAdjustment,
      startDateBusinessDayAdjustment: Option[BusinessDayAdjustment],
      endDateBusinessDayAdjustment: Option[BusinessDayAdjustment],
      stubConvention: Option[StubConvention],
      rollConvention: Option[RollConvention],
      firstRegularStartDate: Option[LocalDate],
      lastRegularEndDate: Option[LocalDate],
      overrideStartDate: Option[AdjustableDate]): PeriodicSchedule =
    new Impl(
      startDate,
      endDate,
      frequency,
      businessDayAdjustment,
      startDateBusinessDayAdjustment,
      endDateBusinessDayAdjustment,
      stubConvention,
      rollConvention,
      firstRegularStartDate,
      lastRegularEndDate,
      overrideStartDate)

  /**
   * The one implementation of a periodic schedule.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it. It is declared rather than written as an anonymous subclass at the instantiation site
   * for two reasons, both about what the class file says: a private member class is one a Java
   * compiler refuses to name, where an anonymous class is public and can be instantiated directly
   * by a caller in another language, and a named class can be compared against, which is what
   * lets [[PeriodicSchedule]] refuse in its own constructor to be any other implementation.
   *
   * @param startDate  the start date, as [[create]] received it
   * @param endDate  the end date, as [[create]] received it
   * @param frequency  the frequency, as [[create]] received it
   * @param businessDayAdjustment  the business day adjustment, as [[create]] received it
   * @param startDateBusinessDayAdjustment  the optional start date adjustment
   * @param endDateBusinessDayAdjustment  the optional end date adjustment
   * @param stubConvention  the optional stub convention
   * @param rollConvention  the optional roll convention
   * @param firstRegularStartDate  the optional first regular start date
   * @param lastRegularEndDate  the optional last regular end date
   * @param overrideStartDate  the optional override start date
   */
  private final class Impl(
      startDate: LocalDate,
      endDate: LocalDate,
      frequency: Frequency,
      businessDayAdjustment: BusinessDayAdjustment,
      startDateBusinessDayAdjustment: Option[BusinessDayAdjustment],
      endDateBusinessDayAdjustment: Option[BusinessDayAdjustment],
      stubConvention: Option[StubConvention],
      rollConvention: Option[RollConvention],
      firstRegularStartDate: Option[LocalDate],
      lastRegularEndDate: Option[LocalDate],
      overrideStartDate: Option[AdjustableDate])
      extends PeriodicSchedule(
        startDate,
        endDate,
        frequency,
        businessDayAdjustment,
        startDateBusinessDayAdjustment,
        endDateBusinessDayAdjustment,
        stubConvention,
        rollConvention,
        firstRegularStartDate,
        lastRegularEndDate,
        overrideStartDate)

  /**
   * Merges runs of dates whose adjusted values coincide, keeping one boundary per run.
   *
   * Two adjacent unadjusted dates that a business day adjustment maps onto the same business day
   * describe a period of no length, which no schedule can hold. This collapses such a run to a
   * single boundary, discarding the unadjusted date at each position dropped.
   *
   * The boundary kept is the '''last''' of each run: a date is dropped for as long as its adjusted
   * date equals the next one's, so the survivor of a run of three is the third of its unadjusted
   * dates rather than the first. The adjusted dates of a run are equal by definition, so the choice
   * is visible only in the unadjusted schedule - and it is visible there, which is why the run is
   * collapsed from its end rather than from its start.
   *
   * The recursion is tail recursive and threads both results as parameters, so no list is mutated
   * and no accumulator is reassigned; the two results come back in order because each is reversed
   * at the end.
   *
   * @param remaining  the unadjusted dates paired with their adjusted dates, in order
   * @param unadjAcc  the unadjusted dates kept up to this point, in reverse order
   * @param adjAcc  the adjusted dates kept up to this point, in reverse order
   * @return the unadjusted and adjusted dates that survive, both in order
   */
  @tailrec
  private def combineCoincident(
      remaining: List[(LocalDate, LocalDate)],
      unadjAcc: List[LocalDate],
      adjAcc: List[LocalDate]): (List[LocalDate], List[LocalDate]) =
    remaining match {
      case Nil => (unadjAcc.reverse, adjAcc.reverse)
      case (unadjDate, adjDate) :: rest =>
        val coincidesWithNext = rest.headOption.exists { case (_, nextAdj) => nextAdj == adjDate }
        if (coincidesWithNext) {
          combineCoincident(rest, unadjAcc, adjAcc)
        } else {
          combineCoincident(rest, unadjDate :: unadjAcc, adjDate :: adjAcc)
        }
    }

  /**
   * Recovers the unadjusted date that a date already adjusted was adjusted from, where it can.
   *
   * For 'EOM' and the day-of-month roll conventions the candidate is the roll day-of-month of the
   * base date's own month, capped at the length of that month so that a 31st roll day is the 28th
   * or 29th in February. For the other conventions - the ones whose roll day is computed from the
   * month or the week - the candidate is the date the convention itself rolls the base date to. In
   * both cases the candidate is accepted only if adjusting it reproduces the base date exactly,
   * which is the evidence that the base date is an adjusted one; otherwise the base date is
   * returned as it stands.
   *
   * Two caveats apply. Where the roll day is computed relative to the month, the recovery takes
   * the adjusted date not to have crossed a month boundary, which is safe because such roll days
   * are not close to the end of a month and no reasonable adjustment moves that far. Where it is
   * computed relative to the week, the convention rolls '''forward''' from the date given, so the
   * recovery does not work for a base date that was itself adjusted forwards - the candidate is
   * then a later date than the base date and the equality test simply fails, leaving the base date
   * unchanged.
   *
   * @param baseDate  the date to recover an unadjusted date from
   * @param rollConvention  the roll convention that implies the candidate
   * @param businessDayAdjustment  the adjustment the candidate is tested with
   * @param refData  the reference data, used to find the holiday calendars
   * @return the recovered unadjusted date, or the base date where none is recovered, or the failure
   *   that the adjustment testing the candidate names a holiday calendar the reference data does
   *   not supply
   */
  private def calculatedUnadjustedDateFromAdjusted(
      baseDate: LocalDate,
      rollConvention: RollConvention,
      businessDayAdjustment: BusinessDayAdjustment,
      refData: ReferenceData): Either[Failure, LocalDate] = {
    val rollDom = rollConvention.dayOfMonth
    if (rollDom > 0 && baseDate.getDayOfMonth != rollDom) {
      val actualDom = math.min(rollDom, baseDate.lengthOfMonth)
      if (baseDate.getDayOfMonth == actualDom) {
        // the base date is already the expected day, so there is nothing to recover
        Right(baseDate)
      } else {
        recovered(baseDate, baseDate.withDayOfMonth(actualDom), businessDayAdjustment, refData)
      }
    } else if (rollDom == 0) {
      // a zero roll day implies that the roll date is calculated relative to the month or week,
      // so the candidate is the valid roll date the convention itself produces
      val rollImpliedDate = rollConvention.adjust(baseDate)
      if (rollImpliedDate == baseDate) {
        Right(baseDate)
      } else {
        recovered(baseDate, rollImpliedDate, businessDayAdjustment, refData)
      }
    } else {
      Right(baseDate)
    }
  }

  /** Accepts the candidate where adjusting it gives the base date back, else the base date. */
  private def recovered(
      baseDate: LocalDate,
      rollImpliedDate: LocalDate,
      businessDayAdjustment: BusinessDayAdjustment,
      refData: ReferenceData): Either[Failure, LocalDate] =
    businessDayAdjustment
      .adjust(rollImpliedDate, refData)
      .map(adjusted => if (adjusted == baseDate) rollImpliedDate else baseDate)

  /**
   * The hashing and equality of schedule definitions.
   *
   * Equality and hashing are those of the case class, which compare all eleven properties. No
   * property holds a `Double`, so there is no bit-pattern comparison to arrange; each has an
   * equality of its own that this one is built from.
   *
   * There is deliberately '''no''' `Order`: no ordering of definitions is meaningful - two
   * definitions differing in their conventions are not ranked by anything.
   *
   * @return the hashing and equality of schedule definitions
   */
  implicit val hash: Hash[PeriodicSchedule] = Hash.fromUniversalHashCode[PeriodicSchedule]

  /**
   * The rendering of schedule definitions as text.
   *
   * This is [[PeriodicSchedule.toString]], which is also the text attached to every failure a
   * definition reports.
   *
   * @return the rendering of schedule definitions as text
   */
  implicit val show: Show[PeriodicSchedule] = Show.show(_.toString)

  /**
   * The field shape the codecs are derived over.
   *
   * The derivation reads the public constructor of a product, and a validated type has none - it is
   * an abstract case class whose constructor is private - so there is no public shape to derive
   * from. Writing the eleven fields out by hand instead would state the same contract a second
   * time.
   *
   * Each field is decided by the codec of its own type: a date is an ISO date string, a frequency,
   * a stub convention and a roll convention are their names, and an adjustment and an adjustable
   * date are the objects their own codecs write.
   *
   * The shape is `java.io.Serializable`, because the compiler makes every `case class` so, and it
   * therefore mixes in [[NoJavaSerialization]] as every product of this port does: these fields
   * reach the library as JSON through the codecs below and in no other form.
   *
   * @param startDate  the start date, carried as its ISO date string
   * @param endDate  the end date, carried as its ISO date string
   * @param frequency  the periodic frequency, carried as its name
   * @param businessDayAdjustment  the business day adjustment, carried as its own object
   * @param startDateBusinessDayAdjustment  the start date's adjustment, omitted when absent
   * @param endDateBusinessDayAdjustment  the end date's adjustment, omitted when absent
   * @param stubConvention  the stub convention, carried as its name, omitted when absent
   * @param rollConvention  the roll convention, carried as its name, omitted when absent
   * @param firstRegularStartDate  the first regular start date, omitted when absent
   * @param lastRegularEndDate  the last regular end date, omitted when absent
   * @param overrideStartDate  the override start date, carried as its own object, omitted when
   *   absent
   */
  private final case class Raw(
      startDate: LocalDate,
      endDate: LocalDate,
      frequency: Frequency,
      businessDayAdjustment: BusinessDayAdjustment,
      startDateBusinessDayAdjustment: Option[BusinessDayAdjustment],
      endDateBusinessDayAdjustment: Option[BusinessDayAdjustment],
      stubConvention: Option[StubConvention],
      rollConvention: Option[RollConvention],
      firstRegularStartDate: Option[LocalDate],
      lastRegularEndDate: Option[LocalDate],
      overrideStartDate: Option[AdjustableDate])
      extends NoJavaSerialization

  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  private val rawEncoder: Encoder[Raw] = deriveEncoder[Raw]

  /**
   * The JSON encoding of schedule definitions.
   *
   * A value is an object of the four required properties followed by whichever of the seven
   * optional ones are present:
   *
   * {{{
   * {"startDate":"2014-06-17",
   *  "endDate":"2014-09-17",
   *  "frequency":"P1M",
   *  "businessDayAdjustment":{"convention":"ModifiedFollowing","calendar":"Sat/Sun"},
   *  "stubConvention":"ShortFinal",
   *  "rollConvention":"Day17"}
   * }}}
   *
   * A property holding no value is '''omitted''' from the object rather than written out with an
   * empty value, which matters here more than anywhere: seven of the eleven properties are optional
   * and a definition typically declares none of them.
   *
   * Both halves of the codec are assembled by the same compile-time derivation over the same raw
   * shape, which is what keeps them from drifting apart, and no part of the encoding inspects a
   * class while the program runs. Two equal values encode to identical bytes: the properties are
   * written in their declaration order and each has one form.
   *
   * @return the JSON encoding of schedule definitions
   */
  implicit val encoder: Encoder[PeriodicSchedule] =
    Codecs.dropNulls(rawEncoder.contramap[PeriodicSchedule] { value =>
      Raw(
        value.startDate,
        value.endDate,
        value.frequency,
        value.businessDayAdjustment,
        value.startDateBusinessDayAdjustment,
        value.endDateBusinessDayAdjustment,
        value.stubConvention,
        value.rollConvention,
        value.firstRegularStartDate,
        value.lastRegularEndDate,
        value.overrideStartDate)
    })

  /**
   * The JSON decoding of schedule definitions.
   *
   * This is the inverse of the encoding above, and it decides whether the properties describe a
   * definition exactly as a caller's arguments are decided: the payload is read into the raw shape
   * and handed to [[of]], so a document whose dates are out of order is a decoding failure carrying
   * every reason it is, rather than a value this type would not have built. The four required
   * properties have to be present; each of the other seven may be omitted, or present carrying
   * JSON's literal for no value, and both of those read as absent.
   *
   * @return the JSON decoding of schedule definitions
   */
  implicit val decoder: Decoder[PeriodicSchedule] =
    Codecs.validatedDecoder[Raw, PeriodicSchedule] { raw =>
      of(
        raw.startDate,
        raw.endDate,
        raw.frequency,
        raw.businessDayAdjustment,
        raw.startDateBusinessDayAdjustment,
        raw.endDateBusinessDayAdjustment,
        raw.stubConvention,
        raw.rollConvention,
        raw.firstRegularStartDate,
        raw.lastRegularEndDate,
        raw.overrideStartDate)
    }(rawDecoder)
}
