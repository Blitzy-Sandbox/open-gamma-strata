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
import cats.data.NonEmptyList
import cats.syntax.foldable._
import cats.syntax.traverse._

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.basics.date.DateAdjuster
import com.opengamma.strata.basics.date.DayCount
import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.ValidatedFailures
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * A complete schedule of periods (date ranges), with both unadjusted and adjusted dates.
 *
 * The schedule consists of one or more adjacent periods (date ranges). This is typically used as
 * the basis for financial calculations, such as accrual of interest.
 *
 * It is recommended to create a schedule using a [[PeriodicSchedule]].
 *
 * ===Construction===
 *
 * A schedule is a '''validated''' value, and it is a `sealed abstract case class` with a private
 * constructor, so there is no public `apply` and no `copy`: every route to a value goes through
 * [[Schedule.of]] or [[Schedule.ofTerm]]. It has two invariants, and they are carried differently.
 * That there is at least one period is carried by the type of [[periods]] rather than by a check,
 * so the state the bean being ported rejected at run time is not expressible here. That the
 * periods run from '''earliest to latest''' is checked by [[Schedule.of]], which reports a list
 * that runs backwards or overlaps and accepts one with gaps, because gaps are allowed and
 * disorder is not - every member that reads the periods reads them as a time line.
 *
 * ===Accessor naming===
 *
 * This type is a [[com.opengamma.strata.basics.date.DayCount.ScheduleInfo]], which is how a day
 * count reads the schedule surrounding the period it is accruing over, and that interface declares
 * `startDate`, `endDate` and `frequency` as `Option`s - the schedule facts an implementation may
 * not know. A schedule knows all of them, but one name cannot carry two types, so the naming is
 * settled once, here, and used consistently by everything in this port that reads a schedule:
 *
 *  - [[startDate]], [[endDate]], [[frequency]] and [[periodEndDate]] are '''the interface
 *    members''', answering `Some` (or, for `periodEndDate`, `Some` where a period contains the
 *    date). They are what a day count sees.
 *  - [[adjustedStartDate]], [[adjustedEndDate]] and [[periodicFrequency]] are '''the schedule's
 *    own values''', plainly typed, and they are what a caller holding a schedule reads. The date
 *    pair is named for what it is - the adjusted bounds of the schedule - which pairs it with
 *    [[unadjustedStartDate]] and [[unadjustedEndDate]], whose names clash with nothing and are
 *    therefore unchanged from the getters being ported.
 *
 * Every other member keeps the name of the Java getter with the `get` prefix dropped, so
 * `getRegularPeriods` is [[regularPeriods]], `getStubs` is [[stubs]], and so on.
 *
 * ===Divergences from the Java original===
 *
 * These are the points on which this port deliberately differs from
 * `com.opengamma.strata.basics.schedule.Schedule`, recorded here for `SCALA_MIGRATION.md`:
 *
 *  - '''"At least one period" becomes a type rather than a check.''' The bean declared its period
 *    list with `validate = "notEmpty"` and threw `IllegalArgumentException` on an empty list;
 *    [[periods]] is a `cats.data.NonEmptyList`, so an empty schedule cannot be built, cannot be
 *    decoded from JSON, and needs no check.
 *  - '''The documented chronology becomes a check.''' The bean documented its period list as
 *    running from earliest to latest and validated only that it was not empty, so a reversed or
 *    overlapping list was constructible - and, through Joda-Beans deserialization, arrivable from
 *    a document. [[Schedule.of]] checks it here, reporting one failure for each pair that is out
 *    of order, in both the unadjusted and the adjusted date pair. Gaps remain allowed, exactly as
 *    the bean allowed them; only disorder is refused. Every schedule this library produces
 *    satisfies the check, so the strengthening rejects no value the port can build.
 *  - '''`getPeriodEndDate` answers `None` instead of raising.''' The method threw
 *    `IllegalArgumentException("Date is not contained in any period")` for a date lying in none of
 *    the periods; [[periodEndDate]] returns `Option`, which is the shape the ported
 *    `ScheduleInfo` fixes for it. A date outside the schedule is data, not a broken call, and the
 *    day counts that read it refuse on their own behalf if they cannot proceed without it.
 *  - '''`getStubs` returns a tuple.''' The pair type of the Java collect module has no counterpart
 *    in this port, so [[stubs]] returns `(Option[SchedulePeriod], Option[SchedulePeriod])`.
 *  - '''The schedule exception becomes a failure value.''' `merge` threw `ScheduleException` for a
 *    date that matches nothing in the schedule and for a grouping that does not divide the regular
 *    periods; [[merge]] returns those as `Left(Failure.Invalid)` carrying the '''same message
 *    text''', so the wording callers and tests rely on is preserved exactly.
 *  - '''`mergeRegular` and `toAdjusted` gain error channels.''' Neither declared a checked failure
 *    in Java beyond `IllegalArgumentException`, but in this port the frequency multiplication
 *    ([[Frequency.of]]) and the rebuilding of a period ([[SchedulePeriod.of]]) both report failure
 *    as a value, so both methods return `Either[Failure, Schedule]`. [[mergeToTerm]] and
 *    [[toUnadjusted]] stay total, as described on each.
 *  - '''Contract checks remain refusals, except the group size.''' A pair of regular dates
 *    supplied out of order and a period index outside the schedule are broken calls rather than
 *    data the library should report on, so they raise `IllegalArgumentException` through
 *    `ArgCheck`, exactly as the methods being ported did. A group size of zero or less is the one
 *    condition that moves the other way: Java raised `IllegalArgumentException` for it, and here
 *    it is a `Left(Failure.Invalid)`, because a group size is ordinarily computed from the same
 *    data as the dates it accompanies and belongs in the same error channel as them.
 *  - '''The periods are held twice, once for the type and once for indexed access.''' The bean
 *    carried its periods in an `ImmutableList`, which answers `size`, `last` and an indexed access
 *    at once; [[periods]] is the `cats.data.NonEmptyList` the Agent Action Plan fixes, which
 *    answers all three in time proportional to its length. The type therefore holds an
 *    unpublished `Vector` of the same periods, computed on first use, and the members that need
 *    positional access read it - a representation detail that changes no value, no equality and
 *    no document, described on `Schedule.periodVector`.
 *  - '''A group size too large to multiply the frequency by is reported.''' `Period.multipliedBy`
 *    multiplies each component exactly and raises `ArithmeticException` where the product does not
 *    fit, so in Java a large caller-supplied group size escaped `merge` and `mergeRegular` as an
 *    exception - and escaped even the path that refuses the group size, because the message named
 *    the multiplied frequency and so multiplied a second time. Here the multiplication happens
 *    once per merge, inside a narrowly caught helper, and an overflow is a `Left(Failure.Invalid)`
 *    like every other data-dependent failure of these two methods (AAP 0.3.3).
 *  - '''No Joda bean, builder or Java serialization.''' The meta-bean, the builder, `ImmutableBean`
 *    and `Serializable` are dropped; [[Schedule.of]] replaces the builder and JSON replaces Java
 *    serialization.
 *
 * @param periods  the schedule periods, of which there is always at least one, ordered from
 *   earliest to latest; each period is intended to be adjacent to the next one, however each
 *   period is independent and non-adjacent periods are allowed
 * @param periodicFrequency  the periodic frequency used when building the schedule, which is a
 *   suitable estimate where the schedule was not built from a regular periodic frequency; this is
 *   the `frequency` property of the bean being ported, renamed to leave the name `frequency` to
 *   the `Option`-returning member of the schedule information interface
 * @param rollConvention  the roll convention used when building the schedule, which is 'None'
 *   where the schedule was not built from a regular periodic frequency
 */
sealed abstract case class Schedule private (
    periods: NonEmptyList[SchedulePeriod],
    periodicFrequency: Frequency,
    rollConvention: RollConvention) extends DayCount.ScheduleInfo {

  /**
   * The periods of this schedule as an indexed sequence, which is a representation detail.
   *
   * [[periods]] is a `cats.data.NonEmptyList`, which the Agent Action Plan fixes as the type of
   * that property and which the codecs of this type are derived over. A linked list answers
   * `size`, `last` and an indexed access in time proportional to its length, so the members that
   * ask those questions - [[size]], [[period]], [[lastPeriod]], [[regularPeriods]] and the scan
   * [[merge]] performs - would each walk the schedule, and code that reads every period by index,
   * which is how a caller holding a schedule usually reads one, would cost time proportional to
   * the square of its length where the immutable list being ported answered each question at once.
   *
   * This is that same list held once as a `Vector`, computed on first use and shared by every
   * member that needs indexed or positional access. It is '''not''' a constructor field, so
   * equality, hashing, `unapply` and both codecs are exactly what they were - they read the three
   * declared properties and nothing else - and it is not published, so nothing outside this file
   * can observe the representation. Initialisation is a `lazy val`, which is thread-safe: a
   * schedule shared between threads computes this once and every thread sees the same value.
   */
  private lazy val periodVector: Vector[SchedulePeriod] = periods.toList.toVector

  //-------------------------------------------------------------------------
  /**
   * Gets the number of periods in the schedule.
   *
   * This returns the number of periods, which will be at least one.
   *
   * @return the number of periods
   */
  def size: Int = periodVector.size

  /**
   * Checks if this schedule represents a single 'Term' period.
   *
   * A 'Term' schedule has one period '''and''' a frequency of 'Term'; a single period at any other
   * frequency is not a term schedule, which is what [[isSinglePeriod]] is for.
   *
   * @return true if this is a 'Term' schedule
   */
  def isTerm: Boolean = size == 1 && periodicFrequency == Frequency.TERM

  /**
   * Checks if this schedule has a single period.
   *
   * @return true if this is a single period
   */
  def isSinglePeriod: Boolean = size == 1

  //-------------------------------------------------------------------------
  /**
   * Gets a schedule period by index.
   *
   * This returns a period using a zero-based index.
   *
   * An index outside the schedule is a broken call rather than data to report on - a caller asking
   * for the fourth period of a three-period schedule has miscounted - so it is refused through
   * `ArgCheck`, as the indexed access of the list being ported refused it.
   *
   * @param index  the zero-based period index
   * @return the schedule period
   * @throws IllegalArgumentException if the index is outside the schedule
   */
  def period(index: Int): SchedulePeriod = {
    ArgCheck.inRange(index, 0, size, Schedule.IndexName)
    periodVector(index)
  }

  /**
   * Gets the first schedule period.
   *
   * @return the first schedule period
   */
  def firstPeriod: SchedulePeriod = periods.head

  /**
   * Gets the last schedule period.
   *
   * @return the last schedule period
   */
  def lastPeriod: SchedulePeriod = periodVector.last

  //-------------------------------------------------------------------------
  /**
   * Gets the start date of the schedule.
   *
   * The first date in the schedule, typically treated as inclusive. If the schedule adjusts for
   * business days, then this is the adjusted date, which is what the name says; [[startDate]] is
   * the same date as the `Option` the schedule information interface asks for.
   *
   * @return the schedule start date
   */
  def adjustedStartDate: LocalDate = firstPeriod.startDate

  /**
   * Gets the end date of the schedule.
   *
   * The last date in the schedule, typically treated as exclusive. If the schedule adjusts for
   * business days, then this is the adjusted date.
   *
   * @return the schedule end date
   */
  def adjustedEndDate: LocalDate = lastPeriod.endDate

  /**
   * Gets the unadjusted start date.
   *
   * The start date before any business day adjustment.
   *
   * @return the unadjusted schedule start date
   */
  def unadjustedStartDate: LocalDate = firstPeriod.unadjustedStartDate

  /**
   * Gets the unadjusted end date.
   *
   * The end date before any business day adjustment.
   *
   * @return the unadjusted schedule end date
   */
  def unadjustedEndDate: LocalDate = lastPeriod.unadjustedEndDate

  //-------------------------------------------------------------------------
  /**
   * Gets the initial stub if it exists.
   *
   * There is an initial stub if the first period is a stub and the frequency is not 'Term'.
   *
   * A period is allocated to one and only one of [[initialStub]], [[regularPeriods]] and
   * [[finalStub]].
   *
   * @return the initial stub, empty if there is no initial stub
   */
  def initialStub: Option[SchedulePeriod] = if (hasInitialStub) Some(firstPeriod) else None

  /**
   * Gets the final stub if it exists.
   *
   * There is a final stub if there is more than one period and the last period is a stub.
   *
   * A period is allocated to one and only one of [[initialStub]], [[regularPeriods]] and
   * [[finalStub]].
   *
   * @return the final stub, empty if there is no final stub
   */
  def finalStub: Option[SchedulePeriod] = if (hasFinalStub) Some(lastPeriod) else None

  /**
   * Gets the stubs if they exist.
   *
   * This returns the initial and the final stub as a pair. The flag handles the case where there
   * are no regular periods and it is therefore unclear whether the one period is an initial or a
   * final stub: a schedule of one stub period answers `(None, Some(stub))` when the final stub is
   * preferred and `(Some(stub), None)` when it is not.
   *
   * A period is allocated to one and only one of [[stubs]] and [[regularPeriods]].
   *
   * The pair type of the Java collect module is not part of this port, so this is an ordinary
   * tuple of the initial stub and the final stub, in that order.
   *
   * @param preferFinal  true to prefer the final stub if there is only one period
   * @return the initial stub and the final stub, each empty if there is no such stub
   */
  def stubs(preferFinal: Boolean): (Option[SchedulePeriod], Option[SchedulePeriod]) = {
    val initial = initialStub
    if (preferFinal && size == 1 && initial.isDefined) (None, initial) else (initial, finalStub)
  }

  /**
   * Gets the regular schedule periods.
   *
   * The regular periods exclude any initial or final stub. In most cases the periods returned are
   * regular, corresponding to the periodic frequency and roll convention, however there are cases
   * where this is not true - a term schedule, whose one period is returned as regular, being the
   * obvious one. See [[SchedulePeriod.isRegular]].
   *
   * A period is allocated to one and only one of [[initialStub]], [[regularPeriods]] and
   * [[finalStub]].
   *
   * @return the non-stub schedule periods
   */
  def regularPeriods: List[SchedulePeriod] =
    if (isTerm) {
      periodVector.toList
    } else {
      val startStub = if (hasInitialStub) 1 else 0
      val endStub = if (hasFinalStub) 1 else 0
      if (startStub == 0 && endStub == 0) periodVector.toList
      else periodVector.slice(startStub, size - endStub).toList
    }

  /**
   * Gets the complete list of unadjusted dates.
   *
   * This returns a list including all the unadjusted period boundary dates, which is the
   * unadjusted start date of the schedule followed by the unadjusted end date of each period. It
   * therefore holds one date more than the schedule has periods, and it is non-empty for the same
   * reason the schedule is.
   *
   * @return the list of unadjusted dates, in order
   */
  def unadjustedDates: NonEmptyList[LocalDate] =
    NonEmptyList(unadjustedStartDate, periods.toList.map(_.unadjustedEndDate))

  //-------------------------------------------------------------------------
  /**
   * Gets the start date of the schedule, as the schedule information interface asks for it.
   *
   * A schedule always knows its start date, so this is always `Some`; the date itself is
   * [[adjustedStartDate]].
   *
   * @return the schedule start date
   */
  override def startDate: Option[LocalDate] = Some(adjustedStartDate)

  /**
   * Gets the end date of the schedule, as the schedule information interface asks for it.
   *
   * A schedule always knows its end date, so this is always `Some`; the date itself is
   * [[adjustedEndDate]].
   *
   * @return the schedule end date
   */
  override def endDate: Option[LocalDate] = Some(adjustedEndDate)

  /**
   * Gets the periodic frequency of the schedule, as the schedule information interface asks for it.
   *
   * A schedule always knows its frequency, so this is always `Some`; the frequency itself is
   * [[periodicFrequency]].
   *
   * @return the periodic frequency
   */
  override def frequency: Option[Frequency] = Some(periodicFrequency)

  /**
   * Checks if the end of month convention is in use.
   *
   * If true then when building a schedule, dates are at the end of the month if the first date in
   * the series is at the end of the month. This holds exactly when the roll convention is 'EOM',
   * which is the test the method being ported made.
   *
   * @return true if the end of month convention is in use
   */
  override def isEndOfMonthConvention: Boolean = rollConvention == RollConventions.EOM

  /**
   * Finds the period end date given a date in the period.
   *
   * The first matching period is used. The adjusted start and end dates of each period are
   * compared, with the start date included and the end date excluded, so a date equal to a
   * period's start date lies in that period and a date equal to its end date lies in the next one.
   *
   * A date lying in none of the periods answers `None`, where the method being ported raised
   * `IllegalArgumentException`. That is the shape the ported schedule information interface fixes,
   * and the reasoning is recorded there: a date outside the schedule describes the data rather
   * than a broken call.
   *
   * @param date  the date to find
   * @return the end date of the period that includes the date, empty if no period includes it
   */
  override def periodEndDate(date: LocalDate): Option[LocalDate] =
    periods.find(_.contains(date)).map(_.endDate)

  //-------------------------------------------------------------------------
  /**
   * Merges this schedule to form a new schedule with a single 'Term' period.
   *
   * The result has one period of type 'Term', with dates matching this schedule: the start dates
   * of the first period and the end dates of the last, in both their adjusted and their unadjusted
   * form.
   *
   * This is '''total'''. The period it builds spans the whole schedule, and for any schedule whose
   * periods run from earliest to latest - which is what [[periods]] documents and what every
   * schedule this library produces is - that span is a pair of dates in order and distinct, so
   * [[SchedulePeriod.of]] accepts it. The only way to reach the rejection is to assemble a
   * schedule whose periods are not in order, and the answer then is '''this schedule unchanged''':
   * there is no single period spanning it, so nothing can be merged, and returning the schedule
   * itself invents no data and discards none. That is the same answer this method already gives a
   * schedule that is a term period, and it is reached without raising, without a placeholder value
   * and without taking the value out of a failure.
   *
   * @return the merged 'Term' schedule, or this schedule where its periods span no single period
   */
  def mergeToTerm: Schedule =
    if (isTerm) {
      this
    } else {
      val first = firstPeriod
      val last = lastPeriod
      SchedulePeriod
        .of(first.startDate, last.endDate, first.unadjustedStartDate, last.unadjustedEndDate)
        .fold(_ => this, period => Schedule.ofTerm(period))
    }

  /**
   * Merges this schedule to form a new schedule by combining the schedule periods.
   *
   * This produces a schedule where some periods are merged together, which is how a three monthly
   * schedule becomes a six monthly one. The merging is controlled by the group size, the number of
   * periods to merge together in the result, so converting three monthly to six monthly is a group
   * size of two.
   *
   * A group size of one returns this schedule, as does a schedule of a single period. A larger
   * group size merges each group of regular periods.
   *
   * The two dates must each be one of the dates of this schedule, either unadjusted or adjusted.
   * All periods before the first regular start date form a single period in the result, and so do
   * all periods after the last regular end date, so an initial or final stub may be merged with a
   * regular period as part of the process. Where two periods of this schedule carry the same date,
   * the '''last''' of them is taken, which is what the scan being ported did by not stopping at
   * the first match.
   *
   * For example, a schedule with an initial stub and five regular periods can be grouped by two if
   * `firstRegularStartDate` equals the end of the first regular period.
   *
   * Four things are reported as failures rather than raised, the middle three carrying the message
   * text of the schedule exception being ported unchanged: a group size of zero or less, a first
   * regular start date matching no date in the schedule, a last regular end date matching no date
   * in the schedule, and a number of regular periods that the group size does not divide. The
   * order of the two dates is caller contract and is refused.
   *
   * @param groupSize  the group size
   * @param firstRegularStartDate  the unadjusted start date of the first regular payment period
   * @param lastRegularEndDate  the unadjusted end date of the last regular payment period
   * @return the merged schedule, or the failure describing why the dates and the group size
   *   describe no schedule
   * @throws IllegalArgumentException if the two dates are out of order
   */
  def merge(
      groupSize: Int,
      firstRegularStartDate: LocalDate,
      lastRegularEndDate: LocalDate): Either[Failure, Schedule] = withGroupSize(groupSize) {
    ArgCheck.inOrderOrEqual(
      firstRegularStartDate,
      lastRegularEndDate,
      Schedule.FirstRegularStartDateName,
      Schedule.LastRegularEndDateName)(Schedule.DateOrder)
    if (isSinglePeriod || groupSize == 1) {
      Right(this)
    } else {
      val all = periodVector
      // the last match is kept for each date, as in the loop being ported, which scanned the whole
      // list and overwrote the index it had found
      val startRegularIndex = all.lastIndexWhere(period =>
        period.unadjustedStartDate == firstRegularStartDate ||
          period.startDate == firstRegularStartDate)
      val lastRegularIndex = all.lastIndexWhere(period =>
        period.unadjustedEndDate == lastRegularEndDate || period.endDate == lastRegularEndDate)
      if (startRegularIndex < 0) {
        Left(Failure.Invalid(
          unmatchedDateMessage(Schedule.FirstRegularStartDateName, firstRegularStartDate)))
      } else if (lastRegularIndex < 0) {
        Left(Failure.Invalid(
          unmatchedDateMessage(Schedule.LastRegularEndDateName, lastRegularEndDate)))
      } else {
        // the frequency is multiplied here, after the two dates have been matched and before
        // either the grouping message or the merged frequency needs it, so that a date matching
        // nothing is still reported as the implementation being ported reported it - that path
        // never multiplied - and the multiplication that both remaining outcomes need happens
        // exactly once
        multipliedFrequencyPeriod(groupSize).flatMap { mergedPeriod =>
          val endRegularIndex = lastRegularIndex + 1
          if ((endRegularIndex - startRegularIndex) % groupSize != 0) {
            Left(Failure.Invalid(
              groupingMessage(mergedPeriod, firstRegularStartDate, lastRegularEndDate)))
          } else {
            // everything before the first regular date is one group, everything after the last
            // regular date is another, and the regular periods in between are grouped in threes,
            // fours or whatever the group size says; the slice is empty where the two indices
            // cross, which is the outcome the ported loop had for the same input
            val leading =
              if (startRegularIndex > 0) List(all.slice(0, startRegularIndex)) else Nil
            val regular = all.slice(startRegularIndex, endRegularIndex).grouped(groupSize).toList
            val trailing =
              if (endRegularIndex < all.size) List(all.slice(endRegularIndex, all.size)) else Nil
            regrouped(leading ::: regular ::: trailing, mergedPeriod)
          }
        }
      }
    }
  }

  /**
   * Merges this schedule to form a new schedule by combining the regular schedule periods.
   *
   * This produces a schedule where some periods are merged together, which is how a three monthly
   * schedule becomes a six monthly one. The group size is the number of periods to merge together,
   * and the roll flag is the direction in which grouping occurs.
   *
   * Any existing stub periods are special and are '''not''' merged. Even where the grouping leaves
   * an excess period - ten regular periods with a group size of three - the excess period is not
   * merged with a stub. Rolling forwards leaves the excess period last (three, three, three, one);
   * rolling backwards leaves it first (one, three, three, three), which is what the negative
   * starting index of the loop being ported achieves.
   *
   * A group size of one returns this schedule, as does a schedule of a single period, so a term
   * schedule is returned unchanged.
   *
   * A group size of zero or less is reported as a failure rather than raised, as it is by
   * [[merge]].
   *
   * @param groupSize  the group size
   * @param rollForwards  whether to roll forwards (true) or backwards (false)
   * @return the merged schedule, or the failure describing why the group size describes no
   *   schedule
   */
  def mergeRegular(groupSize: Int, rollForwards: Boolean): Either[Failure, Schedule] =
    withGroupSize(groupSize) {
      if (isSinglePeriod || groupSize == 1) {
        Right(this)
      } else {
        multipliedFrequencyPeriod(groupSize).flatMap { mergedPeriod =>
          val regular = regularPeriods.toVector
          val regularSize = regular.size
          val remainder = regularSize % groupSize
          // a negative start index is what puts the excess group first when rolling backwards; the
          // bounds of each group are then clamped, exactly as the ported loop clamped them
          val startIndex = if (rollForwards || remainder == 0) 0 else -(groupSize - remainder)
          val regularGroups = Range(startIndex, regularSize, groupSize).toList.map { index =>
            regular.slice(math.max(index, 0), math.min(index + groupSize, regularSize))
          }
          val leading = initialStub.toList.map(stub => Vector(stub))
          val trailing = finalStub.toList.map(stub => Vector(stub))
          regrouped(leading ::: regularGroups ::: trailing, mergedPeriod)
        }
      }
    }

  /**
   * Runs a merge, or reports a group size that describes no merge.
   *
   * The two merges share this guard. A group size of zero or less is the one thing they both
   * refuse before looking at the schedule at all, and it is refused as a '''value''': the Agent
   * Action Plan places the group-size checks of both methods in the failable surface, so a caller
   * that computed a group size from data is told what is wrong with it through the same channel as
   * the dates it supplied alongside it, rather than through an exception.
   *
   * The message is the text the argument checker of this port produces for the same condition,
   * prefixed with what could not be done, so nothing the raised form told a caller is lost.
   *
   * @param groupSize  the group size to check
   * @param merge  the merge to run where the group size is usable, evaluated at most once
   * @return the merged schedule, or the failure describing the group size
   */
  private def withGroupSize(groupSize: Int)(
      merge: => Either[Failure, Schedule]): Either[Failure, Schedule] =
    if (groupSize > 0) {
      merge
    } else {
      Left(
        Failure.Invalid(
          s"Unable to merge schedule, '${Schedule.GroupSizeName}' must not be negative or zero " +
            s"but has value $groupSize"))
    }

  //-------------------------------------------------------------------------
  /**
   * Converts this schedule to one where the start and end dates are adjusted using the specified
   * adjuster.
   *
   * The result has the same number of periods, with each start date and end date replaced by the
   * adjusted date the adjuster returns. The unadjusted start date and unadjusted end date of each
   * period are unchanged.
   *
   * A special rule applies to the first start date and the last end date, and it is the reason
   * each period is told where it sits: if the first period once adjusted is empty then its
   * unadjusted start date is used instead of the adjusted one, and if the last period once
   * adjusted is empty then its unadjusted end date is used instead. This rule avoids some
   * unnecessary failures. Every period in between that adjustment collapses onto a single day is
   * reported as a failure, which is the definition the caller has to correct, and the first such
   * period ends the traversal - the same point at which the implementation being ported threw.
   *
   * '''This schedule itself''' is returned where the adjuster moved no date, which downstream code
   * relies on to avoid recalculating against an identical schedule.
   *
   * The adjuster is an arbitrary function of a date supplied by the caller, so it is '''not''' in
   * this method's gift to know that the adjusted periods still run from earliest to latest: an
   * adjuster that moves one boundary across another produces a period list that no longer is a
   * time line. The adjusted list is therefore rebuilt through [[Schedule.of]] rather than stored
   * unchecked, and a list the adjuster reordered is reported as the same kind of failure a
   * reordered list is reported as anywhere else, collapsed into the single cause this method
   * answers with. A monotonic adjuster - which every business day convention of this library is -
   * cannot reach that failure.
   *
   * @param adjuster  the adjuster to use
   * @return the adjusted schedule, this schedule where nothing moved, or the failure describing
   *   why a period is invalid once adjusted or why the adjusted periods are out of order
   */
  def toAdjusted(adjuster: DateAdjuster): Either[Failure, Schedule] = {
    val lastIndex = size - 1
    periods.zipWithIndex
      .traverse { case (period, index) =>
        val mergeType = if (index == 0) -1 else if (index == lastIndex) 1 else 0
        period.toAdjusted(adjuster, mergeType).left.map(Failure.collapse)
      }
      .flatMap { adjustedPeriods =>
        // each period hands back itself where its dates did not move, so reference inequality is
        // the test for "something changed", as it was in the implementation being ported
        val moved = adjustedPeriods.zipWith(periods)((adjusted, original) => adjusted ne original)
        if (moved.exists(identity)) {
          Schedule
            .of(adjustedPeriods, periodicFrequency, rollConvention)
            .left.map(Failure.collapse)
        } else {
          Right(this)
        }
      }
  }

  /**
   * Converts this schedule to one where every adjusted date is reset to the unadjusted equivalent.
   *
   * The result has the same number of periods, with each start date and end date replaced by the
   * matching unadjusted start or end date.
   *
   * This is '''total''': the dates it moves into the adjusted positions are the unadjusted dates
   * of periods that exist, which were checked to be in order and distinct when those periods were
   * built, so [[SchedulePeriod.toUnadjusted]] needs no error channel and neither does this.
   *
   * @return the equivalent unadjusted schedule
   */
  def toUnadjusted: Schedule =
    Schedule.create(periods.map(_.toUnadjusted), periodicFrequency, rollConvention)

  //-------------------------------------------------------------------------
  /**
   * Builds the schedule that a grouping of the periods of this schedule describes.
   *
   * Shared by [[merge]] and [[mergeRegular]], which differ in how they group the periods and not
   * in what they do with the groups: each group becomes one period, the frequency is multiplied by
   * the group size, and the roll convention is carried over.
   *
   * A group that is one period is that period, so a schedule where nothing was grouped keeps the
   * very periods it had. Any other group is collapsed into the period spanning it, and the
   * frequency is rebuilt from the multiplied period through [[Frequency.of]], which is where a
   * multiplication beyond the frequencies this library expresses is reported - the implementation
   * being ported raised there instead. The multiplication itself is performed once per merge, by
   * [[multipliedFrequencyPeriod]], and its result is handed to this method: multiplying here as
   * well would repeat work that can fail.
   *
   * @param chunks  the groups of periods to collapse, in order, each of which is non-empty
   * @param mergedPeriod  the periodic frequency's period multiplied by the group size
   * @return the regrouped schedule, or the first failure a group reported
   */
  private def regrouped(
      chunks: List[Vector[SchedulePeriod]],
      mergedPeriod: Period): Either[Failure, Schedule] = {
    // every group a caller of this method builds holds at least one period - a slice is taken only
    // where its bounds enclose something and `grouped` never yields an empty group - so nothing is
    // dropped here; the conversion is the way to say "non-empty" in the type without a partial
    // function that could raise
    val groups = chunks.flatMap(chunk => NonEmptyList.fromList(chunk.toList))
    groups.traverse(createSchedulePeriod).flatMap {
      case head :: tail =>
        Frequency
          .of(mergedPeriod)
          .left.map(Failure.collapse)
          .map(frequency => Schedule.create(NonEmptyList(head, tail), frequency, rollConvention))
      case Nil =>
        // unreachable: both callers contribute at least one group for a schedule of at least one
        // period, and this schedule has at least one period; a grouping of nothing leaves nothing
        // to merge, for which this schedule unchanged is the answer that invents no data
        Right(this)
    }
  }

  /**
   * Collapses a group of adjacent periods into the single period spanning it.
   *
   * A group of one period is that period itself, returned as it stands so that a grouping which
   * changes nothing produces the periods it started from. Any other group spans from the start
   * dates of its first period to the end dates of its last, in both their adjusted and their
   * unadjusted form, and that pair of dates is decided by [[SchedulePeriod.of]] - a group whose
   * periods are not in order describes no period, and that is reported rather than raised. The
   * chain of reasons it reports is collapsed into one failure, because a merge answers with a
   * single cause.
   *
   * @param accruals  the adjacent periods to collapse, of which there is at least one
   * @return the period spanning the group, or the failure describing why the group spans none
   */
  private def createSchedulePeriod(
      accruals: NonEmptyList[SchedulePeriod]): Either[Failure, SchedulePeriod] = {
    val first = accruals.head
    if (accruals.tail.isEmpty) {
      Right(first)
    } else {
      val last = accruals.last
      SchedulePeriod
        .of(first.startDate, last.endDate, first.unadjustedStartDate, last.unadjustedEndDate)
        .left.map(Failure.collapse)
    }
  }

  /**
   * The message reporting a regular date that matches no date of this schedule.
   *
   * The text is the text of the schedule exception being ported, with the dates of the schedule
   * rendered as the square-bracketed list the Java message formatter produced from the list it was
   * handed, so a caller matching on this message sees what it saw.
   *
   * @param name  the name of the date argument that matched nothing
   * @param date  the date that matched nothing
   * @return the message
   */
  private def unmatchedDateMessage(name: String, date: LocalDate): String =
    s"Unable to merge schedule, $name $date does not match any date in the underlying schedule " +
      unadjustedDates.toList.mkString("[", ", ", "]")

  /**
   * Multiplies the period of this schedule's frequency by a group size, reporting an overflow.
   *
   * `java.time.Period` multiplies each of its three components exactly, raising
   * `ArithmeticException` where the product does not fit, and the group size is supplied by the
   * caller - so this is a failure that depends on the data a merge was asked to perform and
   * belongs in the error channel the merges answer with rather than in an exception. Only that one
   * exception is caught: anything else is a defect rather than a property of the group size.
   *
   * The result is computed '''once per merge''', before the grouping is worked out, and passed to
   * the two members that need it - [[regrouped]], which builds the merged frequency from it, and
   * [[groupingMessage]], which names it in the failure a group size that does not divide reports.
   * Multiplying separately in each of them would repeat a computation that can fail, which is how
   * the refusal path itself came to raise.
   *
   * Note where the two merges reach this. Both reach it only after their early returns - a
   * single-period schedule and a group size of one answer with this schedule unchanged, as the
   * implementation being ported did, and never multiply at all - and [[merge]] reaches it only
   * after matching its two dates, so a date that matches nothing in the schedule is reported with
   * the ported message rather than displaced by an overflow the ported code would not have
   * reached either.
   *
   * @param groupSize  the group size to multiply by, which is greater than one here
   * @return the multiplied period, or the failure describing why the group size cannot be applied
   */
  private def multipliedFrequencyPeriod(groupSize: Int): Either[Failure, Period] =
    try {
      Right(periodicFrequency.period.multipliedBy(groupSize))
    } catch {
      case _: ArithmeticException =>
        Left(
          Failure.Invalid(
            s"Unable to merge schedule, '${Schedule.GroupSizeName}' of $groupSize is too large " +
              s"to multiply the frequency '${periodicFrequency.name}' by"))
    }

  /**
   * The message reporting a group size that does not divide the regular periods.
   *
   * The frequency named in the text is the '''period''' the multiplication produced, printed by
   * its own ISO-8601 form as `P6M` rather than by the frequency it would become, which is what the
   * message being ported printed. The period is the one [[multipliedFrequencyPeriod]] computed for
   * this merge, passed in rather than recomputed, so building this message cannot fail.
   *
   * @param mergedPeriod  the periodic frequency's period multiplied by the group size
   * @param firstRegularStartDate  the start date of the first regular period
   * @param lastRegularEndDate  the end date of the last regular period
   * @return the message
   */
  private def groupingMessage(
      mergedPeriod: Period,
      firstRegularStartDate: LocalDate,
      lastRegularEndDate: LocalDate): String =
    s"Unable to merge schedule, firstRegularStartDate $firstRegularStartDate and " +
      s"lastRegularEndDate $lastRegularEndDate cannot be used to create regular periods of " +
      s"frequency '$mergedPeriod'"

  /**
   * Renders this schedule as text.
   *
   * The frequency, the roll convention and then each period, so that everything a schedule holds
   * is in the text and a schedule can be read in a failure message:
   *
   * {{{
   * Schedule(P1M, Day/17, [2014-07-17 to 2014-08-16 (unadjusted 2014-07-17 to 2014-08-17)])
   * }}}
   *
   * This is the port's own form. The bean being ported rendered the property-by-property text of a
   * Joda bean, which has no counterpart here; the JSON encoding is where the three properties are
   * written out under their own names.
   *
   * @return the text form of this schedule
   */
  override def toString: String =
    periods.toList.mkString(
      s"Schedule($periodicFrequency, $rollConvention, [",
      ", ",
      "])")

  //-------------------------------------------------------------------------
  /** Checks if there is an initial stub, which is the private test of the class being ported. */
  private def hasInitialStub: Boolean =
    !isTerm && !firstPeriod.isRegular(periodicFrequency, rollConvention)

  /** Checks if there is a final stub, which is the private test of the class being ported. */
  private def hasFinalStub: Boolean =
    !isSinglePeriod && !lastPeriod.isRegular(periodicFrequency, rollConvention)
}

/**
 * Companion of [[Schedule]], holding its factories, its typeclass instances and its codecs.
 *
 * The two factories are the only way to obtain a schedule from outside this file, which is what
 * leaves the type without a public `apply` or `copy`, and [[Schedule.of]] is the single funnel the
 * decoder and the merging algorithms of the type build through.
 *
 * Two typeclass instances are published, and exactly two: a `Hash`, which is the single
 * equality-bearing instance of the type, and a `Show`. There is deliberately '''no''' `Order`: the
 * bean being ported was not comparable, and no ordering of schedules is meaningful - the periods,
 * the frequency and the roll convention give no key that ranks two schedules.
 */
object Schedule {

  /** The name the period index is reported under when it lies outside the schedule. */
  private val IndexName: String = "index"

  /** The name the group size is reported under when it is zero or less. */
  private val GroupSizeName: String = "groupSize"

  /** The name the first regular start date is reported under. */
  private val FirstRegularStartDateName: String = "firstRegularStartDate"

  /** The name the last regular end date is reported under. */
  private val LastRegularEndDateName: String = "lastRegularEndDate"

  /**
   * The ordering of dates the order check of [[Schedule.merge]] is performed with.
   *
   * The checking helpers of this port are generic in the type being compared and take its cats
   * ordering, and cats publishes no instance for `java.time.LocalDate` - the class implements
   * `Comparable[ChronoLocalDate]` rather than `Comparable[LocalDate]`, so the ordering derived from
   * a comparable type does not apply to it either. The instance is therefore stated here, as the
   * natural time-line order the class itself defines, and it is '''passed explicitly''' at the one
   * place that needs it rather than being made implicit: an ordering for a type this module does
   * not own has no business in implicit scope, and an argument written out is also an argument that
   * cannot be reported as unused.
   */
  private val DateOrder: Order[LocalDate] = Order.from((first, second) => first.compareTo(second))

  //-------------------------------------------------------------------------
  /**
   * Obtains a 'Term' instance based on a single period.
   *
   * A 'Term' schedule has one period, a frequency of 'Term' and a roll convention of 'None', so
   * the end-of-month convention is not in use for it.
   *
   * This is '''total''': one period is a schedule, and the frequency and roll convention are
   * constants of this library.
   *
   * @param period  the single period
   * @return the merged 'Term' schedule
   */
  def ofTerm(period: SchedulePeriod): Schedule =
    create(NonEmptyList.one(period), Frequency.TERM, RollConventions.NONE)

  /**
   * Obtains an instance from the periods, the frequency and the roll convention.
   *
   * This is the funnel every schedule is built through, and it is where the '''chronology''' of
   * the period list is decided. Two invariants are stated on [[Schedule.periods]], and this
   * factory is what makes both of them true of every schedule in existence:
   *
   *   1. there is at least one period, which is carried by the type of the field - a
   *      `cats.data.NonEmptyList` - rather than by a check here, so the state the bean being
   *      ported rejected at run time is not expressible;
   *   1. the periods '''run from earliest to latest''', which is checked: each period's end is on
   *      or before the next period's start, in both the unadjusted and the adjusted date pair.
   *
   * What the second check does '''not''' require is adjacency. A gap between one period and the
   * next is explicitly allowed, as it was by the bean being ported, because a schedule may
   * describe accrual that pauses; what is refused is a list that runs backwards or in which two
   * periods overlap, because such a list contradicts the field it is stored in and every member
   * that reads the periods in order. That matters beyond tidiness: the schedule is the
   * [[com.opengamma.strata.basics.date.DayCount.ScheduleInfo]] a day count accrues against,
   * [[Schedule.periodEndDate]] answers with the first period containing a date, stub
   * classification reads the first and last period, the two merges collapse runs of adjacent
   * periods, and [[com.opengamma.strata.basics.value.ValueSchedule]] resolves a step by finding
   * the period whose boundary it names. Every one of those reads the list as a time line, so a
   * list that is not one produces answers that are wrong rather than answers that fail - which is
   * why the refusal belongs here, at the single point of construction, and why the decoder builds
   * through this factory (a document is exactly the route by which a reversed list would otherwise
   * arrive).
   *
   * One failure is reported for each ordering that does not hold, so a list with several
   * misplaced periods reports each of them rather than only the first, in the accumulating channel
   * every validated factory of this port reports through. The other failures a schedule can
   * report belong to the algorithms that derive one schedule from another, which use their own
   * checks and [[SchedulePeriod.of]].
   *
   * @param periods  the schedule periods, of which there is at least one, running from earliest
   *   to latest
   * @param frequency  the periodic frequency used when building the schedule
   * @param rollConvention  the roll convention used when building the schedule
   * @return the schedule, or the failures describing the periods that are out of order
   */
  def of(
      periods: NonEmptyList[SchedulePeriod],
      frequency: Frequency,
      rollConvention: RollConvention): EitherNec[Failure, Schedule] =
    checkedChronology(periods)
      .map(_ => create(periods, frequency, rollConvention))
      .toEither

  /**
   * Checks that a list of periods runs from earliest to latest.
   *
   * Each period is compared with the one after it, and both date pairs are compared: the
   * unadjusted pair, which is the time line the schedule was generated on, and the adjusted pair,
   * which is the time line its dates fall on once the business day convention has had its say. A
   * period whose end - in either pair - falls after the start of the period following it is
   * reported, and the two pairs are reported separately, because they are two different statements
   * about the same list and a caller correcting one is helped by knowing whether the other is
   * wrong too.
   *
   * Equal dates pass: that is the adjacency of a schedule generated from a periodic frequency,
   * where each period begins on the day the one before it ended. Ordering '''within''' a period is
   * not re-checked, since [[SchedulePeriod.of]] decided it when the period was built and no route
   * to a period bypasses it.
   *
   * @param periods  the periods to check, in the order they are to be held
   * @return a passing outcome, or one failure for each ordering that does not hold
   */
  private def checkedChronology(
      periods: NonEmptyList[SchedulePeriod]): ValidatedFailures[Unit] =
    periods.toList
      .zip(periods.tail)
      .zipWithIndex
      .flatMap { case ((earlier, later), index) =>
        List(
          Validate.isFalse(
            earlier.unadjustedEndDate.isAfter(later.unadjustedStartDate),
            outOfOrderMessage(
              index,
              "unadjusted",
              earlier.unadjustedEndDate,
              later.unadjustedStartDate)),
          Validate.isFalse(
            earlier.endDate.isAfter(later.startDate),
            outOfOrderMessage(index, "adjusted", earlier.endDate, later.startDate)))
      }
      .sequence_

  /**
   * The message reporting a pair of periods that does not run from earliest to latest.
   *
   * The text names the two periods by their position in the list, counting from zero as the list
   * is indexed, which pair of dates was compared, and the two dates themselves, so a caller can
   * see what has to move without reading the schedule back out of the failure.
   *
   * @param index  the index of the earlier period of the pair
   * @param dates  which date pair was compared, `unadjusted` or `adjusted`
   * @param end  the end date of the earlier period
   * @param start  the start date of the later period
   * @return the message
   */
  private def outOfOrderMessage(
      index: Int,
      dates: String,
      end: LocalDate,
      start: LocalDate): String =
    s"Unable to create Schedule, the periods must run from earliest to latest but the $dates " +
      s"end date $end of the period at index $index is after the $dates start date $start of the " +
      s"period at index ${index + 1}"

  /**
   * Creates a value, which every route into the type funnels through.
   *
   * This is the only instantiation of the type. The constructor of a `sealed abstract case class`
   * is reachable only from inside the file that declares it, and `new Schedule(...) {}` - an
   * anonymous subclass of the abstract case class - is how it is reached; that is what leaves the
   * type without a public `apply` or `copy` while keeping the `equals`, `hashCode` and `unapply` a
   * case class provides.
   *
   * The method performs '''no check''', so every caller of it owes the chronology
   * [[Schedule.of]] checks, and each of the three in this file discharges that debt by
   * construction:
   *
   *   - [[Schedule.ofTerm]] builds a schedule of one period, and a single period is in order
   *     whatever its dates, there being no pair to compare;
   *   - [[Schedule.toUnadjusted]] moves each period's unadjusted dates into its adjusted
   *     positions, so both pairs of the result are the unadjusted pair of an ordered list and the
   *     result is ordered in both;
   *   - [[Schedule.regrouped]] collapses '''contiguous slices''' of an ordered list into the
   *     periods spanning them, and the spans of contiguous slices of an ordered list are
   *     themselves ordered - each span ends where its last period ended, on or before the start of
   *     the next span's first period.
   *
   * [[Schedule.toAdjusted]], whose adjuster is supplied by the caller and can reorder anything,
   * and the schedule generation of [[PeriodicSchedule]], whose periods come from data, both build
   * through [[Schedule.of]] instead. The method stays visible '''within the schedule package'''
   * for those three proven callers and for that reason only.
   *
   * @param periods  the schedule periods, of which there is at least one
   * @param frequency  the periodic frequency used when building the schedule
   * @param rollConvention  the roll convention used when building the schedule
   * @return the schedule
   */
  private[schedule] def create(
      periods: NonEmptyList[SchedulePeriod],
      frequency: Frequency,
      rollConvention: RollConvention): Schedule =
    new Schedule(periods, frequency, rollConvention) {}

  //-------------------------------------------------------------------------
  /**
   * The hashing and equality of schedules.
   *
   * Equality and hashing are those of the case class, which compare the periods, the frequency and
   * the roll convention, and they are the equality of the bean being ported, which compared the
   * same three properties. No field holds a `Double`, so there is no bit-pattern comparison to
   * arrange; each field has an equality of its own that this one is built from.
   *
   * @return the hashing and equality of schedules
   */
  implicit val hash: Hash[Schedule] = Hash.fromUniversalHashCode[Schedule]

  /**
   * The rendering of schedules as text.
   *
   * Renders what [[Schedule.toString]] renders, so the two ways of putting a schedule into a
   * message agree.
   *
   * @return the rendering of a schedule
   */
  implicit val show: Show[Schedule] = Show.show(_.toString)

  //-------------------------------------------------------------------------
  /**
   * The field shape both codecs are derived from, which the decoder reads before validation.
   *
   * Decoding a validated type is two steps: read the fields, then hand them to the factory that
   * decides whether they describe a value. This product is the first step, and encoding is the same
   * two steps in reverse, so both codecs below derive from this one declaration and the JSON shape
   * of a schedule is stated exactly once. It is private and never returned - the only values of it
   * that exist are the ones the two codecs build.
   *
   * Its field names are the JSON keys, and they are the '''names of the three properties of the
   * bean being ported''': `periods`, `frequency` and `rollConvention`. The middle one is where this
   * shape and the type differ, deliberately - the type calls that field
   * [[Schedule.periodicFrequency]], for the reason recorded on the type, while the document keeps
   * the property name a reader of the Java library knows.
   *
   * The periods are carried as a non-empty array, which the JSON instances cats publishes with
   * circe supply, so a document whose `periods` array is empty is a decoding failure without a
   * check of its own - the same invariant the type carries, enforced at the edge by the same type.
   *
   * @param periods  the schedule periods, each carried as the object its own codec writes
   * @param frequency  the periodic frequency, carried as its name
   * @param rollConvention  the roll convention, carried as its name
   */
  private final case class Raw(
      periods: NonEmptyList[SchedulePeriod],
      frequency: Frequency,
      rollConvention: RollConvention)

  /** The derived decoder of the raw field shape, used by the validating decoder below. */
  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /** The derived encoder of the raw field shape, used by the encoder below. */
  private val rawEncoder: Encoder[Raw] = deriveEncoder[Raw]

  /**
   * The JSON encoding of schedules.
   *
   * A value is an object of three fields, the periods as an array of the objects their own codec
   * writes and the frequency and roll convention as their names:
   *
   * {{{
   * {"periods":[{"startDate":"2014-07-17",
   *              "endDate":"2014-08-16",
   *              "unadjustedStartDate":"2014-07-17",
   *              "unadjustedEndDate":"2014-08-17"}],
   *  "frequency":"P1M",
   *  "rollConvention":"Day/17"}
   * }}}
   *
   * Both halves of the codec are assembled by the same compile-time derivation over the same raw
   * shape, which is what keeps them from drifting apart, and no part of the encoding inspects a
   * class while the program runs. Deriving either codec from [[Schedule]] directly is not possible:
   * the derivation reads the public constructor of a product, and a validated type has none.
   *
   * Two equal values encode to identical bytes: the three fields are written in their declaration
   * order, the periods in the order the schedule holds them, and each field has one form. The
   * result is wrapped so that a field holding no value would be omitted, which is the policy every
   * product of this port follows - all three fields of this type are required, so the wrapping
   * changes nothing about its output and exists so that the policy holds without exception.
   *
   * @return the JSON encoding of schedules
   */
  implicit val encoder: Encoder[Schedule] =
    Codecs.dropNulls(rawEncoder.contramap[Schedule] { value =>
      Raw(value.periods, value.periodicFrequency, value.rollConvention)
    })

  /**
   * The JSON decoding of schedules.
   *
   * This is the inverse of the encoding above, and it builds through the factory a caller builds
   * through: the payload is read into the raw shape and handed to [[Schedule.of]]. All three fields
   * have to be present, the `periods` array has to hold at least one period, and every period,
   * frequency and roll convention in the document is decided by the codec of its own type - so a
   * period whose dates are out of order or a frequency that is not one this library expresses is a
   * decoding failure carrying the reason it is.
   *
   * @return the JSON decoding of schedules
   */
  implicit val decoder: Decoder[Schedule] =
    Codecs.validatedDecoder[Raw, Schedule] { raw =>
      of(raw.periods, raw.frequency, raw.rollConvention)
    }(rawDecoder)
}
