/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.value

import java.time.LocalDate

import scala.annotation.tailrec

import cats.Hash
import cats.Show
import cats.data.NonEmptyList
import cats.syntax.apply._

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.basics.schedule.SchedulePeriod
import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.ValidatedFailures
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * A single step in the variation of a value over time.
 *
 * A financial value, such as the notional or interest rate, may vary over time. This class
 * represents a single change in the value within [[ValueSchedule]].
 *
 * The date of the change is either specified explicitly, or in relative terms via an index. The
 * adjustment to the value can also be specified absolutely, or in relative terms.
 *
 * ===The two ways of positioning a step===
 *
 * Exactly one of the two positions is held by any value of this type, which is the single
 * invariant of the type and the reason its construction is checked:
 *
 *   - a ''period index'' positions the step in relative terms, and is resolved against whatever
 *     schedule the step is later applied to;
 *   - a ''date'' positions it in absolute terms, and has to line up with a boundary of that
 *     schedule.
 *
 * Both positions are optional properties, so both are `Option`s here; the Java bean being ported
 * expressed the same pair as two fields that could each be absent, read back through
 * `OptionalInt` and `Optional`, which no signature of this port names.
 *
 * ===Construction===
 *
 * Construction is validating, so the primary constructor is private and there is no `apply` or
 * `copy`: the three factories of the companion are the only way to obtain a step, and every value
 * that exists therefore holds exactly one position with a period index, where it has one, of one
 * or greater. Two of the three report what is wrong with their input rather than raising it, and
 * the third - the one taking a date - is total, because there is nothing about a date this type
 * can reject.
 *
 * ===Equality===
 *
 * Equality and hashing are those of the three properties, and so are those of
 * [[ValueAdjustment]] where the modifying value is concerned: an adjustment compares its double
 * by bit pattern rather than by numeric comparison, which is what the bean equality of the Java
 * original did and what every double-bearing type of this port does. A step carries no double of
 * its own, so it inherits that behaviour whole rather than restating it - including the two
 * consequences the round-trip properties of the test suite rely on, that a value always equals
 * itself even when its adjustment carries a value that is not a number, and that a negative zero
 * is distinct from a positive zero.
 *
 * ===Thread safety===
 *
 * An instance is immutable and holds only immutable values, so it is safe to share between any
 * number of threads without synchronisation.
 *
 * @param periodIndex  the index of the schedule period boundary at which the change occurs, used
 *   to define the date of the step in relative terms. The date is identified by the '''zero-based
 *   index''' of the schedule period boundary, and the change occurs at the '''start''' of the
 *   specified period; thus an index of zero is the start of the first period or initial stub. The
 *   index must be one or greater, as a change is not permitted at the start of the first period.
 *   For example, consider a 5 year swap from 2012-02-01 to 2017-02-01 with 6 month frequency: a
 *   zero-based index of '2' would refer to the start of the 3rd period, which would be 2013-02-01
 * @param date  the date of the schedule period boundary at which the change occurs, used to
 *   define the date of the step in absolute terms. This must be one of the '''unadjusted''' dates
 *   in the schedule period schedule; it is an unadjusted date and calculation period business day
 *   adjustments will apply. For example, in the swap above, the date '2013-02-01' is an unadjusted
 *   schedule period boundary and so may be specified here
 * @param value  the value representing the change that occurs, which can be an absolute value or
 *   various kinds of relative value
 */
sealed abstract case class ValueStep private (
    periodIndex: Option[Int],
    date: Option[LocalDate],
    value: ValueAdjustment) {

  //-------------------------------------------------------------------------
  /**
   * Finds the index of this value step in the specified schedule.
   *
   * A step positioned by a period index resolves to that index, which is legal only while it
   * names a period the schedule has; an index at or beyond the end of the schedule is reported
   * rather than resolved.
   *
   * A step positioned by a date is matched against the period boundaries in two passes, in this
   * order: first against the '''unadjusted''' start dates, and only if no period starts on that
   * date against the '''adjusted''' start dates. The order matters and the two passes are not one
   * pass over both dates, because a date that is the unadjusted start of a later period and the
   * adjusted start of an earlier one resolves to the later period, exactly as in the Java
   * original. A date matching no boundary at all is not an error here: it is answered with
   * nothing, and the caller decides what to do with it - which is how a step whose date falls
   * inside a period rather than on its edge reaches [[findPreviousIndex]].
   *
   * The absence of a match is where this differs in shape from the method being ported, which
   * returned `-1` in that case. The sentinel becomes `Right(None)`, so a caller cannot mistake it
   * for an index, and the two things that can come back - an index, or no index - are
   * distinguished by the type rather than by the value.
   *
   * @param periods  the periods of the schedule to resolve against, in schedule order
   * @return the index of the schedule period this step applies at, nothing if this step is
   *   positioned by a date that matches no period boundary, or the failure describing why the
   *   position cannot be resolved at all
   */
  private[value] def findIndex(periods: NonEmptyList[SchedulePeriod]): FailureOr[Option[Int]] =
    findIndex(ValueStep.PeriodIndex.of(periods))

  /**
   * Finds the index of this value step in the schedule the specified index was built over.
   *
   * This is the operation above, answered from a [[ValueStep.PeriodIndex]] rather than from the
   * period list itself, and it is what a caller resolving '''many''' steps against '''one'''
   * schedule uses: the index is built once and every step is answered from it, where the list
   * form above rebuilds one per call. The two agree on every input by construction, the list form
   * being written in terms of this one, and the randomised equivalence property of the test suite
   * pins that against a linear search written independently.
   *
   * The semantics are those documented above, unchanged and in the same order: an index-based
   * step at or beyond the end of the schedule is reported, a date-based step is matched against
   * the unadjusted start dates and only then against the adjusted ones, and a date matching
   * neither is answered with nothing. The two passes are two lookups here rather than two walks,
   * and the second is only made where the first found nothing, which is what keeps the order of
   * the passes - and so the period a date that is the unadjusted start of one period and the
   * adjusted start of another resolves to - exactly as it was.
   *
   * @param periods  the index over the periods of the schedule to resolve against
   * @return the index of the schedule period this step applies at, nothing if this step is
   *   positioned by a date that matches no period boundary, or the failure describing why the
   *   position cannot be resolved at all
   */
  private[value] def findIndex(periods: ValueStep.PeriodIndex): FailureOr[Option[Int]] =
    periodIndex match {
      case Some(index) =>
        // index based
        if (index >= periods.size) {
          Left(Failure.Invalid(ValueStep.IndexBeyondSchedule))
        } else {
          Right(Some(index))
        }
      case None =>
        date match {
          case Some(stepDate) =>
            // date based, match one of the unadjusted period boundaries, then the adjusted ones;
            // `orElse` takes its alternative by name, so the adjusted lookup is not made at all
            // where the unadjusted one matched
            Right(
              periods
                .unadjustedStartIndexOf(stepDate)
                .orElse(periods.adjustedStartIndexOf(stepDate)))
          case None =>
            Left(Failure.Invalid(ValueStep.NoPositionHeld))
        }
    }

  /**
   * Finds the index of the period of the specified schedule that precedes this value step.
   *
   * This is the counterpart of [[findIndex]] for a step whose date falls '''inside''' a period
   * rather than on one of its boundaries: it names the period whose value the step adjusts, which
   * is the period the date falls in. It is only ever called on a date-based step, as it was in
   * the Java original.
   *
   * The answer is decided in the order the original decided it, which is the order these four
   * possibilities are listed in and matters wherever more than one of them could apply:
   *
   *   1. a date before the start of the schedule is reported - there is no preceding period;
   *   1. otherwise the period before the first one that starts after the date, so a date within
   *      a period selects that period;
   *   1. otherwise, a date after the end of the schedule is reported - the step is off the end;
   *   1. otherwise the last period, which is the period a date on the final boundary falls in.
   *
   * Taking the periods as a `cats.data.NonEmptyList` makes the "at least size 1" precondition the
   * ported method documented a property of the argument type: a schedule of no periods has no
   * preceding period to name under any of the four rules above, and cannot be passed here at all.
   *
   * @param periods  the periods of the schedule to resolve against, in schedule order
   * @return the index of the schedule period preceding this step, or the failure describing why
   *   the date of this step lies outside the schedule
   */
  private[value] def findPreviousIndex(periods: NonEmptyList[SchedulePeriod]): FailureOr[Int] =
    findPreviousIndex(ValueStep.PeriodIndex.of(periods))

  /**
   * Finds the index of the period preceding this value step in the schedule the specified index
   * was built over.
   *
   * This is the operation above, answered from a [[ValueStep.PeriodIndex]] rather than from the
   * period list itself, and it stands to it exactly as the two [[findIndex]] members stand to
   * each other: one index serves every step, the list form is written in terms of this one, and
   * the four ordered rules and both messages are those documented above, unchanged.
   *
   * @param periods  the index over the periods of the schedule to resolve against
   * @return the index of the schedule period preceding this step, or the failure describing why
   *   the date of this step lies outside the schedule
   */
  private[value] def findPreviousIndex(periods: ValueStep.PeriodIndex): FailureOr[Int] =
    date match {
      case Some(stepDate) => previousIndexOf(stepDate, periods)
      case None => Left(Failure.Invalid(ValueStep.NoDateHeld))
    }

  //-------------------------------------------------------------------------
  /**
   * Finds the index of the period preceding the specified date, which [[findPreviousIndex]]
   * delegates to once it holds the date of this step.
   *
   * Taking the date as a parameter is what keeps the caller total: the date of a step is an
   * optional property, and reading it out of the option rather than pattern-matching on it would
   * be a partial operation on a value this type does happen to guarantee, but guarantees by an
   * invariant the compiler cannot see.
   *
   * The middle of the four rules - the period before the first one that starts after the date -
   * is the one the index answers, and it answers it without assuming the periods are sorted;
   * see [[ValueStep.PeriodIndex.indexBeforeFirstLaterStart]] for how, and why that matters.
   *
   * @param stepDate  the date of this step
   * @param periods  the index over the periods of the schedule to resolve against
   * @return the index of the schedule period preceding the date, or the failure describing why
   *   the date lies outside the schedule
   */
  private def previousIndexOf(
      stepDate: LocalDate,
      periods: ValueStep.PeriodIndex): FailureOr[Int] =
    if (stepDate.isBefore(periods.firstUnadjustedStartDate)) {
      Left(
        Failure.Invalid(
          "ValueStep date is before the start of the schedule: " +
            s"$stepDate < ${periods.firstUnadjustedStartDate}"))
    } else {
      periods.indexBeforeFirstLaterStart(stepDate) match {
        case Some(index) => Right(index)
        case None if stepDate.isAfter(periods.lastUnadjustedEndDate) =>
          Left(
            Failure.Invalid(
              "ValueStep date is after the end of the schedule: " +
                s"$stepDate > ${periods.lastUnadjustedEndDate}"))
        case None => Right(periods.size - 1)
      }
    }

  //-------------------------------------------------------------------------
  /**
   * Renders this step as text.
   *
   * The rendering is the property-by-property form of the Java bean being ported, naming the
   * position this step holds and the adjustment it makes:
   *
   * {{{
   * ValueStep{periodIndex=2, value=ValueAdjustment[result = input + -2000.0]}
   * ValueStep{date=2014-06-30, value=ValueAdjustment[result = input + -2000.0]}
   * }}}
   *
   * One detail differs from the bean, deliberately and cosmetically: the bean named all three
   * properties and rendered the absent one as an empty value, while this names only the position
   * actually held. A step holds exactly one of the two positions, so the field set is decided by
   * the invariant of the type rather than by the data, and a reader comparing two renderings
   * still compares the same layout. Nothing a value holds is hidden by the omission, and no test
   * of either implementation asserts the form.
   *
   * @return the text form of this step
   */
  override def toString: String = {
    val fields =
      periodIndex.map(index => s"periodIndex=$index").toList :::
        date.map(stepDate => s"date=$stepDate").toList :::
        List(s"value=$value")
    s"ValueStep{${fields.mkString(", ")}}"
  }
}

/**
 * Companion of [[ValueStep]], holding its factories, its typeclass instances and its codec.
 *
 * The three factories are the only way to obtain a step from outside this file, which is what
 * makes the invariant of the type - exactly one position held, and a period index of one or
 * greater where one is held - a property of every value that exists rather than a property a
 * caller is asked to respect. They replace the two factories and the builder of the bean being
 * ported: the builder had no counterpart here, and the third factory below is what a caller that
 * assembled a step field by field reaches for instead.
 *
 * Two typeclass instances are published, and exactly two: a `Hash`, which is the single
 * equality-bearing instance of the type - `Hash` extends `Eq`, so declaring an `Eq` as well would
 * leave two instances that could disagree and one of them ambiguous - and a `Show`. There is
 * deliberately no `Order`: the Java type is not `Comparable`, and two steps positioned in
 * different terms, one by an index into a schedule and one by a date, have no ordering between
 * them worth inventing.
 */
object ValueStep {

  //-------------------------------------------------------------------------
  /** Reported when neither position is supplied, in the words of the bean being ported. */
  private val EitherPositionRequired: String = "Either the 'periodIndex' or 'date' must be set"

  /** Reported when both positions are supplied, in the words of the bean being ported. */
  private val SinglePositionRequired: String =
    "Either the 'periodIndex' or 'date' must be set, not both"

  /** Reported when the period index names the start of the first period, or worse. */
  private val PeriodIndexNotPositive: String = "The 'periodIndex' must not be zero or negative"

  /** Reported by `findIndex` when the period index names no period of the schedule. */
  private val IndexBeyondSchedule: String = "ValueStep index is beyond last schedule period"

  /**
   * Reported by `findIndex` for a step holding neither position.
   *
   * No value of this type is in that state - the factories below reject it - so this describes a
   * defect in this library rather than anything a caller did. It is reported rather than raised
   * because this module raises nothing, and it names the type so that a report reaching a log is
   * traceable to here.
   */
  private val NoPositionHeld: String = "ValueStep holds neither a period index nor a date"

  /**
   * Reported by `findPreviousIndex` for a step that is not date-based.
   *
   * As with the message above, no caller can bring this about through a legal value: the
   * preceding period of a step is asked for only once its date has failed to match a boundary,
   * which an index-based step never does.
   */
  private val NoDateHeld: String = "ValueStep is not date-based, so it has no preceding period"

  //-------------------------------------------------------------------------
  /**
   * An index over the periods of one schedule, answering the questions a step asks of them.
   *
   * ===Why it exists===
   *
   * A step resolves by searching the periods of the schedule it is applied to: for the period
   * that starts on its date, or for the period its date falls in. Searching the list itself
   * answers each question in a walk, which is what the Java original did and what a direct
   * transcription of it does; resolving a definition of `m` steps against a schedule of `n`
   * periods then costs `m * n` walks, and a definition holding a sequence expanded into a step
   * per period makes that quadratic in the size of the schedule alone. This index is built
   * '''once''' per resolution and answers each question in constant time, or in logarithmic time
   * for the one question that genuinely needs a search, so the same resolution costs `n + m log n`
   * and allocates one index rather than one zipped list per step.
   *
   * ===What it holds, and why each part is shaped the way it is===
   *
   * The two date-to-index maps are '''first-hit''': where several periods share a start date, the
   * map holds the earliest of them. That is not an arbitrary choice of tie-break but the
   * behaviour being preserved - the search it replaces answered with the first matching period -
   * and it is why the maps are folded rather than built from a list of pairs, which would keep
   * the last duplicate instead of the first.
   *
   * The prefix maxima are what let the predecessor question be answered by a search rather than
   * by a walk. That question is "the first period, after the first one, whose unadjusted start
   * date is after this date", and a binary search over the start dates themselves would be wrong:
   * a schedule is not required to hold its periods in order, and `Schedule.of` accepts any order
   * and explicitly allows periods that are not adjacent. But for any date `d`,
   * `min{i : start(i) > d}` equals `min{i : max(start(1)..start(i)) > d}`, because a prefix
   * maximum exceeds `d` exactly when one of the dates it covers does; the prefix maxima are
   * non-decreasing by construction, whatever order the periods are in, so that second form '''is'''
   * searchable. The maxima cover indices `1` to `n - 1` only, since the question excludes the
   * first period.
   *
   * ===Thread safety===
   *
   * An instance is immutable and holds only immutable values, so it is safe to share between any
   * number of threads without synchronisation. It is also worth nothing beyond the resolution it
   * was built for, which is why it is neither published nor cached: it is a function of the
   * period list, and holding one alongside a schedule would be a second copy of that list to keep
   * in step with it.
   *
   * @param unadjustedStartDates  the unadjusted start date of each period, in schedule order,
   *   non-empty because the factory takes a non-empty list and the constructor is private
   * @param unadjustedStartIndices  the index of the '''first''' period starting on each
   *   unadjusted start date
   * @param adjustedStartIndices  the index of the '''first''' period starting on each adjusted
   *   start date
   * @param lastUnadjustedEndDate  the unadjusted end date of the last period, which is the end of
   *   the schedule as the predecessor question measures it
   * @param laterStartMaxima  the running maximum of the unadjusted start dates of the periods
   *   after the first, one entry per such period, in schedule order
   */
  private[value] final class PeriodIndex private (
      unadjustedStartDates: Vector[LocalDate],
      unadjustedStartIndices: Map[LocalDate, Int],
      adjustedStartIndices: Map[LocalDate, Int],
      val lastUnadjustedEndDate: LocalDate,
      laterStartMaxima: Vector[LocalDate]) {

    /**
     * The number of periods in the schedule this index was built over.
     *
     * @return the period count, one or more
     */
    def size: Int = unadjustedStartDates.size

    /**
     * The unadjusted start date of the first period, which is the start of the schedule as the
     * predecessor question measures it.
     *
     * @return the unadjusted start date of the first period
     */
    def firstUnadjustedStartDate: LocalDate = unadjustedStartDates.head

    /**
     * The unadjusted start date of the period at the specified index.
     *
     * Answers with nothing for an index the schedule does not have, so a caller naming a period
     * it worked out for itself cannot turn a mistake into a raised error. The message reporting
     * two steps that collided in one period is the caller this is for: it names the date the
     * period starts on, and the index it names it from came from resolving a step against this
     * very index.
     *
     * @param index  the zero-based index of the period
     * @return the unadjusted start date of that period, or nothing if the schedule has no such
     *   period
     */
    def unadjustedStartDateAt(index: Int): Option[LocalDate] = unadjustedStartDates.lift(index)

    /**
     * The index of the first period whose '''unadjusted''' start date is the specified date.
     *
     * @param date  the date to look up
     * @return the index of the first such period, or nothing if no period starts on that date
     */
    def unadjustedStartIndexOf(date: LocalDate): Option[Int] = unadjustedStartIndices.get(date)

    /**
     * The index of the first period whose '''adjusted''' start date is the specified date.
     *
     * @param date  the date to look up
     * @return the index of the first such period, or nothing if no period starts on that date
     */
    def adjustedStartIndexOf(date: LocalDate): Option[Int] = adjustedStartIndices.get(date)

    /**
     * The index of the period before the first period, after the first one, that starts after the
     * specified date.
     *
     * This is the middle rule of [[ValueStep.findPreviousIndex]], and it is answered by a binary
     * search over the prefix maxima of the start dates rather than by a walk over the dates
     * themselves - see the documentation of this type for why the maxima are the searchable form
     * of the question and why the dates are not. The answer is the index found '''minus one''',
     * which is the position of the maximum in a vector that starts at period one and so needs no
     * subtraction of its own.
     *
     * @param date  the date of the step being resolved
     * @return the index of the period before the first later-starting one, or nothing if no
     *   period after the first starts after that date
     */
    def indexBeforeFirstLaterStart(date: LocalDate): Option[Int] = {
      val position = firstLaterStart(date, 0, laterStartMaxima.size)
      if (position < laterStartMaxima.size) Some(position) else None
    }

    /**
     * Searches the prefix maxima for the first position holding a date after the one specified.
     *
     * The maxima are non-decreasing, so "holds a date after this one" is a predicate that is
     * false on a prefix of the vector and true on the rest, and the position it first becomes
     * true at is found by halving the range it can lie in. The recursion is in tail position and
     * is compiled to a loop, which is how this is written without mutable state; the bounds
     * shrink on every call, so it terminates, and it answers the size of the vector where the
     * predicate holds nowhere.
     *
     * @param date  the date to compare the maxima against
     * @param low  the first position that could satisfy the predicate
     * @param high  the position after the last one that could satisfy it
     * @return the first position holding a date after the one specified, or the size of the
     *   vector of maxima if none does
     */
    @tailrec
    private def firstLaterStart(date: LocalDate, low: Int, high: Int): Int =
      if (low >= high) {
        low
      } else {
        val middle = low + (high - low) / 2
        if (laterStartMaxima(middle).isAfter(date)) {
          firstLaterStart(date, low, middle)
        } else {
          firstLaterStart(date, middle + 1, high)
        }
      }
  }

  /**
   * Companion of [[PeriodIndex]], holding the single factory that builds one.
   *
   * The constructor of the type is private, so this is the only way an index comes about, and the
   * five parts of one are therefore always derived from the same period list in the same place.
   */
  private[value] object PeriodIndex {

    /**
     * Builds the index over the specified periods.
     *
     * Every part is built by a traversal of the periods or of a vector derived from one: the two
     * vectors of start dates, the first-hit map over each of them, the prefix maxima, and the
     * unadjusted end date of the last period. The cost is therefore linear in the number of
     * periods and is paid once per resolution, which is the whole point of the type.
     *
     * @param periods  the periods of the schedule to index, in schedule order
     * @return the index over those periods
     */
    def of(periods: NonEmptyList[SchedulePeriod]): PeriodIndex = {
      val periodList = periods.toList
      val unadjustedStartDates = periodList.iterator.map(_.unadjustedStartDate).toVector
      val adjustedStartDates = periodList.iterator.map(_.startDate).toVector
      val laterStarts = unadjustedStartDates.drop(1)
      val laterStartMaxima =
        laterStarts.headOption.fold(Vector.empty[LocalDate])(firstStart =>
          laterStarts.tail.scanLeft(firstStart)((runningMax, date) =>
            if (date.isAfter(runningMax)) date else runningMax))
      new PeriodIndex(
        unadjustedStartDates,
        firstHitIndices(unadjustedStartDates),
        firstHitIndices(adjustedStartDates),
        periods.last.unadjustedEndDate,
        laterStartMaxima)
    }

    /**
     * Maps each of the specified dates to the '''first''' position it appears at.
     *
     * A date appearing more than once keeps its earliest position, which is the behaviour the
     * searches this index replaces had: each answered with the first period that matched. Folding
     * is what achieves that - building the map from a list of pairs would keep the last duplicate
     * instead - and a date is only entered where it is not already present, so the fold does no
     * work per duplicate beyond the lookup.
     *
     * @param dates  the dates to index, in schedule order
     * @return the first position of each distinct date
     */
    private def firstHitIndices(dates: Vector[LocalDate]): Map[LocalDate, Int] =
      dates.iterator.zipWithIndex.foldLeft(Map.empty[LocalDate, Int]) {
        case (indices, (date, index)) =>
          if (indices.contains(date)) indices else indices.updated(date, index)
      }
  }

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance that applies at the specified schedule period index.
   *
   * This factory is used to define the date that the step occurs in relative terms. The date is
   * identified by specifying the zero-based index of the schedule period boundary. The change
   * will occur at the start of the specified period. Thus an index of zero is the start of the
   * first period or initial stub. The index must be one or greater, as a change is not permitted
   * at the start of the first period, and an index of zero or less is reported:
   *
   * {{{
   * ValueStep.of(2, ValueAdjustment.ofDeltaAmount(-2000))  // Right(the step at the 3rd period)
   * ValueStep.of(0, ValueAdjustment.ofDeltaAmount(-2000))  // Left(must not be zero or negative)
   * }}}
   *
   * For example, consider a 5 year swap from 2012-02-01 to 2017-02-01 with 6 month frequency. A
   * zero-based index of '2' would refer to start of the 3rd period, which would be 2013-02-01.
   *
   * The value may be absolute or relative, as per [[ValueAdjustment]].
   *
   * @param periodIndex  the index of the period of the value change
   * @param value  the adjustment to make to the value
   * @return the varying step, or the failure describing why the index describes none
   */
  def of(periodIndex: Int, value: ValueAdjustment): ResultNec[ValueStep] =
    checkedPeriodIndex(periodIndex)
      .map(_ => create(Some(periodIndex), None, value))
      .toEither

  /**
   * Obtains an instance that applies at the specified date.
   *
   * This factory obtains a step that causes the value to change at the specified date. The value
   * may be absolute or relative, as per [[ValueAdjustment]].
   *
   * Construction is total, as it was in the Java original: a date is a date, and whether it lines
   * up with a boundary of some schedule is a question about that schedule rather than about this
   * step, decided where the step is resolved. So no outcome is reported here and callers building
   * a run of steps - a [[ValueStepSequence]] walking its dates is the one inside this library -
   * need no error handling around the construction of each one.
   *
   * @param date  the start date of the value change
   * @param value  the adjustment to make to the value
   * @return the varying step
   */
  def of(date: LocalDate, value: ValueAdjustment): ValueStep = create(None, Some(date), value)

  /**
   * Obtains an instance from both optional positions and the adjustment.
   *
   * This is the factory for a caller holding the fields of a step rather than one of the two
   * positions in particular - the decoder below is one, and code that read a step out of some
   * external shape is another - and it is what replaces the builder of the bean being ported.
   * It performs exactly the checks the bean's validator performed, and reports them in the
   * bean's words:
   *
   * {{{
   * ValueStep.of(Some(2), None, adjustment)               // Right(the step at the 3rd period)
   * ValueStep.of(None, Some(date), adjustment)            // Right(the step at that date)
   * ValueStep.of(None, None, adjustment)                  // Left(either must be set)
   * ValueStep.of(Some(1), Some(date), adjustment)         // Left(not both)
   * ValueStep.of(Some(0), None, adjustment)               // Left(must not be zero or negative)
   * ValueStep.of(Some(0), Some(date), adjustment)         // Left(both of the two above)
   * }}}
   *
   * The two checks are independent, so both are reported when both fail, which is the last line
   * above and is more than the validator being ported said: it raised the first fault it found
   * and stopped, so a caller correcting its input learned of the second only on the next attempt.
   * Nothing is invented to accumulate, though - the range of the index is checked only where an
   * index is present, because an index that is absent has no range to be wrong about, and that
   * check simply passes.
   *
   * @param periodIndex  the index of the period of the value change, if the step is positioned in
   *   relative terms
   * @param date  the start date of the value change, if the step is positioned in absolute terms
   * @param value  the adjustment to make to the value
   * @return the varying step, or the failures describing why the fields describe none
   */
  def of(
      periodIndex: Option[Int],
      date: Option[LocalDate],
      value: ValueAdjustment): ResultNec[ValueStep] =
    (checkedPosition(periodIndex, date), checkedOptionalPeriodIndex(periodIndex))
      .mapN((_, _) => create(periodIndex, date, value))
      .toEither

  //-------------------------------------------------------------------------
  /**
   * Creates a value, which every route into the type funnels through.
   *
   * This is the only instantiation of the type and it is private, so the three factories above
   * are the whole of its construction. The type is an abstract case class with a private
   * constructor, so it has neither a public `apply` nor a `copy` and this is written as an
   * anonymous extension of it - the shape every validated type of this port uses to keep those
   * two synthesised members from existing while `unapply` and pattern matching still do.
   *
   * @param periodIndex  the checked period index, if the step is positioned in relative terms
   * @param date  the date, if the step is positioned in absolute terms
   * @param value  the adjustment to make to the value
   * @return the step holding the three fields
   */
  private def create(
      periodIndex: Option[Int],
      date: Option[LocalDate],
      value: ValueAdjustment): ValueStep =
    new ValueStep(periodIndex, date, value) {}

  /**
   * Checks that exactly one of the two positions is supplied.
   *
   * @param periodIndex  the period index supplied, if any
   * @param date  the date supplied, if any
   * @return a passing outcome if exactly one position was supplied, otherwise the failure saying
   *   which way the pair is wrong
   */
  private def checkedPosition(
      periodIndex: Option[Int],
      date: Option[LocalDate]): ValidatedFailures[Unit] =
    (periodIndex.isDefined, date.isDefined) match {
      case (false, false) => Validate.invalidNec[Unit](EitherPositionRequired)
      case (true, true) => Validate.invalidNec[Unit](SinglePositionRequired)
      case _ => Validate.valid(())
    }

  /**
   * Checks that the specified period index does not name the start of the first period.
   *
   * @param periodIndex  the period index supplied
   * @return a passing outcome if the index is one or greater, otherwise the failure
   */
  private def checkedPeriodIndex(periodIndex: Int): ValidatedFailures[Unit] =
    Validate.isTrue(periodIndex >= 1, PeriodIndexNotPositive)

  /**
   * Checks the period index where one is supplied, passing where none is.
   *
   * @param periodIndex  the period index supplied, if any
   * @return a passing outcome if no index was supplied or the index is one or greater, otherwise
   *   the failure
   */
  private def checkedOptionalPeriodIndex(periodIndex: Option[Int]): ValidatedFailures[Unit] =
    periodIndex match {
      case Some(index) => checkedPeriodIndex(index)
      case None => Validate.valid(())
    }

  //-------------------------------------------------------------------------
  /**
   * The hashing and equality of steps.
   *
   * Taken from the `equals` and `hashCode` of the type, which are those synthesised for its three
   * properties and so are those of [[ValueAdjustment]] where its double is concerned - compared
   * by bit pattern, as every double-bearing type of this port compares one. This is the type's
   * only equality-bearing instance, and `Eq[ValueStep]` is obtained from it by subtyping.
   *
   * @return the hashing of steps
   */
  implicit val hash: Hash[ValueStep] = Hash.fromUniversalHashCode[ValueStep]

  /**
   * The rendering of steps as text.
   *
   * Renders what [[ValueStep.toString]] renders, so the two ways of putting a step into a message
   * agree.
   *
   * @return the rendering of a step
   */
  implicit val show: Show[ValueStep] = Show.show(_.toString)

  //-------------------------------------------------------------------------
  /**
   * The raw field shape of a step, from which both halves of the codec below are derived.
   *
   * A validated type needs this intermediate product because derivation reads the public
   * constructor of a product and this type has none - it is an abstract case class whose
   * constructor is private - so there is no public shape to derive from. Writing the three fields
   * out by hand instead would state the same contract a second time.
   *
   * @param periodIndex  the period index, if the step is positioned in relative terms
   * @param date  the date, carried as its ISO date string, if positioned in absolute terms
   * @param value  the adjustment, carried as the object its own codec writes
   */
  private final case class Raw(
      periodIndex: Option[Int],
      date: Option[LocalDate],
      value: ValueAdjustment)

  /** The derived encoder of the raw field shape, used by the encoder below. */
  private val rawEncoder: Encoder[Raw] = deriveEncoder[Raw]

  /** The derived decoder of the raw field shape, used by the validating decoder below. */
  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /**
   * The JSON encoding of steps.
   *
   * A value is an object holding the position it actually has and its adjustment, under the names
   * the Java bean declared and in declaration order:
   *
   * {{{
   * {"periodIndex":2,"value":{"modifyingValue":-2000.0,"type":"DeltaAmount"}}
   * {"date":"2014-06-30","value":{"modifyingValue":-2000.0,"type":"DeltaAmount"}}
   * }}}
   *
   * The position that is not held is dropped from the document rather than written as an
   * explicitly empty field, which is the policy every product of this port follows; the derived
   * decoder reads an absent field as holding nothing, so the round trip is exact either way. The
   * adjustment is written by its own codec, the date as its ISO form, and the index as a JSON
   * number.
   *
   * Both halves of the codec are assembled by the same compile-time derivation over the same raw
   * shape, which is what keeps them from drifting apart, and no part of the encoding inspects a
   * class while the program runs. Two equal values encode to identical bytes: the fields are
   * written in their declaration order, a date has one ISO form, and the field set is decided by
   * the position held rather than by anything a caller chose.
   *
   * @return the JSON encoding of steps
   */
  implicit val encoder: Encoder[ValueStep] =
    Codecs.dropNulls(rawEncoder.contramap[ValueStep] { step =>
      Raw(step.periodIndex, step.date, step.value)
    })

  /**
   * The JSON decoding of steps.
   *
   * This is the inverse of the encoding above, and it decides whether the fields describe a value
   * exactly as a caller's arguments are decided: the payload is read into the raw shape and
   * handed to the three-field [[ValueStep.of]], so a document that names both positions, names
   * neither, or carries a period index of zero or less is a decoding failure carrying every
   * reason it is, rather than a value this type would not have built. The adjustment has to be
   * present; the two positions are optional individually, and it is the factory rather than the
   * shape of the document that requires exactly one of them.
   *
   * @return the JSON decoding of steps
   */
  implicit val decoder: Decoder[ValueStep] =
    Codecs.validatedDecoder[Raw, ValueStep] { raw =>
      of(raw.periodIndex, raw.date, raw.value)
    }(rawDecoder)
}
