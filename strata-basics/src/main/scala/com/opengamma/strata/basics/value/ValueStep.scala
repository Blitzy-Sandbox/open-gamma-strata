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
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.NoJavaSerialization
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
 * Both positions are declared as `Option`s, so the one a step does not hold holds nothing.
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
 * by bit pattern rather than by numeric comparison. A step carries no double of its own, so it
 * inherits that behaviour whole rather than restating it, with both of its consequences: a step
 * always equals itself even when its adjustment carries a value that is not a number, and a
 * negative zero is distinct from a positive zero.
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
 * @param date  the date of the schedule period boundary at which the change occurs, which
 *   defines the date of the step in absolute terms. This must be one of the '''unadjusted''' dates
 *   in the schedule period schedule; it is an unadjusted date and calculation period business day
 *   adjustments will apply. For example, in the swap above, the date '2013-02-01' is an unadjusted
 *   schedule period boundary and so may be specified here
 * @param value  the value representing the change that occurs, which can be an absolute value or
 *   various kinds of relative value
 */
sealed abstract case class ValueStep private (
    periodIndex: Option[Int],
    date: Option[LocalDate],
    value: ValueAdjustment)
    extends NoJavaSerialization {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means - which
  // could hold both positions at once or neither, the check `of` accumulates - can be stopped is
  // here. The single implementation is the companion's hidden `Impl`.
  JvmClosure.requireSoleImplementation(this, classOf[ValueStep.Impl])

  // The invariant of this type, stated over the fields the instance actually holds rather than
  // over the arguments a factory was given, because the class file of the implementation carries a
  // public constructor whatever the source asked for: a caller compiled outside this library can
  // name that constructor directly, and the identity check above would admit a step holding both
  // positions at once or neither. These are the two checks [[ValueStep.of]] accumulates, and the
  // first of them is what makes a step a position: [[ValueStep.findIndex]] reads the index where
  // there is one and the date otherwise, and [[ValueSchedule]] groups steps by that same choice,
  // so a step with neither position would be a step at no position and one with both would be a
  // step at two.
  JvmClosure.requireInvariant(
    "exactly one of its period index and its date is present",
    periodIndex.isDefined != date.isDefined)
  JvmClosure.requireInvariant(
    "its period index, where present, is one or greater",
    periodIndex.forall(index => index >= 1))

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
   * adjusted start of an earlier one resolves to the later period. A date matching no boundary at
   * all is not an error here: it is answered with nothing, and the caller decides what to do with
   * it - which is how a step whose date falls inside a period rather than on its edge reaches
   * [[findPreviousIndex]].
   *
   * An index and the absence of a match are distinguished by the type rather than by a sentinel
   * value: no match is `Right(None)`, which a caller cannot mistake for a period index.
   *
   * @param periods  the periods of the schedule to resolve against, in schedule order
   * @return the index of the schedule period this step applies at, nothing if this step is
   *   positioned by a date that matches no period boundary, or the failure naming the condition
   *   that is broken: a period index that is not less than the number of periods in the schedule,
   *   or a step holding neither of the two positions
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
   * being written in terms of this one.
   *
   * The semantics are those documented above, unchanged and in the same order: an index-based
   * step at or beyond the end of the schedule is reported, a date-based step is matched against
   * the unadjusted start dates and only then against the adjusted ones, and a date matching
   * neither is answered with nothing. The two passes are two questions asked of the index here
   * rather than two searches written out, and the second is only asked where the first found
   * nothing, which is what preserves the order of the passes - and so the period a date that is
   * the unadjusted start of one period and the adjusted start of another resolves to. Whether the
   * index answers a pass by a lookup or by a walk is its own business and changes no answer; not
   * asking the second question at all is also what keeps the adjusted half of the index from
   * being built where no step needs it.
   *
   * @param periods  the index over the periods of the schedule to resolve against
   * @return the index of the schedule period this step applies at, nothing if this step is
   *   positioned by a date that matches no period boundary, or the failure naming the condition
   *   that is broken: a period index that is not less than the number of periods in the schedule,
   *   or a step holding neither of the two positions
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
   * is the period the date falls in. It is only ever called on a date-based step.
   *
   * The answer is decided in the order these four possibilities are listed in, and the order
   * matters wherever more than one of them could apply:
   *
   *   1. a date before the start of the schedule is reported - there is no preceding period;
   *   1. otherwise the period before the first one that starts after the date, so a date within
   *      a period selects that period;
   *   1. otherwise, a date after the end of the schedule is reported - the step is off the end;
   *   1. otherwise the last period, which is the period a date on the final boundary falls in.
   *
   * Taking the periods as a `cats.data.NonEmptyList` makes the "at least size 1" precondition a
   * property of the argument type: a schedule of no periods has no preceding period to name under
   * any of the four rules above, and cannot be passed here at all.
   *
   * @param periods  the periods of the schedule to resolve against, in schedule order
   * @return the index of the schedule period preceding this step, or the failure naming the
   *   condition that is broken: the date of this step is before the unadjusted start date of the
   *   first period or after the unadjusted end date of the last one, or this step is positioned
   *   by a period index and so holds no date to place
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
   * the four ordered rules and both reports are those documented above, unchanged.
   *
   * @param periods  the index over the periods of the schedule to resolve against
   * @return the index of the schedule period preceding this step, or the failure naming the
   *   condition that is broken: the date of this step is before the unadjusted start date of the
   *   first period or after the unadjusted end date of the last one, or this step is positioned
   *   by a period index and so holds no date to place
   */
  private[value] def findPreviousIndex(periods: ValueStep.PeriodIndex): FailureOr[Int] =
    date match {
      case Some(stepDate) => previousIndexOf(stepDate, periods)
      case None => Left(Failure.Invalid(ValueStep.NoDateHeld))
    }

  /**
   * Finds the index of the period preceding the specified date, which [[findPreviousIndex]]
   * delegates to once it holds the date of this step.
   *
   * Taking the date as a parameter is what keeps this operation total: the date of a step is an
   * optional property, and reading it out of the option rather than pattern-matching on it would
   * be a partial operation on a value the type does guarantee, but guarantees by an invariant
   * that is not visible in the type of the field.
   *
   * The middle of the four rules - the period before the first one that starts after the date -
   * is the one the index answers, and it answers it without requiring the periods to be sorted;
   * see [[ValueStep.PeriodIndex.indexBeforeFirstLaterStart]] for how, and why that matters.
   *
   * @param stepDate  the date of this step
   * @param periods  the index over the periods of the schedule to resolve against
   * @return the index of the schedule period preceding the date, or the failure naming the bound
   *   the date breaks: it is before the unadjusted start date of the first period, or after the
   *   unadjusted end date of the last one
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

  /**
   * Renders this step as text.
   *
   * The rendering names the position this step holds and the adjustment it makes, property by
   * property:
   *
   * {{{
   * ValueStep{periodIndex=2, value=ValueAdjustment[result = input + -2000.0]}
   * ValueStep{date=2014-06-30, value=ValueAdjustment[result = input + -2000.0]}
   * }}}
   *
   * Only the position actually held is named. A step holds exactly one of the two, so the field
   * set is decided by the invariant of the type rather than by the data, and two renderings of
   * two steps are therefore still of the same layout.
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
 * caller is asked to respect. The third of them is the one a caller that assembled a step field
 * by field reaches for.
 *
 * Two typeclass instances are published, and exactly two: a `Hash`, which is the single
 * equality-bearing instance of the type - `Hash` extends `Eq`, so declaring an `Eq` as well would
 * leave two instances that could disagree and one of them ambiguous - and a `Show`. There is
 * deliberately no `Order`: two steps positioned in different terms, one by an index into a
 * schedule and one by a date, have no ordering between them worth inventing.
 */
object ValueStep {

  /** Reported when neither position is supplied. */
  private val EitherPositionRequired: String = "Either the 'periodIndex' or 'date' must be set"

  /** Reported when both positions are supplied. */
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
   * because every failure of this module is reported, and it names the type so that a report
   * reaching a log is traceable to here.
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

  /**
   * An index over the periods of one schedule, answering the questions a step asks of them.
   *
   * ===Why it exists===
   *
   * A step resolves by searching the periods of the schedule it is applied to: for the period
   * that starts on its date, or for the period its date falls in. Four questions are asked in
   * all - the number of periods, the unadjusted start date of one named period, the period that
   * starts on a date, and the period a date falls in - and this type is the single answerer of
   * them, so that a resolution reaches the period list in one place rather than once per step and
   * once per question.
   *
   * ===Why nothing here is built until it is asked for===
   *
   * A walk of the periods allocates nothing; the structures that replace a walk with a lookup
   * cost an entry per period, whether one step is being placed or a thousand. Building them
   * unconditionally therefore made a definition holding a single step pay for the whole schedule -
   * measured at ~90x the time and ~83x the allocation of the same resolution in the Java original,
   * which walks - and left resolution of a stepped schedule allocation-bound rather than
   * compute-bound.
   *
   * So this type holds the '''period list itself''' and derives eagerly only what a traversal that
   * allocates nothing can derive: the number of periods, the unadjusted start date of the first,
   * and the unadjusted end date of the last. The three questions that genuinely search are
   * answered by one of two strategies, chosen once when the index is built:
   *
   *   - the '''scan''' strategy walks the period list in schedule order and allocates nothing at
   *     all. This is the shape of the Java original, and it is the cheaper of the two for the
   *     step counts a definition actually holds, which is one or a handful;
   *   - the '''indexed''' strategy answers from a first-hit map per kind of start date and from a
   *     vector of the prefix maxima of the start dates. All three are `lazy val`s, so a resolution
   *     builds only the ones its own steps consult: a definition whose steps all land on
   *     unadjusted boundaries builds the unadjusted map alone, one whose steps are all positioned
   *     by a period index builds none of the three, and the adjusted map is built only where the
   *     date of some step matched no unadjusted boundary.
   *
   * The choice is made from the number of date-positioned steps about to be placed, against
   * [[PeriodIndex.IndexedLookupThreshold]], which records where the two were measured to cross
   * over. Both strategies answer every question identically - the scan takes the first hit in
   * schedule order, and the maps are built to hold the first hit for exactly that reason - so the
   * choice is one of cost alone and never one of answer.
   *
   * ===Why the two searching structures are shaped the way they are===
   *
   * The two date-to-index maps are '''first-hit''': where several periods share a start date, the
   * map holds the earliest of them, which is the period a walk over the list in schedule order
   * matches. Each is built in a single pass whose per-period cost is one entry rather than one
   * map: the periods become date-and-index pairs in '''reverse''' schedule order and are handed to
   * the map factory, which keeps the last pair offered for a repeated key - and the last pair
   * offered for a date is the earliest position that date appears at. Entering each date into a
   * map derived from the previous one would instead cost a fresh map per period, which is what the
   * allocation measured above was mostly made of.
   *
   * The prefix maxima are what let the preceding-period question be answered by a search rather
   * than by a walk. That question is "the first period, after the first one, whose unadjusted start
   * date is after this date", and a binary search over the start dates themselves would be wrong:
   * a schedule is not required to hold its periods in order, and `Schedule.of` accepts any order
   * and explicitly allows periods that are not adjacent. But for any date `d`,
   * `min{i : start(i) > d}` equals `min{i : max(start(1)..start(i)) > d}`, because a prefix
   * maximum exceeds `d` exactly when one of the dates it covers does; the prefix maxima are
   * non-decreasing by construction, whatever order the periods are in, so that second form '''is'''
   * searchable. The maxima cover indices `1` to `n - 1` only, since the question excludes the
   * first period. The scan form of that same question walks those same periods in order and stops
   * at the first one that starts later, which needs no such identity - which is why the two
   * strategies agree on a period list held in any order at all.
   *
   * ===Thread safety===
   *
   * An instance is immutable and holds only immutable values, so it is safe to share between any
   * number of threads without synchronisation. The three derived structures are `lazy val`s: the
   * platform performs each initialisation at most once however many threads reach it together and
   * publishes the result safely, and each is a pure function of the period list, so no thread can
   * observe a half-built structure or a different answer from another thread's. An instance is
   * also worth nothing beyond the resolution it was built for, which is why it is neither
   * published nor cached: it is a function of the period list, and holding one alongside a
   * schedule would be a second copy of that list to keep in step with it.
   *
   * @param periods  the periods of the schedule to resolve against, in schedule order, non-empty
   *   because the factory takes a non-empty list and the constructor is private
   * @param size  the number of periods in the schedule this index was built over, one or more,
   *   counted once by the factory
   * @param firstUnadjustedStartDate  the unadjusted start date of the first period, which is the
   *   start of the schedule as the preceding-period question measures it
   * @param lastUnadjustedEndDate  the unadjusted end date of the last period, which is the end of
   *   the schedule as the preceding-period question measures it
   * @param indexedLookups  whether the three searching questions are answered from the lazily
   *   built structures rather than by a scan of the period list, decided by the factory from the
   *   number of date-positioned steps that are about to be placed against this index
   */
  private[value] final class PeriodIndex private (
      periods: List[SchedulePeriod],
      val size: Int,
      val firstUnadjustedStartDate: LocalDate,
      val lastUnadjustedEndDate: LocalDate,
      indexedLookups: Boolean) {

    /**
     * The index of the '''first''' period starting on each unadjusted start date.
     *
     * Built on first use and only under the indexed strategy, so a resolution that scans never
     * pays for it and one that indexes pays for it once.
     */
    private lazy val unadjustedStartIndices: Map[LocalDate, Int] =
      firstHitIndices(period => period.unadjustedStartDate)

    /**
     * The index of the '''first''' period starting on each adjusted start date.
     *
     * Built on first use, which under the indexed strategy is the first time the date of a step
     * matches no unadjusted boundary: the adjusted pass is the second of the two and is not made
     * at all where the first one matched, so a definition whose steps all name unadjusted
     * boundaries never builds this map.
     */
    private lazy val adjustedStartIndices: Map[LocalDate, Int] =
      firstHitIndices(period => period.startDate)

    /**
     * The running maximum of the unadjusted start dates of the periods after the first, one entry
     * per such period, in schedule order.
     *
     * Built on first use, which under the indexed strategy is the first time a step has to be
     * placed in the period its date falls inside rather than on a boundary. The seed is the start
     * date of the second period, since the question the maxima answer excludes the first.
     */
    private lazy val laterStartMaxima: Vector[LocalDate] =
      periods match {
        case _ :: firstLaterPeriod :: remainingPeriods =>
          remainingPeriods.iterator
            .map(period => period.unadjustedStartDate)
            .scanLeft(firstLaterPeriod.unadjustedStartDate)((runningMaximum, date) =>
              if (date.isAfter(runningMaximum)) date else runningMaximum)
            .toVector
        case _ => Vector.empty[LocalDate]
      }

    /**
     * The unadjusted start date of the period at the specified index.
     *
     * Answers with nothing for an index the schedule does not have, so a caller naming a period
     * it worked out for itself cannot turn a mistake into a raised error. The message reporting
     * two steps that collided in one period is the caller this is for: it names the date the
     * period starts on, and the index it names it from came from resolving a step against this
     * very index.
     *
     * The date is reached by walking the period list to that index. This is the one question of
     * the four that no strategy indexes, because it is asked only while a failure is being
     * reported: a walk on that path costs a resolution that is already over, where a vector of
     * every start date would cost every resolution that succeeds.
     *
     * @param index  the zero-based index of the period
     * @return the unadjusted start date of that period, or nothing if the schedule has no such
     *   period
     */
    def unadjustedStartDateAt(index: Int): Option[LocalDate] =
      if (index < 0) None else unadjustedStartDateFrom(periods, index)

    /**
     * The index of the first period whose '''unadjusted''' start date is the specified date.
     *
     * @param date  the date to look up
     * @return the index of the first such period, or nothing if no period starts on that date
     */
    def unadjustedStartIndexOf(date: LocalDate): Option[Int] =
      if (indexedLookups) {
        unadjustedStartIndices.get(date)
      } else {
        firstStartIndexOf(periods, 0, date, period => period.unadjustedStartDate)
      }

    /**
     * The index of the first period whose '''adjusted''' start date is the specified date.
     *
     * @param date  the date to look up
     * @return the index of the first such period, or nothing if no period starts on that date
     */
    def adjustedStartIndexOf(date: LocalDate): Option[Int] =
      if (indexedLookups) {
        adjustedStartIndices.get(date)
      } else {
        firstStartIndexOf(periods, 0, date, period => period.startDate)
      }

    /**
     * The index of the period before the first period, after the first one, that starts after the
     * specified date.
     *
     * This is the middle rule of [[ValueStep.findPreviousIndex]]. Under the indexed strategy it is
     * answered by a binary search over the prefix maxima of the start dates rather than by a walk
     * over the dates themselves - see the documentation of this type for why the maxima are the
     * searchable form of the question and the dates are not, and in particular why the answer does
     * not require the periods of the schedule to be in date order. Under the scan strategy it is
     * answered by that walk, which stops at the first period that starts later. Both forms answer
     * the index found '''minus one''', which is the position of the maximum in a vector that
     * starts at period one and so needs no subtraction of its own.
     *
     * @param date  the date of the step being resolved
     * @return the index of the period before the first later-starting one, or nothing if no
     *   period after the first starts after that date
     */
    def indexBeforeFirstLaterStart(date: LocalDate): Option[Int] =
      if (indexedLookups) {
        val position = firstLaterStart(date, 0, laterStartMaxima.size)
        if (position < laterStartMaxima.size) Some(position) else None
      } else {
        periods match {
          case _ :: laterPeriods => firstLaterStartPosition(laterPeriods, 0, date)
          case Nil => None
        }
      }

    /**
     * Maps each start date of the periods to the '''first''' position it appears at.
     *
     * The pairs are accumulated by prepending, so the list handed to the map factory holds them in
     * reverse schedule order, and the factory keeps the last pair it is offered for a repeated
     * key: the surviving position of a date that several periods start on is therefore the
     * earliest of them, which is the period a walk in schedule order matches. The cost is one pair
     * and one entry per period, paid once, where entering each date into a map derived from the
     * previous one would cost a map per period.
     *
     * @param startDateOf  the start date of a period to index it by, unadjusted or adjusted
     * @return the first position of each distinct start date
     */
    private def firstHitIndices(startDateOf: SchedulePeriod => LocalDate): Map[LocalDate, Int] =
      Map.from(reversedStartEntries(periods, 0, startDateOf, Nil))

    /**
     * Pairs each period with its index, in reverse schedule order.
     *
     * The walk is forward and the pairs are prepended, which is what reverses them; the recursion
     * is in tail position and so is compiled to a loop.
     *
     * @param remaining  the periods still to pair, in schedule order
     * @param index  the index of the first of those periods
     * @param startDateOf  the start date of a period to pair it by
     * @param entries  the pairs made from the periods already walked, latest first
     * @return every period paired with its index, latest first
     */
    @tailrec
    private def reversedStartEntries(
        remaining: List[SchedulePeriod],
        index: Int,
        startDateOf: SchedulePeriod => LocalDate,
        entries: List[(LocalDate, Int)]): List[(LocalDate, Int)] =
      remaining match {
        case Nil => entries
        case period :: laterPeriods =>
          reversedStartEntries(
            laterPeriods,
            index + 1,
            startDateOf,
            (startDateOf(period), index) :: entries)
      }

    /**
     * Walks the periods for the unadjusted start date of the one at the specified offset.
     *
     * Answers with nothing where the offset runs past the end of the list, which is how an index
     * the schedule has no period for is answered without a bound being checked against the size.
     *
     * @param remaining  the periods still to walk, in schedule order
     * @param countdown  the number of periods still to pass before the one wanted
     * @return the unadjusted start date of that period, or nothing if the list is shorter
     */
    @tailrec
    private def unadjustedStartDateFrom(
        remaining: List[SchedulePeriod],
        countdown: Int): Option[LocalDate] =
      remaining match {
        case Nil => None
        case period :: laterPeriods =>
          if (countdown == 0) {
            Some(period.unadjustedStartDate)
          } else {
            unadjustedStartDateFrom(laterPeriods, countdown - 1)
          }
      }

    /**
     * Walks the periods in schedule order for the first one starting on the specified date.
     *
     * This is the scan strategy's form of both boundary-matching questions, the two differing only
     * in which start date of a period they read. It allocates nothing per period walked and stops
     * at the first hit, which is what makes it agree with the first-hit maps on a list holding
     * several periods that start on one date. The recursion is in tail position and so is compiled
     * to a loop.
     *
     * @param remaining  the periods still to walk, in schedule order
     * @param index  the index of the first of those periods
     * @param date  the date to match
     * @param startDateOf  the start date of a period to match against, unadjusted or adjusted
     * @return the index of the first period starting on that date, or nothing if none does
     */
    @tailrec
    private def firstStartIndexOf(
        remaining: List[SchedulePeriod],
        index: Int,
        date: LocalDate,
        startDateOf: SchedulePeriod => LocalDate): Option[Int] =
      remaining match {
        case Nil => None
        case period :: laterPeriods =>
          if (startDateOf(period) == date) {
            Some(index)
          } else {
            firstStartIndexOf(laterPeriods, index + 1, date, startDateOf)
          }
      }

    /**
     * Walks the periods after the first for the first one that starts after the specified date.
     *
     * This is the scan strategy's form of the preceding-period question, and it answers the
     * position of that period in the list of periods after the first - which is its index in the
     * schedule minus one, the same number the search over the prefix maxima answers. The
     * recursion is in tail position and so is compiled to a loop.
     *
     * @param remaining  the periods after the first that are still to walk, in schedule order
     * @param position  the position of the first of those periods among the periods after the
     *   first
     * @param date  the date of the step being resolved
     * @return the position of the first later-starting period, or nothing if none starts later
     */
    @tailrec
    private def firstLaterStartPosition(
        remaining: List[SchedulePeriod],
        position: Int,
        date: LocalDate): Option[Int] =
      remaining match {
        case Nil => None
        case period :: laterPeriods =>
          if (period.unadjustedStartDate.isAfter(date)) {
            Some(position)
          } else {
            firstLaterStartPosition(laterPeriods, position + 1, date)
          }
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
   * Companion of [[PeriodIndex]], holding the factories that build one and the threshold that
   * decides how it answers.
   *
   * The constructor of the type is private, so these are the only way an index comes about, and
   * the eagerly derived parts of one are therefore always read from the same period list in the
   * same place.
   */
  private[value] object PeriodIndex {

    /**
     * The number of date-positioned steps at which the indexed strategy becomes the cheaper one.
     *
     * Below this count an index scans its period list for each date it is asked about, allocating
     * nothing; at it or above, it builds the structures that turn those scans into lookups.
     *
     * The constant is a measurement rather than a guess. Resolving a definition against a
     * 1000-period monthly schedule, with the two strategies forced in turn and the step count
     * swept, the indexed strategy costs 82-99 microseconds almost flat in the step count up to 64 -
     * the one build dominates it - while the scanning strategy costs what its walks cost, which
     * depends on '''where''' in the schedule the steps sit as well as on how many of them there
     * are: a scan stops at the period it matches, so steps naming the first periods of the schedule
     * are found almost immediately and steps naming the last ones walk nearly the whole list. With
     * the steps at the end of the schedule - the unfavourable placement, and the one that a date
     * matching no boundary at all shares - scanning costs 17 microseconds at one step and 48, 86
     * and 150 at 16, 32 and 64 steps, crossing the indexed cost between 32 and 48. With the steps
     * at the start of the schedule scanning stays under 20 microseconds out to 64 steps and does
     * not cross until around 256.
     *
     * The crossing of the unfavourable placement is the one this constant is set to, because being
     * on the wrong side of that one is what costs an order of magnitude: at 256 tail-placed steps a
     * scan costs 928 microseconds against the index's 132, where at 256 head-placed steps the index
     * costs 113 microseconds against the scan's 106. The position of the crossing is insensitive to
     * the length of the schedule, since the cost of one scan and the cost of the whole build are
     * both proportional to it, which is why one constant serves rather than a function of both
     * counts.
     *
     * Nothing about an answer depends on this number - the two strategies are checked against one
     * another, and against an independent linear reference, on both sides of it.
     */
    private[value] val IndexedLookupThreshold: Int = 32

    /**
     * Builds the index over the specified periods for a single date-positioned step.
     *
     * This is the form for a caller resolving '''one''' step, which is what the two members of
     * [[ValueStep]] that take a period list are: one step is below the threshold, so the index it
     * gets answers by scanning and builds nothing.
     *
     * @param periods  the periods of the schedule to index, in schedule order
     * @return the index over those periods
     */
    def of(periods: NonEmptyList[SchedulePeriod]): PeriodIndex = of(periods, 1)

    /**
     * Builds the index over the specified periods for the specified number of date-positioned
     * steps.
     *
     * Only the three parts that cost nothing are derived here, each by a traversal that allocates
     * nothing: the number of periods, the unadjusted start date of the first period, and the
     * unadjusted end date of the last. The structures that answer the searching questions are
     * built by the index itself, on first use and only where the strategy this count selects is
     * the indexed one.
     *
     * The count is of the steps positioned by a '''date''', because they are the only ones that
     * ask a searching question at all: a step positioned by a period index is answered from the
     * number of periods alone. A caller that does not know the count - which means a caller
     * placing one step - uses the single-argument form above.
     *
     * @param periods  the periods of the schedule to index, in schedule order
     * @param dateStepCount  the number of date-positioned steps to be placed against this index
     * @return the index over those periods
     */
    def of(periods: NonEmptyList[SchedulePeriod], dateStepCount: Int): PeriodIndex = {
      val periodList = periods.toList
      new PeriodIndex(
        periodList,
        periodList.length,
        periods.head.unadjustedStartDate,
        lastOf(periods.head, periods.tail).unadjustedEndDate,
        dateStepCount >= IndexedLookupThreshold)
    }

    /**
     * The last of the specified periods.
     *
     * Written as a walk taking the head and the tail separately so that it is total on a list of
     * any length, including the empty tail of a schedule of one period, and so that it allocates
     * nothing while walking. The recursion is in tail position and so is compiled to a loop.
     *
     * @param period  the period reached so far
     * @param remaining  the periods after it, in schedule order
     * @return the last period
     */
    @tailrec
    private def lastOf(
        period: SchedulePeriod,
        remaining: List[SchedulePeriod]): SchedulePeriod =
      remaining match {
        case Nil => period
        case laterPeriod :: laterPeriods => lastOf(laterPeriod, laterPeriods)
      }
  }

  /**
   * Obtains an instance that applies at the specified schedule period index.
   *
   * This factory defines the date that the step occurs in relative terms. The date is
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
   * @return the varying step, or the failure naming the bound the index breaks: it must be one or
   *   greater
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
   * Construction is total: a date is a date, and whether it lines up with a boundary of some
   * schedule is a question about that schedule rather than about this step, decided where the
   * step is resolved. So no outcome is reported here and callers building a run of steps - a
   * [[ValueStepSequence]] walking its dates is the one inside this library - need no error
   * handling around the construction of each one.
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
   * external shape is another. It applies both checks of the type, the one-position invariant and
   * the range of the index:
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
   * above: one call tells a caller everything it has to correct. The range of the index is
   * checked only where an index is present, because an index that is absent has no range to be
   * wrong about, and that check simply passes.
   *
   * @param periodIndex  the index of the period of the value change, if the step is positioned in
   *   relative terms
   * @param date  the start date of the value change, if the step is positioned in absolute terms
   * @param value  the adjustment to make to the value
   * @return the varying step, or the failures naming the conditions that are broken: exactly one
   *   of the period index and the date must be supplied, so a pair supplying both or neither is
   *   refused, and a period index that is supplied must be one or greater
   */
  def of(
      periodIndex: Option[Int],
      date: Option[LocalDate],
      value: ValueAdjustment): ResultNec[ValueStep] =
    (checkedPosition(periodIndex, date), checkedOptionalPeriodIndex(periodIndex))
      .mapN((_, _) => create(periodIndex, date, value))
      .toEither

  /**
   * Creates a value, which every route into the type funnels through.
   *
   * This is the only instantiation of the type and it is private, so the three factories above
   * are the whole of its construction. The type is an abstract case class with a private
   * constructor, so it has neither a public `apply` nor a `copy`, and this builds [[Impl]], the
   * subclass declared and hidden here - the shape that keeps those two synthesised members from
   * existing while `unapply` and pattern matching still do.
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
    new Impl(periodIndex, date, value)

  /**
   * The one implementation of a value step.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it. It is declared rather than written as an anonymous subclass at the instantiation site
   * for two reasons, both about what the class file says: a private member class is one a Java
   * compiler refuses to name, where an anonymous class is public and can be instantiated directly
   * by a caller in another language, and a named class can be compared against, which is what
   * lets [[ValueStep]] refuse in its own constructor to be any other implementation.
   *
   * @param periodIndex  the checked period index, if the step is positioned in relative terms
   * @param date  the date, if the step is positioned in absolute terms
   * @param value  the adjustment to make to the value
   */
  private final class Impl(
      periodIndex: Option[Int],
      date: Option[LocalDate],
      value: ValueAdjustment)
      extends ValueStep(periodIndex, date, value)

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

  /**
   * The hashing and equality of steps.
   *
   * Taken from the `equals` and `hashCode` of the type, which are those synthesised for its three
   * properties and so are those of [[ValueAdjustment]] where its double is concerned - compared
   * by bit pattern. This is the type's only equality-bearing instance, and `Eq[ValueStep]` is
   * obtained from it by subtyping.
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

  /**
   * The raw field shape of a step, from which both halves of the codec below are derived.
   *
   * A validated type needs this intermediate product because derivation reads the public
   * constructor of a product and this type has none - it is an abstract case class whose
   * constructor is private - so there is no public shape to derive from. Writing the three fields
   * out by hand instead would state the same contract a second time.
   *
   * The shape is `java.io.Serializable`, because the compiler makes every `case class` so, and it
   * therefore mixes in [[NoJavaSerialization]] as every product of this port does: these fields
   * reach the library as JSON through the codecs below and in no other form.
   *
   * @param periodIndex  the period index, if the step is positioned in relative terms
   * @param date  the date, carried as its ISO date string, if positioned in absolute terms
   * @param value  the adjustment, carried as the object its own codec writes
   */
  private final case class Raw(
      periodIndex: Option[Int],
      date: Option[LocalDate],
      value: ValueAdjustment)
      extends NoJavaSerialization

  private val rawEncoder: Encoder[Raw] = deriveEncoder[Raw]

  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /**
   * The JSON encoding of steps.
   *
   * A value is an object holding the position it actually has and its adjustment, under the names
   * of the three properties and in declaration order:
   *
   * {{{
   * {"periodIndex":2,"value":{"modifyingValue":-2000.0,"type":"DeltaAmount"}}
   * {"date":"2014-06-30","value":{"modifyingValue":-2000.0,"type":"DeltaAmount"}}
   * }}}
   *
   * The position that is not held is dropped from the document rather than written as an
   * explicitly empty field; the derived decoder reads an absent field as holding nothing, so the
   * round trip is exact either way. The adjustment is written by its own codec, the date as its
   * ISO form, and the index as a JSON number.
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
