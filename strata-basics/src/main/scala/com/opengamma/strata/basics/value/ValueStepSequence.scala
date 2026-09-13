/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.value

import java.time.DateTimeException
import java.time.LocalDate

import scala.annotation.tailrec

import cats.Hash
import cats.Order
import cats.Show
import cats.syntax.apply._

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.basics.schedule.Frequency
import com.opengamma.strata.basics.schedule.RollConvention
import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * A sequence of steps that vary a value over time.
 *
 * A financial value, such as the notional or interest rate, may vary over time. This class
 * represents a sequence of changes in the value within [[ValueSchedule]].
 *
 * The sequence is defined by a start date, end date and frequency. The adjustment at each step is
 * defined using [[ValueAdjustment]], and it is the '''same''' adjustment at every step: a sequence
 * expresses a repeated change of one shape - a notional reducing by the same amount each year, say
 * - and a run of differing changes is expressed as individual [[ValueStep]] values instead.
 *
 * ===What a sequence stands for===
 *
 * A sequence is a rule rather than a list: it holds two dates and a frequency, and the dates the
 * rule names are worked out only when it is resolved against the roll convention of the schedule
 * it applies to. [[resolve]] performs that expansion, walking the frequency from the first date to
 * the last and pairing each date it lands on with the adjustment. It is where three conditions
 * are checked: the walk has to land exactly on the adjusted last date, it may accept at most
 * [[ValueStepSequence.MaximumStepCount]] dates, and every date it computes has to lie inside the
 * range `java.time` represents. All three depend on the data of the sequence and the convention
 * rather than on any caller contract, so a breach of any of them is reported as a failure value;
 * nothing about a resolution is raised.
 *
 * ===Construction===
 *
 * Construction is validating, so the primary constructor is private and there is neither an
 * `apply` nor a `copy`: [[ValueStepSequence.of]] is the only way to obtain a sequence, and every
 * value that exists therefore has its first date on or before its last and an adjustment whose
 * type is not `Replace`. The two conditions are independent and are reported together when both
 * fail.
 *
 * ===Equality===
 *
 * Equality and hashing are those of the four fields, and so are those of [[ValueAdjustment]]
 * where the modifying value is concerned: an adjustment compares its double by bit pattern rather
 * than by numeric comparison. A sequence carries no double of its own, so it takes that
 * behaviour whole rather than restating it.
 *
 * ===Thread safety===
 *
 * An instance is immutable and holds only immutable values, so it is safe to share between any
 * number of threads without synchronisation.
 *
 * @param firstStepDate  the first date in the sequence. This sequence will change the value
 *   '''on''' this date, but not before. This must be one of the '''unadjusted''' dates in the
 *   schedule period schedule. For example, consider a 5 year swap from 2012-02-01 to 2017-02-01
 *   with 6 month frequency: the date '2013-02-01' is an unadjusted schedule period boundary, and
 *   so may be specified here
 * @param lastStepDate  the last date in the sequence. This sequence will change the value '''on'''
 *   this date, but not after. This must be one of the '''unadjusted''' dates in the schedule
 *   period schedule. In the swap above, the date '2015-02-01' is an unadjusted schedule period
 *   boundary, and so may be specified here
 * @param frequency  the frequency of the sequence. This sequence will change the value on each
 *   date between the start and end defined by this frequency. The frequency is interpreted
 *   relative to the frequency of a `Schedule`, and it must be equal to or greater than the
 *   frequency of that schedule - a sequence cannot change the value more often than the schedule
 *   has periods to change it in
 * @param adjustment  the adjustment representing the change that occurs at each step. The
 *   adjustment type must not be `Replace`: an adjustment of that type yields its modifying value
 *   whatever the base value is, so repeating one produces the same value at every step and
 *   expresses nothing a single [[ValueStep]] does not already express
 */
sealed abstract case class ValueStepSequence private (
    firstStepDate: LocalDate,
    lastStepDate: LocalDate,
    frequency: Frequency,
    adjustment: ValueAdjustment)
    extends NoJavaSerialization {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means - which
  // could hold dates out of order or a `Replace` adjustment, the two checks `of` accumulates -
  // can be stopped is here. The single implementation is the companion's hidden `Impl`.
  JvmClosure.requireSoleImplementation(this, classOf[ValueStepSequence.Impl])

  // The invariant of this type, stated over the fields the instance actually holds rather than
  // over the arguments a factory was given, because the class file of the implementation carries a
  // public constructor whatever the source asked for: a caller compiled outside this library can
  // name that constructor directly, and the identity check above would admit a sequence whose
  // dates run backwards or whose adjustment replaces the value. These are the two checks
  // [[ValueStepSequence.of]] accumulates, and [[ValueStepSequence.resolve]] depends on both: it
  // walks the frequency forward from the first date to the last, which terminates because the
  // first is not after the last, and it applies the adjustment at each step, which says something
  // only where the adjustment modifies the previous value rather than discarding it.
  //
  // Nothing about the frequency is stated, exactly as the factory states nothing about it: a
  // frequency is positive and canonical by its own invariant, and whether it divides the span
  // between the two dates depends on the roll convention of the schedule the sequence is applied
  // to, which [[ValueStepSequence.resolve]] decides.
  JvmClosure.requireInvariant(
    "its first step date falls on or before its last step date",
    !firstStepDate.isAfter(lastStepDate))
  JvmClosure.requireInvariant(
    "its adjustment is not of type 'Replace'",
    adjustment.`type` != ValueAdjustmentType.Replace)

  /**
   * Resolves this sequence to a list of steps, appending them to the steps already held.
   *
   * The dates of the steps are worked out by walking the frequency of this sequence from its
   * first date to its last, under the roll convention supplied: both dates are adjusted by the
   * convention, and each subsequent date is the one the convention reaches from the date before
   * it. That is why the convention is applied to '''every''' generated date rather than only to
   * the two ends - an annual sequence under the IMM convention lands on the third Wednesday of
   * each September, not on the anniversary of the first one.
   *
   * The walk recurses over the `next` operation of the convention from the adjusted first date,
   * in the tail-recursive [[ValueStepSequence.rolledDates]]. `next` always returns a date strictly
   * after the one handed to it, so the walk always terminates and the dates it accepts are
   * exactly those from the adjusted first date up to the adjusted last one.
   *
   * That property is also why the walk '''stops at''' the adjusted last date rather than stepping
   * past it: a date equal to the adjusted last date is the end of the expansion, and because
   * `next` only ever moves forward, the successor of that date could only be after it and could
   * only be discarded. Asking for it is therefore pure waste - and, one frequency short of
   * `LocalDate.MAX`, waste that fails, which is what the guard below is about.
   *
   * Terminating is not the same as being small, so the walk is '''bounded''' at
   * [[ValueStepSequence.MaximumStepCount]] steps, and a sequence describing more than that is
   * reported rather than expanded. The bound is applied to the walk itself rather than to the
   * list it produces, so a daily frequency over a span of centuries - which the dates and
   * frequency of a sequence are free to describe, since neither is checked against any schedule
   * at construction - allocates one date beyond the ceiling instead of however many its span
   * implies. See the ceiling for why the count is where it is.
   *
   * Every piece of date arithmetic the resolution performs is '''guarded''': both endpoint
   * adjustments and every rolling step go through [[ValueStepSequence.guardedDate]], so the two
   * exceptions `java.time` raises at the edges of the date range become the failure value this
   * member already answers with. There are two ways to reach that edge, and both are legal
   * arguments - the two dates of a sequence are checked against each other and against nothing
   * else, so `LocalDate.MAX` is as valid a last date as any other. A rolling step from a date
   * within one frequency of the end of the range leaves it; and an endpoint adjustment can leave
   * it too, because a day-of-week convention moves a date '''forward''' to the next matching day,
   * which from the last few days of the range is off the end of it. Both are reported as failure
   * values, so a resolution reached through the public [[ValueSchedule.resolveValues]] answers
   * with a value in every case its signature promises one.
   *
   * The last date the walk lands on has to '''be''' the adjusted last date of this sequence. Where
   * it is not, the frequency does not divide the span of the sequence - a twelve month frequency
   * over six months, say - and that is reported rather than resolved. The report names the
   * frequency, the convention and the two dates that disagree.
   *
   * The generated steps are appended '''after''' the steps supplied and are in date order, so a
   * caller resolving several sequences into one list builds it up by threading the list through
   * each call.
   *
   * {{{
   * val sequence = ValueStepSequence.of(date1, date2, Frequency.P3M, adjustment)  // Right(...)
   * sequence.flatMap(_.resolve(existingSteps, RollConventions.NONE))              // Right(steps)
   * }}}
   *
   * This is visible within this package rather than publicly: it is the operation
   * [[ValueSchedule]] performs while resolving its own steps, and a caller outside reaches it
   * through that.
   *
   * @param existingSteps  the existing list of steps, which the generated steps are appended to
   * @param rollConv  the roll convention of the schedule this sequence applies to
   * @return the steps supplied followed by the generated steps, or the failure naming the broken
   *   condition: walking the frequency of this sequence under the convention has to land exactly
   *   on the adjusted last date, the walk may accept at most
   *   [[ValueStepSequence.MaximumStepCount]] dates, and every date it computes has to lie inside
   *   the range `java.time` represents
   */
  private[value] def resolve(
      existingSteps: List[ValueStep],
      rollConv: RollConvention): FailureOr[List[ValueStep]] =
    for {
      start <- guardedDate(rollConv)(rollConv.adjust(firstStepDate))
      adjustedLastStepDate <- guardedDate(rollConv)(rollConv.adjust(lastStepDate))
      dates <- rolledDates(start, start, adjustedLastStepDate, rollConv, List.empty, 0)
      // the last date reached, or the adjusted first date where the walk reached nothing at all
      prev = dates.lastOption.getOrElse(start)
      generated <-
        if (prev == adjustedLastStepDate) {
          Right(dates.map(date => ValueStep.of(date, adjustment)))
        } else {
          Left(
            Failure.Invalid(
              ValueStepSequence
                .frequencyMismatch(frequency, rollConv, adjustedLastStepDate, prev)))
        }
    } yield existingSteps ++ generated

  /**
   * Walks one expansion, accepting the dates of the steps and refusing an oversized one.
   *
   * This is the whole of the walk [[resolve]] performs, written as a tail recursion over the
   * dates rather than as a loop over a moving cursor, and the recursion is what carries the three
   * things a step of the walk decides between:
   *
   *  - '''the walk has finished.''' A date after the adjusted last date is not part of the
   *    expansion, so the dates accepted up to that point are the answer. This is the branch a
   *    sequence whose frequency does not divide its span ends on, and the one an adjusted first
   *    date already past the adjusted last date ends on immediately, having accepted nothing;
   *  - '''the walk has reached the end exactly.''' A date equal to the adjusted last date is the
   *    final step of the expansion, so it is accepted and the walk stops '''without''' asking the
   *    convention for a successor. The list is the same one a walk that stepped past the end
   *    would have produced, because `next` always returns a date strictly after its argument and
   *    that successor could only have been discarded - and not asking is what keeps a sequence
   *    ending at the last representable date from failing on arithmetic whose result nothing
   *    would have read;
   *  - '''the walk continues.''' The next boundary is the convention's, computed through the
   *    guard, and the recursion carries it with the date just accepted.
   *
   * The ceiling is applied '''between''' the first branch and the rest, so acceptance stops at
   * [[ValueStepSequence.MaximumStepCount]] dates and the date that would be the one after that is
   * refused. One date beyond the ceiling is therefore the whole of the excess a refused expansion
   * computes or holds, however wide the span and however short the frequency; see the ceiling for
   * why the count is where it is.
   *
   * The accepted dates are accumulated in reverse and reversed once at each exit, so the walk
   * costs one cons cell per date rather than the quadratic copying an append-to-the-end
   * accumulation would cost over the hundred thousand dates the ceiling allows.
   *
   * @param current  the date the walk has reached, which is a step of the expansion unless it is
   *   past the adjusted last date
   * @param start  the first date of the sequence, adjusted by the convention, carried only so
   *   that a refusal can name the span it refused
   * @param adjustedLastStepDate  the last date of the sequence, adjusted by the convention
   * @param rollConv  the roll convention the walk rolls with
   * @param accepted  the dates accepted up to this point, most recent first
   * @param count  how many dates have been accepted, which is the length of `accepted`
   * @return the dates of the expansion in schedule order, or the failure naming the broken
   *   condition: at most [[ValueStepSequence.MaximumStepCount]] dates may be accepted, and every
   *   rolling step has to stay inside the range `java.time` represents
   */
  @tailrec
  private def rolledDates(
      current: LocalDate,
      start: LocalDate,
      adjustedLastStepDate: LocalDate,
      rollConv: RollConvention,
      accepted: List[LocalDate],
      count: Int): FailureOr[List[LocalDate]] =
    if (current.isAfter(adjustedLastStepDate)) {
      Right(accepted.reverse)
    } else if (count >= ValueStepSequence.MaximumStepCount) {
      Left(
        Failure.Invalid(
          ValueStepSequence
            .expansionBeyondCeiling(frequency, rollConv, start, adjustedLastStepDate)))
    } else if (current == adjustedLastStepDate) {
      Right((current :: accepted).reverse)
    } else {
      guardedDate(rollConv)(rollConv.next(current, frequency)) match {
        case Right(rolled) =>
          rolledDates(
            rolled,
            start,
            adjustedLastStepDate,
            rollConv,
            current :: accepted,
            count + 1)
        case Left(overflow) => Left(overflow)
      }
    }

  /**
   * Evaluates one piece of date arithmetic, reporting an overflow instead of raising it.
   *
   * The argument is taken by name and evaluated once, here, so that the two exceptions the
   * `java.time` arithmetic of an adjustment or a roll can raise at the edges of the supported
   * date range become the failure value [[resolve]] answers with everywhere else. Nothing else is
   * caught: an exception of any other type is a defect rather than a property of the dates, and
   * swallowing it would hide it.
   *
   * @param rollConv  the roll convention the arithmetic is performed under, which the failure
   *   names alongside the frequency because the two of them decide the date being computed
   * @param compute  the date arithmetic to evaluate
   * @return the date the arithmetic produced, or the failure describing the overflow
   */
  private def guardedDate(rollConv: RollConvention)(compute: => LocalDate): FailureOr[LocalDate] =
    try {
      Right(compute)
    } catch {
      case _: DateTimeException | _: ArithmeticException =>
        Left(Failure.Invalid(ValueStepSequence.dateRangeOverflow(frequency, rollConv)))
    }

  /**
   * Renders this sequence as text.
   *
   * The rendering names all four fields, in declaration order, inside braces. It is one line; the
   * example below is wrapped only to fit:
   *
   * {{{
   * ValueStepSequence{firstStepDate=2016-04-20, lastStepDate=2016-10-20, frequency=P3M,
   *   adjustment=ValueAdjustment[result = input + -100.0]}
   * }}}
   *
   * @return the text form of this sequence
   */
  override def toString: String =
    s"ValueStepSequence{firstStepDate=$firstStepDate, lastStepDate=$lastStepDate, " +
      s"frequency=$frequency, adjustment=$adjustment}"
}

/**
 * Companion of [[ValueStepSequence]], holding its factory, its typeclass instances and its codec.
 *
 * The single factory is the only way to obtain a sequence from outside this file, which is what
 * makes the invariants of the type - the first date on or before the last, and an adjustment
 * whose type is not `Replace` - properties of every value that exists rather than properties a
 * caller is asked to respect.
 *
 * Two typeclass instances are published, and exactly two: a `Hash`, which is the single
 * equality-bearing instance of the type - `Hash` extends `Eq`, so declaring an `Eq` as well would
 * leave two instances that could disagree and one of them ambiguous - and a `Show`. There is
 * deliberately no `Order`: two sequences could be ranked by their first date, but doing so would
 * order values that differ in three other fields by one of them alone.
 */
object ValueStepSequence {

  /**
   * The greatest number of steps [[ValueStepSequence.resolve]] will expand a sequence into.
   *
   * A sequence is a rule rather than a list, and nothing about its two dates or its frequency is
   * checked against any schedule when it is built, because the schedule is not known until the
   * sequence is resolved. So the span a caller supplies, divided by the frequency it supplies, is
   * the only thing deciding how many steps an expansion produces, and a daily frequency over a
   * span of centuries describes millions of them. Expanding every span strictly would be
   * unbounded work and unbounded allocation driven by data (CWE-400), so this ceiling is drawn
   * and a sequence that crosses it is reported. It is checked inside the walk, by
   * [[ValueStepSequence.rolledDates]], as each date is accepted.
   *
   * The count is where it is for two reasons. A step only means something against a period
   * boundary of the schedule the sequence is resolved with, and the schedule generation of this
   * module refuses to produce more than this many periods, so a sequence expanding to more steps
   * than this could not resolve against any schedule this library builds even if it were
   * expanded - the work would be spent only to be rejected. And a hundred thousand dates paired
   * with a hundred thousand steps is a few megabytes, which bounds what a single rejected
   * expansion can cost before the rejection is reached.
   *
   * The limit is a constant rather than a parameter of `resolve`: the operation's signature is the
   * one [[ValueSchedule]] calls, and widening it to carry a budget would push the choice onto
   * every caller while giving none of them anything to base it on.
   */
  private[value] val MaximumStepCount: Int = 100000

  private val FirstStepDateField: String = "firstStepDate"

  private val LastStepDateField: String = "lastStepDate"

  /** Reported when the adjustment is of type `Replace` rather than one that varies the value. */
  private val ReplacementNotAllowed: String = "ValueAdjustmentType must not be 'Replace'"

  /**
   * The ordering of dates the order check below is performed with.
   *
   * The checking helpers used below are generic in the type being compared and take its cats
   * ordering, and cats publishes no instance for `java.time.LocalDate` - the class implements
   * `Comparable[ChronoLocalDate]` rather than `Comparable[LocalDate]`, so the ordering derived
   * from a comparable type does not apply to it either. The instance is therefore stated here, as
   * the natural time-line order the class itself defines, and kept private: it exists to serve
   * the check of this file, and publishing an ordering for a type this module does not own would
   * put an instance into implicit scope for every file that imports anything from here.
   */
  private implicit val dateOrder: Order[LocalDate] =
    Order.from((first, second) => first.compareTo(second))

  /**
   * Obtains an instance from the dates, frequency and change.
   *
   * The first date must be before the last date or equal to it, and the adjustment must not be of
   * type `Replace`:
   *
   * {{{
   * ValueStepSequence.of(date(2016, 4, 20), date(2016, 10, 20), Frequency.P3M, deltaAmount)
   * // Right(the sequence)
   * ValueStepSequence.of(date(2016, 4, 20), date(2016, 4, 19), Frequency.P3M, deltaAmount)
   * // Left(invalid order)
   * ValueStepSequence.of(date(2016, 4, 20), date(2016, 10, 20), Frequency.P3M, replace)
   * // Left(must not be 'Replace')
   * ValueStepSequence.of(date(2016, 4, 20), date(2016, 4, 19), Frequency.P3M, replace)
   * // Left(both of the two above)
   * }}}
   *
   * The two checks are '''combined rather than sequenced''', so a caller supplying two bad
   * arguments is told about both of them in one chain of reasons instead of correcting one and
   * being sent back for the other. That is the last line above.
   *
   * Nothing about the frequency is checked here. Whether it divides the span between the two
   * dates depends on the roll convention of the schedule the sequence is applied to, which is not
   * known until [[ValueStepSequence.resolve]] is reached, and whether it is coarser than that
   * schedule's own frequency is a question about the schedule.
   *
   * @param firstStepDate  the first date of the sequence
   * @param lastStepDate  the last date of the sequence
   * @param frequency  the frequency of changes
   * @param adjustment  the adjustment at each step
   * @return the varying step sequence, or the failures naming the broken conditions: the first
   *   date has to be on or before the last date, and the adjustment type must not be `Replace`
   */
  def of(
      firstStepDate: LocalDate,
      lastStepDate: LocalDate,
      frequency: Frequency,
      adjustment: ValueAdjustment): ResultNec[ValueStepSequence] =
    (
      Validate.inOrderOrEqual(firstStepDate, lastStepDate, FirstStepDateField, LastStepDateField),
      Validate.isTrue(adjustment.`type` != ValueAdjustmentType.Replace, ReplacementNotAllowed))
      .mapN((_, _) => create(firstStepDate, lastStepDate, frequency, adjustment))
      .toEither

  /**
   * Creates a value, which every route into the type funnels through.
   *
   * This is the only instantiation of the type and it is private, so the factory above is the
   * whole of its construction. The type is an abstract case class whose constructor is private, so
   * it has neither a public `apply` nor a `copy`, and this builds [[Impl]], the subclass declared
   * and hidden here, which keeps those two synthesised members from existing while `unapply` and
   * pattern matching still do.
   *
   * @param firstStepDate  the checked first date of the sequence
   * @param lastStepDate  the checked last date of the sequence
   * @param frequency  the frequency of changes
   * @param adjustment  the checked adjustment at each step
   * @return the sequence holding the four fields
   */
  private def create(
      firstStepDate: LocalDate,
      lastStepDate: LocalDate,
      frequency: Frequency,
      adjustment: ValueAdjustment): ValueStepSequence =
    new Impl(firstStepDate, lastStepDate, frequency, adjustment)

  /**
   * The one implementation of a value step sequence.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it. It is declared rather than written as an anonymous subclass at the instantiation site
   * for two reasons, both about what the class file says: a private member class is one a Java
   * compiler refuses to name, where an anonymous class is public and can be instantiated directly
   * by a caller in another language, and a named class can be compared against, which is what
   * lets [[ValueStepSequence]] refuse in its own constructor to be any other implementation.
   *
   * @param firstStepDate  the checked first date of the sequence
   * @param lastStepDate  the checked last date of the sequence
   * @param frequency  the frequency of changes
   * @param adjustment  the checked adjustment at each step
   */
  private final class Impl(
      firstStepDate: LocalDate,
      lastStepDate: LocalDate,
      frequency: Frequency,
      adjustment: ValueAdjustment)
      extends ValueStepSequence(firstStepDate, lastStepDate, frequency, adjustment)

  /**
   * Describes a sequence whose frequency does not reach its last date under a roll convention.
   *
   * The frequency and the convention are rendered by their names, which is what their own text
   * forms render, and the two dates that disagree are named alongside them.
   *
   * @param frequency  the frequency of the sequence being resolved
   * @param rollConv  the roll convention it was resolved under
   * @param adjustedLastStepDate  the last date of the sequence, adjusted by the convention
   * @param prev  the last date the walk of the frequency actually reached
   * @return the message describing the mismatch
   */
  private def frequencyMismatch(
      frequency: Frequency,
      rollConv: RollConvention,
      adjustedLastStepDate: LocalDate,
      prev: LocalDate): String =
    s"ValueStepSequence lastStepDate did not match frequency '${frequency.name}'" +
      s" using roll convention '${rollConv.name}', $adjustedLastStepDate != $prev"

  /**
   * Describes a sequence whose dates and frequency expand into more steps than the ceiling allows.
   *
   * The message names the limit as well as the sequence being expanded, because the limit is the
   * part a caller cannot see from its own arguments: the two dates and the frequency are what it
   * supplied, and the count they imply is the thing it has to be told about. The reason the limit
   * exists is set out on [[ValueStepSequence.MaximumStepCount]].
   *
   * The dates named are the ones the walk actually used, adjusted by the convention, rather than
   * the two dates the sequence holds: they are the endpoints of the expansion being refused, and
   * where a convention moves either endpoint the adjusted pair is what explains the count.
   *
   * @param frequency  the frequency of the sequence being resolved
   * @param rollConv  the roll convention it was resolved under
   * @param start  the first date of the sequence, adjusted by the convention
   * @param adjustedLastStepDate  the last date of the sequence, adjusted by the convention
   * @return the message describing the refusal
   */
  private def expansionBeyondCeiling(
      frequency: Frequency,
      rollConv: RollConvention,
      start: LocalDate,
      adjustedLastStepDate: LocalDate): String =
    s"ValueStepSequence frequency '${frequency.name}' from $start to $adjustedLastStepDate" +
      s" using roll convention '${rollConv.name}' expands to more than the maximum of" +
      s" $MaximumStepCount steps"

  /**
   * Describes a resolution whose date arithmetic leaves the range of representable dates.
   *
   * The two parts a caller can act on are named: the frequency, which is what a rolling step adds
   * to a date, and the convention, which is what adjusts the result and is itself able to move a
   * date forward past the end of the range. The two dates are not named, because either of them
   * can be the one at fault - a step overflows from the date it steps from, an endpoint
   * adjustment from the endpoint it adjusts - and naming one of them would point at the wrong one
   * half of the time; the sequence itself renders all four of its fields.
   *
   * The reason this message exists is set out on [[ValueStepSequence.guardedDate]].
   *
   * @param frequency  the frequency of the sequence being resolved
   * @param rollConv  the roll convention it was resolved under
   * @return the message describing the overflow
   */
  private def dateRangeOverflow(frequency: Frequency, rollConv: RollConvention): String =
    s"ValueStepSequence frequency '${frequency.name}' using roll convention" +
      s" '${rollConv.name}' moved outside the range of supported dates"

  /**
   * The hashing and equality of sequences.
   *
   * Taken from the `equals` and `hashCode` of the type, which are those synthesised for its four
   * fields and so are those of [[ValueAdjustment]] where its double is concerned - compared by
   * bit pattern. This is the type's only equality-bearing instance, and `Eq[ValueStepSequence]`
   * is obtained from it by subtyping.
   *
   * @return the hashing of sequences
   */
  implicit val hash: Hash[ValueStepSequence] = Hash.fromUniversalHashCode[ValueStepSequence]

  /**
   * The rendering of sequences as text.
   *
   * Renders what [[ValueStepSequence.toString]] renders, so the two ways of putting a sequence
   * into a message agree.
   *
   * @return the rendering of a sequence
   */
  implicit val show: Show[ValueStepSequence] = Show.show(_.toString)

  /**
   * The raw field shape of a sequence, from which both halves of the codec below are derived.
   *
   * A validated type needs this intermediate product because derivation reads the public
   * constructor of a product and this type has none - it is an abstract case class whose
   * constructor is private - so there is no public shape to derive from. Writing the four fields
   * out by hand instead would state the same contract a second time.
   *
   * The shape is `java.io.Serializable`, because the compiler makes every `case class` so, and it
   * therefore mixes in [[NoJavaSerialization]] as every product of this port does: these fields
   * reach the library as JSON through the codecs below and in no other form.
   *
   * @param firstStepDate  the first date, carried as its ISO date string
   * @param lastStepDate  the last date, carried as its ISO date string
   * @param frequency  the frequency, carried as the name its own codec writes
   * @param adjustment  the adjustment, carried as the object its own codec writes
   */
  private final case class Raw(
      firstStepDate: LocalDate,
      lastStepDate: LocalDate,
      frequency: Frequency,
      adjustment: ValueAdjustment)
      extends NoJavaSerialization

  private val rawEncoder: Encoder[Raw] = deriveEncoder[Raw]

  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /**
   * The JSON encoding of sequences.
   *
   * A value is an object holding its four fields, under their own names and in declaration order:
   *
   * {{{
   * {"firstStepDate":"2016-04-20","lastStepDate":"2016-10-20","frequency":"P3M",
   *  "adjustment":{"modifyingValue":-100.0,"type":"DeltaAmount"}}
   * }}}
   *
   * The dates are written in their ISO form, the frequency as the bare name its own codec writes,
   * and the adjustment as the object its own codec writes. No field of this type is optional, so
   * nothing is ever dropped from the document; the encoder is wrapped in the field-dropping
   * combinator all the same, so that the policy holds if a field ever becomes optional.
   *
   * Both halves of the codec are assembled by the same compile-time derivation over the same raw
   * shape, which is what keeps them from drifting apart, and no part of the encoding inspects a
   * class while the program runs. Two equal values encode to identical bytes: the fields are
   * written in their declaration order, and a date, a frequency and an adjustment each have one
   * form.
   *
   * @return the JSON encoding of sequences
   */
  implicit val encoder: Encoder[ValueStepSequence] =
    Codecs.dropNulls(rawEncoder.contramap[ValueStepSequence] { sequence =>
      Raw(sequence.firstStepDate, sequence.lastStepDate, sequence.frequency, sequence.adjustment)
    })

  /**
   * The JSON decoding of sequences.
   *
   * This is the inverse of the encoding above, and it decides whether the fields describe a value
   * exactly as a caller's arguments are decided: the payload is read into the raw shape and handed
   * to [[ValueStepSequence.of]], so a document whose first date is after its last, or whose
   * adjustment is of type `Replace`, is a decoding failure carrying every reason it is, rather
   * than a value this type would not have built. All four fields have to be present.
   *
   * @return the JSON decoding of sequences
   */
  implicit val decoder: Decoder[ValueStepSequence] =
    Codecs.validatedDecoder[Raw, ValueStepSequence] { raw =>
      of(raw.firstStepDate, raw.lastStepDate, raw.frequency, raw.adjustment)
    }(rawDecoder)
}
