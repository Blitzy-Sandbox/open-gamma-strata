/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.value

import cats.Hash
import cats.Show
import cats.data.NonEmptyList
import cats.syntax.foldable._

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.basics.schedule.RollConvention
import com.opengamma.strata.basics.schedule.Schedule
import com.opengamma.strata.basics.schedule.SchedulePeriod
import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * A value that can vary over time.
 *
 * This represents a single initial value and any adjustments over the lifetime of a trade.
 * Adjustments may be specified in absolute or relative terms.
 *
 * The adjustments may be specified as individual steps or as a sequence of steps. An individual
 * step is a change that occurs at a specific date, identified either by the date or the index
 * within the schedule. A sequence of steps consists of a start date, end date and frequency, with
 * the same change applying many times. All changes must occur on dates that are period boundaries
 * in the specified schedule.
 *
 * It is possible to specify both individual steps and a sequence, however this is not recommended.
 * If it is done, then the individual steps and sequence steps must resolve to different dates.
 *
 * The value is specified as a `Double` with the context adding additional meaning. If the value
 * represents an amount of money then the currency is specified separately. If the value represents
 * a rate then a 5% rate is expressed as 0.05.
 *
 * ===Construction is total; resolution is where a definition is judged===
 *
 * The constructor of this type is private and there is no `apply` or `copy`, so the factories of
 * the companion are the only way to obtain a schedule - the shape every type of this port whose
 * construction is checked uses. Here, though, '''none of those factories reports a failure''':
 * every one of them is total, exactly as every factory of the Java bean being ported was, and
 * that is a deliberate part of the contract rather than a check left out.
 *
 * The reason is that a definition can only be judged against the schedule it is resolved against,
 * and the schedule is not known until then. Two steps naming the same period index are the clear
 * case: they may agree, in which case the definition is sound, or disagree, in which case it is
 * not, and nothing about the pair decides which until the periods are in hand. A step naming a
 * date is the same story - whether the date is a period boundary is a question about the schedule.
 * The Java original therefore accepted both and reported neither, and the ported test suite
 * asserts that behaviour directly: a schedule holding two steps at index 1 with different
 * adjustments is '''built''' successfully and fails only when it is resolved. Adding a
 * construction-time check would reject values the original accepted and would contradict that
 * test, so the failable surface of this type is [[resolveValues]] alone.
 *
 * That single failable operation reports three classes of failure, all of them properties of the
 * pairing of a definition with a schedule rather than of either alone: a step positioned beyond
 * the last period of the schedule, two steps resolving to the same period with different
 * adjustments, and a step positioned on a date that is not a period boundary and that would
 * change the value if it were applied where it falls.
 *
 * ===Equality===
 *
 * Equality and hashing compare the initial value by '''bit pattern''' rather than by numeric
 * comparison, which is what the bean equality of the Java original did and what every
 * double-bearing type of this port does. Two consequences follow and are relied upon by the
 * round-trip properties of the test suite: a schedule whose initial value is not a number is
 * equal to itself, and a negative zero initial value is distinct from a positive zero one. The
 * steps and the sequence contribute their own equality, which is bit-aware in the same way
 * wherever a [[ValueAdjustment]] carries a double.
 *
 * ===Thread safety===
 *
 * An instance is immutable and holds only immutable values, so it is safe to share between any
 * number of threads without synchronisation.
 *
 * @param initialValue  the initial value, used for the lifetime of the trade unless specifically
 *   varied by one of the steps below
 * @param steps  the steps defining the change in the value, each consisting of a key locating the
 *   date of the change and the adjustment that occurs. The steps are '''not''' required to be
 *   sorted and are resolved in the order they are given
 * @param stepSequence  the sequence of steps changing the value, where a regular pattern of steps
 *   is encoded rather than written out. All step dates must be unique, thus the list of steps must
 *   not contain any date implied by this sequence
 */
sealed abstract case class ValueSchedule private (
    initialValue: Double,
    steps: List[ValueStep],
    stepSequence: Option[ValueStepSequence]) {

  //-------------------------------------------------------------------------
  /**
   * Resolves the value and adjustments against a specific schedule.
   *
   * This converts a schedule into a list of values, one for each schedule period.
   *
   * This is the single failable operation of this type, for the reason set out above: a definition
   * is only judged against the schedule it is resolved against. Three things can be wrong with the
   * pairing, and each is reported rather than raised:
   *
   *   1. a step positioned by an index at or beyond the end of the schedule, which names a period
   *      the schedule does not have;
   *   1. two steps resolving to the same period with '''different''' adjustments, which is a
   *      contradiction - two steps resolving to the same period with the same adjustment is not,
   *      and resolves as a single change, exactly as it did in the Java original;
   *   1. a step positioned on a date that is not a period boundary and whose adjustment would
   *      change the value of the period the date falls in. A step that would not change that value
   *      is '''ignored''', which is what makes a definition carrying more steps than the schedule
   *      has boundaries resolvable as long as the excess steps do nothing.
   *
   * A sequence that does not divide the span it covers under the roll convention of the schedule
   * is reported too, by the sequence itself while it is being expanded.
   *
   * {{{
   * ValueSchedule.of(200d, step).resolveValues(schedule)  // Right(DoubleArray.of(200, 300, 300))
   * }}}
   *
   * @param schedule  the schedule
   * @return the values, one for each schedule period, or the failure describing why this
   *   definition cannot be resolved against that schedule
   */
  def resolveValues(schedule: Schedule): FailureOr[DoubleArray] =
    resolveValues(schedule.periods, schedule.rollConvention)

  /**
   * Resolves the value and adjustments against the periods and roll convention of a schedule.
   *
   * Split from the step resolution below so that a definition holding no steps at all - much the
   * most common one, and the one every constant schedule is - answers with a filled array without
   * entering the resolution machinery. The Java original split the two for the same reason, to aid
   * inlining of the common path.
   *
   * @param periods  the periods of the schedule, in schedule order
   * @param rollConv  the roll convention of the schedule, which a sequence of steps is expanded
   *   under
   * @return the values, one for each schedule period, or the failure
   */
  private def resolveValues(
      periods: NonEmptyList[SchedulePeriod],
      rollConv: RollConvention): FailureOr[DoubleArray] =
    if (steps.isEmpty && stepSequence.isEmpty) {
      // handle simple case where there are no steps
      Right(DoubleArray.filled(periods.size, initialValue))
    } else {
      resolveSteps(periods, rollConv)
    }

  /**
   * Resolves the steps of this definition against the periods and roll convention of a schedule.
   *
   * The four stages are those of the Java original, in the same order, and the order is part of
   * the behaviour rather than an implementation detail:
   *
   *   1. the sequence of steps, where one is held, is expanded into steps and '''appended''' to
   *      the individual steps, so that an individual step at a period is seen before a sequence
   *      step at the same period and the failure names the pair in that order;
   *   1. every step is assigned to the period it resolves to, the ones resolving to no period
   *      being set aside;
   *   1. the value of each period is computed by applying the adjustment assigned to it, if any,
   *      to the value the previous period ended with;
   *   1. the steps set aside are required to change nothing.
   *
   * Where the original maintained an array of nullable adjustments, an accumulating list and three
   * counting loops, this threads an immutable accumulator through a fold, computes the values with
   * a running scan and checks the set-aside steps by traversal. No stage holds mutable state, and
   * the result is the same array of values the original built in place.
   *
   * @param periods  the periods of the schedule, in schedule order
   * @param rollConv  the roll convention of the schedule
   * @return the values, one for each schedule period, or the failure
   */
  private def resolveSteps(
      periods: NonEmptyList[SchedulePeriod],
      rollConv: RollConvention): FailureOr[DoubleArray] =
    for {
      resolvedSteps <- stepSequence.fold[FailureOr[List[ValueStep]]](Right(steps))(sequence =>
        sequence.resolve(steps, rollConv))
      assignment <- assignSteps(resolvedSteps, periods)
      values = periodValues(assignment.slots, periods.size)
      _ <- checkUnassignedSteps(assignment.unassignedSteps, periods, values)
    } yield DoubleArray.copyOf(values)

  /**
   * Assigns each of the specified steps to the period of the schedule it resolves to.
   *
   * The steps are '''not''' sorted and are taken in the order given, which is the order the Java
   * original took them in and the order the failure of a contradicting pair is decided by. The
   * fold stops at the first step that cannot be assigned, so the failure reported is the one the
   * original raised: the earliest in that order.
   *
   * @param resolvedSteps  the steps to assign, including any expanded from a sequence
   * @param periods  the periods of the schedule, in schedule order
   * @return the assignment of adjustments to periods together with the steps that resolved to no
   *   period, or the failure describing the first step that could not be assigned
   */
  private def assignSteps(
      resolvedSteps: List[ValueStep],
      periods: NonEmptyList[SchedulePeriod]): FailureOr[ValueSchedule.StepAssignment] =
    resolvedSteps.foldM[FailureOr, ValueSchedule.StepAssignment](
      ValueSchedule.StepAssignment.Empty)((assignment, step) => assignStep(assignment, step, periods))

  /**
   * Assigns a single step to the period of the schedule it resolves to.
   *
   * A step resolving to no period is set aside for the no-change check of
   * [[checkUnassignedSteps]] rather than reported here, because a step that changes nothing is
   * legal wherever it falls.
   *
   * A step resolving to a period that already holds an adjustment is a contradiction '''only''' if
   * the two adjustments differ. Where they are equal the assignment is left as it stands, which is
   * what the original achieved by overwriting the slot with an equal value; the comparison is the
   * equality of [[ValueAdjustment]], which compares its double by bit pattern just as the bean
   * equality of the original did.
   *
   * @param assignment  the assignment built from the steps seen so far
   * @param step  the step to assign
   * @param periods  the periods of the schedule, in schedule order
   * @return the assignment including this step, or the failure describing why this step cannot be
   *   assigned
   */
  private def assignStep(
      assignment: ValueSchedule.StepAssignment,
      step: ValueStep,
      periods: NonEmptyList[SchedulePeriod]): FailureOr[ValueSchedule.StepAssignment] =
    step.findIndex(periods).flatMap {
      case None => Right(assignment.withUnassignedStep(step))
      case Some(index) =>
        assignment.slots.get(index) match {
          case Some(assigned) if assigned != step.value =>
            Left(failure(ValueSchedule.duplicateStepMessage(periods, index, this)))
          case _ =>
            Right(assignment.withSlot(index, step.value))
        }
    }

  /**
   * Computes the value of every period of the schedule from the adjustments assigned to them.
   *
   * The value of a period is the value of the period before it with the adjustment assigned to
   * this period applied, if one is, and the value of the first period is the initial value of this
   * definition adjusted in the same way - which is why a step at index zero is meaningful even
   * though no period precedes it. That is a running scan over the period indices with the seed
   * dropped: the original wrote the same recurrence as an assignment to a mutable running value
   * followed by a store into the result array.
   *
   * @param slots  the adjustments assigned to periods, keyed by period index
   * @param size  the number of periods in the schedule
   * @return the value of each period, in schedule order, one entry per period
   */
  private def periodValues(slots: Map[Int, ValueAdjustment], size: Int): Vector[Double] =
    (0 until size)
      .scanLeft(initialValue)((value, index) => slots.get(index).fold(value)(_.adjust(value)))
      .tail
      .toVector

  /**
   * Checks that every step that resolved to no period of the schedule changes nothing.
   *
   * A step whose date is not a period boundary is not rejected out of hand: the Java original
   * allowed it as long as applying its adjustment to the value of the period the date falls in
   * left that value alone, which is how a definition carrying a step that merely restates the
   * value in force - or adds zero, or multiplies by one - resolves rather than failing. The steps
   * are checked in the order they were given, so the failure reported is the earliest of them.
   *
   * @param unassignedSteps  the steps that resolved to no period, in the order they were given
   * @param periods  the periods of the schedule, in schedule order
   * @param values  the value of each period, in schedule order
   * @return nothing where every such step changes nothing, or the failure describing the first
   *   that does not
   */
  private def checkUnassignedSteps(
      unassignedSteps: List[ValueStep],
      periods: NonEmptyList[SchedulePeriod],
      values: Vector[Double]): FailureOr[Unit] =
    unassignedSteps.traverse_(step => checkUnassignedStep(step, periods, values))

  /**
   * Checks that a single step that resolved to no period of the schedule changes nothing.
   *
   * The date of the step selects the period it falls in, and the value that period was computed to
   * hold is the value the adjustment of the step is applied to. A date outside the schedule
   * altogether is reported by the step itself.
   *
   * @param step  the step to check
   * @param periods  the periods of the schedule, in schedule order
   * @param values  the value of each period, in schedule order
   * @return nothing where this step changes nothing, or the failure describing the change it makes
   */
  private def checkUnassignedStep(
      step: ValueStep,
      periods: NonEmptyList[SchedulePeriod],
      values: Vector[Double]): FailureOr[Unit] =
    step.findPreviousIndex(periods).flatMap { index =>
      values
        .lift(index)
        .toRight(failure(ValueSchedule.PeriodValueMissing))
        .flatMap { baseValue =>
          // the primitive comparison of the Java original, deliberately NOT the bit comparison
          // that the equality of this type uses: the two disagree exactly where IEEE-754 does, so
          // a step producing a value that is not a number counts as a change even from a base
          // value that is not a number, and a step turning a positive zero into a negative zero
          // counts as no change. Keeping the comparison the original used keeps every definition
          // it accepted acceptable here
          if (step.value.adjust(baseValue) != baseValue) {
            Left(failure(ValueSchedule.boundaryMismatchMessage(step)))
          } else {
            Right(())
          }
        }
    }

  /**
   * Builds a failure reporting the specified message against this definition.
   *
   * Every failure this type raises of its own is invalid-shaped and carries the whole definition
   * that was rejected as an attribute, which is the convention every schedule-shaped failure of
   * this port follows: the message names what is wrong and the attribute lets a report name the
   * definition it was wrong in without that definition having to be parsed back out of the
   * message. A failure that reaches here from a step or a sequence is passed on unchanged, since
   * it already describes its own subject.
   *
   * @param message  the message describing what is wrong
   * @return the failure
   */
  private def failure(message: String): Failure =
    Failure.Invalid(message).withAttribute(ValueSchedule.DefinitionAttribute, toString)


  //-------------------------------------------------------------------------
  /**
   * Returns a copy of this schedule with the steps replaced.
   *
   * This is one of the two operations that replace the builder of the bean being ported, for a
   * caller that holds a schedule and wants the same initial value and sequence with different
   * steps. It is not a `copy`: this type has none, and the two `with` operations here are
   * deliberately the whole of its field-wise modification, so that every route to a value still
   * runs through a factory of the companion.
   *
   * @param steps  the steps defining the change in the value
   * @return the schedule holding the specified steps
   */
  def withSteps(steps: List[ValueStep]): ValueSchedule =
    ValueSchedule.of(initialValue, steps, stepSequence)

  /**
   * Returns a copy of this schedule with the sequence of steps replaced.
   *
   * The counterpart of [[withSteps]], and the operation a caller that assembled a schedule from an
   * initial value and individual steps uses to add a sequence to it - the combination the Java
   * original allowed only through its builder, and the one its `resolveValues` test for a
   * sequence alongside a step exercises.
   *
   * @param stepSequence  the sequence of steps changing the value
   * @return the schedule holding the specified sequence
   */
  def withStepSequence(stepSequence: ValueStepSequence): ValueSchedule =
    ValueSchedule.of(initialValue, steps, Some(stepSequence))

  //-------------------------------------------------------------------------
  /**
   * Checks whether this instance equals another object.
   *
   * Another instance is equal when its initial value has the same bit pattern and its steps and
   * sequence are equal. The equality synthesised for a case class would compare the double with
   * the numeric comparison of the platform, under which a value that is not a number is not even
   * equal to itself and a negative zero equals a positive zero, so this replaces it. An object of
   * any other type is not equal.
   *
   * @param obj  the object to compare to
   * @return true if the other object holds the same initial value, steps and sequence
   */
  override def equals(obj: Any): Boolean = obj match {
    case other: ValueSchedule =>
      (this eq other) ||
        (java.lang.Double.compare(initialValue, other.initialValue) == 0 &&
          steps == other.steps &&
          stepSequence == other.stepSequence)
    case _ => false
  }

  /**
   * Returns a hash code consistent with `equals`.
   *
   * The mixing is that of the Java bean this replaces - a seed, then each field in declaration
   * order - with the initial value hashed by its bit pattern so that instances which `equals`
   * calls equal always agree here too. The seed is the hash of the type's own name rather than the
   * identity hash of its class, which the generated bean used: that makes the hash of an instance
   * a function of the instance alone, identical in every run of every program.
   *
   * @return the hash code of the initial value, steps and sequence held
   */
  override def hashCode: Int =
    ((ValueSchedule.HashSeed * 31 + java.lang.Double.hashCode(initialValue)) * 31 +
      steps.hashCode) * 31 + stepSequence.hashCode

  /**
   * Renders this schedule as text.
   *
   * The rendering is the property-by-property form of the Java bean being ported, naming the
   * initial value, the steps and the sequence where one is held:
   *
   * {{{
   * ValueSchedule{initialValue=200.0, steps=List()}
   * ValueSchedule{initialValue=200.0, steps=List(), stepSequence=ValueStepSequence{...}}
   * }}}
   *
   * Two details differ from the bean, deliberately and cosmetically. The bean named the sequence
   * even when it held none, rendering it as an empty value, while this names it only when it is
   * actually held - a schedule without a sequence has nothing to say about one. And the steps
   * render in the form the standard library gives a list rather than in the bracketed form the
   * collection library of the original gave one. Nothing a value holds is hidden by either, no
   * test of either implementation asserts the form, and this rendering is also what reaches a
   * failure report as the rejected definition, where naming the fields that are present is what
   * makes the report readable.
   *
   * @return the text form of this schedule
   */
  override def toString: String = {
    val fields =
      List(s"initialValue=$initialValue", s"steps=$steps") :::
        stepSequence.map(sequence => s"stepSequence=$sequence").toList
    s"ValueSchedule{${fields.mkString(", ")}}"
  }
}

/**
 * Companion of [[ValueSchedule]], holding its constants, its factories, its typeclass instances
 * and its codec.
 *
 * The factories below are the only way to obtain a schedule from outside this file, and every one
 * of them is total: the reason is part of the contract of the type and is set out in its
 * documentation. They replace the four factories and the public builder of the bean being ported -
 * the builder is covered by the full-field factory here together with the two `with` operations of
 * the type.
 *
 * ===Why the overload set is shaped the way it is===
 *
 * The bean offered a varargs factory, `of(double, ValueStep...)`, alongside a factory taking the
 * initial value alone. Transcribing both directly would leave `of(0d)` matching two factories at
 * once - the single-value one, and the varargs one with no steps supplied - which is ambiguous and
 * does not compile at the call site. The varargs factory is therefore written to require its first
 * step, `of(initialValue, firstStep, restSteps*)`, which keeps `of(10000d, step1, step2)` reading
 * exactly as it did in Java while leaving `of(0d)` unambiguous. The empty case that Java expressed
 * by passing an empty array is expressed here by passing an empty list to the list factory,
 * `of(10000d, Nil)`.
 *
 * Two typeclass instances are published, and exactly two: a `Hash`, which is the single
 * equality-bearing instance of the type - `Hash` extends `Eq`, so declaring an `Eq` as well would
 * leave two instances that could disagree and one of them ambiguous - and a `Show`. There is
 * deliberately no `Order`: the Java type is not `Comparable`, and an initial value paired with a
 * list of steps has no ordering worth inventing, since ranking two schedules by their initial
 * values alone would order values that differ in everything else by one field.
 */
object ValueSchedule {

  //-------------------------------------------------------------------------
  /**
   * The seed the hash of a schedule is mixed from.
   *
   * The hash of the type's own name, so that the hash of an instance is a function of the instance
   * alone and is identical in every run of every program - unlike the identity hash of the class
   * that the generated bean seeded its own mixing with.
   */
  private val HashSeed: Int = "ValueSchedule".hashCode

  /**
   * The attribute the rejected definition is attached to every failure under.
   *
   * The name every schedule-shaped failure of this port reports its subject under, so that a
   * report naming a step that could not be resolved also carries the whole definition it came
   * from without that definition having to be parsed back out of the message.
   */
  private val DefinitionAttribute: String = "definition"

  /**
   * Reported when a period of the schedule holds no computed value.
   *
   * No schedule brings this about: the index checked against the computed values comes from the
   * schedule those values were computed from, and names one of its periods by construction. It is
   * reported rather than raised because this module raises nothing, and it names the type so that
   * a report reaching a log is traceable to here.
   */
  private val PeriodValueMissing: String =
    "ValueSchedule resolved a step to a schedule period that holds no value"

  //-------------------------------------------------------------------------
  /**
   * Builds the message reporting two steps that resolved to the same period with different
   * adjustments.
   *
   * The wording is that of the Java original, naming the unadjusted start date of the period the
   * two steps collided in and the whole definition they came from.
   *
   * @param periods  the periods of the schedule, in schedule order
   * @param index  the index of the period the two steps resolved to
   * @param definition  the definition being resolved, named in the message as the original named
   *   it
   * @return the message
   */
  private def duplicateStepMessage(
      periods: NonEmptyList[SchedulePeriod],
      index: Int,
      definition: ValueSchedule): String = {
    // the index came from resolving a step against these very periods, so it names one of them;
    // the empty rendering below is unreachable and is written rather than read out of the option
    // so that building a message cannot itself fail
    val startDate = periods.toList.lift(index).map(_.unadjustedStartDate.toString).getOrElse("")
    "Invalid ValueSchedule, two steps resolved to the same schedule period starting on " +
      s"$startDate, schedule defined as $definition"
  }

  /**
   * Builds the message reporting a step whose date is not a period boundary and which changes the
   * value of the period its date falls in.
   *
   * The wording is that of the Java original, naming the date of the step. Only a date-based step
   * ever reaches this message - a step positioned by an index either names a period of the
   * schedule or is reported for naming none - so the empty rendering of an absent date is
   * unreachable, and is written rather than read out of the option so that building a message
   * cannot itself fail.
   *
   * @param step  the step whose date is not a period boundary
   * @return the message
   */
  private def boundaryMismatchMessage(step: ValueStep): String =
    "ValueStep date does not match a period boundary: " +
      step.date.map(_.toString).getOrElse("")

  //-------------------------------------------------------------------------
  /**
   * The adjustments assigned to the periods of a schedule while the steps of a definition are
   * being resolved, together with the steps that resolved to no period at all.
   *
   * This is the immutable counterpart of the two pieces of mutable state the Java original
   * threaded through its assignment loop - an array of nullable adjustments indexed by period, and
   * a list of the steps it set aside - and it is threaded through a fold instead. The set-aside
   * steps accumulate in reverse, which is what makes adding one an operation on the head of a
   * list, and [[StepAssignment.unassignedSteps]] puts them back into the order they were given so
   * that the check they are subject to reports the earliest offender, as the original did.
   *
   * @param slots  the adjustment assigned to each period, keyed by period index; a period with no
   *   entry has no adjustment assigned to it
   * @param reversedUnassignedSteps  the steps that resolved to no period, most recent first
   */
  private[value] final case class StepAssignment(
      slots: Map[Int, ValueAdjustment],
      reversedUnassignedSteps: List[ValueStep]) {

    /**
     * Returns this assignment with the specified adjustment assigned to the specified period.
     *
     * An adjustment already assigned to that period is replaced, which is only ever reached with
     * an equal adjustment: an unequal one is a contradiction and is reported before this is
     * called.
     *
     * @param index  the index of the period
     * @param value  the adjustment to assign to it
     * @return the assignment including that adjustment
     */
    def withSlot(index: Int, value: ValueAdjustment): StepAssignment =
      copy(slots = slots.updated(index, value))

    /**
     * Returns this assignment with the specified step set aside as resolving to no period.
     *
     * @param step  the step that resolved to no period
     * @return the assignment including that step among the ones set aside
     */
    def withUnassignedStep(step: ValueStep): StepAssignment =
      copy(reversedUnassignedSteps = step :: reversedUnassignedSteps)

    /**
     * The steps that resolved to no period, in the order they were given.
     *
     * @return the steps set aside, earliest first
     */
    def unassignedSteps: List[ValueStep] = reversedUnassignedSteps.reverse
  }

  /**
   * The empty assignment, which the fold over the steps of a definition starts from.
   */
  private[value] object StepAssignment {

    /** No adjustment assigned to any period, and no step set aside. */
    val Empty: StepAssignment = StepAssignment(Map.empty, Nil)
  }


  //-------------------------------------------------------------------------
  /** A value schedule that always has the value zero. */
  val ALWAYS_0: ValueSchedule = of(0d)

  /** A value schedule that always has the value one. */
  val ALWAYS_1: ValueSchedule = of(1d)

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance from a single value that does not change over time.
   *
   * @param value  a single value that does not change over time
   * @return the value schedule
   */
  def of(value: Double): ValueSchedule = create(value, Nil, None)

  /**
   * Obtains an instance from an initial value and a list of changes.
   *
   * Each step fully defines a single change in the value. The date of each change can be specified
   * as an absolute date or in relative terms.
   *
   * This is also the factory to reach for where there are no changes at all, `of(initialValue,
   * Nil)`, which is what the Java original expressed by handing its varargs factory an empty
   * array.
   *
   * @param initialValue  the initial value used for the first period
   * @param steps  the full definition of how the value changes over time
   * @return the value schedule
   */
  def of(initialValue: Double, steps: List[ValueStep]): ValueSchedule =
    create(initialValue, steps, None)

  /**
   * Obtains an instance from an initial value and one or more changes.
   *
   * Each step fully defines a single change in the value. The date of each change can be specified
   * as an absolute date or in relative terms.
   *
   * The first change is named separately from the rest so that this factory cannot be confused
   * with the one taking the initial value alone; see the note on the overload set above. A caller
   * holding its steps in a list reaches for the list factory instead.
   *
   * {{{
   * ValueSchedule.of(10000d, step1, step2)  // the schedule changing at both steps
   * }}}
   *
   * @param initialValue  the initial value used for the first period
   * @param firstStep  the first change in the value
   * @param restSteps  the remaining changes in the value, in the order they are to be resolved
   * @return the value schedule
   */
  def of(initialValue: Double, firstStep: ValueStep, restSteps: ValueStep*): ValueSchedule =
    create(initialValue, firstStep :: restSteps.toList, None)

  /**
   * Obtains an instance from an initial value and a sequence of steps.
   *
   * The sequence defines changes from one date to another date using a frequency. For example, the
   * value might change every year from 2011-06-01 to 2015-06-01.
   *
   * @param initialValue  the initial value used for the first period
   * @param stepSequence  the full definition of how the value changes over time
   * @return the value schedule
   */
  def of(initialValue: Double, stepSequence: ValueStepSequence): ValueSchedule =
    create(initialValue, Nil, Some(stepSequence))

  /**
   * Obtains an instance from an initial value, a list of changes and a sequence of steps.
   *
   * This is the factory for a caller holding every field of a schedule rather than one of the
   * shapes above - the decoder below is one, and it is what replaces the builder of the bean being
   * ported, which was the only way that bean could express individual steps and a sequence at
   * once. Doing so is possible but not recommended, as the documentation of the type says, and
   * where it is done the steps and the sequence must resolve to different dates; a clash between
   * them is reported by [[ValueSchedule.resolveValues]] rather than here.
   *
   * @param initialValue  the initial value used for the first period
   * @param steps  the full definition of how the value changes over time
   * @param stepSequence  the sequence of steps changing the value, if the schedule holds one
   * @return the value schedule
   */
  def of(
      initialValue: Double,
      steps: List[ValueStep],
      stepSequence: Option[ValueStepSequence]): ValueSchedule =
    create(initialValue, steps, stepSequence)

  //-------------------------------------------------------------------------
  /**
   * Creates a value, which every route into the type funnels through.
   *
   * This is the only instantiation of the type and it is private, so the factories above are the
   * whole of its construction. The type is an abstract case class with a private constructor, so
   * it has neither a public `apply` nor a `copy` and this is written as an anonymous extension of
   * it - the shape every closed-construction type of this port uses to keep those two synthesised
   * members from existing while `unapply` and pattern matching still do.
   *
   * @param initialValue  the initial value used for the first period
   * @param steps  the full definition of how the value changes over time
   * @param stepSequence  the sequence of steps changing the value, if the schedule holds one
   * @return the schedule holding the three fields
   */
  private def create(
      initialValue: Double,
      steps: List[ValueStep],
      stepSequence: Option[ValueStepSequence]): ValueSchedule =
    new ValueSchedule(initialValue, steps, stepSequence) {}

  //-------------------------------------------------------------------------
  /**
   * The hashing and equality of schedules.
   *
   * Taken from the `equals` and `hashCode` of the type, which compare and mix the initial value by
   * its bit pattern, as every double-bearing type of this port does. This is the type's only
   * equality-bearing instance, and `Eq[ValueSchedule]` is obtained from it by subtyping.
   *
   * @return the hashing of schedules
   */
  implicit val hash: Hash[ValueSchedule] = Hash.fromUniversalHashCode[ValueSchedule]

  /**
   * The rendering of schedules as text.
   *
   * Renders what [[ValueSchedule.toString]] renders, so the two ways of putting a schedule into a
   * message agree.
   *
   * @return the rendering of a schedule
   */
  implicit val show: Show[ValueSchedule] = Show.show(_.toString)

  //-------------------------------------------------------------------------
  // The codec of the double field, brought into scope for the derivations below and for nothing
  // else. It has to be taken from here rather than from the JSON library, whose instance cannot
  // express a value that is not a number; importing it at this point is what makes that choice
  // deliberate and local, as the codec support of `strata-collect` intends. The codecs of the step
  // and sequence fields need no import: they are published by their own companions and so are
  // found for those types wherever they are needed.
  import Codecs.implicits.doubleCodec

  /**
   * The raw field shape of a schedule, from which both halves of the codec below are derived.
   *
   * A type whose construction is closed needs this intermediate product because derivation reads
   * the public constructor of a product and this type has none - it is an abstract case class whose
   * constructor is private - so there is no public shape to derive from. Writing the three fields
   * out by hand instead would state the same contract a second time.
   *
   * The steps are carried as an optional list rather than as a list, which is the one place this
   * shape differs from the type: it makes the decoder accept a document that omits the array
   * altogether and read it as no steps, while the encoder always writes the array, because the
   * steps are a mandatory property of the schedule and an empty list of them is a fact about the
   * schedule rather than an absence.
   *
   * @param initialValue  the initial value, carried through the double policy of this port
   * @param steps  the steps, carried as the array of the objects their own codec writes, or
   *   nothing where a document omits the array
   * @param stepSequence  the sequence, carried as the object its own codec writes, if held
   */
  private final case class Raw(
      initialValue: Double,
      steps: Option[List[ValueStep]],
      stepSequence: Option[ValueStepSequence])

  /** The derived encoder of the raw field shape, used by the encoder below. */
  private val rawEncoder: Encoder[Raw] = deriveEncoder[Raw]

  /** The derived decoder of the raw field shape, used by the decoder below. */
  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /**
   * The JSON encoding of schedules.
   *
   * A value is an object holding its three properties under the names the Java bean declared and
   * in declaration order:
   *
   * {{{
   * {"initialValue":200.0,"steps":[]}
   * {"initialValue":200.0,"steps":[{"date":"2014-02-01","value":{...}}],"stepSequence":{...}}
   * }}}
   *
   * The initial value is written through the single policy of this port for doubles, so a value
   * that is not a number and the two infinities appear as the strings `"NaN"`, `"Infinity"` and
   * `"-Infinity"` while a finite value is written as a JSON number, exactly to the bit. The steps
   * and the sequence are written by their own codecs.
   *
   * A sequence that is not held is dropped from the document rather than written as an explicitly
   * empty field, which is the policy every product of this port follows and which matches the
   * optional property the bean declared; the steps, being a mandatory property, are always written
   * and appear as an empty array where there are none.
   *
   * Both halves of the codec are assembled by the same compile-time derivation over the same raw
   * shape, which is what keeps them from drifting apart, and no part of the encoding inspects a
   * class while the program runs. Two equal values encode to identical bytes: the fields are
   * written in their declaration order, and the order of the steps is part of the value rather
   * than something the encoder chooses.
   *
   * @return the JSON encoding of schedules
   */
  implicit val encoder: Encoder[ValueSchedule] =
    Codecs.dropNulls(rawEncoder.contramap[ValueSchedule] { schedule =>
      Raw(schedule.initialValue, Some(schedule.steps), schedule.stepSequence)
    })

  /**
   * The JSON decoding of schedules.
   *
   * This is the inverse of the encoding above and is likewise derived at compile time. The initial
   * value has to be present and is read through the same policy that wrote it; the steps and the
   * sequence are optional in the document, an omitted array of steps reading as no steps.
   *
   * Construction cannot fail, so nothing beyond the shape of the payload is checked here: a
   * payload of the right shape always yields a schedule, and an encoded schedule decodes back to
   * one equal to it - including one whose initial value is not a number, which the bit-pattern
   * equality of this type makes equal to itself. Whether the decoded definition can be resolved
   * against some schedule of periods is a separate question, answered by
   * [[ValueSchedule.resolveValues]] and not by this decoder, exactly as it is for a schedule a
   * caller built by hand.
   *
   * @return the JSON decoding of schedules
   */
  implicit val decoder: Decoder[ValueSchedule] =
    rawDecoder.map(raw => of(raw.initialValue, raw.steps.getOrElse(Nil), raw.stepSequence))
}
