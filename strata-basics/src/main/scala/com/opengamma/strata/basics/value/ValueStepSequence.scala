/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.value

import java.time.LocalDate

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
 * the last and pairing each date it lands on with the adjustment, and it is where a sequence that
 * does not in fact line up with its frequency is reported.
 *
 * ===Construction===
 *
 * Construction is validating, so the primary constructor is private and there is neither an
 * `apply` nor a `copy`: [[ValueStepSequence.of]] is the only way to obtain a sequence, and every
 * value that exists therefore has its dates in order and an adjustment that varies the value
 * rather than replacing it. The two conditions are independent and are reported together when
 * both fail, which is more than the validator of the bean being ported managed - it raised the
 * first fault it found and stopped.
 *
 * ===Equality===
 *
 * Equality and hashing are those of the four properties, and so are those of [[ValueAdjustment]]
 * where the modifying value is concerned: an adjustment compares its double by bit pattern rather
 * than by numeric comparison, which is what the bean equality of the Java original did and what
 * every double-bearing type of this port does. A sequence carries no double of its own, so it
 * inherits that behaviour whole rather than restating it.
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
 *   adjustment type must not be `Replace`: a step that replaces the value discards whatever the
 *   previous step produced, so repeating one produces the same value at every step and expresses
 *   nothing a single [[ValueStep]] does not already express
 */
sealed abstract case class ValueStepSequence private (
    firstStepDate: LocalDate,
    lastStepDate: LocalDate,
    frequency: Frequency,
    adjustment: ValueAdjustment) {

  //-------------------------------------------------------------------------
  /**
   * Resolves this sequence to a list of steps, appending them to the steps already held.
   *
   * The dates of the steps are worked out by walking the frequency of this sequence from its
   * first date to its last, under the roll convention supplied: both dates are adjusted by the
   * convention, and each subsequent date is the one the convention reaches from the date before
   * it. That is why the convention is applied to '''every''' generated date rather than only to
   * the two ends - an annual sequence under the IMM convention lands on the third Wednesday of
   * each September, not on the anniversary of the first one - and it is what the corresponding
   * method of the Java original did, date by date, through the same two operations.
   *
   * The walk is the one place this port differs in shape from that method, and only in shape: the
   * original maintained a pair of mutable dates and a mutable builder, while this iterates the
   * `next` operation of the convention from the adjusted first date and stops at the first date
   * that passes the adjusted last date. The sequence of dates is identical, because `next` always
   * returns a date strictly after the one handed to it and so the iteration always terminates.
   *
   * The last date the walk lands on has to '''be''' the adjusted last date of this sequence. Where
   * it is not, the frequency does not divide the span of the sequence - a twelve month frequency
   * over six months, say - and that is reported rather than resolved, because the dates a caller
   * supplied describe no sequence of steps under this convention. The report names the frequency,
   * the convention and the two dates that disagree, in the words of the original.
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
   * This is visible within this package rather than publicly, as it was in the Java original: it
   * is the operation [[ValueSchedule]] performs while resolving its own steps, and a caller
   * outside reaches it through that.
   *
   * @param existingSteps  the existing list of steps, which the generated steps are appended to
   * @param rollConv  the roll convention of the schedule this sequence applies to
   * @return the steps supplied followed by the generated steps, or the failure describing why the
   *   dates and frequency of this sequence describe no sequence of steps under the convention
   */
  private[value] def resolve(
      existingSteps: List[ValueStep],
      rollConv: RollConvention): FailureOr[List[ValueStep]] = {

    val start = rollConv.adjust(firstStepDate)
    val adjustedLastStepDate = rollConv.adjust(lastStepDate)
    val dates = Iterator
      .iterate(start)(date => rollConv.next(date, frequency))
      .takeWhile(date => !date.isAfter(adjustedLastStepDate))
      .toList
    // the last date reached, or the adjusted first date where the walk reached nothing at all,
    // which is the state the mutable variable of the original was left in by an empty loop
    val prev = dates.lastOption.getOrElse(start)
    if (prev != adjustedLastStepDate) {
      Left(
        Failure.Invalid(
          ValueStepSequence.frequencyMismatch(frequency, rollConv, adjustedLastStepDate, prev)))
    } else {
      Right(existingSteps ++ dates.map(date => ValueStep.of(date, adjustment)))
    }
  }

  //-------------------------------------------------------------------------
  /**
   * Renders this sequence as text.
   *
   * The rendering is the property-by-property form of the Java bean being ported, naming all four
   * properties in declaration order. It is one line; the example below is wrapped only to fit:
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
 * makes the invariants of the type - dates in order, and an adjustment that varies rather than
 * replaces - properties of every value that exists rather than properties a caller is asked to
 * respect. It replaces both the factory and the private builder of the bean being ported, the
 * latter having had no counterpart here.
 *
 * Two typeclass instances are published, and exactly two: a `Hash`, which is the single
 * equality-bearing instance of the type - `Hash` extends `Eq`, so declaring an `Eq` as well would
 * leave two instances that could disagree and one of them ambiguous - and a `Show`. There is
 * deliberately no `Order`: the Java type is not `Comparable`, and while two sequences could be
 * ranked by their first date, doing so would order values that differ in three other properties by
 * one of them alone.
 */
object ValueStepSequence {

  //-------------------------------------------------------------------------
  /** The name the first date is reported under, which is the property name of the bean. */
  private val FirstStepDateField: String = "firstStepDate"

  /** The name the last date is reported under, which is the property name of the bean. */
  private val LastStepDateField: String = "lastStepDate"

  /**
   * Reported when the adjustment replaces the value rather than varying it, in the words of the
   * bean being ported.
   */
  private val ReplacementNotAllowed: String = "ValueAdjustmentType must not be 'Replace'"

  /**
   * The ordering of dates the order check below is performed with.
   *
   * The checking helpers of this port are generic in the type being compared and take its cats
   * ordering, and cats publishes no instance for `java.time.LocalDate` - the class implements
   * `Comparable[ChronoLocalDate]` rather than `Comparable[LocalDate]`, so the ordering derived
   * from a comparable type does not apply to it either. The instance is therefore stated here, as
   * the natural time-line order the class itself defines, and kept private: it exists to serve
   * the check of this file, and publishing an ordering for a type this module does not own would
   * put an instance into implicit scope for every file that imports anything from here.
   */
  private implicit val dateOrder: Order[LocalDate] =
    Order.from((first, second) => first.compareTo(second))

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance from the dates, frequency and change.
   *
   * The first date must be before the last date or equal to it, and the adjustment must not be of
   * type `Replace`. Both conditions are the validator of the bean being ported, and both are
   * reported in its words:
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
   * being sent back for the other. That is the last line above and is more than the validator
   * managed: it raised the first fault it found and stopped.
   *
   * Nothing about the frequency is checked here. Whether it divides the span between the two
   * dates depends on the roll convention of the schedule the sequence is applied to, which is not
   * known until [[ValueStepSequence.resolve]] is reached, and whether it is coarser than that
   * schedule's own frequency is a question about the schedule; the Java original checked neither
   * at construction either.
   *
   * @param firstStepDate  the first date of the sequence
   * @param lastStepDate  the last date of the sequence
   * @param frequency  the frequency of changes
   * @param adjustment  the adjustment at each step
   * @return the varying step sequence, or the failures describing why the arguments describe none
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

  //-------------------------------------------------------------------------
  /**
   * Creates a value, which every route into the type funnels through.
   *
   * This is the only instantiation of the type and it is private, so the factory above is the
   * whole of its construction. The type is an abstract case class whose constructor is private, so
   * it has neither a public `apply` nor a `copy` and this is written as an anonymous extension of
   * it - the shape every validated type of this port uses to keep those two synthesised members
   * from existing while `unapply` and pattern matching still do.
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
    new ValueStepSequence(firstStepDate, lastStepDate, frequency, adjustment) {}

  /**
   * Describes a sequence whose frequency does not reach its last date under a roll convention.
   *
   * The text is that of the message the Java original raised, with the frequency and the
   * convention rendered by their names, which is what their own text forms render and therefore
   * what the template of the original interpolated.
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

  //-------------------------------------------------------------------------
  /**
   * The hashing and equality of sequences.
   *
   * Taken from the `equals` and `hashCode` of the type, which are those synthesised for its four
   * properties and so are those of [[ValueAdjustment]] where its double is concerned - compared by
   * bit pattern, as every double-bearing type of this port compares one. This is the type's only
   * equality-bearing instance, and `Eq[ValueStepSequence]` is obtained from it by subtyping.
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

  //-------------------------------------------------------------------------
  /**
   * The raw field shape of a sequence, from which both halves of the codec below are derived.
   *
   * A validated type needs this intermediate product because derivation reads the public
   * constructor of a product and this type has none - it is an abstract case class whose
   * constructor is private - so there is no public shape to derive from. Writing the four fields
   * out by hand instead would state the same contract a second time.
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

  /** The derived encoder of the raw field shape, used by the encoder below. */
  private val rawEncoder: Encoder[Raw] = deriveEncoder[Raw]

  /** The derived decoder of the raw field shape, used by the validating decoder below. */
  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /**
   * The JSON encoding of sequences.
   *
   * A value is an object holding its four properties, under the names the Java bean declared and
   * in declaration order:
   *
   * {{{
   * {"firstStepDate":"2016-04-20","lastStepDate":"2016-10-20","frequency":"P3M",
   *  "adjustment":{"modifyingValue":-100.0,"type":"DeltaAmount"}}
   * }}}
   *
   * The dates are written in their ISO form, the frequency as the bare name its own codec writes,
   * and the adjustment as the object its own codec writes. No property of this type is optional,
   * so nothing is ever dropped from the document; the encoder is wrapped in the same
   * empty-field-dropping combinator every product of this port is wrapped in, so that the policy
   * is stated in one place and holds if a property ever becomes optional.
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
   * to [[ValueStepSequence.of]], so a document whose dates are in the wrong order or whose
   * adjustment replaces the value is a decoding failure carrying every reason it is, rather than a
   * value this type would not have built. All four fields have to be present.
   *
   * @return the JSON decoding of sequences
   */
  implicit val decoder: Decoder[ValueStepSequence] =
    Codecs.validatedDecoder[Raw, ValueStepSequence] { raw =>
      of(raw.firstStepDate, raw.lastStepDate, raw.frequency, raw.adjustment)
    }(rawDecoder)
}
