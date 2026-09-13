/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.value

import java.time.LocalDate

import cats.data.NonEmptyList

import cats.Hash
import cats.Show

import io.circe.Json
import io.circe.parser.decode
import io.circe.parser.parse
import io.circe.syntax._

import org.scalacheck.Gen

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import com.opengamma.strata.basics.schedule.SchedulePeriod
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[ValueStep]].
 *
 * The first six tests cover the three factories and the rejections of the two that report one,
 * equality and hashing, the accessors, the rendering, the closed construction surface, and the
 * JSON codec in both directions. The last two compare the resolution helpers of the type against
 * a linear reference written here.
 *
 * ===The asymmetry of the two short factories===
 *
 * `of(periodIndex, value)` reports its outcome and `of(date, value)` does not: an index of zero
 * or less names the start of the first period, where no change is permitted, while whether a date
 * lines up with a period boundary is a question about the schedule the step is resolved against
 * rather than about the step. The two are therefore asserted in the two shapes they have, and the
 * first two tests below are where that difference is visible.
 *
 * The three-field [[ValueStep.of]] is the only factory that can be asked for either of the two
 * states the type forbids - neither position supplied, and both supplied - and so the only one
 * whose rejections name them; `test_builder_invalid` is where all three rejection messages are
 * pinned.
 *
 * ===The two equivalence tests===
 *
 * The last two tests are equivalence tests over the resolution helpers rather than tests of what
 * those helpers mean: the helpers answer from a [[ValueStep.PeriodIndex]] built once per
 * resolution rather than by searching the period list once per step, so the indexed answers are
 * compared against a linear search written independently in this file - over hand-written period
 * lists first, over randomly generated ones after.
 *
 * The period lists those two tests use are synthetic inputs, built from [[SchedulePeriod.of]] and
 * handed straight to `ValueStep.PeriodIndex.of`, `findIndex` and `findPreviousIndex`; no
 * `Schedule` is involved, and the shapes among them are wider than one permits. `Schedule.of`
 * reports any period whose end date falls after the start date of the period after it, in the
 * unadjusted pair or in the adjusted one, so an unsorted, overlapping or duplicate-start list is
 * not a list any `Schedule` holds - gaps between periods are the case that is legal, since equal
 * and later dates pass that check. Pinning the indexed implementation to the reference on those
 * shapes is what keeps the two agreeing without leaning on an order no argument type guarantees.
 */
final class ValueStepSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  /** The relative adjustment most steps of this spec carry, subtracting 2000 from a value. */
  private val DeltaMinus2000: ValueAdjustment = ValueAdjustment.ofDeltaAmount(-2000.0d)

  /** The absolute adjustment used wherever a second, differing adjustment is needed. */
  private val Absolute100: ValueAdjustment = ValueAdjustment.ofReplace(100.0d)

  /** The date the date-based steps of this spec are positioned at. */
  private val StepDate: LocalDate = date(2014, 6, 30)

  /** The later date a date-based step is compared against in the equality assertions. */
  private val LaterStepDate: LocalDate = date(2014, 7, 30)

  //-------------------------------------------------------------------------
  /**
   * The message reported when neither position is supplied.
   *
   * The three messages below are the literals [[ValueStep]] reports, held here and asserted as
   * literals, so a change to any of the three wordings cannot pass unnoticed.
   */
  private val EitherPositionRequired: String = "Either the 'periodIndex' or 'date' must be set"

  /** The message reported when both positions are supplied. */
  private val SinglePositionRequired: String =
    "Either the 'periodIndex' or 'date' must be set, not both"

  /** The message reported for a period index of zero or less. */
  private val PeriodIndexNotPositive: String = "The 'periodIndex' must not be zero or negative"

  //-------------------------------------------------------------------------
  /**
   * The expected JSON of an index-based step.
   *
   * The document holds the position the step actually has and its adjustment. There is no `date`
   * field: the field of the position a step does not hold is dropped from the output rather than
   * written as an explicitly empty one.
   */
  private val ExpectedIndexJson: String =
    """{"periodIndex":2,"value":{"modifyingValue":-2000.0,"type":"DeltaAmount"}}"""

  /** The expected JSON of a date-based step, whose date is written in its ISO form. */
  private val ExpectedDateJson: String =
    """{"date":"2014-06-30","value":{"modifyingValue":-2000.0,"type":"DeltaAmount"}}"""

  //-------------------------------------------------------------------------
  /**
   * Reads the step out of an outcome that is expected to hold one.
   *
   * The factories that can reject their input report the rejection as a value, so a fixture
   * built through one of them is an outcome rather than a step. This is the one place this spec
   * turns the first into the second, and it does so by handling both sides: a fixture that fails
   * is a defect in this spec and is reported as one, naming the failures, rather than raising an
   * error from a partial accessor that would name nothing.
   *
   * @param result  the outcome of a factory, expected to hold a step
   * @return the step the outcome holds
   */
  private def step(result: ResultNec[ValueStep]): ValueStep =
    result.fold(
      failures => fail(s"invalid step fixture: ${failures.toChain.toList.map(_.message).mkString("; ")}"),
      held => held)

  /**
   * The messages of the failures an outcome holds, in the order it holds them.
   *
   * The matchers assert that *some* failure carries a reason or matches a pattern, which is the
   * right question for an outcome that can hold several. This helper answers the other two
   * questions the accumulating factory raises - how many failures there are and which, exactly -
   * so that a chain of two can be pinned to the two messages that belong in it, in order,
   * rather than to the presence of one of them.
   *
   * @param result  the outcome to inspect
   * @return the messages of its failures in order, empty when it holds none
   */
  private def messages(result: ResultNec[ValueStep]): List[String] =
    result.fold(failures => failures.toChain.toList.map(_.message), _ => List.empty[String])

  /**
   * Parses one of the expected JSON forms above into the JSON model.
   *
   * Comparing documents is what makes an assertion about the encoding rather than about its
   * rendering: whitespace and the spelling of a number are matters of presentation, while the
   * fields present, their names and their values are the contract. A literal in this file that
   * does not parse is a defect in the spec itself.
   *
   * @param text  the JSON text to parse
   * @return the parsed document
   */
  private def json(text: String): Json =
    parse(text).getOrElse(fail(s"the expected JSON of this spec is not itself valid JSON: $text"))

  //-------------------------------------------------------------------------
  test("test_of_intAdjustment") {
    // The index-based factory reports its outcome, because an index of zero or less names the
    // start of the first period and is rejected; an index of two names the third period and is
    // accepted, so this outcome holds a step.
    val result: ResultNec[ValueStep] = ValueStep.of(2, DeltaMinus2000)
    result should beSuccess

    // The three properties of the step: the position it holds reads back as `Some`, and the
    // position it does not hold as `None`.
    val test: ValueStep = step(result)
    test.date shouldBe None
    test.periodIndex shouldBe Some(2)
    test.value shouldBe DeltaMinus2000

    // The same step reached through the three-field factory, which is what makes the two routes
    // into an index-based step one value rather than two that merely look alike.
    result should haveValue(step(ValueStep.of(Some(2), None, DeltaMinus2000)))
  }

  //-------------------------------------------------------------------------
  test("test_of_dateAdjustment") {
    // The date-based factory is total: whether a date lines up with a period boundary is a
    // question about the schedule the step is resolved against, so there is nothing about a date
    // this type can reject and no handling is needed around the construction. The factory above
    // reports an outcome because an index of zero or less names the start of the first period,
    // where no change is permitted; the asymmetry is not normalised away here.
    val test: ValueStep = ValueStep.of(StepDate, DeltaMinus2000)
    test.date shouldBe Some(StepDate)
    test.periodIndex shouldBe None
    test.value shouldBe DeltaMinus2000

    // The three-field factory reaches the same value, as it does for an index-based step.
    ValueStep.of(None, Some(StepDate), DeltaMinus2000) should haveValue(test)
  }

  //-------------------------------------------------------------------------
  test("test_builder_invalid") {
    // The three-field `of` is the only factory that can be asked for a step holding neither
    // position, or both, and it reports each rejection as a value rather than raising it. Every
    // rejection here depends on the data supplied, so no exception is expected anywhere below.

    // 1. Neither position supplied, which only this factory can be given.
    val neither: ResultNec[ValueStep] = ValueStep.of(None, None, DeltaMinus2000)
    neither should beFailure
    neither should beFailureWith(FailureReason.INVALID)
    neither should haveFailureMessageMatching(".*Either the 'periodIndex' or 'date' must be set.*")
    messages(neither) shouldBe List(EitherPositionRequired)

    // 2. Both positions supplied. The pattern distinguishes this message from the one above,
    // which is a prefix of it, and the exact list pins which of the two was reported.
    val both: ResultNec[ValueStep] = ValueStep.of(Some(1), Some(StepDate), DeltaMinus2000)
    both should beFailureWith(FailureReason.INVALID)
    both should haveFailureMessageMatching(".*not both.*")
    messages(both) shouldBe List(SinglePositionRequired)

    // 3. An index of zero, which names the start of the first period. Both routes to an
    // index-based step reject it, and they report the same single failure, so a caller that
    // holds an index reads the same thing as one that holds the fields.
    val zero: ResultNec[ValueStep] = ValueStep.of(Some(0), None, DeltaMinus2000)
    zero should beFailureWith(FailureReason.INVALID)
    zero should haveFailureMessageMatching(".*must not be zero or negative.*")
    messages(zero) shouldBe List(PeriodIndexNotPositive)

    val zeroShort: ResultNec[ValueStep] = ValueStep.of(0, DeltaMinus2000)
    zeroShort should beFailureWith(FailureReason.INVALID)
    zeroShort should haveFailureMessageMatching(".*must not be zero or negative.*")
    messages(zeroShort) shouldBe List(PeriodIndexNotPositive)

    // 4. A negative index, which is the same fault and reports the same message.
    val negative: ResultNec[ValueStep] = ValueStep.of(Some(-1), None, DeltaMinus2000)
    negative should beFailureWith(FailureReason.INVALID)
    messages(negative) shouldBe List(PeriodIndexNotPositive)

    val negativeShort: ResultNec[ValueStep] = ValueStep.of(-1, DeltaMinus2000)
    negativeShort should beFailureWith(FailureReason.INVALID)
    messages(negativeShort) shouldBe List(PeriodIndexNotPositive)

    // The first index this factory accepts is one, which is the edge the message names.
    step(ValueStep.of(Some(1), None, DeltaMinus2000)).periodIndex shouldBe Some(1)

    // Where both checks fail at once both are reported, in the order they are checked - the pair
    // of positions first, then the range of the index - and both matchers see the whole chain, so
    // either reason or message is enough to match it.
    val twoFaults: ResultNec[ValueStep] = ValueStep.of(Some(0), Some(StepDate), DeltaMinus2000)
    twoFaults should beFailureWith(FailureReason.INVALID)
    twoFaults should haveFailureMessageMatching(".*not both.*")
    twoFaults should haveFailureMessageMatching(".*must not be zero or negative.*")
    messages(twoFaults).size should be > 1
    messages(twoFaults) shouldBe List(SinglePositionRequired, PeriodIndexNotPositive)
  }

  //-------------------------------------------------------------------------
  test("equals") {
    // Four index-based steps. The second is built independently from equal inputs, so equality is
    // shown to be structural rather than by reference, and either differing field - the index, or
    // the adjustment - is enough to make two steps unequal.
    val a1: ValueStep = step(ValueStep.of(2, DeltaMinus2000))
    val a2: ValueStep = step(ValueStep.of(2, DeltaMinus2000))
    val b: ValueStep = step(ValueStep.of(1, DeltaMinus2000))
    val c: ValueStep = step(ValueStep.of(2, Absolute100))
    a1 shouldBe a1
    a1 shouldBe a2
    a1 should not be b
    a1 should not be c

    // Hashing agrees with equality, and `Hash` - the type's single equality-bearing instance,
    // from which `Eq` is obtained by subtyping - reports the same relation as the platform
    // equality does, in both directions.
    a1.hashCode shouldBe a2.hashCode
    Hash[ValueStep].hash(a2) shouldBe a1.hashCode
    Hash[ValueStep].eqv(a1, a1) shouldBe true
    Hash[ValueStep].eqv(a1, a2) shouldBe true
    Hash[ValueStep].eqv(a1, b) shouldBe false
    Hash[ValueStep].eqv(a1, c) shouldBe false

    // Four date-based steps, and the same facts about them, with the date as a differing field.
    val d1: ValueStep = ValueStep.of(StepDate, DeltaMinus2000)
    val d2: ValueStep = ValueStep.of(StepDate, DeltaMinus2000)
    val e: ValueStep = ValueStep.of(LaterStepDate, DeltaMinus2000)
    val f: ValueStep = ValueStep.of(LaterStepDate, Absolute100)
    d1 shouldBe d1
    d1 shouldBe d2
    d1 should not be e
    d1 should not be f
    d1.hashCode shouldBe d2.hashCode
    Hash[ValueStep].hash(d2) shouldBe d1.hashCode
    Hash[ValueStep].eqv(d1, d2) shouldBe true
    Hash[ValueStep].eqv(d1, e) shouldBe false
    Hash[ValueStep].eqv(d1, f) shouldBe false

    // A step positioned in relative terms is never a step positioned in absolute terms, whatever
    // the two positions would resolve to against some schedule: the two hold different fields,
    // and equality here compares the fields rather than resolving anything.
    a1 should not be d1
    d1 should not be a1
    Hash[ValueStep].eqv(a1, d1) shouldBe false
    Hash[ValueStep].eqv(d1, a1) shouldBe false
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The accessors, the rendering, and the closed construction surface of a validated type, for
    // both of the two shapes a step can have.
    val test: ValueStep = step(ValueStep.of(2, DeltaMinus2000))
    val dateBased: ValueStep = ValueStep.of(StepDate, DeltaMinus2000)

    // Every field goes in and comes back.
    test.periodIndex shouldBe Some(2)
    test.date shouldBe None
    test.value shouldBe DeltaMinus2000
    dateBased.periodIndex shouldBe None
    dateBased.date shouldBe Some(StepDate)
    dateBased.value shouldBe DeltaMinus2000

    // `Show` renders what `toString` renders, so the two ways of putting a step into a message
    // agree. Each rendering names the position the step actually holds, and the adjustment
    // renders as the calculation it performs.
    Show[ValueStep].show(test) shouldBe test.toString
    Show[ValueStep].show(dateBased) shouldBe dateBased.toString
    test.toString shouldBe "ValueStep{periodIndex=2, value=ValueAdjustment[result = input + -2000.0]}"
    dateBased.toString shouldBe
      "ValueStep{date=2014-06-30, value=ValueAdjustment[result = input + -2000.0]}"

    // The construction surface is closed: the primary constructor is private and neither an
    // `apply` nor a `copy` exists, so every step comes from a factory that validates and no
    // caller can reach the states those factories reject. Both facts are checked by compiling a
    // snippet and requiring it to fail - a `copy` would reintroduce every rejected state.
    assertDoesNotCompile("""ValueStep(Some(2), None, ValueAdjustment.ofDeltaAmount(-2000.0d))""")
    assertDoesNotCompile(
      """ValueStep.of(2, ValueAdjustment.ofDeltaAmount(-2000.0d)).map(_.copy(periodIndex = Some(3)))""")
    assertDoesNotCompile(
      """ValueStep.of(date(2014, 6, 30), ValueAdjustment.ofDeltaAmount(-2000.0d)).copy(date = None)""")

    // The same three snippets with the missing member replaced by one that exists, which is what
    // keeps the three above from passing for the wrong reason: a snippet naming an identifier
    // this scope cannot resolve would also fail to compile, and would prove nothing about
    // `apply` or `copy`. These compile, so every other name in them resolves and the only
    // difference is the member each of the three reaches for.
    assertCompiles("""ValueStep.of(Some(2), None, ValueAdjustment.ofDeltaAmount(-2000.0d))""")
    assertCompiles(
      """ValueStep.of(2, ValueAdjustment.ofDeltaAmount(-2000.0d)).map(_.periodIndex)""")
    assertCompiles(
      """ValueStep.of(date(2014, 6, 30), ValueAdjustment.ofDeltaAmount(-2000.0d)).date""")

    // Reading the three fields by pattern is unaffected by that closure: `unapply` is available,
    // so a match on the three fields still compiles and answers with them.
    val destructured: (Option[Int], Option[LocalDate], ValueAdjustment) = dateBased match {
      case ValueStep(index, stepDate, adjustment) => (index, stepDate, adjustment)
    }
    destructured shouldBe ((None, Some(StepDate), DeltaMinus2000))

    val destructuredIndex: (Option[Int], Option[LocalDate], ValueAdjustment) = test match {
      case ValueStep(index, stepDate, adjustment) => (index, stepDate, adjustment)
    }
    destructuredIndex shouldBe ((Some(2), None, DeltaMinus2000))
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // Both directions of the codec are asserted, not only the round trip: an encoding that is
    // wrong and a decoding that is wrong in the same way would still round trip. The documents
    // are compared as parsed JSON rather than as printed text, so the comparison is of the fields
    // rather than of the whitespace and number spelling.
    val test: ValueStep = step(ValueStep.of(2, DeltaMinus2000))
    test.asJson shouldBe json(ExpectedIndexJson)
    decode[ValueStep](test.asJson.noSpaces) shouldBe Right(test)
    decode[ValueStep](ExpectedIndexJson) shouldBe Right(test)

    // The field of the position a step does not hold is absent from the document rather than
    // written as an explicitly empty one, so the key set is the position held and the adjustment,
    // in declaration order.
    test.asJson.asObject.map(_.keys.toList) shouldBe Some(List("periodIndex", "value"))
    test.asJson.hcursor.downField("date").succeeded shouldBe false
    test.asJson.hcursor.downField("periodIndex").as[Int] shouldBe Right(2)

    // The other shape, whose date is written in its ISO form and whose index field is the one
    // that is absent.
    val dateBased: ValueStep = ValueStep.of(StepDate, DeltaMinus2000)
    dateBased.asJson shouldBe json(ExpectedDateJson)
    decode[ValueStep](dateBased.asJson.noSpaces) shouldBe Right(dateBased)
    decode[ValueStep](ExpectedDateJson) shouldBe Right(dateBased)
    dateBased.asJson.asObject.map(_.keys.toList) shouldBe Some(List("date", "value"))
    dateBased.asJson.hcursor.downField("periodIndex").succeeded shouldBe false
    dateBased.asJson.hcursor.downField("date").as[String] shouldBe Right("2014-06-30")

    // The decoder routes the fields through the same validating factory a caller's arguments go
    // through, so a document that names no position, both positions, or an index of zero is
    // rejected rather than decoded into a step the factory would never have built. The message
    // of the rejection is the message of the failure that caused it.
    val neither = decode[ValueStep]("""{"value":{"modifyingValue":-2000.0,"type":"DeltaAmount"}}""")
    neither.isLeft shouldBe true
    neither.swap.toOption.fold("")(error => error.getMessage) should include(EitherPositionRequired)

    val zeroIndex = decode[ValueStep](
      """{"periodIndex":0,"value":{"modifyingValue":-2000.0,"type":"DeltaAmount"}}""")
    zeroIndex.isLeft shouldBe true
    zeroIndex.swap.toOption.fold("")(error => error.getMessage) should include(PeriodIndexNotPositive)

    val bothPositions = decode[ValueStep](
      """{"periodIndex":1,"date":"2014-06-30","value":{"modifyingValue":-2000.0,"type":"DeltaAmount"}}""")
    bothPositions.isLeft shouldBe true
    bothPositions.swap.toOption.fold("")(error => error.getMessage) should include(SinglePositionRequired)
  }

  //-------------------------------------------------------------------------
  // The fixtures, generators and linear references of the two equivalence tests, kept here rather
  // than with the fixtures at the top of the file because nothing above this line uses them.

  /** The date every period and query date of the two equivalence tests is measured from. */
  private val IndexBaseDate: LocalDate = date(2016, 1, 1)

  /**
   * The message `findIndex` reports for an index-based step naming no period of the schedule.
   *
   * The two messages here are the literals the implementation reports, and they are what makes
   * the agreement asserted below an agreement of whole '''outcomes''' rather than of the
   * successful half of them: a failure is compared with its message, so a resolution reporting
   * the right condition in the wrong words would not match the reference.
   */
  private val IndexBeyondSchedule: String = "ValueStep index is beyond last schedule period"

  /** The message `findPreviousIndex` reports for a step that is not date-based. */
  private val NoDateHeld: String = "ValueStep is not date-based, so it has no preceding period"

  /**
   * Reads the period out of an outcome that is expected to hold one.
   *
   * @param result  the outcome of the period factory, expected to hold a period
   * @return the period the outcome holds
   */
  private def period(result: ResultNec[SchedulePeriod]): SchedulePeriod =
    result.fold(
      failures =>
        fail(s"invalid period fixture: ${failures.toChain.toList.map(_.message).mkString("; ")}"),
      held => held)

  /**
   * A period whose adjusted dates are its unadjusted dates, named by day offsets from the base
   * date.
   *
   * @param startOffset  the offset in days of the start date from the base date
   * @param endOffset  the offset in days of the end date from the base date
   * @return the period spanning those two dates, adjusted and unadjusted alike
   */
  private def periodAt(startOffset: Long, endOffset: Long): SchedulePeriod =
    period(
      SchedulePeriod.of(
        IndexBaseDate.plusDays(startOffset),
        IndexBaseDate.plusDays(endOffset)))

  /**
   * A period whose adjusted dates are its unadjusted dates moved by the specified shift.
   *
   * This is the shape in which the two passes of `findIndex` can answer differently: a date can
   * be the adjusted start of one period and the unadjusted start of another, and which period it
   * resolves to is then decided by the order of the passes rather than by the data.
   *
   * @param startOffset  the offset in days of the unadjusted start date from the base date
   * @param endOffset  the offset in days of the unadjusted end date from the base date
   * @param shift  the number of days the adjusted dates are moved by
   * @return the period holding those unadjusted dates and the shifted adjusted ones
   */
  private def shiftedPeriodAt(startOffset: Long, endOffset: Long, shift: Long): SchedulePeriod =
    period(
      SchedulePeriod.of(
        IndexBaseDate.plusDays(startOffset + shift),
        IndexBaseDate.plusDays(endOffset + shift),
        IndexBaseDate.plusDays(startOffset),
        IndexBaseDate.plusDays(endOffset)))

  //-------------------------------------------------------------------------
  /**
   * Finds the index of the specified step by searching the periods.
   *
   * This is the reference the indexed implementation is compared against, and it is written as
   * the two linear searches the question decomposes into rather than as the implementation it is
   * checking: the index of the first period whose unadjusted start date is the date of the step,
   * then - only if there is none - the index of the first whose adjusted start date is. Writing
   * it independently is the whole point; a reference that shared code with its subject would
   * agree with it by construction.
   *
   * @param stepUnderTest  the step to resolve
   * @param periods  the periods to resolve against, in the order they are held
   * @return the outcome the indexed implementation has to match
   */
  private def referenceFindIndex(
      stepUnderTest: ValueStep,
      periods: List[SchedulePeriod]): FailureOr[Option[Int]] =
    (stepUnderTest.periodIndex, stepUnderTest.date) match {
      case (Some(index), _) =>
        if (index >= periods.size) {
          Left(Failure.Invalid(IndexBeyondSchedule))
        } else {
          Right(Some(index))
        }
      case (None, Some(stepDate)) =>
        val unadjusted = periods.indexWhere(_.unadjustedStartDate == stepDate)
        val adjusted = periods.indexWhere(_.startDate == stepDate)
        if (unadjusted >= 0) {
          Right(Some(unadjusted))
        } else if (adjusted >= 0) {
          Right(Some(adjusted))
        } else {
          Right(None)
        }
      case (None, None) =>
        fail("no ValueStep holds neither position, so no generator of this spec produces one")
    }

  /**
   * Finds the index of the period preceding the specified step by searching the periods.
   *
   * The four rules are written in the order the implementation decides them, and the middle one
   * is written as the linear search it describes: the first period after the first one whose
   * unadjusted start date is after the date, minus one - equivalently, the end of the longest run
   * of periods, from the first, whose start dates are all on or before the date. The
   * implementation under test answers that same rule by a binary search over the prefix maxima of
   * the start dates, which is exact only because a prefix maximum is after a date exactly when
   * one of the dates it covers is; this reference relies on no such identity.
   *
   * @param stepUnderTest  the step to resolve
   * @param periods  the periods to resolve against, in the order they are held, non-empty
   * @return the outcome the indexed implementation has to match
   */
  private def referenceFindPreviousIndex(
      stepUnderTest: ValueStep,
      periods: List[SchedulePeriod]): FailureOr[Int] =
    stepUnderTest.date match {
      case None => Left(Failure.Invalid(NoDateHeld))
      case Some(stepDate) =>
        val firstPeriod = periods.head
        val lastPeriod = periods.last
        if (stepDate.isBefore(firstPeriod.unadjustedStartDate)) {
          Left(
            Failure.Invalid(
              "ValueStep date is before the start of the schedule: " +
                s"$stepDate < ${firstPeriod.unadjustedStartDate}"))
        } else {
          val laterStart =
            (1 until periods.size).find(index =>
              periods(index).unadjustedStartDate.isAfter(stepDate))
          laterStart match {
            case Some(index) => Right(index - 1)
            case None if stepDate.isAfter(lastPeriod.unadjustedEndDate) =>
              Left(
                Failure.Invalid(
                  "ValueStep date is after the end of the schedule: " +
                    s"$stepDate > ${lastPeriod.unadjustedEndDate}"))
            case None => Right(periods.size - 1)
          }
        }
    }

  /**
   * Asserts that the indexed resolution of the specified step agrees with the linear reference.
   *
   * Four answers are compared for every pair: the two questions asked of the index, and the same
   * two asked through the members that take the period list, which are written in terms of the
   * indexed ones. The comparison is of whole outcomes, so the condition reported and its message
   * are compared along with the index answered.
   *
   * @param periods  the periods to resolve against, in the order they are held
   * @param stepUnderTest  the step to resolve
   * @return the assertion that all four answers agree
   */
  private def assertResolutionAgrees(
      periods: NonEmptyList[SchedulePeriod],
      stepUnderTest: ValueStep): Assertion = {
    val index = ValueStep.PeriodIndex.of(periods)
    val periodList = periods.toList
    stepUnderTest.findIndex(index) shouldBe referenceFindIndex(stepUnderTest, periodList)
    stepUnderTest.findPreviousIndex(index) shouldBe
      referenceFindPreviousIndex(stepUnderTest, periodList)
    stepUnderTest.findIndex(periods) shouldBe stepUnderTest.findIndex(index)
    stepUnderTest.findPreviousIndex(periods) shouldBe stepUnderTest.findPreviousIndex(index)
  }

  /**
   * Asserts the agreement above over the specified periods, for a window of steps.
   *
   * The steps are every date in a window that runs from before the first period to well past the
   * last, so that the before-the-start and after-the-end rules are reached as well as the ones in
   * between, and every period index from one up to two past the end of the period list, so that
   * indices naming a period and indices beyond the last one are both among them. No step at index
   * zero is included, because no factory of the type builds one.
   *
   * @param periods  the periods to resolve against, in the order they are held
   * @return the assertion that every step agrees
   */
  private def assertEveryStepAgrees(periods: NonEmptyList[SchedulePeriod]): Assertion = {
    val dateSteps =
      (-4L to 30L).map(offset => ValueStep.of(IndexBaseDate.plusDays(offset), DeltaMinus2000))
    val indexSteps =
      (1 to periods.size + 2).map(index => step(ValueStep.of(index, DeltaMinus2000)))
    dateSteps.foreach(dateStep => assertResolutionAgrees(periods, dateStep))
    indexSteps.foreach(indexStep => assertResolutionAgrees(periods, indexStep))
    succeed
  }

  //-------------------------------------------------------------------------
  test("resolution_agreesWithLinearSearch_awkwardSchedules") {
    // Every list below is a synthetic input, handed to the index and the reference directly; the
    // shapes among them that no `Schedule` would hold are noted as they appear.

    // A list of one period, which is the degenerate case of the predecessor search: there is no
    // period after the first, so the vector of prefix maxima the search runs over is empty.
    assertEveryStepAgrees(NonEmptyList.of(periodAt(0L, 2L)))

    // Periods in descending order, which `Schedule.of` reports rather than holds - each period
    // ends after the next one starts. A binary search over the start dates themselves would
    // answer the middle rule wrongly on this input, where a search over their prefix maxima does
    // not.
    assertEveryStepAgrees(
      NonEmptyList.of(periodAt(10L, 12L), periodAt(5L, 7L), periodAt(0L, 2L)))

    // Repeated start dates, where the boundary-matching question has more than one candidate
    // answer and both the index and the reference answer with the earliest of those periods.
    assertEveryStepAgrees(
      NonEmptyList.of(periodAt(0L, 2L), periodAt(0L, 5L), periodAt(0L, 1L), periodAt(3L, 4L)))

    // Non-adjacent periods with gaps between them, which a `Schedule` does hold - a period
    // ending before the next one starts passes its chronology check - and which put dates inside
    // the span of the list that belong to no period at all.
    assertEveryStepAgrees(
      NonEmptyList.of(periodAt(0L, 2L), periodAt(6L, 8L), periodAt(20L, 22L)))

    // Overlapping periods, out of order, with a duplicate start among them: several of the
    // shapes above in one list, which a `Schedule` does not hold either.
    assertEveryStepAgrees(
      NonEmptyList.of(periodAt(6L, 12L), periodAt(0L, 8L), periodAt(6L, 7L), periodAt(2L, 3L)))

    // Adjusted starts colliding with unadjusted starts of other periods. The first period's
    // adjusted start is day 5, which is also the second period's unadjusted start, so day 5
    // resolves to the second period - the unadjusted starts are tried first - while day 2, the
    // adjusted start of the third period and the unadjusted start of none, resolves to the third.
    assertEveryStepAgrees(
      NonEmptyList.of(
        shiftedPeriodAt(0L, 2L, 5L),
        periodAt(5L, 7L),
        shiftedPeriodAt(10L, 12L, -8L)))
  }

  //-------------------------------------------------------------------------
  test("resolution_agreesWithLinearSearch_randomSchedules") {
    // Random period lists over a twenty-day window, narrow enough that duplicated start dates,
    // overlaps, gaps and unsorted order all occur often rather than as rarities, paired with a
    // random step positioned either by a date in and around that window or by an index within or
    // beyond the list. These lists are synthetic in the same way the hand-written ones above are,
    // and every pair asserts the same four-way agreement.
    val genPeriod: Gen[SchedulePeriod] =
      for {
        startOffset <- Gen.choose(0L, 20L)
        unadjustedLength <- Gen.choose(1L, 6L)
        adjustedShift <- Gen.choose(-2L, 2L)
        adjustedLength <- Gen.choose(1L, 6L)
      } yield period(
        SchedulePeriod.of(
          IndexBaseDate.plusDays(startOffset + adjustedShift),
          IndexBaseDate.plusDays(startOffset + adjustedShift + adjustedLength),
          IndexBaseDate.plusDays(startOffset),
          IndexBaseDate.plusDays(startOffset + unadjustedLength)))
    val genPeriods: Gen[NonEmptyList[SchedulePeriod]] =
      for {
        firstPeriod <- genPeriod
        restSize <- Gen.choose(0, 7)
        restPeriods <- Gen.listOfN(restSize, genPeriod)
      } yield NonEmptyList(firstPeriod, restPeriods)
    val genStep: Gen[ValueStep] =
      Gen.oneOf(
        Gen
          .choose(-4L, 30L)
          .map(offset => ValueStep.of(IndexBaseDate.plusDays(offset), DeltaMinus2000)),
        Gen.choose(1, 12).map(index => step(ValueStep.of(index, DeltaMinus2000))))

    forAll(genPeriods, genStep, minSuccessful(500)) {
      (periods: NonEmptyList[SchedulePeriod], stepUnderTest: ValueStep) =>
        assertResolutionAgrees(periods, stepUnderTest)
    }
  }
}
