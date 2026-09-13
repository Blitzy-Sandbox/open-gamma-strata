/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.value

import cats.Hash
import cats.Show
import cats.data.NonEmptyList

import io.circe.Json
import io.circe.parser.decode
import io.circe.parser.parse
import io.circe.syntax._

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.schedule.Frequency
import com.opengamma.strata.basics.schedule.RollConventions
import com.opengamma.strata.basics.schedule.Schedule
import com.opengamma.strata.basics.schedule.SchedulePeriod
import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[ValueSchedule]].
 *
 * ===Construction is validated here, and one of these tests is about where the line falls===
 *
 * Every factory of [[ValueSchedule]] hands back an outcome rather than a schedule, as every
 * validated type of this port does, so every fixture in this spec is unwrapped through the fold
 * helper below - and so are the collaborators, since a period, a schedule, an index-based step
 * and a sequence each report their own rejections.
 *
 * What construction judges is the half of the contradiction that needs no schedule: two steps
 * naming the '''same position as written''' - the same period index, or the same date - with
 * '''different''' adjustments. Steps naming one position with equal adjustments are accepted,
 * which is what `test_resolveValues_indexBased_duplicateDefinitionValid` shows, so it is the
 * difference and not the coincidence that construction refuses.
 * `test_resolveValues_indexBased_duplicateDefinitionInvalid` is the test where that line is
 * drawn, and it goes on to assert the part only resolution can decide: a step named by index and
 * a step named by the date of that period's boundary are two positions that differ as written and
 * coincide only once the periods are in hand.
 *
 * ===How the initial value is compared===
 *
 * The type compares its `initialValue` with `java.lang.Double.compare(a, b) == 0` and hashes it
 * with `java.lang.Double.hashCode`. Both canonicalise not-a-number values - every not-a-number
 * counts as one and the same value whatever its payload - while keeping a negative zero distinct
 * from a positive zero. Two consequences follow and are asserted in `coverage`: a schedule whose
 * initial value is not a number is equal to itself, and one holding -0.0 is not equal to one
 * holding 0.0.
 *
 * ===The three-period fixture is doing real work===
 *
 * The third period of the fixture has an adjusted start of 2014-03-01 and an '''unadjusted''' one
 * of 2014-03-02, which is what separates `test_resolveValues_dateBased` from
 * `test_resolveValues_dateBased_matchAdjusted`: a date-based step is matched against the
 * unadjusted period starts first and against the adjusted ones second, so the two tests reach the
 * same period through the two different passes.
 *
 * Four duties that touch this type are module-wide and are discharged by `SmartConstructorSpec`,
 * `FailableSurfaceSpec`, `ApiSurfaceSpec` and `json/JsonRoundTripSpec` rather than repeated here.
 */
final class ValueScheduleSpec extends AnyFunSuite with Matchers {

  //-------------------------------------------------------------------------
  /**
   * Reads the value out of an outcome that is expected to hold one.
   *
   * The factories of the collaborators used to build the fixtures below can reject their input
   * and report the rejection as a value, so a fixture built through one of them is an outcome
   * rather than a value. This is the one place this spec turns the first into the second, and it
   * does so by handling both sides: a fixture that fails is a defect in this spec and is reported
   * as one, naming the failures, rather than raising an error from a partial accessor that would
   * name nothing.
   *
   * One helper covers every fixture here because every one of them reports through the
   * accumulating channel, the schedule type itself included: its factories and its two `with`
   * operations all answer `ResultNec`, so a fixture of this type is unwrapped exactly as a
   * fixture of a collaborator is.
   *
   * @param result  the outcome of a factory, expected to hold a value
   * @tparam A  the type of the value the outcome holds
   * @return the value the outcome holds
   */
  private def ok[A](result: ResultNec[A]): A =
    result.fold(
      failures => fail(s"invalid fixture: ${failures.toChain.toList.map(_.message).mkString("; ")}"),
      held => held)

  /**
   * Parses one of the expected JSON forms below into the JSON model.
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
  /** The first step of the two-step fixtures below, replacing the value at 2014-06-30. */
  private val Step1: ValueStep = ValueStep.of(date(2014, 6, 30), ValueAdjustment.ofReplace(2000.0d))

  /** The second step of the two-step fixtures below, replacing the value at 2014-07-30. */
  private val Step2: ValueStep = ValueStep.of(date(2014, 7, 30), ValueAdjustment.ofReplace(3000.0d))

  //-------------------------------------------------------------------------
  /** The first period of the schedule fixture, whose dates need no adjustment. */
  private val Period1: SchedulePeriod = ok(SchedulePeriod.of(date(2014, 1, 1), date(2014, 2, 1)))

  /** The second period of the schedule fixture, whose dates need no adjustment. */
  private val Period2: SchedulePeriod = ok(SchedulePeriod.of(date(2014, 2, 1), date(2014, 3, 1)))

  /**
   * The third period of the schedule fixture, whose start is adjusted.
   *
   * The four arguments are in the order the factory declares them - start, end, unadjusted start,
   * unadjusted end - so this period starts on 2014-03-01 having been rolled from an unadjusted
   * 2014-03-02. That split is what makes the two date-based resolution tests below different from
   * one another, and getting the order backwards would make both of them assert the wrong thing
   * while still passing one of them.
   */
  private val Period3: SchedulePeriod =
    ok(SchedulePeriod.of(date(2014, 3, 1), date(2014, 4, 1), date(2014, 3, 2), date(2014, 4, 1)))

  /**
   * The schedule holding the three periods above, which every resolution test resolves against.
   *
   * Its factory takes the three properties of a schedule, and the periods are a non-empty list
   * rather than a list whose non-emptiness is checked, so the fixture states that fact in its
   * type.
   */
  private val ScheduleFixture: Schedule =
    ok(Schedule.of(NonEmptyList.of(Period1, Period2, Period3), Frequency.P1M, RollConventions.DAY_1))

  //-------------------------------------------------------------------------
  /**
   * The quarterly sequence used wherever a schedule holding a sequence is needed.
   *
   * It steps the value down by 100 every three months from 2016-04-20 to 2016-10-20. The dates
   * fall outside the schedule fixture above, which does not matter to the tests that use it: they
   * assert what a schedule '''holds''', not what it resolves to.
   */
  private val QuarterlySequence: ValueStepSequence =
    ok(
      ValueStepSequence
        .of(date(2016, 4, 20), date(2016, 10, 20), Frequency.P3M, ValueAdjustment.ofDeltaAmount(-100.0d)))

  /**
   * The monthly sequence the resolution tests for a sequence use.
   *
   * It steps the value up by 100 every month from 2014-02-01 to 2014-03-01, which are the
   * boundaries of the second and third periods of the schedule fixture, so it resolves to a step
   * at each of them.
   */
  private val MonthlySequence: ValueStepSequence =
    ok(
      ValueStepSequence
        .of(date(2014, 2, 1), date(2014, 3, 1), Frequency.P1M, ValueAdjustment.ofDeltaAmount(100.0d)))

  //-------------------------------------------------------------------------
  /**
   * The expected JSON of a schedule holding two steps and no sequence.
   *
   * The initial value is a JSON number, the steps are the array of the objects their own codec
   * writes, and there is no `stepSequence` field: the property is optional and a schedule that
   * does not hold one says nothing about it. The steps, by contrast, are a mandatory property and
   * are written even when there are none, which the second form below shows.
   */
  private val ExpectedStepsJson: String =
    """{"initialValue":10000.0,"steps":[""" +
      """{"date":"2014-06-30","value":{"modifyingValue":2000.0,"type":"Replace"}},""" +
      """{"date":"2014-07-30","value":{"modifyingValue":3000.0,"type":"Replace"}}]}"""

  /** The expected JSON of a schedule holding a sequence and no individual steps. */
  private val ExpectedSequenceJson: String =
    """{"initialValue":10000.0,"steps":[],"stepSequence":{""" +
      """"firstStepDate":"2016-04-20","lastStepDate":"2016-10-20","frequency":"P3M",""" +
      """"adjustment":{"modifyingValue":-100.0,"type":"DeltaAmount"}}}"""

  //-------------------------------------------------------------------------
  test("test_constant_ALWAYS_0") {
    // The constant schedule of zero, which holds no steps and no sequence, so resolving it
    // against any schedule of periods fills every period with its initial value.
    val test: ValueSchedule = ValueSchedule.ALWAYS_0
    test.initialValue shouldBe 0.0d
    test.steps shouldBe List.empty[ValueStep]
    test.stepSequence shouldBe None
  }

  test("test_constant_ALWAYS_1") {
    val test: ValueSchedule = ValueSchedule.ALWAYS_1
    test.initialValue shouldBe 1.0d
    test.steps shouldBe List.empty[ValueStep]
    test.stepSequence shouldBe None
  }

  //-------------------------------------------------------------------------
  test("test_of_int") {
    // The single-value factory, which is total: there is nothing about a value this type rejects.
    val test: ValueSchedule = ok(ValueSchedule.of(10000.0d))
    test.initialValue shouldBe 10000.0d
    test.steps shouldBe List.empty[ValueStep]
    test.stepSequence shouldBe None
  }

  test("test_of_intStepsArray") {
    // The varargs factory, which names its first step separately from the rest so that it cannot
    // be confused with the single-value factory above. The order of the steps is part of the
    // value - they are resolved in the order given - so it is asserted as an ordered list.
    val test: ValueSchedule = ok(ValueSchedule.of(10000.0d, Step1, Step2))
    test.initialValue shouldBe 10000.0d
    test.steps shouldBe List(Step1, Step2)
    test.stepSequence shouldBe None
  }

  test("test_of_intStepsArray_empty") {
    // Naming the first step separately is what keeps the varargs factory apart from the
    // single-value one, so that factory has no zero-step form: a schedule of no steps is
    // expressed through the list factory, and that route agrees with the single-value factory.
    val test: ValueSchedule = ok(ValueSchedule.of(10000.0d, List.empty[ValueStep]))
    test.initialValue shouldBe 10000.0d
    test.steps shouldBe List.empty[ValueStep]
    test.stepSequence shouldBe None
    test shouldBe ok(ValueSchedule.of(10000.0d))

    // The one-step varargs call, which is the shortest form that factory does have, so this test
    // still covers the varargs factory itself.
    val single: ValueSchedule = ok(ValueSchedule.of(10000.0d, Step1))
    single.steps shouldBe List(Step1)
  }

  test("test_of_intStepsList") {
    // The list factory. The list it takes is immutable, so the schedule holds the list it was
    // given rather than a defensive copy of it.
    val test: ValueSchedule = ok(ValueSchedule.of(10000.0d, List(Step1, Step2)))
    test.initialValue shouldBe 10000.0d
    test.steps shouldBe List(Step1, Step2)
    test.stepSequence shouldBe None

    // The same schedule reached through the varargs factory, which is what makes the two routes
    // into a stepped schedule one value rather than two that merely look alike.
    test shouldBe ok(ValueSchedule.of(10000.0d, Step1, Step2))
  }

  test("test_of_intStepsList_empty") {
    val test: ValueSchedule = ok(ValueSchedule.of(10000.0d, List.empty[ValueStep]))
    test.initialValue shouldBe 10000.0d
    test.steps shouldBe List.empty[ValueStep]
    test.stepSequence shouldBe None
  }

  test("test_of_sequence") {
    // The sequence factory, which holds the sequence and no individual steps. The sequence is an
    // optional property, so one that is held reads back as `Some`.
    val test: ValueSchedule = ok(ValueSchedule.of(10000.0d, QuarterlySequence))
    test.initialValue shouldBe 10000.0d
    test.steps shouldBe List.empty[ValueStep]
    test.stepSequence shouldBe Some(QuarterlySequence)
  }

  test("test_builder_validEmpty") {
    // The full-field factory, handed the values a schedule that carries nothing holds: an initial
    // value of zero, an empty list of steps and no sequence.
    val test: ValueSchedule = ok(ValueSchedule.of(0.0d, List.empty[ValueStep], None))
    test.initialValue shouldBe 0.0d
    test.steps shouldBe List.empty[ValueStep]
    test.stepSequence shouldBe None

    // The same value as the two shorter routes to a schedule of zero, so all three routes supply
    // one set of defaults.
    test shouldBe ok(ValueSchedule.of(0.0d))
    test shouldBe ValueSchedule.ALWAYS_0
  }

  test("test_builder_validFull") {
    // The full-field factory, which is the one call that takes individual steps and a sequence at
    // once, and the factory the two resolution tests for a sequence alongside a step use as well.
    val test: ValueSchedule = ok(ValueSchedule.of(2000.0d, List(Step1, Step2), Some(QuarterlySequence)))
    test.initialValue shouldBe 2000.0d
    test.steps shouldBe List(Step1, Step2)
    test.stepSequence shouldBe Some(QuarterlySequence)

    // The two `with` operations reach the same value from either half of it, so a schedule
    // carrying both can be assembled field by field as well as in the one call above.
    ok(ok(ValueSchedule.of(2000.0d, List(Step1, Step2))).withStepSequence(QuarterlySequence)) shouldBe
      test
    ok(ok(ValueSchedule.of(2000.0d, QuarterlySequence)).withSteps(List(Step1, Step2))) shouldBe test
  }

  //-------------------------------------------------------------------------
  test("test_resolveValues_dateBased") {
    // Two steps positioned by date. The first names the boundary the second period starts on,
    // which is an unadjusted start of the fixture; the second names 2014-03-01, which is the
    // '''adjusted''' start of the third period and is reached by the second matching pass.
    val step1: ValueStep = ValueStep.of(date(2014, 2, 1), ValueAdjustment.ofReplace(300.0d))
    val step2: ValueStep = ValueStep.of(date(2014, 3, 1), ValueAdjustment.ofReplace(400.0d))

    // no steps: every period carries the initial value
    ok(ValueSchedule.of(200.0d, List.empty[ValueStep])).resolveValues(ScheduleFixture) should
      haveValue(DoubleArray.of(200.0d, 200.0d, 200.0d))

    // step1: the change at the second period carries forward into the third
    ok(ValueSchedule.of(200.0d, List(step1))).resolveValues(ScheduleFixture) should
      haveValue(DoubleArray.of(200.0d, 300.0d, 300.0d))

    // step2: the first two periods are untouched
    ok(ValueSchedule.of(200.0d, List(step2))).resolveValues(ScheduleFixture) should
      haveValue(DoubleArray.of(200.0d, 200.0d, 400.0d))

    // step1 and step2: one change at each of the two later periods
    ok(ValueSchedule.of(200.0d, List(step1, step2))).resolveValues(ScheduleFixture) should
      haveValue(DoubleArray.of(200.0d, 300.0d, 400.0d))
  }

  test("test_resolveValues_dateBased_matchAdjusted") {
    // The same four scenarios, with the second step moved onto 2014-03-02 - the '''unadjusted'''
    // start of the third period, which the first matching pass finds. Both this test and the one
    // above therefore resolve their second step to the third period, by the two different passes,
    // and produce the same four vectors.
    val step1: ValueStep = ValueStep.of(date(2014, 2, 1), ValueAdjustment.ofReplace(300.0d))
    val step2: ValueStep = ValueStep.of(date(2014, 3, 2), ValueAdjustment.ofReplace(400.0d))

    // The four dates of the third period, pinned here because this test and the one above are
    // otherwise indistinguishable: they differ only in which of the two matching passes reaches
    // the third period, so an adjusted and unadjusted start swapped round would leave both of
    // them passing while asserting the opposite of what they say.
    Period3.startDate shouldBe date(2014, 3, 1)
    Period3.endDate shouldBe date(2014, 4, 1)
    Period3.unadjustedStartDate shouldBe date(2014, 3, 2)
    Period3.unadjustedEndDate shouldBe date(2014, 4, 1)

    // no steps
    ok(ValueSchedule.of(200.0d, List.empty[ValueStep])).resolveValues(ScheduleFixture) should
      haveValue(DoubleArray.of(200.0d, 200.0d, 200.0d))

    // step1
    ok(ValueSchedule.of(200.0d, List(step1))).resolveValues(ScheduleFixture) should
      haveValue(DoubleArray.of(200.0d, 300.0d, 300.0d))

    // step2
    ok(ValueSchedule.of(200.0d, List(step2))).resolveValues(ScheduleFixture) should
      haveValue(DoubleArray.of(200.0d, 200.0d, 400.0d))

    // step1 and step2
    ok(ValueSchedule.of(200.0d, List(step1, step2))).resolveValues(ScheduleFixture) should
      haveValue(DoubleArray.of(200.0d, 300.0d, 400.0d))
  }

  test("test_resolveValues_dateBased_ignoreExcess") {
    // Five steps, three of which name a date that is not a period boundary at all. A step like
    // that is tolerated exactly when applying its adjustment to the value of the period its date
    // falls in would change nothing, which is the case for all three here: a replacement by the
    // value the period already carries, a delta of zero and a multiplier of one. The remaining
    // two steps are the ones that do the work.
    val step1: ValueStep = ValueStep.of(date(2014, 2, 1), ValueAdjustment.ofReplace(300.0d))
    val step2: ValueStep = ValueStep.of(date(2014, 2, 15), ValueAdjustment.ofReplace(300.0d))
    val step3: ValueStep = ValueStep.of(date(2014, 3, 1), ValueAdjustment.ofReplace(400.0d))
    val step4: ValueStep = ValueStep.of(date(2014, 3, 15), ValueAdjustment.ofDeltaAmount(0.0d))
    val step5: ValueStep = ValueStep.of(date(2014, 4, 1), ValueAdjustment.ofMultiplier(1.0d))

    val test: ValueSchedule = ok(ValueSchedule.of(200.0d, List(step1, step2, step3, step4, step5)))
    test.resolveValues(ScheduleFixture) should haveValue(DoubleArray.of(200.0d, 300.0d, 400.0d))
  }

  //-------------------------------------------------------------------------
  test("test_resolveValues_indexBased") {
    // The same four scenarios with the steps positioned by period index rather than by date. The
    // index factory reports its outcome, because an index must be one or greater - zero would
    // name the first period, which the initial value of the definition already gives a value - so
    // the two fixtures pass through the fold helper.
    val step1: ValueStep = ok(ValueStep.of(1, ValueAdjustment.ofReplace(300.0d)))
    val step2: ValueStep = ok(ValueStep.of(2, ValueAdjustment.ofReplace(400.0d)))

    // no steps
    ok(ValueSchedule.of(200.0d, List.empty[ValueStep])).resolveValues(ScheduleFixture) should
      haveValue(DoubleArray.of(200.0d, 200.0d, 200.0d))

    // step1
    ok(ValueSchedule.of(200.0d, List(step1))).resolveValues(ScheduleFixture) should
      haveValue(DoubleArray.of(200.0d, 300.0d, 300.0d))

    // step2
    ok(ValueSchedule.of(200.0d, List(step2))).resolveValues(ScheduleFixture) should
      haveValue(DoubleArray.of(200.0d, 200.0d, 400.0d))

    // step1 and step2
    ok(ValueSchedule.of(200.0d, List(step1, step2))).resolveValues(ScheduleFixture) should
      haveValue(DoubleArray.of(200.0d, 300.0d, 400.0d))
  }

  test("test_resolveValues_indexBased_duplicateDefinitionValid") {
    // Two steps naming the same period index with the '''same''' adjustment. That is not a
    // contradiction - the period changes once, to the value both steps ask for - so construction
    // accepts the definition and it resolves.
    val step1: ValueStep = ok(ValueStep.of(1, ValueAdjustment.ofReplace(300.0d)))
    val step2: ValueStep = ok(ValueStep.of(1, ValueAdjustment.ofReplace(300.0d)))

    val test: ValueSchedule = ok(ValueSchedule.of(200.0d, List(step1, step2)))
    test.resolveValues(ScheduleFixture) should haveValue(DoubleArray.of(200.0d, 300.0d, 300.0d))
  }

  test("test_resolveValues_indexBased_duplicateDefinitionInvalid") {
    // Two steps naming the same period index with '''different''' adjustments, which is the case
    // construction refuses: the two ask for different values at one point of the time line
    // whatever schedule they are resolved against. Equal adjustments at one position are accepted,
    // as the test above shows, so it is the difference that makes this pair a contradiction. The
    // report is the one failure of that position, carrying both adjustments.
    val step1: ValueStep = ok(ValueStep.of(1, ValueAdjustment.ofReplace(300.0d)))
    val step2: ValueStep = ok(ValueStep.of(1, ValueAdjustment.ofReplace(400.0d)))

    val rejected: ResultNec[ValueSchedule] = ValueSchedule.of(200.0d, List(step1, step2))
    rejected should beFailureWith(FailureReason.INVALID)
    rejected.left.toOption.map(_.length) shouldBe Some(1L)
    rejected should haveFailureMessageMatching(
      ".*two steps name period index 1 with different adjustments.*")

    // Every other route into the type reports it too, since all of them funnel through the
    // factory above: the varargs shorthand, the full-field factory, and the operation that
    // replaces the steps of a definition already built.
    ValueSchedule.of(200.0d, step1, step2) should beFailureWith(FailureReason.INVALID)
    ValueSchedule.of(200.0d, List(step1, step2), None) should beFailureWith(FailureReason.INVALID)
    ok(ValueSchedule.of(200.0d)).withSteps(List(step1, step2)) should
      beFailureWith(FailureReason.INVALID)

    // The half of the contradiction that construction cannot see is still reported by resolution,
    // and this is it: one step names period 1 by its index and the other names it by the date of
    // that period's boundary, with a different adjustment, so the two positions differ as written
    // and coincide only once the periods are in hand.
    val byDate: ValueStep =
      ValueStep.of(Period2.unadjustedStartDate, ValueAdjustment.ofReplace(400.0d))
    val test: ValueSchedule = ok(ValueSchedule.of(200.0d, List(step1, byDate)))
    test.initialValue shouldBe 200.0d
    test.steps shouldBe List(step1, byDate)

    val result: FailureOr[DoubleArray] = test.resolveValues(ScheduleFixture)
    result should beFailure
    result should beFailureWith(FailureReason.INVALID)
    result should haveFailureMessageMatching(".*two steps resolved to the same schedule period.*")

    // The check that reports the contradiction above runs on '''every''' construction, and both
    // the factory and the decoder take a step list of any length from a caller or a document, so
    // what that check costs is part of this type's contract. It is linear in the number of steps
    // plus the sort of their distinct positions: each step is paired with its own position in the
    // list before the steps are grouped, so ordering the reports reads an index already in hand
    // rather than searching the list again for each group. This case exercises that check at
    // sixty thousand distinct positions, and the definition it builds holds every one of them.
    val manySteps: List[ValueStep] =
      List.tabulate(60000)(index => ok(ValueStep.of(index + 1, ValueAdjustment.ofReplace(index.toDouble))))
    val ofManySteps: ResultNec[ValueSchedule] = ValueSchedule.of(100.0d, manySteps)
    ofManySteps.map(schedule => schedule.steps.size) shouldBe Right(60000)
  }

  test("test_resolveValues_dateBased_indexZeroValid") {
    // A step on the first boundary of the schedule adjusts the initial value itself, which is why
    // a step at index zero is meaningful even though no period precedes it.
    val step: ValueStep = ValueStep.of(date(2014, 1, 1), ValueAdjustment.ofReplace(300.0d))

    val test: ValueSchedule = ok(ValueSchedule.of(200.0d, List(step)))
    test.resolveValues(ScheduleFixture) should haveValue(DoubleArray.of(300.0d, 300.0d, 300.0d))
  }

  test("test_resolveValues_indexBased_indexTooBig") {
    // An index of three names a fourth period, which this schedule of three does not have. The
    // step itself is a perfectly good step - the index is positive - so the failure belongs to
    // the pairing of the definition with the schedule and is reported by resolution.
    val step: ValueStep = ok(ValueStep.of(3, ValueAdjustment.ofReplace(300.0d)))

    val result: FailureOr[DoubleArray] = ok(ValueSchedule.of(200.0d, List(step))).resolveValues(ScheduleFixture)
    result should beFailure
    result should beFailureWith(FailureReason.INVALID)
    result should haveFailureMessageMatching(".*index is beyond last schedule period.*")
  }

  test("test_resolveValues_dateBased_invalidChangeValue") {
    // A step on 2014-04-01, which is the end of the schedule rather than the start of any period,
    // and which replaces the value of the last period with a different one. Unlike the three
    // no-op steps of the excess test above, this one would change a value where it falls, so it
    // is reported. The matcher compares the whole message, so the pattern is anchored at its
    // start and closed with a wildcard tail.
    val step: ValueStep = ValueStep.of(date(2014, 4, 1), ValueAdjustment.ofReplace(300.0d))

    val result: FailureOr[DoubleArray] = ok(ValueSchedule.of(200.0d, List(step))).resolveValues(ScheduleFixture)
    result should beFailure
    result should beFailureWith(FailureReason.INVALID)
    result should haveFailureMessageMatching("^ValueStep date does not match a period boundary.*")
  }

  test("test_resolveValues_dateBased_invalidDateBefore") {
    // A step dated before the first period of the schedule has no preceding period to adjust.
    val step: ValueStep = ValueStep.of(date(2013, 12, 31), ValueAdjustment.ofReplace(300.0d))

    val result: FailureOr[DoubleArray] = ok(ValueSchedule.of(200.0d, List(step))).resolveValues(ScheduleFixture)
    result should beFailure
    result should beFailureWith(FailureReason.INVALID)
    result should haveFailureMessageMatching("^ValueStep date is before the start of the schedule.*")
  }

  test("test_resolveValues_dateBased_invalidDateAfter") {
    // A step dated after the last period of the schedule ends is off the end of it. The date one
    // day earlier - the end of the schedule itself - is the case of the invalid-change test
    // above, which is reported for the other reason.
    val step: ValueStep = ValueStep.of(date(2014, 4, 3), ValueAdjustment.ofReplace(300.0d))

    val result: FailureOr[DoubleArray] = ok(ValueSchedule.of(200.0d, List(step))).resolveValues(ScheduleFixture)
    result should beFailure
    result should beFailureWith(FailureReason.INVALID)
    result should haveFailureMessageMatching("^ValueStep date is after the end of the schedule.*")
  }

  //-------------------------------------------------------------------------
  test("test_resolveValues_sequence") {
    // A sequence stepping the value up by 100 at each of the two later boundaries of the
    // schedule, expanded under its roll convention into the two steps those boundaries carry.
    val test: ValueSchedule = ok(ValueSchedule.of(200.0d, MonthlySequence))
    test.stepSequence shouldBe Some(MonthlySequence)
    test.resolveValues(ScheduleFixture) should haveValue(DoubleArray.of(200.0d, 300.0d, 400.0d))
  }

  test("test_resolveValues_sequenceAndSteps") {
    // The same sequence alongside an individual step on the first boundary. The individual step
    // replaces the initial value and the two sequence steps then add to it, so every period is a
    // hundred and fifty above the sequence-only case above.
    val step1: ValueStep = ValueStep.of(date(2014, 1, 1), ValueAdjustment.ofReplace(350.0d))

    val test: ValueSchedule = ok(ValueSchedule.of(200.0d, List(step1), Some(MonthlySequence)))
    test.resolveValues(ScheduleFixture) should haveValue(DoubleArray.of(350.0d, 450.0d, 550.0d))
  }

  test("test_resolveValues_sequenceAndStepClash") {
    // The same sequence alongside an individual step on a boundary the sequence also names, with
    // a '''different''' adjustment. The two resolve to the same period and ask it for different
    // values, which is the contradiction resolution reports. Two steps resolving to one period
    // with equal adjustments are accepted, so it is the difference between the adjustments and
    // not the coincidence of the dates that makes this pair a failure.
    val step1: ValueStep = ValueStep.of(date(2014, 2, 1), ValueAdjustment.ofReplace(350.0d))

    val test: ValueSchedule = ok(ValueSchedule.of(200.0d, List(step1), Some(MonthlySequence)))
    val result: FailureOr[DoubleArray] = test.resolveValues(ScheduleFixture)
    result should beFailure
    result should beFailureWith(FailureReason.INVALID)
    result should haveFailureMessageMatching(".*two steps resolved to the same schedule period.*")
  }

  //-------------------------------------------------------------------------
  test("equals") {
    // The equality matrix, which varies one property at a time: the same definition twice, a
    // different initial value, and a shorter list of steps.
    val a1: ValueSchedule = ok(ValueSchedule.of(10000.0d, List(Step1, Step2)))
    val a2: ValueSchedule = ok(ValueSchedule.of(10000.0d, List(Step1, Step2)))
    val b: ValueSchedule = ok(ValueSchedule.of(5000.0d, List(Step1, Step2)))
    val c: ValueSchedule = ok(ValueSchedule.of(10000.0d, List(Step1)))

    a1 shouldBe a1
    a1 shouldBe a2
    a1 should not be b
    a1 should not be c
    a1 should not be ""
    a1.hashCode shouldBe a2.hashCode

    // The same relations through the typeclass instance, which is the equality every generic
    // caller of this type sees and which is derived from the two members asserted above rather
    // than defined a second time.
    Hash[ValueSchedule].eqv(a1, a1) shouldBe true
    Hash[ValueSchedule].eqv(a1, a2) shouldBe true
    Hash[ValueSchedule].eqv(a1, b) shouldBe false
    Hash[ValueSchedule].eqv(a1, c) shouldBe false
    Hash[ValueSchedule].hash(a1) shouldBe Hash[ValueSchedule].hash(a2)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // Two definitions differing in every property, which is what makes them a pair worth
    // comparing.
    val test: ValueSchedule = ok(ValueSchedule.of(10000.0d, List(Step1, Step2)))
    val that: ValueSchedule = ok(ValueSchedule.of(20000.0d, QuarterlySequence))

    // Every property of both, read back through the accessors of the type.
    test.initialValue shouldBe 10000.0d
    test.steps shouldBe List(Step1, Step2)
    test.stepSequence shouldBe None
    that.initialValue shouldBe 20000.0d
    that.steps shouldBe List.empty[ValueStep]
    that.stepSequence shouldBe Some(QuarterlySequence)

    // Two unrelated definitions are unequal under both the universal equality of the type and
    // its typeclass instance, and the typeclass rendering is the rendering of the type itself.
    test should not be that
    Hash[ValueSchedule].eqv(test, that) shouldBe false
    Show[ValueSchedule].show(test) shouldBe test.toString

    // The rendering names the properties that are present, and only those: a definition without
    // a sequence says nothing about one, while a definition with a sequence names it.
    test.toString should startWith("ValueSchedule{")
    test.toString should include("initialValue=10000.0")
    test.toString should include("steps=")
    test.toString should not include "stepSequence="
    that.toString should include("stepSequence=")

    // The two `with` operations produce the expected modified value and leave the original
    // untouched, which is the whole of the field-wise modification this type has.
    // Both report their outcome, because both hand the caller's steps to the validated factory,
    // so both are unwrapped here exactly as a factory call is.
    ok(test.withSteps(List(Step1))) shouldBe ok(ValueSchedule.of(10000.0d, List(Step1)))
    ok(test.withStepSequence(QuarterlySequence)) shouldBe
      ok(ValueSchedule.of(10000.0d, List(Step1, Step2), Some(QuarterlySequence)))
    test.steps shouldBe List(Step1, Step2)
    test.stepSequence shouldBe None

    // The construction surface is closed: the type is an abstract case class with a private
    // constructor, so it has neither a public `apply` nor a `copy`, and the only way to assert
    // the absence of a member is to require a snippet naming it to fail to compile. The second
    // snippet names a schedule this test already holds rather than a factory call, so what fails
    // to compile in it is `copy` on the type and not the absence of `copy` on the outcome a
    // factory answers with.
    assertDoesNotCompile("""ValueSchedule(10000.0d, Nil, None)""")
    assertDoesNotCompile("""test.copy(initialValue = 5000.0d)""")

    // The same two snippets with the missing member replaced by one that exists, which is what
    // keeps the two above from passing for the wrong reason: a snippet naming an identifier this
    // scope cannot resolve would also fail to compile and would prove nothing about `apply` or
    // `copy`. These compile, so every other name in them resolves.
    assertCompiles("""ValueSchedule.of(10000.0d, Nil, None)""")
    assertCompiles("""test.initialValue""")

    // The initial value is compared with `java.lang.Double.compare` and hashed with
    // `java.lang.Double.hashCode`, as it is by every double-bearing type of this port. Both
    // canonicalise not-a-number values, so a definition whose initial value is not a number is
    // equal to itself whatever the payload of that value, and both keep a negative zero distinct
    // from a positive zero. The signed-zero consequence is stated against the platform comparison
    // that decides it rather than against a numeric test that would report the two zeroes equal.
    ok(ValueSchedule.of(Double.NaN)) shouldBe ok(ValueSchedule.of(Double.NaN))
    java.lang.Double.compare(-0.0d, 0.0d) should not be 0
    ok(ValueSchedule.of(-0.0d)) should not be ok(ValueSchedule.of(0.0d))
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // The round trip through the JSON codec. Both directions are asserted - an encoding that is
    // wrong and a decoding that is wrong in the same way would still round trip - and the
    // documents are compared as parsed JSON rather than as printed text, so the assertion is
    // about the fields rather than the rendering.
    val test: ValueSchedule = ok(ValueSchedule.of(10000.0d, List(Step1, Step2)))
    test.asJson shouldBe json(ExpectedStepsJson)
    decode[ValueSchedule](test.asJson.noSpaces) shouldBe Right(test)
    decode[ValueSchedule](ExpectedStepsJson) shouldBe Right(test)

    // The sequence is an optional property and is dropped from the document of a definition that
    // holds none, rather than written as an explicitly empty field; the steps are a mandatory
    // property and are always written. The key set states both facts, in declaration order.
    test.asJson.asObject.map(_.keys.toList) shouldBe Some(List("initialValue", "steps"))
    test.asJson.hcursor.downField("stepSequence").succeeded shouldBe false
    test.asJson.hcursor.downField("initialValue").as[Double] shouldBe Right(10000.0d)

    // The other shape, which holds a sequence and no individual steps, so the array of steps is
    // written as an empty one and the sequence appears as the object its own codec writes.
    val withSequence: ValueSchedule = ok(ValueSchedule.of(10000.0d, QuarterlySequence))
    withSequence.asJson shouldBe json(ExpectedSequenceJson)
    decode[ValueSchedule](withSequence.asJson.noSpaces) shouldBe Right(withSequence)
    decode[ValueSchedule](ExpectedSequenceJson) shouldBe Right(withSequence)
    withSequence.asJson.asObject.map(_.keys.toList) shouldBe
      Some(List("initialValue", "steps", "stepSequence"))

    // A document that omits the array of steps reads as a definition holding none, which is what
    // makes the shorthand of a constant schedule decodable, and equal values encode to identical
    // bytes because the fields are written in declaration order and the order of the steps is
    // part of the value.
    decode[ValueSchedule]("""{"initialValue":200.0}""") shouldBe Right(ok(ValueSchedule.of(200.0d)))
    ok(ValueSchedule.of(200.0d, List(Step1, Step2))).asJson.noSpaces shouldBe
      ok(ValueSchedule.of(200.0d, Step1, Step2)).asJson.noSpaces
  }
}
