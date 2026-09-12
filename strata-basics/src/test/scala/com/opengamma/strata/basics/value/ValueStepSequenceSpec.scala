/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.value

import java.time.LocalDate

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
 * Test [[ValueStepSequence]].
 *
 * This is a one-to-one port of the Java test class `ValueStepSequenceTest`: each of its seven
 * test methods has a test of the same name here, in the same order, and no test is split or
 * renamed. Every date, frequency and adjustment the Java test named is named here too, as a
 * literal rather than as something recomputed, because the whole value of a ported test is that
 * it agrees with the implementation it was written against.
 *
 * ===The six tests that are not ports===
 *
 * `test_resolve_atMaximumSteps` and `test_resolve_beyondMaximumSteps` have no Java counterpart
 * because the behaviour they cover has none: [[ValueStepSequence.resolve]] bounds its expansion
 * at [[ValueStepSequence.MaximumStepCount]] steps, where the original expanded whatever span its
 * dates and frequency described. They are written as a pair straddling the ceiling by one step,
 * which is what makes them evidence of both halves of that bound - that a sequence filling the
 * ceiling still resolves, and that the one step past it is reported rather than expanded.
 *
 * The four that follow them cover the other behaviour the original did not have: the date
 * arithmetic of a resolution is '''guarded''', so a sequence whose dates sit at the edge of the
 * range `java.time` represents is answered rather than raised out of. They are written as a set
 * of four because the edge is reached in four distinguishable ways, and only two of them are
 * failures - `test_resolve_equalDatesAtMaximumDate` resolves a sequence whose two dates are both
 * the last date the calendar holds, which is the case a walk must not ask for a successor in;
 * `test_resolve_dateRangeOverflow` covers the two that genuinely need a date past the end of the
 * range, one in a rolling step and one in the adjustment of an endpoint;
 * `test_resolve_nearMinimumDate` covers the other end of the range, which fails nothing because
 * the walk only moves forward; and `test_resolve_dateRangeOverflow_throughValueSchedule` asserts
 * the same outcomes on the public path a caller outside this package reaches resolution through.
 *
 * ===What the port changes, and why===
 *
 * Three of the seven methods asserted behaviour this port does not have, and each keeps its name
 * while asserting what replaced it, so nothing the Java test covered is silently dropped:
 *
 *   - `test_of_invalid` and `test_resolve_invalid` asserted that an `IllegalArgumentException`
 *     was raised. Construction and resolution report their outcome as a value here, so each of
 *     the three Java `assertThatIllegalArgumentException` sites is written as an assertion about
 *     a failure: the reason compared as a member of the closed family of reasons, and the message
 *     pinned both by pattern and word for word against the Java text. None of the three is a
 *     caller-contract precondition - the order of two dates, the type of an adjustment and
 *     whether a frequency divides a span all depend on the data supplied - so no exception is
 *     expected anywhere in this spec and none is intercepted. The date-range tests below make
 *     that claim an assertion rather than an omission: they state `noException should be thrownBy`
 *     over the very calls the ported implementation raised from.
 *   - `coverage` called `coverImmutableBean`, a reflective sweep over the properties of a bean
 *     through its meta-bean. There is no meta-bean and no reflective property access here, so its
 *     substance is asserted directly on the two instances the Java test swept: the accessors, the
 *     equality and hashing relation between them, the rendering, and the closed construction
 *     surface of a validated type - proved by requiring two snippets to fail to compile rather
 *     than by asserting something about one instance.
 *   - `test_serialization` asserted a Java serialization round trip, which is not part of this
 *     port at all. The JSON codec takes its place, so the round trip asserted is
 *     `decode(encode(x)) == x` in both directions together with the exact shape of the encoding,
 *     and with two documents the decoder has to reject because the validating factory rejects
 *     what they describe.
 *
 * ===Resolution is reached directly, as it was in Java===
 *
 * `resolve` is visible within this package rather than publicly, exactly as the ported method
 * was package-private, and eight of the thirteen tests call it directly - a ninth reaches it
 * through the public [[ValueSchedule.resolveValues]], which is the only route a caller outside
 * this package has. That is legal from here because a
 * Scala access qualifier names a package rather than a compilation unit or a module: this spec
 * declares the package the type declares, so the member is in scope. Nothing is added to the
 * production type to make these tests possible, and nothing is reached by reflection.
 *
 * ===What is asserted elsewhere===
 *
 * Four duties that touch this type are module-wide and are discharged by module-wide specs
 * rather than repeated here: the sweep of every validated type's invalid inputs and their
 * accumulation (`SmartConstructorSpec`), the inventory of failure-returning methods
 * (`FailableSurfaceSpec`), the proof that no validated type has a public `apply` or `copy`
 * (`ApiSurfaceSpec`), and the property-based codec round trip over every codec-bearing type
 * (`json/JsonRoundTripSpec`). This spec stays with the seven ported methods and the six cases
 * that cover what the port added to `resolve`.
 */
final class ValueStepSequenceSpec extends AnyFunSuite with Matchers {

  /** The relative adjustment of the Java fixture `ADJ`, which varies the value at each step. */
  private val Adj: ValueAdjustment = ValueAdjustment.ofDeltaAmount(-100.0d)

  /** The second relative adjustment of the Java fixture `ADJ2`, used by the coverage instance. */
  private val Adj2: ValueAdjustment = ValueAdjustment.ofDeltaAmount(-200.0d)

  /**
   * The replacing adjustment of the Java fixture `ADJ_BAD`.
   *
   * A step that replaces the value discards whatever the previous step produced, so repeating
   * one at every step of a sequence expresses nothing a single [[ValueStep]] does not already
   * express. That is why the type rejects it, and this is the input the rejection is asserted
   * with.
   */
  private val AdjBad: ValueAdjustment = ValueAdjustment.ofReplace(100.0d)

  //-------------------------------------------------------------------------
  /**
   * The message reported when the two dates are in the wrong order.
   *
   * The three messages below are transcribed from the Java implementation - the first two from
   * the checks of its `@ImmutableValidator`, the third from the template its `resolve` raised -
   * and are asserted as literals as well as by pattern, so a change to the wording of the port
   * cannot pass unnoticed and a reader comparing the two implementations can see that they
   * agree.
   */
  private val InvalidDateOrder: String =
    "Invalid order: Expected 'firstStepDate' <= 'lastStepDate', but found: '2016-04-20' > '2016-04-19'"

  /** The message reported for an adjustment that replaces the value, in the words of the bean. */
  private val ReplacementNotAllowed: String = "ValueAdjustmentType must not be 'Replace'"

  /**
   * The message reported when the frequency does not reach the last date under the convention.
   *
   * The four values it names are those of the `test_resolve_invalid` case below: a twelve month
   * frequency over a six month span leaves the walk on the first date, which is not the last
   * date of the sequence.
   */
  private val FrequencyMismatch: String =
    "ValueStepSequence lastStepDate did not match frequency 'P12M' using roll convention 'None', " +
      "2016-10-20 != 2016-04-20"

  //-------------------------------------------------------------------------
  /**
   * The first date of the two bounded-expansion cases below.
   *
   * The pair of them straddles the ceiling by one step: a daily frequency from this date reaches
   * [[LastDateAtCeiling]] on its hundred-thousandth step and [[LastDateBeyondCeiling]] on the one
   * after that, so the two cases differ in exactly the one step that decides the outcome.
   */
  private val CeilingFirstDate: LocalDate = date(2016, 4, 20)

  /**
   * The last date of a daily sequence of exactly [[ValueStepSequence.MaximumStepCount]] steps.
   *
   * This is `2016-04-20` plus one day short of the ceiling, so the walk lands on it having
   * produced exactly as many steps as the ceiling allows.
   */
  private val LastDateAtCeiling: LocalDate = date(2290, 2, 2)

  /** The last date of a daily sequence of one step more than the ceiling allows. */
  private val LastDateBeyondCeiling: LocalDate = date(2290, 2, 3)

  /**
   * The message reported when the dates and frequency describe more steps than the ceiling.
   *
   * This message belongs to the port rather than to the Java original, which expanded whatever
   * span it was given, so it is transcribed from the implementation for the same reason the
   * ported messages are: to keep a change to its wording from passing unnoticed. It names the
   * limit as well as the two adjusted dates and the frequency, because the limit is the part a
   * caller cannot work out from its own arguments.
   */
  private val ExpansionBeyondCeiling: String =
    "ValueStepSequence frequency 'P1D' from 2016-04-20 to 2290-02-03 using roll convention " +
      "'None' expands to more than the maximum of 100000 steps"

  //-------------------------------------------------------------------------
  /**
   * The message reported when the arithmetic of a resolution leaves the range of dates.
   *
   * This message belongs to the port rather than to the Java original, which performed the same
   * arithmetic unguarded and let `java.time` raise out of a method that otherwise reported its
   * outcome as a value. It names the frequency, which is what a rolling step adds, and the
   * convention, which is what adjusts the result and can itself move a date off the end of the
   * range; it names neither date, because either of them can be the one at fault.
   *
   * This first form is that of the rolling-step case below, a monthly frequency under the
   * convention that adjusts nothing.
   */
  private val MonthlyOverflow: String =
    "ValueStepSequence frequency 'P1M' using roll convention 'None' moved outside the range of " +
      "supported dates"

  /**
   * The same message for the endpoint-adjustment case below, a day-of-week convention.
   *
   * `LocalDate.MAX` is a Friday, so the 'DayMon' convention - which moves a date '''forward''' to
   * the next Monday - cannot adjust it without leaving the range. That is the second way a
   * resolution reaches the edge of the calendar, and it is reached before the walk begins.
   */
  private val DailyMondayOverflow: String =
    "ValueStepSequence frequency 'P1D' using roll convention 'DayMon' moved outside the range of " +
      "supported dates"

  /**
   * The schedule the public-path case below resolves a value schedule against.
   *
   * One period of January 2014 under the convention that adjusts nothing is enough: what that
   * case is about is which channel the outcome comes back through, and the sequences it resolves
   * name dates at the end of the calendar, so no period of any plausible schedule contains them.
   * The convention matters and is deliberately 'None': the sequence is expanded under the
   * convention of the '''schedule''', and a day-of-month convention would pull the dates of the
   * sequence back to the first of the month and so resolve arithmetic that is the point of the
   * case.
   */
  private val ScheduleFixture: Schedule =
    ok(
      Schedule.of(
        NonEmptyList.of(ok(SchedulePeriod.of(date(2014, 1, 1), date(2014, 2, 1)))),
        Frequency.P1M,
        RollConventions.NONE))

  //-------------------------------------------------------------------------
  /**
   * The expected JSON of the sequence the Java test serialized.
   *
   * The document holds the four properties under the names the Java bean declared and in its
   * declaration order: the dates in their ISO form, the frequency as the bare name its own codec
   * writes, and the adjustment as the object its own codec writes.
   */
  private val ExpectedJson: String =
    """{"firstStepDate":"2016-04-20","lastStepDate":"2016-10-20","frequency":"P3M",""" +
      """"adjustment":{"modifyingValue":-100.0,"type":"DeltaAmount"}}"""

  /** A document whose dates are in the wrong order, which the decoder has to reject. */
  private val ReversedDatesJson: String =
    """{"firstStepDate":"2016-04-20","lastStepDate":"2016-04-19","frequency":"P3M",""" +
      """"adjustment":{"modifyingValue":-100.0,"type":"DeltaAmount"}}"""

  /** A document whose adjustment replaces the value, which the decoder has to reject. */
  private val ReplacementJson: String =
    """{"firstStepDate":"2016-04-20","lastStepDate":"2016-10-20","frequency":"P3M",""" +
      """"adjustment":{"modifyingValue":100.0,"type":"Replace"}}"""

  //-------------------------------------------------------------------------
  /**
   * Reads the sequence out of an outcome that is expected to hold one.
   *
   * The factory reports a rejection as a value, so a fixture built through it is an outcome
   * rather than a sequence. This is the one place this spec turns the first into the second, and
   * it does so by handling both sides: a fixture that fails is a defect in this spec and is
   * reported as one, naming the failures, rather than raising an error from a partial accessor
   * that would name nothing.
   *
   * @param result  the outcome of the factory, expected to hold a sequence
   * @return the sequence the outcome holds
   */
  private def sequence(result: ResultNec[ValueStepSequence]): ValueStepSequence =
    result.fold(
      failures =>
        fail(s"invalid sequence fixture: ${failures.toChain.toList.map(_.message).mkString("; ")}"),
      held => held)

  /**
   * The messages of the failures an outcome of the factory holds, in the order it holds them.
   *
   * The matchers assert that ''some'' failure carries a reason or matches a pattern, which is the
   * right question for an outcome that can hold several. This helper answers the other two
   * questions the accumulating factory raises - how many failures there are and which, exactly -
   * so that a chain of two can be pinned to the two messages that belong in it, in order, rather
   * than to the presence of one of them.
   *
   * @param result  the outcome to inspect
   * @return the messages of its failures in order, empty when it holds none
   */
  private def messages(result: ResultNec[ValueStepSequence]): List[String] =
    result.fold(failures => failures.toChain.toList.map(_.message), _ => List.empty[String])

  /**
   * Reads the steps out of a resolution that is expected to have succeeded.
   *
   * Resolution reports a single cause, so its outcome is the one-failure shape rather than the
   * chain the factory produces, and it needs its own reader for the same reason the factory does.
   *
   * @param result  the outcome of resolution, expected to hold the steps
   * @return the steps the outcome holds
   */
  private def resolved(result: FailureOr[List[ValueStep]]): List[ValueStep] =
    result.fold(
      failure => fail(s"the sequence did not resolve: ${failure.message}"),
      held => held)

  /**
   * Reads the value out of a factory outcome that is expected to hold one.
   *
   * This is [[sequence]] for the types the public-path case below builds its target schedule
   * from - a schedule period and a schedule - whose factories report their outcome as a value for
   * the same reason the factory of this type does. A fixture that fails is a defect in this spec
   * and is reported as one, naming the failures.
   *
   * @param result  the outcome of a factory, expected to hold a value
   * @return the value the outcome holds
   * @tparam A  the type the factory builds
   */
  private def ok[A](result: ResultNec[A]): A =
    result.fold(
      failures => fail(s"invalid fixture: ${failures.toChain.toList.map(_.message).mkString("; ")}"),
      held => held)

  /**
   * The message of a resolution that is expected to have failed.
   *
   * The outcome is read generically in the value it would have held, because the failures of a
   * resolution are asserted on two channels here: the steps [[ValueStepSequence.resolve]] returns
   * and the values [[ValueSchedule.resolveValues]] returns, which is where the public-path case
   * below reads its failure from.
   *
   * @param result  the outcome of resolution, expected to hold a failure
   * @return the message of that failure
   * @tparam A  the type the resolution would have produced
   */
  private def failureMessage[A](result: FailureOr[A]): String =
    result.fold(
      failure => failure.message,
      held => fail(s"the sequence resolved where a failure was expected: $held"))

  /**
   * Parses one of the JSON literals above into the JSON model.
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
  test("test_of") {
    // The factory checks its arguments and so reports an outcome; these arguments are the ones
    // the Java test used and they describe a sequence, so the outcome holds one.
    val result: ResultNec[ValueStepSequence] =
      ValueStepSequence.of(date(2016, 4, 20), date(2016, 10, 20), Frequency.P3M, Adj)
    result should beSuccess

    // The four properties of the sequence, read back through the accessors that replace the
    // `getFirstStepDate`-style getters of the bean being ported.
    val test: ValueStepSequence = sequence(result)
    test.firstStepDate shouldBe date(2016, 4, 20)
    test.lastStepDate shouldBe date(2016, 10, 20)
    test.frequency shouldBe Frequency.P3M
    test.adjustment shouldBe Adj

    // A second sequence built independently from equal arguments is the same value, which is what
    // makes the equality the rest of this spec relies on structural rather than by reference.
    ValueStepSequence.of(date(2016, 4, 20), date(2016, 10, 20), Frequency.P3M, Adj) should
      haveValue(test)
  }

  //-------------------------------------------------------------------------
  test("test_of_invalid") {
    // The two rejections the Java test asserted as raised exceptions, asserted here as the
    // failures they are reported as. Both are faults in the data supplied rather than breaches of
    // a caller contract, so both belong on the left of an outcome rather than in a raised error.

    // 1. The dates are in the wrong order. The reason is compared as a member of the closed
    // family of reasons, and the message is pinned both by pattern and as a literal.
    val reversed: ResultNec[ValueStepSequence] =
      ValueStepSequence.of(date(2016, 4, 20), date(2016, 4, 19), Frequency.P3M, Adj)
    reversed should beFailure
    reversed should beFailureWith(FailureReason.INVALID)
    reversed should haveFailureMessageMatching(".*Invalid order: Expected 'firstStepDate' <= .*")
    messages(reversed) shouldBe List(InvalidDateOrder)

    // 2. The adjustment replaces the value rather than varying it.
    val replacing: ResultNec[ValueStepSequence] =
      ValueStepSequence.of(date(2016, 4, 20), date(2016, 10, 20), Frequency.P3M, AdjBad)
    replacing should beFailure
    replacing should beFailureWith(FailureReason.INVALID)
    replacing should haveFailureMessageMatching(".*must not be 'Replace'.*")
    messages(replacing) shouldBe List(ReplacementNotAllowed)

    // The two checks are independent, so where both fail both are reported. That is more than the
    // validator being ported said - it raised the first fault it found and stopped, so a caller
    // correcting its input learned of the second only on the next attempt - and it is the one
    // behaviour this test asserts that the Java test could not. The two are reported in the order
    // they are checked, the order of the dates first and then the type of the adjustment.
    val bothFaults: ResultNec[ValueStepSequence] =
      ValueStepSequence.of(date(2016, 4, 20), date(2016, 4, 19), Frequency.P3M, AdjBad)
    bothFaults should beFailureWith(FailureReason.INVALID)
    bothFaults should haveFailureMessageMatching(".*Invalid order: Expected 'firstStepDate' <= .*")
    bothFaults should haveFailureMessageMatching(".*must not be 'Replace'.*")
    messages(bothFaults).size shouldBe 2
    messages(bothFaults) shouldBe List(InvalidDateOrder, ReplacementNotAllowed)

    // Equal dates are in order, which is the edge the first check names and which neither
    // implementation rejects.
    sequence(
      ValueStepSequence.of(date(2016, 4, 20), date(2016, 4, 20), Frequency.P3M, Adj))
      .lastStepDate shouldBe date(2016, 4, 20)
  }

  //-------------------------------------------------------------------------
  test("test_resolve") {
    // The sequence of the Java test, resolved under the convention that adjusts nothing, so the
    // dates the walk lands on are the quarterly anniversaries of the first date.
    val test: ValueStepSequence =
      sequence(ValueStepSequence.of(date(2016, 4, 20), date(2016, 10, 20), Frequency.P3M, Adj))
    val baseStep: ValueStep = ValueStep.of(date(2016, 1, 20), ValueAdjustment.ofReplace(500.0d))

    val result: FailureOr[List[ValueStep]] = test.resolve(List(baseStep), RollConventions.NONE)
    result should beSuccess

    // The step supplied is kept and the generated steps follow it, in date order. The whole list
    // is compared in one assertion - which also pins that nothing else is in it - and then
    // element by element, as the Java test read it back through `get`.
    val steps: List[ValueStep] = resolved(result)
    steps.size shouldBe 4
    steps shouldBe List(
      baseStep,
      ValueStep.of(date(2016, 4, 20), Adj),
      ValueStep.of(date(2016, 7, 20), Adj),
      ValueStep.of(date(2016, 10, 20), Adj))
    steps.head shouldBe baseStep
    steps(1) shouldBe ValueStep.of(date(2016, 4, 20), Adj)
    steps(2) shouldBe ValueStep.of(date(2016, 7, 20), Adj)
    steps(3) shouldBe ValueStep.of(date(2016, 10, 20), Adj)

    // Every generated step carries the adjustment of the sequence, which is the sense in which a
    // sequence is one repeated change rather than a run of differing ones.
    steps.tail.map(_.value) shouldBe List(Adj, Adj, Adj)
  }

  //-------------------------------------------------------------------------
  test("test_resolve_with_roll_convention") {
    // An annual sequence under the IMM convention, which lands on the third Wednesday of each
    // September rather than on the anniversary of the first date. The five dates are those of the
    // Java test, asserted as literals: they are the evidence that the walk of this port - which
    // iterates the `next` operation of the convention rather than stepping a mutable date -
    // reaches exactly the dates the ported loop reached, and an off-by-one in that rewrite would
    // show up here as a missing or an extra step.
    val test: ValueStepSequence =
      sequence(ValueStepSequence.of(date(2022, 9, 21), date(2026, 9, 21), Frequency.P12M, Adj))

    val result: FailureOr[List[ValueStep]] = test.resolve(List.empty, RollConventions.IMM)
    result should beSuccess

    val steps: List[ValueStep] = resolved(result)
    steps.size shouldBe 5
    steps shouldBe List(
      ValueStep.of(date(2022, 9, 21), Adj),
      ValueStep.of(date(2023, 9, 20), Adj),
      ValueStep.of(date(2024, 9, 18), Adj),
      ValueStep.of(date(2025, 9, 17), Adj),
      ValueStep.of(date(2026, 9, 16), Adj))
    steps.head shouldBe ValueStep.of(date(2022, 9, 21), Adj)
    steps(1) shouldBe ValueStep.of(date(2023, 9, 20), Adj)
    steps(2) shouldBe ValueStep.of(date(2024, 9, 18), Adj)
    steps(3) shouldBe ValueStep.of(date(2025, 9, 17), Adj)
    steps(4) shouldBe ValueStep.of(date(2026, 9, 16), Adj)

    // The convention is applied to the last date of the sequence too, which is why a sequence
    // whose last date is not itself a third Wednesday resolves rather than failing: the date the
    // walk has to reach is the adjusted one.
    steps.last.date shouldBe Some(RollConventions.IMM.adjust(date(2026, 9, 21)))
  }

  //-------------------------------------------------------------------------
  test("test_resolve_invalid") {
    // A twelve month frequency over a six month span: the walk leaves the first date and the next
    // date it would reach is past the last date of the sequence, so the last date the walk lands
    // on is not the last date of the sequence and the arguments describe no sequence of steps
    // under this convention. The Java implementation raised this; here it is reported.
    val test: ValueStepSequence =
      sequence(ValueStepSequence.of(date(2016, 4, 20), date(2016, 10, 20), Frequency.P12M, Adj))
    val baseStep: ValueStep = ValueStep.of(date(2016, 1, 20), ValueAdjustment.ofReplace(500.0d))

    val result: FailureOr[List[ValueStep]] = test.resolve(List(baseStep), RollConventions.NONE)
    result should beFailure
    result should beFailureWith(FailureReason.INVALID)
    result should haveFailureMessageMatching(".*lastStepDate did not match frequency.*")

    // The message names the frequency, the convention and the two dates that disagree, in the
    // words of the template the Java implementation formatted.
    failureMessage(result) shouldBe FrequencyMismatch

    // The step supplied is not returned alongside the failure: a resolution that fails produces
    // no list at all, so a caller cannot mistake a partial one for a whole one.
    result.toOption shouldBe None
  }

  //-------------------------------------------------------------------------
  test("test_resolve_atMaximumSteps") {
    // A daily frequency over a span of one day short of a hundred thousand: the largest
    // expansion the ceiling of this port allows, and the case that proves the ceiling does not
    // bite on a sequence that is merely large. The dates and the frequency are legal arguments -
    // neither is checked against any schedule at construction - so the only thing deciding the
    // outcome is the step count, and here it is exactly the limit.
    val test: ValueStepSequence =
      sequence(ValueStepSequence.of(CeilingFirstDate, LastDateAtCeiling, Frequency.P1D, Adj))

    val result: FailureOr[List[ValueStep]] = test.resolve(List.empty, RollConventions.NONE)
    result should beSuccess

    // Every date of the walk produced a step, the first and the last of them being the two dates
    // of the sequence, which is what makes the count above the count of the expansion rather
    // than of something the ceiling truncated.
    val steps: List[ValueStep] = resolved(result)
    steps.size shouldBe ValueStepSequence.MaximumStepCount
    steps.head shouldBe ValueStep.of(CeilingFirstDate, Adj)
    steps.last shouldBe ValueStep.of(LastDateAtCeiling, Adj)
    steps.forall(_.value == Adj) shouldBe true
  }

  //-------------------------------------------------------------------------
  test("test_resolve_beyondMaximumSteps") {
    // The same sequence with its last date one day later, which is one step more than the
    // ceiling allows. The Java original expanded any span it was given, building a step object
    // for each date; a caller is free to name dates centuries apart and a daily frequency, so
    // that expansion is work and allocation decided by data (CWE-400). This port refuses it.
    val test: ValueStepSequence =
      sequence(ValueStepSequence.of(CeilingFirstDate, LastDateBeyondCeiling, Frequency.P1D, Adj))

    val result: FailureOr[List[ValueStep]] = test.resolve(List.empty, RollConventions.NONE)
    result should beFailure
    result should beFailureWith(FailureReason.INVALID)
    result should haveFailureMessageMatching(".*expands to more than the maximum of 100000 steps")

    // The message names the limit that was crossed, word for word, so a caller reading a report
    // learns the bound rather than only that its arguments were too large.
    failureMessage(result) shouldBe ExpansionBeyondCeiling
    result.toOption shouldBe None

    // And the refusal is reached from a span no amount of iteration could finish: a daily
    // sequence ending at the last date the calendar holds describes hundreds of billions of
    // steps. This case returning at all is the evidence that the ceiling bounds the walk itself
    // rather than the list it produces - an expansion that materialised first would still be
    // running, and would exhaust the heap long before it stopped.
    val unbounded: ValueStepSequence =
      sequence(ValueStepSequence.of(CeilingFirstDate, LocalDate.MAX, Frequency.P1D, Adj))
    val unboundedResult: FailureOr[List[ValueStep]] =
      unbounded.resolve(List.empty, RollConventions.NONE)
    unboundedResult should beFailureWith(FailureReason.INVALID)
    unboundedResult should haveFailureMessageMatching(
      ".*expands to more than the maximum of 100000 steps")
  }

  //-------------------------------------------------------------------------
  test("test_resolve_equalDatesAtMaximumDate") {
    // The two dates of a sequence are checked against each other and against nothing else, so a
    // sequence whose first and last date are both the last date the calendar holds is a legal
    // value - `test_of_invalid` above pins that equal dates are in order - and resolving it is a
    // legal thing for a caller to ask for. Under the convention that adjusts nothing, the walk
    // starts on that date, which is also the date it has to reach, so the answer is a single step
    // there.
    val test: ValueStepSequence =
      sequence(ValueStepSequence.of(LocalDate.MAX, LocalDate.MAX, Frequency.P1D, Adj))

    val result: FailureOr[List[ValueStep]] = test.resolve(List.empty, RollConventions.NONE)
    result should beSuccess

    val steps: List[ValueStep] = resolved(result)
    steps.size shouldBe 1
    steps shouldBe List(ValueStep.of(LocalDate.MAX, Adj))
    steps.head.date shouldBe Some(LocalDate.MAX)
    steps.head.value shouldBe Adj

    // This is the case that decides where the walk stops. A walk that stepped past the date it
    // had reached in order to establish that it was finished would ask the convention for the day
    // after the last date the calendar holds, and `java.time` raises rather than answering; the
    // walk of this port accepts a date equal to the adjusted last date and stops there, which
    // reaches the same list by never asking. The single step above is that list, and it arriving
    // at all - rather than as a raised `DateTimeException` - is what this test pins.
    noException should be thrownBy test.resolve(List.empty, RollConventions.NONE)

    // The steps supplied are still kept and the generated step still follows them, so stopping at
    // the end of the range is the ordinary answer of the ordinary path rather than a special case.
    val baseStep: ValueStep = ValueStep.of(date(2016, 1, 20), ValueAdjustment.ofReplace(500.0d))
    resolved(test.resolve(List(baseStep), RollConventions.NONE)) shouldBe
      List(baseStep, ValueStep.of(LocalDate.MAX, Adj))
  }

  //-------------------------------------------------------------------------
  test("test_resolve_dateRangeOverflow") {
    // A sequence that genuinely needs a date the calendar cannot hold, which is the case the one
    // above is not: a monthly frequency from the day before the last date has to step a month
    // past the end of the range to discover whether it has finished, and that arithmetic has no
    // answer. The Java implementation let `java.time` raise it out of a resolution that reports
    // everything else as a value; this port reports it, and the reason is a member of the closed
    // family of reasons like every other failure of this type.
    val stepping: ValueStepSequence =
      sequence(
        ValueStepSequence.of(LocalDate.MAX.minusDays(1), LocalDate.MAX, Frequency.P1M, Adj))

    val steppingResult: FailureOr[List[ValueStep]] =
      stepping.resolve(List.empty, RollConventions.NONE)
    steppingResult should beFailure
    steppingResult should beFailureWith(FailureReason.INVALID)
    steppingResult should haveFailureMessageMatching(
      ".*moved outside the range of supported dates")
    failureMessage(steppingResult) shouldBe MonthlyOverflow

    // It is a failure rather than a raised exception, which is the whole point, and it carries no
    // partial list of steps.
    noException should be thrownBy stepping.resolve(List.empty, RollConventions.NONE)
    steppingResult.toOption shouldBe None

    // The second way a resolution reaches the edge of the calendar is the adjustment of an
    // endpoint, before any walking happens: a day-of-week convention moves a date forward to the
    // next matching day, and `LocalDate.MAX` is a Friday, so adjusting it to a Monday leaves the
    // range. Both endpoints are adjusted and both are guarded; this sequence reaches it on the
    // first of them.
    val adjusting: ValueStepSequence =
      sequence(ValueStepSequence.of(LocalDate.MAX, LocalDate.MAX, Frequency.P1D, Adj))

    val adjustingResult: FailureOr[List[ValueStep]] =
      adjusting.resolve(List.empty, RollConventions.DAY_MON)
    adjustingResult should beFailure
    adjustingResult should beFailureWith(FailureReason.INVALID)
    failureMessage(adjustingResult) shouldBe DailyMondayOverflow
    noException should be thrownBy adjusting.resolve(List.empty, RollConventions.DAY_MON)
    adjustingResult.toOption shouldBe None
  }

  //-------------------------------------------------------------------------
  test("test_resolve_nearMinimumDate") {
    // The other end of the range fails nothing, and this is the test that says so: the walk only
    // ever moves forward, so a sequence starting on the first date the calendar holds has the
    // whole of the calendar in front of it. Both a daily and a monthly frequency resolve there,
    // with the dates stated as offsets from `LocalDate.MIN` so that the assertion is about the
    // walk rather than about the spelling of a year of nine digits.
    val daily: ValueStepSequence =
      sequence(
        ValueStepSequence.of(LocalDate.MIN, LocalDate.MIN.plusDays(2), Frequency.P1D, Adj))
    resolved(daily.resolve(List.empty, RollConventions.NONE)) shouldBe List(
      ValueStep.of(LocalDate.MIN, Adj),
      ValueStep.of(LocalDate.MIN.plusDays(1), Adj),
      ValueStep.of(LocalDate.MIN.plusDays(2), Adj))

    val monthly: ValueStepSequence =
      sequence(
        ValueStepSequence.of(LocalDate.MIN, LocalDate.MIN.plusMonths(3), Frequency.P1M, Adj))
    resolved(monthly.resolve(List.empty, RollConventions.NONE)).map(_.date) shouldBe List(
      Some(LocalDate.MIN),
      Some(LocalDate.MIN.plusMonths(1)),
      Some(LocalDate.MIN.plusMonths(2)),
      Some(LocalDate.MIN.plusMonths(3)))
  }

  //-------------------------------------------------------------------------
  test("test_resolve_dateRangeOverflow_throughValueSchedule") {
    // Resolution is reached from outside this package through [[ValueSchedule.resolveValues]],
    // which is public, and that is the path a caller sees: a sequence held by a value schedule is
    // expanded under the roll convention of the schedule the values are resolved against, and the
    // failure of the expansion is the failure of the resolution. So the guard has to hold on this
    // path too, and this is where that is asserted rather than inferred from the calls above.
    val overflowing: ValueSchedule =
      ok(
        ValueSchedule.of(
          1000.0d,
          sequence(
            ValueStepSequence.of(LocalDate.MAX.minusDays(1), LocalDate.MAX, Frequency.P1M, Adj))))

    val result: FailureOr[DoubleArray] = overflowing.resolveValues(ScheduleFixture)
    result should beFailure
    result should beFailureWith(FailureReason.INVALID)
    failureMessage(result) shouldBe MonthlyOverflow
    noException should be thrownBy overflowing.resolveValues(ScheduleFixture)
    result.toOption shouldBe None

    // And the sequence of the case above - equal dates at the last date the calendar holds -
    // comes back through the same channel as a value rather than as a raised exception. It
    // resolves to its single step, and the failure reported is the ordinary one for a step that
    // falls outside every period of the schedule it is resolved against, which is what a step at
    // the end of the calendar is for the January 2014 schedule here.
    val atMaximum: ValueSchedule =
      ok(
        ValueSchedule.of(
          1000.0d,
          sequence(ValueStepSequence.of(LocalDate.MAX, LocalDate.MAX, Frequency.P1D, Adj))))

    val atMaximumResult: FailureOr[DoubleArray] = atMaximum.resolveValues(ScheduleFixture)
    atMaximumResult should beFailure
    atMaximumResult should beFailureWith(FailureReason.INVALID)
    atMaximumResult should haveFailureMessageMatching(
      ".*ValueStep date is after the end of the schedule.*")
    noException should be thrownBy atMaximum.resolveValues(ScheduleFixture)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java sweep `coverImmutableBean` walked the properties of a bean through its meta-bean,
    // on the two instances built below. There is neither a meta-bean nor reflective property
    // access here, so what it stood for is asserted directly on the same two instances.
    val test: ValueStepSequence =
      sequence(ValueStepSequence.of(date(2016, 4, 20), date(2016, 10, 20), Frequency.P3M, Adj))
    val test2: ValueStepSequence =
      sequence(ValueStepSequence.of(date(2016, 4, 1), date(2016, 10, 1), Frequency.P1M, Adj2))

    // Every property goes in and comes back, for both instances.
    test.firstStepDate shouldBe date(2016, 4, 20)
    test.lastStepDate shouldBe date(2016, 10, 20)
    test.frequency shouldBe Frequency.P3M
    test.adjustment shouldBe Adj
    test2.firstStepDate shouldBe date(2016, 4, 1)
    test2.lastStepDate shouldBe date(2016, 10, 1)
    test2.frequency shouldBe Frequency.P1M
    test2.adjustment shouldBe Adj2

    // The two instances differ in all four properties and are therefore distinct, under the
    // platform equality and under `Hash` - the type's single equality-bearing instance, from
    // which `Eq` is obtained by subtyping - which is the relation the bean sweep exercised by
    // perturbing one property at a time.
    test should not be test2
    test2 should not be test
    Hash[ValueStepSequence].eqv(test, test2) shouldBe false
    Hash[ValueStepSequence].eqv(test2, test) shouldBe false

    // Two sequences built independently from equal arguments are equal, hash alike, and are
    // reported as equal by `Hash` as well as by the platform equality.
    val copy: ValueStepSequence =
      sequence(ValueStepSequence.of(date(2016, 4, 20), date(2016, 10, 20), Frequency.P3M, Adj))
    test shouldBe test
    test shouldBe copy
    test.hashCode shouldBe copy.hashCode
    Hash[ValueStepSequence].hash(copy) shouldBe test.hashCode
    Hash[ValueStepSequence].eqv(test, test) shouldBe true
    Hash[ValueStepSequence].eqv(test, copy) shouldBe true

    // `Show` renders what `toString` renders, so the two ways of putting a sequence into a
    // message agree, and the form is pinned as a literal: all four properties in declaration
    // order, with the adjustment rendered as the calculation it performs.
    Show[ValueStepSequence].show(test) shouldBe test.toString
    Show[ValueStepSequence].show(test2) shouldBe test2.toString
    test.toString shouldBe
      "ValueStepSequence{firstStepDate=2016-04-20, lastStepDate=2016-10-20, frequency=P3M, " +
        "adjustment=ValueAdjustment[result = input + -100.0]}"
    test2.toString shouldBe
      "ValueStepSequence{firstStepDate=2016-04-01, lastStepDate=2016-10-01, frequency=P1M, " +
        "adjustment=ValueAdjustment[result = input + -200.0]}"

    // A sequence whose dates are in the wrong order, or whose adjustment replaces the value,
    // cannot be built at all, and this is where that claim is proved rather than asserted about
    // one input: the primary constructor is private and neither an `apply` nor a `copy` exists,
    // so the only way in is the factory that validates. A `copy` would reintroduce every state
    // the factory rejects, by taking a value that passed the checks and changing a property
    // without repeating them.
    assertDoesNotCompile(
      """ValueStepSequence(date(2016, 4, 20), date(2016, 10, 20),
           Frequency.P3M, ValueAdjustment.ofDeltaAmount(-100.0d))""")
    assertDoesNotCompile(
      """ValueStepSequence.of(date(2016, 4, 20), date(2016, 10, 20),
           Frequency.P3M, ValueAdjustment.ofDeltaAmount(-100.0d)).map(
           _.copy(lastStepDate = date(2016, 4, 19)))""")

    // The same two snippets with the missing member replaced by one that exists, which is what
    // keeps the two above from passing for the wrong reason: a snippet naming an identifier this
    // scope cannot resolve would also fail to compile, and would prove nothing about `apply` or
    // `copy`. These compile, so every other name in them resolves and the only difference is the
    // member each of the two reaches for.
    assertCompiles(
      """ValueStepSequence.of(date(2016, 4, 20), date(2016, 10, 20),
           Frequency.P3M, ValueAdjustment.ofDeltaAmount(-100.0d))""")
    assertCompiles(
      """ValueStepSequence.of(date(2016, 4, 20), date(2016, 10, 20),
           Frequency.P3M, ValueAdjustment.ofDeltaAmount(-100.0d)).map(
           _.lastStepDate)""")

    // Reading the four properties by pattern is unaffected by that closure - `unapply` is
    // available - which is what keeps a ported call site that matched on the bean readable.
    val destructured: (LocalDate, LocalDate, Frequency, ValueAdjustment) = test match {
      case ValueStepSequence(firstStepDate, lastStepDate, frequency, adjustment) =>
        (firstStepDate, lastStepDate, frequency, adjustment)
    }
    destructured shouldBe ((date(2016, 4, 20), date(2016, 10, 20), Frequency.P3M, Adj))
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // Java serialization is not part of this port; the JSON codec takes its place. Both
    // directions are asserted - an encoding that is wrong and a decoding that is wrong in the
    // same way would still round trip - and the documents are compared as parsed JSON rather than
    // as printed text, so the assertion is about the fields rather than the rendering.
    val test: ValueStepSequence =
      sequence(ValueStepSequence.of(date(2016, 4, 20), date(2016, 10, 20), Frequency.P3M, Adj))
    test.asJson shouldBe json(ExpectedJson)
    decode[ValueStepSequence](test.asJson.noSpaces) shouldBe Right(test)
    decode[ValueStepSequence](ExpectedJson) shouldBe Right(test)

    // The key set is exactly the four properties, in the declaration order of the bean, and each
    // is written in the form its own type publishes: the dates as ISO strings, the frequency as
    // its bare name, the adjustment as an object.
    test.asJson.asObject.map(_.keys.toList) shouldBe
      Some(List("firstStepDate", "lastStepDate", "frequency", "adjustment"))
    test.asJson.hcursor.downField("firstStepDate").as[String] shouldBe Right("2016-04-20")
    test.asJson.hcursor.downField("lastStepDate").as[String] shouldBe Right("2016-10-20")
    test.asJson.hcursor.downField("frequency").as[String] shouldBe Right("P3M")
    test.asJson.hcursor.downField("adjustment").downField("type").as[String] shouldBe
      Right("DeltaAmount")

    // The decoder routes the fields through the same validating factory a caller's arguments go
    // through, so a document whose dates are in the wrong order or whose adjustment replaces the
    // value is rejected rather than decoded into a sequence the factory would never have built.
    // The message of the rejection is the message of the failure that caused it.
    val reversed = decode[ValueStepSequence](ReversedDatesJson)
    reversed.isLeft shouldBe true
    reversed.swap.toOption.fold("")(error => error.getMessage) should include(InvalidDateOrder)

    val replacing = decode[ValueStepSequence](ReplacementJson)
    replacing.isLeft shouldBe true
    replacing.swap.toOption.fold("")(error => error.getMessage) should include(ReplacementNotAllowed)

    // A document missing a property describes no sequence either: all four are required, none has
    // a default, and the rejection names the property that is absent.
    val missing = decode[ValueStepSequence](
      """{"firstStepDate":"2016-04-20","lastStepDate":"2016-10-20","frequency":"P3M"}""")
    missing.isLeft shouldBe true
    missing.swap.toOption.fold("")(error => error.getMessage) should include("adjustment")
  }
}
