/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.value

import java.time.LocalDate

import cats.Hash
import cats.Show

import io.circe.Json
import io.circe.parser.decode
import io.circe.parser.parse
import io.circe.syntax._

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[ValueStep]].
 *
 * This is a one-to-one port of the Java test class `ValueStepTest`: each of its six test
 * methods has a test of the same name here, in the same order, and no test is added or split.
 * The last one of those names is the bare `equals`, which is the name the Java method carried
 * and is kept because the migration joins Java test method to Scala test by name.
 *
 * ===What the port changes, and why===
 *
 * Three of the six methods asserted behaviour this port does not have, and each keeps its name
 * while asserting what replaced it, so nothing the Java test covered is silently dropped:
 *
 *   - `test_builder_invalid` drove the Joda-Beans meta-bean builder, the only route by which the
 *     two states its validator rejected - neither position supplied, and both supplied - could
 *     be reached at all. There is no builder here, because the type is validated at
 *     construction: it is re-pointed onto the three-field [[ValueStep.of]], which is what a
 *     caller holding the fields of a step reaches for and which carries exactly the checks the
 *     `@ImmutableValidator` carried, reported in the same words. A rejection is a value on the
 *     left of an `Either` rather than a raised exception, so every Java
 *     `assertThatIllegalArgumentException` is written here as an assertion about those failures:
 *     the reason compared as a member of the closed family of reasons, and the message pinned
 *     both by pattern and word for word against the Java text. None of the four is a
 *     caller-contract precondition - each depends on the data supplied - so no exception is
 *     expected anywhere in this spec.
 *   - `coverage` called `coverImmutableBean`, a reflective sweep over the properties of a bean
 *     through its meta-bean. There is no meta-bean and no reflective property access here, so
 *     its substance is asserted directly: the accessors, the rendering, and the closed
 *     construction surface of a validated type, proved by requiring two snippets to fail to
 *     compile rather than by asserting something about one instance.
 *   - `test_serialization` asserted a Java serialization round trip, which is not part of this
 *     port at all. The JSON codec takes its place, so the round trip asserted is
 *     `decode(encode(x)) == x` in both directions together with the exact shape of the encoding,
 *     including the absence of the field for the position a step does not hold.
 *
 * ===The asymmetry of the two short factories===
 *
 * `of(periodIndex, value)` reports its outcome and `of(date, value)` does not, and that is
 * faithful to the Java original rather than an oversight: an index of zero or less names the
 * start of the first period, where a change is not permitted, while a date is a date and
 * whether it lines up with a boundary is a question about the schedule the step is applied to.
 * The two are therefore asserted in the two shapes they have, and the first two tests below are
 * where that difference is visible.
 *
 * ===What is asserted elsewhere===
 *
 * Three duties that touch this type are module-wide and are discharged by module-wide specs
 * rather than repeated here: the sweep of every validated type's invalid inputs and their
 * accumulation (`SmartConstructorSpec`), the proof that no validated type has a public `apply`
 * or `copy` (`ApiSurfaceSpec`), and the property-based codec round trip over every
 * codec-bearing type (`json/JsonRoundTripSpec`). This spec stays with the six ported methods,
 * and the resolution helpers of the type - which are not public and are reached only from
 * [[ValueSchedule]] - are asserted through `ValueScheduleSpec`, as they were in Java.
 */
final class ValueStepSpec extends AnyFunSuite with Matchers {

  /** The relative adjustment of the Java fixture `DELTA_MINUS_2000`. */
  private val DeltaMinus2000: ValueAdjustment = ValueAdjustment.ofDeltaAmount(-2000.0d)

  /** The absolute adjustment of the Java fixture `ABSOLUTE_100`. */
  private val Absolute100: ValueAdjustment = ValueAdjustment.ofReplace(100.0d)

  /** The date the Java test positions its date-based steps at. */
  private val StepDate: LocalDate = date(2014, 6, 30)

  /** The later date the Java equality matrix compares against. */
  private val LaterStepDate: LocalDate = date(2014, 7, 30)

  //-------------------------------------------------------------------------
  /**
   * The message reported when neither position is supplied, in the words of the Java bean.
   *
   * The three messages below are transcribed from the `@ImmutableValidator` of the Java type
   * and are asserted as literals, so a change to the wording of the port cannot pass unnoticed
   * and a reader comparing the two implementations can see that they agree.
   */
  private val EitherPositionRequired: String = "Either the 'periodIndex' or 'date' must be set"

  /** The message reported when both positions are supplied, in the words of the Java bean. */
  private val SinglePositionRequired: String =
    "Either the 'periodIndex' or 'date' must be set, not both"

  /** The message reported for a period index of zero or less, in the words of the Java bean. */
  private val PeriodIndexNotPositive: String = "The 'periodIndex' must not be zero or negative"

  //-------------------------------------------------------------------------
  /**
   * The expected JSON of an index-based step.
   *
   * The document holds the position the step actually has and its adjustment, under the names
   * the Java bean declared. There is no `date` field: the field of the position a step does not
   * hold is dropped from the output rather than written as an explicitly empty one, which is the
   * policy of this port and is what `test_serialization` below pins by asserting the key set.
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

    // The three properties of the step, where the Java test read the two positions back through
    // `OptionalInt` and `Optional`. Neither type is named by this port: a position that is held
    // is `Some` and one that is not is `None`, which is the whole of the translation.
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
    // The date-based factory is total, exactly as in Java: there is nothing about a date this
    // type can reject, so it hands back a step rather than an outcome and no handling is needed
    // around it. The asymmetry with the factory above is deliberate and is not normalised away.
    val test: ValueStep = ValueStep.of(StepDate, DeltaMinus2000)
    test.date shouldBe Some(StepDate)
    test.periodIndex shouldBe None
    test.value shouldBe DeltaMinus2000

    // The three-field factory reaches the same value, as it does for an index-based step.
    ValueStep.of(None, Some(StepDate), DeltaMinus2000) should haveValue(test)
  }

  //-------------------------------------------------------------------------
  test("test_builder_invalid") {
    // The Joda-Beans builder the Java test drove four ways has no target in this port; the
    // three-field `of` replaces it and carries the checks of the bean's `@ImmutableValidator`,
    // in the bean's own words, reported as values rather than raised.

    // 1. Neither position supplied. Reachable only through this factory, which is why it exists.
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

    // The first index this factory accepts is one, which is the edge the message names and which
    // the Java test did not reach through the builder.
    step(ValueStep.of(Some(1), None, DeltaMinus2000)).periodIndex shouldBe Some(1)

    // Where two checks fail at once both are reported, which is more than the validator being
    // ported said: it raised the first fault it found and stopped, so a caller correcting its
    // input learned of the second only on the next attempt. The two are reported in the order
    // they are checked - the pair of positions first, then the range of the index - and both
    // matchers see the chain, so either reason or message is enough to match it.
    val twoFaults: ResultNec[ValueStep] = ValueStep.of(Some(0), Some(StepDate), DeltaMinus2000)
    twoFaults should beFailureWith(FailureReason.INVALID)
    twoFaults should haveFailureMessageMatching(".*not both.*")
    twoFaults should haveFailureMessageMatching(".*must not be zero or negative.*")
    messages(twoFaults).size should be > 1
    messages(twoFaults) shouldBe List(SinglePositionRequired, PeriodIndexNotPositive)
  }

  //-------------------------------------------------------------------------
  test("equals") {
    // The index-based matrix of the Java test, built the same four ways. The second instance is
    // built independently from equal inputs, so equality is shown to be structural rather than
    // by reference, and either differing field is enough to make two steps unequal.
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

    // The date-based matrix of the Java test, and the same four facts about it.
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
    // The Java sweep `coverImmutableBean` walked the properties of a bean through its meta-bean.
    // There is neither here, so what it stood for is asserted directly: the accessors, the
    // rendering, and the closed construction surface of a validated type.
    val test: ValueStep = step(ValueStep.of(2, DeltaMinus2000))
    val dateBased: ValueStep = ValueStep.of(StepDate, DeltaMinus2000)

    // Every field goes in and comes back, for both of the two shapes a step can have.
    test.periodIndex shouldBe Some(2)
    test.date shouldBe None
    test.value shouldBe DeltaMinus2000
    dateBased.periodIndex shouldBe None
    dateBased.date shouldBe Some(StepDate)
    dateBased.value shouldBe DeltaMinus2000

    // `Show` renders what `toString` renders, so the two ways of putting a step into a message
    // agree, and both forms are pinned as literals: the rendering names the position the step
    // actually holds, and the adjustment renders as the calculation it performs.
    Show[ValueStep].show(test) shouldBe test.toString
    Show[ValueStep].show(dateBased) shouldBe dateBased.toString
    test.toString shouldBe "ValueStep{periodIndex=2, value=ValueAdjustment[result = input + -2000.0]}"
    dateBased.toString shouldBe
      "ValueStep{date=2014-06-30, value=ValueAdjustment[result = input + -2000.0]}"

    // A step holding no position, both positions, or an index of zero cannot be built at all,
    // and this is where that claim is proved rather than asserted about one input: the primary
    // constructor is private and neither an `apply` nor a `copy` exists, so the only way in is a
    // factory that validates. Both facts are checked by compiling a snippet and requiring it to
    // fail - a `copy` would reintroduce every state the factories reject.
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

    // Reading the three fields by pattern is unaffected by that closure - `unapply` is available
    // - which is what keeps a ported call site that matched on the bean readable.
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
    // Java serialization is not part of this port; the JSON codec takes its place. Both
    // directions are asserted - an encoding that is wrong and a decoding that is wrong in the
    // same way would still round trip - and the documents are compared as parsed JSON rather
    // than as printed text, so the assertion is about the fields rather than the rendering.
    val test: ValueStep = step(ValueStep.of(2, DeltaMinus2000))
    test.asJson shouldBe json(ExpectedIndexJson)
    decode[ValueStep](test.asJson.noSpaces) shouldBe Right(test)
    decode[ValueStep](ExpectedIndexJson) shouldBe Right(test)

    // The field of the position a step does not hold is absent from the document rather than
    // written as an explicitly empty one, which is the point of this assertion: the key set is
    // exactly the position held and the adjustment, in declaration order.
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
}
