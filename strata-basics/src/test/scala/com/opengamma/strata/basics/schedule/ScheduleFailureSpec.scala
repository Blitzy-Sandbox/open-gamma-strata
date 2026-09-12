/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.schedule

import java.time.LocalDate

import scala.collection.immutable.SortedMap

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test the way this port reports a schedule that cannot be calculated, ported from the Java
 * `ScheduleExceptionTest`.
 *
 * ===Why this spec is named differently from the class it ports===
 *
 * `ScheduleException` is deliberately '''not''' ported. It was a
 * `java.lang.IllegalArgumentException` subclass carrying the rejected [[PeriodicSchedule]] as a
 * nullable field and a message assembled from a `{}` template, and the Agent Action Plan replaces
 * that whole exception - not merely its message - with a value: the failure a schedule operation
 * returns on the left of an `Either`. That value is `Failure.Invalid`, one of the ten members of
 * the sealed failure sum of the collect module, and the field that held the rejected definition is
 * an entry of its attribute map, under the key `definition` (AAP §0.4.1, the
 * `PeriodicSchedule.scala` and `Schedule.scala` rows, and §0.8.1 Conflict 3, which fixes the error
 * channel as `EitherNec[Failure, A]` over a sealed error type rather than over text).
 *
 * There being no exception type to name a spec after, the spec is named after what it actually
 * tests, and `manifest/java-test-mapping.csv` records the rename: its two rows carry
 * `java_test_class = com.opengamma.strata.basics.schedule.ScheduleExceptionTest` with
 * `scala_spec = com.opengamma.strata.basics.schedule.ScheduleFailureSpec`, status `consolidated`.
 * The two Java method names are kept verbatim as the test names here, because the acceptance gate
 * joins that file to the JUnit XML on the suite class and the test name; this suite therefore
 * reports exactly two test cases, `test_withDefinition` and `test_withoutDefinition`.
 *
 * ===What each test asserts, and why in two halves===
 *
 * Each Java method constructed an exception directly and read its two accessors back. Constructing
 * the replacement directly is the first half of each test here, and on its own it would assert
 * nothing about this library: the failure type lives in the collect module and would hold whatever
 * it was handed. So each test has a second half that drives a '''real''' rejection through the
 * schedule types and asserts the same two properties of what comes back - which is what proves that
 * the production code attaches the definition where the exception carried it, and attaches nothing
 * where the exception carried nothing:
 *
 *   - `test_withDefinition` pairs the constructed failure with the `test_none_badStub` scenario of
 *     the Java `PeriodicScheduleTest` - 4th June to 17th September by `P1M` rolling on day 4, which
 *     needs a stub while the stub convention forbids one. Java asserted that
 *     `createUnadjustedDates()` threw `ScheduleException`; here it returns
 *     `Left(Failure.Invalid)`, and that failure carries the definition that was rejected.
 *   - `test_withoutDefinition` pairs the constructed failure with a [[Schedule.mergeRegular]]
 *     rejection, which is the contrasting case: a merge is asked of a schedule, not of a
 *     definition, so there is no definition to name and the attribute map stays empty. Java's
 *     `Optional.empty()` only means something alongside a rejection that really does omit it.
 *
 * No test in this file asserts a thrown exception, and no exception type is referenced by it. The
 * rendering of the definition is not invented here either: [[PeriodicSchedule]] attaches its own
 * `toString`, so that is what is asserted.
 */
class ScheduleFailureSpec extends AnyFunSuite with Matchers with ResultMatchers {

  /**
   * The attribute key the rejected schedule definition is carried under.
   *
   * The production code holds this name privately - the definition attribute of
   * `PeriodicSchedule` and of `StubConvention` - so the spec states it independently rather than
   * reading it back from the code under test.
   */
  private val DefinitionAttribute: String = "definition"

  /** The message the two Java methods built from the template `"Hello {}"` and the word `World`. */
  private val FormattedMessage: String = "Hello World"

  //-------------------------------------------------------------------------
  // The dates of the two scenarios, named as the Java test classes named them.

  /** The start date of the valid definition of the Java `test_withDefinition`. */
  private val JUN_30: LocalDate = date(2014, 6, 30)

  /** The end date of the valid definition of the Java `test_withDefinition`. */
  private val AUG_30: LocalDate = date(2014, 8, 30)

  /** The start date of the `test_none_badStub` scenario of the Java `PeriodicScheduleTest`. */
  private val JUN_04: LocalDate = date(2014, 6, 4)

  /** The end date of the `test_none_badStub` scenario of the Java `PeriodicScheduleTest`. */
  private val SEP_17: LocalDate = date(2014, 9, 17)

  //-------------------------------------------------------------------------
  /**
   * The definition of the Java `test_withDefinition`, built exactly as that method built it.
   *
   * 30th June to 30th August by `P1M` divides evenly, so no stub is needed and the 'None' stub
   * convention is satisfied: this definition is valid, which is the point of it. It is what the
   * ported exception would have carried, and it is also the definition the merge rejection of
   * `test_withoutDefinition` is reached through.
   */
  private val definition: PeriodicSchedule =
    accepted(
      PeriodicSchedule.of(
        JUN_30,
        AUG_30,
        Frequency.P1M,
        BusinessDayAdjustment.NONE,
        StubConvention.NONE,
        false))

  /**
   * The definition of the `test_none_badStub` scenario, which is valid but generates no schedule.
   *
   * Nothing about a stub is decided by [[PeriodicSchedule.of]] - deciding it needs the schedule
   * rolled out - so this definition is accepted and rejected later, by generation, which is the
   * point at which the Java form raised its exception.
   */
  private val badStubDefinition: PeriodicSchedule =
    accepted(
      PeriodicSchedule.of(
        JUN_04,
        SEP_17,
        Frequency.P1M,
        BusinessDayAdjustment.NONE,
        StubConvention.NONE,
        RollConventions.DAY_4))

  //-------------------------------------------------------------------------
  test("test_withDefinition") {
    // the shape of the replacement: the message the template produced, and the rejected definition
    // attached under the key the production code uses, which is Java's non-empty getDefinition
    val test: Failure =
      Failure.Invalid(FormattedMessage).withAttribute(DefinitionAttribute, definition.toString)
    test.reason shouldBe FailureReason.INVALID
    test.message shouldBe FormattedMessage
    test.attributes.get(DefinitionAttribute) shouldBe Some(definition.toString)
    test shouldBe Failure.Invalid(
      FormattedMessage,
      SortedMap(DefinitionAttribute -> definition.toString))

    // the real path: the scenario Java asserted ScheduleException for is a value here, and it
    // carries the definition that was rejected - the attribute is attached by the production code,
    // not by this spec
    val rejection: FailureOr[List[LocalDate]] = badStubDefinition.createUnadjustedDates()
    rejection should beFailureWith(FailureReason.INVALID)
    rejection should haveFailureMessageMatching(".*resulted in a disallowed stub.*")
    val reported = failureOf(rejection)
    reported shouldBe a[Failure.Invalid]
    reported.attributes.get(DefinitionAttribute) shouldBe Some(badStubDefinition.toString)
  }

  //-------------------------------------------------------------------------
  test("test_withoutDefinition") {
    // the shape of the replacement without a definition: the same message, and an attribute map
    // that holds nothing, which is Java's Optional.empty()
    val test: Failure = Failure.Invalid(FormattedMessage)
    test.reason shouldBe FailureReason.INVALID
    test.message shouldBe FormattedMessage
    test.attributes shouldBe SortedMap.empty[String, String]
    test.attributes.get(DefinitionAttribute) shouldBe None

    // the contrast that gives the empty case its meaning: a rejection that genuinely has no
    // definition to name. A merge is asked of a schedule rather than of a definition, so the
    // failure reporting an unusable group size carries a message and nothing else
    val schedule = created(definition.createSchedule(ReferenceData.minimal))
    val rejection: FailureOr[Schedule] = schedule.mergeRegular(0, true)
    rejection should beFailureWith(FailureReason.INVALID)
    rejection should haveFailureMessageMatching(".*must not be negative or zero.*")
    val reported = failureOf(rejection)
    reported shouldBe a[Failure.Invalid]
    reported.attributes shouldBe SortedMap.empty[String, String]
    reported.attributes.get(DefinitionAttribute) shouldBe None
  }

  //-------------------------------------------------------------------------
  /**
   * Unwraps a definition built by the validated factory, failing the test if it was rejected.
   *
   * The factory accumulates its invariant failures, so a rejection is reported with every reason
   * it gave rather than only the first.
   *
   * @param result  the result of the factory
   * @return the definition the factory built
   */
  private def accepted(result: ResultNec[PeriodicSchedule]): PeriodicSchedule =
    result.fold(
      failures =>
        fail(
          failures.toNonEmptyList.toList
            .map(failure => failure.message)
            .mkString("Expected a valid definition but the factory rejected it: ", "; ", "")),
      value => value)

  /**
   * Unwraps a schedule created from a definition, failing the test if creation reported a failure.
   *
   * @param result  the result of schedule creation
   * @return the schedule that was created
   */
  private def created(result: FailureOr[Schedule]): Schedule =
    result.fold(
      failure => fail(s"Expected a schedule but creation reported: ${failure.message}"),
      value => value)

  /**
   * Reads the failure an operation reported, failing the test if the operation succeeded.
   *
   * The matchers assert the reason and the message; this is how the attribute map, which no
   * matcher covers, is reached.
   *
   * @param result  the result of the operation
   * @tparam A  the type of the value the operation produces when it succeeds
   * @return the failure the operation reported
   */
  private def failureOf[A](result: FailureOr[A]): Failure =
    result.fold(
      failure => failure,
      value => fail(s"Expected a failure but the operation produced: $value"))
}
