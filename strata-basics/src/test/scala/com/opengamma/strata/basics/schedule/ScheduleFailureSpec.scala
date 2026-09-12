/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.schedule

import java.time.LocalDate

import scala.collection.immutable.SortedMap

import cats.Show

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.ImmutableReferenceData
import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.basics.date.BusinessDayConventions.MODIFIED_FOLLOWING
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.basics.date.HolidayCalendars
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
 * joins that file to the JUnit XML on the suite class and the test name: `test_withDefinition` and
 * `test_withoutDefinition` are therefore reported under exactly those names. The suite carries one
 * further case, `test_definitionAttributeSurvivesHostileCalendarNames`, which no manifest row names
 * because it has no Java counterpart; the join runs from a row to a test case, so an unnamed case
 * costs the gate nothing. It is described below.
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
 * ===What the third test asserts===
 *
 * The definition a failure carries embeds a business day adjustment, which names a
 * [[com.opengamma.strata.basics.date.HolidayCalendarId]] whose name is accepted as given:
 * `HolidayCalendarId.of` is total, so a calendar name may hold a line feed or several thousand
 * characters, and that text reaches both the `definition` attribute and - through the adjustment
 * quoted by the duplicate-adjusted message - a failure message. Two properties have to hold at
 * once for that to be both useful and safe, and the third test states both:
 *
 *   - the structured value stays '''faithful''': the attribute is the definition's own `toString`,
 *     character for character, line feed and all, because a report naming the rejected definition
 *     and a caller comparing it with what it supplied would otherwise read a summary of it;
 *   - the '''text''' of the failure is neutralised: `Show[Failure]` and the text form of a failure
 *     are the same string, hold no character a line-oriented reader could act on, and are bounded,
 *     so a hostile or oversized calendar name cannot forge a line of a log or a report that holds
 *     the failure and cannot inflate it to its own size.
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
  // The fixtures of the third test: calendar names that a caller could supply and that this
  // library does not constrain, reached through the ordinary API rather than by construction.

  /**
   * A calendar name carrying a line feed and text that reads as a log line of its own.
   *
   * This is the shape a forged log entry takes: everything after the line feed would appear as a
   * separate line wherever the failure was written out unescaped, stating something this library
   * never reported (CWE-117).
   */
  private val ForgedCalendarName: String = "GBLO\nINVALID: forged"

  /** A calendar name of a few thousand characters, which no part of the API rejects. */
  private val OversizedCalendarName: String = "GBLO\n" + ("FORGED " * 600)

  /**
   * The ceiling the text of a schedule failure is asserted to stay under.
   *
   * The bound is stated as a concrete number comfortably above an ordinary rendering rather than
   * as an exact length, so that it says what it is for - the text of a failure stays a line that
   * can be read - without pinning the rendering down character by character. A failure of this
   * kind renders as its reason, one message part and one attribute whose key is the ten-character
   * `definition`, and the writing of a failure bounds each part it writes, so no rendering of one
   * can reach this ceiling; the oversized name below is more than three times it, and the
   * attribute holding that name keeps every character of it.
   */
  private val RenderingCeiling: Int = 1200

  /** The adjustment that carries the forged calendar name into a definition. */
  private val forgedAdjustment: BusinessDayAdjustment =
    BusinessDayAdjustment.of(MODIFIED_FOLLOWING, HolidayCalendarId.of(ForgedCalendarName))

  /**
   * The `test_none_badStub` scenario again, adjusted by a calendar whose name is forged.
   *
   * The rejection this reaches needs no reference data, so the identifier is never resolved: the
   * name travels into the definition's own rendering and from there into the attribute, which is
   * exactly the path a caller that parsed a document holding that name would take.
   */
  private val forgedBadStubDefinition: PeriodicSchedule =
    accepted(
      PeriodicSchedule.of(
        JUN_04,
        SEP_17,
        Frequency.P1M,
        forgedAdjustment,
        StubConvention.NONE,
        RollConventions.DAY_4))

  /** The same scenario adjusted by a calendar whose name is several thousand characters long. */
  private val oversizedDefinition: PeriodicSchedule =
    accepted(
      PeriodicSchedule.of(
        JUN_04,
        SEP_17,
        Frequency.P1M,
        BusinessDayAdjustment.of(MODIFIED_FOLLOWING, HolidayCalendarId.of(OversizedCalendarName)),
        StubConvention.NONE,
        RollConventions.DAY_4))

  /**
   * A daily definition over a weekend, which is the rejection whose ''message'' quotes the
   * adjustment.
   *
   * Friday to the following Monday at a daily frequency yields four unadjusted dates, and the
   * weekend calendar moves the Saturday and the Sunday onto the Monday that the fourth of them
   * already is, so the adjusted dates contain duplicates. The failure reporting them names the
   * adjustment that produced them - and therefore the forged calendar name - inside its message
   * rather than only in its attribute.
   */
  private val forgedDailyDefinition: PeriodicSchedule =
    accepted(
      PeriodicSchedule.of(
        date(2020, 9, 18),
        date(2020, 9, 21),
        Frequency.P1D,
        forgedAdjustment,
        StubConvention.SHORT_FINAL,
        false))

  /**
   * Reference data resolving the forged identifier, so that the adjustment can be applied.
   *
   * The identifier is the hostile one, and it resolves to the ordinary weekend calendar: a name is
   * text to this library, so a store may perfectly well hold an entry for it.
   */
  private val forgedReferenceData: ReferenceData =
    ImmutableReferenceData.of(HolidayCalendarId.of(ForgedCalendarName), HolidayCalendars.SAT_SUN)

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
  test("test_definitionAttributeSurvivesHostileCalendarNames") {
    // The first half: the attribute is the definition as it renders. The forged name reached the
    // definition through `HolidayCalendarId.of`, which accepts any text, so it is in the rendering
    // with its line feed intact, and the failure carries that rendering character for character.
    val rejection: FailureOr[List[LocalDate]] = forgedBadStubDefinition.createUnadjustedDates()
    rejection should beFailureWith(FailureReason.INVALID)
    val reported: Failure = failureOf(rejection)
    forgedBadStubDefinition.toString should include(ForgedCalendarName)
    reported.attributes.get(DefinitionAttribute) shouldBe Some(forgedBadStubDefinition.toString)
    reported.attributes(DefinitionAttribute) should include(ForgedCalendarName)
    reported.attributes(DefinitionAttribute) should include("\n")

    // The second half: the text of that same failure is one bounded line. The line feed appears in
    // it as the two characters of its escape, which is what makes the text of the failure unable
    // to state a line the library never reported.
    val rendered: String = neutralisedRendering(reported)
    rendered should include("""GBLO\nINVALID: forged""")
    rendered should include(reported.reason.name)

    // The message-carrying case. The duplicate-adjusted rejection quotes the adjustment inside its
    // own message, so the forged name is in the message as well as in the attribute - exactly, as
    // the caller supplied it - while the text of the failure remains a single bounded line. Only
    // the presence of the name is asserted of the message, not the wording around it, which is the
    // Java wording and is asserted where that contract is tested.
    val duplicated: FailureOr[List[LocalDate]] =
      forgedDailyDefinition.createAdjustedDates(forgedReferenceData)
    duplicated should beFailureWith(FailureReason.INVALID)
    val duplicateFailure: Failure = failureOf(duplicated)
    duplicateFailure.message should include("duplicate adjusted dates")
    duplicateFailure.message should include(ForgedCalendarName)
    duplicateFailure.attributes.get(DefinitionAttribute) shouldBe
      Some(forgedDailyDefinition.toString)
    val duplicateRendering: String = neutralisedRendering(duplicateFailure)
    duplicateRendering should include("""GBLO\nINVALID: forged""")

    // The oversized case. A name of a few thousand characters is kept whole by the attribute,
    // because that is the value a caller reads back, and the text of the failure stays under the
    // same ceiling as every other rendering, the writing of it having stopped and marked the part
    // it could not finish.
    val oversized: FailureOr[List[LocalDate]] = oversizedDefinition.createUnadjustedDates()
    oversized should beFailureWith(FailureReason.INVALID)
    val oversizedFailure: Failure = failureOf(oversized)
    val carried: String = oversizedFailure.attributes(DefinitionAttribute)
    carried shouldBe oversizedDefinition.toString
    carried should include(OversizedCalendarName)
    carried.length should be > OversizedCalendarName.length
    OversizedCalendarName.length should be > RenderingCeiling
    val oversizedRendering: String = neutralisedRendering(oversizedFailure)
    oversizedRendering should include("...")
  }

  //-------------------------------------------------------------------------
  /**
   * Asserts of a failure the three properties its text has to have, and answers with that text.
   *
   * The two routes to the text of a failure - the `Show` instance and the text form of the value -
   * are asserted to be the same string, because a caller reaching for either has to get the
   * neutralised rendering rather than whichever of the two happened to be neutralised. What is
   * then asserted of that string is what makes it safe to write into a log or a report: it holds
   * no character for which `Character.isISOControl` holds, so it is one line and carries no
   * terminal control, and it is bounded, so a value quoted by the failure cannot dominate it.
   *
   * @param failure  the failure whose text is asserted
   * @return the text of that failure
   */
  private def neutralisedRendering(failure: Failure): String = {
    val rendered: String = Show[Failure].show(failure)
    rendered shouldBe failure.toString
    rendered.exists(character => character.isControl) shouldBe false
    rendered.linesIterator.size shouldBe 1
    rendered.length should be <= RenderingCeiling
    rendered
  }

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
