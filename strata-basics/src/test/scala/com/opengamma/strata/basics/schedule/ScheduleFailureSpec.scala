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
 * Test the way a schedule that cannot be calculated is reported.
 *
 * A schedule operation that cannot calculate the definition it is given reports
 * `Left(Failure.Invalid)`, and the definition it rejected travels with that failure, as the entry
 * of its attribute map under the key `definition`. The first two tests assert that pair of
 * properties twice over. A failure constructed by hand proves nothing about this library - the
 * failure type belongs to the collect module and holds whatever it is handed - so each of them
 * also drives a '''real''' rejection through the schedule types and asserts the same two
 * properties of what comes back: a rejection reached from a definition names that definition,
 * while one reached from a schedule has no definition to name and so names nothing.
 *
 * ===Faithful attribute, neutralised text===
 *
 * The definition a failure carries embeds a business day adjustment, which names a
 * [[com.opengamma.strata.basics.date.HolidayCalendarId]] whose name is accepted as given:
 * `HolidayCalendarId.of` is total, so a calendar name may hold a line feed or several thousand
 * characters, and that text reaches both the `definition` attribute and - through the adjustment
 * quoted by the duplicate-adjusted message - a failure message. Two properties have to hold at
 * once for that to be both useful and safe, and the third test states both:
 *
 *   - the structured value stays '''faithful''': the attribute is the definition's own rendering,
 *     character for character, control characters included, because a caller compares what the
 *     failure names with what it supplied, and a report naming the rejected definition would
 *     otherwise read a summary of it;
 *   - the '''text''' of the failure is neutralised: the two routes to it agree, it holds no
 *     control character, it is one line and it is bounded, so a forged calendar name cannot
 *     fabricate a line of a log or a report that holds the failure, and an oversized one cannot
 *     dominate the rendering.
 *
 * The rendering of the definition is not invented here: [[PeriodicSchedule]] attaches its own
 * rendering, so that is what is asserted.
 */
class ScheduleFailureSpec extends AnyFunSuite with Matchers with ResultMatchers {

  /**
   * The attribute key the rejected schedule definition is carried under.
   *
   * The production code holds this name privately, so the spec states it independently rather
   * than reading it back from the code under test.
   */
  private val DefinitionAttribute: String = "definition"

  /** An arbitrary message, carried by the failures the first two tests construct directly. */
  private val FormattedMessage: String = "Hello World"

  //-------------------------------------------------------------------------
  // The dates of the two definitions below: 30th June to 30th August, which generates a
  // schedule, and 4th June to 17th September, which does not.

  private val JUN_30: LocalDate = date(2014, 6, 30)

  private val AUG_30: LocalDate = date(2014, 8, 30)

  private val JUN_04: LocalDate = date(2014, 6, 4)

  private val SEP_17: LocalDate = date(2014, 9, 17)

  //-------------------------------------------------------------------------
  /**
   * A definition that generates a schedule.
   *
   * 30th June to 30th August by `P1M` divides evenly, so no stub is needed and the 'None' stub
   * convention is satisfied: this definition is valid, which is the point of it. It is what the
   * constructed failure of `test_withDefinition` names, and it is also the definition the merge
   * rejection of `test_withoutDefinition` is reached through.
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
   * A definition that is valid but generates no schedule: 4th June to 17th September by `P1M`
   * rolling on day 4 needs a stub, which the 'None' stub convention forbids.
   *
   * Nothing about a stub is decided by [[PeriodicSchedule.of]] - deciding it needs the schedule
   * rolled out - so this definition is accepted on construction and rejected later, by
   * generation.
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

  /**
   * The same name as a reader receives it, with its line feed written as the two characters of an
   * escape.
   *
   * This is what the text form of an identifier produces for that name, and therefore what every
   * rendering carrying the identifier holds: the rendering of the definition, the attribute taken
   * from it, the message that quotes the adjustment, and the text of the failure itself.
   */
  private val NeutralisedForgedName: String = """GBLO\nINVALID: forged"""

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
   * definition's own rendering bounds it before the failure is even built, because the text form
   * of an identifier is bounded.
   */
  private val RenderingCeiling: Int = 1200

  /** The adjustment that carries the forged calendar name into a definition. */
  private val forgedAdjustment: BusinessDayAdjustment =
    BusinessDayAdjustment.of(MODIFIED_FOLLOWING, HolidayCalendarId.of(ForgedCalendarName))

  /**
   * The definition that generates no schedule, adjusted by a calendar whose name is forged.
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

  /** The same definition adjusted by a calendar whose name is several thousand characters long. */
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
    // the shape being asserted: a message, and the rejected definition attached under the key
    // the production code uses
    val test: Failure =
      Failure.Invalid(FormattedMessage).withAttribute(DefinitionAttribute, definition.toString)
    test.reason shouldBe FailureReason.INVALID
    test.message shouldBe FormattedMessage
    test.attributes.get(DefinitionAttribute) shouldBe Some(definition.toString)
    test shouldBe Failure.Invalid(
      FormattedMessage,
      SortedMap(DefinitionAttribute -> definition.toString))

    // the real path: generation reports its rejection as a value carrying the definition it
    // rejected - that attribute is attached by the production code, not by this spec
    val rejection: FailureOr[List[LocalDate]] = badStubDefinition.createUnadjustedDates()
    rejection should beFailureWith(FailureReason.INVALID)
    rejection should haveFailureMessageMatching(".*resulted in a disallowed stub.*")
    val reported = failureOf(rejection)
    reported shouldBe a[Failure.Invalid]
    reported.attributes.get(DefinitionAttribute) shouldBe Some(badStubDefinition.toString)
  }

  //-------------------------------------------------------------------------
  test("test_withoutDefinition") {
    // the shape being asserted where there is no definition to name: the same message, and an
    // attribute map that holds nothing
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
    // The first half: the attribute is the definition as it renders, and a definition renders its
    // calendar by asking the identifier for its text form. That form is where a name a caller
    // supplied is made safe to write out, so the forged name reaches the rendering - and from
    // there the attribute - with its line feed escaped, while the identifier itself still answers
    // with the whole of the text it was built from, which is what a caller correcting its input
    // reads back.
    val rejection: FailureOr[List[LocalDate]] = forgedBadStubDefinition.createUnadjustedDates()
    rejection should beFailureWith(FailureReason.INVALID)
    val reported: Failure = failureOf(rejection)
    forgedAdjustment.calendar.name shouldBe ForgedCalendarName
    forgedBadStubDefinition.toString should include(NeutralisedForgedName)
    forgedBadStubDefinition.toString should not include ForgedCalendarName
    forgedBadStubDefinition.toString.linesIterator.size shouldBe 1
    reported.attributes.get(DefinitionAttribute) shouldBe Some(forgedBadStubDefinition.toString)
    reported.attributes(DefinitionAttribute) should include(NeutralisedForgedName)
    reported.attributes(DefinitionAttribute) should not include "\n"

    // The second half: the text of that same failure is one bounded line. The line feed appears in
    // it as the two characters of its escape, which is what makes the text of the failure unable
    // to state a line the library never reported.
    val rendered: String = neutralisedRendering(reported)
    rendered should include(NeutralisedForgedName)
    rendered should include(reported.reason.name)

    // The message-carrying case. The duplicate-adjusted rejection quotes the adjustment inside its
    // own message, so the neutralised name is in the message as well as in the attribute, and the
    // text of the failure remains a single bounded line. Only the presence of the name is asserted
    // of the message, not the wording around it, which is the Java wording and is asserted where
    // that contract is tested.
    val duplicated: FailureOr[List[LocalDate]] =
      forgedDailyDefinition.createAdjustedDates(forgedReferenceData)
    duplicated should beFailureWith(FailureReason.INVALID)
    val duplicateFailure: Failure = failureOf(duplicated)
    duplicateFailure.message should include("duplicate adjusted dates")
    duplicateFailure.message should include(NeutralisedForgedName)
    duplicateFailure.message should not include ForgedCalendarName
    duplicateFailure.attributes.get(DefinitionAttribute) shouldBe
      Some(forgedDailyDefinition.toString)
    val duplicateRendering: String = neutralisedRendering(duplicateFailure)
    duplicateRendering should include(NeutralisedForgedName)

    // The oversized case. A name of a few thousand characters is bounded by the identifier's text
    // form, so the definition's rendering - and the attribute taken from it - carries the marker
    // standing for what was left out rather than the whole name, while the identifier keeps every
    // character of it; the text of the failure then stays under the same ceiling as every other
    // rendering.
    val oversized: FailureOr[List[LocalDate]] = oversizedDefinition.createUnadjustedDates()
    oversized should beFailureWith(FailureReason.INVALID)
    val oversizedFailure: Failure = failureOf(oversized)
    val carried: String = oversizedFailure.attributes(DefinitionAttribute)
    carried shouldBe oversizedDefinition.toString
    carried should not include OversizedCalendarName
    carried should include("...")
    carried.length should be < OversizedCalendarName.length
    oversizedDefinition.businessDayAdjustment.calendar.name shouldBe OversizedCalendarName
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
   * no ISO control character, so it is one line and carries no terminal control, and it is
   * bounded, so a value quoted by the failure cannot dominate it.
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
