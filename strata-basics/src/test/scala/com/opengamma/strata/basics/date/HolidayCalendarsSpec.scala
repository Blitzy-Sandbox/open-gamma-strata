/*
 * Copyright (C) 2018 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.DayOfWeek.SATURDAY
import java.time.DayOfWeek.SUNDAY
import java.time.LocalDate

import cats.Hash
import cats.Show

import io.circe.Json
import io.circe.parser.decode
import io.circe.parser.parse
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.ImmutableReferenceData
import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.TestingReferenceDataId
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[HolidayCalendars]], ported from the Java `HolidayCalendarsTest`.
 *
 * The Java original ran seven annotated methods, and all seven are here under the names they
 * had: `test_defaulting`, `test_defaulting_combinedWith`, `coverage`, `coverage_combined`,
 * `coverage_noHolidays`, `coverage_weekend` and `test_serialization`. Keeping the four
 * `coverage*` methods as four separate tests rather than folding them together is what keeps
 * the method-level traceability of the migration exact.
 *
 * ===What this spec is about===
 *
 * Two of the seven exercise the only behaviour [[HolidayCalendars]] has beyond publishing four
 * constants: [[HolidayCalendars.defaultingReferenceData]], which decorates a set of reference
 * data so that a holiday calendar identifier it does not hold still resolves - to a calendar
 * whose only holidays are Saturday and Sunday and which carries the identifier that was
 * '''asked for'''. That last point is the whole purpose of the decoration and is asserted
 * directly below, because a calendar's identity is observable in its name, its equality, the
 * name of a calendar combined with it and its JSON form; defaulting to the shared `Sat/Sun`
 * calendar would quietly rewrite the identifier a caller resolved.
 *
 * The decorated type's own contract - a composite identifier left unresolved so that its parts
 * are defaulted one by one, and the re-wrapping performed by `combinedWith` - belongs to
 * `HolidaySafeReferenceDataSpec`, and the general precedence rule of combined reference data
 * belongs to `ReferenceDataSpec` and `CombinedReferenceDataSpec`. What is asserted here is what
 * the Java method asserted: the decoration observed through the holder that produces it.
 *
 * ===How the shape of the port changes the assertions===
 *
 *   - `getValue` returns an `Either` where the Java method returned the value or threw
 *     `ReferenceDataNotFoundException`, which is not ported. Each of the nine precedence
 *     assertions of `test_defaulting_combinedWith` is therefore an assertion that the outcome
 *     is a success carrying the expected calendar. The Java file asserted no exception
 *     anywhere, so nothing here asserts a throw; the one failure asserted below is the
 *     identifier that is not a calendar identifier at all, and its reason is compared as a
 *     value of the closed family of reasons rather than as text.
 *   - `coverPrivateConstructor` and `coverImmutableBean` have no counterpart. The first
 *     reflectively invoked the private constructor of a static holder to satisfy a coverage
 *     tool, and a Scala `object` has no constructor to reach; the second swept a Joda bean's
 *     properties, equality, hashing and rendering, and there is no bean - nor any reflection
 *     at all - in this port. The four `coverage*` tests therefore assert what those calls
 *     stood for, over exactly the values the Java methods named.
 *   - `assertSerialization` round-tripped a calendar through Java serialization, which no type
 *     of this port supports. `test_serialization` asserts the JSON form of AAP section 0.6.4
 *     instead, and asserts the concrete document of each of the five values the Java method
 *     named - the property-based round trip over every codec-bearing type is owned by
 *     `json/JsonRoundTripSpec`, which the test mapping records as the consolidated home of the
 *     Java method. A concrete shape is worth asserting here as well: a property cannot say
 *     whether a built-in calendar is written as a name or as an object.
 *
 * ===Why no assertion here reads the built-in calendar set===
 *
 * Nothing in this spec calls [[HolidayCalendars.of]] or touches `ReferenceData.standard`. Both
 * read [[StandardHolidayCalendars]], which is the library's data rather than anything
 * `HolidayCalendars` decides, and the Java methods ported here read neither: they build their
 * own reference data and ask for `GBLO` precisely because it is '''absent'''. The four
 * constants and the composites of weekend calendars asserted below are values whose whole
 * content follows from their names, so every assertion here is a statement about
 * `HolidayCalendars` itself.
 *
 * @see [[HolidaySafeReferenceData]] for the decoration `defaultingReferenceData` applies
 * @see [[HolidayCalendar]] for the sealed family, its composites and its JSON form
 */
class HolidayCalendarsSpec extends AnyFunSuite with Matchers {

  /**
   * The calendar the decoration supplies for the `Fri/Sat` identifier when it is absent.
   *
   * Note what this value is: a calendar '''identified''' as `Fri/Sat` whose holidays are
   * Saturday and Sunday. The identifier is the one that was asked for and the content is the
   * default weekend, and the two need not agree - which is exactly the trade the decoration
   * makes, and the reason a defaulted calendar is only fit for exploratory work.
   */
  private val DEFAULTED_FRI_SAT: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(HolidayCalendarIds.FRI_SAT, Nil, List(SATURDAY, SUNDAY))

  /**
   * The calendar the decoration supplies for the `GBLO` identifier, which no fixture here
   * holds.
   *
   * `GBLO` is the identifier both defaulting tests use to observe the decoration, because it
   * names a real centre that the reference data they build deliberately does not carry.
   */
  private val DEFAULTED_GBLO: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(HolidayCalendarIds.GBLO, Nil, List(SATURDAY, SUNDAY))

  /** A Thursday, so that a `Thu/Fri` weekend can be told from a `Fri/Sat` one. */
  private val THU_2014_07_10: LocalDate = LocalDate.of(2014, 7, 10)

  /** A Friday, a holiday under both `Fri/Sat` and `Thu/Fri`. */
  private val FRI_2014_07_11: LocalDate = LocalDate.of(2014, 7, 11)

  /** A Saturday, a holiday under both `Fri/Sat` and `Sat/Sun`. */
  private val SAT_2014_07_12: LocalDate = LocalDate.of(2014, 7, 12)

  /** A Sunday, a holiday under `Sat/Sun` alone of the three weekend calendars. */
  private val SUN_2014_07_13: LocalDate = LocalDate.of(2014, 7, 13)

  /** A Monday, a business day under every calendar this spec names. */
  private val MON_2014_07_14: LocalDate = LocalDate.of(2014, 7, 14)

  /**
   * Five consecutive days covering every weekend this spec mentions.
   *
   * A test that means to say "these two calendars answer alike" walks this list rather than
   * naming one date, so the statement is about a known set of days rather than about whichever
   * day happened to be chosen.
   */
  private val WEEK_2014_07: List[LocalDate] =
    List(THU_2014_07_10, FRI_2014_07_11, SAT_2014_07_12, SUN_2014_07_13, MON_2014_07_14)

  /** The four calendars [[HolidayCalendars]] publishes, in the order the holder declares them. */
  private val BUILT_IN: List[HolidayCalendar] =
    List(
      HolidayCalendars.NO_HOLIDAYS,
      HolidayCalendars.SAT_SUN,
      HolidayCalendars.FRI_SAT,
      HolidayCalendars.THU_FRI)

  //-------------------------------------------------------------------------
  test("test_defaulting") {
    // The Java original built this store from a single-entry `ImmutableMap`. The Scala factory
    // is entry-based, and it is `ImmutableReferenceData.of` rather than `ReferenceData.of`
    // because the latter layers the four built-in weekend calendars underneath the caller's
    // entries - and a test whose subject is what happens when a calendar is missing needs a
    // store that holds nothing it was not given.
    val base: ReferenceData = store(ReferenceData.Entry(HolidayCalendarIds.FRI_SAT, HolidayCalendars.FRI_SAT))

    val test: ReferenceData = HolidayCalendars.defaultingReferenceData(base)

    // An identifier the underlying data holds is answered from that data, so the decoration
    // defaults nothing over a calendar that exists.
    test.getValue(HolidayCalendarIds.FRI_SAT) should haveValue(HolidayCalendars.FRI_SAT)

    // An identifier it does not hold is defaulted, and the calendar returned carries `GBLO` -
    // not the identifier of the shared `Sat/Sun` calendar. This is the assertion the whole
    // decoration exists for.
    val defaulted = test.getValue(HolidayCalendarIds.GBLO)
    defaulted should haveValue(DEFAULTED_GBLO)
    defaulted.map(calendar => calendar.id) should haveValue(HolidayCalendarIds.GBLO)
    defaulted.map(calendar => calendar.name) should haveValue("GBLO")
    test.findValue(HolidayCalendarIds.GBLO) should not be Some(HolidayCalendars.SAT_SUN)

    // Stated once on the content as well. [[ImmutableHolidayCalendar]] compares equal on its
    // identifier alone, so the assertion above would hold for a defaulted calendar of any
    // content whatever; these four say that what came back really is weekend-only.
    defaulted.map(calendar => calendar.isHoliday(SAT_2014_07_12)) should haveValue(true)
    defaulted.map(calendar => calendar.isHoliday(SUN_2014_07_13)) should haveValue(true)
    defaulted.map(calendar => calendar.isBusinessDay(MON_2014_07_14)) should haveValue(true)
    defaulted.map(calendar => calendar.isBusinessDay(FRI_2014_07_11)) should haveValue(true)

    // Membership is true for the identifier the data holds and for the one it defaults, and
    // the second of those is true of `every` holiday calendar identifier - including a
    // composite, which `findValue` deliberately reports nothing for so that its parts resolve
    // separately. `HolidaySafeReferenceDataSpec` owns that composite behaviour; what matters
    // here is that membership does not understate what a caller can obtain.
    test.containsValue(HolidayCalendarIds.FRI_SAT) shouldBe true
    test.containsValue(HolidayCalendarIds.GBLO) shouldBe true
    test.containsValue(HolidayCalendarId.of("GBLO+USNY")) shouldBe true

    // And false for an identifier that is not a holiday calendar identifier at all: the
    // decoration defaults calendars, and defaulting an identifier whose data this module knows
    // nothing about could only invent a value. Together with the three assertions above, this
    // is the sharpest available characterisation of the decoration.
    test.containsValue(TestingReferenceDataId("1")) shouldBe false

    // The same identifier through `getValue`, which reports the absence the Java method would
    // have seen as a thrown `ReferenceDataNotFoundException`. The reason is compared by value.
    test.getValue(TestingReferenceDataId("1")) should beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  test("test_defaulting_combinedWith") {
    // Two stores that disagree, transcribed from the Java original unchanged. The second files
    // the `Fri/Sat` calendar under the `Thu/Fri` identifier, which is deliberate: a clash can
    // only be observed where the two sides hold different values for one identifier, and a
    // mismatched pairing is the cheapest way to make the difference visible.
    val base1: ReferenceData = store(ReferenceData.Entry(HolidayCalendarIds.THU_FRI, HolidayCalendars.THU_FRI))
    val base2: ReferenceData = store(
      ReferenceData.Entry(HolidayCalendarIds.THU_FRI, HolidayCalendars.FRI_SAT),
      ReferenceData.Entry(HolidayCalendarIds.FRI_SAT, HolidayCalendars.FRI_SAT))

    val testDefaulted: ReferenceData = HolidayCalendars.defaultingReferenceData(base1)
    testDefaulted.getValue(HolidayCalendarIds.THU_FRI) should haveValue(HolidayCalendars.THU_FRI)
    testDefaulted.getValue(HolidayCalendarIds.FRI_SAT) should haveValue(DEFAULTED_FRI_SAT)
    testDefaulted.getValue(HolidayCalendarIds.GBLO) should haveValue(DEFAULTED_GBLO)

    val testCombined: ReferenceData = testDefaulted.combinedWith(base2)
    testCombined.getValue(HolidayCalendarIds.THU_FRI) should haveValue(HolidayCalendars.THU_FRI) // test1 takes precedence
    testCombined.getValue(HolidayCalendarIds.FRI_SAT) should haveValue(HolidayCalendars.FRI_SAT) // from test2
    testCombined.getValue(HolidayCalendarIds.GBLO) should haveValue(DEFAULTED_GBLO) // from default

    val testCombinedReversed: ReferenceData = base2.combinedWith(testDefaulted)
    testCombinedReversed.getValue(HolidayCalendarIds.THU_FRI) should haveValue(HolidayCalendars.FRI_SAT) // test2 takes precedence
    testCombinedReversed.getValue(HolidayCalendarIds.FRI_SAT) should haveValue(HolidayCalendars.FRI_SAT) // from test2
    testCombinedReversed.getValue(HolidayCalendarIds.GBLO) should haveValue(DEFAULTED_GBLO) // from default

    // The two orderings disagree about the clashing identifier and agree about everything else,
    // which is what makes the nine assertions above a statement about precedence rather than
    // nine repetitions of one lookup. The defaulted `GBLO` survives either ordering because
    // the decoration is re-applied around a combination rather than left on one side of it.
    testCombined.findValue(HolidayCalendarIds.THU_FRI) should not be
      testCombinedReversed.findValue(HolidayCalendarIds.THU_FRI)
    testCombined.findValue(HolidayCalendarIds.FRI_SAT) shouldBe
      testCombinedReversed.findValue(HolidayCalendarIds.FRI_SAT)
    testCombined.findValue(HolidayCalendarIds.GBLO) shouldBe
      testCombinedReversed.findValue(HolidayCalendarIds.GBLO)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java method was `coverPrivateConstructor(HolidayCalendars.class)`: it reflectively
    // invoked the private constructor of a static holder so that a coverage tool would not
    // report the class as unexercised. A Scala `object` has no such constructor to reach, and
    // this port performs no reflection of any kind, so the call has no target. What it stood
    // for - that the holder publishes the calendars it is supposed to publish, and nothing of
    // its own - is asserted directly.
    BUILT_IN.map(calendar => calendar.name) shouldBe List("NoHolidays", "Sat/Sun", "Fri/Sat", "Thu/Fri")
    BUILT_IN.map(calendar => calendar.id) shouldBe
      List(
        HolidayCalendarIds.NO_HOLIDAYS,
        HolidayCalendarIds.SAT_SUN,
        HolidayCalendarIds.FRI_SAT,
        HolidayCalendarIds.THU_FRI)

    // Four distinct calendars, so no constant is an alias of another.
    BUILT_IN.distinct should have size 4

    // Each constant is the very value the family declares under the same name, which is what
    // makes this holder a set of names for existing calendars rather than a second source of
    // them: `HolidayCalendars.SAT_SUN` and `HolidayCalendar.SatSun` are one object.
    HolidayCalendars.NO_HOLIDAYS shouldBe HolidayCalendar.NoHolidays
    HolidayCalendars.SAT_SUN shouldBe HolidayCalendar.SatSun
    HolidayCalendars.FRI_SAT shouldBe HolidayCalendar.FriSat
    HolidayCalendars.THU_FRI shouldBe HolidayCalendar.ThuFri

    // The rendering of each, which the reflective sweep also exercised, and the agreement of
    // `Show` with it - the two ways of putting a calendar into a message must not differ.
    BUILT_IN.map(calendar => calendar.toString) shouldBe
      List(
        "HolidayCalendar[NoHolidays]",
        "HolidayCalendar[Sat/Sun]",
        "HolidayCalendar[Fri/Sat]",
        "HolidayCalendar[Thu/Fri]")
    BUILT_IN.foreach(calendar => Show[HolidayCalendar].show(calendar) shouldBe calendar.toString)

    // Equal values hash alike and unequal ones are reported unequal, which is what filing a
    // calendar in a map requires and what the swept bean's generated `equals`/`hashCode` gave.
    BUILT_IN.foreach { calendar =>
      withClue(s"$calendar: ") {
        Hash[HolidayCalendar].eqv(calendar, calendar) shouldBe true
        Hash[HolidayCalendar].hash(calendar) shouldBe calendar.hashCode
      }
    }
    Hash[HolidayCalendar].eqv(HolidayCalendars.SAT_SUN, HolidayCalendars.FRI_SAT) shouldBe false
  }

  //-------------------------------------------------------------------------
  test("coverage_combined") {
    // The Java method swept `FRI_SAT.combinedWith(SAT_SUN)` as an immutable bean. The value is
    // the subject here too, and the properties that sweep stood for - the parts read back,
    // equality, hashing and rendering - are asserted over it.
    val test: HolidayCalendar = HolidayCalendars.FRI_SAT.combinedWith(HolidayCalendars.SAT_SUN)
    val same: HolidayCalendar = HolidayCalendars.FRI_SAT.combinedWith(HolidayCalendars.SAT_SUN)
    val other: HolidayCalendar = HolidayCalendars.FRI_SAT.combinedWith(HolidayCalendars.THU_FRI)
    val swapped: HolidayCalendar = HolidayCalendars.SAT_SUN.combinedWith(HolidayCalendars.FRI_SAT)

    // The parts read back as they were given, in the order they were given.
    test shouldBe HolidayCalendar.Combined(HolidayCalendars.FRI_SAT, HolidayCalendars.SAT_SUN)

    // A composite compares structurally - unlike a weekend calendar, which compares by
    // identity - so a combination built twice is one value, and equal values hash alike.
    test shouldBe same
    test.hashCode shouldBe same.hashCode
    Hash[HolidayCalendar].eqv(test, same) shouldBe true
    Hash[HolidayCalendar].hash(test) shouldBe Hash[HolidayCalendar].hash(same)

    // A combination of a different pair is a different value.
    test should not be other
    Hash[HolidayCalendar].eqv(test, other) shouldBe false

    // Swapping the operands yields a value that is NOT equal to the original: `combinedWith`
    // does not normalise the order of its parts, and the order is part of the value it builds.
    // Recorded deliberately, because the identifier behaves the other way - a composite
    // identifier sorts its components, so both orderings are named `Fri/Sat+Sat/Sun`. The two
    // therefore differ as values while agreeing on identity, which is sound because a
    // combination observes both calendars and so answers identically either way round.
    test should not be swapped
    Hash[HolidayCalendar].eqv(test, swapped) shouldBe false
    test.id shouldBe swapped.id
    test.id shouldBe HolidayCalendarIds.FRI_SAT.combinedWith(HolidayCalendarIds.SAT_SUN)
    test.name shouldBe "Fri/Sat+Sat/Sun"
    WEEK_2014_07.foreach { date =>
      withClue(s"$date: ")(swapped.isHoliday(date) shouldBe test.isHoliday(date))
    }

    // What the combination answers: the holidays of both parts, so Friday, Saturday and Sunday
    // are holidays and the working week is otherwise intact.
    test.isHoliday(FRI_2014_07_11) shouldBe true
    test.isHoliday(SAT_2014_07_12) shouldBe true
    test.isHoliday(SUN_2014_07_13) shouldBe true
    test.isBusinessDay(THU_2014_07_10) shouldBe true
    test.isBusinessDay(MON_2014_07_14) shouldBe true

    // Rendering, which the sweep read through the bean's `toString`.
    test.toString shouldBe "HolidayCalendar[Fri/Sat+Sat/Sun]"
    Show[HolidayCalendar].show(test) shouldBe test.toString
    Show[HolidayCalendar].show(test) should not be empty
  }

  //-------------------------------------------------------------------------
  test("coverage_noHolidays") {
    // As `coverage_combined`, for the calendar under which every day is a business day.
    val test: HolidayCalendar = HolidayCalendars.NO_HOLIDAYS

    test.id shouldBe HolidayCalendarIds.NO_HOLIDAYS
    test.name shouldBe "NoHolidays"

    // Equality is identity for this calendar, so another reference to it is the same value and
    // hashes alike, while any other calendar of the family is unequal to it.
    test shouldBe HolidayCalendar.NoHolidays
    test.hashCode shouldBe HolidayCalendar.NoHolidays.hashCode
    Hash[HolidayCalendar].eqv(test, HolidayCalendar.NoHolidays) shouldBe true
    Hash[HolidayCalendar].eqv(test, HolidayCalendars.SAT_SUN) shouldBe false

    // No day of any week is a holiday, which is the whole content of this calendar.
    WEEK_2014_07.foreach { date =>
      withClue(s"$date: ") {
        test.isHoliday(date) shouldBe false
        test.isBusinessDay(date) shouldBe true
      }
    }

    // And the two algebraic properties the type documents, which a reflective sweep could
    // never have reached: it is the identity of combining and the absorbing element of linking.
    test.combinedWith(HolidayCalendars.SAT_SUN) shouldBe HolidayCalendars.SAT_SUN
    HolidayCalendars.SAT_SUN.combinedWith(test) shouldBe HolidayCalendars.SAT_SUN
    test.linkedWith(HolidayCalendars.SAT_SUN) shouldBe test
    HolidayCalendars.SAT_SUN.linkedWith(test) shouldBe test

    test.toString shouldBe "HolidayCalendar[NoHolidays]"
    Show[HolidayCalendar].show(test) shouldBe test.toString
    Show[HolidayCalendar].show(test) should not be empty
  }

  //-------------------------------------------------------------------------
  test("coverage_weekend") {
    // As `coverage_combined`, for the weekend calendar the Java method named.
    val test: HolidayCalendar = HolidayCalendars.FRI_SAT

    test.id shouldBe HolidayCalendarIds.FRI_SAT
    test.name shouldBe "Fri/Sat"

    // A weekend calendar compares by identity, so another reference to `FRI_SAT` is the same
    // value and hashes alike, and the other two weekend calendars are different values.
    test shouldBe HolidayCalendar.FriSat
    test.hashCode shouldBe HolidayCalendar.FriSat.hashCode
    Hash[HolidayCalendar].eqv(test, HolidayCalendar.FriSat) shouldBe true
    test should not be HolidayCalendars.SAT_SUN
    test should not be HolidayCalendars.THU_FRI
    Hash[HolidayCalendar].eqv(test, HolidayCalendars.SAT_SUN) shouldBe false
    Hash[HolidayCalendar].eqv(test, HolidayCalendars.THU_FRI) shouldBe false

    // Sharing an identifier is not enough: a calendar carrying holiday data is never equal to
    // a weekend calendar, as in the library being ported. `DEFAULTED_FRI_SAT` is precisely such
    // a calendar - the one the defaulting above supplies under this very identifier - so the
    // two agree on identity and differ both as values and in what they answer.
    test.id shouldBe DEFAULTED_FRI_SAT.id
    test should not be DEFAULTED_FRI_SAT
    DEFAULTED_FRI_SAT should not be test
    test.isHoliday(SUN_2014_07_13) shouldBe false
    DEFAULTED_FRI_SAT.isHoliday(SUN_2014_07_13) shouldBe true

    // What this calendar answers: Friday and Saturday are holidays, and nothing else is.
    test.isHoliday(FRI_2014_07_11) shouldBe true
    test.isHoliday(SAT_2014_07_12) shouldBe true
    test.isBusinessDay(THU_2014_07_10) shouldBe true
    test.isBusinessDay(SUN_2014_07_13) shouldBe true
    test.isBusinessDay(MON_2014_07_14) shouldBe true

    test.toString shouldBe "HolidayCalendar[Fri/Sat]"
    Show[HolidayCalendar].show(test) shouldBe test.toString
    Show[HolidayCalendar].show(test) should not be empty
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // The Java method asserted a Java-serialization round trip of the four constants and of
    // one composite. No type of this port supports Java serialization, so the equivalent is
    // the JSON form of AAP section 0.6.4, asserted over exactly those five values.
    //
    // A calendar this library defines is written as its bare name, because the name locates
    // the same calendar again; only a calendar an application built itself is written
    // structurally. Each of the four constants is therefore a JSON string and not an object,
    // which is asserted rather than merely round-tripped: a round trip alone cannot tell the
    // two forms apart.
    HolidayCalendars.NO_HOLIDAYS.asJson shouldBe Json.fromString("NoHolidays")
    HolidayCalendars.SAT_SUN.asJson shouldBe Json.fromString("Sat/Sun")
    HolidayCalendars.FRI_SAT.asJson shouldBe Json.fromString("Fri/Sat")
    HolidayCalendars.THU_FRI.asJson shouldBe Json.fromString("Thu/Fri")
    BUILT_IN.foreach(calendar => withClue(s"$calendar: ")(calendar.asJson.isString shouldBe true))

    // And each reads back as the same value. These four names are answered by the family
    // itself, so a document naming one of them is read by an application that supplies no
    // reference data at all.
    BUILT_IN.foreach { calendar =>
      withClue(s"$calendar: ") {
        decode[HolidayCalendar](calendar.asJson.noSpaces) shouldBe Right(calendar)
      }
    }

    // The composite is written as an object naming its kind, with its two parts written the
    // same way - so the order of the parts, which is part of the value, survives the round
    // trip. The document is asserted in full and then read back.
    val combined: HolidayCalendar = HolidayCalendars.FRI_SAT.combinedWith(HolidayCalendars.SAT_SUN)
    val expected: Json = parse("""{"Combined":{"a":"Fri/Sat","b":"Sat/Sun"}}""")
      .getOrElse(fail("the expected JSON of this test is not valid JSON"))
    combined.asJson shouldBe expected
    decode[HolidayCalendar](combined.asJson.noSpaces) shouldBe Right(combined)

    // Equal values encode to identical bytes, which is the property that makes a stored
    // document comparable with a freshly written one.
    combined.asJson.noSpaces shouldBe
      HolidayCalendars.FRI_SAT.combinedWith(HolidayCalendars.SAT_SUN).asJson.noSpaces
    HolidayCalendars.SAT_SUN.combinedWith(HolidayCalendars.FRI_SAT).asJson.noSpaces should not be
      combined.asJson.noSpaces
  }

  //-------------------------------------------------------------------------
  /**
   * Builds a set of reference data holding exactly the entries given.
   *
   * `ImmutableReferenceData.of` is used rather than `ReferenceData.of` because the latter
   * layers the four built-in weekend calendars underneath the caller's entries, and every
   * assertion in this spec about a defaulted calendar depends on the store holding nothing it
   * was not given - `Fri/Sat` and `Thu/Fri` are two of those four.
   *
   * That factory reports the one way it can fail, two entries filed under the same identifier,
   * so there is an outcome to unwrap. A fixture that cannot be built is a defect in this spec
   * rather than a property of the subject, so it is reported as a failed test naming the cause
   * instead of being forced; the Java original could not express the case at all, since the
   * `Map` it passed in had already resolved any duplicate by insertion order.
   *
   * @param entries  the reference data entries, which must not repeat an identifier
   * @return the store holding exactly those entries
   */
  private def store(entries: ReferenceData.Entry[_]*): ImmutableReferenceData =
    ImmutableReferenceData.of(entries: _*) match {
      case Right(data) => data
      case Left(failure) => fail(s"Fixture reference data could not be built: ${failure.message}")
    }
}
