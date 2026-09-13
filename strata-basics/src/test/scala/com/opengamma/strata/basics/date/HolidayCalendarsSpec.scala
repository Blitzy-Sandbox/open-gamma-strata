/*
 * Copyright (C) 2018 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.DayOfWeek.FRIDAY
import java.time.DayOfWeek.SATURDAY
import java.time.DayOfWeek.SUNDAY
import java.time.DayOfWeek.THURSDAY
import java.time.LocalDate
import java.util.concurrent.atomic.AtomicInteger

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
 * Test [[HolidayCalendars]].
 *
 * Beyond publishing the four constants, [[HolidayCalendars]] has two behaviours.
 * [[HolidayCalendars.of]] resolves a name against the built-in set - canonical or English upper
 * case, a composite name part by part - and reports a name in neither key space as a failure.
 * [[HolidayCalendars.defaultingReferenceData]] decorates reference data so that a holiday
 * calendar identifier it does not hold still resolves - to a calendar whose only holidays are
 * Saturday and Sunday and which carries the identifier that was '''asked for''', not that of the
 * shared `Sat/Sun` calendar, which would quietly rewrite what a caller resolved. Both answer an
 * `Either`, so every lookup below is asserted as an outcome and every failure compared as a
 * `FailureReason` value.
 *
 * The suite also covers how the built-in set is '''presented''' as reference data, because that
 * is a property of the calendars rather than of the store: `ReferenceData.standard` is built from
 * `StandardHolidayCalendars.deferredByIdentifier` through
 * `ImmutableReferenceData.ofDeferredMap`, so a lookup generates the one calendar it names and the
 * four enumeration and comparison members generate every one. Those tests state the mechanism -
 * which thunk ran, and how often - rather than a duration, so they assert what the design
 * guarantees instead of what a machine happened to measure.
 *
 * @see [[HolidaySafeReferenceData]] for the decoration `defaultingReferenceData` applies
 * @see [[HolidayCalendar]] for the sealed family, its composites and its JSON form
 * @see [[com.opengamma.strata.basics.ImmutableReferenceData]] for the store the built-in set is
 *   presented through, and for what its deferral does and does not change
 */
class HolidayCalendarsSpec extends AnyFunSuite with Matchers {

  /**
   * The calendar the decoration supplies for an absent `Fri/Sat` identifier: one '''identified'''
   * as `Fri/Sat` whose holidays are Saturday and Sunday, so identifier and content need not
   * agree - which is why a defaulted calendar is only fit for exploratory work.
   */
  private val DEFAULTED_FRI_SAT: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(HolidayCalendarIds.FRI_SAT, Nil, List(SATURDAY, SUNDAY))

  /**
   * The calendar the decoration supplies for `GBLO`, the identifier both defaulting tests use to
   * observe it because it names a real centre their reference data deliberately does not carry.
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
   * Five consecutive days covering every weekend this spec mentions, so a test saying "these two
   * calendars answer alike" walks a known set of days rather than one chosen date.
   */
  private val WEEK_2014_07: List[LocalDate] =
    List(THU_2014_07_10, FRI_2014_07_11, SAT_2014_07_12, SUN_2014_07_13, MON_2014_07_14)

  /**
   * The first identifier of the counting fixture, named so that it is no built-in calendar.
   *
   * The three identifiers below are what the deferred-store tests file their counting ways of
   * obtaining a calendar under. None of them is a name this library defines, so nothing those
   * tests observe can be answered by the built-in set instead of by the fixture.
   */
  private val FIRST_ID: HolidayCalendarId = HolidayCalendarId.of("XAAA")

  /** The second identifier of the counting fixture. */
  private val SECOND_ID: HolidayCalendarId = HolidayCalendarId.of("XBBB")

  /**
   * The third identifier of the counting fixture, the one no test ever asks the store for
   * directly - so its count is what says a lookup for another identifier produced nothing here.
   */
  private val THIRD_ID: HolidayCalendarId = HolidayCalendarId.of("XCCC")

  /** The three identifiers of the counting fixture, in the order they are filed. */
  private val COUNTED_IDS: List[HolidayCalendarId] = List(FIRST_ID, SECOND_ID, THIRD_ID)

  /** The four calendars [[HolidayCalendars]] publishes, in the order the holder declares them. */
  private val BUILT_IN: List[HolidayCalendar] =
    List(
      HolidayCalendars.NO_HOLIDAYS,
      HolidayCalendars.SAT_SUN,
      HolidayCalendars.FRI_SAT,
      HolidayCalendars.THU_FRI)

  //-------------------------------------------------------------------------
  test("test_defaulting") {
    val base: ReferenceData = store(ReferenceData.Entry(HolidayCalendarIds.FRI_SAT, HolidayCalendars.FRI_SAT))

    val test: ReferenceData = HolidayCalendars.defaultingReferenceData(base)

    // an identifier the underlying data holds is answered from that data
    test.getValue(HolidayCalendarIds.FRI_SAT) should haveValue(HolidayCalendars.FRI_SAT)

    // an identifier it does not hold is defaulted, carrying `GBLO` rather than `Sat/Sun`
    val defaulted = test.getValue(HolidayCalendarIds.GBLO)
    defaulted should haveValue(DEFAULTED_GBLO)
    defaulted.map(calendar => calendar.id) should haveValue(HolidayCalendarIds.GBLO)
    defaulted.map(calendar => calendar.name) should haveValue("GBLO")
    test.findValue(HolidayCalendarIds.GBLO) should not be Some(HolidayCalendars.SAT_SUN)

    // `ImmutableHolidayCalendar` compares equal on its identifier alone, so the assertions above
    // hold for a defaulted calendar of any content; these say it is weekend-only
    defaulted.map(calendar => calendar.isHoliday(SAT_2014_07_12)) should haveValue(true)
    defaulted.map(calendar => calendar.isHoliday(SUN_2014_07_13)) should haveValue(true)
    defaulted.map(calendar => calendar.isBusinessDay(MON_2014_07_14)) should haveValue(true)
    defaulted.map(calendar => calendar.isBusinessDay(FRI_2014_07_11)) should haveValue(true)

    // membership is true of every calendar identifier, including composites `findValue` skips
    test.containsValue(HolidayCalendarIds.FRI_SAT) shouldBe true
    test.containsValue(HolidayCalendarIds.GBLO) shouldBe true
    test.containsValue(HolidayCalendarId.of("GBLO+USNY")) shouldBe true

    // and false for an identifier of another kind, whose data defaulting could only invent
    test.containsValue(TestingReferenceDataId("1")) shouldBe false

    test.getValue(TestingReferenceDataId("1")) should beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  test("test_defaulting_combinedWith") {
    // Two stores that disagree: the second files `Fri/Sat` under the `Thu/Fri` identifier
    // deliberately, a clash being observable only where the two hold different values for one.
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

    // The orderings disagree about the clashing identifier and agree about everything else; the
    // defaulted `GBLO` survives either, the decoration being re-applied around a combination.
    testCombined.findValue(HolidayCalendarIds.THU_FRI) should not be
      testCombinedReversed.findValue(HolidayCalendarIds.THU_FRI)
    testCombined.findValue(HolidayCalendarIds.FRI_SAT) shouldBe
      testCombinedReversed.findValue(HolidayCalendarIds.FRI_SAT)
    testCombined.findValue(HolidayCalendarIds.GBLO) shouldBe
      testCombinedReversed.findValue(HolidayCalendarIds.GBLO)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The holder publishes the calendars it is supposed to publish, and nothing of its own.
    BUILT_IN.map(calendar => calendar.name) shouldBe List("NoHolidays", "Sat/Sun", "Fri/Sat", "Thu/Fri")
    BUILT_IN.map(calendar => calendar.id) shouldBe
      List(
        HolidayCalendarIds.NO_HOLIDAYS,
        HolidayCalendarIds.SAT_SUN,
        HolidayCalendarIds.FRI_SAT,
        HolidayCalendarIds.THU_FRI)

    BUILT_IN.distinct should have size 4

    // each constant is the value the family declares under the same name, not a second source
    HolidayCalendars.NO_HOLIDAYS shouldBe HolidayCalendar.NoHolidays
    HolidayCalendars.SAT_SUN shouldBe HolidayCalendar.SatSun
    HolidayCalendars.FRI_SAT shouldBe HolidayCalendar.FriSat
    HolidayCalendars.THU_FRI shouldBe HolidayCalendar.ThuFri

    // `Show` must not differ from `toString` - two ways of putting a calendar in a message
    BUILT_IN.map(calendar => calendar.toString) shouldBe
      List(
        "HolidayCalendar[NoHolidays]",
        "HolidayCalendar[Sat/Sun]",
        "HolidayCalendar[Fri/Sat]",
        "HolidayCalendar[Thu/Fri]")
    BUILT_IN.foreach(calendar => Show[HolidayCalendar].show(calendar) shouldBe calendar.toString)

    // equal values hash alike, which is what filing a calendar in a map requires
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
    val test: HolidayCalendar = HolidayCalendars.FRI_SAT.combinedWith(HolidayCalendars.SAT_SUN)
    val same: HolidayCalendar = HolidayCalendars.FRI_SAT.combinedWith(HolidayCalendars.SAT_SUN)
    val other: HolidayCalendar = HolidayCalendars.FRI_SAT.combinedWith(HolidayCalendars.THU_FRI)
    val swapped: HolidayCalendar = HolidayCalendars.SAT_SUN.combinedWith(HolidayCalendars.FRI_SAT)

    test shouldBe HolidayCalendar.Combined(HolidayCalendars.FRI_SAT, HolidayCalendars.SAT_SUN)

    // A composite compares structurally - unlike a weekend calendar, which compares by
    // identity - so a combination built twice is one value, and equal values hash alike.
    test shouldBe same
    test.hashCode shouldBe same.hashCode
    Hash[HolidayCalendar].eqv(test, same) shouldBe true
    Hash[HolidayCalendar].hash(test) shouldBe Hash[HolidayCalendar].hash(same)

    test should not be other
    Hash[HolidayCalendar].eqv(test, other) shouldBe false

    // Swapping the operands yields a value NOT equal to the original: `combinedWith` does not
    // normalise the order of its parts, and the order is part of the value, while the identifier
    // sorts its components - sound, because a combination answers identically either way round.
    test should not be swapped
    Hash[HolidayCalendar].eqv(test, swapped) shouldBe false
    test.id shouldBe swapped.id
    test.id shouldBe HolidayCalendarIds.FRI_SAT.combinedWith(HolidayCalendarIds.SAT_SUN)
    test.name shouldBe "Fri/Sat+Sat/Sun"
    WEEK_2014_07.foreach { date =>
      withClue(s"$date: ")(swapped.isHoliday(date) shouldBe test.isHoliday(date))
    }

    // the combination answers with the holidays of both parts
    test.isHoliday(FRI_2014_07_11) shouldBe true
    test.isHoliday(SAT_2014_07_12) shouldBe true
    test.isHoliday(SUN_2014_07_13) shouldBe true
    test.isBusinessDay(THU_2014_07_10) shouldBe true
    test.isBusinessDay(MON_2014_07_14) shouldBe true

    test.toString shouldBe "HolidayCalendar[Fri/Sat+Sat/Sun]"
    Show[HolidayCalendar].show(test) shouldBe test.toString
    Show[HolidayCalendar].show(test) should not be empty
  }

  //-------------------------------------------------------------------------
  test("coverage_noHolidays") {
    val test: HolidayCalendar = HolidayCalendars.NO_HOLIDAYS

    test.id shouldBe HolidayCalendarIds.NO_HOLIDAYS
    test.name shouldBe "NoHolidays"

    // equality is identity for this calendar, and any other calendar of the family is unequal
    test shouldBe HolidayCalendar.NoHolidays
    test.hashCode shouldBe HolidayCalendar.NoHolidays.hashCode
    Hash[HolidayCalendar].eqv(test, HolidayCalendar.NoHolidays) shouldBe true
    Hash[HolidayCalendar].eqv(test, HolidayCalendars.SAT_SUN) shouldBe false

    WEEK_2014_07.foreach { date =>
      withClue(s"$date: ") {
        test.isHoliday(date) shouldBe false
        test.isBusinessDay(date) shouldBe true
      }
    }

    // it is the identity of combining and the absorbing element of linking
    test.combinedWith(HolidayCalendars.SAT_SUN) shouldBe HolidayCalendars.SAT_SUN
    HolidayCalendars.SAT_SUN.combinedWith(test) shouldBe HolidayCalendars.SAT_SUN
    test.linkedWith(HolidayCalendars.SAT_SUN) shouldBe test
    HolidayCalendars.SAT_SUN.linkedWith(test) shouldBe test

    test.toString shouldBe "HolidayCalendar[NoHolidays]"
    Show[HolidayCalendar].show(test) shouldBe test.toString
    Show[HolidayCalendar].show(test) should not be empty
  }

  //-------------------------------------------------------------------------
  test("test_of_name") {
    // Each calendar resolves under its canonical name and under the English upper-case of it,
    // asserted one by one because a key space missing half its keys still answers the canonical.
    HolidayCalendars.of("NoHolidays") should haveValue(HolidayCalendars.NO_HOLIDAYS)
    HolidayCalendars.of("Sat/Sun") should haveValue(HolidayCalendars.SAT_SUN)
    HolidayCalendars.of("Fri/Sat") should haveValue(HolidayCalendars.FRI_SAT)
    HolidayCalendars.of("Thu/Fri") should haveValue(HolidayCalendars.THU_FRI)
    HolidayCalendars.of("NOHOLIDAYS") should haveValue(HolidayCalendars.NO_HOLIDAYS)
    HolidayCalendars.of("SAT/SUN") should haveValue(HolidayCalendars.SAT_SUN)
    HolidayCalendars.of("FRI/SAT") should haveValue(HolidayCalendars.FRI_SAT)
    HolidayCalendars.of("THU/FRI") should haveValue(HolidayCalendars.THU_FRI)

    HolidayCalendars.of("SAT/SUN").getOrElse(fail("Sat/Sun was not found")) should
      be theSameInstanceAs HolidayCalendars.SAT_SUN

    val london = HolidayCalendars.of("GBLO").getOrElse(fail("GBLO was not found"))
    london.id shouldBe HolidayCalendarIds.GBLO
    london should be theSameInstanceAs StandardHolidayCalendars.GBLO

    // composite names are split on '+' and each part looked up exactly as a whole name is
    val weekendComposite = HolidayCalendars.of("SAT/SUN+THU/FRI").getOrElse(fail("the composite was not found"))
    weekendComposite shouldBe HolidayCalendars.SAT_SUN.combinedWith(HolidayCalendars.THU_FRI)
    weekendComposite.name shouldBe "Sat/Sun+Thu/Fri"
    val londonComposite = HolidayCalendars.of("GBLO+SAT/SUN").getOrElse(fail("the composite was not found"))
    londonComposite.name shouldBe "GBLO+Sat/Sun"
    londonComposite.isHoliday(SAT_2014_07_12) shouldBe true

    // A name in neither key space fails rather than returning an empty result, and the failure
    // carries the name: an unknown name, a lower-case one - case is not folded - and a trailing
    // separator, whose empty part names nothing. Each reason is compared as a `FailureReason`.
    HolidayCalendars.of("Unknown") should beFailureWith(FailureReason.PARSING)
    HolidayCalendars.of("gblo") should beFailureWith(FailureReason.PARSING)
    HolidayCalendars.of("GBLO+") should beFailureWith(FailureReason.PARSING)
    HolidayCalendars.of("Unknown").swap.map(failure => failure.attributes.get("name")) shouldBe
      Right(Some("Unknown"))

    // each calendar is filed under the identifier it carries, so a name resolves to a calendar
    // that agrees about what it is
    StandardHolidayCalendars.all should have size 30
    StandardHolidayCalendars.all.foreach {
      case (id, calendar) => withClue(s"$id: ")(calendar.id shouldBe id)
    }
    StandardHolidayCalendars.minimal should have size 4
    StandardHolidayCalendars.all.foreach {
      case (id, calendar) =>
        withClue(s"$id: ") {
          StandardHolidayCalendars.byName(id.name) shouldBe Some(calendar)
          StandardHolidayCalendars.byUpperName(id.name) shouldBe Some(calendar)
          HolidayCalendars.of(id.name) should haveValue(calendar)
        }
    }
  }

  //-------------------------------------------------------------------------
  test("coverage_weekend") {
    val test: HolidayCalendar = HolidayCalendars.FRI_SAT

    test.id shouldBe HolidayCalendarIds.FRI_SAT
    test.name shouldBe "Fri/Sat"

    // a weekend calendar is equal to a weekend calendar of the same identifier, so the other two
    // weekends are different values
    test shouldBe HolidayCalendar.FriSat
    test.hashCode shouldBe HolidayCalendar.FriSat.hashCode
    Hash[HolidayCalendar].eqv(test, HolidayCalendar.FriSat) shouldBe true
    test should not be HolidayCalendars.SAT_SUN
    test should not be HolidayCalendars.THU_FRI
    Hash[HolidayCalendar].eqv(test, HolidayCalendars.SAT_SUN) shouldBe false
    Hash[HolidayCalendar].eqv(test, HolidayCalendars.THU_FRI) shouldBe false

    // Sharing an identifier is not enough: equality also requires being a calendar of the same
    // kind, and `DEFAULTED_FRI_SAT` carries holiday data, so the two differ as values and in
    // what they answer while naming the same identifier.
    test.id shouldBe DEFAULTED_FRI_SAT.id
    test should not be DEFAULTED_FRI_SAT
    DEFAULTED_FRI_SAT should not be test
    test.isHoliday(SUN_2014_07_13) shouldBe false
    DEFAULTED_FRI_SAT.isHoliday(SUN_2014_07_13) shouldBe true

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
  test("coverage_weekend_family") {
    // The three weekend calendars are members of the sealed family in their own right, and these
    // two assertions say that much and no more: no type named `WeekendHolidayCalendar` is in
    // scope here, so no intermediate weekend type of that name sits between them and
    // `HolidayCalendar`, and `SatSun` is usable as a `HolidayCalendar`. Neither speaks to the
    // size of the family or to a subtype under another name - it currently has seven members
    // (`NoHolidays`, `SatSun`, `FriSat`, `ThuFri`, `ImmutableHolidayCalendar`, `Combined` and
    // `Linked`), and keeping it closed is the compiler's business and `ApiSurfaceSpec`'s.
    assertDoesNotCompile("val weekend: WeekendHolidayCalendar = HolidayCalendar.SatSun")
    assertCompiles("val calendar: HolidayCalendar = HolidayCalendar.SatSun")

    val weekendCalendars: List[HolidayCalendar] =
      List(HolidayCalendars.SAT_SUN, HolidayCalendars.FRI_SAT, HolidayCalendars.THU_FRI)

    // Equality and hashing are the identifier's, not the identity hash a Scala object would
    // otherwise use, which differs from run to run and would make the hash of a calendar - and so
    // the layout of any map keyed by one - unrepeatable.
    weekendCalendars.foreach { calendar =>
      withClue(s"$calendar: ") {
        calendar.hashCode shouldBe calendar.id.hashCode
        Hash[HolidayCalendar].hash(calendar) shouldBe calendar.id.hashCode
        calendar shouldBe calendar
        Hash[HolidayCalendar].eqv(calendar, calendar) shouldBe true
        // a calendar carrying data is unequal to a weekend calendar, asserted both ways
        val sameIdWithData = ImmutableHolidayCalendar.of(calendar.id, Nil, List(SATURDAY, SUNDAY))
        sameIdWithData.id shouldBe calendar.id
        calendar should not be sameIdWithData
        sameIdWithData should not be calendar
        calendar.equals(calendar.id.name) shouldBe false
      }
    }

    weekendCalendars.distinct should have size 3
    HolidayCalendar.SatSun.weekendDays shouldBe Set(SATURDAY, SUNDAY)
    HolidayCalendar.FriSat.weekendDays shouldBe Set(FRIDAY, SATURDAY)
    HolidayCalendar.ThuFri.weekendDays shouldBe Set(THURSDAY, FRIDAY)

    WEEK_2014_07.foreach { date =>
      withClue(s"$date: ") {
        HolidayCalendar.SatSun.isHoliday(date) shouldBe
          HolidayCalendar.SatSun.weekendDays.contains(date.getDayOfWeek)
        HolidayCalendar.FriSat.isHoliday(date) shouldBe
          HolidayCalendar.FriSat.weekendDays.contains(date.getDayOfWeek)
        HolidayCalendar.ThuFri.isHoliday(date) shouldBe
          HolidayCalendar.ThuFri.weekendDays.contains(date.getDayOfWeek)
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_everyPublishedCalendarHasABusinessDay") {
    // A calendar closed on every day of the week cannot answer the question that every date
    // adjustment, schedule and index observation asks of it - "the next business day from here"
    // - so the family refuses to build one and a search of one is refused rather than left to
    // spin. That refusal is worth stating only if the calendars this module actually publishes
    // satisfy it, which is what this test says of all of them at once: every built-in calendar,
    // every constant this holder publishes and the calendar the defaulting reference data
    // invents leaves some day of the week open, and the business day next to an arbitrary date
    // is found within a month of it - orders of magnitude inside the bound a search enforces,
    // so no published calendar is anywhere near it.
    StandardHolidayCalendars.all should have size 30

    val published: List[HolidayCalendar] =
      BUILT_IN ::: StandardHolidayCalendars.all.values.toList :::
        List(DEFAULTED_GBLO, DEFAULTED_FRI_SAT)

    // A whole week, which is the window that decides whether a calendar is closed on every day
    // of it: the weekend of a calendar is not part of the interface this family publishes - the
    // no-holidays and combined members have no weekend to report - so the property is stated as
    // behaviour, which is also the stronger statement, excluding a run of holidays covering a
    // week as well as a weekend that closes one.
    val wholeWeek: List[LocalDate] =
      (0 until 7).map(offset => THU_2014_07_10.plusDays(offset.toLong)).toList

    published.foreach { calendar =>
      withClue(s"${calendar.name}: ") {
        wholeWeek.count(day => calendar.isBusinessDay(day)) should be >= 1
        WEEK_2014_07.foreach { date =>
          val next: LocalDate = calendar.nextOrSame(date)
          calendar.isBusinessDay(next) shouldBe true
          next.toEpochDay - date.toEpochDay should be <= 31L
          val previous: LocalDate = calendar.previousOrSame(date)
          calendar.isBusinessDay(previous) shouldBe true
          date.toEpochDay - previous.toEpochDay should be <= 31L
        }
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_daysBetween_narrowing") {
    // The count of business days is narrowed to an `Int` with `Math.toIntExact`, so a range
    // holding more than an `Int` can express is reported rather than silently wrapped. Only the
    // calendar with no holidays reaches it, answering from the difference of the two dates.
    a[ArithmeticException] should be thrownBy
      HolidayCalendars.NO_HOLIDAYS.daysBetween(LocalDate.MIN, LocalDate.MAX)

    // The counting path every other calendar uses walks the range into a `Long`; a range wide
    // enough to overflow an `Int` cannot be walked here, so a century of days is counted twice.
    val start = LocalDate.of(2014, 1, 1)
    val end = LocalDate.of(2114, 1, 1)
    val expected = Iterator
      .iterate(start)(date => date.plusDays(1L))
      .takeWhile(date => date.isBefore(end))
      .count(date => HolidayCalendars.SAT_SUN.isBusinessDay(date))
    HolidayCalendars.SAT_SUN.daysBetween(start, end) shouldBe expected
    HolidayCalendars.SAT_SUN.daysBetween(start, start) shouldBe 0
    // 36,500 for the years plus the 24 leap days from 2016 to 2112, 2100 not being a leap year
    HolidayCalendars.NO_HOLIDAYS.daysBetween(start, end) shouldBe 36524

    an[IllegalArgumentException] should be thrownBy HolidayCalendars.SAT_SUN.daysBetween(end, start)
  }

  //-------------------------------------------------------------------------
  test("test_compositeIdentifierComposedOnce") {
    // A composite's identifier follows from its parts, fixed when it is built, so it is composed
    // at most once and held - the same object comes back from every read, and composing it parses
    // and normalises a joined name that `name`, `toString`, `Show` and the JSON form all read.
    val combined: HolidayCalendar = HolidayCalendars.FRI_SAT.combinedWith(HolidayCalendars.SAT_SUN)
    val linked: HolidayCalendar = HolidayCalendars.FRI_SAT.linkedWith(HolidayCalendars.SAT_SUN)
    val nested: HolidayCalendar = combined.combinedWith(HolidayCalendars.THU_FRI)

    combined.id should be theSameInstanceAs combined.id
    linked.id should be theSameInstanceAs linked.id
    nested.id should be theSameInstanceAs nested.id

    // what is held is the identifier the parts name, normalised as every composite name is
    combined.id shouldBe HolidayCalendarIds.FRI_SAT.combinedWith(HolidayCalendarIds.SAT_SUN)
    combined.id.name shouldBe "Fri/Sat+Sat/Sun"
    combined.name shouldBe "Fri/Sat+Sat/Sun"
    combined.toString shouldBe "HolidayCalendar[Fri/Sat+Sat/Sun]"
    linked.id shouldBe HolidayCalendarIds.FRI_SAT.linkedWith(HolidayCalendarIds.SAT_SUN)
    linked.id.name shouldBe "Fri/Sat~Sat/Sun"
    nested.id.name shouldBe "Fri/Sat+Sat/Sun+Thu/Fri"

    // an identifier is a value, so holding one cannot make two equal composites differ
    val again: HolidayCalendar = HolidayCalendars.FRI_SAT.combinedWith(HolidayCalendars.SAT_SUN)
    again shouldBe combined
    again.id shouldBe combined.id
    again.hashCode shouldBe combined.hashCode

    // the identifier is independent of the order the parts were given in
    HolidayCalendars.SAT_SUN.combinedWith(HolidayCalendars.FRI_SAT).id shouldBe combined.id
  }

  //-------------------------------------------------------------------------
  test("test_compositeResolution") {
    // Resolving a composite identifier reads its parts together on every call and combines them
    // in the order the normalised name gives, whichever way round the name was written.
    val gblo: ImmutableHolidayCalendar =
      ImmutableHolidayCalendar.of(HolidayCalendarIds.GBLO, List(MON_2014_07_14), List(SATURDAY, SUNDAY))
    val usny: ImmutableHolidayCalendar =
      ImmutableHolidayCalendar.of(HolidayCalendarIds.USNY, List(FRI_2014_07_11), List(SATURDAY, SUNDAY))
    val data: ReferenceData = store(
      ReferenceData.Entry(HolidayCalendarIds.GBLO, gblo),
      ReferenceData.Entry(HolidayCalendarIds.USNY, usny))

    val resolved = HolidayCalendarId.of("USNY+GBLO").resolve(data)
    resolved should haveValue(gblo.combinedWith(usny))
    resolved.map(calendar => calendar.name) should haveValue("GBLO+USNY")

    resolved.map(calendar => calendar.isHoliday(MON_2014_07_14)) should haveValue(true)
    resolved.map(calendar => calendar.isHoliday(FRI_2014_07_11)) should haveValue(true)
    resolved.map(calendar => calendar.isBusinessDay(THU_2014_07_10)) should haveValue(true)

    HolidayCalendarId.of("GBLO+USNY").resolve(data) shouldBe resolved
    resolved.map(calendar => calendar.id) should haveValue(HolidayCalendarId.of("GBLO+USNY"))

    // A part the data does not hold ends the resolution, and the failure names both that part
    // and the identifier being resolved; no calendar assembled from some of the parts is returned.
    val missing = HolidayCalendarId.of("GBLO+XXZZ+USNY").resolve(data)
    missing should beFailureWith(FailureReason.MISSING_DATA)
    missing.swap.map(failure => failure.attributes.get("id")) shouldBe Right(Some("XXZZ"))
    missing.swap.map(failure => failure.attributes.get("compositeId")) shouldBe Right(Some("GBLO+USNY+XXZZ"))

    // where the data holds the whole composite name, that calendar is used, parts unread
    val preCombined: ImmutableHolidayCalendar =
      ImmutableHolidayCalendar.of(HolidayCalendarId.of("GBLO+USNY"), List(THU_2014_07_10), List(SATURDAY, SUNDAY))
    val whole: ReferenceData = store(
      ReferenceData.Entry(HolidayCalendarIds.GBLO, gblo),
      ReferenceData.Entry(HolidayCalendarIds.USNY, usny),
      ReferenceData.Entry(HolidayCalendarId.of("GBLO+USNY"), preCombined))
    HolidayCalendarId.of("USNY+GBLO").resolve(whole) should haveValue(preCombined)
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // A calendar this library defines is written as its bare name, which locates the same
    // calendar again; only one an application built itself is written structurally. Each constant
    // is asserted to be a JSON string, which a round trip alone could not establish.
    HolidayCalendars.NO_HOLIDAYS.asJson shouldBe Json.fromString("NoHolidays")
    HolidayCalendars.SAT_SUN.asJson shouldBe Json.fromString("Sat/Sun")
    HolidayCalendars.FRI_SAT.asJson shouldBe Json.fromString("Fri/Sat")
    HolidayCalendars.THU_FRI.asJson shouldBe Json.fromString("Thu/Fri")
    BUILT_IN.foreach(calendar => withClue(s"$calendar: ")(calendar.asJson.isString shouldBe true))

    // these names are answered by the family itself, without any reference data
    BUILT_IN.foreach { calendar =>
      withClue(s"$calendar: ") {
        decode[HolidayCalendar](calendar.asJson.noSpaces) shouldBe Right(calendar)
      }
    }

    // the composite is an object naming its kind, so the order that is part of it survives
    val combined: HolidayCalendar = HolidayCalendars.FRI_SAT.combinedWith(HolidayCalendars.SAT_SUN)
    val expected: Json = parse("""{"Combined":{"a":"Fri/Sat","b":"Sat/Sun"}}""")
      .getOrElse(fail("the expected JSON of this test is not valid JSON"))
    combined.asJson shouldBe expected
    decode[HolidayCalendar](combined.asJson.noSpaces) shouldBe Right(combined)

    // equal values encode to identical bytes, so a stored document is comparable with a new one
    combined.asJson.noSpaces shouldBe
      HolidayCalendars.FRI_SAT.combinedWith(HolidayCalendars.SAT_SUN).asJson.noSpaces
    HolidayCalendars.SAT_SUN.combinedWith(HolidayCalendars.FRI_SAT).asJson.noSpaces should not be
      combined.asJson.noSpaces
  }

  //-------------------------------------------------------------------------
  test("test_deferredStore_producesOnlyTheIdentifierAskedFor") {
    // A store built from ways of obtaining its values runs the one a lookup names, and runs it
    // once. This is the mechanism `ReferenceData.standard` presents the thirty built-in calendars
    // through, stated over counting thunks so that what ran is observed rather than timed: the
    // count of a calendar the store was never asked for stays at zero however often the others
    // are read, and the count of one that was asked for stays at one however often it is read.
    val counted: CountedStore = countedStore()

    // filing the entries produced nothing
    counted.runs(FIRST_ID).get shouldBe 0
    counted.runs(SECOND_ID).get shouldBe 0
    counted.runs(THIRD_ID).get shouldBe 0

    // every way of asking about one identifier produces that one value, and only it
    counted.store.findValue(FIRST_ID).isDefined shouldBe true
    counted.runs(FIRST_ID).get shouldBe 1
    counted.runs(SECOND_ID).get shouldBe 0
    counted.runs(THIRD_ID).get shouldBe 0

    counted.store.containsValue(FIRST_ID) shouldBe true
    counted.store.getValue(FIRST_ID) should haveValue(calendarOf(FIRST_ID))
    counted.store.findValue(FIRST_ID) shouldBe Some(calendarOf(FIRST_ID))
    withClue("an entry is produced at most once, however often it is read: ")(
      counted.runs(FIRST_ID).get shouldBe 1)
    counted.runs(SECOND_ID).get shouldBe 0
    counted.runs(THIRD_ID).get shouldBe 0

    // and the instance is the one the first read produced, not an equal one built again -
    // `ImmutableHolidayCalendar` compares equal on its identifier alone, so this is the
    // assertion that says the value was memoised rather than the one above
    val first: HolidayCalendar =
      counted.store.findValue(FIRST_ID).getOrElse(fail("the deferred store lost its first value"))
    counted.store.findValue(FIRST_ID).foreach(again => again should be theSameInstanceAs first)

    // a second identifier costs the second value and nothing more
    counted.store.findValue(SECOND_ID).isDefined shouldBe true
    counted.runs(FIRST_ID).get shouldBe 1
    counted.runs(SECOND_ID).get shouldBe 1
    counted.runs(THIRD_ID).get shouldBe 0

    // an identifier the store does not hold produces nothing at all
    counted.store.findValue(HolidayCalendarId.of("XNONE")) shouldBe None
    counted.store.containsValue(HolidayCalendarId.of("XNONE")) shouldBe false
    counted.runs(THIRD_ID).get shouldBe 0

    // nor does merging: a merge moves the ways of obtaining the values, so laying a store over
    // this one leaves everything it had not produced unproduced
    val merged: ReferenceData =
      store(ReferenceData.Entry(HolidayCalendarIds.GBLO, HolidayCalendars.SAT_SUN))
        .combinedWith(counted.store)
    merged.findValue(HolidayCalendarIds.GBLO) shouldBe Some(HolidayCalendars.SAT_SUN)
    counted.runs(THIRD_ID).get shouldBe 0
    merged.findValue(THIRD_ID).isDefined shouldBe true
    counted.runs(THIRD_ID).get shouldBe 1
  }

  //-------------------------------------------------------------------------
  test("test_deferredStore_enumeratingAndComparingProduceEverything") {
    // The four members that cannot answer about a store without every entry of it produce every
    // entry of it, deliberately: an enumeration that omitted what had not been produced would
    // report a different store to each caller, and two equal stores could then compare unequal.
    // Each is asserted over its own store so that the count it leaves behind is its own.
    val enumerated: CountedStore = countedStore()
    enumerated.store.values should have size 3
    enumerated.store.values.keys should contain theSameElementsAs COUNTED_IDS
    COUNTED_IDS.foreach(id => enumerated.runs(id).get shouldBe 1)
    withClue("the entries are materialised once, so reading them again produces nothing: ") {
      enumerated.store.values should have size 3
      COUNTED_IDS.foreach(id => enumerated.runs(id).get shouldBe 1)
    }

    val compared: CountedStore = countedStore()
    compared.store shouldBe eagerStore()
    COUNTED_IDS.foreach(id => compared.runs(id).get shouldBe 1)

    val hashed: CountedStore = countedStore()
    hashed.store.hashCode shouldBe eagerStore().hashCode
    COUNTED_IDS.foreach(id => hashed.runs(id).get shouldBe 1)

    val rendered: CountedStore = countedStore()
    rendered.store.toString shouldBe eagerStore().toString
    rendered.store.toString should include(FIRST_ID.name)
    COUNTED_IDS.foreach(id => rendered.runs(id).get shouldBe 1)
  }

  //-------------------------------------------------------------------------
  test("test_standardReferenceData_observableBehaviourUnchanged") {
    // Presenting the built-in set one identifier at a time changes when a calendar is generated
    // and nothing else, which is what this states: the value, its identity, membership,
    // enumeration, equality with the same set filed eagerly, composite resolution, and the
    // layering `ReferenceData.of` performs all answer as they did.
    val standard: ImmutableReferenceData = ReferenceData.standard
    withClue("the store is computed once, so every read is the same store: ")(
      (ReferenceData.standard eq standard) shouldBe true)

    // the value a lookup answers with '''is''' the built-in calendar, not an equal one: an
    // `ImmutableHolidayCalendar` compares equal on its identifier alone, so identity is the only
    // assertion that can say the built-in instance was the one supplied
    standard.findValue(HolidayCalendarIds.GBLO).isDefined shouldBe true
    standard
      .findValue(HolidayCalendarIds.GBLO)
      .foreach(calendar => calendar should be theSameInstanceAs StandardHolidayCalendars.GBLO)
    standard
      .findValue(HolidayCalendarIds.USNY)
      .foreach(calendar => calendar should be theSameInstanceAs StandardHolidayCalendars.USNY)

    // membership and enumeration cover the whole built-in set
    StandardHolidayCalendars.all should have size 30
    StandardHolidayCalendars.all.keys.foreach { id =>
      withClue(s"$id: ") {
        standard.containsValue(id) shouldBe true
        standard.findValue(id) shouldBe Some(StandardHolidayCalendars.all(id))
      }
    }
    standard.values should have size 30
    standard.values.keys should contain theSameElementsAs StandardHolidayCalendars.all.keys

    // and the store is the store the same set filed eagerly would be
    val eager: ImmutableReferenceData = ImmutableReferenceData.ofMap(StandardHolidayCalendars.all)
    standard shouldBe eager
    standard.hashCode shouldBe eager.hashCode
    standard.toString shouldBe eager.toString
    standard.values shouldBe eager.values

    // a composite identifier resolves through the store part by part, as it always has
    val composite = HolidayCalendarId.of("GBLO+USNY").resolve(standard)
    composite should haveValue(
      StandardHolidayCalendars.GBLO.combinedWith(StandardHolidayCalendars.USNY))
    composite.map(calendar => calendar.name) should haveValue("GBLO+USNY")
    HolidayCalendarId.of("USNY+GBLO").resolve(standard) shouldBe composite
    HolidayCalendarId.of("XNONE").resolve(standard) should beFailureWith(FailureReason.MISSING_DATA)

    // and a caller's own entry is still laid over the minimal four, which the standard set's
    // presentation has nothing to do with
    val custom: HolidayCalendar = calendarOf(FIRST_ID)
    val layered: ReferenceData = ReferenceData.of(ReferenceData.Entry(FIRST_ID, custom)) match {
      case Right(data) => data
      case Left(failure) => fail(s"Fixture reference data could not be built: ${failure.message}")
    }
    layered.findValue(FIRST_ID) shouldBe Some(custom)
    List(
      HolidayCalendarIds.NO_HOLIDAYS,
      HolidayCalendarIds.SAT_SUN,
      HolidayCalendarIds.FRI_SAT,
      HolidayCalendarIds.THU_FRI).foreach { id =>
      withClue(s"$id: ")(layered.findValue(id) shouldBe Some(StandardHolidayCalendars.minimal(id)))
    }
    withClue("the minimal set is what is laid underneath, not the whole built-in set: ")(
      layered.findValue(HolidayCalendarIds.GBLO) shouldBe None)
  }

  //-------------------------------------------------------------------------
  /**
   * Builds a set of reference data holding exactly the entries given.
   *
   * `ImmutableReferenceData.of` is used rather than `ReferenceData.of` because the latter layers
   * the four built-in weekend calendars - `Fri/Sat` and `Thu/Fri` among them - underneath the
   * caller's entries, and every assertion here about a defaulted calendar depends on the store
   * holding nothing it was not given. The one failure that factory reports, a repeated
   * identifier, is turned into a failed test rather than forced.
   *
   * @param entries  the reference data entries, which must not repeat an identifier
   * @return the store holding exactly those entries
   */
  private def store(entries: ReferenceData.Entry[_]*): ImmutableReferenceData =
    ImmutableReferenceData.of(entries: _*) match {
      case Right(data) => data
      case Left(failure) => fail(s"Fixture reference data could not be built: ${failure.message}")
    }

  /**
   * A deferred store together with the number of times each of its values has been produced.
   *
   * The counts are what the deferred-store tests assert over: a store that produces only the
   * value a lookup names cannot be told from one that produces all of them by the values
   * themselves, which are equal either way, so the observation has to be of the work that ran.
   * An `AtomicInteger` is used rather than a counter of any other kind because a `lazy val` is
   * what defers the work and a `lazy val` may be reached from several threads.
   *
   * It is a plain final class rather than a case class because nothing matches on it and this
   * suite has no companion for a nested type to live in, where a case class declared inside the
   * suite would carry an outer reference its own type test cannot check.
   *
   * @param store  the store, whose values are produced by the counting ways of obtaining them
   * @param runs  the number of times each identifier's way of obtaining its value has run
   */
  private final class CountedStore(
      val store: ImmutableReferenceData,
      val runs: Map[HolidayCalendarId, AtomicInteger])

  /**
   * Builds a weekend-only calendar carrying the identifier given.
   *
   * The content is immaterial to every assertion that uses it - `ImmutableHolidayCalendar`
   * compares equal on its identifier alone - and a fresh instance is returned on each call, which
   * is what lets a test say the store answered with the instance it produced first rather than
   * with an equal one produced again.
   *
   * @param id  the identifier the calendar carries
   * @return a weekend-only calendar of that identifier
   */
  private def calendarOf(id: HolidayCalendarId): HolidayCalendar =
    ImmutableHolidayCalendar.of(id, Nil, List(SATURDAY, SUNDAY))

  /**
   * Builds a store over the three counted identifiers, none of its values produced.
   *
   * This is `ReferenceData.standard` in miniature: the same factory, the same shape of table -
   * an identifier mapped to a way of obtaining its value - and three values that record when they
   * are produced instead of thirty calendars that take a hundred and fifty years of rules each. A
   * fresh store, with fresh counts, is built per call so that a test reads only the work it
   * caused.
   *
   * @return the store and the counts of its three ways of obtaining a calendar
   */
  private def countedStore(): CountedStore = {
    val runs: Map[HolidayCalendarId, AtomicInteger] =
      COUNTED_IDS.map(id => id -> new AtomicInteger(0)).toMap
    val deferred: Map[HolidayCalendarId, () => HolidayCalendar] =
      COUNTED_IDS.map { id =>
        id -> (() => {
          val _ = runs(id).incrementAndGet()
          calendarOf(id)
        })
      }.toMap
    new CountedStore(ImmutableReferenceData.ofDeferredMap(deferred), runs)
  }

  /**
   * Builds the store the counted identifiers would make if their values were filed as values.
   *
   * The store a deferred one has to be indistinguishable from: equality, hashing and rendering
   * are properties of the entries a store holds and not of when it produced them, so each of the
   * three is asserted against this.
   *
   * @return the same three entries, filed eagerly
   */
  private def eagerStore(): ImmutableReferenceData =
    ImmutableReferenceData.ofMap(COUNTED_IDS.map(id => id -> calendarOf(id)).toMap)
}
