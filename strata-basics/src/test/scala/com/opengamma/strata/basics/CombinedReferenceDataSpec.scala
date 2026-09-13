/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import java.time.DayOfWeek.SATURDAY
import java.time.DayOfWeek.SUNDAY
import java.time.LocalDate

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.date.HolidayCalendarIds
import com.opengamma.strata.basics.date.HolidayCalendars
import com.opengamma.strata.basics.date.ImmutableHolidayCalendar
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/** Test [[CombinedReferenceData]]. */
class CombinedReferenceDataSpec extends AnyFunSuite with Matchers {

  import GenericTestingReferenceDataId.count
  import GenericTestingReferenceDataId.text

  /**
   * The identifier both stores hold, under a different value in each.
   *
   * The clash is deliberate: precedence is unobservable where the two stores agree. `ID2` is
   * held by the first store alone, `ID3` by the second alone, and `ID4` by neither.
   */
  private val ID1: TestingReferenceDataId = TestingReferenceDataId("1")

  private val ID2: TestingReferenceDataId = TestingReferenceDataId("2")

  private val ID3: TestingReferenceDataId = TestingReferenceDataId("3")

  private val ID4: TestingReferenceDataId = TestingReferenceDataId("4")

  private val VAL1: java.lang.Number = Double.box(123d)

  private val VAL2: java.lang.Number = Double.box(234d)

  private val VAL3: java.lang.Number = Double.box(999d)

  private val BASE_DATA1: ImmutableReferenceData = baseData1()

  private val BASE_DATA2: ImmutableReferenceData = baseData2()

  private val ALL_IDS: List[TestingReferenceDataId] = List(ID1, ID2, ID3, ID4)

  /**
   * The date that tells the caller's `Sat/Sun` calendar apart from the built-in one.
   *
   * A Tuesday, so it falls in neither calendar's weekend: the built-in `Sat/Sun` calendar
   * treats it as a business day and [[OVERRIDING_SAT_SUN]] names it a holiday.
   */
  private val OVERRIDDEN_DATE: LocalDate = LocalDate.of(2015, 6, 30)

  /**
   * A caller's own `Sat/Sun` calendar: the built-in identifier with [[OVERRIDDEN_DATE]] added.
   *
   * A holiday calendar is equal to any other calendar of the same identifier whatever
   * holidays it holds, so only its answer about that date says which of the two answered a
   * lookup.
   */
  private val OVERRIDING_SAT_SUN: ImmutableHolidayCalendar =
    ImmutableHolidayCalendar.of(HolidayCalendarIds.SAT_SUN, List(OVERRIDDEN_DATE), Set(SATURDAY, SUNDAY))

  //-------------------------------------------------------------------------
  test("test_combination") {
    val test = CombinedReferenceData(BASE_DATA1, BASE_DATA2)

    test.containsValue(ID1) shouldBe true
    test.containsValue(ID2) shouldBe true
    test.containsValue(ID3) shouldBe true
    test.containsValue(ID4) shouldBe false

    // `ID1` is held by both stores and answers with the first store's value
    test.getValue(ID1) should haveValue(VAL1)
    test.getValue(ID2) should haveValue(VAL2)
    test.getValue(ID3) should haveValue(VAL3)

    test.getValue(ID4) should beFailureWith(FailureReason.MISSING_DATA)

    test.findValue(ID1) shouldBe Some(VAL1)
    test.findValue(ID2) shouldBe Some(VAL2)
    test.findValue(ID3) shouldBe Some(VAL3)
    test.findValue(ID4) shouldBe None

    //-----------------------------------------------------------------------
    // `ReferenceData.of` lays the caller's entries over the four built-in weekend and
    // no-holiday calendars and prefers the caller's, so the substituted `Sat/Sun` calendar
    // answers for that identifier while the other three remain the built-in ones
    val layered = layeredOverMinimal(
      ReferenceData.Entry(HolidayCalendarIds.SAT_SUN, OVERRIDING_SAT_SUN),
      ReferenceData.Entry(ID1, VAL1))

    layered.getValue(HolidayCalendarIds.SAT_SUN).map(cal => cal.isHoliday(OVERRIDDEN_DATE)) should
      haveValue(true)
    HolidayCalendars.SAT_SUN.isHoliday(OVERRIDDEN_DATE) shouldBe false

    layered.getValue(HolidayCalendarIds.NO_HOLIDAYS) should haveValue(HolidayCalendars.NO_HOLIDAYS)
    layered.getValue(HolidayCalendarIds.FRI_SAT) should haveValue(HolidayCalendars.FRI_SAT)
    layered.getValue(HolidayCalendarIds.THU_FRI) should haveValue(HolidayCalendars.THU_FRI)
    layered.getValue(HolidayCalendarIds.NO_HOLIDAYS).map(cal => cal.isHoliday(OVERRIDDEN_DATE)) should
      haveValue(false)
    layered.getValue(HolidayCalendarIds.FRI_SAT).map(cal => cal.isHoliday(OVERRIDDEN_DATE)) should
      haveValue(false)
    layered.getValue(HolidayCalendarIds.THU_FRI).map(cal => cal.isHoliday(OVERRIDDEN_DATE)) should
      haveValue(false)

    layered.getValue(ID1) should haveValue(VAL1)
    layered.findValue(ID4) shouldBe None

    // the set laid underneath is `minimal`, so `GBLO` - a calendar of `standard` - is absent
    layered.containsValue(HolidayCalendarIds.GBLO) shouldBe false
    layered.getValue(HolidayCalendarIds.GBLO) should beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  // Two instantiations of `GenericTestingReferenceDataId` at different value types are equal
  // after erasure, so a store keys both to one entry; the witness each carries is what
  // retrieval tells them apart by. Each identifier is therefore answered with the value of
  // its own type, from whichever store holds one, in either order: preferring a store that
  // holds data of the other type shadows nothing.
  test("test_combination_wrongValueType") {
    val textId: GenericTestingReferenceDataId[String] =
      GenericTestingReferenceDataId[String]("shared")
    val countId: GenericTestingReferenceDataId[Int] =
      GenericTestingReferenceDataId[Int]("shared")

    (textId: ReferenceDataId[_]) shouldBe (countId: ReferenceDataId[_])

    val textData: ImmutableReferenceData = store(ReferenceData.Entry(textId, "a value"))
    val countData: ImmutableReferenceData = store(ReferenceData.Entry(countId, 42))

    val textFirst = CombinedReferenceData(textData, countData)
    textFirst.findValue(textId) shouldBe Some("a value")
    textFirst.findValue(countId) shouldBe Some(42)
    textFirst.getValue(textId) should haveValue("a value")
    textFirst.getValue(countId) should haveValue(42)

    val countFirst = CombinedReferenceData(countData, textData)
    countFirst.findValue(textId) shouldBe Some("a value")
    countFirst.findValue(countId) shouldBe Some(42)

    // a third instantiation, held by neither store, is absent rather than answered with
    // either value
    val flagId: GenericTestingReferenceDataId[Boolean] =
      GenericTestingReferenceDataId[Boolean]("shared")(
        ReferenceDataType.of("Boolean") { case flag: Boolean => flag })
    textFirst.containsValue(flagId) shouldBe false
    textFirst.findValue(flagId) shouldBe None
    textFirst.getValue(flagId) should beFailureWith(FailureReason.MISSING_DATA)
  }

  // `ImmutableReferenceData.combinedWith` merges two stores into one so that a lookup stays a
  // single map lookup, and a merge is keyed by the erased identifier: merging these two would
  // keep one value and drop the other, so it declines and chains them instead. Two stores
  // whose shared key refers to data of the same type still merge, which is the last group.
  test("test_combinedWith_wrongValueType") {
    val textId: GenericTestingReferenceDataId[String] =
      GenericTestingReferenceDataId[String]("shared")
    val countId: GenericTestingReferenceDataId[Int] =
      GenericTestingReferenceDataId[Int]("shared")
    (textId: ReferenceDataId[_]) shouldBe (countId: ReferenceDataId[_])

    val textData: ImmutableReferenceData = store(ReferenceData.Entry(textId, "a value"))
    val countData: ImmutableReferenceData = store(ReferenceData.Entry(countId, 42))

    val textFirst: ReferenceData = textData.combinedWith(countData)
    textFirst should not be an[ImmutableReferenceData]
    textFirst.findValue(textId) shouldBe Some("a value")
    textFirst.findValue(countId) shouldBe Some(42)
    textFirst.containsValue(textId) shouldBe true
    textFirst.containsValue(countId) shouldBe true
    textFirst.getValue(textId) should haveValue("a value")
    textFirst.getValue(countId) should haveValue(42)
    textId.resolve(textFirst) should haveValue("a value")
    countId.resolve(textFirst) should haveValue(42)
    textId.toReader.run(textFirst) should haveValue("a value")
    countId.toReader.run(textFirst) should haveValue(42)

    val countFirst: ReferenceData = countData.combinedWith(textData)
    countFirst should not be an[ImmutableReferenceData]
    countFirst.findValue(textId) shouldBe Some("a value")
    countFirst.findValue(countId) shouldBe Some(42)
    countFirst.containsValue(textId) shouldBe true
    countFirst.containsValue(countId) shouldBe true
    countFirst.getValue(textId) should haveValue("a value")
    countFirst.getValue(countId) should haveValue(42)
    textId.resolve(countFirst) should haveValue("a value")
    countId.resolve(countFirst) should haveValue(42)
    textId.toReader.run(countFirst) should haveValue("a value")
    countId.toReader.run(countFirst) should haveValue(42)

    val otherTextId: GenericTestingReferenceDataId[String] =
      GenericTestingReferenceDataId[String]("shared")
    val otherTextData: ImmutableReferenceData =
      store(ReferenceData.Entry(otherTextId, "another value"))
    val merged: ReferenceData = textData.combinedWith(otherTextData)
    merged shouldBe an[ImmutableReferenceData]
    merged.findValue(textId) shouldBe Some("a value")
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    val test = CombinedReferenceData(BASE_DATA1, BASE_DATA2)
    val swapped = CombinedReferenceData(BASE_DATA2, BASE_DATA1)

    test.refData1 shouldBe BASE_DATA1
    test.refData2 shouldBe BASE_DATA2

    val same = CombinedReferenceData(baseData1(), baseData2())
    test shouldBe same
    test.hashCode shouldBe same.hashCode

    test should not be swapped
    test.toString should not be swapped.toString

    test.findValue(ID1) shouldBe Some(VAL1)
    swapped.findValue(ID1) shouldBe Some(VAL3)
    test.getValue(ID1) should haveValue(VAL1)
    swapped.getValue(ID1) should haveValue(VAL3)
    ALL_IDS.filterNot(id => id == ID1).foreach(id => swapped.findValue(id) shouldBe test.findValue(id))
  }

  //-------------------------------------------------------------------------
  test("serialization") {
    val test = CombinedReferenceData(BASE_DATA1, BASE_DATA2)

    val readBack: Map[TestingReferenceDataId, java.lang.Number] =
      ALL_IDS.flatMap(id => test.findValue(id).map(value => id -> value)).toMap
    readBack shouldBe Map(ID1 -> VAL1, ID2 -> VAL2, ID3 -> VAL3)

    val rebuilt = store(readBack.toSeq.map { case (id, value) => ReferenceData.Entry(id, value) }: _*)
    ALL_IDS.foreach(id => rebuilt.findValue(id) shouldBe test.findValue(id))

    rebuilt.getValue(ID1) should haveValue(VAL1)
    rebuilt.getValue(ID4) should beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  /**
   * Builds the first store, afresh on each call.
   *
   * `coverage` needs a second, independently constructed store holding the same entries, so
   * that the equality it asserts is equality of two values rather than of two names for one.
   */
  private def baseData1(): ImmutableReferenceData =
    store(ReferenceData.Entry(ID1, VAL1), ReferenceData.Entry(ID2, VAL2))

  private def baseData2(): ImmutableReferenceData =
    store(ReferenceData.Entry(ID1, VAL3), ReferenceData.Entry(ID3, VAL3))

  /**
   * Builds a store holding exactly the entries given.
   *
   * `ImmutableReferenceData.of` rather than `ReferenceData.of`: the latter lays the four
   * built-in weekend calendars underneath, and the assertions that `ID4` is absent need a
   * store holding nothing it was not given. The one failure that factory reports - two
   * entries under one identifier - is a defect in this fixture, so it fails the test.
   *
   * @param entries  the reference data entries, which must not repeat an identifier
   */
  private def store(entries: ReferenceData.Entry[_]*): ImmutableReferenceData =
    ImmutableReferenceData.of(entries: _*) match {
      case Right(data) => data
      case Left(failure) => fail(s"Fixture reference data could not be built: ${failure.message}")
    }

  private def layeredOverMinimal(entries: ReferenceData.Entry[_]*): ReferenceData =
    ReferenceData.of(entries: _*) match {
      case Right(data) => data
      case Left(failure) => fail(s"Fixture reference data could not be built: ${failure.message}")
    }
}
