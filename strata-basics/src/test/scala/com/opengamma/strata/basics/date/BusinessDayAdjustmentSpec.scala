/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate

import cats.Hash
import cats.Show
import cats.syntax.apply._

import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor3

import com.opengamma.strata.basics.RefDataReader
import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[BusinessDayAdjustment]].
 *
 * `adjust` and `resolve` return `Either[Failure, _]` because an adjustment names a calendar the
 * reference data need not hold; `toReader` is the same resolution as a composable reader, whose
 * `run(refData)` answers that `Either`. Every row is driven through all three: reference data
 * supplied now, later, or in composition with another lookup computes the same date.
 */
class BusinessDayAdjustmentSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /** Reference data holding every built-in calendar. */
  private val REF_DATA: ReferenceData = ReferenceData.standard

  private val FRI_2014_07_11: LocalDate = LocalDate.of(2014, 7, 11)
  private val SAT_2014_07_12: LocalDate = LocalDate.of(2014, 7, 12)
  private val SUN_2014_07_13: LocalDate = LocalDate.of(2014, 7, 13)
  private val MON_2014_07_14: LocalDate = LocalDate.of(2014, 7, 14)

  private val FRI_2014_08_29: LocalDate = LocalDate.of(2014, 8, 29)
  private val SAT_2014_08_30: LocalDate = LocalDate.of(2014, 8, 30)
  private val SUN_2014_08_31: LocalDate = LocalDate.of(2014, 8, 31)
  private val MON_2014_09_01: LocalDate = LocalDate.of(2014, 9, 1)

  private val FRI_2014_10_31: LocalDate = LocalDate.of(2014, 10, 31)
  private val SAT_2014_11_01: LocalDate = LocalDate.of(2014, 11, 1)
  private val SUN_2014_11_02: LocalDate = LocalDate.of(2014, 11, 2)
  private val MON_2014_11_03: LocalDate = LocalDate.of(2014, 11, 3)

  private val FRI_2014_11_14: LocalDate = LocalDate.of(2014, 11, 14)
  private val SAT_2014_11_15: LocalDate = LocalDate.of(2014, 11, 15)
  private val SUN_2014_11_16: LocalDate = LocalDate.of(2014, 11, 16)
  private val MON_2014_11_17: LocalDate = LocalDate.of(2014, 11, 17)

  /** A Saturday whose following Monday, 25 August 2014, is the London summer bank holiday. */
  private val SAT_2014_08_23: LocalDate = LocalDate.of(2014, 8, 23)

  private val TUE_2014_08_26: LocalDate = LocalDate.of(2014, 8, 26)

  /** Independence Day 2019, a Thursday: a holiday in New York and a business day in London. */
  private val THU_2019_07_04: LocalDate = LocalDate.of(2019, 7, 4)

  private val FRI_2019_07_05: LocalDate = LocalDate.of(2019, 7, 5)

  /** An identifier no reference data holds, used to observe the failure path. */
  private val UNKNOWN_CALENDAR: HolidayCalendarId = HolidayCalendarId.of("XXXX")

  /**
   * Convention, date to adjust and expected date, against the `Sat/Sun` calendar.
   *
   * The four groups of dates reach every branch of every convention: a Friday and a Monday that
   * need no adjustment, a weekend inside a month, a weekend that straddles a month end - which
   * separates `ModifiedFollowing` from `Following` - and a weekend that straddles the middle of a
   * month, which separates `ModifiedFollowingBiMonthly` from `ModifiedFollowing`.
   */
  private val dataConvention: TableFor3[BusinessDayConvention, LocalDate, LocalDate] = Table(
    ("convention", "input", "expected"),
    (BusinessDayConventions.NO_ADJUST, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.NO_ADJUST, SAT_2014_07_12, SAT_2014_07_12),
    (BusinessDayConventions.NO_ADJUST, SUN_2014_07_13, SUN_2014_07_13),
    (BusinessDayConventions.NO_ADJUST, MON_2014_07_14, MON_2014_07_14),
    (BusinessDayConventions.FOLLOWING, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.FOLLOWING, SAT_2014_07_12, MON_2014_07_14),
    (BusinessDayConventions.FOLLOWING, SUN_2014_07_13, MON_2014_07_14),
    (BusinessDayConventions.FOLLOWING, MON_2014_07_14, MON_2014_07_14),
    (BusinessDayConventions.FOLLOWING, FRI_2014_08_29, FRI_2014_08_29),
    (BusinessDayConventions.FOLLOWING, SAT_2014_08_30, MON_2014_09_01),
    (BusinessDayConventions.FOLLOWING, SUN_2014_08_31, MON_2014_09_01),
    (BusinessDayConventions.FOLLOWING, MON_2014_09_01, MON_2014_09_01),
    (BusinessDayConventions.FOLLOWING, FRI_2014_10_31, FRI_2014_10_31),
    (BusinessDayConventions.FOLLOWING, SAT_2014_11_01, MON_2014_11_03),
    (BusinessDayConventions.FOLLOWING, SUN_2014_11_02, MON_2014_11_03),
    (BusinessDayConventions.FOLLOWING, MON_2014_11_03, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.MODIFIED_FOLLOWING, SAT_2014_07_12, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_FOLLOWING, SUN_2014_07_13, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_FOLLOWING, MON_2014_07_14, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_FOLLOWING, FRI_2014_08_29, FRI_2014_08_29),
    // modified: the following business day is in the next month, so the move is backwards
    (BusinessDayConventions.MODIFIED_FOLLOWING, SAT_2014_08_30, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_FOLLOWING, SUN_2014_08_31, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_FOLLOWING, MON_2014_09_01, MON_2014_09_01),
    (BusinessDayConventions.MODIFIED_FOLLOWING, FRI_2014_10_31, FRI_2014_10_31),
    (BusinessDayConventions.MODIFIED_FOLLOWING, SAT_2014_11_01, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING, SUN_2014_11_02, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING, MON_2014_11_03, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SAT_2014_07_12, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SUN_2014_07_13, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, MON_2014_07_14, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, FRI_2014_08_29, FRI_2014_08_29),
    // modified: a month end is also a half-month end
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SAT_2014_08_30, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SUN_2014_08_31, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, MON_2014_09_01, MON_2014_09_01),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, FRI_2014_10_31, FRI_2014_10_31),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SAT_2014_11_01, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SUN_2014_11_02, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, MON_2014_11_03, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, FRI_2014_11_14, FRI_2014_11_14),
    // modified: the 15th is a half-month boundary, so the move is backwards over it
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SAT_2014_11_15, FRI_2014_11_14),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SUN_2014_11_16, MON_2014_11_17),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, MON_2014_11_17, MON_2014_11_17),
    (BusinessDayConventions.PRECEDING, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.PRECEDING, SAT_2014_07_12, FRI_2014_07_11),
    (BusinessDayConventions.PRECEDING, SUN_2014_07_13, FRI_2014_07_11),
    (BusinessDayConventions.PRECEDING, MON_2014_07_14, MON_2014_07_14),
    (BusinessDayConventions.PRECEDING, FRI_2014_08_29, FRI_2014_08_29),
    (BusinessDayConventions.PRECEDING, SAT_2014_08_30, FRI_2014_08_29),
    (BusinessDayConventions.PRECEDING, SUN_2014_08_31, FRI_2014_08_29),
    (BusinessDayConventions.PRECEDING, MON_2014_09_01, MON_2014_09_01),
    (BusinessDayConventions.PRECEDING, FRI_2014_10_31, FRI_2014_10_31),
    (BusinessDayConventions.PRECEDING, SAT_2014_11_01, FRI_2014_10_31),
    (BusinessDayConventions.PRECEDING, SUN_2014_11_02, FRI_2014_10_31),
    (BusinessDayConventions.PRECEDING, MON_2014_11_03, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_PRECEDING, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.MODIFIED_PRECEDING, SAT_2014_07_12, FRI_2014_07_11),
    (BusinessDayConventions.MODIFIED_PRECEDING, SUN_2014_07_13, FRI_2014_07_11),
    (BusinessDayConventions.MODIFIED_PRECEDING, MON_2014_07_14, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_PRECEDING, FRI_2014_08_29, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_PRECEDING, SAT_2014_08_30, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_PRECEDING, SUN_2014_08_31, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_PRECEDING, MON_2014_09_01, MON_2014_09_01),
    (BusinessDayConventions.MODIFIED_PRECEDING, FRI_2014_10_31, FRI_2014_10_31),
    // modified: the preceding business day is in the previous month, so the move is forwards
    (BusinessDayConventions.MODIFIED_PRECEDING, SAT_2014_11_01, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_PRECEDING, SUN_2014_11_02, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_PRECEDING, MON_2014_11_03, MON_2014_11_03),
    (BusinessDayConventions.NEAREST, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.NEAREST, SAT_2014_07_12, FRI_2014_07_11),
    (BusinessDayConventions.NEAREST, SUN_2014_07_13, MON_2014_07_14),
    (BusinessDayConventions.NEAREST, MON_2014_07_14, MON_2014_07_14))

  //-------------------------------------------------------------------------
  test("test_basics") {
    val test: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.SAT_SUN)

    test.convention shouldBe BusinessDayConventions.MODIFIED_FOLLOWING
    test.calendar shouldBe HolidayCalendarIds.SAT_SUN
    test.toString shouldBe "ModifiedFollowing using calendar Sat/Sun"

    test shouldBe BusinessDayAdjustment(
      BusinessDayConventions.MODIFIED_FOLLOWING,
      HolidayCalendarIds.SAT_SUN)

    // A composite calendar is named in full by the rendering.
    BusinessDayAdjustment
      .of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarId.of("GBLO+USNY"))
      .toString shouldBe "ModifiedFollowing using calendar GBLO+USNY"
  }

  //-------------------------------------------------------------------------
  test("test_adjustDate") {
    forAll(dataConvention) { (convention: BusinessDayConvention, input: LocalDate, expected: LocalDate) =>
      val test: BusinessDayAdjustment = BusinessDayAdjustment.of(convention, HolidayCalendarIds.SAT_SUN)

      withClue(s"$test adjusting $input: ") {
        test.adjust(input, REF_DATA) should haveValue(expected)
        test.resolve(REF_DATA).map(adjuster => adjuster.adjust(input)) should haveValue(expected)
        test.toReader.run(REF_DATA).map(adjuster => adjuster.adjust(input)) should haveValue(expected)
      }
    }

    // Adjustment against holiday data resolved from reference data: the Saturday moves over the
    // Monday bank holiday to the Tuesday, and the month-end Saturday back to the Friday because
    // the following business day would leave August.
    val london: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.GBLO)
    london.adjust(SAT_2014_08_23, REF_DATA) should haveValue(TUE_2014_08_26)

    val londonModified: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.GBLO)
    londonModified.adjust(SAT_2014_08_30, REF_DATA) should haveValue(FRI_2014_08_29)

    // A composite calendar resolves component-wise and observes both sets of holidays, so the New
    // York holiday moves a date that the London-only adjustment leaves alone.
    val both: BusinessDayAdjustment =
      BusinessDayAdjustment.of(
        BusinessDayConventions.FOLLOWING,
        HolidayCalendarIds.GBLO.combinedWith(HolidayCalendarIds.USNY))
    both.adjust(THU_2019_07_04, REF_DATA) should haveValue(FRI_2019_07_05)
    london.adjust(THU_2019_07_04, REF_DATA) should haveValue(THU_2019_07_04)

    // `resolve`, and `toReader.run`, bind one calendar lookup into the adjuster they return, so a
    // run of dates costs one lookup rather than one per date. The answer must not change: one
    // resolved adjuster covers every date of 2015, for all seven conventions, against per-date
    // resolution.
    val dates: List[LocalDate] =
      Iterator
        .iterate(LocalDate.of(2015, 1, 1))(date => date.plusDays(1L))
        .takeWhile(date => date.getYear == 2015)
        .toList
    dates should have size 365

    BusinessDayConvention.values.toList.foreach { convention =>
      val test: BusinessDayAdjustment = BusinessDayAdjustment.of(convention, HolidayCalendarIds.GBLO)
      val resolvedOnce: Either[Failure, DateAdjuster] = test.resolve(REF_DATA)
      val readOnce: Either[Failure, DateAdjuster] = test.toReader.run(REF_DATA)
      resolvedOnce should beSuccess
      readOnce should beSuccess

      dates.foreach { date =>
        withClue(s"$test adjusting $date: ") {
          val perDate: Either[Failure, LocalDate] = test.adjust(date, REF_DATA)
          perDate should beSuccess
          resolvedOnce.map(adjuster => adjuster.adjust(date)) shouldBe perDate
          readOnce.map(adjuster => adjuster.adjust(date)) shouldBe perDate
        }
      }
    }

    // Readers compose, which is why the reader form exists: adjustments are assembled before any
    // reference data is available and the data is supplied once, to the composition.
    val londonAndNewYork: RefDataReader[(LocalDate, LocalDate)] =
      (
        london.toReader,
        BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.USNY).toReader)
        .mapN((londonAdjuster, newYorkAdjuster) =>
          (londonAdjuster.adjust(THU_2019_07_04), newYorkAdjuster.adjust(THU_2019_07_04)))
    londonAndNewYork.run(REF_DATA) should haveValue((THU_2019_07_04, FRI_2019_07_05))

    // A calendar the reference data cannot supply is reported as a `MISSING_DATA` failure through
    // all three entry points, for every convention, with nothing thrown.
    BusinessDayConvention.values.toList.foreach { convention =>
      val test: BusinessDayAdjustment = BusinessDayAdjustment.of(convention, UNKNOWN_CALENDAR)
      withClue(s"$test: ") {
        test.adjust(MON_2014_07_14, REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
        test.resolve(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
        test.toReader.run(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
        noException should be thrownBy test.adjust(MON_2014_07_14, REF_DATA)
      }
    }

    // The failure names the identifier and carries it as an `id` attribute for a caller to act on.
    val failed: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, UNKNOWN_CALENDAR)
    failed.adjust(MON_2014_07_14, REF_DATA) should
      haveFailureMessageMatching("Reference data not found for identifier 'XXXX'")
    failed.adjust(MON_2014_07_14, REF_DATA).swap.map(failure => failure.attributes.get("id")) shouldBe
      Right(Some("XXXX"))

    // A composition one part of which cannot resolve fails as a whole, with that part's failure.
    val partlyUnknown: RefDataReader[(LocalDate, LocalDate)] =
      (london.toReader, failed.toReader)
        .mapN((londonAdjuster, unknownAdjuster) =>
          (londonAdjuster.adjust(THU_2019_07_04), unknownAdjuster.adjust(THU_2019_07_04)))
    partlyUnknown.run(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)

    // `ReferenceData.empty` holds no calendar at all - not even the no-holidays calendar - so
    // every adjustment fails against it, `NONE` included; `ReferenceData.minimal` carries it.
    BusinessDayConvention.values.toList.foreach { convention =>
      val test: BusinessDayAdjustment =
        BusinessDayAdjustment.of(convention, HolidayCalendarIds.NO_HOLIDAYS)
      withClue(s"$test against empty reference data: ") {
        test.adjust(SAT_2014_07_12, ReferenceData.empty) should beFailureWith(FailureReason.MISSING_DATA)
      }
    }
    BusinessDayAdjustment.NONE.adjust(SAT_2014_07_12, ReferenceData.empty) should
      beFailureWith(FailureReason.MISSING_DATA)
    ReferenceData.empty.containsValue(HolidayCalendarIds.NO_HOLIDAYS) shouldBe false

    // A composite whose component is missing names both the component and the composite.
    val composite: BusinessDayAdjustment =
      BusinessDayAdjustment.of(
        BusinessDayConventions.FOLLOWING,
        HolidayCalendarIds.GBLO.combinedWith(UNKNOWN_CALENDAR))
    composite.adjust(MON_2014_07_14, REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    composite.adjust(MON_2014_07_14, REF_DATA) should
      haveFailureMessageMatching(
        "Reference data not found for 'XXXX' of type 'HolidayCalendarId' when finding 'GBLO\\+XXXX'")
  }

  //-------------------------------------------------------------------------
  test("test_noAdjust_constant") {
    val test: BusinessDayAdjustment = BusinessDayAdjustment.NONE

    test.convention shouldBe BusinessDayConventions.NO_ADJUST
    test.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    // `NONE` renders as its convention alone, because naming a calendar that is never consulted
    // would say something untrue about it.
    test.toString shouldBe "NoAdjust"

    test.adjust(SAT_2014_07_12, REF_DATA) should haveValue(SAT_2014_07_12)
    test.adjust(SAT_2014_07_12, ReferenceData.minimal) should haveValue(SAT_2014_07_12)
    test.resolve(REF_DATA).map(adjuster => adjuster.adjust(SAT_2014_07_12)) should
      haveValue(SAT_2014_07_12)
    test.toReader.run(REF_DATA).map(adjuster => adjuster.adjust(SAT_2014_07_12)) should
      haveValue(SAT_2014_07_12)
  }

  test("test_noAdjust_factory") {
    val test: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.NO_ADJUST, HolidayCalendarIds.NO_HOLIDAYS)

    test.convention shouldBe BusinessDayConventions.NO_ADJUST
    test.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    test.toString shouldBe "NoAdjust"

    // Equality is by field, so the value built by the factory is the constant and renders
    // identically: the rendering rule follows the value and not the constant.
    test shouldBe BusinessDayAdjustment.NONE
    Hash[BusinessDayAdjustment].eqv(test, BusinessDayAdjustment.NONE) shouldBe true
    test.toString shouldBe BusinessDayAdjustment.NONE.toString
  }

  test("test_noAdjust_normalized") {
    val test: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.NO_ADJUST, HolidayCalendarIds.SAT_SUN)

    test.convention shouldBe BusinessDayConventions.NO_ADJUST
    test.calendar shouldBe HolidayCalendarIds.SAT_SUN
    // `NO_ADJUST` with a real calendar is deliberately not normalised to `NONE`: the calendar is
    // kept and named so a caller may later replace the convention and still have it.
    test.toString shouldBe "NoAdjust using calendar Sat/Sun"
    test should not be BusinessDayAdjustment.NONE
    Hash[BusinessDayAdjustment].eqv(test, BusinessDayAdjustment.NONE) shouldBe false

    // It still makes no adjustment, because the convention decides that and not the calendar.
    test.adjust(SAT_2014_07_12, REF_DATA) should haveValue(SAT_2014_07_12)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    val test: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.SAT_SUN)
    val same: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.SAT_SUN)
    val otherConvention: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.SAT_SUN)
    val otherCalendar: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.GBLO)

    test.convention shouldBe BusinessDayConventions.MODIFIED_FOLLOWING
    test.calendar shouldBe HolidayCalendarIds.SAT_SUN

    // Equality and hashing are by field, so a value built twice is one value with one hash.
    test shouldBe same
    test.hashCode shouldBe same.hashCode
    Hash[BusinessDayAdjustment].eqv(test, same) shouldBe true
    Hash[BusinessDayAdjustment].hash(test) shouldBe Hash[BusinessDayAdjustment].hash(same)
    test should not be otherConvention
    test should not be otherCalendar
    Hash[BusinessDayAdjustment].eqv(test, otherConvention) shouldBe false
    Hash[BusinessDayAdjustment].eqv(test, otherCalendar) shouldBe false

    test.equals("ModifiedFollowing using calendar Sat/Sun") shouldBe false

    test.copy(calendar = HolidayCalendarIds.GBLO) shouldBe otherCalendar
    test.copy(convention = BusinessDayConventions.FOLLOWING) shouldBe otherConvention

    // `Show` and `toString` are the two ways of putting an adjustment into a message and agree.
    test.toString shouldBe "ModifiedFollowing using calendar Sat/Sun"
    Show[BusinessDayAdjustment].show(test) shouldBe test.toString
    Show[BusinessDayAdjustment].show(BusinessDayAdjustment.NONE) shouldBe "NoAdjust"
  }

  test("coverage_builder") {
    // The factory, the case-class constructor and `copy` are three ways of naming the same value,
    // and changing one field at a time is visible in the value and in its rendering.
    val fromFactory: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.SAT_SUN)
    val fromApply: BusinessDayAdjustment =
      BusinessDayAdjustment(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.SAT_SUN)
    val fromCopy: BusinessDayAdjustment =
      BusinessDayAdjustment.NONE.copy(
        convention = BusinessDayConventions.MODIFIED_FOLLOWING,
        calendar = HolidayCalendarIds.SAT_SUN)

    fromFactory.convention shouldBe BusinessDayConventions.MODIFIED_FOLLOWING
    fromFactory.calendar shouldBe HolidayCalendarIds.SAT_SUN
    fromApply.convention shouldBe fromFactory.convention
    fromApply.calendar shouldBe fromFactory.calendar
    fromCopy.convention shouldBe fromFactory.convention
    fromCopy.calendar shouldBe fromFactory.calendar

    fromApply shouldBe fromFactory
    fromCopy shouldBe fromFactory
    Hash[BusinessDayAdjustment].hash(fromApply) shouldBe Hash[BusinessDayAdjustment].hash(fromFactory)
    Hash[BusinessDayAdjustment].hash(fromCopy) shouldBe Hash[BusinessDayAdjustment].hash(fromFactory)
    Show[BusinessDayAdjustment].show(fromCopy) shouldBe Show[BusinessDayAdjustment].show(fromFactory)

    fromFactory.copy(convention = BusinessDayConventions.PRECEDING).toString shouldBe
      "Preceding using calendar Sat/Sun"
    fromFactory.copy(calendar = HolidayCalendarIds.GBLO).toString shouldBe
      "ModifiedFollowing using calendar GBLO"
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // Each field is written as the bare string its own type publishes. The document is pinned
    // here because a round trip alone cannot state it.
    val test: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.GBLO)
    val encoded: Json = test.asJson

    encoded.asObject.map(obj => obj.keys.toList) shouldBe Some(List("convention", "calendar"))
    encoded.noSpaces shouldBe """{"convention":"Following","calendar":"GBLO"}"""

    // Neither field is optional and the encoder drops absent values, so no null appears.
    encoded.noSpaces should not include "null"
    BusinessDayAdjustment.NONE.asJson.noSpaces should not include "null"
    BusinessDayAdjustment.NONE.asJson.noSpaces shouldBe
      """{"convention":"NoAdjust","calendar":"NoHolidays"}"""

    decode[BusinessDayAdjustment](encoded.noSpaces) shouldBe Right(test)
    decode[BusinessDayAdjustment]("""{"convention":"Following","calendar":"GBLO"}""") shouldBe Right(test)
    decode[BusinessDayAdjustment](BusinessDayAdjustment.NONE.asJson.noSpaces) shouldBe
      Right(BusinessDayAdjustment.NONE)
    val composite: BusinessDayAdjustment =
      BusinessDayAdjustment.of(
        BusinessDayConventions.MODIFIED_FOLLOWING,
        HolidayCalendarIds.GBLO.combinedWith(HolidayCalendarIds.USNY))
    decode[BusinessDayAdjustment](composite.asJson.noSpaces) shouldBe Right(composite)

    // A composite identifier normalises its name, so `USNY+GBLO` and `GBLO+USNY` encode alike.
    BusinessDayAdjustment
      .of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarId.of("USNY+GBLO"))
      .asJson
      .noSpaces shouldBe composite.asJson.noSpaces

    // Both fields are required, so a document missing either is rejected rather than defaulted,
    // and an adjustment is an object rather than a string.
    decode[BusinessDayAdjustment]("""{"convention":"Following"}""").isLeft shouldBe true
    decode[BusinessDayAdjustment]("""{"calendar":"GBLO"}""").isLeft shouldBe true
    decode[BusinessDayAdjustment]("{}").isLeft shouldBe true
    Json.fromString("Following").as[BusinessDayAdjustment].isLeft shouldBe true

    // A convention naming no member of its closed family is rejected by the decoder.
    decode[BusinessDayAdjustment]("""{"convention":"Rubbish","calendar":"GBLO"}""").isLeft shouldBe true

    // An unknown calendar, by contrast, decodes - it is a fact about the reference data rather
    // than about the document - and fails only at adjustment.
    val unknown: Either[io.circe.Error, BusinessDayAdjustment] =
      decode[BusinessDayAdjustment]("""{"convention":"Following","calendar":"XXXX"}""")
    unknown shouldBe Right(BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, UNKNOWN_CALENDAR))
    unknown.map(adjustment => adjustment.adjust(MON_2014_07_14, REF_DATA)).map(outcome => outcome.isLeft) shouldBe
      Right(true)
  }
}
