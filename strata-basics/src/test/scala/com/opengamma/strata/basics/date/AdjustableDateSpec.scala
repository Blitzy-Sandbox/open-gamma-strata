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
import org.scalatest.prop.TableFor2

import com.opengamma.strata.basics.RefDataReader
import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[AdjustableDate]].
 *
 * A date carrying [[BusinessDayAdjustment.NONE]] renders as its date alone; any other renders
 * its date, the words ` adjusted by ` and its adjustment. The short form follows the value of
 * the adjustment rather than the factory that built it. `adjusted(refData)` answers
 * `Either[Failure, LocalDate]` and `toReader` is the same adjustment as a reader whose
 * `run(refData)` answers the same, so a calendar the reference data cannot supply is a
 * `MISSING_DATA` failure rather than a thrown exception.
 */
class AdjustableDateSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  private val REF_DATA: ReferenceData = ReferenceData.standard

  private val BDA_NONE: BusinessDayAdjustment = BusinessDayAdjustment.NONE

  private val BDA_FOLLOW_SAT_SUN: BusinessDayAdjustment =
    BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.SAT_SUN)

  private val THU_2014_07_10: LocalDate = LocalDate.of(2014, 7, 10)
  private val FRI_2014_07_11: LocalDate = LocalDate.of(2014, 7, 11)
  private val SAT_2014_07_12: LocalDate = LocalDate.of(2014, 7, 12)
  private val SUN_2014_07_13: LocalDate = LocalDate.of(2014, 7, 13)
  private val MON_2014_07_14: LocalDate = LocalDate.of(2014, 7, 14)
  private val TUE_2014_07_15: LocalDate = LocalDate.of(2014, 7, 15)

  /** An identifier no reference data of this library holds, used to observe the failure path. */
  private val UNKNOWN_CALENDAR: HolidayCalendarId = HolidayCalendarId.of("XXXX")

  /** The Saturday and the Sunday both roll forward to the Monday, itself a row of the table. */
  private val dataAdjusted: TableFor2[LocalDate, LocalDate] = Table(
    ("date", "expected"),
    (THU_2014_07_10, THU_2014_07_10),
    (FRI_2014_07_11, FRI_2014_07_11),
    (SAT_2014_07_12, MON_2014_07_14),
    (SUN_2014_07_13, MON_2014_07_14),
    (MON_2014_07_14, MON_2014_07_14),
    (TUE_2014_07_15, TUE_2014_07_15))

  //-------------------------------------------------------------------------
  test("test_of_1arg") {
    val test: AdjustableDate = AdjustableDate.of(FRI_2014_07_11)

    test.unadjusted shouldBe FRI_2014_07_11
    test.adjustment shouldBe BDA_NONE

    test.toString shouldBe "2014-07-11"
    Show[AdjustableDate].show(test) shouldBe "2014-07-11"

    test shouldBe AdjustableDate(FRI_2014_07_11, BusinessDayAdjustment.NONE)
    test.adjusted(REF_DATA) should haveValue(FRI_2014_07_11)
    test.toReader.run(REF_DATA) should haveValue(FRI_2014_07_11)

    // `ReferenceData.minimal` carries the no-holidays calendar the constant names, so a date
    // needing no adjustment resolves against it without the full set.
    test.adjusted(ReferenceData.minimal) should haveValue(FRI_2014_07_11)

    AdjustableDate.of(SAT_2014_07_12).adjusted(REF_DATA) should haveValue(SAT_2014_07_12)
    AdjustableDate.of(SAT_2014_07_12).toString shouldBe "2014-07-12"
  }

  test("test_of_2args_withAdjustment") {
    val test: AdjustableDate = AdjustableDate.of(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN)

    test.unadjusted shouldBe FRI_2014_07_11
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN

    test.toString shouldBe "2014-07-11 adjusted by Following using calendar Sat/Sun"
    Show[AdjustableDate].show(test) shouldBe "2014-07-11 adjusted by Following using calendar Sat/Sun"

    test.adjusted(REF_DATA) should haveValue(FRI_2014_07_11)
    test.toReader.run(REF_DATA) should haveValue(FRI_2014_07_11)

    // The unadjusted date is kept as it was given even where it is a weekend.
    val weekend: AdjustableDate = AdjustableDate.of(SAT_2014_07_12, BDA_FOLLOW_SAT_SUN)
    weekend.unadjusted shouldBe SAT_2014_07_12
    weekend.toString shouldBe "2014-07-12 adjusted by Following using calendar Sat/Sun"
    weekend.adjusted(REF_DATA) should haveValue(MON_2014_07_14)

    test shouldBe AdjustableDate(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN)
  }

  test("test_of_2args_withNoAdjustment") {
    val test: AdjustableDate = AdjustableDate.of(FRI_2014_07_11, BDA_NONE)

    test.unadjusted shouldBe FRI_2014_07_11
    test.adjustment shouldBe BDA_NONE

    test.toString shouldBe "2014-07-11"
    test shouldBe AdjustableDate.of(FRI_2014_07_11)
    test.toString shouldBe AdjustableDate.of(FRI_2014_07_11).toString
    test.adjusted(REF_DATA) should haveValue(FRI_2014_07_11)

    // The short form follows the value of the adjustment, not the constant it was taken from:
    // an adjustment rebuilt from its two parts is equal to `NONE` and renders the same way.
    val rebuilt: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.NO_ADJUST, HolidayCalendarIds.NO_HOLIDAYS)
    rebuilt shouldBe BDA_NONE
    AdjustableDate.of(FRI_2014_07_11, rebuilt).toString shouldBe "2014-07-11"

    // An adjustment that adjusts nothing but names some other calendar is a different value,
    // deliberately not normalised to `NONE`, so it renders in the long form.
    val noAdjustSatSun: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.NO_ADJUST, HolidayCalendarIds.SAT_SUN)
    noAdjustSatSun should not be BDA_NONE
    AdjustableDate.of(FRI_2014_07_11, noAdjustSatSun).toString shouldBe
      "2014-07-11 adjusted by NoAdjust using calendar Sat/Sun"
    AdjustableDate.of(SAT_2014_07_12, noAdjustSatSun).adjusted(REF_DATA) should
      haveValue(SAT_2014_07_12)
  }

  test("test_of_null") {
    // Both factories are total and every argument is required, which is what these proofs state;
    // each is paired with the call that does compile, so none of them passes for another reason.
    assertDoesNotCompile("AdjustableDate.of()")
    assertDoesNotCompile("AdjustableDate.of(BDA_FOLLOW_SAT_SUN)")
    assertDoesNotCompile("AdjustableDate.of(FRI_2014_07_11, HolidayCalendarIds.SAT_SUN)")
    assertDoesNotCompile("AdjustableDate(FRI_2014_07_11)")
    assertDoesNotCompile("AdjustableDate()")

    assertCompiles("AdjustableDate.of(FRI_2014_07_11)")
    assertCompiles("AdjustableDate.of(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN)")
    assertCompiles("AdjustableDate(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN)")

    AdjustableDate.of(FRI_2014_07_11).unadjusted shouldBe FRI_2014_07_11
    AdjustableDate.of(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN).adjustment shouldBe BDA_FOLLOW_SAT_SUN
  }

  //-------------------------------------------------------------------------
  test("test_adjusted") {
    // The reader is the same adjustment awaiting its reference data, so both entry points agree.
    forAll(dataAdjusted) { (input: LocalDate, expected: LocalDate) =>
      val test: AdjustableDate = AdjustableDate.of(input, BDA_FOLLOW_SAT_SUN)

      withClue(s"$test: ") {
        val outcome: Either[Failure, LocalDate] = test.adjusted(REF_DATA)
        outcome should haveValue(expected)
        test.toReader.run(REF_DATA) should haveValue(expected)
        test.toReader.run(REF_DATA) shouldBe outcome

        // The unadjusted date is untouched by adjusting: the date as agreed and the date as it
        // will settle are both recoverable from one value.
        test.unadjusted shouldBe input
      }
    }

    // Two readers compose, which is why the reader form exists: several adjustments are assembled
    // before any reference data is available, and the data is supplied once, to the composition.
    val saturday: AdjustableDate = AdjustableDate.of(SAT_2014_07_12, BDA_FOLLOW_SAT_SUN)
    val sunday: AdjustableDate = AdjustableDate.of(SUN_2014_07_13, BDA_FOLLOW_SAT_SUN)
    val both: RefDataReader[(LocalDate, LocalDate)] = (saturday.toReader, sunday.toReader).tupled
    both.run(REF_DATA) should haveValue((MON_2014_07_14, MON_2014_07_14))

    val sameDay: RefDataReader[Boolean] =
      (saturday.toReader, sunday.toReader).mapN((first, second) => first == second)
    sameDay.run(REF_DATA) should haveValue(true)

    saturday.toReader.map(adjusted => adjusted.getDayOfWeek).run(REF_DATA) should
      haveValue(MON_2014_07_14.getDayOfWeek)

    // Adjusting against real holiday data: the London summer bank holiday of 2014 falls on
    // Monday the 25th, so a date rolled forward off the Saturday before it reaches the Tuesday.
    val london: AdjustableDate =
      AdjustableDate.of(
        LocalDate.of(2014, 8, 23),
        BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.GBLO))
    london.adjusted(REF_DATA) should haveValue(LocalDate.of(2014, 8, 26))
    london.toReader.run(REF_DATA) should haveValue(LocalDate.of(2014, 8, 26))

    // An unresolvable calendar is a missing-data failure through both entry points, not a throw.
    val unknown: AdjustableDate =
      AdjustableDate.of(
        FRI_2014_07_11,
        BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, UNKNOWN_CALENDAR))
    unknown.adjusted(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    unknown.toReader.run(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    noException should be thrownBy unknown.adjusted(REF_DATA)

    // A composition fails as a whole where any one of its parts cannot be resolved.
    (saturday.toReader, unknown.toReader).tupled.run(REF_DATA) should
      beFailureWith(FailureReason.MISSING_DATA)

    // `ReferenceData.empty` holds nothing at all - not even the no-holidays calendar - so even a
    // date needing no adjustment fails against it, and the failure is reported rather than thrown.
    saturday.adjusted(ReferenceData.empty) should beFailureWith(FailureReason.MISSING_DATA)
    AdjustableDate.of(FRI_2014_07_11).adjusted(ReferenceData.empty) should
      beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  test("equals") {
    val a1: AdjustableDate = AdjustableDate.of(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN)
    val a2: AdjustableDate = AdjustableDate.of(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN)
    val b: AdjustableDate = AdjustableDate.of(SAT_2014_07_12, BDA_FOLLOW_SAT_SUN)
    val c: AdjustableDate = AdjustableDate.of(FRI_2014_07_11, BDA_NONE)

    a1.equals(a2) shouldBe true
    a1.equals(b) shouldBe false
    a1.equals(c) shouldBe false

    // Equality is by field and equal values hash equally, so a value built twice is one value.
    a1 shouldBe a2
    a1.hashCode shouldBe a2.hashCode
    a1 should not be b
    a1 should not be c

    // `Eq` is obtained from the `Hash` instance by subtyping, so it decides what `equals` does.
    Hash[AdjustableDate].eqv(a1, a2) shouldBe true
    Hash[AdjustableDate].eqv(a1, b) shouldBe false
    Hash[AdjustableDate].eqv(a1, c) shouldBe false
    Hash[AdjustableDate].hash(a1) shouldBe Hash[AdjustableDate].hash(a2)

    // The comparison with a value of another type is written through `equals`, which takes
    // `Any`, because `==` between unrelated types is a compile error under this build's warnings.
    a1.equals("2014-07-11 adjusted by Following using calendar Sat/Sun") shouldBe false
    a1.equals(FRI_2014_07_11) shouldBe false
    a1.equals(BDA_FOLLOW_SAT_SUN) shouldBe false

    // What is held is the agreement, so two dates that adjust to the same day under different
    // adjustments are still two values.
    val modified: AdjustableDate =
      AdjustableDate.of(
        FRI_2014_07_11,
        BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.SAT_SUN))
    a1.adjusted(REF_DATA) shouldBe modified.adjusted(REF_DATA)
    c.adjusted(REF_DATA) shouldBe a1.adjusted(REF_DATA)
    a1 should not be modified
    Hash[AdjustableDate].eqv(a1, modified) shouldBe false
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    val test: AdjustableDate = AdjustableDate.of(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN)
    val same: AdjustableDate = AdjustableDate.of(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN)
    val otherDate: AdjustableDate = AdjustableDate.of(SAT_2014_07_12, BDA_FOLLOW_SAT_SUN)
    val otherAdjustment: AdjustableDate = AdjustableDate.of(FRI_2014_07_11, BDA_NONE)

    test.unadjusted shouldBe FRI_2014_07_11
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN

    test shouldBe same
    test.hashCode shouldBe same.hashCode
    Hash[AdjustableDate].eqv(test, same) shouldBe true
    Hash[AdjustableDate].hash(test) shouldBe Hash[AdjustableDate].hash(same)
    Hash[AdjustableDate].eqv(test, otherDate) shouldBe false
    Hash[AdjustableDate].eqv(test, otherAdjustment) shouldBe false
    test.equals(FRI_2014_07_11) shouldBe false

    // `copy` is public because both fields are required and no invariant needs protecting.
    test.copy(unadjusted = SAT_2014_07_12) shouldBe otherDate
    test.copy(adjustment = BDA_NONE) shouldBe otherAdjustment
    test.copy(unadjusted = SAT_2014_07_12).adjustment shouldBe BDA_FOLLOW_SAT_SUN
    test.copy(adjustment = BDA_NONE).unadjusted shouldBe FRI_2014_07_11

    AdjustableDate(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN) shouldBe test
    AdjustableDate.of(FRI_2014_07_11).copy(adjustment = BDA_FOLLOW_SAT_SUN) shouldBe test
    test.copy(adjustment = BDA_NONE).toString shouldBe "2014-07-11"

    test.toString shouldBe "2014-07-11 adjusted by Following using calendar Sat/Sun"
    Show[AdjustableDate].show(test) shouldBe test.toString
    Show[AdjustableDate].show(otherAdjustment) shouldBe otherAdjustment.toString
    Show[AdjustableDate].show(otherAdjustment) shouldBe "2014-07-11"

    test.adjusted(REF_DATA) should haveValue(FRI_2014_07_11)
    otherDate.adjusted(REF_DATA) should haveValue(MON_2014_07_14)
    otherDate.toReader.run(REF_DATA) should haveValue(MON_2014_07_14)
  }

  test("test_serialization") {
    // The document is pinned because a round trip on its own cannot state it: both fields in
    // declaration order, the date as an ISO-8601 string, the adjustment as its own codec writes it.
    val test: AdjustableDate = AdjustableDate.of(FRI_2014_07_11, BDA_FOLLOW_SAT_SUN)
    val encoded: Json = test.asJson

    encoded.asObject.map(obj => obj.keys.toList) shouldBe Some(List("unadjusted", "adjustment"))
    encoded.noSpaces shouldBe
      """{"unadjusted":"2014-07-11","adjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""

    // Neither field is optional and the encoder drops null values, so no document carries one.
    encoded.noSpaces should not include "null"
    AdjustableDate.of(FRI_2014_07_11).asJson.noSpaces should not include "null"

    // The `toString` abbreviation is not the document: a date carrying the no-adjustment
    // constant still serializes both of its fields.
    AdjustableDate.of(FRI_2014_07_11).asJson.noSpaces shouldBe
      """{"unadjusted":"2014-07-11","adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}"""

    decode[AdjustableDate](encoded.noSpaces) shouldBe Right(test)
    decode[AdjustableDate](
      """{"unadjusted":"2014-07-11","adjustment":{"convention":"Following","calendar":"Sat/Sun"}}""") shouldBe
      Right(test)
    decode[AdjustableDate](AdjustableDate.of(FRI_2014_07_11).asJson.noSpaces) shouldBe
      Right(AdjustableDate.of(FRI_2014_07_11))
    val weekend: AdjustableDate = AdjustableDate.of(SAT_2014_07_12, BDA_FOLLOW_SAT_SUN)
    decode[AdjustableDate](weekend.asJson.noSpaces) shouldBe Right(weekend)
    decode[AdjustableDate](weekend.asJson.noSpaces).map(value => value.adjusted(REF_DATA)) shouldBe
      Right(weekend.adjusted(REF_DATA))

    // Neither field's representation depends on how it was built, so equal values encode alike.
    AdjustableDate
      .of(
        FRI_2014_07_11,
        BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarId.of("Sat/Sun")))
      .asJson
      .noSpaces shouldBe encoded.noSpaces

    // Both fields are required, so a document missing either is rejected rather than defaulted,
    // and an adjustable date is an object rather than the string it renders as.
    decode[AdjustableDate]("""{"unadjusted":"2014-07-11"}""").isLeft shouldBe true
    decode[AdjustableDate](
      """{"adjustment":{"convention":"Following","calendar":"Sat/Sun"}}""").isLeft shouldBe true
    decode[AdjustableDate]("{}").isLeft shouldBe true
    Json.fromString("2014-07-11").as[AdjustableDate].isLeft shouldBe true

    // A malformed date and a convention outside the closed family are rejected by the decoder.
    val notADate: String =
      """{"unadjusted":"not-a-date","adjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""
    val notAConvention: String =
      """{"unadjusted":"2014-07-11","adjustment":{"convention":"Rubbish","calendar":"Sat/Sun"}}"""
    decode[AdjustableDate](notADate).isLeft shouldBe true
    decode[AdjustableDate](notAConvention).isLeft shouldBe true

    // A calendar the library does not know is a fact about the reference data rather than about
    // the document, so it decodes successfully and fails only when the date is adjusted.
    val unknown: Either[io.circe.Error, AdjustableDate] =
      decode[AdjustableDate](
        """{"unadjusted":"2014-07-11","adjustment":{"convention":"Following","calendar":"XXXX"}}""")
    unknown shouldBe Right(
      AdjustableDate.of(
        FRI_2014_07_11,
        BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, UNKNOWN_CALENDAR)))
    unknown.map(value => value.adjusted(REF_DATA).isLeft) shouldBe Right(true)
  }
}
