/*
 * Copyright (C) 2020 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate
import java.time.Period
import java.time.YearMonth

import cats.Hash
import cats.Show
import cats.data.EitherNec

import _root_.io.circe.Decoder
import _root_.io.circe.DecodingFailure
import _root_.io.circe.Json
import _root_.io.circe.syntax.EncoderOps

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[SequenceDate]].
 *
 * A [[SequenceDate]] is an instruction rather than a date: it names which date to take from a
 * [[DateSequence]], and a date exists only once the two are put together, which is why almost
 * every case here pairs an instruction with the date [[DateSequences.QUARTERLY_IMM_3_SERIAL]]
 * yields for it. That sequence makes the two families of factory observable, because its full
 * form interleaves the serial monthly IMM dates of the three nearest months with the quarterly
 * ones. Construction answers with `EitherNec[Failure, SequenceDate]`, so a rejection is a value,
 * asserted by reason ([[FailureReason.INVALID]]) rather than by message text.
 */
class SequenceDateSpec extends AnyFunSuite with Matchers with EitherValues {

  private val SEQUENCE: DateSequence = DateSequences.QUARTERLY_IMM_3_SERIAL

  /** February 2020, a month of no quarter, so the date it selects lies outside it. */
  private val YM_2020_02: YearMonth = YearMonth.of(2020, 2)

  /** March 2020, a quarterly month, whose own IMM date is the one selected. */
  private val YM_2020_03: YearMonth = YearMonth.of(2020, 3)

  private val PERIOD: Period = Period.ofMonths(2)

  /** The date every ladder counts from, chosen so that no sequence date has passed yet. */
  private val INPUT_2020_01_01: LocalDate = date(2020, 1, 1)

  /** A date that is itself a sequence date, which is what separates the two selection methods. */
  private val INPUT_2020_03_18: LocalDate = date(2020, 3, 18)

  //-------------------------------------------------------------------------
  private def instruction(result: EitherNec[Failure, SequenceDate]): SequenceDate = result.value

  private def selected(inputDate: LocalDate, result: EitherNec[Failure, SequenceDate]): LocalDate =
    SEQUENCE.selectDate(inputDate, instruction(result))

  private def selectedOrSame(
      inputDate: LocalDate,
      result: EitherNec[Failure, SequenceDate]): LocalDate =
    SEQUENCE.selectDateOrSame(inputDate, instruction(result))

  //-------------------------------------------------------------------------
  test("test_base_YearMonth") {
    val result = SequenceDate.base(YM_2020_02)
    result should beSuccess
    val test = instruction(result)
    test.yearMonth shouldBe Some(YM_2020_02)
    test.minimumPeriod shouldBe None
    test.sequenceNumber shouldBe 1
    test.fullSequence shouldBe false
  }

  test("test_base_YearMonth_int") {
    val test = instruction(SequenceDate.base(YM_2020_02, 2))
    test.yearMonth shouldBe Some(YM_2020_02)
    test.minimumPeriod shouldBe None
    test.sequenceNumber shouldBe 2
    test.fullSequence shouldBe false

    // A year-month replaces the input date: the count starts at the first day of the month named.
    selected(INPUT_2020_01_01, SequenceDate.base(YM_2020_02, 1)) shouldBe date(2020, 3, 18)
    selected(INPUT_2020_01_01, SequenceDate.base(YM_2020_02, 2)) shouldBe date(2020, 6, 17)
    selected(INPUT_2020_01_01, SequenceDate.base(YM_2020_02, 3)) shouldBe date(2020, 9, 16)

    selected(INPUT_2020_01_01, SequenceDate.base(YM_2020_03, 1)) shouldBe date(2020, 3, 18)
    selected(INPUT_2020_01_01, SequenceDate.base(YM_2020_03, 2)) shouldBe date(2020, 6, 17)
    selected(INPUT_2020_01_01, SequenceDate.base(YM_2020_03, 3)) shouldBe date(2020, 9, 16)
  }

  test("test_base_int") {
    val test = instruction(SequenceDate.base(2))
    test.yearMonth shouldBe None
    test.minimumPeriod shouldBe None
    test.sequenceNumber shouldBe 2
    test.fullSequence shouldBe false

    // The base ladder skips the serial monthly IMM dates that `test_full_int` returns.
    selected(INPUT_2020_01_01, SequenceDate.base(1)) shouldBe date(2020, 3, 18)
    selected(INPUT_2020_01_01, SequenceDate.base(2)) shouldBe date(2020, 6, 17)
    selected(INPUT_2020_01_01, SequenceDate.base(3)) shouldBe date(2020, 9, 16)

    // The input date is itself a base sequence date, so the two selection methods part company.
    selected(INPUT_2020_03_18, SequenceDate.base(1)) shouldBe date(2020, 6, 17)
    selectedOrSame(INPUT_2020_03_18, SequenceDate.base(1)) shouldBe date(2020, 3, 18)
  }

  test("test_base_Period_int") {
    val test = instruction(SequenceDate.base(PERIOD, 3))
    test.yearMonth shouldBe None
    test.minimumPeriod shouldBe Some(PERIOD)
    test.sequenceNumber shouldBe 3
    test.fullSequence shouldBe false

    // A minimum period that runs backwards describes no starting point, in either unit.
    SequenceDate.base(Period.ofMonths(-1), 3) should beFailureWith(FailureReason.INVALID)
    SequenceDate.base(Period.ofDays(-1), 3) should beFailureWith(FailureReason.INVALID)

    // The only coverage of a minimum period interacting with the sequence number. The period
    // moves the starting point forward and the number counts from there, so these seven periods
    // collapse onto the three dates the base sequence has ahead of 1 January 2020: two months
    // still leave the March date ahead, three months no longer do.
    selected(INPUT_2020_01_01, SequenceDate.base(Period.ofMonths(1), 1)) shouldBe date(2020, 3, 18)
    selected(INPUT_2020_01_01, SequenceDate.base(Period.ofMonths(2), 1)) shouldBe date(2020, 3, 18)
    selected(INPUT_2020_01_01, SequenceDate.base(Period.ofMonths(3), 1)) shouldBe date(2020, 6, 17)
    selected(INPUT_2020_01_01, SequenceDate.base(Period.ofMonths(4), 1)) shouldBe date(2020, 6, 17)
    selected(INPUT_2020_01_01, SequenceDate.base(Period.ofMonths(5), 1)) shouldBe date(2020, 6, 17)
    selected(INPUT_2020_01_01, SequenceDate.base(Period.ofMonths(6), 1)) shouldBe date(2020, 9, 16)
    selected(INPUT_2020_01_01, SequenceDate.base(Period.ofMonths(7), 1)) shouldBe date(2020, 9, 16)
  }

  //-------------------------------------------------------------------------
  test("test_full_YearMonth") {
    val result = SequenceDate.full(YM_2020_02)
    result should beSuccess
    val test = instruction(result)
    test.yearMonth shouldBe Some(YM_2020_02)
    test.minimumPeriod shouldBe None
    test.sequenceNumber shouldBe 1
    test.fullSequence shouldBe true
  }

  test("test_full_YearMonth_int") {
    val test = instruction(SequenceDate.full(YM_2020_02, 2))
    test.yearMonth shouldBe Some(YM_2020_02)
    test.minimumPeriod shouldBe None
    test.sequenceNumber shouldBe 2
    test.fullSequence shouldBe true

    // The only coverage of a year-month over the full sequence: February's own serial IMM date,
    // then the serial dates of the two months after it.
    selected(INPUT_2020_01_01, SequenceDate.full(YM_2020_02, 1)) shouldBe date(2020, 2, 19)
    selected(INPUT_2020_01_01, SequenceDate.full(YM_2020_02, 2)) shouldBe date(2020, 3, 18)
    selected(INPUT_2020_01_01, SequenceDate.full(YM_2020_02, 3)) shouldBe date(2020, 4, 15)

    // The same sequence numbers over the base sequence, where February's date becomes March's.
    selected(INPUT_2020_01_01, SequenceDate.base(YM_2020_02, 1)) shouldBe date(2020, 3, 18)
    selected(INPUT_2020_01_01, SequenceDate.base(YM_2020_02, 2)) shouldBe date(2020, 6, 17)
    selected(INPUT_2020_01_01, SequenceDate.base(YM_2020_02, 3)) shouldBe date(2020, 9, 16)

    // A year-month replaces the input date rather than moving it, so the input date is not
    // consulted: these are selected from dates two decades apart and from a sequence date.
    selected(date(1999, 7, 9), SequenceDate.full(YM_2020_02, 1)) shouldBe date(2020, 2, 19)
    selected(date(2035, 12, 31), SequenceDate.full(YM_2020_02, 1)) shouldBe date(2020, 2, 19)
    selected(INPUT_2020_03_18, SequenceDate.full(YM_2020_02, 2)) shouldBe date(2020, 3, 18)
    selected(date(2035, 12, 31), SequenceDate.full(YM_2020_02, 3)) shouldBe date(2020, 4, 15)

    // The count admits the first day of the month named, so the two selection methods agree.
    selectedOrSame(INPUT_2020_01_01, SequenceDate.full(YM_2020_02, 1)) shouldBe date(2020, 2, 19)
    selectedOrSame(INPUT_2020_03_18, SequenceDate.full(YM_2020_02, 2)) shouldBe date(2020, 3, 18)
    selectedOrSame(INPUT_2020_01_01, SequenceDate.full(YM_2020_02, 3)) shouldBe date(2020, 4, 15)
    selectedOrSame(INPUT_2020_01_01, SequenceDate.base(YM_2020_02, 1)) shouldBe date(2020, 3, 18)
  }

  test("test_full_int") {
    val test = instruction(SequenceDate.full(2))
    test.yearMonth shouldBe None
    test.minimumPeriod shouldBe None
    test.sequenceNumber shouldBe 2
    test.fullSequence shouldBe true

    // The full ladder: the IMM dates of January, February and March, then the quarterly ones.
    selected(INPUT_2020_01_01, SequenceDate.full(1)) shouldBe date(2020, 1, 15)
    selected(INPUT_2020_01_01, SequenceDate.full(2)) shouldBe date(2020, 2, 19)
    selected(INPUT_2020_01_01, SequenceDate.full(3)) shouldBe date(2020, 3, 18)
    selected(INPUT_2020_01_01, SequenceDate.full(4)) shouldBe date(2020, 6, 17)
    selected(INPUT_2020_01_01, SequenceDate.full(5)) shouldBe date(2020, 9, 16)

    selected(INPUT_2020_03_18, SequenceDate.full(1)) shouldBe date(2020, 4, 15)
    selectedOrSame(INPUT_2020_03_18, SequenceDate.full(1)) shouldBe date(2020, 3, 18)
  }

  test("test_full_Period_int") {
    val test = instruction(SequenceDate.full(PERIOD, 3))
    test.yearMonth shouldBe None
    test.minimumPeriod shouldBe Some(PERIOD)
    test.sequenceNumber shouldBe 3
    test.fullSequence shouldBe true
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    val test = instruction(SequenceDate.full(PERIOD, 3))
    val same = instruction(SequenceDate.full(PERIOD, 3))
    val test2 = instruction(SequenceDate.base(YM_2020_02, 2))

    test shouldBe same
    test.hashCode shouldBe same.hashCode
    test should not be test2
    // compared in method form, because `==` between unrelated types is an error in this build
    test.equals(test.toString) shouldBe false

    Hash[SequenceDate].eqv(test, same) shouldBe true
    Hash[SequenceDate].eqv(test, test2) shouldBe false
    Hash[SequenceDate].hash(test) shouldBe test.hashCode

    // The rendering names all four fields in declaration order, an absent optional one as `null`.
    test.toString shouldBe
      "SequenceDate{yearMonth=null, minimumPeriod=P2M, sequenceNumber=3, fullSequence=true}"
    test2.toString shouldBe
      "SequenceDate{yearMonth=2020-02, minimumPeriod=null, sequenceNumber=2, fullSequence=false}"

    Show[SequenceDate].show(test) shouldBe test.toString
    Show[SequenceDate].show(test2) shouldBe test2.toString

    // Two starting points at once, or a sequence number below one, describe no date.
    val bothStartingPoints =
      SequenceDate.of(Some(YM_2020_02), Some(PERIOD), 1, fullSequence = false)
    bothStartingPoints should beFailureWith(FailureReason.INVALID)
    SequenceDate.base(0) should beFailureWith(FailureReason.INVALID)
    SequenceDate.base(-1) should beFailureWith(FailureReason.INVALID)
    SequenceDate.full(0) should beFailureWith(FailureReason.INVALID)

    // A minimum period of zero is normalised to none, which makes these instructions equal.
    val fromZeroPeriod = instruction(SequenceDate.base(Period.ZERO, 1))
    fromZeroPeriod.minimumPeriod shouldBe None
    fromZeroPeriod shouldBe instruction(SequenceDate.base(1))
    val fullFromZeroPeriod = instruction(SequenceDate.full(Period.ZERO, 1))
    fullFromZeroPeriod.minimumPeriod shouldBe None
    fullFromZeroPeriod shouldBe instruction(SequenceDate.full(1))
  }

  test("test_serialization") {
    val test = instruction(SequenceDate.full(PERIOD, 3))
    val json = test.asJson
    Decoder[SequenceDate].decodeJson(json) shouldBe Right(test)

    // The encoder drops a field holding nothing, so the absent year-month is not in the output.
    val fields = json.asObject.map(_.keys.toList).getOrElse(Nil)
    fields should not contain "yearMonth"
    fields should contain theSameElementsAs List("minimumPeriod", "sequenceNumber", "fullSequence")
    json.noSpaces should not include "null"

    json.noSpaces shouldBe """{"minimumPeriod":"P2M","sequenceNumber":3,"fullSequence":true}"""

    val withoutYearMonth = Json.obj(
      "minimumPeriod" -> Json.fromString("P2M"),
      "sequenceNumber" -> Json.fromInt(3),
      "fullSequence" -> Json.True)
    Decoder[SequenceDate].decodeJson(withoutYearMonth) shouldBe Right(test)
    Decoder[SequenceDate].decodeJson(withoutYearMonth).map(_.yearMonth) shouldBe Right(None)

    val withYearMonth = instruction(SequenceDate.base(YM_2020_02, 2))
    Decoder[SequenceDate].decodeJson(withYearMonth.asJson) shouldBe Right(withYearMonth)
    withYearMonth.asJson.asObject.map(_.keys.toList).getOrElse(Nil) should contain("yearMonth")
    withYearMonth.asJson.noSpaces shouldBe
      """{"yearMonth":"2020-02","sequenceNumber":2,"fullSequence":false}"""

    // The decoder validates through the factory, so a document describing no instruction fails.
    val invalidPayload = Json.obj(
      "sequenceNumber" -> Json.fromInt(0),
      "fullSequence" -> Json.False)
    val decoded: Either[DecodingFailure, SequenceDate] =
      Decoder[SequenceDate].decodeJson(invalidPayload)
    decoded.toOption shouldBe None
    decoded.swap.map(_.message).getOrElse("") should include("sequenceNumber")
  }
}
