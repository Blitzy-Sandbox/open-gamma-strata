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
 * A [[SequenceDate]] is an instruction rather than a date: it says which date to take from a
 * [[DateSequence]], and it is only when the two are put together that a date exists. That is
 * why almost every case here is written as a pair - an instruction built by one of the eight
 * factories, and the date [[DateSequences.QUARTERLY_IMM_3_SERIAL]] yields when the instruction
 * is applied to it. The sequence is the same one the Java test used, and it is the interesting
 * choice: it is a sequence whose full form interleaves three serial monthly IMM dates with the
 * quarterly ones, so `base` and `full` instructions give measurably different answers over it.
 *
 * ===What the two date ladders pin down===
 *
 * Counted from 1 January 2020, the base sequence answers 2020-03-18, 2020-06-17, 2020-09-16 -
 * the quarterly IMM dates alone - while the full sequence answers 2020-01-15, 2020-02-19,
 * 2020-03-18, 2020-06-17, 2020-09-16, the serial months first and the quarterly dates after
 * them. Those two ladders, in `test_base_int` and `test_full_int`, are the whole of the
 * difference between the two families of factory, and they are transcribed here date for date
 * from the Java test rather than derived.
 *
 * The third ladder, in `test_base_Period_int`, is the only coverage of how a minimum period
 * interacts with the count: adding one or two months to 1 January 2020 still leaves the March
 * date ahead, three, four or five months push the answer out to June, and six or seven months
 * push it to September. Seven rows collapsing onto three distinct dates looks redundant and is
 * not - it is where the boundaries of the interaction are.
 *
 * ===Failure is a value here===
 *
 * The Java constructor threw `IllegalArgumentException` for three combinations of inputs. All
 * three depend on the data supplied rather than on a broken caller contract, so the port
 * reports them through the error channel of `EitherNec[Failure, SequenceDate]`, and this spec
 * asserts them as failures carrying [[FailureReason.INVALID]] - by reason, never by message
 * text. Because every factory answers with that outcome type, each construction is unwrapped
 * exactly once, through `EitherValues`, which reports the failures it found if a construction
 * that should have succeeded did not.
 *
 * Two of the three rejections and the normalisation of a zero minimum period are asserted in
 * `coverage`: the roster of test names is fixed by the migration manifest at the ten names the
 * Java class declared, and `coverage` is the one whose Java body - two reflective bean sweeps -
 * has no target in this port.
 */
class SequenceDateSpec extends AnyFunSuite with Matchers with EitherValues {

  /**
   * The sequence every instruction in this spec is applied to.
   *
   * Its base sequence is the quarterly IMM sequence and its full sequence interleaves the IMM
   * dates of the three nearest months with it, which is what makes the `base` and `full`
   * ladders differ.
   */
  private val SEQUENCE: DateSequence = DateSequences.QUARTERLY_IMM_3_SERIAL

  /** February 2020, a month of no quarter, so the date it selects lies outside it. */
  private val YM_2020_02: YearMonth = YearMonth.of(2020, 2)

  /** March 2020, a quarterly month, whose own IMM date is the one selected. */
  private val YM_2020_03: YearMonth = YearMonth.of(2020, 3)

  /** The minimum period carried by the instructions built with one. */
  private val PERIOD: Period = Period.ofMonths(2)

  /** The date every ladder counts from, chosen so that no sequence date has passed yet. */
  private val INPUT_2020_01_01: LocalDate = date(2020, 1, 1)

  /**
   * A date that is itself a sequence date, which is what separates `selectDate` from
   * `selectDateOrSame`.
   */
  private val INPUT_2020_03_18: LocalDate = date(2020, 3, 18)

  //-------------------------------------------------------------------------
  /**
   * Unwraps a construction that is expected to succeed.
   *
   * Every factory answers with `EitherNec[Failure, SequenceDate]`, so an instruction has to be
   * taken out of that outcome before a sequence can be applied to it. `EitherValues` does the
   * taking, so a construction that unexpectedly failed is reported as a failed assertion at the
   * line that built it rather than as a `NoSuchElementException` from a forced access.
   *
   * @param result  the outcome of one of the factories
   * @return the instruction the factory built
   */
  private def instruction(result: EitherNec[Failure, SequenceDate]): SequenceDate = result.value

  /**
   * Applies an instruction to the sequence, taking a date strictly later than the input date.
   *
   * @param inputDate  the date the count starts from
   * @param result  the outcome of one of the factories
   * @return the date the sequence selects
   */
  private def selected(inputDate: LocalDate, result: EitherNec[Failure, SequenceDate]): LocalDate =
    SEQUENCE.selectDate(inputDate, instruction(result))

  /**
   * Applies an instruction to the sequence, admitting the input date itself as the answer.
   *
   * @param inputDate  the date the count starts from
   * @param result  the outcome of one of the factories
   * @return the date the sequence selects, which may be the input date
   */
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

    // A year-month replaces the input date entirely: the count starts at the first day of the
    // month named, and the first day of the month itself may be the answer.
    selected(INPUT_2020_01_01, SequenceDate.base(YM_2020_02, 1)) shouldBe date(2020, 3, 18)
    selected(INPUT_2020_01_01, SequenceDate.base(YM_2020_02, 2)) shouldBe date(2020, 6, 17)
    selected(INPUT_2020_01_01, SequenceDate.base(YM_2020_02, 3)) shouldBe date(2020, 9, 16)

    // February is a month of no quarter and March is a quarterly month, yet both answer with
    // the March date: a quarterly sequence asked for February gives the next quarter, and
    // asked for March gives March's own IMM date, which is the same date.
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

    // The base ladder: counting over the base sequence skips the serial monthly IMM dates of
    // January and February that the full ladder in `test_full_int` returns.
    selected(INPUT_2020_01_01, SequenceDate.base(1)) shouldBe date(2020, 3, 18)
    selected(INPUT_2020_01_01, SequenceDate.base(2)) shouldBe date(2020, 6, 17)
    selected(INPUT_2020_01_01, SequenceDate.base(3)) shouldBe date(2020, 9, 16)

    // The input date is itself a base sequence date, so the two selection methods part company:
    // one steps over it, the other returns it.
    selected(INPUT_2020_03_18, SequenceDate.base(1)) shouldBe date(2020, 6, 17)
    selectedOrSame(INPUT_2020_03_18, SequenceDate.base(1)) shouldBe date(2020, 3, 18)
  }

  test("test_base_Period_int") {
    val test = instruction(SequenceDate.base(PERIOD, 3))
    test.yearMonth shouldBe None
    test.minimumPeriod shouldBe Some(PERIOD)
    test.sequenceNumber shouldBe 3
    test.fullSequence shouldBe false

    // A minimum period that runs backwards describes no starting point. The Java constructor
    // threw for this; the port reports it as a failure carrying the reason, in both the units a
    // negative period can be expressed in.
    SequenceDate.base(Period.ofMonths(-1), 3) should beFailureWith(FailureReason.INVALID)
    SequenceDate.base(Period.ofDays(-1), 3) should beFailureWith(FailureReason.INVALID)

    // The minimum period moves the starting point forward and the sequence number then counts
    // from there, so the seven periods below collapse onto the three distinct dates that the
    // base sequence has ahead of 1 January 2020. The boundaries are where it matters: two
    // months still leave the March date ahead, three months no longer do.
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
  }

  test("test_full_int") {
    val test = instruction(SequenceDate.full(2))
    test.yearMonth shouldBe None
    test.minimumPeriod shouldBe None
    test.sequenceNumber shouldBe 2
    test.fullSequence shouldBe true

    // The full ladder, and the whole point of the distinction: the first three dates are the
    // IMM dates of January, February and March - the two serial months that the base ladder in
    // `test_base_int` skips - and only then does the quarterly sequence resume.
    selected(INPUT_2020_01_01, SequenceDate.full(1)) shouldBe date(2020, 1, 15)
    selected(INPUT_2020_01_01, SequenceDate.full(2)) shouldBe date(2020, 2, 19)
    selected(INPUT_2020_01_01, SequenceDate.full(3)) shouldBe date(2020, 3, 18)
    selected(INPUT_2020_01_01, SequenceDate.full(4)) shouldBe date(2020, 6, 17)
    selected(INPUT_2020_01_01, SequenceDate.full(5)) shouldBe date(2020, 9, 16)

    // The same contrast as in `test_base_int`, over the full sequence: stepping over the input
    // date lands on April's serial IMM date rather than on June's quarterly one.
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
    // The Java body was `coverImmutableBean(test)` and `coverBeanEquals(test, test2)`: two
    // reflective sweeps over a bean's properties and equality. Both drove the meta-bean
    // machinery of the library being ported, which has no target here, so what they actually
    // established is asserted directly - that equality, hashing and rendering are those of the
    // four fields an instruction carries, and nothing else.
    val test = instruction(SequenceDate.full(PERIOD, 3))
    val same = instruction(SequenceDate.full(PERIOD, 3))
    val test2 = instruction(SequenceDate.base(YM_2020_02, 2))

    test shouldBe same
    test.hashCode shouldBe same.hashCode
    test should not be test2
    // The equals contract against a value of an unrelated type, in method form because the
    // compiler rightly rejects comparing the two with `==`. The unrelated value is the
    // instruction's own rendering, so nothing here has to restate a rendering literal.
    test.equals(test.toString) shouldBe false

    // The single equality-bearing instance agrees with the equality of the values themselves.
    Hash[SequenceDate].eqv(test, same) shouldBe true
    Hash[SequenceDate].eqv(test, test2) shouldBe false
    Hash[SequenceDate].hash(test) shouldBe test.hashCode

    // The rendering names all four fields, always, in the declaration order of the type, with
    // an absent optional field carrying the marker the Java bean printed for one. Both strings
    // are transcribed from that bean's own output, so they are the parity assertion and not a
    // restatement of what the code does.
    test.toString shouldBe
      "SequenceDate{yearMonth=null, minimumPeriod=P2M, sequenceNumber=3, fullSequence=true}"
    test2.toString shouldBe
      "SequenceDate{yearMonth=2020-02, minimumPeriod=null, sequenceNumber=2, fullSequence=false}"

    // The `Show` instance is that rendering rather than a second one, so a future change to
    // either that left the other behind fails here.
    Show[SequenceDate].show(test) shouldBe test.toString
    Show[SequenceDate].show(test2) shouldBe test2.toString

    // The two rejections the Java test left uncovered, both of them properties of the data and
    // both validated by the factory every other case here goes through. They are asserted in
    // this test because the roster of names is fixed at the ten the Java class declared, and
    // this is the one whose original body has no target.
    val bothStartingPoints =
      SequenceDate.of(Some(YM_2020_02), Some(PERIOD), 1, fullSequence = false)
    bothStartingPoints should beFailureWith(FailureReason.INVALID)
    SequenceDate.base(0) should beFailureWith(FailureReason.INVALID)
    SequenceDate.base(-1) should beFailureWith(FailureReason.INVALID)
    SequenceDate.full(0) should beFailureWith(FailureReason.INVALID)

    // A minimum period of zero is no minimum period at all and is normalised away. That
    // normalisation is what makes this a normalising type rather than a merely validated one,
    // and it makes the two instructions below the same value.
    val fromZeroPeriod = instruction(SequenceDate.base(Period.ZERO, 1))
    fromZeroPeriod.minimumPeriod shouldBe None
    fromZeroPeriod shouldBe instruction(SequenceDate.base(1))
    val fullFromZeroPeriod = instruction(SequenceDate.full(Period.ZERO, 1))
    fullFromZeroPeriod.minimumPeriod shouldBe None
    fullFromZeroPeriod shouldBe instruction(SequenceDate.full(1))
  }

  test("test_serialization") {
    // The Java body asserted a round trip through the serialization of the library being
    // ported. The round trip is the same assertion; the format is JSON, written by the codec
    // the companion defines.
    val test = instruction(SequenceDate.full(PERIOD, 3))
    val json = test.asJson
    Decoder[SequenceDate].decodeJson(json) shouldBe Right(test)

    // The encoder is wrapped so that a field holding nothing is dropped rather than written as
    // explicitly empty, so the absent year-month does not appear in the output at all.
    val fields = json.asObject.map(_.keys.toList).getOrElse(Nil)
    fields should not contain "yearMonth"
    fields should contain theSameElementsAs List("minimumPeriod", "sequenceNumber", "fullSequence")
    json.noSpaces should not include "null"

    // The whole document, character for character, because the encoder is derived from the
    // product: this pins the field set and the field order the derivation produces, so a field
    // gained, lost or reordered by a change to the product fails here rather than downstream.
    json.noSpaces shouldBe """{"minimumPeriod":"P2M","sequenceNumber":3,"fullSequence":true}"""

    // The other half of that: a document with the optional field missing decodes to nothing
    // held, which is what makes the round trip above exact.
    val withoutYearMonth = Json.obj(
      "minimumPeriod" -> Json.fromString("P2M"),
      "sequenceNumber" -> Json.fromInt(3),
      "fullSequence" -> Json.True)
    Decoder[SequenceDate].decodeJson(withoutYearMonth) shouldBe Right(test)
    Decoder[SequenceDate].decodeJson(withoutYearMonth).map(_.yearMonth) shouldBe Right(None)

    // An instruction whose optional field is present round-trips through the same codec, so
    // both shapes of the optional field are exercised rather than only the absent one.
    val withYearMonth = instruction(SequenceDate.base(YM_2020_02, 2))
    Decoder[SequenceDate].decodeJson(withYearMonth.asJson) shouldBe Right(withYearMonth)
    withYearMonth.asJson.asObject.map(_.keys.toList).getOrElse(Nil) should contain("yearMonth")
    withYearMonth.asJson.noSpaces shouldBe
      """{"yearMonth":"2020-02","sequenceNumber":2,"fullSequence":false}"""

    // The decoder validates through the same factory a caller's inputs go through, so a
    // document describing no instruction is a decoding failure rather than an invalid value.
    val invalidPayload = Json.obj(
      "sequenceNumber" -> Json.fromInt(0),
      "fullSequence" -> Json.False)
    val decoded: Either[DecodingFailure, SequenceDate] =
      Decoder[SequenceDate].decodeJson(invalidPayload)
    decoded.toOption shouldBe None
    decoded.swap.map(_.message).getOrElse("") should include("sequenceNumber")
  }
}
