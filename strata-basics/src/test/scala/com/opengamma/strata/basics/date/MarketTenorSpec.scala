/*
 * Copyright (C) 2020 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import scala.util.Random

import cats.Hash
import cats.Order
import cats.Show

import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2

import com.opengamma.strata.basics.date.Tenor.TENOR_1D
import com.opengamma.strata.basics.date.Tenor.TENOR_1W
import com.opengamma.strata.basics.date.Tenor.TENOR_2M
import com.opengamma.strata.basics.date.Tenor.TENOR_3Y
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Tests for the market tenor class. Every factory of [[MarketTenor]] reports a rejected input as a
 * failure value rather than by throwing, so a negative case asserts the reason the failure carries.
 */
class MarketTenorSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /** The market conventional spot lag, returned unchanged for a market tenor starting at spot. */
  private val SPOT_LAG_2: DaysAdjustment = DaysAdjustment.ofBusinessDays(2, HolidayCalendarIds.GBLO)

  /** The spot lag of one business day, which is also what `TN` overrides a lag to. */
  private val SPOT_LAG_1: DaysAdjustment = DaysAdjustment.ofBusinessDays(1, HolidayCalendarIds.GBLO)

  /**
   * The spot lag of no business days, which is what `ON` overrides a lag to. Adding zero business
   * days names no day, so the zero-day case of [[DaysAdjustment.ofBusinessDays]] holds
   * `(0, NoHolidays, Following against GBLO)`, whose result calendar is still `GBLO` - which is
   * why `ON.adjustSpotLag(SPOT_LAG_0)` returns it unchanged.
   */
  private val SPOT_LAG_0: DaysAdjustment = DaysAdjustment.ofBusinessDays(0, HolidayCalendarIds.GBLO)

  /** A value of an unrelated type: an unrelated-type `==` is a fatal warning in this build, so
   *  `equals` is called as a method on a value typed `Any`. */
  private val ANOTHER_TYPE: Any = ""

  /** Text that names no market tenor, typed `Any` for the reason above. */
  private val BAD_TEXT: Any = "BAD"

  /** The code of a market tenor as bare text, typed `Any` for the reason above. */
  private val CODE_AS_TEXT: Any = "ON"

  //-------------------------------------------------------------------------
  /** Returns the market tenor a factory produced, failing the test if it failed instead. */
  private def accepted(result: Either[Failure, MarketTenor]): MarketTenor =
    result.fold(
      failure => fail(s"Expected a market tenor but the factory failed with: ${failure.message}"),
      marketTenor => marketTenor)

  //-------------------------------------------------------------------------
  /** Text the parse accepts and the market tenor it names; the last two rows are one market tenor
   *  in both spellings the tenor parse accepts, the canonical `2M` and the ISO-8601 `P2M`. */
  private val data_parseGood: TableFor2[String, MarketTenor] = Table(
    ("input", "expected"),
    ("ON", MarketTenor.ON),
    ("TN", MarketTenor.TN),
    ("SN", MarketTenor.SN),
    ("SW", MarketTenor.SW),
    ("2M", accepted(MarketTenor.ofSpot(TENOR_2M))),
    ("P2M", accepted(MarketTenor.ofSpot(TENOR_2M)))
  )

  /**
   * Text the parse rejects and the reason it reports: text naming no period at all - `PON` being
   * `ON` with the ISO-8601 prefix - is a parsing failure, the empty string is caught by the
   * argument check performed first, and `-2D` names a period `java.time` accepts and is rejected
   * one step later for being negative, so it carries that reason instead.
   */
  private val data_parseBad: TableFor2[String, FailureReason] = Table(
    ("input", "reason"),
    ("", FailureReason.INVALID),
    ("2", FailureReason.PARSING),
    ("2K", FailureReason.PARSING),
    ("-2D", FailureReason.INVALID),
    ("PON", FailureReason.PARSING)
  )

  //-------------------------------------------------------------------------
  test("test_on") {
    // `ON` overrides a supplied lag to zero business days
    val test = MarketTenor.ON
    test.code shouldBe "ON"
    test.tenor shouldBe TENOR_1D
    test.isNonStandardSpotLag shouldBe true
    test.spotLagIndicator shouldBe 0
    test.adjustSpotLag(SPOT_LAG_2) shouldBe SPOT_LAG_0
    test.adjustSpotLag(SPOT_LAG_1) shouldBe SPOT_LAG_0
    test.adjustSpotLag(SPOT_LAG_0) shouldBe SPOT_LAG_0
  }

  test("test_tn") {
    val test = MarketTenor.TN
    test.code shouldBe "TN"
    test.tenor shouldBe TENOR_1D
    test.isNonStandardSpotLag shouldBe true
    test.spotLagIndicator shouldBe 1
    test.adjustSpotLag(SPOT_LAG_2) shouldBe SPOT_LAG_1
    test.adjustSpotLag(SPOT_LAG_1) shouldBe SPOT_LAG_1
    test.adjustSpotLag(SPOT_LAG_0) shouldBe SPOT_LAG_1

    // For a non-standard lag, `adjustSpotLag` discards the day count of the lag supplied and
    // rebuilds it as `ofBusinessDays(spotLagIndicator, suppliedLag.resultCalendar)`.
    test.adjustSpotLag(DaysAdjustment.ofBusinessDays(5, HolidayCalendarIds.USNY)) shouldBe
      DaysAdjustment.ofBusinessDays(1, HolidayCalendarIds.USNY)
  }

  test("test_sn") {
    val test = MarketTenor.SN
    test.code shouldBe "SN"
    test.tenor shouldBe TENOR_1D
    test.isNonStandardSpotLag shouldBe false
    test.adjustSpotLag(SPOT_LAG_2) shouldBe SPOT_LAG_2
    test.adjustSpotLag(SPOT_LAG_1) shouldBe SPOT_LAG_1
    test.adjustSpotLag(SPOT_LAG_0) shouldBe SPOT_LAG_0

    // a spot-starting market tenor returns the very lag it was given, not one merely equal to it
    (test.adjustSpotLag(SPOT_LAG_2) eq SPOT_LAG_2) shouldBe true
  }

  test("test_sw") {
    val test = MarketTenor.SW
    test.code shouldBe "SW"
    test.tenor shouldBe TENOR_1W
    test.isNonStandardSpotLag shouldBe false
    test.adjustSpotLag(SPOT_LAG_2) shouldBe SPOT_LAG_2
    test.adjustSpotLag(SPOT_LAG_1) shouldBe SPOT_LAG_1
    test.adjustSpotLag(SPOT_LAG_0) shouldBe SPOT_LAG_0
  }

  //-------------------------------------------------------------------------
  test("test_ofSpot") {
    val test = accepted(MarketTenor.ofSpot(TENOR_3Y))
    test.code shouldBe "3Y"
    test.tenor shouldBe TENOR_3Y
    test.isNonStandardSpotLag shouldBe false
    test.adjustSpotLag(SPOT_LAG_2) shouldBe SPOT_LAG_2
    test.adjustSpotLag(SPOT_LAG_1) shouldBe SPOT_LAG_1
    test.adjustSpotLag(SPOT_LAG_0) shouldBe SPOT_LAG_0
  }

  test("test_ofSpot_special") {
    // a spot-starting day and week are the market's `SN` and `SW`, not codes `1D` and `1W`
    MarketTenor.ofSpot(TENOR_1W) should haveValue(MarketTenor.SW)
    MarketTenor.ofSpot(TENOR_1D) should haveValue(MarketTenor.SN)
  }

  //-------------------------------------------------------------------------
  test("test_ofSpotDays") {
    accepted(MarketTenor.ofSpotDays(1)).code shouldBe "SN"
    accepted(MarketTenor.ofSpotDays(7)).code shouldBe "SW"
    accepted(MarketTenor.ofSpotDays(3)).code shouldBe "3D"
    accepted(MarketTenor.ofSpotDays(20)).code shouldBe "20D"
    MarketTenor.ofSpotDays(-1) should beFailureWith(FailureReason.INVALID)
  }

  test("test_ofSpotMonths") {
    accepted(MarketTenor.ofSpotMonths(3)).code shouldBe "3M"
    accepted(MarketTenor.ofSpotMonths(20)).code shouldBe "20M"
    // both the negative month count and the negative day count are asserted here
    MarketTenor.ofSpotMonths(-1) should beFailureWith(FailureReason.INVALID)
    MarketTenor.ofSpotDays(-1) should beFailureWith(FailureReason.INVALID)
  }

  test("test_ofSpotYears") {
    accepted(MarketTenor.ofSpotYears(3)).code shouldBe "3Y"
    accepted(MarketTenor.ofSpotYears(20)).code shouldBe "20Y"
    MarketTenor.ofSpotYears(-1) should beFailureWith(FailureReason.INVALID)
  }

  //-------------------------------------------------------------------------
  test("test_parse_String_roundTrip") {
    val representatives: List[MarketTenor] = List(
      MarketTenor.ON,
      MarketTenor.TN,
      MarketTenor.SN,
      MarketTenor.SW,
      accepted(MarketTenor.ofSpot(TENOR_3Y))
    )
    val failed = representatives.filterNot(tenor => MarketTenor.parse(tenor.toString) == Right(tenor))
    failed shouldBe empty
  }

  test("test_parse_String_good_noP") {
    forAll(data_parseGood) { (input: String, expected: MarketTenor) =>
      MarketTenor.parse(input) should haveValue(expected)
    }
  }

  test("test_parse_String_bad") {
    forAll(data_parseBad) { (input: String, reason: FailureReason) =>
      MarketTenor.parse(input) should beFailureWith(reason)
    }
  }

  //-------------------------------------------------------------------------
  test("test_compare") {
    val marketTenors: List[MarketTenor] = List(
      MarketTenor.ON,
      MarketTenor.TN,
      MarketTenor.SN,
      accepted(MarketTenor.ofSpotDays(2)),
      accepted(MarketTenor.ofSpotDays(6)),
      MarketTenor.SW,
      accepted(MarketTenor.ofSpotDays(8)),
      accepted(MarketTenor.ofSpotMonths(1))
    )
    marketTenors should have size 8
    marketTenors.map(_.code) shouldBe List("ON", "TN", "SN", "2D", "6D", "SW", "8D", "1M")

    val ordering = Order[MarketTenor].toOrdering
    val outOfOrder = marketTenors.zip(marketTenors.tail).filterNot {
      case (earlier, later) => Order[MarketTenor].compare(earlier, later) < 0
    }
    outOfOrder shouldBe empty

    // Two fixed permutations - a shuffle with a fixed seed and the reverse of the list, the worst
    // case for any comparison sort - so a sort failure here is reproducible.
    val shuffled = new Random(20200101L).shuffle(marketTenors)
    shuffled shouldNot be(marketTenors)
    shuffled.sorted(ordering) shouldBe marketTenors
    marketTenors.reverse.sorted(ordering) shouldBe marketTenors

    // `compareTo` ranks twelve months and one year equal although the values are unequal, so
    // `Order` breaks that tie by code and places `12M` before `1Y`; the tie-break is consulted
    // only where `compareTo` returns zero, so `compare` is zero for exactly the pairs `eqv` holds.
    val twelveMonths = accepted(MarketTenor.ofSpotMonths(12))
    val oneYear = accepted(MarketTenor.ofSpotYears(1))
    twelveMonths.code shouldBe "12M"
    oneYear.code shouldBe "1Y"
    twelveMonths.compareTo(oneYear) shouldBe 0
    Order[MarketTenor].compare(twelveMonths, oneYear) should be < 0
    Order[MarketTenor].compare(oneYear, twelveMonths) should be > 0
    Order[MarketTenor].eqv(twelveMonths, oneYear) shouldBe false
  }

  //-------------------------------------------------------------------------
  test("test_equals_hashCode") {
    val a1 = MarketTenor.SN
    val a2 = accepted(MarketTenor.ofSpot(TENOR_1D))
    val b = MarketTenor.ON
    a1.equals(a1) shouldBe true
    a1.equals(b) shouldBe false
    a1.equals(a2) shouldBe true

    a2.equals(a1) shouldBe true
    a2.equals(a2) shouldBe true
    a2.equals(b) shouldBe false

    b.equals(a1) shouldBe false
    b.equals(a2) shouldBe false
    b.equals(b) shouldBe true

    a1.hashCode shouldBe a2.hashCode

    Order[MarketTenor].eqv(a1, a2) shouldBe true
    Order[MarketTenor].eqv(a1, b) shouldBe false
    Hash[MarketTenor].hash(a1) shouldBe Hash[MarketTenor].hash(a2)

    // Equality and hashing consider the code alone, so these three construction routes give one
    // equal value, and the tenor and lag each carries agree because the code determines both.
    val fromTenor = accepted(MarketTenor.ofSpot(TENOR_3Y))
    val fromText = accepted(MarketTenor.parse("3Y"))
    val fromCount = accepted(MarketTenor.ofSpotYears(3))
    List(fromTenor, fromText, fromCount).map(_.code) shouldBe List("3Y", "3Y", "3Y")
    fromTenor.equals(fromText) shouldBe true
    fromTenor.equals(fromCount) shouldBe true
    fromText.equals(fromCount) shouldBe true
    fromTenor.hashCode shouldBe fromText.hashCode
    fromTenor.hashCode shouldBe fromCount.hashCode
    Order[MarketTenor].eqv(fromTenor, fromText) shouldBe true
    Hash[MarketTenor].hash(fromTenor) shouldBe Hash[MarketTenor].hash(fromCount)
    fromText.tenor shouldBe TENOR_3Y
    fromText.isNonStandardSpotLag shouldBe false
    fromCount.tenor shouldBe TENOR_3Y

    // codes differ, so `SN` and `ON` are unequal although both carry a tenor of one day
    a1.tenor shouldBe b.tenor
    a1.equals(b) shouldBe false
  }

  test("test_equals_bad") {
    MarketTenor.ON.equals(MarketTenor.ON) shouldBe true
    MarketTenor.ON.equals(MarketTenor.TN) shouldBe false
    MarketTenor.ON.equals(MarketTenor.SN) shouldBe false
    MarketTenor.ON.equals(MarketTenor.SW) shouldBe false
    MarketTenor.ON.equals(BAD_TEXT) shouldBe false
    MarketTenor.ON.equals(ANOTHER_TYPE) shouldBe false
    MarketTenor.ON.equals(null) shouldBe false
    MarketTenor.ON.equals(CODE_AS_TEXT) shouldBe false
  }

  //-----------------------------------------------------------------------
  test("test_serialization") {
    // the JSON form of a market tenor is the bare code as a string, never an object
    val threeYears = accepted(MarketTenor.ofSpot(TENOR_3Y))
    MarketTenor.ON.asJson shouldBe Json.fromString("ON")
    MarketTenor.SN.asJson shouldBe Json.fromString("SN")
    threeYears.asJson shouldBe Json.fromString("3Y")

    MarketTenor.ON.asJson.isString shouldBe true
    MarketTenor.ON.asJson.isObject shouldBe false

    Json.fromString("ON").as[MarketTenor] shouldBe Right(MarketTenor.ON)
    Json.fromString("SN").as[MarketTenor] shouldBe Right(MarketTenor.SN)
    Json.fromString("3Y").as[MarketTenor] shouldBe Right(threeYears)

    // decoding goes through the same parse, so either spelling it accepts is decoded - `P3Y`, and
    // `1W` naming the value written as `SW`
    Json.fromString("P3Y").as[MarketTenor] shouldBe Right(threeYears)
    Json.fromString("1W").as[MarketTenor] shouldBe Right(MarketTenor.SW)

    Json.fromString("2K").as[MarketTenor].isLeft shouldBe true
    Json.fromString("").as[MarketTenor].isLeft shouldBe true
  }

  test("test_jodaConvert") {
    val threeYears = accepted(MarketTenor.ofSpot(TENOR_3Y))
    Show[MarketTenor].show(MarketTenor.ON) shouldBe "ON"
    Show[MarketTenor].show(MarketTenor.SW) shouldBe "SW"
    Show[MarketTenor].show(threeYears) shouldBe "3Y"
    // the rendering is the code itself, which is also what `toString` produces
    Show[MarketTenor].show(MarketTenor.ON) shouldBe MarketTenor.ON.code
    Show[MarketTenor].show(threeYears) shouldBe threeYears.toString

    MarketTenor.parse(Show[MarketTenor].show(MarketTenor.ON)) should haveValue(MarketTenor.ON)
    MarketTenor.parse(Show[MarketTenor].show(MarketTenor.TN)) should haveValue(MarketTenor.TN)
    MarketTenor.parse(Show[MarketTenor].show(MarketTenor.SN)) should haveValue(MarketTenor.SN)
    MarketTenor.parse(Show[MarketTenor].show(MarketTenor.SW)) should haveValue(MarketTenor.SW)
    MarketTenor.parse(Show[MarketTenor].show(threeYears)) should haveValue(threeYears)
  }
}
