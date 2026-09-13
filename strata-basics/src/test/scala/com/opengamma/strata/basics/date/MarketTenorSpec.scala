/*
 * Copyright (C) 2020 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.Period

import scala.util.Random
import scala.util.Try

import cats.Hash
import cats.Order
import cats.Show

import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor1
import org.scalatest.prop.TableFor2

import com.opengamma.strata.basics.date.Tenor.TENOR_1D
import com.opengamma.strata.basics.date.Tenor.TENOR_1W
import com.opengamma.strata.basics.date.Tenor.TENOR_2M
import com.opengamma.strata.basics.date.Tenor.TENOR_3Y
import com.opengamma.strata.collect.Validate
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

  /**
   * The corpus `test_parse_agrees_with_java_time` reads, one text per row.
   *
   * This type reads its own four market codes and hands everything else to [[Tenor.parse]], so
   * the rows are the four codes and their near misses together with the corpus of the tenor: both
   * spellings of an accepted tenor, the case rules that admit `3m` and `P3m` while refusing
   * `p3m`, the multi-section forms in order and out of it, the counts at and past the bounds of
   * an `Int` including a week count whose folding into days overflows, the periods a tenor
   * refuses for being zero or negative, the day and week tenors the spot-starting factory names
   * `ON` and `SW` rather than by their own codes, and the malformed shapes.
   */
  private val data_parseAgreement: TableFor1[String] = Table(
    "input",
    "", "ON", "TN", "SN", "SW", "on", "PON", "ONN", "P", "1D", "1W", "P1D", "P1W", "7D", "P7D",
    "3M", "P3M", "p3m", "P3m", "3m", "2D", "2W", "6W", "12M", "1Y", "10Y", "P1Y2M3D", "P1Y2M",
    "P2Y6M", "P1W3D", "P0D", "0D", "P-2D", "-2D", "-P2D", "+P2D", "P+2D", "P2147483647D",
    "P2147483648D", "P99999999999999999999D", "P2147483647W", "2K", "Rubbish", "P3M4", "3M4",
    "PT1H", "P1D2Y", "P1M1Y", " P3M", "P3M ", "P3.5M", "P1M2", "M3", "3", "-", "+", "PP3M",
    "P3MM", "p", "P3W4D", "P1Y1M1W1D", "P1y2m3w4d", "P000000000000003M", "P0Y0M0W0D",
    "P\uFF11M", "P306783379W", "P306783379W-2147483645D", "P1W-2147483648D", "P-2147483648D",
    "P-2147483649D", "P1D1D", "P2W1W", "P1Y1Y", "P 3M", "PD", "P-D", "1P", "P1"
  )

  /**
   * Every text the combination sweep of `test_parse_agrees_with_java_time` reads.
   *
   * The corpus above names the shapes a reader would think of; this is the mechanical
   * complement, assembled from the pieces of the grammar of a period rather than chosen: a
   * prefix, then a section of a sign, a count and a unit letter, then a tail that is sometimes
   * another section and sometimes debris. Four thousand three hundred and twenty texts result,
   * the great majority of them refusals, which is the half of the behaviour that used to be
   * reported by a constructed exception.
   */
  private val data_parseAgreementCombinations: TableFor1[String] = Table(
    "input",
    (for {
      prefix <- List("P", "p", "")
      sign <- List("", "-", "+")
      count <- List("0", "1", "7", "12", "000012", "2147483647", "2147483648", "306783379")
      unit <- List("Y", "y", "M", "m", "W", "w", "D", "d", "", "X")
      tail <- List("", "3D", "-3d", "1Y", "2W7D", "4")
    } yield prefix + sign + count + unit + tail): _*
  )

  /**
   * Parses a market tenor as the exception-driven implementation this port replaced parsed it.
   *
   * This is [[MarketTenor.parse]] with one substitution: the tenor's own text is read by handing
   * it to `java.time.Period.parse` inside a `Try`, which is how this port read it until the cost
   * of the discarded `DateTimeParseException` was measured. Everything around that - the ceiling
   * of the grammar, the argument check that refuses empty text before anything else, the four
   * market codes, and the spot-starting factory the period is handed to - is reproduced as the
   * method performs it, so the oracle differs from the method under test in exactly the one place
   * the change was made.
   *
   * @param toParse  the text to parse
   * @return the outcome the exception-driven implementation produced for that text
   */
  private def exceptionDrivenParse(toParse: String): Either[Failure, MarketTenor] =
    if (toParse.length > 256) {
      Left(Failure.Parsing("Market tenor string must not exceed 256 characters"))
    } else {
      Validate
        .notEmpty(toParse, "toParse")
        .toEither
        .left
        .map(Failure.collapse)
        .flatMap {
          case "ON" => Right(MarketTenor.ON)
          case "TN" => Right(MarketTenor.TN)
          case "SN" => Right(MarketTenor.SN)
          case "SW" => Right(MarketTenor.SW)
          case text => exceptionDrivenTenor(text).flatMap(MarketTenor.ofSpot)
        }
    }

  /**
   * Reads the tenor of a text as the exception-driven implementation read it.
   *
   * The tail of the oracle above, kept separate because it is the part that changed: the ceiling,
   * the prefixing of a missing upper-case `P`, the throw that stands for a refusal, and
   * [[Tenor.of]] over the period that was read.
   *
   * @param toParse  the text to parse
   * @return the tenor the exception-driven implementation read from that text
   */
  private def exceptionDrivenTenor(toParse: String): Either[Failure, Tenor] =
    if (toParse.length > 256) {
      Left(Failure.Parsing("Tenor string must not exceed 256 characters"))
    } else {
      val prefixed = if (toParse.startsWith("P")) toParse else s"P$toParse"
      Try(Period.parse(prefixed)).toEither match {
        case Right(period) => Tenor.of(period).left.map(Failure.collapse)
        case Left(_) => Left(Failure.Parsing(s"Unable to parse tenor: '$toParse'"))
      }
    }

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

  /**
   * Asserts that reading the text with a walk reads every text exactly as `java.time.Period`
   * read it.
   *
   * No counterpart in the Java test class. This type parses its own four codes and hands
   * everything else to [[Tenor.parse]], which read its text by handing it to `Period.parse`
   * inside a `try`/`catch` until the cost of that was measured: a rejection built a
   * `DateTimeParseException` - message, captured text and stack trace - only to be discarded, and
   * an acceptance built a regular-expression matcher over the text. The text is now read by a
   * walk of its characters, and this test says the substitution moved nothing '''here''' as well
   * as in the tenor, which matters because this type is the one caller of that parse whose own
   * factory can refuse what it read: the '''whole''' outcome is compared, so the value of an
   * acceptance and the reason, the message and the attributes of a refusal are all compared
   * against the exception-driven implementation.
   *
   * The three refusals this type distinguishes are all in the corpus and all compared by message:
   * empty text is refused by the argument check before any reading, `2K` is not a period at all
   * and is a `PARSING` failure quoting the text, and `-2D` is a period that a tenor refuses for
   * being negative and is an `INVALID` failure carrying the tenor factory's message.
   */
  test("test_parse_agrees_with_java_time") {
    forAll(data_parseAgreement) { (input: String) =>
      MarketTenor.parse(input) shouldBe exceptionDrivenParse(input)
    }
    forAll(data_parseAgreementCombinations) { (input: String) =>
      MarketTenor.parse(input) shouldBe exceptionDrivenParse(input)
    }
    // the sweep is the size it claims to be, so a table that silently collapsed - a `for`
    // comprehension over an empty list is still a table - could not leave this test passing
    data_parseAgreementCombinations.size shouldBe 4320
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
