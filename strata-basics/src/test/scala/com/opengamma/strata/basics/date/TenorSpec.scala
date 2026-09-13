/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate
import java.time.OffsetDateTime
import java.time.Period
import java.time.ZoneOffset
import java.time.temporal.ChronoUnit
import java.time.temporal.TemporalUnit
import java.time.temporal.UnsupportedTemporalTypeException

import scala.util.Random
import scala.util.Try

import cats.Hash
import cats.Order
import cats.Show
import cats.data.NonEmptyChain

import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor1
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableFor3

import com.opengamma.strata.basics.date.Tenor.TENOR_10M
import com.opengamma.strata.basics.date.Tenor.TENOR_12M
import com.opengamma.strata.basics.date.Tenor.TENOR_15M
import com.opengamma.strata.basics.date.Tenor.TENOR_18M
import com.opengamma.strata.basics.date.Tenor.TENOR_1D
import com.opengamma.strata.basics.date.Tenor.TENOR_1M
import com.opengamma.strata.basics.date.Tenor.TENOR_1W
import com.opengamma.strata.basics.date.Tenor.TENOR_1Y
import com.opengamma.strata.basics.date.Tenor.TENOR_21M
import com.opengamma.strata.basics.date.Tenor.TENOR_2D
import com.opengamma.strata.basics.date.Tenor.TENOR_2M
import com.opengamma.strata.basics.date.Tenor.TENOR_2W
import com.opengamma.strata.basics.date.Tenor.TENOR_2Y
import com.opengamma.strata.basics.date.Tenor.TENOR_35Y
import com.opengamma.strata.basics.date.Tenor.TENOR_3D
import com.opengamma.strata.basics.date.Tenor.TENOR_3M
import com.opengamma.strata.basics.date.Tenor.TENOR_3W
import com.opengamma.strata.basics.date.Tenor.TENOR_3Y
import com.opengamma.strata.basics.date.Tenor.TENOR_40Y
import com.opengamma.strata.basics.date.Tenor.TENOR_45Y
import com.opengamma.strata.basics.date.Tenor.TENOR_4M
import com.opengamma.strata.basics.date.Tenor.TENOR_4Y
import com.opengamma.strata.basics.date.Tenor.TENOR_50Y
import com.opengamma.strata.basics.date.Tenor.TENOR_6W
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Tests for the tenor class. The factories of [[Tenor]] report a rejected input as a chain of
 * failures rather than by throwing, so a negative case asserts the reason a failure carries; the
 * one exception a member of this type propagates is asserted in `test_temporalAmount`.
 */
class TenorSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /** A value of an unrelated type: an unrelated-type `==` is a fatal warning in this build, so
   *  `equals` is called as a method on a value typed `Any`. */
  private val ANOTHER_TYPE: Any = ""

  /** A plain object, held in a value because comparing against a freshly built one also warns. */
  private val ANOTHER_OBJECT: Any = new AnyRef

  //-------------------------------------------------------------------------
  /** Returns the tenor a factory produced, failing the test if it produced failures instead. */
  private def accepted(result: Either[NonEmptyChain[Failure], Tenor]): Tenor =
    result.fold(
      failures => fail("Expected a tenor but the factory failed with: " +
        failures.toChain.toList.map(_.message).mkString(", ")),
      tenor => tenor)

  //-------------------------------------------------------------------------
  /**
   * The period handed to the factory, the period the tenor holds and the canonical name it takes. A
   * period given in another unit is stored in days - two weeks as fourteen days - and the name is
   * where the rules show: a day count that is a multiple of seven is named in weeks while ten days
   * stays `10D`, and months are never normalised into years, so `12M` stays `12M`.
   */
  private val data_ofPeriod: TableFor3[Period, Period, String] = Table(
    ("period", "stored", "name"),
    (Period.ofDays(1), Period.ofDays(1), "1D"),
    (Period.ofDays(7), Period.ofDays(7), "1W"),
    (Period.ofDays(10), Period.ofDays(10), "10D"),
    (Period.ofWeeks(2), Period.ofDays(14), "2W"),
    (Period.ofMonths(1), Period.ofMonths(1), "1M"),
    (Period.ofMonths(2), Period.ofMonths(2), "2M"),
    (Period.ofMonths(12), Period.ofMonths(12), "12M"),
    (Period.ofYears(1), Period.ofYears(1), "1Y"),
    (Period.ofMonths(20), Period.ofMonths(20), "20M"),
    (Period.ofMonths(24), Period.ofMonths(24), "24M"),
    (Period.ofYears(2), Period.ofYears(2), "2Y"),
    (Period.ofMonths(30), Period.ofMonths(30), "30M"),
    (Period.of(2, 6, 0), Period.of(2, 6, 0), "2Y6M")
  )

  private val data_ofMonths: TableFor3[Int, Period, String] = Table(
    ("months", "stored", "name"),
    (1, Period.ofMonths(1), "1M"),
    (2, Period.ofMonths(2), "2M"),
    (12, Period.ofMonths(12), "12M"),
    (20, Period.ofMonths(20), "20M"),
    (24, Period.ofMonths(24), "24M"),
    (30, Period.ofMonths(30), "30M")
  )

  private val data_ofYears: TableFor3[Int, Period, String] = Table(
    ("years", "stored", "name"),
    (1, Period.ofYears(1), "1Y"),
    (2, Period.ofYears(2), "2Y"),
    (3, Period.ofYears(3), "3Y")
  )

  /**
   * The period of a tenor and the period its normalised form holds, which is the whole rule: days
   * are already canonical and are returned unchanged whether or not the count is a multiple of
   * seven; a year is twelve months by this convention, so one year normalises to twelve months
   * while twelve months stays as it is; and any longer month count carries its excess into years.
   */
  private val data_normalized: TableFor2[Period, Period] = Table(
    ("period", "normalized"),
    (Period.ofDays(1), Period.ofDays(1)),
    (Period.ofDays(7), Period.ofDays(7)),
    (Period.ofDays(10), Period.ofDays(10)),
    (Period.ofWeeks(2), Period.ofDays(14)),
    (Period.ofMonths(1), Period.ofMonths(1)),
    (Period.ofMonths(2), Period.ofMonths(2)),
    (Period.ofMonths(12), Period.ofMonths(12)),
    (Period.ofYears(1), Period.ofMonths(12)),
    (Period.ofMonths(20), Period.of(1, 8, 0)),
    (Period.ofMonths(24), Period.ofYears(2)),
    (Period.ofYears(2), Period.ofYears(2)),
    (Period.ofMonths(30), Period.of(2, 6, 0))
  )

  /** A tenor with whether it is week-based and whether it is month-based; the last row, carrying
   *  months and days, is neither. */
  private val data_based: TableFor3[Tenor, Boolean, Boolean] = Table(
    ("tenor", "weekBased", "monthBased"),
    (accepted(Tenor.ofDays(1)), false, false),
    (accepted(Tenor.ofDays(2)), false, false),
    (accepted(Tenor.ofDays(6)), false, false),
    (accepted(Tenor.ofDays(7)), true, false),
    (accepted(Tenor.ofWeeks(1)), true, false),
    (accepted(Tenor.ofWeeks(3)), true, false),
    (accepted(Tenor.ofMonths(1)), false, true),
    (accepted(Tenor.ofMonths(3)), false, true),
    (accepted(Tenor.ofYears(1)), false, true),
    (accepted(Tenor.ofYears(3)), false, true),
    (accepted(Tenor.of(Period.of(1, 2, 3))), false, false)
  )

  /** The canonical text of a tenor and the tenor it names; the two tests sharing the table assert
   *  this spelling and the same text with the ISO-8601 `P` prefix. */
  private val data_parseGood: TableFor2[String, Tenor] = Table(
    ("input", "expected"),
    ("2D", TENOR_2D),
    ("2W", TENOR_2W),
    ("6W", TENOR_6W),
    ("2M", TENOR_2M),
    ("12M", TENOR_12M),
    ("1Y", TENOR_1Y),
    ("2Y", TENOR_2Y)
  )

  /**
   * Text the parse rejects and the reason it reports: text that names no period at all - the empty
   * string, a bare number, a number with an unknown unit - is a parsing failure, while `-2D` names
   * a period `java.time` accepts and is rejected one step later for being negative, so it carries
   * that reason instead.
   */
  private val data_parseBad: TableFor2[String, FailureReason] = Table(
    ("input", "reason"),
    ("", FailureReason.PARSING),
    ("2", FailureReason.PARSING),
    ("2K", FailureReason.PARSING),
    ("-2D", FailureReason.INVALID)
  )

  /**
   * The corpus `test_parse_agrees_with_java_time` reads, one text per row.
   *
   * Every text of it is named for a reason, and the reasons are worth stating because the parse
   * reads the text with a walk of its own rather than by handing it to `java.time.Period`, and
   * the one thing that walk owes its caller is that no text reads differently than it did. The
   * rows cover: both spellings of an accepted tenor, with and without the ISO-8601 prefix; the
   * case rules, which admit `3m` and `P3m` while refusing `p3m`, only an upper-case `P` counting
   * as a prefix already present; the multi-section forms, in order and out of it; the counts at
   * and past the bounds of an `Int`, including a week count whose folding into days overflows;
   * the periods that read perfectly well and are then refused by [[Tenor.of]] for being zero or
   * negative; and the malformed shapes - empty text, a bare `P`, a bare number, a repeated or
   * misordered section, a decimal point, a time part, surrounding space, a non-ASCII digit, and
   * text that is not a period at all.
   */
  private val data_parseAgreement: TableFor1[String] = Table(
    "input",
    "", "P", "3M", "P3M", "p3m", "P3m", "3m", "2D", "2W", "6W", "12M", "1Y", "10Y", "P1Y2M3D",
    "P1Y2M", "P2Y6M", "P1W3D", "P7D", "P0D", "0D", "P-2D", "-2D", "-P2D", "+P2D", "P+2D",
    "P2147483647D", "P2147483648D", "P99999999999999999999D", "P2147483647W", "2K", "Rubbish",
    "P3M4", "3M4", "PT1H", "P1D2Y", "P1M1Y", " P3M", "P3M ", "P3.5M", "P1M2", "M3", "3", "-",
    "+", "PP3M", "P3MM", "p", "P3W4D", "P1Y1M1W1D", "P1y2m3w4d", "P000000000000003M", "P0Y0M0W0D",
    "P\uFF11M", "P306783379W", "P306783379W-2147483645D", "P1W-2147483648D", "P-2147483648D",
    "P-2147483649D", "P1D1D", "P2W1W", "P1Y1Y", "P3M\n", "P 3M", "PD", "P-D", "1P", "P1"
  )

  /**
   * Every text the combination sweep of `test_parse_agrees_with_java_time` reads.
   *
   * The corpus above names the shapes a reader would think of; this is the mechanical
   * complement, assembled from the pieces of the grammar rather than chosen: a prefix, then a
   * section of a sign, a count and a unit letter, then a tail that is sometimes another section
   * and sometimes debris. Four thousand three hundred and twenty texts result, the great majority
   * of them refusals, which is the half of the behaviour that used to be reported by a
   * constructed exception and is therefore the half most worth sweeping.
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
   * Parses a tenor as the exception-driven implementation this port replaced parsed it.
   *
   * This is that implementation, in full: refuse text past the ceiling of the grammar, prefix a
   * missing upper-case `P`, hand the text to `java.time.Period.parse` and let the
   * `DateTimeParseException` it throws for text it cannot read stand for a refusal, then run
   * [[Tenor.of]] over the period it read. It is the oracle of the agreement test, so that the
   * walk [[Tenor.parse]] now reads text with is held to a statement of the grammar that does not
   * depend on the walk being right.
   *
   * @param toParse  the text to parse
   * @return the outcome the exception-driven implementation produced for that text
   */
  private def exceptionDrivenParse(toParse: String): Either[Failure, Tenor] =
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
  test("test_ofPeriod") {
    forAll(data_ofPeriod) { (period: Period, stored: Period, name: String) =>
      val test = accepted(Tenor.of(period))
      test.period shouldBe stored
      test.name shouldBe name
      test.toString shouldBe name
    }
  }

  test("test_ofMonths") {
    forAll(data_ofMonths) { (months: Int, stored: Period, name: String) =>
      val test = accepted(Tenor.ofMonths(months))
      test.period shouldBe stored
      test.toString shouldBe name
    }
  }

  test("test_ofYears") {
    forAll(data_ofYears) { (years: Int, stored: Period, name: String) =>
      val test = accepted(Tenor.ofYears(years))
      test.period shouldBe stored
      test.toString shouldBe name
    }
  }

  test("test_of_int") {
    Tenor.ofDays(1) should haveValue(TENOR_1D)
    // seven days is a week, and is named as one, so the two constants coincide
    Tenor.ofDays(7) should haveValue(TENOR_1W)
    Tenor.ofWeeks(2) should haveValue(TENOR_2W)
    Tenor.ofMonths(1) should haveValue(TENOR_1M)
    Tenor.ofMonths(15) should haveValue(TENOR_15M)
    Tenor.ofMonths(18) should haveValue(TENOR_18M)
    Tenor.ofMonths(21) should haveValue(TENOR_21M)
    Tenor.ofYears(1) should haveValue(TENOR_1Y)
    Tenor.ofYears(35) should haveValue(TENOR_35Y)
    Tenor.ofYears(40) should haveValue(TENOR_40Y)
    Tenor.ofYears(45) should haveValue(TENOR_45Y)
    Tenor.ofYears(50) should haveValue(TENOR_50Y)
  }

  //-------------------------------------------------------------------------
  test("test_of_notZero") {
    Tenor.of(Period.ofDays(0)) should beFailureWith(FailureReason.INVALID)
    Tenor.ofDays(0) should beFailureWith(FailureReason.INVALID)
    Tenor.ofWeeks(0) should beFailureWith(FailureReason.INVALID)
    Tenor.ofMonths(0) should beFailureWith(FailureReason.INVALID)
    Tenor.ofYears(0) should beFailureWith(FailureReason.INVALID)
  }

  test("test_of_notNegative") {
    Tenor.of(Period.ofDays(-1)) should beFailureWith(FailureReason.INVALID)
    Tenor.ofDays(-1) should beFailureWith(FailureReason.INVALID)
    Tenor.ofWeeks(-1) should beFailureWith(FailureReason.INVALID)
    Tenor.ofMonths(-1) should beFailureWith(FailureReason.INVALID)
    Tenor.ofYears(-1) should beFailureWith(FailureReason.INVALID)
  }

  //-------------------------------------------------------------------------
  test("test_parse_String_roundTrip") {
    // the last row is a mixed months-and-years tenor, whose name is not a count and a unit
    val representatives: List[Tenor] = List(
      TENOR_1D,
      TENOR_3D,
      TENOR_1W,
      TENOR_2W,
      TENOR_1M,
      TENOR_10M,
      TENOR_12M,
      TENOR_1Y,
      TENOR_3Y,
      TENOR_50Y,
      accepted(Tenor.of(Period.of(2, 6, 0)))
    )
    val failed = representatives.filterNot(tenor => Tenor.parse(tenor.toString) == Right(tenor))
    failed shouldBe empty
  }

  test("test_parse_String_good_noP") {
    forAll(data_parseGood) { (input: String, expected: Tenor) =>
      Tenor.parse(input) should haveValue(expected)
    }
    Tenor.parse("3M") should haveValue(TENOR_3M)
  }

  test("test_parse_String_good_withP") {
    forAll(data_parseGood) { (input: String, expected: Tenor) =>
      // the parse prepends a missing `P`, so it accepts the ISO-8601 spelling as well; the
      // canonical name drops that prefix, so both spellings name one tenor
      Tenor.parse("P" + input) should haveValue(expected)
    }
    // `P3M` included: the text alone does not say whether a tenor or a frequency is meant, so it
    // is the type being parsed that decides, and parsed as a tenor it is the three-month tenor
    Tenor.parse("P3M") should haveValue(TENOR_3M)
  }

  test("test_parse_String_bad") {
    forAll(data_parseBad) { (input: String, reason: FailureReason) =>
      Tenor.parse(input) should beFailureWith(reason)
    }
  }

  /**
   * Asserts that reading the text with a walk reads every text exactly as `java.time.Period`
   * read it.
   *
   * No counterpart in the Java test class: the Java method handed the text to `Period.parse`
   * inside a `try`/`catch`, and so did this port until the cost of that was measured. A
   * rejection built a `DateTimeParseException` - message, captured text and stack trace - to be
   * discarded by the `Try` that caught it, and an acceptance built a regular-expression matcher
   * over the text; the parse answers a failure value, so neither was anything a caller could
   * observe. The text is now read by a walk of its characters, and this test is what says the
   * substitution moved nothing: the '''whole''' outcome is compared, so the value of an
   * acceptance and the reason, the message and the attributes of a refusal are all compared, and
   * the oracle it is compared against is the exception-driven implementation itself.
   *
   * The distinction the two halves of the grammar make is the one worth naming. `-2D` is a
   * period `java.time` reads without complaint and a tenor refuses for being negative, so it is
   * an `INVALID` failure carrying [[Tenor.of]]'s own message, while `2K` is not a period at all
   * and is a `PARSING` failure quoting the text. A walk that decided either of those one step
   * earlier or later would change the reason a caller is told, which is why the messages are
   * compared and not only the reasons.
   */
  test("test_parse_agrees_with_java_time") {
    forAll(data_parseAgreement) { (input: String) =>
      Tenor.parse(input) shouldBe exceptionDrivenParse(input)
    }
    forAll(data_parseAgreementCombinations) { (input: String) =>
      Tenor.parse(input) shouldBe exceptionDrivenParse(input)
    }
    // the sweep is the size it claims to be, so a table that silently collapsed - a `for`
    // comprehension over an empty list is still a table - could not leave this test passing
    data_parseAgreementCombinations.size shouldBe 4320
  }

  test("parsing names the text it rejected in full, and the failure renders bounded and on one line") {
    // The failure quotes the text back, which is safe to write out because the rendering of a
    // failure bounds every part it writes and escapes anything that could forge a line.
    //
    // The payload is the largest text the grammar of a tenor admits, two hundred and fifty-six
    // characters: the parse refuses anything longer outright, so text at the ceiling is the
    // largest input whose own spelling is still quoted back. It is named in full, and the
    // rendering, which bounds each part it writes at five hundred and twelve characters,
    // therefore carries the whole of it on one line. What the rendering does to text that
    // overruns its own bound is pinned by the suites whose grammars admit text of any size, such
    // as `CurrencySpec` and `StandardIdSpec`.
    val payload = "A" * 256
    val bounded = Tenor.parse(payload)
    bounded should beFailureWith(FailureReason.PARSING)
    val failure = bounded.left.toOption.getOrElse(fail("expected a failure"))
    failure.message shouldBe s"Unable to parse tenor: '$payload'"
    val rendered = Show[Failure].show(failure)
    rendered.length should be < 1000
    rendered shouldBe s"PARSING: Unable to parse tenor: '$payload'"
    rendered should include(payload)
    rendered.linesIterator.size shouldBe 1

    // One character further is past the ceiling of the grammar, and there the failure names the
    // ceiling instead of the text: the input is refused for its size, so writing it out is the
    // very thing the refusal exists to avoid, and the caller needs the bound rather than a copy
    // of what they sent (CWE-400/CWE-770). The wording is the one `Decimal` reports for the same
    // condition on the numeral it reads. The refusal happens before the leading `P` is looked
    // for and before a character of the text is read, which is why a payload of ten thousand
    // characters costs no more than one of two hundred and fifty-seven.
    List("A" * 257, "A" * 10000, "P" + ("1" * 10000) + "D").foreach { oversized =>
      withClue(s"a payload of ${oversized.length} characters: ") {
        val refused = Tenor.parse(oversized)
        refused should beFailureWith(FailureReason.PARSING)
        val refusal = refused.left.toOption.getOrElse(fail("expected a failure"))
        refusal.message shouldBe "Tenor string must not exceed 256 characters"
        refusal.message should not include oversized
        Show[Failure].show(refusal) shouldBe "PARSING: Tenor string must not exceed 256 characters"
      }
    }

    // a payload holding a line break is named as it stands, and the rendering escapes the break,
    // so the failure occupies one line in a log rather than two
    val injected = Tenor.parse("3M\nINJECTED")
    injected should beFailureWith(FailureReason.PARSING)
    val injectedFailure = injected.left.toOption.getOrElse(fail("expected a failure"))
    injectedFailure.message shouldBe "Unable to parse tenor: '3M\nINJECTED'"
    val injectedRendering = Show[Failure].show(injectedFailure)
    injectedRendering should not include "\n"
    injectedRendering shouldBe "PARSING: Unable to parse tenor: '3M\\nINJECTED'"

    Tenor.parse("Rubbish").left.toOption.map(_.message) shouldBe
      Some("Unable to parse tenor: 'Rubbish'")
  }

  //-------------------------------------------------------------------------
  test("test_getPeriod") {
    TENOR_3D.period shouldBe Period.ofDays(3)
    // three weeks is twenty-one days: the name is in weeks, the period is in days
    TENOR_3W.period shouldBe Period.ofDays(21)
    TENOR_3M.period shouldBe Period.ofMonths(3)
    TENOR_3Y.period shouldBe Period.ofYears(3)
  }

  //-------------------------------------------------------------------------
  test("test_normalized") {
    forAll(data_normalized) { (period: Period, normalized: Period) =>
      accepted(Tenor.of(period)).normalized.period shouldBe normalized
    }
  }

  //-------------------------------------------------------------------------
  test("test_isWeekBased") {
    forAll(data_based) { (test: Tenor, weekBased: Boolean, _: Boolean) =>
      test.isWeekBased shouldBe weekBased
    }
  }

  test("test_isMonthBased") {
    forAll(data_based) { (test: Tenor, _: Boolean, monthBased: Boolean) =>
      test.isMonthBased shouldBe monthBased
    }
  }

  //-------------------------------------------------------------------------
  test("test_addTo") {
    TENOR_3D.addTo(LocalDate.of(2014, 6, 30)) shouldBe LocalDate.of(2014, 7, 3)
    TENOR_1W.addTo(OffsetDateTime.of(2014, 6, 30, 0, 0, 0, 0, ZoneOffset.UTC)) shouldBe
      OffsetDateTime.of(2014, 7, 7, 0, 0, 0, 0, ZoneOffset.UTC)
  }

  test("test_subtractFrom") {
    TENOR_3D.subtractFrom(LocalDate.of(2014, 6, 30)) shouldBe LocalDate.of(2014, 6, 27)
    TENOR_1W.subtractFrom(OffsetDateTime.of(2014, 6, 30, 0, 0, 0, 0, ZoneOffset.UTC)) shouldBe
      OffsetDateTime.of(2014, 6, 23, 0, 0, 0, 0, ZoneOffset.UTC)
  }

  //-------------------------------------------------------------------------
  test("test_temporalAmount") {
    // A tenor is measured in years, months and days: `units` names those three, in that order,
    // `get` answers for each of them, and `addTo`/`subtractFrom` carry the arithmetic.
    TENOR_3D.units shouldBe
      List[TemporalUnit](ChronoUnit.YEARS, ChronoUnit.MONTHS, ChronoUnit.DAYS)
    TENOR_3D.get(ChronoUnit.YEARS) shouldBe 0L
    TENOR_3D.get(ChronoUnit.MONTHS) shouldBe 0L
    TENOR_3D.get(ChronoUnit.DAYS) shouldBe 3L

    TENOR_1W.addTo(LocalDate.of(2014, 6, 30)) shouldBe LocalDate.of(2014, 7, 7)
    TENOR_1W.subtractFrom(LocalDate.of(2014, 6, 30)) shouldBe LocalDate.of(2014, 6, 23)

    // `get` is answered by the underlying `java.time.Period`, which raises this `java.time`
    // exception for any unit other than years, months and days; the exception propagates
    intercept[UnsupportedTemporalTypeException] {
      TENOR_10M.get(ChronoUnit.CENTURIES)
    }
  }

  //-------------------------------------------------------------------------
  test("test_compare") {
    val tenors: List[Tenor] = List(
      accepted(Tenor.ofDays(1)),
      accepted(Tenor.ofDays(3)),
      accepted(Tenor.ofDays(7)),
      accepted(Tenor.ofWeeks(2)),
      accepted(Tenor.ofWeeks(4)),
      accepted(Tenor.ofDays(30)),
      accepted(Tenor.ofMonths(1)),
      accepted(Tenor.ofDays(31)),
      accepted(Tenor.of(Period.of(0, 1, 1))),
      accepted(Tenor.ofDays(60)),
      accepted(Tenor.ofMonths(2)),
      accepted(Tenor.ofDays(61)),
      accepted(Tenor.ofDays(91)),
      accepted(Tenor.ofMonths(3)),
      accepted(Tenor.ofDays(92)),
      accepted(Tenor.ofDays(182)),
      accepted(Tenor.ofMonths(6)),
      accepted(Tenor.ofDays(183)),
      accepted(Tenor.ofDays(365)),
      accepted(Tenor.ofYears(1)),
      accepted(Tenor.ofDays(366)),
      accepted(Tenor.ofDays(730)),
      accepted(Tenor.ofYears(2)),
      accepted(Tenor.ofDays(731)),
      accepted(Tenor.ofDays(1095)),
      accepted(Tenor.ofYears(3)),
      accepted(Tenor.ofDays(1096)),
      accepted(Tenor.ofDays(1460)),
      accepted(Tenor.ofYears(4)),
      accepted(Tenor.ofDays(1461))
    )
    tenors should have size 30

    val ordering = Order[Tenor].toOrdering
    val outOfOrder = tenors.zip(tenors.tail).filterNot {
      case (shorter, longer) => Order[Tenor].compare(shorter, longer) < 0
    }
    outOfOrder shouldBe empty

    // Two fixed permutations - a shuffle with a fixed seed and the reverse of the list, the worst
    // case for any comparison sort - so a sort failure here is reproducible.
    val shuffled = new Random(20140630L).shuffle(tenors)
    shuffled shouldNot be(tenors)
    shuffled.sorted(ordering) shouldBe tenors
    tenors.reverse.sorted(ordering) shouldBe tenors
  }

  //-------------------------------------------------------------------------
  test("test_equals_hashCode") {
    val a1 = TENOR_3D
    val a2 = accepted(Tenor.ofDays(3))
    val b = TENOR_4M
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

    Order[Tenor].eqv(a1, a2) shouldBe true
    Order[Tenor].eqv(a1, b) shouldBe false
    Hash[Tenor].hash(a1) shouldBe Hash[Tenor].hash(a2)

    // `compareTo` ranks twelve months and one year equal although the values are unequal, so
    // `Order` breaks that tie by name and places `12M` before `1Y`
    TENOR_12M.equals(TENOR_1Y) shouldBe false
    Order[Tenor].eqv(TENOR_12M, TENOR_1Y) shouldBe false
    TENOR_12M.compareTo(TENOR_1Y) shouldBe 0
    Order[Tenor].compare(TENOR_12M, TENOR_1Y) should be < 0
    Order[Tenor].compare(TENOR_1Y, TENOR_12M) should be > 0
  }

  test("test_equals_bad") {
    TENOR_3D.equals(null) shouldBe false
    TENOR_3D.equals(ANOTHER_TYPE) shouldBe false
    TENOR_3D.equals(ANOTHER_OBJECT) shouldBe false
  }

  //-------------------------------------------------------------------------
  test("test_toString") {
    // the canonical text of a tenor has no `P` prefix, and months and years are not normalised
    // into each other, so `12M` stays `12M` and `18M` is not `1Y6M`
    TENOR_3D.toString shouldBe "3D"
    TENOR_2W.toString shouldBe "2W"
    TENOR_4M.toString shouldBe "4M"
    TENOR_12M.toString shouldBe "12M"
    TENOR_1Y.toString shouldBe "1Y"
    TENOR_18M.toString shouldBe "18M"
    TENOR_4Y.toString shouldBe "4Y"

    // A tenor's canonical name and rendering drop the `P`, so this is `3M`, where [[Frequency]]
    // renders the same period in the ISO-8601 form `P3M`. `Tenor.parse` accepts both spellings,
    // so the text alone does not say which type is meant - the type being parsed does. Both the
    // name and the rendering are asserted: the library reads the name, a message reads `toString`.
    TENOR_3M.name shouldBe "3M"
    TENOR_3M.toString shouldBe "3M"
  }

  //-----------------------------------------------------------------------
  test("test_serialization") {
    // the JSON form of a tenor is its canonical text as a bare string
    TENOR_3D.asJson shouldBe Json.fromString("3D")
    TENOR_4M.asJson shouldBe Json.fromString("4M")
    TENOR_3Y.asJson shouldBe Json.fromString("3Y")
    // so a document written here holds `3M` rather than the `P3M` that [[Frequency]] writes,
    // although reading accepts either spelling, as the rows below assert
    TENOR_3M.asJson shouldBe Json.fromString("3M")

    Json.fromString("3D").as[Tenor] shouldBe Right(TENOR_3D)
    Json.fromString("4M").as[Tenor] shouldBe Right(TENOR_4M)
    Json.fromString("3Y").as[Tenor] shouldBe Right(TENOR_3Y)
    Json.fromString("3M").as[Tenor] shouldBe Right(TENOR_3M)

    // decoding goes through the same parse the text form uses, so a document holding the ISO-8601
    // spelling is accepted as well; the asymmetry between reading and writing is deliberate
    Json.fromString("P3D").as[Tenor] shouldBe Right(TENOR_3D)
    Json.fromString("P4M").as[Tenor] shouldBe Right(TENOR_4M)
    Json.fromString("P3Y").as[Tenor] shouldBe Right(TENOR_3Y)
    Json.fromString("P3M").as[Tenor] shouldBe Right(TENOR_3M)

    Json.fromString("2K").as[Tenor].isLeft shouldBe true
  }

  test("test_jodaConvert") {
    Show[Tenor].show(TENOR_3D) shouldBe "3D"
    Show[Tenor].show(TENOR_4M) shouldBe "4M"
    Show[Tenor].show(TENOR_3Y) shouldBe "3Y"
    Show[Tenor].show(TENOR_3M) shouldBe "3M"

    Tenor.parse(Show[Tenor].show(TENOR_3D)) should haveValue(TENOR_3D)
    Tenor.parse(Show[Tenor].show(TENOR_4M)) should haveValue(TENOR_4M)
    Tenor.parse(Show[Tenor].show(TENOR_3Y)) should haveValue(TENOR_3Y)
    Tenor.parse(Show[Tenor].show(TENOR_3M)) should haveValue(TENOR_3M)
  }
}
