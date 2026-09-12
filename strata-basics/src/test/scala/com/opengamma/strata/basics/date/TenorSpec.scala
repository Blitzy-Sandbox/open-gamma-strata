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

import cats.Hash
import cats.Order
import cats.Show
import cats.data.NonEmptyChain

import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
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
 * Tests for the tenor class.
 *
 * The Java original held twenty-three annotated methods - fourteen plain tests and nine
 * parameterised ones driven by seven providers - and all twenty-three survive here under
 * their Java names, so that the method-level traceability of the migration stays one to
 * one. Each parameterised method becomes a single test holding its table, which is why
 * `data_based` appears in two tests and `data_parseGood` in two more: the providers are
 * shared exactly as the Java annotations shared them.
 *
 * ===What the port changes, and why===
 *
 * Three shapes of assertion could not be carried over literally, and each is reshaped
 * rather than dropped, so no fact the Java test established is lost.
 *
 *   - '''A rejected input is a value, not an exception.''' The factories of [[Tenor]]
 *     return a failure chain rather than throwing, so the fourteen exception assertions of
 *     the original become assertions that the outcome is a failure carrying an expected
 *     reason. The reason is compared as a value of the closed family of reasons rather than
 *     by matching message text, which is what keeps such an assertion meaningful when a
 *     message is reworded. The single exception that remains an exception is documented
 *     where it is asserted, in `test_temporalAmount`.
 *   - '''`Tenor` is not a `java.time.temporal.TemporalAmount`.''' That interface requires
 *     `getUnits` to return a Java list, which the migration forbids on a public API and the
 *     bytecode audit of the public surface rejects. `test_temporalAmount` therefore asserts
 *     the same five facts through the members that replace it, and the divergence is
 *     recorded in `SCALA_MIGRATION.md`.
 *   - '''Sorting is deterministic.''' The original shuffled its list with an unseeded
 *     shuffle. `test_compare` uses two fixed permutations instead, so a failure is
 *     reproducible rather than intermittent.
 *
 * Two duties the Java class would have covered belong elsewhere in this port and are
 * deliberately not repeated here: `ApiSurfaceSpec` proves that no public `apply` or `copy`
 * exists on this type, and `TypeclassLawsSpec` runs the law suites of its `Order` and
 * `Hash` instances. What is asserted here is the behaviour of the type itself.
 */
class TenorSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * A value of an unrelated type, for the equality contract of `test_equals_bad`.
   *
   * This is the `ANOTHER_TYPE` constant of the Java test, and it is typed as `Any` for the
   * same reason the Java test needed no such care: comparing a tenor with a string through
   * `==` is a compile-time warning in this build, and a warning is an error here. Typing
   * the value as `Any` and calling `equals` asserts what the original asserted - that the
   * equality of a tenor rejects a value of another type - without asking the compiler to
   * approve the comparison.
   */
  private val ANOTHER_TYPE: Any = ""

  /**
   * A plain object, for the equality contract of `test_equals_bad`.
   *
   * This stands in for the `new Object()` of the Java test. It is held in a value rather
   * than built at the point of comparison because a comparison against a freshly built
   * object is another thing this build warns about.
   */
  private val ANOTHER_OBJECT: Any = new AnyRef

  //-------------------------------------------------------------------------
  /**
   * Returns the tenor a factory produced, failing the test if it produced failures instead.
   *
   * Every factory of [[Tenor]] reports a rejected input as a chain of failures rather than
   * by throwing, so a test that means to use the resulting tenor has to say what should
   * happen if there is none. Every call below passes an input the factory is expected to
   * accept, and the answer is therefore that the test fails, naming the messages that came
   * back - which is strictly more informative than the Java original, where an unexpected
   * rejection surfaced as an `IllegalArgumentException` from the line that made the call.
   *
   * The parameter is written as the underlying `Either` rather than through the alias the
   * library publishes for it, because this spec depends on the failure type alone.
   *
   * @param result  the outcome of a factory call that is expected to succeed
   * @return the tenor the factory produced
   */
  private def accepted(result: Either[NonEmptyChain[Failure], Tenor]): Tenor =
    result.fold(
      failures => fail("Expected a tenor but the factory failed with: " +
        failures.toChain.toList.map(_.message).mkString(", ")),
      tenor => tenor)

  //-------------------------------------------------------------------------
  /**
   * The `data_ofPeriod` provider, transcribed row for row.
   *
   * Each row is the period handed to the factory, the period the resulting tenor holds,
   * and the canonical name it takes. The first two columns differ only where the input
   * names the same period in another unit - a period of two weeks is stored as fourteen
   * days - which is what makes the third column the interesting one: seven days is named
   * `1W` while ten days stays `10D`, and twelve months stays `12M` rather than becoming
   * `1Y`, because months are never normalised into years by construction.
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

  /**
   * The `data_ofMonths` provider, transcribed row for row.
   *
   * Each row is a month count, the period the resulting tenor holds and its canonical
   * name. Twenty-four months stays `24M`, which is the same statement about normalisation
   * as the corresponding row of `data_ofPeriod`, reached through the other factory.
   */
  private val data_ofMonths: TableFor3[Int, Period, String] = Table(
    ("months", "stored", "name"),
    (1, Period.ofMonths(1), "1M"),
    (2, Period.ofMonths(2), "2M"),
    (12, Period.ofMonths(12), "12M"),
    (20, Period.ofMonths(20), "20M"),
    (24, Period.ofMonths(24), "24M"),
    (30, Period.ofMonths(30), "30M")
  )

  /**
   * The `data_ofYears` provider, transcribed row for row.
   *
   * Each row is a year count, the period the resulting tenor holds and its canonical name.
   */
  private val data_ofYears: TableFor3[Int, Period, String] = Table(
    ("years", "stored", "name"),
    (1, Period.ofYears(1), "1Y"),
    (2, Period.ofYears(2), "2Y"),
    (3, Period.ofYears(3), "3Y")
  )

  /**
   * The `data_normalized` provider, transcribed row for row.
   *
   * Each row is the period of a tenor and the period its normalised form holds. Three
   * classes of row carry the whole rule:
   *
   *  - days are already canonical, so they are returned unchanged, whether or not the
   *    count is a multiple of seven;
   *  - a year is twelve months by this convention rather than the other way round, so one
   *    year normalises to twelve months while twelve months stays as it is;
   *  - any longer month count carries its excess into years, so twenty months becomes one
   *    year and eight months and twenty-four months becomes two years.
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

  /**
   * The `data_based` provider, transcribed row for row.
   *
   * Each row is a tenor together with whether it is week-based and whether it is
   * month-based, and the two columns are asserted by the two tests that share this table,
   * exactly as the two Java methods shared the provider. The last row is the one that
   * makes both columns necessary: a tenor with months and days is neither.
   *
   * The tenors are built through the factories the Java provider used rather than taken
   * from the constants, so the table exercises the factories on the way in.
   */
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

  /**
   * The `data_parseGood` provider, transcribed row for row.
   *
   * Each row is the canonical text of a tenor and the tenor it names. The table is shared
   * by the two tests that assert the two accepted forms of that text - the canonical form
   * itself and the same text with the ISO-8601 `P` prefix - which is how the Java test
   * used it.
   */
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
   * The `data_parseBad` provider, transcribed row for row and extended with the reason.
   *
   * The Java provider held the rejected text alone, because every row raised the same
   * exception type. Parsing here reports a reason, and the rows do not all report the same
   * one, so the expected reason is part of the table:
   *
   *  - text that names no period at all - the empty string, a bare number, a number with
   *    an unknown unit - is a parsing failure, since there is nothing to interpret;
   *  - `-2D` does name a period, and a valid one as far as `java.time` is concerned. It is
   *    rejected one step later, for being negative, and so reports the reason its rejection
   *    as a tenor carries rather than a parsing reason. Telling a caller that its period is
   *    negative rather than that its text was bad is the point of the distinction.
   */
  private val data_parseBad: TableFor2[String, FailureReason] = Table(
    ("input", "reason"),
    ("", FailureReason.PARSING),
    ("2", FailureReason.PARSING),
    ("2K", FailureReason.PARSING),
    ("-2D", FailureReason.INVALID)
  )

  //-------------------------------------------------------------------------
  test("test_ofPeriod") {
    forAll(data_ofPeriod) { (period: Period, stored: Period, name: String) =>
      val test = accepted(Tenor.of(period))
      test.period shouldBe stored
      // the accessor is `period`, the `getPeriod` of the original having lost its prefix;
      // `toString` is the name, so asserting both pins the rendering to the canonical text
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
    // every one of these is the constant the factory is expected to produce, so the
    // assertion is that a factory call and the corresponding constant are the same value
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
    // the Java original asserted that each of these five calls threw. A zero period is a
    // property of the data handed in rather than a broken call, so it is reported as a
    // failure carrying a reason, and the reason is compared as a value: `INVALID` is what
    // the validation of this library assigns to input it will not accept
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
    // the Java original round-tripped one constant; the property it stood for - that the
    // text a tenor renders itself as parses back to that same tenor - is asserted over a
    // representative spread of them, the original's `TENOR_10M` included, and over a
    // tenor of mixed months and years, which is the shape whose name is not built by
    // appending a unit to a number
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
    // the three-month tenor is not one of the transcribed provider rows, and it is the form
    // users, stored documents and other systems hand to `parse`, so the migration pins it
    // here as well: the text `3M` names this tenor and nothing else
    Tenor.parse("3M") should haveValue(TENOR_3M)
  }

  test("test_parse_String_good_withP") {
    forAll(data_parseGood) { (input: String, expected: Tenor) =>
      // the ISO-8601 form of the same text, which parsing accepts as well; the canonical
      // name deliberately drops that prefix, so both forms name one tenor
      Tenor.parse("P" + input) should haveValue(expected)
    }
    // the same statement for the three-month tenor, which is the case where the prefix
    // matters most: `P3M` is the canonical name of a quarterly frequency elsewhere in this
    // library, and parsing it as a tenor has to yield the three-month tenor all the same
    Tenor.parse("P3M") should haveValue(TENOR_3M)
  }

  test("test_parse_String_bad") {
    forAll(data_parseBad) { (input: String, reason: FailureReason) =>
      Tenor.parse(input) should beFailureWith(reason)
    }
  }

  test("parsing rejects text of any size without echoing it unbounded or across lines") {
    // No Java counterpart: the Java method let the failure of `Period.parse` surface, so it
    // echoed none of the text it was given, while this port reports a failure of its own that
    // quotes the text back. Quoting is useful - the caller is told what could not be read -
    // and it is bounded and escaped, because the message reaches a log or a report and the
    // text reached the library from outside it.
    val payload = "A" * 10000
    val bounded = Tenor.parse(payload)
    bounded should beFailureWith(FailureReason.PARSING)
    // The echo is bounded by the rendering the message is built from, so the message is the
    // fixed text plus at most `MaxDescribedInput + 3` characters of the input, whatever its
    // size - where it was once the whole ten thousand.
    val message = bounded.left.toOption.map(_.message).getOrElse("")
    message.length should be <= "Unable to parse tenor: ''".length + Failure.MaxDescribedInput + 3
    message should startWith("Unable to parse tenor:")

    // A payload holding a line break cannot put one in the message, so a line-oriented
    // consumer of the message cannot be made to record a line the library did not report.
    val injected = Tenor.parse("3M\nINJECTED")
    injected should beFailureWith(FailureReason.PARSING)
    val injectedMessage = injected.left.toOption.map(_.message).getOrElse("")
    injectedMessage should not include "\n"
    injectedMessage shouldBe "Unable to parse tenor: '3M\\nINJECTED'"

    // And the message for an ordinary rejected input is unchanged, character for character,
    // which is what makes the bound invisible to every caller but the adversarial one.
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
    // the third column of the shared table is asserted by the sibling test below, exactly
    // as the two Java methods each asserted one column of the one provider
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
    // a temporal object that is not a date goes through the general overload, which is why
    // this row is kept: it is the only assertion that exercises it
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
    // DIVERGENCE: `Tenor` does not extend `java.time.temporal.TemporalAmount`. That
    // interface requires `getUnits` to return a list from the Java collections framework,
    // which would put a Java collection on the public API of this port - forbidden by the
    // migration and rejected by the audit of the public bytecode surface - so the three
    // members callers actually use survive as ordinary methods instead: `units`, `get` and
    // `addTo`/`subtractFrom`.
    // The divergence is recorded in `SCALA_MIGRATION.md`. Every fact the Java test
    // established is asserted below, through those members.

    // the Java test asserted `getUnits()` contained exactly years, months and days; the
    // replacement member returns the same three units, in the same order, as a Scala list
    TENOR_3D.units shouldBe
      List[TemporalUnit](ChronoUnit.YEARS, ChronoUnit.MONTHS, ChronoUnit.DAYS)
    // and the same information is visible through `get`, which answers for each of them -
    // the values are `Long`, as the interface being replaced declared them
    TENOR_3D.get(ChronoUnit.YEARS) shouldBe 0L
    TENOR_3D.get(ChronoUnit.MONTHS) shouldBe 0L
    TENOR_3D.get(ChronoUnit.DAYS) shouldBe 3L

    // `date.plus(tenor)` and `date.minus(tenor)` were reachable only because the type
    // implemented the interface; the arithmetic they performed is `addTo` and
    // `subtractFrom`, which return a `LocalDate` here rather than a `Temporal`
    TENOR_1W.addTo(LocalDate.of(2014, 6, 30)) shouldBe LocalDate.of(2014, 7, 7)
    TENOR_1W.subtractFrom(LocalDate.of(2014, 6, 30)) shouldBe LocalDate.of(2014, 6, 23)

    // the one exception assertion this file keeps. Querying a unit a tenor is not measured
    // in is not a failure of the data: the query is answered by the underlying
    // `java.time.Period`, which rejects anything but years, months and days by raising
    // this `java.time` exception, and reproducing that behaviour means propagating it.
    intercept[UnsupportedTemporalTypeException] {
      TENOR_10M.get(ChronoUnit.CENTURIES)
    }
  }

  //-------------------------------------------------------------------------
  test("test_compare") {
    // the list of the Java test, in its order, built through the factories it used
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

    // The expected order is the Java order, unaltered. The ordering of this port is the
    // comparison of the original followed by a tie-break on the name, and the tie-break is
    // consulted only where that comparison returns zero - which it does for no pair in
    // this list, every tenor here having a distinct length or estimated length. So the
    // divergence recorded in `SCALA_MIGRATION.md` moves nothing here, and the pair it does
    // move - `12M` against `1Y` - is asserted in `test_equals_hashCode` below, where it
    // belongs. The adjacency check states that expectation directly, independently of the
    // sort: each tenor is strictly shorter than the one after it.
    val ordering = Order[Tenor].toOrdering
    val outOfOrder = tenors.zip(tenors.tail).filterNot {
      case (shorter, longer) => Order[Tenor].compare(shorter, longer) < 0
    }
    outOfOrder shouldBe empty

    // the Java test shuffled with an unseeded shuffle, which makes a failure irreproducible.
    // Two fixed permutations are used instead: a shuffle with a fixed seed, and the reverse
    // of the list, which is the worst case for any comparison sort.
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

    // the same statements through the instance that carries equality for this port, which
    // is where the rest of the library reads it from
    Order[Tenor].eqv(a1, a2) shouldBe true
    Order[Tenor].eqv(a1, b) shouldBe false
    Hash[Tenor].hash(a1) shouldBe Hash[Tenor].hash(a2)

    // DIVERGENCE: twelve months and one year. The original ranks them equal while holding
    // them unequal, which no `cats.Order` may do - its laws require `compare` to return
    // zero exactly when the values are equal - so the ordering of this port breaks that tie
    // by name, placing `12M` immediately before `1Y`. The unrefined comparison of the
    // original remains available as `compareTo` and still ranks them equal, as asserted
    // here. Recorded in `SCALA_MIGRATION.md`.
    TENOR_12M.equals(TENOR_1Y) shouldBe false
    Order[Tenor].eqv(TENOR_12M, TENOR_1Y) shouldBe false
    TENOR_12M.compareTo(TENOR_1Y) shouldBe 0
    Order[Tenor].compare(TENOR_12M, TENOR_1Y) should be < 0
    Order[Tenor].compare(TENOR_1Y, TENOR_12M) should be > 0
  }

  test("test_equals_bad") {
    // `equals` is called as a method, on values typed as `Any`, because this build rejects
    // a `==` between unrelated types as an error; the assertions are those of the original
    TENOR_3D.equals(null) shouldBe false
    TENOR_3D.equals(ANOTHER_TYPE) shouldBe false
    TENOR_3D.equals(ANOTHER_OBJECT) shouldBe false
  }

  //-------------------------------------------------------------------------
  test("test_toString") {
    // the canonical text of a tenor has no `P` prefix, which is the form users, stored
    // documents and other systems know a tenor by; all seven cases of the original are
    // asserted, including the two that pin the absence of normalisation between months and
    // years - `12M` stays `12M`, and `18M` is not `1Y6M` - and they are followed by the
    // three-month form this migration pins as the canonical contract
    TENOR_3D.toString shouldBe "3D"
    TENOR_2W.toString shouldBe "2W"
    TENOR_4M.toString shouldBe "4M"
    TENOR_12M.toString shouldBe "12M"
    TENOR_1Y.toString shouldBe "1Y"
    TENOR_18M.toString shouldBe "18M"
    TENOR_4Y.toString shouldBe "4Y"

    // the three-month tenor is the identity most quoted rates, stored documents and other
    // systems carry, and it is the one name where the prefix changes what is named rather
    // than merely how it is spelled: `3M` is this tenor, while `P3M` names a quarterly
    // frequency. Both the name and the rendering are asserted, because the rest of the
    // library reads the name while a message or a log reads `toString`, and the two are
    // one string that must not drift apart
    TENOR_3M.name shouldBe "3M"
    TENOR_3M.toString shouldBe "3M"
  }

  //-----------------------------------------------------------------------
  test("test_serialization") {
    // Java serialization is not supported by this port; the JSON codec is the serialized
    // form, and it writes the canonical text as a bare string, so the three constants the
    // original round-tripped are round-tripped through it instead
    TENOR_3D.asJson shouldBe Json.fromString("3D")
    TENOR_4M.asJson shouldBe Json.fromString("4M")
    TENOR_3Y.asJson shouldBe Json.fromString("3Y")
    // the three-month tenor is written in the canonical form too, so a document this port
    // writes holds the `3M` that other systems read, never the `P3M` of a frequency
    TENOR_3M.asJson shouldBe Json.fromString("3M")

    Json.fromString("3D").as[Tenor] shouldBe Right(TENOR_3D)
    Json.fromString("4M").as[Tenor] shouldBe Right(TENOR_4M)
    Json.fromString("3Y").as[Tenor] shouldBe Right(TENOR_3Y)
    Json.fromString("3M").as[Tenor] shouldBe Right(TENOR_3M)

    // reading goes through the same parsing the text form uses, so a document holding the
    // ISO-8601 form is accepted as well - part of the contract of the codec, and the
    // reason the asymmetry between reading and writing is deliberate
    Json.fromString("P3D").as[Tenor] shouldBe Right(TENOR_3D)
    Json.fromString("P4M").as[Tenor] shouldBe Right(TENOR_4M)
    Json.fromString("P3Y").as[Tenor] shouldBe Right(TENOR_3Y)
    Json.fromString("P3M").as[Tenor] shouldBe Right(TENOR_3M)

    // and text that names no tenor is rejected rather than decoded into one
    Json.fromString("2K").as[Tenor].isLeft shouldBe true
  }

  test("test_jodaConvert") {
    // annotation-driven string conversion is not supported by this port either. Its two
    // halves are the rendering instance and the parsing factory, so the round trip the
    // original asserted through that mechanism is asserted through them.
    Show[Tenor].show(TENOR_3D) shouldBe "3D"
    Show[Tenor].show(TENOR_4M) shouldBe "4M"
    Show[Tenor].show(TENOR_3Y) shouldBe "3Y"
    // the three-month tenor renders as the name it is known by rather than as the ISO-8601
    // form, which is the same contract the codec and `toString` carry
    Show[Tenor].show(TENOR_3M) shouldBe "3M"

    Tenor.parse(Show[Tenor].show(TENOR_3D)) should haveValue(TENOR_3D)
    Tenor.parse(Show[Tenor].show(TENOR_4M)) should haveValue(TENOR_4M)
    Tenor.parse(Show[Tenor].show(TENOR_3Y)) should haveValue(TENOR_3Y)
    Tenor.parse(Show[Tenor].show(TENOR_3M)) should haveValue(TENOR_3M)
  }

  //-------------------------------------------------------------------------
  // Mapping from the Java test class, for the record: all twenty-three annotated methods
  // are represented above under their Java names, and none is dropped, so the method-level
  // traceability of the migration stays one to one. The twenty-fourth case above,
  // "parsing rejects text of any size without echoing it unbounded or across lines", has no
  // Java counterpart and is named descriptively for that reason: it states how this port
  // quotes rejected text - bounded and on one line - where the Java method quoted none of it.
  //
  // One of the twenty-three is nevertheless consolidated in the mapping file, and the two
  // facts are separate. `java-test-mapping.csv` records `TenorTest.test_serialization`
  // against `com.opengamma.strata.basics.json.JsonRoundTripSpec` as
  // `Tenor_test_serialization`, designating that spec the owner of the property-based codec
  // round trips of this port; the `test_serialization` above is additional coverage of the
  // same codec, stating the exact encoded text of particular tenors, rather than the mapped
  // row itself.
  //
  // Two methods are reshaped where the port's design makes the Java form unstatable, and
  // both keep every fact they asserted: `test_temporalAmount`, because this type is not a
  // `TemporalAmount`, and `test_compare`, because its permutation is fixed rather than
  // random. The fourteen exception assertions of the original become failure assertions,
  // save the one `java.time` exception documented in `test_temporalAmount`.
}
