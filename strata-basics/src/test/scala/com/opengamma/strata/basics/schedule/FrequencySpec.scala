/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.schedule

import java.time.LocalDate
import java.time.Period

import cats.Eq
import cats.Hash
import cats.Order
import cats.Show

import io.circe.Codec
import io.circe.Json

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableFor1
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableFor3
import org.scalatest.prop.TableFor4
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[Frequency]].
 *
 * This is a one-to-one port of the Java test class: each of its thirty-one test methods has a
 * test of the same name here, in the same order, and every one of its nine data providers is
 * transcribed row for row rather than re-derived. A provider that drove several Java methods
 * becomes a single shared table driving the same several tests, so a method that was
 * parameterised over twenty-eight rows remains one test - the row loop moves inside the test
 * rather than multiplying it - and the method-level traceability of the migration is exact.
 *
 * ===What the port changes, and why===
 *
 * The type under test reports a rejection as a value rather than by throwing, so every
 * assertion that the Java test wrote as "this call throws `IllegalArgumentException`" is
 * written here as an assertion about the failure on the left of an `Either`, through the
 * matchers of the shared testkit. That also makes the reason of each failure assertable, which
 * the exception-based assertions could not express, so the tests below state it: every
 * rejection by a factory, by `eventsPerYear` and by `exactDivide` carries
 * `FailureReason.INVALID`, while text that names no period at all is a
 * `FailureReason.PARSING` failure.
 *
 * Because a factory hands back an `Either`, the data providers cannot hold the frequencies
 * they held in Java without unwrapping them first; [[freq]] does that, failing the suite if a
 * row that is supposed to describe a valid frequency does not. The rows themselves are
 * therefore identical to the Java rows, which is the point of the helper.
 *
 * ===The rows that are not identical, and why===
 *
 * The type under test is a normalising type: its construction reduces a period to the canonical
 * form of its length, so a frequency of 12 months and a frequency of 1 year are one value here
 * where Java had two, and a month count above twelve is held - and named - in years and months.
 * Ten rows across four of the Java providers - six distinct lengths, some of them asserted by
 * more than one provider - stated the un-normalised outcome and now state the canonical one
 * instead: the 18-, 24-, 30-month and one-year rows of `data_create`, the 20-, 24- and
 * 30-month rows of `data_ofMonths`, the one-year row of `data_ofYears`, and the two rows of
 * `data_normalized` that converted between the two spellings of a year. Each is marked where it
 * appears. Every other row, including every constant, every day- and week-based row and every
 * events-per-year and division row, is the Java row unchanged - canonicalisation moves no
 * length, so it moves no arithmetic. `test_normalized` carries the assertions that pin the new
 * contract: that the five construction paths to a year reach one value, that construction is
 * idempotent, and that two spellings of one length are one frequency.
 *
 * Four Java assertions have no direct counterpart and are recorded at the test that carries
 * them: the `TemporalAmount` interface the ported type deliberately does not implement
 * (`test_temporalAmount`, `test_addTo`, `test_subtractFrom`), Java serialization
 * (`test_serialization`), Joda-Convert (`test_jodaConvert`) and the `null` input row of
 * `data_parseBad` (`test_parse_String_bad`). Each of those tests keeps its Java name and
 * asserts the behaviour that replaced what was dropped, so nothing is silently lost.
 *
 * Numerical parity with the Java implementation to 1e-9 is not this spec's job - the schedule
 * parity spec discharges that against the captured Java baseline. What this spec keeps are the
 * tolerances the Java test itself used: 1e-8 everywhere except the three mixed month-and-day
 * estimates, which the Java test compared at 1e-3.
 */
final class FrequencySpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  /**
   * The number of draws the one generated property of this file is checked against: the
   * idempotence and length-preservation property inside `test_normalized`.
   *
   * The default of the framework is a handful, which is far too few for what that property
   * asserts. It draws a month count from 1 to 12,000 - the whole range the factories of this
   * type admit - against a day count from 0 to 400, some 4.8 million pairs, and the
   * canonicalisation corners the claim rests on sit at three single values of that range, each
   * about a ten-thousandth of a uniform draw. Five hundred draws, together with those three
   * month counts named as generator specials at the property itself, reach every corner many
   * times over and spread the remaining draws across the range, while leaving this file inside
   * the few seconds it runs in - the property builds four frequencies per draw and nothing else.
   *
   * The count governs generator-driven checks only, so the table-driven `forAll(data_...)` tests
   * below are unaffected by it: each of those evaluates every row of its table, always.
   */
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 500)

  /**
   * Unwraps the outcome of a factory that is expected to produce a frequency.
   *
   * The data providers of the Java test held frequencies built by factories that could throw;
   * here those factories return their failures instead, so a table row has to unwrap one to
   * hold a frequency. Doing that through this helper rather than with `getOrElse` and a
   * fabricated fallback keeps a transcription mistake visible: a row naming a period that is
   * not a frequency fails the suite, naming the row's failures, instead of quietly testing
   * some other value.
   *
   * @param result  the outcome of a factory, expected to hold a frequency
   * @return the frequency the outcome holds
   */
  private def freq(result: ResultNec[Frequency]): Frequency =
    result.fold(
      failures => fail(s"unexpected failure: ${failures.toChain.toList.map(_.message).mkString("; ")}"),
      frequency => frequency)

  //-------------------------------------------------------------------------
  /**
   * The provider shared by `test_of_int`, `test_of_Period` and `test_parse`, transcribed row
   * for row from the Java `data_create`.
   *
   * Each row is a frequency, the period it is expected to hold and the text it is expected to
   * render as - which is also the text `parse` reads back. The rows built by `ofDays` and
   * `ofWeeks` are the ones that pin the days-to-weeks naming: seven days is a week and ninety-one
   * days is thirteen weeks, so those rows expect the week-named text against a period still
   * measured in days.
   *
   * Four rows state the canonical form of their length where the Java rows stated the period
   * they were handed: eighteen, twenty-four and thirty months are held as years and months, and
   * one year is held as twelve months - the canonical form of that length, so the row is the
   * `P12M` row twice over, once through each factory that reaches it.
   */
  private val data_create: TableFor3[Frequency, Period, String] = Table(
    ("frequency", "period", "text"),
    (freq(Frequency.ofDays(1)), Period.ofDays(1), "P1D"),
    (freq(Frequency.ofDays(2)), Period.ofDays(2), "P2D"),
    (freq(Frequency.ofDays(6)), Period.ofDays(6), "P6D"),
    (freq(Frequency.ofDays(7)), Period.ofDays(7), "P1W"),
    (freq(Frequency.ofDays(91)), Period.ofDays(91), "P13W"),
    (freq(Frequency.ofWeeks(1)), Period.ofDays(7), "P1W"),
    (freq(Frequency.ofWeeks(3)), Period.ofDays(21), "P3W"),
    (freq(Frequency.ofMonths(8)), Period.ofMonths(8), "P8M"),
    (freq(Frequency.ofMonths(12)), Period.ofMonths(12), "P12M"),
    // canonical rows: Java held P18M, P24M, P30M and P1Y here
    (freq(Frequency.ofMonths(18)), Period.of(1, 6, 0), "P1Y6M"),
    (freq(Frequency.ofMonths(24)), Period.ofYears(2), "P2Y"),
    (freq(Frequency.ofMonths(30)), Period.of(2, 6, 0), "P2Y6M"),
    (freq(Frequency.ofYears(1)), Period.ofMonths(12), "P12M"),
    (freq(Frequency.ofYears(2)), Period.ofYears(2), "P2Y"),
    (freq(Frequency.of(Period.of(1, 2, 3))), Period.of(1, 2, 3), "P1Y2M3D"),
    (Frequency.P1D, Period.ofDays(1), "P1D"),
    (Frequency.P1W, Period.ofWeeks(1), "P1W"),
    (Frequency.P2W, Period.ofWeeks(2), "P2W"),
    (Frequency.P4W, Period.ofWeeks(4), "P4W"),
    (Frequency.P13W, Period.ofWeeks(13), "P13W"),
    (Frequency.P26W, Period.ofWeeks(26), "P26W"),
    (Frequency.P52W, Period.ofWeeks(52), "P52W"),
    (Frequency.P1M, Period.ofMonths(1), "P1M"),
    (Frequency.P2M, Period.ofMonths(2), "P2M"),
    (Frequency.P3M, Period.ofMonths(3), "P3M"),
    (Frequency.P4M, Period.ofMonths(4), "P4M"),
    (Frequency.P6M, Period.ofMonths(6), "P6M"),
    (Frequency.P12M, Period.ofMonths(12), "P12M")
  )

  /**
   * The provider of `test_ofMonths`, transcribed from the Java `data_ofMonths`.
   *
   * Each row is a number of months, the period the factory is expected to hold for it and the
   * text it renders as. Months beyond twelve are redistributed into years and months, which is
   * what the twenty-, twenty-four- and thirty-month rows assert - the Java rows expected `P20M`,
   * `P24M` and `P30M` there, the periods those factories were handed. Twelve months is the one
   * length whose canonical form is months, so that row is the Java row unchanged.
   */
  private val data_ofMonths: TableFor3[Int, Period, String] = Table(
    ("months", "period", "text"),
    (1, Period.ofMonths(1), "P1M"),
    (2, Period.ofMonths(2), "P2M"),
    (3, Period.ofMonths(3), "P3M"),
    (4, Period.ofMonths(4), "P4M"),
    (6, Period.ofMonths(6), "P6M"),
    (12, Period.ofMonths(12), "P12M"),
    // canonical rows: Java held P20M, P24M and P30M here
    (20, Period.of(1, 8, 0), "P1Y8M"),
    (24, Period.ofYears(2), "P2Y"),
    (30, Period.of(2, 6, 0), "P2Y6M")
  )

  /**
   * The provider of `test_ofYears`, transcribed from the Java `data_ofYears`.
   *
   * Each row is a number of years, the period the factory holds for it and its text. Two years
   * and three years are held as years, as the Java rows expected. One year is not: the canonical
   * form of that length is twelve months, so the factory yields `Frequency.P12M`, where the Java
   * row expected a distinct value named `P1Y`.
   */
  private val data_ofYears: TableFor3[Int, Period, String] = Table(
    ("years", "period", "text"),
    // canonical row: Java held P1Y here
    (1, Period.ofMonths(12), "P12M"),
    (2, Period.ofYears(2), "P2Y"),
    (3, Period.ofYears(3), "P3Y")
  )

  /**
   * The provider of `test_normalized`, transcribed from the Java `data_normalized`.
   *
   * Each row is the period a frequency is built from and the canonical period the frequency
   * holds - which, because construction canonicalises, is both the period of the value and the
   * period of its normalisation. The first four rows are the day-based and week-based cases,
   * which canonicalisation leaves exactly as they are; the rest redistribute months into years
   * and months, so thirty months becomes two years and six months.
   *
   * The two rows for a year are where this port departs from the Java provider. There, twelve
   * months and one year were distinct frequencies and `normalized()` mapped both to the one-year
   * value; here they are a single value whose canonical period is twelve months, so both rows
   * expect `P12M`.
   */
  private val data_normalized: TableFor2[Period, Period] = Table(
    ("period", "canonical"),
    (Period.ofDays(1), Period.ofDays(1)),
    (Period.ofDays(7), Period.ofDays(7)),
    (Period.ofDays(10), Period.ofDays(10)),
    (Period.ofWeeks(2), Period.ofDays(14)),
    (Period.ofMonths(1), Period.ofMonths(1)),
    (Period.ofMonths(2), Period.ofMonths(2)),
    // canonical rows: Java expected P1Y for both of these
    (Period.ofMonths(12), Period.ofMonths(12)),
    (Period.ofYears(1), Period.ofMonths(12)),
    (Period.ofMonths(20), Period.of(1, 8, 0)),
    (Period.ofMonths(24), Period.ofYears(2)),
    (Period.ofYears(2), Period.ofYears(2)),
    (Period.ofMonths(30), Period.of(2, 6, 0))
  )

  /**
   * The provider shared by `test_isWeekBased`, `test_isMonthBased` and `test_isAnnual`,
   * transcribed from the Java `data_based`.
   *
   * Each row is a frequency and the three answers it gives. The rows that matter most are the
   * last two: a frequency mixing years, months and days is neither week-based nor month-based,
   * and the term frequency is neither of those and not annual either, even though its period is
   * a whole number of years.
   */
  private val data_based: TableFor4[Frequency, Boolean, Boolean, Boolean] = Table(
    ("frequency", "weekBased", "monthBased", "annual"),
    (freq(Frequency.ofDays(1)), false, false, false),
    (freq(Frequency.ofDays(2)), false, false, false),
    (freq(Frequency.ofDays(6)), false, false, false),
    (freq(Frequency.ofDays(7)), true, false, false),
    (freq(Frequency.ofWeeks(1)), true, false, false),
    (freq(Frequency.ofWeeks(3)), true, false, false),
    (freq(Frequency.ofMonths(1)), false, true, false),
    (freq(Frequency.ofMonths(3)), false, true, false),
    (freq(Frequency.ofMonths(12)), false, true, true),
    (freq(Frequency.ofYears(1)), false, true, true),
    (freq(Frequency.ofYears(3)), false, true, false),
    (freq(Frequency.of(Period.of(1, 2, 3))), false, false, false),
    (Frequency.TERM, false, false, false)
  )

  /**
   * The provider shared by `test_eventsPerYear` and `test_eventsPerYearEstimate`, transcribed
   * from the Java `data_events`.
   *
   * Each row is one of the fourteen constants and the number of events per year it reports.
   * Every constant has an exact count - which is what makes the constants the constants - and
   * for all of them the estimate agrees with it, term included, whose count is zero.
   */
  private val data_events: TableFor2[Frequency, Int] = Table(
    ("frequency", "eventsPerYear"),
    (Frequency.P1D, 364),
    (Frequency.P1W, 52),
    (Frequency.P2W, 26),
    (Frequency.P4W, 13),
    (Frequency.P13W, 4),
    (Frequency.P26W, 2),
    (Frequency.P52W, 1),
    (Frequency.P1M, 12),
    (Frequency.P2M, 6),
    (Frequency.P3M, 4),
    (Frequency.P4M, 3),
    (Frequency.P6M, 2),
    (Frequency.P12M, 1),
    (Frequency.TERM, 0)
  )

  /**
   * The provider shared by `test_exactDivide` and `test_exactDivide_reverse`, transcribed from
   * the Java `data_exactDivide`.
   *
   * Each row is a frequency, the frequency it is divided by and the exact quotient. The Java
   * blocks are kept: day-based into day-based, week-based into week-based and day-based, then
   * month-based and year-based into month-based. The two kinds never mix, which is what the
   * reverse test and `test_exactDivide_bad` assert from the other side.
   */
  private val data_exactDivide: TableFor3[Frequency, Frequency, Int] = Table(
    ("frequency", "other", "expected"),
    (Frequency.P1D, Frequency.P1D, 1),
    (Frequency.P1W, Frequency.P1D, 7),
    (Frequency.P2W, Frequency.P1D, 14),
    (Frequency.P1W, Frequency.P1W, 1),
    (Frequency.P2W, Frequency.P1W, 2),
    (freq(Frequency.ofWeeks(3)), Frequency.P1W, 3),
    (Frequency.P4W, Frequency.P1W, 4),
    (Frequency.P13W, Frequency.P1W, 13),
    (Frequency.P26W, Frequency.P1W, 26),
    (Frequency.P26W, Frequency.P2W, 13),
    (Frequency.P52W, Frequency.P1W, 52),
    (Frequency.P52W, Frequency.P2W, 26),
    (Frequency.P1M, Frequency.P1M, 1),
    (Frequency.P2M, Frequency.P1M, 2),
    (Frequency.P3M, Frequency.P1M, 3),
    (Frequency.P4M, Frequency.P1M, 4),
    (Frequency.P6M, Frequency.P1M, 6),
    (Frequency.P6M, Frequency.P2M, 3),
    (Frequency.P12M, Frequency.P1M, 12),
    (Frequency.P12M, Frequency.P2M, 6),
    (freq(Frequency.ofYears(1)), Frequency.P6M, 2),
    (freq(Frequency.ofYears(1)), Frequency.P3M, 4),
    (freq(Frequency.ofYears(2)), Frequency.P6M, 4)
  )

  /**
   * The provider shared by `test_parse_String_good_noP` and `test_parse_String_good_withP`,
   * transcribed from the Java `data_parseGood`.
   *
   * Each row is text without the ISO-8601 prefix and the frequency it names. The second test
   * prefixes each one with `P`, so the eight rows cover both spellings of every case.
   */
  private val data_parseGood: TableFor2[String, Frequency] = Table(
    ("text", "expected"),
    ("1D", freq(Frequency.ofDays(1))),
    ("2D", freq(Frequency.ofDays(2))),
    ("91D", freq(Frequency.ofDays(91))),
    ("2W", freq(Frequency.ofWeeks(2))),
    ("6W", freq(Frequency.ofWeeks(6))),
    ("2M", freq(Frequency.ofMonths(2))),
    ("12M", freq(Frequency.ofMonths(12))),
    ("1Y", freq(Frequency.ofYears(1)))
  )

  /**
   * The provider of `test_parse_String_bad`, transcribed from the Java `data_parseBad` with the
   * reason each rejection carries added as a second column.
   *
   * The Java provider had a sixth row holding `null`, which this port has no counterpart for:
   * an absent value is not spelled `null` anywhere in this codebase, so there is no call to
   * make. The five remaining rows are the Java rows, and they divide into the two failures the
   * port distinguishes and the Java assertion could not: text that names no period at all is a
   * parsing failure, while `-2D` names a period perfectly well and is then rejected for being
   * negative, which is an invalid-argument failure.
   */
  private val data_parseBad: TableFor2[String, FailureReason] = Table(
    ("text", "reason"),
    ("", FailureReason.PARSING),
    ("2", FailureReason.PARSING),
    ("2K", FailureReason.PARSING),
    ("-2D", FailureReason.INVALID),
    ("PTerm", FailureReason.PARSING)
  )

  //-------------------------------------------------------------------------
  test("test_of_int") {
    forAll(data_create) { (frequency: Frequency, period: Period, text: String) =>
      frequency.period shouldBe period
      frequency.toString shouldBe text
      frequency.isTerm shouldBe false
    }
  }

  test("test_of_Period") {
    forAll(data_create) { (frequency: Frequency, period: Period, _: String) =>
      val created: ResultNec[Frequency] = Frequency.of(period)
      created should haveValue(frequency)
      created.map(_.period) should haveValue(period)
    }
  }

  test("test_parse") {
    forAll(data_create) { (frequency: Frequency, period: Period, text: String) =>
      val parsed = Frequency.parse(text)
      parsed should haveValue(frequency)
      parsed.map(_.period) should haveValue(period)
    }
  }

  //-------------------------------------------------------------------------
  test("test_term") {
    Frequency.TERM.period shouldBe Period.ofYears(10000)
    Frequency.TERM.isTerm shouldBe true
    Frequency.TERM.toString shouldBe "Term"
    // the four spellings the Java `parse` accepted for the term frequency
    val spellings: TableFor1[String] = Table("text", "Term", "0T", "1T", "T")
    forAll(spellings) { (text: String) =>
      Frequency.parse(text) should haveValue(Frequency.TERM)
    }
  }

  //-------------------------------------------------------------------------
  // Where the Java test asserted that each of these calls throws, the ported factories
  // return the failure instead, so the outcome is asserted rather than an exception: an
  // argument that cannot be a frequency is an invalid-argument failure, and every one of
  // these rejections carries the same message, which is asserted with it.
  test("test_of_notZero") {
    val zeroLength: TableFor1[ResultNec[Frequency]] = Table(
      "outcome",
      Frequency.of(Period.ofDays(0)),
      Frequency.ofDays(0),
      Frequency.ofWeeks(0),
      Frequency.ofMonths(0),
      Frequency.ofYears(0)
    )
    forAll(zeroLength) { (outcome: ResultNec[Frequency]) =>
      outcome should beFailureWith(FailureReason.INVALID)
      outcome should haveFailureMessageMatching("Frequency period must not be zero")
    }
  }

  test("test_of_notNegative") {
    val negative: TableFor1[ResultNec[Frequency]] = Table(
      "outcome",
      Frequency.of(Period.ofDays(-1)),
      Frequency.of(Period.ofMonths(-1)),
      Frequency.of(Period.of(0, -1, -1)),
      Frequency.of(Period.of(0, -1, 1)),
      Frequency.of(Period.of(0, 1, -1)),
      Frequency.ofDays(-1),
      Frequency.ofWeeks(-1),
      Frequency.ofMonths(-1),
      Frequency.ofYears(-1)
    )
    forAll(negative) { (outcome: ResultNec[Frequency]) =>
      outcome should beFailureWith(FailureReason.INVALID)
      outcome should haveFailureMessageMatching("Frequency period must not be negative")
    }
  }

  test("test_of_tooBig") {
    val tooBig: TableFor1[ResultNec[Frequency]] = Table(
      "outcome",
      Frequency.of(Period.ofMonths(12001)),
      Frequency.of(Period.ofMonths(Int.MaxValue)),
      Frequency.of(Period.ofYears(1001)),
      Frequency.of(Period.ofYears(Int.MaxValue)),
      Frequency.ofMonths(12001),
      Frequency.ofMonths(Int.MaxValue),
      Frequency.ofYears(1001),
      Frequency.ofYears(Int.MaxValue),
      Frequency.of(Period.of(10000, 0, 1))
    )
    forAll(tooBig) { (outcome: ResultNec[Frequency]) =>
      outcome should beFailureWith(FailureReason.INVALID)
    }
    // the two messages the Java test asserted verbatim, which the port words identically
    Frequency.ofMonths(12001) should haveFailureMessageMatching("Months must not exceed 12,000")
    Frequency.ofYears(1001) should haveFailureMessageMatching("Years must not exceed 1,000")
  }

  //-------------------------------------------------------------------------
  test("test_ofMonths") {
    forAll(data_ofMonths) { (months: Int, period: Period, text: String) =>
      val created: ResultNec[Frequency] = Frequency.ofMonths(months)
      created.map(_.period) should haveValue(period)
      created.map(_.toString) should haveValue(text)
    }
  }

  test("test_ofYears") {
    forAll(data_ofYears) { (years: Int, period: Period, text: String) =>
      val created: ResultNec[Frequency] = Frequency.ofYears(years)
      created.map(_.period) should haveValue(period)
      created.map(_.toString) should haveValue(text)
    }
  }

  //-------------------------------------------------------------------------
  test("test_normalized") {
    // Canonicalisation happens during construction, so each row is read twice: the period the
    // value holds and the period of its normalisation are the same canonical period, and
    // `normalized` hands back the value itself.
    forAll(data_normalized) { (period: Period, canonical: Period) =>
      val created: ResultNec[Frequency] = Frequency.of(period)
      created.map(_.period) should haveValue(canonical)
      created.map(_.normalized.period) should haveValue(canonical)
      created.map(_.normalized) shouldBe created
    }

    // Every public path to a length of one year reaches one value, and everything derived from
    // that value agrees: its name, its hash, its position in the ordering and its JSON. This is
    // the heart of the normalising contract - the Java type had two unequal frequencies of this
    // length, which made the choice of factory observable in equality, in sorted collections and
    // in serialized documents.
    val frequencyCodec: Codec[Frequency] = implicitly[Codec[Frequency]]
    val annualPaths: TableFor1[ResultNec[Frequency]] = Table(
      "outcome",
      Frequency.ofMonths(12),
      Frequency.ofYears(1),
      Frequency.of(Period.ofMonths(12)),
      Frequency.of(Period.ofYears(1)),
      Frequency.of(Period.of(1, 0, 0))
    )
    forAll(annualPaths) { (outcome: ResultNec[Frequency]) =>
      outcome should haveValue(Frequency.P12M)
      // the constant itself, not a value equal to it, as the companion documents
      outcome.exists(_ eq Frequency.P12M) shouldBe true
      outcome.map(_.name) should haveValue("P12M")
      outcome.map(_.hashCode) should haveValue(Frequency.P12M.hashCode)
      outcome.map(frequency => Order[Frequency].compare(frequency, Frequency.P12M)) should haveValue(0)
      outcome.map(frequency => frequencyCodec(frequency)) should haveValue(Json.fromString("P12M"))
    }
    // and the same length spelled as text, with and without the prefix, in either unit
    forAll(Table("text", "P1Y", "1Y", "P12M", "12M")) { (text: String) =>
      Frequency.parse(text) should haveValue(Frequency.P12M)
    }

    // Construction is idempotent and length-preserving beyond the rows above: rebuilding a
    // frequency from the period it holds gives the same value back, normalising it changes
    // nothing, and the two ways of spelling one length - all months, or years and months - are
    // one frequency. Neither the total number of months nor the number of days moves, which is
    // why no arithmetic of this type is affected by canonicalisation.
    //
    // The claim is measured over the five hundred draws configured at the head of this file,
    // taken from month counts 1 to 12,000 - the whole range the factories admit - against day
    // counts 0 to 400, and not over every pair of that space, which is why three month counts
    // are named as generator specials rather than left to chance: twelve, the one length whose
    // canonical form is twelve months rather than one year; thirteen, the first count
    // redistributed into years and months; and twenty-four, an exact number of years. Under a
    // uniform draw each would appear about once in ten thousand, so the corners this property
    // exists to pin would go unvisited; named as specials they are drawn with the same weight
    // as the bounds of the range. The day count needs no special of its own - `chooseNum`
    // already weights zero, one and both bounds, and a day count only ever passes through
    // canonicalisation unchanged.
    forAll(Gen.chooseNum(1, 12000, 12, 13, 24), Gen.chooseNum(0, 400)) { (months: Int, days: Int) =>
      val frequency = freq(Frequency.of(Period.of(0, months, days)))
      Frequency.of(Period.of(months / 12, months % 12, days)) should haveValue(frequency)
      Frequency.of(frequency.period) should haveValue(frequency)
      frequency.normalized shouldBe frequency
      frequency.period.toTotalMonths shouldBe months.toLong
      frequency.period.getDays shouldBe days
    }

    // The term frequency is the one value no factory admits - ten thousand years exceeds the
    // thousand-year bound, exactly as in Java - so it is canonical by construction and is read
    // back through its name rather than through a factory.
    Frequency.TERM.normalized shouldBe Frequency.TERM
    Frequency.of(Frequency.TERM.period) should beFailureWith(FailureReason.INVALID)
    Frequency.parse(Frequency.TERM.name) should haveValue(Frequency.TERM)
  }

  //-------------------------------------------------------------------------
  test("test_isWeekBased") {
    forAll(data_based) { (frequency: Frequency, weekBased: Boolean, _: Boolean, _: Boolean) =>
      frequency.isWeekBased shouldBe weekBased
    }
  }

  test("test_isMonthBased") {
    forAll(data_based) { (frequency: Frequency, _: Boolean, monthBased: Boolean, _: Boolean) =>
      frequency.isMonthBased shouldBe monthBased
    }
  }

  test("test_isAnnual") {
    forAll(data_based) { (frequency: Frequency, _: Boolean, _: Boolean, annual: Boolean) =>
      frequency.isAnnual shouldBe annual
    }
  }

  //-------------------------------------------------------------------------
  test("test_eventsPerYear") {
    forAll(data_events) { (frequency: Frequency, expected: Int) =>
      frequency.eventsPerYear should haveValue(expected)
    }
  }

  // A frequency with no integral number of events per year is a data-dependent outcome
  // rather than a breach of contract, so the port reports it as a failure where the Java
  // method threw. The six frequencies here are the Java cases: three that do not divide
  // 364 days, two that do not divide 12 months, and one that mixes months with days.
  test("test_eventsPerYear_bad") {
    val inexact: TableFor1[Frequency] = Table(
      "frequency",
      freq(Frequency.ofDays(3)),
      freq(Frequency.ofWeeks(3)),
      freq(Frequency.ofWeeks(104)),
      freq(Frequency.ofMonths(5)),
      freq(Frequency.ofMonths(24)),
      freq(Frequency.of(Period.of(2, 2, 2)))
    )
    forAll(inexact) { (frequency: Frequency) =>
      frequency.eventsPerYear should beFailureWith(FailureReason.INVALID)
    }
  }

  test("test_eventsPerYearEstimate") {
    forAll(data_events) { (frequency: Frequency, expected: Int) =>
      // the estimate is total, so there is no error channel to read here; the widening of
      // the expected count is written out rather than left to the compiler
      frequency.eventsPerYearEstimate shouldBe (expected.toDouble +- 1e-8)
    }
  }

  // The estimate exists for every frequency, including the ones with no exact count, which
  // is what this test asserts. The tolerances are the Java tolerances: the last three rows
  // mix years with days and were compared at 1e-3 there, the rest at 1e-8.
  test("test_eventsPerYearEstimate_bad") {
    val estimates: TableFor3[Frequency, Double, Double] = Table(
      ("frequency", "expected", "tolerance"),
      (freq(Frequency.ofDays(3)), 364d / 3, 1e-8),
      (freq(Frequency.ofWeeks(3)), 364d / 21, 1e-8),
      (freq(Frequency.ofWeeks(104)), 364d / 728, 1e-8),
      (freq(Frequency.ofMonths(5)), 12d / 5, 1e-8),
      (freq(Frequency.ofMonths(22)), 12d / 22, 1e-8),
      (freq(Frequency.ofMonths(24)), 12d / 24, 1e-8),
      (freq(Frequency.ofYears(2)), 0.5d, 1e-8),
      (freq(Frequency.of(Period.of(10, 0, 1))), 0.1d, 1e-3),
      (freq(Frequency.of(Period.of(5, 0, 95))), 0.19d, 1e-3),
      (freq(Frequency.of(Period.of(5, 0, 97))), 0.19d, 1e-3)
    )
    forAll(estimates) { (frequency: Frequency, expected: Double, tolerance: Double) =>
      frequency.eventsPerYearEstimate shouldBe (expected +- tolerance)
    }
  }

  //-------------------------------------------------------------------------
  test("test_exactDivide") {
    forAll(data_exactDivide) { (frequency: Frequency, other: Frequency, expected: Int) =>
      frequency.exactDivide(other) should haveValue(expected)
    }
  }

  // Division the other way round divides a shorter frequency by a longer one, which is never
  // exact unless the two are the same frequency. The Java test skipped the equal rows; here
  // they assert the quotient the row itself carries, which is one for every such row.
  test("test_exactDivide_reverse") {
    forAll(data_exactDivide) { (frequency: Frequency, other: Frequency, expected: Int) =>
      if (frequency != other) {
        other.exactDivide(frequency) should beFailureWith(FailureReason.INVALID)
      } else {
        other.exactDivide(frequency) should haveValue(expected)
      }
    }
  }

  test("test_exactDivide_bad") {
    val indivisible: TableFor2[Frequency, Frequency] = Table(
      ("frequency", "other"),
      (freq(Frequency.ofDays(5)), freq(Frequency.ofDays(2))),
      (freq(Frequency.ofMonths(5)), freq(Frequency.ofMonths(2))),
      (Frequency.P1M, Frequency.P1W),
      (Frequency.P1W, Frequency.P1M),
      (Frequency.TERM, Frequency.P1W),
      (Frequency.P12M, Frequency.TERM),
      (freq(Frequency.ofYears(1)), Frequency.P1W)
    )
    forAll(indivisible) { (frequency: Frequency, other: Frequency) =>
      frequency.exactDivide(other) should beFailureWith(FailureReason.INVALID)
    }
  }

  //-------------------------------------------------------------------------
  test("test_parse_String_roundTrip") {
    Frequency.parse(Frequency.P6M.toString) should haveValue(Frequency.P6M)
  }

  test("test_parse_String_good_noP") {
    forAll(data_parseGood) { (text: String, expected: Frequency) =>
      Frequency.parse(text) should haveValue(expected)
    }
  }

  test("test_parse_String_good_withP") {
    forAll(data_parseGood) { (text: String, expected: Frequency) =>
      Frequency.parse("P" + text) should haveValue(expected)
    }
  }

  test("test_parse_String_term") {
    Frequency.parse("Term") should haveValue(Frequency.TERM)
    Frequency.parse("TERM") should haveValue(Frequency.TERM)
  }

  // The Java provider's sixth row was `null`; it has no counterpart because nothing in this
  // port passes `null` to a factory - an absent value is an `Option` and a rejection is a
  // `Failure` - so the five rows of text remain, each asserting the reason of its rejection.
  test("test_parse_String_bad") {
    forAll(data_parseBad) { (text: String, reason: FailureReason) =>
      Frequency.parse(text) should beFailureWith(reason)
    }
  }

  //-------------------------------------------------------------------------
  test("test_addTo") {
    Frequency.P1D.addTo(LocalDate.of(2014, 6, 30)) shouldBe LocalDate.of(2014, 7, 1)
    // The Java test's second assertion added a week to an `OffsetDateTime` through the
    // `TemporalAmount` interface that the ported type deliberately does not implement - see
    // `test_temporalAmount` - and it has no counterpart: `addTo` takes a `LocalDate`, so the
    // same week step is asserted on a date instead.
    Frequency.P1W.addTo(LocalDate.of(2014, 6, 30)) shouldBe LocalDate.of(2014, 7, 7)
  }

  test("test_subtractFrom") {
    Frequency.P1D.subtractFrom(LocalDate.of(2014, 6, 30)) shouldBe LocalDate.of(2014, 6, 29)
    // as in `test_addTo`, the `OffsetDateTime` assertion becomes the same week step on a date
    Frequency.P1W.subtractFrom(LocalDate.of(2014, 6, 30)) shouldBe LocalDate.of(2014, 6, 23)
  }

  //-------------------------------------------------------------------------
  /**
   * The headline divergence of this file, recorded in `SCALA_MIGRATION.md`.
   *
   * The Java `Frequency` implements `java.time.temporal.TemporalAmount`, whose `getUnits`
   * returns a `java.util.List`; the public API of this port admits no JDK collection type
   * anywhere, which the compiled-descriptor audit of the build enforces, so the interface is
   * not implemented and its two accessors have no replacement. Each of the Java assertions is
   * therefore ported to the surface that replaced it:
   *
   *  - `getUnits()` and `get(MONTHS)` become the components of the period the frequency holds,
   *    which is where a caller now reads them from;
   *  - `date.plus(frequency)` and `date.minus(frequency)` become `addTo` and `subtractFrom`,
   *    which are exactly the operations the Java implementation performed for a `LocalDate`;
   *  - the assertion that `get(CENTURIES)` is unsupported becomes a proof that the interface
   *    really is absent - a date cannot be added to a frequency at all, so the expression the
   *    Java test relied on does not compile.
   */
  test("test_temporalAmount") {
    Frequency.P3M.period.getYears shouldBe 0
    Frequency.P3M.period.getMonths shouldBe 3
    Frequency.P3M.period.getDays shouldBe 0
    Frequency.P1W.addTo(LocalDate.of(2014, 6, 30)) shouldBe LocalDate.of(2014, 7, 7)
    Frequency.P1W.subtractFrom(LocalDate.of(2014, 6, 30)) shouldBe LocalDate.of(2014, 6, 23)
    assertDoesNotCompile("java.time.LocalDate.of(2014, 6, 30).plus(Frequency.P1W)")
  }

  //-------------------------------------------------------------------------
  test("test_equals_hashCode") {
    val a1 = Frequency.P1D
    val a2 = freq(Frequency.ofDays(1))
    val b = Frequency.P3M
    a1 shouldBe a1
    a1 shouldBe a2
    a1 should not be b
    // the Java assertion against a value of an unrelated type; the `null` comparison beside
    // it has no counterpart, because no value of this port is ever compared with `null`
    a1.equals("") shouldBe false
    a1.hashCode shouldBe a2.hashCode
    // The companion publishes one equality-bearing instance - an `Order` that is also a
    // `Hash`, so `Eq`, `Order` and `Hash` cannot disagree - plus `Show`. All three readings
    // of the pair above agree with universal equality, and `Show` renders the name.
    Eq[Frequency].eqv(a1, a2) shouldBe true
    Eq[Frequency].eqv(a1, b) shouldBe false
    Hash[Frequency].hash(a1) shouldBe Hash[Frequency].hash(a2)
    Show[Frequency].show(a1) shouldBe a1.name
  }

  //-----------------------------------------------------------------------
  // Java serialization and Joda-Beans wire compatibility are out of scope for this port, so
  // the Java `assertSerialization` has no target. What replaced it is the JSON codec the
  // companion publishes, and the round trip asserted here is that one: a frequency is a bare
  // string in JSON - its name - so the encoded form is asserted as well as the round trip.
  test("test_serialization") {
    val frequencyCodec: Codec[Frequency] = implicitly[Codec[Frequency]]
    val values: TableFor2[Frequency, String] = Table(
      ("frequency", "json"),
      (Frequency.P1D, "P1D"),
      (Frequency.P3M, "P3M"),
      (Frequency.P12M, "P12M"),
      (Frequency.TERM, "Term")
    )
    forAll(values) { (frequency: Frequency, json: String) =>
      frequencyCodec(frequency) shouldBe Json.fromString(json)
      frequencyCodec.decodeJson(Json.fromString(json)) shouldBe Right(frequency)
    }
  }

  // Joda-Convert has no target either: the text form of a frequency is its name, rendered by
  // `toString`, by `Show` and by the JSON codec alike, and read back by `parse`. That name
  // round trip is what the annotated conversion was asserting, so it is what is asserted here.
  test("test_jodaConvert") {
    val values: TableFor1[Frequency] =
      Table("frequency", Frequency.P1D, Frequency.P3M, Frequency.P12M, Frequency.TERM)
    forAll(values) { (frequency: Frequency) =>
      Frequency.parse(frequency.name) should haveValue(frequency)
      Show[Frequency].show(frequency) shouldBe frequency.name
      frequency.toString shouldBe frequency.name
    }
  }
}
