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
 * The type under test is normalising: construction reduces a period to the canonical form of
 * its length, so one length is one value - twelve months and one year are the same frequency -
 * and a month count above twelve is held, and named, in years and months. The canonical name is
 * a `P3M`-style spelling of that length - in weeks where a day count divides by seven, and
 * `Term` for the term frequency - and it is the text `toString`, `Show`, the JSON codec and
 * `parse` all work in.
 *
 * One further test, named descriptively rather than after a method, states the ceiling the
 * grammar of a frequency puts on the text `parse` reads, which this port tests before it
 * transforms the text.
 *
 * The factories report a rejection as a value rather than by throwing. A table row that has to
 * hold a frequency therefore unwraps one through [[freq]], and a rejection is asserted through
 * the matchers of the shared testkit, reason included: a period that cannot be a frequency is a
 * `FailureReason.INVALID` failure, whether a factory, `eventsPerYear` or `exactDivide` rejects
 * it, while text that names no period at all is a `FailureReason.PARSING` failure.
 *
 * Doubles are compared at 1e-8 throughout, except the three events-per-year estimates whose
 * period mixes years with days, which are compared at 1e-3.
 */
final class FrequencySpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  /**
   * The number of draws the one generated property of this file is checked against: the
   * idempotence and length-preservation property inside `test_normalized`.
   *
   * That property draws a month count from 1 to 12,000 - the whole range the factories of this
   * type admit - against a day count from 0 to 400, some 4.8 million pairs, so the default
   * sample of a handful of draws would say very little about it. Five hundred draws spread
   * across that range, and the three canonicalisation corners the property exists to pin are
   * named as generator specials at the property itself, which gives them far more weight than a
   * uniform draw would: reading the property probabilistically, those corners are very likely to
   * be visited within five hundred draws rather than certain to be. Five hundred is also cheap -
   * the property builds four frequencies per draw and nothing else.
   *
   * The count governs generator-driven checks only, so the table-driven `forAll(data_...)` tests
   * below are unaffected by it: each of those evaluates every row of its table, always.
   */
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 500)

  /**
   * Unwraps the outcome of a factory that is expected to produce a frequency.
   *
   * Unwrapping through this helper rather than with `getOrElse` and a fabricated fallback keeps
   * a mistake in a table visible: a row naming a period that is not a frequency fails the
   * suite, naming the row's failures, instead of quietly testing some other value.
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
   * The table shared by `test_of_int`, `test_of_Period` and `test_parse`.
   *
   * Each row is a frequency, the period it is expected to hold and the text it is expected to
   * render as - which is also the text `parse` reads back. The rows built by `ofDays` and
   * `ofWeeks` are the ones that pin the days-to-weeks naming: seven days is a week and ninety-one
   * days is thirteen weeks, so those rows expect the week-named text against a period still
   * measured in days.
   *
   * Four rows state the canonical form of their length rather than the period the factory was
   * handed: eighteen, twenty-four and thirty months are held as years and months, and one year
   * is held as twelve months, which is why `ofYears(1)` has a `P12M` row of its own beside the
   * `ofMonths(12)` one.
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
    // canonical rows: P1Y6M, P2Y, P2Y6M and P12M
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
   * The table of `test_ofMonths`.
   *
   * Each row is a number of months, the period the factory is expected to hold for it and the
   * text it renders as. Months beyond twelve are redistributed into years and months, which is
   * what the twenty-, twenty-four- and thirty-month rows assert. Twelve months is the one length
   * above a month whose canonical form is still months.
   */
  private val data_ofMonths: TableFor3[Int, Period, String] = Table(
    ("months", "period", "text"),
    (1, Period.ofMonths(1), "P1M"),
    (2, Period.ofMonths(2), "P2M"),
    (3, Period.ofMonths(3), "P3M"),
    (4, Period.ofMonths(4), "P4M"),
    (6, Period.ofMonths(6), "P6M"),
    (12, Period.ofMonths(12), "P12M"),
    // canonical rows: P1Y8M, P2Y and P2Y6M
    (20, Period.of(1, 8, 0), "P1Y8M"),
    (24, Period.ofYears(2), "P2Y"),
    (30, Period.of(2, 6, 0), "P2Y6M")
  )

  /**
   * The table of `test_ofYears`.
   *
   * Each row is a number of years, the period the factory holds for it and its text. Two years
   * and three years are held as years. One year is not: the canonical form of that length is
   * twelve months, so the factory yields `Frequency.P12M`.
   */
  private val data_ofYears: TableFor3[Int, Period, String] = Table(
    ("years", "period", "text"),
    // canonical row: one year is held as P12M
    (1, Period.ofMonths(12), "P12M"),
    (2, Period.ofYears(2), "P2Y"),
    (3, Period.ofYears(3), "P3Y")
  )

  /**
   * The table of `test_normalized`.
   *
   * Each row is the period a frequency is built from and the canonical period the frequency
   * holds - which, because construction canonicalises, is both the period of the value and the
   * period of its normalisation. The first four rows are the day-based and week-based cases,
   * which canonicalisation leaves exactly as they are; the rest redistribute months into years
   * and months, so thirty months becomes two years and six months. Twelve months and one year
   * are one value whose canonical period is twelve months, which is why both rows expect `P12M`.
   */
  private val data_normalized: TableFor2[Period, Period] = Table(
    ("period", "canonical"),
    (Period.ofDays(1), Period.ofDays(1)),
    (Period.ofDays(7), Period.ofDays(7)),
    (Period.ofDays(10), Period.ofDays(10)),
    (Period.ofWeeks(2), Period.ofDays(14)),
    (Period.ofMonths(1), Period.ofMonths(1)),
    (Period.ofMonths(2), Period.ofMonths(2)),
    // canonical rows: both spellings of a year are held as P12M
    (Period.ofMonths(12), Period.ofMonths(12)),
    (Period.ofYears(1), Period.ofMonths(12)),
    (Period.ofMonths(20), Period.of(1, 8, 0)),
    (Period.ofMonths(24), Period.ofYears(2)),
    (Period.ofYears(2), Period.ofYears(2)),
    (Period.ofMonths(30), Period.of(2, 6, 0))
  )

  /**
   * The table shared by `test_isWeekBased`, `test_isMonthBased` and `test_isAnnual`.
   *
   * Each row is a frequency and the three answers it gives. The last two rows carry the
   * non-obvious cases: a frequency mixing years, months and days is neither week-based nor
   * month-based, and the term frequency is neither of those and not annual either, even though
   * its period is a whole number of years.
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
   * The table shared by `test_eventsPerYear` and `test_eventsPerYearEstimate`.
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
   * The table shared by `test_exactDivide` and `test_exactDivide_reverse`.
   *
   * Each row is a frequency, the frequency it is divided by and the exact quotient. The rows run
   * in blocks: day-based into day-based, week-based into week-based and day-based, then
   * month-based and year-based into month-based. Day-based and month-based lengths never divide
   * one another, which is what the reverse test and `test_exactDivide_bad` assert from the other
   * side.
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
   * The table shared by `test_parse_String_good_noP` and `test_parse_String_good_withP`.
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
   * The table of `test_parse_String_bad`: text `parse` rejects, and the reason it rejects it for.
   *
   * The rows divide into the two failures `parse` distinguishes. Text that names no period at
   * all is a `FailureReason.PARSING` failure, while `-2D` names a period perfectly well and is
   * then rejected for being negative, which is a `FailureReason.INVALID` failure.
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
    // the four spellings `parse` accepts for the term frequency
    val spellings: TableFor1[String] = Table("text", "Term", "0T", "1T", "T")
    forAll(spellings) { (text: String) =>
      Frequency.parse(text) should haveValue(Frequency.TERM)
    }
  }

  //-------------------------------------------------------------------------
  // A period that cannot be a frequency is rejected as an invalid-argument failure by every
  // factory that can be handed it, and each of the three rejections below words its message the
  // same way whichever factory produced it, which is why the message is asserted with the
  // reason.
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
    // the two bounds, each named in the message of the failure that reports it
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
    // that value agrees: its name, its hash, its position in the ordering and its JSON. That is
    // what the normalising contract buys - were this length two values, the choice of factory
    // would be observable in equality, in sorted collections and in serialized documents.
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
    forAll(Table("text", "P1Y", "1Y", "P12M", "12M")) { (text: String) =>
      Frequency.parse(text) should haveValue(Frequency.P12M)
    }

    // Construction is idempotent and length-preserving beyond the rows above: rebuilding a
    // frequency from the period it holds gives the same value back, normalising it changes
    // nothing, and the two ways of spelling one length - all months, or years and months - are
    // one frequency. Neither the total number of months nor the number of days moves, which is
    // why no arithmetic of this type is affected by canonicalisation.
    //
    // The claim is sampled, not exhausted: month counts 1 to 12,000 - the whole range the
    // factories admit - against day counts 0 to 400 is some 4.8 million pairs, drawn five
    // hundred times as configured at the head of this file. Three month counts are therefore
    // named as generator specials, which carries them the weight of a bound instead of the
    // roughly one-in-ten-thousand weight of a uniform draw: twelve, the one length whose
    // canonical form is twelve months rather than one year; thirteen, the first count
    // redistributed into years and months; and twenty-four, an exact number of years. Read
    // probabilistically, those three corners are very likely to be drawn and the rest of the
    // sample spreads over the range. The day count needs no special of its own - `chooseNum`
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

    // The term frequency is the one value no factory admits - its ten thousand years exceed the
    // thousand-year bound - so it is canonical by construction and is read back through its name
    // rather than through a factory.
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

  // A frequency with no integral number of events per year is a data-dependent outcome rather
  // than a breach of contract, so it is reported as a failure. The six frequencies here are
  // three that do not divide 364 days, two that do not divide 12 months, and one that mixes
  // months with days.
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

  // The estimate exists for every frequency, including the ones with no exact count, which is
  // what this test asserts. The last three rows mix years with days, so their estimate is the
  // approximation the type makes for such a period and is compared at 1e-3; the rest are
  // compared at the 1e-8 used throughout.
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
  // exact unless the two are the same frequency; where a row holds one frequency twice, the
  // quotient the row carries is one and is asserted as such.
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

  test("test_parse_String_bad") {
    forAll(data_parseBad) { (text: String, reason: FailureReason) =>
      Frequency.parse(text) should beFailureWith(reason)
    }
  }

  /**
   * Asserts the ceiling the grammar of a frequency puts on the text `parse` reads.
   *
   * No counterpart in the Java test class: the Java method compared the text against the four
   * term spellings, copied it to add the ISO-8601 prefix and handed the copy to `Period.parse`,
   * every one of those costs being proportional to the length of text a caller supplied, so text
   * written to be large was worked on at length before being refused (CWE-400/CWE-770). The
   * grammar itself is tiny - four literals, or a period whose longest spelling is a few dozen
   * characters - so the parse refuses text longer than two hundred and fifty-six characters
   * before doing any of that.
   *
   * Two things are asserted, and between them they say the ceiling bounds work without changing
   * meaning. Text at the ceiling behaves exactly as before, including the message that quotes it
   * in full; text past it is refused with a message naming the ceiling and '''not''' the text,
   * which is the wording `Decimal` reports for the same condition on the numeral it reads and
   * the one place this port does not quote what it refused - the input is refused for its size,
   * so writing it out is the very thing the refusal exists to avoid.
   */
  test("parsing refuses text longer than the grammar of a frequency can be, naming the ceiling") {
    // at the ceiling: the ordinary parsing failure, quoting the text in full
    val atCeiling: String = "A" * 256
    Frequency.parse(atCeiling) should beFailureWith(FailureReason.PARSING)
    Frequency.parse(atCeiling).left.toOption.map(failure => failure.message) shouldBe
      Some(s"Unable to parse frequency: '$atCeiling'")

    // past it: the ceiling is named and the text is not, whatever its size and whichever part of
    // the grammar it was aiming at - the term spellings and the period form are both behind it
    List("A" * 257, "A" * 10000, "Term" + ("m" * 10000), "P" + ("1" * 10000) + "D").foreach { oversized =>
      withClue(s"a payload of ${oversized.length} characters: ") {
        val refused = Frequency.parse(oversized)
        refused should beFailureWith(FailureReason.PARSING)
        refused.left.toOption.map(failure => failure.message) shouldBe
          Some("Frequency string must not exceed 256 characters")
      }
    }

    // and the term spellings themselves, which are well inside the ceiling, still parse
    Frequency.parse("Term") should haveValue(Frequency.TERM)
    Frequency.parse("0T") should haveValue(Frequency.TERM)
  }

  //-------------------------------------------------------------------------
  test("test_addTo") {
    Frequency.P1D.addTo(LocalDate.of(2014, 6, 30)) shouldBe LocalDate.of(2014, 7, 1)
    // `addTo` accepts a `LocalDate` and nothing wider: the type implements no
    // `java.time.temporal.TemporalAmount`, as `test_temporalAmount` states
    Frequency.P1W.addTo(LocalDate.of(2014, 6, 30)) shouldBe LocalDate.of(2014, 7, 7)
  }

  test("test_subtractFrom") {
    Frequency.P1D.subtractFrom(LocalDate.of(2014, 6, 30)) shouldBe LocalDate.of(2014, 6, 29)
    Frequency.P1W.subtractFrom(LocalDate.of(2014, 6, 30)) shouldBe LocalDate.of(2014, 6, 23)
  }

  //-------------------------------------------------------------------------
  /**
   * The type under test deliberately implements no `java.time.temporal.TemporalAmount`.
   *
   * That interface's `getUnits` answers a `java.util.List`, and no JDK collection type appears
   * in the public API of this library, so the interface is absent and the surfaces that stand in
   * for it are what this test exercises: the components of the period a frequency holds are
   * where a caller reads its years, months and days, and [[Frequency.addTo]] and
   * [[Frequency.subtractFrom]] are the date arithmetic. The absence itself is asserted rather
   * than assumed - a date cannot be added to a frequency, so `date.plus(frequency)` does not
   * compile.
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
    a1.equals("") shouldBe false
    a1.hashCode shouldBe a2.hashCode
    // The companion publishes one equality-bearing instance - an `Order` that is also a `Hash`
    // - plus `Show`, so the three summons below all read that one instance. Each agrees with
    // universal equality on the pair above, and `Show` renders the name.
    Eq[Frequency].eqv(a1, a2) shouldBe true
    Eq[Frequency].eqv(a1, b) shouldBe false
    Hash[Frequency].hash(a1) shouldBe Hash[Frequency].hash(a2)
    Show[Frequency].show(a1) shouldBe a1.name
  }

  //-----------------------------------------------------------------------
  // The serialized form of a frequency is the JSON the codec its companion publishes produces:
  // a bare string holding the name, which is why the encoded form is asserted alongside the
  // round trip rather than only the round trip.
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

  // The text form of a frequency is its name: `toString` and `Show` both render it and `parse`
  // reads it back, so the three agree on one round trip, which is what this test asserts.
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
