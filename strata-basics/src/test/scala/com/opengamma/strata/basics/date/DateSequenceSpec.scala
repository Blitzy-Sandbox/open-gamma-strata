/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate
import java.time.YearMonth

import cats.Hash
import cats.Order
import cats.Show

import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor1, TableFor4}

import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper._

/**
 * Test [[DateSequence]].
 *
 * The Java original ran twenty-four annotated methods: twelve plain tests and twelve
 * parameterised ones fed by six data providers, each provider serving one `nextOrSame`
 * method and one `next` method. That shape is preserved exactly. Every provider becomes
 * one table declared once, and each of the twelve parameterised methods keeps its own
 * test under its own name, driven from the shared table, so the method-level mapping of
 * the migration stays one-to-one rather than collapsing a pair into a single test.
 *
 * The pairing is the structural heart of the spec and is the reason the pairs are not
 * merged. The `nextOrSame` test of a pair proves that a date which is itself a date of
 * the sequence is returned unchanged; the `next` test proves that the very same date is
 * stepped over. The two therefore derive different expectations from one row, and a
 * merged test could only assert one of the two semantics.
 *
 * ===Three ported constructs that changed shape===
 *
 *  - '''Lookup by name reports rather than raises.''' The original resolved a name
 *    through a factory that threw when the name matched no member. Here
 *    [[DateSequence.parse]] answers `EitherNec[Failure, DateSequence]`, so the six
 *    lookup tests assert the value on the right through the `haveValue` matcher, and
 *    the one name that belongs to no member is asserted as a `PARSING` failure by value
 *    rather than as a thrown error. Nothing in this spec expects an exception, which
 *    matches the original: it contained no exception assertion of any kind.
 *  - '''There is no run-time registry to interrogate.''' The original asked the registry
 *    that had assembled the family from a configuration resource for the complete map of
 *    names it had loaded. The family is closed at compile time here, so `test_extendedEnum`
 *    asserts the equivalent property over the name tables the typeclass derives from the
 *    members themselves.
 *  - '''The fixture subtype has no Scala target.''' The original declared its own
 *    implementation of the interface in order to exercise the four methods the interface
 *    implemented for its subtypes. [[DateSequence]] is `sealed`, so no implementation can
 *    be declared here; `test_dummy` records what the fixture proved and proves it against
 *    the members of the family instead. The reasoning is set out at that test.
 *
 * Numerical parity of the sequence arithmetic against the original is a separate concern,
 * owned by the schedule parity fixture and its spec; what is asserted here is the dates
 * the original test asserted, transcribed row for row.
 *
 * @see [[DateSequences]] for the constants under the identifiers the original published
 * @see [[SequenceDate]] for the instruction type, whose cases belong to its own spec
 */
final class DateSequenceSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The inclusive range of dates the original's `while` loops walked.
   *
   * Each parameterised method of the original advanced a date one day at a time while it
   * was not after the first expected date, asserting on every day in between. The
   * iterator returned here is that sequence of days: it starts at `start`, steps by one
   * day and stops after `end`, so a caller folds over exactly the days the original
   * asserted on and in the same order. It is lazy and finite - the underlying iteration
   * is unbounded and is cut by the stopping condition - and it is empty when `start` is
   * already after `end`, which is the loop that never ran.
   *
   * @param start  the first date to visit
   * @param end  the last date to visit, inclusive
   * @return the days from `start` to `end` inclusive, in ascending order
   */
  private def datesFrom(start: LocalDate, end: LocalDate): Iterator[LocalDate] =
    Iterator.iterate(start)(_.plusDays(1L)).takeWhile(!_.isAfter(end))

  //-------------------------------------------------------------------------
  test("test_QUARTERLY_IMM") {
    val test = DateSequences.QUARTERLY_IMM
    test.name shouldBe "Quarterly-IMM"
    test.toString shouldBe "Quarterly-IMM"
    // March 2013: the Wednesdays are the 6th, 13th and 20th, so the third is the 20th
    test.dateMatching(YearMonth.of(2013, 3)) shouldBe LocalDate.of(2013, 3, 20)
  }

  //-------------------------------------------------------------------------
  // The six lookup tests. Each asserts that the name resolves to its sequence and that
  // the sequence names the right base sequence - the two serial sequences are halves of
  // a pair whose base is the quarterly sequence, and the other four are their own base.
  //-------------------------------------------------------------------------
  test("test_QUARTERLY_IMM_of") {
    DateSequence.parse("Quarterly-IMM") should haveValue(DateSequences.QUARTERLY_IMM)
    DateSequences.QUARTERLY_IMM.baseSequence shouldBe DateSequences.QUARTERLY_IMM
  }

  test("test_QUARTERLY_IMM_6_SERIAL_of") {
    DateSequence.parse("Quarterly-IMM-6-Serial") should
      haveValue(DateSequences.QUARTERLY_IMM_6_SERIAL)
    DateSequences.QUARTERLY_IMM_6_SERIAL.baseSequence shouldBe DateSequences.QUARTERLY_IMM
  }

  test("test_QUARTERLY_IMM_3_SERIAL_of") {
    DateSequence.parse("Quarterly-IMM-3-Serial") should
      haveValue(DateSequences.QUARTERLY_IMM_3_SERIAL)
    DateSequences.QUARTERLY_IMM_3_SERIAL.baseSequence shouldBe DateSequences.QUARTERLY_IMM
  }

  test("test_MONTHLY_IMM_of") {
    DateSequence.parse("Monthly-IMM") should haveValue(DateSequences.MONTHLY_IMM)
    DateSequences.MONTHLY_IMM.baseSequence shouldBe DateSequences.MONTHLY_IMM
  }

  test("test_QUARTERLY_10TH_of") {
    DateSequence.parse("Quarterly-10th") should haveValue(DateSequences.QUARTERLY_10TH)
    DateSequences.QUARTERLY_10TH.baseSequence shouldBe DateSequences.QUARTERLY_10TH
  }

  test("test_MONTHLY_1ST_of") {
    DateSequence.parse("Monthly-1st") should haveValue(DateSequences.MONTHLY_1ST)
    DateSequences.MONTHLY_1ST.baseSequence shouldBe DateSequences.MONTHLY_1ST
  }

  //-------------------------------------------------------------------------
  /**
   * The quarterly IMM provider, transcribed row for row from the original.
   *
   * A row is a base date followed by the three quarterly IMM dates that the sequence
   * produces from it: the third Wednesday of the next March, June, September or December
   * on or after the base date, and the two quarterly dates after that. The base date of
   * each row after the first is the first expected date of the row before it, so the five
   * rows walk a chain of consecutive quarterly dates from January 2013 into September
   * 2014, which is what makes a single shifted date visible as a failure in two rows
   * rather than one.
   */
  private val data_quarterlyImm: TableFor4[LocalDate, LocalDate, LocalDate, LocalDate] = Table(
    ("base", "immDate1", "immDate2", "immDate3"),
    (date(2013, 1, 1), date(2013, 3, 20), date(2013, 6, 19), date(2013, 9, 18)),
    (date(2013, 3, 20), date(2013, 6, 19), date(2013, 9, 18), date(2013, 12, 18)),
    (date(2013, 6, 19), date(2013, 9, 18), date(2013, 12, 18), date(2014, 3, 19)),
    (date(2013, 9, 18), date(2013, 12, 18), date(2014, 3, 19), date(2014, 6, 18)),
    (date(2013, 12, 18), date(2014, 3, 19), date(2014, 6, 18), date(2014, 9, 17))
  )

  test("test_nextOrSameQuarterlyImm") {
    forAll(data_quarterlyImm) {
      (base: LocalDate, immDate1: LocalDate, immDate2: LocalDate, immDate3: LocalDate) =>
        // every day from the day after the base date up to and including the first
        // expected date answers with that first expected date, which is the whole of
        // what "or same" adds: the last day of the walk is the expected date itself
        datesFrom(base.plusDays(1L), immDate1).foreach { current =>
          withClue(s"nextOrSame from $current: ") {
            DateSequences.QUARTERLY_IMM.nextOrSame(current) shouldBe immDate1
            DateSequences.QUARTERLY_IMM.nthOrSame(current, 1) shouldBe immDate1
            DateSequences.QUARTERLY_IMM.nthOrSame(current, 2) shouldBe immDate2
            DateSequences.QUARTERLY_IMM.nthOrSame(current, 3) shouldBe immDate3
          }
        }
        // the original made this assertion once, after its loop, with the date the loop
        // had advanced to - the day after the first expected date, whose month is the
        // month of that date for every row of this provider
        val monthOfFirst = YearMonth.from(immDate1.plusDays(1L))
        DateSequences.QUARTERLY_IMM.dateMatching(monthOfFirst) shouldBe immDate1
    }
  }

  test("test_nextQuarterlyImm") {
    forAll(data_quarterlyImm) {
      (base: LocalDate, immDate1: LocalDate, immDate2: LocalDate, immDate3: LocalDate) =>
        // this walk starts at the base date itself rather than the day after it, so a
        // base date that is a date of the sequence is visited and stepped over
        datesFrom(base, immDate1).foreach { current =>
          withClue(s"next from $current: ") {
            if (current == immDate1) {
              // the input is the first expected date, so `next` passes over it
              DateSequences.QUARTERLY_IMM.next(current) shouldBe immDate2
              DateSequences.QUARTERLY_IMM.nth(current, 1) shouldBe immDate2
              DateSequences.QUARTERLY_IMM.nth(current, 2) shouldBe immDate3
            } else {
              DateSequences.QUARTERLY_IMM.next(current) shouldBe immDate1
              DateSequences.QUARTERLY_IMM.nth(current, 1) shouldBe immDate1
              DateSequences.QUARTERLY_IMM.nth(current, 2) shouldBe immDate2
              DateSequences.QUARTERLY_IMM.nth(current, 3) shouldBe immDate3
            }
          }
        }
        succeed
    }
  }

  //-------------------------------------------------------------------------
  /**
   * The six-serial provider, transcribed row for row from the original.
   *
   * A row is a base date alone, because the expectations of the two tests it drives are
   * not literal dates but the dates the monthly and quarterly sequences produce from the
   * same base date. The six dates straddle the January, February and March IMM dates of
   * 2013 - two dates per month, one before its IMM date and one after it - which is what
   * exercises the boundary at which the serial count moves on a month.
   */
  private val data_quarterlyImm6Serial: TableFor1[LocalDate] = Table(
    "base",
    date(2013, 1, 1),
    date(2013, 1, 25),
    date(2013, 2, 1),
    date(2013, 2, 25),
    date(2013, 3, 1),
    date(2013, 3, 25)
  )

  test("test_nextOrSameQuarterlyImm6Serial") {
    forAll(data_quarterlyImm6Serial) { (base: LocalDate) =>
      withClue(s"nextOrSame from $base: ") {
        // the first six dates of the six-serial sequence are the IMM dates of six
        // consecutive months, so they are the monthly sequence exactly
        DateSequences.QUARTERLY_IMM_6_SERIAL.nextOrSame(base) shouldBe
          DateSequences.MONTHLY_IMM.nextOrSame(base)
        DateSequences.QUARTERLY_IMM_6_SERIAL.nthOrSame(base, 1) shouldBe
          DateSequences.MONTHLY_IMM.nthOrSame(base, 1)
        DateSequences.QUARTERLY_IMM_6_SERIAL.nthOrSame(base, 2) shouldBe
          DateSequences.MONTHLY_IMM.nthOrSame(base, 2)
        DateSequences.QUARTERLY_IMM_6_SERIAL.nthOrSame(base, 3) shouldBe
          DateSequences.MONTHLY_IMM.nthOrSame(base, 3)
        DateSequences.QUARTERLY_IMM_6_SERIAL.nthOrSame(base, 4) shouldBe
          DateSequences.MONTHLY_IMM.nthOrSame(base, 4)
        DateSequences.QUARTERLY_IMM_6_SERIAL.nthOrSame(base, 5) shouldBe
          DateSequences.MONTHLY_IMM.nthOrSame(base, 5)
        DateSequences.QUARTERLY_IMM_6_SERIAL.nthOrSame(base, 6) shouldBe
          DateSequences.MONTHLY_IMM.nthOrSame(base, 6)
        // those six subsume the first two quarterly dates, so from the seventh onwards
        // the sequence is quarterly again with its count four lower
        DateSequences.QUARTERLY_IMM_6_SERIAL.nthOrSame(base, 7) shouldBe
          DateSequences.QUARTERLY_IMM.nthOrSame(base, 3)
        DateSequences.QUARTERLY_IMM_6_SERIAL.nthOrSame(base, 8) shouldBe
          DateSequences.QUARTERLY_IMM.nthOrSame(base, 4)
      }
    }
  }

  test("test_nextQuarterlyImm6Serial") {
    forAll(data_quarterlyImm6Serial) { (base: LocalDate) =>
      withClue(s"next from $base: ") {
        DateSequences.QUARTERLY_IMM_6_SERIAL.next(base) shouldBe
          DateSequences.MONTHLY_IMM.next(base)
        DateSequences.QUARTERLY_IMM_6_SERIAL.nth(base, 1) shouldBe
          DateSequences.MONTHLY_IMM.nth(base, 1)
        DateSequences.QUARTERLY_IMM_6_SERIAL.nth(base, 2) shouldBe
          DateSequences.MONTHLY_IMM.nth(base, 2)
        DateSequences.QUARTERLY_IMM_6_SERIAL.nth(base, 3) shouldBe
          DateSequences.MONTHLY_IMM.nth(base, 3)
        DateSequences.QUARTERLY_IMM_6_SERIAL.nth(base, 4) shouldBe
          DateSequences.MONTHLY_IMM.nth(base, 4)
        DateSequences.QUARTERLY_IMM_6_SERIAL.nth(base, 5) shouldBe
          DateSequences.MONTHLY_IMM.nth(base, 5)
        DateSequences.QUARTERLY_IMM_6_SERIAL.nth(base, 6) shouldBe
          DateSequences.MONTHLY_IMM.nth(base, 6)
        DateSequences.QUARTERLY_IMM_6_SERIAL.nth(base, 7) shouldBe
          DateSequences.QUARTERLY_IMM.nth(base, 3)
        DateSequences.QUARTERLY_IMM_6_SERIAL.nth(base, 8) shouldBe
          DateSequences.QUARTERLY_IMM.nth(base, 4)
      }
    }
  }

  //-------------------------------------------------------------------------
  /**
   * The three-serial provider, transcribed row for row from the original.
   *
   * The original declared this provider separately from the six-serial one even though
   * the two hold the same six dates, and it stays separate here: the two providers feed
   * different sequences, and merging them would tie the coverage of one sequence to the
   * coverage of the other.
   */
  private val data_quarterlyImm3Serial: TableFor1[LocalDate] = Table(
    "base",
    date(2013, 1, 1),
    date(2013, 1, 25),
    date(2013, 2, 1),
    date(2013, 2, 25),
    date(2013, 3, 1),
    date(2013, 3, 25)
  )

  test("test_nextOrSameQuarterlyImm3Serial") {
    forAll(data_quarterlyImm3Serial) { (base: LocalDate) =>
      withClue(s"nextOrSame from $base: ") {
        // the first three dates of the three-serial sequence are the IMM dates of three
        // consecutive months, so they are the monthly sequence exactly
        DateSequences.QUARTERLY_IMM_3_SERIAL.nextOrSame(base) shouldBe
          DateSequences.MONTHLY_IMM.nextOrSame(base)
        DateSequences.QUARTERLY_IMM_3_SERIAL.nthOrSame(base, 1) shouldBe
          DateSequences.MONTHLY_IMM.nthOrSame(base, 1)
        DateSequences.QUARTERLY_IMM_3_SERIAL.nthOrSame(base, 2) shouldBe
          DateSequences.MONTHLY_IMM.nthOrSame(base, 2)
        DateSequences.QUARTERLY_IMM_3_SERIAL.nthOrSame(base, 3) shouldBe
          DateSequences.MONTHLY_IMM.nthOrSame(base, 3)
        // those three subsume the first quarterly date, so from the fourth onwards the
        // sequence is quarterly again with its count two lower
        DateSequences.QUARTERLY_IMM_3_SERIAL.nthOrSame(base, 4) shouldBe
          DateSequences.QUARTERLY_IMM.nthOrSame(base, 2)
        DateSequences.QUARTERLY_IMM_3_SERIAL.nthOrSame(base, 5) shouldBe
          DateSequences.QUARTERLY_IMM.nthOrSame(base, 3)
      }
    }
  }

  test("test_nextQuarterlyImm3Serial") {
    forAll(data_quarterlyImm3Serial) { (base: LocalDate) =>
      withClue(s"next from $base: ") {
        DateSequences.QUARTERLY_IMM_3_SERIAL.next(base) shouldBe
          DateSequences.MONTHLY_IMM.next(base)
        DateSequences.QUARTERLY_IMM_3_SERIAL.nth(base, 1) shouldBe
          DateSequences.MONTHLY_IMM.nth(base, 1)
        DateSequences.QUARTERLY_IMM_3_SERIAL.nth(base, 2) shouldBe
          DateSequences.MONTHLY_IMM.nth(base, 2)
        DateSequences.QUARTERLY_IMM_3_SERIAL.nth(base, 3) shouldBe
          DateSequences.MONTHLY_IMM.nth(base, 3)
        DateSequences.QUARTERLY_IMM_3_SERIAL.nth(base, 4) shouldBe
          DateSequences.QUARTERLY_IMM.nth(base, 2)
        DateSequences.QUARTERLY_IMM_3_SERIAL.nth(base, 5) shouldBe
          DateSequences.QUARTERLY_IMM.nth(base, 3)
      }
    }
  }

  //-------------------------------------------------------------------------
  /**
   * The monthly IMM provider, transcribed row for row from the original.
   *
   * A row is a base date followed by the three monthly IMM dates that follow it - the
   * third Wednesday of each of the next three months. As in the quarterly provider the
   * base date of each row is the first expected date of the row before it, so the three
   * rows walk a chain from December 2014 into May 2015. The chain crosses a year end,
   * which is where a month-arithmetic error surfaces.
   */
  private val data_monthlyImm: TableFor4[LocalDate, LocalDate, LocalDate, LocalDate] = Table(
    ("base", "immDate1", "immDate2", "immDate3"),
    (date(2014, 12, 17), date(2015, 1, 21), date(2015, 2, 18), date(2015, 3, 18)),
    (date(2015, 1, 21), date(2015, 2, 18), date(2015, 3, 18), date(2015, 4, 15)),
    (date(2015, 2, 18), date(2015, 3, 18), date(2015, 4, 15), date(2015, 5, 20))
  )

  test("test_nextOrSameMonthlyImm") {
    forAll(data_monthlyImm) {
      (base: LocalDate, immDate1: LocalDate, immDate2: LocalDate, immDate3: LocalDate) =>
        datesFrom(base.plusDays(1L), immDate1).foreach { current =>
          withClue(s"nextOrSame from $current: ") {
            DateSequences.MONTHLY_IMM.nextOrSame(current) shouldBe immDate1
            DateSequences.MONTHLY_IMM.nthOrSame(current, 1) shouldBe immDate1
            DateSequences.MONTHLY_IMM.nthOrSame(current, 2) shouldBe immDate2
            DateSequences.MONTHLY_IMM.nthOrSame(current, 3) shouldBe immDate3
          }
        }
        // the monthly sequence associates a month with the IMM date of that very month,
        // and the date the original's loop had advanced to is the day after the first
        // expected date, which lies in the same month as it
        val monthOfFirst = YearMonth.from(immDate1.plusDays(1L))
        DateSequences.MONTHLY_IMM.dateMatching(monthOfFirst) shouldBe immDate1
    }
  }

  test("test_nextMonthlyImm") {
    forAll(data_monthlyImm) {
      (base: LocalDate, immDate1: LocalDate, immDate2: LocalDate, immDate3: LocalDate) =>
        datesFrom(base, immDate1).foreach { current =>
          withClue(s"next from $current: ") {
            if (current == immDate1) {
              DateSequences.MONTHLY_IMM.next(current) shouldBe immDate2
              DateSequences.MONTHLY_IMM.nth(current, 1) shouldBe immDate2
              DateSequences.MONTHLY_IMM.nth(current, 2) shouldBe immDate3
            } else {
              DateSequences.MONTHLY_IMM.next(current) shouldBe immDate1
              DateSequences.MONTHLY_IMM.nth(current, 1) shouldBe immDate1
              DateSequences.MONTHLY_IMM.nth(current, 2) shouldBe immDate2
              DateSequences.MONTHLY_IMM.nth(current, 3) shouldBe immDate3
            }
          }
        }
        succeed
    }
  }

  //-------------------------------------------------------------------------
  /**
   * The quarterly tenth provider, transcribed row for row from the original.
   *
   * A row is a base date followed by the three sequence dates that follow it - the tenth
   * day of the next March, June, September or December on or after the base date, and the
   * two quarterly dates after that. The base dates are those of the quarterly IMM
   * provider, which are IMM dates rather than tenths, so every row starts from a date
   * that is deliberately not a date of this sequence.
   */
  private val data_quarterly10th: TableFor4[LocalDate, LocalDate, LocalDate, LocalDate] = Table(
    ("base", "expect1", "expect2", "expect3"),
    (date(2013, 1, 1), date(2013, 3, 10), date(2013, 6, 10), date(2013, 9, 10)),
    (date(2013, 3, 20), date(2013, 6, 10), date(2013, 9, 10), date(2013, 12, 10)),
    (date(2013, 6, 19), date(2013, 9, 10), date(2013, 12, 10), date(2014, 3, 10)),
    (date(2013, 9, 18), date(2013, 12, 10), date(2014, 3, 10), date(2014, 6, 10)),
    (date(2013, 12, 18), date(2014, 3, 10), date(2014, 6, 10), date(2014, 9, 10))
  )

  test("test_nextOrSameQuarterly10th") {
    forAll(data_quarterly10th) {
      (base: LocalDate, expect1: LocalDate, expect2: LocalDate, expect3: LocalDate) =>
        datesFrom(base.plusDays(1L), expect1).foreach { current =>
          withClue(s"nextOrSame from $current: ") {
            DateSequences.QUARTERLY_10TH.nextOrSame(current) shouldBe expect1
            DateSequences.QUARTERLY_10TH.nthOrSame(current, 1) shouldBe expect1
            DateSequences.QUARTERLY_10TH.nthOrSame(current, 2) shouldBe expect2
            DateSequences.QUARTERLY_10TH.nthOrSame(current, 3) shouldBe expect3
          }
        }
        val monthOfFirst = YearMonth.from(expect1.plusDays(1L))
        DateSequences.QUARTERLY_10TH.dateMatching(monthOfFirst) shouldBe expect1
    }
  }

  test("test_nextQuarterly10th") {
    forAll(data_quarterly10th) {
      (base: LocalDate, expect1: LocalDate, expect2: LocalDate, expect3: LocalDate) =>
        datesFrom(base, expect1).foreach { current =>
          withClue(s"next from $current: ") {
            if (current == expect1) {
              DateSequences.QUARTERLY_10TH.next(current) shouldBe expect2
              DateSequences.QUARTERLY_10TH.nth(current, 1) shouldBe expect2
              DateSequences.QUARTERLY_10TH.nth(current, 2) shouldBe expect3
            } else {
              DateSequences.QUARTERLY_10TH.next(current) shouldBe expect1
              DateSequences.QUARTERLY_10TH.nth(current, 1) shouldBe expect1
              DateSequences.QUARTERLY_10TH.nth(current, 2) shouldBe expect2
              DateSequences.QUARTERLY_10TH.nth(current, 3) shouldBe expect3
            }
          }
        }
        succeed
    }
  }

  //-------------------------------------------------------------------------
  /**
   * The monthly first provider, transcribed row for row from the original.
   *
   * A row is a base date followed by the first days of the next three months. The first
   * two rows differ only in their base date - the first of January, which is itself a
   * date of this sequence, and the second of January, which is the day after one - and
   * they carry the same three expectations. That pair is the provider's point: it is what
   * distinguishes the two tests it drives, since the `nextOrSame` test answers the base
   * date itself for the first row and the `next` test never does.
   */
  private val data_monthly1st: TableFor4[LocalDate, LocalDate, LocalDate, LocalDate] = Table(
    ("base", "expect1", "expect2", "expect3"),
    (date(2013, 1, 1), date(2013, 2, 1), date(2013, 3, 1), date(2013, 4, 1)),
    (date(2013, 1, 2), date(2013, 2, 1), date(2013, 3, 1), date(2013, 4, 1)),
    (date(2013, 4, 2), date(2013, 5, 1), date(2013, 6, 1), date(2013, 7, 1))
  )

  test("test_nextOrSameMonthly1st") {
    forAll(data_monthly1st) {
      (base: LocalDate, expect1: LocalDate, expect2: LocalDate, expect3: LocalDate) =>
        datesFrom(base.plusDays(1L), expect1).foreach { current =>
          withClue(s"nextOrSame from $current: ") {
            DateSequences.MONTHLY_1ST.nextOrSame(current) shouldBe expect1
            DateSequences.MONTHLY_1ST.nthOrSame(current, 1) shouldBe expect1
            DateSequences.MONTHLY_1ST.nthOrSame(current, 2) shouldBe expect2
            DateSequences.MONTHLY_1ST.nthOrSame(current, 3) shouldBe expect3
          }
        }
        val monthOfFirst = YearMonth.from(expect1.plusDays(1L))
        DateSequences.MONTHLY_1ST.dateMatching(monthOfFirst) shouldBe expect1
    }
  }

  test("test_nextMonthly1st") {
    forAll(data_monthly1st) {
      (base: LocalDate, expect1: LocalDate, expect2: LocalDate, expect3: LocalDate) =>
        datesFrom(base, expect1).foreach { current =>
          withClue(s"next from $current: ") {
            if (current == expect1) {
              DateSequences.MONTHLY_1ST.next(current) shouldBe expect2
              DateSequences.MONTHLY_1ST.nth(current, 1) shouldBe expect2
              DateSequences.MONTHLY_1ST.nth(current, 2) shouldBe expect3
            } else {
              DateSequences.MONTHLY_1ST.next(current) shouldBe expect1
              DateSequences.MONTHLY_1ST.nth(current, 1) shouldBe expect1
              DateSequences.MONTHLY_1ST.nth(current, 2) shouldBe expect2
              DateSequences.MONTHLY_1ST.nth(current, 3) shouldBe expect3
            }
          }
        }
        succeed
    }
  }

  //-------------------------------------------------------------------------
  /**
   * The dates the fixture of the original was probed at, reused by `test_dummy`.
   *
   * The first three are the dates the original probed - the day before one of its
   * fixture's dates, that date itself, and the day after it - and the remaining three
   * add the calendar boundaries that the derivation of one method from another is most
   * likely to get wrong: the first day of a year, its last day, and the last day of a
   * month whose sequence date has already passed.
   */
  private val dummyProbeDates: List[LocalDate] = list(
    date(2015, 10, 14),
    date(2015, 10, 15),
    date(2015, 10, 16),
    date(2013, 1, 1),
    date(2013, 12, 31),
    date(2015, 10, 31)
  )

  /**
   * What the fixture of the original proved, proved against the members of the family.
   *
   * The original declared its own implementation of the interface - supplying only
   * `nextOrSame`, a weekly rule over three dates of October 2015 - in order to exercise
   * the four methods the interface implemented on behalf of its subtypes. That fixture
   * has no target here: [[DateSequence]] is `sealed`, so no implementation of it can be
   * declared outside its own file, which is the property that makes the family closed and
   * is asserted in `test_extendedEnum`. The two halves below prove the same thing the
   * fixture proved, without one:
   *
   *  - '''the shape of the original's eighteen assertions''', over the monthly first-day
   *    sequence. The original probed the day before one of its dates, the date itself and
   *    the day after it; the three probes below stand in exactly that relation to the
   *    first of October 2015, so every one of the eighteen expectations maps across - the
   *    first, second and third sequence dates here play the parts that the 15th, 22nd and
   *    29th of October played there.
   *  - '''the derivation the fixture existed to demonstrate'''. The four methods of the
   *    original's interface derived from one another: the next date is the date on or
   *    after the following day, the nth is the next date stepped on n - 1 times, and the
   *    nth on or after is the date on or after stepped on n - 1 times. Every member of
   *    this family overrides all four with a direct calculation, so nothing exercises
   *    the derivation - which is precisely what makes it worth asserting that each
   *    member's calculation agrees with it.
   */
  test("test_dummy") {
    val test = DateSequences.MONTHLY_1ST
    val before = date(2015, 9, 30) // the day before a date of the sequence
    val onDate = date(2015, 10, 1) // a date of the sequence
    val after = date(2015, 10, 2) // the day after a date of the sequence
    val first = date(2015, 10, 1)
    val second = date(2015, 11, 1)
    val third = date(2015, 12, 1)

    test.next(before) shouldBe first
    test.next(onDate) shouldBe second
    test.next(after) shouldBe second

    test.nextOrSame(before) shouldBe first
    test.nextOrSame(onDate) shouldBe first
    test.nextOrSame(after) shouldBe second

    test.nth(before, 1) shouldBe first
    test.nth(onDate, 1) shouldBe second
    test.nth(after, 1) shouldBe second
    test.nth(before, 2) shouldBe second
    test.nth(onDate, 2) shouldBe third
    test.nth(after, 2) shouldBe third

    test.nthOrSame(before, 1) shouldBe first
    test.nthOrSame(onDate, 1) shouldBe first
    test.nthOrSame(after, 1) shouldBe second
    test.nthOrSame(before, 2) shouldBe second
    test.nthOrSame(onDate, 2) shouldBe second
    test.nthOrSame(after, 2) shouldBe third

    DateSequence.values.toList.foreach { sequence =>
      dummyProbeDates.foreach { probe =>
        withClue(s"$sequence from $probe: ") {
          // the next date is the date on or after the following day
          sequence.next(probe) shouldBe sequence.nextOrSame(probe.plusDays(1L))
          // the nth date is the next date stepped on n - 1 times
          sequence.nth(probe, 1) shouldBe sequence.next(probe)
          sequence.nth(probe, 2) shouldBe sequence.next(sequence.next(probe))
          sequence.nth(probe, 3) shouldBe sequence.next(sequence.next(sequence.next(probe)))
          // and the nth on or after is the date on or after stepped on n - 1 times
          sequence.nthOrSame(probe, 1) shouldBe sequence.nextOrSame(probe)
          sequence.nthOrSame(probe, 2) shouldBe sequence.next(sequence.nextOrSame(probe))
          sequence.nthOrSame(probe, 3) shouldBe
            sequence.next(sequence.next(sequence.nextOrSame(probe)))
        }
      }
    }
    succeed
  }

  //-------------------------------------------------------------------------
  /**
   * The replacement for the original's interrogation of its run-time registry.
   *
   * The original asked the registry that had assembled the family from a configuration
   * resource for the complete map of the names it had loaded, and looked one name up in
   * it. There is no registry here - the members are declared in the companion and fixed
   * when it is compiled - so the equivalent assertion is made against the name tables the
   * lookup derives from those members: the map keyed by canonical name is the map the
   * original's normalised lookup returned.
   *
   * All three tables a named family may declare are empty for this family, because the
   * configuration resource of the original declared none of them: no alternate spelling
   * of any sequence, no pattern rewriting text before it is looked up, and no group of
   * names published for an external protocol. That is asserted rather than assumed, and
   * it is why there is nothing further to assert here: the name space of the family is
   * exactly its six canonical names, and a seventh name resolves to nothing.
   */
  test("test_extendedEnum") {
    val lookup = NamedEnum[DateSequence]
    lookup.byCanonicalName.get("Quarterly-IMM") shouldBe Some(DateSequences.QUARTERLY_IMM)
    lookup.byCanonicalName shouldBe Map(
      "Quarterly-IMM" -> DateSequences.QUARTERLY_IMM,
      "Quarterly-IMM-6-Serial" -> DateSequences.QUARTERLY_IMM_6_SERIAL,
      "Quarterly-IMM-3-Serial" -> DateSequences.QUARTERLY_IMM_3_SERIAL,
      "Monthly-IMM" -> DateSequences.MONTHLY_IMM,
      "Quarterly-10th" -> DateSequences.QUARTERLY_10TH,
      "Monthly-1st" -> DateSequences.MONTHLY_1ST
    )
    lookup.familyName shouldBe "DateSequence"
    // the family holds six distinct members and no more
    lookup.values.toList should have size 6
    lookup.values.toList.distinct shouldBe lookup.values.toList
    lookup.values shouldBe DateSequence.values
    // the three declarable tables are empty, as they were in the original's configuration
    lookup.alternateNames shouldBe Map.empty[String, String]
    lookup.lenientPatterns shouldBe List.empty[(scala.util.matching.Regex, String)]
    lookup.externalNameGroups shouldBe Set.empty[String]
    // so the six canonical names are the whole name space of the family
    DateSequence.valueOf("Rubbish") shouldBe None
    DateSequence.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
  }

  //-------------------------------------------------------------------------
  /**
   * The replacement for the original's two reflective coverage sweeps.
   *
   * The original swept the enumeration holding the implementations and the class holding
   * the constants, asserting reflectively that every enum constant was reachable and that
   * the constants holder could not be instantiated. Neither sweep has a target here: the
   * implementations are `case object`s of a `sealed` class, and a Scala `object` has no
   * constructor to hide. What the sweeps stood in for is asserted directly instead - that
   * the family is the closed set of six, that the constants republish exactly those six
   * under the identifiers of the original, and that the single equality-bearing instance
   * and the rendering behave by name.
   */
  test("coverage") {
    DateSequence.values.toList shouldBe list(
      DateSequences.QUARTERLY_IMM,
      DateSequences.QUARTERLY_IMM_6_SERIAL,
      DateSequences.QUARTERLY_IMM_3_SERIAL,
      DateSequences.MONTHLY_IMM,
      DateSequences.QUARTERLY_10TH,
      DateSequences.MONTHLY_1ST
    )
    DateSequence.values.toList.map(_.name) shouldBe list(
      "Quarterly-IMM",
      "Quarterly-IMM-6-Serial",
      "Quarterly-IMM-3-Serial",
      "Monthly-IMM",
      "Quarterly-10th",
      "Monthly-1st"
    )
    // every member is reachable from its own name, which is what the enum sweep of the
    // original established by reflection
    DateSequence.values.toList.foreach { sequence =>
      withClue(s"$sequence: ") {
        DateSequence.valueOf(sequence.name) shouldBe Some(sequence)
      }
    }

    // `Order` and `Hash` both extend `Eq`, so the one instance the companion publishes is
    // the only notion of equality the type has; comparison and hashing are both by name,
    // which makes the ordering alphabetical rather than the declaration order of `values`
    val firstByName = DateSequences.MONTHLY_1ST
    val lastByName = DateSequences.QUARTERLY_IMM_6_SERIAL
    Hash[DateSequence].hash(firstByName) shouldBe firstByName.name.hashCode
    Hash[DateSequence].eqv(firstByName, firstByName) shouldBe true
    Hash[DateSequence].eqv(firstByName, lastByName) shouldBe false
    Order[DateSequence].compare(firstByName, firstByName) shouldBe 0
    Order[DateSequence].compare(firstByName, lastByName) should be < 0
    Order[DateSequence].compare(lastByName, firstByName) should be > 0
    DateSequence.values.toList.sorted(Order[DateSequence].toOrdering) shouldBe list(
      DateSequences.MONTHLY_1ST,
      DateSequences.MONTHLY_IMM,
      DateSequences.QUARTERLY_10TH,
      DateSequences.QUARTERLY_IMM,
      DateSequences.QUARTERLY_IMM_3_SERIAL,
      DateSequences.QUARTERLY_IMM_6_SERIAL
    )
    Show[DateSequence].show(firstByName) shouldBe "Monthly-1st"
    Show[DateSequence].show(lastByName) shouldBe "Quarterly-IMM-6-Serial"
    // equality never holds between two members, and never against another type at all;
    // the method form is used for the second because the compiler rejects a comparison
    // between unrelated types written with the operator
    firstByName.equals(lastByName) shouldBe false
    firstByName.equals("Monthly-1st") shouldBe false
  }

  //-------------------------------------------------------------------------
  /**
   * The replacement for the original's binary serialization assertions.
   *
   * The original round-tripped two sequences through Java serialization. This port has no
   * Java serialization and no reflective serialization framework: a sequence travels as
   * JSON, written by the codec its companion publishes, and the form is the bare string
   * of its canonical name rather than an object with a field in it. Both halves matter,
   * so the shape is asserted as well as the round trip - an encoding that became an
   * object would still round-trip, and would still be wrong.
   */
  test("test_serialization") {
    list(DateSequences.QUARTERLY_IMM, DateSequences.MONTHLY_IMM).foreach { sequence =>
      withClue(s"$sequence: ") {
        val json: Json = sequence.asJson
        json shouldBe Json.fromString(sequence.name)
        json.isString shouldBe true
        json.isObject shouldBe false
        json.as[DateSequence] shouldBe Right(sequence)
      }
    }
    DateSequences.QUARTERLY_IMM.asJson.noSpaces shouldBe "\"Quarterly-IMM\""
    DateSequences.MONTHLY_IMM.asJson.noSpaces shouldBe "\"Monthly-IMM\""
  }

  //-------------------------------------------------------------------------
  /**
   * The replacement for the original's string-conversion assertions.
   *
   * The original checked that the annotation-driven string converter of its serialization
   * framework rendered a sequence and read it back. That framework is gone; rendering is
   * the `Show` instance and reading back is `parse`, and this asserts that the two are
   * inverse over the two sequences the original named, with the rendered text pinned to
   * the exact names the original produced so that text written by it still resolves here.
   */
  test("test_jodaConvert") {
    list(DateSequences.QUARTERLY_IMM, DateSequences.MONTHLY_IMM).foreach { sequence =>
      withClue(s"$sequence: ") {
        val rendered = Show[DateSequence].show(sequence)
        rendered shouldBe sequence.name
        rendered shouldBe sequence.toString
        DateSequence.parse(rendered) should haveValue(sequence)
      }
    }
    Show[DateSequence].show(DateSequences.QUARTERLY_IMM) shouldBe "Quarterly-IMM"
    Show[DateSequence].show(DateSequences.MONTHLY_IMM) shouldBe "Monthly-IMM"
  }
}
