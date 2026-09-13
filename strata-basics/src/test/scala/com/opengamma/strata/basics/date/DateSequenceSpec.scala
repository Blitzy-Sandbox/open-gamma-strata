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
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor1, TableFor2, TableFor4}

import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper._

/**
 * Test [[DateSequence]].
 *
 * The stepping tests come in `nextOrSame`/`next` pairs: the two differ on exactly one class of
 * input, a date that is itself a date of the sequence, which `nextOrSame` returns and `next`
 * steps over. Each pair derives different expectations from one table row, so the `next` walks
 * start at the base date itself and the `nextOrSame` walks at the day after it.
 */
final class DateSequenceSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The days from `start` to `end` inclusive, in ascending order - lazy and finite, the
   * unbounded iteration being cut by the stopping condition, and empty when `start` is already
   * after `end`.
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
  // The two serial sequences are halves of a pair whose base is the quarterly sequence; the
  // other four are their own base.
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
   * A base date and the three quarterly IMM dates that follow it - the third Wednesday of the
   * next March, June, September or December, then the next two. Each row's base date is the
   * previous row's first expected date, so a single shifted date shows up in two rows, not one.
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
        // the whole walk answers with the first expected date, its last day being that date
        datesFrom(base.plusDays(1L), immDate1).foreach { current =>
          withClue(s"nextOrSame from $current: ") {
            DateSequences.QUARTERLY_IMM.nextOrSame(current) shouldBe immDate1
            DateSequences.QUARTERLY_IMM.nthOrSame(current, 1) shouldBe immDate1
            DateSequences.QUARTERLY_IMM.nthOrSame(current, 2) shouldBe immDate2
            DateSequences.QUARTERLY_IMM.nthOrSame(current, 3) shouldBe immDate3
          }
        }
        // `dateMatching` associates a month with the sequence date of that month; for every
        // row of this table the day after the first expected date lies in that same month
        val monthOfFirst = YearMonth.from(immDate1.plusDays(1L))
        DateSequences.QUARTERLY_IMM.dateMatching(monthOfFirst) shouldBe immDate1
    }
  }

  test("test_nextQuarterlyImm") {
    forAll(data_quarterlyImm) {
      (base: LocalDate, immDate1: LocalDate, immDate2: LocalDate, immDate3: LocalDate) =>
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
   * Base dates alone; the expectations are the dates the monthly and quarterly sequences give
   * from them. The six straddle the January, February and March IMM dates of 2013 - two per
   * month, one either side of its IMM date - which exercises the boundary where the serial count
   * moves on a month.
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
        // the first six dates are the IMM dates of six consecutive months: the monthly sequence
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
        // those six subsume the first two quarterly dates, so from the seventh the sequence is
        // quarterly again with its count four lower
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
   * The same six base dates as the six-serial table, kept separate because the two feed
   * different sequences: merging them would tie the coverage of one to the coverage of the other.
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
        // the first three dates are the IMM dates of three consecutive months: the monthly one
        DateSequences.QUARTERLY_IMM_3_SERIAL.nextOrSame(base) shouldBe
          DateSequences.MONTHLY_IMM.nextOrSame(base)
        DateSequences.QUARTERLY_IMM_3_SERIAL.nthOrSame(base, 1) shouldBe
          DateSequences.MONTHLY_IMM.nthOrSame(base, 1)
        DateSequences.QUARTERLY_IMM_3_SERIAL.nthOrSame(base, 2) shouldBe
          DateSequences.MONTHLY_IMM.nthOrSame(base, 2)
        DateSequences.QUARTERLY_IMM_3_SERIAL.nthOrSame(base, 3) shouldBe
          DateSequences.MONTHLY_IMM.nthOrSame(base, 3)
        // those three subsume the first quarterly date, so from the fourth the sequence is
        // quarterly again with its count two lower
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
   * A base date and the three monthly IMM dates that follow it. The rows chain as the quarterly
   * rows do, and this chain crosses a year end, which is where a month-arithmetic error surfaces.
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
   * A base date and the three sequence dates that follow it - the tenth day of the next March,
   * June, September or December, then the next two. The base dates are the quarterly IMM dates
   * rather than tenths, so every row starts from a date deliberately not in this sequence.
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
   * A base date and the first days of the next three months. The first two rows differ only in
   * their base date - the first of January, itself a date of this sequence, and the second, the
   * day after one - and carry the same expectations, which is what separates the two tests they
   * drive: for the first row `nextOrSame` answers the base date, `next` the one after it.
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
   * The dates every member is probed at, here and in `test_nth_sequenceNumberNotPositive`: three
   * consecutive days in mid-October 2015, so neighbouring probes differ by the one day the
   * `next`/`nextOrSame` derivation turns on, plus the calendar boundaries that derivation is most
   * likely to get wrong - a year start (itself a date of the monthly first-day sequence), a year
   * end, and a month end later than any sequence date in its month.
   */
  private val dummyProbeDates: List[LocalDate] = list(
    date(2015, 10, 14),
    date(2015, 10, 15),
    date(2015, 10, 16),
    date(2013, 1, 1),
    date(2013, 12, 31),
    date(2015, 10, 31)
  )

  // Every member overrides all four stepping methods with a direct calculation, so nothing else
  // here exercises the derivation identities; they are asserted over every member and probe date.
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
   * The two sequence numbers that name no date, each with the text `ArgCheck.notNegativeOrZero`
   * builds for it: a sequence number is 1-based, so zero and every negative number name no date
   * of any sequence. The message is asserted character for character rather than by a fragment a
   * differently-worded refusal would also satisfy, and -1 sits immediately below the boundary, so
   * a guard written `< 0` where `<= 0` was meant is told apart from a missing one.
   */
  private val data_rejectedSequenceNumbers: TableFor2[Int, String] = Table(
    ("sequenceNumber", "message"),
    (0, "Argument 'sequenceNumber' must not be negative or zero but has value 0"),
    (-1, "Argument 'sequenceNumber' must not be negative or zero but has value -1")
  )

  // A non-positive sequence number is a broken precondition of the call rather than a property of
  // the data, so both methods fail fast with `IllegalArgumentException` from `ArgCheck` instead
  // of returning a `Failure` - unlike `SequenceDate.of`, which validates one that arrived as
  // data. Each of the six members restates the guard in both methods, so twelve guards exist and
  // nothing else here supplies a non-positive number: without this test one could be deleted
  // while the suite stayed green.
  test("test_nth_sequenceNumberNotPositive") {
    forAll(data_rejectedSequenceNumbers) { (sequenceNumber: Int, message: String) =>
      DateSequence.values.toList.foreach { sequence =>
        dummyProbeDates.foreach { probe =>
          withClue(s"$sequence nth from $probe with $sequenceNumber: ") {
            val thrownByNth =
              intercept[IllegalArgumentException](sequence.nth(probe, sequenceNumber))
            thrownByNth.getMessage shouldBe message
          }
          withClue(s"$sequence nthOrSame from $probe with $sequenceNumber: ") {
            val thrownByNthOrSame =
              intercept[IllegalArgumentException](sequence.nthOrSame(probe, sequenceNumber))
            thrownByNthOrSame.getMessage shouldBe message
          }
        }
      }
      succeed
    }
    // the positive controls make the assertion two-sided: the smallest sequence number the guard
    // admits, and the one after it, answer with the dates the two stepping methods reach
    DateSequence.values.toList.foreach { sequence =>
      dummyProbeDates.foreach { probe =>
        withClue(s"$sequence from $probe with a positive sequence number: ") {
          sequence.nth(probe, 1) shouldBe sequence.next(probe)
          sequence.nthOrSame(probe, 1) shouldBe sequence.nextOrSame(probe)
          sequence.nth(probe, 2) shouldBe sequence.next(sequence.next(probe))
          sequence.nthOrSame(probe, 2) shouldBe sequence.next(sequence.nextOrSame(probe))
        }
      }
    }
    succeed
  }

  //-------------------------------------------------------------------------
  // `byCanonicalName` is the map keyed by canonical name. The family declares none of the three
  // optional name tables - no alternate spelling, no lenient pattern, no external name group -
  // asserted rather than assumed, because that is what makes its six canonical names the whole
  // name space, so a seventh name resolves to nothing.
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
    lookup.values.toList should have size 6
    lookup.values.toList.distinct shouldBe lookup.values.toList
    lookup.values shouldBe DateSequence.values
    lookup.alternateNames shouldBe Map.empty[String, String]
    lookup.lenientPatterns shouldBe List.empty[(scala.util.matching.Regex, String)]
    lookup.externalNameGroups shouldBe Set.empty[String]
    DateSequence.valueOf("Rubbish") shouldBe None
    DateSequence.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
  }

  //-------------------------------------------------------------------------
  // The family is the closed set of six, `DateSequences` republishes exactly those six, and
  // equality, ordering and rendering are by name - so the ordering is alphabetical rather than
  // the declaration order of `values`.
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
    DateSequence.values.toList.foreach { sequence =>
      withClue(s"$sequence: ") {
        DateSequence.valueOf(sequence.name) shouldBe Some(sequence)
      }
    }

    // `Order` and `Hash` both extend `Eq`, so one instance carries equality, hashing and order
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
    // `equals` is called as a method because the compiler rejects `==` between unrelated types
    firstByName.equals(lastByName) shouldBe false
    firstByName.equals("Monthly-1st") shouldBe false
  }

  //-------------------------------------------------------------------------
  // A sequence travels as the bare string of its canonical name rather than as an object with a
  // field in it, so the shape is asserted as well as the round trip: an encoding that became an
  // object would still round-trip, and would still be wrong.
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
  // Rendering is the `Show` instance and reading back is `parse`; the two are inverse, with the
  // rendered text pinned to the exact names so that text carrying them still resolves.
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
