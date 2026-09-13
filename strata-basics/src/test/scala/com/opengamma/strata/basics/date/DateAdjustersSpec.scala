/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor4}

/**
 * Test [[DateAdjusters]].
 *
 * Each adjuster is driven through both `adjust` and the `java.time.temporal.TemporalAdjuster`
 * supertype that [[DateAdjuster]] implements with its `adjustInto` default.
 */
class DateAdjustersSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  private val data_nextLeapDay: TableFor4[Int, Int, Int, Int] = Table(
    ("year", "month", "day", "expectedYear"),
    (2000, 1, 1, 2000),
    (2000, 2, 1, 2000),
    (2000, 2, 28, 2000),
    (2000, 2, 29, 2004),
    (2000, 3, 1, 2004),
    (2009, 1, 1, 2012),
    (2009, 2, 1, 2012),
    (2009, 2, 28, 2012),
    (2009, 3, 1, 2012),
    (2010, 1, 1, 2012),
    (2010, 2, 1, 2012),
    (2010, 2, 28, 2012),
    (2010, 3, 1, 2012),
    (2012, 1, 1, 2012),
    (2012, 2, 1, 2012),
    (2012, 2, 28, 2012),
    (2012, 2, 29, 2016),
    (2012, 3, 1, 2016),
    (2013, 1, 1, 2016),
    (2013, 2, 1, 2016),
    (2013, 2, 28, 2016),
    (2013, 3, 1, 2016),
    (2014, 1, 1, 2016),
    (2014, 2, 1, 2016),
    (2014, 2, 28, 2016),
    (2014, 3, 1, 2016),
    (2015, 1, 1, 2016),
    (2015, 2, 1, 2016),
    (2015, 2, 28, 2016),
    (2015, 3, 1, 2016),
    (2016, 1, 1, 2016),
    (2016, 2, 1, 2016),
    (2016, 2, 28, 2016),
    (2016, 2, 29, 2020),
    (2016, 3, 1, 2020),
    (2017, 1, 1, 2020),
    // A February 29 rolls on to the *next* leap year, and 2100 is divisible by four but not by
    // 400 and so is not a leap year: an implementation that rounds up to the next multiple of
    // four passes every other row here and fails these nine.
    (2096, 1, 1, 2096),
    (2096, 2, 1, 2096),
    (2096, 2, 28, 2096),
    (2096, 2, 29, 2104),
    (2096, 3, 1, 2104),
    (2100, 1, 1, 2104),
    (2100, 2, 1, 2104),
    (2100, 2, 28, 2104),
    (2100, 3, 1, 2104)
  )

  /**
   * The expectation for `nextOrSameLeapDay`, derived from the shared table rather than tabulated
   * again: `expectedYear` is the year of the *next* leap day, and the two adjusters differ on one
   * class of input only - a February 29 is already a leap day and is returned unaltered.
   */
  private def expectedNextOrSame(date: LocalDate, expectedYear: Int): LocalDate =
    if (date.getMonthValue == 2 && date.getDayOfMonth == 29) {
      date
    } else {
      LocalDate.of(expectedYear, 2, 29)
    }

  //-------------------------------------------------------------------------
  test("test_nextLeapDay_LocalDate") {
    forAll(data_nextLeapDay) { (year: Int, month: Int, day: Int, expectedYear: Int) =>
      val date = LocalDate.of(year, month, day)
      DateAdjusters.nextLeapDay.adjust(date) shouldBe LocalDate.of(expectedYear, 2, 29)
    }
  }

  test("test_nextLeapDay_Temporal") {
    forAll(data_nextLeapDay) { (year: Int, month: Int, day: Int, expectedYear: Int) =>
      val date = LocalDate.of(year, month, day)
      // `LocalDate.with` routes to `DateAdjuster.adjustInto`, so this variant exercises the
      // `TemporalAdjuster` side of the contract. `with` is a Scala keyword, hence the back-ticks.
      date.`with`(DateAdjusters.nextLeapDay) shouldBe LocalDate.of(expectedYear, 2, 29)
    }
  }

  //-------------------------------------------------------------------------
  test("test_nextOrSameLeapDay_LocalDate") {
    forAll(data_nextLeapDay) { (year: Int, month: Int, day: Int, expectedYear: Int) =>
      val date = LocalDate.of(year, month, day)
      DateAdjusters.nextOrSameLeapDay.adjust(date) shouldBe expectedNextOrSame(date, expectedYear)
    }
  }

  test("test_nextOrSameLeapDay_Temporal") {
    forAll(data_nextLeapDay) { (year: Int, month: Int, day: Int, expectedYear: Int) =>
      val date = LocalDate.of(year, month, day)
      // routed through `adjustInto` by `LocalDate.with`, as above
      date.`with`(DateAdjusters.nextOrSameLeapDay) shouldBe expectedNextOrSame(date, expectedYear)
    }
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    forAll(data_nextLeapDay) { (year: Int, month: Int, day: Int, expectedYear: Int) =>
      val date = LocalDate.of(year, month, day)
      val next = DateAdjusters.nextLeapDay.adjust(date)
      val nextOrSame = DateAdjusters.nextOrSameLeapDay.adjust(date)
      val inputIsLeapDay = month == 2 && day == 29
      withClue(s"$date: next=$next, nextOrSame=$nextOrSame, expected leap year $expectedYear: ") {
        assert(
          // a second application to the same date gives the same answer
          DateAdjusters.nextLeapDay.adjust(date) == next &&
            DateAdjusters.nextOrSameLeapDay.adjust(date) == nextOrSame &&
            // both adjusters land on a February 29
            next.getMonthValue == 2 && next.getDayOfMonth == 29 &&
            nextOrSame.getMonthValue == 2 && nextOrSame.getDayOfMonth == 29 &&
            // and they differ on exactly one class of input: a leap day, returned unaltered by
            // the "or same" adjuster and rolled forward by the other
            (if (inputIsLeapDay) next != nextOrSame else next == nextOrSame)
        )
      }
    }
  }
}
