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
 * The Java original drove four parameterised methods from a single data provider,
 * `data_nextLeapDay`. That shape is preserved exactly: the provider becomes one
 * shared table, declared once, and each of the four methods keeps its own test with
 * its own name, so the method-level traceability of the migration retains a
 * one-to-one mapping rather than collapsing four cases into one.
 *
 * Two of the four methods drive the adjuster through its `adjust` method and two
 * drive it through the `java.time.temporal.TemporalAdjuster` contract, which is why
 * [[DateAdjuster]] keeps that supertype and its `adjustInto` default.
 */
class DateAdjustersSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The shared provider, transcribed row-for-row from the Java data provider.
   *
   * Each row is an input date - given as year, month and day so that the "is this
   * February 29?" cases are visible in the table itself - together with the year of
   * the February 29 that `nextLeapDay` returns for it.
   *
   * The blocks are the Java blocks, in the Java order. Three of them carry the cases
   * that the rule is easy to get wrong:
   *
   *  - a February 29 input moves on to the *next* leap year (2000-02-29 to 2004,
   *    2012-02-29 to 2016, 2016-02-29 to 2020), which is what separates
   *    `nextLeapDay` from `nextOrSameLeapDay`;
   *  - a date before February 29 of a leap year stays inside that same year
   *    (2000-01-01 to 2000, 2012-02-28 to 2012);
   *  - the 2096 and 2100 blocks are the only coverage of the century rule. 2100 is
   *    divisible by four but not by 400, so it is not a leap year: an
   *    implementation that simply rounds up to the next multiple of four passes
   *    every other row here and fails these nine.
   */
  private val data_nextLeapDay: TableFor4[Int, Int, Int, Int] = Table(
    ("year", "month", "day", "expectedYear"),
    (2000, 1, 1, 2000),
    (2000, 2, 1, 2000),
    (2000, 2, 28, 2000),
    (2000, 2, 29, 2004),
    (2000, 3, 1, 2004),
    // 2009 - not a leap year, every date rolls forward to 2012
    (2009, 1, 1, 2012),
    (2009, 2, 1, 2012),
    (2009, 2, 28, 2012),
    (2009, 3, 1, 2012),
    // 2010 - not a leap year, every date rolls forward to 2012
    (2010, 1, 1, 2012),
    (2010, 2, 1, 2012),
    (2010, 2, 28, 2012),
    (2010, 3, 1, 2012),
    // 2012 - a leap year, so dates up to February 28 stay in 2012
    (2012, 1, 1, 2012),
    (2012, 2, 1, 2012),
    (2012, 2, 28, 2012),
    (2012, 2, 29, 2016),
    (2012, 3, 1, 2016),
    // 2013, 2014, 2015 - not leap years, every date rolls forward to 2016
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
    // 2016 - a leap year, so dates up to February 28 stay in 2016
    (2016, 1, 1, 2016),
    (2016, 2, 1, 2016),
    (2016, 2, 28, 2016),
    (2016, 2, 29, 2020),
    (2016, 3, 1, 2020),
    (2017, 1, 1, 2020),
    // 2096 - a leap year; after it the century rule bites, so its leap day and the
    // days after it skip 2100 altogether and land on 2104
    (2096, 1, 1, 2096),
    (2096, 2, 1, 2096),
    (2096, 2, 28, 2096),
    (2096, 2, 29, 2104),
    (2096, 3, 1, 2104),
    // 2100 - divisible by four but not by 400, hence not a leap year: there is no
    // 2100-02-29 to return and every date in it rolls forward to 2104
    (2100, 1, 1, 2104),
    (2100, 2, 1, 2104),
    (2100, 2, 28, 2104),
    (2100, 3, 1, 2104)
  )

  /**
   * The expectation for the "or same" adjuster, derived from a row of the shared
   * table exactly as the Java test derived it.
   *
   * The provider is shared between the two adjusters, and its `expectedYear` column
   * is the *next* leap day. The "or same" adjuster differs on one class of input
   * only: February 29 is already a leap day, so it is returned unaltered rather than
   * rolled forward. Every other input has the same expectation as `nextLeapDay`.
   *
   * @param date  the input date of the row
   * @param expectedYear  the `expectedYear` column of the row
   * @return the date `nextOrSameLeapDay` must return for that input
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
      // comparing the whole date asserts the year, the month and the day of month
      // together, which is what the three separate Java assertions asserted
      DateAdjusters.nextLeapDay.adjust(date) shouldBe LocalDate.of(expectedYear, 2, 29)
    }
  }

  test("test_nextLeapDay_Temporal") {
    forAll(data_nextLeapDay) { (year: Int, month: Int, day: Int, expectedYear: Int) =>
      val date = LocalDate.of(year, month, day)
      // `LocalDate.with(TemporalAdjuster)` delegates to `DateAdjuster.adjustInto`, so
      // this variant exercises the `java.time.temporal.TemporalAdjuster` side of the
      // contract rather than `adjust` directly - which is precisely why the ported
      // trait keeps that supertype. `with` is a Scala keyword, hence the back-ticks.
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
      // as above: routed through `adjustInto` by `LocalDate.with`
      date.`with`(DateAdjusters.nextOrSameLeapDay) shouldBe expectedNextOrSame(date, expectedYear)
    }
  }

  //-------------------------------------------------------------------------
  // The Java test asserted reflectively that the constants holder was a final class
  // with a private constructor; a Scala `object` offers that assertion no target, so
  // the properties it stood in for - that the adjusters are pure, stateless and
  // total - are asserted directly instead, over every row of the shared table.
  test("coverage") {
    forAll(data_nextLeapDay) { (year: Int, month: Int, day: Int, expectedYear: Int) =>
      val date = LocalDate.of(year, month, day)
      val next = DateAdjusters.nextLeapDay.adjust(date)
      val nextOrSame = DateAdjusters.nextOrSameLeapDay.adjust(date)
      val inputIsLeapDay = month == 2 && day == 29
      withClue(s"$date: next=$next, nextOrSame=$nextOrSame, expected leap year $expectedYear: ") {
        assert(
          // stateless and referentially transparent: neither adjuster holds state, so
          // a second application to the same date gives the same answer
          DateAdjusters.nextLeapDay.adjust(date) == next &&
            DateAdjusters.nextOrSameLeapDay.adjust(date) == nextOrSame &&
            // both adjusters always land on a February 29, for every input
            next.getMonthValue == 2 && next.getDayOfMonth == 29 &&
            nextOrSame.getMonthValue == 2 && nextOrSame.getDayOfMonth == 29 &&
            // and they differ on exactly one class of input - a leap day, which the
            // "or same" adjuster returns unaltered while the other rolls it forward
            (if (inputIsLeapDay) next != nextOrSame else next == nextOrSame)
        )
      }
    }
  }
}
