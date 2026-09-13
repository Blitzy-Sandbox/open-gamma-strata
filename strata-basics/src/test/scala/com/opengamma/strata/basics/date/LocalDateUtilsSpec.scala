/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/**
 * Test [[LocalDateUtils]].
 *
 * Each hand-written fast path is compared with the equivalent `java.time.LocalDate` operation over
 * a long contiguous run of dates from the first day of a leap year, so every day-of-month, every
 * month length and every leap-year boundary is exercised rather than a hand-picked sample. The runs
 * - four years for the single-date helpers, eight for `daysBetween` - are deliberately long, these
 * helpers sitting under every ACT-family year fraction and every holiday-calendar scan.
 */
class LocalDateUtilsSpec extends AnyFunSuite with Matchers {

  private val start: LocalDate = LocalDate.of(2012, 1, 1)

  private val walkLength: Int = 366 * 4

  private val daysBetweenLength: Int = 366 * 8

  /**
   * Applies a check to every date of a contiguous ascending range, naming the failing date in the
   * clue. The range is produced with the JDK's own `plusDays`, deliberately not with the helper
   * under test, so a defect in a helper does not conceal itself by also corrupting the walk. The
   * fold evaluates every assertion in order and yields the last, leaving the traversal without a
   * mutable cursor or a discarded result; the `succeed` seed keeps a zero-length range well typed.
   */
  private def forEachDateFrom(first: LocalDate, dayCount: Int)(check: LocalDate => Assertion): Assertion =
    Iterator
      .iterate(first)(_.plusDays(1L))
      .take(dayCount)
      .foldLeft(succeed) { (_, date) =>
        withClue(s"date $date: ") {
          check(date)
        }
      }

  /**
   * Checks `plusDays` against the JDK for one offset, over the whole range. The offset is converted
   * explicitly for the JDK method, which takes a `Long`: this build rejects an implicit numeric
   * widening and treats warnings as errors.
   */
  private def assertPlusDays(daysToAdd: Int): Assertion =
    forEachDateFrom(start, walkLength) { date =>
      LocalDateUtils.plusDays(date, daysToAdd) shouldBe date.plusDays(daysToAdd.toLong)
    }

  test("test_dayOfYear") {
    forEachDateFrom(start, walkLength) { date =>
      LocalDateUtils.doy(date) shouldBe date.getDayOfYear
    }
  }

  test("test_plusDays0") {
    assertPlusDays(0)
  }

  test("test_plusDays1") {
    assertPlusDays(1)
  }

  test("test_plusDays3") {
    assertPlusDays(3)
  }

  test("test_plusDays99") {
    assertPlusDays(99)
  }

  test("test_plusDaysM1") {
    assertPlusDays(-1)
  }

  test("test_daysBetween") {
    val base = start
    forEachDateFrom(base, daysBetweenLength) { date =>
      // Both sides are already `Long`, so the comparison performs no widening.
      LocalDateUtils.daysBetween(base, date) shouldBe (date.toEpochDay - base.toEpochDay)
    }
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // `isLeapYear` and `dates` are the two members the walks above do not reach.
    LocalDateUtils.isLeapYear(2000) shouldBe true
    LocalDateUtils.isLeapYear(2012) shouldBe true
    LocalDateUtils.isLeapYear(1900) shouldBe false
    LocalDateUtils.isLeapYear(2011) shouldBe false
    LocalDateUtils.isLeapYear(2100) shouldBe false

    // `dates` is half-open: an empty range yields nothing and a three-day range yields the start
    // date and the two days after it.
    val sample = LocalDate.of(2015, 3, 31)
    LocalDateUtils.dates(sample, sample).toList shouldBe List.empty[LocalDate]
    LocalDateUtils.dates(sample, sample.plusDays(3L)).toList shouldBe
      List(sample, sample.plusDays(1L), sample.plusDays(2L))

    // Each expectation is written both as a literal and against the `java.time` operation the
    // helper is a fast path for; two calls of the same helper compared with each other would hold
    // for a wrong implementation too.
    LocalDateUtils.doy(sample) shouldBe 90
    LocalDateUtils.doy(sample) shouldBe sample.getDayOfYear
    LocalDateUtils.plusDays(sample, 45) shouldBe LocalDate.of(2015, 5, 15)
    LocalDateUtils.plusDays(sample, 45) shouldBe sample.plusDays(45L)
    LocalDateUtils.daysBetween(sample, sample.plusDays(45L)) shouldBe 45L
    LocalDateUtils.daysBetween(sample, sample.plusDays(45L)) shouldBe
      (sample.plusDays(45L).toEpochDay - sample.toEpochDay)

    // `dates` is the one member for which comparing two calls asserts something no single-call
    // expectation can: its result is a single-use iterator, so reading the same list out of it
    // twice succeeds only if each call hands out a fresh iterator.
    LocalDateUtils.dates(sample, sample.plusDays(3L)).toList shouldBe
      LocalDateUtils.dates(sample, sample.plusDays(3L)).toList
  }

}
