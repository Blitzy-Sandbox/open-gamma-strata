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
 * Test of [[LocalDateUtils]], ported from the Java `LocalDateUtilsTest`.
 *
 * The strategy of the original is preserved exactly, because it is what gives the test its
 * value: each hand-written fast path is compared with the equivalent `java.time.LocalDate`
 * operation over a long contiguous run of dates, so every day-of-month, every month length and
 * every leap-year boundary in the range is exercised rather than a hand-picked sample. The runs
 * start on 2012-01-01 - the first day of a leap year - and span four years for the single-date
 * helpers and eight years for `daysBetween`, which are the bounds of the Java test.
 *
 * Those bounds are named constants below and are deliberately not reduced: these helpers sit
 * underneath every ACT-family year fraction and every holiday-calendar scan, so a shorter run
 * would silently weaken the safety net that the day-count parity fixture relies on.
 *
 * The subject is a `private[basics] object`; this suite is in a sub-package of
 * `com.opengamma.strata.basics`, so it reaches the object directly, with no widening of its
 * visibility and no accessor shim.
 */
class LocalDateUtilsSpec extends AnyFunSuite with Matchers {

  /** The first date of every walked range, as in the Java test: the start of a leap year. */
  private val start: LocalDate = LocalDate.of(2012, 1, 1)

  /** The number of dates walked by the day-of-year and `plusDays` tests: four years' worth. */
  private val walkLength: Int = 366 * 4

  /** The number of dates walked by the `daysBetween` test: eight years' worth. */
  private val daysBetweenLength: Int = 366 * 8

  /**
   * Applies a check to every date of a contiguous ascending range, failing on the first date
   * whose check fails and naming that date in the failure clue - the information the Java loop
   * variable would otherwise have hidden.
   *
   * The range itself is produced with the JDK's own `plusDays`, deliberately not with the helper
   * under test, so that a defect in the port cannot conceal itself by also corrupting the walk.
   * The fold evaluates every assertion in order and yields the last one, which keeps the
   * traversal free of both a mutable cursor and a discarded result; `succeed` is the seed, so a
   * range of length zero would still be a well-typed (if vacuous) assertion rather than an
   * exception.
   *
   * @param first  the first date to check
   * @param dayCount  the number of consecutive dates to check
   * @param check  the assertion to make about each date
   * @return the assertion for the last date of the range
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
   * Checks `plusDays` against the JDK for a single offset, over the whole range.
   *
   * The offset is passed to the helper as the `Int` its signature declares and converted
   * explicitly for the JDK method, which takes a `Long`: this build rejects an implicit numeric
   * widening, and treats every warning as an error.
   *
   * @param daysToAdd  the offset to apply to every date of the range, which may be negative
   * @return the assertion for the last date of the range
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
      // Both sides are `Long`, so the comparison needs no conversion and performs no widening.
      LocalDateUtils.daysBetween(base, date) shouldBe (date.toEpochDay - base.toEpochDay)
    }
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // Java's `assertUtilityClass(LocalDateUtils.class)` has no target here: the port is a Scala
    // `object`, so it is a stateless singleton by construction and cannot be instantiated or
    // given a second instance, and the reflective check has nothing left to prove. What that
    // assertion stood for - a helper that holds no state and answers only from its arguments -
    // is asserted directly instead, along with the two members the walks above do not reach.
    LocalDateUtils.isLeapYear(2000) shouldBe true
    LocalDateUtils.isLeapYear(2012) shouldBe true
    LocalDateUtils.isLeapYear(1900) shouldBe false
    LocalDateUtils.isLeapYear(2011) shouldBe false
    LocalDateUtils.isLeapYear(2100) shouldBe false

    // `dates` is half-open, so an empty range yields nothing and a three-day range yields the
    // start date and the two days after it. The iterator is consumed as a Scala collection.
    val sample = LocalDate.of(2015, 3, 31)
    LocalDateUtils.dates(sample, sample).toList shouldBe List.empty[LocalDate]
    LocalDateUtils.dates(sample, sample.plusDays(3L)).toList shouldBe
      List(sample, sample.plusDays(1L), sample.plusDays(2L))

    // Every helper answers identically when called twice with the same argument, so none of them
    // depends on state left behind by an earlier call - including `dates`, whose iterator is
    // single-use while the method itself hands out a fresh one on each call.
    LocalDateUtils.doy(sample) shouldBe LocalDateUtils.doy(sample)
    LocalDateUtils.plusDays(sample, 45) shouldBe LocalDateUtils.plusDays(sample, 45)
    LocalDateUtils.daysBetween(sample, sample.plusDays(45L)) shouldBe
      LocalDateUtils.daysBetween(sample, sample.plusDays(45L))
    LocalDateUtils.dates(sample, sample.plusDays(3L)).toList shouldBe
      LocalDateUtils.dates(sample, sample.plusDays(3L)).toList
  }

}
