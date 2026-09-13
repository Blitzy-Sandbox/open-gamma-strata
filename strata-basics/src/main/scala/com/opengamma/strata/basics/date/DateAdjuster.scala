/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.temporal.{Temporal, TemporalAdjuster}
import java.time.{LocalDate, Year}

/**
 * Functional interface that can adjust a date.
 *
 * This extends [[java.time.temporal.TemporalAdjuster]] for those cases where the temporal
 * to be adjusted is an ISO-8601 date.
 *
 * The trait declares a single abstract method, so a plain function literal can be written
 * wherever a `DateAdjuster` is expected:
 *
 * {{{
 * val nextDay: DateAdjuster = date => date.plusDays(1)
 * }}}
 *
 * The same conversion is available explicitly through [[DateAdjuster.apply]], which is the
 * preferred form when the target type is not already fixed by the surrounding context.
 *
 * Implementations are expected to be immutable, referentially transparent and thread-safe.
 */
trait DateAdjuster extends TemporalAdjuster {

  /**
   * Adjusts the date according to the rules of the implementation.
   *
   * Implementations must specify how the date is adjusted.
   *
   * @param date the date to adjust
   * @return the adjusted date
   * @throws java.time.DateTimeException if unable to make the adjustment
   * @throws java.lang.ArithmeticException if numeric overflow occurs
   */
  def adjust(date: LocalDate): LocalDate

  /**
   * Adjusts the temporal according to the rules of the implementation.
   *
   * This method implements [[java.time.temporal.TemporalAdjuster]] by calling
   * [[DateAdjuster.adjust]]. Note that conversion to `LocalDate` ignores the calendar
   * system of the input, which is the desired behaviour in this case.
   *
   * @param temporal the temporal to adjust
   * @return the adjusted temporal
   * @throws java.time.DateTimeException if unable to make the adjustment
   * @throws java.lang.ArithmeticException if numeric overflow occurs
   */
  override def adjustInto(temporal: Temporal): Temporal =
    // conversion to LocalDate ensures that other calendar systems are ignored;
    // `with` is a Scala keyword, hence the back-ticks around the java.time call
    temporal.`with`(adjust(LocalDate.from(temporal)))
}

/**
 * Companion of [[DateAdjuster]], providing the conversion from a plain date function.
 */
object DateAdjuster {

  /**
   * Obtains a date adjuster that delegates to the specified function.
   *
   * This makes the conversion from a function to a `DateAdjuster` explicit, so that call
   * sites do not have to rely on the expected type being inferred at the point of use.
   * The returned adjuster is as immutable and thread-safe as the supplied function.
   *
   * @param f the function that adjusts the date
   * @return an adjuster that applies the function
   */
  def apply(f: LocalDate => LocalDate): DateAdjuster = (date: LocalDate) => f(date)
}

/**
 * Date adjusters that perform useful operations on `LocalDate`.
 *
 * This is a utility object; the adjusters it returns are immutable and thread-safe.
 *
 * Each adjuster is paired with the pure function it delegates to: the no-argument methods
 * yield a reusable [[DateAdjuster]] to hand to code that takes one, while the single-argument
 * methods of the same name apply the rule directly to a date and can therefore be composed
 * like any other function.
 */
object DateAdjusters {

  /**
   * The shared adjuster returned by the no-argument `nextLeapDay`.
   *
   * The adjuster holds no state beyond the rule itself, so a single instance is created
   * once and handed out on every call rather than allocated per invocation.
   */
  private val NextLeapDayAdjuster: DateAdjuster = DateAdjuster(input => nextLeapDay(input))

  private val NextOrSameLeapDayAdjuster: DateAdjuster = DateAdjuster(input => nextOrSameLeapDay(input))

  /**
   * Obtains an instance that finds the next leap day after the input date.
   *
   * The adjuster returns the next occurrence of February 29 after the input date.
   *
   * @return an adjuster that finds the next leap day
   */
  def nextLeapDay: DateAdjuster = NextLeapDayAdjuster

  /**
   * Finds the next leap day after the input date.
   *
   * The result is always a February 29 that is strictly after the input date. A date that
   * is itself a leap day moves on by four years, or by eight when the intervening
   * candidate year is a non-leap century year such as 2100.
   *
   * @param input the input date
   * @return the next leap day date
   */
  def nextLeapDay(input: LocalDate): LocalDate =
    // already a leap day, move forward either 4 or 8 years
    if (input.getMonthValue == 2 && input.getDayOfMonth == 29) {
      ensureLeapDay(input.getYear + 4)
    } else if (input.isLeapYear && input.getMonthValue <= 2) {
      // handle if before February 29 in a leap year
      LocalDate.of(input.getYear, 2, 29)
    } else {
      // handle any other date
      ensureLeapDay(((input.getYear / 4) * 4) + 4)
    }

  /**
   * Obtains a date adjuster that finds the next leap day on or after the input date.
   *
   * If the input date is February 29, the input date is returned unaltered.
   * Otherwise, the adjuster returns the next occurrence of February 29 after the input date.
   *
   * @return an adjuster that finds the next leap day
   */
  def nextOrSameLeapDay: DateAdjuster = NextOrSameLeapDayAdjuster

  /**
   * Finds the next leap day on or after the input date.
   *
   * If the input date is February 29, the input date is returned unaltered.
   * Otherwise, the result is the next occurrence of February 29 after the input date.
   *
   * @param input the input date
   * @return the next leap day date
   */
  def nextOrSameLeapDay(input: LocalDate): LocalDate =
    // already a leap day, return it
    if (input.getMonthValue == 2 && input.getDayOfMonth == 29) {
      input
    } else if (input.isLeapYear && input.getMonthValue <= 2) {
      // handle if before February 29 in a leap year
      LocalDate.of(input.getYear, 2, 29)
    } else {
      // handle any other date
      ensureLeapDay(((input.getYear / 4) * 4) + 4)
    }

  /**
   * Converts a candidate year into the leap day at or after it.
   *
   * A year divisible by four is not necessarily a leap year: century years are leap years
   * only when they are also divisible by 400. When the candidate is such a year - 2100,
   * 2200, 2300 and so on - the next candidate four years later is used instead, which is
   * always a leap year because two consecutive multiples of four cannot both be non-leap
   * century years.
   *
   * @param possibleLeapYear the candidate year, which is always divisible by four
   * @return February 29 in the candidate year, or four years later when the candidate is
   *   a non-leap century year
   */
  private def ensureLeapDay(possibleLeapYear: Int): LocalDate =
    // handle 2100, which is not a leap year
    if (Year.isLeap(possibleLeapYear.toLong)) {
      LocalDate.of(possibleLeapYear, 2, 29)
    } else {
      LocalDate.of(possibleLeapYear + 4, 2, 29)
    }
}
