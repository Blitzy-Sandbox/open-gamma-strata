/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.schedule

import java.time.LocalDate
import java.time.Period
import java.time.temporal.ChronoUnit

import scala.util.Try

import cats.Hash
import cats.Order
import cats.Show
import cats.data.EitherNec
import cats.data.NonEmptyChain
import cats.syntax.apply._

import io.circe.Codec

import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * A periodic frequency used by financial products that have a specific event every so often.
 *
 * Frequency is primarily intended to be used to subdivide events within a year. A frequency is
 * any positive, non-zero period of days, weeks, months or years; the companion provides
 * constants for the common ones, which are best used by importing them.
 *
 * A special value, `Term`, is provided for when there are no subdivisions of the entire term.
 * This is also known as 'zero-coupon' or 'once'. It is represented using the period 10,000
 * years, which allows addition and subtraction to work, producing a date after the end of the
 * term.
 *
 * Each frequency is based on a `java.time.Period`. The months and years of that period are '''not'''
 * normalised, so a frequency of 12 months and a frequency of 1 year are two different values
 * that behave identically under date addition. [[normalized]] applies the normalisation.
 *
 * The frequency is often expressed as a number of events per year; [[eventsPerYear]] returns
 * that count for the frequencies for which it is an exact integer, and
 * [[eventsPerYearEstimate]] returns an approximation for every frequency.
 *
 * ===Construction===
 *
 * This is a normalising type in the sense of the port's construction policy. It is a
 * `sealed abstract case class` with a private constructor, so it has no public `apply` and no
 * `copy`, and the only way to obtain a value is through the companion: one of the fourteen
 * constants, or one of the factories, each of which canonicalises its input (a whole number of
 * days becomes weeks) and rejects a period that is zero, negative or longer than the maximum.
 * A factory therefore hands back `EitherNec[Failure, Frequency]` rather than throwing, and
 * every value of this type is valid by construction. Pattern matching still works, since
 * `unapply` is generated as usual:
 *
 * {{{
 * frequency match {
 *   case Frequency(period) if period.getDays > 0 => ...
 *   case _                                       => ...
 * }
 * }}}
 *
 * ===Equality and the name===
 *
 * Two frequencies are equal when their periods are equal, which is the equality of the type
 * being ported: there, `equals` and `hashCode` read the period alone even though the name was
 * held in a second field. This port holds the period as its only field and derives the name
 * from it, so the structural equality the case class generates '''is''' that equality, and a
 * name that contradicts a period cannot be constructed. The derivation is total and injective:
 * `Term` names the 10,000-year period, `P2W` names an exact number of weeks, and every other
 * frequency is named by its ISO-8601 period text, which never contains `W` and never spells
 * `Term`. [[name]], `toString` and the `Show` instance all agree, and the JSON form is that
 * same text.
 *
 * ===Divergences from the Java original, for `SCALA_MIGRATION.md`===
 *
 *  - '''`TemporalAmount` is not implemented.''' The Java `Frequency` implements
 *    `java.time.temporal.TemporalAmount`, whose `getUnits` hands back a mutable-collection type
 *    from the JDK, which the public API of this port does not admit anywhere, so implementing
 *    the interface is not open to it. The two methods callers actually used through
 *    that interface - `date.plus(frequency)` and `date.minus(frequency)` - are offered directly
 *    as [[addTo]] and [[subtractFrom]], which are exactly the fast path the Java
 *    implementation took for a `LocalDate`. `get(TemporalUnit)` and `getUnits` have no
 *    replacement: a caller that needs the components of the frequency reads [[period]] and asks
 *    `java.time.Period` itself. Every date arithmetic site in this package - the roll
 *    conventions, the day-of-week roll overrides and both schedule generators - calls
 *    [[addTo]] or [[subtractFrom]].
 *  - '''`Order` is added.''' The Java class is not `Comparable`. This port publishes an
 *    ordering, by length of the period and then by name, because the type is held in sorted
 *    collections and compared in the law suites; the ordering agrees with equality.
 *  - '''Failures replace exceptions.''' Rejection by a factory is a `Failure` on the left of an
 *    `Either`, and [[eventsPerYear]] and [[exactDivide]] report the two data-dependent
 *    failures the Java methods threw. Java serialization and Joda-Convert are dropped.
 *
 * @param period  the period of the frequency, which is positive and non-zero
 */
sealed abstract case class Frequency private (period: Period) {

  /**
   * The name of this frequency.
   *
   * The name is the ISO-8601 text of the period, with the two special cases the type being
   * ported also had: an exact number of weeks is named in weeks (`P2W` rather than `P14D`),
   * and the 10,000-year period is named `Term`. It is a total function of [[period]], so it is
   * derived here rather than supplied, and it is what `toString`, `Show` and the JSON form of
   * this type all render.
   */
  val name: String = Frequency.nameOf(period)

  /**
   * The exact number of events per year, or the sentinel when there is no exact number.
   *
   * This mirrors the transient field the Java constructor computed, sentinel included, so that
   * the arithmetic of this port is the arithmetic of the original. It is private because the
   * sentinel is not part of the contract of this type: [[eventsPerYear]] is the public reading
   * of it, and it reports the absence of an exact count as a failure rather than as a value.
   */
  private val eventsPerYearRaw: Int = Frequency.eventsPerYearOf(period)

  /**
   * Estimates the number of events that occur in a year.
   *
   * The estimate exists for every frequency, so unlike [[eventsPerYear]] this needs no error
   * channel. `Term` estimates zero; a month-based frequency is 12 divided by the number of
   * months and a day-based one is 364 divided by the number of days, both of which are exact
   * for the constants; and a frequency mixing months and days is estimated from the average
   * durations the calendar units declare.
   *
   * The expressions are those of the Java implementation, evaluated in the same order and at
   * the same widths, because this value feeds the schedule parity fixture and is compared with
   * the captured Java baseline to within 1e-9 absolutely and relatively.
   */
  val eventsPerYearEstimate: Double = Frequency.eventsPerYearEstimateOf(period)

  //-------------------------------------------------------------------------
  /**
   * Checks whether this is the `Term` frequency.
   *
   * The term frequency corresponds to there being no subdivisions of the entire term. Where
   * the Java implementation compared references against its own constant, this compares the
   * period against the 10,000-year period, which identifies the same single value: no factory
   * admits a period of more than 1,000 years, so [[Frequency.TERM]] is the only value that can
   * hold it.
   *
   * @return true if this is the `Term` frequency
   */
  def isTerm: Boolean = period == Frequency.TermPeriod

  /**
   * Checks whether this frequency is week-based.
   *
   * A week-based frequency consists of an integral number of weeks, so it has no month or year
   * element. Note that `Term` is not week-based.
   *
   * @return true if this frequency is week-based
   */
  def isWeekBased: Boolean = period.toTotalMonths == 0L && period.getDays % 7 == 0

  /**
   * Checks whether this frequency is month-based.
   *
   * A month-based frequency consists of an integral number of months, so a year-based
   * frequency is month-based too, and it has no day or week element. Note that `Term` is not
   * month-based, which is what makes [[exactDivide]] reject it.
   *
   * @return true if this frequency is month-based
   */
  def isMonthBased: Boolean = period.toTotalMonths > 0L && period.getDays == 0 && !isTerm

  /**
   * Checks whether this frequency is annual.
   *
   * An annual frequency consists of 12 months, however they are expressed, and has no day or
   * week element. Both `P12M` and a frequency of one year are annual.
   *
   * @return true if this frequency is annual
   */
  def isAnnual: Boolean = period.toTotalMonths == 12L && period.getDays == 0

  //-------------------------------------------------------------------------
  /**
   * Normalises the months and years of this frequency.
   *
   * This returns a frequency of an equivalent length, with any number of months greater than
   * twelve expressed as a combination of years and months. A frequency of 12 months normalises
   * to one of 1 year, and one of 30 months to one of 2 years and 6 months; a day-based or
   * week-based frequency is returned unchanged.
   *
   * The result cannot fail to be a frequency, so this is total, as it was in Java.
   * Normalisation only redistributes the months of the period into years and months, leaving
   * the total number of months and the number of days as they were, so it can neither make the
   * period zero or negative nor push it past the maximum length - the three things a factory
   * rejects. The normalised period is therefore built directly rather than run through a
   * factory whose failure case is unreachable.
   *
   * @return the normalised frequency
   */
  def normalized: Frequency = {
    val norm = period.normalized
    if (norm == period) this else Frequency.create(norm)
  }

  //-------------------------------------------------------------------------
  /**
   * Calculates the number of events that occur in a year.
   *
   * The number of events per year is the number of times the period occurs in a year. Not
   * every frequency has an integral number, and the ones that do are exactly the following.
   * Month-based and year-based frequencies divide 12 by the number of months, so `P1M`, `P2M`,
   * `P3M`, `P4M`, `P6M` and `P12M` (or `P1Y`) return a value. Day-based and week-based
   * frequencies divide 364 by the number of days, so `P1D`, `P2D`, `P4D`, `P7D`, `P13D`,
   * `P26D`, `P28D`, `P52D`, `P91D`, `P182D` and `P364D` return a value, which covers `P1W`,
   * `P2W`, `P4W`, `P13W`, `P26W` and `P52W`. Every constant of this type therefore returns a
   * value, and `Term` returns zero.
   *
   * Any other frequency - `P5M` and `P3D`, for example - has no integral number of events per
   * year, and that is the one failure of this method. It is data-dependent rather than a
   * breach of contract by the caller, which is why it is reported as a failure where the Java
   * method threw.
   *
   * @return the number of events per year, or the failure describing why there is no exact number
   */
  def eventsPerYear: Either[Failure, Int] =
    if (eventsPerYearRaw == Frequency.NoExactEventsPerYear) {
      Left(Failure.Invalid(s"Unable to calculate events per year: $name"))
    } else {
      Right(eventsPerYearRaw)
    }

  /**
   * Exactly divides this frequency by another.
   *
   * This calculates the integer division of this frequency by the specified one, and fails if
   * the result is not an integer. Month-based and year-based frequencies divide their total
   * numbers of months, so `P6M` divided by `P3M` is 2 and a frequency of 2 years divided by
   * `P6M` is 4. Day-based and week-based frequencies divide their numbers of days, so `P26W`
   * divided by `P13W` is 2 and `P2W` divided by `P1D` is 14. The two kinds do not mix: `P1M`
   * divided by `P1W` fails, as does either direction involving `Term`, which is neither
   * month-based nor day-based.
   *
   * @param other  the frequency to divide into this one
   * @return this frequency divided by the other, or the failure describing why it does not divide exactly
   */
  def exactDivide(other: Frequency): Either[Failure, Int] = {
    val ratio =
      if (isMonthBased && other.isMonthBased) {
        Frequency.exactRatio(period.toTotalMonths, other.period.toTotalMonths)
      } else if (period.toTotalMonths == 0L && other.period.toTotalMonths == 0L) {
        Frequency.exactRatio(period.getDays.toLong, other.period.getDays.toLong)
      } else {
        None
      }
    ratio.toRight(Failure.Invalid(s"Frequency '$name' is not a multiple of '${other.name}'"))
  }

  //-------------------------------------------------------------------------
  /**
   * Adds the period of this frequency to the specified date.
   *
   * This is the operation the Java implementation performed when a `LocalDate` was passed to
   * the `TemporalAmount` interface that this port does not implement, and it is written the
   * same way: the total number of months is added, then the number of days. Weeks are days, so
   * a week-based frequency adds days. The result is a date, always, with the single reservation
   * that `java.time` reports a date outside the range it can represent as it does for
   * `LocalDate.plusMonths` - which only a frequency of thousands of years, `Term` among them,
   * can reach.
   *
   * @param date  the date to add this frequency to
   * @return the date with this frequency added
   */
  def addTo(date: LocalDate): LocalDate =
    date.plusMonths(period.toTotalMonths).plusDays(period.getDays.toLong)

  /**
   * Subtracts the period of this frequency from the specified date.
   *
   * This is the mirror of [[addTo]], subtracting the total number of months and then the number
   * of days, and carries the same reservation about dates outside the range `java.time` can
   * represent.
   *
   * @param date  the date to subtract this frequency from
   * @return the date with this frequency subtracted
   */
  def subtractFrom(date: LocalDate): LocalDate =
    date.minusMonths(period.toTotalMonths).minusDays(period.getDays.toLong)

  //-------------------------------------------------------------------------
  /**
   * Returns the text of this frequency, which is its name.
   *
   * The format combines the quantity and the unit - `P1D`, `P2W`, `P3M`, `P4Y` - and the term
   * frequency renders as `Term`. This is the text [[Frequency.parse]] reads back.
   *
   * @return the name of this frequency
   */
  override def toString: String = name
}

/**
 * Provides the constants, factories and instances for periodic frequencies.
 *
 * The fourteen constants are the frequencies that occur throughout finance, and they are the
 * values the factories hand back for the corresponding inputs, so `Frequency.ofMonths(3)`
 * yields the very same value as [[P3M]]. Each factory canonicalises what it is given and
 * rejects what cannot be a frequency, reporting the rejection as a `Failure` rather than
 * throwing; [[parse]] additionally reads the text form back.
 *
 * ===Declaration order matters here===
 *
 * The fourteen constants are built while this object initialises, and building one derives its
 * name and its events-per-year count, which reads the private constants below. Those constants
 * are therefore declared first: moving them after the values would leave [[TERM]] reading an
 * uninitialised period and naming itself `P10000Y`. The ad-hoc and unit tests assert
 * `TERM.name == "Term"`, which is what pins this ordering.
 */
object Frequency {

  /**
   * The artificial maximum length of a frequency in years, mirroring the Java `MAX_YEARS`.
   *
   * A frequency expressed in years or months is bounded, because a schedule generated from one
   * longer than this is a mistake rather than a long-dated trade. A frequency expressed in days
   * or weeks is deliberately not bounded by this, exactly as in the Java original.
   */
  private val MaxYears: Int = 1000

  /** The maximum length of a frequency in months, mirroring the Java `MAX_MONTHS`. */
  private val MaxMonths: Int = MaxYears * 12

  /** The artificial length in years of the `Term` frequency, mirroring the Java `TERM_YEARS`. */
  private val TermYears: Int = 10000

  /**
   * The period of the `Term` frequency.
   *
   * Ten thousand years is far beyond the maximum length any factory admits, so this period
   * identifies [[TERM]] uniquely and is what [[Frequency.isTerm]] compares against.
   */
  private val TermPeriod: Period = Period.ofYears(TermYears)

  /** The name of the `Term` frequency, which is the one name that is not period text. */
  private val TermName: String = "Term"

  /**
   * The value standing for 'there is no integral number of events per year'.
   *
   * The Java implementation used -1 in the same field for the same purpose. It never escapes
   * this file: `Frequency.eventsPerYear` turns it into a failure.
   */
  private val NoExactEventsPerYear: Int = -1

  /** The number of days per year used to count day-based and week-based events, as in Java. */
  private val DaysPerYear: Int = 364

  /** The number of months per year used to count month-based events, as in Java. */
  private val MonthsPerYear: Int = 12

  /** The message reported when a period is longer than the maximum, worded as in Java. */
  private val MaxPeriodMessage: String = "Period must not exceed 1000 years"

  /**
   * The message reported when a number of months is above the maximum, worded as in Java.
   *
   * The Java message was assembled with a `DecimalFormat` grouping the constant 12,000; the
   * grouped text is written out here, since the value is fixed at compile time and this port
   * does not reach for a formatter to print one number.
   */
  private val MaxMonthsMessage: String = "Months must not exceed 12,000"

  /** The message reported when a number of years is above the maximum, worded as in Java. */
  private val MaxYearsMessage: String = "Years must not exceed 1,000"

  //-------------------------------------------------------------------------
  /**
   * A periodic frequency of one day, also known as daily.
   *
   * There are considered to be 364 events per year with this frequency.
   */
  val P1D: Frequency = create(Period.ofDays(1))

  /**
   * A periodic frequency of 1 week (7 days), also known as weekly.
   *
   * There are considered to be 52 events per year with this frequency.
   */
  val P1W: Frequency = create(Period.ofWeeks(1))

  /**
   * A periodic frequency of 2 weeks (14 days), also known as bi-weekly.
   *
   * There are considered to be 26 events per year with this frequency.
   */
  val P2W: Frequency = create(Period.ofWeeks(2))

  /**
   * A periodic frequency of 4 weeks (28 days), also known as lunar.
   *
   * There are considered to be 13 events per year with this frequency.
   */
  val P4W: Frequency = create(Period.ofWeeks(4))

  /**
   * A periodic frequency of 13 weeks (91 days).
   *
   * There are considered to be 4 events per year with this frequency.
   */
  val P13W: Frequency = create(Period.ofWeeks(13))

  /**
   * A periodic frequency of 26 weeks (182 days).
   *
   * There are considered to be 2 events per year with this frequency.
   */
  val P26W: Frequency = create(Period.ofWeeks(26))

  /**
   * A periodic frequency of 52 weeks (364 days).
   *
   * There is considered to be 1 event per year with this frequency.
   */
  val P52W: Frequency = create(Period.ofWeeks(52))

  /**
   * A periodic frequency of 1 month, also known as monthly.
   *
   * There are 12 events per year with this frequency.
   */
  val P1M: Frequency = create(Period.ofMonths(1))

  /**
   * A periodic frequency of 2 months, also known as bi-monthly.
   *
   * There are 6 events per year with this frequency.
   */
  val P2M: Frequency = create(Period.ofMonths(2))

  /**
   * A periodic frequency of 3 months, also known as quarterly.
   *
   * There are 4 events per year with this frequency.
   */
  val P3M: Frequency = create(Period.ofMonths(3))

  /**
   * A periodic frequency of 4 months.
   *
   * There are 3 events per year with this frequency.
   */
  val P4M: Frequency = create(Period.ofMonths(4))

  /**
   * A periodic frequency of 6 months, also known as semi-annual.
   *
   * There are 2 events per year with this frequency.
   */
  val P6M: Frequency = create(Period.ofMonths(6))

  /**
   * A periodic frequency of 12 months (1 year), also known as annual.
   *
   * There is 1 event per year with this frequency. Note that this is not the same value as a
   * frequency of one year: the months of a period are not normalised, so `P12M` and `P1Y`
   * are distinct frequencies of equal length. [[Frequency.normalized]] converts between them.
   */
  val P12M: Frequency = create(Period.ofMonths(12))

  /**
   * A periodic frequency matching the term, also known as zero-coupon.
   *
   * This is represented using the period 10,000 years, so that adding it to a date produces a
   * date after the end of any term. There are no events per year with this frequency, and it
   * is neither month-based nor day-based, so it cannot be divided by another frequency.
   */
  val TERM: Frequency = create(TermPeriod)

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance from a `Period`.
   *
   * The period normally consists of either days and weeks, or months and years. An exact
   * number of days is converted to weeks, so a period of 7 days yields [[P1W]]; months are not
   * normalised into years, so a period of 12 months yields [[P12M]] rather than a frequency of
   * one year.
   *
   * The period must be positive and non-zero, and a period expressed in months or years must
   * not exceed 1,000 years; each of those is a failure of this factory rather than an
   * exception. A period expressed in days is not bounded, as in the Java original.
   *
   * @param period  the period to convert to a periodic frequency
   * @return the frequency, or the failures describing why the period is not one
   */
  def of(period: Period): EitherNec[Failure, Frequency] = {
    val days = period.getDays
    val months = period.toTotalMonths
    if (months == 0L && days != 0) {
      ofDays(days)
    } else if (months > MaxMonths.toLong) {
      rejected(Failure.Invalid(MaxPeriodMessage))
    } else {
      validated(period)
    }
  }

  /**
   * Obtains an instance backed by a period of days.
   *
   * An exact number of weeks is expressed as weeks, so 7 days yields [[P1W]] and 91 days
   * yields [[P13W]]. Zero and negative numbers of days are failures, as they are for every
   * factory here.
   *
   * @param days  the number of days
   * @return the frequency, or the failures describing why that number of days is not one
   */
  def ofDays(days: Int): EitherNec[Failure, Frequency] =
    if (days % 7 == 0) ofWeeks(days / 7) else validated(Period.ofDays(days))

  /**
   * Obtains an instance backed by a period of weeks.
   *
   * The six week-based constants are returned for their own numbers of weeks, and any other
   * positive number of weeks produces a frequency named in weeks - 3 weeks is `P3W`.
   *
   * A number of weeks beyond roughly 306 million cannot be expressed as a number of days at
   * all, and is reported as a failure here. That is the one place this factory is more total
   * than the Java original, which let the arithmetic of `Period.ofWeeks` overflow and throw.
   *
   * @param weeks  the number of weeks
   * @return the frequency, or the failures describing why that number of weeks is not one
   */
  def ofWeeks(weeks: Int): EitherNec[Failure, Frequency] = weeks match {
    case 1 => Right(P1W)
    case 2 => Right(P2W)
    case 4 => Right(P4W)
    case 13 => Right(P13W)
    case 26 => Right(P26W)
    case 52 => Right(P52W)
    case _ =>
      // `Period.ofWeeks` multiplies by seven, so the product is computed in `Long` here and
      // checked before it is narrowed: a period of days is what a period of weeks is.
      val days = weeks.toLong * 7L
      if (days.isValidInt) {
        validated(Period.ofDays(days.toInt))
      } else {
        rejected(Failure.Invalid(s"Frequency of $weeks weeks is too large to express in days"))
      }
  }

  /**
   * Obtains an instance backed by a period of months.
   *
   * Months are not normalised into years, so 24 months yields the frequency `P24M` and not one
   * of two years. The six month-based constants are returned for their own numbers of months,
   * and more than 12,000 months is a failure.
   *
   * @param months  the number of months
   * @return the frequency, or the failures describing why that number of months is not one
   */
  def ofMonths(months: Int): EitherNec[Failure, Frequency] = months match {
    case 1 => Right(P1M)
    case 2 => Right(P2M)
    case 3 => Right(P3M)
    case 4 => Right(P4M)
    case 6 => Right(P6M)
    case 12 => Right(P12M)
    case _ =>
      if (months > MaxMonths) rejected(Failure.Invalid(MaxMonthsMessage)) else validated(Period.ofMonths(months))
  }

  /**
   * Obtains an instance backed by a period of years.
   *
   * The frequency holds the years it is given, so a frequency of one year is not the same value
   * as [[P12M]] even though the two behave identically under date arithmetic. More than 1,000
   * years is a failure.
   *
   * @param years  the number of years
   * @return the frequency, or the failures describing why that number of years is not one
   */
  def ofYears(years: Int): EitherNec[Failure, Frequency] =
    if (years > MaxYears) rejected(Failure.Invalid(MaxYearsMessage)) else validated(Period.ofYears(years))

  //-------------------------------------------------------------------------
  /**
   * Parses the text form of a frequency.
   *
   * The text is ISO-8601, such as `P3M`, and the `P` may be left off, so `2W` and `P2W` both
   * name the same frequency. The term frequency is named `Term`, `T`, `0T` or `1T`, in any
   * mixture of cases. The text is read as a period and then passed through [[of]], so the
   * canonicalisation and the bounds of that factory apply: `P7D` parses to [[P1W]], and `-2D`
   * and `PTerm` are failures.
   *
   * Text that names no period at all is reported as a parsing failure, and text that names a
   * period that is not a frequency reports what [[of]] reported; the accumulated failures of
   * that factory are collapsed into one, since a caller of this method has a single piece of
   * text to correct.
   *
   * @param toParse  the text to parse
   * @return the frequency the text names, or the failure describing why it names none
   */
  def parse(toParse: String): Either[Failure, Frequency] =
    if (isTermText(toParse)) {
      Right(TERM)
    } else {
      // The prefix is added exactly as in Java: only an upper-case `P` is recognised as
      // already present, while `Period.parse` itself reads the units without regard to case.
      val prefixed = if (toParse.startsWith("P")) toParse else "P" + toParse
      Try(Period.parse(prefixed)).toEither match {
        case Right(period) => of(period).left.map(failures => Failure.collapse(failures))
        case Left(_) => Left(Failure.Parsing(s"Unable to parse frequency: '$toParse'"))
      }
    }

  //-------------------------------------------------------------------------
  /**
   * Creates a frequency from a period already known to be one.
   *
   * This is the only place a frequency is instantiated. The private constructor of a
   * `sealed abstract case class` can only be reached through an anonymous subclass, which is
   * what suppresses the public `apply` and `copy` the compiler would otherwise generate, and
   * confines construction to this object.
   *
   * @param period  the period, which must be positive, non-zero and within the maximum length
   * @return the frequency holding that period
   */
  private def create(period: Period): Frequency = new Frequency(period) {}

  /**
   * Creates a frequency from a period, checking that the period can be one.
   *
   * The two checks are those of the Java constructor, and they accumulate: the outcome names
   * every reason the period was rejected rather than only the first. They are the only checks
   * needed at this point, because each factory has already applied whatever bound it carries.
   *
   * @param period  the period to check
   * @return the frequency, or the failures describing why the period is not one
   */
  private def validated(period: Period): EitherNec[Failure, Frequency] =
    Validate.toResult(
      (
        Validate.isFalse(period.isZero, "Frequency period must not be zero"),
        Validate.isFalse(period.isNegative, "Frequency period must not be negative")
      ).mapN((_, _) => create(period)))

  /** Lifts a single failure into the accumulating form the factories return. */
  private def rejected(failure: Failure): EitherNec[Failure, Frequency] = Left(NonEmptyChain.one(failure))

  /**
   * Derives the name of the frequency holding the specified period.
   *
   * The three cases are those of the Java factories, which each chose the name of the value
   * they built: the term period is named `Term`, an exact and non-zero number of days is named
   * in weeks, and anything else is named by the text of the period itself.
   *
   * @param period  the period of the frequency
   * @return the name of that frequency
   */
  private def nameOf(period: Period): String =
    if (period == TermPeriod) {
      TermName
    } else if (period.toTotalMonths == 0L && period.getDays != 0 && period.getDays % 7 == 0) {
      s"P${period.getDays / 7}W"
    } else {
      period.toString
    }

  /**
   * Counts the events per year of the specified period, or reports that there is no exact count.
   *
   * This is the Java constructor's computation, branch for branch: the term period has no
   * events, a month-based period divides twelve by its months, a day-based period divides 364
   * by its days, and anything else - a period mixing months and days - has no exact count.
   *
   * @param period  the period of the frequency
   * @return the number of events per year, or [[NoExactEventsPerYear]] when there is no exact number
   */
  private def eventsPerYearOf(period: Period): Int = {
    val monthsLong = period.toTotalMonths
    if (monthsLong > MaxMonths.toLong) {
      0
    } else {
      val months = monthsLong.toInt
      val days = period.getDays
      if (months > 0 && days == 0) {
        if (MonthsPerYear % months == 0) MonthsPerYear / months else NoExactEventsPerYear
      } else if (days > 0 && months == 0) {
        if (DaysPerYear % days == 0) DaysPerYear / days else NoExactEventsPerYear
      } else {
        NoExactEventsPerYear
      }
    }
  }

  /**
   * Estimates the events per year of the specified period.
   *
   * This is the Java constructor's computation, expression for expression and width for width.
   * The mixed case multiplies the months and the days by the average durations of those units
   * in seconds, sums them as whole seconds, and divides the average length of a year by the
   * result - so the value is the one the Java implementation produced down to the last bit,
   * which is what the parity fixture compares against.
   *
   * @param period  the period of the frequency
   * @return the estimated number of events per year
   */
  private def eventsPerYearEstimateOf(period: Period): Double = {
    val monthsLong = period.toTotalMonths
    if (monthsLong > MaxMonths.toLong) {
      0d
    } else {
      val months = monthsLong.toInt
      val days = period.getDays
      if (months > 0 && days == 0) {
        MonthsPerYear.toDouble / months.toDouble
      } else if (days > 0 && months == 0) {
        DaysPerYear.toDouble / days.toDouble
      } else {
        val estimatedSecs =
          (months.toLong * ChronoUnit.MONTHS.getDuration.getSeconds +
            days.toLong * ChronoUnit.DAYS.getDuration.getSeconds).toDouble
        ChronoUnit.YEARS.getDuration.getSeconds.toDouble / estimatedSecs
      }
    }
  }

  /**
   * Divides one quantity by another, if it divides exactly and the result is an `Int`.
   *
   * The divisor is checked before the remainder is taken and the quotient is checked before it
   * is narrowed, so this cannot raise an arithmetic error for any input. Neither guard is
   * reachable for the frequencies that exist - a divisor of zero would be a zero-length period
   * and the quotient of two frequencies never exceeds the larger of them - and they are here so
   * that the method is total by construction rather than by an argument about its callers.
   *
   * @param dividend  the quantity to divide
   * @param divisor  the quantity to divide by
   * @return the quotient, if the division is exact and fits an `Int`
   */
  private def exactRatio(dividend: Long, divisor: Long): Option[Int] =
    if (divisor == 0L || dividend % divisor != 0L) {
      None
    } else {
      val ratio = dividend / divisor
      if (ratio.isValidInt) Some(ratio.toInt) else None
    }

  /**
   * Checks whether the specified text names the term frequency.
   *
   * The four spellings and the case-insensitive comparison are those of the Java `parse`.
   *
   * @param text  the text to check
   * @return true if the text names the term frequency
   */
  private def isTermText(text: String): Boolean =
    text.equalsIgnoreCase(TermName) || text.equalsIgnoreCase("T") ||
      text.equalsIgnoreCase("0T") || text.equalsIgnoreCase("1T")

  //-------------------------------------------------------------------------
  /**
   * The ordering of frequencies, which is also their hashing.
   *
   * This is the only equality-bearing instance of the type: `Order` and `Hash` both extend
   * `Eq`, so the three can never disagree. Equality is that of the values themselves - the
   * equality of the period, as in Java - and the ordering is by length of the period, comparing
   * the total number of months and then the number of days, with the name breaking the
   * remaining tie.
   *
   * The tie-break is what makes `compare` return zero exactly when the values are equal, which
   * the law suites require: `P12M` and a frequency of one year are of equal length but are not
   * equal values, and they are separated by their names. Since the name is derived from the
   * period and distinct periods have distinct names, no pair of unequal frequencies compares
   * equal. The Java class publishes no comparison at all, so this ordering is an addition of
   * the port rather than a port of anything.
   *
   * @return the ordering of frequencies
   */
  implicit val order: Order[Frequency] with Hash[Frequency] =
    new Order[Frequency] with Hash[Frequency] {

      private val universal: Hash[Frequency] = Hash.fromUniversalHashCode[Frequency]

      override def compare(x: Frequency, y: Frequency): Int = {
        val months = x.period.toTotalMonths.compare(y.period.toTotalMonths)
        if (months != 0) {
          months
        } else {
          val days = x.period.getDays.compare(y.period.getDays)
          if (days != 0) days else x.name.compareTo(y.name)
        }
      }

      override def eqv(x: Frequency, y: Frequency): Boolean = universal.eqv(x, y)

      override def hash(x: Frequency): Int = universal.hash(x)
    }

  /**
   * The rendering of frequencies as text.
   *
   * A frequency renders as its name, which is what `toString` returns and what [[parse]] reads.
   *
   * @return the rendering of a frequency
   */
  implicit val show: Show[Frequency] = Show.show(_.name)

  /**
   * The JSON codec for frequencies.
   *
   * A frequency is written as the string of its name - `"P3M"`, `"P2W"`, `"Term"` - and read
   * back through [[parse]], so a document holding text that names no frequency is rejected with
   * the message of that failure. Being a `Codec`, this single instance serves as both the
   * encoder and the decoder wherever a type containing a frequency derives its own.
   *
   * @return the codec reading and writing a frequency as its name
   */
  implicit val codec: Codec[Frequency] = Codecs.parsedStringCodec[Frequency](parse, _.name)
}
