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
 * Each frequency is based on a `java.time.Period`, held in the canonical form of its length.
 * Construction redistributes the months of that period into years and months, so a frequency of
 * 12 months and a frequency of 1 year are '''one''' value rather than two, and 30 months is two
 * years and six months. Days are left alone, being canonical already, beyond the days-to-weeks
 * naming described below. Canonicalisation is what makes this a normalising type: two
 * frequencies of the same length are the same value, so equality, hashing, ordering, the name
 * and the JSON form of a frequency follow from its length and not from the arithmetic the
 * caller happened to write.
 *
 * The one length whose canonical form is not the one `java.time.Period.normalized` produces is
 * a year: it is held as 12 months and named `P12M`. That is the form the library being ported
 * uses for the annual frequency - it publishes a `P12M` constant and no `P1Y` one, and its
 * `Tenor` normalises a year to `12M` for the same reason - and it is the form every captured
 * Java baseline of the migration carries.
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
 * constants, or one of the factories, each of which canonicalises its input - a whole number of
 * days is named in weeks and the months of a period are redistributed into years and months -
 * and rejects a period that is zero, negative or longer than the maximum. A factory therefore
 * hands back `EitherNec[Failure, Frequency]` rather than throwing, and every value of this type
 * is valid '''and canonical''' by construction, which is why [[normalized]] has nothing left to
 * do. Pattern matching still works, since `unapply` is generated as usual:
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
 * name that contradicts a period cannot be constructed. Because every period is canonical, that
 * equality is equality of length - which is where this port is stricter than the one being
 * ported, whose 12-month and 1-year frequencies were unequal.
 *
 * The derivation of the name is total and injective: `Term` names the 10,000-year period, `P2W`
 * names an exact number of weeks, and every other frequency is named by its ISO-8601 period
 * text, which never contains `W` and never spells `Term`. [[name]], `toString` and the `Show`
 * instance all agree, and the JSON form is that same text.
 *
 * ===Divergences from the Java original, for `SCALA_MIGRATION.md`===
 *
 *  - '''Construction canonicalises the period.''' The Java factories keep the period they are
 *    handed, so a frequency of 12 months and a frequency of 1 year are distinct values there and
 *    `normalized()` converts between them. This port canonicalises in the one place a frequency
 *    is created, as the construction policy of a normalising type requires, with three visible
 *    consequences: the two are one value, so `ofMonths(12)`, `ofYears(1)`, `of(Period.ofYears(1))`
 *    and `parse("P1Y")` all yield [[Frequency.P12M]]; a month count above twelve is named in
 *    years and months, so `ofMonths(30)` is `P2Y6M` where Java named it `P30M`; and
 *    [[normalized]] is the identity. The canonical form of a year is `P12M` rather than `P1Y`,
 *    for the reasons given above, so `normalized` on the annual frequency returns it unchanged
 *    where the Java method returned a value named `P1Y`.
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
   * An annual frequency consists of 12 months, however they were expressed to the factory that
   * built it, and has no day or week element. A frequency built from one year and one built from
   * twelve months are the same value - [[Frequency.P12M]] - and it is the annual one.
   *
   * @return true if this frequency is annual
   */
  def isAnnual: Boolean = period.toTotalMonths == 12L && period.getDays == 0

  //-------------------------------------------------------------------------
  /**
   * Returns this frequency, which is already in canonical form.
   *
   * The type being ported held whatever period it was handed and offered this method to
   * redistribute months beyond twelve into years and months. This port performs that
   * redistribution in the one place a frequency is created, so no caller can hold a frequency
   * that is not canonical and there is nothing left for this method to do. It is therefore the
   * identity - which is also the idempotence the construction policy asks of a normalising
   * type - and it is kept because the ported API published it, so a call site that asks for a
   * normalised frequency still reads and behaves correctly.
   *
   * One value differs from what the ported `normalized()` produced: the canonical form of a
   * year here is 12 months, so the annual frequency is returned unchanged as
   * [[Frequency.P12M]] where the Java method handed back a frequency named `P1Y`. The reason is
   * in the divergence list of this type.
   *
   * @return this frequency, the canonical value of its length
   */
  def normalized: Frequency = this

  //-------------------------------------------------------------------------
  /**
   * Calculates the number of events that occur in a year.
   *
   * The number of events per year is the number of times the period occurs in a year. Not
   * every frequency has an integral number, and the ones that do are exactly the following.
   * Month-based and year-based frequencies divide 12 by the number of months, so `P1M`, `P2M`,
   * `P3M`, `P4M`, `P6M` and `P12M` - the annual frequency, whichever unit the caller spelled
   * that length in - return a value. Day-based and week-based
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
 * values the factories hand back for the corresponding inputs - the very same instances, not
 * merely equal ones, because a factory looks the canonical period up among them before building
 * anything. So `Frequency.ofMonths(3)`, `Frequency.of(Period.ofMonths(3))` and
 * `Frequency.parse("P3M")` all yield [[P3M]] itself, and `Frequency.ofYears(1)` yields
 * [[P12M]]. Each factory canonicalises what it is given and rejects what cannot be a frequency,
 * reporting the rejection as a `Failure` rather than throwing; [[parse]] additionally reads the
 * text form back.
 *
 * ===Declaration order matters here===
 *
 * The fourteen constants are built while this object initialises, and building one canonicalises
 * its period and derives its name and its events-per-year count, which reads the private
 * constants below. Those constants are therefore declared first: moving them after the values
 * would leave [[TERM]] reading an uninitialised period and naming itself `P10000Y`. The ad-hoc
 * and unit tests assert `TERM.name == "Term"`, which is what pins this ordering.
 *
 * The lookup table [[byCanonicalPeriod]] runs the other way: it reads the fourteen values, so it
 * is declared after them. It is consulted only by a factory, which cannot run until this object
 * has finished initialising, so nothing reads it while it is still empty.
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

  /**
   * The canonical period of the annual frequency, which is twelve months rather than one year.
   *
   * A length of exactly twelve months with no days is the one length whose canonical form is not
   * the form `java.time.Period.normalized` produces. Months are chosen because that is the form
   * the ported library uses for the annual frequency - it publishes a `P12M` constant and no
   * `P1Y` one, its `Tenor.normalized` maps a year to `12M`, and its captured baselines spell the
   * annual frequency `P12M` - so holding that length as months keeps the name, the text form and
   * the JSON of the most-used frequency of the library identical to the original.
   *
   * This is read while the fourteen values below are built, so it is declared before them and
   * after [[MonthsPerYear]], which it reads in turn.
   */
  private val AnnualPeriod: Period = Period.ofMonths(MonthsPerYear)

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
   * There is 1 event per year with this frequency. This is '''the''' frequency of that length:
   * twelve months is the canonical form of a year here, so [[ofYears]] of one year, [[of]] of a
   * one-year period and [[parse]] of `P1Y` all yield this value, and it names itself `P12M`.
   * The library being ported kept a frequency of one year apart from this one; the divergence is
   * recorded on the type.
   */
  val P12M: Frequency = create(AnnualPeriod)

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
   * number of days is converted to weeks, so a period of 7 days yields [[P1W]]; the months of
   * the period are redistributed into years and months, so a period of 12 months and a period
   * of one year both yield [[P12M]] and a period of 30 months yields the frequency named
   * `P2Y6M`. Two periods of the same length therefore yield the same value.
   *
   * The period must be positive and non-zero, and a period expressed in months or years must
   * not exceed 1,000 years; each of those is a failure of this factory rather than an
   * exception. A period expressed in days is not bounded, as in the Java original. The bound is
   * applied to the period as it is given, which the redistribution cannot change: it leaves
   * both the total number of months and the number of days exactly as they were.
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
   * Months beyond twelve are redistributed into years and months, so 24 months yields the
   * two-year frequency named `P2Y` and 30 months yields `P2Y6M`; twelve months is the annual
   * frequency [[P12M]], whose canonical form is months and which is therefore named in months.
   * The six month-based constants are returned for their own numbers of months, and more than
   * 12,000 months is a failure.
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
   * One year is the annual frequency [[P12M]], the canonical form of that length being months;
   * any other number of years is held as years, so two years is the frequency named `P2Y`. More
   * than 1,000 years is a failure.
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
   * canonicalisation and the bounds of that factory apply: `P7D` parses to [[P1W]], `P1Y` and
   * `P12M` both parse to [[P12M]], `P30M` parses to the frequency named `P2Y6M`, and `-2D` and
   * `PTerm` are failures. Text naming a length therefore names one frequency, however it spells
   * that length.
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
   * Creates a frequency from a period already known to be one, in canonical form.
   *
   * This is the only place a frequency is instantiated, and it canonicalises the period it is
   * given, so no value of this type can hold a non-canonical period however it was built - the
   * invariant that makes [[Frequency.normalized]] the identity and makes equality equality of
   * length. The private constructor of a `sealed abstract case class` can only be reached
   * through an anonymous subclass, which is what suppresses the public `apply` and `copy` the
   * compiler would otherwise generate, and confines construction to this object.
   *
   * @param period  the period, which must be positive, non-zero and within the maximum length
   * @return the frequency holding the canonical form of that period
   */
  private def create(period: Period): Frequency = new Frequency(canonicalPeriodOf(period)) {}

  /**
   * Creates a frequency from a period, checking that the period can be one.
   *
   * The two checks are those of the Java constructor, and they accumulate: the outcome names
   * every reason the period was rejected rather than only the first. They are the only checks
   * needed at this point, because each factory has already applied whatever bound it carries.
   *
   * The checks run '''before''' the period is canonicalised, which is what keeps
   * canonicalisation safe: it is only ever applied to a positive period whose total number of
   * months a factory has already bounded, so the arithmetic of
   * `java.time.Period.normalized` cannot overflow (see [[canonicalPeriodOf]]).
   *
   * @param period  the period to check
   * @return the frequency, or the failures describing why the period is not one
   */
  private def validated(period: Period): EitherNec[Failure, Frequency] =
    Validate.toResult(
      (
        Validate.isFalse(period.isZero, "Frequency period must not be zero"),
        Validate.isFalse(period.isNegative, "Frequency period must not be negative")
      ).mapN((_, _) => canonical(period)))

  /**
   * Returns the frequency of the length of the specified period, reusing a constant if there is
   * one.
   *
   * Canonicalising first and then looking the result up means a factory hands back the very
   * instance a caller would have named: `of(Period.ofMonths(3))` is [[P3M]] and
   * `ofYears(1)` is [[P12M]], not merely values equal to them. Reuse is an optimisation of
   * identity, not of semantics - equality of this type is structural, so a freshly built value
   * of the same length would compare equal either way.
   *
   * [[create]] canonicalises the period again, which costs nothing - the reduction is idempotent
   * and hands back the very period it is given once that period is canonical - and keeps the
   * invariant stated in one place, the place a frequency is built.
   *
   * @param period  the period, already known to be positive, non-zero and within the maximum length
   * @return the constant of that length, or a new frequency holding its canonical period
   */
  private def canonical(period: Period): Frequency = {
    val canonicalPeriod = canonicalPeriodOf(period)
    byCanonicalPeriod.getOrElse(canonicalPeriod, create(canonicalPeriod))
  }

  /**
   * Reduces a period to the canonical form of its length.
   *
   * Two periods of the same length - the same total number of months and the same number of
   * days - reduce to the same period here, which is what makes a frequency a value of its
   * length rather than of the arithmetic that produced it. The reduction is
   * `java.time.Period.normalized`, which carries months beyond twelve into years and leaves the
   * days untouched, with the single exception of a length of exactly twelve months and no days:
   * that is held as [[AnnualPeriod]], twelve months, for the reasons recorded there. A period
   * already in canonical form is returned as it is.
   *
   * `Period.normalized` reports an arithmetic overflow for a period of more than roughly 2.1
   * billion years, and no such period reaches this method. Every factory bounds the period at
   * 1,000 years, or leaves the total number of months at zero, before anything is created, and
   * the zero and negative checks of [[validated]] short-circuit ahead of it; the only other
   * caller is [[TERM]], whose ten thousand years are already canonical.
   *
   * @param period  the period to reduce
   * @return the canonical period of the same length
   */
  private def canonicalPeriodOf(period: Period): Period =
    if (period.getDays == 0 && period.toTotalMonths == MonthsPerYear.toLong) {
      AnnualPeriod
    } else {
      period.normalized
    }

  /**
   * The fourteen constants indexed by their canonical periods.
   *
   * This is what [[canonical]] consults, so that a factory returns a constant rather than an
   * equal copy of one. It reads the values above and is therefore declared after them; being a
   * lookup from a canonical period, it can only ever be queried with one. The [[TERM]] entry is
   * unreachable through the factories, whose bounds its ten thousand years exceed, and is listed
   * because this is the table of the constants of this type rather than of the reachable ones.
   */
  private val byCanonicalPeriod: Map[Period, Frequency] =
    List(P1D, P1W, P2W, P4W, P13W, P26W, P52W, P1M, P2M, P3M, P4M, P6M, P12M, TERM)
      .map(frequency => frequency.period -> frequency)
      .toMap

  /** Lifts a single failure into the accumulating form the factories return. */
  private def rejected(failure: Failure): EitherNec[Failure, Frequency] = Left(NonEmptyChain.one(failure))

  /**
   * Derives the name of the frequency holding the specified period.
   *
   * The three cases are those of the Java factories, which each chose the name of the value
   * they built: the term period is named `Term`, an exact and non-zero number of days is named
   * in weeks, and anything else is named by the text of the period itself.
   *
   * The period it is given is canonical, so the name is the name of a length: `P12M` for a
   * year, `P2Y6M` for thirty months. Where the Java factories could name two periods of one
   * length differently - `P12M` and `P1Y` - there is only one name to choose here.
   *
   * @param period  the canonical period of the frequency
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
   * the law suites require. It is in fact unreachable: the canonical period of a frequency is a
   * function of its total number of months and its number of days, so two frequencies that
   * agree on both are the same value, and the two comparisons ahead of the tie-break have
   * already separated any pair that is not. The name is compared last so that the ordering is
   * total by construction rather than by that argument - and since the name is derived from the
   * period and distinct periods have distinct names, the tie-break agrees with equality even if
   * it were reached. The Java class publishes no comparison at all, so this ordering is an
   * addition of the port rather than a port of anything.
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
