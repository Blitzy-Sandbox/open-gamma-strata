/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate
import java.time.Period
import java.time.temporal.ChronoUnit
import java.time.temporal.Temporal
import java.time.temporal.TemporalUnit

import scala.annotation.tailrec

import cats.Hash
import cats.Order
import cats.Show
import cats.syntax.apply._

import io.circe.Codec

import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * A tenor indicating how long it will take for a financial instrument to reach maturity.
 *
 * A tenor is any positive, non-zero period of days, weeks, months or years. The common tenors
 * are provided as constants on the companion object, which is how they are normally reached:
 *
 * {{{
 * import com.opengamma.strata.basics.date.Tenor.TENOR_3M
 * }}}
 *
 * ===The period is held as given, not reduced===
 *
 * Each tenor is backed by a `java.time.Period`, and the months and years of that period are
 * deliberately '''not''' normalised against one another. A tenor of twelve months and a tenor
 * of one year are therefore two different values - `P12M` and `P1Y` - that behave identically
 * under date arithmetic, because standard date addition treats them the same way. [[normalized]]
 * produces the canonical form when a caller needs one value rather than two.
 *
 * Days are treated differently: a day count that is an exact multiple of seven is named in
 * weeks, so `Tenor.of(Period.ofDays(14))` is the tenor `2W`. This affects the name only, since
 * `Period.ofWeeks(2)` and `Period.ofDays(14)` are the same period, and it is what makes this
 * type normalising rather than merely validating.
 *
 * ===Identity, equality and ordering===
 *
 * Equality and hashing consider the '''period alone''': the name is derived from the period, so
 * it carries no information equality could use, and two tenors with the same period are the
 * same tenor however they were built. `12M` and `1Y` are consequently '''not''' equal, having
 * different periods.
 *
 * The ordering compares two tenors by length first - day-only tenors by their days, month-only
 * tenors by their total months, and anything else by an estimated length in days, obtained by
 * dividing months by twelve and multiplying by the mean Gregorian year of 365.2425 days - and
 * then breaks a tie by name. The tie-break is deliberate and is what keeps the ordering in step
 * with equality: `cats.Order` requires `compare` to return zero exactly when two values are
 * equal, and `12M` and `1Y` are of equal estimated length while being different tenors. A name
 * is a total, injective function of a period, so two tenors share a name only when they share a
 * period, which is exactly when they are equal; the tie-break therefore returns zero only for
 * equal tenors and cannot reorder a pair the length comparison has already separated. The
 * effect is a total order in which `12M` sorts immediately before `1Y` and tenors of equal
 * estimated length are grouped. [[compareTo]] is the comparison by length on its own, for a
 * caller that wants tenors of equal estimated length ranked equal.
 *
 * ===Applying a tenor to a date===
 *
 * A tenor is applied through its own members rather than through date arithmetic:
 * `tenor.addTo(date)` and `tenor.subtractFrom(date)`, whose `LocalDate` overloads answer a
 * `LocalDate` so that no cast is needed at the call site, with overloads for any other temporal
 * object. [[get]] reads the value of a single unit and [[units]] answers the units a tenor is
 * measured in as a Scala `List`. The JSON codec on the companion is the only serialized form,
 * and it writes the canonical name.
 *
 * ===Construction===
 *
 * There is no public constructor, no `apply` and no `copy`: a value of this type exists only
 * because one of the companion's factories accepted its input, so every tenor in a program is
 * positive and non-zero by construction. The factories report a rejected input as a failure
 * value, and [[Tenor.of]] accumulates, so a caller learns every reason its input was
 * unacceptable at once.
 *
 * This type is immutable and every member is a pure function of the value and its arguments, so
 * it is safe to share between threads without synchronisation.
 *
 * @param period  the period of the tenor, which is always positive and non-zero
 */
sealed abstract case class Tenor private (period: Period) extends NoJavaSerialization {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means - which
  // would carry a period no factory had checked or a name that does not describe it - can be
  // stopped is here. The single implementation is the companion's hidden `Impl`.
  JvmClosure.requireSoleImplementation(this, classOf[Tenor.Impl])

  // The invariant of this type, stated over the fields the instance actually holds rather than
  // over the arguments a factory was given, because the class file of the implementation carries
  // a public constructor whatever the source asked for: a class compiled outside this library can
  // reach it directly, and the check above would admit what it built, since its runtime class is
  // the one class that check admits. What is left to state is therefore the two things
  // [[Tenor.of]] establishes - a period that is positive and non-zero, and a name that is the one
  // this type derives from that period - so that a tenor which exists by any route holds what a
  // factory would have accepted.
  JvmClosure.requireInvariant(
    "its period is neither zero nor negative",
    !period.isZero && !period.isNegative)
  JvmClosure.requireInvariant(
    "its name is the one this type derives from its period",
    name == Tenor.canonicalName(period))

  /**
   * The name of the tenor, which is its canonical text.
   *
   * The name is the ISO-8601 form of the period without the leading `P`, with a day count that
   * is a multiple of seven expressed in weeks: `1D`, `2W`, `3M`, `12M`, `1Y`, `2Y6M`. It is
   * supplied by the factory that built the value and is a function of the period alone, so it
   * cannot disagree with the period it describes.
   *
   * This is the text that [[toString]], the `Show` instance and the JSON codec all produce, and
   * the text that [[Tenor.parse]] accepts, which makes it the identity of the tenor as far as
   * users, stored documents and other systems are concerned.
   *
   * @return the canonical name of the tenor
   */
  def name: String

  /**
   * Checks whether the tenor is week-based.
   *
   * A week-based tenor is an integral number of weeks, so there must be no month or year
   * element and the day count must be a multiple of seven. A tenor of seven days is therefore
   * week-based, and a tenor of six days is not.
   *
   * @return true if this tenor is an integral number of weeks
   */
  def isWeekBased: Boolean = period.toTotalMonths == 0L && period.getDays % 7 == 0

  /**
   * Checks whether the tenor is month-based.
   *
   * A month-based tenor is an integral number of months, so there must be no day element. Any
   * year-based tenor is also month-based, a year being twelve months.
   *
   * @return true if this tenor is an integral number of months
   */
  def isMonthBased: Boolean = period.toTotalMonths > 0L && period.getDays == 0

  /**
   * Normalizes the months and years of the tenor.
   *
   * The result is a tenor of equivalent length in canonical form. A total of twelve months is
   * held in months, so `1Y` normalizes to `12M`; any other period is reduced by
   * `Period.normalized`, which carries excess months into years, so `20M` becomes `1Y8M` and
   * `24M` becomes `2Y`. A tenor already in canonical form is returned unchanged, and normalizing
   * twice therefore gives the same value as normalizing once.
   *
   * Days are untouched, because they are already canonical: `10D` and `2W` normalize to
   * themselves.
   *
   * This operation cannot fail. The length of the tenor is preserved, so the result is positive
   * and non-zero whenever the receiver is, which it always is.
   *
   * @return the equivalent tenor in canonical form
   */
  def normalized: Tenor =
    if (period.getDays == 0 && period.toTotalMonths == 12L) {
      Tenor.TENOR_12M
    } else {
      val reduced = period.normalized
      if (reduced == period) this else Tenor.create(reduced)
    }

  /**
   * Gets the value of the tenor in the specified unit.
   *
   * Values are available for the years, months and days units; note that weeks are not, a week
   * being held as seven days. Any other unit is unsupported and the query is rejected by
   * `java.time`, which raises `UnsupportedTemporalTypeException`, as it does for every amount it
   * measures.
   *
   * @param unit  the unit to query
   * @return the value of the tenor in the requested unit
   */
  def get(unit: TemporalUnit): Long = period.get(unit)

  /**
   * The units a tenor is measured in: years, then months, then days.
   *
   * The list is immutable, and its contents and their order are those of the period underlying
   * every tenor, so it is the same list for every tenor.
   *
   * @return the units of a tenor, in descending order of size
   */
  def units: List[TemporalUnit] = Tenor.SupportedUnits

  /**
   * Adds the tenor to the specified date.
   *
   * This is the overload callers normally want, and it is the fast path: the months of the tenor
   * are added to the date and the days are then applied by the module's own date arithmetic,
   * which is exact and cheaper than the general route through `java.time`. The result is a
   * `LocalDate`, so it composes with the rest of this module without a cast.
   *
   * Month arithmetic clamps the day-of-month, as everywhere in `java.time`: adding one month to
   * the 31st of January gives the 28th of February, or the 29th in a leap year.
   *
   * @param date  the date to add this tenor to
   * @return the date this tenor after the given date
   */
  def addTo(date: LocalDate): LocalDate =
    LocalDateUtils.plusDays(date.plusMonths(period.toTotalMonths), period.getDays)

  /**
   * Adds the tenor to the specified temporal object.
   *
   * A `LocalDate` takes the fast path of the overload above; anything else is handed to
   * `java.time`, which supports any temporal object that understands the years, months and days
   * units, such as a date-time or an offset date-time.
   *
   * @param temporal  the temporal object to add this tenor to
   * @return the temporal object with this tenor added
   */
  def addTo(temporal: Temporal): Temporal = temporal match {
    case date: LocalDate => addTo(date)
    case other => period.addTo(other)
  }

  /**
   * Subtracts the tenor from the specified date.
   *
   * This is [[addTo]] in reverse, with the same fast path and the same clamping of the
   * day-of-month, and is the overload callers normally want.
   *
   * @param date  the date to subtract this tenor from
   * @return the date this tenor before the given date
   */
  def subtractFrom(date: LocalDate): LocalDate =
    LocalDateUtils.plusDays(date.minusMonths(period.toTotalMonths), -period.getDays)

  /**
   * Subtracts the tenor from the specified temporal object.
   *
   * A `LocalDate` takes the fast path of the overload above; anything else is handed to
   * `java.time`.
   *
   * @param temporal  the temporal object to subtract this tenor from
   * @return the temporal object with this tenor subtracted
   */
  def subtractFrom(temporal: Temporal): Temporal = temporal match {
    case date: LocalDate => subtractFrom(date)
    case other => period.subtractFrom(other)
  }

  /**
   * Compares this tenor to another tenor by length.
   *
   * Comparing tenors is a hard problem in general, but for the tenors in common use the outcome
   * is the expected one. Two day-only tenors are compared by their days and two month-only
   * tenors by their total months, both of which are exact. Anything else is compared by an
   * estimated length in days, months being divided by twelve and multiplied by the mean
   * Gregorian year of 365.2425 days. The estimate places a one-month tenor between 30 and 31
   * days, a three-month tenor between 91 and 92 days, a one-year tenor between 365 and 366 days,
   * and a four-year tenor between 1460 and 1461 days.
   *
   * This comparison is '''not''' the `cats.Order` of the companion, and the difference is the
   * point of having both. This method returns zero for tenors of equal estimated length that are
   * not equal - `12M` against `1Y` being the standard example. The `Order` instance calls this
   * method first and then breaks such a tie by name, which is what its laws require; a caller
   * that wants tenors of equal estimated length ranked equal wants this method, and a caller
   * sorting or keying a collection wants the instance.
   *
   * @param other  the other tenor
   * @return negative if this tenor is shorter, zero if they are of equal estimated length, and
   *   positive if this tenor is longer
   */
  def compareTo(other: Tenor): Int = {
    val thisDays = period.getDays
    val thisMonths = period.toTotalMonths
    val otherDays = other.period.getDays
    val otherMonths = other.period.toTotalMonths
    if (thisMonths == 0L && otherMonths == 0L) {
      // both day-only, so the comparison is exact
      java.lang.Integer.compare(thisDays, otherDays)
    } else if (thisDays == 0 && otherDays == 0) {
      // both month-only, so the comparison is again exact
      java.lang.Long.compare(thisMonths, otherMonths)
    } else {
      // mixed, so months are estimated in days; each integral value is converted to a Double
      // explicitly rather than being widened in passing
      val thisLength = thisDays.toDouble + (thisMonths.toDouble / 12d) * 365.2425d
      val otherLength = otherDays.toDouble + (otherMonths.toDouble / 12d) * 365.2425d
      java.lang.Double.compare(thisLength, otherLength)
    }
  }

  /**
   * Returns the canonical text of the tenor, such as `1D`, `2W`, `3M` or `4Y`.
   *
   * This is [[name]]: the text the tenor is known by, rather than the structural rendering a
   * case class would otherwise produce.
   *
   * @return the canonical name of the tenor
   */
  override def toString: String = name

}

/**
 * Provides the tenors in common use as constants, the factories that build any other tenor, and
 * the instances of the type.
 *
 * ===Constants===
 *
 * The constants are named after the tenors they hold - `TENOR_3M`, `TENOR_1Y` - and are values
 * rather than results, because their inputs are known to be acceptable. They are built by the
 * same private code the factories use, so a constant and the equivalent factory call produce
 * equal tenors with identical names.
 *
 * ===Factories===
 *
 * Every public factory reports a rejected input as a failure value, so the only way to hold a
 * tenor is to have had an acceptable input accepted. Two conditions make a period unacceptable:
 *
 *   - a period of zero length;
 *   - a period with any negative element.
 *
 * [[of]] checks both at once and reports every failing check, so a caller does not have to fix
 * one problem to discover the next. The four convenience factories route through [[of]], which
 * keeps the naming rules in one place.
 *
 * ===Instances===
 *
 * The companion declares one equality-bearing instance, one rendering and one codec: `Order` and
 * `Hash` both extend `Eq`, so declaring them as a single value makes it impossible for equality
 * and ordering to disagree, and there is deliberately no separate `Eq`.
 */
object Tenor {

  /**
   * The units a tenor is measured in, in descending order of size.
   *
   * These are the units of the period underlying every tenor, so the list is the same for all
   * of them and is held once here rather than rebuilt per query. It is immutable and is never
   * handed out except as itself, so sharing it between every tenor is safe.
   */
  private val SupportedUnits: List[TemporalUnit] =
    List(ChronoUnit.YEARS, ChronoUnit.MONTHS, ChronoUnit.DAYS)

  /**
   * The longest text a tenor is parsed from, which the grammar of a period puts far below it.
   *
   * A tenor is named by an ISO-8601 period, with or without its leading `P`, and such a period
   * is a handful of characters: the longest one that can name a tenor at all is a signed count
   * of years, months, weeks and days, and even with every count written out to the ten digits an
   * `Int` can hold that is under fifty characters. The ceiling is therefore set at 256 - four
   * times the longest text that can succeed - so that no text a caller means to be read is ever
   * refused for its length, while text written to be large is refused before it is worked on.
   *
   * It bounds work rather than meaning. [[Tenor.parse]] walks the text to read the period it
   * spells, and that walk visits the characters of text that arrived from outside this library
   * (CWE-400/CWE-770): the cost is proportional to the length of the input, and is now reached
   * only by text that is within the grammar's own bound. The walk replaced a copy of the text
   * and a regular-expression matcher over the copy, so the ceiling bounds strictly less work
   * than it was introduced to bound.
   *
   * The value is the one [[com.opengamma.strata.collect.Decimal]] uses for the same purpose on
   * the numeral it reads, so the two ceilings of this port that bound a text grammar are the
   * same number and are reported the same way.
   */
  private val MaxTextLength: Int = 256

  /**
   * Reported for text that is longer than a tenor can be.
   *
   * The message names the ceiling and not the text, which is the one place this port departs
   * from quoting what it refused: the text is refused precisely for being too large to write
   * anywhere, and the caller needs the bound rather than the input to correct it. This is the
   * wording [[com.opengamma.strata.collect.Decimal]] reports for the same condition, with the
   * name of this grammar in place of its own.
   */
  private val MaxTextLengthMessage: String =
    s"Tenor string must not exceed $MaxTextLength characters"

  // The tenors in common use, grouped by unit - days, then weeks, then months, then years - and
  // increasing within each group. A week constant holds the period of that many weeks named in
  // weeks: `TENOR_1W` is the period `P7D` named `1W`.

  /** A tenor of 1 day. */
  val TENOR_1D: Tenor = dayTenor(1)
  /** A tenor of 2 days. */
  val TENOR_2D: Tenor = dayTenor(2)
  /** A tenor of 3 days. */
  val TENOR_3D: Tenor = dayTenor(3)
  /** A tenor of 1 week. */
  val TENOR_1W: Tenor = weekTenor(1)
  /** A tenor of 2 weeks. */
  val TENOR_2W: Tenor = weekTenor(2)
  /** A tenor of 3 weeks. */
  val TENOR_3W: Tenor = weekTenor(3)
  /** A tenor of 4 weeks. */
  val TENOR_4W: Tenor = weekTenor(4)
  /** A tenor of 6 weeks. */
  val TENOR_6W: Tenor = weekTenor(6)
  /** A tenor of 13 weeks. */
  val TENOR_13W: Tenor = weekTenor(13)
  /** A tenor of 26 weeks. */
  val TENOR_26W: Tenor = weekTenor(26)
  /** A tenor of 52 weeks. */
  val TENOR_52W: Tenor = weekTenor(52)
  /** A tenor of 1 month. */
  val TENOR_1M: Tenor = monthTenor(1)
  /** A tenor of 2 months. */
  val TENOR_2M: Tenor = monthTenor(2)
  /** A tenor of 3 months. */
  val TENOR_3M: Tenor = monthTenor(3)
  /** A tenor of 4 months. */
  val TENOR_4M: Tenor = monthTenor(4)
  /** A tenor of 5 months. */
  val TENOR_5M: Tenor = monthTenor(5)
  /** A tenor of 6 months. */
  val TENOR_6M: Tenor = monthTenor(6)
  /** A tenor of 7 months. */
  val TENOR_7M: Tenor = monthTenor(7)
  /** A tenor of 8 months. */
  val TENOR_8M: Tenor = monthTenor(8)
  /** A tenor of 9 months. */
  val TENOR_9M: Tenor = monthTenor(9)
  /** A tenor of 10 months. */
  val TENOR_10M: Tenor = monthTenor(10)
  /** A tenor of 11 months. */
  val TENOR_11M: Tenor = monthTenor(11)
  /** A tenor of 12 months, which is not the same value as a tenor of 1 year. */
  val TENOR_12M: Tenor = monthTenor(12)
  /** A tenor of 15 months. */
  val TENOR_15M: Tenor = monthTenor(15)
  /** A tenor of 18 months. */
  val TENOR_18M: Tenor = monthTenor(18)
  /** A tenor of 21 months. */
  val TENOR_21M: Tenor = monthTenor(21)
  /** A tenor of 1 year, which is not the same value as a tenor of 12 months. */
  val TENOR_1Y: Tenor = yearTenor(1)
  /** A tenor of 2 years. */
  val TENOR_2Y: Tenor = yearTenor(2)
  /** A tenor of 3 years. */
  val TENOR_3Y: Tenor = yearTenor(3)
  /** A tenor of 4 years. */
  val TENOR_4Y: Tenor = yearTenor(4)
  /** A tenor of 5 years. */
  val TENOR_5Y: Tenor = yearTenor(5)
  /** A tenor of 6 years. */
  val TENOR_6Y: Tenor = yearTenor(6)
  /** A tenor of 7 years. */
  val TENOR_7Y: Tenor = yearTenor(7)
  /** A tenor of 8 years. */
  val TENOR_8Y: Tenor = yearTenor(8)
  /** A tenor of 9 years. */
  val TENOR_9Y: Tenor = yearTenor(9)
  /** A tenor of 10 years. */
  val TENOR_10Y: Tenor = yearTenor(10)
  /** A tenor of 11 years. */
  val TENOR_11Y: Tenor = yearTenor(11)
  /** A tenor of 12 years. */
  val TENOR_12Y: Tenor = yearTenor(12)
  /** A tenor of 13 years. */
  val TENOR_13Y: Tenor = yearTenor(13)
  /** A tenor of 14 years. */
  val TENOR_14Y: Tenor = yearTenor(14)
  /** A tenor of 15 years. */
  val TENOR_15Y: Tenor = yearTenor(15)
  /** A tenor of 20 years. */
  val TENOR_20Y: Tenor = yearTenor(20)
  /** A tenor of 25 years. */
  val TENOR_25Y: Tenor = yearTenor(25)
  /** A tenor of 30 years. */
  val TENOR_30Y: Tenor = yearTenor(30)
  /** A tenor of 35 years. */
  val TENOR_35Y: Tenor = yearTenor(35)
  /** A tenor of 40 years. */
  val TENOR_40Y: Tenor = yearTenor(40)
  /** A tenor of 45 years. */
  val TENOR_45Y: Tenor = yearTenor(45)
  /** A tenor of 50 years. */
  val TENOR_50Y: Tenor = yearTenor(50)

  /**
   * Obtains a tenor from a period.
   *
   * The period normally consists of either days and weeks, or months and years, and it must be
   * positive and non-zero. A day count that is an exact multiple of seven is named in weeks, so
   * the period `P14D` gives the tenor `2W`; months are not normalised into years, so `P12M`
   * gives `12M` rather than `1Y`.
   *
   * Both checks on the input are performed, so a caller of this factory receives every reason
   * its period was rejected rather than only the first.
   *
   * @param period  the period to convert to a tenor
   * @return the tenor, or the failures naming the conditions the period breaks: it must be of
   *   non-zero length and must hold no negative element
   */
  def of(period: Period): ResultNec[Tenor] =
    Validate.toResult(
      (
        Validate.isFalse(period.isZero, "Tenor period must not be zero"),
        Validate.isFalse(period.isNegative, "Tenor period must not be negative")
      ).mapN((_, _) => create(period))
    )

  /**
   * Obtains a tenor of the specified number of days.
   *
   * A day count that is an exact multiple of seven is named in weeks, so `ofDays(7)` is the
   * tenor `1W` while `ofDays(8)` is the tenor `8D`. Both name the same period the day count
   * describes, since a week is seven days.
   *
   * @param days  the number of days, which must be positive and non-zero
   * @return the tenor, or the failures naming the conditions the day count breaks: it must be
   *   neither zero nor negative
   */
  def ofDays(days: Int): ResultNec[Tenor] = of(Period.ofDays(days))

  /**
   * Obtains a tenor of the specified number of weeks.
   *
   * @param weeks  the number of weeks, which must be positive and non-zero
   * @return the tenor, or the failures naming the conditions the week count breaks: it must be
   *   neither zero nor negative
   */
  def ofWeeks(weeks: Int): ResultNec[Tenor] = of(Period.ofWeeks(weeks))

  /**
   * Obtains a tenor of the specified number of months.
   *
   * Months are not normalised into years, so `ofMonths(12)` is the tenor `12M`, which is a
   * different value from `ofYears(1)`.
   *
   * @param months  the number of months, which must be positive and non-zero
   * @return the tenor, or the failures naming the conditions the month count breaks: it must be
   *   neither zero nor negative
   */
  def ofMonths(months: Int): ResultNec[Tenor] = of(Period.ofMonths(months))

  /**
   * Obtains a tenor of the specified number of years.
   *
   * @param years  the number of years, which must be positive and non-zero
   * @return the tenor, or the failures naming the conditions the year count breaks: it must be
   *   neither zero nor negative
   */
  def ofYears(years: Int): ResultNec[Tenor] = of(Period.ofYears(years))

  /**
   * Parses a tenor from text.
   *
   * The text is either the ISO-8601 form of the period, such as `P3M`, or that form without its
   * leading `P`, such as `3M` - which is the canonical name a tenor renders itself as, so the
   * output of [[Tenor.name]] always parses back to the same tenor. Both forms are accepted
   * because both occur in market data and in stored documents.
   *
   * Text that names no period at all is a parsing failure. Text that names a period which is
   * not a tenor - `-2D`, say - is reported with the reason that period was rejected for, so the
   * caller is told that the period is negative rather than merely that the text was bad. Where
   * the underlying factory has several reasons, they are combined into one, because a parse has
   * a single error channel.
   *
   * The parsing failure quotes the text back as it was given, so the message names the whole of
   * what was refused.
   *
   * ===The text is read by a walk, not by an exception===
   *
   * The period is read by [[PeriodText.readOptionallyPrefixed]], which walks the characters of
   * the text once and answers the period or nothing. It implements the grammar
   * `java.time.Period.parse` implements, to the character, and it is that method's absence from
   * this path that makes a refusal cost nothing: `Period.parse` reports a text it cannot read by
   * throwing a `java.time.format.DateTimeParseException`, which is constructed - message,
   * captured text and stack trace - only to be discarded here, since this method answers a
   * failure value. The walk also reads the leading `P` in place instead of copying the text to
   * add one, so the spelling without the prefix - the canonical name of a tenor, and therefore
   * the spelling the codec reads - no longer allocates a second string. Neither the accepted nor
   * the rejected spelling has moved by a character; [[PeriodText]] states the grammar it agrees
   * with, and `TenorSpec` holds the two to each other over a corpus of texts.
   *
   * ===The grammar's own ceiling is tested first===
   *
   * Text longer than [[MaxTextLength]] characters names no tenor - the grammar of a period puts
   * every name it admits far below that, as the constant explains - and is refused before
   * anything is done with it: before the leading `P` is looked for and before a character of the
   * text is read. That failure names the ceiling rather than the text, which is the wording
   * [[com.opengamma.strata.collect.Decimal]] reports for the same condition. Every text within
   * the ceiling reads exactly as it did, quoted in full when it is refused, so the ceiling is
   * invisible to every caller but the one handing over a payload.
   *
   * @param toParse  the text to parse
   * @return the tenor the text names, or the failure naming what is wrong with the text: it is
   *   not the form of a period, or the period it holds is of zero length or holds a negative
   *   element
   */
  def parse(toParse: String): FailureOr[Tenor] =
    if (toParse.length > MaxTextLength) {
      Left(Failure.Parsing(MaxTextLengthMessage))
    } else {
      PeriodText.readOptionallyPrefixed(toParse) match {
        case Some(period) => of(period).left.map(Failure.collapse)
        case None => Left(Failure.Parsing(s"Unable to parse tenor: '$toParse'"))
      }
    }

  // Creates a tenor from a period that is already known to be positive and non-zero, applying
  // the naming rules: a period of days alone is named in days or weeks, and anything else is
  // named by its ISO-8601 form without the leading `P`.
  //
  // This is the one place a tenor is named, so `of`, the convenience factories, the constants
  // and `normalized` cannot disagree about what a given period is called. It is private, and
  // every caller either validates first or holds an already-valid tenor, which is what keeps the
  // invariant of the type intact without this method needing an error channel.
  private def create(period: Period): Tenor = {
    val dayCount = period.getDays
    if (period.toTotalMonths == 0L && dayCount != 0) {
      dayTenor(dayCount)
    } else {
      // the ISO-8601 form of a period always begins with `P`, which the canonical name drops
      named(period, period.toString.substring(1))
    }
  }

  // The name this type derives from a period, which is the name a tenor of that period carries.
  //
  // This restates the naming rule `create` applies - a period of days alone is named in days, or
  // in weeks where the count is an exact multiple of seven, and anything else by its ISO-8601
  // form without the leading `P` - as a function of a period alone, which is the form the
  // invariant of the type needs: the invariant reads the fields an instance holds, so it has a
  // period and a name and no memory of how either was arrived at.
  //
  // The two statements of the rule cannot drift apart. Every tenor in this file is built through
  // `named`, and every tenor built runs that invariant as part of its construction, so a rule
  // stated here that disagreed with the one applied above would refuse the first constant this
  // companion builds rather than waiting to be noticed.
  private def canonicalName(period: Period): String = {
    val dayCount = period.getDays
    if (period.toTotalMonths == 0L && dayCount != 0) {
      if (dayCount % 7 == 0) s"${dayCount / 7}W" else s"${dayCount}D"
    } else {
      // the ISO-8601 form of a period always begins with `P`, which the canonical name drops
      period.toString.substring(1)
    }
  }

  // Creates a tenor of a day count, naming it in weeks when the count is an exact multiple of
  // seven; the period is the same either way, a week being seven days.
  private def dayTenor(days: Int): Tenor =
    if (days % 7 == 0) weekTenor(days / 7) else named(Period.ofDays(days), s"${days}D")

  private def weekTenor(weeks: Int): Tenor = named(Period.ofWeeks(weeks), s"${weeks}W")

  // Creates a tenor of a month count, which is never normalised into years.
  private def monthTenor(months: Int): Tenor = named(Period.ofMonths(months), s"${months}M")

  private def yearTenor(years: Int): Tenor = named(Period.ofYears(years), s"${years}Y")

  // The only construction of the type. The implementation class supplies the name, which is why
  // the name can be a fixed member of every instance while staying out of the equality the case
  // class derives from its single period element.
  //
  // The constructor of a `sealed abstract case class` is reachable only from inside this file,
  // and this is the sole place in the file that reaches it, so no caller anywhere can build a
  // tenor whose period has not been checked or whose name does not match its period.
  private def named(period: Period, tenorName: String): Tenor = new Impl(period, tenorName)

  /**
   * The one implementation of a tenor.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it. It is declared rather than written as an anonymous subclass at the instantiation site
   * for two reasons, both about what the class file says: a private member class is one a Java
   * compiler refuses to name, where an anonymous class is public and can be instantiated directly
   * by a caller in another language, and a named class can be compared against, which is what
   * lets [[Tenor]] refuse in its own constructor to be any other implementation.
   *
   * The name the abstract class declares is supplied here as a constructor `val`, which is what
   * keeps it off the single case element of the type and therefore out of its equality.
   *
   * @param period  the period of the tenor, already checked to be positive and normalised
   * @param name  the canonical name of that period, as the factory rendered it
   */
  private final class Impl(period: Period, override val name: String) extends Tenor(period)

  /**
   * The ordering of tenors, which is also their hashing and equality.
   *
   * This is the only equality-bearing instance of the type. `Order` and `Hash` both extend `Eq`,
   * so declaring them together is what makes it impossible for the ordering, the hashing and the
   * equality of a tenor to disagree, and it is why no separate `Eq` is declared.
   *
   * Equality and hashing are those of the value itself, which means the period alone, since that
   * is the only element the case class carries. Ordering is [[Tenor.compareTo]] - the comparison
   * by length - followed by a comparison of names where that returns zero. The tie-break is
   * deliberate and is required rather than cosmetic:
   *
   *   - the comparison by length ranks `12M` and `1Y` equal while they are different tenors, so
   *     on its own it cannot be an `Order`, whose laws demand that `compare` return zero exactly
   *     when the values are equal;
   *   - a name is a total, injective function of a period, so two tenors share a name only when
   *     they share a period, which is exactly when they are equal. The tie-break therefore
   *     returns zero in precisely the cases equality holds;
   *   - it cannot reorder a pair the comparison by length ranked strictly, because it is
   *     consulted only after that comparison has returned zero.
   *
   * The effect is a total order in which `12M` sorts immediately before `1Y` and tenors of equal
   * estimated length are grouped.
   *
   * @return the ordering, hashing and equality of tenors
   */
  implicit val order: Order[Tenor] with Hash[Tenor] =
    new Order[Tenor] with Hash[Tenor] {

      private val universal: Hash[Tenor] = Hash.fromUniversalHashCode[Tenor]

      override def compare(x: Tenor, y: Tenor): Int = {
        val byLength = x.compareTo(y)
        if (byLength != 0) byLength else x.name.compareTo(y.name)
      }

      override def eqv(x: Tenor, y: Tenor): Boolean = universal.eqv(x, y)

      override def hash(x: Tenor): Int = universal.hash(x)
    }

  /**
   * The rendering of tenors as text.
   *
   * A tenor renders as its canonical name - `3M`, `1Y`, `1W` - which is what `toString` produces
   * and what [[parse]] accepts.
   *
   * @return the rendering of a tenor
   */
  implicit val show: Show[Tenor] = Show.show(_.name)

  /**
   * The JSON codec for tenors.
   *
   * A tenor is written as the bare string of its canonical name, so the document holds `"3M"`
   * rather than an object. Reading goes through [[parse]], so a document holding `"P3M"` is
   * accepted as well and text that names no tenor is rejected with the message of the parse
   * failure.
   *
   * Because the encoded form is a function of the value alone, two equal tenors always encode to
   * identical bytes.
   *
   * @return the codec reading and writing a tenor as its canonical text
   */
  implicit val codec: Codec[Tenor] = Codecs.parsedStringCodec(parse, _.name)

}

/**
 * Reads the text form of a `java.time.Period` without constructing an exception.
 *
 * The three types of this port that are named by a period - [[Tenor]],
 * [[com.opengamma.strata.basics.date.MarketTenor]], which reads its text through the tenor, and
 * [[com.opengamma.strata.basics.schedule.Frequency]] - all accept the ISO-8601 spelling of a
 * period with the leading `P` and the same spelling without it, and all three answer an `Either`
 * rather than throwing. Reading that text by handing it to `java.time.Period.parse` nevertheless
 * put an exception on every rejection: that method reports text it cannot read by throwing a
 * `java.time.format.DateTimeParseException`, which is constructed in full - message, a copy of
 * the offending text, and a stack trace walked from the throw site - only to be dropped by the
 * `scala.util.Try` that caught it. An acceptance paid for a `java.util.regex` matcher over the
 * text, and a spelling without the prefix paid for a second string to hold the prefixed copy.
 *
 * This object is that read, done as one left-to-right walk of the characters. It allocates
 * nothing whatever when it refuses, and on acceptance only the period it answers with and the
 * `Some` that carries it. It is the sole reader of period text in this module, so the tenor and
 * the frequency cannot come to disagree about which spellings name a period.
 *
 * ===The grammar===
 *
 * The walk implements, character for character, the grammar `java.time.Period.parse` matches,
 * which that method holds as the pattern
 *
 * {{{
 * ([-+]?)P(?:([-+]?[0-9]+)Y)?(?:([-+]?[0-9]+)M)?(?:([-+]?[0-9]+)W)?(?:([-+]?[0-9]+)D)?
 * }}}
 *
 * compiled `CASE_INSENSITIVE` and matched against the '''whole''' of the text, with at least one
 * of the four sections required to be present. Restated as the rules this walk applies:
 *
 *   - the four sections appear in the order years, months, weeks, days; each may be left out and
 *     none may appear twice, which is one statement - a section's unit may not be one an earlier
 *     section has already passed;
 *   - a section is an optional `-` or `+`, then one or more characters in `0`-`9`, then its unit
 *     letter. A unit letter is matched without regard to case, and since the pattern carries
 *     `CASE_INSENSITIVE` without `UNICODE_CASE` that folding is ASCII-only, which this walk
 *     reproduces by accepting the two ASCII spellings of each of the four letters and nothing
 *     else. The digits are ASCII digits alone: a character that `Character.isDigit` would accept
 *     but the pattern's `[0-9]` would not, such as a full-width `１`, is refused;
 *   - the leading `P` is the one letter of the grammar this reader matches in upper case only,
 *     and it is the prefixing rule of [[readOptionallyPrefixed]] rather than the pattern that
 *     decides it: text that does not begin with an upper-case `P` is read as though one preceded
 *     it, so a lower-case `p3m` is read as `P` followed by `p3m` and refused at the `p`, which is
 *     exactly what the pattern did with the prefixed copy `Pp3m`;
 *   - a section's digits are read as an `Int`, so a count outside that range - with or without a
 *     sign, and however many leading zeros precede it - is a refusal, as it is for the
 *     `Integer.parseInt` the pattern's own reader uses;
 *   - the weeks are folded into the days as `days + weeks * 7`, and a product or a sum that no
 *     `Int` holds is a refusal. `java.time.Period.parse` reports that overflow by an
 *     `ArithmeticException` rather than the parse exception, its overflow-checked arithmetic
 *     sitting outside the conversion its own handler covers; both were caught alike by the
 *     `Try` this walk replaced, so a text such as `P2147483647W` is refused here exactly as it
 *     was before;
 *   - anything else is a refusal: text that ends before its unit letter, a character the grammar
 *     does not admit, a section out of order or repeated, a decimal point, a time part such as
 *     `PT1H`, leading or trailing space, a bare `P`, and empty text.
 *
 * Nothing beyond that is checked, because `java.time.Period` itself checks nothing beyond it: a
 * period of any three counts an `Int` holds is a period, and it is the factory of the type being
 * parsed - [[Tenor.of]] or
 * [[com.opengamma.strata.basics.schedule.Frequency.of]] - that decides whether the period it
 * names is one that type admits. That division is what keeps `-2D` reported as a negative period
 * rather than as unreadable text.
 *
 * ===The leading sign===
 *
 * The grammar admits a sign before the `P`, applied to every section, and the entry point here
 * deliberately does not: it is unreachable from the two parses that call it. Each of them reads
 * text whose first character is a `P` - either the text's own upper-case `P` or the one the
 * prefixing rule supplies - so the sign group of the pattern always matched empty. A text that
 * does begin with a sign, `-P2D`, is not prefixed-in-place but read as `P` followed by `-P2D`,
 * whose first section has a sign and then a `P` where a digit is required, and is refused. That
 * is precisely what `java.time.Period.parse` did with the prefixed copy `P-P2D`, so the outcome
 * of every text is unchanged and no unreachable branch is carried to produce it.
 *
 * This object holds no state and every member is a pure function of its arguments, so it is safe
 * to use from any number of threads. It is a second top-level object of this file rather than a
 * member of the tenor's companion so that a frequency reading its own text does not initialise
 * the forty-eight tenor constants that companion holds.
 */
private[basics] object PeriodText {

  /**
   * The position in the grammar at which each unit may be read.
   *
   * The four sections are ordered, so the walk carries the stage it has reached and a unit is
   * admissible only where its own stage is at least that: a unit that an earlier section has
   * passed - one out of order, or a repeat of one already read - is refused by the same
   * comparison, which is why the ordering of the grammar and the uniqueness of a section need no
   * separate statement here.
   */
  private val YearsStage: Int = 0
  private val MonthsStage: Int = 1
  private val WeeksStage: Int = 2
  private val DaysStage: Int = 3

  /**
   * The stage of a character that is not a unit of the grammar.
   *
   * It is below every stage the walk can reach, so a character the grammar does not admit in the
   * position of a unit is refused by the comparison that orders the units, and needs no test of
   * its own.
   */
  private val NoStage: Int = -1

  /**
   * Answered by [[countOf]] for a section whose digits name no `Int`.
   *
   * A count is carried as a `Long` so that the bounds of an `Int` can be tested rather than
   * silently wrapped, which leaves every `Long` outside the range of an `Int` free to mark the
   * refusal. `Long.MinValue` is chosen from those: it is as far from a readable count as a value
   * can be, so a caller that failed to test for it could not mistake it for one.
   */
  private val NoCount: Long = Long.MinValue

  /** The largest count a section may name, which is the largest value an `Int` holds. */
  private val LargestCount: Long = Int.MaxValue.toLong

  /**
   * The largest magnitude a negated section may name.
   *
   * It is one more than [[LargestCount]], the range of an `Int` being asymmetric, and it is the
   * bound `Integer.parseInt` applies to the same digits: `-2147483648` is a count and
   * `2147483648` is not.
   */
  private val LargestNegatedCount: Long = -Int.MinValue.toLong

  /**
   * Reads the period a text spells, where the leading `P` of the grammar may be left off.
   *
   * This is the rule the two parses share: text beginning with an upper-case `P` is read from
   * the character after it, and any other text is read from its first character, as though a `P`
   * preceded it. Only an upper-case `P` counts as one already present, which is what makes `3m`
   * three months and `p3m` unreadable - the lower-case spelling is read as a `P` followed by
   * `p3m`, and a `p` is not the start of a section. The prefix is applied by choosing where to
   * start rather than by building a prefixed copy of the text, so the spelling without it - the
   * canonical name of a tenor - allocates nothing.
   *
   * @param text  the text to read, which the caller has already bounded in length
   * @return the period the text names, or nothing where the text names no period
   */
  def readOptionallyPrefixed(text: String): Option[Period] =
    if (text.isEmpty) {
      // empty text names no period, and reading it as a bare `P` would be the same refusal one
      // step later; it is answered here so that the walk below never begins past the end
      None
    } else {
      val from: Int = if (text.charAt(0) == 'P') 1 else 0
      readSections(text, from, YearsStage, 0, 0, 0, sectionRead = false)
    }

  /**
   * Reads the sections of a period from the text, one section per step.
   *
   * The walk is tail recursive, so text of any length is read in constant stack space, and it
   * carries what it has read rather than holding it in mutable state: the counts of the three
   * units a period holds, the stage the grammar has reached, and whether any section has been
   * read at all - the last being the one condition the pattern of the grammar cannot express,
   * since every section of it is optional and a bare `P` therefore matches while naming no
   * period.
   *
   * The weeks of the grammar are not carried, because a period holds no weeks: a week section is
   * folded into the days as it is read, which is safe in exactly the same way the folding at the
   * end of the pattern's own reader is - a week section may be read only while the stage is at
   * or before the weeks, so the days are still zero when it arrives, and a day section that
   * follows adds to what it left. The two overflow tests are therefore the two the pattern's
   * reader applies, in the same order and over the same values.
   *
   * @param text  the text being read
   * @param index  the index to read the next section at
   * @param stage  the earliest stage of the grammar a unit may now name
   * @param years  the years read so far
   * @param months  the months read so far
   * @param days  the days read so far, including any week section already folded into them
   * @param sectionRead  whether any section has been read, which at least one must have been
   * @return the period the text names, or nothing where it names no period
   */
  @tailrec
  private def readSections(
      text: String,
      index: Int,
      stage: Int,
      years: Int,
      months: Int,
      days: Int,
      sectionRead: Boolean): Option[Period] =
    if (index == text.length) {
      // the text is spent: it names a period if it held a section, and `Period.of` answers the
      // zero period for three zero counts exactly as the pattern's own reader does
      if (sectionRead) Some(Period.of(years, months, days)) else None
    } else {
      val leading: Char = text.charAt(index)
      val negated: Boolean = leading == '-'
      val digitsFrom: Int = if (negated || leading == '+') index + 1 else index
      val digitsTo: Int = digitsEnd(text, digitsFrom)
      val unit: Int = if (digitsTo < text.length) stageOf(text.charAt(digitsTo)) else NoStage
      if (digitsTo == digitsFrom || unit < stage) {
        // a section with no digits, a unit the grammar does not admit, a unit already passed, or
        // digits the text ended before the unit of
        None
      } else {
        val count: Long = countOf(text, digitsFrom, digitsTo, negated)
        val next: Int = digitsTo + 1
        if (count == NoCount) {
          None
        } else if (unit == YearsStage) {
          readSections(text, next, unit + 1, count.toInt, months, days, sectionRead = true)
        } else if (unit == MonthsStage) {
          readSections(text, next, unit + 1, years, count.toInt, days, sectionRead = true)
        } else if (unit == WeeksStage) {
          val asDays: Long = count * 7L
          if (asDays < -LargestNegatedCount || asDays > LargestCount) {
            None
          } else {
            readSections(text, next, unit + 1, years, months, asDays.toInt, sectionRead = true)
          }
        } else {
          // the days, this being the last stage: a unit below the stage reached was refused
          // above, and the character that is not one of the four units has no stage at all
          val totalDays: Long = days.toLong + count
          if (totalDays < -LargestNegatedCount || totalDays > LargestCount) {
            None
          } else {
            readSections(text, next, unit + 1, years, months, totalDays.toInt, sectionRead = true)
          }
        }
      }
    }

  /**
   * Returns the index at which the run of digits starting at the specified index ends.
   *
   * The run may be empty, in which case the index is answered unchanged and the caller refuses
   * the section: the grammar requires at least one digit. Only the ten ASCII digits are read,
   * which is what the `[0-9]` of the pattern admits and is narrower than `Character.isDigit`.
   *
   * @param text  the text being read
   * @param index  the index the digits start at
   * @return the index one past the last digit, which is the index given where there are none
   */
  @tailrec
  private def digitsEnd(text: String, index: Int): Int =
    if (index < text.length && isAsciiDigit(text.charAt(index))) {
      digitsEnd(text, index + 1)
    } else {
      index
    }

  /**
   * Whether a character is one of the ten digits the grammar admits.
   *
   * @param character  the character to test
   * @return true where the character is an ASCII digit
   */
  private def isAsciiDigit(character: Char): Boolean = character >= '0' && character <= '9'

  /**
   * Returns the stage of the grammar the specified unit letter names.
   *
   * Both spellings of each letter are admitted and nothing else, which is the ASCII-only folding
   * the pattern's `CASE_INSENSITIVE` performs in the absence of `UNICODE_CASE`. The comparison is
   * against the characters themselves rather than against a case-folded copy of the text, so it
   * neither allocates nor depends on a locale.
   *
   * @param unit  the character in the position of a unit letter
   * @return the stage the unit names, or [[NoStage]] where the character is not a unit
   */
  private def stageOf(unit: Char): Int = unit match {
    case 'Y' | 'y' => YearsStage
    case 'M' | 'm' => MonthsStage
    case 'W' | 'w' => WeeksStage
    case 'D' | 'd' => DaysStage
    case _ => NoStage
  }

  /**
   * Reads the count a section's digits name, applying the sign the section carried.
   *
   * The magnitude is accumulated as a `Long` against the bound of an `Int` - the bound being the
   * larger by one where the section is negated, the range of an `Int` being asymmetric - so a
   * count no `Int` holds is answered as [[NoCount]] rather than wrapping. This is the arithmetic
   * `Integer.parseInt` performs on the same digits, and it reports the same refusals: leading
   * zeros are read and do not count towards the bound, and a magnitude past the bound is refused
   * however long the run of digits naming it.
   *
   * @param text  the text being read
   * @param from  the index of the first digit
   * @param until  the index one past the last digit
   * @param negated  whether the section carried a `-`
   * @return the count the digits name, or [[NoCount]] where no `Int` holds it
   */
  private def countOf(text: String, from: Int, until: Int, negated: Boolean): Long = {
    val bound: Long = if (negated) LargestNegatedCount else LargestCount
    val magnitude: Long = magnitudeOf(text, from, until, bound, 0L)
    if (magnitude == NoCount || !negated) magnitude else -magnitude
  }

  /**
   * Accumulates the magnitude of a run of digits, refusing one that passes the bound.
   *
   * The accumulation is tail recursive and stops at the first digit that carries it past the
   * bound, so a run of any length is read in constant stack space and the accumulator itself
   * cannot overflow: it is never larger than the bound when a digit is read, and the bound is
   * that of an `Int`.
   *
   * @param text  the text being read
   * @param index  the index of the next digit to read
   * @param until  the index one past the last digit
   * @param bound  the largest magnitude the section may name
   * @param accumulated  the magnitude of the digits read so far
   * @return the magnitude of the whole run, or [[NoCount]] where it passes the bound
   */
  @tailrec
  private def magnitudeOf(
      text: String,
      index: Int,
      until: Int,
      bound: Long,
      accumulated: Long): Long =
    if (index == until) {
      accumulated
    } else {
      val grown: Long = accumulated * 10L + (text.charAt(index) - '0').toLong
      if (grown > bound) NoCount else magnitudeOf(text, index + 1, until, bound, grown)
    }

}
