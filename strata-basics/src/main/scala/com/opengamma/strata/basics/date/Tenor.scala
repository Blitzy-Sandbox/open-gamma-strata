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

import scala.util.Try

import cats.Hash
import cats.Order
import cats.Show
import cats.syntax.apply._

import io.circe.Codec

import com.opengamma.strata.collect.FailureOr
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
 * Equality and hashing consider the '''period alone''', exactly as in the library being ported:
 * the name is derived from the period, so it carries no information equality could use, and
 * two tenors with the same period are the same tenor however they were built. `12M` and `1Y`
 * are consequently '''not''' equal, having different periods.
 *
 * The ordering is that of the original - day-only tenors by days, month-only tenors by total
 * months, and anything else by an estimated length in days, obtained by dividing months by
 * twelve and multiplying by the mean Gregorian year of 365.2425 days - with '''one deliberate
 * addition''': tenors the original ranks equal are then ordered by name. The original ranks
 * `12M` and `1Y` equal while holding them unequal, which is a contradiction a `cats.Order` is
 * not allowed to carry, since its laws require `compare` to return zero exactly when the values
 * are equal. The name is a total, injective function of the period, so breaking the tie with it
 * restores the law without reordering any pair the original ranked strictly. [[compareTo]]
 * remains available for the unrefined comparison of the original, and the divergence is
 * recorded in `SCALA_MIGRATION.md`.
 *
 * ===Deliberate divergences from the type being ported===
 *
 *   - '''`java.time.temporal.TemporalAmount` is not implemented.''' That interface requires
 *     `getUnits` to return a list from the Java collections framework, which would place a Java
 *     collection on the public API of this port - precisely what the migration forbids, and what
 *     the bytecode audit of the public surface rejects. The three useful members
 *     survive as ordinary methods: [[get]], [[addTo]] and [[subtractFrom]], with [[units]]
 *     returning a Scala `List`. The consequence for callers is that `date.plus(tenor)` is
 *     written `tenor.addTo(date)`, whose `LocalDate` overload returns a `LocalDate` rather than
 *     a `Temporal`, so no cast is needed at the call site.
 *   - '''`Comparable` is not implemented.''' The `cats.Order` instance on the companion is the
 *     ordering of this port, and [[compareTo]] keeps the original's algorithm available to it.
 *   - '''Java serialization and annotation-driven string conversion are not supported.''' The
 *     JSON codec on the companion is the only serialized form, and it writes the canonical name.
 *
 * ===Construction===
 *
 * There is no public constructor, no `apply` and no `copy`: a value of this type exists only
 * because one of the companion's factories accepted its input, so every tenor in a program is
 * positive and non-zero by construction. The factories report a rejected input as a failure
 * rather than by throwing, and [[Tenor.of]] accumulates, so a caller learns every reason its
 * input was unacceptable at once.
 *
 * This type is immutable and every member is a pure function of the value and its arguments, so
 * it is safe to share between threads without synchronisation.
 *
 * @param period  the period of the tenor, which is always positive and non-zero
 */
sealed abstract case class Tenor private (period: Period) {

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
   * The result is a tenor of equivalent length in canonical form. A period of exactly one year
   * becomes twelve months - so `1Y` normalizes to `12M`, which is the convention of the library
   * being ported - and any other period is reduced by `Period.normalized`, which carries excess
   * months into years: `20M` becomes `1Y8M` and `24M` becomes `2Y`. A tenor already in canonical
   * form is returned unchanged, and normalizing twice therefore gives the same value as
   * normalizing once.
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
   * `java.time`, which raises `UnsupportedTemporalTypeException` - the behaviour of the library
   * being ported, and of every other `java.time` amount.
   *
   * @param unit  the unit to query
   * @return the value of the tenor in the requested unit
   */
  def get(unit: TemporalUnit): Long = period.get(unit)

  /**
   * The units a tenor is measured in: years, then months, then days.
   *
   * This replaces the `getUnits` member of the interface this type no longer implements, and
   * returns an immutable Scala `List` in place of that interface's Java list. The contents and
   * their order are those of the period underlying every tenor.
   *
   * @return the units of a tenor, in descending order of size
   */
  def units: List[TemporalUnit] = Tenor.SupportedUnits

  /**
   * Adds the tenor to the specified date.
   *
   * This is the overload callers normally want, and it is the fast path of the library being
   * ported: the months of the tenor are added to the date and the days are then applied by the
   * module's own date arithmetic, which is exact and cheaper than the general route through
   * `java.time`. The result is a `LocalDate`, so it composes with the rest of this module
   * without a cast.
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
   * This mirrors [[addTo]] exactly, including its fast path and its clamping of the
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
   * Compares this tenor to another tenor by length, as the library being ported does.
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
   * point of having both. This method reproduces the original exactly, which means it returns
   * zero for tenors of equal estimated length that are not equal - `12M` against `1Y` being the
   * standard example. The `Order` instance calls this method first and then breaks such a tie by
   * name, which is what its laws require; a caller reproducing the behaviour of the original
   * wants this method, and a caller sorting or keying a collection wants the instance.
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
      // mixed, so months are estimated in days; every conversion here is explicit, because the
      // build rejects a silent widening of one of these integral values to a Double
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
 * The constants carry the names they have in the library being ported - `TENOR_3M`, `TENOR_1Y`
 * - so that call sites, stored data and documentation continue to read the same way after the
 * migration. They are values rather than results, because their inputs are known to be
 * acceptable, and they are built by the same private code the factories use, so a constant and
 * the equivalent factory call produce equal tenors with identical names.
 *
 * ===Factories===
 *
 * Every public factory reports a rejected input as a failure rather than by throwing, which is
 * what makes the type total in the sense the migration requires: the only way to hold a tenor
 * is to have had an acceptable input accepted. Two inputs are unacceptable, and the messages are
 * those of the original:
 *
 *   - a period of zero length - `Tenor period must not be zero`;
 *   - a period with any negative element - `Tenor period must not be negative`.
 *
 * [[of]] checks both at once and reports every failing check, so a caller does not have to fix
 * one problem to discover the next. The four convenience factories express the original's
 * behaviour by routing through [[of]], which is exactly equivalent for every input and keeps the
 * naming rules in one place.
 *
 * ===Instances===
 *
 * The companion declares one equality-bearing instance, one rendering and one codec, which is
 * the convention of this port: `Order` and `Hash` both extend `Eq`, so declaring them as a
 * single value makes it impossible for equality and ordering to disagree, and there is
 * deliberately no separate `Eq`.
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

  //-------------------------------------------------------------------------
  // The tenors in common use, declared in the order of the library being ported and under its
  // names, so that a reader moving between the two finds the same identifiers in the same place.
  // Each is built by the private builder matching the factory the original used, which is what
  // makes the day-count constants weeks: `TENOR_1W` is the period `P7D` named `1W`, exactly as
  // `ofWeeks(1)` produces.

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

  //-------------------------------------------------------------------------
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
   * @return the tenor, or the failures describing why the period is not a tenor
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
   * @return the tenor, or the failures describing why the day count is not a tenor
   */
  def ofDays(days: Int): ResultNec[Tenor] = of(Period.ofDays(days))

  /**
   * Obtains a tenor of the specified number of weeks.
   *
   * @param weeks  the number of weeks, which must be positive and non-zero
   * @return the tenor, or the failures describing why the week count is not a tenor
   */
  def ofWeeks(weeks: Int): ResultNec[Tenor] = of(Period.ofWeeks(weeks))

  /**
   * Obtains a tenor of the specified number of months.
   *
   * Months are not normalised into years, so `ofMonths(12)` is the tenor `12M`, which is a
   * different value from `ofYears(1)`.
   *
   * @param months  the number of months, which must be positive and non-zero
   * @return the tenor, or the failures describing why the month count is not a tenor
   */
  def ofMonths(months: Int): ResultNec[Tenor] = of(Period.ofMonths(months))

  /**
   * Obtains a tenor of the specified number of years.
   *
   * @param years  the number of years, which must be positive and non-zero
   * @return the tenor, or the failures describing why the year count is not a tenor
   */
  def ofYears(years: Int): ResultNec[Tenor] = of(Period.ofYears(years))

  //-------------------------------------------------------------------------
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
   * @param toParse  the text to parse
   * @return the tenor the text names, or the failure describing why it names none
   */
  def parse(toParse: String): FailureOr[Tenor] = {
    val prefixed = if (toParse.startsWith("P")) toParse else s"P$toParse"
    Try(Period.parse(prefixed)).toEither match {
      case Right(period) => of(period).left.map(Failure.collapse)
      case Left(_) => Left(Failure.Parsing(s"Unable to parse tenor: '$toParse'"))
    }
  }

  //-------------------------------------------------------------------------
  // Creates a tenor from a period that is already known to be positive and non-zero, applying
  // the naming rules of the library being ported: a period of days alone is named in days or
  // weeks, and anything else is named by its ISO-8601 form without the leading `P`.
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

  // Creates a tenor of a day count, naming it in weeks when the count is an exact multiple of
  // seven; the period is the same either way, a week being seven days.
  private def dayTenor(days: Int): Tenor =
    if (days % 7 == 0) weekTenor(days / 7) else named(Period.ofDays(days), s"${days}D")

  // Creates a tenor of a week count.
  private def weekTenor(weeks: Int): Tenor = named(Period.ofWeeks(weeks), s"${weeks}W")

  // Creates a tenor of a month count, which is never normalised into years.
  private def monthTenor(months: Int): Tenor = named(Period.ofMonths(months), s"${months}M")

  // Creates a tenor of a year count.
  private def yearTenor(years: Int): Tenor = named(Period.ofYears(years), s"${years}Y")

  // The only construction of the type. The anonymous subclass supplies the name, which is why
  // the name can be a `val` on every instance while staying out of the equality the case class
  // derives from its single period element - the equality of the library being ported.
  //
  // The constructor of a `sealed abstract case class` is reachable only from inside this file,
  // and this is the sole place in the file that reaches it, so no caller anywhere can build a
  // tenor whose period has not been checked or whose name does not match its period.
  private def named(period: Period, tenorName: String): Tenor =
    new Tenor(period) {
      override val name: String = tenorName
    }

  //-------------------------------------------------------------------------
  /**
   * The ordering of tenors, which is also their hashing and equality.
   *
   * This is the only equality-bearing instance of the type. `Order` and `Hash` both extend `Eq`,
   * so declaring them together is what makes it impossible for the ordering, the hashing and the
   * equality of a tenor to disagree, and it is why no separate `Eq` is declared.
   *
   * Equality and hashing are those of the value itself, which means the period alone, since that
   * is the only element the case class carries. Ordering is [[Tenor.compareTo]] - the comparison
   * by length of the library being ported - followed by a comparison of names where that returns
   * zero. The tie-break is a deliberate divergence, recorded in `SCALA_MIGRATION.md`, and it is
   * required rather than cosmetic:
   *
   *   - the original ranks `12M` and `1Y` equal while holding them unequal, so its comparison
   *     cannot be an `Order`, whose laws demand that `compare` return zero exactly when the
   *     values are equal;
   *   - a name is a total, injective function of a period, so two tenors share a name only when
   *     they share a period, which is exactly when they are equal. The tie-break therefore
   *     returns zero in precisely the cases equality holds;
   *   - it cannot reorder a pair the original ranked strictly, because it is consulted only
   *     after the original's comparison has returned zero.
   *
   * The effect is a total order in which `12M` sorts immediately before `1Y`, tenors of equal
   * estimated length are grouped, and every other pair keeps the order the original gave it.
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
   * rather than an object, and the text is identical to the one the library being ported wrote
   * through its own string conversion. Reading goes through [[parse]], so a document holding
   * `"P3M"` is accepted as well and text that names no tenor is rejected with the message of the
   * parse failure.
   *
   * Because the encoded form is a function of the value alone, two equal tenors always encode to
   * identical bytes, which is the stability the round-trip tests of this port require.
   *
   * @return the codec reading and writing a tenor as its canonical text
   */
  implicit val codec: Codec[Tenor] = Codecs.parsedStringCodec(parse, _.name)

}
