/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import java.time.LocalDate
import java.time.LocalTime
import java.time.ZoneId
import java.time.ZonedDateTime

import scala.annotation.tailrec

import cats.Hash
import cats.Order
import cats.Show
import cats.data.EitherNec
import cats.data.NonEmptyList

import io.circe.Codec

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.CurrencyPair
import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.basics.date.BusinessDayConvention
import com.opengamma.strata.basics.date.BusinessDayConventions
import com.opengamma.strata.basics.date.DayCount
import com.opengamma.strata.basics.date.DayCounts
import com.opengamma.strata.basics.date.DaysAdjustment
import com.opengamma.strata.basics.date.HolidayCalendar
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.basics.date.HolidayCalendarIds
import com.opengamma.strata.basics.date.PeriodAdditionConvention
import com.opengamma.strata.basics.date.PeriodAdditionConventions
import com.opengamma.strata.basics.date.Tenor
import com.opengamma.strata.basics.date.TenorAdjustment
import com.opengamma.strata.basics.location.Country
import com.opengamma.strata.basics.schedule.Frequency
import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.Failure

/**
 * An index of an observable value, such as an interest rate, a price level or an exchange rate.
 *
 * An index is an agreed mechanism for determining a financial indicator - an interest rate, a
 * measure of inflation, an exchange rate - which an instrument refers to rather than carries: a
 * swap leg pays what `GBP-LIBOR-3M` fixed at, and the trade records the index while the figure
 * itself arrives as market data. Most indices publish daily. Every index is identified by a name
 * - `GBP-LIBOR-3M`, `EUR-ESTR`, `GB-RPI`, `EUR/USD-ECB` - and that name is the index's identity
 * throughout this library, in text, in JSON and as a map key.
 *
 * This is the abstraction over every kind of index and carries nothing but the name; see
 * [[IborIndex]], [[OvernightIndex]], [[PriceIndex]] and [[FxIndex]] for the four concrete
 * families and the fields each of them holds.
 *
 * ===Why the hierarchy is closed, and why it is one file===
 *
 * The set of indices is reference data, not an extension point: an application refers to the
 * indices the market publishes, and this library knows what those are. The hierarchy is
 * therefore `sealed`, so that a match over it is checked for exhaustiveness by the compiler and
 * no index can exist that this library did not publish - which is what replaces the mutable,
 * classpath-driven registry the implementation being ported resolved a name through. Scala
 * requires every direct subtype of a sealed type to be declared in the same file as the type,
 * which is why this one file holds the whole hierarchy: the three abstractions and all four
 * concrete families. The named constants of each family, and the reference data its members are
 * built from, live in their own files, because those only refer to members and extend nothing.
 *
 * The abstractions are three, and they exist because different code needs different guarantees:
 *
 *   - `Index` is any index at all, which is what an instrument refers to.
 *   - [[RateIndex]] is an index of an interest rate, which is what code computing interest
 *     needs: it excludes price and exchange-rate indices.
 *   - [[FloatingRateIndex]] is an index whose figure is a floating rate, which is what code
 *     resolving a [[FloatingRate]] name to a concrete index needs.
 *
 * ===Implementation notes===
 *
 * Every member of every family is an immutable value created once, when its family is first
 * used, and is therefore safe to share between threads. Two members of one family are equal
 * when their names are equal, and each name exists exactly once, so equality, hashing and
 * reference identity agree throughout.
 *
 * This trait has no JSON codec: it carries no data, and every value reaching it is a member of
 * one of the four families, each of which publishes a codec that writes the member's name.
 *
 * @see [[FloatingRate]] for the abstraction over an index and the family it belongs to
 * @see [[FloatingRateName]] for the family identifier a floating rate index reports
 */
sealed trait Index extends Named

/**
 * Resolves text naming an index of any of the four families, and holds what the families share.
 *
 * ===The search order===
 *
 * [[Index.valueOf]] and [[Index.parse]] search the four families in one fixed order - Ibor
 * index, Overnight index, Price index, then FX index - and answer with the first member found.
 * The order is part of the contract rather than an implementation detail, because the name
 * spaces of the families may overlap and the order is what decides which family answers; it
 * reproduces the order of the combined lookup being ported and must not be rearranged.
 *
 * Each family is probed through its own `valueOf`, the exact, alias-aware lookup: it applies the
 * family's alternate-name table and matches the result against the family's canonical and
 * upper-case keys, so `EUR-ESTER` resolves to the `EUR-ESTR` Overnight index. What a probe
 * deliberately does not do is apply a family's lenient rewrites - none of the four families
 * declares any - which is also what keeps an earlier family from claiming text that a later one
 * matches precisely.
 *
 * Neither operation forces a family that it does not consult: the probes are reached in turn, so
 * resolving an Ibor index initialises the Ibor family alone.
 */
object Index {

  /**
   * The label this abstraction reports in the failure of [[parse]].
   *
   * Held once so that the abstraction cannot describe itself differently in two messages, and
   * equal to the label the implementation being ported used in the same failure.
   */
  private val FamilyName: String = "Index"

  /**
   * Looks up an index of any family by name, answering with nothing when no family has it.
   *
   * The four families are searched in the order documented on this object and the first member
   * found is returned. The lookup is exact and alias-aware, so an alternate spelling such as
   * `USD-FEDFUND` resolves while text differing only in case does not resolve unless the
   * family registers that spelling; use [[parse]] to obtain a failure rather than an absent
   * value.
   *
   * @param name  the index name, such as `GBP-LIBOR-3M`, `EUR-ESTR`, `GB-RPI` or `EUR/USD-ECB`
   * @return the index of that name, or nothing when no family publishes it
   */
  def valueOf(name: String): Option[Index] = {
    val ibor: Option[Index] = IborIndex.valueOf(name)
    ibor
      .orElse(OvernightIndex.valueOf(name))
      .orElse(PriceIndex.valueOf(name))
      .orElse(FxIndex.valueOf(name))
  }

  /**
   * Parses text naming an index of any family, reporting a failure when it names none.
   *
   * This is [[valueOf]] with an absent value reported as
   * [[com.opengamma.strata.collect.result.Failure.Parsing]], carrying the message the
   * implementation being ported used for the error it raised in the same situation. The text
   * the failure names is rendered through
   * [[com.opengamma.strata.collect.result.Failure.describeInput]], so a message reaching a log
   * or a report is bounded in length and cannot forge a line of it.
   *
   * @param name  the text to parse, such as `GBP-LIBOR-3M`
   * @return the index that the text names, or a failure describing the text that named none
   */
  def parse(name: String): Either[Failure, Index] =
    valueOf(name).toRight(Failure.Parsing(s"$FamilyName name not found: ${Failure.describeInput(name)}"))

  //-------------------------------------------------------------------------
  /**
   * Reports a breach of an invariant of this module's own reference data, fail-fast.
   *
   * Two construction steps in this file can fail only if the transcribed index data is
   * internally inconsistent - a tenor whose addition convention cannot be applied to it, and an
   * index whose name does not end in its own tenor - and one accessor can fail only if an index
   * names a floating rate family that is not published. None of the three depends on caller
   * input, so none of them is a data-dependent failure to be reported through `Either`: each is
   * a defect in this library that has to surface at once and loudly, which is what the
   * constructors being ported did when they raised an error while the family was being created.
   *
   * The failure is raised through the module's single sanctioned fail-fast channel,
   * [[com.opengamma.strata.collect.ArgCheck]], so that every invariant breach in the library
   * reports the same kind of error; the second statement is unreachable and exists only because
   * the first is declared to return no value, while this operation must produce one of any type
   * its callers ask for.
   *
   * @param message  the description of the breached invariant
   * @return never returns normally
   */
  private[index] def invariantFailure(message: String): Nothing = {
    ArgCheck.isTrue(false, message)
    sys.error(message)
  }
}

//-------------------------------------------------------------------------
/**
 * An index whose figure is a floating rate.
 *
 * This is the subset of [[Index]] that a [[FloatingRate]] name can resolve to: the Ibor,
 * Overnight and Price indices, but not the exchange-rate indices, whose figure is a rate of
 * exchange rather than a rate of interest. It is the type [[FloatingRate.tryParse]] answers
 * with when the text it was given names a concrete index rather than a family.
 *
 * Beyond the name of an index it adds what a rate needs to be understood: the currency the rate
 * is quoted in, whether the rate is still published, the day count it accrues on, and the day
 * count a fixed leg swapped against it conventionally uses.
 */
sealed trait FloatingRateIndex extends Index with FloatingRate {

  /**
   * Gets the currency of the index.
   *
   * Every concrete index carries its currency as a field, so this accessor is total. The
   * abstraction above this one, [[FloatingRate]], deliberately does not declare a currency,
   * because a floating rate ''name'' derives one by converting itself into an index first and
   * so cannot answer without the possibility of failure; see the note on that trait.
   *
   * @return the currency of the index
   */
  def currency: Currency

  /**
   * Gets whether the index is active.
   *
   * Over time some indices cease to be published. An inactive index is retained here, and
   * remains resolvable by name, because trades referencing it outlive its publication: a
   * historic trade must still describe the rate it was written against. Whether a rate is
   * published today is a fact about the world rather than about this library, so the flag is
   * data like any other field and is not consulted by any operation of this hierarchy.
   *
   * @return true when the index is still published
   */
  def active: Boolean

  /**
   * Gets the day count convention of the index.
   *
   * This is the convention the rate of the index accrues on, which is what converts a period
   * between two dates into the year fraction a rate is applied to.
   *
   * @return the day count convention
   */
  def dayCount: DayCount

  /**
   * Gets the day count convention of the fixed leg conventionally swapped against this index.
   *
   * It defaults to the day count of the index itself, as in the interface being ported, and the
   * families that publish the convention as reference data override it. The two differ in
   * practice - one Overnight rate accrues on one convention while its fixed leg is quoted on
   * another - so neither may be derived from the other.
   *
   * @return the day count convention of the conventional fixed leg
   */
  def defaultFixedLegDayCount: DayCount = dayCount

  /**
   * The name of the floating rate family this index belongs to, as text.
   *
   * This is the state behind [[floatingRateName]] and exists in this shape for one reason: the
   * families of index and the families of floating rate name refer to each other, and an index
   * that held a resolved [[FloatingRateName]] in a field would make the two mutually dependent
   * at creation time, so that whichever of them a program happened to touch first could observe
   * the other half-built. Holding the name as text and resolving it per call breaks that cycle,
   * which is exactly how the implementation being ported avoided it.
   *
   * @return the name of the floating rate family, such as `GBP-LIBOR` for `GBP-LIBOR-3M`
   */
  private[index] def floatingRateNameId: String

  /**
   * Gets the floating rate name identifying the family that this index belongs to.
   *
   * For an Ibor index this is the family without the tenor, so the three-month and six-month
   * sterling Libor indices report the same family. For an Overnight or Price index the index
   * name is itself the family name.
   *
   * The family is resolved when this is called, through the family's own alias-aware lookup,
   * and is deliberately not held in a field; see [[floatingRateNameId]] for why. An index whose
   * family is not published breaches an invariant of this module's reference data rather than
   * describing a bad argument, so it is reported through the fail-fast channel, as the accessor
   * being ported did.
   *
   * @return the floating rate name of the family of this index
   */
  final def floatingRateName: FloatingRateName =
    FloatingRateIndex.familyOf(floatingRateNameId, name)
}

/**
 * Resolves text naming a concrete floating rate index, and holds the rule that converts a
 * floating rate family into one.
 *
 * ===The search order===
 *
 * [[FloatingRateIndex.valueOf]] and [[FloatingRateIndex.parse]] search three families in one
 * fixed order - Ibor index, Overnight index, then Price index - and answer with the first
 * member found, which is the order of the combined lookup being ported. Each family is probed
 * through its own exact, alias-aware `valueOf`, never through its lenient parse, for the reason
 * given on [[Index]].
 *
 * ===Text that names a family rather than an index===
 *
 * Text reaching this abstraction may name a concrete index, `GBP-LIBOR-3M`, or a whole family
 * of rates, `GBP-LIBOR`. The second has to be converted into an index before it can be used,
 * and the conversion needs a tenor - the family says nothing about the period a rate covers -
 * so the operation being ported takes the tenor to use and falls back to the family's own
 * default tenor when none is supplied.
 *
 * That conversion is the property of the floating rate family, which holds the index name a
 * tenor is appended to and the kind of rate it describes; it is not a property of this
 * hierarchy. [[FloatingRateIndex.tryParseWith]] is therefore the rule alone - search the two
 * name spaces, pass a concrete index through, and hand a family to the conversion it was given
 * - stated over the conversion rather than over any particular one, which is the same
 * separation [[FloatingRate.tryParseWith]] makes between the search and the composition it
 * searches. A caller pairs it with the conversion the family publishes:
 *
 * {{{
 * FloatingRateIndex.tryParseWith(text, family => family.toFloatingRateIndex(tenor).toOption)
 * }}}
 *
 * Both operations report a conversion that did not produce an index the same way they report
 * text that named nothing: [[FloatingRateIndex.tryParseWith]] with an absent value and
 * [[FloatingRateIndex.parseWith]] with a failure. The implementation being ported raised an
 * error from the conversion instead, and the message of that error is the message of the
 * failure the conversion itself reports.
 */
object FloatingRateIndex {

  /**
   * The label this abstraction reports in the failure of [[parse]].
   */
  private val FamilyName: String = "FloatingRateIndex"

  /**
   * Looks up a concrete floating rate index by name, answering with nothing when none has it.
   *
   * The Ibor, Overnight and Price families are searched in that order and the first member
   * found is returned; an exchange-rate index is never answered with, because it is not a
   * floating rate index. The lookup is exact and alias-aware.
   *
   * @param name  the index name, such as `GBP-LIBOR-3M`, `EUR-ESTR` or `GB-RPI`
   * @return the index of that name, or nothing when none of the three families publishes it
   */
  def valueOf(name: String): Option[FloatingRateIndex] = {
    val ibor: Option[FloatingRateIndex] = IborIndex.valueOf(name)
    ibor
      .orElse(OvernightIndex.valueOf(name))
      .orElse(PriceIndex.valueOf(name))
  }

  /**
   * Parses text naming a concrete floating rate index, reporting a failure when it names none.
   *
   * @param name  the text to parse, such as `GBP-LIBOR-3M`
   * @return the index that the text names, or a failure describing the text that named none
   */
  def parse(name: String): Either[Failure, FloatingRateIndex] =
    valueOf(name).toRight(Failure.Parsing(s"$FamilyName name not found: ${Failure.describeInput(name)}"))

  /**
   * Tries to parse text naming either a concrete floating rate index or a family of rates,
   * converting a family with the supplied conversion.
   *
   * The text is resolved through [[FloatingRate.tryParse]], which searches the three index
   * families and then the floating rate names, in that order. A concrete index is passed
   * through unchanged; a family is handed to `convert`, which decides which member of the
   * family the caller meant. Text naming neither, and a conversion that produced no index, both
   * answer with nothing.
   *
   * @param indexStr  the text to parse, such as `GBP-LIBOR-3M` or `GBP-LIBOR`
   * @param convert  the conversion of a floating rate family into one of its indices
   * @return the index the text names, or nothing when it names none or the conversion produced
   *   none
   */
  def tryParseWith(
      indexStr: String,
      convert: FloatingRateName => Option[FloatingRateIndex]): Option[FloatingRateIndex] =
    FloatingRate.tryParse(indexStr).flatMap {
      case index: FloatingRateIndex => Some(index)
      case family: FloatingRateName => convert(family)
      // `FloatingRate` is deliberately open, so a value of a third implementor kind may reach
      // this point. Such a value names no member of this closed hierarchy, which is exactly
      // what an absent answer says.
      case _ => None
    }

  /**
   * Parses text naming either a concrete floating rate index or a family of rates, converting a
   * family with the supplied conversion and reporting a failure when nothing was named.
   *
   * This is [[tryParseWith]] with an absent value reported as
   * [[com.opengamma.strata.collect.result.Failure.Parsing]], carrying the message the
   * implementation being ported used for the error it raised in the same situation.
   *
   * @param indexStr  the text to parse, such as `GBP-LIBOR-3M` or `GBP-LIBOR`
   * @param convert  the conversion of a floating rate family into one of its indices
   * @return the index the text names, or a failure describing the text that named none
   */
  def parseWith(
      indexStr: String,
      convert: FloatingRateName => Option[FloatingRateIndex]): Either[Failure, FloatingRateIndex] =
    tryParseWith(indexStr, convert)
      .toRight(Failure.Parsing(s"Floating rate index not known: ${Failure.describeInput(indexStr)}"))

  //-------------------------------------------------------------------------
  /**
   * Resolves the floating rate family of an index from the name the index holds.
   *
   * The lookup is the family's own alias-aware one, so a retired spelling resolves exactly as
   * it does for a caller. Every index published by this library names a published family - each
   * family name is derived from the index name by the rule of the index's own family, and the
   * two tables are pinned against each other by the closedness specification - so an absent
   * family is a defect in this module rather than a bad argument, and is reported through
   * [[Index.invariantFailure]].
   *
   * @param id  the name of the floating rate family, as the index holds it
   * @param indexName  the name of the index asking, named in the message of an invariant breach
   * @return the floating rate family of that name
   */
  private def familyOf(id: String, indexName: String): FloatingRateName =
    FloatingRateName
      .valueOf(id)
      .getOrElse(
        Index.invariantFailure(
          s"Index '$indexName' belongs to the floating rate family '$id', which is not published"))
}

//-------------------------------------------------------------------------
/**
 * An index of an interest rate.
 *
 * This is the subset of [[FloatingRateIndex]] whose figure is an interest rate, which is every
 * Ibor index and every Overnight index but no Price index. Code that computes interest takes
 * this type rather than `Index`, so that a price index or an exchange-rate index cannot reach
 * it.
 *
 * Beyond what a floating rate index carries it adds the two things such code always needs: the
 * calendar that decides which days the rate is fixed on, and the period the rate covers.
 */
sealed trait RateIndex extends FloatingRateIndex {

  /**
   * Gets the calendar that determines which dates are fixing dates.
   *
   * The rate is published on each business day of this calendar. The value is an identifier
   * rather than a calendar, so an index carries no reference data of its own; a caller resolves
   * the identifier against the reference data it holds, which is what makes every date
   * calculation of an index explicit about the data it used.
   *
   * @return the calendar identifier of the fixing dates
   */
  def fixingCalendar: HolidayCalendarId

  /**
   * Gets the tenor of the index.
   *
   * This is the period the rate covers: three months for `GBP-LIBOR-3M`, and one day for every
   * Overnight index.
   *
   * @return the tenor of the index
   */
  def tenor: Tenor
}

/**
 * Resolves text naming an index of an interest rate.
 *
 * [[RateIndex.valueOf]] and [[RateIndex.parse]] search two families in one fixed order - Ibor
 * index, then Overnight index - and answer with the first member found, which is the order of
 * the combined lookup being ported. A price index and an exchange-rate index are not rate
 * indices, so text naming one of those names no rate index, and that is what these operations
 * report.
 */
object RateIndex {

  /**
   * The label this abstraction reports in the failure of [[parse]].
   */
  private val FamilyName: String = "RateIndex"

  /**
   * Looks up an index of an interest rate by name, answering with nothing when neither family
   * has it.
   *
   * @param name  the index name, such as `GBP-LIBOR-3M` or `EUR-ESTR`
   * @return the index of that name, or nothing when neither family publishes it
   */
  def valueOf(name: String): Option[RateIndex] = {
    val ibor: Option[RateIndex] = IborIndex.valueOf(name)
    ibor.orElse(OvernightIndex.valueOf(name))
  }

  /**
   * Parses text naming an index of an interest rate, reporting a failure when it names none.
   *
   * @param name  the text to parse, such as `GBP-LIBOR-3M`
   * @return the index that the text names, or a failure describing the text that named none
   */
  def parse(name: String): Either[Failure, RateIndex] =
    valueOf(name).toRight(Failure.Parsing(s"$FamilyName name not found: ${Failure.describeInput(name)}"))
}

//-------------------------------------------------------------------------
/**
 * An Ibor-like index, whose rate is published for a fixed tenor.
 *
 * An Ibor index fixes, on its fixing date, the rate for borrowing over a period that starts a
 * short offset later and runs for the index's tenor: `GBP-LIBOR-3M` fixes a three-month
 * sterling rate. A member therefore carries three dates' worth of convention - the calendar and
 * time of day the rate is published, the offset from the fixing date to the effective date on
 * which the borrowing starts, and the offset from the effective date to the maturity date on
 * which it ends - together with the currency the rate is quoted in and the day counts it and
 * its conventional fixed leg accrue on.
 *
 * The offsets are held as adjustments rather than as resolved dates, and every date calculation
 * takes the reference data it is to resolve them against, so an index is a pure value and a
 * calculation is reproducible from its arguments alone.
 *
 * ===Creating and comparing members===
 *
 * The class is sealed and its constructor is not available outside this file, so the only
 * members that exist are the ones this library publishes, created in the companion from the
 * published index data. Two members are equal when their names are equal, which is the
 * comparison the implementation being ported performed, and the name is also what the member
 * renders as and what its JSON form is.
 *
 * @param name  the unique name of the index, such as `GBP-LIBOR-3M`
 * @param currency  the currency the rate is quoted in
 * @param active  whether the rate is still published
 * @param fixingCalendar  the calendar of the dates the rate is fixed on
 * @param fixingTime  the local time of day the rate is fixed at
 * @param fixingZone  the time zone the fixing time is expressed in
 * @param fixingDateOffset  the offset from the effective date back to the fixing date
 * @param effectiveDateOffset  the offset from the fixing date to the effective date
 * @param maturityDateOffset  the offset from the effective date to the maturity date, which
 *   carries the tenor of the index
 * @param dayCount  the day count the rate accrues on
 * @param defaultFixedLegDayCount  the day count of the fixed leg conventionally swapped
 *   against this index
 */
sealed abstract class IborIndex private[index] (
    val name: String,
    val currency: Currency,
    val active: Boolean,
    val fixingCalendar: HolidayCalendarId,
    val fixingTime: LocalTime,
    val fixingZone: ZoneId,
    val fixingDateOffset: DaysAdjustment,
    val effectiveDateOffset: DaysAdjustment,
    val maturityDateOffset: TenorAdjustment,
    val dayCount: DayCount,
    override val defaultFixedLegDayCount: DayCount)
    extends RateIndex {

  /**
   * Gets the tenor of the index.
   *
   * The tenor is the period from the effective date to the maturity date, so it is read from
   * the maturity offset rather than stored a second time; the two cannot then disagree.
   *
   * @return the tenor of the index
   */
  final def tenor: Tenor = maturityDateOffset.tenor

  /**
   * The name of the floating rate family this index belongs to, which is its own name without
   * the tenor suffix.
   *
   * The suffix is a hyphen followed by the canonical text of the tenor, so `GBP-LIBOR-3M`
   * belongs to `GBP-LIBOR`, and the derivation is the one the bean being ported performed in
   * its constructor.
   *
   * That the name ends in its own tenor is an invariant of the published index data rather than
   * a property of a caller's argument: the name and the tenor arrive in the same row of that
   * data, and nothing outside this file can create a member. A member whose name and tenor
   * disagree would therefore be a defect in this library, which is why it is reported fail-fast
   * here - as the constructor being ported did, with the same message - instead of making every
   * creation of an index a failure a caller has to handle.
   */
  private[index] final val floatingRateNameId: String = {
    val suffix = "-" + tenor.name
    ArgCheck.isTrue(
      name.endsWith(suffix),
      s"IborIndex name '$name' must end with tenor '${tenor.name}'")
    name.substring(0, name.length - suffix.length)
  }

  //-------------------------------------------------------------------------
  /**
   * Calculates the date and time at which the rate fixes on a fixing date.
   *
   * The fixing time and zone of the index are applied to the date, which is the instant a
   * caller comparing a fixing against another market observation needs. The operation is total
   * and consults no reference data: it does not move the date onto a business day, so a caller
   * holding a date that is not a fixing date obtains the instant of that date rather than of
   * the next fixing.
   *
   * @param fixingDate  the fixing date
   * @return the date, time and zone at which the rate fixes
   */
  final def calculateFixingDateTime(fixingDate: LocalDate): ZonedDateTime =
    fixingDate.atTime(fixingTime).atZone(fixingZone)

  /**
   * Calculates the effective date from a fixing date.
   *
   * The fixing date is first moved onto the next fixing date, if it is not one already, so that
   * a caller holding an arbitrary date obtains a defined answer; the effective offset of the
   * index is then applied.
   *
   * Both steps need a resolved calendar, so the operation reports the failure of resolving one:
   * an index whose calendar the supplied reference data does not contain answers with missing
   * data, which is the same outcome the implementation being ported produced by raising an
   * error.
   *
   * @param fixingDate  the fixing date
   * @param refData  the reference data to resolve the calendars against
   * @return the effective date, or a failure when a calendar could not be resolved
   */
  final def calculateEffectiveFromFixing(
      fixingDate: LocalDate,
      refData: ReferenceData): Either[Failure, LocalDate] =
    for {
      fixingCal <- fixingCalendar.resolve(refData)
      effectiveDate <- effectiveDateOffset.adjust(fixingCal.nextOrSame(fixingDate), refData)
    } yield effectiveDate

  /**
   * Calculates the maturity date from a fixing date.
   *
   * This is [[calculateEffectiveFromFixing]] followed by the maturity offset of the index, so
   * the tenor is added to the effective date rather than to the fixing date - which is what
   * makes the period of the rate exactly its tenor.
   *
   * @param fixingDate  the fixing date
   * @param refData  the reference data to resolve the calendars against
   * @return the maturity date, or a failure when a calendar could not be resolved
   */
  final def calculateMaturityFromFixing(
      fixingDate: LocalDate,
      refData: ReferenceData): Either[Failure, LocalDate] =
    for {
      fixingCal <- fixingCalendar.resolve(refData)
      effectiveDate <- effectiveDateOffset.adjust(fixingCal.nextOrSame(fixingDate), refData)
      maturityDate <- maturityDateOffset.adjust(effectiveDate, refData)
    } yield maturityDate

  /**
   * Calculates the fixing date from an effective date.
   *
   * This is the inverse of [[calculateEffectiveFromFixing]]: the effective date is first moved
   * onto the next business day of the calendar the effective dates of this index live in, and
   * the fixing offset - which runs backwards - is then applied.
   *
   * @param effectiveDate  the effective date
   * @param refData  the reference data to resolve the calendars against
   * @return the fixing date, or a failure when a calendar could not be resolved
   */
  final def calculateFixingFromEffective(
      effectiveDate: LocalDate,
      refData: ReferenceData): Either[Failure, LocalDate] =
    for {
      effectiveCal <- effectiveDateCalendar(refData)
      fixingDate <- fixingDateOffset.adjust(effectiveCal.nextOrSame(effectiveDate), refData)
    } yield fixingDate

  /**
   * Calculates the maturity date from an effective date.
   *
   * The effective date is moved onto the next business day of the calendar the effective dates
   * of this index live in, and the maturity offset is then applied.
   *
   * @param effectiveDate  the effective date
   * @param refData  the reference data to resolve the calendars against
   * @return the maturity date, or a failure when a calendar could not be resolved
   */
  final def calculateMaturityFromEffective(
      effectiveDate: LocalDate,
      refData: ReferenceData): Either[Failure, LocalDate] =
    for {
      effectiveCal <- effectiveDateCalendar(refData)
      maturityDate <- maturityDateOffset.adjust(effectiveCal.nextOrSame(effectiveDate), refData)
    } yield maturityDate

  /**
   * Resolves the index against reference data once and answers with the calculation of the
   * dates of a fixing.
   *
   * A caller computing the dates of many fixings of one index should not resolve a calendar per
   * fixing, so this resolves the fixing calendar and both date adjusters up front and hands
   * back a function that consults no reference data at all: given a fixing date it moves it
   * onto the next fixing date, applies the effective offset, applies the maturity offset to
   * that result, computes the year fraction of the resulting period on the day count of the
   * index, and hands the four values to `build`. That separation of the resolution from the
   * per-fixing calculation is the whole point of the design being ported and is preserved here.
   *
   * The values handed to `build` are, in order, the fixing date exactly as supplied - not the
   * fixing date it was moved onto, since that is the date the caller asked about - the
   * effective date, the maturity date, and the year fraction between the latter two. The
   * function is stated over `build` rather than over one particular result so that the
   * observation of a fixing, which lives with the type that represents it, can be produced by
   * this single resolution without this hierarchy naming that type.
   *
   * @param refData  the reference data to resolve the calendar and the offsets against
   * @param build  the construction of the result from the fixing date, the effective date, the
   *   maturity date and the year fraction of the period between them
   * @tparam A  the type of the result built per fixing date
   * @return the calculation of a fixing's dates, or a failure when a calendar could not be
   *   resolved
   */
  final def resolveWith[A](refData: ReferenceData)(
      build: (LocalDate, LocalDate, LocalDate, Double) => A): Either[Failure, LocalDate => A] =
    for {
      fixingCal <- fixingCalendar.resolve(refData)
      effectiveAdjuster <- effectiveDateOffset.resolve(refData)
      maturityAdjuster <- maturityDateOffset.resolve(refData)
    } yield { (fixingDate: LocalDate) =>
      val effectiveDate = effectiveAdjuster.adjust(fixingCal.nextOrSame(fixingDate))
      val maturityDate = maturityAdjuster.adjust(effectiveDate)
      build(fixingDate, effectiveDate, maturityDate, dayCount.yearFraction(effectiveDate, maturityDate))
    }

  /**
   * Resolves the calendar the effective dates of this index live in.
   *
   * The effective offset states the calendar its result is adjusted against, but an offset that
   * adjusts against no calendar at all says nothing about which days are business days, and in
   * that case the effective dates of the index are the fixing dates - so the fixing calendar is
   * used instead. This is the private helper of the bean being ported.
   *
   * @param refData  the reference data to resolve the calendar against
   * @return the calendar of the effective dates, or a failure when it could not be resolved
   */
  private def effectiveDateCalendar(refData: ReferenceData): Either[Failure, HolidayCalendar] = {
    val offsetCalendar = effectiveDateOffset.resultCalendar
    val calendar =
      if (offsetCalendar == HolidayCalendarIds.NO_HOLIDAYS) fixingCalendar else offsetCalendar
    calendar.resolve(refData)
  }

  //-------------------------------------------------------------------------
  /**
   * Checks if this index equals another index.
   *
   * Two Ibor indices are equal when their names are equal, which is the comparison the bean
   * being ported performed. Since the family is closed and each name exists exactly once, two
   * equal indices are in practice the same instance; comparing the names rather than the
   * instances keeps the answer defined for any value of this type and agrees with [[hashCode]],
   * with the `Hash` instance and with the ordering, all of which read the name.
   *
   * @param obj  the other value to compare to
   * @return true when the other value is an Ibor index with the same name
   */
  override def equals(obj: Any): Boolean = obj match {
    case other: IborIndex => (this eq other) || name == other.name
    case _ => false
  }

  /**
   * Returns a hash code consistent with [[equals]].
   *
   * @return the hash code of the name
   */
  override def hashCode: Int = name.hashCode

  /**
   * Returns the name of the index.
   *
   * @return the unique name, such as `GBP-LIBOR-3M`
   */
  override def toString: String = name
}

/**
 * Holds the published Ibor indices, the lookup of one by name and the instances of the family.
 *
 * Every member of the family is created here from the published Ibor index data, which is the
 * single point of creation of an [[IborIndex]]: the class is sealed and its constructor is
 * unavailable elsewhere, so no other index can exist. The fields that data does not state
 * directly - the two date offsets and the maturity offset - are derived here by the rules the
 * parser being ported applied as it read each row, so that the values of the port and of the
 * original agree field by field.
 *
 * The family declares one alternate name and no other lookup table: the won certificate of
 * deposit rate whose tenor the market spells in months while the index spells it in weeks.
 * There is no lenient rewriting and no group of names published for another protocol, because
 * the reference data behind this family declares neither.
 */
object IborIndex {

  /**
   * The label this family reports when it rejects text, and the label its invariant breaches
   * name.
   *
   * Held once so that the lookup and the messages cannot describe the family differently, and
   * equal to the label the implementation being ported used in the same failure.
   */
  private val FamilyName: String = "IborIndex"

  /**
   * Lifts a value whose construction can only fail if this module's own data is inconsistent.
   *
   * The maturity offset of an index is built by a factory that reports a failure when a
   * month-based addition convention is paired with a tenor that is not a whole number of
   * months. Both halves of that pairing come from the same row of the published index data, and
   * no row of it pairs them that way, so a failure here would mean this library's transcription
   * of that data is wrong rather than that a caller passed something invalid. It is therefore
   * raised fail-fast, at the point the family is created, exactly as the constructor being
   * ported raised it - rather than being reported through `Either` and forcing every caller who
   * merely names an index to handle a failure that cannot happen.
   *
   * @param result  the construction to lift, which is expected to have succeeded
   * @param context  the description of what was being built, named in an invariant breach
   * @tparam A  the type of the constructed value
   * @return the constructed value
   */
  private def builtIn[A](result: EitherNec[Failure, A], context: => String): A = result match {
    case Right(value) => value
    case Left(failures) =>
      Index.invariantFailure(
        s"$context: ${failures.toNonEmptyList.toList.map(failure => failure.message).mkString("; ")}")
  }

  /**
   * Checks whether a period addition convention adds a period by landing on the end of a month.
   *
   * This is the private helper of the parser being ported, and it decides the business day
   * convention to apply to a maturity date when the published data does not state one: a tenor
   * added onto the end of a month is adjusted by the convention that keeps the result inside
   * the month, and any other tenor by the plain following convention.
   *
   * @param tenorConvention  the period addition convention read from the published data
   * @return true when the convention lands on the end of a month
   */
  private def isEndOfMonth(tenorConvention: PeriodAdditionConvention): Boolean =
    tenorConvention == PeriodAdditionConventions.LAST_BUSINESS_DAY ||
      tenorConvention == PeriodAdditionConventions.LAST_DAY

  /**
   * Builds the single member of the family described by one row of the published index data.
   *
   * The row states nine of the eleven fields of an index directly. The remaining two, and the
   * maturity offset, are derived here by the rules the parser being ported applied, statement
   * for statement:
   *
   *   - The '''fixing offset''' runs backwards, from an effective date to the fixing date it
   *     came from, so the number of business days of the row is negated; it is counted in the
   *     offset calendar of the row and the result is moved to the preceding business day of the
   *     fixing calendar, since a rate can only be fixed on a day the rate is published.
   *   - The '''effective offset''' runs forwards by the same number of days in the same
   *     calendar, and its result is moved to the following business day of the effective date
   *     calendar.
   *   - Both are normalised, which collapses an offset whose two calendars coincide, and an
   *     offset of no days, into the simplest equal form - so that two indices with the same
   *     convention hold equal offsets whichever row they came from.
   *   - The '''tenor convention''' column is read twice, and this is deliberate rather than
   *     redundant: its text may name a period addition convention, a business day convention,
   *     or both, and each reading falls back to its own default when the text does not name one
   *     of its kind. A text naming no addition convention adds the tenor plainly; a text naming
   *     no business day convention adjusts the result by the convention that keeps a
   *     month-end tenor inside its month, or plainly following otherwise. Both readings use the
   *     exact, alias-aware lookup of their family rather than the lenient one, as the parser
   *     being ported did, so only a spelling those families publish is recognised.
   *   - The '''maturity offset''' pairs the tenor with those two conventions and is adjusted
   *     against the effective date calendar of the row; see [[builtIn]] for why its failure is
   *     an invariant rather than a reported failure.
   *
   * The day count of the conventional fixed leg is the column of the row and is never derived
   * from the day count of the rate: the two differ on published rows, and the implementation
   * being ported fell back to the rate's day count only when the column was absent, which it
   * never is here.
   *
   * @param row  the row of published index data to build the member of
   * @return the Ibor index of that row
   */
  private def instanceOf(row: IborIndexRow): IborIndex = {
    val fixingDateOffset = DaysAdjustment
      .ofBusinessDays(
        -row.offsetDays,
        row.offsetCalendar,
        BusinessDayAdjustment.of(BusinessDayConventions.PRECEDING, row.fixingCalendar))
      .normalized
    val effectiveDateOffset = DaysAdjustment
      .ofBusinessDays(
        row.offsetDays,
        row.offsetCalendar,
        BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, row.effectiveDateCalendar))
      .normalized
    val periodAdditionConvention = PeriodAdditionConvention
      .valueOf(row.tenorConvention)
      .getOrElse(PeriodAdditionConventions.NONE)
    val tenorBusinessConvention = BusinessDayConvention
      .valueOf(row.tenorConvention)
      .getOrElse(
        if (isEndOfMonth(periodAdditionConvention)) BusinessDayConventions.MODIFIED_FOLLOWING
        else BusinessDayConventions.FOLLOWING)
    val maturityDateOffset = builtIn(
      TenorAdjustment.of(
        row.tenor,
        periodAdditionConvention,
        BusinessDayAdjustment.of(tenorBusinessConvention, row.effectiveDateCalendar)),
      s"$FamilyName ${row.name} maturity date offset")
    new IborIndex(
      row.name,
      row.currency,
      row.active,
      row.fixingCalendar,
      row.fixingTime,
      row.fixingZone,
      fixingDateOffset,
      effectiveDateOffset,
      maturityDateOffset,
      row.dayCount,
      row.fixedLegDayCount) {}
  }

  /**
   * The 271 published Ibor indices, one per row of the published index data, in the declaration
   * order of that data.
   *
   * This is the single point of creation of the family. Everything else - the constants, the
   * lookup, the members a caller iterates - selects from these instances rather than building
   * its own, so an index reached by any route is the same object as the index reached by every
   * other and reference identity agrees with equality throughout.
   *
   * The members are built by mapping the rows rather than by writing each construction out, so
   * that the code creating them is one expression whose size does not grow with the data.
   */
  private val instances: Vector[IborIndex] = IborIndexData.rows.map(instanceOf)

  /**
   * The 271 published Ibor indices, in the declaration order of the published index data.
   *
   * The order is observable through any iteration a caller performs, so it is kept stable and
   * deterministic rather than re-sorted: within each benchmark family the members appear in the
   * order their tenors were declared.
   *
   * @return the members of the family, in declaration order
   */
  val values: NonEmptyList[IborIndex] = NonEmptyList.fromListUnsafe(instances.toList)

  /**
   * The alternate spellings this family accepts, each mapped to a canonical name.
   *
   * One row, and it is behaviour rather than configuration: the won certificate of deposit rate
   * is named by its index for a tenor of thirteen weeks and by the market for a tenor of three
   * months, and both spellings name the same rate. The table is reproduced in the order the
   * published data declared it.
   */
  private val Alternates: Map[String, String] =
    Map(
      "KRW-CD-3M" -> "KRW-CD-13W"
    )

  /**
   * The name lookup of the family.
   *
   * Built from the members and the alternate names alone. The two other tables a named family
   * may declare are empty, because the reference data behind this one declares neither: there
   * is no pattern that rewrites text before it is looked up, and no group of names published
   * for another protocol. The lookup itself registers every member under its canonical name and
   * under the English upper case of that name, which is the whole name space of the
   * implementation being ported.
   *
   * @return the name lookup of the 271 Ibor indices
   */
  implicit val namedEnum: NamedEnum[IborIndex] =
    NamedEnum.of(values, Alternates, Nil, Map.empty, FamilyName)

  /**
   * Looks up an Ibor index by name, answering with nothing when no member has that name.
   *
   * The match is exact but alias-aware and case-folding: `GBP-LIBOR-3M` resolves, so does
   * `GBP-LIBOR-3M` in upper case, and so does an alternate spelling, while text of another
   * shape does not.
   *
   * @param name  the index name, such as `GBP-LIBOR-3M`
   * @return the index of that name, or nothing when the family has no such member
   */
  def valueOf(name: String): Option[IborIndex] = namedEnum.valueOf(name)

  /**
   * Parses text naming an Ibor index, reporting a failure when it names none.
   *
   * This is the exact lookup of [[valueOf]] followed, when that finds nothing, by the family's
   * lenient rewriting - of which this family declares none - with an absent result reported as
   * [[com.opengamma.strata.collect.result.Failure.Parsing]] naming the family and the text.
   *
   * @param name  the text to parse, such as `GBP-LIBOR-3M`
   * @return the index that the text names, or a failure describing the text that named none
   */
  def parse(name: String): EitherNec[Failure, IborIndex] = namedEnum.parse(name)

  //-------------------------------------------------------------------------
  /**
   * The ordering of the family by name, which is also its hashing and its equality.
   *
   * `Order` and `Hash` both extend `Eq`, so publishing this single value is what keeps the
   * family from acquiring two notions of equality that disagree; it also agrees with the
   * [[IborIndex.equals]] of the members themselves, which compares names.
   *
   * @return the ordering of Ibor indices by name
   */
  implicit val order: Order[IborIndex] with Hash[IborIndex] = NamedEnum.orderByName

  /**
   * The rendering of an Ibor index as its name, which is the text form the implementation being
   * ported produced.
   *
   * @return the rendering of an Ibor index
   */
  implicit val show: Show[IborIndex] = NamedEnum.showByName

  /**
   * The JSON form of an Ibor index, which is its name as a string.
   *
   * An index is reference data identified by a name, so the name is what crosses the wire and
   * the fields behind it - the calendars, the times, the offsets and the day counts - are never
   * written structurally. Decoding resolves the text through the family's own lookup, so a
   * payload naming no published index is rejected as a decoding failure rather than producing
   * an index this library never published.
   *
   * @return the codec of an Ibor index
   */
  implicit val codec: Codec[IborIndex] = Codecs.namedEnumCodec
}

//-------------------------------------------------------------------------
/**
 * An Overnight index, whose rate is published for a single business day.
 *
 * An Overnight index fixes the rate for borrowing over one business day, as `EUR-ESTR` and
 * `USD-SOFR` do, and a rate for a longer period is derived from the daily fixings by
 * compounding or by averaging - which is what distinguishes the two overnight kinds of
 * [[FloatingRateType]] rather than anything held here.
 *
 * A member carries the currency of the rate, the calendar of the days it is fixed on, and two
 * offsets counted in business days of that calendar: the number of days after the fixing date
 * on which the rate is published, and the number of days after the fixing date on which the
 * borrowing takes effect. Both are counts rather than adjustments, because the calendar they
 * are counted in is the fixing calendar of the index itself.
 *
 * ===Creating and comparing members===
 *
 * As for every family of this hierarchy, the class is sealed and its constructor is not
 * available outside this file, the published members are created in the companion from the
 * published index data, and two members are equal when their names are equal.
 *
 * @param name  the unique name of the index, such as `EUR-ESTR`
 * @param currency  the currency the rate is quoted in
 * @param active  whether the rate is still published
 * @param fixingCalendar  the calendar of the dates the rate is fixed on
 * @param publicationDateOffset  the number of business days after the fixing date on which the
 *   rate is published, zero or one on the published data
 * @param effectiveDateOffset  the number of business days after the fixing date on which the
 *   borrowing takes effect, zero or one on the published data
 * @param dayCount  the day count the rate accrues on
 * @param defaultFixedLegDayCount  the day count of the fixed leg conventionally swapped
 *   against this index, which differs from the day count of the rate on some members
 */
sealed abstract class OvernightIndex private[index] (
    val name: String,
    val currency: Currency,
    val active: Boolean,
    val fixingCalendar: HolidayCalendarId,
    val publicationDateOffset: Int,
    val effectiveDateOffset: Int,
    val dayCount: DayCount,
    override val defaultFixedLegDayCount: DayCount)
    extends RateIndex {

  /**
   * Gets the tenor of the index, which is one day for every Overnight index.
   *
   * @return the one-day tenor
   */
  final def tenor: Tenor = Tenor.TENOR_1D

  /**
   * The name of the floating rate family this index belongs to, which is its own name.
   *
   * An Overnight index publishes a single rate rather than one rate per tenor, so the family
   * and the index share a name and there is no suffix to remove; this is the derivation the
   * bean being ported performed.
   */
  private[index] final def floatingRateNameId: String = name

  //-------------------------------------------------------------------------
  /**
   * Calculates the publication date from a fixing date.
   *
   * A rate fixed on one day may be published on the next, so this shifts the fixing date
   * forwards by the publication offset of the index. As in every calculation of this family,
   * the date is first moved onto the next fixing date if it is not one already.
   *
   * @param fixingDate  the fixing date
   * @param refData  the reference data to resolve the fixing calendar against
   * @return the publication date, or a failure when the calendar could not be resolved
   */
  final def calculatePublicationFromFixing(
      fixingDate: LocalDate,
      refData: ReferenceData): Either[Failure, LocalDate] =
    shifted(fixingDate, publicationDateOffset, refData)

  /**
   * Calculates the effective date from a fixing date.
   *
   * The effective date is the day the borrowing the rate applies to starts, which is the fixing
   * date shifted forwards by the effective offset of the index - zero days on most published
   * rates.
   *
   * @param fixingDate  the fixing date
   * @param refData  the reference data to resolve the fixing calendar against
   * @return the effective date, or a failure when the calendar could not be resolved
   */
  final def calculateEffectiveFromFixing(
      fixingDate: LocalDate,
      refData: ReferenceData): Either[Failure, LocalDate] =
    shifted(fixingDate, effectiveDateOffset, refData)

  /**
   * Calculates the maturity date from a fixing date.
   *
   * The borrowing lasts one business day, so the maturity date is one business day beyond the
   * effective date.
   *
   * @param fixingDate  the fixing date
   * @param refData  the reference data to resolve the fixing calendar against
   * @return the maturity date, or a failure when the calendar could not be resolved
   */
  final def calculateMaturityFromFixing(
      fixingDate: LocalDate,
      refData: ReferenceData): Either[Failure, LocalDate] =
    shifted(fixingDate, effectiveDateOffset + 1, refData)

  /**
   * Calculates the fixing date from an effective date.
   *
   * This is the inverse of [[calculateEffectiveFromFixing]]: the effective offset is applied
   * backwards.
   *
   * @param effectiveDate  the effective date
   * @param refData  the reference data to resolve the fixing calendar against
   * @return the fixing date, or a failure when the calendar could not be resolved
   */
  final def calculateFixingFromEffective(
      effectiveDate: LocalDate,
      refData: ReferenceData): Either[Failure, LocalDate] =
    shifted(effectiveDate, -effectiveDateOffset, refData)

  /**
   * Calculates the maturity date from an effective date, which is one business day beyond it.
   *
   * @param effectiveDate  the effective date
   * @param refData  the reference data to resolve the fixing calendar against
   * @return the maturity date, or a failure when the calendar could not be resolved
   */
  final def calculateMaturityFromEffective(
      effectiveDate: LocalDate,
      refData: ReferenceData): Either[Failure, LocalDate] =
    shifted(effectiveDate, 1, refData)

  /**
   * Shifts a date by a number of business days of the fixing calendar.
   *
   * Every calculation of this family is this operation with a different number of days, which
   * is why it is written once: the date is moved onto the next business day of the fixing
   * calendar if it is not one already, and the shift is then applied to that. Both steps read
   * the same resolved calendar, so it is resolved once per call.
   *
   * @param date  the date to shift
   * @param amount  the number of business days to shift by, which may be negative
   * @param refData  the reference data to resolve the fixing calendar against
   * @return the shifted date, or a failure when the calendar could not be resolved
   */
  private def shifted(
      date: LocalDate,
      amount: Int,
      refData: ReferenceData): Either[Failure, LocalDate] =
    fixingCalendar.resolve(refData).map(fixingCal => fixingCal.shift(fixingCal.nextOrSame(date), amount))

  //-------------------------------------------------------------------------
  /**
   * Checks if this index equals another index.
   *
   * Two Overnight indices are equal when their names are equal; see the note on the equality of
   * an [[IborIndex]], which this reproduces.
   *
   * @param obj  the other value to compare to
   * @return true when the other value is an Overnight index with the same name
   */
  override def equals(obj: Any): Boolean = obj match {
    case other: OvernightIndex => (this eq other) || name == other.name
    case _ => false
  }

  /**
   * Returns a hash code consistent with [[equals]].
   *
   * @return the hash code of the name
   */
  override def hashCode: Int = name.hashCode

  /**
   * Returns the name of the index.
   *
   * @return the unique name, such as `EUR-ESTR`
   */
  override def toString: String = name
}

/**
 * Holds the published Overnight indices, the lookup of one by name and the instances of the
 * family.
 *
 * Every member of the family is created here from the published Overnight index data, which is
 * the single point of creation of an [[OvernightIndex]]: the class is sealed and its
 * constructor is unavailable elsewhere, so no other index can exist. Unlike the Ibor family,
 * nothing is derived - each row states every field of the index it describes.
 *
 * The family declares ten alternate names and no other lookup table. They are the retired and
 * the market spellings of rates that were renamed or that are written differently in different
 * places: the Chilean and Danish rates renamed with their reform, the euro rate under both of
 * its earlier spellings, the Hong Kong and Japanese rates under their earlier acronyms, and the
 * federal funds rate under the four spellings in circulation for it. There is no lenient
 * rewriting and no group of names published for another protocol, because the reference data
 * behind this family declares neither.
 */
object OvernightIndex {

  /**
   * The label this family reports when it rejects text.
   */
  private val FamilyName: String = "OvernightIndex"

  /**
   * Builds the single member of the family described by one row of the published index data.
   *
   * Every field is copied from the row, which is what the parser being ported did. In
   * particular the day count of the conventional fixed leg is the column of the row and is
   * never derived from the day count of the rate: on at least one published rate the rate
   * accrues on one convention while its conventional fixed leg is quoted on another.
   *
   * @param row  the row of published index data to build the member of
   * @return the Overnight index of that row
   */
  private def instanceOf(row: OvernightIndexRow): OvernightIndex =
    new OvernightIndex(
      row.name,
      row.currency,
      row.active,
      row.fixingCalendar,
      row.publicationOffsetDays,
      row.effectiveOffsetDays,
      row.dayCount,
      row.fixedLegDayCount) {}

  /**
   * The 35 published Overnight indices, one per row of the published index data, in the
   * declaration order of that data.
   *
   * This is the single point of creation of the family; see the note on the Ibor instances,
   * which this reproduces.
   */
  private val instances: Vector[OvernightIndex] = OvernightIndexData.rows.map(instanceOf)

  /**
   * The 35 published Overnight indices, in the declaration order of the published index data.
   *
   * The order is observable through any iteration a caller performs, so it is kept stable and
   * deterministic rather than re-sorted: the rates of the major currencies first, in the order
   * the published data declared them, then the standard rate of each remaining currency.
   *
   * @return the members of the family, in declaration order
   */
  val values: NonEmptyList[OvernightIndex] = NonEmptyList.fromListUnsafe(instances.toList)

  /**
   * The alternate spellings this family accepts, each mapped to a canonical name.
   *
   * Ten rows, reproduced in the order the published data declared them, and behaviour rather
   * than configuration: a rate renamed by a benchmark reform stays resolvable under the name
   * trades were written against, and a rate written differently by different market
   * participants resolves under each spelling. Two of the spellings contain a space, which is
   * how they are written and therefore how they are accepted.
   */
  private val Alternates: Map[String, String] =
    Map(
      "CLP-ICP" -> "CLP-TNA",
      "DKK-Tom Next" -> "DKK-TNR",
      "EUR-ESTER" -> "EUR-ESTR",
      "EUR-EuroSTR" -> "EUR-ESTR",
      "HKD-HONIX" -> "HKD-HONIA",
      "JPY-TONA" -> "JPY-TONAR",
      "USD-FED-FUNDS" -> "USD-FED-FUND",
      "USD-FEDFUND" -> "USD-FED-FUND",
      "USD-FEDFUNDS" -> "USD-FED-FUND",
      "USD-Federal Funds" -> "USD-FED-FUND"
    )

  /**
   * The name lookup of the family.
   *
   * Built from the members and the alternate names alone; see the note on the Ibor lookup for
   * why the two other tables are empty and for the name space the lookup itself derives.
   *
   * @return the name lookup of the 35 Overnight indices
   */
  implicit val namedEnum: NamedEnum[OvernightIndex] =
    NamedEnum.of(values, Alternates, Nil, Map.empty, FamilyName)

  /**
   * Looks up an Overnight index by name, answering with nothing when no member has that name.
   *
   * The match is exact but alias-aware and case-folding, so `EUR-ESTR` resolves and so do the
   * retired spellings `EUR-ESTER` and `EUR-EuroSTR`.
   *
   * @param name  the index name, such as `EUR-ESTR`
   * @return the index of that name, or nothing when the family has no such member
   */
  def valueOf(name: String): Option[OvernightIndex] = namedEnum.valueOf(name)

  /**
   * Parses text naming an Overnight index, reporting a failure when it names none.
   *
   * @param name  the text to parse, such as `EUR-ESTR`
   * @return the index that the text names, or a failure describing the text that named none
   */
  def parse(name: String): EitherNec[Failure, OvernightIndex] = namedEnum.parse(name)

  //-------------------------------------------------------------------------
  /**
   * The ordering of the family by name, which is also its hashing and its equality; see the
   * note on the Ibor ordering.
   *
   * @return the ordering of Overnight indices by name
   */
  implicit val order: Order[OvernightIndex] with Hash[OvernightIndex] = NamedEnum.orderByName

  /**
   * The rendering of an Overnight index as its name.
   *
   * @return the rendering of an Overnight index
   */
  implicit val show: Show[OvernightIndex] = NamedEnum.showByName

  /**
   * The JSON form of an Overnight index, which is its name as a string; see the note on the
   * Ibor codec.
   *
   * @return the codec of an Overnight index
   */
  implicit val codec: Codec[OvernightIndex] = Codecs.namedEnumCodec
}

//-------------------------------------------------------------------------
/**
 * An index of a price level, such as a measure of inflation.
 *
 * A price index is published for a month rather than for a day, as `GB-RPI` and `US-CPI-U` are,
 * so an observation of one names a month rather than a date and the index carries how often it
 * is published rather than any calendar of fixing dates. A member also carries the region whose
 * price level it measures, which is not always the region of its currency: one published index
 * is quoted in euro and measures France.
 *
 * A price index has no day count of its own. The convention that treats every period as one
 * whole year is used, which is the default of the interface being ported, and it is the honest
 * answer for a figure that is a level rather than a rate: there is no period over which a price
 * level accrues.
 *
 * ===Creating and comparing members===
 *
 * As for every family of this hierarchy, the class is sealed and its constructor is not
 * available outside this file, the published members are created in the companion from the
 * published index data, and two members are equal when their names are equal.
 *
 * @param name  the unique name of the index, such as `GB-RPI`
 * @param currency  the currency the index is quoted in
 * @param region  the region whose price level the index measures
 * @param active  whether the index is still published
 * @param publicationFrequency  how often the index is published, monthly on the published data
 */
sealed abstract class PriceIndex private[index] (
    val name: String,
    val currency: Currency,
    val region: Country,
    val active: Boolean,
    val publicationFrequency: Frequency)
    extends FloatingRateIndex {

  /**
   * Gets the day count convention of the index, which treats every period as one whole year.
   *
   * @return the one-to-one day count convention
   */
  final def dayCount: DayCount = DayCounts.ONE_ONE

  /**
   * The name of the floating rate family this index belongs to, which is its own name.
   *
   * A price index publishes a single level rather than one figure per tenor, so the family and
   * the index share a name; this is the derivation the bean being ported performed.
   */
  private[index] final def floatingRateNameId: String = name

  /**
   * Checks if this index equals another index.
   *
   * Two price indices are equal when their names are equal; see the note on the equality of an
   * [[IborIndex]], which this reproduces.
   *
   * @param obj  the other value to compare to
   * @return true when the other value is a price index with the same name
   */
  override def equals(obj: Any): Boolean = obj match {
    case other: PriceIndex => (this eq other) || name == other.name
    case _ => false
  }

  /**
   * Returns a hash code consistent with [[equals]].
   *
   * @return the hash code of the name
   */
  override def hashCode: Int = name.hashCode

  /**
   * Returns the name of the index.
   *
   * @return the unique name, such as `GB-RPI`
   */
  override def toString: String = name
}

/**
 * Holds the published price indices, the lookup of one by name and the instances of the family.
 *
 * Every member of the family is created here from the published price index data, which is the
 * single point of creation of a [[PriceIndex]]: the class is sealed and its constructor is
 * unavailable elsewhere, so no other index can exist. Nothing is derived - each row states
 * every field of the index it describes, and the day count is the one the type itself fixes.
 *
 * The family declares no lookup table at all. The reference data behind it does contain an
 * alternate name section, but every entry of that section is commented out, so no alternate
 * name resolved to an index in the implementation being ported; reviving one here would add
 * behaviour the original did not have.
 */
object PriceIndex {

  /**
   * The label this family reports when it rejects text.
   */
  private val FamilyName: String = "PriceIndex"

  /**
   * Builds the single member of the family described by one row of the published index data.
   *
   * @param row  the row of published index data to build the member of
   * @return the price index of that row
   */
  private def instanceOf(row: PriceIndexRow): PriceIndex =
    new PriceIndex(row.name, row.currency, row.region, row.active, row.publicationFrequency) {}

  /**
   * The nine published price indices, one per row of the published index data, in the
   * declaration order of that data.
   *
   * This is the single point of creation of the family; see the note on the Ibor instances,
   * which this reproduces.
   */
  private val instances: Vector[PriceIndex] = PriceIndexData.rows.map(instanceOf)

  /**
   * The nine published price indices, in the declaration order of the published index data.
   *
   * The order is observable through any iteration a caller performs, so it is kept stable and
   * deterministic rather than re-sorted: the three sterling indices, then the Swiss, the two
   * European, the Japanese, the United States and the French index.
   *
   * @return the members of the family, in declaration order
   */
  val values: NonEmptyList[PriceIndex] = NonEmptyList.fromListUnsafe(instances.toList)

  /**
   * The name lookup of the family.
   *
   * Built from the members alone: all three tables a named family may declare are empty, for
   * the reason given on this object and on the Ibor lookup. The lookup registers every member
   * under its canonical name and under the English upper case of that name, which is the whole
   * name space of the implementation being ported.
   *
   * @return the name lookup of the nine price indices
   */
  implicit val namedEnum: NamedEnum[PriceIndex] =
    NamedEnum.of(values, Map.empty, Nil, Map.empty, FamilyName)

  /**
   * Looks up a price index by name, answering with nothing when no member has that name.
   *
   * @param name  the index name, such as `GB-RPI`
   * @return the index of that name, or nothing when the family has no such member
   */
  def valueOf(name: String): Option[PriceIndex] = namedEnum.valueOf(name)

  /**
   * Parses text naming a price index, reporting a failure when it names none.
   *
   * @param name  the text to parse, such as `GB-RPI`
   * @return the index that the text names, or a failure describing the text that named none
   */
  def parse(name: String): EitherNec[Failure, PriceIndex] = namedEnum.parse(name)

  //-------------------------------------------------------------------------
  /**
   * The ordering of the family by name, which is also its hashing and its equality; see the
   * note on the Ibor ordering.
   *
   * @return the ordering of price indices by name
   */
  implicit val order: Order[PriceIndex] with Hash[PriceIndex] = NamedEnum.orderByName

  /**
   * The rendering of a price index as its name.
   *
   * @return the rendering of a price index
   */
  implicit val show: Show[PriceIndex] = NamedEnum.showByName

  /**
   * The JSON form of a price index, which is its name as a string; see the note on the Ibor
   * codec.
   *
   * @return the codec of a price index
   */
  implicit val codec: Codec[PriceIndex] = Codecs.namedEnumCodec
}

//-------------------------------------------------------------------------
/**
 * An index of a foreign exchange rate between two currencies.
 *
 * An FX index fixes the rate at which one currency converts into another, as `EUR/USD-ECB`
 * does. A member carries the pair of currencies the rate is quoted for, the calendar of the
 * dates the rate is fixed on, and the offset from a fixing date to the date on which a
 * conversion at that rate settles.
 *
 * It is an [[Index]] and nothing more. It is deliberately not a [[RateIndex]] or a
 * [[FloatingRateIndex]] - exactly as in the interface being ported - because the figure it
 * publishes is a rate of exchange rather than a rate of interest: there is no single currency
 * it is quoted in, no day count a rate of exchange accrues on and no family of floating rates
 * it belongs to, which is why a floating rate name never resolves to one. The symmetry with the
 * other three families stops here, and adding those members would be inventing data.
 *
 * ===Creating and comparing members===
 *
 * As for every family of this hierarchy, the class is sealed and its constructor is not
 * available outside this file, the published members are created in the companion from the
 * published index data, and two members are equal when their names are equal.
 *
 * @param name  the unique name of the index, such as `EUR/USD-ECB`
 * @param currencyPair  the pair of currencies the rate is quoted for
 * @param fixingCalendar  the calendar of the dates the rate is fixed on
 * @param maturityDateOffset  the offset from the fixing date to the date a conversion settles
 */
sealed abstract class FxIndex private[index] (
    val name: String,
    val currencyPair: CurrencyPair,
    val fixingCalendar: HolidayCalendarId,
    val maturityDateOffset: DaysAdjustment)
    extends Index {

  /**
   * Gets the offset from the date a conversion settles back to the fixing date it was quoted
   * on.
   *
   * This is the mirror of [[maturityDateOffset]] and is derived from it rather than published:
   * the same number of business days, counted in the same calendar, but running backwards. When
   * that calendar already contains the fixing calendar the offset needs nothing further, since
   * every date it can land on is a fixing date; otherwise the result is moved to the preceding
   * business day of the fixing calendar, because a rate can only be quoted on a day the rate is
   * published.
   *
   * The derivation is the one the bean being ported performed before it built an index, and it
   * ran for every published index, none of which states an offset of its own.
   *
   * @return the offset from the settlement date back to the fixing date
   */
  final val fixingDateOffset: DaysAdjustment = {
    val days = maturityDateOffset.days
    val maturityCalendar = maturityDateOffset.calendar
    if (maturityCalendar.combinedWith(fixingCalendar) == maturityCalendar) {
      DaysAdjustment.ofBusinessDays(-days, maturityCalendar)
    } else {
      DaysAdjustment.ofBusinessDays(
        -days,
        maturityCalendar,
        BusinessDayAdjustment.of(BusinessDayConventions.PRECEDING, fixingCalendar))
    }
  }

  //-------------------------------------------------------------------------
  /**
   * Calculates the date a conversion settles on from the fixing date it was quoted on.
   *
   * The fixing date is first moved onto the next fixing date, if it is not one already, so that
   * a caller holding an arbitrary date obtains a defined answer; the maturity offset of the
   * index is then applied.
   *
   * @param fixingDate  the fixing date
   * @param refData  the reference data to resolve the calendars against
   * @return the settlement date, or a failure when a calendar could not be resolved
   */
  final def calculateMaturityFromFixing(
      fixingDate: LocalDate,
      refData: ReferenceData): Either[Failure, LocalDate] =
    for {
      fixingCal <- fixingCalendar.resolve(refData)
      maturityDate <- maturityDateOffset.adjust(fixingCal.nextOrSame(fixingDate), refData)
    } yield maturityDate

  /**
   * Calculates the fixing date a conversion settling on a date was quoted on.
   *
   * This is the inverse of [[calculateMaturityFromFixing]], and it is found by search rather
   * than by an offset, because the offset that carries a fixing date forwards is not invertible
   * by arithmetic: two fixing dates separated by a holiday can settle on the same day, and the
   * answer wanted is the later of them.
   *
   * The settlement date is first moved onto the next business day of the calendar the
   * settlement dates of this index live in. The search then walks back one calendar day at a
   * time from that date, rejecting a candidate that is not a fixing date and a candidate whose
   * own settlement date falls after the settlement date asked about, and answering with the
   * first candidate that is rejected by neither. The walk is the loop of the bean being ported,
   * condition for condition, expressed as a recursion so that no variable is reassigned.
   *
   * @param maturityDate  the settlement date
   * @param refData  the reference data to resolve the calendars against
   * @return the fixing date, or a failure when a calendar could not be resolved
   */
  final def calculateFixingFromMaturity(
      maturityDate: LocalDate,
      refData: ReferenceData): Either[Failure, LocalDate] =
    for {
      maturityCal <- maturityDateCalendar.resolve(refData)
      fixingCal <- fixingCalendar.resolve(refData)
      maturityFromFixing <- maturityDateOffset.resolve(refData)
    } yield {
      val maturityBusinessDay = maturityCal.nextOrSame(maturityDate)
      FxIndex.fixingOnOrBefore(
        maturityBusinessDay,
        maturityBusinessDay,
        fixingCal,
        candidate => maturityFromFixing.adjust(candidate))
    }

  /**
   * Resolves the index against reference data once and answers with the calculation of the
   * dates of a fixing.
   *
   * A caller computing the dates of many fixings of one index should not resolve a calendar per
   * fixing, so this resolves the fixing calendar and the maturity adjuster up front and hands
   * back a function that consults no reference data at all: given a fixing date it moves it
   * onto the next fixing date and applies the maturity offset to that. That separation of the
   * resolution from the per-fixing calculation is the whole point of the design being ported
   * and is preserved here.
   *
   * The values handed to `build` are, in order, the fixing date exactly as supplied - not the
   * fixing date it was moved onto, since that is the date the caller asked about - and the date
   * a conversion at that fixing settles on. The function is stated over `build` rather than
   * over one particular result so that the observation of a fixing, which lives with the type
   * that represents it, can be produced by this single resolution without this hierarchy naming
   * that type.
   *
   * @param refData  the reference data to resolve the calendar and the offset against
   * @param build  the construction of the result from the fixing date and the settlement date
   * @tparam A  the type of the result built per fixing date
   * @return the calculation of a fixing's dates, or a failure when a calendar could not be
   *   resolved
   */
  final def resolveWith[A](refData: ReferenceData)(
      build: (LocalDate, LocalDate) => A): Either[Failure, LocalDate => A] =
    for {
      fixingCal <- fixingCalendar.resolve(refData)
      maturityAdjuster <- maturityDateOffset.resolve(refData)
    } yield { (fixingDate: LocalDate) =>
      build(fixingDate, maturityAdjuster.adjust(fixingCal.nextOrSame(fixingDate)))
    }

  /**
   * The calendar the settlement dates of this index live in.
   *
   * The maturity offset states the calendar its result is adjusted against, but an offset that
   * adjusts against no calendar at all says nothing about which days are business days, and in
   * that case the settlement dates of the index are the fixing dates - so the fixing calendar
   * is used instead. This is the private helper of the bean being ported.
   *
   * @return the calendar identifier of the settlement dates
   */
  private def maturityDateCalendar: HolidayCalendarId = {
    val offsetCalendar = maturityDateOffset.resultCalendar
    if (offsetCalendar == HolidayCalendarIds.NO_HOLIDAYS) fixingCalendar else offsetCalendar
  }

  //-------------------------------------------------------------------------
  /**
   * Checks if this index equals another index.
   *
   * Two FX indices are equal when their names are equal; see the note on the equality of an
   * [[IborIndex]], which this reproduces. Equality is by name and not by currency pair, because
   * one pair may be published by two administrators: the two euro/dollar indices are different
   * indices quoting the same pair.
   *
   * @param obj  the other value to compare to
   * @return true when the other value is an FX index with the same name
   */
  override def equals(obj: Any): Boolean = obj match {
    case other: FxIndex => (this eq other) || name == other.name
    case _ => false
  }

  /**
   * Returns a hash code consistent with [[equals]].
   *
   * @return the hash code of the name
   */
  override def hashCode: Int = name.hashCode

  /**
   * Returns the name of the index.
   *
   * @return the unique name, such as `EUR/USD-ECB`
   */
  override def toString: String = name
}


/**
 * Holds the published FX indices, the lookup of one by name or by currency pair, and the
 * instances of the family.
 *
 * Every member of the family is created here from the published FX index data, which is the
 * single point of creation of an [[FxIndex]]: the class is sealed and its constructor is
 * unavailable elsewhere, so no other index can exist. The maturity offset is built from the
 * number of days and the calendar of the row, and the fixing offset is derived from it on the
 * index itself.
 *
 * The family declares one alternate name and no other lookup table: the dollar/rupee rate under
 * the acronym of the administrator that published it before the benchmark was reformed. There
 * is no lenient rewriting and no group of names published for another protocol, because the
 * reference data behind this family declares neither.
 *
 * ===A pair of currencies does not identify an index===
 *
 * Two of the published indices quote the euro against the dollar, because two administrators
 * publish that rate, so [[FxIndex.of]] has to choose between the candidates for a pair; it
 * chooses as the implementation being ported did, by name. A pair for which nothing is
 * published is reported as a failure rather than answered with an index minted on the spot,
 * which is the one deliberate divergence of this family and is recorded in the migration note:
 * the family is the closed set of the published indices, and an index invented from a pair's
 * default calendar and a two-day settlement offset is data this library never published.
 */
object FxIndex {

  /**
   * The label this family reports when it rejects text.
   */
  private val FamilyName: String = "FxIndex"

  /**
   * Builds the single member of the family described by one row of the published index data.
   *
   * The maturity offset is the number of business days of the row counted in the calendar of
   * the row, which is what the parser being ported built; the fixing offset is derived from it
   * by the index itself.
   *
   * @param row  the row of published index data to build the member of
   * @return the FX index of that row
   */
  private def instanceOf(row: FxIndexRow): FxIndex =
    new FxIndex(
      row.name,
      row.currencyPair,
      row.fixingCalendar,
      DaysAdjustment.ofBusinessDays(row.maturityDays, row.maturityCalendar)) {}

  /**
   * The 16 published FX indices, one per row of the published index data, in the declaration
   * order of that data.
   *
   * This is the single point of creation of the family; see the note on the Ibor instances,
   * which this reproduces.
   */
  private val instances: Vector[FxIndex] = FxIndexData.rows.map(instanceOf)

  /**
   * The published indices grouped by the pair of currencies they quote, each group in the
   * declaration order of the published data.
   *
   * The value of a group is a sequence rather than a single index because a pair does not
   * identify an index; see the note on this object. The grouping is built by folding the
   * instances in order and appending to the group each one belongs to, so the order within a
   * group is a property of this code rather than of an unspecified library behaviour, and it is
   * built over the instances of the family rather than over the rows, so that the family has
   * one set of members reachable by every route.
   */
  private val byCurrencyPair: Map[CurrencyPair, Vector[FxIndex]] =
    instances.foldLeft(Map.empty[CurrencyPair, Vector[FxIndex]]) { (grouped, index) =>
      grouped.updated(index.currencyPair, grouped.getOrElse(index.currencyPair, Vector.empty) :+ index)
    }

  /**
   * The 16 published FX indices, in the declaration order of the published index data.
   *
   * The order is observable through any iteration a caller performs, so it is kept stable and
   * deterministic rather than re-sorted: the European Central Bank rates, then the rates fixed
   * against the dollar in the London market, then the local rates of the remaining currencies.
   *
   * @return the members of the family, in declaration order
   */
  val values: NonEmptyList[FxIndex] = NonEmptyList.fromListUnsafe(instances.toList)

  /**
   * The alternate spellings this family accepts, each mapped to a canonical name.
   *
   * One row, and it is behaviour rather than configuration: the dollar/rupee rate is named
   * after the administrator that publishes it, and a trade written before the benchmark was
   * reformed names the previous administrator; both spellings name the same rate.
   */
  private val Alternates: Map[String, String] =
    Map(
      "USD/INR-RBIB-INR01" -> "USD/INR-FBIL-INR01"
    )

  /**
   * The name lookup of the family.
   *
   * Built from the members and the alternate name alone; see the note on the Ibor lookup for
   * why the two other tables are empty and for the name space the lookup itself derives.
   *
   * @return the name lookup of the 16 FX indices
   */
  implicit val namedEnum: NamedEnum[FxIndex] =
    NamedEnum.of(values, Alternates, Nil, Map.empty, FamilyName)

  /**
   * Looks up an FX index by name, answering with nothing when no member has that name.
   *
   * The match is exact but alias-aware and case-folding, so `USD/INR-FBIL-INR01` resolves and
   * so does the previous administrator's spelling of it. Use [[of]] to accept the name of a
   * currency pair as well as the name of an index.
   *
   * @param name  the index name, such as `EUR/USD-ECB`
   * @return the index of that name, or nothing when the family has no such member
   */
  def valueOf(name: String): Option[FxIndex] = namedEnum.valueOf(name)

  /**
   * Parses text naming an FX index, reporting a failure when it names none.
   *
   * This resolves the name of an index and nothing else; [[of]] additionally accepts the name
   * of a currency pair.
   *
   * @param name  the text to parse, such as `EUR/USD-ECB`
   * @return the index that the text names, or a failure describing the text that named none
   */
  def parse(name: String): EitherNec[Failure, FxIndex] = namedEnum.parse(name)

  //-------------------------------------------------------------------------
  /**
   * Obtains the published index quoting a pair of currencies, reporting a failure when none
   * quotes it.
   *
   * Two administrators publish the euro/dollar rate, so a pair may name two indices; the one
   * whose name sorts first is answered with, which is the choice the implementation being
   * ported made. The comparison is of the names as text, so `EUR/USD-ECB` is preferred to
   * `EUR/USD-WM`.
   *
   * A pair for which this library publishes nothing is reported as
   * [[com.opengamma.strata.collect.result.Failure.Parsing]]. This is the one deliberate
   * divergence of this family and is recorded in the migration note: the implementation being
   * ported minted an index for such a pair, from the pair's default calendar and a settlement
   * offset of two business days, and this port does not - the family is the closed set of the
   * published indices, and an index invented on the spot is data this library never published
   * and could not be resolved back from its own name.
   *
   * @param currencyPair  the pair of currencies the rate is quoted for
   * @return the published index quoting that pair whose name sorts first, or a failure when
   *   none is published for it
   */
  def of(currencyPair: CurrencyPair): Either[Failure, FxIndex] =
    byCurrencyPair
      .get(currencyPair)
      .flatMap(candidates => candidates.minByOption(_.name))
      .toRight(unableToCreate(currencyPair.toString))

  /**
   * Obtains the published index named by text, which may name an index or a pair of currencies.
   *
   * The name of an index is resolved first, through the family's own alias-aware lookup. Text
   * that names no index is then read as a pair of currencies - `GBP/USD` - and the index
   * quoting that pair is answered with, as [[of]] chooses it. Text that is neither is reported
   * as [[com.opengamma.strata.collect.result.Failure.Parsing]] carrying the message the
   * implementation being ported used for the error it raised in the same situation, which
   * describes the text rather than the reason it was rejected.
   *
   * @param name  the text naming an index or a pair of currencies
   * @return the index the text names, or a failure describing the text that named none
   */
  def of(name: String): Either[Failure, FxIndex] =
    valueOf(name) match {
      case Some(index) => Right(index)
      case None =>
        CurrencyPair
          .parse(name)
          .toOption
          .flatMap(currencyPair => of(currencyPair).toOption)
          .toRight(unableToCreate(name))
    }

  /**
   * The failure reported when text or a pair of currencies names no published index.
   *
   * The message is the one the implementation being ported raised, and the text it quotes is
   * rendered through [[com.opengamma.strata.collect.result.Failure.describeInput]], so a
   * message reaching a log or a report is bounded in length and cannot forge a line of it.
   *
   * @param described  the text or the pair of currencies that named no index
   * @return the failure to report
   */
  private def unableToCreate(described: String): Failure =
    Failure.Parsing(s"Unable to create FX index from ${Failure.describeInput(described)}")

  /**
   * Finds the latest fixing date on or before a candidate whose conversion settles no later
   * than a settlement date.
   *
   * This is the search behind [[FxIndex.calculateFixingFromMaturity]], written as a recursion
   * over the candidate date so that nothing is reassigned; it is a tail recursion, so the
   * compiler turns it into the same loop the bean being ported wrote by hand. A candidate is
   * rejected when it is not a fixing date, or when its own settlement date falls after the
   * settlement date asked about, and the walk steps back one calendar day - not one business
   * day - exactly as the loop being ported did.
   *
   * The walk always terminates on the published data: the settlement offsets are of zero, one
   * or two business days, so a candidate at most a few days earlier settles no later than the
   * date asked about and is a fixing date.
   *
   * @param candidate  the date to test, and the date the walk starts from
   * @param maturityBusinessDay  the settlement date asked about, already moved onto a business
   *   day of the settlement calendar
   * @param fixingCal  the resolved calendar of the fixing dates
   * @param maturityFromFixing  the resolved offset from a fixing date to its settlement date,
   *   as the function it applies, so that this search depends on nothing but the two dates and
   *   the calendar it is given
   * @return the latest fixing date on or before the candidate that settles no later than the
   *   settlement date
   */
  @tailrec
  private def fixingOnOrBefore(
      candidate: LocalDate,
      maturityBusinessDay: LocalDate,
      fixingCal: HolidayCalendar,
      maturityFromFixing: LocalDate => LocalDate): LocalDate =
    if (fixingCal.isHoliday(candidate) ||
      maturityFromFixing(candidate).isAfter(maturityBusinessDay)) {
      fixingOnOrBefore(candidate.minusDays(1), maturityBusinessDay, fixingCal, maturityFromFixing)
    } else {
      candidate
    }

  //-------------------------------------------------------------------------
  /**
   * The ordering of the family by name, which is also its hashing and its equality; see the
   * note on the Ibor ordering.
   *
   * @return the ordering of FX indices by name
   */
  implicit val order: Order[FxIndex] with Hash[FxIndex] = NamedEnum.orderByName

  /**
   * The rendering of an FX index as its name.
   *
   * @return the rendering of an FX index
   */
  implicit val show: Show[FxIndex] = NamedEnum.showByName

  /**
   * The JSON form of an FX index, which is its name as a string; see the note on the Ibor
   * codec.
   *
   * @return the codec of an FX index
   */
  implicit val codec: Codec[FxIndex] = Codecs.namedEnumCodec
}

