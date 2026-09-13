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
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.NoJavaSerialization
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
 * therefore `sealed`, so that a match over it is checked for exhaustiveness and no index can
 * exist that this library did not publish. Scala requires every direct subtype of a sealed type
 * to be declared in the same file as the type, which is why this one file holds the whole
 * hierarchy: the three abstractions and all four concrete families. The named constants of each
 * family, and the reference data its members are built from, live in their own files, because
 * those only refer to members and extend nothing.
 *
 * The abstractions are three, and they exist because different code needs different guarantees:
 *
 *   - `Index` is any index at all, which is what an instrument refers to.
 *   - [[RateIndex]] is an index of an interest rate, which is what code computing interest
 *     needs: it excludes price and exchange-rate indices.
 *   - [[FloatingRateIndex]] is an index whose figure is a floating rate, which is what code
 *     resolving a [[FloatingRate]] name to a concrete index needs.
 *
 * ===Why the three abstractions are classes and not traits===
 *
 * All three are `abstract class`es, and that is a deliberate choice about the class file rather
 * than about the source. A `sealed trait` compiles to a plain JVM interface: `sealed` leaves no
 * trace in the bytecode of Scala 2.13, so a class file produced by any other means - another
 * language on the JVM, or a class written by hand - can declare itself an implementation of the
 * interface, run no code of this library at all, and be accepted everywhere this type is. That is
 * an index belonging to no family, resolving through no lookup and absent from a match the
 * compiler proved exhaustive, which is exactly the openness Rule 4 of the migration removed.
 *
 * A class cannot be implemented that way. A subclass constructor must call a constructor of its
 * superclass - the JVM verifier requires it - so the check in the body of `Index` below runs for
 * every instance of every subtype of it, whatever compiled that subtype, and a subtype belonging
 * to none of the four families cannot be brought into existence. The hierarchy is a chain rather
 * than a diamond - `RateIndex` narrows [[FloatingRateIndex]], which narrows `Index` - so nothing
 * is lost by expressing it with classes, and each family beneath it closes itself the same way,
 * through the guard in its own constructor.
 *
 * [[FloatingRate]] stays an open trait, and deliberately: AAP 0.3.3 keeps it outside the closed
 * families, because it is implemented both by [[FloatingRateIndex]] here and by
 * [[FloatingRateName]] in another file, and Scala's same-file rule for a sealed type would
 * force those two together. Its own closure is the closure of its implementations: a value of it
 * that is an index is a member of one of the four families, and a value of it that is a family
 * name is a published member of that family.
 *
 * ===Implementation notes===
 *
 * Every member of every family is an immutable value created once, when its family is first
 * used, and is therefore safe to share between threads. Two members of one family are equal
 * when their names are equal, and each name exists exactly once, so equality, hashing and
 * reference identity agree throughout.
 *
 * This class has no JSON codec: it carries no data, and every value reaching it is a member of
 * one of the four families, each of which publishes a codec that writes the member's name.
 *
 * @see [[FloatingRate]] for the abstraction over an index and the family it belongs to
 * @see [[FloatingRateName]] for the family identifier a floating rate index reports
 */
sealed abstract class Index private[index] () extends Named {

  // The closure of the root of the hierarchy, run for every instance of every subtype of it. The
  // four families below each refuse, in their own constructors, to be a member they do not
  // publish; what this refuses is a subtype of the root that belongs to no family at all, which
  // is the one shape those four checks cannot see. Naming the families by their class literals
  // loads those classes without initialising their companions, so this costs no family creation
  // and cannot take part in an initialisation cycle.
  JvmClosure.requirePermittedSubtype(
    this,
    classOf[IborIndex],
    classOf[OvernightIndex],
    classOf[PriceIndex],
    classOf[FxIndex])
}

/**
 * Resolves text naming an index of any of the four families, and holds what the families share.
 *
 * ===The search order===
 *
 * [[Index.valueOf]] and [[Index.parse]] search the four families in one fixed order - Ibor
 * index, Overnight index, Price index, then FX index - and answer with the first family that
 * resolves the name. The order is part of the contract rather than an implementation detail,
 * because the name spaces of the families may overlap and the order is what decides which
 * family answers.
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
   * Held once so that the abstraction cannot describe itself differently in two messages.
   */
  private val FamilyName: String = "Index"

  /**
   * One family's probe, as a union of families consumes it: the family's own exact, alias-aware
   * lookup by name, widened to [[Index]].
   *
   * A family declares its lookup over its own type - the Ibor family answers with an
   * `Option[IborIndex]` - and conforms to this type by that type being an index.
   */
  type Lookup = String => Option[Index]

  /**
   * Looks up an index of any family by name, answering with nothing when no family has it.
   *
   * This is [[Index.firstMatch]] applied to [[Index.standardLookups]]: the four families are
   * probed in the order that value declares and the first member found is returned. The lookup is
   * exact and alias-aware, so an alternate spelling such as `USD-FEDFUND` resolves while text
   * differing only in case does not resolve unless the family registers that spelling; use
   * [[parse]] to obtain a failure rather than an absent value.
   *
   * @param name  the index name, such as `GBP-LIBOR-3M`, `EUR-ESTR`, `GB-RPI` or `EUR/USD-ECB`
   * @return the index of that name, or nothing when no family publishes it
   */
  def valueOf(name: String): Option[Index] = firstMatch(name, standardLookups)

  /**
   * The composition of family probes this union searches, in the order documented above: Ibor
   * index, Overnight index, price index, then exchange-rate index.
   *
   * This is the one place in this object that names the four families, and it is what [[valueOf]]
   * and [[parse]] search, so the order a caller observes and the order stated here cannot come
   * apart. A caller needing a different set - the rate families alone, say, or a probe of its own
   * ahead of them - composes one and passes it to [[Index.firstMatch]] rather than reimplementing
   * the search.
   *
   * It is a method rather than a field, and the elements of the sequence it returns are supplied
   * by name, so obtaining the composition forces none of the four companions and each is forced
   * only when its own probe is reached: resolving an Ibor index initialises the Ibor family alone.
   * A strict sequence would force all four families on the first lookup, and the families of index
   * and the families of floating rate name refer to each other, so each family must be
   * initialised by the searches that reach it and by no other.
   *
   * @return the four family probes, in probe order, each produced when it is first reached
   */
  def standardLookups: LazyList[Lookup] =
    ((name: String) => IborIndex.valueOf(name)) #::
      ((name: String) => OvernightIndex.valueOf(name)) #::
      ((name: String) => PriceIndex.valueOf(name)) #::
      ((name: String) => FxIndex.valueOf(name)) #::
      LazyList.empty[Lookup]

  /**
   * Parses text naming an index of any family, reporting a failure when it names none.
   *
   * This is [[valueOf]] with an absent value reported as
   * [[com.opengamma.strata.collect.result.Failure.Parsing]], whose message names this
   * abstraction and quotes the text as it stands. Writing a failure out is where that text is
   * bounded and escaped, so a message reaching a log cannot forge a line of it.
   *
   * @param name  the text to parse, such as `GBP-LIBOR-3M`
   * @return the index that the text names, or a failure describing the text that named none
   */
  def parse(name: String): Either[Failure, Index] =
    valueOf(name).toRight(Failure.Parsing(s"$FamilyName name not found: $name"))

  /**
   * Searches an explicitly supplied composition of probes and answers the first value one of them
   * finds.
   *
   * This is the ordered union rule of this package, written once: the probes are tried in the
   * order they are supplied, each is reached only if the ones before it found nothing, and the
   * first value found is the answer. It is stated over any probe type rather than over an index,
   * because the rule has nothing to do with what an index is.
   *
   * Every union of this package routes through it: [[Index.valueOf]], [[RateIndex.valueOf]],
   * [[FloatingRateIndex.valueOf]] and, over the floating rates, `FloatingRate.tryParseWith`, each
   * over its own `standardLookups`. Supplying a sequence whose elements are themselves by-name -
   * the `LazyList` each of those compositions returns is one - additionally defers producing each
   * probe until it is reached, which is what keeps a family's companion from being loaded by a
   * search that never consults it.
   *
   * @tparam A  the type of value the probes answer with
   * @param name  the text to search for, such as `GBP-LIBOR-3M`
   * @param lookups  the probes to search, in the order they are to be tried
   * @return the value the first matching probe found, or nothing when none of them matched
   */
  private[index] def firstMatch[A](name: String, lookups: Seq[String => Option[A]]): Option[A] =
    lookups.iterator.map(lookup => lookup(name)).find(_.isDefined).flatten

  /**
   * Reports a breach of an invariant of this module's own reference data, fail-fast.
   *
   * Two construction steps in this file can fail only if the transcribed index data is
   * internally inconsistent - a tenor whose addition convention cannot be applied to it, and an
   * index whose name does not end in its own tenor - and one accessor can fail only if an index
   * names a floating rate family that is not published. None of the three depends on caller
   * input, so none of them is a data-dependent failure to be reported through `Either`: each is
   * a defect in this library that has to surface at once and loudly, while the family is being
   * created.
   *
   * The failure is raised through [[com.opengamma.strata.collect.ArgCheck]], so that every
   * invariant breach in the library reports the same kind of error; the second statement is
   * unreachable and exists only because the first is declared to return no value, while this
   * operation must produce one of any type its callers ask for.
   *
   * @param message  the description of the breached invariant
   * @return never returns normally
   */
  private[index] def invariantFailure(message: String): Nothing = {
    ArgCheck.isTrue(false, message)
    sys.error(message)
  }
}

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
 *
 * It is an `abstract class` rather than a trait, and it extends the class [[Index]] rather than
 * an interface, for the reason given on [[Index]]: a class file compiled outside this library
 * cannot be a subtype of it without running the constructor of the root, and the root admits only
 * the four families. [[FloatingRate]], which it also implements, stays an open trait because the
 * AAP requires that - see the same note.
 */
sealed abstract class FloatingRateIndex private[index] () extends Index with FloatingRate {

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
   * historic trade must still describe the rate it was written against. The flag records what
   * the built-in index data states about publication, so it is data like any other field and is
   * not consulted by any operation of this hierarchy.
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
   * It defaults to the day count of the index itself, and the families that carry the
   * convention as reference data override it. The two differ in practice - one Overnight rate
   * accrues on one convention while its fixed leg is quoted on another - so neither may be
   * derived from the other.
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
   * the other half-built. Holding the name as text and resolving it per call breaks that cycle.
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
   * describing a bad argument, so it is reported through the fail-fast channel.
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
 * family that resolves the name. Each family is probed through its own exact, alias-aware
 * `valueOf`, never through its lenient parse, for the reason given on [[Index]].
 *
 * ===Text that names a family rather than an index===
 *
 * Text reaching this abstraction may name a concrete index, `GBP-LIBOR-3M`, or a whole family
 * of rates, `GBP-LIBOR`. The second has to be converted into an index before it can be used,
 * and the conversion needs a tenor - the family says nothing about the period a rate covers -
 * which the caller either supplies or leaves to the family's own default tenor. That is the
 * difference between the lookup and the parses of this object: [[FloatingRateIndex.valueOf]]
 * answers for an index name and for nothing else, while the parses accept a family name as
 * well.
 *
 * The four parsing entry points are these, the overload that takes no tenor being the one that
 * uses the family's default:
 *
 * {{{
 * FloatingRateIndex.parse("GBP-LIBOR-3M")                  // Right(GBP-LIBOR-3M)
 * FloatingRateIndex.parse("GBP-LIBOR")                     // Right(GBP-LIBOR-3M), default tenor
 * FloatingRateIndex.parse("GBP-LIBOR", Tenor.TENOR_6M)     // Right(GBP-LIBOR-6M)
 * FloatingRateIndex.tryParse("GBP-LIBOR")                  // Some(GBP-LIBOR-3M)
 * FloatingRateIndex.tryParse("GBP-LIBOR", Tenor.TENOR_6M)  // Some(GBP-LIBOR-6M)
 * FloatingRateIndex.tryParse("rubbish")                    // None
 * }}}
 *
 * The conversion of a family is the property of the floating rate family, which holds the index
 * name a tenor is appended to and the kind of rate it describes; it is not a property of this
 * hierarchy. [[FloatingRateIndex.parseWith]] is therefore the rule alone - search the two name
 * spaces, pass a concrete index through, and hand a family to the conversion it was given -
 * stated over the conversion rather than over any particular one, which is the same separation
 * [[FloatingRate.tryParseWith]] makes between the search and the composition it searches. It and
 * its absent-valued twin are visible to this package alone, the tenor being the only choice a
 * caller has and the overloads above being that choice.
 *
 * A conversion that produced no index is reported as the failure the conversion itself reports,
 * so the message names what could not be converted; text that named nothing at all is reported
 * as the failure of [[FloatingRate.parse]].
 */
object FloatingRateIndex {

  /**
   * One family's probe, as a union of families consumes it; see [[Index.Lookup]], of which this
   * is the narrowing to the families whose figure is a floating rate.
   */
  type Lookup = String => Option[FloatingRateIndex]

  /**
   * Looks up a concrete floating rate index by name, answering with nothing when none has it.
   *
   * This is [[Index.firstMatch]] applied to [[FloatingRateIndex.standardLookups]]: the Ibor,
   * Overnight and price families are probed in that order and the first member found is
   * returned; an exchange-rate index is never answered with, because it is not a floating rate
   * index. The lookup is exact and alias-aware.
   *
   * Text naming a family of rates rather than one index - `GBP-LIBOR` rather than
   * `GBP-LIBOR-3M` - names no index and is therefore absent here. [[tryParse]] and [[parse]] are
   * the operations that accept it, converting the family with a tenor.
   *
   * @param name  the index name, such as `GBP-LIBOR-3M`, `EUR-ESTR` or `GB-RPI`
   * @return the index of that name, or nothing when none of the three families publishes it
   */
  def valueOf(name: String): Option[FloatingRateIndex] = Index.firstMatch(name, standardLookups)

  /**
   * The composition of family probes this union searches, in the order documented above: Ibor
   * index, Overnight index, then price index.
   *
   * This is the one place in this object that names the three families, and it is what [[valueOf]]
   * searches. It is a method whose elements are supplied by name for the reason given on
   * [[Index.standardLookups]].
   *
   * @return the three family probes, in probe order, each produced when it is first reached
   */
  def standardLookups: LazyList[Lookup] =
    ((name: String) => IborIndex.valueOf(name)) #::
      ((name: String) => OvernightIndex.valueOf(name)) #::
      ((name: String) => PriceIndex.valueOf(name)) #::
      LazyList.empty[Lookup]

  /**
   * Parses text naming either a concrete floating rate index or a family of rates, reporting a
   * failure when it names neither.
   *
   * The text is resolved through [[FloatingRate.parse]], which searches the three index families
   * and then the published floating rate names, in that order. Text naming a concrete index -
   * `GBP-LIBOR-3M`, `EUR-ESTR`, `GB-RPI` - answers with that index. Text naming a family of rates
   * - `GBP-LIBOR` - answers with the member of that family at the family's own default tenor,
   * because a family says nothing about the period a rate covers and one has to be chosen; use
   * [[parse(indexStr:String,defaultIborTenor:com\.opengamma\.strata\.basics\.date\.Tenor)*]] to
   * choose it.
   *
   * A family that cannot produce an index - a price family, which has no tenor to append, or an
   * Ibor family with no member at the tenor asked for - reports the failure of that conversion
   * rather than a failure of this parse, so the message names what could not be converted. Text
   * naming nothing at all is reported as
   * [[com.opengamma.strata.collect.result.Failure.Parsing]] quoting that text.
   *
   * @param indexStr  the text to parse, such as `GBP-LIBOR-3M` or `GBP-LIBOR`
   * @return the index the text names, or the failure describing why it names none
   */
  def parse(indexStr: String): Either[Failure, FloatingRateIndex] =
    parseWith(indexStr, family => family.toFloatingRateIndex)

  /**
   * Parses text naming either a concrete floating rate index or a family of rates, using the
   * supplied tenor for an Ibor family, and reporting a failure when it names neither.
   *
   * This is [[parse(indexStr:String)*]] with the tenor to append to an Ibor family supplied by the
   * caller instead of taken from the family: `FloatingRateIndex.parse("GBP-LIBOR", Tenor.TENOR_6M)`
   * answers with `GBP-LIBOR-6M`. The tenor is used only when the text names a family; text naming
   * a concrete index answers with that index, whose tenor is its own. The overload that takes no
   * tenor uses the family's own default, so that no absent value has to be passed.
   *
   * @param indexStr  the text to parse, such as `GBP-LIBOR-3M` or `GBP-LIBOR`
   * @param defaultIborTenor  the tenor to use when the text names an Ibor family of rates
   * @return the index the text names, or the failure describing why it names none
   */
  def parse(indexStr: String, defaultIborTenor: Tenor): Either[Failure, FloatingRateIndex] =
    parseWith(indexStr, family => family.toFloatingRateIndex(defaultIborTenor))

  /**
   * Tries to parse text naming either a concrete floating rate index or a family of rates,
   * answering with nothing when it names neither.
   *
   * This is [[parse(indexStr:String)*]] with the failure reported as an absent value, for a caller
   * that has its own answer for text it does not recognise.
   *
   * @param indexStr  the text to parse, such as `GBP-LIBOR-3M` or `GBP-LIBOR`
   * @return the index the text names, or nothing when it names none
   */
  def tryParse(indexStr: String): Option[FloatingRateIndex] =
    tryParseWith(indexStr, family => family.toFloatingRateIndex.toOption)

  /**
   * Tries to parse text naming either a concrete floating rate index or a family of rates, using
   * the supplied tenor for an Ibor family, and answering with nothing when it names neither.
   *
   * This is
   * [[parse(indexStr:String,defaultIborTenor:com\.opengamma\.strata\.basics\.date\.Tenor)*]] with
   * the failure reported as an absent value.
   *
   * @param indexStr  the text to parse, such as `GBP-LIBOR-3M` or `GBP-LIBOR`
   * @param defaultIborTenor  the tenor to use when the text names an Ibor family of rates
   * @return the index the text names, or nothing when it names none
   */
  def tryParse(indexStr: String, defaultIborTenor: Tenor): Option[FloatingRateIndex] =
    tryParseWith(indexStr, family => family.toFloatingRateIndex(defaultIborTenor).toOption)

  /**
   * Parses text naming either a concrete floating rate index or a family of rates, converting a
   * family with the supplied conversion.
   *
   * This is the rule the four entry points above are built from, stated over the conversion
   * rather than over one particular one: the text is resolved through [[FloatingRate.parse]], a
   * concrete index is passed through unchanged, and a family is handed to `convert`, which
   * decides which member of the family the caller meant and reports its own failure when it can
   * decide on none. It is visible to this package alone - the choice of tenor is the only degree
   * of freedom a caller has, and the two `parse` overloads are that choice.
   *
   * @param indexStr  the text to parse, such as `GBP-LIBOR-3M` or `GBP-LIBOR`
   * @param convert  the conversion of a floating rate family into one of its indices
   * @return the index the text names, or the failure describing why it names none
   */
  private[index] def parseWith(
      indexStr: String,
      convert: FloatingRateName => Either[Failure, FloatingRateIndex]): Either[Failure, FloatingRateIndex] =
    FloatingRate.parse(indexStr).flatMap {
      case index: FloatingRateIndex => Right(index)
      case family: FloatingRateName => convert(family)
      // `FloatingRate` is deliberately open, so a value of a third implementor kind may reach
      // this point. Such a value names no member of this closed hierarchy, which is what the
      // failure says.
      case other =>
        Left(Failure.Parsing(s"Floating rate index not known: ${other.name}"))
    }

  /**
   * Tries to parse text naming either a concrete floating rate index or a family of rates,
   * converting a family with the supplied conversion.
   *
   * This is [[parseWith]] with both failure channels reported as an absent value: text naming
   * nothing, and a conversion that produced no index, are answered the same way. It is visible to
   * this package alone, for the reason given on [[parseWith]].
   *
   * @param indexStr  the text to parse, such as `GBP-LIBOR-3M` or `GBP-LIBOR`
   * @param convert  the conversion of a floating rate family into one of its indices
   * @return the index the text names, or nothing when it names none or the conversion produced
   *   none
   */
  private[index] def tryParseWith(
      indexStr: String,
      convert: FloatingRateName => Option[FloatingRateIndex]): Option[FloatingRateIndex] =
    FloatingRate.tryParse(indexStr).flatMap {
      case index: FloatingRateIndex => Some(index)
      case family: FloatingRateName => convert(family)
      case _ => None
    }

  /**
   * Resolves the floating rate family of an index from the name the index holds.
   *
   * The lookup is the family's own alias-aware one, so a retired spelling resolves exactly as
   * it does for a caller. Every index published by this library names a published family - each
   * family name is derived from the index name by the rule of the index's own family - so an
   * absent family is a defect in this module's own data rather than a bad argument, and is
   * reported through [[Index.invariantFailure]].
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
 *
 * It is an `abstract class` extending the class [[FloatingRateIndex]], which is the last link of
 * the chain the note on [[Index]] describes: every subtype of it runs the constructor of the root
 * and is therefore one of the two rate families this library publishes.
 */
sealed abstract class RateIndex private[index] () extends FloatingRateIndex {

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
 * index, then Overnight index - and answer with the first family that resolves the name. A
 * price index and an exchange-rate index are not rate indices, so text naming one of those
 * names no rate index, and that is what these operations report.
 */
object RateIndex {

  /**
   * The label this abstraction reports in the failure of [[parse]].
   */
  private val FamilyName: String = "RateIndex"

  /**
   * One family's probe, as a union of families consumes it; see [[Index.Lookup]], of which this
   * is the narrowing to the families whose figure is a rate of interest.
   */
  type Lookup = String => Option[RateIndex]

  /**
   * Looks up an index of an interest rate by name, answering with nothing when neither family
   * has it.
   *
   * This is [[Index.firstMatch]] applied to [[RateIndex.standardLookups]]: the Ibor family is
   * probed, then the Overnight family, and the first member found is returned.
   *
   * @param name  the index name, such as `GBP-LIBOR-3M` or `EUR-ESTR`
   * @return the index of that name, or nothing when neither family publishes it
   */
  def valueOf(name: String): Option[RateIndex] = Index.firstMatch(name, standardLookups)

  /**
   * The composition of family probes this union searches, in the order documented above: Ibor
   * index, then Overnight index.
   *
   * This is the one place in this object that names the two families, and it is what [[valueOf]]
   * and [[parse]] search. It is a method whose elements are supplied by name for the reason given
   * on [[Index.standardLookups]].
   *
   * @return the two family probes, in probe order, each produced when it is first reached
   */
  def standardLookups: LazyList[Lookup] =
    ((name: String) => IborIndex.valueOf(name)) #::
      ((name: String) => OvernightIndex.valueOf(name)) #::
      LazyList.empty[Lookup]

  /**
   * Parses text naming an index of an interest rate, reporting a failure when it names none.
   *
   * @param name  the text to parse, such as `GBP-LIBOR-3M`
   * @return the index that the text names, or a failure describing the text that named none
   */
  def parse(name: String): Either[Failure, RateIndex] =
    valueOf(name).toRight(Failure.Parsing(s"$FamilyName name not found: $name"))
}

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
 * The class is `sealed` and its members are created in the companion from the built-in index
 * data, so the only members that exist are the ones this library publishes. Two members are
 * equal when their names are equal, and the name is also what the member renders as and what
 * its JSON form is.
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
    extends RateIndex
    with NoJavaSerialization {

  // The closure of this family, run for every member as it is constructed: `sealed` and a
  // constructor private to this package are enforced against Scala and leave nothing in the class
  // file, so a subtype compiled by other means - which would be an index this library never
  // published, holding whatever calendars, offsets and day counts it chose - is refused here
  // instead. The members of the family are the instances of the companion's hidden `Impl`.
  JvmClosure.requireDeclaredMember(this, classOf[IborIndex])

  // The invariant of this family, which is what the check above cannot see. `Impl` is emitted
  // with a public constructor whatever the source asked for - only its `InnerClasses` entry
  // records the request, which a Java compiler honours and a hand-written class file does not -
  // so a caller that names that class directly produces an instance of exactly the class admitted
  // above, holding whatever it passed: a published name quoted in another currency, fixed on
  // another calendar, or accruing on another day count. Every field is therefore compared against
  // the row the published index data declares for this member's own name.
  JvmClosure.requireInvariant(
    "its fields are the ones the published Ibor index data declares for its name",
    IborIndex.holdsPublishedFields(this))

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
   * belongs to `GBP-LIBOR`.
   *
   * That the name ends in its own tenor is an invariant of the built-in index data rather than
   * a property of a caller's argument: the name and the tenor arrive in the same row of that
   * data. A member whose name and tenor disagree would therefore be a defect in this library,
   * which is why it is reported fail-fast here instead of making every creation of an index a
   * failure a caller has to handle.
   */
  private[index] final val floatingRateNameId: String = {
    val suffix = "-" + tenor.name
    ArgCheck.isTrue(
      name.endsWith(suffix),
      s"IborIndex name '$name' must end with tenor '${tenor.name}'")
    name.substring(0, name.length - suffix.length)
  }

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
   * data.
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
   * Resolves the index against reference data once and answers with the observation of a fixing.
   *
   * This is the operation for the case that matters to a caller building a series: observing
   * many fixings of one index must not resolve a holiday calendar per fixing. The fixing
   * calendar and both date offsets are resolved here, once, and the function handed back
   * consults no reference data at all - given a fixing date it moves the date onto the next
   * fixing date, applies the effective offset, applies the maturity offset to that result,
   * computes the year fraction of the resulting period on the day count of the index, and
   * builds the observation from the four values.
   *
   * @param refData  the reference data to resolve the fixing calendar and the offsets against
   * @return the observation of a fixing of this index, or a failure when a calendar could not be
   *   resolved
   */
  final def resolve(refData: ReferenceData): Either[Failure, LocalDate => IborIndexObservation] =
    IborIndexObservation.resolve(this, refData)

  /**
   * Resolves the index against reference data once and answers with the calculation of the
   * dates of a fixing, over any construction of a result.
   *
   * This is the body of [[resolve]] with the construction of the result left to the caller, and
   * it is visible to this package alone: [[resolve]] is the operation callers hold, and this is
   * the one place the resolution and the per-fixing calculation are written. A caller computing
   * the dates of many fixings of one index should not resolve a calendar per fixing, so this
   * resolves the fixing calendar and both date adjusters up front and hands back a function that
   * consults no reference data at all: given a fixing date it moves it onto the next fixing date,
   * applies the effective offset, applies the maturity offset to that result, computes the year
   * fraction of the resulting period on the day count of the index, and hands the four values to
   * `build`.
   *
   * The values handed to `build` are, in order, the fixing date exactly as supplied - not the
   * fixing date it was moved onto, since that is the date the caller asked about - the
   * effective date, the maturity date, and the year fraction between the latter two.
   *
   * @param refData  the reference data to resolve the calendar and the offsets against
   * @param build  the construction of the result from the fixing date, the effective date, the
   *   maturity date and the year fraction of the period between them
   * @tparam A  the type of the result built per fixing date
   * @return the calculation of a fixing's dates, or a failure when a calendar could not be
   *   resolved
   */
  private[index] final def resolveWith[A](refData: ReferenceData)(
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
   * used instead.
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

  /**
   * Checks if this index equals another index.
   *
   * Two Ibor indices are equal when their names are equal. Since the family is closed and each
   * name exists exactly once, two equal indices are in practice the same instance; comparing
   * the names rather than the instances keeps the answer defined for any value of this type and
   * agrees with [[hashCode]], with the `Hash` instance and with the ordering, all of which read
   * the name.
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
 * Every member of the family is created here from the built-in Ibor index data, which is the
 * single point of creation of an [[IborIndex]]: the class is `sealed`, so no other index can
 * exist. The fields that data does not state directly - the two date offsets and the maturity
 * offset - are derived here, row by row, by the rules stated on [[instanceOf]].
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
   * Held once so that the lookup and the messages cannot describe the family differently.
   */
  private val FamilyName: String = "IborIndex"

  /**
   * Lifts a value whose construction can only fail if this module's own data is inconsistent.
   *
   * The maturity offset of an index is built by a factory that reports a failure when a
   * month-based addition convention is paired with a tenor that is not a whole number of
   * months. Both halves of that pairing come from the same row of the built-in index data, and
   * no row of it pairs them that way, so a failure here would mean this library's transcription
   * of that data is wrong rather than that a caller passed something invalid. It is therefore
   * raised fail-fast, at the point the family is created, rather than being reported through
   * `Either` and forcing every caller who merely names an index to handle a failure that cannot
   * happen.
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
   * This decides the business day convention to apply to a maturity date when the built-in data
   * does not state one: a tenor added onto the end of a month is adjusted by the convention
   * that keeps the result inside the month, and any other tenor by the plain following
   * convention.
   *
   * @param tenorConvention  the period addition convention read from the built-in data
   * @return true when the convention lands on the end of a month
   */
  private def isEndOfMonth(tenorConvention: PeriodAdditionConvention): Boolean =
    tenorConvention == PeriodAdditionConventions.LAST_BUSINESS_DAY ||
      tenorConvention == PeriodAdditionConventions.LAST_DAY

  /**
   * Builds the single member of the family described by one row of the built-in index data.
   *
   * The row states nine of the eleven fields of an index directly. The remaining two, and the
   * maturity offset, are derived here by these rules:
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
   *     exact, alias-aware lookup of their family rather than the lenient one, so only a
   *     spelling those families publish is recognised.
   *   - The '''maturity offset''' pairs the tenor with those two conventions and is adjusted
   *     against the effective date calendar of the row; see [[builtIn]] for why its failure is
   *     an invariant rather than a reported failure.
   *
   * The day count of the conventional fixed leg is the column of the row and is never derived
   * from the day count of the rate: the two differ on published rows.
   *
   * @param row  the row of built-in index data to build the member of
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
    new Impl(
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
      row.fixedLegDayCount)
  }

  /**
   * Whether a member holds exactly the fields the published index data declares for its name.
   *
   * This is the invariant [[IborIndex]] states in its constructor, and it is stated over the
   * fields of the value rather than over the row a factory read, because a value of this family
   * can come into existence by a route no factory took: the hidden implementation class is
   * emitted with a public constructor, so a class file that names it directly builds an instance
   * of the one admitted class holding whatever fields it chose. The name is the identity of an
   * index throughout this library - it is what a lookup resolves, what equality compares and what
   * crosses the wire - so the row that declares that name is what the other ten fields have to
   * agree with, and a name the data does not declare is not a member of this family at all.
   *
   * Eight fields are compared with the columns of the row directly. The three offsets are
   * compared against the derivation [[instanceOf]] performs on the same row, which is deliberately
   * written twice: there the row becomes an index, here the stored index is held to the row. The
   * two statements cannot drift apart unnoticed, because every one of the published members runs
   * this check as the family is created, so a divergence stops the family from being created at
   * all rather than weakening the check.
   *
   * @param index  the member being constructed
   * @return true when every field of the member is the one the published data declares
   */
  private def holdsPublishedFields(index: IborIndex): Boolean =
    IborIndexData.byName
      .get(index.name)
      .exists(row =>
        index.currency == row.currency &&
          index.active == row.active &&
          index.fixingCalendar == row.fixingCalendar &&
          index.fixingTime == row.fixingTime &&
          index.fixingZone == row.fixingZone &&
          index.dayCount == row.dayCount &&
          index.defaultFixedLegDayCount == row.fixedLegDayCount &&
          index.fixingDateOffset == publishedFixingDateOffset(row) &&
          index.effectiveDateOffset == publishedEffectiveDateOffset(row) &&
          publishedMaturityDateOffset(row).contains(index.maturityDateOffset))

  /**
   * The fixing date offset one row of the published index data implies.
   *
   * The first step of the derivation of [[instanceOf]], restated as a value the stored offset is
   * compared against: the business days of the row negated, counted in the offset calendar of the
   * row, moved to the preceding business day of its fixing calendar, and normalised.
   *
   * @param row  the row of published index data
   * @return the fixing date offset of that row
   */
  private def publishedFixingDateOffset(row: IborIndexRow): DaysAdjustment =
    DaysAdjustment
      .ofBusinessDays(
        -row.offsetDays,
        row.offsetCalendar,
        BusinessDayAdjustment.of(BusinessDayConventions.PRECEDING, row.fixingCalendar))
      .normalized

  /**
   * The effective date offset one row of the published index data implies.
   *
   * The second step of the derivation of [[instanceOf]], restated as a value the stored offset is
   * compared against: the business days of the row counted forwards in the offset calendar of the
   * row, moved to the following business day of its effective date calendar, and normalised.
   *
   * @param row  the row of published index data
   * @return the effective date offset of that row
   */
  private def publishedEffectiveDateOffset(row: IborIndexRow): DaysAdjustment =
    DaysAdjustment
      .ofBusinessDays(
        row.offsetDays,
        row.offsetCalendar,
        BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, row.effectiveDateCalendar))
      .normalized

  /**
   * The maturity date offset one row of the published index data implies.
   *
   * The third step of the derivation of [[instanceOf]], restated as a value the stored offset is
   * compared against: the tenor of the row paired with the two conventions its tenor convention
   * column names - each read with the same fallback the derivation applies - and adjusted against
   * the effective date calendar of the row.
   *
   * The result keeps the reported form of [[TenorAdjustment.of]] rather than being unwrapped,
   * since the comparison is a check and not a construction: a row whose tenor cannot take its own
   * addition convention answers with a failure here, which no stored offset equals, and that is
   * the same row [[instanceOf]] would refuse to build a member from.
   *
   * @param row  the row of published index data
   * @return the maturity date offset of that row, or the failure the pairing reports
   */
  private def publishedMaturityDateOffset(row: IborIndexRow): EitherNec[Failure, TenorAdjustment] = {
    val periodAdditionConvention = PeriodAdditionConvention
      .valueOf(row.tenorConvention)
      .getOrElse(PeriodAdditionConventions.NONE)
    val tenorBusinessConvention = BusinessDayConvention
      .valueOf(row.tenorConvention)
      .getOrElse(
        if (isEndOfMonth(periodAdditionConvention)) BusinessDayConventions.MODIFIED_FOLLOWING
        else BusinessDayConventions.FOLLOWING)
    TenorAdjustment.of(
      row.tenor,
      periodAdditionConvention,
      BusinessDayAdjustment.of(tenorBusinessConvention, row.effectiveDateCalendar))
  }

  /**
   * The one implementation of an Ibor index, and the only class the family admits.
   *
   * A `sealed abstract class` needs a concrete subclass to be instantiated at all, and this is it.
   * It is declared here rather than written as an anonymous subclass at the instantiation site for
   * two reasons, both about what the class file says: a private member class is one a compiler in
   * another language refuses to name, where an anonymous class is public and can be instantiated
   * directly by such a caller; and a class declared inside this companion is a class only these
   * sources can declare, which is what lets [[IborIndex]] refuse, in its own constructor, to be a
   * member this family does not publish.
   *
   * Every parameter is passed straight to the family, which declares them as its fields; see the
   * documentation of [[IborIndex]] for what each of them means.
   *
   * @param name  the unique name of the index
   * @param currency  the currency the rate is quoted in
   * @param active  whether the rate is still published
   * @param fixingCalendar  the calendar of the dates the rate is fixed on
   * @param fixingTime  the local time of day the rate is fixed at
   * @param fixingZone  the time zone the fixing time is expressed in
   * @param fixingDateOffset  the offset from the effective date back to the fixing date
   * @param effectiveDateOffset  the offset from the fixing date to the effective date
   * @param maturityDateOffset  the offset from the effective date to the maturity date
   * @param dayCount  the day count the rate accrues on
   * @param defaultFixedLegDayCount  the day count of the conventional fixed leg
   */
  private final class Impl(
      name: String,
      currency: Currency,
      active: Boolean,
      fixingCalendar: HolidayCalendarId,
      fixingTime: LocalTime,
      fixingZone: ZoneId,
      fixingDateOffset: DaysAdjustment,
      effectiveDateOffset: DaysAdjustment,
      maturityDateOffset: TenorAdjustment,
      dayCount: DayCount,
      defaultFixedLegDayCount: DayCount)
      extends IborIndex(
        name,
        currency,
        active,
        fixingCalendar,
        fixingTime,
        fixingZone,
        fixingDateOffset,
        effectiveDateOffset,
        maturityDateOffset,
        dayCount,
        defaultFixedLegDayCount)

  /**
   * The 271 published Ibor indices, one per row of the built-in index data, in the declaration
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
   * The 271 published Ibor indices, in the declaration order of the built-in index data.
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
   * One row, and resolving it is part of the family's behaviour: the won certificate of deposit
   * rate is named by its index for a tenor of thirteen weeks and by the market for a tenor of
   * three months, and both spellings name the same rate.
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
   * for another protocol. The lookup itself holds every member under its canonical name and
   * under the English upper case of that name, which is the whole name space of the family.
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
   * The rendering of an Ibor index as its name.
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
 * As for every family of this hierarchy, the class is `sealed`, its members are created in the
 * companion from the built-in index data, and two members are equal when their names are equal.
 *
 * @param name  the unique name of the index, such as `EUR-ESTR`
 * @param currency  the currency the rate is quoted in
 * @param active  whether the rate is still published
 * @param fixingCalendar  the calendar of the dates the rate is fixed on
 * @param publicationDateOffset  the number of business days after the fixing date on which the
 *   rate is published, zero or one in the built-in data
 * @param effectiveDateOffset  the number of business days after the fixing date on which the
 *   borrowing takes effect, zero or one in the built-in data
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
    extends RateIndex
    with NoJavaSerialization {

  // The closure of this family, run for every member as it is constructed: `sealed` and a
  // constructor private to this package are enforced against Scala and leave nothing in the class
  // file, so a subtype compiled by other means - which would be an overnight rate this library
  // never published - is refused here instead. The members of the family are the instances of the
  // companion's hidden `Impl`.
  JvmClosure.requireDeclaredMember(this, classOf[OvernightIndex])

  // The invariant of this family, which is what the check above cannot see: the hidden
  // implementation carries a public constructor whatever the source asked for, so a class file
  // that names it directly builds an instance of the admitted class holding fields no row
  // declares - a published rate shifted onto another calendar, or published a day earlier than it
  // is. Every field is therefore compared against the row the published index data declares for
  // this member's own name.
  JvmClosure.requireInvariant(
    "its fields are the ones the published Overnight index data declares for its name",
    OvernightIndex.holdsPublishedFields(this))

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
   * and the index share a name and there is no suffix to remove.
   */
  private[index] final def floatingRateNameId: String = name

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
   * Resolves the index against reference data once and answers with the calculation of the dates
   * of a fixing, over any construction of a result.
   *
   * Every date an observation of an Overnight index derives is a shift of business days of one
   * calendar - the fixing calendar of this index - so a caller building a series of observations
   * has no reason to resolve that calendar per fixing, let alone three times per fixing, which is
   * what reaching the three calculations above in turn costs. This resolves it once and hands
   * back a function that consults no reference data at all: given a fixing date it moves the date
   * onto the next fixing date, shifts that by the publication offset and by the effective offset,
   * shifts the effective date on by the one business day the borrowing lasts, measures the year
   * fraction of the period between the last two on the day count of the index, and hands the five
   * values to `build`.
   *
   * The dates are those of [[calculatePublicationFromFixing]], [[calculateEffectiveFromFixing]]
   * and [[calculateMaturityFromEffective]], step for step, so a value built through this function
   * and a value built through those calculations agree; the fixing date handed to `build` is the
   * one the caller supplied rather than the fixing date it was moved onto, because that is the
   * date the caller asked about.
   *
   * It is visible to this package alone, so that the resolution of the calendar and the
   * per-fixing calculation are written in one place.
   *
   * @param refData  the reference data to resolve the fixing calendar against
   * @param build  the construction of the result from the fixing date, the publication date, the
   *   effective date, the maturity date and the year fraction of the period between the last two
   * @tparam A  the type of the result built per fixing date
   * @return the calculation of a fixing's dates, or a failure when the calendar could not be
   *   resolved
   */
  private[index] final def resolveWith[A](refData: ReferenceData)(
      build: (LocalDate, LocalDate, LocalDate, LocalDate, Double) => A)
      : Either[Failure, LocalDate => A] =
    fixingCalendar.resolve(refData).map { fixingCal => (fixingDate: LocalDate) =>
      val onFixing = fixingCal.nextOrSame(fixingDate)
      val publicationDate = fixingCal.shift(onFixing, publicationDateOffset)
      val effectiveDate = fixingCal.shift(onFixing, effectiveDateOffset)
      val maturityDate = fixingCal.shift(fixingCal.nextOrSame(effectiveDate), 1)
      build(
        fixingDate,
        publicationDate,
        effectiveDate,
        maturityDate,
        dayCount.yearFraction(effectiveDate, maturityDate))
    }

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
 * Every member of the family is created here from the built-in Overnight index data, which is
 * the single point of creation of an [[OvernightIndex]]: the class is `sealed`, so no other
 * index can exist. Nothing is derived - each row states every field of the index it describes.
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
   * Builds the single member of the family described by one row of the built-in index data.
   *
   * Every field is copied from the row. In particular the day count of the conventional fixed
   * leg is the column of the row and is never derived from the day count of the rate: on at
   * least one published rate the rate accrues on one convention while its conventional fixed
   * leg is quoted on another.
   *
   * @param row  the row of built-in index data to build the member of
   * @return the Overnight index of that row
   */
  private def instanceOf(row: OvernightIndexRow): OvernightIndex =
    new Impl(
      row.name,
      row.currency,
      row.active,
      row.fixingCalendar,
      row.publicationOffsetDays,
      row.effectiveOffsetDays,
      row.dayCount,
      row.fixedLegDayCount)

  /**
   * Whether a member holds exactly the fields the published index data declares for its name.
   *
   * This is the invariant [[OvernightIndex]] states in its constructor; see the note on the same
   * check of the Ibor family for why it is stated over the fields of the value rather than over
   * the row a factory read. Nothing is derived in this family - [[instanceOf]] copies every column
   * across - so every field is compared with its column directly, and a name the published data
   * does not declare is not a member of this family at all.
   *
   * @param index  the member being constructed
   * @return true when every field of the member is the one the published data declares
   */
  private def holdsPublishedFields(index: OvernightIndex): Boolean =
    OvernightIndexData.byName
      .get(index.name)
      .exists(row =>
        index.currency == row.currency &&
          index.active == row.active &&
          index.fixingCalendar == row.fixingCalendar &&
          index.publicationDateOffset == row.publicationOffsetDays &&
          index.effectiveDateOffset == row.effectiveOffsetDays &&
          index.dayCount == row.dayCount &&
          index.defaultFixedLegDayCount == row.fixedLegDayCount)

  /**
   * The one implementation of an Overnight index, and the only class the family admits.
   *
   * Declared and hidden here for the reasons given on the implementation of [[IborIndex]]: a
   * private member class cannot be named by a compiler in another language, and a class declared
   * inside this companion is one only these sources can declare, which is what the closure check
   * in the constructor of [[OvernightIndex]] tests for.
   *
   * @param name  the unique name of the index
   * @param currency  the currency the rate is quoted in
   * @param active  whether the rate is still published
   * @param fixingCalendar  the calendar of the dates the rate is fixed on
   * @param publicationDateOffset  the business days from the fixing date to the publication date
   * @param effectiveDateOffset  the business days from the fixing date to the effective date
   * @param dayCount  the day count the rate accrues on
   * @param defaultFixedLegDayCount  the day count of the conventional fixed leg
   */
  private final class Impl(
      name: String,
      currency: Currency,
      active: Boolean,
      fixingCalendar: HolidayCalendarId,
      publicationDateOffset: Int,
      effectiveDateOffset: Int,
      dayCount: DayCount,
      defaultFixedLegDayCount: DayCount)
      extends OvernightIndex(
        name,
        currency,
        active,
        fixingCalendar,
        publicationDateOffset,
        effectiveDateOffset,
        dayCount,
        defaultFixedLegDayCount)

  /**
   * The 35 published Overnight indices, one per row of the built-in index data, in the
   * declaration order of that data.
   *
   * This is the single point of creation of the family; see the note on the Ibor instances,
   * which this reproduces.
   */
  private val instances: Vector[OvernightIndex] = OvernightIndexData.rows.map(instanceOf)

  /**
   * The 35 published Overnight indices, in the declaration order of the built-in index data.
   *
   * The order is observable through any iteration a caller performs, so it is kept stable and
   * deterministic rather than re-sorted: the rates of the major currencies first, in the order
   * the built-in data declares them, then the standard rate of each remaining currency.
   *
   * @return the members of the family, in declaration order
   */
  val values: NonEmptyList[OvernightIndex] = NonEmptyList.fromListUnsafe(instances.toList)

  /**
   * The alternate spellings this family accepts, each mapped to a canonical name.
   *
   * Ten rows, and resolving them is part of the family's behaviour: a rate renamed by a
   * benchmark reform stays resolvable under the name trades were written against, and a rate
   * written differently by different market participants resolves under each spelling. Two of
   * the spellings contain a space, which is how they are written and therefore how they are
   * accepted.
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
 * whole year is used, which is the honest answer for a figure that is a level rather than a
 * rate: there is no period over which a price level accrues.
 *
 * ===Creating and comparing members===
 *
 * As for every family of this hierarchy, the class is `sealed`, its members are created in the
 * companion from the built-in index data, and two members are equal when their names are equal.
 *
 * @param name  the unique name of the index, such as `GB-RPI`
 * @param currency  the currency the index is quoted in
 * @param region  the region whose price level the index measures
 * @param active  whether the index is still published
 * @param publicationFrequency  how often the index is published, monthly in the built-in data
 */
sealed abstract class PriceIndex private[index] (
    val name: String,
    val currency: Currency,
    val region: Country,
    val active: Boolean,
    val publicationFrequency: Frequency)
    extends FloatingRateIndex
    with NoJavaSerialization {

  // The closure of this family, run for every member as it is constructed: `sealed` and a
  // constructor private to this package are enforced against Scala and leave nothing in the class
  // file, so a subtype compiled by other means - which would be a measure of inflation this
  // library never published - is refused here instead. The members of the family are the instances
  // of the companion's hidden `Impl`.
  JvmClosure.requireDeclaredMember(this, classOf[PriceIndex])

  // The invariant of this family, which is what the check above cannot see: the hidden
  // implementation carries a public constructor whatever the source asked for, so a class file
  // that names it directly builds an instance of the admitted class holding fields no row
  // declares - a published measure attributed to another region, currency or publication
  // frequency. Every field is therefore compared against the row the published index data
  // declares for this member's own name.
  JvmClosure.requireInvariant(
    "its fields are the ones the published price index data declares for its name",
    PriceIndex.holdsPublishedFields(this))

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
   * the index share a name and there is no suffix to remove.
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
 * Every member of the family is created here from the built-in price index data, which is the
 * single point of creation of a [[PriceIndex]]: the class is `sealed`, so no other index can
 * exist. Nothing is derived - each row states every field of the index it describes, and the
 * day count is the one the type itself fixes.
 *
 * The family declares no lookup table at all: no alternate spelling, no pattern that rewrites
 * text before it is looked up and no group of names published for another protocol, so a price
 * index resolves under its canonical name and the upper case of it alone.
 */
object PriceIndex {

  /**
   * The label this family reports when it rejects text.
   */
  private val FamilyName: String = "PriceIndex"

  /**
   * Builds the single member of the family described by one row of the built-in index data.
   *
   * @param row  the row of built-in index data to build the member of
   * @return the price index of that row
   */
  private def instanceOf(row: PriceIndexRow): PriceIndex =
    new Impl(row.name, row.currency, row.region, row.active, row.publicationFrequency)

  /**
   * Whether a member holds exactly the fields the published index data declares for its name.
   *
   * This is the invariant [[PriceIndex]] states in its constructor; see the note on the same check
   * of the Ibor family for why it is stated over the fields of the value. Nothing is derived in
   * this family - [[instanceOf]] copies every column across and the day count is the one the type
   * itself fixes - so every field is compared with its column directly.
   *
   * @param index  the member being constructed
   * @return true when every field of the member is the one the published data declares
   */
  private def holdsPublishedFields(index: PriceIndex): Boolean =
    PriceIndexData.byName
      .get(index.name)
      .exists(row =>
        index.currency == row.currency &&
          index.region == row.region &&
          index.active == row.active &&
          index.publicationFrequency == row.publicationFrequency)

  /**
   * The one implementation of a price index, and the only class the family admits.
   *
   * Declared and hidden here for the reasons given on the implementation of [[IborIndex]]: a
   * private member class cannot be named by a compiler in another language, and a class declared
   * inside this companion is one only these sources can declare, which is what the closure check
   * in the constructor of [[PriceIndex]] tests for.
   *
   * @param name  the unique name of the index
   * @param currency  the currency the index is quoted in
   * @param region  the region whose price level the index measures
   * @param active  whether the index is still published
   * @param publicationFrequency  how often the index is published
   */
  private final class Impl(
      name: String,
      currency: Currency,
      region: Country,
      active: Boolean,
      publicationFrequency: Frequency)
      extends PriceIndex(name, currency, region, active, publicationFrequency)

  /**
   * The nine published price indices, one per row of the built-in index data, in the
   * declaration order of that data.
   *
   * This is the single point of creation of the family; see the note on the Ibor instances,
   * which this reproduces.
   */
  private val instances: Vector[PriceIndex] = PriceIndexData.rows.map(instanceOf)

  /**
   * The nine published price indices, in the declaration order of the built-in index data.
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
   * the reason given on this object. The lookup holds every member under its canonical name and
   * under the English upper case of that name, which is the whole name space of the family.
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

/**
 * An index of a foreign exchange rate between two currencies.
 *
 * An FX index fixes the rate at which one currency converts into another, as `EUR/USD-ECB`
 * does. A member carries the pair of currencies the rate is quoted for, the calendar of the
 * dates the rate is fixed on, and the offset from a fixing date to the date on which a
 * conversion at that rate settles.
 *
 * It is an [[Index]] and nothing more. It is deliberately not a [[RateIndex]] or a
 * [[FloatingRateIndex]], because the figure it publishes is a rate of exchange rather than a
 * rate of interest: there is no single currency it is quoted in, no day count a rate of
 * exchange accrues on and no family of floating rates it belongs to, which is why a floating
 * rate name never resolves to one. The symmetry with the other three families stops here, and
 * adding those members would be inventing data.
 *
 * ===Creating and comparing members===
 *
 * As for every family of this hierarchy, the class is `sealed`, its members are created in the
 * companion from the built-in index data, and two members are equal when their names are equal.
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
    extends Index
    with NoJavaSerialization {

  // The closure of this family, run for every member as it is constructed: `sealed` and a
  // constructor private to this package are enforced against Scala and leave nothing in the class
  // file, so a subtype compiled by other means - which would be an exchange-rate index this
  // library never published, and is exactly the dynamic index this port deliberately removed from
  // `FxIndex.of` - is refused here instead. The members of the family are the instances of the
  // companion's hidden `Impl`.
  JvmClosure.requireDeclaredMember(this, classOf[FxIndex])

  // The invariant of this family, which is what the check above cannot see: the hidden
  // implementation carries a public constructor whatever the source asked for, so a class file
  // that names it directly builds an instance of the admitted class holding fields no row
  // declares - a published rate rebound to another pair of currencies, or settling on a calendar
  // and an offset of its own, which is the dynamic index this port removed. Every field is
  // therefore compared against the row the published index data declares for this member's own
  // name; the fixing offset is derived from the maturity offset below and so is pinned with it.
  JvmClosure.requireInvariant(
    "its fields are the ones the published FX index data declares for its name",
    FxIndex.holdsPublishedFields(this))

  /**
   * Gets the offset from the date a conversion settles back to the fixing date it was quoted
   * on.
   *
   * This runs in the opposite direction to [[maturityDateOffset]] and is derived from it rather
   * than stated by the data: the same number of business days, counted in the same calendar,
   * but running backwards. When that calendar already contains the fixing calendar the offset
   * needs nothing further, since every date it can land on is a fixing date; otherwise the
   * result is moved to the preceding business day of the fixing calendar, because a rate can
   * only be quoted on a day the rate is published.
   *
   * The derivation runs for every member of the family, none of which states an offset of its
   * own.
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
   * first candidate that is rejected by neither. The walk is expressed as a recursion, so
   * nothing is reassigned as it proceeds.
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
   * Resolves the index against reference data once and answers with the observation of a fixing.
   *
   * This is the operation for the case that matters to a caller building a series: observing
   * many fixings of one index must not resolve a holiday calendar per fixing. The fixing
   * calendar and the maturity offset are resolved here, once, and the function handed back
   * consults no reference data at all - given a fixing date it moves the date onto the next
   * fixing date, applies the maturity offset to that, and builds the observation from the
   * fixing date the caller asked about and the settlement date it implies.
   *
   * @param refData  the reference data to resolve the fixing calendar and the offset against
   * @return the observation of a fixing of this index, or a failure when a calendar could not be
   *   resolved
   */
  final def resolve(refData: ReferenceData): Either[Failure, LocalDate => FxIndexObservation] =
    FxIndexObservation.resolve(this, refData)

  /**
   * Resolves the index against reference data once and answers with the calculation of the
   * dates of a fixing, over any construction of a result.
   *
   * This is the body of [[resolve]] with the construction of the result left to the caller, and
   * it is visible to this package alone: [[resolve]] is the operation callers hold, and this is
   * the one place the resolution and the per-fixing calculation are written. A caller computing
   * the dates of many fixings of one index should not resolve a calendar per fixing, so this
   * resolves the fixing calendar and the maturity adjuster up front and hands back a function
   * that consults no reference data at all: given a fixing date it moves it onto the next fixing
   * date and applies the maturity offset to that.
   *
   * The values handed to `build` are, in order, the fixing date exactly as supplied - not the
   * fixing date it was moved onto, since that is the date the caller asked about - and the date
   * a conversion at that fixing settles on.
   *
   * @param refData  the reference data to resolve the calendar and the offset against
   * @param build  the construction of the result from the fixing date and the settlement date
   * @tparam A  the type of the result built per fixing date
   * @return the calculation of a fixing's dates, or a failure when a calendar could not be
   *   resolved
   */
  private[index] final def resolveWith[A](refData: ReferenceData)(
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
   * is used instead.
   *
   * @return the calendar identifier of the settlement dates
   */
  private def maturityDateCalendar: HolidayCalendarId = {
    val offsetCalendar = maturityDateOffset.resultCalendar
    if (offsetCalendar == HolidayCalendarIds.NO_HOLIDAYS) fixingCalendar else offsetCalendar
  }

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
 * Every member of the family is created here from the built-in FX index data, which is the
 * single point of creation of an [[FxIndex]]: the class is `sealed`, so no other index can
 * exist. The maturity offset is built from the number of days and the calendar of the row, and
 * the fixing offset is derived from it on the index itself.
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
 * chooses the candidate whose name sorts first. A pair that no member of the family quotes is
 * refused: the family is the closed set of the built-in indices, and an index minted from a
 * pair's default calendar and a settlement offset is data this library never published.
 */
object FxIndex {

  /**
   * The label this family reports when it rejects text.
   */
  private val FamilyName: String = "FxIndex"

  /**
   * Builds the single member of the family described by one row of the built-in index data.
   *
   * The maturity offset is the number of business days of the row counted in the calendar of
   * the row; the fixing offset is derived from it by the index itself.
   *
   * @param row  the row of built-in index data to build the member of
   * @return the FX index of that row
   */
  private def instanceOf(row: FxIndexRow): FxIndex =
    new Impl(
      row.name,
      row.currencyPair,
      row.fixingCalendar,
      DaysAdjustment.ofBusinessDays(row.maturityDays, row.maturityCalendar))

  /**
   * Whether a member holds exactly the fields the published index data declares for its name.
   *
   * This is the invariant [[FxIndex]] states in its constructor; see the note on the same check of
   * the Ibor family for why it is stated over the fields of the value rather than over the row a
   * factory read. Two fields are compared with their columns directly and the maturity offset
   * against the one construction [[instanceOf]] performs on the row - the business days of the row
   * counted in the calendar of the row. The fixing offset needs no comparison of its own: the
   * index derives it from the maturity offset in its own body, so pinning the maturity offset pins
   * it too.
   *
   * @param index  the member being constructed
   * @return true when every field of the member is the one the published data declares
   */
  private def holdsPublishedFields(index: FxIndex): Boolean =
    FxIndexData.byName
      .get(index.name)
      .exists(row =>
        index.currencyPair == row.currencyPair &&
          index.fixingCalendar == row.fixingCalendar &&
          index.maturityDateOffset == DaysAdjustment
            .ofBusinessDays(row.maturityDays, row.maturityCalendar))

  /**
   * The one implementation of an FX index, and the only class the family admits.
   *
   * Declared and hidden here for the reasons given on the implementation of [[IborIndex]]: a
   * private member class cannot be named by a compiler in another language, and a class declared
   * inside this companion is one only these sources can declare, which is what the closure check
   * in the constructor of [[FxIndex]] tests for.
   *
   * @param name  the unique name of the index
   * @param currencyPair  the pair of currencies the rate is quoted for
   * @param fixingCalendar  the calendar of the dates the rate is fixed on
   * @param maturityDateOffset  the offset from the fixing date to the date a conversion settles
   */
  private final class Impl(
      name: String,
      currencyPair: CurrencyPair,
      fixingCalendar: HolidayCalendarId,
      maturityDateOffset: DaysAdjustment)
      extends FxIndex(name, currencyPair, fixingCalendar, maturityDateOffset)

  /**
   * The 16 published FX indices, one per row of the built-in index data, in the declaration
   * order of that data.
   *
   * This is the single point of creation of the family; see the note on the Ibor instances,
   * which this reproduces.
   */
  private val instances: Vector[FxIndex] = FxIndexData.rows.map(instanceOf)

  /**
   * The published indices grouped by the pair of currencies they quote, each group in the
   * declaration order of the built-in data.
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
   * The 16 published FX indices, in the declaration order of the built-in index data.
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
   * One row, and resolving it is part of the family's behaviour: the dollar/rupee rate is named
   * after the administrator that publishes it, and a trade written before the benchmark was
   * reformed names the administrator of that time; both spellings name the same rate.
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

  /**
   * Obtains the published index quoting a pair of currencies, reporting a failure when none
   * quotes it.
   *
   * Two administrators publish the euro/dollar rate, so a pair may name two indices; the one
   * whose name sorts first is answered with. The comparison is of the names as text, so
   * `EUR/USD-ECB` is preferred to `EUR/USD-WM`.
   *
   * A pair that no member of the family quotes is refused, and reported as
   * [[com.opengamma.strata.collect.result.Failure.Parsing]]. No index is minted for such a
   * pair: the family is the closed set of the built-in indices, and an index invented on the
   * spot is data this library never published and could not be resolved back from its own name.
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
   * quoting that pair is answered with, as [[of]] chooses it. Text that is neither, and a pair
   * that no member of the family quotes, are refused and reported as
   * [[com.opengamma.strata.collect.result.Failure.Parsing]], whose message describes the text
   * rather than the reason it was rejected.
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
   * The message quotes the text as it stands, and writing the failure out is where that text is
   * bounded and escaped.
   *
   * @param described  the text or the pair of currencies that named no index
   * @return the failure to report
   */
  private def unableToCreate(described: String): Failure =
    Failure.Parsing(s"Unable to create FX index from $described")

  /**
   * Finds the latest fixing date on or before a candidate whose conversion settles no later
   * than a settlement date.
   *
   * This is the search behind [[FxIndex.calculateFixingFromMaturity]], written as a recursion
   * over the candidate date so that nothing is reassigned; the recursion is in tail position,
   * so the walk runs in constant stack space. A candidate is rejected when it is not a fixing
   * date, or when its own settlement date falls after the settlement date asked about, and the
   * walk steps back one calendar day rather than one business day.
   *
   * The walk always terminates on the built-in data: the settlement offsets are of zero, one
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
