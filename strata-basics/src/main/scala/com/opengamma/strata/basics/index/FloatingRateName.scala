/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import scala.collection.immutable.SortedSet

import cats.Hash
import cats.Order
import cats.Show
import cats.data.NonEmptyList

import io.circe.Codec

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.basics.date.BusinessDayConventions
import com.opengamma.strata.basics.date.DaysAdjustment
import com.opengamma.strata.basics.date.Tenor
import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.Failure

/**
 * A floating rate index name, such as Libor, Euribor or US Fed Fund.
 *
 * A value of this type names some form of floating rate at the level a trade document names it.
 * It matches the FpML/ISDA floating rate index concept, which provides a single key for rates of
 * a variety of kinds - Ibor, Overnight and Price rates together - and which frequently includes
 * the source the rate is published by, as in `GBP-LIBOR-BBA` or `USD-Federal Funds-H.15`. This
 * type is the bridge from that single concept to the specific index implementations used for
 * pricing: [[IborIndex]], [[OvernightIndex]] and [[PriceIndex]].
 *
 * A name says nothing about the period a rate covers, so converting an Ibor name into an index
 * needs a [[Tenor]] as well; an Overnight or Price name converts on its own. The conversions are
 * [[toIborIndex]], [[toOvernightIndex]], [[toPriceIndex]] and the two forms of
 * [[toFloatingRateIndex]], and the most commonly used names are the constants of
 * [[FloatingRateNames]].
 *
 * ===Many names, one index===
 *
 * The published data holds 351 names resolving onto far fewer indices, because a rate is
 * published under several spellings: five names resolve onto `CHF-LIBOR-`, seven onto `EUR-ESTR`.
 * That is the point of the type rather than an accident of the data - a message quoting
 * `EUR-EURIBOR-Reuters` and one quoting `EUR-EURIBOR-Telerate` name the same rate - and
 * [[normalized]] is the operation that collapses a spelling onto the canonical name of its
 * family, the one whose external name equals its index name.
 *
 * ===A closed family===
 *
 * The members are reference data published by this library, not an extension point. The type is
 * `sealed`, every member is created in its companion from the published name table, and the
 * lookup is built from those members alone, so nothing outside this file can add a member and
 * nothing at run time can replace one. The implementation being ported assembled the family at
 * class-initialization time by reading a configuration resource from the classpath through a
 * registry that could be extended, overridden and, on a load failure, silently left empty; none
 * of that machinery survives here.
 *
 * ===What a caller must know about the shape of this port===
 *
 * Three groups of members differ in shape from the interface being ported, and every one of the
 * differences is the same substitution: an operation that raised an error now reports a
 * [[com.opengamma.strata.collect.result.Failure]] as a value. The messages are those of the
 * errors they replace.
 *
 *  - '''The conversions report failure.''' [[toIborIndex]], [[toOvernightIndex]],
 *    [[toPriceIndex]], both forms of [[toFloatingRateIndex]] and [[toIborIndexFixingOffset]]
 *    answer with an `Either`. Two things can go wrong in them: the name is of the wrong kind for
 *    the conversion asked for, which is [[Failure.Invalid]], and the index the name resolves to
 *    is not published, which is [[Failure.Parsing]].
 *  - '''Two accessors report failure.''' [[currency]] is derived by converting to an index, so it
 *    inherits that conversion's failure, and [[normalized]] looks up the canonical name of the
 *    family, which is a lookup that can miss. The interface being ported raised an error from
 *    both.
 *  - '''[[defaultTenor]] reports failure''', which is the one place a caller may be surprised.
 *    The implementation being ported took the first element of the tenor set when neither `3M`
 *    nor `13W` was available, and raised `NoSuchElementException` where that set was empty. The
 *    published data contains exactly such a name: every one of the thirteen euroyen TIBOR Ibor
 *    indices is marked inactive, so `JPY-TIBOR-EUROYEN` has no active tenor at all. This port
 *    reports that as [[Failure.MissingData]] rather than raising.
 *
 * One further difference is in the name of a member rather than its shape: the accessor the
 * interface being ported called `getType` is [[rateType]] here, because `type` is a keyword of
 * this language. The value it answers with is unchanged.
 *
 * ===Names, equality and JSON===
 *
 * [[name]] is the external name, which is the identity of the value: two names are equal exactly
 * when their external names are equal, hashing is that of the external name, and [[toString]]
 * and the `Show` instance both render it. That is the equality of the implementation being
 * ported, and it means a member is compared and stored by the spelling it arrived as, not by the
 * index it resolves to - `EUR-EURIBOR` and `EUR-EURIBOR-Reuters` are different values.
 *
 * The JSON form is the bare external name string, as the string conversion of the implementation
 * being ported was. Reading goes through the family's own name lookup and therefore accepts an
 * external name and its upper-case spelling, and nothing else: text naming a concrete index,
 * `GBP-LIBOR-3M`, is '''not''' accepted by the codec, exactly as it was not accepted by the
 * `of` factory the string conversion was bound to. [[FloatingRateName.parse]] is the wider
 * resolution that does accept it.
 *
 * ===Implementation notes===
 *
 * Values are immutable and safe to share between threads. Every operation that reaches an index
 * family resolves it when called rather than holding it in a field: the index families and this
 * family refer to each other, and a field would make the two mutually dependent at creation
 * time, so that whichever a program touched first could observe the other half-built. The
 * implementation being ported avoided the cycle the same way.
 *
 * @param externalName  the external name, typically from FpML, such as `GBP-LIBOR-BBA`
 * @param indexName  the name of the index family this name resolves to, such as `GBP-LIBOR-`,
 *   which for an Ibor family carries the trailing `-` that a tenor completes
 * @param rateType  the kind of rate this name describes, which decides the kind of index it
 *   converts to; the accessor called `getType` in the implementation being ported
 * @param fixingDateOffsetDays  the number of days of the non-standard fixing date offset this
 *   name implies, or nothing where the offset of the index applies; used only for Ibor names,
 *   and today only by the Danish CIBOR names
 * @see [[FloatingRateNames]] for the named constants of the commonly used members
 * @see [[FloatingRateType]] for the kinds of rate a name may describe
 */
sealed abstract class FloatingRateName private[index] (
    val externalName: String,
    val indexName: String,
    val rateType: FloatingRateType,
    val fixingDateOffsetDays: Option[Int]) extends FloatingRate {

  /**
   * Gets the name that uniquely identifies this floating rate, such as `GBP-LIBOR`.
   *
   * This is the external name, which is the name used in serialization and the name
   * [[FloatingRateName.valueOf]] accepts.
   *
   * @return the external name
   */
  final def name: String = externalName

  /**
   * Gets the floating rate name of this floating rate, which is this value.
   *
   * A concrete index reports the family it belongs to, losing its tenor; a family reports
   * itself, so the operation is the identity here.
   *
   * @return this floating rate name
   */
  final def floatingRateName: FloatingRateName = this

  //-------------------------------------------------------------------------
  /**
   * Gets the active tenors that are applicable for this floating rate.
   *
   * An Ibor name answers with the tenors of the published Ibor indices whose name begins with
   * this name's index name and which are still published; an Overnight or Price name answers
   * with the empty set, because neither has a tenor to choose. The set is ordered by the
   * ordering of tenors, which ranks them by length, so its first element is the shortest tenor
   * available - the element [[toIborIndexFixingOffset]] falls back on.
   *
   * The result can be empty for an Ibor name as well, where every index of the family has been
   * retired: `JPY-TIBOR-EUROYEN` is such a name in the published data. That is why
   * [[defaultTenor]] reports a failure rather than answering with a tenor.
   *
   * This is a method rather than a field, and has to stay one: it reads the published Ibor index
   * family, which refers back to this family, so a field would create an initialization cycle
   * between the two. The cost is a scan of the Ibor indices per call, which is what the
   * implementation being ported did as well.
   *
   * @return the available tenors, shortest first, empty where this name has none
   */
  def tenors: SortedSet[Tenor] =
    if (!rateType.isIbor) {
      FloatingRateName.NoTenors
    } else {
      SortedSet.from(
        IborIndex.values.toList.iterator
          .filter(index => index.name.startsWith(indexName) && index.active)
          .map(index => index.tenor))(FloatingRateName.TenorOrdering)
    }

  /**
   * Gets a default tenor applicable for this floating rate, reporting a failure where this name
   * has no tenor to offer.
   *
   * This is useful for providing a basic default where errors need to be avoided; the value is
   * not intended to be based on market conventions. An Ibor name answers with `3M`, or with
   * `13W` where `3M` is not available - which is the case for the Mexican TIIE rates - and
   * otherwise with the shortest tenor it has. An Overnight name answers with `1D` and any other
   * name with `1Y`.
   *
   * An Ibor name whose every index has been retired has no tenor to answer with, and that is
   * reported as [[Failure.MissingData]] naming the rate. The implementation being ported raised
   * `NoSuchElementException` in that case, which the published data reaches through
   * `JPY-TIBOR-EUROYEN`.
   *
   * @return the default tenor, or a failure where this name has no tenor available
   */
  def defaultTenor: Either[Failure, Tenor] =
    rateType match {
      case FloatingRateType.Ibor =>
        val available = tenors
        if (available.contains(Tenor.TENOR_3M)) {
          Right(Tenor.TENOR_3M)
        } else if (available.contains(Tenor.TENOR_13W)) {
          Right(Tenor.TENOR_13W)
        } else {
          available.headOption.toRight(
            Failure.MissingData(s"No active tenor for floating rate name: $externalName"))
        }
      case FloatingRateType.OvernightCompounded | FloatingRateType.OvernightAveraged =>
        Right(Tenor.TENOR_1D)
      case FloatingRateType.Price | FloatingRateType.Other =>
        Right(Tenor.TENOR_1Y)
    }

  /**
   * Gets the normalized form of this floating rate name, reporting a failure where the canonical
   * name of the family is not published.
   *
   * The normalized form is the name this library uses for the index: the normalized form of
   * `GBP-LIBOR-BBA` is `GBP-LIBOR` and that of `EUR-EURIBOR-Reuters` is `EUR-EURIBOR`. For an
   * Ibor name the trailing `-` of the index name is dropped, since the tenor is not part of a
   * name; for every other kind the index name is the normalized name as it stands.
   *
   * Every published family has a canonical name among the published names, so a failure here
   * would mean the two published tables disagree. It is reported rather than raised because the
   * accessor being ported reported it - as an error - and because the resolution is a lookup
   * whose miss a caller can act on.
   *
   * @return the normalized name, or a failure where the canonical name of the family is not
   *   published
   */
  def normalized: Either[Failure, FloatingRateName] = {
    val canonical =
      if (rateType.isIbor && indexName.endsWith("-")) indexName.dropRight(1) else indexName
    FloatingRateName.valueOf(canonical).toRight(FloatingRateName.notFound(canonical))
  }

  //-------------------------------------------------------------------------
  /**
   * Converts this name to an [[IborIndex]] of the specified tenor, reporting a failure where it
   * is not an Ibor name or the index is not published.
   *
   * The index name is this name's index name followed by the canonical text of the normalized
   * tenor, so `GBP-LIBOR-` and `3M` name `GBP-LIBOR-3M`, and `1Y` normalizes to `12M` so that it
   * names `GBP-LIBOR-12M` as it did in the implementation being ported.
   *
   * @param tenor  the tenor of the index
   * @return the index, [[Failure.Invalid]] where this is not an Ibor name, or
   *   [[Failure.Parsing]] where the family publishes no index of that tenor
   */
  def toIborIndex(tenor: Tenor): Either[Failure, IborIndex] =
    if (!rateType.isIbor) {
      Left(Failure.Invalid(s"Incorrect index type, expected Ibor: $externalName"))
    } else {
      val resolved = indexName + tenor.normalized.name
      IborIndex.valueOf(resolved).toRight(FloatingRateName.indexNotFound("IborIndex", resolved))
    }

  /**
   * Gets the fixing offset associated with the Ibor index of this name, reporting a failure where
   * it is not an Ibor name or the index is not published.
   *
   * The offset is that of the index of this name's shortest available tenor, or of its
   * three-month index where it has no tenor available, except where the name itself implies a
   * non-standard offset, in which case the implied offset replaces it:
   *
   *  - an implied offset of zero days becomes an adjustment that adds no days and moves the
   *    result back to the preceding business day of the index's own calendar;
   *  - any other implied offset becomes the index's own adjustment with that number of days,
   *    reduced to its representative form.
   *
   * This exists primarily to handle Danish CIBOR, where two names share one index. The index
   * itself fixes two days before the reset date and takes effect two days after the fixing,
   * which is the convention named `DKK-CIBOR2-DKNA13`; the alternative name `DKK-CIBOR-DKNA13`
   * fixes on the reset date itself while still taking effect two days later.
   *
   * @return the fixing offset applicable to the index, or the failure of the conversion to that
   *   index
   */
  def toIborIndexFixingOffset: Either[Failure, DaysAdjustment] = {
    val base = toIborIndex(tenors.headOption.getOrElse(Tenor.TENOR_3M)).map(_.fixingDateOffset)
    fixingDateOffsetDays match {
      case None =>
        base
      case Some(0) =>
        base.map(offset =>
          DaysAdjustment.ofCalendarDays(
            0,
            BusinessDayAdjustment.of(BusinessDayConventions.PRECEDING, offset.resultCalendar)))
      case Some(days) =>
        base.map(offset =>
          DaysAdjustment.ofBusinessDays(days, offset.calendar, offset.adjustment).normalized)
    }
  }

  /**
   * Converts this name to an [[OvernightIndex]], reporting a failure where it is not an Overnight
   * name or the index is not published.
   *
   * Both Overnight kinds convert, the compounded and the averaged, because the kind describes how
   * a rate accrues rather than which index publishes it. An index name carrying the averaging
   * suffix names the index without it, so `USD-FED-FUND-AVG` and the name `USD-EFFECTIVE`, whose
   * index name is `USD-FED-FUND` already, both convert to the same index.
   *
   * @return the index, [[Failure.Invalid]] where this is not an Overnight name, or
   *   [[Failure.Parsing]] where the index is not published
   */
  def toOvernightIndex: Either[Failure, OvernightIndex] =
    if (!rateType.isOvernight) {
      Left(Failure.Invalid(s"Incorrect index type, expected Overnight: $externalName"))
    } else {
      val resolved =
        if (indexName.endsWith(FloatingRateName.AverageSuffix)) {
          indexName.dropRight(FloatingRateName.AverageSuffix.length)
        } else {
          indexName
        }
      OvernightIndex
        .valueOf(resolved)
        .toRight(FloatingRateName.indexNotFound("OvernightIndex", resolved))
    }

  /**
   * Converts this name to a [[PriceIndex]], reporting a failure where it is not a price name or
   * the index is not published.
   *
   * @return the index, [[Failure.Invalid]] where this is not a price name, or
   *   [[Failure.Parsing]] where the index is not published
   */
  def toPriceIndex: Either[Failure, PriceIndex] =
    if (!rateType.isPrice) {
      Left(Failure.Invalid(s"Incorrect index type, expected Price: $externalName"))
    } else {
      PriceIndex
        .valueOf(indexName)
        .toRight(FloatingRateName.indexNotFound("PriceIndex", indexName))
    }

  //-------------------------------------------------------------------------
  /**
   * Converts this name to a concrete [[FloatingRateIndex]], using the
   * [[defaultTenor default tenor]] where this is an Ibor name.
   *
   * Ibor, Overnight and Price names convert; a name of any other kind reports
   * [[Failure.Invalid]], carrying the message of the error the implementation being ported
   * raised. The default tenor is resolved only for an Ibor name, which is the one kind that
   * needs it - the implementation being ported duplicated its conversion code for exactly that
   * reason, and the shape here keeps the property without the duplication.
   *
   * @return the index, or the failure of the conversion
   */
  def toFloatingRateIndex: Either[Failure, FloatingRateIndex] =
    rateType match {
      case FloatingRateType.Ibor =>
        defaultTenor.flatMap(tenor => toIborIndex(tenor))
      case FloatingRateType.OvernightCompounded | FloatingRateType.OvernightAveraged =>
        toOvernightIndex
      case FloatingRateType.Price =>
        toPriceIndex
      case FloatingRateType.Other =>
        Left(Failure.Invalid(s"Floating rate index type not known: $rateType"))
    }

  /**
   * Converts this name to a concrete [[FloatingRateIndex]], using the specified tenor where this
   * is an Ibor name.
   *
   * The tenor is used only where this is an Ibor name; an Overnight or Price name ignores it, as
   * the implementation being ported did, because neither has a tenor to choose.
   *
   * @param iborTenor  the tenor to use where this is an Ibor name
   * @return the index, or the failure of the conversion
   */
  def toFloatingRateIndex(iborTenor: Tenor): Either[Failure, FloatingRateIndex] =
    rateType match {
      case FloatingRateType.Ibor =>
        toIborIndex(iborTenor)
      case FloatingRateType.OvernightCompounded | FloatingRateType.OvernightAveraged =>
        toOvernightIndex
      case FloatingRateType.Price =>
        toPriceIndex
      case FloatingRateType.Other =>
        Left(Failure.Invalid(s"Floating rate index type not known: $rateType"))
    }

  /**
   * Gets the currency of this floating rate, reporting a failure where the index it converts to
   * is not available.
   *
   * The currency is not held by a name; it is that of the index the name converts to, through
   * [[toFloatingRateIndex]], so the conversion's failure is this accessor's failure. That is why
   * the abstraction shared with the concrete indices declares no currency: an index holds one and
   * answers totally, while a name derives one and can fail.
   *
   * @return the currency, or the failure of the conversion to an index
   */
  def currency: Either[Failure, Currency] = toFloatingRateIndex.map(index => index.currency)

  //-------------------------------------------------------------------------
  /**
   * Checks whether this floating rate name equals another value.
   *
   * Equality is that of the external name alone, which is the equality of the implementation
   * being ported. Two names that resolve onto the same index are therefore not equal unless they
   * are the same name; [[normalized]] is how a caller asks for the canonical name before
   * comparing.
   *
   * @param obj  the other value
   * @return true where the other value is a floating rate name with the same external name
   */
  override def equals(obj: Any): Boolean = obj match {
    case other: FloatingRateName => externalName == other.externalName
    case _ => false
  }

  /**
   * Gets a hash code, which is that of the external name.
   *
   * @return the hash code of the external name
   */
  override def hashCode: Int = externalName.hashCode

  /**
   * Returns the external name of this floating rate.
   *
   * @return the external name
   */
  override def toString: String = name
}

/**
 * Provides the members of the floating rate name family, the lookup of a member by name, the
 * currency defaults, and the typeclass instances and JSON codec of the family.
 *
 * ===Where the members come from===
 *
 * [[FloatingRateName.values]] is one member per row of the published name table, in the order
 * that table publishes them, and it is the single point of creation of the family. Every other
 * route to a member - the lookup, the constants of [[FloatingRateNames]], the currency defaults -
 * selects from those instances rather than building its own, so a member reached by any route is
 * the same object as the member reached by every other and reference identity agrees with
 * equality throughout.
 *
 * The members are built by mapping the rows rather than by writing 351 constructions out, which
 * keeps the initializer of this object one expression whose size does not grow with the data.
 *
 * ===The name space is the published names, and nothing else===
 *
 * Of the three tables a named family may declare - alternate spellings, lenient rewrites and
 * groups of names published for an external protocol - this family declares none, and that is a
 * property of the data rather than a simplification: the configuration of the implementation
 * being ported declared an empty alternate-name section for this family and no lenient or
 * external section at all. The 351 published names '''are''' the name space. The many spellings
 * of one rate are separate members that share an index name, not aliases of one member, which is
 * why they are rows of the name table and not rows of an alias table. The name lookup adds the
 * upper-case spelling of every name, as it does for every family, so `gbp-libor` does not
 * resolve but `GBP-LIBOR-BBA` in upper case does.
 *
 * ===Initialization order===
 *
 * The values of this object initialize in the order they are written, and three constraints fix
 * that order. [[FloatingRateName.namedEnum]] is built from [[FloatingRateName.values]] and must
 * follow it; [[FloatingRateName.codec]] captures the name lookup when it is created and must
 * follow that; and the two currency default tables resolve published names through the lookup and
 * must follow it as well. Nothing here reaches an index family, so loading this object loads
 * none of them - the conversions that do are methods, evaluated when a caller reaches them, which
 * is what keeps this family and the index families from depending on each other at creation time.
 */
object FloatingRateName {

  /**
   * The name of this family as it appears when the family rejects text.
   *
   * This is the simple name of the type, which is what the registry of the implementation being
   * ported used in the message of the error it raised, so a rejected name reads as it did before.
   */
  private val FamilyName: String = "FloatingRateName"

  /**
   * The suffix that distinguishes an averaged Overnight index name from the index it names.
   *
   * An index name carrying this suffix names the index without it, which is how the averaged
   * US Fed Fund names reach the one published `USD-FED-FUND` index.
   */
  private val AverageSuffix: String = "-AVG"

  /**
   * The ordering of tenors used by [[FloatingRateName.tenors]].
   *
   * This is the `Order` the tenor type publishes, which ranks tenors by length and is a total
   * order - its comparison returns zero exactly when two tenors are equal - so a sorted set built
   * with it holds every distinct tenor handed to it and loses none. The comparison by length is
   * the comparison the implementation being ported sorted its tenor sets with.
   */
  private val TenorOrdering: Ordering[Tenor] = Order[Tenor].toOrdering

  /**
   * The empty tenor set, which is what every name that is not an Ibor name answers with.
   *
   * Held once rather than built per call, since it is the answer for 192 of the 351 published
   * names and carries no state.
   */
  private val NoTenors: SortedSet[Tenor] = SortedSet.empty[Tenor](TenorOrdering)

  //-------------------------------------------------------------------------
  /**
   * Builds the member of the family described by a row of the published name table.
   *
   * The row is carried across as it stands: the external name, the index name - already holding
   * the trailing `-` of an Ibor family - the kind of rate, and the non-standard fixing date offset
   * where the row declares one. Nothing is derived here, so a member cannot disagree with the row
   * it came from.
   *
   * @param row  the row of published name data to build the member of
   * @return the floating rate name of that row
   */
  private def instanceOf(row: FloatingRateNameRow): FloatingRateName =
    new FloatingRateName(row.externalName, row.indexName, row.rateType, row.fixingDateOffsetDays) {}

  /**
   * The 351 published floating rate names, one per row of the published name table, in the order
   * that table publishes them.
   *
   * This is the single point of creation of the family; see the note on this object.
   */
  private val instances: Vector[FloatingRateName] = FloatingRateNameData.rows.map(instanceOf)

  /**
   * The 351 published floating rate names, in the order the published name table declares them:
   * the Ibor names, then the Overnight compounded names, then the Overnight averaged names, then
   * the price names.
   *
   * The order is observable through any iteration a caller performs, so it is the published order
   * rather than a re-sorting of it. The list is non-empty by construction, which is what lets
   * every operation over the family be written without a case for a family with no members.
   *
   * @return the members of the family, in published order
   */
  val values: NonEmptyList[FloatingRateName] = NonEmptyList.fromListUnsafe(instances.toList)

  /**
   * The name lookup for this family.
   *
   * It is built from [[values]] and from no table at all: this family declares no alternate
   * spelling, no lenient rewrite and no external group, for the reason given on this object. The
   * lookup therefore accepts each published external name and its upper-case spelling.
   *
   * @return the name lookup for the 351 published names
   */
  implicit val namedEnum: NamedEnum[FloatingRateName] =
    NamedEnum.of(values, Map.empty, Nil, Map.empty, FamilyName)

  /**
   * The members keyed by the external name each was published under.
   *
   * The 351 published names are distinct, so this table holds every member and loses none - which
   * is precisely what makes [[valueOf]] able to answer every member with itself. It is the table
   * the exact probe of [[valueOf]] reads first; see there for why a second table is needed beside
   * the key space of the name lookup.
   */
  private val byPublishedName: Map[String, FloatingRateName] =
    instances.iterator.map(value => value.name -> value).toMap

  /**
   * Obtains the floating rate name that the specified name identifies, if one exists.
   *
   * This is the exact lookup: it accepts a published external name and the upper-case spelling of
   * one, and answers with nothing for anything else, including text naming a concrete index.
   * [[parse]] is the wider resolution that accepts an index name as well.
   *
   * {{{
   * valueOf("GBP-LIBOR-BBA")  // Some(the GBP-LIBOR-BBA name)
   * valueOf("GBP-LIBOR-3M")   // None - that names an index, not a family
   * valueOf("Rubbish")        // None
   * }}}
   *
   * ===Why the published name is probed before the name lookup===
   *
   * A published name is answered from [[byPublishedName]] and anything else - the upper-case
   * spelling of a mixed-case name - from the family's name lookup. That order is what reproduces
   * the precedence of the loader being ported, which registered each row under its own name
   * ''unconditionally'' and under the upper-case spelling of that name ''only where the spelling
   * was still free''. A name lookup claims both of a member's keys for the first member to offer
   * them, which is the precedence of the other kind of loader in the library being ported, the one
   * that reads a family's members from its own constants; for a family whose members are all
   * distinct once case is discounted the two precedences agree, and every other family of this
   * port is such a family.
   *
   * This one is not. The published data declares four rates twice, differing in the case of one
   * word: `DKK-DESTR-OIS Compound` beside `DKK-DESTR-OIS COMPOUND`, and `SEK-SWESTR-OIS Compound`
   * beside `SEK-SWESTR-OIS COMPOUND`. The upper-case spelling of the first of each pair is the
   * published name of the second, so under the name lookup's precedence alone the first member
   * would hold both keys and the second would be unreachable by the name it was published under -
   * a member of the family that no text resolves to. The loader being ported reaches both, and so
   * does this lookup. The two members of each pair resolve onto the same index and differ in
   * nothing but their spelling, so the distinction is one of identity rather than of behaviour;
   * it matters because a name is the value's identity here, and because a member that cannot be
   * resolved by its own name cannot survive a serialization round trip.
   *
   * The family declares no alternate spellings, so no substitution precedes the exact probe.
   * Were such a table ever added, it would have to be applied to the text before this probe, as
   * the lookup being ported applied it, and this method would be the place to do it.
   *
   * @param name  the external name, such as `GBP-LIBOR-BBA`
   * @return the floating rate name of that name, or nothing where the family has no such member
   */
  def valueOf(name: String): Option[FloatingRateName] =
    byPublishedName.get(name).orElse(namedEnum.valueOf(name))

  /**
   * Tries to parse text naming a floating rate, with extended handling of index names, answering
   * with nothing where it names none.
   *
   * The published names are tried first, and only where none matches is the text resolved as a
   * concrete index - an Ibor, Overnight or Price index, in that order - and the family of that
   * index answered with. An Ibor index therefore resolves to its family and '''its tenor is
   * lost''': `GBP-LIBOR-3M` answers with the `GBP-LIBOR` family, which says nothing about three
   * months.
   *
   * The order matters and is the order of the implementation being ported. It is the opposite of
   * the order of the search over both kinds of floating rate, which probes the three index
   * families first and the published names last. The two exist for different callers - this one
   * wants a family and accepts an index name as a way of naming one, that one wants whichever kind
   * the text named - and both are ported; they are deliberately not unified.
   *
   * {{{
   * tryParse("GBP-LIBOR")     // Some(GBP-LIBOR), a published name
   * tryParse("GBP-LIBOR-3M")  // Some(GBP-LIBOR), through the index, tenor discarded
   * tryParse("GB-RPI")        // Some(GB-RPI), a published name
   * tryParse("Rubbish")       // None
   * }}}
   *
   * @param str  the text to parse, such as `GBP-LIBOR-BBA` or `GBP-LIBOR-3M`
   * @return the floating rate name the text names, or nothing where it names none
   */
  def tryParse(str: String): Option[FloatingRateName] =
    valueOf(str).orElse(FloatingRateIndex.valueOf(str).map(index => index.floatingRateName))

  /**
   * Parses text naming a floating rate, with extended handling of index names, reporting a
   * failure where it names none.
   *
   * This is [[tryParse]] with the absent case reported as [[Failure.Parsing]], carrying the
   * message the implementation being ported used for the error it raised in the same situation.
   * The text is rendered through [[Failure.describeInput]], so the message is bounded in length
   * and holds no character that could forge a line of a log; text within the bound and free of
   * control characters - every name of a floating rate among them - renders to itself, so an
   * ordinary rejected name reads exactly as it did before.
   *
   * @param str  the text to parse, such as `GBP-LIBOR-BBA` or `GBP-LIBOR-3M`
   * @return the floating rate name the text names, or a failure describing the text that named
   *   none
   */
  def parse(str: String): Either[Failure, FloatingRateName] =
    tryParse(str).toRight(
      Failure.Parsing(s"Floating rate name not known: ${Failure.describeInput(str)}"))

  //-------------------------------------------------------------------------
  /**
   * The failure reported where text names no member of this family.
   *
   * The wording is that of the name lookup of every family in this library, and of the registry
   * of the implementation being ported, so a miss reported from here reads like a miss reported
   * from the lookup itself.
   *
   * @param name  the name that resolved to no member
   * @return the failure naming this family and the rendering of the name
   */
  private def notFound(name: String): Failure =
    Failure.Parsing(s"$FamilyName name not found: ${Failure.describeInput(name)}")

  /**
   * The failure reported where a conversion resolves to an index that is not published.
   *
   * @param indexFamily  the name of the index family the conversion asked, such as `IborIndex`
   * @param name  the index name that resolved to no index
   * @return the failure naming that family and the rendering of the index name
   */
  private def indexNotFound(indexFamily: String, name: String): Failure =
    Failure.Parsing(s"$indexFamily name not found: ${Failure.describeInput(name)}")

  /**
   * Resolves a name this library itself names, fail-fast.
   *
   * The names reached through this method are the 41 named constants of [[FloatingRateNames]] and
   * the values of the two currency default tables, all of which are published by this library
   * rather than supplied by a caller. A miss is therefore not a rejected argument but a
   * disagreement between two tables this module transcribes, which has to surface at once and
   * loudly - as it did in the implementation being ported, whose loader raised while assembling
   * the family.
   *
   * The failure is raised through this module's single sanctioned fail-fast channel,
   * [[com.opengamma.strata.collect.ArgCheck]], so that every invariant breach in the library
   * reports the same kind of error. The second statement is unreachable: it exists only because
   * the first is declared to return no value while this method must produce a name.
   *
   * @param name  the external name of a name this library publishes
   * @return the floating rate name of that name
   */
  private[index] def builtIn(name: String): FloatingRateName =
    valueOf(name).getOrElse {
      val message = s"Unknown built-in floating rate name: $name"
      ArgCheck.isTrue(false, message)
      sys.error(message)
    }

  /**
   * The default Ibor rate of each of the 23 currencies that publish one.
   *
   * The published table names its defaults by external name, and they are resolved here, once,
   * while this object initializes - so a value naming no published name fails the load of this
   * module rather than the first call that asks for that currency, which is where the loader of
   * the implementation being ported detected the same breach. The resolved table is a plain map,
   * because a lookup by currency is all any caller does with it.
   */
  private val defaultIborNames: Map[Currency, FloatingRateName] =
    FloatingRateNameData.currencyDefaultIbor.iterator.map {
      case (currency, externalName) => currency -> builtIn(externalName)
    }.toMap

  /**
   * The default Overnight rate of each of the 27 currencies that publish one.
   *
   * Resolved exactly as the Ibor defaults are, and for the same reasons.
   */
  private val defaultOvernightNames: Map[Currency, FloatingRateName] =
    FloatingRateNameData.currencyDefaultOvernight.iterator.map {
      case (currency, externalName) => currency -> builtIn(externalName)
    }.toMap

  /**
   * Gets the default Ibor rate of a currency, reporting a failure where the published data
   * declares none.
   *
   * A default is a convenience for code that has a currency and needs a rate of that currency
   * without asking which one; it is not a market convention. Only 23 currencies publish one -
   * `BRL`, whose market has no term rate, is one of the currencies that does not - and the
   * absence is reported as [[Failure.MissingData]] carrying the message of the error the
   * implementation being ported raised.
   *
   * @param currency  the currency to find the default Ibor rate of
   * @return the default Ibor rate of that currency, or a failure where none is published
   */
  def defaultIborIndex(currency: Currency): Either[Failure, FloatingRateName] =
    defaultIborNames
      .get(currency)
      .toRight(Failure.MissingData(s"No default Ibor index for currency ${currency.code}"))

  /**
   * Gets the default Overnight rate of a currency, reporting a failure where the published data
   * declares none.
   *
   * As with the Ibor defaults, this is a convenience rather than a market convention, and the 27
   * currencies that publish one are the whole of what is available.
   *
   * @param currency  the currency to find the default Overnight rate of
   * @return the default Overnight rate of that currency, or a failure where none is published
   */
  def defaultOvernightIndex(currency: Currency): Either[Failure, FloatingRateName] =
    defaultOvernightNames
      .get(currency)
      .toRight(Failure.MissingData(s"No default Overnight index for currency ${currency.code}"))

  //-------------------------------------------------------------------------
  /**
   * The ordering, hashing and equality of floating rate names, all by external name.
   *
   * `Order` and `Hash` both extend `Eq`, so publishing this single value as the family's
   * equality-bearing instance makes two disagreeing notions of equality impossible; `Eq` is
   * obtained from it by subtyping and is never declared separately. Equality by name is the
   * equality of the implementation being ported, and ordering by name is this port's choice of a
   * total order - the type being ported was not comparable - which agrees with that equality.
   *
   * @return the ordering of floating rate names by name, which is also their hashing and equality
   */
  implicit val order: Order[FloatingRateName] with Hash[FloatingRateName] =
    NamedEnum.orderByName[FloatingRateName]

  /**
   * The rendering of floating rate names as text, which is the external name.
   *
   * This is the text [[FloatingRateName.toString]] produces and the text the JSON codec writes,
   * so a name renders the same way wherever it is rendered.
   *
   * @return the rendering of a floating rate name
   */
  implicit val show: Show[FloatingRateName] = NamedEnum.showByName[FloatingRateName]

  /**
   * The JSON codec for floating rate names.
   *
   * A name is written as the bare string of its external name, which is what the string
   * conversion of the implementation being ported wrote, so a document written before this port
   * reads back as the same member. Reading goes through [[FloatingRateName.valueOf]], the
   * family's exact lookup, so a published name and the upper-case spelling of one are accepted
   * and text naming a concrete index is not - matching the factory that string conversion was
   * bound to. [[FloatingRateName.parse]] is the wider resolution, and it is deliberately not the
   * decoder: a document holding `GBP-LIBOR-3M` names an index, and silently reading it as a
   * family would discard the tenor it carries.
   *
   * The decoder is the family's exact lookup rather than the generic decoder of a named family,
   * which would read the text through the key space of the name lookup alone. The two differ for
   * the four names discussed on [[FloatingRateName.valueOf]] - the pairs that differ only in the
   * case of one word - and the difference is exactly the one that decides whether every member of
   * the family survives a round trip: through the exact lookup each of the four decodes back to
   * the member that encoded it. The text written and the text accepted are otherwise identical,
   * this family declaring neither an alternate spelling nor a lenient rewrite for a generic
   * decoder to apply.
   *
   * The encoded form is a function of the value alone, so two equal names always encode to
   * identical bytes.
   *
   * @return the codec reading and writing a floating rate name as its external name
   */
  implicit val codec: Codec[FloatingRateName] =
    Codecs.parsedStringCodec[FloatingRateName](
      name => valueOf(name).toRight(notFound(name)),
      value => value.name)
}


/**
 * Constants for the commonly used floating rate names.
 *
 * Each constant refers to a standard definition of the specified rate. A rate that has a constant
 * here is fully supported by this library, with example holiday calendar data, which is what
 * distinguishes these 41 names from the 351 the published data declares: the remainder are
 * alternative spellings, historical spellings and rates this library models by name without
 * shipping the data to price them.
 *
 * The constants carry the names they have in the library being ported - `GBP_LIBOR`, `USD_SOFR`,
 * `GB_RPI` - so that code and documentation written against that library names the same values
 * here.
 *
 * ===How a constant is resolved===
 *
 * Every constant selects the published member of that external name through
 * `FloatingRateName.builtIn`, rather than constructing one, so a constant is the same object as
 * the member the lookup answers with and reference identity agrees with equality. A constant
 * naming no published name is a disagreement between two tables of this module and fails the load
 * of this object, which is the behaviour the loader of the implementation being ported had while
 * it assembled the family. Two of the names below look like alternative spellings and are
 * genuinely published rows - `EUR-ESTER`, the former name of the euro short-term rate, and
 * `USD-FED-FUND-AVG`, the averaging form of US Fed Fund - so that check is what confirms them.
 *
 * ===Two rates are no longer current===
 *
 * `CHF-TOIS` has not been published since 2017-12-29, and `EUR-ESTER` was renamed to `EUR-ESTR`;
 * the library being ported marks both deprecated. They are kept, with their published data
 * unchanged, because a trade booked while they were current still names them. They are '''not'''
 * annotated as deprecated here: this build compiles with warnings as errors and forbids
 * suppressing a warning anywhere, so annotating them would make every test and report that
 * enumerates the 41 constants fail to compile. The state of the two rates is documented here
 * instead, and is recorded among the divergences of the migration note.
 *
 * @see [[FloatingRateName]] for what a floating rate name is and how it converts to an index
 */
object FloatingRateNames {

  /** Constant for GBP-LIBOR. */
  val GBP_LIBOR: FloatingRateName = FloatingRateName.builtIn("GBP-LIBOR")

  /** Constant for USD-LIBOR. */
  val USD_LIBOR: FloatingRateName = FloatingRateName.builtIn("USD-LIBOR")

  /** Constant for USD-BSBY. */
  val USD_BSBY: FloatingRateName = FloatingRateName.builtIn("USD-BSBY")

  /** Constant for CHF-LIBOR. */
  val CHF_LIBOR: FloatingRateName = FloatingRateName.builtIn("CHF-LIBOR")

  /** Constant for EUR-LIBOR. */
  val EUR_LIBOR: FloatingRateName = FloatingRateName.builtIn("EUR-LIBOR")

  /** Constant for JPY-LIBOR. */
  val JPY_LIBOR: FloatingRateName = FloatingRateName.builtIn("JPY-LIBOR")

  /** Constant for EUR-EURIBOR. */
  val EUR_EURIBOR: FloatingRateName = FloatingRateName.builtIn("EUR-EURIBOR")

  /** Constant for AUD-BBSW. */
  val AUD_BBSW: FloatingRateName = FloatingRateName.builtIn("AUD-BBSW")

  /** Constant for CAD-CDOR. */
  val CAD_CDOR: FloatingRateName = FloatingRateName.builtIn("CAD-CDOR")

  /** Constant for CZK-PRIBOR. */
  val CZK_PRIBOR: FloatingRateName = FloatingRateName.builtIn("CZK-PRIBOR")

  /** Constant for DKK-CIBOR. */
  val DKK_CIBOR: FloatingRateName = FloatingRateName.builtIn("DKK-CIBOR")

  /** Constant for HUF-BUBOR. */
  val HUF_BUBOR: FloatingRateName = FloatingRateName.builtIn("HUF-BUBOR")

  /** Constant for MXN-TIIE. */
  val MXN_TIIE: FloatingRateName = FloatingRateName.builtIn("MXN-TIIE")

  /** Constant for NOK-NIBOR. */
  val NOK_NIBOR: FloatingRateName = FloatingRateName.builtIn("NOK-NIBOR")

  /** Constant for NZD-BKBM. */
  val NZD_BKBM: FloatingRateName = FloatingRateName.builtIn("NZD-BKBM")

  /** Constant for PLN-WIBOR. */
  val PLN_WIBOR: FloatingRateName = FloatingRateName.builtIn("PLN-WIBOR")

  /** Constant for SEK-STIBOR. */
  val SEK_STIBOR: FloatingRateName = FloatingRateName.builtIn("SEK-STIBOR")

  /** Constant for ZAR-JIBAR. */
  val ZAR_JIBAR: FloatingRateName = FloatingRateName.builtIn("ZAR-JIBAR")

  //-------------------------------------------------------------------------
  /** Constant for GBP-SONIA Overnight index. */
  val GBP_SONIA: FloatingRateName = FloatingRateName.builtIn("GBP-SONIA")

  /** Constant for USD-FED-FUND Overnight index. */
  val USD_FED_FUND: FloatingRateName = FloatingRateName.builtIn("USD-FED-FUND")

  /** Constant for USD-SOFR Overnight index. */
  val USD_SOFR: FloatingRateName = FloatingRateName.builtIn("USD-SOFR")

  /** Constant for CHF-SARON Overnight index. */
  val CHF_SARON: FloatingRateName = FloatingRateName.builtIn("CHF-SARON")

  /**
   * Constant for CHF-TOIS Overnight index.
   *
   * The rate has not been published since 2017-12-29; the library being ported marks this
   * constant deprecated. See the note on this object for why no annotation is applied here.
   */
  val CHF_TOIS: FloatingRateName = FloatingRateName.builtIn("CHF-TOIS")

  /** Constant for EUR-EONIA Overnight index. */
  val EUR_EONIA: FloatingRateName = FloatingRateName.builtIn("EUR-EONIA")

  /** Constant for EUR-ESTR Overnight index. */
  val EUR_ESTR: FloatingRateName = FloatingRateName.builtIn("EUR-ESTR")

  /**
   * Constant for EUR-ESTER Overnight index.
   *
   * The rate was renamed, and [[EUR_ESTR]] is the current name of it; the library being ported
   * marks this constant deprecated. See the note on this object for why no annotation is applied
   * here.
   */
  val EUR_ESTER: FloatingRateName = FloatingRateName.builtIn("EUR-ESTER")

  /** Constant for JPY-TONAR Overnight index. */
  val JPY_TONAR: FloatingRateName = FloatingRateName.builtIn("JPY-TONAR")

  /** Constant for AUD-AONIA Overnight index. */
  val AUD_AONIA: FloatingRateName = FloatingRateName.builtIn("AUD-AONIA")

  /** Constant for BRL-CDI Overnight index. */
  val BRL_CDI: FloatingRateName = FloatingRateName.builtIn("BRL-CDI")

  /** Constant for CAD-CORRA Overnight index. */
  val CAD_CORRA: FloatingRateName = FloatingRateName.builtIn("CAD-CORRA")

  /** Constant for DKK-TNR Overnight index. */
  val DKK_TNR: FloatingRateName = FloatingRateName.builtIn("DKK-TNR")

  /** Constant for NOK-NOWA Overnight index. */
  val NOK_NOWA: FloatingRateName = FloatingRateName.builtIn("NOK-NOWA")

  /** Constant for PLN-POLONIA Overnight index. */
  val PLN_POLONIA: FloatingRateName = FloatingRateName.builtIn("PLN-POLONIA")

  /** Constant for PLN-POLSTR Overnight index. */
  val PLN_POLSTR: FloatingRateName = FloatingRateName.builtIn("PLN-POLSTR")

  /** Constant for SEK-SIOR Overnight index. */
  val SEK_SIOR: FloatingRateName = FloatingRateName.builtIn("SEK-SIOR")

  /** Constant for THB-THOR Overnight index. */
  val THB_THOR: FloatingRateName = FloatingRateName.builtIn("THB-THOR")

  //-------------------------------------------------------------------------
  /** Constant for USD-FED-FUND Overnight index using averaging. */
  val USD_FED_FUND_AVG: FloatingRateName = FloatingRateName.builtIn("USD-FED-FUND-AVG")

  //-------------------------------------------------------------------------
  /** Constant for GB-RPI Price index. */
  val GB_RPI: FloatingRateName = FloatingRateName.builtIn("GB-RPI")

  /** Constant for EU-EXT-CPI Price index. */
  val EU_EXT_CPI: FloatingRateName = FloatingRateName.builtIn("EU-EXT-CPI")

  /** Constant for US-CPI-U Price index. */
  val US_CPI_U: FloatingRateName = FloatingRateName.builtIn("US-CPI-U")

  /** Constant for FR-EXT-CPI Price index. */
  val FR_EXT_CPI: FloatingRateName = FloatingRateName.builtIn("FR-EXT-CPI")
}

