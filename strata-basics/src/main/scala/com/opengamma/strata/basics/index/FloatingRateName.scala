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
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.NoJavaSerialization
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
 * lookup is built from those members alone, so nothing outside this file can add a member and no
 * member can be exchanged for another at run time.
 *
 * ===Where a failure is reported===
 *
 * Every member that resolves a name onto something else answers with an `Either`, because the
 * resolution can miss:
 *
 *  - '''The conversions.''' [[toIborIndex]], [[toOvernightIndex]], [[toPriceIndex]], both forms
 *    of [[toFloatingRateIndex]] and [[toIborIndexFixingOffset]] fail where the name is of the
 *    wrong kind for the conversion asked for, and where the index the name resolves to is not
 *    published.
 *  - '''Two accessors.''' [[currency]] is derived by converting to an index, so it inherits that
 *    conversion's failure, and [[normalized]] looks up the canonical name of the family, which is
 *    a lookup that can miss.
 *  - '''[[defaultTenor]].''' An Ibor name offers a tenor only while an index of its family is
 *    still published, and the published data holds a name that offers none: every one of the
 *    thirteen euroyen TIBOR Ibor indices is marked inactive, so `JPY-TIBOR-EUROYEN` has no active
 *    tenor at all.
 *
 * ===Names, equality and JSON===
 *
 * [[name]] is the external name, which is the identity of the value: two names are equal exactly
 * when their external names are equal, hashing is that of the external name, and [[toString]]
 * and the `Show` instance both render it. A member is therefore compared and stored by the
 * spelling it arrived as, not by the index it resolves to - `EUR-EURIBOR` and
 * `EUR-EURIBOR-Reuters` are different values.
 *
 * The JSON form is the bare external name string. Reading goes through the family's own name
 * lookup and therefore accepts an external name in whatever case it arrives, and nothing else:
 * text naming a concrete index, `GBP-LIBOR-3M`, is '''not''' accepted by the codec.
 * [[FloatingRateName.parse]] is the wider resolution that does accept it.
 *
 * ===Implementation notes===
 *
 * Values are immutable and safe to share between threads. No operation reaches an index family
 * while this family is being created: the index families and this family refer to each other, and
 * a field evaluated at creation time would make the two mutually dependent, so that whichever a
 * program touched first could observe the other half-built. The conversions resolve an index when
 * they are called, and [[tenors]] answers from a table of active tenors that the companion builds
 * lazily, on the first call that needs it, so the one derivation that would otherwise read an
 * index family eagerly is deferred as well.
 *
 * @param externalName  the external name, typically from FpML, such as `GBP-LIBOR-BBA`
 * @param indexName  the name of the index family this name resolves to, such as `GBP-LIBOR-`,
 *   which for an Ibor family carries the trailing `-` that a tenor completes
 * @param rateType  the kind of rate this name describes, which decides the kind of index it
 *   converts to
 * @param fixingDateOffsetDays  the number of days of the non-standard fixing date offset this
 *   name implies, or nothing where the offset of the index applies; used only for Ibor names,
 *   and in the published name data only by the Danish CIBOR names
 * @see [[FloatingRateNames]] for the named constants of the commonly used members
 * @see [[FloatingRateType]] for the kinds of rate a name may describe
 */
sealed abstract class FloatingRateName private[index] (
    val externalName: String,
    val indexName: String,
    val rateType: FloatingRateType,
    val fixingDateOffsetDays: Option[Int]) extends FloatingRate with NoJavaSerialization {

  // The closure of this family, run for every member as it is constructed: `sealed` and a
  // constructor private to this package are enforced against Scala and leave nothing in the class
  // file, so a subtype compiled by other means - which would be a floating rate name outside the
  // published table, resolving to whichever index family it declared - is refused here instead.
  // The members of the family are the instances of the companion's hidden `Impl`.
  JvmClosure.requireDeclaredMember(this, classOf[FloatingRateName])

  // The invariant of this family, which is what the check above cannot see. `Impl` is emitted with
  // a public constructor whatever the source asked for - only its `InnerClasses` entry records the
  // request, which a Java compiler honours and a hand-written class file does not - so a caller
  // that names that class directly produces an instance of exactly the class admitted above,
  // holding whatever it passed: a published external name pointed at an index family of its own,
  // describing a different kind of rate, or carrying a fixing offset the table does not declare.
  // Every field is therefore compared against the row the published name table declares for this
  // member's own external name, which is the identity a lookup resolves and the text the codec
  // writes.
  JvmClosure.requireInvariant(
    "its fields are the ones the published floating rate name table declares for its external name",
    FloatingRateName.holdsPublishedFields(this))

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
   * This is a method rather than a field, and has to stay one: the answer is derived from the
   * published Ibor index family, which refers back to this family, so a field evaluated while
   * this family was created would make the two mutually dependent. The derivation is held in a
   * lazily built table of the active tenors of each Ibor family, forced by the first call that
   * needs it, so the Ibor indices are scanned once for the whole of this family and a call is a
   * lookup in that table. Deferring the table until it is used is what keeps the two families
   * from depending on each other at creation time.
   *
   * @return the available tenors, shortest first, empty where this name has none
   */
  def tenors: SortedSet[Tenor] =
    if (!rateType.isIbor) {
      FloatingRateName.NoTenors
    } else {
      FloatingRateName.activeIborTenorsByIndexName.getOrElse(indexName, FloatingRateName.NoTenors)
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
   * reported as [[Failure.MissingData]] naming the rate. The published data reaches that case
   * through `JPY-TIBOR-EUROYEN`, whose thirteen indices are all marked inactive.
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
   * would mean the two published tables disagree. It is reported as a value rather than raised
   * because the resolution is a lookup, and a caller can act on a lookup that misses.
   *
   * @return the normalized name, or a failure where the canonical name of the family is not
   *   published
   */
  def normalized: Either[Failure, FloatingRateName] = {
    val canonical =
      if (rateType.isIbor && indexName.endsWith("-")) indexName.dropRight(1) else indexName
    FloatingRateName.valueOf(canonical).toRight(FloatingRateName.notFound(canonical))
  }

  /**
   * Converts this name to an [[IborIndex]] of the specified tenor, reporting a failure where it
   * is not an Ibor name or the index is not published.
   *
   * The index name is this name's index name followed by the canonical text of the normalized
   * tenor, so `GBP-LIBOR-` and `3M` name `GBP-LIBOR-3M`, and `1Y` normalizes to `12M` so that it
   * names `GBP-LIBOR-12M`.
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
   * non-standard offset, in which case the implied offset is the one that applies:
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

  /**
   * Converts this name to a concrete [[FloatingRateIndex]], using the
   * [[defaultTenor default tenor]] where this is an Ibor name.
   *
   * Ibor, Overnight and Price names convert; a name of any other kind reports
   * [[Failure.Invalid]]. The default tenor is resolved only for an Ibor name, which is the one
   * kind that needs one, so a name of another kind cannot fail for want of a tenor.
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
   * The tenor is used only where this is an Ibor name; an Overnight or Price name ignores it,
   * because neither has a tenor to choose.
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

  /**
   * Checks whether this floating rate name equals another value.
   *
   * Equality is that of the external name alone. Two names that resolve onto the same index are
   * therefore not equal unless they are the same name; [[normalized]] is how a caller asks for
   * the canonical name before comparing.
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
 * groups of names published for an external protocol - this family declares none. The 351
 * published names '''are''' the name space. The many spellings of one rate are separate members
 * that share an index name, not aliases of one member, which is why they are rows of the name
 * table and not rows of an alias table. The name lookup adds the upper-case spelling of every
 * name, as it does for every family, so `gbp-libor` does not resolve but `GBP-LIBOR-BBA` in
 * upper case does.
 *
 * ===Initialization order===
 *
 * The values of this object initialize in the order they are written, and three constraints fix
 * that order. [[FloatingRateName.namedEnum]] is built from [[FloatingRateName.values]] and must
 * follow it; [[FloatingRateName.codec]] captures the name lookup when it is created and must
 * follow that; and the two currency default tables resolve published names through the lookup and
 * must follow it as well. One member does reach an index family - the table of active tenors that
 * [[FloatingRateName.tenors]] answers from - and it is lazy, so it is not evaluated while this
 * object loads. Loading this object therefore reaches no index family: the conversions that reach
 * one are methods, evaluated when a caller reaches them, and the tenor table is built by the first
 * call that needs it, which is what keeps this family and the index families from depending on
 * each other at creation time.
 */
object FloatingRateName {

  /**
   * The name of this family as it appears when the family rejects text.
   *
   * This is the simple name of the type, and it is what every failure reporting an unresolved
   * name quotes, so a rejection from the lookup and one from a conversion read alike.
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
   * with it holds every distinct tenor handed to it and loses none.
   */
  private val TenorOrdering: Ordering[Tenor] = Order[Tenor].toOrdering

  /**
   * The empty tenor set, which is what every name that is not an Ibor name answers with.
   *
   * Held once rather than built per call, since it is the answer for 192 of the 351 published
   * names and carries no state.
   */
  private val NoTenors: SortedSet[Tenor] = SortedSet.empty[Tenor](TenorOrdering)

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
    new Impl(row.externalName, row.indexName, row.rateType, row.fixingDateOffsetDays)

  /**
   * Whether a member holds exactly the fields the published name table declares for its external
   * name.
   *
   * This is the invariant [[FloatingRateName]] states in its constructor, and it is stated over
   * the fields of the value rather than over the row [[instanceOf]] read, because a value of this
   * family can come into existence by a route no factory took: the hidden implementation class is
   * emitted with a public constructor, so a class file that names it directly builds an instance
   * of the one admitted class holding whatever fields it chose. What such a value would carry is
   * exactly what this family exists to fix - which index family a name resolves to - so the row
   * that declares the external name is what the other three fields have to agree with, and an
   * external name the table does not declare is not a member of this family at all.
   *
   * Nothing is derived here: [[instanceOf]] carries every column across as it stands, so each
   * field is compared with its column directly.
   *
   * @param name  the member being constructed
   * @return true when every field of the member is the one the published table declares
   */
  private def holdsPublishedFields(name: FloatingRateName): Boolean =
    FloatingRateNameData.byExternalName
      .get(name.externalName)
      .exists(row =>
        name.indexName == row.indexName &&
          name.rateType == row.rateType &&
          name.fixingDateOffsetDays == row.fixingDateOffsetDays)

  /**
   * The one implementation of a floating rate name, and the only class the family admits.
   *
   * A `sealed abstract class` needs a concrete subclass to be instantiated at all, and this is it.
   * It is declared here rather than written as an anonymous subclass at the instantiation site for
   * two reasons, both about what the class file says: a private member class is one a compiler in
   * another language refuses to name, where an anonymous class is public and can be instantiated
   * directly by such a caller; and a class declared inside this companion is a class only these
   * sources can declare, which is what lets [[FloatingRateName]] refuse, in its own constructor,
   * to be a member this family does not publish.
   *
   * Every parameter is passed straight to the family, which declares them as its fields; see the
   * documentation of [[FloatingRateName]] for what each of them means.
   *
   * @param externalName  the external name, such as `GBP-LIBOR-BBA`
   * @param indexName  the name of the index family this name resolves to
   * @param rateType  the kind of rate this name describes
   * @param fixingDateOffsetDays  the non-standard fixing date offset this name implies, where the
   *   published table declares one
   */
  private final class Impl(
      externalName: String,
      indexName: String,
      rateType: FloatingRateType,
      fixingDateOffsetDays: Option[Int])
      extends FloatingRateName(externalName, indexName, rateType, fixingDateOffsetDays)

  /**
   * The 351 published floating rate names, one per row of the published name table, in the order
   * that table publishes them.
   *
   * This is the single point of creation of the family; see the note on this object.
   */
  private val instances: Vector[FloatingRateName] = FloatingRateNameData.rows.map(instanceOf)

  /**
   * The active tenors of each Ibor family, keyed by the index name the members of that family
   * carry.
   *
   * This is the table [[FloatingRateName.tenors]] answers an Ibor name from. It holds one entry
   * per distinct index name among the Ibor members, and each entry is the set that accessor
   * describes - the tenors of the published Ibor indices whose name begins with that index name
   * and which are still published, ordered by [[TenorOrdering]]. The keys therefore cover every
   * Ibor member of this family, and a family whose every index has been retired maps to the empty
   * set rather than being absent, so each of its names is answered by a lookup alone.
   *
   * It is `lazy`, and that is the whole of what keeps this family and the index families
   * independent at creation time: the derivation reads [[IborIndex.values]], so a field evaluated
   * while this object loaded would make the two mutually dependent and let whichever half a
   * program touched first observe the other part-built. Evaluating it on the first call that needs
   * it instead means loading this object still reaches no index family. The language's lazy-value
   * semantics publish the table once, so concurrent first calls build it a single time and every
   * caller afterwards reads the same immutable map.
   *
   * It is declared after [[instances]], which it takes its keys from, and derives them from the
   * members rather than from a second reading of the published rows, so the key space of this
   * table and the membership of the family cannot disagree.
   */
  private lazy val activeIborTenorsByIndexName: Map[String, SortedSet[Tenor]] = {
    val activeIborIndices = IborIndex.values.toList.iterator.filter(index => index.active).toVector
    instances.iterator
      .filter(value => value.rateType.isIbor)
      .map(value => value.indexName)
      .distinct
      .map(indexName =>
        indexName -> SortedSet.from(
          activeIborIndices.iterator
            .filter(index => index.name.startsWith(indexName))
            .map(index => index.tenor))(TenorOrdering))
      .toMap
  }

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
   * spelling, no lenient rewrite and no external group. The lookup therefore accepts each
   * published external name and its upper-case spelling.
   *
   * @return the name lookup for the 351 published names
   */
  implicit val namedEnum: NamedEnum[FloatingRateName] =
    NamedEnum.of(values, Map.empty, Nil, Map.empty, FamilyName)

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
   * ===The precedence this lookup applies===
   *
   * There is one lookup for this family and this method is it: the family's [[namedEnum]] resolves
   * the text, and nothing here holds a table beside it. The precedence is the one that lookup
   * applies to every family - a member claims its own published name ''unconditionally'', and the
   * upper-case spelling of that name is claimed afterwards ''only where the name space still has
   * room for it''. That order is what makes the published name of a member the text that reaches
   * that member, whatever the other members are called.
   *
   * The order is observable in this family and in no other, because the published data declares
   * four rates twice, differing in the case of one word: `DKK-DESTR-OIS Compound` beside
   * `DKK-DESTR-OIS COMPOUND`, and `SEK-SWESTR-OIS Compound` beside `SEK-SWESTR-OIS COMPOUND`. The
   * upper-case spelling of the first of each pair '''is''' the published name of the second, so
   * the two share one folded key, which the second holds because the key is its own name; each
   * member of each pair therefore resolves to itself here, and the only narrowing is that the
   * folded key space of the family holds 349 entries where the family has 351 members. Had the
   * folded spelling been claimed first, the earlier member would have held both keys and the later
   * one would have been a member of the family that no text resolves to. The two members of a pair
   * resolve onto the same index and differ in nothing but their spelling, so the distinction is
   * one of identity rather than of behaviour; it matters because a name is the value's identity
   * here, and because a member that cannot be resolved by its own name cannot survive a JSON
   * round trip.
   *
   * The family declares no alternate spellings, so no substitution precedes the lookup. Were such
   * a table ever added, the family's lookup would apply it ahead of both keys, without anything
   * changing here.
   *
   * @param name  the external name, such as `GBP-LIBOR-BBA`
   * @return the floating rate name of that name, or nothing where the family has no such member
   */
  def valueOf(name: String): Option[FloatingRateName] = namedEnum.valueOf(name)

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
   * The order matters. It is the opposite of the order of the search over both kinds of floating
   * rate, which probes the three index families first and the published names last. The two exist
   * for different callers - this one wants a family and accepts an index name as a way of naming
   * one, that one wants whichever kind the text named - and are deliberately not unified.
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
   * This is [[tryParse]] with the absent case reported as [[Failure.Parsing]]. The text is quoted
   * as it stands, so the message names the whole of what was rejected. Bounding it and escaping
   * what it may hold belong to the writing of a failure, which the text form of one and
   * [[Failure.show]] perform for every part they write, so a name from outside cannot forge a
   * line of a log carrying it.
   *
   * @param str  the text to parse, such as `GBP-LIBOR-BBA` or `GBP-LIBOR-3M`
   * @return the floating rate name the text names, or a failure describing the text that named
   *   none
   */
  def parse(str: String): Either[Failure, FloatingRateName] =
    tryParse(str).toRight(
      Failure.Parsing(s"Floating rate name not known: $str"))

  /**
   * The failure reported where text names no member of this family.
   *
   * The wording is that of the name lookup of every family in this library, so a miss reported
   * from here reads like a miss reported from the lookup itself.
   *
   * @param name  the name that resolved to no member
   * @return the failure naming this family and the rendering of the name
   */
  private def notFound(name: String): Failure =
    Failure.Parsing(s"$FamilyName name not found: $name")

  /**
   * The failure reported where a conversion resolves to an index that is not published.
   *
   * @param indexFamily  the name of the index family the conversion asked, such as `IborIndex`
   * @param name  the index name that resolved to no index
   * @return the failure naming that family and the rendering of the index name
   */
  private def indexNotFound(indexFamily: String, name: String): Failure =
    Failure.Parsing(s"$indexFamily name not found: $name")

  /**
   * Resolves a name this library itself names, fail-fast.
   *
   * The names reached through this method are the 41 named constants of [[FloatingRateNames]] and
   * the values of the two currency default tables, all of which are published by this library
   * rather than supplied by a caller. A miss is therefore not a rejected argument but a
   * disagreement between two tables of this module, which has to surface at once and loudly: it
   * fails the load of the object holding the name rather than the first call that reads it.
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
   * module rather than the first call that asks for that currency. The resolved table is a plain
   * map, because a lookup by currency is all any caller does with it.
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
   * absence is reported as [[Failure.MissingData]] naming the currency.
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

  /**
   * The ordering, hashing and equality of floating rate names, all by external name.
   *
   * `Order` and `Hash` both extend `Eq`, so publishing this single value as the family's
   * equality-bearing instance makes two disagreeing notions of equality impossible; `Eq` is
   * obtained from it by subtyping and is never declared separately. Ordering by name is a total
   * order, and it agrees with the equality by name that the same value carries.
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
   * This is the codec of a closed named family, [[Codecs.namedEnumCodec]] over [[namedEnum]]. A
   * name is written as the bare string of its external name, and reading goes through the
   * family's own name lookup, [[NamedEnum.parse]].
   *
   * ===Every member survives the round trip===
   *
   * A canonical name is claimed by its own member in the family's lookup and by nothing else -
   * see [[FloatingRateName.valueOf]] for that precedence and for the four published rows that
   * make it observable - so each of the 351 members decodes back from the name it encodes to,
   * including both members of `DKK-DESTR-OIS Compound`/`DKK-DESTR-OIS COMPOUND` and of
   * `SEK-SWESTR-OIS Compound`/`SEK-SWESTR-OIS COMPOUND`. That property belongs to the lookup
   * rather than to this codec, which is why the codec is the generic one.
   *
   * ===What the decoder accepts besides a canonical name===
   *
   * The family declares no alternate spelling and no lenient rewrite, so the lenient stage of the
   * lookup reduces to its fold to upper case: text whose upper-case form is a registered key
   * resolves, and a published name therefore reads in whatever case it arrives -
   * `gbp-libor-bba` as well as `GBP-LIBOR-BBA`. That is the leniency the family's lookup defines
   * for every one of its names, not a widening peculiar to reading JSON; where the folded
   * spelling of a mixed-case name is itself the published name of another member, it resolves to
   * that member, which is the one place the leniency and the canonical claim meet.
   *
   * Text naming a concrete index is '''not''' accepted: `GBP-LIBOR-3M` names an index rather than
   * a family and no fold of it is a published name. [[FloatingRateName.parse]] is the wider
   * resolution that does accept it, and it is deliberately not the decoder - reading
   * `GBP-LIBOR-3M` as a family would silently discard the tenor the document carries.
   *
   * The encoded form is a function of the value alone, so two equal names always encode to
   * identical bytes.
   *
   * @return the codec reading and writing a floating rate name as its external name
   */
  implicit val codec: Codec[FloatingRateName] = Codecs.namedEnumCodec[FloatingRateName]
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
 * ===How a constant is resolved===
 *
 * Every constant selects the published member of that external name through
 * `FloatingRateName.builtIn`, rather than constructing one, so a constant is the same object as
 * the member the lookup answers with and reference identity agrees with equality. A constant
 * naming no published name is a disagreement between two tables of this module and fails the load
 * of this object. Two of the names below read like alternative spellings and are genuinely
 * published rows - `EUR-ESTER`, an earlier name of the euro short-term rate, and
 * `USD-FED-FUND-AVG`, the averaging form of US Fed Fund - so that check is what confirms them.
 *
 * ===Two constants name rates that are not published under that name===
 *
 * `CHF-TOIS` has not been published since 2017-12-29, and the euro short-term rate that
 * `EUR-ESTER` names is published under the name `EUR-ESTR`. Both constants are kept, with their
 * published data unchanged, because trades booked while those names were in use, and the
 * documents stored with them, still name them. Neither carries a deprecation annotation:
 * whether the rate a name resolves to is still published is carried by the `active` flag of the
 * index it resolves to, which a caller can read and branch on.
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
   * The rate has not been published since 2017-12-29, and the `CHF-TOIS` Overnight index this
   * name resolves to is marked inactive in the published index data.
   */
  val CHF_TOIS: FloatingRateName = FloatingRateName.builtIn("CHF-TOIS")

  /** Constant for EUR-EONIA Overnight index. */
  val EUR_EONIA: FloatingRateName = FloatingRateName.builtIn("EUR-EONIA")

  /** Constant for EUR-ESTR Overnight index. */
  val EUR_ESTR: FloatingRateName = FloatingRateName.builtIn("EUR-ESTR")

  /**
   * Constant for EUR-ESTER Overnight index.
   *
   * This is an earlier name of the euro short-term rate, which is published under the name
   * `EUR-ESTR`. It resolves to the same `EUR-ESTR` Overnight index that [[EUR_ESTR]] does.
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

  /** Constant for USD-FED-FUND Overnight index using averaging. */
  val USD_FED_FUND_AVG: FloatingRateName = FloatingRateName.builtIn("USD-FED-FUND-AVG")

  /** Constant for GB-RPI Price index. */
  val GB_RPI: FloatingRateName = FloatingRateName.builtIn("GB-RPI")

  /** Constant for EU-EXT-CPI Price index. */
  val EU_EXT_CPI: FloatingRateName = FloatingRateName.builtIn("EU-EXT-CPI")

  /** Constant for US-CPI-U Price index. */
  val US_CPI_U: FloatingRateName = FloatingRateName.builtIn("US-CPI-U")

  /** Constant for FR-EXT-CPI Price index. */
  val FR_EXT_CPI: FloatingRateName = FloatingRateName.builtIn("FR-EXT-CPI")
}
