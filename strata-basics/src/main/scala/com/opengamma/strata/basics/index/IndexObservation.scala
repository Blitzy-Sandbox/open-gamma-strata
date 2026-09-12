/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import java.time.LocalDate
import java.time.YearMonth

import scala.collection.immutable.SortedMap

import cats.Hash
import cats.Show

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.CurrencyPair
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * A single observation of an index.
 *
 * Implementations of this trait represent observations of an index. For example, an observation
 * of `GBP-LIBOR-3M` at a specific fixing date.
 *
 * An [[Index]] is the agreed mechanism for determining a financial indicator; an observation is
 * one use of that mechanism - the index together with the point at which its figure is fixed,
 * and whatever dates follow from that point. Where an index is reference data shared by every
 * trade that refers to it, an observation belongs to the instrument that made it, which is why
 * an observation is an ordinary value built on demand rather than a member of a published set.
 *
 * What identifies the point of fixing, and what follows from it, differs by the kind of index,
 * and this abstraction therefore asks for nothing but the index itself. The four implementations
 * are [[IborIndexObservation]] (a fixing date, with the deposit period and year fraction it
 * implies), [[OvernightIndexObservation]] (a fixing date, with its publication and effective
 * dates), [[PriceIndexObservation]] (a fixing month, price indices publishing monthly) and
 * [[FxIndexObservation]] (a fixing date and the maturity date it implies). Code that needs one
 * of those specifics holds the implementation; code that only needs to know which index was
 * observed - to look up market data for it, or to report on it - holds this trait.
 *
 * ===This trait is sealed, and its four implementations are declared below it===
 *
 * The observations of this port are a closed set of four, one per index family, and the families
 * themselves are closed: the sealed [[Index]] hierarchy admits an Ibor, an Overnight, a price and
 * an exchange-rate index and nothing else, so there is no fifth kind of thing to observe. Sealing
 * this trait is what turns that into a property the compiler holds the module to.
 *
 * Scala 2 admits a direct subtype of a sealed type only in the same source file as the type, so
 * the four implementations are declared in this file rather than in four files of their own. That
 * is the same treatment, for the same reason, that this port gives its other closed hierarchies -
 * `HolidayCalendar.scala` holds every calendar, and `Index.scala` holds the whole index hierarchy
 * with its four families - and it is the ordering of this file: the trait, then the Ibor, the
 * Overnight, the price and the exchange-rate observation, each with the factories, typeclass
 * instances and JSON codec that belong to it.
 *
 * Two consequences follow. First, a `match` over this trait is checked for exhaustiveness by the
 * compiler, so code that narrows an observation is told when it has missed a kind, and a default
 * branch is a choice rather than an obligation. Second, the set of observations cannot be extended
 * from outside this file, which is what makes the first consequence worth having; an application
 * with an indicator of its own models it as its own type rather than as a member of this family.
 *
 * Not every abstraction of this port is closed, and the two that are not say so on themselves:
 * `ReferenceData` is open because supplying reference data is precisely what an application does
 * with it, and `FloatingRate` is open because it is implemented by `FloatingRateName` as well as
 * by the index families, whose four hundred rows of data sealing would drag into `Index.scala`.
 * Neither reason applies here.
 *
 * ===Only the index is declared here===
 *
 * This trait declares one member, the index, because that is the one thing every observation
 * has. In particular it declares no currency accessor. The interface being ported declares none
 * either, and the implementations could not agree on one: three of them report a single
 * `currency` taken from their index, while [[FxIndexObservation]] reports a `currencyPair`,
 * an exchange rate being a relation between two currencies rather than an amount in one. Each
 * implementation therefore declares the accessor that is honest for it, and a caller holding
 * this trait reaches it by narrowing to the kind in hand.
 *
 * ===Implementation notes===
 *
 * An implementation is expected to be an immutable value, and is therefore safe to share between
 * threads, and to report the same index for its whole lifetime, since the index is what the
 * observation is of. An implementation is also expected to be internally consistent: any date it
 * derives from its fixing point is derived through the index it reports, so that the observation
 * and the index cannot disagree.
 *
 * Each implementation satisfies `index` with the index type of its own family - `IborIndex`,
 * `OvernightIndex`, `PriceIndex` or `FxIndex`, every one of them a subtype of [[Index]] - which
 * narrows the result for a caller holding the implementation and is what lets such a caller
 * reach the fields of the index without a cast, while still satisfying this declaration.
 *
 * This trait holds no data of its own: it is an abstraction over values that carry their own,
 * and every value reaching it is one of the four implementations. No JSON serialization
 * instances are declared for it, accordingly; those are declared by, and belong to, the
 * implementations that do hold data.
 *
 * @see [[Index]] for the indices that can be observed
 * @see [[IborIndexObservation]], [[OvernightIndexObservation]], [[PriceIndexObservation]] and
 *      [[FxIndexObservation]] for the four kinds of observation
 */
sealed trait IndexObservation {

  /**
   * Gets the index to be observed.
   *
   * The index is the identity of what was observed and is never absent: an observation cannot
   * exist without the index it was made of.
   *
   * @return the index
   */
  def index: Index
}

//-------------------------------------------------------------------------
/**
 * Defines the observation of a rate of interest from a single Ibor index.
 *
 * An interest rate determined directly from an Ibor index. For example, a rate determined from
 * `GBP-LIBOR-3M` on a single fixing date.
 *
 * The observation carries the fixing date the caller asked about together with the three values
 * that follow from it - the effective date of the implied deposit, its maturity date and the year
 * fraction of the period between them - so that everything a calculation needs from the index is
 * settled once, at the point the observation is made, rather than being recomputed against
 * reference data at each use.
 *
 * ===Construction===
 *
 * An observation is a '''validated''' value, and what is validated is its internal consistency:
 * the three derived values are not the caller's to supply, they are what the index makes of the
 * fixing date. [[IborIndexObservation.of]] is therefore the only public route to a value, and it
 * computes all three - the effective date from the fixing date, the maturity date from the
 * '''effective''' date, and the year fraction on the day count of the index - exactly as the
 * factory of the bean being ported did. The type is a `sealed abstract case class` with a private
 * constructor, which is what leaves it without a public `apply` or `copy`: there is no route to an
 * instance whose maturity date disagrees with its index, which a public constructor or a `copy` of
 * one field would otherwise allow. The `equals`, `hashCode` and `unapply` of a case class are all
 * still present, so an observation still takes part in a pattern match over its five fields.
 *
 * Computing the derived dates needs the fixing calendar of the index and both of its date offsets
 * resolved, so construction needs reference data and can fail - an index whose calendar the
 * supplied data does not contain is reported as missing data rather than raised. That is why the
 * factory answers with `Either` and not with an observation.
 *
 * A caller computing the observations of many fixings of one index should use
 * [[IborIndexObservation.resolve]], which resolves the calendar and the offsets once and hands
 * back a function of the fixing date. It is the same calculation as [[IborIndexObservation.of]]
 * and produces the same values; only the resolution is shared.
 *
 * ===Equality===
 *
 * Equality and hashing compare '''all five''' fields, which is what the bean equality of the
 * original compared, and the year fraction is compared by its bit pattern rather than
 * numerically - the comparison `JodaBeanUtils` performed on a `double`. Two consequences are
 * deliberate: an observation whose year fraction is not a number equals itself, and a negative
 * zero differs from a positive zero. Comparing all five fields is specific to this observation:
 * `OvernightIndexObservation` and `FxIndexObservation` compare only their index and fixing date,
 * because their remaining fields are derived from those two, whereas the year fraction here is
 * also a function of the day count and so is compared as well.
 *
 * ===Divergences from the Java original===
 *
 * These are the points on which this port deliberately differs from
 * `com.opengamma.strata.basics.index.IborIndexObservation`, recorded here for
 * `SCALA_MIGRATION.md`:
 *
 *  - '''The factory reports a failure instead of raising.''' Resolving the fixing calendar of the
 *    index against reference data it does not contain produced
 *    `ReferenceDataNotFoundException`; here it is `Failure.MissingData` in the left of an
 *    `Either`, so a caller decides what to do about missing reference data instead of catching.
 *  - '''There is no public constructor and no builder.''' The bean was generated with a
 *    package-scoped constructor and a private builder, both of which admitted the five fields
 *    directly. The port has two doors instead: the public factory, and an internal total
 *    constructor used only by the resolution above, which has already computed the same values.
 *  - '''The meta-bean, the property machinery and Java serialization are gone.''' `meta()`,
 *    `metaBean()`, the `MetaProperty` declarations, `Serializable` and the `serialVersionUID` have
 *    no counterpart; the JSON codecs below are the serialization of this port.
 *  - '''The hash code is stated as a function of the fields alone.''' The generated hash seeded
 *    itself with `getClass().hashCode()`, an identity hash that differs between runs of a program;
 *    here the mixing starts from the index, so the hash of an observation is the same in every run,
 *    which is what the byte-stability and hashing properties of the test suite rely on.
 *
 * @see [[IborIndex]] for the index being observed
 * @see [[IndexObservation]] for the abstraction over the four kinds of observation
 *
 * @param index  the Ibor index the rate is determined from, a well known market index such as
 *   `GBP-LIBOR-3M`
 * @param fixingDate  the date of the index fixing, an adjusted date that is a business day of the
 *   fixing calendar of the index
 * @param effectiveDate  the effective date of the investment implied by the fixing date, equal to
 *   `index.calculateEffectiveFromFixing(fixingDate, refData)`
 * @param maturityDate  the maturity date of the investment implied by the fixing date, equal to
 *   `index.calculateMaturityFromEffective(effectiveDate, refData)`
 * @param yearFraction  the year fraction of the investment implied by the fixing date, the
 *   fraction of the year between the effective date and the maturity date on the day count of the
 *   index - typically close to 1 for one year and close to 0.5 for six months
 */
sealed abstract case class IborIndexObservation private (
    index: IborIndex,
    fixingDate: LocalDate,
    effectiveDate: LocalDate,
    maturityDate: LocalDate,
    yearFraction: Double)
    extends IndexObservation {

  /**
   * Gets the currency of the Ibor index.
   *
   * A rate of interest is a rate on an amount, and the currency of that amount is the currency of
   * the index: an observation of `GBP-LIBOR-3M` is an observation in sterling. The accessor is
   * total and reads the index, so it cannot disagree with it.
   *
   * @return the currency of the index
   */
  def currency: Currency = index.currency

  //-------------------------------------------------------------------------
  /**
   * Checks if this observation equals another observation.
   *
   * All five fields are compared, as the bean equality of the original compared all five
   * properties. The three dates and the index compare by value; the year fraction compares by its
   * bit pattern, which is the comparison `JodaBeanUtils.equal(double, double)` performed, and it
   * differs from a numeric comparison for exactly two inputs - a year fraction that is not a
   * number, which here equals itself, and a negative zero, which here differs from a positive
   * zero. An object of any other type is not equal.
   *
   * Comparing the derived fields is not redundant even though they are derived: two observations
   * of the same index at the same fixing date can only differ in them if they were built against
   * different reference data, and that is a difference a caller should see rather than one this
   * type should hide.
   *
   * @param obj  the object to compare to
   * @return true if the other object is an observation of the same index at the same fixing date
   *   with the same derived dates and year fraction
   */
  override def equals(obj: Any): Boolean = obj match {
    case other: IborIndexObservation =>
      (this eq other) ||
        (index == other.index &&
          fixingDate == other.fixingDate &&
          effectiveDate == other.effectiveDate &&
          maturityDate == other.maturityDate &&
          java.lang.Double.compare(yearFraction, other.yearFraction) == 0)
    case _ => false
  }

  /**
   * Returns a hash code consistent with [[equals]].
   *
   * The mixing is that of the bean this replaces - each field hashed in turn into an accumulator
   * multiplied by the usual odd prime - with the year fraction hashed by its bit pattern so that
   * two observations which [[equals]] calls equal always agree here too. The one departure is the
   * seed: the generated code started from `getClass().hashCode()`, which is an identity hash and
   * therefore differs between runs, so the mixing here starts from the index instead. Every part
   * of the result is a function of the value alone, so the hash of an observation is identical in
   * every run of every program.
   *
   * @return the hash code of the index, the fixing date, the two derived dates and the year
   *   fraction
   */
  override def hashCode: Int = {
    val withIndex = index.hashCode
    val withFixingDate = withIndex * 31 + fixingDate.hashCode
    val withEffectiveDate = withFixingDate * 31 + effectiveDate.hashCode
    val withMaturityDate = withEffectiveDate * 31 + maturityDate.hashCode
    withMaturityDate * 31 + java.lang.Double.hashCode(yearFraction)
  }

  /**
   * Returns the text of this observation.
   *
   * The form is the one the bean generated - the type name followed by its five properties in
   * declaration order, in braces - and it is kept as it was so that a message or a log written by
   * ported code reads exactly as it did:
   *
   * {{{
   * IborIndexObservation{index=GBP-LIBOR-3M, fixingDate=2014-06-30, effectiveDate=2014-06-30, maturityDate=2014-09-30, yearFraction=0.2520547945205479}
   * }}}
   *
   * The index renders as its name, since that is the text of an index, and each date as its ISO
   * form. This observation is the one of the four whose text is the generated field-list form; the
   * other three were hand-written in the original and keep their own shorter forms there.
   *
   * @return the text of the index, the fixing date, the two derived dates and the year fraction
   */
  override def toString: String =
    s"IborIndexObservation{index=$index, fixingDate=$fixingDate, " +
      s"effectiveDate=$effectiveDate, maturityDate=$maturityDate, yearFraction=$yearFraction}"
}

/**
 * The factories, instances and JSON codecs of Ibor index observations.
 *
 * Two doors lead into the type and there is no third. [[IborIndexObservation.of]] is the public
 * one, and it derives the three dependent values of an observation from the index and the fixing
 * date. [[IborIndexObservation.resolve]] is the same derivation with the resolution of reference
 * data lifted out of it, for a caller observing many fixings of one index; it reaches the type
 * through an internal total constructor, because by the time it builds an observation the values
 * that constructor would otherwise have to recompute are already in hand.
 *
 * That resolution is published twice and implemented once. [[IborIndexObservation.resolve]] is the
 * implementation, and it is where the observation is named; [[IborIndex.resolve]] is the same call
 * reached from the index, which is where the hierarchy being ported declared it. The hierarchy
 * itself states only the per-fixing calculation, over the construction of a result rather than
 * over this type, so the resolution can be assembled from either side without either side
 * duplicating it.
 */
object IborIndexObservation {

  /**
   * Creates an instance from an index and fixing date.
   *
   * The three dependent values of an observation are derived here and are not the caller's to
   * supply. The derivation is the one of the factory being ported, in its order, which matters:
   * the effective date is found from the fixing date, the maturity date from the '''effective'''
   * date - not from the fixing date - and the year fraction from the resulting period on the day
   * count of the index. Finding the maturity date from the fixing date instead would move it for
   * every index whose effective dates live in a different calendar from its fixing dates, so the
   * two steps are kept apart exactly as the original kept them.
   *
   * Both steps resolve holiday calendars against the supplied reference data, and neither can be
   * completed when the data does not contain them, so the result is an `Either`: an index whose
   * fixing or effective calendar is absent answers with missing data, where the original raised
   * `ReferenceDataNotFoundException`.
   *
   * The fixing date is carried exactly as supplied. A date that is not a fixing date of the index
   * is moved onto the next one for the purpose of deriving the dependent dates - which is what the
   * index does - but it is the caller's date that the observation reports, as in the original.
   *
   * @param index  the index
   * @param fixingDate  the fixing date
   * @param refData  the reference data to use when resolving holiday calendars
   * @return the rate observation, or the failure describing why reference data could not be
   *   resolved
   */
  def of(
      index: IborIndex,
      fixingDate: LocalDate,
      refData: ReferenceData): Either[Failure, IborIndexObservation] =
    for {
      effectiveDate <- index.calculateEffectiveFromFixing(fixingDate, refData)
      maturityDate <- index.calculateMaturityFromEffective(effectiveDate, refData)
    } yield create(
      index,
      fixingDate,
      effectiveDate,
      maturityDate,
      index.dayCount.yearFraction(effectiveDate, maturityDate))

  /**
   * Resolves an index against reference data once and answers with the observation of a fixing.
   *
   * A caller observing many fixings of one index should not resolve a holiday calendar per
   * fixing. This resolves the fixing calendar and both date offsets of the index up front and
   * hands back a function that consults no reference data at all: given a fixing date it derives
   * the effective date, the maturity date and the year fraction and builds the observation. That
   * separation of the resolution from the per-fixing calculation is what the `resolve` method of
   * the index being ported existed for, and it is preserved here.
   *
   * The function produces, for any fixing date, the value [[IborIndexObservation.of]] produces for
   * the same index, fixing date and reference data. The two are one derivation expressed twice
   * over - the index performs it in both cases - and the only difference is when the calendars are
   * resolved.
   *
   * [[IborIndex.resolve]] is this operation reached from the index, and calls this method; the two
   * are one operation published on both of the types it relates, as the library being ported
   * published it on the index alone.
   *
   * @param index  the index
   * @param refData  the reference data to use when resolving holiday calendars
   * @return the observation of a fixing of this index, or the failure describing why reference
   *   data could not be resolved
   */
  def resolve(
      index: IborIndex,
      refData: ReferenceData): Either[Failure, LocalDate => IborIndexObservation] =
    index.resolveWith(refData) { (fixingDate, effectiveDate, maturityDate, yearFraction) =>
      create(index, fixingDate, effectiveDate, maturityDate, yearFraction)
    }

  /**
   * Creates a value, which every route into the type funnels through.
   *
   * This is the only instantiation of the type. The constructor of a `sealed abstract case class`
   * is reachable only from inside the file that declares it, and
   * `new IborIndexObservation(...) {}` - an anonymous subclass of the abstract case class - is how
   * it is reached; that is what leaves the type without a public `apply` or `copy` while keeping
   * the `equals`, `hashCode` and `unapply` a case class provides.
   *
   * The method performs no check of its own, and it is visible to this package alone so that the
   * obligation to supply consistent values cannot be taken on by a caller outside it. Both of its
   * callers derive the three dependent values from the index rather than accepting them:
   * [[IborIndexObservation.of]] does so directly, and [[IborIndexObservation.resolve]] does so
   * through the resolved calculation of the index, which performs the same three steps in the same
   * order. Widening the public factory to accept the derived values instead would defeat the
   * purpose of this type being validated at all.
   *
   * @param index  the index
   * @param fixingDate  the fixing date, exactly as the caller supplied it
   * @param effectiveDate  the effective date, already derived from the fixing date by the index
   * @param maturityDate  the maturity date, already derived from the effective date by the index
   * @param yearFraction  the year fraction, already computed on the day count of the index
   * @return the observation
   */
  private[index] def create(
      index: IborIndex,
      fixingDate: LocalDate,
      effectiveDate: LocalDate,
      maturityDate: LocalDate,
      yearFraction: Double): IborIndexObservation =
    new IborIndexObservation(index, fixingDate, effectiveDate, maturityDate, yearFraction) {}

  //-------------------------------------------------------------------------
  /**
   * The hashing and equality of Ibor index observations.
   *
   * This is the single equality-bearing instance of the type, and it defers to
   * [[IborIndexObservation.equals]] and [[IborIndexObservation.hashCode]], so the comparison a
   * caller reaches through cats and the comparison a collection reaches through the JVM are one
   * comparison - including its treatment of a year fraction that is not a number and of a negative
   * zero. `Eq` is not declared separately: `Hash` extends it, and one declaration is what keeps a
   * second, differing notion of equality from coming into existence.
   *
   * No ordering is declared. The bean being ported does not implement `Comparable`, and there is
   * no ordering of observations that is theirs rather than a particular caller's: the fixing date,
   * the effective date and the maturity date are each a defensible key, and a caller who wants one
   * states it at the point of sorting.
   *
   * @return the hashing and equality of Ibor index observations
   */
  implicit val hash: Hash[IborIndexObservation] = Hash.fromUniversalHashCode

  /**
   * The rendering of Ibor index observations as text.
   *
   * Renders what [[IborIndexObservation.toString]] renders, so the two ways of putting an
   * observation into a message agree.
   *
   * @return the rendering of an Ibor index observation
   */
  implicit val show: Show[IborIndexObservation] = Show.show(_.toString)

  //-------------------------------------------------------------------------
  // the double policy of this port, which writes and reads the values JSON cannot express as one
  // of three tagged strings; the import has to precede the derivations below so that it outranks
  // the plain numeric instance the JSON library publishes for a double
  import Codecs.implicits.doubleCodec

  /**
   * The field shape both codecs are derived from, which the decoder reads before checking.
   *
   * Decoding a validated type is two steps: read the fields, then decide whether they describe a
   * value. This product is the first step, and encoding is the same two steps in reverse, so both
   * codecs below derive from this one declaration and the JSON shape of an observation is stated
   * exactly once. It is private and never returned - the only values of it that exist are the ones
   * the two codecs build, so no caller can hold an unchecked set of fields. Its field names are
   * the JSON keys, and they are the names of the five fields of [[IborIndexObservation]] itself,
   * which are the names of the five properties of the bean being ported, in their declaration
   * order.
   *
   * Deriving either codec from [[IborIndexObservation]] directly is not possible: the compile-time
   * derivation reads the public constructor of a product, and a validated type has none - it is an
   * abstract case class whose constructor is private - so there is no public shape to derive from.
   *
   * @param index  the index, carried as its name, so that the time of day, the zone and the date
   *   offsets it holds never appear in a document
   * @param fixingDate  the fixing date, carried as its ISO date string
   * @param effectiveDate  the effective date as the document states it, checked below against the
   *   date the index derives
   * @param maturityDate  the maturity date as the document states it, checked below against the
   *   date the index derives
   * @param yearFraction  the year fraction as the document states it, read as a number or as one
   *   of the three tagged strings, and checked below against the fraction the index derives
   */
  private final case class Raw(
      index: IborIndex,
      fixingDate: LocalDate,
      effectiveDate: LocalDate,
      maturityDate: LocalDate,
      yearFraction: Double)

  /** The derived decoder of the raw field shape, used by the checking decoder below. */
  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /** The derived encoder of the raw field shape, used by the encoder below. */
  private val rawEncoder: Encoder[Raw] = deriveEncoder[Raw]

  /**
   * The JSON encoding of Ibor index observations.
   *
   * An observation is an object of five fields - the index as its name, the three dates as ISO
   * date strings and the year fraction as a number:
   *
   * {{{
   * {"index":"GBP-LIBOR-3M",
   *  "fixingDate":"2014-06-30",
   *  "effectiveDate":"2014-06-30",
   *  "maturityDate":"2014-09-30",
   *  "yearFraction":0.2520547945205479}
   * }}}
   *
   * The derived values are written even though the index can derive them, because a document is
   * then a complete statement of the observation rather than an instruction to recompute it, and
   * because writing them is what lets the decoder below detect a document that contradicts itself.
   *
   * Both halves of the codec are assembled by the same compile-time derivation over the same raw
   * shape, which is what keeps them from drifting apart, and no part of the encoding inspects a
   * class while the program runs. Two equal values encode to identical bytes: the five fields are
   * written in their declaration order, an index has one name, a date has one ISO form and a
   * double has one form under the policy of this port. The result is wrapped so that a field
   * holding no value would be omitted, which is the policy every product of this port follows -
   * all five fields of this type are required, so the wrapping changes nothing about its output
   * and exists so that the policy holds without exception.
   *
   * @return the JSON encoding of Ibor index observations
   */
  implicit val encoder: Encoder[IborIndexObservation] =
    Codecs.dropNulls(rawEncoder.contramap[IborIndexObservation] { value =>
      Raw(
        value.index,
        value.fixingDate,
        value.effectiveDate,
        value.maturityDate,
        value.yearFraction)
    })

  /**
   * The JSON decoding of Ibor index observations.
   *
   * This is the inverse of the encoding above, and it decides whether the fields describe a value
   * exactly as a caller's arguments are decided. The index and the fixing date are the only two
   * fields a document really determines, so the observation is '''rebuilt''' from them through
   * [[IborIndexObservation.of]] against [[ReferenceData.standard]], and the three derived fields
   * the document states are then compared against what that rebuilding derived. A document whose
   * effective date, maturity date or year fraction disagrees is a decoding failure naming every
   * field that disagrees, so an inconsistent payload cannot produce a value no factory of this
   * type would have built. The year fraction is compared by bit pattern, the comparison
   * [[IborIndexObservation.equals]] uses, so a document stating a year fraction of `"NaN"` decodes
   * only where the index derives one.
   *
   * All five fields have to be present. An index whose fixing calendar the standard reference data
   * cannot resolve is likewise a decoding failure, carried through from the rebuilding.
   *
   * @return the JSON decoding of Ibor index observations
   */
  implicit val decoder: Decoder[IborIndexObservation] =
    Codecs.checkedDecoder[Raw, IborIndexObservation] { raw =>
      of(raw.index, raw.fixingDate, ReferenceData.standard).flatMap(checked(raw, _))
    }(rawDecoder)

  /**
   * Checks the derived fields of a document against the observation rebuilt from its index and
   * fixing date.
   *
   * Every disagreement is reported, not just the first, because a document stating the dates of
   * some other index is likely to disagree in more than one field and a reader of the failure is
   * better served by all of them. The three comparisons are the three the equality of this type
   * performs on the derived fields, so a document passes here exactly when the value it states
   * equals the value rebuilt from it.
   *
   * @param raw  the fields as the document stated them
   * @param rebuilt  the observation derived from the index and fixing date of the document
   * @return the rebuilt observation, or the failure naming the fields the document disagrees on
   */
  private def checked(
      raw: Raw,
      rebuilt: IborIndexObservation): Either[Failure, IborIndexObservation] = {

    val disagreements: List[String] = List(
      Option.when(raw.effectiveDate != rebuilt.effectiveDate)(
        s"effectiveDate is ${raw.effectiveDate} but the index derives ${rebuilt.effectiveDate}"),
      Option.when(raw.maturityDate != rebuilt.maturityDate)(
        s"maturityDate is ${raw.maturityDate} but the index derives ${rebuilt.maturityDate}"),
      Option.when(java.lang.Double.compare(raw.yearFraction, rebuilt.yearFraction) != 0)(
        s"yearFraction is ${raw.yearFraction} but the index derives ${rebuilt.yearFraction}")
    ).flatten
    if (disagreements.isEmpty) {
      Right(rebuilt)
    } else {
      Left(
        Failure
          .Invalid(
            s"IborIndexObservation of '${raw.index.name}' at fixing date ${raw.fixingDate} states " +
              s"values the index does not derive: ${disagreements.mkString("; ")}")
          .withAttribute("index", raw.index.name)
          .withAttribute("fixingDate", raw.fixingDate.toString))
    }
  }
}

//-------------------------------------------------------------------------
/**
 * Information about a single observation of an Overnight index.
 *
 * Observing an Overnight index requires knowledge of the index, the fixing date, and the
 * publication, effective and maturity dates that the fixing date implies. A value of this type
 * holds all five, together with the year fraction of the one-day investment they describe, so
 * that the instrument holding the observation can be priced without consulting reference data
 * a second time.
 *
 * An Overnight rate is fixed on one day and applies to a borrowing that starts on its effective
 * date and matures one business day later. Three offsets of the index turn a fixing date into
 * those dates - the publication offset, the effective offset, and the fixed one-business-day
 * step from the effective date to maturity - and each of the three is counted in business days
 * of the fixing calendar of the index, which is why building an observation needs reference
 * data. For `GBP-SONIA` the rate fixed on a day is published on the next business day and
 * applies from the fixing day itself; for `THB-THOR` it is published the same day and applies
 * two business days later. Both shapes reach this type through the same route.
 *
 * ===Obtaining one===
 *
 * [[OvernightIndexObservation.of]] is the only way to obtain a value, and it takes the index,
 * the fixing date and the reference data to resolve the fixing calendar of the index against.
 * Every date and the year fraction are then derived, in the one place, from the index the
 * observation reports - so an observation can never describe dates the index it names would not
 * produce. This is AAP §0.3.3 construction kind `[V]`: the type is a
 * `sealed abstract case class` with a private constructor, which leaves it without a public
 * `apply` and without a public `copy` while keeping the `unapply` a case class provides, so
 * pattern matching over an observation still reads as it would over any other value:
 *
 * {{{
 * val observation: Either[Failure, OvernightIndexObservation] =
 *   OvernightIndexObservation.of(OvernightIndices.GBP_SONIA, LocalDate.of(2016, 2, 22), ReferenceData.standard)
 *
 * observation.foreach {
 *   case OvernightIndexObservation(index, fixingDate, publicationDate, effectiveDate, maturityDate, yearFraction) =>
 *     println(s"$index fixes on $fixingDate, published $publicationDate, " +
 *       s"accruing $yearFraction from $effectiveDate to $maturityDate")
 * }
 * }}}
 *
 * The builder of the bean being ported is deliberately not carried over. A builder admits any
 * combination of the six fields, including combinations in which the dates contradict the index,
 * and the whole reason this type is `[V]` is that such a value must not be constructible. The
 * factory replaces it, as AAP §0.3.3 prescribes for every bean of this port.
 *
 * Construction reports a failure rather than raising one: the fixing calendar of the index is
 * named by identifier and has to be found in the reference data supplied, and reference data
 * that does not hold it yields `Left(Failure.MissingData)`. That is the whole of the failure
 * surface of this type - once a value exists, every accessor on it is total.
 *
 * ===Equality ignores the derived values===
 *
 * Two observations are equal when they name the same index and the same fixing date. The
 * publication, effective and maturity dates and the year fraction take no part in equality or
 * hashing, which is exactly what the bean being ported documented and implemented by hand. It
 * is a defensible position rather than an oversight: those four values are functions of the
 * index, the fixing date and the calendar of the index, so two observations that agree on the
 * first two describe the same observation, and comparing the derived values as well would only
 * let a difference in the reference data used at construction masquerade as a difference between
 * observations.
 *
 * One consequence matters to anyone writing a test against this type, and is recorded here
 * because it cannot be seen from the signatures: '''a round-trip property stated with equality
 * alone cannot observe the derived fields at all'''. A serializer that dropped the publication
 * date, or wrote a wrong year fraction, would still satisfy `decode(encode(a)) == a`, because
 * `a` and the decoded value would agree on index and fixing date and equality asks for nothing
 * more. `json/JsonRoundTripSpec` must therefore compare `publicationDate`, `effectiveDate`,
 * `maturityDate` and `yearFraction` field by field for this type, in addition to the equality
 * property it runs for every codec-bearing type. This is the same concern AAP §0.6.2 raises for
 * the identifier-only equality of `ImmutableHolidayCalendar`. The decoder of this type defends
 * the same ground from its own side, by re-deriving the four values and refusing a payload that
 * disagrees with them.
 *
 * Because the year fraction is not part of equality, the bit-pattern comparison of doubles that
 * types such as `CurrencyAmount` and `IborIndexObservation` need does not arise here. No `Double`
 * is compared by `equals` at all.
 *
 * ===Instances===
 *
 * `Hash` and `Show` are provided, and they are the whole of the typeclass surface. `Hash`
 * extends `Eq`, so it is the single equality-bearing instance of the type and a separate `Eq`
 * would be a second answer to the same question; it is taken from the `equals` and `hashCode`
 * of the type and therefore ignores the derived values just as they do. No `Order` is offered,
 * because the bean being ported is not `Comparable` and there is no ordering of observations
 * across indices that means anything - a caller that needs a reproducible sequence should sort
 * by the pair of index name and fixing date, both of which are ordered.
 *
 * @param index  the Overnight index the rate is queried from
 * @param fixingDate  the date the rate is fixed on, a business day of the fixing calendar of
 *   the index
 * @param publicationDate  the date the rate fixed on the fixing date is published, derived from
 *   the fixing date through the publication offset of the index
 * @param effectiveDate  the date the investment the rate applies to starts, derived from the
 *   fixing date through the effective offset of the index
 * @param maturityDate  the date the investment the rate applies to matures, one business day
 *   after the effective date
 * @param yearFraction  the year fraction between the effective date and the maturity date,
 *   measured by the day count of the index, typically close to `1/360` or `1/365`
 * @see [[IndexObservation]] for the abstraction over every kind of observation
 * @see [[OvernightIndex]] for the index observed and the offsets the dates are derived through
 */
sealed abstract case class OvernightIndexObservation private (
    index: OvernightIndex,
    fixingDate: LocalDate,
    publicationDate: LocalDate,
    effectiveDate: LocalDate,
    maturityDate: LocalDate,
    yearFraction: Double)
    extends IndexObservation {

  /**
   * Gets the currency of the Overnight index.
   *
   * An Overnight rate is a rate in one currency, so the currency of an observation is simply
   * that of the index it was made of. This is total: the index is never absent, and an index
   * always names its currency.
   *
   * @return the currency of the index observed
   */
  final def currency: Currency = index.currency

  //-------------------------------------------------------------------------
  /**
   * Compares this observation to another based on the index and fixing date.
   *
   * The publication, effective and maturity dates and the year fraction are ignored, which is
   * the equality the bean being ported defined by hand and the reasoning is given on the type.
   * Because those four values are derived from the two compared here through the calendar of
   * the index, two observations that agree here describe the same observation of the same index.
   *
   * The comparison narrows the other value by type rather than by comparing runtime classes,
   * which is how every value of this port compares: instances of this type are created as an
   * anonymous subclass of it - the way a `sealed abstract case class` is instantiated - so its
   * runtime class is an implementation detail that equality must not depend on.
   *
   * @param obj  the other value to compare to
   * @return true when the other value is an observation of the same index on the same fixing
   *   date
   */
  final override def equals(obj: Any): Boolean = obj match {
    case other: OvernightIndexObservation =>
      (this eq other) || (index == other.index && fixingDate == other.fixingDate)
    case _ => false
  }

  /**
   * Returns a hash code consistent with [[equals]], mixing the index and the fixing date.
   *
   * The mixing is that of the bean this replaces - a seed multiplied by the usual odd prime and
   * added to the hash of the index, then the same step again for the fixing date - with one
   * substitution. The bean seeded the mixing with `getClass().hashCode()`, which is the identity
   * hash of a class object and therefore differs between runs of the same program; this port
   * seeds it with the hash of the name of the type instead, which is specified by
   * `String.hashCode` and so is the same in every run. The hash of an observation is therefore a
   * function of the observation alone, which is what the byte-stability properties of the test
   * suite rely on, and it still distinguishes an observation from a value of another type that
   * happens to hold the same two fields.
   *
   * Nothing outside [[equals]] is hashed, so the derived dates and the year fraction do not
   * appear here either.
   *
   * @return the hash code of the index and the fixing date
   */
  final override def hashCode: Int =
    (OvernightIndexObservation.HashSeed * 31 + index.hashCode) * 31 + fixingDate.hashCode

  /**
   * Returns the string form of this observation, such as
   * `OvernightIndexObservation[GBP-SONIA on 2016-02-22]`.
   *
   * This is the rendering of the bean being ported, reproduced exactly: the name of the type,
   * then the index and the fixing date - the two values that identify the observation and the
   * two that equality reads - in square brackets. The index renders as its unique name, the
   * fixing date in the ISO-8601 form `LocalDate` renders in.
   *
   * @return the string form of the observation
   */
  final override def toString: String =
    s"OvernightIndexObservation[$index on $fixingDate]"
}

/**
 * Builds observations of an Overnight index, and holds their instances of the typeclasses of
 * this port.
 *
 * [[of]] is the single construction route of the type, and everything else here serves it: the
 * private creation of the value it returns, the hashing and rendering of what it returns, and
 * the two halves of the JSON codec, whose decoding route is that same factory.
 *
 * Nothing in this companion is computed from the index family at initialisation. The factory
 * reaches an index only through the argument it is given, and the derived codecs reach the codec
 * of [[OvernightIndex]] through the implicit scope of the field type, which is a dependency in
 * one direction only - `Index.scala` does not name this type, and no value of this type exists
 * before the factory is called.
 */
object OvernightIndexObservation {

  /**
   * The seed of [[OvernightIndexObservation.hashCode]], the specified hash of the name of the
   * type.
   *
   * This replaces the `getClass().hashCode()` the bean being ported seeded its hash with, for
   * the reason given on that method: an identity hash is not reproducible between runs, and the
   * hash of a value of this port is a function of the value alone. It is a value rather than a
   * literal so that the substitution is stated once and is visible as what it is.
   */
  private val HashSeed: Int = "OvernightIndexObservation".hashCode

  //-------------------------------------------------------------------------
  /**
   * Obtains an observation of an Overnight index from the index and the fixing date.
   *
   * The publication, effective and maturity dates and the year fraction are all derived here,
   * which is what makes them consistent with the index for every observation that exists. The
   * derivation is that of the bean being ported, in the same order:
   *
   *  - the publication date is the fixing date shifted by the publication offset of the index;
   *  - the effective date is the fixing date shifted by the effective offset of the index;
   *  - the maturity date is one business day after the '''effective''' date - not after the
   *    fixing date, which differs from it by the effective offset and would give a maturity one
   *    or two days early on an index such as `THB-THOR`;
   *  - the year fraction is the day count of the index applied to the effective and maturity
   *    dates, taken as the index computes it and neither re-derived nor rounded here, so the
   *    figure is the one the original produced to the last bit.
   *
   * All three shifts count business days of the fixing calendar of the index, and that calendar
   * is named by identifier, so the identifier has to be resolved against the reference data
   * supplied - once for the three, since it is one calendar: this factory is
   * [[OvernightIndexObservation.resolve]] applied to a single date, and the resolution happens
   * there. Reference data that does not hold the calendar yields `Left(Failure.MissingData)`
   * naming the identifier, which is the single way this factory fails. Passing
   * `ReferenceData.standard` resolves every calendar built into this library.
   *
   * Where the reference data does hold the calendar the result is always a value: the shifts are
   * total once the calendar is in hand, and the year fraction is measured over two dates in
   * order - the maturity date is a business day after the effective date - by the day count of
   * an Overnight index, which needs no schedule information.
   *
   * {{{
   * OvernightIndexObservation.of(OvernightIndices.USD_FED_FUND, LocalDate.of(2016, 2, 22), ReferenceData.standard)
   * // Right: fixed 2016-02-22, published 2016-02-23, accruing 2016-02-22 to 2016-02-23
   *
   * OvernightIndexObservation.of(OvernightIndices.USD_FED_FUND, LocalDate.of(2016, 2, 22), ReferenceData.empty)
   * // Left(Failure.MissingData): the USNY calendar is not in the reference data supplied
   * }}}
   *
   * @param index  the index observed
   * @param fixingDate  the date the rate is fixed on; a date that is not a business day of the
   *   fixing calendar of the index is treated as the next one that is, which is the behaviour of
   *   the index calculations this delegates to
   * @param refData  the reference data to resolve the fixing calendar of the index against
   * @return the observation, or the failure describing the calendar that could not be resolved
   */
  def of(
      index: OvernightIndex,
      fixingDate: LocalDate,
      refData: ReferenceData): Either[Failure, OvernightIndexObservation] =
    resolve(index, refData).map(observe => observe(fixingDate))

  /**
   * Resolves an index against reference data once and answers with the observation of a fixing.
   *
   * A caller observing a series of fixings of one index - which is what an Overnight rate is
   * consumed as, a compounded or averaged run of daily fixings - should resolve the fixing
   * calendar of the index once rather than once per fixing. This does that: the calendar is
   * resolved here, and the function handed back consults no reference data at all, deriving the
   * publication, effective and maturity dates and the year fraction of each fixing from the
   * resolved calendar alone.
   *
   * The function produces, for any fixing date, the value [[OvernightIndexObservation.of]]
   * produces for the same index, fixing date and reference data - necessarily so, because that
   * factory is this function applied to one date. The derivation is the one documented on it,
   * performed by [[OvernightIndex.resolveWith]] in both cases.
   *
   * {{{
   * val observe = OvernightIndexObservation.resolve(OvernightIndices.USD_FED_FUND, ReferenceData.standard)
   * // Right(function): the USNY calendar was resolved once, here
   * observe.map(build => fixingDates.map(build))
   * // the observations of a run of fixings, resolving nothing further
   * }}}
   *
   * @param index  the index observed
   * @param refData  the reference data to resolve the fixing calendar of the index against
   * @return the observation of a fixing of this index, or the failure describing the calendar
   *   that could not be resolved
   */
  def resolve(
      index: OvernightIndex,
      refData: ReferenceData): Either[Failure, LocalDate => OvernightIndexObservation] =
    index.resolveWith(refData) {
      (fixingDate, publicationDate, effectiveDate, maturityDate, yearFraction) =>
        create(index, fixingDate, publicationDate, effectiveDate, maturityDate, yearFraction)
    }

  /**
   * Creates a value, which every route into the type funnels through.
   *
   * This is the only instantiation of the type and it is private to this companion, so [[of]] and
   * [[resolve]] are the only ways into it. The constructor of a `sealed abstract case class` is
   * reachable only from inside the file that declares it, and `new OvernightIndexObservation(...)
   * {}` - an anonymous subclass of the abstract case class - is how it is reached; that is what
   * leaves the type without a public `apply` or `copy` while keeping the `unapply` and the
   * accessors a case class provides.
   *
   * It is private rather than package-private, which the Ibor and the exchange-rate observations
   * both are, because both of those are constructed from the index side as well: their families
   * declare a `resolve` that the hierarchy being ported declared, and it builds observations.
   * An [[OvernightIndex]] declares no such operation, here or in the library being ported, so
   * every construction of this type happens in this companion - [[resolve]] derives the five
   * values from the resolved calendar and [[of]] is that function applied to one date - and
   * widening this door would let a caller supply dates that do not follow from the index.
   *
   * @param index  the index observed
   * @param fixingDate  the date the rate is fixed on
   * @param publicationDate  the publication date derived from the fixing date
   * @param effectiveDate  the effective date derived from the fixing date
   * @param maturityDate  the maturity date derived from the effective date
   * @param yearFraction  the year fraction derived from the effective and maturity dates
   * @return the observation holding those values
   */
  private def create(
      index: OvernightIndex,
      fixingDate: LocalDate,
      publicationDate: LocalDate,
      effectiveDate: LocalDate,
      maturityDate: LocalDate,
      yearFraction: Double): OvernightIndexObservation =
    new OvernightIndexObservation(
      index,
      fixingDate,
      publicationDate,
      effectiveDate,
      maturityDate,
      yearFraction) {}

  //-------------------------------------------------------------------------
  /**
   * The hashing and equality of observations.
   *
   * This is the only equality-bearing instance of the type: `Hash` extends `Eq`, so a separate
   * `Eq` would be a second answer to the same question. Both are taken from the
   * [[OvernightIndexObservation.equals]] and [[OvernightIndexObservation.hashCode]] of the type,
   * so this instance compares the index and the fixing date and ignores the derived values,
   * exactly as they do. A caller that means to compare the derived values as well - a
   * round-trip test is the case that matters, as the note on the type explains - must compare
   * them field by field rather than reach for this instance.
   *
   * No `Order` is offered, for the reason given on the type.
   *
   * @return the hashing and equality of observations
   */
  implicit val hash: Hash[OvernightIndexObservation] =
    Hash.fromUniversalHashCode[OvernightIndexObservation]

  /**
   * The rendering of observations as text.
   *
   * Renders what [[OvernightIndexObservation.toString]] renders, the
   * `OvernightIndexObservation[GBP-SONIA on 2016-02-22]` form, so the text of an observation is
   * the same however it reaches a message.
   *
   * @return the rendering of an observation
   */
  implicit val show: Show[OvernightIndexObservation] = Show.show(_.toString)

  //-------------------------------------------------------------------------
  // The codec of the double field, brought into scope for the derivation below and for nothing
  // else: the year fraction goes through the single policy this port has for a double, which
  // writes the values JSON cannot express as tagged strings. The import is what makes that
  // choice deliberate and local, as the codec support of `strata-collect` intends.
  import Codecs.implicits._

  /**
   * The field shape both codecs are derived from, which the decoder reads before validation.
   *
   * Decoding a validated type is two steps: read the fields, then hand them to the factory that
   * decides whether they describe a value. This product is the first step, and encoding is the
   * same two steps in reverse, so both codecs below derive from this one declaration and the
   * JSON shape of an observation is stated exactly once. It is private and never returned - the
   * only values of it that exist are the ones the two codecs build. Its field names are the JSON
   * keys and its field order is the order they are written in; both are those of
   * [[OvernightIndexObservation]] itself, which are those of the six properties of the bean
   * being ported, as AAP §0.8.2 requires.
   *
   * Deriving from [[OvernightIndexObservation]] directly is not possible - the constructor of a
   * validated type is not public, so there is no public shape to derive from - and writing the
   * fields out by hand instead would state the same contract a second time.
   *
   * @param index  the index, whose own codec carries it as its bare name, such as
   *   `"USD-FED-FUND"`
   * @param fixingDate  the fixing date, in the ISO-8601 form circe writes a `LocalDate` in
   * @param publicationDate  the publication date as supplied, checked against the index below
   * @param effectiveDate  the effective date as supplied, checked against the index below
   * @param maturityDate  the maturity date as supplied, checked against the index below
   * @param yearFraction  the year fraction as supplied, checked against the index below; a
   *   number, or one of the three tagged strings the double policy of this port accepts
   */
  private final case class Raw(
      index: OvernightIndex,
      fixingDate: LocalDate,
      publicationDate: LocalDate,
      effectiveDate: LocalDate,
      maturityDate: LocalDate,
      yearFraction: Double)

  /** The derived decoder of the raw field shape, used by the checking decoder below. */
  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /** The derived encoder of the raw field shape, used by the encoder below. */
  private val rawEncoder: Encoder.AsObject[Raw] = deriveEncoder[Raw]

  /**
   * The JSON encoding of observations.
   *
   * An observation is an object of six fields, the index as its name, the four dates as ISO-8601
   * text and the year fraction as a number:
   *
   * {{{
   * {"index":"USD-FED-FUND","fixingDate":"2016-02-22","publicationDate":"2016-02-23",
   *  "effectiveDate":"2016-02-22","maturityDate":"2016-02-23","yearFraction":0.002777777777777778}
   * }}}
   *
   * The four derived values are written even though the decoder re-derives them, and that is the
   * point of writing them: a reader of the document - a person, or a program that does not hold
   * this library - sees the dates the observation describes rather than only the two it was built
   * from, and the decoder can check that a document it is handed agrees with the index it names.
   *
   * Both halves of the codec are assembled by the same compile-time derivation over the same raw
   * shape, which is what keeps them from drifting apart: the field names, their order and their
   * element codecs are stated once, in [[Raw]], and the encoder reaches them by mapping an
   * observation onto that shape. The result is wrapped so that a field holding no value would be
   * omitted, which is the policy every product of this port follows - this type has no optional
   * field, so the wrapping changes nothing about its output and exists so that the policy holds
   * without exception.
   *
   * @return the JSON encoding of an observation
   */
  implicit val encoder: Encoder[OvernightIndexObservation] =
    Codecs.dropNulls(
      rawEncoder.contramap[OvernightIndexObservation](observation =>
        Raw(
          observation.index,
          observation.fixingDate,
          observation.publicationDate,
          observation.effectiveDate,
          observation.maturityDate,
          observation.yearFraction)))

  /**
   * The JSON decoding of observations.
   *
   * This is the inverse of the encoding above, and it builds the value through the same factory a
   * caller's arguments go through: the index and the fixing date are read from the payload and
   * handed to [[of]] against [[ReferenceData.standard]], so the four derived values of the
   * result are derived rather than believed. A payload naming an index whose fixing calendar the
   * standard reference data cannot resolve is a decoding failure, carrying the message the
   * factory gave.
   *
   * The derived values the payload carries are then '''checked''' against the derived values, and
   * a disagreement is a decoding failure naming every field that disagrees. This check is not
   * defensive decoration: the equality of this type ignores those four fields, so a payload that
   * contradicts the index it names - a publication date moved by a day, a year fraction from a
   * different day count - would otherwise decode into a value that compares equal to the one that
   * was encoded while describing something else entirely. Refusing it is what makes the round
   * trip of this type mean what it appears to mean. The year fraction is compared by bit pattern,
   * so a figure that differs in its last bit is a disagreement and the tolerance of the parity
   * harness has no part in it.
   *
   * The two steps divide the work as the codec support of this port intends, and the division is
   * visible in what a bad payload is told. Reading a field is the codec's business: a year
   * fraction written as one of the three tagged strings - `"NaN"`, `"Infinity"`, `"-Infinity"` -
   * is read as that double under the single double policy of this port, rather than refused for
   * not being a number. Deciding whether the value read belongs to an observation is this type's
   * business: no non-finite figure can equal the year fraction an Overnight day count produces
   * over one business day, so such a payload is then refused by the check below, and is reported
   * as a year fraction that disagrees with the index.
   *
   * @return the JSON decoding of an observation
   */
  implicit val decoder: Decoder[OvernightIndexObservation] =
    Codecs.checkedDecoder[Raw, OvernightIndexObservation](raw =>
      of(raw.index, raw.fixingDate, ReferenceData.standard)
        .flatMap(observation => agreeing(raw, observation)))(rawDecoder)

  /**
   * Checks the derived values a payload carries against the ones the index implies.
   *
   * Every disagreement is reported rather than only the first, so a document written against a
   * different set of holiday calendars is diagnosed in one reading instead of one field per
   * attempt. The failure names the index and the fixing date in its attributes as well as in its
   * message, so a caller aggregating failures can group them without parsing text.
   *
   * @param raw  the fields the payload carried
   * @param observation  the observation derived from the index and fixing date of that payload
   * @return the observation when the payload agrees with it, or the failure naming every field
   *   that does not
   */
  private def agreeing(
      raw: Raw,
      observation: OvernightIndexObservation): Either[Failure, OvernightIndexObservation] = {
    val disagreements: List[String] =
      List(
        dateDisagreement("publicationDate", raw.publicationDate, observation.publicationDate),
        dateDisagreement("effectiveDate", raw.effectiveDate, observation.effectiveDate),
        dateDisagreement("maturityDate", raw.maturityDate, observation.maturityDate),
        Option.when(java.lang.Double.compare(raw.yearFraction, observation.yearFraction) != 0)(
          s"yearFraction is ${raw.yearFraction} where the index implies ${observation.yearFraction}")).flatten
    if (disagreements.isEmpty) {
      Right(observation)
    } else {
      Left(
        Failure.Invalid(
          s"Observation of ${raw.index.name} fixing on ${raw.fixingDate} disagrees with the index: " +
            disagreements.mkString(", "),
          SortedMap("index" -> raw.index.name, "fixingDate" -> raw.fixingDate.toString)))
    }
  }

  /**
   * Describes a supplied date that is not the date the index implies.
   *
   * @param field  the name of the field, as the JSON document spells it
   * @param supplied  the date the payload carried
   * @param derived  the date the index implies
   * @return the description of the disagreement, or nothing when the two dates are the same
   */
  private def dateDisagreement(
      field: String,
      supplied: LocalDate,
      derived: LocalDate): Option[String] =
    Option.when(supplied != derived)(s"$field is $supplied where the index implies $derived")
}

//-------------------------------------------------------------------------
/**
 * Information about a single observation of a price index.
 *
 * Observing a price index requires knowledge of the index and the month it is observed for, and
 * this type is that pair: the index, such as `GB-RPI`, together with the month whose published
 * level is wanted, such as `2024-01`. A price index publishes a level once a month rather than a
 * rate once a day, so a month is the whole of what identifies the point of fixing - there is no
 * fixing date, no publication offset and no deposit period to carry.
 *
 * {{{
 * val observation = PriceIndexObservation.of(PriceIndices.GB_RPI, YearMonth.of(2024, 1))
 * observation.index       // GB-RPI
 * observation.currency    // GBP, taken from the index
 * observation.toString    // PriceIndexObservation[GB-RPI on 2024-01]
 * }}}
 *
 * ===Construction is total===
 *
 * This is the one observation of this package that needs no reference data and derives no date.
 * The three other kinds - [[IborIndexObservation]], [[OvernightIndexObservation]] and
 * [[FxIndexObservation]] - take a fixing date and compute the dates that follow from it against
 * the fixing calendar of their index, which is a computation that can fail and which their
 * public surface therefore routes through a validating factory. Here there is nothing to
 * compute: a month is a month, whatever index it is paired with, and the only check the Java
 * original performed was that neither field was absent, a state the types of this port cannot
 * express.
 *
 * Construction is therefore unrestricted, and this type is the `[T]` (total) construction kind
 * of the port: the primary constructor, the generated `apply` and the generated `copy` are all
 * public, and [[PriceIndexObservation.of]] exists alongside them purely so that ported call
 * sites read as they did before. Both fields keep the names, and the order, that the Java bean
 * declared - `index`, then `fixingMonth` - and those names are also the keys of the JSON form.
 *
 * ===Equality, hashing and rendering===
 *
 * Equality and hashing are the ones the case class generates, which compare and hash both
 * fields. That is exactly what the hand-written equality of the Java bean did, so nothing is
 * overridden here. (The documentation on those Java methods says that "the maturity date is
 * ignored"; it is copy-paste residue from a sibling bean, as this type has no maturity date and
 * the code compares the two fields it has.) Equality of the index is equality of its name,
 * since the index family compares by name, and equality of a month is equality of its year and
 * month, so two observations are equal exactly when they name the same index and the same month.
 *
 * The rendering is the hand-written form of the Java original, `PriceIndexObservation[<index> on
 * <month>]`, kept to the character so that ported code, its logs and its test expectations read
 * as they did before. The index renders as its name because the index family renders as its
 * name, and the month renders in ISO-8601 form, `2024-01`.
 *
 * ===Immutability and thread safety===
 *
 * Both fields are immutable values - an index is a member of a closed family created once, and
 * `java.time.YearMonth` is immutable - so an instance is immutable, safe to share between
 * threads and safe to use as a key of a map or a member of a set. It reports the same index and
 * the same month for its whole lifetime, which is what the abstraction it implements expects of
 * an observation.
 *
 * ===What the port drops===
 *
 * The Joda-Beans machinery of the original - the private builder, the meta-bean, the property
 * map and the reflective property access - has no counterpart here: `copy` replaces the builder,
 * and the compile-time JSON instances of the companion replace the bean serialization. Java
 * serialization is not supported by any type of this port, so `Serializable` is dropped with it.
 *
 * @param index  the price index to be observed, whose level for the month is wanted
 * @param fixingMonth  the month the index is observed for
 *
 * @see [[IndexObservation]] for the abstraction over the four kinds of observation
 * @see [[PriceIndex]] for the family of indices that can be observed this way
 * @see [[PriceIndices]] for the published indices by name
 */
final case class PriceIndexObservation(index: PriceIndex, fixingMonth: YearMonth)
    extends IndexObservation {

  /**
   * Gets the currency of the index.
   *
   * The level a price index publishes is quoted in one currency, which is a field of the index
   * itself, so this is a delegation and never absent: an observation is of an index, and every
   * index states its currency. Note that the currency of the index is not always the currency of
   * the region whose price level it measures.
   *
   * @return the currency of the index observed
   */
  def currency: Currency = index.currency

  /**
   * Returns this observation as text.
   *
   * The form is the one the Java original produced, the index and the month between brackets, as
   * in `PriceIndexObservation[GB-RPI on 2024-01]`. It is what the `Show` instance of the
   * companion renders.
   *
   * @return the rendering of this observation
   */
  override def toString: String = s"PriceIndexObservation[$index on $fixingMonth]"
}

/**
 * The factory, typeclass instances and JSON form of [[PriceIndexObservation]].
 *
 * Two typeclass instances are published, and exactly two: a `Hash`, which is the single
 * equality-bearing instance of the type - `Hash` extends `Eq`, so declaring an `Eq` as well
 * would leave two instances that could disagree and one of them ambiguous - and a `Show`. There
 * is deliberately no `Order`: the Java type is not `Comparable`, and an index paired with a
 * month has no ordering worth inventing, since ordering by month and ordering by index are both
 * defensible and neither is what a caller would be entitled to assume.
 */
object PriceIndexObservation {

  /**
   * Obtains an instance from an index and a fixing month.
   *
   * This is the factory the Java original published, kept under its own name so that ported call
   * sites read unchanged. It is total, and identical to the generated `apply`: there is nothing
   * about an index and a month that this factory could reject or derive, which is why this type
   * keeps a public constructor where the other observations of this package do not.
   *
   * @param index  the price index to be observed
   * @param fixingMonth  the month the index is observed for
   * @return the observation of that index for that month
   */
  def of(index: PriceIndex, fixingMonth: YearMonth): PriceIndexObservation =
    PriceIndexObservation(index, fixingMonth)

  //-------------------------------------------------------------------------
  /**
   * The hashing and equality of price index observations.
   *
   * Taken from the `equals` and `hashCode` the case class generates, which compare and hash both
   * fields, so the instance and the type can never disagree. This is the type's only
   * equality-bearing instance, and `Eq[PriceIndexObservation]` is obtained from it by subtyping.
   *
   * @return the hashing of price index observations
   */
  implicit val hash: Hash[PriceIndexObservation] = Hash.fromUniversalHashCode[PriceIndexObservation]

  /**
   * The rendering of a price index observation as text.
   *
   * Renders what `toString` renders, which is the form of the Java original.
   *
   * @return the rendering of a price index observation
   */
  implicit val show: Show[PriceIndexObservation] = Show.show(_.toString)

  //-------------------------------------------------------------------------
  /**
   * The JSON encoding of a price index observation.
   *
   * The encoding is derived when this file is compiled, so no part of it inspects a class while
   * the program runs. An instance encodes as an object holding its two fields under the names
   * the Java bean declared, the index as its name and the month in ISO-8601 form:
   *
   * {{{
   * {"index":"GB-RPI","fixingMonth":"2024-01"}
   * }}}
   *
   * The index is written by the codec of its own family, which is a bare name string, and the
   * month by the codec the JSON library publishes for a year and month. Neither field is
   * optional, so the wrapper that drops absent fields from the output has nothing to drop here;
   * it is applied regardless because every derived product encoder of this port is wrapped the
   * same way, and a uniform policy that is a no-op for this shape is worth more than an
   * exception to it.
   *
   * @return the JSON encoding of a price index observation
   */
  implicit val encoder: Encoder[PriceIndexObservation] =
    Codecs.dropNulls(deriveEncoder[PriceIndexObservation])

  /**
   * The JSON decoding of a price index observation.
   *
   * This is the inverse of the encoding above and is likewise derived at compile time. Both
   * fields have to be present, and each is read by the same instance that wrote it, so an
   * encoded observation decodes back to an observation equal to it. The plain derived decoder is
   * the whole of what is needed: construction cannot fail, so beyond the shape of the payload
   * there is nothing to validate, and a payload of the right shape always yields a value. Text
   * that names no published index is rejected by the index codec, as a decoding failure naming
   * the text that named nothing.
   *
   * @return the JSON decoding of a price index observation
   */
  implicit val decoder: Decoder[PriceIndexObservation] = deriveDecoder[PriceIndexObservation]
}

//-------------------------------------------------------------------------
/**
 * Information about a single observation of an FX index.
 *
 * Observing an FX index requires knowledge of the index, the fixing date and the maturity date.
 * The index says which exchange rate was observed, the fixing date says when the rate was quoted,
 * and the maturity date - also known as the value date - is the date on which a conversion at
 * that rate settles. For example, an observation of `EUR/USD-ECB` fixing on 17 February 2016
 * settles on 19 February 2016, two business days later.
 *
 * The maturity date is not an independent third piece of information: it follows from the index
 * and the fixing date, through [[FxIndex.calculateMaturityFromFixing]]. It is held on the
 * observation rather than recomputed on demand because a caller that builds many observations of
 * one index should resolve that index's calendars once, not once per fixing; see [[create]] and
 * the note on the two ways into the type below.
 *
 * ===Creating an observation===
 *
 * The type is a validated value, in the sense the port gives that term: its constructor is not
 * public, it has no `apply` and no `copy`, and the only way to obtain one from outside this
 * package is [[FxIndexObservation.of]], which derives the maturity date rather than accepting
 * one. That is what makes the two dates of an observation unable to disagree - an observation
 * whose maturity date does not follow from its fixing date cannot be built, and so cannot be
 * decoded either. Pattern matching is unaffected: `unapply` is retained, so
 * `case FxIndexObservation(index, fixingDate, maturityDate) => ...` reads the three fields of a
 * value that was built through the factory.
 *
 * {{{
 * val observation = FxIndexObservation.of(
 *   FxIndices.GBP_USD_WM,
 *   LocalDate.of(2016, 2, 22),
 *   ReferenceData.standard)
 *
 * // Right(FxIndexObservation[GBP/USD-WM on 2016-02-22])
 * observation
 *
 * // Right(2016-02-24), the fixing settling two business days later
 * observation.map(_.maturityDate)
 * }}}
 *
 * Construction reports a failure rather than throwing, because it resolves the fixing calendar of
 * the index against the reference data supplied and that resolution can fail: reference data that
 * holds no calendar for the index answers with a failure, and eight of the sixteen published FX
 * indices name calendars - the Chilean, Chinese, Colombian, Indian, Korean, Singaporean and
 * Taiwanese ones - for which this library publishes no holiday data, so an observation of those
 * eight fails to build even against [[ReferenceData.standard]]. The dollar/baht index is among
 * them because its calendar combines the Thai calendar, which is published, with the Singaporean
 * one, which is not. That is the behaviour of the library being ported, in which the same
 * calendars are equally absent, rather than a limitation introduced here.
 *
 * ===The currency pair, not a currency===
 *
 * Unlike the observations of the other three index families, this one reports a
 * [[currencyPair]] rather than a single currency, because an exchange rate is a relation between
 * two currencies rather than a figure quoted in one. [[IndexObservation]] therefore declares no
 * currency accessor at all, and each observation declares the accessor that is honest for it.
 *
 * ===Equality deliberately ignores the maturity date===
 *
 * Two observations are equal when their index and fixing date are equal; the maturity date takes
 * no part in equality or in hashing. This is the documented behaviour of the bean being ported
 * and it is consistent rather than lax: the maturity date is a function of the other two fields,
 * so two observations that agree on index and fixing date agree on maturity date as well - unless
 * they were built against reference data that disagreed about a calendar, in which case the two
 * observations are still observations of the same fixing.
 *
 * One consequence matters to whoever tests this type. A round-trip property stated as
 * `decode(encode(observation)) == observation` is satisfied without the maturity date ever being
 * written or read, so `json/JsonRoundTripSpec` must compare `maturityDate` explicitly for this
 * type rather than relying on equality; the decoder's own cross-check of that field (see
 * [[FxIndexObservation.decoder]]) is likewise invisible to an equality-based assertion.
 *
 * ===Immutability and threading===
 *
 * An observation is an immutable value holding an index and two dates, all three of them
 * immutable, so it is safe to share between threads.
 *
 * @see [[FxIndex]] for the indices that can be observed and the derivation of the maturity date
 * @see [[IndexObservation]] for the abstraction over the four kinds of observation
 * @param index  the FX index, from which the rate will be queried
 * @param fixingDate  the date of the index fixing, an adjusted date that is a business day of the
 *   fixing calendar of the index
 * @param maturityDate  the date of the transfer implied by the fixing date, which is always the
 *   date [[FxIndex.calculateMaturityFromFixing]] derives from the fixing date
 */
sealed abstract case class FxIndexObservation private (
    index: FxIndex,
    fixingDate: LocalDate,
    maturityDate: LocalDate)
    extends IndexObservation {

  /**
   * Gets the currency pair of the FX index.
   *
   * This is the pair of currencies the observed rate is quoted for, taken from the index, and it
   * is the accessor this observation offers in place of the single currency the other
   * observations report.
   *
   * @return the currency pair of the index
   */
  def currencyPair: CurrencyPair = index.currencyPair

  //-------------------------------------------------------------------------
  /**
   * Checks if this observation equals another observation, comparing the index and fixing date.
   *
   * The maturity date is ignored, which is the documented behaviour of the bean being ported; see
   * the note on equality on this type. The all-field equality a case class would otherwise
   * synthesize is therefore replaced here, and the comparison is a type test rather than a
   * comparison of runtime classes: every instance is an anonymous subclass of this class - that
   * being how a validated type of this port is instantiated - so the runtime class is an
   * implementation detail no caller should be made to depend on.
   *
   * @param obj  the other value to compare to
   * @return true when the other value is an observation of the same index and fixing date
   */
  override def equals(obj: Any): Boolean = obj match {
    case other: FxIndexObservation =>
      (this eq other) || (index == other.index && fixingDate == other.fixingDate)
    case _ => false
  }

  /**
   * Returns a hash code based on the index and fixing date.
   *
   * The maturity date is ignored, so that hashing agrees with equality. The arithmetic is that of
   * the bean being ported - a seed combined with the hash of each field in turn - with one
   * difference of mechanism: the seed is derived from the name of the type rather than from the
   * identity hash of a class object, so that it is the same in every run of the program and
   * cannot depend on which anonymous subclass an instance happens to have. Neither property is
   * observable through the contract of `hashCode`, which asks only that equal values hash equally
   * within one run.
   *
   * @return the hash code
   */
  override def hashCode: Int =
    (FxIndexObservation.HashSeed * 31 + index.hashCode) * 31 + fixingDate.hashCode

  /**
   * Returns a string describing the observation.
   *
   * The form is that of the bean being ported, `FxIndexObservation[GBP/USD-WM on 2016-02-22]`,
   * naming the index and the fixing date; the maturity date does not appear, being derived from
   * the two values that do.
   *
   * @return the descriptive string
   */
  override def toString: String = s"FxIndexObservation[$index on $fixingDate]"
}

/**
 * Holds the creation of an observation of an FX index, its typeclass instances and its JSON form.
 *
 * Two public operations lead into the type and both derive the maturity date rather than accepting
 * it from a caller. [[of]] observes one fixing and resolves the calendars of the index on every
 * call. [[resolve]] observes a series: it resolves the fixing calendar and the maturity offset
 * once and hands back a function over fixing dates, which is what a caller converting a run of
 * dates needs and what the `resolve` of the index being ported existed for; [[FxIndex.resolve]]
 * is the same operation reached from the index and calls this one.
 *
 * [[create]] is the single instantiation both of them funnel through. It accepts a maturity date
 * already computed, so it is package-private rather than public: a caller who did not derive the
 * maturity date from the index and the fixing date must not be able to pair two dates that do not
 * belong together, and the two operations above are the only derivations of it that exist.
 */
object FxIndexObservation {

  /**
   * The seed of the hash code of an observation.
   *
   * Held here rather than computed per instance so that it is a constant of the type. Its value
   * carries no meaning beyond being fixed; see the note on [[FxIndexObservation.hashCode]].
   */
  private val HashSeed: Int = "FxIndexObservation".hashCode

  //-------------------------------------------------------------------------
  /**
   * Creates an observation from an index and a fixing date.
   *
   * The reference data is used to find the maturity date from the fixing date, through
   * [[FxIndex.calculateMaturityFromFixing]]: the fixing date is moved onto the next fixing date
   * of the index if it is not one already, and the maturity offset of the index is applied to
   * that. A fixing date that is not a business day of the index is therefore accepted, exactly as
   * the library being ported accepts it, and the observation keeps the date asked about while its
   * maturity date follows from the fixing date the index would actually use.
   *
   * The result is a failure when the reference data cannot resolve the fixing calendar of the
   * index or the calendar its maturity offset counts business days in. Both are ordinary
   * outcomes: reference data holding no calendars resolves nothing, and several published indices
   * name calendars this library publishes no data for.
   *
   * @param index  the index
   * @param fixingDate  the fixing date
   * @param refData  the reference data to use when resolving holiday calendars
   * @return the observation, or the failure describing the calendar that could not be resolved
   */
  def of(
      index: FxIndex,
      fixingDate: LocalDate,
      refData: ReferenceData): Either[Failure, FxIndexObservation] =

    index
      .calculateMaturityFromFixing(fixingDate, refData)
      .map(maturityDate => create(index, fixingDate, maturityDate))

  /**
   * Resolves an index against reference data once and answers with the observation of a fixing.
   *
   * A caller converting a run of dates at one index - a series of forward points, or the fixings
   * of a set of trades on the same rate - should not resolve a holiday calendar per date. This
   * resolves the fixing calendar and the maturity offset of the index up front and hands back a
   * function that consults no reference data at all: given a fixing date it moves the date onto
   * the next fixing date of the index, applies the maturity offset to that, and builds the
   * observation from the date the caller asked about and the settlement date it implies.
   *
   * The function produces, for any fixing date, the value [[FxIndexObservation.of]] produces for
   * the same index, fixing date and reference data; both derive the maturity date through the
   * index, and the only difference is when the calendars are resolved. This is the operation the
   * `resolve` of the index being ported existed for, and [[FxIndex.resolve]] is it reached from
   * the index, calling this method.
   *
   * {{{
   * val observe = FxIndexObservation.resolve(FxIndices.EUR_USD_ECB, ReferenceData.standard)
   * // Right(function): the EUTA+USNY calendars were resolved once, here
   * observe.map(build => fixingDates.map(build))
   * // the observations of a run of fixings, resolving nothing further
   * }}}
   *
   * @param index  the index
   * @param refData  the reference data to use when resolving holiday calendars
   * @return the observation of a fixing of this index, or the failure describing the calendar
   *   that could not be resolved
   */
  def resolve(
      index: FxIndex,
      refData: ReferenceData): Either[Failure, LocalDate => FxIndexObservation] =
    index.resolveWith(refData) { (fixingDate, maturityDate) =>
      create(index, fixingDate, maturityDate)
    }

  /**
   * Creates an observation from an index, a fixing date and the maturity date they imply.
   *
   * This is the only instantiation of the type, which every route into it funnels through, and it
   * performs no derivation and no check of its own: the maturity date is taken as given, so the
   * caller is responsible for having derived it from the index and the fixing date. That is why
   * the method is visible only within this package - the one caller that legitimately holds an
   * already-derived maturity date is the resolution of an index described on this object, and
   * widening either that route or [[of]] would let a caller pair two dates that do not belong
   * together.
   *
   * The constructor of a `sealed abstract case class` is reachable only from inside the file that
   * declares it, and `new FxIndexObservation(...) {}` - an anonymous subclass of the abstract case
   * class - is how it is reached; that is what leaves the type without a public `apply` or `copy`
   * while keeping the `unapply` a case class provides. Every instance of the type is created
   * here, so every instance has the same runtime class.
   *
   * @param index  the index
   * @param fixingDate  the fixing date
   * @param maturityDate  the maturity date already derived from the index and the fixing date
   * @return the observation
   */
  private[index] def create(
      index: FxIndex,
      fixingDate: LocalDate,
      maturityDate: LocalDate): FxIndexObservation =

    new FxIndexObservation(index, fixingDate, maturityDate) {}

  //-------------------------------------------------------------------------
  /**
   * The hashing and equality of observations.
   *
   * Taken from the `equals` and `hashCode` of the type, which compare the index and the fixing
   * date and ignore the maturity date; see the note on equality on [[FxIndexObservation]]. No
   * field holds a `Double`, so there is no bit-pattern comparison to arrange.
   *
   * This is the type's only equality-bearing instance, and `Eq[FxIndexObservation]` is obtained
   * from it by subtyping rather than declared separately. There is no `Order`: the bean being
   * ported is not `Comparable`, and an ordering over an index and two dates would be this port's
   * invention - a caller wanting observations in order of fixing sorts them by `fixingDate`, which
   * says which order was meant.
   *
   * @return the hashing of observations
   */
  implicit val hash: Hash[FxIndexObservation] = Hash.fromUniversalHashCode[FxIndexObservation]

  /**
   * The rendering of observations as text.
   *
   * Renders what `toString` renders, which is the grammar of the bean being ported, so the two
   * ways of putting an observation into a message agree.
   *
   * @return the rendering of an observation
   */
  implicit val show: Show[FxIndexObservation] = Show.show(_.toString)

  //-------------------------------------------------------------------------
  /**
   * The field shape both codecs are derived from, which the decoder reads before validation.
   *
   * Decoding a validated type is two steps: read the fields, then hand them to the factory that
   * decides whether they describe a value. This product is the first step, and encoding is the
   * same two steps in reverse, so both codecs below derive from this one declaration and the JSON
   * shape of an observation is stated exactly once. It is private and never returned - the only
   * values of it that exist are the ones the two codecs build. Its field names are the JSON keys,
   * and they are the names of the three fields of [[FxIndexObservation]] itself, which are the
   * names of the three properties of the bean being ported, in their declaration order.
   *
   * @param index  the index, carried as its canonical name, such as `EUR/USD-ECB`
   * @param fixingDate  the fixing date, carried as its ISO text form
   * @param maturityDate  the maturity date, carried as its ISO text form
   */
  private final case class Raw(
      index: FxIndex,
      fixingDate: LocalDate,
      maturityDate: LocalDate)

  /** The derived decoder of the raw field shape, used by the checking decoder below. */
  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /** The derived encoder of the raw field shape, used by the encoder below. */
  private val rawEncoder: Encoder[Raw] = deriveEncoder[Raw]

  /**
   * Describes a payload whose maturity date is not the one the index derives.
   *
   * @param index  the index named by the payload
   * @param fixingDate  the fixing date named by the payload
   * @param declared  the maturity date the payload declared
   * @param derived  the maturity date the index derives from the fixing date
   * @return the failure describing the disagreement
   */
  private def maturityMismatch(
      index: FxIndex,
      fixingDate: LocalDate,
      declared: LocalDate,
      derived: LocalDate): Failure =

    Failure.Invalid(
      s"FxIndexObservation of '$index' fixing on $fixingDate matures on $derived, " +
        s"but the value read declared $declared")

  /**
   * The JSON encoding of observations.
   *
   * An observation is an object of three fields, the index as its canonical name and the two
   * dates as their ISO text forms:
   *
   * {{{
   * {"index":"GBP/USD-WM","fixingDate":"2016-02-22","maturityDate":"2016-02-24"}
   * }}}
   *
   * The maturity date is written even though it is derived from the other two fields, because it
   * is a field of the value and a reader of the document should not have to derive it; the decoder
   * checks it rather than trusting it.
   *
   * Both halves of the codec are assembled by the same compile-time derivation over the same raw
   * shape, which is what keeps them from drifting apart: the field names, their order and their
   * element codecs are stated once, in [[Raw]], and the encoder reaches them by mapping an
   * observation onto that shape. Deriving from [[FxIndexObservation]] itself is not possible - the
   * constructor of a validated type is not public, so there is no public shape to derive from -
   * and writing the fields out by hand instead would state the same contract a second time.
   *
   * No part of the encoding inspects a class while the program runs, and two observations that are
   * equal encode to identical bytes, their maturity dates being equal whenever they were built
   * against the same reference data. The result is wrapped so that a field holding no value would
   * be omitted, which is the policy every product of this port follows - this type has no optional
   * field, so the wrapping changes nothing about its output and exists so that the policy holds
   * without exception.
   *
   * @return the JSON encoding of an observation
   */
  implicit val encoder: Encoder[FxIndexObservation] =
    Codecs.dropNulls(rawEncoder.contramap[FxIndexObservation] { value =>
      Raw(value.index, value.fixingDate, value.maturityDate)
    })

  /**
   * The JSON decoding of observations.
   *
   * This is the inverse of the encoding above, and it decides whether the fields describe an
   * observation exactly as a caller's arguments are decided: the payload is read into the raw
   * shape and its index and fixing date are handed to [[of]] against [[ReferenceData.standard]],
   * so the maturity date of the result is derived here rather than taken from the document. The
   * maturity date the document declared is then compared with the derived one, and a document that
   * declares a different date is a decoding failure naming both. That check is what stops a
   * tampered or stale payload from becoming a value whose dates disagree, and it is needed
   * precisely because the equality of this type would not notice such a value.
   *
   * Two payloads are rejected for reasons that belong to the data rather than to the syntax, and
   * both are reported as decoding failures carrying the reason: one that names an index whose
   * calendars [[ReferenceData.standard]] cannot resolve - the eight indices naming the Chilean,
   * Chinese, Colombian, Indian, Korean, Singaporean and Taiwanese calendars, for which this
   * library publishes no holiday data - and one that declares a maturity date the index does not
   * derive. All three fields have to be present.
   *
   * Decoding against reference data other than the standard set is not offered, because a decoder
   * takes no argument beyond the document. A caller holding its own calendars builds observations
   * through [[of]] with that reference data, which is the route this type is designed around.
   *
   * @return the JSON decoding of an observation
   */
  implicit val decoder: Decoder[FxIndexObservation] =
    Codecs.checkedDecoder[Raw, FxIndexObservation] { raw =>
      of(raw.index, raw.fixingDate, ReferenceData.standard).flatMap { observation =>
        if (observation.maturityDate == raw.maturityDate) {
          Right(observation)
        } else {
          Left(maturityMismatch(raw.index, raw.fixingDate, raw.maturityDate, observation.maturityDate))
        }
      }
    }(rawDecoder)
}
