/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import java.time.LocalDate

import cats.Hash
import cats.Show

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

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
    extends IndexObservation with NoJavaSerialization {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means - holding
  // dates and a year fraction that follow from no index - can be stopped is here. The single
  // implementation is the companion's hidden `Impl`.
  JvmClosure.requireSoleImplementation(this, classOf[IborIndexObservation.Impl])

  // The invariant of this type, stated over the fields the instance actually holds rather than
  // over the arguments a factory was given, because the class file of the implementation carries
  // a public constructor whatever the source asked for: a class compiled outside this library can
  // call it directly, and identity alone would then admit an observation whose dates and year
  // fraction follow from nothing.
  //
  // What can be stated here is what the derivation of `of` implies without reference data, which
  // a constructor is never handed: the index itself is a published member of its family - its own
  // constructor established that - so its offsets, its tenor and its day count are the true ones,
  // and the three statements below hold the stored dates to them. The one equality the derivation
  // turns on, which business day each calendar admits, is the one thing no check without that
  // reference data can reproduce, so the dates are bounded by the offsets rather than recomputed;
  // the year fraction, which needs no calendar at all, is required to be exactly the value the
  // day count of the index measures over the stored period, which is the computation `of`
  // performs. The bounds are stated in the order they are safe to evaluate in: the year fraction
  // is measured only once the dates are known to be in order.
  JvmClosure.requireInvariant(
    "its effective date is no earlier than its fixing date advanced by the days of the effective " +
      "offset of its index",
    index.effectiveDateOffset.days < 0 ||
      !effectiveDate.isBefore(fixingDate.plusDays(index.effectiveDateOffset.days.toLong)))
  JvmClosure.requireInvariant(
    "its maturity date is the tenor of its index beyond its effective date, up to the month-end " +
      "and business day adjustment the maturity offset of the index applies",
    !maturityDate.isBefore(effectiveDate) &&
      // The two conventions of the maturity offset move the added date by less than two months
      // between them: an addition convention lands it on the end of its month, and a business
      // day convention then moves it off a holiday run or back inside that month.
      math.abs(
        maturityDate.toEpochDay - effectiveDate.plus(index.tenor.period).toEpochDay) <= 62L)
  JvmClosure.requireInvariant(
    "its year fraction is the one the day count of its index measures between its effective and " +
      "maturity dates",
    yearFraction == index.dayCount.yearFraction(effectiveDate, maturityDate))

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
   * `new Impl(...)` - the hidden implementation class declared below - is how
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
    new Impl(index, fixingDate, effectiveDate, maturityDate, yearFraction)

  /**
   * The one implementation of an Ibor index observation.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it. It is declared here rather than written as an anonymous subclass at the instantiation
   * site for two reasons, both about what the class file says: a private member class is one a
   * compiler in another language refuses to name, where an anonymous class is public and can be
   * instantiated directly by such a caller; and a named class can be compared against, which is
   * what lets [[IborIndexObservation]] refuse in its own constructor to be any other
   * implementation.
   *
   * Every parameter is passed straight to the case class, which declares them as its fields, and
   * the values reaching them have already been derived from the index by [[create]].
   *
   * @param index  the index
   * @param fixingDate  the fixing date
   * @param effectiveDate  the effective date derived from the fixing date
   * @param maturityDate  the maturity date derived from the effective date
   * @param yearFraction  the year fraction derived on the day count of the index
   */
  private final class Impl(
      index: IborIndex,
      fixingDate: LocalDate,
      effectiveDate: LocalDate,
      maturityDate: LocalDate,
      yearFraction: Double)
      extends IborIndexObservation(index, fixingDate, effectiveDate, maturityDate, yearFraction)

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
      extends NoJavaSerialization

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
