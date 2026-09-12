/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import java.time.LocalDate

import scala.collection.immutable.SortedMap

import cats.Hash
import cats.Show

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

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
   * Each of the first three shifts counts business days of the fixing calendar of the index, and
   * that calendar is named by identifier, so each has to resolve the identifier against the
   * reference data supplied. Reference data that does not hold the calendar yields
   * `Left(Failure.MissingData)` naming the identifier, which is the single way this factory
   * fails; the three shifts read the same calendar, so a failure is reported once, by the first
   * of them. Passing `ReferenceData.standard` resolves every calendar built into this library.
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
    for {
      publicationDate <- index.calculatePublicationFromFixing(fixingDate, refData)
      effectiveDate <- index.calculateEffectiveFromFixing(fixingDate, refData)
      maturityDate <- index.calculateMaturityFromEffective(effectiveDate, refData)
    } yield create(
      index,
      fixingDate,
      publicationDate,
      effectiveDate,
      maturityDate,
      index.dayCount.yearFraction(effectiveDate, maturityDate))

  /**
   * Creates a value, which every route into the type funnels through.
   *
   * This is the only instantiation of the type and it is private, so [[of]] is the only way into
   * it from outside this file. The constructor of a `sealed abstract case class` is reachable
   * only from inside the file that declares it, and `new OvernightIndexObservation(...) {}` - an
   * anonymous subclass of the abstract case class - is how it is reached; that is what leaves
   * the type without a public `apply` or `copy` while keeping the `unapply` and the accessors a
   * case class provides.
   *
   * Unlike the Ibor and FX observations of this package, this type publishes no second,
   * package-private creation route. Those two exist because the index families they observe can
   * be resolved against reference data and build their observations while doing so; an
   * [[OvernightIndex]] has no such operation - the bean being ported has none either - so there
   * is nothing in `Index.scala` that constructs one of these, and a second door would widen the
   * surface of the type for no caller.
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
