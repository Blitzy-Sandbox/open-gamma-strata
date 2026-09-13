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
import com.opengamma.strata.basics.currency.CurrencyPair
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

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
    extends IndexObservation with NoJavaSerialization {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means - pairing a
  // fixing date with a maturity date the index does not derive - can be stopped is here. The
  // single implementation is the companion's hidden `Impl`.
  JvmClosure.requireSoleImplementation(this, classOf[FxIndexObservation.Impl])

  // The invariant of this type, stated over the two dates the instance actually holds, because the
  // implementation class carries a public constructor whatever the source asked for and identity
  // alone would then admit a fixing date paired with any settlement date at all. The index is a
  // published member of its family - its own constructor established that - so the maturity offset
  // read here is the one the published data declares, and the settlement date is required to lie
  // at or beyond the fixing date by the business days that offset counts. Which days those are is
  // a property of a holiday calendar, and a constructor is handed no reference data to resolve
  // one, so the offset bounds the date rather than recomputing it; `of` is the door that derives
  // it, and it is the only door a caller has.
  JvmClosure.requireInvariant(
    "its maturity date is no earlier than its fixing date advanced by the business days of the " +
      "maturity offset of its index",
    index.maturityDateOffset.days < 0 ||
      !maturityDate.isBefore(fixingDate.plusDays(index.maturityDateOffset.days.toLong)))

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

  /**
   * Checks if this observation equals another observation, comparing the index and fixing date.
   *
   * The maturity date is ignored, which is the documented behaviour of the bean being ported; see
   * the note on equality on this type. The all-field equality a case class would otherwise
   * synthesize is therefore replaced here, and the comparison is a type test rather than a
   * comparison of runtime classes: every instance is of the one hidden subclass of this class - that
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
   * cannot depend on which subclass an instance happens to have. Neither property is
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
   * declares it, and `new Impl(...)` - the hidden implementation class declared below - is how
   * it is reached; that is what leaves the type without a public `apply` or `copy`
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

    new Impl(index, fixingDate, maturityDate)

  /**
   * The one implementation of an FX index observation.
   *
   * Declared and hidden here for the reasons given on the implementation of
   * [[IborIndexObservation]]: a private member class cannot be named by a compiler in another
   * language, and a named class can be compared against, which is what lets
   * [[FxIndexObservation]] refuse in its own constructor to be any other implementation.
   *
   * @param index  the index
   * @param fixingDate  the fixing date
   * @param maturityDate  the maturity date already derived from the index and the fixing date
   */
  private final class Impl(index: FxIndex, fixingDate: LocalDate, maturityDate: LocalDate)
      extends FxIndexObservation(index, fixingDate, maturityDate)

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
      extends NoJavaSerialization

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
