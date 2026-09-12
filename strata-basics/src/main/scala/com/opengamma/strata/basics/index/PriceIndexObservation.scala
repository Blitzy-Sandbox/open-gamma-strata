/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import java.time.YearMonth

import cats.Hash
import cats.Show

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.collect.json.Codecs

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
