/*
 * Copyright (C) 2013 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.result

import cats.Hash
import cats.Order
import cats.Show
import cats.data.EitherNec
import cats.data.NonEmptyList

import io.circe.Decoder
import io.circe.Encoder

import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.named.NamedEnum

/**
 * The reason why a failure occurred.
 *
 * Every failure reported by this library carries exactly one of the ten reasons defined in
 * the companion of this class. The reason classifies a failure coarsely enough to be acted
 * on by a caller that knows nothing else about the operation that produced it: it is the
 * part of a failure a program branches on, while the message that accompanies it is the
 * part a person reads.
 *
 * ===Failures are values===
 *
 * Code in this package is written in a functional style, where an operation that cannot
 * produce a result returns a failure rather than abandoning the call stack. A reason is
 * therefore ordinary immutable data that is returned, combined, matched on and serialized
 * like any other value, and it is never tied to a stack unwinding mechanism of any kind.
 *
 * ===A closed family===
 *
 * The ten reasons are fixed when this file is compiled. The class is `sealed`, its
 * constructor is visible only inside this package, and each member exists exactly once as
 * a value in the companion, so no further reason can be brought into being - not by a
 * caller, not by a subclass in another file, and not by anything discovered while the
 * program runs. Two consequences matter in practice: the compiler checks a `match` over
 * the reasons for exhaustiveness, and the set of reasons observable at run time is
 * precisely the set visible at compile time.
 *
 * That closedness is what replaces the run-time machinery of the original. There, the
 * lookup keys of this enum were derived while the program ran, by a shared name helper that
 * read the constants of the Java enum back from its class; the wider machinery that helper
 * served went further still and could extend a family with members and spellings read from
 * an external table found on the class path. This port derives nothing and reads nothing:
 * the members are listed in `values` below, and a name is resolved against them by the
 * `NamedEnum` instance published alongside.
 *
 * ===Names===
 *
 * The canonical name of a reason is its identifier - upper case, words separated by
 * underscores, as in `MISSING_DATA`. That is the string `name` returns, the string
 * `toString` renders, and the string the JSON codec writes, which makes it the one
 * representation a stored or transmitted failure reason ever takes. The identifiers are
 * deliberately unchanged from the constants of the enum being ported, so that code and
 * documentation referring to a reason by name continue to name the same thing.
 *
 * Parsing is described in detail on `valueOf` and `parse`. In short, the canonical form
 * and its case variants are accepted, and a run-together form such as `MissingData` is
 * not - it was never a name of this family.
 *
 * ===Equality, ordering and rendering===
 *
 * The companion publishes exactly one equality-bearing instance, an `Order` that is also a
 * `Hash`, so there is no way for two notions of equality to disagree about a reason. It is
 * derived from `name` throughout - it compares, equates and hashes reasons by their
 * canonical name, as every named family of this library does. Comparing by name makes the
 * ordering alphabetical, whereas the enum being ported compared its constants by declaration
 * position; `values` still lists the members in declaration order, so both orders remain
 * available and neither is implied by the other.
 *
 * @see [[Failure]] for the failures that carry a reason
 */
sealed abstract class FailureReason private[result] (val name: String) extends Named {

  /**
   * Returns the canonical name of this reason.
   *
   * This is the same string as `name`, which reproduces the rendering of the type being
   * ported, so a reason interpolated into a message reads as its identifier.
   *
   * @return the canonical name of this reason
   */
  override def toString: String = name
}

/**
 * Provides the ten failure reasons, together with the name lookup, typeclass instances and
 * JSON codec for them.
 *
 * The members are declared in the order of the enum constants being ported, and `values`
 * preserves that order.
 */
object FailureReason {

  /**
   * There were several failures, and they did not agree on a reason.
   *
   * An operation may report any number of failures. Where a single failure is reported its
   * own reason describes the outcome, and where several agree on a reason that reason is
   * kept; where they differ, the combined outcome is reported as `MULTIPLE` while each
   * individual failure retains the reason of its own.
   */
  case object MULTIPLE extends FailureReason("MULTIPLE")

  /**
   * An error occurred.
   *
   * This is the general-purpose reason for something having gone wrong, and one of the
   * more specific reasons below should be preferred wherever one of them applies.
   */
  case object ERROR extends FailureReason("ERROR")

  /**
   * The input was invalid.
   *
   * One or more of the parameters supplied to the operation were not acceptable. A
   * validating factory that rejects the arguments it was given reports this reason.
   */
  case object INVALID extends FailureReason("INVALID")

  /**
   * An error occurred while parsing text.
   *
   * The text supplied could not be interpreted as a value of the type expected, which
   * includes the case of a name that belongs to no member of a closed family of named
   * values.
   */
  case object PARSING extends FailureReason("PARSING")

  /**
   * The operation was not applicable to this combination of inputs.
   *
   * The request was well formed, and a result could have been produced for a different
   * combination of inputs; this particular combination simply has no result. A grid of
   * results whose calculation is meaningful for only some of its rows reports this reason
   * for the remaining cells, rather than reporting them as errors.
   */
  case object NOT_APPLICABLE extends FailureReason("NOT_APPLICABLE")

  /**
   * The operation requested is not supported.
   *
   * The operation failed because the implementation does not offer it at all, which is a
   * statement about the implementation rather than about the inputs it was given.
   */
  case object UNSUPPORTED extends FailureReason("UNSUPPORTED")

  /**
   * The operation failed because data it required was missing.
   *
   * One or more pieces of data that the operation needed were not available to it. The
   * operation itself was applicable and its inputs were acceptable; what was absent is
   * something they referred to.
   */
  case object MISSING_DATA extends FailureReason("MISSING_DATA")

  /**
   * The operation failed while converting between currencies.
   *
   * This covers every failure of a conversion, the most common being the absence of a rate
   * for the pair of currencies being converted.
   */
  case object CURRENCY_CONVERSION extends FailureReason("CURRENCY_CONVERSION")

  /**
   * A calculation could not be performed.
   *
   * The operation applied and its inputs were acceptable, but the calculation itself did
   * not reach a result.
   */
  case object CALCULATION_FAILED extends FailureReason("CALCULATION_FAILED")

  /**
   * The failure occurred for some other reason.
   *
   * This is the last resort, to be used only when no other reason applies. Needing it
   * often is a sign that a further, more descriptive reason should be added here.
   */
  case object OTHER extends FailureReason("OTHER")

  /**
   * The complete set of failure reasons, in declaration order.
   *
   * The order is the declaration order of the enum constants being ported and is part of
   * what this file preserves; it is not the order the `Order` instance below imposes, which
   * is alphabetical by name. The list is non-empty by construction, which is what allows
   * every operation over the family - a name lookup table, a generator, an exhaustive
   * report - to be written without a case for a family that has no members.
   *
   * @return the ten reasons, in declaration order
   */
  val values: NonEmptyList[FailureReason] =
    NonEmptyList.of(
      MULTIPLE,
      ERROR,
      INVALID,
      PARSING,
      NOT_APPLICABLE,
      UNSUPPORTED,
      MISSING_DATA,
      CURRENCY_CONVERSION,
      CALCULATION_FAILED,
      OTHER
    )

  /**
   * The name lookup for this family.
   *
   * This instance is the single route from text to a reason, and it is built from `values`
   * alone. The three lookup tables a named family may declare are all empty here, because
   * this family declares none of them: there is no alternate spelling of any reason, no
   * pattern that rewrites text before it is looked up, and no group of names published for
   * an external protocol. The whole name space of the family is therefore its ten
   * canonical names, which is exactly the name space of the enum being ported.
   *
   * The lookup is labelled `FailureReason`, the simple name of the type, which is the label
   * a rejection carries: text naming no reason is reported as a failure whose message opens
   * with this family rather than with a generic one, as `FailureReason name not found:
   * Rubbish`. That reproduces the message of the registry being ported, which built it from
   * the simple name of the type it was looking a name up in.
   *
   * @return the name lookup for the ten reasons
   */
  implicit val namedEnum: NamedEnum[FailureReason] =
    NamedEnum.of(values, Map.empty, Nil, Map.empty, "FailureReason")

  /**
   * Obtains the reason with the specified canonical name, if one exists.
   *
   * The match is exact, against the canonical names as they are declared. Since every
   * canonical name of this family is already upper case, folding a canonical name to upper
   * case changes nothing and the lookup behaves as a plain exact match: `MISSING_DATA`
   * resolves, while `missing_data` does not. Use `parse` to accept text whose case is not
   * known in advance.
   *
   * @param name  the name to look up
   * @return the reason with that name, or `None` when no reason has it
   */
  def valueOf(name: String): Option[FailureReason] = namedEnum.valueOf(name)

  /**
   * Parses a reason from text, tolerating the case of the input.
   *
   * The lookup first tries the exact match of `valueOf`. When that finds nothing, the input
   * is folded to upper case and looked up once more, which is the whole of the leniency
   * available to this family, since it declares no rewrite pattern for the step between the
   * two lookups. The observable result is a lookup that ignores case while respecting every
   * other character:
   *
   * {{{
   * parse("MISSING_DATA")  // Right(MISSING_DATA) - the canonical name
   * parse("missing_data")  // Right(MISSING_DATA) - folded to upper case
   * parse("MissingData")   // Left - a name this family has never had
   * parse("")              // Left - no reason has an empty name
   * }}}
   *
   * The two forms that resolve are exactly the two the enum being ported accepted, so text
   * written by the original is read back as the same reason. The run-together form shown
   * above belongs to families that spell their names that way; this one never has, because
   * its rendering has always been the constant identifier itself.
   *
   * Where the original signalled an unrecognised name by raising an error, this method
   * reports it as a value: the result is `Left` of a chain holding one [[Failure]] whose
   * reason is `PARSING` and whose message names both the family and the text that could not
   * be resolved.
   *
   * @param name  the text to parse
   * @return the reason the text names, or the failure describing why it names none
   */
  def parse(name: String): EitherNec[Failure, FailureReason] = namedEnum.parse(name)

  /**
   * The ordering and hashing of reasons.
   *
   * This is the only equality-bearing instance of the type: `Order` extends `Eq` and
   * `Hash` extends `Eq`, so summoning any of the three yields this one value and the three
   * can never disagree. All three notions are derived from `name`, as they are for every
   * named family of this library: comparison is the comparison of the names as text,
   * equality is equality of the names, and the hash of a reason is the hash of its name.
   * Comparison is therefore alphabetical rather than the declaration ordering of the enum
   * being ported, which `values` still lists. Equality follows the comparison - names are
   * unique across the family, so two reasons compare equal if, and only if, they are the
   * same reason - which is the law the combined instance has to satisfy, and it coincides
   * with the equality of the values themselves because each reason exists exactly once.
   *
   * @return the ordering of reasons by name, which is also their hashing
   */
  implicit val order: Order[FailureReason] with Hash[FailureReason] =
    NamedEnum.orderByName[FailureReason]

  /**
   * The rendering of reasons as text.
   *
   * A reason renders as its canonical name, which is what `toString` produces as well, so
   * the two ways of putting a reason into a message agree.
   *
   * @return the rendering of a reason as its canonical name
   */
  implicit val show: Show[FailureReason] = Show.show(_.name)

  /**
   * The JSON encoder for reasons.
   *
   * A reason is written as the bare string of its canonical name - `"MISSING_DATA"` - and
   * never as an object, so a serialized failure carries its reason as one readable word.
   * That is the single-string form the type being ported wrote, which is what allows a
   * document written before this port to be read after it.
   *
   * The codec is defined here rather than taken from the shared JSON helpers of this
   * module because those helpers are written in terms of the failure model, which is itself
   * written in terms of this type; defining it here keeps the dependencies of the module
   * pointing in one direction. It is derived at compile time, and nothing about it inspects
   * a type at run time.
   *
   * @return the encoder writing a reason as its canonical name
   */
  implicit val encoder: Encoder[FailureReason] = Encoder.encodeString.contramap(_.name)

  /**
   * The JSON decoder for reasons.
   *
   * A string is read through `parse`, so a document is accepted whatever the case of the
   * name it holds, and a document naming no reason of this family is rejected with a
   * decoding failure whose message is built from the messages of the parse failures, joined
   * with `"; "`. The text that could not be resolved appears in that message, because the
   * parse failure names it.
   *
   * @return the decoder reading a reason from its canonical name
   */
  implicit val decoder: Decoder[FailureReason] =
    Decoder.decodeString.emap { text =>
      parse(text).left.map(failures => failures.iterator.map(_.message).mkString("; "))
    }
}
