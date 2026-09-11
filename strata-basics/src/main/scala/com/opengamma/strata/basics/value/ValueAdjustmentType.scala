/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.value

import cats.Hash
import cats.Order
import cats.Show
import cats.data.NonEmptyList

import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder

import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.named.NamedEnum

/**
 * The type of value adjustment.
 *
 * A `Double` value can be transformed into another value in various different ways. Each
 * type is a function of two values, the base value and the modifying value, and the whole
 * of its behaviour is the `adjust` operation it defines over that pair.
 *
 * Each type represents a different way to express the same concept. For example, here is how
 * an increase from 200 to 220 could be represented:
 *
 * {{{
 * Type             baseValue  modifyingValue  Calculation
 * Replace          200        220             result = modifyingValue = 220
 * DeltaAmount      200        20              result = baseValue + modifyingValue = (200 + 20) = 220
 * DeltaMultiplier  200        0.1             result = baseValue + baseValue * modifyingValue = (200 + 200 * 0.1) = 220
 * Multiplier       200        1.1             result = baseValue * modifyingValue = (200 * 1.1) = 220
 * }}}
 *
 * The arithmetic of each member is written in exactly the shape the type being ported used,
 * rather than in an algebraically equivalent one, because floating-point addition and
 * multiplication do not associate: rewriting `baseValue + baseValue * modifyingValue` as
 * `baseValue * (1 + modifyingValue)` would change the result in the last bits for some
 * inputs. Keeping the shape keeps every result bit-identical to the original.
 *
 * ===A closed family===
 *
 * The four types are fixed when this file is compiled. The class is `sealed`, its
 * constructor is visible only inside this package, and each member exists exactly once as a
 * value in the companion, so no further type can be brought into being - not by a caller,
 * not by a subclass in another file, and not by anything discovered while the program runs.
 * The compiler therefore checks a `match` over the types for exhaustiveness, and the set of
 * types observable at run time is precisely the set visible at compile time.
 *
 * That closedness is what replaces the run-time machinery of the original, where the lookup
 * keys of this enum were derived while the program ran by a shared name helper that read the
 * constants of the Java enum back from its class. This port derives nothing and reads
 * nothing: the members are listed in `values`, the spellings they answer to are listed in the
 * companion, and a name is resolved against them by the [[NamedEnum]] instance published
 * there.
 *
 * ===Names===
 *
 * The canonical name of a type is its mixed case form - `Replace`, `DeltaAmount`,
 * `DeltaMultiplier`, `Multiplier` - which the name helper of the original derived from the
 * constant identifier and which this port states directly. That is the string `name`
 * returns, the string `toString` renders, the string `Show` produces and the string the JSON
 * codec writes, so a stored or transmitted adjustment type always takes that one form. The
 * Scala identifier of each member is that same canonical name, so
 * `ValueAdjustmentType.DeltaAmount` is both how the member is written in code and how it
 * renders. The constant identifiers of the Java enum - `DELTA_AMOUNT` and its fellows - remain
 * accepted spellings of the name lookup, so text written against the original still resolves
 * even though no member is spelled that way in code.
 *
 * Parsing is described on `valueOf` and `parse`. In short, every spelling the original
 * accepted is accepted here: the canonical mixed case form, the Java constant identifier, the
 * run-together form, and the upper and lower case variants of each.
 *
 * ===Equality, ordering and rendering===
 *
 * The companion publishes exactly one equality-bearing instance, an `Order` that is also a
 * `Hash`, so there is no way for two notions of equality to disagree about a type. It
 * compares by `name`, which makes the ordering alphabetical, whereas the enum being ported
 * compared its constants by declaration position; `values` still lists the members in
 * declaration order, so both orders remain available and neither is implied by the other.
 *
 * @param name  the canonical name of this type, as it is rendered and parsed
 */
sealed abstract class ValueAdjustmentType private[value] (val name: String) extends Named {

  /**
   * Adjusts the base value based on the type and the modifying value.
   *
   * The operation is total: every pair of values has a result, and a result that is not a
   * number - reached only from an input that is not a number, or from an infinite input
   * whose arithmetic is undefined - is returned as such rather than reported, exactly as in
   * the type being ported. A caller that requires a finite result checks the value it gets
   * back, or checks its inputs before calling.
   *
   * @param baseValue  the base, or previous, value to be adjusted
   * @param modifyingValue  the value that the type uses to modify the base value
   * @return the calculated result
   */
  def adjust(baseValue: Double, modifyingValue: Double): Double

  /**
   * Returns the canonical name of this type.
   *
   * This is the same string as `name`, which reproduces the rendering of the type being
   * ported, so an adjustment type interpolated into a message reads as its mixed case name
   * rather than as the SCREAMING_SNAKE spelling the Java enum constant would have rendered.
   * It is stated here rather than left to the rendering a `case object` derives, so the
   * rendering stays a statement about `name` and not about how the members are spelled.
   *
   * @return the formatted string representing this type
   */
  override def toString: String = name
}

/**
 * Provides the four value adjustment types, together with the name lookup, typeclass
 * instances and JSON codec for them.
 *
 * The members are declared in the order of the enum constants being ported, and `values`
 * preserves that order.
 */
object ValueAdjustmentType {

  /**
   * The modifying value replaces the base value. The input base value is ignored.
   *
   * The result is `modifyingValue`.
   */
  case object Replace extends ValueAdjustmentType("Replace") {
    override def adjust(baseValue: Double, modifyingValue: Double): Double = modifyingValue
  }

  /**
   * Calculates the result by treating the modifying value as a delta, adding it to the base
   * value.
   *
   * The result is `(baseValue + modifyingValue)`.
   *
   * This adjustment type can be referred to as an ''absolute shift''.
   */
  case object DeltaAmount extends ValueAdjustmentType("DeltaAmount") {
    override def adjust(baseValue: Double, modifyingValue: Double): Double =
      (baseValue + modifyingValue)
  }

  /**
   * Calculates the result by treating the modifying value as a multiplication factor, adding
   * it to the base value.
   *
   * The result is `(baseValue + baseValue * modifyingValue)`.
   *
   * This adjustment type can be referred to as a ''relative shift''.
   */
  case object DeltaMultiplier extends ValueAdjustmentType("DeltaMultiplier") {
    override def adjust(baseValue: Double, modifyingValue: Double): Double =
      (baseValue + baseValue * modifyingValue)
  }

  /**
   * Calculates the result by treating the modifying value as a multiplication factor to
   * apply to the base value.
   *
   * The result is `(baseValue * modifyingValue)`.
   */
  case object Multiplier extends ValueAdjustmentType("Multiplier") {
    override def adjust(baseValue: Double, modifyingValue: Double): Double =
      (baseValue * modifyingValue)
  }

  /**
   * The complete set of value adjustment types, in declaration order.
   *
   * The order is the declaration order of the enum constants being ported and is part of what
   * this file preserves; it is not the order the `Order` instance below imposes, which is
   * alphabetical by name. The list is non-empty by construction, which is what allows every
   * operation over the family - a name lookup table, a generator, an exhaustive report - to
   * be written without a case for a family that has no members.
   *
   * @return the four types, in declaration order
   */
  val values: NonEmptyList[ValueAdjustmentType] =
    NonEmptyList.of(
      Replace,
      DeltaAmount,
      DeltaMultiplier,
      Multiplier
    )

  /**
   * The alternate spellings of the family, each mapped to a canonical name.
   *
   * Every key here is text, never a Scala identifier: the SCREAMING_SNAKE forms are the
   * identifiers of the constants of the Java enum being ported, which no member of this family
   * is named after any more, and they are carried as lookup keys so that text written against
   * the original still resolves.
   *
   * The name helper of the original registered six lookup keys for every constant: the Java
   * constant identifier, that identifier in upper and in lower case, the canonical mixed case
   * form, and that form in upper and in lower case. A named family registers two of those on
   * its own - the canonical name and the canonical name folded to upper case - so this table
   * supplies the rest, and only the rest: the Java constant identifier, that identifier in
   * lower case, and the canonical form run together in lower case. Nothing here is a spelling
   * the original did not accept, and nothing the family already registers is repeated, which
   * keeps the set of resolvable names identical to the original's rather than merely a superset
   * of it.
   *
   * The two members whose Java constant identifier is a single word need only one row each. For
   * `Replace` and `Multiplier` that identifier and the run-together form are both the canonical
   * name folded to upper case, which the family already registers, so only the lower case form
   * remains. The two compound members need three rows each, which is how eight rows cover four
   * members.
   */
  private val Alternates: Map[String, String] =
    Map(
      "replace" -> "Replace",
      "DELTA_AMOUNT" -> "DeltaAmount",
      "delta_amount" -> "DeltaAmount",
      "deltaamount" -> "DeltaAmount",
      "DELTA_MULTIPLIER" -> "DeltaMultiplier",
      "delta_multiplier" -> "DeltaMultiplier",
      "deltamultiplier" -> "DeltaMultiplier",
      "multiplier" -> "Multiplier"
    )

  /**
   * The name lookup for this family.
   *
   * This instance is the single route from text to a type, and it is built from `values` and
   * `Alternates` alone. Of the three tables a named family may declare, this family declares
   * only the alternate spellings: there is no pattern that rewrites text before it is looked
   * up, and no group of names published for an external protocol. The whole name space of the
   * family is therefore its four canonical names, those four names folded to upper case, and
   * the eight spellings of `Alternates` - sixteen names in all, which is exactly the set of
   * sixteen the enum being ported could parse.
   *
   * @return the name lookup for the four types
   */
  implicit val namedEnum: NamedEnum[ValueAdjustmentType] =
    NamedEnum.of(values = values, alternates = Alternates, familyName = "ValueAdjustmentType")

  /**
   * Obtains the type with the specified name, if one exists.
   *
   * The match is exact: the alternate spellings above are consulted, and the result is
   * compared against the canonical names and against those names folded to upper case. Every
   * spelling the type being ported accepted resolves here, and nothing else does:
   *
   * {{{
   * valueOf("DeltaAmount")   // Some(DeltaAmount) - the canonical name
   * valueOf("DELTAAMOUNT")   // Some(DeltaAmount) - the canonical name in upper case
   * valueOf("deltaamount")   // Some(DeltaAmount) - the canonical name in lower case
   * valueOf("DELTA_AMOUNT")  // Some(DeltaAmount) - the Java constant identifier
   * valueOf("delta_amount")  // Some(DeltaAmount) - that identifier in lower case
   * valueOf("Delta Amount")  // None - never a name of this family
   * }}}
   *
   * Use `parse` to accept text whose case is not known in advance, and to obtain a failure
   * describing text that names nothing.
   *
   * @param name  the name to look up
   * @return the type with that name, or `None` when no type has it
   */
  def valueOf(name: String): Option[ValueAdjustmentType] = namedEnum.valueOf(name)

  /**
   * Parses a type from text, tolerating the case of the input.
   *
   * The lookup first tries the exact match of `valueOf`. When that finds nothing, the input is
   * folded to upper case and looked up once more, which is the whole of the leniency available
   * to this family, since it declares no rewrite pattern for the step between the two lookups.
   * Every spelling the enum being ported accepted is therefore resolved by this method too,
   * and text that differs from one of them in case alone is resolved as well.
   *
   * Where the original signalled an unrecognised name by raising an error, this method reports
   * it as a value: the result is `Left` of a chain holding one failure whose reason is
   * `PARSING` and whose message names both this family and the text that could not be
   * resolved. No input raises.
   *
   * {{{
   * parse("DeltaAmount")  // Right(DeltaAmount)
   * parse("delta_amount") // Right(DeltaAmount)
   * parse("Rubbish")      // Left - no type of this family has that name
   * }}}
   *
   * @param name  the text to parse
   * @return the type the text names, or the failure describing why it names none
   */
  def parse(name: String): ResultNec[ValueAdjustmentType] = namedEnum.parse(name)

  /**
   * The ordering and hashing of types.
   *
   * This is the only equality-bearing instance of the type: `Order` extends `Eq` and `Hash`
   * extends `Eq`, so summoning any of the three yields this one value and the three can never
   * disagree. Comparison is over `name`, which makes the ordering alphabetical rather than the
   * declaration ordering of the enum being ported, and equality follows it - names are unique
   * across the family, so two types compare equal if, and only if, they are the same type,
   * which is the law the combined instance has to satisfy. Since each member exists exactly
   * once, that agrees with the identity of the members themselves.
   *
   * @return the ordering of types by name, which is also their hashing
   */
  implicit val order: Order[ValueAdjustmentType] with Hash[ValueAdjustmentType] =
    NamedEnum.orderByName

  /**
   * The rendering of types as text.
   *
   * A type renders as its canonical name, which is what `toString` produces as well, so the
   * two ways of putting a type into a message agree.
   *
   * @return the rendering of a type as its canonical name
   */
  implicit val show: Show[ValueAdjustmentType] = NamedEnum.showByName

  /**
   * The JSON form of a type, as the shared codec for a closed named family builds it.
   *
   * The two halves published below are taken from this single value so that reading and
   * writing cannot drift apart, and so that the resolution rules of the family are applied by
   * the family's own lookup rather than restated here. The codec is built at compile time from
   * the `namedEnum` instance above and inspects no type while the program runs.
   */
  private val codec: Codec[ValueAdjustmentType] = Codecs.namedEnumCodec[ValueAdjustmentType]

  /**
   * The JSON encoder for types.
   *
   * A type is written as the bare string of its canonical name - `"DeltaAmount"` - and never
   * as an object, so a serialized adjustment carries its type as one readable word. That is
   * the single-string form the type being ported wrote, which is what allows a document
   * written before this port to be read after it.
   *
   * @return the encoder writing a type as its canonical name
   */
  implicit val encoder: Encoder[ValueAdjustmentType] = codec

  /**
   * The JSON decoder for types.
   *
   * A string is read through `parse`, so a document is accepted whatever the case of the name
   * it holds and whichever of the family's spellings it uses, and a document naming no type of
   * this family is rejected with a decoding failure built from the message of the parse
   * failure. The text that could not be resolved appears in that message, because the parse
   * failure names it.
   *
   * @return the decoder reading a type from its canonical name
   */
  implicit val decoder: Decoder[ValueAdjustmentType] = codec
}
