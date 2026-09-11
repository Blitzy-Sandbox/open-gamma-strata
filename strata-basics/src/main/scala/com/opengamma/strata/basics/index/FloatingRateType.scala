/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import cats.Hash
import cats.Order
import cats.Show
import cats.data.EitherNec
import cats.data.NonEmptyList

import io.circe.Codec

import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.Failure

/**
 * The type of a floating rate index.
 *
 * This provides a high-level categorization of a floating rate index: it says what kind of
 * rate a floating rate name describes, and therefore which concrete index a name has to be
 * turned into before it can be used. An `Ibor` type becomes an Ibor index, either of the two
 * `Overnight` types becomes an Overnight index, `Price` becomes a price index, and `Other`
 * covers a rate this library models by name alone. The distinction between the two Overnight
 * types is the accrual convention rather than the index: compounding is the general case,
 * while averaging is used almost exclusively by US Fed Fund swaps.
 *
 * The family has exactly five members, they are the five constants of the enum being ported,
 * and nothing can add a sixth: the type is `sealed`, every member is declared in this file,
 * and the name lookup is built from those members alone. A `match` over a value of this type
 * is therefore checked for exhaustiveness by the compiler, which is what lets the conversion
 * from a floating rate name to an index enumerate the kinds it handles and be told at compile
 * time if the set ever changes.
 *
 * ===What this replaces===
 *
 * The type being ported is a Java `enum` whose text form was derived from the constant
 * identifier at class-initialization time by a shared name helper, and whose `of` factory
 * raised an error for text naming no constant. This port keeps both name forms and the whole
 * of the accepted name space, and discards the two mechanisms behind them. The text form is
 * now a string written out beside each member, so it is visible in this file rather than
 * computed from an identifier; and the factory reports rejected text as a value - `valueOf`
 * answers with an `Option` and `parse` with a `Failure` on the left of an `EitherNec` - so no
 * caller of this family has an error to catch. The annotations that registered the two
 * directions with the reflective string-conversion library of the original are gone with that
 * library; the JSON codec below is their replacement and is built by the compiler.
 *
 * ===Names===
 *
 * A member's name is the mixed-case rendering of its constant identifier - `IBOR` becomes
 * `Ibor` and `OVERNIGHT_COMPOUNDED` becomes `OvernightCompounded` - which is exactly what the
 * name helper of the original produced, so text written before this port reads back as the
 * same member and text written after it is read by the original unchanged. The name is also
 * what `toString` returns and what the JSON codec writes.
 *
 * The original accepted six spellings of each member: the constant identifier, the rendered
 * name, and each of those in upper and in lower case. All six still resolve, through
 * `valueOf` and through `parse` alike - the rendered name and its upper-case form are the two
 * keys the name lookup registers for every family, and the remaining spellings are declared
 * as alternate names below.
 *
 * ===Equality, ordering and rendering===
 *
 * The companion publishes exactly one equality-bearing instance, an `Order` that is also a
 * `Hash`, so two notions of equality cannot disagree about a type. Both compare by `name`,
 * which makes the ordering alphabetical - `Ibor`, `Other`, `OvernightAveraged`,
 * `OvernightCompounded`, `Price` - rather than the declaration ordering the Java enum
 * compared by. `values` still lists the members in declaration order, so both orders remain
 * available and neither is implied by the other.
 *
 * @see [[FloatingRate]] for the abstraction over the two kinds of floating rate
 */
sealed abstract class FloatingRateType private[index] (val name: String) extends Named {

  /**
   * Checks if the type is 'Ibor'.
   *
   * @return true if Ibor, false otherwise
   */
  def isIbor: Boolean = this == FloatingRateType.Ibor

  /**
   * Checks if the type is 'OvernightCompounded' or 'OvernightAveraged'.
   *
   * Both Overnight types translate to an Overnight index, differing only in how the rate
   * accrues, so code that cares about the index rather than the accrual asks this question
   * instead of comparing against either member.
   *
   * @return true if Overnight, false otherwise
   */
  def isOvernight: Boolean =
    this == FloatingRateType.OvernightCompounded || this == FloatingRateType.OvernightAveraged

  /**
   * Checks if the type is 'Price'.
   *
   * @return true if Price, false otherwise
   */
  def isPrice: Boolean = this == FloatingRateType.Price

  /**
   * Returns the formatted name of the type.
   *
   * This is the same string as `name`, so a type interpolated into a message renders as the
   * mixed-case form the type being ported rendered, and agrees with the `Show` instance and
   * with the JSON representation.
   *
   * @return the formatted string representing the type
   */
  override def toString: String = name
}

/**
 * Provides the five floating rate types, together with the name lookup, typeclass instances
 * and JSON codec for them.
 *
 * The members are declared in the order of the enum constants being ported, and `values`
 * preserves that order.
 */
object FloatingRateType {

  /**
   * A floating rate index that is based on an Ibor index.
   *
   * This kind of rate translates to an Ibor index, the tenor of which is supplied by the
   * caller, since a floating rate name fixes the family of a rate but not its tenor.
   */
  case object Ibor extends FloatingRateType("Ibor")

  /**
   * A floating rate index that is based on an Overnight index with compounding.
   *
   * This kind of rate translates to an Overnight index. Compounding is the usual accrual for
   * an Overnight rate, and is the one assumed wherever a rate does not say otherwise.
   */
  case object OvernightCompounded extends FloatingRateType("OvernightCompounded")

  /**
   * A floating rate index that is based on an Overnight index with averaging.
   *
   * This kind of rate translates to an Overnight index. Averaging is typically used only for
   * US Fed Fund swaps, which is why it is a type of its own rather than a property of the
   * index.
   */
  case object OvernightAveraged extends FloatingRateType("OvernightAveraged")

  /**
   * A floating rate index that is based on a price index.
   *
   * This kind of rate translates to a price index, such as an inflation index, whose
   * observations are monthly rather than daily.
   */
  case object Price extends FloatingRateType("Price")

  /**
   * A floating rate index of another type.
   *
   * This is the kind of a rate that the library holds by name without modelling it as one of
   * the index families above, so it converts to no index at all.
   */
  case object Other extends FloatingRateType("Other")

  /**
   * The complete set of floating rate types, in declaration order.
   *
   * The order is the declaration order of the enum constants being ported and is part of what
   * this file preserves; it is not the order the `Order` instance below imposes, which is
   * alphabetical by name. The list is non-empty by construction, which is what allows every
   * operation over the family - the name lookup, a generator, an exhaustive report - to be
   * written without a case for a family that has no members.
   *
   * @return the five types, in declaration order
   */
  val values: NonEmptyList[FloatingRateType] =
    NonEmptyList.of(
      Ibor,
      OvernightCompounded,
      OvernightAveraged,
      Price,
      Other
    )

  /**
   * The spellings accepted in addition to the two keys every member is registered under.
   *
   * The name helper of the type being ported accepted six spellings of each member: the
   * constant identifier, the rendered name, and each of those folded to upper and to lower
   * case. The name lookup of a family already registers each member under its rendered name
   * and that name folded to upper case, so this table supplies precisely the remainder, and
   * nothing beyond it:
   *
   *  - the constant identifier and its lower-case form, for the two members whose identifier
   *    differs from their rendered name by more than case - the underscore of
   *    `OVERNIGHT_COMPOUNDED` survives no case folding;
   *  - the lower-case form of the rendered name, for every member.
   *
   * Each row maps a spelling to a canonical name rather than to a member, which is the shape
   * the lookup takes, and the lookup expands the table with the upper-case form of every
   * spelling in it. That expansion lands only on keys which already resolve to the same
   * member, so no row here can displace another or redirect a spelling the family already
   * accepted.
   */
  private val AlternateNames: Map[String, String] =
    Map(
      "ibor" -> "Ibor",
      "OVERNIGHT_COMPOUNDED" -> "OvernightCompounded",
      "overnight_compounded" -> "OvernightCompounded",
      "overnightcompounded" -> "OvernightCompounded",
      "OVERNIGHT_AVERAGED" -> "OvernightAveraged",
      "overnight_averaged" -> "OvernightAveraged",
      "overnightaveraged" -> "OvernightAveraged",
      "price" -> "Price",
      "other" -> "Other"
    )

  /**
   * The name lookup for this family.
   *
   * This instance is the single route from text to a type, and it is built from `values` and
   * the alternate spellings above. Of the three tables a named family may declare, the family
   * declares one: there is no pattern that rewrites text before it is looked up, and no group
   * of names published for an external protocol, because the configuration of the library
   * being ported declares neither for this family - it declares nothing for it at all, the
   * whole of its name space having always been derived from its own constants.
   *
   * @return the name lookup for the five types
   */
  implicit val namedEnum: NamedEnum[FloatingRateType] =
    NamedEnum.of(values, AlternateNames, Nil, Map.empty, "FloatingRateType")

  /**
   * Obtains the type that the specified name identifies, if one exists.
   *
   * This is the exact lookup, and it accepts the six spellings of each member that the type
   * being ported accepted, and only those:
   *
   * {{{
   * valueOf("OvernightCompounded")   // Some(OvernightCompounded) - the rendered name
   * valueOf("OVERNIGHTCOMPOUNDED")   // Some(OvernightCompounded) - that name in upper case
   * valueOf("overnightcompounded")   // Some(OvernightCompounded) - and in lower case
   * valueOf("OVERNIGHT_COMPOUNDED")  // Some(OvernightCompounded) - the constant identifier
   * valueOf("overnight_compounded")  // Some(OvernightCompounded) - and in lower case
   * valueOf("OvernightCompounded ")  // None - no member has that name
   * }}}
   *
   * Text of any other shape, including a mixed case the table above does not name, resolves
   * only through `parse`, which tolerates the case of its input.
   *
   * @param name  the name to look up
   * @return the type with that name, or `None` when no type has it
   */
  def valueOf(name: String): Option[FloatingRateType] = namedEnum.valueOf(name)

  /**
   * Parses a type from text, tolerating the case of the input.
   *
   * The exact lookup of `valueOf` is tried first, so every spelling the type being ported
   * accepted is resolved by it. When that finds nothing, the text is folded to upper case and
   * looked up once more, which is the whole of the leniency available to this family, since
   * it declares no rewrite pattern for the step between the two lookups. The observable
   * result is a lookup that ignores case while respecting every other character, so `iBoR`
   * resolves here where the original rejected it, and `Overnight Compounded` is rejected as
   * it always was.
   *
   * Where the original signalled an unrecognised name by raising an error, this method
   * reports it as a value: the result is `Left` of a chain holding one
   * [[com.opengamma.strata.collect.result.Failure.Parsing]] whose message names both this
   * family and the text that could not be resolved. The returned type is the same as
   * `collect.ResultNec[FloatingRateType]`, spelled out here for readability.
   *
   * @param name  the text to parse
   * @return the type the text names, or the failure describing why it names none
   */
  def parse(name: String): EitherNec[Failure, FloatingRateType] = namedEnum.parse(name)

  /**
   * The ordering and hashing of types.
   *
   * This is the only equality-bearing instance of the type: `Order` extends `Eq` and `Hash`
   * extends `Eq`, so summoning any of the three yields this one value and the three can never
   * disagree. All three are derived from `name`, which is sound because the five names are
   * distinct and each member exists exactly once, so two types compare equal if, and only if,
   * they are the same type - the law the combined instance has to satisfy. Comparison by name
   * makes the ordering alphabetical rather than the declaration ordering of the enum being
   * ported.
   *
   * @return the ordering of types by name, which is also their hashing
   */
  implicit val order: Order[FloatingRateType] with Hash[FloatingRateType] =
    NamedEnum.orderByName[FloatingRateType]

  /**
   * The rendering of types as text.
   *
   * A type renders as its name, which is what `toString` produces as well, so the two ways of
   * putting a type into a message agree.
   *
   * @return the rendering of a type as its name
   */
  implicit val show: Show[FloatingRateType] = NamedEnum.showByName[FloatingRateType]

  /**
   * The JSON codec for types.
   *
   * A type is written as the bare string of its name - `"OvernightCompounded"` - and never as
   * an object, which is the single-string form the type being ported wrote through its
   * string-conversion annotations, so a document written before this port is read after it as
   * the same member. A string is read back through `parse`, so a document is accepted whatever
   * the case of the name it holds, and one naming no type of this family is rejected with a
   * decoding failure carrying the message of the parse failure.
   *
   * The codec is assembled by the compiler from the family's own name lookup, taken from the
   * shared JSON helpers of the collect module; nothing about it inspects a type, a class path
   * or a configuration source while the program runs.
   *
   * @return the codec reading and writing a type as its name
   */
  implicit val codec: Codec[FloatingRateType] = Codecs.namedEnumCodec[FloatingRateType]
}
