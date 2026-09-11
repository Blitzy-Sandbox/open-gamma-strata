/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.value

import cats.Hash
import cats.Show

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.collect.json.Codecs

/**
 * An adjustment to a value, describing how to change one value into another.
 *
 * A base value, represented as a `Double`, can be transformed into another value by stating
 * either the result itself (absolute) or the calculation that reaches it (relative). The
 * four ways of stating it are the members of [[ValueAdjustmentType]], and each expresses the
 * same concept differently. For example, here is how an increase from 200 to 220 could be
 * represented:
 *
 * {{{
 * Type             baseValue  modifyingValue  Calculation
 * Replace          200        220             result = modifyingValue = 220
 * DeltaAmount      200        20              result = baseValue + modifyingValue = (200 + 20) = 220
 * DeltaMultiplier  200        0.1             result = baseValue + baseValue * modifyingValue = (200 + 200 * 0.1) = 220
 * Multiplier       200        1.1             result = baseValue * modifyingValue = (200 * 1.1) = 220
 * }}}
 *
 * An adjustment is a pair and nothing more: the modifying value, and the type that gives
 * that value its meaning. None of the arithmetic lives here - `adjust` defers in full to the
 * type - so an adjustment is exactly as expressive as the closed family of types is, and a
 * reader who wants to know what a given adjustment computes reads it there.
 *
 * ===Construction===
 *
 * Construction is total, and all four factories of the companion are total with it. The
 * modifying value may be any double the caller holds, including one that is not a number and
 * the two infinities, because the meaning of the pair is fixed by the type rather than by the
 * range of the value; the only check the Java original performed was that the type reference
 * was present, a state this port cannot express. The primary constructor, `apply` and `copy`
 * are therefore all public, and the four named factories exist alongside them so that ported
 * call sites read as they did before.
 *
 * Both fields keep the names the Java bean declared, `modifyingValue` and `type`, in that
 * order. `type` is a reserved word of this language and so is written in backticks wherever
 * it appears, which changes how it is spelled in source and nothing else: it remains the name
 * of the property, the name of the accessor and the key of the JSON form.
 *
 * ===Equality===
 *
 * Equality and hashing compare the modifying value by its bit pattern rather than by numeric
 * comparison, which is what the bean equality of the Java original did and what every
 * double-bearing type of this port does. Two consequences are deliberate and are relied upon
 * by the round-trip properties of the test suite: a value that is not a number equals itself,
 * so an instance always equals itself, and a negative zero is distinct from a positive zero.
 * The type contributes its own equality, which is the identity of a closed set of four
 * members.
 *
 * ===Thread safety===
 *
 * An instance is immutable and holds only a double and one member of a closed family, so it
 * is safe to share between any number of threads without synchronisation.
 *
 * @param modifyingValue  the value used to modify the base value, which is given meaning by
 *   the associated type
 * @param type  the type of adjustment to make
 */
final case class ValueAdjustment(modifyingValue: Double, `type`: ValueAdjustmentType) {

  //-------------------------------------------------------------------------
  /**
   * Adjusts the base value based on the criteria of this adjustment.
   *
   * For example, if this adjustment represents a 10% decrease, then the result will be the
   * base value minus 10%. The calculation is the one the type defines over the pair, applied
   * in exactly the shape the Java original applied it, so results agree to the bit.
   *
   * The operation is total: every base value has a result, and a result that is not a number
   * - reachable only from an input that is not a number, or from arithmetic on infinities
   * that is undefined - is returned as it stands rather than reported. That matches the
   * original, and a caller that requires a finite result checks the value it gets back.
   *
   * @param baseValue  the base, or previous, value to be adjusted
   * @return the calculated result
   */
  def adjust(baseValue: Double): Double = `type`.adjust(baseValue, modifyingValue)

  //-------------------------------------------------------------------------
  /**
   * Checks whether this instance equals another object.
   *
   * Another instance is equal when its modifying value has the same bit pattern and its type
   * is the same member. The equality synthesised for a case class would compare the double
   * with the numeric comparison of the platform, under which a value that is not a number is
   * not even equal to itself, so this replaces it. An object of any other type is not equal.
   *
   * @param obj  the object to compare to
   * @return true if the other object holds the same modifying value and type
   */
  override def equals(obj: Any): Boolean = obj match {
    case other: ValueAdjustment =>
      (this eq other) ||
        (java.lang.Double.compare(modifyingValue, other.modifyingValue) == 0 &&
          `type` == other.`type`)
    case _ => false
  }

  /**
   * Returns a hash code consistent with `equals`.
   *
   * The mixing is that of the Java bean this replaces - a seed, then each field in
   * declaration order - with the modifying value hashed by its bit pattern so that instances
   * which `equals` calls equal always agree here too. The seed is the hash of the type's own
   * name rather than the identity hash of its class, which the generated bean used: that
   * makes the hash of an instance a function of the instance alone, identical in every run of
   * every program.
   *
   * @return the hash code of the modifying value and type held
   */
  override def hashCode: Int =
    (ValueAdjustment.HashSeed * 31 + java.lang.Double.hashCode(modifyingValue)) * 31 +
      `type`.hashCode

  /**
   * Returns this instance as text, describing the calculation it performs.
   *
   * The rendering names the calculation rather than the fields, so an adjustment reads as
   * what it does to its input - `ValueAdjustment[result = input + input * 0.1]` for a ten
   * per cent increase - and it reproduces the form of the Java original character for
   * character, including the rendering of the double, which is the same on both platforms.
   * The match is over the closed family of types, so the compiler checks that every type has
   * a rendering here and no branch is reached by default.
   *
   * An adjustment that makes no change is rendered as `ValueAdjustment[result = input]`
   * rather than as an addition of zero. Here this port diverges from the original in one
   * observable detail, deliberately: the original selected that form by comparing the
   * instance against its `NONE` constant by ''reference'', so a separately built zero delta
   * amount - `ofDeltaAmount(0)` - rendered as `input + 0.0`, while the constant itself
   * rendered as `input`. This port compares against `NONE` by ''value'', so both render as
   * `input`. The two forms describe the same calculation, no test of the original covers the
   * separately built instance, and comparing by value is what keeps the rendering stable
   * across a round trip through JSON, where the decoded adjustment is a distinct instance. A
   * negative zero remains distinct from `NONE` under the bit-pattern equality above and so
   * still renders as an addition.
   *
   * @return the rendering of the calculation this adjustment performs
   */
  override def toString: String = `type` match {
    case ValueAdjustmentType.DeltaAmount =>
      if (this == ValueAdjustment.NONE) {
        "ValueAdjustment[result = input]"
      } else {
        s"ValueAdjustment[result = input + $modifyingValue]"
      }
    case ValueAdjustmentType.DeltaMultiplier =>
      s"ValueAdjustment[result = input + input * $modifyingValue]"
    case ValueAdjustmentType.Multiplier =>
      s"ValueAdjustment[result = input * $modifyingValue]"
    case ValueAdjustmentType.Replace =>
      s"ValueAdjustment[result = $modifyingValue]"
  }
}

/**
 * The constant, factories, typeclass instances and JSON form of [[ValueAdjustment]].
 *
 * Two typeclass instances are published, and exactly two: a `Hash`, which is the single
 * equality-bearing instance of the type - `Hash` extends `Eq`, so declaring an `Eq` as well
 * would leave two instances that could disagree and one of them ambiguous - and a `Show`.
 * There is deliberately no `Order`: the Java type is not `Comparable`, and a modifying value
 * paired with a type has no ordering worth inventing, since comparing the values of two
 * adjustments of different types compares numbers that mean different things.
 */
object ValueAdjustment {

  /**
   * The seed of the hash code, standing in for the identity hash of the class that the
   * generated bean used, so that hashing is reproducible across runs. Any constant would do;
   * the hash of the type name is used because it is stable, specified by the platform, and
   * distinct from the seed of every other type of this port.
   */
  private val HashSeed: Int = "ValueAdjustment".hashCode

  /**
   * An instance that makes no adjustment to the value.
   *
   * This is a delta amount of zero, so `adjust` adds zero to its input and hands back what it
   * was given - including a value that is not a number, and either infinity. The one input
   * that does not come back bit for bit is a negative zero, which the addition of a positive
   * zero turns into a positive zero; that is the arithmetic of the platform rather than a
   * choice of this port, and the Java original, which performs the same addition, behaves
   * identically. `toString` renders this constant as `ValueAdjustment[result = input]`.
   *
   * The constant is built through `ofDeltaAmount` below, which is a method and so is
   * available while this object is being initialised; nothing in that construction renders
   * an adjustment, so the reference `toString` makes back to this constant is always to a
   * fully built value.
   */
  val NONE: ValueAdjustment = ofDeltaAmount(0d)

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance that replaces the base value.
   *
   * The base value is ignored when calculating the result, which is
   * `replacementValue`.
   *
   * @param replacementValue  the replacement value to use as the result of the adjustment
   * @return the adjustment, capturing the replacement value
   */
  def ofReplace(replacementValue: Double): ValueAdjustment =
    ValueAdjustment(replacementValue, ValueAdjustmentType.Replace)

  /**
   * Obtains an instance specifying an amount to add to the base value.
   *
   * The result will be `(baseValue + deltaAmount)`.
   *
   * @param deltaAmount  the amount to be added to the base value
   * @return the adjustment, capturing the delta amount
   */
  def ofDeltaAmount(deltaAmount: Double): ValueAdjustment =
    ValueAdjustment(deltaAmount, ValueAdjustmentType.DeltaAmount)

  /**
   * Obtains an instance specifying a multiplication factor, adding it to the base value.
   *
   * The result will be `(baseValue + baseValue * deltaMultiplier)`.
   *
   * @param deltaMultiplier  the multiplication factor to apply to the base amount, with the
   *   result added to the base amount
   * @return the adjustment, capturing the delta multiplier
   */
  def ofDeltaMultiplier(deltaMultiplier: Double): ValueAdjustment =
    ValueAdjustment(deltaMultiplier, ValueAdjustmentType.DeltaMultiplier)

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance specifying a multiplication factor to apply to the base value.
   *
   * The result will be `(baseValue * multiplier)`.
   *
   * @param multiplier  the multiplication factor to apply to the base amount
   * @return the adjustment
   */
  def ofMultiplier(multiplier: Double): ValueAdjustment =
    ValueAdjustment(multiplier, ValueAdjustmentType.Multiplier)

  //-------------------------------------------------------------------------
  /**
   * The hashing and equality of adjustments.
   *
   * Taken from the `equals` and `hashCode` of the type, which compare the modifying value by
   * its bit pattern and the type by its identity. This is the type's only equality-bearing
   * instance, and `Eq[ValueAdjustment]` is obtained from it by subtyping.
   *
   * @return the hashing of adjustments
   */
  implicit val hash: Hash[ValueAdjustment] = Hash.fromUniversalHashCode[ValueAdjustment]

  /**
   * The rendering of adjustments as text.
   *
   * Renders what `toString` renders, which describes the calculation and is the form of the
   * Java original, so the two ways of putting an adjustment into a message agree.
   *
   * @return the rendering of an adjustment
   */
  implicit val show: Show[ValueAdjustment] = Show.show(_.toString)

  //-------------------------------------------------------------------------
  // The codec of the double field, brought into scope for the derivations below and for
  // nothing else. It has to be taken from here rather than from the JSON library, whose
  // instance cannot express a value that is not a number; importing it at this point is what
  // makes that choice deliberate and local, as the codec support of `strata-collect`
  // intends. The codec of the type field needs no import: it is published by the companion of
  // `ValueAdjustmentType` and so is found for that type wherever it is needed.
  import Codecs.implicits.doubleCodec

  /**
   * The JSON encoding of adjustments.
   *
   * The encoding is derived when this file is compiled, so no part of it inspects a class
   * while the program runs. An instance encodes as an object holding its two fields under the
   * names the Java bean declared, in declaration order:
   *
   * {{{
   * {"modifyingValue":20.0,"type":"DeltaAmount"}
   * }}}
   *
   * The modifying value is written through the single policy of this port for doubles, so a
   * value that is not a number and the two infinities appear as the strings `"NaN"`,
   * `"Infinity"` and `"-Infinity"` while a finite value is written as a JSON number, exactly
   * to the bit. The type is written as the bare string of its canonical name. Neither field
   * is optional, so there is no absent value to drop from the output and the encoding needs
   * no post-processing.
   *
   * @return the JSON encoding of an adjustment
   */
  implicit val encoder: Encoder.AsObject[ValueAdjustment] = deriveEncoder[ValueAdjustment]

  /**
   * The JSON decoding of adjustments.
   *
   * This is the inverse of the encoding above and is likewise derived at compile time. Both
   * fields have to be present, the modifying value is read through the same policy that wrote
   * it, and the type is resolved by the name lookup of its own family, which accepts every
   * spelling that family accepts. Construction cannot fail, so nothing beyond the shape of
   * the payload is checked here: a payload of the right shape always yields an adjustment,
   * and an encoded adjustment decodes back to one equal to it - including one carrying a
   * modifying value that is not a number, which the bit-pattern equality of this type makes
   * equal to itself.
   *
   * @return the JSON decoding of an adjustment
   */
  implicit val decoder: Decoder[ValueAdjustment] = deriveDecoder[ValueAdjustment]
}
