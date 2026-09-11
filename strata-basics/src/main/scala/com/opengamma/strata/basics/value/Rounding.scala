/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.value

import java.math.BigDecimal
import java.math.RoundingMode

import cats.Hash
import cats.Show
import cats.syntax.apply._

import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.generic.semiauto.deriveDecoder

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.ValidatedFailures
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * A convention defining how to round a number.
 *
 * This defines a standard mechanism for rounding a `Double`, a `BigDecimal` or a [[Decimal]].
 * The standard implementation is [[HalfUp]], which rounds half away from zero, optionally to a
 * fraction of the smallest decimal place; [[NoRounding]] is the convention that changes
 * nothing.
 *
 * Note that rounding a `Double` is not straightforward, because a floating point number is
 * based on a binary representation rather than a decimal one: the value `0.1` cannot be
 * represented exactly in a `Double`. Rounding one therefore goes through the decimal the
 * shortest text of that `Double` names, which is what the Java original did and is why the
 * three overloads below give the answers they do rather than the answers the underlying
 * binary values would give.
 *
 * ===The three overloads===
 *
 * `round(BigDecimal)` is the operation each convention defines, and the other two are
 * expressed in terms of the same rule so that a caller gets the same answer whichever
 * representation it holds. `round(Double)` converts to a `BigDecimal`, rounds, and converts
 * back, exactly as the default method of the Java interface did. `round(Decimal)` is stated by
 * each member rather than inherited, because the arithmetic of [[Decimal]] is total while
 * building one from a `BigDecimal` is not, and this operation is required to stay total; the
 * consequences of that choice are documented on the decimal overload of [[HalfUp]].
 *
 * ===A closed hierarchy===
 *
 * The Java interface invited extension: "additional implementations may be added by
 * implementing this interface". This port deliberately drops that extensibility. `Rounding` is
 * a sealed family of exactly the two conventions the library ships, so every use of one is
 * checked by the compiler for exhaustiveness, the JSON form below is a closed set of shapes,
 * and no implementation can appear whose behaviour the test suite has not measured. A caller
 * that needs a different rule composes the two members with its own code rather than adding a
 * third member here. This is a documented divergence from the original.
 *
 * The other deliberate divergence is a rename: the Java class `HalfUpRounding` is [[HalfUp]]
 * here, because the name of the member is also the key of its JSON form and `{"HalfUp":{...}}`
 * is the form this port specifies. The factory names - `none`, `of`, `ofDecimalPlaces`,
 * `ofFractionalDecimalPlaces` - and the property names `decimalPlaces` and `fraction` are
 * unchanged, so a ported call site reads as it did before.
 *
 * ===Thread safety===
 *
 * Every implementation is immutable and holds nothing but two integers, so an instance is safe
 * to share between any number of threads without synchronisation.
 */
sealed trait Rounding {

  /**
   * Rounds the specified value according to the rules of the convention.
   *
   * This is the operation each convention defines; the other two overloads are expressed in
   * terms of the same rule.
   *
   * @param value  the value to be rounded
   * @return the rounded value
   */
  def round(value: BigDecimal): BigDecimal

  /**
   * Rounds the specified value according to the rules of the convention.
   *
   * The value is converted to a `BigDecimal`, rounded and converted back, which is what the
   * default method of the Java interface did, to the bit. The conversion is the one that reads
   * the shortest text naming the `Double`, so `12.345` rounds as the decimal `12.345` and not
   * as the slightly smaller binary value a `Double` actually holds.
   *
   * @param value  the value to be rounded
   * @return the rounded value
   */
  def round(value: Double): Double = round(BigDecimal.valueOf(value)).doubleValue()

  /**
   * Rounds the specified value according to the rules of the convention.
   *
   * This is stated by each member of the family rather than inherited, because the Java
   * default built its answer through a factory that this port reports failures from, and this
   * operation is required to stay total. Each member therefore rounds a decimal with the total
   * arithmetic of [[Decimal]] itself.
   *
   * @param value  the value to be rounded
   * @return the rounded value
   */
  def round(value: Decimal): Decimal
}

/**
 * The convention that makes no change to the value it is given.
 *
 * This is the instance [[Rounding.none]] returns, and it is the identity on all three
 * representations: the value handed in is the value handed back, the same object where the
 * representation is a reference. `round(Double)` is stated here rather than inherited, so a
 * value that needs no rounding is not put through a `BigDecimal` and back - the original made
 * the same choice, and it is what keeps this convention exact for a value that no decimal
 * names, such as one that is not a number or either infinity.
 *
 * Equality is the identity of a singleton, which is what the equality of the Java class - a
 * type test against a class with one instance - amounted to.
 */
case object NoRounding extends Rounding {

  /**
   * Returns the value unchanged.
   *
   * @param value  the value
   * @return the same value
   */
  override def round(value: BigDecimal): BigDecimal = value

  /**
   * Returns the value unchanged, without converting it to a decimal and back.
   *
   * @param value  the value
   * @return the same value, bit for bit
   */
  override def round(value: Double): Double = value

  /**
   * Returns the value unchanged.
   *
   * @param value  the value
   * @return the same value
   */
  override def round(value: Decimal): Decimal = value

  /**
   * Returns the text form of this convention, which is that of the Java original.
   *
   * @return `No rounding`
   */
  override def toString: String = "No rounding"
}

/**
 * The convention that rounds half away from zero, to a number of decimal places and
 * optionally to a fraction of the smallest of them.
 *
 * Rounding follows the `RoundingMode.HALF_UP` convention of the platform, which rounds a tie
 * away from zero. For example, this is how a price is brought to the number of decimal places
 * its market quotes in, and how a bond price is brought to the nearest thirty-second of a
 * point.
 *
 * The Java class this replaces was called `HalfUpRounding`; the name here is the shorter one
 * because it is also the key of the JSON form, `{"HalfUp":{...}}`. Both properties keep the
 * names the Java bean declared, in the same order.
 *
 * ===What the two fields mean===
 *
 * `decimalPlaces` is the number of decimal places to round to, from 0 to 255 inclusive.
 *
 * `fraction` is the fraction of the smallest decimal place to round to, from 0 to 256
 * inclusive, where 0 means there is no fractional part and rounding is to the decimal place
 * itself. Setting it to 32 rounds to the nearest one thirty-second of the last decimal place,
 * so `ofFractionalDecimalPlaces(4, 32)` rounds to the nearest 1/32nd of the fourth decimal
 * place. A fraction of 1 describes the same rounding as no fraction at all and is normalised
 * to 0 by the factories, which is why an instance never holds it.
 *
 * ===Construction===
 *
 * This is a validated type: the only way to obtain one is through a factory of its companion,
 * which reports every reason the inputs describe no convention rather than throwing. The
 * primary constructor is private and no `apply` or `copy` exists, so an instance outside the
 * documented ranges cannot be built, whether by a caller, by a copy of a valid instance, or by
 * decoding a document. Pattern matching is unaffected: `unapply` is available, so
 * `case HalfUp(places, fraction) => ...` reads the two fields.
 *
 * ===Equality===
 *
 * Two instances are equal when both fields are equal, which is the equality synthesised for
 * this type and needs no help: the Java original compared instances by a packed hash code,
 * `(decimalPlaces << 16) + fraction`, and no two distinct pairs within the permitted ranges
 * collide there, so comparing the fields is the same relation stated directly. Both fields are
 * integers, so the bit-pattern treatment that the double-bearing types of this port apply to
 * equality does not arise here. The Java hash code value itself is not reproduced, and nothing
 * requires it to be: a hash code is not part of the contract of the type, and the value the
 * platform computes for the pair is stable within a run and across runs.
 *
 * @param decimalPlaces  the number of decimal places to round to, from 0 to 255 inclusive
 * @param fraction  the fraction of the smallest decimal place to round to, from 0 to 256
 *   inclusive, 0 meaning there is no fractional part
 */
sealed abstract case class HalfUp private (decimalPlaces: Int, fraction: Int) extends Rounding {

  /**
   * The fraction as a `BigDecimal`, held once per instance rather than built once per call.
   *
   * The Java original held the same value in a transient field, and held nothing at all when
   * there was no fractional part. Nothing at all is not available to this port - no reference
   * here is ever absent - so the field holds the fraction it was given, which is zero when
   * there is no fractional part and is then never read. Building it is a lookup in the cache
   * of small values of the platform for every fraction up to ten, and one small allocation
   * beyond that, so holding it costs less than the alternative of building it per call.
   */
  private val fractionDecimal: BigDecimal = BigDecimal.valueOf(fraction.toLong)

  /**
   * Rounds the specified value half away from zero.
   *
   * Where there is a fractional part the value is scaled up by the fraction, rounded to the
   * requested number of decimal places, and scaled back down; otherwise the scale is simply
   * set. The operations and their order are those of the Java original, so this path agrees
   * with it to the bit - including the scale of the result, which `BigDecimal` equality
   * distinguishes: a value rounded to two decimal places comes back with a scale of exactly
   * two, padded with zeros if need be.
   *
   * @param value  the value to be rounded
   * @return the rounded value, at the scale the requested rounding implies
   * @throws ArithmeticException if there is a fractional part whose division does not
   *   terminate, which is the exact division the Java original performed
   */
  override def round(value: BigDecimal): BigDecimal =
    if (fraction > 1) {
      value
        .multiply(fractionDecimal)
        .setScale(decimalPlaces, RoundingMode.HALF_UP)
        .divide(fractionDecimal)
    } else {
      value.setScale(decimalPlaces, RoundingMode.HALF_UP)
    }

  /**
   * Rounds the specified decimal half away from zero.
   *
   * The rule is the one above, expressed in the arithmetic of [[Decimal]]: scale up by the
   * fraction, round to the requested number of decimal places, scale back down. Every step is
   * total, which is what allows this operation to keep the total signature of the Java
   * original. The Java default method instead rounded as a `BigDecimal` and rebuilt a decimal
   * from the result, a step that reports a failure in this port, so it could not be reused
   * here.
   *
   * Two consequences of taking the decimal route are deliberate and are recorded as
   * divergences of this port:
   *
   *   - where a fractional part leads to a division that does not terminate - a fraction of 3,
   *     for example - the exact division of the Java original raises an `ArithmeticException`,
   *     while the division of a decimal produces the quotient rounded to eighteen significant
   *     digits. The decimal route therefore answers where the `BigDecimal` route above
   *     refuses, and the two agree wherever the division terminates, which is every fraction
   *     the market conventions use.
   *   - a decimal holds a scale of at most eighteen and carries no trailing fractional zero,
   *     so a request for more decimal places than the value has leaves it unchanged rather
   *     than padding it. The `BigDecimal` route above pads to the requested scale, as the
   *     original did. The two values are numerically equal; they differ only in the scale, and
   *     of the two representations only `BigDecimal` distinguishes that in equality.
   *
   * @param value  the value to be rounded
   * @return the rounded value
   */
  override def round(value: Decimal): Decimal =
    if (fraction > 1) {
      value
        .multipliedBy(fraction.toLong)
        .roundToScale(decimalPlaces, RoundingMode.HALF_UP)
        .dividedBy(fraction.toLong)
    } else {
      value.roundToScale(decimalPlaces, RoundingMode.HALF_UP)
    }

  /**
   * Returns the text form of this convention, which is that of the Java original.
   *
   * The rendering names the rounding rather than the fields, so it reads as `Round to 4dp`, or
   * as `Round to 1/32 of 4dp` where there is a fractional part. This replaces the rendering
   * that would otherwise be synthesised for this type, which would name the class and its two
   * fields.
   *
   * @return the rendering of the rounding this convention performs
   */
  override def toString: String = {
    val fractionText = if (fraction > 1) s"1/$fraction of " else ""
    s"Round to $fractionText${decimalPlaces}dp"
  }
}

/**
 * The factories, validation and instance cache of [[HalfUp]].
 *
 * Every way of obtaining an instance is here, and each of the two public ones reports the
 * reasons its inputs describe no convention instead of throwing, which is what makes the type
 * impossible to hold in an invalid state. The messages are those of the Java original, word
 * for word, so a caller that logs one sees what it saw before.
 */
object HalfUp {

  /** The rejection of a number of decimal places outside the permitted range. */
  private val DecimalPlacesMessage: String = "Invalid decimal places, must be from 0 to 255 inclusive"

  /** The rejection of a fraction outside the permitted range. */
  private val FractionMessage: String = "Invalid fraction, must be from 0 to 256 inclusive"

  /** The largest number of decimal places a convention may round to. */
  private val MaxDecimalPlaces: Int = 255

  /** The largest fraction of the smallest decimal place a convention may round to. */
  private val MaxFraction: Int = 256

  /**
   * The cached instances, one per number of decimal places with no fractional part, for the
   * range of decimal places that trades actually use.
   *
   * The Java original cached the same sixteen instances, for the same reason: a rounding
   * convention is held by long-lived objects and asked for repeatedly, so a small fixed set of
   * them is worth keeping. The cache is an immutable vector built once, and it is invisible
   * from outside this object - two instances describing the same rounding are equal whether or
   * not either came from here, and they render identically - so nothing observable depends on
   * whether a given call was served from it.
   */
  private val Cache: Vector[HalfUp] = Vector.tabulate(16)(places => new HalfUp(places, 0) {})

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance that rounds to the specified number of decimal places.
   *
   * Rounding follows the normal `RoundingMode.HALF_UP` convention, with no fractional part.
   *
   * {{{
   * HalfUp.ofDecimalPlaces(4)   // Right(Round to 4dp)
   * HalfUp.ofDecimalPlaces(-1)  // Left(Invalid decimal places, must be from 0 to 255 inclusive)
   * }}}
   *
   * @param decimalPlaces  the number of decimal places to round to, from 0 to 255 inclusive
   * @return the rounding convention, or the failure describing why the input describes none
   */
  def ofDecimalPlaces(decimalPlaces: Int): ResultNec[HalfUp] =
    checkedDecimalPlaces(decimalPlaces)
      .map(places => create(places, 0))
      .toEither

  /**
   * Obtains an instance from the number of decimal places and the fraction.
   *
   * This returns a convention that rounds to a fraction of the specified number of decimal
   * places, following the normal `RoundingMode.HALF_UP` convention. For example, to round to
   * the nearest 1/32nd of the fourth decimal place, call this with the arguments 4 and 32.
   *
   * Both inputs are checked as they were given, and both failures are reported when both are
   * wrong, so one call tells a caller everything it has to correct:
   *
   * {{{
   * HalfUp.ofFractionalDecimalPlaces(4, 32)   // Right(Round to 1/32 of 4dp)
   * HalfUp.ofFractionalDecimalPlaces(4, 1)    // Right(Round to 4dp) - the fraction normalises to none
   * HalfUp.ofFractionalDecimalPlaces(-1, 257) // Left(two failures, one per input)
   * }}}
   *
   * The normalisation of a fraction of 0 or 1 to "no fractional part" is applied only after
   * both checks have passed, which is what makes a fraction of -1 or of 257 a failure rather
   * than something normalisation quietly absorbs. It is also idempotent: passing the fraction
   * of an instance back in yields an equal instance.
   *
   * @param decimalPlaces  the number of decimal places to round to, from 0 to 255 inclusive
   * @param fraction  the fraction of the smallest decimal place, such as 32 for 1/32, from 0
   *   to 256 inclusive
   * @return the rounding convention, or the failures describing why the inputs describe none
   */
  def ofFractionalDecimalPlaces(decimalPlaces: Int, fraction: Int): ResultNec[HalfUp] =
    (checkedDecimalPlaces(decimalPlaces), checkedFraction(fraction))
      .mapN((places, rawFraction) => create(places, normalised(rawFraction)))
      .toEither

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance that rounds to the specified number of decimal places, for a caller
   * that has already established the number is in range.
   *
   * This exists for [[Rounding.of]], whose argument is the number of minor units of a
   * currency. That number is 0, 2 or 3 for every currency of the closed family - no row of the
   * reference data carries anything else - so the range cannot be violated and the factory is
   * total, exactly as the Java method it replaces was. The check below states that invariant
   * rather than tests a possibility: it guards the contract of this method for any future
   * caller, and reaching it would mean the caller, not its data, is wrong. That is why it is a
   * fail-fast check rather than a reported failure.
   *
   * @param decimalPlaces  the number of decimal places to round to, which the caller has
   *   established is from 0 to 255 inclusive
   * @return the rounding convention
   * @throws IllegalArgumentException if the caller breaks that contract
   */
  private[value] def ofDecimalPlacesUnsafe(decimalPlaces: Int): HalfUp = {
    ArgCheck.inRangeInclusive(decimalPlaces, 0, MaxDecimalPlaces, "decimalPlaces")
    create(decimalPlaces, 0)
  }

  //-------------------------------------------------------------------------
  /**
   * Returns the instance for a pair that is already checked and normalised, from the cache
   * where the cache holds it.
   *
   * @param decimalPlaces  the checked number of decimal places
   * @param fraction  the checked and normalised fraction
   * @return the rounding convention
   */
  private def create(decimalPlaces: Int, fraction: Int): HalfUp =
    if (fraction == 0 && decimalPlaces >= 0 && decimalPlaces < Cache.size) {
      Cache(decimalPlaces)
    } else {
      new HalfUp(decimalPlaces, fraction) {}
    }

  /**
   * Normalises a checked fraction, mapping the two fractions that describe no fractional part
   * onto the one value that represents it.
   *
   * A fraction of 1 would scale the value by one before rounding and back afterwards, which is
   * the rounding performed with no fractional part at all; the original collapsed the two the
   * same way, so an instance never holds a fraction of 1.
   *
   * @param fraction  the checked fraction
   * @return 0 where the fraction describes no fractional part, otherwise the fraction
   */
  private def normalised(fraction: Int): Int = if (fraction <= 1) 0 else fraction

  /**
   * Checks that the number of decimal places is one a convention may round to.
   *
   * @param decimalPlaces  the number of decimal places to check
   * @return the number if it is in range, otherwise the failure of the Java original
   */
  private def checkedDecimalPlaces(decimalPlaces: Int): ValidatedFailures[Int] =
    Validate.cond(
      decimalPlaces >= 0 && decimalPlaces <= MaxDecimalPlaces,
      decimalPlaces,
      Failure.Invalid(DecimalPlacesMessage))

  /**
   * Checks that the fraction is one a convention may round to.
   *
   * @param fraction  the fraction to check, before normalisation
   * @return the fraction if it is in range, otherwise the failure of the Java original
   */
  private def checkedFraction(fraction: Int): ValidatedFailures[Int] =
    Validate.cond(
      fraction >= 0 && fraction <= MaxFraction,
      fraction,
      Failure.Invalid(FractionMessage))
}

/**
 * The factories, typeclass instances and JSON form of [[Rounding]].
 *
 * The four factories are those of the Java interface, under the same names. Two of them are
 * total - the convention that makes no change, and the one derived from a currency, whose
 * number of minor units is always in range - and the two that take a number of decimal places
 * from a caller report the reasons an input describes no convention, in the accumulating form
 * every validated factory of this port uses.
 *
 * Two typeclass instances are published, and exactly two: a `Hash`, which is the single
 * equality-bearing instance of the family - `Hash` extends `Eq`, so declaring an `Eq` as well
 * would leave two instances that could disagree and one of them ambiguous - and a `Show` that
 * renders what `toString` renders. There is deliberately no `Order`: the Java type is not
 * `Comparable`, and two conventions have no ordering worth inventing. The instances are
 * published for the family rather than for its members, so they apply to a value however it is
 * typed.
 */
object Rounding {

  /** The JSON key of the convention that makes no change. */
  private val NoRoundingKey: String = "NoRounding"

  /** The JSON key of the half-up convention. */
  private val HalfUpKey: String = "HalfUp"

  /** The JSON field holding the number of decimal places. */
  private val DecimalPlacesField: String = "decimalPlaces"

  /** The JSON field holding the fraction of the smallest decimal place. */
  private val FractionField: String = "fraction"

  /** The rejection of a document that is not one of the two shapes of the family. */
  private val UnknownShapeMessage: String =
    s"Rounding must be an object holding exactly one of '$NoRoundingKey' or '$HalfUpKey'"

  /** The rejection of a document whose no-rounding member carries anything but an empty object. */
  private val NoRoundingPayloadMessage: String =
    s"Rounding '$NoRoundingKey' must hold an empty object, as it has no fields"

  //-------------------------------------------------------------------------
  // The two members of the family are declared beside this object rather than inside it,
  // because Scala requires every direct subtype of a sealed type to be declared in the same
  // file and the file reads better with them at the top level - the Java original also had
  // three top-level types. These three aliases make each member reachable through this
  // companion as well, so `Rounding.HalfUp` and `HalfUp` name the same type and the same
  // object, and a call site may spell either. They introduce no new entity.

  /** The half-up convention, also reachable as `HalfUp`. */
  type HalfUp = com.opengamma.strata.basics.value.HalfUp

  /** The factories of the half-up convention, also reachable as `HalfUp`. */
  val HalfUp: com.opengamma.strata.basics.value.HalfUp.type = com.opengamma.strata.basics.value.HalfUp

  /** The convention that makes no change, also reachable as `NoRounding`. */
  val NoRounding: com.opengamma.strata.basics.value.NoRounding.type =
    com.opengamma.strata.basics.value.NoRounding

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance that performs no rounding.
   *
   * @return the rounding convention that makes no change
   */
  def none: Rounding = NoRounding

  /**
   * Obtains an instance that rounds to the number of minor units of the currency.
   *
   * This returns a convention that rounds for the specified currency, following the normal
   * `RoundingMode.HALF_UP` convention - two decimal places for a currency quoted in cents,
   * none for a currency with no minor unit:
   *
   * {{{
   * Rounding.of(Currency.USD).round(63.455)  // 63.46
   * }}}
   *
   * The result is total, as it was in the original: the number of minor units of a currency is
   * a small non-negative number for every currency of the closed family, so it is always a
   * number of decimal places a convention may round to.
   *
   * @param currency  the currency to round for
   * @return the rounding convention of the currency
   */
  def of(currency: Currency): Rounding = HalfUp.ofDecimalPlacesUnsafe(currency.minorUnitDigits)

  /**
   * Obtains an instance that rounds to the specified number of decimal places.
   *
   * This returns a convention that rounds to the specified number of decimal places, following
   * the normal `RoundingMode.HALF_UP` convention. Where the Java method threw, this reports the
   * reason as a value.
   *
   * @param decimalPlaces  the number of decimal places to round to, from 0 to 255 inclusive
   * @return the rounding convention, or the failure describing why the input describes none
   */
  def ofDecimalPlaces(decimalPlaces: Int): ResultNec[Rounding] = HalfUp.ofDecimalPlaces(decimalPlaces)

  /**
   * Obtains an instance from the number of decimal places and the fraction.
   *
   * This returns a convention that rounds to a fraction of the specified number of decimal
   * places, following the normal `RoundingMode.HALF_UP` convention. For example, to round to
   * the nearest 1/32nd of the fourth decimal place, call this with the arguments 4 and 32.
   * Where the Java method threw, this reports every reason as a value.
   *
   * @param decimalPlaces  the number of decimal places to round to, from 0 to 255 inclusive
   * @param fraction  the fraction of the smallest decimal place, such as 32 for 1/32, from 0
   *   to 256 inclusive
   * @return the rounding convention, or the failures describing why the inputs describe none
   */
  def ofFractionalDecimalPlaces(decimalPlaces: Int, fraction: Int): ResultNec[Rounding] =
    HalfUp.ofFractionalDecimalPlaces(decimalPlaces, fraction)

  //-------------------------------------------------------------------------
  /**
   * The hashing and equality of rounding conventions.
   *
   * Taken from the `equals` and `hashCode` of the members - the identity of a singleton for the
   * convention that makes no change, and the two fields for the half-up convention. This is the
   * family's only equality-bearing instance, and `Eq[Rounding]` is obtained from it by
   * subtyping.
   *
   * @return the hashing of rounding conventions
   */
  implicit val hash: Hash[Rounding] = Hash.fromUniversalHashCode[Rounding]

  /**
   * The rendering of rounding conventions as text.
   *
   * Renders what `toString` renders, which is the form of the Java original - `No rounding`,
   * `Round to 4dp`, `Round to 1/32 of 4dp` - so the two ways of putting a convention into a
   * message agree.
   *
   * @return the rendering of a rounding convention
   */
  implicit val show: Show[Rounding] = Show.show(_.toString)

  //-------------------------------------------------------------------------
  /**
   * The raw field shape the half-up decoder reads before validation.
   *
   * Decoding a validated type is two steps: read the fields, then hand them to the factory that
   * decides whether they describe a value. This product is the first step, and it exists only
   * for that purpose - it is private, it is never returned, and nothing but the decoder below
   * builds one.
   *
   * @param decimalPlaces  the number of decimal places, unvalidated
   * @param fraction  the fraction of the smallest decimal place, unvalidated
   */
  private final case class Raw(decimalPlaces: Int, fraction: Int)

  /** The derived decoder of the raw field shape, used by the validating decoder below. */
  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  /**
   * The encoder of the fields of the half-up convention, written out field by field because the
   * constructor of a validated type is not public and so its shape cannot be derived.
   */
  private val halfUpEncoder: Encoder[HalfUp] =
    Encoder.forProduct2[HalfUp, Int, Int](DecimalPlacesField, FractionField)(halfUp =>
      (halfUp.decimalPlaces, halfUp.fraction))

  /**
   * The decoder of the fields of the half-up convention, which validates them exactly as a
   * caller's arguments are validated, so a document naming a number of decimal places or a
   * fraction outside the permitted range is rejected rather than decoded.
   */
  private val halfUpDecoder: Decoder[HalfUp] =
    Codecs.validatedDecoder[Raw, HalfUp] { raw =>
      HalfUp.ofFractionalDecimalPlaces(raw.decimalPlaces, raw.fraction)
    }(rawDecoder)

  /**
   * The JSON encoding of rounding conventions.
   *
   * A convention is written as an object of one field, whose name is the member and whose value
   * holds that member's fields - the shape a closed family takes throughout this port:
   *
   * {{{
   * {"NoRounding":{}}
   * {"HalfUp":{"decimalPlaces":2,"fraction":0}}
   * }}}
   *
   * The encoding is written out here rather than derived, because the half-up convention is a
   * validated type whose constructor is not public; the shape produced is the one a derivation
   * would have produced. No field is optional, so there is nothing to drop from the output, and
   * the match over the closed family is checked by the compiler for exhaustiveness.
   *
   * @return the JSON encoding of a rounding convention
   */
  implicit val encoder: Encoder[Rounding] = Encoder.instance[Rounding] {
    case halfUp: HalfUp => Json.obj(HalfUpKey -> halfUpEncoder(halfUp))
    case NoRounding => Json.obj(NoRoundingKey -> Json.obj())
  }

  /**
   * The JSON decoding of rounding conventions.
   *
   * This is the inverse of the encoding above. The document has to be an object holding exactly
   * one field, whose name selects the member; a document holding no field, several fields, or a
   * field naming no member of the family is rejected, as is one that is not an object at all.
   *
   * The name of that one field selects the member, and the value of the field is then read as
   * that member's own payload rather than ignored, so a document is accepted only when both
   * halves of the shape match. The convention that makes no change has no fields, so its
   * payload is the empty object the encoder writes and nothing else: `{"NoRounding":{}}`
   * decodes, while a payload that is a number, a string, an array, the JSON literal denoting an
   * absent value, or an object carrying any field at all - `{"NoRounding":123}`,
   * `{"NoRounding":{"decimalPlaces":2}}` and their like - is rejected, the last of those because
   * a field it carries would otherwise be silently dropped. The fields of the half-up
   * convention are validated by its own factory, so an out-of-range document is a decoding
   * failure carrying every reason it was rejected for rather than a value the factory would
   * never have built, and a half-up payload that is not an object at all is rejected by that
   * same decoder.
   *
   * The history of a rejection is that of the cursor the failure was found at - the member
   * cursor for a payload that does not match its member, the document cursor for a shape that
   * names no member - so the position reported in a nested document points at the field that
   * was wrong.
   *
   * @return the JSON decoding of a rounding convention
   */
  implicit val decoder: Decoder[Rounding] = Decoder.instance { cursor =>
    cursor.keys.map(_.toList) match {
      case Some(NoRoundingKey :: Nil) =>
        val member = cursor.downField(NoRoundingKey)
        member.focus.flatMap(_.asObject) match {
          case Some(fields) if fields.isEmpty => Right(NoRounding)
          case _ => Left(DecodingFailure(NoRoundingPayloadMessage, member.history))
        }
      case Some(HalfUpKey :: Nil) => cursor.downField(HalfUpKey).as[HalfUp](halfUpDecoder)
      case _ => Left(DecodingFailure(UnknownShapeMessage, cursor.history))
    }
  }
}
