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
import io.circe.Encoder
import io.circe.Json
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.collect.ArgCheck
import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.NoJavaSerialization
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
 * represented exactly in a `Double`. Rounding one therefore goes through the decimal that the
 * shortest text of that `Double` names, which is why the three overloads below give the answers
 * they do rather than the answers the underlying binary values would give.
 *
 * ===The three overloads===
 *
 * `round(BigDecimal)` is the operation each convention defines, and the other two are
 * expressed in terms of the same rule so that a caller gets the same answer whichever
 * representation it holds. `round(Double)` converts to a `BigDecimal`, rounds, and converts
 * back. `round(Decimal)` is stated by each member rather than inherited, because the arithmetic
 * of [[Decimal]] is total while building one from a `BigDecimal` is not, and this operation is
 * required to stay total; the consequences of that choice are documented on the decimal
 * overload of [[HalfUp]].
 *
 * ===A closed family===
 *
 * `Rounding` is a sealed family of exactly the two conventions this library defines, so a match
 * over a convention is checked for exhaustiveness at compile time and the JSON form below is a
 * closed set of shapes. A caller that needs a different rule composes the two members with its
 * own code rather than adding a third member here.
 *
 * The name of each member is also the key of its JSON form, so a convention is written as
 * `{"NoRounding":{}}` or `{"HalfUp":{...}}`. The four factories are `none`, `of`,
 * `ofDecimalPlaces` and `ofFractionalDecimalPlaces`, and the two properties of the half-up
 * convention are `decimalPlaces` and `fraction`.
 *
 * ===Thread safety===
 *
 * Every implementation is immutable and holds nothing but two integers, so an instance is safe
 * to share between any number of threads without synchronisation.
 *
 * ===Serialization===
 *
 * A rounding convention is written as JSON through the codec this file publishes and in no other
 * form. Both members are a `case class` or a `case object`, which the compiler makes
 * `java.io.Serializable` whether or not the library wants it, so the family's root mixes in
 * [[NoJavaSerialization]]: [[NoRounding]] and [[HalfUp]] both refuse to be written to or read
 * from an object stream, and a pair of integers assembled by a stream rather than by a factory of
 * [[HalfUp]] cannot be presented as a rounding convention of this library.
 */
sealed trait Rounding extends NoJavaSerialization {

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
   * The value is converted to a `BigDecimal`, rounded and converted back. The conversion is the
   * one that reads the shortest text naming the `Double`, so `12.345` rounds as the decimal
   * `12.345` and not as the slightly smaller binary value a `Double` actually holds.
   *
   * @param value  the value to be rounded
   * @return the rounded value
   */
  def round(value: Double): Double = round(BigDecimal.valueOf(value)).doubleValue()

  /**
   * Rounds the specified value according to the rules of the convention.
   *
   * This is stated by each member of the family rather than inherited, because rounding through
   * a `BigDecimal` would have to build a [[Decimal]] back from the result, which is an operation
   * that reports a failure, and this one is required to stay total. Each member therefore rounds
   * a decimal with the total arithmetic of [[Decimal]] itself.
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
 * value that needs no rounding is not put through a `BigDecimal` and back, which is what keeps
 * this convention exact for a value that no decimal names, such as one that is not a number or
 * either infinity.
 *
 * Equality is the identity of a singleton: this convention has exactly one instance.
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
   * Returns the text form of this convention.
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
 * The name of this convention is also the key of its JSON form, `{"HalfUp":{...}}`, and both
 * properties appear there under their own names and in declaration order.
 *
 * ===What the two fields mean===
 *
 * `decimalPlaces` is the number of decimal places to round to, from 0 to 255 inclusive.
 *
 * `fraction` is the fraction of the smallest decimal place to round to, from 0 to 256
 * inclusive, where 0 means there is no fractional part and rounding is to the decimal place
 * itself. Setting it to 32 rounds to the nearest one thirty-second of the last decimal place,
 * so `ofFractionalDecimalPlaces(4, 32)` rounds to the nearest 1/32nd of the fourth decimal
 * place. A fraction of 1 asks for the same rounding as no fractional part at all and is
 * normalised to 0 by the factories, which is why an instance never holds it.
 *
 * ===Construction===
 *
 * This is a validated type: the only way to obtain one is through a factory of its companion,
 * and each public factory reports every bound its input breaks - the number of decimal places
 * must be from 0 to 255 inclusive and the fraction from 0 to 256 inclusive - rather than
 * raising. The primary constructor is private and no `apply` or `copy` exists, so an instance
 * outside those ranges cannot be built, whether by a caller, by a copy of a valid instance, or
 * by decoding a document. Pattern matching is unaffected: `unapply` is available, so
 * `case HalfUp(places, fraction) => ...` reads the two fields.
 *
 * ===Equality===
 *
 * Two instances are equal when both fields are equal, which is the equality synthesised for
 * this type and needs no help. Both fields are integers, so the bit-pattern treatment that the
 * double-bearing types of this library apply to equality does not arise here. The hash code is
 * whatever the platform computes for the pair; it is not part of the contract of the type, and
 * it is stable within a run and across runs.
 *
 * @param decimalPlaces  the number of decimal places to round to, from 0 to 255 inclusive
 * @param fraction  the fraction of the smallest decimal place to round to, from 0 to 256
 *   inclusive, 0 meaning there is no fractional part
 */
sealed abstract case class HalfUp private (decimalPlaces: Int, fraction: Int) extends Rounding {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means - which
  // could hold a number of decimal places or a fraction outside the documented ranges - can be
  // stopped is here. The single implementation is the companion's hidden `Impl`. The refusal of
  // Java serialization is inherited from [[Rounding]], which carries it for the whole family.
  JvmClosure.requireSoleImplementation(this, classOf[HalfUp.Impl])

  // The invariant of this convention, stated over the two numbers the instance actually holds
  // rather than over the arguments a factory was given, because the class file of the
  // implementation carries a public constructor whatever the source asked for: a caller compiled
  // outside this library can name that constructor directly, and the identity check above would
  // admit a convention rounding to a scale `BigDecimal.setScale` cannot reach or scaling by a
  // fraction no factory would accept. The two ranges are those of
  // [[HalfUp.ofDecimalPlaces]] and [[HalfUp.ofFractionalDecimalPlaces]], and the third statement
  // is the normalisation those factories apply: a fraction of 1 describes the same rounding as no
  // fractional part, so it is mapped to 0 before an instance is built and an instance never holds
  // it - which is what keeps two conventions that round identically equal to one another.
  JvmClosure.requireInvariant(
    "its number of decimal places is from 0 to 255 inclusive",
    decimalPlaces >= 0 && decimalPlaces <= HalfUp.MaxDecimalPlaces)
  JvmClosure.requireInvariant(
    "its fraction is from 0 to 256 inclusive",
    fraction >= 0 && fraction <= HalfUp.MaxFraction)
  JvmClosure.requireInvariant(
    "its fraction is normalised, a fraction of 1 being held as no fractional part",
    fraction != 1)

  /**
   * The fraction as a `BigDecimal`, held once per instance rather than built once per call.
   *
   * The field holds the fraction the instance was given, which is zero where there is no
   * fractional part and is then never read. Building it is a lookup in the cache of small values
   * of the platform for every fraction up to ten, and one small allocation beyond that, so
   * holding it costs less than the alternative of building it per call.
   */
  private val fractionDecimal: BigDecimal = BigDecimal.valueOf(fraction.toLong)

  /**
   * Rounds the specified value half away from zero.
   *
   * Where there is a fractional part the value is scaled up by the fraction, rounded to the
   * requested number of decimal places, and scaled back down; otherwise the scale is simply
   * set. The scale of the result is the number of decimal places requested, padded with zeros
   * if need be, which `BigDecimal` equality distinguishes: a value rounded to two decimal
   * places comes back with a scale of exactly two.
   *
   * @param value  the value to be rounded
   * @return the rounded value, at the scale the requested rounding implies
   * @throws ArithmeticException if there is a fractional part whose division does not
   *   terminate, the division being exact
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
   * total, which is what allows this operation to keep a total signature where rounding as a
   * `BigDecimal` and rebuilding a decimal from the result would not.
   *
   * Two consequences of taking the decimal route are deliberate:
   *
   *   - where a fractional part leads to a division that does not terminate - a fraction of 3,
   *     for example - the division of a decimal produces the quotient rounded to eighteen
   *     significant digits, so this overload answers where the `BigDecimal` route above
   *     refuses. The two agree wherever the division terminates, which is every fraction the
   *     market conventions use.
   *   - a decimal holds a scale of at most eighteen and carries no trailing fractional zero,
   *     so a request for more decimal places than the value has leaves it unchanged rather
   *     than padding it, where the `BigDecimal` route above pads to the requested scale. The
   *     two values are numerically equal; they differ only in the scale, and of the two
   *     representations only `BigDecimal` distinguishes that in equality.
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
   * Returns the text form of this convention.
   *
   * The rendering names the rounding rather than the fields, so it reads as `Round to 4dp`, or
   * as `Round to 1/32 of 4dp` where there is a fractional part. It is stated here rather than
   * left to the rendering that would otherwise be synthesised for this type, which would name
   * the class and its two fields.
   *
   * @return the rendering of the rounding this convention performs
   */
  override def toString: String = {
    val fractionText = if (fraction > 1) s"1/$fraction of " else ""
    s"Round to $fractionText${decimalPlaces}dp"
  }
}

/**
 * The factories, validation, instance cache and typeclass instances of [[HalfUp]].
 *
 * Every way of obtaining an instance is here, and each of the two public ones reports every
 * bound its inputs break - the number of decimal places must be from 0 to 255 inclusive and the
 * fraction from 0 to 256 inclusive - instead of raising, which is what makes the type impossible
 * to hold in an invalid state.
 *
 * The two typeclass instances at the foot of this object - a `Hash` and a `Show` - are the
 * family's instances restated at the type of the member, which is what the invariance of those
 * typeclasses requires; [[Rounding]] keeps its own pair for a value typed as the family, and the
 * two agree by construction.
 */
object HalfUp {

  private val DecimalPlacesMessage: String = "Invalid decimal places, must be from 0 to 255 inclusive"

  private val FractionMessage: String = "Invalid fraction, must be from 0 to 256 inclusive"

  private val MaxDecimalPlaces: Int = 255

  private val MaxFraction: Int = 256

  /**
   * The cached instances, one per number of decimal places with no fractional part, for the
   * range of decimal places that trades actually use.
   *
   * A rounding convention is held by long-lived objects and asked for repeatedly, so a small
   * fixed set of them is worth keeping. The cache is an immutable vector built once, and it is
   * invisible from outside this object - two instances describing the same rounding are equal
   * whether or not either came from here, and they render identically - so nothing observable
   * depends on whether a given call was served from it.
   */
  private val Cache: Vector[HalfUp] = Vector.tabulate(16)(places => new Impl(places, 0))

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
   * @return the rounding convention, or the failure naming the bound the input breaks: the
   *   number of decimal places must be from 0 to 255 inclusive
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
   * @return the rounding convention, or one failure for each bound the inputs break: the number
   *   of decimal places must be from 0 to 255 inclusive and the fraction from 0 to 256 inclusive
   */
  def ofFractionalDecimalPlaces(decimalPlaces: Int, fraction: Int): ResultNec[HalfUp] =
    (checkedDecimalPlaces(decimalPlaces), checkedFraction(fraction))
      .mapN((places, rawFraction) => create(places, normalised(rawFraction)))
      .toEither

  /**
   * Obtains an instance that rounds to the specified number of decimal places, for a caller
   * that has already established the number is in range.
   *
   * This exists for [[Rounding.of]], whose argument is the number of minor units of a
   * currency. That number is 0, 2 or 3 for every currency of the closed family - no row of the
   * built-in currency data carries anything else - so the range cannot be violated and the
   * factory is total. The check below states that invariant: it guards the contract of this
   * method for any later caller, and reaching it would mean the caller, not its data, is wrong,
   * which is why it fails fast rather than reporting a failure.
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
      new Impl(decimalPlaces, fraction)
    }

  /**
   * The one implementation of this rounding convention.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it - the single one, built both by [[create]] and by the cache it consults. It is declared
   * rather than written as an anonymous subclass at either instantiation site for two reasons,
   * both about what the class file says: a private member class is one a Java compiler refuses to
   * name, where an anonymous class is public and can be instantiated directly by a caller in
   * another language, and a named class can be compared against, which is what lets [[HalfUp]]
   * refuse in its own constructor to be any other implementation.
   *
   * @param decimalPlaces  the number of decimal places, already checked to be in range
   * @param fraction  the fraction, already checked to be in range and normalised
   */
  private final class Impl(decimalPlaces: Int, fraction: Int)
      extends HalfUp(decimalPlaces, fraction)

  /**
   * Normalises a checked fraction, mapping the two fractions that ask for no fractional part
   * onto the one value that represents it.
   *
   * A fraction of 1 would scale the value by one before rounding and back afterwards, which is
   * the rounding performed with no fractional part at all, so the two collapse and an instance
   * never holds a fraction of 1.
   *
   * @param fraction  the checked fraction
   * @return 0 where the fraction asks for no fractional part, otherwise the fraction
   */
  private def normalised(fraction: Int): Int = if (fraction <= 1) 0 else fraction

  /**
   * Checks that the number of decimal places is one a convention may round to.
   *
   * @param decimalPlaces  the number of decimal places to check
   * @return the number if it is from 0 to 255 inclusive, otherwise the failure naming that range
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
   * @return the fraction if it is from 0 to 256 inclusive, otherwise the failure naming that
   *   range
   */
  private def checkedFraction(fraction: Int): ValidatedFailures[Int] =
    Validate.cond(
      fraction >= 0 && fraction <= MaxFraction,
      fraction,
      Failure.Invalid(FractionMessage))

  /**
   * The hashing and equality of half-up rounding conventions.
   *
   * This is the same equality [[Rounding.hash]] offers - the universal `equals` and `hashCode` of
   * the value, which for this member are its two fields - declared a second time at this type
   * because `cats.Hash` is '''invariant''': `Hash[Rounding]` is not a `Hash[HalfUp]`, so a caller
   * holding a value typed as the member rather than as the family could not summon one from the
   * family's instance. Both declarations are therefore needed, and neither is ambiguous with the
   * other: a summon at `Rounding` can only be answered by the family's instance and a summon at
   * `HalfUp` only by this one, and because both are `Hash.fromUniversalHashCode` the two can never
   * disagree about a value they both see.
   *
   * @return the hashing of half-up rounding conventions
   */
  implicit val hash: Hash[HalfUp] = Hash.fromUniversalHashCode[HalfUp]

  /**
   * The rendering of half-up rounding conventions as text.
   *
   * Renders what `toString` renders - `Round to 4dp`, or `Round to 1/32 of 4dp` where there is a
   * fractional part - so the two ways of putting a convention into a message agree whether the
   * value is typed as the member or as the family. Declared here for the same reason the hashing
   * above is: `cats.Show` is invariant as well.
   *
   * @return the rendering of a half-up rounding convention
   */
  implicit val show: Show[HalfUp] = Show.show(_.toString)
}

/**
 * The factories, typeclass instances and JSON form of [[Rounding]].
 *
 * There are four factories. Two of them are total - the convention that makes no change, and
 * the one derived from a currency, whose number of minor units is always a number of decimal
 * places a convention may round to - and the two that take a number of decimal places from a
 * caller report every bound the input breaks, accumulating them.
 *
 * Two typeclass instances are published, and exactly two: a `Hash`, which is the single
 * equality-bearing instance of the family - `Hash` extends `Eq`, so declaring an `Eq` as well
 * would leave two instances that could disagree and one of them ambiguous - and a `Show` that
 * renders what `toString` renders. There is deliberately no `Order`: two conventions have no
 * ordering worth inventing. The instances are published for the family rather than for its
 * members, so they apply to a value however it is typed.
 */
object Rounding {

  private val NoRoundingKey: String = "NoRounding"

  private val HalfUpKey: String = "HalfUp"

  private val UnknownShapeMessage: String =
    s"Rounding must be an object holding exactly one of '$NoRoundingKey' or '$HalfUpKey'"

  private val NoRoundingPayloadMessage: String =
    s"Rounding '$NoRoundingKey' must hold an empty object, as it has no fields"

  // The two members of the family are declared beside this object rather than inside it,
  // because Scala requires every direct subtype of a sealed type to be declared in the same
  // file, and the file reads better with them at the top level. These three aliases make each
  // member reachable through this companion as well, so `Rounding.HalfUp` and `HalfUp` name the
  // same type and the same object, and a call site may spell either. They introduce no new
  // entity.

  /** The half-up convention, also reachable as `HalfUp`. */
  type HalfUp = com.opengamma.strata.basics.value.HalfUp

  /** The factories of the half-up convention, also reachable as `HalfUp`. */
  val HalfUp: com.opengamma.strata.basics.value.HalfUp.type = com.opengamma.strata.basics.value.HalfUp

  /** The convention that makes no change, also reachable as `NoRounding`. */
  val NoRounding: com.opengamma.strata.basics.value.NoRounding.type =
    com.opengamma.strata.basics.value.NoRounding

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
   * The result is total: the number of minor units of a currency is a small non-negative number
   * for every currency of the closed family, so it is always a number of decimal places a
   * convention may round to.
   *
   * @param currency  the currency to round for
   * @return the rounding convention of the currency
   */
  def of(currency: Currency): Rounding = HalfUp.ofDecimalPlacesUnsafe(currency.minorUnitDigits)

  /**
   * Obtains an instance that rounds to the specified number of decimal places.
   *
   * This returns a convention that rounds to the specified number of decimal places, following
   * the normal `RoundingMode.HALF_UP` convention.
   *
   * @param decimalPlaces  the number of decimal places to round to, from 0 to 255 inclusive
   * @return the rounding convention, or the failure naming the bound the input breaks: the
   *   number of decimal places must be from 0 to 255 inclusive
   */
  def ofDecimalPlaces(decimalPlaces: Int): ResultNec[Rounding] = HalfUp.ofDecimalPlaces(decimalPlaces)

  /**
   * Obtains an instance from the number of decimal places and the fraction.
   *
   * This returns a convention that rounds to a fraction of the specified number of decimal
   * places, following the normal `RoundingMode.HALF_UP` convention. For example, to round to
   * the nearest 1/32nd of the fourth decimal place, call this with the arguments 4 and 32.
   *
   * @param decimalPlaces  the number of decimal places to round to, from 0 to 255 inclusive
   * @param fraction  the fraction of the smallest decimal place, such as 32 for 1/32, from 0
   *   to 256 inclusive
   * @return the rounding convention, or one failure for each bound the inputs break: the number
   *   of decimal places must be from 0 to 255 inclusive and the fraction from 0 to 256 inclusive
   */
  def ofFractionalDecimalPlaces(decimalPlaces: Int, fraction: Int): ResultNec[Rounding] =
    HalfUp.ofFractionalDecimalPlaces(decimalPlaces, fraction)

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
   * Renders what `toString` renders - `No rounding`, `Round to 4dp`, `Round to 1/32 of 4dp` -
   * so the two ways of putting a convention into a message agree.
   *
   * @return the rendering of a rounding convention
   */
  implicit val show: Show[Rounding] = Show.show(_.toString)

  /**
   * The raw field shape both instances of the half-up member are derived over.
   *
   * A type whose constructor is not public cannot be derived over directly, and decoding a
   * validated type is two steps: read the fields, then hand them to the factory that decides
   * whether they describe a value. This product is the shape those fields have, and both
   * directions of the member below go through it. Its field names are the JSON keys, and they
   * are the names of the two properties of the member, in that order, which is what keeps the
   * derived shape and the type from drifting apart. It exists only for that purpose: it is
   * private, it is never returned, and nothing but the two instances below builds or reads one.
   *
   * The shape is `java.io.Serializable`, because the compiler makes every `case class` so, and it
   * therefore mixes in [[NoJavaSerialization]] as every product of this port does: these fields
   * reach the library as JSON through the codecs below and in no other form.
   *
   * @param decimalPlaces  the number of decimal places, unvalidated
   * @param fraction  the fraction of the smallest decimal place, unvalidated
   */
  private final case class Raw(decimalPlaces: Int, fraction: Int) extends NoJavaSerialization

  private val rawDecoder: Decoder[Raw] = deriveDecoder[Raw]

  private val rawEncoder: Encoder[Raw] = deriveEncoder[Raw]

  /**
   * The encoding of the fields of the half-up convention.
   *
   * The shape is derived over the raw product above and a convention is contramapped into it,
   * which is the idiom for a type whose constructor is not public - derive the product of the
   * fields, then state how a value maps onto it - so the keys and their order come from one
   * place and are not written out a second time here.
   *
   * The instance is private, and deliberately so. The codec this library publishes is the
   * family's; the two member instances exist for the derivations below to find and add nothing
   * to the public surface. Anything holding a convention holds it at the type of the family,
   * which is the type the published codec covers.
   */
  private implicit val halfUpEncoder: Encoder[HalfUp] =
    rawEncoder.contramap[HalfUp](halfUp => Raw(halfUp.decimalPlaces, halfUp.fraction))

  /**
   * The decoding of the fields of the half-up convention, which validates them exactly as a
   * caller's arguments are validated, so a document naming a number of decimal places or a
   * fraction outside the permitted range is rejected rather than decoded, and one whose two
   * fields are both wrong is rejected for both reasons at once.
   *
   * It is private for the same reason its counterpart above is.
   */
  private implicit val halfUpDecoder: Decoder[HalfUp] =
    Codecs.validatedDecoder[Raw, HalfUp] { raw =>
      HalfUp.ofFractionalDecimalPlaces(raw.decimalPlaces, raw.fraction)
    }(rawDecoder)

  /**
   * The encoding of the fields of the convention that makes no change, which is derived and
   * writes the empty object, that convention having no field to write.
   *
   * It is private for the same reason the two instances above are.
   */
  private implicit val noRoundingEncoder: Encoder[com.opengamma.strata.basics.value.NoRounding.type] =
    deriveEncoder[com.opengamma.strata.basics.value.NoRounding.type]

  /**
   * The decoding of the fields of the convention that makes no change, which accepts the empty
   * object its encoding writes and nothing else.
   *
   * This one payload is stated rather than derived, because a derivation would accept any
   * object at all: a member with no field reads no field, so every field a document carried
   * would be ignored and `{"NoRounding":{"decimalPlaces":2}}` would decode to this convention
   * while what it asked for was silently discarded. The payload is therefore required to be an
   * object holding nothing, and a number, a string, an array, the literal denoting an absent
   * value or an object carrying any field is a decoding failure naming the shape this member
   * has.
   *
   * It is private for the same reason the instances above are.
   */
  private implicit val noRoundingDecoder: Decoder[com.opengamma.strata.basics.value.NoRounding.type] =
    Decoder[Json].emap(payload =>
      payload.asObject.filter(_.isEmpty).map(_ => NoRounding).toRight(NoRoundingPayloadMessage))

  /**
   * The JSON encoding of rounding conventions.
   *
   * A convention is written as an object of one field, whose name is the member and whose value
   * holds that member's fields, which is the shape every closed family takes here:
   *
   * {{{
   * {"NoRounding":{}}
   * {"HalfUp":{"decimalPlaces":2,"fraction":0}}
   * }}}
   *
   * The wrapper and both payloads are derived when this file is compiled, from the family and
   * from the member instances above, so no part of the encoding inspects a class while the
   * program runs and the keys are stated in one place each. The result is wrapped so that a
   * field holding no value would be omitted, which is the policy every product of this library
   * follows - no field of either member is optional, so the wrapping changes nothing about the
   * bytes of this type and exists so that the policy holds without exception.
   *
   * @return the JSON encoding of a rounding convention
   */
  implicit val encoder: Encoder[Rounding] = Codecs.dropNulls(deriveEncoder[Rounding])

  /**
   * The derived decoding of the family, reached once the gate below has accepted the shape of
   * the document.
   *
   * It reads the one field of the wrapper, selects the member its name denotes, and hands the
   * value of that field to the member's own decoder above, so a document is accepted only when
   * both halves of the shape match: the fields of the half-up convention are validated by its
   * own factory, and the convention that makes no change requires the empty object.
   */
  private val derivedDecoder: Decoder[Rounding] = deriveDecoder[Rounding]

  /**
   * The gate that states the shape of the family before any member is read.
   *
   * The derivation alone would accept a document carrying both member keys, taking whichever it
   * examined first, and would answer a document naming no member with a message phrased in
   * terms of the representation the derivation is built on rather than in terms of this family.
   * This gate reads the document as a whole first and requires an object holding exactly one
   * field whose name is one of the two members; anything else - no field, several fields, a
   * field naming no member, or a document that is not an object at all - is refused here,
   * against the document cursor, with the message the family states for its own shape.
   */
  private val shapeGate: Decoder[Unit] =
    Decoder[Json].emap { document =>
      document.asObject.map(_.keys.toList) match {
        case Some(NoRoundingKey :: Nil) | Some(HalfUpKey :: Nil) => Right(())
        case _ => Left(UnknownShapeMessage)
      }
    }

  /**
   * The JSON decoding of rounding conventions.
   *
   * This is the inverse of the encoding above: the shape gate accepts the wrapper, and the
   * derived decoding then reads the same document and builds the member the one field names.
   * Both steps run against the same cursor, the second only if the first accepted it, so the
   * document is examined twice and no copy of it is made for the gate.
   *
   * A shape that names no member of the family is rejected by the gate, and a payload that
   * does not match the member its key names is rejected by that member's decoder: an
   * out-of-range half-up document is a decoding failure carrying every reason its fields were
   * rejected for rather than a value the factory would never have built, a half-up payload
   * that is not an object at all names no field to validate and is refused on those grounds,
   * and the convention that makes no change accepts `{"NoRounding":{}}` and nothing else.
   *
   * The history of a rejection is that of the cursor the failure was found at - the member
   * cursor for a payload that does not match its member, the document cursor for a shape that
   * names no member - so the position reported in a nested document points at the field that
   * was wrong.
   *
   * @return the JSON decoding of a rounding convention
   */
  implicit val decoder: Decoder[Rounding] = shapeGate.flatMap(_ => derivedDecoder)
}
