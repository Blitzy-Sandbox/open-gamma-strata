/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import java.math.MathContext
import java.math.RoundingMode
import java.math.{BigDecimal => JavaBigDecimal}

import scala.annotation.tailrec
import scala.util.Try

import cats.Hash
import cats.Order
import cats.Show
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder

import com.opengamma.strata.collect.result.Failure

/**
 * A decimal number of up to 18 significant digits, held exactly.
 *
 * A decimal is an unscaled integer paired with a scale, the value being the unscaled integer
 * divided by ten raised to the scale. Both parts are primitives, so a decimal is one small
 * object with no arrays and no boxed fields, which is what makes it cheap enough to use for
 * money and for rates where a `Double` cannot represent the value the market quotes and a
 * `BigDecimal` costs more than the precision is worth.
 *
 * ===Normalisation===
 *
 * Every decimal is normalised: trailing zeroes of the fractional part are removed, so `12.30`
 * and `12.3` are the same value with the same scale, and zero has scale zero however it
 * arose. Normalisation is what makes equality, hashing and comparison agree with one another
 * - two decimals are equal exactly when they compare equal - and it is why the factories
 * rather than a constructor are the way to build one.
 *
 * The scale is in the range 0 to 18 and the precision never exceeds 18 digits. A factory
 * given more precision than that truncates towards zero, which is the behaviour of the type
 * being ported; a factory given a value too large to hold at all reports a failure.
 *
 * ===Failure and the two throwing edges===
 *
 * Building a decimal from data - a `Double`, a `BigDecimal`, an unscaled value and scale, or
 * text - can fail on the data supplied, so those factories answer with
 * `Either[Failure, Decimal]` and never raise. Arithmetic keeps the total signature of the
 * type being ported and reports overflow beyond 18 digits through `ArgCheck`, as a broken
 * precondition rather than as a value, because a sum that does not fit is a fact about the
 * program rather than about its input. The same applies to the argument checks of
 * `roundToScale`, `roundToPrecision`, `format` and `dividedBy`. Those are the only two ways
 * this type can fail, and which one applies is stated on every method.
 *
 * @param unscaledValue  the unscaled value, with no trailing fractional zeroes
 * @param scale  the scale, from 0 to 18
 */
sealed abstract case class Decimal private (unscaledValue: Long, scale: Int) {

  private def toJavaBigDecimal: JavaBigDecimal = JavaBigDecimal.valueOf(unscaledValue, scale)

  /**
   * Returns the sign of this decimal.
   *
   * The sign is read from the unscaled value, so the scale does not affect it.
   *
   * @return -1 when this decimal is negative, 0 when it is zero, 1 when it is positive
   */
  def signum: Int = java.lang.Long.signum(unscaledValue)

  /**
   * Returns true when this decimal is zero.
   *
   * @return true when this decimal is zero
   */
  def isZero: Boolean = unscaledValue == 0L

  /**
   * Returns this decimal negated.
   *
   * @return the decimal with the same magnitude and the opposite sign
   */
  def negated: Decimal = Decimal.create(-unscaledValue, scale)

  /**
   * Returns the absolute value of this decimal.
   *
   * @return this decimal when it is zero or positive, its negation when it is negative
   */
  def abs: Decimal = if (signum < 0) negated else this

  /**
   * Returns this decimal plus the specified decimal.
   *
   * The result is exact unless it needs more than 18 digits, in which case it is truncated
   * towards zero.
   *
   * @param other  the decimal to add
   * @return the sum
   * @throws IllegalArgumentException if the sum does not fit in 18 digits at scale zero
   */
  def plus(other: Decimal): Decimal =
    Decimal.orFail(Decimal.fromJavaBigDecimal(toJavaBigDecimal.add(other.toJavaBigDecimal)))

  /**
   * Returns this decimal minus the specified decimal.
   *
   * The result is exact unless it needs more than 18 digits, in which case it is truncated
   * towards zero.
   *
   * @param other  the decimal to subtract
   * @return the difference
   * @throws IllegalArgumentException if the difference does not fit in 18 digits at scale zero
   */
  def minus(other: Decimal): Decimal =
    Decimal.orFail(Decimal.fromJavaBigDecimal(toJavaBigDecimal.subtract(other.toJavaBigDecimal)))

  /**
   * Returns this decimal multiplied by the specified decimal.
   *
   * The result is exact unless it needs more than 18 digits, in which case it is truncated
   * towards zero.
   *
   * @param other  the decimal to multiply by
   * @return the product
   * @throws IllegalArgumentException if the product does not fit in 18 digits at scale zero
   */
  def multipliedBy(other: Decimal): Decimal =
    Decimal.orFail(Decimal.fromJavaBigDecimal(toJavaBigDecimal.multiply(other.toJavaBigDecimal)))

  /**
   * Returns this decimal divided by the specified decimal, truncating towards zero.
   *
   * @param other  the decimal to divide by
   * @return the quotient, to 18 significant digits
   * @throws IllegalArgumentException if the divisor is zero
   */
  def dividedBy(other: Decimal): Decimal = dividedBy(other, RoundingMode.DOWN)

  /**
   * Returns this decimal divided by the specified decimal, rounding as instructed.
   *
   * @param other  the decimal to divide by
   * @param roundingMode  the rounding to apply to the quotient
   * @return the quotient, to 18 significant digits
   * @throws IllegalArgumentException if the divisor is zero
   */
  def dividedBy(other: Decimal, roundingMode: RoundingMode): Decimal = {
    ArgCheck.isTrue(!other.isZero, "Decimal division by zero")
    val quotient = toJavaBigDecimal.divide(other.toJavaBigDecimal, new MathContext(Decimal.MaxPrecision, roundingMode))
    Decimal.orFail(Decimal.fromJavaBigDecimal(quotient))
  }

  /**
   * Returns this decimal rounded to the specified scale.
   *
   * A scale at or above the scale of this decimal leaves it unchanged, since there is
   * nothing to remove. A negative scale rounds above the decimal point, so a scale of -2
   * rounds to a multiple of one hundred.
   *
   * @param desiredScale  the scale to round to, from -17 to 18
   * @param roundingMode  the rounding to apply
   * @return the rounded decimal
   * @throws IllegalArgumentException if the scale is -18 or less
   */
  def roundToScale(desiredScale: Int, roundingMode: RoundingMode): Decimal = {
    ArgCheck.isTrue(desiredScale > -Decimal.MaxScale, s"Rounding scale must not be -18 or less: $desiredScale")
    if (isZero) {
      Decimal.ZERO
    } else if (desiredScale >= scale) {
      this
    } else {
      val adjustedScale = math.min(desiredScale, Decimal.MaxScale)
      Decimal.orFail(Decimal.fromJavaBigDecimal(toJavaBigDecimal.setScale(adjustedScale, roundingMode)))
    }
  }

  /**
   * Returns this decimal rounded to the specified number of significant digits.
   *
   * @param precision  the number of significant digits to keep, from 1 to 18
   * @param roundingMode  the rounding to apply
   * @return the rounded decimal
   * @throws IllegalArgumentException if the precision is outside the range 1 to 18
   */
  def roundToPrecision(precision: Int, roundingMode: RoundingMode): Decimal = {
    ArgCheck.inRangeInclusive(precision, 1, Decimal.MaxPrecision, "precision")
    val rounded = toJavaBigDecimal.round(new MathContext(precision, roundingMode))
    Decimal.orFail(Decimal.fromJavaBigDecimal(rounded))
  }

  /**
   * Compares this decimal with the specified decimal.
   *
   * Since every decimal is normalised, two decimals compare equal only when they are equal,
   * so the comparison agrees with equality.
   *
   * @param other  the decimal to compare with
   * @return a negative number, zero or a positive number as this decimal is less than, equal
   *   to or greater than the other
   */
  def compareTo(other: Decimal): Int = toJavaBigDecimal.compareTo(other.toJavaBigDecimal)

  /**
   * Returns true when this decimal is greater than the specified decimal.
   *
   * @param other  the decimal to compare with
   * @return true when this decimal is the greater
   */
  def isGreaterThan(other: Decimal): Boolean = compareTo(other) > 0

  /**
   * Returns true when this decimal is less than the specified decimal.
   *
   * @param other  the decimal to compare with
   * @return true when this decimal is the lesser
   */
  def isLessThan(other: Decimal): Boolean = compareTo(other) < 0

  /**
   * Converts this decimal to a `BigDecimal`, exactly.
   *
   * @return the equivalent `BigDecimal`
   */
  def toBigDecimal: BigDecimal = BigDecimal.exact(toJavaBigDecimal)

  /**
   * Converts this decimal to a `Double`, which may lose precision.
   *
   * @return the nearest `Double` to this decimal
   */
  def toDouble: Double = toJavaBigDecimal.doubleValue

  /**
   * Converts this decimal to a `Long`, discarding any fractional part.
   *
   * @return this decimal truncated towards zero
   */
  def toLong: Long = toJavaBigDecimal.longValue

  /**
   * Formats this decimal with at least the specified number of decimal places.
   *
   * Where this decimal has more decimal places than requested they are all shown, since
   * dropping them would change the value shown; where it has fewer, the fraction is padded
   * with zeroes.
   *
   * @param minDecimalPlaces  the minimum number of decimal places, from 0 to 18
   * @return the formatted decimal
   * @throws IllegalArgumentException if the number of decimal places is outside 0 to 18
   */
  def formatAtLeast(minDecimalPlaces: Int): String = {
    ArgCheck.inRangeInclusive(minDecimalPlaces, 0, Decimal.MaxScale, "minDecimalPlaces")
    toJavaBigDecimal.setScale(math.max(minDecimalPlaces, scale)).toPlainString
  }

  /**
   * Formats this decimal with exactly the specified number of decimal places.
   *
   * @param decimalPlaces  the number of decimal places, from 0 to 18
   * @param roundingMode  the rounding to apply when this decimal has more decimal places
   * @return the formatted decimal
   * @throws IllegalArgumentException if the number of decimal places is outside 0 to 18
   */
  def format(decimalPlaces: Int, roundingMode: RoundingMode): String = {
    ArgCheck.inRangeInclusive(decimalPlaces, 0, Decimal.MaxScale, "decimalPlaces")
    toJavaBigDecimal.setScale(decimalPlaces, roundingMode).toPlainString
  }

  /**
   * Returns the canonical text of this decimal.
   *
   * The form is the plain decimal form, with as many decimal places as the scale and never
   * an exponent, so `12.3`, `-0.5` and `4`. This is the form `parse` reads back and the form
   * the JSON codec writes, which makes it the single textual representation of the type.
   *
   * @return the canonical text of this decimal
   */
  override def toString: String = toJavaBigDecimal.toPlainString
}

/**
 * Provides the factories, constants and instances for decimals.
 */
object Decimal {

  /** The maximum number of significant digits a decimal holds. */
  private[collect] val MaxPrecision: Int = 18

  /** The maximum scale a decimal holds. */
  private[collect] val MaxScale: Int = 18

  private val MaxUnscaled: Long = 999999999999999999L

  private val Powers: Array[Long] = Array.iterate(1L, MaxScale + 1)(_ * 10L)

  private val TruncatingContext: MathContext = new MathContext(MaxPrecision, RoundingMode.DOWN)

  /** The decimal zero. */
  val ZERO: Decimal = create(0L, 0)

  /** The largest decimal, 18 nines at scale zero. */
  val MAX_VALUE: Decimal = create(MaxUnscaled, 0)

  /** The smallest decimal, the negation of `MAX_VALUE`. */
  val MIN_VALUE: Decimal = create(-MaxUnscaled, 0)

  /**
   * Obtains a decimal from a `Long`.
   *
   * @param value  the value
   * @return the equivalent decimal, or the failure describing why the value cannot be held
   */
  def of(value: Long): Either[Failure, Decimal] = ofScaled(value, 0)

  /**
   * Obtains a decimal from a `Double`.
   *
   * The decimal obtained is the one the shortest text of the `Double` names, so `0.1`
   * becomes the decimal `0.1` rather than the binary value that `Double` actually holds.
   * A value that is not finite names no decimal and is reported as a failure, which is the
   * one case where this factory rejects its input.
   *
   * @param value  the value
   * @return the equivalent decimal, or the failure describing why the value cannot be held
   */
  def of(value: Double): Either[Failure, Decimal] =
    if (!value.isFinite) {
      Left(Failure.Invalid(s"Decimal value must be finite: $value"))
    } else {
      val longValue = value.toLong
      if (value == longValue.toDouble) {
        ofScaled(longValue, 0)
      } else {
        parse(java.lang.Double.toString(value))
      }
    }

  /**
   * Obtains a decimal from a `BigDecimal`.
   *
   * Precision beyond 18 digits is truncated towards zero.
   *
   * @param value  the value
   * @return the equivalent decimal, or the failure describing why the value cannot be held
   */
  def of(value: BigDecimal): Either[Failure, Decimal] = fromJavaBigDecimal(value.bigDecimal)

  /**
   * Obtains a decimal from an unscaled value and a scale.
   *
   * The scale is brought into the range 0 to 18, any smaller fractional part being dropped
   * by truncation, and the result is normalised, so `ofScaled(1230, 2)` is the decimal
   * `12.3`.
   *
   * @param unscaled  the unscaled value
   * @param scale  the scale
   * @return the equivalent decimal, or the failure describing why the value cannot be held
   */
  def ofScaled(unscaled: Long, scale: Int): Either[Failure, Decimal] =
    if (unscaled == 0L) {
      Right(ZERO)
    } else if (scale < 0 || scale > MaxScale || unscaled > MaxUnscaled || unscaled < -MaxUnscaled) {
      ofScaledAdjusting(unscaled, scale)
    } else {
      Right(create(unscaled, scale))
    }

  /**
   * Parses a decimal from text.
   *
   * The text is read with the semantics of `BigDecimal`, so a leading sign, an exponent and
   * any number of decimal places are all accepted, and precision beyond 18 digits is
   * truncated towards zero. Text that names no number, and a number too large to hold, are
   * both reported as failures.
   *
   * @param str  the text to parse
   * @return the decimal the text names, or the failure describing why it names none
   */
  def parse(str: String): Either[Failure, Decimal] =
    Try(new JavaBigDecimal(str)).toEither match {
      case Right(value) => fromJavaBigDecimal(value)
      case Left(_) => Left(Failure.Parsing(s"Unable to parse decimal: '$str'"))
    }

  // creates from a value that may need rounding, normalising the result
  private def fromJavaBigDecimal(value: JavaBigDecimal): Either[Failure, Decimal] = {
    val stripped = value.round(TruncatingContext).stripTrailingZeros
    val adjusted = if (stripped.scale < 0) stripped.setScale(0) else stripped
    if (adjusted.precision > MaxPrecision) {
      Left(Failure.Invalid(
        s"Decimal value must not exceed $MaxPrecision digits of precision at scale 0: ${value.toPlainString}"))
    } else {
      ofScaled(adjusted.unscaledValue.longValue, adjusted.scale)
    }
  }

  // handles the scales and magnitudes that need adjusting before a decimal can be created
  @tailrec
  private def ofScaledAdjusting(unscaled: Long, scale: Int): Either[Failure, Decimal] =
    if (scale >= 2 * MaxScale) {
      // truncation at the maximum precision leaves nothing of a value this small
      Right(ZERO)
    } else if (scale <= -MaxScale) {
      Left(precisionFailure(unscaled, scale))
    } else if (unscaled > MaxUnscaled || unscaled < -MaxUnscaled) {
      // the precision of a Long is 19 digits, so one truncation is always enough
      if (scale == 0) Left(precisionFailure(unscaled, scale)) else ofScaledAdjusting(unscaled / 10L, scale - 1)
    } else if (scale < 0) {
      val power = Powers(-scale)
      if (math.abs(unscaled) > Long.MaxValue / power) {
        Left(precisionFailure(unscaled, scale))
      } else {
        ofScaledAdjusting(unscaled * power, 0)
      }
    } else if (scale > MaxScale) {
      val truncated = unscaled / Powers(scale - MaxScale)
      if (truncated == 0L) Right(ZERO) else Right(create(truncated, MaxScale))
    } else {
      Right(create(unscaled, scale))
    }

  private def precisionFailure(unscaled: Long, scale: Int): Failure =
    Failure.Invalid(s"Decimal value must not exceed $MaxPrecision digits of precision: ${unscaled}E${-scale}")

  // creates an instance, removing any trailing fractional zeroes
  @tailrec
  private def create(unscaled: Long, scale: Int): Decimal =
    if (scale > 0 && unscaled % 10L == 0L) {
      create(unscaled / 10L, scale - 1)
    } else {
      new Decimal(unscaled, scale) {}
    }

  // the arithmetic edge: a result that does not fit is a broken precondition, not a value
  private def orFail(result: Either[Failure, Decimal]): Decimal =
    result match {
      case Right(value) => value
      case Left(failure) =>
        ArgCheck.isTrue(false, failure.message)
        ZERO
    }

  /**
   * The ordering of decimals, which is also their hashing.
   *
   * This is the only equality-bearing instance of the type: `Order` and `Hash` both extend
   * `Eq`, so the three can never disagree. Comparison is numerical and equality is that of
   * the values themselves, and the two agree because every decimal is normalised - so
   * `compare` returns zero exactly when the decimals are equal.
   *
   * @return the ordering of decimals
   */
  implicit val order: Order[Decimal] with Hash[Decimal] =
    new Order[Decimal] with Hash[Decimal] {

      private val universal: Hash[Decimal] = Hash.fromUniversalHashCode[Decimal]

      override def compare(x: Decimal, y: Decimal): Int = x.compareTo(y)

      override def eqv(x: Decimal, y: Decimal): Boolean = universal.eqv(x, y)

      override def hash(x: Decimal): Int = universal.hash(x)
    }

  /**
   * The rendering of decimals as text.
   *
   * A decimal renders in its canonical form, as `toString` does.
   *
   * @return the rendering of a decimal
   */
  implicit val show: Show[Decimal] = Show.show(_.toString)

  /**
   * The JSON encoder for decimals.
   *
   * A decimal is written as the string of its canonical form rather than as a JSON number,
   * because a JSON number is read back through a `Double` by many parsers and that would
   * lose the exactness the type exists to provide.
   *
   * @return the encoder writing a decimal as its canonical text
   */
  implicit val encoder: Encoder[Decimal] = Encoder.encodeString.contramap(_.toString)

  /**
   * The JSON decoder for decimals.
   *
   * The string is read through `parse`, so a document holding text that names no decimal is
   * rejected with the message of the parse failure.
   *
   * @return the decoder reading a decimal from its canonical text
   */
  implicit val decoder: Decoder[Decimal] =
    Decoder.decodeString.emap(text => parse(text).left.map(_.message))
}
