/*
 * Copyright (C) 2022 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import java.math.BigDecimal
import java.math.MathContext
import java.math.RoundingMode

import scala.annotation.tailrec
import scala.util.Try

import cats.Hash
import cats.Order
import cats.Show
import cats.data.EitherNec
import _root_.io.circe.Codec
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder

import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * A decimal number, held exactly, in the shape finance needs rather than the shape
 * arbitrary-precision arithmetic offers.
 *
 * A decimal is an unscaled `Long` paired with an `Int` scale, its value being the unscaled
 * part divided by ten raised to the scale. Both parts are primitives, so a decimal is one
 * small object holding no array and no boxed field, which is what makes it cheap enough to
 * use for money and for quoted rates - values that a `Double` cannot represent and that a
 * `BigDecimal` represents at a price the precision does not justify.
 *
 * The scale is constrained to the range 0 to 18 and the unscaled part to 18 digits. The type
 * therefore supports 18 decimal places for values between -1 and 1, 17 decimal places for
 * values between -10 and 10, and so on.
 *
 * ===Normalisation===
 *
 * Every decimal is normalised: the fractional part never ends in a zero, so `12.30` and
 * `12.3` are one value with one scale, and zero always has scale zero however it arose.
 * Normalisation is what makes equality, hashing and comparison agree with one another - two
 * decimals compare equal exactly when they are equal - and it is why the factories of the
 * companion, rather than a constructor, are the way to obtain one.
 *
 * A factory given more than 18 digits of precision truncates towards zero, which is the
 * behaviour of the type being ported; a factory given a value too large to hold at any scale
 * reports a failure.
 *
 * ===Failure, and the two edges that raise===
 *
 * Building a decimal from data - a `Long`, a `Double`, a `BigDecimal`, an unscaled value and
 * a scale, or text - can fail on the data supplied, so every factory answers with
 * `Either[Failure, Decimal]` and never raises.
 *
 * Arithmetic keeps the total signature of the type being ported: `plus`, `minus`,
 * `multipliedBy`, `dividedBy`, `remainder`, `movePoint` and the two rounding methods all
 * answer with a `Decimal`. A result needing more than 18 digits at scale zero is reported
 * through [[com.opengamma.strata.collect.ArgCheck]] as a broken precondition, because a sum
 * that does not fit is a fact about the program rather than about its input, and the same
 * applies to the argument checks of `roundToScale`, `roundToPrecision`, `format` and
 * `formatAtLeast`. Division by zero raises the `ArithmeticException` of the platform, exactly
 * as the ported type does. Which of these two edges a method has, if any, is stated on the
 * method.
 *
 * ===Text===
 *
 * `toString` renders the plain decimal form - no exponent, as many decimal places as the
 * scale - and that single canonical form is what `Show` shows, what the JSON codec writes and
 * what `parse` reads back.
 *
 * @param unscaled  the unscaled value, whose fractional part carries no trailing zero
 * @param scale  the scale, from 0 to 18
 */
sealed abstract case class Decimal private (unscaled: Long, scale: Int) {

  //-------------------------------------------------------------------------
  /**
   * Returns the unscaled part of this decimal.
   *
   * The value of this decimal is this number divided by ten raised to `scale`.
   *
   * @return the unscaled value
   */
  def unscaledValue: Long = unscaled

  /**
   * Returns true when this decimal is zero.
   *
   * Zero is held at scale zero, so one comparison of the unscaled part decides this.
   *
   * @return true when this decimal is zero
   */
  def isZero: Boolean = unscaled == 0L

  /**
   * Returns the sign of this decimal.
   *
   * The sign is read from the unscaled part, so the scale does not affect it.
   *
   * @return -1 when this decimal is negative, 0 when it is zero, 1 when it is positive
   */
  def signum: Int = java.lang.Long.signum(unscaled)

  /**
   * Returns this decimal with its sign removed.
   *
   * @return this decimal when it is zero or positive, its negation when it is negative
   */
  def abs: Decimal = Decimal.newDecimal(if (unscaled < 0L) -unscaled else unscaled, scale)

  /**
   * Returns this decimal negated.
   *
   * Negating an already normalised value cannot denormalise it, so the scale is carried over
   * unchanged.
   *
   * @return the decimal of the same magnitude and the opposite sign
   */
  def negated: Decimal = Decimal.newDecimal(-unscaled, scale)

  //-------------------------------------------------------------------------
  /**
   * Returns this decimal plus the specified decimal.
   *
   * The sum is computed on the unscaled parts whenever the scales can be brought together
   * without losing a digit, and on `BigDecimal` otherwise, which is what makes addition exact
   * up to the precision of the type. The result has a scale in the range 0 to 18 and is
   * truncated towards zero where the exact sum needs more precision than that.
   *
   * @param other  the decimal to add
   * @return the sum
   * @throws IllegalArgumentException if the sum needs more than 18 digits at scale zero
   */
  def plus(other: Decimal): Decimal =
    if (unscaled == 0L) {
      other
    } else if (other.unscaled == 0L) {
      this
    } else {
      Decimal.sumOrFail(unscaled, scale, other.unscaled, other.scale)
    }

  /**
   * Returns this decimal plus the specified value.
   *
   * @param other  the value to add
   * @return the sum
   * @throws IllegalArgumentException if the sum needs more than 18 digits at scale zero
   */
  def plus(other: Long): Decimal =
    if (other == 0L) {
      this
    } else if (other < -Decimal.MAX_UNSCALED || other > Decimal.MAX_UNSCALED) {
      // a value this large has no decimal of its own, so the whole part is added in Long
      // arithmetic and the fractional part of this decimal is necessarily lost; the sum is
      // formed first and then tested, and the failure it is tested for is described only in
      // the branch that reports it, so the addition that succeeds allocates nothing
      val whole = longValue
      val total = whole + other
      if (Decimal.isExactSum(whole, other, total)) {
        Decimal.wholeOrFail(total)
      } else {
        Decimal.raise(Decimal.arithmeticFailure(unscaled, "+", other))
      }
    } else {
      Decimal.sumOrFail(unscaled, scale, other, 0)
    }

  /**
   * Returns this decimal plus the specified value.
   *
   * The `Double` is converted to a decimal before the addition, so the value added is the one
   * the shortest text of the `Double` names.
   *
   * @param other  the value to add
   * @return the sum
   * @throws IllegalArgumentException if the value is not finite, or if the sum needs more than
   *   18 digits at scale zero
   */
  def plus(other: Double): Decimal =
    if (other == 0.0) {
      this
    } else {
      // the conversion is the only part that can reject the value, so it alone goes through
      // the outcome of a factory; the addition that follows answers a decimal directly
      plus(Decimal.orFail(Decimal.of(other)))
    }

  //-------------------------------------------------------------------------
  /**
   * Returns this decimal minus the specified decimal.
   *
   * Subtraction is addition of the negation, which is exact because negating a decimal cannot
   * lose a digit.
   *
   * @param other  the decimal to subtract
   * @return the difference
   * @throws IllegalArgumentException if the difference needs more than 18 digits at scale zero
   */
  def minus(other: Decimal): Decimal =
    if (other.unscaled == 0L) {
      this
    } else {
      plus(other.negated)
    }

  /**
   * Returns this decimal minus the specified value.
   *
   * @param other  the value to subtract
   * @return the difference
   * @throws IllegalArgumentException if the difference needs more than 18 digits at scale zero
   */
  def minus(other: Long): Decimal =
    if (other == 0L) {
      this
    } else if (other < -Decimal.MAX_UNSCALED || other > Decimal.MAX_UNSCALED) {
      // as for addition of a value beyond the precision of a decimal: the whole part is
      // subtracted in Long arithmetic, which also covers the value that cannot be negated,
      // and the difference is formed first so that the check on it allocates nothing
      val whole = longValue
      val difference = whole - other
      if (Decimal.isExactDifference(whole, other, difference)) {
        Decimal.wholeOrFail(difference)
      } else {
        Decimal.raise(Decimal.arithmeticFailure(unscaled, "-", other))
      }
    } else {
      Decimal.sumOrFail(unscaled, scale, -other, 0)
    }

  /**
   * Returns this decimal minus the specified value.
   *
   * The `Double` is converted to a decimal before the subtraction.
   *
   * @param other  the value to subtract
   * @return the difference
   * @throws IllegalArgumentException if the value is not finite, or if the difference needs
   *   more than 18 digits at scale zero
   */
  def minus(other: Double): Decimal =
    if (other == 0.0) {
      this
    } else {
      // as for addition of a `Double`: only the conversion answers an outcome
      minus(Decimal.orFail(Decimal.of(other)))
    }

  //-------------------------------------------------------------------------
  /**
   * Returns this decimal multiplied by the specified decimal.
   *
   * A decimal whose scale is zero multiplies through the unscaled parts, since the scale of
   * the product is then the scale of this decimal; any other multiplies through `BigDecimal`,
   * because the scales add and the product needs the wider intermediate.
   *
   * @param other  the decimal to multiply by
   * @return the product
   * @throws IllegalArgumentException if the product needs more than 18 digits at scale zero
   */
  def multipliedBy(other: Decimal): Decimal =
    if (other.scale == 0) {
      multipliedBy(other.unscaled)
    } else {
      Decimal.orFail(Decimal.of(toBigDecimal.multiply(other.toBigDecimal)))
    }

  /**
   * Returns this decimal multiplied by the specified value.
   *
   * @param other  the value to multiply by
   * @return the product
   * @throws IllegalArgumentException if the product needs more than 18 digits at scale zero
   */
  def multipliedBy(other: Long): Decimal =
    if (other == 0L) {
      Decimal.ZERO
    } else {
      // the branch above establishes the non-zero multiplier the exactness test needs
      val product = unscaled * other
      if (Decimal.isExactProduct(unscaled, other, product)) {
        Decimal.scaledOrFail(product, scale)
      } else {
        // the exact product overflows a Long, so it is formed in BigDecimal and then
        // truncated to the precision of a decimal
        Decimal.orFail(Decimal.of(toBigDecimal.multiply(BigDecimal.valueOf(other))))
      }
    }

  /**
   * Returns this decimal multiplied by the specified value.
   *
   * The `Double` is converted to a decimal before the multiplication.
   *
   * @param other  the value to multiply by
   * @return the product
   * @throws IllegalArgumentException if the value is not finite, or if the product needs more
   *   than 18 digits at scale zero
   */
  def multipliedBy(other: Double): Decimal =
    if (other == 0.0) {
      Decimal.ZERO
    } else {
      // as for addition of a `Double`: only the conversion answers an outcome
      multipliedBy(Decimal.orFail(Decimal.of(other)))
    }

  //-------------------------------------------------------------------------
  /**
   * Returns this decimal with its decimal point moved.
   *
   * This multiplies or divides by a power of ten without any loss beyond the precision of the
   * type: a positive movement moves the point right and multiplies, a negative movement moves
   * it left and divides.
   *
   * {{{
   * Decimal.of(1.235).map(_.movePoint(2))    // 123.5
   * Decimal.of(1.235).map(_.movePoint(-2))   // 0.01235
   * }}}
   *
   * A movement so far to the left that nothing of the value survives the maximum scale gives
   * zero, which is also the answer when the movement is so large that the resulting scale is
   * not representable at all.
   *
   * @param movement  the number of places to move by, positive to multiply, negative to divide
   * @return the decimal with the point moved
   * @throws IllegalArgumentException if the result needs more than 18 digits at scale zero
   */
  def movePoint(movement: Int): Decimal =
    if (movement == 0) {
      this
    } else {
      // the arithmetic is done in Long so that a movement that would overflow the scale is
      // detected rather than wrapping round, and such a movement can only shrink the value
      val moved = scale.toLong - movement.toLong
      if (moved > Int.MaxValue.toLong || moved < Int.MinValue.toLong) {
        Decimal.ZERO
      } else {
        // the movement is an adjustment of the scale, so it answers through the adjustment
        // itself rather than through a factory whose outcome would be unwrapped at once
        Decimal.scaledAdjustedOrFail(unscaled, moved.toInt)
      }
    }

  //-------------------------------------------------------------------------
  /**
   * Returns this decimal divided by the specified decimal, truncating towards zero.
   *
   * The quotient is computed to 18 significant digits and then held at a scale in the range
   * 0 to 18, so a quotient that does not terminate is truncated rather than rejected.
   *
   * @param other  the decimal to divide by
   * @return the quotient
   * @throws ArithmeticException if the divisor is zero
   * @throws IllegalArgumentException if the quotient needs more than 18 digits at scale zero
   */
  def dividedBy(other: Decimal): Decimal =
    Decimal.orFail(Decimal.ofRounded(toBigDecimal.divide(other.toBigDecimal, Decimal.MATH_CONTEXT)))

  /**
   * Returns this decimal divided by the specified decimal, rounding as instructed.
   *
   * @param other  the decimal to divide by
   * @param roundingMode  the rounding to apply to the eighteenth significant digit
   * @return the quotient
   * @throws ArithmeticException if the divisor is zero
   * @throws IllegalArgumentException if the quotient needs more than 18 digits at scale zero
   */
  def dividedBy(other: Decimal, roundingMode: RoundingMode): Decimal =
    Decimal.orFail(
      Decimal.ofRounded(
        toBigDecimal.divide(other.toBigDecimal, new MathContext(Decimal.MAX_PRECISION, roundingMode))))

  /**
   * Returns this decimal divided by the specified value, truncating towards zero.
   *
   * Division by a power of ten moves the decimal point instead of dividing, which is exact.
   *
   * @param other  the value to divide by
   * @return the quotient
   * @throws ArithmeticException if the divisor is zero
   * @throws IllegalArgumentException if the quotient needs more than 18 digits at scale zero
   */
  def dividedBy(other: Long): Decimal =
    if (other == 1L) {
      this
    } else {
      val power = Decimal.powerOfTenIndex(other)
      if (power > 0) {
        movePoint(-power)
      } else {
        Decimal.orFail(
          Decimal.ofRounded(toBigDecimal.divide(BigDecimal.valueOf(other), Decimal.MATH_CONTEXT)))
      }
    }

  /**
   * Returns this decimal divided by the specified value, truncating towards zero.
   *
   * The `Double` is converted to a decimal before the division, so dividing by a value that
   * names zero divides by zero.
   *
   * @param other  the value to divide by
   * @return the quotient
   * @throws ArithmeticException if the divisor is zero
   * @throws IllegalArgumentException if the value is not finite, or if the quotient needs more
   *   than 18 digits at scale zero
   */
  def dividedBy(other: Double): Decimal =
    // as for the other operations on a `Double`: only the conversion answers an outcome, and
    // a divisor naming zero reaches the division and raises from there
    dividedBy(Decimal.orFail(Decimal.of(other)))

  /**
   * Returns the remainder of dividing this decimal by the specified decimal.
   *
   * The remainder carries the sign of this decimal, as the remainder of exact decimal division
   * does.
   *
   * @param other  the decimal to divide by
   * @return the remainder
   * @throws ArithmeticException if the divisor is zero
   * @throws IllegalArgumentException if the remainder needs more than 18 digits at scale zero
   */
  def remainder(other: Decimal): Decimal =
    Decimal.orFail(Decimal.ofRounded(toBigDecimal.remainder(other.toBigDecimal, Decimal.MATH_CONTEXT)))

  //-------------------------------------------------------------------------
  /**
   * Returns this decimal rounded to the specified scale.
   *
   * The result has the requested scale or less, because a fractional trailing zero is removed;
   * requesting a scale of 18 or more therefore leaves the decimal unchanged. A negative scale
   * rounds the whole part, so a scale of -1 rounds to a multiple of ten.
   *
   * {{{
   * Decimal.of(1.235).map(_.roundToScale(2, RoundingMode.HALF_UP))   // 1.24
   * Decimal.of(1.201).map(_.roundToScale(2, RoundingMode.HALF_UP))   // 1.2
   * Decimal.of(1235L).map(_.roundToScale(-1, RoundingMode.HALF_UP))  // 1240
   * }}}
   *
   * Truncation, rounding half away from zero, and rounding away from zero are computed on the
   * unscaled part; `FLOOR` and `CEILING` are one of those two according to the sign; any other
   * mode is computed on `BigDecimal`. Every mode agrees with `BigDecimal.setScale` under the
   * same mode.
   *
   * @param desiredScale  the scale to round to, positive for decimal places, negative to round
   *   the whole part, and greater than -18
   * @param roundingMode  the rounding to apply
   * @return the rounded decimal
   * @throws IllegalArgumentException if the scale is -18 or less, where the edge cases of the
   *   representation make the answer ambiguous
   * @throws ArithmeticException if the mode is `UNNECESSARY` and rounding is required
   */
  def roundToScale(desiredScale: Int, roundingMode: RoundingMode): Decimal =
    if (desiredScale >= scale) {
      this
    } else {
      ArgCheck.isTrue(
        desiredScale > -Decimal.MAX_SCALE,
        s"Rounding scale must not be -18 or less: $desiredScale")
      if (unscaled == 0L) {
        Decimal.ZERO
      } else {
        val adjustedScale = math.min(desiredScale, Decimal.MAX_SCALE)
        roundingMode match {
          case RoundingMode.DOWN => roundDownToScale(adjustedScale)
          case RoundingMode.HALF_UP => roundHalfUpToScale(adjustedScale)
          case RoundingMode.UP => roundUpToScale(adjustedScale)
          case RoundingMode.FLOOR =>
            if (unscaled > 0L) roundDownToScale(adjustedScale) else roundUpToScale(adjustedScale)
          case RoundingMode.CEILING =>
            if (unscaled > 0L) roundUpToScale(adjustedScale) else roundDownToScale(adjustedScale)
          case _ =>
            Decimal.orFail(Decimal.of(toBigDecimal.setScale(adjustedScale, roundingMode)))
        }
      }
    }

  // truncation towards zero is an integral division of the unscaled part
  private def roundDownToScale(adjustedScale: Int): Decimal = {
    val scaleDiff = scale - adjustedScale
    if (scaleDiff <= Decimal.MAX_SCALE) {
      Decimal.scaledOrFail(unscaled / Decimal.POWERS(scaleDiff), adjustedScale)
    } else {
      // every digit of the value lies below the requested scale
      Decimal.ZERO
    }
  }

  // rounding half away from zero needs only the first digit being discarded
  private def roundHalfUpToScale(adjustedScale: Int): Decimal = {
    val scaleDiff = scale - adjustedScale
    if (scaleDiff < Decimal.MAX_SCALE) {
      val rescaledPlusNext = unscaled / Decimal.POWERS(scaleDiff - 1)
      val rescaled = rescaledPlusNext / 10L
      val nextDigit = rescaledPlusNext % 10L
      val bump = if (nextDigit >= 5L) 1L else if (nextDigit <= -5L) -1L else 0L
      Decimal.scaledOrFail(rescaled + bump, adjustedScale)
    } else {
      Decimal.orFail(Decimal.of(toBigDecimal.setScale(adjustedScale, RoundingMode.HALF_DOWN)))
    }
  }

  // rounding away from zero is truncation followed by a step of one in the sign of the value
  private def roundUpToScale(adjustedScale: Int): Decimal = {
    val scaleDiff = scale - adjustedScale
    if (scaleDiff <= Decimal.MAX_SCALE) {
      val rescaled = unscaled / Decimal.POWERS(scaleDiff)
      if (unscaled == rescaled) {
        this
      } else {
        Decimal.scaledOrFail(rescaled + math.signum(unscaled), adjustedScale)
      }
    } else {
      Decimal.orFail(Decimal.of(toBigDecimal.setScale(adjustedScale, RoundingMode.UP)))
    }
  }

  /**
   * Returns this decimal rounded to the specified number of significant digits.
   *
   * A precision of 18 or more leaves the decimal unchanged, since it holds no more than that.
   * Note that a value such as 12,000 counts as two significant digits at scale -3 for the
   * purpose of rounding, while the result is stored at scale 0.
   *
   * @param precision  the number of significant digits to keep, not negative
   * @param roundingMode  the rounding to apply
   * @return the rounded decimal
   * @throws IllegalArgumentException if the precision is negative
   * @throws ArithmeticException if the mode is `UNNECESSARY` and rounding is required
   */
  def roundToPrecision(precision: Int, roundingMode: RoundingMode): Decimal = {
    ArgCheck.notNegative(precision, "precision")
    if (precision >= Decimal.MAX_PRECISION) {
      this
    } else {
      Decimal.orFail(Decimal.ofRounded(toBigDecimal.round(new MathContext(precision, roundingMode))))
    }
  }

  //-------------------------------------------------------------------------
  /**
   * Maps this decimal through a function on `Double`.
   *
   * The arithmetic of `Double` is inexact, so the decimal obtained is the one the shortest
   * text of the result names. The function may produce a value that no decimal can hold - one
   * that is not finite, or one too large - which is why this answers with an `Either`.
   *
   * @param fn  the function to apply to the value of this decimal
   * @return the decimal of the result, or the failure describing why the result cannot be held
   */
  def mapAsDouble(fn: Double => Double): Either[Failure, Decimal] = Decimal.of(fn(doubleValue))

  /**
   * Maps this decimal through a function on `BigDecimal`.
   *
   * Precision beyond 18 digits in the result is truncated towards zero, and a result too large
   * to hold is reported as a failure.
   *
   * @param fn  the function to apply to the value of this decimal
   * @return the decimal of the result, or the failure describing why the result cannot be held
   */
  def mapAsBigDecimal(fn: BigDecimal => BigDecimal): Either[Failure, Decimal] =
    Decimal.of(fn(toBigDecimal))

  //-------------------------------------------------------------------------
  /**
   * Returns this decimal as a `Double`, which may lose precision.
   *
   * A value whose unscaled part is small enough to be held exactly by a `Double` is divided by
   * the power of ten of its scale, which is one rounding; a larger one is obtained from the
   * canonical text, which is the nearest `Double` to the decimal.
   *
   * @return the `Double` nearest to this decimal
   */
  def doubleValue: Double =
    if (scale == 0) {
      unscaled.toDouble
    } else if (math.abs(unscaled) < (1L << 52)) {
      unscaled.toDouble / Decimal.POWERS(scale).toDouble
    } else {
      toString.toDouble
    }

  /**
   * Returns this decimal as a `Long`, discarding any fractional part.
   *
   * @return the whole part of this decimal, truncated towards zero
   */
  def longValue: Long = unscaled / Decimal.POWERS(scale)

  /**
   * Returns this decimal as a `BigDecimal`, exactly.
   *
   * The scale of the value returned is the scale of this decimal, so the text of the two
   * agrees.
   *
   * @return the equivalent `BigDecimal`
   */
  def toBigDecimal: BigDecimal = BigDecimal.valueOf(unscaled, scale)

  /**
   * Returns this decimal as a decimal of fixed scale.
   *
   * The fixed scale must be at least the scale of this decimal, since a fixed-scale decimal
   * never drops a digit; call `roundToScale` first where the scale of this decimal is not
   * known to be small enough.
   *
   * @param fixedScale  the fixed scale, from the scale of this decimal to 18
   * @return the fixed-scale decimal, or the failures describing why that scale cannot hold it
   */
  def toFixedScale(fixedScale: Int): EitherNec[Failure, FixedScaleDecimal] =
    FixedScaleDecimal.of(this, fixedScale)

  //-------------------------------------------------------------------------
  /**
   * Formats this decimal with at least the specified number of decimal places.
   *
   * Where this decimal has more decimal places than requested they are all shown, since
   * dropping them would change the value shown; where it has fewer, the fraction is padded
   * with zeroes. With a minimum of two decimal places, `12.1` formats as `12.10` and `12.123`
   * formats as `12.123`. A minimum of zero is therefore `toString`.
   *
   * @param minDecimalPlaces  the minimum number of decimal places, from 0 to 18 inclusive
   * @return the formatted decimal
   * @throws IllegalArgumentException if the number of decimal places is outside 0 to 18
   */
  def formatAtLeast(minDecimalPlaces: Int): String = {
    ArgCheck.isTrue(
      minDecimalPlaces >= 0 && minDecimalPlaces <= Decimal.MAX_SCALE,
      "Format requires decimal places between 0 and 18 inclusive")
    format0(math.max(minDecimalPlaces, scale))
  }

  /**
   * Formats this decimal with exactly the specified number of decimal places.
   *
   * The decimal is rounded to the requested number of places and then padded, so with two
   * decimal places and rounding half away from zero, `12.1` formats as `12.10` and `12.125`
   * formats as `12.13`. Use `RoundingMode.DOWN` to truncate instead of rounding.
   *
   * @param decimalPlaces  the number of decimal places, from 0 to 18 inclusive
   * @param roundingMode  the rounding to apply where this decimal has more decimal places
   * @return the formatted decimal
   * @throws IllegalArgumentException if the number of decimal places is outside 0 to 18
   * @throws ArithmeticException if the mode is `UNNECESSARY` and rounding is required
   */
  def format(decimalPlaces: Int, roundingMode: RoundingMode): String = {
    ArgCheck.isTrue(
      decimalPlaces >= 0 && decimalPlaces <= Decimal.MAX_SCALE,
      "Format requires decimal places between 0 and 18 inclusive")
    roundToScale(decimalPlaces, roundingMode).format0(decimalPlaces)
  }

  /**
   * Formats this decimal to the given number of decimal places, which is never fewer than its
   * scale.
   *
   * Every caller establishes that condition - `toString` passes the scale itself,
   * `formatAtLeast` passes at least the scale, and `format` rounds to the requested places
   * first - so no digit of the value is ever dropped here and the power of ten indexed below
   * is always in range.
   *
   * The fraction is rendered without building it a character at a time. Splitting 78.345 into
   * a whole part of 78 and a fraction of 345 loses the leading zeroes of a fraction such as
   * 005, so the power of ten of the scale is added to the fraction: the extra leading 1 that
   * produces holds the width of the fraction, multiplying by a power of ten pads it on the
   * right to the requested width, and dropping that leading 1 from the text leaves exactly the
   * digits wanted. The padded fraction stays below two times ten to the eighteenth, so it
   * always fits in a `Long`.
   *
   * @param decimalPlaces  the number of decimal places, at least the scale and at most 18
   * @return the formatted decimal
   */
  private def format0(decimalPlaces: Int): String = {
    val absolute = math.abs(unscaled)
    val power = Decimal.POWERS(scale)
    val whole = absolute / power
    val sign = if (unscaled < 0L) "-" else ""
    if (decimalPlaces > 0) {
      val prefixedFraction = (absolute % power) + power
      val paddedFraction = prefixedFraction * Decimal.POWERS(decimalPlaces - scale)
      s"$sign$whole.${paddedFraction.toString.substring(1)}"
    } else {
      s"$sign$whole"
    }
  }

  //-------------------------------------------------------------------------
  /**
   * Compares this decimal with the specified decimal.
   *
   * Decimals of equal scale compare on their unscaled parts; decimals of different scale
   * compare on their whole parts and then on their fractions brought to a common scale, which
   * keeps the comparison exact without leaving the arithmetic of `Long`.
   *
   * Since every decimal is normalised, this returns zero exactly when the two decimals are
   * equal, so the comparison is consistent with equality.
   *
   * @param other  the decimal to compare with
   * @return a negative number, zero or a positive number as this decimal is less than, equal
   *   to, or greater than the other
   */
  def compareTo(other: Decimal): Int =
    if (scale == other.scale) {
      java.lang.Long.compare(unscaled, other.unscaled)
    } else {
      val power = Decimal.POWERS(scale)
      val otherPower = Decimal.POWERS(other.scale)
      val whole = unscaled / power
      val otherWhole = other.unscaled / otherPower
      if (whole < otherWhole) {
        -1
      } else if (whole > otherWhole) {
        1
      } else {
        val fraction = (unscaled % power) * Decimal.POWERS(Decimal.MAX_SCALE - scale)
        val otherFraction = (other.unscaled % otherPower) * Decimal.POWERS(Decimal.MAX_SCALE - other.scale)
        java.lang.Long.compare(fraction, otherFraction)
      }
    }

  /**
   * Returns true when this decimal is greater than the specified decimal.
   *
   * @param other  the decimal to compare with
   * @return true when this decimal is the greater
   */
  def isGreaterThan(other: Decimal): Boolean = compareTo(other) > 0

  /**
   * Returns true when this decimal is greater than or equal to the specified decimal.
   *
   * @param other  the decimal to compare with
   * @return true when this decimal is the greater or the two are equal
   */
  def isGreaterThanEqualTo(other: Decimal): Boolean = compareTo(other) >= 0

  /**
   * Returns true when this decimal is less than the specified decimal.
   *
   * @param other  the decimal to compare with
   * @return true when this decimal is the lesser
   */
  def isLessThan(other: Decimal): Boolean = compareTo(other) < 0

  /**
   * Returns true when this decimal is less than or equal to the specified decimal.
   *
   * @param other  the decimal to compare with
   * @return true when this decimal is the lesser or the two are equal
   */
  def isLessThanEqualTo(other: Decimal): Boolean = compareTo(other) <= 0

  //-------------------------------------------------------------------------
  /**
   * Returns the canonical text of this decimal.
   *
   * The form is the plain decimal form - never an exponent, and as many decimal places as the
   * scale - so `12.3`, `-0.5` and `4`. This is the form `Decimal.parse` reads back and the
   * form the JSON codec writes, which makes it the single textual representation of the type.
   *
   * @return the canonical text of this decimal
   */
  override def toString: String = format0(scale)
}

/**
 * Provides the constants, factories and instances of decimals.
 *
 * ===Construction===
 *
 * A decimal is normalising: a factory canonicalises what it is given - trailing fractional
 * zeroes removed, zero brought to scale zero, a negative scale turned into digits of the
 * whole part, precision beyond 18 digits truncated towards zero - and may still reject it,
 * because no decimal holds a value that is not finite or that needs more than 18 digits at
 * scale zero. Both halves of that are in the signature: every factory answers with
 * `Either[Failure, Decimal]`, and applying a factory to its own output is idempotent.
 *
 * The type has no public constructor and no `copy`, so the factories are the only way to
 * obtain a decimal and no value of the type can be denormalised:
 *
 * {{{
 * val rate: Either[Failure, Decimal] = Decimal.of("1.2350")   // 1.235
 * val same: Either[Failure, Decimal] = Decimal.ofScaled(1235, 3)
 * }}}
 *
 * ===Instances===
 *
 * `Order` carries the hashing of the type as well as its comparison, so one implicit bears
 * equality and the three can never disagree. That single instance is possible because
 * normalisation leaves no room for a tie-break: `compare` returns zero exactly when the two
 * decimals are equal. A fixed-scale decimal, whose comparison deliberately ignores the scale
 * that its equality includes, does need one.
 */
object Decimal {

  /** The number of significant digits a decimal holds. */
  private val MAX_PRECISION: Int = 18

  /**
   * The largest scale a decimal holds.
   *
   * Visible across this module because a fixed-scale decimal is constrained by the same bound
   * and reads it from here rather than restating it.
   */
  private[collect] val MAX_SCALE: Int = 18

  /** The largest unscaled value, 18 nines. */
  private val MAX_UNSCALED: Long = 999999999999999999L

  /**
   * The powers of ten, indexed by exponent, from ten to the zeroth to ten to the eighteenth.
   *
   * Built by repeated multiplication so that every entry is exact, and never handed out, so
   * the array cannot be seen - let alone changed - from outside this type.
   */
  private val POWERS: Array[Long] = Array.iterate(1L, MAX_SCALE + 1)(_ * 10L)

  /** The context that truncates a `BigDecimal` to the precision a decimal supports. */
  private val MATH_CONTEXT: MathContext = new MathContext(MAX_PRECISION, RoundingMode.DOWN)

  /** The longest text a decimal is parsed from. */
  private val MAX_TEXT_LENGTH: Int = 256

  /** The decimal representing zero. */
  val ZERO: Decimal = newDecimal(0L, 0)

  /** The largest supported decimal, 18 nines at scale zero. */
  val MAX_VALUE: Decimal = newDecimal(MAX_UNSCALED, 0)

  /** The smallest supported decimal, the negation of `MAX_VALUE`. */
  val MIN_VALUE: Decimal = newDecimal(-MAX_UNSCALED, 0)

  //-------------------------------------------------------------------------
  /**
   * Obtains a decimal from a `Long`.
   *
   * @param value  the value
   * @return the equivalent decimal, or the failure describing why the value cannot be held
   */
  def of(value: Long): Either[Failure, Decimal] =
    if (value > MAX_UNSCALED || value < -MAX_UNSCALED) {
      Left(precisionAtScaleZeroFailure(value.toString))
    } else {
      Right(newDecimal(value, 0))
    }

  /**
   * Obtains a decimal from a `Double`.
   *
   * The decimal obtained is the one the shortest text of the `Double` names, so `0.1` gives
   * the decimal `0.1` rather than the binary value a `Double` actually holds. A value that is
   * not finite names no decimal and is reported as a failure, as is one too large to hold.
   *
   * @param value  the value
   * @return the equivalent decimal, or the failure describing why the value cannot be held
   */
  def of(value: Double): Either[Failure, Decimal] =
    if (!value.isFinite) {
      Left(Failure.Invalid(s"Decimal value must be finite: $value"))
    } else {
      val whole = value.toLong
      if (value == whole.toDouble) {
        // an integral value keeps scale zero, which the text route would not always give
        of(whole)
      } else {
        of(value.toString)
      }
    }

  /**
   * Obtains a decimal from a `BigDecimal`.
   *
   * The scale is brought into the range 0 to 18 and precision beyond 18 digits is truncated
   * towards zero, so a value with a longer fraction is accepted and shortened while one whose
   * whole part needs more than 18 digits is reported as a failure.
   *
   * @param value  the value
   * @return the equivalent decimal, or the failure describing why the value cannot be held
   */
  def of(value: BigDecimal): Either[Failure, Decimal] = ofRounded(value.round(MATH_CONTEXT))

  /**
   * Obtains a decimal from text.
   *
   * The text is read with the semantics of `BigDecimal` - a leading sign, an exponent and any
   * number of decimal places are all accepted - and the value read is then converted as
   * `of(BigDecimal)` converts it, which is the documented contract of the type being ported:
   * its hand-written scanner exists to save the intermediate object and is specified to agree
   * digit for digit with this route. Text naming no number, text longer than the type accepts,
   * and a number too large to hold are each reported as a failure.
   *
   * Agreeing with that route does not require taking it. After the two guards on the text
   * itself, one pass over the characters - the pre-filter the ported type carries for the same
   * reason - classifies the text into exactly three outcomes:
   *
   *   - a plain numeral, being an optional leading sign, at least one ASCII digit, at most one
   *     decimal point, no more than eighteen significant digits and a scale of no more than
   *     eighteen, is evaluated by the pass itself and finished through `ofScaled`, which
   *     applies the normalisation every factory applies. No `BigDecimal` is built, because for
   *     this shape the pass and `BigDecimal` provably read the same value.
   *   - text carrying an exponent, a unicode decimal digit, more than eighteen significant
   *     digits or a scale beyond eighteen is read by `BigDecimal` and converted by
   *     `of(BigDecimal)`. These are the shapes whose value, or whose truncation, the pass does
   *     not decide, so the semantics above remain those of `BigDecimal` rather than an
   *     imitation of them.
   *   - text that no `BigDecimal` could read - any other character, a second decimal point, a
   *     sign that is neither the leading one nor part of an exponent, an exponent that is not
   *     an optional sign followed by digits, or no digit at all - is reported as a failure by
   *     the pass itself. Nothing is thrown and nothing is caught on this path: a malformed
   *     numeral is input rather than an exceptional condition, and building an exception to
   *     catch it cost more than reading the text does.
   *
   * An unreadable-numeral failure quotes the text back as it was given, so the wording is the
   * one the ported type reported, character for character, and the caller is handed exactly
   * the numeral that was refused. The length of such a message is bounded by the guard above,
   * which rejects text longer than the type reads before any numeral is quoted; escaping what
   * the text may hold belongs to the writing of a failure, which the text form of one and
   * [[com.opengamma.strata.collect.result.Failure.show]] perform for every part they write.
   *
   * @param str  the text to read
   * @return the decimal the text names, or the failure describing why it names none
   */
  def of(str: String): Either[Failure, Decimal] =
    if (str.isEmpty) {
      Left(Failure.Parsing("Decimal string must not be empty"))
    } else if (str.length > MAX_TEXT_LENGTH) {
      Left(Failure.Parsing(s"Decimal string must not exceed $MAX_TEXT_LENGTH characters"))
    } else {
      // the leading sign is taken here rather than in the pass, which therefore treats every
      // sign it meets as the mid-text sign that no numeral carries
      val first = str.charAt(0)
      val negative = first == '-'
      val start = if (negative || first == '+') 1 else 0
      scanText(
        str,
        start,
        if (negative) -1L else 1L,
        precision = 0,
        scale = 0,
        unscaled = 0L,
        afterPoint = false,
        anyDigit = false)
    }

  /**
   * Obtains a decimal from an unscaled value and a scale.
   *
   * The scale is brought into the range 0 to 18, any part of the value below that scale being
   * dropped by truncation, and the result is normalised, so `ofScaled(1230, 2)` is the decimal
   * `12.3`. A negative scale becomes digits of the whole part where they fit, and a value too
   * large to hold at scale zero is reported as a failure.
   *
   * @param unscaled  the unscaled value
   * @param scale  the scale
   * @return the equivalent decimal, or the failure describing why the value cannot be held
   */
  def ofScaled(unscaled: Long, scale: Int): Either[Failure, Decimal] =
    if (unscaled == 0L) {
      // zero is held at scale zero whatever scale it arrives with, including one out of range
      Right(ZERO)
    } else if (isSupported(unscaled, scale)) {
      Right(create(unscaled, scale))
    } else {
      ofScaledAdjusted(unscaled, scale)
    }

  /**
   * Parses a decimal from text.
   *
   * This reads exactly what `of(String)` reads. It carries its own name because that is the
   * name the JSON decoder, and any other reader of text, refers to.
   *
   * @param str  the text to read
   * @return the decimal the text names, or the failure describing why it names none
   */
  def parse(str: String): Either[Failure, Decimal] = of(str)

  //-------------------------------------------------------------------------
  // the character pass of of(String), and the two endings it does not evaluate itself.
  //
  // The pass exists for the two reasons the scanner of the ported type exists: the numeral of
  // a quote or of a market data file is evaluated without building a `BigDecimal` that is then
  // thrown away, and text that names no number is reported without constructing an exception
  // to catch. It decides only the shapes whose outcome provably agrees with
  // `new BigDecimal(str)` followed by `of(BigDecimal)` - a numeral within the precision and the
  // scale of the type, which both routes read as the same unscaled value and scale - and hands
  // every other shape to exactly that route, so `of(String)` carries one set of semantics
  // however the text is spelled.
  //
  // The counting mirrors the loop of the ported scanner: a leading zero contributes no
  // significant digit but a zero after the point still occupies a place, so `0.001` is 1 at
  // scale 3, `1.10` is 110 at scale 2 before `ofScaled` normalises it, `1.` is 1 at scale 0
  // and `.5` is 5 at scale 1.
  //
  // @param str  the text being read
  // @param index  the position of the next character to read
  // @param sign  -1 when the text carried a leading minus, 1 otherwise
  // @param precision  the significant digits accumulated so far, leading zeroes not counted
  // @param scale  the number of digits read after the decimal point so far
  // @param unscaled  the digits accumulated so far, as a whole number without its sign
  // @param afterPoint  true once the decimal point has been read
  // @param anyDigit  true once any ASCII digit has been read, which is what an exponent needs
  //   in front of it and what separates a numeral from text such as "-", "." or "+."
  @tailrec
  private def scanText(
      str: String,
      index: Int,
      sign: Long,
      precision: Int,
      scale: Int,
      unscaled: Long,
      afterPoint: Boolean,
      anyDigit: Boolean): Either[Failure, Decimal] =

    if (index >= str.length) {
      // the pass reached the end of a numeral it evaluated, so the accumulated parts are the
      // value; text holding no digit at all names no number
      if (anyDigit) ofScaled(unscaled * sign, scale) else invalidText(str)
    } else {
      val ch = str.charAt(index)
      if (ch >= '0' && ch <= '9') {
        val digit = (ch - '0').toLong
        val significant = precision > 0 || digit > 0L
        val nextPrecision = if (significant) precision + 1 else precision
        val nextScale = if (afterPoint) scale + 1 else scale
        if (nextPrecision > MAX_PRECISION || nextScale > MAX_SCALE) {
          // the numeral names more than the type holds, so what survives of it is a question
          // about truncation, and `of(BigDecimal)` is the one answer to that question
          bigDecimalText(str)
        } else {
          scanText(
            str,
            index + 1,
            sign,
            nextPrecision,
            nextScale,
            if (significant) unscaled * 10L + digit else unscaled,
            afterPoint,
            anyDigit = true)
        }
      } else if (ch == '.') {
        // one point separates the two halves of a numeral; a second one makes it text
        if (afterPoint) {
          invalidText(str)
        } else {
          scanText(str, index + 1, sign, precision, scale, unscaled, afterPoint = true, anyDigit)
        }
      } else if (ch == 'e' || ch == 'E') {
        // an exponent moves the point by an amount this pass does not apply, so text whose
        // exponent `BigDecimal` could read is read there - including an exponent too large to
        // apply, which is a value it rejects rather than text it cannot read
        if (anyDigit && isExponentTail(str, index + 1)) {
          bigDecimalText(str)
        } else {
          invalidText(str)
        }
      } else if (Character.isDigit(ch)) {
        // a unicode decimal digit outside ASCII is a digit to `BigDecimal`, and reading it
        // there is what keeps the digit rules of this factory the digit rules of `BigDecimal`
        bigDecimalText(str)
      } else {
        invalidText(str)
      }
    }

  // true when the text from the given position is the tail of an exponent: an optional single
  // sign followed by at least one digit and nothing else.
  //
  // This is the acceptance test rather than the rejection test, and deliberately so: a tail
  // this answers true for is handed to `BigDecimal`, whose reading of it is the outcome, while
  // a tail it answers false for is one `BigDecimal` could not read at all. The digits are
  // tested with `Character.isDigit`, as the digits of the numeral are, because `BigDecimal`
  // reads a unicode digit in an exponent as readily as in a mantissa.
  private def isExponentTail(str: String, from: Int): Boolean = {
    val signed = from < str.length && (str.charAt(from) == '+' || str.charAt(from) == '-')
    val start = if (signed) from + 1 else from
    start < str.length && isDigitRun(str, start)
  }

  // true when every character from the given position on is a decimal digit; an empty run
  // answers true, the caller above being what establishes that there is a digit to read
  @tailrec
  private def isDigitRun(str: String, index: Int): Boolean =
    if (index >= str.length) {
      true
    } else if (Character.isDigit(str.charAt(index))) {
      isDigitRun(str, index + 1)
    } else {
      false
    }

  // reads text through `BigDecimal`, the route every shape the pass does not evaluate itself
  // takes; text `BigDecimal` refuses is reported as the failure of this factory, and the pass
  // above exists so that the common malformed numeral never reaches this branch
  private def bigDecimalText(str: String): Either[Failure, Decimal] =
    Try(new BigDecimal(str)).toEither match {
      case Right(value) => of(value)
      case Left(_) => invalidText(str)
    }

  // reports text that names no number, in the one wording every unreadable text is reported
  // by; the text is quoted as it stands, which is the wording of the ported type, and the
  // length of the message is bounded here already - `of(String)` rejects text longer than
  // `MAX_TEXT_LENGTH` before this branch is reachable
  private def invalidText(str: String): Either[Failure, Decimal] =
    Left(Failure.Parsing(s"Decimal string is invalid: '$str'"))

  //-------------------------------------------------------------------------
  // creates from a value already truncated to the supported precision.
  //
  // A `BigDecimal` of negative scale holds its trailing zeroes in the scale rather than in
  // the digits - `1E+500000000` is one digit and a scale of minus five hundred million - so
  // bringing it to scale zero materialises every one of those zeroes. That expansion is
  // performed by the platform through `BigInteger.TEN.pow`, and its cost is proportional to
  // the exponent the caller named rather than to the text or the value the caller supplied:
  // a twelve-character numeral reaching this factory through `of(String)`, through the JSON
  // decoder or through `of(BigDecimal)` can ask for hundreds of millions of digits, which
  // exhausts the heap or the processor rather than answering. The digit count is therefore
  // computed before it is reached.
  //
  // The count is exact rather than approximate: for a non-zero `BigDecimal` the number of
  // digits the value has at scale zero is its precision less its scale, because the scale is
  // exactly the number of places the point is to be moved. So the guard below and the check
  // that follows the expansion decide the same values - a value the guard rejects is one whose
  // expansion would have had more than `MAX_PRECISION` digits, and a value it admits expands
  // to at most `MAX_PRECISION` digits, which is a bounded amount of work. The subtraction is
  // done in `Long` because the scale reaches `Int.MinValue` for text such as `1e2147483648`,
  // where negating it in `Int` would overflow and admit the very value being excluded.
  //
  // Zero is answered before the arithmetic and not through it. `stripTrailingZeros` already
  // brings a zero to scale zero whatever scale it arrived with, so the branch is not reached
  // by any zero today; it is written because the precision of a zero is one while its scale
  // may be anything, and a zero that did arrive with a large negative scale must be the value
  // zero rather than a rejection.
  private def ofRounded(value: BigDecimal): Either[Failure, Decimal] = {
    val stripped = value.stripTrailingZeros
    if (stripped.signum == 0) {
      // zero is held at scale zero, which is the normalisation every factory applies
      Right(ZERO)
    } else if (exceedsPrecisionAtScaleZero(stripped)) {
      Left(precisionAtScaleZeroFailure(value.toString))
    } else {
      val adjusted = if (stripped.scale < 0) stripped.setScale(0) else stripped
      if (adjusted.precision > MAX_PRECISION) {
        Left(precisionAtScaleZeroFailure(value.toString))
      } else {
        // the precision checked above bounds the unscaled value, so this conversion is exact
        ofScaled(adjusted.unscaledValue.longValueExact, adjusted.scale)
      }
    }
  }

  // true when a non-zero value of negative scale holds more digits at scale zero than the type
  // supports, decided by counting those digits rather than by producing them.
  //
  // The value is the one `stripTrailingZeros` produced, so its precision counts only the digits
  // it actually holds and its scale counts the places the point is to move. The count in `Long`
  // therefore needs no expansion and cannot overflow at the extreme scales `BigDecimal` reads.
  private def exceedsPrecisionAtScaleZero(stripped: BigDecimal): Boolean =
    stripped.scale < 0 &&
      stripped.precision.toLong - stripped.scale.toLong > MAX_PRECISION.toLong

  // true when an unscaled value and a scale are held as they stand, so that the pair needs no
  // adjustment and cannot be rejected; shared by the factory and by the arithmetic, which
  // answer the same three cases in two different shapes
  private def isSupported(unscaled: Long, scale: Int): Boolean =
    scale >= 0 && scale <= MAX_SCALE && unscaled <= MAX_UNSCALED && unscaled >= -MAX_UNSCALED

  // brings a scale or a magnitude outside the supported range into it, or reports why it
  // cannot, answering an outcome for the factories that answer one
  private def ofScaledAdjusted(unscaled: Long, scale: Int): Either[Failure, Decimal] =
    adjusted(unscaled, scale, heldAsOutcome, rejectedAsOutcome)

  // the same adjustment answering the decimal itself, for the arithmetic, which is total in
  // its signature and reports a value it cannot hold through the check that raises
  private def scaledAdjustedOrFail(unscaled: Long, scale: Int): Decimal =
    adjusted(unscaled, scale, heldAsDecimal, rejectedAsRaise)

  // the adjustment itself, written once for the two shapes above.
  //
  // The algorithm reaches one of two endings - a decimal it could hold, or a failure saying
  // why it could not - and the two callers differ only in what they do with each ending: a
  // factory wraps them in an outcome, the arithmetic answers the decimal and raises the
  // failure. Passing the endings in keeps one copy of an algorithm whose every branch is
  // load-bearing, and costs no allocation, because each ending is one of the four values
  // below rather than a closure built per call.
  //
  // @param unscaled  the unscaled value to bring into range
  // @param scale  the scale to bring into range
  // @param held  what to do with a decimal the type can hold
  // @param rejected  what to do with a value it cannot
  @tailrec
  private def adjusted[A](
      unscaled: Long,
      scale: Int,
      held: Decimal => A,
      rejected: Failure => A): A =

    if (scale >= 2 * MAX_SCALE) {
      // truncation at the maximum scale leaves nothing of a value this small
      held(ZERO)
    } else if (scale <= -MAX_SCALE) {
      rejected(precisionAtScaleZeroFailure(exponentText(unscaled, scale)))
    } else if (unscaled > MAX_UNSCALED || unscaled < -MAX_UNSCALED) {
      if (scale == 0) {
        rejected(precisionAtScaleZeroFailure(exponentText(unscaled, scale)))
      } else {
        // a Long holds 19 digits, so dropping one digit always reaches the supported precision
        adjusted(unscaled / 10L, scale - 1, held, rejected)
      }
    } else if (scale < 0) {
      val power = POWERS(-scale)
      if (math.abs(unscaled) > Long.MaxValue / power) {
        // the magnitude checked above keeps this division exact, so it decides the overflow of
        // the multiplication below without performing it
        rejected(precisionFailure(exponentText(unscaled, scale)))
      } else {
        // recurse so that the product is checked against the precision supported at scale zero
        adjusted(unscaled * power, 0, held, rejected)
      }
    } else if (scale > MAX_SCALE) {
      val truncated = unscaled / POWERS(scale - MAX_SCALE)
      if (truncated == 0L) held(ZERO) else held(create(truncated, MAX_SCALE))
    } else {
      held(create(unscaled, scale))
    }

  // the four endings of the adjustment, each held as one value so that neither caller
  // allocates to say what its ending is
  private val heldAsOutcome: Decimal => Either[Failure, Decimal] = value => Right(value)

  private val rejectedAsOutcome: Failure => Either[Failure, Decimal] = failure => Left(failure)

  private val heldAsDecimal: Decimal => Decimal = value => value

  private val rejectedAsRaise: Failure => Decimal = failure => raise(failure)

  // creates an instance, removing any trailing zero of the fractional part
  @tailrec
  private def create(unscaled: Long, scale: Int): Decimal =
    if (scale > 0 && (unscaled % 10L) == 0L) {
      create(unscaled / 10L, scale - 1)
    } else {
      newDecimal(unscaled, scale)
    }

  // the one place a decimal is instantiated, reached only from a factory of this companion
  private def newDecimal(unscaled: Long, scale: Int): Decimal = new Decimal(unscaled, scale) {}

  //-------------------------------------------------------------------------
  // the arithmetic edge: a result too large is a broken precondition rather than a value, so
  // the failure is reported through the one object of this module that raises
  //
  // Every path that raises ends here, and no path that succeeds passes through it: the checks
  // of the arithmetic test their condition first and call this only in the branch where the
  // condition does not hold, so a successful operation builds neither an outcome to unwrap nor
  // a message it will not use.
  private def raise(failure: Failure): Decimal = {
    ArgCheck.isTrue(false, failure.message)
    // the check above never returns when its condition is false; this value exists only
    // because the check is typed as Unit rather than as Nothing
    ZERO
  }

  // takes the value of an outcome, reporting a failure as the broken precondition it is; used
  // where the value was produced by a factory that answers an outcome, which is every path
  // computed in BigDecimal
  private def orFail(result: Either[Failure, Decimal]): Decimal =
    result match {
      case Right(value) => value
      case Left(failure) => raise(failure)
    }

  // as of(Long), answering the decimal itself: the whole-number arithmetic of the two
  // large-operand branches finishes here, and a total beyond the supported precision is
  // reported with the message that factory reports
  private def wholeOrFail(value: Long): Decimal =
    if (value <= MAX_UNSCALED && value >= -MAX_UNSCALED) {
      newDecimal(value, 0)
    } else {
      raise(precisionAtScaleZeroFailure(value.toString))
    }

  // as ofScaled, answering the decimal itself: the arithmetic of the type is total in its
  // signature, so every case answers a decimal directly and a result too large is reported
  // through the check above rather than returned
  private def scaledOrFail(unscaled: Long, scale: Int): Decimal =
    if (unscaled == 0L) {
      ZERO
    } else if (isSupported(unscaled, scale)) {
      create(unscaled, scale)
    } else {
      scaledAdjustedOrFail(unscaled, scale)
    }

  // adds two decimals given their parts, answering the sum or reporting that it is too large
  private def sumOrFail(unscaled1: Long, scale1: Int, unscaled2: Long, scale2: Int): Decimal =
    if (scale1 == scale2) {
      // two values of at most 18 digits sum within a Long, so this addition cannot overflow
      scaledOrFail(unscaled1 + unscaled2, scale1)
    } else if (scale1 > scale2) {
      sumSortedOrFail(unscaled1, scale1, unscaled2, scale2)
    } else {
      sumSortedOrFail(unscaled2, scale2, unscaled1, scale1)
    }

  // adds two decimals given their parts, where the first scale is the greater of the two
  private def sumSortedOrFail(unscaled1: Long, scale1: Int, unscaled2: Long, scale2: Int): Decimal = {
    val scaleDiff = scale1 - scale2
    if (scaleDiff < MAX_SCALE && math.abs(unscaled2) < POWERS(MAX_SCALE - scaleDiff - 1)) {
      // the second value rescales inside 18 digits, so the sum also stays inside a Long
      scaledOrFail(unscaled1 + (unscaled2 * POWERS(scaleDiff)), scale1)
    } else {
      // the scales are too far apart to bring together in a Long
      orFail(of(BigDecimal.valueOf(unscaled1, scale1).add(BigDecimal.valueOf(unscaled2, scale2))))
    }
  }

  // true when a sum already computed in Long arithmetic is the exact sum of its operands
  private def isExactSum(a: Long, b: Long, total: Long): Boolean =
    // the sum overflowed exactly when it differs in sign from both operands
    ((a ^ total) & (b ^ total)) >= 0L

  // true when a difference already computed in Long arithmetic is the exact difference
  private def isExactDifference(a: Long, b: Long, difference: Long): Boolean =
    // the difference overflowed exactly when the operands differ in sign and the result differs
    // in sign from the first of them
    ((a ^ b) & (a ^ difference)) >= 0L

  // true when a product already computed in Long arithmetic is the exact product
  //
  // Dividing the product back by the second operand recovers the first exactly when nothing
  // was lost, so the caller must have established that the second operand is not zero -
  // `multipliedBy(Long)` does so in the branch that reaches this. The one product that
  // divides back cleanly and is still wrong is Long.MinValue times minus one, whose division
  // overflows to Long.MinValue again; the second clause turns that case away. No decimal
  // holds Long.MinValue as its unscaled part - the precision of the type stops a digit short
  // of it - so the clause cannot fire from the arithmetic of this file, and it is kept
  // because it is part of the check being ported rather than because a value reaches it.
  private def isExactProduct(a: Long, b: Long, product: Long): Boolean =
    (product / b) == a && !(a == Long.MinValue && b == -1L)

  // the exponent of a value that is a power of ten within the supported scale, -1 for any other
  private def powerOfTenIndex(value: Long): Int = powerOfTenIndexFrom(value, 0)

  @tailrec
  private def powerOfTenIndexFrom(value: Long, index: Int): Int =
    if (index > MAX_SCALE) {
      -1
    } else if (POWERS(index) == value) {
      index
    } else {
      powerOfTenIndexFrom(value, index + 1)
    }

  // renders an unscaled value and a scale the way a value out of range is reported
  private def exponentText(unscaled: Long, scale: Int): String = s"${unscaled}E${-scale}"

  private def precisionAtScaleZeroFailure(text: String): Failure =
    Failure.Invalid(s"Decimal value must not exceed $MAX_PRECISION digits of precision at scale 0: $text")

  private def precisionFailure(text: String): Failure =
    Failure.Invalid(s"Decimal value must not exceed $MAX_PRECISION digits of precision: $text")

  // describes the overflow of an operation on a value beyond the precision of a decimal
  private def arithmeticFailure(unscaled: Long, operator: String, other: Long): Failure =
    precisionAtScaleZeroFailure(s"$unscaled $operator $other")

  //-------------------------------------------------------------------------
  /**
   * The ordering of decimals, which is also their hashing.
   *
   * This is the only equality-bearing instance of the type. `Order` and `Hash` both extend
   * `Eq`, so declaring one value that is both leaves no way for comparison, hashing and
   * equality to disagree, and `Eq[Decimal]` is obtained from it by subtyping rather than being
   * declared again.
   *
   * Comparison is numerical and equality is that of the unscaled value and the scale together.
   * The two agree because every decimal is normalised: `compare` returns zero exactly when the
   * two decimals are equal, so no tie-break over further fields is needed here.
   *
   * @return the ordering and hashing of decimals
   */
  implicit val order: Order[Decimal] with Hash[Decimal] =
    new Order[Decimal] with Hash[Decimal] {

      private val universal: Hash[Decimal] = Hash.fromUniversalHashCode[Decimal]

      override def compare(x: Decimal, y: Decimal): Int = x.compareTo(y)

      override def eqv(x: Decimal, y: Decimal): Boolean = universal.eqv(x, y)

      override def hash(x: Decimal): Int = universal.hash(x)
    }

  /**
   * The rendering of a decimal as text.
   *
   * A decimal shows in its canonical plain form, which is what `toString` renders.
   *
   * @return the rendering of a decimal
   */
  implicit val show: Show[Decimal] = Show.show(_.toString)

  // the JSON form of a decimal is its canonical text rather than a JSON number, because a JSON
  // number is read back through a Double by many parsers and that would lose the exactness the
  // type exists to provide
  private val jsonCodec: Codec[Decimal] = Codecs.parsedStringCodec(parse, _.toString)

  /**
   * The JSON encoder of a decimal, writing its canonical text.
   *
   * @return the encoder of a decimal
   */
  implicit val encoder: Encoder[Decimal] = jsonCodec

  /**
   * The JSON decoder of a decimal, reading the text through `parse`.
   *
   * Text that names no decimal is rejected carrying the message of the parse failure, in the
   * shape every text-identified type of this library rejects its input.
   *
   * @return the decoder of a decimal
   */
  implicit val decoder: Decoder[Decimal] = jsonCodec
}
