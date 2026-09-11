/*
 * Copyright (C) 2022 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import java.math.BigDecimal

import cats.Hash
import cats.Order
import cats.Show
import cats.syntax.apply._

import _root_.io.circe.Codec
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder

import com.opengamma.strata.collect.json.Codecs

/**
 * A [[Decimal]] paired with the scale at which it is to be seen.
 *
 * A decimal is normalised, so `12.30` and `12.3` are one value holding one scale, and that is
 * what makes its equality, hashing and comparison agree. Money does not work that way: a
 * balance of twelve pounds thirty is shown with two decimal places whether or not the second
 * of them is a zero, and a price quoted to five decimal places is quoted to five of them even
 * when it lands on a round number. This type carries that second piece of information - the
 * scale the value is presented at - alongside the value itself, which is why it is the type
 * monetary amounts are built from.
 *
 * The fixed scale is never smaller than the scale of the decimal, since a fixed-scale decimal
 * shows every digit the decimal holds and dropping one would change the value shown. It is
 * never larger than 18, the largest scale a decimal has. Within those bounds it is free, and
 * two instances holding the same decimal at different scales are different values: `12.3` at
 * two decimal places shows `12.30` and at four shows `12.3000`.
 *
 * ===Failure===
 *
 * Pairing a decimal with a scale can fail on the scale supplied, so [[FixedScaleDecimal.of]]
 * and [[FixedScaleDecimal.parse]] answer with a chain of failures rather than raising, and
 * [[map]] - which is `of` applied to the result of a function - answers the same way. The
 * arithmetic of the underlying decimal is reached through `map`, so a caller rounds the
 * decimal to the scale it wants before pairing it:
 *
 * {{{
 * price.map(_.roundToScale(2, RoundingMode.HALF_UP))
 * }}}
 *
 * ===Text===
 *
 * `toString` shows the decimal with at least the fixed number of decimal places, padding the
 * fraction with zeroes where the decimal holds fewer. That single canonical form is what
 * `Show` shows, what the JSON codec writes, and what [[FixedScaleDecimal.parse]] reads back -
 * recovering the fixed scale from the number of digits written after the point, so that a
 * value and its text hold the same scale.
 *
 * @param decimal  the underlying decimal, whose scale is at most the fixed scale
 * @param fixedScale  the scale the value is presented at, from the scale of the decimal to 18
 */
sealed abstract case class FixedScaleDecimal private (decimal: Decimal, fixedScale: Int) {

  //-------------------------------------------------------------------------
  /**
   * Returns this value with its decimal replaced by the result of a function.
   *
   * This is how the arithmetic of [[Decimal]] is reached: the function is applied to the
   * underlying decimal and the result is paired with this fixed scale. The result therefore
   * has to fit that scale, and where it does not - a function that divides `1.00` by three
   * produces more decimal places than two - the outcome carries the failure saying so
   * rather than silently dropping a digit.
   *
   * @param fn  the function to apply to the underlying decimal
   * @return the value with the function applied, or the failures describing why the result
   *   does not fit this scale
   */
  def map(fn: Decimal => Decimal): ResultNec[FixedScaleDecimal] =
    FixedScaleDecimal.of(fn(decimal), fixedScale)

  /**
   * Returns this value as a `BigDecimal` of the fixed scale.
   *
   * The scale of the value returned is the fixed scale rather than the scale of the
   * underlying decimal, so the text of the two agrees: a decimal of `12.3` presented at two
   * decimal places converts to a `BigDecimal` reading `12.30`.
   *
   * @return the equivalent `BigDecimal`, with a scale equal to the fixed scale
   */
  def toBigDecimal: BigDecimal = decimal.toBigDecimal.setScale(fixedScale)

  //-------------------------------------------------------------------------
  /**
   * Compares this value to another, by value and then by scale.
   *
   * The comparison is that of the underlying decimals, so `12.30` at two decimal places and
   * `12.3` at one compare as the same amount of money; where the amounts are equal the
   * scales decide, so that two values compare equal only when they are equal. The type
   * being ported compared the amounts alone and left the scale out.
   *
   * @param other  the value to compare to
   * @return negative when this is the smaller, zero when the two are equal, positive
   *   otherwise
   */
  def compareTo(other: FixedScaleDecimal): Int = {
    val byDecimalComparison = decimal.compareTo(other.decimal)
    if (byDecimalComparison != 0) byDecimalComparison else fixedScale.compareTo(other.fixedScale)
  }

  /**
   * Returns the plain text of this value, showing the fixed number of decimal places.
   *
   * @return the decimal, formatted with at least the fixed number of decimal places
   */
  override def toString: String = decimal.formatAtLeast(fixedScale)
}

/**
 * Holds the factories, the instances and the codec of [[FixedScaleDecimal]].
 */
object FixedScaleDecimal {

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance from a decimal and the scale it is presented at.
   *
   * The fixed scale must be at least the scale of the decimal, since a fixed-scale decimal
   * shows every digit the decimal holds; round the decimal first where its scale is not
   * known to be small enough. It must also be 18 or less, the largest scale a decimal has.
   *
   * Both causes of rejection are reported together rather than the first of them alone, so a
   * caller correcting a scale sees everything wrong with it at once:
   *
   * {{{
   * result.toNec(Decimal.parse("12.3")).flatMap(FixedScaleDecimal.of(_, 2))
   * // Right, showing 12.30; the same with "12.345" is Left, since that scale drops a digit
   * }}}
   *
   * @param decimal  the underlying decimal
   * @param fixedScale  the scale to present the decimal at, from the scale of the decimal to 18
   * @return the fixed-scale decimal, or the failures describing why that scale cannot hold it
   */
  def of(decimal: Decimal, fixedScale: Int): ResultNec[FixedScaleDecimal] =
    (checkedNotBelowDecimal(decimal, fixedScale), checkedWithinMaximum(fixedScale))
      .mapN((_, _) => new FixedScaleDecimal(decimal, fixedScale) {})
      .toEither

  /**
   * Parses an instance from text.
   *
   * This reads the form `toString` produces: a plain decimal, with no exponent, whose digits
   * after the point give the fixed scale. Text holding no point names a value of scale zero,
   * and text holding trailing zeroes after the point keeps them as scale - which is the
   * whole point of the type, and the reason parsing text is not the same as parsing a
   * decimal and choosing a scale afterwards.
   *
   * {{{
   * FixedScaleDecimal.parse("12.30")  // 12.3 presented at two decimal places
   * FixedScaleDecimal.parse("12")     // 12 presented at no decimal places
   * }}}
   *
   * Text is rejected either because its digits name no decimal or because the scale they
   * imply is one no decimal can hold, and the outcome carries whichever of those applies in
   * the same chain of failures [[FixedScaleDecimal.of]] produces. A caller therefore handles
   * one shape of outcome whichever of the two factories it reached the value through, and no
   * cause is dropped on the way.
   *
   * The text is read as a decimal first and only then scanned for the point, which is the
   * order the type being ported reads it in. The bound a decimal applies to its own text -
   * rejecting text longer than it accepts without looking at any of it - therefore holds
   * before the text is scanned, so oversized text is turned away rather than measured, and
   * the scale is derived only from text that named a decimal.
   *
   * @param str  the text to parse
   * @return the fixed-scale decimal, or the failures describing why the text names none
   */
  def parse(str: String): ResultNec[FixedScaleDecimal] =
    result.toNec(Decimal.parse(str)).flatMap { decimal =>
      val pointPosition = str.lastIndexOf('.')
      val impliedScale = if (pointPosition < 0) 0 else str.length - pointPosition - 1
      of(decimal, impliedScale)
    }

  //-------------------------------------------------------------------------
  /**
   * Checks that the scale supplied shows every digit the decimal holds.
   */
  private def checkedNotBelowDecimal(decimal: Decimal, fixedScale: Int): ValidatedFailures[Int] =
    Validate
      .isTrue(
        fixedScale >= decimal.scale,
        s"Scale must be equal or greater than the scale of the decimal: $fixedScale")
      .map(_ => fixedScale)

  /**
   * Checks that the scale supplied is one a decimal can hold.
   */
  private def checkedWithinMaximum(fixedScale: Int): ValidatedFailures[Int] =
    Validate
      .isTrue(fixedScale <= Decimal.MAX_SCALE, s"Scale must be 18 or less: $fixedScale")
      .map(_ => fixedScale)

  //-------------------------------------------------------------------------
  /**
   * The ordering of fixed-scale decimals, which is also their hashing.
   *
   * Values sort by amount and then by scale, as [[FixedScaleDecimal.compareTo]] describes.
   * This is the only equality-bearing instance of the type: `Order` and `Hash` both extend
   * `Eq`, so the three can never disagree, and because the comparison falls through to the
   * scale it returns zero exactly when two values are equal.
   *
   * @return the ordering of fixed-scale decimals
   */
  implicit val order: Order[FixedScaleDecimal] with Hash[FixedScaleDecimal] =
    new Order[FixedScaleDecimal] with Hash[FixedScaleDecimal] {

      private val universal: Hash[FixedScaleDecimal] = Hash.fromUniversalHashCode[FixedScaleDecimal]

      override def compare(x: FixedScaleDecimal, y: FixedScaleDecimal): Int = x.compareTo(y)

      override def eqv(x: FixedScaleDecimal, y: FixedScaleDecimal): Boolean = universal.eqv(x, y)

      override def hash(x: FixedScaleDecimal): Int = universal.hash(x)
    }

  /**
   * The text of a fixed-scale decimal, which is the plain form `toString` produces.
   *
   * @return the rendering of fixed-scale decimals
   */
  implicit val show: Show[FixedScaleDecimal] = Show.show(_.toString)

  /**
   * The JSON form of a fixed-scale decimal, which is the text of its canonical form.
   *
   * A fixed-scale decimal is written as the string `toString` produces and read back with
   * [[FixedScaleDecimal.parse]], so the scale travels with the value in the digits written
   * after the point. Writing it as a JSON number would lose exactly that, since `12.30` and
   * `12.3` are one number.
   *
   * Rejected text is reported by the shared codec support rather than by anything written
   * here, so a document naming an impossible value fails in the one shape every text-valued
   * type of this library fails in.
   */
  private val jsonCodec: Codec[FixedScaleDecimal] = Codecs.parsedStringCodecNec(parse, _.toString)

  /**
   * The JSON encoder of a fixed-scale decimal.
   *
   * @return the encoder writing the canonical text
   */
  implicit val encoder: Encoder[FixedScaleDecimal] = jsonCodec

  /**
   * The JSON decoder of a fixed-scale decimal.
   *
   * @return the decoder reading the canonical text
   */
  implicit val decoder: Decoder[FixedScaleDecimal] = jsonCodec
}
