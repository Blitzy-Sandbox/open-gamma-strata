/*
 * Copyright (C) 2022 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import java.math.BigDecimal
import java.math.MathContext
import java.math.RoundingMode

import scala.collection.immutable.List
import scala.util.Random
import scala.util.Try

import cats.Eq
import cats.Hash
import cats.Order
import cats.Show
import cats.syntax.all._

// the package of this file holds a sub-package named `io`, which shadows the top-level `io`
// package that circe lives in, so circe is reached from the root as the main sources reach it
import _root_.io.circe.parser
import _root_.io.circe.syntax._

import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor1
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableFor3
import org.scalatest.prop.TableFor4
import org.scalatest.prop.TableFor6
import org.scalatest.prop.TableFor7
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import com.opengamma.strata.collect.Arbitraries._
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Tests [[Decimal]], the exact decimal number of this module.
 *
 * ===What is under test===
 *
 * Every member of the type and of its companion: the six factories, the normalisation they
 * all share, the total arithmetic, the two rounding methods, the queries, the conversions,
 * the two formatting methods, the comparison, the canonical text, and the instances -
 * `Order` with `Hash`, `Show` and the JSON codec.
 *
 * ===Where the expected values come from===
 *
 * The tables and the assertions of this spec are transcribed from the test class of the type
 * being ported, `modules/collect/src/test/java/com/opengamma/strata/collect/DecimalTest.java`,
 * whose seven `@MethodSource` tables appear here as the seven tables below with their rows
 * unchanged. Those rows are the behavioural specification of the type rather than a
 * convenience: a row that disagrees with this port is evidence about the port, never a value
 * to be adjusted until the spec passes. Several assertions are additionally checked against
 * `java.math.BigDecimal` computing the same thing a second way, so a row and the
 * implementation cannot drift together.
 *
 * Three groups of cases are checked against `BigDecimal` rather than against a literal,
 * because the literal would be the thing under test: the rounding cases compare all seven
 * rounding modes with `BigDecimal.setScale`, the fuzz case compares ten thousand seeded
 * random operations with the equivalent `BigDecimal` arithmetic, and the four
 * throughput cases of the ported class are reduced to the equivalence they were implicitly
 * asserting.
 *
 * ===The two ways this type reports a problem===
 *
 * The port draws a line the ported class did not have to draw, and this spec asserts which
 * side of it every failing case falls on:
 *
 *   - '''Building''' a decimal can fail on the data supplied, so every factory answers with
 *     `Either[Failure, Decimal]`. Text naming no number, text that is too long, a value that
 *     is not finite and a magnitude needing more than eighteen digits are all `Left`, and
 *     this spec asserts the reason as well as the failure. The ported class threw
 *     `NumberFormatException` or `IllegalArgumentException` for these.
 *   - '''Arithmetic''' keeps the total signature of the ported class - `plus`, `minus`,
 *     `multipliedBy`, `dividedBy`, `remainder` and `movePoint` all answer with a `Decimal` -
 *     so a result that does not fit is a broken precondition and raises
 *     `IllegalArgumentException` through [[ArgCheck]]. Division by zero raises the
 *     `ArithmeticException` of the platform. Both are documented on the methods that have
 *     them and both are asserted here, because a failing arithmetic case turned into a
 *     `Left` would be a silent change to the shape of the type.
 *
 * Nothing in this spec asserts an exception from a factory and nothing asserts a `Left` from
 * arithmetic; that split is the point of the two sections named for it.
 *
 * ===Throughput cases carry no timing===
 *
 * Four methods of the ported class are throughput harnesses: they run an operation fifty
 * million times against `BigDecimal` and against the decimal and print the two durations,
 * asserting nothing, and they are annotated `@Disabled` so they never ran in the first place.
 * A wall-clock assertion is not a test - it fails on a loaded machine and passes on an idle
 * one - so what is ported here is the equivalence each harness implicitly relied on: over the
 * same inputs, the decimal produces what `BigDecimal` produces. The loops are short and the
 * inputs are the literal ones of the harness, so these cases are as deterministic as the rest
 * of the spec.
 *
 * The one section that does read the clock reads it for a different question. A numeral can
 * name a number of hundreds of millions of digits, and converting such a value by expanding it
 * first would cost time proportional to the number named rather than to the text supplied;
 * the bound asserted there separates microseconds from minutes, so it decides whether the work
 * is bounded rather than how fast the machine is.
 *
 * @see [[FixedScaleDecimalSpec]] for the decimal of fixed scale built on this type
 */
class DecimalSpec
    extends AnyFunSuite
    with Matchers
    with TableDrivenPropertyChecks
    with ScalaCheckPropertyChecks {

  //-------------------------------------------------------------------------
  // Helpers. A factory of this type answers with an outcome, so every construction in this
  // spec goes through one of these rather than through a partial accessor: a factory that
  // unexpectedly rejects its input is reported as a test failure naming the reason, which is
  // what tells a reader whether the spec or the port is at fault.

  /**
   * Reads the decimal out of an outcome that is expected to hold one.
   *
   * The outcome is matched rather than unwrapped, so an unexpected rejection names the
   * failure that caused it instead of raising from a partial accessor.
   *
   * @param outcome  the outcome of a factory, expected to hold a decimal
   * @return the decimal it holds
   */
  private def decimalOf(outcome: FailureOr[Decimal]): Decimal =
    outcome match {
      case Right(value) => value
      case Left(failure) =>
        fail(s"Expected a decimal but the factory rejected its input: ${failure.message}")
    }

  /** Builds a decimal from an unscaled value and a scale, the widest route into the type. */
  private def scaled(unscaled: Long, scale: Int): Decimal =
    decimalOf(Decimal.ofScaled(unscaled, scale))

  /** Builds a decimal from its text. */
  private def text(str: String): Decimal = decimalOf(Decimal.of(str))

  /** Builds a decimal from a whole number. */
  private def whole(value: Long): Decimal = decimalOf(Decimal.of(value))

  /** Builds a decimal from a `Double`. */
  private def fromDouble(value: Double): Decimal = decimalOf(Decimal.of(value))

  /** Builds a decimal from a `BigDecimal`. */
  private def fromBigDecimal(value: BigDecimal): Decimal = decimalOf(Decimal.of(value))

  /**
   * The digit the ported table uses to prove that the text of a decimal is read with the
   * digit rules of `BigDecimal` rather than with an ASCII-only scanner.
   *
   * The ported table spells this row with a `\u0661` escape. It is built from the code point
   * here instead, because a unicode escape in Scala source is processed by the scanner and is
   * deprecated, and this build turns a deprecation into an error.
   */
  private val ArabicIndicOne: String = 0x0661.toChar.toString

  /**
   * The texts the four throughput cases of the ported class cycle through.
   *
   * Kept as one value because all four harnesses use the same ten literals, and covering a
   * positive integer, four lengthening fractions, a negative integer, three negative
   * fractions and zero is exactly what makes them a reasonable equivalence fixture.
   */
  private val ThroughputTexts: List[String] =
    List("1", "1.2", "1.23", "1.234", "1.2345", "-1", "-2.3", "-3.4", "-4.5", "0")

  /** The rounding modes, excluding the one that refuses to round. */
  private val RoundingModes: List[RoundingMode] =
    RoundingMode.values.toList.filterNot(mode => mode == RoundingMode.UNNECESSARY)

  /**
   * The three values of a `Double` that name no decimal, each with the message it is reported
   * by.
   *
   * These are the only values a `Double` takes that are not finite, and every arithmetic
   * method taking a `Double` converts its operand before computing, so the same three cases
   * apply to all four of them. The message is the literal text of the report rather than a
   * string this spec builds the way the implementation builds it, so a change to either the
   * text or the branch producing it fails these cases instead of passing quietly.
   */
  private val nonFiniteValues: TableFor2[Double, String] = Table(
    ("value", "message"),
    (Double.NaN, "Decimal value must be finite: NaN"),
    (Double.PositiveInfinity, "Decimal value must be finite: Infinity"),
    (Double.NegativeInfinity, "Decimal value must be finite: -Infinity"))

  //-------------------------------------------------------------------------
  // The seven tables of the ported test class, with their rows unchanged.

  /**
   * Text, unscaled value and scale of the same decimal, reached three ways.
   *
   * Every row is a decimal that the type holds exactly, so the row doubles as the expected
   * normalisation of its own text: `120.00` normalises to unscaled 120 at scale 0, and the
   * rows whose text carries more than eighteen digits of precision state the truncation.
   *
   * Ported from `dataValues`.
   */
  private val values: TableFor3[String, Long, Int] = Table(
    ("text", "unscaled", "scale"),
    ("1", 1L, 0),
    ("1.2", 12L, 1),
    ("1.23", 123L, 2),
    ("1.1234567890123456", 11234567890123456L, 16),
    ("1.12345678901234567", 112345678901234567L, 17),
    ("1.123456789012345678", 112345678901234567L, 17),
    ("1.1234567890123456789", 112345678901234567L, 17),
    ("0.1234567890123456", 1234567890123456L, 16),
    ("0.12345678901234567", 12345678901234567L, 17),
    ("0.123456789012345678", 123456789012345678L, 18),
    ("0.1234567890123456789", 123456789012345678L, 18),
    ("0.01234567890123456", 1234567890123456L, 17),
    ("0.012345678901234567", 12345678901234567L, 18),
    ("0.0123456789012345678", 12345678901234567L, 18),
    ("0.01234567890123456789", 12345678901234567L, 18),
    ("123456789012345678.123456789012345678", 123456789012345678L, 0),
    ("120.00", 120L, 0),
    ("1.20", 12L, 1),
    (".123", 123L, 3),
    ("-.123", -123L, 3),
    ("0.123", 123L, 3),
    ("-0.123", -123L, 3),
    ("0.0123", 123L, 4),
    ("-0.0123", -123L, 4),
    ("2000000", 2000000L, 0),
    ("2000000.000", 2000000L, 0),
    ("2000000.000000000", 2000000L, 0),
    ("2000000.00000000000000000", 2000000L, 0),
    ("2000000.000000000000000000", 2000000L, 0),
    ("2000000.0000000000000000000", 2000000L, 0),
    ("2000000.00000000000000000000", 2000000L, 0),
    ("123E-2", 123L, 2),
    ("123E-0", 123L, 0),
    ("123E0", 123L, 0),
    ("123E+0", 123L, 0),
    ("123E+2", 12300L, 0),
    ("123.456E2", 123456L, 1),
    ("123.456e3", 123456L, 0),
    (ArabicIndicOne + "23", 123L, 0),
    ("123456789012345678.9", 123456789012345678L, 0))

  /**
   * Whole numbers spanning zero, both signs and the eighteen-digit limit.
   *
   * Ported from `dataLongValues`.
   */
  private val longValues: TableFor1[Long] = Table(
    "value",
    0L,
    123L,
    -456L,
    123456789012345678L)

  /**
   * Two decimals and their exact sum, each given as an unscaled value and a scale.
   *
   * The last three rows are the ones that matter most: they add a value at scale 18, 17 and
   * 16 to a whole number, which is where the sum has to be formed without losing a digit and
   * where the truncation at the maximum scale becomes visible.
   *
   * Ported from `dataPlus`.
   */
  private val plusValues: TableFor6[Long, Int, Long, Int, Long, Int] = Table(
    ("unscaled1", "scale1", "unscaled2", "scale2", "expectedUnscaled", "expectedScale"),
    (0L, 0, 2L, 0, 2L, 0),
    (2L, 0, 0L, 0, 2L, 0),
    (2L, 0, 3L, 0, 5L, 0),
    (999L, 0, 999L, 0, 1998L, 0),
    (999999999999999998L, 0, 1L, 0, 999999999999999999L, 0),
    (999999999999999999L, 0, -999999999999999999L, 0, 0L, 0),
    (23L, 1, 1L, 0, 33L, 1),
    (23L, 1, 11L, 0, 133L, 1),
    (23L, 1, 11L, 1, 34L, 1),
    (1L, 18, 1L, 0, 1L, 0),
    (1L, 17, 1L, 0, 100000000000000001L, 17),
    (1L, 16, 1L, 0, 10000000000000001L, 16))

  /**
   * Two decimals and their product, each given as an unscaled value and a scale.
   *
   * The scales add, so these rows cover a product that stays inside the precision of the
   * type, one that is truncated at the maximum scale, and one that vanishes below it.
   *
   * Ported from `dataMultipliedBy`. Three rows of the ported table carry a seventh element
   * that its test method does not declare a parameter for and therefore never reads; those
   * rows appear here with the six columns the assertions use.
   */
  private val multipliedByValues: TableFor6[Long, Int, Long, Int, Long, Int] = Table(
    ("unscaled1", "scale1", "unscaled2", "scale2", "expectedUnscaled", "expectedScale"),
    (0L, 0, 2L, 0, 0L, 0),
    (2L, 0, 0L, 0, 0L, 0),
    (2L, 0, 1L, 0, 2L, 0),
    (2L, 0, 3L, 0, 6L, 0),
    (999999999999999999L, 0, 1L, 0, 999999999999999999L, 0),
    (2L, 2, 3L, 5, 6L, 7),
    (2L, 10, 3L, 9, 0L, 0),
    (11L, 18, 2L, 1, 2L, 18),
    (111111111111111111L, 18, 30L, 0, 333333333333333333L, 17),
    (111111111111111111L, 18, 33L, 0, 366666666666666666L, 17),
    (999999999999999999L, 3, 10L, 0, 999999999999999999L, 2),
    (2L, 18, 10L, 0, 2L, 17),
    (2L, 18, 100L, 0, 2L, 16),
    (2L, 18, 100000000000000000L, 0, 2L, 1))

  /**
   * A dividend, a divisor, the truncated quotient and the unscaled value the same quotient
   * has when the eighteenth significant digit is rounded half away from zero instead.
   *
   * The truncated and rounded columns differ in exactly the rows where the quotient does not
   * terminate inside eighteen digits, which is what makes the pair worth tabulating: `2 / 3`
   * truncates to a string of sixes and rounds to a seven in the last place.
   *
   * Ported from `dataDividedBy`.
   */
  private val dividedByValues: TableFor7[Long, Int, Long, Int, Long, Int, Long] = Table(
    (
      "unscaled1",
      "scale1",
      "unscaled2",
      "scale2",
      "truncatedUnscaled",
      "expectedScale",
      "roundedUnscaled"),
    (0L, 0, 2L, 0, 0L, 0, 0L),
    (2L, 0, 1L, 0, 2L, 0, 2L),
    (2L, 0, 2L, 0, 1L, 0, 1L),
    (2L, 0, 4L, 0, 5L, 1, 5L),
    (2L, 0, 3L, 0, 666666666666666666L, 18, 666666666666666667L),
    (999999999999999999L, 0, 1L, 0, 999999999999999999L, 0, 999999999999999999L),
    (999999999999999999L, 0, 2L, 0, 499999999999999999L, 0, 500000000000000000L),
    (99999999999999999L, 0, 2L, 0, 499999999999999995L, 1, 499999999999999995L),
    (2L, 0, 10L, 0, 2L, 1, 2L),
    (2L, 0, 100L, 0, 2L, 2, 2L),
    (2L, 0, 100000000000000000L, 0, 2L, 17, 2L))

  /**
   * A decimal and a scale to round it to, covering the whole range of scales.
   *
   * The scales run from a hundred - far above the precision of the type, so rounding is a
   * no-op - down to -17, where every digit of the value lies below the requested scale. The
   * expected value is not tabulated: each row is checked against `BigDecimal.setScale` under
   * every rounding mode, which is a stronger statement than any single literal and is how the
   * ported test states it.
   *
   * Ported from `dataRoundToScale`.
   */
  private val roundToScaleValues: TableFor2[String, Int] = Table(
    ("text", "desiredScale"),
    ("0", 2),
    ("0", 0),
    ("0", -2),
    ("1.234", 2),
    ("1.235", 2),
    ("1.236", 2),
    ("123.45", 100),
    ("123.45", 18),
    ("123.45", 17),
    ("123.45", 3),
    ("123.45", 2),
    ("123.45", 1),
    ("123.45", 0),
    ("123.45", -1),
    ("123.45", -2),
    ("123.45", -3),
    ("123.45", -17),
    ("12345", -2),
    ("1.2005", 2),
    ("125.6", 0),
    ("125.6", -1),
    ("0.000005", -15))

  /**
   * A decimal, a number of decimal places, and the text of the two formatting methods.
   *
   * The two expectations differ wherever the decimal has more places than requested:
   * `format` rounds to exactly the number asked for while `formatAtLeast` shows them all,
   * which is the whole distinction between the two methods. The `-0.45` rows at zero places
   * pin the sign of a value that rounds to zero: the text is `0`, not `-0`.
   *
   * Ported from `dataFormat`.
   */
  private val formatValues: TableFor4[String, Int, String, String] = Table(
    ("text", "decimalPlaces", "expectedExact", "expectedAtLeast"),
    ("0", 0, "0", "0"),
    ("0", 1, "0.0", "0.0"),
    ("0", 2, "0.00", "0.00"),
    ("0", 18, "0.000000000000000000", "0.000000000000000000"),
    ("12.345", 0, "12", "12.345"),
    ("12.345", 1, "12.3", "12.345"),
    ("12.345", 2, "12.35", "12.345"),
    ("12.345", 3, "12.345", "12.345"),
    ("12.345", 4, "12.3450", "12.3450"),
    ("12.345", 18, "12.345000000000000000", "12.345000000000000000"),
    ("-12.345", 0, "-12", "-12.345"),
    ("-12.345", 1, "-12.3", "-12.345"),
    ("-12.345", 2, "-12.35", "-12.345"),
    ("-12.345", 3, "-12.345", "-12.345"),
    ("-12.345", 4, "-12.3450", "-12.3450"),
    ("-12.345", 18, "-12.345000000000000000", "-12.345000000000000000"),
    ("0.45", 0, "0", "0.45"),
    ("0.45", 1, "0.5", "0.45"),
    ("0.45", 2, "0.45", "0.45"),
    ("0.45", 3, "0.450", "0.450"),
    ("-0.45", 0, "0", "-0.45"),
    ("-0.45", 1, "-0.5", "-0.45"),
    ("-0.45", 2, "-0.45", "-0.45"),
    ("-0.45", 3, "-0.450", "-0.450"))

  //-------------------------------------------------------------------------
  // Construction, normalisation and text.

  /**
   * Asserts everything the ported test asserts about one row of [[values]], from whichever
   * factory built the decimal.
   *
   * The three factory cases below differ only in how they reach the decimal, so the
   * assertions live here once: the parts are what the row states, the `BigDecimal` of the
   * same parts agrees on the value, the text is the plain form of that `BigDecimal`, and the
   * `Double` is the one `BigDecimal` converts to.
   *
   * @param test  the decimal built by the factory under test
   * @param unscaled  the unscaled value the row states
   * @param scale  the scale the row states
   * @return the assertion that every part of the row holds
   */
  private def assertRow(test: Decimal, unscaled: Long, scale: Int): Assertion = {
    val expected = BigDecimal.valueOf(unscaled, scale)
    test.unscaledValue shouldBe unscaled
    test.scale shouldBe scale
    test.toBigDecimal shouldBe expected
    test.toString shouldBe expected.toPlainString
    test.doubleValue shouldBe expected.doubleValue
  }

  test("a decimal built from an unscaled value and a scale holds the parts of its row") {
    forAll(values) { (_, unscaled, scale) =>
      assertRow(scaled(unscaled, scale), unscaled, scale)
    }
  }

  test("a decimal whose text is the shortest text of its own Double is recovered from it") {
    forAll(values) { (_, unscaled, scale) =>
      val test = scaled(unscaled, scale)
      // only meaningful where the Double names this decimal exactly, which is the condition
      // the ported test guards the round trip with
      if (test.doubleValue.toString == test.toString) {
        Decimal.of(test.doubleValue) shouldBe Right(test)
      }
    }
  }

  test("a decimal equals itself, hashes consistently, and differs in either of its parts") {
    forAll(values) { (_, unscaled, scale) =>
      val test = scaled(unscaled, scale)
      test shouldBe test
      test.hashCode shouldBe scaled(unscaled, scale).hashCode
      // `equals` is called rather than a comparison operator, so the assertion is the one the
      // ported test makes without the compiler rejecting the unrelated types
      test.equals("") shouldBe false
      val differentUnscaled = if (unscaled != 0L) math.abs(unscaled) - 1L else 1L
      test should not be scaled(differentUnscaled, scale)
      test should not be scaled(unscaled, if (scale > 0) scale - 1 else 1)
    }
  }

  test("a decimal built from a BigDecimal holds the parts of its row") {
    forAll(values) { (_, unscaled, scale) =>
      assertRow(fromBigDecimal(BigDecimal.valueOf(unscaled, scale)), unscaled, scale)
    }
  }

  test("a decimal rounded to two places is presented at that fixed scale") {
    forAll(values) { (_, unscaled, scale) =>
      val base = scaled(unscaled, scale).roundToScale(2, RoundingMode.HALF_UP)
      val test = base.toFixedScale(2)
      test should beSuccess
      test.map(fixed => fixed.decimal) shouldBe Right(base)
      test.map(fixed => fixed.fixedScale) shouldBe Right(2)
    }
  }

  test("a decimal built from text holds the parts of its row and parses the same way") {
    forAll(values) { (str, unscaled, scale) =>
      assertRow(text(str), unscaled, scale)
      Decimal.parse(str) shouldBe Right(text(str))
    }
  }

  test("a leading plus sign does not change the decimal the text names") {
    forAll(values) { (str, unscaled, scale) =>
      // a row already carrying a sign is skipped, as in the ported test: two signs name no
      // number and that case belongs to the malformed-text table
      if (!str.startsWith("-") && !str.startsWith("+")) {
        assertRow(text("+" + str), unscaled, scale)
        Decimal.parse("+" + str) shouldBe Right(text(str))
      }
    }
  }

  test("a leading zero does not change the decimal the text names") {
    forAll(values) { (str, unscaled, scale) =>
      val prefixed =
        if (str.startsWith("-") || str.startsWith("+")) {
          str.substring(0, 1) + "0" + str.substring(1)
        } else {
          "0" + str
        }
      assertRow(text(prefixed), unscaled, scale)
      Decimal.parse(prefixed) shouldBe Right(text(prefixed))
    }
  }

  test("a decimal built from a whole number keeps scale zero and converts back") {
    forAll(longValues) { value =>
      val test = whole(value)
      test.unscaledValue shouldBe value
      test.scale shouldBe 0
      test.longValue shouldBe value
    }
  }

  test("precision beyond eighteen digits is truncated towards zero") {
    val test = scaled(1234567890123456789L, 1)
    test.unscaledValue shouldBe 123456789012345678L
    test.scale shouldBe 0
  }

  test("zero is held at scale zero whatever scale it arrives with") {
    scaled(0L, 0) shouldBe Decimal.ZERO
    scaled(0L, 1) shouldBe Decimal.ZERO
    scaled(0L, 17) shouldBe Decimal.ZERO
    scaled(0L, -1) shouldBe Decimal.ZERO
    scaled(0L, -1000) shouldBe Decimal.ZERO
    scaled(0L, Int.MaxValue) shouldBe Decimal.ZERO
    scaled(0L, Int.MinValue) shouldBe Decimal.ZERO
    // a value so small that nothing of it survives the maximum scale is zero as well
    scaled(11L, 20) shouldBe Decimal.ZERO
    scaled(1L, 19) shouldBe Decimal.ZERO
    scaled(2L, 36) shouldBe Decimal.ZERO
  }

  test("a trailing zero of the fraction is removed by normalisation") {
    scaled(20L, 2) shouldBe scaled(2L, 1)
    scaled(2000L, 4) shouldBe scaled(2L, 1)
    scaled(123000L, 5) shouldBe scaled(123L, 2)
    scaled(1234567890123456000L, 19) shouldBe scaled(1234567890123456L, 16)
    scaled(1000L, 20) shouldBe scaled(1L, 17)
  }

  test("a negative scale becomes digits of the whole part") {
    scaled(123L, 2) shouldBe scaled(123L, 2)
    scaled(123L, 0) shouldBe scaled(123L, 0)
    scaled(123L, -2) shouldBe scaled(12300L, 0)
  }

  test("a BigDecimal is normalised as its own parts would be") {
    Decimal.of(new BigDecimal("0.0000000000")) shouldBe Right(Decimal.ZERO)
    Decimal.of(new BigDecimal("0.123000000")) shouldBe Right(scaled(123L, 3))
    Decimal.of(new BigDecimal("-0.123000000")) shouldBe Right(scaled(-123L, 3))
    Decimal.of(new BigDecimal("0.12345678901234567890")) shouldBe
      Right(scaled(123456789012345678L, 18))
    Decimal.of(BigDecimal.valueOf(123L, 2)) shouldBe Right(scaled(123L, 2))
    Decimal.of(BigDecimal.valueOf(123L, 0)) shouldBe Right(scaled(123L, 0))
    Decimal.of(BigDecimal.valueOf(123L, -2)) shouldBe Right(scaled(12300L, 0))
  }

  test("the published zero is the decimal of no value at scale zero") {
    Decimal.ZERO shouldBe scaled(0L, 0)
  }

  test("parse reads what the text factory reads") {
    forAll(values) { (str, _, _) =>
      Decimal.parse(str) shouldBe Decimal.of(str)
    }
    Decimal.parse("1.23") shouldBe Right(scaled(123L, 2))
    Decimal.parse("") should beFailureWith(FailureReason.PARSING)
  }


  //-------------------------------------------------------------------------
  // Rejected input. Every case here is a factory, so every case is a `Left` carrying the
  // reason: text that names no number parses to nothing, and a value that no decimal can
  // hold is invalid. The ported class threw from these; nothing in this section asserts a
  // throw, and nothing in the arithmetic sections below asserts a `Left`.

  /** Text that names no number, each of which the text factory rejects as unparseable. */
  private val malformedTexts: TableFor1[String] = Table(
    "text",
    "",
    "-",
    "+",
    "\n",
    "A",
    "--123",
    "-+123",
    "++123",
    "+-123",
    "1+23",
    "123.4.56",
    "1234567890123456789A",
    "1234567890123456789-",
    // longer than the type reads, which is a rejection of the text rather than of a value
    "." + ("1" * 256))

  /** Text naming a number too large for any decimal to hold. */
  private val oversizedTexts: TableFor1[String] = Table(
    "text",
    "1234567890123456789",
    "1000000000000000000",
    "-1000000000000000000",
    "12345678901234567890")

  test("text that names no number is rejected as unparseable") {
    forAll(malformedTexts) { str =>
      Decimal.of(str) should beFailureWith(FailureReason.PARSING)
      Decimal.parse(str) should beFailureWith(FailureReason.PARSING)
    }
  }

  test("text naming a number beyond eighteen digits is rejected as invalid") {
    forAll(oversizedTexts) { str =>
      Decimal.of(str) should beFailureWith(FailureReason.INVALID)
    }
  }

  test("a Double that is not finite names no decimal") {
    Decimal.of(Double.NaN) should beFailureWith(FailureReason.INVALID)
    Decimal.of(Double.PositiveInfinity) should beFailureWith(FailureReason.INVALID)
    Decimal.of(Double.NegativeInfinity) should beFailureWith(FailureReason.INVALID)
  }

  test("a value beyond eighteen digits is rejected whichever factory is given it") {
    Decimal.of(BigDecimal.valueOf(1L, -18)) should beFailureWith(FailureReason.INVALID)
    Decimal.of(BigDecimal.valueOf(-1L, -18)) should beFailureWith(FailureReason.INVALID)
    Decimal.of(BigDecimal.valueOf(999L, -16)) should beFailureWith(FailureReason.INVALID)
    Decimal.of(BigDecimal.valueOf(-999L, -16)) should beFailureWith(FailureReason.INVALID)
    Decimal.of(BigDecimal.valueOf(1L, -19)) should beFailureWith(FailureReason.INVALID)
    Decimal.of(BigDecimal.valueOf(-1L, -19)) should beFailureWith(FailureReason.INVALID)
    Decimal.of(new BigDecimal("12345678901234567890")) should beFailureWith(FailureReason.INVALID)
    Decimal.of(new BigDecimal("912345678901234567890")) should beFailureWith(FailureReason.INVALID)
    Decimal.of(1000000000000000000.0) should beFailureWith(FailureReason.INVALID)
    Decimal.of(-1000000000000000000.0) should beFailureWith(FailureReason.INVALID)
    Decimal.of(1000000000000000000L) should beFailureWith(FailureReason.INVALID)
    Decimal.of(-1000000000000000000L) should beFailureWith(FailureReason.INVALID)
  }

  test("a rejected factory reports a reason and a message rather than raising") {
    // the shape of the rejection matters as much as its presence: a caller of a factory is
    // never expected to catch anything
    val rejected = Decimal.of("nonsense")
    rejected should beFailure
    rejected should haveFailureMessageMatching(".*nonsense.*")
    rejected.isLeft shouldBe true
  }

  //-------------------------------------------------------------------------
  // An exponent the caller names rather than supplies.
  //
  // A numeral of a dozen characters can name a number of hundreds of millions of digits, and
  // `1e500000000` is the whole of the attack: the text is read by `BigDecimal`, which holds it
  // as one digit at a scale of minus five hundred million, and bringing that to scale zero
  // materialises every zero through `BigInteger.TEN.pow`. A factory that expanded first and
  // counted afterwards would therefore answer the same rejection - eventually, having burned
  // the heap or the processor of whatever read the text - so these cases assert the rejection
  // *and* that it is reached without the expansion. The cost of the guarded path is a
  // subtraction, which is why one wall-clock bound is enough to separate the two: microseconds
  // against minutes, three orders of magnitude clear of the bound below on any machine.
  //
  // These three cases are the one place in this spec where the clock is read, and they read it
  // for a resource bound rather than for throughput: the assertion is not that the factory is
  // fast but that the work it does is proportional to the text it was given rather than to the
  // number the text names - which is why the text they use is a dozen characters long and
  // names five hundred million whole digits.

  /**
   * The bound the guarded path is asserted to stay inside, in milliseconds.
   *
   * Chosen three orders of magnitude above what the guarded path takes - a subtraction and a
   * comparison - and far below what the expansion it guards takes, so a loaded shared machine
   * cannot make this flake while an unguarded factory cannot pass it.
   */
  private val ExpansionBoundMillis: Long = 5000L

  private val HugeExponentText: String = "1e500000000"

  /**
   * The rejection every case of this section is answered by.
   *
   * The wording is the literal text of the report, and the number in it is the canonical
   * `BigDecimal` form of the value rather than its digits - which is itself part of the
   * contract, since a message quoting five hundred million digits would be its own resource
   * problem.
   */
  private val hugeExponentMessage: String =
    "Decimal value must not exceed 18 digits of precision at scale 0: 1E+500000000"

  /**
   * Runs a call and reports how long it took, in milliseconds.
   *
   * @param call  the call to run
   * @return the value the call produced, with its elapsed time in milliseconds
   */
  private def timed[A](call: => A): (A, Long) = {
    val startedAt = System.nanoTime()
    val outcome = call
    (outcome, (System.nanoTime() - startedAt) / 1000000L)
  }

  test("an exponent naming more digits than the type holds is rejected without expanding it") {
    val (fromText, textMillis) = timed(Decimal.of(HugeExponentText))
    fromText should beFailureWith(FailureReason.INVALID)
    fromText.left.map(failure => failure.message) shouldBe Left(hugeExponentMessage)
    withClue(s"reading '$HugeExponentText' took ${textMillis}ms: ") {
      textMillis should be < ExpansionBoundMillis
    }

    val (fromParse, parseMillis) = timed(Decimal.parse(HugeExponentText))
    fromParse shouldBe fromText
    withClue(s"parsing '$HugeExponentText' took ${parseMillis}ms: ") {
      parseMillis should be < ExpansionBoundMillis
    }

    // the guard sits in the conversion rather than in the reading of text, so the public
    // `BigDecimal` factory - which the JSON decoder and every other caller of a `BigDecimal`
    // reaches directly - is bounded by the same guard and not by a check on the text
    val (fromBigDecimal, bigDecimalMillis) = timed(Decimal.of(new BigDecimal(HugeExponentText)))
    fromBigDecimal shouldBe fromText
    withClue(s"converting the BigDecimal took ${bigDecimalMillis}ms: ") {
      bigDecimalMillis should be < ExpansionBoundMillis
    }
  }

  test("an exponent at the top of the range is rejected, and one beyond it names no number") {
    // an exponent at the top of the range of scales, held at a scale of `-Int.MaxValue`
    val atTop = Decimal.of("1e2147483647")
    atTop should beFailureWith(FailureReason.INVALID)
    atTop.left.map(failure => failure.message) shouldBe
      Left("Decimal value must not exceed 18 digits of precision at scale 0: 1E+2147483647")

    // One more than that: `BigDecimal` reads this text on this platform and holds it at a
    // scale of `Int.MinValue`, which is the case that decides the arithmetic of the guard -
    // negating that scale in `Int` overflows to itself, so the digit count is computed in
    // `Long` and this value is rejected rather than admitted and expanded.
    val (pastTop, pastTopMillis) = timed(Decimal.of("1e2147483648"))
    pastTop should beFailureWith(FailureReason.INVALID)
    pastTop.left.map(failure => failure.message) shouldBe
      Left("Decimal value must not exceed 18 digits of precision at scale 0: 1E+2147483648")
    withClue(s"reading '1e2147483648' took ${pastTopMillis}ms: ") {
      pastTopMillis should be < ExpansionBoundMillis
    }

    // An exponent of more digits than `BigDecimal` itself accepts is text that names no
    // number, so it stays on the parsing side of the line this section's neighbours draw.
    Decimal.of("1e21474836470") should beFailureWith(FailureReason.PARSING)
    Decimal.of("1e21474836470").left.map(failure => failure.message) shouldBe
      Left("Decimal string is invalid: '1e21474836470'")
    Decimal.of("1e-2147483648") should beFailureWith(FailureReason.PARSING)
  }

  test("bounding the exponent moves neither end of the range of values the type holds") {
    // the boundary itself: eighteen digits at scale zero is the largest whole number the type
    // holds, nineteen is one too many, and the guard decides both exactly as the expansion did
    assertRow(text("1e17"), 100000000000000000L, 0)
    Decimal.of("1e18") should beFailureWith(FailureReason.INVALID)
    Decimal.of("1e18").left.map(failure => failure.message) shouldBe
      Left("Decimal value must not exceed 18 digits of precision at scale 0: 1E+18")

    // an ordinary exponent is still applied rather than counted and refused
    text("1e5").toString shouldBe "100000"
    assertRow(text("1e5"), 100000L, 0)

    // a zero is the value zero whatever scale it names, at either sign of the exponent, which
    // is the case the guard answers before its arithmetic: the precision of a zero is one and
    // its scale can be anything, so counting digits would have refused a legitimate zero
    Decimal.of("0e100") shouldBe Right(Decimal.ZERO)
    Decimal.of("0e-100") shouldBe Right(Decimal.ZERO)
    Decimal.of(new BigDecimal("0e100")) shouldBe Right(Decimal.ZERO)

    // a negative exponent names a value below the smallest the type distinguishes, which is
    // zero rather than a rejection, and reaches that answer without arithmetic on its scale
    val (tiny, tinyMillis) = timed(Decimal.of("1e-500000000"))
    tiny shouldBe Right(Decimal.ZERO)
    withClue(s"reading '1e-500000000' took ${tinyMillis}ms: ") {
      tinyMillis should be < ExpansionBoundMillis
    }

    // and a whole number whose trailing zeroes live in the scale of the `BigDecimal` that
    // holds it is still read as that number, which is the shape the guard measures
    assertRow(fromBigDecimal(new BigDecimal("100")), 100L, 0)
    assertRow(fromBigDecimal(new BigDecimal("1e2")), 100L, 0)
    text("100").toString shouldBe "100"
  }

  //-------------------------------------------------------------------------
  // The character pass that reads text.
  //
  // `Decimal.of(String)` reads a numeral with one pass over the characters and hands the
  // shapes that pass does not evaluate - an exponent, a unicode digit, more digits than the
  // type holds - to `BigDecimal`. The contract is unchanged by that: the factory answers, for
  // every text, what `new BigDecimal(str)` followed by `Decimal.of(BigDecimal)` answered. That
  // equivalence is what this section asserts, over the whole space of short numeral-ish text
  // rather than over chosen cases, because the cases that distinguish two readings of text are
  // exactly the ones nobody thinks to choose.

  /**
   * The reading of text this factory is specified to agree with.
   *
   * This is the route `of(String)` took in full before the pass existed: the two guards on the
   * text itself, then `BigDecimal` and the conversion of what it read. It is computed here
   * rather than imported, so the assertions below compare two independent algorithms rather
   * than one algorithm with itself. Only the reason of a failure is compared, because the
   * wording of a rejection is asserted separately and literally.
   *
   * @param str  the text to read
   * @return the decimal the text names, or the reason it names none
   */
  private def textReference(str: String): Either[FailureReason, Decimal] =
    if (str.isEmpty) {
      Left(FailureReason.PARSING)
    } else if (str.length > 256) {
      // the length bound of the factory, restated as a literal because the constant holding it
      // is private to the companion, exactly as the malformed-text table above restates it
      Left(FailureReason.PARSING)
    } else {
      Try(new BigDecimal(str)).toEither match {
        case Right(value) => Decimal.of(value).left.map(failure => failure.reason)
        case Left(_) => Left(FailureReason.PARSING)
      }
    }

  private def assertTextAgrees(str: String): Assertion =
    withClue(s"reading '$str': ") {
      Decimal.of(str).left.map(failure => failure.reason) shouldBe textReference(str)
    }

  /**
   * The fullwidth digit one, built from its code point as [[ArabicIndicOne]] is.
   *
   * A second unicode digit outside ASCII, from a different block, so the delegation to
   * `BigDecimal` is asserted over more than one such digit: this digit and the Arabic-Indic
   * one each head a row of `delegatedTexts`, where both are read as the value one.
   */
  private val FullWidthOne: String = 0xff11.toChar.toString

  /**
   * A lone surrogate, which is not a digit and cannot be part of one on its own.
   *
   * `BigDecimal` refuses text containing it - it reads one `char` at a time and never a
   * surrogate pair - so the pass may refuse it directly, and this character is in the sweep
   * alphabet below to hold that agreement.
   */
  private val LoneSurrogate: String = 0xd835.toChar.toString

  /**
   * The characters numerals are spelled with, and several they are not.
   *
   * Digits at both ends of the range and a middling one, the point, both signs, both exponent
   * markers, a unicode digit from two blocks, a lone surrogate, a letter, a space and a
   * newline: every branch of the pass is reachable from this alphabet, including the ones that
   * only a malformed numeral reaches.
   */
  private val NumeralCharacters: List[Char] =
    List(
      '0',
      '1',
      '9',
      '.',
      '-',
      '+',
      'e',
      'E',
      'A',
      ' ',
      '\n',
      ',',
      ArabicIndicOne.head,
      FullWidthOne.head,
      LoneSurrogate.head)

  private val SweepCharacters: List[Char] =
    List('0', '1', '.', '-', '+', 'e', 'A', ' ', ArabicIndicOne.head, LoneSurrogate.head)

  /**
   * Text built from the numeral alphabet, in the three shapes that reach different branches.
   *
   * Free text over the alphabet covers the malformed shapes and, at short lengths, a fair
   * number of well-formed ones; the well-formed shape is generated separately so that the
   * digit runs are long enough to cross the eighteen-digit and eighteen-place boundaries where
   * the pass stops evaluating and delegates; and the oversized shape crosses the length guard.
   */
  private val genNumeralText: Gen[String] = {
    val character = Gen.oneOf(NumeralCharacters)
    val free = Gen.choose(0, 20).flatMap(length => Gen.stringOfN(length, character))
    val oversized = Gen.choose(250, 300).flatMap(length => Gen.stringOfN(length, character))
    val wellFormed = for {
      sign <- Gen.oneOf("", "-", "+")
      whole <- Gen.choose(0, 22).flatMap(length => Gen.stringOfN(length, Gen.numChar))
      point <- Gen.oneOf("", ".")
      fraction <- Gen.choose(0, 22).flatMap(length => Gen.stringOfN(length, Gen.numChar))
      exponent <- Gen.oneOf("", "e3", "E-3", "e+2", "E0", "e400", "e-400")
    } yield sign + whole + point + fraction + exponent
    Gen.frequency((10, free), (8, wellFormed), (1, oversized))
  }

  private def textsOfLength(alphabet: List[Char], length: Int): List[String] =
    if (length == 0) {
      List("")
    } else {
      textsOfLength(alphabet, length - 1)
        .flatMap(prefix => alphabet.map(character => prefix + character))
    }

  /**
   * Text the pass decides itself, with the message the rejection carries.
   *
   * Every row is text no `BigDecimal` could read - a character that is not part of a numeral,
   * a second point, a sign in the middle, an exponent that is not a sign and digits, or no
   * digit at all - so the pass reports it without constructing an exception to catch, which is
   * the whole of the change these rows guard. The message is the literal text of the report
   * rather than one this spec builds the way the implementation builds it.
   */
  private val passRejectedTexts: TableFor2[String, String] = Table(
    ("text", "message"),
    ("not-a-number", "Decimal string is invalid: 'not-a-number'"),
    ("1.2.3", "Decimal string is invalid: '1.2.3'"),
    ("1+23", "Decimal string is invalid: '1+23'"),
    ("--123", "Decimal string is invalid: '--123'"),
    ("A", "Decimal string is invalid: 'A'"),
    // the row whose text is a control character: the rejection names it as it stands, as it
    // names every other text here, and escaping it belongs to the writing of the failure
    ("\n", "Decimal string is invalid: '\n'"),
    ("..", "Decimal string is invalid: '..'"),
    ("1..2", "Decimal string is invalid: '1..2'"),
    ("1.-2", "Decimal string is invalid: '1.-2'"),
    (".", "Decimal string is invalid: '.'"),
    ("-.", "Decimal string is invalid: '-.'"),
    ("1e", "Decimal string is invalid: '1e'"),
    ("1e+", "Decimal string is invalid: '1e+'"),
    ("1.2e3.4", "Decimal string is invalid: '1.2e3.4'"),
    ("e3", "Decimal string is invalid: 'e3'"),
    ("1 2", "Decimal string is invalid: '1 2'"),
    ("1,000", "Decimal string is invalid: '1,000'"))

  /**
   * Text the pass hands to `BigDecimal`, with the parts of the decimal that route gives.
   *
   * The pass evaluates no exponent and no digit outside ASCII, and stops evaluating a numeral
   * that names more than eighteen digits or more than eighteen places, so each of these rows
   * is read by `BigDecimal` and converted by `of(BigDecimal)` - which is why the truncating
   * rows here state exactly the truncation that factory performs.
   */
  private val delegatedTexts: TableFor3[String, Long, Int] = Table(
    ("text", "unscaled", "scale"),
    ("1e3", 1000L, 0),
    ("1E3", 1000L, 0),
    ("1.5e3", 1500L, 0),
    ("1.5E-3", 15L, 4),
    ("+.5e2", 50L, 0),
    ("1.e3", 1000L, 0),
    ("0e3", 0L, 0),
    (ArabicIndicOne + "23", 123L, 0),
    (FullWidthOne + "23", 123L, 0),
    ("0.1234567890123456789", 123456789012345678L, 18),
    ("123456789012345678.9", 123456789012345678L, 0))

  /**
   * Text the pass evaluates itself, with the unscaled value and scale it counts.
   *
   * The counting each row states is the counting the pass performs: a leading zero
   * contributes no significant digit, a zero after the point still occupies a place, and a
   * trailing zero of the fraction is removed by the normalisation every factory shares rather
   * than by the pass. The rows carry the boundaries of that counting - eighteen significant
   * digits and eighteen places, at both signs - since a text naming more of either is handed
   * to `BigDecimal` instead.
   */
  private val passCountedTexts: TableFor3[String, Long, Int] = Table(
    ("text", "unscaled", "scale"),
    ("0.001", 1L, 3),
    ("1.10", 11L, 1),
    ("1.", 1L, 0),
    (".5", 5L, 1),
    ("-1.", -1L, 0),
    ("+0", 0L, 0),
    ("-0", 0L, 0),
    ("0000000000000000000005", 5L, 0),
    ("00000.000001", 1L, 6),
    ("999999999999999999", 999999999999999999L, 0),
    ("-999999999999999999", -999999999999999999L, 0),
    ("999999999999999990", 999999999999999990L, 0),
    ("0.999999999999999999", 999999999999999999L, 18))

  test("the text factory reads what BigDecimal reads, over generated numeral-ish text") {
    forAll(genNumeralText, minSuccessful(2000)) { str =>
      assertTextAgrees(str)
    }
  }

  test("the text factory reads what BigDecimal reads, over every short text of its alphabet") {
    // an exhaustive sweep rather than a property: every string of up to four characters over
    // an alphabet holding one character of each kind the pass distinguishes, which is the
    // space in which a misclassified character or a mishandled boundary has nowhere to hide
    (0 to 4).foreach { length =>
      textsOfLength(SweepCharacters, length).foreach(str => assertTextAgrees(str))
    }
    // the fifth character only matters where it can still change the classification, so the
    // longer sweep is over the digits, the point, the signs and the exponent marker
    textsOfLength(List('0', '1', '.', '-', '+', 'e'), 5).foreach(str => assertTextAgrees(str))
    succeed
  }

  test("text the factory decides without BigDecimal is rejected with its reason and message") {
    forAll(passRejectedTexts) { (str, message) =>
      Decimal.of(str) should beFailureWith(FailureReason.PARSING)
      Decimal.of(str).left.map(failure => failure.message) shouldBe Left(message)
      Decimal.parse(str) should beFailureWith(FailureReason.PARSING)
      assertTextAgrees(str)
    }
  }

  test("text the factory hands to BigDecimal keeps the reading BigDecimal gives it") {
    forAll(delegatedTexts) { (str, unscaled, scale) =>
      assertRow(text(str), unscaled, scale)
      assertTextAgrees(str)
    }
  }

  test("parsing names the text it rejected, and the failure renders bounded and on one line") {
    // A malformed numeral is reported by naming the text that was refused, so two independent
    // bounds decide what a log can receive from it: text longer than the type reads is
    // rejected by the length guard before a numeral is quoted at all, and the rendering of any
    // failure bounds every part it writes.
    val payload = "H" * 10000
    val bounded: FailureOr[Decimal] = Decimal.of(payload)
    bounded should beFailureWith(FailureReason.PARSING)
    // The length guard owns this message, so it names the limit rather than the text - which is
    // the strongest form of the bound: text of any size is reported without being echoed at all.
    bounded.left.map(failure => failure.message) shouldBe
      Left("Decimal string must not exceed 256 characters")

    // Text within the length guard is echoed, and the failure names the whole of it; the
    // rendering of that failure is what is bounded, and it marks what it left out.
    val longNumeral = "1" * 200 + "Z"
    val echoed: FailureOr[Decimal] = Decimal.of(longNumeral)
    echoed should beFailureWith(FailureReason.PARSING)
    val echoedFailure = echoed.left.toOption.getOrElse(fail("expected a failure"))
    echoedFailure.message shouldBe s"Decimal string is invalid: '$longNumeral'"
    Show[Failure].show(echoedFailure).length should be < 600

    // Text holding a line break is named as it stands and rendered on one line, so a
    // line-oriented consumer of the rendering cannot be made to record a line the library did
    // not report.
    val injected: FailureOr[Decimal] = Decimal.of("1.5\nINJECTED")
    injected should beFailureWith(FailureReason.PARSING)
    val injectedFailure = injected.left.toOption.getOrElse(fail("expected a failure"))
    injectedFailure.message shouldBe "Decimal string is invalid: '1.5\nINJECTED'"
    val injectedRendering = Show[Failure].show(injectedFailure)
    injectedRendering should not include "\n"
    injectedRendering should not include "\r"
    injectedRendering shouldBe "PARSING: Decimal string is invalid: '1.5\\nINJECTED'"

    // And the message for an ordinary malformed numeral reads as it always did, in the failure
    // and in its rendering alike, which is what makes the bound invisible to every caller but
    // the adversarial one.
    Decimal.of("not-a-number").left.map(failure => failure.message) shouldBe
      Left("Decimal string is invalid: 'not-a-number'")
    Decimal.of("not-a-number").left.map(failure => Show[Failure].show(failure)) shouldBe
      Left("PARSING: Decimal string is invalid: 'not-a-number'")
  }

  test("text naming more than the type holds is still decided by BigDecimal") {
    // the pass stops evaluating and delegates rather than deciding the truncation or the
    // overflow itself, which is what keeps these on the side of the line the two tables of
    // rejected text above draw: invalid values, not unreadable text
    Decimal.of("1234567890123456789") should beFailureWith(FailureReason.INVALID)
    Decimal.of("12345678901234567890") should beFailureWith(FailureReason.INVALID)
    Decimal.of("1e400") should beFailureWith(FailureReason.INVALID)
    Decimal.of("1e-400") shouldBe Right(Decimal.ZERO)
    Decimal.of("0.0000000000000000001") shouldBe Right(Decimal.ZERO)
  }

  test("the text factory counts precision and scale as the ported scanner counts them") {
    forAll(passCountedTexts) { (str, unscaled, scale) =>
      assertRow(text(str), unscaled, scale)
      Decimal.parse(str) shouldBe Right(text(str))
      assertTextAgrees(str)
    }
  }

  //-------------------------------------------------------------------------
  // Addition and subtraction.

  test("the sum of two decimals is exact and commutative") {
    forAll(plusValues) { (unscaled1, scale1, unscaled2, scale2, expectedUnscaled, expectedScale) =>
      val test1 = scaled(unscaled1, scale1)
      val test2 = scaled(unscaled2, scale2)
      val expected = scaled(expectedUnscaled, expectedScale)
      test1.plus(test2) shouldBe expected
      test2.plus(test1) shouldBe expected
      if (scale2 == 0) {
        test1.plus(unscaled2) shouldBe expected
      }
    }
  }

  test("negating both operands negates their sum") {
    forAll(plusValues) { (unscaled1, scale1, unscaled2, scale2, expectedUnscaled, expectedScale) =>
      val test1 = scaled(unscaled1, scale1)
      val test2 = scaled(unscaled2, scale2)
      val expected = scaled(expectedUnscaled, expectedScale)
      test1.negated.plus(test2.negated) shouldBe expected.negated
      test2.negated.plus(test1.negated) shouldBe expected.negated
      if (scale2 == 0) {
        test1.negated.plus(-unscaled2) shouldBe expected.negated
      }
    }
  }

  test("subtracting the negation of an operand gives the sum") {
    forAll(plusValues) { (unscaled1, scale1, unscaled2, scale2, expectedUnscaled, expectedScale) =>
      val test1 = scaled(unscaled1, scale1)
      val test2 = scaled(unscaled2, scale2)
      val expected = scaled(expectedUnscaled, expectedScale)
      test1.minus(test2.negated) shouldBe expected
      test2.minus(test1.negated) shouldBe expected
      if (scale2 == 0) {
        test1.minus(-unscaled2) shouldBe expected
      }
    }
  }

  test("a decimal subtracted from itself, or added to its negation, is zero") {
    forAll(plusValues) { (unscaled1, scale1, _, _, _, _) =>
      val test1 = scaled(unscaled1, scale1)
      test1.minus(test1) shouldBe Decimal.ZERO
      test1.plus(test1.negated) shouldBe Decimal.ZERO
    }
  }

  test("a sum needing more than eighteen digits raises rather than returning a value") {
    // arithmetic is total in its signature, so the overflow is a broken precondition and not
    // a `Left`: this is the assertion that keeps the two error channels of the type apart
    assertThrows[IllegalArgumentException](Decimal.MAX_VALUE.plus(whole(1L)))
    assertThrows[IllegalArgumentException](Decimal.MAX_VALUE.minus(whole(-1L)))
    assertThrows[IllegalArgumentException](Decimal.MIN_VALUE.minus(whole(1L)))
    assertThrows[IllegalArgumentException](Decimal.MIN_VALUE.plus(whole(-1L)))
    assertThrows[IllegalArgumentException](Decimal.MAX_VALUE.plus(Decimal.MAX_VALUE))
  }

  test("adding a whole number covers the values no decimal of its own can hold") {
    fromDouble(12.0).plus(13L) shouldBe whole(25L)
    fromDouble(0.0).plus(0L) shouldBe whole(0L)
    fromDouble(0.0).plus(999999999999999999L) shouldBe whole(999999999999999999L)
    fromDouble(-1.0).plus(1000000000000000000L) shouldBe whole(999999999999999999L)
    // the fractional part is necessarily lost when the whole part alone fills the precision
    fromDouble(-1.5).plus(1000000000000000000L) shouldBe whole(999999999999999999L)
    fromDouble(0.0).plus(-999999999999999999L) shouldBe whole(-999999999999999999L)
    fromDouble(1.0).plus(-1000000000000000000L) shouldBe whole(-999999999999999999L)
    fromDouble(1.5).plus(-1000000000000000000L) shouldBe whole(-999999999999999999L)
    assertThrows[IllegalArgumentException](fromDouble(0.0).plus(1000000000000000000L))
    assertThrows[IllegalArgumentException](fromDouble(0.0).plus(Long.MaxValue))
    assertThrows[IllegalArgumentException](fromDouble(1.0).plus(Long.MaxValue))
    assertThrows[IllegalArgumentException](fromDouble(0.0).plus(-1000000000000000000L))
    assertThrows[IllegalArgumentException](fromDouble(0.0).plus(Long.MinValue))
    assertThrows[IllegalArgumentException](fromDouble(-1.0).plus(Long.MinValue))
  }

  test("adding a Double adds the decimal its shortest text names") {
    fromDouble(0.0).plus(0.0) shouldBe fromDouble(0.0)
    fromDouble(0.0).plus(123.45) shouldBe fromDouble(123.45)
    fromDouble(123.45).plus(123.45) shouldBe fromDouble(246.9)
    fromDouble(-123.45).plus(123.45) shouldBe fromDouble(0.0)
  }

  test("adding a Double that is not finite raises rather than adding") {
    // the operand is converted before the addition and a value that is not finite names no
    // decimal, so the precondition broken is that of the conversion and the message names the
    // value; the finite case is here so this cannot pass by every addition raising
    fromDouble(1.5).plus(2.5) shouldBe fromDouble(4.0)
    forAll(nonFiniteValues) { (value, message) =>
      intercept[IllegalArgumentException](fromDouble(1.5).plus(value)).getMessage shouldBe message
    }
  }

  test("subtracting a whole number covers the values no decimal of its own can hold") {
    fromDouble(12.0).minus(13L) shouldBe whole(-1L)
    fromDouble(0.0).minus(0L) shouldBe whole(0L)
    fromDouble(0.0).minus(999999999999999999L) shouldBe whole(-999999999999999999L)
    fromDouble(1.0).minus(1000000000000000000L) shouldBe whole(-999999999999999999L)
    fromDouble(1.5).minus(1000000000000000000L) shouldBe whole(-999999999999999999L)
    fromDouble(0.0).minus(-999999999999999999L) shouldBe whole(999999999999999999L)
    fromDouble(-1.0).minus(-1000000000000000000L) shouldBe whole(999999999999999999L)
    fromDouble(-1.5).minus(-1000000000000000000L) shouldBe whole(999999999999999999L)
    assertThrows[IllegalArgumentException](fromDouble(0.0).minus(1000000000000000000L))
    assertThrows[IllegalArgumentException](fromDouble(0.0).minus(Long.MaxValue))
    assertThrows[IllegalArgumentException](fromDouble(-1.0).minus(Long.MaxValue))
    assertThrows[IllegalArgumentException](fromDouble(0.0).minus(-1000000000000000000L))
    assertThrows[IllegalArgumentException](fromDouble(0.0).minus(Long.MinValue))
    assertThrows[IllegalArgumentException](fromDouble(1.0).minus(Long.MinValue))
  }

  test("subtracting a Double subtracts the decimal its shortest text names") {
    fromDouble(0.0).minus(0.0) shouldBe fromDouble(0.0)
    fromDouble(0.0).minus(123.45) shouldBe fromDouble(-123.45)
    fromDouble(123.45).minus(123.45) shouldBe fromDouble(0.0)
    fromDouble(-123.45).minus(123.45) shouldBe fromDouble(-246.9)
  }

  test("subtracting a Double that is not finite raises rather than subtracting") {
    fromDouble(1.5).minus(2.5) shouldBe fromDouble(-1.0)
    forAll(nonFiniteValues) { (value, message) =>
      intercept[IllegalArgumentException](fromDouble(1.5).minus(value)).getMessage shouldBe message
    }
  }

  //-------------------------------------------------------------------------
  // Multiplication.

  test("the product of two decimals is exact, commutative and sign-respecting") {
    forAll(multipliedByValues) {
      (unscaled1, scale1, unscaled2, scale2, expectedUnscaled, expectedScale) =>
        val test1 = scaled(unscaled1, scale1)
        val test2 = scaled(unscaled2, scale2)
        val expected = scaled(expectedUnscaled, expectedScale)
        test1.multipliedBy(test2) shouldBe expected
        test2.multipliedBy(test1) shouldBe expected
        test1.negated.multipliedBy(test2) shouldBe expected.negated
        test2.negated.multipliedBy(test1) shouldBe expected.negated
        if (scale2 == 0) {
          test1.multipliedBy(unscaled2) shouldBe expected
        }
    }
  }

  test("a product needing more than eighteen digits raises rather than returning a value") {
    assertThrows[IllegalArgumentException](Decimal.MAX_VALUE.multipliedBy(whole(3L)))
    assertThrows[IllegalArgumentException](Decimal.MAX_VALUE.multipliedBy(10L))
  }

  test("multiplying by a Double multiplies by the decimal its shortest text names") {
    fromDouble(0.0).multipliedBy(0.0) shouldBe fromDouble(0.0)
    fromDouble(0.0).multipliedBy(123.45) shouldBe fromDouble(0.0)
    fromDouble(123.45).multipliedBy(2.0) shouldBe fromDouble(246.9)
    fromDouble(-123.45).multipliedBy(2.0) shouldBe fromDouble(-246.9)
  }

  test("multiplying by a Double that is not finite raises rather than multiplying") {
    fromDouble(1.5).multipliedBy(2.5) shouldBe fromDouble(3.75)
    forAll(nonFiniteValues) { (value, message) =>
      intercept[IllegalArgumentException](fromDouble(1.5).multipliedBy(value))
        .getMessage shouldBe message
    }
  }

  //-------------------------------------------------------------------------
  // Moving the decimal point.

  test("moving the decimal point multiplies or divides by a power of ten") {
    scaled(1235L, 3).movePoint(0) shouldBe scaled(1235L, 3)
    scaled(1235L, 3).movePoint(1) shouldBe scaled(1235L, 2)
    scaled(1235L, 3).movePoint(2) shouldBe scaled(1235L, 1)
    scaled(1235L, 3).movePoint(3) shouldBe scaled(1235L, 0)
    scaled(1235L, 3).movePoint(4) shouldBe scaled(12350L, 0)
    scaled(1235L, 3).movePoint(-1) shouldBe scaled(1235L, 4)
    scaled(1235L, 3).movePoint(-2) shouldBe scaled(1235L, 5)
    // a movement to the right that overruns the precision is a broken precondition, while one
    // to the left merely exhausts the value and gives zero
    assertThrows[IllegalArgumentException](scaled(1235L, 3).movePoint(20))
    assertThrows[IllegalArgumentException](scaled(1235L, 3).movePoint(Int.MaxValue))
    scaled(1235L, 3).movePoint(-20) shouldBe Decimal.ZERO
    scaled(1235L, 3).movePoint(Int.MinValue + 100) shouldBe Decimal.ZERO
    scaled(1235L, 3).movePoint(Int.MinValue) shouldBe Decimal.ZERO
  }

  //-------------------------------------------------------------------------
  // Division and remainder.

  test("the quotient of two decimals is truncated towards zero") {
    forAll(dividedByValues) {
      (unscaled1, scale1, unscaled2, scale2, truncatedUnscaled, expectedScale, _) =>
        val test1 = scaled(unscaled1, scale1)
        val test2 = scaled(unscaled2, scale2)
        val expected = scaled(truncatedUnscaled, expectedScale)
        test1.dividedBy(test2) shouldBe expected
        test1.negated.dividedBy(test2) shouldBe expected.negated
        if (scale2 == 0) {
          test1.dividedBy(unscaled2) shouldBe expected
        }
    }
  }

  test("the quotient of two decimals rounds the eighteenth digit when asked to") {
    forAll(dividedByValues) {
      (unscaled1, scale1, unscaled2, scale2, _, expectedScale, roundedUnscaled) =>
        val test1 = scaled(unscaled1, scale1)
        val test2 = scaled(unscaled2, scale2)
        test1.dividedBy(test2, RoundingMode.HALF_UP) shouldBe scaled(roundedUnscaled, expectedScale)
    }
  }

  test("dividing by zero, or to a quotient beyond eighteen digits, raises") {
    // division by zero is the platform's arithmetic error, exactly as in the ported class,
    // while a quotient that does not fit is the same broken precondition as any other
    // arithmetic overflow
    assertThrows[ArithmeticException](Decimal.ZERO.dividedBy(Decimal.ZERO))
    assertThrows[ArithmeticException](whole(2L).dividedBy(Decimal.ZERO))
    assertThrows[IllegalArgumentException](Decimal.MAX_VALUE.dividedBy(scaled(1L, 1)))
  }

  test("dividing by a Double divides by the decimal its shortest text names") {
    assertThrows[ArithmeticException](Decimal.ZERO.dividedBy(0.0))
    fromDouble(0.0).dividedBy(123.45) shouldBe fromDouble(0.0)
    fromDouble(123.45).dividedBy(2.0) shouldBe fromDouble(61.725)
    fromDouble(-123.45).dividedBy(2.0) shouldBe fromDouble(-61.725)
  }

  test("dividing by a Double that is not finite raises rather than dividing") {
    // a divisor that is not finite is rejected by the conversion, which is a broken
    // precondition and not the arithmetic error a zero divisor raises
    fromDouble(1.5).dividedBy(2.5) shouldBe fromDouble(0.6)
    forAll(nonFiniteValues) { (value, message) =>
      intercept[IllegalArgumentException](fromDouble(1.5).dividedBy(value))
        .getMessage shouldBe message
    }
  }

  test("dividing by a whole zero raises whatever the dividend is") {
    // zero is no power of ten, so the division reaches BigDecimal and raises the arithmetic
    // error of the platform - for a dividend of zero as for any other - while a divisor that
    // is not zero divides normally
    whole(6L).dividedBy(3L) shouldBe whole(2L)
    assertThrows[ArithmeticException](whole(7L).dividedBy(0L))
    assertThrows[ArithmeticException](Decimal.ZERO.dividedBy(0L))
  }

  test("the remainder carries the sign of the dividend and rejects a zero divisor") {
    whole(7L).remainder(whole(3L)) shouldBe whole(1L)
    whole(-7L).remainder(whole(3L)) shouldBe whole(-1L)
    whole(7L).remainder(whole(-3L)) shouldBe whole(1L)
    scaled(125L, 1).remainder(whole(1L)) shouldBe scaled(5L, 1)
    assertThrows[ArithmeticException](whole(2L).remainder(Decimal.ZERO))
  }


  //-------------------------------------------------------------------------
  // Rounding. Each row is checked against `BigDecimal.setScale` under every rounding mode,
  // so the expectation is the behaviour of the platform's decimal rather than a literal this
  // spec could have copied from the implementation it is testing.

  test("rounding to a scale agrees with BigDecimal under every rounding mode") {
    forAll(roundToScaleValues) { (str, desiredScale) =>
      RoundingModes.foreach { mode =>
        withClue(s"rounding $str to scale $desiredScale under $mode: ") {
          text(str).roundToScale(desiredScale, mode).toString shouldBe
            new BigDecimal(str).setScale(desiredScale, mode).stripTrailingZeros.toPlainString
        }
      }
      succeed
    }
  }

  test("rounding a negated value to a scale agrees with BigDecimal under every rounding mode") {
    forAll(roundToScaleValues) { (str, desiredScale) =>
      RoundingModes.foreach { mode =>
        withClue(s"rounding -$str to scale $desiredScale under $mode: ") {
          text(str).negated.roundToScale(desiredScale, mode).toString shouldBe
            new BigDecimal(str).negate.setScale(desiredScale, mode).stripTrailingZeros.toPlainString
        }
      }
      succeed
    }
  }

  test("rounding to a scale of minus eighteen or beyond raises") {
    // the edges of the representation make the answer ambiguous there, so the bound is a
    // documented precondition of the method
    assertThrows[IllegalArgumentException](Decimal.MAX_VALUE.roundToScale(-Decimal.MAX_SCALE, RoundingMode.DOWN))
    assertThrows[IllegalArgumentException](Decimal.MAX_VALUE.roundToScale(-Decimal.MAX_SCALE, RoundingMode.HALF_UP))
    assertThrows[IllegalArgumentException](Decimal.MAX_VALUE.roundToScale(-Decimal.MAX_SCALE - 1, RoundingMode.HALF_UP))
  }

  test("rounding to a scale under UNNECESSARY raises where a digit must be dropped") {
    // the mode that refuses to round is the one mode the BigDecimal-agreement cases of this
    // section exclude, and it reports an arithmetic error rather than a broken precondition.
    // A scale at or above the scale of a decimal returns it unchanged without consulting the
    // mode, so a case reaching the rounding itself has to ask for a smaller scale; because
    // every decimal is normalised, a fraction never ends in a zero and a smaller positive
    // scale therefore always drops a digit that matters. The control consequently rounds the
    // whole part, where the digits dropped are zeroes and the mode has nothing to refuse
    whole(1200L).roundToScale(-2, RoundingMode.UNNECESSARY) shouldBe whole(1200L)
    assertThrows[ArithmeticException](scaled(1235L, 3).roundToScale(2, RoundingMode.UNNECESSARY))
    assertThrows[ArithmeticException](scaled(1235L, 3).roundToScale(0, RoundingMode.UNNECESSARY))
    assertThrows[ArithmeticException](whole(1235L).roundToScale(-1, RoundingMode.UNNECESSARY))
  }

  test("rounding to a number of significant digits keeps that many") {
    scaled(12345L, 2).roundToPrecision(3, RoundingMode.UP) shouldBe scaled(124L, 0)
    scaled(12345L, 2).roundToPrecision(2, RoundingMode.UP) shouldBe scaled(130L, 0)
    scaled(12345L, 2).roundToPrecision(1, RoundingMode.UP) shouldBe scaled(200L, 0)
    scaled(12345L, 2).roundToPrecision(1, RoundingMode.HALF_UP) shouldBe scaled(100L, 0)
    // a precision of eighteen or more cannot change a decimal, which holds no more than that
    scaled(12345L, 2).roundToPrecision(18, RoundingMode.HALF_UP) shouldBe scaled(12345L, 2)
    assertThrows[IllegalArgumentException](Decimal.ZERO.roundToPrecision(-1, RoundingMode.CEILING))
    assertThrows[IllegalArgumentException](Decimal.MAX_VALUE.roundToPrecision(-Decimal.MAX_SCALE, RoundingMode.DOWN))
    assertThrows[IllegalArgumentException](Decimal.MAX_VALUE.roundToPrecision(-Decimal.MAX_SCALE, RoundingMode.HALF_UP))
    assertThrows[IllegalArgumentException](Decimal.MAX_VALUE.roundToPrecision(-Decimal.MAX_SCALE - 1, RoundingMode.HALF_UP))
  }

  test("rounding to a precision under UNNECESSARY raises where a digit must be dropped") {
    // a precision that keeps every significant digit of the value leaves it unchanged and the
    // mode has nothing to refuse, so the control keeps all five digits of 123.45
    scaled(12345L, 2).roundToPrecision(5, RoundingMode.UNNECESSARY) shouldBe scaled(12345L, 2)
    assertThrows[ArithmeticException](scaled(12345L, 2).roundToPrecision(3, RoundingMode.UNNECESSARY))
    assertThrows[ArithmeticException](scaled(12345L, 2).roundToPrecision(1, RoundingMode.UNNECESSARY))
  }

  //-------------------------------------------------------------------------
  // Queries and conversions.

  test("a decimal knows whether it is zero") {
    scaled(123L, 2).isZero shouldBe false
    Decimal.ZERO.isZero shouldBe true
    scaled(-123L, 2).isZero shouldBe false
  }

  test("the absolute value of a decimal drops its sign") {
    scaled(123L, 2).abs shouldBe scaled(123L, 2)
    scaled(-123L, 2).abs shouldBe scaled(123L, 2)
  }

  test("negating a decimal flips its sign and keeps its scale") {
    scaled(123L, 2).negated shouldBe scaled(-123L, 2)
    scaled(-123L, 2).negated shouldBe scaled(123L, 2)
  }

  test("the sign of a decimal is read from its unscaled part") {
    scaled(123L, 2).signum shouldBe 1
    Decimal.ZERO.signum shouldBe 0
    scaled(-123L, 2).signum shouldBe -1
  }

  test("the four comparison queries agree with the ordering they are built on") {
    fromDouble(1.000009).isGreaterThan(fromDouble(1.0)) shouldBe true
    fromDouble(1.000009).isGreaterThanEqualTo(fromDouble(1.0)) shouldBe true
    fromDouble(1.0).isGreaterThanEqualTo(fromDouble(1.0)) shouldBe true
    fromDouble(1.0).isGreaterThan(fromDouble(1.000009)) shouldBe false
    fromDouble(1.0).isGreaterThanEqualTo(fromDouble(1.000009)) shouldBe false

    fromDouble(9.99999999).isLessThan(fromDouble(10.0)) shouldBe true
    fromDouble(9.99999999).isLessThanEqualTo(fromDouble(10.0)) shouldBe true
    fromDouble(10.0).isLessThanEqualTo(fromDouble(10.0)) shouldBe true
    fromDouble(10.0).isLessThan(fromDouble(9.999999999)) shouldBe false
    fromDouble(10.0).isLessThanEqualTo(fromDouble(9.999999999)) shouldBe false
  }

  test("mapping through a function on Double answers with an outcome") {
    // the function may produce a value no decimal can hold, which is why this is an outcome
    // rather than a decimal
    scaled(123L, 2).mapAsDouble(value => value / 2 + 5) shouldBe Right(scaled(5615L, 3))
    scaled(123L, 2).mapAsDouble(_ => Double.NaN) should beFailureWith(FailureReason.INVALID)
  }

  test("mapping through a function on BigDecimal answers with an outcome") {
    scaled(123L, 2).mapAsBigDecimal(value => value.divide(BigDecimal.valueOf(2L))) shouldBe
      Right(scaled(615L, 3))
    scaled(123L, 2)
      .mapAsBigDecimal(value => value.movePointRight(20)) should beFailureWith(FailureReason.INVALID)
  }

  //-------------------------------------------------------------------------
  // Formatting.

  test("formatting to exactly a number of decimal places rounds and pads") {
    forAll(formatValues) { (str, decimalPlaces, expectedExact, _) =>
      text(str).format(decimalPlaces, RoundingMode.HALF_UP) shouldBe expectedExact
    }
  }

  test("formatting to at least a number of decimal places pads but never drops a digit") {
    forAll(formatValues) { (str, decimalPlaces, _, expectedAtLeast) =>
      text(str).formatAtLeast(decimalPlaces) shouldBe expectedAtLeast
    }
  }

  test("formatting outside zero to eighteen decimal places raises") {
    assertThrows[IllegalArgumentException](Decimal.ZERO.format(-1, RoundingMode.HALF_UP))
    assertThrows[IllegalArgumentException](Decimal.ZERO.format(Decimal.MAX_SCALE + 1, RoundingMode.HALF_UP))
    assertThrows[IllegalArgumentException](Decimal.ZERO.formatAtLeast(-1))
    assertThrows[IllegalArgumentException](Decimal.ZERO.formatAtLeast(Decimal.MAX_SCALE + 1))
  }

  test("formatting under UNNECESSARY raises where a digit must be dropped") {
    // formatting rounds to the requested places before padding, so the mode that refuses to
    // round refuses the format that would drop a digit; asking for the places the decimal
    // already has, or more, drops nothing and formats as it always does
    scaled(1235L, 3).format(3, RoundingMode.UNNECESSARY) shouldBe "1.235"
    scaled(1235L, 3).format(4, RoundingMode.UNNECESSARY) shouldBe "1.2350"
    assertThrows[ArithmeticException](scaled(1235L, 3).format(2, RoundingMode.UNNECESSARY))
    assertThrows[ArithmeticException](scaled(1235L, 3).format(0, RoundingMode.UNNECESSARY))
  }

  //-------------------------------------------------------------------------
  // Comparison.

  test("comparison is numerical and consistent with equality") {
    Decimal.ZERO.compareTo(Decimal.ZERO) shouldBe 0
    Decimal.ZERO.compareTo(scaled(1L, 18)) should be < 0
    Decimal.ZERO.compareTo(scaled(1L, 0)) should be < 0
    Decimal.ZERO.compareTo(Decimal.MAX_VALUE) should be < 0
    Decimal.ZERO.compareTo(scaled(-1L, 18)) should be > 0
    Decimal.ZERO.compareTo(scaled(-1L, 0)) should be > 0
    Decimal.ZERO.compareTo(Decimal.MIN_VALUE) should be > 0

    val base = text("1.23")
    base.compareTo(scaled(123L, 2)) shouldBe 0
    base.compareTo(text("2")) should be < 0
    base.compareTo(text("1.230000000001")) should be < 0
    base.compareTo(text("1.231")) should be < 0
    base.compareTo(text("1.24")) should be < 0
    base.compareTo(text("0")) should be > 0
    base.compareTo(text("1.229999999999")) should be > 0
    base.compareTo(text("1.229")) should be > 0
    base.compareTo(text("1.22")) should be > 0
    base.compareTo(text("1.2")) should be > 0
  }

  //-------------------------------------------------------------------------
  // A wide sweep of seeded random values, ported from the `testMega` case.

  test("arithmetic agrees with BigDecimal over ten thousand seeded random pairs") {
    // the generator is seeded, so this case is as reproducible as any other: it is a breadth
    // sweep rather than a property, and it is the case that catches a rescaling mistake that
    // the tabulated rows happen not to reach
    val random = new Random(1001L)
    val context = new MathContext(Decimal.MAX_SCALE, RoundingMode.HALF_UP)
    val truncatingContext = new MathContext(Decimal.MAX_SCALE, RoundingMode.DOWN)
    (0 until 10000).foreach { _ =>
      val base = scaled(random.nextInt().toLong, random.nextInt(19))
      val other = scaled(random.nextInt().toLong, random.nextInt(19))
      withClue(s"$base and $other: ") {
        Decimal.of(base.toBigDecimal.add(other.toBigDecimal)) shouldBe Right(base.plus(other))
        Decimal.of(base.toBigDecimal.subtract(other.toBigDecimal)) shouldBe Right(base.minus(other))
        if (base.abs.unscaledValue < 999999999L && other.abs.unscaledValue < 999999999L) {
          Decimal.of(base.toBigDecimal.multiply(other.toBigDecimal)) shouldBe
            Right(base.multipliedBy(other))
          // the divisor is checked for zero as well as for scale: the ported case relies on a
          // seed that never draws zero, and this keeps the case honest under any seed
          if (other.scale < 9 && !other.isZero) {
            Decimal.of(base.toBigDecimal.divide(other.toBigDecimal, context)) shouldBe
              Right(base.dividedBy(other, RoundingMode.HALF_UP))
            Decimal.of(base.toBigDecimal.divide(other.toBigDecimal, truncatingContext)) shouldBe
              Right(base.dividedBy(other, RoundingMode.DOWN))
            Decimal.of(base.toBigDecimal.remainder(other.toBigDecimal)) shouldBe
              Right(base.remainder(other))
          }
        }
      }
    }
    succeed
  }

  //-------------------------------------------------------------------------
  // The four throughput harnesses of the ported class, reduced to the equivalence each one
  // relied on. No case here reads a clock: a wall-clock assertion is not reproducible, and
  // the harnesses asserted nothing at all, so what is worth keeping is that the fast path
  // and `BigDecimal` agree over the inputs the harness cycled through.

  test("a decimal built from an unscaled value converts as BigDecimal does") {
    (0 until 1000).foreach { index =>
      val value = index.toLong
      scaled(value, 2).longValue shouldBe BigDecimal.valueOf(value, 2).longValue
    }
    succeed
  }

  test("a decimal read from text converts as BigDecimal does") {
    ThroughputTexts.foreach { str =>
      withClue(s"reading $str: ") {
        text(str).longValue shouldBe new BigDecimal(str).longValue
      }
    }
    succeed
  }

  test("a decimal multiplied by a whole number converts as BigDecimal does") {
    ThroughputTexts.foreach { str =>
      withClue(s"multiplying $str: ") {
        text(str).multipliedBy(2L).longValue shouldBe
          new BigDecimal(str).multiply(BigDecimal.valueOf(2L)).longValue
        text(str).multipliedBy(2L) shouldBe
          fromBigDecimal(new BigDecimal(str).multiply(BigDecimal.valueOf(2L)))
      }
    }
    succeed
  }

  test("a decimal rounded to one decimal place converts as BigDecimal does") {
    ThroughputTexts.foreach { str =>
      withClue(s"rounding $str: ") {
        text(str).roundToScale(1, RoundingMode.HALF_UP).longValue shouldBe
          new BigDecimal(str).setScale(1, RoundingMode.HALF_UP).longValue
        text(str).roundToScale(1, RoundingMode.HALF_UP) shouldBe
          fromBigDecimal(new BigDecimal(str).setScale(1, RoundingMode.HALF_UP))
      }
    }
    succeed
  }


  //-------------------------------------------------------------------------
  // The instances, and the single canonical text they all agree on. None of this is in the
  // ported test class, which had reflective bean serialization and a `Comparable` instead.

  test("one equality-bearing instance carries comparison, hashing and equality together") {
    val order = implicitly[Order[Decimal]]
    val hash = implicitly[Hash[Decimal]]
    // `Eq` is obtained by subtyping rather than declared a second time, which is what makes
    // it impossible for equality and comparison to disagree
    val equality = implicitly[Eq[Decimal]]
    val left = text("1.23")
    val right = scaled(123L, 2)
    order.compare(left, right) shouldBe 0
    order.eqv(left, right) shouldBe true
    equality.eqv(left, right) shouldBe true
    hash.hash(left) shouldBe hash.hash(right)
    order.compare(left, text("1.24")) should be < 0
    order.eqv(left, text("1.24")) shouldBe false
  }

  test("the text, the Show output and the JSON string are one canonical form") {
    forAll(genDecimal) { decimal =>
      val canonical = decimal.toString
      decimal.show shouldBe canonical
      decimal.asJson.asString shouldBe Some(canonical)
    }
  }

  test("a decimal is JSON as a bare string, not as a JSON number") {
    // a JSON number is read back through a Double by many parsers, which would lose the
    // exactness this type exists to provide
    scaled(123L, 2).asJson.noSpaces shouldBe "\"1.23\""
    Decimal.ZERO.asJson.noSpaces shouldBe "\"0\""
    Decimal.MAX_VALUE.asJson.noSpaces shouldBe "\"999999999999999999\""
    scaled(-123L, 4).asJson.noSpaces shouldBe "\"-0.0123\""
  }

  test("a decimal survives a JSON round trip") {
    forAll(genDecimal) { decimal =>
      parser.decode[Decimal](decimal.asJson.noSpaces) shouldBe Right(decimal)
    }
  }

  test("JSON that names no decimal is rejected by the decoder") {
    parser.decode[Decimal]("\"nonsense\"").isLeft shouldBe true
    parser.decode[Decimal]("\"\"").isLeft shouldBe true
    // a JSON number is not the encoded form, so it is not accepted as one
    parser.decode[Decimal]("1.23").isLeft shouldBe true
  }

  test("the text of a decimal is read back as the same decimal") {
    forAll(genDecimal) { decimal =>
      Decimal.of(decimal.toString) shouldBe Right(decimal)
      Decimal.parse(decimal.toString) shouldBe Right(decimal)
    }
  }

  //-------------------------------------------------------------------------
  // Properties over the whole domain of the type, which the tabulated rows cannot cover.

  test("a factory applied to its own output changes nothing") {
    forAll(genDecimal) { decimal =>
      Decimal.ofScaled(decimal.unscaledValue, decimal.scale) shouldBe Right(decimal)
      Decimal.of(decimal.toBigDecimal) shouldBe Right(decimal)
    }
  }

  test("normalisation leaves no trailing zero in a fraction and no scale out of range") {
    forAll(genDecimal) { decimal =>
      decimal.scale should be >= 0
      decimal.scale should be <= Decimal.MAX_SCALE
      if (decimal.scale > 0) {
        decimal.unscaledValue % 10L should not be 0L
      }
      if (decimal.isZero) {
        decimal.scale shouldBe 0
      }
      succeed
    }
  }

  test("each of the three invariants of this type refuses with the sentence the type states") {
    // The three phrases below are the ones the construction block of the type states, and the
    // sentences are the complete text a refusal reports - which is contract, because a value
    // forged by a class file compiled outside this library is refused by these very words and
    // nothing else records why it was refused.
    //
    // The refusal is provoked through the member the construction block calls, since no route
    // exists from Scala source to an instance that breaks one of these: the implementation is
    // a private class of the companion, the case class is abstract and has a private
    // constructor, so neither the constructor nor a `copy` nor a subclass can be written here.
    // What the properties above establish is the other half of the same statement - that no
    // value the factories produce breaks any of the three.
    intercept[IllegalArgumentException](JvmClosure.requireInvariant(
      s"its scale is between 0 and ${Decimal.MAX_SCALE}",
      condition = false)).getMessage shouldBe
      "a value of this type requires that its scale is between 0 and 18, and the value being " +
        "constructed does not: a value of this type is obtained from its factory"
    intercept[IllegalArgumentException](JvmClosure.requireInvariant(
      "its unscaled value carries no trailing zero that its scale could absorb",
      condition = false)).getMessage shouldBe
      "a value of this type requires that its unscaled value carries no trailing zero that " +
        "its scale could absorb, and the value being constructed does not: a value of this " +
        "type is obtained from its factory"
    intercept[IllegalArgumentException](JvmClosure.requireInvariant(
      "its unscaled value is within the precision of this type",
      condition = false)).getMessage shouldBe
      "a value of this type requires that its unscaled value is within the precision of this " +
        "type, and the value being constructed does not: a value of this type is obtained " +
        "from its factory"
    // the scale bound quoted by the first sentence is the one the type publishes
    Decimal.MAX_SCALE shouldBe 18
  }

  test("comparison returns zero exactly when two decimals are equal") {
    forAll(genDecimal, genDecimal) { (left, right) =>
      (left.compareTo(right) == 0) shouldBe (left == right)
    }
  }

  test("negating a decimal twice gives it back") {
    forAll(genDecimal) { decimal =>
      decimal.negated.negated shouldBe decimal
      decimal.negated.scale shouldBe decimal.scale
    }
  }

  test("the absolute value and the sign of a decimal agree with one another") {
    forAll(genDecimal) { decimal =>
      decimal.abs.signum should be >= 0
      decimal.abs shouldBe (if (decimal.signum < 0) decimal.negated else decimal)
      decimal.signum shouldBe decimal.toBigDecimal.signum
    }
  }

  test("the sum of two decimals is commutative, and overflows on both orders alike") {
    forAll(genDecimal, genDecimal) { (left, right) =>
      // the oracle is the exact sum held by BigDecimal: where it fits, both orders give it,
      // and where it does not, both orders raise
      Decimal.of(left.toBigDecimal.add(right.toBigDecimal)) match {
        case Right(expected) =>
          left.plus(right) shouldBe expected
          right.plus(left) shouldBe expected
        case Left(_) =>
          assertThrows[IllegalArgumentException](left.plus(right))
          assertThrows[IllegalArgumentException](right.plus(left))
      }
    }
  }

  test("rounding to a scale is idempotent and never raises the scale") {
    forAll(genDecimal) { decimal =>
      val rounded = decimal.roundToScale(2, RoundingMode.HALF_UP)
      rounded.scale should be <= 2
      rounded.roundToScale(2, RoundingMode.HALF_UP) shouldBe rounded
    }
  }

  test("moving the decimal point and moving it back recovers the decimal") {
    forAll(genDecimal) { decimal =>
      // a movement to the left is always representable; moving back is only guaranteed while
      // the value keeps its digits, which the scale bound below establishes
      if (decimal.scale <= Decimal.MAX_SCALE - 2) {
        decimal.movePoint(-2).movePoint(2) shouldBe decimal
      }
      succeed
    }
  }

  test("a decimal presented at a fixed scale keeps its value") {
    forAll(genDecimal) { decimal =>
      val presented = decimal.toFixedScale(Decimal.MAX_SCALE)
      presented should beSuccess
      presented.map(fixed => fixed.decimal) shouldBe Right(decimal)
    }
  }

  test("a fixed scale below the scale of a decimal is rejected") {
    forAll(genDecimal) { decimal =>
      if (decimal.scale > 0) {
        decimal.toFixedScale(decimal.scale - 1) should beFailure
      }
      succeed
    }
  }

  test("formatting to at least the scale of a decimal is its own text") {
    forAll(genDecimal) { decimal =>
      decimal.formatAtLeast(decimal.scale) shouldBe decimal.toString
      decimal.formatAtLeast(0) shouldBe decimal.toString
    }
  }

  test("the conversions of a decimal agree with BigDecimal") {
    forAll(genDecimal) { decimal =>
      decimal.toBigDecimal shouldBe BigDecimal.valueOf(decimal.unscaledValue, decimal.scale)
      decimal.longValue shouldBe decimal.toBigDecimal.longValue
      decimal.doubleValue shouldBe decimal.toBigDecimal.doubleValue
      decimal.toString shouldBe decimal.toBigDecimal.toPlainString
    }
  }
}
