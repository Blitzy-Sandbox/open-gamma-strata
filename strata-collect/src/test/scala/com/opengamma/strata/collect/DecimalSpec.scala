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

import cats.Eq
import cats.Hash
import cats.Order
import cats.syntax.all._

// the package of this file holds a sub-package named `io`, which shadows the top-level `io`
// package that circe lives in, so circe is reached from the root as the main sources reach it
import _root_.io.circe.parser
import _root_.io.circe.syntax._

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

