/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.value

import java.math.BigDecimal

import cats.Hash
import cats.Show

import io.circe.Json
import io.circe.parser.decode
import io.circe.parser.parse
import io.circe.syntax._

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor3

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[HalfUp]], the half-up rounding convention of [[Rounding]].
 *
 * A convention rounds to a number of decimal places from 0 to 255 inclusive, optionally to a
 * fraction of the smallest of them from 0 to 256 inclusive. The two inputs are validated
 * independently, and a fraction of 1 collapses to 0 only after both have passed, so an instance
 * never holds a fraction of 1.
 *
 * All three rounding overloads are compared exactly, with no tolerance.
 */
final class HalfUpRoundingSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /** The message the factories report for decimal places outside the permitted range. */
  private val DecimalPlacesMessage: String = "Invalid decimal places, must be from 0 to 255 inclusive"

  /** The message the factories report for a fraction outside the permitted range. */
  private val FractionMessage: String = "Invalid fraction, must be from 0 to 256 inclusive"

  /** The JSON form of the convention that rounds to four decimal places with no fractional part. */
  private val ExpectedJson: String = """{"HalfUp":{"decimalPlaces":4,"fraction":0}}"""

  /** The JSON form of the convention that rounds to the nearest 1/32nd of the fourth decimal place. */
  private val ExpectedFractionalJson: String = """{"HalfUp":{"decimalPlaces":4,"fraction":32}}"""

  //-------------------------------------------------------------------------
  /**
   * Returns the convention a factory outcome holds; a rejected outcome fails the suite, naming
   * the failures it carries.
   */
  private def rounding[A <: Rounding](result: ResultNec[A]): A =
    result.fold(
      _ => fail(s"invalid rounding fixture: ${messages(result).mkString("; ")}"),
      convention => convention)

  /** Returns the messages of the failures an outcome holds, in order, empty when it holds none. */
  private def messages(result: ResultNec[Rounding]): List[String] =
    result.fold(failures => failures.toChain.toList.map(_.message), _ => List.empty[String])

  /** Returns the decimal a `Double` names, failing the suite when the fixture is not one. */
  private def decimal(value: Double): Decimal =
    Decimal.of(value).fold(
      failure => fail(s"invalid decimal fixture: ${failure.message}"),
      held => held)

  /**
   * Returns one of the expected JSON forms above as a document, so an encoding is compared as a
   * document rather than as printed text, failing the suite when the literal does not parse.
   */
  private def json(text: String): Json =
    parse(text).getOrElse(fail(s"the expected JSON of this spec is not itself valid JSON: $text"))

  //-------------------------------------------------------------------------
  test("test_of_Currency") {
    val test: Rounding = Rounding.of(Currency.USD)
    test.round(63.455d) shouldBe 63.46d
    test.round(63.454d) shouldBe 63.45d

    // The number of minor units of a currency is always a permitted number of decimal places,
    // so this factory is total and returns a convention rather than an outcome.
    Currency.USD.minorUnitDigits shouldBe 2
    test shouldBe rounding(HalfUp.ofDecimalPlaces(2))
  }

  //-------------------------------------------------------------------------
  test("test_ofDecimalPlaces") {
    val test: HalfUp = rounding(HalfUp.ofDecimalPlaces(4))
    test.decimalPlaces shouldBe 4
    test.fraction shouldBe 0
    test.toString shouldBe "Round to 4dp"

    val widened: ResultNec[Rounding] = Rounding.ofDecimalPlaces(4)
    widened should beSuccess
    widened should haveValue(test)
  }

  //-------------------------------------------------------------------------
  test("test_ofDecimalPlaces_big") {
    val test: HalfUp = rounding(HalfUp.ofDecimalPlaces(40))
    test.decimalPlaces shouldBe 40
    test.fraction shouldBe 0
    test.toString shouldBe "Round to 40dp"

    val widened: ResultNec[Rounding] = Rounding.ofDecimalPlaces(40)
    widened should haveValue(test)

    // `HalfUp` caches sixteen instances, one per number of decimal places with no fractional
    // part, so forty decimal places is off the cached path while 15 and 16 straddle its
    // boundary. All three hold no fractional part.
    rounding(HalfUp.ofDecimalPlaces(15)).fraction shouldBe 0
    rounding(HalfUp.ofDecimalPlaces(16)).fraction shouldBe 0
  }

  //-------------------------------------------------------------------------
  test("test_ofDecimalPlaces_invalid") {
    // The factories report a rejection as a value: the reason is a member of the closed family
    // of reasons, and the message names the decimal places.
    val negative: ResultNec[HalfUp] = HalfUp.ofDecimalPlaces(-1)
    negative should beFailureWith(FailureReason.INVALID)
    negative should haveFailureMessageMatching(".*decimal places.*")
    messages(negative) shouldBe List(DecimalPlacesMessage)

    val tooBig: ResultNec[HalfUp] = HalfUp.ofDecimalPlaces(257)
    tooBig should beFailureWith(FailureReason.INVALID)
    tooBig should haveFailureMessageMatching(".*decimal places.*")
    messages(tooBig) shouldBe List(DecimalPlacesMessage)

    val negativeWidened: ResultNec[Rounding] = Rounding.ofDecimalPlaces(-1)
    val tooBigWidened: ResultNec[Rounding] = Rounding.ofDecimalPlaces(257)
    negativeWidened should beFailureWith(FailureReason.INVALID)
    tooBigWidened should beFailureWith(FailureReason.INVALID)
    messages(negativeWidened) shouldBe List(DecimalPlacesMessage)
    messages(tooBigWidened) shouldBe List(DecimalPlacesMessage)

    // The edges of the range the message names: 255 decimal places is the largest accepted and
    // 256 is already too many.
    rounding(HalfUp.ofDecimalPlaces(255)).decimalPlaces shouldBe 255
    val justTooBig: ResultNec[HalfUp] = HalfUp.ofDecimalPlaces(256)
    justTooBig should beFailureWith(FailureReason.INVALID)
    messages(justTooBig) shouldBe List(DecimalPlacesMessage)
  }

  //-------------------------------------------------------------------------
  test("test_ofFractionalDecimalPlaces") {
    val test: HalfUp = rounding(HalfUp.ofFractionalDecimalPlaces(4, 32))
    test.decimalPlaces shouldBe 4
    test.fraction shouldBe 32
    test.toString shouldBe "Round to 1/32 of 4dp"

    val widened: ResultNec[Rounding] = Rounding.ofFractionalDecimalPlaces(4, 32)
    widened should beSuccess
    widened should haveValue(test)
  }

  //-------------------------------------------------------------------------
  test("test_ofFractionalDecimalPlaces_invalid") {
    // The two inputs are validated independently, and the message names the one that was
    // rejected.
    val negativePlaces: ResultNec[HalfUp] = HalfUp.ofFractionalDecimalPlaces(-1, 0)
    negativePlaces should beFailureWith(FailureReason.INVALID)
    negativePlaces should haveFailureMessageMatching(".*decimal places.*")
    messages(negativePlaces) shouldBe List(DecimalPlacesMessage)

    val tooManyPlaces: ResultNec[HalfUp] = HalfUp.ofFractionalDecimalPlaces(257, 0)
    tooManyPlaces should beFailureWith(FailureReason.INVALID)
    tooManyPlaces should haveFailureMessageMatching(".*decimal places.*")
    messages(tooManyPlaces) shouldBe List(DecimalPlacesMessage)

    val negativeFraction: ResultNec[HalfUp] = HalfUp.ofFractionalDecimalPlaces(0, -1)
    negativeFraction should beFailureWith(FailureReason.INVALID)
    negativeFraction should haveFailureMessageMatching(".*fraction.*")
    messages(negativeFraction) shouldBe List(FractionMessage)

    val tooBigFraction: ResultNec[HalfUp] = HalfUp.ofFractionalDecimalPlaces(0, 257)
    tooBigFraction should beFailureWith(FailureReason.INVALID)
    tooBigFraction should haveFailureMessageMatching(".*fraction.*")
    messages(tooBigFraction) shouldBe List(FractionMessage)

    val widened: ResultNec[Rounding] = Rounding.ofFractionalDecimalPlaces(0, 257)
    widened should beFailureWith(FailureReason.INVALID)
    messages(widened) shouldBe List(FractionMessage)

    // A fraction of 256 is the largest accepted, one more than the largest number of decimal
    // places, which is why the two messages name different bounds.
    rounding(HalfUp.ofFractionalDecimalPlaces(0, 256)).fraction shouldBe 256

    // Both inputs wrong at once yields both failures from one call, accumulated in check order:
    // the decimal places before the fraction.
    val bothWrong: ResultNec[HalfUp] = HalfUp.ofFractionalDecimalPlaces(-1, 257)
    bothWrong should beFailureWith(FailureReason.INVALID)
    bothWrong should haveFailureMessageMatching(".*decimal places.*")
    bothWrong should haveFailureMessageMatching(".*fraction.*")
    messages(bothWrong).size shouldBe 2
    messages(bothWrong) shouldBe List(DecimalPlacesMessage, FractionMessage)
  }

  //-------------------------------------------------------------------------
  test("test_builder") {
    val test: HalfUp = rounding(HalfUp.ofFractionalDecimalPlaces(4, 1))
    test.decimalPlaces shouldBe 4
    test.fraction shouldBe 0
    test.toString shouldBe "Round to 4dp"

    // A fraction of one scales the value by one before rounding and back afterwards, which is
    // the rounding performed with no fractional part at all, so these three spellings name one
    // and the same convention and an instance never holds a fraction of one.
    test shouldBe rounding(HalfUp.ofFractionalDecimalPlaces(4, 0))
    test shouldBe rounding(HalfUp.ofDecimalPlaces(4))

    // The collapse is applied after the checks and is idempotent: handing the fraction an
    // instance holds back to the factory yields an equal instance, whether it is none or a real
    // one.
    rounding(HalfUp.ofFractionalDecimalPlaces(4, test.fraction)) shouldBe test
    val fractional: HalfUp = rounding(HalfUp.ofFractionalDecimalPlaces(4, 32))
    rounding(HalfUp.ofFractionalDecimalPlaces(4, fractional.fraction)) shouldBe fractional
  }

  //-------------------------------------------------------------------------
  test("test_builder_invalid") {
    // The inputs are checked before the collapse of a fraction of one to none, so a fraction of
    // -1 is rejected rather than absorbed by a normalisation that maps anything at or below one
    // to none.
    val negativePlaces: ResultNec[HalfUp] = HalfUp.ofDecimalPlaces(-1)
    negativePlaces should beFailureWith(FailureReason.INVALID)
    negativePlaces should haveFailureMessageMatching(".*decimal places.*")
    messages(negativePlaces) shouldBe List(DecimalPlacesMessage)

    val tooManyPlaces: ResultNec[HalfUp] = HalfUp.ofDecimalPlaces(257)
    tooManyPlaces should beFailureWith(FailureReason.INVALID)
    tooManyPlaces should haveFailureMessageMatching(".*decimal places.*")
    messages(tooManyPlaces) shouldBe List(DecimalPlacesMessage)

    val negativeFraction: ResultNec[HalfUp] = HalfUp.ofFractionalDecimalPlaces(4, -1)
    negativeFraction should beFailureWith(FailureReason.INVALID)
    negativeFraction should haveFailureMessageMatching(".*fraction.*")
    messages(negativeFraction) shouldBe List(FractionMessage)

    val tooBigFraction: ResultNec[HalfUp] = HalfUp.ofFractionalDecimalPlaces(4, 257)
    tooBigFraction should beFailureWith(FailureReason.INVALID)
    tooBigFraction should haveFailureMessageMatching(".*fraction.*")
    messages(tooBigFraction) shouldBe List(FractionMessage)
  }

  //-------------------------------------------------------------------------
  /**
   * The table shared by `round_double`, `round_BigDecimal` and `round_Decimal`.
   *
   * The first six rows round to two decimal places with no fractional part and sit either side
   * of a tie: below it a value rounds down, the tie itself rounds away from zero, above it a
   * value rounds up. The last six round to the nearest half of the second decimal place - a
   * fraction of two - so their answers are multiples of half of it, such as 12.345 and 12.340.
   */
  private val data_round: TableFor3[HalfUp, Double, Double] = Table(
    ("rounding", "input", "expected"),
    (rounding(HalfUp.ofDecimalPlaces(2)), 12.3449d, 12.34d),
    (rounding(HalfUp.ofDecimalPlaces(2)), 12.3450d, 12.35d),
    (rounding(HalfUp.ofDecimalPlaces(2)), 12.3451d, 12.35d),
    (rounding(HalfUp.ofDecimalPlaces(2)), 12.3500d, 12.35d),
    (rounding(HalfUp.ofDecimalPlaces(2)), 12.3549d, 12.35d),
    (rounding(HalfUp.ofDecimalPlaces(2)), 12.3550d, 12.36d),
    (rounding(HalfUp.ofFractionalDecimalPlaces(2, 2)), 12.3424d, 12.340d),
    (rounding(HalfUp.ofFractionalDecimalPlaces(2, 2)), 12.3425d, 12.345d),
    (rounding(HalfUp.ofFractionalDecimalPlaces(2, 2)), 12.3426d, 12.345d),
    (rounding(HalfUp.ofFractionalDecimalPlaces(2, 2)), 12.3449d, 12.345d),
    (rounding(HalfUp.ofFractionalDecimalPlaces(2, 2)), 12.3450d, 12.345d),
    (rounding(HalfUp.ofFractionalDecimalPlaces(2, 2)), 12.3451d, 12.345d))

  //-------------------------------------------------------------------------
  test("round_double") {
    forAll(data_round) { (convention: HalfUp, input: Double, expected: Double) =>
      convention.round(input) shouldBe expected
    }
  }

  //-------------------------------------------------------------------------
  test("round_BigDecimal") {
    // Compared with the equality of `BigDecimal`, which distinguishes scale: the fractional rows
    // come back at the scale the exact quotient of the division by the fraction needs, which is
    // three for a half of the second decimal place.
    forAll(data_round) { (convention: HalfUp, input: Double, expected: Double) =>
      convention.round(BigDecimal.valueOf(input)) shouldBe BigDecimal.valueOf(expected)
    }
  }

  //-------------------------------------------------------------------------
  test("round_Decimal") {
    forAll(data_round) { (convention: HalfUp, input: Double, expected: Double) =>
      convention.round(decimal(input)) shouldBe decimal(expected)
    }
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    val test: HalfUp = rounding(HalfUp.ofDecimalPlaces(4))
    val test2: HalfUp = rounding(HalfUp.ofFractionalDecimalPlaces(4, 32))

    val same: HalfUp = rounding(HalfUp.ofFractionalDecimalPlaces(4, 0))
    same shouldBe test
    same.hashCode shouldBe test.hashCode
    Hash[Rounding].eqv(same, test) shouldBe true
    Hash[Rounding].hash(same) shouldBe test.hashCode

    // Either field differing makes a convention unequal, under the platform equality and under
    // the typeclass alike.
    test2 should not be test
    Hash[Rounding].eqv(test2, test) shouldBe false
    val otherPlaces: HalfUp = rounding(HalfUp.ofDecimalPlaces(5))
    otherPlaces should not be test
    Hash[Rounding].eqv(otherPlaces, test) shouldBe false

    Rounding.none should not be test
    Hash[Rounding].eqv(Rounding.none, test) shouldBe false

    // The collapse of a fraction of one makes these three spellings one and the same convention.
    rounding(HalfUp.ofFractionalDecimalPlaces(4, 1)) shouldBe test
    rounding(HalfUp.ofFractionalDecimalPlaces(4, 0)) shouldBe test
    rounding(HalfUp.ofDecimalPlaces(4)) shouldBe test

    // `Show` renders what `toString` renders.
    Show[Rounding].show(test) shouldBe test.toString
    Show[Rounding].show(test2) shouldBe test2.toString
    test.toString shouldBe "Round to 4dp"
    test2.toString shouldBe "Round to 1/32 of 4dp"

    // The construction surface is closed: the primary constructor is private and neither an
    // `apply` nor a `copy` exists, so the only way to a convention is a validating factory.
    // Each snippet below is required not to compile.
    assertDoesNotCompile("""HalfUp(4, 0)""")
    assertDoesNotCompile("""Rounding.HalfUp(4, 0)""")
    assertDoesNotCompile("""HalfUp.ofDecimalPlaces(4).map(_.copy(decimalPlaces = 2))""")
    assertDoesNotCompile("""Rounding.ofDecimalPlaces(4).map(_.copy())""")

    // `unapply` is unaffected by that closure, so the two fields are readable by pattern.
    val matched: (Int, Int) = test2 match {
      case HalfUp(places, fraction) => (places, fraction)
    }
    matched shouldBe ((4, 32))
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // A convention is written as an object of one field, whose name is the member and whose
    // value holds that member's fields. Both directions are asserted, because an encoding and a
    // decoding wrong in the same way would still round trip.
    val test: Rounding = rounding(HalfUp.ofDecimalPlaces(4))
    test.asJson shouldBe json(ExpectedJson)
    decode[Rounding](test.asJson.noSpaces) shouldBe Right(test)
    decode[Rounding](ExpectedJson) shouldBe Right(test)

    val fractional: Rounding = rounding(HalfUp.ofFractionalDecimalPlaces(4, 32))
    fractional.asJson shouldBe json(ExpectedFractionalJson)
    decode[Rounding](fractional.asJson.noSpaces) shouldBe Right(fractional)
    decode[Rounding](ExpectedFractionalJson) shouldBe Right(fractional)

    // The decoder routes both fields through the same validating factory a caller's arguments go
    // through, so a document naming a number of decimal places or a fraction outside the
    // permitted range is rejected, and the message names which of the two was at fault.
    val rejectedPlaces = decode[Rounding]("""{"HalfUp":{"decimalPlaces":-1,"fraction":0}}""")
    rejectedPlaces.isLeft shouldBe true
    rejectedPlaces.swap.toOption.fold("")(error => error.getMessage) should include("decimal places")

    val rejectedFraction = decode[Rounding]("""{"HalfUp":{"decimalPlaces":4,"fraction":257}}""")
    rejectedFraction.isLeft shouldBe true
    rejectedFraction.swap.toOption.fold("")(error => error.getMessage) should include("fraction")

    // The payload of the member is read as this member's own fields, so a payload that is not an
    // object names no fields to validate and is rejected.
    val rejectedShape = decode[Rounding]("""{"HalfUp":4}""")
    rejectedShape.isLeft shouldBe true
    rejectedShape.swap.toOption.fold("")(error => error.getMessage) should include("decimalPlaces")
  }
}
