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

import com.opengamma.strata.collect.Decimal

/**
 * Test [[NoRounding]], the member of [[Rounding]] that makes no change.
 *
 * [[Rounding.none]] is a singleton, and this member states all three `round` overloads itself,
 * each returning the value it was given, so one test reads each of them.
 */
final class NoRoundingSpec extends AnyFunSuite with Matchers {

  /** The value rounded, in each of the three representations below. */
  private val Input: Double = 1.23d

  /** The rendering of the convention. */
  private val ExpectedText: String = "No rounding"

  /**
   * The JSON form of the convention: the wrapper object of the closed family, keyed by the
   * member, over that member's fields, of which this member has none.
   */
  private val ExpectedJson: String = """{"NoRounding":{}}"""

  /**
   * Returns the expected form above as a document, so an encoding is compared as a document
   * rather than as printed text, failing the suite when the literal does not parse.
   */
  private def json(text: String): Json =
    parse(text).getOrElse(fail(s"the expected JSON of this spec is not itself valid JSON: $text"))

  /**
   * Returns the decimal fixture the decimal overload is asserted against, failing the suite and
   * naming itself when the value is not a decimal.
   */
  private def decimal(value: Double): Decimal =
    Decimal
      .of(value)
      .fold(
        failure => fail(s"the expected decimal of this spec is not itself a decimal: ${failure.message}"),
        identity)

  /**
   * Returns the half-up fixture `coverage` compares against, failing the suite and naming
   * itself when the number of decimal places describes no convention.
   */
  private def halfUpRounding(decimalPlaces: Int): Rounding =
    Rounding
      .ofDecimalPlaces(decimalPlaces)
      .fold(
        failures => fail(s"the expected rounding of this spec is not itself a rounding: $failures"),
        identity)

  //-------------------------------------------------------------------------
  test("test_none") {
    val test: Rounding = Rounding.none

    test.toString shouldBe ExpectedText

    // The factory hands back a singleton, so two calls are the same object and not merely equal
    // values.
    Rounding.none shouldBe test
    (Rounding.none eq test) shouldBe true

    // The value the factory returns is the named member of the family, so a call site can state
    // the convention it holds and a pattern match can read it - see `coverage` below.
    test shouldBe NoRounding

    // `Show` renders what `toString` renders.
    Show[Rounding].show(test) shouldBe ExpectedText
  }

  //-------------------------------------------------------------------------
  test("round_double") {
    Rounding.none.round(Input) shouldBe 1.23d

    // This convention states the double overload itself rather than inheriting the trip through
    // a decimal and back that the family declares, which is what keeps it exact for a value no
    // decimal names: the three values neither JSON nor `BigDecimal` can express are handed
    // straight back instead of failing on the way through.
    Rounding.none.round(Double.NaN).isNaN shouldBe true
    Rounding.none.round(Double.PositiveInfinity) shouldBe Double.PositiveInfinity
    Rounding.none.round(Double.NegativeInfinity) shouldBe Double.NegativeInfinity

    // The same exactness, bit for bit, for the negative zero that an equality of doubles cannot
    // tell from the positive one: the raw bits are what distinguish the two, so a comparison of
    // the doubles would hold whether or not the sign survived.
    java.lang.Double.doubleToRawLongBits(Rounding.none.round(-0.0d)) shouldBe
      java.lang.Double.doubleToRawLongBits(-0.0d)
  }

  //-------------------------------------------------------------------------
  test("round_BigDecimal") {
    Rounding.none.round(BigDecimal.valueOf(Input)) shouldBe BigDecimal.valueOf(Input)

    // The comparison above is the `equals` of `BigDecimal`, which distinguishes the scale, so it
    // says the scale is preserved as well as the number; a comparison by `compareTo` would hold
    // for the value at any scale and would say less. The two assertions below name the scale and
    // the text that are preserved.
    val rounded = Rounding.none.round(BigDecimal.valueOf(Input))
    rounded.scale shouldBe 2
    rounded.toPlainString shouldBe "1.23"

    // Nothing is changed and nothing is copied: the value handed back is the object handed in.
    val value = BigDecimal.valueOf(Input)
    (Rounding.none.round(value) eq value) shouldBe true
  }

  //-------------------------------------------------------------------------
  test("round_Decimal") {
    Rounding.none.round(decimal(Input)) shouldBe decimal(Input)

    // As with the `BigDecimal` overload, the value handed back is the object handed in.
    val value = decimal(Input)
    (Rounding.none.round(value) eq value) shouldBe true
    Rounding.none.round(value).toString shouldBe "1.23"
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    val test: Rounding = Rounding.none

    // The convention is a singleton: every route to it - the factory, the alias on the
    // companion, and the member named directly - reaches one object, so its equality is
    // identity.
    (test eq NoRounding) shouldBe true
    (Rounding.NoRounding eq NoRounding) shouldBe true

    Hash[Rounding].eqv(test, NoRounding) shouldBe true
    Hash[Rounding].hash(test) shouldBe Hash[Rounding].hash(NoRounding)

    // The other member of the family is a different convention, and both notions of equality
    // say so.
    val halfUp = halfUpRounding(2)
    test should not be halfUp
    Hash[Rounding].eqv(test, halfUp) shouldBe false
    halfUp should not be test

    // Both members render the same way through `Show` as through `toString`.
    Show[Rounding].show(test) shouldBe test.toString
    Show[Rounding].show(halfUp) shouldBe halfUp.toString
    test.toString shouldBe ExpectedText
    halfUp.toString shouldBe "Round to 2dp"

    // The family has exactly two members, so this match names both with no default branch and
    // the compiler checks that it is exhaustive.
    val described: String = test match {
      case NoRounding => "the convention that makes no change"
      case other: HalfUp => s"the half-up convention rounding to ${other.decimalPlaces}dp"
    }
    described shouldBe "the convention that makes no change"
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    val test: Rounding = Rounding.none

    // The encoding is the wrapper object of the closed family, keyed by the member and holding
    // that member's fields, of which this member has none; decoding it returns the member it
    // came from. Both directions are asserted, because an encoding and a decoding wrong in the
    // same way would still round trip.
    test.asJson shouldBe json(ExpectedJson)
    decode[Rounding](ExpectedJson) shouldBe Right(NoRounding)
    decode[Rounding](test.asJson.noSpaces) shouldBe Right(test)

    // The shape spelled out: one field, named for the member, over an object holding nothing.
    test.asJson.asObject.map(_.keys.toList) shouldBe Some(List("NoRounding"))
    test.asJson.hcursor.downField("NoRounding").as[Json] shouldBe Right(Json.obj())

    // This member has no fields, so the empty object above is the whole of what it accepts: a
    // payload that is not an object, or an object carrying a field, would otherwise decode to
    // this convention while what it held was discarded. Each is rejected, and the message of
    // each rejection is asserted rather than only its presence.
    def rejectionOf(text: String): String =
      decode[Rounding](text).fold(
        error => error.getMessage,
        decodedConvention =>
          fail(s"a document outside the shape of the family decoded as $decodedConvention: $text"))

    rejectionOf("""{"NoRounding":123}""") should include("'NoRounding' must hold an empty object")
    rejectionOf("""{"NoRounding":null}""") should include("'NoRounding' must hold an empty object")
    rejectionOf("""{"NoRounding":[1]}""") should include("'NoRounding' must hold an empty object")
    rejectionOf("""{"NoRounding":"x"}""") should include("'NoRounding' must hold an empty object")
    rejectionOf("""{"NoRounding":{"decimalPlaces":2}}""") should include(
      "'NoRounding' must hold an empty object")

    // The value encodes and decodes at the type of the family, which is the type a convention
    // held as a field of a larger document is encoded at.
    val decoded: Rounding = decode[Rounding](ExpectedJson)
      .fold(error => fail(s"the expected JSON of this spec does not decode: ${error.getMessage}"), identity)
    decoded shouldBe test
    decoded.asJson shouldBe json(ExpectedJson)
  }
}
