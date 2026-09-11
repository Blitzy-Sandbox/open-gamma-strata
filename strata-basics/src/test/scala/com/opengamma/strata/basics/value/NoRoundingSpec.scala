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
 * Test [[NoRounding]].
 *
 * The Java original held six test methods and this spec holds the same six, under the same
 * names and in the same order, because the migration is traced method by method: `test_none`
 * reads the factory and the rendering, the three `round_*` methods read one overload each,
 * `coverage` stands in for a reflective sweep, and `test_serialization` asserts that the
 * convention survives a round trip.
 *
 * Four of the six assert exactly what the original asserted, against the same input - the
 * value `1.23` in each of the three representations - and the same expected values. Two could
 * not, and what replaced them is worth stating once here rather than inside each test.
 *
 * The Java `coverage` method called `coverImmutableBean`, which walked the properties of a
 * Joda-Beans bean through its meta-bean and compared instances rebuilt from them. This port
 * has no meta-bean and no reflective property access, so the call has no target. Its substance
 * does: for a bean with no property at all - which is what the Java class was - it stood for
 * the claims that the instance is the single instance of its kind, that it is equal to itself
 * and to nothing else, and that it renders itself faithfully. Those claims are asserted here
 * directly, on the singleton, on the family's two typeclass instances, and against the other
 * member of the family, which says more than the sweep did because it names the expected
 * outcome of each case instead of merely visiting fields that did not exist.
 *
 * The Java `test_serialization` method asserted a Java-serialization round trip. Java
 * serialization is not part of this port at all, and the JSON codec of [[Rounding]] takes its
 * place, so the round trip asserted here is `decode(encode(x)) == x`, together with the exact
 * shape of the encoding - the wrapper object a closed family takes throughout this port.
 *
 * ===Why the three overloads are three tests===
 *
 * In the Java interface the double and decimal overloads were default methods expressed in
 * terms of the abstract `BigDecimal` one, and this class overrode all three to hand the value
 * straight back. The port keeps that arrangement: [[Rounding]] states the double overload once
 * for the family, [[NoRounding]] overrides all three, and each remains separately observable.
 * Asserting each by itself therefore asserts that arrangement, which is why all three names
 * survive as their own tests rather than as one table.
 *
 * ===What this spec does not do===
 *
 * The property-based round trip of every codec-bearing type, the typeclass law suites and the
 * proof that no rounding convention can be declared outside this family belong to the specs
 * that own them. The other member of the family, the half-up convention, has its own spec.
 * This spec is the six ported methods and nothing beyond them.
 */
final class NoRoundingSpec extends AnyFunSuite with Matchers {

  /** The value the Java test rounded, in each of the three representations below. */
  private val Input: Double = 1.23d

  /** The rendering of the convention, which is the text of the Java original. */
  private val ExpectedText: String = "No rounding"

  /**
   * The JSON form of the convention: an object of one field, whose name is the member and
   * whose value holds that member's fields, of which this member has none.
   */
  private val ExpectedJson: String = """{"NoRounding":{}}"""

  /**
   * Parses the expected form above into the JSON model, so that an encoding is compared with a
   * parsed document rather than with printed text.
   *
   * Comparing documents is what makes the assertion about the encoding rather than about its
   * rendering: whitespace and the order a printer happens to emit are matters of presentation,
   * while the fields present and their names are the contract. A literal in this file that does
   * not parse is a defect in the spec itself rather than a failure of the subject, so it is
   * reported as such.
   *
   * @param text  the JSON text to parse
   * @return the parsed document
   */
  private def json(text: String): Json =
    parse(text).getOrElse(fail(s"the expected JSON of this spec is not itself valid JSON: $text"))

  /**
   * Builds the decimal fixture the decimal overload is asserted against.
   *
   * The Java factory was total; this port reports the values that are not decimals - a value
   * that is not finite, or one needing more digits than a decimal holds - instead of throwing,
   * so the outcome is consumed here rather than assumed. The fixture below is a decimal, so
   * the failure branch is unreachable for it; it is written out all the same, because a fixture
   * that stopped being valid has to name itself rather than surface later as an unrelated
   * assertion failure.
   *
   * @param value  the value to build a decimal from
   * @return the decimal of that value
   */
  private def decimal(value: Double): Decimal =
    Decimal
      .of(value)
      .fold(
        failure => fail(s"the expected decimal of this spec is not itself a decimal: ${failure.message}"),
        identity)

  /**
   * Builds the half-up fixture the convention is compared against in `coverage`.
   *
   * The factory reports every reason its input describes no convention, so the outcome is
   * consumed the same way as the decimal above: a number of decimal places in range yields the
   * convention, and one out of range names itself here rather than later.
   *
   * @param decimalPlaces  the number of decimal places to round to
   * @return the half-up convention rounding to that many decimal places
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

    // The rendering is the text of the Java original, to the character.
    test.toString shouldBe ExpectedText

    // The Java test asserted that a second call to the factory produces something equal to the
    // first. It does, and for a stronger reason in this port as in the original: the factory
    // hands back a singleton, so the two are the same object and not merely equal values.
    Rounding.none shouldBe test
    (Rounding.none eq test) shouldBe true

    // The value the factory returns is the member of the family that makes no change. The Java
    // interface hid its implementation class, so its test could not say this; here the member
    // is named, which is what lets a call site state the convention it holds and a pattern
    // match read it - see `coverage` below.
    test shouldBe NoRounding

    // `Show` renders what `toString` renders, so the two ways of putting the convention into a
    // message agree.
    Show[Rounding].show(test) shouldBe ExpectedText
  }

  //-------------------------------------------------------------------------
  test("round_double") {
    Rounding.none.round(Input) shouldBe 1.23d

    // This convention states the double overload itself rather than inheriting the trip through
    // a decimal and back that the family declares, exactly as the Java class did. That is what
    // keeps it exact for a value no decimal names: the three values JSON and `BigDecimal` alike
    // cannot express are handed straight back instead of failing on the way through.
    Rounding.none.round(Double.NaN).isNaN shouldBe true
    Rounding.none.round(Double.PositiveInfinity) shouldBe Double.PositiveInfinity
    Rounding.none.round(Double.NegativeInfinity) shouldBe Double.NegativeInfinity

    // The same exactness, bit for bit, for the negative zero that an equality of doubles cannot
    // tell from the positive one; the raw bits are compared because the comparison of the two
    // doubles would hold whether or not the sign survived.
    java.lang.Double.doubleToRawLongBits(Rounding.none.round(-0.0d)) shouldBe
      java.lang.Double.doubleToRawLongBits(-0.0d)
  }

  //-------------------------------------------------------------------------
  test("round_BigDecimal") {
    Rounding.none.round(BigDecimal.valueOf(Input)) shouldBe BigDecimal.valueOf(Input)

    // The comparison above is the `equals` of `BigDecimal`, which distinguishes the scale, so it
    // says the scale is preserved as well as the number - the assertion of the Java test, whose
    // `isEqualTo` compared the same way. A comparison by `compareTo` would hold for the value at
    // any scale and would therefore say less. These two assertions name the scale that is being
    // preserved, so the claim does not rest on the reader knowing what `BigDecimal.valueOf`
    // produces.
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

    // As with the `BigDecimal` overload, the value handed back is the object handed in. This
    // overload is stated by each member of the family rather than inherited, because the Java
    // default built its answer through a factory that this port reports failures from; for this
    // member the identity is what that statement amounts to.
    val value = decimal(Input)
    (Rounding.none.round(value) eq value) shouldBe true
    Rounding.none.round(value).toString shouldBe "1.23"
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    val test: Rounding = Rounding.none

    // The convention is a singleton: every route to it - the factory, the alias on the
    // companion, and the member named directly - reaches one object, so its equality is the
    // identity the Java class approximated with a test against its own class.
    (test eq NoRounding) shouldBe true
    (Rounding.NoRounding eq NoRounding) shouldBe true

    // It is equal to itself and hashes equally under the family's single equality-bearing
    // instance, from which `Eq[Rounding]` is obtained by subtyping.
    Hash[Rounding].eqv(test, NoRounding) shouldBe true
    Hash[Rounding].hash(test) shouldBe Hash[Rounding].hash(NoRounding)

    // It is equal to nothing else: the other member of the family is a different convention,
    // whichever number of decimal places it rounds to, and both notions of equality say so.
    val halfUp = halfUpRounding(2)
    test should not be halfUp
    Hash[Rounding].eqv(test, halfUp) shouldBe false
    halfUp should not be test

    // Both members render themselves faithfully, and both do it the same way through `Show` as
    // through `toString`, which is the claim the sweep made about rendering.
    Show[Rounding].show(test) shouldBe test.toString
    Show[Rounding].show(halfUp) shouldBe halfUp.toString
    test.toString shouldBe ExpectedText
    halfUp.toString shouldBe "Round to 2dp"

    // The family is closed, so the two members above are all there are: this match names both
    // and no default branch, and the compiler checks that the pair is exhaustive - a build
    // that warns is a build that fails here, so the check is an assertion about the family and
    // not merely about this value. The value it selects is asserted, which is also what keeps
    // the match from being a discarded expression.
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
    // came from. Both directions are asserted, because an encoding that is wrong and a decoding
    // that is wrong in the same way would still round trip if only the round trip were checked.
    test.asJson shouldBe json(ExpectedJson)
    decode[Rounding](ExpectedJson) shouldBe Right(NoRounding)
    decode[Rounding](test.asJson.noSpaces) shouldBe Right(test)

    // The shape spelled out: one field, named for the member, over an object holding nothing.
    test.asJson.asObject.map(_.keys.toList) shouldBe Some(List("NoRounding"))
    test.asJson.hcursor.downField("NoRounding").as[Json] shouldBe Right(Json.obj())

    // The value is encoded through the type of the family rather than through the type of the
    // member, which is how a convention held as a field of a larger document is encoded, and it
    // decodes back at that same type. That is what makes the round trip above a statement about
    // the codec of [[Rounding]] and not about an instance particular to this member.
    val decoded: Rounding = decode[Rounding](ExpectedJson)
      .fold(error => fail(s"the expected JSON of this spec does not decode: ${error.getMessage}"), identity)
    decoded shouldBe test
    decoded.asJson shouldBe json(ExpectedJson)
  }
}
