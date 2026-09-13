/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.value

import cats.Hash
import cats.Show

import io.circe.Json
import io.circe.parser.decode
import io.circe.parser.parse
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/**
 * Test [[ValueAdjustment]].
 *
 * An adjustment is a modifying value paired with the type that gives that value its meaning,
 * and each factory test reads one factory: the value it captures, the type it carries, the
 * result it computes from a base value, and the calculation it renders.
 *
 * ===How two adjustments are compared===
 *
 * Equality and hashing compare the modifying value with `java.lang.Double.compare` and
 * `java.lang.Double.hashCode`. Both canonicalise not-a-number values - every not-a-number
 * counts as one and the same value, whatever payload it carries - and both keep a negative
 * zero distinct from a positive zero. The type takes part in equality as well, so two
 * instances carrying the same modifying value under different types are unequal.
 *
 * ===How an adjustment renders===
 *
 * `toString` names the calculation rather than the fields, and it selects the
 * `ValueAdjustment[result = input]` form for a delta amount by comparing the instance against
 * the `NONE` constant by ''value''. A separately built `ofDeltaAmount(0.0)` is therefore that
 * constant and renders as `input`, while `ofDeltaAmount(-0.0)` is a distinct value under the
 * comparison above and renders as the addition `input + -0.0`.
 *
 * ===Tolerance and JSON===
 *
 * The two multiplier cases are asserted within 1e-8 because `100 + 100 * 0.1` and `100 * 1.1`
 * are inexact in binary floating point; every other result here is exact and is asserted
 * exactly. On the JSON path the modifying value is written through the single policy of this
 * port for the three values JSON has no number syntax for, so a not-a-number modifying value
 * appears as the string `"NaN"`.
 */
final class ValueAdjustmentSpec extends AnyFunSuite with Matchers {

  /** The tolerance applied to the two cases below whose arithmetic is inexact. */
  private val Tolerance: Double = 1e-8d

  /** The base value every adjustment below is applied to. */
  private val BaseValue: Double = 100.0d

  /**
   * The JSON form of `ofReplace(200.0)`: an object holding the two fields of the type, the
   * modifying value as a JSON number and the type as the bare string of its canonical name.
   */
  private val ExpectedReplaceJson: String = """{"modifyingValue":200.0,"type":"Replace"}"""

  /** The JSON form of the constant that makes no adjustment, which is a delta amount of zero. */
  private val ExpectedNoneJson: String = """{"modifyingValue":0.0,"type":"DeltaAmount"}"""

  /** The JSON form of `ofMultiplier(1.1)`, the second type carried through the round trip. */
  private val ExpectedMultiplierJson: String = """{"modifyingValue":1.1,"type":"Multiplier"}"""

  /**
   * Parses one of the expected forms above into the JSON model, so that an encoding is
   * compared with a parsed document rather than with printed text.
   *
   * Comparing documents is what makes the assertion about the encoding rather than about its
   * rendering: whitespace, the order a printer happens to emit and the spelling of a number
   * are matters of presentation, while the fields present, their names and their values are
   * the contract. A literal in this file that does not parse is a defect in the spec itself
   * rather than a failure of the subject, so it is reported as such.
   *
   * @param text  the JSON text to parse
   * @return the parsed document
   */
  private def json(text: String): Json =
    parse(text).getOrElse(fail(s"the expected JSON of this spec is not itself valid JSON: $text"))

  //-------------------------------------------------------------------------
  test("test_NONE") {
    val test = ValueAdjustment.NONE
    test.modifyingValue shouldBe 0.0d
    test.`type` shouldBe ValueAdjustmentType.DeltaAmount

    // Adding zero hands the base value back unchanged, so the constant adjusts nothing -
    // which is the whole of its purpose.
    test.adjust(BaseValue) shouldBe 100.0d

    // The rendering names the calculation rather than the addition of zero that performs it.
    test.toString shouldBe "ValueAdjustment[result = input]"
  }

  //-------------------------------------------------------------------------
  test("test_ofReplace") {
    val test = ValueAdjustment.ofReplace(200.0d)
    test.modifyingValue shouldBe 200.0d
    test.`type` shouldBe ValueAdjustmentType.Replace

    // The base value is ignored: the result is the modifying value itself, exactly, so no
    // tolerance is involved here.
    test.adjust(BaseValue) shouldBe 200.0d
    test.toString shouldBe "ValueAdjustment[result = 200.0]"
  }

  //-------------------------------------------------------------------------
  test("test_ofDeltaAmount") {
    val test = ValueAdjustment.ofDeltaAmount(20.0d)
    test.modifyingValue shouldBe 20.0d
    test.`type` shouldBe ValueAdjustmentType.DeltaAmount

    // (100 + 20) is exact in binary floating point, so this is asserted exactly.
    test.adjust(BaseValue) shouldBe 120.0d
    test.toString shouldBe "ValueAdjustment[result = input + 20.0]"
  }

  //-------------------------------------------------------------------------
  test("test_ofDeltaMultiplier") {
    val test = ValueAdjustment.ofDeltaMultiplier(0.1d)
    test.modifyingValue shouldBe 0.1d
    test.`type` shouldBe ValueAdjustmentType.DeltaMultiplier

    // (100 + 100 * 0.1) is not exact - 0.1 has no finite binary expansion - so the result is
    // asserted within the tolerance rather than to the last bit.
    test.adjust(BaseValue) shouldBe (110.0d +- Tolerance)
    test.toString shouldBe "ValueAdjustment[result = input + input * 0.1]"
  }

  //-------------------------------------------------------------------------
  test("test_ofMultiplier") {
    val test = ValueAdjustment.ofMultiplier(1.1d)
    test.modifyingValue shouldBe 1.1d
    test.`type` shouldBe ValueAdjustmentType.Multiplier

    // (100 * 1.1) is inexact for the same reason, and is asserted on the same terms.
    test.adjust(BaseValue) shouldBe (110.0d +- Tolerance)
    test.toString shouldBe "ValueAdjustment[result = input * 1.1]"
  }

  //-------------------------------------------------------------------------
  test("equals") {
    // Four instances: two built independently from the same parts, one differing from them in
    // type alone, and one differing in both fields.
    val a1 = ValueAdjustment.ofReplace(200.0d)
    val a2 = ValueAdjustment.ofReplace(200.0d)
    val b = ValueAdjustment.ofDeltaMultiplier(200.0d)
    val c = ValueAdjustment.ofDeltaMultiplier(0.1d)

    // Equality is structural, so two separately built instances holding equal parts are equal
    // and an instance differing in either field is not.
    (a1 == a2) shouldBe true
    (a1 == b) shouldBe false
    (a1 == c) shouldBe false

    // `b` differs from `a1` in type alone - both carry 200.0 - which is what shows the type
    // participates in equality rather than being implied by the value.
    a1.modifyingValue shouldBe b.modifyingValue
    a1.`type` should not be b.`type`

    // Equal instances hash equally.
    a1.hashCode shouldBe a2.hashCode

    // `Hash` is the type's single equality-bearing instance - it extends `Eq`, so summoning
    // either yields this one value - and it has to report exactly what the platform equality
    // above reports, in both directions, or two notions of equality would disagree about the
    // same pair.
    Hash[ValueAdjustment].eqv(a1, a2) shouldBe true
    Hash[ValueAdjustment].eqv(a1, b) shouldBe false
    Hash[ValueAdjustment].eqv(a1, c) shouldBe false
    Hash[ValueAdjustment].hash(a1) shouldBe a1.hashCode

    // An object of another type is never equal, which is the remaining branch of the type's
    // own `equals`. It is asserted by calling that method rather than by writing
    // `a1 == ValueAdjustmentType.Replace`, which does not compile: the compiler rejects a
    // comparison between these two types as one that can only ever be false - true, and
    // precisely the branch under test here.
    a1.equals(ValueAdjustmentType.Replace) shouldBe false

    // Equality is reflexive, for the instance and through the typeclass alike.
    (a1 == a1) shouldBe true
    Hash[ValueAdjustment].eqv(a1, a1) shouldBe true
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // Two distinct instances, covering two of the four types, so that every claim below is
    // made about more than a single shape of value.
    val a1 = ValueAdjustment.ofReplace(200.0d)
    val b = ValueAdjustment.ofDeltaMultiplier(0.1d)

    // Each accessor hands back the part the instance was built from.
    a1.modifyingValue shouldBe 200.0d
    a1.`type` shouldBe ValueAdjustmentType.Replace
    b.modifyingValue shouldBe 0.1d
    b.`type` shouldBe ValueAdjustmentType.DeltaMultiplier

    // Instances rebuilt from equal parts are equal and hash equally, and the two distinct
    // instances are equal to neither each other nor a rebuild of the other.
    val a1Rebuilt = ValueAdjustment.ofReplace(200.0d)
    val bRebuilt = ValueAdjustment.ofDeltaMultiplier(0.1d)
    a1Rebuilt shouldBe a1
    bRebuilt shouldBe b
    Hash[ValueAdjustment].hash(a1Rebuilt) shouldBe Hash[ValueAdjustment].hash(a1)
    Hash[ValueAdjustment].hash(bRebuilt) shouldBe Hash[ValueAdjustment].hash(b)
    a1 should not be b
    Hash[ValueAdjustment].eqv(a1, b) shouldBe false

    // `Show` renders what `toString` renders, so the two ways of putting an adjustment into a
    // message agree, and both forms are pinned as literals so that a change to either cannot
    // pass unnoticed.
    Show[ValueAdjustment].show(a1) shouldBe a1.toString
    Show[ValueAdjustment].show(b) shouldBe b.toString
    a1.toString shouldBe "ValueAdjustment[result = 200.0]"
    b.toString shouldBe "ValueAdjustment[result = input + input * 0.1]"

    // Nothing about a modifying value paired with a type can be rejected, so this is a total
    // type: the case-class constructor and `copy` are both public and both reachable here,
    // alongside the four named factories.
    ValueAdjustment(20.0d, ValueAdjustmentType.DeltaAmount) shouldBe
      ValueAdjustment.ofDeltaAmount(20.0d)
    ValueAdjustment(200.0d, ValueAdjustmentType.Replace) shouldBe a1
    a1.copy(modifyingValue = 300.0d).modifyingValue shouldBe 300.0d
    a1.copy(modifyingValue = 300.0d).`type` shouldBe ValueAdjustmentType.Replace
    a1.copy(`type` = ValueAdjustmentType.Multiplier).`type` shouldBe ValueAdjustmentType.Multiplier
    a1.copy(`type` = ValueAdjustmentType.Multiplier).modifyingValue shouldBe 200.0d

    // Equality compares the modifying value with `java.lang.Double.compare`, which counts
    // every not-a-number value as one and the same value whatever payload it carries, so an
    // instance carrying one equals an independently built instance carrying one, and equals
    // itself. That comparison is asserted underneath the equality rather than the platform
    // comparison of two doubles, which reports the opposite for this input and would make the
    // assertion pass for the wrong reason.
    // The rebuilt instance is what makes this claim more than a statement about references:
    // the type's `equals` answers a comparison against the same object without looking at any
    // field, so a not-a-number value asserted only against itself would say nothing about how
    // the field is compared.
    val notANumber = ValueAdjustment.ofDeltaAmount(Double.NaN)
    val notANumberRebuilt = ValueAdjustment.ofDeltaAmount(Double.NaN)
    notANumber shouldBe notANumberRebuilt
    notANumber shouldBe notANumber
    Hash[ValueAdjustment].eqv(notANumber, notANumberRebuilt) shouldBe true
    Hash[ValueAdjustment].hash(notANumber) shouldBe Hash[ValueAdjustment].hash(notANumberRebuilt)
    java.lang.Double.compare(Double.NaN, Double.NaN) shouldBe 0

    // The other half of that same comparison: a negative zero stays distinct from a positive
    // zero here, where the platform comparison of the raw doubles calls the two equal.
    // `Double.compare` is asserted rather than that comparison, for the same reason as above.
    ValueAdjustment.ofDeltaAmount(-0.0d) should not be ValueAdjustment.ofDeltaAmount(0.0d)
    Hash[ValueAdjustment]
      .eqv(ValueAdjustment.ofDeltaAmount(-0.0d), ValueAdjustment.ofDeltaAmount(0.0d)) shouldBe false
    java.lang.Double.compare(-0.0d, 0.0d) should not be 0

    // Those two facts fix how the constant is recognised. `toString` selects the `input`
    // rendering for a delta amount by comparing the instance against `NONE` by value, so a
    // separately built positive zero delta amount is that constant and renders like it, while
    // a negative zero is a distinct value under the comparison above and so renders as an
    // addition.
    ValueAdjustment.ofDeltaAmount(0.0d) shouldBe ValueAdjustment.NONE
    ValueAdjustment.ofDeltaAmount(0.0d).toString shouldBe "ValueAdjustment[result = input]"
    ValueAdjustment.ofDeltaAmount(-0.0d) should not be ValueAdjustment.NONE
    ValueAdjustment.ofDeltaAmount(-0.0d).toString shouldBe "ValueAdjustment[result = input + -0.0]"
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    val test = ValueAdjustment.ofReplace(200.0d)

    // The encoding is the product form, carrying the two fields of the type under their own
    // names, and decoding it returns an instance equal to the one it came from. Both
    // directions are asserted, because an encoding that is wrong and a decoding that is wrong
    // in the same way would still round trip if only the round trip were checked.
    test.asJson shouldBe json(ExpectedReplaceJson)
    decode[ValueAdjustment](test.asJson.noSpaces) shouldBe Right(test)

    // The two keys, in declaration order. The second field is spelled in backticks in the
    // source because the word is reserved by the language, which changes nothing about the
    // name: the key of the JSON form is `type`, and this is what says so.
    test.asJson.asObject.map(_.keys.toList) shouldBe Some(List("modifyingValue", "type"))

    // The modifying value is a JSON number and the type a bare string, rather than either
    // being wrapped in an object naming its constructor, which keeps a serialized adjustment
    // readable.
    test.asJson.hcursor.downField("modifyingValue").as[Double] shouldBe Right(200.0d)
    test.asJson.hcursor.downField("type").as[String] shouldBe Right("Replace")

    // The constant that makes no adjustment carries no special form: it encodes as the delta
    // amount of zero that it is, and decodes back to a value equal to the constant - equal,
    // not identical, which is the case the value-based comparison of `toString` is written
    // for.
    ValueAdjustment.NONE.asJson shouldBe json(ExpectedNoneJson)
    decode[ValueAdjustment](ValueAdjustment.NONE.asJson.noSpaces) shouldBe Right(ValueAdjustment.NONE)
    decode[ValueAdjustment](ExpectedNoneJson).map(_.toString) shouldBe
      Right("ValueAdjustment[result = input]")

    // A second type, to show that the type field round trips as more than one value.
    val multiplier = ValueAdjustment.ofMultiplier(1.1d)
    multiplier.asJson shouldBe json(ExpectedMultiplierJson)
    decode[ValueAdjustment](multiplier.asJson.noSpaces) shouldBe Right(multiplier)

    // Every double on the path is written through the single policy of this port for the three
    // values JSON cannot express as a number, so a not-a-number modifying value appears as the
    // string "NaN" and decodes back to an equal instance - equal because the equality of this
    // type counts every not-a-number as one and the same value.
    val notANumber = ValueAdjustment.ofDeltaAmount(Double.NaN)
    notANumber.asJson shouldBe json("""{"modifyingValue":"NaN","type":"DeltaAmount"}""")
    decode[ValueAdjustment](notANumber.asJson.noSpaces) shouldBe Right(notANumber)
  }

  //-------------------------------------------------------------------------
  test("test_serialization_dropNullsPolicy") {
    // Every product encoder of this port is published through the wrapper that omits a field
    // holding no value, and this type is no exception: neither of its two fields is optional,
    // so no field of an encoded adjustment holds the literal that denotes an absent value.
    def absentValuedFields(value: ValueAdjustment): List[String] =
      value.asJson.asObject.toList.flatMap(fields =>
        fields.toList.collect { case (fieldName, field) if field.isNull => fieldName })

    absentValuedFields(ValueAdjustment.ofReplace(200.0d)) shouldBe empty
    absentValuedFields(ValueAdjustment.NONE) shouldBe empty
    absentValuedFields(ValueAdjustment.ofMultiplier(1.1d)) shouldBe empty
    absentValuedFields(ValueAdjustment.ofDeltaAmount(Double.NaN)) shouldBe empty

    // The wrapper post-processes the document a derivation produced, so what it publishes is an
    // `Encoder` and cannot be an `Encoder.AsObject`. The positive control is asserted alongside
    // the refusal, which on its own would also hold were there no encoder in scope at all.
    assertCompiles("implicitly[io.circe.Encoder[ValueAdjustment]]")
    assertDoesNotCompile("implicitly[io.circe.Encoder.AsObject[ValueAdjustment]]")
  }
}
