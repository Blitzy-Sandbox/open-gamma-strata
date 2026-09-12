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
 * The Java original held eight test methods and this spec holds the same eight, under the
 * same names and in the same order, because the migration is traced method by method: the
 * five `test_*` methods read one factory each, `equals` states which instances are equal,
 * `coverage` stands in for a reflective sweep, and `test_serialization` asserts that an
 * instance survives a round trip. The name of the sixth is `equals` rather than
 * `test_equals`, which is the name the Java method carried; it is a test name here and not a
 * method, so it overrides nothing.
 *
 * Six of the eight assert exactly what the original asserted, against the same inputs and
 * the same expected values. Two could not, and what replaced them is worth stating once here
 * rather than in each test.
 *
 * The Java `coverage` method called `coverImmutableBean`, which walked the properties of a
 * Joda-Beans bean through its meta-bean and compared instances rebuilt from them. This port
 * has no meta-bean and no reflective property access, so the call has no target. Its
 * substance does: it stood for the claims that an instance exposes the parts it was built
 * from, that instances built independently from equal parts are equal, that instances
 * differing in any field are not, and that an instance renders itself faithfully. Those
 * claims are asserted here directly, on the type's own members and on its two typeclass
 * instances, which says more than the sweep did because it names the expected outcome of
 * each case instead of merely visiting the fields.
 *
 * The Java `test_serialization` method asserted a Java-serialization round trip. Java
 * serialization is not part of this port at all, and the JSON codec derived when
 * [[ValueAdjustment]] is compiled takes its place, so the round trip asserted here is
 * `decode(encode(x)) == x`, together with the exact shape of the encoding.
 *
 * ===Where this port renders an adjustment differently===
 *
 * The Java `toString` chose between `input` and `input + 0.0` for a delta amount of zero by
 * comparing the instance against the `NONE` constant by ''reference''. This port compares by
 * ''value'', so a separately built zero delta amount renders as `input` where the original
 * rendered it as an addition. The divergence is deliberate, is documented on the type
 * itself, and is asserted below as the behaviour of this port - never as the behaviour of
 * the original, which no test of the original covered.
 *
 * ===What this spec does not do===
 *
 * The tolerance on the two multiplier cases is the one the Java test chose, because the
 * arithmetic it covers is not exact in binary floating point; it is not a statement of
 * numerical agreement with the original. That agreement is measured to a tighter bound, over
 * captured Java values, by the parity fixtures, and the property-based round trip of every
 * codec-bearing type and the typeclass law suites likewise belong to the specs that own
 * them. This spec is the eight ported methods, together with one test that pins the codec
 * policy the encoder of this type is published under, and nothing beyond them.
 */
final class ValueAdjustmentSpec extends AnyFunSuite with Matchers {

  /**
   * The tolerance the Java test applied to the two cases whose arithmetic is inexact,
   * transcribed from its `within(1e-8d)` offset.
   */
  private val Tolerance: Double = 1e-8d

  /** The base value every adjustment below is applied to, as in the Java test. */
  private val BaseValue: Double = 100.0d

  /**
   * The JSON form of `ofReplace(200.0)`: an object holding the two field names the Java bean
   * declared, the modifying value as a JSON number and the type as the bare string of its
   * canonical name.
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

    // (100 + 20) is exact in binary floating point, so this is asserted exactly, as the Java
    // test asserted it.
    test.adjust(BaseValue) shouldBe 120.0d
    test.toString shouldBe "ValueAdjustment[result = input + 20.0]"
  }

  //-------------------------------------------------------------------------
  test("test_ofDeltaMultiplier") {
    val test = ValueAdjustment.ofDeltaMultiplier(0.1d)
    test.modifyingValue shouldBe 0.1d
    test.`type` shouldBe ValueAdjustmentType.DeltaMultiplier

    // (100 + 100 * 0.1) is not exact - 0.1 has no finite binary expansion - so the result is
    // asserted within the tolerance the Java test chose for precisely this reason. The shape
    // of the arithmetic is the shape the original used, which is what keeps the last bits
    // agreeing; the tolerance here admits the ones that shape produces.
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
    // The four instances of the Java test, unchanged: two built independently from the same
    // parts, one differing from them in type alone, and one differing in both fields.
    val a1 = ValueAdjustment.ofReplace(200.0d)
    val a2 = ValueAdjustment.ofReplace(200.0d)
    val b = ValueAdjustment.ofDeltaMultiplier(200.0d)
    val c = ValueAdjustment.ofDeltaMultiplier(0.1d)

    // The three facts the Java method asserted, in its own form: equality is structural, so
    // two separately built instances holding equal parts are equal, and an instance differing
    // in either field is not.
    (a1 == a2) shouldBe true
    (a1 == b) shouldBe false
    (a1 == c) shouldBe false

    // `b` differs from `a1` in type alone - both carry 200.0 - which is what shows the type
    // participates in equality rather than being implied by the value.
    a1.modifyingValue shouldBe b.modifyingValue
    a1.`type` should not be b.`type`

    // Equal instances hash equally. The bean this replaces seeded its hash with the identity
    // hash of the class, so this was true within a run; the port seeds it with a constant, so
    // it is true across runs too, and the assertion is the same either way.
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
    // own `equals`. It is asserted by calling that method rather than by writing a comparison
    // between the two types, which the compiler would reject as one that can only ever be
    // false - true, and precisely the branch under test here.
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

    // Each accessor hands back the part the instance was built from. This is what the
    // reflective property walk of the Java sweep was establishing, stated directly.
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
    // alongside the four factories that the ported call sites read through.
    ValueAdjustment(20.0d, ValueAdjustmentType.DeltaAmount) shouldBe
      ValueAdjustment.ofDeltaAmount(20.0d)
    ValueAdjustment(200.0d, ValueAdjustmentType.Replace) shouldBe a1
    a1.copy(modifyingValue = 300.0d).modifyingValue shouldBe 300.0d
    a1.copy(modifyingValue = 300.0d).`type` shouldBe ValueAdjustmentType.Replace
    a1.copy(`type` = ValueAdjustmentType.Multiplier).`type` shouldBe ValueAdjustmentType.Multiplier
    a1.copy(`type` = ValueAdjustmentType.Multiplier).modifyingValue shouldBe 200.0d

    // Equality compares the modifying value by its bit pattern - the comparison Joda-Beans
    // used and that every double-bearing type of this port preserves - so a not-a-number
    // value is equal to itself and an instance carrying one equals an equal instance, and is
    // equal to itself. The justification is the bit-level comparison underneath, not the
    // platform comparison of two doubles, which reports the opposite for this input and would
    // make the assertion pass for the wrong reason.
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

    // The other half of that same equality: the two zeroes have different bit patterns, so
    // they are different values here, where the platform comparison of the raw doubles calls
    // them equal. The bit-level fact is asserted rather than that comparison, for the same
    // reason as above.
    ValueAdjustment.ofDeltaAmount(-0.0d) should not be ValueAdjustment.ofDeltaAmount(0.0d)
    Hash[ValueAdjustment]
      .eqv(ValueAdjustment.ofDeltaAmount(-0.0d), ValueAdjustment.ofDeltaAmount(0.0d)) shouldBe false
    java.lang.Double.compare(-0.0d, 0.0d) should not be 0

    // Those two facts fix how the constant is recognised, and here this port diverges from
    // the original in one observable detail. The original selected the `input` rendering by
    // comparing against `NONE` by reference, so a separately built zero delta amount rendered
    // as an addition; this port compares by value, so it renders as `input`. What follows is
    // the behaviour of this port, asserted as such: a separately built positive zero is the
    // constant and renders like it, while a negative zero is a different value under the
    // bit-pattern equality above and so still renders as an addition.
    ValueAdjustment.ofDeltaAmount(0.0d) shouldBe ValueAdjustment.NONE
    ValueAdjustment.ofDeltaAmount(0.0d).toString shouldBe "ValueAdjustment[result = input]"
    ValueAdjustment.ofDeltaAmount(-0.0d) should not be ValueAdjustment.NONE
    ValueAdjustment.ofDeltaAmount(-0.0d).toString shouldBe "ValueAdjustment[result = input + -0.0]"
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    val test = ValueAdjustment.ofReplace(200.0d)

    // The encoding is the product form, carrying the two field names the Java bean declared,
    // and decoding it returns an instance equal to the one it came from. Both directions are
    // asserted, because an encoding that is wrong and a decoding that is wrong in the same way
    // would still round trip if only the round trip were checked.
    test.asJson shouldBe json(ExpectedReplaceJson)
    decode[ValueAdjustment](test.asJson.noSpaces) shouldBe Right(test)

    // The two keys, in declaration order. The second field is spelled in backticks in the
    // source because the word is reserved by the language, which changes nothing about the
    // name: the key of the JSON form is `type`, as the Java property was, and this is what
    // says so.
    test.asJson.asObject.map(_.keys.toList) shouldBe Some(List("modifyingValue", "type"))

    // The modifying value is a JSON number and the type a bare string, rather than either
    // being wrapped in an object naming its constructor. That keeps a serialized adjustment
    // readable and is the form the type being ported wrote for its type field.
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
    // string "NaN" and decodes back to an equal instance - equal because the bit-pattern
    // equality of this type makes such a value equal to itself.
    val notANumber = ValueAdjustment.ofDeltaAmount(Double.NaN)
    notANumber.asJson shouldBe json("""{"modifyingValue":"NaN","type":"DeltaAmount"}""")
    decode[ValueAdjustment](notANumber.asJson.noSpaces) shouldBe Right(notANumber)
  }

  //-------------------------------------------------------------------------
  test("test_serialization_dropNullsPolicy") {
    // Every product encoder of this port is published through the wrapper that omits a field
    // holding no value, and this type is no exception to that policy. Neither of its two fields
    // is optional, so the wrapper removes nothing from these documents; what is asserted is that
    // no field of an encoded adjustment holds the literal that denotes an absent value, which is
    // the observable half of the policy and would begin to fail were a field ever to write one.
    def absentValuedFields(value: ValueAdjustment): List[String] =
      value.asJson.asObject.toList.flatMap(fields =>
        fields.toList.collect { case (fieldName, field) if field.isNull => fieldName })

    absentValuedFields(ValueAdjustment.ofReplace(200.0d)) shouldBe empty
    absentValuedFields(ValueAdjustment.NONE) shouldBe empty
    absentValuedFields(ValueAdjustment.ofMultiplier(1.1d)) shouldBe empty
    absentValuedFields(ValueAdjustment.ofDeltaAmount(Double.NaN)) shouldBe empty

    // The other half is which instance is published, and the two assertions below state it
    // exactly. The wrapper post-processes the document a derivation produced, so what it returns
    // is an `Encoder` and cannot be an `Encoder.AsObject`: the encoder in implicit scope being
    // an `Encoder` and not an `Encoder.AsObject` is therefore the statement that the derivation
    // is published through the wrapper rather than directly. The positive control is asserted
    // alongside the refusal, because a refusal on its own would also be satisfied by there being
    // no encoder in scope at all.
    assertCompiles("implicitly[io.circe.Encoder[ValueAdjustment]]")
    assertDoesNotCompile("implicitly[io.circe.Encoder.AsObject[ValueAdjustment]]")
  }
}
