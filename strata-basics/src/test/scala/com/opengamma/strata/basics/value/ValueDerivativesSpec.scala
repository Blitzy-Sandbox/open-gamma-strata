/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.value

import cats.Hash
import cats.Show

import io.circe.Json
import io.circe.parser.decode
import io.circe.parser.parse
import io.circe.syntax._

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.collect.array.DoubleArray

/**
 * Test [[ValueDerivatives]].
 *
 * An instance is a value paired with the derivatives of that value, so `test_of` reads the
 * accessors and the indexed read, `coverage` states which instances are equal and how one
 * renders, and `test_serialization` carries an instance through its JSON form and back.
 *
 * ===How two instances are compared===
 *
 * The value is compared with `java.lang.Double.compare` and hashed with
 * `java.lang.Double.hashCode`, and the derivatives are compared element by element with
 * `java.util.Arrays.equals`. All three canonicalise not-a-number values - every not-a-number
 * counts as one and the same value, whatever payload it carries - and all three keep a
 * negative zero distinct from a positive zero. Both consequences are asserted below, for the
 * value and for an element of the array alike, and each is asserted alongside the operation
 * that produces it rather than alongside the platform comparison of two doubles, which
 * disagrees with this equality on exactly those two inputs and would make the assertions pass
 * for the wrong reason.
 *
 * The arithmetic of the derivative array belongs to `DoubleArray` and is measured in
 * `strata-collect`; here the array is only carried, read and compared.
 */
final class ValueDerivativesSpec extends AnyFunSuite with Matchers {

  /** The value every instance below carries. */
  private val Value: Double = 123.4d

  /** The derivatives every instance below carries. */
  private val Derivatives: DoubleArray = DoubleArray.of(1.0d, 2.0d, 3.0d)

  /**
   * The JSON form of an instance built from the two fixtures above: an object holding the two
   * fields of the type, the value as a JSON number and the derivatives as a JSON array of
   * numbers.
   */
  private val ExpectedJson: String = """{"value":123.4,"derivatives":[1.0,2.0,3.0]}"""

  /**
   * The JSON form of an instance whose value and last derivative are positive infinity, which
   * the JSON grammar has no syntax for and which the single policy of this port for doubles
   * therefore writes as the string `"Infinity"`.
   */
  private val ExpectedNonFiniteJson: String =
    """{"value":"Infinity","derivatives":[1.0,"Infinity"]}"""

  /**
   * Parses one of the expected forms above into the JSON model, so that an encoding is compared
   * with a parsed document rather than with printed text.
   *
   * Comparing documents is what makes the assertion about the encoding rather than about its
   * rendering: whitespace, the ordering the printer happens to use and the spelling of a number
   * are all matters of presentation, while the fields present, their names and their values are
   * the contract. A literal in this file that does not parse is a defect in the spec itself, not
   * a failure of the subject, so it is reported as such.
   *
   * @param text  the JSON text to parse
   * @return the parsed document
   */
  private def json(text: String): Json =
    parse(text).getOrElse(fail(s"the expected JSON of this spec is not itself valid JSON: $text"))

  //-------------------------------------------------------------------------
  test("test_of") {
    val test = ValueDerivatives.of(Value, Derivatives)
    test.value shouldBe Value
    test.derivatives shouldBe Derivatives
    test.getDerivative(0) shouldBe Derivatives.get(0)
    test.getDerivative(1) shouldBe Derivatives.get(1)
    test.getDerivative(2) shouldBe Derivatives.get(2)

    // `getDerivative` reads the array directly, so an index outside it raises the index
    // exception of the runtime, exactly as reading that array would. This is the only exception
    // this spec asserts, because an array index is a caller-contract precondition - the calling
    // code is wrong, and no caller can recover from it - rather than a data-dependent failure of
    // the kind this port hands back as a value. Construction itself cannot fail, so there is no
    // such value here to inspect.
    assertThrows[IndexOutOfBoundsException](test.getDerivative(3))
    assertThrows[IndexOutOfBoundsException](test.getDerivative(-1))
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    val test = ValueDerivatives.of(Value, Derivatives)

    // A second instance built independently from equal parts, so that equality is shown to be
    // structural rather than by reference. `Hash` is the type's single equality-bearing
    // instance and has to agree with the platform equality in both directions, so both are
    // asserted, and the hash of the instance is asserted to be the hash the typeclass reports.
    val same = ValueDerivatives.of(123.4d, DoubleArray.of(1.0d, 2.0d, 3.0d))
    same shouldBe test
    same.hashCode shouldBe test.hashCode
    Hash[ValueDerivatives].eqv(same, test) shouldBe true
    Hash[ValueDerivatives].hash(same) shouldBe test.hashCode

    // Either field differing is enough to make an instance unequal, under the platform equality
    // and under the typeclass alike. The first differs only in the last derivative, which -
    // taken with the equal instance above, whose array is a separate object - is what shows the
    // array participates in equality element by element; the second differs only in the value.
    val otherDerivatives = ValueDerivatives.of(123.4d, DoubleArray.of(1.0d, 2.0d, 4.0d))
    val otherValue = ValueDerivatives.of(0.0d, Derivatives)
    otherDerivatives should not be test
    otherValue should not be test
    Hash[ValueDerivatives].eqv(otherDerivatives, test) shouldBe false
    Hash[ValueDerivatives].eqv(otherValue, test) shouldBe false

    // `Show` renders what `toString` renders, so the two ways of putting an instance into a
    // message agree, and the form is pinned as a literal so that a change to it cannot pass
    // unnoticed.
    Show[ValueDerivatives].show(test) shouldBe test.toString
    test.toString shouldBe "ValueDerivatives{value=123.4, derivatives=[1.0, 2.0, 3.0]}"

    // Nothing about a value paired with its derivatives can be rejected, so this is a total
    // type: the case-class constructor and `copy` are both public and both reachable here,
    // alongside the `of` factory.
    ValueDerivatives(Value, Derivatives) shouldBe test
    test.copy(value = 1.0d).value shouldBe 1.0d
    test.copy(value = 1.0d).derivatives shouldBe Derivatives
    test.copy(derivatives = DoubleArray.of(9.0d)).derivatives shouldBe DoubleArray.of(9.0d)
    test.copy(derivatives = DoubleArray.of(9.0d)).value shouldBe Value

    // Equality compares the value with `java.lang.Double.compare`, which counts every
    // not-a-number value as one and the same value whatever payload it carries, so an instance
    // carrying one equals an independently built instance carrying one. That comparison is what
    // is asserted underneath, rather than the platform comparison of two doubles with `==`,
    // which reports the opposite for this input and would make the assertion pass for the wrong
    // reason.
    ValueDerivatives.of(Double.NaN, Derivatives) shouldBe ValueDerivatives.of(Double.NaN, Derivatives)
    java.lang.Double.compare(Double.NaN, Double.NaN) shouldBe 0
    // The same claim carried through every equality-bearing member of the type, on two instances
    // built independently of each other: the second names its not-a-number as an arithmetic
    // expression rather than as the constant, and holds a fresh array of its own rather than the
    // shared fixture, so the two instances share no object and the assertions are about the
    // equality of this type - which counts every not-a-number as one value and compares the
    // array element by element - rather than about reference identity. The two spellings of
    // not-a-number are interchangeable here precisely because of that first property. `Hash` is
    // asserted alongside the platform equality because it is this type's single
    // equality-bearing instance and the two have to agree, and the hashes are asserted equal
    // because equal instances that hash differently would be broken in every hashed collection.
    val notANumber = ValueDerivatives.of(Double.NaN, Derivatives)
    val sameNotANumber = ValueDerivatives.of(0.0d / 0.0d, DoubleArray.of(1.0d, 2.0d, 3.0d))
    sameNotANumber shouldBe notANumber
    Hash[ValueDerivatives].eqv(sameNotANumber, notANumber) shouldBe true
    sameNotANumber.hashCode shouldBe notANumber.hashCode
    // and the converse, without which the assertions above could be satisfied by an equality
    // that accepts anything: an instance carrying a not-a-number value is unequal to the
    // numeric fixture, under both members alike.
    notANumber should not be test
    Hash[ValueDerivatives].eqv(notANumber, test) shouldBe false

    // The other half of that same comparison: a negative zero stays distinct from a positive
    // zero here, where the platform comparison calls the two equal. `doubleToLongBits` is
    // asserted alongside `compare` because the two agree on the zeroes: it maps each signed
    // zero to a value of its own, while reducing every not-a-number to a single one, which is
    // where the property above comes from.
    ValueDerivatives.of(-0.0d, Derivatives) should not be ValueDerivatives.of(0.0d, Derivatives)
    java.lang.Double.compare(-0.0d, 0.0d) should not be 0
    java.lang.Double.doubleToLongBits(-0.0d) should not be java.lang.Double.doubleToLongBits(0.0d)

    // Both facts hold for a derivative inside the array as well, because the array compares its
    // elements on the same terms - the element-wise comparison of `java.util.Arrays.equals`,
    // which counts every not-a-number as one value and keeps the two zeroes apart, and whose
    // behaviour on these two inputs is asserted underneath.
    ValueDerivatives.of(Value, DoubleArray.of(Double.NaN)) shouldBe
      ValueDerivatives.of(Value, DoubleArray.of(Double.NaN))
    ValueDerivatives.of(Value, DoubleArray.of(-0.0d)) should not be
      ValueDerivatives.of(Value, DoubleArray.of(0.0d))
    java.util.Arrays.equals(Array(Double.NaN), Array(Double.NaN)) shouldBe true
    java.util.Arrays.equals(Array(-0.0d), Array(0.0d)) shouldBe false
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    val test = ValueDerivatives.of(Value, Derivatives)

    // The encoding is the product form, with the two fields of the type under their own names,
    // and decoding it returns an instance equal to the one it came from. Both directions are
    // asserted, because an encoding that is right and a decoding that is wrong would still
    // round trip if only the round trip were checked, and vice versa.
    test.asJson shouldBe json(ExpectedJson)
    decode[ValueDerivatives](test.asJson.noSpaces) shouldBe Right(test)

    // Every double on the path - the scalar field and each array element alike - is written
    // through the single policy of this port for the three values JSON cannot express as a
    // number, so an infinity appears as the string "Infinity" in both positions and is read
    // back as the double that tag names, yielding an instance equal to the one encoded.
    val nonFinite =
      ValueDerivatives.of(Double.PositiveInfinity, DoubleArray.of(1.0d, Double.PositiveInfinity))
    nonFinite.asJson shouldBe json(ExpectedNonFiniteJson)
    decode[ValueDerivatives](nonFinite.asJson.noSpaces) shouldBe Right(nonFinite)
  }

  //-------------------------------------------------------------------------
  test("test_serialization_dropNullsPolicy") {
    // Every product encoder of this port is published through the wrapper that omits a field
    // holding no value, and this type is no exception: neither of its two fields is optional,
    // so no field of an encoded instance holds the literal that denotes an absent value.
    def absentValuedFields(value: ValueDerivatives): List[String] =
      value.asJson.asObject.toList.flatMap(fields =>
        fields.toList.collect { case (fieldName, field) if field.isNull => fieldName })

    absentValuedFields(ValueDerivatives.of(Value, Derivatives)) shouldBe empty
    absentValuedFields(ValueDerivatives.of(Value, DoubleArray.of())) shouldBe empty
    absentValuedFields(ValueDerivatives.of(Double.NaN, DoubleArray.of(Double.NaN))) shouldBe empty

    // The wrapper post-processes the document a derivation produced, so what it publishes is an
    // `Encoder` and cannot be an `Encoder.AsObject`. The positive control is asserted alongside
    // the refusal, which on its own would also hold were there no encoder in scope at all.
    assertCompiles("implicitly[io.circe.Encoder[ValueDerivatives]]")
    assertDoesNotCompile("implicitly[io.circe.Encoder.AsObject[ValueDerivatives]]")
  }
}
