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
 * The Java original held three methods and this spec holds the same three, under the same
 * names, because the migration is traced method by method: `test_of` reads the accessors,
 * `coverage` exercises equality and rendering, and `test_serialization` asserts that an
 * instance survives a round trip. What each of the last two has to assert changed, and the
 * reason is worth stating once here rather than in each test.
 *
 * The Java `coverage` method called three reflective helpers - `coverImmutableBean`,
 * `coverBeanEquals` and a check that the meta-bean existed - which walked the properties of a
 * Joda-Beans bean through its meta-bean and compared instances built from them. This port has
 * no meta-bean and no reflective property access, so none of the three has a target. Their
 * substance does: they stood for the claims that two instances built independently from equal
 * parts are equal, that instances differing in any field are not, and that an instance renders
 * itself faithfully. Those claims are asserted here directly, on the type's own members and on
 * its typeclass instances, which is a stronger statement than the sweep was making because it
 * names the expected outcome of each case instead of merely visiting the fields.
 *
 * The Java `test_serialization` method asserted a Java-serialization round trip. Java
 * serialization is not part of this port at all, and the JSON codec derived when
 * [[ValueDerivatives]] is compiled takes its place, so the round trip asserted here is
 * `decode(encode(x)) == x`, together with the exact shape of the encoding - both the ordinary
 * form and the form a value that JSON cannot write as a number takes.
 *
 * ===Why the equality assertions are written bit for bit===
 *
 * This type carries a double, and every double-bearing type of this port compares that double
 * by its bit pattern rather than by the numeric comparison of the platform, which is what the
 * bean equality it replaces did. Two consequences follow, and both are asserted below: a
 * not-a-number value is equal to itself, so an instance carrying one equals an equal instance,
 * and a negative zero is distinct from a positive zero. Each is justified by the bit-level fact
 * underneath it - `java.lang.Double.compare`, `java.lang.Double.doubleToLongBits` and, for the
 * array, `java.util.Arrays.equals` - rather than by the platform comparison, because the
 * platform comparison disagrees with this equality on exactly these two inputs and asserting
 * with it would pass for the wrong reason.
 *
 * ===What this spec does not do===
 *
 * It adds no tolerance-based numeric comparison. The arithmetic of the derivative array belongs
 * to `DoubleArray`, whose own spec and parity fixture in `strata-collect` measure it against the
 * Java baseline; here the array is only carried, read and compared.
 */
final class ValueDerivativesSpec extends AnyFunSuite with Matchers {

  /** The value every instance below carries, transcribed from the Java test fixture. */
  private val Value: Double = 123.4d

  /** The derivatives every instance below carries, transcribed from the Java test fixture. */
  private val Derivatives: DoubleArray = DoubleArray.of(1.0d, 2.0d, 3.0d)

  /**
   * The JSON form of an instance built from the two fixtures above: an object holding the two
   * field names the Java bean declared, the value as a JSON number and the derivatives as a
   * JSON array of numbers.
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

    // `getDerivative` reads the array directly, so an index outside it fails with the index
    // exception of the runtime, exactly as the Java accessor documented and exactly as reading
    // that array would. This is the only exception this spec asserts, and it is asserted
    // because an array index is a caller-contract precondition - the calling code is wrong, and
    // no caller can recover from it - rather than a data-dependent failure of the kind this
    // port hands back as a value. Construction itself cannot fail, so there is no such value
    // here to inspect.
    assertThrows[IndexOutOfBoundsException](test.getDerivative(3))
    assertThrows[IndexOutOfBoundsException](test.getDerivative(-1))
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    val test = ValueDerivatives.of(Value, Derivatives)

    // Built independently from equal parts, as the Java second instance was, so that equality
    // is shown to be structural rather than by reference. `Hash` is the type's single
    // equality-bearing instance and has to agree with the platform equality in both directions,
    // so both are asserted, and the hash of the instance is asserted to be the hash the
    // typeclass reports.
    val same = ValueDerivatives.of(123.4d, DoubleArray.of(1.0d, 2.0d, 3.0d))
    same shouldBe test
    same.hashCode shouldBe test.hashCode
    Hash[ValueDerivatives].eqv(same, test) shouldBe true
    Hash[ValueDerivatives].hash(same) shouldBe test.hashCode

    // Either field differing is enough to make an instance unequal, under the platform equality
    // and under the typeclass alike. The first differs only in the last derivative, which is
    // what proves the array participates in equality element by element rather than by
    // reference; the second differs only in the value.
    val otherDerivatives = ValueDerivatives.of(123.4d, DoubleArray.of(1.0d, 2.0d, 4.0d))
    val otherValue = ValueDerivatives.of(0.0d, Derivatives)
    otherDerivatives should not be test
    otherValue should not be test
    Hash[ValueDerivatives].eqv(otherDerivatives, test) shouldBe false
    Hash[ValueDerivatives].eqv(otherValue, test) shouldBe false

    // `Show` renders what `toString` renders, and what `toString` renders is the form the Java
    // bean produced, pinned here as a literal so that a change to it cannot pass unnoticed.
    Show[ValueDerivatives].show(test) shouldBe test.toString
    test.toString shouldBe "ValueDerivatives{value=123.4, derivatives=[1.0, 2.0, 3.0]}"

    // Nothing about a value paired with its derivatives can be rejected, so this is a total
    // type: the case-class constructor and `copy` are both public and both reachable here,
    // alongside the `of` factory that the ported call sites read through.
    ValueDerivatives(Value, Derivatives) shouldBe test
    test.copy(value = 1.0d).value shouldBe 1.0d
    test.copy(value = 1.0d).derivatives shouldBe Derivatives
    test.copy(derivatives = DoubleArray.of(9.0d)).derivatives shouldBe DoubleArray.of(9.0d)
    test.copy(derivatives = DoubleArray.of(9.0d)).value shouldBe Value

    // Equality compares the value by its bit pattern - the `doubleToLongBits` equality that
    // Joda-Beans used and that this port preserves - so a not-a-number value is equal to itself
    // and an instance carrying one equals an equal instance. The fact asserted underneath it is
    // `java.lang.Double.compare`, the total ordering of doubles that bit-pattern equality agrees
    // with, and not the platform comparison of two doubles with `==`, which reports the opposite
    // for this input and would make the assertion pass for the wrong reason.
    ValueDerivatives.of(Double.NaN, Derivatives) shouldBe ValueDerivatives.of(Double.NaN, Derivatives)
    java.lang.Double.compare(Double.NaN, Double.NaN) shouldBe 0
    // The same claim carried through every equality-bearing member of the type, on two instances
    // built independently of each other: the second names its not-a-number as an arithmetic
    // expression rather than as the constant, and holds a fresh array of its own rather than the
    // shared fixture - the shape the second instance of the Java `coverBeanEquals` call had - so
    // the two instances share no object and the assertions are about the equality of this type,
    // which compares every not-a-number as one value and compares the array element by element,
    // rather than about reference identity. The two spellings of not-a-number are interchangeable
    // here precisely because of that first property. `Hash` is asserted alongside the platform
    // equality because it is this type's single equality-bearing instance and the two have to
    // agree, and the hashes are asserted equal because equal instances that hash differently
    // would be broken in every hashed collection.
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

    // The other half of that same equality: the two zeroes have different bit patterns, so they
    // are different values here, where the platform comparison calls them equal.
    ValueDerivatives.of(-0.0d, Derivatives) should not be ValueDerivatives.of(0.0d, Derivatives)
    java.lang.Double.compare(-0.0d, 0.0d) should not be 0
    java.lang.Double.doubleToLongBits(-0.0d) should not be java.lang.Double.doubleToLongBits(0.0d)

    // Both facts hold for a derivative inside the array as well, because the array compares its
    // elements on the same terms - the element-wise, bit-for-bit comparison of
    // `java.util.Arrays.equals`, whose behaviour on these two inputs is asserted underneath.
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

    // The encoding is the product form, with the two field names the Java bean declared, and
    // decoding it returns an instance equal to the one it came from. Both directions are
    // asserted, because an encoding that is right and a decoding that is wrong would still
    // round trip if only the round trip were checked, and vice versa.
    test.asJson shouldBe json(ExpectedJson)
    decode[ValueDerivatives](test.asJson.noSpaces) shouldBe Right(test)

    // Every double on the path - the value and each derivative - is written through the single
    // policy of this port for the three values JSON cannot express as a number, so an infinity
    // appears as the string "Infinity" in the scalar field and inside the array alike, and
    // decodes back to an equal instance. Equal because the bit-pattern equality of this type
    // makes an infinity equal to itself, which is the same property the not-a-number cases of
    // the coverage test rest on.
    val nonFinite =
      ValueDerivatives.of(Double.PositiveInfinity, DoubleArray.of(1.0d, Double.PositiveInfinity))
    nonFinite.asJson shouldBe json(ExpectedNonFiniteJson)
    decode[ValueDerivatives](nonFinite.asJson.noSpaces) shouldBe Right(nonFinite)
  }
}
