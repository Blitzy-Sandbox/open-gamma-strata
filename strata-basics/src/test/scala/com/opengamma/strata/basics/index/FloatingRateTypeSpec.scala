/*
 * Copyright (C) 2017 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import java.util.Locale

import cats.Eq
import cats.Hash
import cats.Order
import cats.Show

import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2

import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[FloatingRateType]].
 *
 * The name lookup registers every member under its rendered name and that name folded
 * to upper case; the alternate-name table adds the lower-case name for every member,
 * plus the underscored constant identifier in upper and lower case for the two
 * Overnight members. The distinct accepted spellings are therefore three for `Ibor`,
 * `Price` and `Other` - name, upper, lower - and five for `OvernightCompounded` and
 * `OvernightAveraged`, whose underscored forms survive no case folding.
 */
class FloatingRateTypeSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The rows the four table tests are driven from, pairing a type with the name it
   * renders as and is looked up by. They list the averaged Overnight type before the
   * compounded one, the reverse of the declaration order `values` preserves; writing
   * them out rather than deriving them from `values` keeps the two orders independent.
   */
  private val dataName: TableFor2[FloatingRateType, String] = Table(
    ("type", "name"),
    (FloatingRateType.Ibor, "Ibor"),
    (FloatingRateType.OvernightAveraged, "OvernightAveraged"),
    (FloatingRateType.OvernightCompounded, "OvernightCompounded"),
    (FloatingRateType.Price, "Price"),
    (FloatingRateType.Other, "Other")
  )

  //-------------------------------------------------------------------------
  test("test_isIbor") {
    FloatingRateType.Ibor.isIbor shouldBe true
    FloatingRateType.OvernightAveraged.isIbor shouldBe false
    FloatingRateType.OvernightCompounded.isIbor shouldBe false
    FloatingRateType.Price.isIbor shouldBe false
    FloatingRateType.Other.isIbor shouldBe false
  }

  test("test_isOvernight") {
    // the one predicate true of two members, which differ only in how the rate accrues
    FloatingRateType.Ibor.isOvernight shouldBe false
    FloatingRateType.OvernightAveraged.isOvernight shouldBe true
    FloatingRateType.OvernightCompounded.isOvernight shouldBe true
    FloatingRateType.Price.isOvernight shouldBe false
    FloatingRateType.Other.isOvernight shouldBe false
  }

  test("test_isPrice") {
    FloatingRateType.Ibor.isPrice shouldBe false
    FloatingRateType.OvernightAveraged.isPrice shouldBe false
    FloatingRateType.OvernightCompounded.isPrice shouldBe false
    FloatingRateType.Price.isPrice shouldBe true
    FloatingRateType.Other.isPrice shouldBe false
  }

  //-------------------------------------------------------------------------
  test("test_toString") {
    forAll(dataName) { (floatingRateType: FloatingRateType, name: String) =>
      floatingRateType.toString shouldBe name
    }
  }

  test("test_of_lookup") {
    forAll(dataName) { (floatingRateType: FloatingRateType, name: String) =>
      withClue(s"$name: ") {
        FloatingRateType.valueOf(name) shouldBe Some(floatingRateType)
        FloatingRateType.parse(name) should haveValue(floatingRateType)
      }
    }
  }

  test("test_of_lookupUpperCase") {
    // the upper-case spelling is the second key the family registers for every member
    forAll(dataName) { (floatingRateType: FloatingRateType, name: String) =>
      val upperCase = name.toUpperCase(Locale.ENGLISH)
      withClue(s"$upperCase: ") {
        FloatingRateType.valueOf(upperCase) shouldBe Some(floatingRateType)
        FloatingRateType.parse(upperCase) should haveValue(floatingRateType)
      }
    }
  }

  test("test_of_lookupLowerCase") {
    // the lower-case spelling is a row of the alternate-name table
    forAll(dataName) { (floatingRateType: FloatingRateType, name: String) =>
      val lowerCase = name.toLowerCase(Locale.ENGLISH)
      withClue(s"$lowerCase: ") {
        FloatingRateType.valueOf(lowerCase) shouldBe Some(floatingRateType)
        FloatingRateType.parse(lowerCase) should haveValue(floatingRateType)
      }
    }
  }

  test("test_of_lookup_notFound") {
    // the failure is compared by reason, so its diagnostic wording stays free to change
    FloatingRateType.valueOf("Rubbish") shouldBe None
    FloatingRateType.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
  }

  test("test_of_lookup_null") {
    // an absent name reaches these factories as the empty name or a blank one
    FloatingRateType.valueOf("") shouldBe None
    FloatingRateType.parse("") should beFailureWith(FailureReason.PARSING)
    FloatingRateType.valueOf("   ") shouldBe None
    FloatingRateType.parse("   ") should beFailureWith(FailureReason.PARSING)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // the closed surface: five distinct values, each reachable by its own name
    val all: List[FloatingRateType] = FloatingRateType.values.toList
    all should have size 5
    all.distinct should have size 5
    all.map(_.name).distinct should have size 5

    all.foreach { floatingRateType =>
      withClue(s"${floatingRateType.name}: ") {
        FloatingRateType.valueOf(floatingRateType.name) shouldBe Some(floatingRateType)
        FloatingRateType.parse(floatingRateType.name) should haveValue(floatingRateType)
        Show[FloatingRateType].show(floatingRateType) shouldBe floatingRateType.name
        floatingRateType.toString shouldBe floatingRateType.name
      }
    }

    // The companion publishes one equality-bearing instance, an ordering that is also a
    // hashing, so all three summons yield that value. All three are asked about every
    // ordered pair of members and required to give one answer about equality.
    for (left <- all; right <- all) {
      val sameValue = left == right
      withClue(s"${left.name} against ${right.name}: ") {
        Eq[FloatingRateType].eqv(left, right) shouldBe sameValue
        Hash[FloatingRateType].eqv(left, right) shouldBe sameValue
        (Order[FloatingRateType].compare(left, right) == 0) shouldBe sameValue
        if (sameValue) {
          Hash[FloatingRateType].hash(left) shouldBe Hash[FloatingRateType].hash(right)
        } else {
          Order[FloatingRateType].compare(left, right) should not be 0
        }
      }
    }
  }

  test("test_serialization") {
    // the shape the codec is required to have for this family: a member is written as the
    // bare string of its name, never as an object, and is read back from that string
    forAll(dataName) { (floatingRateType: FloatingRateType, name: String) =>
      val encoded = floatingRateType.asJson
      withClue(s"$name: ") {
        encoded shouldBe Json.fromString(name)
        encoded.as[FloatingRateType] shouldBe Right(floatingRateType)
      }
    }
    Json.fromString("Rubbish").as[FloatingRateType].isLeft shouldBe true
  }

  test("test_jodaConvert") {
    // the text round trip, independent of the JSON one in `test_serialization`
    forAll(dataName) { (floatingRateType: FloatingRateType, name: String) =>
      val rendered = Show[FloatingRateType].show(floatingRateType)
      withClue(s"$name: ") {
        rendered shouldBe name
        rendered shouldBe floatingRateType.name
        FloatingRateType.parse(rendered) should haveValue(floatingRateType)
      }
    }
  }
}
