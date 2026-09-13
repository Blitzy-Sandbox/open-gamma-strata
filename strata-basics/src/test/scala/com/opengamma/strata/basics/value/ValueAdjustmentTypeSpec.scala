/*
 * Copyright (C) 2017 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.value

import java.util.Locale

import cats.Hash
import cats.Order
import cats.Show

import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2

import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[ValueAdjustmentType]].
 *
 * A type is a member of a closed family of four, each of which is a name and an `adjust`
 * operation over a base value and a modifying value. The tests read that arithmetic, the
 * canonical name in each of the forms it takes, the two lookups from text back to a member,
 * and the published inventory of the family.
 *
 * The canonical name is a single contract rather than several: `name`, `toString`, `Show`
 * and the JSON form all produce the same string, and the lookup accepts exactly that string
 * back. Several tests below are therefore views of that one fact, written over the same
 * table of members and names.
 */
final class ValueAdjustmentTypeSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The four members paired with the canonical name each renders as and parses from.
   *
   * The rows are alphabetical by name rather than in the declaration order of the members;
   * the declaration order is asserted separately, in `coverage`, over `values`.
   */
  private val dataName: TableFor2[ValueAdjustmentType, String] = Table(
    ("type", "name"),
    (ValueAdjustmentType.DeltaAmount, "DeltaAmount"),
    (ValueAdjustmentType.DeltaMultiplier, "DeltaMultiplier"),
    (ValueAdjustmentType.Multiplier, "Multiplier"),
    (ValueAdjustmentType.Replace, "Replace")
  )

  /**
   * The four members, in the declaration order `values` publishes them in.
   *
   * Held once so that the inventory assertions of `coverage` and the pairwise equality
   * assertions read from the same list the production companion publishes.
   */
  private val allTypes: List[ValueAdjustmentType] = ValueAdjustmentType.values.toList

  //-------------------------------------------------------------------------
  test("test_adjust") {
    // the four arithmetic shapes, one per member. Every literal carries an explicit `d`: the
    // build compiles with numeric widening treated as an error, so an `Int` literal in a
    // `Double` position would not compile
    ValueAdjustmentType.DeltaAmount.adjust(2.0d, 3.0d) shouldBe 5.0d
    ValueAdjustmentType.DeltaMultiplier.adjust(2.0d, 1.5d) shouldBe 5.0d
    ValueAdjustmentType.Multiplier.adjust(2.0d, 1.5d) shouldBe 3.0d
    ValueAdjustmentType.Replace.adjust(2.0d, 1.5d) shouldBe 1.5d
  }

  //-------------------------------------------------------------------------
  test("test_toString") {
    forAll(dataName) { (adjustmentType: ValueAdjustmentType, name: String) =>
      adjustmentType.toString shouldBe name
      // `name` and `Show` are the same contract as `toString`: all three render the
      // canonical name, so the three ways of putting a type into a message agree
      adjustmentType.name shouldBe name
      Show[ValueAdjustmentType].show(adjustmentType) shouldBe name
    }
  }

  //-------------------------------------------------------------------------
  test("test_of_lookup") {
    forAll(dataName) { (adjustmentType: ValueAdjustmentType, name: String) =>
      // two lookups carry text back to a member: an exact one answering `Option` and a
      // lenient one answering a failure as a value. The canonical name resolves through both
      ValueAdjustmentType.valueOf(name) shouldBe Some(adjustmentType)
      ValueAdjustmentType.parse(name) should haveValue(adjustmentType)
    }

    // No member is spelled in SCREAMING_SNAKE; those spellings are alternate names of the
    // lookup, so a stored document written against them still resolves. They are asserted here
    // so that removing a row of the alternate table fails rather than silently narrowing the
    // set of spellings such a document may use.
    ValueAdjustmentType.valueOf("DELTA_AMOUNT") shouldBe Some(ValueAdjustmentType.DeltaAmount)
    ValueAdjustmentType.valueOf("delta_amount") shouldBe Some(ValueAdjustmentType.DeltaAmount)
    ValueAdjustmentType.valueOf("DELTA_MULTIPLIER") shouldBe Some(ValueAdjustmentType.DeltaMultiplier)
    ValueAdjustmentType.valueOf("delta_multiplier") shouldBe Some(ValueAdjustmentType.DeltaMultiplier)
    ValueAdjustmentType.parse("DELTA_AMOUNT") should haveValue(ValueAdjustmentType.DeltaAmount)
    ValueAdjustmentType.parse("DELTA_MULTIPLIER") should haveValue(ValueAdjustmentType.DeltaMultiplier)
  }

  test("test_of_lookupUpperCase") {
    forAll(dataName) { (adjustmentType: ValueAdjustmentType, name: String) =>
      // the English locale is named deliberately: the no-argument fold is locale sensitive
      // and would not be the same operation everywhere
      val upperCase = name.toUpperCase(Locale.ENGLISH)
      // every member is registered under its canonical name folded to upper case, so the
      // upper-case spelling resolves through the exact lookup and not only through `parse`
      ValueAdjustmentType.valueOf(upperCase) shouldBe Some(adjustmentType)
      ValueAdjustmentType.parse(upperCase) should haveValue(adjustmentType)
    }
  }

  test("test_of_lookupLowerCase") {
    forAll(dataName) { (adjustmentType: ValueAdjustmentType, name: String) =>
      val lowerCase = name.toLowerCase(Locale.ENGLISH)
      // the lower-case run-together spelling is an alternate name of the family, which the
      // exact lookup consults before matching, so this too resolves through `valueOf`
      ValueAdjustmentType.valueOf(lowerCase) shouldBe Some(adjustmentType)
      ValueAdjustmentType.parse(lowerCase) should haveValue(adjustmentType)
    }
  }

  //-------------------------------------------------------------------------
  test("test_of_lookup_notFound") {
    // text naming no member is rejected as a value rather than by raising: `parse` reports a
    // parsing failure and `valueOf` answers `None`. The reason is compared as a member of the
    // closed family of reasons rather than as its text, so a renamed reason fails to compile
    ValueAdjustmentType.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
    ValueAdjustmentType.valueOf("Rubbish") shouldBe None
  }

  test("test_of_lookup_null") {
    // There is no throwing `of` lookup on this family at all: the snippet below names one and
    // does not compile, which is what the refusal states. Text naming no member resolves to
    // nothing instead - `valueOf` answers `None` and `parse` reports a parsing failure - and
    // empty text is such text, rejected on exactly those terms rather than by a rule of its
    // own
    assertDoesNotCompile("""ValueAdjustmentType.of("Replace")""")
    ValueAdjustmentType.parse("") should beFailureWith(FailureReason.PARSING)
    ValueAdjustmentType.valueOf("") shouldBe None
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The family is closed because the class is `sealed` and its four members are the four
    // `case object`s declared beside it in that one file, which the compiler enforces.
    // `values` publishes that inventory, in declaration order, and these assertions read it
    allTypes shouldBe List(
      ValueAdjustmentType.Replace,
      ValueAdjustmentType.DeltaAmount,
      ValueAdjustmentType.DeltaMultiplier,
      ValueAdjustmentType.Multiplier
    )
    allTypes.size shouldBe 4
    allTypes.distinct shouldBe allTypes
    allTypes.map(_.name).distinct.size shouldBe 4

    // every member is reachable from its own name, through both lookups
    allTypes.foreach { adjustmentType =>
      ValueAdjustmentType.valueOf(adjustmentType.name) shouldBe Some(adjustmentType)
      ValueAdjustmentType.parse(adjustmentType.name) should haveValue(adjustmentType)
    }

    // the companion publishes one equality-bearing instance, an `Order` that is also a `Hash`,
    // and its comparison, equality and hashing all read the name, so a member equals itself and
    // nothing else and hashing follows equality
    allTypes.foreach { adjustmentType =>
      Hash[ValueAdjustmentType].eqv(adjustmentType, adjustmentType) shouldBe true
      Order[ValueAdjustmentType].compare(adjustmentType, adjustmentType) shouldBe 0
      Hash[ValueAdjustmentType].hash(adjustmentType) shouldBe adjustmentType.name.hashCode
    }
    // the pairs are formed by identity rather than by name, so that the assertions below
    // prove what names alone could not: that no two members share a name, and therefore
    // that equality by name separates the members as their identity does. Four members yield
    // twelve ordered pairs of distinct members, three for each left member
    val distinctPairs =
      for {
        left <- allTypes
        right <- allTypes
        if left ne right
      } yield (left, right)
    distinctPairs.size shouldBe 12
    distinctPairs.foreach { case (left, right) =>
      Hash[ValueAdjustmentType].eqv(left, right) shouldBe false
      Order[ValueAdjustmentType].compare(left, right) should not be 0
    }

    // the ordering is by name and therefore alphabetical, while `values` keeps the
    // declaration order, so both orders stay available and neither implies the other
    allTypes.sorted(Order[ValueAdjustmentType].toOrdering) shouldBe allTypes.sortBy(_.name)
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // the JSON codec is what carries a type out of the process and back. The type is ascribed
    // because the encoder is invariant in its type and the singleton type of a member is not
    // the type the family publishes
    val deltaAmount: ValueAdjustmentType = ValueAdjustmentType.DeltaAmount
    deltaAmount.asJson shouldBe Json.fromString("DeltaAmount")
    decode[ValueAdjustmentType]("\"DeltaAmount\"") shouldBe Right(ValueAdjustmentType.DeltaAmount)

    forAll(dataName) { (adjustmentType: ValueAdjustmentType, name: String) =>
      // a bare string, never a wrapper object, so a serialized adjustment carries its type
      // as one readable word
      adjustmentType.asJson shouldBe Json.fromString(name)
      adjustmentType.asJson.isString shouldBe true
      decode[ValueAdjustmentType](Json.fromString(name).noSpaces) shouldBe Right(adjustmentType)
    }

    // text naming no member is rejected by the reader rather than decoded to something
    // else. The reader reports a decoding error of the JSON library, which is not one of
    // the failure shapes the result matchers describe, so this one assertion reads the
    // `Either` directly where every assertion over a parse failure above goes through
    // `beFailureWith`
    decode[ValueAdjustmentType]("\"Rubbish\"").isLeft shouldBe true
  }

  test("test_jodaConvert") {
    // the canonical name is the one string form of a type: every member renders it through
    // `name` and through `Show`, and both lookups take that same string back to the member it
    // came from. Asserted here for each of the four
    forAll(dataName) { (adjustmentType: ValueAdjustmentType, name: String) =>
      adjustmentType.name shouldBe name
      Show[ValueAdjustmentType].show(adjustmentType) shouldBe name
      ValueAdjustmentType.parse(adjustmentType.name) should haveValue(adjustmentType)
      ValueAdjustmentType.valueOf(Show[ValueAdjustmentType].show(adjustmentType)) shouldBe
        Some(adjustmentType)
    }
  }
}
