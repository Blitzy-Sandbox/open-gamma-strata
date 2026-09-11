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
 * The Java original held ten test methods, four of which were parameterised from a single
 * data provider, `data_name`. That shape is preserved exactly: the provider becomes one
 * shared table, declared once, and each of the ten methods keeps a test of its own under
 * its own name, with the table iterated inside it. Collapsing the four parameterised
 * methods into one test - or expanding one of them into a test per row - would break the
 * method-level traceability of the migration, which joins each ported Java method to the
 * test case this suite emits.
 *
 * ===The three methods whose Java form has no target===
 *
 * Three of the ten asserted things about machinery this port removes, so each keeps its
 * name and asserts the fact that replaced it:
 *
 *  - `test_of_lookup_null` asserted that the throwing factory rejected an absent reference.
 *    The port has neither a throwing factory nor an absent reference to hand one, so the
 *    test proves both absences instead.
 *  - `coverage` called a reflective helper over the Java enum class. Nothing is derived
 *    reflectively here, so the properties the helper stood in for - a complete, ordered,
 *    duplicate-free family whose names round-trip, with one consistent notion of equality -
 *    are asserted directly.
 *  - `test_serialization` and `test_jodaConvert` asserted Java serialization and
 *    Joda-Convert round-trips. Neither library is on the class path of this port; the JSON
 *    codec and the name lookup are what carry a type out of the process and back, so those
 *    are what the two tests assert.
 *
 * The four canonical names are a single contract, not three: `name`, `toString`, `Show` and
 * the JSON form all produce the same string, and the lookup accepts exactly that string
 * back. `test_toString`, `test_serialization` and `test_jodaConvert` are therefore three
 * views of one fact, and they are written over the same table so that they cannot drift.
 */
final class ValueAdjustmentTypeSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The shared provider, transcribed row for row from the Java data provider.
   *
   * Each row is a type together with the canonical name it renders as and parses from. The
   * rows are in the order the Java provider listed them, which is alphabetical by name
   * rather than the declaration order of the members; the declaration order is asserted
   * separately, in `coverage`, over `values`.
   */
  private val dataName: TableFor2[ValueAdjustmentType, String] = Table(
    ("type", "name"),
    (ValueAdjustmentType.DELTA_AMOUNT, "DeltaAmount"),
    (ValueAdjustmentType.DELTA_MULTIPLIER, "DeltaMultiplier"),
    (ValueAdjustmentType.MULTIPLIER, "Multiplier"),
    (ValueAdjustmentType.REPLACE, "Replace")
  )

  /**
   * The four members, in the declaration order of the enum being ported.
   *
   * Held once so that the closure assertions of `coverage` and the pairwise equality
   * assertions read from the same list the production companion publishes.
   */
  private val allTypes: List[ValueAdjustmentType] = ValueAdjustmentType.values.toList

  //-------------------------------------------------------------------------
  test("test_adjust") {
    // the four arithmetic shapes, with the operands and expectations of the Java test.
    // Every literal carries an explicit `d`: the build compiles with numeric widening
    // treated as an error, so an `Int` literal in a `Double` position would not compile
    ValueAdjustmentType.DELTA_AMOUNT.adjust(2.0d, 3.0d) shouldBe 5.0d
    ValueAdjustmentType.DELTA_MULTIPLIER.adjust(2.0d, 1.5d) shouldBe 5.0d
    ValueAdjustmentType.MULTIPLIER.adjust(2.0d, 1.5d) shouldBe 3.0d
    ValueAdjustmentType.REPLACE.adjust(2.0d, 1.5d) shouldBe 1.5d
  }

  //-------------------------------------------------------------------------
  test("test_toString") {
    forAll(dataName) { (adjustmentType: ValueAdjustmentType, name: String) =>
      adjustmentType.toString shouldBe name
      // `name` and `Show` are the same contract as `toString` in this port: the Java
      // rendering was produced by a name helper, here it is the canonical name itself,
      // and the three ways of putting a type into a message have to agree
      adjustmentType.name shouldBe name
      Show[ValueAdjustmentType].show(adjustmentType) shouldBe name
    }
  }

  //-------------------------------------------------------------------------
  test("test_of_lookup") {
    forAll(dataName) { (adjustmentType: ValueAdjustmentType, name: String) =>
      // the Java factory threw on an unknown name; the port splits that into an exact
      // lookup answering `Option` and a lenient one answering the failure as a value,
      // and the canonical name resolves through both
      ValueAdjustmentType.valueOf(name) shouldBe Some(adjustmentType)
      ValueAdjustmentType.parse(name) should haveValue(adjustmentType)
    }
  }

  test("test_of_lookupUpperCase") {
    forAll(dataName) { (adjustmentType: ValueAdjustmentType, name: String) =>
      // the English locale is named explicitly, as the Java test named it: the no-argument
      // fold is locale sensitive and would not be the same operation everywhere
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
    // where the Java factory raised an illegal-argument error, the port reports the
    // rejection as a value. The reason is compared as a member of the closed family of
    // reasons rather than as its text, so a renamed reason fails to compile here
    ValueAdjustmentType.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
    ValueAdjustmentType.valueOf("Rubbish") shouldBe None
  }

  test("test_of_lookup_null") {
    // The Java case asserted that the factory rejected an absent reference - the argument
    // it passed denoted nothing at all. That failure mode is not expressible in this port:
    // there is no throwing factory to call, and no operation of this family either accepts
    // an absent reference or produces one, because the exact lookup answers `Option` and
    // the lenient one answers a failure as a value. The test therefore proves the two facts
    // that replaced it: that no throwing lookup survives, and that text naming no member is
    // rejected as a value. Text that is present but empty is the nearest input this port can
    // express, and it is rejected for the same reason as any other unknown name
    assertDoesNotCompile("""ValueAdjustmentType.of("Replace")""")
    ValueAdjustmentType.parse("") should beFailureWith(FailureReason.PARSING)
    ValueAdjustmentType.valueOf("") shouldBe None
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java test swept the enum class reflectively. Nothing here is derived from a
    // class or from the class path, so the properties that sweep stood in for are asserted
    // over the published family directly - which is also what makes the family closed:
    // `values` is the whole of it, in declaration order
    allTypes shouldBe List(
      ValueAdjustmentType.REPLACE,
      ValueAdjustmentType.DELTA_AMOUNT,
      ValueAdjustmentType.DELTA_MULTIPLIER,
      ValueAdjustmentType.MULTIPLIER
    )
    allTypes.size shouldBe 4
    allTypes.distinct shouldBe allTypes
    allTypes.map(_.name).distinct.size shouldBe 4

    // every member is reachable from its own name, through both lookups
    allTypes.foreach { adjustmentType =>
      ValueAdjustmentType.valueOf(adjustmentType.name) shouldBe Some(adjustmentType)
      ValueAdjustmentType.parse(adjustmentType.name) should haveValue(adjustmentType)
    }

    // the companion publishes one equality-bearing instance, an `Order` that is also a
    // `Hash`, so equality, hashing and comparison cannot disagree: a member equals itself
    // and nothing else, and hashing follows equality
    allTypes.foreach { adjustmentType =>
      Hash[ValueAdjustmentType].eqv(adjustmentType, adjustmentType) shouldBe true
      Order[ValueAdjustmentType].compare(adjustmentType, adjustmentType) shouldBe 0
      Hash[ValueAdjustmentType].hash(adjustmentType) shouldBe adjustmentType.name.hashCode
    }
    // the pairs are formed by identity rather than by name, so that the assertions below
    // prove what names alone could not: that no two members share a name, and therefore
    // that equality by name separates the members as their identity does
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

    // the ordering is by name, which is alphabetical, and deliberately not the declaration
    // order the Java enum compared by; both orders stay available because `values` keeps
    // the declaration one
    allTypes.sorted(Order[ValueAdjustmentType].toOrdering) shouldBe allTypes.sortBy(_.name)
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // Java serialization has no target in this port; the JSON codec is what carries a type
    // out of the process and back. The type is ascribed because the encoder is invariant in
    // its type and the singleton type of a member is not the type the family publishes
    val deltaAmount: ValueAdjustmentType = ValueAdjustmentType.DELTA_AMOUNT
    deltaAmount.asJson shouldBe Json.fromString("DeltaAmount")
    decode[ValueAdjustmentType]("\"DeltaAmount\"") shouldBe Right(ValueAdjustmentType.DELTA_AMOUNT)

    forAll(dataName) { (adjustmentType: ValueAdjustmentType, name: String) =>
      // a bare string, never a wrapper object, so a serialized adjustment carries its type
      // as one readable word - the single-string form the type being ported wrote
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
    // Joda-Convert itself has no target: the port depends on neither Joda library. What its
    // string conversion annotations expressed - a canonical string form that the type
    // renders and the companion parses back - is the identity contract of a named family,
    // and that is what is asserted here, for every member
    forAll(dataName) { (adjustmentType: ValueAdjustmentType, name: String) =>
      adjustmentType.name shouldBe name
      Show[ValueAdjustmentType].show(adjustmentType) shouldBe name
      ValueAdjustmentType.parse(adjustmentType.name) should haveValue(adjustmentType)
      ValueAdjustmentType.valueOf(Show[ValueAdjustmentType].show(adjustmentType)) shouldBe
        Some(adjustmentType)
    }
  }
}
