/*
 * Copyright (C) 2017 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import java.util.Locale

import scala.collection.mutable.ListBuffer

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
 * Every method of the Java original is kept, under its own name, so the method-level
 * traceability of the migration stays one-to-one. The four parameterised methods of the
 * original were driven from a single provider, `data_name`; that shape is preserved -
 * the provider becomes one shared table, declared once below, and each of the four
 * methods keeps its own test driven from it.
 *
 * Three of the methods asserted machinery this port does not have, so each is ported as
 * the assertion of the guarantee that machinery gave rather than dropped. The reasoning
 * is recorded at each of them:
 *
 *  - `test_of_lookup_null` asserted that the factory rejected an absent name by raising
 *    an error. The factories here answer with a value, so the case becomes the empty and
 *    blank name resolving to a failure.
 *  - `coverage` called a reflective sweep over the constants of a Java enum, which a
 *    sealed family of case objects offers no target for; the closed-family properties
 *    that sweep stood in for are asserted directly.
 *  - `test_jodaConvert` asserted the round trip of the reflective string-conversion
 *    library of the original, which this port does not depend on; the guarantee its two
 *    annotations gave - a value renders as its name, and that name reads back as the
 *    same value - is asserted directly.
 *
 * The two case-folding methods are worth singling out. They pass only because the name
 * lookup of [[FloatingRateType]] carries the six spellings per member that the name
 * helper of the original derived from each constant, so a failure here is a defect in
 * that table rather than an assertion to relax.
 *
 * A final section, after the ported methods and marked as such, asserts the union search
 * of the sibling abstraction [[FloatingRate]]. It is here because the Java sources have
 * no test class for that abstraction - the union is exercised through the specs of its
 * consumers, which are ported together with the index families they assert against - and
 * because what the union itself owns can be asserted without any family having members.
 * The reasoning is repeated at the section.
 */
class FloatingRateTypeSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The shared provider, transcribed row-for-row from the Java data provider.
   *
   * Each row pairs a type with the name it renders as and is looked up by. The rows are
   * in the order of the Java provider, which lists the averaged Overnight type before
   * the compounded one - the reverse of the declaration order that `values` preserves.
   * Keeping the provider's order rather than deriving the rows from `values` is
   * deliberate: the two orders are then asserted independently, so a mistake in one
   * cannot be masked by the other.
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
    // the one predicate that is true of two members: both Overnight types translate to
    // an Overnight index and differ only in how the rate accrues
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
    // The Java factory both looked a name up exactly and reported an unknown one; the
    // port splits those into `valueOf`, the exact lookup, and `parse`, which reports the
    // failure. Both are asserted, so the two entry points cannot drift apart.
    forAll(dataName) { (floatingRateType: FloatingRateType, name: String) =>
      withClue(s"$name: ") {
        FloatingRateType.valueOf(name) shouldBe Some(floatingRateType)
        FloatingRateType.parse(name) should haveValue(floatingRateType)
      }
    }
  }

  test("test_of_lookupUpperCase") {
    // `valueOf` is the load-bearing assertion here: the exact lookup resolves the
    // upper-case spelling only because that spelling is one of the six keys the name
    // lookup of this family registers for each member.
    forAll(dataName) { (floatingRateType: FloatingRateType, name: String) =>
      val upperCase = name.toUpperCase(Locale.ENGLISH)
      withClue(s"$upperCase: ") {
        FloatingRateType.valueOf(upperCase) shouldBe Some(floatingRateType)
        FloatingRateType.parse(upperCase) should haveValue(floatingRateType)
      }
    }
  }

  test("test_of_lookupLowerCase") {
    // as above, for the lower-case spelling, which the alternate-name table supplies
    forAll(dataName) { (floatingRateType: FloatingRateType, name: String) =>
      val lowerCase = name.toLowerCase(Locale.ENGLISH)
      withClue(s"$lowerCase: ") {
        FloatingRateType.valueOf(lowerCase) shouldBe Some(floatingRateType)
        FloatingRateType.parse(lowerCase) should haveValue(floatingRateType)
      }
    }
  }

  test("test_of_lookup_notFound") {
    // Where the Java factory raised an error for text naming no member, the port reports
    // it as a value. The reason is compared by value rather than by matching the message,
    // so the diagnostic wording of the failure stays free to change.
    FloatingRateType.valueOf("Rubbish") shouldBe None
    FloatingRateType.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
  }

  test("test_of_lookup_null") {
    // Reinterpretation: the Java method passed the absent reference to the factory and
    // asserted that it raised an error. This port writes no such reference and its
    // factories take a name they resolve as a value, so the case is asserted as the two
    // spellings of an absent name that can actually be supplied - the empty name and a
    // blank one - each of which names no member and so resolves to a parsing failure.
    FloatingRateType.valueOf("") shouldBe None
    FloatingRateType.parse("") should beFailureWith(FailureReason.PARSING)
    FloatingRateType.valueOf("   ") shouldBe None
    FloatingRateType.parse("   ") should beFailureWith(FailureReason.PARSING)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java method swept the constants of the enum reflectively. A sealed family of
    // case objects has no such constants to read back, so the properties that sweep
    // stood in for are asserted over the family's own closed surface instead: the
    // membership is exactly five distinct values, each one is reachable by its own name
    // through both entry points, and the typeclass instances agree with each other. The
    // cross-family sweep of NamedEnumClosedSpec is complementary to this and does not
    // replace it - that spec proves every family is closed, this one proves what the
    // members of this family are.
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

    // The companion publishes one equality-bearing instance - an ordering that is also a
    // hashing - so summoning the equality, the hashing or the ordering yields that one
    // value and the three can never disagree. The assertions below are the observable
    // form of that: all three are asked about every ordered pair of members, and all
    // three have to give the same answer about equality.
    for (left <- all; right <- all) {
      val sameValue = left == right
      withClue(s"${left.name} against ${right.name}: ") {
        Eq[FloatingRateType].eqv(left, right) shouldBe sameValue
        Hash[FloatingRateType].eqv(left, right) shouldBe sameValue
        // the ordering is consistent with equality: it compares equal exactly when the
        // two values are equal, which is the law the combined instance has to satisfy
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
    // The Java method asserted a round trip through the serialization mechanism of the
    // platform, which this port does not support. Its replacement is the JSON codec, and
    // what this test pins is the shape that codec is required to have for this family: a
    // type is written as the bare string of its name and never as an object, so a
    // document written by the original reads back here as the same member. The
    // property-based round trip over every codec-bearing type of the module belongs to
    // the consolidated json.JsonRoundTripSpec, per the test mapping manifest; this test
    // deliberately asserts the per-type representation rather than repeating that sweep.
    forAll(dataName) { (floatingRateType: FloatingRateType, name: String) =>
      val encoded = floatingRateType.asJson
      withClue(s"$name: ") {
        encoded shouldBe Json.fromString(name)
        encoded.as[FloatingRateType] shouldBe Right(floatingRateType)
      }
    }
    // a string naming no member of the family is rejected by the reader
    Json.fromString("Rubbish").as[FloatingRateType].isLeft shouldBe true
  }

  test("test_jodaConvert") {
    // The Java method asserted the round trip of the reflective string-conversion
    // library the original registered two annotations with. The library is gone with the
    // port, and the guarantee it gave is asserted directly: a value renders as its name,
    // and that rendering reads back as the same value. This is the text round trip; the
    // JSON round trip is `test_serialization` above, and the two are kept apart because
    // the representations they pin are independent of each other.
    forAll(dataName) { (floatingRateType: FloatingRateType, name: String) =>
      val rendered = Show[FloatingRateType].show(floatingRateType)
      withClue(s"$name: ") {
        rendered shouldBe name
        rendered shouldBe floatingRateType.name
        FloatingRateType.parse(rendered) should haveValue(floatingRateType)
      }
    }
  }

  //-------------------------------------------------------------------------
  // The union search of the sibling abstraction, FloatingRate, is asserted below. It is
  // asserted in this spec because the Java sources have no test class for that
  // abstraction, so the test inventory of this port maps no spec to it: the union is
  // exercised through the specs of its consumers, which are ported together with the
  // index families those specs assert against. What belongs here is what the union owns
  // by itself and what can be asserted without a family having members - the search
  // rule, over probes this spec supplies, and the shape of the composition the library
  // ships. Which family answers a given name is a property of the families and is
  // asserted with them, so no assertion below reads a family's membership.

  /**
   * Builds a probe that records when it is produced and when it is applied.
   *
   * The production entry is what makes the laziness of a composition observable: the
   * elements of the shipped composition are supplied by name, so a search that stops
   * early must never produce the probes after the one that answered - that is what keeps
   * a search from loading the companion of a family it never consults. The application
   * entry records the search order.
   *
   * @param label  the name this probe records itself under
   * @param hits  the text this probe claims
   * @param log  the record of production and application, in the order they happened
   * @return the probe
   */
  private def recordingProbe(
      label: String,
      hits: Set[String],
      log: ListBuffer[String]): String => Option[String] = {
    log += s"produced $label"
    name => {
      log += s"probed $label"
      if (hits.contains(name)) Some(s"$label:$name") else None
    }
  }

  /**
   * A three-probe composition in the shape of the shipped one: by-name elements, and two
   * probes claiming the same text so that precedence is observable.
   *
   * @param log  the record the probes write to
   * @return the composition
   */
  private def recordingComposition(log: ListBuffer[String]): LazyList[String => Option[String]] =
    recordingProbe("first", Set("a", "shared"), log) #::
      recordingProbe("second", Set("b", "shared"), log) #::
      recordingProbe("third", Set("c"), log) #::
      LazyList.empty[String => Option[String]]

  test("test_union_searchesInOrderAndStopsAtTheFirstHit") {
    val log = ListBuffer.empty[String]
    val composition = recordingComposition(log)
    // obtaining the composition produces no probe at all
    log.toList shouldBe empty
    FloatingRate.tryParseWith("b", composition) shouldBe Some("second:b")
    // the first probe was produced and applied, then the second, which answered; the
    // third was never produced, so a family behind it would not have been loaded
    log.toList shouldBe List("produced first", "probed first", "produced second", "probed second")
  }

  test("test_union_earlierProbeClaimsSharedText") {
    // the property the fixed probe order exists for: where two families claim the same
    // text, the earlier of them answers and the later is never reached
    val log = ListBuffer.empty[String]
    FloatingRate.tryParseWith("shared", recordingComposition(log)) shouldBe Some("first:shared")
    log.toList shouldBe List("produced first", "probed first")
  }

  test("test_union_findsNothingWhenEveryProbeMisses") {
    val log = ListBuffer.empty[String]
    FloatingRate.tryParseWith("Rubbish", recordingComposition(log)) shouldBe None
    // every probe was produced and applied exactly once, in order
    log.toList shouldBe List(
      "produced first",
      "probed first",
      "produced second",
      "probed second",
      "produced third",
      "probed third")
  }

  test("test_union_findsNothingInAnEmptyComposition") {
    // the search is total in the composition it is given, including the empty one
    FloatingRate.tryParseWith("a", LazyList.empty[FloatingRate.Lookup]) shouldBe None
    FloatingRate.tryParseWith("a", List.empty[FloatingRate.Lookup]) shouldBe None
  }

  test("test_union_searchesAStrictComposition") {
    // a caller composing its own probes need not build a lazy sequence: the search
    // tries whatever ordered sequence it is handed, and still stops at the first hit
    val log = ListBuffer.empty[String]
    val strict: List[String => Option[String]] =
      List(recordingProbe("only", Set("a"), log), recordingProbe("unreached", Set("a"), log))
    FloatingRate.tryParseWith("a", strict) shouldBe Some("only:a")
    log.toList.filter(_.startsWith("probed")) shouldBe List("probed only")
  }

  test("test_standardLookups") {
    // the composition the library ships is four probes: the Ibor, Overnight and Price
    // index families and then the floating rate names. Which family each probe belongs
    // to is asserted with the families themselves; here it is the count and the fact
    // that each probe is an exact lookup, so text naming no member is answered by every
    // one of them with nothing rather than by a lenient rewrite
    FloatingRate.standardLookups.size shouldBe 4
    FloatingRate.standardLookups.zipWithIndex.foreach { case (lookup, index) =>
      withClue(s"probe $index: ") {
        lookup("Rubbish") shouldBe None
      }
    }
    // a subset composes: the search of the three index families alone is the shipped
    // composition without its last probe
    FloatingRate.standardLookups.take(3).size shouldBe 3
  }

  test("test_union_unknownTextIsReportedNotRaised") {
    // the two entry points of the union differ only in how they report text naming
    // nothing: the Java original raised an error from `parse`, and the message it
    // carried is now the message of the failure, unchanged
    FloatingRate.tryParse("Rubbish") shouldBe None
    FloatingRate.parse("Rubbish") match {
      case Left(failure) =>
        failure.reason shouldBe FailureReason.PARSING
        failure.message shouldBe "Floating rate index not known: Rubbish"
      case Right(floatingRate) =>
        fail(s"expected a parsing failure, found $floatingRate")
    }
  }
}
