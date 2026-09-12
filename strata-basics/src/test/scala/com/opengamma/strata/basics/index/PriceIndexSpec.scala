/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

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

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.date.DayCounts
import com.opengamma.strata.basics.location.Country
import com.opengamma.strata.basics.schedule.Frequency
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[PriceIndex]].
 *
 * Every method of the Java original is kept, under its own name, so that the method-level
 * traceability of the migration stays one-to-one: the test inventory of this port joins each
 * row of its test mapping to a test of a named suite, so a renamed or omitted method is an
 * unmapped row rather than a tidier spec. The four parameterised methods of the original were
 * driven from a single provider, `data_name`; that shape is preserved - the provider becomes
 * one shared table, declared once below, and each of the four methods keeps its own test
 * driven from it rather than becoming nine tests of its own.
 *
 * ===What this family is===
 *
 * A price index is the smallest and the most regular of the four index families: nine
 * published members, nine constants covering them exactly, every one of them active and
 * published monthly, and - alone among the four - no alternate name at all. The reference
 * data behind the family does declare an alternate name section, but every entry of it was
 * commented out, so no alternate spelling ever resolved to an index. That is asserted
 * positively in `test_of_lookup_notFound` rather than left implicit, because the spellings in
 * question (`UK-RPI` and its siblings) are live names of a *different* type - they are
 * alternate spellings of a [[FloatingRateName]] - so their not resolving here is a fact worth
 * pinning rather than an absence nobody could observe.
 *
 * The regularity is also why this is the right spec to assert that the constants cover the
 * data completely; the same property is false of the Ibor, Overnight and exchange-rate
 * families, whose constants name a subset of their published rows.
 *
 * ===Methods asserting machinery this port does not have===
 *
 * Four of the twelve methods asserted machinery the port replaces rather than reproduces, so
 * each is ported as the assertion of the guarantee that machinery gave. None is dropped, and
 * the reasoning is repeated at each of them:
 *
 *  - `test_extendedEnum` read the run-time registry the family was published through. Under
 *    the closed-family design of this port (AAP §0.7 Rule 4) there is no registry to read, so
 *    the test asserts the closed-family equivalent of what the registry was consulted for.
 *  - `test_of_lookup_null` asserted that the factory rejected an absent name by raising an
 *    error. This port writes no such reference and its factories answer with a value, so the
 *    case becomes the two spellings of an absent name that can actually be supplied.
 *  - `coverage` called reflective sweeps over a Java bean and over the private constructor of
 *    its constants holder, and compared that bean against a custom-built index. No such
 *    helper exists here and, the family being closed, no custom index is representable; the
 *    properties those sweeps stood in for are asserted directly.
 *  - `test_jodaConvert` and `test_serialization` asserted round trips through the reflective
 *    string-conversion library and the serialization mechanism of the platform, neither of
 *    which this port depends on (AAP §0.2.2). Each is ported as the round trip that replaces
 *    it - text and JSON respectively - and the two are kept apart, since the representations
 *    they pin are independent of each other.
 *
 * Two of the obligations of this port are visible in how the failing cases are written. Text
 * naming no member is reported as a value and asserted through the outcome matchers, never
 * caught as an exception (AAP §0.7 Rule 5), and the reason is compared by value rather than
 * by matching a message, so the diagnostic wording stays free to change. No index is ever
 * constructed here: every value under test is reached through the family's own lookup or
 * through the constants that directory publishes (Rule 4).
 */
class PriceIndexSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The shared provider, transcribed row-for-row from the Java data provider.
   *
   * Each row pairs one of the nine published indices with the name it renders as and is
   * looked up by. The rows are in the order of the Java provider, which is also the
   * declaration order of the published index data: the three sterling indices, then the
   * Swiss, the two European, the Japanese, the United States and the French index.
   *
   * The rows are written out rather than derived from `PriceIndex.values`, deliberately: the
   * table and the family's own enumeration are then two independent statements of the same
   * nine members, so a mistake in either cannot be masked by the other. `test_extendedEnum`
   * is where the two are compared.
   */
  private val dataName: TableFor2[PriceIndex, String] = Table(
    ("index", "name"),
    (PriceIndices.GB_HICP, "GB-HICP"),
    (PriceIndices.GB_RPI, "GB-RPI"),
    (PriceIndices.GB_RPIX, "GB-RPIX"),
    (PriceIndices.CH_CPI, "CH-CPI"),
    (PriceIndices.EU_AI_CPI, "EU-AI-CPI"),
    (PriceIndices.EU_EXT_CPI, "EU-EXT-CPI"),
    (PriceIndices.JP_CPI_EXF, "JP-CPI-EXF"),
    (PriceIndices.US_CPI_U, "US-CPI-U"),
    (PriceIndices.FR_EXT_CPI, "FR-EXT-CPI")
  )

  //-------------------------------------------------------------------------
  test("test_gbpHicp") {
    // The index is reached by name rather than through its constant, as the Java method did:
    // what is under test is that the lookup answers with a fully populated index, so reading
    // the constant instead would assert less. Both entry points of the lookup are exercised -
    // `valueOf`, the exact lookup that answers with a value or nothing, and `parse`, which
    // reports text naming no member - so the two cannot drift apart.
    val test: PriceIndex =
      PriceIndex.valueOf("GB-HICP").getOrElse(fail("the price index family publishes no GB-HICP"))
    PriceIndex.parse("GB-HICP") should haveValue(test)

    test.name shouldBe "GB-HICP"
    test.currency shouldBe Currency.GBP
    test.region shouldBe Country.GB
    test.active shouldBe true
    test.publicationFrequency shouldBe Frequency.P1M
    // The floating rate family of a price index is its own name, whole and unaltered; the
    // comparison is against the value of that name in the FloatingRateName family, which is a
    // different type resolved from a different table even where the spelling coincides. The
    // Java accessor was total, so the family has to be published - an unpublished one is a
    // breach of this module's reference data - and the lookup that finds it here answers with
    // an option, so the expectation is written as the option holding that family.
    FloatingRateName.valueOf("GB-HICP") shouldBe Some(test.floatingRateName)
    test.toString shouldBe "GB-HICP"
  }

  test("test_getFloatingRateName") {
    // A family-wide invariant, asserted over every member rather than over a sample. The Java
    // method iterated the run-time registry of the family; under Rule 4 the closed enumeration
    // `PriceIndex.values` is that same membership, known at compile time.
    //
    // For a price index the floating rate family is the WHOLE index name: a price index
    // publishes one level rather than one figure per tenor, so there is no tenor suffix to
    // strip. This is the point at which the price family differs from the Ibor family, whose
    // own spec asserts the stripped form, and the two must not be conflated.
    val all: List[PriceIndex] = PriceIndex.values.toList
    all.foreach { index =>
      withClue(s"${index.name}: ") {
        FloatingRateName.valueOf(index.name) shouldBe Some(index.floatingRateName)
        index.floatingRateName.name shouldBe index.name
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_name") {
    forEvery(dataName) { (index: PriceIndex, name: String) =>
      index.name shouldBe name
    }
  }

  test("test_toString") {
    forEvery(dataName) { (index: PriceIndex, name: String) =>
      index.toString shouldBe name
    }
  }

  test("test_of_lookup") {
    // The Java factory both looked a name up exactly and reported an unknown one; the port
    // splits those into `valueOf`, the exact lookup, and `parse`, which reports the failure.
    // Both are asserted for every row, so the two entry points cannot drift apart.
    forEvery(dataName) { (index: PriceIndex, name: String) =>
      withClue(s"$name: ") {
        PriceIndex.valueOf(name) shouldBe Some(index)
        PriceIndex.parse(name) should haveValue(index)
      }
    }
  }

  test("test_extendedEnum") {
    // Ruling (AAP §0.4.1, §0.7 Rule 4): the Java method built the run-time registry of the
    // family and indexed it by name. No registry exists in this port - the family is a closed
    // sealed hierarchy whose members are created once from the published index data - so what
    // the registry was consulted for is asserted against that closed family instead: each
    // name of the provider resolves to the index the provider pairs it with, and the
    // enumeration of the family is exactly nine members with distinct names.
    //
    // Uniquely for this family, the constants cover the published data completely, so the name
    // set of the enumeration and the name set of the constants directory are asserted to be
    // the same set. That property is false of the Ibor, Overnight and exchange-rate families,
    // whose constants name a subset of their rows, which is why it is asserted here.
    //
    // This is complementary to two other specs and weakens neither: the fidelity of the nine
    // rows against the values captured from the Java implementation belongs to the module's
    // ReferenceDataManifestSpec, and the closedness of every named family to
    // NamedEnumClosedSpec. What is asserted here is which members this family has.
    forEvery(dataName) { (index: PriceIndex, name: String) =>
      withClue(s"$name: ") {
        PriceIndex.valueOf(name) shouldBe Some(index)
      }
    }

    val all: List[PriceIndex] = PriceIndex.values.toList
    all should have size 9
    all.distinct should have size 9
    all.map(_.name).distinct should have size 9

    val rows: List[(PriceIndex, String)] = dataName.toList
    val constants: List[PriceIndex] = rows.map { case (index, _) => index }
    val providedNames: List[String] = rows.map { case (_, name) => name }
    all.map(_.name).toSet shouldBe constants.map(_.name).toSet
    // the enumeration order is the declaration order of the published data, which is the order
    // of the provider, so the two agree as sequences and not merely as sets
    all.map(_.name) shouldBe providedNames
  }

  test("test_of_lookup_notFound") {
    // Where the Java factory raised an error for text naming no member, the port reports it as
    // a value (Rule 5). The reason is compared by value rather than by matching the message,
    // so the diagnostic wording of the failure stays free to change.
    PriceIndex.valueOf("Rubbish") shouldBe None
    PriceIndex.parse("Rubbish") should beFailureWith(FailureReason.PARSING)

    // The distinguishing fact about this family, asserted positively: it has no alternate name
    // table at all. The reference data declared one, but every entry of it was commented out,
    // so no alternate spelling resolved to an index in the implementation being ported, and
    // reviving one here would be new behaviour rather than a port of existing behaviour.
    //
    // These spellings are worth naming because they are not inert: `UK-RPI` and its siblings
    // are live alternate spellings of a FloatingRateName, so text that resolves in that family
    // still resolves to nothing in this one. Nor is the miss the work of a lenient rewrite
    // being absent - this family declares no rewrite either, so exact lookup and parse agree.
    val commentedOutAliases: List[String] =
      List("UK-HICP", "UK-RPI", "UK-RPIX", "SWF-CPI", "EUR-AI-CPI", "JPY-CPI-EXF", "USA-CPI-U")
    commentedOutAliases.foreach { alias =>
      withClue(s"$alias: ") {
        PriceIndex.valueOf(alias) shouldBe None
        PriceIndex.parse(alias) should beFailureWith(FailureReason.PARSING)
      }
    }
  }

  test("test_of_lookup_null") {
    // Reinterpretation: the Java method passed the absent reference to the factory and asserted
    // that it raised an error. This port writes no such reference and its factories take a name
    // they resolve as a value, so the case is asserted as the two spellings of an absent name
    // that can actually be supplied - the empty name and a blank one - each of which names no
    // member and so resolves to a parsing failure.
    PriceIndex.valueOf("") shouldBe None
    PriceIndex.parse("") should beFailureWith(FailureReason.PARSING)
    PriceIndex.valueOf("   ") shouldBe None
    PriceIndex.parse("   ") should beFailureWith(FailureReason.PARSING)
  }

  test("test_gb_rpi") {
    // The two day counts of a price index are fixed by the type rather than supplied by the
    // published data: the type answers the one-to-one convention, and the convention of a
    // conventionally swapped fixed leg defaults to the convention of the index itself, so both
    // are that one value. This is the only test that pins them, and it pins both.
    PriceIndices.GB_RPI.currency shouldBe Currency.GBP
    PriceIndices.GB_RPI.dayCount shouldBe DayCounts.ONE_ONE
    PriceIndices.GB_RPI.defaultFixedLegDayCount shouldBe DayCounts.ONE_ONE
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // Ruling - custom-instance substitution. The Java method did three things this port has no
    // target for: it swept the private constructor of the constants holder reflectively, it
    // swept the index as a Java bean, and it compared that index against one built by hand
    // through a builder, naming it "Test" with its own region, currency and publication
    // frequency.
    //
    // None of the three survives. The constants holder is a Scala `object`, which has no
    // constructor a caller could reach. No reflective bean sweep exists here - the testkit of
    // `strata-collect` publishes exactly five helpers, none of them reflective - because the
    // port derives nothing reflectively. And the hand-built index is unrepresentable on
    // purpose: under Rule 4 / AAP §0.3.3 kind [R] the family is closed to the nine configured
    // rows, its class is sealed with a constructor unavailable outside its own file, and there
    // is no builder, so an index named "Test" cannot exist.
    //
    // What those sweeps stood in for is asserted directly instead. First, the directory and
    // the family agree: the nine constants are distinct values and each one is the very value
    // the family answers with for its own name, through both entry points of the lookup.
    val constants: List[PriceIndex] = dataName.toList.map { case (index, _) => index }
    constants should have size 9
    constants.distinct should have size 9
    constants.foreach { index =>
      withClue(s"${index.name}: ") {
        PriceIndex.valueOf(index.name) shouldBe Some(index)
        PriceIndex.parse(index.name) should haveValue(index)
        Show[PriceIndex].show(index) shouldBe index.name
        index.toString shouldBe index.name
      }
    }

    // Second, the equality cover the bean sweeps provided, over two distinct *configured*
    // instances - the United States and the sterling retail index - which is what replaces the
    // comparison against the hand-built one. The companion publishes a single equality-bearing
    // implicit, an ordering that is also a hashing, so summoning the equality, the hashing or
    // the ordering yields that one value and the three can never disagree; the assertions
    // below are the observable form of that.
    val left: PriceIndex = PriceIndices.US_CPI_U
    val right: PriceIndex = PriceIndices.GB_RPI
    left should not be right
    Eq[PriceIndex].eqv(left, right) shouldBe false
    Hash[PriceIndex].eqv(left, right) shouldBe false
    Hash[PriceIndex].hash(left) should not be Hash[PriceIndex].hash(right)
    Show[PriceIndex].show(left) shouldBe "US-CPI-U"
    Show[PriceIndex].show(right) shouldBe "GB-RPI"
    Order[PriceIndex].compare(left, right) should not be 0

    // the ordering is consistent with equality over every ordered pair of the family: it
    // compares equal exactly when the two values are equal, and equal values hash alike
    val all: List[PriceIndex] = PriceIndex.values.toList
    for (first <- all; second <- all) {
      val sameValue = first == second
      withClue(s"${first.name} against ${second.name}: ") {
        Eq[PriceIndex].eqv(first, second) shouldBe sameValue
        Hash[PriceIndex].eqv(first, second) shouldBe sameValue
        (Order[PriceIndex].compare(first, second) == 0) shouldBe sameValue
        if (sameValue) {
          Hash[PriceIndex].hash(first) shouldBe Hash[PriceIndex].hash(second)
        } else {
          Order[PriceIndex].compare(first, second) should not be 0
        }
      }
    }
  }

  test("test_jodaConvert") {
    // The Java method asserted the round trip of the reflective string-conversion library the
    // original registered its two annotations with. That library is gone with the port (AAP
    // §0.2.2, §0.7 Rule 1), and the guarantee it gave is asserted directly: an index renders
    // as its name, and that rendering reads back as the same index.
    //
    // This is the text round trip. The JSON round trip is `test_serialization` below, and the
    // two are kept apart because the representations they pin are independent of each other -
    // the text form is what a caller writes and reads through `Show` and `parse`, the JSON
    // form is what the codec writes and reads.
    forEvery(dataName) { (index: PriceIndex, name: String) =>
      val rendered = Show[PriceIndex].show(index)
      withClue(s"$name: ") {
        rendered shouldBe name
        rendered shouldBe index.name
        PriceIndex.parse(rendered) should haveValue(index)
      }
    }
  }

  test("test_serialization") {
    // The Java method asserted a round trip through the serialization mechanism of the
    // platform, which this port does not support (AAP §0.2.2). Its replacement is the JSON
    // codec, and what this test pins is the shape that codec is required to have for this
    // family under AAP §0.6.4: an index is written as the bare string of its name and never as
    // an object, so a document naming an index reads back here as the same member.
    //
    // The property-based round trip over every codec-bearing type of the module belongs to the
    // consolidated json.JsonRoundTripSpec, which is where the test mapping routes the
    // traceability of this Java method; this test deliberately asserts the per-type
    // representation of this one family rather than repeating that sweep, so that neither the
    // shape nor the sweep can be lost with the other.
    forEvery(dataName) { (index: PriceIndex, name: String) =>
      val encoded = index.asJson
      withClue(s"$name: ") {
        encoded shouldBe Json.fromString(name)
        encoded.as[PriceIndex] shouldBe Right(index)
      }
    }
    // a string naming no member of the family is rejected by the reader rather than decoded
    // into some nearby index, and so is a structural form the encoder never writes
    Json.fromString("Rubbish").as[PriceIndex].isLeft shouldBe true
    Json.fromString("UK-RPI").as[PriceIndex].isLeft shouldBe true
    Json.obj("name" -> Json.fromString("US-CPI-U")).as[PriceIndex].isLeft shouldBe true
  }
}
