/*
 * Copyright (C) 2018 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import scala.collection.immutable.Set

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2

import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[Index]].
 *
 * Every method of the Java original is kept, under its own name, so that the method-level
 * traceability of the migration stays one-to-one; the test mapping manifest maps all six methods
 * of `IndexTest` to this spec. The three parameterised methods of the original were driven from
 * a single provider, `data_name`; that shape is preserved - the provider becomes the one shared
 * table declared below, and each of the three methods keeps its own test driven from it.
 *
 * ===What this spec owns===
 *
 * [[Index]] is the abstraction over every kind of index, and the only one of the three union
 * abstractions of this package that admits an exchange-rate index. What it owns, and what is
 * asserted here, is therefore the union itself: that text naming a member of any of the four
 * families resolves through the one entry point of the abstraction, and that the membership of
 * the union is exactly the membership of those four families and nothing besides.
 *
 * Two neighbouring concerns are deliberately left to the specs that own them. The sibling
 * abstractions exclude a family each - a rate index admits no price index, and a floating rate
 * index admits no exchange-rate index - and each asserts its own exclusion, which is why the
 * only text this spec asserts is unresolvable is text that names no index at all. And the
 * compile-level proof that no subtype of [[Index]] can be declared outside the file that
 * declares the abstraction belongs to the api-surface spec of the module, so it is not repeated
 * here.
 *
 * ===Two rulings, recorded where they apply===
 *
 * Two methods of the original asserted machinery this port does not have, so each is ported as
 * the assertion of the guarantee that machinery gave rather than dropped. The reasoning is
 * repeated at each of them: `test_of_lookup_null` becomes the absent name that this API can
 * actually be given, and `coverage` becomes the closed-union cover that the reflective sweep of
 * the constants holder stood in for - the holder itself was folded into the file declaring the
 * abstraction by the port, so there is no separate object left to sweep.
 *
 * ===Obligations of the request this spec witnesses===
 *
 * These are requirements of the user's original request, carried by AAP §0.7 / §0.8.1; the
 * project supplies no rules document (`review_rules` reports none), so enterprise practice
 * governs the rest.
 *
 *  - Rule 4, closed families with no runtime registry: [[Index]] is named in the closed-hierarchy
 *    list, and `coverage` is this spec's witness for it at the union level. Nothing here consults
 *    a registry or a classpath resource, and nothing here constructs an index. The cross-family
 *    sweep of the module's closed-enum spec and the data fidelity of its reference-data manifest
 *    spec are complementary to this and replace none of it.
 *  - Rule 5, explicit functional error handling: the three sites where the original asserted a
 *    raised error assert a reported failure instead, comparing the reason by value rather than by
 *    matching the diagnostic prose, and reading every outcome through the result matchers.
 *  - AAP §0.6.3, the search order: the union probes the four families in the order the combined
 *    lookup being ported declared - Ibor index, Overnight index, Price index, FX index - and
 *    `coverage` asserts both that the order holds and that today's data cannot observe it.
 *  - AAP §0.6.4, the JSON exclusion: the abstraction publishes no codec because each of its four
 *    families does; `coverage` asserts both halves of that.
 */
class IndexSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The shared provider, transcribed row-for-row from the Java data provider.
   *
   * Each row pairs an index with the name it renders as and is looked up by. The rows keep the
   * order and the four-family grouping of the provider - five Ibor indices, five Overnight
   * indices, three Price indices, four FX indices - because that grouping is the point of the
   * provider: it is what makes every one of the four families the union admits reachable in the
   * three tests driven from it.
   *
   * The first column is typed as the union rather than as the leaf families, so that every
   * assertion below is made about a value whose static type is [[Index]]. That is what keeps the
   * tests honest: a name resolved through the union is compared with a value held as a member of
   * the union, and neither side can quietly narrow to a family.
   */
  private val dataName: TableFor2[Index, String] = Table(
    ("index", "name"),
    (IborIndices.GBP_LIBOR_6M, "GBP-LIBOR-6M"),
    (IborIndices.CHF_LIBOR_6M, "CHF-LIBOR-6M"),
    (IborIndices.EUR_LIBOR_6M, "EUR-LIBOR-6M"),
    (IborIndices.JPY_LIBOR_6M, "JPY-LIBOR-6M"),
    (IborIndices.USD_LIBOR_6M, "USD-LIBOR-6M"),
    (OvernightIndices.GBP_SONIA, "GBP-SONIA"),
    (OvernightIndices.CHF_SARON, "CHF-SARON"),
    (OvernightIndices.EUR_EONIA, "EUR-EONIA"),
    (OvernightIndices.JPY_TONAR, "JPY-TONAR"),
    (OvernightIndices.USD_FED_FUND, "USD-FED-FUND"),
    (PriceIndices.GB_HICP, "GB-HICP"),
    (PriceIndices.CH_CPI, "CH-CPI"),
    (PriceIndices.EU_AI_CPI, "EU-AI-CPI"),
    (FxIndices.EUR_CHF_ECB, "EUR/CHF-ECB"),
    (FxIndices.EUR_GBP_ECB, "EUR/GBP-ECB"),
    (FxIndices.GBP_USD_WM, "GBP/USD-WM"),
    (FxIndices.USD_JPY_WM, "USD/JPY-WM")
  )

  //-------------------------------------------------------------------------
  test("test_name") {
    // The name is the identity of an index throughout the library, and it is declared by the
    // member rather than derived from it, so every member is asserted against the spelling the
    // provider of the original pins.
    forEvery(dataName) { (index: Index, name: String) =>
      index.name shouldBe name
    }
  }

  test("test_toString") {
    // The rendering of an index is its name and nothing else, which is what allows a name read
    // from text, a name written to text and a name used as a key to be the same string.
    forEvery(dataName) { (index: Index, name: String) =>
      index.toString shouldBe name
    }
  }

  test("test_of_lookup") {
    // The factory of the original both looked a name up exactly and reported an unknown one; the
    // port splits those into `valueOf`, the exact alias-aware lookup, and `parse`, which reports
    // the failure. Both are asserted, so the two entry points of the union cannot drift apart.
    //
    // This is the test that proves all four families are reachable through the union, the two
    // slash-bearing exchange-rate names among them. The outcome is held at the union type, so
    // what is asserted is that the union answers - not that some family did.
    forEvery(dataName) { (index: Index, name: String) =>
      withClue(s"$name: ") {
        val found: Either[Failure, Index] = Index.parse(name)
        found should haveValue(index)
        Index.valueOf(name) shouldBe Some(index)
      }
    }
  }

  test("test_of_lookup_notFound") {
    // Where the factory of the original raised an error for text naming no index, the port
    // reports it as a value. The reason is compared by value rather than by matching the
    // message, so the diagnostic wording of the failure stays free to change.
    //
    // Text that names no index at all is the only text the union rejects, because the union
    // admits every family; the sibling abstractions, which each exclude a family, assert their
    // own narrower rejections.
    Index.valueOf("Rubbish") shouldBe None
    Index.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
  }

  test("test_of_lookup_null") {
    // Reinterpretation: the Java method passed an absent reference to the factory and asserted
    // that it raised an error. This port writes no such reference and its lookups take a name
    // they resolve as a value, so the case is asserted as the two spellings of an absent name a
    // caller can actually supply - the empty name and a blank one - each of which names no index
    // and so resolves to a parsing failure rather than to a raised error.
    Index.valueOf("") shouldBe None
    Index.parse("") should beFailureWith(FailureReason.PARSING)
    Index.valueOf("   ") shouldBe None
    Index.parse("   ") should beFailureWith(FailureReason.PARSING)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // Ruling: the Java method swept the private constructor of the constants holder `Indices`
    // reflectively. The port folded that holder into the file declaring the abstraction, so
    // there is no separate object and no constructor left to sweep, and the port's test helper
    // publishes no reflective sweep to call. What the sweep stood in for - that this abstraction
    // is a closed union and that the union is exactly its four families - is asserted directly
    // below, which is the Rule 4 content of this spec.

    // The membership of the union is the membership of the four families, each of which is the
    // closed set of one row per row of its published reference data: 271 Ibor indices, 35
    // Overnight indices, 9 Price indices and 16 FX indices.
    val ibor: List[Index] = IborIndex.values.toList
    val overnight: List[Index] = OvernightIndex.values.toList
    val price: List[Index] = PriceIndex.values.toList
    val fx: List[Index] = FxIndex.values.toList
    ibor.size shouldBe 271
    overnight.size shouldBe 35
    price.size shouldBe 9
    fx.size shouldBe 16

    val union: List[Index] = ibor ::: overnight ::: price ::: fx
    union.size shouldBe (271 + 35 + 9 + 16)
    union.distinct.size shouldBe union.size

    // Every member of every family is reachable through the union entry points, and resolves to
    // itself rather than to another member sharing its name. The second assertion of the pair is
    // the stronger one and the reason the probe order of AAP §0.6.3 cannot be observed by any
    // name of today's data: exactly one of the four families answers each member's name, and it
    // answers with that very member. It is alias-aware, since each family is probed through its
    // own exact lookup, so an alternate spelling registered by one family is covered too.
    union.foreach { index =>
      withClue(s"${index.name}: ") {
        Index.valueOf(index.name) shouldBe Some(index)
        Index.parse(index.name) should haveValue(index)
        val answers: List[Index] =
          List(
            IborIndex.valueOf(index.name),
            OvernightIndex.valueOf(index.name),
            PriceIndex.valueOf(index.name),
            FxIndex.valueOf(index.name)
          ).flatten
        answers shouldBe List(index)
      }
    }

    // The same fact stated over the name spaces themselves, pairwise: no name is published by
    // two families. This is the fact that would break first if a later slice added a colliding
    // name, at which point the probe order becomes observable and this assertion is the one that
    // says so.
    val families: List[(String, List[Index])] =
      List("Ibor" -> ibor, "Overnight" -> overnight, "Price" -> price, "FX" -> fx)
    val nameSpaces: List[(String, Set[String])] =
      families.map { case (label, members) => (label, members.map(_.name).toSet) }
    families.zip(nameSpaces).foreach {
      case ((label, members), (_, names)) =>
        // within a family a name identifies a member, so the name space is as large as the
        // family; this is what makes the disjointness below a statement about all 331 members
        withClue(s"$label: ") {
          names.size shouldBe members.size
        }
    }
    for {
      (leftLabel, leftNames) <- nameSpaces
      (rightLabel, rightNames) <- nameSpaces
      if leftLabel != rightLabel
    } {
      withClue(s"$leftLabel against $rightLabel: ") {
        leftNames.intersect(rightNames) shouldBe Set.empty[String]
      }
    }

    // The probe order itself, asserted positively: text naming a member of each family resolves,
    // through the union, to a value of that family's type. The order reproduces the declaration
    // of the combined lookup being ported - Ibor, Overnight, Price, FX - and each case binds the
    // value it matched and asserts on it, so the match is an assertion rather than a type test
    // evaluated for its own sake.
    Index.parse("GBP-LIBOR-6M") match {
      case Right(found: IborIndex) => found shouldBe IborIndices.GBP_LIBOR_6M
      case other => fail(s"expected an Ibor index for 'GBP-LIBOR-6M', but was: $other")
    }
    Index.parse("GBP-SONIA") match {
      case Right(found: OvernightIndex) => found shouldBe OvernightIndices.GBP_SONIA
      case other => fail(s"expected an Overnight index for 'GBP-SONIA', but was: $other")
    }
    Index.parse("GB-HICP") match {
      case Right(found: PriceIndex) => found shouldBe PriceIndices.GB_HICP
      case other => fail(s"expected a Price index for 'GB-HICP', but was: $other")
    }
    Index.parse("EUR/CHF-ECB") match {
      case Right(found: FxIndex) => found shouldBe FxIndices.EUR_CHF_ECB
      case other => fail(s"expected an FX index for 'EUR/CHF-ECB', but was: $other")
    }

    // The JSON exclusion of AAP §0.6.4, asserted as the compile-level fact it is: this
    // abstraction carries no data and publishes no codec, so a reader or writer for it cannot be
    // summoned. The exclusion is deliberate rather than an omission, which is the other half of
    // the assertion: each of the four families publishes a codec of its own that writes a member
    // as the bare string of its name, and those four are what every document of this module
    // actually contains. The property-based round trip over every codec-bearing type belongs to
    // the module's consolidated JSON spec; what is pinned here is only that the union is absent
    // from it by design and that its families are present.
    //
    // The two halves are stated in this order on purpose: the four summons below compile, which
    // is what makes the two that do not compile meaningful - they fail for want of an instance
    // for the abstraction and not because the library naming them is unreachable from here.
    assertDoesNotCompile("implicitly[io.circe.Encoder[Index]]")
    assertDoesNotCompile("implicitly[io.circe.Decoder[Index]]")
    val iborCodec: io.circe.Codec[IborIndex] = implicitly
    val overnightCodec: io.circe.Codec[OvernightIndex] = implicitly
    val priceCodec: io.circe.Codec[PriceIndex] = implicitly
    val fxCodec: io.circe.Codec[FxIndex] = implicitly
    iborCodec(IborIndices.GBP_LIBOR_6M).asString shouldBe Some("GBP-LIBOR-6M")
    overnightCodec(OvernightIndices.GBP_SONIA).asString shouldBe Some("GBP-SONIA")
    priceCodec(PriceIndices.GB_HICP).asString shouldBe Some("GB-HICP")
    fxCodec(FxIndices.EUR_CHF_ECB).asString shouldBe Some("EUR/CHF-ECB")
  }
}
