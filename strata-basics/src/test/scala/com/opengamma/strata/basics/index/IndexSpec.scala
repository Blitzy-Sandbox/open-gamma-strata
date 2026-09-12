/*
 * Copyright (C) 2018 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import java.util.concurrent.atomic.AtomicReference

import scala.collection.immutable.Set
import scala.collection.mutable.ListBuffer

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor1
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
 *
 * ===The sibling union [[FloatingRate]], folded in===
 *
 * A final section, after the ported methods and marked as such, asserts what the sibling union
 * abstraction [[FloatingRate]] owns by itself. It is asserted in this spec because the Java
 * sources have no test class for that abstraction - the union there is exercised through the
 * specs of its consumers - so the test inventory of AAP §0.3.1 lists no spec for it, and this
 * spec is the union spec of the package: three of the four families [[FloatingRate]] probes are
 * families of this union, and the fourth is the published name family.
 *
 * Three specs already own parts of that abstraction and none of them is repeated here: the
 * search rule over supplied probes, and the wording of the failure text naming nothing
 * produces, belong to `FloatingRateTypeSpec`; the membership and the observable order of the
 * union over the real members of the four families belong to `NamedEnumClosedSpec`; and the
 * openness of the trait, witnessed by an implementation declared outside the families, belongs
 * to the module's `ApiSurfaceSpec`. The reasoning is repeated at the section, and its tests are
 * named `floatingRate_*` rather than after a Java method, because
 * `manifest/java-test-mapping.csv` maps no Java method to any of them.
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

    //-----------------------------------------------------------------------------------
    // The search rule of the sibling abstraction [[FloatingRate]], asserted inside this
    // mapped test rather than as a test of its own: the Java sources carry no test class
    // for `FloatingRate`, so the inventory of this port maps no spec to it and no ledger
    // row would join a test name added for it, while this spec owns the union rule of the
    // package (AAP §0.6.3). Only what the search owns by itself is asserted, which needs
    // no family to have members - every assertion is made over probes this test supplies,
    // never over a family's membership, which each family's own spec owns.

    // A probe records its application, newest last; an AtomicReference over an immutable
    // List keeps that record free of any mutable collection and of a var.
    def recorder(): AtomicReference[List[String]] = new AtomicReference[List[String]](List.empty)
    def probe(
        applied: AtomicReference[List[String]],
        label: String,
        hits: Set[String]): String => Option[String] =
      name => {
        // bound to a wildcard because the new record is read through the reference below
        val _ = applied.updateAndGet(recorded => recorded :+ label)
        if (hits.contains(name)) Some(s"$label:$name") else None
      }
    // A composition in the shape of the shipped one - elements supplied by name - whose
    // first two probes both claim "shared", so that precedence is observable. The third
    // element is the caller's, so a case can put either a probe or something whose mere
    // production fails the test in the position after the answering probe.
    def probes(
        applied: AtomicReference[List[String]],
        beyond: => (String => Option[String])): LazyList[String => Option[String]] =
      probe(applied, "first", Set("a", "shared")) #:: probe(applied, "second", Set("b", "shared")) #::
        beyond #:: LazyList.empty[String => Option[String]]

    // The probes are tried in the order supplied and the search stops at the first hit: the
    // probe after the one that answered is neither applied nor, in a LazyList, even produced.
    // Producing it fails this test, which is what makes the laziness that keeps a search from
    // loading a family it never consults an assertion rather than a claim.
    val stopped = recorder()
    FloatingRate.tryParseWith("b", probes(stopped, fail("a probe beyond the hit was produced"))) shouldBe
      Some("second:b")
    stopped.get() shouldBe List("first", "second")
    // where two probes claim the same text the earlier one answers and the later is unused
    val precedence = recorder()
    FloatingRate.tryParseWith(
      "shared",
      probes(precedence, fail("a probe beyond the hit was produced"))) shouldBe Some("first:shared")
    precedence.get() shouldBe List("first")
    // on a miss every probe is applied exactly once, in the order supplied
    val missed = recorder()
    FloatingRate.tryParseWith("Rubbish", probes(missed, probe(missed, "third", Set("c")))) shouldBe None
    missed.get() shouldBe List("first", "second", "third")
    // the search is total in the composition it is handed, including the empty one, of
    // either kind of sequence
    FloatingRate.tryParseWith("a", LazyList.empty[FloatingRate.Lookup]) shouldBe None
    FloatingRate.tryParseWith("a", List.empty[FloatingRate.Lookup]) shouldBe None
    // a caller need not compose lazily: a strict List is searched in the order given and
    // still stops at the first hit, proven by a second element that fails this test if applied
    val strictly = recorder()
    val strict: List[String => Option[String]] =
      List(probe(strictly, "only", Set("a")), (_: String) => fail("a strict search passed the hit"))
    FloatingRate.tryParseWith("a", strict) shouldBe Some("only:a")
    strictly.get() shouldBe List("only")

    // The composition the library ships is four probes, each an exact lookup, so text naming
    // no member is answered by every one of them with nothing rather than by a lenient
    // rewrite. Which family occupies which position is a property of the families and is
    // asserted with them; here it is the count, the exactness, and that a subset composes.
    FloatingRate.standardLookups.size shouldBe 4
    FloatingRate.standardLookups.zipWithIndex.foreach { case (lookup, position) =>
      withClue(s"probe $position: ") {
        lookup("Rubbish") shouldBe None
      }
    }
    FloatingRate.tryParseWith("Rubbish", FloatingRate.standardLookups.take(3)) shouldBe None

    // Which probe sits at which position, asserted here because this union is the one whose
    // families really do share names, so its order is a property of the shipped composition that
    // the data can observe rather than one only the specification states. One member of each
    // family is named - which is not an assertion about the membership of any family, but about
    // the position of its probe:
    val probeList = FloatingRate.standardLookups.toList
    probeList.map(probe => probe("GBP-LIBOR-3M").isDefined) shouldBe List(true, false, false, false)
    probeList.map(probe => probe("GBP-SONIA").isDefined) shouldBe List(false, true, false, true)
    probeList.map(probe => probe("GB-RPI").isDefined) shouldBe List(false, false, true, true)
    probeList.map(probe => probe("GBP-LIBOR-BBA").isDefined) shouldBe List(false, false, false, true)

    // The third and fourth rows above are the collisions: every Overnight and every price index
    // name is also published as a floating rate name, so the last probe claims them too and the
    // order decides which value a caller gets. It gets the index, because the index families are
    // probed first - stated over all 44 colliding names rather than over the two samples, and
    // stated as the type of the value, which is what a permutation of the composition would
    // change.
    val collidingNames: List[String] =
      (OvernightIndex.values.toList.map(_.name) ::: PriceIndex.values.toList.map(_.name))
        .filter(name => FloatingRateName.valueOf(name).isDefined)
    collidingNames should have size 44
    collidingNames.foreach { name =>
      withClue(s"$name: ") {
        probeList.last(name).isDefined shouldBe true
        FloatingRate.tryParse(name) shouldBe Index.valueOf(name)
        FloatingRate.tryParse(name).exists {
          case _: FloatingRateIndex => true
          case _ => false
        } shouldBe true
      }
    }

    // and the entry points search that composition and nothing else, for a name of each family,
    // for a colliding name and for text naming none
    (collidingNames ::: List("GBP-LIBOR-3M", "GBP-LIBOR-BBA", "Rubbish", "")).foreach { name =>
      withClue(s"$name: ") {
        FloatingRate.tryParse(name) shouldBe
          FloatingRate.tryParseWith(name, FloatingRate.standardLookups)
      }
    }

    // The two entry points differ only in how they report text naming nothing: the interface
    // being ported raised an error from `parse`, whose message is now the failure's message,
    // unchanged, so the wording is asserted alongside the reason.
    FloatingRate.tryParse("Rubbish") shouldBe None
    FloatingRate.parse("Rubbish") match {
      case Left(failure) =>
        failure.reason shouldBe FailureReason.PARSING
        failure.message shouldBe "Floating rate index not known: Rubbish"
      case Right(floatingRate) => fail(s"expected a parsing failure, found $floatingRate")
    }
    // end of the relocated FloatingRate union coverage
  }

  //-------------------------------------------------------------------------
  // The sibling union abstraction, `FloatingRate`, is asserted below. It is asserted in this spec
  // because the Java sources have no test class for it - the union there is exercised through the
  // specs of its consumers, which are ported together with the index families those specs assert
  // against - so the test inventory of this port maps no spec to it, and this spec is the union
  // spec of the package.
  //
  // What is asserted is what that abstraction owns by itself and what no other spec states:
  //
  //  - the contract of the trait, held at the trait type over one member of each of its four
  //    implementor kinds, so that `name` and `floatingRateName` are asserted to agree across the
  //    kinds rather than family by family;
  //  - the agreement of the three entry points with each other, so `parse`, `tryParse` and
  //    `tryParseWith` applied to the shipped composition can never answer differently;
  //  - the totality of the search in its input, over the hostile shapes a caller can arrive with;
  //  - the shape of the shipped composition - four probes, each of them a family's own exact
  //    lookup, in the documented order - asserted by comparing each probe with the family lookup
  //    it is required to be, and the laziness that keeps a family's companion from being loaded
  //    by a search that never consults it;
  //  - the search being stated over the value type of the probes rather than over this trait.
  //
  // Three neighbouring concerns are deliberately left to the specs that own them, and nothing
  // below weakens or repeats them: the search rule over supplied probes and the wording of the
  // failure for text naming nothing are `FloatingRateTypeSpec`'s; the membership and the
  // observable order of the union over every real member of the four families are
  // `NamedEnumClosedSpec`'s; and the openness of the trait, witnessed by an implementation
  // declared outside the families, is `ApiSurfaceSpec`'s, which is why no such implementation is
  // declared here.
  //-------------------------------------------------------------------------
  /**
   * Text the union search is applied to: names of every shape the families use, names that are
   * not, and the hostile shapes a caller can arrive with.
   *
   * Every assertion that reads this table holds whatever the families answer for a given row,
   * because no assertion below reads a family's membership - which family answers a given piece
   * of text is a property of the families and is asserted with them.
   */
  private val floatingRateSampleText: TableFor1[String] =
    Table(
      "text",
      "GBP-LIBOR-3M",
      "GBP-LIBOR-BBA",
      "GBP-SONIA",
      "EUR-ESTER",
      "EUR-ESTR",
      "USD-SOFR",
      "GB-RPI",
      "US-CPI-U",
      "EUR/USD-ECB",
      "Rubbish",
      "",
      "   ",
      "gbp-libor-3m",
      "GBP-LIBOR-3M ",
      "\u0000",
      "\ud83d\ude00",
      "' OR 1=1 --")

  //-------------------------------------------------------------------------
  test("floatingRate_traitContractOverItsImplementorKinds") {
    // The trait asks for two things: the name, which it inherits from being a named value, and
    // the family the rate belongs to. Every value reaching a caller is a member of one of four
    // implementor kinds - the three rate index families of this union and the published name
    // family - so one member of each is held at the trait type here and both members of the
    // contract are asserted over it. Stating it at the trait type is what this test adds to the
    // per-family sweeps: those assert that each family agrees with the name family, this asserts
    // that the four agree with one another through the abstraction they share.
    val ibor: FloatingRate = IborIndices.GBP_LIBOR_6M
    val overnight: FloatingRate = OvernightIndices.GBP_SONIA
    val price: FloatingRate = PriceIndices.GB_HICP
    val family: FloatingRate = FloatingRateNames.GBP_LIBOR

    ibor.name shouldBe "GBP-LIBOR-6M"
    overnight.name shouldBe "GBP-SONIA"
    price.name shouldBe "GB-HICP"
    family.name shouldBe "GBP-LIBOR"

    // An index reports the family it belongs to, which drops an Ibor index's tenor - the
    // six-month and the three-month index report the same family - and is the whole name of an
    // Overnight or a price index; a family reports itself.
    ibor.floatingRateName shouldBe FloatingRateNames.GBP_LIBOR
    (IborIndices.GBP_LIBOR_3M: FloatingRate).floatingRateName shouldBe ibor.floatingRateName
    overnight.floatingRateName.name shouldBe overnight.name
    price.floatingRateName.name shouldBe price.name
    family.floatingRateName shouldBe family

    // Stated over all four kinds at once: the family a floating rate reports is a published
    // member of the name family, and the accessor is idempotent, which is what lets a caller
    // apply it without knowing which kind it holds.
    List(ibor, overnight, price, family).foreach { rate =>
      withClue(s"${rate.name}: ") {
        val reported = rate.floatingRateName
        FloatingRateName.valueOf(reported.name) shouldBe Some(reported)
        reported.floatingRateName shouldBe reported
      }
    }

    // And a value of this trait reaches a caller through the union search, which answers with the
    // very member held above - including, for the last of them, a family identifier that names no
    // index at all and so can only have come from the fourth probe.
    FloatingRate.tryParse("GBP-LIBOR-6M") shouldBe Some(ibor)
    FloatingRate.tryParse("GBP-SONIA") shouldBe Some(overnight)
    FloatingRate.parse("GB-HICP") should haveValue(price)
    FloatingRate.parse("GBP-LIBOR") should haveValue(family)
  }

  test("floatingRate_entryPointsAgreeWithEachOther") {
    // The three entry points are one search reported three ways, so they can never disagree:
    // `tryParse` is the search applied to the shipped composition, and `parse` is `tryParse` with
    // the absent case turned into a failure. Which family answers a given piece of text is not
    // asserted here - it is a property of the families - only that the three answers are one
    // answer, over text of every shape the families use and several they do not.
    forEvery(floatingRateSampleText) { (text: String) =>
      withClue(s"'$text': ") {
        val found = FloatingRate.tryParse(text)
        found shouldBe FloatingRate.tryParseWith(text, FloatingRate.standardLookups)
        FloatingRate.parse(text).toOption shouldBe found
        FloatingRate.parse(text).isRight shouldBe found.isDefined
      }
    }
  }

  test("floatingRate_hostileTextIsAnsweredNotRaised") {
    // The search is total in its input. Every shape below is text a caller can arrive with - the
    // empty and the blank name, a control character, a line break, a tab, a surrogate pair, an
    // injection payload, a path traversal, format specifiers, and a payload long enough to matter
    // - and each is answered, with a value or with a failure, rather than raising. The reason of
    // the failure is asserted; the prose of its message is diagnostic rather than contract and is
    // pinned where it is owned, in `FloatingRateTypeSpec`.
    val hostile: List[String] =
      List(
        "",
        "   ",
        "\u0000",
        "\n",
        "\t",
        "\ud83d\ude00",
        "' OR 1=1 --",
        "../../etc/passwd",
        "%s%n{}",
        "A" * 10000)

    hostile.foreach { text =>
      withClue(s"length ${text.length}: ") {
        val found = FloatingRate.tryParse(text)
        FloatingRate.parse(text).toOption shouldBe found
        if (found.isEmpty) {
          FloatingRate.parse(text).left.map(_.reason) shouldBe Left(FailureReason.PARSING)
        }
      }
    }
  }

  //-------------------------------------------------------------------------
  test("floatingRate_standardLookupsAreTheFourFamilyProbesInOrder") {
    // The shipped composition is four probes, and each is required to be one family's own exact
    // lookup, in the documented order: Ibor index, Overnight index, Price index, then floating
    // rate name. That requirement is asserted as an identity between each probe and the family
    // lookup it has to be, over every piece of text of the table above, which pins the order
    // without reading any family's membership - `NamedEnumClosedSpec` pins the same order by
    // reading it, and the two are complementary. That each probe is an exact lookup rather than a
    // lenient one, and that there are four of them, is asserted in `FloatingRateTypeSpec`.
    val probes = FloatingRate.standardLookups.toList
    probes should have size 4

    forEvery(floatingRateSampleText) { (text: String) =>
      withClue(s"'$text': ") {
        probes.head.apply(text) shouldBe IborIndex.valueOf(text)
        probes(1).apply(text) shouldBe OvernightIndex.valueOf(text)
        probes(2).apply(text) shouldBe PriceIndex.valueOf(text)
        probes(3).apply(text) shouldBe FloatingRateName.valueOf(text)
      }
    }
  }

  test("floatingRate_standardLookupsAreProducedFreshlyOnEveryCall") {
    // The composition has to stay a method whose elements are supplied by name. A value holding
    // the four lookups pre-assembled would force all four companions the first time anything
    // parsed, and the families and that object depend on each other, so it would reintroduce an
    // initialization cycle whose symptom depends on which class a program happens to touch first.
    // Being a method is observable, and asserted here, because the property is easy to lose in a
    // refactor and expensive to diagnose afterwards.
    val first = FloatingRate.standardLookups
    val second = FloatingRate.standardLookups
    first should not be theSameInstanceAs(second)
    first should have size 4
    second should have size 4

    // a subset composes, which is how a caller searches fewer families without reimplementing the
    // search: the three index families of this union, without the published name family behind
    // them, answer exactly as a composition a caller writes out does
    FloatingRate.standardLookups.take(3) should have size 3
    val indexFamiliesOnly: List[FloatingRate.Lookup] =
      List(
        (name: String) => IborIndex.valueOf(name),
        (name: String) => OvernightIndex.valueOf(name),
        (name: String) => PriceIndex.valueOf(name))
    FloatingRate.tryParseWith("GBP-LIBOR-3M", FloatingRate.standardLookups.take(3)) shouldBe
      FloatingRate.tryParseWith("GBP-LIBOR-3M", indexFamiliesOnly)
    FloatingRate.tryParseWith("GBP-LIBOR-BBA", FloatingRate.standardLookups.take(3)) shouldBe
      FloatingRate.tryParseWith("GBP-LIBOR-BBA", indexFamiliesOnly)
  }

  test("floatingRate_searchIsStatedOverAnyValueAProbeAnswersWith") {
    // The search rule has nothing to do with what a floating rate is, and the operation says so:
    // it is stated over the value type of the probes, which is what lets it be read and tested
    // with no family involved, and what serves a caller composing probes over its own type.
    val numeric: List[String => Option[Int]] =
      List(text => Option.when(text == "one")(1), text => Option.when(text.nonEmpty)(text.length))
    FloatingRate.tryParseWith("one", numeric) shouldBe Some(1)
    FloatingRate.tryParseWith("four", numeric) shouldBe Some(4)
    FloatingRate.tryParseWith("", numeric) shouldBe None

    // and it is total in the composition it is given, whatever ordered sequence that is: the
    // empty lazy and strict compositions are asserted in `FloatingRateTypeSpec`, the third shape
    // a caller reaches for here
    FloatingRate.tryParseWith("a", Vector.empty[FloatingRate.Lookup]) shouldBe None
  }

  //-------------------------------------------------------------------------
  // The order of the union, as a property of the shipped composition rather than of the data.
  //
  // The `coverage` test above establishes that the four name spaces are disjoint today, and that
  // is exactly why membership assertions cannot pin the probe order: with no name claimed by two
  // families, any permutation of the four probes answers every name of today's data identically.
  // The order is nonetheless part of the contract - AAP §0.6.3 fixes it as the order of the
  // `[types]` declaration the combined lookup being ported consumed - so it is asserted here
  // against `Index.standardLookups`, the value the production lookup searches, in three parts: the
  // composition holds the four families in that order, the entry points really do search that
  // composition, and the search itself answers with the first probe to match and reaches no
  // further. The last part uses probes this spec supplies, since a collision cannot be built from
  // the published data.
  //-------------------------------------------------------------------------
  test("test_standardLookups_probeOrder") {
    val probes: List[Index.Lookup] = Index.standardLookups.toList
    probes should have size 4

    // Positional identity. Each row names a member of one family and states which probe claims
    // it: exactly one does, and it is the one at that family's position. Swapping any two probes
    // of the production composition moves a `true` and fails this assertion, which is what makes
    // the order observable rather than merely documented.
    val perFamilySamples: TableFor2[String, Int] =
      Table(
        ("name", "position"),
        ("GBP-LIBOR-3M", 0),
        ("GBP-SONIA", 1),
        ("GB-RPI", 2),
        ("EUR/USD-ECB", 3))
    forEvery(perFamilySamples) { (name: String, position: Int) =>
      withClue(s"$name: ") {
        probes.map(probe => probe(name).isDefined) shouldBe List.tabulate(4)(_ == position)
        probes(position)(name) shouldBe Index.valueOf(name)
      }
    }

    // Every probe is an exact lookup, so text naming no member is answered by all four with
    // nothing rather than rewritten by one of them into a name another family publishes.
    probes.zipWithIndex.foreach {
      case (probe, position) => withClue(s"probe $position: ")(probe("Rubbish") shouldBe None)
    }

    // The entry points search that composition and nothing else: the answer of the production
    // lookup and the answer of the search over the shipped composition agree for every published
    // name and for text naming none. A hand-rolled chain inside `valueOf` that drifted from the
    // composition asserted above would show up here.
    val everyName: List[String] =
      (IborIndex.values.toList ::: OvernightIndex.values.toList :::
        PriceIndex.values.toList ::: FxIndex.values.toList).map(_.name)
    (everyName ::: List("Rubbish", "", "   ")).foreach { name =>
      withClue(s"$name: ") {
        Index.valueOf(name) shouldBe Index.firstMatch(name, Index.standardLookups)
      }
    }
  }

  test("test_standardLookups_firstMatchWins") {
    // The search rule the union is built on, with a collision the published data has no case of:
    // two probes claim one piece of text and the earlier answers. The probes record their own
    // application, so the assertion is not only which value came back but that the second probe
    // was never reached - the property that keeps a family's companion from being initialised by
    // a search that stops before it.
    val probed = ListBuffer.empty[String]
    def probe(label: String, hits: Set[String]): String => Option[String] =
      name => {
        probed += label
        if (hits.contains(name)) Some(s"$label:$name") else None
      }
    val composition = List(probe("first", Set("a", "shared")), probe("second", Set("b", "shared")))

    Index.firstMatch("shared", composition) shouldBe Some("first:shared")
    probed.toList shouldBe List("first")

    probed.clear()
    Index.firstMatch("b", composition) shouldBe Some("second:b")
    probed.toList shouldBe List("first", "second")

    probed.clear()
    Index.firstMatch("c", composition) shouldBe None
    probed.toList shouldBe List("first", "second")

    // and the search is total in the composition it is given, including the empty one
    Index.firstMatch("a", List.empty[Index.Lookup]) shouldBe None
  }
}
