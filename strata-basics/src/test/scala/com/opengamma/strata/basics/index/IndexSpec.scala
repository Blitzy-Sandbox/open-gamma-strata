/*
 * Copyright (C) 2018 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference

import scala.collection.immutable.Set
import scala.collection.mutable.ListBuffer

import org.scalatest.exceptions.TestFailedException
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
 * [[Index]] is the only one of the three union abstractions of this package that admits an
 * exchange-rate index, and its membership is exactly the four families it unions - 271 Ibor, 35
 * Overnight, 9 price and 16 exchange-rate indices, 331 distinct members - whose name spaces are
 * disjoint today, so no membership assertion can observe the declared probe order Ibor index,
 * Overnight index, price index, exchange-rate index; that order is asserted positionally over
 * `Index.standardLookups`, in `test_standardLookups_probeOrder`. The sibling unions exclude a
 * family each and assert their own exclusion, so the only text asserted unresolvable here is text
 * naming no index at all. A final section asserts what the sibling union [[FloatingRate]] owns.
 */
class IndexSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The shared table the name, rendering and lookup tests are driven from, pairing each index with
   * the name it renders as and is looked up by. The rows are written out rather than derived from
   * the families, which fixes the grouping and keeps all four families of the union reachable in
   * the three tests driven from it; the first column is typed as the union rather than as the leaf
   * families, so every assertion made from it is about a value of static type [[Index]].
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
    forEvery(dataName) { (index: Index, name: String) =>
      index.name shouldBe name
    }
  }

  test("test_toString") {
    forEvery(dataName) { (index: Index, name: String) =>
      index.toString shouldBe name
    }
  }

  test("test_of_lookup") {
    // `valueOf` is the exact lookup and `parse` reports the name it cannot resolve; both are
    // asserted at the union type, over names of all four families, slash-bearing ones included.
    forEvery(dataName) { (index: Index, name: String) =>
      withClue(s"$name: ") {
        val found: Either[Failure, Index] = Index.parse(name)
        found should haveValue(index)
        Index.valueOf(name) shouldBe Some(index)
      }
    }
  }

  test("test_of_lookup_notFound") {
    // the reason is compared by value, so the diagnostic wording of the failure is free to change
    Index.valueOf("Rubbish") shouldBe None
    Index.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
  }

  test("test_of_lookup_null") {
    // the absent name a caller can supply is the empty and the blank one, each a parsing failure
    Index.valueOf("") shouldBe None
    Index.parse("") should beFailureWith(FailureReason.PARSING)
    Index.valueOf("   ") shouldBe None
    Index.parse("   ") should beFailureWith(FailureReason.PARSING)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // the union's membership is the four families, each closed over its published reference data
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

    // Every published name of the four families resolves through the union to that very member,
    // and exactly one family answers each name - which is why no name of today's data can observe
    // the probe order. The names probed are the canonical ones; alternate spellings are exercised
    // where they are declared, in `NamedEnumClosedSpec`'s sweep of the declared alias rows.
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

    // the same fact over the name spaces, pairwise: no name is published by two families
    val families: List[(String, List[Index])] =
      List("Ibor" -> ibor, "Overnight" -> overnight, "Price" -> price, "FX" -> fx)
    val nameSpaces: List[(String, Set[String])] =
      families.map { case (label, members) => (label, members.map(_.name).toSet) }
    families.zip(nameSpaces).foreach {
      case ((label, members), (_, names)) =>
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

    // Reachability and routing: text naming a member of each family resolves, through the union,
    // to a value of that family's type. With the name spaces disjoint any permutation of the
    // probes answers these four names identically, so the order is not established here - it is
    // asserted positionally over `Index.standardLookups`, in `test_standardLookups_probeOrder`.
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

    // The abstraction carries no data and publishes no codec, so a reader or writer for it cannot
    // be summoned, while each of its four families publishes one that writes a member as the bare
    // string of its name. The four summons that compile are what make the two that do not
    // meaningful: they fail for want of an instance, not because the library is unreachable here.
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
    // The search rule of the sibling abstraction `FloatingRate`, over probes this test supplies:
    // tried in the order given, stopping at the first hit, with the probe after the answering one
    // never applied and, in a LazyList, never produced - producing it fails this test. Then the
    // shipped composition: four exact probes, the position each family occupies, and the 44 names
    // an index and a published name share, which the index wins. A probe records its application
    // in an AtomicReference over an immutable List, so no var is needed.
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
    // a composition shaped like the shipped one, whose first two probes both claim "shared" and
    // whose third element is the caller's, to sit after the answering probe
    def probes(
        applied: AtomicReference[List[String]],
        beyond: => (String => Option[String])): LazyList[String => Option[String]] =
      probe(applied, "first", Set("a", "shared")) #:: probe(applied, "second", Set("b", "shared")) #::
        beyond #:: LazyList.empty[String => Option[String]]

    val stopped = recorder()
    FloatingRate.tryParseWith("b", probes(stopped, fail("a probe beyond the hit was produced"))) shouldBe
      Some("second:b")
    stopped.get() shouldBe List("first", "second")
    val precedence = recorder()
    FloatingRate.tryParseWith(
      "shared",
      probes(precedence, fail("a probe beyond the hit was produced"))) shouldBe Some("first:shared")
    precedence.get() shouldBe List("first")
    val missed = recorder()
    FloatingRate.tryParseWith("Rubbish", probes(missed, probe(missed, "third", Set("c")))) shouldBe None
    missed.get() shouldBe List("first", "second", "third")
    FloatingRate.tryParseWith("a", LazyList.empty[FloatingRate.Lookup]) shouldBe None
    FloatingRate.tryParseWith("a", List.empty[FloatingRate.Lookup]) shouldBe None
    val strictly = recorder()
    val strict: List[String => Option[String]] =
      List(probe(strictly, "only", Set("a")), (_: String) => fail("a strict search passed the hit"))
    FloatingRate.tryParseWith("a", strict) shouldBe Some("only:a")
    strictly.get() shouldBe List("only")

    FloatingRate.standardLookups.size shouldBe 4
    FloatingRate.standardLookups.zipWithIndex.foreach { case (lookup, position) =>
      withClue(s"probe $position: ") {
        lookup("Rubbish") shouldBe None
      }
    }
    FloatingRate.tryParseWith("Rubbish", FloatingRate.standardLookups.take(3)) shouldBe None

    val probeList = FloatingRate.standardLookups.toList
    probeList.map(probe => probe("GBP-LIBOR-3M").isDefined) shouldBe List(true, false, false, false)
    probeList.map(probe => probe("GBP-SONIA").isDefined) shouldBe List(false, true, false, true)
    probeList.map(probe => probe("GB-RPI").isDefined) shouldBe List(false, false, true, true)
    probeList.map(probe => probe("GBP-LIBOR-BBA").isDefined) shouldBe List(false, false, false, true)

    // The collisions: every Overnight and every price index name is also published as a floating
    // rate name, so the last probe claims them too - stated over all 44, and as the value's type.
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

    (collidingNames ::: List("GBP-LIBOR-3M", "GBP-LIBOR-BBA", "Rubbish", "")).foreach { name =>
      withClue(s"$name: ") {
        FloatingRate.tryParse(name) shouldBe
          FloatingRate.tryParseWith(name, FloatingRate.standardLookups)
      }
    }

    // `parse` reports text naming nothing as a failure whose wording is asserted with its reason
    FloatingRate.tryParse("Rubbish") shouldBe None
    FloatingRate.parse("Rubbish") match {
      case Left(failure) =>
        failure.reason shouldBe FailureReason.PARSING
        failure.message shouldBe "Floating rate index not known: Rubbish"
      case Right(floatingRate) => fail(s"expected a parsing failure, found $floatingRate")
    }
  }

  //-------------------------------------------------------------------------
  // What the sibling union `FloatingRate` owns by itself is asserted below.
  //-------------------------------------------------------------------------
  /**
   * Text the union search is applied to: names of every shape the families use, names that are
   * not, and the hostile shapes a caller can arrive with. Every assertion reading this table
   * holds whatever the families answer for a row, since none of them reads a family's membership.
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
    // The trait asks for a name and for the family the rate belongs to. It is open, and four
    // implementations are built in - the three rate index families of this union and the published
    // name family - so one member of each is held at the trait type and both are asserted there.
    val ibor: FloatingRate = IborIndices.GBP_LIBOR_6M
    val overnight: FloatingRate = OvernightIndices.GBP_SONIA
    val price: FloatingRate = PriceIndices.GB_HICP
    val family: FloatingRate = FloatingRateNames.GBP_LIBOR

    ibor.name shouldBe "GBP-LIBOR-6M"
    overnight.name shouldBe "GBP-SONIA"
    price.name shouldBe "GB-HICP"
    family.name shouldBe "GBP-LIBOR"

    // the reported family drops an Ibor index's tenor and carries the same name for the other kinds
    ibor.floatingRateName shouldBe FloatingRateNames.GBP_LIBOR
    (IborIndices.GBP_LIBOR_3M: FloatingRate).floatingRateName shouldBe ibor.floatingRateName
    overnight.floatingRateName.name shouldBe overnight.name
    price.floatingRateName.name shouldBe price.name
    family.floatingRateName shouldBe family

    // Stated over the four built-in implementations at once: the family a floating rate reports
    // is a published member of the name family, and the accessor is idempotent, which is what
    // lets a caller apply it without knowing which of them it holds.
    List(ibor, overnight, price, family).foreach { rate =>
      withClue(s"${rate.name}: ") {
        val reported = rate.floatingRateName
        FloatingRateName.valueOf(reported.name) shouldBe Some(reported)
        reported.floatingRateName shouldBe reported
      }
    }

    // each reaches a caller through the union search, the last only through the fourth probe
    FloatingRate.tryParse("GBP-LIBOR-6M") shouldBe Some(ibor)
    FloatingRate.tryParse("GBP-SONIA") shouldBe Some(overnight)
    FloatingRate.parse("GB-HICP") should haveValue(price)
    FloatingRate.parse("GBP-LIBOR") should haveValue(family)
  }

  test("floatingRate_entryPointsAgreeWithEachOther") {
    // The three entry points are asserted to give one answer over the sample text of this spec:
    // `tryParse` is the search applied to the shipped composition, and `parse` differs from it
    // only in turning the absent case into a failure.
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
    // Total in its input: blank text, control characters, a surrogate pair, an injection payload,
    // a traversal, format specifiers and a long payload are all answered rather than raised.
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

  /**
   * A stand-in for a union of families, in the exact shape the four production compositions have:
   * an object holding a memoised `LazyList` of by-name probes over sibling objects that stand for
   * the family companions. The deferral assertions of this spec are stated over it rather than over
   * the real families, so that they observe the shape itself and not the order in which this run
   * happened to load the index classes: nothing else in this spec or in the library refers to the
   * two stand-ins, so each records its own initialization, once, when a probe naming it is applied.
   */
  private object DeferralFixture {

    /** How many times the first stand-in family's initializer has run. */
    val firstInitializations: AtomicInteger = new AtomicInteger(0)

    /** How many times the second stand-in family's initializer has run. */
    val secondInitializations: AtomicInteger = new AtomicInteger(0)

    /**
     * Records one run of a stand-in family's initializer. The count is bound to a wildcard because
     * what the assertions read is the reference, not this increment.
     *
     * @param sink  the counter of the family being initialized
     */
    private def record(sink: AtomicInteger): Unit = {
      val _ = sink.incrementAndGet()
    }

    /** A stand-in family whose initializer is observable through the count it records. */
    private object FirstFamily {
      record(firstInitializations)
      def valueOf(name: String): Option[String] = Option.when(name == "first")("first:first")
    }

    /** The second stand-in family, so that a probe can be applied without reaching the first. */
    private object SecondFamily {
      record(secondInitializations)
      def valueOf(name: String): Option[String] = Option.when(name == "second")("second:second")
    }

    /**
     * The composition: memoised, with elements supplied by name and each written out as a function
     * of the name, which is what the production compositions are.
     */
    lazy val standardLookups: LazyList[String => Option[String]] =
      ((name: String) => FirstFamily.valueOf(name)) #::
        ((name: String) => SecondFamily.valueOf(name)) #::
        LazyList.empty[String => Option[String]]
  }

  //-------------------------------------------------------------------------
  test("floatingRate_standardLookupsAreTheFourFamilyProbesInOrder") {
    // Each probe is required to be one family's own exact lookup, in the declared order Ibor,
    // Overnight, price, then floating rate name - asserted as an identity with that family's
    // lookup over every row of the table, which reads no family's membership.
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

  test("test_standardLookups_memoisedCompositionWithProbesStillDeferred") {
    // Each of the four compositions is assembled once and searched thereafter, so obtaining one
    // twice yields a single value; what the memoisation had to leave alone is the production of
    // each probe, which is what defers a family's initialization to the search that reaches it.
    val firstObtained = FloatingRate.standardLookups
    val secondObtained = FloatingRate.standardLookups
    firstObtained should be theSameInstanceAs secondObtained
    Index.standardLookups should be theSameInstanceAs Index.standardLookups
    RateIndex.standardLookups should be theSameInstanceAs RateIndex.standardLookups
    FloatingRateIndex.standardLookups should be theSameInstanceAs FloatingRateIndex.standardLookups

    // A memoised composition is searched, not consumed: each is traversed repeatedly here, and its
    // documented size and its answers are the same every time, which a value backed by an iterator
    // would not be.
    Index.standardLookups should have size 4
    Index.standardLookups.toList should have size 4
    RateIndex.standardLookups should have size 2
    FloatingRateIndex.standardLookups should have size 3
    FloatingRate.standardLookups should have size 4
    (floatingRateSampleText.toList ::: List("GBP-LIBOR", "EUR/CHF-ECB")).foreach { name =>
      withClue(s"'$name': ") {
        Index.firstMatch(name, Index.standardLookups) shouldBe Index.valueOf(name)
        Index.firstMatch(name, Index.standardLookups) shouldBe Index.valueOf(name)
        Index.firstMatch(name, RateIndex.standardLookups) shouldBe RateIndex.valueOf(name)
        Index.firstMatch(name, FloatingRateIndex.standardLookups) shouldBe
          FloatingRateIndex.valueOf(name)
        FloatingRate.tryParseWith(name, FloatingRate.standardLookups) shouldBe
          FloatingRate.tryParse(name)
        FloatingRate.tryParseWith(name, FloatingRate.standardLookups) shouldBe
          FloatingRate.tryParse(name)
      }
    }

    // The deferral itself, proved over the fixture above rather than over the families, so that
    // nothing here depends on which class this run happened to load first: forcing every cell of a
    // composition of the production shape produces both probes and runs neither family's
    // initializer, and applying one probe runs exactly the initializer of the family it names.
    val composition = DeferralFixture.standardLookups
    composition should be theSameInstanceAs DeferralFixture.standardLookups
    composition.size shouldBe 2
    composition.toList should have size 2
    DeferralFixture.firstInitializations.get() shouldBe 0
    DeferralFixture.secondInitializations.get() shouldBe 0

    composition(1).apply("second") shouldBe Some("second:second")
    DeferralFixture.secondInitializations.get() shouldBe 1
    DeferralFixture.firstInitializations.get() shouldBe 0

    composition.head.apply("first") shouldBe Some("first:first")
    DeferralFixture.firstInitializations.get() shouldBe 1
    DeferralFixture.secondInitializations.get() shouldBe 1

    // an initializer runs once however often its probe is applied, and a probe of the memoised
    // composition answers the same after the family behind it has been initialized
    composition.head.apply("other") shouldBe None
    composition(1).apply("other") shouldBe None
    composition.head.apply("first") shouldBe Some("first:first")
    composition(1).apply("second") shouldBe Some("second:second")
    DeferralFixture.firstInitializations.get() shouldBe 1
    DeferralFixture.secondInitializations.get() shouldBe 1
  }

  test("floatingRate_standardLookupsComposeIntoSubsets") {
    // a subset composes, so a caller searches fewer families without reimplementing the search
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

    // and taking a subset leaves the memoised composition it was taken from as it was
    FloatingRate.standardLookups should have size 4
    FloatingRate.standardLookups should be theSameInstanceAs FloatingRate.standardLookups
  }

  test("floatingRate_searchIsStatedOverAnyValueAProbeAnswersWith") {
    // the search is stated over the value type of the probes, so no family need be involved
    val numeric: List[String => Option[Int]] =
      List(text => Option.when(text == "one")(1), text => Option.when(text.nonEmpty)(text.length))
    FloatingRate.tryParseWith("one", numeric) shouldBe Some(1)
    FloatingRate.tryParseWith("four", numeric) shouldBe Some(4)
    FloatingRate.tryParseWith("", numeric) shouldBe None

    // and it is total in the composition it is given, whatever ordered sequence that is
    FloatingRate.tryParseWith("a", Vector.empty[FloatingRate.Lookup]) shouldBe None
  }

  //-------------------------------------------------------------------------
  // The order of the union is a property of the shipped composition rather than of the data: with
  // the name spaces disjoint, any permutation of the probes answers every name of today's data
  // identically. It is asserted positionally against `Index.standardLookups`, the value the
  // production lookup searches, in three parts - the four families in the declared order, the
  // entry points searching that composition, and first-hit-wins over probes supplied here.
  //-------------------------------------------------------------------------
  test("test_standardLookups_probeOrder") {
    val probes: List[Index.Lookup] = Index.standardLookups.toList
    probes should have size 4

    // Positional identity: exactly one probe claims each name, and it is the one at that family's
    // position, so swapping any two probes of the production composition fails this assertion.
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

    // every probe is exact, so text naming no member is answered by all four with nothing
    probes.zipWithIndex.foreach {
      case (probe, position) => withClue(s"probe $position: ")(probe("Rubbish") shouldBe None)
    }

    // the production lookup and the search over the shipped composition agree for every published
    // name and for text naming none, so the entry points search that composition and nothing else
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
    // First-hit-wins, with a collision the published data has no case of: two probes claim one
    // piece of text, the earlier answers, and the later is asserted never to be applied.
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

    // And the search is total in the composition it is given, including the empty one, whatever
    // ordered sequence that is: the lazy sequence the four unions hold and two strict ones.
    Index.firstMatch("a", List.empty[Index.Lookup]) shouldBe None
    Index.firstMatch("a", LazyList.empty[Index.Lookup]) shouldBe None
    Index.firstMatch("a", Vector.empty[Index.Lookup]) shouldBe None
    Index.firstMatch("", LazyList.empty[Index.Lookup]) shouldBe None

    // The probe after the answering one is not merely left unapplied, it is never produced: the
    // element beyond the hit raises where it is produced, and the searches that stop before it
    // answer normally while the one that reaches it fails - which is what the last line asserts.
    probed.clear()
    def beyondTheHit(beyond: => (String => Option[String])): LazyList[String => Option[String]] =
      probe("first", Set("a")) #:: probe("second", Set("b")) #:: beyond #::
        LazyList.empty[String => Option[String]]
    Index.firstMatch("a", beyondTheHit(fail("the probe beyond the answering one was produced"))) shouldBe
      Some("first:a")
    probed.toList shouldBe List("first")
    probed.clear()
    Index.firstMatch("b", beyondTheHit(fail("the probe beyond the answering one was produced"))) shouldBe
      Some("second:b")
    probed.toList shouldBe List("first", "second")
    probed.clear()
    a[TestFailedException] should be thrownBy
      Index.firstMatch("c", beyondTheHit(fail("the probe beyond the answering one was produced")))
    probed.toList shouldBe List("first", "second")
  }
}
