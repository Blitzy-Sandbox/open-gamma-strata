/*
 * Copyright (C) 2017 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import cats.Show

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2

import com.opengamma.strata.basics.date.Tenor
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[FloatingRateIndex]].
 *
 * The union holds the Ibor, Overnight and price families, searched in that order; the
 * exchange-rate family is deliberately excluded, an FX index quoting a rate of exchange rather
 * than a rate of interest, which is the boundary `test_of_lookup_notFound` pins.
 *
 * A supplied tenor is used only where the text carries none: `("GBP-LIBOR", 6M)` gives the
 * six-month index while `("GBP-LIBOR-1M", 6M)` gives the one-month one, and bare `GBP-LIBOR`
 * falls back on the family's own default of `3M`. Failures are compared by reason, not message.
 */
class FloatingRateIndexSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * Renders a member of the union as text, through the `Show` instance of the family that
   * published it.
   *
   * The union publishes no `Show` while each family does, so this matches over the families, and
   * the compiler requires the match to be exhaustive: a fourth member would not compile.
   *
   * @param index  the index to render
   * @return the text the index renders as, which is its unique name
   */
  private def renderThroughUnion(index: FloatingRateIndex): String =
    index match {
      case iborIndex: IborIndex => Show[IborIndex].show(iborIndex)
      case overnightIndex: OvernightIndex => Show[OvernightIndex].show(overnightIndex)
      case priceIndex: PriceIndex => Show[PriceIndex].show(priceIndex)
    }

  /**
   * The rows the four table tests are driven from, pairing an index with the name it renders as
   * and is looked up by. The names are written out rather than derived from `values`, so the
   * expected text is stated independently of the index data it checks, and the first column is
   * typed as the union, so the tests exercise the union type rather than the leaf types.
   */
  private val dataName: TableFor2[FloatingRateIndex, String] = Table(
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
    (PriceIndices.EU_AI_CPI, "EU-AI-CPI")
  )

  //-------------------------------------------------------------------------
  test("test_parse_noTenor") {
    FloatingRateIndex.parse("GBP-LIBOR") should haveValue(IborIndices.GBP_LIBOR_3M)

    FloatingRateIndex.parse("GBP-LIBOR-1M") should haveValue(IborIndices.GBP_LIBOR_1M)
    FloatingRateIndex.parse("GBP-LIBOR-3M") should haveValue(IborIndices.GBP_LIBOR_3M)

    FloatingRateIndex.parse("GBP-SONIA") should haveValue(OvernightIndices.GBP_SONIA)
    FloatingRateIndex.parse("GB-RPI") should haveValue(PriceIndices.GB_RPI)

    FloatingRateIndex.parse("") should beFailureWith(FailureReason.PARSING)
    FloatingRateIndex.parse("   ") should beFailureWith(FailureReason.PARSING)

    FloatingRateIndex.parse("NotAnIndex") should beFailureWith(FailureReason.PARSING)
  }

  test("test_parse_withTenor") {
    FloatingRateIndex.parse("GBP-LIBOR", Tenor.TENOR_6M) should haveValue(IborIndices.GBP_LIBOR_6M)

    FloatingRateIndex.parse("GBP-LIBOR-1M", Tenor.TENOR_6M) should haveValue(
      IborIndices.GBP_LIBOR_1M)
    FloatingRateIndex.parse("GBP-LIBOR-3M", Tenor.TENOR_6M) should haveValue(
      IborIndices.GBP_LIBOR_3M)

    FloatingRateIndex.parse("GBP-SONIA", Tenor.TENOR_6M) should haveValue(
      OvernightIndices.GBP_SONIA)
    FloatingRateIndex.parse("GB-RPI", Tenor.TENOR_6M) should haveValue(PriceIndices.GB_RPI)

    FloatingRateIndex.parse("", Tenor.TENOR_6M) should beFailureWith(FailureReason.PARSING)
    FloatingRateIndex.parse("   ", Tenor.TENOR_6M) should beFailureWith(FailureReason.PARSING)

    FloatingRateIndex.parse("NotAnIndex", Tenor.TENOR_6M) should beFailureWith(
      FailureReason.PARSING)
  }

  test("test_tryParse_noTenor") {
    FloatingRateIndex.tryParse("GBP-LIBOR") shouldBe Some(IborIndices.GBP_LIBOR_3M)
    FloatingRateIndex.tryParse("GBP-LIBOR-1M") shouldBe Some(IborIndices.GBP_LIBOR_1M)
    FloatingRateIndex.tryParse("GBP-LIBOR-3M") shouldBe Some(IborIndices.GBP_LIBOR_3M)
    FloatingRateIndex.tryParse("GBP-SONIA") shouldBe Some(OvernightIndices.GBP_SONIA)
    FloatingRateIndex.tryParse("GB-RPI") shouldBe Some(PriceIndices.GB_RPI)

    FloatingRateIndex.tryParse("") shouldBe None
    FloatingRateIndex.tryParse("   ") shouldBe None

    FloatingRateIndex.tryParse("NotAnIndex") shouldBe None
  }

  test("test_tryParse_withTenor") {
    FloatingRateIndex.tryParse("GBP-LIBOR", Tenor.TENOR_6M) shouldBe Some(IborIndices.GBP_LIBOR_6M)
    FloatingRateIndex.tryParse("GBP-LIBOR-1M", Tenor.TENOR_6M) shouldBe Some(
      IborIndices.GBP_LIBOR_1M)
    FloatingRateIndex.tryParse("GBP-LIBOR-3M", Tenor.TENOR_6M) shouldBe Some(
      IborIndices.GBP_LIBOR_3M)
    FloatingRateIndex.tryParse("GBP-SONIA", Tenor.TENOR_6M) shouldBe Some(
      OvernightIndices.GBP_SONIA)
    FloatingRateIndex.tryParse("GB-RPI", Tenor.TENOR_6M) shouldBe Some(PriceIndices.GB_RPI)

    FloatingRateIndex.tryParse("", Tenor.TENOR_6M) shouldBe None
    FloatingRateIndex.tryParse("   ", Tenor.TENOR_6M) shouldBe None
    FloatingRateIndex.tryParse("NotAnIndex", Tenor.TENOR_6M) shouldBe None
  }

  //-------------------------------------------------------------------------
  test("test_name") {
    forEvery(dataName) { (index: FloatingRateIndex, name: String) =>
      index.name shouldBe name
    }
  }

  test("test_toString") {
    forEvery(dataName) { (index: FloatingRateIndex, name: String) =>
      index.toString shouldBe name
    }
  }

  test("test_of_lookup") {
    // `valueOf`, the exact lookup, and `parse`, which reports the failure, are both asserted for
    // every row, and every row carries a full index name, so no tenor defaulting takes part here
    forEvery(dataName) { (index: FloatingRateIndex, name: String) =>
      withClue(s"$name: ") {
        FloatingRateIndex.valueOf(name) shouldBe Some(index)
        FloatingRateIndex.parse(name) should haveValue(index)
      }
    }

    val published: List[FloatingRateIndex] =
      IborIndex.values.toList ::: OvernightIndex.values.toList ::: PriceIndex.values.toList
    published.foreach { index =>
      withClue(s"${index.name}: ") {
        FloatingRateIndex.valueOf(index.name) shouldBe Some(index)
      }
    }
  }

  test("test_of_convert") {
    // the rendering of the union and its exact lookup are inverse in both directions; the union
    // is a trait excluded from JSON, so this is the text round trip rather than a codec test
    forEvery(dataName) { (index: FloatingRateIndex, name: String) =>
      withClue(s"$name: ") {
        renderThroughUnion(index) shouldBe name
        FloatingRateIndex.valueOf(name) shouldBe Some(index)
        FloatingRateIndex.parse(renderThroughUnion(index)) should haveValue(index)
        FloatingRateIndex.valueOf(name).map(renderThroughUnion) shouldBe Some(name)
      }
    }
  }

  test("test_of_lookup_notFound") {
    // `EUR/USD-ECB` is a published exchange-rate index and a published `Index`, so the name is
    // not rubbish: it resolves to nothing here only because that family is outside this union
    FloatingRateIndex.valueOf(FxIndices.EUR_USD_ECB.name) shouldBe None
    FloatingRateIndex.parse(FxIndices.EUR_USD_ECB.name) should beFailureWith(FailureReason.PARSING)

    FxIndex.values.toList.foreach { fxIndex =>
      withClue(s"${fxIndex.name}: ") {
        FloatingRateIndex.valueOf(fxIndex.name) shouldBe None
        FloatingRateIndex.parse(fxIndex.name) should beFailureWith(FailureReason.PARSING)
      }
    }

    FloatingRateIndex.valueOf("NotAnIndex") shouldBe None
    FloatingRateIndex.parse("NotAnIndex") should beFailureWith(FailureReason.PARSING)
  }

  test("test_of_lookup_null") {
    // an absent name reaches these entry points as the empty name or a blank one
    FloatingRateIndex.valueOf("") shouldBe None
    FloatingRateIndex.parse("") should beFailureWith(FailureReason.PARSING)
    FloatingRateIndex.valueOf("   ") shouldBe None
    FloatingRateIndex.parse("   ") should beFailureWith(FailureReason.PARSING)
  }

  //-------------------------------------------------------------------------
  test("test_standardLookups_probeOrder") {
    // The three name spaces are disjoint, so no published name distinguishes one probe order from
    // another. What is asserted is the composition the lookup searches - the Ibor, Overnight and
    // price families at those positions, and nothing else; first hit wins is in `IndexSpec`.
    val probes: List[FloatingRateIndex.Lookup] = FloatingRateIndex.standardLookups.toList
    probes should have size 3

    val samples: TableFor2[String, Int] =
      Table(
        ("name", "position"),
        (IborIndices.GBP_LIBOR_3M.name, 0),
        (OvernightIndices.GBP_SONIA.name, 1),
        (PriceIndices.GB_RPI.name, 2))
    forEvery(samples) { (name: String, position: Int) =>
      withClue(s"$name: ") {
        probes.map(probe => probe(name).isDefined) shouldBe List.tabulate(3)(_ == position)
        probes(position)(name) shouldBe FloatingRateIndex.valueOf(name)
      }
    }

    // the exchange-rate family is outside the composition, not merely refused by the entry point
    (FxIndex.values.toList.map(_.name) ::: List("Rubbish", "GBP-LIBOR")).foreach { name =>
      withClue(s"$name: ")(probes.flatMap(probe => probe(name)) shouldBe empty)
    }

    val everyName: List[String] =
      (IborIndex.values.toList ::: OvernightIndex.values.toList ::: PriceIndex.values.toList)
        .map(_.name) ::: List("GBP-LIBOR", "Rubbish", "")
    everyName.foreach { name =>
      withClue(s"$name: ") {
        FloatingRateIndex.valueOf(name) shouldBe
          Index.firstMatch(name, FloatingRateIndex.standardLookups)
      }
    }
  }

  test("test_standardLookups_memoisedComposition") {
    // The composition is assembled once and searched thereafter, so obtaining it twice yields a
    // single value; it is searched rather than consumed, so a second search of the same value
    // answers as the first one did, and the three probes stay the three families in their order.
    val firstObtained = FloatingRateIndex.standardLookups
    val secondObtained = FloatingRateIndex.standardLookups
    firstObtained should be theSameInstanceAs secondObtained
    firstObtained should have size 3
    secondObtained should have size 3

    List(
      IborIndices.GBP_LIBOR_3M.name,
      OvernightIndices.GBP_SONIA.name,
      PriceIndices.GB_RPI.name,
      FxIndices.EUR_USD_ECB.name,
      "GBP-LIBOR",
      "Rubbish",
      "").foreach { name =>
      withClue(s"$name: ") {
        val expected = FloatingRateIndex.valueOf(name)
        Index.firstMatch(name, firstObtained) shouldBe expected
        Index.firstMatch(name, secondObtained) shouldBe expected
        Index.firstMatch(name, FloatingRateIndex.standardLookups) shouldBe expected
        FloatingRateIndex.valueOf(name) shouldBe expected
      }
    }
  }
}
