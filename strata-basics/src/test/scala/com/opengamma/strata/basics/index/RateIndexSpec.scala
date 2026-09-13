/*
 * Copyright (C) 2018 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import cats.Show

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2

import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[RateIndex]].
 *
 * [[RateIndex]] is the union of exactly two families in one fixed search order - Ibor index, then
 * Overnight index - so its membership is the 271 Ibor and the 35 Overnight indices together. A
 * price index and an exchange-rate index are excluded, a refusal asserted beside text naming
 * nothing at all so that it is shown to be about the family rather than about the text being
 * unknown. The union is a trait carrying no JSON codec, so a member's JSON form is asserted in
 * the spec of its own family.
 */
class RateIndexSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  private val dataName: TableFor2[RateIndex, String] = Table(
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
    (OvernightIndices.USD_FED_FUND, "USD-FED-FUND")
  )

  /**
   * Renders a rate index through the text instance published by the family it belongs to, the
   * union itself being a trait that carries no instance of its own; the match needs no default
   * case because the union is sealed over these two families.
   *
   * @param index  the index to render
   * @return the text the index renders as, which is its name
   */
  private def renderedName(index: RateIndex): String =
    index match {
      case ibor: IborIndex => Show[IborIndex].show(ibor)
      case overnight: OvernightIndex => Show[OvernightIndex].show(overnight)
    }

  //-------------------------------------------------------------------------
  test("test_name") {
    forEvery(dataName) { (index: RateIndex, name: String) =>
      index.name shouldBe name
    }
  }

  test("test_toString") {
    forEvery(dataName) { (index: RateIndex, name: String) =>
      index.toString shouldBe name
    }
  }

  test("test_of_lookup") {
    // `valueOf` is the exact lookup and `parse` reports the failure as a value; both are asserted
    // over the same rows, and the annotation holds `valueOf` to answering with the union.
    forEvery(dataName) { (index: RateIndex, name: String) =>
      withClue(s"$name: ") {
        val resolved: Option[RateIndex] = RateIndex.valueOf(name)
        resolved shouldBe Some(index)
        RateIndex.parse(name) should haveValue(index)
      }
    }

    IborIndex.values.toList.foreach { ibor =>
      withClue(s"${ibor.name}: ") {
        RateIndex.valueOf(ibor.name) shouldBe Some(ibor)
      }
    }
    OvernightIndex.values.toList.foreach { overnight =>
      withClue(s"${overnight.name}: ") {
        RateIndex.valueOf(overnight.name) shouldBe Some(overnight)
      }
    }
  }

  test("test_of_convert") {
    forEvery(dataName) { (index: RateIndex, name: String) =>
      val rendered = renderedName(index)
      withClue(s"$name: ") {
        rendered shouldBe name
        rendered shouldBe index.name
        RateIndex.valueOf(rendered) shouldBe Some(index)
        RateIndex.parse(rendered) should haveValue(index)
      }
    }
  }

  test("test_of_lookup_notFound") {
    // `GB-RPI` names a price index: not a rate index, but still an index and a floating rate
    // index, which is the contrast that shows the refusal to be this union's membership.
    val priceName = PriceIndices.GB_RPI.name
    RateIndex.valueOf(priceName) shouldBe None
    RateIndex.parse(priceName) should beFailureWith(FailureReason.PARSING)
    FloatingRateIndex.valueOf(priceName) shouldBe Some(PriceIndices.GB_RPI)
    Index.valueOf(priceName) shouldBe Some(PriceIndices.GB_RPI)

    val fxName = FxIndices.EUR_USD_ECB.name
    RateIndex.valueOf(fxName) shouldBe None
    RateIndex.parse(fxName) should beFailureWith(FailureReason.PARSING)
    Index.valueOf(fxName) shouldBe Some(FxIndices.EUR_USD_ECB)
  }

  test("test_of_lookup_null") {
    RateIndex.valueOf("") shouldBe None
    RateIndex.parse("") should beFailureWith(FailureReason.PARSING)
    RateIndex.valueOf("   ") shouldBe None
    RateIndex.parse("   ") should beFailureWith(FailureReason.PARSING)
  }

  //-------------------------------------------------------------------------
  test("test_standardLookups_probeOrder") {
    // The two families this union searches and the order it searches them in, pinned against the
    // composition the entry points use: those two families, at those two positions.
    val probes: List[RateIndex.Lookup] = RateIndex.standardLookups.toList
    probes should have size 2

    val iborName = IborIndices.GBP_LIBOR_3M.name
    val overnightName = OvernightIndices.GBP_SONIA.name
    probes.map(probe => probe(iborName).isDefined) shouldBe List(true, false)
    probes.map(probe => probe(overnightName).isDefined) shouldBe List(false, true)
    probes.head(iborName) shouldBe Some(IborIndices.GBP_LIBOR_3M)
    probes(1)(overnightName) shouldBe Some(OvernightIndices.GBP_SONIA)

    List(PriceIndices.GB_RPI.name, FxIndices.EUR_USD_ECB.name, "Rubbish").foreach { name =>
      withClue(s"$name: ")(probes.flatMap(probe => probe(name)) shouldBe empty)
    }

    val everyName: List[String] =
      (IborIndex.values.toList ::: OvernightIndex.values.toList).map(_.name) :::
        List(PriceIndices.GB_RPI.name, FxIndices.EUR_USD_ECB.name, "Rubbish", "")
    everyName.foreach { name =>
      withClue(s"$name: ") {
        RateIndex.valueOf(name) shouldBe Index.firstMatch(name, RateIndex.standardLookups)
      }
    }
  }

  test("test_standardLookups_memoisedComposition") {
    // The composition is assembled once and searched thereafter, so obtaining it twice yields a
    // single value; it is searched rather than consumed, so a second search of the same value
    // answers as the first one did, and the two probes stay the two families in their order.
    val firstObtained = RateIndex.standardLookups
    val secondObtained = RateIndex.standardLookups
    firstObtained should be theSameInstanceAs secondObtained
    firstObtained should have size 2
    secondObtained should have size 2

    List(IborIndices.GBP_LIBOR_3M.name, OvernightIndices.GBP_SONIA.name, "GB-RPI", "Rubbish", "")
      .foreach { name =>
        withClue(s"$name: ") {
          val expected = RateIndex.valueOf(name)
          Index.firstMatch(name, firstObtained) shouldBe expected
          Index.firstMatch(name, secondObtained) shouldBe expected
          Index.firstMatch(name, RateIndex.standardLookups) shouldBe expected
          RateIndex.valueOf(name) shouldBe expected
        }
      }
  }
}
