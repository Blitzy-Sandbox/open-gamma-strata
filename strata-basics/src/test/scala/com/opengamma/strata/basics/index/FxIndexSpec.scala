/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import java.time.LocalDate

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

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.CurrencyPair
import com.opengamma.strata.basics.date.DaysAdjustment
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.basics.date.HolidayCalendarIds
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[FxIndex]].
 *
 * `FxIndex` is the closed set of the sixteen published rows, of which eight have a named constant.
 * A currency pair identifies an index only through those rows: `FxIndex.of` reports a parsing
 * failure for a pair no row is published for, and answers with the matching index of lowest name
 * where a pair has more than one. Two neighbouring tests are easy to confuse:
 * `test_of_lookup_parse_currency` asks for `USD/CAD`, which nothing is published for and so fails
 * by every route into the family, while `test_of_lookup_currency_pair_from_extendedEnum` asks for
 * `USD/COP`, which is published and so succeeds. A failure is read as a value through the outcome
 * matchers and compared by reason, never by message text.
 */
class FxIndexSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The reference data every date calculation below is resolved against: the standard set, holding
   * every built-in holiday calendar. Eight of the sixteen published indices name a fixing calendar
   * this library ships no holiday data for, so those indices do not resolve against this set - a
   * property of the published data, asserted below rather than worked around.
   */
  private val RefData: ReferenceData = ReferenceData.standard

  /**
   * The shared table driving the parameterised tests: each row pairs one of the eight published
   * constants with the name it renders as and is looked up by.
   *
   * The expected names are written out rather than derived from `FxIndex.values`, so the table
   * states them independently of the family it checks, and `test_extendedEnum` compares the two.
   * Eight of the sixteen published indices have no constant, so this table is not the family; the
   * two reached by name in `test_cny` and `test_inr` are among the eight absent from it.
   */
  private val dataName: TableFor2[FxIndex, String] = Table(
    ("index", "name"),
    (FxIndices.EUR_CHF_ECB, "EUR/CHF-ECB"),
    (FxIndices.EUR_GBP_ECB, "EUR/GBP-ECB"),
    (FxIndices.EUR_JPY_ECB, "EUR/JPY-ECB"),
    (FxIndices.EUR_USD_ECB, "EUR/USD-ECB"),
    (FxIndices.USD_CHF_WM, "USD/CHF-WM"),
    (FxIndices.EUR_USD_WM, "EUR/USD-WM"),
    (FxIndices.GBP_USD_WM, "GBP/USD-WM"),
    (FxIndices.USD_JPY_WM, "USD/JPY-WM")
  )

  //-------------------------------------------------------------------------
  test("test_name") {
    forEvery(dataName) { (index: FxIndex, name: String) =>
      index.name shouldBe name
    }
  }

  test("test_toString") {
    forEvery(dataName) { (index: FxIndex, name: String) =>
      index.toString shouldBe name
    }
  }

  test("test_of_lookup") {
    forEvery(dataName) { (index: FxIndex, name: String) =>
      withClue(s"$name: ") {
        FxIndex.of(name) should haveValue(index)
        FxIndex.valueOf(name) shouldBe Some(index)
        FxIndex.parse(name) should haveValue(index)
      }
    }
  }

  test("test_extendedEnum") {
    // Each name of the table resolves, through the family's own lookup, to the constant it is
    // paired with, and the family holds exactly the sixteen published indices under distinct names.
    forEvery(dataName) { (index: FxIndex, name: String) =>
      withClue(s"$name: ") {
        FxIndex.valueOf(name) shouldBe Some(index)
      }
    }

    val all: List[FxIndex] = FxIndex.values.toList
    all should have size 16
    all.distinct should have size 16
    all.map(_.name).distinct should have size 16

    all.foreach { index =>
      withClue(s"${index.name}: ") {
        FxIndex.valueOf(index.name) shouldBe Some(index)
      }
    }

    val constants: List[FxIndex] = dataName.toList.map { case (index, _) => index }
    constants.map(_.name).toSet.subsetOf(all.map(_.name).toSet) shouldBe true
    // eight of the sixteen published indices deliberately have no constant
    constants.distinct should have size 8
  }

  test("test_of_lookup_notFound") {
    // `Rubbish` is neither the name of an index nor readable as a currency pair, so this is not
    // the closed-family case; that one is `test_of_lookup_parse_currency`.
    FxIndex.valueOf("Rubbish") shouldBe None
    FxIndex.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
    FxIndex.of("Rubbish") should beFailureWith(FailureReason.PARSING)
    CurrencyPair.parse("Rubbish").isLeft shouldBe true
  }

  test("test_of_lookup_null") {
    FxIndex.valueOf("") shouldBe None
    FxIndex.parse("") should beFailureWith(FailureReason.PARSING)
    FxIndex.of("") should beFailureWith(FailureReason.PARSING)
    FxIndex.valueOf("   ") shouldBe None
    FxIndex.parse("   ") should beFailureWith(FailureReason.PARSING)
    FxIndex.of("   ") should beFailureWith(FailureReason.PARSING)

    CurrencyPair.parse("").isLeft shouldBe true
    CurrencyPair.parse("   ").isLeft shouldBe true
  }

  //-------------------------------------------------------------------------
  test("test_of_lookup_parse_currency") {
    // The family is the closed set of the sixteen published rows, so `USD/CAD`, which no row is
    // published for, is a parsing failure by every route into it - the name lookup, the parse, the
    // factory taking text and the factory taking the pair. There is no expected index to compare
    // against here: the failure is the behaviour, and nothing stands behind the four routes to
    // build a member the published rows do not name.
    val usdCad: CurrencyPair = CurrencyPair.of(Currency.USD, Currency.CAD)
    usdCad.toString shouldBe "USD/CAD"

    FxIndex.valueOf(usdCad.toString) shouldBe None
    FxIndex.parse(usdCad.toString) should beFailureWith(FailureReason.PARSING)

    FxIndex.of(usdCad.toString) should beFailureWith(FailureReason.PARSING)

    // the factory taking the pair itself reports it too
    FxIndex.of(usdCad) should beFailureWith(FailureReason.PARSING)

    // the failure is the absence of a published index for an ordinary pair, not unreadable text
    CurrencyPair.parse(usdCad.toString) shouldBe Right(usdCad)
    FxIndex.values.toList.map(_.currencyPair) should not contain usdCad
  }

  test("test_of_lookup_currency_pair_from_extendedEnum") {
    // The other side: `USD/COP` *is* published, so asking for its index succeeds. Equality is by
    // name alone, so fields are compared one by one, written out rather than read from the row.
    val usdCop: CurrencyPair = CurrencyPair.of(Currency.USD, Currency.COP)
    val cobo: HolidayCalendarId = HolidayCalendarId.of("COBO")
    val resolved: FxIndex =
      FxIndex.of(usdCop).getOrElse(fail("the FX index family publishes no index for USD/COP"))

    resolved.name shouldBe "USD/COP-TRM-COP02"
    resolved.currencyPair shouldBe usdCop
    resolved.fixingCalendar shouldBe cobo
    resolved.maturityDateOffset shouldBe DaysAdjustment.ofBusinessDays(0, cobo)

    FxIndex.valueOf("USD/COP-TRM-COP02") shouldBe Some(resolved)
    FxIndex.of("USD/COP-TRM-COP02") should haveValue(resolved)
    FxIndex.parse("USD/COP-TRM-COP02") should haveValue(resolved)
    val byName: FxIndex =
      FxIndex.valueOf("USD/COP-TRM-COP02").getOrElse(fail("no index named USD/COP-TRM-COP02"))
    byName.currencyPair shouldBe resolved.currencyPair
    byName.fixingCalendar shouldBe resolved.fixingCalendar
    byName.maturityDateOffset shouldBe resolved.maturityDateOffset

    resolved.maturityDateOffset should not be DaysAdjustment.ofBusinessDays(2, cobo)

    //-----------------------------------------------------------------------
    // The selection rule: a pair does not identify an index, because two administrators publish
    // the euro against the dollar, and `of(pair)` answers with the candidate of lowest name.
    val eurUsd: CurrencyPair = CurrencyPair.of(Currency.EUR, Currency.USD)
    FxIndices.EUR_USD_ECB.currencyPair shouldBe eurUsd
    FxIndices.EUR_USD_WM.currencyPair shouldBe eurUsd
    FxIndex.values.toList.filter(_.currencyPair == eurUsd).map(_.name) should contain theSameElementsAs
      List("EUR/USD-ECB", "EUR/USD-WM")

    FxIndex.of(eurUsd) should haveValue(FxIndices.EUR_USD_ECB)
    FxIndex.of(eurUsd) should not (haveValue(FxIndices.EUR_USD_WM))
    FxIndex.of(eurUsd).map(_.name) shouldBe Right("EUR/USD-ECB")
    FxIndex.of(eurUsd.toString) should haveValue(FxIndices.EUR_USD_ECB)

    FxIndex.valueOf("EUR/USD-WM") shouldBe Some(FxIndices.EUR_USD_WM)
  }

  //-------------------------------------------------------------------------
  test("test_ecb_eur_gbp_dates") {
    // The offsets mirror each other in the euro-area and London calendars combined, and the
    // backward one is derived rather than published.
    val test: FxIndex = FxIndices.EUR_GBP_ECB
    val eutaGblo: HolidayCalendarId = HolidayCalendarIds.EUTA.combinedWith(HolidayCalendarIds.GBLO)

    test.fixingDateOffset shouldBe DaysAdjustment.ofBusinessDays(-2, eutaGblo)
    test.maturityDateOffset shouldBe DaysAdjustment.ofBusinessDays(2, eutaGblo)

    test.calculateMaturityFromFixing(date(2014, 10, 13), RefData) should haveValue(date(2014, 10, 15))
    test.calculateFixingFromMaturity(date(2014, 10, 15), RefData) should haveValue(date(2014, 10, 13))
    // weekend
    test.calculateMaturityFromFixing(date(2014, 10, 16), RefData) should haveValue(date(2014, 10, 20))
    test.calculateFixingFromMaturity(date(2014, 10, 20), RefData) should haveValue(date(2014, 10, 16))
    test.calculateMaturityFromFixing(date(2014, 10, 17), RefData) should haveValue(date(2014, 10, 21))
    test.calculateFixingFromMaturity(date(2014, 10, 21), RefData) should haveValue(date(2014, 10, 17))
    // input date is Sunday
    test.calculateMaturityFromFixing(date(2014, 10, 19), RefData) should haveValue(date(2014, 10, 22))
    test.calculateFixingFromMaturity(date(2014, 10, 19), RefData) should haveValue(date(2014, 10, 16))
    // skip maturity over EUR (1st May) and GBP (5th May) holiday
    test.calculateMaturityFromFixing(date(2014, 4, 30), RefData) should haveValue(date(2014, 5, 6))
    test.calculateFixingFromMaturity(date(2014, 5, 6), RefData) should haveValue(date(2014, 4, 30))

    // resolve - `FxIndex.resolve` resolves the calendar and the offset once and answers with a
    // function over fixing dates; applying it agrees with observing the fixing directly. The
    // success is asserted too, without which two failures would compare equal and say nothing.
    val fixing: LocalDate = date(2014, 5, 6)
    val fromResolution = test.resolve(RefData).map(build => build(fixing))
    fromResolution should beSuccess
    fromResolution shouldBe FxIndexObservation.of(test, fixing, RefData)

    FxIndexObservation.resolve(test, RefData).map(build => build(fixing)) shouldBe fromResolution
    val resolved: FxIndexObservation =
      test.resolve(RefData).map(build => build(fixing)).getOrElse(fail("the index did not resolve"))
    resolved.fixingDate shouldBe fixing
    resolved.maturityDate shouldBe
      test.calculateMaturityFromFixing(fixing, RefData).getOrElse(fail("no maturity date"))

    test.resolve(ReferenceData.empty) should beFailureWith(FailureReason.MISSING_DATA)
  }

  test("test_dates") {
    // A plain weekday, a settlement crossing a weekend, a fixing input that is not a business day,
    // and the holiday case - Columbus Day 2014-10-13 is a holiday in New York but not in the euro
    // area, so two fixing dates settle on the same day and the backward calculation is a search.
    val test: FxIndex = FxIndices.EUR_USD_ECB
    test.fixingCalendar shouldBe HolidayCalendarIds.EUTA
    test.maturityDateOffset shouldBe DaysAdjustment.ofBusinessDays(
      2,
      HolidayCalendarIds.EUTA.combinedWith(HolidayCalendarIds.USNY))

    test.calculateMaturityFromFixing(date(2014, 10, 15), RefData) should haveValue(date(2014, 10, 17))
    test.calculateFixingFromMaturity(date(2014, 10, 17), RefData) should haveValue(date(2014, 10, 15))

    test.calculateMaturityFromFixing(date(2014, 10, 16), RefData) should haveValue(date(2014, 10, 20))
    test.calculateFixingFromMaturity(date(2014, 10, 20), RefData) should haveValue(date(2014, 10, 16))
    test.calculateMaturityFromFixing(date(2014, 10, 17), RefData) should haveValue(date(2014, 10, 21))
    test.calculateFixingFromMaturity(date(2014, 10, 21), RefData) should haveValue(date(2014, 10, 17))

    test.calculateMaturityFromFixing(date(2014, 10, 18), RefData) should haveValue(date(2014, 10, 22))
    test.calculateMaturityFromFixing(date(2014, 10, 19), RefData) should haveValue(date(2014, 10, 22))
    test.calculateMaturityFromFixing(date(2014, 10, 20), RefData) should haveValue(date(2014, 10, 22))
    test.calculateFixingFromMaturity(date(2014, 10, 18), RefData) should haveValue(date(2014, 10, 16))
    test.calculateFixingFromMaturity(date(2014, 10, 19), RefData) should haveValue(date(2014, 10, 16))
    test.calculateFixingFromMaturity(date(2014, 10, 20), RefData) should haveValue(date(2014, 10, 16))

    test.calculateMaturityFromFixing(date(2014, 10, 10), RefData) should haveValue(date(2014, 10, 15))
    test.calculateMaturityFromFixing(date(2014, 10, 13), RefData) should haveValue(date(2014, 10, 15))
    test.calculateFixingFromMaturity(date(2014, 10, 15), RefData) should haveValue(date(2014, 10, 13))

    // The zero-lag published row: the Colombian peso index settles on the fixing date itself, and
    // its calendar is one of the eight with no shipped holiday data, so it reports missing data.
    val zeroLag: FxIndex =
      FxIndex.valueOf("USD/COP-TRM-COP02").getOrElse(fail("no index named USD/COP-TRM-COP02"))
    zeroLag.maturityDateOffset shouldBe DaysAdjustment.ofBusinessDays(0, HolidayCalendarId.of("COBO"))
    zeroLag.calculateMaturityFromFixing(date(2014, 10, 15), RefData) should
      beFailureWith(FailureReason.MISSING_DATA)
    zeroLag.calculateFixingFromMaturity(date(2014, 10, 15), RefData) should
      beFailureWith(FailureReason.MISSING_DATA)
  }

  test("test_cny") {
    // One of the eight published rows with no constant, so it is reached by name.
    val test: FxIndex =
      FxIndex.valueOf("USD/CNY-SAEC-CNY01").getOrElse(fail("no index named USD/CNY-SAEC-CNY01"))
    test.name shouldBe "USD/CNY-SAEC-CNY01"
    FxIndex.of("USD/CNY-SAEC-CNY01") should haveValue(test)
    FxIndex.parse("USD/CNY-SAEC-CNY01") should haveValue(test)
    test.currencyPair shouldBe CurrencyPair.of(Currency.USD, Currency.CNY)
  }

  test("test_inr") {
    val test: FxIndex =
      FxIndex.valueOf("USD/INR-FBIL-INR01").getOrElse(fail("no index named USD/INR-FBIL-INR01"))
    test.name shouldBe "USD/INR-FBIL-INR01"
    FxIndex.of("USD/INR-FBIL-INR01") should haveValue(test)
    test.currencyPair shouldBe CurrencyPair.of(Currency.USD, Currency.INR)

    // The family declares exactly one alternate name, for the previous administrator of this rate.
    // Both spellings resolve to the same value, and only the canonical one is ever reported.
    FxIndex.valueOf("USD/INR-RBIB-INR01") shouldBe Some(test)
    FxIndex.of("USD/INR-RBIB-INR01") should haveValue(test)
    FxIndex.parse("USD/INR-RBIB-INR01") should haveValue(test)
    FxIndex.valueOf("USD/INR-RBIB-INR01").map(_.name) shouldBe Some("USD/INR-FBIL-INR01")
    FxIndex.values.toList.map(_.name) should not contain "USD/INR-RBIB-INR01"
  }

  //-------------------------------------------------------------------------
  test("test_equals") {
    // Equality is by name and by nothing else: `EUR_USD_ECB` and `EUR_USD_WM` quote one and the
    // same currency pair and are still unequal.
    val left: FxIndex = FxIndices.EUR_USD_ECB
    val right: FxIndex = FxIndices.EUR_USD_WM

    left.currencyPair shouldBe right.currencyPair
    left.name should not be right.name
    left should not be right
    Eq[FxIndex].eqv(left, right) shouldBe false
    Hash[FxIndex].eqv(left, right) shouldBe false
    Hash[FxIndex].hash(left) should not be Hash[FxIndex].hash(right)
    left.hashCode should not be right.hashCode

    val looked: FxIndex =
      FxIndex.valueOf("EUR/USD-ECB").getOrElse(fail("no index named EUR/USD-ECB"))
    Eq[FxIndex].eqv(looked, left) shouldBe true
    looked shouldBe left
    looked.hashCode shouldBe left.hashCode
    Eq[FxIndex].eqv(looked, right) shouldBe false

    left should not be left.name
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // First, the directory and the family agree: the eight constants are distinct, and each is the
    // value the family answers with for its own name, resolved through the exact name lookup.
    val constants: List[FxIndex] = dataName.toList.map { case (index, _) => index }
    constants should have size 8
    constants.distinct should have size 8
    constants.foreach { index =>
      withClue(s"${index.name}: ") {
        FxIndex.valueOf(index.name) shouldBe Some(index)
        FxIndex.parse(index.name) should haveValue(index)
        Show[FxIndex].show(index) shouldBe index.name
        index.toString shouldBe index.name
      }
    }
    FxIndices.EUR_USD_ECB should not be FxIndices.EUR_USD_WM
    FxIndex.of(CurrencyPair.of(Currency.EUR, Currency.USD)) should haveValue(FxIndices.EUR_USD_ECB)

    // Second, equality, hashing, ordering and rendering over two distinct published indices. The
    // companion publishes one equality-bearing instance, an ordering that is also a hashing, and
    // the assertions below ask all three about the same pairs and require one answer.
    val left: FxIndex = FxIndices.EUR_CHF_ECB
    val right: FxIndex = FxIndices.GBP_USD_WM
    left should not be right
    Eq[FxIndex].eqv(left, right) shouldBe false
    Hash[FxIndex].eqv(left, right) shouldBe false
    Hash[FxIndex].hash(left) should not be Hash[FxIndex].hash(right)
    Show[FxIndex].show(left) shouldBe "EUR/CHF-ECB"
    Show[FxIndex].show(right) shouldBe "GBP/USD-WM"
    Order[FxIndex].compare(left, right) should not be 0
    Order[FxIndex].compare(left, right) should be < 0
    Order[FxIndex].compare(right, left) should be > 0
    Order[FxIndex].compare(left, left) shouldBe 0

    val all: List[FxIndex] = FxIndex.values.toList
    for (first <- all; second <- all) {
      val sameValue = first == second
      withClue(s"${first.name} against ${second.name}: ") {
        Eq[FxIndex].eqv(first, second) shouldBe sameValue
        Hash[FxIndex].eqv(first, second) shouldBe sameValue
        (Order[FxIndex].compare(first, second) == 0) shouldBe sameValue
        if (sameValue) {
          Hash[FxIndex].hash(first) shouldBe Hash[FxIndex].hash(second)
        } else {
          Order[FxIndex].compare(first, second) should not be 0
        }
      }
    }
  }

  test("test_jodaConvert") {
    // The text round trip: an index renders as its name and that rendering reads back as the same
    // index. Every name contains a solidus, so a reader that split a name on it would resolve the
    // pair instead of the index - for the euro/dollar rates, the wrong administrator's index.
    forEvery(dataName) { (index: FxIndex, name: String) =>
      val rendered = Show[FxIndex].show(index)
      withClue(s"$name: ") {
        rendered shouldBe name
        rendered shouldBe index.name
        rendered should include("/")
        FxIndex.parse(rendered) should haveValue(index)
        FxIndex.valueOf(rendered) shouldBe Some(index)
        FxIndex.of(rendered) should haveValue(index)
      }
    }
  }

  test("test_serialization") {
    // The JSON shape of this family: an index is written as the bare string of its name, never as
    // an object. The round trip over every codec-bearing type belongs to `json.JsonRoundTripSpec`.
    forEvery(dataName) { (index: FxIndex, name: String) =>
      val encoded = index.asJson
      withClue(s"$name: ") {
        encoded shouldBe Json.fromString(name)
        encoded.asString shouldBe Some(name)
        encoded.as[FxIndex] shouldBe Right(index)
      }
    }

    val inr: FxIndex =
      FxIndex.valueOf("USD/INR-FBIL-INR01").getOrElse(fail("no index named USD/INR-FBIL-INR01"))
    Json.fromString("USD/INR-RBIB-INR01").as[FxIndex] shouldBe Right(inr)
    inr.asJson shouldBe Json.fromString("USD/INR-FBIL-INR01")

    // a string naming no member is rejected, as is a structural form the encoder never writes
    Json.fromString("Rubbish").as[FxIndex].isLeft shouldBe true
    Json.fromString("USD/CAD").as[FxIndex].isLeft shouldBe true
    Json.fromString("EUR/USD").as[FxIndex].isLeft shouldBe true
    Json.obj("name" -> Json.fromString("EUR/CHF-ECB")).as[FxIndex].isLeft shouldBe true
  }

  //-------------------------------------------------------------------------
  // The published table this family is created from, `FxIndexData`, is asserted below. The family
  // being closed, the table alone fixes which pairs an FX index exists for, so adding or omitting a
  // row changes the public API rather than merely a lookup. The rows themselves, column by column
  // and in published order, are compared against the captured reference data by
  // `ReferenceDataManifestSpec`; below is the structure a consumer of the table depends on.
  //-------------------------------------------------------------------------
  test("data_names_areDistinct") {
    FxIndexData.rows should have size 16
    FxIndexData.rows.map(_.name).distinct should have size 16
  }

  test("data_byName_holdsEveryRowUnderItsCanonicalNameOnly") {
    FxIndexData.byName should have size 16
    FxIndexData.rows.foreach { row =>
      withClue(s"${row.name}: ") {
        FxIndexData.byName.get(row.name) shouldBe Some(row)
      }
    }
    FxIndexData.byName.keySet shouldBe FxIndexData.rows.map(_.name).toSet

    // The keys are the canonical names alone: case-insensitive and alternate keys belong to the
    // family's lookup rather than to a row.
    FxIndexData.byName.get("eur/usd-ecb") shouldBe None
    FxIndexData.byName.get("EUR/USD-ECB ") shouldBe None
    FxIndexData.byName.get("USD/INR-RBIB-INR01") shouldBe None
    FxIndexData.byName.get("Rubbish") shouldBe None
  }

  test("data_compositeFixingAndMaturityCalendarsAreNormalised") {
    // The literals are the text of the published column and normalisation is the identifier's
    // business, so the first row's maturity calendar is named `CHZU+EUTA` for a column `EUTA+CHZU`.
    FxIndexData.byName.get("EUR/CHF-ECB").map(_.maturityCalendar.name) shouldBe Some("CHZU+EUTA")
    FxIndexData.byName.get("EUR/GBP-ECB").map(_.maturityCalendar.name) shouldBe Some("EUTA+GBLO")
    FxIndexData.byName.get("EUR/JPY-ECB").map(_.maturityCalendar.name) shouldBe Some("EUTA+JPTO")
    FxIndexData.byName.get("EUR/USD-ECB").map(_.maturityCalendar.name) shouldBe Some("EUTA+USNY")
    FxIndexData.byName.get("USD/CHF-WM").map(_.maturityCalendar.name) shouldBe Some("CHZU+USNY")
    FxIndexData.byName.get("EUR/USD-WM").map(_.maturityCalendar.name) shouldBe Some("EUTA+USNY")
    FxIndexData.byName.get("GBP/USD-WM").map(_.maturityCalendar.name) shouldBe Some("GBLO+USNY")
    FxIndexData.byName.get("USD/JPY-WM").map(_.maturityCalendar.name) shouldBe Some("JPTO+USNY")
    FxIndexData.byName.get("USD/THB-VWAP-THB01").map(_.fixingCalendar.name) shouldBe Some("SGSI+THBA")
    FxIndexData.byName.get("USD/THB-VWAP-THB01").map(_.maturityCalendar.name) shouldBe Some("SGSI+THBA")

    FxIndexData.byName.get("EUR/USD-ECB").map(_.maturityCalendar) shouldBe
      FxIndexData.byName.get("EUR/USD-WM").map(_.maturityCalendar)
  }

  test("data_fixingCalendars") {
    // The four European Central Bank rates fix on the euro settlement calendar and the four
    // WM/Reuters rates on the New York calendar. The eight remaining fix on the calendar of the
    // market publishing them, and none of those eight resolves against the standard set.
    FxIndexData.rows.filter(_.name.endsWith("-ECB")).map(_.fixingCalendar).distinct shouldBe
      Vector(HolidayCalendarIds.EUTA)
    FxIndexData.rows.filter(_.name.endsWith("-WM")).map(_.fixingCalendar).distinct shouldBe
      Vector(HolidayCalendarIds.USNY)

    val industry = FxIndexData.rows.filterNot(row => row.name.endsWith("-ECB") || row.name.endsWith("-WM"))
    industry should have size 8
    industry.map(_.fixingCalendar.name) shouldBe
      Vector("CLSA", "CNBE", "COBO", "INMU", "KRSE", "SGSI", "SGSI+THBA", "TWTA")
    industry.forall(row => row.fixingCalendar == row.maturityCalendar) shouldBe true
  }

  test("data_maturityDays") {
    // Two business days for every rate but two: zero for the Colombian peso index, one for the
    // Chilean.
    FxIndexData.rows.map(_.maturityDays).distinct.sorted shouldBe Vector(0, 1, 2)
    FxIndexData.byName.get("USD/COP-TRM-COP02").map(_.maturityDays) shouldBe Some(0)
    FxIndexData.byName.get("USD/CLP-DOLAR-OBS-CLP10").map(_.maturityDays) shouldBe Some(1)
    FxIndexData.rows.filter(_.maturityDays == 2) should have size 14
  }

  test("data_byCurrencyPair_groupsTheRowsSharingAPair") {
    // Fifteen groups over sixteen rows, because the euro/dollar rate is published twice - which is
    // why an index cannot be identified by its pair alone. Both its rows are here, in order.
    FxIndexData.byCurrencyPair should have size 15

    val eurUsd = CurrencyPair.of(Currency.EUR, Currency.USD)
    FxIndexData.byCurrencyPair.get(eurUsd).map(_.map(_.name)) shouldBe Some(Vector("EUR/USD-ECB", "EUR/USD-WM"))

    FxIndexData.byCurrencyPair.foreach {
      case (pair, grouped) =>
        withClue(s"$pair: ") {
          grouped should not be empty
          grouped.forall(_.currencyPair == pair) shouldBe true
          if (pair != eurUsd) {
            grouped should have size 1
          }
        }
    }
    FxIndexData.byCurrencyPair.values.flatten.map(_.name).toVector.sorted shouldBe
      FxIndexData.rows.map(_.name).sorted
    FxIndexData.rows.foreach { row =>
      withClue(s"${row.name}: ") {
        row.name should startWith(row.currencyPair.toString)
      }
    }
  }

  test("data_iterationOrderIsStable") {
    // Each table is read twice and the readings compared, so a per-read order would show up here.
    FxIndexData.rows shouldBe FxIndexData.rows
    FxIndexData.byName.toVector shouldBe FxIndexData.byName.toVector
    FxIndexData.byCurrencyPair.toVector shouldBe FxIndexData.byCurrencyPair.toVector
  }
}
