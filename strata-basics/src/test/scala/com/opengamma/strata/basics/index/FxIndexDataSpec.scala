/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor6

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.CurrencyPair
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.basics.date.HolidayCalendarIds

/**
 * Test [[FxIndexData]].
 *
 * The Java sources have no test class for this data: in the original it was a comma separated
 * resource that a loader read from the class path on first use, and what the Java `FxIndexTest`
 * asserted about it was the fields of the indices the loader produced. Here the sixteen rows are
 * Scala literals fixed at compile time, so the table can be asserted directly, and this spec is
 * the port's own.
 *
 * The row-for-row comparison against the captured Java reference data belongs to
 * `ReferenceDataManifestSpec`. What this spec pins is the table as a consumer of it sees it, and
 * this table is worth more than most: because the fallback that minted an index for an
 * unconfigured currency pair is deliberately not ported, the table alone fixes which pairs an FX
 * index exists for, so adding or omitting a row changes the public API rather than merely a
 * lookup. So the sixteen rows are transcribed here independently from the resource being ported,
 * with all six of their columns, and asserted against the object under test.
 *
 * Three properties beyond the rows themselves are asserted, each of which a consumer depends on:
 * the declaration order, which is the order the index family creates its members in and the order
 * within each group of the pair lookup; the normalisation of a composite calendar identifier,
 * which the literals of the table deliberately leave to the identifier factory exactly as the
 * Java parser did, so that the column text `EUTA+CHZU` yields the identifier named `CHZU+EUTA`;
 * and the one currency pair that two rows share, which is why an index cannot be identified by
 * its pair alone.
 */
class FxIndexDataSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The sixteen published rows with every column, in the declaration order of the data being
   * ported, transcribed from the resource of the original rather than from the object under test.
   *
   * The calendar columns are the text of the resource, so the composite ones are written in the
   * order the resource wrote them; the normalised name each yields is asserted separately.
   */
  private val publishedRows: TableFor6[String, Currency, Currency, String, Int, String] =
    Table(
      ("name", "base", "counter", "fixingCalendar", "maturityDays", "maturityCalendar"),
      // the European Central Bank rates
      ("EUR/CHF-ECB", Currency.EUR, Currency.CHF, "EUTA", 2, "EUTA+CHZU"),
      ("EUR/GBP-ECB", Currency.EUR, Currency.GBP, "EUTA", 2, "EUTA+GBLO"),
      ("EUR/JPY-ECB", Currency.EUR, Currency.JPY, "EUTA", 2, "EUTA+JPTO"),
      ("EUR/USD-ECB", Currency.EUR, Currency.USD, "EUTA", 2, "EUTA+USNY"),
      // the WM/Reuters rates
      ("USD/CHF-WM", Currency.USD, Currency.CHF, "USNY", 2, "USNY+CHZU"),
      ("EUR/USD-WM", Currency.EUR, Currency.USD, "USNY", 2, "USNY+EUTA"),
      ("GBP/USD-WM", Currency.GBP, Currency.USD, "USNY", 2, "USNY+GBLO"),
      ("USD/JPY-WM", Currency.USD, Currency.JPY, "USNY", 2, "USNY+JPTO"),
      // the rates named after the industry definitions that publish them
      ("USD/CLP-DOLAR-OBS-CLP10", Currency.USD, Currency.CLP, "CLSA", 1, "CLSA"),
      ("USD/CNY-SAEC-CNY01", Currency.USD, Currency.CNY, "CNBE", 2, "CNBE"),
      ("USD/COP-TRM-COP02", Currency.USD, Currency.COP, "COBO", 0, "COBO"),
      ("USD/INR-FBIL-INR01", Currency.USD, Currency.INR, "INMU", 2, "INMU"),
      ("USD/KRW-KFTC18-KRW02", Currency.USD, Currency.KRW, "KRSE", 2, "KRSE"),
      ("USD/SGD-VWAP-SGD3", Currency.USD, Currency.SGD, "SGSI", 2, "SGSI"),
      ("USD/THB-VWAP-THB01", Currency.USD, Currency.THB, "SGSI+THBA", 2, "SGSI+THBA"),
      ("USD/TWD-TAIFX1-TWD03", Currency.USD, Currency.TWD, "TWTA", 2, "TWTA"))

  //-------------------------------------------------------------------------
  test("test_rows_holdEveryPublishedRowWithEveryColumn") {
    FxIndexData.rows should have size 16

    forAll(publishedRows) {
      (
          name: String,
          base: Currency,
          counter: Currency,
          fixingCalendar: String,
          maturityDays: Int,
          maturityCalendar: String) =>
        withClue(s"$name: ") {
          FxIndexData.byName.get(name) shouldBe Some(
            FxIndexRow(
              name = name,
              currencyPair = CurrencyPair.of(base, counter),
              fixingCalendar = HolidayCalendarId.of(fixingCalendar),
              maturityDays = maturityDays,
              maturityCalendar = HolidayCalendarId.of(maturityCalendar)))
        }
    }
  }

  test("test_rows_areInTheDeclarationOrderOfTheOriginalData") {
    // The order is observable through any iteration a consumer performs - the order the family
    // creates its members in, and the order within each group of the pair lookup - so it is part
    // of the contract of this table.
    FxIndexData.rows.map(_.name) shouldBe publishedRows.map { case (name, _, _, _, _, _) => name }.toVector
    FxIndexData.rows.map(_.name).take(4) shouldBe
      Vector("EUR/CHF-ECB", "EUR/GBP-ECB", "EUR/JPY-ECB", "EUR/USD-ECB")
    FxIndexData.rows.map(_.name).last shouldBe "USD/TWD-TAIFX1-TWD03"
  }

  test("test_names_areDistinct") {
    FxIndexData.rows.map(_.name).distinct should have size 16
  }

  test("test_byName_holdsEveryRowUnderItsCanonicalNameOnly") {
    FxIndexData.byName should have size 16
    FxIndexData.rows.foreach { row =>
      withClue(s"${row.name}: ") {
        FxIndexData.byName.get(row.name) shouldBe Some(row)
      }
    }
    FxIndexData.byName.keySet shouldBe FxIndexData.rows.map(_.name).toSet

    // The keys are the canonical names alone. Registering each name a second time in upper case
    // was how the registry of the original answered a case-insensitive lookup, and an alternate
    // name - the original declared exactly one, for the Indian rupee index - is a property of the
    // family's lookup rather than of a row, so neither appears here.
    FxIndexData.byName.get("eur/usd-ecb") shouldBe None
    FxIndexData.byName.get("EUR/USD-ECB ") shouldBe None
    FxIndexData.byName.get("USD/INR-RBIB-INR01") shouldBe None
    FxIndexData.byName.get("Rubbish") shouldBe None
  }

  test("test_compositeFixingAndMaturityCalendarsAreNormalised") {
    // The literals of the table are the text of the resource column, and normalisation - the
    // parts deduplicated and sorted by name - is the identifier's business, which is precisely
    // how the Java parser handed each column to it. So the maturity calendar of the first row is
    // named `CHZU+EUTA` although the resource wrote `EUTA+CHZU`, and a consumer reading the
    // identifier sees the sorted form. Each expectation below was adjudicated against the
    // published Java jar.
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

    // the two rows whose maturity calendar is the same combination written in the opposite order
    // therefore carry the same identifier, which is what normalisation is for
    FxIndexData.byName.get("EUR/USD-ECB").map(_.maturityCalendar) shouldBe
      FxIndexData.byName.get("EUR/USD-WM").map(_.maturityCalendar)
  }

  test("test_fixingCalendars") {
    // The four European Central Bank rates fix on the euro settlement calendar and the four
    // WM/Reuters rates on the New York calendar; the eight remaining rates fix on the calendar of
    // the market that publishes them, seven of which this library ships no holidays for - because
    // the original shipped none either - so those indices resolve against the standard reference
    // data with a missing-data failure exactly as they did there.
    FxIndexData.rows.filter(_.name.endsWith("-ECB")).map(_.fixingCalendar).distinct shouldBe
      Vector(HolidayCalendarIds.EUTA)
    FxIndexData.rows.filter(_.name.endsWith("-WM")).map(_.fixingCalendar).distinct shouldBe
      Vector(HolidayCalendarIds.USNY)

    val industry = FxIndexData.rows.filterNot(row => row.name.endsWith("-ECB") || row.name.endsWith("-WM"))
    industry should have size 8
    industry.map(_.fixingCalendar.name) shouldBe
      Vector("CLSA", "CNBE", "COBO", "INMU", "KRSE", "SGSI", "SGSI+THBA", "TWTA")
    // each of those fixes and matures on the same calendar
    industry.forall(row => row.fixingCalendar == row.maturityCalendar) shouldBe true
  }

  test("test_maturityDays") {
    // Every rate matures two business days after its fixing except two: the Colombian peso index
    // matures on the fixing date itself and the Chilean peso index one business day later.
    FxIndexData.rows.map(_.maturityDays).distinct.sorted shouldBe Vector(0, 1, 2)
    FxIndexData.byName.get("USD/COP-TRM-COP02").map(_.maturityDays) shouldBe Some(0)
    FxIndexData.byName.get("USD/CLP-DOLAR-OBS-CLP10").map(_.maturityDays) shouldBe Some(1)
    FxIndexData.rows.filter(_.maturityDays == 2) should have size 14
  }

  test("test_byCurrencyPair_groupsTheRowsSharingAPair") {
    // Fifteen groups over sixteen rows, because the euro/dollar rate is published twice - by the
    // European Central Bank and by WM/Reuters. That is why an index cannot be identified by its
    // pair alone, and why the family resolves a pair to the matching row of lowest name; the
    // selection rule belongs to the family rather than to this table, so what is asserted here is
    // that both rows are present, in declaration order.
    FxIndexData.byCurrencyPair should have size 15

    val eurUsd = CurrencyPair.of(Currency.EUR, Currency.USD)
    FxIndexData.byCurrencyPair.get(eurUsd).map(_.map(_.name)) shouldBe Some(Vector("EUR/USD-ECB", "EUR/USD-WM"))

    // every other pair holds exactly one row, and every row is reachable through its own pair
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
    // the pair of a row is written base then counter, exactly as its name reads
    FxIndexData.rows.foreach { row =>
      withClue(s"${row.name}: ") {
        row.name should startWith(row.currencyPair.toString)
      }
    }
  }

  test("test_iterationOrderIsStable") {
    // A consumer reading these tables twice has to see the same thing both times, whatever private
    // lookups are derived from them.
    FxIndexData.rows shouldBe FxIndexData.rows
    FxIndexData.byName.toVector shouldBe FxIndexData.byName.toVector
    FxIndexData.byCurrencyPair.toVector shouldBe FxIndexData.byCurrencyPair.toVector
  }
}
