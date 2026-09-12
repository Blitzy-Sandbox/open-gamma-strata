/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor5

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.location.Country
import com.opengamma.strata.basics.schedule.Frequency

/**
 * Test [[PriceIndexData]].
 *
 * The Java sources have no test class for this data: in the original it was a comma separated
 * resource that a loader read from the class path on first use, and what the Java
 * `PriceIndexTest` asserted about it was the fields of a handful of the indices it produced.
 * Here the nine rows are Scala literals fixed at compile time, so the table can be asserted
 * directly, and this spec is the port's own.
 *
 * The row-for-row comparison against the captured Java reference data belongs to
 * `ReferenceDataManifestSpec`. What this spec pins is the table as a consumer of it sees it: the
 * nine rows with all five of their columns, transcribed here independently from the resource
 * being ported so that a mistranscription fails against a second reading of the same source; the
 * declaration order, which is the order the index family creates its members in and is therefore
 * observable; the derived lookup agreeing with the rows; and the two properties that hold of the
 * data as it stands rather than of the type - every index is published, and every index is
 * published monthly.
 */
class PriceIndexDataSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The nine published rows with every column, in the declaration order of the data being
   * ported, transcribed from the resource of the original rather than from the object under
   * test.
   */
  private val publishedRows: TableFor5[String, Currency, Country, Boolean, Frequency] =
    Table(
      ("name", "currency", "region", "active", "publicationFrequency"),
      ("GB-HICP", Currency.GBP, Country.GB, true, Frequency.P1M),
      ("GB-RPI", Currency.GBP, Country.GB, true, Frequency.P1M),
      ("GB-RPIX", Currency.GBP, Country.GB, true, Frequency.P1M),
      ("CH-CPI", Currency.CHF, Country.CH, true, Frequency.P1M),
      ("EU-AI-CPI", Currency.EUR, Country.EU, true, Frequency.P1M),
      ("EU-EXT-CPI", Currency.EUR, Country.EU, true, Frequency.P1M),
      ("JP-CPI-EXF", Currency.JPY, Country.JP, true, Frequency.P1M),
      ("US-CPI-U", Currency.USD, Country.US, true, Frequency.P1M),
      ("FR-EXT-CPI", Currency.EUR, Country.FR, true, Frequency.P1M))

  //-------------------------------------------------------------------------
  test("test_rows_holdEveryPublishedRowWithEveryColumn") {
    PriceIndexData.rows should have size 9

    val byName = PriceIndexData.rows.iterator.map(row => row.name -> row).toMap
    forAll(publishedRows) {
      (
          name: String,
          currency: Currency,
          region: Country,
          active: Boolean,
          publicationFrequency: Frequency) =>
        withClue(s"$name: ") {
          byName.get(name) shouldBe Some(
            PriceIndexRow(
              name = name,
              currency = currency,
              region = region,
              active = active,
              publicationFrequency = publicationFrequency))
        }
    }
  }

  test("test_rows_areInTheDeclarationOrderOfTheOriginalData") {
    // The order is observable through any iteration a consumer performs - notably the order the
    // index family creates its members in - so it is part of the contract of this table rather
    // than an accident of how it was written. It is the three sterling indices, then the Swiss,
    // the two European, the Japanese, the United States and the French index.
    PriceIndexData.rows.map(_.name) shouldBe
      Vector(
        "GB-HICP",
        "GB-RPI",
        "GB-RPIX",
        "CH-CPI",
        "EU-AI-CPI",
        "EU-EXT-CPI",
        "JP-CPI-EXF",
        "US-CPI-U",
        "FR-EXT-CPI")
    PriceIndexData.rows.map(_.name) shouldBe publishedRows.map { case (name, _, _, _, _) => name }.toVector
  }

  test("test_names_areDistinct") {
    // A name is the identity of an index, so two rows sharing one would make the lookup below
    // lose a row without any other symptom.
    PriceIndexData.rows.map(_.name).distinct should have size 9
  }

  test("test_byName_holdsEveryRowUnderItsCanonicalNameOnly") {
    PriceIndexData.byName should have size 9
    PriceIndexData.rows.foreach { row =>
      withClue(s"${row.name}: ") {
        PriceIndexData.byName.get(row.name) shouldBe Some(row)
      }
    }
    PriceIndexData.byName.keySet shouldBe PriceIndexData.rows.map(_.name).toSet

    // The keys are the canonical names alone. Registering each name a second time in upper case
    // was how the registry of the original answered a case-insensitive lookup; that
    // responsibility belongs to the named enum support now, so a folded name is absent here.
    PriceIndexData.byName.get("gb-rpi") shouldBe None
    PriceIndexData.byName.get("GB_RPI") shouldBe None
    PriceIndexData.byName.get("Rubbish") shouldBe None
  }

  test("test_everyIndexIsActiveAndPublishedMonthly") {
    // Both are properties of the data as it stands rather than constraints on the row type: the
    // type admits a discontinued index and any publication frequency, exactly as the column of
    // the original did, so an index that is later discontinued is a change of one literal.
    PriceIndexData.rows.forall(_.active) shouldBe true
    PriceIndexData.rows.map(_.publicationFrequency).distinct shouldBe Vector(Frequency.P1M)
  }

  test("test_regionsAndCurrencies") {
    // The three sterling indices measure the United Kingdom, the Swiss index Switzerland, the two
    // European indices the `EU` region rather than any member state, the Japanese index Japan and
    // the United States index the United States.
    PriceIndexData.rows.filter(_.currency == Currency.GBP).map(_.name) shouldBe
      Vector("GB-HICP", "GB-RPI", "GB-RPIX")
    PriceIndexData.rows.filter(_.region == Country.EU).map(_.name) shouldBe Vector("EU-AI-CPI", "EU-EXT-CPI")

    // The French index is the one row whose region is not the region of its currency: it is
    // quoted in euro and measures France. That is the row a transcription is most likely to get
    // wrong, so it is asserted on its own as well as in the table above.
    PriceIndexData.byName.get("FR-EXT-CPI").map(row => (row.currency, row.region)) shouldBe
      Some((Currency.EUR, Country.FR))
    PriceIndexData.rows.count(row => row.currency == Currency.EUR) shouldBe 3
  }

  test("test_iterationOrderIsStable") {
    // A consumer reading this table twice has to see the same thing both times, whatever private
    // lookups are derived from it.
    PriceIndexData.rows shouldBe PriceIndexData.rows
    PriceIndexData.byName.toVector shouldBe PriceIndexData.byName.toVector
  }
}
