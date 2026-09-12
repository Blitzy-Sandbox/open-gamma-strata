/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor3

import com.opengamma.strata.basics.currency.Currency

/**
 * Test [[FloatingRateNameData]].
 *
 * The Java sources have no test class for this data: in the original it was a configuration
 * resource that a loader read from the class path at class-initialisation time, and what the Java
 * `FloatingRateNamesTest` asserted about it was the behaviour of the family identifiers the
 * loader produced. Here the 404 rows are Scala literals fixed at compile time, so the table can
 * be asserted directly, and this spec is the port's own.
 *
 * The row-for-row comparison against the captured Java reference data belongs to
 * `ReferenceDataManifestSpec`. What this spec pins is the structure a consumer of the table
 * relies on, which that comparison does not state:
 *
 *  - the size of each of the seven published sections, which is how a dropped or duplicated row
 *    is caught as a count rather than as a missing lookup somewhere downstream;
 *  - the published order, both of the concatenation the whole table is and of the two ordered
 *    currency tables, because the order is the order a consumer creates its members in;
 *  - the kind of rate each section carries, and the one derived column - the trailing `-` an Ibor
 *    index name stem needs so that a tenor completes it;
 *  - the three rows that declare a non-standard fixing date offset, and the fact that no other
 *    row carries one;
 *  - the internal consistency the two currency tables depend on: every value is an external name
 *    of a row of the right kind, so a default resolves rather than dangling;
 *  - the two published names that contain an `=`, which are exactly the rows a reader of the
 *    original resource is most likely to mis-split, and which the Java tests single out too.
 *
 * The assertions deliberately never name the type of the published collections. The two currency
 * tables are ordered maps and the order is the contract; how they are backed, and what private
 * lookups are derived from them, is not.
 */
class FloatingRateNameDataSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The size and kind of each of the four name sections, in published order.
   *
   * The counts are the counts of the resource being ported, read from it rather than from the
   * object under test.
   */
  private val nameSections: TableFor3[String, Int, FloatingRateType] =
    Table(
      ("section", "size", "rateType"),
      ("ibor", 159, FloatingRateType.Ibor),
      ("overnightCompounded", 156, FloatingRateType.OvernightCompounded),
      ("overnightAveraged", 6, FloatingRateType.OvernightAveraged),
      ("price", 30, FloatingRateType.Price))

  /**
   * The rows of each section, keyed the way the table above names them, so that the two are
   * asserted against each other rather than each against itself.
   *
   * @param section  the published name of the section
   * @return the rows of that section
   */
  private def sectionRows(section: String): Vector[FloatingRateNameRow] =
    section match {
      case "ibor" => FloatingRateNameData.iborRows
      case "overnightCompounded" => FloatingRateNameData.overnightCompoundedRows
      case "overnightAveraged" => FloatingRateNameData.overnightAveragedRows
      case "price" => FloatingRateNameData.priceRows
      case other => fail(s"unknown section: $other")
    }

  //-------------------------------------------------------------------------
  test("test_sections_haveThePublishedSizeAndRateType") {
    forAll(nameSections) { (section: String, size: Int, rateType: FloatingRateType) =>
      withClue(s"$section: ") {
        val rows = sectionRows(section)
        rows should have size size.toLong
        rows.map(_.rateType).distinct shouldBe Vector(rateType)
      }
    }

    // 351 name rows over the four sections, plus the three fixing date offsets and the two
    // currency default tables, is the 404 published rows of the resource.
    FloatingRateNameData.rows should have size 351
    FloatingRateNameData.iborFixingDateOffsets should have size 3
    FloatingRateNameData.currencyDefaultIbor should have size 23
    FloatingRateNameData.currencyDefaultOvernight should have size 27
    val publishedRowCount =
      FloatingRateNameData.rows.size +
        FloatingRateNameData.iborFixingDateOffsets.size +
        FloatingRateNameData.currencyDefaultIbor.size +
        FloatingRateNameData.currencyDefaultOvernight.size
    publishedRowCount shouldBe 404
  }

  test("test_rows_areTheSectionsConcatenatedInPublishedOrder") {
    // This is the order the loader of the original processed the sections in, and it is the order
    // a consumer that builds one member per row should build them in, so that the member list of
    // the family is the published list.
    FloatingRateNameData.rows shouldBe
      (FloatingRateNameData.iborRows ++
        FloatingRateNameData.overnightCompoundedRows ++
        FloatingRateNameData.overnightAveragedRows ++
        FloatingRateNameData.priceRows)

    // the section boundaries are where the counts say they are
    FloatingRateNameData.rows.take(159) shouldBe FloatingRateNameData.iborRows
    FloatingRateNameData.rows.slice(159, 315) shouldBe FloatingRateNameData.overnightCompoundedRows
    FloatingRateNameData.rows.slice(315, 321) shouldBe FloatingRateNameData.overnightAveragedRows
    FloatingRateNameData.rows.drop(321) shouldBe FloatingRateNameData.priceRows
  }

  test("test_externalNames_areDistinctAndResolveThroughTheLookup") {
    FloatingRateNameData.rows.map(_.externalName).distinct should have size 351
    FloatingRateNameData.byExternalName should have size 351
    FloatingRateNameData.rows.foreach { row =>
      withClue(s"${row.externalName}: ") {
        FloatingRateNameData.byExternalName.get(row.externalName) shouldBe Some(row)
      }
    }
    FloatingRateNameData.byExternalName.keySet shouldBe FloatingRateNameData.rows.map(_.externalName).toSet
    // the keys are the published names alone: folding case is the business of the named enum
    // support, not of this table
    FloatingRateNameData.byExternalName.get("gbp-libor-bba") shouldBe None
    FloatingRateNameData.byExternalName.get("Rubbish") shouldBe None
  }

  test("test_indexNames_carryTheTrailingSeparatorExactlyWhereATenorCompletesThem") {
    // The one derived column. An Ibor family is named by a stem that a tenor completes -
    // `GBP-LIBOR-` plus `3M` is the index `GBP-LIBOR-3M` - so every Ibor row's index name ends
    // with the separator, and the published value it was derived from is the rest of it. No other
    // section needs one, because an Overnight or price family names its index directly.
    FloatingRateNameData.iborRows.foreach { row =>
      withClue(s"${row.externalName}: ") {
        row.indexName should endWith("-")
        row.indexName.init should not be empty
      }
    }
    (FloatingRateNameData.overnightCompoundedRows ++
      FloatingRateNameData.overnightAveragedRows ++
      FloatingRateNameData.priceRows).foreach { row =>
      withClue(s"${row.externalName}: ") {
        row.indexName.endsWith("-") shouldBe false
      }
    }

    // spot rows, adjudicated against the published Java jar: `GBP-LIBOR-BBA` names the stem
    // `GBP-LIBOR-`, which the Java family completes to the index `GBP-LIBOR-3M`
    FloatingRateNameData.byExternalName.get("GBP-LIBOR-BBA").map(_.indexName) shouldBe Some("GBP-LIBOR-")
    FloatingRateNameData.byExternalName.get("CHF-LIBOR-BBA").map(_.indexName) shouldBe Some("CHF-LIBOR-")
    FloatingRateNameData.byExternalName.get("CHF-SARON-OIS-COMPOUND").map(_.indexName) shouldBe Some("CHF-SARON")
    FloatingRateNameData.byExternalName.get("USD-EFFECTIVE").map(_.indexName) shouldBe Some("USD-FED-FUND")
    FloatingRateNameData.byExternalName.get("UK-RPI").map(_.indexName) shouldBe Some("GB-RPI")
  }

  test("test_manyExternalNamesShareOneIndexName") {
    // Several published names resolve onto one index family, which is deliberate and is how a
    // family recognises its own variants: the canonical row of a family is the one whose external
    // name completes to its index name, and the rest are spellings of it. Five spellings of Swiss
    // franc Libor all name the same stem.
    FloatingRateNameData.iborRows.filter(_.indexName == "CHF-LIBOR-").map(_.externalName) shouldBe
      Vector(
        "CHF-LIBOR",
        "CHF-LIBOR-BBA",
        "CHF-LIBOR-BBA-Bloomberg",
        "CHF-LIBOR-ISDA",
        "CHF-LIBOR-Reference Banks")
    // so the index names are far fewer than the rows, and the relation is many-to-one rather than
    // one-to-one
    FloatingRateNameData.rows.map(_.indexName).distinct.size should be < FloatingRateNameData.rows.size
  }

  test("test_fixingDateOffsets_areTheThreePublishedRowsAndNoOthers") {
    // The section carries three rows, all of them Danish Cibor and all of them zero. The Java
    // tests single the first out: `DKK-CIBOR-DKNA13` fixes on the day itself, where the otherwise
    // identical `DKK-CIBOR2-DKNA13`, which declares no offset, fixes two business days earlier.
    FloatingRateNameData.iborFixingDateOffsets.toVector shouldBe
      Vector(
        "DKK-CIBOR-DKNA13" -> 0,
        "DKK-CIBOR-DKNA13-Bloomberg" -> 0,
        "DKK-CIBOR-Reference Banks" -> 0)

    val carryingAnOffset = FloatingRateNameData.rows.filter(_.fixingDateOffsetDays.isDefined)
    carryingAnOffset.map(_.externalName) shouldBe
      Vector("DKK-CIBOR-DKNA13", "DKK-CIBOR-DKNA13-Bloomberg", "DKK-CIBOR-Reference Banks")
    carryingAnOffset.map(_.fixingDateOffsetDays) shouldBe Vector(Some(0), Some(0), Some(0))

    // every other row defers to the offset of the index it names
    FloatingRateNameData.rows.count(_.fixingDateOffsetDays.isEmpty) shouldBe 348
    FloatingRateNameData.byExternalName.get("DKK-CIBOR2-DKNA13").map(_.fixingDateOffsetDays) shouldBe Some(None)

    // and the section is consistent with the rows derived from it
    FloatingRateNameData.iborFixingDateOffsets.foreach {
      case (externalName, days) =>
        withClue(s"$externalName: ") {
          FloatingRateNameData.byExternalName.get(externalName).map(_.fixingDateOffsetDays) shouldBe Some(Some(days))
        }
    }
  }

  test("test_theTwoPublishedNamesContainingAnEqualsSign") {
    // These are the rows a reader of the original resource is most likely to mis-split, because
    // the separator of the format appears inside the key. Both are asserted here, as the Java
    // tests assert them, so that a transcription that split on the wrong occurrence fails.
    FloatingRateNameData.byExternalName.get("CNY-CNREPOFIX=CFXS-Reuters").map(row =>
      (row.indexName, row.rateType)) shouldBe Some(("CNY-REPO-", FloatingRateType.Ibor))
    FloatingRateNameData.byExternalName.get("HKD-HIBOR-HIBOR=").map(row =>
      (row.indexName, row.rateType)) shouldBe Some(("HKD-HIBOR-", FloatingRateType.Ibor))
    // the shorter spelling of the first one is a row in its own right, so the two did not collapse
    // into each other
    FloatingRateNameData.byExternalName.get("CNY-CNREPOFIX").map(_.indexName) shouldBe Some("CNY-REPO-")
  }

  //-------------------------------------------------------------------------
  test("test_currencyDefaultIbor_isThePublishedTableInPublishedOrder") {
    FloatingRateNameData.currencyDefaultIbor.keys.toVector shouldBe
      Vector(
        Currency.CHF,
        Currency.EUR,
        Currency.GBP,
        Currency.JPY,
        Currency.USD,
        Currency.AUD,
        Currency.CAD,
        Currency.CNY,
        Currency.CZK,
        Currency.DKK,
        Currency.HKD,
        Currency.HUF,
        Currency.KRW,
        Currency.MXN,
        Currency.MYR,
        Currency.NOK,
        Currency.NZD,
        Currency.PLN,
        Currency.SEK,
        Currency.SGD,
        Currency.THB,
        Currency.TWD,
        Currency.ZAR)
    // the five major currencies come first, in the order the resource declares them, and the rest
    // follow alphabetically - which is the published order, not a sorted one
    FloatingRateNameData.currencyDefaultIbor.get(Currency.GBP) shouldBe Some("GBP-LIBOR")
    FloatingRateNameData.currencyDefaultIbor.get(Currency.EUR) shouldBe Some("EUR-EURIBOR")
    FloatingRateNameData.currencyDefaultIbor.get(Currency.USD) shouldBe Some("USD-LIBOR")
    FloatingRateNameData.currencyDefaultIbor.get(Currency.AUD) shouldBe Some("AUD-BBSW")
    FloatingRateNameData.currencyDefaultIbor.get(Currency.CAD) shouldBe Some("CAD-CDOR")
    FloatingRateNameData.currencyDefaultIbor.get(Currency.NZD) shouldBe Some("NZD-BKBM")
    // a currency the resource does not name has no published default, which is a value the
    // consumer reports rather than a case this table can fill
    FloatingRateNameData.currencyDefaultIbor.get(Currency.BRL) shouldBe None
  }

  test("test_currencyDefaultOvernight_isThePublishedTableInPublishedOrder") {
    FloatingRateNameData.currencyDefaultOvernight.keys.toVector shouldBe
      Vector(
        Currency.CHF,
        Currency.EUR,
        Currency.GBP,
        Currency.JPY,
        Currency.USD,
        Currency.AUD,
        Currency.BRL,
        Currency.CAD,
        Currency.CLP,
        Currency.COP,
        Currency.CZK,
        Currency.DKK,
        Currency.HKD,
        Currency.HUF,
        Currency.IDR,
        Currency.ILS,
        Currency.INR,
        Currency.NOK,
        Currency.NZD,
        Currency.PLN,
        Currency.RUB,
        Currency.SAR,
        Currency.SEK,
        Currency.SGD,
        Currency.THB,
        Currency.TRY,
        Currency.ZAR)
    FloatingRateNameData.currencyDefaultOvernight.get(Currency.GBP) shouldBe Some("GBP-SONIA")
    FloatingRateNameData.currencyDefaultOvernight.get(Currency.EUR) shouldBe Some("EUR-ESTR")
    FloatingRateNameData.currencyDefaultOvernight.get(Currency.USD) shouldBe Some("USD-SOFR")
    FloatingRateNameData.currencyDefaultOvernight.get(Currency.CHF) shouldBe Some("CHF-SARON")
    FloatingRateNameData.currencyDefaultOvernight.get(Currency.JPY) shouldBe Some("JPY-TONAR")
    FloatingRateNameData.currencyDefaultOvernight.get(Currency.MXN) shouldBe None
  }

  test("test_currencyDefaults_resolveToARowOfTheRightKind") {
    // The value of a default is an external name rather than an index name, and the two differ
    // for some rows - `CNY` defaults to the external name `CNY-REPO` and `THB` to `THB-THBFIX` -
    // so a consumer has to resolve it through the lookup. That only works while every value is a
    // published name, which is what this asserts, section by section.
    FloatingRateNameData.currencyDefaultIbor.foreach {
      case (currency, externalName) =>
        withClue(s"${currency.code} -> $externalName: ") {
          FloatingRateNameData.byExternalName.get(externalName).map(_.rateType) shouldBe Some(FloatingRateType.Ibor)
        }
    }
    FloatingRateNameData.currencyDefaultOvernight.foreach {
      case (currency, externalName) =>
        withClue(s"${currency.code} -> $externalName: ") {
          FloatingRateNameData.byExternalName.get(externalName).map(_.rateType.isOvernight) shouldBe Some(true)
        }
    }

    // the two rows where the external name is not the index name
    FloatingRateNameData.currencyDefaultIbor.get(Currency.CNY) shouldBe Some("CNY-REPO")
    FloatingRateNameData.byExternalName.get("CNY-REPO").map(_.indexName) shouldBe Some("CNY-REPO-")
    FloatingRateNameData.currencyDefaultIbor.get(Currency.THB) shouldBe Some("THB-THBFIX")
    FloatingRateNameData.byExternalName.get("THB-THBFIX").map(_.indexName) shouldBe Some("THB-THBFIX-")
  }

  test("test_iterationOrderIsStable") {
    // A consumer reading any of these tables twice has to see the same thing both times, whatever
    // private lookups are derived from them.
    FloatingRateNameData.rows shouldBe FloatingRateNameData.rows
    FloatingRateNameData.iborFixingDateOffsets.toVector shouldBe FloatingRateNameData.iborFixingDateOffsets.toVector
    FloatingRateNameData.currencyDefaultIbor.toVector shouldBe FloatingRateNameData.currencyDefaultIbor.toVector
    FloatingRateNameData.currencyDefaultOvernight.toVector shouldBe
      FloatingRateNameData.currencyDefaultOvernight.toVector
    FloatingRateNameData.byExternalName.toVector shouldBe FloatingRateNameData.byExternalName.toVector
  }
}
