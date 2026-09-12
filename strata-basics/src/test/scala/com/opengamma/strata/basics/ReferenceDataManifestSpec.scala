/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import java.time.DayOfWeek
import java.time.MonthDay

import cats.effect.unsafe.implicits.global

import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.JsonObject
import io.circe.generic.semiauto.deriveDecoder
import io.circe.parser.parse

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.CurrencyData
import com.opengamma.strata.basics.currency.CurrencyPairData
import com.opengamma.strata.basics.date.BusinessDayConvention
import com.opengamma.strata.basics.date.BusinessDayConventions
import com.opengamma.strata.basics.date.DateSequence
import com.opengamma.strata.basics.date.DateSequences
import com.opengamma.strata.basics.date.HolidayCalendarData
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.basics.date.HolidayCalendarIds
import com.opengamma.strata.basics.date.PeriodAdditionConvention
import com.opengamma.strata.basics.date.PeriodAdditionConventions
import com.opengamma.strata.basics.date.StandardHolidayCalendars
import com.opengamma.strata.basics.index.FloatingRateNameData
import com.opengamma.strata.basics.index.FloatingRateType
import com.opengamma.strata.basics.index.FxIndexData
import com.opengamma.strata.basics.index.PriceIndexData
import com.opengamma.strata.basics.location.CountryData
import com.opengamma.strata.collect.io.Resources

/**
 * The transcription guard over the reference data of the port.
 *
 * ===Why this suite exists===
 *
 * Every reference data table of this module used to be a class path resource that the Java
 * implementation parsed while it ran; in the port each one is a Scala literal fixed at compile
 * time. That exchange removes a whole class of run time failure and makes the data visible in the
 * source, and it introduces exactly one new risk: a row can be '''mistranscribed'''. A wrong minor
 * unit digit, a currency pair quoted the wrong way round, a lenient pattern moved one place up the
 * list - none of these is detectable by any check the data can make on itself, because a
 * mistranscribed table is perfectly self consistent. A round trip through JSON returns it
 * unchanged, its own derived views agree with it, and every unit spec written against it passes.
 *
 * So the port is measured here against data captured from the '''Java''' implementation it
 * replaces. `tools/parity-capture/capture-baseline.jsh` enumerated every table from the Java side -
 * from the very INI and CSV resources the Scala literals were transcribed from, read through the
 * Java loaders - asserting its counts as it went, and wrote
 * `strata-basics/src/test/resources/manifest/reference-data-manifest.json`. This suite reads that
 * document and asserts, table by table and row by row, that the Scala data equals it. That is the
 * independent check AAP section 0.6.1 calls for, "so a mistranscribed row fails independently of
 * any self-round-trip", and it is the spec AAP section 0.10.1 Gate 5 (the user's Rule 4) requires
 * green.
 *
 * ===The document is the authority===
 *
 * The manifest is a generated artefact and is read only here. This suite never edits it, never
 * "corrects" a captured value and never drops a key from its coverage. Where the port and the
 * manifest disagree, the port is wrong until the read only Java resource behind the manifest says
 * otherwise, and the disagreement is reported rather than smoothed over. The schema of record is
 * section 7 of `tools/parity-capture/README.md`; strict JSON admits no comments, so the shape each
 * test relies on is restated at that test.
 *
 * ===What is covered, and what is declared pending===
 *
 * The port is delivered in slices and this checkpoint is one of them, so ten of the twenty nine
 * keys of the manifest have no Scala counterpart yet: their data objects and constant holders are
 * later files of the AAP section 0.3.1 inventory. Those keys are named, one by one, in
 * `PendingKeys` together with the file that will carry each, and five nested shapes of otherwise
 * covered keys are named the same way in `PendingNestedShapes`. Nothing else is exempt.
 *
 * The declaration is not a comment that can rot. `every top-level key of the manifest is either
 * asserted against the port or declared pending` compares the union of the covered set and the
 * pending set with the keys the document actually carries, and fails on a key that belongs to
 * neither. A key added to the manifest by a later capture, or a pending key left pending after its
 * data object lands, therefore fails this suite instead of quietly going unchecked - and the size
 * of the remaining gap is readable from the pending set rather than being invisible. The pending
 * keys are not left entirely unread either: the document's own counts for them are asserted
 * against the figures section 7 of the README publishes, so the artefact the later slices will be
 * measured against cannot drift while it waits for them.
 *
 * ===How the document is read===
 *
 * Once, for the whole suite. [[com.opengamma.strata.collect.io.Resources.readClasspathText]] is the
 * module's single reader of class path text and is what the parity harness beside this file uses;
 * the text is parsed and each key decoded into the typed view the tests assert from, so no test
 * re-reads or re-parses a quarter of a megabyte of JSON. The top level is kept as a
 * [[io.circe.JsonObject]] rather than decoded into a case class of twenty nine fields, because the
 * key set itself is something this suite asserts and a case class would silently ignore a key it
 * had no field for.
 *
 * ===Scope===
 *
 * This suite asserts '''data fidelity''' and nothing else. That a family is closed and that every
 * name round trips is `NamedEnumClosedSpec`; the behaviour built on these tables - the conventional
 * pair decision, the lenient rewrite chain, calendar resolution - belongs to the spec of the type
 * that owns it. Numerical parity is the business of the `parity` package. None of that is repeated
 * here.
 */
final class ReferenceDataManifestSpec extends AnyFunSuite with Matchers {

  import ReferenceDataManifestSpec._

  //-------------------------------------------------------------------------
  // Document identity and coverage.
  //-------------------------------------------------------------------------

  test("the document under assertion is the manifest the pinned capture script writes") {
    // These three keys are the document's identity. Asserting them is what stops another
    // generator's output, or a hand-edited file, from being mistaken for the captured baseline:
    // every other test in this suite treats the document as ground truth, so the document has to
    // prove it is the one the capture wrote. The key order is asserted with them, because it is
    // part of what makes the document byte-stable between captures.
    withClue(s"manifest resource '$ManifestResource': ") {
      schemaVersion shouldBe 1
      generator shouldBe "tools/parity-capture/capture-baseline.jsh"
      randomSeed shouldBe 20240117
      documentKeys.distinct.size shouldBe documentKeys.size
      documentKeys shouldBe DocumentedKeyOrder
    }
  }

  test("every top-level key of the manifest is either asserted against the port or declared pending") {
    // The honesty check of this suite. It reads the keys the document actually carries, so it fails
    // the day the capture emits a key nobody extended this suite for, and it fails equally if a
    // key is declared pending and covered at once.
    val pending = PendingKeys.keySet
    val declaration = PendingKeys.toVector.sorted
      .map { case (key, file) => s"$key -> $file" }
      .mkString("; ")
    withClue(s"${CoveredKeys.size} keys covered; ${pending.size} declared pending ($declaration): ") {
      CoveredKeys.intersect(pending) shouldBe empty
      (CoveredKeys ++ pending) shouldBe documentKeys.toSet
    }
  }

  test("every nested shape of a partially covered key is either asserted or declared pending") {
    // Three covered keys are only partly portable today: their remaining shapes name families that
    // do not exist yet. Coverage is therefore stated one level deeper for exactly those keys, so
    // that a shape added, renamed or left behind inside one of them is reported rather than
    // subsumed by the top-level key being "covered".
    PartiallyCoveredKeys.foreach {
      case (key, coveredShapes) =>
        val declaredPending = PendingNestedShapes.keySet.collect {
          case shape if shape.startsWith(s"$key.") => shape.drop(key.length + 1)
        }
        val present = nestedKeysOf(key)
        withClue(s"manifest key '$key' carries ${present.toVector.sorted.mkString(", ")}: ") {
          coveredShapes.intersect(declaredPending) shouldBe empty
          (coveredShapes ++ declaredPending) shouldBe present
        }
        ()
    }
    succeed
  }

  test("the manifest keys with no Scala counterpart yet carry the counts the schema documents") {
    // A pending key is not an unread key. The Scala side cannot be compared yet, but the document's
    // own shape is pinned here - the declared count against the length of the list, and the length
    // against the figure section 7 of the README states.
    PendingNameGroupCounts.foreach {
      case (key, documentedCount) =>
        val group = nameGroup(key)
        withClue(s"manifest key '$key' (pending: ${PendingKeys(key)}): ") {
          group.count shouldBe documentedCount
          group.names.size shouldBe documentedCount
          group.names.count(_.trim.isEmpty) shouldBe 0
        }
        ()
    }
    succeed
  }

  test("the pending index tables carry one fully populated row per documented CSV row") {
    // `iborIndices` and `overnightIndices` are `{count, headers[], rows[{<header>: value}]}`, and
    // the promise of that shape is that every CSV column survives on every row. A row missing a
    // column would silently become a defaulted field once `IborIndexData.scala` is written against
    // this document, so the completeness of the rows is asserted now rather than then.
    PendingIndexTableCounts.foreach {
      case (key, documentedCount) =>
        val table = indexTable(key)
        withClue(s"manifest key '$key' (pending: ${PendingKeys(key)}): ") {
          table.count shouldBe documentedCount
          table.rows.size shouldBe documentedCount
          table.headers should not be empty
          table.rows.zipWithIndex.foreach {
            case (row, index) =>
              withClue(s"row $index: ") {
                row.keySet shouldBe table.headers.toSet
              }
              ()
          }
        }
        ()
    }
    succeed
  }

  test("the pending alternate-name tables record the INI rows and the API-expanded rows") {
    // `alternateNames` records two counts on purpose: the INI `[alternates]` rows are what a port
    // transcribes, while the runtime view additionally registers an upper-case key for every
    // mixed-case alternate - ten INI rows become thirteen API entries for `OvernightIndex`. Both
    // counts, and the inclusion of the former in the latter, are the contract `Index.scala` will be
    // held to, so they are pinned here.
    withClue(s"manifest key 'alternateNames' (pending: ${PendingKeys("alternateNames")}): ") {
      alternateNames.keySet shouldBe DocumentedAlternateNameCounts.keySet
    }
    DocumentedAlternateNameCounts.foreach {
      case (family, (documentedIniRows, documentedApiRows)) =>
        val table = alternateNames(family)
        withClue(s"manifest alternateNames.$family: ") {
          table.iniRowCount shouldBe documentedIniRows
          table.iniRows.size shouldBe documentedIniRows
          table.apiExpandedRowCount shouldBe documentedApiRows
          table.apiExpandedRows.size shouldBe documentedApiRows
          table.apiExpandedRows should contain allElementsOf table.iniRows
        }
        ()
    }
    succeed
  }

  test("the pending nested shapes carry the counts the schema documents") {
    // The same treatment as the pending keys, one level down: the family behind each of these
    // shapes does not exist yet, so its rows cannot be compared, but the shape and the size of the
    // captured table are pinned so the document cannot drift before the family arrives.
    withClue(
      s"manifest floatingRateNames.constants " +
        s"(pending: ${PendingNestedShapes("floatingRateNames.constants")}): ") {
      floatingRateNames.constants.count shouldBe DocumentedFloatingRateConstantCount
      floatingRateNames.constants.names.size shouldBe DocumentedFloatingRateConstantCount
      floatingRateNames.constants.names.distinct.size shouldBe DocumentedFloatingRateConstantCount
    }
    DocumentedPendingExternalGroups.foreach {
      case (family, groups) =>
        val captured = externalNames(family)
        withClue(
          s"manifest externalNames.$family " +
            s"(pending: ${PendingNestedShapes(s"externalNames.$family")}): ") {
          captured.keySet shouldBe groups.keySet
          groups.foreach {
            case (group, documentedCount) =>
              withClue(s"group '$group': ") {
                captured(group).count shouldBe documentedCount
                captured(group).rows.size shouldBe documentedCount
              }
              ()
          }
        }
        ()
    }
    DocumentedPendingLenientCounts.foreach {
      case (family, documentedCount) =>
        withClue(
          s"manifest lenientPatterns.$family " +
            s"(pending: ${PendingNestedShapes(s"lenientPatterns.$family")}): ") {
          lenientPatterns(family).count shouldBe documentedCount
          lenientPatternRows(family).size shouldBe documentedCount
        }
        ()
    }
    succeed
  }

  //-------------------------------------------------------------------------
  // Currencies, the market convention ordering and currency pairs.
  //-------------------------------------------------------------------------

  test("CurrencyData.rows equals the manifest currencies, row for row and in order") {
    // Shape: {count, historicCount, activeCount, rows[{code, minorUnitDigits,
    // triangulationCurrency, historic}]}, the rows in the declaration order of the Java currency
    // configuration. Order is asserted as well as content, because it is the order `Currency`
    // creates its 74 instances in and the order any report over the family follows.
    val expected = currencies.rows.map(row =>
      (row.code, row.minorUnitDigits, row.triangulationCurrency, row.historic))
    val actual = CurrencyData.rows.map(row =>
      (row.code, row.minorUnitDigits, row.triangulationCurrencyCode, row.historic))
    sameRows("currency", actual, expected)
    succeed
  }

  test("the currency family and its code views equal the manifest currency counts") {
    val expectedCodes = currencies.rows.map(_.code)
    withClue("manifest key 'currencies': ") {
      currencies.count shouldBe expectedCodes.size
      currencies.historicCount shouldBe currencies.rows.count(_.historic)
      currencies.activeCount shouldBe currencies.rows.count(row => !row.historic)
      // The three published views of the table, and the family built from it, are each compared
      // with the manifest rather than with one another: a derivation that agreed with a wrong table
      // would agree with it consistently.
      CurrencyData.codes shouldBe expectedCodes
      CurrencyData.historicCodes shouldBe currencies.rows.filter(_.historic).map(_.code)
      CurrencyData.nonHistoricCodes shouldBe currencies.rows.filterNot(_.historic).map(_.code)
      Currency.values.toList.map(_.code) shouldBe expectedCodes.toList
      CurrencyData.byCode.keySet shouldBe expectedCodes.toSet
      // Every triangulation currency has to be a currency of the table, which is what makes
      // `Currency.triangulationCurrency` total.
      currencies.rows.map(_.triangulationCurrency).toSet.diff(expectedCodes.toSet) shouldBe empty
    }
  }

  test("CurrencyData.marketConventionPriority equals the manifest ordering, in order") {
    // The one key of the manifest that is a bare JSON array, and deliberately so: the order decides
    // which currency of an unlisted pair becomes the base, so it is semantically load-bearing. It
    // is compared as a sequence; comparing it as a set would pass on any permutation and so would
    // assert nothing about the only property it has.
    withClue(s"manifest key 'marketConventionPriority': ${marketConventionPriority.mkString(", ")}: ") {
      CurrencyData.marketConventionPriority shouldBe marketConventionPriority
      // The derived index is what `CurrencyPair` reads, so it is held to the same ordering.
      CurrencyData.marketConventionPriorityIndex shouldBe marketConventionPriority.zipWithIndex.toMap
      marketConventionPriority.toSet.diff(CurrencyData.codes.toSet) shouldBe empty
    }
  }

  test("CurrencyPairData.rows equals the manifest currency pairs, row for row and in order") {
    // Shape: {count, rows[{pair, rateDigits}]}, one row per '''conventional''' direction, the
    // inverse direction deliberately absent. The pair is rendered `BASE/COUNTER` from the two
    // currencies of the port's row, so a pair transcribed the wrong way round fails here rather
    // than turning into a silently inverted market convention.
    val expected = currencyPairs.rows.map(row => (row.pair, row.rateDigits))
    val actual = CurrencyPairData.rows.map {
      case (base, counter, rateDigits) => (s"${base.code}/${counter.code}", rateDigits)
    }
    sameRows("currency pair", actual, expected)
    withClue("manifest key 'currencyPairs': ") {
      currencyPairs.count shouldBe currencyPairs.rows.size
      CurrencyPairData.rateDigitsByCurrencies.size shouldBe currencyPairs.rows.size
      // The derived lookup is keyed by the two currencies, so it is compared through the same
      // rendering as the rows.
      CurrencyPairData.rateDigitsByCurrencies.map {
        case ((base, counter), rateDigits) => (s"${base.code}/${counter.code}", rateDigits)
      }.toSet shouldBe expected.toSet
    }
  }

  //-------------------------------------------------------------------------
  // Countries.
  //-------------------------------------------------------------------------

  test("CountryData.alpha3ToAlpha2 equals the manifest countries, row for row") {
    // Shape: {count, rows[{alpha3, alpha2}]}. The manifest emits the rows in the order of the Java
    // properties file, which is alpha-2 order, while the port holds a map sorted by alpha-3; the
    // captured rows are therefore sorted by alpha-3 before the comparison, and the key and value
    // views are compared too so that neither an extra row in the port nor a missing one goes
    // unseen.
    val expected = countries.rows.map(row => (row.alpha3, row.alpha2)).sortBy(_._1)
    val actual = CountryData.alpha3ToAlpha2.toVector
    sameRows("country", actual, expected)
    withClue("manifest key 'countries': ") {
      countries.count shouldBe countries.rows.size
      CountryData.alpha3Codes.toVector shouldBe expected.map(_._1)
      // The inverse view is only well defined because the relation is a bijection, which the
      // captured rows have to agree with.
      CountryData.alpha2ToAlpha3.size shouldBe expected.size
      CountryData.alpha2Codes.toVector shouldBe expected.map(_._2).sorted
    }
  }

  //-------------------------------------------------------------------------
  // The two index tables that are ported at this checkpoint.
  //-------------------------------------------------------------------------

  test("PriceIndexData.rows equals the manifest price indices, row for row and in order") {
    // Shape: {count, headers[], rows[{<header>: value}]} with every value the raw CSV text. The
    // header list is asserted first, because the comparison below reads columns by name: a renamed
    // column would otherwise turn into a missing key and a far less obvious failure.
    withClue("manifest key 'priceIndices': ") {
      priceIndices.headers shouldBe PriceIndexHeaders
      priceIndices.count shouldBe priceIndices.rows.size
    }
    val expected = priceIndices.rows.map(row =>
      (
        row(PriceIndexNameHeader),
        row(PriceIndexCurrencyHeader),
        row(PriceIndexCountryHeader),
        row(PriceIndexActiveHeader),
        row(PriceIndexFrequencyHeader)))
    val actual = PriceIndexData.rows.map(row =>
      (
        row.name,
        row.currency.code,
        row.region.code,
        row.active.toString,
        row.publicationFrequency.name))
    sameRows("price index", actual, expected)
    succeed
  }

  test("FxIndexData.rows equals the manifest FX indices, row for row and in order") {
    // Same shape as the price index table. Two columns need a word. `Maturity Days` is CSV text and
    // is compared as the text of the port's integer. And a calendar column holds the text the CSV
    // declared - `EUTA+CHZU` - while a `HolidayCalendarId` normalises a composite name by
    // deduplicating and sorting its parts, so the same column reaches the port as `CHZU+EUTA`. The
    // column is therefore put through the identifier factory before it is compared, which is
    // exactly what the Java parser did with it; comparing the raw text would report a difference
    // that is normalisation rather than a mistranscription.
    withClue("manifest key 'fxIndices': ") {
      fxIndices.headers shouldBe FxIndexHeaders
      fxIndices.count shouldBe fxIndices.rows.size
    }
    val expected = fxIndices.rows.map(row =>
      (
        row(FxIndexNameHeader),
        s"${row(FxIndexBaseCurrencyHeader)}/${row(FxIndexCounterCurrencyHeader)}",
        HolidayCalendarId.of(row(FxIndexFixingCalendarHeader)).name,
        row(FxIndexMaturityDaysHeader),
        HolidayCalendarId.of(row(FxIndexMaturityCalendarHeader)).name))
    val actual = FxIndexData.rows.map(row =>
      (
        row.name,
        s"${row.currencyPair.base.code}/${row.currencyPair.counter.code}",
        row.fixingCalendar.name,
        row.maturityDays.toString,
        row.maturityCalendar.name))
    sameRows("FX index", actual, expected)
    succeed
  }

  //-------------------------------------------------------------------------
  // Floating rate names.
  //-------------------------------------------------------------------------

  test("the manifest floating rate name sections account for every transcribed row") {
    // Shape: {constants{count, names[]}, aliasRowCount, sections{<section>{count, rows[{key,
    // value}]}}}. The seven sections and their sizes are the division the port keeps, so the
    // section names are asserted before any of them is compared and the declared alias row count
    // is reconciled with the sections it totals.
    val sections = floatingRateNames.sections
    withClue(s"manifest key 'floatingRateNames': ${sections.keys.toVector.sorted.mkString(", ")}: ") {
      sections.keySet shouldBe DocumentedFloatingRateSectionCounts.keySet
      floatingRateNames.aliasRowCount shouldBe sections.values.map(_.rows.size).sum
      floatingRateNames.aliasRowCount shouldBe DocumentedFloatingRateSectionCounts.values.sum
    }
    DocumentedFloatingRateSectionCounts.foreach {
      case (section, documentedCount) =>
        withClue(s"manifest floatingRateNames.sections.$section: ") {
          sections(section).count shouldBe documentedCount
          sections(section).rows.size shouldBe documentedCount
        }
        ()
    }
    succeed
  }

  test("FloatingRateNameData's Ibor rows equal the manifest ibor section, in published order") {
    // The published value of an Ibor row is the '''stem''' of an index name, and the Java loader
    // appended a `-` to it because an Ibor index name is completed by a tenor: `GBP-LIBOR-` plus
    // `3M`. The port carries the appended form, so the manifest value is completed the same way
    // before the comparison - which is also what pins the appending itself, since a row that
    // forgot it would fail here.
    val expected = floatingRateSection(IborSection).map(row => (row.key, s"${row.value}-"))
    val actual = FloatingRateNameData.iborRows.map(row => (row.externalName, row.indexName))
    sameRows("floating rate name (ibor)", actual, expected)
    withClue("Ibor rows all carry the Ibor rate type: ") {
      FloatingRateNameData.iborRows.map(_.rateType).distinct shouldBe Vector(FloatingRateType.Ibor)
    }
  }

  test("FloatingRateNameData.iborFixingDateOffsets equals the manifest iborFixingDateOffset section") {
    // Three rows, published as a section of their own and consumed twice by the port: once as the
    // table itself and once as the `fixingDateOffsetDays` of the three Ibor rows they name. Both
    // are compared, because a table that agreed with the manifest while the rows it feeds did not
    // would leave the offsets missing from every consumer that reads a row.
    val expected = floatingRateSection(IborFixingDateOffsetSection).map(row => (row.key, row.value))
    val actual = FloatingRateNameData.iborFixingDateOffsets.toVector.map {
      case (externalName, days) => (externalName, days.toString)
    }
    sameRows("floating rate name fixing date offset", actual, expected)
    val expectedOnRows =
      expected.map { case (externalName, days) => (externalName, Some(days.toInt)) }
    val actualOnRows = FloatingRateNameData.iborRows
      .filter(row => row.fixingDateOffsetDays.isDefined)
      .map(row => (row.externalName, row.fixingDateOffsetDays))
    sameRows("floating rate name row carrying a fixing date offset", actualOnRows, expectedOnRows)
    succeed
  }

  test("FloatingRateNameData's overnight and price rows equal the manifest sections, in published order") {
    // The published value of a row of these three sections is a complete index name and is
    // transcribed unchanged, so no completion step applies here. Each section also fixes the rate
    // type of the rows it produces, which is what decides the kind of index a name converts to.
    sameRows(
      "floating rate name (overnight compounded)",
      FloatingRateNameData.overnightCompoundedRows.map(row => (row.externalName, row.indexName)),
      floatingRateSection(OvernightCompoundedSection).map(row => (row.key, row.value)))
    sameRows(
      "floating rate name (overnight averaged)",
      FloatingRateNameData.overnightAveragedRows.map(row => (row.externalName, row.indexName)),
      floatingRateSection(OvernightAveragedSection).map(row => (row.key, row.value)))
    sameRows(
      "floating rate name (price)",
      FloatingRateNameData.priceRows.map(row => (row.externalName, row.indexName)),
      floatingRateSection(PriceSection).map(row => (row.key, row.value)))
    withClue("floating rate name rate types and section order: ") {
      FloatingRateNameData.overnightCompoundedRows.map(_.rateType).distinct shouldBe
        Vector(FloatingRateType.OvernightCompounded)
      FloatingRateNameData.overnightAveragedRows.map(_.rateType).distinct shouldBe
        Vector(FloatingRateType.OvernightAveraged)
      FloatingRateNameData.priceRows.map(_.rateType).distinct shouldBe Vector(FloatingRateType.Price)
      // The four name sections together are the published name space, in the order the Java loader
      // processed them, and `rows` is that concatenation.
      FloatingRateNameData.rows.map(_.externalName) shouldBe
        (floatingRateSection(IborSection) ++
          floatingRateSection(OvernightCompoundedSection) ++
          floatingRateSection(OvernightAveragedSection) ++
          floatingRateSection(PriceSection)).map(_.key)
      FloatingRateNameData.byExternalName.size shouldBe FloatingRateNameData.rows.size
    }
  }

  test("FloatingRateNameData's currency default tables equal the manifest sections, in published order") {
    // Two sections keyed by currency code, whose values are '''external names''' rather than index
    // names - `CNY` defaults to `CNY-REPO` - which is why they are resolved through
    // `byExternalName` and not read as index names. That every value names a row of the table is
    // asserted too, since a default resolving to nothing would be a lookup failure no count could
    // reveal.
    val iborExpected =
      floatingRateSection(CurrencyDefaultIborSection).map(row => (row.key, row.value))
    val iborActual = FloatingRateNameData.currencyDefaultIbor.toVector.map {
      case (currency, externalName) => (currency.code, externalName)
    }
    sameRows("currency default Ibor rate", iborActual, iborExpected)
    val overnightExpected =
      floatingRateSection(CurrencyDefaultOvernightSection).map(row => (row.key, row.value))
    val overnightActual = FloatingRateNameData.currencyDefaultOvernight.toVector.map {
      case (currency, externalName) => (currency.code, externalName)
    }
    sameRows("currency default Overnight rate", overnightActual, overnightExpected)
    withClue("every currency default resolves to a transcribed row: ") {
      (iborExpected ++ overnightExpected)
        .map(_._2)
        .filterNot(externalName =>
          FloatingRateNameData.byExternalName.contains(externalName)) shouldBe empty
    }
  }

  //-------------------------------------------------------------------------
  // The three convention families that are ported at this checkpoint.
  //-------------------------------------------------------------------------

  test("BusinessDayConvention's members equal the manifest businessDayConventions constants") {
    // Shape: {count, names[]}, the names being those of the public constants of the Java
    // `BusinessDayConventions` holder, emitted in name order. The port's members are compared in
    // that order, and the constants holder is compared with the members, so a constant naming the
    // wrong member or a member with no constant is reported.
    val expected = nameGroup(BusinessDayConventionsKey)
    withClue(s"manifest key '$BusinessDayConventionsKey': ") {
      expected.count shouldBe expected.names.size
      BusinessDayConvention.values.toList.map(_.name).sorted shouldBe expected.names.toList
      BusinessDayConventionConstants.map(_.name).sorted shouldBe expected.names
      BusinessDayConventionConstants.toSet shouldBe BusinessDayConvention.values.toList.toSet
    }
  }

  test("BusinessDayConvention's external name groups equal the manifest externalNames rows") {
    // Shape: {<family>{<group>{count, rows[{externalName, standardName}]}}}, sorted so the bytes
    // are stable. These groups take part in no lookup - they are read through `externalNames` - so
    // nothing but a comparison with the captured rows pins them.
    val groups = externalNames(BusinessDayConventionFamily)
    withClue(s"manifest externalNames.$BusinessDayConventionFamily: ") {
      groups.keySet shouldBe DocumentedBusinessDayExternalGroups.keySet
      BusinessDayConvention.namedEnum.externalNameGroups shouldBe groups.keySet
    }
    DocumentedBusinessDayExternalGroups.foreach {
      case (group, documentedCount) =>
        val captured = groups(group)
        withClue(s"manifest externalNames.$BusinessDayConventionFamily.$group: ") {
          captured.count shouldBe documentedCount
          captured.rows.size shouldBe documentedCount
          BusinessDayConvention.namedEnum.externalNamesRaw(group) shouldBe
            Some(captured.rows.map(row => row.externalName -> row.standardName).toMap)
        }
        ()
    }
    succeed
  }

  test("BusinessDayConvention's lenient patterns equal the manifest rows, in file order") {
    // The lenient rewrite is '''sequential''' - each matching pattern rewrites the string before
    // the next is tried - so the manifest preserves the file order of the rows and the port holds
    // an ordered list. Comparing these as sets would accept a reordering that changes which text
    // resolves and to what, so they are compared as sequences.
    val expected = lenientPatternRows(BusinessDayConventionFamily).map(row => (row.key, row.value))
    val actual = BusinessDayConvention.namedEnum.lenientPatterns.toVector.map {
      case (pattern, replacement) => (pattern.regex, replacement)
    }
    sameRows(s"lenient pattern ($BusinessDayConventionFamily)", actual, expected)
    withClue(s"manifest lenientPatterns.$BusinessDayConventionFamily: ") {
      lenientPatterns(BusinessDayConventionFamily).count shouldBe expected.size
    }
  }

  test("PeriodAdditionConvention's members and lenient patterns equal the manifest") {
    val expectedNames = nameGroup(PeriodAdditionConventionsKey)
    withClue(s"manifest key '$PeriodAdditionConventionsKey': ") {
      expectedNames.count shouldBe expectedNames.names.size
      PeriodAdditionConvention.values.toList.map(_.name).sorted shouldBe expectedNames.names.toList
      PeriodAdditionConventionConstants.map(_.name).sorted shouldBe expectedNames.names
      PeriodAdditionConventionConstants.toSet shouldBe PeriodAdditionConvention.values.toList.toSet
    }
    val expectedPatterns =
      lenientPatternRows(PeriodAdditionConventionFamily).map(row => (row.key, row.value))
    val actualPatterns = PeriodAdditionConvention.namedEnum.lenientPatterns.toVector.map {
      case (pattern, replacement) => (pattern.regex, replacement)
    }
    sameRows(s"lenient pattern ($PeriodAdditionConventionFamily)", actualPatterns, expectedPatterns)
    withClue(s"manifest lenientPatterns.$PeriodAdditionConventionFamily: ") {
      lenientPatterns(PeriodAdditionConventionFamily).count shouldBe expectedPatterns.size
      // This family publishes no external names, and the capture recorded none for it, so the
      // absence is asserted rather than assumed.
      PeriodAdditionConvention.namedEnum.externalNameGroups shouldBe empty
      externalNames.keySet should not contain PeriodAdditionConventionFamily
    }
  }

  test("DateSequence's members equal the manifest dateSequences constants") {
    val expected = nameGroup(DateSequencesKey)
    withClue(s"manifest key '$DateSequencesKey': ") {
      expected.count shouldBe expected.names.size
      DateSequence.values.toList.map(_.name).sorted shouldBe expected.names.toList
      DateSequenceConstants.map(_.name).sorted shouldBe expected.names
      DateSequenceConstants.toSet shouldBe DateSequence.values.toList.toSet
      // The Java configuration declared no alternate, lenient or external table for this family and
      // the capture recorded none, so the port declares none either.
      DateSequence.namedEnum.lenientPatterns shouldBe empty
      DateSequence.namedEnum.alternateNames shouldBe empty
      DateSequence.namedEnum.externalNameGroups shouldBe empty
    }
  }

  //-------------------------------------------------------------------------
  // Holiday calendars.
  //-------------------------------------------------------------------------

  test("HolidayCalendarIds' constants equal the manifest holidayCalendarIds") {
    // Shape: {count, names[]}, the names of the public constants of the Java `HolidayCalendarIds`
    // holder in name order. The port's constants are named one by one in the companion below rather
    // than discovered reflectively, so a constant that disappeared is a compile error here and a
    // constant that was added is a failure of this comparison.
    val expected = nameGroup(HolidayCalendarIdsKey)
    withClue(s"manifest key '$HolidayCalendarIdsKey': ") {
      expected.count shouldBe expected.names.size
      HolidayCalendarIdConstants.map(_.name).sorted shouldBe expected.names
      HolidayCalendarIdConstants.map(_.name).distinct.size shouldBe HolidayCalendarIdConstants.size
    }
  }

  test("HolidayCalendarId.defaultByCurrency equals the manifest defaults, including resolvability") {
    // Shape: {count, resolvableCount, unresolvableCount, rows[{currency, calendarId,
    // resolvableAgainstStandardReferenceData}]}. Thirteen of the captured rows name calendars the
    // Java library did not ship either, so the capture recorded the mapping '''and''' whether it
    // resolves; both are asserted, because a port that quietly invented the missing thirteen would
    // otherwise pass. The lookup is also compared over the whole currency family, so a default the
    // port added for a currency the capture recorded no convention for is reported.
    val rows = holidayCalendarDefaultByCurrency.rows
    withClue(s"manifest key '$HolidayCalendarDefaultKey': ") {
      holidayCalendarDefaultByCurrency.count shouldBe rows.size
      holidayCalendarDefaultByCurrency.resolvableCount shouldBe
        rows.count(_.resolvableAgainstStandardReferenceData)
      holidayCalendarDefaultByCurrency.unresolvableCount shouldBe
        rows.count(row => !row.resolvableAgainstStandardReferenceData)
    }
    rows.foreach { row =>
      withClue(s"manifest default calendar row '${row.currency}': ") {
        Currency.of(row.currency).map(currency =>
          HolidayCalendarId.defaultByCurrency(currency).map(_.name)) shouldBe
          Right(Some(row.calendarId))
        HolidayCalendarId.of(row.calendarId).resolve(ReferenceData.standard).isRight shouldBe
          row.resolvableAgainstStandardReferenceData
      }
      ()
    }
    val declared = Currency.values.toList
      .flatMap(currency =>
        HolidayCalendarId.defaultByCurrency(currency).map(id => currency.code -> id.name))
      .toMap
    withClue("the port declares a default calendar for exactly the captured currencies: ") {
      declared shouldBe rows.map(row => row.currency -> row.calendarId).toMap
    }
  }

  test("StandardHolidayCalendars.all equals the manifest builtInHolidayCalendars") {
    // Shape: {count, names[]}, captured from the normalised registry of the Java library: the four
    // weekend and no-holiday calendars plus twenty-six dated ones. Each entry of the port's map is
    // keyed by the identifier its own calendar carries, so the key names are what the set is
    // compared through.
    val expected = nameGroup(BuiltInHolidayCalendarsKey)
    withClue(s"manifest key '$BuiltInHolidayCalendarsKey': ") {
      expected.count shouldBe expected.names.size
      StandardHolidayCalendars.all.keys.toVector.map(_.name).sorted shouldBe expected.names
      // A built-in calendar is what `ReferenceData.standard` answers with, so every captured name
      // has to resolve there as well as be present in the map.
      expected.names.filterNot(name =>
        HolidayCalendarId.of(name).resolve(ReferenceData.standard).isRight) shouldBe empty
    }
  }

  test("HolidayCalendarData.thba equals the manifest THBA year rows and weekend") {
    // Shape: {THBA{yearRowCount, weekend, rows[{year, dates}]}}. The dates of a row are the text
    // the Java configuration carried - `Jan03,Feb23,…` - and are parsed into month-days here rather
    // than the port's month-days being formatted into text, so the comparison cannot be affected by
    // the locale or the calendar data of the host. The weekend is emitted separately by the
    // capture, since counting it as a year row would report 76 rows instead of 75.
    val expected = thbaRows
    val actual = HolidayCalendarData.thba.toVector
    sameRows("THBA year", actual, expected)
    withClue(s"manifest key '$HolidayCalendarDataKey': ") {
      holidayCalendarDataThba.yearRowCount shouldBe expected.size
      HolidayCalendarData.thbaWeekendDays shouldBe thbaWeekend
      // The published rows are not filtered against the weekend, so the resolved dates are the rows
      // exactly as captured - one date per captured month-day.
      HolidayCalendarData.thbaHolidays.size shouldBe expected.map(_._2.size).sum
    }
  }

  //-------------------------------------------------------------------------
  // Element-wise comparison with a failure that names the offending row.
  //-------------------------------------------------------------------------

  /**
   * Asserts two row sequences are equal, reporting the '''first''' difference with its index and
   * both values.
   *
   * A single comparison of two vectors of 404 rows reports both vectors in full and leaves the
   * reader to find the difference, which for a table this size is the difference between a
   * diagnosable failure and an unreadable one. Sizes are compared first, so that a truncated or
   * extended table is reported as such rather than as a difference in whichever row happens to fall
   * off the end.
   *
   * @param what  how a row of this table is named in the failure message
   * @param actual  the rows the port holds
   * @param expected  the rows captured from the Java implementation
   */
  private def sameRows[A](what: String, actual: Vector[A], expected: Vector[A]): Unit = {
    withClue(s"$what rows: the port has ${actual.size}, the manifest ${expected.size}: ") {
      actual.size shouldBe expected.size
    }
    actual.indices.foreach { index =>
      val found = actual(index)
      val captured = expected(index)
      withClue(s"$what row $index: the port has '$found', the manifest '$captured': ") {
        found shouldBe captured
      }
      ()
    }
    ()
  }
}

/**
 * The manifest document, decoded once, and the declaration of what this suite covers.
 *
 * Everything here is computed while this object initialises, which happens once per run of the
 * suite: the resource is read once, parsed once, and each key decoded into the typed view the tests
 * read. A document whose shape has changed under a key therefore fails while this object
 * initialises, naming the key and the decoding failure, rather than producing a confusing
 * difference inside a comparison.
 *
 * Members are declared in dependency order - the key names first, then the document, then the
 * typed views derived from both - because the initialiser of every value of an object runs in
 * textual order, and a view that read a key name declared below it would read `null`.
 */
private object ReferenceDataManifestSpec {

  //-------------------------------------------------------------------------
  // Keys, families and the counts section 7 of the README documents, named once each.
  //-------------------------------------------------------------------------

  /** The captured document, named as the class loader sees it. */
  val ManifestResource: String = "manifest/reference-data-manifest.json"

  val BusinessDayConventionsKey: String = "businessDayConventions"
  val PeriodAdditionConventionsKey: String = "periodAdditionConventions"
  val DateSequencesKey: String = "dateSequences"
  val HolidayCalendarIdsKey: String = "holidayCalendarIds"
  val HolidayCalendarDefaultKey: String = "holidayCalendarDefaultByCurrency"
  val BuiltInHolidayCalendarsKey: String = "builtInHolidayCalendars"
  val HolidayCalendarDataKey: String = "holidayCalendarData"

  /** The one calendar whose dates were published as data rather than derived from rules. */
  val ThbaCalendarName: String = "THBA"

  val BusinessDayConventionFamily: String = "BusinessDayConvention"
  val PeriodAdditionConventionFamily: String = "PeriodAdditionConvention"

  val IborSection: String = "ibor"
  val IborFixingDateOffsetSection: String = "iborFixingDateOffset"
  val OvernightCompoundedSection: String = "overnightCompounded"
  val OvernightAveragedSection: String = "overnightAveraged"
  val PriceSection: String = "price"
  val CurrencyDefaultIborSection: String = "currencyDefaultIbor"
  val CurrencyDefaultOvernightSection: String = "currencyDefaultOvernight"

  /** The columns of the Java price index data, in the order the capture emits them. */
  val PriceIndexHeaders: Vector[String] =
    Vector("Name", "Currency", "Country", "Active", "Publication Frequency")

  val PriceIndexNameHeader: String = "Name"
  val PriceIndexCurrencyHeader: String = "Currency"
  val PriceIndexCountryHeader: String = "Country"
  val PriceIndexActiveHeader: String = "Active"
  val PriceIndexFrequencyHeader: String = "Publication Frequency"

  /** The columns of the Java FX index data, in the order the capture emits them. */
  val FxIndexHeaders: Vector[String] =
    Vector(
      "Name",
      "Base Currency",
      "Counter Currency",
      "Fixing Calendar",
      "Maturity Days",
      "Maturity Calendar")

  val FxIndexNameHeader: String = "Name"
  val FxIndexBaseCurrencyHeader: String = "Base Currency"
  val FxIndexCounterCurrencyHeader: String = "Counter Currency"
  val FxIndexFixingCalendarHeader: String = "Fixing Calendar"
  val FxIndexMaturityDaysHeader: String = "Maturity Days"
  val FxIndexMaturityCalendarHeader: String = "Maturity Calendar"

  /**
   * The seven floating rate name sections and their documented sizes.
   *
   * 159 + 3 + 156 + 6 + 30 + 23 + 27 = 404, which is the `aliasRowCount` the document declares.
   */
  val DocumentedFloatingRateSectionCounts: Map[String, Int] =
    Map(
      IborSection -> 159,
      IborFixingDateOffsetSection -> 3,
      OvernightCompoundedSection -> 156,
      OvernightAveragedSection -> 6,
      PriceSection -> 30,
      CurrencyDefaultIborSection -> 23,
      CurrencyDefaultOvernightSection -> 27)

  /** The documented number of `FloatingRateName` constants, whose family is not ported yet. */
  val DocumentedFloatingRateConstantCount: Int = 41

  /** The external name groups of the business day convention family, and their documented sizes. */
  val DocumentedBusinessDayExternalGroups: Map[String, Int] = Map("FpML" -> 5, "SWIFT" -> 3)

  /** The external name groups of the two families that are not ported yet, and their sizes. */
  val DocumentedPendingExternalGroups: Map[String, Map[String, Int]] =
    Map("DayCount" -> Map("FpML" -> 14, "SWIFT" -> 8), "RollConvention" -> Map("FpML" -> 44))

  /** The lenient pattern tables of the two families that are not ported yet, and their sizes. */
  val DocumentedPendingLenientCounts: Map[String, Int] = Map("DayCount" -> 67, "RollConvention" -> 11)

  /** The alternate name families, each with its documented INI and API-expanded row counts. */
  val DocumentedAlternateNameCounts: Map[String, (Int, Int)] =
    Map("IborIndex" -> ((1, 1)), "OvernightIndex" -> ((10, 13)), "FxIndex" -> ((1, 1)))

  /**
   * The top-level keys in the order section 7 of the README documents, which is the order the
   * capture writes them.
   *
   * Held as a sequence because the order is part of what makes the document byte-stable between
   * captures, and a reordering is a change to the artefact the later slices will read.
   */
  val DocumentedKeyOrder: Vector[String] =
    Vector(
      "schemaVersion",
      "generator",
      "randomSeed",
      "currencies",
      "marketConventionPriority",
      "currencyPairs",
      "countries",
      "iborIndices",
      "overnightIndices",
      "priceIndices",
      "fxIndices",
      "iborIndexConstants",
      "overnightIndexConstants",
      "priceIndexConstants",
      "fxIndexConstants",
      "floatingRateNames",
      "dayCounts",
      BusinessDayConventionsKey,
      "rollConventions",
      PeriodAdditionConventionsKey,
      DateSequencesKey,
      "stubConventions",
      HolidayCalendarIdsKey,
      HolidayCalendarDefaultKey,
      BuiltInHolidayCalendarsKey,
      HolidayCalendarDataKey,
      "externalNames",
      "lenientPatterns",
      "alternateNames")

  //-------------------------------------------------------------------------
  // What this suite covers, and what remains pending. Nothing is exempt from one of the two.
  //-------------------------------------------------------------------------

  /**
   * The keys asserted against the port by the tests of this suite.
   *
   * Three of them - `floatingRateNames`, `externalNames` and `lenientPatterns` - are covered for
   * the families that exist today and carry shapes that do not, which is why coverage is stated one
   * level deeper for them in [[PartiallyCoveredKeys]] and [[PendingNestedShapes]].
   */
  val CoveredKeys: Set[String] =
    Set(
      "schemaVersion",
      "generator",
      "randomSeed",
      "currencies",
      "marketConventionPriority",
      "currencyPairs",
      "countries",
      "priceIndices",
      "fxIndices",
      "floatingRateNames",
      BusinessDayConventionsKey,
      PeriodAdditionConventionsKey,
      DateSequencesKey,
      HolidayCalendarIdsKey,
      HolidayCalendarDefaultKey,
      BuiltInHolidayCalendarsKey,
      HolidayCalendarDataKey,
      "externalNames",
      "lenientPatterns")

  /**
   * The keys whose Scala counterpart does not exist at this checkpoint, each naming the file of the
   * AAP section 0.3.1 inventory that will carry it.
   *
   * This is a '''declaration of the remaining gap''', not a list of exemptions: the coverage test
   * compares this set plus [[CoveredKeys]] with the keys the document carries, so an entry that
   * stays here after its file lands is caught by the suite that names it - as is a key the capture
   * adds that nobody has assigned to either set. Each entry was checked against the tree rather
   * than assumed: the four index families of `index/Index.scala` are declared with empty member
   * tables at this checkpoint, and `date/DayCount.scala`, `schedule/RollConvention.scala` and
   * `schedule/StubConvention.scala` do not exist yet.
   */
  val PendingKeys: Map[String, String] =
    Map(
      "iborIndices" ->
        "strata-basics/src/main/scala/com/opengamma/strata/basics/index/IborIndexData.scala",
      "overnightIndices" ->
        "strata-basics/src/main/scala/com/opengamma/strata/basics/index/OvernightIndexData.scala",
      "iborIndexConstants" ->
        "strata-basics/src/main/scala/com/opengamma/strata/basics/index/IborIndices.scala",
      "overnightIndexConstants" ->
        "strata-basics/src/main/scala/com/opengamma/strata/basics/index/OvernightIndices.scala",
      "priceIndexConstants" ->
        "strata-basics/src/main/scala/com/opengamma/strata/basics/index/PriceIndices.scala",
      "fxIndexConstants" ->
        "strata-basics/src/main/scala/com/opengamma/strata/basics/index/FxIndices.scala",
      "dayCounts" ->
        "strata-basics/src/main/scala/com/opengamma/strata/basics/date/DayCount.scala",
      "rollConventions" ->
        "strata-basics/src/main/scala/com/opengamma/strata/basics/schedule/RollConvention.scala",
      "stubConventions" ->
        "strata-basics/src/main/scala/com/opengamma/strata/basics/schedule/StubConvention.scala",
      "alternateNames" ->
        "strata-basics/src/main/scala/com/opengamma/strata/basics/index/Index.scala")

  /**
   * The nested shapes of a covered key that have no Scala counterpart yet, each naming the file
   * that will carry it.
   *
   * `floatingRateNames.constants` enumerates the 41 members of the `FloatingRateName` family, whose
   * member table is empty until the index families it converts to are ported. The day count and
   * roll convention families own their own external name groups and lenient patterns, so those
   * shapes arrive with those files.
   */
  val PendingNestedShapes: Map[String, String] =
    Map(
      "floatingRateNames.constants" ->
        "strata-basics/src/main/scala/com/opengamma/strata/basics/index/FloatingRateName.scala",
      "externalNames.DayCount" ->
        "strata-basics/src/main/scala/com/opengamma/strata/basics/date/DayCount.scala",
      "externalNames.RollConvention" ->
        "strata-basics/src/main/scala/com/opengamma/strata/basics/schedule/RollConvention.scala",
      "lenientPatterns.DayCount" ->
        "strata-basics/src/main/scala/com/opengamma/strata/basics/date/DayCount.scala",
      "lenientPatterns.RollConvention" ->
        "strata-basics/src/main/scala/com/opengamma/strata/basics/schedule/RollConvention.scala")

  /** The covered nested shapes of each partially covered key. */
  val PartiallyCoveredKeys: Map[String, Set[String]] =
    Map(
      "floatingRateNames" -> Set("aliasRowCount", "sections"),
      "externalNames" -> Set(BusinessDayConventionFamily),
      "lenientPatterns" -> Set(BusinessDayConventionFamily, PeriodAdditionConventionFamily))

  /** The documented size of each pending named-constant group. */
  val PendingNameGroupCounts: Map[String, Int] =
    Map(
      "iborIndexConstants" -> 113,
      "overnightIndexConstants" -> 21,
      "priceIndexConstants" -> 9,
      "fxIndexConstants" -> 8,
      "dayCounts" -> 21,
      "rollConventions" -> 45,
      "stubConventions" -> 8)

  /** The documented size of each pending index table. */
  val PendingIndexTableCounts: Map[String, Int] =
    Map("iborIndices" -> 271, "overnightIndices" -> 35)

  //-------------------------------------------------------------------------
  // Row models. Every field name is the JSON key the capture writes.
  //-------------------------------------------------------------------------

  /** One currency row: `{code, minorUnitDigits, triangulationCurrency, historic}`. */
  final case class CurrencyManifestRow(
      code: String,
      minorUnitDigits: Int,
      triangulationCurrency: String,
      historic: Boolean)

  /** The currency table: `{count, historicCount, activeCount, rows[]}`. */
  final case class CurrencyManifestTable(
      count: Int,
      historicCount: Int,
      activeCount: Int,
      rows: Vector[CurrencyManifestRow])

  /** One conventional currency pair: `{pair, rateDigits}`. */
  final case class CurrencyPairManifestRow(pair: String, rateDigits: Int)

  /** The currency pair table: `{count, rows[]}`. */
  final case class CurrencyPairManifestTable(count: Int, rows: Vector[CurrencyPairManifestRow])

  /** One country row: `{alpha3, alpha2}`. */
  final case class CountryManifestRow(alpha3: String, alpha2: String)

  /** The country table: `{count, rows[]}`. */
  final case class CountryManifestTable(count: Int, rows: Vector[CountryManifestRow])

  /** A named-constant group: `{count, names[]}`, the names in name order. */
  final case class NameGroup(count: Int, names: Vector[String])

  /** One key/value row of a configuration section: `{key, value}`, in file order. */
  final case class KeyValueRow(key: String, value: String)

  /** One configuration section: `{count, rows[]}`. */
  final case class KeyValueSection(count: Int, rows: Vector[KeyValueRow])

  /** One external name row: `{externalName, standardName}`. */
  final case class ExternalNameRow(externalName: String, standardName: String)

  /** One external name group: `{count, rows[]}`. */
  final case class ExternalNameGroup(count: Int, rows: Vector[ExternalNameRow])

  /** One alternate name row: `{alternateName, standardName}`. */
  final case class AlternateNameRow(alternateName: String, standardName: String)

  /** The alternate names of one family, as INI rows and as the expanded runtime view. */
  final case class AlternateNameTable(
      iniRowCount: Int,
      iniRows: Vector[AlternateNameRow],
      apiExpandedRowCount: Int,
      apiExpandedRows: Vector[AlternateNameRow])

  /** An index table: `{count, headers[], rows[{<header>: value}]}`, values as raw text. */
  final case class IndexManifestTable(
      count: Int,
      headers: Vector[String],
      rows: Vector[Map[String, String]])

  /** The floating rate name table: the constants, the total alias row count, and the sections. */
  final case class FloatingRateNameManifestTable(
      constants: NameGroup,
      aliasRowCount: Int,
      sections: Map[String, KeyValueSection])

  /** One default calendar row: `{currency, calendarId, resolvableAgainstStandardReferenceData}`. */
  final case class HolidayCalendarDefaultRow(
      currency: String,
      calendarId: String,
      resolvableAgainstStandardReferenceData: Boolean)

  /** The default calendar table: `{count, resolvableCount, unresolvableCount, rows[]}`. */
  final case class HolidayCalendarDefaultTable(
      count: Int,
      resolvableCount: Int,
      unresolvableCount: Int,
      rows: Vector[HolidayCalendarDefaultRow])

  /** One THBA year row: `{year, dates}`, the dates as `MMMdd` text separated by commas. */
  final case class ThbaYearRow(year: String, dates: String)

  /** The THBA table: `{yearRowCount, weekend, rows[]}`. */
  final case class ThbaTable(yearRowCount: Int, weekend: String, rows: Vector[ThbaYearRow])

  // Declared in dependency order: a table's decoder captures the row decoder it needs while it is
  // itself initialised, so a row decoder below its table would be read as `null`.
  implicit val currencyRowDecoder: Decoder[CurrencyManifestRow] = deriveDecoder
  implicit val currencyTableDecoder: Decoder[CurrencyManifestTable] = deriveDecoder
  implicit val currencyPairRowDecoder: Decoder[CurrencyPairManifestRow] = deriveDecoder
  implicit val currencyPairTableDecoder: Decoder[CurrencyPairManifestTable] = deriveDecoder
  implicit val countryRowDecoder: Decoder[CountryManifestRow] = deriveDecoder
  implicit val countryTableDecoder: Decoder[CountryManifestTable] = deriveDecoder
  implicit val nameGroupDecoder: Decoder[NameGroup] = deriveDecoder
  implicit val keyValueRowDecoder: Decoder[KeyValueRow] = deriveDecoder
  implicit val keyValueSectionDecoder: Decoder[KeyValueSection] = deriveDecoder
  implicit val externalNameRowDecoder: Decoder[ExternalNameRow] = deriveDecoder
  implicit val externalNameGroupDecoder: Decoder[ExternalNameGroup] = deriveDecoder
  implicit val alternateNameRowDecoder: Decoder[AlternateNameRow] = deriveDecoder
  implicit val alternateNameTableDecoder: Decoder[AlternateNameTable] = deriveDecoder
  implicit val indexTableDecoder: Decoder[IndexManifestTable] = deriveDecoder
  implicit val floatingRateNameTableDecoder: Decoder[FloatingRateNameManifestTable] = deriveDecoder
  implicit val holidayCalendarDefaultRowDecoder: Decoder[HolidayCalendarDefaultRow] = deriveDecoder
  implicit val holidayCalendarDefaultTableDecoder: Decoder[HolidayCalendarDefaultTable] =
    deriveDecoder
  implicit val thbaYearRowDecoder: Decoder[ThbaYearRow] = deriveDecoder
  implicit val thbaTableDecoder: Decoder[ThbaTable] = deriveDecoder

  //-------------------------------------------------------------------------
  // The document, read and parsed once.
  //-------------------------------------------------------------------------

  /**
   * The top level of the manifest, as an object rather than as a case class.
   *
   * The key set is something this suite asserts - the coverage check reads it - and a case class
   * would discard a key it had no field for, which is precisely the change that check exists to
   * catch. Reading the text goes through the module's own class path reader, the same one the parity
   * harness uses, so this suite has no second way of reading a resource.
   */
  private val document: JsonObject = {
    val text = Resources.readClasspathText(ManifestResource).unsafeRunSync()
    parse(text)
      .flatMap(json =>
        json.asObject.toRight(
          DecodingFailure(
            s"the reference data manifest '$ManifestResource' is not a JSON object",
            Nil)))
      .fold(failure => throw new IllegalStateException(failure.getMessage, failure), identity)
  }

  /** The top-level keys, in the order the capture wrote them. */
  val documentKeys: Vector[String] = document.keys.toVector

  /**
   * Decodes one key of the document, failing with the key named when its shape is not the
   * documented one.
   *
   * @param key  the top-level key to decode
   * @return the decoded value
   */
  private def keyAs[A: Decoder](key: String): A =
    document(key) match {
      case None =>
        throw new IllegalStateException(
          s"the reference data manifest '$ManifestResource' has no key '$key'; the schema of " +
            "record is section 7 of tools/parity-capture/README.md")
      case Some(json) =>
        json
          .as[A]
          .fold(
            failure =>
              throw new IllegalStateException(
                s"the reference data manifest key '$key' does not have the documented shape: " +
                  failure.getMessage,
                failure),
            identity)
    }

  /** The nested keys of one top-level key. */
  def nestedKeysOf(key: String): Set[String] = keyAs[JsonObject](key).keys.toSet

  //-------------------------------------------------------------------------
  // The typed views the tests assert from.
  //-------------------------------------------------------------------------

  val schemaVersion: Int = keyAs[Int]("schemaVersion")
  val generator: String = keyAs[String]("generator")
  val randomSeed: Int = keyAs[Int]("randomSeed")
  val currencies: CurrencyManifestTable = keyAs[CurrencyManifestTable]("currencies")
  val marketConventionPriority: Vector[String] = keyAs[Vector[String]]("marketConventionPriority")
  val currencyPairs: CurrencyPairManifestTable = keyAs[CurrencyPairManifestTable]("currencyPairs")
  val countries: CountryManifestTable = keyAs[CountryManifestTable]("countries")
  val priceIndices: IndexManifestTable = keyAs[IndexManifestTable]("priceIndices")
  val fxIndices: IndexManifestTable = keyAs[IndexManifestTable]("fxIndices")

  val floatingRateNames: FloatingRateNameManifestTable =
    keyAs[FloatingRateNameManifestTable]("floatingRateNames")

  val holidayCalendarDefaultByCurrency: HolidayCalendarDefaultTable =
    keyAs[HolidayCalendarDefaultTable](HolidayCalendarDefaultKey)

  private val holidayCalendarData: Map[String, ThbaTable] =
    keyAs[Map[String, ThbaTable]](HolidayCalendarDataKey)

  val holidayCalendarDataThba: ThbaTable = holidayCalendarData(ThbaCalendarName)

  val externalNames: Map[String, Map[String, ExternalNameGroup]] =
    keyAs[Map[String, Map[String, ExternalNameGroup]]]("externalNames")

  val lenientPatterns: Map[String, KeyValueSection] =
    keyAs[Map[String, KeyValueSection]]("lenientPatterns")

  val alternateNames: Map[String, AlternateNameTable] =
    keyAs[Map[String, AlternateNameTable]]("alternateNames")

  /** A named-constant group of the document, by key. */
  def nameGroup(key: String): NameGroup = keyAs[NameGroup](key)

  /** An index table of the document, by key. */
  def indexTable(key: String): IndexManifestTable = keyAs[IndexManifestTable](key)

  /** The rows of one floating rate name section, in published order. */
  def floatingRateSection(section: String): Vector[KeyValueRow] =
    floatingRateNames.sections(section).rows

  /** The rows of one lenient pattern table, in file order. */
  def lenientPatternRows(family: String): Vector[KeyValueRow] = lenientPatterns(family).rows

  //-------------------------------------------------------------------------
  // The THBA rows, parsed from the captured text.
  //-------------------------------------------------------------------------

  /** The month abbreviations the captured configuration uses, indexed from January. */
  private val MonthAbbreviations: Vector[String] =
    Vector("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  /** The day abbreviations the captured weekend declaration uses. */
  private val DayAbbreviations: Map[String, DayOfWeek] =
    Map(
      "Mon" -> DayOfWeek.MONDAY,
      "Tue" -> DayOfWeek.TUESDAY,
      "Wed" -> DayOfWeek.WEDNESDAY,
      "Thu" -> DayOfWeek.THURSDAY,
      "Fri" -> DayOfWeek.FRIDAY,
      "Sat" -> DayOfWeek.SATURDAY,
      "Sun" -> DayOfWeek.SUNDAY)

  /**
   * Parses one `MMMdd` date of a captured THBA row, such as `Jan03`.
   *
   * The month abbreviation is resolved through the table above rather than through a date time
   * formatter, so the parse is independent of the locale and of the calendar data of the host: the
   * captured text is ASCII English by construction, and a formatter would make the outcome of this
   * comparison depend on the machine it ran on.
   *
   * @param year  the year the row is recorded under, for the failure message
   * @param token  the captured date
   * @return the month-day the token names
   */
  private def monthDayOf(year: String, token: String): MonthDay = {
    val month = MonthAbbreviations.indexOf(token.take(3)) + 1
    val day = token.drop(3)
    if (month == 0 || day.isEmpty || !day.forall(_.isDigit)) {
      throw new IllegalStateException(
        s"the reference data manifest THBA row '$year' holds '$token', which is not a date of " +
          "the documented 'MMMdd' form")
    }
    MonthDay.of(month, day.toInt)
  }

  /** The captured THBA year rows, in the form the port holds them in. */
  val thbaRows: Vector[(Int, List[MonthDay])] =
    holidayCalendarDataThba.rows.map(row =>
      (row.year.toInt, row.dates.split(",").toList.map(token => monthDayOf(row.year, token))))

  /** The captured THBA weekend, in the form the port holds it in. */
  val thbaWeekend: Set[DayOfWeek] =
    holidayCalendarDataThba.weekend
      .split(",")
      .toSet
      .map((abbreviation: String) =>
        DayAbbreviations.getOrElse(
          abbreviation,
          throw new IllegalStateException(
            s"the reference data manifest THBA weekend holds '$abbreviation', which is not a day " +
              "of the documented three-letter form")))

  //-------------------------------------------------------------------------
  // The constant holders of the port, named one by one.
  //-------------------------------------------------------------------------

  /**
   * The constants of `BusinessDayConventions`, named rather than discovered.
   *
   * The capture read the public constants of the Java holder reflectively; nothing here reflects
   * over anything, so the counterpart is this list. Naming each constant is what makes a constant
   * that disappeared a compile error in this file rather than a count that quietly fell by one.
   */
  val BusinessDayConventionConstants: Vector[BusinessDayConvention] =
    Vector(
      BusinessDayConventions.NO_ADJUST,
      BusinessDayConventions.FOLLOWING,
      BusinessDayConventions.MODIFIED_FOLLOWING,
      BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY,
      BusinessDayConventions.PRECEDING,
      BusinessDayConventions.MODIFIED_PRECEDING,
      BusinessDayConventions.NEAREST)

  /** The constants of `PeriodAdditionConventions`, named rather than discovered. */
  val PeriodAdditionConventionConstants: Vector[PeriodAdditionConvention] =
    Vector(
      PeriodAdditionConventions.NONE,
      PeriodAdditionConventions.LAST_DAY,
      PeriodAdditionConventions.LAST_BUSINESS_DAY)

  /** The constants of `DateSequences`, named rather than discovered. */
  val DateSequenceConstants: Vector[DateSequence] =
    Vector(
      DateSequences.QUARTERLY_IMM,
      DateSequences.QUARTERLY_IMM_6_SERIAL,
      DateSequences.QUARTERLY_IMM_3_SERIAL,
      DateSequences.MONTHLY_IMM,
      DateSequences.QUARTERLY_10TH,
      DateSequences.MONTHLY_1ST)

  /** The constants of `HolidayCalendarIds`, named rather than discovered. */
  val HolidayCalendarIdConstants: Vector[HolidayCalendarId] =
    Vector(
      HolidayCalendarIds.NO_HOLIDAYS,
      HolidayCalendarIds.SAT_SUN,
      HolidayCalendarIds.FRI_SAT,
      HolidayCalendarIds.THU_FRI,
      HolidayCalendarIds.GBLO,
      HolidayCalendarIds.FRPA,
      HolidayCalendarIds.DEFR,
      HolidayCalendarIds.CHZU,
      HolidayCalendarIds.EUTA,
      HolidayCalendarIds.USGS,
      HolidayCalendarIds.USNY,
      HolidayCalendarIds.NYFD,
      HolidayCalendarIds.NYSE,
      HolidayCalendarIds.JPTO,
      HolidayCalendarIds.AUSY,
      HolidayCalendarIds.BRBD,
      HolidayCalendarIds.CAMO,
      HolidayCalendarIds.CATO,
      HolidayCalendarIds.CZPR,
      HolidayCalendarIds.DKCO,
      HolidayCalendarIds.HUBU,
      HolidayCalendarIds.MXMC,
      HolidayCalendarIds.NOOS,
      HolidayCalendarIds.NZAU,
      HolidayCalendarIds.NZWE,
      HolidayCalendarIds.PLWA,
      HolidayCalendarIds.SEST,
      HolidayCalendarIds.THBA,
      HolidayCalendarIds.ZAJO)
}
