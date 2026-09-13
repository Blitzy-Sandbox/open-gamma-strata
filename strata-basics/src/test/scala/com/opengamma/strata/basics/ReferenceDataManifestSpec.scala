/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import java.nio.ByteBuffer
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets
import java.time.DayOfWeek
import java.time.LocalDate
import java.time.MonthDay
import java.util.Locale

import scala.util.Try
import scala.util.Using
import scala.util.matching.Regex

import io.circe.Decoder
import io.circe.Json
import io.circe.JsonObject
import io.circe.KeyDecoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.parser.parse

import org.scalatest.Assertions
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.CurrencyData
import com.opengamma.strata.basics.currency.CurrencyPairData
import com.opengamma.strata.basics.date.BusinessDayConvention
import com.opengamma.strata.basics.date.BusinessDayConventions
import com.opengamma.strata.basics.date.DateSequence
import com.opengamma.strata.basics.date.DateSequences
import com.opengamma.strata.basics.date.DayCount
import com.opengamma.strata.basics.date.DayCounts
import com.opengamma.strata.basics.date.HolidayCalendarData
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.basics.date.HolidayCalendarIds
import com.opengamma.strata.basics.date.PeriodAdditionConvention
import com.opengamma.strata.basics.date.PeriodAdditionConventions
import com.opengamma.strata.basics.date.StandardHolidayCalendars
import com.opengamma.strata.basics.index.FloatingRateName
import com.opengamma.strata.basics.index.FloatingRateNameData
import com.opengamma.strata.basics.index.FloatingRateNames
import com.opengamma.strata.basics.index.FloatingRateType
import com.opengamma.strata.basics.index.FxIndex
import com.opengamma.strata.basics.index.FxIndexData
import com.opengamma.strata.basics.index.FxIndices
import com.opengamma.strata.basics.index.IborIndex
import com.opengamma.strata.basics.index.IborIndexData
import com.opengamma.strata.basics.index.IborIndexRow
import com.opengamma.strata.basics.index.IborIndices
import com.opengamma.strata.basics.index.OvernightIndex
import com.opengamma.strata.basics.index.OvernightIndexData
import com.opengamma.strata.basics.index.OvernightIndexRow
import com.opengamma.strata.basics.index.OvernightIndices
import com.opengamma.strata.basics.index.PriceIndex
import com.opengamma.strata.basics.index.PriceIndexData
import com.opengamma.strata.basics.index.PriceIndices
import com.opengamma.strata.basics.location.CountryData
import com.opengamma.strata.basics.parity.KeySchema
import com.opengamma.strata.basics.parity.ParityHarness
import com.opengamma.strata.basics.schedule.RollConvention
import com.opengamma.strata.basics.schedule.RollConventions
import com.opengamma.strata.basics.schedule.StubConvention

/**
 * The transcription guard over the reference data of this module.
 *
 * ===Why this suite exists===
 *
 * Every reference data table of this module is a Scala literal fixed at compile time, so a row can
 * be '''mistranscribed''' - and no check the data makes on itself detects that, because a
 * mistranscribed table is perfectly self consistent: a JSON round trip, its derived views and every
 * unit spec written against it all pass. Only an independently captured document catches it.
 * `tools/parity-capture/capture-baseline.jsh` enumerates every table from the '''Java'''
 * implementation this module replaces, asserting its counts as it goes, and writes
 * `strata-basics/src/test/resources/manifest/reference-data-manifest.json`; this suite compares the
 * Scala data with it table by table and row by row, which is the data-fidelity half of the
 * acceptance gate over closed families and reference data (Gate 5, the user's Rule 4). The document
 * is a generated artefact, read only here and never edited: where this module and the manifest
 * disagree, this module is wrong. Its schema of record is section 7 of
 * `tools/parity-capture/README.md`, and since strict JSON admits no comments the shape each test
 * relies on is restated at that test.
 *
 * ===A key nobody reads is a column nobody compares===
 *
 * A derived decoder ignores every key its model has no field for, so a column a later capture adds
 * would be dropped in silence while every comparison still passed over the columns it did read.
 * Every fixed-shape object is therefore checked against a declared key set '''before''' it is
 * decoded, through [[com.opengamma.strata.basics.parity.ParityHarness.strictObject]]; each set is a
 * [[com.opengamma.strata.basics.parity.KeySchema]] beside the model it describes, and `every
 * declared object schema is the key set the committed manifest carries` holds it against the
 * document. The shapes whose keys are '''data''' - an index row keyed by its table's headers, and
 * the families of `externalNames`, `lenientPatterns`, `alternateNames` and
 * `floatingRateNames.sections` - are asserted as sets against the header list or the documented
 * family set by the count and coverage tests.
 *
 * ===What is covered===
 *
 * All twenty-nine top-level keys and every nested shape of each: no sampled table, no exemption.
 * The coverage tests compare the union of the covered set with
 * [[ReferenceDataManifestSpec.PendingKeys]] and [[ReferenceDataManifestSpec.PendingNestedShapes]],
 * both empty, against the keys the document carries - at the top level and one level down for the
 * four keys whose content divides by family - so a key or shape a later capture adds fails this
 * suite rather than going unchecked. Counts are asserted twice: every declared count against the
 * length of its own list, which catches a truncated capture, and every table against the
 * '''literal''' figure the schema of record publishes, which catches a re-capture that shrank a
 * table and a module that shrank with it.
 *
 * ===How the document is read===
 *
 * Once, on first use, behind a `lazy val`: a bounded, total class path lookup capped at
 * [[ReferenceDataManifestSpec.ManifestByteCeiling]] bytes and decoded as UTF-8 explicitly, whose
 * result is an `Either` carrying the document or the reason it could not be obtained. Nothing is
 * read while this class or its companion initialises, so a resource that is absent, empty,
 * unparseable or not an object fails `the manifest resource is present on the class path, is
 * non-empty and parses as a JSON object` with the reason instead of aborting the suite with an
 * `ExceptionInInitializerError`. The top level stays a [[io.circe.JsonObject]] rather than a case
 * class of twenty-nine fields, because the key set is itself asserted.
 *
 * ===Scope===
 *
 * '''Data fidelity''', and beyond it only the THBA invariants below: that a family is closed and
 * that every name round trips is `NamedEnumClosedSpec`, the behaviour built on these tables belongs
 * to the type that owns it, and numerical parity to the `parity` package. The Thai bank calendar is
 * the only calendar of this module whose dates are published rather than derived from rules, and
 * [[com.opengamma.strata.basics.date.HolidayCalendarData]] has no spec of its own, so what the
 * manifest comparison cannot state is asserted here, beside it.
 */
final class ReferenceDataManifestSpec extends AnyFunSuite with Matchers {

  import ReferenceDataManifestSpec._

  //-------------------------------------------------------------------------
  // Document identity and coverage.
  //-------------------------------------------------------------------------

  test("the manifest resource is present on the class path, is non-empty and parses as a JSON object") {
    // The load is a test rather than an initialiser, and its four failures are distinguished by the
    // diagnostic, which names the resource and the script that writes it: a resource absent from
    // the class path is a build or path problem, one that does not parse is a capture problem.
    withClue(documentOrFailure.swap.getOrElse("the manifest loaded and is a JSON object")) {
      documentOrFailure.isRight shouldBe true
    }
    documentKeys should not be empty
    // The four refusals of the load, over input chosen for the purpose: the committed resource
    // exercises only the success path, so without these the guards between a corrupt resource and a
    // comparison stated over it would never have been observed to refuse anything.
    withClue("strict UTF-8 decoding: ") {
      // A lead byte of a two-byte sequence with its continuation byte missing: `new String(bytes,
      // UTF_8)` answers "\uFFFD", a strict decoder refuses it.
      decodeUtf8(Array[Byte](0xc3.toByte)).isLeft shouldBe true
      decodeUtf8(Array[Byte](0x41, 0xc3.toByte, 0xa9.toByte)) shouldBe Right("A\u00e9")
      decodeUtf8("{}".getBytes(StandardCharsets.UTF_8)) shouldBe Right("{}")
    }
    withClue("the three ways text is not the document: ") {
      documentOf("").swap.getOrElse("") should include("read as empty text")
      documentOf("   \n ").swap.getOrElse("") should include("read as empty text")
      documentOf("schemaVersion,generator").swap.getOrElse("") should include("is not valid JSON")
      documentOf("[1, 2, 3]").swap.getOrElse("") should include("is not a JSON object")
      documentOf("""{"schemaVersion":1}""").map(_.keys.toVector) shouldBe Right(
        Vector("schemaVersion"))
    }
  }

  test("the document under assertion is the manifest the pinned capture script writes") {
    // These three keys are the document's identity: they stop another generator's output, or a
    // hand-edited file, from being taken for the captured baseline. The key order is asserted with
    // them, being part of what makes the document byte-stable between captures.
    withClue(s"manifest resource '$ManifestResource': ") {
      schemaVersion shouldBe 1
      generator shouldBe "tools/parity-capture/capture-baseline.jsh"
      randomSeed shouldBe 20240117
      documentKeys.distinct.size shouldBe documentKeys.size
      documentKeys shouldBe DocumentedKeyOrder
    }
  }

  test("every top-level key of the manifest is either asserted against the port or declared pending") {
    // Read from the keys the document actually carries, so it fails the day the capture emits a key
    // nobody extended this suite for, and equally if a key is declared pending and covered at once.
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
    // Four keys divide their content by family rather than carrying one table, so coverage is
    // stated one level deeper for them: a shape added to, renamed in or dropped from one of them is
    // reported rather than subsumed by the top-level key being "covered".
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

  test("the manifest declares no runtime provider anywhere in the document") {
    // The families of this module are closed: no member is contributed at run time, so a
    // `providers` key would mean the capture had recorded a mechanism with no counterpart here. The
    // scan is recursive, because such a declaration would arrive inside its family, and reads key
    // names only - a captured *value* may mention a provider - case-insensitively.
    withClue(s"manifest resource '$ManifestResource' carries ${documentKeyNames.size} distinct " +
      "key names: ") {
      documentKeyNames.filter(_.toLowerCase(Locale.ENGLISH).contains("provider")) shouldBe empty
    }
  }

  test("every table of the manifest carries the row count the schema of record documents") {
    // The count assertion a self-consistent document cannot satisfy: elsewhere a declared count is
    // reconciled with its own list and the list compared with the module's own, which two tables
    // that both lost the same row would still pass. The figures below are the literals section 7 of
    // `tools/parity-capture/README.md` publishes, and each is asserted against the declared count
    // *and* the list length.
    DocumentedNameGroupCounts.foreach {
      case (key, documentedCount) =>
        val group = nameGroup(key)
        withClue(s"manifest key '$key': ") {
          group.count shouldBe documentedCount
          group.names.size shouldBe documentedCount
          group.names.count(_.trim.isEmpty) shouldBe 0
        }
        ()
    }
    DocumentedIndexTableCounts.foreach {
      case (key, documentedCount) =>
        val table = indexTable(key)
        withClue(s"manifest key '$key': ") {
          table.count shouldBe documentedCount
          table.rows.size shouldBe documentedCount
          table.headers should not be empty
          table.headers.distinct.size shouldBe table.headers.size
          // Every row has to carry every column: a row missing one would decode as an absent cell.
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
    withClue("manifest key 'currencies': ") {
      currencies.count shouldBe DocumentedCurrencyCount
      currencies.rows.size shouldBe DocumentedCurrencyCount
      currencies.historicCount shouldBe DocumentedHistoricCurrencyCount
      currencies.activeCount shouldBe DocumentedActiveCurrencyCount
      DocumentedHistoricCurrencyCount + DocumentedActiveCurrencyCount shouldBe
        DocumentedCurrencyCount
    }
    withClue("manifest key 'marketConventionPriority': ") {
      marketConventionPriority.size shouldBe DocumentedMarketConventionPriorityCount
    }
    withClue("manifest key 'currencyPairs': ") {
      currencyPairs.count shouldBe DocumentedCurrencyPairCount
      currencyPairs.rows.size shouldBe DocumentedCurrencyPairCount
    }
    withClue("manifest key 'countries': ") {
      countries.count shouldBe DocumentedCountryCount
      countries.rows.size shouldBe DocumentedCountryCount
    }
    withClue("manifest key 'floatingRateNames': ") {
      floatingRateNames.constants.count shouldBe DocumentedFloatingRateConstantCount
      floatingRateNames.constants.names.size shouldBe DocumentedFloatingRateConstantCount
      floatingRateNames.aliasRowCount shouldBe DocumentedFloatingRateSectionCounts.values.sum
    }
    DocumentedFloatingRateSectionCounts.foreach {
      case (section, documentedCount) =>
        withClue(s"manifest floatingRateNames.sections.$section: ") {
          floatingRateNames.sections(section).count shouldBe documentedCount
          floatingRateNames.sections(section).rows.size shouldBe documentedCount
        }
        ()
    }
    withClue(s"manifest key '$HolidayCalendarDefaultKey': ") {
      holidayCalendarDefaultByCurrency.count shouldBe DocumentedDefaultCalendarCount
      holidayCalendarDefaultByCurrency.rows.size shouldBe DocumentedDefaultCalendarCount
      holidayCalendarDefaultByCurrency.resolvableCount shouldBe
        DocumentedResolvableDefaultCalendarCount
      holidayCalendarDefaultByCurrency.unresolvableCount shouldBe
        DocumentedUnresolvableDefaultCalendarCount
    }
    withClue(s"manifest key '$HolidayCalendarDataKey' ($ThbaCalendarName): ") {
      holidayCalendarDataThba.yearRowCount shouldBe DocumentedThbaYearRowCount
      holidayCalendarDataThba.rows.size shouldBe DocumentedThbaYearRowCount
    }
    DocumentedExternalGroups.foreach {
      case (family, groups) =>
        withClue(s"manifest externalNames.$family: ") {
          externalNames(family).keySet shouldBe groups.keySet
          groups.foreach {
            case (group, documentedCount) =>
              withClue(s"group '$group': ") {
                externalNames(family)(group).count shouldBe documentedCount
                externalNames(family)(group).rows.size shouldBe documentedCount
              }
              ()
          }
        }
        ()
    }
    withClue("manifest key 'lenientPatterns': ") {
      lenientPatterns.keySet shouldBe DocumentedLenientCounts.keySet
    }
    DocumentedLenientCounts.foreach {
      case (family, documentedCount) =>
        withClue(s"manifest lenientPatterns.$family: ") {
          lenientPatterns(family).count shouldBe documentedCount
          lenientPatternRows(family).size shouldBe documentedCount
        }
        ()
    }
    withClue("manifest key 'alternateNames': ") {
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
        }
        ()
    }
    succeed
  }

  //-------------------------------------------------------------------------
  // The strict schema layer: no key of a decoded object goes unread.
  //
  // These five tests are about the reading of the document rather than the module's data, and
  // everything below them is only as strong as the decoding underneath: a derived decoder ignores a
  // key its model has no field for, so without them a column the capture added would be dropped in
  // silence while every comparison still passed over the columns it did read. Each test exercises
  // every documented shape, working from a specimen taken out of the committed document.
  //-------------------------------------------------------------------------

  test("every declared object schema is the key set the committed manifest carries") {
    // Every schema is matched against a specimen of its own shape taken from the document, so a key
    // the capture starts writing inside a table or a row is reported here, with the shape and the
    // path it came from. That every schema is uniform is asserted rather than assumed: the captured
    // document holds one key set per shape, so a documented shape is exact key-set equality.
    StrictShapes.foreach { shape =>
      withClue(
        s"${shape.schema.shape} at '${shape.path}', declared as ${shape.schema.describe}: ") {
        shape.schema.variants.size shouldBe 1
        shape.schema.optional shouldBe empty
        shape.specimenKeys shouldBe shape.schema.known
        shape.schema.matching(shape.specimenKeys) shouldBe Some(shape.schema.shape)
      }
      ()
    }
    withClue(s"${StrictShapes.size} shapes declared: ") {
      StrictShapes.size shouldBe DocumentedObjectShapeCount
      StrictShapes.map(_.schema.shape).distinct.size shouldBe StrictShapes.size
      StrictShapes.map(_.path).distinct.size shouldBe StrictShapes.size
    }
  }

  test("every declared object shape decodes the specimen the manifest carries") {
    // The acceptance half, and the reason the refusals below are evidence of anything: a check that
    // refused every object would pass the next three tests and leave the suite unable to read its
    // own document.
    StrictShapes.foreach { shape =>
      val outcome = shape.decode(Json.fromJsonObject(shape.specimen))
      val refusal = outcome.swap.toOption.map(_.getMessage).getOrElse("")
      withClue(s"${shape.schema.shape} at '${shape.path}' was refused: $refusal: ") {
        outcome.isRight shouldBe true
      }
      ()
    }
    succeed
  }

  test("an object shape carrying a key no schema knows is refused, naming the key") {
    // The case this layer exists for: a later capture adds a field, the model has no counterpart
    // for it, and a derived decoder would report a clean row with the new key's value uncompared.
    StrictShapes.foreach { shape =>
      val perturbed =
        shape.specimen.add(UncapturedKey, Json.fromString("a value no comparison here reads"))
      val refusal =
        refusalOf(shape, perturbed, s"a specimen carrying the extra key '$UncapturedKey'")
      withClue(s"${shape.schema.shape} was refused with: $refusal: ") {
        refusal should include(UncapturedKey)
        refusal should include(shape.schema.shape)
      }
      ()
    }
    succeed
  }

  test("an object shape missing a documented key is refused, naming the key") {
    // Every documented key of every shape, one at a time: a model whose field was made optional, or
    // a schema naming a key the objects need not carry, leaves exactly one of these unrefused.
    StrictShapes.foreach { shape =>
      shape.schema.known.toVector.sorted.foreach { key =>
        val refusal =
          refusalOf(
            shape,
            shape.specimen.remove(key),
            s"a specimen without the documented key '$key'")
        withClue(s"${shape.schema.shape} without '$key' was refused with: $refusal: ") {
          refusal should include(key)
        }
        ()
      }
    }
    succeed
  }

  test("an object shape whose documented key is renamed is refused, naming both spellings") {
    // A rename is the perturbation a decoder of optional fields cannot catch at all: the old key is
    // gone, so the field reads as absent, and the new key is unknown, so its value is dropped. The
    // message names the spelling that arrived as well as the one that left, since half of that
    // tells a reader the document changed and not into what.
    StrictShapes.foreach { shape =>
      shape.schema.known.toVector.sorted.foreach { key =>
        val renamed = s"capturedAs${key.capitalize}"
        val value =
          shape.specimen(key).getOrElse(fail(s"${shape.schema.shape} has no key '$key' to rename"))
        val refusal =
          refusalOf(
            shape,
            shape.specimen.remove(key).add(renamed, value),
            s"a specimen with '$key' renamed to '$renamed'")
        withClue(s"${shape.schema.shape} with '$renamed' was refused with: $refusal: ") {
          refusal should include(key)
          refusal should include(renamed)
        }
        ()
      }
    }
    succeed
  }

  /**
   * Decodes a perturbed specimen that must be refused, and answers the refusal's message.
   *
   * The decoder is the one this suite reads that shape with, not a copy of it, so a shape whose
   * strictness was removed fails here rather than being tested in a form nothing uses.
   *
   * @param shape  the documented shape being perturbed
   * @param perturbed  the object the decoder must refuse
   * @param what  how the perturbation is named in the failure message
   * @return the message the decoder refused with
   */
  private def refusalOf(shape: StrictShape, perturbed: JsonObject, what: String): String =
    shape.decode(Json.fromJsonObject(perturbed)) match {
      case Left(failure) => failure.getMessage
      case Right(decoded) =>
        fail(
          s"${shape.schema.shape}: $what decoded as '$decoded' instead of being refused. The " +
            s"shape's specimen is the manifest object at '${shape.path}' and its documented key " +
            s"set is ${shape.schema.describe}; a decoder that accepts an object whose keys are " +
            "not the documented ones leaves whatever the undocumented key carries unmeasured")
    }

  //-------------------------------------------------------------------------
  // Currencies, the market convention ordering and currency pairs.
  //-------------------------------------------------------------------------

  test("CurrencyData.rows equals the manifest currencies, row for row and in order") {
    // Shape: {count, historicCount, activeCount, rows[{code, minorUnitDigits,
    // triangulationCurrency, historic}]}, in the captured declaration order. Order is asserted as
    // well as content: it is the order `Currency` creates its 74 instances in.
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
      // Each published view is compared with the manifest rather than with the others: a derivation
      // of a wrong table agrees with it consistently.
      CurrencyData.codes shouldBe expectedCodes
      CurrencyData.historicCodes shouldBe currencies.rows.filter(_.historic).map(_.code)
      CurrencyData.nonHistoricCodes shouldBe currencies.rows.filterNot(_.historic).map(_.code)
      Currency.values.toList.map(_.code) shouldBe expectedCodes.toList
      CurrencyData.byCode.keySet shouldBe expectedCodes.toSet
      // Every triangulation currency is a currency of the table, which is what makes
      // `Currency.triangulationCurrency` total.
      currencies.rows.map(_.triangulationCurrency).toSet.diff(expectedCodes.toSet) shouldBe empty
    }
  }

  test("CurrencyData.marketConventionPriority equals the manifest ordering, in order") {
    // The one key of the manifest that is a bare JSON array, and deliberately so: the order decides
    // which currency of an unlisted pair becomes the base. It is compared as a sequence; as a set
    // it would pass on any permutation and so assert nothing about the only property it has.
    withClue(s"manifest key 'marketConventionPriority': ${marketConventionPriority.mkString(", ")}: ") {
      CurrencyData.marketConventionPriority shouldBe marketConventionPriority
      CurrencyData.marketConventionPriorityIndex shouldBe marketConventionPriority.zipWithIndex.toMap
      marketConventionPriority.toSet.diff(CurrencyData.codes.toSet) shouldBe empty
    }
  }

  test("CurrencyPairData.rows equals the manifest currency pairs, row for row and in order") {
    // Shape: {count, rows[{pair, rateDigits}]}, one row per '''conventional''' direction, the
    // inverse direction deliberately absent. The pair is rendered `BASE/COUNTER` from the two
    // currencies of the row, so a pair transcribed the wrong way round fails here rather than
    // becoming a silently inverted market convention.
    val expected = currencyPairs.rows.map(row => (row.pair, row.rateDigits))
    val actual = CurrencyPairData.rows.map {
      case (base, counter, rateDigits) => (s"${base.code}/${counter.code}", rateDigits)
    }
    sameRows("currency pair", actual, expected)
    withClue("manifest key 'currencyPairs': ") {
      currencyPairs.count shouldBe currencyPairs.rows.size
      CurrencyPairData.rateDigitsByCurrencies.size shouldBe currencyPairs.rows.size
      // The derived lookup is keyed by the two currencies, so it is rendered as the rows are.
      CurrencyPairData.rateDigitsByCurrencies.map {
        case ((base, counter), rateDigits) => (s"${base.code}/${counter.code}", rateDigits)
      }.toSet shouldBe expected.toSet
    }
  }

  //-------------------------------------------------------------------------
  // Countries.
  //-------------------------------------------------------------------------

  test("CountryData.alpha3ToAlpha2 equals the manifest countries, row for row") {
    // Shape: {count, rows[{alpha3, alpha2}]}. The captured rows are in alpha-2 order and this
    // module holds a map sorted by alpha-3, so they are sorted by alpha-3 first; the key and value
    // views are compared too, so neither an extra row nor a missing one goes unseen.
    val expected = countries.rows.map(row => (row.alpha3, row.alpha2)).sortBy(_._1)
    val actual = CountryData.alpha3ToAlpha2.toVector
    sameRows("country", actual, expected)
    withClue("manifest key 'countries': ") {
      countries.count shouldBe countries.rows.size
      CountryData.alpha3Codes.toVector shouldBe expected.map(_._1)
      // The inverse view is well defined only because the relation is a bijection.
      CountryData.alpha2ToAlpha3.size shouldBe expected.size
      CountryData.alpha2Codes.toVector shouldBe expected.map(_._2).sorted
    }
  }

  //-------------------------------------------------------------------------
  // The four published index tables.
  //-------------------------------------------------------------------------

  test("the captured cell normalisations are the Java forms, transcribed rather than looked up") {
    // `dayCountCell` and `calendarCell` bring a captured cell to the form the Java parser held it
    // in, transcribed rather than delegated - see the note in the Ibor index test - and a
    // transcription is worth what a test of it is worth. The expectations are literals read off the
    // two Java sources the helpers name, covering every branch of each.
    withClue(s"manifest key '$DayCountsKey' yields ${CapturedDayCountNames.size} spellings: ") {
      // Twenty-one canonical names and the ten upper case forms that differ from them.
      CapturedDayCountNames.size shouldBe 31
      nameGroup(DayCountsKey).names.foreach { name =>
        dayCountCell(name) shouldBe name
        ()
      }
      dayCountCell("ACT/360") shouldBe "Act/360"
      dayCountCell("Act/360") shouldBe "Act/360"
      dayCountCell("ACT/ACT ISDA") shouldBe "Act/Act ISDA"
      // A day count that carries a calendar has no captured spelling, so it passes through - as the
      // this module's member renders it - and so does text that names nothing.
      dayCountCell("Bus/252 BRBD") shouldBe "Bus/252 BRBD"
      dayCountCell("Act/366") shouldBe "Act/366"
      // The fold is English, not the default locale of the host.
      dayCountCell("act/360") shouldBe "act/360"
    }
    withClue("the composite calendar normalisation of HolidayCalendarId.java:87-120: ") {
      // A simple name is its own normal form; a combined name is deduplicated and sorted.
      calendarCell("GBLO") shouldBe "GBLO"
      calendarCell("SGSI+GBLO") shouldBe "GBLO+SGSI"
      calendarCell("USNY+EUTA") shouldBe "EUTA+USNY"
      calendarCell("GBLO+USNY") shouldBe "GBLO+USNY"
      calendarCell("USNY+GBLO+EUTA") shouldBe "EUTA+GBLO+USNY"
      calendarCell("GBLO+GBLO") shouldBe "GBLO"
      // `NoHolidays` contributes nothing to a combination and absorbs a link.
      calendarCell(s"GBLO+$NoHolidaysCalendarName") shouldBe "GBLO"
      calendarCell(s"$NoHolidaysCalendarName+GBLO") shouldBe "GBLO"
      calendarCell("USNY~GBLO") shouldBe "GBLO~USNY"
      calendarCell(s"GBLO~$NoHolidaysCalendarName") shouldBe NoHolidaysCalendarName
      // `~` is tested before `+`, so the combination inside this link is sorted before the link is.
      calendarCell("GBLO~USNY+EUTA") shouldBe "EUTA+USNY~GBLO"
    }
  }

  test("IborIndexData.rows equals the manifest Ibor indices, column by column and in order") {
    // Shape: {count, headers[], rows[{<header>: value}]}, every value the raw text of the CSV cell
    // the Java loader read. This is the largest table - 271 rows of thirteen columns - and it is
    // compared cell by cell, so a failure names the row, the index and the column instead of
    // printing two thirteen-element rows.
    //
    // Three columns are normalised first, each by the normalisation the Java parser performed on
    // it: a day count column through the spellings the captured `dayCounts` names imply, because
    // the fixed leg day count is spelled `ACT/360` on the Czech rows and `Act/360` on the rest; a
    // calendar column through the composite rule, because the text `SGSI+GBLO` reaches this module
    // as `GBLO+SGSI`; and the active column for case, which is not data. Everything else is
    // compared as captured, including the tenor convention column, which names a business day
    // convention on some rows and a period addition convention on others.
    //
    // None of the three consults a production type, deliberately: a lookup applied to the captured
    // text as well as to this module's value cancels out of the comparison, so a wrong lookup would
    // rewrite both sides identically and the rows would still agree - the self-round-trip this
    // suite exists to be independent of. `dayCountCell` and `calendarCell` are therefore local to
    // this file, built from the manifest's own names and the Java rules they cite.
    withClue("manifest key 'iborIndices': ") {
      iborIndices.headers shouldBe IborIndexHeaders
      iborIndices.count shouldBe iborIndices.rows.size
    }
    sameTable(
      "Ibor index",
      IborIndexHeaders,
      IborIndexData.rows.map(portIborCells),
      iborIndices.rows.map(capturedIborCells))
    withClue("the Ibor index names are distinct and the lookup covers every row: ") {
      IborIndexData.rows.map(_.name).distinct.size shouldBe IborIndexData.rows.size
      IborIndexData.byName.keySet shouldBe iborIndices.rows.map(_(IborIndexNameHeader)).toSet
    }
  }

  test("OvernightIndexData.rows equals the manifest overnight indices, column by column") {
    // The same shape and normalisations as the Ibor table; this one has eight columns, two of them
    // day counts, one of which is `Bus/252 BRBD` - a day count that carries a calendar and so is
    // not one of the twenty-one standard members. No captured spelling covers it, so it passes
    // through and is compared against the same text this module's member renders, unresolved.
    withClue("manifest key 'overnightIndices': ") {
      overnightIndices.headers shouldBe OvernightIndexHeaders
      overnightIndices.count shouldBe overnightIndices.rows.size
    }
    sameTable(
      "overnight index",
      OvernightIndexHeaders,
      OvernightIndexData.rows.map(portOvernightCells),
      overnightIndices.rows.map(capturedOvernightCells))
    withClue("the overnight index names are distinct and the lookup covers every row: ") {
      OvernightIndexData.rows.map(_.name).distinct.size shouldBe OvernightIndexData.rows.size
      OvernightIndexData.byName.keySet shouldBe
        overnightIndices.rows.map(_(OvernightIndexNameHeader)).toSet
    }
  }

  test("PriceIndexData.rows equals the manifest price indices, row for row and in order") {
    // Shape: {count, headers[], rows[{<header>: value}]} with every value the raw CSV text. The
    // header list is asserted first, because the comparison reads columns by name and a renamed
    // column would otherwise become a missing key.
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
    // Same shape as the price index table. Two columns need a word: `Maturity Days` is CSV text and
    // is compared as the text of this module's integer, and a calendar column holds the text the
    // CSV declared - `EUTA+CHZU` - which reaches this module as the normalised `CHZU+EUTA`, so the
    // captured text is brought to that form by `calendarCell` first. That normalisation is
    // transcribed rather than taken from `HolidayCalendarId.of`, which is a subject of this suite.
    withClue("manifest key 'fxIndices': ") {
      fxIndices.headers shouldBe FxIndexHeaders
      fxIndices.count shouldBe fxIndices.rows.size
    }
    val expected = fxIndices.rows.map(row =>
      (
        row(FxIndexNameHeader),
        s"${row(FxIndexBaseCurrencyHeader)}/${row(FxIndexCounterCurrencyHeader)}",
        calendarCell(row(FxIndexFixingCalendarHeader)),
        row(FxIndexMaturityDaysHeader),
        calendarCell(row(FxIndexMaturityCalendarHeader))))
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
  // The named constants of the four index families.
  //-------------------------------------------------------------------------

  test("IborIndices' constants equal the manifest iborIndexConstants") {
    // Shape: {count, names[]}, the captured constant names in name order. A constants holder is a
    // selection from its family - 113 of the 271 published Ibor indices have a constant - so two
    // things are asserted: this module's constant names equal the captured names, and every
    // constant is the member the family publishes under that name rather than a second instance
    // beside it.
    sameNamedConstants("Ibor index", IborIndexConstantsKey, IborIndexConstants.map(_.name))
    withClue("every Ibor index constant is the published member of that name: ") {
      IborIndexConstants.filterNot(index =>
        IborIndex.valueOf(index.name).contains(index)) shouldBe empty
      IborIndexConstants.toSet.subsetOf(IborIndex.values.toList.toSet) shouldBe true
    }
  }

  test("OvernightIndices' constants equal the manifest overnightIndexConstants") {
    // The captured names hold `EUR-ESTR` twice, which is why they are compared as a sequence rather
    // than a set: both `EUR_ESTR` and the retired `EUR_ESTER` name the same index, so twenty-one
    // names holding twenty distinct ones is what proves this module's two constants are one
    // instance.
    sameNamedConstants(
      "overnight index",
      OvernightIndexConstantsKey,
      OvernightIndexConstants.map(_.name))
    withClue("every overnight index constant is the published member of that name: ") {
      OvernightIndexConstants.filterNot(index =>
        OvernightIndex.valueOf(index.name).contains(index)) shouldBe empty
      OvernightIndexConstants.toSet.subsetOf(OvernightIndex.values.toList.toSet) shouldBe true
    }
  }

  test("PriceIndices' constants equal the manifest priceIndexConstants") {
    // Nine constants for nine published rows, so the constants and the family coincide and both
    // directions are asserted.
    sameNamedConstants("price index", PriceIndexConstantsKey, PriceIndexConstants.map(_.name))
    withClue("the price index constants are exactly the members of the family: ") {
      PriceIndexConstants.filterNot(index =>
        PriceIndex.valueOf(index.name).contains(index)) shouldBe empty
      PriceIndexConstants.toSet shouldBe PriceIndex.values.toList.toSet
    }
  }

  test("FxIndices' constants equal the manifest fxIndexConstants") {
    // Eight constants over sixteen published rows: the four European Central Bank and four
    // WM/Reuters fixings are named, the eight Asian and Latin American ones reached by name.
    sameNamedConstants("FX index", FxIndexConstantsKey, FxIndexConstants.map(_.name))
    withClue("every FX index constant is the published member of that name: ") {
      FxIndexConstants.filterNot(index =>
        FxIndex.valueOf(index.name).contains(index)) shouldBe empty
      FxIndexConstants.toSet.subsetOf(FxIndex.values.toList.toSet) shouldBe true
    }
  }

  test("the index families' alternate names equal the manifest alternateNames tables") {
    // Shape: {<family>{iniRowCount, iniRows[{alternateName, standardName}], apiExpandedRowCount,
    // apiExpandedRows[]}}. Two tables are captured per family on purpose: the INI rows are what is
    // transcribed, while the runtime view additionally registers the upper-case spelling of every
    // mixed-case alternate, so ten INI rows become thirteen entries for the overnight family.
    //
    // This module publishes the expanded view and keeps the transcribed table private, so the
    // comparison runs the other way: the expanded map against the captured expanded rows, and those
    // against the expansion of the captured INI rows. Together they pin the transcribed table,
    // because the expansion only ever adds an upper-case key.
    DocumentedAlternateNameCounts.keySet.foreach { family =>
      val table = alternateNames(family)
      val capturedIni = table.iniRows.map(row => row.alternateName -> row.standardName).toMap
      val capturedExpanded =
        table.apiExpandedRows.map(row => row.alternateName -> row.standardName).toMap
      withClue(s"manifest alternateNames.$family: ") {
        capturedIni.size shouldBe table.iniRows.size
        capturedExpanded.size shouldBe table.apiExpandedRows.size
        // An alternate spelling naming no member would be a lookup that silently fails.
        capturedExpanded.values.filterNot(canonicalNamesOf(family).contains).toVector shouldBe empty
        ()
      }
      sameEntries(
        s"the expansion of the captured alternateNames.$family INI rows",
        capturedExpanded,
        expandAlternateNames(capturedIni))
      sameEntries(s"alternate spellings of $family", alternateNamesOf(family), capturedExpanded)
      ()
    }
    withClue("only the three families the capture recorded declare alternate spellings: ") {
      // An alternate table on one of the other families would be an addition to the reference data.
      FamiliesWithoutAlternateNames.filter {
        case (_, alternates) => alternates.nonEmpty
      } shouldBe empty
    }
  }

  //-------------------------------------------------------------------------
  // Floating rate names.
  //-------------------------------------------------------------------------

  test("the manifest floating rate name sections account for every transcribed row") {
    // Shape: {constants{count, names[]}, aliasRowCount, sections{<section>{count, rows[{key,
    // value}]}}}. The seven sections and their sizes are the division this module keeps, so the
    // section names are asserted first and the declared alias row count reconciled with them.
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

  test("FloatingRateNames' constants equal the manifest floatingRateNames constants") {
    // Shape: {constants{count, names[]}}, the captured constant names in name order. These
    // forty-one are a selection from the 351 rows of the four name sections - a published name is
    // any alias a counterparty may write, a constant one of the names a caller writes in code - so
    // each is also required to be the member the family publishes under that name.
    sameConstantNames(
      "floating rate name",
      s"$FloatingRateNamesKey.constants",
      FloatingRateNameConstants.map(_.name),
      floatingRateNames.constants)
    withClue("every floating rate name constant is the published member of that name: ") {
      FloatingRateNameConstants.filterNot(rate =>
        FloatingRateName.valueOf(rate.name).contains(rate)) shouldBe empty
      FloatingRateNameConstants.toSet.subsetOf(FloatingRateName.values.toList.toSet) shouldBe true
      // Unlike the overnight index holder, no two of these are one value under two spellings.
      FloatingRateNameConstants.map(_.name).distinct.size shouldBe FloatingRateNameConstants.size
    }
  }

  test("FloatingRateNameData's Ibor rows equal the manifest ibor section, in published order") {
    // The published value of an Ibor row is the '''stem''' of an index name, to which a `-` is
    // appended because an Ibor index name is completed by a tenor: `GBP-LIBOR-` plus `3M`. The
    // manifest value is completed the same way, which also pins the appending: a row that forgot it
    // fails here.
    val expected = floatingRateSection(IborSection).map(row => (row.key, s"${row.value}-"))
    val actual = FloatingRateNameData.iborRows.map(row => (row.externalName, row.indexName))
    sameRows("floating rate name (ibor)", actual, expected)
    withClue("Ibor rows all carry the Ibor rate type: ") {
      FloatingRateNameData.iborRows.map(_.rateType).distinct shouldBe Vector(FloatingRateType.Ibor)
    }
  }

  test("FloatingRateNameData.iborFixingDateOffsets equals the manifest iborFixingDateOffset section") {
    // Three rows, consumed twice: as the table itself, and as the `fixingDateOffsetDays` of the
    // three Ibor rows they name. Both are compared, because a table that agreed with the manifest
    // while the rows it feeds did not would leave the offsets missing from every row consumer.
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
    // The published value of a row of these three sections is a complete index name, transcribed
    // unchanged, so no completion step applies. Each section also fixes the rate type of the rows
    // it produces, which decides the kind of index a name converts to.
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
      // The four name sections in order are the published name space, and `rows` is that
      // concatenation.
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
    // names - `CNY` defaults to `CNY-REPO` - which is why they resolve through `byExternalName`.
    // That every value names a row is asserted too: a default resolving to nothing would be a
    // lookup failure no count could reveal.
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
  // The six convention families.
  //-------------------------------------------------------------------------

  test("DayCount's members equal the manifest dayCounts constants") {
    // Shape: {count, names[]}, the captured constant names in name order. Twenty-one standard
    // members, and the family has exactly twenty-one: `Bus/252` is not among them because it
    // carries a calendar and so is a member per calendar rather than a constant.
    val expected = nameGroup(DayCountsKey)
    withClue(s"manifest key '$DayCountsKey': ") {
      expected.count shouldBe expected.names.size
      DayCount.values.toList.map(_.name).sorted shouldBe expected.names.toList
      DayCountConstants.map(_.name).sorted shouldBe expected.names
      DayCountConstants.toSet shouldBe DayCount.values.toList.toSet
    }
  }

  test("DayCount's external name groups equal the manifest externalNames rows") {
    // The FpML group is the one place in the document where an external row names a member the
    // family does not publish: `BUS/252` maps to `Bus/252 BRBD`, a day count that carries a
    // calendar. The raw, unresolved table is therefore what is compared, as this module holds it.
    sameExternalNameGroups(DayCountFamily, DayCount.namedEnum.externalNameGroups, group =>
      DayCount.namedEnum.externalNamesRaw(group))
  }

  test("DayCount's lenient patterns equal the manifest rows, in file order") {
    // Sixty-seven rows, the longest lenient table of the reference data, and the order is
    // behaviour: the rewrite is sequential, so `ACTUAL/ACTUAL` is rewritten by the first row and
    // the result offered to the second.
    sameLenientPatterns(DayCountFamily, DayCount.namedEnum.lenientPatterns)
  }

  test("BusinessDayConvention's members equal the manifest businessDayConventions constants") {
    // Shape: {count, names[]}, the captured constant names in name order. The members are compared
    // in that order and the constants holder with the members, so a constant naming the wrong
    // member, or a member with no constant, is reported.
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
    // a comparison with the captured rows is the only thing that pins them.
    val groups = externalNames(BusinessDayConventionFamily)
    val documented = DocumentedExternalGroups(BusinessDayConventionFamily)
    withClue(s"manifest externalNames.$BusinessDayConventionFamily: ") {
      groups.keySet shouldBe documented.keySet
      BusinessDayConvention.namedEnum.externalNameGroups shouldBe groups.keySet
    }
    documented.foreach {
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
    // the next is tried - so the manifest preserves the file order of the rows, this module holds
    // an ordered list, and the two are compared as sequences.
    val expected = lenientPatternRows(BusinessDayConventionFamily).map(row => (row.key, row.value))
    val actual = BusinessDayConvention.namedEnum.lenientPatterns.toVector.map {
      case (pattern, replacement) => (pattern.regex, replacement)
    }
    sameRows(s"lenient pattern ($BusinessDayConventionFamily)", actual, expected)
    withClue(s"manifest lenientPatterns.$BusinessDayConventionFamily: ") {
      lenientPatterns(BusinessDayConventionFamily).count shouldBe expected.size
    }
  }

  test("RollConvention's members equal the manifest rollConventions constants") {
    // Forty-five names: the eight standard conventions plus thirty day-of-month and seven
    // day-of-week members. All forty-five have a constant, so the constants and the family coincide
    // and both directions are asserted.
    val expected = nameGroup(RollConventionsKey)
    withClue(s"manifest key '$RollConventionsKey': ") {
      expected.count shouldBe expected.names.size
      RollConvention.values.toList.map(_.name).sorted shouldBe expected.names.toList
      RollConventionConstants.map(_.name).sorted shouldBe expected.names
      RollConventionConstants.toSet shouldBe RollConvention.values.toList.toSet
      // `Day31` is absent: the thirty-first is the end of the month, so that text maps onto `EOM`.
      expected.names should not contain "Day31"
    }
  }

  test("RollConvention's external name groups equal the manifest externalNames rows") {
    // Forty-four rows in one group, the largest external table of the reference data: the numbers
    // one to thirty-one, the seven day-of-week abbreviations and the six standard conventions FpML
    // names. The row `31 -> EOM` is asserted here as data rather than as behaviour.
    sameExternalNameGroups(
      RollConventionFamily,
      RollConvention.namedEnum.externalNameGroups,
      group => RollConvention.namedEnum.externalNamesRaw(group))
  }

  test("RollConvention's lenient patterns equal the manifest rows, in file order") {
    // Eleven rows whose order is load-bearing in a way no other lenient table's is: `(Day_?)?31`
    // has to be tried before `(Day_?)?([1-2]?[0-9])`, and `(Day_?)?30` before it too, or `31` would
    // be rewritten to `Day3`. Comparing the rows as a sequence is what pins that.
    sameLenientPatterns(RollConventionFamily, RollConvention.namedEnum.lenientPatterns)
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
      // The capture recorded no external names for this family, so the absence is asserted.
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
      // The capture recorded no alternate, lenient or external table for this family, so this
      // module declares none either.
      DateSequence.namedEnum.lenientPatterns shouldBe empty
      DateSequence.namedEnum.alternateNames shouldBe empty
      DateSequence.namedEnum.externalNameGroups shouldBe empty
    }
  }

  test("StubConvention's members equal the manifest stubConventions, in declaration order") {
    // The one named-constant group the capture did '''not''' sort: the captured type is an
    // enumeration, so its values were emitted in declaration order - initial stubs, then final
    // stubs, then both - which is the order this module declares its members in. The comparison is
    // therefore of sequences, asserting the order as well as the membership.
    val expected = nameGroup(StubConventionsKey)
    withClue(s"manifest key '$StubConventionsKey': ") {
      expected.count shouldBe expected.names.size
      StubConvention.values.toList.map(_.name) shouldBe expected.names.toList
      StubConventionConstants.map(_.name) shouldBe expected.names
      StubConventionConstants.toSet shouldBe StubConvention.values.toList.toSet
      // No external, lenient or alternate table is captured for an enumeration: its accepted
      // spellings are the constant identifier and its case folds, a property of the type.
      externalNames.keySet should not contain StubConventionFamily
      lenientPatterns.keySet should not contain StubConventionFamily
      alternateNames.keySet should not contain StubConventionFamily
    }
  }

  //-------------------------------------------------------------------------
  // Holiday calendars.
  //-------------------------------------------------------------------------

  test("HolidayCalendarIds' constants equal the manifest holidayCalendarIds") {
    // Shape: {count, names[]}, the captured constant names in name order. This module's constants
    // are named one by one in the companion, so one that disappeared is a compile error here and
    // one that was added is a failure of this comparison.
    val expected = nameGroup(HolidayCalendarIdsKey)
    withClue(s"manifest key '$HolidayCalendarIdsKey': ") {
      expected.count shouldBe expected.names.size
      HolidayCalendarIdConstants.map(_.name).sorted shouldBe expected.names
      HolidayCalendarIdConstants.map(_.name).distinct.size shouldBe HolidayCalendarIdConstants.size
    }
  }

  test("HolidayCalendarId.defaultByCurrency equals the manifest defaults, including resolvability") {
    // Shape: {count, resolvableCount, unresolvableCount, rows[{currency, calendarId,
    // resolvableAgainstStandardReferenceData}]}. Thirteen of the thirty-one captured rows name
    // calendars no built-in calendar answers, so the capture recorded the mapping '''and''' whether
    // it resolves, and both are asserted: a module that invented the missing thirteen would
    // otherwise pass. The lookup is compared over the whole currency family, so an added default is
    // reported.
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
    // Shape: {count, names[]}: the four weekend and no-holiday calendars plus twenty-six dated
    // ones. Each entry of this module's map is keyed by the identifier its own calendar carries, so
    // the key names are what the set is compared through.
    //
    // A built-in calendar is what `ReferenceData.standard` answers with, so every captured name has
    // to resolve there as well as be present in the map.
    val expected = nameGroup(BuiltInHolidayCalendarsKey)
    withClue(s"manifest key '$BuiltInHolidayCalendarsKey': ") {
      expected.count shouldBe expected.names.size
      StandardHolidayCalendars.all.keys.toVector.map(_.name).sorted shouldBe expected.names
      expected.names.filterNot(name =>
        HolidayCalendarId.of(name).resolve(ReferenceData.standard).isRight) shouldBe empty
    }
  }

  test("HolidayCalendarData.thba equals the manifest THBA year rows and weekend") {
    // Shape: {THBA{yearRowCount, weekend, rows[{year, dates}]}}. The dates of a row are captured as
    // text - `Jan03,Feb23,…` - and are parsed into month-days here rather than this module's
    // month-days being formatted into text, so the comparison cannot depend on the locale or the
    // calendar data of the host. The weekend is captured apart from the year rows, so the
    // seventy-five rows are seventy-five years.
    val expected = thbaRows
    val actual = HolidayCalendarData.thba.toVector
    sameRows("THBA year", actual, expected)
    withClue(s"manifest key '$HolidayCalendarDataKey': ") {
      holidayCalendarDataThba.yearRowCount shouldBe expected.size
      HolidayCalendarData.thbaWeekendDays shouldBe thbaWeekend
      // The rows are not filtered against the weekend: one resolved date per captured month-day.
      HolidayCalendarData.thbaHolidays.size shouldBe expected.map(_._2.size).sum
    }
  }

  //-------------------------------------------------------------------------
  // The THBA table's own invariants.
  //
  // The rows themselves are established by the manifest comparison above and are not restated. What
  // follows is what a row-for-row comparison cannot state and a consumer of the table relies on:
  // the shape of every row, the resolution of a month-day against the year it is filed under, the
  // weekend, and that this table is the whole content of the calendar assembled from it.
  //-------------------------------------------------------------------------

  test("every THBA year row is non-empty, strictly ascending and free of duplicates") {
    HolidayCalendarData.thba.foreach {
      case (year, monthDays) =>
        withClue(s"$year: ") {
          monthDays should not be empty
          monthDays.distinct.size shouldBe monthDays.size
          // Ascending is the captured order, and why the resolved dates need no sorting.
          monthDays.sliding(2).foreach {
            case List(earlier, later) => earlier.isBefore(later) shouldBe true
            case _ => succeed
          }
        }
    }

    // The shortest published year holds 13 dates and the longest 19, so a row that lost or gained a
    // date en bloc is reported here even if the total happened to be preserved.
    val rowSizes = HolidayCalendarData.thba.values.map(monthDays => monthDays.size).toList
    rowSizes.min shouldBe DocumentedThbaShortestYearRowSize
    rowSizes.max shouldBe DocumentedThbaLongestYearRowSize
    rowSizes.sum shouldBe DocumentedThbaDateCount
  }

  test("HolidayCalendarData.thbaHolidays is the THBA table resolved into ascending dates") {
    HolidayCalendarData.thbaHolidays.size shouldBe DocumentedThbaDateCount
    HolidayCalendarData.thbaHolidays.distinct.size shouldBe DocumentedThbaDateCount

    // Strictly ascending overall, asserted directly because it is the property a consumer relies on
    // when it builds a calendar from the list without sorting it.
    HolidayCalendarData.thbaHolidays.sliding(2).foreach {
      case List(earlier, later) =>
        withClue(s"$earlier then $later: ")(earlier.isBefore(later) shouldBe true)
      case _ => succeed
    }

    // Consistent with the table it derives from: the same dates, each the month-day of its row
    // resolved against that row's year, in order, and the years compared with the captured years.
    val resolved: List[LocalDate] =
      HolidayCalendarData.thba.toList.flatMap {
        case (year, monthDays) => monthDays.map(monthDay => monthDay.atYear(year))
      }
    HolidayCalendarData.thbaHolidays shouldBe resolved
    HolidayCalendarData.thbaHolidays.map(date => date.getYear).distinct shouldBe
      thbaRows.map(row => row._1).toList

    // The accessor is a method over a value computed once, so asking twice gives the same list.
    HolidayCalendarData.thbaHolidays shouldBe HolidayCalendarData.thbaHolidays
  }

  test("every THBA month-day is valid for the year it is filed under") {
    // Resolving a month-day against its year '''adjusts''' the 29th of February in a non-leap year
    // rather than failing. One published row holds a leap day - 2056 - so this is what
    // distinguishes correct data from a leap day filed under the wrong year, which would become the
    // 28th.
    HolidayCalendarData.thba.foreach {
      case (year, monthDays) =>
        monthDays.foreach { monthDay =>
          val resolved = monthDay.atYear(year)
          withClue(s"$year-$monthDay resolved to $resolved: ") {
            resolved.getYear shouldBe year
            resolved.getMonthValue shouldBe monthDay.getMonthValue
            resolved.getDayOfMonth shouldBe monthDay.getDayOfMonth
          }
        }
    }

    // The one leap day the section publishes, asserted concretely.
    HolidayCalendarData.thba(2056) should contain(MonthDay.of(2, 29))
    HolidayCalendarData.thbaHolidays should contain(LocalDate.of(2056, 2, 29))
  }

  test("the published THBA holiday that falls at a weekend is kept, with the Sat,Sun weekend") {
    // The published rows are not filtered against the weekend, unlike the rule-generated calendars,
    // and exactly one row exercises that: the 4th of May 2031 is a Sunday and stays in the table.
    // Scanned exhaustively, so filtering the table - or adding a second such date - fails here.
    val weekendHolidays: List[LocalDate] =
      HolidayCalendarData.thbaHolidays.filter(date =>
        HolidayCalendarData.thbaWeekendDays.contains(date.getDayOfWeek))

    weekendHolidays shouldBe List(LocalDate.of(2031, 5, 4))
    LocalDate.of(2031, 5, 4).getDayOfWeek shouldBe DayOfWeek.SUNDAY

    // The two days are named here as well as compared above, because outside the published years
    // the calendar applies its weekend alone and nothing else here states which days those are.
    HolidayCalendarData.thbaWeekendDays shouldBe Set(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY)
  }

  test("HolidayCalendarData.thba is the data the built-in THBA calendar is assembled from") {
    // This table is the whole content of the built-in Thai bank calendar, so every published date
    // is a holiday of it and the weekend it declares is the one the calendar applies.
    // `StandardHolidayCalendars` is the assembler; the data object never builds a calendar itself,
    // which keeps the two free of an initialisation cycle.
    val calendar = StandardHolidayCalendars.THBA
    calendar.id shouldBe HolidayCalendarIds.THBA
    calendar.weekendDays shouldBe HolidayCalendarData.thbaWeekendDays

    HolidayCalendarData.thbaHolidays.foreach { date =>
      withClue(s"$date: ")(calendar.isHoliday(date) shouldBe true)
    }

    // Outside the published range the weekend rule alone answers, which is the documented fallback.
    calendar.isHoliday(LocalDate.of(2004, 1, 1)) shouldBe false
    calendar.isBusinessDay(LocalDate.of(2004, 1, 1)) shouldBe true
    calendar.isHoliday(LocalDate.of(2080, 12, 31)) shouldBe false
    calendar.isHoliday(LocalDate.of(2080, 12, 28)) shouldBe true
  }

  //-------------------------------------------------------------------------
  // Element-wise comparison with a failure that names the offending row.
  //-------------------------------------------------------------------------

  /**
   * Asserts two row sequences are equal, reporting the '''first''' difference with its index and
   * both values.
   *
   * A single comparison of two vectors of 251 country rows reports both in full, which at that size
   * is the difference between a diagnosable failure and an unreadable one. Sizes are compared
   * first, so a truncated or extended table is reported as such rather than as a difference in
   * whichever row falls off the end.
   *
   * @param what  how a row of this table is named in the failure message
   * @param actual  the rows this module holds
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

  /**
   * Asserts two tables of rendered cells are equal, reporting the '''first''' difference with the
   * row it sits in, the name that row carries and the column it belongs to.
   *
   * [[sameRows]] one dimension further in, for the two tables wide enough that a row-level failure
   * would be unreadable. Every cell is a rendered string, both sides normalised the same way by the
   * callers, and the first cell of a row is its name in the message - both index tables begin with
   * `Name`. A row's column count is checked against the header list first, so a short row is
   * reported as such rather than as a missing cell.
   *
   * @param what  how a row of this table is named in the failure message
   * @param headers  the column names, in the order the cells are rendered in
   * @param actual  the rendered rows this module holds
   * @param expected  the rendered rows captured from the Java implementation
   */
  private def sameTable(
      what: String,
      headers: Vector[String],
      actual: Vector[Vector[String]],
      expected: Vector[Vector[String]]): Unit = {
    withClue(s"$what rows: the port has ${actual.size}, the manifest ${expected.size}: ") {
      actual.size shouldBe expected.size
    }
    actual.indices.foreach { row =>
      val found = actual(row)
      val captured = expected(row)
      val rowName = found.headOption.getOrElse(captured.headOption.getOrElse(""))
      withClue(s"$what row $row '$rowName': ") {
        found.size shouldBe headers.size
        captured.size shouldBe headers.size
      }
      headers.indices.foreach { column =>
        withClue(
          s"$what row $row '$rowName' column '${headers(column)}': the port has " +
            s"'${found(column)}', the manifest '${captured(column)}': ") {
          found(column) shouldBe captured(column)
        }
        ()
      }
      ()
    }
    ()
  }

  /**
   * Asserts two keyed tables are equal, reporting the '''symmetric difference''' rather than the
   * two tables.
   *
   * A map has no order, so a plain comparison prints both maps in full - over the forty-four
   * external spellings of the largest group, a failure nobody reads. The two differences are named
   * instead: the entries the manifest has that this module does not hold with that value, and the
   * entries this module declares that the manifest does not. An entry with different values on the
   * two sides appears in both lists.
   *
   * @param what  how this table is named in the failure message
   * @param actual  the table this module holds
   * @param expected  the table captured from the Java implementation
   */
  private def sameEntries(
      what: String,
      actual: Map[String, String],
      expected: Map[String, String]): Unit = {
    def render(entries: Vector[(String, String)]): String =
      if (entries.isEmpty) "nothing"
      else entries.map { case (key, value) => s"'$key' -> '$value'" }.mkString(", ")
    val missing = expected.toVector.sorted.filterNot {
      case (key, value) => actual.get(key).contains(value)
    }
    val unexpected = actual.toVector.sorted.filterNot {
      case (key, value) => expected.get(key).contains(value)
    }
    withClue(
      s"$what: the manifest has ${render(missing)} where the port does not, and the port " +
        s"declares ${render(unexpected)} where the manifest does not: ") {
      missing shouldBe empty
      unexpected shouldBe empty
      ()
    }
  }

  /**
   * Asserts the named constants of this module equal a captured constant group, in name order.
   *
   * The captured group's own count is reconciled with its name list first, so a document
   * disagreeing with itself is reported as that rather than as a difference against this module.
   * The names are then compared as a '''sequence''' of sorted names, not as a set, because a
   * repeated name is data: the overnight index holder publishes two constants for one index.
   *
   * @param what  how a constant of this family is named in the failure message
   * @param where  how the captured group is named in the failure message
   * @param actual  the names of the constants this module declares
   * @param captured  the captured constant group
   */
  private def sameConstantNames(
      what: String,
      where: String,
      actual: Vector[String],
      captured: NameGroup): Unit = {
    withClue(s"manifest $where: ") {
      captured.count shouldBe captured.names.size
      captured.names.count(_.trim.isEmpty) shouldBe 0
    }
    sameRows(s"$what constant name", actual.sorted, captured.names)
  }

  /**
   * Asserts the named constants of this module equal the constant group under a top-level key.
   *
   * @param what  how a constant of this family is named in the failure message
   * @param key  the top-level key of the document carrying the group
   * @param actual  the names of the constants this module declares
   */
  private def sameNamedConstants(what: String, key: String, actual: Vector[String]): Unit =
    sameConstantNames(what, s"key '$key'", actual, nameGroup(key))

  /**
   * Asserts the external spelling groups of one family equal the captured rows.
   *
   * The comparison is against the '''raw''' table - external spelling to canonical name as text -
   * rather than the resolved view, for two reasons: a row may name a member the family does not
   * publish, which the resolved view drops and the transcription must keep, and resolving both
   * sides through the same lookup would let a mistranscribed canonical name agree with itself.
   *
   * @param family  the family name the document keys the groups under
   * @param groups  the group names this module publishes for the family
   * @param rawOf  this module's raw table for a group, by group name
   */
  private def sameExternalNameGroups(
      family: String,
      groups: Set[String],
      rawOf: String => Option[Map[String, String]]): Unit = {
    val documented = DocumentedExternalGroups(family)
    val captured = externalNames(family)
    withClue(s"manifest externalNames.$family: ") {
      captured.keySet shouldBe documented.keySet
      groups shouldBe captured.keySet
    }
    documented.foreach {
      case (group, documentedCount) =>
        withClue(s"manifest externalNames.$family.$group: ") {
          captured(group).count shouldBe documentedCount
          captured(group).rows.size shouldBe documentedCount
          rawOf(group) should not be empty
          ()
        }
        sameEntries(
          s"external spellings of $family in group '$group'",
          rawOf(group).getOrElse(Map.empty),
          captured(group).rows.map(row => row.externalName -> row.standardName).toMap)
        ()
    }
    ()
  }

  /**
   * Asserts the lenient rewrites of one family equal the captured rows, in file order.
   *
   * The rewrite is sequential - each matching expression rewrites the text before the next is
   * tried - so the order of the rows is behaviour and the comparison is of sequences. Each
   * expression is compared through its source text, which is what the reference data held; the
   * compiled expressions are copies made insensitive to case, a derivation rather than the data.
   *
   * @param family  the family name the document keys the table under
   * @param patterns  the lenient rewrites this module declares, in the order they are applied
   */
  private def sameLenientPatterns(family: String, patterns: List[(Regex, String)]): Unit = {
    val expected = lenientPatternRows(family).map(row => (row.key, row.value))
    val actual = patterns.toVector.map {
      case (pattern, replacement) => (pattern.regex, replacement)
    }
    sameRows(s"lenient pattern ($family)", actual, expected)
    withClue(s"manifest lenientPatterns.$family: ") {
      lenientPatterns(family).count shouldBe expected.size
      DocumentedLenientCounts(family) shouldBe expected.size
      ()
    }
  }
}

/**
 * The manifest document, read and decoded once, and the declaration of what this suite covers.
 *
 * '''Nothing here runs while this object initialises.''' The resource is read on first use, behind
 * a `lazy val`, and the read answers with an `Either` rather than throwing, so a resource that is
 * missing, empty, unparseable or not an object is reported by the test whose subject that is rather
 * than by an `ExceptionInInitializerError`. Each key is then decoded into the typed view the tests
 * read, and those views are lazy for the same reason: a document whose shape has changed under one
 * key fails the tests of that key and leaves the remaining twenty-eight still asserted.
 */
private object ReferenceDataManifestSpec {

  //-------------------------------------------------------------------------
  // Keys, families and the counts section 7 of the README documents, named once each.
  //-------------------------------------------------------------------------

  /** The captured document, named as the class loader sees it. */
  val ManifestResource: String = "manifest/reference-data-manifest.json"

  val IborIndicesKey: String = "iborIndices"
  val OvernightIndicesKey: String = "overnightIndices"
  val IborIndexConstantsKey: String = "iborIndexConstants"
  val OvernightIndexConstantsKey: String = "overnightIndexConstants"
  val PriceIndexConstantsKey: String = "priceIndexConstants"
  val FxIndexConstantsKey: String = "fxIndexConstants"
  val FloatingRateNamesKey: String = "floatingRateNames"
  val DayCountsKey: String = "dayCounts"
  val BusinessDayConventionsKey: String = "businessDayConventions"
  val RollConventionsKey: String = "rollConventions"
  val PeriodAdditionConventionsKey: String = "periodAdditionConventions"
  val DateSequencesKey: String = "dateSequences"
  val StubConventionsKey: String = "stubConventions"
  val HolidayCalendarIdsKey: String = "holidayCalendarIds"
  val HolidayCalendarDefaultKey: String = "holidayCalendarDefaultByCurrency"
  val BuiltInHolidayCalendarsKey: String = "builtInHolidayCalendars"
  val HolidayCalendarDataKey: String = "holidayCalendarData"

  /** The one calendar whose dates were published as data rather than derived from rules. */
  val ThbaCalendarName: String = "THBA"

  val DayCountFamily: String = "DayCount"
  val RollConventionFamily: String = "RollConvention"
  val BusinessDayConventionFamily: String = "BusinessDayConvention"
  val PeriodAdditionConventionFamily: String = "PeriodAdditionConvention"
  val StubConventionFamily: String = "StubConvention"
  val IborIndexFamily: String = "IborIndex"
  val OvernightIndexFamily: String = "OvernightIndex"
  val FxIndexFamily: String = "FxIndex"

  val IborSection: String = "ibor"
  val IborFixingDateOffsetSection: String = "iborFixingDateOffset"
  val OvernightCompoundedSection: String = "overnightCompounded"
  val OvernightAveragedSection: String = "overnightAveraged"
  val PriceSection: String = "price"
  val CurrencyDefaultIborSection: String = "currencyDefaultIbor"
  val CurrencyDefaultOvernightSection: String = "currencyDefaultOvernight"

  /** The columns of the Java Ibor index data, in the order the capture emits them. */
  val IborIndexHeaders: Vector[String] =
    Vector(
      "Name",
      "Currency",
      "Active",
      "Day Count",
      "Fixing Calendar",
      "Offset Days",
      "Offset Calendar",
      "Effective Date Calendar",
      "Tenor",
      "Tenor Convention",
      "FixingTime",
      "FixingZone",
      "Fixed Leg Day Count")

  val IborIndexNameHeader: String = "Name"

  /** The columns of the Java overnight index data, in the order the capture emits them. */
  val OvernightIndexHeaders: Vector[String] =
    Vector(
      "Name",
      "Currency",
      "Active",
      "Day Count",
      "Fixing Calendar",
      "Publication Offset Days",
      "Effective Offset Days",
      "Fixed Leg Day Count")

  val OvernightIndexNameHeader: String = "Name"

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

  /** The documented number of `FloatingRateName` constants. */
  val DocumentedFloatingRateConstantCount: Int = 41

  /** The documented number of currencies, and the split the capture cross-checked. */
  val DocumentedCurrencyCount: Int = 74
  val DocumentedHistoricCurrencyCount: Int = 19
  val DocumentedActiveCurrencyCount: Int = 55

  /** The documented length of the market convention ordering. */
  val DocumentedMarketConventionPriorityCount: Int = 9

  /** The documented number of conventional currency pairs. */
  val DocumentedCurrencyPairCount: Int = 92

  /** The documented number of country code rows. */
  val DocumentedCountryCount: Int = 251

  /** The documented default calendar rows, and the split by whether they resolve. */
  val DocumentedDefaultCalendarCount: Int = 31
  val DocumentedResolvableDefaultCalendarCount: Int = 18
  val DocumentedUnresolvableDefaultCalendarCount: Int = 13

  /** The documented number of `THBA` year rows, one per year from 2005 to 2079. */
  val DocumentedThbaYearRowCount: Int = 75

  /**
   * The documented `THBA` date figures: the total number of published dates, and the sizes of the
   * shortest and the longest year row.
   *
   * These are the published totals of the section the rows were captured from
   * (`HolidayCalendarData.ini:32-106`), so they report a row that lost or gained a date en bloc on
   * both sides of the manifest comparison at once.
   */
  val DocumentedThbaDateCount: Int = 1220
  val DocumentedThbaShortestYearRowSize: Int = 13
  val DocumentedThbaLongestYearRowSize: Int = 19

  /**
   * The documented size of every named-constant group of the document.
   *
   * Twelve of the twenty-nine keys are `{count, names[]}` groups, and every one of them is here:
   * the four index constant holders, the six convention families, the holiday calendar identifiers
   * and the built-in calendar set. The figures are the literals section 7 of
   * `tools/parity-capture/README.md` publishes.
   */
  val DocumentedNameGroupCounts: Map[String, Int] =
    Map(
      IborIndexConstantsKey -> 113,
      OvernightIndexConstantsKey -> 21,
      PriceIndexConstantsKey -> 9,
      FxIndexConstantsKey -> 8,
      DayCountsKey -> 21,
      BusinessDayConventionsKey -> 7,
      RollConventionsKey -> 45,
      PeriodAdditionConventionsKey -> 3,
      DateSequencesKey -> 6,
      StubConventionsKey -> 8,
      HolidayCalendarIdsKey -> 29,
      BuiltInHolidayCalendarsKey -> 30)

  /** The documented size of every index table of the document. */
  val DocumentedIndexTableCounts: Map[String, Int] =
    Map(
      IborIndicesKey -> 271,
      OvernightIndicesKey -> 35,
      "priceIndices" -> 9,
      "fxIndices" -> 16)

  /**
   * The external name groups of every family that publishes any, and their documented sizes.
   *
   * Three families do: the day count, roll convention and business day convention families. The
   * other named families publish none, which the tests of those families assert directly rather
   * than through this table.
   */
  val DocumentedExternalGroups: Map[String, Map[String, Int]] =
    Map(
      DayCountFamily -> Map("FpML" -> 14, "SWIFT" -> 8),
      RollConventionFamily -> Map("FpML" -> 44),
      BusinessDayConventionFamily -> Map("FpML" -> 5, "SWIFT" -> 3))

  /** The lenient pattern table of every family that declares one, and its documented size. */
  val DocumentedLenientCounts: Map[String, Int] =
    Map(
      DayCountFamily -> 67,
      RollConventionFamily -> 11,
      BusinessDayConventionFamily -> 11,
      PeriodAdditionConventionFamily -> 3)

  /** The alternate name families, each with its documented INI and API-expanded row counts. */
  val DocumentedAlternateNameCounts: Map[String, (Int, Int)] =
    Map(
      IborIndexFamily -> ((1, 1)),
      OvernightIndexFamily -> ((10, 13)),
      FxIndexFamily -> ((1, 1)))

  /**
   * The top-level keys in the order section 7 of the README documents, which is the order the
   * capture writes them.
   *
   * Held as a sequence because the order is part of what makes the document byte-stable between
   * captures.
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
  // What this suite covers. Nothing is exempt, and the declaration is itself asserted.
  //-------------------------------------------------------------------------

  /**
   * The keys asserted against this module by the tests of this suite, which is all of them.
   *
   * Four of them - `floatingRateNames`, `externalNames`, `lenientPatterns` and `alternateNames` -
   * divide their content by family rather than carrying one table, so coverage is stated one level
   * deeper for them in [[PartiallyCoveredKeys]] as well.
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
      IborIndicesKey,
      OvernightIndicesKey,
      "priceIndices",
      "fxIndices",
      IborIndexConstantsKey,
      OvernightIndexConstantsKey,
      PriceIndexConstantsKey,
      FxIndexConstantsKey,
      FloatingRateNamesKey,
      DayCountsKey,
      BusinessDayConventionsKey,
      RollConventionsKey,
      PeriodAdditionConventionsKey,
      DateSequencesKey,
      StubConventionsKey,
      HolidayCalendarIdsKey,
      HolidayCalendarDefaultKey,
      BuiltInHolidayCalendarsKey,
      HolidayCalendarDataKey,
      "externalNames",
      "lenientPatterns",
      "alternateNames")

  /**
   * The keys whose Scala counterpart does not exist yet, each naming the file that will carry it.
   *
   * Empty, because every table of the manifest has one. The map is how the '''next''' gap is
   * reported: a key this suite has no comparison for is declared here and appears in the failure
   * message of the coverage test until it is covered, with the path of the file that will carry the
   * counterpart, so a reader learns where the work is and not merely that work remains.
   */
  val PendingKeys: Map[String, String] = Map.empty

  /**
   * The nested shapes of a covered key that have no Scala counterpart yet, keyed `<key>.<shape>`.
   *
   * Empty, with the same purpose as [[PendingKeys]] one level down: a shape of a family-divided key
   * has its own owner in this module, so it is declared and reported on its own.
   */
  val PendingNestedShapes: Map[String, String] = Map.empty

  /**
   * The covered nested shapes of each key whose content is divided by family.
   *
   * A shape of one of these four is a separate table with a separate owner - `lenientPatterns`
   * `.DayCount` is transcribed in `date/DayCount.scala` and `lenientPatterns.RollConvention` in
   * `schedule/RollConvention.scala` - so "the key is covered" is not a statement about any one of
   * them. The nested coverage test compares these sets with the shapes the document carries.
   */
  val PartiallyCoveredKeys: Map[String, Set[String]] =
    Map(
      FloatingRateNamesKey -> Set("constants", "aliasRowCount", "sections"),
      "externalNames" -> Set(DayCountFamily, RollConventionFamily, BusinessDayConventionFamily),
      "lenientPatterns" -> Set(
        DayCountFamily,
        RollConventionFamily,
        BusinessDayConventionFamily,
        PeriodAdditionConventionFamily),
      "alternateNames" -> Set(IborIndexFamily, OvernightIndexFamily, FxIndexFamily))

  //-------------------------------------------------------------------------
  // Row models, each with the key set of the object it decodes. Every field name is the JSON key
  // the capture writes, and every key set is `uniform` - one shape, no optional key - because the
  // captured manifest is uniform: a table carries the same keys in every one of its rows, present
  // rather than omitted. `ParityHarness.strictObject` applies the set to the object before the
  // derived decoder reads it, so an unknown, a missing or a renamed key is refused with the key
  // named, and `every declared object schema is the key set the committed manifest carries` holds
  // each set against the document.
  //-------------------------------------------------------------------------

  /** One currency row: `{code, minorUnitDigits, triangulationCurrency, historic}`. */
  final case class CurrencyManifestRow(
      code: String,
      minorUnitDigits: Int,
      triangulationCurrency: String,
      historic: Boolean)

  val CurrencyRowSchema: KeySchema =
    KeySchema.uniform(
      "manifest currency row",
      Set("code", "minorUnitDigits", "triangulationCurrency", "historic"))

  /** The currency table: `{count, historicCount, activeCount, rows[]}`. */
  final case class CurrencyManifestTable(
      count: Int,
      historicCount: Int,
      activeCount: Int,
      rows: Vector[CurrencyManifestRow])

  val CurrencyTableSchema: KeySchema =
    KeySchema.uniform(
      "manifest currency table",
      Set("count", "historicCount", "activeCount", "rows"))

  /** One conventional currency pair: `{pair, rateDigits}`. */
  final case class CurrencyPairManifestRow(pair: String, rateDigits: Int)

  val CurrencyPairRowSchema: KeySchema =
    KeySchema.uniform("manifest currency pair row", Set("pair", "rateDigits"))

  /** The currency pair table: `{count, rows[]}`. */
  final case class CurrencyPairManifestTable(count: Int, rows: Vector[CurrencyPairManifestRow])

  val CurrencyPairTableSchema: KeySchema =
    KeySchema.uniform("manifest currency pair table", Set("count", "rows"))

  /** One country row: `{alpha3, alpha2}`. */
  final case class CountryManifestRow(alpha3: String, alpha2: String)

  val CountryRowSchema: KeySchema =
    KeySchema.uniform("manifest country row", Set("alpha3", "alpha2"))

  /** The country table: `{count, rows[]}`. */
  final case class CountryManifestTable(count: Int, rows: Vector[CountryManifestRow])

  val CountryTableSchema: KeySchema =
    KeySchema.uniform("manifest country table", Set("count", "rows"))

  /** A named-constant group: `{count, names[]}`, the names in name order. */
  final case class NameGroup(count: Int, names: Vector[String])

  /**
   * The key set of a named-constant group: the shape of the twelve constant-holder keys of the
   * document, which are the keys of [[DocumentedNameGroupCounts]], and of
   * `floatingRateNames.constants`.
   */
  val NameGroupSchema: KeySchema =
    KeySchema.uniform("manifest named-constant group", Set("count", "names"))

  /** One key/value row of a configuration section: `{key, value}`, in file order. */
  final case class KeyValueRow(key: String, value: String)

  /**
   * The key set of one key/value row: the shape of a row of every
   * `floatingRateNames.sections.<section>.rows` and of every `lenientPatterns.<family>.rows`.
   */
  val KeyValueRowSchema: KeySchema =
    KeySchema.uniform("manifest key/value row", Set("key", "value"))

  /** One configuration section: `{count, rows[]}`. */
  final case class KeyValueSection(count: Int, rows: Vector[KeyValueRow])

  /**
   * The key set of one configuration section: the shape of every
   * `floatingRateNames.sections.<section>` and of every `lenientPatterns.<family>`.
   */
  val KeyValueSectionSchema: KeySchema =
    KeySchema.uniform("manifest key/value section", Set("count", "rows"))

  /** One external name row: `{externalName, standardName}`. */
  final case class ExternalNameRow(externalName: String, standardName: String)

  val ExternalNameRowSchema: KeySchema =
    KeySchema.uniform("manifest external name row", Set("externalName", "standardName"))

  /** One external name group: `{count, rows[]}`. */
  final case class ExternalNameGroup(count: Int, rows: Vector[ExternalNameRow])

  val ExternalNameGroupSchema: KeySchema =
    KeySchema.uniform("manifest external name group", Set("count", "rows"))

  /** One alternate name row: `{alternateName, standardName}`. */
  final case class AlternateNameRow(alternateName: String, standardName: String)

  /**
   * The key set of one alternate name row: the shape of a row of every
   * `alternateNames.<family>.iniRows` and of every `alternateNames.<family>.apiExpandedRows`.
   */
  val AlternateNameRowSchema: KeySchema =
    KeySchema.uniform("manifest alternate name row", Set("alternateName", "standardName"))

  /** The alternate names of one family, as INI rows and as the expanded runtime view. */
  final case class AlternateNameTable(
      iniRowCount: Int,
      iniRows: Vector[AlternateNameRow],
      apiExpandedRowCount: Int,
      apiExpandedRows: Vector[AlternateNameRow])

  val AlternateNameTableSchema: KeySchema =
    KeySchema.uniform(
      "manifest alternate name table",
      Set("iniRowCount", "iniRows", "apiExpandedRowCount", "apiExpandedRows"))

  /** An index table: `{count, headers[], rows[{<header>: value}]}`, values as raw text. */
  final case class IndexManifestTable(
      count: Int,
      headers: Vector[String],
      rows: Vector[Map[String, String]])

  /**
   * The key set of an index table: the shape of `iborIndices`, `overnightIndices`, `priceIndices`
   * and `fxIndices`.
   *
   * The schema governs the three keys of the table itself and stops there: a '''row''' of one of
   * these tables is keyed by the table's own column headers, so its keys are data rather than a
   * record shape, and they are asserted against `headers`, for every row of every index table, by
   * `every table of the manifest carries the row count the schema of record documents`.
   */
  val IndexTableSchema: KeySchema =
    KeySchema.uniform("manifest index table", Set("count", "headers", "rows"))

  /** The floating rate name table: the constants, the total alias row count, and the sections. */
  final case class FloatingRateNameManifestTable(
      constants: NameGroup,
      aliasRowCount: Int,
      sections: Map[String, KeyValueSection])

  /**
   * The key set of the `floatingRateNames` table.
   *
   * The `sections` key holds a map from section name to section, not a record: the seven section
   * names are the division of the published alias space and are asserted as a set by `the manifest
   * floating rate name sections account for every transcribed row`. Each section is itself a fixed
   * shape, decoded through [[KeyValueSectionSchema]].
   */
  val FloatingRateNameTableSchema: KeySchema =
    KeySchema.uniform(
      "manifest floating rate name table",
      Set("constants", "aliasRowCount", "sections"))

  /** One default calendar row: `{currency, calendarId, resolvableAgainstStandardReferenceData}`. */
  final case class HolidayCalendarDefaultRow(
      currency: String,
      calendarId: String,
      resolvableAgainstStandardReferenceData: Boolean)

  val HolidayCalendarDefaultRowSchema: KeySchema =
    KeySchema.uniform(
      "manifest default calendar row",
      Set("currency", "calendarId", "resolvableAgainstStandardReferenceData"))

  /** The default calendar table: `{count, resolvableCount, unresolvableCount, rows[]}`. */
  final case class HolidayCalendarDefaultTable(
      count: Int,
      resolvableCount: Int,
      unresolvableCount: Int,
      rows: Vector[HolidayCalendarDefaultRow])

  val HolidayCalendarDefaultTableSchema: KeySchema =
    KeySchema.uniform(
      "manifest default calendar table",
      Set("count", "resolvableCount", "unresolvableCount", "rows"))

  /** One THBA year row: `{year, dates}`, the dates as `MMMdd` text separated by commas. */
  final case class ThbaYearRow(year: String, dates: String)

  val ThbaYearRowSchema: KeySchema =
    KeySchema.uniform("manifest THBA year row", Set("year", "dates"))

  /** The THBA table: `{yearRowCount, weekend, rows[]}`. */
  final case class ThbaTable(yearRowCount: Int, weekend: String, rows: Vector[ThbaYearRow])

  /**
   * The key set of one table of `holidayCalendarData`, of which the document carries one: `THBA`.
   *
   * The schema is of the table, not of the key it hangs under; the object that holds the keys has a
   * schema of its own, immediately below.
   */
  val ThbaTableSchema: KeySchema =
    KeySchema.uniform("manifest THBA table", Set("yearRowCount", "weekend", "rows"))

  /**
   * The key set of `holidayCalendarData` itself: the one calendar the capture emits table data for.
   *
   * This object looks like the map-shaped parts of the manifest, whose keys are '''data''' and are
   * asserted as sets rather than declared, but it is not one of them: only `THBA` is read out of
   * it, so a second calendar added by a later capture would sit in the map unread and leave that
   * whole table unmeasured. Declaring the key set refuses it instead. `Map[String, ThbaTable]`
   * remains the decoded type - the name is still the key of a table - and what is declared is which
   * keys may appear.
   */
  val HolidayCalendarDataSchema: KeySchema =
    KeySchema.uniform("manifest holiday calendar data", Set(ThbaCalendarName))

  // Declared in dependency order: a table's decoder captures the row decoder it needs while it is
  // itself initialised, so a row decoder below its table would be read as `null`. Each one is the
  // derived decoder of its model behind the key check of its schema, so the keys of an object are
  // settled before any field of it is read.
  implicit val currencyRowDecoder: Decoder[CurrencyManifestRow] =
    ParityHarness.strictObject(CurrencyRowSchema)(deriveDecoder[CurrencyManifestRow])

  implicit val currencyTableDecoder: Decoder[CurrencyManifestTable] =
    ParityHarness.strictObject(CurrencyTableSchema)(deriveDecoder[CurrencyManifestTable])

  implicit val currencyPairRowDecoder: Decoder[CurrencyPairManifestRow] =
    ParityHarness.strictObject(CurrencyPairRowSchema)(deriveDecoder[CurrencyPairManifestRow])

  implicit val currencyPairTableDecoder: Decoder[CurrencyPairManifestTable] =
    ParityHarness.strictObject(CurrencyPairTableSchema)(deriveDecoder[CurrencyPairManifestTable])

  implicit val countryRowDecoder: Decoder[CountryManifestRow] =
    ParityHarness.strictObject(CountryRowSchema)(deriveDecoder[CountryManifestRow])

  implicit val countryTableDecoder: Decoder[CountryManifestTable] =
    ParityHarness.strictObject(CountryTableSchema)(deriveDecoder[CountryManifestTable])

  implicit val nameGroupDecoder: Decoder[NameGroup] =
    ParityHarness.strictObject(NameGroupSchema)(deriveDecoder[NameGroup])

  implicit val keyValueRowDecoder: Decoder[KeyValueRow] =
    ParityHarness.strictObject(KeyValueRowSchema)(deriveDecoder[KeyValueRow])

  implicit val keyValueSectionDecoder: Decoder[KeyValueSection] =
    ParityHarness.strictObject(KeyValueSectionSchema)(deriveDecoder[KeyValueSection])

  implicit val externalNameRowDecoder: Decoder[ExternalNameRow] =
    ParityHarness.strictObject(ExternalNameRowSchema)(deriveDecoder[ExternalNameRow])

  implicit val externalNameGroupDecoder: Decoder[ExternalNameGroup] =
    ParityHarness.strictObject(ExternalNameGroupSchema)(deriveDecoder[ExternalNameGroup])

  implicit val alternateNameRowDecoder: Decoder[AlternateNameRow] =
    ParityHarness.strictObject(AlternateNameRowSchema)(deriveDecoder[AlternateNameRow])

  implicit val alternateNameTableDecoder: Decoder[AlternateNameTable] =
    ParityHarness.strictObject(AlternateNameTableSchema)(deriveDecoder[AlternateNameTable])

  implicit val indexTableDecoder: Decoder[IndexManifestTable] =
    ParityHarness.strictObject(IndexTableSchema)(deriveDecoder[IndexManifestTable])

  implicit val floatingRateNameTableDecoder: Decoder[FloatingRateNameManifestTable] =
    ParityHarness.strictObject(FloatingRateNameTableSchema)(
      deriveDecoder[FloatingRateNameManifestTable])

  implicit val holidayCalendarDefaultRowDecoder: Decoder[HolidayCalendarDefaultRow] =
    ParityHarness.strictObject(HolidayCalendarDefaultRowSchema)(
      deriveDecoder[HolidayCalendarDefaultRow])

  implicit val holidayCalendarDefaultTableDecoder: Decoder[HolidayCalendarDefaultTable] =
    ParityHarness.strictObject(HolidayCalendarDefaultTableSchema)(
      deriveDecoder[HolidayCalendarDefaultTable])

  implicit val thbaYearRowDecoder: Decoder[ThbaYearRow] =
    ParityHarness.strictObject(ThbaYearRowSchema)(deriveDecoder[ThbaYearRow])

  implicit val thbaTableDecoder: Decoder[ThbaTable] =
    ParityHarness.strictObject(ThbaTableSchema)(deriveDecoder[ThbaTable])

  /**
   * The decoder of `holidayCalendarData`: a map of calendar name to table, behind the key check of
   * [[HolidayCalendarDataSchema]].
   *
   * Declared implicitly so that the one place the document reads that key picks it up rather than
   * the library's own map decoder, which would accept a calendar this suite never reads.
   */
  implicit val holidayCalendarDataDecoder: Decoder[Map[String, ThbaTable]] =
    ParityHarness.strictObject(HolidayCalendarDataSchema)(
      Decoder.decodeMap[String, ThbaTable](KeyDecoder.decodeKeyString, thbaTableDecoder))

  //-------------------------------------------------------------------------
  // The document, read and parsed once, on first use.
  //-------------------------------------------------------------------------

  /**
   * The largest manifest this suite will read, in bytes.
   *
   * Eight mebibytes is roughly thirty times the committed document, which is a quarter of a
   * megabyte (254,640 bytes): room for a table to grow in and still a ceiling. The read below is
   * bounded so that a resource which is not the manifest at all is reported as too large rather
   * than exhausting the heap of the forked test JVM, and a capture that genuinely outgrew the bound
   * fails the load test with a message naming the figure.
   */
  val ManifestByteCeiling: Int = 8 * 1024 * 1024

  /**
   * Reads the committed manifest resource as UTF-8 text, without throwing.
   *
   * The module's effectful class path reader is not used: this suite holds no effect boundary, and
   * one committed resource needs none. The read carries the same three guarantees, because a weaker
   * read of this document would be a defect of this suite: bounded by [[ManifestByteCeiling]] - one
   * byte beyond it is requested, so a document at the ceiling is distinguishable from one over it -
   * closed by [[scala.util.Using]] whether it completes or fails, and decoded as '''strict''' UTF-8
   * by [[decodeUtf8]], so a corrupt capture cannot be rewritten into a passing comparison before
   * the JSON is parsed. The outcome is a value the load test asserts and every other accessor
   * reports through, so nothing throws here.
   *
   * A leading `/` on the resource name is tolerated because `ClassLoader.getResourceAsStream`
   * rejects it where `Class.getResourceAsStream` requires it.
   *
   * @return the text of the resource, or the reason it could not be read, naming the resource
   */
  private def readManifestText(): Either[String, String] = {
    val loader = Option(getClass.getClassLoader).getOrElse(ClassLoader.getPlatformClassLoader)
    val path = if (ManifestResource.startsWith("/")) ManifestResource.drop(1) else ManifestResource
    Option(loader.getResourceAsStream(path))
      .toRight(
        s"the reference data manifest '$ManifestResource' was not found on the test class path; " +
          "the resource is written by tools/parity-capture/capture-baseline.jsh and committed " +
          "under strata-basics/src/test/resources")
      .flatMap { stream =>
        Using(stream) { open =>
          val bytes = open.readNBytes(ManifestByteCeiling + 1)
          if (bytes.length > ManifestByteCeiling) {
            Left(
              s"the reference data manifest '$ManifestResource' is larger than the " +
                s"$ManifestByteCeiling byte ceiling this suite reads; the committed document is a " +
                "quarter of a megabyte, so the resource on the class path is not the one " +
                "tools/parity-capture/capture-baseline.jsh writes")
          } else {
            decodeUtf8(bytes)
          }
        }.toEither.left
          .map(failure =>
            s"the reference data manifest '$ManifestResource' could not be read: " +
              s"${failure.getClass.getName}: ${failure.getMessage}")
          .flatten
      }
  }

  /**
   * Decodes bytes as strict UTF-8, refusing a sequence that is not valid UTF-8.
   *
   * `new String(bytes, UTF_8)` is not this: its decoder '''replaces''' a malformed sequence with
   * `U+FFFD` and carries on, so a corrupted capture would reach the JSON parser as text that
   * differs from the bytes on disk. A decoder set to [[java.nio.charset.CodingErrorAction#REPORT]]
   * raises [[java.nio.charset.CharacterCodingException]] instead, which is what this suite needs:
   * the document is the evidence every other test is stated against. Kept separate from
   * [[readManifestText]], and visible to the suite, so the guarantee is asserted by a test over
   * bytes chosen for the purpose.
   *
   * @param bytes  the bytes read from the resource
   * @return the decoded text, or the reason the bytes are not UTF-8, naming the resource
   */
  def decodeUtf8(bytes: Array[Byte]): Either[String, String] =
    Try {
      StandardCharsets.UTF_8
        .newDecoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)
        .decode(ByteBuffer.wrap(bytes))
        .toString
    }.toEither.left
      .map(failure =>
        s"the reference data manifest '$ManifestResource' is not valid UTF-8 text: " +
          s"${failure.getClass.getName}: ${failure.getMessage}")

  /**
   * The top level of the manifest, or the reason the document could not be obtained.
   *
   * Held as an `Either` rather than read into a member that throws, so the test whose subject is
   * the resource can assert the outcome and print the reason, and nothing happens while this object
   * initialises. The four distinguishable failures - absent, empty text, not JSON, JSON but not an
   * object - each name the resource and the script that writes it.
   */
  lazy val documentOrFailure: Either[String, JsonObject] =
    readManifestText().flatMap(documentOf)

  /**
   * Turns the text of the resource into the top level of the document, or into the reason it is not
   * one.
   *
   * Separated from the read, and visible to the suite, for the same reason [[decodeUtf8]] is: the
   * three ways text can still not be the manifest are each asserted over text chosen for the
   * purpose, the committed resource exercising the success path only.
   *
   * @param text  the text read from the resource
   * @return the top level of the document, or the reason the text is not a JSON object
   */
  def documentOf(text: String): Either[String, JsonObject] =
    if (text.trim.isEmpty) {
      Left(
        s"the reference data manifest '$ManifestResource' read as empty text; the resource is " +
          "written by tools/parity-capture/capture-baseline.jsh and committed under " +
          "strata-basics/src/test/resources")
    } else {
      parse(text)
        .left
        .map(failure =>
          s"the reference data manifest '$ManifestResource' is not valid JSON: " +
            failure.getMessage)
        .flatMap(json =>
          json.asObject.toRight(
            s"the reference data manifest '$ManifestResource' is not a JSON object; the schema " +
              "of record is section 7 of tools/parity-capture/README.md"))
    }

  /**
   * The manifest document, for the tests that assert its content.
   *
   * A document that could not be obtained is reported as a '''failed test''' carrying the
   * diagnostic, through [[org.scalatest.Assertions#fail]], rather than as an exception out of an
   * initialiser: the test that reads the manifest is the one that reports it unreadable.
   */
  private lazy val document: JsonObject =
    documentOrFailure.fold(diagnostic => Assertions.fail(diagnostic), identity)

  /** The top-level keys, in the order the capture wrote them. */
  lazy val documentKeys: Vector[String] = document.keys.toVector

  /**
   * Collects the key names of a JSON value and of everything nested inside it.
   *
   * A fold over the JSON algebra rather than a cast-and-test, so every shape a value can take is
   * accounted for.
   *
   * @param json  the value to collect from
   * @return every key name occurring at or below this value
   */
  private def keyNamesOf(json: Json): Set[String] =
    json.fold(
      jsonNull = Set.empty[String],
      jsonBoolean = _ => Set.empty[String],
      jsonNumber = _ => Set.empty[String],
      jsonString = _ => Set.empty[String],
      jsonArray =
        elements => elements.foldLeft(Set.empty[String])((names, element) =>
          names ++ keyNamesOf(element)),
      jsonObject =
        fields => fields.values.foldLeft(fields.keys.toSet)((names, value) =>
          names ++ keyNamesOf(value)))

  /**
   * Every key name that occurs anywhere in the document, at any depth.
   *
   * Read for one purpose: to assert that no part of the document declares a runtime provider. Key
   * '''names''' only, each recorded once, since the question asked of this set is only whether a
   * name is present.
   */
  lazy val documentKeyNames: Set[String] = keyNamesOf(Json.fromJsonObject(document))

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
  // The typed views the tests assert from, each decoded on first use and then held.
  //
  // Lazy on purpose: a `lazy val` memoises, so decoding happens once, but inside the test that
  // reads the view rather than while this object initialises. A section whose shape has changed
  // therefore fails the tests that read it, naming the key and the JSON path, and leaves every
  // other section still asserted, where eager decoding would abort the whole suite.
  //-------------------------------------------------------------------------

  lazy val schemaVersion: Int = keyAs[Int]("schemaVersion")
  lazy val generator: String = keyAs[String]("generator")
  lazy val randomSeed: Int = keyAs[Int]("randomSeed")
  lazy val currencies: CurrencyManifestTable = keyAs[CurrencyManifestTable]("currencies")
  lazy val marketConventionPriority: Vector[String] = keyAs[Vector[String]]("marketConventionPriority")
  lazy val currencyPairs: CurrencyPairManifestTable = keyAs[CurrencyPairManifestTable]("currencyPairs")
  lazy val countries: CountryManifestTable = keyAs[CountryManifestTable]("countries")
  lazy val iborIndices: IndexManifestTable = keyAs[IndexManifestTable](IborIndicesKey)
  lazy val overnightIndices: IndexManifestTable = keyAs[IndexManifestTable](OvernightIndicesKey)
  lazy val priceIndices: IndexManifestTable = keyAs[IndexManifestTable]("priceIndices")
  lazy val fxIndices: IndexManifestTable = keyAs[IndexManifestTable]("fxIndices")

  lazy val floatingRateNames: FloatingRateNameManifestTable =
    keyAs[FloatingRateNameManifestTable]("floatingRateNames")

  lazy val holidayCalendarDefaultByCurrency: HolidayCalendarDefaultTable =
    keyAs[HolidayCalendarDefaultTable](HolidayCalendarDefaultKey)

  private lazy val holidayCalendarData: Map[String, ThbaTable] =
    keyAs[Map[String, ThbaTable]](HolidayCalendarDataKey)

  lazy val holidayCalendarDataThba: ThbaTable = holidayCalendarData(ThbaCalendarName)

  lazy val externalNames: Map[String, Map[String, ExternalNameGroup]] =
    keyAs[Map[String, Map[String, ExternalNameGroup]]]("externalNames")

  lazy val lenientPatterns: Map[String, KeyValueSection] =
    keyAs[Map[String, KeyValueSection]]("lenientPatterns")

  lazy val alternateNames: Map[String, AlternateNameTable] =
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
  // The documented object shapes, each with a specimen of it taken from the document.
  //
  // The inventory the strictness tests read, and what makes the key sets declared beside the row
  // models statements about the committed artefact: every schema is matched against a real object
  // of its shape, and the accept-and-refuse tests perturb that same object.
  //-------------------------------------------------------------------------

  /**
   * One fixed-shape object of the manifest: its declared key set, where the document carries an
   * instance of it, and the decoder this suite reads that shape through.
   *
   * `decode` answers `Decoder.Result[Any]` because the two questions the strictness tests ask of it
   * - whether it accepted the object, and what it said when it refused - do not depend on the type
   * the shape decodes to.
   *
   * @param schema  the declared key set of the shape
   * @param path  where the document carries an instance, as [[specimenAt]] reads a path
   * @param decode  the decoder under test, exactly as this suite decodes that shape
   */
  final case class StrictShape(
      schema: KeySchema,
      path: String,
      decode: Json => Decoder.Result[Any]) {

    /** The instance of this shape the committed document carries. */
    def specimen: JsonObject = specimenAt(path)

    /** The keys that instance carries. */
    def specimenKeys: Set[String] = specimen.keys.toSet
  }

  /**
   * The instance of an object shape the document carries at a path.
   *
   * The path is read left to right, each step naming a key of the object reached so far, and a step
   * suffixed `[]` meaning the '''first''' element of the array that key holds - so
   * `currencies.rows[]` is the first row of the currency table. A step that cannot be taken throws,
   * naming the whole path and that step: a path that no longer resolves is a change in the capture,
   * which a decode failure somewhere else would not say.
   *
   * @param path  the dotted path, any step of which may end `[]`
   * @return the object the path names
   */
  private def specimenAt(path: String): JsonObject = {
    val reached = path.split('.').toVector.foldLeft(Json.fromJsonObject(document)) { (json, step) =>
      val indexed = step.endsWith("[]")
      val key = if (indexed) step.dropRight(2) else step
      val field =
        json.asObject.flatMap(_(key)).getOrElse(specimenFailure(path, step, "names no key"))
      if (indexed) {
        field.asArray.flatMap(_.headOption).getOrElse(specimenFailure(path, step, "holds no row"))
      } else {
        field
      }
    }
    reached.asObject.getOrElse(specimenFailure(path, path, "does not name an object"))
  }

  /**
   * Reports a declared specimen path that the document does not answer.
   *
   * @param path  the whole path, as declared in [[StrictShapes]]
   * @param step  the step that could not be taken
   * @param what  what is wrong with that step
   * @return never; the call throws
   */
  private def specimenFailure(path: String, step: String, what: String): Nothing =
    throw new IllegalStateException(
      s"the reference data manifest '$ManifestResource' carries no object at the specimen path " +
        s"'$path': the step '$step' $what. The strictness tests take a specimen of every " +
        "documented object shape from the document itself, so a path that no longer resolves is " +
        "a change in the capture and the schema declared beside that shape needs revisiting; " +
        "the schema of record is section 7 of tools/parity-capture/README.md")

  /**
   * The number of fixed-shape objects the document carries, one per row model of this suite.
   *
   * Asserted against [[StrictShapes]], so a shape dropped from that vector - and so out of the
   * drift check and the accept-and-refuse tests - is reported.
   */
  val DocumentedObjectShapeCount: Int = 20

  /** The key a later capture is imagined to have added, for the refusal tests. */
  val UncapturedKey: String = "addedByALaterCapture"

  /**
   * Every fixed-shape object of the manifest, each with a specimen of it.
   *
   * Twenty: one per row model, plus `holidayCalendarData`, whose keys name calendars but of which
   * only `THBA` is ever read. That is every object shape of the document except the two kinds whose
   * keys are genuinely '''data''' - the top level, and the maps keyed by a name - each asserted as
   * a set by the coverage, identity and count tests instead.
   *
   * A shape carried at more than one path is listed once, at the first instance the document holds
   * of it: the shape of a key/value section does not depend on whether it is a lenient pattern
   * table or an alias section.
   */
  val StrictShapes: Vector[StrictShape] =
    Vector(
      StrictShape(CurrencyTableSchema, "currencies", _.as[CurrencyManifestTable]),
      StrictShape(CurrencyRowSchema, "currencies.rows[]", _.as[CurrencyManifestRow]),
      StrictShape(CurrencyPairTableSchema, "currencyPairs", _.as[CurrencyPairManifestTable]),
      StrictShape(CurrencyPairRowSchema, "currencyPairs.rows[]", _.as[CurrencyPairManifestRow]),
      StrictShape(CountryTableSchema, "countries", _.as[CountryManifestTable]),
      StrictShape(CountryRowSchema, "countries.rows[]", _.as[CountryManifestRow]),
      StrictShape(NameGroupSchema, DayCountsKey, _.as[NameGroup]),
      StrictShape(IndexTableSchema, IborIndicesKey, _.as[IndexManifestTable]),
      StrictShape(
        FloatingRateNameTableSchema,
        FloatingRateNamesKey,
        _.as[FloatingRateNameManifestTable]),
      StrictShape(
        KeyValueSectionSchema,
        s"$FloatingRateNamesKey.sections.$IborSection",
        _.as[KeyValueSection]),
      StrictShape(
        KeyValueRowSchema,
        s"$FloatingRateNamesKey.sections.$IborSection.rows[]",
        _.as[KeyValueRow]),
      StrictShape(
        ExternalNameGroupSchema,
        s"externalNames.$DayCountFamily.FpML",
        _.as[ExternalNameGroup]),
      StrictShape(
        ExternalNameRowSchema,
        s"externalNames.$DayCountFamily.FpML.rows[]",
        _.as[ExternalNameRow]),
      StrictShape(
        AlternateNameTableSchema,
        s"alternateNames.$IborIndexFamily",
        _.as[AlternateNameTable]),
      StrictShape(
        AlternateNameRowSchema,
        s"alternateNames.$IborIndexFamily.iniRows[]",
        _.as[AlternateNameRow]),
      StrictShape(
        HolidayCalendarDefaultTableSchema,
        HolidayCalendarDefaultKey,
        _.as[HolidayCalendarDefaultTable]),
      StrictShape(
        HolidayCalendarDefaultRowSchema,
        s"$HolidayCalendarDefaultKey.rows[]",
        _.as[HolidayCalendarDefaultRow]),
      StrictShape(
        HolidayCalendarDataSchema,
        HolidayCalendarDataKey,
        _.as[Map[String, ThbaTable]]),
      StrictShape(
        ThbaTableSchema,
        s"$HolidayCalendarDataKey.$ThbaCalendarName",
        _.as[ThbaTable]),
      StrictShape(
        ThbaYearRowSchema,
        s"$HolidayCalendarDataKey.$ThbaCalendarName.rows[]",
        _.as[ThbaYearRow]))

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
   * The month abbreviation is resolved through the table above rather than a date time formatter,
   * so the parse does not depend on the locale or the calendar data of the host; the captured text
   * is ASCII English by construction.
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

  /** The captured THBA year rows, in the form this module holds them in. */
  lazy val thbaRows: Vector[(Int, List[MonthDay])] =
    holidayCalendarDataThba.rows.map(row =>
      (row.year.toInt, row.dates.split(",").toList.map(token => monthDayOf(row.year, token))))

  /** The captured THBA weekend, in the form this module holds it in. */
  lazy val thbaWeekend: Set[DayOfWeek] =
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
  // The two wide index tables, rendered to comparable cells.
  //-------------------------------------------------------------------------

  /**
   * Renders a captured boolean cell.
   *
   * The column is spelled `TRUE`/`FALSE` in the Ibor and overnight index data and `true`/`false` in
   * the price index data, and the case of a boolean literal is not data. Text that is neither is
   * returned unchanged, so the comparison reports it with the row and column named rather than an
   * exception being thrown out of a cell renderer.
   *
   * @param captured  the captured cell text
   * @return the normal form of the cell
   */
  def booleanCell(captured: String): String =
    captured match {
      case "TRUE" | "true" => "TRUE"
      case "FALSE" | "false" => "FALSE"
      case other => other
    }

  /** Renders a boolean this module holds, in the same normal form as [[booleanCell]]. */
  def booleanCell(value: Boolean): String = if (value) "TRUE" else "FALSE"

  /**
   * The canonical day count name of every spelling the Java registry resolved, from the manifest.
   *
   * The reference data spelled a day count in two ways in the same column - `ACT/360` on the Czech
   * koruna rows and `Act/360` everywhere else - so the captured text has to be brought to one
   * spelling before it can be compared. The index is built from the '''manifest's own'''
   * `dayCounts` names and the registration rule of the Java registry the capture read, and from
   * nothing else: [[com.opengamma.strata.basics.date.DayCount]] is a subject of this suite, so
   * resolving a cell through it would rewrite expectation and answer the same wrong way and still
   * compare equal.
   *
   * The rule is transcribed from
   * `modules/collect/src/main/java/com/opengamma/strata/collect/named/`
   * `ExtendedEnum.java:225-236`, which registers every constant twice - under its canonical
   * name and under the English upper case of it - with the first registration of a key winning:
   *
   * {{{
   * instances.putIfAbsent(instance.getName(), instance);
   * instances.putIfAbsent(instance.getName().toUpperCase(Locale.ENGLISH), instance);
   * }}}
   *
   * The canonical names are registered first because Java iterates a holder's declared fields in an
   * undefined order, which would matter if an upper case form collided with the canonical name of a
   * '''different''' member; the twenty-one captured names hold no such collision, and ten differ
   * from their upper case form, giving an index of thirty-one keys. The fold is
   * [[java.util.Locale#ENGLISH]], so the index does not depend on the host that runs the suite.
   */
  lazy val CapturedDayCountNames: Map[String, String] = {
    val canonical = nameGroup(DayCountsKey).names
    canonical.foldLeft(canonical.map(name => name -> name).toMap) { (index, name) =>
      val upperCase = name.toUpperCase(Locale.ENGLISH)
      if (index.contains(upperCase)) index else index.updated(upperCase, name)
    }
  }

  /**
   * Renders a captured day count cell as the canonical name of the convention it names.
   *
   * The captured text is mapped through [[CapturedDayCountNames]], the manifest's own name list
   * read through the registration rule of the Java registry the capture read; no production type is
   * consulted. Text the index does not hold is returned unchanged, so the comparison reports it
   * against the name this module renders: that is what happens to `Bus/252 BRBD`, a day count that
   * carries a calendar and so is not one of the twenty-one standard names.
   *
   * @param captured  the captured cell text
   * @return the canonical name of the day count, or the text when the captured names hold no
   *         spelling of it
   */
  def dayCountCell(captured: String): String = CapturedDayCountNames.getOrElse(captured, captured)

  /** The one calendar identifier whose presence absorbs the others it is composed with. */
  val NoHolidaysCalendarName: String = "NoHolidays"

  /**
   * Splits a composite calendar name on one separator, keeping empty parts.
   *
   * The Java code this is transcribed from neither trims nor omits empty results, so `split` is
   * given a limit of `-1`: Scala's one-argument `split` drops trailing empty strings, which would
   * make `GBLO+` normalise to `GBLO` here and to the empty-part composite there.
   *
   * @param name  the composite name
   * @param separator  the separator to split on
   * @return the parts, in order, including any empty ones
   */
  private def splitCalendarName(name: String, separator: Char): Vector[String] =
    name.split(Regex.quote(separator.toString), -1).toVector

  /**
   * The name a composite calendar identifier is normalised to, transcribed from the Java factory.
   *
   * This is the expected side of the comparison, so it may not call
   * [[com.opengamma.strata.basics.date.HolidayCalendarId]]: the identifier's normalisation is part
   * of what the manifest exists to check, and this module's factory would rewrite both sides the
   * same wrong way. The rule is transcribed from
   * `modules/basics/src/main/java/com/opengamma/strata/basics/date/HolidayCalendarId.java:87-120`
   * (`of` and `create`), the code that produced the names the Java parser held:
   *
   *  - a name containing `~` is a linked identifier: its parts are normalised by this same rule,
   *    deduplicated, sorted by name and rejoined with `~`, and a part naming `NoHolidays` absorbs
   *    the whole identifier, the absorption being decided on the normalised parts as in Java;
   *  - a name containing `+` is a combined identifier: parts whose '''raw''' text is `NoHolidays`
   * are dropped before anything else - Java filters on the raw text, so a part that only normalises
   *    to `NoHolidays` survives the filter - and the rest are normalised, deduplicated, sorted by
   *    name and rejoined with `+`;
   *  - any other name is its own normal form.
   *
   * The sort is the lexicographic order of [[java.lang.String]], which is what Java's
   * `comparing(HolidayCalendarId::getName)` compares with, and `~` is tested before `+` because
   * `create` tests it first.
   *
   * @param name  the captured composite or simple name
   * @return the name the Java factory normalises it to
   */
  private def normalisedCalendarName(name: String): String =
    if (name.indexOf('~') >= 0) {
      val parts = splitCalendarName(name, '~').map(normalisedCalendarName).distinct.sorted
      if (parts.contains(NoHolidaysCalendarName)) NoHolidaysCalendarName else parts.mkString("~")
    } else if (name.indexOf('+') >= 0) {
      splitCalendarName(name, '+')
        .filterNot(_ == NoHolidaysCalendarName)
        .map(normalisedCalendarName)
        .distinct
        .sorted
        .mkString("+")
    } else {
      name
    }

  /**
   * Renders a captured calendar cell as the name of the identifier it denotes.
   *
   * A composite identifier is normalised by deduplicating and sorting its parts, so the column text
   * `SGSI+GBLO` denotes the identifier named `GBLO+SGSI` and comparing the raw text would report
   * that normalisation as a difference. The normalisation is [[normalisedCalendarName]],
   * transcribed from the Java factory rather than obtained by calling this module's, so a factory
   * that normalised a composite name wrongly fails this comparison instead of surviving it.
   *
   * @param captured  the captured cell text
   * @return the name of the identifier the text denotes
   */
  def calendarCell(captured: String): String = normalisedCalendarName(captured)

  /**
   * Renders one captured Ibor index row as cells, in [[IborIndexHeaders]] order.
   *
   * @param captured  the captured row, keyed by column header
   * @return the cells of the row
   */
  def capturedIborCells(captured: Map[String, String]): Vector[String] =
    Vector(
      captured("Name"),
      captured("Currency"),
      booleanCell(captured("Active")),
      dayCountCell(captured("Day Count")),
      calendarCell(captured("Fixing Calendar")),
      captured("Offset Days"),
      calendarCell(captured("Offset Calendar")),
      calendarCell(captured("Effective Date Calendar")),
      captured("Tenor"),
      captured("Tenor Convention"),
      captured("FixingTime"),
      captured("FixingZone"),
      dayCountCell(captured("Fixed Leg Day Count")))

  /**
   * Renders one Ibor index row of this module as cells, in [[IborIndexHeaders]] order.
   *
   * The time and the zone render through their own textual forms, which are the ISO forms the
   * reference data was written in: `11:55` and `Europe/London`.
   *
   * @param row  the transcribed row
   * @return the cells of the row
   */
  def portIborCells(row: IborIndexRow): Vector[String] =
    Vector(
      row.name,
      row.currency.code,
      booleanCell(row.active),
      row.dayCount.name,
      row.fixingCalendar.name,
      row.offsetDays.toString,
      row.offsetCalendar.name,
      row.effectiveDateCalendar.name,
      row.tenor.name,
      row.tenorConvention,
      row.fixingTime.toString,
      row.fixingZone.getId,
      row.fixedLegDayCount.name)

  /**
   * Renders one captured overnight index row as cells, in [[OvernightIndexHeaders]] order.
   *
   * @param captured  the captured row, keyed by column header
   * @return the cells of the row
   */
  def capturedOvernightCells(captured: Map[String, String]): Vector[String] =
    Vector(
      captured("Name"),
      captured("Currency"),
      booleanCell(captured("Active")),
      dayCountCell(captured("Day Count")),
      calendarCell(captured("Fixing Calendar")),
      captured("Publication Offset Days"),
      captured("Effective Offset Days"),
      dayCountCell(captured("Fixed Leg Day Count")))

  /**
   * Renders one overnight index row of this module as cells, in [[OvernightIndexHeaders]] order.
   *
   * @param row  the transcribed row
   * @return the cells of the row
   */
  def portOvernightCells(row: OvernightIndexRow): Vector[String] =
    Vector(
      row.name,
      row.currency.code,
      booleanCell(row.active),
      row.dayCount.name,
      row.fixingCalendar.name,
      row.publicationOffsetDays.toString,
      row.effectiveOffsetDays.toString,
      row.fixedLegDayCount.name)

  //-------------------------------------------------------------------------
  // The alternate spellings of the index families.
  //-------------------------------------------------------------------------

  /**
   * Expands a transcribed alternate name table the way a named family does.
   *
   * Alongside each supplied spelling the table holds that spelling folded to upper case, unless it
   * already holds one identical to it, so an alternate name is reachable both from the exact lookup
   * and from the lenient one; a supplied spelling is never displaced. Written here so the captured
   * INI rows and the captured expanded rows can be held to each other: the expansion only ever adds
   * an upper-case key, so a missing INI row cannot be reinstated by it and one transcribed in the
   * wrong case expands to a different set of keys.
   *
   * @param supplied  the transcribed table, alternate spelling to canonical name
   * @return the table as the family publishes it
   */
  def expandAlternateNames(supplied: Map[String, String]): Map[String, String] =
    supplied.toList.sortBy { case (spelling, _) => spelling }.foldLeft(supplied) {
      case (expanded, (spelling, canonicalName)) =>
        val upper = spelling.toUpperCase(Locale.ENGLISH)
        if (expanded.contains(upper)) expanded else expanded.updated(upper, canonicalName)
    }

  /**
   * The alternate spellings this module publishes for one index family, as the family publishes
   * them.
   *
   * @param family  the family name the document keys the table under
   * @return the alternate spellings mapped to canonical names
   */
  def alternateNamesOf(family: String): Map[String, String] =
    family match {
      case IborIndexFamily => IborIndex.namedEnum.alternateNames
      case OvernightIndexFamily => OvernightIndex.namedEnum.alternateNames
      case FxIndexFamily => FxIndex.namedEnum.alternateNames
      case other =>
        throw new IllegalStateException(
          s"the reference data manifest records alternate names for '$other', which this suite " +
            "has no family for; the families the capture records are " +
            s"$IborIndexFamily, $OvernightIndexFamily and $FxIndexFamily")
    }

  /**
   * The canonical names of one index family, which are the names an alternate spelling may name.
   *
   * @param family  the family name the document keys the table under
   * @return the canonical names of the members of the family
   */
  def canonicalNamesOf(family: String): Set[String] =
    family match {
      case IborIndexFamily => IborIndex.namedEnum.byCanonicalName.keySet
      case OvernightIndexFamily => OvernightIndex.namedEnum.byCanonicalName.keySet
      case FxIndexFamily => FxIndex.namedEnum.byCanonicalName.keySet
      case other =>
        throw new IllegalStateException(
          s"the reference data manifest records alternate names for '$other', which this suite " +
            "has no family for; the families the capture records are " +
            s"$IborIndexFamily, $OvernightIndexFamily and $FxIndexFamily")
    }

  /**
   * The named families of this module that declare no alternate spelling, each with its table.
   *
   * The capture recorded alternate names for three families and none for the rest. Holding the
   * others here with their tables, rather than their names alone, is what lets the absence be
   * asserted instead of assumed. The stub convention is not among them: its accepted spellings are
   * a property of an enumeration rather than captured data, asserted by the test of that family.
   */
  val FamiliesWithoutAlternateNames: Map[String, Map[String, String]] =
    Map(
      "PriceIndex" -> PriceIndex.namedEnum.alternateNames,
      DayCountFamily -> DayCount.namedEnum.alternateNames,
      RollConventionFamily -> RollConvention.namedEnum.alternateNames,
      BusinessDayConventionFamily -> BusinessDayConvention.namedEnum.alternateNames,
      PeriodAdditionConventionFamily -> PeriodAdditionConvention.namedEnum.alternateNames,
      "DateSequence" -> DateSequence.namedEnum.alternateNames,
      "FloatingRateName" -> FloatingRateName.namedEnum.alternateNames,
      "Currency" -> Currency.namedEnum.alternateNames)

  //-------------------------------------------------------------------------
  // The constant holders of this module, named one by one.
  //-------------------------------------------------------------------------

  /**
   * The constants of `BusinessDayConventions`, named rather than discovered.
   *
   * The capture read its holder reflectively; nothing here reflects over anything, so the
   * counterpart is this list, and a constant that disappeared is a compile error in this file.
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

  /**
   * The constants of `IborIndices`, named rather than discovered.
   *
   * One hundred and thirteen of the two hundred and seventy-one published Ibor indices have a
   * constant: the benchmark families a caller writes in code are named and the rest are reached by
   * name.
   */
  val IborIndexConstants: Vector[IborIndex] =
    Vector(
      IborIndices.GBP_LIBOR_1W, IborIndices.GBP_LIBOR_1M, IborIndices.GBP_LIBOR_2M,
      IborIndices.GBP_LIBOR_3M, IborIndices.GBP_LIBOR_6M, IborIndices.GBP_LIBOR_12M,
      IborIndices.CHF_LIBOR_1W, IborIndices.CHF_LIBOR_1M, IborIndices.CHF_LIBOR_2M,
      IborIndices.CHF_LIBOR_3M, IborIndices.CHF_LIBOR_6M, IborIndices.CHF_LIBOR_12M,
      IborIndices.EUR_LIBOR_1W, IborIndices.EUR_LIBOR_1M, IborIndices.EUR_LIBOR_2M,
      IborIndices.EUR_LIBOR_3M, IborIndices.EUR_LIBOR_6M, IborIndices.EUR_LIBOR_12M,
      IborIndices.JPY_LIBOR_1W, IborIndices.JPY_LIBOR_1M, IborIndices.JPY_LIBOR_2M,
      IborIndices.JPY_LIBOR_3M, IborIndices.JPY_LIBOR_6M, IborIndices.JPY_LIBOR_12M,
      IborIndices.USD_LIBOR_1W, IborIndices.USD_LIBOR_1M, IborIndices.USD_LIBOR_2M,
      IborIndices.USD_LIBOR_3M, IborIndices.USD_LIBOR_6M, IborIndices.USD_LIBOR_12M,
      IborIndices.EUR_EURIBOR_1W, IborIndices.EUR_EURIBOR_2W, IborIndices.EUR_EURIBOR_1M,
      IborIndices.EUR_EURIBOR_2M, IborIndices.EUR_EURIBOR_3M, IborIndices.EUR_EURIBOR_6M,
      IborIndices.EUR_EURIBOR_9M, IborIndices.EUR_EURIBOR_12M, IborIndices.JPY_TIBOR_JAPAN_1W,
      IborIndices.JPY_TIBOR_JAPAN_1M, IborIndices.JPY_TIBOR_JAPAN_2M,
      IborIndices.JPY_TIBOR_JAPAN_3M, IborIndices.JPY_TIBOR_JAPAN_6M,
      IborIndices.JPY_TIBOR_JAPAN_12M, IborIndices.JPY_TIBOR_EUROYEN_1W,
      IborIndices.JPY_TIBOR_EUROYEN_1M, IborIndices.JPY_TIBOR_EUROYEN_2M,
      IborIndices.JPY_TIBOR_EUROYEN_3M, IborIndices.JPY_TIBOR_EUROYEN_6M,
      IborIndices.JPY_TIBOR_EUROYEN_12M, IborIndices.AUD_BBSW_1M, IborIndices.AUD_BBSW_2M,
      IborIndices.AUD_BBSW_3M, IborIndices.AUD_BBSW_4M, IborIndices.AUD_BBSW_5M,
      IborIndices.AUD_BBSW_6M, IborIndices.CAD_CDOR_1M, IborIndices.CAD_CDOR_2M,
      IborIndices.CAD_CDOR_3M, IborIndices.CAD_CDOR_6M, IborIndices.CAD_CDOR_12M,
      IborIndices.CZK_PRIBOR_1W, IborIndices.CZK_PRIBOR_2W, IborIndices.CZK_PRIBOR_1M,
      IborIndices.CZK_PRIBOR_2M, IborIndices.CZK_PRIBOR_3M, IborIndices.CZK_PRIBOR_6M,
      IborIndices.CZK_PRIBOR_9M, IborIndices.CZK_PRIBOR_12M, IborIndices.DKK_CIBOR_1W,
      IborIndices.DKK_CIBOR_2W, IborIndices.DKK_CIBOR_1M, IborIndices.DKK_CIBOR_2M,
      IborIndices.DKK_CIBOR_3M, IborIndices.DKK_CIBOR_6M, IborIndices.DKK_CIBOR_9M,
      IborIndices.DKK_CIBOR_12M, IborIndices.HUF_BUBOR_1W, IborIndices.HUF_BUBOR_2W,
      IborIndices.HUF_BUBOR_1M, IborIndices.HUF_BUBOR_2M, IborIndices.HUF_BUBOR_3M,
      IborIndices.HUF_BUBOR_6M, IborIndices.HUF_BUBOR_9M, IborIndices.HUF_BUBOR_12M,
      IborIndices.MXN_TIIE_4W, IborIndices.MXN_TIIE_13W, IborIndices.MXN_TIIE_26W,
      IborIndices.NOK_NIBOR_1W, IborIndices.NOK_NIBOR_1M, IborIndices.NOK_NIBOR_2M,
      IborIndices.NOK_NIBOR_3M, IborIndices.NOK_NIBOR_6M, IborIndices.NZD_BKBM_1M,
      IborIndices.NZD_BKBM_2M, IborIndices.NZD_BKBM_3M, IborIndices.NZD_BKBM_4M,
      IborIndices.NZD_BKBM_5M, IborIndices.NZD_BKBM_6M, IborIndices.PLN_WIBOR_1W,
      IborIndices.PLN_WIBOR_1M, IborIndices.PLN_WIBOR_3M, IborIndices.PLN_WIBOR_6M,
      IborIndices.PLN_WIBOR_12M, IborIndices.SEK_STIBOR_1W, IborIndices.SEK_STIBOR_1M,
      IborIndices.SEK_STIBOR_2M, IborIndices.SEK_STIBOR_3M, IborIndices.SEK_STIBOR_6M,
      IborIndices.ZAR_JIBAR_1M, IborIndices.ZAR_JIBAR_3M, IborIndices.ZAR_JIBAR_6M,
      IborIndices.ZAR_JIBAR_12M)

  /**
   * The constants of `OvernightIndices`, named rather than discovered.
   *
   * Twenty-one constants naming twenty distinct indices: `EUR_ESTER` is the retired spelling of
   * `EUR_ESTR` and both constants are the same index, which the captured names record by holding
   * `EUR-ESTR` twice.
   */
  val OvernightIndexConstants: Vector[OvernightIndex] =
    Vector(
      OvernightIndices.GBP_SONIA, OvernightIndices.CHF_SARON, OvernightIndices.CHF_TOIS,
      OvernightIndices.EUR_EONIA, OvernightIndices.EUR_ESTR, OvernightIndices.EUR_ESTER,
      OvernightIndices.JPY_TONAR, OvernightIndices.USD_FED_FUND, OvernightIndices.USD_SOFR,
      OvernightIndices.USD_AMERIBOR, OvernightIndices.AUD_AONIA, OvernightIndices.BRL_CDI,
      OvernightIndices.CAD_CORRA, OvernightIndices.DKK_TNR, OvernightIndices.NOK_NOWA,
      OvernightIndices.NZD_NZIONA, OvernightIndices.PLN_POLONIA, OvernightIndices.PLN_POLSTR,
      OvernightIndices.SEK_SIOR, OvernightIndices.THB_THOR, OvernightIndices.ZAR_SABOR)

  /** The constants of `PriceIndices`, named rather than discovered; nine, one per published row. */
  val PriceIndexConstants: Vector[PriceIndex] =
    Vector(
      PriceIndices.GB_HICP, PriceIndices.GB_RPI, PriceIndices.GB_RPIX, PriceIndices.CH_CPI,
      PriceIndices.EU_AI_CPI, PriceIndices.EU_EXT_CPI, PriceIndices.JP_CPI_EXF,
      PriceIndices.US_CPI_U, PriceIndices.FR_EXT_CPI)

  /**
   * The constants of `FxIndices`, named rather than discovered.
   *
   * Eight constants over sixteen published rows: the four European Central Bank and four WM/Reuters
   * fixings are named, and the eight Asian and Latin American ones are reached by name.
   */
  val FxIndexConstants: Vector[FxIndex] =
    Vector(
      FxIndices.EUR_CHF_ECB, FxIndices.EUR_GBP_ECB, FxIndices.EUR_JPY_ECB, FxIndices.EUR_USD_ECB,
      FxIndices.USD_CHF_WM, FxIndices.GBP_USD_WM, FxIndices.EUR_USD_WM, FxIndices.USD_JPY_WM)

  /**
   * The constants of `DayCounts`, named rather than discovered.
   *
   * Twenty-one, which is the whole family: `Bus/252` has no constant because it carries a calendar
   * and so is a member per calendar rather than a named one.
   */
  val DayCountConstants: Vector[DayCount] =
    Vector(
      DayCounts.ONE_ONE, DayCounts.ACT_ACT_ISDA, DayCounts.ACT_ACT_ICMA, DayCounts.ACT_ACT_AFB,
      DayCounts.ACT_ACT_YEAR, DayCounts.ACT_365_ACTUAL, DayCounts.ACT_365L, DayCounts.ACT_360,
      DayCounts.ACT_364, DayCounts.ACT_365F, DayCounts.ACT_365_25, DayCounts.NL_360,
      DayCounts.NL_365, DayCounts.THIRTY_360_ISDA, DayCounts.THIRTY_U_360,
      DayCounts.THIRTY_U_360_EOM, DayCounts.THIRTY_360_PSA, DayCounts.THIRTY_E_360_ISDA,
      DayCounts.THIRTY_E_360, DayCounts.THIRTY_EPLUS_360, DayCounts.THIRTY_E_365)

  /**
   * The constants of `RollConventions`, named rather than discovered.
   *
   * Forty-five, which is the whole family: the eight standard conventions, the thirty day-of-month
   * members and the seven day-of-week members. There is no `DAY_31`, because the thirty-first of
   * the month is the end of it and the reference data maps that text onto `EOM`.
   */
  val RollConventionConstants: Vector[RollConvention] =
    Vector(
      RollConventions.NONE, RollConventions.EOM, RollConventions.IMM, RollConventions.IMMCAD,
      RollConventions.IMMAUD, RollConventions.IMMNZD, RollConventions.SFE, RollConventions.TBILL,
      RollConventions.DAY_1, RollConventions.DAY_2, RollConventions.DAY_3, RollConventions.DAY_4,
      RollConventions.DAY_5, RollConventions.DAY_6, RollConventions.DAY_7, RollConventions.DAY_8,
      RollConventions.DAY_9, RollConventions.DAY_10, RollConventions.DAY_11,
      RollConventions.DAY_12, RollConventions.DAY_13, RollConventions.DAY_14,
      RollConventions.DAY_15, RollConventions.DAY_16, RollConventions.DAY_17,
      RollConventions.DAY_18, RollConventions.DAY_19, RollConventions.DAY_20,
      RollConventions.DAY_21, RollConventions.DAY_22, RollConventions.DAY_23,
      RollConventions.DAY_24, RollConventions.DAY_25, RollConventions.DAY_26,
      RollConventions.DAY_27, RollConventions.DAY_28, RollConventions.DAY_29,
      RollConventions.DAY_30, RollConventions.DAY_MON, RollConventions.DAY_TUE,
      RollConventions.DAY_WED, RollConventions.DAY_THU, RollConventions.DAY_FRI,
      RollConventions.DAY_SAT, RollConventions.DAY_SUN)

  /**
   * The members of `StubConvention`, named rather than discovered, '''in declaration order'''.
   *
   * The order matters here and nowhere else among the constant holders: this is the one captured
   * group the capture did not sort, so the comparison is of sequences and this list is written in
   * declaration order - initial stubs, then final stubs, then both - rather than by name.
   */
  val StubConventionConstants: Vector[StubConvention] =
    Vector(
      StubConvention.NONE, StubConvention.SHORT_INITIAL, StubConvention.LONG_INITIAL,
      StubConvention.SMART_INITIAL, StubConvention.SHORT_FINAL, StubConvention.LONG_FINAL,
      StubConvention.SMART_FINAL, StubConvention.BOTH)

  /**
   * The constants of `FloatingRateNames`, named rather than discovered.
   *
   * Forty-one constants selected from the three hundred and fifty-one published names: a published
   * name is any spelling a counterparty may write, a constant one a caller writes in code.
   */
  val FloatingRateNameConstants: Vector[FloatingRateName] =
    Vector(
      FloatingRateNames.GBP_LIBOR, FloatingRateNames.USD_LIBOR, FloatingRateNames.USD_BSBY,
      FloatingRateNames.CHF_LIBOR, FloatingRateNames.EUR_LIBOR, FloatingRateNames.JPY_LIBOR,
      FloatingRateNames.EUR_EURIBOR, FloatingRateNames.AUD_BBSW, FloatingRateNames.CAD_CDOR,
      FloatingRateNames.CZK_PRIBOR, FloatingRateNames.DKK_CIBOR, FloatingRateNames.HUF_BUBOR,
      FloatingRateNames.MXN_TIIE, FloatingRateNames.NOK_NIBOR, FloatingRateNames.NZD_BKBM,
      FloatingRateNames.PLN_WIBOR, FloatingRateNames.SEK_STIBOR, FloatingRateNames.ZAR_JIBAR,
      FloatingRateNames.GBP_SONIA, FloatingRateNames.USD_FED_FUND, FloatingRateNames.USD_SOFR,
      FloatingRateNames.CHF_SARON, FloatingRateNames.CHF_TOIS, FloatingRateNames.EUR_EONIA,
      FloatingRateNames.EUR_ESTR, FloatingRateNames.EUR_ESTER, FloatingRateNames.JPY_TONAR,
      FloatingRateNames.AUD_AONIA, FloatingRateNames.BRL_CDI, FloatingRateNames.CAD_CORRA,
      FloatingRateNames.DKK_TNR, FloatingRateNames.NOK_NOWA, FloatingRateNames.PLN_POLONIA,
      FloatingRateNames.PLN_POLSTR, FloatingRateNames.SEK_SIOR, FloatingRateNames.THB_THOR,
      FloatingRateNames.USD_FED_FUND_AVG, FloatingRateNames.GB_RPI, FloatingRateNames.EU_EXT_CPI,
      FloatingRateNames.US_CPI_U, FloatingRateNames.FR_EXT_CPI)
}
