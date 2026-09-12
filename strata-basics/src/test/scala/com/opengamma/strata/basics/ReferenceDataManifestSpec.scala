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
 * ===A key nobody reads is a column nobody compares===
 *
 * A derived JSON decoder reads the fields its model declares and ignores every other key of the
 * object it is given, which for a transcription guard is the one failure mode that cannot be seen
 * from the outside: a field the capture starts emitting inside a table or a row - a new column, a
 * renamed one - would be dropped in silence, every table would still equal the manifest, and this
 * suite would report a pass over data it was no longer comparing in full. So every fixed-shape
 * object of the document is checked against its declared key set '''before''' it is decoded, in
 * the one place where the keys are still visible, through the same
 * [[com.opengamma.strata.basics.parity.ParityHarness.strictObject]] the parity suites beside this
 * file decode their fixtures with - one strictness rule for the whole module, with one message.
 * Each shape's key set is declared as a [[com.opengamma.strata.basics.parity.KeySchema]] beside
 * the model it describes, and `every declared object schema is the key set the committed manifest
 * carries` holds those declarations against the document itself, so a schema cannot drift from the
 * artefact it describes any more than a count can.
 *
 * The shapes whose keys are '''data''' are not records and are not treated as any: a row of an
 * index table is keyed by that table's own column headers, and the families of `externalNames`,
 * `lenientPatterns`, `alternateNames` and `floatingRateNames.sections` are keyed by the name of
 * the family. Each of those key sets is asserted where it belongs - against the table's header
 * list, or against the documented family set - by the count and coverage tests above.
 *
 * ===What is covered===
 *
 * '''All twenty nine top-level keys''', and every nested shape of each, are asserted against the
 * port: there is no sampled table, no key read only for its count and no exemption. The port
 * reached that point in slices, and while it was being built this suite carried a declaration of
 * the keys whose data object had not landed yet; [[ReferenceDataManifestSpec.PendingKeys]] and
 * [[ReferenceDataManifestSpec.PendingNestedShapes]] are now empty and the machinery that read them
 * is kept, because it is what will report the next gap rather than let it pass.
 *
 * The declaration is not a comment that can rot. `every top-level key of the manifest is either
 * asserted against the port or declared pending` compares the union of the covered set and the
 * pending set with the keys the document actually carries, and fails on a key that belongs to
 * neither; `every nested shape of a partially covered key is either asserted or declared pending`
 * does the same one level down for the three keys whose content is divided by family. A key or a
 * shape added to the manifest by a later capture, or one left declared pending after its data
 * object lands, therefore fails this suite instead of quietly going unchecked.
 *
 * Counts are asserted twice over, and the second is the one that matters. Every table's declared
 * count is reconciled with the length of the list it describes, which catches a truncated capture;
 * and `every table of the manifest carries the row count the schema of record documents` compares
 * each one with the '''literal''' figure section 7 of `tools/parity-capture/README.md` publishes,
 * which is what catches a re-capture that shrank a table and a port that shrank with it. A pair of
 * internally consistent tables that have both lost the same row would pass every other assertion
 * in this file.
 *
 * ===How the document is read===
 *
 * Once, for the whole suite, on first use, and without an effect type. The read is a bounded, total
 * class path lookup - `getResourceAsStream` under [[scala.util.Using]], capped at
 * [[ReferenceDataManifestSpec.ManifestByteCeiling]] bytes and decoded as UTF-8 explicitly - whose
 * result is an `Either` carrying either the document or the reason it could not be obtained. The
 * text is parsed and each key decoded into the typed view the tests assert from, so no test
 * re-reads or re-parses a quarter of a megabyte of JSON. The top level is kept as a
 * [[io.circe.JsonObject]] rather than decoded into a case class of twenty nine fields, because the
 * key set itself is something this suite asserts - at the top level and recursively - and a case
 * class would silently ignore a key it had no field for.
 *
 * No effect type appears here, deliberately. AAP section 0.3.3 confines the effect monad of the
 * port to `collect.io.Resources`, the parity harness and `BasicsDemoApp`, and this suite is none of
 * the three; discharging an effect in it would add a boundary outside that posture, which is what
 * the Rule 7 gate exists to prevent. Reading one committed resource is not an effect this suite
 * needs to sequence: it needs the bytes, and it needs the failure to be reportable.
 *
 * Reportable is the second half of it. Nothing is read while this class or its companion
 * initialises - the document sits behind a `lazy val` - so a resource that is absent, empty,
 * unparseable or not an object fails `the manifest resource is present on the class path, is
 * non-empty and parses as a JSON object` with the reason, instead of aborting the suite with an
 * `ExceptionInInitializerError` before any test can report anything at all.
 *
 * ===Scope===
 *
 * This suite asserts '''data fidelity''', and beyond it only the THBA invariants recorded below.
 * That a family is closed and that every name round trips is `NamedEnumClosedSpec`; the behaviour
 * built on these tables - the conventional pair decision, the lenient rewrite chain, calendar
 * resolution - belongs to the spec of the type that owns it. Numerical parity is the business
 * of the `parity` package. None of that is repeated here.
 *
 * The one addition is the section `The THBA table's own invariants` below. The Thai bank
 * calendar is the only calendar of the port whose dates are published rather than derived from
 * rules, and [[com.opengamma.strata.basics.date.HolidayCalendarData]] is the only data object
 * whose subject has no spec of its own: the AAP's frozen test inventory (section 0.3.1) gives
 * the `date` package twenty-two specs, one per retained Java test class, and the Java
 * implementation's only test over this table tested the INI parser this port does not have. So
 * the invariants the manifest comparison cannot state - that every row is non-empty, ascending
 * and free of duplicates, that every month-day is valid for the year it is filed under, that the
 * one published holiday falling at a weekend is kept rather than filtered, and that this table
 * is the whole content of the built-in `THBA` calendar - are asserted here, next to the
 * comparison that establishes the rows themselves. The transcription of the rows is not restated
 * by them; that is the manifest's business, and the case the section sits beside is what does it.
 */
final class ReferenceDataManifestSpec extends AnyFunSuite with Matchers {

  import ReferenceDataManifestSpec._

  //-------------------------------------------------------------------------
  // Document identity and coverage.
  //-------------------------------------------------------------------------

  test("the manifest resource is present on the class path, is non-empty and parses as a JSON object") {
    // The load of the document is a test rather than an initialiser. Every other test here reads
    // the document, so a resource that is absent, truncated to nothing, not JSON or JSON that is
    // not an object would fail all of them with the same message and no test would be reporting the
    // thing that is actually wrong; read eagerly while the companion initialised, as this file once
    // did, it would abort the suite before any test reported at all. The four failures are
    // distinguished by the diagnostic, which names the resource and the script that writes it,
    // because the next step differs: a resource absent from the class path is a build or a path
    // problem, and a document that does not parse is a capture problem.
    withClue(documentOrFailure.swap.getOrElse("the manifest loaded and is a JSON object")) {
      documentOrFailure.isRight shouldBe true
    }
    // Reading the loaded document proves the accessor the rest of the suite goes through, and its
    // key count is the shape every coverage assertion below is stated over.
    documentKeys should not be empty
    // The four refusals of the load, asserted over input chosen for the purpose. Reading the
    // committed resource exercises only the success path, so without these the guards that stand
    // between a corrupt resource and a comparison stated over it would never have been observed
    // to refuse anything.
    withClue("strict UTF-8 decoding: ") {
      // A lead byte of a two-byte sequence with its continuation byte missing. `new String(bytes,
      // UTF_8)` answers "\uFFFD" for this; a strict decoder refuses it, which is what keeps a
      // corrupted capture from reaching the JSON parser as text that differs from the bytes.
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
    // Four keys divide their content by family rather than carrying one table, so coverage is
    // stated one level deeper for them: a shape added, renamed or left behind inside one of them is
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
    // The registry the port replaces was extensible at run time: every configuration file carried a
    // `[providers]` section naming the classes that contributed members, and the override chain of
    // base, library and application files let a host add to any family. None of that is ported, so
    // none of it belongs in the document either - a `providers` key would mean the capture had
    // recorded a mechanism the port deliberately does not have, and the first thing a reader would
    // do with it is look for the Scala counterpart that cannot exist.
    //
    // The scan is recursive rather than over the top level, because a provider declaration would
    // arrive inside the family it belongs to. It reads key names only: a captured *value* may well
    // mention a provider, and a row of the Ibor index table is keyed by a CSV column header, so the
    // match is on the key text and is case-insensitive to catch any spelling of it.
    withClue(s"manifest resource '$ManifestResource' carries ${documentKeyNames.size} distinct " +
      "key names: ") {
      documentKeyNames.filter(_.toLowerCase(Locale.ENGLISH).contains("provider")) shouldBe empty
    }
  }

  test("every table of the manifest carries the row count the schema of record documents") {
    // The count assertions of this suite come in two kinds and this is the one that cannot be
    // satisfied by a self-consistent document. Everywhere else a declared count is reconciled with
    // the length of the list it describes and the list is compared with the port, which catches a
    // truncated capture and a mistranscribed row; it does not catch a table that lost a row on both
    // sides at once, because two tables that agree with each other agree just as well one row
    // short. The figures below are the literals section 7 of `tools/parity-capture/README.md`
    // publishes, transcribed from the document rather than derived from it, so that the size of
    // every table is pinned to something outside both the manifest and the port.
    //
    // Each figure is asserted against the declared count *and* against the length of the list, so a
    // document whose count field disagrees with its own rows fails here too.
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
          // Every row of an index table has to carry every column: a row missing one would be
          // decoded as an absent cell and compared against whatever the port happens to hold.
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
  // These five tests are about the reading of the document rather than about the port, and they
  // are here because everything below them is only as strong as the decoding underneath it. A
  // derived decoder ignores a key its model has no field for, so without them a column the capture
  // added would be dropped in silence and every comparison in this file would still pass over the
  // columns it did read. Each test exercises every documented shape, and the specimen it works
  // from is taken out of the committed document, so a schema that has drifted from the artefact
  // fails the first of them rather than quietly admitting the wrong key set everywhere else.
  //-------------------------------------------------------------------------

  test("every declared object schema is the key set the committed manifest carries") {
    // The declaration cannot rot. Every schema is matched against a specimen of its own shape
    // taken from the document, so a key the capture starts writing inside a table or a row is
    // reported here - with the shape and the path the specimen came from - and not merely wherever
    // that shape next happens to be decoded. Every schema is uniform, which is asserted rather
    // than assumed: the captured document holds one key set per shape, so a documented shape is
    // exact equality of key sets, with no variant to choose between and no optional key.
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
    // The acceptance half of the strictness tests, and the reason the refusals below are evidence
    // of anything: a check that refused every object would pass the three tests after this one
    // while making the suite unable to read its own document.
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
    // The case the finding this layer answers describes: a later capture adds a field to a table
    // or a row, the model has no counterpart for it, and the derived decoder would read the rest
    // of the object and report a clean row - leaving whatever the new key carried uncompared.
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
    // Every documented key of every shape, one at a time, because a key set is only as enforced as
    // its least enforced member: a model whose field was made optional, or a schema that named a
    // key the objects do not have to carry, would leave exactly one of these unrefused.
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
    // A rename is the perturbation a key-set check has to catch in both directions at once, and it
    // is the one a decoder of optional fields cannot catch at all: the old key is gone, so the
    // field reads as absent, and the new key is unknown, so its value is dropped. The message has
    // to name the spelling that arrived as well as the one that left, since a reader given only
    // half of that learns that the document changed and not into what.
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
   * The decoder under test is the one this suite reads that shape with, not a copy of it, so a
   * shape whose strictness was removed fails here rather than being tested in a form nothing uses.
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
  // The four published index tables.
  //-------------------------------------------------------------------------

  test("the captured cell normalisations are the Java forms, transcribed rather than looked up") {
    // The two index tables are compared through `dayCountCell` and `calendarCell`, which bring a
    // captured cell to the form the Java parser held it in. Neither may call the production type
    // whose behaviour the comparison judges - see the note in the Ibor index test - so each is
    // transcribed from the Java source instead, and a transcription is only worth what a test of it
    // is worth: a helper that quietly became the identity function would retire the normalisation
    // and leave the two index tests reporting normalisations as differences, or worse, agreeing for
    // the wrong reason. The expectations below are literals read off the two Java sources named in
    // the scaladoc of the helpers, chosen to cover every branch of each.
    withClue(s"manifest key '$DayCountsKey' yields ${CapturedDayCountNames.size} spellings: ") {
      // Twenty-one canonical names and the ten upper case forms that differ from them.
      CapturedDayCountNames.size shouldBe 31
      nameGroup(DayCountsKey).names.foreach { name =>
        dayCountCell(name) shouldBe name
        ()
      }
      // The Czech koruna spelling of the fixed leg day count, and the spelling of every other row.
      dayCountCell("ACT/360") shouldBe "Act/360"
      dayCountCell("Act/360") shouldBe "Act/360"
      dayCountCell("ACT/ACT ISDA") shouldBe "Act/Act ISDA"
      // A day count that carries a calendar has no constant and so no captured spelling; it is
      // passed through, which is the text the port's member renders. So is text that names nothing.
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
      // `~` is tested before `+`, and the parts of a link are normalised by the same rule, so the
      // combination inside this link is sorted before the link itself is.
      calendarCell("GBLO~USNY+EUTA") shouldBe "EUTA+USNY~GBLO"
    }
  }

  test("IborIndexData.rows equals the manifest Ibor indices, column by column and in order") {
    // Shape: {count, headers[], rows[{<header>: value}]}, every value the raw text of the CSV cell
    // the Java loader read. This is the largest table of the port - 271 rows of thirteen columns -
    // and it is compared cell by cell rather than row by row, so a failure names the row, the index
    // and the column rather than printing two thirteen-element rows and leaving the reader to find
    // the difference.
    //
    // Three kinds of column are normalised before the comparison, and each normalisation is the
    // one the Java parser itself performed on the column:
    //
    //  - A day count column is mapped through the spellings the captured `dayCounts` names imply,
    //    because the fixed leg day count is spelled `ACT/360` on the Czech rows and `Act/360` on
    //    the rest, and the registry being ported held both keys for one convention. Text no
    //    captured spelling covers is left as it stands so that the comparison reports it against
    //    the port's canonical name.
    //  - A calendar column is put through the composite normalisation the Java factory performs,
    //    because a composite name is normalised by deduplicating and sorting its parts: the column
    //    text `SGSI+GBLO` reaches the port as `GBLO+SGSI`, which is a normalisation rather than a
    //    mistranscription.
    //  - The active column is a boolean spelled in upper case here and in lower case in the price
    //    index table, and the case of a boolean literal is not data.
    //
    // None of the three consults a production type, and that is deliberate rather than incidental.
    // The expected side of a comparison may not be computed by the subject the comparison judges:
    // a lookup or a normalisation applied to the captured text as well as to the port's value
    // cancels out of the comparison, so a wrong lookup would rewrite both sides identically and
    // the rows would still agree - which is exactly the self-round-trip AAP section 0.6.1 requires
    // this suite to be independent of. `dayCountCell` therefore reads an index built from the
    // manifest's own names and the registration rule of `ExtendedEnum.java:225-236`, and
    // `calendarCell` applies the normalisation transcribed from `HolidayCalendarId.java:87-120`;
    // both are local to this file and are pinned by `the captured cell normalisations are the Java
    // forms, transcribed rather than looked up`.
    //
    // Everything else is compared as text exactly as captured, including the tenor convention
    // column, which the port carries verbatim because it names a business day convention on some
    // rows and a period addition convention on others.
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
    // The same shape and the same three normalisations as the Ibor table; this table has eight
    // columns and two of them are day counts, one of which is `Bus/252 BRBD` - a day count that
    // carries a calendar and so is not one of the twenty-one standard members. No captured spelling
    // covers it, so `dayCountCell` passes it through unchanged and it is compared against the text
    // the port's member renders, which is the same text: the cell is comparable without anything
    // resolving it, and nothing here resolves it, because the family whose lookup would do so is a
    // subject of this suite rather than an authority for it.
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
    // declared - `EUTA+CHZU` - while a composite identifier is normalised by deduplicating and
    // sorting its parts, so the same column reaches the port as `CHZU+EUTA`. The captured text is
    // therefore brought to that normal form by `calendarCell` before it is compared; comparing the
    // raw text would report a normalisation as a mistranscription.
    //
    // That normalisation is transcribed from the Java factory and computed here, not obtained by
    // calling `HolidayCalendarId.of` on the captured text. The identifier's normalisation is itself
    // part of what this suite checks, and a factory applied to both sides of a comparison cancels
    // out of it: a port that sorted composite parts wrongly would rewrite the expectation in the
    // same wrong way and the rows would still be equal. The expected side of every comparison in
    // this suite is therefore built from the manifest and from code local to this file only.
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
    // Shape: {count, names[]}, the names of the public constants of the Java `IborIndices` holder,
    // emitted in name order. A constants holder is a selection from its family rather than the
    // family itself - 113 of the 271 published Ibor indices have a constant - so two things are
    // asserted: the names of the port's constants equal the captured names, and every constant is
    // the very member the family publishes under that name rather than a second instance built
    // beside it. The constants are named one by one below rather than discovered reflectively, so a
    // constant that disappeared is a compile error in this file.
    sameNamedConstants("Ibor index", IborIndexConstantsKey, IborIndexConstants.map(_.name))
    withClue("every Ibor index constant is the published member of that name: ") {
      IborIndexConstants.filterNot(index =>
        IborIndex.valueOf(index.name).contains(index)) shouldBe empty
      IborIndexConstants.toSet.subsetOf(IborIndex.values.toList.toSet) shouldBe true
    }
  }

  test("OvernightIndices' constants equal the manifest overnightIndexConstants") {
    // The captured names hold `EUR-ESTR` twice, and that is the point of comparing the names as a
    // sequence rather than as a set: the Java holder published both `EUR_ESTR` and the retired
    // `EUR_ESTER`, and both name the same index because the retired spelling is an alternate name
    // of it. The port reproduces that - its two constants are one instance - so the comparison of
    // twenty-one names holding twenty distinct ones is what proves it.
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
    // Nine constants for nine published rows, so the constants and the family coincide here and
    // both are asserted: a family member without a constant, or a constant naming a member the
    // family does not publish, is reported.
    sameNamedConstants("price index", PriceIndexConstantsKey, PriceIndexConstants.map(_.name))
    withClue("the price index constants are exactly the members of the family: ") {
      PriceIndexConstants.filterNot(index =>
        PriceIndex.valueOf(index.name).contains(index)) shouldBe empty
      PriceIndexConstants.toSet shouldBe PriceIndex.values.toList.toSet
    }
  }

  test("FxIndices' constants equal the manifest fxIndexConstants") {
    // Eight constants over sixteen published rows: the Java holder named the European Central Bank
    // and WM/Reuters fixings and left the eight Asian and Latin American ones to be reached by
    // name, which the port reproduces rather than completing.
    sameNamedConstants("FX index", FxIndexConstantsKey, FxIndexConstants.map(_.name))
    withClue("every FX index constant is the published member of that name: ") {
      FxIndexConstants.filterNot(index =>
        FxIndex.valueOf(index.name).contains(index)) shouldBe empty
      FxIndexConstants.toSet.subsetOf(FxIndex.values.toList.toSet) shouldBe true
    }
  }

  test("the index families' alternate names equal the manifest alternateNames tables") {
    // Shape: {<family>{iniRowCount, iniRows[{alternateName, standardName}], apiExpandedRowCount,
    // apiExpandedRows[]}}. Two tables are captured per family on purpose. The INI rows are what a
    // port transcribes; the runtime view additionally registers the upper-case spelling of every
    // mixed-case alternate, so that the same table serves the exact lookup and the lenient one,
    // which folds case before it looks anything up. Ten INI rows become thirteen entries for the
    // overnight family.
    //
    // The port publishes the expanded view - the transcribed table itself is private to the family,
    // as it should be - so the comparison runs the other way: the expanded map is compared with the
    // captured expanded rows, and the captured expanded rows are compared with the expansion of the
    // captured INI rows. Together those pin the transcribed table, because the expansion only ever
    // adds an upper-case key: a missing INI row cannot be reinstated by it, and a row transcribed
    // in the wrong case expands to a different set of keys.
    DocumentedAlternateNameCounts.keySet.foreach { family =>
      val table = alternateNames(family)
      val capturedIni = table.iniRows.map(row => row.alternateName -> row.standardName).toMap
      val capturedExpanded =
        table.apiExpandedRows.map(row => row.alternateName -> row.standardName).toMap
      withClue(s"manifest alternateNames.$family: ") {
        capturedIni.size shouldBe table.iniRows.size
        capturedExpanded.size shouldBe table.apiExpandedRows.size
        // An alternate spelling that resolves to nothing would be a lookup that silently fails, so
        // every canonical name a row names has to be a member of the family.
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
      // The other named families of the port declare none, and the capture recorded none for them;
      // an alternate table appearing on one of them would be an addition to the reference data.
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

  test("FloatingRateNames' constants equal the manifest floatingRateNames constants") {
    // Shape: {constants{count, names[]}}, the names of the public constants of the Java
    // `FloatingRateNames` holder in name order. These forty-one are a selection from the 351 rows
    // of the four name sections - the published name space is every alias a counterparty may write,
    // while a constant is one of the names a caller writes in code - so the constants are compared
    // with the captured names and each one is then required to be the member the family publishes
    // under that name, which is what ties the two together.
    sameConstantNames(
      "floating rate name",
      s"$FloatingRateNamesKey.constants",
      FloatingRateNameConstants.map(_.name),
      floatingRateNames.constants)
    withClue("every floating rate name constant is the published member of that name: ") {
      FloatingRateNameConstants.filterNot(rate =>
        FloatingRateName.valueOf(rate.name).contains(rate)) shouldBe empty
      FloatingRateNameConstants.toSet.subsetOf(FloatingRateName.values.toList.toSet) shouldBe true
      // The constants name distinct rates: unlike the overnight index holder, no two of them are
      // the same value under two spellings, which the captured names agree with.
      FloatingRateNameConstants.map(_.name).distinct.size shouldBe FloatingRateNameConstants.size
    }
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
  // The six convention families.
  //-------------------------------------------------------------------------

  test("DayCount's members equal the manifest dayCounts constants") {
    // Shape: {count, names[]}, the names of the public constants of the Java `DayCounts` holder in
    // name order. Twenty-one standard members, and the family has exactly twenty-one: `Bus/252` is
    // not among them because it carries a calendar and so is a member per calendar rather than a
    // constant, which is why the captured group holds twenty-one names and not twenty-two.
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
    // calendar. The raw table is therefore what is compared - the rows as the reference data
    // supplied them, unresolved - which is exactly how the port holds them and how the
    // implementation being ported used them.
    sameExternalNameGroups(DayCountFamily, DayCount.namedEnum.externalNameGroups, group =>
      DayCount.namedEnum.externalNamesRaw(group))
  }

  test("DayCount's lenient patterns equal the manifest rows, in file order") {
    // Sixty-seven rows, the longest lenient table of the reference data, and the order is
    // behaviour: the rewrite is sequential, so `ACTUAL/ACTUAL` is rewritten by the first row and
    // the result offered to the second. Comparing these as a set would accept a reordering that
    // silently changes what a given piece of text resolves to.
    sameLenientPatterns(DayCountFamily, DayCount.namedEnum.lenientPatterns)
  }

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

  test("RollConvention's members equal the manifest rollConventions constants") {
    // Forty-five names: the eight standard conventions and the thirty-seven day-of-month and
    // day-of-week members the Java implementation pre-built. All forty-five have a constant in the
    // holder, so the constants and the family coincide and both directions are asserted - a member
    // without a constant and a constant naming no member are each reported.
    val expected = nameGroup(RollConventionsKey)
    withClue(s"manifest key '$RollConventionsKey': ") {
      expected.count shouldBe expected.names.size
      RollConvention.values.toList.map(_.name).sorted shouldBe expected.names.toList
      RollConventionConstants.map(_.name).sorted shouldBe expected.names
      RollConventionConstants.toSet shouldBe RollConvention.values.toList.toSet
      // `Day31` is deliberately absent: the thirty-first of the month is the end of it, so the
      // reference data maps that text onto `EOM` rather than publishing a member for it.
      expected.names should not contain "Day31"
    }
  }

  test("RollConvention's external name groups equal the manifest externalNames rows") {
    // Forty-four rows in one group, which is the largest external table of the reference data: the
    // numbers one to thirty-one, the seven day-of-week abbreviations and the six standard
    // conventions that FpML names. The row `31 -> EOM` is the same substitution the lenient table
    // makes and is asserted here as data rather than as behaviour.
    sameExternalNameGroups(
      RollConventionFamily,
      RollConvention.namedEnum.externalNameGroups,
      group => RollConvention.namedEnum.externalNamesRaw(group))
  }

  test("RollConvention's lenient patterns equal the manifest rows, in file order") {
    // Eleven rows whose order is load-bearing in a way no other lenient table's is: `(Day_?)?31`
    // has to be tried before `(Day_?)?([1-2]?[0-9])`, and `(Day_?)?30` before it too, or `31` would
    // be rewritten to `Day3` by the third row. Comparing the rows as a sequence is what pins that.
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

  test("StubConvention's members equal the manifest stubConventions, in declaration order") {
    // The one named-constant group the capture did '''not''' sort. The Java type is an enumeration
    // rather than a registry-backed family, so the capture read its declared values and emitted
    // them in that order - initial stubs, then final stubs, then both - and that order is the one
    // the port declares its members in. It is compared as a sequence, which asserts the order as
    // well as the membership; the sorted comparison the other five families use would pass on any
    // permutation of it.
    val expected = nameGroup(StubConventionsKey)
    withClue(s"manifest key '$StubConventionsKey': ") {
      expected.count shouldBe expected.names.size
      StubConvention.values.toList.map(_.name) shouldBe expected.names.toList
      StubConventionConstants.map(_.name) shouldBe expected.names
      StubConventionConstants.toSet shouldBe StubConvention.values.toList.toSet
      // The reference data declared no external, lenient or alternate table for an enumeration -
      // the name helper it used accepted the constant identifier and case-folded spellings, which
      // is the port's alternate table and is a property of the type rather than captured data - so
      // the capture recorded none and none is expected here.
      externalNames.keySet should not contain StubConventionFamily
      lenientPatterns.keySet should not contain StubConventionFamily
      alternateNames.keySet should not contain StubConventionFamily
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
  // The THBA table's own invariants.
  //
  // The rows themselves are established by the manifest comparison above, and nothing here
  // restates them. What follows is what a row-for-row comparison cannot state and a consumer of
  // the table nevertheless relies on - the shape of every row, the resolution of a month-day
  // against the year it is filed under, the weekend, and the fact that this table is the whole
  // content of the built-in calendar assembled from it. It is asserted in this suite because the
  // AAP's frozen test inventory (section 0.3.1) gives the `date` package one spec per retained
  // Java test class and the only Java test over this data tested the INI parser the port does not
  // have, so the table's subject has no spec of its own; the reasoning behind folding it here is
  // recorded in the scaladoc of this suite.
  //-------------------------------------------------------------------------

  test("every THBA year row is non-empty, strictly ascending and free of duplicates") {
    HolidayCalendarData.thba.foreach {
      case (year, monthDays) =>
        withClue(s"$year: ") {
          monthDays should not be empty
          monthDays.distinct.size shouldBe monthDays.size
          // Strictly ascending within the row, which is the order the captured section
          // publishes and the reason the resolved dates need no sorting on the way out.
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

    // Strictly ascending overall, which follows from the map being sorted by year and every row
    // being ascending - and is asserted directly, because it is the property a consumer relies on
    // when it builds a calendar from the list without sorting it.
    HolidayCalendarData.thbaHolidays.sliding(2).foreach {
      case List(earlier, later) =>
        withClue(s"$earlier then $later: ")(earlier.isBefore(later) shouldBe true)
      case _ => succeed
    }

    // Consistent with the table it is derived from: the same number of dates, each one the
    // month-day of its row resolved against the year that row is filed under, in that order. The
    // years the resolved dates fall in are compared with the years the capture recorded, so the
    // accessor is held to the same rows as the table.
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
    // A month-day is resolved against its year on the way out, and that resolution '''adjusts'''
    // the 29th of February in a year that is not a leap year rather than failing. One published
    // row holds a leap day - 2056 - so this assertion is what distinguishes correct data from a
    // leap day filed under the wrong year, which would otherwise become the 28th in silence.
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
    // Asserted as an exhaustive scan rather than as a single date, so that filtering the table - or
    // adding a second such date - fails here.
    val weekendHolidays: List[LocalDate] =
      HolidayCalendarData.thbaHolidays.filter(date =>
        HolidayCalendarData.thbaWeekendDays.contains(date.getDayOfWeek))

    weekendHolidays shouldBe List(LocalDate.of(2031, 5, 4))
    LocalDate.of(2031, 5, 4).getDayOfWeek shouldBe DayOfWeek.SUNDAY

    // The captured weekend and the port's are compared by the manifest case above; the two days
    // are named here as well, because outside the published years the calendar applies its
    // weekend alone and nothing else in this suite states which days those are.
    HolidayCalendarData.thbaWeekendDays shouldBe Set(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY)
  }

  test("HolidayCalendarData.thba is the data the built-in THBA calendar is assembled from") {
    // The purpose of this table: it is the whole content of the built-in Thai bank calendar, so
    // every published date is a holiday of that calendar and the weekend it declares is the one
    // the calendar applies. `StandardHolidayCalendars` is the assembler - the data object never
    // builds a calendar itself, which is what keeps the two free of an initialisation cycle.
    val calendar = StandardHolidayCalendars.THBA
    calendar.id shouldBe HolidayCalendarIds.THBA
    calendar.weekendDays shouldBe HolidayCalendarData.thbaWeekendDays

    HolidayCalendarData.thbaHolidays.foreach { date =>
      withClue(s"$date: ")(calendar.isHoliday(date) shouldBe true)
    }

    // And a year outside the published range is answered by the weekend rule alone, which is the
    // documented fallback of a calendar asked about a date its data does not cover.
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

  /**
   * Asserts two tables of rendered cells are equal, reporting the '''first''' difference with the
   * row it sits in, the name that row carries and the column it belongs to.
   *
   * This is [[sameRows]] one dimension further in, for the two tables wide enough that a row-level
   * failure would be unreadable: a thirteen-column row printed twice tells the reader that
   * something differs and nothing about what. Every cell is a rendered string, both sides
   * normalised the same way by the callers, and the first cell of each row is taken as its name for
   * the message - which holds for both index tables, whose first column is `Name`.
   *
   * The column count of every row is checked against the header list before any cell is read, so a
   * short row is reported as such rather than as a missing cell.
   *
   * @param what  how a row of this table is named in the failure message
   * @param headers  the column names, in the order the cells are rendered in
   * @param actual  the rendered rows the port holds
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
   * A map has no order, so there is no row index to report and a plain comparison prints both maps
   * in full and leaves the reader to find the entry that differs - over forty-four external
   * spellings that is a failure nobody reads. The two differences are computed and named instead:
   * the entries the manifest has that the port does not hold with that value, and the entries the
   * port declares that the manifest does not. An entry present on both sides with different values
   * appears in both lists, which is what identifies it.
   *
   * @param what  how this table is named in the failure message
   * @param actual  the table the port holds
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
   * Asserts the named constants of the port equal a captured constant group, in name order.
   *
   * The captured group's own count is reconciled with the length of its name list first, so that a
   * document disagreeing with itself is reported as that rather than as a difference against the
   * port. The names are then compared as a '''sequence''' of sorted names rather than as a set,
   * because a repeated name is data: the overnight index holder publishes two constants for one
   * index, and a set comparison would accept a port that published only one of them.
   *
   * @param what  how a constant of this family is named in the failure message
   * @param where  how the captured group is named in the failure message
   * @param actual  the names of the constants the port declares
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
   * Asserts the named constants of the port equal the constant group under a top-level key.
   *
   * @param what  how a constant of this family is named in the failure message
   * @param key  the top-level key of the document carrying the group
   * @param actual  the names of the constants the port declares
   */
  private def sameNamedConstants(what: String, key: String, actual: Vector[String]): Unit =
    sameConstantNames(what, s"key '$key'", actual, nameGroup(key))

  /**
   * Asserts the external spelling groups of one family equal the captured rows.
   *
   * The comparison is against the '''raw''' table - the rows as the reference data supplied them,
   * external spelling to canonical name as text - and not against the resolved view, for two
   * reasons. A row may name a member the family does not publish, which the resolved view drops and
   * the transcription must keep; and resolving both sides through the same lookup would let a
   * mistranscribed canonical name agree with itself.
   *
   * @param family  the family name the document keys the groups under
   * @param groups  the group names the port publishes for the family
   * @param rawOf  the port's raw table for a group, by group name
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
   * tried - so the order of the rows is behaviour and the comparison is of sequences. The
   * expression is compared through its source text, which is what the reference data held and what
   * the port transcribes; the compiled expressions are copies made insensitive to case, so
   * comparing those would compare a derivation instead of the data.
   *
   * @param family  the family name the document keys the table under
   * @param patterns  the lenient rewrites the port declares, in the order they are applied
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
 * '''Nothing here runs while this object initialises.''' The resource is read on first use, behind a
 * `lazy val`, and the read answers with an `Either` rather than throwing, so a resource that is
 * missing, empty, unparseable or not an object is reported by the test whose subject that is and
 * through the diagnostic of every accessor that cannot proceed without it - never as an
 * `ExceptionInInitializerError` that aborts the suite before it reports anything. Each key is then
 * decoded into the typed view the tests read, and those views are lazy for the same reason:
 * decoding still happens once, but it happens inside the test that reads the view, so a document
 * whose shape has changed under one key fails the tests of that key, naming the key and the JSON
 * path, and leaves the remaining twenty-eight still asserted.
 *
 * Members are declared in dependency order - the key names first, then the document, then the typed
 * views derived from both - which is how the file reads even though everything derived from the
 * document being lazy means the order no longer decides whether one of them sees another as `null`.
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
   * The manifest comparison establishes that the port holds the rows the capture recorded; these are
   * the published totals of the section those rows were captured from
   * (`HolidayCalendarData.ini:32-106`), and they are what reports a row that lost or gained a date
   * en bloc on both sides of that comparison at once.
   */
  val DocumentedThbaDateCount: Int = 1220
  val DocumentedThbaShortestYearRowSize: Int = 13
  val DocumentedThbaLongestYearRowSize: Int = 19

  /**
   * The documented size of every named-constant group of the document.
   *
   * Eleven of the twenty nine keys are `{count, names[]}` groups, and every one of them is here:
   * the four index constant holders, the six convention families and the holiday calendar
   * identifiers, plus the built-in calendar set. The figures are those section 7 of
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
   * other named families of the port publish none, which the tests of those families assert
   * directly rather than through this table.
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
  // What this suite covers. Nothing is exempt, and the declaration is asserted, not asserted of.
  //-------------------------------------------------------------------------

  /**
   * The keys asserted against the port by the tests of this suite, which is all of them.
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
   * Empty, because every table of the manifest now has one. The map is kept rather than deleted,
   * because it is the mechanism by which the '''next''' gap is reported: the port was built in
   * slices, and while the index tables, the day count, roll convention and stub convention families
   * and the index alternate names were outstanding this map named the file each was waiting for and
   * the coverage test held that declaration against the document. A later capture that adds a key
   * this suite has no comparison for is declared here for exactly as long as that is true, and
   * appears in the failure message of the coverage test until it is not - which is the opposite of
   * a key silently going unchecked.
   *
   * The value of an entry is the path of the file that will carry the port's counterpart, so a
   * reader of the failure learns where the work is and not merely that work remains.
   */
  val PendingKeys: Map[String, String] = Map.empty

  /**
   * The nested shapes of a covered key that have no Scala counterpart yet, keyed `<key>.<shape>`.
   *
   * Empty, for the same reason and with the same purpose as [[PendingKeys]]: the day count and roll
   * convention families own their external name groups and their lenient patterns, and those four
   * shapes were declared here until those two files landed.
   */
  val PendingNestedShapes: Map[String, String] = Map.empty

  /**
   * The covered nested shapes of each key whose content is divided by family.
   *
   * Coverage is stated one level deeper for these four because a shape of one of them is a separate
   * table with a separate owner in the port - `lenientPatterns.DayCount` is transcribed in
   * `date/DayCount.scala` and `lenientPatterns.RollConvention` in `schedule/RollConvention.scala` -
   * so "the key is covered" is not a statement about any one of them. The nested coverage test
   * compares these sets with the shapes the document actually carries, which is what reports a
   * family added to, renamed in or dropped from the capture.
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
  // the capture writes, and every key set here is `uniform` - one documented shape, no optional
  // key - because the captured manifest is a uniform document: a table carries the same keys in
  // every one of its rows, present rather than omitted. A uniform schema is therefore exact
  // equality of key sets, and `ParityHarness.strictObject` applies it to the object before the
  // derived decoder reads it, so an unknown, a missing or a renamed key is refused with the key
  // named rather than ignored. Each schema's key set is held against the committed document by
  // `every declared object schema is the key set the committed manifest carries`, so none of them
  // can drift from the artefact it describes.
  //-------------------------------------------------------------------------

  /** One currency row: `{code, minorUnitDigits, triangulationCurrency, historic}`. */
  final case class CurrencyManifestRow(
      code: String,
      minorUnitDigits: Int,
      triangulationCurrency: String,
      historic: Boolean)

  /** The key set of one row of `currencies.rows`. */
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

  /** The key set of the `currencies` table. */
  val CurrencyTableSchema: KeySchema =
    KeySchema.uniform(
      "manifest currency table",
      Set("count", "historicCount", "activeCount", "rows"))

  /** One conventional currency pair: `{pair, rateDigits}`. */
  final case class CurrencyPairManifestRow(pair: String, rateDigits: Int)

  /** The key set of one row of `currencyPairs.rows`. */
  val CurrencyPairRowSchema: KeySchema =
    KeySchema.uniform("manifest currency pair row", Set("pair", "rateDigits"))

  /** The currency pair table: `{count, rows[]}`. */
  final case class CurrencyPairManifestTable(count: Int, rows: Vector[CurrencyPairManifestRow])

  /** The key set of the `currencyPairs` table. */
  val CurrencyPairTableSchema: KeySchema =
    KeySchema.uniform("manifest currency pair table", Set("count", "rows"))

  /** One country row: `{alpha3, alpha2}`. */
  final case class CountryManifestRow(alpha3: String, alpha2: String)

  /** The key set of one row of `countries.rows`. */
  val CountryRowSchema: KeySchema =
    KeySchema.uniform("manifest country row", Set("alpha3", "alpha2"))

  /** The country table: `{count, rows[]}`. */
  final case class CountryManifestTable(count: Int, rows: Vector[CountryManifestRow])

  /** The key set of the `countries` table. */
  val CountryTableSchema: KeySchema =
    KeySchema.uniform("manifest country table", Set("count", "rows"))

  /** A named-constant group: `{count, names[]}`, the names in name order. */
  final case class NameGroup(count: Int, names: Vector[String])

  /**
   * The key set of a named-constant group.
   *
   * The shape of the twelve constant-holder keys of the document - `iborIndexConstants`,
   * `overnightIndexConstants`, `priceIndexConstants`, `fxIndexConstants`, `dayCounts`,
   * `businessDayConventions`, `rollConventions`, `periodAdditionConventions`, `dateSequences`,
   * `stubConventions`, `holidayCalendarIds` and `builtInHolidayCalendars` - and of
   * `floatingRateNames.constants`.
   */
  val NameGroupSchema: KeySchema =
    KeySchema.uniform("manifest named-constant group", Set("count", "names"))

  /** One key/value row of a configuration section: `{key, value}`, in file order. */
  final case class KeyValueRow(key: String, value: String)

  /**
   * The key set of one key/value row.
   *
   * The shape of a row of every `floatingRateNames.sections.<section>.rows` and of every
   * `lenientPatterns.<family>.rows`.
   */
  val KeyValueRowSchema: KeySchema =
    KeySchema.uniform("manifest key/value row", Set("key", "value"))

  /** One configuration section: `{count, rows[]}`. */
  final case class KeyValueSection(count: Int, rows: Vector[KeyValueRow])

  /**
   * The key set of one configuration section.
   *
   * The shape of every `floatingRateNames.sections.<section>` and of every
   * `lenientPatterns.<family>`.
   */
  val KeyValueSectionSchema: KeySchema =
    KeySchema.uniform("manifest key/value section", Set("count", "rows"))

  /** One external name row: `{externalName, standardName}`. */
  final case class ExternalNameRow(externalName: String, standardName: String)

  /** The key set of one row of `externalNames.<family>.<group>.rows`. */
  val ExternalNameRowSchema: KeySchema =
    KeySchema.uniform("manifest external name row", Set("externalName", "standardName"))

  /** One external name group: `{count, rows[]}`. */
  final case class ExternalNameGroup(count: Int, rows: Vector[ExternalNameRow])

  /** The key set of one `externalNames.<family>.<group>` group. */
  val ExternalNameGroupSchema: KeySchema =
    KeySchema.uniform("manifest external name group", Set("count", "rows"))

  /** One alternate name row: `{alternateName, standardName}`. */
  final case class AlternateNameRow(alternateName: String, standardName: String)

  /**
   * The key set of one alternate name row.
   *
   * The shape of a row of every `alternateNames.<family>.iniRows` and of every
   * `alternateNames.<family>.apiExpandedRows`, which the capture writes in the same shape.
   */
  val AlternateNameRowSchema: KeySchema =
    KeySchema.uniform("manifest alternate name row", Set("alternateName", "standardName"))

  /** The alternate names of one family, as INI rows and as the expanded runtime view. */
  final case class AlternateNameTable(
      iniRowCount: Int,
      iniRows: Vector[AlternateNameRow],
      apiExpandedRowCount: Int,
      apiExpandedRows: Vector[AlternateNameRow])

  /** The key set of one `alternateNames.<family>` table. */
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
   * The key set of an index table.
   *
   * The shape of `iborIndices`, `overnightIndices`, `priceIndices` and `fxIndices`. The schema
   * governs the three keys of the table itself and stops there: a '''row''' of one of these tables
   * is keyed by the table's own column headers, so its keys are data rather than a record shape,
   * and they are asserted against `headers` - for every row of every index table - by `every table
   * of the manifest carries the row count the schema of record documents`. Turning a row into a
   * fixed record would pin four wide tables to one spelling of thirteen, eight, five and six
   * column names apiece and would still say nothing the header comparison does not already say.
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
   * floating rate name sections account for every transcribed row`, which is also where the
   * declared alias row count is reconciled with the sections it totals. Each section itself is a
   * fixed shape and is decoded through [[KeyValueSectionSchema]].
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

  /** The key set of one row of `holidayCalendarDefaultByCurrency.rows`. */
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

  /** The key set of the `holidayCalendarDefaultByCurrency` table. */
  val HolidayCalendarDefaultTableSchema: KeySchema =
    KeySchema.uniform(
      "manifest default calendar table",
      Set("count", "resolvableCount", "unresolvableCount", "rows"))

  /** One THBA year row: `{year, dates}`, the dates as `MMMdd` text separated by commas. */
  final case class ThbaYearRow(year: String, dates: String)

  /** The key set of one row of `holidayCalendarData.THBA.rows`. */
  val ThbaYearRowSchema: KeySchema =
    KeySchema.uniform("manifest THBA year row", Set("year", "dates"))

  /** The THBA table: `{yearRowCount, weekend, rows[]}`. */
  final case class ThbaTable(yearRowCount: Int, weekend: String, rows: Vector[ThbaYearRow])

  /**
   * The key set of one table of `holidayCalendarData`, of which the document carries one: `THBA`.
   *
   * The schema is of the table, not of the key it hangs under; the object that holds the keys has
   * a schema of its own, immediately below.
   */
  val ThbaTableSchema: KeySchema =
    KeySchema.uniform("manifest THBA table", Set("yearRowCount", "weekend", "rows"))

  /**
   * The key set of `holidayCalendarData` itself: the one calendar the capture emits table data
   * for.
   *
   * This object looks like the map-shaped parts of the manifest, whose keys are '''data''' and
   * are therefore asserted as sets rather than declared, but it is not one of them: only `THBA`
   * is read out of it and compared with the port, so a second calendar added by a later capture
   * would decode into the map, sit there unread, and leave that whole table unmeasured while
   * every other check of this suite still passed. Declaring the key set is what refuses it
   * instead - a calendar the capture starts emitting has to be read here before this suite can
   * report on the manifest again, which is precisely the intent.
   *
   * `Map[String, ThbaTable]` remains the decoded type: the name is still the key of a table, and
   * nothing about the shape of a table changes. What is declared is which keys may appear.
   */
  val HolidayCalendarDataSchema: KeySchema =
    KeySchema.uniform("manifest holiday calendar data", Set(ThbaCalendarName))

  // Declared in dependency order: a table's decoder captures the row decoder it needs while it is
  // itself initialised, so a row decoder below its table would be read as `null`. Each one is the
  // derived decoder of its model behind the key check of its schema, so the keys of an object are
  // settled before any field of it is read; wrapping does not change what a field decodes to, and
  // it leaves the order above exactly as load-bearing as it was.
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
   * The decoder of `holidayCalendarData`: a map of calendar name to table, behind the key check
   * of [[HolidayCalendarDataSchema]].
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
   * The committed document is a quarter of a megabyte, so eight megabytes is four orders of
   * magnitude of room for a table to grow in and still a ceiling: the read below is given a bound
   * so that a resource which is not the manifest at all - a truncated stream that never ends, an
   * archive that resolved under the same name - is reported as being too large rather than
   * exhausting the heap of the forked test JVM. A capture that genuinely outgrew this ceiling fails
   * the load test with a message naming the figure.
   */
  val ManifestByteCeiling: Int = 8 * 1024 * 1024

  /**
   * Reads the committed manifest resource as UTF-8 text, without an effect and without throwing.
   *
   * '''Why this is not the module's effectful reader.''' AAP section 0.3.3 confines the effect monad
   * of the port to `collect.io.Resources`, the parity harness and `BasicsDemoApp`, and this suite is
   * none of the three: discharging that reader's effect here would add a boundary the Rule 7 posture
   * does not have, and discharging it in an eagerly initialised `val`, as this file once did, turned
   * a missing or malformed resource into an `ExceptionInInitializerError` that aborted the suite
   * before a single test could report anything. Reading one committed class path resource needs no
   * effect type at all: it is done here with the class loader, bounded, total, and decoded
   * explicitly, and the outcome is a value that the load test asserts and every other accessor
   * reports through.
   *
   * '''The alternative, and why it is not taken here.''' The other way to keep this suite free of an
   * effect boundary is for the read to happen in one of the three places that may hold one - the
   * natural candidate being the parity harness beside this file, which already loads classpath
   * fixtures through `Resources.readClasspathText` - and for an `Either` to be handed to this suite
   * already read. That is a better home for it if the module ever wants one reader and one only,
   * and it is a small change: a method there returning `Either[String, String]` and this file
   * calling it in place of [[readManifestText]]. It is not done here because the harness is not
   * this file's concern to change, and because the guarantees that make the choice matter - a
   * ceiling, a close on every outcome, and strict UTF-8 - are each stated and asserted below rather
   * than assumed.
   *
   * The read carries the same three guarantees as the reader it stands in for, because a weaker
   * read of this document would be a defect of this suite rather than a convenience: it is bounded
   * by [[ManifestByteCeiling]] - one byte beyond it is requested, so a document at the ceiling is
   * distinguishable from one over it - it is closed by [[scala.util.Using]] whether it completes or
   * fails, and it is decoded as '''strict''' UTF-8 by [[decodeUtf8]], which refuses a malformed
   * byte sequence instead of substituting a replacement character for it. That last one is the one
   * worth stating: this suite exists to prove a captured document was transcribed faithfully, so a
   * decoder that silently rewrote a byte before the JSON was parsed could turn a corrupt capture
   * into a passing comparison.
   *
   * A leading `/` on the resource name is tolerated because `ClassLoader.getResourceAsStream`
   * rejects it where `Class.getResourceAsStream` requires it, and the constant is written in the
   * class loader's form.
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
   * `new String(bytes, UTF_8)` is not this: its decoder is configured to '''replace''' a malformed
   * or unmappable sequence with `U+FFFD` and carry on, so a corrupted capture would reach the JSON
   * parser as text that differs from the bytes on disk. A decoder obtained from the charset and set
   * to [[java.nio.charset.CodingErrorAction#REPORT]] raises
   * [[java.nio.charset.CharacterCodingException]] instead, which is what the module's own class
   * path reader does with the same input and what this suite needs: the document it reads is the
   * evidence every other test in the file is stated against, so the bytes must survive the read
   * unaltered or the read must say so.
   *
   * Kept separate from [[readManifestText]], and visible to the suite, so that the guarantee is
   * asserted by a test over bytes chosen for the purpose rather than inferred from a document that
   * happens to be well formed.
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
   * Held as an `Either` rather than read into a member that throws, so that the one test whose
   * subject is the resource itself can assert the outcome and print the reason, and so that nothing
   * at all happens while this object initialises. The four distinguishable failures - the resource
   * is absent, it read as empty text, it is not JSON, it is JSON but not an object - each name the
   * resource and the script that writes it, because the useful next step differs in each case.
   *
   * The top level is kept as a [[io.circe.JsonObject]] rather than decoded into a case class of
   * twenty nine fields: the key set itself is something this suite asserts, at the top level and
   * recursively, and a case class would silently ignore a key it had no field for - precisely the
   * change those assertions exist to catch.
   */
  lazy val documentOrFailure: Either[String, JsonObject] =
    readManifestText().flatMap(documentOf)

  /**
   * Turns the text of the resource into the top level of the document, or into the reason it is not
   * one.
   *
   * Separated from the read, and visible to the suite, for the same reason [[decodeUtf8]] is: the
   * three ways a document can be text and still not be the manifest are each asserted by a test
   * over text chosen for the purpose. Reading the committed resource exercises the success path
   * only, and a guard whose failure path has never been observed is a guard that may not report
   * what it claims to.
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
   * initialiser: the accessor is reached from inside a test, so the test that reads the manifest is
   * the test that reports the manifest being unreadable. `the manifest resource is present on the
   * class path, is non-empty and parses as a JSON object` asserts the same outcome on its own, so
   * the reason is reported once by a test whose subject it is rather than repeated by all of them.
   */
  private lazy val document: JsonObject =
    documentOrFailure.fold(diagnostic => Assertions.fail(diagnostic), identity)

  /** The top-level keys, in the order the capture wrote them. */
  lazy val documentKeys: Vector[String] = document.keys.toVector

  /**
   * Collects the key names of a JSON value and of everything nested inside it.
   *
   * Written as a fold over the JSON algebra rather than as a cast-and-test, so that every shape a
   * value can take is accounted for and a future shape cannot be missed: the four scalar shapes
   * contribute nothing, an array contributes the names of its elements, and an object contributes
   * its own keys and the names of its values.
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
   * Read for one purpose: to assert that no part of the document declares a runtime provider, which
   * would be a mechanism the port does not have. Key '''names''' only - the values a key holds are
   * compared by the tests that own them - and a name is recorded once however many times it occurs,
   * since the question asked of this set is whether a name is present at all.
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
  // Lazy on purpose. Decoding is done once - a `lazy val` memoises, so no test re-decodes a view -
  // but it is done inside the test that reads the view rather than while this object initialises,
  // so a section whose shape has changed fails the tests that read that section, naming the key and
  // the JSON path, and leaves every other section still asserted. Decoding all of them eagerly
  // would turn one malformed section into an initialiser error that aborts the whole suite and
  // reports nothing else. Laziness also removes the textual-order hazard of a view reading a key
  // name declared below it.
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
  // This is the inventory the strictness tests read. It exists so that the key sets declared
  // beside the row models are statements about the committed artefact rather than about what
  // somebody believed the capture wrote: every schema is matched against a real object of its
  // shape, and the accept-and-refuse tests perturb that same object rather than a hand-written
  // stand-in that could agree with the schema and disagree with the document.
  //-------------------------------------------------------------------------

  /**
   * One fixed-shape object of the manifest: its declared key set, where the document carries an
   * instance of it, and the decoder this suite reads that shape through.
   *
   * `decode` answers `Decoder.Result[Any]` because the two questions the strictness tests ask of
   * it - whether it accepted the object, and what it said when it refused - are the same for every
   * shape and neither depends on the type the shape decodes to.
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
   * The path is read left to right, each step naming a key of the object reached so far, and a
   * step suffixed `[]` meaning the '''first''' element of the array that key holds - so
   * `currencies.rows[]` is the first row of the currency table. Each step is checked as it is
   * taken and one that cannot be taken throws, naming the whole path and the step that failed: a
   * path into this document that no longer resolves is a change in the capture, and reporting it
   * as a missing specimen says so, where a decode failure somewhere else would not.
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
   * Asserted against [[StrictShapes]] so that the inventory cannot lose an entry: a shape dropped
   * from that vector would take its schema out of the drift check and its decoder out of the
   * accept-and-refuse tests while every remaining shape still passed them.
   */
  val DocumentedObjectShapeCount: Int = 20

  /** The key a later capture is imagined to have added, for the refusal tests. */
  val UncapturedKey: String = "addedByALaterCapture"

  /**
   * Every fixed-shape object of the manifest, each with a specimen of it.
   *
   * Twenty: one per row model, plus `holidayCalendarData`, whose keys name calendars but of which
   * only `THBA` is ever read - so its key set is declared rather than asserted as data, because a
   * calendar a later capture added would otherwise sit in the map unmeasured. That is every object
   * shape of the document except the two kinds whose keys are genuinely '''data''' and whose whole
   * content is asserted elsewhere, each of which is checked where it belongs instead:
   *
   *  - the top level, whose twenty-nine keys are compared with the covered and pending sets by
   *    `every top-level key of the manifest is either asserted against the port or declared
   *    pending`, and whose order is asserted by the identity test;
   *  - the maps keyed by a name - a row of an index table, keyed by that table's column headers,
   *    and the families of `externalNames`, `lenientPatterns`, `alternateNames` and
   *    `floatingRateNames.sections` - whose key sets are asserted as sets against the headers
   *    list and against the documented family sets by the count and coverage tests.
   *
   * A shape carried at more than one path is listed once, at the path of the first instance the
   * document holds of it: the shape of a key/value section does not depend on whether it is a
   * lenient pattern table or an alias section, and asserting it twice would assert nothing twice.
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
  lazy val thbaRows: Vector[(Int, List[MonthDay])] =
    holidayCalendarDataThba.rows.map(row =>
      (row.year.toInt, row.dates.split(",").toList.map(token => monthDayOf(row.year, token))))

  /** The captured THBA weekend, in the form the port holds it in. */
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
   * The column is spelled `TRUE`/`FALSE` in the Ibor and overnight index data and `true`/`false`
   * in the price index data, because the two came from files maintained by different hands; the
   * case of a boolean literal is not data, so both spellings render to the same cell. Text that is
   * neither is returned unchanged rather than rejected here, so that the comparison reports it
   * against the port's value with the row and column named - which is a better failure than an
   * exception thrown out of a cell renderer.
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

  /** Renders a boolean the port holds, in the same normal form as [[booleanCell]]. */
  def booleanCell(value: Boolean): String = if (value) "TRUE" else "FALSE"

  /**
   * The canonical day count name of every spelling the Java registry resolved, from the manifest.
   *
   * The reference data spelled a day count in two ways in the same column - `ACT/360` on the Czech
   * koruna rows and `Act/360` everywhere else - so the captured text has to be brought to one
   * spelling before it can be compared. The index that does it is built from the '''manifest's own'''
   * `dayCounts` names and from the registration rule of the registry being ported, and from nothing
   * else: [[com.opengamma.strata.basics.date.DayCount]] is a subject of this suite, so resolving a
   * captured cell through it would let a wrong lookup rewrite the expectation and the port's answer
   * in the same wrong way and still compare equal.
   *
   * The rule is transcribed from `modules/collect/src/main/java/com/opengamma/strata/collect/named/`
   * `ExtendedEnum.java:225-236`, which registers every constant twice - under its canonical name and
   * under the English upper case of that name - with the first registration of a key winning:
   *
   * {{{
   * instances.putIfAbsent(instance.getName(), instance);
   * instances.putIfAbsent(instance.getName().toUpperCase(Locale.ENGLISH), instance);
   * }}}
   *
   * The canonical names are registered here before the upper case forms, because Java iterates the
   * declared fields of the constants holder in an order the reflection API does not define: an upper
   * case form that collided with the canonical name of a '''different''' member would make the Java
   * outcome depend on that order, and the twenty-one captured names contain no such collision - ten
   * of them differ from their upper case form, giving an index of thirty-one keys. The case fold is
   * [[java.util.Locale#ENGLISH]] rather than the default locale, so the index does not depend on the
   * host that runs the suite.
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
   * The captured text is mapped through [[CapturedDayCountNames]], which is the manifest's own name
   * list read through the registration rule of the registry being ported - no production type is
   * consulted. Text the index does not hold is returned unchanged, so the comparison reports it
   * against the name the port renders: that is what happens to `Bus/252 BRBD`, a day count that
   * carries a calendar and so is not one of the twenty-one standard names, and the port renders it
   * with exactly that text.
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
   * Guava's `Splitter.on(char)` neither trims nor omits empty results, and the Java code this
   * normalisation is transcribed from uses it bare, so `split` is given a limit of `-1`: Scala's
   * one-argument `split` drops trailing empty strings, which would quietly make `GBLO+` normalise to
   * `GBLO` here and to the empty-part composite there.
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
   * of what the manifest exists to check, and putting the captured text through the port's factory
   * would report a wrong normalisation as agreement, because the same wrong normalisation would be
   * applied to the port's own name as well. The rule is therefore transcribed from
   * `modules/basics/src/main/java/com/opengamma/strata/basics/date/HolidayCalendarId.java:87-120`
   * (`of` and `create`), which is the code that produced the names the Java parser held:
   *
   *  - a name containing `~` is a linked identifier: its parts are normalised by this same rule,
   *    deduplicated, sorted by name and rejoined with `~`, and a part naming `NoHolidays` absorbs
   *    the whole identifier, because a link with a calendar that has no holidays has none either.
   *    The absorption is decided on the normalised parts, as in Java;
   *  - a name containing `+` is a combined identifier: parts whose '''raw''' text is `NoHolidays` are
   *    dropped before anything else - Java filters on the raw text, so a part that only normalises
   *    to `NoHolidays` survives the filter and is kept - and the rest are normalised, deduplicated,
   *    sorted by name and rejoined with `+`;
   *  - any other name is its own normal form.
   *
   * The sort is the plain lexicographic order of [[java.lang.String]], which is what Java's
   * `comparing(HolidayCalendarId::getName)` compares with. `~` is tested before `+` here because
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
   * `SGSI+GBLO` denotes the identifier named `GBLO+SGSI`; comparing the raw text would report that
   * normalisation as a difference. The normalisation applied here is [[normalisedCalendarName]],
   * transcribed from the Java factory rather than obtained by calling the port's, so a port that
   * normalised a composite name wrongly fails this comparison instead of surviving it.
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
   * Renders one Ibor index row of the port as cells, in [[IborIndexHeaders]] order.
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
   * Renders one overnight index row of the port as cells, in [[OvernightIndexHeaders]] order.
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
   * The rule reproduced here is the one the lookup applies and the one the implementation being
   * ported applied before it: alongside each supplied spelling the table holds that spelling folded
   * to upper case, unless the table already holds a spelling identical to it, so that an alternate
   * name is reachable both from the exact lookup and from the lenient one, which folds case before
   * it looks anything up. The supplied spellings are therefore never displaced.
   *
   * Written here so that the captured INI rows and the captured expanded rows can be held to each
   * other: the expansion only ever adds an upper-case key, so a row missing from the transcription
   * cannot be reinstated by it and a row transcribed in the wrong case expands to a different set
   * of keys.
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
   * The alternate spellings the port publishes for one index family, as the family publishes them.
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
   * The named families of the port that declare no alternate spelling, each with its table.
   *
   * The capture recorded alternate names for three families, and the reference data declared them
   * for exactly those three. Holding the remaining families here, with their tables rather than
   * with their names alone, is what lets the absence be asserted instead of assumed: a table
   * appearing on one of them would be an addition to the reference data, which the user's
   * instruction not to add a convention forbids.
   *
   * The price index family is among them, and so is every convention family of the module bar the
   * stub convention, whose alternate table is a property of an enumeration rather than captured
   * data - the name helper of the type being ported accepted the constant identifier and the
   * case-folded spellings of every member - and is asserted by the test of that family.
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

  /**
   * The constants of `IborIndices`, named rather than discovered.
   *
   * One hundred and thirteen of the two hundred and seventy-one published Ibor indices have a
   * constant: the Java holder named the benchmark families a caller writes in code and left the
   * rest to be reached by name. Naming each one here rather than reflecting over the holder is what
   * makes a constant that disappeared a compile error in this file and a constant that was added a
   * failure of the comparison against the captured names.
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
   * Eight constants over sixteen published rows: the European Central Bank and WM/Reuters fixings
   * are named and the eight Asian and Latin American ones are reached by name.
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
   * The order matters here and nowhere else among the constant holders: the captured group for this
   * family is the one the capture did not sort, because the Java type is an enumeration and the
   * capture read its declared values. The comparison is therefore of sequences, so this list is
   * written in the order the type declares its members - initial stubs, then final stubs, then
   * both - rather than in name order.
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
   * name is any spelling a counterparty may write, while a constant is one of the names a caller
   * writes in code. Unlike the overnight index holder, no two of these name the same rate.
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
