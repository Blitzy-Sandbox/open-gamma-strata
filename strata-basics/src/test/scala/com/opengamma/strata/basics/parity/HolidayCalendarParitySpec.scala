/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.parity

import java.time.LocalDate

import cats.effect.IO
import cats.effect.Ref
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all._

import io.circe.Decoder
import io.circe.Json
import io.circe.generic.semiauto.deriveDecoder

import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.date.HolidayCalendar
import com.opengamma.strata.basics.date.HolidayCalendarId

/**
 * Parity of the built-in holiday calendars against the Java baseline.
 *
 * This is the measurement that pins the calendar layer of the port: the rule-generated national
 * calendars, the published Thai calendar, the four weekend and no-holiday calendars, the
 * composite calendars, the monthly-bitmask representation they are all stored in, the weekend-only
 * fallback outside a calendar's year range, and the refusal to answer for a year outside 0000 to
 * 9999.
 *
 * The gate that consumes it (AAP section 0.10.1, Gate 3, for the user's Rule 2) runs
 *
 * {{{
 * sbt -batch "testOnly *ParitySpec"
 * }}}
 *
 * and then reads `<parity.report.dir>/holiday.json`, requiring `failed == 0`. The `ParitySpec`
 * suffix of this class, its package, the fixture stem `holiday` and the five keys of the report
 * document are therefore all part of that contract and none of them may drift.
 *
 * ===Everything here compares exactly===
 *
 * Every expectation of this fixture is a date, a list of dates, a boolean or an integer, so every
 * comparison is exact and goes through [[ParityHarness.assertExact]]. No tolerance is used
 * anywhere in this file, and `assertParity` - the harness's numeric comparator - is deliberately
 * never called: there is no `Double` in `holiday-baseline.json`, and a tolerance applied to a
 * business-day count or a shifted date would admit an answer that is simply wrong.
 *
 * The one comparison that does not call `assertExact` is the year's holiday list, and it differs
 * only in how a difference is '''rendered''': the values are compared by the same exact equality,
 * and a mismatch is then reported as a bounded symmetric difference instead of as two printed
 * vectors. A year holds up to 366 dates, the fixture holds 3,846 rows, and a systematically broken
 * bitmask would otherwise write tens of megabytes of report - so the comparison stays exact while
 * the message stays readable. See [[HolidayCalendarParitySpec.compareHolidays]].
 *
 * ===What `holidays` means, and why it is the union===
 *
 * A row's `holidays` is '''every''' date of that calendar year for which `isHoliday` answers true,
 * '''weekends included'''. It is not the rule-derived holiday table and not "the holidays that are
 * not weekends", and the difference is load bearing rather than presentational:
 *
 *   - `isHoliday` treats a weekend day as a holiday, and a calendar's weekend days are not part of
 *     its public API, so the full `isHoliday` truth is the only contract both implementations can
 *     compute symmetrically. The Java tests assert exactly this predicate, from the other side:
 *     `GlobalHolidayCalendarsTest.test_gblo` walks every day of the year and asserts
 *     `GBLO.isHoliday(d) == (table.contains(d) || d is Saturday || d is Sunday)`
 *     (`modules/basics/src/test/java/com/opengamma/strata/basics/date/GlobalHolidayCalendarsTest.java:276-286`).
 *   - It is what makes `HUBU` measurable without a special case. Budapest is built with SUNDAY as
 *     its '''only''' weekend day and lists its Saturdays explicitly as holidays, minus the
 *     Saturdays its market works, so the Java assertion for it carries a third column:
 *     `HUBU.isHoliday(d) == ((table.contains(d) || Sat || Sun) && !workDays.contains(d))`
 *     (same file, `:924-935`). Capturing the resolved union means this spec needs no knowledge of
 *     either column: the working Saturday 2020-08-29 is simply absent from the captured set for
 *     `hubu-2020` while the ordinary Saturday 2020-08-22 is present, and the same generic code path
 *     that measures `GBLO` measures that.
 *   - It is also what makes the weekend and no-holiday calendars worth a row at all: for `Sat/Sun`
 *     the expectation is that year's Saturdays and Sundays, and for `NoHolidays` it is the empty
 *     list.
 *
 * The recomputation here is deliberately direct - every day of the year handed to `isHoliday`,
 * one call per day - rather than routed through the calendar's own `holidays(start, end)` range
 * method. The single predicate is what the monthly bitmask implements, so measuring it a day at a
 * time is what pins the bitmask; and it keeps the range method, which has its own precondition and
 * its own unit spec, off the path of this measurement.
 *
 * ===Comparing holiday sets, not calendars===
 *
 * Equality on calendars follows Java: `ImmutableHolidayCalendar` and the three weekend calendars
 * compare by identifier alone, and only the composites compare structurally. Asserting equality of
 * calendar objects would therefore be satisfied by any calendar carrying the right name and would
 * prove nothing at all about the data inside it. That is why this spec compares the answers a
 * calendar gives over a whole year, and why the identifier is checked as one field among many
 * rather than as the measurement.
 *
 * ===Out of range is two different things===
 *
 * The fixture distinguishes them and so does this spec, driven by the row rather than by the year:
 *
 *   - '''A year outside the calendar's own holiday range is answered, not refused.''' A dated
 *     calendar asked about 1949 or 2100 falls back to a weekend-only test
 *     (`modules/basics/src/main/java/com/opengamma/strata/basics/date/ImmutableHolidayCalendar.java:397-415`,
 *     asserted by `ImmutableHolidayCalendarTest.test_isBusinessDay_outOfRange` at `:477-485`), so
 *     the 52 `outOfRange` rows carry an ordinary `holidays` expectation - that year's weekend dates
 *     - and are measured exactly like any other row.
 *   - '''A year outside 0000 to 9999 is refused.''' There the lookup neither answers nor falls
 *     back: it fails fast through `ArgCheck`, as the library being ported did. The two `yearRange`
 *     rows (`GBLO` for year 10000 and year -1) carry no `holidays` at all and carry `error`
 *     instead, and every one of their sample operations is required to refuse its argument.
 *
 * A refusal is observed through [[ParityHarness.attemptArgCheck]], which runs the call inside an
 * effect and turns the throw back into a value. Neither `intercept` nor `try`/`catch` appears in
 * this file: both would abandon the rest of the row, and the remaining measurements of a row are
 * exactly what a report is for. Only the '''type''' of the refusal is asserted. The captured
 * message is the message of the implementation being replaced, and pinning the port's wording to
 * it would make this a test of prose; it is quoted in the diagnostic instead.
 *
 * ===Row schema===
 *
 * Section 6 of `tools/parity-capture/README.md` is the schema of record - strict JSON admits no
 * comments - and the model in the companion follows it field for field. Each row is one
 * (calendar, year) pair:
 *
 * {{{
 * {"id":"gblo-2020","source":"generated","calendar":"GBLO","year":2020,
 *  "holidays":["2020-01-01","2020-01-04", …],
 *  "samples":[{"date":"2020-01-01","isHoliday":true,"isBusinessDay":false,
 *              "next":"2020-01-02","previous":"2019-12-31",
 *              "nextOrSame":"2020-01-02","previousOrSame":"2019-12-31",
 *              "shift":{"amount":-3,"result":"2019-12-27"},
 *              "daysBetween":{"endExclusive":"2021-01-01","result":254},
 *              "error":null}, …],
 *  "error":null}
 * }}}
 *
 * Three samples per row, at the fixed dates 1 January, 15 June and 24 December, each carrying the
 * shift amount and the end date it was captured with - so this spec replays them without knowing
 * the sampling rule.
 *
 * The '''row''' states the outcome and every sample of it states the same one: where the row
 * carries no `error`, all three samples carry no `error` and carry all eight of their expectations;
 * where the row carries `error`, all three carry `error` and none of the eight. A sample that
 * disagrees with its row about the outcome, and a sample carrying some but not all of the eight,
 * are both fixtures that no longer agree with this spec: they are reported as such, and the row is
 * then '''not''' measured at all, because which of the two measurements applies is exactly what has
 * become unclear. Keying the rule to the row rather than to each sample's own `error` is what
 * closes the gap a refused row full of populated samples would otherwise pass through - the row
 * would be measured as a refusal and every captured expectation in it would go unread. Every
 * expectation a sample carries is measured: they were all captured from Java, so leaving
 * `isBusinessDay`, `nextOrSame` or `previousOrSame` unread would discard captured baseline rather
 * than avoid re-deriving anything.
 *
 * What this spec does '''not''' do is invent a probe the fixture has no expectation for.
 * `nextSameOrLastInMonth`, `lastBusinessDayOfMonth`, `isLastBusinessDayOfMonth`, `businessDays`,
 * `combinedWith` and `linkedWith` are unit behaviour owned by `date.HolidayCalendarSpec` and
 * `date.ImmutableHolidayCalendarSpec`, and computing an expectation for one of them here would be
 * re-deriving by hand the very values the fixture-first design exists to capture. The 201 per-year
 * Java tables and the Easter algorithm are likewise `date.GlobalHolidayCalendarsSpec`'s
 * responsibility; this document covers every remaining year of the 1950-2099 span.
 *
 * ===The fixture is the authority===
 *
 * `holiday-baseline.json` was captured from the untouched Java modules by
 * `tools/parity-capture/capture-baseline.jsh`, which cross-checked its rows against the Java test
 * tables before writing them. It is never edited here, no expectation is ever "corrected", and no
 * row is ever skipped: a row that disagrees with the port means the port is wrong, and reporting it
 * is the whole job. Because a gate that reads `failed == 0` cannot tell a complete measurement from
 * a thinned one, the second test below asserts the population the baseline is required to carry -
 * as the '''exact''' set of (`source`, calendar, year) triples the endpoints of AAP section 0.6.1
 * fix, compared in both directions. Counts and contiguity are not enough for that: a span shifted
 * to 1951-2100, a fallback probe moved from 1949 to 1948 or a weekend row recaptured for 2021
 * leaves every count and every contiguous run intact while retiring exactly the boundary years the
 * fallback, the bitmask start and the bitmask end are pinned by. See
 * [[HolidayCalendarParitySpec.RequiredPopulation]].
 *
 * ===No timing===
 *
 * This is the largest fixture of the module, and it carries no duration or elapsed-time assertion
 * of any kind - nor does the report it publishes. Cost is controlled structurally instead: the
 * calendars are resolved once per distinct name for the whole run, and the built-in set is a lazy
 * value over lazily generated calendars, so the rule generation is paid once per JVM.
 */
class HolidayCalendarParitySpec extends AsyncFunSuite with AsyncIOSpec with Matchers {

  import HolidayCalendarParitySpec._

  /*
   * The measurement is declared first, deliberately: the report it publishes is the artifact
   * Gate 3 collects, so it is written on every run of this suite rather than only on the runs
   * where some other case happens to pass first.
   */
  test("the built-in holiday calendars reproduce the Java baseline exactly") {
    for {
      // One resolution per distinct calendar name for the whole run. The cache is an immutable
      // map behind a cats-effect reference created inside this effect: no `var`, no mutable
      // collection, and nothing ambient - the rows are measured one after another, so the map is
      // built in fixture order and a repeated name costs a lookup.
      calendars <- Ref.of[IO, Map[String, HolidayCalendar]](Map.empty)
      report <- ParityHarness.runFixture[HolidayRow](FixtureName, FixtureResource, RowSchema)(row =>
        checkRow(calendars, row))
      _ <- ParityHarness.failIfAny(report)
    } yield succeed
  }

  test("the fixture carries the population the holiday baseline is required to measure") {
    ParityHarness.loadStrict[HolidayRow](FixtureResource, RowSchema).map { rows =>
      val counts = rows.groupBy(_.source).view.mapValues(_.size).toMap
      val population = rows.map(row => (row.source, row.calendar, row.year)).toSet
      val differences =
        describePopulation("required rows missing from the fixture", RequiredPopulation -- population) :::
          describePopulation(
            "rows present that this spec does not account for",
            population -- RequiredPopulation)
      withClue(
        s"fixture rows: ${rows.size}; rows by source: " +
          s"${counts.toVector.sortBy(_._1).mkString(", ")}: ") {
        // The population is compared as the exact set of (source, calendar, year) triples the
        // endpoints of AAP section 0.6.1 require - in both directions, so neither a row that is
        // missing nor a row this spec cannot account for passes. This is what makes the endpoints
        // themselves the contract: a span shifted to 1951-2100, a fallback probe substituted for
        // its neighbour, or a weekend row moved off 2020 changes the set and is reported, where a
        // count floor and a contiguity check see nothing.
        differences shouldBe empty
        // The set comparison is blind to multiplicity, so the count is asserted against the size
        // of the required set: a (source, calendar, year) captured twice under two identities
        // would otherwise leave the set equal.
        rows.size shouldBe RequiredPopulation.size
        rows.map(_.id).distinct should have size rows.size.toLong
        // A row of unstated provenance is named as that rather than only as a row that is not
        // required, which is how the exact comparison above would render it.
        rows.map(_.source).distinct.filterNot(RequiredSources.contains) shouldBe empty
        // Every calendar the port is required to reproduce appears, including the three
        // composites and the four weekend and no-holiday calendars. Subsumed by the comparison
        // above, and kept because a calendar that has vanished entirely names itself here instead
        // of being read off a list of its years.
        rows.map(_.calendar).distinct.toSet shouldBe RequiredCalendars
        // Exactly the two deliberate rows refuse to answer, and a row refuses if and only if it
        // carries no holiday list: those are the two row kinds this spec measures differently, so
        // the distinction is asserted rather than assumed. Only identities are reported, because
        // a row prints its whole year.
        rows.filter(_.error.isDefined).map(_.id) shouldBe RefusingRowIds
        rows.filter(row => row.error.isDefined != row.holidays.isEmpty).map(_.id) shouldBe empty
        rows.filter(_.samples.size != SamplesPerRow).map(_.id) shouldBe empty
        succeed
      }
    }
  }

  /*
   * The three tests below are about the decoding of the fixture rather than about the port. They
   * exist because the two above cannot see what they are not given: a gate that reads
   * `failed == 0` over rows that decoded perfectly cannot tell a fixture that is measured in full
   * from one that has grown a key nothing reads. So the four schemas are asserted to be the key
   * sets the models actually read, and the refusals are exercised rather than assumed.
   */

  test("the declared row, sample and probe key sets are the ones the models read") {
    IO {
      // Each schema is its model's own field set, so the keys the decoders enforce cannot drift
      // from the fields this spec measures: a field added to a model without being added to its
      // schema, or the reverse, fails here.
      RowSchema.known shouldBe DocumentedRowModel.productElementNames.toSet
      RowSchema.known.size shouldBe 7
      SampleSchema.known shouldBe DocumentedSampleModel.productElementNames.toSet
      SampleSchema.known.size shouldBe 10
      ShiftProbeSchema.known shouldBe DocumentedSampleModel.shift.productElementNames.toSet
      BetweenProbeSchema.known shouldBe DocumentedSampleModel.daysBetween.productElementNames.toSet
      // And the documented shapes are those key sets, which is what ties the committed documents
      // below to the declarations above. Every schema here has one variant and no optional key,
      // so satisfying it is equality of key sets.
      DocumentedRow.asObject.map(_.keys.toSet) shouldBe Some(RowSchema.known)
      DocumentedSample.asObject.map(_.keys.toSet) shouldBe Some(SampleSchema.known)
      DocumentedShift.asObject.map(_.keys.toSet) shouldBe Some(ShiftProbeSchema.known)
      DocumentedDaysBetween.asObject.map(_.keys.toSet) shouldBe Some(BetweenProbeSchema.known)
      Vector(RowSchema, SampleSchema, ShiftProbeSchema, BetweenProbeSchema)
        .map(schema => (schema.variants.size, schema.optional)) shouldBe
        Vector.fill(4)((1, Set.empty[String]))
      succeed
    }
  }

  test("a captured row whose keys are not the documented seven is refused by name") {
    IO {
      // The documented shape decodes, field for field, nested objects included: strictness
      // refuses what the document does not document and nothing else. This is a decode identity
      // compared exactly - the mapping of keys onto fields - and not a measurement.
      StrictRowDecoder.decodeJson(DocumentedRow) shouldBe Right(DocumentedRowModel)
      // A key the capture has started emitting. This is the case the finding is about: a derived
      // decoder would ignore it and measure the row as though the new expectation did not exist.
      refusalOf(
        StrictRowDecoder,
        withKey(DocumentedRow, "businessDays", Json.arr())) should include("unknown keys {businessDays}")
      // A schema-required key the document no longer carries. `holidays` is the one this spec
      // reads as the difference between a row that answers and a row that refuses, so its
      // absence must be refused rather than read as the other kind of row.
      refusalOf(StrictRowDecoder, withoutKey(DocumentedRow, "holidays")) should include(
        "a holiday parity row is missing {holidays}")
      // A renamed key is both at once, and the refusal names both halves.
      val renamed = refusalOf(StrictRowDecoder, withRenamedKey(DocumentedRow, SamplesKey, "probes"))
      renamed should include("unknown keys {probes}")
      renamed should include(s"a holiday parity row is missing {$SamplesKey}")
      succeed
    }
  }

  test("a sample or one of its probes whose keys are not the documented ones is refused by name") {
    IO {
      // A sample's keys, and a probe's, are visible only to that object's own decoder, so each
      // refusal below is offered through a row exactly as the loader reads one.
      StrictRowDecoder.decodeJson(rowWithSample(DocumentedSample)).map(_.samples) shouldBe
        Right(Vector(DocumentedSampleModel))
      // One more captured probe of the calendar, which nothing would read.
      refusalOf(
        StrictRowDecoder,
        rowWithSample(withKey(DocumentedSample, "lastBusinessDayOfMonth", Json.Null))) should include(
        "unknown keys {lastBusinessDayOfMonth}")
      // An expectation the capture has stopped emitting, which `Option` alone cannot tell apart
      // from the `null` that records a refusal.
      refusalOf(StrictRowDecoder, rowWithSample(withoutKey(DocumentedSample, "nextOrSame"))) should include(
        "a holiday parity sample is missing {nextOrSame}")
      val renamedSample = refusalOf(
        StrictRowDecoder,
        rowWithSample(withRenamedKey(DocumentedSample, "previousOrSame", "priorOrSame")))
      renamedSample should include("unknown keys {priorOrSame}")
      renamedSample should include("a holiday parity sample is missing {previousOrSame}")
      // The shift probe: a second operand, a missing operand and a renamed one.
      refusalOf(
        StrictRowDecoder,
        rowWithSample(
          sampleWithProbe(ShiftKey, withKey(DocumentedShift, "calendar", Json.fromString("GBLO"))))
      ) should include("unknown keys {calendar}")
      refusalOf(
        StrictRowDecoder,
        rowWithSample(sampleWithProbe(ShiftKey, withoutKey(DocumentedShift, "amount")))) should include(
        "the shift probe of a holiday parity sample is missing {amount}")
      val renamedShift = refusalOf(
        StrictRowDecoder,
        rowWithSample(sampleWithProbe(ShiftKey, withRenamedKey(DocumentedShift, "result", "shifted"))))
      renamedShift should include("unknown keys {shifted}")
      renamedShift should include("the shift probe of a holiday parity sample is missing {result}")
      // The days-between probe: the same three departures.
      refusalOf(
        StrictRowDecoder,
        rowWithSample(
          sampleWithProbe(DaysBetweenKey, withKey(DocumentedDaysBetween, "startInclusive", Json.Null)))
      ) should include("unknown keys {startInclusive}")
      refusalOf(
        StrictRowDecoder,
        rowWithSample(sampleWithProbe(DaysBetweenKey, withoutKey(DocumentedDaysBetween, "endExclusive")))
      ) should include("the days-between probe of a holiday parity sample is missing {endExclusive}")
      val renamedBetween = refusalOf(
        StrictRowDecoder,
        rowWithSample(
          sampleWithProbe(DaysBetweenKey, withRenamedKey(DocumentedDaysBetween, "result", "count"))))
      renamedBetween should include("unknown keys {count}")
      renamedBetween should include("the days-between probe of a holiday parity sample is missing {result}")
      succeed
    }
  }
}

/**
 * The row model of `holiday-baseline.json` and the checks applied to one row.
 *
 * It lives in the companion rather than in the suite because the JSON derivation needs the row
 * types on a stable path, and because keeping the measurement out of the suite body makes it plain
 * that the suite contributes nothing to the measurement beyond ordering it. Everything here is
 * confined to this package.
 */
private[parity] object HolidayCalendarParitySpec {

  //-------------------------------------------------------------------------
  // Contract constants. The first two are agreements with something outside this file - the
  // resource name with the capture script that writes it, the fixture stem with the gate script
  // that reads `<parity.report.dir>/holiday.json` - so neither may drift.
  //-------------------------------------------------------------------------

  /** The fixture stem, which is also the `fixture` field of the report and its file name. */
  val FixtureName: String = "holiday"

  /** The classpath name of the captured baseline, relative to the test resource root. */
  val FixtureResource: String = "parity/holiday-baseline.json"

  /** The number of date samples every row carries, at 1 January, 15 June and 24 December. */
  val SamplesPerRow: Int = 3

  /**
   * The identity of one row within the population: its `source`, its calendar and its year.
   *
   * This triple, not the row's `id`, is what the population contract is stated over: the `id` is a
   * rendering of it, so pinning the triples pins which (calendar, year) pairs were measured under
   * which provenance, independently of how the capture chose to name them.
   */
  type RowKey = (String, String, Int)

  /** The `source` of a row produced by one of the rule generators, over the calendar's own span. */
  val GeneratedSource: String = "generated"

  /** The `source` of a row of a calendar published as a data table rather than generated. */
  val DataTableSource: String = "dataTable"

  /** The `source` of a probe one year outside a calendar's own span, where Java falls back. */
  val OutOfRangeSource: String = "outOfRange"

  /** The `source` of a row of one of the weekend and no-holiday calendars. */
  val WeekendSource: String = "weekend"

  /** The `source` of a row of one of the composite calendars. */
  val CompositeSource: String = "composite"

  /** The `source` of a row whose year lies outside 0000 to 9999, which the calendar refuses. */
  val YearRangeSource: String = "yearRange"

  /**
   * Every rule-generated calendar, in the order `GlobalHolidayCalendars` builds them.
   *
   * Twenty-five names
   * (`modules/basics/src/main/java/com/opengamma/strata/basics/date/GlobalHolidayCalendars.java:59-96`),
   * of which twenty-four loop 1950-2099 and `EUTA` covers 1997-2099 instead because
   * `generateEuropeanTarget` starts there (same file, `:128,296`). The split is stated once, in
   * [[DatedSpans]], so the span of a calendar and the out-of-range probes either side of it cannot
   * drift apart.
   */
  val GeneratedCalendars: Vector[String] =
    Vector(
      "GBLO",
      "FRPA",
      "DEFR",
      "CHZU",
      "EUTA",
      "USGS",
      "USNY",
      "NYFD",
      "NYSE",
      "JPTO",
      "AUSY",
      "BRBD",
      "CAMO",
      "CATO",
      "CZPR",
      "DKCO",
      "HUBU",
      "MXMC",
      "NOOS",
      "NZAU",
      "NZWE",
      "NZBD",
      "PLWA",
      "SEST",
      "ZAJO"
    )

  /** The calendar published as an explicit data table rather than generated from rules. */
  val DataTableCalendar: String = "THBA"

  /** The calendar whose span starts later than every other generated one, `EUTA`. */
  val LateStartingCalendar: String = "EUTA"

  /** The four weekend and no-holiday calendars, which carry no dated span of their own. */
  val WeekendCalendars: Vector[String] = Vector("NoHolidays", "Sat/Sun", "Fri/Sat", "Thu/Fri")

  /** The three composite calendars, two combined and one linked. */
  val CompositeCalendars: Vector[String] = Vector("GBLO+USNY", "GBLO~USNY", "JPTO+USNY")

  /** The calendar the two out-of-year-range refusals were captured for. */
  val YearRangeCalendar: String = "GBLO"

  /** The first year of the twenty-four generated calendars that loop the whole span. */
  val GeneratedFirstYear: Int = 1950

  /** The last year of every generated calendar, `EUTA` included. */
  val GeneratedLastYear: Int = 2099

  /** The first year of `EUTA`, whose generator starts in 1997 rather than 1950. */
  val LateStartingFirstYear: Int = 1997

  /** The first year of the published Thai table. */
  val DataTableFirstYear: Int = 2005

  /** The last year of the published Thai table. */
  val DataTableLastYear: Int = 2079

  /**
   * The two years the weekend, no-holiday and composite calendars were captured for.
   *
   * These calendars answer for every year, so a span would be arbitrary; 2020 and 2024 are the two
   * the capture fixed on - one with 1 January on a Wednesday and one on a Monday, both leap years -
   * and AAP section 0.6.1 names them as the population of those rows.
   */
  val ProbeYears: Vector[Int] = Vector(2020, 2024)

  /**
   * The two years outside 0000 to 9999 that the calendar refuses to answer for.
   *
   * They bracket the accepted range from both sides, which is what makes the refusal a range check
   * rather than an upper bound (`ImmutableHolidayCalendar.java:410-415`).
   */
  val YearRangeYears: Vector[Int] = Vector(-1, 10000)

  /**
   * One calendar's own span of years, and the provenance the rows of that span carry.
   *
   * The out-of-range probes are derived from the same two endpoints rather than listed separately
   * (see [[fallbackProbes]]), so a span that is widened, narrowed or shifted moves its probes with
   * it and cannot leave a stale pair behind that still satisfies this spec.
   *
   * @param source  the `source` the rows inside the span carry
   * @param calendar  the calendar name
   * @param firstYear  the first year of the span, inclusive
   * @param lastYear  the last year of the span, inclusive
   */
  final case class DatedSpan(source: String, calendar: String, firstYear: Int, lastYear: Int) {

    /** Every row the span itself requires, one per year from [[firstYear]] to [[lastYear]]. */
    def rows: Vector[RowKey] =
      (firstYear to lastYear).toVector.map(year => (source, calendar, year))

    /**
     * The two rows that probe the weekend-only fallback, immediately below and above the span.
     *
     * Java answers these years rather than refusing them, falling back to a weekend-only test
     * outside the bitmask array (`ImmutableHolidayCalendar.java:397-415`), so they are ordinary
     * measured rows and carry the [[OutOfRangeSource]] provenance.
     */
    def fallbackProbes: Vector[RowKey] =
      Vector((OutOfRangeSource, calendar, firstYear - 1), (OutOfRangeSource, calendar, lastYear + 1))
  }

  /**
   * The span of every calendar that has one, generated or published.
   *
   * AAP section 0.6.1's holiday fixture row and section 0.6.2's data port are the source of these
   * endpoints: the twenty-four generators over 1950-2099, `EUTA` over 1997-2099, and `THBA` over
   * the 75 published years 2005-2079 (`HolidayCalendarData.ini:31-107`).
   */
  val DatedSpans: Vector[DatedSpan] =
    GeneratedCalendars.map { calendar =>
      val firstYear = if (calendar == LateStartingCalendar) LateStartingFirstYear else GeneratedFirstYear
      DatedSpan(GeneratedSource, calendar, firstYear, GeneratedLastYear)
    } :+ DatedSpan(DataTableSource, DataTableCalendar, DataTableFirstYear, DataTableLastYear)

  /**
   * Every (`source`, calendar, year) the baseline is required to carry, and nothing besides.
   *
   * Derived entirely from the constants above, so the contract is stated as the endpoints AAP
   * section 0.6.1 fixes rather than as a row count: the dated spans, the fallback probe either side
   * of each of them, the weekend and composite calendars in [[ProbeYears]], and the two
   * out-of-year-range refusals. The committed document holds exactly these 3,846 triples.
   */
  val RequiredPopulation: Set[RowKey] =
    (DatedSpans.flatMap(_.rows) ++
      DatedSpans.flatMap(_.fallbackProbes) ++
      WeekendCalendars.flatMap(calendar => ProbeYears.map(year => (WeekendSource, calendar, year))) ++
      CompositeCalendars.flatMap(calendar => ProbeYears.map(year => (CompositeSource, calendar, year))) ++
      YearRangeYears.map(year => (YearRangeSource, YearRangeCalendar, year))).toSet

  /**
   * The closed set of `source` keys, read off the required population.
   *
   * A row whose `source` is not one of these six is a fixture this spec does not know how to
   * account for. The exact population comparison would report it too, as a row that is not
   * required; naming the unknown provenance separately says which of the two it is.
   */
  val RequiredSources: Set[String] = RequiredPopulation.map { case (source, _, _) => source }

  /**
   * Every calendar name the baseline is required to measure, read off the required population.
   *
   * The 25 generated calendars and the published Thai calendar, the four weekend and no-holiday
   * calendars, and the three composites - AAP section 0.6.2's covered set, which
   * `SCALA_MIGRATION.md` documents as the ported calendar set.
   */
  val RequiredCalendars: Set[String] = RequiredPopulation.map { case (_, calendar, _) => calendar }

  /**
   * The identifiers of the two rows that record a refusal rather than an answer.
   *
   * Named rather than derived, so that a fixture in which some other row has started to record an
   * error - or in which one of these two has stopped - is reported. A calendar refusing to answer
   * is the narrow, documented precondition of AAP section 0.3.3, not a general outcome.
   */
  val RefusingRowIds: Vector[String] = Vector("gblo-10000-year-range", "gblo-minus1-year-range")

  /**
   * The number of differing dates quoted in each direction when a holiday list does not match.
   *
   * The message always reports the true totals; this bounds only how many are named. A calendar
   * year holds up to 366 dates and the fixture holds thousands of rows, so an unbounded rendering
   * of a systematic difference would produce a report too large to read and too large to publish.
   */
  val HolidayDifferenceLimit: Int = 10

  /**
   * The number of rows named when the fixture's population differs from the required one.
   *
   * A difference of one endpoint is a difference of one row, but a shifted span or a renamed source
   * differs in thousands, and a failure message that printed them all would be unreadable. The
   * message always reports the true totals; this bounds only how many are named.
   */
  val PopulationDifferenceLimit: Int = 12

  //-------------------------------------------------------------------------
  // The row model. Field for field the schema of section 6 of `tools/parity-capture/README.md`,
  // with `Option` exactly where that schema writes JSON `null`: a row that refuses to answer
  // carries no holiday list and no sample expectations, and carries `error` instead.
  //
  // Each of the four shapes of the document - the row, a sample, and the two probes nested in a
  // sample - is declared as a [[KeySchema]] beside the model it describes, and every object is
  // checked against its schema before it is decoded: the row by `loadStrict`, the three nested
  // shapes by their own decoders, which are the only places those objects' keys are ever visible.
  // Without that, derived decoding would read the fields these models declare and ignore every
  // other key, so a probe the capture started emitting - another operation, another operand of a
  // shift, a renamed field - would be dropped in silence while the report still read
  // `failed == 0`. Declaring the keys also makes `Option` mean what the schema says: the key must
  // be present, and `null` is then the one way it records a refusal, which a merely optional
  // field cannot distinguish from a key that has gone missing.
  //-------------------------------------------------------------------------

  /** The key of the business-day shift probe, inside a sample. */
  val ShiftKey: String = "shift"

  /** The key of the business-day count probe, inside a sample. */
  val DaysBetweenKey: String = "daysBetween"

  /** The key of the sample list, inside a row. */
  val SamplesKey: String = "samples"

  /**
   * The shift a sample was captured with, and what it produced.
   *
   * @param amount  the number of business days the date was shifted by, which may be negative
   * @param result  the shifted date, absent where the calendar refused to answer
   */
  final case class ShiftProbe(amount: Int, result: Option[LocalDate])

  /**
   * The documented shape of the `shift` object of a sample.
   *
   * This is the schema '''of''' [[ShiftProbe]] - the two keys that model declares, asserted
   * against each other by `the declared row, sample and probe key sets are the ones the models
   * read`. Its authority is the committed document, in which every one of the 11,538 `shift`
   * objects carries exactly `amount` and `result`, together with section 6 of
   * `tools/parity-capture/README.md`, which documents the probe as self-describing: the amount is
   * in the document so that this spec replays the shift without knowing the sampling rule. A
   * second operand appearing here would change what was captured, so it is refused by name rather
   * than ignored.
   */
  val ShiftProbeSchema: KeySchema =
    KeySchema.uniform("the shift probe of a holiday parity sample", Set("amount", "result"))

  /**
   * The business-day count a sample was captured with, and what it produced.
   *
   * @param endExclusive  the end of the counted range, excluded from the count
   * @param result  the number of business days in the range, absent where the calendar refused
   */
  final case class BetweenProbe(endExclusive: LocalDate, result: Option[Int])

  /**
   * The documented shape of the `daysBetween` object of a sample.
   *
   * This is the schema '''of''' [[BetweenProbe]], on the same authority as [[ShiftProbeSchema]]:
   * every one of the committed document's 11,538 `daysBetween` objects carries exactly
   * `endExclusive` and `result`, and section 6 of `tools/parity-capture/README.md` documents the
   * end date as part of the probe so that the count is replayed over the captured range rather
   * than over one this spec chose.
   */
  val BetweenProbeSchema: KeySchema =
    KeySchema.uniform(
      "the days-between probe of a holiday parity sample",
      Set("endExclusive", "result"))

  /**
   * One sampled date of a row, with every answer the Java implementation gave for it.
   *
   * The outcome is the row's, and this sample states the same one: where its row carries no
   * `error`, `error` is absent here and every one of the eight expectations is present; where its
   * row carries `error`, so does this sample, carrying the message Java produced and none of the
   * eight. [[checkSample]] and [[checkRefusedSample]] are the two readings of that, [[checkRow]]
   * decides between them from the row, and [[checkShape]] requires the sample to agree with the
   * row before either is reached.
   *
   * @param date  the sampled date
   * @param isHoliday  whether the date is a holiday, weekends included
   * @param isBusinessDay  whether the date is a business day
   * @param next  the first business day strictly after the date
   * @param previous  the last business day strictly before the date
   * @param nextOrSame  the date itself where it is a business day, otherwise the next one
   * @param previousOrSame  the date itself where it is a business day, otherwise the previous one
   * @param shift  the business-day shift applied to the date, and its result
   * @param daysBetween  the business-day count from the date, and its result
   * @param error  the refusal Java reported, where it refused to answer for this date
   */
  final case class HolidaySample(
      date: LocalDate,
      isHoliday: Option[Boolean],
      isBusinessDay: Option[Boolean],
      next: Option[LocalDate],
      previous: Option[LocalDate],
      nextOrSame: Option[LocalDate],
      previousOrSame: Option[LocalDate],
      shift: ShiftProbe,
      daysBetween: BetweenProbe,
      error: Option[String])

  /**
   * The documented shape of one element of a row's `samples` list.
   *
   * This is the schema '''of''' [[HolidaySample]] - the ten keys that model declares, which the
   * committed document carries on every one of its 11,538 samples, and which section 6 of
   * `tools/parity-capture/README.md` documents together with the fixed (date, shift amount,
   * `endExclusive`) triple each sample stands for.
   *
   * One documented key set, so satisfying it is equality of key sets. That is what closes the
   * gap the fixture's own `error` convention leaves open: a sample records either every
   * expectation or none of them, and an eleventh key - one more probe of the calendar, captured
   * from Java and therefore an expectation like any other - would otherwise be read by nothing
   * while [[checkShape]] still found the sample consistent.
   */
  val SampleSchema: KeySchema =
    KeySchema.uniform(
      "a holiday parity sample",
      Set(
        "date",
        "isHoliday",
        "isBusinessDay",
        "next",
        "previous",
        "nextOrSame",
        "previousOrSame",
        ShiftKey,
        DaysBetweenKey,
        "error"))

  /**
   * One (calendar, year) row of the captured baseline.
   *
   * @param id  the identity of the row, unique across the document and what the report names
   * @param source  the population the row belongs to, one of [[RequiredSources]]
   * @param calendar  the name of the calendar identifier, such as `GBLO`, `Sat/Sun` or `GBLO+USNY`
   * @param year  the calendar year the row measures
   * @param holidays  every date of that year for which `isHoliday` is true, weekends included;
   *                  absent where the calendar refuses to answer for the year at all
   * @param samples  the three sampled dates of the year
   * @param error  the refusal Java reported for the year, where it refused
   */
  final case class HolidayRow(
      id: String,
      source: String,
      calendar: String,
      year: Int,
      holidays: Option[Vector[LocalDate]],
      samples: Vector[HolidaySample],
      error: Option[String])
      extends ParityRow

  /**
   * The documented shape of one row of `holiday-baseline.json`.
   *
   * This is the schema '''of''' [[HolidayRow]] - the seven keys the committed document carries on
   * every one of its 3,846 rows, which are exactly the seven fields that model declares, and
   * which section 6 of `tools/parity-capture/README.md` records as identical in every row.
   *
   * One documented key set, so satisfying it is equality of key sets: a row that has gained a key
   * is refused with that key named, and a row that has lost one - `holidays`, say, whose absence
   * this spec reads as "the calendar refused to answer for the year" - is refused rather than
   * measured as the other kind of row.
   */
  val RowSchema: KeySchema =
    KeySchema.uniform(
      "a holiday parity row",
      Set("id", "source", "calendar", "year", "holidays", SamplesKey, "error"))

  implicit val shiftProbeDecoder: Decoder[ShiftProbe] =
    ParityHarness.strictObject(ShiftProbeSchema)(deriveDecoder[ShiftProbe])

  implicit val betweenProbeDecoder: Decoder[BetweenProbe] =
    ParityHarness.strictObject(BetweenProbeSchema)(deriveDecoder[BetweenProbe])

  implicit val holidaySampleDecoder: Decoder[HolidaySample] =
    ParityHarness.strictObject(SampleSchema)(deriveDecoder[HolidaySample])

  /**
   * The row's own fields, read once the keys are known to be the documented ones.
   *
   * Deliberately not implicit: nothing may summon a decoder for a row of this document that is
   * not the strict one below, so the only reference to this value is the composition that makes
   * it strict.
   */
  private val holidayRowFields: Decoder[HolidayRow] = deriveDecoder[HolidayRow]

  /**
   * The decoder the fixture is read through, which is the row's fields behind the key check.
   *
   * This is the implicit a loader summons, so every path that reads a row of this document -
   * `ParityHarness.loadStrict`, which composes this with the same check again while reading, and
   * the strictness tests of this suite, which decode hand-built objects through it - is strict
   * about keys by construction rather than by remembering to be.
   */
  implicit val StrictRowDecoder: Decoder[HolidayRow] =
    ParityHarness.strictObject(RowSchema)(holidayRowFields)

  //-------------------------------------------------------------------------
  // The documented shapes as documents, and the three ways a document departs from one.
  //
  // A schema that is only exercised by the fixture it already agrees with proves nothing about
  // what it would refuse, so the strictness tests of this suite decode these documents: the
  // documented shapes, which must be accepted and must decode to the models below, and then the
  // same documents with one key added, one key removed and one key renamed, each of which must be
  // refused with the offending key named. The accepted documents are the committed shapes, key
  // for key - the first row of `holiday-baseline.json` and its first sample - with the holiday
  // list and the sample list shortened to what a decode needs; a decode reads the elements of a
  // list, and their number is a property of the measurement that [[checkShape]] and the
  // population test assert.
  //-------------------------------------------------------------------------

  /** The `shift` probe of the first sample of the committed document. */
  val DocumentedShift: Json =
    Json.obj("amount" -> Json.fromInt(-3), "result" -> Json.fromString("1949-12-28"))

  /** The `daysBetween` probe of the first sample of the committed document. */
  val DocumentedDaysBetween: Json =
    Json.obj(
      "endExclusive" -> Json.fromString("1951-01-01"),
      "result" -> Json.fromInt(254))

  /** The first sample of the committed document, key for key. */
  val DocumentedSample: Json =
    Json.obj(
      "date" -> Json.fromString("1950-01-01"),
      "isHoliday" -> Json.fromBoolean(true),
      "isBusinessDay" -> Json.fromBoolean(false),
      "next" -> Json.fromString("1950-01-02"),
      "previous" -> Json.fromString("1949-12-30"),
      "nextOrSame" -> Json.fromString("1950-01-02"),
      "previousOrSame" -> Json.fromString("1949-12-30"),
      ShiftKey -> DocumentedShift,
      DaysBetweenKey -> DocumentedDaysBetween,
      "error" -> Json.Null)

  /** The first row of the committed document, key for key, carrying that one sample. */
  val DocumentedRow: Json =
    Json.obj(
      "id" -> Json.fromString("gblo-1950"),
      "source" -> Json.fromString("generated"),
      "calendar" -> Json.fromString("GBLO"),
      "year" -> Json.fromInt(1950),
      "holidays" -> Json.arr(Json.fromString("1950-01-01"), Json.fromString("1950-01-07")),
      SamplesKey -> Json.arr(DocumentedSample),
      "error" -> Json.Null)

  /** What [[DocumentedSample]] is required to decode to, field for field. */
  val DocumentedSampleModel: HolidaySample =
    HolidaySample(
      date = LocalDate.of(1950, 1, 1),
      isHoliday = Some(true),
      isBusinessDay = Some(false),
      next = Some(LocalDate.of(1950, 1, 2)),
      previous = Some(LocalDate.of(1949, 12, 30)),
      nextOrSame = Some(LocalDate.of(1950, 1, 2)),
      previousOrSame = Some(LocalDate.of(1949, 12, 30)),
      shift = ShiftProbe(-3, Some(LocalDate.of(1949, 12, 28))),
      daysBetween = BetweenProbe(LocalDate.of(1951, 1, 1), Some(254)),
      error = None)

  /** What [[DocumentedRow]] is required to decode to, field for field. */
  val DocumentedRowModel: HolidayRow =
    HolidayRow(
      id = "gblo-1950",
      source = "generated",
      calendar = "GBLO",
      year = 1950,
      holidays = Some(Vector(LocalDate.of(1950, 1, 1), LocalDate.of(1950, 1, 7))),
      samples = Vector(DocumentedSampleModel),
      error = None)

  /**
   * The documented row carrying the given sample, which is where the nested objects live.
   *
   * A nested object's keys are visible only to that object's own decoder, so the way to prove
   * that the sample and probe schemas are in force is to offer them through a row, exactly as
   * the loader does.
   *
   * @param sample  the sample object to put on the row
   * @return the row document
   */
  def rowWithSample(sample: Json): Json =
    withKey(DocumentedRow, SamplesKey, Json.arr(sample))

  /**
   * The documented sample carrying the given probe under the given key.
   *
   * @param key  either [[ShiftKey]] or [[DaysBetweenKey]]
   * @param probe  the probe object to put on the sample
   * @return the sample document
   */
  def sampleWithProbe(key: String, probe: Json): Json = withKey(DocumentedSample, key, probe)

  /**
   * The same object with one key added, which is the shape a newly captured field arrives in.
   *
   * @param document  the object to change
   * @param key  the key to add, or to replace where the object already carries it
   * @param value  the value of that key
   * @return the changed object
   */
  def withKey(document: Json, key: String, value: Json): Json =
    document.mapObject(fields => fields.add(key, value))

  /**
   * The same object with one key removed, which is the shape a retired field leaves behind.
   *
   * @param document  the object to change
   * @param key  the key to remove
   * @return the changed object
   */
  def withoutKey(document: Json, key: String): Json =
    document.mapObject(fields => fields.remove(key))

  /**
   * The same object with one key renamed, keeping its value - a rename is a loss and a gain at
   * once, and a schema has to report both halves for the message to say what happened.
   *
   * @param document  the object to change
   * @param from  the key as the schema declares it
   * @param to  the key the document is to carry instead
   * @return the changed object
   */
  def withRenamedKey(document: Json, from: String, to: String): Json =
    document.mapObject(fields => fields.remove(from).add(to, fields(from).getOrElse(Json.Null)))

  /**
   * The message a decoder refuses a document with.
   *
   * A decoder that '''accepts''' the document answers with a description of what it accepted, so
   * that the assertion on the refusal's wording fails naming the value that got through rather
   * than failing on an empty string that says nothing.
   *
   * @param decoder  the decoder under test
   * @param document  the document to offer it
   * @return the refusal message, or what was accepted instead
   */
  def refusalOf[A](decoder: Decoder[A], document: Json): String =
    decoder.decodeJson(document) match {
      case Left(failure) => failure.message
      case Right(value) => s"the decoder accepted $value"
    }

  //-------------------------------------------------------------------------
  // Resolving a calendar, once per distinct name.
  //-------------------------------------------------------------------------

  /**
   * Resolves the calendar a row names, reusing the one already resolved for that name.
   *
   * `HolidayCalendarId.of` is total and accepts any name, including a composite joined with `'+'`
   * or `'~'`; resolution against the built-in set is what can fail, and it fails only for a name
   * the built-in set does not hold. This spec resolves against `ReferenceData.standard`, which
   * does no defaulting of unknown identifiers - that is `HolidaySafeReferenceData`'s behaviour and
   * `date.HolidaySafeReferenceDataSpec`'s subject - so a name the standard set cannot resolve is a
   * defect in the port or in the fixture rather than a parity result. It is therefore lifted into
   * a failed effect through [[ParityHarness.raise]], which the driver records against the row.
   *
   * Composite names resolve component by component and are combined, so these rows measure that
   * resolution as well as the combination law.
   *
   * @param cache  the calendars resolved so far in this run, keyed by the name the fixture uses
   * @param name  the calendar name of the row
   * @return the resolved calendar
   */
  private def calendarFor(
      cache: Ref[IO, Map[String, HolidayCalendar]],
      name: String): IO[HolidayCalendar] =

    cache.get.flatMap { resolved =>
      resolved.get(name) match {
        case Some(calendar) => IO.pure(calendar)
        case None =>
          ParityHarness
            .raise(HolidayCalendarId.of(name).resolve(ReferenceData.standard))
            .flatMap(calendar => cache.update(_.updated(name, calendar)).as(calendar))
      }
    }

  //-------------------------------------------------------------------------
  // Measuring one row.
  //-------------------------------------------------------------------------

  /**
   * Measures one row of the fixture, answering with everything that differed.
   *
   * The '''row''' decides which of the two measurements applies, and it is the only outcome
   * authority in this file. A row carrying `error` records that Java refused to answer for the
   * year at all, so every operation of the row is required to refuse; any other row carries a
   * holiday list and three fully populated samples, and every expectation in it is compared.
   *
   * A row whose shape this spec cannot read is '''not''' measured. [[checkShape]] runs first, and
   * where it answers with anything the row is reported as a fixture disagreement and neither
   * [[checkAnsweredYear]] nor [[checkRefusedYear]] is called: which of the two applies is exactly
   * what has become unclear, so measuring one of them anyway would add discrepancies that say
   * nothing about the port - or, worse, would pass. `FxParitySpec.checkRow` gates its measurement
   * the same way for the same reason.
   *
   * The identifier of the resolved calendar is checked in every case, shape disagreement included:
   * a composite name normalises its parts, so the name the port produces is itself a captured
   * expectation, and it is readable from the row's identity alone.
   *
   * @param cache  the calendars resolved so far in this run
   * @param row  the row to measure
   * @return every discrepancy found in the row, empty where it matched in every respect
   */
  def checkRow(cache: Ref[IO, Map[String, HolidayCalendar]], row: HolidayRow): IO[List[String]] =
    calendarFor(cache, row.calendar).flatMap { calendar =>
      val name = ParityHarness.assertExact("calendar name", calendar.name, row.calendar)
      val shape = checkShape(row)
      if (shape.nonEmpty) {
        IO.pure(name ::: shape)
      } else if (row.error.isDefined) {
        checkRefusedYear(calendar, row).map(messages => name ::: messages)
      } else {
        checkAnsweredYear(calendar, row).map(messages => name ::: messages)
      }
    }

  /**
   * Checks the invariants of the row model itself, before anything is measured.
   *
   * These are properties of the document rather than of the port: a row that no longer carries
   * three samples, that carries both an error and a holiday list, that carries a sample disagreeing
   * with it about the outcome, or that carries a half-populated sample, is a fixture that has
   * stopped agreeing with this spec. Reporting that as what it is keeps it from being read as a
   * defect of the port, and keeps it from being absorbed silently by a check that simply finds
   * nothing to compare.
   *
   * ===The row is the outcome authority===
   *
   * The population rule is keyed to the '''row''', because the row is what [[checkRow]] dispatches
   * on. Two things are required of every sample, and they are reported separately so a failure
   * says which one broke:
   *
   *   - '''alignment''' - the sample records a refusal if and only if its row does. A refused row
   *     carrying a sample with no `error` is a disagreement inside the document, and the captured
   *     expectations of such a sample would otherwise never be read at all: [[checkRefusedYear]]
   *     would run, would find that eight calls refuse, and would report nothing.
   *   - '''population''' - the eight expectations are all present when the row answered and all
   *     absent when the row refused. The eight are `isHoliday`, `isBusinessDay`, `next`,
   *     `previous`, `nextOrSame`, `previousOrSame`, `shift.result` and `daysBetween.result`; how
   *     many of them a disagreeing sample carried is part of the message, because a half-populated
   *     sample and an entirely populated one on a refused row are different fixture defects.
   *
   * @param row  the row to inspect
   * @return every way in which the row departs from the schema this spec reads
   */
  private def checkShape(row: HolidayRow): List[String] = {
    val sampleCount =
      if (row.samples.size == SamplesPerRow) Nil
      else
        List(
          s"fixture disagreement: the row carries ${row.samples.size} samples, and this spec " +
            s"reads $SamplesPerRow")
    val exclusive =
      if (row.error.isDefined == row.holidays.isEmpty) Nil
      else
        List(
          "fixture disagreement: a row records either a holiday list or a refusal, but this row " +
            s"carries holidays=${row.holidays.map(_.size)} and error=${row.error}")
    val rowRefused = row.error.isDefined
    val samples = row.samples.iterator.zipWithIndex.flatMap { case (sample, index) =>
      val populated = Vector(
        sample.isHoliday.isDefined,
        sample.isBusinessDay.isDefined,
        sample.next.isDefined,
        sample.previous.isDefined,
        sample.nextOrSame.isDefined,
        sample.previousOrSame.isDefined,
        sample.shift.result.isDefined,
        sample.daysBetween.result.isDefined)
      val alignment =
        if (sample.error.isDefined == rowRefused) Nil
        else
          List(
            s"fixture disagreement: ${label(sample, index)} records ${outcomeOf(sample.error)} " +
              s"while its row records ${outcomeOf(row.error)}, and a sample records the outcome " +
              "of its row")
      val population =
        if (populated.forall(_ == !rowRefused)) Nil
        else
          List(
            s"fixture disagreement: ${label(sample, index)} carries " +
              s"${populated.count(identity)} of ${populated.size} expectations present, and its " +
              s"row records ${outcomeOf(row.error)}, so the sample must carry " +
              (if (rowRefused) "none of them" else "all of them"))
      alignment ::: population
    }
    sampleCount ::: exclusive ::: samples.toList
  }

  /**
   * Names an outcome for a fixture-disagreement message: an answer, or the refusal it recorded.
   *
   * The captured message is quoted rather than summarised, because a disagreement about the
   * outcome is read by someone deciding whether the document or this spec is wrong, and the Java
   * text is what tells them which.
   *
   * @param error  the `error` field of a row or of a sample
   * @return the outcome it states, as a phrase
   */
  private def outcomeOf(error: Option[String]): String =
    error match {
      case Some(message) => s"a refusal ('$message')"
      case None => "an answer"
    }

  /**
   * Measures a row the calendar answers: the whole year, then each sample.
   *
   * Reached only for a row [[checkShape]] found nothing wrong with, so the holiday list and all
   * three fully populated samples are present. The list is still read through [[attempted]] rather
   * than unwrapped, which is what keeps that guarantee an assertion instead of an assumption: an
   * absent list would be reported as the fixture disagreement it is.
   *
   * @param calendar  the resolved calendar
   * @param row  the row to measure
   * @return every discrepancy found
   */
  private def checkAnsweredYear(calendar: HolidayCalendar, row: HolidayRow): IO[List[String]] =
    for {
      year <- attempted("holidays", row.holidays)(holidaysOfYear(calendar, row.year))(
        compareHolidays(_, _))
      samples <- row.samples.toList.zipWithIndex.traverse { case (sample, index) =>
        checkSample(calendar, sample, index)
      }
    } yield year ::: samples.flatten

  /**
   * Measures a row the calendar refuses: every operation of it must refuse its argument.
   *
   * The year itself is probed through `isHoliday` on 1 January, which is the call the capture
   * recorded the row's `error` from, and each sample is probed through all eight of its
   * operations. Only the type of the refusal is asserted; the captured message is quoted in the
   * label so that a report says which Java refusal the row stood for.
   *
   * @param calendar  the resolved calendar
   * @param row  the row to measure
   * @return every discrepancy found
   */
  private def checkRefusedYear(calendar: HolidayCalendar, row: HolidayRow): IO[List[String]] = {
    val firstOfYear = LocalDate.of(row.year, 1, 1)
    for {
      year <- ParityHarness.attemptArgCheck(
        s"isHoliday($firstOfYear) of year ${row.year}, which Java refused with " +
          s"'${row.error.getOrElse("")}'")(calendar.isHoliday(firstOfYear))
      samples <- row.samples.toList.zipWithIndex.traverse { case (sample, index) =>
        checkRefusedSample(calendar, sample, index)
      }
    } yield year ::: samples.flatten
  }

  //-------------------------------------------------------------------------
  // Measuring one sample.
  //-------------------------------------------------------------------------

  /**
   * Compares every expectation of one answered sample.
   *
   * All eight are measured, and each is measured independently of the others: a call that raises
   * where the fixture recorded a value is reported as that field's discrepancy, and the remaining
   * fields of the sample are still measured. `daysBetween` is compared as the `Int` it is, never
   * widened.
   *
   * @param calendar  the resolved calendar
   * @param sample  the sample to measure
   * @param index  the position of the sample in its row, for the message
   * @return every discrepancy found
   */
  private def checkSample(calendar: HolidayCalendar, sample: HolidaySample, index: Int): IO[List[String]] = {
    val prefix = label(sample, index)
    val date = sample.date
    List(
      expect(s"$prefix isHoliday", sample.isHoliday)(calendar.isHoliday(date)),
      expect(s"$prefix isBusinessDay", sample.isBusinessDay)(calendar.isBusinessDay(date)),
      expect(s"$prefix next", sample.next)(calendar.next(date)),
      expect(s"$prefix previous", sample.previous)(calendar.previous(date)),
      expect(s"$prefix nextOrSame", sample.nextOrSame)(calendar.nextOrSame(date)),
      expect(s"$prefix previousOrSame", sample.previousOrSame)(calendar.previousOrSame(date)),
      expect(s"$prefix shift by ${sample.shift.amount}", sample.shift.result)(
        calendar.shift(date, sample.shift.amount)),
      expect(s"$prefix daysBetween to ${sample.daysBetween.endExclusive}", sample.daysBetween.result)(
        calendar.daysBetween(date, sample.daysBetween.endExclusive))
    ).sequence.map(_.flatten)
  }

  /**
   * Requires every operation of one refused sample to refuse its argument.
   *
   * The eight operations are the eight the capture attempted, and Java refused all of them: each
   * of `next`, `previous`, `nextOrSame`, `previousOrSame`, `shift` and `daysBetween` reaches
   * `isHoliday`, and `isHoliday` is where the year is checked. Probing all eight rather than one
   * is what keeps the precondition on the whole surface the fixture covers.
   *
   * @param calendar  the resolved calendar
   * @param sample  the sample to measure
   * @param index  the position of the sample in its row, for the message
   * @return every discrepancy found
   */
  private def checkRefusedSample(
      calendar: HolidayCalendar,
      sample: HolidaySample,
      index: Int): IO[List[String]] = {

    val prefix = s"${label(sample, index)}, which Java refused with '${sample.error.getOrElse("")}',"
    val date = sample.date
    List(
      ParityHarness.attemptArgCheck(s"$prefix isHoliday")(calendar.isHoliday(date)),
      ParityHarness.attemptArgCheck(s"$prefix isBusinessDay")(calendar.isBusinessDay(date)),
      ParityHarness.attemptArgCheck(s"$prefix next")(calendar.next(date)),
      ParityHarness.attemptArgCheck(s"$prefix previous")(calendar.previous(date)),
      ParityHarness.attemptArgCheck(s"$prefix nextOrSame")(calendar.nextOrSame(date)),
      ParityHarness.attemptArgCheck(s"$prefix previousOrSame")(calendar.previousOrSame(date)),
      ParityHarness.attemptArgCheck(s"$prefix shift by ${sample.shift.amount}")(
        calendar.shift(date, sample.shift.amount)),
      ParityHarness.attemptArgCheck(s"$prefix daysBetween to ${sample.daysBetween.endExclusive}")(
        calendar.daysBetween(date, sample.daysBetween.endExclusive))
    ).sequence.map(_.flatten)
  }

  //-------------------------------------------------------------------------
  // The two comparisons, and the one recomputation.
  //-------------------------------------------------------------------------

  /**
   * Every date of a calendar year the calendar reports as a holiday, weekends included.
   *
   * Built from the length of the year and one `isHoliday` call per day, in ascending order, with
   * no mutable accumulator anywhere: the range is mapped to the dates of the year and filtered.
   * This is the predicate the fixture captured, computed the way the Java assertion loops compute
   * it.
   *
   * @param calendar  the calendar to interrogate
   * @param year  the calendar year
   * @return the holidays of that year, ascending
   */
  private def holidaysOfYear(calendar: HolidayCalendar, year: Int): Vector[LocalDate] = {
    val length = LocalDate.of(year, 1, 1).lengthOfYear()
    (1 to length).iterator
      .map(dayOfYear => LocalDate.ofYearDay(year, dayOfYear))
      .filter(date => calendar.isHoliday(date))
      .toVector
  }

  /**
   * Compares two holiday lists exactly, and renders a difference briefly.
   *
   * The comparison is the same exact equality [[ParityHarness.assertExact]] applies - order and
   * multiplicity included - so nothing here is more permissive than the parity rule. What differs
   * is the message: rather than printing both lists, it reports the counts and up to
   * [[HolidayDifferenceLimit]] dates in each direction, with the true totals, so that one badly
   * wrong calendar year cannot fill the report. A count difference with an empty symmetric
   * difference is possible in principle - it would mean a repeated or misordered date - and is
   * reported through the counts.
   *
   * @param actual  the holidays the port reported
   * @param expected  the holidays captured from Java
   * @return the discrepancy, or nothing when the two are equal
   */
  private def compareHolidays(actual: Vector[LocalDate], expected: Vector[LocalDate]): List[String] =
    if (actual == expected) {
      Nil
    } else {
      val missing = expected.filterNot(actual.toSet)
      val unexpected = actual.filterNot(expected.toSet)
      List(
        s"holidays: actual ${actual.size} dates, expected ${expected.size}; " +
          s"${missing.size} expected but absent${quote(missing)}; " +
          s"${unexpected.size} reported but not expected${quote(unexpected)}")
    }

  /** Renders up to [[HolidayDifferenceLimit]] dates of a difference, or nothing when there are none. */
  private def quote(dates: Vector[LocalDate]): String =
    if (dates.isEmpty) {
      ""
    } else {
      val quoted = dates.take(HolidayDifferenceLimit)
      val omitted = dates.size - quoted.size
      val ellipsis = if (omitted > 0) s", and $omitted more" else ""
      quoted.mkString(" (", ", ", s"$ellipsis)")
    }

  /**
   * Renders one direction of a population difference, or nothing when that direction is empty.
   *
   * Both directions of the comparison can be systematic - a shifted span differs by one row at
   * each end of every calendar, a renamed `source` by every row of it - so the rows are named in a
   * stable order and bounded to [[PopulationDifferenceLimit]], with the true total and the size of
   * the required population always stated. Ordering is by source, then calendar, then year, so the
   * examples a failure quotes are the same on every run over the same fixture.
   *
   * @param what  the direction being reported, which is what makes the message stand alone
   * @param rows  the rows of that direction, in any order
   * @return the message, or nothing when there is no difference in that direction
   */
  private def describePopulation(what: String, rows: Set[RowKey]): List[String] =
    if (rows.isEmpty) {
      Nil
    } else {
      val ordered = rows.toVector.sorted
      val quoted = ordered.take(PopulationDifferenceLimit)
      val omitted = ordered.size - quoted.size
      val ellipsis = if (omitted > 0) s", and $omitted more" else ""
      val named =
        quoted
          .map { case (source, calendar, year) => s"$source/$calendar/$year" }
          .mkString(", ")
      List(
        s"$what: ${ordered.size}, against the ${RequiredPopulation.size} rows the baseline is " +
          s"required to carry in total ($named$ellipsis)")
    }

  /**
   * Compares one expectation of a sample, reporting a raised error as that field's discrepancy.
   *
   * The call is evaluated inside an effect and its outcome turned back into a value, so that a
   * port which refuses where the baseline recorded an answer is reported field by field instead of
   * ending the row. An expectation the fixture does not carry on a row that records no refusal is
   * reported as a fixture disagreement rather than quietly passing.
   *
   * @param name  the label of the field, which is what makes the report readable
   * @param expected  the value captured from Java, where the fixture carries one
   * @param thunk  the call to measure
   * @return the discrepancy, or nothing when the value matches
   */
  private def expect[A](name: String, expected: Option[A])(thunk: => A): IO[List[String]] =
    attempted(name, expected)(thunk)((actual, value) => ParityHarness.assertExact(name, actual, value))

  /**
   * Evaluates one measurement of the port and applies a comparison to its result.
   *
   * The common shape of [[expect]] and of the holiday-list comparison: an absent expectation is a
   * fixture disagreement, a raised error is a discrepancy naming the error, and a value is handed
   * to the comparison supplied by the caller.
   *
   * @param name  the label of the field
   * @param expected  the value captured from Java, where the fixture carries one
   * @param thunk  the call to measure
   * @param compare  the comparison of what the port produced against what Java produced
   * @return the discrepancies found
   */
  private def attempted[A](name: String, expected: Option[A])(thunk: => A)(
      compare: (A, A) => List[String]): IO[List[String]] =

    expected match {
      case None =>
        IO.pure(
          List(
            "fixture disagreement: the row records no refusal, so it must carry an expectation " +
              s"for '$name', and it carries none"))
      case Some(value) =>
        IO.delay[A](thunk).attempt.map {
          case Right(actual) => compare(actual, value)
          case Left(error) =>
            List(
              s"$name: expected $value, but the call failed with " +
                s"${error.getClass.getName}: ${Option(error.getMessage).getOrElse("no message")}")
        }
    }

  /** Names one sample of a row by its position and its date, for a message that stands alone. */
  private def label(sample: HolidaySample, index: Int): String = s"samples[$index] at ${sample.date}"
}
