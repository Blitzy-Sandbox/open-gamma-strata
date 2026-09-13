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
 * Parity of the built-in holiday calendars against the committed baseline.
 *
 * The measurement covers the rule-generated national calendars, the published Thai calendar, the
 * four weekend and no-holiday calendars, the composites, the monthly-bitmask representation they
 * are stored in, the weekend-only fallback outside a calendar's year range, and the refusal to
 * answer for a year outside 0000 to 9999. `holiday-baseline.json` holds the reference values and
 * is read-only here: no expectation is corrected and no row is skipped.
 *
 * ===Everything here compares exactly===
 *
 * Every expectation of this fixture is a date, a list of dates, a boolean or an integer, so every
 * comparison is exact and goes through [[ParityHarness.assertExact]]. `assertParity`, the
 * harness's numeric comparator, is never called: there is no `Double` here, and a tolerance on a
 * business-day count or a shifted date would admit an answer that is simply wrong.
 *
 * The one comparison that does not call `assertExact` is the year's holiday list, and it differs
 * only in how a difference is '''rendered''': the values are compared by the same exact equality,
 * and a mismatch is reported as a bounded symmetric difference. A year holds up to 366 dates and
 * the fixture holds 3,846 rows, so rendering a systematically broken bitmask in full would write
 * tens of megabytes of report. See [[HolidayCalendarParitySpec.compareHolidays]].
 *
 * ===What `holidays` means, and why it is the union===
 *
 * A row's `holidays` is '''every''' date of that calendar year for which `isHoliday` answers true,
 * '''weekends included''' - not the rule-derived holiday table, and not "the holidays that are not
 * weekends". Three things follow from that:
 *
 *   - `isHoliday(d)` is true when the calendar's own table holds `d` or `d` falls on one of its
 *     weekend days, and a calendar's weekend days are not part of its public API, so the full
 *     `isHoliday` truth is the only contract both sides of the comparison compute symmetrically.
 *   - It is what makes `HUBU` measurable with no special case. Budapest is built with SUNDAY as
 *     its '''only''' weekend day and lists its Saturdays explicitly as holidays, minus the
 *     Saturdays its market works, so `isHoliday(d)` is true for a date its table or its weekend
 *     day names and false again for a date its market works: the working Saturday 2020-08-29 is
 *     absent for `hubu-2020` while the ordinary Saturday 2020-08-22 is present, measured by the
 *     same generic code path as `GBLO`.
 *   - It is also what makes the weekend and no-holiday calendars worth a row at all: `Sat/Sun`
 *     expects that year's Saturdays and Sundays, `NoHolidays` the empty list.
 *
 * The recomputation is direct - every day of the year handed to `isHoliday`, one call per day -
 * rather than routed through the calendar's own `holidays(start, end)` range method. The single
 * predicate is what the monthly bitmask implements, so measuring it a day at a time pins the
 * bitmask, and it keeps the range method, with its own precondition and unit spec, off this path.
 *
 * ===Comparing holiday sets, not calendars===
 *
 * `ImmutableHolidayCalendar` and the three weekend calendars compare by identifier alone, and only
 * the composites compare structurally, so asserting equality of calendar objects would be
 * satisfied by any calendar carrying the right name and prove nothing about the data inside it.
 * Hence the answers over a whole year are compared, and the identifier is one field among many.
 *
 * ===Out of range is two different things===
 *
 * The row, not the year, says which of the two applies:
 *
 *   - '''A year outside the calendar's own holiday range is answered, not refused.''' A dated
 *     calendar asked about 1949 or 2100 falls back to a weekend-only test, so the 52 `outOfRange`
 *     rows carry an ordinary `holidays` expectation - that year's weekend dates - and are measured
 *     like any other row.
 *   - '''A year outside 0000 to 9999 is refused.''' The lookup neither answers nor falls back: it
 *     fails fast through `ArgCheck`. The two `yearRange` rows, `GBLO` for year 10000 and year -1,
 *     carry no `holidays` and carry `error` instead, and every sample operation of them must
 *     refuse its argument.
 *
 * A refusal is observed through [[ParityHarness.attemptArgCheck]], which runs the call inside an
 * effect and turns the throw back into a value, so the rest of the row keeps being measured;
 * neither `intercept` nor `try`/`catch` appears in this file. Only the '''type''' of the refusal
 * is asserted, and the captured message is quoted in the diagnostic.
 *
 * ===Row schema===
 *
 * Each row is one (calendar, year) pair, which the model in the companion follows field for field:
 *
 * {{{
 * {"id":"gblo-2020","source":"generated","calendar":"GBLO","year":2020,
 *  "holidays":["2020-01-01","2020-01-04", …],
 *  "samples":[{"date":"2020-01-01","isHoliday":true,"isBusinessDay":false,
 *              "next":"2020-01-02","previous":"2019-12-31","nextOrSame":"2020-01-02",
 *              "previousOrSame":"2019-12-31","shift":{"amount":-3,"result":"2019-12-27"},
 *              "daysBetween":{"endExclusive":"2021-01-01","result":254},"error":null}, …],
 *  "error":null}
 * }}}
 *
 * The three samples sit at the fixed dates 1 January, 15 June and 24 December and carry the shift
 * amount and the end date they were captured with, so this spec replays them without knowing the
 * sampling rule.
 *
 * The '''row''' states the outcome and every sample of it states the same one: where the row
 * carries no `error`, all three samples carry no `error` and carry all eight of their expectations;
 * where the row carries `error`, all three carry `error` and none of the eight. A sample that
 * disagrees with its row, and a sample carrying some but not all of the eight, are fixtures that
 * no longer agree with this spec: they are reported as that, and the row is then '''not''' measured
 * at all, because which of the two measurements applies has become unclear. Keying the rule to the
 * row closes the gap a refused row full of populated samples would otherwise pass through - it
 * would be measured as a refusal and every captured expectation in it would go unread. Every
 * expectation a sample carries is measured, so `isBusinessDay`, `nextOrSame` and `previousOrSame`
 * are not left unread.
 *
 * `nextSameOrLastInMonth`, `lastBusinessDayOfMonth`, `isLastBusinessDayOfMonth`, `businessDays`,
 * `combinedWith` and `linkedWith` are unit behaviour owned by `date.HolidayCalendarSpec` and
 * `date.ImmutableHolidayCalendarSpec`, and the per-year tables and the Easter algorithm belong to
 * `date.GlobalHolidayCalendarsSpec`; this document covers the remaining years of the span.
 *
 * Because a report of rows that all matched cannot tell a complete measurement from a thinned one,
 * the second test asserts the population the baseline is required to carry as the '''exact''' set
 * of (`source`, calendar, year) triples, compared in both directions. Counts and contiguity are not
 * enough: a span shifted by a year, a fallback probe moved from 1949 to 1948 or a weekend row
 * recaptured for another year leaves every count and every contiguous run intact while retiring
 * exactly the boundary years the fallback and the bitmask ends are pinned by. See
 * [[HolidayCalendarParitySpec.RequiredPopulation]].
 */
class HolidayCalendarParitySpec extends AsyncFunSuite with AsyncIOSpec with Matchers {

  import HolidayCalendarParitySpec._

  test("the built-in holiday calendars reproduce the Java baseline exactly") {
    for {
      // The calendars are resolved once per distinct name for the whole run, and the built-in set
      // is lazily generated, so each calendar's rule generation is paid once per JVM. The cache is
      // an immutable map behind a reference created inside this effect: no `var`, nothing ambient.
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
        differences shouldBe empty
        // The set comparison is blind to multiplicity, so the count is asserted too: a
        // (source, calendar, year) captured twice under two identities leaves the set equal.
        rows.size shouldBe RequiredPopulation.size
        rows.map(_.id).distinct should have size rows.size.toLong
        // A row of unstated provenance is named as that rather than only as a row not required.
        rows.map(_.source).distinct.filterNot(RequiredSources.contains) shouldBe empty
        // Subsumed by the comparison above, and kept because a calendar that has vanished
        // entirely names itself here instead of being read off a list of its years.
        rows.map(_.calendar).distinct.toSet shouldBe RequiredCalendars
        // A row refuses if and only if it carries no holiday list: those are the two row kinds
        // measured differently, so the distinction is asserted rather than assumed. Only
        // identities are reported, because a row prints its whole year.
        rows.filter(_.error.isDefined).map(_.id) shouldBe RefusingRowIds
        rows.filter(row => row.error.isDefined != row.holidays.isEmpty).map(_.id) shouldBe empty
        rows.filter(_.samples.size != SamplesPerRow).map(_.id) shouldBe empty
        succeed
      }
    }
  }

  /*
   * The three tests below are about the decoding of the fixture. A report over rows that all
   * decoded cannot tell a fixture measured in full from one that has grown a key nothing reads,
   * so the schemas are asserted to be the key sets the models read and the refusals exercised.
   */

  test("the declared row, sample and probe key sets are the ones the models read") {
    IO {
      // Each schema is its model's own field set, so a field added to a model without being added
      // to its schema, or the reverse, fails here.
      RowSchema.known shouldBe DocumentedRowModel.productElementNames.toSet
      RowSchema.known.size shouldBe 7
      SampleSchema.known shouldBe DocumentedSampleModel.productElementNames.toSet
      SampleSchema.known.size shouldBe 10
      ShiftProbeSchema.known shouldBe DocumentedSampleModel.shift.productElementNames.toSet
      BetweenProbeSchema.known shouldBe DocumentedSampleModel.daysBetween.productElementNames.toSet
      // The documented shapes are those key sets, and every schema here has one variant and no
      // optional key, so satisfying it is equality of key sets.
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
      StrictRowDecoder.decodeJson(DocumentedRow) shouldBe Right(DocumentedRowModel)
      // A key the capture has started emitting: a derived decoder would ignore it and measure the
      // row as though the new expectation did not exist.
      refusalOf(
        StrictRowDecoder,
        withKey(DocumentedRow, "businessDays", Json.arr())) should include("unknown keys {businessDays}")
      // `holidays` is the key this spec reads as the difference between a row that answers and a
      // row that refuses, so its absence is refused rather than read as the other kind of row.
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
 * The row model of `holiday-baseline.json` and the checks applied to one row. It lives in the
 * companion because the JSON derivation needs the row types on a stable path.
 */
private[parity] object HolidayCalendarParitySpec {

  /** The fixture stem, which is also the `fixture` field of the report and its file name. */
  val FixtureName: String = "holiday"

  val FixtureResource: String = "parity/holiday-baseline.json"

  val SamplesPerRow: Int = 3

  /** The identity of one row within the population: its `source`, its calendar and its year. */
  type RowKey = (String, String, Int)

  // A row's `source` is its provenance: a rule generator over the calendar's own span, a published
  // data table, a probe one year outside that span where the calendar falls back to a weekend-only
  // test, one of the weekend and no-holiday calendars, a composite, or a refused year.

  val GeneratedSource: String = "generated"

  val DataTableSource: String = "dataTable"

  val OutOfRangeSource: String = "outOfRange"

  val WeekendSource: String = "weekend"

  val CompositeSource: String = "composite"

  val YearRangeSource: String = "yearRange"

  /**
   * Every rule-generated calendar, in the order `GlobalHolidayCalendars` builds them: twenty-four
   * cover 1950-2099 and `EUTA` covers 1997-2099. The split is stated once, in [[DatedSpans]], so
   * a calendar's span and the out-of-range probes either side of it cannot drift apart.
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

  val LateStartingCalendar: String = "EUTA"

  /** The four weekend and no-holiday calendars, which carry no dated span of their own. */
  val WeekendCalendars: Vector[String] = Vector("NoHolidays", "Sat/Sun", "Fri/Sat", "Thu/Fri")

  /** The three composite calendars, two combined and one linked. */
  val CompositeCalendars: Vector[String] = Vector("GBLO+USNY", "GBLO~USNY", "JPTO+USNY")

  val YearRangeCalendar: String = "GBLO"

  val GeneratedFirstYear: Int = 1950

  val GeneratedLastYear: Int = 2099

  val LateStartingFirstYear: Int = 1997

  val DataTableFirstYear: Int = 2005

  val DataTableLastYear: Int = 2079

  /**
   * The two years the weekend, no-holiday and composite calendars are measured over: they answer
   * for every year, so a span would be arbitrary. Both are leap years, one with 1 January on a
   * Wednesday and one on a Monday.
   */
  val ProbeYears: Vector[Int] = Vector(2020, 2024)

  /**
   * The two years outside 0000 to 9999 the calendar refuses to answer for, bracketing the accepted
   * range from both sides so that the refusal is a range check rather than an upper bound.
   */
  val YearRangeYears: Vector[Int] = Vector(-1, 10000)

  /**
   * One calendar's own span of years, inclusive of both endpoints, and the provenance the rows of
   * that span carry. The out-of-range probes are derived from the same two endpoints rather than
   * listed separately (see [[fallbackProbes]]), so a span that is widened, narrowed or shifted
   * moves its probes with it and leaves no stale pair behind.
   */
  final case class DatedSpan(source: String, calendar: String, firstYear: Int, lastYear: Int) {

    /** Every row the span itself requires, one per year from [[firstYear]] to [[lastYear]]. */
    def rows: Vector[RowKey] =
      (firstYear to lastYear).toVector.map(year => (source, calendar, year))

    /** The two rows that probe the weekend-only fallback, immediately below and above the span. */
    def fallbackProbes: Vector[RowKey] =
      Vector((OutOfRangeSource, calendar, firstYear - 1), (OutOfRangeSource, calendar, lastYear + 1))
  }

  /**
   * The span of every calendar that has one, generated or published: the twenty-four generators
   * over 1950-2099, `EUTA` over 1997-2099, and `THBA` over its 75 published years 2005-2079.
   */
  val DatedSpans: Vector[DatedSpan] =
    GeneratedCalendars.map { calendar =>
      val firstYear = if (calendar == LateStartingCalendar) LateStartingFirstYear else GeneratedFirstYear
      DatedSpan(GeneratedSource, calendar, firstYear, GeneratedLastYear)
    } :+ DatedSpan(DataTableSource, DataTableCalendar, DataTableFirstYear, DataTableLastYear)

  /**
   * Every (`source`, calendar, year) the baseline is required to carry, and nothing besides: the
   * dated spans, the fallback probe either side of each, the weekend and composite calendars in
   * [[ProbeYears]], and the two out-of-year-range refusals. Derived from the constants above, so
   * the contract is those endpoints rather than a row count. The document holds exactly these
   * 3,846 triples.
   */
  val RequiredPopulation: Set[RowKey] =
    (DatedSpans.flatMap(_.rows) ++
      DatedSpans.flatMap(_.fallbackProbes) ++
      WeekendCalendars.flatMap(calendar => ProbeYears.map(year => (WeekendSource, calendar, year))) ++
      CompositeCalendars.flatMap(calendar => ProbeYears.map(year => (CompositeSource, calendar, year))) ++
      YearRangeYears.map(year => (YearRangeSource, YearRangeCalendar, year))).toSet

  /** The closed set of `source` keys: a row whose `source` is not one of these six is unknown. */
  val RequiredSources: Set[String] = RequiredPopulation.map { case (source, _, _) => source }

  /** The 25 generated calendars, the published Thai table, the four weekend and the three composites. */
  val RequiredCalendars: Set[String] = RequiredPopulation.map { case (_, calendar, _) => calendar }

  /**
   * The two rows that record a refusal rather than an answer, named rather than derived so that a
   * fixture in which another row records an error, or one of these does not, is reported.
   */
  val RefusingRowIds: Vector[String] = Vector("gblo-10000-year-range", "gblo-minus1-year-range")

  /** Differing dates quoted in each direction; the message always reports the true totals. */
  val HolidayDifferenceLimit: Int = 10

  /** Rows named when the population differs; the message always reports the true totals. */
  val PopulationDifferenceLimit: Int = 12

  //-------------------------------------------------------------------------
  // The row model, with `Option` exactly where the document writes JSON `null`: a row that refuses
  // to answer carries no holiday list and no sample expectations, and carries `error` instead.
  // Each of the four shapes - the row, a sample, and the two probes nested in a sample - is
  // declared as a KeySchema beside the model it describes and checked against it before the object
  // is decoded: the row by `loadStrict`, the nested shapes by their own decoders, the only places
  // their keys are visible. Without that, derived decoding would read the declared fields and
  // ignore every other key, so a probe that started being emitted would be dropped in silence.
  // Declaring the keys is also what makes `Option` mean what the document says: the key must be
  // present, and `null` is then the one way it records a refusal.
  //-------------------------------------------------------------------------

  val ShiftKey: String = "shift"

  val DaysBetweenKey: String = "daysBetween"

  val SamplesKey: String = "samples"

  /** The shift a sample was captured with, by a signed number of business days, and its result. */
  final case class ShiftProbe(amount: Int, result: Option[LocalDate])

  /** The two keys [[ShiftProbe]] declares; a second operand here would change what was measured. */
  val ShiftProbeSchema: KeySchema =
    KeySchema.uniform("the shift probe of a holiday parity sample", Set("amount", "result"))

  /** The business-day count a sample was captured with, to an excluded end date, and its result. */
  final case class BetweenProbe(endExclusive: LocalDate, result: Option[Int])

  /** The two keys [[BetweenProbe]] declares; the end date is in the document so it is replayed. */
  val BetweenProbeSchema: KeySchema =
    KeySchema.uniform(
      "the days-between probe of a holiday parity sample",
      Set("endExclusive", "result"))

  /** One sampled date of a row, with every answer captured for it. */
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
   * The documented shape of one element of a row's `samples` list: the ten keys [[HolidaySample]]
   * declares. That closes the gap the `error` convention leaves open - a sample records either
   * every expectation or none - since an eleventh key, one more probe of the calendar and so an
   * expectation like any other, would be read by nothing while [[checkShape]] still agreed.
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

  /** One (calendar, year) row of the captured baseline. */
  final case class HolidayRow(
      id: String,
      source: String,
      calendar: String,
      year: Int,
      holidays: Option[Vector[LocalDate]],
      samples: Vector[HolidaySample],
      error: Option[String])
      extends ParityRow

  /** The documented shape of one row: the seven keys [[HolidayRow]] declares. */
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

  /** The row's own fields; not implicit, so the only reference is the strict composition below. */
  private val holidayRowFields: Decoder[HolidayRow] = deriveDecoder[HolidayRow]

  /** The row's fields behind the key check: the implicit a loader summons, strict by construction. */
  implicit val StrictRowDecoder: Decoder[HolidayRow] =
    ParityHarness.strictObject(RowSchema)(holidayRowFields)

  //-------------------------------------------------------------------------
  // The documented shapes as documents, and the three ways a document departs from one. A schema
  // exercised only by the fixture it already agrees with proves nothing about what it would
  // refuse, so the strictness tests decode these documents: the documented shapes, which must be
  // accepted and must decode to the models below, and then the same documents with one key added,
  // one removed and one renamed, each of which must be refused with the offending key named. The
  // accepted documents are the committed shapes, key for key - the baseline's first row and its
  // first sample - with the holiday and sample lists shortened to what a decode needs.
  //-------------------------------------------------------------------------

  val DocumentedShift: Json =
    Json.obj("amount" -> Json.fromInt(-3), "result" -> Json.fromString("1949-12-28"))

  val DocumentedDaysBetween: Json =
    Json.obj(
      "endExclusive" -> Json.fromString("1951-01-01"),
      "result" -> Json.fromInt(254))

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

  val DocumentedRow: Json =
    Json.obj(
      "id" -> Json.fromString("gblo-1950"),
      "source" -> Json.fromString("generated"),
      "calendar" -> Json.fromString("GBLO"),
      "year" -> Json.fromInt(1950),
      "holidays" -> Json.arr(Json.fromString("1950-01-01"), Json.fromString("1950-01-07")),
      SamplesKey -> Json.arr(DocumentedSample),
      "error" -> Json.Null)

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

  val DocumentedRowModel: HolidayRow =
    HolidayRow(
      id = "gblo-1950",
      source = "generated",
      calendar = "GBLO",
      year = 1950,
      holidays = Some(Vector(LocalDate.of(1950, 1, 1), LocalDate.of(1950, 1, 7))),
      samples = Vector(DocumentedSampleModel),
      error = None)

  /** The documented row carrying the given sample, which is where the nested objects live. */
  def rowWithSample(sample: Json): Json =
    withKey(DocumentedRow, SamplesKey, Json.arr(sample))

  /** The documented sample carrying the given probe, under [[ShiftKey]] or [[DaysBetweenKey]]. */
  def sampleWithProbe(key: String, probe: Json): Json = withKey(DocumentedSample, key, probe)

  /** The same object with one key added, which is the shape a newly captured field arrives in. */
  def withKey(document: Json, key: String, value: Json): Json =
    document.mapObject(fields => fields.add(key, value))

  /** The same object with one key removed, which is the shape a retired field leaves behind. */
  def withoutKey(document: Json, key: String): Json =
    document.mapObject(fields => fields.remove(key))

  /**
   * The same object with one key renamed, keeping its value - a rename is a loss and a gain at
   * once, and a schema has to report both halves for the message to say what happened.
   */
  def withRenamedKey(document: Json, from: String, to: String): Json =
    document.mapObject(fields => fields.remove(from).add(to, fields(from).getOrElse(Json.Null)))

  /**
   * The message a decoder refuses a document with. A decoder that '''accepts''' it answers with
   * what it accepted, so an assertion on the refusal fails naming the value that got through.
   */
  def refusalOf[A](decoder: Decoder[A], document: Json): String =
    decoder.decodeJson(document) match {
      case Left(failure) => failure.message
      case Right(value) => s"the decoder accepted $value"
    }

  /**
   * Resolves the calendar a row names, reusing the one already resolved for that name.
   *
   * `HolidayCalendarId.of` is total and accepts any name, composites joined with `'+'` or `'~'`
   * included; resolution against `ReferenceData.standard` is what can fail, and it does no
   * defaulting of unknown identifiers - that is `HolidaySafeReferenceData`'s behaviour and
   * `date.HolidaySafeReferenceDataSpec`'s subject - so an unresolvable name is a defect in the
   * implementation or the fixture rather than a parity result, and is raised through
   * [[ParityHarness.raise]]. Composite names resolve component by component.
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

  /**
   * Measures one row, answering with everything that differed. [[checkShape]] runs first, and
   * where it answers with anything neither [[checkAnsweredYear]] nor [[checkRefusedYear]] is
   * called. The resolved calendar's identifier is checked in every case, shape disagreement
   * included: a composite name normalises its parts, so it too is a captured expectation.
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
   * Checks the invariants of the row model itself, before anything is measured. Two things are
   * required of every sample, reported separately so a failure says which one broke:
   * '''alignment''', the sample records a refusal if and only if its row does, and
   * '''population''', the eight expectations - `isHoliday`, `isBusinessDay`, `next`, `previous`,
   * `nextOrSame`, `previousOrSame`, `shift.result` and `daysBetween.result` - all present when the
   * row answered and all absent when it refused. How many a disagreeing sample carried is part of
   * the message: half populated and fully populated on a refused row are different defects.
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

  /** Names an outcome for a fixture-disagreement message: an answer, or the refusal, quoted. */
  private def outcomeOf(error: Option[String]): String =
    error match {
      case Some(message) => s"a refusal ('$message')"
      case None => "an answer"
    }

  /**
   * Measures a row the calendar answers: the whole year, then each sample. The holiday list is
   * read through [[attempted]], so an absent list is reported as the fixture disagreement it is.
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
   * Measures a row the calendar refuses: the year through `isHoliday` on 1 January, the call its
   * `error` was recorded from, and each sample through all eight of its operations.
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

  /**
   * Compares all eight expectations of one answered sample, each independently of the others, so
   * that one raising call does not hide the rest. `daysBetween` is compared as the `Int` it is.
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
   * Requires all eight operations of one refused sample to refuse their argument. Each of them
   * reaches `isHoliday`, where the year is checked, so probing all eight keeps the precondition on
   * the whole surface the fixture covers.
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

  /** Every holiday of a calendar year, weekends included: one `isHoliday` call per day, ascending. */
  private def holidaysOfYear(calendar: HolidayCalendar, year: Int): Vector[LocalDate] = {
    val length = LocalDate.of(year, 1, 1).lengthOfYear()
    (1 to length).iterator
      .map(dayOfYear => LocalDate.ofYearDay(year, dayOfYear))
      .filter(date => calendar.isHoliday(date))
      .toVector
  }

  /**
   * Compares two holiday lists exactly, and renders a difference briefly: the same exact equality
   * [[ParityHarness.assertExact]] applies, order and multiplicity included, with only the message
   * differing. A count difference with an empty symmetric difference - a repeated or misordered
   * date - is reported through the counts.
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
   * Renders one direction of a population difference, or nothing when it is empty. Either
   * direction can be systematic, so the rows are ordered by source, calendar, year and bounded.
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
   * Compares one expectation of a sample. The call runs inside an effect and its outcome is turned
   * back into a value, so a refusal where the baseline answered is reported field by field.
   */
  private def expect[A](name: String, expected: Option[A])(thunk: => A): IO[List[String]] =
    attempted(name, expected)(thunk)((actual, value) => ParityHarness.assertExact(name, actual, value))

  /**
   * Evaluates one measurement and applies a comparison to its result - the common shape of
   * [[expect]] and of the holiday-list comparison. An absent expectation is a fixture
   * disagreement, a raised error is a discrepancy naming it, and a value is handed to `compare`.
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
