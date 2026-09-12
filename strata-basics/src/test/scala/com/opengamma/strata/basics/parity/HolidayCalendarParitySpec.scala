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
 * the sampling rule. A sample either carries a complete set of expectations or carries `error`
 * with every expectation absent; a half-populated sample is a fixture that no longer agrees with
 * this spec and is reported as such rather than measured. Every expectation a sample carries is
 * measured: they were all captured from Java, so leaving `isBusinessDay`, `nextOrSame` or
 * `previousOrSame` unread would discard captured baseline rather than avoid re-deriving anything.
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
 * a thinned one, the second test below asserts the population the baseline is required to carry.
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
      report <- ParityHarness.runFixture[HolidayRow](FixtureName, FixtureResource)(row =>
        checkRow(calendars, row))
      _ <- ParityHarness.failIfAny(report)
    } yield succeed
  }

  test("the fixture carries the population the holiday baseline is required to measure") {
    ParityHarness.load[HolidayRow](FixtureResource).map { rows =>
      val counts = rows.groupBy(_.source).view.mapValues(_.size).toMap
      withClue(
        s"fixture rows: ${rows.size}; rows by source: " +
          s"${counts.toVector.sortBy(_._1).mkString(", ")}: ") {
        // The floors are the contract of the capture, restated on the consuming side so that a
        // reduced fixture fails here instead of reporting a green measurement of less. They are
        // floors rather than equalities so that extending the coverage stays possible.
        rows.size should be >= MinimumRows
        rows.map(_.id).distinct should have size rows.size.toLong
        MinimumRowsBySource.foreach { case (source, minimum) =>
          withClue(s"source '$source': ") {
            counts.getOrElse(source, 0) should be >= minimum
          }
        }
        // A row of unstated provenance is reported rather than counted towards a floor it does
        // not belong to.
        rows.map(_.source).distinct.filterNot(MinimumRowsBySource.contains) shouldBe empty
        // Every calendar the port is required to reproduce appears, including the three
        // composites and the four weekend and no-holiday calendars.
        val named = rows.map(_.calendar).distinct.toSet
        RequiredCalendars.filterNot(named.contains) shouldBe empty
        // Each calendar's dated rows cover a contiguous span of years, so thinning the middle of
        // a span cannot pass while the row-count floor still holds.
        rows.groupBy(_.calendar).foreach { case (calendar, calendarRows) =>
          val years = calendarRows.filter(row => DatedSources.contains(row.source)).map(_.year).sorted
          withClue(s"calendar '$calendar' covers years ${years.headOption} to ${years.lastOption}: ") {
            years.distinct should have size years.size.toLong
            years.zip(years.drop(1)).filterNot { case (earlier, later) => later == earlier + 1 } shouldBe empty
          }
        }
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
   * The least number of rows the baseline is worth measuring.
   *
   * The committed document holds 3,846: one per (calendar, year) over the whole generated span.
   * Section 6 of `tools/parity-capture/README.md` records that count and states that the coverage
   * is not negotiable, because the per-year span is the only thing that pins the monthly-bitmask
   * representation across every year the calendars cover.
   */
  val MinimumRows: Int = 3846

  /**
   * The least number of rows of each captured population, keyed by the row's `source`.
   *
   * The key set is closed: a row whose `source` is not one of these five is a fixture this spec
   * does not know how to account for, and is reported rather than measured silently.
   */
  val MinimumRowsBySource: Map[String, Int] =
    Map(
      // the 24 calendars over 1950-2099 and EUTA over 1997-2099
      "generated" -> 3703,
      // THBA over its published range, 2005-2079
      "dataTable" -> 75,
      // the year either side of each calendar's range, where Java falls back to weekends
      "outOfRange" -> 52,
      // the four weekend and no-holiday calendars, for 2020 and 2024
      "weekend" -> 8,
      // GBLO+USNY, GBLO~USNY and JPTO+USNY, for 2020 and 2024
      "composite" -> 6,
      // GBLO for year 10000 and year -1, the two rows that refuse to answer
      "yearRange" -> 2
    )

  /** The sources whose rows name a year of a calendar's own span, which must be contiguous. */
  val DatedSources: Set[String] = Set("generated", "dataTable")

  /**
   * Every calendar name the baseline is required to measure.
   *
   * The 25 generated calendars and the published Thai calendar, the four weekend and no-holiday
   * calendars, and the three composites - AAP section 0.6.2's covered set, which
   * `SCALA_MIGRATION.md` documents as the ported calendar set.
   */
  val RequiredCalendars: Vector[String] =
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
      "ZAJO",
      "THBA",
      "NoHolidays",
      "Sat/Sun",
      "Fri/Sat",
      "Thu/Fri",
      "GBLO+USNY",
      "GBLO~USNY",
      "JPTO+USNY"
    )

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

  //-------------------------------------------------------------------------
  // The row model. Field for field the schema of section 6 of `tools/parity-capture/README.md`,
  // with `Option` exactly where that schema writes JSON `null`: a row that refuses to answer
  // carries no holiday list and no sample expectations, and carries `error` instead.
  //-------------------------------------------------------------------------

  /**
   * The shift a sample was captured with, and what it produced.
   *
   * @param amount  the number of business days the date was shifted by, which may be negative
   * @param result  the shifted date, absent where the calendar refused to answer
   */
  final case class ShiftProbe(amount: Int, result: Option[LocalDate])

  /**
   * The business-day count a sample was captured with, and what it produced.
   *
   * @param endExclusive  the end of the counted range, excluded from the count
   * @param result  the number of business days in the range, absent where the calendar refused
   */
  final case class BetweenProbe(endExclusive: LocalDate, result: Option[Int])

  /**
   * One sampled date of a row, with every answer the Java implementation gave for it.
   *
   * Either every expectation is present and `error` is absent, or every expectation is absent and
   * `error` carries the message Java produced. [[checkSample]] and [[checkRefusedSample]] are the
   * two readings of that, and [[HolidayRow]] decides between them from the row.
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
   * One (calendar, year) row of the captured baseline.
   *
   * @param id  the identity of the row, unique across the document and what the report names
   * @param source  the population the row belongs to, one of the keys of [[MinimumRowsBySource]]
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

  implicit val shiftProbeDecoder: Decoder[ShiftProbe] = deriveDecoder[ShiftProbe]

  implicit val betweenProbeDecoder: Decoder[BetweenProbe] = deriveDecoder[BetweenProbe]

  implicit val holidaySampleDecoder: Decoder[HolidaySample] = deriveDecoder[HolidaySample]

  implicit val holidayRowDecoder: Decoder[HolidayRow] = deriveDecoder[HolidayRow]

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
   * The row decides which of the two measurements applies. A row carrying `error` records that
   * Java refused to answer for the year at all, so every operation of the row is required to
   * refuse; any other row carries a holiday list and three fully populated samples, and every
   * expectation in it is compared. The identifier of the resolved calendar is checked in both
   * cases: a composite name normalises its parts, so the name the port produces is itself a
   * captured expectation.
   *
   * @param cache  the calendars resolved so far in this run
   * @param row  the row to measure
   * @return every discrepancy found in the row, empty where it matched in every respect
   */
  def checkRow(cache: Ref[IO, Map[String, HolidayCalendar]], row: HolidayRow): IO[List[String]] =
    calendarFor(cache, row.calendar).flatMap { calendar =>
      val name = ParityHarness.assertExact("calendar name", calendar.name, row.calendar)
      val shape = checkShape(row)
      val body =
        if (row.error.isDefined) checkRefusedYear(calendar, row) else checkAnsweredYear(calendar, row)
      body.map(messages => name ::: shape ::: messages)
    }

  /**
   * Checks the invariants of the row model itself, before anything is measured.
   *
   * These are properties of the document rather than of the port: a row that no longer carries
   * three samples, or that carries a half-populated sample, or that carries both an error and a
   * holiday list, is a fixture that has stopped agreeing with this spec. Reporting that as what it
   * is keeps it from being read as a defect of the port, and keeps it from being absorbed silently
   * by a check that simply finds nothing to compare.
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
      val expectedPopulated = sample.error.isEmpty
      if (populated.forall(_ == expectedPopulated)) {
        Nil
      } else {
        List(
          s"fixture disagreement: ${label(sample, index)} carries error=${sample.error} with " +
            s"${populated.count(identity)} of ${populated.size} expectations present, and a " +
            "sample must carry either all of them or none")
      }
    }
    sampleCount ::: exclusive ::: samples.toList
  }

  /**
   * Measures a row the calendar answers: the whole year, then each sample.
   *
   * @param calendar  the resolved calendar
   * @param row  the row to measure
   * @return every discrepancy found
   */
  private def checkAnsweredYear(calendar: HolidayCalendar, row: HolidayRow): IO[List[String]] = {
    val holidays = row.holidays match {
      case expected @ Some(_) =>
        attempted("holidays", expected)(holidaysOfYear(calendar, row.year))(compareHolidays(_, _))
      case None =>
        // The shape check has already reported this; measuring nothing here keeps one fixture
        // defect to one message.
        IO.pure(List.empty[String])
    }
    for {
      year <- holidays
      samples <- row.samples.toList.zipWithIndex.traverse { case (sample, index) =>
        checkSample(calendar, sample, index)
      }
    } yield year ::: samples.flatten
  }

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
