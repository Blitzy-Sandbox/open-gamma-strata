/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.parity

import java.time.LocalDate

import cats.data.EitherNec
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all._

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.date.AdjustableDate
import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.basics.date.BusinessDayConvention
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.basics.schedule.Frequency
import com.opengamma.strata.basics.schedule.PeriodicSchedule
import com.opengamma.strata.basics.schedule.RollConvention
import com.opengamma.strata.basics.schedule.Schedule
import com.opengamma.strata.basics.schedule.SchedulePeriod
import com.opengamma.strata.basics.schedule.StubConvention
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason

/**
 * Parity of periodic schedule generation against the Java baseline.
 *
 * This is the measurement that pins the schedule layer of the port. Each captured row names a
 * `PeriodicSchedule` definition by its eleven fields; this spec rebuilds that definition through
 * the port's validating factory, resolves it against `ReferenceData.standard`, and compares the
 * unadjusted dates, the adjusted dates, every period, both stubs, the resolved roll convention and
 * the resolved frequency against what the Java implementation produced for the same definition.
 * The eleven `data_replace` rows additionally reproduce the whole `replaceStartDate` operation: the
 * post-replacement definition field by field, the unadjusted dates that definition creates, and the
 * schedule it resolves to.
 *
 * The gate that consumes it (AAP section 0.10.1, Gate 3, for the user's Rule 2) runs
 *
 * {{{
 * sbt -batch "testOnly *ParitySpec"
 * }}}
 *
 * and then reads `<parity.report.dir>/schedule.json`, requiring `failed == 0` and copying `rows`
 * and `passed` into `target/gate-report.md`. The `ParitySpec` suffix of this class, its package,
 * the fixture stem `schedule` and the five keys of the report document are therefore all part of
 * that contract and none of them may drift: renaming the class removes this measurement from the
 * gate without failing anything.
 *
 * ===Everything here compares exactly===
 *
 * A schedule is dates, names and structure - there is not one numeric expectation in this fixture -
 * so every comparison goes through [[ParityHarness.assertExact]] and no tolerance appears anywhere
 * in this file. Reaching for [[ParityHarness.assertParity]] on a date would be the wrong comparator
 * for the wrong kind of value (AAP section 0.6.1: date, list and string expectations compare
 * exactly).
 *
 * ===Why the date lists are read off the resolved schedule===
 *
 * The capture derived `unadjustedDates`, `adjustedDates`, `periods`, `initialStub` and `finalStub`
 * from the periods of one resolved `Schedule`
 * (`tools/parity-capture/capture-baseline.jsh`, `setResolvedScheduleExpectations`), which is what
 * makes the four structural identities of the document - `periods[i].unadjustedStart ==
 * unadjustedDates[i]` and its three siblings - hold by construction. This spec reproduces that
 * derivation rather than substituting `createUnadjustedDates()`, because in Java those are not the
 * same answer: on `data_replace` row 2 the no-reference-data form returns a long initial stub
 * beginning at the replacement date while `createSchedule` rolls the start onto the 17th
 * (capture README, section 6). Both are correct Java answers for one definition, the fixture
 * carries each under its own key, and the port has to reproduce both.
 *
 * The two reference-data forms are measured as well, against the same captured lists, because the
 * Java tests do assert that identity for every `data_generation` row -
 * `createUnadjustedDates(REF_DATA)` at
 * `modules/basics/src/test/java/com/opengamma/strata/basics/schedule/PeriodicScheduleTest.java:737`
 * and `createAdjustedDates(REF_DATA)` at `:812` - so holding the port to it is reproducing a
 * stated Java expectation rather than inventing one. The no-reference-data form is measured under
 * the guard that Java itself applies to it (`:740`: only where the start date carries no business
 * day adjustment of its own and the declared roll convention is not `EOM`, because the pre-adjusted
 * start-date recovery that the other form performs needs the holiday calendars).
 *
 * ===The composite calendars are the ordinary case here, not an edge case===
 *
 * 256 of the 879 rows adjust against `GBLO+USNY`. `ReferenceData.standard` holds no composite
 * entry, so every one of those rows exercises the fallback path of `HolidayCalendarId.resolve`:
 * the full composite name is looked up first, and only when it is absent are the parts resolved
 * and combined
 * (`modules/basics/src/main/java/com/opengamma/strata/basics/date/HolidayCalendarId.java:123-142`).
 * The identifier is built by handing the captured name straight to `HolidayCalendarId.of`, which
 * parses `'+'` and `'~'` and normalises, deduplicates and sorts the parts; splitting a composite
 * name here would be reimplementing that and would stop measuring it.
 *
 * ===The three calendar-bearing roll conventions===
 *
 * In Java, `IMMCAD`, `IMMAUD` and `TBILL` capture built-in holiday calendars at
 * class-initialisation time through the static `ReferenceData.standard` accessor
 * (`StandardRollConventions.java:60-63,74-75,103-104,133-134` - `IMMCAD` holds `GBLO` and
 * `CATO.combinedWith(CAMO)`, `IMMAUD` holds `AUSY`, `TBILL` holds `USNY`), whereas the port binds
 * the `StandardHolidayCalendars` constants directly as data (AAP sections 0.3.3 and 0.6.5). The
 * generated grid puts each of the three through 96 rows - 4 frequencies x 8 stub conventions x 3
 * calendars - with `SFE` and `IMMNZD`, which use no calendar, as the control group alongside them.
 *
 * What those rows do and do not establish is worth being exact about, because the grid's span,
 * 2015-01-15 to 2018-01-15, does not begin or end on a date any of the four IMM conventions rolls
 * to:
 *
 *   - '''`IMMCAD` resolves 36 of its 96 rows''', twelve against each of the three calendars, and
 *     every date in them was produced by `IMMCAD.adjust` - the third Wednesday moved by the `GBLO`
 *     and `CATO`+`CAMO` calendars it holds. Those dates are the date-level evidence that binding
 *     the constants directly reproduces what the class-initialisation lookup produced.
 *   - '''`IMMAUD`, `TBILL`, `IMMNZD` and `SFE` are refused in all 96 rows each''', because the
 *     start date does not match the convention rolling forwards and the end date does not match it
 *     rolling backwards. A refusal is still a measurement of `adjust`: the port must move those two
 *     dates, and a convention that left them where they were would produce a schedule where Java
 *     refused one, which this spec reports. It does not, on its own, discriminate '''which'''
 *     calendar the convention holds - `TBILL.adjust(2015-01-15)` leaves the date either way.
 *   - The probes that do discriminate the calendar are the 51 rows of the Java `data_adjust`
 *     provider, including the two whose Java comment reads "Tuesday due to holiday" - `TBILL`
 *     taking 2018-08-31 and 2018-09-01 both to 2018-09-04, which only a `USNY` holiday produces.
 *     They are ported in `com.opengamma.strata.basics.schedule.RollConventionSpec`, where AAP
 *     section 0.4.1 places them, and this fixture carries no direct `adjust(date)` probe at all, so
 *     nothing is added here that would duplicate them. `RollConvention.adjust(date)` keeps Java's
 *     signature precisely because those calendars are constants, so there is no reference data to
 *     thread through it.
 *
 * The population test pins each half of that: `IMMCAD` must keep its resolved rows over all three
 * calendars, and the refusal-only members must keep their refusals.
 *
 * ===An error is an expectation===
 *
 * 573 rows record a refusal, and the port must refuse the same definitions. Every one of them is
 * checked through [[ParityHarness.assertLeft]] on the error channel - there is no `intercept`, no
 * `try`/`catch` and no `.get` on an `Either` in this file - and then for its shape, because a
 * refusal for the wrong reason is not parity. The port replaces Java's `ScheduleException` with
 * `Failure.Invalid` carrying the rejected definition under a `definition` attribute
 * (`PeriodicSchedule.scala`, `failure`), so `reason` is required to be
 * [[com.opengamma.strata.collect.result.FailureReason.INVALID]] for every failure and the
 * attribute is required of every failure that a '''generation''' step produced.
 *
 * It is required of those and not of the others because of where the attribute comes from. A
 * definition the factory itself refuses - eight rows, all `Invalid order: ...` - never becomes a
 * `PeriodicSchedule`, so there is no definition to attach: those failures come from `Validate`'s
 * order checks and carry no attributes at all. Which stage refused a row is therefore read from
 * the port's own outcome rather than guessed from the row's `source`, and the fixture shows why
 * that matters: `PeriodicScheduleTest.invalid.termWithExplicitStub` looks like a sibling of the
 * seven builder-validation rows but is refused by generation, and so does carry the attribute.
 *
 * The captured `error` text is '''never''' asserted on. Exception-to-`Failure` is a deliberate
 * divergence (AAP section 0.8.3), so the port's wording is its own; the Java message is quoted in
 * a diagnostic, where it helps a reader, and nowhere else.
 *
 * ===No timing===
 *
 * Parity is a value comparison. Nothing here, and nothing in the report it publishes, asserts a
 * duration. The cost of 879 resolutions is controlled structurally instead: `ReferenceData.standard`
 * is resolved once for the whole run, and the built-in calendars behind it are lazily generated, so
 * the rule generation of each calendar is paid once per JVM however many rows name it.
 */
class ScheduleParitySpec extends AsyncFunSuite with AsyncIOSpec with Matchers {

  import ScheduleParitySpec._

  /*
   * The measurement is declared first, deliberately: the report it publishes is the artifact
   * Gate 3 collects, so it is written on every run of this suite rather than only on the runs
   * where some other case happens to pass first. `runFixture` writes it before returning, and
   * `failIfAny` is what then decides the verdict, so the counts survive a failing run.
   */
  test("periodic schedule generation reproduces the Java baseline exactly") {
    ParityHarness
      .runFixture[ScheduleRow](FixtureName, FixtureResource)(checkRow)
      .flatMap(report => ParityHarness.failIfAny(report))
      .as(succeed)
  }

  test("the fixture carries the population the schedule baseline is required to measure") {
    ParityHarness.load[ScheduleRow](FixtureResource).map { rows =>
      val bySource = rows.groupBy(_.source).view.mapValues(_.size).toMap
      withClue(
        s"fixture rows: ${rows.size}; rows by source: " +
          s"${bySource.toVector.sortBy(_._1).mkString(", ")}: ") {
        // The floors are the contract of the capture, restated on the consuming side so that a
        // reduced fixture fails here instead of reporting a green measurement of less. They are
        // floors rather than equalities so that extending the coverage stays possible.
        rows.size should be >= MinimumRows
        // This fixture is the one with no `id` field, so a row's identity is derived - from its
        // source, its eleven inputs and its replacement date. Asserting distinctness makes that
        // derivation a checked property: without the replacement date two `data_replace` rows
        // share an identity and a report could not say which of them failed.
        rows.map(_.id).distinct should have size rows.size.toLong
        MinimumRowsBySource.foreach { case (source, minimum) =>
          withClue(s"source '$source': ") {
            bySource.getOrElse(source, 0) should be >= minimum
          }
        }
        // A row of unstated provenance is reported rather than counted towards a floor it does
        // not belong to.
        rows.map(_.source).distinct.filterNot(MinimumRowsBySource.contains) shouldBe empty
        // Exactly one of a refusal and the full expectation set, which is the document's rule
        // (capture README, section 6). These are the two row kinds this spec measures
        // differently, so the distinction is asserted rather than assumed: a row carrying
        // neither would measure nothing at all, and a row carrying some of the seven
        // expectation keys would measure less than it appears to.
        rows.filter(row => row.error.isDefined == row.isResolved).map(_.id) shouldBe empty
        rows.filter(_.isPartiallyResolved).map(_.id) shouldBe empty
        rows.count(_.error.isDefined) should be >= MinimumErrorRows
        rows.count(_.isResolved) should be >= MinimumResolvedRows
        // The four structural identities of a resolved row, which the capture guarantees by
        // construction. A row that broke one would make the three date expectations disagree
        // with each other, and this spec would then report a discrepancy of the port for a
        // defect of the fixture.
        rows.filterNot(_.isStructurallyConsistent).map(_.id) shouldBe empty
        // Every calendar shape the fixture is required to exercise, including the composite one
        // whose 256 rows are the only coverage of resolving `GBLO+USNY` from its parts.
        val calendars = rows.map(_.definition.businessDayAdjustment.calendar).toSet
        RequiredCalendars.filterNot(calendars.contains) shouldBe empty
        rows.count(_.definition.businessDayAdjustment.calendar == CompositeCalendar) should
          be >= MinimumCompositeCalendarRows
        // The generated grid, roll convention by roll convention and over all three calendars
        // each. `IMMCAD`, `IMMAUD` and `TBILL` are the three whose Java originals captured
        // built-in calendars at class-initialisation time and which the port binds directly, so
        // thinning any of them out would retire the evidence that the substitution preserves
        // behaviour; `SFE` and `IMMNZD` use no calendar and are the control group.
        GridRollConventions.foreach { convention =>
          withClue(s"grid rows for roll convention '$convention': ") {
            val forConvention = rows.filter(row =>
              row.source == GridSource && row.definition.rollConvention.contains(convention))
            forConvention.size should be >= MinimumRowsPerGridRollConvention
            val forConventionCalendars =
              forConvention.map(_.definition.businessDayAdjustment.calendar).toSet
            GridCalendars.filterNot(forConventionCalendars.contains) shouldBe empty
            // The two halves of the evidence, each pinned where the fixture provides it. A
            // convention that resolves must keep resolving, over all three calendars, because
            // those rows are the only ones whose generated dates were produced by the calendars
            // the convention holds; a convention this span refuses must keep refusing, because a
            // port that resolved one of those rows would be disagreeing with Java about the roll.
            MinimumResolvedGridRows.get(convention).foreach { minimum =>
              val resolved = forConvention.filter(_.isResolved)
              withClue(s"resolved grid rows for '$convention': ") {
                resolved.size should be >= minimum
                val resolvedCalendars =
                  resolved.map(_.definition.businessDayAdjustment.calendar).toSet
                GridCalendars.filterNot(resolvedCalendars.contains) shouldBe empty
              }
            }
            MinimumRefusedGridRows.get(convention).foreach { minimum =>
              withClue(s"refused grid rows for '$convention': ") {
                forConvention.count(_.error.isDefined) should be >= minimum
              }
            }
            // Every grid roll convention is accounted for by one of the two maps above, so a
            // convention whose outcome changed wholesale cannot slip through unmeasured.
            withClue(s"outcome floor stated for '$convention': ") {
              (MinimumResolvedGridRows.contains(convention) ||
                MinimumRefusedGridRows.contains(convention)) shouldBe true
            }
          }
        }
        // The `data_replace` family, whose rows model an operation rather than a resolution. The
        // replacement date and the source identify the same rows, a resolved one carries both of
        // its own expectations, a rejected one carries neither, and at least one row is rejected -
        // that being the only coverage of `replaceStartDate` refusing a date after the end date.
        val replaceRows = rows.filter(_.replacedStartDate.isDefined)
        rows.filter(row => (row.source == ReplaceSource) != row.replacedStartDate.isDefined)
          .map(_.id) shouldBe empty
        replaceRows.size should be >= MinimumReplaceRows
        replaceRows.count(_.error.isDefined) should be >= MinimumReplaceRefusals
        replaceRows
          .filter(row =>
            row.error.isEmpty && (row.replacedDefinition.isEmpty || row.replacedUnadjustedDates.isEmpty))
          .map(_.id) shouldBe empty
        replaceRows.filter(row => row.error.isDefined && row.replacedDefinition.isDefined)
          .map(_.id) shouldBe empty
        // A resolved replacement row declares all five of its own keys. This is checked on the key
        // set rather than on the values because three of the five are legitimately null - a
        // replaced definition need declare no roll convention - so a renamed or dropped key would
        // otherwise read as "the replacement declares none", which every one of those comparisons
        // would then pass without measuring anything.
        replaceRows
          .filter(row => row.error.isEmpty && !ReplaceExpectationKeys.subsetOf(row.declaredKeys))
          .map(_.id) shouldBe empty
        // And a refused one declares none of them, so the two shapes stay distinct.
        replaceRows
          .filter(row =>
            row.error.isDefined && ReplaceExpectationKeys.intersect(row.declaredKeys).nonEmpty)
          .map(_.id) shouldBe empty
        succeed
      }
    }
  }
}

/**
 * The row model of `schedule-baseline.json`, the definition it encodes, and the checks applied to
 * one row.
 *
 * It lives in the companion rather than in the suite because the JSON derivation needs the row
 * types on a stable path, and because keeping the measurement out of the suite body makes it plain
 * that the suite contributes nothing to the measurement beyond ordering it. Everything here is
 * confined to this package.
 *
 * No `Double` appears in any row of this fixture - a schedule is dates, names and structure - so
 * the tagged non-finite double policy of `com.opengamma.strata.collect.json.Codecs` is not needed
 * here and is deliberately not imported: an import that nothing uses would fail the warning-clean
 * build. The `java.time.LocalDate` instances circe publishes in its own companion read the
 * ISO-8601 dates of the document, which is the form the capture writes.
 */
private[parity] object ScheduleParitySpec {

  //-------------------------------------------------------------------------
  // Contract constants. The first two are agreements with something outside this file - the
  // resource name with the capture script that writes it, the fixture stem with the gate script
  // that reads `<parity.report.dir>/schedule.json` - so neither may drift.
  //-------------------------------------------------------------------------

  /** The fixture stem, which is also the `fixture` field of the report and its file name. */
  val FixtureName: String = "schedule"

  /** The classpath name of the captured baseline, relative to the test resource root. */
  val FixtureResource: String = "parity/schedule-baseline.json"

  /**
   * The attribute under which a rejected definition rides on its failure.
   *
   * The exception of the library being ported carried the definition it rejected as a field; the
   * port attaches the same text as an attribute of `Failure.Invalid`, and this is the key it uses
   * (`PeriodicSchedule.scala`, `DefinitionAttribute`).
   */
  val DefinitionAttribute: String = "definition"

  /** The `source` of the generated combination grid, which is 768 of the 879 rows. */
  val GridSource: String = "grid.combinations"

  /** The `source` of the `replaceStartDate` rows. */
  val ReplaceSource: String = "PeriodicScheduleTest.data_replace"

  /** The composite calendar the grid adjusts a third of its rows against. */
  val CompositeCalendar: String = "GBLO+USNY"

  /** The number of rows the committed fixture holds. */
  val MinimumRows: Int = 879

  /** The number of rows that record a refusal. */
  val MinimumErrorRows: Int = 573

  /** The number of rows that record a resolved schedule. */
  val MinimumResolvedRows: Int = 306

  /** The number of `data_replace` rows. */
  val MinimumReplaceRows: Int = 11

  /** The number of `data_replace` rows whose replacement date is refused. */
  val MinimumReplaceRefusals: Int = 1

  /** The number of rows adjusting against the composite calendar. */
  val MinimumCompositeCalendarRows: Int = 256

  /** The number of grid rows each roll convention of the grid appears in. */
  val MinimumRowsPerGridRollConvention: Int = 96

  /**
   * Every `source` the fixture carries, with the number of rows it is required to contribute.
   *
   * The three populations of AAP section 0.6.1 - the two Java tables and the generated grid - plus
   * the thirteen named feature rows, which are the only coverage of the three inputs the tables and
   * the grid never populate: the end-date business day adjustment, the override start date, and the
   * factory's own order invariants. A source absent from this map is reported by the population
   * test rather than silently counted.
   */
  val MinimumRowsBySource: Map[String, Int] = Map(
    GridSource -> 768,
    "PeriodicScheduleTest.data_generation" -> 87,
    ReplaceSource -> MinimumReplaceRows,
    "PeriodicScheduleTest.test_startEndAdjust" -> 1,
    "PeriodicScheduleTest.test_firstPaymentDate_before_effectiveDate" -> 1,
    "PeriodicScheduleTest.test_override_fallbackWhenStartDateMismatch" -> 1,
    "PeriodicScheduleTest.test_override_fallbackWhenStartDateMismatchEndStub" -> 1,
    "PeriodicScheduleTest.coverage_builder" -> 1,
    "PeriodicScheduleTest.invalid.startAfterEnd" -> 1,
    "PeriodicScheduleTest.invalid.startEqualsEnd" -> 1,
    "PeriodicScheduleTest.invalid.overrideStartAfterEnd" -> 1,
    "PeriodicScheduleTest.invalid.firstRegularAfterEnd" -> 1,
    "PeriodicScheduleTest.invalid.firstRegularBeforeStart" -> 1,
    "PeriodicScheduleTest.invalid.lastRegularBeforeFirstRegular" -> 1,
    "PeriodicScheduleTest.invalid.firstRegularWithOverride" -> 1,
    "PeriodicScheduleTest.invalid.termWithExplicitStub" -> 1)

  /**
   * The eight roll conventions of the generated grid.
   *
   * `IMMCAD`, `IMMAUD` and `TBILL` are the three that hold built-in calendars, and `SFE` and
   * `IMMNZD` - which hold none - are the control group they are compared against.
   */
  val GridRollConventions: List[String] =
    List("EOM", "IMM", "IMMCAD", "IMMAUD", "IMMNZD", "SFE", "TBILL", "Day15")

  /** The three calendars each roll convention of the grid is exercised against. */
  val GridCalendars: List[String] = List("GBLO", "USNY", CompositeCalendar)

  /**
   * The grid roll conventions whose combinations resolve, with the number of rows each resolves.
   *
   * `IMMCAD` is the member of the calendar-bearing family that appears here, and its 36 rows -
   * twelve per calendar - are the only place in this fixture where dates produced through a
   * convention's own holiday calendars are compared against Java's.
   */
  val MinimumResolvedGridRows: Map[String, Int] = Map("EOM" -> 84, "IMMCAD" -> 36, "Day15" -> 84)

  /**
   * The grid roll conventions the grid's span refuses outright, with the number of refusals each
   * contributes.
   *
   * Neither 2015-01-15 nor 2018-01-15 is a date these conventions roll to, so every combination
   * is refused - which is itself a measurement of the port's `adjust`, since a convention that
   * left either date unmoved would resolve where Java refused.
   */
  val MinimumRefusedGridRows: Map[String, Int] =
    Map("IMM" -> 96, "IMMAUD" -> 96, "IMMNZD" -> 96, "SFE" -> 96, "TBILL" -> 96)

  /** Every calendar the fixture is required to name somewhere. */
  val RequiredCalendars: List[String] =
    List("GBLO", "USNY", CompositeCalendar, "Sat/Sun", "NoHolidays", "JPTO")

  /**
   * The reference data every resolution in this fixture was captured against.
   *
   * Resolved once for the whole suite. `ReferenceData.standard` is a lazy value rather than a
   * method, so the thirty built-in calendars behind it are generated on first use and then shared
   * by all 879 rows; binding it here makes that explicit and keeps the per-row code free of it.
   */
  val referenceData: ReferenceData = ReferenceData.standard

  //-------------------------------------------------------------------------
  // The row model.
  //
  // Every field name below is the key the capture writes, and every nesting level is the one it
  // writes it at (`tools/parity-capture/README.md`, section 6, is the schema of record). The
  // fixture is a read-only artifact: where the two disagree it is this decoder that is wrong.
  //-------------------------------------------------------------------------

  /**
   * A business day adjustment as the fixture carries it: a convention name and a calendar name.
   *
   * The calendar name may be composite - `GBLO+USNY` - and is handed to `HolidayCalendarId.of`
   * whole. The adjustment the library uses for "no adjustment at all" is written like any other,
   * as `NoAdjust` over `NoHolidays`, which is exactly `BusinessDayAdjustment.NONE`.
   *
   * @param convention  the business day convention's name
   * @param calendar  the holiday calendar identifier's name
   */
  final case class BusinessDayAdjustmentRow(convention: String, calendar: String) {

    /** The text this adjustment is named by in a diagnostic. */
    def render: String = s"$convention/$calendar"
  }

  /**
   * An adjustable date as the fixture carries it.
   *
   * The adjustment is modelled as optional because the capture renders an absent one as JSON null
   * (`jAdjustableDate`). The Java type it comes from defaults that property to
   * `BusinessDayAdjustment.NONE` rather than leaving it unset, so an absent adjustment is read as
   * that constant - the same value, arrived at from the other direction.
   *
   * @param unadjusted  the date before adjustment
   * @param adjustment  the adjustment to apply, where the capture recorded one
   */
  final case class AdjustableDateRow(
      unadjusted: LocalDate,
      adjustment: Option[BusinessDayAdjustmentRow]) {

    /** The text this date is named by in a diagnostic. */
    def render: String = s"$unadjusted${adjustment.fold("")(value => s"[${value.render}]")}"
  }

  /**
   * One schedule period as the fixture carries it: the two unadjusted dates and the two adjusted.
   *
   * @param unadjustedStart  the unadjusted start date of the period
   * @param unadjustedEnd  the unadjusted end date of the period
   * @param start  the adjusted start date of the period
   * @param end  the adjusted end date of the period
   */
  final case class PeriodRow(
      unadjustedStart: LocalDate,
      unadjustedEnd: LocalDate,
      start: LocalDate,
      end: LocalDate)

  /**
   * A periodic schedule definition as the fixture carries it: the eleven fields, in Java order.
   *
   * These eleven keys sit at the top level of a row and are also the whole content of the nested
   * `replacedDefinition`, so one model serves both. They are present in '''every''' row of the
   * document, null where unset, including in the rows whose definition the factory refuses: a
   * definition that never builds is rendered from the raw values it was offered, so the input key
   * set does not vary with the outcome (capture README, section 6). That uniformity is what lets
   * one decoder read the inputs of all four row shapes.
   *
   * @param startDate  the unadjusted start date of the schedule
   * @param endDate  the unadjusted end date of the schedule
   * @param frequency  the periodic frequency's name, such as `P3M` or `Term`
   * @param businessDayAdjustment  the adjustment applied to every date of the schedule
   * @param startDateBusinessDayAdjustment  the adjustment of the start date, where it has its own
   * @param endDateBusinessDayAdjustment  the adjustment of the end date, where it has its own
   * @param stubConvention  the stub convention's name, where one is declared
   * @param rollConvention  the roll convention's name, where one is declared
   * @param firstRegularStartDate  the start of the first regular period, where one is declared
   * @param lastRegularEndDate  the end of the last regular period, where one is declared
   * @param overrideStartDate  the overriding start date of the first period, where one is declared
   */
  final case class DefinitionRow(
      startDate: LocalDate,
      endDate: LocalDate,
      frequency: String,
      businessDayAdjustment: BusinessDayAdjustmentRow,
      startDateBusinessDayAdjustment: Option[BusinessDayAdjustmentRow],
      endDateBusinessDayAdjustment: Option[BusinessDayAdjustmentRow],
      stubConvention: Option[String],
      rollConvention: Option[String],
      firstRegularStartDate: Option[LocalDate],
      lastRegularEndDate: Option[LocalDate],
      overrideStartDate: Option[AdjustableDateRow]) {

    /**
     * The text this definition is identified by, which names every one of the eleven fields that
     * is present.
     *
     * This is what distinguishes the 768 grid rows from one another, so it carries the frequency,
     * the stub convention, the roll convention and the calendar rather than only the dates.
     */
    def render: String = {
      val declared = List(
        Some(s"$startDate..$endDate"),
        Some(frequency),
        Some(s"bda=${businessDayAdjustment.render}"),
        startDateBusinessDayAdjustment.map(value => s"startBda=${value.render}"),
        endDateBusinessDayAdjustment.map(value => s"endBda=${value.render}"),
        stubConvention.map(value => s"stub=$value"),
        rollConvention.map(value => s"roll=$value"),
        firstRegularStartDate.map(value => s"firstRegular=$value"),
        lastRegularEndDate.map(value => s"lastRegular=$value"),
        overrideStartDate.map(value => s"override=${value.render}"))
      declared.flatten.mkString(" ")
    }
  }


  /** The seven expectation keys a resolved row carries, all of them or none. */
  val ResolvedExpectationKeys: Set[String] = Set(
    "unadjustedDates",
    "adjustedDates",
    "periods",
    "initialStub",
    "finalStub",
    "resolvedRollConvention",
    "resolvedFrequency")

  /** The five further keys a resolved `data_replace` row carries. */
  val ReplaceExpectationKeys: Set[String] = Set(
    "replacedDefinition",
    "replacedUnadjustedDates",
    "expectedStubConvention",
    "expectedLastRegularEndDate",
    "expectedRollConvention")

  /**
   * One row of `schedule-baseline.json`.
   *
   * The eleven inputs are read into [[DefinitionRow]] off this row's own cursor, because the
   * capture writes them at the top level rather than nested; the same model reads the nested
   * `replacedDefinition`, which holds exactly those eleven keys.
   *
   * ===Why the declared key set is carried===
   *
   * Two of the expectations - `initialStub` and `finalStub` - are legitimately JSON null on a
   * resolved row, meaning "this schedule has no such stub", and three more of the `data_replace`
   * keys are nullable in the same way. An `Option` field cannot tell a null apart from a key that
   * is not there, so the key set of the row object is decoded alongside the values. It is what
   * [[isResolved]] is decided from, and it lets the population test hold the document to the key
   * sets its schema states instead of inferring them from values that could be null either way.
   *
   * @param definition  the eleven input fields, which every row carries
   * @param source  the Java test method or generated population this row came from
   * @param unadjustedDates  the unadjusted boundary dates of the resolved schedule
   * @param adjustedDates  the adjusted boundary dates of the resolved schedule
   * @param periods  the resolved periods, in order
   * @param initialStub  the initial stub, where the resolved schedule has one
   * @param finalStub  the final stub, where the resolved schedule has one
   * @param resolvedRollConvention  the name of the roll convention the schedule was built with
   * @param resolvedFrequency  the name of the frequency the schedule was built with
   * @param replacedStartDate  the date passed to `replaceStartDate`, on a `data_replace` row
   * @param replacedDefinition  the definition that replacement produced
   * @param replacedUnadjustedDates  `createUnadjustedDates()` on the replaced definition
   * @param expectedStubConvention  the stub convention of the replaced definition
   * @param expectedLastRegularEndDate  the last regular end date of the replaced definition
   * @param expectedRollConvention  the roll convention of the replaced definition
   * @param error  the Java exception message, where the operation was refused
   * @param declaredKeys  the keys this row object actually declares
   */
  final case class ScheduleRow(
      definition: DefinitionRow,
      source: String,
      unadjustedDates: Option[Vector[LocalDate]],
      adjustedDates: Option[Vector[LocalDate]],
      periods: Option[Vector[PeriodRow]],
      initialStub: Option[PeriodRow],
      finalStub: Option[PeriodRow],
      resolvedRollConvention: Option[String],
      resolvedFrequency: Option[String],
      replacedStartDate: Option[LocalDate],
      replacedDefinition: Option[DefinitionRow],
      replacedUnadjustedDates: Option[Vector[LocalDate]],
      expectedStubConvention: Option[String],
      expectedLastRegularEndDate: Option[LocalDate],
      expectedRollConvention: Option[String],
      error: Option[String],
      declaredKeys: Set[String])
      extends ParityRow {

    /**
     * The name this row is reported under.
     *
     * This fixture carries no `id`, and its 879 rows share sixteen `source` values, so the
     * identity is derived: the source, the eleven inputs and - where the row has one - the
     * replacement date, without which two `data_replace` rows over one base definition would
     * collide (capture README, section 6, which measured both counts). The population test
     * asserts that the result is unique across the document.
     */
    override def id: String = {
      val replacement = replacedStartDate.fold("")(date => s" replaced=$date")
      s"$source ${definition.render}$replacement"
    }

    /** Whether this row declares the whole resolved expectation set. */
    def isResolved: Boolean = ResolvedExpectationKeys.subsetOf(declaredKeys)

    /** Whether this row declares some of the resolved expectation set but not all of it. */
    def isPartiallyResolved: Boolean = {
      val declared = ResolvedExpectationKeys.intersect(declaredKeys)
      declared.nonEmpty && declared.size != ResolvedExpectationKeys.size
    }

    /**
     * Whether the three date expectations of this row agree with one another.
     *
     * The capture derives all three from the periods of one schedule, so
     * `periods[i].unadjustedStart == unadjustedDates[i]`, `periods[i].unadjustedEnd ==
     * unadjustedDates[i + 1]`, `periods[i].start == adjustedDates[i]`, `periods[i].end ==
     * adjustedDates[i + 1]` and `periods.length == unadjustedDates.length - 1` hold by
     * construction. A row that broke one of them would set this spec to compare the port against
     * three mutually inconsistent expectations, at least one of which nothing could satisfy.
     */
    def isStructurallyConsistent: Boolean =
      (unadjustedDates, adjustedDates, periods) match {
        case (Some(unadjusted), Some(adjusted), Some(periodRows)) =>
          unadjusted.sizeIs == periodRows.size + 1 && adjusted.sizeIs == unadjusted.size &&
          periodRows.zipWithIndex.forall { case (period, index) =>
            period.unadjustedStart == unadjusted(index) &&
            period.unadjustedEnd == unadjusted(index + 1) &&
            period.start == adjusted(index) &&
            period.end == adjusted(index + 1)
          }
        case (None, None, None) => true
        case _ => false
      }
  }

  //-------------------------------------------------------------------------
  // The decoders, in dependency order. Every one of them is derived at compile time except the
  // row itself, which is written out because it reads the eleven input fields from its own
  // cursor - they are the row's own keys, not a nested object - and because it captures the key
  // set alongside the values.
  //-------------------------------------------------------------------------

  implicit val businessDayAdjustmentRowDecoder: Decoder[BusinessDayAdjustmentRow] =
    deriveDecoder[BusinessDayAdjustmentRow]

  implicit val adjustableDateRowDecoder: Decoder[AdjustableDateRow] =
    deriveDecoder[AdjustableDateRow]

  implicit val periodRowDecoder: Decoder[PeriodRow] = deriveDecoder[PeriodRow]

  implicit val definitionRowDecoder: Decoder[DefinitionRow] = deriveDecoder[DefinitionRow]

  implicit val scheduleRowDecoder: Decoder[ScheduleRow] = Decoder.instance { cursor =>
    for {
      definition <- cursor.as[DefinitionRow]
      source <- cursor.get[String]("source")
      unadjustedDates <- cursor.get[Option[Vector[LocalDate]]]("unadjustedDates")
      adjustedDates <- cursor.get[Option[Vector[LocalDate]]]("adjustedDates")
      periods <- cursor.get[Option[Vector[PeriodRow]]]("periods")
      initialStub <- cursor.get[Option[PeriodRow]]("initialStub")
      finalStub <- cursor.get[Option[PeriodRow]]("finalStub")
      resolvedRollConvention <- cursor.get[Option[String]]("resolvedRollConvention")
      resolvedFrequency <- cursor.get[Option[String]]("resolvedFrequency")
      replacedStartDate <- cursor.get[Option[LocalDate]]("replacedStartDate")
      replacedDefinition <- cursor.get[Option[DefinitionRow]]("replacedDefinition")
      replacedUnadjustedDates <- cursor.get[Option[Vector[LocalDate]]]("replacedUnadjustedDates")
      expectedStubConvention <- cursor.get[Option[String]]("expectedStubConvention")
      expectedLastRegularEndDate <- cursor.get[Option[LocalDate]]("expectedLastRegularEndDate")
      expectedRollConvention <- cursor.get[Option[String]]("expectedRollConvention")
      error <- cursor.get[Option[String]]("error")
    } yield ScheduleRow(
      definition,
      source,
      unadjustedDates,
      adjustedDates,
      periods,
      initialStub,
      finalStub,
      resolvedRollConvention,
      resolvedFrequency,
      replacedStartDate,
      replacedDefinition,
      replacedUnadjustedDates,
      expectedStubConvention,
      expectedLastRegularEndDate,
      expectedRollConvention,
      error,
      cursor.keys.fold(Set.empty[String])(_.toSet))
  }

  //-------------------------------------------------------------------------
  // The labels a discrepancy is filed under. Each one names the operation that produced the
  // value, so a report says which call of the port disagreed rather than only which field.
  //-------------------------------------------------------------------------

  /** The label of the validating factory that builds a definition. */
  val FactoryLabel: String = "PeriodicSchedule.of"

  /** The label of schedule creation. */
  val ScheduleLabel: String = "createSchedule(refData)"

  /** The label of the start-date replacement. */
  val ReplaceLabel: String = "replaceStartDate"

  //-------------------------------------------------------------------------
  // Rebuilding a captured definition.
  //
  // Every convention, frequency, calendar and adjustment is resolved BY NAME, through the same
  // public lookups any other caller uses, so the measurement covers the name contract as well as
  // the arithmetic: a member whose name drifted from the Java one fails here rather than being
  // quietly substituted by a constant this file names directly. A name the port cannot parse is a
  // defect of the port or of the fixture and not a parity result of any kind, so it is lifted into
  // the effect by `ParityHarness.raise`/`raiseNec` and ends the row.
  //-------------------------------------------------------------------------

  /** Resolves a frequency by its captured name, such as `P3M`, `P2D`, `P2Y` or `Term`. */
  def frequencyOf(name: String): IO[Frequency] = ParityHarness.raise(Frequency.parse(name))

  /** Resolves a stub convention by its captured name, such as `ShortInitial` or `Both`. */
  def stubConventionOf(name: String): IO[StubConvention] =
    ParityHarness.raiseNec(StubConvention.parse(name))

  /**
   * Resolves a roll convention by its captured name.
   *
   * The names include `None`, `EOM`, the four IMM conventions, `SFE`, `TBILL` and the
   * day-of-month members `Day1` to `Day30`. The member whose name is `None` is reached the same
   * way as every other - through the lookup, by name - and never through the Scala identifier
   * `NONE` that the port gives it so that it cannot shadow `scala.None` inside its own companion.
   */
  def rollConventionOf(name: String): IO[RollConvention] =
    ParityHarness.raiseNec(RollConvention.parse(name))

  /**
   * Rebuilds a business day adjustment from its captured convention and calendar names.
   *
   * The calendar name is handed to `HolidayCalendarId.of` whole, composite or not: that factory
   * parses `'+'` and `'~'` and normalises, deduplicates and sorts the parts, and it is one of the
   * things being measured, so splitting the name here would replace it with a reimplementation.
   */
  def adjustmentOf(row: BusinessDayAdjustmentRow): IO[BusinessDayAdjustment] =
    ParityHarness
      .raiseNec(BusinessDayConvention.parse(row.convention))
      .map(convention => BusinessDayAdjustment.of(convention, HolidayCalendarId.of(row.calendar)))

  /**
   * Rebuilds an adjustable date from its captured form.
   *
   * An absent adjustment becomes `BusinessDayAdjustment.NONE`, which is the value the Java
   * property defaults to and the value the capture writes when it is set explicitly, so both
   * renderings arrive at the same date.
   */
  def adjustableDateOf(row: AdjustableDateRow): IO[AdjustableDate] =
    row.adjustment
      .traverse(adjustmentOf)
      .map(adjustment => AdjustableDate.of(row.unadjusted, adjustment.getOrElse(BusinessDayAdjustment.NONE)))

  /**
   * Rebuilds a definition from its eleven captured fields.
   *
   * The outcome of the factory is returned as it stands rather than lifted, because a refusal is
   * an expectation of this fixture: eight rows are definitions the factory itself rejects, and one
   * `data_replace` row is refused later. What is lifted is only the resolution of the names the
   * row is written in, which cannot fail for a correct port and a correct fixture.
   *
   * @param row  the captured definition
   * @return the definition, or the failures the factory reported for it
   */
  def definitionOf(row: DefinitionRow): IO[EitherNec[Failure, PeriodicSchedule]] =
    for {
      frequency <- frequencyOf(row.frequency)
      businessDayAdjustment <- adjustmentOf(row.businessDayAdjustment)
      startAdjustment <- row.startDateBusinessDayAdjustment.traverse(adjustmentOf)
      endAdjustment <- row.endDateBusinessDayAdjustment.traverse(adjustmentOf)
      stubConvention <- row.stubConvention.traverse(stubConventionOf)
      rollConvention <- row.rollConvention.traverse(rollConventionOf)
      overrideStartDate <- row.overrideStartDate.traverse(adjustableDateOf)
    } yield PeriodicSchedule.of(
      row.startDate,
      row.endDate,
      frequency,
      businessDayAdjustment,
      startAdjustment,
      endAdjustment,
      stubConvention,
      rollConvention,
      row.firstRegularStartDate,
      row.lastRegularEndDate,
      overrideStartDate)

  //-------------------------------------------------------------------------
  // The measurement of one row.
  //-------------------------------------------------------------------------

  /**
   * Measures one captured row, answering with everything that differed.
   *
   * The three branches are the three shapes the capture's own guarded block could take
   * (`tools/parity-capture/capture-baseline.jsh`): the definition is refused before anything is
   * generated, the row is a start-date replacement, or the row is a plain resolution. Which branch
   * a row takes is decided by the port's own outcome and by the row's inputs, never by its
   * `source`: the eight `Invalid order` rows and the `Term`-with-explicit-stub row are all named
   * `PeriodicScheduleTest.invalid.*`, yet the first eight are refused by the factory and the last
   * by generation.
   *
   * Nothing here returns at the first difference. Each branch concatenates the messages of every
   * comparison it makes, so one run names every field of every row that disagrees.
   *
   * @param row  the captured row
   * @return the discrepancies, or nothing when the port reproduced the row
   */
  def checkRow(row: ScheduleRow): IO[List[String]] =
    definitionOf(row.definition).map(built => checkBuilt(row, built))

  /** Dispatches on whether the definition was built, and on what the row records about it. */
  private def checkBuilt(
      row: ScheduleRow,
      built: EitherNec[Failure, PeriodicSchedule]): List[String] =
    built match {
      case Left(_) =>
        // The factory refused the definition. On an `error` row that is the expectation, and the
        // refusal is checked for its shape - without requiring the `definition` attribute, since
        // no definition exists to attach. On a resolved row it is a discrepancy, reported by the
        // comparator that names the failure it did produce.
        if (row.error.isDefined) refusalNec(FactoryLabel, built, requireDefinition = false)
        else ParityHarness.assertRight(FactoryLabel, built)(_ => Nil)
      case Right(definition) =>
        row.replacedStartDate match {
          case Some(replacement) => checkReplacement(row, definition, replacement)
          case None => checkResolution(row, definition)
        }
    }

  //-------------------------------------------------------------------------
  // Refusals.
  //
  // A refusal is asserted on the error channel and then on its shape. Two entry points, one per
  // error type of the port: `EitherNec` for the factory and the replacement, which accumulate,
  // and `Either` for the generation steps, which report a single cause.
  //-------------------------------------------------------------------------

  /** Checks an accumulating operation refused its input, and the shape of every cause. */
  private def refusalNec(
      label: String,
      actual: EitherNec[Failure, Any],
      requireDefinition: Boolean): List[String] =
    ParityHarness.assertLeft(label, actual) :::
      actual.swap.toOption.toList.flatMap(failures =>
        failures.toChain.toList.flatMap(failure =>
          failureShapeMessages(label, failure, requireDefinition)))

  /** Checks a single-cause operation refused its input, and the shape of that cause. */
  private def refusal(
      label: String,
      actual: Either[Failure, Any],
      requireDefinition: Boolean): List[String] =
    ParityHarness.assertLeft(label, actual) :::
      actual.swap.toOption.toList.flatMap(failure =>
        failureShapeMessages(label, failure, requireDefinition))

  /**
   * Checks that a refusal is the refusal the port is specified to report.
   *
   * Two properties, and the captured message is not one of them. Java's `ScheduleException` and
   * its argument checks both become `Failure.Invalid` in the port, so the reason is required to be
   * `INVALID` for every cause; and every failure a '''generation''' step reports attaches the
   * definition it rejected under the `definition` attribute, as the exception being replaced
   * carried it as a field, so that attribute is required of those. It is not required of a failure
   * the factory reported, because the definition it would name does not exist: those come from the
   * order checks of `Validate` and carry no attributes at all.
   *
   * The wording is deliberately not compared. Exception-to-`Failure` is a recorded divergence
   * (AAP section 0.8.3), so the port's messages are its own; a message is quoted inside a
   * discrepancy, to make it readable, and never asserted against the fixture's `error`.
   *
   * @param label  the operation the refusal came from
   * @param failure  one cause of the refusal
   * @param requireDefinition  whether the cause must carry the rejected definition
   * @return the discrepancies, or nothing when the refusal has the specified shape
   */
  private def failureShapeMessages(
      label: String,
      failure: Failure,
      requireDefinition: Boolean): List[String] = {
    val reason =
      if (failure.reason == FailureReason.INVALID) {
        Nil
      } else {
        List(
          s"$label: the refusal's reason is '${failure.reason.name}', expected " +
            s"'${FailureReason.INVALID.name}' (message: '${failure.message}')")
      }
    val definition =
      if (!requireDefinition || failure.attributes.contains(DefinitionAttribute)) {
        Nil
      } else {
        List(
          s"$label: the refusal carries no '$DefinitionAttribute' attribute, which every failure " +
            s"a generation step attaches the rejected definition under (message: " +
            s"'${failure.message}', attributes: ${failure.attributes.keys.mkString("[", ", ", "]")})")
      }
    reason ::: definition
  }


  //-------------------------------------------------------------------------
  // A plain resolution: the 868 rows that are not start-date replacements.
  //-------------------------------------------------------------------------

  /**
   * Measures a row that records a resolution, or the refusal of one.
   *
   * Creation is attempted exactly once, as the capture attempted it once, and the row's `error`
   * decides which of the two outcomes is the expectation. A refused row is checked on the error
   * channel and for its shape, with the `definition` attribute required because a generation
   * failure has a definition to name.
   *
   * @param row  the captured row
   * @param definition  the definition rebuilt from the row's inputs
   * @return the discrepancies, or nothing when the port reproduced the row
   */
  private def checkResolution(row: ScheduleRow, definition: PeriodicSchedule): List[String] = {
    val schedule = definition.createSchedule(referenceData)
    if (row.error.isDefined) {
      refusal(ScheduleLabel, schedule, requireDefinition = true)
    } else {
      ParityHarness.assertRight(ScheduleLabel, schedule)(resolved => scheduleMessages(row, resolved)) :::
        dateListMessages(row, row.definition, definition)
    }
  }

  /**
   * Compares a resolved schedule against the seven expectations a resolved row carries.
   *
   * Each is compared only where the row declares it, and every one of them is a date, a list of
   * dates or a name, so every comparison here is exact.
   *
   * @param row  the captured row
   * @param schedule  the schedule the port resolved
   * @return the discrepancies, or nothing when every expectation matched
   */
  private def scheduleMessages(row: ScheduleRow, schedule: Schedule): List[String] =
    exactly("unadjustedDates", schedule.unadjustedDates.toList, row.unadjustedDates.map(_.toList)) :::
      exactly("adjustedDates", adjustedDatesOf(schedule), row.adjustedDates.map(_.toList)) :::
      periodsMessages(schedule.periods.toList, row.periods) :::
      whenDeclared(row, "initialStub")(stubMessages("initialStub", schedule.initialStub, row.initialStub)) :::
      whenDeclared(row, "finalStub")(stubMessages("finalStub", schedule.finalStub, row.finalStub)) :::
      exactly("resolvedRollConvention", schedule.rollConvention.name, row.resolvedRollConvention) :::
      exactly("resolvedFrequency", schedule.periodicFrequency.name, row.resolvedFrequency)

  /**
   * The adjusted boundary dates of a schedule, as the capture derived them.
   *
   * The adjusted start of the schedule followed by the adjusted end of each period, which is one
   * date more than there are periods. The port publishes the unadjusted view of this list as
   * `Schedule.unadjustedDates` and leaves the adjusted one to be read off the periods, which is
   * exactly what the capture did.
   */
  private def adjustedDatesOf(schedule: Schedule): List[LocalDate] =
    schedule.adjustedStartDate :: schedule.periods.toList.map(_.endDate)

  /**
   * Compares the resolved periods against the captured ones, one period at a time.
   *
   * A length difference is reported once and stops there: a schedule with an extra or a missing
   * period would otherwise report four discrepancies for every index after the divergence and
   * bury the one fact worth knowing. Equal lengths are compared index by index, each of the four
   * dates under its own indexed label, so a report names the period and the field that differ.
   */
  private def periodsMessages(
      actual: List[SchedulePeriod],
      expected: Option[Vector[PeriodRow]]): List[String] =
    expected.toList.flatMap { periods =>
      if (actual.sizeIs != periods.size) {
        List(
          s"periods: actual ${actual.size} periods, expected ${periods.size} " +
            s"(actual ${actual.mkString("[", ", ", "]")})")
      } else {
        actual.zip(periods).zipWithIndex.flatMap { case ((period, expectedPeriod), index) =>
          periodMessages(s"periods[$index]", period, expectedPeriod)
        }
      }
    }

  /** Compares the four dates of one period. */
  private def periodMessages(
      label: String,
      actual: SchedulePeriod,
      expected: PeriodRow): List[String] =
    ParityHarness.assertExact(s"$label.unadjustedStart", actual.unadjustedStartDate, expected.unadjustedStart) :::
      ParityHarness.assertExact(s"$label.unadjustedEnd", actual.unadjustedEndDate, expected.unadjustedEnd) :::
      ParityHarness.assertExact(s"$label.start", actual.startDate, expected.start) :::
      ParityHarness.assertExact(s"$label.end", actual.endDate, expected.end)

  /**
   * Compares a stub, presence first.
   *
   * A stub the fixture records as absent is an expectation in its own right - most resolved rows
   * have no final stub - so a schedule that produced one where none was captured, or none where
   * one was, is reported as that rather than as four date differences. The caller only reaches
   * this where the row declares the key, so an absent expectation here is a captured JSON null
   * and means "this schedule has no such stub".
   */
  private def stubMessages(
      label: String,
      actual: Option[SchedulePeriod],
      expected: Option[PeriodRow]): List[String] =
    (actual, expected) match {
      case (None, None) => Nil
      case (Some(period), Some(expectedPeriod)) => periodMessages(label, period, expectedPeriod)
      case (Some(period), None) => List(s"$label: actual $period, expected no stub")
      case (None, Some(expectedPeriod)) => List(s"$label: actual no stub, expected $expectedPeriod")
    }

  /**
   * Measures the three date-list members against the captured lists.
   *
   * These are a different route to the same dates, and the Java tests state the identity for every
   * `data_generation` row: `createUnadjustedDates(REF_DATA)` equals the table's unadjusted list
   * (`PeriodicScheduleTest.java:737`) and `createAdjustedDates(REF_DATA)` equals its adjusted list
   * (`:812`). Since the capture cross-checked the schedule-derived lists against those same table
   * columns, holding the port to the identity here is reproducing a stated Java expectation.
   *
   * The no-reference-data form is measured under the guard Java applies to it (`:740`): only where
   * the start date carries no business day adjustment of its own and the declared roll convention
   * is not `EOM`. Outside that guard the two forms genuinely differ, because the recovery of a
   * pre-adjusted start date needs the holiday calendars and the bare form has none - the Java test
   * says as much in a comment, and `data_replace` row 2 is the case in the fixture where it shows.
   * That row's own answer is captured separately, under `replacedUnadjustedDates`.
   *
   * @param row  the captured row, for the expectations
   * @param inputs  the captured inputs, for the guard
   * @param definition  the definition rebuilt from those inputs
   * @return the discrepancies, or nothing when every list matched
   */
  private def dateListMessages(
      row: ScheduleRow,
      inputs: DefinitionRow,
      definition: PeriodicSchedule): List[String] = {
    val unadjusted = row.unadjustedDates.map(_.toList)
    val adjusted = row.adjustedDates.map(_.toList)
    val bare =
      if (reproducesUnadjustedWithoutReferenceData(inputs)) {
        rightExactly("createUnadjustedDates()", definition.createUnadjustedDates(), unadjusted)
      } else {
        Nil
      }
    rightExactly("createUnadjustedDates(refData)", definition.createUnadjustedDates(referenceData), unadjusted) :::
      rightExactly("createAdjustedDates(refData)", definition.createAdjustedDates(referenceData), adjusted) :::
      bare
  }

  /**
   * Whether the no-reference-data form of the unadjusted dates is expected to agree with the
   * reference-data form for this definition.
   *
   * This is the guard of `PeriodicScheduleTest.test_monthly_unadjusted` (`:740`), restated: the
   * start date has no adjustment of its own and the declared roll convention is not `EOM`.
   */
  private def reproducesUnadjustedWithoutReferenceData(inputs: DefinitionRow): Boolean =
    inputs.startDateBusinessDayAdjustment.isEmpty && !inputs.rollConvention.contains(EomName)


  //-------------------------------------------------------------------------
  // A start-date replacement: the eleven `data_replace` rows.
  //-------------------------------------------------------------------------

  /**
   * Measures a row that records a `replaceStartDate` operation.
   *
   * The row carries the '''base''' definition and the replacement date, so the whole operation is
   * reproduced here rather than only its result: the replacement is applied to the base, and then
   * the post-replacement definition, the unadjusted dates that definition creates, the three
   * properties the Java table asserts of it, and the schedule it resolves to are each compared.
   * That is four independent expectations of one operation, which is what the capture recorded
   * (`tools/parity-capture/capture-baseline.jsh`, `emitDataReplace`).
   *
   * The one refused row is the only coverage anywhere of `replaceStartDate` rejecting a date after
   * the end date, and the failure it must report carries the definition, so the attribute is
   * required of it.
   *
   * The captured `replacedUnadjustedDates` is compared against the '''bare'''
   * `createUnadjustedDates()`, which is the call the Java table asserts, and is deliberately not
   * compared against this row's `unadjustedDates`: the two disagree on one of these rows and both
   * answers are correct (see [[dateListMessages]]).
   *
   * @param row  the captured row
   * @param base  the base definition rebuilt from the row's inputs
   * @param replacement  the replacement start date
   * @return the discrepancies, or nothing when the port reproduced the row
   */
  private def checkReplacement(
      row: ScheduleRow,
      base: PeriodicSchedule,
      replacement: LocalDate): List[String] = {
    val replaced = base.replaceStartDate(replacement)
    if (row.error.isDefined) {
      refusalNec(ReplaceLabel, replaced, requireDefinition = true)
    } else {
      ParityHarness.assertRight(ReplaceLabel, replaced) { definition =>
        definitionMessages("replacedDefinition", definition, row.replacedDefinition) :::
          rightExactly(
            "replacedUnadjustedDates",
            definition.createUnadjustedDates(),
            row.replacedUnadjustedDates.map(_.toList)) :::
          whenDeclared(row, "expectedStubConvention")(
            ParityHarness.assertExact(
              "expectedStubConvention",
              definition.stubConvention.map(_.name),
              row.expectedStubConvention)) :::
          whenDeclared(row, "expectedLastRegularEndDate")(
            ParityHarness.assertExact(
              "expectedLastRegularEndDate",
              definition.lastRegularEndDate,
              row.expectedLastRegularEndDate)) :::
          whenDeclared(row, "expectedRollConvention")(
            ParityHarness.assertExact(
              "expectedRollConvention",
              definition.rollConvention.map(_.name),
              row.expectedRollConvention)) :::
          ParityHarness.assertRight(ScheduleLabel, definition.createSchedule(referenceData))(
            resolved => scheduleMessages(row, resolved))
      }
    }
  }

  /**
   * Compares a definition against the eleven captured fields of one.
   *
   * Every field is compared, not only the four the Java test names, because the whole point of the
   * captured `replacedDefinition` is that `replaceStartDate` is specified by what it changes '''and
   *''' by what it leaves alone: the start date becomes the replacement, the start-date adjustment
   * becomes `BusinessDayAdjustment.NONE`, the first regular start date and the override start date
   * are cleared, the stub convention and last regular end date may be rewritten, and the other four
   * fields must be untouched
   * (`modules/basics/src/test/java/com/opengamma/strata/basics/schedule/PeriodicScheduleTest.java:1002-1013`).
   *
   * Conventions and calendars are compared by name, which is the form the fixture carries and the
   * identity the port is required to preserve.
   *
   * @param label  the name of the definition being compared
   * @param actual  the definition the port produced
   * @param expected  the captured definition, where the row carries one
   * @return the discrepancies, or nothing when all eleven fields matched
   */
  private def definitionMessages(
      label: String,
      actual: PeriodicSchedule,
      expected: Option[DefinitionRow]): List[String] =
    expected.toList.flatMap { definition =>
      ParityHarness.assertExact(s"$label.startDate", actual.startDate, definition.startDate) :::
        ParityHarness.assertExact(s"$label.endDate", actual.endDate, definition.endDate) :::
        ParityHarness.assertExact(s"$label.frequency", actual.frequency.name, definition.frequency) :::
        adjustmentMessages(
          s"$label.businessDayAdjustment",
          Some(actual.businessDayAdjustment),
          Some(definition.businessDayAdjustment)) :::
        adjustmentMessages(
          s"$label.startDateBusinessDayAdjustment",
          actual.startDateBusinessDayAdjustment,
          definition.startDateBusinessDayAdjustment) :::
        adjustmentMessages(
          s"$label.endDateBusinessDayAdjustment",
          actual.endDateBusinessDayAdjustment,
          definition.endDateBusinessDayAdjustment) :::
        ParityHarness.assertExact(
          s"$label.stubConvention",
          actual.stubConvention.map(_.name),
          definition.stubConvention) :::
        ParityHarness.assertExact(
          s"$label.rollConvention",
          actual.rollConvention.map(_.name),
          definition.rollConvention) :::
        ParityHarness.assertExact(
          s"$label.firstRegularStartDate",
          actual.firstRegularStartDate,
          definition.firstRegularStartDate) :::
        ParityHarness.assertExact(
          s"$label.lastRegularEndDate",
          actual.lastRegularEndDate,
          definition.lastRegularEndDate) :::
        adjustableDateMessages(
          s"$label.overrideStartDate",
          actual.overrideStartDate,
          definition.overrideStartDate)
    }

  /** Compares a business day adjustment by convention and calendar name, presence first. */
  private def adjustmentMessages(
      label: String,
      actual: Option[BusinessDayAdjustment],
      expected: Option[BusinessDayAdjustmentRow]): List[String] =
    (actual, expected) match {
      case (None, None) => Nil
      case (Some(adjustment), Some(expectedAdjustment)) =>
        ParityHarness.assertExact(
          s"$label.convention",
          adjustment.convention.name,
          expectedAdjustment.convention) :::
          ParityHarness.assertExact(
            s"$label.calendar",
            adjustment.calendar.name,
            expectedAdjustment.calendar)
      case (Some(adjustment), None) =>
        List(s"$label: actual $adjustment, expected no adjustment")
      case (None, Some(expectedAdjustment)) =>
        List(s"$label: actual no adjustment, expected ${expectedAdjustment.render}")
    }

  /**
   * Compares an adjustable date, presence first.
   *
   * A captured adjustable date whose adjustment is absent is read as `BusinessDayAdjustment.NONE`,
   * which is the value the property defaults to, so the comparison is against the same value
   * whichever way the capture wrote it.
   */
  private def adjustableDateMessages(
      label: String,
      actual: Option[AdjustableDate],
      expected: Option[AdjustableDateRow]): List[String] =
    (actual, expected) match {
      case (None, None) => Nil
      case (Some(date), Some(expectedDate)) =>
        ParityHarness.assertExact(s"$label.unadjusted", date.unadjusted, expectedDate.unadjusted) :::
          adjustmentMessages(
            s"$label.adjustment",
            Some(date.adjustment),
            Some(expectedDate.adjustment.getOrElse(NoAdjustment)))
      case (Some(date), None) => List(s"$label: actual $date, expected no override start date")
      case (None, Some(expectedDate)) =>
        List(s"$label: actual no override start date, expected ${expectedDate.render}")
    }

  //-------------------------------------------------------------------------
  // The shared comparators. Each answers with what differed, so a caller concatenates them and
  // every field of a row that disagrees is named in one run.
  //-------------------------------------------------------------------------

  /**
   * Compares a value against an expectation the row may not carry.
   *
   * An expectation the row does not carry is not compared - and not silently passed either: the
   * population test is what holds the document to carrying the expectations it is required to,
   * which keeps "this row says nothing about that" and "this row was measured" apart.
   */
  private def exactly[A](label: String, actual: => A, expected: Option[A]): List[String] =
    expected.toList.flatMap(value => ParityHarness.assertExact(label, actual, value))

  /**
   * Compares the value of an operation that could have failed, against an expectation the row may
   * not carry.
   *
   * A failure is reported once, naming the failure, and no comparison is attempted on a value that
   * does not exist.
   */
  private def rightExactly[A](
      label: String,
      actual: Either[Failure, A],
      expected: Option[A]): List[String] =
    expected.toList.flatMap(value =>
      ParityHarness.assertRight(label, actual)(produced =>
        ParityHarness.assertExact(label, produced, value)))

  /**
   * Runs comparisons only where the row declares the key they are about.
   *
   * Five of the expectation keys are legitimately JSON null on a row that carries them - a
   * schedule with no final stub, a replaced definition that declares no roll convention - so
   * absence of a '''value''' cannot decide whether an expectation was made. The key set can, and
   * this is where that distinction is applied.
   */
  private def whenDeclared(row: ScheduleRow, key: String)(messages: => List[String]): List[String] =
    if (row.declaredKeys.contains(key)) messages else Nil

  //-------------------------------------------------------------------------
  // Two names used above, stated once.
  //-------------------------------------------------------------------------

  /** The name of the end-of-month roll convention, which the bare-form guard tests for. */
  private val EomName: String = "EOM"

  /**
   * The captured form of `BusinessDayAdjustment.NONE`.
   *
   * The constant is `NoAdjust` over `NoHolidays`, and the capture writes it exactly like any other
   * adjustment, so this is what an adjustable date's absent adjustment is compared against.
   */
  private val NoAdjustment: BusinessDayAdjustmentRow =
    BusinessDayAdjustmentRow("NoAdjust", "NoHolidays")
}

