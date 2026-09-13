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
import io.circe.parser.decode

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
 * Parity of periodic schedule generation.
 *
 * Each row of `parity/schedule-baseline.json` names a `PeriodicSchedule` definition by its eleven
 * fields. This spec rebuilds that definition through the validating factory, resolves it against
 * `ReferenceData.standard`, and compares the unadjusted dates, the adjusted dates, every period,
 * both stubs, the resolved roll convention and the resolved frequency against the row. The eleven
 * `data_replace` rows additionally reproduce the whole `replaceStartDate` operation: the
 * post-replacement definition field by field, the unadjusted dates it creates, and the schedule it
 * resolves to.
 *
 * A schedule is dates, names and structure, and there is not one numeric expectation in the
 * fixture, so every comparison goes through [[ParityHarness.assertExact]] and no tolerance appears
 * in this file. The committed baseline holds the reference values and is read-only here: no
 * expectation is corrected or loosened and no row is skipped.
 */
class ScheduleParitySpec extends AsyncFunSuite with AsyncIOSpec with Matchers {

  import ScheduleParitySpec._

  test("periodic schedule generation reproduces the Java baseline exactly") {
    ParityHarness
      .runFixture[ScheduleRow](FixtureName, FixtureResource, RowSchema)(checkRow)
      .flatMap(report => ParityHarness.failIfAny(report))
      .as(succeed)
  }

  test("the fixture carries the population the schedule baseline is required to measure") {
    ParityHarness.loadStrict[ScheduleRow](FixtureResource, RowSchema).map { rows =>
      val bySource = rows.groupBy(_.source).view.mapValues(_.size).toMap
      withClue(
        s"fixture rows: ${rows.size}; rows by source: " +
          s"${bySource.toVector.sortBy(_._1).mkString(", ")}: ") {
        // A gate reading `failed == 0` cannot tell a complete measurement from a thinned one, so
        // the population is asserted here, as floors rather than equalities so that coverage can
        // grow.
        rows.size should be >= MinimumRows
        // The derived identity of a row - see `ScheduleRow.id` - is a checked property, not an
        // assumption: without the replacement date two `data_replace` rows would share one.
        rows.map(_.id).distinct should have size rows.size.toLong
        MinimumRowsBySource.foreach { case (source, minimum) =>
          withClue(s"source '$source': ") {
            bySource.getOrElse(source, 0) should be >= minimum
          }
        }
        // A row of unstated provenance is reported rather than counted towards another floor.
        rows.map(_.source).distinct.filterNot(MinimumRowsBySource.contains) shouldBe empty
        // Exactly one of a refusal and the full expectation set, which is the rule the four
        // variants encode: a row carrying neither, or only some of the seven expectation keys,
        // would measure less.
        rows.filter(row => row.error.isDefined == row.isResolved).map(_.id) shouldBe empty
        rows.filter(_.isPartiallyResolved).map(_.id) shouldBe empty
        rows.count(_.error.isDefined) should be >= MinimumErrorRows
        rows.count(_.isResolved) should be >= MinimumResolvedRows
        // The four structural identities of a resolved row, which hold by construction - see
        // `ScheduleRow.isStructurallyConsistent`. A row that broke one would make the three date
        // expectations disagree, and a defect of the fixture would be reported as a discrepancy.
        rows.filterNot(_.isStructurallyConsistent).map(_.id) shouldBe empty
        // Every calendar shape the fixture is required to exercise, the composite one included.
        val calendars = rows.map(_.definition.businessDayAdjustment.calendar).toSet
        RequiredCalendars.filterNot(calendars.contains) shouldBe empty
        rows.count(_.definition.businessDayAdjustment.calendar == CompositeCalendar) should
          be >= MinimumCompositeCalendarRows
        // The generated grid, roll convention by roll convention and over all three calendars each.
        // Thinning `IMMCAD`, `IMMAUD` or `TBILL` would retire the evidence that a convention
        // resolves dates through the calendars it holds; `SFE` and `IMMNZD` hold none and are the
        // control group.
        GridRollConventions.foreach { convention =>
          withClue(s"grid rows for roll convention '$convention': ") {
            val forConvention = rows.filter(row =>
              row.source == GridSource && row.definition.rollConvention.contains(convention))
            forConvention.size should be >= MinimumRowsPerGridRollConvention
            val forConventionCalendars =
              forConvention.map(_.definition.businessDayAdjustment.calendar).toSet
            GridCalendars.filterNot(forConventionCalendars.contains) shouldBe empty
            // Both halves of the grid's evidence: a convention that resolves keeps resolving over
            // all three calendars, because those rows are the only ones whose dates were generated
            // through the calendars the convention holds, and a convention this span refuses keeps
            // refusing.
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
            // Every grid roll convention is accounted for by one of the two maps above.
            withClue(s"outcome floor stated for '$convention': ") {
              (MinimumResolvedGridRows.contains(convention) ||
                MinimumRefusedGridRows.contains(convention)) shouldBe true
            }
          }
        }
        // The `data_replace` family, whose rows model an operation rather than a resolution: a
        // resolved row carries both of its own expectations, a rejected one carries neither, and at
        // least one is rejected - the only coverage of `replaceStartDate` refusing a date after the
        // end date.
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
        // A resolved replacement row declares all five of its own keys, checked on the key set
        // because three of the five are legitimately null and a dropped key would otherwise read as
        // "none".
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

  //-------------------------------------------------------------------------
  // The strictness of the decoding. A document that satisfies a schema carries no counter-example,
  // so the three cases below decode hand-built documents through the real decoders.
  //-------------------------------------------------------------------------

  test("each documented row shape decodes to the variant that describes it") {
    IO {
      SampleRows.foreach { case (variant, fields) =>
        withClue(s"the hand-built '$variant' row: ") {
          val row = decoded(fields)
          row.variant shouldBe variant
          row.declaredKeys shouldBe fields.map(_._1).toSet
          // The two readings of "resolved" agree: the variant matched, and the seven expectation
          // keys present. A variant added on one side only would separate them here.
          row.isResolved shouldBe ResolvedExpectationKeys.subsetOf(row.declaredKeys)
          row.isPartiallyResolved shouldBe false
        }
      }
      succeed
    }
  }

  test("a row whose key set is not a documented variant is refused, naming the key at fault") {
    IO {
      // The two keys the mutations use must belong to every variant: dropping a key only some
      // variants declare would turn one documented shape into another and the row would be
      // accepted.
      RowSchema.variants.foreach { case (variant, keys) =>
        withClue(s"the keys of variant '$variant': ") {
          keys should contain allOf (RowRequiredKey, RowRenamedKey)
        }
      }
      SampleRows.foreach { case (variant, fields) =>
        withClue(s"the '$variant' row with the undocumented key '$UndocumentedKey': ") {
          refusalOf(withUnknownKey(fields)) should include(UndocumentedKey)
        }
        withClue(s"the '$variant' row with '$RowRequiredKey' dropped: ") {
          refusalOf(without(fields, RowRequiredKey)) should include(RowRequiredKey)
        }
        withClue(s"the '$variant' row with '$RowRenamedKey' renamed: ") {
          val message = refusalOf(renaming(fields, RowRenamedKey))
          message should include(renamed(RowRenamedKey))
          message should include(RowRenamedKey)
        }
      }
      succeed
    }
  }

  test("a nested captured object whose keys are not the documented ones is refused") {
    IO {
      NestedSamples.foreach { sample =>
        def nested(fields: SampleFields): SampleFields =
          replacing(sample.host, sample.key, objectText(fields))
        withClue(s"${sample.shape} under '${sample.key}' with '$UndocumentedKey' added: ") {
          refusalOf(nested(withUnknownKey(sample.fields))) should include(UndocumentedKey)
        }
        withClue(s"${sample.shape} under '${sample.key}' with '${sample.required}' dropped: ") {
          refusalOf(nested(without(sample.fields, sample.required))) should include(sample.required)
        }
        withClue(s"${sample.shape} under '${sample.key}' with '${sample.required}' renamed: ") {
          val message = refusalOf(nested(renaming(sample.fields, sample.required)))
          message should include(renamed(sample.required))
          message should include(sample.required)
        }
      }
      succeed
    }
  }

  private def decoded(fields: SampleFields): ScheduleRow = {
    val payload = objectText(fields)
    decode[ScheduleRow](payload).fold(
      failure => fail(s"the document '$payload' should have decoded: ${failure.getMessage}"),
      identity)
  }

  private def refusalOf(fields: SampleFields): String = {
    val payload = objectText(fields)
    decode[ScheduleRow](payload) match {
      case Left(failure) => failure.getMessage
      case Right(row) =>
        fail(
          s"the document '$payload' should have been refused, but decoded as the " +
            s"'${row.variant}' variant")
    }
  }
}

/**
 * The row model of `schedule-baseline.json`, the definition it encodes, and the checks applied to
 * one row. No `Double` appears in any row, so the tagged non-finite double policy of
 * `com.opengamma.strata.collect.json.Codecs` is neither needed nor imported.
 */
private[parity] object ScheduleParitySpec {

  val FixtureName: String = "schedule"

  val FixtureResource: String = "parity/schedule-baseline.json"

  val DefinitionAttribute: String = "definition"

  /** The `source` of the generated combination grid, which is 768 of the 879 rows. */
  val GridSource: String = "grid.combinations"

  val ReplaceSource: String = "PeriodicScheduleTest.data_replace"

  /** The composite calendar a third of the grid's rows adjust against. */
  val CompositeCalendar: String = "GBLO+USNY"

  val MinimumRows: Int = 879

  val MinimumErrorRows: Int = 573

  val MinimumResolvedRows: Int = 306

  val MinimumReplaceRows: Int = 11

  val MinimumReplaceRefusals: Int = 1

  val MinimumCompositeCalendarRows: Int = 256

  val MinimumRowsPerGridRollConvention: Int = 96

  /**
   * Every `source` the fixture carries, with the number of rows it is required to contribute. The
   * thirteen named feature rows are the only coverage of the three inputs the two tabular
   * populations and the grid never populate: the end-date business day adjustment, the override
   * start date, and the factory's own order invariants.
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
   * The eight roll conventions of the generated grid. Three hold holiday calendars of their own,
   * bound as `StandardHolidayCalendars` constants: `IMMCAD` holds `GBLO` and `CATO` combined with
   * `CAMO`, `IMMAUD` holds `AUSY`, and `TBILL` holds `USNY`. `SFE` and `IMMNZD` hold none and are
   * the control group.
   */
  val GridRollConventions: List[String] =
    List("EOM", "IMM", "IMMCAD", "IMMAUD", "IMMNZD", "SFE", "TBILL", "Day15")

  val GridCalendars: List[String] = List("GBLO", "USNY", CompositeCalendar)

  /**
   * The grid roll conventions whose combinations resolve, with the number of rows each resolves.
   * `IMMCAD` is the calendar-bearing member that appears here, and its 36 rows (twelve per
   * calendar) are the only dates in this fixture generated through a convention's own holiday
   * calendars, so they are its date-level evidence of the calendars it holds.
   */
  val MinimumResolvedGridRows: Map[String, Int] = Map("EOM" -> 84, "IMMCAD" -> 36, "Day15" -> 84)

  /**
   * The grid roll conventions the grid's span refuses outright, with the number of refusals each
   * contributes. The start date does not match these conventions rolling forwards and the end date
   * does not match them rolling backwards, so all 96 combinations of each are refused. A refusal
   * measures `adjust` - a convention that left either date unmoved would resolve - but does not
   * discriminate '''which''' calendar a convention holds; the probes that do live in
   * `com.opengamma.strata.basics.schedule.RollConventionSpec`.
   */
  val MinimumRefusedGridRows: Map[String, Int] =
    Map("IMM" -> 96, "IMMAUD" -> 96, "IMMNZD" -> 96, "SFE" -> 96, "TBILL" -> 96)

  val RequiredCalendars: List[String] =
    List("GBLO", "USNY", CompositeCalendar, "Sat/Sun", "NoHolidays", "JPTO")

  /**
   * The reference data every resolution here is measured against, resolved once for the whole run;
   * its calendars are generated lazily, so each calendar's rule generation is paid once per JVM.
   */
  val referenceData: ReferenceData = ReferenceData.standard

  final case class BusinessDayAdjustmentRow(convention: String, calendar: String) {

    def render: String = s"$convention/$calendar"
  }

  val AdjustmentSchema: KeySchema =
    KeySchema.uniform("a captured business day adjustment", Set("convention", "calendar"))

  /**
   * An adjustable date as the fixture carries it, an absent adjustment being read as
   * `BusinessDayAdjustment.NONE`, the value the property defaults to.
   */
  final case class AdjustableDateRow(
      unadjusted: LocalDate,
      adjustment: Option[BusinessDayAdjustmentRow]) {

    def render: String = s"$unadjusted${adjustment.fold("")(value => s"[${value.render}]")}"
  }

  /**
   * The documented key set of a captured adjustable date. Both keys are '''required''' although the
   * adjustment is an `Option`: an absent adjustment is null rather than omitted, so declaring it
   * optional would accept an object that had dropped the key.
   */
  val AdjustableDateSchema: KeySchema =
    KeySchema.uniform("a captured adjustable date", Set("unadjusted", "adjustment"))

  final case class PeriodRow(
      unadjustedStart: LocalDate,
      unadjustedEnd: LocalDate,
      start: LocalDate,
      end: LocalDate)

  val PeriodSchema: KeySchema =
    KeySchema.uniform(
      "a captured schedule period",
      Set("unadjustedStart", "unadjustedEnd", "start", "end"))

  /**
   * A periodic schedule definition as the fixture carries it: the eleven fields. They sit at the
   * top level of a row and are also the whole content of the nested `replacedDefinition`, and they
   * are present in '''every''' row, null where unset - including the rows whose definition the
   * factory refuses, which is what lets one decoder read all four row shapes.
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

  val InputKeys: Set[String] = Set(
    "startDate",
    "endDate",
    "frequency",
    "businessDayAdjustment",
    "startDateBusinessDayAdjustment",
    "endDateBusinessDayAdjustment",
    "stubConvention",
    "rollConvention",
    "firstRegularStartDate",
    "lastRegularEndDate",
    "overrideStartDate")

  /**
   * The documented key set of the nested `replacedDefinition`, the only place a definition's own
   * keys are visible as an object of their own.
   */
  val DefinitionSchema: KeySchema =
    KeySchema.uniform("a captured schedule definition", InputKeys)

  val ResolvedExpectationKeys: Set[String] = Set(
    "unadjustedDates",
    "adjustedDates",
    "periods",
    "initialStub",
    "finalStub",
    "resolvedRollConvention",
    "resolvedFrequency")

  val ReplaceExpectationKeys: Set[String] = Set(
    "replacedDefinition",
    "replacedUnadjustedDates",
    "expectedStubConvention",
    "expectedLastRegularEndDate",
    "expectedRollConvention")

  //-------------------------------------------------------------------------
  // The key schema of a row. The document holds four row shapes and no others, declared here as the
  // variants of one schema. A derived decoder ignores keys its model does not declare, so an object
  // that gained, lost or renamed one would decode silently and leave its expectation unmeasured
  // while the report still read zero failures.
  //-------------------------------------------------------------------------

  val ErrorVariant: String = "error"

  val ErrorReplacementVariant: String = "error with a replaced start date"

  val ResolvedVariant: String = "resolved"

  val ResolvedReplacementVariant: String = "resolved replacement"

  /**
   * The four documented key sets of one row of `schedule-baseline.json`.
   *
   * Each variant is an '''exact''' key set: [[KeySchema]] requires every key of the variant and
   * admits nothing outside it, and no key of this document is optional. The four are therefore
   * mutually exclusive even though `error` is a subset of `error with a replaced start date`, and
   * the order they are declared in does not affect which one a row matches.
   *
   *  - `error` - the eleven inputs, `source` and `error`: 13 keys, 572 rows.
   *  - `error with a replaced start date` - those 13 plus `replacedStartDate`: 14 keys, 1 row.
   *  - `resolved` - the eleven inputs, `source` and the seven expectations: 19 keys, 296 rows.
   *  - `resolved replacement` - those 19 plus `replacedStartDate` and the five replacement
   *    expectations: 25 keys, 10 rows.
   */
  val RowSchema: KeySchema = KeySchema.variants(
    "a row of parity/schedule-baseline.json",
    ErrorVariant -> (InputKeys ++ Set("source", "error")),
    ErrorReplacementVariant -> (InputKeys ++ Set("source", "error", "replacedStartDate")),
    ResolvedVariant -> (InputKeys ++ Set("source") ++ ResolvedExpectationKeys),
    ResolvedReplacementVariant ->
      (InputKeys ++ Set("source", "replacedStartDate") ++ ResolvedExpectationKeys ++
        ReplaceExpectationKeys))

  val RowVariantKeys: Map[String, Set[String]] = RowSchema.variants.toMap

  val ResolvedVariants: Set[String] = Set(ResolvedVariant, ResolvedReplacementVariant)

  /**
   * One row of `schedule-baseline.json`. The matched variant is carried because the absence of a
   * value is not the absence of an expectation: `initialStub` and `finalStub` are legitimately JSON
   * null on a resolved row, and three of the `data_replace` keys are nullable in the same way.
   * [[variant]] is the variant of [[RowSchema]] this row's key set was validated against, so it is
   * exactly the keys the row carries - see [[declaredKeys]].
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
      variant: String)
      extends ParityRow {

    /**
     * The name this row is reported under. This fixture carries no `id` key, and its 879 rows share
     * sixteen `source` values, so the identity is derived: the source, the eleven inputs and -
     * where the row has one - the replacement date, without which two `data_replace` rows over one
     * base definition would collide.
     */
    override def id: String = {
      val replacement = replacedStartDate.fold("")(date => s" replaced=$date")
      s"$source ${definition.render}$replacement"
    }

    def declaredKeys: Set[String] = RowVariantKeys(variant)

    def isResolved: Boolean = ResolvedVariants.contains(variant)

    /**
     * Whether this row declares some of the resolved expectation set but not all of it. No variant
     * of [[RowSchema]] does, so a variant added with four of the seven keys would measure three
     * fewer expectations than it appeared to.
     */
    def isPartiallyResolved: Boolean = {
      val declared = ResolvedExpectationKeys.intersect(declaredKeys)
      declared.nonEmpty && declared.size != ResolvedExpectationKeys.size
    }

    /**
     * Whether the three date expectations of this row agree with one another. All three are derived
     * from the periods of one resolved schedule, so `periods[i].unadjustedStart ==
     * unadjustedDates[i]`, `periods[i].unadjustedEnd == unadjustedDates[i + 1]`, `periods[i].start
     * == adjustedDates[i]`, `periods[i].end == adjustedDates[i + 1]` and `periods.length ==
     * unadjustedDates.length - 1` hold by construction. A row that broke one would be three
     * mutually inconsistent expectations, at least one of which nothing could satisfy.
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
  // The decoders, in dependency order, which is also initialisation order. Every field derivation
  // is wrapped in `ParityHarness.strictObject`, so each captured object is held to its documented
  // key set; the row is written out because it reads the eleven input fields from its own cursor
  // and records which variant of `RowSchema` validated it.
  //-------------------------------------------------------------------------

  implicit val businessDayAdjustmentRowDecoder: Decoder[BusinessDayAdjustmentRow] =
    ParityHarness.strictObject(AdjustmentSchema)(deriveDecoder[BusinessDayAdjustmentRow])

  implicit val adjustableDateRowDecoder: Decoder[AdjustableDateRow] =
    ParityHarness.strictObject(AdjustableDateSchema)(deriveDecoder[AdjustableDateRow])

  implicit val periodRowDecoder: Decoder[PeriodRow] =
    ParityHarness.strictObject(PeriodSchema)(deriveDecoder[PeriodRow])

  /**
   * Reads the eleven input fields off whatever cursor it is given, without a key-set check: on a
   * row that cursor carries the row's other keys as well, so holding it to [[DefinitionSchema]]
   * would refuse every row. Those keys are the row's own key set, which [[RowSchema]] validates in
   * full.
   */
  val definitionFieldsDecoder: Decoder[DefinitionRow] = deriveDecoder[DefinitionRow]

  implicit val definitionRowDecoder: Decoder[DefinitionRow] =
    ParityHarness.strictObject(DefinitionSchema)(definitionFieldsDecoder)

  implicit val scheduleRowDecoder: Decoder[ScheduleRow] =
    ParityHarness.strictVariant(RowSchema) { variant =>
      Decoder.instance { cursor =>
        for {
          definition <- definitionFieldsDecoder(cursor)
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
          variant)
      }
    }

  val FactoryLabel: String = "PeriodicSchedule.of"

  val ScheduleLabel: String = "createSchedule(refData)"

  val ReplaceLabel: String = "replaceStartDate"

  //-------------------------------------------------------------------------
  // Rebuilding a captured definition. Every convention, frequency, calendar and adjustment is
  // resolved BY NAME, through the same public lookups any other caller uses, so the measurement
  // covers the name contract as well as the arithmetic. A name that cannot be parsed is a defect of
  // the implementation or of the fixture and not a parity result, so it is lifted into the effect
  // and ends the row.
  //-------------------------------------------------------------------------

  def frequencyOf(name: String): IO[Frequency] = ParityHarness.raise(Frequency.parse(name))

  def stubConventionOf(name: String): IO[StubConvention] =
    ParityHarness.raiseNec(StubConvention.parse(name))

  /**
   * Resolves a roll convention by its captured name. The member named `None` is reached through the
   * lookup like every other, never through the `NONE` identifier it is bound to.
   */
  def rollConventionOf(name: String): IO[RollConvention] =
    ParityHarness.raiseNec(RollConvention.parse(name))

  /**
   * Rebuilds a business day adjustment from its captured convention and calendar names.
   *
   * The calendar name is handed to `HolidayCalendarId.of` whole, composite or not: that factory
   * parses `'+'` and `'~'` and normalises, deduplicates and sorts the parts, and it is one of the
   * things being measured, so splitting the name here would replace it with a reimplementation.
   * Composite calendars are the ordinary case: 256 of the 879 rows adjust against `GBLO+USNY`,
   * which `ReferenceData.standard` holds no entry for, so those rows exercise the resolution
   * order: the full composite name first, and only when it is absent the parts resolved and
   * combined.
   */
  def adjustmentOf(row: BusinessDayAdjustmentRow): IO[BusinessDayAdjustment] =
    ParityHarness
      .raiseNec(BusinessDayConvention.parse(row.convention))
      .map(convention => BusinessDayAdjustment.of(convention, HolidayCalendarId.of(row.calendar)))

  def adjustableDateOf(row: AdjustableDateRow): IO[AdjustableDate] =
    row.adjustment
      .traverse(adjustmentOf)
      .map(adjustment => AdjustableDate.of(row.unadjusted, adjustment.getOrElse(BusinessDayAdjustment.NONE)))

  /**
   * Rebuilds a definition from its eleven captured fields. The outcome of the factory is returned
   * as it stands rather than lifted, because a refusal is an expectation of this fixture.
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

  /**
   * Measures one captured row, answering with everything that differed.
   *
   * The three branches are the three shapes a row can take: the definition is refused before
   * anything is generated, the row is a start-date replacement, or the row is a plain resolution.
   * Which branch a row takes is decided by the outcome of the factory and by the row's inputs,
   * never by its `source`: the rows whose `source` names an invalid definition are all named alike,
   * yet most are refused by the factory and the `Term`-with-explicit-stub row by generation.
   */
  def checkRow(row: ScheduleRow): IO[List[String]] =
    definitionOf(row.definition).map(built => checkBuilt(row, built))

  private def checkBuilt(
      row: ScheduleRow,
      built: EitherNec[Failure, PeriodicSchedule]): List[String] =
    built match {
      case Left(_) =>
        // The factory refused the definition. On an `error` row that is the expectation, checked
        // for its shape without requiring the `definition` attribute, since none exists to attach.
        if (row.error.isDefined) refusalNec(FactoryLabel, built, requireDefinition = false)
        else ParityHarness.assertRight(FactoryLabel, built)(_ => Nil)
      case Right(definition) =>
        row.replacedStartDate match {
          case Some(replacement) => checkReplacement(row, definition, replacement)
          case None => checkResolution(row, definition)
        }
    }

  //-------------------------------------------------------------------------
  // Refusals. Every refusal is asserted through `ParityHarness.assertLeft` on the error channel -
  // there is no `intercept`, no `try`/`catch` and no `.get` on an `Either` in this file - and then
  // for its shape, because a refusal for the wrong reason is not parity. Two entry points, one per
  // error type: `EitherNec` for the factory and the replacement, which accumulate, and `Either`
  // for the generation steps, which report a single cause.
  //-------------------------------------------------------------------------

  private def refusalNec(
      label: String,
      actual: EitherNec[Failure, Any],
      requireDefinition: Boolean): List[String] =
    ParityHarness.assertLeft(label, actual) :::
      actual.swap.toOption.toList.flatMap(failures =>
        failures.toChain.toList.flatMap(failure =>
          failureShapeMessages(label, failure, requireDefinition)))

  private def refusal(
      label: String,
      actual: Either[Failure, Any],
      requireDefinition: Boolean): List[String] =
    ParityHarness.assertLeft(label, actual) :::
      actual.swap.toOption.toList.flatMap(failure =>
        failureShapeMessages(label, failure, requireDefinition))

  /**
   * Checks that a refusal is the refusal this module is specified to report.
   *
   * Two properties, and the captured message is not one of them. Every refusal of this layer is a
   * `Failure.Invalid`, so the reason is required to be `INVALID` for every cause; and every failure
   * a '''generation''' step reports attaches the definition it rejected under the `definition`
   * attribute, so that attribute is required of those. It is not required of a failure the factory
   * reported: a definition the factory refuses never becomes a `PeriodicSchedule`, and those
   * failures come from the order checks with no attributes at all. The wording is never compared.
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


  private def checkResolution(row: ScheduleRow, definition: PeriodicSchedule): List[String] = {
    val schedule = definition.createSchedule(referenceData)
    if (row.error.isDefined) {
      refusal(ScheduleLabel, schedule, requireDefinition = true)
    } else {
      ParityHarness.assertRight(ScheduleLabel, schedule)(resolved => scheduleMessages(row, resolved)) :::
        dateListMessages(row, row.definition, definition)
    }
  }

  private def scheduleMessages(row: ScheduleRow, schedule: Schedule): List[String] =
    exactly("unadjustedDates", schedule.unadjustedDates.toList, row.unadjustedDates.map(_.toList)) :::
      exactly("adjustedDates", adjustedDatesOf(schedule), row.adjustedDates.map(_.toList)) :::
      periodsMessages(schedule.periods.toList, row.periods) :::
      whenDeclared(row, "initialStub")(stubMessages("initialStub", schedule.initialStub, row.initialStub)) :::
      whenDeclared(row, "finalStub")(stubMessages("finalStub", schedule.finalStub, row.finalStub)) :::
      exactly("resolvedRollConvention", schedule.rollConvention.name, row.resolvedRollConvention) :::
      exactly("resolvedFrequency", schedule.periodicFrequency.name, row.resolvedFrequency)

  /**
   * The adjusted boundary dates of a schedule: the adjusted start date followed by the adjusted end
   * of each period, one date more than there are periods, which `Schedule` leaves to be read off
   * the periods.
   */
  private def adjustedDatesOf(schedule: Schedule): List[LocalDate] =
    schedule.adjustedStartDate :: schedule.periods.toList.map(_.endDate)

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

  private def periodMessages(
      label: String,
      actual: SchedulePeriod,
      expected: PeriodRow): List[String] =
    ParityHarness.assertExact(s"$label.unadjustedStart", actual.unadjustedStartDate, expected.unadjustedStart) :::
      ParityHarness.assertExact(s"$label.unadjustedEnd", actual.unadjustedEndDate, expected.unadjustedEnd) :::
      ParityHarness.assertExact(s"$label.start", actual.startDate, expected.start) :::
      ParityHarness.assertExact(s"$label.end", actual.endDate, expected.end)

  /**
   * Compares a stub, presence first: a stub the fixture records as absent is an expectation in its
   * own right, so a schedule that produced one where none was captured is reported as that.
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
   * Measures the three date-list members against the lists the row carries.
   *
   * `createUnadjustedDates(refData)` and `createAdjustedDates(refData)` are a different route to
   * the dates of the resolved schedule, and every row carrying those lists is held to that
   * identity. `createUnadjustedDates()` is not the same answer for every definition, so it is
   * measured only under the guard it needs: the start date carries no business day adjustment of
   * its own and the declared roll convention is not `EOM`, because recovering the pre-adjusted
   * start date needs the holiday calendars. One `data_replace` row is where the difference shows:
   * the bare form returns a long initial stub beginning at the replacement date while
   * `createSchedule` rolls the start onto the 17th, and that row's own answer is carried under
   * `replacedUnadjustedDates`.
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

  private def reproducesUnadjustedWithoutReferenceData(inputs: DefinitionRow): Boolean =
    inputs.startDateBusinessDayAdjustment.isEmpty && !inputs.rollConvention.contains(EomName)


  /**
   * Measures a row that records a `replaceStartDate` operation.
   *
   * The row carries the '''base''' definition and the replacement date, so the whole operation is
   * reproduced rather than only its result: the replacement is applied to the base, and then the
   * post-replacement definition, the unadjusted dates it creates, its stub convention, its last
   * regular end date, its roll convention and the schedule it resolves to are each compared. The
   * one refused row is the only coverage of `replaceStartDate` rejecting a date after the end date,
   * and its failure carries the definition. `replacedUnadjustedDates` is compared against the
   * '''bare''' `createUnadjustedDates()`, not against this row's `unadjustedDates`: the two
   * disagree on one of these rows (see [[dateListMessages]]).
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
   * Compares a definition against the eleven captured fields of one. Every field is compared,
   * because `replaceStartDate` is specified by what it changes '''and''' by what it leaves alone:
   * the start date becomes the replacement, the start-date adjustment becomes
   * `BusinessDayAdjustment.NONE`, the first regular start date and the override start date are
   * cleared, the stub convention and last regular end date may be rewritten, and the other four
   * fields must be untouched.
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

  private def exactly[A](label: String, actual: => A, expected: Option[A]): List[String] =
    expected.toList.flatMap(value => ParityHarness.assertExact(label, actual, value))

  private def rightExactly[A](
      label: String,
      actual: Either[Failure, A],
      expected: Option[A]): List[String] =
    expected.toList.flatMap(value =>
      ParityHarness.assertRight(label, actual)(produced =>
        ParityHarness.assertExact(label, produced, value)))

  private def whenDeclared(row: ScheduleRow, key: String)(messages: => List[String]): List[String] =
    if (row.declaredKeys.contains(key)) messages else Nil

  //-------------------------------------------------------------------------
  // The hand-built objects the strictness tests decode: one row of each documented variant and one
  // of each nested shape, built key by key so that a test can add, drop or rename a key. They are
  // text rather than models because a key set is exactly what a model cannot express, and no
  // expectation lives here: the values are plausible only so that a refusal is attributable to the
  // key check.
  //-------------------------------------------------------------------------

  type SampleFields = Vector[(String, String)]

  def quoted(value: String): String = "\"" + value + "\""

  def objectText(fields: SampleFields): String =
    fields.map { case (key, value) => s"${quoted(key)}: $value" }.mkString("{", ", ", "}")

  val UndocumentedKey: String = "capturedAt"

  /**
   * The name a renamed key takes, which no shape of this document knows either. Not a superstring
   * of the key it replaces: a refusal has to name the unknown key '''and''' the documented key that
   * is now missing, and one fragment of the message must not satisfy both assertions.
   */
  def renamed(key: String): String = s"renamed${key.capitalize}"

  def withUnknownKey(fields: SampleFields): SampleFields =
    fields ++ Vector(UndocumentedKey -> quoted("2026-01-01T00:00:00Z"))

  def without(fields: SampleFields, key: String): SampleFields = fields.filterNot(_._1 == key)

  def renaming(fields: SampleFields, key: String): SampleFields =
    fields.map { case (name, value) => if (name == key) renamed(name) -> value else name -> value }

  def replacing(fields: SampleFields, key: String, value: String): SampleFields =
    fields.map { case (name, existing) => if (name == key) name -> value else name -> existing }

  /**
   * A key every row variant declares. Dropping it cannot turn one documented row shape into
   * another, which a key like `replacedStartDate` would: the 14-key error row without it '''is'''
   * the 13-key error row.
   */
  val RowRequiredKey: String = "frequency"

  val RowRenamedKey: String = "source"

  val SampleAdjustment: SampleFields =
    Vector("convention" -> quoted("ModifiedFollowing"), "calendar" -> quoted(CompositeCalendar))

  val SampleAdjustableDate: SampleFields =
    Vector("unadjusted" -> quoted("2014-06-17"), "adjustment" -> objectText(SampleAdjustment))

  val SamplePeriod: SampleFields = Vector(
    "unadjustedStart" -> quoted("2014-06-17"),
    "unadjustedEnd" -> quoted("2014-07-17"),
    "start" -> quoted("2014-06-17"),
    "end" -> quoted("2014-07-17"))

  val SampleInputs: SampleFields = Vector(
    "startDate" -> quoted("2014-06-17"),
    "endDate" -> quoted("2014-07-17"),
    "frequency" -> quoted("P1M"),
    "businessDayAdjustment" -> objectText(SampleAdjustment),
    "startDateBusinessDayAdjustment" -> "null",
    "endDateBusinessDayAdjustment" -> "null",
    "stubConvention" -> quoted("LongInitial"),
    "rollConvention" -> quoted("Day17"),
    "firstRegularStartDate" -> "null",
    "lastRegularEndDate" -> "null",
    "overrideStartDate" -> objectText(SampleAdjustableDate))

  val SampleResolvedExpectations: SampleFields = Vector(
    "unadjustedDates" -> """["2014-06-17", "2014-07-17"]""",
    "adjustedDates" -> """["2014-06-17", "2014-07-17"]""",
    "periods" -> s"[${objectText(SamplePeriod)}]",
    "initialStub" -> objectText(SamplePeriod),
    "finalStub" -> "null",
    "resolvedRollConvention" -> quoted("Day17"),
    "resolvedFrequency" -> quoted("P1M"))

  val SampleReplaceExpectations: SampleFields = Vector(
    "replacedDefinition" -> objectText(SampleInputs),
    "replacedUnadjustedDates" -> """["2014-05-19", "2014-07-17"]""",
    "expectedStubConvention" -> quoted("LongInitial"),
    "expectedLastRegularEndDate" -> "null",
    "expectedRollConvention" -> quoted("Day17"))

  /**
   * A row of the `error` variant. The roll convention is `IMM` so that the refusal quoted in
   * `error` is the one these inputs really produce: 2014-06-17 is a Tuesday, and `IMM` rolls to the
   * third Wednesday.
   */
  val SampleErrorRow: SampleFields =
    replacing(SampleInputs, "rollConvention", quoted("IMM")) ++ Vector(
      "source" -> quoted(GridSource),
      "error" -> quoted(
        "ScheduleException: Date '2014-06-17' does not match roll convention 'IMM' when " +
          "starting to roll forwards"))

  val SampleErrorReplacementRow: SampleFields = SampleInputs ++ Vector(
    "source" -> quoted(ReplaceSource),
    "replacedStartDate" -> quoted("2014-09-04"),
    "error" -> quoted("IllegalArgumentException: Cannot alter leg to have start date after end date"))

  val SampleResolvedRow: SampleFields =
    SampleInputs ++ Vector("source" -> quoted(GridSource)) ++ SampleResolvedExpectations

  val SampleResolvedReplacementRow: SampleFields =
    SampleInputs ++
      Vector("source" -> quoted(ReplaceSource), "replacedStartDate" -> quoted("2014-05-19")) ++
      SampleResolvedExpectations ++ SampleReplaceExpectations

  val SampleRows: Vector[(String, SampleFields)] = Vector(
    ErrorVariant -> SampleErrorRow,
    ErrorReplacementVariant -> SampleErrorReplacementRow,
    ResolvedVariant -> SampleResolvedRow,
    ResolvedReplacementVariant -> SampleResolvedReplacementRow)

  final case class NestedSample(
      shape: String,
      host: SampleFields,
      key: String,
      fields: SampleFields,
      required: String)

  val NestedSamples: Vector[NestedSample] = Vector(
    NestedSample(
      AdjustmentSchema.shape,
      SampleResolvedRow,
      "businessDayAdjustment",
      SampleAdjustment,
      "convention"),
    NestedSample(
      AdjustableDateSchema.shape,
      SampleResolvedRow,
      "overrideStartDate",
      SampleAdjustableDate,
      "unadjusted"),
    NestedSample(PeriodSchema.shape, SampleResolvedRow, "initialStub", SamplePeriod, "start"),
    NestedSample(
      DefinitionSchema.shape,
      SampleResolvedReplacementRow,
      "replacedDefinition",
      SampleInputs,
      "frequency"))

  private val EomName: String = "EOM"

  private val NoAdjustment: BusinessDayAdjustmentRow =
    BusinessDayAdjustmentRow("NoAdjust", "NoHolidays")
}
