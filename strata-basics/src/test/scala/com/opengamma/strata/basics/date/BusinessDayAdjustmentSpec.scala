/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate

import cats.Hash
import cats.Show
import cats.syntax.apply._

import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor3

import com.opengamma.strata.basics.RefDataReader
import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[BusinessDayAdjustment]], ported from the Java `BusinessDayAdjustmentTest`.
 *
 * All eight methods of the Java class are kept, each under the name the Java method had -
 * `test_basics`, `test_adjustDate`, `test_noAdjust_constant`, `test_noAdjust_factory`,
 * `test_noAdjust_normalized`, `coverage`, `coverage_builder` and `test_serialization` - so that a
 * Java test method and a test of this suite stay in one-to-one correspondence and the
 * method-level traceability the migration manifest records resolves on the pair of suite class
 * and test name. Nothing is added under a name of its own: everything this port asserts beyond
 * the Java assertions belongs to whichever of the eight methods already owned that ground, which
 * is why `test_adjustDate` is the long one.
 *
 * ===The provider table===
 *
 * `test_adjustDate` was parameterised in Java from `BusinessDayConventionTest#data_convention`
 * through a `@MethodSource` naming that other test class: 72 rows of convention, input date and
 * expected date, over a weekend-only calendar. The rows are transcribed here rather than read
 * from the sibling spec, so that this file depends on the production types alone and can be read
 * and changed without a second test class in hand. They are the Java rows in the Java order, and
 * the same table is exercised from the other side - the convention applied to a calendar
 * directly, with no reference data in the picture - by [[BusinessDayConventionSpec]].
 *
 * ===How the shape of the port changes the assertions===
 *
 *   - `adjust` and `resolve` return `Either[Failure, _]` where the Java methods returned a bare
 *     value and threw `ReferenceDataNotFoundException` for a calendar the reference data does not
 *     hold. Every ported assertion is therefore that the outcome is a success carrying the Java
 *     value, and the failure path - which the Java class never exercised, having nothing but an
 *     exception to exercise it with - is asserted at the end of `test_adjustDate` for all seven
 *     conventions, by the reason the failure carries rather than by its message.
 *   - Java's `test_adjustDate` asserted two paths, `adjust(date, refData)` and
 *     `resolve(refData).adjust(date)`. This port has a third, `toReader.run(refData)`, which is
 *     the same resolution expressed as a value awaiting reference data, so all three are driven
 *     through every row: supplying reference data once, or later, or in composition with another
 *     lookup, must not change what is computed.
 *   - `coverage` called `coverImmutableBean`, a reflective sweep of a Joda bean's properties,
 *     equality, hashing and rendering. There is no bean and no reflection in this port, so what
 *     the sweep stood for is asserted directly over the same value the Java method swept.
 *   - `coverage_builder` used the generated Joda builder, which has no counterpart here:
 *     construction is total, the case-class constructor is the whole of it, and `with*` methods
 *     would have nothing to do that `copy` does not. The test asserts instead that the three
 *     ways of building the same value - `of`, `apply` and `copy` - agree field by field.
 *   - `test_serialization` asserted Joda-Beans binary and JSON round trips. Joda wire
 *     compatibility is out of scope for this port, so the test is a circe round trip through the
 *     derived codec, which additionally pins the concrete document: the two Java property names,
 *     in declaration order, each written as the bare string its own type publishes. The
 *     property-based round trip over every codec-bearing type of the module lives in
 *     `json.JsonRoundTripSpec`; what a property cannot state is the document itself, which is
 *     what is stated here.
 *
 * @see [[BusinessDayConventionSpec]] for the adjustment rules themselves, applied without
 *   reference data
 * @see [[HolidayCalendarIdSpec]] for the calendar resolution this type delegates to
 */
class BusinessDayAdjustmentSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /** The reference data the Java class used throughout, which holds every built-in calendar. */
  private val REF_DATA: ReferenceData = ReferenceData.standard

  // the dates of the Java provider, named as the Java class named them, so that a row of the
  // table below can be read against the Java source without translating a date
  private val FRI_2014_07_11: LocalDate = LocalDate.of(2014, 7, 11)
  private val SAT_2014_07_12: LocalDate = LocalDate.of(2014, 7, 12)
  private val SUN_2014_07_13: LocalDate = LocalDate.of(2014, 7, 13)
  private val MON_2014_07_14: LocalDate = LocalDate.of(2014, 7, 14)

  private val FRI_2014_08_29: LocalDate = LocalDate.of(2014, 8, 29)
  private val SAT_2014_08_30: LocalDate = LocalDate.of(2014, 8, 30)
  private val SUN_2014_08_31: LocalDate = LocalDate.of(2014, 8, 31)
  private val MON_2014_09_01: LocalDate = LocalDate.of(2014, 9, 1)

  private val FRI_2014_10_31: LocalDate = LocalDate.of(2014, 10, 31)
  private val SAT_2014_11_01: LocalDate = LocalDate.of(2014, 11, 1)
  private val SUN_2014_11_02: LocalDate = LocalDate.of(2014, 11, 2)
  private val MON_2014_11_03: LocalDate = LocalDate.of(2014, 11, 3)

  private val FRI_2014_11_14: LocalDate = LocalDate.of(2014, 11, 14)
  private val SAT_2014_11_15: LocalDate = LocalDate.of(2014, 11, 15)
  private val SUN_2014_11_16: LocalDate = LocalDate.of(2014, 11, 16)
  private val MON_2014_11_17: LocalDate = LocalDate.of(2014, 11, 17)

  /** A Saturday whose following Monday, 25 August 2014, is the London summer bank holiday. */
  private val SAT_2014_08_23: LocalDate = LocalDate.of(2014, 8, 23)

  /** The Tuesday after that bank holiday, which is the next London business day. */
  private val TUE_2014_08_26: LocalDate = LocalDate.of(2014, 8, 26)

  /** Independence Day 2019, a Thursday: a holiday in New York and a business day in London. */
  private val THU_2019_07_04: LocalDate = LocalDate.of(2019, 7, 4)

  /** The Friday after Independence Day 2019, a business day in both. */
  private val FRI_2019_07_05: LocalDate = LocalDate.of(2019, 7, 5)

  /** An identifier no reference data of this library holds, used to observe the failure path. */
  private val UNKNOWN_CALENDAR: HolidayCalendarId = HolidayCalendarId.of("XXXX")

  /**
   * The 72 rows of the Java `BusinessDayConventionTest#data_convention` provider.
   *
   * Each row is a convention, a date to adjust and the date the library being ported produces
   * for it against the `Sat/Sun` calendar. The four groups of dates are chosen so that every
   * branch of every convention is reached: a Friday and a Monday that need no adjustment, a
   * weekend inside a month, a weekend that straddles a month end - which is what separates
   * `ModifiedFollowing` from `Following` - and a weekend that straddles the middle of a month,
   * which is what separates `ModifiedFollowingBiMonthly` from `ModifiedFollowing`.
   */
  private val dataConvention: TableFor3[BusinessDayConvention, LocalDate, LocalDate] = Table(
    ("convention", "input", "expected"),
    (BusinessDayConventions.NO_ADJUST, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.NO_ADJUST, SAT_2014_07_12, SAT_2014_07_12),
    (BusinessDayConventions.NO_ADJUST, SUN_2014_07_13, SUN_2014_07_13),
    (BusinessDayConventions.NO_ADJUST, MON_2014_07_14, MON_2014_07_14),
    (BusinessDayConventions.FOLLOWING, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.FOLLOWING, SAT_2014_07_12, MON_2014_07_14),
    (BusinessDayConventions.FOLLOWING, SUN_2014_07_13, MON_2014_07_14),
    (BusinessDayConventions.FOLLOWING, MON_2014_07_14, MON_2014_07_14),
    (BusinessDayConventions.FOLLOWING, FRI_2014_08_29, FRI_2014_08_29),
    (BusinessDayConventions.FOLLOWING, SAT_2014_08_30, MON_2014_09_01),
    (BusinessDayConventions.FOLLOWING, SUN_2014_08_31, MON_2014_09_01),
    (BusinessDayConventions.FOLLOWING, MON_2014_09_01, MON_2014_09_01),
    (BusinessDayConventions.FOLLOWING, FRI_2014_10_31, FRI_2014_10_31),
    (BusinessDayConventions.FOLLOWING, SAT_2014_11_01, MON_2014_11_03),
    (BusinessDayConventions.FOLLOWING, SUN_2014_11_02, MON_2014_11_03),
    (BusinessDayConventions.FOLLOWING, MON_2014_11_03, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.MODIFIED_FOLLOWING, SAT_2014_07_12, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_FOLLOWING, SUN_2014_07_13, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_FOLLOWING, MON_2014_07_14, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_FOLLOWING, FRI_2014_08_29, FRI_2014_08_29),
    // modified: the following business day is in the next month, so the move is backwards
    (BusinessDayConventions.MODIFIED_FOLLOWING, SAT_2014_08_30, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_FOLLOWING, SUN_2014_08_31, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_FOLLOWING, MON_2014_09_01, MON_2014_09_01),
    (BusinessDayConventions.MODIFIED_FOLLOWING, FRI_2014_10_31, FRI_2014_10_31),
    (BusinessDayConventions.MODIFIED_FOLLOWING, SAT_2014_11_01, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING, SUN_2014_11_02, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING, MON_2014_11_03, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SAT_2014_07_12, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SUN_2014_07_13, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, MON_2014_07_14, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, FRI_2014_08_29, FRI_2014_08_29),
    // modified: a month end is also a half-month end
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SAT_2014_08_30, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SUN_2014_08_31, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, MON_2014_09_01, MON_2014_09_01),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, FRI_2014_10_31, FRI_2014_10_31),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SAT_2014_11_01, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SUN_2014_11_02, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, MON_2014_11_03, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, FRI_2014_11_14, FRI_2014_11_14),
    // modified: the 15th is a half-month boundary, so the move is backwards over it
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SAT_2014_11_15, FRI_2014_11_14),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SUN_2014_11_16, MON_2014_11_17),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, MON_2014_11_17, MON_2014_11_17),
    (BusinessDayConventions.PRECEDING, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.PRECEDING, SAT_2014_07_12, FRI_2014_07_11),
    (BusinessDayConventions.PRECEDING, SUN_2014_07_13, FRI_2014_07_11),
    (BusinessDayConventions.PRECEDING, MON_2014_07_14, MON_2014_07_14),
    (BusinessDayConventions.PRECEDING, FRI_2014_08_29, FRI_2014_08_29),
    (BusinessDayConventions.PRECEDING, SAT_2014_08_30, FRI_2014_08_29),
    (BusinessDayConventions.PRECEDING, SUN_2014_08_31, FRI_2014_08_29),
    (BusinessDayConventions.PRECEDING, MON_2014_09_01, MON_2014_09_01),
    (BusinessDayConventions.PRECEDING, FRI_2014_10_31, FRI_2014_10_31),
    (BusinessDayConventions.PRECEDING, SAT_2014_11_01, FRI_2014_10_31),
    (BusinessDayConventions.PRECEDING, SUN_2014_11_02, FRI_2014_10_31),
    (BusinessDayConventions.PRECEDING, MON_2014_11_03, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_PRECEDING, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.MODIFIED_PRECEDING, SAT_2014_07_12, FRI_2014_07_11),
    (BusinessDayConventions.MODIFIED_PRECEDING, SUN_2014_07_13, FRI_2014_07_11),
    (BusinessDayConventions.MODIFIED_PRECEDING, MON_2014_07_14, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_PRECEDING, FRI_2014_08_29, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_PRECEDING, SAT_2014_08_30, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_PRECEDING, SUN_2014_08_31, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_PRECEDING, MON_2014_09_01, MON_2014_09_01),
    (BusinessDayConventions.MODIFIED_PRECEDING, FRI_2014_10_31, FRI_2014_10_31),
    // modified: the preceding business day is in the previous month, so the move is forwards
    (BusinessDayConventions.MODIFIED_PRECEDING, SAT_2014_11_01, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_PRECEDING, SUN_2014_11_02, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_PRECEDING, MON_2014_11_03, MON_2014_11_03),
    (BusinessDayConventions.NEAREST, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.NEAREST, SAT_2014_07_12, FRI_2014_07_11),
    (BusinessDayConventions.NEAREST, SUN_2014_07_13, MON_2014_07_14),
    (BusinessDayConventions.NEAREST, MON_2014_07_14, MON_2014_07_14))

  //-------------------------------------------------------------------------
  test("test_basics") {
    val test: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.SAT_SUN)

    test.convention shouldBe BusinessDayConventions.MODIFIED_FOLLOWING
    test.calendar shouldBe HolidayCalendarIds.SAT_SUN
    test.toString shouldBe "ModifiedFollowing using calendar Sat/Sun"

    // `of` is the factory of the library being ported and the constructor of the case class is
    // public, construction being total, so the two are the same value - which is what lets a
    // ported call site read unchanged while new code may use either.
    test shouldBe BusinessDayAdjustment(
      BusinessDayConventions.MODIFIED_FOLLOWING,
      HolidayCalendarIds.SAT_SUN)

    // A composite calendar is named in full by the rendering, as in the Java original.
    BusinessDayAdjustment
      .of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarId.of("GBLO+USNY"))
      .toString shouldBe "ModifiedFollowing using calendar GBLO+USNY"
  }

  //-------------------------------------------------------------------------
  test("test_adjustDate") {
    // The Java parameterised method: every row of the provider, against the `Sat/Sun` calendar
    // it used. Java asserted the unresolved and the resolved path; this port adds the reader, and
    // all three must produce the value the library being ported produced.
    forAll(dataConvention) { (convention: BusinessDayConvention, input: LocalDate, expected: LocalDate) =>
      val test: BusinessDayAdjustment = BusinessDayAdjustment.of(convention, HolidayCalendarIds.SAT_SUN)

      withClue(s"$test adjusting $input: ") {
        test.adjust(input, REF_DATA) should haveValue(expected)
        test.resolve(REF_DATA).map(adjuster => adjuster.adjust(input)) should haveValue(expected)
        test.toReader.run(REF_DATA).map(adjuster => adjuster.adjust(input)) should haveValue(expected)
      }
    }

    // Beyond the Java provider, which uses a weekend-only calendar throughout: adjustment against
    // holiday data that has to be resolved from reference data. Both rows are values of the
    // library being ported - the London summer bank holiday of 2014 falls on Monday the 25th, so
    // a Saturday is followed by the Tuesday, and the month-end Saturday is held back to the
    // Friday because the following business day would leave August.
    val london: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.GBLO)
    london.adjust(SAT_2014_08_23, REF_DATA) should haveValue(TUE_2014_08_26)

    val londonModified: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.GBLO)
    londonModified.adjust(SAT_2014_08_30, REF_DATA) should haveValue(FRI_2014_08_29)

    // A composite calendar resolves component-wise and observes both sets of holidays, so the
    // Independence Day holiday of New York moves a date that is a London business day.
    val both: BusinessDayAdjustment =
      BusinessDayAdjustment.of(
        BusinessDayConventions.FOLLOWING,
        HolidayCalendarIds.GBLO.combinedWith(HolidayCalendarIds.USNY))
    both.adjust(THU_2019_07_04, REF_DATA) should haveValue(FRI_2019_07_05)
    london.adjust(THU_2019_07_04, REF_DATA) should haveValue(THU_2019_07_04)

    // `resolve` looks the calendar up once and binds it into the adjuster returned, which is the
    // point of having the method at all: a run of dates costs one lookup rather than one per
    // date. What that has to leave unchanged is the answer, so one resolved adjuster is applied
    // to many dates - every date of 2015, for all seven conventions, against a calendar with
    // real holidays - and compared with resolving afresh for each of them.
    val dates: List[LocalDate] =
      Iterator
        .iterate(LocalDate.of(2015, 1, 1))(date => date.plusDays(1L))
        .takeWhile(date => date.getYear == 2015)
        .toList
    dates should have size 365

    BusinessDayConvention.values.toList.foreach { convention =>
      val test: BusinessDayAdjustment = BusinessDayAdjustment.of(convention, HolidayCalendarIds.GBLO)
      val resolvedOnce: Either[Failure, DateAdjuster] = test.resolve(REF_DATA)
      val readOnce: Either[Failure, DateAdjuster] = test.toReader.run(REF_DATA)
      resolvedOnce should beSuccess
      readOnce should beSuccess

      dates.foreach { date =>
        withClue(s"$test adjusting $date: ") {
          val perDate: Either[Failure, LocalDate] = test.adjust(date, REF_DATA)
          perDate should beSuccess
          resolvedOnce.map(adjuster => adjuster.adjust(date)) shouldBe perDate
          readOnce.map(adjuster => adjuster.adjust(date)) shouldBe perDate
        }
      }
    }

    // Two readers compose, which is the reason the reader form exists: several adjustments are
    // assembled while no reference data is available and the data is supplied once, to the
    // composition, rather than to each of them.
    val londonAndNewYork: RefDataReader[(LocalDate, LocalDate)] =
      (
        london.toReader,
        BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.USNY).toReader)
        .mapN((londonAdjuster, newYorkAdjuster) =>
          (londonAdjuster.adjust(THU_2019_07_04), newYorkAdjuster.adjust(THU_2019_07_04)))
    londonAndNewYork.run(REF_DATA) should haveValue((THU_2019_07_04, FRI_2019_07_05))

    // The failure path, which the Java class never exercised because the Java methods threw
    // `ReferenceDataNotFoundException`. For every convention, and through all three entry points,
    // a calendar the reference data cannot supply is reported as a missing-data failure and
    // nothing is raised.
    BusinessDayConvention.values.toList.foreach { convention =>
      val test: BusinessDayAdjustment = BusinessDayAdjustment.of(convention, UNKNOWN_CALENDAR)
      withClue(s"$test: ") {
        test.adjust(MON_2014_07_14, REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
        test.resolve(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
        test.toReader.run(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
        noException should be thrownBy test.adjust(MON_2014_07_14, REF_DATA)
      }
    }

    // The failure names the identifier that could not be found and carries it as an attribute, so
    // a caller can act on it without reading the message.
    val failed: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, UNKNOWN_CALENDAR)
    failed.adjust(MON_2014_07_14, REF_DATA) should
      haveFailureMessageMatching("Reference data not found for identifier 'XXXX'")
    failed.adjust(MON_2014_07_14, REF_DATA).swap.map(failure => failure.attributes.get("id")) shouldBe
      Right(Some("XXXX"))

    // A composition one reader of which cannot resolve fails as a whole, with that reader's
    // failure rather than one about the composition.
    val partlyUnknown: RefDataReader[(LocalDate, LocalDate)] =
      (london.toReader, failed.toReader)
        .mapN((londonAdjuster, unknownAdjuster) =>
          (londonAdjuster.adjust(THU_2019_07_04), unknownAdjuster.adjust(THU_2019_07_04)))
    partlyUnknown.run(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)

    // Empty reference data holds nothing at all - not even the no-holidays calendar - so every
    // adjustment fails against it, including `NONE`. That is the behaviour of the library being
    // ported rather than a defect of this port: `ReferenceData.empty()` does not contain
    // `NoHolidays` there either.
    BusinessDayConvention.values.toList.foreach { convention =>
      val test: BusinessDayAdjustment =
        BusinessDayAdjustment.of(convention, HolidayCalendarIds.NO_HOLIDAYS)
      withClue(s"$test against empty reference data: ") {
        test.adjust(SAT_2014_07_12, ReferenceData.empty) should beFailureWith(FailureReason.MISSING_DATA)
      }
    }
    BusinessDayAdjustment.NONE.adjust(SAT_2014_07_12, ReferenceData.empty) should
      beFailureWith(FailureReason.MISSING_DATA)
    ReferenceData.empty.containsValue(HolidayCalendarIds.NO_HOLIDAYS) shouldBe false

    // A composite calendar one part of which is missing names both the part and the composite,
    // which is the failure the identifier reports and the context a caller needs.
    val composite: BusinessDayAdjustment =
      BusinessDayAdjustment.of(
        BusinessDayConventions.FOLLOWING,
        HolidayCalendarIds.GBLO.combinedWith(UNKNOWN_CALENDAR))
    composite.adjust(MON_2014_07_14, REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    composite.adjust(MON_2014_07_14, REF_DATA) should
      haveFailureMessageMatching(
        "Reference data not found for 'XXXX' of type 'HolidayCalendarId' when finding 'GBLO\\+XXXX'")
  }

  //-------------------------------------------------------------------------
  test("test_noAdjust_constant") {
    val test: BusinessDayAdjustment = BusinessDayAdjustment.NONE

    test.convention shouldBe BusinessDayConventions.NO_ADJUST
    test.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    // The adjustment that makes no adjustment renders as its convention alone, because naming a
    // calendar that is never consulted would say something untrue about it.
    test.toString shouldBe "NoAdjust"

    // And it is the identity: a holiday given to it comes back unaltered, through each of the
    // three entry points, against reference data that holds the no-holidays calendar.
    test.adjust(SAT_2014_07_12, REF_DATA) should haveValue(SAT_2014_07_12)
    test.adjust(SAT_2014_07_12, ReferenceData.minimal) should haveValue(SAT_2014_07_12)
    test.resolve(REF_DATA).map(adjuster => adjuster.adjust(SAT_2014_07_12)) should
      haveValue(SAT_2014_07_12)
    test.toReader.run(REF_DATA).map(adjuster => adjuster.adjust(SAT_2014_07_12)) should
      haveValue(SAT_2014_07_12)
  }

  test("test_noAdjust_factory") {
    val test: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.NO_ADJUST, HolidayCalendarIds.NO_HOLIDAYS)

    test.convention shouldBe BusinessDayConventions.NO_ADJUST
    test.calendar shouldBe HolidayCalendarIds.NO_HOLIDAYS
    test.toString shouldBe "NoAdjust"

    // Built by the factory rather than taken from the constant, it is the same value - equality
    // is by field, so the constant is not privileged - and it renders identically, which is what
    // makes the rendering rule a property of the value and not of the constant.
    test shouldBe BusinessDayAdjustment.NONE
    Hash[BusinessDayAdjustment].eqv(test, BusinessDayAdjustment.NONE) shouldBe true
    test.toString shouldBe BusinessDayAdjustment.NONE.toString
  }

  test("test_noAdjust_normalized") {
    val test: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.NO_ADJUST, HolidayCalendarIds.SAT_SUN)

    test.convention shouldBe BusinessDayConventions.NO_ADJUST
    test.calendar shouldBe HolidayCalendarIds.SAT_SUN
    // The calendar is kept and named even though the convention never consults it, exactly as in
    // the library being ported: a caller may later replace the convention and expect the calendar
    // to still be there, so this value is deliberately NOT normalised to `NONE`.
    test.toString shouldBe "NoAdjust using calendar Sat/Sun"
    test should not be BusinessDayAdjustment.NONE
    Hash[BusinessDayAdjustment].eqv(test, BusinessDayAdjustment.NONE) shouldBe false

    // It still makes no adjustment, because the convention decides that and not the calendar.
    test.adjust(SAT_2014_07_12, REF_DATA) should haveValue(SAT_2014_07_12)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java method was `coverImmutableBean(BusinessDayAdjustment.of(MODIFIED_FOLLOWING,
    // SAT_SUN))`: a reflective sweep over a Joda bean's properties, equality, hashing and
    // rendering. There is no bean and no reflection in this port, so that sweep has no target and
    // the properties it stood for are asserted directly, over exactly the value the Java method
    // named.
    val test: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.SAT_SUN)
    val same: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.SAT_SUN)
    val otherConvention: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.SAT_SUN)
    val otherCalendar: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.GBLO)

    // The two properties read back as they were given.
    test.convention shouldBe BusinessDayConventions.MODIFIED_FOLLOWING
    test.calendar shouldBe HolidayCalendarIds.SAT_SUN

    // Equality and hashing are by field, so a value built twice is one value, and a difference in
    // either field is a different value.
    test shouldBe same
    test.hashCode shouldBe same.hashCode
    Hash[BusinessDayAdjustment].eqv(test, same) shouldBe true
    Hash[BusinessDayAdjustment].hash(test) shouldBe Hash[BusinessDayAdjustment].hash(same)
    test should not be otherConvention
    test should not be otherCalendar
    Hash[BusinessDayAdjustment].eqv(test, otherConvention) shouldBe false
    Hash[BusinessDayAdjustment].eqv(test, otherCalendar) shouldBe false

    // A value of another type is not equal to an adjustment, which the reflective sweep also
    // checked by handing the bean a foreign object.
    test.equals("ModifiedFollowing using calendar Sat/Sun") shouldBe false

    // `copy` changes one field and leaves the other, which is the property sweep's write half.
    test.copy(calendar = HolidayCalendarIds.GBLO) shouldBe otherCalendar
    test.copy(convention = BusinessDayConventions.FOLLOWING) shouldBe otherConvention

    // Rendering, which the sweep read through the bean's `toString`, and the agreement of `Show`
    // with it - the two ways of putting an adjustment into a message must not differ.
    test.toString shouldBe "ModifiedFollowing using calendar Sat/Sun"
    Show[BusinessDayAdjustment].show(test) shouldBe test.toString
    Show[BusinessDayAdjustment].show(BusinessDayAdjustment.NONE) shouldBe "NoAdjust"
  }

  test("coverage_builder") {
    // The Java method built the value through the generated Joda builder. That builder has no
    // counterpart here and needs none: construction is total, both fields are required, and the
    // factory, the case-class constructor and `copy` are the three ways of naming the same value.
    // Each is built here and compared with the others field by field, which is what the builder
    // test was checking - that a value assembled a field at a time is the value constructed
    // directly.
    val fromFactory: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.SAT_SUN)
    val fromApply: BusinessDayAdjustment =
      BusinessDayAdjustment(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.SAT_SUN)
    val fromCopy: BusinessDayAdjustment =
      BusinessDayAdjustment.NONE.copy(
        convention = BusinessDayConventions.MODIFIED_FOLLOWING,
        calendar = HolidayCalendarIds.SAT_SUN)

    // field by field, in declaration order, for each of the three
    fromFactory.convention shouldBe BusinessDayConventions.MODIFIED_FOLLOWING
    fromFactory.calendar shouldBe HolidayCalendarIds.SAT_SUN
    fromApply.convention shouldBe fromFactory.convention
    fromApply.calendar shouldBe fromFactory.calendar
    fromCopy.convention shouldBe fromFactory.convention
    fromCopy.calendar shouldBe fromFactory.calendar

    // and therefore as whole values, in equality, hashing and rendering
    fromApply shouldBe fromFactory
    fromCopy shouldBe fromFactory
    Hash[BusinessDayAdjustment].hash(fromApply) shouldBe Hash[BusinessDayAdjustment].hash(fromFactory)
    Hash[BusinessDayAdjustment].hash(fromCopy) shouldBe Hash[BusinessDayAdjustment].hash(fromFactory)
    Show[BusinessDayAdjustment].show(fromCopy) shouldBe Show[BusinessDayAdjustment].show(fromFactory)

    // Changing one field at a time is what a builder was used for, and each change is visible in
    // the value and in its rendering.
    fromFactory.copy(convention = BusinessDayConventions.PRECEDING).toString shouldBe
      "Preceding using calendar Sat/Sun"
    fromFactory.copy(calendar = HolidayCalendarIds.GBLO).toString shouldBe
      "ModifiedFollowing using calendar GBLO"
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // The Java method was `assertSerialization`, a Joda-Beans binary and JSON round trip. Joda
    // wire compatibility is out of scope for this port, so the round trip is the derived circe
    // codec, and the document it produces is pinned here: an object of the two fields under the
    // names the Java bean declared, in declaration order, each the bare string its own type
    // publishes.
    val test: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.GBLO)
    val encoded: Json = test.asJson

    encoded.asObject.map(obj => obj.keys.toList) shouldBe Some(List("convention", "calendar"))
    encoded.noSpaces shouldBe """{"convention":"Following","calendar":"GBLO"}"""

    // Neither field is optional, and the encoder drops absent values in any case, so no null can
    // appear in the document of any adjustment.
    encoded.noSpaces should not include "null"
    BusinessDayAdjustment.NONE.asJson.noSpaces should not include "null"
    BusinessDayAdjustment.NONE.asJson.noSpaces shouldBe
      """{"convention":"NoAdjust","calendar":"NoHolidays"}"""

    // The round trip itself, for a plain value, for the constant, and for a composite calendar.
    decode[BusinessDayAdjustment](encoded.noSpaces) shouldBe Right(test)
    decode[BusinessDayAdjustment]("""{"convention":"Following","calendar":"GBLO"}""") shouldBe Right(test)
    decode[BusinessDayAdjustment](BusinessDayAdjustment.NONE.asJson.noSpaces) shouldBe
      Right(BusinessDayAdjustment.NONE)
    val composite: BusinessDayAdjustment =
      BusinessDayAdjustment.of(
        BusinessDayConventions.MODIFIED_FOLLOWING,
        HolidayCalendarIds.GBLO.combinedWith(HolidayCalendarIds.USNY))
    decode[BusinessDayAdjustment](composite.asJson.noSpaces) shouldBe Right(composite)

    // Equal values encode to identical bytes whatever order a composite calendar was built in,
    // because the identifier normalises its own name.
    BusinessDayAdjustment
      .of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarId.of("USNY+GBLO"))
      .asJson
      .noSpaces shouldBe composite.asJson.noSpaces

    // Both fields are required, so a document missing either is rejected rather than defaulted,
    // and an adjustment is an object rather than a string.
    decode[BusinessDayAdjustment]("""{"convention":"Following"}""").isLeft shouldBe true
    decode[BusinessDayAdjustment]("""{"calendar":"GBLO"}""").isLeft shouldBe true
    decode[BusinessDayAdjustment]("{}").isLeft shouldBe true
    Json.fromString("Following").as[BusinessDayAdjustment].isLeft shouldBe true

    // The convention is read by the name lookup of its own closed family, so text that names no
    // convention is rejected here rather than at resolution time.
    decode[BusinessDayAdjustment]("""{"convention":"Rubbish","calendar":"GBLO"}""").isLeft shouldBe true

    // The calendar, by contrast, accepts any name - a calendar this library knows nothing about is
    // a fact about the reference data rather than about the document - and the value that results
    // fails only when it is resolved.
    val unknown: Either[io.circe.Error, BusinessDayAdjustment] =
      decode[BusinessDayAdjustment]("""{"convention":"Following","calendar":"XXXX"}""")
    unknown shouldBe Right(BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, UNKNOWN_CALENDAR))
    unknown.map(adjustment => adjustment.adjust(MON_2014_07_14, REF_DATA)).map(outcome => outcome.isLeft) shouldBe
      Right(true)
  }
}
