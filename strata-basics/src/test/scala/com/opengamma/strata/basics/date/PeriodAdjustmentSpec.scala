/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.DayOfWeek
import java.time.LocalDate
import java.time.Period

import scala.util.matching.Regex

import cats.Hash
import cats.Show
import cats.syntax.apply._

import io.circe.DecodingFailure
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
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[PeriodAdjustment]], ported from the Java `PeriodAdjustmentTest`.
 *
 * All eleven annotated methods of the Java class are kept, each under the name the Java method
 * had - `test_NONE`, `test_of_additionConventionNone`, `test_of_additionConventionLastDay`,
 * `test_ofLastDay`, `test_ofLastBusinessDay`, `test_of_invalid_conventionForPeriod`,
 * `test_adjust`, `equals`, `test_beanBuilder`, `coverage` and `test_serialization` - so that a
 * Java test method and a test of this suite stay in one-to-one correspondence and the
 * method-level traceability the migration manifest records resolves on the pair of suite class
 * and test name. Nothing is added under a name of its own: everything this port asserts beyond
 * the Java assertions belongs to whichever of the eleven methods already owned that ground,
 * which is why `test_adjust` is the long one.
 *
 * ===The provider table===
 *
 * `test_adjust` was parameterised in Java from a `@MethodSource` provider of eight rows, each a
 * number of months, a date to adjust and the date the library being ported produced for it under
 * the last-day-of-month convention against the `Sat/Sun` calendar. The rows are transcribed
 * verbatim, in the Java order, into the table this test drives, so the parameterised method
 * contributes one test case rather than eight - which is what the naming contract above requires
 * and what keeps the count of reported tests equal to the count of Java methods.
 *
 * ===How the shape of the port changes the assertions===
 *
 *   - Construction returns `EitherNec[Failure, PeriodAdjustment]` where the Java factories
 *     returned a bean and threw `IllegalArgumentException` for the one pairing the type rejects.
 *     Every ported construction is therefore asserted as a success carrying the Java value, and
 *     `test_of_invalid_conventionForPeriod` - four `assertThatIllegalArgumentException` sites in
 *     Java - asserts a failure whose reason is compared as a value of the closed family of
 *     reasons and whose message is the wording of the validator being ported.
 *   - `adjust` and `resolve` return `Either[Failure, _]` where the Java methods returned a bare
 *     date and threw `ReferenceDataNotFoundException` for a calendar the reference data does not
 *     hold. That failure path, which the Java class never exercised because it had nothing but an
 *     exception to exercise it with, is asserted at the end of `test_adjust`.
 *   - Java's `test_adjust` asserted two paths, `adjust(date, refData)` and
 *     `resolve(refData).adjust(date)`. This port has a third, `toReader.run(refData)`, which is
 *     the same resolution expressed as a value awaiting reference data, so all three are driven
 *     through every row: supplying reference data once, or later, or in composition with another
 *     lookup, must not change what is computed.
 *   - `test_beanBuilder` used the generated Joda builder, which has no counterpart here and could
 *     not have one: a value assembled a field at a time would exist before its invariant had been
 *     checked. The factories are what replaced it, so the test asserts that each of them names
 *     the same value, field by field.
 *   - `coverage` called `coverImmutableBean`, a reflective sweep of a Joda bean's properties,
 *     equality, hashing and rendering. There is no bean and no reflection in this port, so what
 *     the sweep stood for is asserted directly over the same value the Java method swept. It is
 *     asserted without `copy`, which this validated type does not have.
 *   - `test_serialization` asserted Joda-Beans binary and JSON round trips. Joda wire
 *     compatibility is out of scope for this port, so the test is a circe round trip through the
 *     codec pair, which additionally pins the concrete document and the rejection of a payload
 *     the factory would not have built.
 *
 * ===What is asserted elsewhere===
 *
 * The per-type matrix of invalid inputs and their accumulation, the sweep that exercises every
 * failable method of the module with a failing input, the compile-time proofs that a validated
 * type has no public `apply` and no `copy`, the typeclass law suites and the property-based round
 * trip of every codec-bearing type each live in their own spec at the root of the test tree; the
 * fixture-driven numerical parity of schedule and date arithmetic belongs to the parity specs.
 * This suite asserts the cases of the Java test it is ported from, which overlap those sweeps by
 * design.
 *
 * @see [[PeriodAdjustment]] for the type under test
 * @see [[PeriodAdditionConventionSpec]] for the end-of-month rules step one applies, including
 *   the month-based flag this type's validator reads
 * @see [[BusinessDayAdjustmentSpec]] for step two, asserted without an addition in the picture
 * @see [[TenorAdjustmentSpec]] for the same rule expressed over a tenor rather than a period
 */
final class PeriodAdjustmentSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /** The reference data the Java class used throughout, which holds every built-in calendar. */
  private val REF_DATA: ReferenceData = ReferenceData.standard

  /**
   * The addition convention that adds a period unchanged, named as the Java class named it.
   *
   * The member is reached as `NONE` rather than as `None`, which belongs to the empty `Option` of
   * the standard library; its name - and so its rendering and its JSON - is still `None`.
   */
  private val PAC_NONE: PeriodAdditionConvention = PeriodAdditionConventions.NONE

  /** The business day adjustment that makes no adjustment, named as the Java class named it. */
  private val BDA_NONE: BusinessDayAdjustment = BusinessDayAdjustment.NONE

  /** The business day adjustment of the Java class: roll forward over a weekend. */
  private val BDA_FOLLOW_SAT_SUN: BusinessDayAdjustment =
    BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.SAT_SUN)

  /**
   * The refusal of a period holding days under a month-based addition convention.
   *
   * This is the wording of the validator of the bean being ported, character for character. It is
   * part of the ported contract rather than an incidental string: the reason a failure carries is
   * what a caller acts on, and this message is what tells the two month-based conventions'
   * rejection apart from any other invalid-argument report in a log.
   */
  private val MonthBasedMessage: String =
    "Period must not contain days when addition convention is month-based"

  /**
   * A value of a type unrelated to an adjustment, for the equality assertion that needs one.
   *
   * Held at the type `Any` and named as the reflective sweep's foreign object was named, so that
   * the assertion reads as a comparison against a foreign value rather than as a comparison the
   * compiler could reject outright.
   */
  private val ANOTHER_TYPE: Any = ""

  /** An identifier no reference data of this library holds, used to observe the failure path. */
  private val UNKNOWN_CALENDAR: HolidayCalendarId = HolidayCalendarId.of("XXXX")

  /**
   * The eight rows of the Java `data_adjust` provider, in the Java order.
   *
   * Each row is a number of months to add, a date to adjust and the date the library being ported
   * produces for it from `PeriodAdjustment.of(Period.ofMonths(months), LAST_DAY,
   * BDA_FOLLOW_SAT_SUN)`. The first group is a base date that is not the last day of its month, so
   * the addition convention does nothing and only the business day convention can move the result;
   * the second group is a base date that is the last day of its month, which is what the addition
   * convention is for.
   */
  private val dataAdjust: TableFor3[Int, LocalDate, LocalDate] = Table(
    ("months", "input", "expected"),
    // not last day
    (0, date(2014, 8, 15), date(2014, 8, 15)),
    (1, date(2014, 8, 15), date(2014, 9, 15)),
    (2, date(2014, 8, 15), date(2014, 10, 15)),
    // the addition lands on Saturday 15 November, which the business day convention rolls forward
    (3, date(2014, 8, 15), date(2014, 11, 17)),
    (-1, date(2014, 8, 15), date(2014, 7, 15)),
    // the subtraction lands on Sunday 15 June, which the business day convention rolls forward
    (-2, date(2014, 8, 15), date(2014, 6, 16)),
    // last day
    (1, date(2014, 2, 28), date(2014, 3, 31)),
    (1, date(2014, 6, 30), date(2014, 7, 31)))

  //-------------------------------------------------------------------------
  test("test_NONE") {
    val test: PeriodAdjustment = PeriodAdjustment.NONE

    test.period shouldBe Period.ZERO
    test.additionConvention shouldBe PAC_NONE
    test.adjustment shouldBe BDA_NONE
    // Every part of the constant says nothing, so the rendering is the period alone: the addition
    // convention is the one that adds a period unchanged and the business day adjustment is the
    // one that adjusts nothing, and naming either of them would say something untrue about it.
    test.toString shouldBe "P0D"

    // And it is the identity of the type: a date given to it comes back unaltered, through each
    // of the three entry points, and a weekend date is not moved either - which is the whole
    // content of pairing the no-adjust convention with the no-holidays calendar.
    val saturday: LocalDate = date(2014, 8, 16)
    saturday.getDayOfWeek shouldBe DayOfWeek.SATURDAY
    test.adjust(saturday, REF_DATA) should haveValue(saturday)
    test.resolve(REF_DATA).map(adjuster => adjuster.adjust(saturday)) should haveValue(saturday)
    test.toReader.run(REF_DATA).map(adjuster => adjuster.adjust(saturday)) should haveValue(saturday)
  }

  //-------------------------------------------------------------------------
  test("test_of_additionConventionNone") {
    val outcome: ResultNec[PeriodAdjustment] =
      PeriodAdjustment.of(Period.of(1, 2, 3), PAC_NONE, BDA_NONE)
    outcome should beSuccess

    val test: PeriodAdjustment = unwrap(outcome)
    test.period shouldBe Period.of(1, 2, 3)
    test.additionConvention shouldBe PAC_NONE
    test.adjustment shouldBe BDA_NONE
    test.toString shouldBe "P1Y2M3D"

    // A period holding days is admitted here, and only here: the convention that adds a period
    // unchanged is not month-based, so the one invariant of the type does not bite. The addition
    // is plain calendar arithmetic - years, then months, then days - and the result is left on
    // the Sunday it lands on because this adjustment makes no business day adjustment.
    val adjusted: LocalDate = date(2015, 10, 18)
    adjusted.getDayOfWeek shouldBe DayOfWeek.SUNDAY
    test.adjust(date(2014, 8, 15), REF_DATA) should haveValue(adjusted)
  }

  test("test_of_additionConventionLastDay") {
    val outcome: ResultNec[PeriodAdjustment] =
      PeriodAdjustment.of(Period.ofMonths(3), PeriodAdditionConventions.LAST_DAY, BDA_FOLLOW_SAT_SUN)
    outcome should beSuccess

    val test: PeriodAdjustment = unwrap(outcome)
    test.period shouldBe Period.ofMonths(3)
    test.additionConvention shouldBe PeriodAdditionConventions.LAST_DAY
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN
    // Both of the other two fields say something now, so both are named, in the order the two
    // steps run in - the addition convention first and the business day adjustment second.
    test.toString shouldBe "P3M with LastDay then apply Following using calendar Sat/Sun"
  }

  test("test_ofLastDay") {
    val outcome: ResultNec[PeriodAdjustment] =
      PeriodAdjustment.ofLastDay(Period.ofMonths(3), BDA_FOLLOW_SAT_SUN)
    outcome should beSuccess

    val test: PeriodAdjustment = unwrap(outcome)
    test.period shouldBe Period.ofMonths(3)
    test.additionConvention shouldBe PeriodAdditionConventions.LAST_DAY
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN
    test.toString shouldBe "P3M with LastDay then apply Following using calendar Sat/Sun"

    // The convenience factory names the convention on the caller's behalf and adds nothing else,
    // so it is the general factory with that convention supplied - including in what it rejects,
    // which `test_of_invalid_conventionForPeriod` asserts for both forms.
    outcome shouldBe
      PeriodAdjustment.of(Period.ofMonths(3), PeriodAdditionConventions.LAST_DAY, BDA_FOLLOW_SAT_SUN)
  }

  test("test_ofLastBusinessDay") {
    val outcome: ResultNec[PeriodAdjustment] =
      PeriodAdjustment.ofLastBusinessDay(Period.ofMonths(3), BDA_FOLLOW_SAT_SUN)
    outcome should beSuccess

    val test: PeriodAdjustment = unwrap(outcome)
    test.period shouldBe Period.ofMonths(3)
    test.additionConvention shouldBe PeriodAdditionConventions.LAST_BUSINESS_DAY
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN
    test.toString shouldBe "P3M with LastBusinessDay then apply Following using calendar Sat/Sun"

    outcome shouldBe
      PeriodAdjustment.of(
        Period.ofMonths(3),
        PeriodAdditionConventions.LAST_BUSINESS_DAY,
        BDA_FOLLOW_SAT_SUN)

    // The two month-based conventions are not the same rule, and the difference is visible on a
    // base date that is the last business day of its month without being the last day of it:
    // Friday 29 August 2014 is followed by a weekend, so the last-business-day rule reads it as a
    // month end and produces the last business day of the month a month later, while the
    // last-day rule reads it as an ordinary date and simply adds the month.
    val lastBusinessDay: PeriodAdjustment =
      unwrap(PeriodAdjustment.ofLastBusinessDay(Period.ofMonths(1), BDA_FOLLOW_SAT_SUN))
    val lastDay: PeriodAdjustment =
      unwrap(PeriodAdjustment.ofLastDay(Period.ofMonths(1), BDA_FOLLOW_SAT_SUN))
    lastBusinessDay.adjust(date(2014, 8, 29), REF_DATA) should haveValue(date(2014, 9, 30))
    lastDay.adjust(date(2014, 8, 29), REF_DATA) should haveValue(date(2014, 9, 29))
  }

  //-------------------------------------------------------------------------
  test("test_of_invalid_conventionForPeriod") {
    // The one invariant of the type, and the four ways the Java class reached it: a period holding
    // days paired with either of the two month-based addition conventions, through the general
    // factory and through each convenience factory. Java asserted an IllegalArgumentException for
    // each; here each is a failure carrying the reason and the wording of the same validator.
    val period: Period = Period.of(1, 2, 3)
    val rejected: List[(String, ResultNec[PeriodAdjustment])] = List(
      "of with LastDay" ->
        PeriodAdjustment.of(period, PeriodAdditionConventions.LAST_DAY, BDA_NONE),
      "of with LastBusinessDay" ->
        PeriodAdjustment.of(period, PeriodAdditionConventions.LAST_BUSINESS_DAY, BDA_NONE),
      "ofLastDay" -> PeriodAdjustment.ofLastDay(period, BDA_NONE),
      "ofLastBusinessDay" -> PeriodAdjustment.ofLastBusinessDay(period, BDA_NONE))

    rejected.foreach { case (route, outcome) =>
      withClue(s"$route: ") {
        // the reason is compared as a value of the closed family of reasons, so a reason spelled
        // as text cannot be passed here and a member that does not exist cannot be named
        outcome should beFailureWith(FailureReason.INVALID)
        outcome should haveFailureMessageMatching(Regex.quote(MonthBasedMessage))
        // and the whole failure is pinned, which additionally states that there is exactly one of
        // them: this type has a single invariant, so there is nothing here to accumulate with
        failuresOf(outcome) shouldBe List(Failure.Invalid(MonthBasedMessage))
      }
    }

    // The rejection is of the pairing and not of either part of it. The same period is admitted
    // by the convention that is not month-based, and both month-based conventions admit a period
    // measured only in months and years - including the zero period, whose days are zero.
    PeriodAdjustment.of(period, PAC_NONE, BDA_NONE) should beSuccess
    PeriodAdjustment.ofLastDay(Period.of(1, 2, 0), BDA_NONE) should beSuccess
    PeriodAdjustment.ofLastBusinessDay(Period.of(1, 2, 0), BDA_NONE) should beSuccess
    PeriodAdjustment.ofLastDay(Period.ZERO, BDA_NONE) should beSuccess

    // It is the presence of a day component that is rejected rather than its sign, which is the
    // condition the validator of the bean being ported tested.
    PeriodAdjustment.ofLastDay(Period.ofDays(1), BDA_NONE) should
      beFailureWith(FailureReason.INVALID)
    PeriodAdjustment.ofLastDay(Period.ofDays(-1), BDA_NONE) should
      beFailureWith(FailureReason.INVALID)

    // Nothing is raised on any of these routes: the report is the return value, which is what
    // lets a caller decide what to do about it.
    noException should be thrownBy PeriodAdjustment.ofLastDay(period, BDA_NONE)

    // The Java class asserted an IllegalArgumentException for this pairing and for nothing else -
    // it has no absent-reference case, all three arguments being required values of their own
    // types. What stands in for a run-time check of an absent reference here is therefore the
    // type of each argument, which the compiler enforces before the program runs; the valid call
    // is asserted to compile so that the proof cannot be passing for an unrelated reason.
    assertDoesNotCompile(
      """PeriodAdjustment.of(Period.ofMonths(3), "LastDay", BusinessDayAdjustment.NONE)""")
    assertDoesNotCompile("""PeriodAdjustment.ofLastDay(Period.ofMonths(3))""")
    assertCompiles(
      """PeriodAdjustment.of(
           Period.ofMonths(3),
           PeriodAdditionConventions.LAST_DAY,
           BusinessDayAdjustment.NONE)""")
  }


  //-------------------------------------------------------------------------
  test("test_adjust") {
    // The Java parameterised method: every row of the provider, under the last-day-of-month
    // convention and the weekend calendar it used. Java asserted the unresolved and the resolved
    // path; this port adds the reader, and all three must produce the value the library being
    // ported produced.
    forAll(dataAdjust) { (months: Int, input: LocalDate, expected: LocalDate) =>
      val test: PeriodAdjustment =
        unwrap(PeriodAdjustment.ofLastDay(Period.ofMonths(months), BDA_FOLLOW_SAT_SUN))

      withClue(s"$test adjusting $input: ") {
        test.adjust(input, REF_DATA) should haveValue(expected)
        test.resolve(REF_DATA).map(adjuster => adjuster.adjust(input)) should haveValue(expected)
        test.toReader.run(REF_DATA).map(adjuster => adjuster.adjust(input)) should haveValue(expected)
      }
    }

    //-----------------------------------------------------------------------
    // The two steps run in one order and not the other, which is the behaviour a naive port
    // breaks and which two rows of the table above make observable.
    //
    // Row `(3, 2014-08-15, 2014-11-17)`: the business day convention runs on the result of the
    // addition. The input is a Friday and needs no adjustment, so an implementation that adjusted
    // first and added second would answer with the Saturday the addition lands on.
    val threeMonths: PeriodAdjustment =
      unwrap(PeriodAdjustment.ofLastDay(Period.ofMonths(3), BDA_FOLLOW_SAT_SUN))
    val additionOnly: PeriodAdjustment =
      unwrap(PeriodAdjustment.ofLastDay(Period.ofMonths(3), BDA_NONE))
    date(2014, 8, 15).getDayOfWeek shouldBe DayOfWeek.FRIDAY
    additionOnly.adjust(date(2014, 8, 15), REF_DATA) should haveValue(date(2014, 11, 15))
    date(2014, 11, 15).getDayOfWeek shouldBe DayOfWeek.SATURDAY
    threeMonths.adjust(date(2014, 8, 15), REF_DATA) should haveValue(date(2014, 11, 17))

    // Row `(1, 2014-02-28, 2014-03-31)`: the addition convention runs first, and on the input
    // rather than on anything the business day convention produced. Without that convention the
    // addition lands on 28 March, and it is the end-of-month rule reading 28 February as a month
    // end that moves the answer to 31 March.
    val oneMonthPlain: PeriodAdjustment =
      unwrap(PeriodAdjustment.of(Period.ofMonths(1), PAC_NONE, BDA_FOLLOW_SAT_SUN))
    val oneMonthLastDay: PeriodAdjustment =
      unwrap(PeriodAdjustment.ofLastDay(Period.ofMonths(1), BDA_FOLLOW_SAT_SUN))
    oneMonthPlain.adjust(date(2014, 2, 28), REF_DATA) should haveValue(date(2014, 3, 28))
    oneMonthLastDay.adjust(date(2014, 2, 28), REF_DATA) should haveValue(date(2014, 3, 31))

    //-----------------------------------------------------------------------
    // `resolve` looks the calendar up once and binds it into the adjuster returned, which is the
    // point of having the method at all: a run of dates costs one lookup rather than one per
    // date. What that has to leave unchanged is the answer, so one resolved adjuster is applied
    // to two different dates - the second of which needs both steps, the addition moving it to a
    // month end that falls on a Saturday and the business day convention rolling that into the
    // next month - and each answer is compared with resolving afresh for that date.
    val resolvedOnce: Either[Failure, DateAdjuster] = threeMonths.resolve(REF_DATA)
    val readOnce: Either[Failure, DateAdjuster] = threeMonths.toReader.run(REF_DATA)
    resolvedOnce should beSuccess
    readOnce should beSuccess

    List(date(2014, 8, 15) -> date(2014, 11, 17), date(2014, 2, 28) -> date(2014, 6, 2)).foreach {
      case (input, expected) =>
        withClue(s"$threeMonths adjusting $input: ") {
          val perDate: Either[Failure, LocalDate] = threeMonths.adjust(input, REF_DATA)
          perDate should haveValue(expected)
          resolvedOnce.map(adjuster => adjuster.adjust(input)) shouldBe perDate
          readOnce.map(adjuster => adjuster.adjust(input)) shouldBe perDate
        }
    }

    // Two readers compose, which is the reason the reader form exists: several adjustments are
    // assembled while no reference data is available and the data is supplied once, to the
    // composition, rather than to each of them. The two adjustments here differ only in their
    // addition convention, so the pair they produce also states that difference once more.
    val bothConventions: RefDataReader[(LocalDate, LocalDate)] =
      (
        unwrap(PeriodAdjustment.ofLastDay(Period.ofMonths(1), BDA_FOLLOW_SAT_SUN)).toReader,
        unwrap(PeriodAdjustment.ofLastBusinessDay(Period.ofMonths(1), BDA_FOLLOW_SAT_SUN)).toReader)
        .mapN((lastDayAdjuster, lastBusinessDayAdjuster) =>
          (lastDayAdjuster.adjust(date(2014, 8, 29)), lastBusinessDayAdjuster.adjust(date(2014, 8, 29))))
    bothConventions.run(REF_DATA) should haveValue((date(2014, 9, 29), date(2014, 9, 30)))

    //-----------------------------------------------------------------------
    // The failure path, which the Java class never exercised because the Java methods threw
    // `ReferenceDataNotFoundException`. A calendar the reference data cannot supply is reported
    // as a missing-data failure through all three entry points, and nothing is raised.
    val unknown: PeriodAdjustment =
      unwrap(
        PeriodAdjustment.ofLastDay(
          Period.ofMonths(3),
          BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, UNKNOWN_CALENDAR)))
    unknown.adjust(date(2014, 8, 15), REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    unknown.resolve(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    unknown.toReader.run(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    noException should be thrownBy unknown.adjust(date(2014, 8, 15), REF_DATA)

    // The failure names the identifier that could not be found, so a caller can act on it, and a
    // composition one reader of which cannot resolve fails as a whole with that reader's failure.
    unknown.adjust(date(2014, 8, 15), REF_DATA) should
      haveFailureMessageMatching(".*'XXXX'.*")
    val partlyUnknown: RefDataReader[(LocalDate, LocalDate)] =
      (threeMonths.toReader, unknown.toReader)
        .mapN((known, missing) =>
          (known.adjust(date(2014, 8, 15)), missing.adjust(date(2014, 8, 15))))
    partlyUnknown.run(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  test("equals") {
    val a: PeriodAdjustment =
      unwrap(PeriodAdjustment.of(Period.ofMonths(3), PeriodAdditionConventions.LAST_DAY, BDA_FOLLOW_SAT_SUN))
    val b: PeriodAdjustment =
      unwrap(PeriodAdjustment.of(Period.ofMonths(1), PeriodAdditionConventions.LAST_DAY, BDA_FOLLOW_SAT_SUN))
    val c: PeriodAdjustment =
      unwrap(PeriodAdjustment.of(Period.ofMonths(3), PAC_NONE, BDA_FOLLOW_SAT_SUN))
    val d: PeriodAdjustment =
      unwrap(PeriodAdjustment.of(Period.ofMonths(3), PeriodAdditionConventions.LAST_DAY, BDA_NONE))

    // The three Java assertions: a difference in any one of the three fields is a different
    // value, so each field is part of the identity.
    a.equals(b) shouldBe false
    a.equals(c) shouldBe false
    a.equals(d) shouldBe false
    Hash[PeriodAdjustment].eqv(a, b) shouldBe false
    Hash[PeriodAdjustment].eqv(a, c) shouldBe false
    Hash[PeriodAdjustment].eqv(a, d) shouldBe false

    // The other half of the contract, which the Java method left to the reflective sweep: a value
    // built twice is one value, and equal values hash alike. The two routes used here are the
    // general factory and the convenience factory, so this also states that the convenience
    // factory produces no privileged instance.
    val sameAsA: PeriodAdjustment =
      unwrap(PeriodAdjustment.ofLastDay(Period.ofMonths(3), BDA_FOLLOW_SAT_SUN))
    a.equals(sameAsA) shouldBe true
    a shouldBe sameAsA
    a.hashCode shouldBe sameAsA.hashCode
    Hash[PeriodAdjustment].eqv(a, sameAsA) shouldBe true
    Hash[PeriodAdjustment].hash(a) shouldBe Hash[PeriodAdjustment].hash(sameAsA)

    // A value of another type is not equal to an adjustment, and neither is the text it renders
    // as - the comparison is written through the method that takes any value, because the two
    // types are unrelated and the operator form would be rejected outright.
    a.equals(ANOTHER_TYPE) shouldBe false
    a.equals(a.toString) shouldBe false

    // Period equality is that of `java.time.Period`, which compares the three amounts rather than
    // the length of time they denote, so an adjustment adding twelve months is not equal to one
    // adding a year. This is the behaviour of the library being ported, for the same reason.
    val twelveMonths: PeriodAdjustment =
      unwrap(PeriodAdjustment.ofLastDay(Period.ofMonths(12), BDA_FOLLOW_SAT_SUN))
    val oneYear: PeriodAdjustment =
      unwrap(PeriodAdjustment.ofLastDay(Period.ofYears(1), BDA_FOLLOW_SAT_SUN))
    twelveMonths.equals(oneYear) shouldBe false
    Hash[PeriodAdjustment].eqv(twelveMonths, oneYear) shouldBe false
  }

  //-------------------------------------------------------------------------
  test("test_beanBuilder") {
    // The Java method built the value through the generated Joda-Beans builder. That builder has
    // no target in this port and could not have one: `with*` methods and the factories replaced
    // it, and a value assembled a field at a time would exist before the invariant of the type
    // had been checked. This type additionally exposes no `with*` method - it derives no instance
    // from another - so the general factory and the two convention-specific forms are the whole
    // of construction, and what the builder test was checking is that the routes agree: a value
    // named one way is the value named another. Each route is built here and compared with the
    // primary construction field by field, in declaration order.
    val fromOf: PeriodAdjustment =
      unwrap(PeriodAdjustment.of(Period.ofMonths(3), PeriodAdditionConventions.LAST_DAY, BDA_FOLLOW_SAT_SUN))
    val fromOfLastDay: PeriodAdjustment =
      unwrap(PeriodAdjustment.ofLastDay(Period.ofMonths(3), BDA_FOLLOW_SAT_SUN))

    // the three fields of the Java bean, read back as the Java method read them
    fromOf.period shouldBe Period.ofMonths(3)
    fromOf.additionConvention shouldBe PeriodAdditionConventions.LAST_DAY
    fromOf.adjustment shouldBe BDA_FOLLOW_SAT_SUN

    // field by field for the second route
    fromOfLastDay.period shouldBe fromOf.period
    fromOfLastDay.additionConvention shouldBe fromOf.additionConvention
    fromOfLastDay.adjustment shouldBe fromOf.adjustment

    // and therefore as whole values, in equality, hashing and rendering
    fromOfLastDay shouldBe fromOf
    Hash[PeriodAdjustment].hash(fromOfLastDay) shouldBe Hash[PeriodAdjustment].hash(fromOf)
    Show[PeriodAdjustment].show(fromOfLastDay) shouldBe Show[PeriodAdjustment].show(fromOf)

    // Changing one field at a time is what a builder was used for. Here that is a fresh call to
    // the factory, and each change is visible in the value and in its rendering.
    val otherConvention: PeriodAdjustment =
      unwrap(PeriodAdjustment.ofLastBusinessDay(Period.ofMonths(3), BDA_FOLLOW_SAT_SUN))
    val otherAdjustment: PeriodAdjustment =
      unwrap(PeriodAdjustment.of(Period.ofMonths(3), PeriodAdditionConventions.LAST_DAY, BDA_NONE))
    otherConvention.additionConvention shouldBe PeriodAdditionConventions.LAST_BUSINESS_DAY
    otherConvention.toString shouldBe
      "P3M with LastBusinessDay then apply Following using calendar Sat/Sun"
    otherAdjustment.adjustment shouldBe BDA_NONE
    otherAdjustment.toString shouldBe "P3M with LastDay"
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java method was `coverImmutableBean(PeriodAdjustment.of(Period.ofMonths(3), LAST_DAY,
    // BDA_FOLLOW_SAT_SUN))`: a reflective sweep over a Joda bean's properties, equality, hashing
    // and rendering. There is no bean and no reflection in this port, so that sweep has no target
    // and the properties it stood for are asserted directly, over exactly the value the Java
    // method named. The sweep's write half - reading a property back after changing it - is
    // asserted through a second construction rather than through `copy`, which this validated
    // type does not have.
    val test: PeriodAdjustment =
      unwrap(PeriodAdjustment.of(Period.ofMonths(3), PeriodAdditionConventions.LAST_DAY, BDA_FOLLOW_SAT_SUN))
    val same: PeriodAdjustment =
      unwrap(PeriodAdjustment.of(Period.ofMonths(3), PeriodAdditionConventions.LAST_DAY, BDA_FOLLOW_SAT_SUN))
    val other: PeriodAdjustment =
      unwrap(PeriodAdjustment.of(Period.of(1, 2, 3), PAC_NONE, BDA_NONE))

    // the three properties read back as they were given, for each of the two distinct instances
    test.period shouldBe Period.ofMonths(3)
    test.additionConvention shouldBe PeriodAdditionConventions.LAST_DAY
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN
    other.period shouldBe Period.of(1, 2, 3)
    other.additionConvention shouldBe PAC_NONE
    other.adjustment shouldBe BDA_NONE

    // Pattern matching reads the same three fields, which is what the case-class extractor of a
    // validated type is for: the constructor is private and there is no `apply`, but taking a
    // value apart is unaffected.
    PeriodAdjustment.unapply(test) shouldBe
      Some((Period.ofMonths(3), PeriodAdditionConventions.LAST_DAY, BDA_FOLLOW_SAT_SUN))

    // equality and hashing, by field, over two distinct instances and a repeat of one of them
    test shouldBe same
    test.hashCode shouldBe same.hashCode
    Hash[PeriodAdjustment].eqv(test, same) shouldBe true
    Hash[PeriodAdjustment].hash(test) shouldBe Hash[PeriodAdjustment].hash(same)
    test should not be other
    Hash[PeriodAdjustment].eqv(test, other) shouldBe false
    test.equals(ANOTHER_TYPE) shouldBe false

    // rendering, which the sweep read through the bean's `toString`, and the agreement of `Show`
    // with it - the two ways of putting an adjustment into a message must not differ
    test.toString shouldBe "P3M with LastDay then apply Following using calendar Sat/Sun"
    Show[PeriodAdjustment].show(test) shouldBe test.toString
    other.toString shouldBe "P1Y2M3D"
    Show[PeriodAdjustment].show(other) shouldBe other.toString
    Show[PeriodAdjustment].show(PeriodAdjustment.NONE) shouldBe "P0D"
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // The Java method was `assertSerialization`, a Joda-Beans binary and JSON round trip. Joda
    // wire compatibility is out of scope for this port, so the round trip is the circe codec
    // pair, and the document it produces is pinned here: an object of the three fields under the
    // names the Java bean declared, in declaration order, the period as its ISO text form, the
    // addition convention as its canonical name and the business day adjustment as the object its
    // own codec writes.
    val test: PeriodAdjustment =
      unwrap(PeriodAdjustment.of(Period.ofMonths(3), PeriodAdditionConventions.LAST_DAY, BDA_FOLLOW_SAT_SUN))
    val encoded: Json = test.asJson

    encoded.asObject.map(obj => obj.keys.toList) shouldBe
      Some(List("period", "additionConvention", "adjustment"))
    encoded.noSpaces shouldBe
      """{"period":"P3M","additionConvention":"LastDay",""" +
        """"adjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""

    // No field of this type is optional, and the encoder drops absent values in any case, so no
    // null can appear in the document of any adjustment.
    encoded.noSpaces should not include "null"
    PeriodAdjustment.NONE.asJson.noSpaces should not include "null"

    // The round trip itself, from the encoded value and from the document written out by hand, for
    // a plain value, for the constant, and for a period holding days under the convention that
    // admits one.
    encoded.as[PeriodAdjustment] shouldBe Right(test)
    decode[PeriodAdjustment](encoded.noSpaces) shouldBe Right(test)
    decode[PeriodAdjustment](
      """{"period":"P3M","additionConvention":"LastDay",""" +
        """"adjustment":{"convention":"Following","calendar":"Sat/Sun"}}""") shouldBe Right(test)
    PeriodAdjustment.NONE.asJson.noSpaces shouldBe
      """{"period":"P0D","additionConvention":"None",""" +
        """"adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}"""
    decode[PeriodAdjustment](PeriodAdjustment.NONE.asJson.noSpaces) shouldBe
      Right(PeriodAdjustment.NONE)
    val withDays: PeriodAdjustment =
      unwrap(PeriodAdjustment.of(Period.of(1, 2, 3), PAC_NONE, BDA_NONE))
    decode[PeriodAdjustment](withDays.asJson.noSpaces) shouldBe Right(withDays)

    // Equal values encode to identical bytes, which is what makes the document a function of the
    // value rather than of the route that built it.
    unwrap(PeriodAdjustment.ofLastDay(Period.ofMonths(3), BDA_FOLLOW_SAT_SUN)).asJson.noSpaces shouldBe
      encoded.noSpaces

    // The decoder runs the fields through the factory, so a document pairing a period that holds
    // days with a month-based addition convention is a decoding failure carrying the wording the
    // factory reports, rather than a value this type would never have built.
    val invalid: Json = Json.obj(
      "period" -> Json.fromString("P1Y2M3D"),
      "additionConvention" -> Json.fromString("LastDay"),
      "adjustment" -> BDA_NONE.asJson)
    val rejected: Either[DecodingFailure, PeriodAdjustment] = invalid.as[PeriodAdjustment]
    rejected.isLeft shouldBe true
    rejected.left.map(failure => failure.message) shouldBe Left(MonthBasedMessage)

    // All three fields are required, so a document missing any of them is rejected rather than
    // defaulted, and an adjustment is an object rather than a string.
    decode[PeriodAdjustment](
      """{"additionConvention":"LastDay","adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}""")
      .isLeft shouldBe true
    decode[PeriodAdjustment](
      """{"period":"P3M","adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}""")
      .isLeft shouldBe true
    decode[PeriodAdjustment]("""{"period":"P3M","additionConvention":"LastDay"}""").isLeft shouldBe true
    decode[PeriodAdjustment]("{}").isLeft shouldBe true
    Json.fromString("P3M").as[PeriodAdjustment].isLeft shouldBe true

    // Each field is read by the codec of its own type, so text that names no addition convention
    // and text that is no period are both rejected here rather than later.
    decode[PeriodAdjustment](
      """{"period":"P3M","additionConvention":"Rubbish",""" +
        """"adjustment":{"convention":"Following","calendar":"Sat/Sun"}}""").isLeft shouldBe true
    decode[PeriodAdjustment](
      """{"period":"Rubbish","additionConvention":"LastDay",""" +
        """"adjustment":{"convention":"Following","calendar":"Sat/Sun"}}""").isLeft shouldBe true
  }

  //-------------------------------------------------------------------------
  /**
   * Reads the adjustment out of an outcome that is expected to have produced one.
   *
   * The factories are the only way to build an adjustment and they report what was wrong with
   * their arguments as a value, so every fixture of this suite arrives wrapped. The outcome is
   * folded rather than unwrapped by a partial accessor, so a fixture that fails to build is
   * reported as a test failure naming every reason it failed instead of raising an error from
   * somewhere else in the suite.
   *
   * @param outcome  the outcome expected to carry an adjustment
   * @return the adjustment it carries
   */
  private def unwrap(outcome: ResultNec[PeriodAdjustment]): PeriodAdjustment =
    outcome.fold(
      failures =>
        fail(
          "Expected an adjustment but the factory failed with: " +
            failures.toChain.toList.map(failure => failure.message).mkString(", ")),
      adjustment => adjustment)

  /**
   * Reads the failures out of an outcome that is expected to have produced no adjustment.
   *
   * Only the rejection assertions need this: the matchers hold when '''some''' failure of an
   * outcome satisfies what was asked, which is the right reading in general but says nothing
   * about how many failures a chain holds. This type has a single invariant, so asserting that
   * exactly one failure is reported - and that it is the whole failure, attributes included -
   * needs the chain itself.
   *
   * @param outcome  the outcome expected to carry failures
   * @return the failures it carries, in the order it holds them
   */
  private def failuresOf(outcome: ResultNec[PeriodAdjustment]): List[Failure] =
    outcome.fold(
      failures => failures.toChain.toList,
      adjustment => fail(s"Expected a failure but the factory built the adjustment $adjustment"))

}
