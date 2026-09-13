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
 * Test [[PeriodAdjustment]].
 *
 * An adjustment runs in two steps, in this order: the period is added under the addition
 * convention, then the business day adjustment is applied to the result. A month-based addition
 * convention rejects a period containing days, reported as `FailureReason.INVALID`, so
 * construction returns `EitherNec[Failure, PeriodAdjustment]`. `adjust(date, refData)`,
 * `resolve(refData).adjust(date)` and `toReader.run(refData)` agree, and a calendar the reference
 * data does not hold is a `Failure` value rather than an exception.
 */
final class PeriodAdjustmentSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  private val REF_DATA: ReferenceData = ReferenceData.standard

  private val PAC_NONE: PeriodAdditionConvention = PeriodAdditionConventions.NONE

  private val BDA_NONE: BusinessDayAdjustment = BusinessDayAdjustment.NONE

  private val BDA_FOLLOW_SAT_SUN: BusinessDayAdjustment =
    BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.SAT_SUN)

  /** The message the factories report when a month-based convention meets a period holding days. */
  private val MonthBasedMessage: String =
    "Period must not contain days when addition convention is month-based"

  private val ANOTHER_TYPE: Any = ""

  private val UNKNOWN_CALENDAR: HolidayCalendarId = HolidayCalendarId.of("XXXX")

  /**
   * Rows of months to add, date to adjust and expected result under `ofLastDay` with
   * `BDA_FOLLOW_SAT_SUN`; the first group of base dates does not end its month, the second does.
   */
  private val dataAdjust: TableFor3[Int, LocalDate, LocalDate] = Table(
    ("months", "input", "expected"),
    (0, date(2014, 8, 15), date(2014, 8, 15)),
    (1, date(2014, 8, 15), date(2014, 9, 15)),
    (2, date(2014, 8, 15), date(2014, 10, 15)),
    // the addition lands on Saturday 15 November, which the business day convention rolls forward
    (3, date(2014, 8, 15), date(2014, 11, 17)),
    (-1, date(2014, 8, 15), date(2014, 7, 15)),
    // the subtraction lands on Sunday 15 June, which the business day convention rolls forward
    (-2, date(2014, 8, 15), date(2014, 6, 16)),
    (1, date(2014, 2, 28), date(2014, 3, 31)),
    (1, date(2014, 6, 30), date(2014, 7, 31)))

  //-------------------------------------------------------------------------
  test("test_NONE") {
    val test: PeriodAdjustment = PeriodAdjustment.NONE

    test.period shouldBe Period.ZERO
    test.additionConvention shouldBe PAC_NONE
    test.adjustment shouldBe BDA_NONE
    // Every part of the constant says nothing, so the rendering is the period alone.
    test.toString shouldBe "P0D"

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

    // A period holding days is admitted only under the convention that is not month-based, and
    // no business day adjustment moves the Sunday plain calendar arithmetic lands on.
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
    // The rendering names both steps in the order they run.
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

    // The two month-based rules differ on a base date that is the last business day of its month
    // without being the last day of it: Friday 29 August 2014 is followed by a weekend, so
    // `LAST_BUSINESS_DAY` reads it as a month end while `LAST_DAY` simply adds the month.
    val lastBusinessDay: PeriodAdjustment =
      unwrap(PeriodAdjustment.ofLastBusinessDay(Period.ofMonths(1), BDA_FOLLOW_SAT_SUN))
    val lastDay: PeriodAdjustment =
      unwrap(PeriodAdjustment.ofLastDay(Period.ofMonths(1), BDA_FOLLOW_SAT_SUN))
    lastBusinessDay.adjust(date(2014, 8, 29), REF_DATA) should haveValue(date(2014, 9, 30))
    lastDay.adjust(date(2014, 8, 29), REF_DATA) should haveValue(date(2014, 9, 29))
  }

  //-------------------------------------------------------------------------
  test("test_of_invalid_conventionForPeriod") {
    // The invariant, through all four construction routes: a period holding days paired with
    // either month-based convention is a failure carrying the reason and the factory's wording.
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
        outcome should beFailureWith(FailureReason.INVALID)
        outcome should haveFailureMessageMatching(Regex.quote(MonthBasedMessage))
        // exactly one failure: this type has one invariant, so there is nothing to accumulate with
        failuresOf(outcome) shouldBe List(Failure.Invalid(MonthBasedMessage))
      }
    }

    // The pairing is rejected rather than either part of it.
    PeriodAdjustment.of(period, PAC_NONE, BDA_NONE) should beSuccess
    PeriodAdjustment.ofLastDay(Period.of(1, 2, 0), BDA_NONE) should beSuccess
    PeriodAdjustment.ofLastBusinessDay(Period.of(1, 2, 0), BDA_NONE) should beSuccess
    PeriodAdjustment.ofLastDay(Period.ZERO, BDA_NONE) should beSuccess

    // It is the presence of a day component that is rejected rather than its sign.
    PeriodAdjustment.ofLastDay(Period.ofDays(1), BDA_NONE) should
      beFailureWith(FailureReason.INVALID)
    PeriodAdjustment.ofLastDay(Period.ofDays(-1), BDA_NONE) should
      beFailureWith(FailureReason.INVALID)

    // Nothing is raised on any of these routes; the report is the return value.
    noException should be thrownBy PeriodAdjustment.ofLastDay(period, BDA_NONE)

    // All three arguments are mandatory values of their own types, which the compiler enforces.
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
    // Row `(3, 2014-08-15, 2014-11-17)`: the input is a Friday and needs no adjustment, so
    // adjusting first and adding second would answer with the Saturday the addition lands on.
    val threeMonths: PeriodAdjustment =
      unwrap(PeriodAdjustment.ofLastDay(Period.ofMonths(3), BDA_FOLLOW_SAT_SUN))
    val additionOnly: PeriodAdjustment =
      unwrap(PeriodAdjustment.ofLastDay(Period.ofMonths(3), BDA_NONE))
    date(2014, 8, 15).getDayOfWeek shouldBe DayOfWeek.FRIDAY
    additionOnly.adjust(date(2014, 8, 15), REF_DATA) should haveValue(date(2014, 11, 15))
    date(2014, 11, 15).getDayOfWeek shouldBe DayOfWeek.SATURDAY
    threeMonths.adjust(date(2014, 8, 15), REF_DATA) should haveValue(date(2014, 11, 17))

    // Row `(1, 2014-02-28, 2014-03-31)`: without the addition convention the addition lands on
    // 28 March, and the end-of-month rule reading 28 February as a month end gives 31 March.
    val oneMonthPlain: PeriodAdjustment =
      unwrap(PeriodAdjustment.of(Period.ofMonths(1), PAC_NONE, BDA_FOLLOW_SAT_SUN))
    val oneMonthLastDay: PeriodAdjustment =
      unwrap(PeriodAdjustment.ofLastDay(Period.ofMonths(1), BDA_FOLLOW_SAT_SUN))
    oneMonthPlain.adjust(date(2014, 2, 28), REF_DATA) should haveValue(date(2014, 3, 28))
    oneMonthLastDay.adjust(date(2014, 2, 28), REF_DATA) should haveValue(date(2014, 3, 31))

    //-----------------------------------------------------------------------
    // `resolve` looks the calendar up once and binds it into the adjuster returned, which must
    // not change the answer: one adjuster answers two dates as resolving afresh for each does.
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

    // Readers compose, so adjustments assembled without reference data are supplied it once.
    val bothConventions: RefDataReader[(LocalDate, LocalDate)] =
      (
        unwrap(PeriodAdjustment.ofLastDay(Period.ofMonths(1), BDA_FOLLOW_SAT_SUN)).toReader,
        unwrap(PeriodAdjustment.ofLastBusinessDay(Period.ofMonths(1), BDA_FOLLOW_SAT_SUN)).toReader)
        .mapN((lastDayAdjuster, lastBusinessDayAdjuster) =>
          (lastDayAdjuster.adjust(date(2014, 8, 29)), lastBusinessDayAdjuster.adjust(date(2014, 8, 29))))
    bothConventions.run(REF_DATA) should haveValue((date(2014, 9, 29), date(2014, 9, 30)))

    //-----------------------------------------------------------------------
    // A calendar the reference data cannot supply is a missing-data failure, not an exception.
    val unknown: PeriodAdjustment =
      unwrap(
        PeriodAdjustment.ofLastDay(
          Period.ofMonths(3),
          BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, UNKNOWN_CALENDAR)))
    unknown.adjust(date(2014, 8, 15), REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    unknown.resolve(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    unknown.toReader.run(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    noException should be thrownBy unknown.adjust(date(2014, 8, 15), REF_DATA)

    // The failure names the identifier, and a composition fails as a whole with its failure.
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

    a.equals(b) shouldBe false
    a.equals(c) shouldBe false
    a.equals(d) shouldBe false
    Hash[PeriodAdjustment].eqv(a, b) shouldBe false
    Hash[PeriodAdjustment].eqv(a, c) shouldBe false
    Hash[PeriodAdjustment].eqv(a, d) shouldBe false

    val sameAsA: PeriodAdjustment =
      unwrap(PeriodAdjustment.ofLastDay(Period.ofMonths(3), BDA_FOLLOW_SAT_SUN))
    a.equals(sameAsA) shouldBe true
    a shouldBe sameAsA
    a.hashCode shouldBe sameAsA.hashCode
    Hash[PeriodAdjustment].eqv(a, sameAsA) shouldBe true
    Hash[PeriodAdjustment].hash(a) shouldBe Hash[PeriodAdjustment].hash(sameAsA)

    a.equals(ANOTHER_TYPE) shouldBe false
    a.equals(a.toString) shouldBe false

    // Period equality is that of `java.time.Period`, which compares the three amounts rather than
    // the length of time they denote, so twelve months is not a year.
    val twelveMonths: PeriodAdjustment =
      unwrap(PeriodAdjustment.ofLastDay(Period.ofMonths(12), BDA_FOLLOW_SAT_SUN))
    val oneYear: PeriodAdjustment =
      unwrap(PeriodAdjustment.ofLastDay(Period.ofYears(1), BDA_FOLLOW_SAT_SUN))
    twelveMonths.equals(oneYear) shouldBe false
    Hash[PeriodAdjustment].eqv(twelveMonths, oneYear) shouldBe false
  }

  //-------------------------------------------------------------------------
  test("test_beanBuilder") {
    // The factories are the whole of construction, so the routes must name the same value.
    val fromOf: PeriodAdjustment =
      unwrap(PeriodAdjustment.of(Period.ofMonths(3), PeriodAdditionConventions.LAST_DAY, BDA_FOLLOW_SAT_SUN))
    val fromOfLastDay: PeriodAdjustment =
      unwrap(PeriodAdjustment.ofLastDay(Period.ofMonths(3), BDA_FOLLOW_SAT_SUN))

    fromOf.period shouldBe Period.ofMonths(3)
    fromOf.additionConvention shouldBe PeriodAdditionConventions.LAST_DAY
    fromOf.adjustment shouldBe BDA_FOLLOW_SAT_SUN

    fromOfLastDay.period shouldBe fromOf.period
    fromOfLastDay.additionConvention shouldBe fromOf.additionConvention
    fromOfLastDay.adjustment shouldBe fromOf.adjustment

    fromOfLastDay shouldBe fromOf
    Hash[PeriodAdjustment].hash(fromOfLastDay) shouldBe Hash[PeriodAdjustment].hash(fromOf)
    Show[PeriodAdjustment].show(fromOfLastDay) shouldBe Show[PeriodAdjustment].show(fromOf)

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
    // A property is read back after a second construction rather than after a `copy`.
    val test: PeriodAdjustment =
      unwrap(PeriodAdjustment.of(Period.ofMonths(3), PeriodAdditionConventions.LAST_DAY, BDA_FOLLOW_SAT_SUN))
    val same: PeriodAdjustment =
      unwrap(PeriodAdjustment.of(Period.ofMonths(3), PeriodAdditionConventions.LAST_DAY, BDA_FOLLOW_SAT_SUN))
    val other: PeriodAdjustment =
      unwrap(PeriodAdjustment.of(Period.of(1, 2, 3), PAC_NONE, BDA_NONE))

    test.period shouldBe Period.ofMonths(3)
    test.additionConvention shouldBe PeriodAdditionConventions.LAST_DAY
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN
    other.period shouldBe Period.of(1, 2, 3)
    other.additionConvention shouldBe PAC_NONE
    other.adjustment shouldBe BDA_NONE

    // Pattern matching reads the same three fields although there is no public `apply`.
    PeriodAdjustment.unapply(test) shouldBe
      Some((Period.ofMonths(3), PeriodAdditionConventions.LAST_DAY, BDA_FOLLOW_SAT_SUN))

    test shouldBe same
    test.hashCode shouldBe same.hashCode
    Hash[PeriodAdjustment].eqv(test, same) shouldBe true
    Hash[PeriodAdjustment].hash(test) shouldBe Hash[PeriodAdjustment].hash(same)
    test should not be other
    Hash[PeriodAdjustment].eqv(test, other) shouldBe false
    test.equals(ANOTHER_TYPE) shouldBe false

    test.toString shouldBe "P3M with LastDay then apply Following using calendar Sat/Sun"
    Show[PeriodAdjustment].show(test) shouldBe test.toString
    other.toString shouldBe "P1Y2M3D"
    Show[PeriodAdjustment].show(other) shouldBe other.toString
    Show[PeriodAdjustment].show(PeriodAdjustment.NONE) shouldBe "P0D"
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // The document is an object of the three fields in declaration order: the period as ISO text,
    // the convention as its canonical name, the business day adjustment as its own object.
    val test: PeriodAdjustment =
      unwrap(PeriodAdjustment.of(Period.ofMonths(3), PeriodAdditionConventions.LAST_DAY, BDA_FOLLOW_SAT_SUN))
    val encoded: Json = test.asJson

    encoded.asObject.map(obj => obj.keys.toList) shouldBe
      Some(List("period", "additionConvention", "adjustment"))
    encoded.noSpaces shouldBe
      """{"period":"P3M","additionConvention":"LastDay",""" +
        """"adjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""

    encoded.noSpaces should not include "null"
    PeriodAdjustment.NONE.asJson.noSpaces should not include "null"

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

    unwrap(PeriodAdjustment.ofLastDay(Period.ofMonths(3), BDA_FOLLOW_SAT_SUN)).asJson.noSpaces shouldBe
      encoded.noSpaces

    // The decoder runs the fields through the factory, so a document pairing a day-bearing period
    // with a month-based convention is a decoding failure carrying the factory's wording.
    val invalid: Json = Json.obj(
      "period" -> Json.fromString("P1Y2M3D"),
      "additionConvention" -> Json.fromString("LastDay"),
      "adjustment" -> BDA_NONE.asJson)
    val rejected: Either[DecodingFailure, PeriodAdjustment] = invalid.as[PeriodAdjustment]
    rejected.isLeft shouldBe true
    rejected.left.map(failure => failure.message) shouldBe Left(MonthBasedMessage)

    // All three fields are required, and an adjustment is an object rather than a string.
    decode[PeriodAdjustment](
      """{"additionConvention":"LastDay","adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}""")
      .isLeft shouldBe true
    decode[PeriodAdjustment](
      """{"period":"P3M","adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}""")
      .isLeft shouldBe true
    decode[PeriodAdjustment]("""{"period":"P3M","additionConvention":"LastDay"}""").isLeft shouldBe true
    decode[PeriodAdjustment]("{}").isLeft shouldBe true
    Json.fromString("P3M").as[PeriodAdjustment].isLeft shouldBe true

    decode[PeriodAdjustment](
      """{"period":"P3M","additionConvention":"Rubbish",""" +
        """"adjustment":{"convention":"Following","calendar":"Sat/Sun"}}""").isLeft shouldBe true
    decode[PeriodAdjustment](
      """{"period":"Rubbish","additionConvention":"LastDay",""" +
        """"adjustment":{"convention":"Following","calendar":"Sat/Sun"}}""").isLeft shouldBe true
  }

  //-------------------------------------------------------------------------
  /** Reads the adjustment out of an outcome, failing the test with the reasons if it carries none. */
  private def unwrap(outcome: ResultNec[PeriodAdjustment]): PeriodAdjustment =
    outcome.fold(
      failures =>
        fail(
          "Expected an adjustment but the factory failed with: " +
            failures.toChain.toList.map(failure => failure.message).mkString(", ")),
      adjustment => adjustment)

  /** Reads the whole failure chain, which the matchers do not expose, so its size can be asserted. */
  private def failuresOf(outcome: ResultNec[PeriodAdjustment]): List[Failure] =
    outcome.fold(
      failures => failures.toChain.toList,
      adjustment => fail(s"Expected a failure but the factory built the adjustment $adjustment"))

}
