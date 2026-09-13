/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate
import java.time.Period
import java.util.Locale

import cats.Eq
import cats.Hash
import cats.Order
import cats.Show

import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.concurrent.TimeLimits
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor1
import org.scalatest.prop.TableFor11
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableFor7
import org.scalatest.prop.TableFor8
import org.scalatest.time.Seconds
import org.scalatest.time.Span

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.schedule.Frequency
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[DayCount]].
 *
 * The two numeric tables mark a row on which the `30/360` family applies no day-of-month
 * adjustment, and the test body computes the unadjusted expectation for such a row instead of
 * reading a literal. The day-count marker is the integer zero, so an expectation that evaluates
 * to zero is read as the marker and recomputed; [[DayCountSpec.SIMPLE_30_360DAYS]] names the one
 * row where that happens.
 *
 * A broken call is refused and a data-dependent failure is returned: dates out of order and
 * absent schedule information raise through `ArgCheck`, while text that names no day count is
 * reported as a value. `Bus/252` is covered by `Business252DayCountSpec`.
 */
class DayCountSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks with TimeLimits {

  import DayCountSpec._

  //-------------------------------------------------------------------------
  test("test_null") {
    // Both dates are required parameters of a required type, so a call that omits one or supplies
    // something else is rejected at compile time; each assertion states that of one expression.
    assertDoesNotCompile("DayCounts.ACT_360.yearFraction()")
    assertDoesNotCompile("DayCounts.ACT_360.yearFraction(java.time.LocalDate.of(2010, 1, 1))")
    assertDoesNotCompile("""DayCounts.ACT_360.yearFraction("2010-01-01", "2010-01-02")""")
    assertDoesNotCompile("DayCounts.ACT_360.days()")
    assertDoesNotCompile("DayCounts.ACT_360.days(java.time.LocalDate.of(2010, 1, 1))")
    assertDoesNotCompile("DayCounts.ACT_360.days(2010, 1, 1)")

    // The schedule information is the whole-year fixture, so the four members that read
    // something are given what they read.
    forAll(dataTypes) { (dayCount: DayCount) =>
      withClue(s"${dayCount.name}: ") {
        noException should be thrownBy dayCount.yearFraction(JAN_01, JUL_01, wholeYearInfo)
        noException should be thrownBy dayCount.relativeYearFraction(JAN_01, JUL_01, wholeYearInfo)
        noException should be thrownBy dayCount.days(JAN_01, JUL_01)
      }
    }
  }

  test("test_wrongOrder") {
    forAll(dataTypes) { (dayCount: DayCount) =>
      // The precondition: `yearFraction` and `days` are documented to take their dates in
      // time-line order, so a reversed pair is a broken call and is raised through `ArgCheck`.
      // `relativeYearFraction` is the member that accepts one.
      withClue(s"${dayCount.name} yearFraction: ") {
        val thrown = intercept[IllegalArgumentException](dayCount.yearFraction(JAN_02, JAN_01))
        thrown.getMessage shouldBe DatesOutOfOrderMessage
      }
      withClue(s"${dayCount.name} days: ") {
        val thrown = intercept[IllegalArgumentException](dayCount.days(JAN_02, JAN_01))
        thrown.getMessage shouldBe DatesOutOfOrderMessage
      }
    }
  }

  test("test_same") {
    forAll(dataTypes) { (dayCount: DayCount) =>
      withClue(s"${dayCount.name}: ") {
        if (dayCount != DayCounts.ONE_ONE) {
          // Every member but one answers for equal dates before reading any schedule
          // information, which is why the two-argument overload suffices here.
          dayCount.yearFraction(JAN_02, JAN_02) shouldBe 0d
          dayCount.days(JAN_02, JAN_02) shouldBe 0
        } else {
          // `1/1` answers one whatever the dates, which is why the sanity tests exclude it.
          dayCount.yearFraction(JAN_02, JAN_02) shouldBe 1d
          dayCount.days(JAN_02, JAN_02) shouldBe 1
        }
      }
    }
  }

  test("test_halfYear") {
    forAll(dataTypes) { (dayCount: DayCount) =>
      if (dayCount != DayCounts.ONE_ONE) {
        withClue(s"${dayCount.name}: ") {
          dayCount.yearFraction(JAN_01, JUL_01, wholeYearInfo) shouldBe 0.5d +- 0.01d
          dayCount.days(JAN_01, JUL_01) shouldBe 182 +- 2
        }
      } else {
        succeed
      }
    }
  }

  test("test_wholeYear") {
    // The `30/360` members use the whole of the five-day tolerance, answering 360.
    forAll(dataTypes) { (dayCount: DayCount) =>
      if (dayCount != DayCounts.ONE_ONE) {
        withClue(s"${dayCount.name}: ") {
          dayCount.yearFraction(JAN_01, JAN_01_NEXT, wholeYearInfo) shouldBe 1d +- 0.02d
          dayCount.days(JAN_01, JAN_01_NEXT) shouldBe 365 +- 5
        }
      } else {
        succeed
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_yearFraction") {
    // The year-fraction marker is not-a-number, which is what `expectedFraction` tests for.
    SIMPLE_30_360.isNaN shouldBe true

    forAll(dataYearFraction) {
      (dayCount: DayCount, y1: Int, m1: Int, d1: Int, y2: Int, m2: Int, d2: Int, value: Double) =>
        val expected = expectedFraction(value, y1, m1, d1, y2, m2, d2)
        val date1 = LocalDate.of(y1, m1, d1)
        val date2 = LocalDate.of(y2, m2, d2)
        withClue(s"${dayCount.name} $date1 to $date2: ") {
          dayCount.yearFraction(date1, date2) shouldBe expected
        }
    }
  }

  test("test_relativeYearFraction") {
    // The relative form reaches the same calculation without the order check in front of it.
    forAll(dataYearFraction) {
      (dayCount: DayCount, y1: Int, m1: Int, d1: Int, y2: Int, m2: Int, d2: Int, value: Double) =>
        val expected = expectedFraction(value, y1, m1, d1, y2, m2, d2)
        val date1 = LocalDate.of(y1, m1, d1)
        val date2 = LocalDate.of(y2, m2, d2)
        withClue(s"${dayCount.name} $date1 to $date2: ") {
          dayCount.relativeYearFraction(date1, date2) shouldBe expected
        }
    }
  }

  test("test_relativeYearFraction_reverse") {
    // Dates reversed: the relative form answers the negation where the plain form refuses.
    forAll(dataYearFraction) {
      (dayCount: DayCount, y1: Int, m1: Int, d1: Int, y2: Int, m2: Int, d2: Int, value: Double) =>
        val expected = expectedFraction(value, y1, m1, d1, y2, m2, d2)
        val date1 = LocalDate.of(y1, m1, d1)
        val date2 = LocalDate.of(y2, m2, d2)
        withClue(s"${dayCount.name} $date2 back to $date1: ") {
          dayCount.relativeYearFraction(date2, date1) shouldBe -expected
        }
    }
  }

  //-------------------------------------------------------------------------
  test("test_days") {
    forAll(dataDays) {
      (dayCount: DayCount, y1: Int, m1: Int, d1: Int, y2: Int, m2: Int, d2: Int, value: Int) =>
        val expected = expectedDays(value, y1, m1, d1, y2, m2, d2)
        val date1 = LocalDate.of(y1, m1, d1)
        val date2 = LocalDate.of(y2, m2, d2)
        withClue(s"${dayCount.name} $date1 to $date2: ") {
          dayCount.days(date1, date2) shouldBe expected
        }
    }
  }

  //-------------------------------------------------------------------------
  test("test_yearFraction_30U360_notEom") {
    // `30U/360` is the one member that reads the end-of-month flag, choosing between two
    // day-of-month rules by it. With the flag clear it behaves as `30/360 ISDA`, which is why
    // both read the first expectation column.
    forAll(data30U360) {
      (y1: Int, m1: Int, d1: Int, y2: Int, m2: Int, d2: Int, valueNotEom: Double, valueEom: Double) =>
        val expected = expectedFraction(valueNotEom, y1, m1, d1, y2, m2, d2)
        val date1 = LocalDate.of(y1, m1, d1)
        val date2 = LocalDate.of(y2, m2, d2)
        withClue(s"$date1 to $date2 (the end-of-month expectation being $valueEom): ") {
          DayCounts.THIRTY_U_360.yearFraction(date1, date2, Info(false)) shouldBe expected
        }
    }
  }

  test("test_yearFraction_30U360_eom") {
    // The same member with the flag set, where the end-of-February rule applies and the second
    // expectation column parts company with the first.
    forAll(data30U360) {
      (y1: Int, m1: Int, d1: Int, y2: Int, m2: Int, d2: Int, valueNotEom: Double, valueEom: Double) =>
        val expected = expectedFraction(valueEom, y1, m1, d1, y2, m2, d2)
        val date1 = LocalDate.of(y1, m1, d1)
        val date2 = LocalDate.of(y2, m2, d2)
        withClue(s"$date1 to $date2 (the plain expectation being $valueNotEom): ") {
          DayCounts.THIRTY_U_360.yearFraction(date1, date2, Info(true)) shouldBe expected
        }
    }
  }

  test("test_yearFraction_30360ISDA") {
    // `30/360 ISDA` reads no schedule information, so it answers the first expectation column
    // even though this test sets the flag.
    forAll(data30U360) {
      (y1: Int, m1: Int, d1: Int, y2: Int, m2: Int, d2: Int, valueNotEom: Double, valueEom: Double) =>
        val expected = expectedFraction(valueNotEom, y1, m1, d1, y2, m2, d2)
        val date1 = LocalDate.of(y1, m1, d1)
        val date2 = LocalDate.of(y2, m2, d2)
        withClue(s"$date1 to $date2 (the end-of-month expectation being $valueEom): ") {
          DayCounts.THIRTY_360_ISDA.yearFraction(date1, date2, Info(true)) shouldBe expected
        }
    }
  }

  test("test_yearFraction_30U360EOM") {
    // `30U/360 EOM` applies the end-of-February rule unconditionally, so it answers the second
    // expectation column whatever the flag says.
    forAll(data30U360) {
      (y1: Int, m1: Int, d1: Int, y2: Int, m2: Int, d2: Int, valueNotEom: Double, valueEom: Double) =>
        val expected = expectedFraction(valueEom, y1, m1, d1, y2, m2, d2)
        val date1 = LocalDate.of(y1, m1, d1)
        val date2 = LocalDate.of(y2, m2, d2)
        withClue(s"$date1 to $date2 (the plain expectation being $valueNotEom): ") {
          DayCounts.THIRTY_U_360_EOM.yearFraction(date1, date2, Info(true)) shouldBe expected
        }
    }
  }

  //-------------------------------------------------------------------------
  test("test_yearFraction_30E360ISDA_notMaturity") {
    // `30E/360 ISDA` changes a second day-of-month at the end of February to 30 unless that date
    // is the maturity, so it reads the end date, and only in that case. An end date it reaches
    // the point of reading may not be absent, so the fixture states the schedule ending a year
    // after the second date; the four rows ending on the last day of February depend on it.
    forAll(data30E360ISDA) {
      (y1: Int, m1: Int, d1: Int, y2: Int, m2: Int, d2: Int, valueNotMaturity: Double, valueMaturity: Double) =>
        val expected = expectedFraction(valueNotMaturity, y1, m1, d1, y2, m2, d2)
        val date1 = LocalDate.of(y1, m1, d1)
        val date2 = LocalDate.of(y2, m2, d2)
        val info = Info(None, Some(date2.plusYears(1L)), None, false, None)
        withClue(s"$date1 to $date2 (the maturity expectation being $valueMaturity): ") {
          DayCounts.THIRTY_E_360_ISDA.yearFraction(date1, date2, info) shouldBe expected
        }
    }
  }

  test("test_yearFraction_30E360ISDA_maturity") {
    // The same rows with the second date declared to be the maturity, which suppresses the
    // end-of-February rule on it.
    forAll(data30E360ISDA) {
      (y1: Int, m1: Int, d1: Int, y2: Int, m2: Int, d2: Int, valueNotMaturity: Double, valueMaturity: Double) =>
        val expected = expectedFraction(valueMaturity, y1, m1, d1, y2, m2, d2)
        val date1 = LocalDate.of(y1, m1, d1)
        val date2 = LocalDate.of(y2, m2, d2)
        val info = Info(None, Some(date2), None, false, Some(Frequency.P3M))
        withClue(s"$date1 to $date2 (the plain expectation being $valueNotMaturity): ") {
          DayCounts.THIRTY_E_360_ISDA.yearFraction(date1, date2, info) shouldBe expected
        }
    }
  }

  //-------------------------------------------------------------------------
  test("test_yearFraction_ACTACTAFB") {
    // The rule is under-specified, so these rows, and the reading recorded with the table, are
    // the specification of this member's behaviour.
    forAll(dataACTACTAFB) { (y1: Int, m1: Int, d1: Int, y2: Int, m2: Int, d2: Int, expected: Double) =>
      val date1 = LocalDate.of(y1, m1, d1)
      val date2 = LocalDate.of(y2, m2, d2)
      withClue(s"$date1 to $date2: ") {
        DayCounts.ACT_ACT_AFB.yearFraction(date1, date2) shouldBe expected
      }
    }
  }

  test("test_yearFraction_ACT365L") {
    // `Act/365L` reads the end of the schedule period containing the first date and the
    // frequency, and nothing else: an annual frequency puts the leap day test on the whole
    // period, any other frequency asks only whether the period ends in a leap year.
    forAll(dataACT365L) {
      (
          y1: Int,
          m1: Int,
          d1: Int,
          y2: Int,
          m2: Int,
          d2: Int,
          freq: Frequency,
          y3: Int,
          m3: Int,
          d3: Int,
          expected: Double) =>
        val date1 = LocalDate.of(y1, m1, d1)
        val date2 = LocalDate.of(y2, m2, d2)
        val periodEnd = LocalDate.of(y3, m3, d3)
        val info = Info(None, None, Some(periodEnd), false, Some(freq))
        withClue(s"$date1 to $date2, period ending $periodEnd at $freq: ") {
          DayCounts.ACT_365L.yearFraction(date1, date2, info) shouldBe expected
        }
    }
  }

  //-------------------------------------------------------------------------
  // The canonical `Act/Act ICMA` worked examples. Each states its expectation as the arithmetic
  // of the nominal periods the convention is defined over - named above the case where it spans
  // more than one - so a "simplified" expectation would remove the statement of the answer.
  test("test_actActIcma_singlePeriod") {
    val start = LocalDate.of(2003, 11, 1)
    val end = LocalDate.of(2004, 5, 1)
    val info = Info(Some(start), Some(end), Some(end), true, Some(Frequency.P6M))
    DayCounts.ACT_ACT_ICMA.yearFraction(start, end.minusDays(1L), info) shouldBe (181d / (182d * 2d))
    DayCounts.ACT_ACT_ICMA.yearFraction(start, end, info) shouldBe (182d / (182d * 2d))
  }

  test("test_actActIcma_longInitialStub_eomFlagEom_short") {
    // nominals, 2011-08-31 (P91D) 2011-11-30 (P91D) 2012-02-29
    val start = LocalDate.of(2011, 10, 1)
    val periodEnd = LocalDate.of(2012, 2, 29)
    val end = LocalDate.of(2011, 11, 12) // before first nominal
    val info =
      Info(Some(start), Some(periodEnd.plus(Frequency.P3M.period)), Some(periodEnd), true, Some(Frequency.P3M))
    DayCounts.ACT_ACT_ICMA.yearFraction(start, end, info) shouldBe (42d / (91d * 4d))
  }

  test("test_actActIcma_longInitialStub_eomFlagEom_long") {
    // nominals, 2011-08-31 (P91D) 2011-11-30 (P91D) 2012-02-29
    val start = LocalDate.of(2011, 10, 1)
    val periodEnd = LocalDate.of(2012, 2, 29)
    val end = LocalDate.of(2012, 1, 12) // after first nominal
    val info =
      Info(Some(start), Some(periodEnd.plus(Frequency.P3M.period)), Some(periodEnd), true, Some(Frequency.P3M))
    DayCounts.ACT_ACT_ICMA.yearFraction(start, end, info) shouldBe ((60d / (91d * 4d)) + (43d / (91d * 4d)))
  }

  test("test_actActIcma_veryLongInitialStub_eomFlagEom_short") {
    // nominals, 2011-05-31 (P92D) 2011-08-31 (P91D) 2011-11-30 (P91D) 2012-02-29
    val start = LocalDate.of(2011, 7, 1)
    val periodEnd = LocalDate.of(2012, 2, 29)
    val end = LocalDate.of(2011, 8, 12) // before first nominal
    val info =
      Info(Some(start), Some(periodEnd.plus(Frequency.P3M.period)), Some(periodEnd), true, Some(Frequency.P3M))
    DayCounts.ACT_ACT_ICMA.yearFraction(start, end, info) shouldBe (42d / (92d * 4d))
  }

  test("test_actActIcma_veryLongInitialStub_eomFlagEom_mid") {
    // nominals, 2011-05-31 (P92D) 2011-08-31 (P91D) 2011-11-30 (P91D) 2012-02-29
    val start = LocalDate.of(2011, 7, 1)
    val periodEnd = LocalDate.of(2012, 2, 29)
    val end = LocalDate.of(2011, 11, 12)
    val info =
      Info(Some(start), Some(periodEnd.plus(Frequency.P3M.period)), Some(periodEnd), true, Some(Frequency.P3M))
    DayCounts.ACT_ACT_ICMA.yearFraction(start, end, info) shouldBe ((61d / (92d * 4d)) + (73d / (91d * 4d)))
  }

  test("test_actActIcma_longInitialStub_notEomFlagEom_short") {
    // nominals, 2011-08-29 (P92D) 2011-11-29 (P92D) 2012-02-29
    val start = LocalDate.of(2011, 10, 1)
    val periodEnd = LocalDate.of(2012, 2, 29)
    val end = LocalDate.of(2011, 11, 12) // before first nominal
    val info =
      Info(Some(start), Some(periodEnd.plus(Frequency.P3M.period)), Some(periodEnd), false, Some(Frequency.P3M))
    DayCounts.ACT_ACT_ICMA.yearFraction(start, end, info) shouldBe (42d / (92d * 4d))
  }

  test("test_actActIcma_longInitialStub_notEomFlagEom_long") {
    // nominals, 2011-08-29 (P92D) 2011-11-29 (P92D) 2012-02-29
    val start = LocalDate.of(2011, 10, 1)
    val periodEnd = LocalDate.of(2012, 2, 29)
    val end = LocalDate.of(2012, 1, 12) // after first nominal
    val info =
      Info(Some(start), Some(periodEnd.plus(Frequency.P3M.period)), Some(periodEnd), false, Some(Frequency.P3M))
    DayCounts.ACT_ACT_ICMA.yearFraction(start, end, info) shouldBe ((59d / (92d * 4d)) + (44d / (92d * 4d)))
  }

  //-------------------------------------------------------------------------
  test("test_actActIcma_longFinalStub_eomFlagEom_short") {
    // nominals, 2011-08-31 (P91D) 2011-11-30 (P91D) 2012-02-29
    val start = LocalDate.of(2011, 8, 31)
    val periodEnd = LocalDate.of(2012, 1, 31)
    val end = LocalDate.of(2011, 11, 12) // before first nominal
    val info =
      Info(Some(start.minus(Frequency.P3M.period)), Some(periodEnd), Some(periodEnd), true, Some(Frequency.P3M))
    DayCounts.ACT_ACT_ICMA.yearFraction(start, end, info) shouldBe (73d / (91d * 4d))
  }

  test("test_actActIcma_longFinalStub_eomFlagEom_long") {
    // nominals, 2011-08-31 (P91D) 2011-11-30 (P91D) 2012-02-29
    val start = LocalDate.of(2011, 8, 31)
    val periodEnd = LocalDate.of(2012, 1, 31)
    val end = LocalDate.of(2012, 1, 12) // after first nominal
    val info =
      Info(Some(start.minus(Frequency.P3M.period)), Some(periodEnd), Some(periodEnd), true, Some(Frequency.P3M))
    DayCounts.ACT_ACT_ICMA.yearFraction(start, end, info) shouldBe ((91d / (91d * 4d)) + (43d / (91d * 4d)))
  }

  test("test_actActIcma_longFinalStub_notEomFlagEom_short") {
    // nominals, 2012-02-29 (P90D) 2012-05-29 (P92D) 2012-08-29
    val start = LocalDate.of(2012, 2, 29)
    val periodEnd = LocalDate.of(2012, 7, 31)
    val end = LocalDate.of(2012, 4, 1) // before first nominal
    val info =
      Info(Some(start.minus(Frequency.P3M.period)), Some(periodEnd), Some(periodEnd), false, Some(Frequency.P3M))
    DayCounts.ACT_ACT_ICMA.yearFraction(start, end, info) shouldBe (32d / (90d * 4d))
  }

  test("test_actActIcma_longFinalStub_notEomFlagEom_long") {
    // nominals, 2012-02-29 (P90D) 2012-05-29 (P92D) 2012-08-29
    val start = LocalDate.of(2012, 2, 29)
    val periodEnd = LocalDate.of(2012, 7, 31)
    val end = LocalDate.of(2012, 6, 1) // after first nominal
    val info =
      Info(Some(start.minus(Frequency.P3M.period)), Some(periodEnd), Some(periodEnd), false, Some(Frequency.P3M))
    DayCounts.ACT_ACT_ICMA.yearFraction(start, end, info) shouldBe ((90d / (90d * 4d)) + (3d / (92d * 4d)))
  }

  test("test_actActIcma_middle") {
    // nominals, 2012-03-30 (P92D) 2012-06-30 (2011, 11, 2012-09-30)
    val start = LocalDate.of(2012, 4, 10)
    val end = LocalDate.of(2012, 5, 10)
    val periodEnd = LocalDate.of(2012, 6, 30)
    val scheduleStart = LocalDate.of(2011, 12, 30)
    val scheduleEnd = LocalDate.of(2012, 9, 30)
    val info = Info(Some(scheduleStart), Some(scheduleEnd), Some(periodEnd), false, Some(Frequency.P3M))
    DayCounts.ACT_ACT_ICMA.yearFraction(start, end, info) shouldBe (30d / (4d * 92d))
  }

  //-------------------------------------------------------------------------
  // The official `Act/Act` examples - http://www.isda.org/c_and_a/pdf/ACT-ACT-ISDA-1999.pdf -
  // each asserting the three `Act/Act` members against one pair of dates, because the statement
  // being made is that the three answers differ in the documented way.
  test("test_actAct_isdaTestCase_normal") {
    val start = LocalDate.of(2003, 11, 1)
    val end = LocalDate.of(2004, 5, 1)
    val info = Info(Some(start), Some(end.plus(Frequency.P6M.period)), Some(end), true, Some(Frequency.P6M))
    DayCounts.ACT_ACT_ISDA.yearFraction(start, end) shouldBe ((61d / 365d) + (121d / 366d))
    DayCounts.ACT_ACT_ICMA.yearFraction(start, end, info) shouldBe (182d / (182d * 2d))
    DayCounts.ACT_ACT_AFB.yearFraction(start, end) shouldBe (182d / 366d)
  }

  test("test_actAct_isdaTestCase_shortInitialStub") {
    val start = LocalDate.of(1999, 2, 1)
    val firstRegular = LocalDate.of(1999, 7, 1)
    val end = LocalDate.of(2000, 7, 1)
    val info1 =
      Info(Some(start), Some(end.plus(Frequency.P12M.period)), Some(firstRegular), true, Some(Frequency.P12M))
    val info2 = Info(Some(start), Some(end.plus(Frequency.P12M.period)), Some(end), true, Some(Frequency.P12M))
    DayCounts.ACT_ACT_ISDA.yearFraction(start, firstRegular) shouldBe (150d / 365d)
    DayCounts.ACT_ACT_ICMA.yearFraction(start, firstRegular, info1) shouldBe (150d / (365d * 1d))
    DayCounts.ACT_ACT_AFB.yearFraction(start, firstRegular) shouldBe (150d / 365d)

    DayCounts.ACT_ACT_ISDA.yearFraction(firstRegular, end) shouldBe ((184d / 365d) + (182d / 366d))
    DayCounts.ACT_ACT_ICMA.yearFraction(firstRegular, end, info2) shouldBe (366d / (366d * 1d))
    DayCounts.ACT_ACT_AFB.yearFraction(firstRegular, end) shouldBe (366d / 366d)
  }

  test("test_actAct_isdaTestCase_longInitialStub") {
    val start = LocalDate.of(2002, 8, 15)
    val firstRegular = LocalDate.of(2003, 7, 15)
    val end = LocalDate.of(2004, 1, 15)
    val info1 = Info(Some(start), Some(end), Some(firstRegular), true, Some(Frequency.P6M))
    val info2 = Info(Some(start), Some(end), Some(end), true, Some(Frequency.P6M))
    DayCounts.ACT_ACT_ISDA.yearFraction(start, firstRegular) shouldBe (334d / 365d)
    DayCounts.ACT_ACT_ICMA.yearFraction(start, firstRegular, info1) shouldBe
      ((181d / (181d * 2d)) + (153d / (184d * 2d)))
    DayCounts.ACT_ACT_AFB.yearFraction(start, firstRegular) shouldBe (334d / 365d)
    // example is wrong in 1998 euro swap version
    DayCounts.ACT_ACT_ISDA.yearFraction(firstRegular, end) shouldBe ((170d / 365d) + (14d / 366d))
    DayCounts.ACT_ACT_ICMA.yearFraction(firstRegular, end, info2) shouldBe (184d / (184d * 2d))
    DayCounts.ACT_ACT_AFB.yearFraction(firstRegular, end) shouldBe (184d / 365d)
  }

  test("test_actAct_isdaTestCase_shortFinalStub") {
    val start = LocalDate.of(1999, 7, 30)
    val lastRegular = LocalDate.of(2000, 1, 30)
    val end = LocalDate.of(2000, 6, 30)
    val info1 = Info(Some(start), Some(end), Some(lastRegular), true, Some(Frequency.P6M))
    val info2 = Info(Some(start), Some(end), Some(end), true, Some(Frequency.P6M))
    DayCounts.ACT_ACT_ISDA.yearFraction(start, lastRegular) shouldBe ((155d / 365d) + (29d / 366d))
    DayCounts.ACT_ACT_ICMA.yearFraction(start, lastRegular, info1) shouldBe (184d / (184d * 2d))
    DayCounts.ACT_ACT_AFB.yearFraction(start, lastRegular) shouldBe (184d / 365d)

    DayCounts.ACT_ACT_ISDA.yearFraction(lastRegular, end) shouldBe (152d / 366d)
    DayCounts.ACT_ACT_ICMA.yearFraction(lastRegular, end, info2) shouldBe (152d / (182d * 2d))
    DayCounts.ACT_ACT_AFB.yearFraction(lastRegular, end) shouldBe (152d / 366d)
  }

  test("test_actAct_isdaTestCase_longFinalStub") {
    val start = LocalDate.of(1999, 11, 30)
    val end = LocalDate.of(2000, 4, 30)
    val info = Info(Some(start.minus(Frequency.P3M.period)), Some(end), Some(end), true, Some(Frequency.P3M))
    DayCounts.ACT_ACT_ISDA.yearFraction(start, end) shouldBe ((32d / 365d) + (120d / 366d))
    DayCounts.ACT_ACT_ICMA.yearFraction(start, end, info) shouldBe ((91d / (91d * 4d)) + (61d / (92d * 4d)))
    DayCounts.ACT_ACT_AFB.yearFraction(start, end) shouldBe (152d / 366d)
  }

  //-------------------------------------------------------------------------
  test("test_actActYearVsIcma") {
    // Over a sweep of 400 consecutive start dates and every period of up to a year from each,
    // the annual `Act/Act ICMA` answer and the `Act/Act Year` answer agree to the last bit. No
    // clue is built per iteration: at this volume the strings would cost more than the
    // arithmetic, so a message is assembled only for a pair that disagrees.
    val firstStart = LocalDate.of(2011, 1, 1)
    (0 until 400).foreach { i =>
      val start = firstStart.plusDays(i.toLong)
      (0 until 365).foreach { j =>
        val end = start.plusDays(j.toLong)
        val info = Info(Some(start), Some(end), Some(start.plusYears(1L)), false, Some(Frequency.P12M))
        val icma = DayCounts.ACT_ACT_ICMA.yearFraction(start, end, info)
        val year = DayCounts.ACT_ACT_YEAR.yearFraction(start, end)
        if (icma != year) {
          fail(s"Act/Act ICMA gave $icma and Act/Act Year gave $year for $start to $end")
        }
      }
    }
    succeed
  }

  //-------------------------------------------------------------------------
  test("test_name") {
    forAll(dataName) { (dayCount: DayCount, name: String) =>
      dayCount.name shouldBe name
    }
  }

  test("test_toString") {
    forAll(dataName) { (dayCount: DayCount, name: String) =>
      withClue(s"$name: ") {
        dayCount.toString shouldBe name
        Show[DayCount].show(dayCount) shouldBe name
      }
    }
  }

  test("test_of_lookup") {
    // Each member is keyed under its canonical name and the upper case of it, and both lookups
    // are asserted for both spellings so the two entry points are held to the same answer.
    forAll(dataName) { (dayCount: DayCount, name: String) =>
      withClue(s"$name: ") {
        DayCount.valueOf(name) shouldBe Some(dayCount)
        DayCount.parse(name) should haveValue(dayCount)

        val upperCase = name.toUpperCase(Locale.ENGLISH)
        DayCount.valueOf(upperCase) shouldBe Some(dayCount)
        DayCount.parse(upperCase) should haveValue(dayCount)
      }
    }
  }

  test("test_lenientLookup_standardNames") {
    // For the 20 names that contain a letter the lower-case spelling is outside the exact key
    // space and resolves only through the leniency; `1/1` contains none, so its lower-case
    // spelling *is* the canonical name and the exact lookup answers it directly.
    forAll(dataName) { (dayCount: DayCount, name: String) =>
      val lowerCase = name.toLowerCase(Locale.ENGLISH)
      withClue(s"$lowerCase: ") {
        DayCount.parse(lowerCase) should haveValue(dayCount)
        if (lowerCase == name) {
          DayCount.valueOf(lowerCase) shouldBe Some(dayCount)
        } else {
          DayCount.valueOf(lowerCase) shouldBe None
        }
      }
    }
  }

  test("test_extendedEnum") {
    // The family is closed and its tables are data, so this is where they are read row for row.
    val lookup = DayCount.namedEnum

    forAll(dataName) { (dayCount: DayCount, name: String) =>
      withClue(s"$name: ")(lookup.byCanonicalName.get(name) shouldBe Some(dayCount))
    }

    lookup.familyName shouldBe "DayCount"
    lookup.toString shouldBe "NamedEnum[DayCount]"
    lookup.values.toList shouldBe declarationOrder

    lookup.byCanonicalName should have size 21
    lookup.byCanonicalName.keySet shouldBe standardNames.toSet
    lookup.byUpperName should have size 21
    lookup.byUpperName.keySet shouldBe standardNames.map(_.toUpperCase(Locale.ENGLISH)).toSet

    // Ten of the 21 names contain a lower-case letter and so contribute a second key; the other
    // eleven are already their own upper case, which is why the union holds 31 keys and not 42.
    val allKeys = lookup.byCanonicalName.keySet ++ lookup.byUpperName.keySet
    allKeys should have size 31
    standardNames.filter(name => name != name.toUpperCase(Locale.ENGLISH)) should have size 10

    // Every spelling beyond those 31 keys arrives through the lenient chain.
    lookup.alternateNames shouldBe Map.empty[String, String]

    // The external spellings take part in no lookup: `ACT/360` resolves because the lenient
    // chain accepts it, not because FpML publishes it.
    lookup.externalNameGroups shouldBe Set("FpML", "SWIFT")
    lookup.externalNamesRaw("FpML") shouldBe
      Some(
        Map(
          "1/1" -> "1/1",
          "30/360" -> "30/360 ISDA",
          "30E/360" -> "30E/360",
          "30E/360.ISDA" -> "30E/360 ISDA",
          "ACT/360" -> "Act/360",
          "ACT/365.FIXED" -> "Act/365F",
          "ACT/365" -> "Act/365F",
          "ACT/365L" -> "Act/365L",
          "ACT/ACT.AFB" -> "Act/Act AFB",
          "ACT/ACT.ICMA" -> "Act/Act ICMA",
          "ACT/ACT.ISMA" -> "Act/Act ICMA",
          "ACT/ACT.ISDA" -> "Act/Act ISDA",
          "ACT/365.ISDA" -> "Act/Act ISDA",
          "BUS/252" -> "Bus/252 BRBD"))
    lookup.externalNamesRaw("SWIFT") shouldBe
      Some(
        Map(
          "30E/360" -> "30E/360 ISDA",
          "360/360" -> "30U/360",
          "ACT/360" -> "Act/360",
          "ACT/365" -> "Act/Act ISDA",
          "AFI/365" -> "Act/365F",
          "EBD/360" -> "30E/360",
          "EXA/EXA" -> "Act/Act AFB",
          "ICM/ACT" -> "Act/Act ICMA"))

    // The FpML group is the one place a published external row names something outside the
    // closed family. `BUS/252` resolves none the less, because an external row is resolved
    // through `DayCount.valueOf`, which spans the standard members and the calendar-bearing
    // `Bus/252` conventions together, while `values` holds only the former.
    lookup.externalNames("FpML").map(_.size) shouldBe Some(14)
    lookup.externalNames("FpML").map(_.keySet) shouldBe lookup.externalNamesRaw("FpML").map(_.keySet)
    lookup.externalNames("FpML").flatMap(_.get("BUS/252")) shouldBe
      Some(DayCount.ofBus252(StandardHolidayCalendars.BRBD))
    lookup.externalNamesRaw("FpML").flatMap(_.get("BUS/252")) shouldBe Some("Bus/252 BRBD")
    lookup.values.toList.contains(DayCount.ofBus252(StandardHolidayCalendars.BRBD)) shouldBe false
    DayCount.valueOf("Bus/252 BRBD") shouldBe Some(DayCount.ofBus252(StandardHolidayCalendars.BRBD))
    DayCount.parse("BUS/252") should haveValue(DayCount.ofBus252(StandardHolidayCalendars.BRBD))

    lookup.externalNames("FpML").flatMap(_.get("ACT/ACT.ISMA")) shouldBe Some(DayCounts.ACT_ACT_ICMA)
    lookup.externalNames("SWIFT").map(_.size) shouldBe Some(8)
    // The two groups disagree about two spellings, which is why they are held separately.
    lookup.externalNames("FpML").flatMap(_.get("30E/360")) shouldBe Some(DayCounts.THIRTY_E_360)
    lookup.externalNames("SWIFT").flatMap(_.get("30E/360")) shouldBe Some(DayCounts.THIRTY_E_360_ISDA)
    lookup.externalNames("FpML").flatMap(_.get("ACT/365")) shouldBe Some(DayCounts.ACT_365F)
    lookup.externalNames("SWIFT").flatMap(_.get("ACT/365")) shouldBe Some(DayCounts.ACT_ACT_ISDA)

    lookup.externalNames("Rubbish") shouldBe None
    lookup.externalNamesRaw("Rubbish") shouldBe None

    // The order of the lenient table is part of the data: `parse` applies every pattern in turn
    // and a later one sees what an earlier one produced, so the sequence of replacements is
    // asserted and not only the size. `lenientSources` is the raw text view, so reading the rows
    // this way compiles none of the expressions.
    lookup.lenientSources should have size 67
    lookup.lenientSources.map { case (_, replacement) => replacement } shouldBe lenientReplacements
    lookup.lenientSources.map { case (source, _) => source }.head shouldBe "ACTUAL/ACTUAL(.*)"
  }

  test("test_of_lookup_notFound") {
    // Text naming no member is reported as a value rather than raised: its content decides the
    // outcome, so the outcome carries a reason and a message naming the family and the text.
    DayCount.valueOf("Rubbish") shouldBe None
    DayCount.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
    DayCount.parse("Rubbish") should haveFailureMessageMatching("DayCount name not found: Rubbish")

    // A `Bus/252` name is recognised as far as its prefix, so an undefined calendar makes the
    // calendar's own failure the one reported.
    DayCount.valueOf("Bus/252 ZZZZ") shouldBe None
    DayCount.parse("Bus/252 ZZZZ") should beFailureWith(FailureReason.PARSING)
    DayCount.parse("Bus/252 ZZZZ") should
      haveFailureMessageMatching("HolidayCalendar name not found: ZZZZ")

    // An identifier the reference data cannot resolve is missing data, not unparseable text.
    DayCount.ofBus252(HolidayCalendarId.of("ZZZZ"), ReferenceData.standard) should
      beFailureWith(FailureReason.MISSING_DATA)
    DayCount.ofBus252(HolidayCalendarIds.BRBD, ReferenceData.standard) should
      haveValue(DayCount.ofBus252(StandardHolidayCalendars.BRBD))
  }

  test("test_of_lookup_null") {
    // The name is a required parameter of a required type, so an omitted or mistyped argument is
    // rejected at compile time; each assertion states that of one expression.
    assertDoesNotCompile("DayCount.parse()")
    assertDoesNotCompile("DayCount.valueOf()")
    assertDoesNotCompile("DayCount.parse(1)")
    assertDoesNotCompile("DayCount.valueOf(1)")

    val hostile: List[String] =
      List(
        "",
        "   ",
        "\t\n",
        "Act/365F ",
        " Act/365F",
        "Act /365F",
        "null",
        "ACT__365F",
        "Act/365F+Act/360",
        "Bus/252",
        "Bus/252  ")

    hostile.filterNot(_ == "Bus/252").foreach { name =>
      withClue(s"[$name]: ") {
        DayCount.valueOf(name) shouldBe None
        DayCount.parse(name) should beFailureWith(FailureReason.PARSING)
        noException should be thrownBy DayCount.parse(name)
      }
    }

    // `Bus/252` sits in the list above to be excluded from it: it is the one entry that does
    // resolve, the lenient table defaulting it to the Brazilian calendar, which is what keeps
    // the near-miss `"Bus/252  "` from reading as an accident.
    DayCount.parse("Bus/252") should haveValue(DayCount.ofBus252(StandardHolidayCalendars.BRBD))

    // `parse` runs all 67 rewrites over its input, several of which hold a group that can consume
    // text of unbounded length, so the cost of rejecting text must be a function of its length
    // and not of its length squared: `(.*)[(](.*)[)]` has an open bracket to try at every one of
    // two hundred thousand positions. The time limit is a regression guard on that cost.
    val oversized: String = "(" * 200000
    val oversizedOutcome: ResultNec[DayCount] = failAfter(Span(30L, Seconds))(DayCount.parse(oversized))
    oversizedOutcome should beFailureWith(FailureReason.PARSING)
    noException should be thrownBy DayCount.parse(oversized)
    DayCount.valueOf(oversized) shouldBe None

    // And the other half of that statement: text is never rejected for its size at the exact
    // stage of the lookup, which is the stage a `Bus/252` name resolves in - it is claimed by the
    // wider provider of this family, never by the rewrites. A `Bus/252` name may carry a combined
    // calendar of any number of parts, so ten thousand characters naming two thousand and one
    // calendars resolve to the day count over London and New York - the duplicate parts folding
    // away, as a calendar combined with itself does - where a length cutoff applied to the exact
    // stage would have refused the name outright.
    val longCalendar: String = "Bus/252 " + ("GBLO+" * 2000) + "USNY"
    longCalendar.length shouldBe 10012
    DayCount.parse(longCalendar).map(dayCount => dayCount.name) should haveValue("Bus/252 GBLO+USNY")
    DayCount.valueOf(longCalendar).map(dayCount => dayCount.name) shouldBe Some("Bus/252 GBLO+USNY")

    // The lenient stage is the bounded one, and this is where the two stages part company. The
    // ceiling the name lookup derives from this family's own data - its longest key, alternate
    // spelling, alternate target and expression source, plus the margin the typeclass adds - is
    // what the chain is applied within: text inside it is rewritten row by row as it always was,
    // and text beyond it comes back exactly as it was handed over, no rule having run over it.
    // That bound is what makes refusing a name cost what the name costs, and this family is the
    // one it matters most for, holding sixty-seven rows and running the chain itself rather than
    // through `parse`.
    DayCount.namedEnum.rewriteLeniently("ACT/ACT") shouldBe "Act/Act ISDA"
    val beyondTheCeiling: String = "ACT/ACT" + ("X" * 10000)
    beyondTheCeiling.length should be > DayCount.namedEnum.lenientLengthCeiling
    DayCount.namedEnum.rewriteLeniently(beyondTheCeiling) shouldBe beyondTheCeiling

    // And this family applies that same bound itself, ahead of the fold, because it runs stage two
    // rather than delegating it. The distinction is invisible in the assertions above - their text
    // is upper case already, so folding it copies nothing - and it is the whole of the cost for
    // text that is not: a fold is a copy of the whole input, and the input is as long as whoever
    // supplied it chose. Lower-case text beyond the ceiling is therefore reported unresolved
    // without being folded and without a rewrite being offered it, through `parse` and through the
    // decoder of a document alike, and the failure is the ordinary one this family gives.
    val lowerCaseBeyondTheCeiling: String = "act/act" + ("x" * 10000)
    lowerCaseBeyondTheCeiling.length should be > DayCount.namedEnum.lenientLengthCeiling
    DayCount.parse(lowerCaseBeyondTheCeiling) should beFailureWith(FailureReason.PARSING)
    DayCount.parse(lowerCaseBeyondTheCeiling, ReferenceData.standard) should
      beFailureWith(FailureReason.PARSING)
    DayCount.valueOf(lowerCaseBeyondTheCeiling) shouldBe None
    Json.fromString(lowerCaseBeyondTheCeiling).as[DayCount].isLeft shouldBe true

    // The spelling inside the ceiling still resolves through the fold and the rewrites, which is
    // what makes the assertion above a statement about length rather than about case.
    DayCount.parse("act/act") should haveValue(DayCounts.ACT_ACT_ISDA)
  }

  //-------------------------------------------------------------------------
  test("test_lenientLookup_specialNames") {
    // Each row is asserted in three spellings: the input is folded to upper case before the
    // patterns are applied, and the patterns match insensitively to case.
    forAll(dataLenient) { (name: String, dayCount: DayCount) =>
      withClue(s"$name: ") {
        DayCount.parse(name.toLowerCase(Locale.ENGLISH)) should haveValue(dayCount)
        DayCount.parse(name) should haveValue(dayCount)
        DayCount.parse(name.toUpperCase(Locale.ENGLISH)) should haveValue(dayCount)
      }
    }
  }

  test("test_lenientLookup_constants") {
    // Each identifier the constants holder publishes resolves leniently to the constant it
    // names, in its own spelling and folded to lower case.
    forAll(dataConstantIdentifiers) { (identifier: String, dayCount: DayCount) =>
      withClue(s"$identifier: ") {
        DayCount.parse(identifier) should haveValue(dayCount)
        DayCount.parse(identifier.toLowerCase(Locale.ENGLISH)) should haveValue(dayCount)
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_relativeYearFraction_defaultMethod") {
    // `DayCount` is sealed, so no stand-in implementation can be declared here to exercise the
    // inherited `relativeYearFraction` in isolation; the property it carries is asserted over
    // real members instead.
    val date1 = date(2015, 6, 1)
    val date2 = date(2015, 7, 1)

    DayCounts.ACT_365F.relativeYearFraction(date1, date2) shouldBe
      DayCounts.ACT_365F.yearFraction(date1, date2)
    DayCounts.ACT_365F.relativeYearFraction(date2, date1) shouldBe
      -DayCounts.ACT_365F.relativeYearFraction(date1, date2)
    DayCounts.ACT_365F.relativeYearFraction(date1, date2) shouldBe (30d / 365d)
    DayCounts.ACT_365F.relativeYearFraction(date2, date1) shouldBe (-30d / 365d)

    DayCounts.ONE_ONE.relativeYearFraction(date1, date2) shouldBe 1d
    DayCounts.ONE_ONE.relativeYearFraction(date2, date1) shouldBe -1d

    // Over the whole family: the relative form is antisymmetric and answers for a reversed pair.
    forAll(dataTypes) { (dayCount: DayCount) =>
      withClue(s"${dayCount.name}: ") {
        dayCount.relativeYearFraction(date1, date2, wholeYearInfo) shouldBe
          dayCount.yearFraction(date1, date2, wholeYearInfo)
        dayCount.relativeYearFraction(date2, date1, wholeYearInfo) shouldBe
          -dayCount.relativeYearFraction(date1, date2, wholeYearInfo)
        // The same precondition as `test_wrongOrder`: a reversed pair breaks the documented
        // contract of `yearFraction`, so it is raised through `ArgCheck` rather than returned.
        intercept[IllegalArgumentException](dayCount.yearFraction(date2, date1, wholeYearInfo))
          .getMessage shouldBe DatesOutOfOrderMessage
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_scheduleInfo") {
    // The bare schedule information reports the end-of-month convention as in use and `None`
    // to everything else: "this schedule does not know" is a value, not a refusal.
    val test = DayCount.ScheduleInfo.simple
    test.isEndOfMonthConvention shouldBe true
    test.startDate shouldBe None
    test.endDate shouldBe None
    test.frequency shouldBe None
    test.periodEndDate(JAN_01) shouldBe None

    // Total accessors do not make a day count answer without the facts its rule is defined in
    // terms of: a schedule that cannot supply them breaks the contract of the call and is
    // refused through `ArgCheck`. The precondition, member by member, is
    //
    //   - `Act/Act ICMA` reads the end of the schedule, the end of the period containing the
    //     first date, the frequency and the flag;
    //   - `Act/365L` reads the period end and the frequency;
    //   - `30E/360 ISDA` reads the end of the schedule, and only where the second date is the
    //     last day of February and is not already being adjusted;
    //   - `30U/360` reads the flag, which is the one accessor that is not optional, so it never
    //     refuses.
    intercept[IllegalArgumentException](DayCounts.ACT_ACT_ICMA.yearFraction(JAN_01, JUL_01, test))
      .getMessage shouldBe "The end date of the schedule is required"
    intercept[IllegalArgumentException](DayCounts.ACT_365L.yearFraction(JAN_01, JUL_01, test))
      .getMessage shouldBe "The end date of the schedule period is required"
    intercept[IllegalArgumentException](
      DayCounts.THIRTY_E_360_ISDA.yearFraction(LocalDate.of(2011, 12, 28), LocalDate.of(2012, 2, 29), test))
      .getMessage shouldBe "The end date of the schedule is required"

    // The second precondition, and the one a schedule reaches with '''every''' fact present.
    // `Act/Act ICMA` divides each nominal period by the number of events the schedule's frequency
    // has in a year, so `P5M` describes a schedule it is not defined over. The frequency type
    // reports that as a value; here it is the contract of the call, so it is raised.
    val fiveMonthly = frequencyOf(Frequency.of(Period.ofMonths(5)))
    fiveMonthly.name shouldBe "P5M"
    fiveMonthly.eventsPerYear.left.map(failure => failure.message) shouldBe
      Left(NonIntegralEventsMessage)

    val fiveMonthlyInfo = Info(Some(JAN_01), Some(JAN_01_NEXT), Some(JAN_01_NEXT), false, Some(fiveMonthly))
    fiveMonthlyInfo.startDate shouldBe Some(JAN_01)
    fiveMonthlyInfo.endDate shouldBe Some(JAN_01_NEXT)
    fiveMonthlyInfo.periodEndDate(JAN_01) shouldBe Some(JAN_01_NEXT)
    fiveMonthlyInfo.frequency shouldBe Some(fiveMonthly)
    intercept[IllegalArgumentException](DayCounts.ACT_ACT_ICMA.yearFraction(JAN_01, JUL_01, fiveMonthlyInfo))
      .getMessage shouldBe NonIntegralEventsMessage

    // The control that keeps the assertion above about the frequency and not the fixture: the
    // same shape with a frequency that does divide the year answers, the measured period's 181
    // days over that length taken twice being exactly half a year.
    val sixMonthlyInfo = Info(Some(JAN_01), Some(JAN_01_NEXT), Some(JAN_01_NEXT), false, Some(Frequency.P6M))
    Frequency.P6M.eventsPerYear shouldBe Right(2)
    DayCounts.ACT_ACT_ICMA.yearFraction(JAN_01, JUL_01, sixMonthlyInfo) shouldBe 0.5d

    // The seventeen members that read nothing calculate against it, and so do the two that read
    // only what it always carries or only in a case these dates avoid.
    DayCounts.ACT_365F.yearFraction(JAN_01, JUL_01, test) shouldBe (181d / 365d)
    DayCounts.THIRTY_U_360.yearFraction(JAN_01, JUL_01, test) shouldBe (180d / 360d)
    DayCounts.THIRTY_E_360_ISDA.yearFraction(JAN_01, JUL_01, test) shouldBe (180d / 360d)

    wholeYearInfo.startDate shouldBe Some(JAN_01)
    wholeYearInfo.endDate shouldBe Some(JAN_01_NEXT)
    wholeYearInfo.periodEndDate(JAN_01) shouldBe Some(JAN_01_NEXT)
    // The fixture answers its single period end date for *any* date asked about, which the
    // worked ICMA examples depend on.
    wholeYearInfo.periodEndDate(JUL_01) shouldBe Some(JAN_01_NEXT)
    wholeYearInfo.frequency shouldBe Some(Frequency.P12M)
    wholeYearInfo.isEndOfMonthConvention shouldBe false
    Info(true).isEndOfMonthConvention shouldBe true
    Info(true).startDate shouldBe None
    Info(true).endDate shouldBe None
    Info(true).frequency shouldBe None
    Info(true).periodEndDate(JAN_01) shouldBe None
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // `values` holds the 21 standard members in declaration order, which is not the alphabetical
    // order the `Order` instance imposes, and excludes the calendar-bearing `Bus/252`
    // conventions: there is one per holiday calendar, so they are built on demand.
    DayCount.values.toList shouldBe declarationOrder
    DayCount.values.toList should have size 21
    DayCount.values.toList.distinct should have size 21
    DayCount.values.toList.map(_.name).distinct should have size 21
    DayCount.values.toList.map(_.name) shouldBe standardNames
    DayCount.values.toList.contains(DayCount.ofBus252(StandardHolidayCalendars.BRBD)) shouldBe false

    // Each constant of the holder is the very member, so the two agree by reference.
    forAll(dataConstantIdentifiers) { (identifier: String, dayCount: DayCount) =>
      withClue(s"$identifier: ")(declarationOrder.exists(_ eq dayCount) shouldBe true)
    }
    DayCounts.ONE_ONE should be theSameInstanceAs DayCount.ONE_ONE
    DayCounts.ACT_365F should be theSameInstanceAs DayCount.ACT_365F
    DayCounts.THIRTY_360_ISDA should be theSameInstanceAs DayCount.THIRTY_360_ISDA
    DayCounts.THIRTY_E_365 should be theSameInstanceAs DayCount.THIRTY_E_365

    dataConstantIdentifiers.map { case (_, dayCount) => dayCount }.toList shouldBe declarationOrder

    DayCount.values.toList.foreach { dayCount =>
      withClue(s"${dayCount.name}: ") {
        DayCount.valueOf(dayCount.name) shouldBe Some(dayCount)
        DayCount.parse(dayCount.name) should haveValue(dayCount)
        Show[DayCount].show(dayCount) shouldBe dayCount.name
        dayCount.toString shouldBe dayCount.name
      }
    }

    // The companion publishes one equality-bearing instance - an ordering that is also a hashing
    // - so the equality, the hashing and the ordering summoned here are that one value.
    val all: List[DayCount] = DayCount.values.toList
    for (left <- all; right <- all) {
      val sameValue = left == right
      withClue(s"${left.name} against ${right.name}: ") {
        Eq[DayCount].eqv(left, right) shouldBe sameValue
        Hash[DayCount].eqv(left, right) shouldBe sameValue
        (Order[DayCount].compare(left, right) == 0) shouldBe sameValue
        if (sameValue) {
          Hash[DayCount].hash(left) shouldBe Hash[DayCount].hash(right)
        } else {
          Order[DayCount].compare(left, right) should not be 0
        }
      }
    }

    // The ordering is by name, and a concrete pair states its direction so that the sorted
    // assertion is not self-referential.
    all.sorted(Order[DayCount].toOrdering).map(_.name) shouldBe all.map(_.name).sorted
    Order[DayCount].compare(DayCounts.ACT_360, DayCounts.ACT_364) should be < 0
    Order[DayCount].compare(DayCounts.ACT_364, DayCounts.ACT_360) should be > 0
    Order[DayCount].compare(DayCounts.ONE_ONE, DayCounts.ONE_ONE) shouldBe 0

    DayCounts.ACT_360.equals("Act/360") shouldBe false
    DayCounts.ACT_360.equals(DayCounts.ACT_364) shouldBe false
    DayCounts.ACT_360.equals(DayCounts.ACT_360) shouldBe true
  }

  test("test_serialization") {
    // The document of a standard member is the bare canonical name and never an object: the
    // object form belongs to the `Bus/252` conventions, which `Business252DayCountSpec` owns.
    val encoded: Json = DayCounts.ACT_364.asJson
    encoded.isString shouldBe true
    encoded.isObject shouldBe false
    encoded shouldBe Json.fromString("Act/364")
    encoded.noSpaces shouldBe "\"Act/364\""
    encoded.as[DayCount] shouldBe Right(DayCounts.ACT_364)

    forAll(dataName) { (dayCount: DayCount, name: String) =>
      withClue(s"$name: ") {
        val document: Json = dayCount.asJson
        document shouldBe Json.fromString(name)
        document.isString shouldBe true
        document.isObject shouldBe false
        document.noSpaces shouldBe s""""$name""""
        document.as[DayCount] shouldBe Right(dayCount)
      }
    }

    // An unrecognised name, or a document of the wrong type, is a decoding failure.
    Json.fromString("Rubbish").as[DayCount].isLeft shouldBe true
    Json.fromInt(1).as[DayCount].isLeft shouldBe true
    Json.obj("name" -> Json.fromString("Act/364")).as[DayCount].isLeft shouldBe true

    // The reader is as lenient as `parse`, so a hand-written document is still read.
    Json.fromString("ACT/364").as[DayCount] shouldBe Right(DayCounts.ACT_364)
    Json.fromString("Actual/Actual (ISDA)").as[DayCount] shouldBe Right(DayCounts.ACT_ACT_ISDA)
  }

  test("test_jodaConvert") {
    // `Show` renders a member as text and `DayCount.parse` reads that text back as the member.
    Show[DayCount].show(DayCounts.THIRTY_360_ISDA) shouldBe "30/360 ISDA"
    DayCount.parse(Show[DayCount].show(DayCounts.THIRTY_360_ISDA)) should
      haveValue(DayCounts.THIRTY_360_ISDA)
    Show[DayCount].show(DayCounts.ACT_365F) shouldBe "Act/365F"
    DayCount.parse(Show[DayCount].show(DayCounts.ACT_365F)) should haveValue(DayCounts.ACT_365F)

    forAll(dataName) { (dayCount: DayCount, name: String) =>
      withClue(s"$name: ") {
        Show[DayCount].show(dayCount) shouldBe name
        dayCount.toString shouldBe name
        DayCount.parse(Show[DayCount].show(dayCount)) should haveValue(dayCount)
      }
    }
  }

  //-------------------------------------------------------------------------
  /**
   * Unwraps the outcome of a frequency factory for use as a fixture. Threading it through here
   * rather than forcing it with `getOrElse` and a fabricated fallback keeps a mistake in the
   * fixture visible: a period that is not a frequency fails this spec naming its failures.
   *
   * @param result  the outcome of a frequency factory, expected to hold a frequency
   * @return the frequency the outcome holds
   */
  private def frequencyOf(result: ResultNec[Frequency]): Frequency =
    result match {
      case Right(frequency) => frequency
      case Left(failures) =>
        fail(
          "Fixture frequency could not be built: " +
            failures.toChain.toList.map(failure => failure.message).mkString("; "))
    }
}

/**
 * The fixtures and the nine transcribed data tables of this spec.
 *
 * Every table is a `lazy val`. That is not decoration: the nine hold 618 rows between them, and
 * building all of them in one initialiser would put a single method uncomfortably close to the
 * 64KB limit the JVM places on method bytecode.
 */
private[date] object DayCountSpec extends TableDrivenPropertyChecks {

  val JAN_01: LocalDate = LocalDate.of(2010, 1, 1)

  val JAN_02: LocalDate = LocalDate.of(2010, 1, 2)

  val JUL_01: LocalDate = LocalDate.of(2010, 7, 1)

  val JAN_01_NEXT: LocalDate = LocalDate.of(2011, 1, 1)

  /** The message the date-order precondition reports, asserted rather than paraphrased. */
  val DatesOutOfOrderMessage: String = "Dates must be in time-line order"

  /**
   * The message a frequency with no whole number of events in a year reports, asserted rather
   * than paraphrased. The wording is `Frequency.eventsPerYear`'s own, which `Act/Act ICMA`
   * raises as it stands rather than wrapping.
   */
  val NonIntegralEventsMessage: String = "Unable to calculate events per year: P5M"

  /**
   * The marker [[dataYearFraction]] uses for a row on which no day-of-month adjustment applies.
   * Not-a-number is equal to nothing, including itself, so no row can carry it as a genuine
   * expectation, and [[expectedFraction]] is the single place the branch is taken.
   */
  val SIMPLE_30_360: Double = Double.NaN

  /**
   * The same marker for the day-count table, whose expectations are whole numbers.
   *
   * The marker is zero, and [[expectedDays]] takes its branch when `value == SIMPLE_30_360DAYS`,
   * so a row whose transcribed expectation evaluates to zero is read '''as''' the marker and its
   * expectation is recomputed by [[calc360Days]]. [[dataDays]] holds exactly one such row -
   * `30E/365` from 2012-02-29 to 2016-02-29, written as `calc360Days(2012, 2, 30, 2012, 2, 30)` -
   * and the recomputed 1440 is the answer that rule gives, where the literal zero would not be.
   */
  val SIMPLE_30_360DAYS: Int = 0

  /**
   * The year fraction of a `30/360` rule over two dates whose days-of-month need no adjustment,
   * being [[calc360Days]] over 360. Each date arrives as year, month and day-of-month, the day
   * already adjusted where a row adjusts it.
   */
  def calc360(y1: Int, m1: Int, d1: Int, y2: Int, m2: Int, d2: Int): Double =
    calc360Days(y1, m1, d1, y2, m2, d2).toDouble / 360d

  /**
   * The day count of a `30/360` rule over two dates whose days-of-month need no adjustment,
   * months being thirty days and years three hundred and sixty.
   */
  def calc360Days(y1: Int, m1: Int, d1: Int, y2: Int, m2: Int, d2: Int): Int =
    (y2 - y1) * 360 + (m2 - m1) * 30 + (d2 - d1)

  /**
   * The year fraction a row expects: the value it carries, or [[calc360]] over its two dates
   * where that value is [[SIMPLE_30_360]].
   */
  def expectedFraction(value: Double, y1: Int, m1: Int, d1: Int, y2: Int, m2: Int, d2: Int): Double =
    if (value.isNaN) calc360(y1, m1, d1, y2, m2, d2) else value

  /**
   * The day count a row expects: the value it carries, or [[calc360Days]] over its two dates
   * where that value is [[SIMPLE_30_360DAYS]], which a zero expectation also takes.
   */
  def expectedDays(value: Int, y1: Int, m1: Int, d1: Int, y2: Int, m2: Int, d2: Int): Int =
    if (value == SIMPLE_30_360DAYS) calc360Days(y1, m1, d1, y2, m2, d2) else value

  //-------------------------------------------------------------------------
  /**
   * The schedule information the parameterised cases and the worked examples are driven with.
   * `periodEndDate` answers the one date it holds for '''any''' date asked about, ignoring its
   * argument: the worked `Act/Act ICMA` examples supply a period end that is not the end of the
   * period containing every date they pass, and they depend on getting it back regardless.
   *
   * @param startDate  the start date of the schedule, where one is declared
   * @param endDate  the end date of the schedule, where one is declared
   * @param periodEnd  the period end date answered for every date, where one is declared
   * @param isEndOfMonthConvention  whether the end-of-month convention is in use
   * @param frequency  the frequency of the schedule, where one is declared
   */
  final case class Info(
      override val startDate: Option[LocalDate],
      override val endDate: Option[LocalDate],
      periodEnd: Option[LocalDate],
      override val isEndOfMonthConvention: Boolean,
      override val frequency: Option[Frequency])
      extends DayCount.ScheduleInfo {

    override def periodEndDate(date: LocalDate): Option[LocalDate] = periodEnd
  }

  object Info {

    /**
     * Obtains schedule information carrying the end-of-month flag and no dates or frequency.
     *
     * @param eom  whether the end-of-month convention is in use
     * @return the schedule information
     */
    def apply(eom: Boolean): Info = Info(None, None, None, eom, None)
  }

  /**
   * The schedule of one annual period from [[JAN_01]] to [[JAN_01_NEXT]], with the end-of-month
   * convention not in use. It supplies every fact any member reads, which is what lets the
   * sanity tests drive all 21 members through the three-argument overload.
   */
  lazy val wholeYearInfo: Info =
    Info(Some(JAN_01), Some(JAN_01_NEXT), Some(JAN_01_NEXT), false, Some(Frequency.P12M))

  //-------------------------------------------------------------------------
  /**
   * Each of the 21 standard members with the name it renders as, in declaration order.
   * [[declarationOrder]] and [[standardNames]] are read from this table, so the order and the
   * names `DayCount.values` is asserted against come from here and not from the type under test.
   */
  lazy val dataName: TableFor2[DayCount, String] = Table(
    ("dayCount", "name"),
    (DayCounts.ONE_ONE, "1/1"),
    (DayCounts.ACT_ACT_ISDA, "Act/Act ISDA"),
    (DayCounts.ACT_ACT_ICMA, "Act/Act ICMA"),
    (DayCounts.ACT_ACT_AFB, "Act/Act AFB"),
    (DayCounts.ACT_ACT_YEAR, "Act/Act Year"),
    (DayCounts.ACT_365_ACTUAL, "Act/365 Actual"),
    (DayCounts.ACT_365L, "Act/365L"),
    (DayCounts.ACT_360, "Act/360"),
    (DayCounts.ACT_364, "Act/364"),
    (DayCounts.ACT_365F, "Act/365F"),
    (DayCounts.ACT_365_25, "Act/365.25"),
    (DayCounts.NL_360, "NL/360"),
    (DayCounts.NL_365, "NL/365"),
    (DayCounts.THIRTY_360_ISDA, "30/360 ISDA"),
    (DayCounts.THIRTY_U_360, "30U/360"),
    (DayCounts.THIRTY_U_360_EOM, "30U/360 EOM"),
    (DayCounts.THIRTY_360_PSA, "30/360 PSA"),
    (DayCounts.THIRTY_E_360_ISDA, "30E/360 ISDA"),
    (DayCounts.THIRTY_E_360, "30E/360"),
    (DayCounts.THIRTY_EPLUS_360, "30E+/360"),
    (DayCounts.THIRTY_E_365, "30E/365")
  )

  /** The 21 standard members in the declaration order [[dataName]] lists them in. */
  lazy val declarationOrder: List[DayCount] = dataName.map { case (dayCount, _) => dayCount }.toList

  /** The 21 canonical names in that same order. */
  lazy val standardNames: List[String] = dataName.map { case (_, name) => name }.toList

  /**
   * The 21 standard members as a one-column table, read from [[declarationOrder]]. The
   * calendar-bearing `Bus/252` conventions are not among them, so they take no part in the tests
   * this table drives.
   */
  lazy val dataTypes: TableFor1[DayCount] = Table("dayCount", declarationOrder: _*)

  /**
   * The identifiers the `DayCounts` constants holder publishes, paired with the member each
   * names, in declaration order. `coverage` is where the list is asserted to be exactly the
   * members; each identifier also resolves through the lenient chain, which holds a row for it.
   */
  lazy val dataConstantIdentifiers: TableFor2[String, DayCount] = Table(
    ("identifier", "dayCount"),
    ("ONE_ONE", DayCounts.ONE_ONE),
    ("ACT_ACT_ISDA", DayCounts.ACT_ACT_ISDA),
    ("ACT_ACT_ICMA", DayCounts.ACT_ACT_ICMA),
    ("ACT_ACT_AFB", DayCounts.ACT_ACT_AFB),
    ("ACT_ACT_YEAR", DayCounts.ACT_ACT_YEAR),
    ("ACT_365_ACTUAL", DayCounts.ACT_365_ACTUAL),
    ("ACT_365L", DayCounts.ACT_365L),
    ("ACT_360", DayCounts.ACT_360),
    ("ACT_364", DayCounts.ACT_364),
    ("ACT_365F", DayCounts.ACT_365F),
    ("ACT_365_25", DayCounts.ACT_365_25),
    ("NL_360", DayCounts.NL_360),
    ("NL_365", DayCounts.NL_365),
    ("THIRTY_360_ISDA", DayCounts.THIRTY_360_ISDA),
    ("THIRTY_U_360", DayCounts.THIRTY_U_360),
    ("THIRTY_U_360_EOM", DayCounts.THIRTY_U_360_EOM),
    ("THIRTY_360_PSA", DayCounts.THIRTY_360_PSA),
    ("THIRTY_E_360_ISDA", DayCounts.THIRTY_E_360_ISDA),
    ("THIRTY_E_360", DayCounts.THIRTY_E_360),
    ("THIRTY_EPLUS_360", DayCounts.THIRTY_EPLUS_360),
    ("THIRTY_E_365", DayCounts.THIRTY_E_365)
  )

  /**
   * The replacement of each of the 67 lenient rewrite rules, in the order they are applied,
   * transcribed from the published configuration they come from rather than from the table under
   * test. The order is the data: a rule whose expression matches the whole of the current text
   * replaces that text, so a later rule sees what an earlier one produced.
   */
  lazy val lenientReplacements: List[String] =
    List(
      "Act/Act$1",
      "Act/$1",
      "Act/Act$1",
      "Act/$1",
      "Act/Act$1",
      "Act/$1",
      "$1$2",
      "$1 $2$3",
      "$1 ICMA",
      "Act/Act ISDA",
      "Act/Act ISDA",
      "Act/Act ISDA",
      "Act/Act ICMA",
      "Act/Act ICMA",
      "Act/Act AFB",
      "Act/Act Year",
      "Act/365 Actual",
      "Act/365 Actual",
      "Act/365L",
      "Act/365L",
      "Act/360",
      "Act/365F",
      "Act/365F",
      "Act/365F",
      "Act/365F",
      "NL/360",
      "NL/360",
      "NL/365",
      "NL/365",
      "NL/365",
      "1/1",
      "Act/Act ISDA",
      "Act/Act ICMA",
      "Act/Act AFB",
      "Act/Act Year",
      "Act/365 Actual",
      "Act/365L",
      "Act/360",
      "Act/364",
      "Act/365F",
      "Act/365.25",
      "NL/360",
      "NL/365",
      "30/360 ISDA",
      "30U/360",
      "30U/360 EOM",
      "30/360 PSA",
      "30E/360 ISDA",
      "30E/360",
      "30E+/360",
      "30E/365",
      "30/360 ISDA",
      "30E/360",
      "30E/360",
      "30E/360",
      "30E/360",
      "30E/360 ISDA",
      "30E/360 ISDA",
      "30U/360",
      "30U/360",
      "30U/360",
      "30U/360",
      "30U/360",
      "30U/360",
      "30U/360",
      "30E/365",
      "Bus/252 BRBD"
    )

  //-------------------------------------------------------------------------
  /**
   * All 201 year-fraction rows, in order: a member, the two dates as year, month and day, and
   * the year fraction expected - or [[SIMPLE_30_360]] where the `30/360` rule of that member
   * needs no day-of-month adjustment. The expectations are written as the arithmetic that
   * produces them, `4d / 365d + 58d / 366d` rather than a decimal, which is the only form that
   * states the rule being asserted.
   */
  lazy val dataYearFraction: TableFor8[DayCount, Int, Int, Int, Int, Int, Int, Double] = Table(
    ("dayCount", "y1", "m1", "d1", "y2", "m2", "d2", "value"),
    (DayCounts.ONE_ONE, 2011, 12, 28, 2012, 2, 28, 1d),
    (DayCounts.ONE_ONE, 2011, 12, 28, 2012, 2, 29, 1d),
    (DayCounts.ONE_ONE, 2011, 12, 28, 2012, 3, 1, 1d),
    (DayCounts.ONE_ONE, 2011, 12, 28, 2016, 2, 28, 1d),
    (DayCounts.ONE_ONE, 2011, 12, 28, 2016, 2, 29, 1d),
    (DayCounts.ONE_ONE, 2011, 12, 28, 2016, 3, 1, 1d),
    (DayCounts.ONE_ONE, 2012, 2, 29, 2012, 3, 29, 1d),
    (DayCounts.ONE_ONE, 2012, 2, 29, 2012, 3, 28, 1d),
    (DayCounts.ONE_ONE, 2012, 3, 1, 2012, 3, 28, 1d),

    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2012, 2, 28, (4d / 365d + 58d / 366d)),
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2012, 2, 29, (4d / 365d + 59d / 366d)),
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2012, 3, 1, (4d / 365d + 60d / 366d)),
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2016, 2, 28, (4d / 365d + 58d / 366d + 4d)),
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2016, 2, 29, (4d / 365d + 59d / 366d + 4d)),
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2016, 3, 1, (4d / 365d + 60d / 366d + 4d)),
    (DayCounts.ACT_ACT_ISDA, 2012, 2, 29, 2012, 3, 29, 29d / 366d),
    (DayCounts.ACT_ACT_ISDA, 2012, 2, 29, 2012, 3, 28, 28d / 366d),
    (DayCounts.ACT_ACT_ISDA, 2012, 3, 1, 2012, 3, 28, 27d / 366d),

    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2012, 2, 28, (62d / 365d)),
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2012, 2, 29, (63d / 365d)),
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2012, 3, 1, (64d / 366d)),
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2016, 2, 28, (62d / 365d) + 4d),
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2016, 2, 29, (63d / 365d) + 4d),
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2016, 3, 1, (64d / 366d) + 4d),
    (DayCounts.ACT_ACT_AFB, 2012, 2, 28, 2012, 3, 28, 29d / 366d),
    (DayCounts.ACT_ACT_AFB, 2012, 2, 29, 2012, 3, 28, 28d / 366d),
    (DayCounts.ACT_ACT_AFB, 2012, 3, 1, 2012, 3, 28, 27d / 365d),

    (DayCounts.ACT_ACT_YEAR, 2011, 12, 28, 2012, 2, 28, (62d / 366d)),
    (DayCounts.ACT_ACT_YEAR, 2011, 12, 28, 2012, 2, 29, (63d / 366d)),
    (DayCounts.ACT_ACT_YEAR, 2011, 12, 28, 2012, 3, 1, (64d / 366d)),
    (DayCounts.ACT_ACT_YEAR, 2011, 12, 28, 2016, 2, 28, (62d / 366d) + 4d),
    (DayCounts.ACT_ACT_YEAR, 2011, 12, 28, 2016, 2, 29, (63d / 366d) + 4d),
    (DayCounts.ACT_ACT_YEAR, 2011, 12, 28, 2016, 3, 1, (64d / 366d) + 4d),
    (DayCounts.ACT_ACT_YEAR, 2012, 2, 28, 2012, 3, 28, 29d / 366d),
    (DayCounts.ACT_ACT_YEAR, 2012, 2, 29, 2012, 3, 28, 28d / 365d),
    (DayCounts.ACT_ACT_YEAR, 2012, 3, 1, 2012, 3, 28, 27d / 365d),

    (DayCounts.ACT_ACT_YEAR, 2011, 2, 28, 2011, 3, 2, (2d / 365d)),
    (DayCounts.ACT_ACT_YEAR, 2011, 3, 1, 2011, 3, 2, (1d / 366d)),

    (DayCounts.ACT_ACT_YEAR, 2012, 2, 28, 2016, 3, 2, (3d / 366d) + 4d),
    (DayCounts.ACT_ACT_YEAR, 2012, 2, 29, 2016, 3, 2, (2d / 365d) + 4d),

    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2012, 2, 28, (62d / 365d)),
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2012, 2, 29, (63d / 366d)),
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2012, 3, 1, (64d / 366d)),
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2016, 2, 28, ((62d + 366d + 365d + 365d + 365d) / 366d)),
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2016, 2, 29, ((63d + 366d + 365d + 365d + 365d) / 366d)),
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2016, 3, 1, ((64d + 366d + 365d + 365d + 365d) / 366d)),
    (DayCounts.ACT_365_ACTUAL, 2012, 2, 28, 2012, 3, 28, 29d / 366d),
    (DayCounts.ACT_365_ACTUAL, 2012, 2, 29, 2012, 3, 28, 28d / 365d),
    (DayCounts.ACT_365_ACTUAL, 2012, 3, 1, 2012, 3, 28, 27d / 365d),

    (DayCounts.ACT_360, 2011, 12, 28, 2012, 2, 28, (62d / 360d)),
    (DayCounts.ACT_360, 2011, 12, 28, 2012, 2, 29, (63d / 360d)),
    (DayCounts.ACT_360, 2011, 12, 28, 2012, 3, 1, (64d / 360d)),
    (DayCounts.ACT_360, 2011, 12, 28, 2016, 2, 28, ((62d + 366d + 365d + 365d + 365d) / 360d)),
    (DayCounts.ACT_360, 2011, 12, 28, 2016, 2, 29, ((63d + 366d + 365d + 365d + 365d) / 360d)),
    (DayCounts.ACT_360, 2011, 12, 28, 2016, 3, 1, ((64d + 366d + 365d + 365d + 365d) / 360d)),
    (DayCounts.ACT_360, 2012, 2, 28, 2012, 3, 28, 29d / 360d),
    (DayCounts.ACT_360, 2012, 2, 29, 2012, 3, 28, 28d / 360d),
    (DayCounts.ACT_360, 2012, 3, 1, 2012, 3, 28, 27d / 360d),

    (DayCounts.ACT_364, 2011, 12, 28, 2012, 2, 28, (62d / 364d)),
    (DayCounts.ACT_364, 2011, 12, 28, 2012, 2, 29, (63d / 364d)),
    (DayCounts.ACT_364, 2011, 12, 28, 2012, 3, 1, (64d / 364d)),
    (DayCounts.ACT_364, 2011, 12, 28, 2016, 2, 28, ((62d + 366d + 365d + 365d + 365d) / 364d)),
    (DayCounts.ACT_364, 2011, 12, 28, 2016, 2, 29, ((63d + 366d + 365d + 365d + 365d) / 364d)),
    (DayCounts.ACT_364, 2011, 12, 28, 2016, 3, 1, ((64d + 366d + 365d + 365d + 365d) / 364d)),
    (DayCounts.ACT_364, 2012, 2, 28, 2012, 3, 28, 29d / 364d),
    (DayCounts.ACT_364, 2012, 2, 29, 2012, 3, 28, 28d / 364d),
    (DayCounts.ACT_364, 2012, 3, 1, 2012, 3, 28, 27d / 364d),

    (DayCounts.ACT_365F, 2011, 12, 28, 2012, 2, 28, (62d / 365d)),
    (DayCounts.ACT_365F, 2011, 12, 28, 2012, 2, 29, (63d / 365d)),
    (DayCounts.ACT_365F, 2011, 12, 28, 2012, 3, 1, (64d / 365d)),
    (DayCounts.ACT_365F, 2011, 12, 28, 2016, 2, 28, ((62d + 366d + 365d + 365d + 365d) / 365d)),
    (DayCounts.ACT_365F, 2011, 12, 28, 2016, 2, 29, ((63d + 366d + 365d + 365d + 365d) / 365d)),
    (DayCounts.ACT_365F, 2011, 12, 28, 2016, 3, 1, ((64d + 366d + 365d + 365d + 365d) / 365d)),
    (DayCounts.ACT_365F, 2012, 2, 28, 2012, 3, 28, 29d / 365d),
    (DayCounts.ACT_365F, 2012, 2, 29, 2012, 3, 28, 28d / 365d),
    (DayCounts.ACT_365F, 2012, 3, 1, 2012, 3, 28, 27d / 365d),

    (DayCounts.ACT_365_25, 2011, 12, 28, 2012, 2, 28, (62d / 365.25d)),
    (DayCounts.ACT_365_25, 2011, 12, 28, 2012, 2, 29, (63d / 365.25d)),
    (DayCounts.ACT_365_25, 2011, 12, 28, 2012, 3, 1, (64d / 365.25d)),
    (DayCounts.ACT_365_25, 2011, 12, 28, 2016, 2, 28, ((62d + 366d + 365d + 365d + 365d) / 365.25d)),
    (DayCounts.ACT_365_25, 2011, 12, 28, 2016, 2, 29, ((63d + 366d + 365d + 365d + 365d) / 365.25d)),
    (DayCounts.ACT_365_25, 2011, 12, 28, 2016, 3, 1, ((64d + 366d + 365d + 365d + 365d) / 365.25d)),
    (DayCounts.ACT_365_25, 2012, 2, 28, 2012, 3, 28, 29d / 365.25d),
    (DayCounts.ACT_365_25, 2012, 2, 29, 2012, 3, 28, 28d / 365.25d),
    (DayCounts.ACT_365_25, 2012, 3, 1, 2012, 3, 28, 27d / 365.25d),

    (DayCounts.NL_360, 2011, 12, 28, 2012, 2, 28, (62d / 360d)),
    (DayCounts.NL_360, 2011, 12, 28, 2012, 2, 29, (62d / 360d)),
    (DayCounts.NL_360, 2011, 12, 28, 2012, 3, 1, (63d / 360d)),
    (DayCounts.NL_360, 2011, 12, 28, 2016, 2, 28, ((62d + 365d + 365d + 365d + 365d) / 360d)),
    (DayCounts.NL_360, 2011, 12, 28, 2016, 2, 29, ((62d + 365d + 365d + 365d + 365d) / 360d)),
    (DayCounts.NL_360, 2011, 12, 28, 2016, 3, 1, ((63d + 365d + 365d + 365d + 365d) / 360d)),
    (DayCounts.NL_360, 2012, 2, 28, 2012, 3, 28, 28d / 360d),
    (DayCounts.NL_360, 2012, 2, 29, 2012, 3, 28, 28d / 360d),
    (DayCounts.NL_360, 2012, 3, 1, 2012, 3, 28, 27d / 360d),
    (DayCounts.NL_360, 2011, 12, 1, 2012, 12, 1, 365d / 360d),

    (DayCounts.NL_365, 2011, 12, 28, 2012, 2, 28, (62d / 365d)),
    (DayCounts.NL_365, 2011, 12, 28, 2012, 2, 29, (62d / 365d)),
    (DayCounts.NL_365, 2011, 12, 28, 2012, 3, 1, (63d / 365d)),
    (DayCounts.NL_365, 2011, 12, 28, 2016, 2, 28, ((62d + 365d + 365d + 365d + 365d) / 365d)),
    (DayCounts.NL_365, 2011, 12, 28, 2016, 2, 29, ((62d + 365d + 365d + 365d + 365d) / 365d)),
    (DayCounts.NL_365, 2011, 12, 28, 2016, 3, 1, ((63d + 365d + 365d + 365d + 365d) / 365d)),
    (DayCounts.NL_365, 2012, 2, 28, 2012, 3, 28, 28d / 365d),
    (DayCounts.NL_365, 2012, 2, 29, 2012, 3, 28, 28d / 365d),
    (DayCounts.NL_365, 2012, 3, 1, 2012, 3, 28, 27d / 365d),
    (DayCounts.NL_365, 2011, 12, 1, 2012, 12, 1, 365d / 365d),

    (DayCounts.THIRTY_360_ISDA, 2011, 12, 28, 2012, 2, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_360_ISDA, 2011, 12, 28, 2012, 2, 29, SIMPLE_30_360),
    (DayCounts.THIRTY_360_ISDA, 2011, 12, 28, 2012, 3, 1, SIMPLE_30_360),
    (DayCounts.THIRTY_360_ISDA, 2011, 12, 28, 2016, 2, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_360_ISDA, 2011, 12, 28, 2016, 2, 29, SIMPLE_30_360),
    (DayCounts.THIRTY_360_ISDA, 2011, 12, 28, 2016, 3, 1, SIMPLE_30_360),

    (DayCounts.THIRTY_360_ISDA, 2012, 2, 28, 2012, 3, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_360_ISDA, 2012, 2, 29, 2012, 3, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_360_ISDA, 2011, 2, 28, 2012, 2, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_360_ISDA, 2011, 2, 28, 2012, 2, 29, SIMPLE_30_360),
    (DayCounts.THIRTY_360_ISDA, 2012, 2, 29, 2016, 2, 29, SIMPLE_30_360),

    (DayCounts.THIRTY_360_ISDA, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_360_ISDA, 2012, 5, 30, 2013, 8, 29, SIMPLE_30_360),
    (DayCounts.THIRTY_360_ISDA, 2012, 5, 29, 2013, 8, 30, SIMPLE_30_360),
    (DayCounts.THIRTY_360_ISDA, 2012, 5, 30, 2013, 8, 30, SIMPLE_30_360),
    (DayCounts.THIRTY_360_ISDA, 2012, 5, 29, 2013, 8, 31, SIMPLE_30_360),
    (DayCounts.THIRTY_360_ISDA, 2012, 5, 30, 2013, 8, 31, calc360(2012, 5, 30, 2013, 8, 30)),
    (DayCounts.THIRTY_360_ISDA, 2012, 5, 31, 2013, 8, 30, calc360(2012, 5, 30, 2013, 8, 30)),
    (DayCounts.THIRTY_360_ISDA, 2012, 5, 31, 2013, 8, 31, calc360(2012, 5, 30, 2013, 8, 30)),

    (DayCounts.THIRTY_360_PSA, 2011, 12, 28, 2012, 2, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_360_PSA, 2011, 12, 28, 2012, 2, 29, SIMPLE_30_360),
    (DayCounts.THIRTY_360_PSA, 2011, 12, 28, 2012, 3, 1, SIMPLE_30_360),
    (DayCounts.THIRTY_360_PSA, 2011, 12, 28, 2016, 2, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_360_PSA, 2011, 12, 28, 2016, 2, 29, SIMPLE_30_360),
    (DayCounts.THIRTY_360_PSA, 2011, 12, 28, 2016, 3, 1, SIMPLE_30_360),

    (DayCounts.THIRTY_360_PSA, 2012, 2, 28, 2012, 3, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_360_PSA, 2012, 2, 29, 2012, 3, 28, calc360(2012, 2, 30, 2012, 3, 28)),
    (DayCounts.THIRTY_360_PSA, 2011, 2, 28, 2012, 2, 28, calc360(2011, 2, 30, 2012, 2, 28)),
    (DayCounts.THIRTY_360_PSA, 2011, 2, 28, 2012, 2, 29, calc360(2011, 2, 30, 2012, 2, 29)),
    (DayCounts.THIRTY_360_PSA, 2012, 2, 29, 2016, 2, 29, calc360(2012, 2, 30, 2016, 2, 29)),

    (DayCounts.THIRTY_360_PSA, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_360_PSA, 2012, 5, 30, 2013, 8, 29, SIMPLE_30_360),
    (DayCounts.THIRTY_360_PSA, 2012, 5, 29, 2013, 8, 30, SIMPLE_30_360),
    (DayCounts.THIRTY_360_PSA, 2012, 5, 30, 2013, 8, 30, SIMPLE_30_360),
    (DayCounts.THIRTY_360_PSA, 2012, 5, 29, 2013, 8, 31, SIMPLE_30_360),
    (DayCounts.THIRTY_360_PSA, 2012, 5, 30, 2013, 8, 31, calc360(2012, 5, 30, 2013, 8, 30)),
    (DayCounts.THIRTY_360_PSA, 2012, 5, 31, 2013, 8, 30, calc360(2012, 5, 30, 2013, 8, 30)),
    (DayCounts.THIRTY_360_PSA, 2012, 5, 31, 2013, 8, 31, calc360(2012, 5, 30, 2013, 8, 30)),

    (DayCounts.THIRTY_E_360, 2011, 12, 28, 2012, 2, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_E_360, 2011, 12, 28, 2012, 2, 29, SIMPLE_30_360),
    (DayCounts.THIRTY_E_360, 2011, 12, 28, 2012, 3, 1, SIMPLE_30_360),
    (DayCounts.THIRTY_E_360, 2011, 12, 28, 2016, 2, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_E_360, 2011, 12, 28, 2016, 2, 29, SIMPLE_30_360),
    (DayCounts.THIRTY_E_360, 2011, 12, 28, 2016, 3, 1, SIMPLE_30_360),

    (DayCounts.THIRTY_E_360, 2012, 2, 28, 2012, 3, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_E_360, 2012, 2, 29, 2012, 3, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_E_360, 2011, 2, 28, 2012, 2, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_E_360, 2011, 2, 28, 2012, 2, 29, SIMPLE_30_360),
    (DayCounts.THIRTY_E_360, 2012, 2, 29, 2016, 2, 29, SIMPLE_30_360),

    (DayCounts.THIRTY_E_360, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_E_360, 2012, 5, 30, 2013, 8, 29, SIMPLE_30_360),
    (DayCounts.THIRTY_E_360, 2012, 5, 29, 2013, 8, 30, SIMPLE_30_360),
    (DayCounts.THIRTY_E_360, 2012, 5, 30, 2013, 8, 30, SIMPLE_30_360),
    (DayCounts.THIRTY_E_360, 2012, 5, 29, 2013, 8, 31, calc360(2012, 5, 29, 2013, 8, 30)),
    (DayCounts.THIRTY_E_360, 2012, 5, 30, 2013, 8, 31, calc360(2012, 5, 30, 2013, 8, 30)),
    (DayCounts.THIRTY_E_360, 2012, 5, 31, 2013, 8, 30, calc360(2012, 5, 30, 2013, 8, 30)),
    (DayCounts.THIRTY_E_360, 2012, 5, 31, 2013, 8, 31, calc360(2012, 5, 30, 2013, 8, 30)),

    (DayCounts.THIRTY_EPLUS_360, 2011, 12, 28, 2012, 2, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_EPLUS_360, 2011, 12, 28, 2012, 2, 29, SIMPLE_30_360),
    (DayCounts.THIRTY_EPLUS_360, 2011, 12, 28, 2012, 3, 1, SIMPLE_30_360),
    (DayCounts.THIRTY_EPLUS_360, 2011, 12, 28, 2016, 2, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_EPLUS_360, 2011, 12, 28, 2016, 2, 29, SIMPLE_30_360),
    (DayCounts.THIRTY_EPLUS_360, 2011, 12, 28, 2016, 3, 1, SIMPLE_30_360),

    (DayCounts.THIRTY_EPLUS_360, 2012, 2, 28, 2012, 3, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_EPLUS_360, 2012, 2, 29, 2012, 3, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_EPLUS_360, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_EPLUS_360, 2011, 2, 28, 2012, 2, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_EPLUS_360, 2011, 2, 28, 2012, 2, 29, SIMPLE_30_360),
    (DayCounts.THIRTY_EPLUS_360, 2012, 2, 29, 2016, 2, 29, SIMPLE_30_360),

    (DayCounts.THIRTY_EPLUS_360, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360),
    (DayCounts.THIRTY_EPLUS_360, 2012, 5, 30, 2013, 8, 29, SIMPLE_30_360),
    (DayCounts.THIRTY_EPLUS_360, 2012, 5, 29, 2013, 8, 30, SIMPLE_30_360),
    (DayCounts.THIRTY_EPLUS_360, 2012, 5, 30, 2013, 8, 30, SIMPLE_30_360),
    (DayCounts.THIRTY_EPLUS_360, 2012, 5, 29, 2013, 8, 31, calc360(2012, 5, 29, 2013, 9, 1)),
    (DayCounts.THIRTY_EPLUS_360, 2012, 5, 30, 2013, 8, 31, calc360(2012, 5, 30, 2013, 9, 1)),
    (DayCounts.THIRTY_EPLUS_360, 2012, 5, 31, 2013, 8, 30, calc360(2012, 5, 30, 2013, 8, 30)),
    (DayCounts.THIRTY_EPLUS_360, 2012, 5, 31, 2013, 8, 31, calc360(2012, 5, 30, 2013, 9, 1)),

    (DayCounts.THIRTY_E_365, 2011, 12, 28, 2012, 2, 28, calc360Days(2011, 12, 28, 2012, 2, 28).toDouble / 365d),
    (DayCounts.THIRTY_E_365, 2011, 12, 28, 2012, 2, 29, calc360Days(2011, 12, 28, 2012, 2, 30).toDouble / 365d),
    (DayCounts.THIRTY_E_365, 2011, 12, 28, 2012, 3, 1, calc360Days(2011, 12, 28, 2012, 3, 1).toDouble / 365d),
    (DayCounts.THIRTY_E_365, 2011, 12, 28, 2016, 2, 28, calc360Days(2011, 12, 28, 2016, 2, 28).toDouble / 365d),
    (DayCounts.THIRTY_E_365, 2011, 12, 28, 2016, 2, 29, calc360Days(2011, 12, 28, 2016, 2, 30).toDouble / 365d),
    (DayCounts.THIRTY_E_365, 2011, 12, 28, 2016, 3, 1, calc360Days(2011, 12, 28, 2016, 3, 1).toDouble / 365d),

    (DayCounts.THIRTY_E_365, 2012, 2, 28, 2012, 3, 28, calc360Days(2012, 2, 28, 2012, 3, 28).toDouble / 365d),
    (DayCounts.THIRTY_E_365, 2012, 2, 29, 2012, 3, 28, calc360Days(2012, 2, 30, 2012, 3, 28).toDouble / 365d),
    (DayCounts.THIRTY_E_365, 2011, 2, 28, 2012, 2, 28, calc360Days(2011, 2, 30, 2012, 2, 28).toDouble / 365d),
    (DayCounts.THIRTY_E_365, 2011, 2, 28, 2012, 2, 29, calc360Days(2011, 2, 30, 2012, 2, 30).toDouble / 365d),
    (DayCounts.THIRTY_E_365, 2012, 2, 29, 2016, 2, 29, calc360Days(2012, 2, 30, 2016, 2, 30).toDouble / 365d),

    (DayCounts.THIRTY_E_365, 2012, 3, 1, 2012, 3, 28, calc360Days(2012, 3, 1, 2012, 3, 28).toDouble / 365d),
    (DayCounts.THIRTY_E_365, 2012, 5, 30, 2013, 8, 29, calc360Days(2012, 5, 30, 2013, 8, 29).toDouble / 365d),
    (DayCounts.THIRTY_E_365, 2012, 5, 29, 2013, 8, 30, calc360Days(2012, 5, 29, 2013, 8, 30).toDouble / 365d),
    (DayCounts.THIRTY_E_365, 2012, 5, 30, 2013, 8, 30, calc360Days(2012, 5, 30, 2013, 8, 30).toDouble / 365d),
    (DayCounts.THIRTY_E_365, 2012, 5, 29, 2013, 8, 31, calc360Days(2012, 5, 29, 2013, 8, 30).toDouble / 365d),
    (DayCounts.THIRTY_E_365, 2012, 5, 30, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 8, 30).toDouble / 365d),
    (DayCounts.THIRTY_E_365, 2012, 5, 31, 2013, 8, 30, calc360Days(2012, 5, 30, 2013, 8, 30).toDouble / 365d),
    (DayCounts.THIRTY_E_365, 2012, 5, 31, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 8, 30).toDouble / 365d)
  )

  /**
   * All 185 day-count rows, in order: as above, with the day count expected in place of the year
   * fraction and [[SIMPLE_30_360DAYS]] as the marker.
   */
  lazy val dataDays: TableFor8[DayCount, Int, Int, Int, Int, Int, Int, Int] = Table(
    ("dayCount", "y1", "m1", "d1", "y2", "m2", "d2", "value"),
    (DayCounts.ONE_ONE, 2011, 12, 28, 2012, 2, 28, 1),
    (DayCounts.ONE_ONE, 2011, 12, 28, 2012, 2, 29, 1),
    (DayCounts.ONE_ONE, 2011, 12, 28, 2012, 3, 1, 1),
    (DayCounts.ONE_ONE, 2011, 12, 28, 2016, 2, 28, 1),
    (DayCounts.ONE_ONE, 2011, 12, 28, 2016, 2, 29, 1),
    (DayCounts.ONE_ONE, 2011, 12, 28, 2016, 3, 1, 1),
    (DayCounts.ONE_ONE, 2012, 2, 29, 2012, 3, 29, 1),
    (DayCounts.ONE_ONE, 2012, 2, 29, 2012, 3, 28, 1),
    (DayCounts.ONE_ONE, 2012, 3, 1, 2012, 3, 28, 1),

    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2012, 2, 28, 62),
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2012, 2, 29, 63),
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2012, 3, 1, 64),
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2016, 2, 28, 1523),
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2016, 2, 29, 1524),
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2016, 3, 1, 1525),

    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2012, 2, 28, 62),
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2012, 2, 29, 63),
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2012, 3, 1, 64),
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2016, 2, 28, 1523),
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2016, 2, 29, 1524),
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2016, 3, 1, 1525),

    (DayCounts.ACT_ACT_YEAR, 2011, 12, 28, 2012, 2, 28, 62),
    (DayCounts.ACT_ACT_YEAR, 2011, 12, 28, 2012, 2, 29, 63),
    (DayCounts.ACT_ACT_YEAR, 2011, 12, 28, 2012, 3, 1, 64),
    (DayCounts.ACT_ACT_YEAR, 2011, 12, 28, 2016, 2, 28, 1523),
    (DayCounts.ACT_ACT_YEAR, 2011, 12, 28, 2016, 2, 29, 1524),
    (DayCounts.ACT_ACT_YEAR, 2011, 12, 28, 2016, 3, 1, 1525),

    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2012, 2, 28, 62),
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2012, 2, 29, 63),
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2012, 3, 1, 64),
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2016, 2, 28, 62 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2016, 2, 29, 63 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2016, 3, 1, 64 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_365_ACTUAL, 2012, 2, 28, 2012, 3, 28, 29),
    (DayCounts.ACT_365_ACTUAL, 2012, 2, 29, 2012, 3, 28, 28),
    (DayCounts.ACT_365_ACTUAL, 2012, 3, 1, 2012, 3, 28, 27),

    (DayCounts.ACT_360, 2011, 12, 28, 2012, 2, 28, 62),
    (DayCounts.ACT_360, 2011, 12, 28, 2012, 2, 29, 63),
    (DayCounts.ACT_360, 2011, 12, 28, 2012, 3, 1, 64),
    (DayCounts.ACT_360, 2011, 12, 28, 2016, 2, 28, 62 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_360, 2011, 12, 28, 2016, 2, 29, 63 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_360, 2011, 12, 28, 2016, 3, 1, 64 + 366 + 365 + 365 + 365),

    (DayCounts.ACT_364, 2011, 12, 28, 2012, 2, 28, 62),
    (DayCounts.ACT_364, 2011, 12, 28, 2012, 2, 29, 63),
    (DayCounts.ACT_364, 2011, 12, 28, 2012, 3, 1, 64),
    (DayCounts.ACT_364, 2011, 12, 28, 2016, 2, 28, 62 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_364, 2011, 12, 28, 2016, 2, 29, 63 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_364, 2011, 12, 28, 2016, 3, 1, 64 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_364, 2012, 2, 28, 2012, 3, 28, 29),
    (DayCounts.ACT_364, 2012, 2, 29, 2012, 3, 28, 28),
    (DayCounts.ACT_364, 2012, 3, 1, 2012, 3, 28, 27),

    (DayCounts.ACT_365F, 2011, 12, 28, 2012, 2, 28, 62),
    (DayCounts.ACT_365F, 2011, 12, 28, 2012, 2, 29, 63),
    (DayCounts.ACT_365F, 2011, 12, 28, 2012, 3, 1, 64),
    (DayCounts.ACT_365F, 2011, 12, 28, 2016, 2, 28, 62 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_365F, 2011, 12, 28, 2016, 2, 29, 63 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_365F, 2011, 12, 28, 2016, 3, 1, 64 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_365F, 2012, 2, 28, 2012, 3, 28, 29),
    (DayCounts.ACT_365F, 2012, 2, 29, 2012, 3, 28, 28),
    (DayCounts.ACT_365F, 2012, 3, 1, 2012, 3, 28, 27),

    (DayCounts.ACT_365_25, 2011, 12, 28, 2012, 2, 28, 62),
    (DayCounts.ACT_365_25, 2011, 12, 28, 2012, 2, 29, 63),
    (DayCounts.ACT_365_25, 2011, 12, 28, 2012, 3, 1, 64),
    (DayCounts.ACT_365_25, 2011, 12, 28, 2016, 2, 28, 62 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_365_25, 2011, 12, 28, 2016, 2, 29, 63 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_365_25, 2011, 12, 28, 2016, 3, 1, 64 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_365_25, 2012, 2, 28, 2012, 3, 28, 29),
    (DayCounts.ACT_365_25, 2012, 2, 29, 2012, 3, 28, 28),
    (DayCounts.ACT_365_25, 2012, 3, 1, 2012, 3, 28, 27),

    (DayCounts.NL_360, 2011, 12, 28, 2012, 2, 28, 62),
    (DayCounts.NL_360, 2011, 12, 28, 2012, 2, 29, 62),
    (DayCounts.NL_360, 2011, 12, 28, 2012, 3, 1, 63),
    (DayCounts.NL_360, 2011, 12, 28, 2016, 2, 28, 62 + 365 + 365 + 365 + 365),
    (DayCounts.NL_360, 2011, 12, 28, 2016, 2, 29, 62 + 365 + 365 + 365 + 365),
    (DayCounts.NL_360, 2011, 12, 28, 2016, 3, 1, 63 + 365 + 365 + 365 + 365),
    (DayCounts.NL_360, 2012, 2, 28, 2012, 3, 28, 28),
    (DayCounts.NL_360, 2012, 2, 29, 2012, 3, 28, 28),
    (DayCounts.NL_360, 2012, 3, 1, 2012, 3, 28, 27),
    (DayCounts.NL_360, 2011, 12, 1, 2012, 12, 1, 365),

    (DayCounts.NL_365, 2011, 12, 28, 2012, 2, 28, 62),
    (DayCounts.NL_365, 2011, 12, 28, 2012, 2, 29, 62),
    (DayCounts.NL_365, 2011, 12, 28, 2012, 3, 1, 63),
    (DayCounts.NL_365, 2011, 12, 28, 2016, 2, 28, 62 + 365 + 365 + 365 + 365),
    (DayCounts.NL_365, 2011, 12, 28, 2016, 2, 29, 62 + 365 + 365 + 365 + 365),
    (DayCounts.NL_365, 2011, 12, 28, 2016, 3, 1, 63 + 365 + 365 + 365 + 365),
    (DayCounts.NL_365, 2012, 2, 28, 2012, 3, 28, 28),
    (DayCounts.NL_365, 2012, 2, 29, 2012, 3, 28, 28),
    (DayCounts.NL_365, 2012, 3, 1, 2012, 3, 28, 27),
    (DayCounts.NL_365, 2011, 12, 1, 2012, 12, 1, 365),

    (DayCounts.THIRTY_360_ISDA, 2011, 12, 28, 2012, 2, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_ISDA, 2011, 12, 28, 2012, 2, 29, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_ISDA, 2011, 12, 28, 2012, 3, 1, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_ISDA, 2011, 12, 28, 2016, 2, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_ISDA, 2011, 12, 28, 2016, 2, 29, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_ISDA, 2011, 12, 28, 2016, 3, 1, SIMPLE_30_360DAYS),

    (DayCounts.THIRTY_360_ISDA, 2012, 2, 28, 2012, 3, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_ISDA, 2012, 2, 29, 2012, 3, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_ISDA, 2011, 2, 28, 2012, 2, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_ISDA, 2011, 2, 28, 2012, 2, 29, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_ISDA, 2012, 2, 29, 2016, 2, 29, SIMPLE_30_360DAYS),

    (DayCounts.THIRTY_360_ISDA, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_ISDA, 2012, 5, 30, 2013, 8, 29, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_ISDA, 2012, 5, 29, 2013, 8, 30, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_ISDA, 2012, 5, 30, 2013, 8, 30, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_ISDA, 2012, 5, 29, 2013, 8, 31, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_ISDA, 2012, 5, 30, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 8, 30)),
    (DayCounts.THIRTY_360_ISDA, 2012, 5, 31, 2013, 8, 30, calc360Days(2012, 5, 30, 2013, 8, 30)),
    (DayCounts.THIRTY_360_ISDA, 2012, 5, 31, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 8, 30)),

    (DayCounts.THIRTY_360_PSA, 2011, 12, 28, 2012, 2, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_PSA, 2011, 12, 28, 2012, 2, 29, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_PSA, 2011, 12, 28, 2012, 3, 1, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_PSA, 2011, 12, 28, 2016, 2, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_PSA, 2011, 12, 28, 2016, 2, 29, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_PSA, 2011, 12, 28, 2016, 3, 1, SIMPLE_30_360DAYS),

    (DayCounts.THIRTY_360_PSA, 2012, 2, 28, 2012, 3, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_PSA, 2012, 2, 29, 2012, 3, 28, calc360Days(2012, 2, 30, 2012, 3, 28)),
    (DayCounts.THIRTY_360_PSA, 2011, 2, 28, 2012, 2, 28, calc360Days(2011, 2, 30, 2012, 2, 28)),
    (DayCounts.THIRTY_360_PSA, 2011, 2, 28, 2012, 2, 29, calc360Days(2011, 2, 30, 2012, 2, 29)),
    (DayCounts.THIRTY_360_PSA, 2012, 2, 29, 2016, 2, 29, calc360Days(2012, 2, 30, 2016, 2, 29)),

    (DayCounts.THIRTY_360_PSA, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_PSA, 2012, 5, 30, 2013, 8, 29, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_PSA, 2012, 5, 29, 2013, 8, 30, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_PSA, 2012, 5, 30, 2013, 8, 30, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_PSA, 2012, 5, 29, 2013, 8, 31, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_360_PSA, 2012, 5, 30, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 8, 30)),
    (DayCounts.THIRTY_360_PSA, 2012, 5, 31, 2013, 8, 30, calc360Days(2012, 5, 30, 2013, 8, 30)),
    (DayCounts.THIRTY_360_PSA, 2012, 5, 31, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 8, 30)),

    (DayCounts.THIRTY_E_360, 2011, 12, 28, 2012, 2, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_360, 2011, 12, 28, 2012, 2, 29, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_360, 2011, 12, 28, 2012, 3, 1, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_360, 2011, 12, 28, 2016, 2, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_360, 2011, 12, 28, 2016, 2, 29, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_360, 2011, 12, 28, 2016, 3, 1, SIMPLE_30_360DAYS),

    (DayCounts.THIRTY_E_360, 2012, 2, 28, 2012, 3, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_360, 2012, 2, 29, 2012, 3, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_360, 2011, 2, 28, 2012, 2, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_360, 2011, 2, 28, 2012, 2, 29, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_360, 2012, 2, 29, 2016, 2, 29, SIMPLE_30_360DAYS),

    (DayCounts.THIRTY_E_360, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_360, 2012, 5, 30, 2013, 8, 29, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_360, 2012, 5, 29, 2013, 8, 30, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_360, 2012, 5, 30, 2013, 8, 30, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_360, 2012, 5, 29, 2013, 8, 31, calc360Days(2012, 5, 29, 2013, 8, 30)),
    (DayCounts.THIRTY_E_360, 2012, 5, 30, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 8, 30)),
    (DayCounts.THIRTY_E_360, 2012, 5, 31, 2013, 8, 30, calc360Days(2012, 5, 30, 2013, 8, 30)),
    (DayCounts.THIRTY_E_360, 2012, 5, 31, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 8, 30)),

    (DayCounts.THIRTY_EPLUS_360, 2011, 12, 28, 2012, 2, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_EPLUS_360, 2011, 12, 28, 2012, 2, 29, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_EPLUS_360, 2011, 12, 28, 2012, 3, 1, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_EPLUS_360, 2011, 12, 28, 2016, 2, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_EPLUS_360, 2011, 12, 28, 2016, 2, 29, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_EPLUS_360, 2011, 12, 28, 2016, 3, 1, SIMPLE_30_360DAYS),

    (DayCounts.THIRTY_EPLUS_360, 2012, 2, 28, 2012, 3, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_EPLUS_360, 2012, 2, 29, 2012, 3, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_EPLUS_360, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_EPLUS_360, 2011, 2, 28, 2012, 2, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_EPLUS_360, 2011, 2, 28, 2012, 2, 29, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_EPLUS_360, 2012, 2, 29, 2016, 2, 29, SIMPLE_30_360DAYS),

    (DayCounts.THIRTY_EPLUS_360, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_EPLUS_360, 2012, 5, 30, 2013, 8, 29, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_EPLUS_360, 2012, 5, 29, 2013, 8, 30, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_EPLUS_360, 2012, 5, 30, 2013, 8, 30, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_EPLUS_360, 2012, 5, 29, 2013, 8, 31, calc360Days(2012, 5, 29, 2013, 9, 1)),
    (DayCounts.THIRTY_EPLUS_360, 2012, 5, 30, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 9, 1)),
    (DayCounts.THIRTY_EPLUS_360, 2012, 5, 31, 2013, 8, 30, calc360Days(2012, 5, 30, 2013, 8, 30)),
    (DayCounts.THIRTY_EPLUS_360, 2012, 5, 31, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 9, 1)),

    (DayCounts.THIRTY_E_365, 2011, 12, 28, 2012, 2, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_365, 2011, 12, 28, 2012, 2, 29, calc360Days(2011, 12, 28, 2012, 2, 30)),
    (DayCounts.THIRTY_E_365, 2011, 12, 28, 2012, 3, 1, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_365, 2011, 12, 28, 2016, 2, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_365, 2011, 12, 28, 2016, 2, 29, calc360Days(2011, 12, 28, 2016, 2, 30)),
    (DayCounts.THIRTY_E_365, 2011, 12, 28, 2016, 3, 1, SIMPLE_30_360DAYS),

    (DayCounts.THIRTY_E_365, 2012, 2, 28, 2012, 3, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_365, 2012, 2, 29, 2012, 3, 28, calc360Days(2012, 2, 30, 2012, 3, 28)),
    (DayCounts.THIRTY_E_365, 2011, 2, 28, 2012, 2, 28, calc360Days(2011, 2, 30, 2012, 2, 28)),
    (DayCounts.THIRTY_E_365, 2011, 2, 28, 2012, 2, 29, calc360Days(2011, 2, 30, 2012, 2, 30)),
    (DayCounts.THIRTY_E_365, 2012, 2, 29, 2016, 2, 29, calc360Days(2012, 2, 30, 2012, 2, 30)),

    (DayCounts.THIRTY_E_365, 2012, 3, 1, 2012, 3, 28, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_365, 2012, 5, 30, 2013, 8, 29, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_365, 2012, 5, 29, 2013, 8, 30, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_365, 2012, 5, 30, 2013, 8, 30, SIMPLE_30_360DAYS),
    (DayCounts.THIRTY_E_365, 2012, 5, 29, 2013, 8, 31, calc360Days(2012, 5, 29, 2013, 8, 30)),
    (DayCounts.THIRTY_E_365, 2012, 5, 30, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 8, 30)),
    (DayCounts.THIRTY_E_365, 2012, 5, 31, 2013, 8, 30, calc360Days(2012, 5, 30, 2013, 8, 30)),
    (DayCounts.THIRTY_E_365, 2012, 5, 31, 2013, 8, 31, calc360Days(2012, 5, 30, 2013, 8, 30))
  )

  /**
   * All 22 rows of the `30U/360` family, in order. Each carries two expectations for the same
   * pair of dates: the one that holds when the end-of-month convention is not in use and the one
   * that holds when it is. `30/360 ISDA` reads no flag and so answers the first column;
   * `30U/360 EOM` applies the rule unconditionally and so answers the second.
   */
  lazy val data30U360: TableFor8[Int, Int, Int, Int, Int, Int, Double, Double] = Table(
    ("y1", "m1", "d1", "y2", "m2", "d2", "valueNotEom", "valueEom"),
    (2011, 12, 28, 2012, 2, 28, SIMPLE_30_360, SIMPLE_30_360),
    (2011, 12, 28, 2012, 2, 29, SIMPLE_30_360, SIMPLE_30_360),
    (2011, 12, 28, 2012, 3, 1, SIMPLE_30_360, SIMPLE_30_360),
    (2011, 12, 28, 2016, 2, 28, SIMPLE_30_360, SIMPLE_30_360),
    (2011, 12, 28, 2016, 2, 29, SIMPLE_30_360, SIMPLE_30_360),
    (2011, 12, 28, 2016, 3, 1, SIMPLE_30_360, SIMPLE_30_360),

    (2012, 2, 28, 2012, 3, 28, SIMPLE_30_360, SIMPLE_30_360),
    (2012, 2, 29, 2012, 3, 28, SIMPLE_30_360, calc360(2012, 2, 30, 2012, 3, 28)),
    (2012, 2, 29, 2012, 3, 30, SIMPLE_30_360, calc360(2012, 2, 30, 2012, 3, 30)),
    (2012, 2, 29, 2012, 3, 31, SIMPLE_30_360, calc360(2012, 2, 30, 2012, 3, 30)),
    (2012, 2, 29, 2013, 2, 28, SIMPLE_30_360, calc360(2012, 2, 30, 2013, 2, 30)),
    (2011, 2, 28, 2012, 2, 28, SIMPLE_30_360, calc360(2011, 2, 30, 2012, 2, 28)),
    (2011, 2, 28, 2012, 2, 29, SIMPLE_30_360, calc360(2011, 2, 30, 2012, 2, 30)),
    (2012, 2, 29, 2016, 2, 29, SIMPLE_30_360, calc360(2012, 2, 30, 2016, 2, 30)),

    (2012, 3, 1, 2012, 3, 28, SIMPLE_30_360, SIMPLE_30_360),
    (2012, 5, 30, 2013, 8, 29, SIMPLE_30_360, SIMPLE_30_360),
    (2012, 5, 29, 2013, 8, 30, SIMPLE_30_360, SIMPLE_30_360),
    (2012, 5, 30, 2013, 8, 30, SIMPLE_30_360, SIMPLE_30_360),
    (2012, 5, 29, 2013, 8, 31, SIMPLE_30_360, SIMPLE_30_360),
    (2012, 5, 30, 2013, 8, 31, calc360(2012, 5, 30, 2013, 8, 30), calc360(2012, 5, 30, 2013, 8, 30)),
    (2012, 5, 31, 2013, 8, 30, calc360(2012, 5, 30, 2013, 8, 30), calc360(2012, 5, 30, 2013, 8, 30)),
    (2012, 5, 31, 2013, 8, 31, calc360(2012, 5, 30, 2013, 8, 30), calc360(2012, 5, 30, 2013, 8, 30))
  )

  /**
   * All 19 `30E/360 ISDA` rows, in order. Each carries the expectation for the second date being
   * the maturity of the schedule and for it not being the maturity, which is the one fact this
   * member reads from a schedule.
   */
  lazy val data30E360ISDA: TableFor8[Int, Int, Int, Int, Int, Int, Double, Double] = Table(
    ("y1", "m1", "d1", "y2", "m2", "d2", "valueNotMaturity", "valueMaturity"),
    (2011, 12, 28, 2012, 2, 28, SIMPLE_30_360, SIMPLE_30_360),
    (2011, 12, 28, 2012, 2, 29, calc360(2011, 12, 28, 2012, 2, 30), SIMPLE_30_360),
    (2011, 12, 28, 2012, 3, 1, SIMPLE_30_360, SIMPLE_30_360),
    (2011, 12, 28, 2016, 2, 28, SIMPLE_30_360, SIMPLE_30_360),
    (2011, 12, 28, 2016, 2, 29, calc360(2011, 12, 28, 2016, 2, 30), SIMPLE_30_360),
    (2011, 12, 28, 2016, 3, 1, SIMPLE_30_360, SIMPLE_30_360),

    (2012, 2, 28, 2012, 3, 28, SIMPLE_30_360, SIMPLE_30_360),
    (2012, 2, 29, 2012, 3, 28, calc360(2012, 2, 30, 2012, 3, 28), calc360(2012, 2, 30, 2012, 3, 28)),
    (2011, 2, 28, 2012, 2, 28, calc360(2011, 2, 30, 2012, 2, 28), calc360(2011, 2, 30, 2012, 2, 28)),
    (2011, 2, 28, 2012, 2, 29, calc360(2011, 2, 30, 2012, 2, 30), calc360(2011, 2, 30, 2012, 2, 29)),
    (2012, 2, 29, 2016, 2, 29, calc360(2012, 2, 30, 2016, 2, 30), calc360(2012, 2, 30, 2016, 2, 29)),

    (2012, 3, 1, 2012, 3, 28, SIMPLE_30_360, SIMPLE_30_360),
    (2012, 5, 30, 2013, 8, 29, SIMPLE_30_360, SIMPLE_30_360),
    (2012, 5, 29, 2013, 8, 30, SIMPLE_30_360, SIMPLE_30_360),
    (2012, 5, 30, 2013, 8, 30, SIMPLE_30_360, SIMPLE_30_360),
    (2012, 5, 29, 2013, 8, 31, calc360(2012, 5, 29, 2013, 8, 30), calc360(2012, 5, 29, 2013, 8, 30)),
    (2012, 5, 30, 2013, 8, 31, calc360(2012, 5, 30, 2013, 8, 30), calc360(2012, 5, 30, 2013, 8, 30)),
    (2012, 5, 31, 2013, 8, 30, calc360(2012, 5, 30, 2013, 8, 30), calc360(2012, 5, 30, 2013, 8, 30)),
    (2012, 5, 31, 2013, 8, 31, calc360(2012, 5, 30, 2013, 8, 30), calc360(2012, 5, 30, 2013, 8, 30))
  )

  //-------------------------------------------------------------------------
  /**
   * All 57 `Act/Act AFB` rows, in order.
   *
   * The rule is under-specified, and these rows are the record of how it is read: ISDA's
   * "Calculation Period" translates "Periode d'Application", which meant the period the day
   * count is applied over and not the regular periodic schedule; ISDA's clarification for
   * rolling backward does not appear in the original French, so February 29th is chosen only
   * where the end date of the period is February 29th and the rolled-back date is in a leap
   * year; and, no document saying when to stop, rolling back in whole years continues until the
   * remainder is less than one year. The rows annotated with what the ISDA end-of-February
   * reading would have given are those decisions themselves.
   */
  lazy val dataACTACTAFB: TableFor7[Int, Int, Int, Int, Int, Int, Double] = Table(
    ("y1", "m1", "d1", "y2", "m2", "d2", "expected"),
    // example from the original French specification
    (1994, 2, 10, 1997, 6, 30, 140d / 365d + 3d),
    (1994, 2, 10, 1994, 6, 30, 140d / 365d),

    // simple examples that are less than one year long
    (2004, 2, 10, 2005, 2, 10, 1d),
    (2004, 2, 28, 2005, 2, 28, 1d),
    (2004, 2, 29, 2005, 2, 28, 365d / 366d),
    (2004, 3, 1, 2005, 3, 1, 1d),

    // examples over one year, from a fixed start date
    (2003, 2, 28, 2005, 2, 27, 1d + (364d / 365d)),
    (2003, 2, 28, 2005, 2, 28, 2d),
    (2003, 2, 28, 2005, 3, 1, 2d + (1d / 365d)),
    (2003, 2, 28, 2008, 2, 27, 4d + (364d / 365d)),
    (2003, 2, 28, 2008, 2, 28, 5d),
    (2003, 2, 28, 2008, 2, 29, 5d),
    (2003, 2, 28, 2008, 3, 1, 5d + (1d / 365d)),
    (2004, 2, 28, 2005, 2, 27, (365d / 366d)),
    (2004, 2, 28, 2005, 2, 28, 1d),
    (2004, 2, 28, 2005, 3, 1, 1d + (2d / 366d)),
    (2004, 2, 28, 2008, 2, 27, 3d + (365d / 366d)),
    (2004, 2, 28, 2008, 2, 28, 4d),  // ISDA end-of-February would give (4d + (1d / 365d))
    (2004, 2, 28, 2008, 2, 29, 4d + (1d / 365d)),
    (2004, 2, 28, 2008, 3, 1, 4d + (2d / 366d)),
    (2004, 2, 29, 2005, 2, 28, 365d / 366d),
    (2004, 2, 29, 2005, 3, 1, 1d + (1d / 366d)),
    (2004, 2, 29, 2008, 2, 27, 3d + (364d / 366d)),
    (2004, 2, 29, 2008, 2, 28, 3d + (365d / 366d)),  // ISDA end-of-February would give (4d)
    (2004, 2, 29, 2008, 2, 29, 4d),
    (2004, 2, 29, 2008, 3, 1, 4d + (1d / 366d)),
    (2004, 3, 1, 2005, 2, 28, 364d / 365d),
    (2004, 3, 1, 2005, 3, 1, 1d),
    (2004, 3, 1, 2008, 2, 27, 3d + (363d / 365d)),
    (2004, 3, 1, 2008, 2, 28, 3d + (364d / 365d)),
    (2004, 3, 1, 2008, 2, 29, 3d + (364d / 365d)),
    (2004, 3, 1, 2008, 3, 1, 4d),
    (2003, 3, 1, 2005, 2, 27, 1d + (363d / 365d)),
    (2003, 3, 1, 2005, 2, 28, 1d + (364d / 365d)),  // ISDA end-of-February would give (2d)
    (2003, 3, 1, 2005, 3, 1, 2d),
    (2003, 3, 1, 2008, 2, 27, 4d + (363d / 365d)),  // ISDA end-of-February would give (5d)
    (2003, 3, 1, 2008, 2, 28, 4d + (364d / 365d)),
    (2003, 3, 1, 2008, 2, 29, 5d),
    (2003, 3, 1, 2008, 3, 1, 5d),

    // examples over one year, up to a fixed end date
    (2004, 2, 28, 2006, 3, 1, 2d + (2d / 366d)),
    (2004, 2, 29, 2006, 3, 1, 2d + (1d / 366d)),
    (2004, 3, 1, 2006, 3, 1, 2d),
    (2005, 2, 28, 2007, 3, 1, 2d + (1d / 365d)),
    (2005, 3, 1, 2007, 3, 1, 2d),
    (2004, 2, 27, 2008, 2, 28, 4d + (1d / 365d)),  // ISDA end-of-February would give (4d + (2d / 365d))
    (2004, 2, 28, 2008, 2, 28, 4d),  // ISDA end-of-February would give (4d + (1d / 365d))
    (2004, 2, 29, 2008, 2, 28, 3d + (365d / 366d)),  // ISDA end-of-February would give (4d)
    (2004, 3, 1, 2008, 2, 28, 3d + (364d / 365d)),
    (2006, 2, 27, 2008, 2, 28, 2d + (1d / 365d)),
    (2006, 2, 28, 2008, 2, 28, 2d),
    (2006, 3, 1, 2008, 2, 28, 1d + (364d / 365d)),
    (2004, 2, 28, 2008, 2, 29, 4d + (1d / 365d)),
    (2004, 2, 29, 2008, 2, 29, 4d),
    (2004, 3, 1, 2008, 2, 29, 3d + (364d / 365d)),
    (2006, 2, 27, 2008, 2, 29, 2d + (1d / 365d)),
    (2006, 2, 28, 2008, 2, 29, 2d),
    (2006, 3, 1, 2008, 2, 29, 1d + (364d / 365d))
  )

  /**
   * All 12 `Act/365L` rows, in order: the two dates, the frequency and the end of the schedule
   * period - the two facts this member reads - and the expected fraction. The rows pair an
   * annual frequency, which puts the leap-day test on the whole period, against a semi-annual
   * one, which asks only whether the period ends in a leap year.
   */
  lazy val dataACT365L: TableFor11[Int, Int, Int, Int, Int, Int, Frequency, Int, Int, Int, Double] = Table(
    ("y1", "m1", "d1", "y2", "m2", "d2", "freq", "y3", "m3", "d3", "expected"),
    (2011, 12, 28, 2012, 2, 28, Frequency.P12M, 2012, 2, 28, 62d / 365d),
    (2011, 12, 28, 2012, 2, 28, Frequency.P12M, 2012, 2, 29, 62d / 366d),
    (2011, 12, 28, 2012, 2, 28, Frequency.P12M, 2012, 3, 1, 62d / 366d),

    (2011, 12, 28, 2012, 2, 29, Frequency.P12M, 2012, 2, 29, 63d / 366d),
    (2011, 12, 28, 2012, 2, 29, Frequency.P12M, 2012, 3, 1, 63d / 366d),

    (2011, 12, 28, 2012, 2, 28, Frequency.P6M, 2012, 2, 28, 62d / 366d),
    (2011, 12, 28, 2012, 2, 28, Frequency.P6M, 2012, 2, 29, 62d / 366d),
    (2011, 12, 28, 2012, 2, 28, Frequency.P6M, 2012, 3, 1, 62d / 366d),

    (2011, 12, 28, 2012, 2, 29, Frequency.P6M, 2012, 2, 29, 63d / 366d),
    (2011, 12, 28, 2012, 2, 29, Frequency.P6M, 2012, 3, 1, 63d / 366d),

    (2010, 12, 28, 2011, 2, 28, Frequency.P6M, 2011, 2, 28, 62d / 365d),
    (2010, 12, 28, 2011, 2, 28, Frequency.P6M, 2011, 3, 1, 62d / 365d)
  )

  /**
   * All 80 lenient spellings, in order, each with the member it resolves to. Two spellings
   * appear twice - `Actual/Actual ISDA` and `Act/Act` - and both occurrences are kept, because
   * this table is a transcription of the published list. The last row does not name a member of
   * the closed family: `BUS/252` resolves to the convention over the Brazilian calendar.
   */
  lazy val dataLenient: TableFor2[String, DayCount] = Table(
    ("name", "dayCount"),
    ("Actual/Actual", DayCounts.ACT_ACT_ISDA),
    ("Act/Act", DayCounts.ACT_ACT_ISDA),
    ("A/A", DayCounts.ACT_ACT_ISDA),
    ("Actual/Actual ISDA", DayCounts.ACT_ACT_ISDA),
    ("A/A ISDA", DayCounts.ACT_ACT_ISDA),
    ("Actual/Actual ISDA", DayCounts.ACT_ACT_ISDA),
    ("A/A (ISDA)", DayCounts.ACT_ACT_ISDA),
    ("Act/Act (ISDA)", DayCounts.ACT_ACT_ISDA),
    ("Actual/Actual (ISDA)", DayCounts.ACT_ACT_ISDA),
    ("Act/Act", DayCounts.ACT_ACT_ISDA),
    ("Actual/Actual (Historical)", DayCounts.ACT_ACT_ISDA),

    ("A/A ICMA", DayCounts.ACT_ACT_ICMA),
    ("Actual/Actual ICMA", DayCounts.ACT_ACT_ICMA),
    ("A/A (ICMA)", DayCounts.ACT_ACT_ICMA),
    ("Act/Act (ICMA)", DayCounts.ACT_ACT_ICMA),
    ("Actual/Actual (ICMA)", DayCounts.ACT_ACT_ICMA),
    ("ISMA-99", DayCounts.ACT_ACT_ICMA),
    ("Actual/Actual (Bond)", DayCounts.ACT_ACT_ICMA),

    ("A/A AFB", DayCounts.ACT_ACT_AFB),
    ("Actual/Actual AFB", DayCounts.ACT_ACT_AFB),
    ("A/A (AFB)", DayCounts.ACT_ACT_AFB),
    ("Act/Act (AFB)", DayCounts.ACT_ACT_AFB),
    ("Actual/Actual (AFB)", DayCounts.ACT_ACT_AFB),
    ("Actual/Actual (Euro)", DayCounts.ACT_ACT_AFB),

    ("A/365 Actual", DayCounts.ACT_365_ACTUAL),
    ("Actual/365 Actual", DayCounts.ACT_365_ACTUAL),
    ("A/365 (Actual)", DayCounts.ACT_365_ACTUAL),
    ("Act/365 (Actual)", DayCounts.ACT_365_ACTUAL),
    ("Actual/365 (Actual)", DayCounts.ACT_365_ACTUAL),
    ("A/365A", DayCounts.ACT_365_ACTUAL),
    ("Act/365A", DayCounts.ACT_365_ACTUAL),
    ("Actual/365A", DayCounts.ACT_365_ACTUAL),

    ("A/365L", DayCounts.ACT_365L),
    ("Actual/365L", DayCounts.ACT_365L),
    ("A/365 Leap year", DayCounts.ACT_365L),
    ("Act/365 Leap year", DayCounts.ACT_365L),
    ("Actual/365 Leap year", DayCounts.ACT_365L),
    ("ISMA-Year", DayCounts.ACT_365L),

    ("Actual/360", DayCounts.ACT_360),
    ("A/360", DayCounts.ACT_360),
    ("French", DayCounts.ACT_360),

    ("Actual/364", DayCounts.ACT_364),
    ("A/364", DayCounts.ACT_364),

    ("A/365F", DayCounts.ACT_365F),
    ("Actual/365F", DayCounts.ACT_365F),
    ("A/365", DayCounts.ACT_365F),
    ("Act/365", DayCounts.ACT_365F),
    ("Actual/365", DayCounts.ACT_365F),
    ("Act/365 (Fixed)", DayCounts.ACT_365F),
    ("Actual/365 (Fixed)", DayCounts.ACT_365F),
    ("A/365 (Fixed)", DayCounts.ACT_365F),
    ("Actual/Fixed 365", DayCounts.ACT_365F),
    ("English", DayCounts.ACT_365F),

    ("A/365.25", DayCounts.ACT_365_25),
    ("Actual/365.25", DayCounts.ACT_365_25),

    ("NL360", DayCounts.NL_360),
    ("Act/360 No leap year", DayCounts.NL_360),

    ("A/NL", DayCounts.NL_365),
    ("Actual/NL", DayCounts.NL_365),
    ("NL365", DayCounts.NL_365),
    ("Act/365 No leap year", DayCounts.NL_365),

    ("30/360", DayCounts.THIRTY_360_ISDA),

    ("Eurobond Basis", DayCounts.THIRTY_E_360),
    ("30S/360", DayCounts.THIRTY_E_360),
    ("Special German", DayCounts.THIRTY_E_360),
    ("30/360 ICMA", DayCounts.THIRTY_E_360),
    ("30/360 (ICMA)", DayCounts.THIRTY_E_360),

    ("30/360 German", DayCounts.THIRTY_E_360_ISDA),
    ("German", DayCounts.THIRTY_E_360_ISDA),

    ("30/360 US", DayCounts.THIRTY_U_360),
    ("30/360 (US)", DayCounts.THIRTY_U_360),
    ("30US/360", DayCounts.THIRTY_U_360),
    ("360/360", DayCounts.THIRTY_U_360),
    ("Bond Basis", DayCounts.THIRTY_U_360),
    ("US", DayCounts.THIRTY_U_360),
    ("ISMA-30/360", DayCounts.THIRTY_U_360),
    ("30/360 SIA", DayCounts.THIRTY_U_360),
    ("30/360 (SIA)", DayCounts.THIRTY_U_360),

    ("30/365 German", DayCounts.THIRTY_E_365),

    ("BUS/252", DayCount.ofBus252(StandardHolidayCalendars.BRBD))
  )
}
