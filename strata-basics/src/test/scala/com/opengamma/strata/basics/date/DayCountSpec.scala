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
 * Test [[DayCount]], ported from the Java `DayCountTest`.
 *
 * This is the largest test class of the folder being ported - 49 annotated methods, 26 of them
 * plain and 23 parameterised over nine data providers - and it is the front line for day-count
 * correctness. Every method keeps the name it had, so the method-level traceability recorded in
 * `manifest/java-test-mapping.csv` stays one-to-one, and each parameterised method becomes one
 * test whose table is driven inside it rather than several tests: the roster of names is the
 * join key the acceptance gate uses, so it is closed and exact.
 *
 * ===The nine providers===
 *
 * Every provider is transcribed in full, and the row counts here are the counts in the Java
 * source rather than round numbers:
 *
 *   - `data_types` - the 21 standard members, driving `test_null`, `test_wrongOrder`,
 *     `test_same`, `test_halfYear` and `test_wholeYear`;
 *   - `data_yearFraction` - '''201''' rows, driving `test_yearFraction`,
 *     `test_relativeYearFraction` and `test_relativeYearFraction_reverse`;
 *   - `data_days` - '''185''' rows;
 *   - `data_30U360` - 22 rows, each carrying the expectation for the flag set both ways,
 *     driving four tests;
 *   - `data_30E360ISDA` - 19 rows, each carrying the expectation for the second date being the
 *     maturity and not being it, driving two tests;
 *   - `data_ACTACTAFB` - '''57''' rows, with the commentary of the original provider kept
 *     because it records how an under-specified rule was interpreted;
 *   - `data_ACT365L` - 12 rows;
 *   - `data_name` - the 21 names, driving five tests;
 *   - `data_lenient` - '''80''' rows.
 *
 * The last count is worth a word, because it is easy to confuse with another. 80 is the number
 * of rows in the Java `data_lenient` provider, which is what this spec drives; 67 is the number
 * of ordered rewrite rules in the configuration resource those rows exercise, which the port
 * transcribes into `DayCount` as code. Both are asserted: the 80 rows here, and the size and
 * replacement order of the 67-row table in `test_extendedEnum`, so neither a lost row nor a
 * reordered rule can pass unnoticed.
 *
 * ===The sentinel in the numeric providers===
 *
 * `data_yearFraction` marks a row on which the `30/360` family applies no day-of-month
 * adjustment with the sentinel `SIMPLE_30_360`, and `data_days` does the same with
 * `SIMPLE_30_360DAYS`; the test body then computes the unadjusted expectation for such a row.
 * That branching is reproduced here exactly rather than expanded into literals, because it is
 * load-bearing in a way a reader would not guess: the integer sentinel is zero, so a row whose
 * transcribed expectation happens to '''be''' zero is also rewritten, and `data_days` contains
 * one such row - `30E/365` from 2012-02-29 to 2016-02-29, written as
 * `calc360Days(2012, 2, 30, 2012, 2, 30)`, which is zero and whose correct answer, 1440, the
 * sentinel branch supplies. Expanding the sentinels would have silently asserted zero there.
 *
 * ===How the shape of the port changes the assertions===
 *
 *   - `test_null` passed the absent reference in each of six argument positions and asserted a
 *     throw. The `notNull` family of the argument checker that raised those throws has no target
 *     in this port, because the argument it guarded against cannot be expressed: both dates are
 *     required parameters of a required type. The case is asserted as a compile-time proof - six
 *     of them, one per Java position - and no `null` is written anywhere in this file.
 *   - `test_wrongOrder` stays a throw, and deliberately so. Supplying the dates out of order
 *     breaks the documented contract of `yearFraction` and `days` rather than handing them data
 *     they might legitimately be asked about, which is the classification AAP 0.3.3 applies: a
 *     caller-contract violation is raised through `ArgCheck`, and only a failure that depends on
 *     the data of the arguments is returned as a value. The same classification keeps the
 *     absence of schedule information a throw (see `test_scheduleInfo`) while making
 *     unresolvable text a `Left` (see `test_of_lookup_notFound`).
 *   - `test_of_lookup` asserted one throwing factory, `DayCount.of`. Two members replaced it -
 *     [[DayCount.valueOf]], the exact lookup returning an `Option`, and [[DayCount.parse]], the
 *     lenient lookup returning `EitherNec[Failure, _]` - and both are asserted wherever Java
 *     asserted `of`, so the two entry points cannot drift apart.
 *   - `test_extendedEnum` read the registry the Java type assembled by loading a configuration
 *     resource from the class path. There is no registry here: the family is closed and the
 *     tables are code (AAP D-2 / Rule 4). The counterpart of the Java `lookupAll` is the union
 *     of `byCanonicalName` and `byUpperName`, and of `lookupAllNormalized` is
 *     `byCanonicalName`; both are asserted by their exact key sets, along with the two groups
 *     of external spellings and the ordered lenient table, because a row lost in transcription
 *     would otherwise be invisible.
 *   - `test_lenientLookup_constants` reflected over the public fields of `DayCounts`. This port
 *     performs no reflection of any kind, so the 21 identifiers are written out as a table; that
 *     they are exactly the members is asserted in `coverage`.
 *   - `test_relativeYearFraction_defaultMethod` built an anonymous subclass of `DayCount`. The
 *     family is sealed, so that form is unrepresentable here; the property the Java method was
 *     really about is asserted over real members instead.
 *   - `test_scheduleInfo` asserted that four accessors of the bare interface raise. They are
 *     total in this port and answer `None` (AAP 0.6.1), so that is what is asserted - together
 *     with the two refusals that did '''not''' move: a day count asked to calculate against a
 *     schedule that cannot tell it what its rule is defined in terms of, and `Act/Act ICMA`
 *     asked to accrue over a schedule whose frequency has no whole number of events in a year,
 *     which is a complete schedule that the convention is none the less not defined over.
 *   - `coverage` invoked a private constructor and read the values of a Java enum reflectively,
 *     to satisfy a coverage tool. Neither has a target here, so what they stood for is asserted
 *     directly: the constants are the members, `values` is closed and in declaration order, and
 *     the typeclass instances agree with each other.
 *   - `test_serialization` round-tripped through Java serialization and through the bean
 *     library's own encodings, all three reflective. None is a dependency of this port, so the
 *     round trip is asserted through the circe codec that replaced them, whose document for a
 *     standard member is the bare canonical name.
 *   - `test_jodaConvert` asserted a round trip through the reflective string-conversion library
 *     the type was annotated for. Those two annotations became `Show` and [[DayCount.parse]].
 *
 * `Bus/252` is covered by `Business252DayCountSpec`, the 1e-9 comparison against the captured
 * Java baseline by `parity.DayCountParitySpec`, the cross-family alias sweep by
 * `NamedEnumClosedSpec`, the equality of the transcribed tables with the captured manifest by
 * `ReferenceDataManifestSpec`, and the property-based codec round trip by
 * `json.JsonRoundTripSpec`. This spec carries the hand-written Java expectations and nothing
 * else.
 */
class DayCountSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks with TimeLimits {

  import DayCountSpec._

  //-------------------------------------------------------------------------
  test("test_null") {
    // Reinterpretation. The Java method made six calls - `yearFraction` and `days`, each with
    // the absent reference first, second and in both positions - and asserted that every one of
    // them raised. What raised was the `notNull` guard of the argument checker, which this port
    // does not carry, because what it guarded against is not expressible: both dates are
    // required parameters of a required type, so a call that omits one, supplies one, or
    // supplies something that is not a date is rejected when this spec is compiled rather than
    // when it runs. Each of the six Java positions therefore becomes a compile-time proof, and
    // no `null` appears anywhere in this file.
    assertDoesNotCompile("DayCounts.ACT_360.yearFraction()")
    assertDoesNotCompile("DayCounts.ACT_360.yearFraction(java.time.LocalDate.of(2010, 1, 1))")
    assertDoesNotCompile("""DayCounts.ACT_360.yearFraction("2010-01-01", "2010-01-02")""")
    assertDoesNotCompile("DayCounts.ACT_360.days()")
    assertDoesNotCompile("DayCounts.ACT_360.days(java.time.LocalDate.of(2010, 1, 1))")
    assertDoesNotCompile("DayCounts.ACT_360.days(2010, 1, 1)")

    // The other half of what the Java method established: given the two dates it requires, every
    // member of the family answers and none of them raises. The schedule information is the
    // whole-year fixture, so the four members that read something are given what they read.
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
      // A sanctioned refusal rather than a returned failure. `yearFraction` and `days` are
      // documented to take their dates in time-line order, so a reversed pair is a broken call
      // and not an input whose data happens to be unanswerable - the distinction AAP 0.3.3
      // draws, and the reason this precondition stays an `IllegalArgumentException` raised
      // through `ArgCheck` exactly as the Java implementation raised it. The member that
      // accepts a reversed pair is `relativeYearFraction`, which bypasses this check on
      // purpose; `test_relativeYearFraction_reverse` is where that is asserted.
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
          // A period of no length is no fraction of a year and no days, for every member but
          // one. Note that this holds for the four members that read schedule information as
          // well, and with the two-argument overload that carries none: each of them answers
          // for equal dates before reading anything, which is the short circuit the Java
          // implementation had and the reason this case needs no fixture.
          dayCount.yearFraction(JAN_02, JAN_02) shouldBe 0d
          dayCount.days(JAN_02, JAN_02) shouldBe 0
        } else {
          // `1/1` is the member the Java method excluded, because it answers one whatever the
          // dates. Asserting what it does answer states why it is excluded.
          dayCount.yearFraction(JAN_02, JAN_02) shouldBe 1d
          dayCount.days(JAN_02, JAN_02) shouldBe 1
        }
      }
    }
  }

  test("test_halfYear") {
    // A sanity check that half a year is a fraction close to a half, for every rule. The
    // tolerances are the Java ones: a hundredth of a year, and two days on a count of 182.
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
    // The same sanity check over a whole year, with the Java tolerances of two hundredths of a
    // year and five days on a count of 365. The `30/360` members are the ones that use the
    // whole of the day tolerance, answering 360.
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
    // The sentinel is the not-a-number value, which is how one row can be marked as carrying no
    // expectation of its own: the Java body compared the boxed expectation against the boxed
    // constant by identity, and testing for not-a-number is the same test made on the primitive.
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
    // Dates in order: the relative form agrees with the plain one to the last bit, because it
    // reaches the same calculation without the order check in front of it.
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
    // Dates reversed: the relative form answers the negation where the plain form refuses, which
    // is the whole point of the two-layer split between the public method that checks the order
    // and the calculation that does not.
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
    // the first expectation column of the provider is shared with `test_yearFraction_30360ISDA`.
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
    // The same member with the flag set, which is where the end-of-February rule applies and
    // the second expectation column parts company with the first.
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
    // `30/360 ISDA` reads no schedule information at all, so it answers the first expectation
    // column even though the flag is set - which is exactly what this test, driven with the
    // flag set, establishes.
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
    // `30U/360 EOM` applies the end-of-February rule unconditionally, reading no flag, so it
    // answers the second expectation column.
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
    // `30E/360 ISDA` changes a second day-of-month at the end of February to 30 unless that
    // date is the maturity of the schedule, so it reads the end date - and only in that one
    // case, which is the short circuit the ported expression performs.
    //
    // The Java fixture for this half of the provider carried no end date at all and relied on
    // the comparison against the absent reference answering "not the maturity". An absent end
    // date is `None` here, and the port refuses to guess for a member that has reached the
    // point of reading it, so the fixture states the same fact positively: the schedule ends
    // somewhere other than the second date. Four rows of this provider - those whose second
    // date is the last day of February - are the ones that depend on it.
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
    // The same provider with the second date declared to be the maturity, which suppresses the
    // end-of-February rule on it. The fixture is the Java one field for field.
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
    // The rule is poorly specified, and these rows are the interpretation the library being
    // ported settled on - including the cases where it deliberately departs from the ISDA
    // clarification, which the provider records in its own commentary. They are transcribed
    // unchanged, because they are the specification of this member's behaviour.
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
  // The canonical `Act/Act ICMA` worked examples. Each builds its schedule by hand and states
  // the expected fraction as the arithmetic of the nominal periods it is defined over, and each
  // is transcribed date for date and term for term: these are the examples the whole day-count
  // baseline is trusted on, so a "simplified" expectation here would remove the only
  // independent statement of what the answer should be. The nominal periods each case relies on
  // are named in the comment the Java method carried.
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
  // each asserting the three `Act/Act` members against the same pair of dates, which is what
  // makes them worth keeping as one test per case rather than one per member: the three answers
  // differing in the documented way is the statement being made.
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
    // initial period
    val info1 =
      Info(Some(start), Some(end.plus(Frequency.P12M.period)), Some(firstRegular), true, Some(Frequency.P12M))
    // regular period
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
    // initial period
    val info1 = Info(Some(start), Some(end), Some(firstRegular), true, Some(Frequency.P6M))
    // regular period
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
    // regular period
    val info1 = Info(Some(start), Some(end), Some(lastRegular), true, Some(Frequency.P6M))
    // final period
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
    // Over 400 consecutive start dates and, for each, every period of up to a year, the annual
    // `Act/Act ICMA` answer and the `Act/Act Year` answer agree to the last bit. That is 146,000
    // comparisons of two independently written rules, and it is the strongest single statement
    // in this spec about the `Act/Act ICMA` nominal-period machinery.
    //
    // The comparison is made without a clue built per iteration - at this volume the strings
    // would cost more than the arithmetic - and the failure message is assembled only for a
    // pair that disagrees, naming the dates, which is what a reader of a failure needs.
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
      // The 21 names are the contract, not a label: they are what a caller writes, what the
      // codec puts on the wire, and what the captured Java manifest is compared against.
      dayCount.name shouldBe name
    }
  }

  test("test_toString") {
    forAll(dataName) { (dayCount: DayCount, name: String) =>
      withClue(s"$name: ") {
        dayCount.toString shouldBe name
        // `Show` is the third way of rendering a day count as text, and it agrees with the
        // other two - which is what lets a message, a document and a log line all name the
        // same convention the same way.
        Show[DayCount].show(dayCount) shouldBe name
      }
    }
  }

  test("test_of_lookup") {
    // The Java `DayCount.of(name)` was one throwing factory. Both members that replaced it are
    // asserted here for every canonical name, and for the upper-case spelling of it, because
    // those are the two keys each member is registered under.
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
    // The Java method asked the registry for the lower-case spelling of each canonical name,
    // through `findLenient`; `parse` is that method here. The exact lookup is asserted
    // alongside it to show where the answer comes from: for the 20 names that contain a letter
    // the lower-case spelling is outside the exact key space and resolves only because of the
    // leniency, and for `1/1`, which contains none, the lower-case spelling *is* the canonical
    // name and the exact lookup answers directly.
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
    // The Java method read `extendedEnum().lookupAll()` - the registry assembled by loading
    // `DayCount.ini` from the class path - and looked each canonical name up in it. There is no
    // registry here and nothing is loaded: the family is closed and the three tables of that
    // resource are code in the companion (AAP D-2 / Rule 4). This test is therefore where those
    // tables are asserted, because a row lost in transcription would otherwise be invisible.
    val lookup = DayCount.namedEnum

    forAll(dataName) { (dayCount: DayCount, name: String) =>
      // `byCanonicalName` is the Java `lookupAllNormalized`: the members re-keyed by their own
      // name, which is the view the Java method's `map.get(name)` was really asking for.
      withClue(s"$name: ")(lookup.byCanonicalName.get(name) shouldBe Some(dayCount))
    }

    lookup.familyName shouldBe "DayCount"
    lookup.toString shouldBe "NamedEnum[DayCount]"
    lookup.values.toList shouldBe declarationOrder

    lookup.byCanonicalName should have size 21
    lookup.byCanonicalName.keySet shouldBe standardNames.toSet
    lookup.byUpperName should have size 21
    lookup.byUpperName.keySet shouldBe standardNames.map(_.toUpperCase(Locale.ENGLISH)).toSet

    // The Java `lookupAll` key set was the union of the two: every canonical name and the
    // English upper case of it, first registration winning. Ten of the 21 names contain a
    // lower-case letter and so contribute a second key; the other eleven are already their own
    // upper case and contribute one, which is why the union holds 31 keys and not 42.
    val allKeys = lookup.byCanonicalName.keySet ++ lookup.byUpperName.keySet
    allKeys should have size 31
    standardNames.filter(name => name != name.toUpperCase(Locale.ENGLISH)) should have size 10

    // This family declares no alternate spelling, because the resource declared none for it:
    // every spelling beyond those 31 keys arrives through the lenient chain.
    lookup.alternateNames shouldBe Map.empty[String, String]

    // The two groups of external spellings, asserted row for row. They take part in no lookup -
    // `ACT/360` resolves because the lenient chain accepts it, not because FpML publishes it -
    // so they exist to be read explicitly, and this is the only place they are read.
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

    // The resolved views hold the same rows as values, all 14 of them and all 8. The FpML group
    // is the one place in this module where a published external row names something that is not
    // a member of the closed family: `BUS/252` names `Bus/252 BRBD`, one of the calendar-bearing
    // conventions, of which there is one per holiday calendar rather than one per family. It
    // resolves none the less, because an external row is resolved through the '''family's own'''
    // lookup - `DayCount.valueOf`, which spans the 21 standard members and the `Bus/252`
    // conventions together - exactly as the external lookup of the ported registry resolved such
    // a row by delegating the name it carries to its second provider. So the resolved view is the
    // raw table row for row, while `values` still holds the 21 and not that convention, and
    // `parse("BUS/252")` reaches the same day count by the lenient route.
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
    // The two groups disagree about two spellings, which is exactly why they are published
    // separately rather than merged into one table of aliases.
    lookup.externalNames("FpML").flatMap(_.get("30E/360")) shouldBe Some(DayCounts.THIRTY_E_360)
    lookup.externalNames("SWIFT").flatMap(_.get("30E/360")) shouldBe Some(DayCounts.THIRTY_E_360_ISDA)
    lookup.externalNames("FpML").flatMap(_.get("ACT/365")) shouldBe Some(DayCounts.ACT_365F)
    lookup.externalNames("SWIFT").flatMap(_.get("ACT/365")) shouldBe Some(DayCounts.ACT_ACT_ISDA)

    // A group the family does not publish is an empty answer rather than a failure.
    lookup.externalNames("Rubbish") shouldBe None
    lookup.externalNamesRaw("Rubbish") shouldBe None

    // The ordered lenient table: the 67 rows of the `[lenientPatterns]` section of the resource,
    // in the order it listed them. The order is part of the data, because `parse` applies every
    // pattern in turn and a later one sees what an earlier one produced, so the sequence of
    // replacements is asserted rather than just the count - reordering the rows would change
    // what resolves and to what, and is the one change to this table that no individual row
    // assertion would catch.
    // Read as text through `lenientSources`, the raw view, rather than through the compiled
    // projection: the rows are what is being asserted, and reading them this way compiles none of
    // the sixty-seven expressions.
    lookup.lenientSources should have size 67
    lookup.lenientSources.map { case (_, replacement) => replacement } shouldBe lenientReplacements
    lookup.lenientSources.map { case (source, _) => source }.head shouldBe "ACTUAL/ACTUAL(.*)"
  }

  test("test_of_lookup_notFound") {
    // Where the Java factory raised for text naming no member, the port reports it as a value.
    // This is the data-dependent half of the Rule 5 split: the text is an input whose content
    // decides the outcome, so the outcome is a `Left` carrying a reason from the closed family
    // of reasons and a message naming both the family and the text.
    DayCount.valueOf("Rubbish") shouldBe None
    DayCount.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
    DayCount.parse("Rubbish") should haveFailureMessageMatching("DayCount name not found: Rubbish")

    // A `Bus/252` name whose calendar this library does not define is recognised as far as its
    // prefix and then fails on the calendar, so the calendar's own failure is what is reported -
    // the more specific answer, and the one the ported implementation raised from the same place.
    DayCount.valueOf("Bus/252 ZZZZ") shouldBe None
    DayCount.parse("Bus/252 ZZZZ") should beFailureWith(FailureReason.PARSING)
    DayCount.parse("Bus/252 ZZZZ") should
      haveFailureMessageMatching("HolidayCalendar name not found: ZZZZ")

    // The reference-data form of the same lookup: an identifier the supplied data cannot
    // resolve is missing data rather than unparseable text, which is the reason the resolution
    // reports and the second data-dependent failure of this type's surface.
    DayCount.ofBus252(HolidayCalendarId.of("ZZZZ"), ReferenceData.standard) should
      beFailureWith(FailureReason.MISSING_DATA)
    DayCount.ofBus252(HolidayCalendarIds.BRBD, ReferenceData.standard) should
      haveValue(DayCount.ofBus252(StandardHolidayCalendars.BRBD))
  }

  test("test_of_lookup_null") {
    // Reinterpretation, as in `test_null`: the Java method handed the absent reference to the
    // factory and asserted a throw. The `notNull` guard behind that throw has no target here,
    // because the name is a required parameter of a required type, so the case becomes a
    // compile-time proof and no `null` is written.
    assertDoesNotCompile("DayCount.parse()")
    assertDoesNotCompile("DayCount.valueOf()")
    assertDoesNotCompile("DayCount.parse(1)")
    assertDoesNotCompile("DayCount.valueOf(1)")

    // What a caller can actually supply is text that names nothing, so the rest of the case is
    // every spelling of "no usable name" - empty, blank, padded, and several near-misses - each
    // of which resolves to nothing, reports the parsing reason, and raises nothing at all.
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

    // `Bus/252` is in the list above to be excluded from it deliberately: it is the one entry
    // that does resolve, because the resource declared a lenient row defaulting it to the
    // Brazilian calendar. Stating that here keeps the near-miss `"Bus/252  "`, which does not
    // resolve, from reading as an accident.
    DayCount.parse("Bus/252") should haveValue(DayCount.ofBus252(StandardHolidayCalendars.BRBD))

    // The last thing a caller can supply is a lot of text. `parse` folds its input to upper case
    // and runs all 67 rewrites over it, several of which hold a group that can consume text of
    // unbounded length, so the cost of rejecting text has to be a function of its length and not
    // of its length squared. Two hundred thousand open brackets is the input that made that
    // difference visible: `(.*)[(](.*)[)]` has an open bracket to try at every one of those
    // positions and, before the rewrites learned what a full match of them must end with, took
    // minutes to decide it could not match. It is asserted as the other entries are - nothing
    // resolves, the parsing reason is reported, and nothing is raised - inside a generous time
    // limit that is a regression guard on the cost rather than a measurement of it: the work is
    // now one pass, so thirty seconds cannot flake however loaded the host is, while the
    // quadratic behaviour could not fit in it.
    val oversized: String = "(" * 200000
    val oversizedOutcome: ResultNec[DayCount] = failAfter(Span(30L, Seconds))(DayCount.parse(oversized))
    oversizedOutcome should beFailureWith(FailureReason.PARSING)
    noException should be thrownBy DayCount.parse(oversized)
    DayCount.valueOf(oversized) shouldBe None

    // And the other half of that statement, which is why the cost is bounded by the rewrites
    // rather than by refusing long text outright: text is never rejected for its size, at either
    // stage of the lookup. A `Bus/252` name may carry a combined calendar of any number of parts,
    // so ten thousand characters naming two thousand and one calendars resolve to the day count
    // over London and New York - the duplicate parts folding away, as a calendar combined with
    // itself does - where a length cutoff would have refused the name outright.
    val longCalendar: String = "Bus/252 " + ("GBLO+" * 2000) + "USNY"
    longCalendar.length shouldBe 10012
    DayCount.parse(longCalendar).map(dayCount => dayCount.name) should haveValue("Bus/252 GBLO+USNY")
    DayCount.valueOf(longCalendar).map(dayCount => dayCount.name) shouldBe Some("Bus/252 GBLO+USNY")

    // The lenient stage is reached by text of any length as well, which is the same statement
    // about the second stage: the chain rewrites the head of this ten-thousand-character input
    // and hands the rest of it back untouched, rather than being skipped because the input is
    // long. That the result then names no day count is beside the point being made here - what
    // matters is that the rewrites ran.
    DayCount.namedEnum.rewriteLeniently("ACT/ACT" + ("X" * 10000)) shouldBe "Act/Act" + ("X" * 10000)
  }

  //-------------------------------------------------------------------------
  test("test_lenientLookup_specialNames") {
    // The Java method drove every row of `data_lenient` through `findLenient` after folding it
    // to lower case. `parse` is that method here, and each row is asserted in three spellings -
    // as written, folded down and folded up - because the input is folded to upper case before
    // the patterns are applied and the patterns themselves are matched insensitively to case.
    forAll(dataLenient) { (name: String, dayCount: DayCount) =>
      withClue(s"$name: ") {
        DayCount.parse(name.toLowerCase(Locale.ENGLISH)) should haveValue(dayCount)
        DayCount.parse(name) should haveValue(dayCount)
        DayCount.parse(name.toUpperCase(Locale.ENGLISH)) should haveValue(dayCount)
      }
    }
  }

  test("test_lenientLookup_constants") {
    // The port of the reflective sweep over the public fields of `DayCounts`: each identifier
    // resolves leniently to the constant it names, in its own spelling and folded to lower
    // case, exactly as the Java method asserted for the field names it discovered. No
    // reflection is performed - the 21 identifiers are a table, and `coverage` is where they
    // are shown to be exactly the members.
    forAll(dataConstantIdentifiers) { (identifier: String, dayCount: DayCount) =>
      withClue(s"$identifier: ") {
        DayCount.parse(identifier) should haveValue(dayCount)
        DayCount.parse(identifier.toLowerCase(Locale.ENGLISH)) should haveValue(dayCount)
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_relativeYearFraction_defaultMethod") {
    // Reinterpretation. The Java method declared an anonymous subclass of `DayCount` whose
    // `yearFraction` and `days` both answered one, and then asserted that the inherited
    // `relativeYearFraction` answered one forwards and minus one backwards - a test of the
    // default method in isolation from any real rule.
    //
    // `DayCount` is sealed in this port (Rule 4), so the anonymous-subclass form is
    // unrepresentable: every member exists in the companion and no subtype can be declared
    // outside that file. The property the Java method was about is therefore asserted over
    // real members instead - that the relative form agrees with the plain one on dates in
    // order, and negates itself when they are reversed - which is the same statement about the
    // same inherited implementation, made through the only subtypes there are.
    val date1 = date(2015, 6, 1)
    val date2 = date(2015, 7, 1)

    DayCounts.ACT_365F.relativeYearFraction(date1, date2) shouldBe
      DayCounts.ACT_365F.yearFraction(date1, date2)
    DayCounts.ACT_365F.relativeYearFraction(date2, date1) shouldBe
      -DayCounts.ACT_365F.relativeYearFraction(date1, date2)
    DayCounts.ACT_365F.relativeYearFraction(date1, date2) shouldBe (30d / 365d)
    DayCounts.ACT_365F.relativeYearFraction(date2, date1) shouldBe (-30d / 365d)

    // The member the Java stub stood in for most directly: `1/1` answers one whatever the
    // dates, so the two assertions of the Java method are reproduced literally by it.
    DayCounts.ONE_ONE.relativeYearFraction(date1, date2) shouldBe 1d
    DayCounts.ONE_ONE.relativeYearFraction(date2, date1) shouldBe -1d

    // And the whole family, so that no member's inherited default can drift: the relative form
    // is antisymmetric, and it answers for a reversed pair where the plain form refuses.
    forAll(dataTypes) { (dayCount: DayCount) =>
      withClue(s"${dayCount.name}: ") {
        dayCount.relativeYearFraction(date1, date2, wholeYearInfo) shouldBe
          dayCount.yearFraction(date1, date2, wholeYearInfo)
        dayCount.relativeYearFraction(date2, date1, wholeYearInfo) shouldBe
          -dayCount.relativeYearFraction(date1, date2, wholeYearInfo)
        // The contrast that makes the relative form worth having, and a sanctioned refusal of
        // the same kind as `test_wrongOrder`: a reversed pair breaks the documented contract of
        // `yearFraction`, so it is raised through `ArgCheck` rather than returned.
        intercept[IllegalArgumentException](dayCount.yearFraction(date2, date1, wholeYearInfo))
          .getMessage shouldBe DatesOutOfOrderMessage
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_scheduleInfo") {
    // Reinterpretation. The Java method instantiated the bare interface and asserted that the
    // end-of-month flag defaulted to true and that the other four accessors raised
    // `UnsupportedOperationException`. In this port those four are total and answer `None`
    // (AAP 0.6.1): "this schedule does not know" is stated as a value, so an implementation
    // overrides only what it knows and none of them has to raise.
    val test = DayCount.ScheduleInfo.simple
    test.isEndOfMonthConvention shouldBe true
    test.startDate shouldBe None
    test.endDate shouldBe None
    test.frequency shouldBe None
    test.periodEndDate(JAN_01) shouldBe None

    // What that change does *not* do is make a day count answer without the facts its rule is
    // defined in terms of. A caller handing such a convention a schedule that cannot supply
    // them has broken the contract of the call - which is what the Java
    // `UnsupportedOperationException` said too - so it is still a refusal raised through
    // `ArgCheck`, and this is the inventory of it:
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

    // The second refusal of the same kind, and the one a schedule reaches with '''every''' fact
    // present. `Act/Act ICMA` divides each nominal period by the number of events the schedule's
    // frequency has in a year, so a frequency that has no whole number of them - `P5M`, which no
    // whole number of periods fills a year with - describes a schedule the convention is not
    // defined over. The frequency type reports that as a value, because for its own callers it
    // depends on data; here it is the contract of the call, so it is raised through `ArgCheck`
    // carrying the frequency's own message, which is what the implementation being ported threw.
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

    // The control that keeps the assertion above about the frequency and not about the fixture:
    // the same schedule shape with a frequency that does divide the year answers. Two six-month
    // events fill the year from `JAN_01`, the period measured is the first of them, and its 181
    // days over the 362 the two nominal periods span is exactly half a year.
    val sixMonthlyInfo = Info(Some(JAN_01), Some(JAN_01_NEXT), Some(JAN_01_NEXT), false, Some(Frequency.P6M))
    Frequency.P6M.eventsPerYear shouldBe Right(2)
    DayCounts.ACT_ACT_ICMA.yearFraction(JAN_01, JUL_01, sixMonthlyInfo) shouldBe 0.5d

    // The 17 members that read nothing calculate against it, and so do the two that read only
    // what it always carries or only in a case these dates avoid.
    DayCounts.ACT_365F.yearFraction(JAN_01, JUL_01, test) shouldBe (181d / 365d)
    DayCounts.THIRTY_U_360.yearFraction(JAN_01, JUL_01, test) shouldBe (180d / 360d)
    DayCounts.THIRTY_E_360_ISDA.yearFraction(JAN_01, JUL_01, test) shouldBe (180d / 360d)

    // And the schedule information carried by the fixtures of this spec is the same contract
    // seen from the other side: an implementation that answers `Some` for what it knows.
    wholeYearInfo.startDate shouldBe Some(JAN_01)
    wholeYearInfo.endDate shouldBe Some(JAN_01_NEXT)
    wholeYearInfo.periodEndDate(JAN_01) shouldBe Some(JAN_01_NEXT)
    // The Java fixture returned its single period end date for *any* date asked about, and that
    // is reproduced exactly, because the worked ICMA examples depend on it.
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
    // The Java method was `coverPrivateConstructor(DayCounts.class)` and
    // `coverEnum(StandardDayCounts.class)`: the first reflectively invoked the private
    // constructor of a static holder and the second read the values of a package-private enum,
    // both so that a coverage tool would not report them as unexercised. A Scala `object` has no
    // constructor to reach and there is no second enum - the members are declared in the
    // companion - so what those calls stood for is asserted directly.

    // The family has exactly 21 standard members, in the declaration order of the enum being
    // ported, which is not the alphabetical order the `Order` instance imposes. The
    // calendar-bearing `Bus/252` conventions are deliberately absent: there is one per holiday
    // calendar, so the set is open and cannot be enumerated - the same line the ported library
    // drew between its two providers.
    DayCount.values.toList shouldBe declarationOrder
    DayCount.values.toList should have size 21
    DayCount.values.toList.distinct should have size 21
    DayCount.values.toList.map(_.name).distinct should have size 21
    DayCount.values.toList.map(_.name) shouldBe standardNames
    DayCount.values.toList.contains(DayCount.ofBus252(StandardHolidayCalendars.BRBD)) shouldBe false

    // Each constant of the holder is the very member of the family, not a copy and not a
    // registry indirection, so a call site reading the constant and one reading the member are
    // indistinguishable - including by reference.
    forAll(dataConstantIdentifiers) { (identifier: String, dayCount: DayCount) =>
      withClue(s"$identifier: ")(declarationOrder.exists(_ eq dayCount) shouldBe true)
    }
    DayCounts.ONE_ONE should be theSameInstanceAs DayCount.ONE_ONE
    DayCounts.ACT_365F should be theSameInstanceAs DayCount.ACT_365F
    DayCounts.THIRTY_360_ISDA should be theSameInstanceAs DayCount.THIRTY_360_ISDA
    DayCounts.THIRTY_E_365 should be theSameInstanceAs DayCount.THIRTY_E_365

    // The holder publishes those 21 and nothing else, which is the other half of what the
    // reflective sweep would have discovered.
    dataConstantIdentifiers.map { case (_, dayCount) => dayCount }.toList shouldBe declarationOrder

    // Every member is reachable by its own name through both entry points, and its three text
    // renderings agree.
    DayCount.values.toList.foreach { dayCount =>
      withClue(s"${dayCount.name}: ") {
        DayCount.valueOf(dayCount.name) shouldBe Some(dayCount)
        DayCount.parse(dayCount.name) should haveValue(dayCount)
        Show[DayCount].show(dayCount) shouldBe dayCount.name
        dayCount.toString shouldBe dayCount.name
      }
    }

    // The companion publishes one equality-bearing instance - an ordering that is also a
    // hashing - so summoning the equality, the hashing or the ordering yields that one value
    // and the three can never disagree. Every ordered pair of members is asked all three.
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

    // The ordering is by name, which is alphabetical rather than the declaration order above,
    // and a concrete pair states the direction so the assertion is not self-referential.
    all.sorted(Order[DayCount].toOrdering).map(_.name) shouldBe all.map(_.name).sorted
    Order[DayCount].compare(DayCounts.ACT_360, DayCounts.ACT_364) should be < 0
    Order[DayCount].compare(DayCounts.ACT_364, DayCounts.ACT_360) should be > 0
    Order[DayCount].compare(DayCounts.ONE_ONE, DayCounts.ONE_ONE) shouldBe 0

    // Equality of a member with something that is not a day count at all is false rather than a
    // type error, which is the contract of `equals` and worth stating once for a sealed family
    // whose members are singletons.
    DayCounts.ACT_360.equals("Act/360") shouldBe false
    DayCounts.ACT_360.equals(DayCounts.ACT_364) shouldBe false
    DayCounts.ACT_360.equals(DayCounts.ACT_360) shouldBe true
  }

  test("test_serialization") {
    // The Java method was `assertSerialization(ACT_364)`: a round trip through Java
    // serialization and through the binary and JSON encodings of the bean library the type
    // belonged to, all three of which read the class back reflectively. None of them is a
    // dependency of this port, and neither Java serialization nor wire compatibility with that
    // library's JSON is in its scope (AAP 0.2.2). What replaced them is the circe codec the
    // companion publishes, so the round trip is asserted through that - over the member the
    // Java method named, and then over all 21.
    //
    // The document of a standard member is the bare canonical name and never an object: that is
    // the single-string form the annotated string conversion of the ported type wrote. The
    // object form belongs to the calendar-bearing `Bus/252` conventions alone, and asserting
    // that it is *not* used here is the point - `Business252DayCountSpec` owns that shape.
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

    // Text that names no day count is rejected by the reader, as is a document of the wrong
    // JSON type - which is what keeps an unrecognised name a decoding failure and not an
    // exception.
    Json.fromString("Rubbish").as[DayCount].isLeft shouldBe true
    Json.fromInt(1).as[DayCount].isLeft shouldBe true
    Json.obj("name" -> Json.fromString("Act/364")).as[DayCount].isLeft shouldBe true

    // The reader is as lenient as `parse`, which is what lets a document written by hand, or by
    // the ported library through one of its external vocabularies, still be read.
    Json.fromString("ACT/364").as[DayCount] shouldBe Right(DayCounts.ACT_364)
    Json.fromString("Actual/Actual (ISDA)").as[DayCount] shouldBe Right(DayCounts.ACT_ACT_ISDA)
  }

  test("test_jodaConvert") {
    // The Java method asserted, for two members, a round trip through the reflective
    // string-conversion library the type was annotated for: the annotated renderer produced one
    // string and the annotated factory read it back as the same value. That library is not a
    // dependency of this port, and its two annotations became `Show` and `DayCount.parse`, so
    // the guarantee is asserted over those - for the two members the Java method named, whose
    // exact text is the contract, and then for all 21.
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
   * Unwraps the outcome of a frequency factory for use as a fixture.
   *
   * The frequencies this spec names as constants need no unwrapping, but the one frequency it
   * builds - the five-month period of `test_scheduleInfo`, which no constant offers because no
   * whole number of such periods fills a year - comes from a factory that reports a period it
   * cannot accept as a value. Threading that outcome through here rather than forcing it with
   * `getOrElse` and a fabricated fallback keeps a mistake in the fixture visible: a period that
   * is not a frequency fails this spec naming its failures, instead of quietly testing some
   * other value.
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
 * The fixtures and the nine transcribed data providers of this spec.
 *
 * The Java class declared its providers as public static methods and its schedule stub as a
 * static nested class; both belong here for the same reason they belonged there - they are
 * fixtures of this spec rather than a published surface - and the object is visible within the
 * `date` test package alone.
 *
 * Every table is a `lazy val`. That is not decoration: the nine providers hold 597 rows between
 * them, and building all of them in one initialiser would put a single method uncomfortably
 * close to the 64KB limit the JVM places on method bytecode. One accessor per table keeps each
 * well inside it.
 */
private[date] object DayCountSpec extends TableDrivenPropertyChecks {

  /** The first of January, the start date of the sanity-check fixtures. */
  val JAN_01: LocalDate = LocalDate.of(2010, 1, 1)

  /** The second of January, used by the equal-date and reversed-date cases. */
  val JAN_02: LocalDate = LocalDate.of(2010, 1, 2)

  /** The first of July, half a year after [[JAN_01]]. */
  val JUL_01: LocalDate = LocalDate.of(2010, 7, 1)

  /** The first of January of the following year, a whole year after [[JAN_01]]. */
  val JAN_01_NEXT: LocalDate = LocalDate.of(2011, 1, 1)

  /** The message the date-order precondition reports, asserted rather than paraphrased. */
  val DatesOutOfOrderMessage: String = "Dates must be in time-line order"

  /**
   * The message a frequency with no whole number of events in a year reports, asserted rather
   * than paraphrased.
   *
   * This is the text `Frequency.eventsPerYear` puts in its failure, which `Act/Act ICMA` raises
   * as it stands rather than wrapping: a caller that supplied `P5M` is told which frequency the
   * convention could not accrue over, and the wording comes from the type that owns the rule.
   */
  val NonIntegralEventsMessage: String = "Unable to calculate events per year: P5M"

  /**
   * The marker the numeric providers use for a row on which no day-of-month adjustment applies.
   *
   * The Java provider stored the boxed not-a-number constant in such a row and the test body
   * compared the row's expectation against it by identity, which worked precisely because
   * not-a-number is equal to nothing including itself. Testing the primitive for not-a-number is
   * that same test: no row of either provider carries a genuine expectation of not-a-number, so
   * the two agree row for row, and [[expectedFraction]] is the single place the branch is taken.
   */
  val SIMPLE_30_360: Double = Double.NaN

  /**
   * The same marker for the day-count provider, whose expectations are whole numbers.
   *
   * Zero, as in the Java provider - and, as there, a row whose transcribed expectation evaluates
   * to zero is therefore also treated as unmarked. That is not a defect being copied blindly:
   * `data_days` contains exactly one such row, `30E/365` from 2012-02-29 to 2016-02-29, written
   * as `calc360Days(2012, 2, 30, 2012, 2, 30)`, and the unadjusted computation the branch
   * substitutes - 1440 - is the answer that rule gives, where the literal zero would not be.
   */
  val SIMPLE_30_360DAYS: Int = 0

  /**
   * The year fraction of a `30/360` rule over two dates whose days-of-month need no adjustment.
   *
   * @param y1  the year of the first date
   * @param m1  the month of the first date
   * @param d1  the day-of-month of the first date, already adjusted where a row adjusts it
   * @param y2  the year of the second date
   * @param m2  the month of the second date
   * @param d2  the day-of-month of the second date, already adjusted where a row adjusts it
   * @return the year fraction, being the day count over 360
   */
  def calc360(y1: Int, m1: Int, d1: Int, y2: Int, m2: Int, d2: Int): Double =
    calc360Days(y1, m1, d1, y2, m2, d2).toDouble / 360d

  /**
   * The day count of a `30/360` rule over two dates whose days-of-month need no adjustment.
   *
   * @param y1  the year of the first date
   * @param m1  the month of the first date
   * @param d1  the day-of-month of the first date, already adjusted where a row adjusts it
   * @param y2  the year of the second date
   * @param m2  the month of the second date
   * @param d2  the day-of-month of the second date, already adjusted where a row adjusts it
   * @return the day count, months being thirty days and years three hundred and sixty
   */
  def calc360Days(y1: Int, m1: Int, d1: Int, y2: Int, m2: Int, d2: Int): Int =
    (y2 - y1) * 360 + (m2 - m1) * 30 + (d2 - d1)

  /**
   * Resolves the expectation of a year-fraction row, expanding the marker where it carries one.
   *
   * This is the branch the Java test bodies made, reproduced rather than expanded: see
   * [[SIMPLE_30_360]] for why the two forms of the test agree.
   *
   * @param value  the expectation the row carries, or [[SIMPLE_30_360]]
   * @param y1  the year of the first date
   * @param m1  the month of the first date
   * @param d1  the day-of-month of the first date
   * @param y2  the year of the second date
   * @param m2  the month of the second date
   * @param d2  the day-of-month of the second date
   * @return the year fraction the row expects
   */
  def expectedFraction(value: Double, y1: Int, m1: Int, d1: Int, y2: Int, m2: Int, d2: Int): Double =
    if (value.isNaN) calc360(y1, m1, d1, y2, m2, d2) else value

  /**
   * Resolves the expectation of a day-count row, expanding the marker where it carries one.
   *
   * @param value  the expectation the row carries, or [[SIMPLE_30_360DAYS]]
   * @param y1  the year of the first date
   * @param m1  the month of the first date
   * @param d1  the day-of-month of the first date
   * @param y2  the year of the second date
   * @param m2  the month of the second date
   * @param d2  the day-of-month of the second date
   * @return the day count the row expects
   */
  def expectedDays(value: Int, y1: Int, m1: Int, d1: Int, y2: Int, m2: Int, d2: Int): Int =
    if (value == SIMPLE_30_360DAYS) calc360Days(y1, m1, d1, y2, m2, d2) else value

  //-------------------------------------------------------------------------
  /**
   * The schedule stub of the Java test class, field for field.
   *
   * Two things about it are deliberate and load-bearing. The first is that `periodEndDate`
   * answers the one date it holds for '''any''' date asked about, ignoring its argument exactly
   * as the Java stub did: the worked `Act/Act ICMA` examples supply a period end that is not the
   * end of the period containing every date they pass, and they depend on getting it back
   * regardless. The second is that each field the Java stub could hold as an absent reference is
   * an `Option` here, so a Java `null` is written as `None` and nothing has to raise to report
   * that the schedule does not know.
   *
   * @param startDate  the start date of the schedule, where the fixture declares one
   * @param endDate  the end date of the schedule, where the fixture declares one
   * @param periodEnd  the period end date returned for every date, where the fixture declares one
   * @param isEndOfMonthConvention  whether the end-of-month convention is in use
   * @param frequency  the frequency of the schedule, where the fixture declares one
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

  /**
   * The second construction shape of the Java stub: the end-of-month flag and nothing else.
   *
   * Called positionally, as the Java constructor was, because naming an argument of an
   * overloaded application is a restriction this port has no reason to test.
   */
  object Info {

    /**
     * Obtains a schedule stub that carries the end-of-month flag and no dates or frequency.
     *
     * @param eom  whether the end-of-month convention is in use
     * @return the schedule stub
     */
    def apply(eom: Boolean): Info = Info(None, None, None, eom, None)
  }

  /**
   * The schedule of one annual period from [[JAN_01]] to [[JAN_01_NEXT]], with the end-of-month
   * convention not in use.
   *
   * This is the fixture the five `data_types` tests of the Java class built inline, and it is
   * the one that supplies every fact any member reads, which is what lets those tests drive all
   * 21 members through the three-argument overload without a member refusing.
   */
  lazy val wholeYearInfo: Info =
    Info(Some(JAN_01), Some(JAN_01_NEXT), Some(JAN_01_NEXT), false, Some(Frequency.P12M))

  //-------------------------------------------------------------------------
  /**
   * The Java `data_name` provider: each of the 21 standard members with the name it renders as.
   *
   * The provider lists the members in the declaration order of the enum being ported, which is
   * why [[declarationOrder]] and [[standardNames]] are read from it: doing so keeps the order
   * this spec asserts `DayCount.values` against a transcription of the Java source rather than a
   * restatement of the Scala source.
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

  /** The 21 standard members in the declaration order the Java provider lists them in. */
  lazy val declarationOrder: List[DayCount] = dataName.map { case (dayCount, _) => dayCount }.toList

  /** The 21 canonical names in that same order. */
  lazy val standardNames: List[String] = dataName.map { case (_, name) => name }.toList

  /**
   * The Java `data_types` provider: the members of the enum being ported.
   *
   * The Java method read `StandardDayCounts.values()`, the package-private enum that declared
   * the 21 standard members. Its counterpart is [[declarationOrder]], read from the name
   * provider above. The calendar-bearing `Bus/252` conventions are not members of it - in Java
   * they came from a second provider, and here they are built on demand from a calendar - so
   * they are absent from these five tests exactly as they were absent from the Java ones.
   */
  lazy val dataTypes: TableFor1[DayCount] = Table("dayCount", declarationOrder: _*)

  /**
   * The identifiers the `DayCounts` constants holder publishes, paired with the member each
   * names, in declaration order.
   *
   * This is the table that replaces the reflective sweep of the Java `test_lenientLookup_
   * constants`: the field names that method discovered by reflection are written out, and the
   * list being exactly the members is asserted in `coverage`. Each identifier resolves through
   * the lenient chain because the configuration resource declared a row for it, which is why
   * this table doubles as the statement that those 21 rows survived transcription.
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
   * The replacement of each of the 67 lenient rewrite rules, in the order the configuration
   * resource of the ported library listed them.
   *
   * Transcribed from that resource rather than from the Scala transcription of it, so that
   * `test_extendedEnum` compares the port against the original and not against itself. The
   * order is the data: `parse` folds its input to upper case and then applies every rule in
   * turn, a rule whose expression matches the whole of the current text replacing that text, so
   * a later rule sees what an earlier one produced.
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
   * The Java `data_yearFraction` provider, all 201 rows, in the order the provider listed them.
   *
   * Each row is a member, the two dates as year, month and day, and the year fraction expected -
   * or [[SIMPLE_30_360]] where the `30/360` rule of that member needs no day-of-month
   * adjustment and the unadjusted computation is the expectation. The expectations are written
   * as the arithmetic that produces them, `4d / 365d + 58d / 366d` rather than a decimal, which
   * is both the Java form and the only form that states the rule being asserted.
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

    //-------------------------------------------------------
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2012, 2, 28, (4d / 365d + 58d / 366d)),
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2012, 2, 29, (4d / 365d + 59d / 366d)),
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2012, 3, 1, (4d / 365d + 60d / 366d)),
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2016, 2, 28, (4d / 365d + 58d / 366d + 4d)),
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2016, 2, 29, (4d / 365d + 59d / 366d + 4d)),
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2016, 3, 1, (4d / 365d + 60d / 366d + 4d)),
    (DayCounts.ACT_ACT_ISDA, 2012, 2, 29, 2012, 3, 29, 29d / 366d),
    (DayCounts.ACT_ACT_ISDA, 2012, 2, 29, 2012, 3, 28, 28d / 366d),
    (DayCounts.ACT_ACT_ISDA, 2012, 3, 1, 2012, 3, 28, 27d / 366d),

    //-------------------------------------------------------
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2012, 2, 28, (62d / 365d)),
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2012, 2, 29, (63d / 365d)),
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2012, 3, 1, (64d / 366d)),
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2016, 2, 28, (62d / 365d) + 4d),
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2016, 2, 29, (63d / 365d) + 4d),
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2016, 3, 1, (64d / 366d) + 4d),
    (DayCounts.ACT_ACT_AFB, 2012, 2, 28, 2012, 3, 28, 29d / 366d),
    (DayCounts.ACT_ACT_AFB, 2012, 2, 29, 2012, 3, 28, 28d / 366d),
    (DayCounts.ACT_ACT_AFB, 2012, 3, 1, 2012, 3, 28, 27d / 365d),

    //-------------------------------------------------------
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

    //-------------------------------------------------------
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2012, 2, 28, (62d / 365d)),
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2012, 2, 29, (63d / 366d)),
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2012, 3, 1, (64d / 366d)),
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2016, 2, 28, ((62d + 366d + 365d + 365d + 365d) / 366d)),
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2016, 2, 29, ((63d + 366d + 365d + 365d + 365d) / 366d)),
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2016, 3, 1, ((64d + 366d + 365d + 365d + 365d) / 366d)),
    (DayCounts.ACT_365_ACTUAL, 2012, 2, 28, 2012, 3, 28, 29d / 366d),
    (DayCounts.ACT_365_ACTUAL, 2012, 2, 29, 2012, 3, 28, 28d / 365d),
    (DayCounts.ACT_365_ACTUAL, 2012, 3, 1, 2012, 3, 28, 27d / 365d),

    //-------------------------------------------------------
    (DayCounts.ACT_360, 2011, 12, 28, 2012, 2, 28, (62d / 360d)),
    (DayCounts.ACT_360, 2011, 12, 28, 2012, 2, 29, (63d / 360d)),
    (DayCounts.ACT_360, 2011, 12, 28, 2012, 3, 1, (64d / 360d)),
    (DayCounts.ACT_360, 2011, 12, 28, 2016, 2, 28, ((62d + 366d + 365d + 365d + 365d) / 360d)),
    (DayCounts.ACT_360, 2011, 12, 28, 2016, 2, 29, ((63d + 366d + 365d + 365d + 365d) / 360d)),
    (DayCounts.ACT_360, 2011, 12, 28, 2016, 3, 1, ((64d + 366d + 365d + 365d + 365d) / 360d)),
    (DayCounts.ACT_360, 2012, 2, 28, 2012, 3, 28, 29d / 360d),
    (DayCounts.ACT_360, 2012, 2, 29, 2012, 3, 28, 28d / 360d),
    (DayCounts.ACT_360, 2012, 3, 1, 2012, 3, 28, 27d / 360d),

    //-------------------------------------------------------
    (DayCounts.ACT_364, 2011, 12, 28, 2012, 2, 28, (62d / 364d)),
    (DayCounts.ACT_364, 2011, 12, 28, 2012, 2, 29, (63d / 364d)),
    (DayCounts.ACT_364, 2011, 12, 28, 2012, 3, 1, (64d / 364d)),
    (DayCounts.ACT_364, 2011, 12, 28, 2016, 2, 28, ((62d + 366d + 365d + 365d + 365d) / 364d)),
    (DayCounts.ACT_364, 2011, 12, 28, 2016, 2, 29, ((63d + 366d + 365d + 365d + 365d) / 364d)),
    (DayCounts.ACT_364, 2011, 12, 28, 2016, 3, 1, ((64d + 366d + 365d + 365d + 365d) / 364d)),
    (DayCounts.ACT_364, 2012, 2, 28, 2012, 3, 28, 29d / 364d),
    (DayCounts.ACT_364, 2012, 2, 29, 2012, 3, 28, 28d / 364d),
    (DayCounts.ACT_364, 2012, 3, 1, 2012, 3, 28, 27d / 364d),

    //-------------------------------------------------------
    (DayCounts.ACT_365F, 2011, 12, 28, 2012, 2, 28, (62d / 365d)),
    (DayCounts.ACT_365F, 2011, 12, 28, 2012, 2, 29, (63d / 365d)),
    (DayCounts.ACT_365F, 2011, 12, 28, 2012, 3, 1, (64d / 365d)),
    (DayCounts.ACT_365F, 2011, 12, 28, 2016, 2, 28, ((62d + 366d + 365d + 365d + 365d) / 365d)),
    (DayCounts.ACT_365F, 2011, 12, 28, 2016, 2, 29, ((63d + 366d + 365d + 365d + 365d) / 365d)),
    (DayCounts.ACT_365F, 2011, 12, 28, 2016, 3, 1, ((64d + 366d + 365d + 365d + 365d) / 365d)),
    (DayCounts.ACT_365F, 2012, 2, 28, 2012, 3, 28, 29d / 365d),
    (DayCounts.ACT_365F, 2012, 2, 29, 2012, 3, 28, 28d / 365d),
    (DayCounts.ACT_365F, 2012, 3, 1, 2012, 3, 28, 27d / 365d),

    //-------------------------------------------------------
    (DayCounts.ACT_365_25, 2011, 12, 28, 2012, 2, 28, (62d / 365.25d)),
    (DayCounts.ACT_365_25, 2011, 12, 28, 2012, 2, 29, (63d / 365.25d)),
    (DayCounts.ACT_365_25, 2011, 12, 28, 2012, 3, 1, (64d / 365.25d)),
    (DayCounts.ACT_365_25, 2011, 12, 28, 2016, 2, 28, ((62d + 366d + 365d + 365d + 365d) / 365.25d)),
    (DayCounts.ACT_365_25, 2011, 12, 28, 2016, 2, 29, ((63d + 366d + 365d + 365d + 365d) / 365.25d)),
    (DayCounts.ACT_365_25, 2011, 12, 28, 2016, 3, 1, ((64d + 366d + 365d + 365d + 365d) / 365.25d)),
    (DayCounts.ACT_365_25, 2012, 2, 28, 2012, 3, 28, 29d / 365.25d),
    (DayCounts.ACT_365_25, 2012, 2, 29, 2012, 3, 28, 28d / 365.25d),
    (DayCounts.ACT_365_25, 2012, 3, 1, 2012, 3, 28, 27d / 365.25d),

    //-------------------------------------------------------
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

    //-------------------------------------------------------
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

    //-------------------------------------------------------
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

    //-------------------------------------------------------
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

    //-------------------------------------------------------
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

    //-------------------------------------------------------
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

    //-------------------------------------------------------
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
   * The Java `data_days` provider, all 185 rows, in the order the provider listed them.
   *
   * As above, with the day count expected in place of the year fraction and
   * [[SIMPLE_30_360DAYS]] as the marker.
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

    //-------------------------------------------------------
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2012, 2, 28, 62),
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2012, 2, 29, 63),
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2012, 3, 1, 64),
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2016, 2, 28, 1523),
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2016, 2, 29, 1524),
    (DayCounts.ACT_ACT_ISDA, 2011, 12, 28, 2016, 3, 1, 1525),

    //-------------------------------------------------------
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2012, 2, 28, 62),
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2012, 2, 29, 63),
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2012, 3, 1, 64),
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2016, 2, 28, 1523),
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2016, 2, 29, 1524),
    (DayCounts.ACT_ACT_AFB, 2011, 12, 28, 2016, 3, 1, 1525),

    //-------------------------------------------------------
    (DayCounts.ACT_ACT_YEAR, 2011, 12, 28, 2012, 2, 28, 62),
    (DayCounts.ACT_ACT_YEAR, 2011, 12, 28, 2012, 2, 29, 63),
    (DayCounts.ACT_ACT_YEAR, 2011, 12, 28, 2012, 3, 1, 64),
    (DayCounts.ACT_ACT_YEAR, 2011, 12, 28, 2016, 2, 28, 1523),
    (DayCounts.ACT_ACT_YEAR, 2011, 12, 28, 2016, 2, 29, 1524),
    (DayCounts.ACT_ACT_YEAR, 2011, 12, 28, 2016, 3, 1, 1525),

    //-------------------------------------------------------
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2012, 2, 28, 62),
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2012, 2, 29, 63),
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2012, 3, 1, 64),
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2016, 2, 28, 62 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2016, 2, 29, 63 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_365_ACTUAL, 2011, 12, 28, 2016, 3, 1, 64 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_365_ACTUAL, 2012, 2, 28, 2012, 3, 28, 29),
    (DayCounts.ACT_365_ACTUAL, 2012, 2, 29, 2012, 3, 28, 28),
    (DayCounts.ACT_365_ACTUAL, 2012, 3, 1, 2012, 3, 28, 27),

    //-------------------------------------------------------
    (DayCounts.ACT_360, 2011, 12, 28, 2012, 2, 28, 62),
    (DayCounts.ACT_360, 2011, 12, 28, 2012, 2, 29, 63),
    (DayCounts.ACT_360, 2011, 12, 28, 2012, 3, 1, 64),
    (DayCounts.ACT_360, 2011, 12, 28, 2016, 2, 28, 62 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_360, 2011, 12, 28, 2016, 2, 29, 63 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_360, 2011, 12, 28, 2016, 3, 1, 64 + 366 + 365 + 365 + 365),

    //-------------------------------------------------------
    (DayCounts.ACT_364, 2011, 12, 28, 2012, 2, 28, 62),
    (DayCounts.ACT_364, 2011, 12, 28, 2012, 2, 29, 63),
    (DayCounts.ACT_364, 2011, 12, 28, 2012, 3, 1, 64),
    (DayCounts.ACT_364, 2011, 12, 28, 2016, 2, 28, 62 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_364, 2011, 12, 28, 2016, 2, 29, 63 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_364, 2011, 12, 28, 2016, 3, 1, 64 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_364, 2012, 2, 28, 2012, 3, 28, 29),
    (DayCounts.ACT_364, 2012, 2, 29, 2012, 3, 28, 28),
    (DayCounts.ACT_364, 2012, 3, 1, 2012, 3, 28, 27),

    //-------------------------------------------------------
    (DayCounts.ACT_365F, 2011, 12, 28, 2012, 2, 28, 62),
    (DayCounts.ACT_365F, 2011, 12, 28, 2012, 2, 29, 63),
    (DayCounts.ACT_365F, 2011, 12, 28, 2012, 3, 1, 64),
    (DayCounts.ACT_365F, 2011, 12, 28, 2016, 2, 28, 62 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_365F, 2011, 12, 28, 2016, 2, 29, 63 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_365F, 2011, 12, 28, 2016, 3, 1, 64 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_365F, 2012, 2, 28, 2012, 3, 28, 29),
    (DayCounts.ACT_365F, 2012, 2, 29, 2012, 3, 28, 28),
    (DayCounts.ACT_365F, 2012, 3, 1, 2012, 3, 28, 27),

    //-------------------------------------------------------
    (DayCounts.ACT_365_25, 2011, 12, 28, 2012, 2, 28, 62),
    (DayCounts.ACT_365_25, 2011, 12, 28, 2012, 2, 29, 63),
    (DayCounts.ACT_365_25, 2011, 12, 28, 2012, 3, 1, 64),
    (DayCounts.ACT_365_25, 2011, 12, 28, 2016, 2, 28, 62 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_365_25, 2011, 12, 28, 2016, 2, 29, 63 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_365_25, 2011, 12, 28, 2016, 3, 1, 64 + 366 + 365 + 365 + 365),
    (DayCounts.ACT_365_25, 2012, 2, 28, 2012, 3, 28, 29),
    (DayCounts.ACT_365_25, 2012, 2, 29, 2012, 3, 28, 28),
    (DayCounts.ACT_365_25, 2012, 3, 1, 2012, 3, 28, 27),

    //-------------------------------------------------------
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

    //-------------------------------------------------------
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

    //-------------------------------------------------------
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

    //-------------------------------------------------------
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

    //-------------------------------------------------------
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

    //-------------------------------------------------------
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

    //-------------------------------------------------------
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
   * The Java `data_30U360` provider, all 22 rows, in the order the provider listed them.
   *
   * Each row carries two expectations for the same pair of dates: the one that holds when the
   * end-of-month convention is not in use and the one that holds when it is. Four tests are
   * driven from it - `30U/360` with the flag both ways, `30/360 ISDA`, which reads no flag and
   * so answers the first column, and `30U/360 EOM`, which applies the rule unconditionally and
   * so answers the second.
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
   * The Java `data_30E360ISDA` provider, all 19 rows, in the order the provider listed them.
   *
   * Each row carries the expectation for the second date being the maturity of the schedule and
   * for it not being the maturity, which is the one fact `30E/360 ISDA` reads from a schedule.
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
  // The AFB day count is poorly defined, so tests were used to identify a sensible
  // interpretation
  // 1) The ISDA use of "Calculation Period" is a translation of "Periode d'Application"
  // where the original simply meant the period the day count is applied over
  // and NOT the regular periodic schedule (ISDA's definition of "Calculation Period").
  // 2) The ISDA "clarification" for rolling backward does not appear in the original French.
  // The ISDA rule produce strange results (in comments below) which can be avoided.
  // OpenGamma interprets that February 29th should only be chosen if the end date of the period
  // is February 29th and the rolled back date is a leap year.
  // 3) No document indicates precisely when to stop rolling back and treat the remainder as a
  // fraction
  // OpenGamma interprets that rolling back in whole years continues until the remainder
  // is less than one year, and possibly zero if two dates are an exact number of years apart
  // 4) In all cases, the rule has strange effects when interest through a period encounters
  // February 29th and the denominator suddenly changes from 365 to 366 for the rest of the year
  /**
   * The Java `data_ACTACTAFB` provider, all 57 rows, in the order the provider listed them.
   *
   * The commentary above and the per-row commentary below are the Java provider's own, kept
   * because they are the record of how an under-specified rule was interpreted - the rows
   * marked with what the ISDA end-of-February reading would have given are the interpretation
   * decisions themselves, and a row changed without reading them would be a change of
   * behaviour dressed as a correction.
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
    // from Feb28 2003
    (2003, 2, 28, 2005, 2, 27, 1d + (364d / 365d)),
    (2003, 2, 28, 2005, 2, 28, 2d),
    (2003, 2, 28, 2005, 3, 1, 2d + (1d / 365d)),
    (2003, 2, 28, 2008, 2, 27, 4d + (364d / 365d)),
    (2003, 2, 28, 2008, 2, 28, 5d),
    (2003, 2, 28, 2008, 2, 29, 5d),
    (2003, 2, 28, 2008, 3, 1, 5d + (1d / 365d)),
    // from Feb28 2004
    (2004, 2, 28, 2005, 2, 27, (365d / 366d)),
    (2004, 2, 28, 2005, 2, 28, 1d),
    (2004, 2, 28, 2005, 3, 1, 1d + (2d / 366d)),
    (2004, 2, 28, 2008, 2, 27, 3d + (365d / 366d)),
    (2004, 2, 28, 2008, 2, 28, 4d),  // ISDA end-of-February would give (4d + (1d / 365d))
    (2004, 2, 28, 2008, 2, 29, 4d + (1d / 365d)),
    (2004, 2, 28, 2008, 3, 1, 4d + (2d / 366d)),
    // from Feb29 2004
    (2004, 2, 29, 2005, 2, 28, 365d / 366d),
    (2004, 2, 29, 2005, 3, 1, 1d + (1d / 366d)),
    (2004, 2, 29, 2008, 2, 27, 3d + (364d / 366d)),
    (2004, 2, 29, 2008, 2, 28, 3d + (365d / 366d)),  // ISDA end-of-February would give (4d)
    (2004, 2, 29, 2008, 2, 29, 4d),
    (2004, 2, 29, 2008, 3, 1, 4d + (1d / 366d)),
    // from Mar01 2004
    (2004, 3, 1, 2005, 2, 28, 364d / 365d),
    (2004, 3, 1, 2005, 3, 1, 1d),
    (2004, 3, 1, 2008, 2, 27, 3d + (363d / 365d)),
    (2004, 3, 1, 2008, 2, 28, 3d + (364d / 365d)),
    (2004, 3, 1, 2008, 2, 29, 3d + (364d / 365d)),
    (2004, 3, 1, 2008, 3, 1, 4d),
    // from Mar01 2003
    (2003, 3, 1, 2005, 2, 27, 1d + (363d / 365d)),
    (2003, 3, 1, 2005, 2, 28, 1d + (364d / 365d)),  // ISDA end-of-February would give (2d)
    (2003, 3, 1, 2005, 3, 1, 2d),
    (2003, 3, 1, 2008, 2, 27, 4d + (363d / 365d)),  // ISDA end-of-February would give (5d)
    (2003, 3, 1, 2008, 2, 28, 4d + (364d / 365d)),
    (2003, 3, 1, 2008, 2, 29, 5d),
    (2003, 3, 1, 2008, 3, 1, 5d),

    // examples over one year, up to a fixed end date (not relevant in real life)
    // up to Mar01 from leap year
    (2004, 2, 28, 2006, 3, 1, 2d + (2d / 366d)),
    (2004, 2, 29, 2006, 3, 1, 2d + (1d / 366d)),
    (2004, 3, 1, 2006, 3, 1, 2d),
    // up to Mar01 from non leap year
    (2005, 2, 28, 2007, 3, 1, 2d + (1d / 365d)),
    (2005, 3, 1, 2007, 3, 1, 2d),
    // up to Feb28 in leap year from leap year
    (2004, 2, 27, 2008, 2, 28, 4d + (1d / 365d)),  // ISDA end-of-February would give (4d + (2d / 365d))
    (2004, 2, 28, 2008, 2, 28, 4d),  // ISDA end-of-February would give (4d + (1d / 365d))
    (2004, 2, 29, 2008, 2, 28, 3d + (365d / 366d)),  // ISDA end-of-February would give (4d)
    (2004, 3, 1, 2008, 2, 28, 3d + (364d / 365d)),
    // up to Feb28 in leap year from non leap year
    (2006, 2, 27, 2008, 2, 28, 2d + (1d / 365d)),
    (2006, 2, 28, 2008, 2, 28, 2d),
    (2006, 3, 1, 2008, 2, 28, 1d + (364d / 365d)),
    // up to Feb29 in leap year from leap year
    (2004, 2, 28, 2008, 2, 29, 4d + (1d / 365d)),
    (2004, 2, 29, 2008, 2, 29, 4d),
    (2004, 3, 1, 2008, 2, 29, 3d + (364d / 365d)),
    // up to Feb29 in leap year from non leap year
    (2006, 2, 27, 2008, 2, 29, 2d + (1d / 365d)),
    (2006, 2, 28, 2008, 2, 29, 2d),
    (2006, 3, 1, 2008, 2, 29, 1d + (364d / 365d))
  )

  /**
   * The Java `data_ACT365L` provider, all 12 rows, in the order the provider listed them.
   *
   * Each row carries the two dates, the frequency of the schedule and the end of the schedule
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
   * The Java `data_lenient` provider, all 80 rows, in the order the provider listed them.
   *
   * Every row is a spelling that the ordered chain of 67 rewrites turns into a canonical name:
   * the long and short spellings of `Actual`, the bracketed and dotted qualifier forms, the
   * `ISMA` spelling of `ICMA`, a shelf of market nicknames, and the screaming-snake spellings
   * of the constant identifiers. Two rows appear twice in the Java provider - `Actual/Actual
   * ISDA` and `Act/Act` - and both are kept, because this table is a transcription and a
   * de-duplicated table would no longer be one.
   *
   * The last row is the one that does not name a member of the closed family: `BUS/252` resolves
   * to the calendar-bearing convention over the Brazilian calendar, which the resource declared
   * as the default for that spelling.
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
