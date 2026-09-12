/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate
import java.time.Period

import cats.Hash
import cats.Show
import cats.data.NonEmptyChain
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
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[TenorAdjustment]], ported from the Java `TenorAdjustmentTest`.
 *
 * All ten methods of the Java class are kept, each under the name the Java method had -
 * `test_of_additionConventionNone`, `test_of_additionConventionLastDay`, `test_ofLastDay`,
 * `test_ofLastBusinessDay`, `test_of_invalid_conventionForPeriod`, `test_adjust`, `equals`,
 * `test_beanBuilder`, `coverage` and `test_serialization` - so that a Java test method and a
 * test of this suite stay in one-to-one correspondence and the method-level traceability the
 * migration manifest records resolves on the pair of suite class and test name. Nothing is added
 * under a name of its own: everything this port asserts beyond the Java assertions belongs to
 * whichever of the ten methods already owned that ground, which is why `test_adjust` is the long
 * one.
 *
 * ===No `NONE`, and therefore no `test_NONE`===
 *
 * [[PeriodAdjustment]], the period-based sibling of this type, has a `NONE` constant and its
 * Java test has a `test_NONE` method for it. This type has neither, in the library being ported
 * and here, and the reason is a property of a tenor rather than an omission: a tenor is always a
 * positive, non-zero period, so there is no tenor that adds nothing and no adjustment of this
 * type that leaves every date alone. A `test_NONE` here would therefore have no subject, and the
 * suite deliberately does not have one.
 *
 * ===How the shape of the port changes the assertions===
 *
 *   - The three factories return `Either[NonEmptyChain[Failure], TenorAdjustment]` - the type the
 *     library publishes as `ResultNec` - where the Java factories returned a bare value and threw
 *     `IllegalArgumentException` from their validator. Every ported
 *     assertion is therefore that the outcome is a success carrying the Java value, and the four
 *     `assertThatIllegalArgumentException` sites of `test_of_invalid_conventionForPeriod` become
 *     `Left` assertions that name the reason as a value of the closed family of reasons and
 *     check the validator's message character for character.
 *   - `adjust` and `resolve` return `Either[Failure, _]` where the Java methods returned a bare
 *     value and threw `ReferenceDataNotFoundException` for a calendar the reference data does
 *     not hold. That failure path, which the Java class never exercised because it had only an
 *     exception to exercise it with, is asserted at the end of `test_adjust`.
 *   - Java's `test_adjust` asserted two paths, `adjust(date, refData)` and
 *     `resolve(refData).adjust(date)`. This port has a third, `toReader.run(refData)`, which is
 *     the same resolution expressed as a value awaiting reference data, so all three are driven
 *     through every row of the provider: supplying reference data once, or later, or in
 *     composition with another lookup, must not change what is computed.
 *   - `test_beanBuilder` used the generated Joda-Beans builder, which has no counterpart here.
 *     The factories are the whole of construction for a checked type, so the test asserts that
 *     every route to the value produces the same three fields.
 *   - `coverage` called `coverImmutableBean`, a reflective sweep of a Joda bean's properties,
 *     equality, hashing and rendering. There is no bean and no reflection in this port, so what
 *     the sweep stood for is asserted directly over the same value the Java method swept.
 *   - `test_serialization` asserted Joda-Beans binary and JSON round trips. Joda wire
 *     compatibility is out of scope for this port, so the test is a circe round trip through the
 *     codec pair, which additionally pins the concrete document. The property-based round trip
 *     over every codec-bearing type of the module lives in `json.JsonRoundTripSpec`, which is
 *     where the migration manifest records this Java method as consolidated; what a property
 *     cannot state is the document itself, which is what is stated here.
 *
 * @see [[PeriodAdjustmentSpec]] for the period-based sibling of this type
 * @see [[TenorSpec]] for the tenors these adjustments add, and for `isMonthBased` per tenor
 * @see [[PeriodAdditionConventionSpec]] for the addition conventions, and for `isMonthBased` per
 *   convention
 * @see [[BusinessDayAdjustmentSpec]] for the second of the two steps an adjustment performs
 */
class TenorAdjustmentSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /** The reference data the Java class used throughout, which holds every built-in calendar. */
  private val REF_DATA: ReferenceData = ReferenceData.standard

  /**
   * The addition convention that adds the period unchanged, named as the Java class named it.
   *
   * The member is `NONE` rather than `None` because `None` is the empty option of the standard
   * library and a convention of that name could not be referred to without qualification; the
   * name the convention renders and parses as is still `None`, which is what the JSON document
   * of `test_serialization` shows.
   */
  private val PAC_NONE: PeriodAdditionConvention = PeriodAdditionConventions.NONE

  /** The last day of month convention, which is month-based. */
  private val PAC_LAST_DAY: PeriodAdditionConvention = PeriodAdditionConventions.LAST_DAY

  /** The last business day of month convention, which is month-based and reads the calendar. */
  private val PAC_LAST_BUSINESS_DAY: PeriodAdditionConvention =
    PeriodAdditionConventions.LAST_BUSINESS_DAY

  /** The business day adjustment that performs no adjustment, as in the Java class. */
  private val BDA_NONE: BusinessDayAdjustment = BusinessDayAdjustment.NONE

  /** `Following` over the weekend-only calendar, as in the Java class. */
  private val BDA_FOLLOW_SAT_SUN: BusinessDayAdjustment =
    BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.SAT_SUN)

  /**
   * The message the validator reports, character for character.
   *
   * This is the text the Java validator threw with, so a caller that matched on the text of the
   * original failure still matches. It is asserted rather than merely described because the
   * message is part of what the port preserves.
   */
  private val MONTH_BASED_MESSAGE: String =
    "Tenor must not contain days when addition convention is month-based"

  /**
   * The whole failure the validator produces, as a value.
   *
   * The reason is compared as a member of the closed family of reasons and the attributes are
   * empty, so the failure of a rejected pairing is this value and nothing else - which is a
   * stronger statement than either the reason or the message alone.
   */
  private val MONTH_BASED_FAILURE: Failure = Failure.Invalid(MONTH_BASED_MESSAGE)

  /** An identifier no reference data of this library holds, used to observe the failure path. */
  private val UNKNOWN_CALENDAR: HolidayCalendarId = HolidayCalendarId.of("XXXX")

  /**
   * A value of another type, for the equality contract of `equals`.
   *
   * It is typed as `Any` and compared through the `equals` method rather than with `==`, because
   * comparing values of unrelated types with `==` is a compile-time warning in this build and a
   * warning is an error here. The text is the rendering of the adjustment it is compared with,
   * so the assertion also says that an adjustment is not equal to its own description.
   */
  private val ANOTHER_TYPE: Any = "3M with LastDay then apply Following using calendar Sat/Sun"

  /** The period of `test_of_additionConventionNone`: one year, two months and three days. */
  private val MIXED_PERIOD: Period = Period.of(1, 2, 3)

  //-------------------------------------------------------------------------
  // The JSON documents of `test_serialization`, named rather than written inline so that each is
  // one readable line of the shape the codec produces or is asked to read.

  /** The document of the value the Java `test_serialization` serialized. */
  private val LAST_DAY_DOCUMENT: String =
    """{"tenor":"3M","additionConvention":"LastDay","adjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""

  /** The document of the value of `test_of_additionConventionNone`, whose two parts say nothing. */
  private val PLAIN_DOCUMENT: String =
    """{"tenor":"1Y2M3D","additionConvention":"None","adjustment":{"convention":"NoAdjust","calendar":"NoHolidays"}}"""

  /** A document pairing a week tenor with a month-based convention, which the factory rejects. */
  private val INVALID_DOCUMENT: String =
    """{"tenor":"1W","additionConvention":"LastDay","adjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""

  /** A document with no tenor field, which is required. */
  private val MISSING_TENOR_DOCUMENT: String =
    """{"additionConvention":"LastDay","adjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""

  /** A document whose tenor field names no tenor. */
  private val UNKNOWN_TENOR_DOCUMENT: String =
    """{"tenor":"Rubbish","additionConvention":"LastDay","adjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""

  /** A document whose addition convention field names no convention. */
  private val UNKNOWN_CONVENTION_DOCUMENT: String =
    """{"tenor":"3M","additionConvention":"Rubbish","adjustment":{"convention":"Following","calendar":"Sat/Sun"}}"""

  /** A document naming a calendar no reference data holds, which decodes but does not resolve. */
  private val UNKNOWN_CALENDAR_DOCUMENT: String =
    """{"tenor":"3M","additionConvention":"LastDay","adjustment":{"convention":"Following","calendar":"XXXX"}}"""

  //-------------------------------------------------------------------------
  /**
   * Returns the value a factory produced, failing the test if it produced failures instead.
   *
   * Every factory used here reports a rejected input as a chain of failures rather than by
   * throwing, so a test that means to use the resulting value has to say what should happen if
   * there is none. Every call below passes an input the factory is expected to accept, and the
   * answer is therefore that the test fails, naming the messages that came back - which is
   * strictly more informative than the Java original, where an unexpected rejection surfaced as
   * an `IllegalArgumentException` from the line that made the call.
   *
   * The parameter is written as the underlying `Either` rather than through the `ResultNec` alias
   * the library publishes for it, because this spec depends on the failure type alone. The two
   * are the same type, so a factory's outcome is accepted here without conversion.
   *
   * @tparam A  the type of the value the factory produces
   * @param outcome  the outcome of a factory call that is expected to succeed
   * @return the value the factory produced
   */
  private def accepted[A](outcome: Either[NonEmptyChain[Failure], A]): A =
    outcome.fold(
      failures =>
        fail(
          "Expected a value but the factory failed with: " +
            failures.toChain.toList.map(failure => failure.message).mkString(", ")),
      value => value)

  /**
   * Returns the failures a factory produced, failing the test if it produced a value instead.
   *
   * This is the other half of [[accepted]] and exists so that a rejected pairing can be asserted
   * as a whole value - reason, message and attributes - rather than only through a matcher that
   * looks for one failure satisfying a condition.
   *
   * @tparam A  the type of the value the factory would have produced
   * @param outcome  the outcome of a factory call that is expected to fail
   * @return the failures the factory produced, in the order it accumulated them
   */
  private def rejected[A](outcome: Either[NonEmptyChain[Failure], A]): List[Failure] =
    outcome.fold(
      failures => failures.toChain.toList,
      value => fail(s"Expected failures but the factory produced: $value"))

  //-------------------------------------------------------------------------
  /**
   * The `data_adjust` provider of the Java class, transcribed row for row.
   *
   * Each row is a number of months, the date to adjust and the date the library being ported
   * produces for it, using the last day of month addition convention followed by `Following`
   * over the `Sat/Sun` calendar. The two groups are what make the two steps separately
   * observable: in the first the base date is not the last day of its month, so the addition
   * convention adds the period unchanged and only the third row needs the business day
   * convention - the sum lands on Saturday 15 November 2014 and is moved on to Monday the 17th;
   * in the second the base date is the last day of its month, so the addition convention carries
   * the sum to the last day of the target month and the business day convention then has nothing
   * to move.
   */
  private val data_adjust: TableFor3[Int, LocalDate, LocalDate] = Table(
    ("months", "input", "expected"),
    // not last day
    (1, date(2014, 8, 15), date(2014, 9, 15)),
    (2, date(2014, 8, 15), date(2014, 10, 15)),
    (3, date(2014, 8, 15), date(2014, 11, 17)),
    // last day
    (1, date(2014, 2, 28), date(2014, 3, 31)),
    (1, date(2014, 6, 30), date(2014, 7, 31)))

  //-------------------------------------------------------------------------
  test("test_of_additionConventionNone") {
    val tenor: Tenor = accepted(Tenor.of(MIXED_PERIOD))
    val outcome: Either[NonEmptyChain[Failure], TenorAdjustment] = TenorAdjustment.of(tenor, PAC_NONE, BDA_NONE)
    outcome should beSuccess
    val test: TenorAdjustment = accepted(outcome)

    // the three properties, read back as they were given
    test.tenor shouldBe tenor
    test.tenor shouldBe accepted(Tenor.of(Period.of(1, 2, 3)))
    test.additionConvention shouldBe PAC_NONE
    test.adjustment shouldBe BDA_NONE

    // Rendering: neither part says anything - the convention adds the period unchanged and the
    // business day adjustment is `NONE` - so the tenor alone is the whole description.
    test.toString shouldBe "1Y2M3D"
    Show[TenorAdjustment].show(test) shouldBe "1Y2M3D"

    // A tenor of years, months and days is not month-based, and this is the pairing that makes
    // such a tenor admissible: the convention that adds the period unchanged has no month rule
    // to apply, so the validator has nothing to object to. The rejecting side of that same rule
    // is `test_of_invalid_conventionForPeriod`.
    tenor.isMonthBased shouldBe false
    PAC_NONE.isMonthBased shouldBe false

    // and the adjustment adds the period as plain calendar arithmetic: 15 August 2014 plus one
    // year, two months and three days is Sunday 18 October 2015, which `NoAdjust` keeps.
    test.adjust(date(2014, 8, 15), REF_DATA) should haveValue(date(2015, 10, 18))
  }

  test("test_of_additionConventionLastDay") {
    val outcome: Either[NonEmptyChain[Failure], TenorAdjustment] =
      TenorAdjustment.of(Tenor.TENOR_3M, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN)
    outcome should beSuccess
    val test: TenorAdjustment = accepted(outcome)

    // the three properties, read back as they were given
    test.tenor shouldBe Tenor.TENOR_3M
    test.additionConvention shouldBe PAC_LAST_DAY
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN

    // Rendering: both parts say something this time, so both appear, in the grammar of the
    // library being ported - the tenor, the addition convention after `with`, and the business
    // day adjustment after `then apply`.
    test.toString shouldBe "3M with LastDay then apply Following using calendar Sat/Sun"
    Show[TenorAdjustment].show(test) shouldBe test.toString

    // A three-month tenor is month-based, which is what admits it under a month-based convention.
    test.tenor.isMonthBased shouldBe true
    PAC_LAST_DAY.isMonthBased shouldBe true
  }

  test("test_ofLastDay") {
    val outcome: Either[NonEmptyChain[Failure], TenorAdjustment] =
      TenorAdjustment.ofLastDay(Tenor.TENOR_3M, BDA_FOLLOW_SAT_SUN)
    outcome should beSuccess
    val test: TenorAdjustment = accepted(outcome)

    test.tenor shouldBe Tenor.TENOR_3M
    test.additionConvention shouldBe PAC_LAST_DAY
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN
    test.toString shouldBe "3M with LastDay then apply Following using calendar Sat/Sun"

    // The convenience factory is the three-argument factory with the convention supplied, so the
    // two do not merely agree - they produce one value, with one hash and one rendering.
    val explicit: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_3M, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN))
    test shouldBe explicit
    test.hashCode shouldBe explicit.hashCode
    Hash[TenorAdjustment].eqv(test, explicit) shouldBe true
    Show[TenorAdjustment].show(test) shouldBe Show[TenorAdjustment].show(explicit)
    outcome should haveValue(explicit)

    // It checks the same pairing as the factory it delegates to, which is asserted in
    // `test_of_invalid_conventionForPeriod` for the tenor that fails it.
    test.adjust(date(2014, 8, 15), REF_DATA) should haveValue(date(2014, 11, 17))
  }

  test("test_ofLastBusinessDay") {
    val outcome: Either[NonEmptyChain[Failure], TenorAdjustment] =
      TenorAdjustment.ofLastBusinessDay(Tenor.TENOR_3M, BDA_FOLLOW_SAT_SUN)
    outcome should beSuccess
    val test: TenorAdjustment = accepted(outcome)

    test.tenor shouldBe Tenor.TENOR_3M
    test.additionConvention shouldBe PAC_LAST_BUSINESS_DAY
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN
    test.toString shouldBe "3M with LastBusinessDay then apply Following using calendar Sat/Sun"

    // As above, the convenience factory names the same value as the three-argument one.
    val explicit: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_3M, PAC_LAST_BUSINESS_DAY, BDA_FOLLOW_SAT_SUN))
    test shouldBe explicit
    test.hashCode shouldBe explicit.hashCode
    Hash[TenorAdjustment].eqv(test, explicit) shouldBe true
    outcome should haveValue(explicit)

    // The two month rules are different conventions and therefore different adjustments, which
    // the rendering shows and which `test_adjust` shows in the dates they produce.
    test should not be accepted(TenorAdjustment.ofLastDay(Tenor.TENOR_3M, BDA_FOLLOW_SAT_SUN))
    PAC_LAST_BUSINESS_DAY.isMonthBased shouldBe true
  }

  //-------------------------------------------------------------------------
  test("test_of_invalid_conventionForPeriod") {
    // Non-vacuity first. The validator fires on `additionConvention.isMonthBased &&
    // !tenor.isMonthBased`, so a case is a case at all only if the tenor is genuinely not
    // month-based and the convention genuinely is. A week tenor and a day tenor are not - a
    // month end has no bearing on a period of seven days - and both month rules are. Choosing a
    // month tenor here would make every assertion below pass for the wrong reason.
    Tenor.TENOR_1W.isMonthBased shouldBe false
    Tenor.TENOR_1D.isMonthBased shouldBe false
    PAC_LAST_DAY.isMonthBased shouldBe true
    PAC_LAST_BUSINESS_DAY.isMonthBased shouldBe true

    // The four Java sites, each of which was an `assertThatIllegalArgumentException`. Nothing is
    // thrown here: the rejection is a value, so each is asserted as a `Left` whose reason is
    // compared as a member of the closed family of reasons and whose message is the one the Java
    // validator threw with, character for character. Asserting the whole failure as a value says
    // in addition that there is exactly one of them and that it carries no attributes.
    val invalid: List[Either[NonEmptyChain[Failure], TenorAdjustment]] = List(
      TenorAdjustment.of(Tenor.TENOR_1W, PAC_LAST_DAY, BDA_NONE),
      TenorAdjustment.of(Tenor.TENOR_1W, PAC_LAST_BUSINESS_DAY, BDA_NONE),
      TenorAdjustment.ofLastDay(Tenor.TENOR_1W, BDA_NONE),
      TenorAdjustment.ofLastBusinessDay(Tenor.TENOR_1W, BDA_NONE))
    invalid should have size 4

    invalid.zipWithIndex.foreach {
      case (outcome, index) =>
        withClue(s"invalid case $index: ") {
          outcome should beFailure
          outcome should beFailureWith(FailureReason.INVALID)
          outcome should haveFailureMessageMatching(MONTH_BASED_MESSAGE)
          rejected(outcome) shouldBe List(MONTH_BASED_FAILURE)
          rejected(outcome).map(failure => failure.message) shouldBe List(MONTH_BASED_MESSAGE)
          rejected(outcome).map(failure => failure.reason) shouldBe List(FailureReason.INVALID)
          rejected(outcome).flatMap(failure => failure.attributes.toList) shouldBe Nil
        }
    }

    // The same rule from the other side: the mixed tenor of `test_of_additionConventionNone` is
    // not month-based either, because it contains days, so it is admissible under the convention
    // with no month rule and rejected under both month rules.
    val mixed: Tenor = accepted(Tenor.of(MIXED_PERIOD))
    mixed.isMonthBased shouldBe false
    TenorAdjustment.of(mixed, PAC_LAST_DAY, BDA_NONE) should beFailureWith(FailureReason.INVALID)
    TenorAdjustment.of(mixed, PAC_LAST_BUSINESS_DAY, BDA_NONE) should
      beFailureWith(FailureReason.INVALID)
    TenorAdjustment.of(mixed, PAC_NONE, BDA_NONE) should beSuccess

    // The admissible side of the boundary, so that the validator is shown to reject the pairing
    // rather than the tenor or the convention on its own: a week tenor under the convention with
    // no month rule, and a month tenor under each month rule and each convenience factory.
    TenorAdjustment.of(Tenor.TENOR_1W, PAC_NONE, BDA_NONE) should beSuccess
    TenorAdjustment.of(Tenor.TENOR_1D, PAC_NONE, BDA_NONE) should beSuccess
    TenorAdjustment.of(Tenor.TENOR_1M, PAC_LAST_DAY, BDA_NONE) should beSuccess
    TenorAdjustment.of(Tenor.TENOR_1M, PAC_LAST_BUSINESS_DAY, BDA_NONE) should beSuccess
    TenorAdjustment.ofLastDay(Tenor.TENOR_1M, BDA_NONE) should beSuccess
    TenorAdjustment.ofLastBusinessDay(Tenor.TENOR_1M, BDA_NONE) should beSuccess

    // The business day adjustment plays no part in the decision, so the same rejected pairing is
    // rejected identically whichever adjustment accompanies it.
    rejected(TenorAdjustment.of(Tenor.TENOR_1W, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN)) shouldBe
      List(MONTH_BASED_FAILURE)

    // Nothing is raised on either side of the boundary: a rejected pairing is reported, not
    // thrown, which is what lets a caller of the factory handle it.
    noException should be thrownBy TenorAdjustment.of(Tenor.TENOR_1W, PAC_LAST_DAY, BDA_NONE)
    noException should be thrownBy TenorAdjustment.ofLastBusinessDay(Tenor.TENOR_1W, BDA_NONE)

    // The Java class had no null-argument assertion to port, and could have none here: the
    // `notNull` family is dropped from `ArgCheck` because a Scala signature states the
    // requirement instead of checking it. All three arguments are required, so omitting one is a
    // compile error rather than a failure a test could observe at run time - which is what these
    // two proofs record, in place of the run-time assertions such a check would have needed.
    assertDoesNotCompile("TenorAdjustment.of(Tenor.TENOR_3M, PeriodAdditionConventions.LAST_DAY)")
    assertDoesNotCompile("TenorAdjustment.ofLastDay(Tenor.TENOR_3M)")
  }

  //-------------------------------------------------------------------------
  test("test_adjust") {
    // The five rows of the Java provider, through all three entry points. Java asserted two -
    // `adjust(date, refData)` and `resolve(refData).adjust(date)` - and this port adds the reader
    // form, which is the same resolution as a value awaiting its reference data: supplying the
    // data now, later, or in a composition must not change what is computed.
    forAll(data_adjust) { (months: Int, input: LocalDate, expected: LocalDate) =>
      val tenor: Tenor = accepted(Tenor.ofMonths(months))
      val test: TenorAdjustment =
        accepted(TenorAdjustment.of(tenor, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN))
      withClue(s"$test adjusting $input: ") {
        val direct: Either[Failure, LocalDate] = test.adjust(input, REF_DATA)
        direct should haveValue(expected)
        test.resolve(REF_DATA).map(adjuster => adjuster.adjust(input)) should haveValue(expected)
        test.toReader.run(REF_DATA).map(adjuster => adjuster.adjust(input)) should
          haveValue(expected)
      }
    }

    // The order of the two steps, and the one calendar both of them read. Friday 29 August 2014
    // is the last *business* day of its month over `Sat/Sun` - the 30th and 31st are the weekend -
    // but it is not the last *day* of it, so the two month rules part company. `LastBusinessDay`
    // consults the calendar, finds the base date is its month's last business day, and carries
    // the sum to the last business day of September, Tuesday the 30th. `LastDay` finds the base
    // date is not its month's last day, leaves the sum where plain arithmetic put it, Monday 29
    // September, and the business day convention then has nothing to move. Step one therefore
    // runs first and reads the calendar, which is what this pair of results observes: no ordering
    // of the two steps other than addition-then-business-day produces both of these dates.
    val lastBusinessDay: TenorAdjustment =
      accepted(TenorAdjustment.ofLastBusinessDay(Tenor.TENOR_1M, BDA_FOLLOW_SAT_SUN))
    val lastDay: TenorAdjustment =
      accepted(TenorAdjustment.ofLastDay(Tenor.TENOR_1M, BDA_FOLLOW_SAT_SUN))
    lastBusinessDay.adjust(date(2014, 8, 29), REF_DATA) should haveValue(date(2014, 9, 30))
    lastDay.adjust(date(2014, 8, 29), REF_DATA) should haveValue(date(2014, 9, 29))

    // One resolved adjuster, two dates. `resolve` looks the calendar up once and binds it into
    // the adjuster it returns, together with the tenor's period and the business day convention,
    // so that a run of dates costs one lookup rather than one per date. What that has to leave
    // unchanged is the answer, and applying a single adjuster to two different dates - one that
    // exercises the last-day rule and one that does not - is what shows what was bound was the
    // calendar and the period rather than anything about a particular date.
    val resolvedOnce: Either[Failure, DateAdjuster] = lastDay.resolve(REF_DATA)
    val readOnce: Either[Failure, DateAdjuster] = lastDay.toReader.run(REF_DATA)
    resolvedOnce should beSuccess
    readOnce should beSuccess
    resolvedOnce.map(adjuster =>
      (adjuster.adjust(date(2014, 2, 28)), adjuster.adjust(date(2014, 6, 30)))) should
      haveValue((date(2014, 3, 31), date(2014, 7, 31)))
    readOnce.map(adjuster =>
      (adjuster.adjust(date(2014, 2, 28)), adjuster.adjust(date(2014, 6, 30)))) should
      haveValue((date(2014, 3, 31), date(2014, 7, 31)))

    // and the same adjuster over a whole year of dates agrees with resolving afresh for each of
    // them, which is the property the binding has to have for the two forms to be interchangeable
    val dates: List[LocalDate] =
      Iterator
        .iterate(date(2014, 1, 1))(day => day.plusDays(1L))
        .takeWhile(day => day.getYear == 2014)
        .toList
    dates should have size 365

    dates.foreach { day =>
      withClue(s"$lastDay adjusting $day: ") {
        val perDate: Either[Failure, LocalDate] = lastDay.adjust(day, REF_DATA)
        perDate should beSuccess
        resolvedOnce.map(adjuster => adjuster.adjust(day)) shouldBe perDate
        readOnce.map(adjuster => adjuster.adjust(day)) shouldBe perDate
      }
    }

    // Two readers compose, which is the reason the reader form exists: several adjustments are
    // assembled while no reference data is available and the data is supplied once, to the
    // composition, rather than to each of them.
    val threeMonths: TenorAdjustment =
      accepted(TenorAdjustment.ofLastDay(Tenor.TENOR_3M, BDA_FOLLOW_SAT_SUN))
    val bothEnds: RefDataReader[(LocalDate, LocalDate)] =
      (lastDay.toReader, threeMonths.toReader).mapN((near, far) =>
        (near.adjust(date(2014, 8, 15)), far.adjust(date(2014, 8, 15))))
    bothEnds.run(REF_DATA) should haveValue((date(2014, 9, 15), date(2014, 11, 17)))

    // The failure path, which the Java class never exercised because its methods threw
    // `ReferenceDataNotFoundException`. A calendar the reference data cannot supply is reported
    // as a missing-data failure through each of the three entry points, and nothing is raised.
    val unknown: TenorAdjustment =
      accepted(
        TenorAdjustment.ofLastDay(
          Tenor.TENOR_1M,
          BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, UNKNOWN_CALENDAR)))
    unknown.adjust(date(2014, 8, 15), REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    unknown.resolve(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    unknown.toReader.run(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
    noException should be thrownBy unknown.adjust(date(2014, 8, 15), REF_DATA)

    // and the failure names the identifier that could not be found, so a caller is told which
    // calendar to supply rather than merely that something was missing
    unknown.adjust(date(2014, 8, 15), REF_DATA) should
      haveFailureMessageMatching("Reference data not found for identifier 'XXXX'")

    // A composition one reader of which cannot resolve fails as a whole, with that reader's
    // failure rather than one about the composition.
    val partlyUnknown: RefDataReader[(LocalDate, LocalDate)] =
      (lastDay.toReader, unknown.toReader).mapN((known, missing) =>
        (known.adjust(date(2014, 8, 15)), missing.adjust(date(2014, 8, 15))))
    partlyUnknown.run(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)

    // Reference data that holds nothing supplies no calendar at all, so even the adjustment that
    // makes no business day adjustment fails against it - the addition convention still needs a
    // calendar to be resolved, because one calendar serves both steps.
    accepted(TenorAdjustment.of(Tenor.TENOR_1M, PAC_NONE, BDA_NONE))
      .adjust(date(2014, 8, 15), ReferenceData.empty) should beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  test("equals") {
    val a: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_3M, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN))
    val b: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_1M, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN))
    val c: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_3M, PAC_NONE, BDA_FOLLOW_SAT_SUN))
    val d: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_3M, PAC_LAST_DAY, BDA_NONE))
    val again: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_3M, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN))

    // the three Java assertions: a difference in any one of the three fields - the tenor, the
    // addition convention, the business day adjustment - is a different value
    a.equals(b) shouldBe false
    a.equals(c) shouldBe false
    a.equals(d) shouldBe false
    Hash[TenorAdjustment].eqv(a, b) shouldBe false
    Hash[TenorAdjustment].eqv(a, c) shouldBe false
    Hash[TenorAdjustment].eqv(a, d) shouldBe false

    // reflexive, and equal by field rather than by identity: the same three parts built twice are
    // one value, with one hash, and the `Hash` instance agrees with `equals` and `hashCode`
    // because it is derived from them
    a.equals(a) shouldBe true
    a shouldBe again
    a.hashCode shouldBe again.hashCode
    Hash[TenorAdjustment].eqv(a, again) shouldBe true
    Hash[TenorAdjustment].hash(a) shouldBe Hash[TenorAdjustment].hash(again)
    Hash[TenorAdjustment].hash(a) shouldBe a.hashCode

    // symmetric, for each of the three differences
    b.equals(a) shouldBe false
    c.equals(a) shouldBe false
    d.equals(a) shouldBe false

    // and transitive over the values that are equal
    again.equals(a) shouldBe true

    // A value of another type is not an adjustment. The comparison goes through the `equals`
    // method and a value typed as `Any`, because comparing unrelated types with `==` is an error
    // in this build; the text compared is this adjustment's own rendering, so the assertion also
    // says that an adjustment is not equal to its description.
    a.equals(ANOTHER_TYPE) shouldBe false
    a.toString shouldBe ANOTHER_TYPE
  }

  //-------------------------------------------------------------------------
  test("test_beanBuilder") {
    // The Java method assembled the value through the generated Joda-Beans builder - `builder()
    // .tenor(..).additionConvention(..).adjustment(..).build()`. That builder has no target in
    // this port: the beans are gone, and a checked type is built by its factories, which is where
    // the pairing of tenor and addition convention is decided. `TenorAdjustment` publishes no
    // `with*` method and no `copy` either, because a change to any one of its three fields has to
    // go back through that check - so the factories are the whole of construction, and what this
    // test asserts is that every route to the value produces the same three fields, which is what
    // the builder test was checking.
    val fromFactory: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_3M, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN))
    val fromConvenience: TenorAdjustment =
      accepted(TenorAdjustment.ofLastDay(Tenor.TENOR_3M, BDA_FOLLOW_SAT_SUN))
    val fromDecoded: Either[io.circe.Error, TenorAdjustment] =
      decode[TenorAdjustment](fromFactory.asJson.noSpaces)

    // the three Java assertions, on the value the builder produced
    fromFactory.tenor shouldBe Tenor.TENOR_3M
    fromFactory.additionConvention shouldBe PAC_LAST_DAY
    fromFactory.adjustment shouldBe BDA_FOLLOW_SAT_SUN

    // field by field, in declaration order, for the other two routes
    fromConvenience.tenor shouldBe fromFactory.tenor
    fromConvenience.additionConvention shouldBe fromFactory.additionConvention
    fromConvenience.adjustment shouldBe fromFactory.adjustment
    fromDecoded.map(adjustment => adjustment.tenor) shouldBe Right(fromFactory.tenor)
    fromDecoded.map(adjustment => adjustment.additionConvention) shouldBe
      Right(fromFactory.additionConvention)
    fromDecoded.map(adjustment => adjustment.adjustment) shouldBe Right(fromFactory.adjustment)

    // and therefore as whole values, in equality, hashing and rendering
    fromConvenience shouldBe fromFactory
    fromDecoded shouldBe Right(fromFactory)
    Hash[TenorAdjustment].hash(fromConvenience) shouldBe Hash[TenorAdjustment].hash(fromFactory)
    Show[TenorAdjustment].show(fromConvenience) shouldBe Show[TenorAdjustment].show(fromFactory)

    // Changing a field is what a builder was used for, and the way to do it here is to build
    // again through the factory - which re-checks the pairing, so a change that breaks it is
    // rejected rather than built. That is the difference from a builder, stated as an assertion:
    // a builder would have accepted the second of these and produced a value no factory would.
    accepted(TenorAdjustment.of(Tenor.TENOR_1M, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN)).tenor shouldBe
      Tenor.TENOR_1M
    TenorAdjustment.of(Tenor.TENOR_1W, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN) should
      beFailureWith(FailureReason.INVALID)

    // Every field is required, as the builder's `build()` required them: there is no default for
    // any of the three, which is stated by the signature rather than checked at run time.
    assertDoesNotCompile("TenorAdjustment.of(Tenor.TENOR_3M)")
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java method was `coverImmutableBean(TenorAdjustment.of(TENOR_3M, LAST_DAY,
    // BDA_FOLLOW_SAT_SUN))`: a reflective sweep over a Joda bean's properties, equality, hashing
    // and rendering. There is no bean and no reflection in this port, so the sweep has no target
    // and the properties it stood for are asserted directly, over exactly the value the Java
    // method named and over a second, distinct value. The sweep's write half - setting a property
    // through the bean - has no counterpart either: this is a checked type with no `copy`, so a
    // changed value is a new construction through the factory, which `test_beanBuilder` covers.
    val test: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_3M, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN))
    val other: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_6M, PAC_LAST_BUSINESS_DAY, BDA_NONE))
    val same: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_3M, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN))

    // the three properties, which the sweep read through the bean's meta-properties
    test.tenor shouldBe Tenor.TENOR_3M
    test.additionConvention shouldBe PAC_LAST_DAY
    test.adjustment shouldBe BDA_FOLLOW_SAT_SUN
    other.tenor shouldBe Tenor.TENOR_6M
    other.additionConvention shouldBe PAC_LAST_BUSINESS_DAY
    other.adjustment shouldBe BDA_NONE

    // and the same three parts read by pattern matching, which a case class with a private
    // constructor still supports because `unapply` is synthesised as it is for any case class
    test match {
      case TenorAdjustment(tenor, additionConvention, adjustment) =>
        tenor shouldBe Tenor.TENOR_3M
        additionConvention shouldBe PAC_LAST_DAY
        adjustment shouldBe BDA_FOLLOW_SAT_SUN
    }

    // equality and hashing, which the sweep checked against a copy and against a foreign object
    test shouldBe same
    test.hashCode shouldBe same.hashCode
    Hash[TenorAdjustment].eqv(test, same) shouldBe true
    Hash[TenorAdjustment].hash(test) shouldBe Hash[TenorAdjustment].hash(same)
    test should not be other
    Hash[TenorAdjustment].eqv(test, other) shouldBe false
    test.equals(ANOTHER_TYPE) shouldBe false

    // rendering, which the sweep read through the bean's `toString`, and the agreement of `Show`
    // with it - the two ways of putting an adjustment into a message must not differ. The second
    // value also covers the branch of the description that omits the business day adjustment,
    // because that adjustment is `NONE`.
    test.toString shouldBe "3M with LastDay then apply Following using calendar Sat/Sun"
    other.toString shouldBe "6M with LastBusinessDay"
    Show[TenorAdjustment].show(test) shouldBe test.toString
    Show[TenorAdjustment].show(other) shouldBe other.toString
  }

  test("test_serialization") {
    // The Java method was `assertSerialization`, a Joda-Beans binary and JSON round trip. Joda
    // wire compatibility is out of scope for this port, so the round trip is the circe codec
    // pair, and the document it produces is pinned here: an object of the three fields under the
    // names the Java bean declared, in declaration order, the tenor and the convention each the
    // bare string its own type publishes and the business day adjustment the object its own codec
    // writes. The property-based round trip over every codec-bearing type of the module lives in
    // `json.JsonRoundTripSpec`; what a property cannot state is the document itself.
    val test: TenorAdjustment =
      accepted(TenorAdjustment.of(Tenor.TENOR_3M, PAC_LAST_DAY, BDA_FOLLOW_SAT_SUN))
    val encoded: Json = test.asJson

    encoded.asObject.map(obj => obj.keys.toList) shouldBe
      Some(List("tenor", "additionConvention", "adjustment"))
    encoded.noSpaces shouldBe LAST_DAY_DOCUMENT

    // No field is optional and the encoder drops absent values in any case, so no null can appear
    // in the document of any adjustment.
    encoded.noSpaces should not include "null"

    // the round trip itself, for that value and for one whose parts both take the omitted form of
    // the rendering - the convention named `None` and the business day adjustment `NONE`
    decode[TenorAdjustment](encoded.noSpaces) shouldBe Right(test)
    val plain: TenorAdjustment =
      accepted(TenorAdjustment.of(accepted(Tenor.of(MIXED_PERIOD)), PAC_NONE, BDA_NONE))
    plain.asJson.noSpaces shouldBe PLAIN_DOCUMENT
    decode[TenorAdjustment](plain.asJson.noSpaces) shouldBe Right(plain)
    plain.asJson.noSpaces should not include "null"

    // Equal values encode to identical bytes whichever factory built them, which is what makes
    // the encoding a function of the value alone.
    accepted(TenorAdjustment.ofLastDay(Tenor.TENOR_3M, BDA_FOLLOW_SAT_SUN)).asJson.noSpaces shouldBe
      encoded.noSpaces
    accepted(TenorAdjustment.ofLastBusinessDay(Tenor.TENOR_3M, BDA_FOLLOW_SAT_SUN)).asJson
      .noSpaces should include("\"additionConvention\":\"LastBusinessDay\"")

    // The decoder routes the payload through the checking factory, so a document that pairs a
    // month-based addition convention with a tenor that is not month-based is a decoding failure
    // carrying the validator's reason rather than a value the factory would never have built.
    val invalid: Either[io.circe.Error, TenorAdjustment] = decode[TenorAdjustment](INVALID_DOCUMENT)
    invalid.isLeft shouldBe true
    val invalidMessage: Option[String] =
      invalid.swap.toOption.collect { case failure: DecodingFailure => failure.message }
    invalidMessage should not be empty
    invalidMessage.getOrElse("") should include(MONTH_BASED_MESSAGE)

    // Every field is required, so a document missing any of the three is rejected rather than
    // defaulted, and an adjustment is an object rather than a string.
    decode[TenorAdjustment]("""{"tenor":"3M","additionConvention":"LastDay"}""").isLeft shouldBe true
    decode[TenorAdjustment](MISSING_TENOR_DOCUMENT).isLeft shouldBe true
    decode[TenorAdjustment]("{}").isLeft shouldBe true
    Json.fromString("3M").as[TenorAdjustment].isLeft shouldBe true

    // The tenor and the convention are read by their own types, so text that names neither a
    // tenor nor a convention is rejected here rather than at adjustment time.
    decode[TenorAdjustment](UNKNOWN_TENOR_DOCUMENT).isLeft shouldBe true
    decode[TenorAdjustment](UNKNOWN_CONVENTION_DOCUMENT).isLeft shouldBe true

    // The calendar, by contrast, accepts any name - a calendar this library knows nothing about is
    // a fact about the reference data rather than about the document - and the value that results
    // fails only when it is adjusted, which is where `test_adjust` observes it.
    val unknownCalendar: Either[io.circe.Error, TenorAdjustment] =
      decode[TenorAdjustment](UNKNOWN_CALENDAR_DOCUMENT)
    unknownCalendar shouldBe Right(
      accepted(
        TenorAdjustment.of(
          Tenor.TENOR_3M,
          PAC_LAST_DAY,
          BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, UNKNOWN_CALENDAR))))
    unknownCalendar
      .map(adjustment => adjustment.adjust(date(2014, 8, 15), REF_DATA).isLeft) shouldBe Right(true)
  }
}
