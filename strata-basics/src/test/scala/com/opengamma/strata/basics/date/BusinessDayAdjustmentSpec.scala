/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.LocalDate

import cats.Hash
import cats.Show

import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[BusinessDayAdjustment]], ported from the Java `BusinessDayAdjustmentTest`.
 *
 * Every method of the Java class is kept under the name it had, so the method-level
 * traceability recorded in `manifest/java-test-mapping.csv` stays one-to-one: `test_basics`,
 * `test_adjustDate`, `test_noAdjust_constant`, `test_noAdjust_factory`,
 * `test_noAdjust_normalized`, `coverage` and `coverage_builder`. The Java `test_serialization`
 * is deliberately absent - the manifest consolidates it into `json.JsonRoundTripSpec` - while
 * the '''shape''' of this type's JSON document is asserted here, in `test_codec`, because a
 * property-based round trip cannot say whether an adjustment is written as an object of two
 * named strings.
 *
 * `test_adjustDate` was parameterised in Java from `BusinessDayConventionTest#data_convention`
 * through a `@MethodSource` naming that other class. That relationship is preserved rather than
 * duplicated: the 72 rows are read from the companion of [[BusinessDayConventionSpec]], so the
 * two specs cannot drift on to different tables.
 *
 * ===How the shape of the port changes the assertions===
 *
 *   - `adjust` and `resolve` return `Either[Failure, _]` where the Java methods returned a bare
 *     value and threw `ReferenceDataNotFoundException` for a calendar the reference data does not
 *     hold. Every ported assertion is therefore an assertion that the outcome is a success
 *     carrying the Java value, and the failure path - which the Java class never exercised - is
 *     asserted in `test_adjustDate_unresolvableCalendar` for all seven conventions.
 *   - Java's `test_adjustDate` asserted two paths, `adjust(date, refData)` and
 *     `resolve(refData).adjust(date)`. This port has a third, `toReader.run(refData)`, which is
 *     the same resolution expressed as a value awaiting reference data, so all three are driven
 *     through every row - reference data supplied once must not change what is computed.
 *   - `coverage` called `coverImmutableBean`, a reflective sweep of a Joda bean's properties,
 *     equality, hashing and rendering. There is no bean and no reflection in this port, so what
 *     the sweep stood for is asserted directly over the same value the Java method swept.
 *   - `coverage_builder` used the generated Joda builder. Construction here is total and the
 *     case class constructor is the whole of it, so the test asserts the three equivalent ways
 *     of building the same value - `of`, `apply` and `copy` - together with the field equality
 *     and the instances a builder-built bean was expected to have.
 *
 * @see [[BusinessDayConventionSpec]] for the rules themselves, applied without reference data
 * @see [[HolidayCalendarIdSpec]] for the resolution this type delegates to
 */
class BusinessDayAdjustmentSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  import BusinessDayConventionSpec.FRI_2014_08_29
  import BusinessDayConventionSpec.MON_2014_07_14
  import BusinessDayConventionSpec.SAT_2014_07_12
  import BusinessDayConventionSpec.SAT_2014_08_30
  import BusinessDayConventionSpec.dataConvention

  /** The reference data the Java class used throughout, which holds every built-in calendar. */
  private val REF_DATA: ReferenceData = ReferenceData.standard

  /** A Saturday in a month whose following Monday is the UK summer bank holiday. */
  private val SAT_2014_08_23: LocalDate = LocalDate.of(2014, 8, 23)

  /** The Tuesday after that bank holiday, which is the next London business day. */
  private val TUE_2014_08_26: LocalDate = LocalDate.of(2014, 8, 26)

  /** An identifier no reference data of this library holds, used to observe the failure path. */
  private val UNKNOWN_CALENDAR: HolidayCalendarId = HolidayCalendarId.of("XXXX")

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
    // The Java parameterised method, driven from the whole 72-row provider of
    // `BusinessDayConventionSpec` against the same `Sat/Sun` calendar it used. Java asserted the
    // unresolved and the resolved path; this port adds the reader, and all three must agree.
    forAll(dataConvention) { (convention: BusinessDayConvention, input: LocalDate, expected: LocalDate) =>
      val test: BusinessDayAdjustment =
        BusinessDayAdjustment.of(convention, HolidayCalendarIds.SAT_SUN)

      withClue(s"$test adjusting $input: ") {
        test.adjust(input, REF_DATA) should haveValue(expected)
        test.resolve(REF_DATA).map(adjuster => adjuster.adjust(input)) should haveValue(expected)
        test.toReader.run(REF_DATA).map(adjuster => adjuster.adjust(input)) should haveValue(expected)
      }
    }
  }

  test("test_adjustDate_realCalendar") {
    // Beyond the Java provider, which uses a weekend-only calendar throughout: adjustment
    // against holiday data that has to be resolved from reference data. Both rows are values
    // captured from the library being ported - the London summer bank holiday of 2014 falls on
    // Monday the 25th, so a Saturday is followed by the Tuesday and the month-end Saturday is
    // held back to the Friday.
    val london: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.GBLO)
    london.adjust(SAT_2014_08_23, REF_DATA) should haveValue(TUE_2014_08_26)

    val londonModified: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.GBLO)
    londonModified.adjust(SAT_2014_08_30, REF_DATA) should haveValue(FRI_2014_08_29)

    // A composite calendar resolves component-wise and observes both sets of holidays, so the
    // Independence Day holiday of New York moves a London business day as well.
    val both: BusinessDayAdjustment =
      BusinessDayAdjustment.of(
        BusinessDayConventions.FOLLOWING,
        HolidayCalendarIds.GBLO.combinedWith(HolidayCalendarIds.USNY))
    both.adjust(LocalDate.of(2019, 7, 4), REF_DATA) should haveValue(LocalDate.of(2019, 7, 5))
    london.adjust(LocalDate.of(2019, 7, 4), REF_DATA) should haveValue(LocalDate.of(2019, 7, 4))
  }

  test("test_adjustDate_resolveBindsTheCalendarOnce") {
    // The resolved adjuster is bound to the calendar it was resolved against and performs no
    // further lookup. What that has to leave unchanged is the answer, so the two paths are
    // compared over every date of a whole year for all seven conventions, against a calendar
    // with real holidays - 2,555 comparisons, which is the statement that supplying reference
    // data once is not a behavioural choice.
    val dates: List[LocalDate] =
      Iterator
        .iterate(LocalDate.of(2015, 1, 1))(date => date.plusDays(1L))
        .takeWhile(date => date.getYear == 2015)
        .toList
    dates should have size 365

    BusinessDayConvention.values.toList.foreach { convention =>
      val test: BusinessDayAdjustment = BusinessDayAdjustment.of(convention, HolidayCalendarIds.GBLO)
      val adjuster = test.resolve(REF_DATA)
      val reader = test.toReader.run(REF_DATA)
      adjuster should beSuccess
      reader should beSuccess
      dates.foreach { date =>
        withClue(s"$test adjusting $date: ") {
          val direct = test.adjust(date, REF_DATA)
          direct should beSuccess
          adjuster.map(bound => bound.adjust(date)) shouldBe direct
          reader.map(bound => bound.adjust(date)) shouldBe direct
        }
      }
    }
  }

  test("test_adjustDate_unresolvableCalendar") {
    // The failure path, which the Java class never exercised because the Java methods threw. For
    // every convention, and through all three entry points, a calendar the reference data cannot
    // supply is reported as `Failure.MissingData` and nothing is raised.
    BusinessDayConvention.values.toList.foreach { convention =>
      val test: BusinessDayAdjustment = BusinessDayAdjustment.of(convention, UNKNOWN_CALENDAR)
      withClue(s"$test: ") {
        test.adjust(MON_2014_07_14, REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
        test.resolve(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
        test.toReader.run(REF_DATA) should beFailureWith(FailureReason.MISSING_DATA)
        noException should be thrownBy test.adjust(MON_2014_07_14, REF_DATA)
      }
    }

    // The failure names the identifier that could not be found, and carries it as an attribute
    // so a caller can act on it without reading the message.
    val failed = BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, UNKNOWN_CALENDAR)
    failed.adjust(MON_2014_07_14, REF_DATA) should
      haveFailureMessageMatching("Reference data not found for identifier 'XXXX'")
    failed.adjust(MON_2014_07_14, REF_DATA).swap.map(failure => failure.attributes.get("id")) shouldBe
      Right(Some("XXXX"))

    // Empty reference data holds nothing at all - not even the no-holidays calendar - so every
    // adjustment fails against it, including `NONE`. That is Java-identical rather than a
    // defect of this port: `ReferenceData.empty()` does not contain `NoHolidays` there either,
    // and `BusinessDayAdjustment.NONE.adjust(date, ReferenceData.empty())` throws in Java.
    BusinessDayConvention.values.toList.foreach { convention =>
      val test: BusinessDayAdjustment = BusinessDayAdjustment.of(convention, HolidayCalendarIds.NO_HOLIDAYS)
      withClue(s"$test against empty reference data: ") {
        test.adjust(SAT_2014_07_12, ReferenceData.empty) should beFailureWith(FailureReason.MISSING_DATA)
      }
    }
    BusinessDayAdjustment.NONE.adjust(SAT_2014_07_12, ReferenceData.empty) should
      beFailureWith(FailureReason.MISSING_DATA)
    ReferenceData.empty.containsValue(HolidayCalendarIds.NO_HOLIDAYS) shouldBe false

    // A composite one part of which is missing names both the part and the composite, which is
    // the failure `HolidayCalendarId` reports and the context a caller needs.
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
    // is by field, so the constant is not privileged.
    test shouldBe BusinessDayAdjustment.NONE
    Hash[BusinessDayAdjustment].eqv(test, BusinessDayAdjustment.NONE) shouldBe true
  }

  test("test_noAdjust_normalized") {
    val test: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.NO_ADJUST, HolidayCalendarIds.SAT_SUN)

    test.convention shouldBe BusinessDayConventions.NO_ADJUST
    test.calendar shouldBe HolidayCalendarIds.SAT_SUN
    // The calendar is kept and named even though the convention never consults it, exactly as in
    // the library being ported: a caller may later replace the convention and expect the
    // calendar to still be there, so this value is deliberately NOT normalised to `NONE`.
    test.toString shouldBe "NoAdjust using calendar Sat/Sun"
    test should not be BusinessDayAdjustment.NONE
    Hash[BusinessDayAdjustment].eqv(test, BusinessDayAdjustment.NONE) shouldBe false

    // It still makes no adjustment, because the convention decides that and not the calendar.
    test.adjust(SAT_2014_07_12, REF_DATA) should haveValue(SAT_2014_07_12)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java method was `coverImmutableBean(BusinessDayAdjustment.of(MODIFIED_FOLLOWING,
    // SAT_SUN))`: a reflective sweep over the bean's properties, equality, hashing and
    // rendering. There is no bean here, so the properties that sweep stood for are asserted over
    // exactly the value the Java method named.
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

    // Equality and hashing are by field, so a value built twice is one value, and a difference
    // in either field is a different value.
    test shouldBe same
    test.hashCode shouldBe same.hashCode
    Hash[BusinessDayAdjustment].eqv(test, same) shouldBe true
    Hash[BusinessDayAdjustment].hash(test) shouldBe Hash[BusinessDayAdjustment].hash(same)
    test should not be otherConvention
    test should not be otherCalendar
    Hash[BusinessDayAdjustment].eqv(test, otherConvention) shouldBe false
    Hash[BusinessDayAdjustment].eqv(test, otherCalendar) shouldBe false

    // A value of another type is not equal to an adjustment, which the reflective sweep also
    // checked by passing it a foreign object.
    test.equals("ModifiedFollowing using calendar Sat/Sun") shouldBe false

    // Rendering, which the sweep read through the bean's `toString`, and the agreement of `Show`
    // with it - the two ways of putting an adjustment into a message must not differ.
    test.toString shouldBe "ModifiedFollowing using calendar Sat/Sun"
    Show[BusinessDayAdjustment].show(test) shouldBe test.toString
    Show[BusinessDayAdjustment].show(BusinessDayAdjustment.NONE) shouldBe "NoAdjust"
  }

  test("coverage_builder") {
    // The Java method built the value through the generated Joda builder. Construction here is
    // total - both fields are required and neither can be absent - so the builder has no
    // counterpart, and the three equivalent ways of building the same value are asserted
    // instead: the factory, the case-class constructor and `copy` of another value.
    val fromFactory: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.SAT_SUN)
    val fromApply: BusinessDayAdjustment =
      BusinessDayAdjustment(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.SAT_SUN)
    val fromCopy: BusinessDayAdjustment =
      BusinessDayAdjustment.NONE.copy(
        convention = BusinessDayConventions.MODIFIED_FOLLOWING,
        calendar = HolidayCalendarIds.SAT_SUN)

    fromFactory.convention shouldBe BusinessDayConventions.MODIFIED_FOLLOWING
    fromFactory.calendar shouldBe HolidayCalendarIds.SAT_SUN
    fromApply shouldBe fromFactory
    fromCopy shouldBe fromFactory
    Hash[BusinessDayAdjustment].hash(fromApply) shouldBe Hash[BusinessDayAdjustment].hash(fromFactory)
    Hash[BusinessDayAdjustment].hash(fromCopy) shouldBe Hash[BusinessDayAdjustment].hash(fromFactory)
    Show[BusinessDayAdjustment].show(fromCopy) shouldBe Show[BusinessDayAdjustment].show(fromFactory)

    // Changing one field at a time through `copy` is what the builder was used for, and each
    // change is visible in the value and in its rendering.
    fromFactory.copy(convention = BusinessDayConventions.PRECEDING).toString shouldBe
      "Preceding using calendar Sat/Sun"
    fromFactory.copy(calendar = HolidayCalendarIds.GBLO).toString shouldBe
      "ModifiedFollowing using calendar GBLO"
  }

  //-------------------------------------------------------------------------
  test("test_codec") {
    // Not a Java method: the manifest consolidates the Java `test_serialization` into
    // `json.JsonRoundTripSpec`, and what that property-based sweep cannot state is the concrete
    // document of this type. The shape required by AAP section 0.6.4 is an object holding the
    // two fields under the names the Java bean declared, in declaration order, each written as
    // the bare string its own type publishes.
    val test: BusinessDayAdjustment =
      BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, HolidayCalendarIds.GBLO)
    test.asJson.noSpaces shouldBe """{"convention":"Following","calendar":"GBLO"}"""
    decode[BusinessDayAdjustment]("""{"convention":"Following","calendar":"GBLO"}""") shouldBe Right(test)
    decode[BusinessDayAdjustment](test.asJson.noSpaces) shouldBe Right(test)
    decode[BusinessDayAdjustment](BusinessDayAdjustment.NONE.asJson.noSpaces) shouldBe
      Right(BusinessDayAdjustment.NONE)

    // Equal values encode to identical bytes, whatever order a composite calendar was built in,
    // because the identifier normalises its own name.
    BusinessDayAdjustment
      .of(BusinessDayConventions.FOLLOWING, HolidayCalendarId.of("USNY+GBLO"))
      .asJson
      .noSpaces shouldBe """{"convention":"Following","calendar":"GBLO+USNY"}"""

    // Both fields are required, so a document missing either is rejected rather than defaulted.
    decode[BusinessDayAdjustment]("""{"convention":"Following"}""").isLeft shouldBe true
    decode[BusinessDayAdjustment]("""{"calendar":"GBLO"}""").isLeft shouldBe true
    decode[BusinessDayAdjustment]("{}").isLeft shouldBe true

    // The convention is read by the name lookup of its own closed family, so text that names no
    // convention is rejected here rather than at resolution time.
    decode[BusinessDayAdjustment]("""{"convention":"Rubbish","calendar":"GBLO"}""").isLeft shouldBe true
    Json.fromString("Following").as[BusinessDayAdjustment].isLeft shouldBe true

    // The calendar, by contrast, accepts any name - a calendar this library knows nothing about
    // is a fact about the reference data rather than about the document - and the value that
    // results fails only when it is resolved.
    val unknown = decode[BusinessDayAdjustment]("""{"convention":"Following","calendar":"XXXX"}""")
    unknown shouldBe Right(BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, UNKNOWN_CALENDAR))
    unknown.map(adjustment => adjustment.adjust(MON_2014_07_14, REF_DATA)) shouldBe
      Right(BusinessDayAdjustment.of(BusinessDayConventions.FOLLOWING, UNKNOWN_CALENDAR)
        .adjust(MON_2014_07_14, REF_DATA))
  }
}
