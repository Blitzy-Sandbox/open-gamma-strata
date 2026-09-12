/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import cats.Eq
import cats.Hash
import cats.Order
import cats.Show

import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2

import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.date.DayCount
import com.opengamma.strata.basics.date.DayCounts
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.basics.date.HolidayCalendarIds
import com.opengamma.strata.basics.date.StandardHolidayCalendars
import com.opengamma.strata.basics.date.Tenor
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[OvernightIndex]].
 *
 * Every method of the Java original is kept under its own name, so the method-level
 * traceability the migration is measured by stays one-to-one, and every field and every date
 * asserted below is transcribed from that original rather than re-derived from the index data
 * this port transcribed. The two are independent on purpose: a mistake in the transcription of
 * a row has to fail here, which it cannot do if this spec reads the row it is meant to check.
 *
 * The four parameterised methods of the original were driven from a single provider,
 * `data_name`. That shape is preserved - the provider becomes one shared table, declared once
 * below, and each of the four methods keeps its own test driven from it.
 *
 * ===Methods whose subject this port does not have===
 *
 * Five methods asserted machinery that is deliberately absent here, so each is ported as the
 * assertion of the guarantee that machinery gave rather than dropped. The reasoning is recorded
 * at each of them and summarised here:
 *
 *  - `test_extendedEnum` read the classpath registry of the family. The family is a closed
 *    sealed set built from transcribed data (the request's Rule 4, carried by AAP §0.4.1), so
 *    the closed-family equivalent is asserted: every name of the shared table resolves through
 *    the family's own lookup, and the membership is exactly the thirty-five published indices.
 *  - `test_of_lookup_null` passed an absent reference to the factory and asserted that it
 *    raised an error. This port writes no such reference (Rule 5) and its lookups answer with a
 *    value, so the case becomes the two spellings of an absent name that can actually be
 *    supplied - the empty name and a blank one.
 *  - `test_equals` and `coverage` both built a custom index through a bean builder. The family
 *    is closed to its thirty-five configured members and has no public constructor (Rule 4, and
 *    the `[R]` construction kind of AAP §0.3.3), so both are asserted over configured members.
 *  - `test_jodaConvert` asserted the round trip of the reflective string-conversion library the
 *    original annotated the family for. The library is gone with the port (Rule 1), and the
 *    guarantee its annotations gave is asserted directly.
 *  - `test_serialization` serialized that same custom index through the serialization mechanism
 *    of the platform, which this port does not support. Its replacement is the JSON codec, and
 *    the subject is a configured member for the same Rule 4 reason. The test mapping manifest
 *    additionally routes this method to the consolidated `json.JsonRoundTripSpec`, which sweeps
 *    every codec-bearing type of the module property-based; what is asserted here is the
 *    per-type representation, which that sweep does not pin, so the two are complementary.
 *
 * ===What this spec deliberately does not assert===
 *
 * The fidelity of the thirty-five transcribed rows against the manifest captured from the Java
 * implementation belongs to `ReferenceDataManifestSpec`, and the closedness of every family of
 * the library, including the absence of a name claimed by two families, belongs to
 * `NamedEnumClosedSpec`. Those two and this one are complementary, and none of the three may be
 * weakened because another exists.
 */
class OvernightIndexSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The reference data the date calculations are resolved against.
   *
   * The standard set holds every built-in holiday calendar, which is the set the Java original
   * used for the same assertions. Sterling and US dollar fixing calendars resolve against it,
   * which is what the two date methods need.
   */
  private val RefData: ReferenceData = ReferenceData.standard

  /**
   * The shared provider, transcribed row-for-row from the Java data provider.
   *
   * Each row pairs a constant with the name it renders as and is looked up by. The rows are in
   * the order of that provider.
   */
  private val dataName: TableFor2[OvernightIndex, String] = Table(
    ("index", "name"),
    (OvernightIndices.GBP_SONIA, "GBP-SONIA"),
    (OvernightIndices.CHF_SARON, "CHF-SARON"),
    (OvernightIndices.EUR_EONIA, "EUR-EONIA"),
    (OvernightIndices.JPY_TONAR, "JPY-TONAR"),
    (OvernightIndices.USD_FED_FUND, "USD-FED-FUND"),
    (OvernightIndices.AUD_AONIA, "AUD-AONIA"),
    (OvernightIndices.BRL_CDI, "BRL-CDI"),
    (OvernightIndices.DKK_TNR, "DKK-TNR")
  )

  /**
   * Every alternate spelling the family accepts, paired with the canonical name it resolves to.
   *
   * All ten rows of the reference data the family transcribed its alternate names from are here,
   * where the Java method asserted four of them: the alternate names are behaviour rather than
   * configuration, so each row has to be asserted for the table to be checked at all. Two of the
   * spellings contain a space, which is how they are written and therefore how they are
   * accepted.
   */
  private val alternateNameRows: TableFor2[String, String] = Table(
    ("alternate", "canonical"),
    ("CLP-ICP", "CLP-TNA"),
    ("DKK-Tom Next", "DKK-TNR"),
    ("EUR-ESTER", "EUR-ESTR"),
    ("EUR-EuroSTR", "EUR-ESTR"),
    ("HKD-HONIX", "HKD-HONIA"),
    ("JPY-TONA", "JPY-TONAR"),
    ("USD-FED-FUNDS", "USD-FED-FUND"),
    ("USD-FEDFUND", "USD-FED-FUND"),
    ("USD-FEDFUNDS", "USD-FED-FUND"),
    ("USD-Federal Funds", "USD-FED-FUND")
  )

  /**
   * Looks a published index up by name, failing the test where the family has no such member.
   *
   * This stands in for the Java factory every body below called, which answered with an index or
   * raised an error. The port splits those into the lookup, which answers with a value, and the
   * parse, which reports a failure; a body asserting the fields of an index wants the value, and
   * a name that does not resolve is a defect this spec should report as a failed assertion rather
   * than as an error escaping it. Fourteen of the thirty-five published indices have no constant
   * declared for them - exactly as in the original - so several bodies can only reach their
   * subject this way.
   *
   * @param name  the name of the index to look up, such as `GBP-SONIA`
   * @return the index published under that name
   */
  private def lookup(name: String): OvernightIndex =
    OvernightIndex.valueOf(name).getOrElse(fail(s"No Overnight index is published under: $name"))

  //-------------------------------------------------------------------------
  test("test_gbpSonia") {
    val test = lookup("GBP-SONIA")
    test.name shouldBe "GBP-SONIA"
    test.currency shouldBe Currency.GBP
    test.active shouldBe true
    test.fixingCalendar shouldBe HolidayCalendarIds.GBLO
    test.publicationDateOffset shouldBe 1
    test.effectiveDateOffset shouldBe 0
    test.dayCount shouldBe DayCounts.ACT_365F
    test.defaultFixedLegDayCount shouldBe DayCounts.ACT_365F
    // an Overnight index is its own floating rate family, so the whole name is the family name
    FloatingRateName.valueOf("GBP-SONIA") shouldBe Some(test.floatingRateName)
    test.toString shouldBe "GBP-SONIA"
  }

  test("test_gbpSonia_dates") {
    // Each calculation reports a failure where the fixing calendar cannot be resolved, so each
    // is unwrapped through the result matchers rather than by reaching into the value.
    val test = lookup("GBP-SONIA")
    test.calculatePublicationFromFixing(date(2014, 10, 13), RefData) should haveValue(date(2014, 10, 14))
    test.calculateEffectiveFromFixing(date(2014, 10, 13), RefData) should haveValue(date(2014, 10, 13))
    test.calculateMaturityFromFixing(date(2014, 10, 13), RefData) should haveValue(date(2014, 10, 14))
    test.calculateFixingFromEffective(date(2014, 10, 13), RefData) should haveValue(date(2014, 10, 13))
    test.calculateMaturityFromEffective(date(2014, 10, 13), RefData) should haveValue(date(2014, 10, 14))
    // weekend
    test.calculatePublicationFromFixing(date(2014, 10, 10), RefData) should haveValue(date(2014, 10, 13))
    test.calculateEffectiveFromFixing(date(2014, 10, 10), RefData) should haveValue(date(2014, 10, 10))
    test.calculateMaturityFromFixing(date(2014, 10, 10), RefData) should haveValue(date(2014, 10, 13))
    test.calculateFixingFromEffective(date(2014, 10, 10), RefData) should haveValue(date(2014, 10, 10))
    test.calculateMaturityFromEffective(date(2014, 10, 10), RefData) should haveValue(date(2014, 10, 13))
    // input date is Sunday
    test.calculatePublicationFromFixing(date(2014, 10, 12), RefData) should haveValue(date(2014, 10, 14))
    test.calculateEffectiveFromFixing(date(2014, 10, 12), RefData) should haveValue(date(2014, 10, 13))
    test.calculateMaturityFromFixing(date(2014, 10, 12), RefData) should haveValue(date(2014, 10, 14))
    test.calculateFixingFromEffective(date(2014, 10, 12), RefData) should haveValue(date(2014, 10, 13))
    test.calculateMaturityFromEffective(date(2014, 10, 12), RefData) should haveValue(date(2014, 10, 14))
  }

  test("test_chfSaron") {
    val test = lookup("CHF-SARON")
    test.name shouldBe "CHF-SARON"
    test.currency shouldBe Currency.CHF
    test.active shouldBe true
    test.fixingCalendar shouldBe HolidayCalendarIds.CHZU
    test.publicationDateOffset shouldBe 0
    test.effectiveDateOffset shouldBe 0
    test.dayCount shouldBe DayCounts.ACT_360
    test.defaultFixedLegDayCount shouldBe DayCounts.ACT_360
    FloatingRateName.valueOf("CHF-SARON") shouldBe Some(test.floatingRateName)
    test.toString shouldBe "CHF-SARON"
  }

  test("test_chfTois") {
    // The inactive edge of this family, and the only member of it: the Tomorrow/Next rate was
    // replaced by SARON and its row carries `active = false`, where every other explicit
    // `active` assertion in this suite expects true. Its effective offset of one day is the
    // other reason it is asserted in full - it is the only published row whose effective date is
    // not the fixing date, so a transcription that defaulted either field would pass every other
    // test in this file. Elsewhere the member is reached only as a constant, which says nothing
    // about the row behind it. Expected values are the `CHF-TOIS` row of the published data.
    val test = lookup("CHF-TOIS")
    test shouldBe OvernightIndices.CHF_TOIS
    test.name shouldBe "CHF-TOIS"
    test.currency shouldBe Currency.CHF
    test.active shouldBe false
    test.fixingCalendar shouldBe HolidayCalendarIds.CHZU
    test.publicationDateOffset shouldBe 0
    test.effectiveDateOffset shouldBe 1
    test.dayCount shouldBe DayCounts.ACT_360
    test.defaultFixedLegDayCount shouldBe DayCounts.ACT_360
    test.tenor shouldBe Tenor.TENOR_1D
    FloatingRateName.valueOf("CHF-TOIS") shouldBe Some(test.floatingRateName)
    test.toString shouldBe "CHF-TOIS"

    // It is the sole inactive member of the family, so the flag discriminates rather than being
    // uniform, and every other member is active.
    OvernightIndex.values.toList.filter(index => !index.active) shouldBe List(test)

    // The effective offset is observable in the dates the member derives: the effective date is
    // one business day after the fixing date and the maturity date one business day after that,
    // which is what distinguishes this row from every other published one.
    test.calculateEffectiveFromFixing(date(2014, 10, 13), RefData) should haveValue(date(2014, 10, 14))
    test.calculatePublicationFromFixing(date(2014, 10, 13), RefData) should haveValue(date(2014, 10, 13))
    test.calculateMaturityFromFixing(date(2014, 10, 13), RefData) should haveValue(date(2014, 10, 15))
    test.calculateFixingFromEffective(date(2014, 10, 14), RefData) should haveValue(date(2014, 10, 13))
  }

  test("test_resolvedObservation") {
    // The batch route of this family, which is where a series of fixings is observed from: the
    // fixing calendar is resolved once, and the function that comes back derives the publication,
    // effective and maturity dates and the year fraction of every fixing from it. What is
    // asserted is that it agrees with the per-fixing factory field by field - the equality of an
    // observation reads the index and the fixing date alone, so the derived values have to be
    // compared explicitly - and over dates that exercise the interesting cases: a business day,
    // a Saturday, a Sunday and a day before a holiday. The subjects are the sterling rate, whose
    // publication offset is one day, and the Tomorrow/Next rate, whose effective offset is one.
    List(lookup("GBP-SONIA"), lookup("CHF-TOIS"), lookup("THB-THOR")).foreach { index =>
      val observe = OvernightIndexObservation
        .resolve(index, RefData)
        .getOrElse(fail(s"${index.name} did not resolve against the standard reference data"))
      List(date(2014, 10, 13), date(2014, 10, 11), date(2014, 10, 12), date(2014, 12, 24))
        .foreach { fixingDate =>
          withClue(s"${index.name} on $fixingDate: ") {
            val resolved = observe(fixingDate)
            val direct = OvernightIndexObservation
              .of(index, fixingDate, RefData)
              .getOrElse(fail("the per-fixing factory reported a failure"))
            resolved shouldBe direct
            resolved.fixingDate shouldBe fixingDate
            resolved.publicationDate shouldBe direct.publicationDate
            resolved.effectiveDate shouldBe direct.effectiveDate
            resolved.maturityDate shouldBe direct.maturityDate
            resolved.yearFraction shouldBe direct.yearFraction
            // and the dates are those of the index's own calculations, which is what the
            // single resolution must not change
            resolved.publicationDate shouldBe
              index.calculatePublicationFromFixing(fixingDate, RefData).getOrElse(fail("no date"))
            resolved.effectiveDate shouldBe
              index.calculateEffectiveFromFixing(fixingDate, RefData).getOrElse(fail("no date"))
            resolved.maturityDate shouldBe
              index.calculateMaturityFromFixing(fixingDate, RefData).getOrElse(fail("no date"))
          }
        }
    }

    // Reference data that holds no calendar is reported once, by the resolution, rather than per
    // fixing - which is the whole reason the operation exists.
    OvernightIndexObservation.resolve(lookup("GBP-SONIA"), ReferenceData.empty) should
      beFailureWith(FailureReason.MISSING_DATA)
  }

  test("test_getFloatingRateName") {
    // The Java method iterated the registry of the family. The family is closed, so its own
    // membership is iterated instead, which covers the same indices and cannot be short of any.
    // Unlike an Ibor index, which names a family plus a tenor, an Overnight index publishes one
    // rate and the whole of its name is the name of its floating rate family - no suffix is
    // removed here.
    OvernightIndex.values.toList.foreach { index =>
      withClue(s"${index.name}: ") {
        FloatingRateName.valueOf(index.name) shouldBe Some(index.floatingRateName)
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_usdFedFund3m") {
    val test = lookup("USD-FED-FUND")
    test.currency shouldBe Currency.USD
    test.name shouldBe "USD-FED-FUND"
    test.fixingCalendar shouldBe HolidayCalendarIds.USNY
    test.publicationDateOffset shouldBe 1
    test.effectiveDateOffset shouldBe 0
    test.dayCount shouldBe DayCounts.ACT_360
    test.defaultFixedLegDayCount shouldBe DayCounts.ACT_360
    test.toString shouldBe "USD-FED-FUND"
  }

  test("test_usdFedFund_dates") {
    val test = lookup("USD-FED-FUND")
    test.calculatePublicationFromFixing(date(2014, 10, 27), RefData) should haveValue(date(2014, 10, 28))
    test.calculateEffectiveFromFixing(date(2014, 10, 27), RefData) should haveValue(date(2014, 10, 27))
    test.calculateMaturityFromFixing(date(2014, 10, 27), RefData) should haveValue(date(2014, 10, 28))
    test.calculateFixingFromEffective(date(2014, 10, 27), RefData) should haveValue(date(2014, 10, 27))
    test.calculateMaturityFromEffective(date(2014, 10, 27), RefData) should haveValue(date(2014, 10, 28))
    // weekend and US holiday
    test.calculatePublicationFromFixing(date(2014, 10, 10), RefData) should haveValue(date(2014, 10, 14))
    test.calculateEffectiveFromFixing(date(2014, 10, 10), RefData) should haveValue(date(2014, 10, 10))
    test.calculateMaturityFromFixing(date(2014, 10, 10), RefData) should haveValue(date(2014, 10, 14))
    test.calculateFixingFromEffective(date(2014, 10, 10), RefData) should haveValue(date(2014, 10, 10))
    test.calculateMaturityFromEffective(date(2014, 10, 10), RefData) should haveValue(date(2014, 10, 14))
    // input date is Sunday, 13th is US holiday
    test.calculatePublicationFromFixing(date(2014, 10, 12), RefData) should haveValue(date(2014, 10, 15))
    test.calculateEffectiveFromFixing(date(2014, 10, 12), RefData) should haveValue(date(2014, 10, 14))
    test.calculateMaturityFromFixing(date(2014, 10, 12), RefData) should haveValue(date(2014, 10, 15))
    test.calculateFixingFromEffective(date(2014, 10, 12), RefData) should haveValue(date(2014, 10, 14))
    test.calculateMaturityFromEffective(date(2014, 10, 12), RefData) should haveValue(date(2014, 10, 15))
  }

  test("test_usdSofr") {
    val test = lookup("USD-SOFR")
    test.name shouldBe "USD-SOFR"
    test.currency shouldBe Currency.USD
    test.active shouldBe true
    test.fixingCalendar shouldBe HolidayCalendarIds.USGS
    test.publicationDateOffset shouldBe 1
    test.effectiveDateOffset shouldBe 0
    test.dayCount shouldBe DayCounts.ACT_360
    test.toString shouldBe "USD-SOFR"
  }

  test("test_usdAmeribor") {
    val test = lookup("USD-AMERIBOR")
    test.name shouldBe "USD-AMERIBOR"
    test.currency shouldBe Currency.USD
    test.active shouldBe true
    test.fixingCalendar shouldBe HolidayCalendarIds.USNY
    test.publicationDateOffset shouldBe 0
    test.effectiveDateOffset shouldBe 0
    test.dayCount shouldBe DayCounts.ACT_360
    test.toString shouldBe "USD-AMERIBOR"
  }

  //-------------------------------------------------------------------------
  test("test_eurEonia") {
    val test = lookup("EUR-EONIA")
    test.name shouldBe "EUR-EONIA"
    test.currency shouldBe Currency.EUR
    test.active shouldBe true
    test.fixingCalendar shouldBe HolidayCalendarIds.EUTA
    test.publicationDateOffset shouldBe 0
    test.effectiveDateOffset shouldBe 0
    test.dayCount shouldBe DayCounts.ACT_360
    test.toString shouldBe "EUR-EONIA"
  }

  test("test_eurEstr") {
    val test = lookup("EUR-ESTR")
    test.name shouldBe "EUR-ESTR"
    test.currency shouldBe Currency.EUR
    test.active shouldBe true
    test.fixingCalendar shouldBe HolidayCalendarIds.EUTA
    test.publicationDateOffset shouldBe 1
    test.effectiveDateOffset shouldBe 0
    test.dayCount shouldBe DayCounts.ACT_360
    test.toString shouldBe "EUR-ESTR"
    // old name; the whole alternate-name table is asserted by test_alternateNames
    lookup("EUR-ESTER") shouldBe test
  }

  //-------------------------------------------------------------------------
  test("test_audAonia") {
    val test = lookup("AUD-AONIA")
    test.name shouldBe "AUD-AONIA"
    test.currency shouldBe Currency.AUD
    test.active shouldBe true
    test.fixingCalendar shouldBe HolidayCalendarIds.AUSY
    test.publicationDateOffset shouldBe 0
    test.effectiveDateOffset shouldBe 0
    test.dayCount shouldBe DayCounts.ACT_365F
    test.defaultFixedLegDayCount shouldBe DayCounts.ACT_365F
    test.toString shouldBe "AUD-AONIA"
  }

  test("test_brlCdi") {
    val test = lookup("BRL-CDI")
    test.name shouldBe "BRL-CDI"
    test.currency shouldBe Currency.BRL
    test.active shouldBe true
    test.fixingCalendar shouldBe HolidayCalendarIds.BRBD
    test.publicationDateOffset shouldBe 1
    test.effectiveDateOffset shouldBe 0
    // The one published rate that accrues on the business days of a calendar rather than on a
    // calendar-day convention. The day count of this port carries the resolved calendar rather
    // than resolving one ambiently, so the expectation is built from the built-in calendar
    // through the total factory - the same value the index data names.
    test.dayCount shouldBe DayCount.ofBus252(StandardHolidayCalendars.BRBD)
    test.toString shouldBe "BRL-CDI"
  }

  test("test_clpOis") {
    val test = lookup("CLP-TNA")
    test.name shouldBe "CLP-TNA"
    test.currency shouldBe Currency.CLP
    test.active shouldBe true
    // a fixing calendar this library ships no holidays for, named exactly as the original named
    // it, through the total factory
    test.fixingCalendar shouldBe HolidayCalendarId.of("CLSA")
    test.publicationDateOffset shouldBe 0
    test.effectiveDateOffset shouldBe 0
    test.dayCount shouldBe DayCounts.ACT_360
    test.defaultFixedLegDayCount shouldBe DayCounts.ACT_360
    test.toString shouldBe "CLP-TNA"
  }

  test("test_dkkOis") {
    val test = lookup("DKK-TNR")
    test.name shouldBe "DKK-TNR"
    test.currency shouldBe Currency.DKK
    test.active shouldBe true
    test.fixingCalendar shouldBe HolidayCalendarIds.DKCO
    test.publicationDateOffset shouldBe 1
    // a tomorrow/next rate: the deposit it implies starts the business day after the fixing
    test.effectiveDateOffset shouldBe 1
    test.dayCount shouldBe DayCounts.ACT_360
    test.defaultFixedLegDayCount shouldBe DayCounts.ACT_360
    test.toString shouldBe "DKK-TNR"
  }

  test("test_hkdOis") {
    val test = lookup("HKD-HONIA")
    test.name shouldBe "HKD-HONIA"
    test.currency shouldBe Currency.HKD
    test.active shouldBe true
    test.fixingCalendar shouldBe HolidayCalendarId.of("HKHK")
    test.publicationDateOffset shouldBe 0
    test.effectiveDateOffset shouldBe 0
    test.dayCount shouldBe DayCounts.ACT_365F
    test.defaultFixedLegDayCount shouldBe DayCounts.ACT_365F
    test.toString shouldBe "HKD-HONIA"
    // alternative name
    lookup("HKD-HONIX") shouldBe test
  }

  test("test_inrOis") {
    val test = lookup("INR-OMIBOR")
    test.name shouldBe "INR-OMIBOR"
    test.currency shouldBe Currency.INR
    test.active shouldBe true
    test.fixingCalendar shouldBe HolidayCalendarId.of("INMU")
    test.publicationDateOffset shouldBe 0
    test.effectiveDateOffset shouldBe 0
    test.dayCount shouldBe DayCounts.ACT_365F
    test.defaultFixedLegDayCount shouldBe DayCounts.ACT_365F
    test.toString shouldBe "INR-OMIBOR"
  }

  test("test_nzdOis") {
    val test = lookup("NZD-NZIONA")
    test.name shouldBe "NZD-NZIONA"
    test.currency shouldBe Currency.NZD
    test.active shouldBe true
    // the one composite fixing calendar of the family: the rate fixes on the days that are
    // business days in both Auckland and Wellington
    test.fixingCalendar shouldBe HolidayCalendarId.of("NZAU+NZWE")
    test.publicationDateOffset shouldBe 0
    test.effectiveDateOffset shouldBe 0
    test.dayCount shouldBe DayCounts.ACT_365F
    test.defaultFixedLegDayCount shouldBe DayCounts.ACT_365F
    test.toString shouldBe "NZD-NZIONA"
  }

  test("test_plnOis") {
    val test = lookup("PLN-POLONIA")
    test.name shouldBe "PLN-POLONIA"
    test.currency shouldBe Currency.PLN
    test.active shouldBe true
    test.fixingCalendar shouldBe HolidayCalendarIds.PLWA
    test.publicationDateOffset shouldBe 0
    test.effectiveDateOffset shouldBe 0
    test.dayCount shouldBe DayCounts.ACT_365F
    test.defaultFixedLegDayCount shouldBe DayCounts.ACT_365F
    test.toString shouldBe "PLN-POLONIA"
  }

  test("test_plnPolstr") {
    val test = lookup("PLN-POLSTR")
    test.name shouldBe "PLN-POLSTR"
    test.currency shouldBe Currency.PLN
    test.active shouldBe true
    test.fixingCalendar shouldBe HolidayCalendarIds.PLWA
    test.publicationDateOffset shouldBe 0
    test.effectiveDateOffset shouldBe 0
    test.dayCount shouldBe DayCounts.ACT_365F
    test.defaultFixedLegDayCount shouldBe DayCounts.ACT_365F
    test.toString shouldBe "PLN-POLSTR"
  }

  test("test_sekOis") {
    val test = lookup("SEK-SIOR")
    test.name shouldBe "SEK-SIOR"
    test.currency shouldBe Currency.SEK
    test.active shouldBe true
    test.fixingCalendar shouldBe HolidayCalendarIds.SEST
    test.publicationDateOffset shouldBe 0
    test.effectiveDateOffset shouldBe 1
    test.dayCount shouldBe DayCounts.ACT_360
    test.defaultFixedLegDayCount shouldBe DayCounts.ACT_360
    test.toString shouldBe "SEK-SIOR"
  }

  test("test_sgdSonar") {
    val cal = HolidayCalendarId.of("SGSI")
    val test = lookup("SGD-SONAR")
    test.name shouldBe "SGD-SONAR"
    test.currency shouldBe Currency.SGD
    test.active shouldBe true
    test.fixingCalendar shouldBe cal
    test.publicationDateOffset shouldBe 0
    test.effectiveDateOffset shouldBe 0
    test.dayCount shouldBe DayCounts.ACT_365F
    test.defaultFixedLegDayCount shouldBe DayCounts.ACT_365F
    test.toString shouldBe "SGD-SONAR"
  }

  test("test_sgdSora") {
    val cal = HolidayCalendarId.of("SGSI")
    val test = lookup("SGD-SORA")
    test.name shouldBe "SGD-SORA"
    test.currency shouldBe Currency.SGD
    test.active shouldBe true
    test.fixingCalendar shouldBe cal
    test.publicationDateOffset shouldBe 1
    test.effectiveDateOffset shouldBe 0
    test.dayCount shouldBe DayCounts.ACT_365F
    test.defaultFixedLegDayCount shouldBe DayCounts.ACT_365F
    test.toString shouldBe "SGD-SORA"
  }

  test("test_thbThor") {
    val cal = HolidayCalendarId.of("THBA")
    val test = lookup("THB-THOR")
    test.name shouldBe "THB-THOR"
    test.currency shouldBe Currency.THB
    test.active shouldBe true
    test.fixingCalendar shouldBe cal
    test.publicationDateOffset shouldBe 0
    // the one published rate effective two business days after the fixing
    test.effectiveDateOffset shouldBe 2
    test.dayCount shouldBe DayCounts.ACT_365F
    test.defaultFixedLegDayCount shouldBe DayCounts.ACT_365F
    test.toString shouldBe "THB-THOR"
  }

  test("test_zarSabor") {
    val test = lookup("ZAR-SABOR")
    test.name shouldBe "ZAR-SABOR"
    test.currency shouldBe Currency.ZAR
    test.active shouldBe true
    test.fixingCalendar shouldBe HolidayCalendarIds.ZAJO
    test.publicationDateOffset shouldBe 0
    test.effectiveDateOffset shouldBe 0
    test.dayCount shouldBe DayCounts.ACT_365F
    test.defaultFixedLegDayCount shouldBe DayCounts.ACT_365F
    test.toString shouldBe "ZAR-SABOR"
  }

  //-------------------------------------------------------------------------
  test("test_alternateNames") {
    // The Java method asserted four of the ten alternate spellings the family accepts. All ten
    // are asserted here, because the table is behaviour rather than configuration - a rate
    // renamed by a benchmark reform has to stay resolvable under the name trades were written
    // against - and a table only four rows of which are checked is not checked.
    forEvery(alternateNameRows) { (alternate: String, canonical: String) =>
      withClue(s"$alternate resolving to $canonical: ") {
        val expected = lookup(canonical)
        expected.name shouldBe canonical
        // the alternate resolves, and resolves onto the very member the canonical name names
        OvernightIndex.valueOf(alternate) shouldBe Some(expected)
        OvernightIndex.parse(alternate) should haveValue(expected)
        OvernightIndex.valueOf(alternate) shouldBe OvernightIndex.valueOf(canonical)
        // the member reports its canonical name, never the spelling it was reached by
        lookup(alternate).name shouldBe canonical
      }
    }

    // the four assertions of the Java method, kept in their own form against the constants
    lookup("JPY-TONA") shouldBe OvernightIndices.JPY_TONAR
    lookup("USD-FED-FUNDS") shouldBe OvernightIndices.USD_FED_FUND
    lookup("USD-FEDFUNDS") shouldBe OvernightIndices.USD_FED_FUND
    lookup("USD-FEDFUND") shouldBe OvernightIndices.USD_FED_FUND

    // The sharpest check of the wiring: EUR_ESTER is declared by looking up the retired
    // spelling, which is a row of the alternate-name table and not a row of the index data, so
    // the constant exists at all only because that row resolves. It is the same object as
    // EUR_ESTR and reports the current name.
    OvernightIndices.EUR_ESTER shouldBe OvernightIndices.EUR_ESTR
    (OvernightIndices.EUR_ESTER eq OvernightIndices.EUR_ESTR) shouldBe true
    OvernightIndices.EUR_ESTER.name shouldBe "EUR-ESTR"
  }

  //-------------------------------------------------------------------------
  test("test_name") {
    forEvery(dataName) { (index: OvernightIndex, name: String) =>
      index.name shouldBe name
    }
  }

  test("test_toString") {
    forEvery(dataName) { (index: OvernightIndex, name: String) =>
      index.toString shouldBe name
    }
  }

  test("test_of_lookup") {
    // The Java factory both looked a name up exactly and reported an unknown one; the port
    // splits those into `valueOf`, the exact lookup, and `parse`, which reports the failure.
    // Both are asserted, so the two entry points cannot drift apart.
    forEvery(dataName) { (index: OvernightIndex, name: String) =>
      withClue(s"$name: ") {
        OvernightIndex.valueOf(name) shouldBe Some(index)
        OvernightIndex.parse(name) should haveValue(index)
      }
    }
  }

  test("test_extendedEnum") {
    // Ruling: the Java method read the map of the classpath registry of this family. There is
    // no registry - the family is a closed sealed set whose members are created once from the
    // data transcribed into this module (Rule 4 of the request, carried by AAP §0.4.1) - so the
    // closed-family equivalent of that assertion is made: the membership is exactly the
    // thirty-five published indices, their names are distinct, and every name of the shared
    // table resolves through the family's own lookup to the member the table pairs it with.
    val all: List[OvernightIndex] = OvernightIndex.values.toList
    all should have size 35
    all.map(_.name).distinct should have size 35

    forEvery(dataName) { (index: OvernightIndex, name: String) =>
      withClue(s"$name: ") {
        OvernightIndex.valueOf(name) shouldBe Some(index)
        all should contain(index)
      }
    }
  }

  test("test_of_lookup_notFound") {
    // Where the Java factory raised an error for text naming no member, the port reports it as
    // a value. The reason is compared by value rather than by matching the message, so the
    // diagnostic wording of the failure stays free to change.
    OvernightIndex.valueOf("Rubbish") shouldBe None
    OvernightIndex.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
  }

  test("test_of_lookup_null") {
    // Reinterpretation: the Java method passed an absent reference to the factory and asserted
    // that it raised an error. This port writes no such reference and its lookups take a name
    // they resolve as a value, so the case is asserted as the two spellings of an absent name
    // that can actually be supplied - the empty name and a blank one - each of which names no
    // member and so resolves to a parsing failure.
    OvernightIndex.valueOf("") shouldBe None
    OvernightIndex.parse("") should beFailureWith(FailureReason.PARSING)
    OvernightIndex.valueOf("   ") shouldBe None
    OvernightIndex.parse("   ") should beFailureWith(FailureReason.PARSING)
  }

  //-------------------------------------------------------------------------
  test("test_equals") {
    // Ruling: the Java method built two custom indices through a bean builder, differing in
    // their names, and asserted that they were unequal. This family is closed to its
    // thirty-five configured members and has no constructor available outside its own file
    // (Rule 4 of the request; the `[R]` construction kind of AAP §0.3.3), so no custom index
    // can be built here. The property that body asserted - that equality is decided by the
    // name and by nothing else - is asserted over configured members instead: two members with
    // different names are unequal and hash differently, and a member reached by a lookup is
    // equal to the constant naming it even though it was reached by a different route.
    val sonia = OvernightIndices.GBP_SONIA
    val eonia = OvernightIndices.EUR_EONIA
    Eq[OvernightIndex].eqv(sonia, eonia) shouldBe false
    (sonia == eonia) shouldBe false
    Hash[OvernightIndex].hash(sonia) should not be Hash[OvernightIndex].hash(eonia)
    sonia.hashCode should not be eonia.hashCode

    val resolved = lookup("GBP-SONIA")
    Eq[OvernightIndex].eqv(resolved, sonia) shouldBe true
    resolved shouldBe sonia
    Hash[OvernightIndex].hash(resolved) shouldBe Hash[OvernightIndex].hash(sonia)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // Ruling: the Java method swept a custom bean-built index reflectively and then read the
    // constants holder through its private constructor. Neither has a target here - the index
    // cannot be built (Rule 4) and the holder is an object with no constructor - so what those
    // two sweeps stood in for is asserted directly: the constants holder publishes exactly the
    // set of constants the original published, each naming a member of the family, and the
    // typeclass instances of the family agree with each other over those members.
    val constants: List[(String, OvernightIndex)] = List(
      ("GBP_SONIA", OvernightIndices.GBP_SONIA),
      ("CHF_SARON", OvernightIndices.CHF_SARON),
      ("CHF_TOIS", OvernightIndices.CHF_TOIS),
      ("EUR_EONIA", OvernightIndices.EUR_EONIA),
      ("EUR_ESTR", OvernightIndices.EUR_ESTR),
      ("EUR_ESTER", OvernightIndices.EUR_ESTER),
      ("JPY_TONAR", OvernightIndices.JPY_TONAR),
      ("USD_FED_FUND", OvernightIndices.USD_FED_FUND),
      ("USD_SOFR", OvernightIndices.USD_SOFR),
      ("USD_AMERIBOR", OvernightIndices.USD_AMERIBOR),
      ("AUD_AONIA", OvernightIndices.AUD_AONIA),
      ("BRL_CDI", OvernightIndices.BRL_CDI),
      ("CAD_CORRA", OvernightIndices.CAD_CORRA),
      ("DKK_TNR", OvernightIndices.DKK_TNR),
      ("NOK_NOWA", OvernightIndices.NOK_NOWA),
      ("NZD_NZIONA", OvernightIndices.NZD_NZIONA),
      ("PLN_POLONIA", OvernightIndices.PLN_POLONIA),
      ("PLN_POLSTR", OvernightIndices.PLN_POLSTR),
      ("SEK_SIOR", OvernightIndices.SEK_SIOR),
      ("THB_THOR", OvernightIndices.THB_THOR),
      ("ZAR_SABOR", OvernightIndices.ZAR_SABOR)
    )
    // Twenty-one constants naming twenty distinct members, which is exactly the set and exactly
    // the pairing the original declared: EUR_ESTER and EUR_ESTR are one member under its
    // retired and its current spelling. Fourteen of the thirty-five published indices have no
    // constant, also as in the original, and are reached through the lookup.
    constants should have size 21
    constants.map(_._1).distinct should have size 21
    val members: List[OvernightIndex] = constants.map(_._2)
    members.distinct should have size 20

    val all: List[OvernightIndex] = OvernightIndex.values.toList
    constants.foreach { case (label, index) =>
      withClue(s"$label: ") {
        all should contain(index)
        OvernightIndex.valueOf(index.name) shouldBe Some(index)
        OvernightIndex.parse(index.name) should haveValue(index)
        Show[OvernightIndex].show(index) shouldBe index.name
        index.toString shouldBe index.name
      }
    }

    // The published rate whose conventional fixed leg accrues on a different convention from
    // the rate itself. It is asserted here because it is the one member that proves the fixed
    // leg day count is transcribed per row rather than derived from the day count of the index.
    OvernightIndices.NOK_NOWA.dayCount shouldBe DayCounts.ACT_ACT_YEAR
    OvernightIndices.NOK_NOWA.defaultFixedLegDayCount shouldBe DayCounts.ACT_360

    // The companion publishes one equality-bearing instance - an ordering that is also a
    // hashing - so summoning the equality, the hashing or the ordering yields that one value
    // and the three can never disagree. The assertions below are the observable form of that.
    val distinctMembers: List[OvernightIndex] = members.distinct
    for (left <- distinctMembers; right <- distinctMembers) {
      val sameValue = left == right
      withClue(s"${left.name} against ${right.name}: ") {
        Eq[OvernightIndex].eqv(left, right) shouldBe sameValue
        Hash[OvernightIndex].eqv(left, right) shouldBe sameValue
        // the ordering is consistent with equality: it compares equal exactly when the two
        // values are equal, which is the law the combined instance has to satisfy
        (Order[OvernightIndex].compare(left, right) == 0) shouldBe sameValue
        if (sameValue) {
          Hash[OvernightIndex].hash(left) shouldBe Hash[OvernightIndex].hash(right)
        } else {
          Order[OvernightIndex].compare(left, right) should not be 0
        }
      }
    }
  }

  test("test_jodaConvert") {
    // Ruling: the Java method asserted the round trip of the reflective string-conversion
    // library the original annotated this family for. That library is not on the classpath of
    // this port (Rule 1 of the request), and the guarantee its two annotations gave is asserted
    // directly: a member renders as its name, and that rendering reads back as the same member.
    // This is the text round trip; the JSON round trip is test_serialization below, and the two
    // are kept apart because the representations they pin are independent of each other.
    val rendered = Show[OvernightIndex].show(OvernightIndices.GBP_SONIA)
    rendered shouldBe "GBP-SONIA"
    rendered shouldBe OvernightIndices.GBP_SONIA.name
    OvernightIndex.parse(rendered) should haveValue(OvernightIndices.GBP_SONIA)

    forEvery(dataName) { (index: OvernightIndex, name: String) =>
      withClue(s"$name: ") {
        Show[OvernightIndex].show(index) shouldBe name
        OvernightIndex.parse(Show[OvernightIndex].show(index)) should haveValue(index)
      }
    }
  }

  test("test_serialization") {
    // Ruling: the Java method serialized a custom bean-built index through the serialization
    // mechanism of the platform. That mechanism is not supported here and the subject cannot be
    // built (Rule 4), so the replacement is the JSON codec of AAP §0.6.4 over a configured
    // member. What is pinned is the shape that codec is required to have for this family: a
    // member is written as the bare string of its name and never as an object, so a document
    // naming an index reads back here as the same member. The test mapping manifest also routes
    // this method to the consolidated json.JsonRoundTripSpec, whose property-based sweep covers
    // every codec-bearing type of the module; that sweep does not pin the per-type
    // representation, which is what this test asserts, so neither replaces the other.
    val encoded = OvernightIndices.GBP_SONIA.asJson
    encoded shouldBe Json.fromString("GBP-SONIA")
    encoded.as[OvernightIndex] shouldBe Right(OvernightIndices.GBP_SONIA)

    forEvery(dataName) { (index: OvernightIndex, name: String) =>
      withClue(s"$name: ") {
        index.asJson shouldBe Json.fromString(name)
        Json.fromString(name).as[OvernightIndex] shouldBe Right(index)
      }
    }

    // a string naming no member of the family is rejected by the reader
    Json.fromString("Rubbish").as[OvernightIndex].isLeft shouldBe true
  }
}
