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
 * The family publishes thirty-five indices. Every field and every date expected below is written
 * out here rather than read back from the index data the assertions are checking, so a row that
 * carries the wrong value fails here.
 *
 * Two of the fields vary by row and cannot be assumed from any one member. A rate publishes
 * either on its fixing date or on the following business day, and is effective on its fixing date
 * on all but six rows: `CHF-TOIS`, `DKK-TNR`, `DKK-DESTR`, `SEK-SIOR` and `SEK-SWESTR` are
 * effective one business day later and `THB-THOR` two.
 */
class OvernightIndexSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The reference data the date calculations are resolved against.
   *
   * The standard set holds every built-in holiday calendar. Ten fixing calendars of this family -
   * `CLSA`, `COBO`, `HKHK`, `IDJA`, `ILTA`, `INMU`, `RUMO`, `SARI`, `SGSI` and `TRIS` - name
   * holiday data this library does not ship, so those indices do not resolve against it.
   */
  private val RefData: ReferenceData = ReferenceData.standard

  /**
   * The rows the name, rendering, lookup and codec tests are all driven from.
   *
   * Each row pairs a constant with the name it renders as and is looked up by. The pairs are
   * written out rather than derived from `OvernightIndex.values`, which would report any name the
   * family got wrong as the expectation too.
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
   * The ten spellings resolve to six rows, among them the former euro and Japanese overnight
   * names and four spellings of the United States federal funds rate. They belong to the family's
   * lookup rather than to a row of index data, and two of them contain a space.
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
   * `OvernightIndices` declares twenty-one constants naming twenty of the thirty-five published
   * indices, so fifteen rows are reachable by name only and several bodies below reach their
   * subject that way. A name that does not resolve is a defect, reported as a failed assertion.
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
    FloatingRateName.valueOf("GBP-SONIA") shouldBe Some(test.floatingRateName)
    test.toString shouldBe "GBP-SONIA"
  }

  test("test_gbpSonia_dates") {
    // each calculation reports a failure where the calendar cannot be resolved, so the dates are
    // unwrapped through the result matchers
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
    // The one discontinued row: the Tomorrow/Next rate was replaced by SARON and carries
    // `active = false`. It stays published because a trade written against a retired rate still
    // has to be valued. Its effective offset of one day is one of the six non-zero ones, so the
    // flag and that offset are both asserted here rather than left to rows that carry defaults.
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

    // It is the sole inactive member, so the flag discriminates rather than being uniform.
    OvernightIndex.values.toList.filter(index => !index.active) shouldBe List(test)

    // the offset is observable in the derived dates: effective one business day after the fixing,
    // maturity one business day after that
    test.calculateEffectiveFromFixing(date(2014, 10, 13), RefData) should haveValue(date(2014, 10, 14))
    test.calculatePublicationFromFixing(date(2014, 10, 13), RefData) should haveValue(date(2014, 10, 13))
    test.calculateMaturityFromFixing(date(2014, 10, 13), RefData) should haveValue(date(2014, 10, 15))
    test.calculateFixingFromEffective(date(2014, 10, 14), RefData) should haveValue(date(2014, 10, 13))
  }

  test("test_resolvedObservation") {
    // The batch route: the fixing calendar is resolved once and the function that comes back
    // derives the publication, effective and maturity dates and the year fraction of every
    // fixing. An observation's equality reads the index and the fixing date alone, so the derived
    // values are compared field by field against the per-fixing factory. The three subjects cover
    // a publication offset of one day and effective offsets of one and two days, over a business
    // day, a Saturday, a Sunday and the day before a holiday.
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
            resolved.publicationDate shouldBe
              index.calculatePublicationFromFixing(fixingDate, RefData).getOrElse(fail("no date"))
            resolved.effectiveDate shouldBe
              index.calculateEffectiveFromFixing(fixingDate, RefData).getOrElse(fail("no date"))
            resolved.maturityDate shouldBe
              index.calculateMaturityFromFixing(fixingDate, RefData).getOrElse(fail("no date"))
          }
        }
    }

    // Reference data holding no calendar is reported once, by the resolution, rather than per
    // fixing - which is the reason the operation exists.
    OvernightIndexObservation.resolve(lookup("GBP-SONIA"), ReferenceData.empty) should
      beFailureWith(FailureReason.MISSING_DATA)
  }

  test("test_getFloatingRateName") {
    // An Overnight index publishes one rate, so the whole of its name is the name of its floating
    // rate family - unlike an Ibor index, no tenor suffix is removed here.
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
    // The one rate accruing on the business days of a calendar rather than on a calendar-day
    // convention; a Bus/252 day count carries the calendar, so the expectation names it too.
    test.dayCount shouldBe DayCount.ofBus252(StandardHolidayCalendars.BRBD)
    test.toString shouldBe "BRL-CDI"
  }

  test("test_clpOis") {
    val test = lookup("CLP-TNA")
    test.name shouldBe "CLP-TNA"
    test.currency shouldBe Currency.CLP
    test.active shouldBe true
    // a fixing calendar this library ships no holiday data for, so it is named rather than
    // resolved
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
    // All ten spellings are asserted: a rate renamed by a benchmark reform has to stay resolvable
    // under the name trades were written against.
    forEvery(alternateNameRows) { (alternate: String, canonical: String) =>
      withClue(s"$alternate resolving to $canonical: ") {
        val expected = lookup(canonical)
        expected.name shouldBe canonical
        OvernightIndex.valueOf(alternate) shouldBe Some(expected)
        OvernightIndex.parse(alternate) should haveValue(expected)
        OvernightIndex.valueOf(alternate) shouldBe OvernightIndex.valueOf(canonical)
        // the member reports its canonical name, never the spelling it was reached by
        lookup(alternate).name shouldBe canonical
      }
    }

    lookup("JPY-TONA") shouldBe OvernightIndices.JPY_TONAR
    lookup("USD-FED-FUNDS") shouldBe OvernightIndices.USD_FED_FUND
    lookup("USD-FEDFUNDS") shouldBe OvernightIndices.USD_FED_FUND
    lookup("USD-FEDFUND") shouldBe OvernightIndices.USD_FED_FUND

    // EUR_ESTER is declared by looking up the retired spelling, which is a row of the
    // alternate-name table and not a row of the index data, so the constant exists only because
    // that row resolves. It is the same object as EUR_ESTR and reports the current name.
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
    // `valueOf` is the exact lookup and `parse` is the reporting one, and both are asserted for
    // every row of the table.
    forEvery(dataName) { (index: OvernightIndex, name: String) =>
      withClue(s"$name: ") {
        OvernightIndex.valueOf(name) shouldBe Some(index)
        OvernightIndex.parse(name) should haveValue(index)
      }
    }
  }

  test("test_extendedEnum") {
    // The family is a closed set, so its membership is asserted directly: thirty-five indices
    // under thirty-five distinct names, each name of the table resolving to the member beside it.
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
    // Text naming no member answers `None` from the lookup and a parsing failure from the parse.
    // The reason is compared by value rather than by message, so the wording stays free to change.
    OvernightIndex.valueOf("Rubbish") shouldBe None
    OvernightIndex.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
  }

  test("test_of_lookup_null") {
    // The empty name and a blank one are the two spellings of an absent name a caller can supply
    // here; each names no member, so each answers `None` and a parsing failure.
    OvernightIndex.valueOf("") shouldBe None
    OvernightIndex.parse("") should beFailureWith(FailureReason.PARSING)
    OvernightIndex.valueOf("   ") shouldBe None
    OvernightIndex.parse("   ") should beFailureWith(FailureReason.PARSING)
  }

  //-------------------------------------------------------------------------
  test("test_equals") {
    // Equality is decided by the name and by nothing else: two members with different names are
    // unequal, and a member reached by a lookup is equal to the constant naming it. The hash
    // contract runs one way - equal values hash equally, which is what the second pair below
    // requires. The differing hashes of the first pair are an observation about these two
    // members rather than a guarantee the family owes, since distinct values may collide.
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
    // The constants the holder publishes, each paired with the label it is declared under, so a
    // constant bound to the wrong member fails here.
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
    // Twenty-one constants naming twenty distinct members: EUR_ESTER and EUR_ESTR are one member
    // under its retired and its current spelling. The remaining fifteen of the thirty-five
    // published indices have no constant and are reached through the lookup.
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

    // The one published rate whose conventional fixed leg accrues on a different convention from
    // the rate itself, so the fixed-leg day count cannot be derived from the index's own.
    OvernightIndices.NOK_NOWA.dayCount shouldBe DayCounts.ACT_ACT_YEAR
    OvernightIndices.NOK_NOWA.defaultFixedLegDayCount shouldBe DayCounts.ACT_360

    // The companion publishes one equality-bearing instance, an ordering that is also a hashing,
    // and the loop asks all three about the same pairs and requires one answer. Equal values are
    // required to hash equally; for unequal ones the requirement is on the ordering, not the hash.
    val distinctMembers: List[OvernightIndex] = members.distinct
    for (left <- distinctMembers; right <- distinctMembers) {
      val sameValue = left == right
      withClue(s"${left.name} against ${right.name}: ") {
        Eq[OvernightIndex].eqv(left, right) shouldBe sameValue
        Hash[OvernightIndex].eqv(left, right) shouldBe sameValue
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
    // The text round trip: a member renders as its name and that rendering reads back as the same
    // member. The JSON representation is independent of it and is pinned separately, below.
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
    // The shape the codec is required to have here: a member is written as the bare string of its
    // name and never as an object, so a document naming an index reads back as the same member.
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
