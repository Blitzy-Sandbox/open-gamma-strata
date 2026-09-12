/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import java.time.LocalDate
import java.time.LocalTime
import java.time.ZoneId

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
import com.opengamma.strata.basics.currency.Currency.AUD
import com.opengamma.strata.basics.currency.Currency.CZK
import com.opengamma.strata.basics.currency.Currency.DKK
import com.opengamma.strata.basics.currency.Currency.EUR
import com.opengamma.strata.basics.currency.Currency.GBP
import com.opengamma.strata.basics.currency.Currency.HKD
import com.opengamma.strata.basics.currency.Currency.HUF
import com.opengamma.strata.basics.currency.Currency.JPY
import com.opengamma.strata.basics.currency.Currency.KRW
import com.opengamma.strata.basics.currency.Currency.MXN
import com.opengamma.strata.basics.currency.Currency.MYR
import com.opengamma.strata.basics.currency.Currency.NZD
import com.opengamma.strata.basics.currency.Currency.PLN
import com.opengamma.strata.basics.currency.Currency.SEK
import com.opengamma.strata.basics.currency.Currency.SGD
import com.opengamma.strata.basics.currency.Currency.THB
import com.opengamma.strata.basics.currency.Currency.TWD
import com.opengamma.strata.basics.currency.Currency.USD
import com.opengamma.strata.basics.currency.Currency.ZAR
import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.basics.date.BusinessDayConventions.FOLLOWING
import com.opengamma.strata.basics.date.BusinessDayConventions.MODIFIED_FOLLOWING
import com.opengamma.strata.basics.date.BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY
import com.opengamma.strata.basics.date.BusinessDayConventions.PRECEDING
import com.opengamma.strata.basics.date.DayCounts.ACT_360
import com.opengamma.strata.basics.date.DayCounts.ACT_365F
import com.opengamma.strata.basics.date.DayCounts.ACT_ACT_ISDA
import com.opengamma.strata.basics.date.DayCounts.THIRTY_U_360
import com.opengamma.strata.basics.date.DaysAdjustment
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.basics.date.HolidayCalendarIds.AUSY
import com.opengamma.strata.basics.date.HolidayCalendarIds.CZPR
import com.opengamma.strata.basics.date.HolidayCalendarIds.DKCO
import com.opengamma.strata.basics.date.HolidayCalendarIds.EUTA
import com.opengamma.strata.basics.date.HolidayCalendarIds.GBLO
import com.opengamma.strata.basics.date.HolidayCalendarIds.HUBU
import com.opengamma.strata.basics.date.HolidayCalendarIds.JPTO
import com.opengamma.strata.basics.date.HolidayCalendarIds.MXMC
import com.opengamma.strata.basics.date.HolidayCalendarIds.PLWA
import com.opengamma.strata.basics.date.HolidayCalendarIds.SEST
import com.opengamma.strata.basics.date.HolidayCalendarIds.USNY
import com.opengamma.strata.basics.date.HolidayCalendarIds.ZAJO
import com.opengamma.strata.basics.date.PeriodAdditionConventions
import com.opengamma.strata.basics.date.Tenor.TENOR_13W
import com.opengamma.strata.basics.date.Tenor.TENOR_1M
import com.opengamma.strata.basics.date.Tenor.TENOR_2M
import com.opengamma.strata.basics.date.Tenor.TENOR_3M
import com.opengamma.strata.basics.date.Tenor.TENOR_4M
import com.opengamma.strata.basics.date.Tenor.TENOR_4W
import com.opengamma.strata.basics.date.Tenor.TENOR_5M
import com.opengamma.strata.basics.date.Tenor.TENOR_6M
import com.opengamma.strata.basics.date.TenorAdjustment
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._
import com.opengamma.strata.collect.testkit.TestHelper.date

/**
 * Test [[IborIndex]].
 *
 * Every method of the Java original is kept under its own name, so the method-level
 * traceability the migration is measured by stays one-to-one, and every field, date and time
 * asserted below is transcribed from that original rather than re-derived from the index data
 * this port transcribed. The two are independent on purpose: a mistake in the transcription of
 * a row has to fail here, which it cannot do if this spec reads the row it is meant to check.
 *
 * The four parameterised methods of the original were driven from a single provider,
 * `data_name`. That shape is preserved - the provider becomes one shared table, declared once
 * below, and each of the four methods keeps its own test driven from it.
 *
 * ===Reaching an index, and reading a result===
 *
 * The Java factory `IborIndex.of(name)` answered with an index or raised an error. This port
 * splits those: [[IborIndex.valueOf]] answers with a value and [[IborIndex.parse]] reports a
 * failure, so a body asserting fields uses the first through the `lookup` helper below and a
 * body asserting rejection uses the second. Of the 271 published indices only 113 have a
 * declared constant - exactly as in the original - so several bodies can reach their subject
 * only by name.
 *
 * The five date calculations report the failure of resolving a holiday calendar, so each is
 * read through the result matchers rather than by reaching into the value. `calculateFixingDateTime`
 * is total and consults no reference data, so it is compared directly.
 *
 * The maturity offset of an index is a `TenorAdjustment`, whose factories validate the pairing
 * of the tenor with the addition convention and therefore answer with a result. The expected
 * offset is consequently the left-hand side of its assertion - `expected should
 * haveValue(index.maturityDateOffset)` - which asserts in one matcher call both that the
 * expected construction is accepted and that it names exactly the offset the index carries,
 * without unwrapping anything.
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
 *    the family's own lookup, and the membership is exactly the 271 published indices.
 *  - `test_of_lookup_null` passed an absent reference to the factory and asserted that it
 *    raised an error. This port writes no such reference (Rule 5) and its lookups answer with a
 *    value, so the case becomes the two spellings of an absent name that can actually be
 *    supplied - the empty name and a blank one.
 *  - `test_equals` and `coverage` both built a custom index through a bean builder. The family
 *    is closed to its 271 configured members and has no public constructor (Rule 4, and the
 *    `[R]` construction kind of AAP §0.3.3), so both are asserted over configured members.
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
 * The fidelity of the 271 transcribed rows against the manifest captured from the Java
 * implementation belongs to `ReferenceDataManifestSpec`, and the closedness of every family of
 * the library, including the absence of a name claimed by two families, belongs to
 * `NamedEnumClosedSpec`. Those two and this one are complementary, and none of the three may be
 * weakened because another exists.
 */
class IborIndexSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The reference data the date calculations are resolved against.
   *
   * The standard set holds every built-in holiday calendar, which is the set the Java original
   * used for the same assertions.
   */
  private val RefData: ReferenceData = ReferenceData.standard

  /**
   * The identifier of the New Zealand bank calendar, which has no constant declared for it.
   *
   * The Java original named it the same way and for the same reason; the factory is total, so
   * an identifier naming a calendar this library has no data for is still a perfectly good
   * identifier and only fails when something tries to resolve it.
   */
  private val NZBD: HolidayCalendarId = HolidayCalendarId.of("NZBD")

  /**
   * The shared provider, transcribed row-for-row from the Java data provider.
   *
   * Each row pairs a constant with the name it renders as and is looked up by. The rows are in
   * the order of that provider.
   */
  private val dataName: TableFor2[IborIndex, String] = Table(
    ("index", "name"),
    (IborIndices.GBP_LIBOR_6M, "GBP-LIBOR-6M"),
    (IborIndices.CHF_LIBOR_6M, "CHF-LIBOR-6M"),
    (IborIndices.EUR_LIBOR_6M, "EUR-LIBOR-6M"),
    (IborIndices.JPY_LIBOR_6M, "JPY-LIBOR-6M"),
    (IborIndices.USD_LIBOR_6M, "USD-LIBOR-6M"),
    (IborIndices.EUR_EURIBOR_1M, "EUR-EURIBOR-1M"),
    (IborIndices.JPY_TIBOR_JAPAN_3M, "JPY-TIBOR-JAPAN-3M"),
    (IborIndices.JPY_TIBOR_EUROYEN_6M, "JPY-TIBOR-EUROYEN-6M"),
    (IborIndices.AUD_BBSW_1M, "AUD-BBSW-1M"),
    (IborIndices.AUD_BBSW_2M, "AUD-BBSW-2M"),
    (IborIndices.AUD_BBSW_3M, "AUD-BBSW-3M"),
    (IborIndices.AUD_BBSW_4M, "AUD-BBSW-4M"),
    (IborIndices.AUD_BBSW_5M, "AUD-BBSW-5M"),
    (IborIndices.AUD_BBSW_6M, "AUD-BBSW-6M")
  )

  /**
   * Looks a published index up by name, failing the test where the family has no such member.
   *
   * This stands in for the Java factory every body below called. A name that does not resolve
   * is a defect this spec should report as a failed assertion rather than as an error escaping
   * it, and 158 of the 271 published indices have no constant - exactly as in the original - so
   * several bodies can only reach their subject this way.
   *
   * @param name  the name of the index to look up, such as `GBP-LIBOR-3M`
   * @return the index published under that name
   */
  private def lookup(name: String): IborIndex =
    IborIndex.valueOf(name).getOrElse(fail(s"No Ibor index is published under: $name"))

  //-------------------------------------------------------------------------
  test("test_gbpLibor3m") {
    val test = lookup("GBP-LIBOR-3M")
    test.name shouldBe "GBP-LIBOR-3M"
    test.currency shouldBe GBP
    test.active shouldBe true
    test.tenor shouldBe TENOR_3M
    test.fixingCalendar shouldBe GBLO
    test.fixingDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(PRECEDING, GBLO))
    test.effectiveDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(FOLLOWING, GBLO))
    TenorAdjustment.ofLastBusinessDay(TENOR_3M, BusinessDayAdjustment.of(MODIFIED_FOLLOWING, GBLO)) should
      haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_365F
    test.defaultFixedLegDayCount shouldBe ACT_365F
    // an Ibor index names a family plus a tenor, so the family name is the name without the suffix
    FloatingRateName.valueOf("GBP-LIBOR") shouldBe Some(test.floatingRateName)
    test.toString shouldBe "GBP-LIBOR-3M"
  }

  test("test_gbpLibor3m_dates") {
    val test = lookup("GBP-LIBOR-3M")
    test.calculateEffectiveFromFixing(date(2014, 10, 13), RefData) should haveValue(date(2014, 10, 13))
    test.calculateMaturityFromFixing(date(2014, 10, 13), RefData) should haveValue(date(2015, 1, 13))
    test.calculateFixingFromEffective(date(2014, 10, 13), RefData) should haveValue(date(2014, 10, 13))
    test.calculateMaturityFromEffective(date(2014, 10, 13), RefData) should haveValue(date(2015, 1, 13))
    // weekend
    test.calculateEffectiveFromFixing(date(2014, 10, 10), RefData) should haveValue(date(2014, 10, 10))
    test.calculateMaturityFromFixing(date(2014, 10, 10), RefData) should haveValue(date(2015, 1, 12))
    test.calculateFixingFromEffective(date(2014, 10, 10), RefData) should haveValue(date(2014, 10, 10))
    test.calculateMaturityFromEffective(date(2014, 10, 10), RefData) should haveValue(date(2015, 1, 12))
    // input date is Sunday
    test.calculateEffectiveFromFixing(date(2014, 10, 12), RefData) should haveValue(date(2014, 10, 13))
    test.calculateMaturityFromFixing(date(2014, 10, 12), RefData) should haveValue(date(2015, 1, 13))
    test.calculateFixingFromEffective(date(2014, 10, 12), RefData) should haveValue(date(2014, 10, 13))
    test.calculateMaturityFromEffective(date(2014, 10, 12), RefData) should haveValue(date(2015, 1, 13))
    // fixing time and zone
    test.calculateFixingDateTime(date(2014, 10, 13)) shouldBe
      date(2014, 10, 13).atTime(LocalTime.of(11, 55)).atZone(ZoneId.of("Europe/London"))
    // resolve - the resolved calculation of a fixing produces what the per-fixing factory does,
    // and the observation is built only through that factory, so the two results are compared
    // as results rather than one of them being unwrapped
    val fixing: LocalDate = date(2014, 10, 13)
    IborIndexObservation.of(test, fixing, RefData) should beSuccess
    IborIndexObservation.resolve(test, RefData).map(observe => observe(fixing)) shouldBe
      IborIndexObservation.of(test, fixing, RefData)
  }

  test("test_getFloatingRateName") {
    // The Java method iterated the registry of the family. The family is closed, so its own
    // membership is iterated instead, which covers the same indices and cannot be short of any.
    // An Ibor index names a family and a tenor, so the family is the name up to its last hyphen
    // - the derivation the Java method performed, transcribed verbatim.
    IborIndex.values.toList.foreach { index =>
      val name = index.name.substring(0, index.name.lastIndexOf('-'))
      withClue(s"${index.name} -> $name: ") {
        FloatingRateName.valueOf(name) shouldBe Some(index.floatingRateName)
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_usdLibor3m") {
    val test = lookup("USD-LIBOR-3M")
    test.currency shouldBe USD
    test.name shouldBe "USD-LIBOR-3M"
    test.tenor shouldBe TENOR_3M
    test.fixingCalendar shouldBe GBLO
    test.fixingDateOffset shouldBe DaysAdjustment.ofBusinessDays(-2, GBLO)
    // the rate fixes in London but settles where both London and New York are open, so the
    // effective offset counts London business days and lands on a business day of the composite
    test.effectiveDateOffset shouldBe DaysAdjustment.ofBusinessDays(
      2, GBLO, BusinessDayAdjustment.of(FOLLOWING, GBLO.combinedWith(USNY)))
    TenorAdjustment.ofLastBusinessDay(
      TENOR_3M, BusinessDayAdjustment.of(MODIFIED_FOLLOWING, GBLO.combinedWith(USNY))) should
      haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_360
    test.defaultFixedLegDayCount shouldBe ACT_360
    FloatingRateName.valueOf("USD-LIBOR") shouldBe Some(test.floatingRateName)
    test.toString shouldBe "USD-LIBOR-3M"
  }

  test("test_usdLibor3m_dates") {
    val test = lookup("USD-LIBOR-3M")
    test.calculateEffectiveFromFixing(date(2014, 10, 27), RefData) should haveValue(date(2014, 10, 29))
    test.calculateMaturityFromFixing(date(2014, 10, 27), RefData) should haveValue(date(2015, 1, 29))
    test.calculateFixingFromEffective(date(2014, 10, 29), RefData) should haveValue(date(2014, 10, 27))
    test.calculateMaturityFromEffective(date(2014, 10, 29), RefData) should haveValue(date(2015, 1, 29))
    // weekend
    test.calculateEffectiveFromFixing(date(2014, 10, 10), RefData) should haveValue(date(2014, 10, 14))
    test.calculateMaturityFromFixing(date(2014, 10, 10), RefData) should haveValue(date(2015, 1, 14))
    test.calculateFixingFromEffective(date(2014, 10, 14), RefData) should haveValue(date(2014, 10, 10))
    test.calculateMaturityFromEffective(date(2014, 10, 14), RefData) should haveValue(date(2015, 1, 14))
    // effective date is US holiday
    test.calculateEffectiveFromFixing(date(2015, 1, 16), RefData) should haveValue(date(2015, 1, 20))
    test.calculateMaturityFromFixing(date(2015, 1, 16), RefData) should haveValue(date(2015, 4, 20))
    test.calculateFixingFromEffective(date(2015, 1, 20), RefData) should haveValue(date(2015, 1, 16))
    test.calculateMaturityFromEffective(date(2015, 1, 20), RefData) should haveValue(date(2015, 4, 20))
    // input date is Sunday, 13th is US holiday, but not UK holiday (can fix, but not be effective)
    test.calculateEffectiveFromFixing(date(2014, 10, 12), RefData) should haveValue(date(2014, 10, 15))
    test.calculateMaturityFromFixing(date(2014, 10, 12), RefData) should haveValue(date(2015, 1, 15))
    test.calculateFixingFromEffective(date(2014, 10, 12), RefData) should haveValue(date(2014, 10, 10))
    test.calculateMaturityFromEffective(date(2014, 10, 12), RefData) should haveValue(date(2015, 1, 14))
    // fixing time and zone
    test.calculateFixingDateTime(date(2014, 10, 13)) shouldBe
      date(2014, 10, 13).atTime(LocalTime.of(11, 55)).atZone(ZoneId.of("Europe/London"))
    // resolve
    val fixing: LocalDate = date(2014, 10, 27)
    IborIndexObservation.of(test, fixing, RefData) should beSuccess
    IborIndexObservation.resolve(test, RefData).map(observe => observe(fixing)) shouldBe
      IborIndexObservation.of(test, fixing, RefData)
  }

  test("test_euribor3m") {
    val test = lookup("EUR-EURIBOR-3M")
    test.currency shouldBe EUR
    test.name shouldBe "EUR-EURIBOR-3M"
    test.tenor shouldBe TENOR_3M
    test.fixingCalendar shouldBe EUTA
    test.fixingDateOffset shouldBe DaysAdjustment.ofBusinessDays(-2, EUTA)
    test.effectiveDateOffset shouldBe DaysAdjustment.ofBusinessDays(2, EUTA)
    TenorAdjustment.ofLastBusinessDay(TENOR_3M, BusinessDayAdjustment.of(MODIFIED_FOLLOWING, EUTA)) should
      haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_360
    // the one family here whose conventional fixed leg accrues on a different convention from
    // the rate itself, which is why the two are asserted separately
    test.defaultFixedLegDayCount shouldBe THIRTY_U_360
    FloatingRateName.valueOf("EUR-EURIBOR") shouldBe Some(test.floatingRateName)
    test.toString shouldBe "EUR-EURIBOR-3M"
  }

  test("test_euribor3m_dates") {
    val test = lookup("EUR-EURIBOR-3M")
    test.calculateEffectiveFromFixing(date(2014, 10, 27), RefData) should haveValue(date(2014, 10, 29))
    test.calculateMaturityFromFixing(date(2014, 10, 27), RefData) should haveValue(date(2015, 1, 29))
    test.calculateFixingFromEffective(date(2014, 10, 29), RefData) should haveValue(date(2014, 10, 27))
    test.calculateMaturityFromEffective(date(2014, 10, 29), RefData) should haveValue(date(2015, 1, 29))
    // weekend
    test.calculateEffectiveFromFixing(date(2014, 10, 10), RefData) should haveValue(date(2014, 10, 14))
    test.calculateMaturityFromFixing(date(2014, 10, 10), RefData) should haveValue(date(2015, 1, 14))
    test.calculateFixingFromEffective(date(2014, 10, 14), RefData) should haveValue(date(2014, 10, 10))
    test.calculateMaturityFromEffective(date(2014, 10, 14), RefData) should haveValue(date(2015, 1, 14))
    // input date is Sunday
    test.calculateEffectiveFromFixing(date(2014, 10, 12), RefData) should haveValue(date(2014, 10, 15))
    test.calculateMaturityFromFixing(date(2014, 10, 12), RefData) should haveValue(date(2015, 1, 15))
    test.calculateFixingFromEffective(date(2014, 10, 12), RefData) should haveValue(date(2014, 10, 9))
    test.calculateMaturityFromEffective(date(2014, 10, 12), RefData) should haveValue(date(2015, 1, 13))
    // fixing time and zone
    test.calculateFixingDateTime(date(2014, 10, 13)) shouldBe
      date(2014, 10, 13).atTime(LocalTime.of(11, 0)).atZone(ZoneId.of("Europe/Brussels"))
  }

  test("test_tibor_japan3m") {
    val test = lookup("JPY-TIBOR-JAPAN-3M")
    test.currency shouldBe JPY
    test.name shouldBe "JPY-TIBOR-JAPAN-3M"
    test.tenor shouldBe TENOR_3M
    test.fixingCalendar shouldBe JPTO
    test.fixingDateOffset shouldBe DaysAdjustment.ofBusinessDays(-2, JPTO)
    test.effectiveDateOffset shouldBe DaysAdjustment.ofBusinessDays(2, JPTO)
    TenorAdjustment.ofLastBusinessDay(TENOR_3M, BusinessDayAdjustment.of(MODIFIED_FOLLOWING, JPTO)) should
      haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_365F
    test.defaultFixedLegDayCount shouldBe ACT_365F
    FloatingRateName.valueOf("JPY-TIBOR-JAPAN") shouldBe Some(test.floatingRateName)
    test.toString shouldBe "JPY-TIBOR-JAPAN-3M"
  }

  test("test_tibor_japan3m_dates") {
    val test = lookup("JPY-TIBOR-JAPAN-3M")
    test.calculateEffectiveFromFixing(date(2014, 10, 27), RefData) should haveValue(date(2014, 10, 29))
    test.calculateMaturityFromFixing(date(2014, 10, 27), RefData) should haveValue(date(2015, 1, 29))
    test.calculateFixingFromEffective(date(2014, 10, 29), RefData) should haveValue(date(2014, 10, 27))
    test.calculateMaturityFromEffective(date(2014, 10, 29), RefData) should haveValue(date(2015, 1, 29))
    // weekend
    test.calculateEffectiveFromFixing(date(2014, 10, 10), RefData) should haveValue(date(2014, 10, 15))
    test.calculateMaturityFromFixing(date(2014, 10, 10), RefData) should haveValue(date(2015, 1, 15))
    test.calculateFixingFromEffective(date(2014, 10, 15), RefData) should haveValue(date(2014, 10, 10))
    test.calculateMaturityFromEffective(date(2014, 10, 15), RefData) should haveValue(date(2015, 1, 15))
    // input date is Sunday
    test.calculateEffectiveFromFixing(date(2014, 10, 12), RefData) should haveValue(date(2014, 10, 16))
    test.calculateMaturityFromFixing(date(2014, 10, 12), RefData) should haveValue(date(2015, 1, 16))
    test.calculateFixingFromEffective(date(2014, 10, 12), RefData) should haveValue(date(2014, 10, 9))
    test.calculateMaturityFromEffective(date(2014, 10, 12), RefData) should haveValue(date(2015, 1, 14))
    // fixing time and zone
    test.calculateFixingDateTime(date(2014, 10, 13)) shouldBe
      date(2014, 10, 13).atTime(LocalTime.of(13, 0)).atZone(ZoneId.of("Asia/Tokyo"))
  }

  test("test_tibor_euroyen3m") {
    val test = lookup("JPY-TIBOR-EUROYEN-3M")
    test.currency shouldBe JPY
    test.name shouldBe "JPY-TIBOR-EUROYEN-3M"
    test.tenor shouldBe TENOR_3M
    test.fixingCalendar shouldBe JPTO
    test.fixingDateOffset shouldBe DaysAdjustment.ofBusinessDays(-2, JPTO)
    test.effectiveDateOffset shouldBe DaysAdjustment.ofBusinessDays(2, JPTO)
    TenorAdjustment.ofLastBusinessDay(TENOR_3M, BusinessDayAdjustment.of(MODIFIED_FOLLOWING, JPTO)) should
      haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_360
    test.defaultFixedLegDayCount shouldBe ACT_365F
    FloatingRateName.valueOf("JPY-TIBOR-EUROYEN") shouldBe Some(test.floatingRateName)
    test.toString shouldBe "JPY-TIBOR-EUROYEN-3M"
  }

  test("test_tibor_euroyen3m_dates") {
    val test = lookup("JPY-TIBOR-EUROYEN-3M")
    test.calculateEffectiveFromFixing(date(2014, 10, 27), RefData) should haveValue(date(2014, 10, 29))
    test.calculateMaturityFromFixing(date(2014, 10, 27), RefData) should haveValue(date(2015, 1, 29))
    test.calculateFixingFromEffective(date(2014, 10, 29), RefData) should haveValue(date(2014, 10, 27))
    test.calculateMaturityFromEffective(date(2014, 10, 29), RefData) should haveValue(date(2015, 1, 29))
    // weekend
    test.calculateEffectiveFromFixing(date(2014, 10, 10), RefData) should haveValue(date(2014, 10, 15))
    test.calculateMaturityFromFixing(date(2014, 10, 10), RefData) should haveValue(date(2015, 1, 15))
    test.calculateFixingFromEffective(date(2014, 10, 15), RefData) should haveValue(date(2014, 10, 10))
    test.calculateMaturityFromEffective(date(2014, 10, 15), RefData) should haveValue(date(2015, 1, 15))
    // input date is Sunday
    test.calculateEffectiveFromFixing(date(2014, 10, 12), RefData) should haveValue(date(2014, 10, 16))
    test.calculateMaturityFromFixing(date(2014, 10, 12), RefData) should haveValue(date(2015, 1, 16))
    test.calculateFixingFromEffective(date(2014, 10, 12), RefData) should haveValue(date(2014, 10, 9))
    test.calculateMaturityFromEffective(date(2014, 10, 12), RefData) should haveValue(date(2015, 1, 14))
    // fixing time and zone
    test.calculateFixingDateTime(date(2014, 10, 13)) shouldBe
      date(2014, 10, 13).atTime(LocalTime.of(13, 0)).atZone(ZoneId.of("Asia/Tokyo"))
  }

  test("test_usdLibor_all") {
    // Every tenor of the family that has a declared constant, asserted both by the name the
    // lookup resolves and by identity with that constant - so a constant naming the wrong row
    // and a row carrying the wrong name are separate failures.
    lookup("USD-LIBOR-1W").name shouldBe "USD-LIBOR-1W"
    lookup("USD-LIBOR-1W") shouldBe IborIndices.USD_LIBOR_1W
    lookup("USD-LIBOR-1M").name shouldBe "USD-LIBOR-1M"
    lookup("USD-LIBOR-1M") shouldBe IborIndices.USD_LIBOR_1M
    lookup("USD-LIBOR-2M").name shouldBe "USD-LIBOR-2M"
    lookup("USD-LIBOR-2M") shouldBe IborIndices.USD_LIBOR_2M
    lookup("USD-LIBOR-3M").name shouldBe "USD-LIBOR-3M"
    lookup("USD-LIBOR-3M") shouldBe IborIndices.USD_LIBOR_3M
    lookup("USD-LIBOR-6M").name shouldBe "USD-LIBOR-6M"
    lookup("USD-LIBOR-6M") shouldBe IborIndices.USD_LIBOR_6M
    lookup("USD-LIBOR-12M").name shouldBe "USD-LIBOR-12M"
    lookup("USD-LIBOR-12M") shouldBe IborIndices.USD_LIBOR_12M
  }

  test("test_usdAmeriborTerm") {
    // No constant is declared for this family, so its members are reached by name alone - which
    // is what the Java method did too, through the factory this lookup stands in for.
    lookup("USD-AMERIBORTERM-1M").name shouldBe "USD-AMERIBORTERM-1M"
    lookup("USD-AMERIBORTERM-3M").name shouldBe "USD-AMERIBORTERM-3M"
  }

  test("test_usdBsby") {
    lookup("USD-BSBY-1M").name shouldBe "USD-BSBY-1M"
    lookup("USD-BSBY-3M").name shouldBe "USD-BSBY-3M"
    lookup("USD-BSBY-6M").name shouldBe "USD-BSBY-6M"
    lookup("USD-BSBY-12M").name shouldBe "USD-BSBY-12M"
  }

  test("test_usdTermSofr") {
    lookup("USD-SOFRCMETERM-1M").name shouldBe "USD-SOFRCMETERM-1M"
    lookup("USD-SOFRCMETERM-3M").name shouldBe "USD-SOFRCMETERM-3M"
    lookup("USD-SOFRCMETERM-6M").name shouldBe "USD-SOFRCMETERM-6M"
    lookup("USD-SOFRCMETERM-12M").name shouldBe "USD-SOFRCMETERM-12M"
  }

  test("test_gbpSoniaIceTerm") {
    lookup("GBP-SONIAICETERM-1M").name shouldBe "GBP-SONIAICETERM-1M"
    lookup("GBP-SONIAICETERM-3M").name shouldBe "GBP-SONIAICETERM-3M"
    lookup("GBP-SONIAICETERM-6M").name shouldBe "GBP-SONIAICETERM-6M"
    lookup("GBP-SONIAICETERM-12M").name shouldBe "GBP-SONIAICETERM-12M"
  }

  test("test_gbpSoniaRefinitivTerm") {
    lookup("GBP-SONIAREFINITIVTERM-1M").name shouldBe "GBP-SONIAREFINITIVTERM-1M"
    lookup("GBP-SONIAREFINITIVTERM-3M").name shouldBe "GBP-SONIAREFINITIVTERM-3M"
    lookup("GBP-SONIAREFINITIVTERM-6M").name shouldBe "GBP-SONIAREFINITIVTERM-6M"
    lookup("GBP-SONIAREFINITIVTERM-12M").name shouldBe "GBP-SONIAREFINITIVTERM-12M"
  }

  test("test_jpyTorf") {
    lookup("JPY-TORF-1M").name shouldBe "JPY-TORF-1M"
    lookup("JPY-TORF-3M").name shouldBe "JPY-TORF-3M"
    lookup("JPY-TORF-6M").name shouldBe "JPY-TORF-6M"
  }


  //-------------------------------------------------------------------------
  test("test_bbsw1m") {
    val test = lookup("AUD-BBSW-1M")
    test.currency shouldBe AUD
    test.name shouldBe "AUD-BBSW-1M"
    test.tenor shouldBe TENOR_1M
    test.fixingCalendar shouldBe AUSY
    test.fixingDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(PRECEDING, AUSY))
    test.effectiveDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(FOLLOWING, AUSY))
    // the bank bill families add the tenor plainly and then keep the result inside the fortnight
    // of its month, so the addition convention is none and the business day convention is the
    // bi-monthly one - a different pairing from the Libor families above
    TenorAdjustment.of(
      TENOR_1M,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING_BI_MONTHLY, AUSY)) should
      haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_365F
    test.defaultFixedLegDayCount shouldBe ACT_365F
    test.toString shouldBe "AUD-BBSW-1M"
  }

  test("test_bbsw2m") {
    val test = lookup("AUD-BBSW-2M")
    test.currency shouldBe AUD
    test.name shouldBe "AUD-BBSW-2M"
    test.tenor shouldBe TENOR_2M
    test.fixingCalendar shouldBe AUSY
    test.fixingDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(PRECEDING, AUSY))
    test.effectiveDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(FOLLOWING, AUSY))
    TenorAdjustment.of(
      TENOR_2M,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING_BI_MONTHLY, AUSY)) should
      haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_365F
    test.toString shouldBe "AUD-BBSW-2M"
  }

  test("test_bbsw3m") {
    val test = lookup("AUD-BBSW-3M")
    test.currency shouldBe AUD
    test.name shouldBe "AUD-BBSW-3M"
    test.tenor shouldBe TENOR_3M
    test.fixingCalendar shouldBe AUSY
    test.fixingDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(PRECEDING, AUSY))
    test.effectiveDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(FOLLOWING, AUSY))
    TenorAdjustment.of(
      TENOR_3M,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING_BI_MONTHLY, AUSY)) should
      haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_365F
    test.toString shouldBe "AUD-BBSW-3M"
  }

  test("test_bbsw4m") {
    val test = lookup("AUD-BBSW-4M")
    test.currency shouldBe AUD
    test.name shouldBe "AUD-BBSW-4M"
    test.tenor shouldBe TENOR_4M
    test.fixingCalendar shouldBe AUSY
    test.fixingDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(PRECEDING, AUSY))
    test.effectiveDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(FOLLOWING, AUSY))
    TenorAdjustment.of(
      TENOR_4M,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING_BI_MONTHLY, AUSY)) should
      haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_365F
    test.toString shouldBe "AUD-BBSW-4M"
  }

  test("test_bbsw5m") {
    val test = lookup("AUD-BBSW-5M")
    test.currency shouldBe AUD
    test.name shouldBe "AUD-BBSW-5M"
    test.tenor shouldBe TENOR_5M
    test.fixingCalendar shouldBe AUSY
    test.fixingDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(PRECEDING, AUSY))
    test.effectiveDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(FOLLOWING, AUSY))
    TenorAdjustment.of(
      TENOR_5M,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING_BI_MONTHLY, AUSY)) should
      haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_365F
    test.toString shouldBe "AUD-BBSW-5M"
  }

  test("test_bbsw6m") {
    val test = lookup("AUD-BBSW-6M")
    test.currency shouldBe AUD
    test.name shouldBe "AUD-BBSW-6M"
    test.tenor shouldBe TENOR_6M
    test.fixingCalendar shouldBe AUSY
    test.fixingDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(PRECEDING, AUSY))
    test.effectiveDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(FOLLOWING, AUSY))
    TenorAdjustment.of(
      TENOR_6M,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING_BI_MONTHLY, AUSY)) should
      haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_365F
    test.toString shouldBe "AUD-BBSW-6M"
  }

  test("test_czk_pribor") {
    val test = lookup("CZK-PRIBOR-3M")
    test.currency shouldBe CZK
    test.name shouldBe "CZK-PRIBOR-3M"
    test.tenor shouldBe TENOR_3M
    test.fixingCalendar shouldBe CZPR
    test.fixingDateOffset shouldBe DaysAdjustment.ofBusinessDays(-2, CZPR)
    test.effectiveDateOffset shouldBe DaysAdjustment.ofBusinessDays(2, CZPR)
    TenorAdjustment.of(
      TENOR_3M,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING, CZPR)) should haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_360
    test.defaultFixedLegDayCount shouldBe ACT_360
    test.toString shouldBe "CZK-PRIBOR-3M"
  }

  test("test_dkk_cibor") {
    val test = lookup("DKK-CIBOR-3M")
    test.currency shouldBe DKK
    test.name shouldBe "DKK-CIBOR-3M"
    test.tenor shouldBe TENOR_3M
    test.fixingCalendar shouldBe DKCO
    test.fixingDateOffset shouldBe DaysAdjustment.ofBusinessDays(-2, DKCO)
    test.effectiveDateOffset shouldBe DaysAdjustment.ofBusinessDays(2, DKCO)
    TenorAdjustment.of(
      TENOR_3M,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING, DKCO)) should haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_360
    test.defaultFixedLegDayCount shouldBe THIRTY_U_360
    test.toString shouldBe "DKK-CIBOR-3M"
  }

  test("test_hkd_hibor") {
    // no constant is declared for the Hong Kong calendar, and this library carries no data for
    // it either - the identifier is still the identifier the published row names
    val cal = HolidayCalendarId.of("HKHK")
    val test = lookup("HKD-HIBOR-3M")
    test.currency shouldBe HKD
    test.name shouldBe "HKD-HIBOR-3M"
    test.tenor shouldBe TENOR_3M
    test.fixingCalendar shouldBe cal
    test.fixingDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(PRECEDING, cal))
    test.effectiveDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(FOLLOWING, cal))
    TenorAdjustment.of(
      TENOR_3M,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING, cal)) should haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_365F
    test.defaultFixedLegDayCount shouldBe ACT_365F
    test.toString shouldBe "HKD-HIBOR-3M"
  }

  test("test_huf_bubor") {
    val test = lookup("HUF-BUBOR-3M")
    test.currency shouldBe HUF
    test.name shouldBe "HUF-BUBOR-3M"
    test.tenor shouldBe TENOR_3M
    test.fixingCalendar shouldBe HUBU
    test.fixingDateOffset shouldBe DaysAdjustment.ofBusinessDays(-2, HUBU)
    test.effectiveDateOffset shouldBe DaysAdjustment.ofBusinessDays(2, HUBU)
    TenorAdjustment.of(
      TENOR_3M,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING, HUBU)) should haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_360
    test.defaultFixedLegDayCount shouldBe ACT_365F
    test.toString shouldBe "HUF-BUBOR-3M"
  }

  test("test_krw_cd") {
    val cal = HolidayCalendarId.of("KRSE")
    val test = lookup("KRW-CD-13W")
    test.currency shouldBe KRW
    test.name shouldBe "KRW-CD-13W"
    test.tenor shouldBe TENOR_13W
    test.fixingCalendar shouldBe cal
    test.fixingDateOffset shouldBe DaysAdjustment.ofBusinessDays(-1, cal)
    test.effectiveDateOffset shouldBe DaysAdjustment.ofBusinessDays(1, cal)
    // a week-based tenor cannot be paired with a month-end addition convention, so this family
    // adds plainly and adjusts plainly following - not the last-business-day pairing of Libor
    TenorAdjustment.of(
      TENOR_13W,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.of(FOLLOWING, cal)) should haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_365F
    test.defaultFixedLegDayCount shouldBe ACT_365F
    test.toString shouldBe "KRW-CD-13W"

    // The one alternate name the family declares: the market spells this rate's tenor in months
    // while the index spells it in weeks, and both name the same rate. It is behaviour rather
    // than configuration (Rule 4 of the request), so the alternate spelling has to resolve, and
    // to the very same value rather than to an equal copy.
    val test2 = lookup("KRW-CD-3M")
    test2.name shouldBe "KRW-CD-13W"
    test2 shouldBe test
  }

  test("test_mxn_tiie") {
    val test = lookup("MXN-TIIE-4W")
    test.currency shouldBe MXN
    test.name shouldBe "MXN-TIIE-4W"
    test.tenor shouldBe TENOR_4W
    test.fixingCalendar shouldBe MXMC
    test.fixingDateOffset shouldBe DaysAdjustment.ofBusinessDays(-1, MXMC)
    test.effectiveDateOffset shouldBe DaysAdjustment.ofBusinessDays(1, MXMC)
    TenorAdjustment.of(
      TENOR_4W,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.of(FOLLOWING, MXMC)) should haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_360
    test.defaultFixedLegDayCount shouldBe ACT_360
    test.toString shouldBe "MXN-TIIE-4W"
  }

  test("test_myr_klibor") {
    val cal = HolidayCalendarId.of("MYKL")
    val test = lookup("MYR-KLIBOR-3M")
    test.currency shouldBe MYR
    test.name shouldBe "MYR-KLIBOR-3M"
    test.tenor shouldBe TENOR_3M
    test.fixingCalendar shouldBe cal
    test.fixingDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(PRECEDING, cal))
    test.effectiveDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(FOLLOWING, cal))
    TenorAdjustment.of(
      TENOR_3M,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING, cal)) should haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_365F
    test.defaultFixedLegDayCount shouldBe ACT_365F
    test.toString shouldBe "MYR-KLIBOR-3M"
  }

  test("test_nzd_bkbm") {
    val test = lookup("NZD-BKBM-3M")
    test.currency shouldBe NZD
    test.name shouldBe "NZD-BKBM-3M"
    test.tenor shouldBe TENOR_3M
    test.fixingCalendar shouldBe NZBD
    test.fixingDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(PRECEDING, NZBD))
    test.effectiveDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(FOLLOWING, NZBD))
    TenorAdjustment.of(
      TENOR_3M,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING, NZBD)) should haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_365F
    test.defaultFixedLegDayCount shouldBe ACT_365F
    test.toString shouldBe "NZD-BKBM-3M"
  }

  test("test_pln_wibor") {
    val test = lookup("PLN-WIBOR-3M")
    test.currency shouldBe PLN
    test.name shouldBe "PLN-WIBOR-3M"
    test.tenor shouldBe TENOR_3M
    test.fixingCalendar shouldBe PLWA
    test.fixingDateOffset shouldBe DaysAdjustment.ofBusinessDays(-2, PLWA)
    test.effectiveDateOffset shouldBe DaysAdjustment.ofBusinessDays(2, PLWA)
    TenorAdjustment.of(
      TENOR_3M,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING, PLWA)) should haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_365F
    test.defaultFixedLegDayCount shouldBe ACT_ACT_ISDA
    test.toString shouldBe "PLN-WIBOR-3M"
  }

  test("test_sek_stibor") {
    val test = lookup("SEK-STIBOR-3M")
    test.currency shouldBe SEK
    test.name shouldBe "SEK-STIBOR-3M"
    test.tenor shouldBe TENOR_3M
    test.fixingCalendar shouldBe SEST
    test.fixingDateOffset shouldBe DaysAdjustment.ofBusinessDays(-2, SEST)
    test.effectiveDateOffset shouldBe DaysAdjustment.ofBusinessDays(2, SEST)
    TenorAdjustment.of(
      TENOR_3M,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING, SEST)) should haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_360
    test.defaultFixedLegDayCount shouldBe THIRTY_U_360
    test.toString shouldBe "SEK-STIBOR-3M"
  }

  test("test_sgd_sibor") {
    val cal = HolidayCalendarId.of("SGSI")
    val test = lookup("SGD-SIBOR-3M")
    test.currency shouldBe SGD
    test.name shouldBe "SGD-SIBOR-3M"
    test.tenor shouldBe TENOR_3M
    test.fixingCalendar shouldBe cal
    test.fixingDateOffset shouldBe DaysAdjustment.ofBusinessDays(-2, cal)
    test.effectiveDateOffset shouldBe DaysAdjustment.ofBusinessDays(2, cal)
    TenorAdjustment.of(
      TENOR_3M,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING, cal)) should haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_365F
    test.defaultFixedLegDayCount shouldBe ACT_365F
    test.toString shouldBe "SGD-SIBOR-3M"
  }

  test("test_thb_thbfix") {
    val cal = HolidayCalendarId.of("THBA")
    val test = lookup("THB-THBFIX-6M")
    test.currency shouldBe THB
    test.name shouldBe "THB-THBFIX-6M"
    test.tenor shouldBe TENOR_6M
    test.fixingCalendar shouldBe cal
    test.fixingDateOffset shouldBe DaysAdjustment.ofBusinessDays(-2, cal)
    test.effectiveDateOffset shouldBe DaysAdjustment.ofBusinessDays(2, cal)
    TenorAdjustment.of(
      TENOR_6M,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING, cal)) should haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_365F
    test.defaultFixedLegDayCount shouldBe ACT_365F
    test.toString shouldBe "THB-THBFIX-6M"
  }

  test("test_twd_taibor") {
    val cal = HolidayCalendarId.of("TWTA")
    val test = lookup("TWD-TAIBOR-6M")
    test.currency shouldBe TWD
    test.name shouldBe "TWD-TAIBOR-6M"
    test.tenor shouldBe TENOR_6M
    test.fixingCalendar shouldBe cal
    test.fixingDateOffset shouldBe DaysAdjustment.ofBusinessDays(-2, cal)
    test.effectiveDateOffset shouldBe DaysAdjustment.ofBusinessDays(2, cal)
    TenorAdjustment.of(
      TENOR_6M,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING, cal)) should haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_365F
    test.defaultFixedLegDayCount shouldBe ACT_365F
    test.toString shouldBe "TWD-TAIBOR-6M"
  }

  test("test_zar_jibar") {
    val test = lookup("ZAR-JIBAR-3M")
    test.currency shouldBe ZAR
    test.name shouldBe "ZAR-JIBAR-3M"
    test.tenor shouldBe TENOR_3M
    test.fixingCalendar shouldBe ZAJO
    test.fixingDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(PRECEDING, ZAJO))
    test.effectiveDateOffset shouldBe DaysAdjustment.ofCalendarDays(0, BusinessDayAdjustment.of(FOLLOWING, ZAJO))
    TenorAdjustment.of(
      TENOR_3M,
      PeriodAdditionConventions.NONE,
      BusinessDayAdjustment.of(MODIFIED_FOLLOWING, ZAJO)) should haveValue(test.maturityDateOffset)
    test.dayCount shouldBe ACT_365F
    test.defaultFixedLegDayCount shouldBe ACT_365F
    test.toString shouldBe "ZAR-JIBAR-3M"
  }


  //-------------------------------------------------------------------------
  test("test_name") {
    forEvery(dataName) { (index: IborIndex, name: String) =>
      withClue(s"$name: ") {
        index.name shouldBe name
      }
    }
  }

  test("test_toString") {
    forEvery(dataName) { (index: IborIndex, name: String) =>
      withClue(s"$name: ") {
        index.toString shouldBe name
      }
    }
  }

  test("test_of_lookup") {
    forEvery(dataName) { (index: IborIndex, name: String) =>
      withClue(s"$name: ") {
        lookup(name) shouldBe index
        IborIndex.parse(name) should haveValue(index)
      }
    }
  }

  test("test_extendedEnum") {
    // Ruling: the Java method read the classpath registry of the family and indexed it by name.
    // No registry exists here - the family is a closed sealed set built from transcribed data
    // (Rule 4 of the request, carried by AAP §0.4.1) - so the closed-family equivalent of that
    // assertion is made: the membership is exactly the 271 published indices, their names are
    // distinct, and every name of the shared table resolves through the family's own lookup to
    // the member the table pairs it with.
    val all: List[IborIndex] = IborIndex.values.toList
    all should have size 271
    all.map(_.name).distinct should have size 271

    forEvery(dataName) { (index: IborIndex, name: String) =>
      withClue(s"$name: ") {
        IborIndex.valueOf(name) shouldBe Some(index)
        all should contain(index)
      }
    }
  }

  test("test_of_lookup_notFound") {
    // Where the Java factory raised an error for text naming no member, the port reports it as
    // a value. The reason is compared by value rather than by matching the message, so the
    // diagnostic wording of the failure stays free to change.
    IborIndex.valueOf("Rubbish") shouldBe None
    IborIndex.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
  }

  test("test_of_lookup_null") {
    // Reinterpretation: the Java method passed an absent reference to the factory and asserted
    // that it raised an error. This port writes no such reference and its lookups take a name
    // they resolve as a value, so the case is asserted as the two spellings of an absent name
    // that can actually be supplied - the empty name and a blank one - each of which names no
    // member and so resolves to a parsing failure.
    IborIndex.valueOf("") shouldBe None
    IborIndex.parse("") should beFailureWith(FailureReason.PARSING)
    IborIndex.valueOf("   ") shouldBe None
    IborIndex.parse("   ") should beFailureWith(FailureReason.PARSING)
  }

  //-------------------------------------------------------------------------
  test("test_equals") {
    // Ruling: the Java method built a custom index through a bean builder, renamed a copy of it
    // and asserted that the two were unequal. This family is closed to its 271 configured
    // members and has no constructor available outside its own file (Rule 4 of the request; the
    // `[R]` construction kind of AAP §0.3.3), so no such instance can exist and the Java body is
    // unrepresentable. The property that body asserted - that equality is decided by the name
    // and by nothing else - is asserted over configured members instead: two members with
    // different names are unequal and hash differently, and a member reached by a lookup is
    // equal to the constant naming it even though it was reached by a different route.
    val libor3m = IborIndices.GBP_LIBOR_3M
    val libor6m = IborIndices.GBP_LIBOR_6M
    Eq[IborIndex].eqv(libor3m, libor6m) shouldBe false
    (libor3m == libor6m) shouldBe false
    Hash[IborIndex].hash(libor3m) should not be Hash[IborIndex].hash(libor6m)
    libor3m.hashCode should not be libor6m.hashCode

    val resolved = lookup("GBP-LIBOR-3M")
    Eq[IborIndex].eqv(resolved, libor3m) shouldBe true
    resolved shouldBe libor3m
    Hash[IborIndex].hash(resolved) shouldBe Hash[IborIndex].hash(libor3m)
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // Ruling: the Java method swept a custom bean-built index reflectively and then read the
    // constants holder through its private constructor. Neither has a target here - the index
    // cannot be built (Rule 4) and the holder is an object with no constructor - so what those
    // two sweeps stood in for is asserted directly: the constants holder publishes exactly the
    // set of constants the original published, each naming a member of the family, and the
    // typeclass instances of the family agree with each other over those members.
    val constants: List[(String, IborIndex)] = List(
      ("GBP_LIBOR_1W", IborIndices.GBP_LIBOR_1W),
      ("GBP_LIBOR_1M", IborIndices.GBP_LIBOR_1M),
      ("GBP_LIBOR_2M", IborIndices.GBP_LIBOR_2M),
      ("GBP_LIBOR_3M", IborIndices.GBP_LIBOR_3M),
      ("GBP_LIBOR_6M", IborIndices.GBP_LIBOR_6M),
      ("GBP_LIBOR_12M", IborIndices.GBP_LIBOR_12M),
      ("CHF_LIBOR_1W", IborIndices.CHF_LIBOR_1W),
      ("CHF_LIBOR_1M", IborIndices.CHF_LIBOR_1M),
      ("CHF_LIBOR_2M", IborIndices.CHF_LIBOR_2M),
      ("CHF_LIBOR_3M", IborIndices.CHF_LIBOR_3M),
      ("CHF_LIBOR_6M", IborIndices.CHF_LIBOR_6M),
      ("CHF_LIBOR_12M", IborIndices.CHF_LIBOR_12M),
      ("EUR_LIBOR_1W", IborIndices.EUR_LIBOR_1W),
      ("EUR_LIBOR_1M", IborIndices.EUR_LIBOR_1M),
      ("EUR_LIBOR_2M", IborIndices.EUR_LIBOR_2M),
      ("EUR_LIBOR_3M", IborIndices.EUR_LIBOR_3M),
      ("EUR_LIBOR_6M", IborIndices.EUR_LIBOR_6M),
      ("EUR_LIBOR_12M", IborIndices.EUR_LIBOR_12M),
      ("JPY_LIBOR_1W", IborIndices.JPY_LIBOR_1W),
      ("JPY_LIBOR_1M", IborIndices.JPY_LIBOR_1M),
      ("JPY_LIBOR_2M", IborIndices.JPY_LIBOR_2M),
      ("JPY_LIBOR_3M", IborIndices.JPY_LIBOR_3M),
      ("JPY_LIBOR_6M", IborIndices.JPY_LIBOR_6M),
      ("JPY_LIBOR_12M", IborIndices.JPY_LIBOR_12M),
      ("USD_LIBOR_1W", IborIndices.USD_LIBOR_1W),
      ("USD_LIBOR_1M", IborIndices.USD_LIBOR_1M),
      ("USD_LIBOR_2M", IborIndices.USD_LIBOR_2M),
      ("USD_LIBOR_3M", IborIndices.USD_LIBOR_3M),
      ("USD_LIBOR_6M", IborIndices.USD_LIBOR_6M),
      ("USD_LIBOR_12M", IborIndices.USD_LIBOR_12M),
      ("EUR_EURIBOR_1W", IborIndices.EUR_EURIBOR_1W),
      ("EUR_EURIBOR_2W", IborIndices.EUR_EURIBOR_2W),
      ("EUR_EURIBOR_1M", IborIndices.EUR_EURIBOR_1M),
      ("EUR_EURIBOR_2M", IborIndices.EUR_EURIBOR_2M),
      ("EUR_EURIBOR_3M", IborIndices.EUR_EURIBOR_3M),
      ("EUR_EURIBOR_6M", IborIndices.EUR_EURIBOR_6M),
      ("EUR_EURIBOR_9M", IborIndices.EUR_EURIBOR_9M),
      ("EUR_EURIBOR_12M", IborIndices.EUR_EURIBOR_12M),
      ("JPY_TIBOR_JAPAN_1W", IborIndices.JPY_TIBOR_JAPAN_1W),
      ("JPY_TIBOR_JAPAN_1M", IborIndices.JPY_TIBOR_JAPAN_1M),
      ("JPY_TIBOR_JAPAN_2M", IborIndices.JPY_TIBOR_JAPAN_2M),
      ("JPY_TIBOR_JAPAN_3M", IborIndices.JPY_TIBOR_JAPAN_3M),
      ("JPY_TIBOR_JAPAN_6M", IborIndices.JPY_TIBOR_JAPAN_6M),
      ("JPY_TIBOR_JAPAN_12M", IborIndices.JPY_TIBOR_JAPAN_12M),
      ("JPY_TIBOR_EUROYEN_1W", IborIndices.JPY_TIBOR_EUROYEN_1W),
      ("JPY_TIBOR_EUROYEN_1M", IborIndices.JPY_TIBOR_EUROYEN_1M),
      ("JPY_TIBOR_EUROYEN_2M", IborIndices.JPY_TIBOR_EUROYEN_2M),
      ("JPY_TIBOR_EUROYEN_3M", IborIndices.JPY_TIBOR_EUROYEN_3M),
      ("JPY_TIBOR_EUROYEN_6M", IborIndices.JPY_TIBOR_EUROYEN_6M),
      ("JPY_TIBOR_EUROYEN_12M", IborIndices.JPY_TIBOR_EUROYEN_12M),
      ("AUD_BBSW_1M", IborIndices.AUD_BBSW_1M),
      ("AUD_BBSW_2M", IborIndices.AUD_BBSW_2M),
      ("AUD_BBSW_3M", IborIndices.AUD_BBSW_3M),
      ("AUD_BBSW_4M", IborIndices.AUD_BBSW_4M),
      ("AUD_BBSW_5M", IborIndices.AUD_BBSW_5M),
      ("AUD_BBSW_6M", IborIndices.AUD_BBSW_6M),
      ("CAD_CDOR_1M", IborIndices.CAD_CDOR_1M),
      ("CAD_CDOR_2M", IborIndices.CAD_CDOR_2M),
      ("CAD_CDOR_3M", IborIndices.CAD_CDOR_3M),
      ("CAD_CDOR_6M", IborIndices.CAD_CDOR_6M),
      ("CAD_CDOR_12M", IborIndices.CAD_CDOR_12M),
      ("CZK_PRIBOR_1W", IborIndices.CZK_PRIBOR_1W),
      ("CZK_PRIBOR_2W", IborIndices.CZK_PRIBOR_2W),
      ("CZK_PRIBOR_1M", IborIndices.CZK_PRIBOR_1M),
      ("CZK_PRIBOR_2M", IborIndices.CZK_PRIBOR_2M),
      ("CZK_PRIBOR_3M", IborIndices.CZK_PRIBOR_3M),
      ("CZK_PRIBOR_6M", IborIndices.CZK_PRIBOR_6M),
      ("CZK_PRIBOR_9M", IborIndices.CZK_PRIBOR_9M),
      ("CZK_PRIBOR_12M", IborIndices.CZK_PRIBOR_12M),
      ("DKK_CIBOR_1W", IborIndices.DKK_CIBOR_1W),
      ("DKK_CIBOR_2W", IborIndices.DKK_CIBOR_2W),
      ("DKK_CIBOR_1M", IborIndices.DKK_CIBOR_1M),
      ("DKK_CIBOR_2M", IborIndices.DKK_CIBOR_2M),
      ("DKK_CIBOR_3M", IborIndices.DKK_CIBOR_3M),
      ("DKK_CIBOR_6M", IborIndices.DKK_CIBOR_6M),
      ("DKK_CIBOR_9M", IborIndices.DKK_CIBOR_9M),
      ("DKK_CIBOR_12M", IborIndices.DKK_CIBOR_12M),
      ("HUF_BUBOR_1W", IborIndices.HUF_BUBOR_1W),
      ("HUF_BUBOR_2W", IborIndices.HUF_BUBOR_2W),
      ("HUF_BUBOR_1M", IborIndices.HUF_BUBOR_1M),
      ("HUF_BUBOR_2M", IborIndices.HUF_BUBOR_2M),
      ("HUF_BUBOR_3M", IborIndices.HUF_BUBOR_3M),
      ("HUF_BUBOR_6M", IborIndices.HUF_BUBOR_6M),
      ("HUF_BUBOR_9M", IborIndices.HUF_BUBOR_9M),
      ("HUF_BUBOR_12M", IborIndices.HUF_BUBOR_12M),
      ("MXN_TIIE_4W", IborIndices.MXN_TIIE_4W),
      ("MXN_TIIE_13W", IborIndices.MXN_TIIE_13W),
      ("MXN_TIIE_26W", IborIndices.MXN_TIIE_26W),
      ("NOK_NIBOR_1W", IborIndices.NOK_NIBOR_1W),
      ("NOK_NIBOR_1M", IborIndices.NOK_NIBOR_1M),
      ("NOK_NIBOR_2M", IborIndices.NOK_NIBOR_2M),
      ("NOK_NIBOR_3M", IborIndices.NOK_NIBOR_3M),
      ("NOK_NIBOR_6M", IborIndices.NOK_NIBOR_6M),
      ("NZD_BKBM_1M", IborIndices.NZD_BKBM_1M),
      ("NZD_BKBM_2M", IborIndices.NZD_BKBM_2M),
      ("NZD_BKBM_3M", IborIndices.NZD_BKBM_3M),
      ("NZD_BKBM_4M", IborIndices.NZD_BKBM_4M),
      ("NZD_BKBM_5M", IborIndices.NZD_BKBM_5M),
      ("NZD_BKBM_6M", IborIndices.NZD_BKBM_6M),
      ("PLN_WIBOR_1W", IborIndices.PLN_WIBOR_1W),
      ("PLN_WIBOR_1M", IborIndices.PLN_WIBOR_1M),
      ("PLN_WIBOR_3M", IborIndices.PLN_WIBOR_3M),
      ("PLN_WIBOR_6M", IborIndices.PLN_WIBOR_6M),
      ("PLN_WIBOR_12M", IborIndices.PLN_WIBOR_12M),
      ("SEK_STIBOR_1W", IborIndices.SEK_STIBOR_1W),
      ("SEK_STIBOR_1M", IborIndices.SEK_STIBOR_1M),
      ("SEK_STIBOR_2M", IborIndices.SEK_STIBOR_2M),
      ("SEK_STIBOR_3M", IborIndices.SEK_STIBOR_3M),
      ("SEK_STIBOR_6M", IborIndices.SEK_STIBOR_6M),
      ("ZAR_JIBAR_1M", IborIndices.ZAR_JIBAR_1M),
      ("ZAR_JIBAR_3M", IborIndices.ZAR_JIBAR_3M),
      ("ZAR_JIBAR_6M", IborIndices.ZAR_JIBAR_6M),
      ("ZAR_JIBAR_12M", IborIndices.ZAR_JIBAR_12M)
    )
    // 113 constants naming 113 distinct members, which is exactly the set the original
    // declared. The remaining 158 published indices have no constant, also as in the original,
    // and are reached through the lookup - which is what the term-rate bodies above do.
    constants should have size 113
    constants.map(_._1).distinct should have size 113
    val members: List[IborIndex] = constants.map(_._2)
    members.distinct should have size 113

    val all: List[IborIndex] = IborIndex.values.toList
    constants.foreach { case (label, index) =>
      withClue(s"$label: ") {
        all should contain(index)
        IborIndex.valueOf(index.name) shouldBe Some(index)
        IborIndex.parse(index.name) should haveValue(index)
        Show[IborIndex].show(index) shouldBe index.name
        index.toString shouldBe index.name
        // the tenor is read from the maturity offset rather than stored twice, so the name of a
        // member always ends in the canonical text of the tenor it reports
        index.name should endWith("-" + index.tenor.name)
      }
    }

    // The companion publishes one equality-bearing instance - an ordering that is also a
    // hashing - so summoning the equality, the hashing or the ordering yields that one value
    // and the three can never disagree. A representative pair of members observes that.
    val sample: List[IborIndex] =
      List(IborIndices.GBP_LIBOR_3M, IborIndices.GBP_LIBOR_6M)
    for (left <- sample; right <- sample) {
      val sameValue = left == right
      withClue(s"${left.name} against ${right.name}: ") {
        Eq[IborIndex].eqv(left, right) shouldBe sameValue
        Hash[IborIndex].eqv(left, right) shouldBe sameValue
        // the ordering is consistent with equality: it compares equal exactly when the two
        // values are equal, which is the law the combined instance has to satisfy
        (Order[IborIndex].compare(left, right) == 0) shouldBe sameValue
        if (sameValue) {
          Hash[IborIndex].hash(left) shouldBe Hash[IborIndex].hash(right)
        } else {
          Order[IborIndex].compare(left, right) should not be 0
        }
      }
    }
  }

  test("test_jodaConvert") {
    // Ruling: the Java method asserted the round trip of the reflective string-conversion
    // library the original annotated this family for. That library is not on the classpath of
    // this port (Rule 1 of the request, AAP §0.2.2), and the guarantee its two annotations gave
    // is asserted directly: a member renders as its name, and that rendering reads back as the
    // same member. The subject is the one the Java method named. This is the text round trip;
    // the JSON round trip is test_serialization below, and the two are kept apart because the
    // representations they pin are independent of each other.
    val rendered = Show[IborIndex].show(IborIndices.GBP_LIBOR_12M)
    rendered shouldBe "GBP-LIBOR-12M"
    rendered shouldBe IborIndices.GBP_LIBOR_12M.name
    IborIndex.parse(rendered) should haveValue(IborIndices.GBP_LIBOR_12M)

    forEvery(dataName) { (index: IborIndex, name: String) =>
      withClue(s"$name: ") {
        Show[IborIndex].show(index) shouldBe name
        IborIndex.parse(Show[IborIndex].show(index)) should haveValue(index)
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
    val encoded = IborIndices.GBP_LIBOR_3M.asJson
    encoded shouldBe Json.fromString("GBP-LIBOR-3M")
    encoded.as[IborIndex] shouldBe Right(IborIndices.GBP_LIBOR_3M)

    forEvery(dataName) { (index: IborIndex, name: String) =>
      withClue(s"$name: ") {
        index.asJson shouldBe Json.fromString(name)
        Json.fromString(name).as[IborIndex] shouldBe Right(index)
      }
    }

    // a string naming no member of the family is rejected by the reader
    Json.fromString("Rubbish").as[IborIndex].isLeft shouldBe true
  }
}
