/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import java.util.Locale

import scala.collection.immutable.SortedSet

import cats.Hash
import cats.Order
import cats.Show

import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor1
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableFor3

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.basics.date.BusinessDayConventions
import com.opengamma.strata.basics.date.DaysAdjustment
import com.opengamma.strata.basics.date.HolidayCalendarIds
import com.opengamma.strata.basics.date.Tenor
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[FloatingRateName]] and the constants [[FloatingRateNames]] publishes.
 *
 * [[FloatingRateName.values]] holds '''351''' members, one per published row (159 Ibor + 156
 * Overnight compounded + 6 Overnight averaged + 30 price), while [[FloatingRateNames]] publishes
 * '''41''' named constants: the constants are the subset the library ships pricing data for, the
 * other 310 rows being alternative and historical spellings modelled by name alone. The first
 * count is asserted in `test_normalized` and `test_getFloatingRateName`, the second in
 * `test_types` and `coverage`. Unlike the four index families, this one declares no alternate-name,
 * lenient-rewrite or external-group table, so those 351 names are the whole of its name space and
 * no alias row is asserted; row-for-row fidelity against the captured reference data belongs to
 * `ReferenceDataManifestSpec`.
 *
 * The conversion surface reports `INVALID` for a name of the wrong kind for the conversion asked
 * for, `PARSING` for a name of the right kind whose index or tenor is not published, and
 * `MISSING_DATA` where the data names nothing to answer with - an Ibor family whose every index
 * has been retired, a currency with no default rate. A failure is compared by reason, not by
 * message text, and an `Either` is unwrapped by a matcher, except in `test_name`, which folds the
 * conversion it needs the index of so that the failure branch fails the row.
 *
 * Twelve tests have no Java counterpart: three named in prose, nine prefixed `data_`. The three
 * state that the family has one lookup, the [[com.opengamma.strata.collect.named.NamedEnum]] its
 * companion publishes, that JSON is read and written through it, and that both rows of each pair
 * declaring one rate twice in the case of one word stay reachable - a lookup claiming folded
 * spellings ahead of published names would leave a member no text could reach. The nine assert the
 * structure of the transcribed table the members are created from, `FloatingRateNameData`.
 */
class FloatingRateNamesSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /** The number of members the published name table declares, which several tests sweep. */
  private val PublishedNameCount: Int = 351

  /** The number of named constants [[FloatingRateNames]] publishes. */
  private val ConstantCount: Int = 41

  //-------------------------------------------------------------------------
  /**
   * Published names, each with the index name it resolves to and the kind of rate it describes.
   *
   * Names carrying an embedded space, dot or equals sign - `USD-Federal Funds-H.15`,
   * `CNY-CNREPOFIX=CFXS-Reuters`, `HKD-HIBOR-HIBOR=` - are transcribed as published, a published
   * name being the text that arrives in an FpML message. The second column is inconsistent about
   * the trailing `-` of an Ibor index name, which `test_name` absorbs.
   */
  private val dataNameType: TableFor3[String, String, FloatingRateType] =
    Table(
      ("name", "indexName", "rateType"),
      ("GBP-LIBOR", "GBP-LIBOR-", FloatingRateType.Ibor),
      ("GBP-LIBOR-BBA", "GBP-LIBOR-", FloatingRateType.Ibor),
      ("CHF-LIBOR", "CHF-LIBOR-", FloatingRateType.Ibor),
      ("CHF-LIBOR-BBA", "CHF-LIBOR-", FloatingRateType.Ibor),
      ("EUR-LIBOR", "EUR-LIBOR-", FloatingRateType.Ibor),
      ("EUR-LIBOR-BBA", "EUR-LIBOR-", FloatingRateType.Ibor),
      ("JPY-LIBOR", "JPY-LIBOR-", FloatingRateType.Ibor),
      ("JPY-LIBOR-BBA", "JPY-LIBOR-", FloatingRateType.Ibor),
      ("USD-LIBOR", "USD-LIBOR-", FloatingRateType.Ibor),
      ("USD-LIBOR-BBA", "USD-LIBOR-", FloatingRateType.Ibor),
      ("EUR-EURIBOR", "EUR-EURIBOR-", FloatingRateType.Ibor),
      ("EUR-EURIBOR-Reuters", "EUR-EURIBOR-", FloatingRateType.Ibor),
      ("JPY-TIBOR-JAPAN", "JPY-TIBOR-JAPAN-", FloatingRateType.Ibor),
      ("JPY-TIBOR-TIBM", "JPY-TIBOR-JAPAN-", FloatingRateType.Ibor),
      ("GBP-SONIA", "GBP-SONIA", FloatingRateType.OvernightCompounded),
      ("GBP-WMBA-SONIA-COMPOUND", "GBP-SONIA", FloatingRateType.OvernightCompounded),
      ("GBP-SONIA-COMPOUND", "GBP-SONIA", FloatingRateType.OvernightCompounded),
      ("CHF-SARON", "CHF-SARON", FloatingRateType.OvernightCompounded),
      ("CHF-SARON-OIS-COMPOUND", "CHF-SARON", FloatingRateType.OvernightCompounded),
      ("CHF-TOIS", "CHF-TOIS", FloatingRateType.OvernightCompounded),
      ("CHF-TOIS-OIS-COMPOUND", "CHF-TOIS", FloatingRateType.OvernightCompounded),
      ("EUR-EONIA", "EUR-EONIA", FloatingRateType.OvernightCompounded),
      ("EUR-EONIA-OIS-COMPOUND", "EUR-EONIA", FloatingRateType.OvernightCompounded),
      ("EUR-ESTR", "EUR-ESTR", FloatingRateType.OvernightCompounded),
      ("EUR-ESTR-COMPOUND", "EUR-ESTR", FloatingRateType.OvernightCompounded),
      ("EUR-ESTER", "EUR-ESTR", FloatingRateType.OvernightCompounded),
      ("EUR-ESTER-OIS-COMPOUND", "EUR-ESTR", FloatingRateType.OvernightCompounded),
      ("JPY-TONAR", "JPY-TONAR", FloatingRateType.OvernightCompounded),
      ("JPY-TONA-OIS-COMPOUND", "JPY-TONAR", FloatingRateType.OvernightCompounded),
      ("USD-FED-FUND", "USD-FED-FUND", FloatingRateType.OvernightCompounded),
      ("USD-Federal Funds-H.15-OIS-COMPOUND", "USD-FED-FUND", FloatingRateType.OvernightCompounded),
      ("USD-FED-FUND-AVG", "USD-FED-FUND-AVG", FloatingRateType.OvernightAveraged),
      ("USD-Federal Funds-H.15", "USD-FED-FUND-AVG", FloatingRateType.OvernightAveraged),
      ("GB-HICP", "GB-HICP", FloatingRateType.Price),
      ("UK-HICP", "GB-HICP", FloatingRateType.Price),
      ("GB-RPI", "GB-RPI", FloatingRateType.Price),
      ("UK-RPI", "GB-RPI", FloatingRateType.Price),
      ("GB-RPIX", "GB-RPIX", FloatingRateType.Price),
      ("UK-RPIX", "GB-RPIX", FloatingRateType.Price),
      ("CH-CPI", "CH-CPI", FloatingRateType.Price),
      ("SWF-CPI", "CH-CPI", FloatingRateType.Price),
      ("EU-AI-CPI", "EU-AI-CPI", FloatingRateType.Price),
      ("EUR-AI-CPI", "EU-AI-CPI", FloatingRateType.Price),
      ("EU-EXT-CPI", "EU-EXT-CPI", FloatingRateType.Price),
      ("EUR-EXT-CPI", "EU-EXT-CPI", FloatingRateType.Price),
      ("JP-CPI-EXF", "JP-CPI-EXF", FloatingRateType.Price),
      ("JPY-CPI-EXF", "JP-CPI-EXF", FloatingRateType.Price),
      ("US-CPI-U", "US-CPI-U", FloatingRateType.Price),
      ("USA-CPI-U", "US-CPI-U", FloatingRateType.Price),
      ("FR-EXT-CPI", "FR-EXT-CPI", FloatingRateType.Price),
      ("FRC-EXT-CPI", "FR-EXT-CPI", FloatingRateType.Price),
      ("AUD-BBR-BBSW", "AUD-BBSW", FloatingRateType.Ibor),
      ("CAD-BA-CDOR", "CAD-CDOR", FloatingRateType.Ibor),
      ("CNY-CNREPOFIX=CFXS-Reuters", "CNY-REPO", FloatingRateType.Ibor),
      ("CZK-PRIBOR-PRBO", "CZK-PRIBOR", FloatingRateType.Ibor),
      ("DKK-CIBOR-DKNA13", "DKK-CIBOR", FloatingRateType.Ibor),
      ("HKD-HIBOR-ISDC", "HKD-HIBOR", FloatingRateType.Ibor),
      ("HKD-HIBOR-HIBOR=", "HKD-HIBOR", FloatingRateType.Ibor),
      ("HUF-BUBOR-Reuters", "HUF-BUBOR", FloatingRateType.Ibor),
      ("KRW-CD-KSDA-Bloomberg", "KRW-CD", FloatingRateType.Ibor),
      ("MXN-TIIE-Banxico", "MXN-TIIE", FloatingRateType.Ibor),
      ("MYR-KLIBOR-BNM", "MYR-KLIBOR", FloatingRateType.Ibor),
      ("NOK-NIBOR-OIBOR", "NOK-NIBOR", FloatingRateType.Ibor),
      ("NZD-BBR-FRA", "NZD-BKBM", FloatingRateType.Ibor),
      ("PLN-WIBOR-WIBO", "PLN-WIBOR", FloatingRateType.Ibor),
      ("SEK-STIBOR-Bloomberg", "SEK-STIBOR", FloatingRateType.Ibor),
      ("SGD-SOR-VWAP", "SGD-SOR", FloatingRateType.Ibor),
      ("ZAR-JIBAR-SAFEX", "ZAR-JIBAR", FloatingRateType.Ibor),
      ("HKD-HONIA-OIS-COMPOUND", "HKD-HONIA", FloatingRateType.OvernightCompounded),
      ("HKD-HONIX-OIS-COMPOUND", "HKD-HONIA", FloatingRateType.OvernightCompounded),
      ("INR-MIBOR-OIS-COMPOUND", "INR-OMIBOR", FloatingRateType.OvernightCompounded),
      ("PLN-POLSTR", "PLN-POLSTR", FloatingRateType.OvernightCompounded),
      ("PLN-POLSTR-OIS-COMPOUND", "PLN-POLSTR", FloatingRateType.OvernightCompounded),
      ("RUB-RUONIA-OIS-COMPOUND", "RUB-RUONIA", FloatingRateType.OvernightCompounded),
      ("SGD-SORA-COMPOUND", "SGD-SORA", FloatingRateType.OvernightCompounded),
      ("TRY-TLREF-OIS-COMPOUND", "TRY-TLREF", FloatingRateType.OvernightCompounded))

  /**
   * The whole of the closed family as a table, so that a sweep reports every member that fails
   * rather than stopping at the first.
   */
  private val allPublishedNames: TableFor1[FloatingRateName] =
    Table("floatingRateName", FloatingRateName.values.toList: _*)

  /**
   * The two rates the published data declares twice, differing in the case of one word, each as
   * the mixed-case published name beside the upper-case one. The upper-case spelling of the first
   * is '''exactly''' the published name of the second, which makes these four rows the place
   * where the order in which a lookup claims a published name and a folded one is observable.
   */
  private val caseDifferingPairs: TableFor2[String, String] =
    Table(
      ("mixedCaseName", "upperCaseName"),
      ("DKK-DESTR-OIS Compound", "DKK-DESTR-OIS COMPOUND"),
      ("SEK-SWESTR-OIS Compound", "SEK-SWESTR-OIS COMPOUND"))

  /**
   * The number of distinct folded keys the published names have: two fewer than the member count,
   * the rows of each pair in [[caseDifferingPairs]] folding to one key, held by the upper-case one.
   */
  private val FoldedKeyCount: Int = PublishedNameCount - 2

  /**
   * Every constant [[FloatingRateNames]] publishes, in declaration order, with the element type
   * stated so that the compiler checks the type of each constant named. The list is maintained by
   * hand: it covers the constants it names and no others.
   */
  private val allConstants: List[FloatingRateName] =
    List(
      FloatingRateNames.GBP_LIBOR,
      FloatingRateNames.USD_LIBOR,
      FloatingRateNames.USD_BSBY,
      FloatingRateNames.CHF_LIBOR,
      FloatingRateNames.EUR_LIBOR,
      FloatingRateNames.JPY_LIBOR,
      FloatingRateNames.EUR_EURIBOR,
      FloatingRateNames.AUD_BBSW,
      FloatingRateNames.CAD_CDOR,
      FloatingRateNames.CZK_PRIBOR,
      FloatingRateNames.DKK_CIBOR,
      FloatingRateNames.HUF_BUBOR,
      FloatingRateNames.MXN_TIIE,
      FloatingRateNames.NOK_NIBOR,
      FloatingRateNames.NZD_BKBM,
      FloatingRateNames.PLN_WIBOR,
      FloatingRateNames.SEK_STIBOR,
      FloatingRateNames.ZAR_JIBAR,
      FloatingRateNames.GBP_SONIA,
      FloatingRateNames.USD_FED_FUND,
      FloatingRateNames.USD_SOFR,
      FloatingRateNames.CHF_SARON,
      FloatingRateNames.CHF_TOIS,
      FloatingRateNames.EUR_EONIA,
      FloatingRateNames.EUR_ESTR,
      FloatingRateNames.EUR_ESTER,
      FloatingRateNames.JPY_TONAR,
      FloatingRateNames.AUD_AONIA,
      FloatingRateNames.BRL_CDI,
      FloatingRateNames.CAD_CORRA,
      FloatingRateNames.DKK_TNR,
      FloatingRateNames.NOK_NOWA,
      FloatingRateNames.PLN_POLONIA,
      FloatingRateNames.PLN_POLSTR,
      FloatingRateNames.SEK_SIOR,
      FloatingRateNames.THB_THOR,
      FloatingRateNames.USD_FED_FUND_AVG,
      FloatingRateNames.GB_RPI,
      FloatingRateNames.EU_EXT_CPI,
      FloatingRateNames.US_CPI_U,
      FloatingRateNames.FR_EXT_CPI)

  //-------------------------------------------------------------------------
  /**
   * Resolves a published name through the family's exact lookup, failing the test where the family
   * has no such member.
   *
   * @param externalName  the published external name, such as `GBP-LIBOR-BBA`
   * @return the member of that name
   */
  private def named(externalName: String): FloatingRateName =
    FloatingRateName.valueOf(externalName) match {
      case Some(value) => value
      case None => fail(s"Not a published floating rate name: '$externalName'")
    }

  //-------------------------------------------------------------------------
  test("test_name") {
    forEvery(dataNameType) { (name: String, indexName: String, rateType: FloatingRateType) =>
      withClue(s"'$name': ") {
        val test = named(name)
        test.name shouldBe name

        // The `-` is genuinely part of an Ibor index name - the Ibor rows append it so that a
        // tenor completes the name - and `normalized` is the operation that drops it again.
        val expectedNormalized =
          if (indexName.endsWith("-")) indexName.dropRight(1) else indexName
        test.normalized.map(normalized => normalized.name) should haveValue(expectedNormalized)

        test.rateType shouldBe rateType

        // A name derives its currency from the index it converts to, so the accessor is
        // `Either`-valued. Comparing it against the index's own currency would also be satisfied
        // by a failed conversion - both sides carrying one failure - so the index comes first.
        val index: FloatingRateIndex = test.toFloatingRateIndex.fold(
          (failure: Failure) =>
            fail(s"expected a conversion to an index, but it failed: ${failure.message}"),
          (resolved: FloatingRateIndex) => resolved)
        test.currency should haveValue(index.currency)
      }
    }
  }

  test("test_toString") {
    forEvery(dataNameType) { (name: String, _: String, _: FloatingRateType) =>
      withClue(s"'$name': ") {
        named(name).toString shouldBe name
      }
    }
  }

  test("test_of_lookup") {
    forEvery(dataNameType) { (name: String, _: String, _: FloatingRateType) =>
      withClue(s"'$name': ") {
        val test = FloatingRateName.valueOf(name)
        test.isDefined shouldBe true
        FloatingRateName.valueOf(name) shouldBe test
      }
    }
  }

  test("test_of_lookup_notFound") {
    FloatingRateName.valueOf("Rubbish") shouldBe None
    FloatingRateName.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
  }

  test("test_of_lookup_null") {
    FloatingRateName.valueOf("") shouldBe None
    FloatingRateName.parse("") should beFailureWith(FailureReason.PARSING)
    FloatingRateName.valueOf("   ") shouldBe None
    FloatingRateName.parse("   ") should beFailureWith(FailureReason.PARSING)
  }

  test("test_parse") {
    FloatingRateName.parse("GBP-LIBOR") should haveValue(FloatingRateNames.GBP_LIBOR)
    // A name naming a concrete index resolves to its family, losing the tenor: the wider
    // resolution falls back through the index families, the exact lookup does not.
    FloatingRateName.parse("GBP-LIBOR-3M") should haveValue(FloatingRateNames.GBP_LIBOR)
    FloatingRateName.parse("GBP-SONIA") should haveValue(FloatingRateNames.GBP_SONIA)
    FloatingRateName.parse("GB-RPI") should haveValue(FloatingRateNames.GB_RPI)
    FloatingRateName.parse("NotAnIndex") should beFailureWith(FailureReason.PARSING)
  }

  test("test_tryParse") {
    FloatingRateName.tryParse("GBP-LIBOR") shouldBe Some(FloatingRateNames.GBP_LIBOR)
    FloatingRateName.tryParse("GBP-LIBOR-3M") shouldBe Some(FloatingRateNames.GBP_LIBOR)
    FloatingRateName.tryParse("GBP-SONIA") shouldBe Some(FloatingRateNames.GBP_SONIA)
    FloatingRateName.tryParse("GB-RPI") shouldBe Some(FloatingRateNames.GB_RPI)
    FloatingRateName.tryParse("NotAnIndex") shouldBe None
  }

  //-------------------------------------------------------------------------
  test("test_defaultIborIndex") {
    FloatingRateName.defaultIborIndex(Currency.GBP) should haveValue(FloatingRateNames.GBP_LIBOR)
    FloatingRateName.defaultIborIndex(Currency.EUR) should haveValue(FloatingRateNames.EUR_EURIBOR)
    FloatingRateName.defaultIborIndex(Currency.USD) should haveValue(FloatingRateNames.USD_LIBOR)
    FloatingRateName.defaultIborIndex(Currency.AUD) should haveValue(FloatingRateNames.AUD_BBSW)
    FloatingRateName.defaultIborIndex(Currency.CAD) should haveValue(FloatingRateNames.CAD_CDOR)
    FloatingRateName.defaultIborIndex(Currency.NZD) should haveValue(FloatingRateNames.NZD_BKBM)

    // Only 23 currencies publish a default term rate: the Brazilian real is in the Overnight
    // defaults and not in these, and the reported miss is half the accessor's contract.
    FloatingRateName.defaultIborIndex(Currency.BRL) should beFailureWith(FailureReason.MISSING_DATA)
  }

  test("test_defaultOvernightIndex") {
    FloatingRateName.defaultOvernightIndex(Currency.GBP) should haveValue(named("GBP-SONIA"))
    FloatingRateName.defaultOvernightIndex(Currency.EUR) should haveValue(named("EUR-ESTR"))
    FloatingRateName.defaultOvernightIndex(Currency.USD) should haveValue(named("USD-SOFR"))

    // The Overnight defaults cover 27 currencies; gold has no overnight rate to default to.
    FloatingRateName.defaultOvernightIndex(Currency.XAU) should
      beFailureWith(FailureReason.MISSING_DATA)
  }

  //-------------------------------------------------------------------------
  test("test_normalized") {
    named("GBP-LIBOR-BBA").normalized should haveValue(named("GBP-LIBOR"))
    named("GBP-WMBA-SONIA-COMPOUND").normalized should haveValue(named("GBP-SONIA"))

    // A failure here would mean a family whose canonical name is not itself published.
    FloatingRateName.values.size shouldBe PublishedNameCount
    forEvery(allPublishedNames) { (value: FloatingRateName) =>
      withClue(s"'${value.name}': ") {
        value.normalized should beSuccess
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_getFloatingRateName") {
    // A concrete index reports the family it belongs to; a family reports itself.
    FloatingRateName.values.size shouldBe PublishedNameCount
    forEvery(allPublishedNames) { (value: FloatingRateName) =>
      withClue(s"'${value.name}': ") {
        value.floatingRateName shouldBe value
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_iborIndex_tenor") {
    val test = named("GBP-LIBOR-BBA")
    test.defaultTenor should haveValue(Tenor.TENOR_3M)
    test.toFloatingRateIndex should haveValue(IborIndices.GBP_LIBOR_3M)
    test.toFloatingRateIndex(Tenor.TENOR_1M) should haveValue(IborIndices.GBP_LIBOR_1M)
    test.toIborIndex(Tenor.TENOR_6M) should haveValue(IborIndices.GBP_LIBOR_6M)
    test.toIborIndex(Tenor.TENOR_12M) should haveValue(IborIndices.GBP_LIBOR_12M)
    // One year and twelve months are one period, normalized before completing the index name.
    test.toIborIndex(Tenor.TENOR_1Y) should haveValue(IborIndices.GBP_LIBOR_12M)

    named("GBP-WMBA-SONIA-COMPOUND").toIborIndex(Tenor.TENOR_6M) should
      beFailureWith(FailureReason.INVALID)

    // A tenor the family publishes no index for is the other failure: the kind matches and the
    // index the name and tenor resolve to is what is missing, so it is `PARSING` - a caller can
    // retry a different tenor, but not a different kind.
    test.toIborIndex(Tenor.TENOR_1D) should beFailureWith(FailureReason.PARSING)

    // The tenor set is ordered by the length of a tenor.
    val tenors: SortedSet[Tenor] = test.tenors
    tenors.toList shouldBe
      List(
        Tenor.TENOR_1W,
        Tenor.TENOR_1M,
        Tenor.TENOR_2M,
        Tenor.TENOR_3M,
        Tenor.TENOR_6M,
        Tenor.TENOR_12M)

    test.toIborIndexFixingOffset should haveValue(
      DaysAdjustment.ofCalendarDays(
        0,
        BusinessDayAdjustment.of(BusinessDayConventions.PRECEDING, HolidayCalendarIds.GBLO)))

    // The default tenor tries three months, then thirteen weeks, then the shortest tenor the
    // family has. The third branch is reached by the Chinese repo rate, active at one week only.
    val shortestTenorOnly = named("CNY-REPO")
    shortestTenorOnly.tenors.toList shouldBe List(Tenor.TENOR_1W)
    shortestTenorOnly.defaultTenor should haveValue(Tenor.TENOR_1W)
    shortestTenorOnly.toFloatingRateIndex.map(index => index.name) should haveValue("CNY-REPO-1W")

    // An Ibor family whose every index has been retired has no tenor to offer, so every accessor
    // needing a default tenor propagates one `MISSING_DATA`; euroyen TIBOR reaches that case.
    val everyIndexRetired = named("JPY-TIBOR-EUROYEN")
    everyIndexRetired.tenors.toList shouldBe List.empty[Tenor]
    everyIndexRetired.defaultTenor should beFailureWith(FailureReason.MISSING_DATA)
    everyIndexRetired.toFloatingRateIndex should beFailureWith(FailureReason.MISSING_DATA)
    everyIndexRetired.currency should beFailureWith(FailureReason.MISSING_DATA)

    // An explicit tenor needs no default: the retired indices are published, simply inactive.
    everyIndexRetired.toIborIndex(Tenor.TENOR_3M) should
      haveValue(IborIndices.JPY_TIBOR_EUROYEN_3M)
  }

  test("test_overnightIndex") {
    val test = named("GBP-WMBA-SONIA-COMPOUND")
    test.defaultTenor should haveValue(Tenor.TENOR_1D)
    test.toFloatingRateIndex should haveValue(OvernightIndices.GBP_SONIA)
    // An Overnight name has no tenor to choose, so a tenor supplied is ignored, not rejected.
    test.toFloatingRateIndex(Tenor.TENOR_1M) should haveValue(OvernightIndices.GBP_SONIA)
    test.toOvernightIndex should haveValue(OvernightIndices.GBP_SONIA)

    FloatingRateNames.USD_FED_FUND.toOvernightIndex should haveValue(OvernightIndices.USD_FED_FUND)
    // The averaging name carries a `-AVG` suffix the published index does not, so it is stripped.
    FloatingRateNames.USD_FED_FUND_AVG.toOvernightIndex should
      haveValue(OvernightIndices.USD_FED_FUND)

    named("GBP-LIBOR-BBA").toOvernightIndex should beFailureWith(FailureReason.INVALID)

    // As with the Ibor conversion, a name of the right kind whose index is not published is
    // `PARSING`, not `INVALID`. The names of this shape are term risk-free rates published as
    // overnight compounded names: a rate modelled by name without the data to price it.
    val indexNotPublished = named("JPY-TORF")
    indexNotPublished.rateType shouldBe FloatingRateType.OvernightCompounded
    indexNotPublished.toOvernightIndex should beFailureWith(FailureReason.PARSING)
    indexNotPublished.toFloatingRateIndex should beFailureWith(FailureReason.PARSING)
    indexNotPublished.currency should beFailureWith(FailureReason.PARSING)

    val tenors: SortedSet[Tenor] = test.tenors
    tenors.toList shouldBe List.empty[Tenor]

    test.toIborIndexFixingOffset should beFailureWith(FailureReason.INVALID)
  }

  test("test_priceIndex") {
    // `UK-HICP` is a published floating rate name and not a published price index name: it
    // resolves to `GB-HICP`. The two name spaces overlap, so the expected value states its type.
    val test = named("UK-HICP")
    test.defaultTenor should haveValue(Tenor.TENOR_1Y)
    val expectedIndex: PriceIndex = PriceIndices.GB_HICP
    test.toFloatingRateIndex should haveValue(expectedIndex)
    test.toFloatingRateIndex(Tenor.TENOR_1M) should haveValue(expectedIndex)
    test.toPriceIndex should haveValue(expectedIndex)

    named("GBP-LIBOR-BBA").toPriceIndex should beFailureWith(FailureReason.INVALID)

    val tenors: SortedSet[Tenor] = test.tenors
    tenors.toList shouldBe List.empty[Tenor]

    test.toIborIndexFixingOffset should beFailureWith(FailureReason.INVALID)
  }

  //-------------------------------------------------------------------------
  test("test_cibor") {
    val test = named("DKK-CIBOR-DKNA13")
    test.defaultTenor should haveValue(Tenor.TENOR_3M)
    test.toFloatingRateIndex should haveValue(IborIndices.DKK_CIBOR_3M)
    test.toFloatingRateIndex(Tenor.TENOR_1M) should haveValue(IborIndices.DKK_CIBOR_1M)
    test.toIborIndex(Tenor.TENOR_6M) should haveValue(IborIndices.DKK_CIBOR_6M)
    // Two published names share one index, so the fixing offset is carried by the name.
    named("DKK-CIBOR2-DKNA13").toIborIndex(Tenor.TENOR_6M) should
      haveValue(IborIndices.DKK_CIBOR_6M)

    // `DKK-CIBOR-DKNA13` declares an offset of zero days moved back to the preceding business
    // day, while `DKK-CIBOR2-DKNA13` keeps the index's own two-business-day offset.
    test.toIborIndexFixingOffset should haveValue(
      DaysAdjustment.ofCalendarDays(
        0,
        BusinessDayAdjustment.of(BusinessDayConventions.PRECEDING, HolidayCalendarIds.DKCO)))
    named("DKK-CIBOR2-DKNA13").toIborIndexFixingOffset should
      haveValue(DaysAdjustment.ofBusinessDays(-2, HolidayCalendarIds.DKCO))
  }

  test("test_tiee") {
    // The Mexican rates publish no three-month index, so the default falls to thirteen weeks.
    val test = named("MXN-TIIE")
    test.defaultTenor should haveValue(Tenor.TENOR_13W)
    test.toFloatingRateIndex should haveValue(IborIndices.MXN_TIIE_13W)
    test.toFloatingRateIndex(Tenor.TENOR_4W) should haveValue(IborIndices.MXN_TIIE_4W)
    test.toIborIndex(Tenor.TENOR_4W) should haveValue(IborIndices.MXN_TIIE_4W)
    test.toIborIndexFixingOffset should
      haveValue(DaysAdjustment.ofBusinessDays(-1, HolidayCalendarIds.MXMC))
  }

  test("test_nzd") {
    named("NZD-BKBM").currency should haveValue(Currency.NZD)
    named("NZD-NZIONA").currency should haveValue(Currency.NZD)
  }

  //-------------------------------------------------------------------------
  test("test_types") {
    // `allConstants` names the 41 published constants with its element type stated, so a constant
    // of the wrong type fails the build rather than this test. The list is maintained by hand: it
    // covers the constants it names and no others, so a constant added to `FloatingRateNames` and
    // left out of the list is not reached here at all.
    allConstants should have size ConstantCount.toLong
    allConstants.distinct should have size ConstantCount.toLong

    // Each constant named is the published member of its own name, not merely of the right type.
    forEvery(Table("constant", allConstants: _*)) { (value: FloatingRateName) =>
      withClue(s"'${value.name}': ") {
        FloatingRateName.valueOf(value.name) shouldBe Some(value)
      }
    }
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    allConstants.distinct should have size ConstantCount.toLong

    val test = named("GBP-LIBOR-BBA")
    val other = named("USD-Federal Funds-H.15")

    Order[FloatingRateName].eqv(test, test) shouldBe true
    Order[FloatingRateName].eqv(test, other) shouldBe false
    Hash[FloatingRateName].hash(test) shouldBe test.name.hashCode
    Hash[FloatingRateName].hash(test) should not be Hash[FloatingRateName].hash(other)
    Show[FloatingRateName].show(test) shouldBe test.name
    Show[FloatingRateName].show(other) shouldBe other.name

    // Ordering, hashing and equality are one instance over the external name.
    Order[FloatingRateName].compare(test, test) shouldBe 0
    Order[FloatingRateName].compare(test, other) should not be 0
    Order[FloatingRateName].compare(test, other).sign shouldBe
      test.name.compareTo(other.name).sign
  }

  test("test_jodaConvert") {
    val subjects: List[FloatingRateName] =
      List(named("GBP-LIBOR-BBA"), named("USD-Federal Funds-H.15"))
    forEvery(Table("subject", subjects: _*)) { (value: FloatingRateName) =>
      withClue(s"'${value.name}': ") {
        val rendered = Show[FloatingRateName].show(value)
        rendered shouldBe value.name
        rendered shouldBe value.toString
        FloatingRateName.valueOf(rendered) shouldBe Some(value)
        FloatingRateName.parse(rendered) should haveValue(value)
      }
    }
  }

  test("test_serialization") {
    val subjects: List[FloatingRateName] =
      List(
        named("GBP-LIBOR-BBA"),
        named("USD-Federal Funds-H.15"),
        named("GBP-SONIA"),
        named("UK-HICP"))
    forEvery(Table("subject", subjects: _*)) { (value: FloatingRateName) =>
      withClue(s"'${value.name}': ") {
        val encoded = Encoder[FloatingRateName].apply(value)
        encoded shouldBe Json.fromString(value.name)
        Decoder[FloatingRateName].decodeJson(encoded) shouldBe Right(value)
      }
    }

    Decoder[FloatingRateName].decodeJson(Json.fromString("Rubbish")).isLeft shouldBe true
    // A name naming a concrete index is rejected too: reading it as a family loses the tenor.
    Decoder[FloatingRateName].decodeJson(Json.fromString("GBP-LIBOR-3M")).isLeft shouldBe true
  }

  //-------------------------------------------------------------------------
  // The three tests below have no Java counterpart; see the note on this suite.
  //-------------------------------------------------------------------------
  test("the family has one lookup and every published name reaches its own member through it") {
    // `FloatingRateName.valueOf` is the family's `NamedEnum` lookup and no second table sits
    // beside it, so every name is resolved both ways and both answers compared with its member.
    FloatingRateName.values.size shouldBe PublishedNameCount
    forEvery(allPublishedNames) { (value: FloatingRateName) =>
      withClue(s"'${value.name}': ") {
        FloatingRateName.valueOf(value.name) shouldBe Some(value)
        NamedEnum[FloatingRateName].valueOf(value.name) shouldBe Some(value)
      }
    }

    // The normalised view holds one entry per member, the folded view two fewer - the pairs that
    // differ in case alone, which is the narrowing the published data forces.
    NamedEnum[FloatingRateName].byCanonicalName.size shouldBe PublishedNameCount
    NamedEnum[FloatingRateName].byUpperName.size shouldBe FoldedKeyCount
    forEvery(allPublishedNames) { (value: FloatingRateName) =>
      withClue(s"'${value.name}': ") {
        NamedEnum[FloatingRateName].byCanonicalName.get(value.name) shouldBe Some(value)
      }
    }

    FloatingRateName.valueOf("Rubbish") shouldBe None
    FloatingRateName.valueOf("GBP-LIBOR-3M") shouldBe None
    NamedEnum[FloatingRateName].valueOf("GBP-LIBOR-3M") shouldBe None
  }

  test("both rows of each case-differing pair resolve to themselves and round trip through JSON") {
    forEvery(caseDifferingPairs) { (mixedCaseName: String, upperCaseName: String) =>
      withClue(s"'$mixedCaseName' / '$upperCaseName': ") {
        // The folded spelling of the first row is the published name of the second, and the two
        // are distinct members, so a lookup answering the second with the first would leave a
        // member unreachable.
        mixedCaseName.toUpperCase(Locale.ENGLISH) shouldBe upperCaseName
        val mixed = named(mixedCaseName)
        val upper = named(upperCaseName)
        mixed.name shouldBe mixedCaseName
        upper.name shouldBe upperCaseName
        mixed should not be upper

        NamedEnum[FloatingRateName].valueOf(mixedCaseName) shouldBe Some(mixed)
        NamedEnum[FloatingRateName].valueOf(upperCaseName) shouldBe Some(upper)
        NamedEnum[FloatingRateName].byUpperName.get(upperCaseName) shouldBe Some(upper)

        // Each writes its own name and reads back as itself, not as the earlier row.
        forEvery(Table("subject", mixed, upper)) { (value: FloatingRateName) =>
          withClue(s"'${value.name}': ") {
            val encoded = Encoder[FloatingRateName].apply(value)
            encoded shouldBe Json.fromString(value.name)
            Decoder[FloatingRateName].decodeJson(encoded) shouldBe Right(value)
          }
        }

        mixed.rateType shouldBe upper.rateType
        mixed.normalized shouldBe upper.normalized
      }
    }
  }

  test("the decoder reads a published name in any case and still refuses an index name") {
    // Reading goes through `NamedEnum.parse`, which retries the lookup over the upper-case fold
    // of the text. No lenient rewrite is declared, so that fold is the whole of the leniency.
    val mixedCase = named("USD-Federal Funds-H.15")
    Decoder[FloatingRateName].decodeJson(Json.fromString("USD-Federal Funds-H.15")) shouldBe
      Right(mixedCase)
    Decoder[FloatingRateName].decodeJson(Json.fromString("usd-federal funds-h.15")) shouldBe
      Right(mixedCase)
    Decoder[FloatingRateName].decodeJson(Json.fromString("USD-FEDERAL FUNDS-H.15")) shouldBe
      Right(mixedCase)

    // The exact lookup is the stricter: a published name or the upper-case spelling of one.
    FloatingRateName.valueOf("usd-federal funds-h.15") shouldBe None
    FloatingRateName.valueOf("USD-FEDERAL FUNDS-H.15") shouldBe Some(mixedCase)

    Decoder[FloatingRateName].decodeJson(Json.fromString("gbp-libor-bba")) shouldBe
      Right(named("GBP-LIBOR-BBA"))

    // No fold of `GBP-LIBOR-3M` is a published name, so the decoder refuses it in any case, while
    // `parse` - the wider resolution, deliberately not the decoder - answers with the family.
    FloatingRateName.valueOf("gbp-libor-3m") shouldBe None
    Decoder[FloatingRateName].decodeJson(Json.fromString("gbp-libor-3m")).isLeft shouldBe true
    Decoder[FloatingRateName].decodeJson(Json.fromString("GBP-LIBOR-3M")).isLeft shouldBe true
    FloatingRateName.parse("GBP-LIBOR-3M") should haveValue(FloatingRateNames.GBP_LIBOR)
  }

  // The published name table the members are created from, `FloatingRateNameData`, is asserted
  // below, no spec of its own covering it. What these tests pin is the structure a consumer relies
  // on and the row-for-row comparison in `ReferenceDataManifestSpec` does not state: the size and
  // kind of each section, the published order of the concatenation and of the section boundaries
  // within it, the one derived column, the rows declaring a non-standard fixing date offset, the
  // two names containing an `=`, and that every currency default names a row of the right kind.
  // The assertions never name the type of the published collections: the currency tables are
  // ordered maps and the order is the contract, but how they are backed is not.
  //-------------------------------------------------------------------------
  /**
   * The size and kind of each of the four name sections, in published order, stated here rather
   * than read from the object under test, and totalling [[PublishedNameCount]].
   */
  private val publishedNameSections: TableFor3[String, Int, FloatingRateType] =
    Table(
      ("section", "size", "rateType"),
      ("ibor", 159, FloatingRateType.Ibor),
      ("overnightCompounded", 156, FloatingRateType.OvernightCompounded),
      ("overnightAveraged", 6, FloatingRateType.OvernightAveraged),
      ("price", 30, FloatingRateType.Price))

  /**
   * The rows of each section, keyed the way the table above names them.
   *
   * @param section  the published name of the section
   * @return the rows of that section
   */
  private def publishedSectionRows(section: String): Vector[FloatingRateNameRow] =
    section match {
      case "ibor" => FloatingRateNameData.iborRows
      case "overnightCompounded" => FloatingRateNameData.overnightCompoundedRows
      case "overnightAveraged" => FloatingRateNameData.overnightAveragedRows
      case "price" => FloatingRateNameData.priceRows
      case other => fail(s"unknown section: $other")
    }

  //-------------------------------------------------------------------------
  test("data_sections_haveThePublishedSizeAndRateType") {
    forEvery(publishedNameSections) { (section: String, size: Int, rateType: FloatingRateType) =>
      withClue(s"$section: ") {
        val rows = publishedSectionRows(section)
        rows should have size size.toLong
        rows.map(_.rateType).distinct shouldBe Vector(rateType)
      }
    }

    // The 351 name rows over the four sections, plus the three fixing date offsets and the two
    // currency default tables, are the 404 published rows.
    FloatingRateNameData.rows should have size PublishedNameCount.toLong
    FloatingRateNameData.iborFixingDateOffsets should have size 3
    FloatingRateNameData.currencyDefaultIbor should have size 23
    FloatingRateNameData.currencyDefaultOvernight should have size 27
    val publishedRowCount =
      FloatingRateNameData.rows.size +
        FloatingRateNameData.iborFixingDateOffsets.size +
        FloatingRateNameData.currencyDefaultIbor.size +
        FloatingRateNameData.currencyDefaultOvernight.size
    publishedRowCount shouldBe 404
  }

  test("data_rows_areTheSectionsConcatenatedInPublishedOrder") {
    // The published order is the order a consumer that builds one member per row builds them in.
    FloatingRateNameData.rows shouldBe
      (FloatingRateNameData.iborRows ++
        FloatingRateNameData.overnightCompoundedRows ++
        FloatingRateNameData.overnightAveragedRows ++
        FloatingRateNameData.priceRows)

    FloatingRateNameData.rows.take(159) shouldBe FloatingRateNameData.iborRows
    FloatingRateNameData.rows.slice(159, 315) shouldBe FloatingRateNameData.overnightCompoundedRows
    FloatingRateNameData.rows.slice(315, 321) shouldBe FloatingRateNameData.overnightAveragedRows
    FloatingRateNameData.rows.drop(321) shouldBe FloatingRateNameData.priceRows
  }

  test("data_externalNames_areDistinctAndResolveThroughTheLookup") {
    FloatingRateNameData.rows.map(_.externalName).distinct should have size PublishedNameCount.toLong
    FloatingRateNameData.byExternalName should have size PublishedNameCount.toLong
    FloatingRateNameData.rows.foreach { row =>
      withClue(s"${row.externalName}: ") {
        FloatingRateNameData.byExternalName.get(row.externalName) shouldBe Some(row)
      }
    }
    FloatingRateNameData.byExternalName.keySet shouldBe FloatingRateNameData.rows.map(_.externalName).toSet
    // the keys are the published names alone: folding case is the named enum's business
    FloatingRateNameData.byExternalName.get("gbp-libor-bba") shouldBe None
    FloatingRateNameData.byExternalName.get("Rubbish") shouldBe None
  }

  test("data_indexNames_carryTheTrailingSeparatorExactlyWhereATenorCompletesThem") {
    // An Ibor family is named by a stem a tenor completes - `GBP-LIBOR-` plus `3M` is the index
    // `GBP-LIBOR-3M` - so only the Ibor rows end with the separator.
    FloatingRateNameData.iborRows.foreach { row =>
      withClue(s"${row.externalName}: ") {
        row.indexName should endWith("-")
        row.indexName.init should not be empty
      }
    }
    (FloatingRateNameData.overnightCompoundedRows ++
      FloatingRateNameData.overnightAveragedRows ++
      FloatingRateNameData.priceRows).foreach { row =>
      withClue(s"${row.externalName}: ") {
        row.indexName.endsWith("-") shouldBe false
      }
    }

    FloatingRateNameData.byExternalName.get("GBP-LIBOR-BBA").map(_.indexName) shouldBe Some("GBP-LIBOR-")
    FloatingRateNameData.byExternalName.get("CHF-LIBOR-BBA").map(_.indexName) shouldBe Some("CHF-LIBOR-")
    FloatingRateNameData.byExternalName.get("CHF-SARON-OIS-COMPOUND").map(_.indexName) shouldBe Some("CHF-SARON")
    FloatingRateNameData.byExternalName.get("USD-EFFECTIVE").map(_.indexName) shouldBe Some("USD-FED-FUND")
    FloatingRateNameData.byExternalName.get("UK-RPI").map(_.indexName) shouldBe Some("GB-RPI")
  }

  test("data_manyExternalNamesShareOneIndexName") {
    // The canonical row is the one whose external name completes to its index name, the rest
    // being spellings of it: five spellings of Swiss franc Libor name one stem.
    FloatingRateNameData.iborRows.filter(_.indexName == "CHF-LIBOR-").map(_.externalName) shouldBe
      Vector(
        "CHF-LIBOR",
        "CHF-LIBOR-BBA",
        "CHF-LIBOR-BBA-Bloomberg",
        "CHF-LIBOR-ISDA",
        "CHF-LIBOR-Reference Banks")
    FloatingRateNameData.rows.map(_.indexName).distinct.size should be < FloatingRateNameData.rows.size
  }

  test("data_fixingDateOffsets_areTheThreePublishedRowsAndNoOthers") {
    // Three rows, all Danish Cibor and all zero: `DKK-CIBOR-DKNA13` fixes on the day itself,
    // where `DKK-CIBOR2-DKNA13`, declaring no offset, fixes two business days earlier.
    FloatingRateNameData.iborFixingDateOffsets.toVector shouldBe
      Vector(
        "DKK-CIBOR-DKNA13" -> 0,
        "DKK-CIBOR-DKNA13-Bloomberg" -> 0,
        "DKK-CIBOR-Reference Banks" -> 0)

    val carryingAnOffset = FloatingRateNameData.rows.filter(_.fixingDateOffsetDays.isDefined)
    carryingAnOffset.map(_.externalName) shouldBe
      Vector("DKK-CIBOR-DKNA13", "DKK-CIBOR-DKNA13-Bloomberg", "DKK-CIBOR-Reference Banks")
    carryingAnOffset.map(_.fixingDateOffsetDays) shouldBe Vector(Some(0), Some(0), Some(0))

    // every other row defers to the offset of the index it names
    FloatingRateNameData.rows.count(_.fixingDateOffsetDays.isEmpty) shouldBe 348
    FloatingRateNameData.byExternalName.get("DKK-CIBOR2-DKNA13").map(_.fixingDateOffsetDays) shouldBe Some(None)

    FloatingRateNameData.iborFixingDateOffsets.foreach {
      case (externalName, days) =>
        withClue(s"$externalName: ") {
          FloatingRateNameData.byExternalName.get(externalName).map(_.fixingDateOffsetDays) shouldBe Some(Some(days))
        }
    }
  }

  test("data_theTwoPublishedNamesContainingAnEqualsSign") {
    // The separator of the source format appears inside these keys, so a wrong split fails here.
    FloatingRateNameData.byExternalName.get("CNY-CNREPOFIX=CFXS-Reuters").map(row =>
      (row.indexName, row.rateType)) shouldBe Some(("CNY-REPO-", FloatingRateType.Ibor))
    FloatingRateNameData.byExternalName.get("HKD-HIBOR-HIBOR=").map(row =>
      (row.indexName, row.rateType)) shouldBe Some(("HKD-HIBOR-", FloatingRateType.Ibor))
    // the shorter spelling of the first is a row in its own right
    FloatingRateNameData.byExternalName.get("CNY-CNREPOFIX").map(_.indexName) shouldBe Some("CNY-REPO-")
  }

  test("data_currencyDefaults_resolveToARowOfTheRightKind") {
    // The value of a default is an external name, not an index name, and the two differ for some
    // rows, so a consumer resolves it through the lookup: every value must name a row of the
    // right kind.
    FloatingRateNameData.currencyDefaultIbor.foreach {
      case (currency, externalName) =>
        withClue(s"${currency.code} -> $externalName: ") {
          FloatingRateNameData.byExternalName.get(externalName).map(_.rateType) shouldBe Some(FloatingRateType.Ibor)
        }
    }
    FloatingRateNameData.currencyDefaultOvernight.foreach {
      case (currency, externalName) =>
        withClue(s"${currency.code} -> $externalName: ") {
          FloatingRateNameData.byExternalName.get(externalName).map(_.rateType.isOvernight) shouldBe Some(true)
        }
    }

    // the two rows where the external name is not the index name
    FloatingRateNameData.currencyDefaultIbor.get(Currency.CNY) shouldBe Some("CNY-REPO")
    FloatingRateNameData.byExternalName.get("CNY-REPO").map(_.indexName) shouldBe Some("CNY-REPO-")
    FloatingRateNameData.currencyDefaultIbor.get(Currency.THB) shouldBe Some("THB-THBFIX")
    FloatingRateNameData.byExternalName.get("THB-THBFIX").map(_.indexName) shouldBe Some("THB-THBFIX-")

    // a currency the source does not name has no published default
    FloatingRateNameData.currencyDefaultIbor.get(Currency.BRL) shouldBe None
    FloatingRateNameData.currencyDefaultOvernight.get(Currency.MXN) shouldBe None
  }

  test("data_iterationOrderIsStable") {
    // Each table is read twice in one run and the reads compared, so a table answering a second
    // read differently fails here; agreement with the published order is asserted above.
    FloatingRateNameData.rows shouldBe FloatingRateNameData.rows
    FloatingRateNameData.iborFixingDateOffsets.toVector shouldBe FloatingRateNameData.iborFixingDateOffsets.toVector
    FloatingRateNameData.currencyDefaultIbor.toVector shouldBe FloatingRateNameData.currencyDefaultIbor.toVector
    FloatingRateNameData.currencyDefaultOvernight.toVector shouldBe
      FloatingRateNameData.currencyDefaultOvernight.toVector
    FloatingRateNameData.byExternalName.toVector shouldBe FloatingRateNameData.byExternalName.toVector
  }
}
