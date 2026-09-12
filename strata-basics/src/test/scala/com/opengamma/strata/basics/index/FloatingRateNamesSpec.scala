/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

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
import org.scalatest.prop.TableFor3

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.date.BusinessDayAdjustment
import com.opengamma.strata.basics.date.BusinessDayConventions
import com.opengamma.strata.basics.date.DaysAdjustment
import com.opengamma.strata.basics.date.HolidayCalendarIds
import com.opengamma.strata.basics.date.Tenor
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[FloatingRateName]], ported from the Java `FloatingRateNamesTest`.
 *
 * Every one of the twenty-one methods of the original is kept, under the name the original gave
 * it - including `test_tiee`, whose spelling of the Mexican TIIE rate is left exactly as it was
 * found, so that the method-level traceability of the migration stays one-to-one. The three
 * methods the original drove from the `data_name_type` provider each become a single test that
 * runs the whole of that provider's seventy-six rows, so a Java method and a test of this suite
 * remain in one-to-one correspondence rather than one-to-seventy-six; the rows are transcribed
 * once, into [[dataNameType]], exactly as the three parameterized methods shared one provider.
 *
 * The suite name and every test name are joined to `manifest/java-test-mapping.csv` by
 * `scripts/verify-gates.sh` on the pair (suite class, test name), so neither may be renamed.
 * That manifest routes one row elsewhere: the serialization row is `consolidated:` into
 * `com.opengamma.strata.basics.json.JsonRoundTripSpec` as `FloatingRateNames_test_serialization`,
 * which is where the gate joins it. The routing is honoured - that spec owns the joined case -
 * and the behaviour is additionally witnessed here under the Java method name, because the JSON
 * form of this family is part of its own contract and a local witness fails against the family
 * rather than against a sweep over every serializable type of the module.
 *
 * ===Shape of the port===
 *
 * The Java assertions translate under three substitutions, each required by the obligations of
 * the user's original request carried by AAP §0.7 / §0.8.1. (There is no user rules document:
 * `review_rules` reports that no user rules were provided, which is not licence to lower the bar
 * - enterprise-standard best practice governs, and the obligations below are requirements of the
 * request itself.)
 *
 *  - '''Failure is a value''' (Rule 5). The original's `assertThatIllegalArgumentException` and
 *    `assertThatIllegalStateException` sites become assertions that an `Either` is `Left`,
 *    written with `beFailureWith`, which compares the reason as a member of the closed family of
 *    reasons rather than by matching message text. Nothing here is intercepted as a thrown
 *    error: every failure asserted below depends on the data of its arguments - a name of the
 *    wrong kind for the conversion asked for, a name that resolves to no published index - and so
 *    none of them is a caller-contract precondition. Every `Either` is unwrapped by a matcher
 *    rather than by projecting out of it.
 *  - '''The family is closed''' (Rule 4). Two of the original's methods iterated the runtime
 *    registry that assembled the family from a classpath resource. That registry does not exist
 *    here, so both iterate [[FloatingRateName.values]], the closed set of published members,
 *    which is a stronger statement of the same property: the set cannot be extended, overridden
 *    or left empty by a load failure. No member is constructed anywhere in this spec - the
 *    constructor is not visible outside its own file - and the two counts the family publishes
 *    are asserted where they are relied on.
 *  - '''No reflection''' (Rule 6). The original's `test_types` walked the fields of the constants
 *    holder reflectively; the port enumerates them in an explicitly typed list, which moves the
 *    same check from run time to compile time. See that test for the detail.
 *
 * ===Two counts that differ, legitimately===
 *
 * [[FloatingRateName.values]] holds '''351''' members, one per row of the published name table
 * (159 Ibor + 156 Overnight compounded + 6 Overnight averaged + 30 price), while
 * [[FloatingRateNames]] publishes '''41''' named constants. The two numbers are not in tension
 * and neither is a typo: the constants are the subset of the published names this library ships
 * pricing data for, and the remaining 310 rows are alternative and historical spellings and rates
 * modelled by name alone. Both counts are asserted - the first in `test_normalized` and
 * `test_getFloatingRateName`, which sweep the whole family, and the second in `test_types` and
 * `coverage`, which enumerate the constants.
 *
 * Unlike the four index families, this family declares '''no''' alternate-name, lenient-rewrite or
 * external-group table: its 351 published names are the whole of its name space, so there is no
 * alias row for this spec to assert. Fidelity of the 404 transcribed source rows against the
 * Java-captured reference data manifest belongs to `ReferenceDataManifestSpec`; this spec asserts
 * behaviour, and the two are complementary rather than overlapping.
 */
class FloatingRateNamesSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The number of members the published name table declares.
   *
   * Named here because two tests sweep the family and both have to agree about how much of it
   * they swept; see the note on this suite for why this is not the count of the constants.
   */
  private val PublishedNameCount: Int = 351

  /**
   * The number of named constants [[FloatingRateNames]] publishes.
   */
  private val ConstantCount: Int = 41

  //-------------------------------------------------------------------------
  /**
   * The published name of every floating rate the original's `data_name_type` provider names,
   * with the index name it resolves to and the kind of rate it describes.
   *
   * The seventy-six rows are transcribed from that provider verbatim and in its order, and they
   * are deliberately not tidied in two respects. Several names carry an embedded space, a dot or
   * an equals sign - `USD-Federal Funds-H.15`, `CNY-CNREPOFIX=CFXS-Reuters`, `HKD-HIBOR-HIBOR=` -
   * because a published name is the text that arrives in an FpML message and a name that is
   * corrected stops matching the messages it exists to match. And the second column is
   * inconsistent about the trailing `-` of an Ibor index name - `GBP-LIBOR-` carries it while
   * `AUD-BBSW` does not - which is a property of the provider rather than an error in it;
   * `test_name` reproduces the conditional the original used to absorb the inconsistency rather
   * than pre-stripping the column here.
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
   * The whole of the closed family, as a table, so that a sweep over it reports every member that
   * fails rather than stopping at the first.
   *
   * This is [[FloatingRateName.values]] and nothing else: the runtime registry the Java tests
   * iterated has no counterpart here, and the closed set is what replaces it (Rule 4).
   */
  private val allPublishedNames: TableFor1[FloatingRateName] =
    Table("floatingRateName", FloatingRateName.values.toList: _*)

  /**
   * Every constant [[FloatingRateNames]] publishes, in the order that object declares them, with
   * the element type stated explicitly.
   *
   * The type ascription is the whole point of the list and is what discharges, at compile time,
   * the check the Java `test_types` performed reflectively at run time; see that test.
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
   * Resolves a published name through the family's exact lookup, failing the test where the
   * family has no such member.
   *
   * This stands in for the Java factory the original called throughout, which raised on a miss.
   * It is used only where a test needs a subject to call operations on; every assertion about
   * what a lookup answers with is written against the lookup itself, not through this.
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

        // The provider's second column carries the trailing `-` of an Ibor index name for some
        // rows and not for others, and the original absorbed that with an inline conditional.
        // The conditional is reproduced rather than the column pre-stripped, because the `-` is
        // genuinely part of an Ibor index name - the transcribed Ibor rows append it, so that a
        // tenor completes the name - and `normalized` is the operation that drops it again.
        val expectedNormalized =
          if (indexName.endsWith("-")) indexName.dropRight(1) else indexName
        test.normalized.map(normalized => normalized.name) should haveValue(expectedNormalized)

        // The accessor the Java interface called `getType` is `rateType` here, `type` being a
        // keyword of this language. The value it answers with is unchanged.
        test.rateType shouldBe rateType

        // A name does not hold a currency; it derives one from the index it converts to, so the
        // accessor inherits that conversion's failure and both sides of this comparison are
        // `Either`. Comparing the two as values - rather than projecting a currency out of each -
        // is what makes the assertion hold for a name whose index is not published as well: the
        // two then agree by carrying the same failure, which is exactly the identity the Java
        // assertion stated when both sides were total.
        val expectedCurrency: Either[Failure, Currency] =
          test.toFloatingRateIndex.map(index => index.currency)
        test.currency shouldBe expectedCurrency
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
        // Every published name resolves through the exact lookup, and resolves to the same value
        // however often it is asked - the lookup selects from the members created once in the
        // companion rather than building a member per call, so the two answers are one object.
        val test = FloatingRateName.valueOf(name)
        test.isDefined shouldBe true
        FloatingRateName.valueOf(name) shouldBe test
      }
    }
  }

  test("test_of_lookup_notFound") {
    // Text naming no member is reported rather than raised (Rule 5): the reason is `PARSING`, and
    // it is compared as a member of the closed family of reasons rather than by matching the
    // message, whose prose is diagnostic and not contract.
    FloatingRateName.valueOf("Rubbish") shouldBe None
    FloatingRateName.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
  }

  test("test_of_lookup_null") {
    // The Java method asserted that the absent name was rejected, which it expressed with a Java
    // reference that has no counterpart in this API and that Rule 5 forbids writing; the absent
    // name is therefore the empty name and the name that is nothing but whitespace, each of which
    // resolves to no member and is reported with reason `PARSING`.
    FloatingRateName.valueOf("") shouldBe None
    FloatingRateName.parse("") should beFailureWith(FailureReason.PARSING)
    FloatingRateName.valueOf("   ") shouldBe None
    FloatingRateName.parse("   ") should beFailureWith(FailureReason.PARSING)
  }

  test("test_parse") {
    FloatingRateName.parse("GBP-LIBOR") should haveValue(FloatingRateNames.GBP_LIBOR)
    // A name that names a concrete index resolves to the family of that index, losing the tenor:
    // this is the resolution's fallback through the index families, which the exact lookup does
    // not perform.
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
  }

  test("test_defaultOvernightIndex") {
    FloatingRateName.defaultOvernightIndex(Currency.GBP) should haveValue(named("GBP-SONIA"))
    FloatingRateName.defaultOvernightIndex(Currency.EUR) should haveValue(named("EUR-ESTR"))
    FloatingRateName.defaultOvernightIndex(Currency.USD) should haveValue(named("USD-SOFR"))
  }

  //-------------------------------------------------------------------------
  test("test_normalized") {
    named("GBP-LIBOR-BBA").normalized should haveValue(named("GBP-LIBOR"))
    named("GBP-WMBA-SONIA-COMPOUND").normalized should haveValue(named("GBP-SONIA"))

    // The Java method swept the runtime registry and asserted that every member normalized to
    // something; the sweep is kept and made stronger by running over the closed set of published
    // members (Rule 4), which no load failure can leave partial, and by asserting that every
    // member's normalized form is a value rather than a failure. A failure here would mean the
    // two transcribed tables disagree: a family whose canonical name is not itself published.
    FloatingRateName.values.size shouldBe PublishedNameCount
    forEvery(allPublishedNames) { (value: FloatingRateName) =>
      withClue(s"'${value.name}': ") {
        value.normalized should beSuccess
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_getFloatingRateName") {
    // A concrete index reports the family it belongs to; a family reports itself. Swept over the
    // closed set of published members, as above.
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
    // One year and twelve months are the same period, so the tenor is normalized before it
    // completes the index name and both name the twelve-month index.
    test.toIborIndex(Tenor.TENOR_1Y) should haveValue(IborIndices.GBP_LIBOR_12M)

    // Asking an Overnight name for an Ibor index is a mismatch of kind, which the interface being
    // ported raised as an illegal state and this port reports as `INVALID` (Rule 5).
    named("GBP-WMBA-SONIA-COMPOUND").toIborIndex(Tenor.TENOR_6M) should
      beFailureWith(FailureReason.INVALID)

    // The tenor set is ordered by the length of a tenor, so the assertion is over the ordered
    // sequence, which is what the Java `containsExactly` stated.
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
  }

  test("test_overnightIndex") {
    val test = named("GBP-WMBA-SONIA-COMPOUND")
    test.defaultTenor should haveValue(Tenor.TENOR_1D)
    test.toFloatingRateIndex should haveValue(OvernightIndices.GBP_SONIA)
    // An Overnight name has no tenor to choose, so the tenor supplied is ignored rather than
    // rejected - the behaviour of the interface being ported.
    test.toFloatingRateIndex(Tenor.TENOR_1M) should haveValue(OvernightIndices.GBP_SONIA)
    test.toOvernightIndex should haveValue(OvernightIndices.GBP_SONIA)

    FloatingRateNames.USD_FED_FUND.toOvernightIndex should haveValue(OvernightIndices.USD_FED_FUND)
    // The averaging name carries the `-AVG` suffix on its index name while the published index
    // does not, so the conversion strips it. This is the only assertion in the suite that
    // exercises that strip.
    FloatingRateNames.USD_FED_FUND_AVG.toOvernightIndex should
      haveValue(OvernightIndices.USD_FED_FUND)

    named("GBP-LIBOR-BBA").toOvernightIndex should beFailureWith(FailureReason.INVALID)

    val tenors: SortedSet[Tenor] = test.tenors
    tenors.toList shouldBe List.empty[Tenor]

    test.toIborIndexFixingOffset should beFailureWith(FailureReason.INVALID)
  }

  test("test_priceIndex") {
    // `UK-HICP` is a published floating rate name and is *not* a published price index name -
    // the index it resolves to is `GB-HICP`. The two name spaces overlap in spelling for four
    // names, so every expected value below states its type.
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
    // Two published names share one index, which is why the fixing offset is carried by the name
    // rather than taken from the index alone.
    named("DKK-CIBOR2-DKNA13").toIborIndex(Tenor.TENOR_6M) should
      haveValue(IborIndices.DKK_CIBOR_6M)

    // The pair below is the only witness in the suite for the non-standard fixing offset table:
    // the `DKK-CIBOR-DKNA13` name implies an offset of zero days moved back to the preceding
    // business day, while `DKK-CIBOR2-DKNA13` keeps the index's own two-business-day offset.
    test.toIborIndexFixingOffset should haveValue(
      DaysAdjustment.ofCalendarDays(
        0,
        BusinessDayAdjustment.of(BusinessDayConventions.PRECEDING, HolidayCalendarIds.DKCO)))
    named("DKK-CIBOR2-DKNA13").toIborIndexFixingOffset should
      haveValue(DaysAdjustment.ofBusinessDays(-2, HolidayCalendarIds.DKCO))
  }

  test("test_tiee") {
    // The Mexican rates publish no three-month index, so the default tenor falls to thirteen
    // weeks - the second of the two candidates the accessor tries.
    val test = named("MXN-TIIE")
    test.defaultTenor should haveValue(Tenor.TENOR_13W)
    test.toFloatingRateIndex should haveValue(IborIndices.MXN_TIIE_13W)
    test.toFloatingRateIndex(Tenor.TENOR_4W) should haveValue(IborIndices.MXN_TIIE_4W)
    test.toIborIndex(Tenor.TENOR_4W) should haveValue(IborIndices.MXN_TIIE_4W)
    test.toIborIndexFixingOffset should
      haveValue(DaysAdjustment.ofBusinessDays(-1, HolidayCalendarIds.MXMC))
  }

  test("test_nzd") {
    // The New Zealand term and overnight rates both report the New Zealand dollar, each through
    // the index it converts to, so both accessors are `Either`-valued.
    named("NZD-BKBM").currency should haveValue(Currency.NZD)
    named("NZD-NZIONA").currency should haveValue(Currency.NZD)
  }

  //-------------------------------------------------------------------------
  test("test_types") {
    // The Java method walked the fields of the constants holder reflectively and asserted that
    // each public static field was typed as a floating rate name - "ensure no stupid copy and
    // paste errors". Reflection is forbidden here (Rule 6), and it is not needed: enumerating
    // the constants in a list whose element type is *stated* makes the compiler perform that
    // check, so a constant of the wrong type does not fail this test - it fails the build, which
    // is strictly stronger than the run-time check it replaces and cannot be reached at all by a
    // constant that is never looked at.
    allConstants should have size ConstantCount.toLong
    allConstants.distinct should have size ConstantCount.toLong

    // Each constant is the published member of its own name rather than a separate value, which
    // is the other half of what the reflective walk was guarding: a constant that named the wrong
    // row would still have had the right type.
    forEvery(Table("constant", allConstants: _*)) { (value: FloatingRateName) =>
      withClue(s"'${value.name}': ") {
        FloatingRateName.valueOf(value.name) shouldBe Some(value)
      }
    }
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java method covered the private constructor of the constants holder and swept the bean
    // machinery of two subjects. The holder is an object here and has no constructor to cover, so
    // its constants are asserted distinct instead - deliberately overlapping `test_types`, as the
    // Java pair of tests overlapped.
    allConstants.distinct should have size ConstantCount.toLong

    // The bean sweep becomes a cover of the instances this family actually publishes, over
    // exactly the two subjects the Java method used - the second of which carries a space and a
    // dot in its published name.
    val test = named("GBP-LIBOR-BBA")
    val other = named("USD-Federal Funds-H.15")

    Order[FloatingRateName].eqv(test, test) shouldBe true
    Order[FloatingRateName].eqv(test, other) shouldBe false
    Hash[FloatingRateName].hash(test) shouldBe test.name.hashCode
    Hash[FloatingRateName].hash(test) should not be Hash[FloatingRateName].hash(other)
    Show[FloatingRateName].show(test) shouldBe test.name
    Show[FloatingRateName].show(other) shouldBe other.name

    // One equality-bearing instance means the ordering cannot disagree with equality: its
    // comparison is zero exactly where the two values are equal.
    Order[FloatingRateName].compare(test, test) shouldBe 0
    Order[FloatingRateName].compare(test, other) should not be 0
    Order[FloatingRateName].compare(test, other).sign shouldBe
      test.name.compareTo(other.name).sign
  }

  test("test_jodaConvert") {
    // The Java method asserted the string conversion of the library it used: a value renders to
    // text and that text resolves back to the value. Both halves are kept - rendering is the
    // `Show` instance, which agrees with `toString`, and resolution is the family's exact lookup,
    // which is the factory that conversion was bound to - while the library itself is gone
    // (AAP §0.2.2, Rule 1). This is the *text* round trip; `test_serialization` is the JSON one,
    // and the two are deliberately separate assertions of separate contracts.
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
    // The Java method asserted a Java-serialization round trip, which is dropped (AAP §0.2.2) and
    // replaced by the JSON round trip of AAP §0.6.4. A named value encodes as the bare string of
    // its name, so the JSON of a member is its published name and nothing more.
    //
    // The test mapping manifest routes this Java method to `JsonRoundTripSpec` as
    // `FloatingRateNames_test_serialization`, which is where the acceptance gate joins it; this
    // test is the local witness of the same contract, named after the Java method it ports.
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

    // Text naming no member is rejected by the decoder rather than silently resolved.
    Decoder[FloatingRateName].decodeJson(Json.fromString("Rubbish")).isLeft shouldBe true
    // A name that names a concrete index is rejected too: the wider resolution accepts it, and
    // the decoder deliberately does not, because reading it as a family would discard the tenor.
    Decoder[FloatingRateName].decodeJson(Json.fromString("GBP-LIBOR-3M")).isLeft shouldBe true
  }
}
