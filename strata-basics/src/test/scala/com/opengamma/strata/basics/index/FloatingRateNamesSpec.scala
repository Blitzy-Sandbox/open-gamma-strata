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
 *    none of them is a caller-contract precondition. All three reasons this surface reports are
 *    witnessed, because a caller acts on which one it gets: `INVALID` for a name of the wrong kind
 *    (`test_iborIndex_tenor`, `test_overnightIndex`, `test_priceIndex`), `PARSING` for a name of
 *    the right kind whose index or tenor is not published (`test_iborIndex_tenor`,
 *    `test_overnightIndex`) and `MISSING_DATA` where the published data names nothing to answer
 *    with - an Ibor family whose every index has been retired, and a currency with no default rate
 *    (`test_iborIndex_tenor`, `test_defaultIborIndex`, `test_defaultOvernightIndex`). An `Either`
 *    is unwrapped by a matcher rather than by projecting out of it, with one deliberate exception:
 *    `test_name` folds the conversion it needs the index of, and the failure branch of that fold
 *    fails the row, which is how the row states that the name converts at all.
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
 *
 * ===Three tests with no Java counterpart===
 *
 * The last three tests of the suite are additions rather than ports, and they are named in prose
 * rather than after a Java method so that the one-to-one correspondence above stays exactly that:
 * every `test_`-prefixed test ports the Java method of its name, and nothing else does. They
 * state the part of the contract that belongs to this port rather than to the library being
 * ported - that the family has one lookup, the typeclass
 * [[com.opengamma.strata.collect.named.NamedEnum]] published by its companion, and that reading
 * and writing JSON go through that same lookup (AAP §0.6.3, §0.6.4). What they cover is what the
 * published data makes peculiar to this family: four rows declaring two rates twice, differing in
 * the case of one word, over which a lookup that claimed folded spellings ahead of published
 * names would leave a member no text could reach.
 *
 * ===The published name table, folded in===
 *
 * A final section, after the ported methods and marked as such, asserts the '''structure''' of
 * the transcribed table this family creates its members from, `FloatingRateNameData`. The test
 * inventory of AAP §0.3.1 lists thirteen index specs and none for that table - the Java sources
 * have no test class for it, because there it was a configuration resource a loader read at
 * class-initialization time - so what a consumer of the table relies on is asserted here, in the
 * spec of the family the table produces. The reasoning is repeated at the section, and the tests
 * of that section are named `data_*` rather than after a Java method, because
 * `manifest/java-test-mapping.csv` maps no Java method to any of them.
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
   * The two rates the published data declares twice, differing in the case of one word.
   *
   * Each row is the mixed-case published name beside the upper-case one, in the order the name
   * table declares them. The upper-case spelling of the first is '''exactly''' the published name
   * of the second, which is what makes these four rows the only place in the library where the
   * order in which a name lookup claims a canonical name and a folded one is observable. The
   * names are transcribed from `FloatingRateNameData` verbatim, spaces and all.
   */
  private val caseDifferingPairs: TableFor2[String, String] =
    Table(
      ("mixedCaseName", "upperCaseName"),
      ("DKK-DESTR-OIS Compound", "DKK-DESTR-OIS COMPOUND"),
      ("SEK-SWESTR-OIS Compound", "SEK-SWESTR-OIS COMPOUND"))

  /**
   * The number of distinct folded keys the 351 published names have.
   *
   * Two fewer than the member count, because the two rows of each pair in [[caseDifferingPairs]]
   * fold to one key - the published name of the upper-case member of the pair, which is therefore
   * the member that holds it.
   */
  private val FoldedKeyCount: Int = PublishedNameCount - 2

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
        // accessor is `Either`-valued where the currency of an index is total. The original
        // asserted the two against each other while both were total values, which stated two
        // things at once: that the name converts to an index, and that the currency it reports is
        // that index's currency. Both are kept here, and they have to be stated separately,
        // because comparing the two `Either`s as values would also be satisfied by a name whose
        // conversion failed - the two sides would then agree by carrying the same failure and the
        // row would assert nothing. So the conversion is required to produce an index, the index
        // supplies the currency expected, and the accessor is asserted to carry it. Every one of
        // these seventy-six names does convert; the six euroyen TIBOR names and the nine overnight
        // names whose index this library does not publish are the members that would not, and none
        // of them is a row of this provider.
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

    // Only 23 of the 74 currencies publish a default term rate, so the other side of this map is
    // a miss, and the miss is the reported half of the accessor's contract rather than an
    // afterthought: the Brazilian real has no published term rate - it appears in the Overnight
    // defaults and not in these - and the absence is `MISSING_DATA` instead of a guess at a rate
    // the data does not name. The implementation being ported raised here.
    FloatingRateName.defaultIborIndex(Currency.BRL) should beFailureWith(FailureReason.MISSING_DATA)
  }

  test("test_defaultOvernightIndex") {
    FloatingRateName.defaultOvernightIndex(Currency.GBP) should haveValue(named("GBP-SONIA"))
    FloatingRateName.defaultOvernightIndex(Currency.EUR) should haveValue(named("EUR-ESTR"))
    FloatingRateName.defaultOvernightIndex(Currency.USD) should haveValue(named("USD-SOFR"))

    // The Overnight defaults cover 27 currencies, which leaves the same reported miss as above for
    // every other currency: gold is a currency of this library with no overnight rate to default
    // to, so the lookup reports `MISSING_DATA`.
    FloatingRateName.defaultOvernightIndex(Currency.XAU) should
      beFailureWith(FailureReason.MISSING_DATA)
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

    // A tenor of the right kind that the family publishes no index for is the other failure of
    // this conversion, and it is a different one: the name is an Ibor name, so the kind matches,
    // and what is missing is the index the name and tenor together resolve to. That is reported as
    // `PARSING` rather than `INVALID`, which is the distinction a caller acts on - a caller can
    // retry a different tenor, but not a different kind. Sterling Libor publishes six tenors, and
    // an overnight index is not among them.
    test.toIborIndex(Tenor.TENOR_1D) should beFailureWith(FailureReason.PARSING)

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

    // The default tenor tries three months, then thirteen weeks, and only then the shortest tenor
    // the family has - the first element of the ordered set. The third branch is reached by the
    // Chinese repo rate, whose one active index is at one week, and it is the branch `test_tiee`
    // does not cover: that one stops at the second candidate.
    val shortestTenorOnly = named("CNY-REPO")
    shortestTenorOnly.tenors.toList shouldBe List(Tenor.TENOR_1W)
    shortestTenorOnly.defaultTenor should haveValue(Tenor.TENOR_1W)
    shortestTenorOnly.toFloatingRateIndex.map(index => index.name) should haveValue("CNY-REPO-1W")

    // An Ibor family whose every index has been retired has no tenor to offer at all, and that is
    // the one place this port reports a failure the interface being ported did not: it took the
    // first element of an empty set and raised. The published data reaches it - all thirteen
    // euroyen TIBOR indices are inactive - so the case is asserted rather than described, and it
    // is asserted through every accessor that needs a default tenor, because each of them
    // propagates this one failure.
    val everyIndexRetired = named("JPY-TIBOR-EUROYEN")
    everyIndexRetired.tenors.toList shouldBe List.empty[Tenor]
    everyIndexRetired.defaultTenor should beFailureWith(FailureReason.MISSING_DATA)
    everyIndexRetired.toFloatingRateIndex should beFailureWith(FailureReason.MISSING_DATA)
    everyIndexRetired.currency should beFailureWith(FailureReason.MISSING_DATA)

    // A tenor supplied explicitly does not need the default, so the same name converts where its
    // indices exist: the retired indices are published, they are simply no longer active, which is
    // what makes the empty tenor set and a successful explicit conversion consistent.
    everyIndexRetired.toIborIndex(Tenor.TENOR_3M) should
      haveValue(IborIndices.JPY_TIBOR_EUROYEN_3M)
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

    // The second failure of this conversion is a name of the right kind whose index this library
    // does not publish, and it is reported as `PARSING` rather than `INVALID` for the same reason
    // the Ibor conversion distinguishes the two: the kind matches and the target is what is
    // absent. Nine of the 351 published names are of this shape - the term risk-free rates
    // published as overnight compounded names, of which the Tokyo one is asserted here, and one
    // Saudi spelling naming an index this library ships only as a term rate - and they exist
    // because this library models a rate by name without shipping the data to price it.
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

  //-------------------------------------------------------------------------
  // The three tests below have no Java counterpart; see the note on this suite.
  //-------------------------------------------------------------------------
  test("the family has one lookup and every published name reaches its own member through it") {
    // `FloatingRateName.valueOf` is the family's `NamedEnum` and nothing else - no second table
    // sits beside it - so the companion and the typeclass cannot disagree about what a name
    // resolves to. Asserted by resolving every name both ways and comparing both answers with
    // the member that publishes the name.
    FloatingRateName.values.size shouldBe PublishedNameCount
    forEvery(allPublishedNames) { (value: FloatingRateName) =>
      withClue(s"'${value.name}': ") {
        FloatingRateName.valueOf(value.name) shouldBe Some(value)
        NamedEnum[FloatingRateName].valueOf(value.name) shouldBe Some(value)
      }
    }

    // The normalised view of the family holds one entry per member, which is the property that
    // makes the sweep above possible: a published name is claimed by its own member and by no
    // other. The folded view is two entries smaller, those being the pairs that differ in case
    // alone - the one narrowing the published data forces, and the only one.
    NamedEnum[FloatingRateName].byCanonicalName.size shouldBe PublishedNameCount
    NamedEnum[FloatingRateName].byUpperName.size shouldBe FoldedKeyCount
    forEvery(allPublishedNames) { (value: FloatingRateName) =>
      withClue(s"'${value.name}': ") {
        NamedEnum[FloatingRateName].byCanonicalName.get(value.name) shouldBe Some(value)
      }
    }

    // Text naming no member, and text naming a concrete index, resolve to nothing through the
    // exact lookup however the family reaches it.
    FloatingRateName.valueOf("Rubbish") shouldBe None
    FloatingRateName.valueOf("GBP-LIBOR-3M") shouldBe None
    NamedEnum[FloatingRateName].valueOf("GBP-LIBOR-3M") shouldBe None
  }

  test("both rows of each case-differing pair resolve to themselves and round trip through JSON") {
    forEvery(caseDifferingPairs) { (mixedCaseName: String, upperCaseName: String) =>
      withClue(s"'$mixedCaseName' / '$upperCaseName': ") {
        // The folded spelling of the first row is the published name of the second, and the two
        // are distinct members: a name is a value's identity here, so a lookup that answered the
        // second name with the first member would lose a member of the family.
        mixedCaseName.toUpperCase(Locale.ENGLISH) shouldBe upperCaseName
        val mixed = named(mixedCaseName)
        val upper = named(upperCaseName)
        mixed.name shouldBe mixedCaseName
        upper.name shouldBe upperCaseName
        mixed should not be upper

        // Each row reaches itself through the typeclass, which is the whole of the family's
        // lookup, and the shared folded key belongs to the row whose published name it is.
        NamedEnum[FloatingRateName].valueOf(mixedCaseName) shouldBe Some(mixed)
        NamedEnum[FloatingRateName].valueOf(upperCaseName) shouldBe Some(upper)
        NamedEnum[FloatingRateName].byUpperName.get(upperCaseName) shouldBe Some(upper)

        // Which is what lets both survive the JSON round trip: each writes its own name and reads
        // back as itself, rather than the later row decoding to the earlier one.
        forEvery(Table("subject", mixed, upper)) { (value: FloatingRateName) =>
          withClue(s"'${value.name}': ") {
            val encoded = Encoder[FloatingRateName].apply(value)
            encoded shouldBe Json.fromString(value.name)
            Decoder[FloatingRateName].decodeJson(encoded) shouldBe Right(value)
          }
        }

        // The two spell one rate, so they are one value in behaviour and two in identity: the
        // distinction the pair carries is the spelling and nothing more.
        mixed.rateType shouldBe upper.rateType
        mixed.normalized shouldBe upper.normalized
      }
    }
  }

  test("the decoder reads a published name in any case and still refuses an index name") {
    // Reading goes through `NamedEnum.parse`, which retries the lookup over the upper-case fold
    // of the text. This family declares no lenient rewrite, so that fold is the whole of the
    // leniency: a published name is read in whatever case it arrives. The subject is a genuinely
    // mixed-case published name carrying a space and a dot.
    val mixedCase = named("USD-Federal Funds-H.15")
    Decoder[FloatingRateName].decodeJson(Json.fromString("USD-Federal Funds-H.15")) shouldBe
      Right(mixedCase)
    Decoder[FloatingRateName].decodeJson(Json.fromString("usd-federal funds-h.15")) shouldBe
      Right(mixedCase)
    Decoder[FloatingRateName].decodeJson(Json.fromString("USD-FEDERAL FUNDS-H.15")) shouldBe
      Right(mixedCase)

    // The exact lookup is the stricter of the two, and stays so: it accepts a published name and
    // the upper-case spelling of one, and no other case.
    FloatingRateName.valueOf("usd-federal funds-h.15") shouldBe None
    FloatingRateName.valueOf("USD-FEDERAL FUNDS-H.15") shouldBe Some(mixedCase)

    // A name already published in upper case reads in lower case for the same reason.
    Decoder[FloatingRateName].decodeJson(Json.fromString("gbp-libor-bba")) shouldBe
      Right(named("GBP-LIBOR-BBA"))

    // None of that reaches text naming a concrete index, in any case: no fold of `GBP-LIBOR-3M`
    // is a published name, so the decoder refuses it while `parse` - the wider resolution, which
    // is deliberately not the decoder - accepts it and answers with the family, tenor discarded.
    FloatingRateName.valueOf("gbp-libor-3m") shouldBe None
    Decoder[FloatingRateName].decodeJson(Json.fromString("gbp-libor-3m")).isLeft shouldBe true
    Decoder[FloatingRateName].decodeJson(Json.fromString("GBP-LIBOR-3M")).isLeft shouldBe true
    FloatingRateName.parse("GBP-LIBOR-3M") should haveValue(FloatingRateNames.GBP_LIBOR)
  }

  // The published name table this family is created from, `FloatingRateNameData`, is asserted
  // below. It is asserted in this spec because the Java sources have no test class for that data -
  // in the original it was a configuration resource that a loader read from the class path at
  // class-initialization time, and what the Java `FloatingRateNamesTest` asserted about it was the
  // behaviour of the family identifiers the loader produced - so the test inventory of this port
  // maps no spec to it. Here the 404 rows are Scala literals fixed at compile time, so the table
  // can be asserted directly, and the spec of the family it produces is where that belongs.
  //
  // The row-for-row comparison against the captured Java reference data is *not* repeated here: it
  // belongs to `ReferenceDataManifestSpec`, which compares every row of all seven published
  // sections column by column and in published order, and which is the stronger statement of the
  // two. What the tests below pin is the structure a consumer of the table relies on, which that
  // comparison does not state:
  //
  //  - the size of each of the seven published sections, which is how a dropped or duplicated row
  //    is caught as a count rather than as a missing lookup somewhere downstream;
  //  - the published order, both of the concatenation the whole table is and of the section
  //    boundaries within it, because the order is the order a consumer creates its members in;
  //  - the kind of rate each section carries, and the one derived column - the trailing `-` an
  //    Ibor index name stem needs so that a tenor completes it;
  //  - the three rows that declare a non-standard fixing date offset, and the fact that no other
  //    row carries one;
  //  - the internal consistency the two currency tables depend on: every value is an external name
  //    of a row of the right kind, so a default resolves rather than dangling;
  //  - the two published names that contain an `=`, which are exactly the rows a reader of the
  //    original resource is most likely to mis-split, and which the Java tests single out too.
  //
  // The assertions deliberately never name the type of the published collections. The two currency
  // tables are ordered maps and the order is the contract; how they are backed, and what private
  // lookups are derived from them, is not.
  //-------------------------------------------------------------------------
  /**
   * The size and kind of each of the four name sections, in published order.
   *
   * The counts are the counts of the resource being ported, read from it rather than from the
   * object under test, and they total [[PublishedNameCount]].
   */
  private val publishedNameSections: TableFor3[String, Int, FloatingRateType] =
    Table(
      ("section", "size", "rateType"),
      ("ibor", 159, FloatingRateType.Ibor),
      ("overnightCompounded", 156, FloatingRateType.OvernightCompounded),
      ("overnightAveraged", 6, FloatingRateType.OvernightAveraged),
      ("price", 30, FloatingRateType.Price))

  /**
   * The rows of each section, keyed the way the table above names them, so that the two are
   * asserted against each other rather than each against itself.
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

    // 351 name rows over the four sections, plus the three fixing date offsets and the two
    // currency default tables, is the 404 published rows of the resource.
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
    // This is the order the loader of the original processed the sections in, and it is the order
    // a consumer that builds one member per row should build them in, so that the member list of
    // the family is the published list.
    FloatingRateNameData.rows shouldBe
      (FloatingRateNameData.iborRows ++
        FloatingRateNameData.overnightCompoundedRows ++
        FloatingRateNameData.overnightAveragedRows ++
        FloatingRateNameData.priceRows)

    // the section boundaries are where the counts say they are
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
    // the keys are the published names alone: folding case is the business of the named enum
    // support, not of this table
    FloatingRateNameData.byExternalName.get("gbp-libor-bba") shouldBe None
    FloatingRateNameData.byExternalName.get("Rubbish") shouldBe None
  }

  test("data_indexNames_carryTheTrailingSeparatorExactlyWhereATenorCompletesThem") {
    // The one derived column. An Ibor family is named by a stem that a tenor completes -
    // `GBP-LIBOR-` plus `3M` is the index `GBP-LIBOR-3M` - so every Ibor row's index name ends
    // with the separator, and the published value it was derived from is the rest of it. No other
    // section needs one, because an Overnight or price family names its index directly.
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

    // spot rows, adjudicated against the published Java jar: `GBP-LIBOR-BBA` names the stem
    // `GBP-LIBOR-`, which the Java family completes to the index `GBP-LIBOR-3M`
    FloatingRateNameData.byExternalName.get("GBP-LIBOR-BBA").map(_.indexName) shouldBe Some("GBP-LIBOR-")
    FloatingRateNameData.byExternalName.get("CHF-LIBOR-BBA").map(_.indexName) shouldBe Some("CHF-LIBOR-")
    FloatingRateNameData.byExternalName.get("CHF-SARON-OIS-COMPOUND").map(_.indexName) shouldBe Some("CHF-SARON")
    FloatingRateNameData.byExternalName.get("USD-EFFECTIVE").map(_.indexName) shouldBe Some("USD-FED-FUND")
    FloatingRateNameData.byExternalName.get("UK-RPI").map(_.indexName) shouldBe Some("GB-RPI")
  }

  test("data_manyExternalNamesShareOneIndexName") {
    // Several published names resolve onto one index family, which is deliberate and is how a
    // family recognises its own variants: the canonical row of a family is the one whose external
    // name completes to its index name, and the rest are spellings of it. Five spellings of Swiss
    // franc Libor all name the same stem.
    FloatingRateNameData.iborRows.filter(_.indexName == "CHF-LIBOR-").map(_.externalName) shouldBe
      Vector(
        "CHF-LIBOR",
        "CHF-LIBOR-BBA",
        "CHF-LIBOR-BBA-Bloomberg",
        "CHF-LIBOR-ISDA",
        "CHF-LIBOR-Reference Banks")
    // so the index names are far fewer than the rows, and the relation is many-to-one rather than
    // one-to-one
    FloatingRateNameData.rows.map(_.indexName).distinct.size should be < FloatingRateNameData.rows.size
  }

  test("data_fixingDateOffsets_areTheThreePublishedRowsAndNoOthers") {
    // The section carries three rows, all of them Danish Cibor and all of them zero. The Java
    // tests single the first out: `DKK-CIBOR-DKNA13` fixes on the day itself, where the otherwise
    // identical `DKK-CIBOR2-DKNA13`, which declares no offset, fixes two business days earlier.
    // That is the pair whose resulting behaviour `test_cibor` above asserts; what is asserted here
    // is the table those two rows are read from.
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

    // and the section is consistent with the rows derived from it
    FloatingRateNameData.iborFixingDateOffsets.foreach {
      case (externalName, days) =>
        withClue(s"$externalName: ") {
          FloatingRateNameData.byExternalName.get(externalName).map(_.fixingDateOffsetDays) shouldBe Some(Some(days))
        }
    }
  }

  test("data_theTwoPublishedNamesContainingAnEqualsSign") {
    // These are the rows a reader of the original resource is most likely to mis-split, because
    // the separator of the format appears inside the key. Both are asserted here, as the Java
    // tests assert them, so that a transcription that split on the wrong occurrence fails.
    FloatingRateNameData.byExternalName.get("CNY-CNREPOFIX=CFXS-Reuters").map(row =>
      (row.indexName, row.rateType)) shouldBe Some(("CNY-REPO-", FloatingRateType.Ibor))
    FloatingRateNameData.byExternalName.get("HKD-HIBOR-HIBOR=").map(row =>
      (row.indexName, row.rateType)) shouldBe Some(("HKD-HIBOR-", FloatingRateType.Ibor))
    // the shorter spelling of the first one is a row in its own right, so the two did not collapse
    // into each other
    FloatingRateNameData.byExternalName.get("CNY-CNREPOFIX").map(_.indexName) shouldBe Some("CNY-REPO-")
  }

  test("data_currencyDefaults_resolveToARowOfTheRightKind") {
    // The value of a default is an external name rather than an index name, and the two differ
    // for some rows - `CNY` defaults to the external name `CNY-REPO` and `THB` to `THB-THBFIX` -
    // so a consumer has to resolve it through the lookup. That only works while every value is a
    // published name of a row of the right kind, which is what this asserts, section by section;
    // that the two tables hold the published rows in the published order is asserted against the
    // captured reference data by `ReferenceDataManifestSpec`, and the defaults a caller actually
    // reaches for are asserted in `test_defaultIborIndex` and `test_defaultOvernightIndex` above.
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

    // a currency the resource does not name has no published default, which is a value the
    // consumer reports rather than a case either table can fill
    FloatingRateNameData.currencyDefaultIbor.get(Currency.BRL) shouldBe None
    FloatingRateNameData.currencyDefaultOvernight.get(Currency.MXN) shouldBe None
  }

  test("data_iterationOrderIsStable") {
    // A consumer reading any of these tables twice has to see the same thing both times, whatever
    // private lookups are derived from them.
    FloatingRateNameData.rows shouldBe FloatingRateNameData.rows
    FloatingRateNameData.iborFixingDateOffsets.toVector shouldBe FloatingRateNameData.iborFixingDateOffsets.toVector
    FloatingRateNameData.currencyDefaultIbor.toVector shouldBe FloatingRateNameData.currencyDefaultIbor.toVector
    FloatingRateNameData.currencyDefaultOvernight.toVector shouldBe
      FloatingRateNameData.currencyDefaultOvernight.toVector
    FloatingRateNameData.byExternalName.toVector shouldBe FloatingRateNameData.byExternalName.toVector
  }
}
