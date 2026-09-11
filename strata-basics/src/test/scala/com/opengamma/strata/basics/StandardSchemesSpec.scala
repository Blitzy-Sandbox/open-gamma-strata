/*
 * Copyright (C) 2020 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2

import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[StandardSchemes]], ported from the Java `StandardSchemesTest`.
 *
 * ===Why a test that only reads constants is worth having===
 *
 * The two tests of the Java original are kept here one for one, under the same names. The
 * first of them looks like a test that asserts a program against itself, and it is not: the
 * strings these constants hold reach stored documents, other modules and the tests of those
 * modules, so a scheme renamed by a careless edit would be a silent change to data already
 * written rather than a compilation error. Each of the twenty-four values is therefore
 * spelled out again here, against the name of the constant that holds it, which is what makes
 * such an edit fail.
 *
 * Three of them are easy to mistake for typing errors and are intentional:
 *
 *   - [[StandardSchemes.CHAIN_RIC_SCHEME]] is named with an underscore and holds `CHAINRIC`
 *     with no separator at all, so the name and the value deliberately disagree;
 *   - [[StandardSchemes.OG_COUNTERPARTY]] and [[StandardSchemes.OG_PORTFOLIO]] carry no
 *     `_SCHEME` suffix, unlike the twenty-two constants around them.
 *
 * ===How the shape of the port changes the assertions===
 *
 * Two differences from the original are structural rather than a matter of taste:
 *
 *   - Neither helper throws. Whether a market identifier code is four characters long, and
 *     whether an identifier has the shape of a TICMIC, depend on the data that reached the
 *     program, so each helper reports what was wrong as a value and every assertion that was
 *     `assertThatIllegalArgumentException` is an assertion that the outcome is a failure -
 *     carrying, in each case, the reason and the message the helper assigns.
 *   - The pair the second helper returns is an ordinary `Tuple2`, the tuple type of the
 *     language, in place of the pair type of the library being ported.
 *
 * The first test also asserts one thing the original did not, for the cost of a single line:
 * that every value here is itself a scheme [[StandardId.of]] accepts. That is the sole
 * purpose these constants exist for, and nothing else in the module checks it, so a value
 * holding a character a scheme may not hold would otherwise be found by whichever caller
 * first tried to build an identifier with it.
 *
 * Each of the two tests keeps the name of the Java method it comes from, and the whole of a
 * Java method stays one test here, which is what keeps the method-level traceability of the
 * migration exact.
 *
 * @see [[StandardSchemes]] for the constants and the two helpers under test
 */
class StandardSchemesSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The number of schemes the object publishes.
   *
   * The table below drives the check that every scheme is usable, so a row lost from it
   * would weaken that check silently rather than fail. Asserting the count says out loud how
   * many schemes there are, and that number is also the count the twenty-four assertions of
   * `test_schemes` are read against.
   */
  private val SchemeCount: Int = 24

  /**
   * Every scheme the object publishes, each against the name of the constant holding it.
   *
   * The rows follow the declaration order of the object, which is the order the Java test
   * asserted them in: the eight OpenGamma schemes, the seven schemes of the public numbering
   * systems, then the nine schemes of the commercial identifiers. The constants are read
   * here rather than repeated as literals, so this table tracks the object while the
   * assertions of `test_schemes` pin the object to its values.
   */
  private val data_schemes: TableFor2[String, String] = Table(
    ("constant", "scheme"),
    ("OG_TICKER_SCHEME", StandardSchemes.OG_TICKER_SCHEME),
    ("OG_ETD_SCHEME", StandardSchemes.OG_ETD_SCHEME),
    ("OG_TRADE_SCHEME", StandardSchemes.OG_TRADE_SCHEME),
    ("OG_POSITION_SCHEME", StandardSchemes.OG_POSITION_SCHEME),
    ("OG_SENSITIVITY_SCHEME", StandardSchemes.OG_SENSITIVITY_SCHEME),
    ("OG_SECURITY_SCHEME", StandardSchemes.OG_SECURITY_SCHEME),
    ("OG_COUNTERPARTY", StandardSchemes.OG_COUNTERPARTY),
    ("OG_PORTFOLIO", StandardSchemes.OG_PORTFOLIO),
    ("TICKER_SCHEME", StandardSchemes.TICKER_SCHEME),
    ("TICMIC_SCHEME", StandardSchemes.TICMIC_SCHEME),
    ("ISIN_SCHEME", StandardSchemes.ISIN_SCHEME),
    ("CUSIP_SCHEME", StandardSchemes.CUSIP_SCHEME),
    ("SEDOL_SCHEME", StandardSchemes.SEDOL_SCHEME),
    ("WKN_SCHEME", StandardSchemes.WKN_SCHEME),
    ("VALOR_SCHEME", StandardSchemes.VALOR_SCHEME),
    ("RIC_SCHEME", StandardSchemes.RIC_SCHEME),
    ("CHAIN_RIC_SCHEME", StandardSchemes.CHAIN_RIC_SCHEME),
    ("BBG_SCHEME", StandardSchemes.BBG_SCHEME),
    ("FIGI_SCHEME", StandardSchemes.FIGI_SCHEME),
    ("LEI_SCHEME", StandardSchemes.LEI_SCHEME),
    ("RED6_SCHEME", StandardSchemes.RED6_SCHEME),
    ("RED9_SCHEME", StandardSchemes.RED9_SCHEME),
    ("OPRA_SCHEME", StandardSchemes.OPRA_SCHEME),
    ("OCC_SCHEME", StandardSchemes.OCC_SCHEME))

  //-------------------------------------------------------------------------
  /**
   * Builds an identifier, asserting that the factory accepted the two parts.
   *
   * The factory reports its failures rather than throwing, so a spec that wants the value
   * has to say what should happen when there is none. Asserting success here rather than at
   * every call site keeps the second test reading like the Java one, and routing the
   * assertion through `beSuccess` means an unexpected rejection is reported with the reason
   * and message of every failure rather than as a failed pattern match.
   *
   * @param scheme  the scheme of the identifier
   * @param value  the value of the identifier
   * @return the identifier the two parts name
   */
  private def identifier(scheme: String, value: String): StandardId = {
    val outcome = StandardId.of(scheme, value)
    outcome should beSuccess
    outcome.getOrElse(fail(s"StandardId.of('$scheme', '$value') was expected to name an identifier"))
  }

  //-------------------------------------------------------------------------
  test("test_schemes") {
    StandardSchemes.OG_TICKER_SCHEME shouldBe "OG-Ticker"
    StandardSchemes.OG_ETD_SCHEME shouldBe "OG-ETD"
    StandardSchemes.OG_TRADE_SCHEME shouldBe "OG-Trade"
    StandardSchemes.OG_POSITION_SCHEME shouldBe "OG-Position"
    StandardSchemes.OG_SENSITIVITY_SCHEME shouldBe "OG-Sensitivity"
    StandardSchemes.OG_SECURITY_SCHEME shouldBe "OG-Security"
    StandardSchemes.OG_COUNTERPARTY shouldBe "OG-Counterparty"
    StandardSchemes.OG_PORTFOLIO shouldBe "OG-Portfolio"

    StandardSchemes.TICKER_SCHEME shouldBe "TICKER"
    StandardSchemes.TICMIC_SCHEME shouldBe "TICMIC"
    StandardSchemes.ISIN_SCHEME shouldBe "ISIN"
    StandardSchemes.CUSIP_SCHEME shouldBe "CUSIP"
    StandardSchemes.SEDOL_SCHEME shouldBe "SEDOL"
    StandardSchemes.WKN_SCHEME shouldBe "WKN"
    StandardSchemes.VALOR_SCHEME shouldBe "VALOR"

    StandardSchemes.RIC_SCHEME shouldBe "RIC"
    // The name of this one holds an underscore that its value does not: it is `CHAINRIC`.
    StandardSchemes.CHAIN_RIC_SCHEME shouldBe "CHAINRIC"
    StandardSchemes.BBG_SCHEME shouldBe "BBG"
    StandardSchemes.FIGI_SCHEME shouldBe "FIGI"
    StandardSchemes.LEI_SCHEME shouldBe "LEI"
    StandardSchemes.RED6_SCHEME shouldBe "RED6"
    StandardSchemes.RED9_SCHEME shouldBe "RED9"
    StandardSchemes.OPRA_SCHEME shouldBe "OPRA"
    StandardSchemes.OCC_SCHEME shouldBe "OCC"

    // Every one of those twenty-four schemes is named above and appears once in the table,
    // which is what lets the check below stand for the whole object.
    data_schemes.length shouldBe SchemeCount

    // Beyond the Java test: each value has to be a scheme an identifier can actually be filed
    // under, since that is the only thing these constants are for. The value is the shortest
    // acceptable one, because it is the scheme that is under test here.
    forAll(data_schemes) { (constant, scheme) =>
      withClue(s"$constant: ") {
        StandardId.of(scheme, "x") should beSuccess
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_ticMic") {
    // The scheme is written out rather than read from the constant, as the Java test wrote
    // it, so that this asserts which scheme the helper files a TICMIC under and not merely
    // that it agrees with itself.
    val ticMic = identifier("TICMIC", "ULVR@XLON")
    StandardSchemes.createTicMic("ULVR", "XLON") should haveValue(ticMic)

    // A market identifier code has to be four characters: `LSE` is three. The value the two
    // parts would form, `ULVR@LSE`, is a perfectly acceptable identifier value, so the length
    // of the code is the only thing reported.
    val shortMic = StandardSchemes.createTicMic("ULVR", "LSE")
    shortMic should beFailureWith(FailureReason.INVALID)
    shortMic should haveFailureMessageMatching(".*MIC must have 4 characters, but was LSE.*")

    // The pair is a `Tuple2`, in the order ticker then market identifier code.
    StandardSchemes.splitTicMic(ticMic) should haveValue(("ULVR", "XLON"))

    // Nothing separates a ticker from a code here.
    val noSeparator = StandardSchemes.splitTicMic(identifier("TICMIC", "ULVR"))
    noSeparator should beFailureWith(FailureReason.PARSING)
    noSeparator should haveFailureMessageMatching(".*TICMIC~ULVR.*")

    // A separator is present, but the value is too short to hold a ticker and a four
    // character code either side of it.
    val tooShort = StandardSchemes.splitTicMic(identifier("TICMIC", "A@B"))
    tooShort should beFailureWith(FailureReason.PARSING)
    tooShort should haveFailureMessageMatching(".*TICMIC~A@B.*")

    // Long enough, separated, and still not a TICMIC: the code after the separator is three
    // characters rather than four.
    val micNotFourCharacters = StandardSchemes.splitTicMic(identifier("TICMIC", "ABC@BOB"))
    micNotFourCharacters should beFailureWith(FailureReason.PARSING)
    micNotFourCharacters should haveFailureMessageMatching(".*TICMIC~ABC@BOB.*")
  }
}
