/*
 * Copyright (C) 2020 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import cats.Show

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2

import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/** Test [[StandardSchemes]]. */
class StandardSchemesSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  private val SchemeCount: Int = 24

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
  private def identifier(scheme: String, value: String): StandardId = {
    val outcome = StandardId.of(scheme, value)
    outcome should beSuccess
    outcome.getOrElse(fail(s"StandardId.of('$scheme', '$value') was expected to name an identifier"))
  }

  private val MicLengthPrefix: String = "MIC must have 4 characters, but was "

  /**
   * Picks the length failure out of what `createTicMic` accumulated over a refused market
   * identifier code.
   *
   * A code that is both the wrong length and unusable inside an identifier value is reported
   * by two failures, so selecting by wording - rather than taking the sole member of a chain -
   * is what lets the tests below hand the helper an adversarial code and still assert against
   * the failure they mean.
   */
  private def refusedMic(exchangeMic: String): Failure = {
    val outcome = StandardSchemes.createTicMic("ULVR", exchangeMic)
    outcome should beFailureWith(FailureReason.INVALID)
    outcome.left.toOption
      .flatMap(failures => failures.find(failure => failure.message.startsWith(MicLengthPrefix)))
      .getOrElse(
        fail(s"createTicMic was expected to refuse a MIC of ${exchangeMic.length} characters"))
  }

  //-------------------------------------------------------------------------
  test("test_schemes") {
    // Two spellings are deliberate rather than typing errors: OG_COUNTERPARTY and OG_PORTFOLIO
    // carry no `_SCHEME` suffix, unlike the other twenty-two constants, and CHAIN_RIC_SCHEME
    // holds `CHAINRIC` with no separator, so its name and its value disagree.
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
    StandardSchemes.CHAIN_RIC_SCHEME shouldBe "CHAINRIC"
    StandardSchemes.BBG_SCHEME shouldBe "BBG"
    StandardSchemes.FIGI_SCHEME shouldBe "FIGI"
    StandardSchemes.LEI_SCHEME shouldBe "LEI"
    StandardSchemes.RED6_SCHEME shouldBe "RED6"
    StandardSchemes.RED9_SCHEME shouldBe "RED9"
    StandardSchemes.OPRA_SCHEME shouldBe "OPRA"
    StandardSchemes.OCC_SCHEME shouldBe "OCC"

    data_schemes.length shouldBe SchemeCount

    forAll(data_schemes) { (constant, scheme) =>
      withClue(s"$constant: ") {
        StandardId.of(scheme, "x") should beSuccess
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_ticMic") {
    val ticMic = identifier("TICMIC", "ULVR@XLON")
    StandardSchemes.createTicMic("ULVR", "XLON") should haveValue(ticMic)

    val shortMic = StandardSchemes.createTicMic("ULVR", "LSE")
    shortMic should beFailureWith(FailureReason.INVALID)
    shortMic should haveFailureMessageMatching(".*MIC must have 4 characters, but was LSE.*")

    StandardSchemes.splitTicMic(ticMic) should haveValue(("ULVR", "XLON"))

    val noSeparator = StandardSchemes.splitTicMic(identifier("TICMIC", "ULVR"))
    noSeparator should beFailureWith(FailureReason.PARSING)
    noSeparator should haveFailureMessageMatching(".*TICMIC~ULVR.*")

    val tooShort = StandardSchemes.splitTicMic(identifier("TICMIC", "A@B"))
    tooShort should beFailureWith(FailureReason.PARSING)
    tooShort should haveFailureMessageMatching(".*TICMIC~A@B.*")

    val micNotFourCharacters = StandardSchemes.splitTicMic(identifier("TICMIC", "ABC@BOB"))
    micNotFourCharacters should beFailureWith(FailureReason.PARSING)
    micNotFourCharacters should haveFailureMessageMatching(".*TICMIC~ABC@BOB.*")
  }

  test("splitting names a rejected identifier in full, and the failure renders bounded") {
    val payload = "H" * 10000
    val bounded = StandardSchemes.splitTicMic(identifier("TICMIC", payload))
    bounded should beFailureWith(FailureReason.PARSING)
    val failure = bounded.left.toOption.getOrElse(fail("expected a failure"))
    failure.message shouldBe s"Invalid TICMIC identifier: TICMIC~$payload"
    val rendered = Show[Failure].show(failure)
    rendered.length should be < 1000
    rendered should startWith("PARSING: Invalid TICMIC identifier: TICMIC~HHH")
    rendered should endWith("...")
    failure.message should not include "\n"
    rendered should not include "\n"
    rendered should not include "\r"

    StandardSchemes
      .splitTicMic(identifier("TICMIC", "ULVR"))
      .left
      .toOption
      .map(failure => failure.message) shouldBe Some("Invalid TICMIC identifier: TICMIC~ULVR")
  }

  test("creating names a rejected market identifier code in full, and the failure renders bounded") {
    val lineFeed = refusedMic("XL\nON")
    lineFeed.message shouldBe "MIC must have 4 characters, but was XL\nON"
    val renderedLineFeed = Show[Failure].show(lineFeed)
    renderedLineFeed shouldBe "INVALID: MIC must have 4 characters, but was XL\\nON"
    lineFeed.toString shouldBe renderedLineFeed
    renderedLineFeed.linesIterator.size shouldBe 1
    renderedLineFeed.exists(character => character.isControl) shouldBe false

    val carriageReturn = refusedMic("XL\rON")
    carriageReturn.message shouldBe "MIC must have 4 characters, but was XL\rON"
    val renderedCarriageReturn = Show[Failure].show(carriageReturn)
    renderedCarriageReturn shouldBe "INVALID: MIC must have 4 characters, but was XL\\rON"
    carriageReturn.toString shouldBe renderedCarriageReturn
    renderedCarriageReturn.linesIterator.size shouldBe 1
    renderedCarriageReturn.exists(character => character.isControl) shouldBe false

    val payload = "H" * 10000
    val oversized = refusedMic(payload)
    oversized.message shouldBe s"MIC must have 4 characters, but was $payload"
    val renderedOversized = Show[Failure].show(oversized)
    renderedOversized.length should be < 1000
    renderedOversized should startWith("INVALID: MIC must have 4 characters, but was HHH")
    renderedOversized should endWith("...")
    oversized.toString shouldBe renderedOversized

    val shortMic = refusedMic("LSE")
    shortMic.message shouldBe "MIC must have 4 characters, but was LSE"
    Show[Failure].show(shortMic) shouldBe "INVALID: MIC must have 4 characters, but was LSE"
  }
}
