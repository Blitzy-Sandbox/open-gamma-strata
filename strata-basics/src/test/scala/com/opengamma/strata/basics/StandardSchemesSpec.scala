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
 * Two further tests sit beside that pair, named for the property they pin rather than for a
 * Java method because the original has no equivalent. Each states the policy both helpers
 * follow where they quote text a caller supplied: the failure carries the whole of what was
 * refused, in the wording the Java method produced, while the bound on the size of the
 * diagnostic and the escaping of anything that could forge a line of a log belong to the
 * rendering of the failure.
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

  /**
   * How every message about the length of a market identifier code begins.
   *
   * The wording is the one the Java method produced, and it is written out here rather than
   * assembled, so a change to it fails the assertions below rather than being carried into
   * them.
   */
  private val MicLengthPrefix: String = "MIC must have 4 characters, but was "

  /**
   * Builds the failure the factory reports over a market identifier code it refuses.
   *
   * The two checks of the factory accumulate, so a code that is both the wrong length and
   * unusable inside an identifier value is reported by two failures rather than one.
   * Selecting the failure about the length by its wording, instead of taking the sole member
   * of a chain, is what lets the cases below hand the factory an adversarial code and still
   * assert against the failure they mean.
   *
   * @param exchangeMic  the market identifier code the factory is expected to refuse
   * @return the failure reporting the length of that code
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

  /**
   * Asserts that a rejected identifier is named in full and rendered bounded and on one line.
   *
   * Beyond the Java test: the value part of an identifier is caller-supplied text that nothing
   * bounds, and the method being ported interpolated the whole of it into the exception it
   * threw. This port names it the same way in the failure it returns, and bounds it where the
   * failure is written out.
   */
  test("splitting names a rejected identifier in full, and the failure renders bounded") {
    // An identifier whose value is ten thousand characters and holds no separator: a perfectly
    // legal identifier, and not a TICMIC.
    val payload = "H" * 10000
    val bounded = StandardSchemes.splitTicMic(identifier("TICMIC", payload))
    bounded should beFailureWith(FailureReason.PARSING)
    val failure = bounded.left.toOption.getOrElse(fail("expected a failure"))
    // The message quotes the identifier as `toString` writes it, whole.
    failure.message shouldBe s"Invalid TICMIC identifier: TICMIC~$payload"
    // The rendering is where the size stops, and it marks what it left out.
    val rendered = Show[Failure].show(failure)
    rendered.length should be < 1000
    rendered should startWith("PARSING: Invalid TICMIC identifier: TICMIC~HHH")
    rendered should endWith("...")
    // A line break can reach neither the message nor the rendering here, though for two
    // different reasons: the characters a value may hold stop below the space, and a rendering
    // escapes any that could forge a line.
    failure.message should not include "\n"
    rendered should not include "\n"
    rendered should not include "\r"

    // And the message for an ordinary rejected identifier is unchanged, character for
    // character, which is what makes the bound invisible to every caller but the adversarial
    // one.
    StandardSchemes
      .splitTicMic(identifier("TICMIC", "ULVR"))
      .left
      .toOption
      .map(failure => failure.message) shouldBe Some("Invalid TICMIC identifier: TICMIC~ULVR")
  }

  /**
   * Asserts that a rejected market identifier code is named in full and rendered bounded and
   * on one line.
   *
   * Beyond the Java test: a market identifier code is text a caller supplied, and the method
   * being ported wrote the whole of it into the exception it threw. This port keeps that
   * wording character for character in the failure it returns, so a caller is handed exactly
   * what it has to correct, and it is the rendering of the failure that bounds the text and
   * escapes what a line-oriented reader could act on. Both halves are asserted together,
   * because either one on its own would serve the caller or the log but not both.
   */
  test("creating names a rejected market identifier code in full, and the failure renders bounded") {
    // A code holding a line feed. The message carries that line feed as it stands, and the
    // rendering writes it as the two characters of an escape, so the failure occupies a single
    // line however it reaches a log.
    val lineFeed = refusedMic("XL\nON")
    lineFeed.message shouldBe "MIC must have 4 characters, but was XL\nON"
    val renderedLineFeed = Show[Failure].show(lineFeed)
    renderedLineFeed shouldBe "INVALID: MIC must have 4 characters, but was XL\\nON"
    // The text form of a failure is that same rendering, so writing one out cannot bypass the
    // escaping by asking for its text instead.
    lineFeed.toString shouldBe renderedLineFeed
    renderedLineFeed.linesIterator.size shouldBe 1
    renderedLineFeed.exists(character => character.isControl) shouldBe false

    // A carriage return is neutralised the same way, and it is the one a reader that ends a
    // line on it would otherwise act on.
    val carriageReturn = refusedMic("XL\rON")
    carriageReturn.message shouldBe "MIC must have 4 characters, but was XL\rON"
    val renderedCarriageReturn = Show[Failure].show(carriageReturn)
    renderedCarriageReturn shouldBe "INVALID: MIC must have 4 characters, but was XL\\rON"
    carriageReturn.toString shouldBe renderedCarriageReturn
    renderedCarriageReturn.linesIterator.size shouldBe 1
    renderedCarriageReturn.exists(character => character.isControl) shouldBe false

    // Ten thousand characters. The message holds all of them, since that is the code the
    // caller has to correct, while the rendering stops far short of them and marks what it
    // left out. The ceiling asserted is a size a reader can take in rather than the constant
    // the renderer holds, so this pins the property and not the number.
    val payload = "H" * 10000
    val oversized = refusedMic(payload)
    oversized.message shouldBe s"MIC must have 4 characters, but was $payload"
    val renderedOversized = Show[Failure].show(oversized)
    renderedOversized.length should be < 1000
    renderedOversized should startWith("INVALID: MIC must have 4 characters, but was HHH")
    renderedOversized should endWith("...")
    oversized.toString shouldBe renderedOversized

    // And an ordinary rejected code reads as the Java wording character for character, in the
    // failure and in its rendering alike, which is what makes the bound invisible to every
    // caller but the adversarial one.
    val shortMic = refusedMic("LSE")
    shortMic.message shouldBe "MIC must have 4 characters, but was LSE"
    Show[Failure].show(shortMic) shouldBe "INVALID: MIC must have 4 characters, but was LSE"
  }
}
