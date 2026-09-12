/*
 * Copyright (C) 2020 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import cats.syntax.apply._
import cats.syntax.either._

import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.result.Failure

/**
 * A set of schemes that can be used with [[StandardId]].
 *
 * The scheme part of a [[StandardId]] is freeform, but it obviously works best where the
 * scheme name is widely agreed. This object holds the schemes that are widely agreed, so that
 * a caller names one of them rather than repeating a string literal that a neighbouring
 * caller may spell differently:
 *
 * {{{
 * StandardId.of(StandardSchemes.ISIN_SCHEME, "GB0031348658")
 * }}}
 *
 * ===What this object is===
 *
 * Every member is either a scheme string or a helper over one of them, so there is no data
 * type here and nothing to serialize. The type being ported was a final class with a private
 * constructor holding only static members, which is what an `object` is, so the restriction
 * on instantiating it needs no expression of its own.
 *
 * The names and the values of the constants are exactly those of the class being ported, down
 * to the two members that do not carry the `_SCHEME` suffix ([[OG_COUNTERPARTY]] and
 * [[OG_PORTFOLIO]]), because both the identifiers and the strings they hold reach stored
 * documents, other modules and tests.
 *
 * ===How failures are reported===
 *
 * The two helpers below can be handed data they cannot use - a market identifier code of the
 * wrong length, or an identifier that does not have the shape of a TICMIC. Whether they
 * succeed therefore depends on the data that reached the program rather than on the calling
 * code being correct, so each reports what was wrong as a value: [[createTicMic]] returns a
 * `ResultNec` because two independent things can be wrong with its two arguments, and
 * [[splitTicMic]] returns a `FailureOr` because a malformed identifier is a single cause.
 * Neither throws.
 *
 * This object holds no state and every member is a pure function of its arguments, so it is
 * safe to use from any number of threads.
 *
 * @see [[StandardId]] for the identifier these schemes categorize
 */
object StandardSchemes {

  //-------------------------------------------------------------------------
  /**
   * The OpenGamma scheme used to identify values in market data.
   */
  val OG_TICKER_SCHEME: String = "OG-Ticker"

  /**
   * The OpenGamma scheme used to identify ETDs in market data.
   */
  val OG_ETD_SCHEME: String = "OG-ETD"

  /**
   * The OpenGamma scheme used for trade identifiers.
   */
  val OG_TRADE_SCHEME: String = "OG-Trade"

  /**
   * The OpenGamma scheme used for position identifiers.
   */
  val OG_POSITION_SCHEME: String = "OG-Position"

  /**
   * The OpenGamma scheme used for portfolio sensitivity identifiers.
   */
  val OG_SENSITIVITY_SCHEME: String = "OG-Sensitivity"

  /**
   * The OpenGamma scheme used for securities.
   */
  val OG_SECURITY_SCHEME: String = "OG-Security"

  /**
   * The OpenGamma scheme used for counterparties.
   *
   * This is one of the two members whose name carries no `_SCHEME` suffix, which is the name
   * the class being ported gave it.
   */
  val OG_COUNTERPARTY: String = "OG-Counterparty"

  /**
   * The OpenGamma scheme used for portfolios.
   *
   * This is one of the two members whose name carries no `_SCHEME` suffix, which is the name
   * the class being ported gave it.
   */
  val OG_PORTFOLIO: String = "OG-Portfolio"

  //-------------------------------------------------------------------------
  /**
   * The scheme for exchange Tickers.
   *
   * A ticker is the human-readable identifier used by the exchange for a security. It is not a
   * stable identifier, and each exchange defines their own tickers. A company can change
   * ticker over time, such as if the company merges or changes its name. If a company ceases
   * to use a particular ticker, the ticker can be reused for an entirely different company.
   * Tickers are typically reused over time as companies change.
   *
   * A ticker is unlikely to be useful as an identifier without an exchange, see
   * [[TICMIC_SCHEME]].
   */
  val TICKER_SCHEME: String = "TICKER"

  /**
   * The scheme for TICMICs combining the exchange Ticker with the exchange MIC.
   *
   * A TICMIC is an identifier that combines the Ticker, as defined by the exchange (see
   * [[TICKER_SCHEME]]), with the MIC (Market Identifier Code) that identifies the exchange.
   * The format is `<ticker>@<exchangeMic>`. For example, `ULVR@XLON` represents Unilever on
   * the London Stock Exchange.
   *
   * Identifiers in this scheme are assembled by [[createTicMic]] and taken apart again by
   * [[splitTicMic]].
   */
  val TICMIC_SCHEME: String = "TICMIC"

  /**
   * The scheme for ISINs, the International Securities Identification Number.
   *
   * See https://en.wikipedia.org/wiki/International_Securities_Identification_Number
   */
  val ISIN_SCHEME: String = "ISIN"

  /**
   * The scheme for CUSIPs, the North American numbering system.
   *
   * See https://en.wikipedia.org/wiki/CUSIP
   */
  val CUSIP_SCHEME: String = "CUSIP"

  /**
   * The scheme for SEDOLs, the United Kingdom numbering system.
   *
   * See https://en.wikipedia.org/wiki/SEDOL
   */
  val SEDOL_SCHEME: String = "SEDOL"

  /**
   * The scheme for Wertpapierkennnummer, the German numbering system.
   *
   * See https://en.wikipedia.org/wiki/Wertpapierkennnummer
   */
  val WKN_SCHEME: String = "WKN"

  /**
   * The scheme for VALOR numbers, the Swiss numbering system.
   *
   * See https://en.wikipedia.org/wiki/Valoren_number
   */
  val VALOR_SCHEME: String = "VALOR"

  //-------------------------------------------------------------------------
  /**
   * The scheme for RICs, the Reuters Instrument Code.
   *
   * See https://en.wikipedia.org/wiki/Reuters_Instrument_Code
   */
  val RIC_SCHEME: String = "RIC"

  /**
   * The scheme for Chain RICs, which identifies a set of linked RICs.
   */
  val CHAIN_RIC_SCHEME: String = "CHAINRIC"

  /**
   * The scheme for Bloomberg Tickers.
   */
  val BBG_SCHEME: String = "BBG"

  /**
   * The scheme for FIGIs, the Financial Instrument Global Identifier.
   *
   * See https://en.wikipedia.org/wiki/Financial_Instrument_Global_Identifier
   */
  val FIGI_SCHEME: String = "FIGI"

  /**
   * The scheme for LEIs, the Legal Entity Identifier.
   *
   * See https://en.wikipedia.org/wiki/Legal_Entity_Identifier
   */
  val LEI_SCHEME: String = "LEI"

  /**
   * The scheme for 6 character RED codes, the Reference Entity Data code.
   *
   * See https://ihsmarkit.com/products/red-cds.html
   */
  val RED6_SCHEME: String = "RED6"

  /**
   * The scheme for 9 character RED codes, the Reference Entity Data code.
   *
   * See https://ihsmarkit.com/products/red-cds.html
   */
  val RED9_SCHEME: String = "RED9"

  /**
   * The scheme for OPRA option codes.
   *
   * These codes have:
   *
   *   - 1 to 5 characters for the underlying root symbol
   *   - 1 letter representing the month and put/call
   *   - 2 digits for the day-of-month
   *   - 2 digits for the year
   *   - 1 character flag indicating the scale of the strike
   *   - 6 digits for the strike
   */
  val OPRA_SCHEME: String = "OPRA"

  /**
   * The scheme for OCC option codes.
   *
   * These codes have:
   *
   *   - 1 to 6 characters for the underlying root symbol
   *   - 2 digits for the year
   *   - 2 digits for the month
   *   - 2 digits for the day-of-month
   *   - 1 letter representing put/call
   *   - 8 digits for the strike multiplied by 1000
   *
   * See https://ibkr.info/node/972
   */
  val OCC_SCHEME: String = "OCC"

  //-------------------------------------------------------------------------
  /** The number of characters a Market Identifier Code is made of. */
  private val MicLength: Int = 4

  /**
   * What separates the Ticker from the MIC inside a TICMIC identifier.
   *
   * It is held as text rather than as a character so that searching a value for it selects the
   * overload of `lastIndexOf` that takes no number, and so needs no widening of a character
   * into one.
   */
  private val TicMicSeparator: String = "@"

  /**
   * The shortest text that can be a TICMIC value: one Ticker character, the separator and the
   * four characters of the MIC.
   */
  private val MinimumTicMicLength: Int = 6

  /**
   * Creates a TICMIC identifier.
   *
   * A TICMIC is an identifier that combines the Ticker, as defined by the exchange (see
   * [[TICKER_SCHEME]]), with the MIC (Market Identifier Code) that defines the exchange. The
   * value of the identifier produced is `ticker@exchangeMic`:
   *
   * {{{
   * StandardSchemes.createTicMic("ULVR", "XLON").map(_.toString)  // Right("TICMIC~ULVR@XLON")
   * }}}
   *
   * The market identifier code has to be four characters long, and the two parts together have
   * to form a value an identifier can hold - so text holding a character that
   * [[StandardId.of]] does not permit is rejected here as well. Both conditions are reported
   * at once rather than one per call, because a caller correcting its input has both causes in
   * front of it:
   *
   * {{{
   * StandardSchemes.createTicMic("ULVR", "LSE")  // Left(one failure: the MIC is not four characters)
   * }}}
   *
   * The failure over the market identifier code names the code it refused as that code stands,
   * which is the wording the method being ported produced. A code reaches this method from
   * outside the library and nothing bounds it, so bounding it and escaping what it may hold
   * belong to the writing of the failure and not to the building of one:
   * [[com.opengamma.strata.collect.result.Failure.show]] and the text form of a failure do
   * that for every part they write, here as in [[splitTicMic]]. The message is built only on
   * the failing path, `Validate.isTrue` taking it by name, so a code of the right length is
   * never quoted at all.
   *
   * @param ticker  the ticker, as defined by the exchange
   * @param exchangeMic  the MIC code of the exchange, four characters
   * @return the TICMIC identifier, or the failures describing why the parts name none
   */
  def createTicMic(ticker: String, exchangeMic: String): ResultNec[StandardId] =
    (
      Validate.isTrue(
        exchangeMic.length == MicLength,
        s"MIC must have $MicLength characters, but was $exchangeMic"),
      StandardId.of(TICMIC_SCHEME, ticker + TicMicSeparator + exchangeMic).toValidated
    ).mapN((_, identifier) => identifier).toEither

  /**
   * Splits a TICMIC identifier.
   *
   * This method extracts the Ticker and exchange MIC from the identifier, which are returned
   * as a pair in that order:
   *
   * {{{
   * StandardId.of("TICMIC", "ULVR@XLON").flatMap(StandardSchemes.splitTicMic)
   * // Right(("ULVR", "XLON"))
   * }}}
   *
   * The value has to hold at least one Ticker character, then the separator, then exactly the
   * four characters of the market identifier code. The three conditions tested below say that
   * between them, and are the ones the method being ported tested: a separator has to be
   * present, the value has to be at least six characters long, and the separator has to sit
   * five characters from the end. An identifier that does not satisfy them is reported as a
   * failure rather than interrupting the caller, since the shape of an identifier is data:
   *
   * {{{
   * StandardId.of("TICMIC", "ABC@BOB").flatMap(StandardSchemes.splitTicMic)
   * // Left(a parsing failure: the MIC is three characters, not four)
   * }}}
   *
   * The scheme of the identifier is deliberately not examined, which is the behaviour of the
   * method being ported: a value of the right shape is split whatever scheme it was filed
   * under.
   *
   * The failure quotes the identifier back exactly as [[StandardId.toString]] writes it, which
   * is the wording the method being ported produced. The value part of an identifier is text a
   * caller supplied and nothing bounds it, so writing the failure out is where it is bounded:
   * [[com.opengamma.strata.collect.result.Failure.show]] and the text form of a failure bound
   * every part they write and escape anything a line-oriented reader could act on.
   *
   * @param ticMic  the TICMIC identifier
   * @return the Ticker and the MIC, or the failure describing why the identifier is not a
   *   TICMIC
   */
  def splitTicMic(ticMic: StandardId): FailureOr[(String, String)] = {
    val value = ticMic.value
    val splitPos = value.lastIndexOf(TicMicSeparator)
    if (splitPos < 0 ||
      value.length < MinimumTicMicLength ||
      splitPos != value.length - (MicLength + 1)) {
      // the identifier is quoted as it stands, which is the wording being ported; bounding it
      // for a reader is the business of writing the failure out, as the note above sets out
      Left(Failure.Parsing(s"Invalid TICMIC identifier: $ticMic"))
    } else {
      Right((value.substring(0, splitPos), value.substring(splitPos + 1)))
    }
  }
}
