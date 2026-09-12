/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import com.opengamma.strata.collect.ArgCheck

/**
 * Constants and implementations for commonly used Ibor indices.
 *
 * Each constant returns a standard definition of the specified index.
 *
 * If a floating rate has a constant here, then it is fully supported by Strata with example
 * holiday calendar data.
 *
 * ===A view onto the family, not a second copy of it===
 *
 * Every member of this object is a named reference to an instance that [[IborIndex]] already
 * owns. The index definitions themselves - currency, calendars, fixing time and zone, the three
 * date offsets, day counts - live in the published data table that the `IborIndex` companion
 * turns into the members of its closed family. Nothing is defined here, so a constant and the
 * family can never describe the same index differently.
 *
 * The Java class being ported reached its instances through an `ExtendedEnum` registry, and said
 * in a comment that the indirection existed so the constants could be replaced by configuration
 * on the classpath. That registry is deliberately not ported: the family is closed, its members
 * are fixed at compile time, and resolution here is an ordinary lookup by name against it. There
 * is no registry, no reflection and no resource to read, so what `IborIndices.GBP_LIBOR_3M`
 * denotes is settled by this build rather than by whatever happens to be on the classpath.
 *
 * ===Only some indices have a constant===
 *
 * The family has far more members than this object has constants - a constant exists for the
 * indices that come with example holiday calendar data, which is a subset of the published
 * table. The asymmetry is inherited from the Java original and is preserved exactly: no constant
 * is added and none is dropped. An index without a constant is not missing and is reached by
 * name through `IborIndex.valueOf` or `IborIndex.parse`, or found among `IborIndex.values`.
 *
 * For the same reason this object exposes ''only'' the constants. It publishes no `values`
 * sequence and no lookup map, because the companion of the family already provides both and a
 * second surface over the same data could drift from the first.
 *
 * ===Initialisation===
 *
 * The constants are eager, as the Java static fields were. Each one resolves its name against
 * the family the first time this object is touched, so a name that the family does not carry is
 * reported then - at once, and for every constant - rather than lying dormant until some later
 * caller happens to read that one field. The cost is one lookup per constant, paid once.
 *
 * ===Deliberate divergence: deprecation is documented, not annotated===
 *
 * Seven of the constants below carry an `@deprecated` note in the Javadoc of the Java original,
 * recording the date each rate stopped being published. Those notes are reproduced verbatim as
 * documentation, but no `@deprecated` annotation is attached to the members.
 *
 * The reason is that the note describes the rate, not this API. Nothing here is superseded and
 * there is nothing for a caller to migrate to: the index still exists, still has a definition,
 * and is still the right thing to reference when valuing a trade that was struck against it. The
 * state the note reports is already carried by the domain data, where every one of those seven
 * indices is marked inactive and can be tested with `IborIndex.active`, which is a value a
 * caller can branch on rather than a warning the compiler emits. Attaching the annotation as
 * well would additionally make every mention of those seven constants a build failure, because
 * this build reports deprecation as a warning and treats warnings as errors - and would do so in
 * the tests and callers that must reference them precisely ''in order'' to assert that the
 * inactive indices are still present and correct.
 *
 * ===Thread safety===
 *
 * Every constant is an immutable value published once during class initialisation, so this
 * object is safe to use from any number of threads.
 */
object IborIndices {

  /**
   * Resolves one built-in index by the name that the published data gives it.
   *
   * A lookup here returning nothing does not mean a caller asked for something unreasonable; it
   * means this object and the data table behind [[IborIndex]] disagree about what the family
   * contains. Both are fixed at compile time and neither is reachable from outside this library,
   * so a disagreement between them is a defect in the port - a mistranscribed name string - and
   * not a data-dependent failure that any caller could anticipate, recover from or be given as a
   * value to inspect. It is therefore reported fail-fast through `ArgCheck`, which raises
   * `IllegalArgumentException` exactly as the static initialiser of the Java original did when
   * its own lookup failed.
   *
   * @param name the name the published index data gives the index, such as `GBP-LIBOR-3M`
   * @return the index of that name
   * @throws IllegalArgumentException if the family does not carry an index of that name
   */
  private def builtIn(name: String): IborIndex =
    IborIndex.valueOf(name).getOrElse {
      val message = s"Unknown built-in Ibor index: $name"
      // Raises IllegalArgumentException; ArgCheck is the one place in either module that throws.
      ArgCheck.isTrue(false, message)
      // Unreachable: the check above never returns for a false condition. The call supplies the
      // `Nothing` that the result type of this method needs, and is not a second failure path.
      sys.error(message)
    }

  //-------------------------------------------------------------------------
  /**
   * The 1 week LIBOR index for GBP.
   *
   * The "London Interbank Offered Rate".
   */
  val GBP_LIBOR_1W: IborIndex = builtIn("GBP-LIBOR-1W")
  /**
   * The 1 month LIBOR index for GBP.
   *
   * The "London Interbank Offered Rate".
   */
  val GBP_LIBOR_1M: IborIndex = builtIn("GBP-LIBOR-1M")
  /**
   * The 2 month LIBOR index for GBP.
   *
   * The "London Interbank Offered Rate".
   */
  val GBP_LIBOR_2M: IborIndex = builtIn("GBP-LIBOR-2M")
  /**
   * The 3 month LIBOR index for GBP.
   *
   * The "London Interbank Offered Rate".
   */
  val GBP_LIBOR_3M: IborIndex = builtIn("GBP-LIBOR-3M")
  /**
   * The 6 month LIBOR index for GBP.
   *
   * The "London Interbank Offered Rate".
   */
  val GBP_LIBOR_6M: IborIndex = builtIn("GBP-LIBOR-6M")
  /**
   * The 12 month LIBOR index for GBP.
   *
   * The "London Interbank Offered Rate".
   */
  val GBP_LIBOR_12M: IborIndex = builtIn("GBP-LIBOR-12M")

  //-------------------------------------------------------------------------
  /**
   * The 1 week LIBOR index for CHF.
   *
   * The "London Interbank Offered Rate".
   */
  val CHF_LIBOR_1W: IborIndex = builtIn("CHF-LIBOR-1W")
  /**
   * The 1 month LIBOR index for CHF.
   *
   * The "London Interbank Offered Rate".
   */
  val CHF_LIBOR_1M: IborIndex = builtIn("CHF-LIBOR-1M")
  /**
   * The 2 month LIBOR index for CHF.
   *
   * The "London Interbank Offered Rate".
   */
  val CHF_LIBOR_2M: IborIndex = builtIn("CHF-LIBOR-2M")
  /**
   * The 3 month LIBOR index for CHF.
   *
   * The "London Interbank Offered Rate".
   */
  val CHF_LIBOR_3M: IborIndex = builtIn("CHF-LIBOR-3M")
  /**
   * The 6 month LIBOR index for CHF.
   *
   * The "London Interbank Offered Rate".
   */
  val CHF_LIBOR_6M: IborIndex = builtIn("CHF-LIBOR-6M")
  /**
   * The 12 month LIBOR index for CHF.
   *
   * The "London Interbank Offered Rate".
   */
  val CHF_LIBOR_12M: IborIndex = builtIn("CHF-LIBOR-12M")

  //-------------------------------------------------------------------------
  /**
   * The 1 week LIBOR index for EUR.
   *
   * The "London Interbank Offered Rate".
   */
  val EUR_LIBOR_1W: IborIndex = builtIn("EUR-LIBOR-1W")
  /**
   * The 1 month LIBOR index for EUR.
   *
   * The "London Interbank Offered Rate".
   */
  val EUR_LIBOR_1M: IborIndex = builtIn("EUR-LIBOR-1M")
  /**
   * The 2 month LIBOR index for EUR.
   *
   * The "London Interbank Offered Rate".
   */
  val EUR_LIBOR_2M: IborIndex = builtIn("EUR-LIBOR-2M")
  /**
   * The 3 month LIBOR index for EUR.
   *
   * The "London Interbank Offered Rate".
   */
  val EUR_LIBOR_3M: IborIndex = builtIn("EUR-LIBOR-3M")
  /**
   * The 6 month LIBOR index for EUR.
   *
   * The "London Interbank Offered Rate".
   */
  val EUR_LIBOR_6M: IborIndex = builtIn("EUR-LIBOR-6M")
  /**
   * The 12 month LIBOR index for EUR.
   *
   * The "London Interbank Offered Rate".
   */
  val EUR_LIBOR_12M: IborIndex = builtIn("EUR-LIBOR-12M")

  //-------------------------------------------------------------------------
  /**
   * The 1 week LIBOR index for JPY.
   *
   * The "London Interbank Offered Rate".
   */
  val JPY_LIBOR_1W: IborIndex = builtIn("JPY-LIBOR-1W")
  /**
   * The 1 month LIBOR index for JPY.
   *
   * The "London Interbank Offered Rate".
   */
  val JPY_LIBOR_1M: IborIndex = builtIn("JPY-LIBOR-1M")
  /**
   * The 2 month LIBOR index for JPY.
   *
   * The "London Interbank Offered Rate".
   */
  val JPY_LIBOR_2M: IborIndex = builtIn("JPY-LIBOR-2M")
  /**
   * The 3 month LIBOR index for JPY.
   *
   * The "London Interbank Offered Rate".
   */
  val JPY_LIBOR_3M: IborIndex = builtIn("JPY-LIBOR-3M")
  /**
   * The 6 month LIBOR index for JPY.
   *
   * The "London Interbank Offered Rate".
   */
  val JPY_LIBOR_6M: IborIndex = builtIn("JPY-LIBOR-6M")
  /**
   * The 12 month LIBOR index for JPY.
   *
   * The "London Interbank Offered Rate".
   */
  val JPY_LIBOR_12M: IborIndex = builtIn("JPY-LIBOR-12M")

  //-------------------------------------------------------------------------
  /**
   * The 1 week LIBOR index for USD.
   *
   * The "London Interbank Offered Rate".
   */
  val USD_LIBOR_1W: IborIndex = builtIn("USD-LIBOR-1W")
  /**
   * The 1 month LIBOR index for USD.
   *
   * The "London Interbank Offered Rate".
   */
  val USD_LIBOR_1M: IborIndex = builtIn("USD-LIBOR-1M")
  /**
   * The 2 month LIBOR index for USD.
   *
   * The "London Interbank Offered Rate".
   */
  val USD_LIBOR_2M: IborIndex = builtIn("USD-LIBOR-2M")
  /**
   * The 3 month LIBOR index for USD.
   *
   * The "London Interbank Offered Rate".
   */
  val USD_LIBOR_3M: IborIndex = builtIn("USD-LIBOR-3M")
  /**
   * The 6 month LIBOR index for USD.
   *
   * The "London Interbank Offered Rate".
   */
  val USD_LIBOR_6M: IborIndex = builtIn("USD-LIBOR-6M")
  /**
   * The 12 month LIBOR index for USD.
   *
   * The "London Interbank Offered Rate".
   */
  val USD_LIBOR_12M: IborIndex = builtIn("USD-LIBOR-12M")

  //-------------------------------------------------------------------------
  /**
   * The 1 week EURIBOR index.
   *
   * The "Euro Interbank Offered Rate".
   */
  val EUR_EURIBOR_1W: IborIndex = builtIn("EUR-EURIBOR-1W")
  /**
   * The 2 week EURIBOR index.
   *
   * The "Euro Interbank Offered Rate".
   *
   * @deprecated Not published as of 2018-12-03
   */
  val EUR_EURIBOR_2W: IborIndex = builtIn("EUR-EURIBOR-2W")
  /**
   * The 1 month EURIBOR index.
   *
   * The "Euro Interbank Offered Rate".
   */
  val EUR_EURIBOR_1M: IborIndex = builtIn("EUR-EURIBOR-1M")
  /**
   * The 2 month EURIBOR index.
   *
   * The "Euro Interbank Offered Rate".
   *
   * @deprecated Not published as of 2018-12-03
   */
  val EUR_EURIBOR_2M: IborIndex = builtIn("EUR-EURIBOR-2M")
  /**
   * The 3 month EURIBOR index.
   *
   * The "Euro Interbank Offered Rate".
   */
  val EUR_EURIBOR_3M: IborIndex = builtIn("EUR-EURIBOR-3M")
  /**
   * The 6 month EURIBOR index.
   *
   * The "Euro Interbank Offered Rate".
   */
  val EUR_EURIBOR_6M: IborIndex = builtIn("EUR-EURIBOR-6M")
  /**
   * The 9 month EURIBOR index.
   *
   * The "Euro Interbank Offered Rate".
   *
   * @deprecated Not published as of 2018-12-03
   */
  val EUR_EURIBOR_9M: IborIndex = builtIn("EUR-EURIBOR-9M")
  /**
   * The 12 month EURIBOR index.
   *
   * The "Euro Interbank Offered Rate".
   */
  val EUR_EURIBOR_12M: IborIndex = builtIn("EUR-EURIBOR-12M")

  //-------------------------------------------------------------------------
  /**
   * The 1 week TIBOR (Japan) index.
   *
   * The "Tokyo Interbank Offered Rate", unsecured call market.
   */
  val JPY_TIBOR_JAPAN_1W: IborIndex = builtIn("JPY-TIBOR-JAPAN-1W")
  /**
   * The 1 month TIBOR (Japan) index.
   *
   * The "Tokyo Interbank Offered Rate", unsecured call market.
   */
  val JPY_TIBOR_JAPAN_1M: IborIndex = builtIn("JPY-TIBOR-JAPAN-1M")
  /**
   * The 2 month TIBOR (Japan) index.
   *
   * The "Tokyo Interbank Offered Rate", unsecured call market.
   *
   * @deprecated Not published as of 2019-04-01
   */
  val JPY_TIBOR_JAPAN_2M: IborIndex = builtIn("JPY-TIBOR-JAPAN-2M")
  /**
   * The 3 month TIBOR (Japan) index.
   *
   * The "Tokyo Interbank Offered Rate", unsecured call market.
   */
  val JPY_TIBOR_JAPAN_3M: IborIndex = builtIn("JPY-TIBOR-JAPAN-3M")
  /**
   * The 6 month TIBOR (Japan) index.
   *
   * The "Tokyo Interbank Offered Rate", unsecured call market.
   */
  val JPY_TIBOR_JAPAN_6M: IborIndex = builtIn("JPY-TIBOR-JAPAN-6M")
  /**
   * The 12 month TIBOR (Japan) index.
   *
   * The "Tokyo Interbank Offered Rate", unsecured call market.
   */
  val JPY_TIBOR_JAPAN_12M: IborIndex = builtIn("JPY-TIBOR-JAPAN-12M")

  //-------------------------------------------------------------------------
  /**
   * The 1 week TIBOR (Euroyen) index.
   *
   * The "Tokyo Interbank Offered Rate", Japan offshore market.
   */
  val JPY_TIBOR_EUROYEN_1W: IborIndex = builtIn("JPY-TIBOR-EUROYEN-1W")
  /**
   * The 1 month TIBOR (Euroyen) index.
   *
   * The "Tokyo Interbank Offered Rate", Japan offshore market.
   */
  val JPY_TIBOR_EUROYEN_1M: IborIndex = builtIn("JPY-TIBOR-EUROYEN-1M")
  /**
   * The 2 month TIBOR (Euroyen) index.
   *
   * The "Tokyo Interbank Offered Rate", Japan offshore market.
   *
   * @deprecated Not published as of 2019-04-01
   */
  val JPY_TIBOR_EUROYEN_2M: IborIndex = builtIn("JPY-TIBOR-EUROYEN-2M")
  /**
   * The 3 month TIBOR (Euroyen) index.
   *
   * The "Tokyo Interbank Offered Rate", Japan offshore market.
   */
  val JPY_TIBOR_EUROYEN_3M: IborIndex = builtIn("JPY-TIBOR-EUROYEN-3M")
  /**
   * The 6 month TIBOR (Euroyen) index.
   *
   * The "Tokyo Interbank Offered Rate", Japan offshore market.
   */
  val JPY_TIBOR_EUROYEN_6M: IborIndex = builtIn("JPY-TIBOR-EUROYEN-6M")
  /**
   * The 12 month TIBOR (Euroyen) index.
   *
   * The "Tokyo Interbank Offered Rate", Japan offshore market.
   */
  val JPY_TIBOR_EUROYEN_12M: IborIndex = builtIn("JPY-TIBOR-EUROYEN-12M")

  //-------------------------------------------------------------------------
  /**
   * The 1 month BBSW index.
   *
   * The AFMA Australian Bank Bill Short Term Rate.
   */
  val AUD_BBSW_1M: IborIndex = builtIn("AUD-BBSW-1M")
  /**
   * The 2 month BBSW index.
   *
   * The AFMA Australian Bank Bill Short Term Rate.
   */
  val AUD_BBSW_2M: IborIndex = builtIn("AUD-BBSW-2M")
  /**
   * The 3 month BBSW index.
   *
   * The AFMA Australian Bank Bill Short Term Rate.
   */
  val AUD_BBSW_3M: IborIndex = builtIn("AUD-BBSW-3M")
  /**
   * The 4 month BBSW index.
   *
   * The AFMA Australian Bank Bill Short Term Rate.
   */
  val AUD_BBSW_4M: IborIndex = builtIn("AUD-BBSW-4M")
  /**
   * The 5 month BBSW index.
   *
   * The AFMA Australian Bank Bill Short Term Rate.
   */
  val AUD_BBSW_5M: IborIndex = builtIn("AUD-BBSW-5M")
  /**
   * The 6 month BBSW index.
   *
   * The AFMA Australian Bank Bill Short Term Rate.
   */
  val AUD_BBSW_6M: IborIndex = builtIn("AUD-BBSW-6M")

  //-------------------------------------------------------------------------
  /**
   * The 1 month CDOR index.
   *
   * The "Canadian Dollar Offered Rate".
   */
  val CAD_CDOR_1M: IborIndex = builtIn("CAD-CDOR-1M")
  /**
   * The 2 month CDOR index.
   *
   * The "Canadian Dollar Offered Rate".
   */
  val CAD_CDOR_2M: IborIndex = builtIn("CAD-CDOR-2M")
  /**
   * The 3 month CDOR index.
   *
   * The "Canadian Dollar Offered Rate".
   */
  val CAD_CDOR_3M: IborIndex = builtIn("CAD-CDOR-3M")
  /**
   * The 6 month CDOR index.
   *
   * The "Canadian Dollar Offered Rate".
   *
   * @deprecated Not published as of 2021-05-17
   */
  val CAD_CDOR_6M: IborIndex = builtIn("CAD-CDOR-6M")
  /**
   * The 12 month CDOR index.
   *
   * The "Canadian Dollar Offered Rate".
   *
   * @deprecated Not published as of 2021-05-17
   */
  val CAD_CDOR_12M: IborIndex = builtIn("CAD-CDOR-12M")

  //-------------------------------------------------------------------------
  /**
   * The 1 week PRIBOR index.
   *
   * The "Prague Interbank Offered Rate".
   */
  val CZK_PRIBOR_1W: IborIndex = builtIn("CZK-PRIBOR-1W")
  /**
   * The 2 week PRIBOR index.
   *
   * The "Prague Interbank Offered Rate".
   */
  val CZK_PRIBOR_2W: IborIndex = builtIn("CZK-PRIBOR-2W")
  /**
   * The 1 month PRIBOR index.
   *
   * The "Prague Interbank Offered Rate".
   */
  val CZK_PRIBOR_1M: IborIndex = builtIn("CZK-PRIBOR-1M")
  /**
   * The 2 month PRIBOR index.
   *
   * The "Prague Interbank Offered Rate".
   */
  val CZK_PRIBOR_2M: IborIndex = builtIn("CZK-PRIBOR-2M")
  /**
   * The 3 month PRIBOR index.
   *
   * The "Prague Interbank Offered Rate".
   */
  val CZK_PRIBOR_3M: IborIndex = builtIn("CZK-PRIBOR-3M")
  /**
   * The 6 month PRIBOR index.
   *
   * The "Prague Interbank Offered Rate".
   */
  val CZK_PRIBOR_6M: IborIndex = builtIn("CZK-PRIBOR-6M")
  /**
   * The 9 month PRIBOR index.
   *
   * The "Prague Interbank Offered Rate".
   */
  val CZK_PRIBOR_9M: IborIndex = builtIn("CZK-PRIBOR-9M")
  /**
   * The 12 month PRIBOR index.
   *
   * The "Prague Interbank Offered Rate".
   */
  val CZK_PRIBOR_12M: IborIndex = builtIn("CZK-PRIBOR-12M")

  //-------------------------------------------------------------------------
  /**
   * The 1 week CIBOR index.
   *
   * The "Copenhagen Interbank Offered Rate".
   */
  val DKK_CIBOR_1W: IborIndex = builtIn("DKK-CIBOR-1W")
  /**
   * The 2 week CIBOR index.
   *
   * The "Copenhagen Interbank Offered Rate".
   */
  val DKK_CIBOR_2W: IborIndex = builtIn("DKK-CIBOR-2W")
  /**
   * The 1 month CIBOR index.
   *
   * The "Copenhagen Interbank Offered Rate".
   */
  val DKK_CIBOR_1M: IborIndex = builtIn("DKK-CIBOR-1M")
  /**
   * The 2 month CIBOR index.
   *
   * The "Copenhagen Interbank Offered Rate".
   */
  val DKK_CIBOR_2M: IborIndex = builtIn("DKK-CIBOR-2M")
  /**
   * The 3 month CIBOR index.
   *
   * The "Copenhagen Interbank Offered Rate".
   */
  val DKK_CIBOR_3M: IborIndex = builtIn("DKK-CIBOR-3M")
  /**
   * The 6 month CIBOR index.
   *
   * The "Copenhagen Interbank Offered Rate".
   */
  val DKK_CIBOR_6M: IborIndex = builtIn("DKK-CIBOR-6M")
  /**
   * The 9 month CIBOR index.
   *
   * The "Copenhagen Interbank Offered Rate".
   */
  val DKK_CIBOR_9M: IborIndex = builtIn("DKK-CIBOR-9M")
  /**
   * The 12 month CIBOR index.
   *
   * The "Copenhagen Interbank Offered Rate".
   */
  val DKK_CIBOR_12M: IborIndex = builtIn("DKK-CIBOR-12M")

  //-------------------------------------------------------------------------
  /**
   * The 1 week BUBOR index.
   *
   * The "Budapest Interbank Offered Rate".
   */
  val HUF_BUBOR_1W: IborIndex = builtIn("HUF-BUBOR-1W")
  /**
   * The 2 week BUBOR index.
   *
   * The "Budapest Interbank Offered Rate".
   */
  val HUF_BUBOR_2W: IborIndex = builtIn("HUF-BUBOR-2W")
  /**
   * The 1 month BUBOR index.
   *
   * The "Budapest Interbank Offered Rate".
   */
  val HUF_BUBOR_1M: IborIndex = builtIn("HUF-BUBOR-1M")
  /**
   * The 2 month BUBOR index.
   *
   * The "Budapest Interbank Offered Rate".
   */
  val HUF_BUBOR_2M: IborIndex = builtIn("HUF-BUBOR-2M")
  /**
   * The 3 month BUBOR index.
   *
   * The "Budapest Interbank Offered Rate".
   */
  val HUF_BUBOR_3M: IborIndex = builtIn("HUF-BUBOR-3M")
  /**
   * The 6 month BUBOR index.
   *
   * The "Budapest Interbank Offered Rate".
   */
  val HUF_BUBOR_6M: IborIndex = builtIn("HUF-BUBOR-6M")
  /**
   * The 9 month BUBOR index.
   *
   * The "Budapest Interbank Offered Rate".
   */
  val HUF_BUBOR_9M: IborIndex = builtIn("HUF-BUBOR-9M")
  /**
   * The 12 month BUBOR index.
   *
   * The "Budapest Interbank Offered Rate".
   */
  val HUF_BUBOR_12M: IborIndex = builtIn("HUF-BUBOR-12M")

  //-------------------------------------------------------------------------
  /**
   * The 4 week TIIE index.
   *
   * The "Interbank Equilibrium Interest Rate".
   */
  val MXN_TIIE_4W: IborIndex = builtIn("MXN-TIIE-4W")
  /**
   * The 13 week TIIE index.
   *
   * The "Interbank Equilibrium Interest Rate".
   */
  val MXN_TIIE_13W: IborIndex = builtIn("MXN-TIIE-13W")
  /**
   * The 26 week TIIE index.
   *
   * The "Interbank Equilibrium Interest Rate".
   */
  val MXN_TIIE_26W: IborIndex = builtIn("MXN-TIIE-26W")

  //-------------------------------------------------------------------------
  /**
   * The 1 week NIBOR index.
   *
   * The "Norwegian Interbank Offered Rate".
   */
  val NOK_NIBOR_1W: IborIndex = builtIn("NOK-NIBOR-1W")
  /**
   * The 1 month NIBOR index.
   *
   * The "Norwegian Interbank Offered Rate".
   */
  val NOK_NIBOR_1M: IborIndex = builtIn("NOK-NIBOR-1M")
  /**
   * The 2 month NIBOR index.
   *
   * The "Norwegian Interbank Offered Rate".
   */
  val NOK_NIBOR_2M: IborIndex = builtIn("NOK-NIBOR-2M")
  /**
   * The 3 month NIBOR index.
   *
   * The "Norwegian Interbank Offered Rate".
   */
  val NOK_NIBOR_3M: IborIndex = builtIn("NOK-NIBOR-3M")
  /**
   * The 6 month NIBOR index.
   *
   * The "Norwegian Interbank Offered Rate".
   */
  val NOK_NIBOR_6M: IborIndex = builtIn("NOK-NIBOR-6M")

  //-------------------------------------------------------------------------
  /**
   * The 1 month BKBM index.
   *
   * The "New Zealand Bank Bill Benchmark Rate".
   */
  val NZD_BKBM_1M: IborIndex = builtIn("NZD-BKBM-1M")
  /**
   * The 2 month BKBM index.
   *
   * The "New Zealand Bank Bill Benchmark Rate".
   */
  val NZD_BKBM_2M: IborIndex = builtIn("NZD-BKBM-2M")
  /**
   * The 3 month BKBM index.
   *
   * The "New Zealand Bank Bill Benchmark Rate".
   */
  val NZD_BKBM_3M: IborIndex = builtIn("NZD-BKBM-3M")
  /**
   * The 4 month BKBM index.
   *
   * The "New Zealand Bank Bill Benchmark Rate".
   */
  val NZD_BKBM_4M: IborIndex = builtIn("NZD-BKBM-4M")
  /**
   * The 5 month BKBM index.
   *
   * The "New Zealand Bank Bill Benchmark Rate".
   */
  val NZD_BKBM_5M: IborIndex = builtIn("NZD-BKBM-5M")
  /**
   * The 6 month BKBM index.
   *
   * The "New Zealand Bank Bill Benchmark Rate".
   */
  val NZD_BKBM_6M: IborIndex = builtIn("NZD-BKBM-6M")

  //-------------------------------------------------------------------------
  /**
   * The 1 week WIBOR index.
   *
   * The "Polish Interbank Offered Rate".
   */
  val PLN_WIBOR_1W: IborIndex = builtIn("PLN-WIBOR-1W")
  /**
   * The 1 month WIBOR index.
   *
   * The "Polish Interbank Offered Rate".
   */
  val PLN_WIBOR_1M: IborIndex = builtIn("PLN-WIBOR-1M")
  /**
   * The 3 month WIBOR index.
   *
   * The "Polish Interbank Offered Rate".
   */
  val PLN_WIBOR_3M: IborIndex = builtIn("PLN-WIBOR-3M")
  /**
   * The 6 month WIBOR index.
   *
   * The "Polish Interbank Offered Rate".
   */
  val PLN_WIBOR_6M: IborIndex = builtIn("PLN-WIBOR-6M")
  /**
   * The 12 month WIBOR index.
   *
   * The "Polish Interbank Offered Rate".
   */
  val PLN_WIBOR_12M: IborIndex = builtIn("PLN-WIBOR-12M")

  //-------------------------------------------------------------------------
  /**
   * The 1 week STIBOR index.
   *
   * The "Swedish Interbank Offered Rate".
   */
  val SEK_STIBOR_1W: IborIndex = builtIn("SEK-STIBOR-1W")
  /**
   * The 1 month STIBOR index.
   *
   * The "Swedish Interbank Offered Rate".
   */
  val SEK_STIBOR_1M: IborIndex = builtIn("SEK-STIBOR-1M")
  /**
   * The 2 month STIBOR index.
   *
   * The "Swedish Interbank Offered Rate".
   */
  val SEK_STIBOR_2M: IborIndex = builtIn("SEK-STIBOR-2M")
  /**
   * The 3 month STIBOR index.
   *
   * The "Swedish Interbank Offered Rate".
   */
  val SEK_STIBOR_3M: IborIndex = builtIn("SEK-STIBOR-3M")
  /**
   * The 6 month STIBOR index.
   *
   * The "Swedish Interbank Offered Rate".
   */
  val SEK_STIBOR_6M: IborIndex = builtIn("SEK-STIBOR-6M")

  //-------------------------------------------------------------------------
  /**
   * The 1 month JIBAR index.
   *
   * The "Johannnesburg Interbank Average Rate".
   */
  val ZAR_JIBAR_1M: IborIndex = builtIn("ZAR-JIBAR-1M")
  /**
   * The 3 month JIBAR index.
   *
   * The "Johannnesburg Interbank Average Rate".
   */
  val ZAR_JIBAR_3M: IborIndex = builtIn("ZAR-JIBAR-3M")
  /**
   * The 6 month JIBAR index.
   *
   * The "Johannnesburg Interbank Average Rate".
   */
  val ZAR_JIBAR_6M: IborIndex = builtIn("ZAR-JIBAR-6M")
  /**
   * The 12 month JIBAR index.
   *
   * The "Johannnesburg Interbank Average Rate".
   */
  val ZAR_JIBAR_12M: IborIndex = builtIn("ZAR-JIBAR-12M")
}
