/*
 * Copyright (C) 2017 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

import cats.Show

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2

import com.opengamma.strata.basics.date.Tenor
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[FloatingRateIndex]].
 *
 * Every method of the Java original is kept, under its own name, so the method-level
 * traceability of the migration stays one-to-one: the ten rows this class contributes to
 * `manifest/java-test-mapping.csv` are all `ported` to a test of this suite named after the
 * Java method, and the acceptance gate joins them on that name. The original's four
 * parameterised methods were driven from a single provider, `data_name`; that shape is
 * preserved - the provider becomes one shared table, declared once below, and each of the four
 * methods keeps its own test driven from it. Nothing is added: the Java class has no `coverage`
 * and no `test_serialization` method, so this suite has neither either, and an invented test
 * would be a testcase no row of the manifest names.
 *
 * ===What this spec is about===
 *
 * [[FloatingRateIndex]] is the union of three of the four published index families - Ibor,
 * Overnight, then Price - searched in that fixed order, which is the order of the `[types]`
 * declaration the combined lookup being ported consumed. The exclusion is as much of the
 * contract as the inclusion: the exchange-rate family is deliberately absent, because the
 * figure of an FX index is a rate of exchange rather than a rate of interest. So `EUR/USD-ECB`
 * names an [[Index]] and names no floating rate index, and `test_of_lookup_notFound` is that
 * boundary. It is also what distinguishes this spec from its two siblings, which must not
 * borrow each other's exclusion case: `RateIndex` excludes a price index, `Index` excludes
 * nothing but text naming no member at all.
 *
 * ===The two kinds of entry point, and which test uses which===
 *
 * The Java interface had two distinct resolutions and this spec keeps them apart, because the
 * Java methods did:
 *
 *  - `of(String)` was the ''exact'' union lookup - it resolved a full index name and raised for
 *    anything else. The port splits it into [[FloatingRateIndex.valueOf]], which answers with an
 *    absent value, and [[FloatingRateIndex.parse]], which reports the failure; both are asserted
 *    everywhere the Java method was used, so the two entry points cannot drift apart. These are
 *    what `test_of_lookup`, `test_of_convert`, `test_of_lookup_notFound` and
 *    `test_of_lookup_null` exercise, and all thirteen rows of the shared table carry a full
 *    tenor-bearing name, so no tenor defaulting takes part in them.
 *  - `parse(String[, Tenor])` and `tryParse(String[, Tenor])` were the ''wider'' resolution:
 *    they searched the three index families and then the floating rate ''families'', and
 *    converted a family into one of its indices, which needs a tenor because a family says
 *    nothing about the period a rate covers. The port states that search rule once, in
 *    [[FloatingRateIndex.parseWith]] and [[FloatingRateIndex.tryParseWith]], and takes the
 *    conversion as an argument, so the tenor-defaulting policy stays the property of the family
 *    that owns it - [[FloatingRateName.toFloatingRateIndex]], which uses the family's own
 *    default tenor, and `toFloatingRateIndex(tenor)`, which uses a supplied one. The four Java
 *    arities are therefore the four combinations of channel and tenor, and each is named by one
 *    of the four helpers below so that the tests read as the Java tests read.
 *
 * The tenor rule is easy to state backwards, so both `withTenor` tests keep the contrast that
 * pins it: a supplied default is used '''only''' where the text carries no tenor of its own.
 * `("GBP-LIBOR", 6M)` resolves to the six-month index because `GBP-LIBOR` is a bare family
 * name, while `("GBP-LIBOR-1M", 6M)` resolves to the ''one''-month index because the text names
 * an index outright and the family conversion is never reached. The no-tenor form has no
 * supplied default and falls back on the family's own, which is `3M` for the sterling Libor
 * family, which is why `GBP-LIBOR` alone resolves to `GBP-LIBOR-3M`.
 *
 * ===Two rulings, each recorded where it is applied===
 *
 *  - `test_of_convert` asserted the guarantee Joda-Convert's `@ToString`/`@FromString` pair
 *    gave. Joda is not on this classpath, so the guarantee itself is asserted: rendering a
 *    value produces its name, resolving that name produces the value back, and the two are
 *    inverse. It is deliberately not a JSON codec test - the union is a trait and is excluded
 *    from JSON, and the families that do have codecs assert their own.
 *  - `test_of_lookup_null` and the absent-name assertions of the two `parse` tests asserted
 *    that the absent reference was rejected. This port writes no such reference and resolves a
 *    name as a value, so the case is asserted as the two spellings of an absent name a caller
 *    can actually supply.
 *
 * Failures are compared by reason rather than by message: the reason and the text that could
 * not be resolved are the contract, while the diagnostic prose around them is free to change.
 */
class FloatingRateIndexSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The wider resolution with no tenor supplied, which is Java's `parse(String)`.
   *
   * The conversion handed to the search is the family's own, so a family reached by this route
   * contributes its default tenor - `3M` for an Ibor family that publishes an active
   * three-month index. A failure of the conversion is reported the same way as text that named
   * nothing, exactly as the ported method raised for both.
   *
   * @param indexStr  the text to resolve, such as `GBP-LIBOR-3M` or `GBP-LIBOR`
   * @return the index the text names, or the failure describing text that named none
   */
  private def parseDefaultingTenor(indexStr: String): Either[Failure, FloatingRateIndex] =
    FloatingRateIndex.parseWith(indexStr, family => family.toFloatingRateIndex.toOption)

  /**
   * The wider resolution with a default Ibor tenor supplied, which is Java's
   * `parse(String, Tenor)`.
   *
   * @param indexStr  the text to resolve, such as `GBP-LIBOR-3M` or `GBP-LIBOR`
   * @param defaultIborTenor  the tenor to use where the text names an Ibor family rather than
   *   an index
   * @return the index the text names, or the failure describing text that named none
   */
  private def parseDefaultingTenor(
      indexStr: String,
      defaultIborTenor: Tenor): Either[Failure, FloatingRateIndex] =
    FloatingRateIndex.parseWith(indexStr, family => family.toFloatingRateIndex(defaultIborTenor).toOption)

  /**
   * The wider resolution with no tenor supplied, answering with an absent value, which is
   * Java's `tryParse(String)`.
   *
   * This is the other channel of the same rule and is kept distinct from the `parse` helpers
   * throughout: text naming nothing is an absent value here and a reported failure there, and
   * an assertion of one must never be routed through the matchers of the other.
   *
   * @param indexStr  the text to resolve, such as `GBP-LIBOR-3M` or `GBP-LIBOR`
   * @return the index the text names, or nothing where it names none
   */
  private def tryParseDefaultingTenor(indexStr: String): Option[FloatingRateIndex] =
    FloatingRateIndex.tryParseWith(indexStr, family => family.toFloatingRateIndex.toOption)

  /**
   * The wider resolution with a default Ibor tenor supplied, answering with an absent value,
   * which is Java's `tryParse(String, Tenor)`.
   *
   * @param indexStr  the text to resolve, such as `GBP-LIBOR-3M` or `GBP-LIBOR`
   * @param defaultIborTenor  the tenor to use where the text names an Ibor family rather than
   *   an index
   * @return the index the text names, or nothing where it names none
   */
  private def tryParseDefaultingTenor(
      indexStr: String,
      defaultIborTenor: Tenor): Option[FloatingRateIndex] =
    FloatingRateIndex.tryParseWith(indexStr, family => family.toFloatingRateIndex(defaultIborTenor).toOption)

  /**
   * Renders a member of the union as text, through the `Show` instance of the family that
   * published it.
   *
   * This is what `test_of_convert` renders with, and it is written as a match over the union
   * rather than through one instance because the union is a trait and publishes none: each of
   * the three families publishes its own, by name. Two things follow, and both are wanted. The
   * rendering asserted is the one a caller actually gets, family by family; and the match is
   * required to be exhaustive by the compiler, which makes this method a compile-time witness
   * that the membership of the union is exactly the Ibor, Overnight and Price families - the
   * exchange-rate family is not a case here because it is not a member, and a fourth member
   * appearing would stop this file compiling rather than slipping through untested.
   *
   * @param index  the index to render
   * @return the text the index renders as, which is its unique name
   */
  private def renderThroughUnion(index: FloatingRateIndex): String =
    index match {
      case iborIndex: IborIndex => Show[IborIndex].show(iborIndex)
      case overnightIndex: OvernightIndex => Show[OvernightIndex].show(overnightIndex)
      case priceIndex: PriceIndex => Show[PriceIndex].show(priceIndex)
    }

  /**
   * The shared provider, transcribed row for row from the Java data provider.
   *
   * Each row pairs an index with the name it renders as and is looked up by. The rows are in
   * the order and the grouping of the Java provider - five Ibor indices, five Overnight
   * indices, three Price indices - and the first column is typed as the union rather than as
   * the three leaf types, so every test driven from this table exercises the union type and not
   * the families underneath it.
   */
  private val dataName: TableFor2[FloatingRateIndex, String] = Table(
    ("index", "name"),
    (IborIndices.GBP_LIBOR_6M, "GBP-LIBOR-6M"),
    (IborIndices.CHF_LIBOR_6M, "CHF-LIBOR-6M"),
    (IborIndices.EUR_LIBOR_6M, "EUR-LIBOR-6M"),
    (IborIndices.JPY_LIBOR_6M, "JPY-LIBOR-6M"),
    (IborIndices.USD_LIBOR_6M, "USD-LIBOR-6M"),
    (OvernightIndices.GBP_SONIA, "GBP-SONIA"),
    (OvernightIndices.CHF_SARON, "CHF-SARON"),
    (OvernightIndices.EUR_EONIA, "EUR-EONIA"),
    (OvernightIndices.JPY_TONAR, "JPY-TONAR"),
    (OvernightIndices.USD_FED_FUND, "USD-FED-FUND"),
    (PriceIndices.GB_HICP, "GB-HICP"),
    (PriceIndices.CH_CPI, "CH-CPI"),
    (PriceIndices.EU_AI_CPI, "EU-AI-CPI")
  )

  //-------------------------------------------------------------------------
  test("test_parse_noTenor") {
    // A bare family name carries no tenor, and no tenor is supplied by this form, so the
    // family's own default tenor decides which member is meant. The sterling Libor family
    // defaults to 3M, which is why `GBP-LIBOR` alone resolves to the three-month index.
    parseDefaultingTenor("GBP-LIBOR") should haveValue(IborIndices.GBP_LIBOR_3M)

    // Text that names an index outright is resolved by the index families, so the family
    // conversion is never reached and the tenor of the text is the tenor of the answer.
    parseDefaultingTenor("GBP-LIBOR-1M") should haveValue(IborIndices.GBP_LIBOR_1M)
    parseDefaultingTenor("GBP-LIBOR-3M") should haveValue(IborIndices.GBP_LIBOR_3M)

    // Neither of the other two families has a tenor to choose, so both resolve directly.
    parseDefaultingTenor("GBP-SONIA") should haveValue(OvernightIndices.GBP_SONIA)
    parseDefaultingTenor("GB-RPI") should haveValue(PriceIndices.GB_RPI)

    // Reinterpretation of the Java assertion that the absent reference was rejected: this port
    // writes no such reference, so the case is asserted as the absent name a caller can supply.
    parseDefaultingTenor("") should beFailureWith(FailureReason.PARSING)
    parseDefaultingTenor("   ") should beFailureWith(FailureReason.PARSING)

    // Text naming neither an index nor a family is reported rather than raised.
    parseDefaultingTenor("NotAnIndex") should beFailureWith(FailureReason.PARSING)
  }

  test("test_parse_withTenor") {
    // The supplied default is used because the text carries no tenor of its own.
    parseDefaultingTenor("GBP-LIBOR", Tenor.TENOR_6M) should haveValue(IborIndices.GBP_LIBOR_6M)

    // The tenor of the text wins and the supplied default is ignored: the contrast between
    // this row and the one above it is the whole semantics of the tenor argument, so both
    // rows stay, as they did in the Java method.
    parseDefaultingTenor("GBP-LIBOR-1M", Tenor.TENOR_6M) should haveValue(IborIndices.GBP_LIBOR_1M)
    parseDefaultingTenor("GBP-LIBOR-3M", Tenor.TENOR_6M) should haveValue(IborIndices.GBP_LIBOR_3M)

    // An Overnight and a Price name have no tenor to choose, so the supplied one is irrelevant.
    parseDefaultingTenor("GBP-SONIA", Tenor.TENOR_6M) should haveValue(OvernightIndices.GBP_SONIA)
    parseDefaultingTenor("GB-RPI", Tenor.TENOR_6M) should haveValue(PriceIndices.GB_RPI)

    // Reinterpretation of the Java absent-reference assertion, as in `test_parse_noTenor`.
    parseDefaultingTenor("", Tenor.TENOR_6M) should beFailureWith(FailureReason.PARSING)
    parseDefaultingTenor("   ", Tenor.TENOR_6M) should beFailureWith(FailureReason.PARSING)

    // A tenor cannot rescue text that names nothing.
    parseDefaultingTenor("NotAnIndex", Tenor.TENOR_6M) should beFailureWith(FailureReason.PARSING)
  }

  test("test_tryParse_noTenor") {
    // The same five inputs as `test_parse_noTenor` through the other channel: this resolution
    // answers with an absent value rather than a reported failure, which is the distinction the
    // Java pair of methods drew with `Optional` and is asserted here with `Option`.
    tryParseDefaultingTenor("GBP-LIBOR") shouldBe Some(IborIndices.GBP_LIBOR_3M)
    tryParseDefaultingTenor("GBP-LIBOR-1M") shouldBe Some(IborIndices.GBP_LIBOR_1M)
    tryParseDefaultingTenor("GBP-LIBOR-3M") shouldBe Some(IborIndices.GBP_LIBOR_3M)
    tryParseDefaultingTenor("GBP-SONIA") shouldBe Some(OvernightIndices.GBP_SONIA)
    tryParseDefaultingTenor("GB-RPI") shouldBe Some(PriceIndices.GB_RPI)

    // Reinterpretation of the Java row that passed the absent reference and expected an empty
    // answer: the absent name a caller can supply, answered with nothing.
    tryParseDefaultingTenor("") shouldBe None
    tryParseDefaultingTenor("   ") shouldBe None

    tryParseDefaultingTenor("NotAnIndex") shouldBe None
  }

  test("test_tryParse_withTenor") {
    // The expectations of `test_parse_withTenor`, in the absent-value channel. The contrast
    // that pins the tenor rule is kept here too: the supplied default applies to the bare
    // family name and is ignored by the name that carries its own tenor.
    tryParseDefaultingTenor("GBP-LIBOR", Tenor.TENOR_6M) shouldBe Some(IborIndices.GBP_LIBOR_6M)
    tryParseDefaultingTenor("GBP-LIBOR-1M", Tenor.TENOR_6M) shouldBe Some(IborIndices.GBP_LIBOR_1M)
    tryParseDefaultingTenor("GBP-LIBOR-3M", Tenor.TENOR_6M) shouldBe Some(IborIndices.GBP_LIBOR_3M)
    tryParseDefaultingTenor("GBP-SONIA", Tenor.TENOR_6M) shouldBe Some(OvernightIndices.GBP_SONIA)
    tryParseDefaultingTenor("GB-RPI", Tenor.TENOR_6M) shouldBe Some(PriceIndices.GB_RPI)

    tryParseDefaultingTenor("", Tenor.TENOR_6M) shouldBe None
    tryParseDefaultingTenor("   ", Tenor.TENOR_6M) shouldBe None
    tryParseDefaultingTenor("NotAnIndex", Tenor.TENOR_6M) shouldBe None
  }

  //-------------------------------------------------------------------------
  test("test_name") {
    forEvery(dataName) { (index: FloatingRateIndex, name: String) =>
      index.name shouldBe name
    }
  }

  test("test_toString") {
    // A member renders as its unique name, which is what makes the name the identity of the
    // value: the same text a diagnostic shows is the text the lookup below accepts.
    forEvery(dataName) { (index: FloatingRateIndex, name: String) =>
      index.toString shouldBe name
    }
  }

  test("test_of_lookup") {
    // The Java factory both resolved a name exactly and reported an unknown one; the port
    // splits those into `valueOf` and `parse`, and both are asserted so that the two entry
    // points cannot drift apart. Every row carries a full index name, so no tenor defaulting
    // takes part - that is the business of the four `parse`/`tryParse` tests above.
    forEvery(dataName) { (index: FloatingRateIndex, name: String) =>
      withClue(s"$name: ") {
        FloatingRateIndex.valueOf(name) shouldBe Some(index)
        FloatingRateIndex.parse(name) should haveValue(index)
      }
    }

    // The membership of the union, asserted here rather than in a test of its own because the
    // Java class has no `coverage` method to host it: the union resolves every member the three
    // families publish, to that very member. Nothing is added and nothing is hidden by the
    // search, so the union is exactly the Ibor, Overnight and Price families - whose closedness
    // and whose transcribed data are pinned by `NamedEnumClosedSpec` and
    // `ReferenceDataManifestSpec` respectively, which this assertion complements and neither
    // replaces. The complementary half, that no exchange-rate index is a member, is asserted in
    // `test_of_lookup_notFound`.
    val published: List[FloatingRateIndex] =
      IborIndex.values.toList ::: OvernightIndex.values.toList ::: PriceIndex.values.toList
    published.foreach { index =>
      withClue(s"${index.name}: ") {
        FloatingRateIndex.valueOf(index.name) shouldBe Some(index)
      }
    }
  }

  test("test_of_convert") {
    // Ruling: the Java method asserted the guarantee that Joda-Convert's `@ToString`/
    // `@FromString` pair gave - rendering an index produced its name and resolving that name
    // produced the index back. Joda is not on this classpath, so the guarantee itself is
    // asserted: the rendering of the union and the exact lookup of the union are inverse, in
    // both directions, for every row. This is deliberately not a JSON codec test - the union is
    // a trait and is excluded from JSON, and the three families that do publish codecs assert
    // their own round trips.
    forEvery(dataName) { (index: FloatingRateIndex, name: String) =>
      withClue(s"$name: ") {
        // value to text: the rendering a caller gets, through the family's own `Show`
        renderThroughUnion(index) shouldBe name

        // text to value: the exact union lookup, which is the inverse of that rendering
        FloatingRateIndex.valueOf(name) shouldBe Some(index)

        // and the round trip closes in both directions, which is what the pair guaranteed: the
        // rendering of a resolved value is the text it was resolved from, and the resolution of
        // a rendered value is the value it was rendered from
        FloatingRateIndex.parse(renderThroughUnion(index)) should haveValue(index)
        FloatingRateIndex.valueOf(name).map(renderThroughUnion) shouldBe Some(name)
      }
    }
  }

  test("test_of_lookup_notFound") {
    // This is the exclusion boundary of the union, and the failure is the point of the test.
    // `EUR/USD-ECB` is a published exchange-rate index and a published `Index`, so the name is
    // not rubbish; it resolves to nothing here only because the exchange-rate family is not a
    // member of this union. The exact input the Java method used is kept.
    FloatingRateIndex.valueOf(FxIndices.EUR_USD_ECB.name) shouldBe None
    FloatingRateIndex.parse(FxIndices.EUR_USD_ECB.name) should beFailureWith(FailureReason.PARSING)

    // The boundary holds for the whole family, not only for the member the Java method named.
    FxIndex.values.toList.foreach { fxIndex =>
      withClue(s"${fxIndex.name}: ") {
        FloatingRateIndex.valueOf(fxIndex.name) shouldBe None
        FloatingRateIndex.parse(fxIndex.name) should beFailureWith(FailureReason.PARSING)
      }
    }

    // Text naming no member of any family is reported the same way, as a parsing failure
    // rather than a raised error.
    FloatingRateIndex.valueOf("NotAnIndex") shouldBe None
    FloatingRateIndex.parse("NotAnIndex") should beFailureWith(FailureReason.PARSING)
  }

  test("test_of_lookup_null") {
    // Reinterpretation: the Java method handed the absent reference to the exact lookup and
    // asserted that it raised. This port writes no such reference and resolves a name as a
    // value, so the case is asserted as the two spellings of an absent name that can actually
    // be supplied - the empty name and a blank one - each of which names no member and so is
    // reported as a parsing failure.
    FloatingRateIndex.valueOf("") shouldBe None
    FloatingRateIndex.parse("") should beFailureWith(FailureReason.PARSING)
    FloatingRateIndex.valueOf("   ") shouldBe None
    FloatingRateIndex.parse("   ") should beFailureWith(FailureReason.PARSING)
  }
}
