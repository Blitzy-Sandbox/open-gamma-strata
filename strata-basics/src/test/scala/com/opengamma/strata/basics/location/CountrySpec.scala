/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.location

import java.util.Locale

import cats.Eq
import cats.Hash
import cats.Order
import cats.Show

import io.circe.Json
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor1
import org.scalatest.prop.TableFor2

import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[Country]].
 *
 * Every method of the Java original is kept, under its own name, so that the method-level
 * traceability of the migration stays one-to-one; the original's two bad-input providers
 * become the two tables declared with the tests that read them. The original's
 * `test_serialization` is the one method not here: the test mapping manifest consolidates it
 * into `json.JsonRoundTripSpec`, so what this spec pins instead is the per-type JSON
 * representation, in a test of its own at the end.
 *
 * Five of the ported methods assert a guarantee the port gives differently from the original,
 * and the reasoning is recorded at each of them:
 *
 *  - `test_of_String` and `test_of_String_unknownCountryCreated` asserted that two calls with
 *    one code returned the ''same instance'', which was a property of the instance cache the
 *    original kept rather than of the value. This port interns nothing, so the guarantee that
 *    matters - two calls are indistinguishable - is asserted as equality together with an equal
 *    hash.
 *  - `test_new_Country_included_in_getAvailable` asserted that building a country grew the
 *    available set by one, which is the same instance cache seen from the outside. The
 *    available set here describes what the library knows rather than what it has been asked
 *    for, so the assertion is the documented divergence: the set is fixed, and building a
 *    country outside it leaves it unchanged.
 *  - `test_of_String_bad` and `test_parse_String_bad` asserted that malformed text raised. It is
 *    reported as a value here, so each row is asserted to be an `Invalid` failure, and the
 *    absent-input row of each provider has no counterpart because no such value can be written.
 *  - `test_compareTo_null` asserted that comparing against an absent value raised. There is no
 *    such value, so the case becomes the property that makes comparison safe to rely on: the
 *    ordering is total, antisymmetric, and agrees with equality.
 *  - `test_from3CharString_missing` and `test_get3CharString_missing` asserted one raised error
 *    apiece. The port distinguishes the two causes the original conflated - text that is not the
 *    shape of a three letter code, and a well formed code that names nothing - so both are
 *    asserted, by reason and by the message the original used.
 *
 * The three letter conversions were adjudicated against the published Java jar rather than
 * derived by hand: `CRI` is `CR`, `GIB` is `GI`, and `GB`, `FR`, `US` convert to `GBR`, `FRA`
 * and `USA`.
 */
class CountrySpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * The 46 constants of the type, each with the code it carries, grouped as the original
   * declared them: Europe, the Americas, then the rest of the world.
   *
   * The table is the pairing the Java `test_constants` asserted one line at a time, and it is
   * read by several tests below so that a constant added to the type is covered by all of them
   * at once.
   */
  private val dataConstants: TableFor2[String, Country] =
    Table(
      ("code", "country"),
      // selected countries of Europe
      ("EU", Country.EU),
      ("AT", Country.AT),
      ("BE", Country.BE),
      ("CH", Country.CH),
      ("CZ", Country.CZ),
      ("DE", Country.DE),
      ("DK", Country.DK),
      ("ES", Country.ES),
      ("FI", Country.FI),
      ("FR", Country.FR),
      ("GB", Country.GB),
      ("GR", Country.GR),
      ("HU", Country.HU),
      ("IE", Country.IE),
      ("IS", Country.IS),
      ("IT", Country.IT),
      ("LU", Country.LU),
      ("NL", Country.NL),
      ("NO", Country.NO),
      ("PL", Country.PL),
      ("PT", Country.PT),
      ("SE", Country.SE),
      ("SK", Country.SK),
      ("TR", Country.TR),
      // selected countries of the Americas
      ("AR", Country.AR),
      ("BR", Country.BR),
      ("CA", Country.CA),
      ("CL", Country.CL),
      ("MX", Country.MX),
      ("US", Country.US),
      // selected countries of the rest of the world
      ("AU", Country.AU),
      ("CN", Country.CN),
      ("EG", Country.EG),
      ("HK", Country.HK),
      ("ID", Country.ID),
      ("IL", Country.IL),
      ("IN", Country.IN),
      ("JP", Country.JP),
      ("KR", Country.KR),
      ("MY", Country.MY),
      ("NZ", Country.NZ),
      ("RU", Country.RU),
      ("SA", Country.SA),
      ("SG", Country.SG),
      ("TH", Country.TH),
      ("ZA", Country.ZA))

  //-------------------------------------------------------------------------
  test("test_constants") {
    forAll(dataConstants) { (code: String, country: Country) =>
      withClue(s"$code: ") {
        Country.of(code) should haveValue(country)
        country.code shouldBe code
      }
    }
    // the table covers every constant the type declares, which is what makes the tests that
    // read it exhaustive rather than a sample
    dataConstants.length shouldBe 46
  }

  //-----------------------------------------------------------------------
  test("test_getAvailable") {
    val available: Set[Country] = Country.availableCountries
    available should contain(Country.US)
    available should contain(Country.EU)
    available should contain(Country.JP)
    available should contain(Country.GB)
    available should contain(Country.CH)
    available should contain(Country.AU)
    available should contain(Country.CA)

    // The set is the union of the alpha-2 side of the reference data with the constants of the
    // type. The constants contribute exactly one country the data does not - the `EU` region,
    // which the standard gives no three letter code - so the size is one more than the data's.
    available should have size (CountryData.alpha2Codes.size.toLong + 1L)
    available should have size 252
    forAll(dataConstants) { (code: String, country: Country) =>
      withClue(s"$code: ") {
        available should contain(country)
      }
    }
    // it is ordered by code, being a sorted set over the ordering of the type
    available.toList shouldBe available.toList.sorted(Order[Country].toOrdering)
  }

  test("test_new_Country_included_in_getAvailable") {
    // Documented divergence. The Java equivalent of this set exposed the contents of an
    // instance cache, so asking for a country the cache did not hold grew it by one and this
    // test asserted that growth. The set here is fixed: it describes the countries the library
    // knows about rather than the ones it has been asked for, so building a country outside it -
    // which `of` permits for any well formed code - leaves it exactly as it was.
    val before: Set[Country] = Country.availableCountries
    before.size should be > 0

    Country.of("XZ") should beSuccess
    val after: Set[Country] = Country.availableCountries

    after.size - before.size shouldBe 0
    after shouldBe before
    // and the country that was built is genuinely outside the set, so the assertion above is
    // not satisfied by it having been there all along
    Country.of("XZ").map(after.contains) should haveValue(false)
  }

  //-----------------------------------------------------------------------
  test("test_of_String") {
    Country.of("SE") should haveValue(Country.SE)
    Country.of("SE").map(_.code) should haveValue("SE")
    // Reinterpretation: the Java assertion was that two calls returned the same instance, a
    // property of its instance cache. Nothing is interned here, so what is asserted is that two
    // calls are indistinguishable - equal, with equal hashes.
    val first = Country.of("SE")
    val second = Country.of("SE")
    first shouldBe second
    first.map(_.hashCode) shouldBe second.map(_.hashCode)
    first.map(_.code) shouldBe second.map(_.code)
  }

  test("test_of_String_unknownCountryCreated") {
    // The code space is open: any two upper case ASCII letters are accepted whether or not the
    // standard assigns them, which is the behaviour of the original.
    Country.of("AA").map(_.code) should haveValue("AA")
    Country.of("AA") shouldBe Country.of("AA")
    Country.of("ZZ").map(_.code) should haveValue("ZZ")
    Country.of("QQ").map(_.code) should haveValue("QQ")
    // an unassigned code is a country like any other, and is not equal to an assigned one
    Country.of("AA") should not be Country.of("AT")
  }

  test("test_of_String_bad") {
    // The rows of the Java `data_ofBad` provider, less its absent-input row, which cannot be
    // written here. Every row is one accumulated `Invalid` failure: the three ways of being
    // malformed share one message in the original because what the caller has to correct is the
    // same in each case, and they share one here too.
    val dataOfBad: TableFor1[String] =
      Table("input", "", "A", "gb", "ABC", "123", " GB", "G B", "G-B", "GBR", "\u0000B", "gB", "Gb")

    forAll(dataOfBad) { (input: String) =>
      withClue(s"'$input': ") {
        Country.of(input) should beFailureWith(FailureReason.INVALID)
        // a single cause, not an accumulation: the input has one thing wrong with it
        Country.of(input).left.map(_.length) shouldBe Left(1L)
      }
    }
  }

  //-----------------------------------------------------------------------
  test("test_parse_String") {
    Country.parse("GB") should haveValue(Country.GB)
    Country.parse("GB").map(_.code) should haveValue("GB")
  }

  test("test_parse_String_unknownCountryCreated") {
    Country.parse("zy").map(_.code) should haveValue("ZY")
    Country.parse("zy") shouldBe Country.of("ZY")
  }

  test("test_parse_String_lowerCase") {
    Country.parse("gb") should haveValue(Country.GB)
    Country.parse("gb").map(_.code) should haveValue("GB")
    // any mixture of case, folded for the English locale rather than the default locale of the
    // host, so the result does not depend on where the program runs
    Country.parse("gB") should haveValue(Country.GB)
    Country.parse("Gb") should haveValue(Country.GB)
    forAll(dataConstants) { (code: String, country: Country) =>
      withClue(s"$code: ") {
        Country.parse(code.toLowerCase(Locale.ENGLISH)) should haveValue(country)
      }
    }
  }

  test("test_parse_String_bad") {
    // The rows of the Java `data_parseBad` provider, less its absent-input row. `gb` is absent
    // from this provider and present in the other, because folding case is exactly what `parse`
    // adds to `of`.
    val dataParseBad: TableFor1[String] =
      Table("input", "", "A", "ABC", "123", " GB", "G B", "\u00c9\u00c9", "\uff21\uff21")

    forAll(dataParseBad) { (input: String) =>
      withClue(s"'$input': ") {
        Country.parse(input) should beFailureWith(FailureReason.INVALID)
      }
    }
    // `parse` folds the text before checking it, so the message quotes the folded code rather
    // than the text as it was supplied. That is the behaviour of the original, which folds in
    // exactly the same place and lets its check report the folded value, so it is what is
    // asserted here. (The scaladoc of `Country.parse` currently says the opposite; the code, and
    // the Java it ports, are what this asserts.)
    Country.parse("abc") should haveFailureMessageMatching(".*'ABC'.*")
    Country.parse("abc") should beFailureWith(FailureReason.INVALID)
    // and `of`, which folds nothing, quotes exactly what it was given
    Country.of("abc") should haveFailureMessageMatching(".*'abc'.*")
  }

  //-----------------------------------------------------------------------
  test("test_compareTo") {
    val a = Country.EU
    val b = Country.GB
    val c = Country.JP
    Order[Country].compare(a, a) shouldBe 0
    Order[Country].compare(b, b) shouldBe 0
    Order[Country].compare(c, c) shouldBe 0

    Order[Country].compare(a, b) should be < 0
    Order[Country].compare(b, a) should be > 0

    Order[Country].compare(a, c) should be < 0
    Order[Country].compare(c, a) should be > 0

    Order[Country].compare(b, c) should be < 0
    Order[Country].compare(c, b) should be > 0

    // the ordering is alphabetical by code, which is the comparison of the original
    List(Country.JP, Country.EU, Country.GB).sorted(Order[Country].toOrdering) shouldBe
      List(Country.EU, Country.GB, Country.JP)
  }

  test("test_compareTo_null") {
    // Reinterpretation: the Java method compared against an absent value and asserted that it
    // raised. No such value can be written here, so what is asserted instead is the property
    // that makes the ordering safe to rely on and which the original's `compareTo` shares: it is
    // total over the type, antisymmetric, and agrees with equality - `compare` is zero exactly
    // when the two countries are equal.
    val sample: List[Country] =
      List(Country.EU, Country.GB, Country.JP, Country.US) ::: List("AA", "ZZ", "QQ").flatMap(code =>
        Country.of(code).toOption.toList)

    for (left <- sample; right <- sample) {
      val comparison = Order[Country].compare(left, right)
      val reversed = Order[Country].compare(right, left)
      withClue(s"${left.code} against ${right.code}: ") {
        (comparison == 0) shouldBe Eq[Country].eqv(left, right)
        math.signum(comparison) shouldBe -math.signum(reversed)
      }
    }
  }

  //-----------------------------------------------------------------------
  test("test_from3CharString_constants") {
    Country.of3Char("GBR") shouldBe Right(Country.GB)
    Country.of3Char("FRA") shouldBe Right(Country.FR)
    Country.of3Char("USA") shouldBe Right(Country.US)
  }

  //-----------------------------------------------------------------------
  test("test_from3CharString_nonConstants") {
    // a country that no constant names is reached the same way, and the two factories agree on it
    Country.of3Char("CRI").map(_.code) shouldBe Right("CR")
    Country.of3Char("CRI").toOption shouldBe Country.of("CR").toOption
    Country.of3Char("GIB").map(_.code) shouldBe Right("GI")
    Country.of3Char("GIB").toOption shouldBe Country.of("GI").toOption
  }

  //-----------------------------------------------------------------------
  test("test_from3CharString_missing") {
    // The original raised one kind of error for both of these; the port reports them as the two
    // distinct causes they are. A well formed code that names no country is a `Parsing` failure
    // carrying the message the original used.
    Country.of3Char("ZZZ") should beFailureWith(FailureReason.PARSING)
    Country.of3Char("ZZZ").left.map(_.message) shouldBe Left("Unknown country code: ZZZ")
    // text that is not the shape of a three letter code never reaches the lookup, so it is an
    // `Invalid` failure instead
    Country.of3Char("zzz") should beFailureWith(FailureReason.INVALID)
    Country.of3Char("") should beFailureWith(FailureReason.INVALID)
    Country.of3Char("GB") should beFailureWith(FailureReason.INVALID)
    Country.of3Char("GBRA") should beFailureWith(FailureReason.INVALID)
    Country.of3Char("12 ") should beFailureWith(FailureReason.INVALID)
  }

  //-----------------------------------------------------------------------
  test("test_get3CharString") {
    Country.GB.code3Char shouldBe Right("GBR")
    Country.FR.code3Char shouldBe Right("FRA")
    Country.US.code3Char shouldBe Right("USA")
    Country.of("CR").map(_.code3Char) shouldBe Right(Right("CRI"))
    Country.of("GI").map(_.code3Char) shouldBe Right(Right("GIB"))

    // every row of the reference data round-trips in both directions, which is the property the
    // three constants above are a sample of
    CountryData.alpha3ToAlpha2.foreach {
      case (alpha3, alpha2) =>
        withClue(s"$alpha3/$alpha2: ") {
          Country.of3Char(alpha3).map(_.code) shouldBe Right(alpha2)
          Country.of3Char(alpha3).flatMap(_.code3Char) shouldBe Right(alpha3)
        }
    }
  }

  //-----------------------------------------------------------------------
  test("test_get3CharString_missing") {
    // A well formed code with no row in the reference data has no three letter form. That is a
    // property of the data rather than a mistake by the caller, so it is `MissingData`, with the
    // message the original used.
    Country.of("ZZ").map(_.code3Char) shouldBe Right(Left(Failure.MissingData("Unknown country: ZZ")))
    Country.EU.code3Char should beFailureWith(FailureReason.MISSING_DATA)
    Country.EU.code3Char.left.map(_.message) shouldBe Left("Unknown country: EU")
  }

  //-----------------------------------------------------------------------
  test("test_equals_hashCode") {
    val a1 = Country.GB
    val a2 = Country.of("GB")
    val b = Country.EU

    a1 shouldBe a1
    a2 should haveValue(a1)
    a1 should not be b
    a1 should not equal ""
    a2.map(_.hashCode) shouldBe Right(a1.hashCode)

    // the code is the whole of the value, so the three equality-bearing views agree
    Eq[Country].eqv(a1, b) shouldBe false
    Hash[Country].eqv(a1, b) shouldBe false
    Order[Country].compare(a1, b) should not be 0
    Country.of("GB").map(country => Hash[Country].hash(country)) shouldBe Right(Hash[Country].hash(a1))
  }

  //-----------------------------------------------------------------------
  test("test_toString") {
    Country.GB.toString shouldBe "GB"
    Show[Country].show(Country.GB) shouldBe "GB"
    forAll(dataConstants) { (code: String, country: Country) =>
      withClue(s"$code: ") {
        country.toString shouldBe code
        Show[Country].show(country) shouldBe code
      }
    }
  }

  test("test_jodaConvert") {
    // Reinterpretation: the Java method asserted the round trip of the reflective
    // string-conversion library the original registered with. The library is gone with the
    // port, and the guarantee it gave is asserted directly: a country renders as its bare code,
    // and that rendering reads back as the same country.
    forAll(dataConstants) { (code: String, country: Country) =>
      val rendered = Show[Country].show(country)
      withClue(s"$code: ") {
        rendered shouldBe code
        Country.of(rendered) should haveValue(country)
      }
    }
    Country.of(Country.US.toString) should haveValue(Country.US)
  }

  //-----------------------------------------------------------------------
  // The tests below are the port's own: the JSON representation, which replaces the platform
  // serialization the consolidated round-trip spec no longer covers per type, and the closed
  // construction surface of a validated type.

  test("test_codec") {
    Country.GB.asJson shouldBe Json.fromString("GB")
    Json.fromString("GB").as[Country] shouldBe Right(Country.GB)
    // an unassigned but well formed code is written and read like any other
    Country.of("AA").map(_.asJson) shouldBe Right(Json.fromString("AA"))
    Json.fromString("AA").as[Country].map(_.code) shouldBe Right("AA")
    // reading goes through `of` rather than `parse`, so the codec accepts exactly the canonical
    // form it writes and a document holding a folded code is rejected rather than quietly
    // corrected
    Json.fromString("gb").as[Country].isLeft shouldBe true
    Json.fromString("ABC").as[Country].isLeft shouldBe true
    Json.fromString("").as[Country].isLeft shouldBe true
    Json.fromInt(1).as[Country].isLeft shouldBe true
    Json.Null.as[Country].isLeft shouldBe true
  }

  test("test_construction_isClosed") {
    // A validated type publishes its factory and nothing else: there is no generated
    // constructor to bypass the check with, and no copy to change a checked value with. Both are
    // compile-time properties, so both are asserted as such.
    assertDoesNotCompile("""Country("GB")""")
    assertDoesNotCompile("""Country.GB.copy(code = "XX")""")
    assertDoesNotCompile("""new Country("GB") {}""")
    // the factory is the only way in, and it answers with a value rather than raising
    Country.of("GB") should beSuccess
  }
}
