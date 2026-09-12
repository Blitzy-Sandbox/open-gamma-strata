/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.location

import java.util.Locale

import scala.collection.immutable.SortedSet

import cats.Eq
import cats.Hash
import cats.Order
import cats.Show

import io.circe.DecodingFailure
import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.EncoderOps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor1
import org.scalatest.prop.TableFor2

import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers

/**
 * Test [[Country]].
 *
 * All twenty-one methods of the Java original are kept, under their own names and in the
 * order the original declared them, so that the method-level traceability of the migration
 * stays one-to-one; each of the original's two bad-input providers becomes one table declared
 * with the test that reads it, carrying the rows the provider carried other than its
 * absent-input row, for the reason given below. The five cases of the closing section are the
 * only ones that answer to no Java method, and they are named after the members of
 * [[CountryData]] they pin rather than after a method of the original, so the twenty-one cases
 * above remain exactly the count and the names of the original.
 *
 * Where the port gives a guarantee differently from the original the reasoning is recorded at
 * the method, and the same reasoning is collected here:
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
 *  - `test_serialization` asserted the round trip of the platform serialization the original
 *    supported. That support is not carried over, and the JSON codec is the supported way to
 *    write and read a country, so the method asserts the codec: the bare string the type
 *    writes, what it reads back, and what it refuses. (The test mapping manifest additionally
 *    routes this Java method to the cross-type round-trip spec, which exercises the same codec
 *    through generated values; what is asserted here is the representation itself, which is
 *    per-type and which a generated round trip cannot pin.)
 *  - `test_jodaConvert` asserted the round trip of the reflective string-conversion library
 *    the original registered with. The library is gone, and the guarantee it gave - the
 *    rendered name is the identity, and reads back as the same country - is asserted directly.
 *
 * The three letter conversions were adjudicated against the published Java jar rather than
 * derived by hand: `CRI` is `CR`, `GIB` is `GI`, and `GB`, `FR`, `US` convert to `GBR`, `FRA`
 * and `USA`.
 *
 * ===The reference data behind the three letter conversions===
 *
 * The closing section pins [[CountryData]], the table those conversions resolve through. The
 * AAP's frozen test inventory (section 0.3.1) gives the `location` package this one spec, and
 * the table has no Java test class of its own: in the original it was not code but a properties
 * resource a loader read from the class path, and what the Java `CountryTest` asserted about it
 * is what the tests above assert. Here the table is Scala literals fixed at compile time, so
 * what a consumer of it relies on is asserted directly - the published order, the shape of every
 * key and value, the fact that the relation is a bijection, which is what makes the inverse
 * total and unambiguous as the original's bidirectional map was, and the agreement of the
 * derived views with the table they are views of.
 *
 * The row-for-row comparison against the data captured from the Java implementation is not
 * repeated there. `ReferenceDataManifestSpec` compares every table of the module with the
 * captured manifest, the 251 country rows and their count among them, and `test_get3CharString`
 * above already carries every row of the table through `Country` in both directions; both are
 * stronger than a hand-written expectation table, so the closing section states only what
 * neither of them does. Its assertions deliberately never name the type of the collections: the
 * published members are ordered maps and sets and the order is the contract, while how they are
 * backed, and what private lookups are derived from them, is not.
 */
class CountrySpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks with ResultMatchers {

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
    // an immutable sorted set of the library's own collection vocabulary, where the original
    // answered with a platform set
    val available: SortedSet[Country] = Country.availableCountries
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
    // which `of` permits for any well formed code - leaves it exactly as it was. The divergence
    // is the one this spec states, and it is recorded in `SCALA_MIGRATION.md`; the behaviour it
    // replaces is at `Country.java:55,285` (the cache and the call that grows it) and
    // `Country.java:265-268` (the snapshot of it this method answered with).
    val before: SortedSet[Country] = Country.availableCountries
    before.size should be > 0

    Country.of("XZ") should beSuccess
    val after: SortedSet[Country] = Country.availableCountries

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
    // standard assigns them, which is the behaviour of the original and the reason this type is
    // a validated value rather than one of the closed named families of the library.
    Country.of("AA").map(_.code) should haveValue("AA")
    Country.of("AA") shouldBe Country.of("AA")
    Country.of("ZZ").map(_.code) should haveValue("ZZ")
    Country.of("QQ").map(_.code) should haveValue("QQ")
    // an unassigned code is a country like any other, and is not equal to an assigned one
    Country.of("AA") should not be Country.of("AT")
    // and it is genuinely unassigned: no row of the reference data names it, so it is outside
    // the set of countries the library knows about, which is what makes it a probe of openness
    // rather than a second way of asking for a catalogued country
    Country.of("AA").map(Country.availableCountries.contains) should haveValue(false)
  }

  test("test_of_String_bad") {
    // The six rows of the Java `data_ofBad` provider, transcribed in the order the provider
    // declared them. Its seventh row was an absent input, and that row has no counterpart: the
    // port models an absent value as an `Option` and drops the checks the original made against
    // a missing reference, so no such argument can be written - a fact asserted below at the
    // only level at which it can be, the compiler's.
    //
    // Note that `gb` is rejected here and accepted by `parse`, which is the asymmetry the two
    // providers of the original encode between them: folding case is exactly what `parse` adds.
    val dataOfBad: TableFor1[String] =
      Table("input", "", "A", "gb", "ABC", "123", " GB")

    forAll(dataOfBad) { (input: String) =>
      withClue(s"'$input': ") {
        // Every row is one accumulated `Invalid` failure: the three ways of being malformed
        // share one message in the original because what the caller has to correct is the same
        // in each case, and they share one here too.
        Country.of(input) should beFailureWith(FailureReason.INVALID)
        // a single cause, not an accumulation: the input has one thing wrong with it
        Country.of(input).left.map(_.length) shouldBe Left(1L)
      }
    }

    // beyond the provider, the shapes it samples: an embedded space, a punctuation character, a
    // three letter code, a control character and a mixture of case are each malformed too
    List("G B", "G-B", "GBR", "\u0000B", "gB", "Gb").foreach { input =>
      withClue(s"'$input': ") {
        Country.of(input) should beFailureWith(FailureReason.INVALID)
      }
    }

    // The absent-input row of the provider, at the only level it can be asserted. The positive
    // form is asserted alongside it so that the ruling is known to be about the argument being
    // absent rather than about anything else in the snippet.
    assertCompiles("""Country.of("GB")""")
    assertDoesNotCompile("""Country.of(Option.empty[String])""")
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
    // every constant is recovered from the lower case of its code; the locale is named here for
    // the same reason the type names it, so that the folding this test performs is the folding
    // the type performs and neither depends on the host the tests run on
    forAll(dataConstants) { (code: String, country: Country) =>
      withClue(s"$code: ") {
        Country.parse(code.toLowerCase(Locale.ENGLISH)) should haveValue(country)
      }
    }
  }

  test("test_parse_String_bad") {
    // The five rows of the Java `data_parseBad` provider, in its order. Its sixth row was an
    // absent input and has no counterpart, for the reason given on `test_of_String_bad`. `gb` is
    // deliberately absent from this provider and present in the other, because folding case is
    // exactly what `parse` adds to `of`, so it is valid here - asserted as such below.
    val dataParseBad: TableFor1[String] =
      Table("input", "", "A", "ABC", "123", " GB")

    forAll(dataParseBad) { (input: String) =>
      withClue(s"'$input': ") {
        Country.parse(input) should beFailureWith(FailureReason.INVALID)
      }
    }

    // the row the other provider holds and this one does not, asserted from the other side
    Country.parse("gb") should beSuccess

    // beyond the provider: folding case does not enlarge the code space, so a letter outside
    // ASCII is malformed however it is cased
    List("G B", "\u00c9\u00c9", "\uff21\uff21").foreach { input =>
      withClue(s"'$input': ") {
        Country.parse(input) should beFailureWith(FailureReason.INVALID)
      }
    }

    // the absent-input row of the provider, at the only level it can be asserted
    assertCompiles("""Country.parse("GB")""")
    assertDoesNotCompile("""Country.parse(Option.empty[String])""")
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

    // the ordering agrees with equality over these values - `compare` is zero exactly when the
    // two countries are equal - which the code being the whole of the value makes true without
    // any secondary comparison, unlike several other validated types of this library
    for (left <- List(a, b, c); right <- List(a, b, c)) {
      withClue(s"${left.code} against ${right.code}: ") {
        (Order[Country].compare(left, right) == 0) shouldBe Eq[Country].eqv(left, right)
      }
    }
  }

  test("test_compareTo_null") {
    // Reinterpretation: the Java `test_compareTo_null` compared a country against an absent
    // value and asserted that the comparison raised. The port models an absent value as an
    // `Option` and drops the checks against a missing reference, so that comparison cannot be
    // written and the error it raised cannot be observed - which is why nothing here expects a
    // raised error. What replaces it is the pair of facts that make the raised error
    // unnecessary: the comparison is rejected at compile time rather than at run time, and
    // comparison over the values that do exist is total.
    assertCompiles("""cats.Order[Country].compare(Country.EU, Country.GB)""")
    assertDoesNotCompile("""cats.Order[Country].compare(Country.EU, Option.empty[Country])""")
    assertDoesNotCompile("""cats.Order[Country].compare(Country.EU, "GB")""")

    // and over real inhabitants the ordering is total, antisymmetric, and agrees with equality
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
    Country.of3Char("GBR") should haveValue(Country.GB)
    Country.of3Char("FRA") should haveValue(Country.FR)
    Country.of3Char("USA") should haveValue(Country.US)
  }

  //-----------------------------------------------------------------------
  test("test_from3CharString_nonConstants") {
    // a country that no constant names is reached the same way, and the two factories agree on it
    Country.of3Char("CRI").map(_.code) should haveValue("CR")
    Country.of3Char("CRI").toOption shouldBe Country.of("CR").toOption
    Country.of3Char("GIB").map(_.code) should haveValue("GI")
    Country.of3Char("GIB").toOption shouldBe Country.of("GI").toOption
  }

  //-----------------------------------------------------------------------
  test("test_from3CharString_missing") {
    // The original raised one kind of error for both of its cases; the port reports them as the
    // two distinct causes they are. A well formed code that names no country is a `Parsing`
    // failure carrying the message the original used.
    Country.of3Char("ZZZ") should beFailureWith(FailureReason.PARSING)
    Country.of3Char("ZZZ").left.map(_.message) shouldBe Left("Unknown country code: ZZZ")
    // text that is not the shape of a three letter code never reaches the lookup, so it is an
    // `Invalid` failure instead: too short, lower case, empty, too long, or holding characters
    // outside the code space
    Country.of3Char("zzz") should beFailureWith(FailureReason.INVALID)
    Country.of3Char("gbr") should beFailureWith(FailureReason.INVALID)
    Country.of3Char("") should beFailureWith(FailureReason.INVALID)
    Country.of3Char("GB") should beFailureWith(FailureReason.INVALID)
    Country.of3Char("GBRA") should beFailureWith(FailureReason.INVALID)
    Country.of3Char("12 ") should beFailureWith(FailureReason.INVALID)
    // The original's second case passed an absent value and asserted that it raised. As with
    // `test_of_String_bad`, no such argument can be written here, so the case is asserted at
    // the compiler.
    assertCompiles("""Country.of3Char("GBR")""")
    assertDoesNotCompile("""Country.of3Char(Option.empty[String])""")
  }

  //-----------------------------------------------------------------------
  test("test_get3CharString") {
    Country.GB.code3Char should haveValue("GBR")
    Country.FR.code3Char should haveValue("FRA")
    Country.US.code3Char should haveValue("USA")
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
    a2.map(_.hashCode) shouldBe Right(a1.hashCode)

    // The original's two negative cases compared a country against text and against an absent
    // value. Both are asserted through the untyped comparison of the framework, whose parameter
    // is `Any`, with the country ascribed so that no comparison of unrelated types is written -
    // one of those would be reported by the compiler and, this build treating a report as an
    // error, would not compile at all.
    (a1: Any) should not equal ""
    (a1: Any) should not equal null

    // the code is the whole of the value, so the three equality-bearing views agree
    Country.of("GB").map(country => Eq[Country].eqv(a1, country)) should haveValue(true)
    Eq[Country].eqv(a1, b) shouldBe false
    Hash[Country].eqv(a1, b) shouldBe false
    Order[Country].compare(a1, b) should not be 0
    Country.of("GB").map(country => Hash[Country].hash(country)) shouldBe Right(Hash[Country].hash(a1))

    // and the typed equality of the library cannot be asked the untyped question at all, which
    // is why the comparisons against text above are the framework's and not the type class's
    assertCompiles("""cats.Eq[Country].eqv(Country.GB, Country.EU)""")
    assertDoesNotCompile("""cats.Eq[Country].eqv(Country.GB, "")""")
  }

  //-----------------------------------------------------------------------
  test("test_toString") {
    Country.GB.toString shouldBe "GB"
    // the rendering of the type class is the rendering of the value, as it is for every named
    // or text-identified type of this port
    Show[Country].show(Country.GB) shouldBe "GB"
    forAll(dataConstants) { (code: String, country: Country) =>
      withClue(s"$code: ") {
        country.toString shouldBe code
        Show[Country].show(country) shouldBe code
      }
    }
    // a country outside the reference data renders the same way: the code is the whole of the
    // value, so there is nothing else it could render as
    Country.of("ZZ").map(_.toString) should haveValue("ZZ")
    Country.of("ZZ").map(country => Show[Country].show(country)) should haveValue("ZZ")
  }

  test("test_serialization") {
    // Reinterpretation: the Java method asserted the round trip of the platform serialization
    // the original supported, and the wire format of the bean library it was written against.
    // Neither is carried over - platform serialization and that library's documents are both
    // outside the scope of this port - and the JSON codec is the supported way to write and
    // read a country, so what is asserted here is that codec.
    //
    // A country is text, not a record: the document is the bare two letter code rather than an
    // object naming the field, which is the form the original wrote and which keeps a country
    // usable wherever a string is expected.
    Country.GB.asJson shouldBe Json.fromString("GB")
    Country.GB.asJson.noSpaces shouldBe "\"GB\""
    decode[Country]("\"GB\"") shouldBe Right(Country.GB)
    Json.fromString("GB").as[Country] shouldBe Right(Country.GB)

    // the round trip of the two values the original serialized, written and read back through
    // the document rather than through the encoder alone
    Country.GB.asJson.as[Country] shouldBe Right(Country.GB)
    Country.of("US").map(country => decode[Country](country.asJson.noSpaces)) shouldBe
      Right(Right(Country.US))

    // the code space is open in a document exactly as it is in the factory: a well formed code
    // the reference data does not hold is written and read like any other
    Country.of("AA").map(_.asJson) shouldBe Right(Json.fromString("AA"))
    decode[Country]("\"ZZ\"").map(_.code) shouldBe Right("ZZ")
    Json.fromString("AA").as[Country].map(_.code) shouldBe Right("AA")

    // Reading goes through `of` rather than `parse`, so the codec accepts exactly the canonical
    // form it writes: a document holding a folded or malformed code is rejected rather than
    // quietly corrected, and the rejection carries the message of the check that rejected it.
    List("\"gb\"", "\"ABC\"", "\"\"", "\" GB\"").foreach { document =>
      withClue(s"$document: ") {
        decode[Country](document) match {
          case Left(failure: DecodingFailure) => failure.message should include("countryCode")
          case other => fail(s"expected a DecodingFailure for $document but was $other")
        }
      }
    }

    // a document that is not text at all is rejected by the reader of the string, before the
    // factory is reached
    Json.fromInt(1).as[Country].isLeft shouldBe true
    Json.Null.as[Country].isLeft shouldBe true
    decode[Country]("{\"code\":\"GB\"}").isLeft shouldBe true
  }

  test("test_jodaConvert") {
    // Reinterpretation: the Java method asserted the round trip of the reflective
    // string-conversion library the original registered with. The library is gone with the
    // port, and the guarantee it gave is asserted directly: a country renders as its bare code,
    // and that rendering reads back as the same country, through both of the factories that
    // accept it. The rendered name remains the identity of the value, which is what made the
    // conversion the original registered worth asserting in the first place.
    forAll(dataConstants) { (code: String, country: Country) =>
      val rendered = Show[Country].show(country)
      withClue(s"$code: ") {
        rendered shouldBe code
        country.toString shouldBe code
        Country.of(country.code) should haveValue(country)
        Country.parse(country.toString) should haveValue(country)
      }
    }
    // the two values the original converted, stated as the original stated them
    Country.of(Country.GB.code) should haveValue(Country.GB)
    Country.parse(Country.GB.toString) should haveValue(Country.GB)
    Country.of("US").map(country => Show[Country].show(country)) should haveValue("US")
    Country.of(Country.US.toString) should haveValue(Country.US)
    Country.parse(Country.US.toString) should haveValue(Country.US)
  }

  //-----------------------------------------------------------------------
  // The reference data table the three letter conversions resolve through.
  //
  // No case below answers to a method of the Java original; each is named after the member of
  // `CountryData` it pins. What the captured reference data and `test_get3CharString` already
  // establish is not restated here - the scaladoc of this spec records which of them covers what.
  //-----------------------------------------------------------------------

  test("test_alpha3ToAlpha2_isPublishedInAscendingOrderOfCode") {
    // The published order is the contract of the member, and it is ascending by three letter
    // code: a consumer that iterates the table - a report, or a comparison against the captured
    // reference data - sees the rows in that order.
    val keys = CountryData.alpha3ToAlpha2.keys.toVector
    keys shouldBe keys.sorted
    keys.head shouldBe "ABW"
    keys.last shouldBe "ZWE"
    // and the order is stable: iterating twice gives the same sequence, so no consumer can
    // observe a different one
    CountryData.alpha3ToAlpha2.toVector shouldBe CountryData.alpha3ToAlpha2.toVector
  }

  test("test_alpha3ToAlpha2_keysAndValuesAreWellFormedCodes") {
    CountryData.alpha3ToAlpha2.foreach {
      case (alpha3, alpha2) =>
        withClue(s"$alpha3 -> $alpha2: ") {
          alpha3 should have length 3
          alpha2 should have length 2
          alpha3.forall(character => character >= 'A' && character <= 'Z') shouldBe true
          alpha2.forall(character => character >= 'A' && character <= 'Z') shouldBe true
        }
    }
  }

  test("test_relationIsABijection") {
    // The original held this data in a bidirectional map, which is what let it convert in both
    // directions. That is only sound while the relation is one-to-one, so it is asserted here:
    // both sides are distinct, and the two conversions compose to the identity in both
    // directions over the whole table.
    val alpha3Keys = CountryData.alpha3ToAlpha2.keys.toVector
    val alpha2Values = CountryData.alpha3ToAlpha2.values.toVector
    alpha3Keys.distinct should have size 251
    alpha2Values.distinct should have size 251

    CountryData.alpha3ToAlpha2.foreach {
      case (alpha3, alpha2) =>
        withClue(s"$alpha3 -> $alpha2: ") {
          CountryData.alpha2ToAlpha3.get(alpha2) shouldBe Some(alpha3)
        }
    }
    CountryData.alpha2ToAlpha3.foreach {
      case (alpha2, alpha3) =>
        withClue(s"$alpha2 -> $alpha3: ") {
          CountryData.alpha3ToAlpha2.get(alpha3) shouldBe Some(alpha2)
        }
    }
  }

  test("test_alpha2ToAlpha3_isTheDerivedInverse") {
    CountryData.alpha2ToAlpha3 should have size 251
    val keys = CountryData.alpha2ToAlpha3.keys.toVector
    keys shouldBe keys.sorted
    CountryData.alpha2ToAlpha3.get("GB") shouldBe Some("GBR")
    CountryData.alpha2ToAlpha3.get("CR") shouldBe Some("CRI")
    // the `EU` region is the notable code the standard gives no three letter form, so it is
    // absent from the inverse and `Country.EU.code3Char` has nothing to answer with
    CountryData.alpha2ToAlpha3.get("EU") shouldBe None
    CountryData.alpha2ToAlpha3.get("ZZ") shouldBe None
  }

  test("test_codeSets_agreeWithTheTableTheyAreViewsOf") {
    CountryData.alpha3Codes should have size 251
    CountryData.alpha2Codes should have size 251
    CountryData.alpha3Codes.toVector shouldBe CountryData.alpha3ToAlpha2.keys.toVector
    CountryData.alpha2Codes.toVector shouldBe CountryData.alpha3ToAlpha2.values.toVector.sorted
    // both are published in ascending order
    CountryData.alpha3Codes.toVector shouldBe CountryData.alpha3Codes.toVector.sorted
    CountryData.alpha2Codes.toVector shouldBe CountryData.alpha2Codes.toVector.sorted
    // the alpha-2 set is the domain of the three letter conversions, not the set of codes a
    // country may carry: any two letter code is well formed, and `EU` is the code that shows the
    // difference
    CountryData.alpha2Codes should contain("GB")
    CountryData.alpha2Codes.contains("EU") shouldBe false
  }
}
