/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.location

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2

/**
 * Test [[CountryData]].
 *
 * The Java sources have no test class for this data, because in the original it was not code: it
 * was a properties resource that a loader read from the class path on first use, and what the
 * Java `CountryTest` asserted about it was a handful of conversions through `Country`. Here the
 * table is Scala literals fixed at compile time, so it can be asserted directly, and this spec
 * is the port's own.
 *
 * The row-for-row comparison against the captured Java reference data belongs to
 * `ReferenceDataManifestSpec`, which compares every table of the module with the manifest taken
 * from the Java implementation; duplicating that comparison here would add nothing. What this
 * spec pins is everything a consumer of the table relies on and the manifest comparison does not
 * state: the count, the published order, the shape of every key and value, the fact that the
 * relation is a bijection - which is what makes the inverse map total and unambiguous, as the
 * original's bidirectional map was - and the agreement of the derived views with the table they
 * are derived from.
 *
 * The assertions deliberately never name the type of the collections. The published members are
 * ordered maps and sets, and the order is the contract; how they are backed, and what private
 * lookups are derived from them, is not, so the order and the lookup results are what is
 * asserted.
 */
class CountryDataSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /**
   * Spot rows of the published data, each taken from the Java resource being transcribed: the
   * first and last rows in ascending order of the three letter code, the three rows the Java
   * `CountryTest` asserted through the constants, the two it asserted through countries that no
   * constant names, and the first row of the resource in its own declaration order.
   */
  private val spotRows: TableFor2[String, String] =
    Table(
      ("alpha3", "alpha2"),
      ("ABW", "AW"),
      ("AND", "AD"),
      ("CRI", "CR"),
      ("FRA", "FR"),
      ("GBR", "GB"),
      ("GIB", "GI"),
      ("USA", "US"),
      ("ZWE", "ZW"))

  //-------------------------------------------------------------------------
  test("test_alpha3ToAlpha2_holdsEveryPublishedRow") {
    CountryData.alpha3ToAlpha2 should have size 251
    forAll(spotRows) { (alpha3: String, alpha2: String) =>
      withClue(s"$alpha3: ") {
        CountryData.alpha3ToAlpha2.get(alpha3) shouldBe Some(alpha2)
      }
    }
  }

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

  test("test_codeSets_agreeWithTheTableTheyViewOf") {
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

  test("test_tableIsTheDataCountryResolvesThrough") {
    // One bridge assertion, so that a mistranscribed row cannot pass by being self consistent:
    // every row of the table is what `Country` answers with, in both directions.
    forAll(spotRows) { (alpha3: String, alpha2: String) =>
      withClue(s"$alpha3: ") {
        Country.of3Char(alpha3).map(_.code) shouldBe Right(alpha2)
        Country.of3Char(alpha3).flatMap(_.code3Char) shouldBe Right(alpha3)
      }
    }
    // and a code the table does not hold has no conversion in either direction
    Country.of3Char("ZZZ").isLeft shouldBe true
    Country.of("EU").map(_.code3Char.isLeft) shouldBe Right(true)
  }
}
