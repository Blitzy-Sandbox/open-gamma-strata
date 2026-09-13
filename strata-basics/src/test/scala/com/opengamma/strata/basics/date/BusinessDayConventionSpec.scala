/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import java.time.DayOfWeek.SATURDAY
import java.time.DayOfWeek.SUNDAY
import java.time.LocalDate
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
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableFor3

import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[BusinessDayConvention]].
 *
 * `adjust` takes a calendar rather than reference data, so these tests are the pure rule of each
 * convention with no lookup involved.
 *
 * Lookup has two entry points, both asserted wherever a name is looked up: `valueOf`, the exact
 * lookup over the canonical and upper-case keys, answering with an `Option`, and `parse`, which
 * adds the ordered lenient rewrites and answers with `EitherNec[Failure, _]`.
 */
class BusinessDayConventionSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  import BusinessDayConventionSpec._

  /** Each convention with the name it renders as. */
  private val dataName: TableFor2[BusinessDayConvention, String] = Table(
    ("convention", "name"),
    (BusinessDayConventions.NO_ADJUST, "NoAdjust"),
    (BusinessDayConventions.FOLLOWING, "Following"),
    (BusinessDayConventions.MODIFIED_FOLLOWING, "ModifiedFollowing"),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, "ModifiedFollowingBiMonthly"),
    (BusinessDayConventions.PRECEDING, "Preceding"),
    (BusinessDayConventions.MODIFIED_PRECEDING, "ModifiedPreceding"),
    (BusinessDayConventions.NEAREST, "Nearest")
  )

  /**
   * Spellings that the ordered chain of lenient rewrites turns into a canonical name: the
   * abbreviations, the screaming-snake spellings of the constant identifiers, and the spaced and
   * hyphen-free spellings of a name. A reordering of the pattern list changes what resolves.
   */
  private val dataLenient: TableFor2[String, BusinessDayConvention] = Table(
    ("name", "convention"),
    ("FOLLOWING", BusinessDayConventions.FOLLOWING),
    ("MODIFIED_FOLLOWING", BusinessDayConventions.MODIFIED_FOLLOWING),
    ("MODIFIED_FOLLOWING_BI_MONTHLY", BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY),
    ("PRECEDING", BusinessDayConventions.PRECEDING),
    ("MODIFIED_PRECEDING", BusinessDayConventions.MODIFIED_PRECEDING),
    ("F", BusinessDayConventions.FOLLOWING),
    ("M", BusinessDayConventions.MODIFIED_FOLLOWING),
    ("MF", BusinessDayConventions.MODIFIED_FOLLOWING),
    ("P", BusinessDayConventions.PRECEDING),
    ("MP", BusinessDayConventions.MODIFIED_PRECEDING),
    ("Follow", BusinessDayConventions.FOLLOWING),
    ("None", BusinessDayConventions.NO_ADJUST),
    ("Modified", BusinessDayConventions.MODIFIED_FOLLOWING),
    ("Mod", BusinessDayConventions.MODIFIED_FOLLOWING),
    ("Modified Following", BusinessDayConventions.MODIFIED_FOLLOWING),
    ("ModifiedFollowing", BusinessDayConventions.MODIFIED_FOLLOWING),
    ("Modified Follow", BusinessDayConventions.MODIFIED_FOLLOWING),
    ("ModifiedFollow", BusinessDayConventions.MODIFIED_FOLLOWING),
    ("Mod Following", BusinessDayConventions.MODIFIED_FOLLOWING),
    ("ModFollowing", BusinessDayConventions.MODIFIED_FOLLOWING),
    ("Mod Follow", BusinessDayConventions.MODIFIED_FOLLOWING),
    ("ModFollow", BusinessDayConventions.MODIFIED_FOLLOWING),
    ("Modified Preceding", BusinessDayConventions.MODIFIED_PRECEDING),
    ("ModifiedPreceding", BusinessDayConventions.MODIFIED_PRECEDING),
    ("Mod Preceding", BusinessDayConventions.MODIFIED_PRECEDING),
    ("ModPreceding", BusinessDayConventions.MODIFIED_PRECEDING),
    ("ModFollowingBiMonthly", BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY)
  )

  /**
   * Each constant identifier of [[BusinessDayConventions]] with the convention it names, every
   * one of which must resolve leniently.
   */
  private val dataConstantIdentifiers: TableFor2[String, BusinessDayConvention] = Table(
    ("identifier", "convention"),
    ("NO_ADJUST", BusinessDayConventions.NO_ADJUST),
    ("FOLLOWING", BusinessDayConventions.FOLLOWING),
    ("MODIFIED_FOLLOWING", BusinessDayConventions.MODIFIED_FOLLOWING),
    ("MODIFIED_FOLLOWING_BI_MONTHLY", BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY),
    ("PRECEDING", BusinessDayConventions.PRECEDING),
    ("MODIFIED_PRECEDING", BusinessDayConventions.MODIFIED_PRECEDING),
    ("NEAREST", BusinessDayConventions.NEAREST)
  )

  /** The seven conventions in declaration order. */
  private val declarationOrder: List[BusinessDayConvention] =
    List(
      BusinessDayConvention.NoAdjust,
      BusinessDayConvention.Following,
      BusinessDayConvention.ModifiedFollowing,
      BusinessDayConvention.ModifiedFollowingBiMonthly,
      BusinessDayConvention.Preceding,
      BusinessDayConvention.ModifiedPreceding,
      BusinessDayConvention.Nearest)

  //-------------------------------------------------------------------------
  test("test_convention") {
    forAll(dataConvention) { (convention: BusinessDayConvention, input: LocalDate, expected: LocalDate) =>
      withClue(s"$convention adjusting $input: ") {
        convention.adjust(input, HolidayCalendars.SAT_SUN) shouldBe expected
      }
    }

    // Every convention against a calendar that also holds a holiday, so that a run of
    // non-business days has to be walked rather than a single weekend, which is what makes the
    // direction each convention chooses observable - in particular `Nearest`, which decides from
    // the day of the week of the input before searching, so the Sunday and the holiday Monday
    // both move forward to the Tuesday even though the Friday is nearer.
    val calendar: HolidayCalendar =
      ImmutableHolidayCalendar.of(HolidayCalendarId.of("Test"), List(MON_2014_07_14), SATURDAY, SUNDAY)

    val expectations: TableFor3[BusinessDayConvention, LocalDate, LocalDate] = Table(
      ("convention", "input", "expected"),
      (BusinessDayConventions.NO_ADJUST, FRI_2014_07_11, FRI_2014_07_11),
      (BusinessDayConventions.NO_ADJUST, SAT_2014_07_12, SAT_2014_07_12),
      (BusinessDayConventions.NO_ADJUST, SUN_2014_07_13, SUN_2014_07_13),
      (BusinessDayConventions.NO_ADJUST, MON_2014_07_14, MON_2014_07_14),
      (BusinessDayConventions.FOLLOWING, FRI_2014_07_11, FRI_2014_07_11),
      (BusinessDayConventions.FOLLOWING, SAT_2014_07_12, TUE_2014_07_15),
      (BusinessDayConventions.FOLLOWING, SUN_2014_07_13, TUE_2014_07_15),
      (BusinessDayConventions.FOLLOWING, MON_2014_07_14, TUE_2014_07_15),
      (BusinessDayConventions.MODIFIED_FOLLOWING, FRI_2014_07_11, FRI_2014_07_11),
      (BusinessDayConventions.MODIFIED_FOLLOWING, SAT_2014_07_12, TUE_2014_07_15),
      (BusinessDayConventions.MODIFIED_FOLLOWING, SUN_2014_07_13, TUE_2014_07_15),
      (BusinessDayConventions.MODIFIED_FOLLOWING, MON_2014_07_14, TUE_2014_07_15),
      (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, FRI_2014_07_11, FRI_2014_07_11),
      (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SAT_2014_07_12, TUE_2014_07_15),
      (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SUN_2014_07_13, TUE_2014_07_15),
      (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, MON_2014_07_14, TUE_2014_07_15),
      (BusinessDayConventions.PRECEDING, FRI_2014_07_11, FRI_2014_07_11),
      (BusinessDayConventions.PRECEDING, SAT_2014_07_12, FRI_2014_07_11),
      (BusinessDayConventions.PRECEDING, SUN_2014_07_13, FRI_2014_07_11),
      (BusinessDayConventions.PRECEDING, MON_2014_07_14, FRI_2014_07_11),
      (BusinessDayConventions.MODIFIED_PRECEDING, FRI_2014_07_11, FRI_2014_07_11),
      (BusinessDayConventions.MODIFIED_PRECEDING, SAT_2014_07_12, FRI_2014_07_11),
      (BusinessDayConventions.MODIFIED_PRECEDING, SUN_2014_07_13, FRI_2014_07_11),
      (BusinessDayConventions.MODIFIED_PRECEDING, MON_2014_07_14, FRI_2014_07_11),
      (BusinessDayConventions.NEAREST, FRI_2014_07_11, FRI_2014_07_11),
      (BusinessDayConventions.NEAREST, SAT_2014_07_12, FRI_2014_07_11),
      (BusinessDayConventions.NEAREST, SUN_2014_07_13, TUE_2014_07_15),
      (BusinessDayConventions.NEAREST, MON_2014_07_14, TUE_2014_07_15)
    )

    forAll(expectations) { (convention: BusinessDayConvention, input: LocalDate, expected: LocalDate) =>
      withClue(s"$convention adjusting $input: ") {
        convention.adjust(input, calendar) shouldBe expected
      }
    }

    // `NoAdjust` is the only member that can answer with a non-business day; every other member
    // answers with a business day of the calendar it was given.
    val nonBusinessDays: List[LocalDate] = List(SAT_2014_07_12, SUN_2014_07_13, MON_2014_07_14)
    BusinessDayConvention.values.toList.filterNot(_ == BusinessDayConventions.NO_ADJUST).foreach {
      convention =>
        nonBusinessDays.foreach { date =>
          withClue(s"$convention adjusting $date: ") {
            calendar.isBusinessDay(convention.adjust(date, calendar)) shouldBe true
          }
        }
    }
  }

  test("test_nearest") {
    // The rule decides from the day of the week of the input before searching: the Saturday goes
    // back to the Friday, the Sunday and the holiday Monday forward to the Tuesday.
    val calendar: HolidayCalendar =
      ImmutableHolidayCalendar.of(HolidayCalendarId.of("Test"), List(MON_2014_07_14), SATURDAY, SUNDAY)

    BusinessDayConventions.NEAREST.adjust(FRI_2014_07_11, calendar) shouldBe FRI_2014_07_11
    BusinessDayConventions.NEAREST.adjust(SAT_2014_07_12, calendar) shouldBe FRI_2014_07_11
    BusinessDayConventions.NEAREST.adjust(SUN_2014_07_13, calendar) shouldBe TUE_2014_07_15
    BusinessDayConventions.NEAREST.adjust(MON_2014_07_14, calendar) shouldBe TUE_2014_07_15
  }

  //-------------------------------------------------------------------------
  test("test_name") {
    forAll(dataName) { (convention: BusinessDayConvention, name: String) =>
      convention.name shouldBe name
    }
  }

  test("test_toString") {
    forAll(dataName) { (convention: BusinessDayConvention, name: String) =>
      convention.toString shouldBe name
    }
  }

  test("test_of_lookup") {
    // The canonical name and its upper-case spelling are the two keys each member is registered
    // under, and the lenient lookup accepts everything the exact one does.
    forAll(dataName) { (convention: BusinessDayConvention, name: String) =>
      withClue(s"$name: ") {
        BusinessDayConvention.valueOf(name) shouldBe Some(convention)
        BusinessDayConvention.parse(name) should haveValue(convention)

        val upperCase = name.toUpperCase(Locale.ENGLISH)
        BusinessDayConvention.valueOf(upperCase) shouldBe Some(convention)
        BusinessDayConvention.parse(upperCase) should haveValue(convention)
      }
    }
  }

  test("test_lenientLookup_standardNames") {
    // A lower-case name is deliberately asserted to be outside the exact key space: it resolves
    // through the leniency and not through a third key registered for each member.
    forAll(dataName) { (convention: BusinessDayConvention, name: String) =>
      val lowerCase = name.toLowerCase(Locale.ENGLISH)
      withClue(s"$lowerCase: ") {
        BusinessDayConvention.parse(lowerCase) should haveValue(convention)
        BusinessDayConvention.valueOf(lowerCase) shouldBe None
      }
    }
  }

  test("test_extendedEnum") {
    // `byCanonicalName` is the canonical-key map, 7 keys, and `byUpperName` the upper-case one;
    // their union is the 14-key space the exact lookup answers over, asserted by its exact keys.
    val lookup = BusinessDayConvention.namedEnum

    forAll(dataName) { (convention: BusinessDayConvention, name: String) =>
      withClue(s"$name: ")(lookup.byCanonicalName.get(name) shouldBe Some(convention))
    }

    lookup.familyName shouldBe "BusinessDayConvention"
    lookup.byCanonicalName should have size 7
    lookup.byCanonicalName.keySet shouldBe
      Set(
        "NoAdjust",
        "Following",
        "ModifiedFollowing",
        "ModifiedFollowingBiMonthly",
        "Preceding",
        "ModifiedPreceding",
        "Nearest")

    (lookup.byCanonicalName.keySet ++ lookup.byUpperName.keySet) shouldBe
      Set(
        "NoAdjust",
        "NOADJUST",
        "Following",
        "FOLLOWING",
        "ModifiedFollowing",
        "MODIFIEDFOLLOWING",
        "ModifiedFollowingBiMonthly",
        "MODIFIEDFOLLOWINGBIMONTHLY",
        "Preceding",
        "PRECEDING",
        "ModifiedPreceding",
        "MODIFIEDPRECEDING",
        "Nearest",
        "NEAREST")
    (lookup.byCanonicalName.keySet ++ lookup.byUpperName.keySet) should have size 14
    lookup.byUpperName should have size 7

    // This family declares no alternate spelling, so everything beyond those 14 keys arrives
    // through the lenient chain or through an external group.
    lookup.alternateNames shouldBe Map.empty[String, String]

    // The two groups of external spellings, asserted row for row: they take part in no lookup,
    // so a lost row would otherwise be invisible.
    lookup.externalNameGroups shouldBe Set("FpML", "SWIFT")
    lookup.externalNames("FpML") shouldBe
      Some(
        Map(
          "NONE" -> BusinessDayConventions.NO_ADJUST,
          "FOLLOWING" -> BusinessDayConventions.FOLLOWING,
          "MODFOLLOWING" -> BusinessDayConventions.MODIFIED_FOLLOWING,
          "PRECEDING" -> BusinessDayConventions.PRECEDING,
          "NEAREST" -> BusinessDayConventions.NEAREST))
    lookup.externalNames("SWIFT") shouldBe
      Some(
        Map(
          "FOLLOWING" -> BusinessDayConventions.FOLLOWING,
          "MODIFIEDF" -> BusinessDayConventions.MODIFIED_FOLLOWING,
          "PRECEDING" -> BusinessDayConventions.PRECEDING))

    // The raw tables hold the same keys as text, so neither names a member the family lacks.
    lookup.externalNamesRaw("FpML").map(_.keySet) shouldBe lookup.externalNames("FpML").map(_.keySet)
    lookup.externalNamesRaw("SWIFT").map(_.keySet) shouldBe lookup.externalNames("SWIFT").map(_.keySet)
    lookup.externalNamesRaw("FpML").map(_("NONE")) shouldBe Some("NoAdjust")
    lookup.externalNamesRaw("SWIFT").map(_("MODIFIEDF")) shouldBe Some("ModifiedFollowing")

    // A group the family does not publish answers with nothing rather than failing.
    lookup.externalNames("Rubbish") shouldBe None
    lookup.externalNamesRaw("Rubbish") shouldBe None

    // The lenient chain is ordered and each pattern sees what the previous one produced, so the
    // row order is part of the data: a reordering changes what resolves.
    lookup.lenientPatterns should have size 11
    lookup.lenientPatterns.map { case (_, replacement) => replacement } shouldBe
      List(
        "Following",
        "Following",
        "ModifiedFollowing",
        "ModifiedFollowing",
        "ModifiedFollowing",
        "Preceding",
        "ModifiedPreceding",
        "ModifiedPreceding",
        "ModifiedFollowingBiMonthly",
        "NoAdjust",
        "NoAdjust")

    // `MODIFIEDF` is a spelling no lenient pattern accepts, so the SWIFT table is the only route
    // from it to a convention.
    BusinessDayConvention.valueOf("MODIFIEDF") shouldBe None
    BusinessDayConvention.parse("MODIFIEDF") should beFailureWith(FailureReason.PARSING)
    lookup.externalNames("SWIFT").flatMap(_.get("MODIFIEDF")) shouldBe
      Some(BusinessDayConventions.MODIFIED_FOLLOWING)

    lookup.values.toList shouldBe declarationOrder
    lookup.toString shouldBe "NamedEnum[BusinessDayConvention]"
  }

  test("test_of_lookup_notFound") {
    // Text naming no member is reported as a `PARSING` failure whose message names the family
    // and the text, rather than raised.
    BusinessDayConvention.valueOf("Rubbish") shouldBe None
    BusinessDayConvention.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
    BusinessDayConvention.parse("Rubbish") should
      haveFailureMessageMatching("BusinessDayConvention name not found: Rubbish")
  }

  test("test_of_lookup_null") {
    // The name is a required parameter, which these two proofs state.
    assertDoesNotCompile("BusinessDayConvention.parse()")
    assertDoesNotCompile("BusinessDayConvention.valueOf()")

    // The rest of the case is hostile text - empty, blank and near-misses - each of which
    // resolves to nothing and raises nothing.
    val hostile: List[String] =
      List(
        "",
        "   ",
        "\t\n",
        "Following ",
        " Following",
        "Follow ing",
        "null",
        "None ",
        "MODIFIED__FOLLOWING",
        "Following+Preceding")

    hostile.foreach { name =>
      withClue(s"[$name]: ") {
        BusinessDayConvention.valueOf(name) shouldBe None
        BusinessDayConvention.parse(name) should beFailureWith(FailureReason.PARSING)
        noException should be thrownBy BusinessDayConvention.parse(name)
      }
    }
  }

  //-------------------------------------------------------------------------
  test("test_lenientLookup_specialNames") {
    // Each row is asserted in three spellings - as written, folded down and folded up - because
    // the input is folded to upper case before the patterns are applied.
    forAll(dataLenient) { (name: String, convention: BusinessDayConvention) =>
      withClue(s"$name: ") {
        BusinessDayConvention.parse(name.toLowerCase(Locale.ENGLISH)) should haveValue(convention)
        BusinessDayConvention.parse(name) should haveValue(convention)
        BusinessDayConvention.parse(name.toUpperCase(Locale.ENGLISH)) should haveValue(convention)
      }
    }
  }

  test("test_lenientLookup_constants") {
    forAll(dataConstantIdentifiers) { (identifier: String, convention: BusinessDayConvention) =>
      withClue(s"$identifier: ") {
        BusinessDayConvention.parse(identifier) should haveValue(convention)
        BusinessDayConvention.parse(identifier.toLowerCase(Locale.ENGLISH)) should haveValue(convention)
      }
    }
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // `values` is in declaration order, not the alphabetical order `Order` imposes.
    BusinessDayConvention.values.toList shouldBe declarationOrder
    BusinessDayConvention.values.toList should have size 7
    BusinessDayConvention.values.toList.distinct should have size 7
    BusinessDayConvention.values.toList.map(_.name).distinct should have size 7

    // Each constant of the holder is the member itself, not a copy.
    BusinessDayConventions.NO_ADJUST should be theSameInstanceAs BusinessDayConvention.NoAdjust
    BusinessDayConventions.FOLLOWING should be theSameInstanceAs BusinessDayConvention.Following
    BusinessDayConventions.MODIFIED_FOLLOWING should be theSameInstanceAs
      BusinessDayConvention.ModifiedFollowing
    BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY should be theSameInstanceAs
      BusinessDayConvention.ModifiedFollowingBiMonthly
    BusinessDayConventions.PRECEDING should be theSameInstanceAs BusinessDayConvention.Preceding
    BusinessDayConventions.MODIFIED_PRECEDING should be theSameInstanceAs
      BusinessDayConvention.ModifiedPreceding
    BusinessDayConventions.NEAREST should be theSameInstanceAs BusinessDayConvention.Nearest

    // The holder publishes exactly those seven.
    val constants: List[BusinessDayConvention] =
      List(
        BusinessDayConventions.NO_ADJUST,
        BusinessDayConventions.FOLLOWING,
        BusinessDayConventions.MODIFIED_FOLLOWING,
        BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY,
        BusinessDayConventions.PRECEDING,
        BusinessDayConventions.MODIFIED_PRECEDING,
        BusinessDayConventions.NEAREST)
    constants shouldBe declarationOrder

    BusinessDayConvention.values.toList.foreach { convention =>
      withClue(s"${convention.name}: ") {
        BusinessDayConvention.valueOf(convention.name) shouldBe Some(convention)
        BusinessDayConvention.parse(convention.name) should haveValue(convention)
        Show[BusinessDayConvention].show(convention) shouldBe convention.name
        convention.toString shouldBe convention.name
      }
    }

    // The companion publishes a single equality-bearing instance - an ordering that is also a
    // hashing - and every ordered pair of members is checked through all three: equal values
    // hash equally and compare 0, unequal members compare non-zero.
    val all: List[BusinessDayConvention] = BusinessDayConvention.values.toList
    for (left <- all; right <- all) {
      val sameValue = left == right
      withClue(s"${left.name} against ${right.name}: ") {
        Eq[BusinessDayConvention].eqv(left, right) shouldBe sameValue
        Hash[BusinessDayConvention].eqv(left, right) shouldBe sameValue
        (Order[BusinessDayConvention].compare(left, right) == 0) shouldBe sameValue
        if (sameValue) {
          Hash[BusinessDayConvention].hash(left) shouldBe Hash[BusinessDayConvention].hash(right)
        } else {
          Order[BusinessDayConvention].compare(left, right) should not be 0
        }
      }
    }

    all.sorted(Order[BusinessDayConvention].toOrdering).map(_.name) shouldBe
      all.map(_.name).sorted
  }

  test("test_serialization") {
    // The document is the bare canonical name and never an object, so a document from either
    // side names the same convention.
    val noAdjust: Json = BusinessDayConventions.NO_ADJUST.asJson
    noAdjust.isString shouldBe true
    noAdjust.isObject shouldBe false
    noAdjust shouldBe Json.fromString("NoAdjust")
    noAdjust.as[BusinessDayConvention] shouldBe Right(BusinessDayConventions.NO_ADJUST)

    forAll(dataName) { (convention: BusinessDayConvention, name: String) =>
      withClue(s"$name: ") {
        val encoded: Json = convention.asJson
        encoded shouldBe Json.fromString(name)
        encoded.isString shouldBe true
        encoded.isObject shouldBe false
        encoded.noSpaces shouldBe s""""$name""""
        encoded.as[BusinessDayConvention] shouldBe Right(convention)
      }
    }

    // A name no convention answers to is rejected, as is a document of the wrong JSON type: the
    // codec reads a string and nothing else, and both are decoding failures, not exceptions.
    Json.fromString("Rubbish").as[BusinessDayConvention].isLeft shouldBe true
    Json.fromInt(1).as[BusinessDayConvention].isLeft shouldBe true
    Json.obj("name" -> Json.fromString("NoAdjust")).as[BusinessDayConvention].isLeft shouldBe true

    // The decoder is as lenient as `parse`, which is what lets a hand-written name or an
    // external spelling still be read.
    Json.fromString("MODFOLLOWING").as[BusinessDayConvention] shouldBe
      Right(BusinessDayConventions.MODIFIED_FOLLOWING)
  }

  test("test_jodaConvert") {
    // `name`, `toString` and `Show` render the same text, and `parse` reads that text back as
    // the same convention.
    forAll(dataName) { (convention: BusinessDayConvention, name: String) =>
      withClue(s"$name: ") {
        convention.name shouldBe name
        convention.toString shouldBe name
        Show[BusinessDayConvention].show(convention) shouldBe name
        BusinessDayConvention.parse(Show[BusinessDayConvention].show(convention)) should
          haveValue(convention)
      }
    }

    Show[BusinessDayConvention].show(BusinessDayConventions.NO_ADJUST) shouldBe "NoAdjust"
    BusinessDayConvention.parse("NoAdjust") should haveValue(BusinessDayConventions.NO_ADJUST)
    Show[BusinessDayConvention].show(BusinessDayConventions.MODIFIED_FOLLOWING) shouldBe
      "ModifiedFollowing"
    BusinessDayConvention.parse("ModifiedFollowing") should
      haveValue(BusinessDayConventions.MODIFIED_FOLLOWING)
  }
}

/**
 * The date fixtures of this spec and the 72-row convention table it is driven from.
 *
 * Visible within the `date` test package alone, because it is a fixture and not a published
 * surface.
 */
private[date] object BusinessDayConventionSpec extends TableDrivenPropertyChecks {

  val FRI_2014_07_11: LocalDate = LocalDate.of(2014, 7, 11)
  val SAT_2014_07_12: LocalDate = LocalDate.of(2014, 7, 12)
  val SUN_2014_07_13: LocalDate = LocalDate.of(2014, 7, 13)
  val MON_2014_07_14: LocalDate = LocalDate.of(2014, 7, 14)
  val TUE_2014_07_15: LocalDate = LocalDate.of(2014, 7, 15)

  val FRI_2014_08_29: LocalDate = LocalDate.of(2014, 8, 29)
  val SAT_2014_08_30: LocalDate = LocalDate.of(2014, 8, 30)
  val SUN_2014_08_31: LocalDate = LocalDate.of(2014, 8, 31)
  val MON_2014_09_01: LocalDate = LocalDate.of(2014, 9, 1)

  val FRI_2014_10_31: LocalDate = LocalDate.of(2014, 10, 31)
  val SAT_2014_11_01: LocalDate = LocalDate.of(2014, 11, 1)
  val SUN_2014_11_02: LocalDate = LocalDate.of(2014, 11, 2)
  val MON_2014_11_03: LocalDate = LocalDate.of(2014, 11, 3)

  val FRI_2014_11_14: LocalDate = LocalDate.of(2014, 11, 14)
  val SAT_2014_11_15: LocalDate = LocalDate.of(2014, 11, 15)
  val SUN_2014_11_16: LocalDate = LocalDate.of(2014, 11, 16)
  val MON_2014_11_17: LocalDate = LocalDate.of(2014, 11, 17)

  /**
   * Each convention with an input date and the date it adjusts to against a `Sat/Sun` calendar.
   *
   * The rows cross every boundary a convention can be held back by: a weekend inside a month, a
   * weekend spanning a month end in both directions, and - for the bi-monthly convention - a
   * weekend spanning the 15th. The rows marked `modified` are where those reverse direction.
   */
  val dataConvention: TableFor3[BusinessDayConvention, LocalDate, LocalDate] = Table(
    ("convention", "input", "expected"),
    (BusinessDayConventions.NO_ADJUST, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.NO_ADJUST, SAT_2014_07_12, SAT_2014_07_12),
    (BusinessDayConventions.NO_ADJUST, SUN_2014_07_13, SUN_2014_07_13),
    (BusinessDayConventions.NO_ADJUST, MON_2014_07_14, MON_2014_07_14),
    (BusinessDayConventions.FOLLOWING, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.FOLLOWING, SAT_2014_07_12, MON_2014_07_14),
    (BusinessDayConventions.FOLLOWING, SUN_2014_07_13, MON_2014_07_14),
    (BusinessDayConventions.FOLLOWING, MON_2014_07_14, MON_2014_07_14),
    (BusinessDayConventions.FOLLOWING, FRI_2014_08_29, FRI_2014_08_29),
    (BusinessDayConventions.FOLLOWING, SAT_2014_08_30, MON_2014_09_01),
    (BusinessDayConventions.FOLLOWING, SUN_2014_08_31, MON_2014_09_01),
    (BusinessDayConventions.FOLLOWING, MON_2014_09_01, MON_2014_09_01),
    (BusinessDayConventions.FOLLOWING, FRI_2014_10_31, FRI_2014_10_31),
    (BusinessDayConventions.FOLLOWING, SAT_2014_11_01, MON_2014_11_03),
    (BusinessDayConventions.FOLLOWING, SUN_2014_11_02, MON_2014_11_03),
    (BusinessDayConventions.FOLLOWING, MON_2014_11_03, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.MODIFIED_FOLLOWING, SAT_2014_07_12, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_FOLLOWING, SUN_2014_07_13, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_FOLLOWING, MON_2014_07_14, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_FOLLOWING, FRI_2014_08_29, FRI_2014_08_29),
    // modified: the next business day would cross into September
    (BusinessDayConventions.MODIFIED_FOLLOWING, SAT_2014_08_30, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_FOLLOWING, SUN_2014_08_31, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_FOLLOWING, MON_2014_09_01, MON_2014_09_01),
    (BusinessDayConventions.MODIFIED_FOLLOWING, FRI_2014_10_31, FRI_2014_10_31),
    (BusinessDayConventions.MODIFIED_FOLLOWING, SAT_2014_11_01, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING, SUN_2014_11_02, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING, MON_2014_11_03, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SAT_2014_07_12, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SUN_2014_07_13, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, MON_2014_07_14, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, FRI_2014_08_29, FRI_2014_08_29),
    // modified: the next business day would cross into September
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SAT_2014_08_30, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SUN_2014_08_31, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, MON_2014_09_01, MON_2014_09_01),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, FRI_2014_10_31, FRI_2014_10_31),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SAT_2014_11_01, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SUN_2014_11_02, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, MON_2014_11_03, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, FRI_2014_11_14, FRI_2014_11_14),
    // modified: the next business day would cross the 15th, out of the first half-month
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SAT_2014_11_15, FRI_2014_11_14),
    // and not modified the other way round: a date in the second half-month is only ever held
    // back by the month end
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, SUN_2014_11_16, MON_2014_11_17),
    (BusinessDayConventions.MODIFIED_FOLLOWING_BI_MONTHLY, MON_2014_11_17, MON_2014_11_17),
    (BusinessDayConventions.PRECEDING, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.PRECEDING, SAT_2014_07_12, FRI_2014_07_11),
    (BusinessDayConventions.PRECEDING, SUN_2014_07_13, FRI_2014_07_11),
    (BusinessDayConventions.PRECEDING, MON_2014_07_14, MON_2014_07_14),
    (BusinessDayConventions.PRECEDING, FRI_2014_08_29, FRI_2014_08_29),
    (BusinessDayConventions.PRECEDING, SAT_2014_08_30, FRI_2014_08_29),
    (BusinessDayConventions.PRECEDING, SUN_2014_08_31, FRI_2014_08_29),
    (BusinessDayConventions.PRECEDING, MON_2014_09_01, MON_2014_09_01),
    (BusinessDayConventions.PRECEDING, FRI_2014_10_31, FRI_2014_10_31),
    (BusinessDayConventions.PRECEDING, SAT_2014_11_01, FRI_2014_10_31),
    (BusinessDayConventions.PRECEDING, SUN_2014_11_02, FRI_2014_10_31),
    (BusinessDayConventions.PRECEDING, MON_2014_11_03, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_PRECEDING, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.MODIFIED_PRECEDING, SAT_2014_07_12, FRI_2014_07_11),
    (BusinessDayConventions.MODIFIED_PRECEDING, SUN_2014_07_13, FRI_2014_07_11),
    (BusinessDayConventions.MODIFIED_PRECEDING, MON_2014_07_14, MON_2014_07_14),
    (BusinessDayConventions.MODIFIED_PRECEDING, FRI_2014_08_29, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_PRECEDING, SAT_2014_08_30, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_PRECEDING, SUN_2014_08_31, FRI_2014_08_29),
    (BusinessDayConventions.MODIFIED_PRECEDING, MON_2014_09_01, MON_2014_09_01),
    (BusinessDayConventions.MODIFIED_PRECEDING, FRI_2014_10_31, FRI_2014_10_31),
    // modified: the previous business day would cross back into October
    (BusinessDayConventions.MODIFIED_PRECEDING, SAT_2014_11_01, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_PRECEDING, SUN_2014_11_02, MON_2014_11_03),
    (BusinessDayConventions.MODIFIED_PRECEDING, MON_2014_11_03, MON_2014_11_03),
    (BusinessDayConventions.NEAREST, FRI_2014_07_11, FRI_2014_07_11),
    (BusinessDayConventions.NEAREST, SAT_2014_07_12, FRI_2014_07_11),
    (BusinessDayConventions.NEAREST, SUN_2014_07_13, MON_2014_07_14),
    (BusinessDayConventions.NEAREST, MON_2014_07_14, MON_2014_07_14)
  )
}
