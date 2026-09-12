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
 * Test [[BusinessDayConvention]], ported from the Java `BusinessDayConventionTest`.
 *
 * Every method of the Java class is kept under the name it had, so the method-level
 * traceability recorded in `manifest/java-test-mapping.csv` stays one-to-one:
 * `test_convention`, `test_nearest`, `test_name`, `test_toString`, `test_of_lookup`,
 * `test_lenientLookup_standardNames`, `test_extendedEnum`, `test_of_lookup_notFound`,
 * `test_of_lookup_null`, `test_lenientLookup_specialNames`, `test_lenientLookup_constants`,
 * `coverage` and `test_jodaConvert`. The Java `test_serialization` is deliberately absent: the
 * manifest consolidates it into `json.JsonRoundTripSpec`, and what belongs here about the text
 * and JSON forms is asserted by `test_jodaConvert`.
 *
 * The three Java data providers are transcribed in full rather than sampled, and each becomes
 * one table shared by the tests that were driven from it:
 *
 *   - `data_convention` - '''72''' rows, driven against `Sat/Sun`;
 *   - `data_name` - 7 rows, driven by `test_name`, `test_toString`, `test_of_lookup`,
 *     `test_lenientLookup_standardNames` and `test_extendedEnum`;
 *   - `data_lenient` - '''27''' rows.
 *
 * ===How the shape of the port changes the assertions===
 *
 *   - The throwing `BusinessDayConvention.of(name)` became two members: [[
 *     BusinessDayConvention.valueOf]], the exact lookup over the canonical and upper-case keys,
 *     returning an `Option`, and [[BusinessDayConvention.parse]], the lenient lookup, returning
 *     `EitherNec[Failure, _]`. Both are asserted wherever Java asserted `of`, so the two entry
 *     points cannot drift apart.
 *   - `extendedEnum()` - the registry the Java type built by reading a configuration resource
 *     off the class path - became the `NamedEnum[BusinessDayConvention]` instance the companion
 *     publishes. `test_extendedEnum` asserts the canonical map the Java method read
 *     (`lookupAll`), states the 14-key union of canonical and upper-case names explicitly so
 *     the Java relationship stays visible, and asserts the two external groups and the lenient
 *     table the resource declared, because in this port they are code and a lost row would
 *     otherwise be invisible.
 *   - `test_of_lookup_null` passed the absent reference to the factory and asserted a throw.
 *     There is no null in this API and no factory that throws, so the case is asserted as the
 *     hostile text a caller can actually supply - empty, blank, and several near-misses - each
 *     of which answers `None`/`Left(PARSING)` and never raises.
 *   - `test_lenientLookup_constants` reflected over the public constants of
 *     `BusinessDayConventions` to check that each identifier resolves leniently. This port
 *     performs no reflection, so the seven identifiers are written out as a table; the constants
 *     themselves are checked against the members in `coverage`.
 *   - `coverage` called `coverPrivateConstructor` and `coverEnum`, which existed only to satisfy
 *     a coverage tool by reflectively touching a private constructor and the values of a Java
 *     enum. Neither has a target here, so what they stood for is asserted directly: the seven
 *     constants are the members, `values` is in Java declaration order, and the typeclass
 *     instances agree with each other.
 *   - `test_jodaConvert` asserted a round trip through the reflective string-conversion library
 *     the Java type was annotated for. That library is not a dependency of this port, so the
 *     guarantee it gave is asserted over the two text forms this port does have: the name, which
 *     `Show` and `toString` agree on, and the JSON codec, which writes the bare canonical name.
 *
 * @see [[BusinessDayAdjustmentSpec]] for the same conventions applied through reference data
 */
class BusinessDayConventionSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  import BusinessDayConventionSpec._

  /** The Java `data_name` provider: each convention with the name it renders as. */
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
   * The Java `data_lenient` provider, all 27 rows, in the order the provider listed them.
   *
   * Every row is a spelling that the ordered chain of lenient rewrites turns into a canonical
   * name. The rows exercise the abbreviations, the screaming-snake spellings of the constant
   * identifiers, and the spaced and hyphen-free spellings of a name - so a reordering of the
   * transcribed pattern list, which would change what resolves, is caught here.
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
   * The identifiers of the constants of [[BusinessDayConventions]], with the convention each
   * names.
   *
   * This table replaces the reflective sweep of the Java `test_lenientLookup_constants`, which
   * read the public static final fields of the constants holder and asserted that each field
   * '''name''' resolved leniently to the field's value. The identifiers are written out because
   * this port performs no reflection; they are the seven the holder declares, in declaration
   * order, and `coverage` asserts that no eighth constant exists.
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

  /** The seven conventions in the declaration order of the enum being ported. */
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
    // The Java parameterised method, driven from the whole provider against the same calendar
    // it used. `adjust` takes a calendar rather than reference data, so this is the pure rule
    // of each convention with no lookup involved.
    forAll(dataConvention) { (convention: BusinessDayConvention, input: LocalDate, expected: LocalDate) =>
      withClue(s"$convention adjusting $input: ") {
        convention.adjust(input, HolidayCalendars.SAT_SUN) shouldBe expected
      }
    }
  }

  test("test_convention_againstHolidayCalendar") {
    // Beyond the Java provider, which uses a weekend-only calendar throughout: every convention
    // against a calendar that also holds a holiday, so that a run of non-business days has to be
    // walked rather than a single weekend. The expected values are those of the Java rules
    // applied to this calendar, and they are what makes the direction each convention chooses
    // observable - in particular `Nearest`, which decides from the day of the week of the input
    // before searching, so Sunday and the holiday Monday both move forward past the Monday.
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

    // Every convention other than `NoAdjust` answers with a business day of the calendar it was
    // given; `NoAdjust` is the one member that can return a holiday, which the rows above show
    // for all three non-business days.
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
    // Transcribed from the Java method, including the calendar it built: a single holiday on
    // Monday, with the Saturday/Sunday weekend. The Sunday and the Monday move forward to the
    // Tuesday even though the Friday is nearer, which is the documented behaviour of the rule.
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
    // The Java `of` was one throwing factory. Both of the members that replaced it are asserted
    // here: the exact lookup, which answers for the canonical name and for its upper-case
    // spelling because those are the two keys each member is registered under, and the lenient
    // one, which accepts everything the exact one does.
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
    // The Java method asked the registry for the lower-case spelling of each canonical name,
    // through `findLenient`. Here that is `parse`, and the lower-case spelling is deliberately
    // asserted to be outside the exact key space: it resolves because of the leniency and not
    // because the family registered a third key for each member.
    forAll(dataName) { (convention: BusinessDayConvention, name: String) =>
      val lowerCase = name.toLowerCase(Locale.ENGLISH)
      withClue(s"$lowerCase: ") {
        BusinessDayConvention.parse(lowerCase) should haveValue(convention)
        BusinessDayConvention.valueOf(lowerCase) shouldBe None
      }
    }
  }

  test("test_extendedEnum") {
    // The Java method read `extendedEnum().lookupAll()` and looked each canonical name up in it.
    // The counterpart of that map here is `byCanonicalName`, which is the Java
    // `lookupAllNormalized` - the 7 canonical keys - while the Java `lookupAll` was the 14-key
    // union of those with the upper-case spellings. Both are asserted, the union by its exact
    // key set, so the relationship to the Java registry stays readable.
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

    // The Java `lookupAll` key set, transcribed from the registry: every canonical name and the
    // English upper-case of it, 14 keys for 7 members.
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

    // This family declares no alternate spelling, because the configuration resource being
    // transcribed declared none for it: everything beyond the 14 keys arrives through the
    // lenient chain or through an external group.
    lookup.alternateNames shouldBe Map.empty[String, String]

    // The two groups of external spellings the resource published, asserted row for row. They
    // take part in no lookup, which is why a lost row would otherwise be invisible.
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

    // The raw tables hold the same rows as text, and every row resolves - so neither group names
    // a member this family does not have, which is what comparing the two key sets establishes.
    lookup.externalNamesRaw("FpML").map(_.keySet) shouldBe lookup.externalNames("FpML").map(_.keySet)
    lookup.externalNamesRaw("SWIFT").map(_.keySet) shouldBe lookup.externalNames("SWIFT").map(_.keySet)
    lookup.externalNamesRaw("FpML").map(_("NONE")) shouldBe Some("NoAdjust")
    lookup.externalNamesRaw("SWIFT").map(_("MODIFIEDF")) shouldBe Some("ModifiedFollowing")

    // A group the family does not publish is an empty answer rather than a failure.
    lookup.externalNames("Rubbish") shouldBe None
    lookup.externalNamesRaw("Rubbish") shouldBe None

    // The ordered lenient table, whose 11 rows are the rows of the resource. The order is part
    // of the data - a later pattern sees what an earlier one produced - so the count is asserted
    // alongside the rows that `test_lenientLookup_specialNames` drives through it.
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

    // The lookup is the family's own and covers exactly its members.
    lookup.values.toList shouldBe declarationOrder
    lookup.toString shouldBe "NamedEnum[BusinessDayConvention]"
  }

  test("test_lenientLookup_swiftOnlySpelling") {
    // Beyond the Java methods, and the sharpest statement about the SWIFT group: `MODIFIEDF` is
    // a spelling no lenient pattern accepts, so the external table is the only route from it to
    // a convention. Java behaves identically - `findLenient("MODIFIEDF")` is empty there too -
    // which is why it appears in no lenient row.
    BusinessDayConvention.valueOf("MODIFIEDF") shouldBe None
    BusinessDayConvention.parse("MODIFIEDF") should beFailureWith(FailureReason.PARSING)
    BusinessDayConvention.namedEnum.externalNames("SWIFT").flatMap(_.get("MODIFIEDF")) shouldBe
      Some(BusinessDayConventions.MODIFIED_FOLLOWING)
  }

  test("test_of_lookup_notFound") {
    // Where the Java factory raised an error for text naming no member, the port reports it as a
    // value. The reason is compared as a member of the closed family of reasons, and the message
    // is asserted once because it names the family and the text - the two things a caller has to
    // be told.
    BusinessDayConvention.valueOf("Rubbish") shouldBe None
    BusinessDayConvention.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
    BusinessDayConvention.parse("Rubbish") should
      haveFailureMessageMatching("BusinessDayConvention name not found: Rubbish")
  }

  test("test_of_lookup_null") {
    // Reinterpretation: the Java method passed the absent reference to the factory and asserted
    // that it raised an error. This port writes no null and its two lookups answer with a value,
    // so what is asserted is that every spelling of "no usable name" a caller can actually
    // supply resolves to nothing and raises nothing.
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
    // The Java method drove every row of `data_lenient` through `findLenient` after folding it
    // to lower case. `parse` is that method here, and each row is asserted in three spellings -
    // as written, folded down and folded up - because the patterns are matched insensitively to
    // case and the input is folded to upper case before they are applied.
    forAll(dataLenient) { (name: String, convention: BusinessDayConvention) =>
      withClue(s"$name: ") {
        BusinessDayConvention.parse(name.toLowerCase(Locale.ENGLISH)) should haveValue(convention)
        BusinessDayConvention.parse(name) should haveValue(convention)
        BusinessDayConvention.parse(name.toUpperCase(Locale.ENGLISH)) should haveValue(convention)
      }
    }
  }

  test("test_lenientLookup_constants") {
    // The port of the reflective sweep over the constants holder: each identifier resolves
    // leniently to the constant it names, in its own spelling and folded to lower case, exactly
    // as the Java method asserted for the field names it discovered.
    forAll(dataConstantIdentifiers) { (identifier: String, convention: BusinessDayConvention) =>
      withClue(s"$identifier: ") {
        BusinessDayConvention.parse(identifier) should haveValue(convention)
        BusinessDayConvention.parse(identifier.toLowerCase(Locale.ENGLISH)) should haveValue(convention)
      }
    }
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java method was `coverPrivateConstructor(BusinessDayConventions.class)` and
    // `coverEnum(StandardBusinessDayConventions.class)`: the first reflectively invoked the
    // private constructor of a static holder, the second read the values of a package-private
    // enum, both so that a coverage tool would not report them as unexercised. A Scala `object`
    // has no constructor to reach and there is no second enum - the members are declared in the
    // companion - so what those calls stood for is asserted directly.

    // The family has exactly seven members, in the declaration order of the enum being ported,
    // which is not the alphabetical order the `Order` instance imposes.
    BusinessDayConvention.values.toList shouldBe declarationOrder
    BusinessDayConvention.values.toList should have size 7
    BusinessDayConvention.values.toList.distinct should have size 7
    BusinessDayConvention.values.toList.map(_.name).distinct should have size 7

    // Each constant of the holder is the very member of the family, not a copy or a registry
    // indirection, so a call site reading the constant and one reading the member are the same.
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

    // The holder publishes those seven and nothing else, which is the other half of what the
    // reflective sweep would have discovered.
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

    // Every member is reachable by its own name through both entry points, so the name space and
    // the membership agree.
    BusinessDayConvention.values.toList.foreach { convention =>
      withClue(s"${convention.name}: ") {
        BusinessDayConvention.valueOf(convention.name) shouldBe Some(convention)
        BusinessDayConvention.parse(convention.name) should haveValue(convention)
        Show[BusinessDayConvention].show(convention) shouldBe convention.name
        convention.toString shouldBe convention.name
      }
    }

    // The companion publishes one equality-bearing instance - an ordering that is also a hashing
    // - so summoning the equality, the hashing or the ordering yields that one value and the
    // three can never disagree. Every ordered pair of members is asked all three questions.
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

    // And the ordering is by name, which is alphabetical rather than the declaration order above.
    all.sorted(Order[BusinessDayConvention].toOrdering).map(_.name) shouldBe
      all.map(_.name).sorted
  }

  test("test_jodaConvert") {
    // The Java method asserted a round trip through the reflective string-conversion library the
    // type was annotated for. That library is not a dependency of this port, so its guarantee -
    // a convention renders as one string and that string reads back as the same convention - is
    // asserted over the two text forms this port has: the name, which `Show` and `toString`
    // agree on, and the JSON codec, which writes the bare canonical name and never an object.
    forAll(dataName) { (convention: BusinessDayConvention, name: String) =>
      withClue(s"$name: ") {
        convention.name shouldBe name
        convention.toString shouldBe name
        Show[BusinessDayConvention].show(convention) shouldBe name
        BusinessDayConvention.parse(name) should haveValue(convention)

        val encoded: Json = convention.asJson
        encoded shouldBe Json.fromString(name)
        encoded.isString shouldBe true
        encoded.as[BusinessDayConvention] shouldBe Right(convention)
      }
    }

    // The two conventions the Java method named explicitly, asserted as concrete documents.
    BusinessDayConventions.NO_ADJUST.asJson.noSpaces shouldBe "\"NoAdjust\""
    BusinessDayConventions.MODIFIED_FOLLOWING.asJson.noSpaces shouldBe "\"ModifiedFollowing\""

    // Text that names no convention is rejected by the reader, as is a document of the wrong
    // JSON type - the codec reads a string and nothing else.
    Json.fromString("Rubbish").as[BusinessDayConvention].isLeft shouldBe true
    Json.fromInt(1).as[BusinessDayConvention].isLeft shouldBe true
    Json.obj("name" -> Json.fromString("NoAdjust")).as[BusinessDayConvention].isLeft shouldBe true

    // The decoder is as lenient as `parse`, which is what lets a document written by hand, or by
    // the library being ported through one of its external vocabularies, still be read.
    Json.fromString("MODFOLLOWING").as[BusinessDayConvention] shouldBe
      Right(BusinessDayConventions.MODIFIED_FOLLOWING)
  }
}

/**
 * The shared data providers of this spec, held where a sibling spec can read them.
 *
 * The Java class declared its providers as public static methods, and
 * `BusinessDayAdjustmentTest.test_adjustDate` was driven from `data_convention` through a
 * `@MethodSource` naming this very class. The relationship is preserved here rather than
 * duplicated: the provider and the dates it is written from live in this companion, and
 * [[BusinessDayAdjustmentSpec]] drives its own `test_adjustDate` from the same rows, so the two
 * specs can never assert against different tables.
 *
 * Visible within the `date` test package alone, because it is a fixture of these two specs and
 * not a published surface.
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
   * The Java `data_convention` provider, all 72 rows, in the order the provider listed them.
   *
   * The rows cross every boundary the family can be held back by: a weekend inside a month, a
   * weekend spanning a month end in both directions, and - for the bi-monthly convention - a
   * weekend spanning the 15th. The rows commented `modified` are the ones where the modified
   * conventions reverse direction, and they are the reason the whole provider is transcribed
   * rather than sampled.
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
    // back by the month end, which is the asymmetry the Java implementation had
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
