/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import java.util.Locale

import scala.util.matching.Regex

import cats.data.NonEmptyChain

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor1
import org.scalatest.prop.TableFor3
import org.scalatest.prop.TableFor4

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.CurrencyPair
import com.opengamma.strata.basics.date.BusinessDayConvention
import com.opengamma.strata.basics.date.DateSequence
import com.opengamma.strata.basics.date.DayCount
import com.opengamma.strata.basics.date.HolidayCalendarId
import com.opengamma.strata.basics.date.HolidayCalendars
import com.opengamma.strata.basics.date.PeriodAdditionConvention
import com.opengamma.strata.basics.date.StandardHolidayCalendars
import com.opengamma.strata.basics.index.FloatingRate
import com.opengamma.strata.basics.index.FloatingRateIndex
import com.opengamma.strata.basics.index.FloatingRateName
import com.opengamma.strata.basics.index.FloatingRateNames
import com.opengamma.strata.basics.index.FloatingRateType
import com.opengamma.strata.basics.index.FxIndex
import com.opengamma.strata.basics.index.FxIndices
import com.opengamma.strata.basics.index.IborIndex
import com.opengamma.strata.basics.index.IborIndices
import com.opengamma.strata.basics.index.Index
import com.opengamma.strata.basics.index.OvernightIndex
import com.opengamma.strata.basics.index.OvernightIndices
import com.opengamma.strata.basics.index.PriceIndex
import com.opengamma.strata.basics.index.PriceIndices
import com.opengamma.strata.basics.index.RateIndex
import com.opengamma.strata.basics.schedule.RollConvention
import com.opengamma.strata.basics.schedule.RollConventions
import com.opengamma.strata.basics.schedule.StubConvention
import com.opengamma.strata.basics.value.ValueAdjustmentType
import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Holds every closed named family of this module to the name space the library being ported
 * published, so that replacing its runtime registry with sealed data lost nothing.
 *
 * The implementation being ported resolved a convention, an index or a currency through
 * `ExtendedEnum`: a registry assembled at class-initialization time by reading a configuration
 * resource off the class path, asking the providers it named for their constants, and folding in
 * the alternate names, the protocol spellings and the lenient patterns that the resource
 * declared. This port has no registry and reads no resource. Each family is a sealed type whose
 * members exist only in its companion, and the three tables the resource carried are Scala
 * literals in that companion. That exchange is only safe if the resulting name space is the same
 * one, member for member and row for row, and this specification is where that is established.
 *
 * ===What this specification owns, and what it does not===
 *
 * It has no single counterpart among the Java tests. It stands in for the `extendedEnum()`
 * assertions that were scattered across the Java suites - `lookupAll`, `lookupAllNormalized`,
 * `findLenient` and the alias probes of `IndexTest`, `RateIndexTest`, `FloatingRateIndexTest` and
 * `DayCountTest` - and for the configuration resources themselves, which are now code and whose
 * lost row would otherwise be invisible. Accordingly `manifest/java-test-mapping.csv` names no
 * test of this class, and the tests below are named for what they state.
 *
 * Three neighbours own the parts this one deliberately leaves alone, and the overlap with them is
 * intentional rather than accidental: this is the specification the acceptance gate for closed
 * enumerations runs, so the properties it needs are asserted here whether or not another
 * specification also asserts them.
 *
 *   - `com.opengamma.strata.collect.NamedEnumSpec` tests the '''mechanism''' of the name lookup on
 *     sample families declared inside it - registration precedence, the sequential rewrite, the
 *     bound on the lenient stage. This specification tests the '''real families''' and assumes the
 *     mechanism works.
 *   - [[ReferenceDataManifestSpec]] tests that the transcribed '''data''' equals the manifest
 *     captured from the Java implementation. This specification tests that the data '''resolves''':
 *     that every member answers to its own name and every transcribed row reaches the member it
 *     names.
 *   - The per-family specifications - `DayCountSpec`, `BusinessDayConventionSpec`,
 *     `RollConventionSpec`, `CurrencySpec`, `FxIndexSpec` and the rest - port their Java test
 *     methods one for one. This specification sweeps every family with the same properties, so a
 *     family added later is covered the day it joins the table below rather than when someone
 *     remembers to write its specification.
 *
 * ===Exhaustive rather than sampled===
 *
 * Every family here is a closed, finite set, so there is no reason to sample one: each property
 * iterates the whole of `values` and the whole of each transcribed table. The sweep visits 855
 * members and some 1,400 transcribed rows, and a specification that checked a hand-picked subset
 * would pass with a row missing - which is precisely the failure this file exists to prevent. The
 * lenient tables are the one place a table cannot simply be iterated, because a row's left-hand
 * side may be a pattern rather than a spelling; the rows are therefore partitioned into the
 * literal ones, which are driven from the production table itself, and the pattern-shaped ones,
 * which are driven by a probe written for each - and the two counts are asserted to add up to the
 * size of the production table, so a row can hide in neither part.
 *
 * ===Cardinalities, and where they come from===
 *
 * Each family is declared with the number of members its reference data fixes, and that number is
 * asserted against the production `values` as a literal rather than derived from it: `values.size
 * shouldBe values.size` would pass with a row dropped. The numbers are those of the manifest
 * captured from the Java implementation - 74 currencies, 21 standard day counts, 7 business day
 * conventions, 45 roll conventions, 3 period addition conventions, 6 date sequences, 8 stub
 * conventions, 5 floating rate types, 4 value adjustment types, 271 Ibor, 35 Overnight, 9 price
 * and 16 FX indices, 351 published floating rate names and 30 built-in holiday calendars.
 *
 * Two of those deserve a note. The day count family is 21 members and not 22: a `Bus/252`
 * convention exists per holiday calendar rather than per family, so the set of them is open and is
 * reached through a factory, exactly as the ported library reached it through a second provider.
 * The floating rate name family is the 351 rows the published table declares, of which 41 are
 * named constants; both figures appear in the manifest and this file asserts each where it belongs.
 *
 * ===The two documented divergences===
 *
 * Two families are closed here where the ported library would mint a member on demand, and both
 * are asserted as behaviour rather than left as prose:
 *
 *   - a currency code outside the published 74 is reported, where the ported factory invented a
 *     currency guessing zero minor units and dollar triangulation;
 *   - a currency pair no published FX index quotes is reported, where the ported factory invented
 *     an index from the pair's default calendar and a two-day settlement offset.
 *
 * Both are recorded in `SCALA_MIGRATION.md`, and neither is a defect to be fixed by reopening the
 * family: a member invented on the spot is data this library never published and could not be
 * resolved back from its own name.
 *
 * @see [[ReferenceDataManifestSpec]] for the transcription of the same tables against the captured
 *   Java manifest
 */
class NamedEnumClosedSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  import NamedEnumClosedSpec._

  test("every closed family holds exactly the members its reference data fixes") {
    forAll(families) { (family: Family) =>
      withClue(s"${family.label}: ") {
        // Non-empty is the type of `values` rather than a hope, but a family whose data table
        // failed to load would still be caught here rather than by a puzzling failure later.
        family.members should not be empty
        family.members.size shouldBe family.expectedMembers

        // Distinct canonical names are what makes a name an identity. Were two members to share
        // one, the second would be unreachable by it, absent from the normalised view, and unable
        // to survive a serialization round trip - so this is asserted before anything is looked up.
        family.members.map(_.name).distinct.size shouldBe family.expectedMembers

        // The lookup is the family's own, covers exactly its members, and describes itself as the
        // family it belongs to - the label that appears in the failure of `parse`.
        family.familyName shouldBe family.label
        family.lookupDescription shouldBe s"NamedEnum[${family.label}]"
      }
    }
  }

  test("every member of every closed family resolves to itself by its canonical name") {
    // This is the round trip closed enumerations turn on: the name a member renders is the name
    // that reaches it. Both entry points are asserted, because the throwing Java factory became
    // two - the exact `valueOf`, answering with an `Option`, and the lenient `parse`, answering
    // with a failure - and a family whose members resolved through one but not the other would
    // serialize to text it could not read back.
    forAll(families) { (family: Family) =>
      family.members.foreach { member =>
        withClue(s"${family.label} ${member.name}: ") {
          family.valueOf(member.name) shouldBe Some(member)
          family.parse(member.name) should haveValue(member)
        }
      }
    }
  }

  test("every member of every closed family resolves by the upper-case form of its name") {
    // The ported providers registered each instance twice, under its own name and under the
    // English upper case of it, first registration winning. That second key is why `ACT/365F`
    // names a day count whose canonical spelling is `Act/365F`, and it is asserted for every
    // member of every family rather than for the families whose names happen to be mixed case.
    forAll(families) { (family: Family) =>
      family.members.foreach { member =>
        val folded = member.name.toUpperCase(Locale.ENGLISH)
        withClue(s"${family.label} $folded: ") {
          // Stated as the property that holds of every family: the value the folded key reaches
          // is a member whose own folded name is that key.
          family.valueOf(folded).map(_.name.toUpperCase(Locale.ENGLISH)) shouldBe Some(folded)
          family.parse(folded) should beSuccess

          // Where no two members of the family fold to the same key - every family but the
          // published floating rate names - the value reached is the member itself. The one
          // family that does have such a pair is asserted exactly, by name, further down.
          if (family.shadowedNames.isEmpty) {
            family.valueOf(folded) shouldBe Some(member)
          }
        }
      }
    }
  }

  test("the canonical-name view of every family is keyed as its members render themselves") {
    // `byCanonicalName` is the Java `lookupAllNormalized`: the registry's keys re-keyed by the
    // canonical name of each value, *not* folded to upper case. The distinction matters because
    // the Java method was what a caller iterated to obtain the family, so a key in the wrong case
    // would have changed what that caller saw.
    forAll(families) { (family: Family) =>
      withClue(s"${family.label}: ") {
        family.byCanonicalName.foreach {
          case (key, value) => withClue(s"key $key: ")(key shouldBe value.name)
        }
        family.byCanonicalName.size shouldBe family.expectedMembers - family.shadowedNames.size

        // A member is absent from this view only where an earlier member claimed both of its
        // keys, which can happen only between two members differing in case alone. The set of
        // such names is declared per family - empty for all but one - so a new collision fails
        // here instead of quietly removing a member from the family's own normalised view.
        (family.members.map(_.name).toSet -- family.byCanonicalName.keySet) shouldBe
          family.shadowedNames

        family.members.filterNot(member => family.shadowedNames.contains(member.name)).foreach {
          member =>
            withClue(s"${member.name}: ")(family.byCanonicalName.get(member.name) shouldBe Some(member))
        }
      }
    }
  }

  test("the upper-case view of every family keys every member by its folded name") {
    forAll(families) { (family: Family) =>
      withClue(s"${family.label}: ") {
        family.byUpperName.keySet shouldBe
          family.members.map(_.name.toUpperCase(Locale.ENGLISH)).toSet
        family.byUpperName.foreach {
          case (key, _) => withClue(s"key $key: ")(key shouldBe key.toUpperCase(Locale.ENGLISH))
        }

        // One key per distinct folded name: the count differs from the member count by exactly
        // the number of members whose folded name another member already holds.
        family.byUpperName.size shouldBe family.expectedMembers - family.shadowedNames.size
      }
    }
  }

  test("no closed family resolves text that names no member") {
    // Where the Java factory raised an error for unrecognised text, the port reports it as a
    // value. The reason is compared as a member of the closed family of reasons rather than by
    // matching the message, so the wording of a message is free to improve without touching this.
    forAll(families) { (family: Family) =>
      unknownNames.foreach { text =>
        withClue(s"${family.label} on [$text]: ") {
          family.valueOf(text) shouldBe None
          family.parse(text) should beFailureWith(FailureReason.PARSING)
        }
      }
    }
  }

  //-------------------------------------------------------------------------
  // The transcribed tables. Each of these rows lived in a configuration resource that the ported
  // library read at class-initialization time; here they are literals in a companion, and a row
  // that failed to be transcribed would change what resolves without changing any type. These are
  // the tests the acceptance gate for closed enumerations depends on most.

  test("the external name groups hold the rows the configuration resources declared") {
    forAll(families) { (family: Family) =>
      withClue(s"${family.label}: ") {
        // A family publishes exactly the groups its resource declared, and answers with nothing
        // for a group it does not publish rather than with an empty group.
        family.externalNameGroups shouldBe family.expectedExternals.keySet
        family.externalNames("Nonexistent-Group") shouldBe None
        family.externalNamesRaw("Nonexistent-Group") shouldBe None

        family.expectedExternals.foreach {
          case (group, expectedRows) =>
            withClue(s"group $group: ") {
              val raw = family.externalNamesRaw(group).getOrElse(
                fail(s"${family.label} publishes no group named $group"))
              val resolved = family.externalNames(group).getOrElse(
                fail(s"${family.label} publishes no resolved group named $group"))

              // The row count is the resource's row count, asserted as a literal: this is the
              // only thing standing between a dropped protocol spelling and silence, since an
              // external row takes part in no lookup and so breaks nothing when it disappears.
              raw.size shouldBe expectedRows

              // The resolved view is the raw table with the rows that name no member removed, so
              // the two key sets differ by exactly the rows declared unresolvable below.
              val unresolvable = family.expectedUnresolvedExternals.getOrElse(group, Set.empty)
              resolved.keySet shouldBe (raw.keySet -- unresolvable)

              // Every resolved row reaches a member of this family, through the same alias-aware
              // exact lookup a caller would use on the canonical name the row carries.
              resolved.foreach {
                case (spelling, value) =>
                  withClue(s"row $spelling: ") {
                    family.members should contain(value)
                    family.valueOf(raw(spelling)) shouldBe Some(value)
                  }
              }

              // And every row declared unresolvable names something this family does not hold,
              // which is why the resolved view drops it. The day count row for `BUS/252` is the
              // only one in the library, and the test below shows the name it carries is real.
              unresolvable.foreach { spelling =>
                withClue(s"unresolved row $spelling: ") {
                  raw.keySet should contain(spelling)
                  family.members.map(_.name) should not contain raw(spelling)
                }
              }
            }
        }
      }
    }
  }

  test("the lenient tables hold the rows the configuration resources declared") {
    forAll(families) { (family: Family) =>
      withClue(s"${family.label}: ") {
        // The whole table, as a literal count. The order of these rows is behaviour rather than
        // presentation - a later pattern is applied to what an earlier one produced - so the
        // order is exercised by the chaining test further down.
        family.lenientPatterns.size shouldBe
          family.expectedLiteralLenient + family.expectedPatternLenient

        // The partition the two tests after this one drive. Asserting both parts against
        // literals, and their sum against the table, is what stops a row from hiding in the part
        // the other test does not cover.
        family.literalLenientRows.size shouldBe family.expectedLiteralLenient
        family.patternLenientRows.size shouldBe family.expectedPatternLenient
        (family.literalLenientRows.size + family.patternLenientRows.size) shouldBe
          family.lenientPatterns.size
      }
    }
  }

  test("every literal lenient row rewrites its own spelling to the member it names") {
    // Driven from the production table rather than from a copy of it, so a row added there is
    // exercised here without this file changing. A literal row is one whose left-hand side holds
    // no pattern metacharacter and whose right-hand side holds no group reference, which makes
    // the row a plain claim: this spelling names that member.
    forAll(families) { (family: Family) =>
      family.literalLenientRows.foreach {
        case (spelling, replacement) =>
          withClue(s"${family.label} $spelling -> $replacement: ") {
            val expected = family.valueOf(replacement).getOrElse(
              fail(s"${family.label} lenient row $spelling names the unknown member $replacement"))

            // The spelling resolves, and it resolves to the member the row names. The chain may
            // rewrite the row's output further - several day count rows do - so the assertion is
            // against the member finally reached rather than against the intermediate text.
            family.parse(spelling) should haveValue(expected)

            // The lenient stage folds its input to upper case before applying a pattern, and
            // every pattern is matched without regard to case, so the spelling resolves however
            // it is written. This is the ported behaviour and is why `act/360` names a day count.
            family.parse(spelling.toUpperCase(Locale.ENGLISH)) should haveValue(expected)
            family.parse(spelling.toLowerCase(Locale.ENGLISH)) should haveValue(expected)
          }
      }
    }
  }

  test("every pattern-shaped lenient row is exercised by a spelling that requires it") {
    // The rows a table cannot be iterated over: their left-hand side is a pattern, so there is no
    // spelling to feed back in. One probe is written for each, and the accounting below holds the
    // set of probed patterns to the set of pattern-shaped rows the production table declares.
    forAll(patternLenientProbes) {
      (label: String, pattern: String, spelling: String, expected: Named) =>
        val family = familiesByLabel(label)
        withClue(s"$label $pattern on $spelling: ") {
          family.patternSources should contain(pattern)
          family.parse(spelling) should haveValue(expected)
        }
    }

    forAll(families) { (family: Family) =>
      withClue(s"${family.label}: ") {
        val probed = patternLenientProbes.collect {
          case (label, pattern, _, _) if label == family.label => pattern
        }.toSet
        probed shouldBe family.patternSources.toSet
        probed.size shouldBe family.expectedPatternLenient
      }
    }
  }

  test("the lenient rewrites are reached only after the exact lookup has missed") {
    // The precedence the ported registry had: `find` first, which consults the alternate names
    // and the two registered keys of each member and rewrites nothing, then `findLenient`, which
    // folds case and applies the patterns. The sharpest statement of it is a spelling that one
    // accepts and the other does not - which a folded canonical name is not, since the exact
    // stage registers that as a key of its own.
    DayCount.valueOf("ACT/360") shouldBe Some(DayCount.ACT_360)
    DayCount.valueOf("ACT_360") shouldBe None
    DayCount.parse("ACT_360") should haveValue(DayCount.ACT_360)
    DayCount.valueOf("Actual/360") shouldBe None
    DayCount.parse("Actual/360") should haveValue(DayCount.ACT_360)

    // The smoke case the plan names for this table, kept because it is the spelling a caller
    // reading FpML sends: it resolves, and it resolves at the exact stage rather than through the
    // chain, the folded canonical name being a key the family registers - which is exactly why it
    // is no use as a discriminator above.
    DayCount.parse("ACT/360") should haveValue(DayCount.ACT_360)

    BusinessDayConvention.valueOf("MF") shouldBe None
    BusinessDayConvention.parse("MF") should haveValue(BusinessDayConvention.ModifiedFollowing)

    RollConvention.valueOf("Day31") shouldBe None
    RollConvention.parse("Day31") should haveValue(RollConventions.EOM)

    PeriodAdditionConvention.valueOf("LAST_DAY") shouldBe None
    PeriodAdditionConvention.parse("LAST_DAY") should haveValue(PeriodAdditionConvention.LAST_DAY)

    // The consequence that matters for the data: because the exact stage runs first and claims
    // every canonical name, no pattern can displace a member's own name. That holds for every
    // member of every family and is asserted over all of them by the round-trip test above; here
    // it is stated for the family whose table is largest and most eager, whose patterns rewrite
    // `ACT/...` and `A/...` and would otherwise be free to claim names of their own.
    DayCount.values.toList.foreach { dayCount =>
      withClue(s"${dayCount.name}: ") {
        DayCount.valueOf(dayCount.name) shouldBe Some(dayCount)
        DayCount.parse(dayCount.name) should haveValue(dayCount)
      }
    }
  }

  test("no closed family declares both an alternate-name table and a lenient table") {
    // The ported lookup consulted the alternate names before it rewrote anything, so a spelling
    // present in both tables resolved through the alternate name. No family of this library
    // declares both, which is why that precedence cannot be demonstrated on real data here - the
    // mechanism is pinned on sample families by `collect.NamedEnumSpec`. This assertion is the
    // guard on that statement: a family that acquires both tables fails here, and whoever adds
    // the row is then pointed at the precedence question rather than discovering it in
    // production.
    forAll(families) { (family: Family) =>
      withClue(s"${family.label}: ") {
        (family.alternateNames.nonEmpty && family.lenientPatterns.nonEmpty) shouldBe false
      }
    }

    // The two mechanisms, and which family uses which. Six families carry alternate spellings:
    // the three index families, whose spellings are the retired and market names their
    // configuration resources declared, and the three constant families, whose spellings are the
    // ones the ported enumeration-name lookup derived from their constant identifiers. Four
    // families carry lenient patterns, all of them transcribed from a resource. The two sets are
    // disjoint, which is the statement above made concrete.
    families.filter(candidate => candidate.alternateNames.nonEmpty).map(_.label).toSet shouldBe
      Set(
        "IborIndex",
        "OvernightIndex",
        "FxIndex",
        "StubConvention",
        "FloatingRateType",
        "ValueAdjustmentType")
    families.filter(candidate => candidate.lenientPatterns.nonEmpty).map(_.label).toSet shouldBe
      Set("DayCount", "RollConvention", "BusinessDayConvention", "PeriodAdditionConvention")
    families.filter(candidate => candidate.enumNameSpellings).map(_.label).toSet shouldBe
      Set("StubConvention", "FloatingRateType", "ValueAdjustmentType")
  }

  test("every alternate spelling a family declares reaches a member without shadowing a name") {
    // The universal sweep over the alternate-name tables: every entry of every family, expanded
    // spellings included. A spelling is substituted before either lookup stage consults the keys,
    // so an entry whose key were a canonical name would divert that name to another member and
    // break the round trip - which is why the second assertion is here and not only in prose.
    forAll(families) { (family: Family) =>
      withClue(s"${family.label}: ") {
        family.alternateNames.size shouldBe family.expectedAlternateEntries

        family.alternateNames.foreach {
          case (spelling, canonicalName) =>
            withClue(s"$spelling -> $canonicalName: ") {
              val expected = family.valueOf(canonicalName).getOrElse(
                fail(s"${family.label} has no member named $canonicalName"))
              family.valueOf(spelling) shouldBe Some(expected)
              family.parse(spelling) should haveValue(expected)
              family.byCanonicalName.keySet should not contain spelling
            }
        }
      }
    }
  }

  test("the constant families accept the spellings the enumeration-name lookup derived") {
    // Three families were plain Java enumerations resolved through `EnumNames`, which accepted a
    // member's name in three further spellings derived from its constant identifier: the
    // screaming-snake identifier, that identifier in lower case, and the name run together in
    // lower case. Those spellings are a table in this port rather than a derivation, so they are
    // asserted here by deriving them again, for every member of every such family - a row lost
    // from the table fails here rather than the day a caller sends `SHORT_INITIAL`.
    val constantFamilies = families.filter(candidate => candidate.enumNameSpellings)
    constantFamilies.size shouldBe 3

    constantFamilies.foreach { family =>
      family.members.foreach { member =>
        val screamingSnake = screamingSnakeOf(member.name)
        val spellings =
          List(
            screamingSnake,
            screamingSnake.toLowerCase(Locale.ENGLISH),
            member.name.toLowerCase(Locale.ENGLISH))

        spellings.foreach { spelling =>
          withClue(s"${family.label} ${member.name} as $spelling: ") {
            family.valueOf(spelling) shouldBe Some(member)
            family.parse(spelling) should haveValue(member)
          }
        }
      }
    }
  }

  test("every alternate spelling resolves to the published member it renames") {
    forAll(alternateSpellings) { (label: String, spelling: String, canonicalName: String) =>
      val family = familiesByLabel(label)
      withClue(s"$label $spelling -> $canonicalName: ") {
        val expected = family.valueOf(canonicalName).getOrElse(
          fail(s"$label has no member named $canonicalName"))

        // The spelling reaches the member, through the exact lookup as well as through `parse`,
        // since an alternate name is substituted before either stage consults the keys.
        family.valueOf(spelling) shouldBe Some(expected)
        family.parse(spelling) should haveValue(expected)
        family.alternateNames.get(spelling) shouldBe Some(canonicalName)

        // And the spelling is a retired or market name rather than a member: were it a member of
        // its own, the family would hold two values for one rate.
        family.members.map(_.name) should not contain spelling
        family.byCanonicalName.keySet should not contain spelling
      }
    }

    // The accounting: the rows probed above are the rows the configuration resources declared,
    // family by family.
    forAll(families) { (family: Family) =>
      withClue(s"${family.label}: ") {
        alternateSpellings.count { case (label, _, _) => label == family.label } shouldBe
          family.expectedAlternateRows
      }
    }
  }

  test("the expanded alternate-name tables are the declared rows plus their folded spellings") {
    // The lookup expands the table it is given: alongside each declared spelling it holds that
    // spelling folded to upper case, unless the table already declares one identical to it. The
    // expansion is what lets an alternate name be reached from `parse`, where the text has
    // already been folded, as well as from `valueOf`. Asserted as whole maps, so neither a lost
    // row nor an invented one survives.
    IborIndex.namedEnum.alternateNames shouldBe Map("KRW-CD-3M" -> "KRW-CD-13W")

    OvernightIndex.namedEnum.alternateNames shouldBe
      Map(
        "CLP-ICP" -> "CLP-TNA",
        "DKK-Tom Next" -> "DKK-TNR",
        "DKK-TOM NEXT" -> "DKK-TNR",
        "EUR-ESTER" -> "EUR-ESTR",
        "EUR-EuroSTR" -> "EUR-ESTR",
        "EUR-EUROSTR" -> "EUR-ESTR",
        "HKD-HONIX" -> "HKD-HONIA",
        "JPY-TONA" -> "JPY-TONAR",
        "USD-FED-FUNDS" -> "USD-FED-FUND",
        "USD-FEDFUND" -> "USD-FED-FUND",
        "USD-FEDFUNDS" -> "USD-FED-FUND",
        "USD-Federal Funds" -> "USD-FED-FUND",
        "USD-FEDERAL FUNDS" -> "USD-FED-FUND")

    FxIndex.namedEnum.alternateNames shouldBe Map("USD/INR-RBIB-INR01" -> "USD/INR-FBIL-INR01")

    // Eight families declare none, because the resource behind each of them declared none: the
    // whole name space of such a family is its canonical names, their folded forms, and whatever
    // its lenient table rewrites into one of those.
    val withoutAlternates =
      Set(
        "Currency",
        "DayCount",
        "BusinessDayConvention",
        "RollConvention",
        "PeriodAdditionConvention",
        "DateSequence",
        "PriceIndex",
        "FloatingRateName")
    families.filter(candidate => withoutAlternates.contains(candidate.label)).foreach { candidate =>
      withClue(s"${candidate.label}: ")(candidate.alternateNames shouldBe Map.empty[String, String])
    }
    families.map(_.label).toSet.diff(withoutAlternates) shouldBe
      Set(
        "IborIndex",
        "OvernightIndex",
        "FxIndex",
        "StubConvention",
        "FloatingRateType",
        "ValueAdjustmentType")
  }

  test("an alternate spelling resolves whatever case it is written in") {
    // The ported loader registered an alternate spelling under the spelling it was given and
    // under the folded form of it, which is why the mixed-case rows - `DKK-Tom Next`,
    // `EUR-EuroSTR`, `USD-Federal Funds` - resolve from text a caller typed in any case.
    forAll(alternateSpellings) { (label: String, spelling: String, canonicalName: String) =>
      val family = familiesByLabel(label)
      withClue(s"$label $spelling: ") {
        val expected = family.valueOf(canonicalName).getOrElse(
          fail(s"$label has no member named $canonicalName"))

        family.valueOf(spelling.toUpperCase(Locale.ENGLISH)) shouldBe Some(expected)
        family.parse(spelling.toUpperCase(Locale.ENGLISH)) should haveValue(expected)

        // `parse` folds its input before it looks up the folded key, so a lower-case spelling
        // resolves through the same expanded table, while `valueOf` - which folds nothing -
        // answers with nothing unless the table happens to declare that exact spelling.
        family.parse(spelling.toLowerCase(Locale.ENGLISH)) should haveValue(expected)
      }
    }
  }

  test("the lenient rewrites chain, each pattern seeing what the one before it produced") {
    // Two chains through the day count table, which is the only one in the library deep enough to
    // have them. Each intermediate form is shown to resolve to nothing on its own, so the chain
    // is doing the work rather than one lucky pattern; reordering the transcribed rows would
    // break both.

    // `ACTUAL/ACTUAL(.*)` expands the long spelling, then `(.*)[(](.*)[)]` removes the brackets.
    DayCount.valueOf("Actual/Actual (ISDA)") shouldBe None
    DayCount.valueOf("Act/Act (ISDA)") shouldBe None
    DayCount.valueOf("Act/Act ISDA") shouldBe Some(DayCount.ACT_ACT_ISDA)
    DayCount.parse("Actual/Actual (ISDA)") should haveValue(DayCount.ACT_ACT_ISDA)

    // Three patterns in order: `ACT/ACT(.*)` normalises the abbreviation, then
    // `(.*)[.]([A-Z])(.*)` turns the dotted qualifier into a spaced one, then `(.*) ISMA`
    // renames the retired qualifier. This is the FpML spelling of the convention, and it is
    // reached by the chain rather than by the protocol table, which takes part in no lookup.
    DayCount.valueOf("ACT/ACT.ISMA") shouldBe None
    DayCount.valueOf("Act/Act.ISMA") shouldBe None
    DayCount.valueOf("Act/Act ISMA") shouldBe None
    DayCount.valueOf("Act/Act ICMA") shouldBe Some(DayCount.ACT_ACT_ICMA)
    DayCount.parse("ACT/ACT.ISMA") should haveValue(DayCount.ACT_ACT_ICMA)

    // The same chain reached from the shortest spelling the table accepts, which fires
    // `A/A(.*)` before `(.*) ISMA`.
    DayCount.parse("A/A ISMA") should haveValue(DayCount.ACT_ACT_ICMA)
  }

  //-------------------------------------------------------------------------
  // The families whose closedness needs a statement of its own: one that is closed over its
  // standard members while a second, open kind is reached through a factory, one whose reference
  // data is a set of calendars rather than a list of names, and the two whose closedness is a
  // documented divergence from the library being ported.

  test("the day count family is closed over its standard members while Bus/252 is per calendar") {
    // The ported registry named two providers for this family: the constants of the standard
    // conventions, and a lookup that built a `Bus/252` convention for whatever calendar a name
    // held. Only the first of those is a closed set, so `values` is the 21 standard conventions
    // and the family's own lookup knows nothing of `Bus/252`.
    DayCount.values.length shouldBe 21
    DayCount.values.toList.map(_.name) should not contain "Bus/252 BRBD"
    DayCount.namedEnum.values.toList shouldBe DayCount.values.toList
    DayCount.namedEnum.valueOf("Bus/252 BRBD") shouldBe None
    DayCount.namedEnum.byCanonicalName.keySet should not contain "Bus/252 BRBD"

    // The second provider is the pair of factories that replaced it. The total one takes the
    // resolved calendar, so the convention it returns is a pure function of its argument rather
    // than of ambient reference data.
    DayCount.ofBus252(StandardHolidayCalendars.BRBD).name shouldBe "Bus/252 BRBD"
    DayCount.ofBus252(StandardHolidayCalendars.GBLO).name shouldBe "Bus/252 GBLO"
    DayCount.ofBus252(HolidayCalendarId.of("BRBD"), ReferenceData.standard) should
      haveValue(DayCount.ofBus252(StandardHolidayCalendars.BRBD))
    DayCount.ofBus252(HolidayCalendarId.of("GBXX"), ReferenceData.standard) should
      beFailureWith(FailureReason.MISSING_DATA)

    // Reading a `Bus/252` name resolves its calendar against the calendars built into this
    // library, which are constant data rather than the ambient standard reference data the
    // ported factory reached for. The prefix is matched without regard to case, as it was there;
    // the calendar name that follows it is matched against the canonical and folded names of the
    // built-in calendars, which is the pair of keys the ported calendar registry held, so `GBLO`
    // resolves and `gblo` does not.
    DayCount.valueOf("Bus/252 BRBD").map(_.name) shouldBe Some("Bus/252 BRBD")
    DayCount.valueOf("BUS/252 BRBD").map(_.name) shouldBe Some("Bus/252 BRBD")
    DayCount.parse("Bus/252 BRBD").map(_.name) shouldBe Right("Bus/252 BRBD")
    DayCount.parse("BUS/252 GBLO").map(_.name) shouldBe Right("Bus/252 GBLO")
    DayCount.parse("bus/252 GBLO").map(_.name) shouldBe Right("Bus/252 GBLO")

    // A lower-case calendar name is reported rather than resolved, and the failure comes from the
    // calendar lookup rather than from this family: the `Bus/252` provider recognises the text on
    // its case-insensitive prefix and so claims it, exactly as the ported provider did, which is
    // why the lenient stage is never reached for such a name. The ported implementation raised
    // from the same place, its calendar registry holding `GBLO` and `GBLO` folded and nothing
    // else, so this is the ported answer expressed as a value.
    DayCount.valueOf("Bus/252 gblo") shouldBe None
    DayCount.parse("Bus/252 gblo") should beFailureWith(FailureReason.PARSING)
    DayCount.parse("Bus/252 gblo") should haveFailureMessageMatching(
      ".*HolidayCalendar name not found.*")

    // The bare prefix is a lenient row, defaulted to the Brazilian calendar exactly as the
    // resource declared, and it is the row that makes the FpML spelling below resolve.
    DayCount.parse("Bus/252").map(_.name) shouldBe Right("Bus/252 BRBD")
    DayCount.parse("BUS/252").map(_.name) shouldBe Right("Bus/252 BRBD")

    // A name whose calendar this library does not define is reported rather than invented, and
    // the failure is the calendar lookup's own - the more specific of the two available answers,
    // and the error the ported implementation raised from the same place.
    DayCount.valueOf("Bus/252 GBXX") shouldBe None
    DayCount.parse("Bus/252 GBXX") should beFailureWith(FailureReason.PARSING)
    DayCount.parse("Bus/252 GBXX", ReferenceData.standard) should
      beFailureWith(FailureReason.MISSING_DATA)

    // The one external row in the library that names a non-member: the FpML spelling of the
    // Brazilian convention. The row is real - the name it carries resolves - which is why it is
    // published by the raw table while the resolved view, which maps onto members, drops it.
    DayCount.namedEnum.externalNamesRaw("FpML").flatMap(_.get("BUS/252")) shouldBe
      Some("Bus/252 BRBD")
    DayCount.namedEnum.externalNamesRaw("FpML").map(_.size) shouldBe Some(14)
    DayCount.namedEnum.externalNames("FpML").map(_.contains("BUS/252")) shouldBe Some(false)
    DayCount.namedEnum.externalNames("FpML").map(_.size) shouldBe Some(13)
    DayCount.parse("BUS/252").map(_.name) shouldBe Right("Bus/252 BRBD")
  }

  test("every built-in holiday calendar is reachable by its identifier and resolves as standard") {
    // The ported registry published the calendars as a named family assembled from a generator
    // and a configuration resource; here they are the constant set this module ships, and the
    // properties that made the registry usable are asserted over the whole of it.
    val builtIn = StandardHolidayCalendars.all
    builtIn.size shouldBe 30

    builtIn.foreach {
      case (id, calendar) =>
        withClue(s"${id.name}: ") {
          // The invariant the registry established by re-keying its providers' output: a
          // calendar is filed under the identifier it carries, never under another.
          calendar.name shouldBe id.name
          HolidayCalendarId.of(calendar.name) shouldBe id

          // Both name views of the built-in set reach it: the canonical one, which is what the
          // JSON form of a calendar writes and reads, and the folded one, which is why
          // `Bus/252 gblo` names the same day count as `Bus/252 GBLO`.
          StandardHolidayCalendars.byName(calendar.name) shouldBe Some(calendar)
          StandardHolidayCalendars.byUpperName(calendar.name.toUpperCase(Locale.ENGLISH)) shouldBe
            Some(calendar)

          // And both resolution routes reach it: the short cut that reads a name directly, and
          // the reference data route every adjustment and schedule in the library takes.
          HolidayCalendars.of(calendar.name) should haveValue(calendar)
          id.resolve(ReferenceData.standard) should haveValue(calendar)
          ReferenceData.standard.containsValue(id) shouldBe true
        }
    }

    // The 30 identifiers are distinct, so no calendar displaced another while the set was built.
    builtIn.keySet.size shouldBe 30
    builtIn.values.map(_.name).toSet.size shouldBe 30

    // A name this library does not define is an empty answer from the views and a reported
    // failure from the two routes that have an error channel. The reasons differ by design: a
    // name that no built-in calendar carries is text this library cannot parse, while an
    // identifier absent from reference data is data the caller did not supply.
    StandardHolidayCalendars.byName("GBXX") shouldBe None
    StandardHolidayCalendars.byUpperName("GBXX") shouldBe None
    HolidayCalendars.of("GBXX") should beFailureWith(FailureReason.PARSING)
    HolidayCalendarId.of("GBXX").resolve(ReferenceData.standard) should
      beFailureWith(FailureReason.MISSING_DATA)
  }

  test("the currency family is closed: a code outside the published set is reported, not minted") {
    // A documented divergence, recorded in `SCALA_MIGRATION.md` as Conflict 4 of the plan. The
    // ported factory created a currency for any three upper-case letters, guessing zero minor
    // units and dollar triangulation, so `Currency.of("XYZ")` answered with a currency that no
    // reference data described. This family is the closed set of the 74 published codes and
    // reports the unknown code instead.
    Currency.values.length shouldBe 74

    Currency.valueOf("XYZ") shouldBe None
    Currency.of("XYZ") should beFailureWith(FailureReason.PARSING)
    Currency.parse("XYZ") should beFailureWith(FailureReason.PARSING)
    Currency.parse("xyz") should beFailureWith(FailureReason.PARSING)

    // The closure holds for every shape of unknown code, not only for well-formed ones, and
    // repeating the call yields the same failure rather than a second invented currency.
    Currency.parse("QQQ") should beFailureWith(FailureReason.PARSING)
    Currency.parse("ZZZ") should beFailureWith(FailureReason.PARSING)
    Currency.parse("XYZ") shouldBe Currency.parse("XYZ")

    // What remains true is the case tolerance of the ported factory: a published code resolves
    // however it is written, and only the exact lookup is case sensitive.
    Currency.parse("gbp") should haveValue(Currency.GBP)
    Currency.parse("GbP") should haveValue(Currency.GBP)
    Currency.of("gbp") should beFailureWith(FailureReason.PARSING)
    Currency.valueOf("gbp") shouldBe None
  }

  test("the FX index family is closed: an unquoted currency pair is reported, not minted") {
    // A documented divergence, recorded in `SCALA_MIGRATION.md` as Conflict 7 of the plan. The
    // ported factory minted an index for a pair no administrator publishes, from the pair's
    // default calendar and a settlement offset of two business days; this family is the closed
    // set of the 16 published indices. `FxIndexSpec` asserts the same thing over the family's own
    // surface - the duplication is deliberate, because this is the specification the acceptance
    // gate for closed enumerations runs.
    FxIndex.values.length shouldBe 16

    val unquoted = CurrencyPair.of(Currency.GBP, Currency.SEK)
    FxIndex.of(unquoted) should beFailureWith(FailureReason.PARSING)
    FxIndex.of("GBP/SEK") should beFailureWith(FailureReason.PARSING)
    FxIndex.values.toList.map(_.currencyPair) should not contain unquoted

    // The inverse of a quoted pair is not quoted either: the published data is keyed by the pair
    // as it is quoted, which is how the ported lookup compared it before falling back to minting.
    FxIndex.of(CurrencyPair.of(Currency.USD, Currency.EUR)) should
      beFailureWith(FailureReason.PARSING)

    // What remains is the selection rule for a pair two administrators publish: the index whose
    // name sorts first, which is the choice the ported implementation made.
    FxIndex.of(CurrencyPair.of(Currency.EUR, Currency.USD)) should haveValue(FxIndices.EUR_USD_ECB)
    FxIndex.of("EUR/USD") should haveValue(FxIndices.EUR_USD_ECB)
    FxIndex.of("EUR/USD-ECB") should haveValue(FxIndices.EUR_USD_ECB)
  }

  //-------------------------------------------------------------------------
  // The four unions. The ported library built three of these from `[types]` declarations in
  // configuration resources and the fourth by hand, each searching its member families in its own
  // order and answering with the first value found. Each is now a `parse` of its own, and what
  // fixes it is its membership - which names it accepts and which it refuses - so membership is
  // asserted over the whole of every family rather than over an example of each.

  test("Index.parse accepts every member of the Ibor, Overnight, price and FX families") {
    IborIndex.values.toList.foreach { index =>
      withClue(s"${index.name}: ") {
        Index.valueOf(index.name) shouldBe Some(index)
        Index.parse(index.name) should haveValue(index)
      }
    }
    OvernightIndex.values.toList.foreach { index =>
      withClue(s"${index.name}: ")(Index.parse(index.name) should haveValue(index))
    }
    PriceIndex.values.toList.foreach { index =>
      withClue(s"${index.name}: ")(Index.parse(index.name) should haveValue(index))
    }
    FxIndex.values.toList.foreach { index =>
      withClue(s"${index.name}: ")(Index.parse(index.name) should haveValue(index))
    }

    // The union is over the four index families and no other, so a name belonging only to the
    // published floating rate names - the fifth family of the widest union - is refused. Those
    // names are split by whether an index family claims them, which ties the union's membership
    // to the name spaces of its members rather than restating it: the 54 names the published
    // table shares with the Overnight and price families resolve, and the other 297 do not.
    val (claimedByIndex, ownedByNameFamily) =
      FloatingRateName.values.toList.partition(value => allIndexNames.contains(value.name))
    claimedByIndex.size shouldBe 54
    ownedByNameFamily.size shouldBe 297

    claimedByIndex.foreach { value =>
      withClue(s"${value.name}: ")(Index.parse(value.name) should beSuccess)
    }
    ownedByNameFamily.foreach { value =>
      withClue(s"${value.name}: ") {
        Index.valueOf(value.name) shouldBe None
        Index.parse(value.name) should beFailureWith(FailureReason.PARSING)
      }
    }

    unknownNames.foreach { text =>
      withClue(s"[$text]: ") {
        Index.valueOf(text) shouldBe None
        Index.parse(text) should beFailureWith(FailureReason.PARSING)
      }
    }
  }

  test("RateIndex.parse accepts the Ibor and Overnight families and refuses the other two") {
    IborIndex.values.toList.foreach { index =>
      withClue(s"${index.name}: ") {
        RateIndex.valueOf(index.name) shouldBe Some(index)
        RateIndex.parse(index.name) should haveValue(index)
      }
    }
    OvernightIndex.values.toList.foreach { index =>
      withClue(s"${index.name}: ") {
        RateIndex.valueOf(index.name) shouldBe Some(index)
        RateIndex.parse(index.name) should haveValue(index)
      }
    }

    // A price index measures a level rather than a rate of interest, and an FX index a rate of
    // exchange; neither is a rate index, so every member of both families is refused.
    (PriceIndex.values.toList.map(_.name) ++ FxIndex.values.toList.map(_.name)).foreach { name =>
      withClue(s"$name: ") {
        RateIndex.valueOf(name) shouldBe None
        RateIndex.parse(name) should beFailureWith(FailureReason.PARSING)
      }
    }

    RateIndex.parse("GBP-LIBOR") should beFailureWith(FailureReason.PARSING)
    unknownNames.foreach { text =>
      withClue(s"[$text]: ")(RateIndex.parse(text) should beFailureWith(FailureReason.PARSING))
    }
  }

  test("FloatingRateIndex.parse accepts the three rate families and refuses the FX family") {
    (IborIndex.values.toList ++ OvernightIndex.values.toList ++ PriceIndex.values.toList).foreach {
      index =>
        withClue(s"${index.name}: ") {
          FloatingRateIndex.valueOf(index.name) shouldBe Some(index)
          FloatingRateIndex.parse(index.name) should haveValue(index)
        }
    }

    FxIndex.values.toList.foreach { index =>
      withClue(s"${index.name}: ") {
        FloatingRateIndex.valueOf(index.name) shouldBe None
        FloatingRateIndex.parse(index.name) should beFailureWith(FailureReason.PARSING)
      }
    }

    unknownNames.foreach { text =>
      withClue(s"[$text]: ") {
        FloatingRateIndex.parse(text) should beFailureWith(FailureReason.PARSING)
      }
    }
  }

  test("FloatingRate.parse searches the three index families before the published names") {
    // The widest union, and the only one whose member families share names - which is what makes
    // its order observable rather than merely declared. The shared names are asserted below; here
    // the membership is established first.
    (IborIndex.values.toList ++ OvernightIndex.values.toList ++ PriceIndex.values.toList).foreach {
      index =>
        withClue(s"${index.name}: ") {
          FloatingRate.tryParse(index.name) shouldBe Some(index)
          FloatingRate.parse(index.name) should haveValue(index)
        }
    }

    // The published names that no index family holds resolve to the family identifier itself,
    // which is the fourth probe and the reason this union is wider than the one above.
    FloatingRateName.values.toList.filterNot(value => allIndexNames.contains(value.name)).foreach {
      value =>
        withClue(s"${value.name}: ") {
          FloatingRate.tryParse(value.name) shouldBe Some(value)
          FloatingRate.parse(value.name) should haveValue(value)
        }
    }

    // An FX index is not a floating rate: its figure is a rate of exchange, so it belongs to no
    // probe of this union and every member of the family is refused.
    FxIndex.values.toList.foreach { index =>
      withClue(s"${index.name}: ") {
        FloatingRate.tryParse(index.name) shouldBe None
        FloatingRate.parse(index.name) should beFailureWith(FailureReason.PARSING)
      }
    }

    unknownNames.foreach { text =>
      withClue(s"[$text]: ") {
        FloatingRate.tryParse(text) shouldBe None
        FloatingRate.parse(text) should beFailureWith(FailureReason.PARSING)
      }
    }

    // The order stated with the named constants, one per probe, as the documented union reads:
    // an Ibor index, an Overnight index, a price index, then a family identifier that names no
    // index at all and so can only come from the fourth probe.
    FloatingRate.parse("GBP-LIBOR-3M") should haveValue(IborIndices.GBP_LIBOR_3M)
    FloatingRate.parse("EUR-ESTR") should haveValue(OvernightIndices.EUR_ESTR)
    FloatingRate.parse("GB-RPI") should haveValue(PriceIndices.GB_RPI)
    FloatingRate.parse("GBP-LIBOR") should haveValue(FloatingRateNames.GBP_LIBOR)
    FloatingRateIndex.valueOf("GBP-LIBOR") shouldBe None
  }

  test("the four index families claim pairwise disjoint name spaces") {
    // The collision guard. Nothing in the types stops two index families from publishing the same
    // name, and were they to, the earlier probe of every union above would shadow the later one
    // silently - a member of a family that no text resolves to. The name space compared is the
    // whole of what each family answers to: its canonical names, their folded forms, and its
    // alternate spellings.
    val indexFamilies = List("IborIndex", "OvernightIndex", "PriceIndex", "FxIndex").map(familiesByLabel)
    val pairs = for {
      first <- indexFamilies.indices
      second <- indexFamilies.indices if second > first
    } yield (indexFamilies(first), indexFamilies(second))

    pairs.size shouldBe 6
    pairs.foreach {
      case (first, second) =>
        withClue(s"${first.label} against ${second.label}: ") {
          (first.nameSpace intersect second.nameSpace) shouldBe empty
        }
    }

    // Equivalently, and as a single number: the four name spaces partition their union.
    val combined = indexFamilies.foldLeft(Set.empty[String])((acc, family) => acc ++ family.nameSpace)
    combined.size shouldBe indexFamilies.map(_.nameSpace.size).sum
  }

  test("the published floating rate names overlap the rate families, and the index wins") {
    // The deliberate exception to the guard above, and the reason the order of the widest union is
    // behaviour rather than presentation: the published name table republishes every Overnight
    // index name - and every alternate spelling of one - and every price index name, because an
    // Overnight or price rate is named by its index while an Ibor rate is named by its family and
    // its tenor separately. Every one of those names therefore resolves to two different values
    // depending on which lookup is asked, and the union answers with the index because the index
    // families are probed first.
    val shared = FloatingRateName.values.toList.filter { value =>
      FloatingRateIndex.valueOf(value.name).isDefined
    }
    shared.size shouldBe 54

    shared.foreach { value =>
      withClue(s"${value.name}: ") {
        val index = FloatingRateIndex.valueOf(value.name).getOrElse(
          fail(s"${value.name} no longer names an index"))

        // The union answers with the index, from an earlier probe...
        FloatingRate.tryParse(value.name) shouldBe Some(index)
        FloatingRate.parse(value.name) should haveValue(index)

        // ...while the published name family, asked directly, answers with its own member. The
        // two answers differ, which is what makes the probe order observable.
        FloatingRateName.valueOf(value.name) shouldBe Some(value)
        FloatingRate.tryParse(value.name) should not be FloatingRateName.valueOf(value.name)
      }
    }

    // The overlap accounted for family by family: it is the Overnight family's canonical names
    // together with the ten alternate spellings it declares, and the price family's names. No
    // Ibor index name and no FX index name is a published floating rate name, so the narrower
    // unions - which is every other one - are unaffected by any of this.
    val sharedNames = shared.map(_.name).toSet
    val overnightShared = sharedNames.filter(name => OvernightIndex.valueOf(name).isDefined)
    val priceShared = sharedNames.filter(name => PriceIndex.valueOf(name).isDefined)

    overnightShared.size shouldBe 45
    priceShared.size shouldBe 9
    (overnightShared ++ priceShared) shouldBe sharedNames
    sharedNames.filter(name => IborIndex.valueOf(name).isDefined) shouldBe empty

    overnightShared shouldBe
      (OvernightIndex.values.toList.map(_.name).toSet ++ declaredAlternates("OvernightIndex"))
    priceShared shouldBe PriceIndex.values.toList.map(_.name).toSet

    IborIndex.values.toList.filter(index => FloatingRateName.valueOf(index.name).isDefined) shouldBe
      empty
    FxIndex.values.toList.filter(index => FloatingRateName.valueOf(index.name).isDefined) shouldBe
      empty
  }

  test("the published floating rate name constants are members of the closed family") {
    // The family is the 351 rows the published table declares, of which these 41 are the named
    // constants the ported interface exposed. Both figures are in the captured manifest; the
    // constants are written out here because this port performs no reflection and so cannot
    // enumerate them, and because a constant that stopped being a member would otherwise compile.
    FloatingRateName.values.length shouldBe 351
    floatingRateNameConstants.size shouldBe 41
    floatingRateNameConstants.distinct.size shouldBe 41

    floatingRateNameConstants.foreach { constant =>
      withClue(s"${constant.name}: ") {
        FloatingRateName.values.toList should contain(constant)
        FloatingRateName.valueOf(constant.name) shouldBe Some(constant)
        FloatingRateName.parse(constant.name) should haveValue(constant)
        FloatingRate.parse(constant.name) should beSuccess
      }
    }
  }

  test("the published name family reaches both members of each pair differing only in case") {
    // Two rows of the published table are declared twice, differing in the case of one word, and
    // both spellings are published names of their own. That makes this the one family in the
    // library where a member's folded key is another member's canonical name, and it is why the
    // family's lookup by name probes the published names before the general name lookup: under
    // the general lookup's precedence alone - first member to offer a key keeps it - the second of
    // each pair would be unreachable by the name it was published under, and so unable to survive
    // a serialization round trip.
    foldedNamePairs.foreach {
      case (mixedCase, folded) =>
        withClue(s"$mixedCase / $folded: ") {
          folded shouldBe mixedCase.toUpperCase(Locale.ENGLISH)
          folded should not be mixedCase

          // Both members are reachable by their own name through the family's own lookup.
          FloatingRateName.valueOf(mixedCase).map(_.name) shouldBe Some(mixedCase)
          FloatingRateName.valueOf(folded).map(_.name) shouldBe Some(folded)
          FloatingRateName.parse(mixedCase).map(_.name) shouldBe Right(mixedCase)
          FloatingRateName.parse(folded).map(_.name) shouldBe Right(folded)

          // The general name lookup, asked directly, shows the precedence being worked around:
          // the member declared first holds the folded key, and the member whose canonical name
          // that key is has no entry in the normalised view at all.
          FloatingRateName.namedEnum.valueOf(folded).map(_.name) shouldBe Some(mixedCase)
          FloatingRateName.namedEnum.byCanonicalName.contains(folded) shouldBe false
          FloatingRateName.namedEnum.byCanonicalName.contains(mixedCase) shouldBe true
        }
    }

    // Which is the whole of the difference between the family and its normalised view: 351
    // members, 349 canonical keys, the two missing keys being those of the pairs above.
    FloatingRateName.namedEnum.byCanonicalName.size shouldBe 349
    FloatingRateName.namedEnum.byUpperName.size shouldBe 349
    (FloatingRateName.values.toList.map(_.name).toSet --
      FloatingRateName.namedEnum.byCanonicalName.keySet) shouldBe foldedNamePairs.map(_._2).toSet
  }
}

/**
 * The closed families under assertion and the tables that drive them.
 *
 * Held beside the specification rather than inside it so that the inventory of families reads as
 * one list: adding a family here is what puts it under every property above.
 */
private[basics] object NamedEnumClosedSpec extends TableDrivenPropertyChecks {

  /**
   * The characters that make the left-hand side of a lenient row a pattern rather than a
   * spelling.
   *
   * A row whose expression holds none of these, and whose replacement refers to no captured
   * group, is a plain claim that one spelling names one member - which is a row the production
   * table can be iterated over and driven directly. Every other row needs a probe written for
   * it, because there is no single spelling to feed back in.
   */
  private val PatternMetacharacters: Set[Char] = "\\^$.|?*+()[]{}".toSet

  /**
   * A closed named family under assertion, with the type of its members erased to [[Named]].
   *
   * The families differ in their member type and nothing else that matters here, so each is
   * widened to this one shape and swept by the same properties. The widening is ordinary
   * subtyping - `Option`, `Map`, `List` and `Either` are all covariant in the position that
   * holds a member - so nothing is cast and no member arrives here having lost anything but the
   * static knowledge of which family it came from, which the properties below do not need.
   *
   * The `valueOf` and `parse` held here are the family's own, taken from its companion rather
   * than from the name lookup, because two families wrap that lookup: the day counts add the
   * `Bus/252` conventions, which no closed set can express, and the published floating rate
   * names add the precedence their duplicated spellings require. Asserting the companion's
   * entry points is asserting what a caller actually reaches.
   *
   * @param label  the name of the family, which is also the label its lookup reports
   * @param members  the members of the family, in declaration order
   * @param valueOf  the family's exact, alias-aware lookup by name
   * @param parse  the family's lenient lookup by name
   * @param familyName  the label the lookup reports when it rejects text
   * @param lookupDescription  the rendering of the lookup itself
   * @param alternateNames  the alternate spellings, expanded with their folded forms
   * @param byUpperName  the members keyed by their folded name
   * @param byCanonicalName  the members keyed by the name they render
   * @param lenientPatterns  the lenient rewrites, in the order they are applied
   * @param externalNameGroups  the names of the groups of protocol spellings published
   * @param externalNamesRaw  a group of protocol spellings as the family declared it
   * @param externalNames  a group of protocol spellings resolved onto members
   * @param expectedMembers  the number of members the captured reference data fixes
   * @param shadowedNames  the canonical names an earlier member of the same family claims,
   *   which are therefore absent from the normalised view
   * @param expectedAlternateRows  the number of alternate spellings the configuration resource
 *   declared, which is zero for a family whose spellings come from its constant identifiers
 * @param expectedAlternateEntries  the size of the alternate-name table after the lookup has
 *   expanded it with the folded form of every declared spelling
 * @param enumNameSpellings  whether the family declares the spellings that the ported
 *   enumeration-name lookup derived from its constant identifiers
   * @param expectedExternals  the number of rows of each group the resource declared
   * @param expectedUnresolvedExternals  the spellings of each group naming no member
   * @param expectedLiteralLenient  the number of lenient rows that are plain spellings
   * @param expectedPatternLenient  the number of lenient rows that are patterns
   */
  final class Family private[NamedEnumClosedSpec] (
      val label: String,
      val members: List[Named],
      val valueOf: String => Option[Named],
      val parse: String => ResultNec[Named],
      val familyName: String,
      val lookupDescription: String,
      val alternateNames: Map[String, String],
      val byUpperName: Map[String, Named],
      val byCanonicalName: Map[String, Named],
      val lenientPatterns: List[(Regex, String)],
      val externalNameGroups: Set[String],
      val externalNamesRaw: String => Option[Map[String, String]],
      val externalNames: String => Option[Map[String, Named]],
      val expectedMembers: Int,
      val shadowedNames: Set[String],
      val expectedAlternateRows: Int,
      val expectedAlternateEntries: Int,
      val enumNameSpellings: Boolean,
      val expectedExternals: Map[String, Int],
      val expectedUnresolvedExternals: Map[String, Set[String]],
      val expectedLiteralLenient: Int,
      val expectedPatternLenient: Int) {

    /**
     * Every spelling this family answers to through its exact lookup.
     *
     * The two keys each member is registered under, together with the alternate spellings the
     * family declares. This is what two families must not share if the union that searches them
     * is to reach both.
     *
     * @return the spellings this family claims
     */
    def nameSpace: Set[String] =
      members.map(_.name).toSet ++
        members.map(_.name.toUpperCase(Locale.ENGLISH)).toSet ++
        alternateNames.keySet

    /**
     * The lenient rows that are plain spellings, as text.
     *
     * @return the spelling and replacement of each literal row
     */
    def literalLenientRows: List[(String, String)] =
      lenientPatterns.collect {
        case (expression, replacement) if isLiteralRow(expression, replacement) =>
          (expression.pattern.pattern(), replacement)
      }

    /**
     * The lenient rows that are patterns.
     *
     * @return the expression and replacement of each pattern-shaped row
     */
    def patternLenientRows: List[(Regex, String)] =
      lenientPatterns.filterNot {
        case (expression, replacement) => isLiteralRow(expression, replacement)
      }

    /**
     * The sources of the pattern-shaped lenient rows, as the production table spells them.
     *
     * @return the expression source of each pattern-shaped row
     */
    def patternSources: List[String] =
      patternLenientRows.map { case (expression, _) => expression.pattern.pattern() }

    override def toString: String = label

    private def isLiteralRow(expression: Regex, replacement: String): Boolean =
      !expression.pattern.pattern().exists(PatternMetacharacters.contains) &&
        !replacement.contains('$')
  }

  /**
   * Declares a closed family, widening its member type and taking its tables from its lookup.
   *
   * @param label  the name of the family
   * @param expectedMembers  the number of members the captured reference data fixes
   * @param valueOf  the family's exact lookup by name
   * @param parse  the family's lenient lookup by name
   * @param shadowedNames  the canonical names an earlier member of the family claims
   * @param expectedAlternateRows  the number of alternate spellings the configuration resource
 *   declared, which is zero for a family whose spellings come from its constant identifiers
 * @param expectedAlternateEntries  the size of the alternate-name table after the lookup has
 *   expanded it with the folded form of every declared spelling
 * @param enumNameSpellings  whether the family declares the spellings that the ported
 *   enumeration-name lookup derived from its constant identifiers
   * @param expectedExternals  the number of rows of each published group
   * @param expectedUnresolvedExternals  the spellings of each group naming no member
   * @param expectedLiteralLenient  the number of lenient rows that are plain spellings
   * @param expectedPatternLenient  the number of lenient rows that are patterns
   * @param lookup  the name lookup the family's companion publishes
   * @tparam A  the member type of the family
   * @return the family, ready to be swept
   */
  private def family[A <: Named](
      label: String,
      expectedMembers: Int,
      valueOf: String => Option[A],
      parse: String => ResultNec[A],
      shadowedNames: Set[String] = Set.empty,
      expectedAlternateRows: Int = 0,
      expectedAlternateEntries: Int = 0,
      enumNameSpellings: Boolean = false,
      expectedExternals: Map[String, Int] = Map.empty,
      expectedUnresolvedExternals: Map[String, Set[String]] = Map.empty,
      expectedLiteralLenient: Int = 0,
      expectedPatternLenient: Int = 0)(implicit lookup: NamedEnum[A]): Family =

    new Family(
      label = label,
      members = lookup.values.toList,
      valueOf = valueOf,
      parse = parse,
      familyName = lookup.familyName,
      lookupDescription = lookup.toString,
      alternateNames = lookup.alternateNames,
      byUpperName = lookup.byUpperName,
      byCanonicalName = lookup.byCanonicalName,
      lenientPatterns = lookup.lenientPatterns,
      externalNameGroups = lookup.externalNameGroups,
      externalNamesRaw = group => lookup.externalNamesRaw(group),
      externalNames = group => lookup.externalNames(group),
      expectedMembers = expectedMembers,
      shadowedNames = shadowedNames,
      expectedAlternateRows = expectedAlternateRows,
      expectedAlternateEntries = expectedAlternateEntries,
      enumNameSpellings = enumNameSpellings,
      expectedExternals = expectedExternals,
      expectedUnresolvedExternals = expectedUnresolvedExternals,
      expectedLiteralLenient = expectedLiteralLenient,
      expectedPatternLenient = expectedPatternLenient)

  /** Lifts a single-cause result into the accumulating shape the sweep compares. */
  private def toNec[A](result: FailureOr[A]): ResultNec[A] =
    result.fold(failure => Left(NonEmptyChain.one(failure)), value => Right(value))

  /**
   * Derives the constant identifier of a member from the name it renders.
   *
   * The three families that were plain Java enumerations name their members in camel case and
   * their constants in screaming snake case, and the ported enumeration-name lookup converted
   * between the two by inserting a separator before each capital after the first and folding the
   * result to upper case. The conversion is derived here rather than tabulated so that the
   * spellings asserted are a function of the members rather than a second copy of the table
   * being asserted.
   *
   * @param name  the name a member renders, in camel case
   * @return the constant identifier of that member, in screaming snake case
   */
  def screamingSnakeOf(name: String): String =
    name.zipWithIndex
      .map {
        case (character, index) =>
          if (index > 0 && character.isUpper) s"_$character" else character.toString
      }
      .mkString
      .toUpperCase(Locale.ENGLISH)

  /**
   * Every closed named family of this module, with the cardinality its reference data fixes.
   *
   * The numbers are those of `manifest/reference-data-manifest.json`, captured from the
   * implementation being ported, and they are written here as literals so that a row dropped
   * from a data table fails rather than quietly shrinking a family.
   */
  val families: TableFor1[Family] = Table(
    "family",
    family[Currency](
      label = "Currency",
      expectedMembers = 74,
      valueOf = name => Currency.valueOf(name),
      parse = name => toNec(Currency.parse(name))),
    family[DayCount](
      label = "DayCount",
      expectedMembers = 21,
      valueOf = name => DayCount.valueOf(name),
      parse = name => DayCount.parse(name),
      expectedExternals = Map("FpML" -> 14, "SWIFT" -> 8),
      expectedUnresolvedExternals = Map("FpML" -> Set("BUS/252")),
      expectedLiteralLenient = 58,
      expectedPatternLenient = 9),
    family[BusinessDayConvention](
      label = "BusinessDayConvention",
      expectedMembers = 7,
      valueOf = name => BusinessDayConvention.valueOf(name),
      parse = name => BusinessDayConvention.parse(name),
      expectedExternals = Map("FpML" -> 5, "SWIFT" -> 3),
      expectedLiteralLenient = 8,
      expectedPatternLenient = 3),
    family[RollConvention](
      label = "RollConvention",
      expectedMembers = 45,
      valueOf = name => RollConvention.valueOf(name),
      parse = name => RollConvention.parse(name),
      expectedExternals = Map("FpML" -> 44),
      expectedLiteralLenient = 1,
      expectedPatternLenient = 10),
    family[PeriodAdditionConvention](
      label = "PeriodAdditionConvention",
      expectedMembers = 3,
      valueOf = name => PeriodAdditionConvention.valueOf(name),
      parse = name => PeriodAdditionConvention.parse(name),
      expectedLiteralLenient = 3),
    family[DateSequence](
      label = "DateSequence",
      expectedMembers = 6,
      valueOf = name => DateSequence.valueOf(name),
      parse = name => DateSequence.parse(name)),
    family[StubConvention](
      label = "StubConvention",
      expectedMembers = 8,
      valueOf = name => StubConvention.valueOf(name),
      parse = name => StubConvention.parse(name),
      expectedAlternateEntries = 28,
      enumNameSpellings = true),
    family[FloatingRateType](
      label = "FloatingRateType",
      expectedMembers = 5,
      valueOf = name => FloatingRateType.valueOf(name),
      parse = name => FloatingRateType.parse(name),
      expectedAlternateEntries = 14,
      enumNameSpellings = true),
    family[ValueAdjustmentType](
      label = "ValueAdjustmentType",
      expectedMembers = 4,
      valueOf = name => ValueAdjustmentType.valueOf(name),
      parse = name => ValueAdjustmentType.parse(name),
      expectedAlternateEntries = 12,
      enumNameSpellings = true),
    family[IborIndex](
      label = "IborIndex",
      expectedMembers = 271,
      valueOf = name => IborIndex.valueOf(name),
      parse = name => IborIndex.parse(name),
      expectedAlternateRows = 1,
      expectedAlternateEntries = 1),
    family[OvernightIndex](
      label = "OvernightIndex",
      expectedMembers = 35,
      valueOf = name => OvernightIndex.valueOf(name),
      parse = name => OvernightIndex.parse(name),
      expectedAlternateRows = 10,
      expectedAlternateEntries = 13),
    family[PriceIndex](
      label = "PriceIndex",
      expectedMembers = 9,
      valueOf = name => PriceIndex.valueOf(name),
      parse = name => PriceIndex.parse(name)),
    family[FxIndex](
      label = "FxIndex",
      expectedMembers = 16,
      valueOf = name => FxIndex.valueOf(name),
      parse = name => FxIndex.parse(name),
      expectedAlternateRows = 1,
      expectedAlternateEntries = 1),
    family[FloatingRateName](
      label = "FloatingRateName",
      expectedMembers = 351,
      valueOf = name => FloatingRateName.valueOf(name),
      parse = name => toNec(FloatingRateName.parse(name)),
      shadowedNames = Set("DKK-DESTR-OIS COMPOUND", "SEK-SWESTR-OIS COMPOUND"))
  )

  /** The families by the label they are declared under. */
  val familiesByLabel: Map[String, Family] =
    families.map(entry => entry.label -> entry).toMap

  /** Every spelling the four index families answer to, which is what their unions can resolve. */
  lazy val allIndexNames: Set[String] =
    List("IborIndex", "OvernightIndex", "PriceIndex", "FxIndex")
      .map(familiesByLabel)
      .foldLeft(Set.empty[String])((claimed, family) => claimed ++ family.nameSpace)

  /**
   * Text that names no member of any family.
   *
   * Four shapes rather than one, because a family rejects text in one place and the shapes reach
   * it differently: ordinary text, the empty and blank strings that the ported `notNull` guard
   * has no counterpart for, and text long enough to carry punctuation a lenient pattern might
   * have matched.
   */
  val unknownNames: List[String] =
    List("Rubbish", "", "  ", "Zz9-Unknown-Family-Member")

  /**
   * Every alternate spelling the configuration resources declared, with the name it renames.
   *
   * Twelve rows across three families: the won certificate of deposit rate, named by its tenor in
   * weeks and in months; the ten retired and market spellings of the Overnight rates; and the
   * previous administrator of the dollar/rupee rate. Each is behaviour rather than configuration -
   * a trade written under the old name has to keep resolving - so each is driven through the
   * family it belongs to.
   */
  val alternateSpellings: TableFor3[String, String, String] = Table(
    ("family", "alternate spelling", "canonical name"),
    ("IborIndex", "KRW-CD-3M", "KRW-CD-13W"),
    ("OvernightIndex", "CLP-ICP", "CLP-TNA"),
    ("OvernightIndex", "DKK-Tom Next", "DKK-TNR"),
    ("OvernightIndex", "EUR-ESTER", "EUR-ESTR"),
    ("OvernightIndex", "EUR-EuroSTR", "EUR-ESTR"),
    ("OvernightIndex", "HKD-HONIX", "HKD-HONIA"),
    ("OvernightIndex", "JPY-TONA", "JPY-TONAR"),
    ("OvernightIndex", "USD-FED-FUNDS", "USD-FED-FUND"),
    ("OvernightIndex", "USD-FEDFUND", "USD-FED-FUND"),
    ("OvernightIndex", "USD-FEDFUNDS", "USD-FED-FUND"),
    ("OvernightIndex", "USD-Federal Funds", "USD-FED-FUND"),
    ("FxIndex", "USD/INR-RBIB-INR01", "USD/INR-FBIL-INR01")
  )

  /** The alternate spellings declared for one family, as the resource declared them. */
  def declaredAlternates(label: String): Set[String] =
    alternateSpellings.collect { case (family, spelling, _) if family == label => spelling }.toSet

  /**
   * One probe for each pattern-shaped lenient row, with the member the probe must reach.
   *
   * Twenty-two rows: nine day count patterns, ten roll convention patterns and three business day
   * convention patterns. A probe is a spelling that the row it names has to fire for - remove the
   * row and the probe stops resolving - which is the most a probe can claim where the rows chain,
   * as the day count rows do. The set of patterns probed here is held equal to the set the
   * production tables declare, so a new pattern row cannot arrive unprobed.
   */
  val patternLenientProbes: TableFor4[String, String, String, Named] = Table(
    ("family", "pattern", "spelling", "member"),
    ("DayCount", "ACTUAL/ACTUAL(.*)", "Actual/Actual ISDA", DayCount.ACT_ACT_ISDA),
    ("DayCount", "ACTUAL/(.*)", "Actual/360", DayCount.ACT_360),
    ("DayCount", "ACT/ACT(.*)", "ACT/ACT AFB", DayCount.ACT_ACT_AFB),
    ("DayCount", "ACT/(.*)", "ACT/364", DayCount.ACT_364),
    ("DayCount", "A/A(.*)", "A/A AFB", DayCount.ACT_ACT_AFB),
    ("DayCount", "A/(.*)", "A/360", DayCount.ACT_360),
    ("DayCount", "(.*)[(](.*)[)]", "Act/Act (ISDA)", DayCount.ACT_ACT_ISDA),
    ("DayCount", "(.*)[.]([A-Z])(.*)", "ACT/ACT.ISDA", DayCount.ACT_ACT_ISDA),
    ("DayCount", "(.*) ISMA", "Act/Act ISMA", DayCount.ACT_ACT_ICMA),
    ("RollConvention", "(Day_?)?31", "Day31", RollConventions.EOM),
    ("RollConvention", "(Day_?)?30", "Day_30", RollConventions.DAY_30),
    ("RollConvention", "(Day_?)?([1-2]?[0-9])", "15", RollConventions.DAY_15),
    ("RollConvention", "(Day_?)?MON", "MON", RollConventions.DAY_MON),
    ("RollConvention", "(Day_?)?TUE", "Day_TUE", RollConventions.DAY_TUE),
    ("RollConvention", "(Day_?)?WED", "WED", RollConventions.DAY_WED),
    ("RollConvention", "(Day_?)?THU", "Day_THU", RollConventions.DAY_THU),
    ("RollConvention", "(Day_?)?FRI", "FRI", RollConventions.DAY_FRI),
    ("RollConvention", "(Day_?)?SAT", "SAT", RollConventions.DAY_SAT),
    ("RollConvention", "(Day_?)?SUN", "Day_SUN", RollConventions.DAY_SUN),
    (
      "BusinessDayConvention",
      "Mod(ified)?[_ ]?(Follow(ing)?)?",
      "MOD_FOLLOW",
      BusinessDayConvention.ModifiedFollowing),
    (
      "BusinessDayConvention",
      "Mod(ified)?[_ ]?Preceding",
      "Mod Preceding",
      BusinessDayConvention.ModifiedPreceding),
    (
      "BusinessDayConvention",
      "Mod(ified)?[_ ]?(Follow(ing)?)?[_ ]?Bi[_ ]?Monthly",
      "ModFollowBiMonthly",
      BusinessDayConvention.ModifiedFollowingBiMonthly)
  )

  /**
   * The two pairs of published floating rate names that differ only in the case of one word.
   *
   * The published table declares each of these rates twice, and both spellings are names of their
   * own. They are the only reason any family of this library has a member absent from its
   * normalised view, and the reason the published name family probes its own table before the
   * shared name lookup.
   */
  val foldedNamePairs: List[(String, String)] =
    List(
      ("DKK-DESTR-OIS Compound", "DKK-DESTR-OIS COMPOUND"),
      ("SEK-SWESTR-OIS Compound", "SEK-SWESTR-OIS COMPOUND"))

  /**
   * The 41 named constants the published floating rate name family exposes.
   *
   * Written out because this port performs no reflection and so cannot enumerate the constants of
   * an object, and because a constant that ceased to be a member of the family would otherwise
   * still compile.
   */
  val floatingRateNameConstants: List[FloatingRateName] =
    List(
      FloatingRateNames.GBP_LIBOR,
      FloatingRateNames.USD_LIBOR,
      FloatingRateNames.USD_BSBY,
      FloatingRateNames.CHF_LIBOR,
      FloatingRateNames.EUR_LIBOR,
      FloatingRateNames.JPY_LIBOR,
      FloatingRateNames.EUR_EURIBOR,
      FloatingRateNames.AUD_BBSW,
      FloatingRateNames.CAD_CDOR,
      FloatingRateNames.CZK_PRIBOR,
      FloatingRateNames.DKK_CIBOR,
      FloatingRateNames.HUF_BUBOR,
      FloatingRateNames.MXN_TIIE,
      FloatingRateNames.NOK_NIBOR,
      FloatingRateNames.NZD_BKBM,
      FloatingRateNames.PLN_WIBOR,
      FloatingRateNames.SEK_STIBOR,
      FloatingRateNames.ZAR_JIBAR,
      FloatingRateNames.GBP_SONIA,
      FloatingRateNames.USD_FED_FUND,
      FloatingRateNames.USD_SOFR,
      FloatingRateNames.CHF_SARON,
      FloatingRateNames.CHF_TOIS,
      FloatingRateNames.EUR_EONIA,
      FloatingRateNames.EUR_ESTR,
      FloatingRateNames.EUR_ESTER,
      FloatingRateNames.JPY_TONAR,
      FloatingRateNames.AUD_AONIA,
      FloatingRateNames.BRL_CDI,
      FloatingRateNames.CAD_CORRA,
      FloatingRateNames.DKK_TNR,
      FloatingRateNames.NOK_NOWA,
      FloatingRateNames.PLN_POLONIA,
      FloatingRateNames.PLN_POLSTR,
      FloatingRateNames.SEK_SIOR,
      FloatingRateNames.THB_THOR,
      FloatingRateNames.USD_FED_FUND_AVG,
      FloatingRateNames.GB_RPI,
      FloatingRateNames.EU_EXT_CPI,
      FloatingRateNames.US_CPI_U,
      FloatingRateNames.FR_EXT_CPI)
}
