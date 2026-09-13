/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import java.lang.reflect.Modifier
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
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Holds every closed named family of this module, and the failure reasons of `strata-collect`, to
 * the name space its published reference data fixes: every member answers to its own name, and
 * every transcribed alternate spelling, protocol spelling and lenient row reaches the member it
 * names. This is the specification the Rule 4 acceptance gate for closed enumerations runs.
 *
 * It also holds every text any family declares - canonical and folded names, alternate and
 * protocol spellings, lenient sources and replacements - inside that family's own lenient length
 * ceiling, since a row beyond it would be one nothing could resolve.
 *
 * ===Exhaustive rather than sampled===
 *
 * Every family here is a closed, finite set, so each property iterates the whole of `values` and
 * the whole of each transcribed table: a hand-picked subset would pass with a row missing. The
 * lenient tables are the one place a table cannot simply be iterated, a row's left-hand side being
 * a pattern rather than a spelling; the rows are therefore partitioned into the literal ones,
 * driven from the production table itself, and the pattern-shaped ones, driven by a probe written
 * for each, and the two counts are asserted to add up to the table's size, so a row can hide in
 * neither part.
 *
 * ===Cardinalities===
 *
 * Each family is declared with the number of members its reference data fixes, asserted against
 * the production `values` as a literal rather than derived from it: `values.size shouldBe
 * values.size` would pass with a row dropped. The fifteen families hold 865 members - 74
 * currencies, 21 standard day counts, 7 business day conventions, 45 roll conventions, 3 period
 * addition conventions, 6 date sequences, 8 stub conventions, 5 floating rate types, 4 value
 * adjustment types, 271 Ibor, 35 Overnight, 9 price and 16 FX indices, 351 published floating
 * rate names and 10 failure reasons - and the 30 built-in holiday calendars are swept beside them.
 *
 * Three figures need a note. The day counts are 21 and not 22 because a `Bus/252` convention
 * exists per holiday calendar rather than per family, so that set is open and is reached through a
 * factory. The floating rate names are the 351 rows the published table declares, of which 41 are
 * named constants, each figure asserted where it belongs. The failure reasons are ten because ten
 * are the constants of the type.
 *
 * A currency code outside the published 74 is reported as a failure rather than resolved, and so
 * is a currency pair no published FX index quotes; both are asserted as behaviour below.
 *
 * @see [[ReferenceDataManifestSpec]] for the transcription of the same tables against the captured
 *   manifest
 */
class NamedEnumClosedSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  import NamedEnumClosedSpec._

  test("the family table is the whole inventory of closed named families") {
    // The sweeps below are only as exhaustive as this table: a family missing from it has no
    // coverage rather than a failing assertion, so the inventory is a literal.
    families.map(_.label).toSet shouldBe
      Set(
        "Currency",
        "DayCount",
        "BusinessDayConvention",
        "RollConvention",
        "PeriodAdditionConvention",
        "DateSequence",
        "StubConvention",
        "FloatingRateType",
        "ValueAdjustmentType",
        "IborIndex",
        "OvernightIndex",
        "PriceIndex",
        "FxIndex",
        "FloatingRateName",
        "FailureReason")
    families.size shouldBe 15
    families.map(_.label).distinct.size shouldBe 15

    // 865 members in total, which is the size of the name space every property below iterates.
    families.map(_.expectedMembers).sum shouldBe 865
    families.map(_.members.size).sum shouldBe 865
  }

  test("every closed family holds exactly the members its reference data fixes") {
    forAll(families) { (family: Family) =>
      withClue(s"${family.label}: ") {
        family.members should not be empty
        family.members.size shouldBe family.expectedMembers

        // Distinct canonical names are what makes a name an identity: a shared one would leave
        // the second member unreachable by it and absent from the normalised view.
        family.members.map(_.name).distinct.size shouldBe family.expectedMembers

        family.familyName shouldBe family.label
        family.lookupDescription shouldBe s"NamedEnum[${family.label}]"
      }
    }
  }

  test("every closed family is closed on the JVM, and not only in the source that seals it") {
    // `sealed` and a `private[pkg]` constructor are checked by this compiler and by nothing else.
    // Neither survives into the class file: a family's base class is compiled to an ordinary
    // public abstract class with a public constructor, and the JVM of this language version has
    // no way to record which subclasses were permitted. A class compiled against these class
    // files by another language could therefore declare itself a member of a family - a currency
    // with invented minor units, an index with an invented calendar, a convention with invented
    // adjustment behaviour - and would be accepted everywhere the family's type is accepted while
    // resolving through none of the lookups swept above. That is precisely the dynamic membership
    // this port removed, re-entering below the level the rest of this file tests.
    //
    // The base class of every family therefore guards its own construction, which every subtype
    // must run, and this sweep holds the whole of that: the class of every published member is one
    // the family itself declared, the guard admits each of them, and a value the family did not
    // declare is refused. The read path is asserted alongside it, because a member reconstructed
    // by `java.io.ObjectInputStream` would be a second way into the same set.
    forAll(families) { (family: Family) =>
      withClue(s"${family.label}: ") {
        val root: Class[_] =
          familyRoot(family.label, family.members.head).getOrElse(
            fail(s"the base class of ${family.label} was not found above its first member"))
        withClue("the family is headed by an abstract class, whose constructor runs the guard: ")(
          Modifier.isAbstract(root.getModifiers) shouldBe true)

        family.members.foreach { member =>
          // `Named` is a universal trait, so a member is statically only an `Any`; every member of
          // every family here is a reference value, which the ascription states once
          val instance: AnyRef = member.asInstanceOf[AnyRef]
          val implementation: Class[_] = instance.getClass
          withClue(s"${member.name} refuses the read path of java.io.ObjectInputStream: ")(
            classOf[NoJavaSerialization].isAssignableFrom(implementation) shouldBe true)
          withClue(s"${member.name} is an instance of a class ${family.label} itself declares: ") {
            declaredIn(implementation, root) shouldBe true
            // final, or the singleton class of a `case object`, which cannot be extended either
            (Modifier.isFinal(implementation.getModifiers) ||
              implementation.getName.endsWith("$")) shouldBe true
            JvmClosure.requireDeclaredMember(instance, root)
          }
        }

        // and the negative half, which is what a subtype compiled elsewhere would meet: a value
        // whose class the family did not declare cannot finish construction
        val refused: IllegalArgumentException =
          intercept[IllegalArgumentException](JvmClosure.requireDeclaredMember(new AnyRef, root))
        refused.getMessage should include(s"${root.getName} is a closed family")
        refused.getMessage should include("is not one of its published members")
      }
    }
  }

  test("every member of every closed family resolves to itself by its canonical name") {
    // The round trip closed enumerations turn on: the name a member renders is the name that
    // reaches it. Both entry points are asserted, since a family resolving through one but not
    // the other would serialize to text it could not read back.
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
    // Each member is registered twice, under its own name and under the English upper case of it,
    // first registration winning - which is why `ACT/365F` names the day count `Act/365F`.
    forAll(families) { (family: Family) =>
      family.members.foreach { member =>
        val folded = member.name.toUpperCase(Locale.ENGLISH)
        withClue(s"${family.label} $folded: ") {
          family.valueOf(folded).map(_.name.toUpperCase(Locale.ENGLISH)) shouldBe Some(folded)
          family.parse(folded) should beSuccess

          // A canonical name is registered unconditionally, so a folded key is claimed by the
          // member whose canonical name it is and by the first to offer it otherwise.
          if (family.foldedNameCollisions.contains(folded) && member.name != folded) {
            family.valueOf(folded).map(_.name) shouldBe Some(folded)
            family.valueOf(folded) should not be Some(member)
          } else {
            family.valueOf(folded) shouldBe Some(member)
          }
        }
      }
    }
  }

  test("the canonical-name view of every family is keyed as its members render themselves") {
    // `byCanonicalName` re-keys the lookup by each value's canonical name, *not* folded to upper
    // case, and is the view a caller iterates to obtain the family.
    forAll(families) { (family: Family) =>
      withClue(s"${family.label}: ") {
        family.byCanonicalName.foreach {
          case (key, value) => withClue(s"key $key: ")(key shouldBe value.name)
        }

        // One key per member, with no exception, against the declared cardinality.
        family.byCanonicalName.size shouldBe family.expectedMembers
        family.byCanonicalName.keySet shouldBe family.members.map(_.name).toSet

        family.members.foreach { member =>
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

        // The one view that can hold fewer entries than the family has members: names differing in
        // case alone share a folded key, so the count is short by the declared collisions.
        family.byUpperName.size shouldBe
          family.expectedMembers - family.foldedNameCollisions.size
      }
    }
  }

  test("no closed family resolves text that names no member") {
    // Unrecognised text is reported as a value. The reason is compared as a member of the closed
    // family of reasons rather than by matching the message, so wording is free to change.
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
  // The transcribed tables. Every row is a literal in a companion, so a row that failed to be
  // transcribed would change what resolves without changing any type.

  test("the external name groups hold the rows the configuration resources declared") {
    forAll(families) { (family: Family) =>
      withClue(s"${family.label}: ") {
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

              // The row count as a literal: an external row takes part in no lookup, so nothing
              // else catches a dropped protocol spelling.
              raw.size shouldBe expectedRows

              // The resolved view drops a row whose canonical name nothing reaches, so equal key
              // sets state that no transcribed spelling points at a name that has gone.
              resolved.keySet shouldBe raw.keySet

              // Every resolved row reaches the value its canonical name identifies: a member,
              // except the one declared row the day counts resolve onto a per-calendar
              // `Bus/252` convention, which is outside `values`.
              val beyondValues = family.expectedExternalsBeyondValues.getOrElse(group, Set.empty)
              resolved.foreach {
                case (spelling, value) =>
                  withClue(s"row $spelling: ") {
                    family.valueOf(raw(spelling)) shouldBe Some(value)
                    value.name shouldBe raw(spelling)
                    if (beyondValues.contains(spelling)) {
                      family.members should not contain value
                    } else {
                      family.members should contain(value)
                    }
                  }
              }

              // And every row declared to resolve beyond the members does so.
              beyondValues.foreach { spelling =>
                withClue(s"row beyond values $spelling: ") {
                  raw.keySet should contain(spelling)
                  family.members.map(_.name) should not contain raw(spelling)
                  resolved.get(spelling).map(_.name) shouldBe Some(raw(spelling))
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
        // The whole table, as a literal count. The order of these rows is behaviour - a later
        // pattern is applied to what an earlier one produced - and is exercised further down.
        family.lenientRows.size shouldBe
          family.expectedLiteralLenient + family.expectedPatternLenient

        // The partition the two tests after this one drive. Asserting both parts against literals
        // and their sum against the table stops a row hiding in the part neither test covers.
        family.literalLenientRows.size shouldBe family.expectedLiteralLenient
        family.patternLenientRows.size shouldBe family.expectedPatternLenient
        (family.literalLenientRows.size + family.patternLenientRows.size) shouldBe
          family.lenientRows.size
      }
    }
  }

  test("every literal lenient row rewrites its own spelling to the member it names") {
    // Driven from the production table rather than a copy of it, so a row added there is exercised
    // here without this file changing. A literal row holds no pattern metacharacter on its left
    // and no group reference on its right: a plain claim that this spelling names that member.
    forAll(families) { (family: Family) =>
      family.literalLenientRows.foreach {
        case (spelling, replacement) =>
          withClue(s"${family.label} $spelling -> $replacement: ") {
            val expected = family.valueOf(replacement).getOrElse(
              fail(s"${family.label} lenient row $spelling names the unknown member $replacement"))

            // The chain may rewrite the row's output further - several day count rows do - so the
            // assertion is against the member finally reached rather than the intermediate text.
            family.parse(spelling) should haveValue(expected)

            // The lenient stage folds its input and matches every pattern without regard to case,
            // so a spelling resolves however it is written - which is why `act/360` resolves.
            family.parse(spelling.toUpperCase(Locale.ENGLISH)) should haveValue(expected)
            family.parse(spelling.toLowerCase(Locale.ENGLISH)) should haveValue(expected)
          }
      }
    }
  }

  test("every pattern-shaped lenient row is exercised by a spelling that requires it") {
    // The rows a table cannot be iterated over, their left-hand side being a pattern: one probe
    // each, held below equal to the pattern-shaped rows the production table declares.
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

  test("every family resolves exactly what a naive pass over all of its lenient rows resolves") {
    // The lenient stage does not offer text to a row whose own expression could not match it,
    // and this is the property that makes the narrowing invisible: for every family, and for
    // every text that family's own data can produce, the chain agrees with a reference that
    // compiles each row the way the production rule compiles it and applies all of them in
    // order, screening nothing. The reference is the ported algorithm written out, so the
    // comparison is against the specification of the chain rather than against another copy of
    // the implementation.
    //
    // Agreement is asserted at both of the places a caller can observe it: the chain itself,
    // and the lookup's lenient parse, whose algorithm is reproduced here around the reference
    // chain - the exact lookup, then the length bound, then the fold to upper case and the
    // chain, then the exact lookup again. The family's own `parse` is held to the reference
    // where the reference resolves; it is not held to a failure where the reference fails,
    // because two families deliberately resolve more than their shared lookup does - the day
    // counts add the `Bus/252` conventions and the published floating rate names add a
    // concrete index name, both beyond the closed members.
    forAll(families) { (family: Family) =>
      val reference = naiveLenientRules(family.lenientRows)
      val corpus = lenientCorpus(family)
      withClue(s"${family.label}: ") {
        corpus should not be empty

        corpus.foreach { text =>
          withClue(s"text [$text]: ") {
            val rewritten = naiveRewrite(reference, family.lenientLengthCeiling, text)
            family.lookupRewriteLeniently(text) shouldBe rewritten

            val resolved =
              family.lookupValueOf(text).orElse(
                if (text.length > family.lenientLengthCeiling) None
                else
                  family.lookupValueOf(
                    naiveRewrite(
                      reference,
                      family.lenientLengthCeiling,
                      text.toUpperCase(Locale.ENGLISH))))

            resolved match {
              case Some(member) =>
                family.lookupParse(text) should haveValue(member)
                if (family.ownParseIsLenient) {
                  family.parse(text) should haveValue(member)
                }
              case None =>
                family.lookupParse(text) should beFailure
            }
          }
        }

        // The sweep is only worth running while the chain actually fires over the corpus, so
        // the four families that declare rows are held to having rewritten some of it. A
        // family that declares none is the identity over every text, which is the other half
        // of the same statement and is asserted by the comparison above.
        val rewrittenTexts = corpus.count { text =>
          val folded = text.toUpperCase(Locale.ENGLISH)
          naiveRewrite(reference, family.lenientLengthCeiling, folded) != folded
        }
        if (family.lenientRows.isEmpty) {
          rewrittenTexts shouldBe 0
        } else {
          rewrittenTexts should be > 0
        }
        info(
          s"${family.label}: ${corpus.size} texts swept against ${family.lenientRows.size} " +
            s"reference rows, $rewrittenTexts of them rewritten by the chain")
      }
    }

    // The one family whose own `parse` is not the lenient parse of its lookup, asserted rather
    // than only declared: the published floating rate names resolve a name exactly, or as a
    // concrete index name, and fold no case, so the sweep holds that family's own entry point to
    // nothing. Were it to become lenient, this assertion is what says so.
    familiesByLabel("FloatingRateName").ownParseIsLenient shouldBe false
    families.filterNot(candidate => candidate.ownParseIsLenient).map(_.label).toSet shouldBe
      Set("FloatingRateName")
    FloatingRateName.valueOf("CHF-LIBOR").map(_.name) shouldBe Some("CHF-LIBOR")
    toNec(FloatingRateName.parse("chf-libor")) should beFailure
    FloatingRateName.namedEnum.parse("chf-libor") should haveValue(FloatingRateNames.CHF_LIBOR)

    // The two day-count chains that are the canaries of the narrowed pass, named rather than
    // left inside the sweep: each reaches its member only by a row rewriting a character that
    // decides which rows the rest of the pass considers.
    DayCount.namedEnum.rewriteLeniently("ACT/ACT.ISMA") shouldBe "Act/Act ICMA"
    DayCount.namedEnum.rewriteLeniently("A/A ISMA") shouldBe "Act/Act ICMA"
    DayCount.parse("ACT/ACT.ISMA") should haveValue(DayCount.ACT_ACT_ICMA)
    DayCount.parse("A/A ISMA") should haveValue(DayCount.ACT_ACT_ICMA)
  }

  test("the lenient rewrites are reached only after the exact lookup has missed") {
    // The exact stage consults the alternate names and the two registered keys of each member and
    // rewrites nothing; the lenient stage then folds case and applies the patterns. The
    // discriminator is a spelling one accepts and the other does not.
    DayCount.valueOf("ACT/360") shouldBe Some(DayCount.ACT_360)
    DayCount.valueOf("ACT_360") shouldBe None
    DayCount.parse("ACT_360") should haveValue(DayCount.ACT_360)
    DayCount.valueOf("Actual/360") shouldBe None
    DayCount.parse("Actual/360") should haveValue(DayCount.ACT_360)

    DayCount.parse("ACT/360") should haveValue(DayCount.ACT_360)

    BusinessDayConvention.valueOf("MF") shouldBe None
    BusinessDayConvention.parse("MF") should haveValue(BusinessDayConvention.ModifiedFollowing)

    RollConvention.valueOf("Day31") shouldBe None
    RollConvention.parse("Day31") should haveValue(RollConventions.EOM)

    PeriodAdditionConvention.valueOf("LAST_DAY") shouldBe None
    PeriodAdditionConvention.parse("LAST_DAY") should haveValue(PeriodAdditionConvention.LAST_DAY)

    // Because the exact stage runs first and claims every canonical name, no pattern can displace
    // a member's own name. Stated for the family whose patterns are most eager.
    DayCount.values.toList.foreach { dayCount =>
      withClue(s"${dayCount.name}: ") {
        DayCount.valueOf(dayCount.name) shouldBe Some(dayCount)
        DayCount.parse(dayCount.name) should haveValue(dayCount)
      }
    }
  }

  test("no text any family declares is beyond that family's lenient length ceiling") {
    // The lenient stage of a lookup is bounded: text longer than the ceiling the family derives
    // from its own data is reported rather than folded and rewritten, which is what keeps the
    // cost of rejecting a name a constant of the family. `collect.NamedEnumSpec` owns the
    // mechanism - how the ceiling is derived and that it is applied at both entry points - and
    // this is the property that has to hold of the real data: every text a transcribed table
    // holds is within the ceiling of the family that holds it, so no row of any resource this
    // port turned into code can have become unresolvable for its length.
    //
    // It is the table rows rather than the resolvable spellings that are swept, and deliberately
    // so. A caller offers the spelling a resource declares, or a case variant of it, and case
    // folding preserves length; the margin the typeclass adds above the longest declared datum
    // is what covers the spaced and screaming-snake spellings a caller may offer beyond that.
    // Sweeping the rows is therefore the strongest statement available from the data alone, and
    // it is the statement a row added later has to keep true.
    forAll(families) { (family: Family) =>
      withClue(s"${family.label}: ") {
        val externalRows = family.externalNameGroups.toList.flatMap { group =>
          family.externalNamesRaw(group).toList.flatMap(_.toList).flatMap {
            case (spelling, canonicalName) =>
              List(s"external spelling of $group" -> spelling, s"external target of $group" -> canonicalName)
          }
        }
        val declared: List[(String, String)] =
          family.members.map(member => "canonical name" -> member.name) :::
            family.members.map(member =>
              "folded canonical name" -> member.name.toUpperCase(Locale.ENGLISH)) :::
            family.alternateNames.toList.flatMap {
              case (spelling, canonicalName) =>
                List("alternate spelling" -> spelling, "alternate target" -> canonicalName)
            } :::
            externalRows :::
            family.lenientRows.flatMap {
              case (source, replacement) =>
                List("lenient source" -> source, "lenient replacement" -> replacement)
            }

        // Every row of every table, named by the table it came from so that a failure says which
        // row is the one that outgrew the ceiling.
        declared.foreach {
          case (kind, text) =>
            withClue(s"$kind '$text' of ${text.length} characters: ") {
              text.length should be <= family.lenientLengthCeiling
            }
        }

        // The ceiling of a family is a property of its data, so the figures are reported rather
        // than tabulated here: a ceiling and the longest text the family declares, per family,
        // which is the evidence a reader of this run needs to see that the margin is real room
        // and not a coincidence.
        val (widestKind, widestText) =
          declared.maxBy { case (_, text) => text.length }
        info(
          s"${family.label}: ceiling ${family.lenientLengthCeiling}, longest declared text " +
            s"${widestText.length} ($widestKind '$widestText'), room " +
            s"${family.lenientLengthCeiling - widestText.length}")

        // And the ceiling leaves room above that longest text rather than merely reaching it,
        // which is what makes a case variant or a spaced spelling of a declared row resolvable
        // too. The room is the margin the typeclass adds, so it is the same for every family;
        // asserting that it is present, without naming it, keeps this file free of a second copy
        // of a number the typeclass owns.
        family.lenientLengthCeiling should be > widestText.length
      }
    }
  }

  test("no closed family declares both an alternate-name table and a lenient table") {
    // The alternate names are consulted before anything is rewritten, so a spelling present in
    // both tables would resolve through the alternate name. No family declares both.
    forAll(families) { (family: Family) =>
      withClue(s"${family.label}: ") {
        (family.alternateNames.nonEmpty && family.lenientRows.nonEmpty) shouldBe false
      }
    }

    // Six families carry alternate spellings - the three index families, whose spellings are
    // retired and market names, and the three constant families, whose spellings come from their
    // constant identifiers - and four carry lenient patterns, the two sets disjoint.
    families.filter(candidate => candidate.alternateNames.nonEmpty).map(_.label).toSet shouldBe
      Set(
        "IborIndex",
        "OvernightIndex",
        "FxIndex",
        "StubConvention",
        "FloatingRateType",
        "ValueAdjustmentType")
    families.filter(candidate => candidate.lenientRows.nonEmpty).map(_.label).toSet shouldBe
      Set("DayCount", "RollConvention", "BusinessDayConvention", "PeriodAdditionConvention")
    families.filter(candidate => candidate.enumNameSpellings).map(_.label).toSet shouldBe
      Set("StubConvention", "FloatingRateType", "ValueAdjustmentType")
  }

  test("every alternate spelling a family declares reaches a member without shadowing a name") {
    // Every entry of every alternate-name table, expanded spellings included. A spelling is
    // substituted before either stage consults the keys, so an entry keyed by a canonical name
    // would divert that name to another member.
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
    // Three families accept three further spellings of a member's name, derived from its constant
    // identifier. The spellings are a table rather than a derivation, so they are asserted by
    // deriving them again - a lost row fails here, not when a caller sends one.
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

        family.valueOf(spelling) shouldBe Some(expected)
        family.parse(spelling) should haveValue(expected)
        family.alternateNames.get(spelling) shouldBe Some(canonicalName)

        // And it is a retired or market name rather than a member of its own.
        family.members.map(_.name) should not contain spelling
        family.byCanonicalName.keySet should not contain spelling
      }
    }

    // The accounting: the rows probed above are the rows the tables declare, family by family.
    forAll(families) { (family: Family) =>
      withClue(s"${family.label}: ") {
        alternateSpellings.count { case (label, _, _) => label == family.label } shouldBe
          family.expectedAlternateRows
      }
    }
  }

  test("the expanded alternate-name tables are the declared rows plus their folded spellings") {
    // The lookup holds each declared spelling folded to upper case as well, which is what lets an
    // alternate name be reached from `parse`. Asserted as whole maps, so no row is invented.
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

    // Nine families declare none, so the name space of each is its canonical names, their folded
    // forms, and whatever its lenient table rewrites into one of those.
    val withoutAlternates =
      Set(
        "Currency",
        "DayCount",
        "BusinessDayConvention",
        "RollConvention",
        "PeriodAdditionConvention",
        "DateSequence",
        "PriceIndex",
        "FloatingRateName",
        "FailureReason")
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
    // An alternate spelling is registered under the spelling declared and under the folded form of
    // it, which is why the mixed-case rows resolve from text a caller typed in any case.
    forAll(alternateSpellings) { (label: String, spelling: String, canonicalName: String) =>
      val family = familiesByLabel(label)
      withClue(s"$label $spelling: ") {
        val expected = family.valueOf(canonicalName).getOrElse(
          fail(s"$label has no member named $canonicalName"))

        family.valueOf(spelling.toUpperCase(Locale.ENGLISH)) shouldBe Some(expected)
        family.parse(spelling.toUpperCase(Locale.ENGLISH)) should haveValue(expected)

        family.parse(spelling.toLowerCase(Locale.ENGLISH)) should haveValue(expected)
      }
    }
  }

  test("the lenient rewrites chain, each pattern seeing what the one before it produced") {
    // Two chains through the day count table, the only one deep enough to have them. Each
    // intermediate form resolves to nothing on its own, so the chain is doing the work and
    // reordering the transcribed rows would break both.

    // `ACTUAL/ACTUAL(.*)` expands the long spelling, then `(.*)[(](.*)[)]` removes the brackets.
    DayCount.valueOf("Actual/Actual (ISDA)") shouldBe None
    DayCount.valueOf("Act/Act (ISDA)") shouldBe None
    DayCount.valueOf("Act/Act ISDA") shouldBe Some(DayCount.ACT_ACT_ISDA)
    DayCount.parse("Actual/Actual (ISDA)") should haveValue(DayCount.ACT_ACT_ISDA)

    // Three patterns in order: `ACT/ACT(.*)` normalises the abbreviation, `(.*)[.]([A-Z])(.*)`
    // spaces the dotted qualifier, `(.*) ISMA` renames the retired one. The protocol table takes
    // part in no lookup, so this chain is what reaches the FpML spelling.
    DayCount.valueOf("ACT/ACT.ISMA") shouldBe None
    DayCount.valueOf("Act/Act.ISMA") shouldBe None
    DayCount.valueOf("Act/Act ISMA") shouldBe None
    DayCount.valueOf("Act/Act ICMA") shouldBe Some(DayCount.ACT_ACT_ICMA)
    DayCount.parse("ACT/ACT.ISMA") should haveValue(DayCount.ACT_ACT_ICMA)

    DayCount.parse("A/A ISMA") should haveValue(DayCount.ACT_ACT_ICMA)
  }

  //-------------------------------------------------------------------------
  // The families whose closedness needs a statement of its own: one closed over its standard
  // members while a second, open kind is reached through a factory, one whose reference data is a
  // set of calendars, and the two that report a name outside their published set.

  test("the day count family is closed over its standard members while Bus/252 is per calendar") {
    // Only the standard conventions are a closed set, so `values` is those 21 and the family's
    // own lookup knows nothing of `Bus/252`.
    DayCount.values.length shouldBe 21
    DayCount.values.toList.map(_.name) should not contain "Bus/252 BRBD"
    DayCount.namedEnum.values.toList shouldBe DayCount.values.toList
    DayCount.namedEnum.valueOf("Bus/252 BRBD") shouldBe None
    DayCount.namedEnum.byCanonicalName.keySet should not contain "Bus/252 BRBD"

    // The per-calendar conventions are reached through two factories, the total one taking the
    // resolved calendar so that its convention is a pure function of its argument.
    DayCount.ofBus252(StandardHolidayCalendars.BRBD).name shouldBe "Bus/252 BRBD"
    DayCount.ofBus252(StandardHolidayCalendars.GBLO).name shouldBe "Bus/252 GBLO"
    DayCount.ofBus252(HolidayCalendarId.of("BRBD"), ReferenceData.standard) should
      haveValue(DayCount.ofBus252(StandardHolidayCalendars.BRBD))
    DayCount.ofBus252(HolidayCalendarId.of("GBXX"), ReferenceData.standard) should
      beFailureWith(FailureReason.MISSING_DATA)

    // Reading a `Bus/252` name resolves its calendar against the built-in calendars rather than
    // ambient reference data. The prefix is matched without regard to case; the calendar name
    // after it against the canonical and folded names, so `GBLO` resolves and `gblo` does not.
    DayCount.valueOf("Bus/252 BRBD").map(_.name) shouldBe Some("Bus/252 BRBD")
    DayCount.valueOf("BUS/252 BRBD").map(_.name) shouldBe Some("Bus/252 BRBD")
    DayCount.parse("Bus/252 BRBD").map(_.name) shouldBe Right("Bus/252 BRBD")
    DayCount.parse("BUS/252 GBLO").map(_.name) shouldBe Right("Bus/252 GBLO")
    DayCount.parse("bus/252 GBLO").map(_.name) shouldBe Right("Bus/252 GBLO")

    // A lower-case calendar name is reported with the calendar lookup's failure rather than the
    // family's: the `Bus/252` route claims the text on its case-insensitive prefix, so the
    // lenient stage is never reached for such a name.
    DayCount.valueOf("Bus/252 gblo") shouldBe None
    DayCount.parse("Bus/252 gblo") should beFailureWith(FailureReason.PARSING)
    DayCount.parse("Bus/252 gblo") should haveFailureMessageMatching(
      ".*HolidayCalendar name not found.*")

    // The bare prefix is a lenient row defaulted to the Brazilian calendar.
    DayCount.parse("Bus/252").map(_.name) shouldBe Right("Bus/252 BRBD")
    DayCount.parse("BUS/252").map(_.name) shouldBe Right("Bus/252 BRBD")

    // A name whose calendar this library does not define is reported rather than invented.
    DayCount.valueOf("Bus/252 GBXX") shouldBe None
    DayCount.parse("Bus/252 GBXX") should beFailureWith(FailureReason.PARSING)
    DayCount.parse("Bus/252 GBXX", ReferenceData.standard) should
      beFailureWith(FailureReason.MISSING_DATA)

    // The one external row in the library that names a value outside `values`: the FpML spelling
    // of the Brazilian convention. It resolves, so the resolved group is the whole table, 14 rows
    // of 14, rather than dropping the row for not naming one of the 21.
    DayCount.namedEnum.externalNamesRaw("FpML").flatMap(_.get("BUS/252")) shouldBe
      Some("Bus/252 BRBD")
    DayCount.namedEnum.externalNamesRaw("FpML").map(_.size) shouldBe Some(14)
    DayCount.namedEnum.externalNames("FpML").map(_.size) shouldBe Some(14)
    DayCount.namedEnum.externalNames("FpML").map(_.keySet) shouldBe
      DayCount.namedEnum.externalNamesRaw("FpML").map(_.keySet)
    DayCount.namedEnum.externalNames("FpML").flatMap(_.get("BUS/252")).map(_.name) shouldBe
      Some("Bus/252 BRBD")

    // And it resolves without reopening the family: the convention it names is still no member.
    val resolvedBus252 = DayCount.namedEnum.externalNames("FpML").flatMap(_.get("BUS/252")).getOrElse(
      fail("the FpML group no longer resolves the BUS/252 row"))
    DayCount.values.toList should not contain resolvedBus252
    DayCount.namedEnum.values.toList should not contain resolvedBus252
    DayCount.valueOf("Bus/252 BRBD") shouldBe Some(resolvedBus252)
    DayCount.parse("BUS/252").map(_.name) shouldBe Right("Bus/252 BRBD")
  }

  test("every built-in holiday calendar is reachable by its identifier and resolves as standard") {
    val builtIn = StandardHolidayCalendars.all
    builtIn.size shouldBe 30

    builtIn.foreach {
      case (id, calendar) =>
        withClue(s"${id.name}: ") {
          calendar.name shouldBe id.name
          HolidayCalendarId.of(calendar.name) shouldBe id

          // Both name views reach it: the canonical one, keyed by the name the calendar carries
          // and used by its JSON form, and the folded one, keyed by that name in upper case.
          StandardHolidayCalendars.byName(calendar.name) shouldBe Some(calendar)
          StandardHolidayCalendars.byUpperName(calendar.name.toUpperCase(Locale.ENGLISH)) shouldBe
            Some(calendar)

          // Those two key spaces, and no third, are what the name route consults, matching the
          // text it is given without folding it. The folded view above tolerates any case, but it
          // is a view rather than the route a name takes.
          HolidayCalendars.of(calendar.name.toUpperCase(Locale.ENGLISH)) should haveValue(calendar)
          HolidayCalendars.of(calendar.name.toLowerCase(Locale.ENGLISH)) should
            beFailureWith(FailureReason.PARSING)

          // Both routes reach it: the name short cut, and the reference data route.
          HolidayCalendars.of(calendar.name) should haveValue(calendar)
          id.resolve(ReferenceData.standard) should haveValue(calendar)
          ReferenceData.standard.containsValue(id) shouldBe true
        }
    }

    // The 30 identifiers are distinct, so no calendar displaced another while the set was built.
    builtIn.keySet.size shouldBe 30
    builtIn.values.map(_.name).toSet.size shouldBe 30

    // A name this library does not define is empty from the views and a reported failure from the
    // two routes with an error channel. The reasons differ by design: unknown text cannot be
    // parsed, while an identifier absent from reference data is data the caller did not supply.
    StandardHolidayCalendars.byName("GBXX") shouldBe None
    StandardHolidayCalendars.byUpperName("GBXX") shouldBe None
    HolidayCalendars.of("GBXX") should beFailureWith(FailureReason.PARSING)
    HolidayCalendarId.of("GBXX").resolve(ReferenceData.standard) should
      beFailureWith(FailureReason.MISSING_DATA)
  }

  test("the currency family is closed: a code outside the published set is reported, not minted") {
    // The family is the closed set of the 74 published codes: a code outside it is reported as a
    // failure rather than resolved onto a currency no reference data describes.
    Currency.values.length shouldBe 74

    Currency.valueOf("XYZ") shouldBe None
    Currency.of("XYZ") should beFailureWith(FailureReason.PARSING)
    Currency.parse("XYZ") should beFailureWith(FailureReason.PARSING)
    Currency.parse("xyz") should beFailureWith(FailureReason.PARSING)

    // Closure holds for every shape of unknown code, and repeating a call yields the same failure.
    Currency.parse("QQQ") should beFailureWith(FailureReason.PARSING)
    Currency.parse("ZZZ") should beFailureWith(FailureReason.PARSING)
    Currency.parse("XYZ") shouldBe Currency.parse("XYZ")

    // A published code resolves however it is written; only the exact lookup is case sensitive.
    Currency.parse("gbp") should haveValue(Currency.GBP)
    Currency.parse("GbP") should haveValue(Currency.GBP)
    Currency.of("gbp") should beFailureWith(FailureReason.PARSING)
    Currency.valueOf("gbp") shouldBe None
  }

  test("the FX index family is closed: an unquoted currency pair is reported, not minted") {
    // The family is the closed set of the 16 published indices: a currency pair no administrator
    // publishes is reported as a failure rather than resolved onto an invented index.
    FxIndex.values.length shouldBe 16

    val unquoted = CurrencyPair.of(Currency.GBP, Currency.SEK)
    FxIndex.of(unquoted) should beFailureWith(FailureReason.PARSING)
    FxIndex.of("GBP/SEK") should beFailureWith(FailureReason.PARSING)
    FxIndex.values.toList.map(_.currencyPair) should not contain unquoted

    // The inverse of a quoted pair is not quoted: the data is keyed by the pair as it is quoted.
    FxIndex.of(CurrencyPair.of(Currency.USD, Currency.EUR)) should
      beFailureWith(FailureReason.PARSING)

    // The selection rule for a pair two administrators publish: the index whose name sorts first.
    FxIndex.of(CurrencyPair.of(Currency.EUR, Currency.USD)) should haveValue(FxIndices.EUR_USD_ECB)
    FxIndex.of("EUR/USD") should haveValue(FxIndices.EUR_USD_ECB)
    FxIndex.of("EUR/USD-ECB") should haveValue(FxIndices.EUR_USD_ECB)
  }

  //-------------------------------------------------------------------------
  // The four unions. Each searches its member families in its own order and answers with the
  // first value found, so what fixes it is its membership - which names it accepts and which it
  // refuses - asserted over the whole of every family.

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
    // published floating rate names is refused. Splitting those names by whether an index family
    // claims them ties this membership to its members' name spaces: the 54 names shared with the
    // Overnight and price families resolve, and the other 297 do not.
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

    // A price index measures a level and an FX index a rate of exchange; neither is a rate index.
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
    // The widest union, and the only one whose member families share names, which is what makes
    // its order observable rather than merely declared.
    (IborIndex.values.toList ++ OvernightIndex.values.toList ++ PriceIndex.values.toList).foreach {
      index =>
        withClue(s"${index.name}: ") {
          FloatingRate.tryParse(index.name) shouldBe Some(index)
          FloatingRate.parse(index.name) should haveValue(index)
        }
    }

    // The published names no index family holds resolve to the family identifier itself, which is
    // the fourth probe and what makes this union the wider one.
    FloatingRateName.values.toList.filterNot(value => allIndexNames.contains(value.name)).foreach {
      value =>
        withClue(s"${value.name}: ") {
          FloatingRate.tryParse(value.name) shouldBe Some(value)
          FloatingRate.parse(value.name) should haveValue(value)
        }
    }

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

    // The order with named constants, one per probe, the last naming no index at all.
    FloatingRate.parse("GBP-LIBOR-3M") should haveValue(IborIndices.GBP_LIBOR_3M)
    FloatingRate.parse("EUR-ESTR") should haveValue(OvernightIndices.EUR_ESTR)
    FloatingRate.parse("GB-RPI") should haveValue(PriceIndices.GB_RPI)
    FloatingRate.parse("GBP-LIBOR") should haveValue(FloatingRateNames.GBP_LIBOR)
    FloatingRateIndex.valueOf("GBP-LIBOR") shouldBe None
  }

  test("the four index families claim pairwise disjoint name spaces") {
    // Nothing in the types stops two index families from publishing one name, and were they to,
    // the earlier probe of every union above would silently shadow the later - leaving a member
    // no text resolves to. The name space compared is the whole of what each family answers to.
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

    val combined = indexFamilies.foldLeft(Set.empty[String])((acc, family) => acc ++ family.nameSpace)
    combined.size shouldBe indexFamilies.map(_.nameSpace.size).sum
  }

  test("the published floating rate names overlap the rate families, and the index wins") {
    // The deliberate exception to the guard above: the published name table republishes every
    // Overnight index name - and every alternate spelling of one - and every price index name,
    // because an Overnight or price rate is named by its index while an Ibor rate is named by its
    // family and tenor separately. Each such name resolves to two values.
    val shared = FloatingRateName.values.toList.filter { value =>
      FloatingRateIndex.valueOf(value.name).isDefined
    }
    shared.size shouldBe 54

    shared.foreach { value =>
      withClue(s"${value.name}: ") {
        val index = FloatingRateIndex.valueOf(value.name).getOrElse(
          fail(s"${value.name} no longer names an index"))

        // The union answers with the index; the name family, asked directly, with its own member.
        FloatingRate.tryParse(value.name) shouldBe Some(index)
        FloatingRate.parse(value.name) should haveValue(index)
        FloatingRateName.valueOf(value.name) shouldBe Some(value)
        FloatingRate.tryParse(value.name) should not be FloatingRateName.valueOf(value.name)
      }
    }

    // The overlap accounted for family by family: the Overnight family's 35 canonical names with
    // the ten alternate spellings it declares, and the price family's 9 names. No Ibor or FX index
    // name is a published floating rate name, so the narrower unions are unaffected.
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
    // constants. They are written out because nothing here reflects over an object's members, so
    // they cannot be enumerated, and because one that stopped being a member would still compile.
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
    // The one family where a member's folded key is another member's canonical name, which is the
    // case the registration order is built for: a canonical name is registered unconditionally
    // and a folded name only where the key is free, so both members keep their published name.
    foldedNamePairs.foreach {
      case (mixedCase, folded) =>
        withClue(s"$mixedCase / $folded: ") {
          folded shouldBe mixedCase.toUpperCase(Locale.ENGLISH)
          folded should not be mixedCase

          // Asserted through the shared name lookup, where the property has to hold: the family's
          // own entry points add index resolution, which the rest of the library does not share.
          FloatingRateName.namedEnum.valueOf(mixedCase).map(_.name) shouldBe Some(mixedCase)
          FloatingRateName.namedEnum.valueOf(folded).map(_.name) shouldBe Some(folded)
          FloatingRateName.namedEnum.parse(mixedCase).map(_.name) shouldBe Right(mixedCase)
          FloatingRateName.namedEnum.parse(folded).map(_.name) shouldBe Right(folded)

          FloatingRateName.valueOf(mixedCase).map(_.name) shouldBe Some(mixedCase)
          FloatingRateName.valueOf(folded).map(_.name) shouldBe Some(folded)
          FloatingRateName.parse(mixedCase).map(_.name) shouldBe Right(mixedCase)
          FloatingRateName.parse(folded).map(_.name) shouldBe Right(folded)

          FloatingRateName.namedEnum.byCanonicalName.get(mixedCase).map(_.name) shouldBe
            Some(mixedCase)
          FloatingRateName.namedEnum.byCanonicalName.get(folded).map(_.name) shouldBe Some(folded)

          // The one key the pair shares is the folded one, held by the member whose canonical name
          // it is - the upper-case row - rather than by whichever of the two was declared first.
          FloatingRateName.namedEnum.byUpperName.get(folded).map(_.name) shouldBe Some(folded)
          FloatingRateName.namedEnum.byUpperName.get(folded) should not be
            FloatingRateName.namedEnum.valueOf(mixedCase)
        }
    }

    // The whole of the difference between the family and its two views: 351 members, 351
    // canonical keys - one per member - and 349 folded keys, the two pairs each folding to one.
    FloatingRateName.values.length shouldBe 351
    FloatingRateName.namedEnum.byCanonicalName.size shouldBe 351
    (FloatingRateName.values.toList.map(_.name).toSet --
      FloatingRateName.namedEnum.byCanonicalName.keySet) shouldBe empty
    FloatingRateName.namedEnum.byUpperName.size shouldBe 349
    (FloatingRateName.values.toList.map(_.name.toUpperCase(Locale.ENGLISH)).toSet --
      FloatingRateName.namedEnum.byUpperName.keySet) shouldBe empty

    // And every member, not only the four rows of the two pairs, answers to its own name here.
    FloatingRateName.values.toList.foreach { value =>
      withClue(s"${value.name}: ") {
        FloatingRateName.namedEnum.valueOf(value.name) shouldBe Some(value)
        FloatingRateName.namedEnum.parse(value.name) should haveValue(value)
      }
    }
  }
}

/**
 * The closed families under assertion and the tables that drive them.
 *
 * Held beside the specification so that the inventory of families reads as one list: adding a
 * family here is what puts it under every property above.
 */
private[basics] object NamedEnumClosedSpec extends TableDrivenPropertyChecks {

  /**
   * The characters that make the left-hand side of a lenient row a pattern rather than a spelling.
   *
   * A row holding none of these, whose replacement refers to no captured group, is a plain claim
   * that one spelling names one member and can be driven from the production table; every other
   * row needs a probe, there being no single spelling to feed back in.
   */
  private val PatternMetacharacters: Set[Char] = "\\^$.|?*+()[]{}".toSet

  /**
   * The inline flag the shared lookup prefixes to a lenient source to make it ignore case.
   *
   * Written out here because the reference implementation of the chain has to compile each row
   * exactly as the production rule compiles it, prefix included, or the two would disagree over
   * every row a table spells in mixed case.
   */
  private val CaseInsensitiveFlag: String = "(?i)"

  /**
   * A closed named family under assertion, with the type of its members erased to [[Named]].
   *
   * The families differ in their member type and nothing else that matters here, so each is
   * widened to this one shape and swept by the same properties; the widening is ordinary
   * subtyping, so nothing is cast.
   *
   * The `valueOf` and `parse` held here are the family's own rather than the lookup's, because two
   * families wrap that lookup: the day counts add the `Bus/252` conventions and the published
   * floating rate names add the resolution of a concrete index name, so the companion's entry
   * points are what a caller reaches. A property belonging to the shared lookup instead is
   * asserted through the lookup itself.
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
   * @param lenientRows  the lenient rewrites as text, in the order they are applied - the raw
   *   view, which is what a table comparison needs and which compiles no expression
   * @param lenientLengthCeiling  the greatest length of text the family's lenient stage is
   *   applied to, derived by the lookup from the family's own data
   * @param lookupValueOf  the shared lookup's own exact lookup, which is narrower than the
   *   family's for the two families that wrap it
   * @param lookupParse  the shared lookup's own lenient lookup, likewise
   * @param lookupRewriteLeniently  the shared lookup's chain of rewrites, run on its own
   * @param ownParseIsLenient  whether the family's own `parse` is the lenient parse of the shared
   *   lookup, which is false for the one family whose `parse` is an exact lookup widened with
   *   index names rather than a lenient one
   * @param externalNameGroups  the names of the groups of protocol spellings published
   * @param externalNamesRaw  a group of protocol spellings as the family declared it
   * @param externalNames  a group of protocol spellings resolved onto values
   * @param expectedMembers  the number of members the published reference data fixes
   * @param foldedNameCollisions  the canonical names that are also another member's folded name
   * @param expectedAlternateRows  the number of alternate spellings declared, zero where they come
   *   from constant identifiers
   * @param expectedAlternateEntries  the table size after expansion with folded spellings
   * @param enumNameSpellings  whether the spellings come from the constant identifiers
   * @param expectedExternals  the number of rows of each group declared
   * @param expectedExternalsBeyondValues  the spellings of each group that resolve beyond `values`
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
      val lenientRows: List[(String, String)],
      val lenientLengthCeiling: Int,
      val lookupValueOf: String => Option[Named],
      val lookupParse: String => ResultNec[Named],
      val lookupRewriteLeniently: String => String,
      val ownParseIsLenient: Boolean,
      val externalNameGroups: Set[String],
      val externalNamesRaw: String => Option[Map[String, String]],
      val externalNames: String => Option[Map[String, Named]],
      val expectedMembers: Int,
      val foldedNameCollisions: Set[String],
      val expectedAlternateRows: Int,
      val expectedAlternateEntries: Int,
      val enumNameSpellings: Boolean,
      val expectedExternals: Map[String, Int],
      val expectedExternalsBeyondValues: Map[String, Set[String]],
      val expectedLiteralLenient: Int,
      val expectedPatternLenient: Int) {

    /**
     * Every spelling this family answers to through its exact lookup: the two keys each member is
     * registered under, together with the alternate spellings the family declares. This is what
     * two families must not share if the union that searches them is to reach both.
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
      lenientRows.filter { case (source, replacement) => isLiteralRow(source, replacement) }

    /**
     * The lenient rows that are patterns.
     *
     * @return the expression and replacement of each pattern-shaped row
     */
    def patternLenientRows: List[(String, String)] =
      lenientRows.filterNot { case (source, replacement) => isLiteralRow(source, replacement) }

    /**
     * The sources of the pattern-shaped lenient rows, as the production table spells them.
     *
     * @return the expression source of each pattern-shaped row
     */
    def patternSources: List[String] =
      patternLenientRows.map { case (source, _) => source }

    override def toString: String = label

    private def isLiteralRow(source: String, replacement: String): Boolean =
      !source.exists(PatternMetacharacters.contains) && !replacement.contains('$')
  }

  /**
   * The lenient rows of a family as a naive reference implementation of the chain.
   *
   * Every row compiled exactly as the production rule compiles it - the flag that makes an
   * expression insensitive to case prefixed unless the row carries it already - and nothing
   * else: this is the table as the ported algorithm read it, with no screening of any kind.
   *
   * @param rows  the lenient rewrites of a family, in the order they are applied
   * @return each row as a compiled expression and its replacement
   */
  private def naiveLenientRules(rows: List[(String, String)]): List[(Regex, String)] =
    rows.map {
      case (source, replacement) =>
        ((if (source.startsWith(CaseInsensitiveFlag)) source else CaseInsensitiveFlag + source).r, replacement)
    }

  /**
   * Applies every rule of a naive reference to text, in order, screening nothing.
   *
   * The ported algorithm exactly: each expression is matched against the whole of the current
   * text and, where it matches, replaces it, the expression after it seeing the replacement.
   * The length bound is the one the production chain applies, and it is applied here for the
   * same reason - text beyond it is answered without an expression being run over it - so this
   * function is a reference for `rewriteLeniently` on text of any length.
   *
   * @param rules  the reference rules of a family, in declaration order
   * @param ceiling  the greatest length of text the family's lenient stage is applied to
   * @param text  the text to rewrite
   * @return the text that survives every rule
   */
  private def naiveRewrite(rules: List[(Regex, String)], ceiling: Int, text: String): String =
    if (text.length > ceiling) {
      text
    } else {
      rules.foldLeft(text) {
        case (current, (expression, replacement)) =>
          val matcher = expression.pattern.matcher(current)
          if (matcher.matches()) matcher.replaceFirst(replacement) else current
      }
    }

  /**
   * Every text the equivalence sweep offers a family, built from that family's own data.
   *
   * The spellings a caller can plausibly offer - each member's name and its folded, lowered and
   * screaming-snake forms, both sides of the alternate-name table, both sides of every group of
   * protocol spellings, and both sides of every lenient row - together with the junk below, so
   * that the sweep covers the text the family resolves and the text it must not. Duplicates are
   * removed, the same spelling commonly arriving from two tables.
   *
   * @param family  the family to build the corpus for
   * @return every text the sweep offers that family
   */
  private def lenientCorpus(family: Family): List[String] = {
    val memberSpellings = family.members.flatMap { member =>
      List(
        member.name,
        member.name.toUpperCase(Locale.ENGLISH),
        member.name.toLowerCase(Locale.ENGLISH),
        screamingSnakeOf(member.name))
    }
    val alternateSides = family.alternateNames.toList.flatMap {
      case (spelling, canonicalName) => List(spelling, canonicalName)
    }
    val externalSides = family.externalNameGroups.toList.flatMap { group =>
      family.externalNamesRaw(group).toList.flatMap(_.toList).flatMap {
        case (spelling, canonicalName) => List(spelling, canonicalName)
      }
    }
    val lenientSides = family.lenientRows.flatMap {
      case (source, replacement) => List(source, replacement)
    }
    (memberSpellings ::: alternateSides ::: externalSides ::: lenientSides ::: corpusJunk).distinct
  }

  /**
   * The text of the equivalence sweep that no family declares.
   *
   * The shapes that reach the rewrites differently from a spelling: nothing at all, whitespace,
   * one character, text carrying a character outside the ASCII range or one whose case folds
   * onto such a character, and text spelling a piece of a regular expression - a bracket left
   * open, a quoted run, an alternation, an inline flag, a wildcard - which is what a family
   * would be handed by anything trying to reach its expressions rather than its names. The last
   * few are spellings the transcribed tables rewrite, held here so that every family is offered
   * text that some family resolves.
   */
  private val corpusJunk: List[String] =
    List(
      "",
      " ",
      "  ",
      "A",
      "a",
      "X",
      "1",
      "Zz9-Unknown-Family-Member",
      "\u00dcn\u00efcod\u00e9",
      "\u00c9\u00c9",
      "\u00df",
      "\u212a",
      "k",
      "K",
      "(",
      "[",
      "([",
      "\\Q",
      "a|b",
      "(?i)A",
      ".*",
      "ACT_360",
      "actual/actual",
      "ACT/ACT.ISMA",
      "A/A ISMA",
      "Act/Act (ISDA)",
      "MOD_FOLLOW",
      "Day_31",
      "LAST_DAY",
      "BUS/252",
      "bus/252 brbd")

  /**
   * The base class at the head of a family, found above one of its members.
   *
   * Every member of a closed family is an instance of a class the family's companion declares -
   * the singleton class of a `case object`, or the hidden class the family builds from its data
   * table - so the family's own base class is one of its ancestors, and it is the one whose simple
   * name is the family's label. Answering with an option rather than raising keeps this usable
   * from the companion, where the failure of a specification cannot be reported.
   *
   * @param label  the name of the family
   * @param member  one of its published members
   * @return the base class of the family, when it is found above the member
   */
  private def familyRoot(label: String, member: Named): Option[Class[_]] = {
    val ancestors: Iterator[Class[_]] =
      Iterator.unfold[Class[_], Option[Class[_]]](Option(member.asInstanceOf[AnyRef].getClass))(
        current => current.map(candidate => (candidate, Option[Class[_]](candidate.getSuperclass))))
    ancestors.find(candidate => candidate.getSimpleName == label)
  }

  /**
   * Whether a class is declared inside a family, which on the JVM is what "a member of the family"
   * means.
   *
   * The declaring class of a member class is the class it is nested in, and the members of these
   * families are nested in the companion of the family itself. A class compiled elsewhere is
   * nested in something else or in nothing at all, and is a member of no family.
   *
   * @param candidate  the class of a value
   * @param family  the base class at the head of the family
   * @return true when the class is declared inside the family
   */
  private def declaredIn(candidate: Class[_], family: Class[_]): Boolean =
    Option(candidate.getDeclaringClass).exists(declaring => declaring == family)

  /**
   * Declares a closed family, widening its member type and taking its tables from its lookup.
   *
   * @param label  the name of the family
   * @param expectedMembers  the number of members the published reference data fixes
   * @param valueOf  the family's exact lookup by name
   * @param parse  the family's lenient lookup by name
   * @param foldedNameCollisions  the canonical names that are also another member's folded name
   * @param expectedAlternateRows  the number of alternate spellings declared
   * @param expectedAlternateEntries  the table size after expansion with folded spellings
   * @param enumNameSpellings  whether the spellings come from the constant identifiers
   * @param expectedExternals  the number of rows of each published group
   * @param expectedExternalsBeyondValues  the spellings of each group that resolve beyond `values`
   * @param expectedLiteralLenient  the number of lenient rows that are plain spellings
   * @param expectedPatternLenient  the number of lenient rows that are patterns
   * @param ownParseIsLenient  whether the family's own `parse` is the lenient parse of the lookup
   * @param lookup  the name lookup the family's companion publishes
   * @tparam A  the member type of the family
   * @return the family, ready to be swept
   */
  private def family[A <: Named](
      label: String,
      expectedMembers: Int,
      valueOf: String => Option[A],
      parse: String => ResultNec[A],
      foldedNameCollisions: Set[String] = Set.empty,
      expectedAlternateRows: Int = 0,
      expectedAlternateEntries: Int = 0,
      enumNameSpellings: Boolean = false,
      expectedExternals: Map[String, Int] = Map.empty,
      expectedExternalsBeyondValues: Map[String, Set[String]] = Map.empty,
      expectedLiteralLenient: Int = 0,
      expectedPatternLenient: Int = 0,
      ownParseIsLenient: Boolean = true)(implicit lookup: NamedEnum[A]): Family =

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
      lenientRows = lookup.lenientSources,
      lenientLengthCeiling = lookup.lenientLengthCeiling,
      lookupValueOf = name => lookup.valueOf(name),
      lookupParse = name => lookup.parse(name),
      lookupRewriteLeniently = name => lookup.rewriteLeniently(name),
      ownParseIsLenient = ownParseIsLenient,
      externalNameGroups = lookup.externalNameGroups,
      externalNamesRaw = group => lookup.externalNamesRaw(group),
      externalNames = group => lookup.externalNames(group),
      expectedMembers = expectedMembers,
      foldedNameCollisions = foldedNameCollisions,
      expectedAlternateRows = expectedAlternateRows,
      expectedAlternateEntries = expectedAlternateEntries,
      enumNameSpellings = enumNameSpellings,
      expectedExternals = expectedExternals,
      expectedExternalsBeyondValues = expectedExternalsBeyondValues,
      expectedLiteralLenient = expectedLiteralLenient,
      expectedPatternLenient = expectedPatternLenient)

  /** Lifts a single-cause result into the accumulating shape the sweep compares. */
  private def toNec[A](result: FailureOr[A]): ResultNec[A] =
    result.fold(failure => Left(NonEmptyChain.one(failure)), value => Right(value))

  /**
   * Derives the constant identifier of a member from the name it renders.
   *
   * The three constant families name their members in camel case and their constants in screaming
   * snake case, converting between the two by inserting a separator before each capital after the
   * first and folding to upper case. Deriving that here keeps the spellings asserted a function of
   * the members rather than a copy of the table asserted.
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
   * Every closed named family of this module and the failure reasons of `strata-collect`, each
   * with the cardinality its reference data fixes.
   *
   * The cardinalities are literals so that a row dropped from a data table fails rather than
   * quietly shrinking a family; the failure reasons' ten is the number of that type's constants.
   * That family is declared exactly as the families of this module are - members in a companion,
   * a `NamedEnum` built from `values`, an exact `valueOf` and a lenient `parse`.
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
      expectedExternalsBeyondValues = Map("FpML" -> Set("BUS/252")),
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
      foldedNameCollisions = Set("DKK-DESTR-OIS COMPOUND", "SEK-SWESTR-OIS COMPOUND"),
      ownParseIsLenient = false),
    family[FailureReason](
      label = "FailureReason",
      expectedMembers = 10,
      valueOf = name => FailureReason.valueOf(name),
      parse = name => FailureReason.parse(name))
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
   * Four shapes rather than one, because they reach the rejection differently: ordinary text, the
   * empty and blank strings, and text carrying punctuation a lenient pattern might have matched.
   */
  val unknownNames: List[String] =
    List("Rubbish", "", "  ", "Zz9-Unknown-Family-Member")

  /**
   * Every alternate spelling the published tables declare, with the name it renames.
   *
   * Twelve rows across three families: the won certificate of deposit rate, named by its tenor in
   * weeks and in months; the ten retired and market spellings of the Overnight rates; and the
   * previous administrator of the dollar/rupee rate. Each is behaviour, a trade written under an
   * old name having to keep resolving.
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

  /** The alternate spellings declared for one family, as the table declares them. */
  def declaredAlternates(label: String): Set[String] =
    alternateSpellings.collect { case (family, spelling, _) if family == label => spelling }.toSet

  /**
   * One probe for each pattern-shaped lenient row, with the member the probe must reach.
   *
   * Twenty-two rows: nine day count patterns, ten roll convention patterns and three business day
   * convention patterns. A probe is a spelling the row it names has to fire for - remove the row
   * and the probe stops resolving - which is the most a probe can claim where rows chain. The
   * patterns probed are held equal to the ones the tables declare, so none arrives unprobed.
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
   * own. They are the only reason any family of this library holds two members that fold to one
   * upper-case key, and so the only reason a folded view is smaller than its member list.
   *
   * The first element of each pair is the mixed-case spelling and the second its folded form,
   * which is also the other member's published name - so the second elements are exactly the
   * `foldedNameCollisions` the family declares.
   */
  val foldedNamePairs: List[(String, String)] =
    List(
      ("DKK-DESTR-OIS Compound", "DKK-DESTR-OIS COMPOUND"),
      ("SEK-SWESTR-OIS Compound", "SEK-SWESTR-OIS COMPOUND"))

  /**
   * The 41 named constants the published floating rate name family exposes.
   *
   * Written out because nothing here reflects over an object's members, so the constants cannot
   * be enumerated, and because a constant that ceased to be a member would still compile.
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
