/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import java.util.Locale

import scala.util.matching.Regex

import cats.Eq
import cats.Hash
import cats.Order
import cats.Show
import cats.data.NonEmptyChain
import cats.data.NonEmptyList

import org.scalatest.concurrent.TimeLimits
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.time.Seconds
import org.scalatest.time.Span

import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Tests the name lookup of a closed family of named values.
 *
 * This one spec carries the whole of what four test classes of the port's source covered:
 * the registry that resolved a name at run time, the formatter that resolved the name of a
 * plain enumeration, the name-keyed factory carried by the named interface itself, and the
 * registry that resolved a name across several families at once. All four collapse onto the
 * single typeclass under test here, so all four sets of cases are expressed against it.
 *
 * ===The fixtures are code, not configuration===
 *
 * The families the original tests resolved were assembled at run time from configuration
 * files found on the class path, one per family, declaring the classes that provided the
 * members together with three tables over them. This port has no such mechanism: a family
 * hands its members and its tables to the typeclass in its own companion, and what it can
 * resolve is fixed when it is compiled. Every fixture of this spec is therefore a sealed
 * family declared in [[NamedEnumFixtures]] below, and the tables are transcribed from the
 * configuration of the original fixture exactly - the same five members, the same single
 * alternate spelling, the same two groups of external spellings, and the same three lenient
 * rewrites in the same order. Nothing here reads a resource, a class or a class path, which
 * is the property that makes each family closed and is asserted directly further down.
 *
 * ===What changed, and is asserted as changed===
 *
 * Three differences from the original are deliberate, and each is pinned by a test rather
 * than left to be discovered:
 *
 *  - '''text that names no member is reported, not raised'''. Every operation of the
 *    original that could fail threw: the strict lookup, the lookup of an unknown group of
 *    external spellings, the reverse lookup of a member absent from a group, and the parse
 *    of an unknown name. Here they answer `None` or a `Failure.Parsing` on the left of an
 *    `EitherNec`, and nothing in this spec expects an exception.
 *  - '''a family cannot be empty, and cannot be misconfigured'''. The original had a test
 *    for a family whose configuration declared no provider, which resolved nothing, and one
 *    for seven separately misconfigured families, each of which logged a warning and
 *    resolved nothing. Neither state is reachable now - the members arrive as a
 *    `NonEmptyList` of a type bounded by [[Named]] - so both cases are re-expressed as the
 *    smallest family that can exist and as compile-time proofs that the unreachable states
 *    do not compile.
 *  - '''there is no combined family and no reflective factory'''. The union of several
 *    families is a function that tries each of their exact lookups in a fixed order, and
 *    the name-keyed factory that searched a type for a factory method at run time is gone.
 *    Both are covered by the cases that stood behind them.
 *
 * One difference is worth stating at more length, because it is a case the original fixture
 * had and this one cannot. In the original, a member could be registered under one lookup
 * key rather than two: a provider declared as `constants` or as `instance` registered each
 * member under its canonical name and under that name folded to upper case, while a
 * provider declared as a lookup function registered whatever its own map held, and the
 * fixture's lookup function held only the exact name `Other`. `OTHER` therefore resolved to
 * nothing while `Other` resolved to a member. Provider kinds are exactly what this port
 * removes, and the algorithm it keeps registers every member under both of its keys: its
 * canonical name, claimed unconditionally, and that name folded to upper case, added only
 * where the name space still has room for it. The asymmetry is consequently unreachable -
 * a key can only be missing from a view by having been claimed by another member, which
 * resolves it to that member rather than to nothing - so this spec asserts the uniform
 * registration that replaced it, and covers the two ways a key view can still be narrower
 * than the member list: a member whose canonical name is already upper case offers one key
 * rather than two, and two members whose names differ in case alone share one folded key,
 * which belongs to the member whose canonical name it is. Both are asserted below, the
 * first against a real family of this module. The canonical view is never narrower, every
 * member holding the name it publishes.
 *
 * ===Traceability===
 *
 * Each test names the method of the original test class whose cases it carries, so that the
 * mapping from the original suite to this one can be read off the test names. The original
 * packed many assertions into few methods; this spec splits them so that a regression names
 * the behaviour it broke.
 */
final class NamedEnumSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks with TimeLimits {

  import NamedEnumFixtures._

  /** The lookup of the ported sample family, summoned as a caller summons it. */
  private val sample: NamedEnum[SampleNamed] = NamedEnum[SampleNamed]

  /** The lookup of the family standing in for the ported plain enumeration. */
  private val mock: NamedEnum[MockEnum] = NamedEnum[MockEnum]

  //-------------------------------------------------------------------------
  // the members of a family, and the views keyed by name
  //-------------------------------------------------------------------------

  test("values holds the members of the family in declaration order (ExtendedEnumTest.test_enum_SampleNamed)") {
    sample.values shouldBe NonEmptyList.of(
      SampleNamed.STANDARD,
      SampleNamed.MORE,
      SampleNamed.OTHER,
      SampleNamed.ANOTHER1,
      SampleNamed.ANOTHER2)
  }

  test("values is non-empty by its type, so a family always has a first member (ExtendedEnumTest.test_enum_SampleNamed)") {
    sample.values.head shouldBe SampleNamed.STANDARD
    sample.values.toList.size shouldBe 5
    sample.values.toList.map(_.name) shouldBe List("Standard", "More", "Other", "Another1", "Another2")
  }

  test("byCanonicalName keys every member by the name it renders (ExtendedEnumTest.test_enum_SampleNamed)") {
    sample.byCanonicalName shouldBe Map(
      "Standard" -> SampleNamed.STANDARD,
      "More" -> SampleNamed.MORE,
      "Other" -> SampleNamed.OTHER,
      "Another1" -> SampleNamed.ANOTHER1,
      "Another2" -> SampleNamed.ANOTHER2)
  }

  test("byUpperName keys every member by its upper-case name (ExtendedEnumTest.test_enum_SampleNamed)") {
    sample.byUpperName shouldBe Map(
      "STANDARD" -> SampleNamed.STANDARD,
      "MORE" -> SampleNamed.MORE,
      "OTHER" -> SampleNamed.OTHER,
      "ANOTHER1" -> SampleNamed.ANOTHER1,
      "ANOTHER2" -> SampleNamed.ANOTHER2)
  }

  test("every member is registered under both of its keys (ExtendedEnumTest.test_enum_SampleNamed)") {
    // This is the algorithm the constants provider of the original performed, and the one
    // the port keeps for every member of every family: the canonical name, and that name
    // folded to upper case in the English locale. The fixture of the original additionally
    // had a member registered under its exact name alone, because that member came from a
    // lookup-function provider; provider kinds are gone, so the two views below are the
    // member list keyed two ways, with nothing missing from either.
    val canonicalKeys = sample.values.toList.map(_.name).toSet
    val upperKeys = sample.values.toList.map(_.name.toUpperCase(Locale.ENGLISH)).toSet
    sample.byCanonicalName.keySet shouldBe canonicalKeys
    sample.byUpperName.keySet shouldBe upperKeys
    sample.byCanonicalName.values.toSet shouldBe sample.values.toList.toSet
    sample.byUpperName.values.toSet shouldBe sample.values.toList.toSet
  }

  test("alternateNames is the supplied table expanded with upper-case spellings (ExtendedEnumTest.test_enum_SampleNamed)") {
    sample.alternateNames shouldBe Map("Alternate" -> "Standard", "ALTERNATE" -> "Standard")
  }

  test("expanding the alternate names adds no key for a spelling already upper case (ExtendedEnumTest.test_enum_SampleNamed)") {
    // The expansion fills gaps and never displaces a supplied spelling, so a table whose
    // spelling is its own upper-case form comes back exactly as it was supplied.
    aliasBeatsRewrite.alternateNames shouldBe Map("A1" -> "Another1")
  }

  test("expanding the alternate names leaves a supplied upper-case spelling as it was supplied (ExtendedEnumTest.test_enum_SampleNamed)") {
    // The branch where the expansion has something to displace and must not: this table
    // supplies both a spelling and the upper-case form of that spelling, and the two name
    // different members. Were the expansion to overwrite rather than fill, the folded form of
    // the first row would take the second row's key and the family would resolve `ALIAS` to
    // the wrong member. The table therefore comes back with exactly the two rows supplied.
    aliasCaseConflict.alternateNames shouldBe Map("alias" -> "Standard", "ALIAS" -> "More")
    aliasCaseConflict.alternateNames.size shouldBe 2
  }

  test("a supplied upper-case spelling is authoritative in both lookups (ExtendedEnumTest.test_enum_SampleNamed)") {
    // The same branch read through the operations rather than through the table. Each row is
    // reached by the spelling it was supplied under, and the fold the lenient stage performs
    // reaches the upper-case row rather than the row it is the folded form of - which is the
    // one thing an expansion that overwrote a supplied spelling would silently change.
    aliasCaseConflict.valueOf("alias") shouldBe Some(SampleNamed.STANDARD)
    aliasCaseConflict.valueOf("ALIAS") shouldBe Some(SampleNamed.MORE)
    aliasCaseConflict.valueOf("Alias") shouldBe None
    aliasCaseConflict.parse("alias") should haveValue(SampleNamed.STANDARD)
    aliasCaseConflict.parse("ALIAS") should haveValue(SampleNamed.MORE)
    aliasCaseConflict.parse("Alias") should haveValue(SampleNamed.MORE)
  }

  //-------------------------------------------------------------------------
  // the exact lookup
  //-------------------------------------------------------------------------

  test("valueOf resolves a canonical name (ExtendedEnumTest.test_enum_SampleNamed)") {
    sample.valueOf("Standard") shouldBe Some(SampleNamed.STANDARD)
    sample.valueOf("More") shouldBe Some(SampleNamed.MORE)
    sample.valueOf("Other") shouldBe Some(SampleNamed.OTHER)
    sample.valueOf("Another1") shouldBe Some(SampleNamed.ANOTHER1)
    sample.valueOf("Another2") shouldBe Some(SampleNamed.ANOTHER2)
  }

  test("valueOf resolves the upper-case form of a canonical name (ExtendedEnumTest.test_enum_SampleNamed)") {
    sample.valueOf("STANDARD") shouldBe Some(SampleNamed.STANDARD)
    sample.valueOf("MORE") shouldBe Some(SampleNamed.MORE)
    sample.valueOf("OTHER") shouldBe Some(SampleNamed.OTHER)
    sample.valueOf("ANOTHER1") shouldBe Some(SampleNamed.ANOTHER1)
    sample.valueOf("ANOTHER2") shouldBe Some(SampleNamed.ANOTHER2)
  }

  test("valueOf answers None for text that names no member (ExtendedEnumTest.test_enum_SampleNamed)") {
    sample.valueOf("Rubbish") shouldBe None
    sample.valueOf("") shouldBe None
  }

  test("valueOf resolves an alternate name and its upper-case form (ExtendedEnumTest.test_enum_SampleNamed)") {
    sample.valueOf("Alternate") shouldBe Some(SampleNamed.STANDARD)
    sample.valueOf("ALTERNATE") shouldBe Some(SampleNamed.STANDARD)
  }

  test("valueOf is case sensitive beyond the two keys a member is registered under (ExtendedEnumTest.test_enum_SampleNamed)") {
    sample.valueOf("standard") shouldBe None
    sample.valueOf("sTaNdArD") shouldBe None
    sample.valueOf("alternate") shouldBe None
  }

  test("an alternate name does not chain into a second alternate name (ExtendedEnumTest.test_enum_SampleNamed)") {
    // The substitution happens once, so a table whose target is itself an alternate
    // spelling resolves to nothing rather than following the second hop.
    chainedAlias.valueOf("First") shouldBe None
    chainedAlias.valueOf("Second") shouldBe Some(SampleNamed.STANDARD)
  }

  test("familyName and toString name the family (ExtendedEnumTest.test_enum_SampleNamed)") {
    sample.familyName shouldBe "SampleNamed"
    sample.toString shouldBe "NamedEnum[SampleNamed]"
  }

  test("a family that supplies no label is labelled generically (ExtendedEnumTest.test_enum_SampleNamed)") {
    unlabelled.familyName shouldBe "NamedEnum"
    unlabelled.toString shouldBe "NamedEnum[NamedEnum]"
  }

  //-------------------------------------------------------------------------
  // reporting text that names no member
  //-------------------------------------------------------------------------

  test("parse reports text that names no member instead of raising (ExtendedEnumTest.test_enum_SampleNamed)") {
    val parsed: ResultNec[SampleNamed] = sample.parse("Rubbish")
    parsed should beFailure
    parsed should beFailureWith(FailureReason.PARSING)
    parsed should haveFailureMessageMatching("SampleNamed name not found: Rubbish")
  }

  test("the failure of parse is a parsing failure naming the family and the text (ExtendedEnumTest.test_enum_SampleNamed)") {
    val failures: List[Failure] = failuresOf(sample.parse("Rubbish"))
    failures shouldBe List(Failure.Parsing("SampleNamed name not found: Rubbish"))
    failures.head.reason shouldBe FailureReason.PARSING
  }

  test("the failure of parse carries no attributes, so two failures over the same text are equal (ExtendedEnumTest.test_enum_SampleNamed)") {
    val failures: List[Failure] = failuresOf(sample.parse("Rubbish"))
    failures.head.attributes shouldBe empty
    sample.parse("Rubbish") shouldBe sample.parse("Rubbish")
  }

  test("parse reports the text as it was supplied, not as it was rewritten (ExtendedEnumTest.test_enum_SampleNamed)") {
    failuresOf(sample.parse("rubbish")).head.message shouldBe "SampleNamed name not found: rubbish"
  }

  //-------------------------------------------------------------------------
  // the groups of external spellings
  //-------------------------------------------------------------------------

  test("externalNameGroups names the groups the family publishes (ExtendedEnumTest.test_enum_SampleNamed_externals)") {
    sample.externalNameGroups shouldBe Set("Foo", "Bar")
  }

  test("externalNames answers None for a group the family does not publish (ExtendedEnumTest.test_enum_SampleNamed_externals)") {
    // The original raised here; the port reports the absence in the result type.
    sample.externalNames("Rubbish") shouldBe None
    sample.externalNamesRaw("Rubbish") shouldBe None
  }

  test("externalNames resolves the first group to its member (ExtendedEnumTest.test_enum_SampleNamed_externals)") {
    sample.externalNames("Foo") shouldBe Some(Map("Foo1" -> SampleNamed.STANDARD))
    sample.externalNamesRaw("Foo") shouldBe Some(Map("Foo1" -> "Standard"))
  }

  test("externalNames resolves the second group, where one spelling names another member (ExtendedEnumTest.test_enum_SampleNamed_externals)") {
    sample.externalNames("Bar") shouldBe Some(Map("Foo1" -> SampleNamed.MORE, "Foo2" -> SampleNamed.STANDARD))
    sample.externalNamesRaw("Bar") shouldBe Some(Map("Foo1" -> "More", "Foo2" -> "Standard"))
  }

  test("a group of external spellings supports the reverse lookup of the original (ExtendedEnumTest.test_enum_SampleNamed_externals)") {
    // The original exposed the reverse direction as an operation that raised for a member
    // the group did not name. The raw table is the whole of what that operation needed, and
    // inverting it leaves the absence of such a member observable as a missing key.
    val reversed: Map[String, String] =
      sample.externalNamesRaw("Bar").getOrElse(Map.empty).map { case (external, canonical) => canonical -> external }
    reversed.get(SampleNamed.MORE.name) shouldBe Some("Foo1")
    reversed.get(SampleNamed.STANDARD.name) shouldBe Some("Foo2")
    reversed.get(SampleNamed.OTHER.name) shouldBe None
  }

  test("a group of external spellings takes part in no lookup (ExtendedEnumTest.test_enum_SampleNamed_externals)") {
    sample.valueOf("Foo1") shouldBe None
    sample.parse("Foo1") should beFailure
    sample.parse("Foo2") should beFailure
  }

  test("a row of a group that names no member is dropped from the resolved group (ExtendedEnumTest.test_enum_SampleNamed_externals)") {
    // The resolved group is therefore able to be smaller than the table it came from, and
    // comparing the two key sets is how a row naming a member the family does not have is
    // found.
    danglingExternal.externalNamesRaw("Foo").map(_.keySet) shouldBe Some(Set("Foo1", "Gone"))
    danglingExternal.externalNames("Foo") shouldBe Some(Map("Foo1" -> SampleNamed.STANDARD))
  }

  test("a row of a group may name a member through an alternate spelling (ExtendedEnumTest.test_enum_SampleNamed_externals)") {
    // Each row is resolved through the same alias-aware exact lookup as valueOf.
    aliasedExternal.externalNames("Foo") shouldBe Some(Map("Foo1" -> SampleNamed.STANDARD))
  }

  test("a row of a group may name a value only the family's own resolution reaches (ExtendedEnumTest.test_enum_SampleNamed_externals)") {
    // A family whose name space is wider than its closed members carries rows naming values
    // that `values` does not hold, and supplies the resolution that reaches them when it
    // builds its lookup. Without that resolution such a row is dropped exactly as a row
    // naming nothing is, which is the difference asserted here; with it the row resolves,
    // while the exact lookup of the family still refuses the name - the value being outside
    // the closed members is the whole reason the resolution has to be supplied.
    WiderNamed.withoutResolution.externalNames("Foo") shouldBe Some(Map("In" -> WiderNamed.INSIDE))
    WiderNamed.lookup.externalNames("Foo") shouldBe
      Some(Map("In" -> WiderNamed.INSIDE, "Out" -> WiderNamed.OUTSIDE))
    WiderNamed.lookup.externalNamesRaw("Foo") shouldBe
      Some(Map("In" -> "Inside", "Out" -> "Outside"))
    WiderNamed.lookup.values.toList shouldBe List(WiderNamed.INSIDE)
    WiderNamed.lookup.valueOf("Outside") shouldBe None
    WiderNamed.lookup.valueOf("Inside") shouldBe Some(WiderNamed.INSIDE)
  }

  //-------------------------------------------------------------------------
  // the lenient lookup, and the chain of rewrites behind it
  //-------------------------------------------------------------------------

  test("parse resolves a canonical name without rewriting it (ExtendedEnumTest.test_enum_lenient)") {
    sample.parse("Standard") should haveValue(SampleNamed.STANDARD)
    sample.parse("Another2") should haveValue(SampleNamed.ANOTHER2)
  }

  test("parse resolves A1 through the chain A1 -> B1 -> Standard (ExtendedEnumTest.test_enum_lenient)") {
    // The three rewrites of the fixture are, in the order the family declares them:
    //
    //   A([1-2]) -> B$1        B1 -> Standard        B2 -> More
    //
    // Each rewrite whose expression matches the whole of the current text replaces that
    // text, and the rewrite after it sees the replacement. `A1` therefore becomes `B1` at
    // the first rewrite and `Standard` at the second, and only then is the exact lookup
    // tried. A single pass over the table - a plain map from text to member, say - resolves
    // neither of these two inputs, which is what the next two tests make impossible to
    // miss. Nothing here may be rewritten into a one-shot lookup.
    sample.parse("A1") should haveValue(SampleNamed.STANDARD)
  }

  test("parse resolves A2 through the chain A2 -> B2 -> More (ExtendedEnumTest.test_enum_lenient)") {
    sample.parse("A2") should haveValue(SampleNamed.MORE)
  }

  test("the chain is sequential: neither the input nor its intermediate form resolves alone (ExtendedEnumTest.test_enum_lenient)") {
    // No member and no alternate spelling is named `A1`, `A2`, `B1` or `B2`, so the two
    // results above can only have been reached by applying a second rewrite to the output
    // of the first.
    sample.valueOf("A1") shouldBe None
    sample.valueOf("A2") shouldBe None
    sample.valueOf("B1") shouldBe None
    sample.valueOf("B2") shouldBe None
    sample.alternateNames.keySet shouldBe Set("Alternate", "ALTERNATE")
  }

  test("reordering the rewrites breaks the chain, so their order is load-bearing (ExtendedEnumTest.test_enum_lenient)") {
    // The same three rewrites, with the one that produces the intermediate form moved to
    // the end of the table. `A1` still becomes `B1`, but no rewrite runs after that, so the
    // lookup that follows the chain has nothing to resolve. This is the permanent control
    // on the two tests above: were the implementation to apply its table in one pass, or in
    // any order but the declared one, this family would resolve what it must not.
    brokenChain.lenientPatterns.map(sourceOf).toSet shouldBe sample.lenientPatterns.map(sourceOf).toSet
    brokenChain.lenientPatterns.map(sourceOf) should not be sample.lenientPatterns.map(sourceOf)
    brokenChain.parse("A1") should beFailure
    brokenChain.parse("A2") should beFailure
    brokenChain.parse("Standard") should haveValue(SampleNamed.STANDARD)
  }

  test("lenientPatterns exposes the rewrites in the order the family declared them (ExtendedEnumTest.test_enum_lenient)") {
    sample.lenientPatterns.map(sourceOf) shouldBe
      List("A([1-2])" -> "B$1", "B1" -> "Standard", "B2" -> "More")
  }

  test("an alternate name is honoured ahead of a rewrite that would also match (ExtendedEnumTest.test_enum_lenient)") {
    // The alias substitution and the exact lookup both run before the first rewrite is
    // reached, so a family whose alternate table and whose rewrite table both claim the
    // same text resolves it through the alternate table. Under the rewrites alone this
    // input resolves to a different member, which is what makes the precedence observable.
    aliasBeatsRewrite.valueOf("A1") shouldBe Some(SampleNamed.ANOTHER1)
    aliasBeatsRewrite.parse("A1") should haveValue(SampleNamed.ANOTHER1)
    sample.parse("A1") should haveValue(SampleNamed.STANDARD)
  }

  test("only the lenient stage folds case, so parse resolves text that valueOf rejects (ExtendedEnumTest.test_enum_lenient)") {
    sample.valueOf("standard") shouldBe None
    sample.parse("standard") should haveValue(SampleNamed.STANDARD)
    sample.valueOf("another1") shouldBe None
    sample.parse("another1") should haveValue(SampleNamed.ANOTHER1)
  }

  test("the lenient stage folds case before the rewrites, so a rewrite matches lower-case text (ExtendedEnumTest.test_enum_lenient)") {
    sample.parse("a1") should haveValue(SampleNamed.STANDARD)
    sample.parse("a2") should haveValue(SampleNamed.MORE)
  }

  test("the two rewrites that pin the lenient stage are spelled exactly as declared (ExtendedEnumTest.test_enum_lenient)") {
    // Every rewrite of the sample family is spelled in upper case, which is why neither
    // half of the lenient stage - the fold of the input, and the copy of each expression
    // made insensitive to case - is observable through that family: fold or not, copy or
    // not, an upper-case expression matches upper-case text. The two rewrites below are
    // spelled so that each half is observable on its own, so their spelling is the whole of
    // what makes the two tests after this one able to fail, and is asserted verbatim here.
    // Folding either source to upper case re-opens the hole and fails this test.
    caseFolding.lenientPatterns.map(sourceOf) shouldBe
      List("Mix/Mix" -> "Other", "(?-i)sensitive/sensitive" -> "More")
  }

  test("a rewrite spelled in mixed case applies only because its copy ignores case (ExtendedEnumTest.test_enum_lenient)") {
    // The source as the family supplied it cannot match the text a rewrite is handed, that
    // text having been folded to upper case; the copy the family applies can.
    MixedCaseSource.r.matches("MIX/MIX") shouldBe false
    ("(?i)" + MixedCaseSource).r.matches("MIX/MIX") shouldBe true
    // So this rule resolves at all only through that copy - and through it, it resolves
    // whatever case the input arrives in, which is the tolerance the mixed-case rows of a
    // real table depend on.
    caseFolding.parse("Mix/Mix") should haveValue(SampleNamed.OTHER)
    caseFolding.parse("MIX/MIX") should haveValue(SampleNamed.OTHER)
    caseFolding.parse("mix/mix") should haveValue(SampleNamed.OTHER)
    caseFolding.parse("mIx/MiX") should haveValue(SampleNamed.OTHER)
  }

  test("a rewrite that turns the inline flag off again applies to nothing, the input being folded first (ExtendedEnumTest.test_enum_lenient)") {
    // The copy is made by prefixing an inline flag to the source, so a source that turns
    // that flag off again is beyond its reach and can match only the lower-case text it
    // spells. The fold has already turned the input to upper case by then, so no input
    // whatever reaches this rule.
    caseFolding.parse("sensitive/sensitive") should beFailure
    caseFolding.parse("SENSITIVE/SENSITIVE") should beFailure
    caseFolding.parse("Sensitive/Sensitive") should beFailure
    // The rule is unreachable rather than merely broken, and its target is a member this
    // family has: compiled the way the family compiles it, the expression does match the
    // text it spells, so the fold of the input is the only thing keeping it from applying.
    ("(?i)" + CaseSensitiveSource).r.matches("sensitive/sensitive") shouldBe true
    ("(?i)" + CaseSensitiveSource).r.matches("SENSITIVE/SENSITIVE") shouldBe false
    caseFolding.valueOf("More") shouldBe Some(SampleNamed.MORE)
  }

  test("a family declaring no rewrite still folds the case of its input (ExtendedEnumTest.test_enum_lenient)") {
    SingleValueNamed.lookup.lenientPatterns shouldBe empty
    SingleValueNamed.lookup.valueOf("solo") shouldBe None
    SingleValueNamed.lookup.parse("solo") should haveValue(SingleValueNamed.SOLO)
  }

  test("a rewrite must match the whole of the text to be applied (ExtendedEnumTest.test_enum_lenient)") {
    // `A1` matches the first rewrite in full while `XA1` does not match it anywhere it
    // could be applied, so the text passes through the table unchanged and resolves to
    // nothing.
    sample.parse("XA1") should beFailure
    sample.parse("A1X") should beFailure
  }

  test("rewriteLeniently applies the chain of a family on its own (ExtendedEnumTest.test_enum_lenient)") {
    // The third step of `parse`, published on its own for a family whose name space is wider
    // than its closed members and which therefore has to run the chain between its own exact
    // lookup and its own repeat lookup. It is the same chain, rule for rule: `A1` becomes
    // `B1` and then `Standard`, text no expression matches comes back as it was, and a family
    // declaring no expression is the identity.
    sample.rewriteLeniently("A1") shouldBe "Standard"
    sample.rewriteLeniently("A2") shouldBe "More"
    sample.rewriteLeniently("B1") shouldBe "Standard"
    sample.rewriteLeniently("XA1") shouldBe "XA1"
    sample.rewriteLeniently("") shouldBe ""
    SingleValueNamed.lookup.rewriteLeniently("anything at all") shouldBe "anything at all"
    // And running it with the exact lookup is the whole of what `parse` does for a name that
    // only a rewrite reaches, which is what makes it usable by such a family.
    sample.valueOf(sample.rewriteLeniently("A1")) shouldBe Some(SampleNamed.STANDARD)
  }

  test("rewriteLeniently applies its expressions without regard to case (ExtendedEnumTest.test_enum_lenient)") {
    // The expressions are compiled to ignore case, so the chain rewrites text of any case
    // when it is called directly - `parse` has folded its input by the time it reaches the
    // chain, so the tolerance is only observable through this operation.
    sample.rewriteLeniently("a1") shouldBe "Standard"
    caseFolding.rewriteLeniently("mix/mix") shouldBe "Other"
    caseFolding.rewriteLeniently("MIX/MIX") shouldBe "Other"
  }

  test("a family may hand over the sources of its rewrites rather than the expressions (ExtendedEnumTest.test_enum_lenient)") {
    // `ofSources` is the form the families of the library declare their tables in: a rule
    // arrives as text and is compiled exactly once to be applied, where a rule handed over
    // already compiled is compiled a second time. The table reads back as the rows it was
    // given, so a family's transcription can still be compared against its source.
    fromSources.lenientPatterns.map(sourceOf) shouldBe SampleNamed.LenientSources
    fromSources.lenientPatterns.map(sourceOf) shouldBe sample.lenientPatterns.map(sourceOf)
    bracketRewrite.lenientPatterns.map(sourceOf) shouldBe List(BracketSource -> "Standard")
    // And the lookup it builds resolves what the lookup built from compiled expressions
    // resolves, through every table the family declared: the chain, the alternate spelling,
    // the exact lookup and the group of external spellings.
    fromSources.parse("A1") should haveValue(SampleNamed.STANDARD)
    fromSources.parse("A2") should haveValue(SampleNamed.MORE)
    fromSources.parse("standard") should haveValue(SampleNamed.STANDARD)
    fromSources.valueOf("Alternate") shouldBe Some(SampleNamed.STANDARD)
    fromSources.externalNames("Bar") shouldBe sample.externalNames("Bar")
    fromSources.parse("Rubbish") should beFailure
  }

  //-------------------------------------------------------------------------
  // the length of the text the lenient stage is applied to, which is any length
  //-------------------------------------------------------------------------

  test("every spelling a family resolves is resolved whatever the length of the text") {
    // No text is refused for its size, at either stage: the lenient lookup of the type being
    // ported applied every one of its rewrites to whatever text arrived, and so does this
    // one. Every route into a family - a canonical name, its upper-case form, a folded
    // spelling, an alternate spelling and a spelling only a rewrite reaches - is therefore
    // reached by the text that names it and by nothing else. They are asserted together
    // here because all of them run through the one operation.
    sample.parse("Standard") should haveValue(SampleNamed.STANDARD)
    sample.parse("STANDARD") should haveValue(SampleNamed.STANDARD)
    sample.parse("standard") should haveValue(SampleNamed.STANDARD)
    sample.parse("Alternate") should haveValue(SampleNamed.STANDARD)
    sample.parse("ALTERNATE") should haveValue(SampleNamed.STANDARD)
    sample.parse("alternate") should haveValue(SampleNamed.STANDARD)
    sample.parse("A1") should haveValue(SampleNamed.STANDARD)
    sample.parse("a1") should haveValue(SampleNamed.STANDARD)
    mock.parse("TWENTY_ONE") should haveValue(MockEnum.TWENTY_ONE)
    mock.parse("twenty_one") should haveValue(MockEnum.TWENTY_ONE)
  }

  test("a spelling longer than every key of its family resolves, exactly and folded") {
    // Neither stage counts the characters of the text it is handed, so a family reached
    // through an alternate spelling far longer than any of its names resolves that spelling
    // and its upper-case form exactly, and its lower-case form through the fold of the
    // lenient stage.
    LongAliasSpelling.length should be > sample.values.toList.map(_.name.length).max
    longAlias.valueOf(LongAliasSpelling) shouldBe Some(SampleNamed.MORE)
    longAlias.parse(LongAliasSpelling) should haveValue(SampleNamed.MORE)
    longAlias.parse(LongAliasSpelling.toUpperCase(Locale.ENGLISH)) should haveValue(SampleNamed.MORE)
    longAlias.parse(LongAliasSpelling.toLowerCase(Locale.ENGLISH)) should haveValue(SampleNamed.MORE)
  }

  test("a name far longer than every name of the sample family resolves exactly and in lower case") {
    // The fold of the input to upper case is the whole of the leniency a family that declares
    // no rewrite has, and it is applied to text of every length, so the lower-case spelling
    // of a name written out at this length resolves through it while the exact lookup, which
    // folds nothing, refuses it.
    val spelling = LongNameNamed.SPELLED_OUT.name
    spelling.length should be > sample.values.toList.map(_.name.length).max
    LongNameNamed.lookup.lenientPatterns shouldBe empty
    LongNameNamed.lookup.valueOf(spelling) shouldBe Some(LongNameNamed.SPELLED_OUT)
    LongNameNamed.lookup.valueOf(spelling.toUpperCase(Locale.ENGLISH)) shouldBe
      Some(LongNameNamed.SPELLED_OUT)
    LongNameNamed.lookup.valueOf(spelling.toLowerCase(Locale.ENGLISH)) shouldBe None
    LongNameNamed.lookup.parse(spelling) should haveValue(LongNameNamed.SPELLED_OUT)
    LongNameNamed.lookup.parse(spelling.toLowerCase(Locale.ENGLISH)) should
      haveValue(LongNameNamed.SPELLED_OUT)
  }

  test("a rewrite that can consume text of any length is applied to text of any length") {
    // The single rewrite of this family turns any text ending in `X` into the name of a
    // member, so a resolution is proof that the rewrite ran over the whole of the text. It
    // runs over a hundred thousand characters as readily as over one: there is no length at
    // which the stage stops being applied, which is the lenient lookup of the type being
    // ported and what the specification of this port requires of it.
    greedyRewrite.parse("X") should haveValue(SampleNamed.STANDARD)
    greedyRewrite.parse("anything at all, ending in x") should haveValue(SampleNamed.STANDARD)
    greedyRewrite.parse("A" * 100000 + "X") should haveValue(SampleNamed.STANDARD)
    greedyRewrite.parse("A" * 100000 + "x") should haveValue(SampleNamed.STANDARD)
    // And text the rewrite cannot match is named in full in the failure it reports, as every
    // rejected name is: the message names what was refused, and bounding it for a reader is the
    // business of writing the failure out. The length of the text decides neither the answer nor
    // the wording.
    val refused = "A" * 100000 + "Y"
    val parsed: ResultNec[SampleNamed] = greedyRewrite.parse(refused)
    parsed should beFailureWith(FailureReason.PARSING)
    failuresOf(parsed) shouldBe List(Failure.Parsing(s"GreedyRewrite name not found: $refused"))
    failuresOf(parsed).head.attributes shouldBe empty
  }

  test("a rewrite is not applied to text that could not match it, which changes no answer") {
    // What the length of the text may not do is cost more than the text is worth, and that is
    // settled by asking each expression once - when the family hands it over - which
    // character a full match of it must end with. The expression of this family ends in the
    // plain literal `X`, so text ending in anything else is a match that cannot happen and is
    // declined rather than attempted.
    //
    // The property that makes the requirement invisible is asserted by result rather than by
    // timing, in both directions: text the expression could match resolves however long it
    // is, and text it could not is answered exactly as it would have been had the expression
    // been applied and failed to match.
    greedyRewrite.parse("A" * 100000 + "X") should haveValue(SampleNamed.STANDARD)
    greedyRewrite.parse("A" * 100000 + "Y") should beFailure
    greedyRewrite.parse("XA") should beFailure
    // A declined expression leaves the text as the expressions before it left it, so every
    // other route into the family is reached exactly as it is when no expression is declared:
    // the fold still resolves a lower-case name, and the exact lookup is untouched.
    greedyRewrite.parse("standard") should haveValue(SampleNamed.STANDARD)
    greedyRewrite.parse("another1") should haveValue(SampleNamed.ANOTHER1)
    greedyRewrite.valueOf("Standard") shouldBe Some(SampleNamed.STANDARD)
  }

  test("an expression closing with a class of one character requires that character of the text") {
    // The second shape the requirement is read from, and the shape the tables of the library
    // actually carry: a class holding exactly one character, which is how a literal bracket
    // is spelled in them. The rule resolves the text it was written for, of any length ...
    bracketRewrite.parse("FOO(BAR)") should haveValue(SampleNamed.STANDARD)
    bracketRewrite.parse("(" * 1000 + ")") should haveValue(SampleNamed.STANDARD)
    // ... and text not ending in the bracket it requires is answered without it, which is
    // the same answer the expression itself would have given.
    bracketRewrite.parse("FOO(BAR") should beFailure
    bracketRewrite.parse("standard") should haveValue(SampleNamed.STANDARD)
  }

  test("a rewrite whose source ends in a comment is applied, its text notwithstanding") {
    // The requirement on the last character of the text is read from the text of a source, so a
    // source whose tail is not part of its language must yield no requirement at all. Under the
    // inline flag that turns comments on, `(?x)A # X` matches `A` and nothing else; reading the
    // `X` as required would refuse the one text this rule accepts. Asserted through both
    // constructors, because either could be handed such a source.
    CommentedSource.r.matches("A") shouldBe true
    commentedRewrite.rewriteLeniently("A") shouldBe "Standard"
    commentedRewrite.parse("A") should haveValue(SampleNamed.STANDARD)
    commentedRewriteFromPatterns.rewriteLeniently("A") shouldBe "Standard"
    commentedRewriteFromPatterns.parse("A") should haveValue(SampleNamed.STANDARD)
    // And the text the source spells is not accepted, the expression not matching it either.
    CommentedSource.r.matches("A # X") shouldBe false
    commentedRewrite.parse("A # X") should beFailure
  }

  test("hostile text that no expression of a family could match is answered in one pass") {
    // The regression guard for the shape above, and the only test here that measures time.
    // The expression is the one row of the transcribed day-count table that can consume text
    // of unbounded length, and two hundred thousand opening brackets is the text crafted to
    // make it backtrack: every position of that text is a position its first group could end
    // at. Applied to the text the expression costs minutes; declined on the last character of
    // the text it costs milliseconds. The limit is generous because what it has to separate
    // is milliseconds from minutes, and a loaded host must not be able to make it flake.
    val hostile = "(" * HostileInputLength
    failAfter(Span(60, Seconds)) {
      bracketRewrite.parse(hostile) should beFailure
      // Text of exactly the same size, ending in the bracket the expression requires, is
      // rewritten and resolves. What decides is the last character of the text and never the
      // size of it, which is why nothing here is a bound on the length of an input.
      bracketRewrite.parse(hostile.dropRight(1) + ")") should haveValue(SampleNamed.STANDARD)
    }
  }

  test("the chain of rewrites is applied to text of every length") {
    // The single rewrite of this family matches every text there is and turns it into the
    // name of a member, so a resolution is proof that the chain ran over the text. Forty
    // thousand characters and two hundred thousand characters both resolve, which is the
    // absence of a cutoff stated as plainly as this typeclass allows it to be stated.
    rewritesAnything.parse("literally anything") should haveValue(SampleNamed.STANDARD)
    rewritesAnything.parse("A" * 40000) should haveValue(SampleNamed.STANDARD)
    rewritesAnything.parse("A" * HostileInputLength) should haveValue(SampleNamed.STANDARD)
  }

  test("the names a family declares decide nothing about the text its rewrites accept") {
    // The same rewrite over two families whose names differ greatly in length accepts the
    // same text. Were the stage bounded by the data of a family, the family of short names
    // would report text that the family of long names resolved, and this pair of
    // measurements is what makes that impossible to introduce unnoticed.
    val text = "A" * 5000
    sample.values.toList.map(_.name.length).max should be < LongNameNamed.SPELLED_OUT.name.length
    rewritesAnything.parse(text) should haveValue(SampleNamed.STANDARD)
    longNameRewritesAnything.parse(text) should haveValue(LongNameNamed.SPELLED_OUT)
  }

  test("a rewrite accepts the text its expression admits, however the source is written") {
    // The length of a source decides nothing either. This one is longer than every key of
    // its family and admits a name followed by any number of underscores, and any number is
    // what the family accepts - while text the source does not admit still resolves to
    // nothing, so the leniency is that of the expression rather than of its length.
    val sourceLength = longSourceRewrite.lenientPatterns.map { case (expression, _) =>
      expression.pattern.pattern().length
    }.max
    sourceLength should be > sample.values.toList.map(_.name.length).max
    longSourceRewrite.parse("MORE" + "_" * (sourceLength + 1)) should haveValue(SampleNamed.MORE)
    longSourceRewrite.parse("MORE" + "_" * 5000) should haveValue(SampleNamed.MORE)
    longSourceRewrite.parse("MORE" + "-" * 5000) should beFailure
  }

  test("parsing names the text it rejected in full, and the failure renders bounded and on one line") {
    // The message is the one the lookup being ported raised - the family, then the text as it
    // was supplied - so a caller correcting its input is handed back exactly what was refused.
    // This operation is the one every family parses through, so the wording is asserted here
    // once for all of them, together with the property that makes it safe to write out: the
    // rendering of the failure bounds every part it writes and escapes anything that could
    // forge a line, so an adversarial name cannot reach a log unbounded or across lines.
    val payload = "H" * 10000
    val large: ResultNec[SampleNamed] = sample.parse(payload)
    large should beFailureWith(FailureReason.PARSING)
    val failure = failuresOf(large).head
    failure.message shouldBe s"SampleNamed name not found: $payload"
    // The rendering is where the size stops: a ten-thousand-character name renders to a line of
    // a few hundred characters, marked to say that there was more.
    val rendered = Show[Failure].show(failure)
    rendered.length should be < 1000
    rendered should startWith("PARSING: SampleNamed name not found: HHH")
    rendered should endWith("...")

    // Text holding a line break is named as it stands and rendered on one line, so a
    // line-oriented consumer of the rendering cannot be made to record a line the library did
    // not report.
    val injected: ResultNec[SampleNamed] = sample.parse("EUR\nUSD")
    injected should beFailureWith(FailureReason.PARSING)
    val injectedFailure = failuresOf(injected).head
    injectedFailure.message shouldBe "SampleNamed name not found: EUR\nUSD"
    val injectedRendering = Show[Failure].show(injectedFailure)
    injectedRendering should not include "\n"
    injectedRendering should not include "\r"
    injectedRendering shouldBe "PARSING: SampleNamed name not found: EUR\\nUSD"

    // And an ordinary rejected name reads the same in the failure and in its rendering.
    failuresOf(sample.parse("Rubbish")).head.message shouldBe "SampleNamed name not found: Rubbish"
    Show[Failure].show(failuresOf(sample.parse("Rubbish")).head) shouldBe
      "PARSING: SampleNamed name not found: Rubbish"
  }

  //-------------------------------------------------------------------------
  // the smallest family there can be, standing in for the ported empty family
  //-------------------------------------------------------------------------

  test("a family with one member and no tables resolves that member and nothing else (ExtendedEnumTest.test_enum_SampleOther)") {
    // The original had a family whose configuration declared no provider at all, so it
    // resolved nothing: its whole lookup, its alternate table and its groups were empty.
    // That state cannot exist here, because a family's members arrive as a non-empty list,
    // so the case is carried by the smallest family that can exist instead.
    SingleValueNamed.lookup.values.toList shouldBe List(SingleValueNamed.SOLO)
    SingleValueNamed.lookup.byCanonicalName shouldBe Map("Solo" -> SingleValueNamed.SOLO)
    SingleValueNamed.lookup.byUpperName shouldBe Map("SOLO" -> SingleValueNamed.SOLO)
  }

  test("a family with one member declares no table at all (ExtendedEnumTest.test_enum_SampleOther)") {
    SingleValueNamed.lookup.alternateNames shouldBe empty
    SingleValueNamed.lookup.externalNameGroups shouldBe empty
    SingleValueNamed.lookup.lenientPatterns shouldBe empty
    SingleValueNamed.lookup.externalNames("Foo") shouldBe None
  }

  test("a family with one member reports text that names no member (ExtendedEnumTest.test_enum_SampleOther)") {
    SingleValueNamed.lookup.valueOf("Rubbish") shouldBe None
    SingleValueNamed.lookup.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
    SingleValueNamed.lookup.toString shouldBe "NamedEnum[SampleOther]"
  }

  test("a family with no members does not compile, which is why the ported empty family has no counterpart (ExtendedEnumTest.test_enum_SampleOther)") {
    assertDoesNotCompile("""NonEmptyList.of[SampleNamed]()""")
    assertDoesNotCompile("""NamedEnum.of(List(SampleNamed.STANDARD))""")
    assertDoesNotCompile("""NamedEnum.of(Nil)""")
  }

  //-------------------------------------------------------------------------
  // misconfiguration, which the port moved from a run-time warning to a compile error
  //-------------------------------------------------------------------------

  test("a family that declares no members does not compile (ExtendedEnumTest.test_enum_invalid)") {
    // The original had seven separately misconfigured families - a provider class that did
    // not exist, one that was not of the right type, a member that was not a constant, and
    // so on. Each logged a warning and left the family resolving nothing, which is why that
    // test could only assert emptiness. There is no configuration to get wrong here, so
    // each of those mistakes is now either impossible to write or rejected by the compiler,
    // and the cases are carried by these proofs.
    assertDoesNotCompile("""NamedEnum.of()""")
  }

  test("a member that is not a named value does not compile (ExtendedEnumTest.test_enum_invalid)") {
    assertDoesNotCompile("""NamedEnum.of(NonEmptyList.of("Standard"))""")
    assertDoesNotCompile("""NamedEnum.of(NonEmptyList.of(1))""")
  }

  test("a table of the wrong shape does not compile (ExtendedEnumTest.test_enum_invalid)") {
    assertDoesNotCompile(
      """NamedEnum.of(NonEmptyList.of(SampleNamed.STANDARD), List("Alternate" -> "Standard"))""")
    assertDoesNotCompile(
      """NamedEnum.of(NonEmptyList.of(SampleNamed.STANDARD), Map("Alternate" -> SampleNamed.STANDARD))""")
    assertDoesNotCompile(
      """NamedEnum.of(NonEmptyList.of(SampleNamed.STANDARD), Map.empty[String, String], Map("B1".r -> "Standard"))""")
  }

  test("a family resolves nothing it was not given, and the lookup has no way to be extended (ExtendedEnumTest.test_enum_invalid)") {
    // Nothing reads a class path, a resource or a class, so the resolvable name space of a
    // family is the one visible in its companion. The proof is behavioural: a name that no
    // table of the fixture mentions resolves to nothing, however it is spelled.
    List("Provider", "SampleNamed", "com.opengamma.strata.collect.named.SampleNameds", "base")
      .foreach { absent =>
        sample.valueOf(absent) shouldBe None
        sample.parse(absent) should beFailure
      }
  }

  //-------------------------------------------------------------------------
  // the name-keyed factory the ported interface carried
  //-------------------------------------------------------------------------

  test("a name resolves through the lookup its family publishes (NamedTest.test_of)") {
    // The original resolved a name by searching the family's own type for a factory method
    // at run time. The port resolves it through the typeclass instance the family publishes,
    // which the compiler finds, so the two cases that test carried are these.
    sample.valueOf("Standard") shouldBe Some(SampleNamed.STANDARD)
    sample.valueOf("Rubbish") shouldBe None
  }

  test("the reflective name-keyed factory of the ported interface is gone (NamedTest.test_of)") {
    // `Named` declares the name of a value and nothing else, and has no companion, so there
    // is no route from a type and a name to a value except the family's own lookup.
    assertDoesNotCompile("""Named.of(classOf[SampleNamed], "Standard")""")
    assertDoesNotCompile("""Named.of("Standard")""")
    assertDoesNotCompile("""Named.valueOf("Standard")""")
  }

  //-------------------------------------------------------------------------
  // the union of several families, which replaces the ported combined registry
  //-------------------------------------------------------------------------

  test("a union resolves a name from the family that has it (CombinedExtendedEnumTest.test_lookup)") {
    UberNamed.parse("Standard") should haveValue(FirstNamed.STANDARD)
    UberNamed.parse("More") should haveValue(SecondNamed.MORE)
  }

  test("a union tries its families in declaration order (CombinedExtendedEnumTest.test_lookup)") {
    // Both families have a member named `Shared`, and the union names the first family
    // first, so that is the member it resolves. The order is the whole of what the union
    // is, which is why it is asserted rather than assumed.
    FirstNamed.lookup.valueOf("Shared") shouldBe Some(FirstNamed.SHARED)
    SecondNamed.lookup.valueOf("Shared") shouldBe Some(SecondNamed.SHARED)
    UberNamed.parse("Shared") should haveValue(FirstNamed.SHARED)
    UberNamed.parse("Shared") shouldNot haveValue(SecondNamed.SHARED)
  }

  test("a union reports a name that no family it spans has (CombinedExtendedEnumTest.test_lookup)") {
    UberNamed.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
    UberNamed.parse("Rubbish") should haveFailureMessageMatching("UberNamed name not found: Rubbish")
  }

  test("a union resolves the upper-case form of a member's name (CombinedExtendedEnumTest.test_lookup)") {
    UberNamed.parse("STANDARD") should haveValue(FirstNamed.STANDARD)
    UberNamed.parse("MORE") should haveValue(SecondNamed.MORE)
  }

  test("no combined lookup type exists: a union is a function over the lookups it names (CombinedExtendedEnumTest.test_lookup)") {
    // The original assembled a second registry over the first, with a type and a rendering
    // of its own. Here the union is the function asserted above, so there is no instance of
    // the typeclass spanning several families and nothing to summon for one.
    assertDoesNotCompile("""NamedEnum[Named]""")
    assertDoesNotCompile("""NamedEnum.combined(FirstNamed.lookup, SecondNamed.lookup)""")
  }

  //-------------------------------------------------------------------------
  // the ported formatter of a plain enumeration
  //-------------------------------------------------------------------------

  test("the canonical name of a member is the formatted name of the ported constant (EnumNamesTest.test_format)") {
    MockEnum.ONE.name shouldBe "One"
    MockEnum.TWENTY_ONE.name shouldBe "TwentyOne"
    MockEnum.FooBar.name shouldBe "Foobar"
    MockEnum.WOO_BAR_WAA.name shouldBe "WooBarWaa"
  }

  test("rendering a member renders its canonical name (EnumNamesTest.test_format)") {
    Show[MockEnum].show(MockEnum.ONE) shouldBe "One"
    Show[MockEnum].show(MockEnum.TWENTY_ONE) shouldBe "TwentyOne"
    Show[MockEnum].show(MockEnum.FooBar) shouldBe "Foobar"
    Show[MockEnum].show(MockEnum.WOO_BAR_WAA) shouldBe "WooBarWaa"
  }

  test("parse accepts every spelling of the single-word member (EnumNamesTest.test_parse_one)") {
    forAll(Table("spelling", "One", "ONE", "one")) { (spelling: String) =>
      mock.parse(spelling) should haveValue(MockEnum.ONE)
    }
  }

  test("parse accepts every spelling of the two-word member (EnumNamesTest.test_parse_twentyOne)") {
    forAll(Table("spelling", "TwentyOne", "TWENTYONE", "twentyone", "TWENTY_ONE", "twenty_one")) {
      (spelling: String) => mock.parse(spelling) should haveValue(MockEnum.TWENTY_ONE)
    }
  }

  test("the underscore tolerance of the ported formatter is a lenient rewrite here (EnumNamesTest.test_parse_twentyOne)") {
    // The formatter of the original removed underscores while comparing; the port declares
    // that tolerance as rewrites over the family, which is the mechanism this typeclass
    // offers. The exact lookup consequently does not accept the underscored spelling.
    mock.valueOf("TWENTY_ONE") shouldBe None
    mock.parse("TWENTY_ONE") should haveValue(MockEnum.TWENTY_ONE)
    mock.lenientPatterns.map(sourceOf) shouldBe
      List("([A-Z0-9]+)_([A-Z0-9]+)_([A-Z0-9]+)" -> "$1$2$3", "([A-Z0-9]+)_([A-Z0-9]+)" -> "$1$2")
  }

  test("parse accepts every spelling of the mixed-case member, including its alias (EnumNamesTest.test_parse_fooBarWithAlias)") {
    forAll(Table("spelling", "Foobar", "FOOBAR", "foobar", "FooBar", "Fb", "FB", "fb")) {
      (spelling: String) => mock.parse(spelling) should haveValue(MockEnum.FooBar)
    }
  }

  test("the parse alias of the ported formatter is an alternate name here (EnumNamesTest.test_parse_fooBarWithAlias)") {
    mock.alternateNames shouldBe Map("Fb" -> "Foobar", "FB" -> "Foobar")
    mock.valueOf("Fb") shouldBe Some(MockEnum.FooBar)
    mock.valueOf("FB") shouldBe Some(MockEnum.FooBar)
    mock.valueOf("fb") shouldBe None
  }

  test("parse accepts every spelling of the three-word member (EnumNamesTest.test_parse_wooBarWaa)") {
    forAll(Table("spelling", "WooBarWaa", "WOOBARWAA", "woobarwaa", "WOO_BAR_WAA", "woo_bar_waa")) {
      (spelling: String) => mock.parse(spelling) should haveValue(MockEnum.WOO_BAR_WAA)
    }
  }

  test("parse reports an unknown name of a plain enumeration rather than raising (EnumNamesTest.test_parse_invalid)") {
    val parsed: ResultNec[MockEnum] = mock.parse("unknown")
    parsed should beFailureWith(FailureReason.PARSING)
    parsed should haveFailureMessageMatching("MockEnum name not found: unknown")
    failuresOf(parsed) shouldBe List(Failure.Parsing("MockEnum name not found: unknown"))
  }

  test("parse reports a spelling that no rewrite of the family produces (EnumNamesTest.test_parse_invalid)") {
    forAll(Table("spelling", "Twenty-One", "TWENTY__ONE", "Woo_Bar", "ONE_TWO_THREE_FOUR", "")) {
      (spelling: String) => mock.parse(spelling) should beFailure
    }
  }

  //-------------------------------------------------------------------------
  // closedness
  //-------------------------------------------------------------------------

  test("every member of a family resolves to itself, so its names are exhaustive and distinct") {
    // The colliding family is swept alongside the rest, and is the reason the canonical name
    // of a member is claimed unconditionally: its two members fold to one key, and each is
    // still reached by the name it publishes.
    List[NamedEnum[_ <: Named]](
      sample,
      mock,
      SingleValueNamed.lookup,
      FirstNamed.lookup,
      SecondNamed.lookup,
      CollidingNamed.lookup)
      .foreach { family =>
        val members = family.values.toList
        members.foreach(member => family.valueOf(member.name) shouldBe Some(member))
        family.byCanonicalName.size shouldBe members.size
      }
  }

  test("a family of this module cannot be extended from another file") {
    // The families of the library are sealed and their constructors are not public, so the
    // set of members of each is fixed where it is declared. The reason family of the result
    // package is declared in its own file, which makes it the right subject: a new member
    // of it cannot be written here.
    assertDoesNotCompile("""case object SNEAKY extends FailureReason("SNEAKY")""")
    assertDoesNotCompile("""final class Sneaky extends FailureReason("SNEAKY")""")
  }

  test("the members a family declares are the only instances of it") {
    assertDoesNotCompile("""new SampleNamed("Sneaky") {}""")
    assertDoesNotCompile("""SampleNamed("Sneaky")""")
    assertDoesNotCompile("""SampleNamed.STANDARD.copy(name = "Sneaky")""")
  }

  test("a family resolves names from its members and its tables alone, with no lookup of a resource") {
    // Held as a behavioural statement of the same property the build gates check over the
    // sources: the name space of the fixture is the five members, the one alternate
    // spelling and the two rewrite targets, and nothing outside it resolves.
    val resolvable = sample.values.toList.flatMap(member =>
      List(member.name, member.name.toUpperCase(Locale.ENGLISH))) ++ List("Alternate", "ALTERNATE")
    resolvable.foreach(name => sample.valueOf(name) should not be None)
    val everyKey = sample.byCanonicalName.keySet ++ sample.byUpperName.keySet ++ sample.alternateNames.keySet
    everyKey shouldBe resolvable.toSet
  }

  //-------------------------------------------------------------------------
  // the views a key can be missing from, which is where the ported asymmetry went
  //-------------------------------------------------------------------------

  test("a member whose canonical name is already upper case offers one key rather than two") {
    // The reason family of the result package is the real family of this module with that
    // shape: every one of its names is upper case, so folding a name changes nothing and
    // the two key views coincide.
    val reasons = NamedEnum[FailureReason]
    reasons.values.toList.size shouldBe 10
    reasons.byUpperName shouldBe reasons.byCanonicalName
    reasons.byCanonicalName.size shouldBe 10
    reasons.valueOf("PARSING") shouldBe Some(FailureReason.PARSING)
  }

  test("a real family of this module resolves its names through the same two operations") {
    val reasons = NamedEnum[FailureReason]
    reasons.valueOf("MISSING_DATA") shouldBe Some(FailureReason.MISSING_DATA)
    reasons.valueOf("missing_data") shouldBe None
    reasons.parse("missing_data") should haveValue(FailureReason.MISSING_DATA)
    reasons.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
  }

  test("when two members fold to one key each keeps the name it publishes") {
    // A member's own canonical name is claimed unconditionally and the folded spellings fill
    // only the slots still free, which is the precedence of the loader the port took its
    // registration from: that loader put each row under its own name unconditionally and
    // under the folded spelling only where the spelling was free. It is what keeps every
    // member of every family resolvable by the name it publishes - here the later member is
    // named `DUP`, which is also the folded spelling of the earlier member's `Dup`, and it
    // reaches itself through that name rather than reaching the member declared before it.
    //
    // So the only view that can be narrower than the member list is the folded one: the two
    // members genuinely share one folded key, and it belongs to the member whose canonical
    // name it is. This is where the single-key member of the fixture of the original went.
    CollidingNamed.lookup.values.toList shouldBe
      List(CollidingNamed.MIXED, CollidingNamed.UPPER, CollidingNamed.PLAIN)
    CollidingNamed.lookup.valueOf("Dup") shouldBe Some(CollidingNamed.MIXED)
    CollidingNamed.lookup.valueOf("DUP") shouldBe Some(CollidingNamed.UPPER)
    CollidingNamed.lookup.valueOf("Plain") shouldBe Some(CollidingNamed.PLAIN)
    CollidingNamed.lookup.byCanonicalName shouldBe
      Map(
        "Dup" -> CollidingNamed.MIXED,
        "DUP" -> CollidingNamed.UPPER,
        "Plain" -> CollidingNamed.PLAIN)
    CollidingNamed.lookup.byUpperName shouldBe
      Map("DUP" -> CollidingNamed.UPPER, "PLAIN" -> CollidingNamed.PLAIN)
    // Every member is in the canonical view, and the folded view is the one short entry.
    CollidingNamed.lookup.byCanonicalName.size shouldBe CollidingNamed.lookup.values.toList.size
    CollidingNamed.lookup.byUpperName.size shouldBe
      CollidingNamed.lookup.values.toList.size - 1
    // The lenient stage folds its input, so the spelling that belongs to neither member
    // reaches the member whose canonical name the fold produces.
    CollidingNamed.lookup.valueOf("dup") shouldBe None
    CollidingNamed.lookup.parse("dup") should haveValue(CollidingNamed.UPPER)
  }

  //-------------------------------------------------------------------------
  // the instances a family publishes
  //-------------------------------------------------------------------------

  test("a family renders, hashes and compares by name") {
    Show[SampleNamed].show(SampleNamed.STANDARD) shouldBe "Standard"
    Hash[SampleNamed].hash(SampleNamed.STANDARD) shouldBe "Standard".hashCode
    Hash[SampleNamed].eqv(SampleNamed.STANDARD, SampleNamed.STANDARD) shouldBe true
    Hash[SampleNamed].eqv(SampleNamed.STANDARD, SampleNamed.MORE) shouldBe false
  }

  test("ordering a family orders it by name, and agrees with its equality") {
    Order[SampleNamed].compare(SampleNamed.ANOTHER1, SampleNamed.STANDARD) should be < 0
    Order[SampleNamed].compare(SampleNamed.STANDARD, SampleNamed.ANOTHER1) should be > 0
    Order[SampleNamed].compare(SampleNamed.STANDARD, SampleNamed.STANDARD) shouldBe 0
    sample.values.toList.sorted(Order[SampleNamed].toOrdering) shouldBe
      List(
        SampleNamed.ANOTHER1,
        SampleNamed.ANOTHER2,
        SampleNamed.MORE,
        SampleNamed.OTHER,
        SampleNamed.STANDARD)
  }

  test("comparing two members is zero exactly when they are equal") {
    val members = sample.values.toList
    members.foreach { left =>
      members.foreach { right =>
        (Order[SampleNamed].compare(left, right) == 0) shouldBe Order[SampleNamed].eqv(left, right)
      }
    }
  }

  test("a family with no order hashes by name, which is not the hashing of its values") {
    // The hashing offered on its own, for a family whose members carry no meaningful order.
    // The hash of a member is the hash of its name, so a hashing that answered a constant,
    // or that fell back on the identity of the value, is visible here: the twin is a
    // different value of the same name, and the two members are different names.
    val hash = Hash[UnorderedNamed]
    UnorderedNamed.values.toList.foreach { member =>
      hash.hash(member) shouldBe member.name.hashCode
    }
    hash.hash(UnorderedNamed.FirstTwin) shouldBe "First".hashCode
    hash.hash(UnorderedNamed.FirstTwin) shouldBe hash.hash(UnorderedNamed.FIRST)
    hash.hash(UnorderedNamed.FIRST) should not be hash.hash(UnorderedNamed.SECOND)
  }

  test("a family with no order compares by name, which is not the equality of its values") {
    val hash = Hash[UnorderedNamed]
    // The twin is not the member, under the equality the language gives every value ...
    (UnorderedNamed.FirstTwin == UnorderedNamed.FIRST) shouldBe false
    // ... and is the member, under the equality the family publishes.
    hash.eqv(UnorderedNamed.FIRST, UnorderedNamed.FirstTwin) shouldBe true
    hash.eqv(UnorderedNamed.FirstTwin, UnorderedNamed.FIRST) shouldBe true
    hash.eqv(UnorderedNamed.FIRST, UnorderedNamed.FIRST) shouldBe true
    hash.eqv(UnorderedNamed.FIRST, UnorderedNamed.SECOND) shouldBe false
  }

  test("the equality of a family with no order arrives from the one instance it publishes") {
    // Hashing extends equality, so the single instance is the whole of what the family has
    // to declare for both to be available and to agree by construction: both summon to the
    // one value the family published, rather than to two that could disagree.
    Eq[UnorderedNamed] shouldBe UnorderedNamed.hash
    Hash[UnorderedNamed] shouldBe UnorderedNamed.hash
    Eq[UnorderedNamed].eqv(UnorderedNamed.FIRST, UnorderedNamed.FirstTwin) shouldBe true
    Eq[UnorderedNamed].eqv(UnorderedNamed.FIRST, UnorderedNamed.SECOND) shouldBe false
  }

  test("hashing by name and ordering by name agree, which is why either may be published") {
    // The two helpers are documented to agree on hashing and on equality, so that a family
    // moving from one to the other keeps the semantics it had. Asserted over the twin as
    // well as the members, since that is where a fallback on the value rather than the name
    // would show.
    val hashOnly = Hash[UnorderedNamed]
    val ordered: Order[UnorderedNamed] with Hash[UnorderedNamed] = NamedEnum.orderByName
    val everyValue = UnorderedNamed.FirstTwin :: UnorderedNamed.values.toList
    everyValue.foreach { left =>
      ordered.hash(left) shouldBe hashOnly.hash(left)
      everyValue.foreach { right =>
        ordered.eqv(left, right) shouldBe hashOnly.eqv(left, right)
        (ordered.compare(left, right) == 0) shouldBe hashOnly.eqv(left, right)
      }
    }
  }

  test("a family with no order publishes no ordering and no rendering") {
    // The helpers are plain methods, so a family gets exactly the instances it declares:
    // this one declared the hashing alone, and neither of the other two can be summoned for
    // it however the members of the family compare as text.
    assertDoesNotCompile("""Order[UnorderedNamed]""")
    assertDoesNotCompile("""Show[UnorderedNamed]""")
    assertCompiles("""Hash[UnorderedNamed]""")
  }

  test("the typeclass publishes no instance of its own, so a family must publish its lookup") {
    // The helpers that build the instances of a family are plain methods, and the typeclass
    // declares no implicit at all, so summoning a lookup finds exactly the one its family
    // published and a family that publishes none is not resolvable.
    NamedEnum[SampleNamed] should be theSameInstanceAs SampleNamed.namedEnum
    NamedEnum[MockEnum] should be theSameInstanceAs MockEnum.namedEnum
    assertDoesNotCompile("""NamedEnum[SingleValueNamed]""")
    assertDoesNotCompile("""NamedEnum[CollidingNamed]""")
  }

  //-------------------------------------------------------------------------
  // helpers
  //-------------------------------------------------------------------------

  /**
   * Returns the failures of the specified outcome, in the order it holds them.
   *
   * The matchers cover the assertions this spec makes about an outcome; this is for the few
   * cases that assert on a failure as a value, which is what makes the absence of an
   * exception type visible.
   *
   * @param result  the outcome to inspect
   * @tparam A  the type of the value the outcome would have carried
   * @return the failures of the outcome, empty when it succeeded
   */
  private def failuresOf[A](result: ResultNec[A]): List[Failure] =
    result.fold(chain => chain.toChain.toList, _ => List.empty)

  /**
   * Returns a lenient rewrite as the text of its expression and its replacement.
   *
   * A compiled expression has no equality of its own, so a table of rewrites is compared
   * through the text it was compiled from. That text is also what makes an assertion over a
   * table readable, since it is exactly what the original wrote in its configuration.
   *
   * @param rewrite  the rewrite to describe
   * @return the source text of the expression, paired with its replacement
   */
  private def sourceOf(rewrite: (Regex, String)): (String, String) =
    rewrite._1.pattern.pattern() -> rewrite._2
}

/**
 * The sample families the spec above resolves names against.
 *
 * Each family here replaces one fixture of the port's source, where a family was a Java
 * interface whose members were contributed by provider classes named in a configuration
 * file, together with the three tables that file declared. A family is now a sealed type
 * whose members are the values of its companion, and the tables are Scala values handed to
 * the typeclass alongside them, so a family's whole name space is visible in one place and
 * is fixed when this file is compiled.
 *
 * ===What is transcribed, and from where===
 *
 * [[NamedEnumFixtures.SampleNamed]] carries the five members and the three tables of the
 * fixture the original resolved most of its cases against, transcribed row for row: the one
 * alternate spelling, the two groups of external spellings with their three rows, and the
 * three lenient rewrites '''in the order the original declared them''', which is behaviour
 * rather than presentation and is asserted as such.
 *
 * Beside it sit the narrower families each remaining case needs, and a series of further
 * lookups over the same five members which vary one table at a time - a table whose alternate
 * spelling and whose rewrite both claim one text, a table supplying both a spelling and the
 * upper-case form of that spelling with a different member behind each, a table whose
 * alternate spelling points at another alternate spelling, a table with no label, a group
 * naming a member that does not exist, a group naming a member through its alternate
 * spelling, the three rewrites of the fixture in an order that breaks the chain they form,
 * two rewrites spelled so that the fold of the input and the case-insensitive copy of a
 * source are each observable on their own, three rewrites whose shape decides what a rule
 * requires of the last character of the text, and the whole of the fixture declared through
 * the sources of its rewrites rather than through compiled expressions. Varying one table at
 * a time is what lets each test name the single rule it covers.
 *
 * ===Why a lookup is published implicitly here and not there===
 *
 * [[NamedEnumFixtures.SampleNamed]] and [[NamedEnumFixtures.MockEnum]] publish their lookup
 * and their instances implicitly, as every family of the library does, so that the spec
 * summons them the way production code does. [[NamedEnumFixtures.SingleValueNamed]] and
 * [[NamedEnumFixtures.CollidingNamed]] deliberately publish neither: they hold their lookup
 * as a plain value, which is what makes it possible to assert that the typeclass itself
 * offers no instance for a family that publishes none.
 * [[NamedEnumFixtures.UnorderedNamed]] is the third arrangement a family can be in: it
 * publishes one instance and no lookup, that instance being the hashing a family without a
 * meaningful order declares in place of the combined ordering.
 */
private[collect] object NamedEnumFixtures {

  /**
   * The family the ported fixture configured, with five members and all three tables.
   *
   * The members of the original arrived from five providers of three different kinds; here
   * they are the five values of the companion, in the order those providers contributed
   * them.
   *
   * @param name  the canonical name of the member
   */
  sealed abstract class SampleNamed private (val name: String) extends Named

  /** The members and the tables of the sample family. */
  object SampleNamed {

    /** The member a constants provider of the original contributed first. */
    case object STANDARD extends SampleNamed("Standard")

    /** The member a second constants provider contributed. */
    case object MORE extends SampleNamed("More")

    /** The member a lookup-function provider contributed. */
    case object OTHER extends SampleNamed("Other")

    /** The member the first instance provider contributed. */
    case object ANOTHER1 extends SampleNamed("Another1")

    /** The member the second instance provider contributed. */
    case object ANOTHER2 extends SampleNamed("Another2")

    /** The members, in the order the providers of the ported fixture contributed them. */
    val values: NonEmptyList[SampleNamed] =
      NonEmptyList.of(STANDARD, MORE, OTHER, ANOTHER1, ANOTHER2)

    /** The one alternate spelling the ported fixture declared. */
    val Alternates: Map[String, String] = Map("Alternate" -> "Standard")

    /**
     * The three lenient rewrites the ported fixture declared, in its order, as text.
     *
     * The order is load-bearing: the first rewrite turns `A1` into `B1`, which the second
     * turns into `Standard`, and the exact lookup runs only once the whole table has been
     * applied. Reordering these three changes what the family resolves, which
     * [[NamedEnumFixtures.brokenChain]] exists to demonstrate.
     *
     * This is the one place the three rows are written out. The typeclass accepts a table
     * either as sources or as compiled expressions, and both forms of this fixture are
     * derived from these rows, so the two cannot drift apart.
     */
    val LenientSources: List[(String, String)] =
      List("A([1-2])" -> "B$1", "B1" -> "Standard", "B2" -> "More")

    /** The three lenient rewrites as compiled expressions, for a family declaring them so. */
    val LenientPatterns: List[(Regex, String)] =
      LenientSources.map { case (source, replacement) => source.r -> replacement }

    /** The two groups of external spellings the ported fixture declared. */
    val Externals: Map[String, Map[String, String]] =
      Map(
        "Foo" -> Map("Foo1" -> "Standard"),
        "Bar" -> Map("Foo1" -> "More", "Foo2" -> "Standard"))

    /** The lookup of this family, published as every family of the library publishes it. */
    implicit val namedEnum: NamedEnum[SampleNamed] =
      NamedEnum.of(values, Alternates, LenientPatterns, Externals, "SampleNamed")

    /** The ordering, hashing and equality of this family, all derived from its names. */
    implicit val order: Order[SampleNamed] with Hash[SampleNamed] = NamedEnum.orderByName

    /** The rendering of a member as its name. */
    implicit val show: Show[SampleNamed] = NamedEnum.showByName
  }

  /**
   * The rewrites of the sample family in an order that breaks the chain they form.
   *
   * The same three rewrites are declared, with the one that produces the intermediate form
   * moved to the end, so that nothing runs after it. This is the control that keeps the
   * sequencing assertions honest: a lookup that applied its table in one pass, or in any
   * order but the declared one, would resolve what this family must not.
   */
  val brokenChain: NamedEnum[SampleNamed] =
    NamedEnum.of(
      SampleNamed.values,
      SampleNamed.Alternates,
      List("B1".r -> "Standard", "B2".r -> "More", "A([1-2])".r -> "B$1"),
      SampleNamed.Externals,
      "BrokenChain")

  /**
   * The lookup of the sample family built from the sources of its rewrites.
   *
   * The same five members and the same three tables as [[NamedEnumFixtures.SampleNamed]]
   * publishes, declared through `ofSources` rather than through `of`: the rewrites arrive as
   * the text of each expression, which is the form the families of the library use and which
   * leaves one compiled expression per rule in the program. Everything the family resolves is
   * expected to be identical, and is asserted to be.
   */
  val fromSources: NamedEnum[SampleNamed] =
    NamedEnum.ofSources(
      SampleNamed.values,
      SampleNamed.Alternates,
      SampleNamed.LenientSources,
      SampleNamed.Externals,
      "FromSources")

  /**
   * A lookup whose alternate spelling and whose rewrite both claim the text `A1`.
   *
   * Through the rewrites alone that text resolves to `Standard`; through the alternate
   * table it resolves to `Another1`. The alias substitution runs first, so the second is
   * what a lookup of this family answers.
   */
  val aliasBeatsRewrite: NamedEnum[SampleNamed] =
    NamedEnum.of(
      SampleNamed.values,
      Map("A1" -> "Another1"),
      SampleNamed.LenientPatterns,
      familyName = "AliasBeatsRewrite")

  /**
   * The source of a rewrite written in genuinely mixed case.
   *
   * The tables of the library carry sources spelled this way, and a source spelled this way
   * can only ever be reached because the family applies a copy of it that ignores case: the
   * text handed to the rewrites has been folded to upper case, which this source as written
   * does not match. Held as a value so that the family below and the assertions over it
   * cannot drift apart, and asserted verbatim so that folding it to upper case - which would
   * make every case of it pass whether the copy ignores case or not - fails a test.
   */
  val MixedCaseSource: String = "Mix/Mix"

  /**
   * The source of a rewrite that re-enables sensitivity to case within itself.
   *
   * This is the control for the fold rather than for the copy. The copy is made by prefixing
   * an inline flag to the source, and a source that turns that flag off again is beyond its
   * reach, so the only text this rewrite can match is the lower-case text it spells - which
   * the fold has already turned to upper case by the time any rewrite is applied. The
   * rewrite is therefore well formed and unreachable, and it becomes reachable the moment
   * the fold stops happening. It names text that the rewrite above cannot match, so the two
   * controls stay independent of each other.
   */
  val CaseSensitiveSource: String = "(?-i)sensitive/sensitive"

  /**
   * A lookup whose two rewrites separate the fold of the input from the copy of the source.
   *
   * [[NamedEnumFixtures.MixedCaseSource]] resolves only while the copy ignores case, and
   * [[NamedEnumFixtures.CaseSensitiveSource]] resolves nothing at all only while the input is
   * folded. Between them the two rules pin both halves of the lenient stage, neither of which
   * the rewrites of the sample family can distinguish: every one of those is spelled in upper
   * case already.
   */
  val caseFolding: NamedEnum[SampleNamed] =
    NamedEnum.of(
      SampleNamed.values,
      lenient = List(MixedCaseSource.r -> "Other", CaseSensitiveSource.r -> "More"),
      familyName = "CaseFolding")

  /**
   * A lookup supplying both a spelling and its upper-case form, each naming a member of its
   * own.
   *
   * The expansion of the alternate table adds the upper-case form of every supplied spelling,
   * so this is the table where it has something to displace: `alias` folds to `ALIAS`, which
   * the family supplied itself and pointed at a different member. A supplied spelling is
   * authoritative and the folded ones only fill the gaps, so both rows survive and each is
   * reached by the spelling it was supplied under. Nothing but a table of this shape can tell
   * the two orders apart.
   */
  val aliasCaseConflict: NamedEnum[SampleNamed] =
    NamedEnum.of(
      SampleNamed.values,
      Map("alias" -> "Standard", "ALIAS" -> "More"),
      familyName = "AliasCaseConflict")

  /** A lookup whose alternate spelling points at another alternate spelling. */
  val chainedAlias: NamedEnum[SampleNamed] =
    NamedEnum.of(
      SampleNamed.values,
      Map("First" -> "Second", "Second" -> "Standard"),
      familyName = "ChainedAlias")

  /** A lookup that supplies no label, and so is labelled generically. */
  val unlabelled: NamedEnum[SampleNamed] = NamedEnum.of(SampleNamed.values)

  /** A lookup one of whose external rows names a member that does not exist. */
  val danglingExternal: NamedEnum[SampleNamed] =
    NamedEnum.of(
      SampleNamed.values,
      externals = Map("Foo" -> Map("Foo1" -> "Standard", "Gone" -> "NoSuchMember")),
      familyName = "DanglingExternal")

  /** A lookup whose external row names its member through an alternate spelling. */
  val aliasedExternal: NamedEnum[SampleNamed] =
    NamedEnum.of(
      SampleNamed.values,
      SampleNamed.Alternates,
      externals = Map("Foo" -> Map("Foo1" -> "Alternate")),
      familyName = "AliasedExternal")

  /** An alternate spelling far longer than any name of the family it resolves in. */
  val LongAliasSpelling: String = "AnAlternateSpellingLongerThanEveryKeyOfThisFamily"

  /**
   * A lookup reached through an alternate spelling longer than every key of its family.
   *
   * Neither stage counts the characters of the text it is handed, so this spelling resolves
   * exactly and its folded forms resolve through the fold the lenient stage performs.
   */
  val longAlias: NamedEnum[SampleNamed] =
    NamedEnum.of(
      SampleNamed.values,
      Map(LongAliasSpelling -> "More"),
      familyName = "LongAlias")

  /**
   * A lookup whose single rewrite consumes text of any length before a literal.
   *
   * A greedy group matches text of whatever length it is given, so this is the shape of
   * expression whose cost of matching grows with the text rather than with the family, and
   * the shape the requirement a rule derives from its own source exists for: the source ends
   * in the plain literal `X`, so every full match of it ends in `X` and text ending in
   * anything else is declined rather than matched. The source carries no anchor - a rewrite
   * is applied through a match of the whole of the text, which makes an anchor redundant -
   * so its last character is the literal the requirement is read from.
   */
  val greedyRewrite: NamedEnum[SampleNamed] =
    NamedEnum.of(
      SampleNamed.values,
      lenient = List("(.*)X".r -> "Standard"),
      familyName = "GreedyRewrite")

  /**
   * The one expression shape of the transcribed tables that can consume unbounded text.
   *
   * Transcribed from the day-count table of the library, where it accepts a name followed by
   * a parenthesised qualifier. Held as a value so that the family below and the assertions
   * over it cannot drift apart: this is the expression the timing guard measures, and its
   * closing bracket - a class holding exactly one character - is the shape the requirement on
   * the last character of the text is read from when the expression ends in a class rather
   * than in a plain literal.
   */
  val BracketSource: String = "(.*)[(](.*)[)]"

  /**
   * The number of characters of hostile text the timing guard hands to that expression.
   *
   * Two hundred thousand opening brackets is text crafted for the expression above: every
   * position of it is a position the first group could end at, so applying the expression to
   * it costs minutes, while declining it on the last character of the text costs
   * milliseconds. The same number is used for the text that every rewrite of a family does
   * match, so that the two are the same size and only the expressions differ.
   */
  val HostileInputLength: Int = 200000

  /**
   * A rewrite whose text ends in something other than what it spells.
   *
   * Under the inline flag that turns comments on, whitespace is ignored and everything after a
   * `#` is a comment, so this expression matches `A` alone and the `# X` closing its source is
   * no part of the language it accepts. It is the counter-example to reading the last character
   * of a source as a character the text must end with, which is why the reading declines any
   * source carrying an inline construct.
   */
  val CommentedSource: String = "(?x)A # X"

  /**
   * A lookup declaring that rewrite, so the reading of its source can be asserted.
   */
  val commentedRewrite: NamedEnum[SampleNamed] =
    NamedEnum.ofSources(
      SampleNamed.values,
      lenient = List(CommentedSource -> "Standard"),
      familyName = "CommentedRewrite")

  /**
   * The same rewrite handed over already compiled, to assert both constructors alike.
   */
  val commentedRewriteFromPatterns: NamedEnum[SampleNamed] =
    NamedEnum.of(
      SampleNamed.values,
      lenient = List(CommentedSource.r -> "Standard"),
      familyName = "CommentedRewriteCompiled")

  /**
   * A lookup declaring the bracketed rewrite, built from the source of that rewrite.
   *
   * Declared through `ofSources`, which is the form a family of the library uses: a rule
   * handed over as text is compiled exactly once, where a rule handed over already compiled
   * is compiled a second time to be applied without regard to case.
   */
  val bracketRewrite: NamedEnum[SampleNamed] =
    NamedEnum.ofSources(
      SampleNamed.values,
      lenient = List(BracketSource -> "Standard"),
      familyName = "BracketRewrite")

  /**
   * A lookup whose single rewrite turns every text there is into the name of a member.
   *
   * With this family a resolution is proof that the chain of rewrites ran over the text, which
   * is how the tests above assert that the chain is applied to text of every length -
   * cheaply and deterministically, rather than by timing a call.
   */
  val rewritesAnything: NamedEnum[SampleNamed] =
    NamedEnum.of(
      SampleNamed.values,
      lenient = List("^.*$".r -> "Standard"),
      familyName = "RewritesAnything")

  /**
   * A lookup whose expression source is longer than every key of its family.
   *
   * The length of a source decides nothing about the text the family accepts: this source
   * admits a name followed by any number of underscores, and any number is what the family
   * accepts, the source being longer than every key of it either way.
   */
  val longSourceRewrite: NamedEnum[SampleNamed] =
    NamedEnum.of(
      SampleNamed.values,
      lenient = List("^MORE[_]*$".r -> "More"),
      familyName = "LongSourceRewrite")

  /**
   * A family whose single member is named at far greater length than any other fixture here.
   *
   * The leniency of a family that declares no rewrite is the fold of its input to upper case,
   * and that fold is applied to text of every length, so the lower-case spelling of this name
   * resolves through it. The family is also the second subject of the pair of measurements
   * showing that the length of a family's own names decides nothing about the text its
   * rewrites accept.
   *
   * @param name  the canonical name of the member
   */
  sealed abstract class LongNameNamed private (val name: String) extends Named

  /** The single long-named member, and the lookup over it. */
  object LongNameNamed {

    /** The member whose name is written out at far greater length than any other here. */
    case object SPELLED_OUT
        extends LongNameNamed("AVeryLongCanonicalNameOfAMemberOfAFamilyDeclaringNoRewrite")

    /** The lookup of the long-named family, declaring no table at all. */
    val lookup: NamedEnum[LongNameNamed] =
      NamedEnum.of(NonEmptyList.one(SPELLED_OUT), familyName = "LongNameNamed")
  }

  /**
   * The rewrite of [[NamedEnumFixtures.rewritesAnything]] over the long-named family.
   *
   * The same expression over a family whose longest key is far longer accepts the same text,
   * which is the pair of measurements that shows the lenient stage to depend on the rules a
   * family declares and not on the length of the names it holds.
   */
  val longNameRewritesAnything: NamedEnum[LongNameNamed] =
    NamedEnum.of(
      NonEmptyList.one(LongNameNamed.SPELLED_OUT),
      lenient = List("^.*$".r -> LongNameNamed.SPELLED_OUT.name),
      familyName = "LongNameRewritesAnything")

  /**
   * A family whose name space is wider than the members its lookup holds.
   *
   * A family of the library can layer a second provider over its closed members - a
   * convention parameterised by a calendar, say - which leaves it with values that `values`
   * does not hold and with external rows naming them. The resolution such a family supplies
   * when it builds its lookup is the only thing that can reach those rows; without it a row
   * naming one of them is dropped exactly as a row naming nothing is. This family is the
   * smallest shape that has both: one closed member, one value outside the member list, and a
   * group naming each.
   *
   * @param name  the canonical name of the value
   */
  sealed abstract class WiderNamed private (val name: String) extends Named

  /** The closed member, the value beyond it, and the two lookups over them. */
  object WiderNamed {

    /** The closed member, which the exact lookup of the family resolves. */
    case object INSIDE extends WiderNamed("Inside")

    /** The value outside the member list, reachable only through the wider resolution. */
    case object OUTSIDE extends WiderNamed("Outside")

    /** The members of the family, deliberately excluding the value above. */
    val values: NonEmptyList[WiderNamed] = NonEmptyList.one(INSIDE)

    /** A group naming the member and the value outside the member list. */
    val Externals: Map[String, Map[String, String]] =
      Map("Foo" -> Map("In" -> "Inside", "Out" -> "Outside"))

    /** The lookup a family with no wider resolution gets, which drops the second row. */
    val withoutResolution: NamedEnum[WiderNamed] =
      NamedEnum.ofSources(values, externals = Externals, familyName = "WiderNamedClosed")

    /**
     * The lookup the family builds, resolving external rows through its whole name space.
     *
     * The resolution supplied here is the family's own: the value beyond the member list
     * first, and the exact lookup of the closed members for everything else.
     */
    val lookup: NamedEnum[WiderNamed] =
      NamedEnum.ofSources(
        values,
        externals = Externals,
        familyName = "WiderNamed",
        externalTargets = Some((canonicalName: String) =>
          if (canonicalName == OUTSIDE.name) Some(OUTSIDE)
          else withoutResolution.valueOf(canonicalName)))
  }

  /**
   * The smallest family there can be, standing in for the ported empty family.
   *
   * The original had a family whose configuration declared no provider, so it resolved
   * nothing at all. A family's members now arrive as a non-empty list, which makes that
   * state unrepresentable, so the cases that covered it are carried by a family with one
   * member and no tables. Its lookup is deliberately not published implicitly.
   *
   * @param name  the canonical name of the member
   */
  sealed abstract class SingleValueNamed private (val name: String) extends Named

  /** The single member of the smallest family, and its lookup. */
  object SingleValueNamed {

    /** The only member of the family. */
    case object SOLO extends SingleValueNamed("Solo")

    /** The lookup of the family, labelled as the ported empty family was named. */
    val lookup: NamedEnum[SingleValueNamed] =
      NamedEnum.of(NonEmptyList.one(SOLO), familyName = "SampleOther")
  }

  /**
   * The first family of the union, which shares a name with the second.
   *
   * @param name  the canonical name of the member
   */
  sealed abstract class FirstNamed private (val name: String) extends Named

  /** The members of the first family of the union, and its lookup. */
  object FirstNamed {

    /** The member the union resolves from this family. */
    case object STANDARD extends FirstNamed("Standard")

    /** The member whose name the second family of the union also has. */
    case object SHARED extends FirstNamed("Shared")

    /** The lookup of the first family of the union. */
    val lookup: NamedEnum[FirstNamed] =
      NamedEnum.of(NonEmptyList.of(STANDARD, SHARED), familyName = "FirstNamed")
  }

  /**
   * The second family of the union, which shares a name with the first.
   *
   * @param name  the canonical name of the member
   */
  sealed abstract class SecondNamed private (val name: String) extends Named

  /** The members of the second family of the union, and its lookup. */
  object SecondNamed {

    /** The member the union resolves from this family. */
    case object MORE extends SecondNamed("More")

    /** The member whose name the first family of the union also has. */
    case object SHARED extends SecondNamed("Shared")

    /** The lookup of the second family of the union. */
    val lookup: NamedEnum[SecondNamed] =
      NamedEnum.of(NonEmptyList.of(MORE, SHARED), familyName = "SecondNamed")
  }

  /**
   * The union of the two families above, replacing the ported combined registry.
   *
   * The original assembled a second registry over the families named in a configuration
   * file, with a type and a rendering of its own. A union is now the function below: it
   * tries the exact lookup of each family in the order this code names them and answers the
   * first member it finds, reporting the text when no family has it. The order is therefore
   * written down rather than configured, which is the whole of what the ported type did.
   */
  object UberNamed {

    /**
     * Resolves a name against the two families, in declaration order.
     *
     * @param name  the name to resolve
     * @return the member the name identifies, or the failure describing why it identifies
     *   none
     */
    def parse(name: String): ResultNec[Named] =
      FirstNamed.lookup
        .valueOf(name)
        .orElse[Named](SecondNamed.lookup.valueOf(name))
        .toRight(NonEmptyChain.one(Failure.Parsing(s"UberNamed name not found: $name")))
  }

  /**
   * The family standing in for the plain enumeration of the ported formatter.
   *
   * The original had a formatter that derived the name of an enum constant by capitalizing
   * its first letter and lower-casing the rest of each word, and resolved text back to a
   * constant while ignoring case and underscores. There is no such formatter here, so the
   * four constants carry their formatted names directly and the tolerance the formatter
   * provided is declared as the tables of the family: an alternate spelling for the parse
   * alias the original added by hand, and two rewrites that remove the underscores of a
   * two-word and a three-word name. The member identifiers are those of the ported
   * constants, including the one written in mixed case.
   *
   * @param name  the canonical name of the member, as the ported formatter rendered it
   */
  sealed abstract class MockEnum private (val name: String) extends Named

  /** The members and the tables of the family standing in for the plain enumeration. */
  object MockEnum {

    /** The single-word constant. */
    case object ONE extends MockEnum("One")

    /** The two-word constant. */
    case object TWENTY_ONE extends MockEnum("TwentyOne")

    /** The constant the original wrote in mixed case, which formats to one word. */
    case object FooBar extends MockEnum("Foobar")

    /** The three-word constant. */
    case object WOO_BAR_WAA extends MockEnum("WooBarWaa")

    /** The members, in the order the ported enumeration declared its constants. */
    val values: NonEmptyList[MockEnum] = NonEmptyList.of(ONE, TWENTY_ONE, FooBar, WOO_BAR_WAA)

    /** The parse alias the ported test added to the formatter by hand. */
    val Alternates: Map[String, String] = Map("Fb" -> "Foobar")

    /**
     * The rewrites that give the underscore tolerance of the ported formatter.
     *
     * The three-segment rewrite is declared first so that a three-word name is joined in
     * one step; the two-segment rewrite then leaves the result alone, having no underscore
     * to match. Text the lenient stage has folded to upper case is what these see.
     */
    val LenientPatterns: List[(Regex, String)] =
      List(
        "([A-Z0-9]+)_([A-Z0-9]+)_([A-Z0-9]+)".r -> "$1$2$3",
        "([A-Z0-9]+)_([A-Z0-9]+)".r -> "$1$2")

    /** The lookup of this family. */
    implicit val namedEnum: NamedEnum[MockEnum] =
      NamedEnum.of(values, Alternates, LenientPatterns, familyName = "MockEnum")

    /** The rendering of a member as its name, which is what the formatter produced. */
    implicit val show: Show[MockEnum] = NamedEnum.showByName
  }

  /**
   * A family that carries no order, publishing the hashing of its names and nothing else.
   *
   * Every other family here publishes the combined ordering, so the hashing offered on its
   * own - the instance a family whose members have no meaningful order declares - would
   * otherwise be published by nobody and exercised by nothing. This family publishes exactly
   * that one instance: no ordering, no rendering, and an equality that arrives only because
   * hashing extends it.
   *
   * @param name  the canonical name of the member
   */
  sealed abstract class UnorderedNamed private (val name: String) extends Named

  /** The members of the unordered family, and the single instance it publishes. */
  object UnorderedNamed {

    /** The first member, whose name a second value of the family also carries. */
    case object FIRST extends UnorderedNamed("First")

    /** The second member, which shares its name with nothing. */
    case object SECOND extends UnorderedNamed("Second")

    /**
     * A second value carrying the name of [[UnorderedNamed.FIRST]].
     *
     * The name-derived instances of a family are indistinguishable from the universal
     * equality and hashing of its members while every name belongs to exactly one value:
     * two members are unequal and hash apart under either reading. This value exists so
     * that they are distinguishable - it is not the first member, so universal equality
     * separates the two and their identities hash apart, while their names are the same
     * word, so a name-derived instance must hold them equal and hash them alike.
     *
     * It is a value of the family rather than a member of it, so it is deliberately absent
     * from `values` and from any lookup: a family whose lookup held two values of one name
     * would be the collision case that [[NamedEnumFixtures.CollidingNamed]] covers, which is
     * a different subject.
     */
    val FirstTwin: UnorderedNamed = new UnorderedNamed("First") {}

    /** The members of the family, in declaration order. */
    val values: NonEmptyList[UnorderedNamed] = NonEmptyList.of(FIRST, SECOND)

    /** The hashing of this family, and so its equality, derived from its names. */
    implicit val hash: Hash[UnorderedNamed] = NamedEnum.hashByName
  }

  /**
   * A family two of whose members fold to one lookup key.
   *
   * The second member's name is the upper-case form of the first member's, so the folded
   * spelling the first member offers is the canonical name of the second. A canonical name is
   * claimed unconditionally and a folded spelling only fills a slot still free, so each
   * member keeps the name it publishes and the folded key belongs to the member whose
   * canonical name it is. This is the one way a key view of a family is narrower than its
   * member list - the folded view has one entry fewer - and it is where the single-key member
   * of the ported fixture went. Its lookup is deliberately not published implicitly.
   *
   * @param name  the canonical name of the member
   */
  sealed abstract class CollidingNamed private (val name: String) extends Named

  /** The colliding members, in the order they claim their keys. */
  object CollidingNamed {

    /** The earlier member, which claims `Dup` and offers `DUP` as its folded spelling. */
    case object MIXED extends CollidingNamed("Dup")

    /** The later member, whose canonical name is the folded spelling of the earlier one. */
    case object UPPER extends CollidingNamed("DUP")

    /** A member that collides with nothing, to show the rest of the family is unaffected. */
    case object PLAIN extends CollidingNamed("Plain")

    /** The lookup of the colliding family. */
    val lookup: NamedEnum[CollidingNamed] =
      NamedEnum.of(NonEmptyList.of(MIXED, UPPER, PLAIN), familyName = "CollidingNamed")
  }
}
