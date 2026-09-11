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

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

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
 * removes, and the algorithm it keeps is the one the constants provider performed: every
 * member is registered under both of its keys. The asymmetry is consequently unreachable -
 * a key can only be missing from the second view by having been claimed by an earlier
 * member, which resolves it to that earlier member rather than to nothing - so this spec
 * asserts the uniform registration that replaced it, and covers the two ways a key view can
 * still be narrower than the member list: a member whose canonical name is already upper
 * case offers one key rather than two, and a member whose keys were both claimed by an
 * earlier member is absent from the canonical view altogether. Both are asserted below, the
 * first against a real family of this module.
 *
 * ===Traceability===
 *
 * Each test names the method of the original test class whose cases it carries, so that the
 * mapping from the original suite to this one can be read off the test names. The original
 * packed many assertions into few methods; this spec splits them so that a regression names
 * the behaviour it broke.
 */
final class NamedEnumSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

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
    List[NamedEnum[_ <: Named]](sample, mock, SingleValueNamed.lookup, FirstNamed.lookup, SecondNamed.lookup)
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

  test("when two members claim one key the earlier member keeps it") {
    // The merge of the original kept the first registration of a key, and so does this
    // port. A member declared after one whose upper-case name it shares therefore holds no
    // key at all: the key resolves to the earlier member - never to nothing - and the later
    // member is absent from the canonical view while remaining in the member list. This is
    // the only way a key view is narrower than the member list, and it is the case that
    // stands where the fixture of the original had a member registered under one key.
    CollidingNamed.lookup.values.toList shouldBe
      List(CollidingNamed.MIXED, CollidingNamed.UPPER, CollidingNamed.PLAIN)
    CollidingNamed.lookup.valueOf("Dup") shouldBe Some(CollidingNamed.MIXED)
    CollidingNamed.lookup.valueOf("DUP") shouldBe Some(CollidingNamed.MIXED)
    CollidingNamed.lookup.byCanonicalName shouldBe
      Map("Dup" -> CollidingNamed.MIXED, "Plain" -> CollidingNamed.PLAIN)
    CollidingNamed.lookup.byUpperName shouldBe
      Map("DUP" -> CollidingNamed.MIXED, "PLAIN" -> CollidingNamed.PLAIN)
    CollidingNamed.lookup.byCanonicalName.values.toList.contains(CollidingNamed.UPPER) shouldBe false
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
 * Beside it sit the narrower families each remaining case needs, and seven further lookups
 * over the same five members which vary one table at a time - a table whose alternate
 * spelling and whose rewrite both claim one text, a table whose alternate spelling points
 * at another alternate spelling, a table with no label, a group naming a member that does
 * not exist, a group naming a member through its alternate spelling, the three rewrites of
 * the fixture in an order that breaks the chain they form, and two rewrites spelled so that
 * the fold of the input and the case-insensitive copy of a source are each observable on
 * their own. Varying one table at a time is what lets each test name the single rule it
 * covers.
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
     * The three lenient rewrites the ported fixture declared, in its order.
     *
     * The order is load-bearing: the first rewrite turns `A1` into `B1`, which the second
     * turns into `Standard`, and the exact lookup runs only once the whole table has been
     * applied. Reordering these three changes what the family resolves, which
     * [[NamedEnumFixtures.brokenChain]] exists to demonstrate.
     */
    val LenientPatterns: List[(Regex, String)] =
      List("A([1-2])".r -> "B$1", "B1".r -> "Standard", "B2".r -> "More")

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
   * A family two of whose members claim one lookup key.
   *
   * The second member's name is the upper-case form of the first member's, so the two offer
   * the same second key and, for the second member, the same first key as well. The merge
   * keeps the first registration of a key, which leaves the later member holding none. This
   * is the one way a key view of a family is narrower than its member list, and it is where
   * the single-key member of the ported fixture went. Its lookup is deliberately not
   * published implicitly.
   *
   * @param name  the canonical name of the member
   */
  sealed abstract class CollidingNamed private (val name: String) extends Named

  /** The colliding members, in the order that decides which of them keeps the key. */
  object CollidingNamed {

    /** The earlier member, which claims both `Dup` and `DUP`. */
    case object MIXED extends CollidingNamed("Dup")

    /** The later member, whose only offered key is already claimed. */
    case object UPPER extends CollidingNamed("DUP")

    /** A member that collides with nothing, to show the rest of the family is unaffected. */
    case object PLAIN extends CollidingNamed("Plain")

    /** The lookup of the colliding family. */
    val lookup: NamedEnum[CollidingNamed] =
      NamedEnum.of(NonEmptyList.of(MIXED, UPPER, PLAIN), familyName = "CollidingNamed")
  }
}
