/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import scala.collection.immutable.List

import cats.Eq
import cats.Hash
import cats.Order
import cats.Show
import cats.syntax.show._

import _root_.io.circe.DecodingFailure
import _root_.io.circe.Json
import _root_.io.circe.parser.decode
import _root_.io.circe.syntax._

import org.scalacheck.Shrink
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import com.opengamma.strata.collect.Arbitraries._
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Tests the support a ''typed string'' is built from: [[TypedStringCompanion]] and the three
 * validations its nested object supplies.
 *
 * No type of this port is a typed string yet - the abstraction is carried forward so that a
 * later slice of the migration inherits a tested contract rather than an untested one - so
 * this spec is the only thing that exercises it and therefore covers the whole of its surface
 * rather than a sample of it. The three fixtures in [[TypedStringFixtures]] stand in for the
 * concrete types a later slice will write, and between them they use each of the three
 * validations the support offers.
 *
 * ===What the port inverted, and why it shows up here===
 *
 * The type being ported was an abstract base class: it held the text, validated it in three
 * constructors, and implemented comparison, equality, hashing and rendering once for every
 * subclass. A Scala value class cannot extend a class, and can mix in only a trait that itself
 * extends `Any`; giving up the value class would cost an allocation for every wrapper - the
 * expense the abstraction exists to avoid - so the inheritance runs the other way here. The
 * values carry only their text:
 *
 * {{{
 * final class SampleType private (val name: String) extends AnyVal with Named
 *
 * object SampleType extends TypedStringCompanion[SampleType](validation, new SampleType(_))
 * }}}
 *
 * and everything the base class used to provide is inherited by the ''companion'' instead.
 * Three observable consequences follow, and each is asserted below rather than assumed: the
 * class synthesises no `apply` and no `copy`, its constructor is reachable only from its
 * companion, and `of` is consequently the only way a value of the type can come into being.
 *
 * ===The two cases whose subject the port removed===
 *
 * Two of the eight cases of the original suite tested features this port does not carry, so
 * each is redirected to the contract that replaced it, under the original name so that the
 * mapping from the original suite stays readable:
 *
 *   - `test_serialization` asserted platform serialization, which no type of this port
 *     supports. It now asserts the JSON round trip, which is how a typed string travels.
 *   - `test_jodaConvert` asserted the reflective text-form conversion of the library that has
 *     been dropped. It now asserts that the three text forms a typed string has - its
 *     rendering, its `Show` and its JSON - are the same text, which is the property that
 *     conversion was there to provide.
 *
 * The remaining six cases keep both their names and their assertions, except where the port's
 * behaviour genuinely differs; each such difference is stated in the test that pins it.
 *
 * ===What the generated-text properties add to the fixed tables===
 *
 * The two redirected cases, and the fixed tables beside them, pin the behaviour at the texts
 * this file names. The two contracts they replace were reflective, and therefore unbounded in
 * the text they reached, so each of those assertions is made a second time over generated
 * text: the shared generators of [[Arbitraries]] supply the text a fixture accepts -
 * [[Arbitraries.genNonEmptyText]] for the plain fixture, [[Arbitraries.genUpperLetterText]]
 * for the two that constrain the shape of their text as well - and
 * [[Arbitraries.genNonUpperText]] supplies the text a shape check has to reject. The
 * properties at the foot of this file assert, over those generators, the three text forms,
 * the JSON round trip, equality and hashing, the laws of the ordering, the rejecting branch of
 * both validated fixtures and the rejecting branch of the decoder. Every one of them
 * exercises all three fixtures of [[TypedStringFixtures]], so a divergence between the three
 * validations cannot hide behind whichever of them the fixed tables happen to name.
 *
 * One consequence of generating the text has to be handled deliberately, and is handled where
 * the shrinking is declared below: every fixture rejects empty text, and the shrinking the
 * library supplies for text minimises towards the empty string, so a genuine counterexample
 * would be reported as one of the builders of this spec rejecting empty text rather than as
 * the behaviour that actually broke.
 *
 * ===Divergences from the type being ported, settled by measurement===
 *
 * These were observed on this toolchain rather than predicted, and they belong in the
 * migration note under the divergence list:
 *
 *   - '''The hash code no longer mixes the class in.''' The original hashed
 *     `name.hashCode ^ getClass.hashCode`, caching the result in a field. A value class has no
 *     field to cache in and takes its hash from the value it wraps, so the hash of a typed
 *     string ''is'' the hash of its text. Only the contract that matters is asserted here -
 *     equal values hash alike - together with the port's own definition of the instance.
 *   - '''Two typed strings wrapping the same text are unequal, yet share a hash code.''' The
 *     original was unequal because it compared run-time classes; the boxed form of a value
 *     class compares its wrapper class too, so equality still discriminates the type, exactly
 *     as before. What is lost is the class in the hash, so two such values now collide. This
 *     is narrower than the blanket statement in the divergence list of
 *     [[TypedStringCompanion]], which reads as though the values had become equal; the
 *     assertions below are the authority, and the note should follow them.
 *   - '''Rendering is a choice the concrete type makes.''' The original rendered as its name
 *     for every subclass, because `toString` was final on the base class. A value class
 *     written to the minimal documented pattern renders as the platform default instead -
 *     `Show` is what renders the name - and a concrete type that wants the original rendering
 *     adds `override def toString: String = name`, which is legal on a value class. The two
 *     fixtures that stand for the original's own fixtures add it, so their cases port
 *     unchanged; the third keeps the minimal pattern, so the difference is pinned rather than
 *     hidden.
 *   - '''Text the type rejects is reported, not raised.''' Every rejection was an
 *     `IllegalArgumentException` in the original; `of` returns the reasons on the left of an
 *     `EitherNec`, so nothing in this spec expects an exception from the API under test.
 *   - '''Absence is not a value.''' Two of the original's cases built a value from an absent
 *     argument and expected it to be rejected at run time. Scala represents absence as
 *     `Option`, an absent value cannot be passed where text is required, and the absent
 *     reference this port does not use appears nowhere in it; the two cases are therefore
 *     re-expressed as compile-time proofs, plus the handling of an absent text before
 *     construction.
 *
 * ===What the compile-time proofs can and cannot reach===
 *
 * Several properties of the pattern are asserted with `assertDoesNotCompile`, and two limits
 * of that macro were measured while writing them. Both are recorded here so that no proof in
 * this spec passes for a reason other than the one it claims, and so that a reader does not
 * add one that does:
 *
 *   - '''A violated type-parameter bound escapes the macro.''' A companion declared over a
 *     type that is not [[Named]] passes `assertDoesNotCompile` although the compiler rejects
 *     the very same code when it is written into a source file - the bound is checked when the
 *     symbol is completed rather than when the snippet is typechecked. No proof of the bound
 *     is written here; that it holds is visible in the positive direction instead, where a
 *     fixture is passed wherever a named value is required.
 *   - '''A value class cannot be declared in a snippet at all.''' The macro typechecks its
 *     snippet in the scope that encloses it, which is a method body, and a value class may be
 *     neither a local class nor a member of one. Every declaration of a value class therefore
 *     fails to compile whatever it contains - including a perfectly legal one - so a proof
 *     written that way would assert nothing about its subject. The two language rules that
 *     matter to this design were consequently verified by compiling the declarations for real
 *     rather than through the macro, and are recorded rather than asserted: a value class may
 *     declare neither `equals` nor `hashCode` (`redefinition of equals method. See SIP-15,
 *     criterion 5. is not allowed in value class`), which is why the class-mixing hash of the
 *     original cannot be reproduced even deliberately and why the divergence in
 *     `test_equalsHashCode` is structural rather than a choice; and `Named` has to extend `Any`
 *     for a value class to be able to mix it in at all. The observable consequences of the
 *     first rule are asserted, in `test_equalsHashCode` and in the case covering two typed
 *     strings that wrap the same text.
 */
final class TypedStringSpec
    extends AnyFunSuite
    with Matchers
    with ScalaCheckPropertyChecks
    with TableDrivenPropertyChecks {

  import TypedStringFixtures._

  /**
   * Texts that the emptiness check accepts, covering the shapes a typed string is asked to
   * carry: single and multiple characters, digits, whitespace, the canonical forms of this
   * library, punctuation, non-ASCII text and a long run.
   *
   * The table is fixed rather than generated so that a failure names the text that caused it
   * and re-runs identically.
   */
  private val acceptedTexts =
    Table(
      "text",
      "A",
      "AB",
      "abc",
      "Zz9",
      "1",
      " ",
      "   ",
      "a b",
      "Act/365F",
      "GBP-LIBOR-3M",
      "EUR/USD",
      "P3M",
      "GBLO+USNY",
      "scheme~value",
      "Z\u00fcrich",
      "x" * 64)

  /** Pairs of texts whose ordering the ordering of values has to agree with. */
  private val orderedPairs =
    Table(
      ("left", "right"),
      ("A", "B"),
      ("B", "A"),
      ("A", "A"),
      ("A", "AB"),
      ("AB", "A"),
      ("Z", "a"),
      ("a", "Z"),
      ("Act/360", "Act/365F"),
      (" ", "A"),
      ("1", "A"))

  /**
   * Whether text is built only from the letters `A` to `Z`, which is the shape two fixtures
   * require and the third is indifferent to.
   *
   * This is the predicate the two shape validations of [[TypedStringFixtures]] apply, written
   * out here rather than borrowed from them: the shrinking below has to agree with the
   * generators of [[Arbitraries]] about which side of that line a piece of text falls on, and
   * a predicate taken from the code under test would agree with a broken validation.
   *
   * @param text  the text to classify
   * @return true if every character is an upper-case letter
   */
  private def isUpperLetterText(text: String): Boolean =
    text.forall(character => character >= 'A' && character <= 'Z')

  /**
   * Minimises failing text without leaving the domain the text was generated from.
   *
   * Minimisation has to stay inside the validation domain of the generator that produced the
   * failing value, and the shrinking the library supplies for text leaves it in two ways. It
   * removes characters, so it reaches the empty string, which every fixture rejects. It also
   * shrinks characters individually - through the shrinking of `Char` - so upper-case text can
   * shrink to text holding a lower-case character, and text whose only offending character is
   * that one can shrink to upper-case text. Either departure makes a minimised counterexample
   * fail for the wrong reason: the builders below would report a fixture rejecting the
   * minimised text, or a case asserting a rejection would find the minimised text accepted,
   * and in both readings the behaviour that actually broke goes unnamed.
   *
   * The shrinking declared here keeps every candidate on the same side of both lines as the
   * text it came from: non-empty, so the emptiness check is never what fails, and upper-case
   * only exactly when its source was, so text drawn from [[Arbitraries.genUpperLetterText]]
   * minimises to text a shape validation still accepts and text drawn from
   * [[Arbitraries.genNonUpperText]] minimises to text a shape validation still rejects. It is
   * a filter over the library's candidates rather than a replacement for them, so it inherits
   * their order and their termination: filtering a well-founded sequence of candidates leaves
   * it well founded, and text of one character has no candidate at all, which is correct -
   * nothing smaller is in the domain.
   *
   * It is declared rather than derived because it has to outrank the one the library would
   * otherwise supply for text, and it is local to this spec because a generator of text is not
   * by itself a generator of text a typed string accepts. The two cases at the foot of this
   * file assert both invariants directly, so the guarantee is tested rather than argued.
   */
  private implicit val shrinkNonEmptyText: Shrink[String] =
    Shrink.withLazyList[String] { text =>
      val upperOnly = isUpperLetterText(text)
      LazyList
        .from(Shrink.shrinkString.shrink(text))
        .filter(candidate =>
          candidate.nonEmpty && isUpperLetterText(candidate) == upperOnly)
    }

  /** Builds a value of the unvalidated fixture, failing the test if its text is rejected. */
  private def sample(text: String): SampleType =
    SampleType.of(text).getOrElse(fail(s"the unvalidated fixture rejected <$text>"))

  /** Builds a value of the pattern-validated fixture, failing the test if it is rejected. */
  private def validated(text: String): SampleValidatedType =
    SampleValidatedType.of(text).getOrElse(fail(s"the validated fixture rejected <$text>"))

  /** Builds a value of the character-validated fixture, failing the test if it is rejected. */
  private def characters(text: String): SampleCharacterType =
    SampleCharacterType.of(text).getOrElse(fail(s"the character fixture rejected <$text>"))

  /** The failures an outcome holds, in order, for assertions on their exact content. */
  private def failuresOf[A](outcome: ResultNec[A]): List[Failure] =
    outcome.fold(failures => failures.toChain.toList, _ => List.empty)

  /** Reads the name of any named value, which is how a typed string is consumed as one. */
  private def nameOf(named: Named): String = named.name

  //-------------------------------------------------------------------------
  // the eight cases of the original suite, under their original names
  //-------------------------------------------------------------------------

  test("test_of") {
    // The original built a value and asserted its rendering. Both fixtures that stand for
    // the original's fixtures render as their name, so that assertion ports unchanged; the
    // name and the Show of the value are asserted alongside it, those being the forms the
    // port guarantees for every typed string.
    val built = SampleType.of("A")
    built should beSuccess
    built should haveValue(sample("A"))

    val value = sample("A")
    value.name shouldBe "A"
    value.toString shouldBe "A"
    value.show shouldBe "A"
  }

  test("test_of_invalid") {
    // The original asserted that empty text and an absent argument both threw. Empty text is
    // now reported: one failure, carrying the reason of an invalid argument and the wording of
    // the shared emptiness check, with no attributes.
    val empty = SampleType.of("")
    empty should beFailure
    empty should beFailureWith(FailureReason.INVALID)
    empty should haveFailureMessageMatching("Argument 'name' must not be empty")
    failuresOf(empty).map(failure => failure.message) shouldBe
      List("Argument 'name' must not be empty")
    failuresOf(empty).forall(failure => failure.attributes.isEmpty) shouldBe true

    // The absent-argument half of the original case has no counterpart to raise, because
    // absence is a value of another type here and the compiler keeps it out of the factory
    // altogether.
    assertDoesNotCompile("""SampleType.of(None)""")
    assertDoesNotCompile("""val absent: SampleType = None""")

    // What a caller with possibly-absent text does instead: decide the absent case before
    // construction, so that `of` is only ever handed text.
    Option.empty[String].map(text => SampleType.of(text)) shouldBe None
    Option("A").map(text => SampleType.of(text)).flatMap(outcome => outcome.toOption) shouldBe
      Some(sample("A"))
  }

  test("test_of_validated") {
    val built = SampleValidatedType.of("ABC")
    built should beSuccess
    built should haveValue(validated("ABC"))

    val value = validated("ABC")
    value.name shouldBe "ABC"
    value.toString shouldBe "ABC"
    value.show shouldBe "ABC"
  }

  test("test_of_validated_invalid") {
    // The original asserted that of("ABc") threw with the message its fixture supplied. The
    // message is carried verbatim by the reported failure.
    val rejected = SampleValidatedType.of("ABc")
    rejected should beFailure
    rejected should beFailureWith(FailureReason.INVALID)
    failuresOf(rejected).map(failure => failure.message) shouldBe List("Name must be letters")

    // Emptiness is checked before the shape, as it was in the constructors being ported, so
    // empty text is reported as empty rather than as the wrong shape.
    failuresOf(SampleValidatedType.of("")).map(failure => failure.message) shouldBe
      List("Argument 'name' must not be empty")

    // The absent-argument half of the original case, as in test_of_invalid.
    assertDoesNotCompile("""SampleValidatedType.of(None)""")
    assertDoesNotCompile("""val absent: SampleValidatedType = None""")
  }

  test("test_equalsHashCode") {
    val a1 = sample("A")
    val a2 = sample("A")
    val b = sample("B")

    // The five equality assertions of the original, in its order, written with `==` rather
    // than with a direct `equals` call: the compiler rejects the latter on a value class,
    // because `==` is the form that stays cooperative with the equality of the text
    // underneath. The fourth of the original's assertions - a comparison against an absent
    // value - is the one that cannot be written at all, and the compile-time proofs in
    // test_of_invalid stand in for it; the fifth compares against a value of another type,
    // which the original wrote as the empty string.
    (a1 == a1) shouldBe true
    (a1 == a2) shouldBe true
    (a1 == b) shouldBe false
    val anotherType: Any = ""
    (a1 == anotherType) shouldBe false
    val sameTextAsAnotherType: Any = "A"
    (a1 == sameTextAsAnotherType) shouldBe false
    a1.hashCode shouldBe a2.hashCode

    // Symmetry and the ordinary contract, which the original left implicit.
    (a2 == a1) shouldBe true
    (b == a1) shouldBe false
    Eq[SampleType].eqv(a1, a2) shouldBe true
    Eq[SampleType].eqv(a1, b) shouldBe false
    Hash[SampleType].hash(a1) shouldBe Hash[SampleType].hash(a2)

    // Divergence, measured: the hash of a value is the hash of its text, where the original
    // mixed the run-time class into it. The formula of the original is deliberately not
    // asserted; what is asserted is the invariant that matters and the port's own definition.
    Hash[SampleType].hash(a1) shouldBe "A".hashCode
    a1.hashCode shouldBe "A".hashCode
  }

  test("test_compareTo") {
    val a = sample("A")
    val b = sample("B")
    val c = sample("C")
    val values = List(a, b, c)

    // The original sorted a list in natural and in reverse order. The ordering comes from the
    // companion, and it is converted to the standard library's form for sorting.
    values.sorted(Order[SampleType].toOrdering).map(value => value.name) shouldBe
      List("A", "B", "C")
    values.sorted(Order[SampleType].toOrdering.reverse).map(value => value.name) shouldBe
      List("C", "B", "A")
    List(c, a, b).sorted(Order[SampleType].toOrdering).map(value => value.name) shouldBe
      List("A", "B", "C")

    // The comparison itself, which the original reached only through the sort.
    Order[SampleType].compare(a, b) should be < 0
    Order[SampleType].compare(b, a) should be > 0
    Order[SampleType].compare(a, sample("A")) shouldBe 0
    Order[SampleType].min(a, b) shouldBe a
    Order[SampleType].max(a, b) shouldBe b
  }

  test("test_serialization") {
    // Redirected: platform serialization is not carried by any type of this port, and a typed
    // string travels as JSON text instead. The round trip is asserted in both directions, for
    // a fixture of each of the three validations.
    val value = sample("A")
    value.asJson shouldBe Json.fromString("A")
    value.asJson.noSpaces shouldBe "\"A\""
    decode[SampleType](value.asJson.noSpaces) shouldBe Right(value)

    val validatedValue = validated("ABC")
    validatedValue.asJson shouldBe Json.fromString("ABC")
    decode[SampleValidatedType](validatedValue.asJson.noSpaces) shouldBe Right(validatedValue)

    val characterValue = characters("ABC")
    characterValue.asJson shouldBe Json.fromString("ABC")
    decode[SampleCharacterType](characterValue.asJson.noSpaces) shouldBe Right(characterValue)
  }

  test("test_jodaConvert") {
    // Redirected: the reflective text-form conversion of the dropped library is replaced by
    // the three text forms a typed string has, which have to carry the same text for the value
    // to survive being written out and read back.
    //
    // All three carry it exactly: the name a value answers with, the text it renders as and the
    // JSON string it encodes to are the text the value was built from, whatever that text
    // holds. A rendering adds nothing and takes nothing away, because the name is the identity
    // of the value - a caller that compares, re-parses or re-serializes what it rendered has to
    // receive its own text back - and making text safe for a line-oriented reader belongs to
    // the places that write a diagnostic, which a case further down states.
    val value = sample("A")
    val text = value.name
    value.toString shouldBe text
    Show[SampleType].show(value) shouldBe text
    value.show shouldBe text
    value.asJson shouldBe Json.fromString(text)
    decode[SampleType](Json.fromString(text).noSpaces) shouldBe Right(value)

    // And for the validated fixture, whose text the factory additionally constrains.
    val validatedValue = validated("ABC")
    validatedValue.toString shouldBe validatedValue.name
    validatedValue.show shouldBe validatedValue.name
    validatedValue.asJson shouldBe Json.fromString(validatedValue.name)
  }

  //-------------------------------------------------------------------------
  // the factory, and the three validations it is built from
  //-------------------------------------------------------------------------

  test("of accepts any text holding a character, including text that is only whitespace") {
    // The check behind the plain form is emptiness and not blankness, which is the check the
    // constructor being ported performed, so whitespace is text like any other.
    SampleType.of(" ") should beSuccess
    sample(" ").name shouldBe " "
    sample("   ").name shouldBe "   "
    sample("\t").name shouldBe "\t"
  }

  test("of reports one reason at a time for each of the three supplied validations") {
    // The outcome accumulates so that a validation combining independent checks can report
    // several reasons at once. None of the three supplied validations is of that kind - each
    // checks emptiness and then, only if that passed, the shape - so each reports exactly one.
    failuresOf(SampleType.of("")).size shouldBe 1
    failuresOf(SampleValidatedType.of("")).size shouldBe 1
    failuresOf(SampleValidatedType.of("ABc")).size shouldBe 1
    failuresOf(SampleCharacterType.of("")).size shouldBe 1
    failuresOf(SampleCharacterType.of("ABc")).size shouldBe 1
  }

  test("the pattern validation requires the whole text to match, so no anchors are written") {
    // The fixture's pattern describes upper-case letters and nothing else. Text that merely
    // starts or ends with them does not match, which is why a concrete type writes its
    // requirement without anchoring it.
    SampleValidatedType.of("ABC") should beSuccess
    SampleValidatedType.of("A") should beSuccess
    failuresOf(SampleValidatedType.of("ABC1")).map(failure => failure.message) shouldBe
      List("Name must be letters")
    failuresOf(SampleValidatedType.of("1ABC")).map(failure => failure.message) shouldBe
      List("Name must be letters")
    failuresOf(SampleValidatedType.of("AB C")).map(failure => failure.message) shouldBe
      List("Name must be letters")
    failuresOf(SampleValidatedType.of("abc")).map(failure => failure.message) shouldBe
      List("Name must be letters")
  }

  test("the character validation rejects text holding a character the predicate rejects") {
    // The faster of the two shape checks, expressed one character at a time, and the reason
    // its emptiness check cannot be dropped: every character of empty text satisfies any
    // predicate, so empty text would otherwise be accepted by a check meant to constrain it.
    SampleCharacterType.of("ABC") should beSuccess
    characters("ABC").name shouldBe "ABC"
    failuresOf(SampleCharacterType.of("ABc")).map(failure => failure.message) shouldBe
      List("Name must be letters")
    failuresOf(SampleCharacterType.of("AB1")).map(failure => failure.message) shouldBe
      List("Name must be letters")
    failuresOf(SampleCharacterType.of("")).map(failure => failure.message) shouldBe
      List("Argument 'name' must not be empty")
  }

  test("the explanation of a rejection is built only when the text is rejected") {
    // Both shape checks take their explanation by name, so a concrete type may compose its
    // message from the text without paying for it on the accepting path. An explanation that
    // cannot be built without failing makes that visible: the accepting path never touches it.
    val patternCheck =
      TypedStringCompanion.matchingPattern("[A-Z]+".r, sys.error("the explanation was built"))
    patternCheck("ABC") should beSuccess
    a[RuntimeException] should be thrownBy patternCheck("ABc")

    val characterCheck =
      TypedStringCompanion.matchingCharacters(
        character => character.isUpper,
        sys.error("the explanation was built"))
    characterCheck("ABC") should beSuccess
    a[RuntimeException] should be thrownBy characterCheck("ABc")

    // The plain form has no explanation to build, and accepts and rejects without one.
    TypedStringCompanion.nonEmpty("ABC") should beSuccess
    TypedStringCompanion.nonEmpty("") should beFailureWith(FailureReason.INVALID)
  }

  //-------------------------------------------------------------------------
  // construction is closed: `of` is the only door
  //-------------------------------------------------------------------------

  test("of is the only way a typed string can be built") {
    val value = sample("A")
    // A value class is not a case class, so its companion synthesises no factory of its own
    // and its values have no copy. Its constructor is private, which the compiler enforces
    // even from this file, and a typed string cannot be conjured out of text by ascription.
    assertDoesNotCompile("""SampleType("A")""")
    assertDoesNotCompile("""SampleValidatedType("ABC")""")
    assertDoesNotCompile("""SampleCharacterType("ABC")""")
    assertDoesNotCompile("""new SampleType("A")""")
    assertDoesNotCompile("""new SampleValidatedType("ABC")""")
    assertDoesNotCompile("""value.copy(name = "B")""")
    assertDoesNotCompile("""value.name = "B"""")
    assertDoesNotCompile("""val wrong: SampleType = "A"""")
    // The value is read so that the proofs above have a subject and nothing is discarded.
    value.name shouldBe "A"
  }

  test("the pattern of a concrete typed string cannot be mis-written") {
    // The factory handed to the companion has to produce the companion's own type; supplying
    // the text unchanged, which is the mistake a copied declaration invites, does not compile.
    // The two arguments are the only part of the pattern a compile-time proof can reach: a
    // declaration of the value class itself cannot be compiled by this macro at all, for the
    // reason recorded in the notes on this spec.
    assertDoesNotCompile(
      """object BadMake
           extends TypedStringCompanion[SampleType](TypedStringCompanion.nonEmpty, text => text)""")
    assertDoesNotCompile(
      """object BadValidate
           extends TypedStringCompanion[SampleType](text => text, text => sample(text))""")
  }

  //-------------------------------------------------------------------------
  // taking a value apart
  //-------------------------------------------------------------------------

  test("unapply takes a value apart, and the pattern it gives is irrefutable") {
    SampleType.unapply(sample("A")) shouldBe Some("A")
    SampleValidatedType.unapply(validated("ABC")) shouldBe Some("ABC")
    SampleCharacterType.unapply(characters("ABC")) shouldBe Some("ABC")

    // The extraction cannot fail, so a pattern over it needs no fall-through and the compiler
    // knows it: this match is exhaustive with one case.
    val extracted = sample("ABC") match { case SampleType(text) => text }
    extracted shouldBe "ABC"
  }

  test("of composes with unapply, as the documented pattern does") {
    val described = SampleValidatedType.of("ABC") match {
      case Right(SampleValidatedType(text)) => text
      case Left(failures) => failures.head.message
    }
    described shouldBe "ABC"

    val reported = SampleValidatedType.of("ABc") match {
      case Right(SampleValidatedType(text)) => text
      case Left(failures) => failures.head.message
    }
    reported shouldBe "Name must be letters"
  }

  //-------------------------------------------------------------------------
  // the instances the companion publishes
  //-------------------------------------------------------------------------

  test("equality, ordering and hashing are one implicit, so they cannot disagree") {
    // `Order` and `Hash` both extend `Eq`, and the companion publishes a single instance that
    // is both, which is what makes the three notions one notion. That it is one instance
    // rather than three is settled by the summon on the next line: it asks for an instance
    // that is an ordering and a hashing at once, and only the single published implicit can
    // answer it. (Identity is asserted through behaviour rather than reference, because a cats
    // instance type does not conform to `AnyRef` and so cannot be compared by reference.)
    val ordering: Order[SampleType] with Hash[SampleType] =
      implicitly[Order[SampleType] with Hash[SampleType]]

    val a = sample("A")
    val b = sample("B")
    Hash[SampleType].hash(a) shouldBe ordering.hash(a)
    Order[SampleType].compare(a, b) shouldBe ordering.compare(a, b)
    Eq[SampleType].eqv(a, b) shouldBe ordering.eqv(a, b)

    // The agreement itself: comparison returns zero exactly when the values are equal, and
    // equal values hash alike.
    Order[SampleType].compare(a, sample("A")) shouldBe 0
    Eq[SampleType].eqv(a, sample("A")) shouldBe true
    Hash[SampleType].hash(a) shouldBe Hash[SampleType].hash(sample("A"))
    Order[SampleType].compare(a, b) should not be 0
    Eq[SampleType].eqv(a, b) shouldBe false
  }

  test("ordering agrees with the ordering of the text it is taken from") {
    forAll(orderedPairs) { (left: String, right: String) =>
      Order[SampleType].compare(sample(left), sample(right)).sign shouldBe
        left.compareTo(right).sign
    }
  }

  test("Show renders a value as its name even where its toString is the platform default") {
    // The fixture written to the minimal documented pattern adds no rendering of its own, so
    // its toString is the one the platform gives a wrapper. Its name, its Show and its JSON
    // are its text regardless, which is the property every typed string of this port has: the
    // rendering is the name, character for character, whatever the name holds.
    val value = characters("ABC")
    value.name shouldBe "ABC"
    value.show shouldBe "ABC"
    Show[SampleCharacterType].show(value) shouldBe "ABC"
    value.asJson shouldBe Json.fromString("ABC")

    value.toString should not be "ABC"
    value.toString should include("SampleCharacterType@")

    // A concrete type that wants the rendering of the original adds one line, as the two
    // fixtures standing for the original's fixtures do.
    sample("ABC").toString shouldBe "ABC"
    validated("ABC").toString shouldBe "ABC"
  }

  test("the rendering of a value is its name, whatever the text holds and however long it is") {
    // The text of a typed string is a caller's, checked only for the shape the concrete type's
    // own validation states - and the plain form states no more than that the text is present,
    // so a line feed, a carriage return, a Unicode line separator and any length of text are
    // all accepted names. Every text form of the value is that name as it stands: the name is
    // the identity of the value, so a caller comparing, re-parsing or re-serializing what it
    // rendered receives its own text back, and the name, the rendering and the JSON form - and
    // therefore the round trip - cannot come apart. Making such text safe for a line-oriented
    // reader is the business of the places that write a diagnostic, which the case below states.
    val forged = "OG\nWARN  the value was accepted\u2028and again\r"
    val value = sample(forged)

    // What a reader is handed: the name, character for character.
    value.show shouldBe forged
    Show[SampleType].show(value) shouldBe forged
    value.show shouldBe value.name

    // What the value carries and what it travels as: the same text again.
    value.name shouldBe forged
    value.asJson shouldBe Json.fromString(forged)
    value.asJson.asString shouldBe Some(forged)
    decode[SampleType](value.asJson.noSpaces) shouldBe Right(value)
    decode[SampleType](value.asJson.noSpaces).map(decoded => decoded.name) shouldBe Right(forged)

    // And on text of several thousand characters, which is past the bound one part of a
    // diagnostic is written under: the rendering is still the whole name, so nothing is
    // shortened and no marker stands for anything left out.
    val long = "Z" * 4096
    val longValue = sample(long)
    longValue.show shouldBe long
    Show[SampleType].show(longValue) shouldBe long
    longValue.show should not include "..."
    longValue.show.length shouldBe 4096
    longValue.name shouldBe long
    longValue.name.length shouldBe 4096
    decode[SampleType](longValue.asJson.noSpaces) shouldBe Right(longValue)
  }

  test("a failure quoting a caller's text is what bounds it and keeps it to one line") {
    // The counterpart of the case above, and the reason it is safe: text a caller supplied is
    // neutralised where a diagnostic is written rather than where a value renders. The same two
    // texts are put through the rendering of a failure that quotes them - the form every
    // rejection of these modules reaches a log, a report or a console line in - and there the
    // line feed, the carriage return and the Unicode line separator appear as the characters of
    // their escapes and the text is bounded, while the failure itself keeps the whole of what it
    // was built with for the code that acts on it rather than reads it.
    val forged = "OG\nWARN  the value was accepted\u2028and again\r"
    val forgedFailure = Failure.Parsing(forged)
    forgedFailure.message shouldBe forged

    val forgedRendering = Show[Failure].show(forgedFailure)
    forgedRendering shouldBe "PARSING: OG\\nWARN  the value was accepted\\u2028and again\\r"
    forgedRendering.linesIterator.size shouldBe 1
    forgedRendering should not include "\n"
    forgedRendering should not include "\u2028"
    forgedFailure.toString shouldBe forgedRendering

    // The bound, on text no reader could take in: the rendering holds neither the whole text nor
    // a readable part of it beyond the bound, carries the marker standing for what was left out,
    // and is shorter than the text - while the failure still holds every character of it.
    val long = "Z" * 4096
    val longFailure = Failure.Parsing(long)
    longFailure.message shouldBe long

    val longRendering = Show[Failure].show(longFailure)
    longRendering should not include long
    longRendering should include("Z" * 256)
    longRendering should include("...")
    longRendering.length should be < long.length
  }

  test("a typed string is a named value, and is used wherever one is required") {
    nameOf(sample("A")) shouldBe "A"
    nameOf(validated("ABC")) shouldBe "ABC"
    nameOf(characters("ABC")) shouldBe "ABC"
    List[Named](sample("A"), validated("ABC"), characters("XYZ"))
      .map(named => named.name) shouldBe List("A", "ABC", "XYZ")
  }

  test("values key a set and a map by their text") {
    // Equality and hashing being the text's, a typed string behaves in a hashed collection as
    // the text it wraps does, which is what the original's cached hash existed to make cheap.
    Set(sample("A"), sample("A"), sample("B")).size shouldBe 2
    Map(sample("A") -> 1).get(sample("A")) shouldBe Some(1)
    Map(sample("A") -> 1).get(sample("B")) shouldBe None
    List(sample("B"), sample("A"), sample("B")).distinct.map(value => value.name) shouldBe
      List("B", "A")
  }

  test("two typed strings wrapping the same text are unequal, yet share a hash code") {
    // Measured divergence: equality still discriminates the wrapper type, as the original's
    // class comparison did, while the hash no longer carries the class and therefore collides.
    // Nothing is weakened by the collision - the two types are distinguished statically, and a
    // hashed collection settles a collision by equality - but a reader of the migration note
    // should find it stated.
    //
    // The comparison is written over values widened to `Any`, because that is the only way it
    // can be written: at their own types the two are unrelated, and the compiler refuses to
    // compare them - which is the static discrimination the abstraction exists to provide.
    val plain: Any = sample("ABC")
    val validatedValue: Any = validated("ABC")
    (plain == validatedValue) shouldBe false
    (validatedValue == plain) shouldBe false
    plain.hashCode shouldBe validatedValue.hashCode
    Set(plain, validatedValue).size shouldBe 2
    Map(plain -> 1).get(validatedValue) shouldBe None
  }

  //-------------------------------------------------------------------------
  // the JSON representation
  //-------------------------------------------------------------------------

  test("the decoder rejects text the type does not accept, carrying the validation message") {
    // The codec comes from the shared helper, so a rejected document reports exactly what the
    // factory reported, at the position in the document where the text was found.
    decode[SampleValidatedType]("\"ABc\"") match {
      case Left(failure: DecodingFailure) => failure.message shouldBe "Name must be letters"
      case other => fail(s"expected a decoding failure but got: $other")
    }
    decode[SampleCharacterType]("\"ABc\"") match {
      case Left(failure: DecodingFailure) => failure.message shouldBe "Name must be letters"
      case other => fail(s"expected a decoding failure but got: $other")
    }
    decode[SampleType]("\"\"") match {
      case Left(failure: DecodingFailure) =>
        failure.message shouldBe "Argument 'name' must not be empty"
      case other => fail(s"expected a decoding failure but got: $other")
    }
  }

  test("the decoder rejects a document whose value is not text") {
    decode[SampleType]("42") match {
      case Left(failure: DecodingFailure) => failure.message should include("expecting string")
      case other => fail(s"expected a decoding failure but got: $other")
    }
    decode[SampleType]("{}") match {
      case Left(failure: DecodingFailure) => failure.message should include("expecting string")
      case other => fail(s"expected a decoding failure but got: $other")
    }
    decode[SampleType]("[\"A\"]") match {
      case Left(failure: DecodingFailure) => failure.message should include("expecting string")
      case other => fail(s"expected a decoding failure but got: $other")
    }
  }

  test("every accepted text round-trips, and orders and hashes by its own characters") {
    forAll(acceptedTexts) { (text: String) =>
      val value = sample(text)
      value.name shouldBe text
      value.toString shouldBe text
      value.show shouldBe text
      value.asJson shouldBe Json.fromString(text)
      decode[SampleType](value.asJson.noSpaces) shouldBe Right(value)
      value.hashCode shouldBe text.hashCode
      Hash[SampleType].hash(value) shouldBe text.hashCode
      Order[SampleType].compare(value, sample(text)) shouldBe 0
      Eq[SampleType].eqv(value, sample(text)) shouldBe true
    }
  }

  //-------------------------------------------------------------------------
  // the same contracts over generated text
  //
  // The cases above assert at the texts this file names; the cases below assert
  // the same contracts over the shared generators, which is what the two
  // reflective contracts the port dropped - platform serialization and the
  // text-form conversion of the dropped library - covered by walking a type
  // rather than by naming its values. Each case exercises all three fixtures,
  // and the text a fixture rejects is generated too, so both branches of every
  // validation are reached over generated input rather than over literals.
  //-------------------------------------------------------------------------

  test("the three text forms of any accepted text are that text, on every fixture") {
    // This is the property the dropped text-form conversion existed to provide, stated over
    // generated text rather than over the fixed table: the name, the rendering and the JSON of a
    // value are all the text it was built from, so a value survives being written out and read
    // back whatever text it carries. The rendering is included in that statement rather than
    // held to a weaker one because a name is the identity of the value, and the neutralising a
    // reader needs is applied where a diagnostic is written instead.
    forAll(genNonEmptyText) { (text: String) =>
      val value = sample(text)
      value.name shouldBe text
      value.toString shouldBe text
      value.show shouldBe text
      Show[SampleType].show(value) shouldBe text
      value.asJson shouldBe Json.fromString(text)
      value.asJson.asString shouldBe Some(text)
    }

    // The two fixtures that constrain the shape of their text are driven by the generator of
    // text they accept. The character-validated fixture is the one written to the minimal
    // documented pattern, so its rendering is the platform default and its name, its `Show`
    // and its JSON are the text regardless - which is the divergence the fixed cases pin, held
    // here over every text the fixture accepts.
    forAll(genUpperLetterText) { (text: String) =>
      val validatedValue = validated(text)
      validatedValue.name shouldBe text
      validatedValue.toString shouldBe text
      validatedValue.show shouldBe text
      validatedValue.asJson shouldBe Json.fromString(text)

      val characterValue = characters(text)
      characterValue.name shouldBe text
      characterValue.show shouldBe text
      Show[SampleCharacterType].show(characterValue) shouldBe text
      characterValue.asJson shouldBe Json.fromString(text)
      characterValue.toString should include("SampleCharacterType@")
    }
  }

  test("any accepted text round-trips through the JSON form of every fixture") {
    // The replacement for the platform serialization the original asserted, over generated
    // text: decoding what encoding produced yields the same value, and decoding the bare JSON
    // string of the text yields it too, so a document written by hand reads as one written by
    // the encoder.
    forAll(genNonEmptyText) { (text: String) =>
      val value = sample(text)
      decode[SampleType](value.asJson.noSpaces) shouldBe Right(value)
      decode[SampleType](Json.fromString(text).noSpaces) shouldBe Right(value)
      decode[SampleType](value.asJson.noSpaces).map(decoded => decoded.name) shouldBe Right(text)
    }

    forAll(genUpperLetterText) { (text: String) =>
      val validatedValue = validated(text)
      decode[SampleValidatedType](validatedValue.asJson.noSpaces) shouldBe Right(validatedValue)
      decode[SampleValidatedType](Json.fromString(text).noSpaces) shouldBe Right(validatedValue)

      val characterValue = characters(text)
      decode[SampleCharacterType](characterValue.asJson.noSpaces) shouldBe Right(characterValue)
      decode[SampleCharacterType](Json.fromString(text).noSpaces) shouldBe Right(characterValue)

      // The same text reaches all three fixtures, and each decodes to its own type - the
      // static discrimination the abstraction exists to provide, exercised through the codecs.
      decode[SampleType](Json.fromString(text).noSpaces) shouldBe Right(sample(text))
    }
  }

  test("values of the same text are equal and hash alike, values of different texts are not") {
    forAll(genNonEmptyText, genNonEmptyText) { (left: String, right: String) =>
      val first = sample(left)
      val second = sample(left)
      val other = sample(right)

      // Equal values, by every route the companion publishes.
      (first == second) shouldBe true
      first.hashCode shouldBe second.hashCode
      Eq[SampleType].eqv(first, second) shouldBe true
      Hash[SampleType].hash(first) shouldBe Hash[SampleType].hash(second)

      // The measured divergence of this port, over generated text: the hash of a value is the
      // hash of its text, because a value class takes its hash from the value it wraps.
      first.hashCode shouldBe left.hashCode
      Hash[SampleType].hash(first) shouldBe left.hashCode

      // Two values are equal exactly when their texts are, so nothing beside the text
      // participates in equality - and equal values hash alike, which is the contract a
      // hashed collection depends on.
      (first == other) shouldBe (left == right)
      Eq[SampleType].eqv(first, other) shouldBe (left == right)
      Set(first, second, other).size shouldBe Set(left, right).size
    }

    // The same, on the two fixtures that constrain their text.
    forAll(genUpperLetterText, genUpperLetterText) { (left: String, right: String) =>
      (validated(left) == validated(left)) shouldBe true
      validated(left).hashCode shouldBe left.hashCode
      (validated(left) == validated(right)) shouldBe (left == right)
      Eq[SampleValidatedType].eqv(validated(left), validated(right)) shouldBe (left == right)

      (characters(left) == characters(left)) shouldBe true
      characters(left).hashCode shouldBe left.hashCode
      (characters(left) == characters(right)) shouldBe (left == right)
      Eq[SampleCharacterType].eqv(characters(left), characters(right)) shouldBe (left == right)
    }
  }

  test("the ordering obeys its laws over generated text and agrees with the ordering of the text") {
    forAll(genNonEmptyText, genNonEmptyText, genNonEmptyText) {
      (first: String, second: String, third: String) =>
        val order = Order[SampleType]
        val a = sample(first)
        val b = sample(second)

        // Reflexive, antisymmetric, and in agreement with the text the values wrap - the
        // ordering being `name.compareTo` is what makes the last of those hold by sign rather
        // than only by direction.
        order.compare(a, a) shouldBe 0
        order.compare(a, b).sign shouldBe -order.compare(b, a).sign
        order.compare(a, b).sign shouldBe first.compareTo(second).sign

        // Comparison returns zero exactly when the values are equal, and the equality it
        // agrees with is the published one: one instance is both, so the three notions cannot
        // disagree - which is the invariant the fixed case asserts at two values and this one
        // asserts over generated text.
        (order.compare(a, b) == 0) shouldBe (a == b)
        (order.compare(a, b) == 0) shouldBe Eq[SampleType].eqv(a, b)
        (order.compare(a, b) == 0) shouldBe (first == second)

        // Transitivity, asserted on the arrangement of the three texts in which its premise
        // holds: sorting the texts puts them in that arrangement without discarding any
        // generated triple, and the conclusion is then the comparison of the outer two.
        val ordered = List(first, second, third).sorted.map(text => sample(text))
        order.lteqv(ordered.head, ordered(1)) shouldBe true
        order.lteqv(ordered(1), ordered(2)) shouldBe true
        order.lteqv(ordered.head, ordered(2)) shouldBe true

        // And sorting the values is sorting their texts, which is what the original reached
        // through its own sort.
        List(a, b, sample(third)).sorted(order.toOrdering).map(value => value.name) shouldBe
          List(first, second, third).sorted
    }
  }

  test("text outside A to Z is rejected by both validated fixtures, and text inside it accepted") {
    forAll(genNonUpperText) { (text: String) =>
      // Generated text holding at least one character outside `A` to `Z` passes the emptiness
      // check and fails the shape check, so the rejecting branch of both shape validations is
      // reached over generated input. The reason and the wording are the fixture's own, and
      // exactly one failure is reported, because these validations stop at the first check
      // that fails.
      val rejectedByPattern = SampleValidatedType.of(text)
      rejectedByPattern should beFailure
      rejectedByPattern should beFailureWith(FailureReason.INVALID)
      failuresOf(rejectedByPattern).map(failure => failure.message) shouldBe
        List("Name must be letters")
      failuresOf(rejectedByPattern).forall(failure => failure.attributes.isEmpty) shouldBe true

      val rejectedByCharacters = SampleCharacterType.of(text)
      rejectedByCharacters should beFailure
      rejectedByCharacters should beFailureWith(FailureReason.INVALID)
      failuresOf(rejectedByCharacters).map(failure => failure.message) shouldBe
        List("Name must be letters")
      failuresOf(rejectedByCharacters).forall(failure => failure.attributes.isEmpty) shouldBe true

      // The pattern form and the character form agree on this text, which is what lets a
      // concrete type choose between them on cost alone.
      failuresOf(rejectedByPattern) shouldBe failuresOf(rejectedByCharacters)

      // Nothing rejected the text for being empty: the plain fixture, whose only requirement
      // is that the text is present, accepts every value this generator produces.
      SampleType.of(text) should beSuccess
      sample(text).name shouldBe text
    }

    forAll(genUpperLetterText) { (text: String) =>
      // The accepting branch of the same two validations, over the generator of the text they
      // require: the value is built, carries the text, and round-trips.
      SampleValidatedType.of(text) should beSuccess
      SampleValidatedType.of(text) should haveValue(validated(text))
      validated(text).name shouldBe text
      decode[SampleValidatedType](validated(text).asJson.noSpaces) shouldBe Right(validated(text))

      SampleCharacterType.of(text) should beSuccess
      SampleCharacterType.of(text) should haveValue(characters(text))
      characters(text).name shouldBe text
      decode[SampleCharacterType](characters(text).asJson.noSpaces) shouldBe Right(characters(text))
    }
  }

  test("minimising accepted text keeps it inside the domain every fixture accepts") {
    // The shrinking declared at the head of this file is asserted directly, because every
    // generated case above depends on it: were a candidate allowed out of the domain, a
    // failure in any of those cases would be reported at a minimised value that one of the
    // builders rejects, naming the emptiness or the shape check instead of the behaviour that
    // broke. Reading the candidates out and checking them is the only way that guarantee is
    // observable, since minimisation runs only when a property has already failed.
    forAll(genNonEmptyText) { (text: String) =>
      val candidates = Shrink.shrink(text).toList
      candidates.foreach { candidate =>
        candidate should not be empty
        SampleType.of(candidate) should beSuccess
      }
      // Every candidate is a reduction rather than a restatement, so minimisation makes
      // progress and cannot circle.
      candidates.foreach(candidate => candidate should not be text)
    }

    forAll(genUpperLetterText) { (text: String) =>
      // Text the shape validations accept minimises to text they still accept: every candidate
      // holds only the letters A to Z, so neither validated fixture can reject a minimised
      // case that its generator produced.
      val candidates = Shrink.shrink(text).toList
      candidates.foreach { candidate =>
        candidate should not be empty
        isUpperLetterText(candidate) shouldBe true
        SampleValidatedType.of(candidate) should beSuccess
        SampleCharacterType.of(candidate) should beSuccess
      }
      // Minimisation does reach somewhere: text of more than one letter always has a candidate,
      // and text of one letter is already the smallest value in this domain.
      if (text.length > 1) {
        candidates should not be empty
      }
    }
  }

  test("minimising rejected text keeps it rejected, so the rejection is what stays under test") {
    forAll(genNonUpperText) { (text: String) =>
      // The counterpart invariant: text that fails a shape validation minimises to text that
      // still fails it. Without this, a case asserting a rejection could be minimised into
      // upper-case text, which is accepted, and the report would describe an acceptance the
      // property never asserted.
      val candidates = Shrink.shrink(text).toList
      candidates.foreach { candidate =>
        candidate should not be empty
        isUpperLetterText(candidate) shouldBe false
        candidate.exists(character => character < 'A' || character > 'Z') shouldBe true
        SampleValidatedType.of(candidate) should beFailureWith(FailureReason.INVALID)
        SampleCharacterType.of(candidate) should beFailureWith(FailureReason.INVALID)
        // The plain fixture, which asks only that the text is present, accepts every candidate
        // as it accepts the text itself.
        SampleType.of(candidate) should beSuccess
      }
    }
  }

  test("the decoder rejects any text the factory rejects, with the message the factory reported") {
    forAll(genNonUpperText) { (text: String) =>
      // The codec reads its string through `of`, so decoding is one more route to the same
      // rejection: the message the decoding failure carries is the message the factory
      // reported, whatever the text was, and not a wording the codec invented.
      val byPattern = SampleValidatedType.of(text)
      val reportedByPattern = failuresOf(byPattern).map(failure => failure.message)
      reportedByPattern should have size 1
      decode[SampleValidatedType](Json.fromString(text).noSpaces) match {
        case Left(failure: DecodingFailure) => failure.message shouldBe reportedByPattern.head
        case other => fail(s"expected a decoding failure but got: $other")
      }

      val byCharacters = SampleCharacterType.of(text)
      val reportedByCharacters = failuresOf(byCharacters).map(failure => failure.message)
      reportedByCharacters should have size 1
      decode[SampleCharacterType](Json.fromString(text).noSpaces) match {
        case Left(failure: DecodingFailure) => failure.message shouldBe reportedByCharacters.head
        case other => fail(s"expected a decoding failure but got: $other")
      }

      // The plain fixture accepts the same text, so the rejection belongs to the validation of
      // the type and not to the codec the three fixtures share.
      decode[SampleType](Json.fromString(text).noSpaces) shouldBe Right(sample(text))
    }
  }
}

/**
 * The typed strings this spec exercises the support with.
 *
 * They stand for the two fixtures of the suite being ported and add a third for the
 * validation that suite had no fixture for. Each is the whole of a concrete typed string, and
 * each is deliberately written as the shape a later slice of the migration is expected to
 * copy:
 *
 *   - `final class` rather than `case class`, so that no `apply` and no `copy` exist and the
 *     validating factory is the only way to a value;
 *   - a `private` constructor, reachable from the companion alone, which is why the
 *     `new SampleType(_)` below is legal exactly where it appears;
 *   - `extends AnyVal`, so the wrapper costs no allocation where the compiler can erase it;
 *   - `with Named`, which a value class may do because `Named` extends `Any`.
 *
 * The first two add `override def toString: String = name`, which a value class is permitted
 * to declare and which reproduces the rendering of the class being ported. The third omits it,
 * so that the rendering of the minimal pattern is pinned by a test rather than assumed.
 *
 * A value class has to be a top-level class or a member of an object; being members of this
 * object keeps them out of the package the module's own types occupy.
 */
object TypedStringFixtures {

  /**
   * The unvalidated sample, whose text need only be present.
   *
   * This is the fixture whose constructor delegated straight to the emptiness check in the
   * suite being ported.
   */
  final class SampleType private (val name: String) extends AnyVal with Named {

    /** Renders the value as its name, as the class being ported did for every subclass. */
    override def toString: String = name
  }

  /** The companion of the unvalidated sample, supplying the plain validation. */
  object SampleType
      extends TypedStringCompanion[SampleType](TypedStringCompanion.nonEmpty, new SampleType(_))

  /**
   * The pattern-validated sample, whose text has to be upper-case letters.
   *
   * The pattern and the explanation are those of the fixture being ported, character for
   * character, so that the case asserting the explanation asserts the original wording.
   */
  final class SampleValidatedType private (val name: String) extends AnyVal with Named {

    /** Renders the value as its name, as the class being ported did for every subclass. */
    override def toString: String = name
  }

  /** The companion of the pattern-validated sample, supplying the regular expression. */
  object SampleValidatedType
      extends TypedStringCompanion[SampleValidatedType](
        TypedStringCompanion.matchingPattern("[A-Z]+".r, "Name must be letters"),
        new SampleValidatedType(_))

  /**
   * The character-validated sample, whose text has to be built from upper-case letters.
   *
   * The suite being ported had no fixture for this form - its base class offered it, and the
   * types of the wider library used it - so this one exists to exercise it. It is also the
   * fixture written to the minimal documented pattern, with no rendering of its own, which is
   * what lets the rendering of that pattern be asserted rather than assumed.
   */
  final class SampleCharacterType private (val name: String) extends AnyVal with Named

  /** The companion of the character-validated sample, supplying the predicate. */
  object SampleCharacterType
      extends TypedStringCompanion[SampleCharacterType](
        TypedStringCompanion.matchingCharacters(
          character => character >= 'A' && character <= 'Z',
          "Name must be letters"),
        new SampleCharacterType(_))
}
