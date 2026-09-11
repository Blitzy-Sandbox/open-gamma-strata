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

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

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
final class TypedStringSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

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
    // the three text forms a typed string has, which have to be the same text for the value
    // to survive being written out and read back.
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
    // are its text regardless, which is the property every typed string of this port has.
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
