/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.result

import java.util.Locale

import scala.collection.immutable.SortedMap

import cats.Eq
import cats.Hash
import cats.Order
import cats.Show
import cats.data.Chain
import cats.data.NonEmptyChain

import io.circe.DecodingFailure
import io.circe.parser.decode
import io.circe.syntax._

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import com.opengamma.strata.collect.Arbitraries._
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Tests the error vocabulary of this module: [[FailureReason]], the ten reasons an operation
 * can fail for, and [[Failure]], the value that carries one of them together with a message
 * and attributes.
 *
 * This is the foundational spec of the `result` package. The other two specs beside it
 * assert over outcomes whose failure side is built from this vocabulary, so the facts pinned
 * here - which names resolve, what a failure holds, how failures render, serialize and
 * combine - are the facts those specs rely on rather than restate.
 *
 * ===What the port removed, and how this spec covers it===
 *
 * The three test classes this file is ported from tested three types, two of which do not
 * survive the port:
 *
 *   - the reason enum survives as [[FailureReason]], a closed family of ten values;
 *   - the failure item - a bean holding a reason, a message, a message template, a set of
 *     attributes, the type of a captured condition and the rendered stack of the call that
 *     reported it - survives as [[Failure]], holding a reason, a message and attributes and
 *     nothing else;
 *   - the aggregate of failure items, and the builder that assembled one, do not survive at
 *     all: a `NonEmptyChain[Failure]` is the aggregate, and `Failure.collapse` presents such
 *     a chain as a single failure.
 *
 * Thirteen of the eighteen cases of the item test class therefore tested machinery that is
 * gone: template placeholders that populated attributes by name, capture of a thrown
 * condition and its type, and the rendered stack. Each of those cases still has a case of
 * its own here, under "What a failure no longer holds", and each pins the fact that replaced
 * the behaviour it tested rather than asserting a tautology - that a reason is chosen at the
 * construction site, that a message is whatever the reporter interpolated, and that nothing
 * about an underlying condition survives in the value. The mapping at the foot of this file
 * records that correspondence case by case.
 *
 * ===Why the reason lookup needs two cases where the original needed one===
 *
 * The lookup of the original registered every constant twice, under its own name and under
 * that name folded to lower case, so its single factory accepted `MISSING_DATA`,
 * `missing_data` and, since folding an upper-case name changes nothing, the upper-case form
 * as well. The port splits that surface in two: `valueOf` is the exact lookup and `parse` is
 * the lenient one, which folds its input to upper case. Between them they accept everything
 * the original accepted, and this spec asserts each of the three casings against both
 * members so that the division is visible rather than implied.
 *
 * ===Serialization===
 *
 * The JSON codecs of these two types are declared by the types themselves, deliberately:
 * the shared JSON helpers of this module are written in terms of the failure model, so the
 * codecs for the failure model cannot be written there without pointing the dependencies of
 * the module back at themselves. This spec is consequently the only place their round-trips
 * are exercised - the spec of the shared helpers asserts that it defines none of them - and
 * it covers the wire form, the round-trip over generated values, and the byte-stability that
 * holding attributes in key order buys.
 *
 * ===Determinism===
 *
 * Every case is a pure function of literal data or of generated data drawn from the shared
 * generators of this module. Nothing here reads a clock, a file, the class path or the
 * environment, and no case shares mutable state with another, so the outcome does not depend
 * on the order the cases run in.
 *
 * @see [[FailureReason]] for the ten reasons
 * @see [[Failure]] for the failures that carry them
 * @see [[com.opengamma.strata.collect.testkit.ResultMatchers]] for the outcome matchers used here
 */
final class FailureSpec
    extends AnyFunSuite
    with Matchers
    with ScalaCheckPropertyChecks
    with TableDrivenPropertyChecks {

  // ---------------------------------------------------------------------------
  // Fixtures.
  //
  // The two failures are the pair the aggregate test class being ported used,
  // with the same reasons and the same messages. They disagree on their reason,
  // which is what makes them collapse to `MULTIPLE`, and each is ascribed to the
  // trait rather than left at its member type: the JSON codecs and the equality
  // instance are declared for `Failure`, so an expression whose static type is a
  // member would not resolve them.
  // ---------------------------------------------------------------------------

  /** The first failure of the pair: invalid input. */
  private val failure1: Failure = Failure.Invalid("invalid")

  /** The second failure of the pair: missing data, disagreeing with the first. */
  private val failure2: Failure = Failure.MissingData("data")

  /** The pair as a chain, the shape that replaces the aggregate type of the original. */
  private val pair: NonEmptyChain[Failure] = NonEmptyChain.of(failure1, failure2)

  /**
   * The ten reasons paired with their canonical names, transcribed from the data provider of
   * the test class being ported - which lists them alphabetically, not in declaration order.
   * The declaration order is asserted separately, over `FailureReason.values`.
   */
  private val reasonNames = Table(
    ("reason", "name"),
    (FailureReason.CALCULATION_FAILED: FailureReason, "CALCULATION_FAILED"),
    (FailureReason.CURRENCY_CONVERSION: FailureReason, "CURRENCY_CONVERSION"),
    (FailureReason.ERROR: FailureReason, "ERROR"),
    (FailureReason.INVALID: FailureReason, "INVALID"),
    (FailureReason.MISSING_DATA: FailureReason, "MISSING_DATA"),
    (FailureReason.MULTIPLE: FailureReason, "MULTIPLE"),
    (FailureReason.NOT_APPLICABLE: FailureReason, "NOT_APPLICABLE"),
    (FailureReason.OTHER: FailureReason, "OTHER"),
    (FailureReason.PARSING: FailureReason, "PARSING"),
    (FailureReason.UNSUPPORTED: FailureReason, "UNSUPPORTED"))

  /** The ten failure members paired with the reason each carries. */
  private val failuresByReason = Table(
    ("failure", "reason"),
    (Failure.Multiple("m"): Failure, FailureReason.MULTIPLE: FailureReason),
    (Failure.Error("m"): Failure, FailureReason.ERROR: FailureReason),
    (Failure.Invalid("m"): Failure, FailureReason.INVALID: FailureReason),
    (Failure.Parsing("m"): Failure, FailureReason.PARSING: FailureReason),
    (Failure.NotApplicable("m"): Failure, FailureReason.NOT_APPLICABLE: FailureReason),
    (Failure.Unsupported("m"): Failure, FailureReason.UNSUPPORTED: FailureReason),
    (Failure.MissingData("m"): Failure, FailureReason.MISSING_DATA: FailureReason),
    (Failure.CurrencyConversion("m"): Failure, FailureReason.CURRENCY_CONVERSION: FailureReason),
    (Failure.CalculationFailed("m"): Failure, FailureReason.CALCULATION_FAILED: FailureReason),
    (Failure.Other("m"): Failure, FailureReason.OTHER: FailureReason))

  /**
   * Failures that agree on a reason, in a chain.
   *
   * Used by the collapse property: a chain assembled this way has one reason throughout, so
   * collapsing it must report that reason rather than `MULTIPLE`.
   */
  private val genChainSharingAReason: Gen[NonEmptyChain[Failure]] =
    for {
      reason <- genFailureReason
      head <- genFailureWithReason(reason)
      count <- Gen.choose(0, 3)
      tail <- Gen.listOfN(count, genFailureWithReason(reason))
    } yield NonEmptyChain.of(head, tail: _*)

  // ===========================================================================
  // FailureReason - names and rendering
  // ===========================================================================

  test("every reason renders as its upper-underscore name") {
    forAll(reasonNames) { (reason: FailureReason, name: String) =>
      // `name` is the canonical form, `toString` reproduces the rendering of the enum being
      // ported, and `Show` is the instance that code of this port renders through. All three
      // are the bare identifier, so a reason reads the same way however it reaches text.
      reason.name shouldBe name
      reason.toString shouldBe name
      Show[FailureReason].show(reason) shouldBe name
    }
  }

  // ===========================================================================
  // FailureReason - resolving a name
  // ===========================================================================

  test("valueOf and parse both resolve the canonical name of every reason") {
    forAll(reasonNames) { (reason: FailureReason, name: String) =>
      FailureReason.valueOf(name) shouldBe Some(reason)
      FailureReason.parse(name) should beSuccess
      FailureReason.parse(name) should haveValue(reason)
    }
  }

  test("the upper-case form of a reason name is its canonical form and resolves") {
    forAll(reasonNames) { (reason: FailureReason, name: String) =>
      // The canonical names of this family are already upper case, so folding one changes
      // nothing and this case coincides with the exact-case case above. It is kept as a case
      // of its own because the class being ported asserted it separately, and because the
      // coincidence is a property of these names rather than of the lookup.
      val upper = name.toUpperCase(Locale.ENGLISH)
      upper shouldBe name
      FailureReason.valueOf(upper) shouldBe Some(reason)
      FailureReason.parse(upper) should haveValue(reason)
    }
  }

  test("valueOf rejects the lower-case form of a reason name that parse resolves") {
    forAll(reasonNames) { (reason: FailureReason, name: String) =>
      // This is the case that separates the two members. The lookup of the original held a
      // lower-case key for every constant, so its single factory resolved this text; here
      // the exact lookup does not, and the lenient one does because it folds its input to
      // upper case first. Between them the port accepts exactly what the original accepted.
      val lower = name.toLowerCase(Locale.ENGLISH)
      lower should not be name
      FailureReason.valueOf(lower) shouldBe None
      FailureReason.parse(lower) should beSuccess
      FailureReason.parse(lower) should haveValue(reason)
    }
  }

  test("parse also resolves a mixed-case reason name the original would have rejected") {
    // A widening, deliberately recorded: the lookup of the original held two keys per
    // constant, its own and the lower-case one, so text in any other casing resolved to
    // nothing. Folding to upper case accepts every casing, which is strictly more permissive
    // and cannot turn text that used to resolve into text that does not.
    FailureReason.parse("Missing_Data") should haveValue(FailureReason.MISSING_DATA: FailureReason)
    FailureReason.parse("mIsSiNg_DaTa") should haveValue(FailureReason.MISSING_DATA: FailureReason)
    // Only the casing is tolerated. A run-together spelling was never a name of this family.
    FailureReason.parse("MissingData") should beFailureWith(FailureReason.PARSING)
  }

  test("text that names no reason is rejected as a parsing failure") {
    // Where the original raised an error, the port reports the rejection as a value: the
    // left of the outcome holds one failure whose reason is `PARSING` and whose message
    // names both the family and the text that could not be resolved. The message is
    // asserted in full, because the label it opens with is this family's own - the lookup
    // is labelled `FailureReason`, as the registry being ported labelled its own rejections
    // with the simple name of the type - and not the generic label a family that supplies
    // none would carry.
    FailureReason.valueOf("Rubbish") shouldBe None
    FailureReason.parse("Rubbish") should beFailure
    FailureReason.parse("Rubbish") should beFailureWith(FailureReason.PARSING)
    FailureReason.parse("Rubbish") should
      haveFailureMessageMatching("FailureReason name not found: Rubbish")
    // The same fact without the matcher, which also pins that exactly one failure is
    // reported: the chain is a single parse failure carrying that message and no other.
    FailureReason.parse("Rubbish").left.map(_.iterator.map(_.message).toList) shouldBe
      Left(List("FailureReason name not found: Rubbish"))
  }

  test("empty and blank text name no reason") {
    // This stands in for the case of the original that passed nothing at all. Nothing is not
    // a value of `String` in this port - a parameter of that type is always a string - so
    // the closest text that can be supplied, and the text a caller reading from a file or a
    // form actually supplies, is empty or blank. Both are rejected, as no reason is named by
    // either.
    FailureReason.valueOf("") shouldBe None
    FailureReason.valueOf("  ") shouldBe None
    FailureReason.parse("") should beFailureWith(FailureReason.PARSING)
    FailureReason.parse("  ") should beFailureWith(FailureReason.PARSING)
  }

  // ===========================================================================
  // FailureReason - the closed family
  //
  // These two cases replace the reflective sweep with which the class being
  // ported closed: it walked the constants of the enum back from its own class
  // and exercised each. Nothing here reads a class, because the family is a list
  // in the companion; what is worth asserting instead is that the list is the
  // one the port claims - ten members, in the declaration order of the original
  // - and that the single equality-bearing instance behaves as an equality.
  // ===========================================================================

  test("the family holds exactly the ten reasons, in declaration order") {
    val values = FailureReason.values.toList
    values should have size 10
    values.distinct should have size 10
    // Declaration order, which is the order of the enum constants being ported and is not
    // the alphabetical order the table above uses or the `Order` instance imposes.
    values shouldBe List(
      FailureReason.MULTIPLE,
      FailureReason.ERROR,
      FailureReason.INVALID,
      FailureReason.PARSING,
      FailureReason.NOT_APPLICABLE,
      FailureReason.UNSUPPORTED,
      FailureReason.MISSING_DATA,
      FailureReason.CURRENCY_CONVERSION,
      FailureReason.CALCULATION_FAILED,
      FailureReason.OTHER)
    // Every member is reachable by its own name, which is what makes the family closed in
    // the sense that matters: the set resolvable at run time is the set listed here.
    values.foreach(reason => FailureReason.valueOf(reason.name) shouldBe Some(reason))
    values.map(_.name).distinct should have size 10
    // The lookup the companion publishes is built from that list and from nothing else. All
    // three of the tables a named family may declare are empty here, so the whole name space
    // of the family is its ten canonical names - which is why a run-together spelling
    // resolves to nothing, and why no name can be added to the family from outside it.
    val lookup = NamedEnum[FailureReason]
    lookup.values shouldBe FailureReason.values
    lookup.byCanonicalName.keySet shouldBe values.map(_.name).toSet
    lookup.byUpperName.keySet shouldBe values.map(_.name).toSet
    lookup.alternateNames.isEmpty shouldBe true
    lookup.lenientPatterns.isEmpty shouldBe true
    lookup.externalNameGroups.isEmpty shouldBe true
    // The lookup carries this family's own label rather than the generic one a family that
    // supplies none falls back to, which is what makes a rejection name `FailureReason`
    // exactly as the registry being ported named the type it was searching.
    lookup.familyName shouldBe "FailureReason"
    lookup.toString shouldBe "NamedEnum[FailureReason]"
  }

  test("the ordering of reasons agrees with their equality and hashing") {
    val values = FailureReason.values.toList
    val order = Order[FailureReason]
    val hash = Hash[FailureReason]
    for {
      left <- values
      right <- values
    } {
      // The instance is one value that is both an `Order` and a `Hash`, so the two notions
      // cannot disagree: comparing equal and being equal are the same condition here.
      withClue(s"${left.name} against ${right.name}: ") {
        (order.compare(left, right) == 0) shouldBe order.eqv(left, right)
        (order.compare(left, right) == 0) shouldBe hash.eqv(left, right)
        if (hash.eqv(left, right)) {
          hash.hash(left) shouldBe hash.hash(right)
        }
      }
    }
    // Comparison is by name, so the ordering is alphabetical rather than positional.
    values.sorted(order.toOrdering).map(_.name) shouldBe values.map(_.name).sorted
    // Every one of the three notions is derived from the name, hashing included: the hash of
    // a reason is the hash of its canonical name, which is the hashing a named family of
    // this library publishes and what lets a reason hash the same way as the name a document
    // holds it under. An identity-based or constant hash fails this line, and it holds for
    // all ten reasons rather than for a sampled one.
    values.foreach(reason => withClue(s"${reason.name}: ") {
      hash.hash(reason) shouldBe reason.name.hashCode
    })
    // Distinct reasons hash distinctly, which is what makes the previous line a statement
    // about the names rather than about one shared number.
    values.map(hash.hash).distinct should have size 10
  }

  // ===========================================================================
  // FailureReason - JSON
  //
  // The codec replaces two mechanisms of the original at once: the binary
  // serialization of the enum, and the string conversion an annotation drove.
  // Both wrote the constant name, so the JSON form is that same single string
  // and a document written by either is read back as the same reason.
  // ===========================================================================

  test("every reason encodes as the bare string of its canonical name and decodes back") {
    forAll(reasonNames) { (reason: FailureReason, name: String) =>
      reason.asJson.noSpaces shouldBe s""""$name""""
      decode[FailureReason](s""""$name"""") shouldBe Right(reason)
    }
  }

  test("any reason round-trips through its JSON form") {
    forAll { (reason: FailureReason) =>
      decode[FailureReason](reason.asJson.noSpaces) shouldBe Right(reason)
    }
  }

  test("the JSON decoder reads any casing, and rejects text that names no reason") {
    // The decoder reads through the lenient lookup, so a document holding a name in the
    // lower-case form the original also wrote is accepted.
    decode[FailureReason]("\"missing_data\"") shouldBe Right(FailureReason.MISSING_DATA)
    // Text that names no reason is a decoding failure carrying the messages of the parse
    // failures joined with "; " - one message here - so the decoding failure reads as the
    // family-labelled rejection in full, and a value of the wrong JSON type is rejected as
    // well: only a string is a reason.
    val unknown = decode[FailureReason]("\"Rubbish\"")
    unknown.isLeft shouldBe true
    unknown.left.toOption.collect { case failure: DecodingFailure => failure.message } shouldBe
      Some("FailureReason name not found: Rubbish")
    decode[FailureReason]("42").isLeft shouldBe true
    decode[FailureReason]("{}").isLeft shouldBe true
  }

  // ===========================================================================
  // Failure - construction, message and attributes
  // ===========================================================================

  test("a failure carries the reason of its class, the message interpolated at the call site and no attributes") {
    // The case of the original built its message from a template holding two positional
    // placeholders. There is no template here: the message is interpolated where the failure
    // is reported, which is why nothing of the template survives on the value.
    val big = "big"
    val bad = "bad"
    val test: Failure = Failure.Invalid(s"my $big $bad failure")
    test.reason shouldBe FailureReason.INVALID
    test.message shouldBe "my big bad failure"
    test.attributes shouldBe SortedMap.empty[String, String]
    Show[Failure].show(test) shouldBe "INVALID: my big bad failure"
    // The three properties the original also held are absent from the type, not merely
    // empty on this value. That the accessors do not exist is the proof.
    assertDoesNotCompile("""Failure.Invalid("my big bad failure").messageTemplate""")
    assertDoesNotCompile("""Failure.Invalid("my big bad failure").stackTrace""")
    assertDoesNotCompile("""Failure.Invalid("my big bad failure").causeType""")
  }

  test("withAttribute adds one attribute and preserves the class, the reason and the message") {
    // The case of the original also exercised named placeholders, which populated attributes
    // from the message template and recorded where in the template each name sat. Neither
    // exists here, so an attribute is present exactly when a caller put it there.
    val test: Failure = Failure.Invalid("my big bad failure").withAttribute("foo", "bar")
    test.attributes shouldBe SortedMap("foo" -> "bar")
    test.reason shouldBe FailureReason.INVALID
    test.message shouldBe "my big bad failure"
    test shouldBe Failure.Invalid("my big bad failure", SortedMap("foo" -> "bar"))
    // Adding the same key again replaces its value rather than accumulating a second entry.
    test.withAttribute("foo", "baz").attributes shouldBe SortedMap("foo" -> "baz")
  }

  test("withAttributes merges the supplied attributes, letting them win, and holds the result in key order") {
    val base: Failure = Failure.Invalid("my big bad failure", SortedMap("one" -> "big", "two" -> "bad"))
    val test: Failure = base.withAttributes(Map("foo" -> "bar", "two" -> "good"))
    // `two` was held already and is supplied again: the supplied value wins, which is the
    // rule the case of the original proved by overwriting a value derived from its template.
    test.attributes shouldBe SortedMap("foo" -> "bar", "one" -> "big", "two" -> "good")
    // The iteration order is the key order, whatever order the attributes arrived in. The
    // original held an insertion-ordered map, so this is a deliberate difference: it is what
    // makes the rendering and the JSON of a failure a function of its value alone.
    test.attributes.toList shouldBe List("foo" -> "bar", "one" -> "big", "two" -> "good")
    test.reason shouldBe FailureReason.INVALID
    test.message shouldBe "my big bad failure"
    // Merging nothing changes nothing.
    base.withAttributes(Map.empty) shouldBe base
  }

  test("mapMessage transforms the message and preserves the class, the reason and the attributes") {
    val base: Failure = Failure.Invalid("Failure", SortedMap("one" -> "big"))
    val test: Failure = base.mapMessage(message => "Big " + message)
    test.message shouldBe "Big Failure"
    test.reason shouldBe base.reason
    test.attributes shouldBe base.attributes
    test shouldBe Failure.Invalid("Big Failure", SortedMap("one" -> "big"))
  }

  test("mapMessage with the identity function changes nothing, for any failure") {
    forAll { (failure: Failure) =>
      failure.mapMessage(identity) shouldBe failure
      // Whatever the transformation, it reaches the message and nothing else.
      val mapped = failure.mapMessage(message => s"context: $message")
      mapped.reason shouldBe failure.reason
      mapped.attributes shouldBe failure.attributes
      mapped.message shouldBe s"context: ${failure.message}"
    }
  }


  // ===========================================================================
  // Failure - what a failure no longer holds
  //
  // The thirteen cases below correspond one for one to the cases of the item
  // test class that exercised machinery this port removed: message templates
  // that populated attributes by name and recorded their positions, capture of a
  // thrown condition together with its type, the rendered stack of the reporting
  // call, and the rule by which a failure wrapped in a condition replaced the
  // reason its caller had asked for.
  //
  // None of it exists here. A failure is a value holding a reason fixed by its
  // class, a message the reporter interpolated, and the attributes the reporter
  // chose - and a condition that is thrown is never turned into one, inside this
  // module or anywhere else in the port. Each case therefore pins the fact that
  // replaced the behaviour it is ported from, and where the absence of an
  // accessor is the point, it is proved by the accessor failing to compile.
  // ===========================================================================

  test("a failure holds no stack trace, so there is none to shorten or to summarise") {
    val test: Failure = Failure.Invalid("my issue")
    // The rendering of the original appended a summary of the stack to the reason and the
    // message when it had one. There is nothing to append, so the rendering is the two parts
    // the port keeps.
    Show[Failure].show(test) shouldBe "INVALID: my issue"
    assertDoesNotCompile("""Failure.Invalid("my issue").stackTrace""")
    assertDoesNotCompile("""Failure.Invalid("my issue").summarizeStackTrace""")
    // A caller that wants such a detail in the rendering carries it as an attribute, which
    // is ordinary data and renders after the message.
    Show[Failure].show(test.withAttribute("detail", "Short detail")) shouldBe
      "INVALID: my issue [detail=Short detail]"
  }

  test("data named by a message travels in attributes added at the construction site") {
    // The case of the original wrote `{value}` and `{name}` in a template, and the machinery
    // that expanded it also added an attribute per placeholder. Here the message and the
    // attributes are written separately, by the reporter, which is the only way the two can
    // be made to agree.
    val test: Failure = Failure
      .Unsupported("This someValue is unsupported for someName")
      .withAttributes(Map("value" -> "someValue", "name" -> "someName"))
    test.reason shouldBe FailureReason.UNSUPPORTED
    test.message shouldBe "This someValue is unsupported for someName"
    test.attributes shouldBe SortedMap("name" -> "someName", "value" -> "someValue")
    assertDoesNotCompile("""Failure.Unsupported("m").messageTemplate""")
  }

  test("an interpolated message leaves no placeholder behind and populates no attribute") {
    // The positional form of the template of the original produced the same message as the
    // named form but populated nothing. Interpolation is that case, and it is now the only
    // case: the message is finished text by the time the failure exists.
    val value = "someValue"
    val name = "someName"
    val test: Failure = Failure.Unsupported(s"This $value is unsupported for $name")
    test.message shouldBe "This someValue is unsupported for someName"
    test.message should not include "{"
    test.attributes shouldBe SortedMap.empty[String, String]
  }

  test("a message with nothing to interpolate is carried verbatim and adds no attribute") {
    val text = "This value is unsupported for name"
    val test: Failure = Failure.Unsupported(text)
    test.message shouldBe text
    test.attributes shouldBe SortedMap.empty[String, String]
    Show[Failure].show(test) shouldBe s"UNSUPPORTED: $text"
  }

  test("a failure reporting a rejected argument is built explicitly and retains no cause type") {
    // The original derived the reason, the message and the type of the cause from a thrown
    // condition. The reason is now chosen by naming a member, and the message is the text the
    // reporter wanted; the type of whatever signalled the problem is not part of the value.
    val test: Failure = Failure.Invalid("exmsg")
    test.reason shouldBe FailureReason.INVALID
    test.message shouldBe "exmsg"
    test.attributes shouldBe SortedMap.empty[String, String]
    Show[Failure].show(test) shouldBe "INVALID: exmsg"
    assertDoesNotCompile("""Failure.Invalid("exmsg").causeType""")
  }

  test("a failure reporting a fatal condition is built explicitly and retains no cause type") {
    // The companion case of the original built a failure from an `Error` rather than an
    // exception and recorded that type. Since no type is recorded, two failures reporting
    // different underlying conditions under the same reason and message are the same value -
    // which is exactly why the port asks the reporter to say what went wrong in the message.
    val test: Failure = Failure.Error("exmsg")
    test.reason shouldBe FailureReason.ERROR
    test.message shouldBe "exmsg"
    Show[Failure].show(test) shouldBe "ERROR: exmsg"
    Hash[Failure].eqv(test, Failure.Error("exmsg")) shouldBe true
    // The ten reasons classify what went wrong; none of them names a kind of condition.
    val names = FailureReason.values.toList.map(_.name)
    names.contains("EXCEPTION") shouldBe false
    names.contains("THROWABLE") shouldBe false
  }

  test("a message supplied by the reporter stands on its own, without the text of a cause") {
    // The original appended the text of the captured condition to the rendering while
    // keeping the supplied message as the message. Here the message is the whole of what is
    // rendered, and a reporter that wants the detail interpolates it.
    val test: Failure = Failure.Invalid("my failure")
    test.message shouldBe "my failure"
    Show[Failure].show(test) shouldBe "INVALID: my failure"
    val detail = "exmsg"
    val detailed: Failure = Failure.Invalid(s"my failure: $detail")
    detailed.message shouldBe "my failure: exmsg"
    Show[Failure].show(detailed) shouldBe "INVALID: my failure: exmsg"
  }

  test("nothing nested behind a failure is retained, only what its message and attributes say") {
    // The case of the original nested one condition inside another and asserted that the
    // outer type was recorded. A failure has no place to record either, so a failure
    // reported over a nest of conditions is indistinguishable from one reported directly.
    val test: Failure = Failure.Invalid("my big bad failure")
    test.message shouldBe "my big bad failure"
    test.attributes shouldBe SortedMap.empty[String, String]
    Hash[Failure].eqv(test, Failure.Invalid("my big bad failure")) shouldBe true
    assertDoesNotCompile("""Failure.Invalid("my big bad failure").cause""")
  }

  test("the attributes of a failure are the data its message names, and nothing is added for it") {
    // The original added an attribute of its own alongside the ones its template named - the
    // text of the captured condition - so the attribute map of a failure was partly the
    // reporter's and partly the machinery's. Here it is entirely the reporter's.
    val test: Failure = Failure
      .Invalid("a big bad failure")
      .withAttribute("foo", "big")
      .withAttribute("bar", "bad")
    test.reason shouldBe FailureReason.INVALID
    test.message shouldBe "a big bad failure"
    test.attributes shouldBe SortedMap("bar" -> "bad", "foo" -> "big")
    test.attributes.keySet shouldBe Set("bar", "foo")
  }

  test("the text of an underlying condition is carried as an ordinary attribute when it is wanted") {
    // Where the original reserved an attribute name for the text of the captured condition,
    // an attribute name here is an ordinary string with no reserved vocabulary behind it.
    val test: Failure = Failure.Invalid("failure: error").withAttribute("exceptionMessage", "exmsg")
    test.attributes shouldBe SortedMap("exceptionMessage" -> "exmsg")
    test.message shouldBe "failure: error"
    Show[Failure].show(test) shouldBe "INVALID: failure: error [exceptionMessage=exmsg]"
    // There is no enumeration of permitted attribute names to consult or extend.
    assertDoesNotCompile("FailureAttributeKeys.VALUE")
  }

  test("a failure keeps the reason chosen by the code that reported it") {
    // In the original, wrapping a condition that already carried a failure replaced the
    // reason the caller asked for with the reason that failure held. No such rule exists
    // here: the class of a failure fixes its reason, and no combinator can change it.
    Failure.Parsing("Error on line 23: Bad value 'foo'").reason shouldBe FailureReason.PARSING
    Failure.Invalid("Error on line 23").reason shouldBe FailureReason.INVALID
    val parsing: Failure = Failure.Parsing("Bad value 'foo'")
    parsing.mapMessage(message => s"Error on line 23: $message").reason shouldBe FailureReason.PARSING
    parsing.withAttribute("lineNumber", "23").reason shouldBe FailureReason.PARSING
    parsing.withAttributes(Map("value" -> "foo")).reason shouldBe FailureReason.PARSING
  }

  test("a message is whatever the reporter interpolated, line breaks and all") {
    // The original parsed its template to find the placeholder for the text of the condition
    // and behaved differently when a line break sat next to it. Nothing inspects a message
    // here, so its layout is carried through untouched.
    val message = "Error on line 23: \n Bad value 'foo'"
    val test: Failure = Failure.Parsing(message)
    test.message shouldBe message
    test.message should include("\n")
    // The value keeps the line break; the rendering of the value does not, because a rendering
    // is read where a line means something. The cases under "the rendering" below pin that.
    Show[Failure].show(test) shouldBe "PARSING: Error on line 23: \\n Bad value 'foo'"
  }

  test("an attribute key given twice keeps the last value, so no key is ever renamed") {
    // Where a template of the original named one placeholder twice, the machinery invented
    // numbered attribute names to keep both values. Attributes are a map here: the last
    // value given for a key survives, and a reporter that wants two values chooses two keys.
    val collided: Failure = Failure
      .Parsing("Error 1 2: Bad value 3")
      .withAttribute("value", "1")
      .withAttribute("value", "3")
    collided.attributes shouldBe SortedMap("value" -> "3")
    val distinct: Failure = Failure
      .Parsing("Error 1 2: Bad value 3")
      .withAttributes(Map("value" -> "1", "value1" -> "2", "value2" -> "3"))
    distinct.attributes.toList shouldBe List("value" -> "1", "value1" -> "2", "value2" -> "3")
  }

  test("a failure is never derived from a thrown condition, only constructed") {
    // The original offered a factory taking a throwable, which unwrapped a failure carried by
    // one of its own exception types and otherwise reported the throwable as an error. No
    // exception type exists in this module and no such factory does either: a step that can
    // fail returns its failure. Escalating a failure to something thrown happens only at the
    // edges of the port, where an effect is run, and never inside the domain modules.
    val test: Failure = Failure.Error("foo")
    test.reason shouldBe FailureReason.ERROR
    test.message shouldBe "foo"
    test.attributes shouldBe SortedMap.empty[String, String]
    assertDoesNotCompile("""Failure.from(new RuntimeException("foo"))""")
    assertDoesNotCompile("""new FailureException(Failure.Invalid("failure"))""")
    assertDoesNotCompile("""new FailureItemException(Failure.Invalid("failure"))""")
  }


  // ===========================================================================
  // Failure - the ten members
  //
  // The reason of a failure is fixed by its class rather than supplied, so the
  // two can never contradict one another. These cases assert that pairing from
  // both directions: from the member to its reason, and from a reason held as a
  // value to the member that carries it.
  // ===========================================================================

  test("each of the ten failures is built from a message alone and reports its matching reason") {
    forAll(failuresByReason) { (failure: Failure, reason: FailureReason) =>
      failure.reason shouldBe reason
      failure.message shouldBe "m"
      failure.attributes shouldBe SortedMap.empty[String, String]
      // The reason is not a constructor parameter, so it cannot be set to disagree with the
      // class, and the rebuilding combinators preserve the class they were called on.
      failure.withAttribute("k", "v").reason shouldBe reason
      failure.mapMessage(_ => "other").reason shouldBe reason
    }
  }

  test("Failure.of maps every reason onto the member that carries it") {
    // Generic code - a lookup, a validating helper, a decoder - receives a reason as a value
    // and cannot name a member, so this is the route it takes. It is total over the family.
    FailureReason.values.toList.foreach { reason =>
      withClue(s"${reason.name}: ") {
        val built = Failure.of(reason, "m")
        built.reason shouldBe reason
        built.message shouldBe "m"
        built.attributes shouldBe SortedMap.empty[String, String]
        val withAttributes = Failure.of(reason, "m", SortedMap("k" -> "v"))
        withAttributes.attributes shouldBe SortedMap("k" -> "v")
        withAttributes.reason shouldBe reason
      }
    }
    // The member returned is the one named for the reason, not merely one that reports it.
    Failure.of(FailureReason.MISSING_DATA, "m") shouldBe Failure.MissingData("m")
    Failure.of(FailureReason.MULTIPLE, "m") shouldBe Failure.Multiple("m")
    Failure.of(FailureReason.CURRENCY_CONVERSION, "m") shouldBe Failure.CurrencyConversion("m")
  }

  test("Show renders the reason, the message, and the attributes in key order") {
    Show[Failure].show(Failure.MissingData("No holiday calendar")) shouldBe
      "MISSING_DATA: No holiday calendar"
    Show[Failure].show(
      Failure.Invalid("Schedule is invalid", SortedMap("definition" -> "P3M from 2024-01-15"))) shouldBe
      "INVALID: Schedule is invalid [definition=P3M from 2024-01-15]"
    // Attributes render in key order whatever order they were supplied in, which is the
    // rendering counterpart of the byte stability asserted for the JSON form below.
    Show[Failure].show(Failure.Other("m", SortedMap("b" -> "2", "a" -> "1"))) shouldBe
      "OTHER: m [a=1, b=2]"
    // There is one rendering and the text form of a failure is it, so a failure written out by
    // a logger, by interpolation or by a debugger reads the same way and is neutralised the
    // same way. The bounding and escaping this brings with it are asserted further below.
    Failure.Invalid("m").toString shouldBe "INVALID: m"
    Failure.Other("m", SortedMap("b" -> "2", "a" -> "1")).toString shouldBe "OTHER: m [a=1, b=2]"
  }

  // ===========================================================================
  // Failure - JSON
  //
  // These four cases are the whole of the serialization coverage for this type
  // and for the reason above: the codecs are declared by the types themselves,
  // because the shared JSON helpers of this module are written in terms of the
  // failure model and cannot also be its source. They are derived when this
  // module is compiled, so nothing on this path inspects a type while the
  // program runs.
  // ===========================================================================

  test("a failure encodes as the single-key object that names its member") {
    (Failure.MissingData("No holiday calendar", SortedMap("id" -> "GBLO")): Failure).asJson.noSpaces shouldBe
      """{"MissingData":{"message":"No holiday calendar","attributes":{"id":"GBLO"}}}"""
    // The attribute object is always written, empty when there are no attributes, so every
    // encoded failure carries both of the fields its member declares.
    failure1.asJson.noSpaces shouldBe """{"Invalid":{"message":"invalid","attributes":{}}}"""
    // The reason is not written: the member name already determines it, and writing both
    // would allow an encoded failure to disagree with itself.
    failure1.asJson.noSpaces should not include "INVALID"
  }

  test("any failure round-trips through its JSON form") {
    forAll { (failure: Failure) =>
      decode[Failure](failure.asJson.noSpaces) shouldBe Right(failure)
    }
  }

  test("equal failures encode to identical bytes whatever order their attributes were added in") {
    val built: Failure = Failure
      .Other("m")
      .withAttribute("b", "2")
      .withAttribute("a", "1")
    val reversed: Failure = Failure
      .Other("m")
      .withAttribute("a", "1")
      .withAttribute("b", "2")
    val literal: Failure = Failure.Other("m", SortedMap("a" -> "1", "b" -> "2"))
    built shouldBe reversed
    built shouldBe literal
    built.asJson.noSpaces shouldBe reversed.asJson.noSpaces
    built.asJson.noSpaces shouldBe literal.asJson.noSpaces
    built.asJson.noSpaces shouldBe """{"Other":{"message":"m","attributes":{"a":"1","b":"2"}}}"""
    forAll { (failure: Failure) =>
      // Restating the attributes of a generated failure entry by entry cannot change its
      // encoding, whatever order the entries are visited in.
      val rebuilt = failure.attributes.toList.reverse.foldLeft(failure.mapMessage(identity)) {
        case (acc, (key, value)) => acc.withAttribute(key, value)
      }
      rebuilt.asJson.noSpaces shouldBe failure.asJson.noSpaces
    }
  }

  test("the JSON decoder requires the attributes field and rejects an unknown member") {
    decode[Failure]("""{"Invalid":{"message":"m","attributes":{}}}""") shouldBe Right(Failure.Invalid("m"))
    // A derived decoder reads the fields a member declares and does not consult the default
    // value declared for the attributes, so the field has to be present. The encoder always
    // writes it, which is what makes the round-trip above total.
    decode[Failure]("""{"Invalid":{"message":"m"}}""").isLeft shouldBe true
    // Only the ten members decode: the closed set of the type is enforced on the way in as
    // well as on the way out.
    decode[Failure]("""{"Rubbish":{"message":"m","attributes":{}}}""").isLeft shouldBe true
    decode[Failure](""""INVALID: m"""").isLeft shouldBe true
    decode[Failure]("{}").isLeft shouldBe true
    decode[Failure]("42").isLeft shouldBe true
    // A document naming two members is not rejected: the derived decoder identifies a
    // member by the key it finds and reads the first one that names a member, ignoring the
    // rest. That is a property of the derivation rather than a decision of this port, and it
    // is recorded here because no encoder of this port can produce such a document - every
    // failure encodes as exactly one key - so the case is reachable only from text written
    // by hand.
    decode[Failure](
      """{"Invalid":{"message":"m","attributes":{}},"Other":{"message":"other","attributes":{}}}""") shouldBe
      Right(Failure.Invalid("m"))
  }


  // ===========================================================================
  // Chains of failures
  //
  // The aggregate type of the original, and the builder that assembled one, have
  // no counterpart here: an operation that can fail in more than one way reports
  // a `NonEmptyChain[Failure]`, and each failure in it keeps its own reason,
  // message and attributes. The first eight cases below are the eight cases of
  // the aggregate test class, re-expressed against that chain; the four that
  // follow them restate four of those cases over generated chains, and the
  // comment above them says which and why.
  //
  // Two operations must not be confused, and both are asserted here.
  // Concatenation keeps every failure, duplicates included, in order; collapsing
  // - the next section - folds equal failures together before joining what is
  // left.
  // ===========================================================================

  test("a chain of failures cannot be empty") {
    // The original had an empty instance and a predicate to test for it. Emptiness is not
    // representable here, which is the point of the type: there is no empty chain to check
    // for, and the absence of failures is the absence of a chain.
    NonEmptyChain.fromChain(Chain.empty[Failure]) shouldBe None
    NonEmptyChain.fromSeq(List.empty[Failure]) shouldBe None
    NonEmptyChain.fromChain(Chain.one(failure1)).map(_.toChain.toList) shouldBe Some(List(failure1))
    // Collapsing therefore needs no case for an empty input: the type will not admit one.
    assertDoesNotCompile("""Failure.collapse(Chain.empty[Failure])""")
    assertDoesNotCompile("""Failure.collapse(List.empty[Failure])""")
  }

  test("a chain built from failures given one at a time holds them in order") {
    val test = NonEmptyChain.of(failure1, failure2)
    test.toChain.toList shouldBe List(failure1, failure2)
    test.head shouldBe failure1
    test.length shouldBe 2L
  }

  test("a chain built from a list of failures holds them in order") {
    NonEmptyChain.fromSeq(List(failure1, failure2)).map(_.toChain.toList) shouldBe
      Some(List(failure1, failure2))
    NonEmptyChain.fromChain(Chain.fromSeq(List(failure1, failure2))).map(_.toChain.toList) shouldBe
      Some(List(failure1, failure2))
    // Building from a list yields the same chain as naming the failures one at a time.
    NonEmptyChain.fromSeq(List(failure1, failure2)) shouldBe Some(pair)
  }

  test("failures are gathered by construction rather than through a builder") {
    // The builder of the original accumulated failures one call at a time into a mutable
    // holder. The chain is immutable and its additions return new chains, so the same
    // accumulation is a fold - or, for two known failures, one expression.
    NonEmptyChain.one(failure1).append(failure2) shouldBe pair
    List(failure2).foldLeft(NonEmptyChain.one(failure1)) { (accumulated, failure) =>
      accumulated.append(failure)
    } shouldBe pair
    assertDoesNotCompile("FailureItems.builder()")
  }

  test("a chain is extended with several failures at once by concatenation") {
    // The bulk form of the builder of the original; the chain does it with one call, and
    // extending by nothing leaves the chain as it was.
    NonEmptyChain.one(failure1).appendChain(Chain(failure2)) shouldBe pair
    pair.appendChain(Chain.empty[Failure]) shouldBe pair
    NonEmptyChain.one(failure1).appendChain(Chain(failure2, failure1)).toChain.toList shouldBe
      List(failure1, failure2, failure1)
  }

  test("concatenating chains keeps every failure, duplicates included, in order") {
    (pair ++ pair).toChain.toList shouldBe List(failure1, failure2, failure1, failure2)
    (pair ++ NonEmptyChain.one(failure1)).toChain.toList shouldBe List(failure1, failure2, failure1)
    // Concatenating what an empty source would have produced is a no-op, expressed through
    // the option that stands in for an empty chain rather than through a chain that cannot
    // exist.
    val nothingToAdd: Option[NonEmptyChain[Failure]] = NonEmptyChain.fromChain(Chain.empty[Failure])
    nothingToAdd.fold(pair)(other => pair ++ other) shouldBe pair
    // This is where concatenation and collapsing part company: the duplicates kept here are
    // folded together by `collapse`, asserted in the next section.
    (pair ++ pair).length shouldBe 4L
    Failure.collapse(pair ++ pair) shouldBe Failure.collapse(pair)
  }

  test("two chains holding the same failures in the same order are equal and hash alike") {
    // This replaces the reflective bean sweep with which the aggregate test class closed:
    // there is no bean to walk, and the property worth asserting in its place is that a
    // chain of failures is an ordinary value, comparable by what it holds.
    NonEmptyChain.of(failure1, failure2) shouldBe pair
    NonEmptyChain.of(failure1, failure2).hashCode shouldBe pair.hashCode
    // Order is part of the value, so the reversed chain is a different chain, even though
    // collapsing either yields the same reason.
    NonEmptyChain.of(failure2, failure1) should not be pair
    Hash[Failure].eqv(failure1, Failure.Invalid("invalid")) shouldBe true
    Hash[Failure].eqv(failure1, failure2) shouldBe false
  }

  test("a chain reaches JSON by being collapsed to the single failure that describes it") {
    // The aggregate type of the original was serializable in its own right. The chain is a
    // container from the effect library and is not a domain type, so it carries no codec of
    // this port's making; a chain that has to be written out is collapsed first, and the
    // failure that results is what serializes.
    val collapsed: Failure = Failure.collapse(pair)
    collapsed.asJson.noSpaces shouldBe
      """{"Multiple":{"message":"invalid, data","attributes":{}}}"""
    decode[Failure](collapsed.asJson.noSpaces) shouldBe Right(collapsed)
  }

  // ---------------------------------------------------------------------------
  // The same four contracts over generated chains.
  //
  // The four cases above state them at the pair of failures the aggregate test
  // class being ported used. That class closed with a reflective sweep of the
  // bean, which reached every shape of aggregate the type admitted rather than
  // the one an example names, so the same contracts are stated again over the
  // shared generator of chains - equality and hashing, sensitivity to order,
  // concatenation against decomposition, and the collapse that is a chain's only
  // route to JSON.
  //
  // Every chain here is ascribed to `NonEmptyChain[Failure]`. A chain left to
  // inference takes the element type `Failure with Product with Serializable`,
  // for which the shared shrinking does not apply, and a counterexample would
  // then be reported at the size it was generated at rather than minimised.
  // ---------------------------------------------------------------------------

  test("any chain rebuilt from its own failures, in their order, is equal to it and hashes alike") {
    forAll { (failures: NonEmptyChain[Failure]) =>
      val elements: List[Failure] = failures.toChain.toList
      val rebuilt: NonEmptyChain[Failure] = NonEmptyChain.of(elements.head, elements.tail: _*)
      rebuilt shouldBe failures
      rebuilt.hashCode shouldBe failures.hashCode

      // The other two factories answer the same chain from the same failures, so how a chain
      // was assembled leaves no trace in the value - which is what makes a chain comparable
      // by what it holds, and what the bean sweep of the original established by walking it.
      NonEmptyChain.fromSeq(elements) shouldBe Some(failures)
      NonEmptyChain.fromChain(failures.toChain) shouldBe Some(failures)

      // The instances agree with `==`. `Hash[Failure]` is universal equality, so the chain
      // instances the effect library derives from it are the structural equality and the
      // structural hash of the chain, and a spec may use either form.
      val equality = Eq[NonEmptyChain[Failure]]
      val hashing = Hash[NonEmptyChain[Failure]]
      equality.eqv(rebuilt, failures) shouldBe true
      equality.eqv(rebuilt, failures) shouldBe (rebuilt == failures)
      hashing.hash(rebuilt) shouldBe hashing.hash(failures)
    }
  }

  test("reversing a chain of two or more distinct failures yields a different chain") {
    forAll { (failures: NonEmptyChain[Failure]) =>
      val elements: List[Failure] = failures.toChain.toList
      val reversed: NonEmptyChain[Failure] = failures.reverse

      // Reversing holds the same failures in the opposite order, and doing it twice is the
      // identity - true of every chain, whatever it holds.
      reversed.toChain.toList shouldBe elements.reverse
      reversed.reverse shouldBe failures

      // Order is part of the value, so a chain that reads differently backwards is a different
      // chain. The guard excludes the two arrangements for which that statement is false
      // rather than letting them falsify it: a chain of one failure, and a chain that reads
      // the same in both directions. The generator produces both, since it draws one to four
      // failures from a small pool of messages, so they are guarded rather than assumed away.
      whenever(elements != elements.reverse) {
        reversed should not be failures
        Eq[NonEmptyChain[Failure]].eqv(reversed, failures) shouldBe false

        // What order does not change is the reason the chain collapses to: collapsing folds
        // equal failures together and then asks whether one reason is common to what is left,
        // and reversing the chain changes neither the failures that remain nor their reasons.
        Failure.collapse(reversed).reason shouldBe Failure.collapse(failures).reason
      }
    }
  }

  test("reading any chain out and rebuilding it, and concatenating two chains, keep every failure in order") {
    forAll { (left: NonEmptyChain[Failure], right: NonEmptyChain[Failure]) =>
      val leftElements: List[Failure] = left.toChain.toList
      val rightElements: List[Failure] = right.toChain.toList

      // Decomposition and construction are inverse: reading a chain out to a list and
      // rebuilding it from that list yields the chain it came from, by either factory.
      NonEmptyChain.fromSeq(leftElements) shouldBe Some(left)
      NonEmptyChain.fromChain(Chain.fromSeq(leftElements)) shouldBe Some(left)

      // Concatenation lays the second chain after the first and folds nothing together: the
      // elements of `a ++ b` are the elements of `a` followed by those of `b`, duplicates and
      // order included. This is the distinction from `collapse` that the next section turns on.
      val joined: NonEmptyChain[Failure] = left ++ right
      joined.toChain.toList shouldBe leftElements ::: rightElements
      joined.length shouldBe left.length + right.length
      joined.head shouldBe left.head

      // A chain concatenated with itself therefore holds every failure twice, however many
      // of its failures are already equal to one another.
      val doubled: NonEmptyChain[Failure] = left ++ left
      doubled.toChain.toList shouldBe leftElements ::: leftElements
      doubled.length shouldBe left.length + left.length
    }
  }

  test("collapsing any chain round-trips through JSON, and a doubled chain collapses to the same failure") {
    forAll { (failures: NonEmptyChain[Failure]) =>
      val collapsed: Failure = Failure.collapse(failures)

      // Collapsing is a chain's only route to JSON, so the two have to agree at every chain
      // and not only at the pair above: the wire form is the single-key object that names the
      // member the reason chose, and reading it back gives the failure that was written.
      collapsed.asJson.asObject.map(fields => fields.keys.size) shouldBe Some(1)
      decode[Failure](collapsed.asJson.noSpaces) shouldBe Right(collapsed)
      decode[Failure](collapsed.asJson.noSpaces).map(failure => failure.reason) shouldBe
        Right(collapsed.reason)

      // Collapsing a chain concatenated with itself gives the same failure as collapsing it
      // once, because equal failures fold together: the duplicates concatenation keeps are
      // exactly the ones the collapse removes, so the wire form is stable under them too.
      Failure.collapse(failures ++ failures) shouldBe collapsed
      Failure.collapse(failures ++ failures).asJson.noSpaces shouldBe collapsed.asJson.noSpaces
    }
  }

  // ===========================================================================
  // Failure.collapse
  //
  // The contract is the one the aggregating factory of the original followed:
  // the input is non-empty, equal failures are folded together, the messages of
  // what remains are joined with ", ", and the reason is the common reason of
  // them all or `MULTIPLE` where they differ. Attributes, which the original had
  // no rule for at this level, are merged left to right as `withAttributes`
  // merges them.
  // ===========================================================================

  test("collapsing failures that agree on a reason keeps it and joins their messages") {
    val test = Failure.collapse(
      NonEmptyChain.of(
        Failure.MissingData("message 1"),
        Failure.MissingData("message 2"),
        Failure.MissingData("message 3")))
    test.reason shouldBe FailureReason.MISSING_DATA
    test.message shouldBe "message 1, message 2, message 3"
    test shouldBe Failure.MissingData("message 1, message 2, message 3")
  }

  test("collapsing failures that disagree on a reason yields MULTIPLE and joins their messages") {
    val test = Failure.collapse(
      NonEmptyChain.of(
        Failure.MissingData("message 1"),
        Failure.CalculationFailed("message 2"),
        Failure.Error("message 3")))
    test.reason shouldBe FailureReason.MULTIPLE
    test.message shouldBe "message 1, message 2, message 3"
    // Collapsing is the only thing that produces `MULTIPLE` from other failures, and the
    // failures it summarised keep their own reasons - they are not rewritten.
    test shouldBe Failure.Multiple("message 1, message 2, message 3")
  }

  test("collapsing a single failure yields that failure unchanged") {
    Failure.collapse(NonEmptyChain.one(failure2)) shouldBe failure2
    Failure.collapse(NonEmptyChain.one(failure2)).reason shouldBe FailureReason.MISSING_DATA
    Failure.collapse(NonEmptyChain.one(failure2)).message shouldBe "data"
    // A failure reported on its own therefore never becomes a `MULTIPLE`.
    val withAttributes: Failure = Failure.Invalid("m", SortedMap("k" -> "v"))
    Failure.collapse(NonEmptyChain.one(withAttributes)) shouldBe withAttributes
  }

  test("collapsing failures that all report MULTIPLE keeps MULTIPLE") {
    // `MULTIPLE` is the common reason of this input, not a disagreement, so it survives for
    // the same reason any other shared reason does.
    val test = Failure.collapse(NonEmptyChain.of(Failure.Multiple("a"), Failure.Multiple("b")))
    test.reason shouldBe FailureReason.MULTIPLE
    test.message shouldBe "a, b"
  }

  test("collapsing folds equal failures together, so a cause reported twice is described once") {
    // The aggregating factory of the original held its items in a set, so the same failure
    // arriving twice was described once. The chain keeps both, and collapsing restores that
    // behaviour: the joined message names the cause once rather than repeating it.
    val repeated: Failure = Failure.MissingData("failure")
    val test = Failure.collapse(NonEmptyChain.of(repeated, repeated))
    test shouldBe repeated
    test.reason shouldBe FailureReason.MISSING_DATA
    test.message shouldBe "failure"
    test.message should not be "failure, failure"
    // Folding is by equality, so failures differing in any part are all kept.
    Failure
      .collapse(NonEmptyChain.of(repeated, Failure.MissingData("failure "), repeated))
      .message shouldBe "failure, failure "
    Failure
      .collapse(NonEmptyChain.of(repeated, repeated.withAttribute("k", "v")))
      .message shouldBe "failure, failure"
  }

  test("collapsing merges the attributes of every failure, letting the later value win") {
    val test = Failure.collapse(
      NonEmptyChain.of(
        Failure.Invalid("a", SortedMap("k" -> "1", "left" -> "yes")),
        Failure.Invalid("b", SortedMap("k" -> "2", "right" -> "yes"))))
    test.reason shouldBe FailureReason.INVALID
    test.message shouldBe "a, b"
    test.attributes shouldBe SortedMap("k" -> "2", "left" -> "yes", "right" -> "yes")
  }

  test("collapsing a one-failure chain is the identity, for any failure") {
    forAll { (failure: Failure) =>
      Failure.collapse(NonEmptyChain.one(failure)) shouldBe failure
    }
  }

  test("collapsing a chain whose failures agree on a reason preserves that reason") {
    forAll(genChainSharingAReason) { (failures: NonEmptyChain[Failure]) =>
      val collapsed = Failure.collapse(failures)
      collapsed.reason shouldBe failures.head.reason
      // Every message of the chain is present in the joined message, and the joined message
      // is never empty because no generated message is.
      failures.toNonEmptyList.toList.foreach(failure => collapsed.message should include(failure.message))
      collapsed.message should not be empty
    }
  }

  test("collapsing any chain reports MULTIPLE exactly when its failures disagree") {
    forAll { (failures: NonEmptyChain[Failure]) =>
      val collapsed = Failure.collapse(failures)
      val distinctReasons = failures.toNonEmptyList.toList.distinct.map(_.reason).distinct
      if (distinctReasons.size == 1) {
        collapsed.reason shouldBe distinctReasons.head
      } else {
        collapsed.reason shouldBe FailureReason.MULTIPLE
      }
      // The result is always one of the ten members, whatever went into it.
      FailureReason.values.toList should contain(collapsed.reason)
    }
  }

  // ===========================================================================
  // Failure - the rendering
  //
  // A failure carries the text it was built with, whole: the message names the
  // value that was rejected as that value stands, exactly as the library being
  // ported did, and `message` and `attributes` hand it back unchanged to code
  // that acts on a failure rather than reads one.
  //
  // Writing a failure out is therefore where text that reached the library from
  // outside it is made safe, and these cases pin that boundary. Every part of a
  // rendering - the message, and the key and the value of each attribute - is
  // bounded in length and holds no character a line-oriented reader could act
  // on, so a value supplied from outside can neither forge a line of a log that
  // holds the failure (CWE-117) nor make that line as large as itself. There is
  // one rendering, and the text form of a failure is it, so no path writes a
  // failure out unneutralised.
  // ===========================================================================

  /** U+2028 LINE SEPARATOR, held by its code point so that no escape appears in this source. */
  private val lineSeparator: Char = 0x2028.toChar

  /** U+2029 PARAGRAPH SEPARATOR, held the same way. */
  private val paragraphSeparator: Char = 0x2029.toChar

  /**
   * The bound the rendering applies to one part of a failure.
   *
   * The constant itself is private to [[Failure]]: the number is the renderer's own, and
   * nothing outside that file chooses or adjusts it, which is why the renderer takes only the
   * text it is to write. It is restated here because the cases below pin the behaviour at the
   * boundary - a part of exactly this length written whole, one character more truncated and
   * marked - which is what makes the bound a contract rather than an implementation detail
   * that may drift.
   */
  private val MaxRenderedPart: Int = 512

  /**
   * The surrogates of the text that are not one half of a pair.
   *
   * A character outside the basic multilingual plane is two chars in a Java string, so text
   * truncated a char at a time could be cut through the middle of one, leaving half a
   * character in a rendering. This is how the cases below state that the rendering never does
   * that: the list is empty for text in which every surrogate has its partner.
   *
   * @param text  the text to examine
   * @return the surrogates standing on their own, in the order they appear
   */
  private def unpairedSurrogates(text: String): List[Char] =
    text.toList.zipWithIndex.collect {
      case (character, index)
          if character.isHighSurrogate &&
            !(index + 1 < text.length && text.charAt(index + 1).isLowSurrogate) =>
        character
      case (character, index)
          if character.isLowSurrogate && !(index > 0 && text.charAt(index - 1).isHighSurrogate) =>
        character
    }

  test("a failure carries the message it was built with, whole and unrendered") {
    // The property the messages of this library rest on: a rejected value is named as it
    // stands, so a caller correcting its input is handed back exactly what was refused, and a
    // test or a decoder comparing failures compares the values rather than renderings of them.
    val payload = "H" * 10000
    val large: Failure = Failure.Parsing(s"Unable to parse tenor: '$payload'")
    large.message shouldBe s"Unable to parse tenor: '$payload'"
    large.message.length shouldBe "Unable to parse tenor: ''".length + 10000

    val textWithALineBreak = "Error on line 23: \n Bad value 'foo'"
    val verbatim: Failure = Failure.Parsing(textWithALineBreak)
    verbatim.message shouldBe textWithALineBreak
    verbatim.message should include("\n")

    // Attributes are carried the same way, whatever they hold, and adding one leaves the
    // message alone.
    val attributed = verbatim.withAttribute("definition", s"P3M\n$payload")
    attributed.attributes shouldBe SortedMap("definition" -> s"P3M\n$payload")
    attributed.message shouldBe textWithALineBreak
  }

  test("the rendering escapes every character of a message that could forge a line") {
    // The characters with no short escape are written by code point rather than as source
    // escapes, so that this file holds no escape sequence of its own to misread.
    val nul = 0x0000.toChar
    val escape = 0x001b.toChar
    val delete = 0x007f.toChar
    val escaped = Table(
      ("description", "text", "rendering"),
      ("a line feed", "3M\nINJECTED", "3M\\nINJECTED"),
      ("a carriage return", "3M\rINJECTED", "3M\\rINJECTED"),
      ("a tab", "3M\tINJECTED", "3M\\tINJECTED"),
      ("a null", s"3M${nul}INJECTED", "3M\\u0000INJECTED"),
      ("an escape", s"3M${escape}INJECTED", "3M\\u001bINJECTED"),
      ("a delete", s"3M${delete}INJECTED", "3M\\u007fINJECTED"),
      ("a line separator", s"3M${lineSeparator}INJECTED", "3M\\u2028INJECTED"),
      ("a paragraph separator", s"3M${paragraphSeparator}INJECTED", "3M\\u2029INJECTED"))
    forAll(escaped) { (description: String, text: String, rendering: String) =>
      withClue(s"$description: ") {
        val failure: Failure = Failure.Parsing(s"Tenor name not found: $text")
        // The failure holds the character; the rendering does not, which is the property that
        // matters: whatever a reader of the rendering does with lines, it is handed one line,
        // so it cannot be made to record a line the library did not report.
        failure.message should include(text)
        Show[Failure].show(failure) shouldBe s"PARSING: Tenor name not found: $rendering"
        Show[Failure].show(failure).exists(character =>
          character.isControl ||
            character == lineSeparator ||
            character == paragraphSeparator) shouldBe false
      }
    }
    // Only the characters a reader could act on are escaped. A backslash the caller supplied
    // is carried as it stands and is not doubled, which is why a rendering is written to be
    // read rather than to be parsed back: text holding the two characters of an escape renders
    // the same way the character it names does.
    Show[Failure].show(Failure.Parsing("C:\\rates\\3M")) shouldBe "PARSING: C:\\rates\\3M"
  }

  test("the rendering escapes the key and the value of every attribute") {
    // The other half of a rendering, and the one a schedule, a calendar or a reference-data
    // identifier reaches: the definition of a rejected schedule and the name of a missing
    // calendar are attached as attributes, and both are built from text a caller supplied.
    val injected = Failure
      .Invalid("Schedule is invalid")
      .withAttribute("definition", "startDate=2024-01-15\nINVALID: forged")
      .withAttribute("id\nkey", "GBLO\rUSNY")
    injected.attributes shouldBe
      SortedMap(
        "definition" -> "startDate=2024-01-15\nINVALID: forged",
        "id\nkey" -> "GBLO\rUSNY")
    Show[Failure].show(injected) shouldBe
      "INVALID: Schedule is invalid " +
        "[definition=startDate=2024-01-15\\nINVALID: forged, id\\nkey=GBLO\\rUSNY]"
    Show[Failure].show(injected).exists(_.isControl) shouldBe false
  }

  test("the rendering writes a part of the bounded length whole and unmarked") {
    // The bound is inclusive, so nothing about a failure whose parts fit reveals that a bound
    // exists at all - which is what keeps every ordinary rendering the text its reporter wrote.
    val atBound = "A" * MaxRenderedPart
    Show[Failure].show(Failure.Parsing(atBound)) shouldBe s"PARSING: $atBound"
    Show[Failure].show(Failure.Parsing(atBound)) should not include "..."
    val attributed = Failure.Invalid("m").withAttribute("definition", atBound)
    Show[Failure].show(attributed) shouldBe s"INVALID: m [definition=$atBound]"
    attributed.attributes("definition").length shouldBe MaxRenderedPart
  }

  test("the rendering truncates a part one character past the bound and marks what it left out") {
    val pastBound = "A" * (MaxRenderedPart + 1)
    val rendered = Show[Failure].show(Failure.Parsing(pastBound))
    rendered shouldBe s"PARSING: ${"A" * MaxRenderedPart}..."
    rendered should endWith("...")
    // What survives is the beginning of the part, which is what identifies it to whoever has
    // to correct it, and the marker states that there was more.
    rendered.drop("PARSING: ".length).take(MaxRenderedPart) shouldBe pastBound.take(MaxRenderedPart)
    // The failure itself is unchanged: truncation is an act of writing it out.
    Failure.Parsing(pastBound).message shouldBe pastBound
  }

  test("no value a caller supplies can make a rendered failure large") {
    // The finding this case pins: a ten-thousand-character value reaching a message once
    // reached a log line of the same size, in the message and again in the attribute. Each
    // part of the rendering is now bounded, so the line is bounded whatever arrives.
    val payload = "A" * 10000
    val failure = Failure
      .Parsing(s"Unable to parse tenor: '$payload'")
      .withAttribute("definition", payload)
      .withAttribute(payload, "value")
    val rendered = Show[Failure].show(failure)
    // Three parts are rendered, each bounded by the same amount, plus the fixed text of the
    // rendering itself - where the failure holds thirty thousand characters of payload.
    val fixedText = "PARSING: Unable to parse tenor: '' [definition=, =value]"
    rendered.length should be <= 3 * (MaxRenderedPart + 3) + fixedText.length
    rendered.length should be < 2000
    val quoted = "Unable to parse tenor: '"
    rendered should include(s"$quoted${"A" * (MaxRenderedPart - quoted.length)}...")
    failure.message.length should be > 10000
    failure.attributes("definition").length shouldBe 10000
  }

  test("the rendering keeps a surrogate pair whole, so truncation leaves no half character") {
    // A character outside the basic plane, held as the surrogate pair a Java string represents
    // it with, and built from its code point so that this file holds no escape of its own.
    val emoji: String = new String(Character.toChars(0x1f600))
    emoji.length shouldBe 2
    Show[Failure].show(Failure.Parsing(emoji * 3)) shouldBe s"PARSING: ${emoji * 3}"
    unpairedSurrogates(Show[Failure].show(Failure.Parsing(emoji * 3))) shouldBe empty
    // A run of emoji past the bound is cut between two of them and never through one, because
    // the pair is taken as a single unit.
    val truncated = Show[Failure].show(Failure.Parsing(emoji * (MaxRenderedPart + 100)))
    truncated shouldBe s"PARSING: ${emoji * (MaxRenderedPart / 2)}..."
    unpairedSurrogates(truncated) shouldBe empty
    // A pair that would straddle the bound is left out entirely rather than halved, so a
    // rendering can stop one character short of the bound; that is the cost of never
    // producing half a character.
    val straddling = Show[Failure].show(Failure.Parsing("A" * (MaxRenderedPart - 1) + emoji))
    straddling shouldBe s"PARSING: ${"A" * (MaxRenderedPart - 1)}..."
    unpairedSurrogates(straddling) shouldBe empty
    // A surrogate that arrived on its own is half a character and names no text, so it is
    // escaped like any other character a reader cannot be handed.
    Show[Failure].show(Failure.Parsing(s"3M${0xd83d.toChar}")) shouldBe "PARSING: 3M\\ud83d"
    Show[Failure].show(Failure.Parsing(s"${0xde00.toChar}3M")) shouldBe "PARSING: \\ude003M"
  }

  test("an ordinary failure renders as the text its reporter wrote, character for character") {
    // Punctuation, accented letters and CJK are characters like any other and are left alone,
    // so a failure quoting text from a document in any language still quotes it; and this is
    // why applying the rendering cost the wording of no message in this port.
    Show[Failure].show(Failure.Parsing("Currency name not found: Rubbish")) shouldBe
      "PARSING: Currency name not found: Rubbish"
    Show[Failure].show(Failure.Parsing("café, 東京 - 3M/6M (P1Y)")) shouldBe
      "PARSING: café, 東京 - 3M/6M (P1Y)"
    // Empty and blank text are text: they are carried through rather than trimmed, so a
    // rendering shows that something blank arrived rather than that nothing did.
    Show[Failure].show(Failure.Parsing("")) shouldBe "PARSING: "
    Show[Failure].show(Failure.Invalid("m").withAttribute("definition", "   ")) shouldBe
      "INVALID: m [definition=   ]"
  }

  test("the rendering of any failure is bounded, single-line and never half a character") {
    // The generator draws from what a payload actually holds - letters, the punctuation of a
    // tenor or an index name, every kind of control character, the two Unicode separators and
    // lone surrogates - and from lengths on both sides of the bound, so the property covers
    // parts that are truncated as well as parts carried whole.
    val genCharacter: Gen[Char] = Gen.frequency(
      (6, Gen.alphaNumChar),
      (2, Gen.oneOf(' ', '-', '\'', '/', ':', '.', '\\')),
      (2, Gen.oneOf('\n', '\r', '\t', 0x0000.toChar, 0x001b.toChar, 0x007f.toChar)),
      (1, Gen.oneOf(lineSeparator, paragraphSeparator)),
      (1, Gen.choose(0xd800, 0xdfff).map(_.toChar)),
      (1, Gen.choose(0x0080, 0xffff).map(_.toChar)))
    val genText: Gen[String] = Gen
      .choose(0, 2 * MaxRenderedPart)
      .flatMap(length => Gen.listOfN(length, genCharacter).map(_.mkString))
    forAll(genText, genText) { (message: String, attribute: String) =>
      val failure = Failure.Parsing(message).withAttribute("definition", attribute)
      val rendered = Show[Failure].show(failure)
      rendered.length should be <= "PARSING: ".length + (MaxRenderedPart + 3) +
        " [definition=]".length + (MaxRenderedPart + 3)
      rendered.find(_.isControl) shouldBe None
      rendered.find(character => character == lineSeparator || character == paragraphSeparator) shouldBe None
      unpairedSurrogates(rendered) shouldBe empty
      // And the failure still holds what it was built with, however it rendered.
      failure.message shouldBe message
      failure.attributes("definition") shouldBe attribute
    }
    // The same properties over the failures the shared generators produce, whose messages and
    // attributes are arbitrary rather than drawn from a payload alphabet.
    forAll { (failure: Failure) =>
      val rendered = Show[Failure].show(failure)
      rendered.find(_.isControl) shouldBe None
      unpairedSurrogates(rendered) shouldBe empty
    }
  }

  test("the text form of a failure is the rendering, so writing one out cannot bypass it") {
    // Interpolating a failure, handing one to a logger that calls `toString`, or printing one
    // all go through the same neutralised form, which is what makes the property above hold of
    // every path rather than of the instance alone.
    val injected = Failure
      .MissingData("No holiday calendar: GBXX\nINJECTED")
      .withAttribute("id", "GBXX\nINJECTED")
    injected.toString shouldBe Show[Failure].show(injected)
    s"$injected" shouldBe Show[Failure].show(injected)
    String.valueOf(injected: Any) shouldBe Show[Failure].show(injected)
    injected.toString should not include "\n"
    injected.toString shouldBe
      "MISSING_DATA: No holiday calendar: GBXX\\nINJECTED [id=GBXX\\nINJECTED]"
    // The fields are what a caller reads when it wants the value rather than the reading of it.
    injected.message shouldBe "No holiday calendar: GBXX\nINJECTED"
    injected.attributes("id") shouldBe "GBXX\nINJECTED"
  }

  // ===========================================================================
  // Failure.renderDiagnostic - the renderer as a contract of its own
  //
  // The rendering above is not only reached through a failure: text on its way
  // to a line-oriented sink reaches it from the JSON decoder bridge, from the
  // resource reader naming a source it could not read, and from the text form
  // of an identifier a caller supplied. Those callers are outside this file, so
  // the renderer is a published member and these cases pin it as one: what it
  // does to ordinary text, to each character that could forge a line, to a
  // character held as a surrogate pair, and at the bound. The last case ties it
  // to `Show[Failure]`, so the rendering a failure receives and the rendering
  // every other caller receives cannot come apart.
  // ===========================================================================

  test("Failure.renderDiagnostic writes ordinary text as itself, character for character") {
    // The property every other consumer of the renderer depends on: applying it costs the
    // wording of an ordinary diagnostic nothing, so a decoder error, a resource name or an
    // identifier reads exactly as the code that wrote it meant.
    Failure.renderDiagnostic("Currency name not found: Rubbish") shouldBe
      "Currency name not found: Rubbish"
    Failure.renderDiagnostic("café, 東京 - 3M/6M (P1Y)") shouldBe "café, 東京 - 3M/6M (P1Y)"
    Failure.renderDiagnostic("") shouldBe ""
    Failure.renderDiagnostic("   ") shouldBe "   "
    Failure.renderDiagnostic("C:\\rates\\3M") shouldBe "C:\\rates\\3M"
    val atBound = "A" * MaxRenderedPart
    Failure.renderDiagnostic(atBound) shouldBe atBound
    Failure.renderDiagnostic(atBound) should not include "..."
  }

  test("Failure.renderDiagnostic escapes every character of any text that could forge a line") {
    // The characters with no short escape are written by code point rather than as source
    // escapes, so that this case holds no escape sequence of its own to misread. A bell is
    // there as an ISO control that is neither one of the three with a short escape nor one of
    // the two the earlier cases already name.
    val bell = 0x0007.toChar
    val escaped = Table(
      ("description", "text", "rendering"),
      ("a line feed", "GBLO\nINJECTED", "GBLO\\nINJECTED"),
      ("a carriage return", "GBLO\rINJECTED", "GBLO\\rINJECTED"),
      ("a tab", "GBLO\tINJECTED", "GBLO\\tINJECTED"),
      ("a bell", s"GBLO${bell}INJECTED", "GBLO\\u0007INJECTED"),
      ("a line separator", s"GBLO${lineSeparator}INJECTED", "GBLO\\u2028INJECTED"),
      ("a paragraph separator", s"GBLO${paragraphSeparator}INJECTED", "GBLO\\u2029INJECTED"),
      ("a lone high surrogate", s"GBLO${0xd83d.toChar}", "GBLO\\ud83d"),
      ("a lone low surrogate", s"${0xde00.toChar}GBLO", "\\ude00GBLO"))
    forAll(escaped) { (description: String, text: String, rendering: String) =>
      withClue(s"$description: ") {
        Failure.renderDiagnostic(text) shouldBe rendering
        Failure.renderDiagnostic(text).exists(character =>
          character.isControl ||
            character == lineSeparator ||
            character == paragraphSeparator) shouldBe false
        unpairedSurrogates(Failure.renderDiagnostic(text)) shouldBe empty
      }
    }
  }

  test("Failure.renderDiagnostic keeps a surrogate pair whole and bounds what it writes") {
    // A character outside the basic plane is one unit to the renderer, so it survives whole
    // and truncation never leaves half of one behind.
    val emoji: String = new String(Character.toChars(0x1f600))
    Failure.renderDiagnostic(emoji * 3) shouldBe emoji * 3
    unpairedSurrogates(Failure.renderDiagnostic(emoji * 3)) shouldBe empty
    Failure.renderDiagnostic(emoji * (MaxRenderedPart + 100)) shouldBe
      s"${emoji * (MaxRenderedPart / 2)}..."
    unpairedSurrogates(Failure.renderDiagnostic(emoji * (MaxRenderedPart + 100))) shouldBe empty

    // Past the bound the text is cut and the three characters of the marker state that there
    // was more, so no value a caller supplies can make a diagnostic large - the property the
    // JSON bridge and the resource reader rely on as much as a failure does.
    val pastBound = "A" * (MaxRenderedPart + 1)
    Failure.renderDiagnostic(pastBound) shouldBe s"${"A" * MaxRenderedPart}..."
    Failure.renderDiagnostic("A" * 10000) shouldBe s"${"A" * MaxRenderedPart}..."
    Failure.renderDiagnostic("A" * 10000).length shouldBe MaxRenderedPart + 3
    Failure.renderDiagnostic(s"Unable to parse tenor: '${"A" * 10000}'").length should be <=
      MaxRenderedPart + 3
  }

  test("Failure.renderDiagnostic renders exactly what the rendering of a failure renders") {
    // One renderer, so the text a failure is written out with and the text every other
    // consumer receives are the same text. Asserting the equality rather than the two results
    // separately is what makes drift between them impossible.
    val shaped = Table(
      "text",
      "Currency name not found: Rubbish",
      "Tenor name not found: 3M\nINJECTED: forged",
      s"Calendar not found: GBLO${lineSeparator}INJECTED",
      s"Unable to parse tenor: '${"A" * 10000}'",
      new String(Character.toChars(0x1f600)) * (MaxRenderedPart + 100),
      "")
    forAll(shaped) { (text: String) =>
      Show[Failure].show(Failure.Parsing(text)) shouldBe s"PARSING: ${Failure.renderDiagnostic(text)}"
      Failure.Parsing(text).toString shouldBe s"PARSING: ${Failure.renderDiagnostic(text)}"
    }
    // And over the failures the shared generators produce, message and attributes alike.
    forAll { (failure: Failure) =>
      val attributes = failure.attributes.iterator.map { case (key, value) =>
        s"${Failure.renderDiagnostic(key)}=${Failure.renderDiagnostic(value)}"
      }
      val expected = s"${failure.reason.name}: ${Failure.renderDiagnostic(failure.message)}"
      Show[Failure].show(failure) shouldBe
        (if (failure.attributes.isEmpty) expected else s"$expected [${attributes.mkString(", ")}]")
    }
  }

  test("the JSON form carries the text whole, escaped as the JSON grammar requires") {
    // The other way a failure leaves the program, and the reason the model keeps the value
    // rather than a rendering of it: the encoding is machine-readable, so it writes the whole
    // of the text and neutralises a control character by the grammar of the format instead.
    val payload = "A" * 10000
    val failure: Failure = Failure
      .Parsing(s"Unable to parse tenor: 'P3M\n$payload'")
      .withAttribute("definition", "GBLO\nUSNY")
    val json = failure.asJson.noSpaces
    json should include("\\n")
    json should not include "\n"
    decode[Failure](json) shouldBe Right(failure)
    decode[Failure](json).toOption.map(_.message) shouldBe Some(failure.message)
  }
}


// ---------------------------------------------------------------------------
// Mapping from the test classes being ported to the cases of this file.
//
// Three Java test classes of the package this file ports are covered here, and
// every one of their test methods has a case of its own: FailureReasonTest (9),
// FailureItemTest (18) and FailureItemsTest (8), 35 in all. A line reading
// `consolidated:FailureSpec` names a second case that covers a further aspect of
// the method above it; the method itself is `ported` by the case named first.
//
// The four remaining test classes of that Java package - FailureExceptionTest,
// FailureItemExceptionTest, IllegalArgFailureExceptionTest and
// ParseFailureExceptionTest - have no counterpart here and are absent by design:
// they test exception types, and this module defines none. A failure is a value
// that is returned, and raising one from a failure happens only at the edges of
// the port where an effect is run.
//
// ---- FailureReasonTest (9) ------------------------------------------------
//
// test_toString
//   -> "every reason renders as its upper-underscore name"
// test_of_lookup
//   -> "valueOf and parse both resolve the canonical name of every reason"
// test_of_lookupUpperCase
//   -> "the upper-case form of a reason name is its canonical form and resolves"
// test_of_lookupLowerCase
//   -> "valueOf rejects the lower-case form of a reason name that parse resolves"
//      The single factory of the original accepted three casings because its
//      lookup held an upper and a lower key per constant; the port covers the
//      same ground with an exact member and a lenient one.
// test_of_lookup_notFound
//   -> "text that names no reason is rejected as a parsing failure"
//      replaces: the original raised an error; the port reports the rejection as
//      a value on the left of an outcome.
// test_of_lookup_null
//   -> "empty and blank text name no reason"
//      replaces: the original passed nothing at all. Nothing is not a value of
//      `String` in this port, so the case asserts the text a caller can actually
//      supply - empty and blank - and this file contains no such literal.
// coverage
//   -> "the family holds exactly the ten reasons, in declaration order"
//      replaces: the original swept the constants of the enum reflectively. The
//      family is a list in the companion, so the assertion is over that list.
//   -> "the ordering of reasons agrees with their equality and hashing"
//      consolidated:FailureSpec - the second aspect of the sweep, over the one
//      equality-bearing instance the companion publishes.
// test_serialization
//   -> "every reason encodes as the bare string of its canonical name and decodes back"
//      replaces: binary serialization, which this port does not support. The
//      wire form is JSON and holds the same single name string.
//   -> "any reason round-trips through its JSON form"
//      consolidated:FailureSpec - the same property over generated values.
// test_jodaConvert
//   -> "the JSON decoder reads any casing, and rejects text that names no reason"
//      replaces: the string conversion an annotation drove in the original. The
//      JSON codec is the single string form that replaces it.
//
// ---- FailureItemTest (18) -------------------------------------------------
//
// test_of_reasonMessage
//   -> "a failure carries the reason of its class, the message interpolated at
//      the call site and no attributes"
// test_of_reasonMessageShortStackTrace
//   -> "a failure holds no stack trace, so there is none to shorten or to summarise"
//      replaces: stack capture and its rendering, neither of which exists.
// test_of_stackTraceErrorMessageWithNamedAttributes
//   -> "data named by a message travels in attributes added at the construction site"
//      replaces: named template placeholders that populated attributes.
// test_of_stackTraceErrorMessageWithUnnamedAttributes
//   -> "an interpolated message leaves no placeholder behind and populates no attribute"
//      replaces: positional template placeholders.
// test_of_stackTraceErrorMessageWithoutAttributes
//   -> "a message with nothing to interpolate is carried verbatim and adds no attribute"
//      replaces: a template with no placeholder.
// test_of_reasonException
//   -> "a failure reporting a rejected argument is built explicitly and retains no cause type"
//      replaces: deriving a failure from a caught exception.
// test_of_reasonError
//   -> "a failure reporting a fatal condition is built explicitly and retains no cause type"
//      replaces: deriving a failure from a caught error.
// test_of_reasonMessageException
//   -> "a message supplied by the reporter stands on its own, without the text of a cause"
//      replaces: appending the text of a captured condition to the rendering.
// test_of_reasonMessageExceptionNestedException
//   -> "nothing nested behind a failure is retained, only what its message and
//      attributes say"
//      replaces: recording the type of the outermost of a nest of conditions.
// test_of_reasonMessageExceptionNestedExceptionWithAttributes
//   -> "the attributes of a failure are the data its message names, and nothing
//      is added for it"
//      replaces: an attribute added on the failure's behalf beside the ones its
//      template named.
// test_of_reasonMessageExceptionWithAttributes
//   -> "the text of an underlying condition is carried as an ordinary attribute
//      when it is wanted"
//      replaces: the reserved attribute name the original used for that text.
// test_of_reasonMessageExceptionWithFailureItemException
//   -> "a failure keeps the reason chosen by the code that reported it"
//      replaces: the rule by which a failure carried inside a condition replaced
//      the reason its caller asked for. The class of a failure fixes its reason.
// test_of_reasonMessageExceptionWithFailureItemExceptionNoExMessageParam
//   -> "a message is whatever the reporter interpolated, line breaks and all"
//      replaces: parsing a template to locate a placeholder and its surroundings.
// test_of_reasonMessageExceptionWithFailureItemExceptionDuplicateName
//   -> "an attribute key given twice keeps the last value, so no key is ever renamed"
//      replaces: inventing numbered attribute names for a repeated placeholder.
// test_from_Throwable
//   -> "a failure is never derived from a thrown condition, only constructed"
//      replaces: the factory taking a throwable, which unwrapped the exception
//      types this module no longer defines.
// test_withAttribute
//   -> "withAttribute adds one attribute and preserves the class, the reason and
//      the message"
// test_withAttributes
//   -> "withAttributes merges the supplied attributes, letting them win, and
//      holds the result in key order"
//      The key order is a deliberate difference: the original held an
//      insertion-ordered map, and this port holds a sorted one so that the
//      rendering and the JSON of a failure depend on its value alone.
// test_map
//   -> "mapMessage transforms the message and preserves the class, the reason and
//      the attributes"
//   -> "mapMessage with the identity function changes nothing, for any failure"
//      consolidated:FailureSpec - the same contract as a property.
//
// ---- FailureItemsTest (8) -------------------------------------------------
//
// The aggregate type and its builder have no Scala target; a non-empty chain of
// failures replaces both, and `Failure.collapse` presents such a chain as one
// failure.
//
// test_EMPTY
//   -> "a chain of failures cannot be empty"
//      replaces: the empty instance and the predicate that tested for it.
//      Emptiness is not representable, which is the point of the type.
// test_of_array
//   -> "a chain built from failures given one at a time holds them in order"
// test_of_list
//   -> "a chain built from a list of failures holds them in order"
// test_builder_add
//   -> "failures are gathered by construction rather than through a builder"
//      replaces: the mutable builder, which has no target.
// test_builder_addAll
//   -> "a chain is extended with several failures at once by concatenation"
//      replaces: the bulk form of that builder.
// test_combinedWith
//   -> "concatenating chains keeps every failure, duplicates included, in order"
//   -> "reading any chain out and rebuilding it, and concatenating two chains,
//      keep every failure in order"
//      consolidated:FailureSpec - the same contract over generated chains, with
//      decomposition and construction asserted to be inverse.
// coverage
//   -> "two chains holding the same failures in the same order are equal and hash alike"
//      replaces: the reflective bean sweep. A chain is an ordinary value.
//   -> "any chain rebuilt from its own failures, in their order, is equal to it
//      and hashes alike"
//      consolidated:FailureSpec - the equality and the hashing of the sweep over
//      generated chains rather than over one pair, including the agreement of the
//      derived `Eq` and `Hash` instances with `==`.
//   -> "reversing a chain of two or more distinct failures yields a different chain"
//      consolidated:FailureSpec - the other half of that equality: order is part
//      of the value, asserted over generated chains and guarded for the single
//      and the palindromic arrangements.
// test_serialization
//   -> "a chain reaches JSON by being collapsed to the single failure that describes it"
//      replaces: binary serialization of the aggregate. The chain is a container
//      of the effect library and carries no codec of this port's making.
//   -> "collapsing any chain round-trips through JSON, and a doubled chain
//      collapses to the same failure"
//      consolidated:FailureSpec - the same round trip over generated chains, plus
//      the stability of the collapse under the duplicates concatenation keeps.
//
// ---- Cases with no counterpart in the test classes being ported ----------
//
// These pin behaviour the port introduces or that the original specified in its
// main sources rather than in these test classes - chiefly the collapse contract
// of the aggregating factory of the original, and the JSON forms, which are the
// only serialization this port supports. The last fourteen cover the rendering of
// a failure, which the original had no counterpart for at all: the messages of the
// library being ported echoed the text they were handed and were written out as
// they stood, and this port writes every part of a failure bounded and on a
// single line while the failure itself keeps the whole of that text. The four that
// name the renderer directly pin it as the published member the JSON decoder
// bridge, the resource reader and the text form of an identifier all reach for.
//
//   "parse also resolves a mixed-case reason name the original would have rejected"
//   "each of the ten failures is built from a message alone and reports its matching reason"
//   "Failure.of maps every reason onto the member that carries it"
//   "Show renders the reason, the message, and the attributes in key order"
//   "a failure encodes as the single-key object that names its member"
//   "any failure round-trips through its JSON form"
//   "equal failures encode to identical bytes whatever order their attributes were added in"
//   "the JSON decoder requires the attributes field and rejects an unknown member"
//   "collapsing failures that agree on a reason keeps it and joins their messages"
//   "collapsing failures that disagree on a reason yields MULTIPLE and joins their messages"
//   "collapsing a single failure yields that failure unchanged"
//   "collapsing failures that all report MULTIPLE keeps MULTIPLE"
//   "collapsing folds equal failures together, so a cause reported twice is described once"
//   "collapsing merges the attributes of every failure, letting the later value win"
//   "collapsing a one-failure chain is the identity, for any failure"
//   "collapsing a chain whose failures agree on a reason preserves that reason"
//   "collapsing any chain reports MULTIPLE exactly when its failures disagree"
//   "a failure carries the message it was built with, whole and unrendered"
//   "the rendering escapes every character of a message that could forge a line"
//   "the rendering escapes the key and the value of every attribute"
//   "the rendering writes a part of the bounded length whole and unmarked"
//   "the rendering truncates a part one character past the bound and marks what
//      it left out"
//   "no value a caller supplies can make a rendered failure large"
//   "the rendering keeps a surrogate pair whole, so truncation leaves no half
//      character"
//   "an ordinary failure renders as the text its reporter wrote, character for
//      character"
//   "the rendering of any failure is bounded, single-line and never half a
//      character"
//   "the text form of a failure is the rendering, so writing one out cannot
//      bypass it"
//   "Failure.renderDiagnostic writes ordinary text as itself, character for
//      character"
//   "Failure.renderDiagnostic escapes every character of any text that could
//      forge a line"
//   "Failure.renderDiagnostic keeps a surrogate pair whole and bounds what it
//      writes"
//   "Failure.renderDiagnostic renders exactly what the rendering of a failure
//      renders"
//   "the JSON form carries the text whole, escaped as the JSON grammar requires"
//
// ---------------------------------------------------------------------------
