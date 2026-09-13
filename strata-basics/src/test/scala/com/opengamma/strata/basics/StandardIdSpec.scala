/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import cats.Order
import cats.Show

import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor1
import org.scalatest.prop.TableFor2

import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers._

/** Test [[StandardId]]. */
class StandardIdSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  private val SCHEME: String = "Scheme"

  /**
   * The longest a scheme or a value may be, stated here independently of the type.
   *
   * The type holds this privately and names it in the failures it reports, and the text ceiling
   * of `parse` is derived from it, so the suite states the one number and computes the other the
   * same way the type does. A bound changed on one side alone therefore fails here, which is the
   * point: both numbers are part of what the factories promise a caller.
   */
  private val PartCeiling: Int = 65536

  private val OTHER_SCHEME: String = "Other"

  //-------------------------------------------------------------------------
  /**
   * Schemes and values the two-part factory accepts: the two ASCII alphabets, the digits with
   * seven of the eight punctuation characters a scheme admits - the percent sign, which a scheme
   * also admits, is covered by the escaping cases instead - and a value spanning the printable
   * ASCII a value admits. A value is accepted in the language `[!-z][ -z]*`, so one character is
   * enough; the `+` the failure messages quote is message text rather than the language applied.
   *
   * Every row of this table and of the three below it sits deliberately on one side of a
   * character-set boundary, so a single altered character silently weakens the check it makes.
   */
  private val data_factoryValid: TableFor2[String, String] = Table(
    ("scheme", "value"),
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "123"),
    ("abcdefghijklmnopqrstuvwxyz", "123"),
    ("0123456789:/+.=_-", "123"),
    ("ABC", "! !\"$%%^&*()123abcxyzABCXYZ"))

  /**
   * Schemes and values the factory refuses, one row just past each boundary: neither part may
   * be empty, a scheme admits only `[A-Za-z0-9:/+.=_%-]` so it holds neither a space nor a
   * brace, and a value may neither begin with a space nor hold anything outside `[ -z]`, which
   * rules out the closing brace above that range and the NUL below it.
   */
  private val data_factoryInvalid: TableFor2[String, String] = Table(
    ("scheme", "value"),
    ("", ""),
    (" ", "123"),
    ("{", "123"),
    ("ABC", " 123"),
    ("ABC", "12}3"),
    ("ABC", "12\u00003"))

  /**
   * Values and the `scheme~value` text they render as, read by two tests that run in opposite
   * directions, so the table pins rendering and parsing as inverses of one another.
   *
   * A plus sign is permitted in both parts, which is why it is neither a separator nor an
   * encoded space.
   */
  private val data_formats: TableFor2[String, String] = Table(
    ("value", "expected"),
    ("Value", "A~Value"),
    ("a+b", "A~a+b"))

  /**
   * The text shapes `parse` refuses: no separator, nothing after the separator, nothing before
   * it, the wrong separator, and two separators.
   *
   * The last row fixes the rule that a value may not hold a tilde, so the first tilde of the
   * text is always the one that separates the two parts.
   */
  private val data_parseInvalidFormat: TableFor1[String] = Table(
    "text",
    "Scheme",
    "Scheme~",
    "~value",
    "Scheme:value",
    "a~b~c")

  //-------------------------------------------------------------------------
  private def identifier(scheme: String, value: String): StandardId = {
    val outcome = StandardId.of(scheme, value)
    outcome should beSuccess
    outcome.getOrElse(fail(s"StandardId.of('$scheme', '$value') was expected to name an identifier"))
  }

  private def parsed(text: String): StandardId = {
    val outcome = StandardId.parse(text)
    outcome should beSuccess
    outcome.getOrElse(fail(s"StandardId.parse('$text') was expected to name an identifier"))
  }

  /**
   * Counts the failures the two-part factory reports, which is how its accumulation is
   * observed: one failure per rejected part, and both parts reported together when both are
   * wrong.
   *
   * The two checks over a value - its characters, then its leading character - run in
   * sequence, so a value at fault contributes one failure however many of them it would fail.
   */
  private def failureCount(scheme: String, value: String): Option[Int] =
    StandardId.of(scheme, value).swap.toOption.map(failures => failures.toNonEmptyList.size)

  private def messagesOf(outcome: ResultNec[StandardId]): List[String] =
    outcome.swap.toOption
      .map(failures => failures.toNonEmptyList.toList.map(failure => failure.message))
      .getOrElse(List.empty[String])

  private def renderingsOf(outcome: ResultNec[StandardId]): List[String] =
    outcome.swap.toOption
      .map(failures => failures.toNonEmptyList.toList.map(failure => Show[Failure].show(failure)))
      .getOrElse(List.empty[String])

  //-------------------------------------------------------------------------
  test("test_factory_String_String") {
    val test = identifier("scheme:/+foo", "value")
    test.scheme shouldBe "scheme:/+foo"
    test.value shouldBe "value"
    test.toString shouldBe "scheme:/+foo~value"
  }

  test("test_factory_String_String_nullScheme") {
    val outcome = StandardId.of("", "value")
    outcome should beFailure
    outcome should haveFailureMessageMatching(".*'scheme'.*")
    failureCount("", "value") shouldBe Some(1)
  }

  test("test_factory_String_String_nullValue") {
    val outcome = StandardId.of(SCHEME, "")
    outcome should beFailure
    outcome should haveFailureMessageMatching(".*'value'.*")
    failureCount(SCHEME, "") shouldBe Some(1)
  }

  test("test_factory_String_String_emptyValue") {
    val outcome = StandardId.of("Scheme", "")
    outcome should beFailure
    outcome should beFailureWith(FailureReason.INVALID)
  }

  test("test_factory_String_String_valid") {
    forAll(data_factoryValid) { (scheme: String, value: String) =>
      val test = identifier(scheme, value)
      test.scheme shouldBe scheme
      test.value shouldBe value
      test.toString shouldBe s"$scheme~$value"
    }

    val single = identifier("A", "1")
    single.value shouldBe "1"
    single.toString shouldBe "A~1"
    StandardId.of("A", " ") should beFailure
  }

  test("test_factory_String_String_invalid") {
    forAll(data_factoryInvalid) { (scheme: String, value: String) =>
      StandardId.of(scheme, value) should beFailure
    }

    failureCount("", "") shouldBe Some(2)
    failureCount(" ", "123") shouldBe Some(1)
    failureCount("ABC", " 123") shouldBe Some(1)
    failureCount("ABC", "12}3") shouldBe Some(1)
  }

  //-------------------------------------------------------------------------
  test("test_encodeScheme") {
    // Every character a scheme may not hold is percent-escaped, one group of three characters
    // per byte of its UTF-8 form, and the percent is escaped as well although a scheme may
    // hold it, since every escape begins with one. Text of at least one character therefore
    // encodes to a scheme the factory accepts unchanged; empty text encodes to empty, which
    // is no scheme at all.
    val testScheme = StandardId.encodeScheme("https://opengamma.com/foo/../~bar#test")
    val expectedScheme = "https://opengamma.com/foo/../%7Ebar%23test"

    testScheme shouldBe expectedScheme
    identifier(testScheme, "value").scheme shouldBe expectedScheme

    val encodings: TableFor2[String, String] = Table(
      ("text", "encoded"),
      (" ", "%20"),
      ("%", "%25"),
      ("~", "%7E"),
      ("\u00e9", "%C3%A9"))

    forAll(encodings) { (text: String, encoded: String) =>
      StandardId.encodeScheme(text) shouldBe encoded
      identifier(encoded, "value").scheme shouldBe encoded
    }

    StandardId.encodeScheme("") shouldBe ""
    StandardId.of("", "value") should beFailure
  }

  //-------------------------------------------------------------------------
  test("test_formats_toString") {
    forAll(data_formats) { (value: String, expected: String) =>
      val test = identifier("A", value)
      test.toString shouldBe expected
    }
  }

  test("test_formats_parse") {
    forAll(data_formats) { (value: String, text: String) =>
      val test = parsed(text)
      test.scheme shouldBe "A"
      test.value shouldBe value
    }
  }

  //-------------------------------------------------------------------------
  test("test_parse") {
    val test = parsed("Scheme~value")
    test.scheme shouldBe SCHEME
    test.value shouldBe "value"
    test.toString shouldBe "Scheme~value"
  }

  test("test_parse_invalidFormat") {
    forAll(data_parseInvalidFormat) { (text: String) =>
      StandardId.parse(text) should beFailure
    }

    StandardId.parse("Scheme") should beFailureWith(FailureReason.PARSING)
    StandardId.parse("Scheme:value") should beFailureWith(FailureReason.PARSING)
    StandardId.parse("a~b~c") should beFailure
    StandardId.parse("a~b~c") should haveFailureMessageMatching(".*'b~c'.*")
  }

  test("both factories name rejected text in full, and their failures render bounded and on one line") {
    // A failure names the whole of the text it refused, so a caller is handed back exactly
    // what it has to correct, while the rendering of that failure is bounded, marks what it
    // left out, and escapes anything that could forge a line of a log. All three wordings
    // that quote caller text are asserted: the no-separator wording of `parse`, the part
    // check of `of` over each of the two parts, and the leading-space check that follows the
    // character check of a value.
    val payload = "H" * 10000
    val bounded = StandardId.parse(payload)
    bounded should beFailureWith(FailureReason.PARSING)
    val failure = bounded.left.toOption.getOrElse(fail("expected a failure"))
    failure.message shouldBe s"Invalid identifier format: $payload"
    val rendered = Show[Failure].show(failure)
    rendered.length should be < 1000
    rendered should startWith("PARSING: Invalid identifier format: HHH")
    rendered should endWith("...")

    val rejectedPart = "{" * 10000
    val badScheme = StandardId.of(rejectedPart, "value")
    badScheme should beFailure
    messagesOf(badScheme) shouldBe
      List(s"Argument 'scheme' with value '$rejectedPart' must match pattern: [A-Za-z0-9:/+.=_%-]+")
    val badValue = StandardId.of(SCHEME, rejectedPart)
    badValue should beFailure
    messagesOf(badValue) shouldBe
      List(s"Argument 'value' with value '$rejectedPart' must match pattern: [!-z][ -z]+")
    renderingsOf(badScheme).head.length should be < 1000
    renderingsOf(badValue).head.length should be < 1000

    val leadingSpace = StandardId.of(SCHEME, " " + payload)
    leadingSpace should beFailure
    messagesOf(leadingSpace) shouldBe
      List(s"Invalid initial space in value ' $payload' must match regex '[!-z][ -z]+'")
    renderingsOf(leadingSpace).head.length should be < 1000

    val injected = StandardId.parse("Scheme\nvalue")
    injected should beFailureWith(FailureReason.PARSING)
    val injectedFailure = injected.left.toOption.getOrElse(fail("expected a failure"))
    injectedFailure.message shouldBe "Invalid identifier format: Scheme\nvalue"
    val injectedRendering = Show[Failure].show(injectedFailure)
    injectedRendering should not include "\n"
    injectedRendering should not include "\r"
    injectedRendering shouldBe "PARSING: Invalid identifier format: Scheme\\nvalue"
    val injectedValue = StandardId.of(SCHEME, "va\nlue")
    injectedValue should beFailure
    messagesOf(injectedValue) shouldBe
      List("Argument 'value' with value 'va\nlue' must match pattern: [!-z][ -z]+")
    renderingsOf(injectedValue) shouldBe
      List("INVALID: Argument 'value' with value 'va\\nlue' must match pattern: [!-z][ -z]+")

    StandardId.parse("Scheme").left.toOption.map(failure => failure.message) shouldBe
      Some("Invalid identifier format: Scheme")
    messagesOf(StandardId.of("{", "value")) shouldBe
      List("Argument 'scheme' with value '{' must match pattern: [A-Za-z0-9:/+.=_%-]+")
    messagesOf(StandardId.of(SCHEME, " 123")) shouldBe
      List("Invalid initial space in value ' 123' must match regex '[!-z][ -z]+'")
  }

  /**
   * Asserts the ceiling both factories put on the size of an identifier.
   *
   * No counterpart in the Java test class, and none in this port until now: neither part of an
   * identifier has a length the grammar fixes, so both were admitted at any size, walked
   * character by character, stored on the instance and quoted into every failure that named them
   * (CWE-400/CWE-770). Both factories now refuse a part beyond sixty-five thousand five hundred
   * and thirty-six characters, before the walk and before anything is quoted, and `parse`
   * refuses text beyond the longest text an identifier renders to.
   *
   * Those are two ceilings and not one, and the second is derived from the first rather than
   * equal to it. An identifier renders as `scheme~value`, so the longest text any value the
   * factories admit can render to is two parts and the separator between them - and the class
   * documentation of the type promises that `toString` and `parse` are inverses. A text ceiling
   * set at the part ceiling would have broken that promise rather than bounded it: two parts of
   * forty thousand characters are admitted, and their rendering of eighty thousand would have
   * been refused by the very parse that is supposed to read it back, in JSON as well as in text.
   * The boundary is therefore asserted from both directions below - the largest identifier the
   * factories admit reads back, and one character more than its rendering does not - because a
   * generator drawing short parts cannot reach it.
   *
   * The ceiling is far above anything an identifier is used for, which is the property that
   * matters and is asserted first: the ten-thousand-character identifier the suites of this
   * module treat as legal is still legal, and the ten-thousand-character rejections of the test
   * above still read as they always did. What the ceiling changes is only reachable by a payload
   * written to be one, and there the failure names the bound rather than the text - the input is
   * refused for its size, so writing it out is the very thing the refusal exists to avoid, which
   * is how `Decimal` reports the same condition on the numeral it reads.
   */
  test("both factories refuse a part beyond the ceiling, naming the ceiling rather than the part") {
    // Below the ceiling nothing has changed: a long identifier is an identifier.
    val legal: String = "H" * 10000
    val large: StandardId = identifier(SCHEME, legal)
    large.value shouldBe legal
    large.toString shouldBe s"$SCHEME~$legal"
    StandardId.parse(large.toString) shouldBe Right(large)
    identifier(legal, legal).scheme shouldBe legal

    // Past it, each part is refused on its own terms, with the ceiling named and the part absent
    // from both the message and its rendering.
    val oversized: String = "H" * (PartCeiling + 1)
    val schemeMessage: String = s"Argument 'scheme' must not exceed $PartCeiling characters"
    val valueMessage: String = s"Argument 'value' must not exceed $PartCeiling characters"
    val badScheme = StandardId.of(oversized, "value")
    badScheme should beFailureWith(FailureReason.INVALID)
    messagesOf(badScheme) shouldBe List(schemeMessage)
    renderingsOf(badScheme) shouldBe List(s"INVALID: $schemeMessage")
    val badValue = StandardId.of(SCHEME, oversized)
    badValue should beFailureWith(FailureReason.INVALID)
    messagesOf(badValue) shouldBe List(valueMessage)
    renderingsOf(badValue) shouldBe List(s"INVALID: $valueMessage")

    // Both parts past the ceiling accumulate, as the two part checks always did, and neither
    // failure quotes what it refused.
    val bothParts = StandardId.of(oversized, oversized)
    messagesOf(bothParts) shouldBe List(schemeMessage, valueMessage)

    // A part past the ceiling is described by the ceiling alone: the character check that would
    // have quoted it is not reached, so text that is both too long and malformed reads as too
    // long.
    messagesOf(StandardId.of("{" * (PartCeiling + 1), "value")) shouldBe List(schemeMessage)

    // `parse` applies its own ceiling to the whole of its text, before the separator is looked
    // for and before either substring is taken, and reports it as a parsing failure naming that
    // ceiling. The number is the longest text an identifier renders to - two parts at the part
    // ceiling and the separator between them - so no rendering of any value the factories admit
    // meets it.
    val textCeiling: Int = 2 * PartCeiling + 1
    val parseMessage: String = s"Identifier string must not exceed $textCeiling characters"
    val overLongText: String = "H" * (textCeiling + 1)
    val refusedText = StandardId.parse(overLongText)
    refusedText should beFailureWith(FailureReason.PARSING)
    refusedText.left.toOption.map(failure => failure.message) shouldBe Some(parseMessage)
    Show[Failure].show(refusedText.left.toOption.getOrElse(fail("expected a failure"))) shouldBe
      s"PARSING: $parseMessage"

    // Inside that ceiling, a part past the part ceiling is refused as that part: the text is
    // short enough to read, and what it reads out is held to the same bound `of` holds it to.
    StandardId
      .parse(s"$SCHEME~${"H" * (PartCeiling + 1)}")
      .left
      .toOption
      .map(failure => failure.message) shouldBe Some(valueMessage)

    // Text at the part ceiling with no separator still reaches the ordinary wording, quoted in
    // full, because it is text this parse is willing to read rather than text it refuses to.
    val atCeiling: String = "H" * PartCeiling
    StandardId.parse(atCeiling).left.toOption.map(failure => failure.message) shouldBe
      Some(s"Invalid identifier format: $atCeiling")

    // Both directions of the boundary the two ceilings meet at. The largest identifier the
    // factories admit renders to text of exactly the text ceiling and reads back as itself -
    // which is the inverse the class documentation promises, at the one size a generator of
    // short parts never draws - while one character beyond that rendering is refused.
    val maximumPart: String = "H" * PartCeiling
    val maximum: StandardId = identifier(maximumPart, maximumPart)
    maximum.toString.length shouldBe textCeiling
    StandardId.parse(maximum.toString) shouldBe Right(maximum)
    StandardId.parse(maximum.toString + "H") should beFailureWith(FailureReason.PARSING)

    // and the same of a large identifier well inside the part ceiling whose rendering is past
    // the part ceiling, which is the case the two-ceiling arrangement exists for
    val wide: StandardId = identifier("A" * 40000, "B" * 40000)
    wide.toString.length shouldBe 80001
    StandardId.parse(wide.toString) shouldBe Right(wide)
  }

  //-------------------------------------------------------------------------
  test("test_equals") {
    val d1a = identifier(SCHEME, "d1")
    val d1b = identifier(SCHEME, "d1")
    val d2 = identifier(SCHEME, "d2")
    val d3 = identifier("Different", "d1")
    d1a shouldBe d1a
    d1a shouldBe d1b
    d1a should not be d2
    d1a should not be d3
    d1a should not equal ""
    d1a.hashCode shouldBe d1b.hashCode
  }

  test("test_comparisonByScheme") {
    val id1 = identifier(SCHEME, "123")
    val id2 = identifier(OTHER_SCHEME, "234")
    Order[StandardId].compare(id1, id2) should be > 0
    Order[StandardId].compare(id2, id1) should be < 0
  }

  test("test_comparisonWithSchemeSame") {
    val id1 = identifier(SCHEME, "123")
    val id2 = identifier(SCHEME, "234")
    Order[StandardId].compare(id1, id2) should be < 0
    Order[StandardId].compare(id2, id1) should be > 0
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    val sample = identifier(SCHEME, "123")
    val same = identifier(SCHEME, "123")
    val otherValue = identifier(SCHEME, "124")
    val otherScheme = identifier(OTHER_SCHEME, "123")

    sample shouldBe same
    sample.hashCode shouldBe same.hashCode
    sample should not be otherValue
    sample should not be otherScheme

    Show[StandardId].show(sample) shouldBe sample.toString
    Show[StandardId].show(sample) shouldBe "Scheme~123"

    val instances = List(sample, same, otherValue, otherScheme)
    val pairs: TableFor2[StandardId, StandardId] = Table(
      ("left", "right"),
      instances.flatMap(left => instances.map(right => (left, right))): _*)

    forAll(pairs) { (left: StandardId, right: StandardId) =>
      (Order[StandardId].compare(left, right) == 0) shouldBe Order[StandardId].eqv(left, right)
      Order[StandardId].eqv(left, right) shouldBe (left == right)
    }
  }

  test("test_serialization") {
    val sample = identifier(SCHEME, "123")

    val encoded: Json = Encoder[StandardId].apply(sample)
    encoded shouldBe Json.fromString("Scheme~123")
    encoded.asString shouldBe Some("Scheme~123")

    val decoded: Either[DecodingFailure, StandardId] = Decoder[StandardId].decodeJson(encoded)
    decoded shouldBe Right(sample)

    val rejected: Either[DecodingFailure, StandardId] =
      Decoder[StandardId].decodeJson(Json.fromString("Scheme"))
    rejected.isLeft shouldBe true
    rejected.swap.toOption.map(failure => failure.message) shouldBe
      Some("Invalid identifier format: Scheme")
  }
}
