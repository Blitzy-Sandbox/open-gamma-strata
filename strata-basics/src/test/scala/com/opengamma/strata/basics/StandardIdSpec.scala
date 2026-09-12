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

/**
 * Test [[StandardId]], ported from the Java `StandardIdTest`.
 *
 * ===What the four tables are for===
 *
 * The Java original drove six of its sixteen methods from four data providers, and those
 * providers carry the whole of the character-set contract: which characters a scheme may
 * hold, which characters a value may hold, and where the boundaries of those two sets fall.
 * They are transcribed here row for row, because each row sits deliberately on one side of
 * a boundary and a single altered character would turn a boundary test into a test of the
 * middle of the range. The three that are easy to mistake for typing errors are
 * intentional:
 *
 *   - `"! !\"$%%^&*()123abcxyzABCXYZ"` is a plain string, not a format string, so the two
 *     percent signs are two literal percent signs and the escape is a literal double quote;
 *   - `"12}3"` is rejected because the closing brace is one of the four printable ASCII
 *     characters above lower-case `z` that a value may not hold, the fourth being the tilde
 *     that separates the two parts of the text form;
 *   - `"12\u00003"` embeds a NUL, which is rejected because it falls below the space at the
 *     bottom of the permitted range.
 *
 * A fifth table sits inside `test_encodeScheme` rather than alongside these four, because it
 * transcribes no provider: it holds the four inputs that make the encoder's guarantee worth
 * asserting, being three characters a scheme may not hold and the percent that every escape
 * begins with.
 *
 * ===How the shape of the port changes the assertions===
 *
 * Three differences from the original are structural rather than a matter of taste, and each
 * is noted again at the test it affects:
 *
 *   - Both factories report their failures rather than throwing, so every assertion that was
 *     `assertThatIllegalArgumentException` is now an assertion that the outcome is a `Left`.
 *     `of` accumulates, so an input at fault in both of its parts reports both.
 *   - There is no null anywhere in this API, so the two tests that passed one keep their
 *     names and assert the nearest input that can actually be written.
 *   - The reflective bean sweep and Java serialization have no counterpart, so `coverage`
 *     and `test_serialization` assert the properties those two stood for: that equality,
 *     hashing, rendering and ordering agree with one another, and that an identifier
 *     survives a round trip through its JSON form.
 *
 * Sixteen of the seventeen tests keep the name of the Java method they come from, and each of the
 * four table-driven groups is one test that runs its whole table, which is what keeps the
 * method-level traceability of the migration exact. The seventeenth is this port's own and
 * answers to no Java method: it asserts that both factories name rejected text in full while the
 * rendering of their failures stays bounded and on one line, which is a promise the Java class -
 * whose factories threw - could not make.
 */
class StandardIdSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /** The scheme used by most of the tests, as in the Java original. */
  private val SCHEME: String = "Scheme"

  /** A second scheme, ordered before [[SCHEME]], used by the comparison tests. */
  private val OTHER_SCHEME: String = "Other"

  //-------------------------------------------------------------------------
  /**
   * Schemes and values that together name an identifier, transcribed from the Java provider.
   *
   * The four rows are the upper-case alphabet, the lower-case alphabet, the digits together
   * with every permitted punctuation character of a scheme, and a value exercising the
   * printable ASCII characters a value may hold.
   */
  private val data_factoryValid: TableFor2[String, String] = Table(
    ("scheme", "value"),
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "123"),
    ("abcdefghijklmnopqrstuvwxyz", "123"),
    ("0123456789:/+.=_-", "123"),
    ("ABC", "! !\"$%%^&*()123abcxyzABCXYZ"))

  /**
   * Schemes and values that name no identifier, transcribed from the Java provider.
   *
   * The rows cover, in order: both parts absent; a scheme of one character that is not
   * permitted; a scheme holding a character above the permitted set; a value beginning with
   * a space; a value holding a closing brace; and a value holding a NUL.
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
   * Values and the text they render as, transcribed from the Java provider.
   *
   * Both of the tests that use this table read both columns: one renders the value and
   * compares the text, the other parses the text and compares the value, so the table asserts
   * that rendering and parsing are inverse over these rows. The second row matters because a
   * plus sign is permitted in both parts, so it cannot be treated as a separator or as an
   * encoded space.
   */
  private val data_formats: TableFor2[String, String] = Table(
    ("value", "expected"),
    ("Value", "A~Value"),
    ("a+b", "A~a+b"))

  /**
   * Text that names no identifier, transcribed from the Java provider.
   *
   * The rows cover text with no separator at all, a separator with nothing after it, a
   * separator with nothing before it, the wrong separator, and two separators - the last
   * being the row that fixes the rule that a value may not hold a tilde, so that the first
   * tilde of the text is always the one that separates the two parts.
   */
  private val data_parseInvalidFormat: TableFor1[String] = Table(
    "text",
    "Scheme",
    "Scheme~",
    "~value",
    "Scheme:value",
    "a~b~c")

  //-------------------------------------------------------------------------
  /**
   * Builds an identifier, asserting that the factory accepted the two parts.
   *
   * The factory reports its failures rather than throwing, so a spec that wants the value
   * has to say what should happen when there is none. Asserting success here rather than at
   * every call site keeps the sixteen tests reading like the Java ones, and routing the
   * assertion through `beSuccess` means an unexpected rejection is reported with the reason
   * and message of every failure rather than as a failed pattern match.
   *
   * @param scheme  the scheme of the identifier
   * @param value  the value of the identifier
   * @return the identifier the two parts name
   */
  private def identifier(scheme: String, value: String): StandardId = {
    val outcome = StandardId.of(scheme, value)
    outcome should beSuccess
    outcome.getOrElse(fail(s"StandardId.of('$scheme', '$value') was expected to name an identifier"))
  }

  /**
   * Parses an identifier, asserting that the text named one.
   *
   * The counterpart of [[identifier]] for the other factory, which reports a single failure
   * rather than a chain of them.
   *
   * @param text  the text to parse
   * @return the identifier the text names
   */
  private def parsed(text: String): StandardId = {
    val outcome = StandardId.parse(text)
    outcome should beSuccess
    outcome.getOrElse(fail(s"StandardId.parse('$text') was expected to name an identifier"))
  }

  /**
   * Counts the failures an outcome of the two-part factory reports.
   *
   * This is how the accumulation of the factory is observed: the count is the length of the
   * chain, so a value of two says that the scheme and the value were each described rather
   * than the first fault ending the checks.
   *
   * @param scheme  the scheme to check
   * @param value  the value to check
   * @return the number of failures reported, or `None` if the two parts named an identifier
   */
  private def failureCount(scheme: String, value: String): Option[Int] =
    StandardId.of(scheme, value).swap.toOption.map(failures => failures.toNonEmptyList.size)

  /**
   * Reads the messages an outcome of the two-part factory reports, in the order they
   * accumulated.
   *
   * The counterpart of [[failureCount]] for the tests that assert the wording of a rejection
   * rather than its arity, and it asserts the arity as well by being compared against a list:
   * a message list of one says that exactly one failure was reported.
   *
   * @param outcome  the outcome expected to carry failures
   * @return the message of each failure, in order, or nothing when the parts named an
   *   identifier
   */
  private def messagesOf(outcome: ResultNec[StandardId]): List[String] =
    outcome.swap.toOption
      .map(failures => failures.toNonEmptyList.toList.map(failure => failure.message))
      .getOrElse(List.empty[String])

  /**
   * The rendering of each failure of an outcome, in order.
   *
   * This is the text a log or a report receives, as against the message the failure carries:
   * the two agree for every rejection of a realistic size, and differ exactly where the
   * rendering has to bound or escape what a caller supplied.
   *
   * @param outcome  the outcome expected to carry failures
   * @return the rendering of each failure, in order, or nothing when the parts named an
   *   identifier
   */
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
    // The Java test passed `null` as the scheme and asserted an IllegalArgumentException.
    // That case cannot be written against this API: the `notNull` family of checks was
    // dropped in the port because an absent argument is modelled by `Option`, not by a null
    // reference, and no factory of either module accepts one. Passing a null in regardless -
    // which the language would permit for a `String` - would assert the behaviour of the
    // platform rather than of this type, so the assertion instead names the nearest input a
    // caller can actually supply: a scheme holding no characters. It is rejected, and the
    // failure names the scheme, so a caller who supplied nothing for that part is told which
    // of the two parts was at fault.
    val outcome = StandardId.of("", "value")
    outcome should beFailure
    outcome should haveFailureMessageMatching(".*'scheme'.*")
    failureCount("", "value") shouldBe Some(1)
  }

  test("test_factory_String_String_nullValue") {
    // As above, for the other argument: `null` is not expressible here, so the assertion is
    // made against a value holding no characters, and the failure names the value.
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
      // The Java body called the factory and discarded the result, which asserted only that
      // no exception was thrown. Here the outcome is a value, so it is asserted: the factory
      // accepted the two parts, and the identifier it built holds exactly the parts given and
      // renders as the two of them separated by a tilde.
      val test = identifier(scheme, value)
      test.scheme shouldBe scheme
      test.value shouldBe value
      test.toString shouldBe s"$scheme~$value"
    }

    // The language a value is accepted in is `[!-z][ -z]*` and not `[!-z][ -z]+`: one
    // character is enough, here as in the Java original, and such a value round trips
    // through the text form like any other. The `+` the failure messages quote is the
    // original's message text, which this port reproduces word for word, so only an
    // assertion can settle which of the two the factory actually applies. The boundary is
    // real rather than absent: the one single-character value that is rejected is a space,
    // because a value may not begin with one.
    val single = identifier("A", "1")
    single.value shouldBe "1"
    single.toString shouldBe "A~1"
    StandardId.of("A", " ") should beFailure
  }

  test("test_factory_String_String_invalid") {
    forAll(data_factoryInvalid) { (scheme: String, value: String) =>
      StandardId.of(scheme, value) should beFailure
    }

    // The scheme and the value are checked independently and their failures accumulate, so
    // an input at fault in both parts is described twice rather than once: the caller can
    // correct both in one pass. The two parts of a value - its characters and its leading
    // character - are checked in sequence, which is why a value at fault contributes exactly
    // one failure however many of those two checks it would fail.
    failureCount("", "") shouldBe Some(2)
    failureCount(" ", "123") shouldBe Some(1)
    failureCount("ABC", " 123") shouldBe Some(1)
    failureCount("ABC", "12}3") shouldBe Some(1)
  }

  //-------------------------------------------------------------------------
  test("test_encodeScheme") {
    val testScheme = StandardId.encodeScheme("https://opengamma.com/foo/../~bar#test")
    val expectedScheme = "https://opengamma.com/foo/../%7Ebar%23test"

    testScheme shouldBe expectedScheme
    // Test use of the encoded scheme. This is the half of the Java test that gives the first
    // half its point: percent is itself a permitted scheme character, so text holding at
    // least one character is encoded into a scheme the factory accepts unchanged.
    identifier(testScheme, "value").scheme shouldBe expectedScheme

    // The rows are the inputs that make that qualified guarantee worth stating: the space
    // and the tilde, which a scheme may not hold at all - the tilde being the separator of
    // the text form - a character outside ASCII, which becomes one escape group per byte of
    // its UTF-8 form, and the percent, which a scheme may hold but which the encoder escapes
    // regardless, since it is the character every escape begins with. The encoded text is
    // asserted as well as its acceptance, so that a change of escaping is a failure here
    // rather than a silently different scheme.
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

    // Empty text is the one input the guarantee does not cover, and it is a legal argument:
    // there is nothing to escape, so the encoded form is empty too, and a scheme may not be
    // empty. The encoder stays text to text - as the escaper it replaces is - so ruling this
    // case out belongs to the caller, which is what the method's documentation states.
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

    // Text holding no tilde at all names no identifier and is reported as text that could
    // not be read, distinctly from text whose two parts were read but were unacceptable.
    StandardId.parse("Scheme") should beFailureWith(FailureReason.PARSING)
    StandardId.parse("Scheme:value") should beFailureWith(FailureReason.PARSING)
    // Text holding two tildes is split at the first one, which leaves a value holding the
    // second: a value may not hold a tilde, so the text is rejected rather than being read
    // as an identifier whose rendering would differ from the text it came from.
    StandardId.parse("a~b~c") should beFailure
    StandardId.parse("a~b~c") should haveFailureMessageMatching(".*'b~c'.*")
  }

  /**
   * Asserts that rejected text is named in full and rendered bounded and on one line.
   *
   * No counterpart in the Java test class: the original interpolated the text it was handed
   * into the exception it threw, as it stood, and this port names it the same way in the
   * failure it returns, so a caller correcting its input is handed back exactly what was
   * refused. What the port adds is the boundary at which such a failure is written out - its
   * rendering bounds every part and escapes anything that could forge a line of a log holding
   * it. Three messages quote caller text here and all three are asserted: the no-separator
   * wording of `parse`, the two part checks `of` performs, and the leading-space check that
   * follows the character check of a value.
   */
  test("both factories name rejected text in full, and their failures render bounded and on one line") {
    // The parse wording, over text holding no separator at all.
    val payload = "H" * 10000
    val bounded = StandardId.parse(payload)
    bounded should beFailureWith(FailureReason.PARSING)
    val failure = bounded.left.toOption.getOrElse(fail("expected a failure"))
    failure.message shouldBe s"Invalid identifier format: $payload"
    // The rendering is where the size stops: the ten thousand characters reach a log as a few
    // hundred, marked to say that there was more.
    val rendered = Show[Failure].show(failure)
    rendered.length should be < 1000
    rendered should startWith("PARSING: Invalid identifier format: HHH")
    rendered should endWith("...")

    // The two part checks of the other factory, reached with a part of the same size built from
    // a character neither part admits, since ten thousand letters are a perfectly good scheme
    // and a perfectly good value.
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

    // And the leading-space check, which quotes the value it was handed as well.
    val leadingSpace = StandardId.of(SCHEME, " " + payload)
    leadingSpace should beFailure
    messagesOf(leadingSpace) shouldBe
      List(s"Invalid initial space in value ' $payload' must match regex '[!-z][ -z]+'")
    renderingsOf(leadingSpace).head.length should be < 1000

    // Text holding a line break is named as it stands and rendered on one line, so a
    // line-oriented consumer of a rendering cannot be made to record a line the library did
    // not report.
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

    // And the messages for ordinary rejected text are unchanged, character for character, which
    // is what makes the bound invisible to every caller but the adversarial one.
    StandardId.parse("Scheme").left.toOption.map(failure => failure.message) shouldBe
      Some("Invalid identifier format: Scheme")
    messagesOf(StandardId.of("{", "value")) shouldBe
      List("Argument 'scheme' with value '{' must match pattern: [A-Za-z0-9:/+.=_%-]+")
    messagesOf(StandardId.of(SCHEME, " 123")) shouldBe
      List("Invalid initial space in value ' 123' must match regex '[!-z][ -z]+'")
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
    // The Java test also asserted inequality against the empty string and against null. The
    // first is kept, through the universal equality that assertion used; the second is
    // dropped because no identifier this port produces is ever a null reference and the API
    // offers no way to obtain one, so the clause has nothing to assert.
    d1a should not equal ""
    d1a.hashCode shouldBe d1b.hashCode
  }

  test("test_comparisonByScheme") {
    val id1 = identifier(SCHEME, "123")
    val id2 = identifier(OTHER_SCHEME, "234")
    // as schemes are different, will compare by scheme
    Order[StandardId].compare(id1, id2) should be > 0
    Order[StandardId].compare(id2, id1) should be < 0
  }

  test("test_comparisonWithSchemeSame") {
    val id1 = identifier(SCHEME, "123")
    val id2 = identifier(SCHEME, "234")
    // as schemes are same, will compare by id
    Order[StandardId].compare(id1, id2) should be < 0
    Order[StandardId].compare(id2, id1) should be > 0
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java test swept the immutable bean reflectively. There is no bean and no such
    // helper here, so the properties that sweep stood for are asserted directly: that two
    // identifiers built separately from the same parts are one value, that identifiers
    // differing in either part are not, that rendering agrees with the string form, and that
    // ordering agrees with equality - the last being the invariant of publishing a single
    // equality-bearing instance, which cannot be checked by looking at the instance alone.
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
      // compare is zero exactly when the two are equal, in both directions of the biconditional
      (Order[StandardId].compare(left, right) == 0) shouldBe Order[StandardId].eqv(left, right)
      Order[StandardId].eqv(left, right) shouldBe (left == right)
    }
  }

  test("test_serialization") {
    // The Java test asserted Java serialization, which this port does not support. The
    // serialized form of an identifier here is its JSON form, which is the same text
    // `toString` writes and `parse` reads - a bare string, not an object of two fields - so
    // that a document holding an identifier stays readable and stays comparable with one
    // written by the library this type is ported from.
    val sample = identifier(SCHEME, "123")

    val encoded: Json = Encoder[StandardId].apply(sample)
    encoded shouldBe Json.fromString("Scheme~123")
    encoded.asString shouldBe Some("Scheme~123")

    val decoded: Either[DecodingFailure, StandardId] = Decoder[StandardId].decodeJson(encoded)
    decoded shouldBe Right(sample)

    // Text that names no identifier is rejected as a decoding failure carrying the message
    // of the parse failure, rather than decoded into an identifier that could not have been
    // built by the factory.
    val rejected: Either[DecodingFailure, StandardId] =
      Decoder[StandardId].decodeJson(Json.fromString("Scheme"))
    rejected.isLeft shouldBe true
    rejected.swap.toOption.map(failure => failure.message) shouldBe
      Some("Invalid identifier format: Scheme")
  }
}
