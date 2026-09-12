/*
 * Copyright (C) 2022 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import java.math.BigDecimal
import java.math.RoundingMode

import scala.collection.immutable.List
import scala.collection.immutable.Map
import scala.collection.immutable.Set

import cats.Eq
import cats.Hash
import cats.Order
import cats.Show
import cats.syntax.all._

import _root_.io.circe.DecodingFailure
import _root_.io.circe.Json
import _root_.io.circe.parser.decode
import _root_.io.circe.syntax._

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import com.opengamma.strata.collect.Arbitraries.arbDecimal
import com.opengamma.strata.collect.Arbitraries.arbFixedScaleDecimal
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Tests [[FixedScaleDecimal]], the pairing of a [[Decimal]] with the scale it is shown at.
 *
 * ===What is under test===
 *
 * The whole surface of the type: the two factories `of` and `parse`, the two operations `map`
 * and `toBigDecimal`, the comparison, the rendering, and the three instances the companion
 * publishes - the single equality-bearing `Order` with `Hash`, the `Show`, and the JSON codec.
 *
 * The six cases of the suite being ported keep their original method names verbatim -
 * `testValues`, `testMap`, `testParse`, `testBad`, `testCompareTo`, `testEquals` - because the
 * method-level mapping of the migration
 * (`strata-basics/src/test/resources/manifest/java-test-mapping.csv`) names each of them as
 * the case that carries the original, and the traceability gate resolves those names against
 * the test report. They are therefore a published contract and must not be reworded. The cases
 * added beyond them carry descriptive names, which are equally stable.
 *
 * ===Failure replaces raising===
 *
 * Both factories of the type being ported raised: the private constructor threw
 * `IllegalArgumentException` for a scale it would not accept. Both factories of the port
 * answer with a chain of failures instead, because a scale is data supplied by a caller and
 * not a broken precondition, so every rejection is asserted here as a `Left` carrying the
 * reason and the message rather than as a thrown exception. No assertion in this spec expects
 * a throw.
 *
 * The two messages are reproduced from the type being ported character for character
 * (`FixedScaleDecimal.java:64-69`), so they are written once in this file - in
 * [[belowDecimalMessage]] and [[aboveMaximumMessage]] - and asserted from there.
 *
 * ===The ordering tie-break this spec owns===
 *
 * The comparison of the port falls through to the fixed scale where the decimals are equal,
 * while the comparison being ported stopped at the decimals (`FixedScaleDecimal.java:120-122`).
 * The reason is the consistency law: the two values `1.2` at one decimal place and `1.2` at
 * two are *unequal*, so a comparison that returned zero for them would contradict equality,
 * and the law suites of the dependent module - which check `Order` against `Eq` for this type -
 * would falsify. That tie-break is asserted nowhere else in this port, so the test that pins it
 * and the law it protects both live here, and the divergence is recorded in the migration note.
 *
 * ===Accumulation, and the one thing it cannot be shown to do===
 *
 * `of` combines its two checks with the accumulating combinator, so its outcome is a chain
 * that reports every reason a scale was rejected rather than the first of them. The two
 * reasons cannot both apply to one input, and that is a fact about the domain rather than
 * about the implementation: a scale is rejected as too small only when it is below the scale
 * of the decimal, which is never above eighteen, and as too large only when it is above
 * eighteen. A single input satisfying both would need a decimal of scale twenty or more,
 * which no decimal has. Rather than assert a two-failure chain that no input can produce,
 * this spec asserts the stronger statement that covers it: for any decimal and any scale the
 * chain holds *exactly* the messages of the checks that failed, in the order they are
 * declared - so a check that stopped reporting, or reported something it should not, is
 * caught - and the mutual exclusivity itself is asserted as a property, so a later change
 * that lets a decimal hold a larger scale will fail this spec and send a reader back to this
 * note. The accumulating combinator is covered on its own terms by `ValidateSpec`.
 *
 * ===Notes on the fixtures===
 *
 * Values are built through the factories and unwrapped by [[pair]] and [[fixed]], which fail
 * the test rather than raise when a factory rejects its input, so a mistake in a fixture is
 * reported against the fixture and not as an exception inside an unrelated assertion.
 * Property-based cases draw from the generators of [[Arbitraries]], which build every value
 * through `of` and therefore produce valid pairings only.
 *
 * Table-driven checks come from `ScalaCheckPropertyChecks`, which extends the table-driven
 * trait, so the table vocabulary and the generator vocabulary are both available from the one
 * mixin; a separate import of the table-driven trait would be an unused import, which this
 * build rejects.
 */
final class FixedScaleDecimalSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  //-------------------------------------------------------------------------
  // fixtures and helpers
  //-------------------------------------------------------------------------

  /** Stands for the unrelated object the original suite compared a value against. */
  private val unrelated: Any = ""

  /**
   * The rejection of a scale that would drop a digit the decimal holds.
   *
   * The wording is that of the type being ported (`FixedScaleDecimal.java:65`), reproduced
   * character for character, and this is the only place this spec writes it.
   */
  private def belowDecimalMessage(fixedScale: Int): String =
    s"Scale must be equal or greater than the scale of the decimal: $fixedScale"

  /**
   * The rejection of a scale beyond the largest a decimal holds.
   *
   * The wording is that of the type being ported (`FixedScaleDecimal.java:68`), reproduced
   * character for character, including the literal eighteen.
   */
  private def aboveMaximumMessage(fixedScale: Int): String = s"Scale must be 18 or less: $fixedScale"

  /** Reads a decimal from text, failing the test if the text names none. */
  private def decimalOf(text: String): Decimal =
    Decimal.parse(text).getOrElse(fail(s"the decimal fixture rejected <$text>"))

  /** Pairs a decimal with a scale, failing the test if the pairing is rejected. */
  private def pair(source: Decimal, fixedScale: Int): FixedScaleDecimal =
    FixedScaleDecimal
      .of(source, fixedScale)
      .getOrElse(fail(s"of rejected the decimal <$source> at scale <$fixedScale>"))

  /** Reads a fixed-scale decimal from text, failing the test if the text names none. */
  private def fixed(text: String): FixedScaleDecimal =
    FixedScaleDecimal.parse(text).getOrElse(fail(s"parse rejected <$text>"))

  /** The failures an outcome holds, in order, for assertions on their exact content. */
  private def failuresOf[A](outcome: ResultNec[A]): List[Failure] =
    outcome.fold(failures => failures.toChain.toList, _ => List.empty)

  /** The messages of the failures an outcome holds, in order. */
  private def messagesOf[A](outcome: ResultNec[A]): List[String] =
    failuresOf(outcome).map(failure => failure.message)

  /**
   * The messages `of` is required to report for a pairing, derived from its two checks.
   *
   * The checks are restated here rather than read from the type, because restating them is
   * what makes the assertion independent: an outcome is compared against what the documented
   * contract demands, not against what the implementation happens to do.
   */
  private def expectedMessagesFor(source: Decimal, fixedScale: Int): List[String] = {
    val tooSmall = if (fixedScale < source.scale) List(belowDecimalMessage(fixedScale)) else List.empty
    val tooLarge = if (fixedScale > Decimal.MAX_SCALE) List(aboveMaximumMessage(fixedScale)) else List.empty
    tooSmall ++ tooLarge
  }

  //-------------------------------------------------------------------------
  // the six cases of the original suite, under their original names
  //-------------------------------------------------------------------------

  test("testValues") {
    // The seven rows of the original's `dataValues`, with the rendering each is expected to
    // produce, asserted through the factory the original used.
    val values = Table(
      ("decimalText", "fixedScale", "expectedText"),
      ("1", 0, "1"),
      ("1.2", 1, "1.2"),
      ("1.2", 2, "1.20"),
      ("1.2", 3, "1.200"),
      ("-2", 0, "-2"),
      ("-2.3", 1, "-2.3"),
      ("-2.3", 2, "-2.30"))

    val zero = fixed("0")
    forAll(values) { (decimalText: String, fixedScale: Int, expectedText: String) =>
      val source = decimalOf(decimalText)
      val outcome = FixedScaleDecimal.of(source, fixedScale)
      outcome should beSuccess
      val value = pair(source, fixedScale)

      // The decimal is the one supplied. The original asserted it against the decimal rounded
      // to the fixed scale, which is the same value here for every accepted pairing - the
      // fixed scale is never below the scale of the decimal, so rounding to it changes
      // nothing - and both forms are asserted so that the port is pinned to rounding nothing.
      value.decimal shouldBe source
      value.decimal shouldBe source.roundToScale(fixedScale, RoundingMode.HALF_UP)
      value.fixedScale shouldBe fixedScale

      // The conversion carries the fixed scale, which is what the original asserted against a
      // `BigDecimal` built from the text and set to that scale.
      value.toBigDecimal shouldBe new BigDecimal(decimalText).setScale(fixedScale, RoundingMode.HALF_UP)
      value.toBigDecimal.scale shouldBe fixedScale

      // Equality against an unrelated object, against itself, and against another value of
      // the type, as the original asserted for every row.
      value.equals(unrelated) shouldBe false
      value.equals(value) shouldBe true
      value.equals(zero) shouldBe false
      value.hashCode should not be zero.hashCode

      value.toString shouldBe expectedText
    }
  }

  test("testMap") {
    // The original doubled a value of two decimal places and expected a value of two decimal
    // places. The port's `map` validates, so the outcome is a chain rather than a value, and
    // the expected value is compared through it.
    val value = fixed("1.25")
    value.map(decimal => decimal.multipliedBy(2L)) should haveValue(fixed("2.50"))
    value.map(decimal => decimal.multipliedBy(2L)) shouldBe FixedScaleDecimal.parse("2.50")
  }

  test("testParse") {
    // The three rows of the original, each asserting that parsing text produces the same value
    // as pairing the decimal the text names with the scale its digits imply.
    FixedScaleDecimal.parse("1.25") shouldBe FixedScaleDecimal.of(decimalOf("1.25"), 2)
    FixedScaleDecimal.parse("1.20") shouldBe FixedScaleDecimal.of(decimalOf("1.2"), 2)
    FixedScaleDecimal.parse("1") shouldBe FixedScaleDecimal.of(decimalOf("1"), 0)

    // And the values themselves, so that the equality above cannot be satisfied by two
    // failures that happen to agree.
    fixed("1.25").fixedScale shouldBe 2
    fixed("1.20").decimal shouldBe decimalOf("1.2")
    fixed("1").fixedScale shouldBe 0
  }

  test("testBad") {
    // The four rows of the original's `dataBad`. Each raised an `IllegalArgumentException`
    // there and is a `Left` here, carrying the reason and the message of the check that
    // rejected it.
    val rejected = Table(
      ("decimalText", "fixedScale", "expectedMessage"),
      ("1.2", 0, belowDecimalMessage(0)),
      ("1.2567", 0, belowDecimalMessage(0)),
      ("1.2", -1, belowDecimalMessage(-1)),
      ("1.2", 19, aboveMaximumMessage(19)))

    forAll(rejected) { (decimalText: String, fixedScale: Int, expectedMessage: String) =>
      val outcome = FixedScaleDecimal.of(decimalOf(decimalText), fixedScale)
      outcome should beFailure
      outcome should beFailureWith(FailureReason.INVALID)
      messagesOf(outcome) shouldBe List(expectedMessage)
      failuresOf(outcome).forall(failure => failure.attributes.isEmpty) shouldBe true
    }
  }

  test("testCompareTo") {
    // The chained comparison of the original: a value equal by comparison, one greater and
    // one smaller. Equality by comparison is asserted against the same pairing reached the
    // other way, through `of`, which is what the original compared against.
    val value = fixed("1.20")
    val ordering = Order[FixedScaleDecimal]
    ordering.compare(value, pair(decimalOf("1.2"), 2)) shouldBe 0
    ordering.compare(value, fixed("1.21")) should be < 0
    ordering.compare(value, fixed("1.19")) should be > 0

    // The comparison the type publishes for itself, which the ordering is taken from.
    value.compareTo(pair(decimalOf("1.2"), 2)) shouldBe 0
    value.compareTo(fixed("1.21")) should be < 0
    value.compareTo(fixed("1.19")) should be > 0
  }

  test("testEquals") {
    // The original asserted that a value equals another built from the same text and differs
    // from one built from text of a different scale. Both hold here, and the second is the
    // observable half of the tie-break asserted below.
    val value = fixed("1.2")
    val atTwoPlaces = fixed("1.20")
    value shouldBe fixed("1.2")
    value should not be atTwoPlaces
    atTwoPlaces should not be value
    value.hashCode shouldBe fixed("1.2").hashCode
  }

  //-------------------------------------------------------------------------
  // construction: the factory and the two checks it applies
  //-------------------------------------------------------------------------

  test("of pairs the decimal it was given, rounding nothing and normalising nothing") {
    // The decimal is carried unchanged, whatever padding the scale asks for. This is what
    // makes the two halves of the value independent: the decimal keeps its own normalised
    // scale and the fixed scale records how many places the value is shown at.
    val source = decimalOf("1.2")
    source.scale shouldBe 1

    val shown = Table(
      ("fixedScale", "expectedText"),
      (1, "1.2"),
      (2, "1.20"),
      (5, "1.20000"),
      (18, "1.200000000000000000"))

    forAll(shown) { (fixedScale: Int, expectedText: String) =>
      val value = pair(source, fixedScale)
      value.decimal shouldBe source
      value.decimal.scale shouldBe 1
      value.fixedScale shouldBe fixedScale
      value.toString shouldBe expectedText
    }
  }

  test("of accepts every scale from the scale of the decimal up to the maximum a decimal holds") {
    // The accepted range is closed at both ends, and the upper end is read from the type
    // rather than restated, so the two move together if the type ever supports a larger scale.
    val source = decimalOf("12.34")
    val accepted = (source.scale to Decimal.MAX_SCALE).toList
    accepted.head shouldBe 2
    accepted.last shouldBe 18

    val outcomes = accepted.map(fixedScale => FixedScaleDecimal.of(source, fixedScale))
    outcomes.count(outcome => outcome.isRight) shouldBe accepted.size
    outcomes.flatMap(outcome => outcome.toOption).map(value => value.fixedScale) shouldBe accepted
    pair(source, 2).toString shouldBe "12.34"
    pair(source, 4).toString shouldBe "12.3400"
    pair(source, Decimal.MAX_SCALE).toString shouldBe "12.340000000000000000"

    // Zero is held at scale zero however it was written, so every scale is available to it.
    val zeroOutcomes = (0 to Decimal.MAX_SCALE).toList.map(scale => FixedScaleDecimal.of(Decimal.ZERO, scale))
    zeroOutcomes.count(outcome => outcome.isRight) shouldBe Decimal.MAX_SCALE + 1
    pair(Decimal.ZERO, 0).toString shouldBe "0"
    pair(Decimal.ZERO, 3).toString shouldBe "0.000"
  }

  test("of rejects a scale that would drop a digit the decimal holds, naming the scale supplied") {
    // Every scale below the scale of the decimal is rejected, including the negative scales a
    // caller can write, and the message names the scale that was supplied rather than the one
    // that was required - the wording of the type being ported.
    val rejected = Table(
      ("decimalText", "fixedScale"),
      ("1.2", 0),
      ("1.2", -1),
      ("1.2", -18),
      ("1.2567", 0),
      ("1.2567", 3),
      ("0.000000000000000001", 17),
      ("12.34", 1))

    forAll(rejected) { (decimalText: String, fixedScale: Int) =>
      val outcome = FixedScaleDecimal.of(decimalOf(decimalText), fixedScale)
      outcome should beFailure
      outcome should beFailureWith(FailureReason.INVALID)
      messagesOf(outcome) shouldBe List(belowDecimalMessage(fixedScale))
    }

    // The scale of a decimal is never negative, so a negative scale is always rejected, and it
    // is rejected by this check rather than by the maximum check.
    messagesOf(FixedScaleDecimal.of(Decimal.ZERO, -1)) shouldBe List(belowDecimalMessage(-1))
  }

  test("of rejects a scale beyond the maximum a decimal holds, naming the scale supplied") {
    // Nineteen is the first rejected scale, and every scale above it is rejected for the same
    // reason with the same wording.
    val rejected = Table(
      ("fixedScale"),
      (Decimal.MAX_SCALE + 1),
      (20),
      (1000),
      (Int.MaxValue))

    forAll(rejected) { (fixedScale: Int) =>
      val outcome = FixedScaleDecimal.of(decimalOf("1.2"), fixedScale)
      outcome should beFailure
      outcome should beFailureWith(FailureReason.INVALID)
      messagesOf(outcome) shouldBe List(aboveMaximumMessage(fixedScale))
    }

    // The boundary itself is accepted, so the check is on the far side of the maximum.
    FixedScaleDecimal.of(decimalOf("1.2"), Decimal.MAX_SCALE) should beSuccess
    messagesOf(FixedScaleDecimal.of(decimalOf("1.2"), 19)) shouldBe List("Scale must be 18 or less: 19")
  }

  test("of reports every reason its scale was rejected, in the order the checks are declared") {
    // The outcome is a chain because the two checks are combined by accumulation rather than
    // by short-circuiting, so what is asserted is the whole content of the chain: exactly the
    // messages of the checks that failed, in declaration order, and nothing else. See the note
    // on this spec for why no input can make that chain hold two messages.
    val cases = Table(
      ("decimalText", "fixedScale"),
      ("1.2", 1),
      ("1.2", 18),
      ("1.2", 0),
      ("1.2", -1),
      ("1.2", 19),
      ("1.2", 1000),
      ("0", 0),
      ("0", 19),
      ("1.2567", 2),
      ("1.2567", 4),
      ("999999999999999999", 0),
      ("999999999999999999", 18))

    forAll(cases) { (decimalText: String, fixedScale: Int) =>
      val source = decimalOf(decimalText)
      val expected = expectedMessagesFor(source, fixedScale)
      messagesOf(FixedScaleDecimal.of(source, fixedScale)) shouldBe expected
      FixedScaleDecimal.of(source, fixedScale).isRight shouldBe expected.isEmpty
    }
  }

  test("the two reasons of of are mutually exclusive, because no decimal holds a larger scale") {
    // The fact the note on this spec rests on, asserted rather than assumed: a decimal's scale
    // never exceeds the maximum, so a scale below it can never also be above the maximum. Were
    // that ever to change, this property falsifies and the accumulating outcome of `of` would
    // become observable as a chain of two.
    forAll { (source: Decimal, fixedScale: Int) =>
      source.scale should be >= 0
      source.scale should be <= Decimal.MAX_SCALE
      val tooSmall = fixedScale < source.scale
      val tooLarge = fixedScale > Decimal.MAX_SCALE
      (tooSmall && tooLarge) shouldBe false
      messagesOf(FixedScaleDecimal.of(source, fixedScale)) shouldBe
        expectedMessagesFor(source, fixedScale)
    }
  }

  test("no scale is rejected for both reasons, over every scale a decimal can hold") {
    // The deterministic form of the property above, so that the note on this spec rests on an
    // exhaustive measurement rather than on sampling: one decimal of every scale a decimal can
    // hold, against every scale a caller can plausibly write. A chain of two failures would
    // show up here as a maximum of two, and a check that stopped reporting would show up as a
    // row whose messages differ from the contract.
    val decimals = (0 to Decimal.MAX_SCALE).toList.map(scale =>
      Decimal.ofScaled(1L, scale).getOrElse(fail(s"no decimal exists of scale <$scale>")))
    decimals.map(source => source.scale) shouldBe (0 to Decimal.MAX_SCALE).toList

    val writtenScales = (-20 to 20).toList
    val outcomes = for {
      source <- decimals
      fixedScale <- writtenScales
    } yield (source, fixedScale, messagesOf(FixedScaleDecimal.of(source, fixedScale)))

    outcomes.size shouldBe decimals.size * writtenScales.size
    outcomes.map { case (_, _, messages) => messages.size }.max shouldBe 1
    outcomes.filter { case (source, fixedScale, messages) =>
      messages != expectedMessagesFor(source, fixedScale)
    } shouldBe List.empty

    // The accepted region is exactly the closed range from the scale of the decimal to the
    // maximum, which is 19 scales for a decimal of scale zero down to one for a decimal of the
    // maximum scale.
    outcomes.count { case (_, _, messages) => messages.isEmpty } shouldBe
      decimals.map(source => Decimal.MAX_SCALE - source.scale + 1).sum
  }

  test("of is the only way a fixed-scale decimal can be built") {
    val value = fixed("1.20")
    // The class is a case class whose constructor is private and which is abstract as well, so
    // no `apply` and no `copy` are synthesised, the constructor cannot be reached from here,
    // and the family cannot be extended from outside its own file. Each proof is a typed
    // expression, so none of them can fail to compile for an unrelated reason.
    assertDoesNotCompile("""FixedScaleDecimal(Decimal.ZERO, 2): FixedScaleDecimal""")
    assertDoesNotCompile("""new FixedScaleDecimal(Decimal.ZERO, 2): FixedScaleDecimal""")
    assertDoesNotCompile("""value.copy(fixedScale = 3): FixedScaleDecimal""")
    assertDoesNotCompile("""value.copy(decimal = Decimal.ZERO): FixedScaleDecimal""")
    assertDoesNotCompile("""class Widened extends FixedScaleDecimal(Decimal.ZERO, 2)""")
    // The subject of the proofs is read, so nothing above is vacuous.
    value.fixedScale shouldBe 2
  }

  test("unapply takes a value apart, and the pattern it gives is irrefutable") {
    val value = fixed("1.20")
    val extracted = value match { case FixedScaleDecimal(decimal, fixedScale) => (decimal, fixedScale) }
    extracted shouldBe ((decimalOf("1.2"), 2))

    // The extraction of a case class with a private constructor is still generated, which is
    // what lets a caller read a value apart without the type exposing a way to build one.
    FixedScaleDecimal.unapply(value).map(parts => parts._2) shouldBe Some(2)
  }

  //-------------------------------------------------------------------------
  // parsing: the scale is recovered from the text
  //-------------------------------------------------------------------------

  test("parse recovers the fixed scale from the digits written after the point") {
    // This is the behaviour the type exists for, and it is behaviour rather than formatting:
    // the same decimal read from text of different widths gives values of different scales,
    // which are different values.
    val texts = Table(
      ("text", "expectedDecimalText", "expectedFixedScale"),
      ("1.5", "1.5", 1),
      ("1.50", "1.5", 2),
      ("1.500", "1.5", 3),
      ("1.50000", "1.5", 5),
      ("1.25", "1.25", 2),
      ("1.250", "1.25", 3),
      ("-2.30", "-2.3", 2),
      ("12.340000000000000000", "12.34", 18))

    forAll(texts) { (text: String, expectedDecimalText: String, expectedFixedScale: Int) =>
      val value = fixed(text)
      value.decimal shouldBe decimalOf(expectedDecimalText)
      value.fixedScale shouldBe expectedFixedScale
      value.toString shouldBe text
    }

    // The two the note singles out: the same decimal at two scales is two values.
    fixed("1.5").fixedScale shouldBe 1
    fixed("1.50").fixedScale shouldBe 2
    fixed("1.5") should not be fixed("1.50")
    fixed("1.5").decimal shouldBe fixed("1.50").decimal
  }

  test("parse of text holding no point names a value of scale zero") {
    val texts = Table(
      ("text", "expectedText"),
      ("0", "0"),
      ("1", "1"),
      ("-2", "-2"),
      ("999999999999999999", "999999999999999999"))

    forAll(texts) { (text: String, expectedText: String) =>
      val value = fixed(text)
      value.fixedScale shouldBe 0
      value.toString shouldBe expectedText
      value.toBigDecimal.scale shouldBe 0
    }
  }

  test("parse rejects text that names no decimal, reporting why the text was unreadable") {
    // The failure comes from the decimal the text is read as, so its reason is parsing rather
    // than invalidity, and the message is the one the decimal reported.
    val rejected = Table(
      ("text", "expectedMessage"),
      ("", "Decimal string must not be empty"),
      ("abc", "Decimal string is invalid: 'abc'"),
      ("1.2.3", "Decimal string is invalid: '1.2.3'"),
      (" 1.5 ", "Decimal string is invalid: ' 1.5 '"),
      ("1,5", "Decimal string is invalid: '1,5'"),
      ("--1", "Decimal string is invalid: '--1'"))

    forAll(rejected) { (text: String, expectedMessage: String) =>
      val outcome = FixedScaleDecimal.parse(text)
      outcome should beFailure
      outcome should beFailureWith(FailureReason.PARSING)
      messagesOf(outcome) shouldBe List(expectedMessage)
    }
  }

  test("parse rejects oversized text by the bound of the decimal, before any scale is derived") {
    // Text is read as a decimal before it is scanned for the point, so the bound the decimal
    // applies to its own text is the single guard on length, and the scale a long fraction
    // would imply is never reached. Three hundred digits after the point would imply a scale
    // no decimal holds, yet the only failure reported is the one the decimal reports on the
    // length of the text - the scale check of `of` contributes nothing to this outcome.
    val oversized = "1." + "0" * 300
    oversized.length shouldBe 302
    val outcome = FixedScaleDecimal.parse(oversized)
    outcome should beFailure
    outcome should beFailureWith(FailureReason.PARSING)
    messagesOf(outcome) shouldBe List("Decimal string must not exceed 256 characters")
    messagesOf(outcome) should not contain aboveMaximumMessage(300)

    // The boundary from the accepting side, so the case above cannot pass for the wrong
    // reason: text of exactly the length a decimal accepts, naming a value of a scale the
    // type holds, is read as it always was. Two hundred and thirty-seven leading zeroes keep
    // the value within eighteen digits of precision while the fraction fills the scale.
    val atBound = "0" * 237 + ".123456789012345678"
    atBound.length shouldBe 256
    val value = fixed(atBound)
    value.decimal shouldBe decimalOf("0.123456789012345678")
    value.fixedScale shouldBe 18
    value.toString shouldBe "0.123456789012345678"

    // One character more is one character too many, whatever it names.
    val pastBound = "0" + atBound
    pastBound.length shouldBe 257
    val rejectedAtBound = FixedScaleDecimal.parse(pastBound)
    rejectedAtBound should beFailure
    rejectedAtBound should beFailureWith(FailureReason.PARSING)
    messagesOf(rejectedAtBound) shouldBe List("Decimal string must not exceed 256 characters")
  }

  test("parse rejects an exponent naming more digits than a decimal holds, without expanding it") {
    // Twelve characters name a number of five hundred million digits. The text is read as a
    // decimal before any scale is derived from it, so the bound the decimal applies to an
    // exponent it cannot hold is the bound this factory inherits: the outcome is the decimal's
    // own rejection, carried in this type's chain of failures, and it is reached without the
    // power-of-ten expansion that bringing such a value to scale zero would perform. The clock
    // is read for that second half - the work is proportional to the text supplied rather than
    // to the number it names - and the bound is three orders of magnitude above the guarded
    // path, so a loaded machine cannot make it flake.
    val startedAt = System.nanoTime()
    val outcome = FixedScaleDecimal.parse("1e500000000")
    val elapsedMillis = (System.nanoTime() - startedAt) / 1000000L
    outcome should beFailure
    outcome should beFailureWith(FailureReason.INVALID)
    messagesOf(outcome) shouldBe
      List("Decimal value must not exceed 18 digits of precision at scale 0: 1E+500000000")
    withClue(s"parsing '1e500000000' took ${elapsedMillis}ms: ") {
      elapsedMillis should be < 5000L
    }

    // The same number reaching this type through `of` rather than through text is refused at
    // the same place, since no decimal can be built from it to pair with a scale at all.
    Decimal.of(new BigDecimal("1e500000000")).isLeft shouldBe true
  }

  test("parse rejects text whose implied scale is beyond the maximum a decimal holds") {
    // Nineteen digits after the point imply a scale no decimal holds. The decimal itself is
    // read - rounded to the precision a decimal supports - and it is the scale that is
    // rejected, by the same check and with the same wording `of` uses.
    val outcome = FixedScaleDecimal.parse("1.1234567890123456789")
    outcome should beFailure
    outcome should beFailureWith(FailureReason.INVALID)
    messagesOf(outcome) shouldBe List(aboveMaximumMessage(19))

    // Eighteen digits are accepted, so the boundary of parsing is the boundary of pairing.
    fixed("1.123456789012345678").fixedScale shouldBe 18
  }

  test("parse normalises the value it reads, so a written zero keeps only its scale") {
    // The decimal is normalised on the way in: trailing zeroes of the fraction and the sign of
    // a negative zero are gone from the decimal, while the width of the text survives as the
    // fixed scale. The value therefore renders as it was written even though the decimal
    // behind it does not.
    val negativeZero = fixed("-0.00")
    negativeZero.decimal shouldBe Decimal.ZERO
    negativeZero.fixedScale shouldBe 2
    negativeZero.toString shouldBe "0.00"
    negativeZero shouldBe fixed("0.00")

    val padded = fixed("1.2000")
    padded.decimal shouldBe decimalOf("1.2")
    padded.decimal.scale shouldBe 1
    padded.fixedScale shouldBe 4
    padded.toString shouldBe "1.2000"
  }

  test("parse reads back the text of a value, for every value the original suite named") {
    val texts = Table(
      ("text"),
      ("0"),
      ("1"),
      ("1.2"),
      ("1.20"),
      ("1.200"),
      ("1.25"),
      ("2.50"),
      ("-2"),
      ("-2.3"),
      ("-2.30"),
      ("999999999999999999"),
      ("0.000000000000000001"))

    forAll(texts) { (text: String) =>
      val value = fixed(text)
      value.toString shouldBe text
      FixedScaleDecimal.parse(value.toString) should haveValue(value)
    }
  }

  //-------------------------------------------------------------------------
  // map: the arithmetic of the decimal, re-validated against the fixed scale
  //-------------------------------------------------------------------------

  test("map reaches the arithmetic of the decimal and keeps the fixed scale of the receiver") {
    val value = fixed("1.25")
    value.map(decimal => decimal.multipliedBy(2L)) should haveValue(fixed("2.50"))
    value.map(decimal => decimal.plus(decimal)) should haveValue(fixed("2.50"))
    value.map(decimal => decimal.negated) should haveValue(fixed("-1.25"))
    value.map(decimal => decimal.multipliedBy(4L)) should haveValue(fixed("5.00"))

    // The fixed scale of the result is that of the receiver, not that of the result's decimal:
    // a result of fewer decimal places is still shown at two.
    val doubled = value.map(decimal => decimal.multipliedBy(4L)).getOrElse(fail("doubling was rejected"))
    doubled.fixedScale shouldBe 2
    doubled.decimal shouldBe decimalOf("5")
    doubled.decimal.scale shouldBe 0
    doubled.toString shouldBe "5.00"
  }

  test("map re-validates the scale, so a result needing more decimal places is rejected") {
    // A third of one has more decimal places than two, so the pairing the function asks for
    // would drop digits and is refused - by the same check, with the same wording, that `of`
    // applies, because `map` is `of` applied to the result of the function.
    val outcome = fixed("1.00").map(decimal => decimal.dividedBy(decimalOf("3")))
    outcome should beFailure
    outcome should beFailureWith(FailureReason.INVALID)
    messagesOf(outcome) shouldBe List(belowDecimalMessage(2))

    // A rounding of the result inside the function is how a caller keeps it acceptable, which
    // is the documented way to reach the arithmetic of the decimal at a fixed scale.
    fixed("1.00").map(decimal =>
      decimal.dividedBy(decimalOf("3")).roundToScale(2, RoundingMode.HALF_UP)) should
      haveValue(fixed("0.33"))
  }

  test("map with the identity function returns the value unchanged") {
    forAll { (value: FixedScaleDecimal) =>
      value.map(decimal => decimal) should haveValue(value)
    }
  }

  //-------------------------------------------------------------------------
  // conversion to BigDecimal
  //-------------------------------------------------------------------------

  test("toBigDecimal carries the fixed scale rather than the scale of the decimal") {
    // The conversion pads the fraction to the fixed scale, so the text of the two agrees, and
    // the scale of the result is the fixed scale in every case - including zero, whose decimal
    // is held at scale zero however it was written.
    val conversions = Table(
      ("text", "expectedScale", "expectedValue"),
      ("1", 0, new BigDecimal("1")),
      ("1.2", 1, new BigDecimal("1.2")),
      ("1.20", 2, new BigDecimal("1.20")),
      ("1.200", 3, new BigDecimal("1.200")),
      ("-2.30", 2, new BigDecimal("-2.30")),
      ("0", 0, new BigDecimal("0")),
      ("0.000", 3, new BigDecimal("0.000")))

    forAll(conversions) { (text: String, expectedScale: Int, expectedValue: BigDecimal) =>
      val value = fixed(text)
      value.toBigDecimal shouldBe expectedValue
      value.toBigDecimal.scale shouldBe expectedScale
      value.toBigDecimal.toPlainString shouldBe text
    }

    // The widest pairing a decimal supports converts too, at the scale it is shown at.
    pair(Decimal.ZERO, Decimal.MAX_SCALE).toBigDecimal.scale shouldBe 18
    pair(Decimal.ZERO, Decimal.MAX_SCALE).toBigDecimal shouldBe
      new BigDecimal("0.000000000000000000")
  }

  //-------------------------------------------------------------------------
  // the three text forms, which are one text
  //-------------------------------------------------------------------------

  test("toString, Show and JSON are the same canonical text") {
    // The rendering of the type being ported was also what its text-form conversion wrote and
    // what its parser read. Here the rendering, the `Show` and the JSON have to agree for the
    // same reason: a value written out and read back has to come back with its scale.
    val texts = Table(
      ("text"),
      ("1"),
      ("1.20"),
      ("-2.30"),
      ("0.000"),
      ("999999999999999999"))

    forAll(texts) { (text: String) =>
      val value = fixed(text)
      value.toString shouldBe text
      Show[FixedScaleDecimal].show(value) shouldBe text
      value.show shouldBe text
      value.asJson shouldBe Json.fromString(text)
      decode[FixedScaleDecimal](value.asJson.noSpaces) shouldBe Right(value)
    }
  }

  test("the JSON form is the bare canonical text, and decoding recovers the scale from its digits") {
    // The value travels as a JSON string and not as a JSON number, because a number cannot
    // carry the scale: `1.20` and `1.2` are one number and two values of this type. The
    // decoder is the type's own `parse`, so the digits written after the point are what the
    // scale is recovered from.
    val value = fixed("1.20")
    value.asJson.noSpaces shouldBe "\"1.20\""
    decode[FixedScaleDecimal]("\"1.20\"") shouldBe Right(value)
    decode[FixedScaleDecimal]("\"1.2\"") shouldBe Right(fixed("1.2"))
    decode[FixedScaleDecimal]("\"1.2\"") should not be Right(value)
    decode[FixedScaleDecimal]("\"1.20\"").map(decoded => decoded.fixedScale) shouldBe Right(2)
    decode[FixedScaleDecimal]("\"1.2\"").map(decoded => decoded.fixedScale) shouldBe Right(1)
  }

  test("the decoder reports the reason the text was rejected") {
    // The codec reports exactly what the factory reported, at the position in the document
    // where the text was found, so both kinds of rejection - unreadable text and an impossible
    // scale - reach a reader with their own message.
    decode[FixedScaleDecimal]("\"abc\"") match {
      case Left(failure: DecodingFailure) => failure.message shouldBe "Decimal string is invalid: 'abc'"
      case other => fail(s"expected a decoding failure but got: $other")
    }
    decode[FixedScaleDecimal]("\"\"") match {
      case Left(failure: DecodingFailure) => failure.message shouldBe "Decimal string must not be empty"
      case other => fail(s"expected a decoding failure but got: $other")
    }
    decode[FixedScaleDecimal]("\"1.1234567890123456789\"") match {
      case Left(failure: DecodingFailure) => failure.message shouldBe aboveMaximumMessage(19)
      case other => fail(s"expected a decoding failure but got: $other")
    }
  }

  test("the decoder rejects a document whose value is not text") {
    decode[FixedScaleDecimal]("1.25") match {
      case Left(failure: DecodingFailure) => failure.message should include("expecting string")
      case other => fail(s"expected a decoding failure but got: $other")
    }
    decode[FixedScaleDecimal]("{}") match {
      case Left(failure: DecodingFailure) => failure.message should include("expecting string")
      case other => fail(s"expected a decoding failure but got: $other")
    }
    decode[FixedScaleDecimal]("[\"1.20\"]") match {
      case Left(failure: DecodingFailure) => failure.message should include("expecting string")
      case other => fail(s"expected a decoding failure but got: $other")
    }
  }

  //-------------------------------------------------------------------------
  // ordering, equality and hashing
  //-------------------------------------------------------------------------

  test("ordering falls through to the fixed scale, where the type being ported stopped at the decimal") {
    // DIVERGENCE, deliberate and recorded in the migration note: the comparison of the type
    // being ported was the comparison of the decimals alone, so `1.2` shown at one decimal
    // place and `1.2` shown at two compared equal while being unequal. `Order` and `Eq` are
    // one instance here and their laws require agreement, so the comparison falls through to
    // the fixed scale and returns zero exactly when the values are equal. Do not "restore"
    // the comparison of the original: doing so falsifies the ordering laws that the dependent
    // module checks for this type.
    val atOne = fixed("1.2")
    val atTwo = fixed("1.20")
    val atFour = fixed("1.2000")
    val ordering = Order[FixedScaleDecimal]

    // The decimals are equal, which is where the comparison of the original stopped.
    atOne.decimal shouldBe atTwo.decimal
    atOne.decimal.compareTo(atTwo.decimal) shouldBe 0

    // The values are not, and the comparison says so.
    Eq[FixedScaleDecimal].eqv(atOne, atTwo) shouldBe false
    ordering.compare(atOne, atTwo) should not be 0
    ordering.compare(atOne, atTwo) should be < 0
    ordering.compare(atTwo, atOne) should be > 0
    atOne.compareTo(atTwo) should be < 0

    // The smaller scale sorts first, and the ordering is total over the three.
    List(atFour, atTwo, atOne).sorted(ordering.toOrdering) shouldBe List(atOne, atTwo, atFour)
    ordering.min(atOne, atTwo) shouldBe atOne
    ordering.max(atOne, atFour) shouldBe atFour

    // The fall-through applies only where the decimals are equal: a larger decimal at a
    // smaller scale is still the larger value.
    ordering.compare(fixed("1.3"), atTwo) should be > 0
    ordering.compare(atTwo, fixed("1.3")) should be < 0
  }

  test("comparison returns zero exactly when two values are equal") {
    // The consistency law, asserted on representative pairs: equal decimal and equal scale is
    // the only case that compares equal.
    val pairs = Table(
      ("left", "right"),
      (fixed("1.20"), fixed("1.20")),
      (fixed("1.20"), fixed("1.2")),
      (fixed("1.20"), fixed("1.200")),
      (fixed("1.20"), fixed("1.21")),
      (fixed("1.20"), fixed("1.19")),
      (fixed("0"), fixed("0.00")),
      (fixed("0"), fixed("0")),
      (fixed("-2.30"), fixed("-2.3")),
      (fixed("-2.30"), fixed("2.30")),
      (fixed("999999999999999999"), fixed("999999999999999999")))

    forAll(pairs) { (left: FixedScaleDecimal, right: FixedScaleDecimal) =>
      (Order[FixedScaleDecimal].compare(left, right) == 0) shouldBe
        Eq[FixedScaleDecimal].eqv(left, right)
      (Order[FixedScaleDecimal].compare(left, right) == 0) shouldBe (left == right)
    }
  }

  test("ordering agrees with the ordering of the decimals where the decimals differ") {
    val pairs = Table(
      ("leftText", "rightText"),
      ("1.19", "1.20"),
      ("1.20", "1.21"),
      ("-2.30", "-1.00"),
      ("-0.01", "0.00"),
      ("0.00", "0.01"),
      ("999999999999999998", "999999999999999999"))

    forAll(pairs) { (leftText: String, rightText: String) =>
      val left = fixed(leftText)
      val right = fixed(rightText)
      Order[FixedScaleDecimal].compare(left, right).sign shouldBe
        left.decimal.compareTo(right.decimal).sign
      Order[FixedScaleDecimal].compare(left, right) should be < 0
    }
  }

  test("equality, ordering and hashing are one implicit, so they cannot disagree") {
    // The summon asks for an instance that is an ordering and a hashing at once, which only
    // the single published implicit can answer; identity is then asserted through behaviour,
    // because a cats instance type does not conform to `AnyRef` and cannot be compared by
    // reference. There is deliberately no separate `Eq`: it is obtained by subtyping.
    val instance: Order[FixedScaleDecimal] with Hash[FixedScaleDecimal] =
      implicitly[Order[FixedScaleDecimal] with Hash[FixedScaleDecimal]]

    val value = fixed("1.20")
    val same = fixed("1.20")
    val other = fixed("1.2")

    Hash[FixedScaleDecimal].hash(value) shouldBe instance.hash(value)
    Order[FixedScaleDecimal].compare(value, other) shouldBe instance.compare(value, other)
    Eq[FixedScaleDecimal].eqv(value, other) shouldBe instance.eqv(value, other)

    // The agreement itself, and the hashing being that of the value's two parts.
    Eq[FixedScaleDecimal].eqv(value, same) shouldBe true
    Order[FixedScaleDecimal].compare(value, same) shouldBe 0
    Hash[FixedScaleDecimal].hash(value) shouldBe Hash[FixedScaleDecimal].hash(same)
    Hash[FixedScaleDecimal].hash(value) shouldBe value.hashCode
    Eq[FixedScaleDecimal].eqv(value, other) shouldBe false
    Order[FixedScaleDecimal].compare(value, other) should not be 0
  }

  test("values key a set and a map by their decimal and their scale") {
    // Equality carrying both halves of the value, a hashed collection distinguishes the same
    // decimal shown at two scales - which is the distinction the type exists to make.
    Set(fixed("1.20"), fixed("1.20"), fixed("1.2")).size shouldBe 2
    Map(fixed("1.20") -> "two places").get(fixed("1.20")) shouldBe Some("two places")
    Map(fixed("1.20") -> "two places").get(fixed("1.2")) shouldBe None
    List(fixed("1.2"), fixed("1.20"), fixed("1.2")).distinct.map(value => value.toString) shouldBe
      List("1.2", "1.20")
  }

  //-------------------------------------------------------------------------
  // properties over the whole domain of the type
  //-------------------------------------------------------------------------

  test("every value round-trips through its canonical text") {
    // The text a value renders as is the text `parse` reads it back from, scale included,
    // which is the contract the JSON codec and every other reader of text rests on.
    forAll { (value: FixedScaleDecimal) =>
      FixedScaleDecimal.parse(value.toString) should haveValue(value)
      value.toString shouldBe value.decimal.formatAtLeast(value.fixedScale)
    }
  }

  test("every value round-trips through JSON") {
    forAll { (value: FixedScaleDecimal) =>
      value.asJson shouldBe Json.fromString(value.toString)
      decode[FixedScaleDecimal](value.asJson.noSpaces) shouldBe Right(value)
    }
  }

  test("of is idempotent over a pairing it has already accepted") {
    forAll { (value: FixedScaleDecimal) =>
      FixedScaleDecimal.of(value.decimal, value.fixedScale) should haveValue(value)
      value.fixedScale should be >= value.decimal.scale
      value.fixedScale should be <= Decimal.MAX_SCALE
    }
  }

  test("comparison is consistent with equality and hashing over generated values") {
    forAll { (left: FixedScaleDecimal, right: FixedScaleDecimal) =>
      val comparison = Order[FixedScaleDecimal].compare(left, right)
      (comparison == 0) shouldBe Eq[FixedScaleDecimal].eqv(left, right)
      (comparison == 0) shouldBe (left == right)
      Order[FixedScaleDecimal].compare(right, left).sign shouldBe -comparison.sign
      if (comparison == 0) {
        Hash[FixedScaleDecimal].hash(left) shouldBe Hash[FixedScaleDecimal].hash(right)
      } else {
        comparison.sign should not be 0
      }
    }
  }

  test("toBigDecimal has the fixed scale of the value it came from") {
    forAll { (value: FixedScaleDecimal) =>
      value.toBigDecimal.scale shouldBe value.fixedScale
      value.toBigDecimal.toPlainString shouldBe value.toString
      value.toBigDecimal.compareTo(value.decimal.toBigDecimal) shouldBe 0
    }
  }
}
