/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.value

import java.math.BigDecimal

import cats.Hash
import cats.Show

import io.circe.Json
import io.circe.parser.decode
import io.circe.parser.parse
import io.circe.syntax._

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor3

import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Test [[HalfUp]], the half-up rounding convention of [[Rounding]].
 *
 * This is a one-to-one port of the Java test class `HalfUpRoundingTest`: each of its thirteen
 * test methods has a test of the same name here, in the same order, and its single twelve-row
 * data provider is transcribed row for row rather than re-derived. The suite keeps the Java
 * name while the subject was renamed - the Java class `HalfUpRounding` is [[HalfUp]] here,
 * because the member name is also the key of the JSON form of the family - so that the mapping
 * from Java test method to Scala test remains a name-to-name join.
 *
 * The provider that drove three Java methods stays one table driving the same three tests, with
 * the row loop inside each test rather than multiplying it, so each of those three remains a
 * single test case and the method-level traceability of the migration is exact.
 *
 * ===What the port changes, and why===
 *
 * Four of the Java methods asserted behaviour this port does not have. Each keeps its name and
 * asserts what replaced it, so nothing is silently lost:
 *
 *   - both factories report a rejection as a value instead of throwing, so every Java
 *     `assertThatIllegalArgumentException` is written here as an assertion about the failures on
 *     the left of an `Either` - the reason compared as a value of the closed family of reasons,
 *     and the message pinned word for word against the Java text. Rejecting a number of decimal
 *     places or a fraction is a data-dependent failure of the kind this port hands back rather
 *     than a caller-contract precondition, so no exception is expected anywhere in this spec.
 *   - `test_builder` and `test_builder_invalid` exercised the Joda-Beans meta-bean builder,
 *     which has no target in this port. They are re-pointed onto the factory that carries the
 *     same validation and the same normalisation the builder was reaching through: the builder
 *     set the two raw fields and the constructor checked them before collapsing a fraction of
 *     one to none, and `ofFractionalDecimalPlaces` checks the same two raw values in the same
 *     order before the same collapse.
 *   - `coverage` called the reflective bean sweeps `coverImmutableBean` and `coverBeanEquals`,
 *     which walked the properties of a Joda-Beans bean through its meta-bean. There is no
 *     meta-bean and no reflective property access here, so their substance is asserted
 *     directly: equality in both directions, hashing, rendering, and the closed construction
 *     surface of a validated type.
 *   - `test_serialization` asserted a Java serialization round trip. Java serialization is not
 *     part of this port at all; the JSON codec of the family takes its place, so the round trip
 *     asserted is `decode(encode(x)) == x` together with the exact shape of the encoding.
 *
 * ===What the three rounding overloads pin===
 *
 * The `Double` and `BigDecimal` overloads are compared exactly, with no tolerance, because that
 * is what the Java test did and because `BigDecimal` equality distinguishes scale: a value
 * scaled up by its fraction, rounded and divided back down comes out at the scale the exact
 * quotient needs, and the fractional rows below depend on it. The `Decimal` overload is compared
 * against a decimal built from the same expected `Double`, which is the comparison the Java test
 * made through `Decimal.of`.
 *
 * Numerical parity with the Java implementation to 1e-9 is not this spec's job - the parity
 * specs discharge that against the captured Java baseline. What this spec keeps is exactly what
 * the Java test asserted: the twelve rows, compared for equality.
 */
final class HalfUpRoundingSpec extends AnyFunSuite with Matchers with TableDrivenPropertyChecks {

  /** The rejection of a number of decimal places outside the permitted range, as Java worded it. */
  private val DecimalPlacesMessage: String = "Invalid decimal places, must be from 0 to 255 inclusive"

  /** The rejection of a fraction outside the permitted range, as Java worded it. */
  private val FractionMessage: String = "Invalid fraction, must be from 0 to 256 inclusive"

  /**
   * The JSON form of the convention that rounds to four decimal places with no fractional part,
   * which is the instance the Java `test_serialization` method serialized.
   */
  private val ExpectedJson: String = """{"HalfUp":{"decimalPlaces":4,"fraction":0}}"""

  /** The JSON form of the convention that rounds to the nearest 1/32nd of the fourth decimal place. */
  private val ExpectedFractionalJson: String = """{"HalfUp":{"decimalPlaces":4,"fraction":32}}"""

  //-------------------------------------------------------------------------
  /**
   * Unwraps the outcome of a factory that is expected to produce a convention.
   *
   * The Java test held conventions built by factories that could throw; here those factories
   * return their failures instead, so a fixture has to unwrap one. Doing that through this
   * helper rather than with a projection and a fabricated fallback keeps a transcription mistake
   * visible: a fixture naming a number of decimal places that is not one fails the suite, naming
   * the failures it was rejected for, instead of quietly testing some other value.
   *
   * The type parameter is what lets one helper serve both factories: given the outcome of a
   * [[HalfUp]] factory it hands back a [[HalfUp]], whose two accessors the tests below read,
   * and given the outcome of a [[Rounding]] factory it hands back the family type.
   *
   * @param result  the outcome of a factory, expected to hold a convention
   * @tparam A  the type of convention the outcome holds
   * @return the convention the outcome holds
   */
  private def rounding[A <: Rounding](result: ResultNec[A]): A =
    result.fold(
      _ => fail(s"invalid rounding fixture: ${messages(result).mkString("; ")}"),
      convention => convention)

  /**
   * The messages of the failures an outcome holds, in the order it holds them.
   *
   * The matchers assert that *some* failure carries a reason or matches a pattern, which is the
   * right question for an outcome that can hold several. This helper answers the other two
   * questions the accumulating factory raises - how many failures there are and which, exactly -
   * so that a chain of two can be pinned to the two messages that belong in it rather than to
   * the presence of one of them.
   *
   * @param result  the outcome to inspect
   * @return the messages of its failures in order, empty when it holds none
   */
  private def messages(result: ResultNec[Rounding]): List[String] =
    result.fold(failures => failures.toChain.toList.map(_.message), _ => List.empty[String])

  /**
   * Builds the decimal a `Double` names, for the expectations of the decimal overload.
   *
   * `Decimal.of` reports a value it cannot hold rather than throwing, and every value used below
   * is an ordinary two- or three-place decimal, so a failure here is a mistake in this spec and
   * is reported as one.
   *
   * @param value  the value the decimal is to name
   * @return the decimal the value names
   */
  private def decimal(value: Double): Decimal =
    Decimal.of(value).fold(
      failure => fail(s"invalid decimal fixture: ${failure.message}"),
      held => held)

  /**
   * Parses one of the expected JSON forms above into the JSON model.
   *
   * Comparing documents is what makes an assertion about the encoding rather than about its
   * rendering: whitespace, field ordering in the printed text and the spelling of a number are
   * matters of presentation, while the fields present, their names and their values are the
   * contract. A literal in this file that does not parse is a defect in the spec itself.
   *
   * @param text  the JSON text to parse
   * @return the parsed document
   */
  private def json(text: String): Json =
    parse(text).getOrElse(fail(s"the expected JSON of this spec is not itself valid JSON: $text"))

  //-------------------------------------------------------------------------
  test("test_of_Currency") {
    val test: Rounding = Rounding.of(Currency.USD)
    test.round(63.455d) shouldBe 63.46d
    test.round(63.454d) shouldBe 63.45d

    // The factory is total, as it was in Java, because the number of minor units of a currency
    // is always a number of decimal places a convention may round to. For the dollar that number
    // is two, so this is the convention that rounds to two decimal places with no fractional
    // part - asserted here so that the two roundings above are tied to a stated convention
    // rather than only to their answers.
    Currency.USD.minorUnitDigits shouldBe 2
    test shouldBe rounding(HalfUp.ofDecimalPlaces(2))
  }

  //-------------------------------------------------------------------------
  test("test_ofDecimalPlaces") {
    val test: HalfUp = rounding(HalfUp.ofDecimalPlaces(4))
    test.decimalPlaces shouldBe 4
    test.fraction shouldBe 0
    test.toString shouldBe "Round to 4dp"

    // The factory of the family returns the same convention as the factory of the member, which
    // is the assertion the Java test made with `Rounding.ofDecimalPlaces(4)`.
    val widened: ResultNec[Rounding] = Rounding.ofDecimalPlaces(4)
    widened should beSuccess
    widened should haveValue(test)
  }

  //-------------------------------------------------------------------------
  test("test_ofDecimalPlaces_big") {
    val test: HalfUp = rounding(HalfUp.ofDecimalPlaces(40))
    test.decimalPlaces shouldBe 40
    test.fraction shouldBe 0
    test.toString shouldBe "Round to 40dp"

    val widened: ResultNec[Rounding] = Rounding.ofDecimalPlaces(40)
    widened should haveValue(test)

    // Forty decimal places is beyond the cache of sixteen conventions, which is the reason this
    // case exists alongside the one above: the Java constructor reached this path with a fraction
    // of one and relied on the collapse of a fraction of one to none, while the cached path was
    // built with none in the first place. Both paths have to end up holding no fractional part,
    // so the two conventions either side of the cache boundary are asserted to do so.
    rounding(HalfUp.ofDecimalPlaces(15)).fraction shouldBe 0
    rounding(HalfUp.ofDecimalPlaces(16)).fraction shouldBe 0
  }

  //-------------------------------------------------------------------------
  test("test_ofDecimalPlaces_invalid") {
    // Where the Java factory threw, this one reports the reason as a value. The reason is
    // compared as a member of the closed family of reasons rather than as text, and the message
    // is pinned both by pattern - which distinguishes it from the fraction message - and word
    // for word against the Java wording.
    val negative: ResultNec[HalfUp] = HalfUp.ofDecimalPlaces(-1)
    negative should beFailureWith(FailureReason.INVALID)
    negative should haveFailureMessageMatching(".*decimal places.*")
    messages(negative) shouldBe List(DecimalPlacesMessage)

    val tooBig: ResultNec[HalfUp] = HalfUp.ofDecimalPlaces(257)
    tooBig should beFailureWith(FailureReason.INVALID)
    tooBig should haveFailureMessageMatching(".*decimal places.*")
    messages(tooBig) shouldBe List(DecimalPlacesMessage)

    // The factory of the family delegates to the factory of the member, so it reports the same
    // failures; the Java test reached this factory too, through `Rounding.ofDecimalPlaces`.
    val negativeWidened: ResultNec[Rounding] = Rounding.ofDecimalPlaces(-1)
    val tooBigWidened: ResultNec[Rounding] = Rounding.ofDecimalPlaces(257)
    negativeWidened should beFailureWith(FailureReason.INVALID)
    tooBigWidened should beFailureWith(FailureReason.INVALID)
    messages(negativeWidened) shouldBe List(DecimalPlacesMessage)
    messages(tooBigWidened) shouldBe List(DecimalPlacesMessage)

    // The edges of the range the message names: 255 decimal places is the largest accepted, and
    // 256 is already too many, which the Java test did not reach and which is what keeps the two
    // ranges of this type - 255 for the decimal places, 256 for the fraction - from being
    // confused with one another.
    rounding(HalfUp.ofDecimalPlaces(255)).decimalPlaces shouldBe 255
    val justTooBig: ResultNec[HalfUp] = HalfUp.ofDecimalPlaces(256)
    justTooBig should beFailureWith(FailureReason.INVALID)
    messages(justTooBig) shouldBe List(DecimalPlacesMessage)
  }

  //-------------------------------------------------------------------------
  test("test_ofFractionalDecimalPlaces") {
    val test: HalfUp = rounding(HalfUp.ofFractionalDecimalPlaces(4, 32))
    test.decimalPlaces shouldBe 4
    test.fraction shouldBe 32
    test.toString shouldBe "Round to 1/32 of 4dp"

    val widened: ResultNec[Rounding] = Rounding.ofFractionalDecimalPlaces(4, 32)
    widened should beSuccess
    widened should haveValue(test)
  }

  //-------------------------------------------------------------------------
  test("test_ofFractionalDecimalPlaces_invalid") {
    // The two inputs are checked separately and the message says which one was rejected, so each
    // of the four Java cases is asserted against the message family that belongs to it.
    val negativePlaces: ResultNec[HalfUp] = HalfUp.ofFractionalDecimalPlaces(-1, 0)
    negativePlaces should beFailureWith(FailureReason.INVALID)
    negativePlaces should haveFailureMessageMatching(".*decimal places.*")
    messages(negativePlaces) shouldBe List(DecimalPlacesMessage)

    val tooManyPlaces: ResultNec[HalfUp] = HalfUp.ofFractionalDecimalPlaces(257, 0)
    tooManyPlaces should beFailureWith(FailureReason.INVALID)
    tooManyPlaces should haveFailureMessageMatching(".*decimal places.*")
    messages(tooManyPlaces) shouldBe List(DecimalPlacesMessage)

    val negativeFraction: ResultNec[HalfUp] = HalfUp.ofFractionalDecimalPlaces(0, -1)
    negativeFraction should beFailureWith(FailureReason.INVALID)
    negativeFraction should haveFailureMessageMatching(".*fraction.*")
    messages(negativeFraction) shouldBe List(FractionMessage)

    val tooBigFraction: ResultNec[HalfUp] = HalfUp.ofFractionalDecimalPlaces(0, 257)
    tooBigFraction should beFailureWith(FailureReason.INVALID)
    tooBigFraction should haveFailureMessageMatching(".*fraction.*")
    messages(tooBigFraction) shouldBe List(FractionMessage)

    // The same four cases through the factory of the family, which the Java test also reached.
    val widened: ResultNec[Rounding] = Rounding.ofFractionalDecimalPlaces(0, 257)
    widened should beFailureWith(FailureReason.INVALID)
    messages(widened) shouldBe List(FractionMessage)

    // A fraction of 256 is the largest accepted, which is one more than the largest number of
    // decimal places and is why the two messages name different bounds.
    rounding(HalfUp.ofFractionalDecimalPlaces(0, 256)).fraction shouldBe 256

    // Both inputs wrong at once is the case the accumulating validation of this port buys over
    // the fail-fast constructor it replaces: the Java constructor checked the decimal places
    // first and threw, so a caller passing two bad values learned about one of them, while this
    // factory reports both in one call, in the order the two checks are written.
    val bothWrong: ResultNec[HalfUp] = HalfUp.ofFractionalDecimalPlaces(-1, 257)
    bothWrong should beFailureWith(FailureReason.INVALID)
    bothWrong should haveFailureMessageMatching(".*decimal places.*")
    bothWrong should haveFailureMessageMatching(".*fraction.*")
    messages(bothWrong).size shouldBe 2
    messages(bothWrong) shouldBe List(DecimalPlacesMessage, FractionMessage)
  }

  //-------------------------------------------------------------------------
  test("test_builder") {
    // The Joda-Beans meta-bean builder this Java method exercised has no target in this port.
    // The factory carries the validation and the normalisation the builder was reaching through -
    // the builder set the two raw fields and the constructor checked them before collapsing a
    // fraction of one to none - so the Java case, the two fields set to four and one, is asserted
    // against that factory.
    val test: HalfUp = rounding(HalfUp.ofFractionalDecimalPlaces(4, 1))
    test.decimalPlaces shouldBe 4
    test.fraction shouldBe 0
    test.toString shouldBe "Round to 4dp"

    // A fraction of one scales the value by one before rounding and back afterwards, which is the
    // rounding performed with no fractional part at all, so these three spellings all name the
    // same convention and an instance never holds a fraction of one.
    test shouldBe rounding(HalfUp.ofFractionalDecimalPlaces(4, 0))
    test shouldBe rounding(HalfUp.ofDecimalPlaces(4))

    // The collapse is applied after the checks and is idempotent: handing the fraction an
    // instance holds back to the factory yields an equal instance, whether that fraction is none
    // or a real one.
    rounding(HalfUp.ofFractionalDecimalPlaces(4, test.fraction)) shouldBe test
    val fractional: HalfUp = rounding(HalfUp.ofFractionalDecimalPlaces(4, 32))
    rounding(HalfUp.ofFractionalDecimalPlaces(4, fractional.fraction)) shouldBe fractional
  }

  //-------------------------------------------------------------------------
  test("test_builder_invalid") {
    // The same re-pointing as above, for the four cases the Java method built through the
    // meta-bean builder. What they pin is the order of the two steps: the raw inputs are checked
    // before the collapse of a fraction of one to none, so a fraction of -1 is reported rather
    // than absorbed by a normalisation that would otherwise map anything at or below one to none.
    // The first two cases set only the decimal places, which leaves the fraction at zero, so they
    // are asserted through the factory that takes the decimal places alone.
    val negativePlaces: ResultNec[HalfUp] = HalfUp.ofDecimalPlaces(-1)
    negativePlaces should beFailureWith(FailureReason.INVALID)
    negativePlaces should haveFailureMessageMatching(".*decimal places.*")
    messages(negativePlaces) shouldBe List(DecimalPlacesMessage)

    val tooManyPlaces: ResultNec[HalfUp] = HalfUp.ofDecimalPlaces(257)
    tooManyPlaces should beFailureWith(FailureReason.INVALID)
    tooManyPlaces should haveFailureMessageMatching(".*decimal places.*")
    messages(tooManyPlaces) shouldBe List(DecimalPlacesMessage)

    val negativeFraction: ResultNec[HalfUp] = HalfUp.ofFractionalDecimalPlaces(4, -1)
    negativeFraction should beFailureWith(FailureReason.INVALID)
    negativeFraction should haveFailureMessageMatching(".*fraction.*")
    messages(negativeFraction) shouldBe List(FractionMessage)

    val tooBigFraction: ResultNec[HalfUp] = HalfUp.ofFractionalDecimalPlaces(4, 257)
    tooBigFraction should beFailureWith(FailureReason.INVALID)
    tooBigFraction should haveFailureMessageMatching(".*fraction.*")
    messages(tooBigFraction) shouldBe List(FractionMessage)
  }

  //-------------------------------------------------------------------------
  /**
   * The provider shared by `round_double`, `round_BigDecimal` and `round_Decimal`, transcribed
   * row for row from the Java `data_round`.
   *
   * The first six rows round to two decimal places with no fractional part and are the half-way
   * cases either side of a tie: a value below the tie rounds down, the tie itself rounds away
   * from zero, and a value above it rounds up. The last six round to the nearest half of the
   * second decimal place - a fraction of two - which is why their answers land on a third decimal
   * digit of five.
   */
  private val data_round: TableFor3[HalfUp, Double, Double] = Table(
    ("rounding", "input", "expected"),
    (rounding(HalfUp.ofDecimalPlaces(2)), 12.3449d, 12.34d),
    (rounding(HalfUp.ofDecimalPlaces(2)), 12.3450d, 12.35d),
    (rounding(HalfUp.ofDecimalPlaces(2)), 12.3451d, 12.35d),
    (rounding(HalfUp.ofDecimalPlaces(2)), 12.3500d, 12.35d),
    (rounding(HalfUp.ofDecimalPlaces(2)), 12.3549d, 12.35d),
    (rounding(HalfUp.ofDecimalPlaces(2)), 12.3550d, 12.36d),
    (rounding(HalfUp.ofFractionalDecimalPlaces(2, 2)), 12.3424d, 12.340d),
    (rounding(HalfUp.ofFractionalDecimalPlaces(2, 2)), 12.3425d, 12.345d),
    (rounding(HalfUp.ofFractionalDecimalPlaces(2, 2)), 12.3426d, 12.345d),
    (rounding(HalfUp.ofFractionalDecimalPlaces(2, 2)), 12.3449d, 12.345d),
    (rounding(HalfUp.ofFractionalDecimalPlaces(2, 2)), 12.3450d, 12.345d),
    (rounding(HalfUp.ofFractionalDecimalPlaces(2, 2)), 12.3451d, 12.345d))

  //-------------------------------------------------------------------------
  test("round_double") {
    forAll(data_round) { (convention: HalfUp, input: Double, expected: Double) =>
      convention.round(input) shouldBe expected
    }
  }

  //-------------------------------------------------------------------------
  test("round_BigDecimal") {
    // Compared with the equality of `BigDecimal`, which distinguishes scale, exactly as the Java
    // assertion did: the fractional rows come back at the scale the exact quotient of the
    // division by the fraction needs, and a value rounded to two decimal places comes back at a
    // scale of two even when its last digit is a zero.
    forAll(data_round) { (convention: HalfUp, input: Double, expected: Double) =>
      convention.round(BigDecimal.valueOf(input)) shouldBe BigDecimal.valueOf(expected)
    }
  }

  //-------------------------------------------------------------------------
  test("round_Decimal") {
    forAll(data_round) { (convention: HalfUp, input: Double, expected: Double) =>
      convention.round(decimal(input)) shouldBe decimal(expected)
    }
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    val test: HalfUp = rounding(HalfUp.ofDecimalPlaces(4))
    val test2: HalfUp = rounding(HalfUp.ofFractionalDecimalPlaces(4, 32))

    // Built independently from equal inputs, as the second instance of the Java sweep was, so
    // that equality is shown to be structural rather than by reference. `Hash` is the family's
    // single equality-bearing instance and has to agree with the platform equality in both
    // directions, so both are asserted, along with the hash of the instance.
    val same: HalfUp = rounding(HalfUp.ofFractionalDecimalPlaces(4, 0))
    same shouldBe test
    same.hashCode shouldBe test.hashCode
    Hash[Rounding].eqv(same, test) shouldBe true
    Hash[Rounding].hash(same) shouldBe test.hashCode

    // Either field differing is enough to make a convention unequal, under the platform equality
    // and under the typeclass alike. This is the relation the Java original expressed as the
    // packed code `(decimalPlaces << 16) + fraction`, in which no two distinct pairs within the
    // permitted ranges collide, so comparing the two fields is the same relation stated directly.
    test2 should not be test
    Hash[Rounding].eqv(test2, test) shouldBe false
    val otherPlaces: HalfUp = rounding(HalfUp.ofDecimalPlaces(5))
    otherPlaces should not be test
    Hash[Rounding].eqv(otherPlaces, test) shouldBe false

    // The other member of the closed family is a different convention again, which is what the
    // family-level instance has to report for a comparison across members.
    Rounding.none should not be test
    Hash[Rounding].eqv(Rounding.none, test) shouldBe false

    // Every spelling of "four decimal places, no fractional part" is that one convention, because
    // the factories collapse a fraction of one to none before the instance is built.
    rounding(HalfUp.ofFractionalDecimalPlaces(4, 1)) shouldBe test
    rounding(HalfUp.ofFractionalDecimalPlaces(4, 0)) shouldBe test
    rounding(HalfUp.ofDecimalPlaces(4)) shouldBe test

    // `Show` renders what `toString` renders, and what `toString` renders is the form of the Java
    // original, pinned as a literal so a change to it cannot pass unnoticed.
    Show[Rounding].show(test) shouldBe test.toString
    Show[Rounding].show(test2) shouldBe test2.toString
    test.toString shouldBe "Round to 4dp"
    test2.toString shouldBe "Round to 1/32 of 4dp"

    // A convention outside the permitted ranges cannot be built, and this is where that claim is
    // proved rather than asserted about one input: the primary constructor is private and neither
    // an `apply` nor a `copy` exists, so the only way in is a factory that validates. Both facts
    // are checked by compiling the snippet and requiring it to fail.
    assertDoesNotCompile("""HalfUp(4, 0)""")
    assertDoesNotCompile("""Rounding.HalfUp(4, 0)""")
    assertDoesNotCompile("""HalfUp.ofDecimalPlaces(4).map(_.copy(decimalPlaces = 2))""")
    assertDoesNotCompile("""Rounding.ofDecimalPlaces(4).map(_.copy())""")

    // Reading the two fields by pattern is unaffected by that closure - `unapply` is available -
    // which is what keeps a ported call site that matched on the bean readable.
    val matched: (Int, Int) = test2 match {
      case HalfUp(places, fraction) => (places, fraction)
    }
    matched shouldBe ((4, 32))
  }

  //-------------------------------------------------------------------------
  test("test_serialization") {
    // Java serialization is not part of this port; the JSON codec of the family takes its place.
    // A convention is written as an object of one field, whose name is the member and whose value
    // holds that member's fields, and both directions are asserted - an encoding that is wrong
    // and a decoding that is wrong in the same way would still round trip.
    val test: Rounding = rounding(HalfUp.ofDecimalPlaces(4))
    test.asJson shouldBe json(ExpectedJson)
    decode[Rounding](test.asJson.noSpaces) shouldBe Right(test)
    decode[Rounding](ExpectedJson) shouldBe Right(test)

    val fractional: Rounding = rounding(HalfUp.ofFractionalDecimalPlaces(4, 32))
    fractional.asJson shouldBe json(ExpectedFractionalJson)
    decode[Rounding](fractional.asJson.noSpaces) shouldBe Right(fractional)
    decode[Rounding](ExpectedFractionalJson) shouldBe Right(fractional)

    // The decoder routes the fields through the same validating factory a caller's arguments go
    // through, so a document naming a number of decimal places or a fraction outside the
    // permitted range is rejected rather than decoded into a convention the factory would never
    // have built. The message of the rejection names which of the two inputs was at fault.
    val rejectedPlaces = decode[Rounding]("""{"HalfUp":{"decimalPlaces":-1,"fraction":0}}""")
    rejectedPlaces.isLeft shouldBe true
    rejectedPlaces.swap.toOption.fold("")(error => error.getMessage) should include("decimal places")

    val rejectedFraction = decode[Rounding]("""{"HalfUp":{"decimalPlaces":4,"fraction":257}}""")
    rejectedFraction.isLeft shouldBe true
    rejectedFraction.swap.toOption.fold("")(error => error.getMessage) should include("fraction")
  }
}
