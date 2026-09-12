/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import java.time.Duration
import java.time.LocalDate

import scala.collection.immutable.List
import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.immutable.SortedMap
import scala.collection.immutable.Vector

import cats.Show
import cats.data.NonEmptyChain
import cats.data.Validated
import cats.syntax.all._

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import com.opengamma.strata.collect.ArgCheckTables.durationOrder
import com.opengamma.strata.collect.ArgCheckTables.localDateOrder
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureOr
import com.opengamma.strata.collect.result.FailureReason
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.result.ValidatedFailures
import com.opengamma.strata.collect.testkit.ResultMatchers._

/**
 * Tests [[Validate]], the accumulating half of the validation vocabulary of this module.
 *
 * ===What is under test===
 *
 * Every member of [[Validate]]: the six members that build an outcome directly - `valid`,
 * `invalid`, `invalidNec`, `cond`, `fromResult` and `toResult` - and then the predicate
 * vocabulary it shares with [[ArgCheck]]: the two boolean checks, the two forms of `matches`,
 * the blankness check, the seven `notEmpty` overloads, the two duplicate checks, the four
 * sign families over int, long, double and decimal, the not-a-number check, the two
 * tolerance-bearing checks, the nine range checks and the two order checks.
 *
 * Three things separate this spec from [[ArgCheckSpec]], and they are the reason this file
 * exists rather than a second assertion inside that one:
 *
 *   - '''the outcome is a value'''. Every check here answers with `Valid` carrying the
 *     argument it checked, or with `Invalid` carrying a [[Failure]]. A passing case can
 *     therefore assert the payload, which the fail-fast half cannot, and a failing case reads
 *     the message out of a failure rather than out of a thrown error;
 *   - '''the wording is shared'''. Both objects report the same text for the same input, and
 *     the parity section below asserts that by driving both surfaces from one argument and
 *     comparing the two strings. That is what makes the shared fixture in [[ArgCheckTables]]
 *     more than a convenience: a message reworded on either object fails a test here;
 *   - '''failures accumulate'''. Combining two checks with `mapN` keeps the failures of both,
 *     which is the behaviour the fail-fast half cannot express at all. The accumulation
 *     section asserts the whole chain, in order, for two, three and four independently
 *     failing checks, and contrasts it with the sequencing `andThen` performs.
 *
 * ===The shape of a failure===
 *
 * Every predicate reports exactly one `Failure.Invalid`, whose message is the text the shared
 * fixture records and whose attributes are empty. A failing case asserts all three at once by
 * comparing the accumulated failures with `List(Failure.Invalid(expectedMessage))`, since the
 * case class carries an empty attribute map by default; the reason is asserted separately
 * through `beFailureWith` so that a change of reason is reported as such. The three members
 * that take a failure from the caller - `invalid`, `cond` and `fromResult` - are the only way
 * a different reason or a populated attribute map reaches an outcome, and each has a case
 * below that proves it passes the failure through untouched.
 *
 * ===What is shared with the spec for `ArgCheck`===
 *
 * The inputs and messages live in [[ArgCheckTables]], at the top of `ArgCheckSpec.scala`, and
 * both specs iterate them; nothing is duplicated here. Each predicate section ends with two
 * table-driven tests, one over the invalid rows and one over the valid rows. One table,
 * [[ArgCheckTables.invalidNotEmptyMatrix]], is deliberately not read here: the matrix overload
 * exists on [[ArgCheck]] alone, and the section of checks with no target proves that the call
 * does not compile against this object.
 *
 * Several valid tables hold a not-a-number value, which does not equal itself, so a payload of
 * type double is compared by its bit pattern rather than by equality. That also makes the
 * comparison stricter than equality elsewhere, since it separates the two signed zeros.
 *
 * ===Which cases of the Java original are answered here===
 *
 * The case inventory comes from the test class of the Java original, whose 134 methods are
 * answered across this spec and [[ArgCheckSpec]]. The methods this spec answers are the sign
 * family - `notPositive` over int, long, double and decimal, and `notPositiveIfPresent` - and
 * the seventeen methods that exercised the checks for an absent reference, `notNull`,
 * `notNullItem` and `noNulls`, which have no target because absence is described by `Option`
 * here and a reference is never empty of a referent. Those seventeen are answered rather than
 * dropped: the section of checks with no target rejects the call that would have been made, at
 * compile time, and exercises the expression that replaces it. The remaining predicates of the
 * original are covered here too, in full, because the accumulating surface needs its own
 * evidence for each of them.
 *
 * @see [[ArgCheckSpec]] for the fail-fast half and for the shared fixture
 */
class ValidateSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  /**
   * The argument name every check in this spec is called under.
   *
   * The expected message text of every row of [[ArgCheckTables]] names this argument, so the
   * calls and the expectations are tied to one another through this value rather than through
   * a literal repeated in both places.
   */
  private val Name: String = ArgCheckTables.ArgumentName

  /** The name the two order checks report their first argument under. */
  private val FirstName: String = ArgCheckTables.FirstName

  /** The name the two order checks report their second argument under. */
  private val SecondName: String = ArgCheckTables.SecondName

  //-------------------------------------------------------------------------
  /**
   * Reads the failures out of an outcome, in the order they accumulated.
   *
   * A passing outcome has none, which is what lets the same method serve a case that asserts
   * a specific chain and a case that asserts there is nothing to report.
   *
   * @param outcome  the outcome of one check, or of several combined
   * @return the failures it carries, in order
   */
  private def failuresOf(outcome: ValidatedFailures[Any]): List[Failure] =
    outcome.fold(chain => chain.toChain.toList, _ => List.empty[Failure])

  /**
   * Reads the messages of the failures of an outcome, in the order they accumulated.
   *
   * @param outcome  the outcome of one check, or of several combined
   * @return the message of each failure, in order
   */
  private def messagesOf(outcome: ValidatedFailures[Any]): List[String] =
    failuresOf(outcome).map(failure => failure.message)

  /**
   * Reads the message of an outcome that is expected to have failed for exactly one reason.
   *
   * The test fails if the outcome passed, or if it accumulated more than one failure, so the
   * arity of the failure is asserted by every case that uses this.
   *
   * @param outcome  the outcome expected to carry one failure
   * @return the message of that failure
   */
  private def messageOf(outcome: ValidatedFailures[Any]): String =
    failuresOf(outcome) match {
      case single :: Nil => single.message
      case Nil => fail("Expected a failing check but the check passed")
      case several => fail(s"Expected one failure but found ${several.size}: ${several.mkString(", ")}")
    }

  /**
   * Reads the value out of an outcome that is expected to have passed.
   *
   * The outcome is matched rather than unwrapped, so a failing outcome is reported as a test
   * failure naming what went wrong instead of raising an error from a partial accessor.
   *
   * @tparam A  the type of the checked value
   * @param outcome  the outcome expected to carry a value
   * @return the value it carries
   */
  private def valueOf[A](outcome: ValidatedFailures[A]): A =
    outcome match {
      case Validated.Valid(checked) => checked
      case Validated.Invalid(reported) =>
        fail(s"Expected a passing check but it failed with: ${reported.toChain.toList.mkString(", ")}")
    }

  /**
   * Reads the bit pattern of a double an outcome carries.
   *
   * Several valid rows of the shared fixture hold a not-a-number value, which does not equal
   * itself, so a case that asserts the value a check gave back compares bit patterns. That is
   * also stricter than equality, since the two signed zeros differ here.
   *
   * @param outcome  the outcome expected to carry a double
   * @return the bit pattern of that double
   */
  private def bitsOf(outcome: ValidatedFailures[Double]): Long =
    java.lang.Double.doubleToLongBits(valueOf(outcome))

  /**
   * Asserts that an outcome failed exactly once, with the specified message, as every
   * predicate of [[Validate]] is documented to fail.
   *
   * The comparison covers the reason, the message and the attributes together: the reason is
   * asserted through the matcher, and the equality against a freshly built `Failure.Invalid`
   * covers the message and proves the attribute map is empty, since that is the default the
   * case class carries.
   *
   * @param outcome  the outcome expected to carry one failure
   * @param expectedMessage  the complete text that failure is expected to report
   * @return the assertion that both hold
   */
  private def rejects(outcome: ValidatedFailures[Any], expectedMessage: String): Assertion = {
    outcome should beFailureWith(FailureReason.INVALID)
    failuresOf(outcome) shouldBe List(Failure.Invalid(expectedMessage))
  }

  /**
   * Runs the fail-fast counterpart of a check and reads the message it threw.
   *
   * This is used only by the parity section, which drives both objects from one argument; the
   * type of the error is asserted here, so a parity case also proves that the fail-fast half
   * is still the half that throws.
   *
   * @param check  the [[ArgCheck]] call expected to fail
   * @return the message of the error it threw
   */
  private def thrownMessageOf(check: => Unit): String =
    intercept[IllegalArgumentException](check).getMessage

  //-------------------------------------------------------------------------
  // The published fixture, and the failure shape every predicate reports.

  test("the shared tables this spec reads are the ones the fail-fast spec reads") {
    ArgCheckTables.ArgumentName shouldBe "name"
    ArgCheckTables.FirstName shouldBe "a"
    ArgCheckTables.SecondName shouldBe "b"
    ArgCheckTables.ToleranceName shouldBe "tolerance"
  }

  test("every predicate reports the reason this module assigns to invalid input") {
    Validate.notBlank(" ", Name) should beFailureWith(FailureReason.INVALID)
    Validate.notPositive(1, Name) should beFailureWith(FailureReason.INVALID)
    Validate.notNaN(Double.NaN, Name) should beFailureWith(FailureReason.INVALID)
    Validate.inRange(2, 0, 2, Name) should beFailureWith(FailureReason.INVALID)
  }

  test("the reason a predicate reports is named INVALID in upper underscore form") {
    val failure = Failure.Invalid("Message")
    failure.reason shouldBe FailureReason.INVALID
    failure.reason.name shouldBe "INVALID"
    messageOf(Validate.notBlank(" ", Name)) shouldBe "Argument 'name' must not be blank"
    failuresOf(Validate.notBlank(" ", Name)).map(item => item.reason.name) shouldBe List("INVALID")
  }

  test("the failure a predicate reports carries no attributes") {
    failuresOf(Validate.notBlank(" ", Name)).map(item => item.attributes) shouldBe
      List(SortedMap.empty[String, String])
    failuresOf(Validate.notPositive(1, Name)).flatMap(item => item.attributes.keys) shouldBe
      List.empty[String]
    failuresOf(Validate.notZero(0.0, Name)).forall(item => item.attributes.isEmpty) shouldBe true
  }

  test("a failing predicate reports exactly one failure") {
    failuresOf(Validate.notBlank(" ", Name)) should have size 1
    failuresOf(Validate.notEmpty("", Name)) should have size 1
    failuresOf(Validate.noDuplicates(Array(1.0, 1.0), Name)) should have size 1
  }

  test("a passing predicate reports no failure at all") {
    failuresOf(Validate.notBlank("OG", Name)) shouldBe List.empty[Failure]
    Validate.notBlank("OG", Name) should beSuccess
  }

  test("a rejected argument is reported as Invalid holding a chain of exactly one failure") {
    Validate.notBlank(" ", Name) shouldBe
      Validated.invalid[NonEmptyChain[Failure], String](
        NonEmptyChain.one(Failure.Invalid("Argument 'name' must not be blank")))
    Validate.notPositive(1, Name) shouldBe
      Validated.invalid[NonEmptyChain[Failure], Int](
        NonEmptyChain.one(Failure.Invalid("Argument 'name' must not be positive but has value 1")))
  }

  test("a passing predicate is reported as Valid holding the argument it checked") {
    Validate.notBlank("OG", Name) shouldBe
      Validated.valid[NonEmptyChain[Failure], String]("OG")
    Validate.notPositive(-1, Name) shouldBe Validated.valid[NonEmptyChain[Failure], Int](-1)
  }

  test("the message of a failure names the argument under check, wherever the check reads it") {
    Validate.notBlank(" ", Name) should haveFailureMessageMatching(".*'name'.*must not be blank")
    Validate.notPositive(1, Name) should haveFailureMessageMatching(".*'name'.*has value 1")
    Validate.notNaN(Double.NaN, Name) should haveFailureMessageMatching(".*'name'.*must not be NaN")
    Validate.inRange(2, 0, 2, Name) should haveFailureMessageMatching(".*'name'.*but found 2")
    Validate.inOrderNotEqual(
      LocalDate.of(2011, 7, 3),
      LocalDate.of(2011, 7, 2),
      FirstName,
      SecondName) should haveFailureMessageMatching(".*'a'.*'b'.*")
  }

  test("every rejected row of the shared fixture names the argument in the message it reports") {
    forAll(ArgCheckTables.invalidNotPositiveDouble) { (argument, _) =>
      Validate.notPositive(argument, Name) should haveFailureMessageMatching(".*'name'.*")
    }
    forAll(ArgCheckTables.invalidNotNegativeOrZeroInt) { (argument, _) =>
      Validate.notNegativeOrZero(argument, Name) should haveFailureMessageMatching(".*'name'.*")
    }
    forAll(ArgCheckTables.invalidNotEmptyIterable) { (argument, _) =>
      Validate.notEmpty(argument, Name) should haveFailureMessageMatching(".*'name'.*")
    }
  }


  //-------------------------------------------------------------------------
  // valid, the passing outcome a factory reaches for when a value needs no checking.

  test("valid carries the value it was handed") {
    Validate.valid(3) should haveValue(3)
    Validate.valid("OG") should haveValue("OG")
    valueOf(Validate.valid(List(1, 2, 3))) shouldBe List(1, 2, 3)
  }

  test("valid reports no failure, so it combines without disturbing the others") {
    val outcome =
      (Validate.valid(3), Validate.notBlank("OG", Name)).mapN((number, text) => (number, text))
    outcome should haveValue((3, "OG"))
    failuresOf(outcome) shouldBe List.empty[Failure]
  }

  test("valid keeps the identity of the value rather than a copy of it") {
    val values = Vector(1.0, 2.0)
    valueOf(Validate.valid(values)) should be theSameInstanceAs values
  }

  //-------------------------------------------------------------------------
  // invalid, the escape hatch that reports a failure the caller built.

  test("invalid carries the failure it was handed, untouched") {
    val failure = Failure.MissingData("No calendar for 'GBXX'")
    val outcome = Validate.invalid[Int](failure)
    outcome should beFailure
    failuresOf(outcome) shouldBe List(failure)
  }

  test("invalid preserves a reason other than the one the predicates report") {
    val outcome = Validate.invalid[String](Failure.Parsing("Cannot parse 'XYZ'"))
    outcome should beFailureWith(FailureReason.PARSING)
    messageOf(outcome) shouldBe "Cannot parse 'XYZ'"
  }

  test("invalid preserves the attributes of the failure it was handed") {
    val failure = Failure.Invalid("Schedule must not be empty").withAttribute("definition", "P3M")
    val outcome = Validate.invalid[Int](failure)
    failuresOf(outcome).map(item => item.attributes) shouldBe
      List(SortedMap("definition" -> "P3M"))
  }

  test("invalid reports exactly one failure, held in a chain of one") {
    val failure = Failure.Other("Something else")
    Validate.invalid[Int](failure) shouldBe
      Validated.invalid[NonEmptyChain[Failure], Int](NonEmptyChain.one(failure))
  }

  //-------------------------------------------------------------------------
  // invalidNec, the escape hatch for a caller that only needs its own wording.

  test("invalidNec reports the wording it was given under the reason for invalid input") {
    val outcome = Validate.invalidNec[Int]("Amounts must all be in GBP")
    outcome should beFailureWith(FailureReason.INVALID)
    failuresOf(outcome) shouldBe List(Failure.Invalid("Amounts must all be in GBP"))
  }

  test("invalidNec leaves the attributes of its failure empty") {
    failuresOf(Validate.invalidNec[Int]("Message")).map(item => item.attributes) shouldBe
      List(SortedMap.empty[String, String])
  }

  //-------------------------------------------------------------------------
  // cond, the general form for a condition none of the predicates expresses.

  test("cond produces the value when its condition holds") {
    Validate.cond(true, 3, Failure.Invalid("Message")) should haveValue(3)
  }

  test("cond reports the failure when its condition does not hold") {
    rejects(Validate.cond[Int](false, 3, Failure.Invalid("Message")), "Message")
  }

  test("cond passes a caller-built failure through with its reason and attributes") {
    val failure = Failure.CurrencyConversion("No rate for EUR/JPY").withAttribute("pair", "EUR/JPY")
    val outcome = Validate.cond[Double](false, 1.0, failure)
    outcome should beFailureWith(FailureReason.CURRENCY_CONVERSION)
    failuresOf(outcome) shouldBe List(failure)
  }

  test("cond builds the value it produces only when its condition holds") {
    val outcome = Validate.cond[Int](false, fail("the value was built"), Failure.Invalid("Message"))
    rejects(outcome, "Message")
  }

  test("cond builds the failure it reports only when its condition fails") {
    val outcome = Validate.cond[Int](true, 3, fail("the failure was built"))
    outcome should haveValue(3)
  }

  //-------------------------------------------------------------------------
  // fromResult, which lets a value built by a failing factory join the expression.

  test("fromResult lifts a right-biased success into the accumulating form") {
    val outcome: FailureOr[Int] = Right(3)
    Validate.fromResult(outcome) should haveValue(3)
  }

  test("fromResult lifts a right-biased failure into the accumulating form") {
    val failure = Failure.MissingData("Nothing here")
    val outcome: FailureOr[Int] = Left(failure)
    failuresOf(Validate.fromResult(outcome)) shouldBe List(failure)
  }

  test("fromResult lifts the failure of a real factory with the wording that factory chose") {
    val outcome = Validate.fromResult(Decimal.of(Double.NaN))
    outcome should beFailureWith(FailureReason.INVALID)
    messageOf(outcome) shouldBe "Decimal value must be finite: NaN"
  }

  test("fromResult preserves a reason the predicates never report") {
    val outcome = Validate.fromResult(Decimal.parse("bad"))
    outcome should beFailureWith(FailureReason.PARSING)
    messageOf(outcome) shouldBe "Decimal string is invalid: 'bad'"
  }

  test("fromResult lifts the value of a real factory that succeeded") {
    val outcome = Validate.fromResult(Decimal.parse("1.2"))
    valueOf(outcome).toString shouldBe "1.2"
  }

  test("fromResult accumulates alongside a predicate rather than short-circuiting it") {
    val outcome = (Validate.fromResult(Decimal.of(Double.NaN)), Validate.notBlank("", Name))
      .mapN((amount, text) => (amount, text))
    messagesOf(outcome) shouldBe
      List("Decimal value must be finite: NaN", "Argument 'name' must not be blank")
  }

  //-------------------------------------------------------------------------
  // toResult, the conversion a factory performs last.

  test("toResult converts a passing outcome into the result a factory returns") {
    Validate.toResult(Validate.notBlank("OG", Name)) shouldBe Right("OG")
  }

  test("toResult converts a failing outcome, keeping every failure in order") {
    val outcome = (Validate.notBlank("", Name), Validate.notPositive(1, Name))
      .mapN((text, number) => (text, number))
    Validate.toResult(outcome) shouldBe Left(
      NonEmptyChain.of(
        Failure.Invalid("Argument 'name' must not be blank"),
        Failure.Invalid("Argument 'name' must not be positive but has value 1")))
  }

  test("toResult performs the same conversion as toEither on the expression itself") {
    val passing = Validate.notPositive(-1, Name)
    val failing = Validate.notPositive(1, Name)
    Validate.toResult(passing) shouldBe passing.toEither
    Validate.toResult(failing) shouldBe failing.toEither
  }

  test("a right-biased success round-trips through the accumulating form") {
    val original: FailureOr[Int] = Right(3)
    val roundTripped: ResultNec[Int] = Validate.toResult(Validate.fromResult(original))
    roundTripped shouldBe Right(3)
  }

  test("a right-biased failure round-trips through the accumulating form") {
    val failure = Failure.Unsupported("Not supported")
    val original: FailureOr[Int] = Left(failure)
    val roundTripped: ResultNec[Int] = Validate.toResult(Validate.fromResult(original))
    roundTripped shouldBe Left(NonEmptyChain.one(failure))
  }

  test("the three steps of a validating factory read as the documentation of Validate shows") {
    def rate(value: Double, tenor: Int): ResultNec[(Double, Int)] =
      Validate.toResult(
        (Validate.notNaN(value, "rate"), Validate.notNegativeOrZero(tenor, "tenor"))
          .mapN((checkedRate, checkedTenor) => (checkedRate, checkedTenor)))
    rate(0.01, 3) shouldBe Right((0.01, 3))
    rate(Double.NaN, 0) shouldBe Left(
      NonEmptyChain.of(
        Failure.Invalid("Argument 'rate' must not be NaN"),
        Failure.Invalid("Argument 'tenor' must not be negative or zero but has value 0")))
  }


  //-------------------------------------------------------------------------
  // isTrue.

  test("isTrue without a message passes a true expression") {
    Validate.isTrue(true) should beSuccess
    valueOf(Validate.isTrue(true)) shouldBe (())
  }

  test("isTrue without a message reports a false expression with the standard wording") {
    rejects(Validate.isTrue(false), "Invalid argument, expression must be true")
  }

  test("isTrue with a message passes a true expression") {
    Validate.isTrue(true, "Message") should beSuccess
  }

  test("isTrue with a message reports that message and nothing else") {
    rejects(Validate.isTrue(false, "Message"), "Message")
  }

  test("isTrue with an interpolated message passes a true expression") {
    val text = "A"
    val count = 2
    val amount = 3.0
    Validate.isTrue(true, s"Message $text $count $amount") should beSuccess
  }

  test("isTrue reports a message interpolating three values of different types") {
    val text = "A"
    val count = 2
    val amount = 3.0
    rejects(Validate.isTrue(false, s"Message $text $count $amount"), "Message A 2 3.0")
  }

  test("isTrue with a message interpolating a long passes a true expression") {
    val days = 3L
    Validate.isTrue(true, s"Message $days") should beSuccess
  }

  test("isTrue reports a message interpolating a long as that long") {
    val days = 3L
    rejects(Validate.isTrue(false, s"Message $days"), "Message 3")
  }

  test("isTrue with a message interpolating a double passes a true expression") {
    val amount = 3.0
    Validate.isTrue(true, s"Message $amount") should beSuccess
  }

  test("isTrue reports a message interpolating a double as that double") {
    val amount = 3.0
    rejects(Validate.isTrue(false, s"Message $amount"), "Message 3.0")
  }

  test("isTrue builds its message only on the failing path") {
    Validate.isTrue(true, throw new IllegalStateException("the message was built")) should beSuccess
    an[IllegalStateException] should be thrownBy {
      Validate.isTrue(false, throw new IllegalStateException("the message was built"))
    }
  }

  //-------------------------------------------------------------------------
  // isFalse.

  test("isFalse passes a false expression") {
    Validate.isFalse(false, "Message") should beSuccess
    valueOf(Validate.isFalse(false, "Message")) shouldBe (())
  }

  test("isFalse reports its message and nothing else for a true expression") {
    rejects(Validate.isFalse(true, "Message"), "Message")
  }

  test("isFalse with an interpolated message passes a false expression") {
    val text = "A"
    val count = 2
    val amount = 3.0
    Validate.isFalse(false, s"Message $text $count $amount") should beSuccess
  }

  test("isFalse reports a message interpolating three values of different types") {
    val text = "A"
    val count = 2
    val amount = 3.0
    rejects(Validate.isFalse(true, s"Message $text $count $amount"), "Message A 2 3.0")
  }

  test("isFalse builds its message only on the failing path") {
    Validate.isFalse(false, throw new IllegalStateException("the message was built")) should beSuccess
    an[IllegalStateException] should be thrownBy {
      Validate.isFalse(true, throw new IllegalStateException("the message was built"))
    }
  }

  test("isFalse has no form without a message, as in the original") {
    assertDoesNotCompile("Validate.isFalse(false)")
  }

  //-------------------------------------------------------------------------
  // The checks that have no target on this object.
  //
  // The seventeen methods of the original that guarded against an absent reference have no
  // counterpart here: absence is an Option, and a reference is never empty of a referent. The
  // matrix overload of notEmpty has none either, for a different reason - an empty numeric
  // buffer is a caller-contract breach rather than a property of user data, so it belongs to
  // the fail-fast half. Each case below rejects the call that would have been made and
  // exercises the expression that replaces it at a call site.

  test("the check for a present reference has no target, because a reference is never absent") {
    assertDoesNotCompile("""Validate.notNull("OG", "name")""")
    val present: Option[String] = Some("OG")
    present.traverse(value => Validate.notEmpty(value, Name)) should haveValue(Some("OG"))
  }

  test("an absent value is an empty Option, and the check it guards never runs") {
    val absent: Option[String] = None
    val outcome = absent.traverse(value => Validate.notEmpty(value, Name))
    outcome should haveValue(Option.empty[String])
    failuresOf(outcome) shouldBe List.empty[Failure]
  }

  test("the item form of the present-reference check has no target either") {
    assertDoesNotCompile("""Validate.notNullItem("OG")""")
    assertDoesNotCompile("""Validate.notNullItem("OG", "name")""")
  }

  test("an absent item is an empty Option and reaches no check") {
    val items: List[Option[String]] = List(Some("A"), None, Some("B"))
    items.flatten shouldBe List("A", "B")
    Validate.notEmpty(items.flatten, Name) should haveValue(List("A", "B"))
  }

  test("the array form of the check for absent elements has no target") {
    assertDoesNotCompile("""Validate.noNulls(Array("Element"), "name")""")
  }

  test("an empty array of optional elements narrows to no elements and is then rejected as empty") {
    val elements: Array[Option[String]] = Array.empty
    elements.flatten.toList shouldBe List.empty[String]
    rejects(Validate.notEmpty(elements.flatten, Name), "Argument array 'name' must not be empty")
  }

  test("an array of optional elements is narrowed to its present elements rather than checked") {
    val elements: Array[Option[String]] = Array(Some("Element"), None)
    elements.flatten.toList shouldBe List("Element")
    Validate.notEmpty(elements.flatten, Name) should beSuccess
  }

  test("an absent array is an empty Option, so only a present array is checked") {
    val absent: Option[Array[String]] = None
    val present: Option[Array[String]] = Some(Array.empty[String])
    absent.traverse(values => Validate.notEmpty(values, Name)) should beSuccess
    rejects(
      present.traverse(values => Validate.notEmpty(values, Name)),
      "Argument array 'name' must not be empty")
  }

  test("the iterable form of the check for absent elements has no target") {
    assertDoesNotCompile("""Validate.noNulls(List("Element"), "name")""")
  }

  test("an empty iterable of optional elements narrows to no elements") {
    val elements: List[Option[String]] = List.empty
    elements.flatten shouldBe List.empty[String]
    rejects(Validate.notEmpty(elements.flatten, Name), "Argument iterable 'name' must not be empty")
  }

  test("an iterable of optional elements is narrowed to its present elements rather than checked") {
    val elements: List[Option[String]] = List(Some("Element"), None)
    elements.flatten shouldBe List("Element")
    Validate.notEmpty(elements.flatten, Name) should haveValue(List("Element"))
  }

  test("an absent iterable is an empty Option, so only a present iterable is checked") {
    val absent: Option[List[String]] = None
    val present: Option[List[String]] = Some(List.empty[String])
    absent.traverse(values => Validate.notEmpty(values, Name)) should beSuccess
    rejects(
      present.traverse(values => Validate.notEmpty(values, Name)),
      "Argument iterable 'name' must not be empty")
  }

  test("the map form of the check for absent entries has no target") {
    assertDoesNotCompile("""Validate.noNulls(Map("A" -> "B"), "name")""")
  }

  test("an empty map of optional values narrows to no entries") {
    val entries: Map[String, Option[String]] = Map.empty
    val present = entries.collect { case (mapKey, Some(mapValue)) => mapKey -> mapValue }
    present shouldBe Map.empty[String, String]
    rejects(Validate.notEmpty(present, Name), "Argument map 'name' must not be empty")
  }

  test("a map built from optional keys keeps only the entries whose key is present") {
    val entries: List[(Option[String], String)] = List((Some("A"), "B"), (None, "Z"))
    val present = entries.collect { case (Some(mapKey), mapValue) => mapKey -> mapValue }.toMap
    present shouldBe Map("A" -> "B")
    Validate.notEmpty(present, Name) should haveValue(Map("A" -> "B"))
  }

  test("a map built from optional values keeps only the entries whose value is present") {
    val entries: Map[String, Option[String]] = Map("A" -> Some("B"), "Z" -> None)
    val present = entries.collect { case (mapKey, Some(mapValue)) => mapKey -> mapValue }
    present shouldBe Map("A" -> "B")
    Validate.notEmpty(present, Name) should beSuccess
  }

  test("the matrix overload of notEmpty belongs to the fail-fast object alone") {
    assertDoesNotCompile("""Validate.notEmpty(ArgCheckTables.EmptyMatrix, "name")""")
    forAll(ArgCheckTables.invalidNotEmptyMatrix) { (argument, expectedMessage) =>
      thrownMessageOf(ArgCheck.notEmpty(argument, Name)) shouldBe expectedMessage
    }
  }

  test("Validate is a singleton object, so there is neither a constructor nor a subclass of it") {
    assertDoesNotCompile("new Validate")
    assertDoesNotCompile("class Extended extends Validate")
    Validate.isTrue(true) should beSuccess
  }


  //-------------------------------------------------------------------------
  // matches, against a pattern.

  test("matches accepts an argument the pattern matches in full, carrying it through") {
    Validate.matches("[A-Z]+".r, "OG", Name) should haveValue("OG")
  }

  test("matches requires the whole argument to match, not a part of it") {
    rejects(
      Validate.matches("[A-Z]+".r, "OG1", Name),
      "Argument 'name' with value 'OG1' must match pattern: [A-Z]+")
  }

  test("matches quotes both the argument and the pattern in the failure it reports") {
    messageOf(Validate.matches("[0-9]{2}".r, "1", Name)) shouldBe
      "Argument 'name' with value '1' must match pattern: [0-9]{2}"
  }

  test("matches rejects empty text unless the pattern admits it") {
    rejects(
      Validate.matches("[A-Z]+".r, "", Name),
      "Argument 'name' with value '' must match pattern: [A-Z]+")
    Validate.matches("[A-Z]*".r, "", Name) should haveValue("")
  }

  test("matches reports every pattern and argument the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidMatchesRegex) { (pattern, argument, expectedMessage) =>
      rejects(Validate.matches(pattern, argument, Name), expectedMessage)
    }
  }

  test("matches accepts every pattern and argument the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validMatchesRegex) { (pattern, argument) =>
      Validate.matches(pattern, argument, Name) should haveValue(argument)
    }
  }

  //-------------------------------------------------------------------------
  // matches, against a predicate over characters and a pair of length bounds.

  test("the character form of matches accepts an argument within its bounds") {
    Validate.matches(ArgCheckTables.UpperCaseLetter, 1, 2, "OG", Name, "[A-Z]{1,2}") should
      haveValue("OG")
  }

  test("the character form of matches rejects an argument shorter than its lower bound") {
    rejects(
      Validate.matches(ArgCheckTables.UpperCaseLetter, 2, 3, "A", Name, "[A-Z]{2,3}"),
      "Argument 'name' with value 'A' must match pattern: [A-Z]{2,3}")
  }

  test("the character form of matches rejects an argument longer than its upper bound") {
    rejects(
      Validate.matches(ArgCheckTables.UpperCaseLetter, 1, 2, "ABC", Name, "[A-Z]{1,2}"),
      "Argument 'name' with value 'ABC' must match pattern: [A-Z]{1,2}")
  }

  test("the character form of matches rejects a character its predicate refuses") {
    rejects(
      Validate.matches(ArgCheckTables.UpperCaseLetter, 1, Int.MaxValue, "Og", Name, "[A-Z]+"),
      "Argument 'name' with value 'Og' must match pattern: [A-Z]+")
  }

  test("the character form of matches quotes the readable pattern it was given, not the predicate") {
    messageOf(
      Validate.matches(ArgCheckTables.UpperCaseLetter, 1, Int.MaxValue, "123", Name, "[A-Z]+")) shouldBe
      "Argument 'name' with value '123' must match pattern: [A-Z]+"
  }

  test("the character form of matches reports every argument the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidMatchesPredicate) {
      (matcher, minLength, maxLength, argument, equivalentRegex, expectedMessage) =>
        rejects(
          Validate.matches(matcher, minLength, maxLength, argument, Name, equivalentRegex),
          expectedMessage)
    }
  }

  test("the character form of matches accepts every argument the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validMatchesPredicate) {
      (matcher, minLength, maxLength, argument, equivalentRegex) =>
        Validate.matches(matcher, minLength, maxLength, argument, Name, equivalentRegex) should
          haveValue(argument)
    }
  }

  test("matches names the argument it rejected in full, and the failure renders bounded and on one line") {
    // No counterpart in the ported tests: the check being ported interpolated the argument into
    // the message as it stood, and this port does the same, so the wording is that one
    // character for character. What the port adds is the boundary at which such a message is
    // written out - the rendering of a failure bounds every part it writes and escapes anything
    // that could forge a line. Both forms of the check are asserted, because both build their
    // message here.
    val payload = "H" * 10000
    val bounded = Validate.matches("[A-Z]{2}".r, payload, Name)
    bounded should beFailureWith(FailureReason.INVALID)
    messageOf(bounded) shouldBe
      s"Argument 'name' with value '$payload' must match pattern: [A-Z]{2}"
    messageOf(Validate.matches(ArgCheckTables.UpperCaseLetter, 1, 2, payload, Name, "[A-Z]{1,2}")) shouldBe
      s"Argument 'name' with value '$payload' must match pattern: [A-Z]{1,2}"
    // The rendering is where the size stops: a ten-thousand-character argument renders to a
    // line of a few hundred characters, marked to say that there was more.
    val rendered = Show[Failure].show(failuresOf(bounded).head)
    rendered.length should be < 1000
    rendered should startWith("INVALID: Argument 'name' with value 'HHH")
    rendered should endWith("...")

    // An argument holding a line break is named as it stands and rendered on one line, so a
    // line-oriented consumer of the rendering cannot be made to record a line the library did
    // not report.
    val injected = Validate.matches("[A-Z]{7}".r, "EUR\nUSD", Name)
    messageOf(injected) shouldBe "Argument 'name' with value 'EUR\nUSD' must match pattern: [A-Z]{7}"
    val injectedRendering = Show[Failure].show(failuresOf(injected).head)
    injectedRendering should not include "\n"
    injectedRendering should not include "\r"
    injectedRendering shouldBe
      "INVALID: Argument 'name' with value 'EUR\\nUSD' must match pattern: [A-Z]{7}"

    // And the message for an ordinary rejected argument is unchanged, character for character,
    // which is what makes the bound invisible to every caller but the adversarial one, and is
    // why the parity section below still reads one wording off both halves of the vocabulary.
    messageOf(Validate.matches("[A-Z]+".r, "OG1", Name)) shouldBe
      "Argument 'name' with value 'OG1' must match pattern: [A-Z]+"
  }

  //-------------------------------------------------------------------------
  // notBlank.

  test("notBlank accepts text holding a character that is not whitespace") {
    Validate.notBlank("OG", Name) should haveValue("OG")
  }

  test("notBlank carries untrimmed text through unchanged rather than trimming it") {
    Validate.notBlank(" OG ", Name) should haveValue(" OG ")
    valueOf(Validate.notBlank(" OG ", Name)) shouldBe " OG "
  }

  test("notBlank rejects text holding nothing but whitespace") {
    rejects(Validate.notBlank("   ", Name), "Argument 'name' must not be blank")
    rejects(Validate.notBlank("\t\n", Name), "Argument 'name' must not be blank")
  }

  test("notBlank reports every argument the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotBlank) { (argument, expectedMessage) =>
      rejects(Validate.notBlank(argument, Name), expectedMessage)
    }
  }

  test("notBlank accepts every argument the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotBlank) { argument =>
      Validate.notBlank(argument, Name) should haveValue(argument)
    }
  }

  //-------------------------------------------------------------------------
  // notEmpty, over text.

  test("notEmpty accepts text holding at least one character, whitespace included") {
    Validate.notEmpty("OG", Name) should haveValue("OG")
    Validate.notEmpty(" ", Name) should haveValue(" ")
  }

  test("notEmpty rejects text holding no character at all") {
    rejects(Validate.notEmpty("", Name), "Argument 'name' must not be empty")
  }

  test("notEmpty reports every text the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotEmptyString) { (argument, expectedMessage) =>
      rejects(Validate.notEmpty(argument, Name), expectedMessage)
    }
  }

  test("notEmpty accepts every text the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotEmptyString) { argument =>
      Validate.notEmpty(argument, Name) should haveValue(argument)
    }
  }

  //-------------------------------------------------------------------------
  // notEmpty, over an array of references.

  test("notEmpty accepts a populated array of references and carries that very array through") {
    val argument = Array("Element")
    valueOf(Validate.notEmpty(argument, Name)) should be theSameInstanceAs argument
  }

  test("notEmpty rejects an empty array of references with the array wording") {
    rejects(
      Validate.notEmpty(Array.empty[String], Name),
      "Argument array 'name' must not be empty")
  }

  test("notEmpty reports every array of references the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotEmptyObjectArray) { (argument, expectedMessage) =>
      rejects(Validate.notEmpty(argument, Name), expectedMessage)
    }
  }

  test("notEmpty accepts every array of references the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotEmptyObjectArray) { argument =>
      valueOf(Validate.notEmpty(argument, Name)) should be theSameInstanceAs argument
    }
  }

  test("notEmpty reads a nested array by its outer length, whatever its elements hold") {
    val argument = Array(Array.empty[String])
    valueOf(Validate.notEmpty(argument, Name)) should be theSameInstanceAs argument
  }

  test("notEmpty reports every nested array the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotEmptyNestedArray) { (argument, expectedMessage) =>
      rejects(Validate.notEmpty(argument, Name), expectedMessage)
    }
  }

  test("notEmpty accepts every nested array the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotEmptyNestedArray) { argument =>
      valueOf(Validate.notEmpty(argument, Name)) should be theSameInstanceAs argument
    }
  }

  //-------------------------------------------------------------------------
  // notEmpty, over the three arrays of primitives.

  test("notEmpty accepts a populated array of ints and carries that very array through") {
    val argument = Array(6)
    valueOf(Validate.notEmpty(argument, Name)) should be theSameInstanceAs argument
  }

  test("notEmpty reports every array of ints the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotEmptyIntArray) { (argument, expectedMessage) =>
      rejects(Validate.notEmpty(argument, Name), expectedMessage)
    }
  }

  test("notEmpty accepts every array of ints the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotEmptyIntArray) { argument =>
      valueOf(Validate.notEmpty(argument, Name)) should be theSameInstanceAs argument
    }
  }

  test("notEmpty accepts a populated array of longs and carries that very array through") {
    val argument = Array(6L)
    valueOf(Validate.notEmpty(argument, Name)) should be theSameInstanceAs argument
  }

  test("notEmpty reports every array of longs the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotEmptyLongArray) { (argument, expectedMessage) =>
      rejects(Validate.notEmpty(argument, Name), expectedMessage)
    }
  }

  test("notEmpty accepts every array of longs the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotEmptyLongArray) { argument =>
      valueOf(Validate.notEmpty(argument, Name)) should be theSameInstanceAs argument
    }
  }

  test("notEmpty counts an array of doubles rather than reading it, so a NaN element passes") {
    val argument = Array(Double.NaN)
    valueOf(Validate.notEmpty(argument, Name)) should be theSameInstanceAs argument
  }

  test("notEmpty reports every array of doubles the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotEmptyDoubleArray) { (argument, expectedMessage) =>
      rejects(Validate.notEmpty(argument, Name), expectedMessage)
    }
  }

  test("notEmpty accepts every array of doubles the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotEmptyDoubleArray) { argument =>
      valueOf(Validate.notEmpty(argument, Name)) should be theSameInstanceAs argument
    }
  }

  //-------------------------------------------------------------------------
  // notEmpty, over an iterable and over a map.

  test("notEmpty accepts every populated collection shape through the iterable form") {
    Validate.notEmpty(List("Element"), Name) should haveValue(List("Element"))
    Validate.notEmpty(Vector("A", "B"), Name) should haveValue(Vector("A", "B"))
    Validate.notEmpty(Set("A"), Name) should haveValue(Set("A"))
  }

  test("notEmpty rejects an empty iterable with the iterable wording, whatever its shape") {
    rejects(
      Validate.notEmpty(List.empty[String], Name),
      "Argument iterable 'name' must not be empty")
    rejects(
      Validate.notEmpty(Set.empty[String], Name),
      "Argument iterable 'name' must not be empty")
  }

  test("notEmpty reads an iterable by its size, so an empty element passes") {
    Validate.notEmpty(List(""), Name) should haveValue(List(""))
  }

  test("notEmpty reports every iterable the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotEmptyIterable) { (argument, expectedMessage) =>
      rejects(Validate.notEmpty(argument, Name), expectedMessage)
    }
  }

  test("notEmpty accepts every iterable the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotEmptyIterable) { argument =>
      Validate.notEmpty(argument, Name) should haveValue(argument)
    }
  }

  test("notEmpty accepts a populated map and carries its entries through") {
    Validate.notEmpty(Map("Element" -> "Element"), Name) should
      haveValue(Map("Element" -> "Element"))
  }

  test("notEmpty rejects an empty map with the map wording, ordered or not") {
    rejects(
      Validate.notEmpty(Map.empty[String, String], Name),
      "Argument map 'name' must not be empty")
    rejects(
      Validate.notEmpty(SortedMap.empty[String, String], Name),
      "Argument map 'name' must not be empty")
  }

  test("notEmpty reports every map the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotEmptyMap) { (argument, expectedMessage) =>
      rejects(Validate.notEmpty(argument, Name), expectedMessage)
    }
  }

  test("notEmpty accepts every map the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotEmptyMap) { argument =>
      Validate.notEmpty(argument, Name) should haveValue(argument)
    }
  }


  //-------------------------------------------------------------------------
  // noDuplicates.

  test("noDuplicates accepts an array holding no repeated value and carries that very array") {
    val argument = Array(0.0, 1.0, 10.0, 5.0)
    valueOf(Validate.noDuplicates(argument, Name)) should be theSameInstanceAs argument
  }

  test("noDuplicates accepts an array too short to repeat anything") {
    Validate.noDuplicates(Array.empty[Double], Name) should beSuccess
    Validate.noDuplicates(Array(1.0), Name) should beSuccess
  }

  test("noDuplicates rejects a repeated value wherever it sits in the array") {
    rejects(
      Validate.noDuplicates(Array(1.0, 1.0), Name),
      "Argument array 'name' must not contain duplicates")
    rejects(
      Validate.noDuplicates(Array(0.0, 1.0, 10.0, 5.0, 1.0), Name),
      "Argument array 'name' must not contain duplicates")
  }

  test("noDuplicates compares by bit pattern, so two NaNs repeat and the signed zeros do not") {
    rejects(
      Validate.noDuplicates(Array(0.0, Double.NaN, Double.NaN), Name),
      "Argument array 'name' must not contain duplicates")
    Validate.noDuplicates(Array(0.0, -0.0), Name) should beSuccess
  }

  test("noDuplicates accepts an array in any order, being no test of order") {
    Validate.noDuplicates(Array(10.0, 1.0, 5.0, 0.0), Name) should beSuccess
  }

  test("noDuplicates reports every array the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNoDuplicates) { (argument, expectedMessage) =>
      rejects(Validate.noDuplicates(argument, Name), expectedMessage)
    }
  }

  test("noDuplicates accepts every array the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNoDuplicates) { argument =>
      valueOf(Validate.noDuplicates(argument, Name)) should be theSameInstanceAs argument
    }
  }

  //-------------------------------------------------------------------------
  // noDuplicatesSorted.

  test("noDuplicatesSorted accepts an array whose values increase strictly") {
    val argument = Array(0.0, 1.0, 5.0, 10.0)
    valueOf(Validate.noDuplicatesSorted(argument, Name)) should be theSameInstanceAs argument
  }

  test("noDuplicatesSorted accepts an array too short to be out of order") {
    Validate.noDuplicatesSorted(Array.empty[Double], Name) should beSuccess
    Validate.noDuplicatesSorted(Array(1.0), Name) should beSuccess
  }

  test("noDuplicatesSorted reports a repeat as a duplicate rather than as disorder") {
    rejects(
      Validate.noDuplicatesSorted(Array(0.0, 1.0, 5.0, 5.0, 10.0), Name),
      "Argument array 'name' must not contain duplicates")
  }

  test("noDuplicatesSorted reports a value out of order with the wording for order") {
    rejects(
      Validate.noDuplicatesSorted(Array(0.0, 1.0, 5.0, 10.0, 4.0), Name),
      "Argument array 'name' must be sorted and not contain duplicates")
  }

  test("noDuplicatesSorted compares arithmetically, so the signed zeros repeat each other") {
    rejects(
      Validate.noDuplicatesSorted(Array(0.0, -0.0), Name),
      "Argument array 'name' must not contain duplicates")
  }

  test("noDuplicatesSorted passes over a NaN, which neither equals nor orders against a neighbour") {
    Validate.noDuplicatesSorted(Array(0.0, 1.0, 5.0, Double.NaN, 10.0), Name) should beSuccess
  }

  test("noDuplicatesSorted admits the two infinities as the bounds of an ordered array") {
    Validate.noDuplicatesSorted(
      Array(Double.NegativeInfinity, 0.0, Double.PositiveInfinity),
      Name) should beSuccess
  }

  test("noDuplicatesSorted reports every array the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNoDuplicatesSorted) { (argument, expectedMessage) =>
      rejects(Validate.noDuplicatesSorted(argument, Name), expectedMessage)
    }
  }

  test("noDuplicatesSorted accepts every array the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNoDuplicatesSorted) { argument =>
      valueOf(Validate.noDuplicatesSorted(argument, Name)) should be theSameInstanceAs argument
    }
  }

  //-------------------------------------------------------------------------
  // notPositive, over an int and over a long.

  test("notPositive accepts an int that is zero or less, carrying that int through") {
    Validate.notPositive(0, Name) should haveValue(0)
    Validate.notPositive(-1, Name) should haveValue(-1)
    Validate.notPositive(Int.MinValue, Name) should haveValue(Int.MinValue)
  }

  test("notPositive rejects a positive int, quoting the value it found") {
    rejects(
      Validate.notPositive(1, Name),
      "Argument 'name' must not be positive but has value 1")
    rejects(
      Validate.notPositive(Int.MaxValue, Name),
      "Argument 'name' must not be positive but has value 2147483647")
  }

  test("notPositive reports every int the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotPositiveInt) { (argument, expectedMessage) =>
      rejects(Validate.notPositive(argument, Name), expectedMessage)
    }
  }

  test("notPositive accepts every int the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotPositiveInt) { argument =>
      Validate.notPositive(argument, Name) should haveValue(argument)
    }
  }

  test("notPositive accepts a long that is zero or less, carrying that long through") {
    Validate.notPositive(0L, Name) should haveValue(0L)
    Validate.notPositive(Long.MinValue, Name) should haveValue(Long.MinValue)
  }

  test("notPositive rejects a positive long, quoting the value it found") {
    rejects(
      Validate.notPositive(Long.MaxValue, Name),
      "Argument 'name' must not be positive but has value 9223372036854775807")
  }

  test("notPositive reports every long the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotPositiveLong) { (argument, expectedMessage) =>
      rejects(Validate.notPositive(argument, Name), expectedMessage)
    }
  }

  test("notPositive accepts every long the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotPositiveLong) { argument =>
      Validate.notPositive(argument, Name) should haveValue(argument)
    }
  }

  //-------------------------------------------------------------------------
  // notPositive, over a double and over a decimal.

  test("notPositive accepts a double that is zero or less, keeping the sign of a signed zero") {
    Validate.notPositive(0.0, Name) should haveValue(0.0)
    bitsOf(Validate.notPositive(-0.0, Name)) shouldBe java.lang.Double.doubleToLongBits(-0.0)
    Validate.notPositive(Double.NegativeInfinity, Name) should haveValue(Double.NegativeInfinity)
  }

  test("notPositive admits a NaN, which does not order against zero") {
    Validate.notPositive(Double.NaN, Name) should beSuccess
    bitsOf(Validate.notPositive(Double.NaN, Name)) shouldBe
      java.lang.Double.doubleToLongBits(Double.NaN)
  }

  test("notPositive rejects a positive double however small, and an infinite one") {
    rejects(
      Validate.notPositive(1.0E-9, Name),
      "Argument 'name' must not be positive but has value 1.0E-9")
    rejects(
      Validate.notPositive(Double.PositiveInfinity, Name),
      "Argument 'name' must not be positive but has value Infinity")
  }

  test("notPositive reports every double the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotPositiveDouble) { (argument, expectedMessage) =>
      rejects(Validate.notPositive(argument, Name), expectedMessage)
    }
  }

  test("notPositive accepts every double the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotPositiveDouble) { argument =>
      Validate.notPositive(argument, Name) should beSuccess
      bitsOf(Validate.notPositive(argument, Name)) shouldBe
        java.lang.Double.doubleToLongBits(argument)
    }
  }

  test("notPositive accepts a decimal that is zero or less, carrying that decimal through") {
    Validate.notPositive(ArgCheckTables.DecimalZero, Name) should
      haveValue(ArgCheckTables.DecimalZero)
    Validate.notPositive(ArgCheckTables.DecimalNegative, Name) should
      haveValue(ArgCheckTables.DecimalNegative)
  }

  test("notPositive rejects a positive decimal, quoting it as the decimal renders") {
    rejects(
      Validate.notPositive(ArgCheckTables.DecimalPositive, Name),
      "Argument 'name' must not be positive but has value 1.2")
  }

  test("notPositive reports every decimal the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotPositiveDecimal) { (argument, expectedMessage) =>
      rejects(Validate.notPositive(argument, Name), expectedMessage)
    }
  }

  test("notPositive accepts every decimal the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotPositiveDecimal) { argument =>
      Validate.notPositive(argument, Name) should haveValue(argument)
    }
  }

  //-------------------------------------------------------------------------
  // notPositiveIfPresent, which is where the absent-reference check of the original went.

  test("notPositiveIfPresent passes an absent decimal, there being nothing to check") {
    val outcome = Validate.notPositiveIfPresent(None, Name)
    outcome should beSuccess
    outcome should haveValue(Option.empty[Decimal])
  }

  test("notPositiveIfPresent carries a present decimal back inside its Option") {
    Validate.notPositiveIfPresent(Some(ArgCheckTables.DecimalZero), Name) should
      haveValue(Some(ArgCheckTables.DecimalZero))
    Validate.notPositiveIfPresent(Some(ArgCheckTables.DecimalMinusOne), Name) should
      haveValue(Some(ArgCheckTables.DecimalMinusOne))
  }

  test("notPositiveIfPresent rejects a present positive decimal with the wording of the check it wraps") {
    rejects(
      Validate.notPositiveIfPresent(Some(ArgCheckTables.DecimalOne), Name),
      "Argument 'name' must not be positive but has value 1")
  }

  test("notPositiveIfPresent reports every Option the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotPositiveIfPresent) { (argument, expectedMessage) =>
      rejects(Validate.notPositiveIfPresent(argument, Name), expectedMessage)
    }
  }

  test("notPositiveIfPresent accepts every Option the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotPositiveIfPresent) { argument =>
      Validate.notPositiveIfPresent(argument, Name) should haveValue(argument)
    }
  }

  //-------------------------------------------------------------------------
  // notNegative, over an int, a long, a double and a decimal.

  test("notNegative accepts an int that is zero or greater, carrying that int through") {
    Validate.notNegative(0, Name) should haveValue(0)
    Validate.notNegative(Int.MaxValue, Name) should haveValue(Int.MaxValue)
  }

  test("notNegative rejects a negative int, quoting the value it found") {
    rejects(
      Validate.notNegative(-1, Name),
      "Argument 'name' must not be negative but has value -1")
    rejects(
      Validate.notNegative(Int.MinValue, Name),
      "Argument 'name' must not be negative but has value -2147483648")
  }

  test("notNegative reports every int the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotNegativeInt) { (argument, expectedMessage) =>
      rejects(Validate.notNegative(argument, Name), expectedMessage)
    }
  }

  test("notNegative accepts every int the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotNegativeInt) { argument =>
      Validate.notNegative(argument, Name) should haveValue(argument)
    }
  }

  test("notNegative accepts a long that is zero or greater, carrying that long through") {
    Validate.notNegative(0L, Name) should haveValue(0L)
    Validate.notNegative(Long.MaxValue, Name) should haveValue(Long.MaxValue)
  }

  test("notNegative rejects a negative long, quoting the value it found") {
    rejects(
      Validate.notNegative(Long.MinValue, Name),
      "Argument 'name' must not be negative but has value -9223372036854775808")
  }

  test("notNegative reports every long the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotNegativeLong) { (argument, expectedMessage) =>
      rejects(Validate.notNegative(argument, Name), expectedMessage)
    }
  }

  test("notNegative accepts every long the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotNegativeLong) { argument =>
      Validate.notNegative(argument, Name) should haveValue(argument)
    }
  }

  test("notNegative accepts a negative zero, which compares equal to zero") {
    Validate.notNegative(-0.0, Name) should beSuccess
    bitsOf(Validate.notNegative(-0.0, Name)) shouldBe java.lang.Double.doubleToLongBits(-0.0)
  }

  test("notNegative admits a NaN over doubles, for the reason the positive check does") {
    Validate.notNegative(Double.NaN, Name) should beSuccess
  }

  test("notNegative rejects a negative double however small, and an infinite one") {
    rejects(
      Validate.notNegative(-1.0E-9, Name),
      "Argument 'name' must not be negative but has value -1.0E-9")
    rejects(
      Validate.notNegative(Double.NegativeInfinity, Name),
      "Argument 'name' must not be negative but has value -Infinity")
  }

  test("notNegative reports every double the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotNegativeDouble) { (argument, expectedMessage) =>
      rejects(Validate.notNegative(argument, Name), expectedMessage)
    }
  }

  test("notNegative accepts every double the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotNegativeDouble) { argument =>
      Validate.notNegative(argument, Name) should beSuccess
      bitsOf(Validate.notNegative(argument, Name)) shouldBe
        java.lang.Double.doubleToLongBits(argument)
    }
  }

  test("notNegative accepts a decimal that is zero or greater, carrying that decimal through") {
    Validate.notNegative(ArgCheckTables.DecimalZero, Name) should
      haveValue(ArgCheckTables.DecimalZero)
    Validate.notNegative(ArgCheckTables.DecimalPositive, Name) should
      haveValue(ArgCheckTables.DecimalPositive)
  }

  test("notNegative rejects a negative decimal, quoting it as the decimal renders") {
    rejects(
      Validate.notNegative(ArgCheckTables.DecimalNegative, Name),
      "Argument 'name' must not be negative but has value -1.2")
  }

  test("notNegative reports every decimal the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotNegativeDecimal) { (argument, expectedMessage) =>
      rejects(Validate.notNegative(argument, Name), expectedMessage)
    }
  }

  test("notNegative accepts every decimal the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotNegativeDecimal) { argument =>
      Validate.notNegative(argument, Name) should haveValue(argument)
    }
  }

  //-------------------------------------------------------------------------
  // notNaN.

  test("notNaN accepts an actual number, the two infinities included") {
    Validate.notNaN(0.0, Name) should haveValue(0.0)
    Validate.notNaN(Double.PositiveInfinity, Name) should haveValue(Double.PositiveInfinity)
    Validate.notNaN(Double.NegativeInfinity, Name) should haveValue(Double.NegativeInfinity)
  }

  test("notNaN rejects the one value that is not a number") {
    rejects(Validate.notNaN(Double.NaN, Name), "Argument 'name' must not be NaN")
  }

  test("notNaN reports every double the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotNaN) { (argument, expectedMessage) =>
      rejects(Validate.notNaN(argument, Name), expectedMessage)
    }
  }

  test("notNaN accepts every double the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotNaN) { argument =>
      Validate.notNaN(argument, Name) should haveValue(argument)
      bitsOf(Validate.notNaN(argument, Name)) shouldBe java.lang.Double.doubleToLongBits(argument)
    }
  }


  //-------------------------------------------------------------------------
  // notNegativeOrZero, over an int, a long, a double and a decimal.

  test("notNegativeOrZero accepts an int above zero, carrying that int through") {
    Validate.notNegativeOrZero(1, Name) should haveValue(1)
    Validate.notNegativeOrZero(Int.MaxValue, Name) should haveValue(Int.MaxValue)
  }

  test("notNegativeOrZero rejects zero and every int below it, quoting the value") {
    rejects(
      Validate.notNegativeOrZero(0, Name),
      "Argument 'name' must not be negative or zero but has value 0")
    rejects(
      Validate.notNegativeOrZero(-1, Name),
      "Argument 'name' must not be negative or zero but has value -1")
  }

  test("notNegativeOrZero reports every int the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotNegativeOrZeroInt) { (argument, expectedMessage) =>
      rejects(Validate.notNegativeOrZero(argument, Name), expectedMessage)
    }
  }

  test("notNegativeOrZero accepts every int the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotNegativeOrZeroInt) { argument =>
      Validate.notNegativeOrZero(argument, Name) should haveValue(argument)
    }
  }

  test("notNegativeOrZero accepts a long above zero, carrying that long through") {
    Validate.notNegativeOrZero(1L, Name) should haveValue(1L)
    Validate.notNegativeOrZero(Long.MaxValue, Name) should haveValue(Long.MaxValue)
  }

  test("notNegativeOrZero rejects zero and every long below it, quoting the value") {
    rejects(
      Validate.notNegativeOrZero(0L, Name),
      "Argument 'name' must not be negative or zero but has value 0")
    rejects(
      Validate.notNegativeOrZero(Long.MinValue, Name),
      "Argument 'name' must not be negative or zero but has value -9223372036854775808")
  }

  test("notNegativeOrZero reports every long the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotNegativeOrZeroLong) { (argument, expectedMessage) =>
      rejects(Validate.notNegativeOrZero(argument, Name), expectedMessage)
    }
  }

  test("notNegativeOrZero accepts every long the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotNegativeOrZeroLong) { argument =>
      Validate.notNegativeOrZero(argument, Name) should haveValue(argument)
    }
  }

  test("notNegativeOrZero rejects both signed zeros over doubles, rendering the sign it found") {
    rejects(
      Validate.notNegativeOrZero(0.0, Name),
      "Argument 'name' must not be negative or zero but has value 0.0")
    rejects(
      Validate.notNegativeOrZero(-0.0, Name),
      "Argument 'name' must not be negative or zero but has value -0.0")
  }

  test("notNegativeOrZero admits a NaN over doubles, which does not order against zero") {
    Validate.notNegativeOrZero(Double.NaN, Name) should beSuccess
  }

  test("notNegativeOrZero reports every double the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotNegativeOrZeroDouble) { (argument, expectedMessage) =>
      rejects(Validate.notNegativeOrZero(argument, Name), expectedMessage)
    }
  }

  test("notNegativeOrZero accepts every double the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotNegativeOrZeroDouble) { argument =>
      Validate.notNegativeOrZero(argument, Name) should beSuccess
      bitsOf(Validate.notNegativeOrZero(argument, Name)) shouldBe
        java.lang.Double.doubleToLongBits(argument)
    }
  }

  test("notNegativeOrZero accepts a decimal above zero and rejects zero itself") {
    Validate.notNegativeOrZero(ArgCheckTables.DecimalOne, Name) should
      haveValue(ArgCheckTables.DecimalOne)
    rejects(
      Validate.notNegativeOrZero(ArgCheckTables.DecimalZero, Name),
      "Argument 'name' must not be negative or zero but has value 0")
  }

  test("notNegativeOrZero reports every decimal the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotNegativeOrZeroDecimal) { (argument, expectedMessage) =>
      rejects(Validate.notNegativeOrZero(argument, Name), expectedMessage)
    }
  }

  test("notNegativeOrZero accepts every decimal the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotNegativeOrZeroDecimal) { argument =>
      Validate.notNegativeOrZero(argument, Name) should haveValue(argument)
    }
  }

  //-------------------------------------------------------------------------
  // notNegativeOrZero, with a tolerance, which is sequential rather than accumulating.

  test("the tolerant form of notNegativeOrZero accepts an argument clear of the interval") {
    Validate.notNegativeOrZero(1.0, 0.0001, Name) should haveValue(1.0)
    Validate.notNegativeOrZero(0.001, 0.0001, Name) should haveValue(0.001)
  }

  test("the tolerant form of notNegativeOrZero treats an argument inside the interval as zero") {
    rejects(
      Validate.notNegativeOrZero(0.0000001, 0.0001, Name),
      "Argument 'name' must not be zero")
    rejects(
      Validate.notNegativeOrZero(-0.00005, 0.0001, Name),
      "Argument 'name' must not be zero")
  }

  test("the tolerant form of notNegativeOrZero counts the boundary of the interval as zero") {
    rejects(Validate.notNegativeOrZero(0.0001, 0.0001, Name), "Argument 'name' must not be zero")
  }

  test("the tolerant form of notNegativeOrZero reports an argument clearly below zero as such") {
    rejects(
      Validate.notNegativeOrZero(-1.0, 0.0001, Name),
      "Argument 'name' must be greater than zero but has value -1.0")
  }

  test("the tolerant form of notNegativeOrZero reports an unusable tolerance rather than throwing") {
    rejects(
      Validate.notNegativeOrZero(1.0, -0.1, Name),
      "Argument 'tolerance' must not be negative but has value -0.1")
  }

  test("the tolerant form of notNegativeOrZero checks its tolerance first and stops there") {
    val outcome = Validate.notNegativeOrZero(0.0, -0.1, Name)
    messagesOf(outcome) shouldBe List("Argument 'tolerance' must not be negative but has value -0.1")
    failuresOf(outcome) should have size 1
  }

  test("the tolerant form of notNegativeOrZero reports a not-a-number tolerance rather than throwing") {
    rejects(Validate.notNegativeOrZero(1.0, Double.NaN, Name), "Argument 'tolerance' must not be NaN")
  }

  test("the tolerant form of notNegativeOrZero stops at a not-a-number tolerance, whatever the argument") {
    val nearZero = Validate.notNegativeOrZero(0.0, Double.NaN, Name)
    messagesOf(nearZero) shouldBe List("Argument 'tolerance' must not be NaN")
    failuresOf(nearZero) should have size 1
    val belowZero = Validate.notNegativeOrZero(-1.0, Double.NaN, Name)
    messagesOf(belowZero) shouldBe List("Argument 'tolerance' must not be NaN")
    failuresOf(belowZero) should have size 1
  }

  test("the tolerant form of notNegativeOrZero admits a negative zero tolerance, which is not negative") {
    Validate.notNegativeOrZero(1.0, -0.0, Name) should haveValue(1.0)
  }

  test("the tolerant form of notNegativeOrZero admits a zero tolerance, which describes no interval") {
    Validate.notNegativeOrZero(1.0, 0.0, Name) should haveValue(1.0)
    rejects(Validate.notNegativeOrZero(0.0, 0.0, Name), "Argument 'name' must not be zero")
  }

  test("the tolerant form of notNegativeOrZero reports every row the shared fixture rejects") {
    forAll(ArgCheckTables.invalidNotNegativeOrZeroWithTolerance) {
      (argument, tolerance, expectedMessage) =>
        rejects(Validate.notNegativeOrZero(argument, tolerance, Name), expectedMessage)
    }
  }

  test("the tolerant form of notNegativeOrZero accepts every row the shared fixture admits") {
    forAll(ArgCheckTables.validNotNegativeOrZeroWithTolerance) { (argument, tolerance) =>
      Validate.notNegativeOrZero(argument, tolerance, Name) should beSuccess
      bitsOf(Validate.notNegativeOrZero(argument, tolerance, Name)) shouldBe
        java.lang.Double.doubleToLongBits(argument)
    }
  }

  //-------------------------------------------------------------------------
  // notZero, plain and with a tolerance.

  test("notZero accepts a double that differs from zero, however slightly") {
    Validate.notZero(1.0E-300, Name) should haveValue(1.0E-300)
    Validate.notZero(-1.0, Name) should haveValue(-1.0)
  }

  test("notZero rejects both signed zeros with one wording, the sign being of no interest") {
    rejects(Validate.notZero(0.0, Name), "Argument 'name' must not be zero")
    rejects(Validate.notZero(-0.0, Name), "Argument 'name' must not be zero")
  }

  test("notZero admits a NaN and the two infinities, none of which is zero") {
    Validate.notZero(Double.NaN, Name) should beSuccess
    Validate.notZero(Double.PositiveInfinity, Name) should haveValue(Double.PositiveInfinity)
    Validate.notZero(Double.NegativeInfinity, Name) should haveValue(Double.NegativeInfinity)
  }

  test("notZero reports every double the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidNotZero) { (argument, expectedMessage) =>
      rejects(Validate.notZero(argument, Name), expectedMessage)
    }
  }

  test("notZero accepts every double the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validNotZero) { argument =>
      Validate.notZero(argument, Name) should beSuccess
      bitsOf(Validate.notZero(argument, Name)) shouldBe java.lang.Double.doubleToLongBits(argument)
    }
  }

  test("the tolerant form of notZero has no interest in the sign of the argument") {
    rejects(Validate.notZero(0.05, 0.1, Name), "Argument 'name' must not be zero")
    rejects(Validate.notZero(-0.05, 0.1, Name), "Argument 'name' must not be zero")
  }

  test("the tolerant form of notZero counts the boundary of the interval as zero") {
    rejects(Validate.notZero(0.1, 0.1, Name), "Argument 'name' must not be zero")
  }

  test("the tolerant form of notZero accepts an argument clear of the interval on either side") {
    Validate.notZero(0.2, 0.1, Name) should haveValue(0.2)
    Validate.notZero(-0.2, 0.1, Name) should haveValue(-0.2)
  }

  test("the tolerant form of notZero reports an unusable tolerance rather than throwing") {
    rejects(
      Validate.notZero(1.0, -0.1, Name),
      "Argument 'tolerance' must not be negative but has value -0.1")
  }

  test("the tolerant form of notZero checks its tolerance first and stops there") {
    val outcome = Validate.notZero(0.0, -0.1, Name)
    messagesOf(outcome) shouldBe List("Argument 'tolerance' must not be negative but has value -0.1")
    failuresOf(outcome) should have size 1
  }

  test("the tolerant form of notZero reports a not-a-number tolerance rather than throwing") {
    rejects(Validate.notZero(1.0, Double.NaN, Name), "Argument 'tolerance' must not be NaN")
  }

  test("the tolerant form of notZero stops at a not-a-number tolerance, whatever the argument") {
    val outcome = Validate.notZero(0.0, Double.NaN, Name)
    messagesOf(outcome) shouldBe List("Argument 'tolerance' must not be NaN")
    failuresOf(outcome) should have size 1
  }

  test("the tolerant form of notZero admits a negative zero tolerance, which is not negative") {
    Validate.notZero(1.0, -0.0, Name) should haveValue(1.0)
  }

  test("the tolerant form of notZero reports every row the shared fixture rejects") {
    forAll(ArgCheckTables.invalidNotZeroWithTolerance) { (argument, tolerance, expectedMessage) =>
      rejects(Validate.notZero(argument, tolerance, Name), expectedMessage)
    }
  }

  test("the tolerant form of notZero accepts every row the shared fixture admits") {
    forAll(ArgCheckTables.validNotZeroWithTolerance) { (argument, tolerance) =>
      Validate.notZero(argument, tolerance, Name) should beSuccess
      bitsOf(Validate.notZero(argument, tolerance, Name)) shouldBe
        java.lang.Double.doubleToLongBits(argument)
    }
  }

  //-------------------------------------------------------------------------
  // The three interval shapes over doubles.

  test("inRange over doubles admits its low bound and refuses its high bound") {
    Validate.inRange(0.0, 0.0, 1.0, Name) should haveValue(0.0)
    rejects(Validate.inRange(1.0, 0.0, 1.0, Name), "Expected 0.0 <= 'name' < 1.0, but found 1.0")
  }

  test("inRange over doubles names the interval it rejected an argument against") {
    messageOf(Validate.inRange(-1.0, 0.0, 1.0, Name)) shouldBe
      "Expected 0.0 <= 'name' < 1.0, but found -1.0"
  }

  test("inRange over doubles reports every row the shared fixture rejects") {
    forAll(ArgCheckTables.invalidInRangeDouble) {
      (argument, lowInclusive, highExclusive, expectedMessage) =>
        rejects(Validate.inRange(argument, lowInclusive, highExclusive, Name), expectedMessage)
    }
  }

  test("inRange over doubles accepts every row the shared fixture admits") {
    forAll(ArgCheckTables.validInRangeDouble) { (argument, lowInclusive, highExclusive) =>
      Validate.inRange(argument, lowInclusive, highExclusive, Name) should haveValue(argument)
    }
  }

  test("inRangeInclusive over doubles admits both of its bounds") {
    Validate.inRangeInclusive(0.0, 0.0, 1.0, Name) should haveValue(0.0)
    Validate.inRangeInclusive(1.0, 0.0, 1.0, Name) should haveValue(1.0)
  }

  test("inRangeInclusive over doubles reports every row the shared fixture rejects") {
    forAll(ArgCheckTables.invalidInRangeInclusiveDouble) {
      (argument, lowInclusive, highInclusive, expectedMessage) =>
        rejects(
          Validate.inRangeInclusive(argument, lowInclusive, highInclusive, Name),
          expectedMessage)
    }
  }

  test("inRangeInclusive over doubles accepts every row the shared fixture admits") {
    forAll(ArgCheckTables.validInRangeInclusiveDouble) { (argument, lowInclusive, highInclusive) =>
      Validate.inRangeInclusive(argument, lowInclusive, highInclusive, Name) should
        haveValue(argument)
    }
  }

  test("inRangeExclusive over doubles refuses both of its bounds") {
    rejects(
      Validate.inRangeExclusive(0.0, 0.0, 1.0, Name),
      "Expected 0.0 < 'name' < 1.0, but found 0.0")
    rejects(
      Validate.inRangeExclusive(1.0, 0.0, 1.0, Name),
      "Expected 0.0 < 'name' < 1.0, but found 1.0")
  }

  test("inRangeExclusive over doubles reports every row the shared fixture rejects") {
    forAll(ArgCheckTables.invalidInRangeExclusiveDouble) {
      (argument, lowExclusive, highExclusive, expectedMessage) =>
        rejects(
          Validate.inRangeExclusive(argument, lowExclusive, highExclusive, Name),
          expectedMessage)
    }
  }

  test("inRangeExclusive over doubles accepts every row the shared fixture admits") {
    forAll(ArgCheckTables.validInRangeExclusiveDouble) { (argument, lowExclusive, highExclusive) =>
      Validate.inRangeExclusive(argument, lowExclusive, highExclusive, Name) should
        haveValue(argument)
    }
  }

  //-------------------------------------------------------------------------
  // The three interval shapes over ints.

  test("inRange over ints is the shape an index is checked with") {
    Validate.inRange(0, 0, 2, Name) should haveValue(0)
    Validate.inRange(1, 0, 2, Name) should haveValue(1)
    rejects(Validate.inRange(2, 0, 2, Name), "Expected 0 <= 'name' < 2, but found 2")
  }

  test("inRange over ints reports every row the shared fixture rejects") {
    forAll(ArgCheckTables.invalidInRangeInt) {
      (argument, lowInclusive, highExclusive, expectedMessage) =>
        rejects(Validate.inRange(argument, lowInclusive, highExclusive, Name), expectedMessage)
    }
  }

  test("inRange over ints accepts every row the shared fixture admits") {
    forAll(ArgCheckTables.validInRangeInt) { (argument, lowInclusive, highExclusive) =>
      Validate.inRange(argument, lowInclusive, highExclusive, Name) should haveValue(argument)
    }
  }

  test("inRangeInclusive over ints admits both of its bounds") {
    Validate.inRangeInclusive(0, 0, 2, Name) should haveValue(0)
    Validate.inRangeInclusive(2, 0, 2, Name) should haveValue(2)
    rejects(Validate.inRangeInclusive(3, 0, 2, Name), "Expected 0 <= 'name' <= 2, but found 3")
  }

  test("inRangeInclusive over ints reports every row the shared fixture rejects") {
    forAll(ArgCheckTables.invalidInRangeInclusiveInt) {
      (argument, lowInclusive, highInclusive, expectedMessage) =>
        rejects(
          Validate.inRangeInclusive(argument, lowInclusive, highInclusive, Name),
          expectedMessage)
    }
  }

  test("inRangeInclusive over ints accepts every row the shared fixture admits") {
    forAll(ArgCheckTables.validInRangeInclusiveInt) { (argument, lowInclusive, highInclusive) =>
      Validate.inRangeInclusive(argument, lowInclusive, highInclusive, Name) should
        haveValue(argument)
    }
  }

  test("inRangeExclusive over ints refuses both of its bounds") {
    rejects(Validate.inRangeExclusive(0, 0, 2, Name), "Expected 0 < 'name' < 2, but found 0")
    rejects(Validate.inRangeExclusive(2, 0, 2, Name), "Expected 0 < 'name' < 2, but found 2")
    Validate.inRangeExclusive(1, 0, 2, Name) should haveValue(1)
  }

  test("inRangeExclusive over ints reports every row the shared fixture rejects") {
    forAll(ArgCheckTables.invalidInRangeExclusiveInt) {
      (argument, lowExclusive, highExclusive, expectedMessage) =>
        rejects(
          Validate.inRangeExclusive(argument, lowExclusive, highExclusive, Name),
          expectedMessage)
    }
  }

  test("inRangeExclusive over ints accepts every row the shared fixture admits") {
    forAll(ArgCheckTables.validInRangeExclusiveInt) { (argument, lowExclusive, highExclusive) =>
      Validate.inRangeExclusive(argument, lowExclusive, highExclusive, Name) should
        haveValue(argument)
    }
  }

  //-------------------------------------------------------------------------
  // The three interval shapes over a type with an ordering of its own.

  test("inRangeComparable orders by the ordering it was given rather than by a comparison interface") {
    Validate.inRangeComparable(
      Duration.ofSeconds(1),
      Duration.ZERO,
      Duration.ofSeconds(2),
      Name) should haveValue(Duration.ofSeconds(1))
    rejects(
      Validate.inRangeComparable(
        Duration.ofSeconds(2),
        Duration.ZERO,
        Duration.ofSeconds(2),
        Name),
      "Expected PT0S <= 'name' < PT2S, but found PT2S")
  }

  test("inRangeComparable reports every row the shared fixture rejects") {
    forAll(ArgCheckTables.invalidInRangeComparable) {
      (argument, lowInclusive, highExclusive, expectedMessage) =>
        rejects(
          Validate.inRangeComparable(argument, lowInclusive, highExclusive, Name),
          expectedMessage)
    }
  }

  test("inRangeComparable accepts every row the shared fixture admits") {
    forAll(ArgCheckTables.validInRangeComparable) { (argument, lowInclusive, highExclusive) =>
      Validate.inRangeComparable(argument, lowInclusive, highExclusive, Name) should
        haveValue(argument)
    }
  }

  test("inRangeComparableInclusive admits both of its bounds") {
    Validate.inRangeComparableInclusive(
      Duration.ofSeconds(2),
      Duration.ZERO,
      Duration.ofSeconds(2),
      Name) should haveValue(Duration.ofSeconds(2))
    rejects(
      Validate.inRangeComparableInclusive(
        Duration.ofSeconds(3),
        Duration.ZERO,
        Duration.ofSeconds(2),
        Name),
      "Expected PT0S <= 'name' <= PT2S, but found PT3S")
  }

  test("inRangeComparableInclusive reports every row the shared fixture rejects") {
    forAll(ArgCheckTables.invalidInRangeComparableInclusive) {
      (argument, lowInclusive, highInclusive, expectedMessage) =>
        rejects(
          Validate.inRangeComparableInclusive(argument, lowInclusive, highInclusive, Name),
          expectedMessage)
    }
  }

  test("inRangeComparableInclusive accepts every row the shared fixture admits") {
    forAll(ArgCheckTables.validInRangeComparableInclusive) {
      (argument, lowInclusive, highInclusive) =>
        Validate.inRangeComparableInclusive(argument, lowInclusive, highInclusive, Name) should
          haveValue(argument)
    }
  }

  test("inRangeComparableExclusive refuses both of its bounds") {
    rejects(
      Validate.inRangeComparableExclusive(
        Duration.ZERO,
        Duration.ZERO,
        Duration.ofSeconds(2),
        Name),
      "Expected PT0S < 'name' < PT2S, but found PT0S")
    Validate.inRangeComparableExclusive(
      Duration.ofMillis(1),
      Duration.ZERO,
      Duration.ofSeconds(2),
      Name) should haveValue(Duration.ofMillis(1))
  }

  test("inRangeComparableExclusive reports every row the shared fixture rejects") {
    forAll(ArgCheckTables.invalidInRangeComparableExclusive) {
      (argument, lowExclusive, highExclusive, expectedMessage) =>
        rejects(
          Validate.inRangeComparableExclusive(argument, lowExclusive, highExclusive, Name),
          expectedMessage)
    }
  }

  test("inRangeComparableExclusive accepts every row the shared fixture admits") {
    forAll(ArgCheckTables.validInRangeComparableExclusive) {
      (argument, lowExclusive, highExclusive) =>
        Validate.inRangeComparableExclusive(argument, lowExclusive, highExclusive, Name) should
          haveValue(argument)
    }
  }

  //-------------------------------------------------------------------------
  // The two order checks, which carry the pair they checked.

  test("inOrderNotEqual accepts a strictly increasing pair and carries both values back") {
    val first = LocalDate.of(2011, 7, 2)
    val second = LocalDate.of(2011, 7, 3)
    Validate.inOrderNotEqual(first, second, FirstName, SecondName) should haveValue((first, second))
  }

  test("inOrderNotEqual rejects a pair out of order, quoting both values and both names") {
    rejects(
      Validate.inOrderNotEqual(
        LocalDate.of(2011, 7, 3),
        LocalDate.of(2011, 7, 2),
        FirstName,
        SecondName),
      "Invalid order: Expected 'a' < 'b', but found: '2011-07-03' >= '2011-07-02'")
  }

  test("inOrderNotEqual rejects an equal pair, which is what separates it from the other check") {
    val date = LocalDate.of(2011, 7, 3)
    rejects(
      Validate.inOrderNotEqual(date, date, FirstName, SecondName),
      "Invalid order: Expected 'a' < 'b', but found: '2011-07-03' >= '2011-07-03'")
    Validate.inOrderOrEqual(date, date, FirstName, SecondName) should haveValue((date, date))
  }

  test("inOrderNotEqual reports every pair the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidInOrderNotEqual) { (first, second, expectedMessage) =>
      rejects(Validate.inOrderNotEqual(first, second, FirstName, SecondName), expectedMessage)
    }
  }

  test("inOrderNotEqual accepts every pair the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validInOrderNotEqual) { (first, second) =>
      Validate.inOrderNotEqual(first, second, FirstName, SecondName) should
        haveValue((first, second))
    }
  }

  test("inOrderOrEqual rejects a pair out of order, quoting both values and both names") {
    rejects(
      Validate.inOrderOrEqual(
        LocalDate.of(2012, 1, 1),
        LocalDate.of(2011, 12, 31),
        FirstName,
        SecondName),
      "Invalid order: Expected 'a' <= 'b', but found: '2012-01-01' > '2011-12-31'")
  }

  test("inOrderOrEqual reports every pair the shared fixture expects it to reject") {
    forAll(ArgCheckTables.invalidInOrderOrEqual) { (first, second, expectedMessage) =>
      rejects(Validate.inOrderOrEqual(first, second, FirstName, SecondName), expectedMessage)
    }
  }

  test("inOrderOrEqual accepts every pair the shared fixture expects it to admit") {
    forAll(ArgCheckTables.validInOrderOrEqual) { (first, second) =>
      Validate.inOrderOrEqual(first, second, FirstName, SecondName) should
        haveValue((first, second))
    }
  }


  //-------------------------------------------------------------------------
  // Wording parity with the fail-fast half.
  //
  // Each case below drives both objects from one argument and compares the two strings, so
  // the wording of a check cannot drift on one object without failing here. The expected text
  // of the shared fixture is not read by these cases at all: the point is not what the message
  // says but that both halves say the same thing, and the sections above have already asserted
  // the text itself against the fixture.

  test("the two halves word the rejection of both forms of matches identically") {
    forAll(ArgCheckTables.invalidMatchesRegex) { (pattern, argument, _) =>
      messageOf(Validate.matches(pattern, argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.matches(pattern, argument, Name))
    }
    forAll(ArgCheckTables.invalidMatchesPredicate) {
      (matcher, minLength, maxLength, argument, equivalentRegex, _) =>
        messageOf(Validate.matches(matcher, minLength, maxLength, argument, Name, equivalentRegex)) shouldBe
          thrownMessageOf(ArgCheck.matches(matcher, minLength, maxLength, argument, Name, equivalentRegex))
    }
  }

  test("the two halves word the rejection of notBlank and of empty text identically") {
    forAll(ArgCheckTables.invalidNotBlank) { (argument, _) =>
      messageOf(Validate.notBlank(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notBlank(argument, Name))
    }
    forAll(ArgCheckTables.invalidNotEmptyString) { (argument, _) =>
      messageOf(Validate.notEmpty(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notEmpty(argument, Name))
    }
  }

  test("the two halves word the rejection of an empty array identically, whatever its element type") {
    forAll(ArgCheckTables.invalidNotEmptyObjectArray) { (argument, _) =>
      messageOf(Validate.notEmpty(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notEmpty(argument, Name))
    }
    forAll(ArgCheckTables.invalidNotEmptyNestedArray) { (argument, _) =>
      messageOf(Validate.notEmpty(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notEmpty(argument, Name))
    }
    forAll(ArgCheckTables.invalidNotEmptyIntArray) { (argument, _) =>
      messageOf(Validate.notEmpty(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notEmpty(argument, Name))
    }
    forAll(ArgCheckTables.invalidNotEmptyLongArray) { (argument, _) =>
      messageOf(Validate.notEmpty(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notEmpty(argument, Name))
    }
    forAll(ArgCheckTables.invalidNotEmptyDoubleArray) { (argument, _) =>
      messageOf(Validate.notEmpty(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notEmpty(argument, Name))
    }
  }

  test("the two halves word the rejection of an empty iterable and of an empty map identically") {
    forAll(ArgCheckTables.invalidNotEmptyIterable) { (argument, _) =>
      messageOf(Validate.notEmpty(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notEmpty(argument, Name))
    }
    forAll(ArgCheckTables.invalidNotEmptyMap) { (argument, _) =>
      messageOf(Validate.notEmpty(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notEmpty(argument, Name))
    }
  }

  test("the two halves word the rejection of the two duplicate checks identically") {
    forAll(ArgCheckTables.invalidNoDuplicates) { (argument, _) =>
      messageOf(Validate.noDuplicates(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.noDuplicates(argument, Name))
    }
    forAll(ArgCheckTables.invalidNoDuplicatesSorted) { (argument, _) =>
      messageOf(Validate.noDuplicatesSorted(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.noDuplicatesSorted(argument, Name))
    }
  }

  test("the two halves word the rejection of a positive value identically over every type") {
    forAll(ArgCheckTables.invalidNotPositiveInt) { (argument, _) =>
      messageOf(Validate.notPositive(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notPositive(argument, Name))
    }
    forAll(ArgCheckTables.invalidNotPositiveLong) { (argument, _) =>
      messageOf(Validate.notPositive(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notPositive(argument, Name))
    }
    forAll(ArgCheckTables.invalidNotPositiveDouble) { (argument, _) =>
      messageOf(Validate.notPositive(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notPositive(argument, Name))
    }
    forAll(ArgCheckTables.invalidNotPositiveDecimal) { (argument, _) =>
      messageOf(Validate.notPositive(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notPositive(argument, Name))
    }
    forAll(ArgCheckTables.invalidNotPositiveIfPresent) { (argument, _) =>
      messageOf(Validate.notPositiveIfPresent(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notPositiveIfPresent(argument, Name))
    }
  }

  test("the two halves word the rejection of a negative value identically over every type") {
    forAll(ArgCheckTables.invalidNotNegativeInt) { (argument, _) =>
      messageOf(Validate.notNegative(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notNegative(argument, Name))
    }
    forAll(ArgCheckTables.invalidNotNegativeLong) { (argument, _) =>
      messageOf(Validate.notNegative(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notNegative(argument, Name))
    }
    forAll(ArgCheckTables.invalidNotNegativeDouble) { (argument, _) =>
      messageOf(Validate.notNegative(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notNegative(argument, Name))
    }
    forAll(ArgCheckTables.invalidNotNegativeDecimal) { (argument, _) =>
      messageOf(Validate.notNegative(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notNegative(argument, Name))
    }
  }

  test("the two halves word the rejection of a value that is not a number identically") {
    forAll(ArgCheckTables.invalidNotNaN) { (argument, _) =>
      messageOf(Validate.notNaN(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notNaN(argument, Name))
    }
  }

  test("the two halves word the rejection of a value at or below zero identically over every type") {
    forAll(ArgCheckTables.invalidNotNegativeOrZeroInt) { (argument, _) =>
      messageOf(Validate.notNegativeOrZero(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notNegativeOrZero(argument, Name))
    }
    forAll(ArgCheckTables.invalidNotNegativeOrZeroLong) { (argument, _) =>
      messageOf(Validate.notNegativeOrZero(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notNegativeOrZero(argument, Name))
    }
    forAll(ArgCheckTables.invalidNotNegativeOrZeroDouble) { (argument, _) =>
      messageOf(Validate.notNegativeOrZero(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notNegativeOrZero(argument, Name))
    }
    forAll(ArgCheckTables.invalidNotNegativeOrZeroDecimal) { (argument, _) =>
      messageOf(Validate.notNegativeOrZero(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notNegativeOrZero(argument, Name))
    }
  }

  test("the two halves word the rejection of the two tolerance-bearing checks identically") {
    forAll(ArgCheckTables.invalidNotNegativeOrZeroWithTolerance) { (argument, tolerance, _) =>
      messageOf(Validate.notNegativeOrZero(argument, tolerance, Name)) shouldBe
        thrownMessageOf(ArgCheck.notNegativeOrZero(argument, tolerance, Name))
    }
    forAll(ArgCheckTables.invalidNotZeroWithTolerance) { (argument, tolerance, _) =>
      messageOf(Validate.notZero(argument, tolerance, Name)) shouldBe
        thrownMessageOf(ArgCheck.notZero(argument, tolerance, Name))
    }
  }

  test("the two halves word the rejection of a zero argument identically") {
    forAll(ArgCheckTables.invalidNotZero) { (argument, _) =>
      messageOf(Validate.notZero(argument, Name)) shouldBe
        thrownMessageOf(ArgCheck.notZero(argument, Name))
    }
  }

  test("the two halves word the rejection of the three interval shapes over doubles identically") {
    forAll(ArgCheckTables.invalidInRangeDouble) { (argument, low, high, _) =>
      messageOf(Validate.inRange(argument, low, high, Name)) shouldBe
        thrownMessageOf(ArgCheck.inRange(argument, low, high, Name))
    }
    forAll(ArgCheckTables.invalidInRangeInclusiveDouble) { (argument, low, high, _) =>
      messageOf(Validate.inRangeInclusive(argument, low, high, Name)) shouldBe
        thrownMessageOf(ArgCheck.inRangeInclusive(argument, low, high, Name))
    }
    forAll(ArgCheckTables.invalidInRangeExclusiveDouble) { (argument, low, high, _) =>
      messageOf(Validate.inRangeExclusive(argument, low, high, Name)) shouldBe
        thrownMessageOf(ArgCheck.inRangeExclusive(argument, low, high, Name))
    }
  }

  test("the two halves word the rejection of the three interval shapes over ints identically") {
    forAll(ArgCheckTables.invalidInRangeInt) { (argument, low, high, _) =>
      messageOf(Validate.inRange(argument, low, high, Name)) shouldBe
        thrownMessageOf(ArgCheck.inRange(argument, low, high, Name))
    }
    forAll(ArgCheckTables.invalidInRangeInclusiveInt) { (argument, low, high, _) =>
      messageOf(Validate.inRangeInclusive(argument, low, high, Name)) shouldBe
        thrownMessageOf(ArgCheck.inRangeInclusive(argument, low, high, Name))
    }
    forAll(ArgCheckTables.invalidInRangeExclusiveInt) { (argument, low, high, _) =>
      messageOf(Validate.inRangeExclusive(argument, low, high, Name)) shouldBe
        thrownMessageOf(ArgCheck.inRangeExclusive(argument, low, high, Name))
    }
  }

  test("the two halves word the rejection of the three interval shapes over an ordered type identically") {
    forAll(ArgCheckTables.invalidInRangeComparable) { (argument, low, high, _) =>
      messageOf(Validate.inRangeComparable(argument, low, high, Name)) shouldBe
        thrownMessageOf(ArgCheck.inRangeComparable(argument, low, high, Name))
    }
    forAll(ArgCheckTables.invalidInRangeComparableInclusive) { (argument, low, high, _) =>
      messageOf(Validate.inRangeComparableInclusive(argument, low, high, Name)) shouldBe
        thrownMessageOf(ArgCheck.inRangeComparableInclusive(argument, low, high, Name))
    }
    forAll(ArgCheckTables.invalidInRangeComparableExclusive) { (argument, low, high, _) =>
      messageOf(Validate.inRangeComparableExclusive(argument, low, high, Name)) shouldBe
        thrownMessageOf(ArgCheck.inRangeComparableExclusive(argument, low, high, Name))
    }
  }

  test("the two halves word the rejection of the two order checks identically") {
    forAll(ArgCheckTables.invalidInOrderNotEqual) { (first, second, _) =>
      messageOf(Validate.inOrderNotEqual(first, second, FirstName, SecondName)) shouldBe
        thrownMessageOf(ArgCheck.inOrderNotEqual(first, second, FirstName, SecondName))
    }
    forAll(ArgCheckTables.invalidInOrderOrEqual) { (first, second, _) =>
      messageOf(Validate.inOrderOrEqual(first, second, FirstName, SecondName)) shouldBe
        thrownMessageOf(ArgCheck.inOrderOrEqual(first, second, FirstName, SecondName))
    }
  }

  test("the two halves word the two boolean checks identically, standard wording included") {
    messageOf(Validate.isTrue(false)) shouldBe thrownMessageOf(ArgCheck.isTrue(false))
    messageOf(Validate.isTrue(false, "Message")) shouldBe
      thrownMessageOf(ArgCheck.isTrue(false, "Message"))
    messageOf(Validate.isFalse(true, "Message")) shouldBe
      thrownMessageOf(ArgCheck.isFalse(true, "Message"))
  }

  test("the two halves word an interpolated message identically, whatever it interpolates") {
    val text = "A"
    val count = 2
    val amount = 3.0
    val days = 3L
    messageOf(Validate.isTrue(false, s"Message $text $count $amount")) shouldBe
      thrownMessageOf(ArgCheck.isTrue(false, s"Message $text $count $amount"))
    messageOf(Validate.isTrue(false, s"Message $days")) shouldBe
      thrownMessageOf(ArgCheck.isTrue(false, s"Message $days"))
    messageOf(Validate.isTrue(false, s"Message $amount")) shouldBe
      thrownMessageOf(ArgCheck.isTrue(false, s"Message $amount"))
    messageOf(Validate.isFalse(true, s"Message $text $count $amount")) shouldBe
      thrownMessageOf(ArgCheck.isFalse(true, s"Message $text $count $amount"))
  }

  test("the two halves admit the same arguments, so neither is the stricter of the pair") {
    forAll(ArgCheckTables.validNotBlank) { argument =>
      noException should be thrownBy ArgCheck.notBlank(argument, Name)
      Validate.notBlank(argument, Name) should beSuccess
    }
    forAll(ArgCheckTables.validNotPositiveDouble) { argument =>
      noException should be thrownBy ArgCheck.notPositive(argument, Name)
      Validate.notPositive(argument, Name) should beSuccess
    }
    forAll(ArgCheckTables.validNoDuplicatesSorted) { argument =>
      noException should be thrownBy ArgCheck.noDuplicatesSorted(argument, Name)
      Validate.noDuplicatesSorted(argument, Name) should beSuccess
    }
    forAll(ArgCheckTables.validInRangeComparableExclusive) { (argument, low, high) =>
      noException should be thrownBy ArgCheck.inRangeComparableExclusive(argument, low, high, Name)
      Validate.inRangeComparableExclusive(argument, low, high, Name) should beSuccess
    }
  }


  //-------------------------------------------------------------------------
  // Accumulation, which is the behaviour the fail-fast half cannot express.

  test("two independently failing checks report both failures, in the order the expression names them") {
    val outcome = (Validate.notBlank("", Name), Validate.notPositive(1, Name))
      .mapN((text, number) => (text, number))
    outcome should beFailure
    messagesOf(outcome) shouldBe List(
      "Argument 'name' must not be blank",
      "Argument 'name' must not be positive but has value 1")
  }

  test("three independently failing checks report all three, in order") {
    val outcome =
      (Validate.notBlank("", Name), Validate.notPositive(1, Name), Validate.notNaN(Double.NaN, Name))
        .mapN((text, number, amount) => (text, number, amount))
    messagesOf(outcome) shouldBe List(
      "Argument 'name' must not be blank",
      "Argument 'name' must not be positive but has value 1",
      "Argument 'name' must not be NaN")
  }

  test("four independently failing checks report all four, in order") {
    val outcome = (
      Validate.notBlank("", Name),
      Validate.notPositive(1, Name),
      Validate.notNaN(Double.NaN, Name),
      Validate.notZero(0.0, Name)).mapN((text, number, amount, rate) => (text, number, amount, rate))
    failuresOf(outcome) should have size 4
    messagesOf(outcome) shouldBe List(
      "Argument 'name' must not be blank",
      "Argument 'name' must not be positive but has value 1",
      "Argument 'name' must not be NaN",
      "Argument 'name' must not be zero")
  }

  test("a mixed expression reports only the checks that failed") {
    val outcome = (
      Validate.notBlank("OG", Name),
      Validate.notPositive(1, Name),
      Validate.notNaN(1.0, Name),
      Validate.notZero(0.0, Name)).mapN((text, number, amount, rate) => (text, number, amount, rate))
    messagesOf(outcome) shouldBe List(
      "Argument 'name' must not be positive but has value 1",
      "Argument 'name' must not be zero")
  }

  test("an expression whose checks all pass carries the tupled payload of every one of them") {
    val outcome =
      (Validate.notBlank("OG", Name), Validate.notPositive(-1, Name), Validate.notNaN(1.5, Name))
        .mapN((text, number, amount) => (text, number, amount))
    outcome should beSuccess
    outcome should haveValue(("OG", -1, 1.5))
  }

  test("the payload of a combined expression is what a validating factory hands its constructor") {
    final case class Rate(label: String, tenor: Int)
    val outcome = (Validate.notBlank("3M", "label"), Validate.notNegativeOrZero(3, "tenor"))
      .mapN((label, tenor) => Rate(label, tenor))
    outcome should haveValue(Rate("3M", 3))
  }

  test("the accumulated failures all carry the reason this module assigns to invalid input") {
    val outcome = (Validate.notBlank("", Name), Validate.notPositive(1, Name))
      .mapN((text, number) => (text, number))
    outcome should beFailureWith(FailureReason.INVALID)
    failuresOf(outcome).map(failure => failure.reason) shouldBe
      List(FailureReason.INVALID, FailureReason.INVALID)
    failuresOf(outcome).forall(failure => failure.attributes.isEmpty) shouldBe true
  }

  test("an accumulated chain keeps a repeated message rather than collapsing it") {
    val outcome = (Validate.notPositive(1, Name), Validate.notPositive(1, Name))
      .mapN((first, second) => (first, second))
    failuresOf(outcome) should have size 2
    messagesOf(outcome) shouldBe List(
      "Argument 'name' must not be positive but has value 1",
      "Argument 'name' must not be positive but has value 1")
  }

  test("accumulation reaches across the kinds of check, a lifted result included") {
    val outcome = (
      Validate.fromResult(Decimal.parse("bad")),
      Validate.notBlank("", Name),
      Validate.inRange(2, 0, 2, Name)).mapN((amount, text, index) => (amount, text, index))
    messagesOf(outcome) shouldBe List(
      "Decimal string is invalid: 'bad'",
      "Argument 'name' must not be blank",
      "Expected 0 <= 'name' < 2, but found 2")
    failuresOf(outcome).map(failure => failure.reason) shouldBe
      List(FailureReason.PARSING, FailureReason.INVALID, FailureReason.INVALID)
  }

  test("more than four checks accumulate too, when a factory sequences a collection of them") {
    val checks: List[ValidatedFailures[Int]] = List(
      Validate.notPositive(1, Name),
      Validate.notPositive(2, Name),
      Validate.notPositive(-1, Name),
      Validate.notNegative(-1, Name),
      Validate.notNegativeOrZero(0, Name))
    val outcome = checks.sequence
    failuresOf(outcome) should have size 4
    messagesOf(outcome) shouldBe List(
      "Argument 'name' must not be positive but has value 1",
      "Argument 'name' must not be positive but has value 2",
      "Argument 'name' must not be negative but has value -1",
      "Argument 'name' must not be negative or zero but has value 0")
  }

  test("sequencing a collection of passing checks carries every value it checked, in order") {
    val checks: List[ValidatedFailures[Int]] = List(
      Validate.notPositive(0, Name),
      Validate.notPositive(-1, Name),
      Validate.notPositive(-2, Name))
    checks.sequence should haveValue(List(0, -1, -2))
  }

  test("combining with andThen stops at the first failure, which is why mapN combines the checks") {
    val sequenced = Validate
      .notBlank("", Name)
      .andThen(text => Validate.notPositive(1, Name).map(number => (text, number)))
    messagesOf(sequenced) shouldBe List("Argument 'name' must not be blank")
    val accumulated = (Validate.notBlank("", Name), Validate.notPositive(1, Name))
      .mapN((text, number) => (text, number))
    messagesOf(accumulated) shouldBe List(
      "Argument 'name' must not be blank",
      "Argument 'name' must not be positive but has value 1")
    failuresOf(accumulated).size should be > failuresOf(sequenced).size
  }

  test("the whole accumulated chain reaches the caller through toResult") {
    val outcome =
      (Validate.notBlank("", Name), Validate.notPositive(1, Name), Validate.notNaN(Double.NaN, Name))
        .mapN((text, number, amount) => (text, number, amount))
    Validate.toResult(outcome) shouldBe Left(
      NonEmptyChain.of(
        Failure.Invalid("Argument 'name' must not be blank"),
        Failure.Invalid("Argument 'name' must not be positive but has value 1"),
        Failure.Invalid("Argument 'name' must not be NaN")))
  }

  //-------------------------------------------------------------------------
  // Nothing here throws, which is the whole difference in how an outcome is handled.

  test("no check throws for an argument it rejects") {
    noException should be thrownBy {
      val rejected: List[ValidatedFailures[Any]] = List(
        Validate.isTrue(false),
        Validate.isFalse(true, "Message"),
        Validate.matches("[A-Z]+".r, "og", Name),
        Validate.notBlank(" ", Name),
        Validate.notEmpty("", Name),
        Validate.notEmpty(Array.empty[Double], Name),
        Validate.notEmpty(List.empty[String], Name),
        Validate.notEmpty(Map.empty[String, String], Name),
        Validate.noDuplicates(Array(1.0, 1.0), Name),
        Validate.noDuplicatesSorted(Array(1.0, 0.0), Name),
        Validate.notPositive(1, Name),
        Validate.notNegative(-1, Name),
        Validate.notNaN(Double.NaN, Name),
        Validate.notNegativeOrZero(0, Name),
        Validate.notZero(0.0, Name),
        Validate.inRange(2.0, 0.0, 1.0, Name),
        Validate.inRangeComparable(Duration.ofSeconds(3), Duration.ZERO, Duration.ofSeconds(2), Name),
        Validate.inOrderNotEqual(
          LocalDate.of(2011, 7, 3),
          LocalDate.of(2011, 7, 2),
          FirstName,
          SecondName))
      rejected
    }
  }

  test("every one of those rejections is reported as a value instead") {
    val rejected: List[ValidatedFailures[Any]] = List(
      Validate.isTrue(false),
      Validate.isFalse(true, "Message"),
      Validate.notBlank(" ", Name),
      Validate.notEmpty("", Name),
      Validate.noDuplicates(Array(1.0, 1.0), Name),
      Validate.notPositive(1, Name),
      Validate.notNegative(-1, Name),
      Validate.notNaN(Double.NaN, Name),
      Validate.notNegativeOrZero(0, Name),
      Validate.notZero(0.0, Name),
      Validate.inRange(2.0, 0.0, 1.0, Name))
    rejected.foreach(outcome => outcome should beFailure)
    rejected.flatMap(outcome => failuresOf(outcome)) should have size rejected.size.toLong
  }

  test("the tolerance-bearing checks report an unusable tolerance as a value, not as an error") {
    noException should be thrownBy Validate.notZero(1.0, -0.1, Name)
    noException should be thrownBy Validate.notNegativeOrZero(1.0, -0.1, Name)
    noException should be thrownBy Validate.notZero(1.0, Double.NaN, Name)
    noException should be thrownBy Validate.notNegativeOrZero(1.0, Double.NaN, Name)
    Validate.notZero(1.0, -0.1, Name) should beFailure
    Validate.notNegativeOrZero(1.0, -0.1, Name) should beFailure
    Validate.notZero(1.0, Double.NaN, Name) should beFailure
    Validate.notNegativeOrZero(1.0, Double.NaN, Name) should beFailure
  }

  test("combining failing checks does not throw either") {
    noException should be thrownBy {
      (Validate.notBlank("", Name), Validate.notPositive(1, Name))
        .mapN((text, number) => (text, number))
    }
  }

  //-------------------------------------------------------------------------
  // Properties over inputs no table can enumerate.

  test("notPositive passes an int exactly when it is zero or less, and quotes it otherwise") {
    forAll { (argument: Int) =>
      if (argument > 0) {
        messageOf(Validate.notPositive(argument, Name)) shouldBe
          s"Argument 'name' must not be positive but has value $argument"
      } else {
        Validate.notPositive(argument, Name) should haveValue(argument)
      }
    }
  }

  test("notNegative passes an int exactly when it is zero or greater, and quotes it otherwise") {
    forAll { (argument: Int) =>
      if (argument < 0) {
        messageOf(Validate.notNegative(argument, Name)) shouldBe
          s"Argument 'name' must not be negative but has value $argument"
      } else {
        Validate.notNegative(argument, Name) should haveValue(argument)
      }
    }
  }

  test("notNegativeOrZero passes an int exactly when it is above zero, and quotes it otherwise") {
    forAll { (argument: Int) =>
      if (argument <= 0) {
        messageOf(Validate.notNegativeOrZero(argument, Name)) shouldBe
          s"Argument 'name' must not be negative or zero but has value $argument"
      } else {
        Validate.notNegativeOrZero(argument, Name) should haveValue(argument)
      }
    }
  }

  test("notNegative passes a long exactly when it is zero or greater, and quotes it otherwise") {
    forAll { (argument: Long) =>
      if (argument < 0L) {
        messageOf(Validate.notNegative(argument, Name)) shouldBe
          s"Argument 'name' must not be negative but has value $argument"
      } else {
        Validate.notNegative(argument, Name) should haveValue(argument)
      }
    }
  }

  test("inRange over ints passes exactly the arguments inside the interval it was given") {
    forAll { (argument: Int, lowInclusive: Int, highExclusive: Int) =>
      val outcome = Validate.inRange(argument, lowInclusive, highExclusive, Name)
      if (argument >= lowInclusive && argument < highExclusive) {
        outcome should haveValue(argument)
      } else {
        messageOf(outcome) shouldBe
          s"Expected $lowInclusive <= 'name' < $highExclusive, but found $argument"
      }
    }
  }

  test("notBlank passes text exactly when it holds a character that is not whitespace") {
    forAll { (argument: String) =>
      val outcome = Validate.notBlank(argument, Name)
      if (argument.trim.isEmpty) {
        messageOf(outcome) shouldBe "Argument 'name' must not be blank"
      } else {
        outcome should haveValue(argument)
      }
    }
  }

  test("notEmpty passes an iterable exactly when it holds an element") {
    forAll { (argument: List[Int]) =>
      val outcome = Validate.notEmpty(argument, Name)
      if (argument.isEmpty) {
        messageOf(outcome) shouldBe "Argument iterable 'name' must not be empty"
      } else {
        outcome should haveValue(argument)
      }
    }
  }

  test("a passing check carries back the very value it was handed, never a copy of it") {
    forAll { (argument: String) =>
      whenever(argument.nonEmpty) {
        valueOf(Validate.notEmpty(argument, Name)) should be theSameInstanceAs argument
      }
    }
  }

  test("the two halves agree over every int, one by throwing and one by reporting") {
    forAll { (argument: Int) =>
      val outcome = Validate.notPositive(argument, Name)
      if (argument > 0) {
        messageOf(outcome) shouldBe thrownMessageOf(ArgCheck.notPositive(argument, Name))
      } else {
        noException should be thrownBy ArgCheck.notPositive(argument, Name)
        outcome should beSuccess
      }
    }
  }

  test("combining two checks over arbitrary ints accumulates exactly the failures that occurred") {
    forAll { (first: Int, second: Int) =>
      val outcome = (Validate.notPositive(first, Name), Validate.notNegative(second, Name))
        .mapN((positive, negative) => (positive, negative))
      val expected = (if (first > 0) 1 else 0) + (if (second < 0) 1 else 0)
      failuresOf(outcome) should have size expected.toLong
      if (expected == 0) {
        outcome should haveValue((first, second))
      } else {
        outcome should beFailureWith(FailureReason.INVALID)
      }
    }
  }

}
