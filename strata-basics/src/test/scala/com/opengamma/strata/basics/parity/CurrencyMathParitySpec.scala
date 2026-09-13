/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.parity

import java.math.RoundingMode

import scala.util.matching.Regex

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all._

import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.HCursor
import io.circe.Json
import io.circe.JsonObject
import io.circe.generic.semiauto.deriveDecoder

import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

import com.opengamma.strata.basics.currency.BigMoney
import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.CurrencyAmount
import com.opengamma.strata.basics.currency.CurrencyAmountArray
import com.opengamma.strata.basics.currency.CurrencyPair
import com.opengamma.strata.basics.currency.FxRate
import com.opengamma.strata.basics.currency.FxRateProvider
import com.opengamma.strata.basics.currency.Money
import com.opengamma.strata.basics.currency.MultiCurrencyAmount
import com.opengamma.strata.basics.currency.MultiCurrencyAmountArray
import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.json.Codecs.implicits._
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.FailureReason

/**
 * Parity of the currency arithmetic of this module against the committed baseline.
 *
 * The measurement pins the money layer: `CurrencyAmount` and its normalisations, `Money` and
 * `BigMoney` with their rounding to a currency's minor units and to scale twelve, the two
 * amount-array types with their element-wise arithmetic and their aggregation, and
 * `MultiCurrencyAmount` with its duplicate-rejecting factory and its merging total. The committed
 * baseline `strata-basics/src/test/resources/parity/currency-math-baseline.json` holds the
 * reference values and is read-only here: no expectation is corrected, loosened or skipped.
 *
 * ===Two comparison rules, and which applies where===
 *
 * Every `Double` this module produces - an amount, an array element, a converted value - goes
 * through [[ParityHarness.assertParity]], which requires the difference to be within `1e-9`
 * '''absolutely and relatively'''.
 *
 * Every `Money` and `BigMoney` expectation goes through [[ParityHarness.assertExact]] and never
 * through a tolerance, because those amounts are recorded as '''decimal strings''' and rounding to
 * a currency's minor units is a discrete decision: `BHD 100.120` and `BHD 100.12` are the same
 * quantity and different answers, only one of which `Money.of(BHD, 100.12)` produces, and a
 * tolerance would accept either. Currencies, sizes and indices compare exactly for the same reason.
 *
 * A third rule applies to the entries that carry `doubleToLongBits`. Positive and negative zero are
 * within every tolerance of each other and are not the same representation, so where an entry
 * carries that key this spec compares the '''bit pattern''' of the value produced against the
 * captured `long`. Those entries pin three distinct answers - `CurrencyAmount.of` normalises `-0.0`
 * away, `CurrencyAmountArray` keeps the sign bit of its elements, and `CurrencyAmountArray.get`
 * normalises it again on the way out - and a sign-blind implementation satisfies none of them.
 *
 * ===Failures are values, with exactly one exception===
 *
 * Every `error` entry is an expectation in its own right. The data-dependent ones - a currency
 * mismatch, an unknown currency, a size mismatch, a duplicate currency, a rate that is not one
 * where no conversion is required, a missing rate - are reported in the error channel and are
 * checked with [[ParityHarness.assertLeft]]; there is no exception-interception helper, no
 * exception handler and nothing that reaches into an `Either` in this file.
 *
 * The single exception is the entry that adds `+Infinity` to `-Infinity` in one currency.
 * Arithmetic on `CurrencyAmount` is total in signature, so a sum that is not a number is refused by
 * the type's invariant rather than returned as a failure; it is observed with
 * [[ParityHarness.attemptArgCheck]], and the choice between the two is made from the operands
 * rather than configured: an `error` entry whose two amounts name '''the same''' currency can only
 * be that numeric edge, because a same-currency addition has no other way to fail. Captured
 * message text is never asserted.
 *
 * ===The fixture grammar, and why there is no positional pairing===
 *
 * The document holds six rows, each carrying the same fifteen keys: an identity, the `source` of
 * its values, seven input lists and six expectation buckets. The input lists are a '''registry''' -
 * the deduplicated, insertion-ordered record of the values the expectations were computed from -
 * and not a positional pairing: an expectation names its operation in `op` and carries its own
 * operands inline. The registry is used for what it is good for, which is checking that every
 * recorded input can still be built and still renders as it did
 * ([[CurrencyMathParitySpec.checkRegistry]]); an entry whose shape this spec does not recognise is
 * '''reported''' as a fixture disagreement rather than passed over.
 *
 * ===The document's shape is declared rather than assumed===
 *
 * Every object this spec reads - the row, an entry of each of the six buckets, and each nested
 * value - declares the key sets it carries as a [[KeySchema]] beside the model that describes it,
 * and is read through [[ParityHarness.strictObject]], which checks those keys before the object is
 * decoded. Those declarations are needed because the models are permissive: a bucket holds several
 * operations, so every operand field is optional, and a decoder with optional fields reads an entry
 * that has gained a key, lost one or had one renamed just as happily as the documented one. The
 * gained key would be ignored, the lost operand would become `None`, the check would measure a
 * smaller operation or none at all, and a newly captured expectation would go unmeasured while the
 * report still read `failed == 0`. The declared shapes are ten in `currencyAmountResults`, eight in
 * `moneyResults`, nine in `bigMoneyResults`, twelve in `currencyAmountArrayResults`, nine in
 * `multiCurrencyAmountResults` and eleven in `multiCurrencyAmountArrayResults`, each naming its
 * operation in `op` and stating exactly one of `result` and `error`.
 *
 * The two money buckets have two schemas rather than one: `Money` and `BigMoney` are read through
 * the same model but hold different operations - only `BigMoney` records `roundToScale`, only
 * `Money` records a conversion through a rate list - so each bucket is read against its own key
 * sets, and the row decoder names the decoder of each bucket explicitly instead of leaving the
 * choice to an implicit lookup that would find one decoder for both.
 *
 * ===Where a replayed rate comes from===
 *
 * Every captured conversion that names its rates is replayed against its own. An entry that names
 * none resolves its rate from the '''document-wide''' rate registry - every `rates` entry of every
 * row and of every entry, in document order - so no literal rate appears in this file, and the
 * resolution is confined to entries carrying no rate of their own, which is why no `error`
 * expectation can be satisfied by it.
 *
 * That registry is assembled '''by the driver''', not by the suite:
 * [[ParityHarness.runFixtureWith]] decodes the rows and builds it under `attempt` inside the
 * measurement, so a captured rate that has stopped being buildable yields a report of `passed = 0`
 * with a single discrepancy attributed to `currency-math:setup`, and the verdict is still left to
 * [[ParityHarness.failIfAny]]. Building it ahead of the driver would raise before any report was
 * written.
 *
 * The population test reads the document a second time because it measures the coverage of the
 * fixture rather than the implementation, and publishes no report: a check of `failed == 0` cannot
 * distinguish a complete measurement from a thinned one, so that test asserts the population the
 * baseline must carry, including all three distinct `minorUnitDigits` values and the
 * seventy-four-currency sweeps.
 */
class CurrencyMathParitySpec extends AsyncFunSuite with AsyncIOSpec with Matchers {

  import CurrencyMathParitySpec._

  test("currency arithmetic reproduces the Java baseline exactly") {
    for {
      // The registry is built by the driver rather than here, so a registry that cannot be built
      // is an attributed failure in a published report rather than an exception raised before any
      // report exists.
      report <- ParityHarness.runFixtureWith[CurrencyMathRow, FxRateProvider](
        FixtureName,
        FixtureResource,
        RowSchema)(rows => registryProvider(rows))((registry, row) => checkRow(registry, row))
      _ <- ParityHarness.failIfAny(report)
    } yield succeed
  }

  test("the fixture carries the population the currency-math baseline is required to measure") {
    ParityHarness.loadStrict[CurrencyMathRow](FixtureResource, RowSchema).map { rows =>
      val entryCount = rows.map(entryCountOf).sum
      val errorCount = rows.map(errorCountOf).sum
      withClue(
        s"fixture rows: ${rows.map(_.id).mkString(", ")}; expectation entries: $entryCount; " +
          s"error entries: $errorCount: ") {
        // The six rows are named rather than counted: each of the six families has to be present,
        // and a row that vanished would otherwise be hidden by a row that was added.
        rows.map(_.id) shouldBe RowIds
        // Floors rather than equalities, so that extending the fixture stays possible while
        // thinning it cannot pass.
        entryCount should be >= MinimumEntries
        errorCount should be >= MinimumErrorEntries
        // Every row carries the bucket of its own family and nothing else, which is the invariant
        // the row shape rests on; `checkRow` measures a row on that basis.
        rows.filterNot(row => populatedBuckets(row) == Vector(bucketOf(row.id))).map(_.id) shouldBe empty
        // Money rounding is only measured if all three distinct minor-unit scales are exercised:
        // zero digits, two and three. This is the property the per-currency sweeps exist for.
        minorUnitDigitsCovered(rows) shouldBe RequiredMinorUnitDigits
        // The two sweeps across every configured and historic currency, which hold the rounding
        // claim for the whole closed currency family rather than for a representative of it.
        sweptCurrencies(rows, _.moneyResults) should have size CurrencyFamilySize.toLong
        sweptCurrencies(rows, _.bigMoneyResults) should have size CurrencyFamilySize.toLong
        // The entries that pin the sign of a zero, without which the three distinct answers
        // would all be satisfied by a sign-blind implementation.
        signedZeroEntries(rows) should be >= MinimumSignedZeroEntries
        succeed
      }
    }
  }

  test("a union-typed value is decided by its shape and whole key set, not by declaration order") {
    IO.pure(unionDecoderDiscrepancies).map(discrepancies => discrepancies shouldBe empty)
  }

  test("a captured description is accepted only when the grammar accounts for all of it") {
    describedFormDiscrepancies.map(discrepancies => discrepancies shouldBe empty)
  }

  test("a captured object whose keys are not the documented ones is refused rather than measured") {
    IO {
      withClue(s"probed shapes: ${StrictShapes.map(_.label).mkString("; ")}: ") {
        // Every nested value shape, one entry of each of the six buckets, and the row: the
        // documented object decodes, and the same object with a key added, with a required key
        // removed and with a key renamed is refused with the key named.
        strictnessViolations shouldBe empty
        // And the two money buckets, whose entries share one model, do not accept each other's
        // shapes.
        crossFamilyViolations shouldBe empty
        succeed
      }
    }
  }
}

/** The row model of `currency-math-baseline.json` and the checks applied to one row. */
private[parity] object CurrencyMathParitySpec {

  /** The fixture stem, which names the measurement and its report document. */
  val FixtureName: String = "currency-math"

  val FixtureResource: String = "parity/currency-math-baseline.json"

  /** The mapping function the composed expectations were computed with, as an entry states it. */
  val MapAmountsText: String = "x -> x * x"

  private def mapAmountsFunction(value: Double): Double = value * value

  /** The description an entry carries for the amount that holds nothing. */
  private val EmptyDescription: String = "empty"

  /** The six row identities, in the order the document lists them. */
  val RowIds: Vector[String] =
    Vector(
      "currency-amount",
      "money",
      "big-money",
      "currency-amount-array",
      "multi-currency-amount",
      "multi-currency-amount-array")

  /** The number of expectation entries the baseline carries, used as a floor. */
  val MinimumEntries: Int = 355

  /** The number of `error` entries the baseline carries, used as a floor. */
  val MinimumErrorEntries: Int = 29

  /** The three distinct minor-unit scales of the closed currency family, all of which must appear. */
  val RequiredMinorUnitDigits: Set[Int] = Set(0, 2, 3)

  /** The size of the closed currency family, which both money sweeps cover in full. */
  val CurrencyFamilySize: Int = 74

  /** The entries that pin the sign of a zero through a captured bit pattern. */
  val MinimumSignedZeroEntries: Int = 4

  //-------------------------------------------------------------------------
  // The captured value shapes. Each reads its object through `ParityHarness.strictObject` against
  // the key set declared beside it, because a decoder on its own ignores every key it does not
  // know; a decoder is written out where a JSON key is not a name a field may take.
  //-------------------------------------------------------------------------

  /** A `CurrencyAmount`: `{"currency": "GBP", "amount": 100.0}`. */
  final case class AmountValue(currency: String, amount: Double)

  /** A `CurrencyAmountArray`: `{"currency": "GBP", "values": [1.0, 2.0, 3.0]}`. */
  final case class ArrayValue(currency: String, values: Vector[Double])

  /** A `Money` or a `BigMoney`, whose `amount` is a decimal string and `text` its own rendering. */
  final case class MoneyValue(currency: String, amount: String, text: String)

  /** A `MultiCurrencyAmountArray`: `{"size": 3, "arrays": [{"currency": …, "values": …}]}`. */
  final case class MultiArrayValue(size: Int, arrays: Vector[ArrayValue])

  /** A registered multi-currency run, which the input list records without its size. */
  final case class MultiArrayInput(arrays: Vector[ArrayValue])

  /** A rate a provider was built from: `{"pair": "GBP/USD", "rate": 1.6}`. */
  final case class RateValue(pair: String, rate: Double)

  val AmountValueSchema: KeySchema =
    KeySchema.uniform("a captured CurrencyAmount", Set("currency", "amount"))

  val ArrayValueSchema: KeySchema =
    KeySchema.uniform("a captured CurrencyAmountArray", Set("currency", "values"))

  val MoneyValueSchema: KeySchema =
    KeySchema.uniform("a captured Money or BigMoney", Set("currency", "amount", "toString"))

  val MultiArrayValueSchema: KeySchema =
    KeySchema.uniform("a captured MultiCurrencyAmountArray", Set("size", "arrays"))

  /** The schema of a registered multi-currency run, which carries `arrays` without a `size`. */
  val MultiArrayInputSchema: KeySchema =
    KeySchema.uniform("a registered multi-currency run", Set("arrays"))

  val RateValueSchema: KeySchema =
    KeySchema.uniform("a captured FX rate", Set("pair", "rate"))

  implicit val amountValueDecoder: Decoder[AmountValue] =
    ParityHarness.strictObject(AmountValueSchema)(deriveDecoder[AmountValue])

  implicit val arrayValueDecoder: Decoder[ArrayValue] =
    ParityHarness.strictObject(ArrayValueSchema)(deriveDecoder[ArrayValue])

  implicit val moneyValueDecoder: Decoder[MoneyValue] =
    ParityHarness.strictObject(MoneyValueSchema)(Decoder.instance(cursor =>
      for {
        currency <- cursor.get[String]("currency")
        amount <- cursor.get[String]("amount")
        text <- cursor.get[String]("toString")
      } yield MoneyValue(currency, amount, text)))

  implicit val multiArrayValueDecoder: Decoder[MultiArrayValue] =
    ParityHarness.strictObject(MultiArrayValueSchema)(deriveDecoder[MultiArrayValue])

  implicit val multiArrayInputDecoder: Decoder[MultiArrayInput] =
    ParityHarness.strictObject(MultiArrayInputSchema)(deriveDecoder[MultiArrayInput])

  implicit val rateValueDecoder: Decoder[RateValue] =
    ParityHarness.strictObject(RateValueSchema)(deriveDecoder[RateValue])

  //-------------------------------------------------------------------------
  // The expectation of one entry.
  //-------------------------------------------------------------------------

  /** What one operation produced: a value, or a refusal, which is an expectation in its own right. */
  sealed trait Expected[+A]

  object Expected {

    final case class Value[+A](value: A) extends Expected[A]

    /** The operation was refused, with this message, used in diagnostics and never asserted. */
    final case class Failed(message: String) extends Expected[Nothing]
  }

  /** Reads an entry's expectation: exactly one of `error` and `result`, else a decoding failure. */
  private def expectedOf[A: Decoder](cursor: HCursor): Decoder.Result[Expected[A]] = {
    val error = cursor.downField("error")
    val result = cursor.downField("result")
    (error.succeeded, result.succeeded) match {
      case (true, false) => error.as[String].map(Expected.Failed(_))
      case (false, true) => result.as[A].map(Expected.Value(_))
      case (true, true) =>
        Left(
          DecodingFailure(
            "an expectation entry carries both 'result' and 'error', and exactly one of them " +
              "states the expectation",
            cursor.history))
      case (false, false) =>
        Left(
          DecodingFailure(
            "an expectation entry carries neither 'result' nor 'error', and exactly one of them " +
              "states the expectation",
            cursor.history))
    }
  }

  //-------------------------------------------------------------------------
  // The operands whose shape is a union.
  //
  // Three keys - `right`, `input` and `result` - carry more than one shape, because the members
  // they record are overloaded: an amount array is added to another array or to a single amount, a
  // multi-currency run to another run or to a single multi-currency amount, and a run's expectation
  // is a run, a converted single-currency array or a bare list of values.
  //
  // All five unions below are decoded by `unionDecoder`, which decides the alternative from the
  // document and never from declaration order: the JSON shape selects first, and an object must
  // carry exactly one alternative's whole key set. A chain of alternative decoders cannot do that,
  // because a derived product decoder ignores a key it does not know, so an object carrying the
  // keys of two alternatives would be accepted by whichever is tried first and the sibling's
  // fields discarded without a word - a captured expectation silently replaced by a smaller one.
  //-------------------------------------------------------------------------

  /** The JSON shape that selects one alternative of a union, naming its '''whole''' key set. */
  private sealed trait UnionShape {

    def describe: String
  }

  private object UnionShape {

    /** Selected by the value being a JSON array, of which a union has at most one alternative. */
    case object ArrayShape extends UnionShape {
      val describe: String = "a JSON array"
    }

    /** Selected by the value being a JSON string, of which a union has at most one alternative. */
    case object StringShape extends UnionShape {
      val describe: String = "a JSON string"
    }

    /** Selected by the value being a JSON object whose key set equals [[keys]] exactly. */
    final case class ObjectShape(name: String, keys: Set[String]) extends UnionShape {
      val describe: String =
        s"$name, an object with exactly the keys {${keys.toVector.sorted.mkString(", ")}}"
    }
  }

  /** One alternative of a union: the shape that selects it, and the decoder that reads it. */
  private final case class UnionVariant[A](shape: UnionShape, decoder: Decoder[A])

  private val ArrayValueShape: UnionShape =
    UnionShape.ObjectShape("an array of amounts in one currency", Set("currency", "values"))

  private val AmountValueShape: UnionShape =
    UnionShape.ObjectShape("a single amount", Set("currency", "amount"))

  private val MultiArrayValueShape: UnionShape =
    UnionShape.ObjectShape("a multi-currency run", Set("size", "arrays"))

  /**
   * Decodes a union by deciding the alternative before reading a field of it: from the value's
   * shape and, for an object, from its '''whole''' key set. Nothing falls back to a later
   * alternative, so no captured field can be discarded by an earlier one.
   */
  private def unionDecoder[A](label: String, variants: Vector[UnionVariant[A]]): Decoder[A] =
    Decoder.instance { cursor =>
      val json = cursor.value
      val claimed =
        json.asObject match {
          case Some(obj) =>
            val keys = obj.keys.toSet
            variants.filter(variant =>
              variant.shape match {
                case UnionShape.ObjectShape(_, accepted) => accepted == keys
                case _ => false
              })
          case None if json.isArray => variants.filter(_.shape == UnionShape.ArrayShape)
          case None if json.isString => variants.filter(_.shape == UnionShape.StringShape)
          case None => Vector.empty
        }
      claimed match {
        case Vector(variant) => variant.decoder(cursor)
        case _ => Left(DecodingFailure(unionFailureMessage(label, json, variants), cursor.history))
      }
    }

  /** The message of a union no alternative claims: what was found, and every accepted form. */
  private def unionFailureMessage[A](
      label: String,
      json: Json,
      variants: Vector[UnionVariant[A]]): String = {
    val found =
      json.asObject match {
        case Some(obj) => s"an object with the keys {${obj.keys.toVector.sorted.mkString(", ")}}"
        case None if json.isArray => "a JSON array"
        case None if json.isString => "a JSON string"
        case None => s"the value ${json.noSpaces}"
      }
    s"$label does not state exactly one captured shape: found $found, and the accepted shapes " +
      s"are ${variants.map(_.shape.describe).mkString("; ")}"
  }

  /** The right-hand operand, or the expectation, of a single-currency amount array operation. */
  sealed trait ArrayOperand

  object ArrayOperand {

    final case class Run(value: ArrayValue) extends ArrayOperand

    /** One amount, which an array operation broadcasts over its elements. */
    final case class Single(value: AmountValue) extends ArrayOperand
  }

  implicit val arrayOperandDecoder: Decoder[ArrayOperand] =
    unionDecoder[ArrayOperand](
      "the operand or expectation of an amount-array operation",
      Vector(
        UnionVariant(ArrayValueShape, arrayValueDecoder.map[ArrayOperand](ArrayOperand.Run(_))),
        UnionVariant(
          AmountValueShape,
          amountValueDecoder.map[ArrayOperand](ArrayOperand.Single(_)))))

  /** The construction input of an amount array: a literal list of values, or a description. */
  sealed trait ArrayInput

  object ArrayInput {

    /** The values the amounts were built from, all in the entry's currency. */
    final case class Values(values: Vector[Double]) extends ArrayInput

    /** A description such as `"GBP 4, USD 5"`, which names amounts in more than one currency. */
    final case class Described(text: String) extends ArrayInput
  }

  implicit val arrayInputDecoder: Decoder[ArrayInput] =
    unionDecoder[ArrayInput](
      "the construction input of an amount-array operation",
      Vector(
        UnionVariant(
          UnionShape.ArrayShape,
          Decoder[Vector[Double]].map[ArrayInput](ArrayInput.Values(_))),
        UnionVariant(
          UnionShape.StringShape,
          Decoder[String].map[ArrayInput](ArrayInput.Described(_)))))

  /** The expectation of a multi-currency amount operation: every amount, or a single one. */
  sealed trait MultiOperand

  object MultiOperand {

    /** The amounts of a multi-currency value, written sorted by currency code. */
    final case class Amounts(amounts: Vector[AmountValue]) extends MultiOperand

    /** One amount, which is what `getAmount` and `convertedTo` produce. */
    final case class Single(value: AmountValue) extends MultiOperand
  }

  implicit val multiOperandDecoder: Decoder[MultiOperand] =
    unionDecoder[MultiOperand](
      "the expectation of a MultiCurrencyAmount operation",
      Vector(
        UnionVariant(
          UnionShape.ArrayShape,
          Decoder[Vector[AmountValue]].map[MultiOperand](MultiOperand.Amounts(_))),
        UnionVariant(
          AmountValueShape,
          amountValueDecoder.map[MultiOperand](MultiOperand.Single(_)))))

  /** The right-hand operand of a multi-currency run operation: another run, or one amount. */
  sealed trait MultiArrayOperand

  object MultiArrayOperand {

    final case class Run(value: MultiArrayValue) extends MultiArrayOperand

    /** One multi-currency amount, which a run operation broadcasts over its indices. */
    final case class Amounts(amounts: Vector[AmountValue]) extends MultiArrayOperand
  }

  implicit val multiArrayOperandDecoder: Decoder[MultiArrayOperand] =
    unionDecoder[MultiArrayOperand](
      "the right-hand operand of a MultiCurrencyAmountArray operation",
      Vector(
        UnionVariant(
          MultiArrayValueShape,
          multiArrayValueDecoder.map[MultiArrayOperand](MultiArrayOperand.Run(_))),
        UnionVariant(
          UnionShape.ArrayShape,
          Decoder[Vector[AmountValue]].map[MultiArrayOperand](MultiArrayOperand.Amounts(_)))))

  sealed trait MultiArrayResult

  object MultiArrayResult {

    final case class Run(value: MultiArrayValue) extends MultiArrayResult

    /** A single-currency array, which is what `convertedTo` produces. */
    final case class Converted(value: ArrayValue) extends MultiArrayResult

    /** The values held for one currency, which is what `getValues` produces. */
    final case class Values(values: Vector[Double]) extends MultiArrayResult
  }

  implicit val multiArrayResultDecoder: Decoder[MultiArrayResult] =
    unionDecoder[MultiArrayResult](
      "the expectation of a MultiCurrencyAmountArray operation",
      Vector(
        UnionVariant(
          MultiArrayValueShape,
          multiArrayValueDecoder.map[MultiArrayResult](MultiArrayResult.Run(_))),
        UnionVariant(
          ArrayValueShape,
          arrayValueDecoder.map[MultiArrayResult](MultiArrayResult.Converted(_))),
        UnionVariant(
          UnionShape.ArrayShape,
          Decoder[Vector[Double]].map[MultiArrayResult](MultiArrayResult.Values(_)))))

  //-------------------------------------------------------------------------
  // The five entry models, one per expectation bucket - the two money buckets share theirs.
  //
  // Every operand key is optional because the bucket holds several operations, and every decoder is
  // written out rather than derived because the expectation is a sum of `result` and `error` that
  // no derivation produces. An entry that carries an operand this spec does not expect for its
  // operation is reported by the check, not by the decoder: reading the document is one concern and
  // recognising an operation is another. The key sets declared below are what keeps those optional
  // fields honest, for the reason the class documentation gives.
  //-------------------------------------------------------------------------

  /** An entry shape whose expectation is a value: its operand keys, plus `op` and `result`. */
  private def valueVariant(name: String, keys: String*): (String, Set[String]) =
    name -> (keys.toSet + "op" + "result")

  /** An entry shape whose expectation is a refusal: its operand keys, plus `op` and `error`. */
  private def errorVariant(name: String, keys: String*): (String, Set[String]) =
    name -> (keys.toSet + "op" + "error")

  /** The schema of one entry of `currencyAmountResults`: the ten key sets that bucket carries. */
  val CurrencyAmountEntrySchema: KeySchema =
    KeySchema.variants(
      "a captured CurrencyAmount operation",
      valueVariant("scaled", "left", "scalar"),
      valueVariant("scaled-sub-minor-unit", "left", "scalar", "minorUnitDigits", "captureOnly"),
      valueVariant("scaled-with-bits", "left", "scalar", "doubleToLongBits"),
      valueVariant("unary", "left"),
      valueVariant("combined", "left", "right"),
      valueVariant("converted", "left", "rate", "target"),
      valueVariant("built-with-bits", "input", "doubleToLongBits"),
      errorVariant("refused-combination", "left", "right"),
      errorVariant("refused-conversion", "left", "rate", "target"),
      errorVariant("refused-construction", "input"))

  /** The schema of one entry of `moneyResults`: the eight key sets that bucket carries. */
  val MoneyEntrySchema: KeySchema =
    KeySchema.variants(
      "a captured Money operation",
      valueVariant("built", "currency", "amount", "minorUnitDigits", "captureOnly"),
      valueVariant("widened", "left"),
      valueVariant("scaled", "left", "scalar"),
      valueVariant("combined", "left", "right"),
      valueVariant("converted", "left", "rate", "target"),
      valueVariant("converted-by-provider", "left", "rates", "target"),
      errorVariant("refused-combination", "left", "right"),
      errorVariant("refused-conversion", "left", "rate", "target"))

  /** The schema of one entry of `bigMoneyResults`: the nine key sets that bucket carries. */
  val BigMoneyEntrySchema: KeySchema =
    KeySchema.variants(
      "a captured BigMoney operation",
      valueVariant("built", "currency", "amount", "minorUnitDigits", "captureOnly"),
      valueVariant("narrowed", "left", "minorUnitDigits", "captureOnly"),
      valueVariant("narrowed-plain", "left"),
      valueVariant("rounded-to-scale", "left", "scale", "roundingMode"),
      valueVariant("scaled", "left", "scalar"),
      valueVariant("combined", "left", "right"),
      valueVariant("converted", "left", "rate", "target"),
      errorVariant("refused-combination", "left", "right"),
      errorVariant("refused-conversion", "left", "rate", "target"))

  /** The schema of one entry of `currencyAmountArrayResults`: the twelve key sets it carries. */
  val CurrencyAmountArrayEntrySchema: KeySchema =
    KeySchema.variants(
      "a captured CurrencyAmountArray operation",
      valueVariant("built", "currency", "input"),
      valueVariant("built-with-size", "currency", "input", "size"),
      valueVariant("built-with-bits", "currency", "input", "doubleToLongBits"),
      valueVariant("element-with-bits", "left", "index", "doubleToLongBits"),
      valueVariant("combined", "left", "right"),
      valueVariant("scaled", "left", "scalar", "composed"),
      valueVariant("mapped", "left", "mapAmountsFn", "composed"),
      valueVariant("converted", "left", "rate", "target"),
      errorVariant("refused-combination", "left", "right"),
      errorVariant("refused-construction", "input"),
      errorVariant("refused-construction-with-size", "input", "size"),
      errorVariant("refused-conversion", "left", "rates", "target"))

  /** The schema of one entry of `multiCurrencyAmountResults`: the nine key sets it carries. */
  val MultiCurrencyAmountEntrySchema: KeySchema =
    KeySchema.variants(
      "a captured MultiCurrencyAmount operation",
      valueVariant("built", "amounts"),
      valueVariant("built-from-description", "input"),
      valueVariant("combined", "left", "right"),
      valueVariant("scaled", "left", "scalar"),
      valueVariant("mapped", "left", "mapAmountsFn"),
      valueVariant("looked-up", "currency", "left"),
      valueVariant("converted-by-provider", "left", "rates", "target"),
      errorVariant("refused-lookup", "currency", "left"),
      errorVariant("refused-construction", "input"))

  /** The schema of one entry of `multiCurrencyAmountArrayResults`: the eleven key sets it holds. */
  val MultiCurrencyAmountArrayEntrySchema: KeySchema =
    KeySchema.variants(
      "a captured MultiCurrencyAmountArray operation",
      valueVariant("built", "left"),
      valueVariant("built-from-description", "input"),
      valueVariant("built-from-description-with-size", "input", "size"),
      valueVariant("combined", "left", "right"),
      valueVariant("scaled", "left", "scalar", "composed"),
      valueVariant("mapped", "left", "mapAmountsFn", "composed"),
      valueVariant("looked-up", "currency", "left"),
      valueVariant("converted", "left", "rates", "target"),
      errorVariant("refused-combination", "left", "right"),
      errorVariant("refused-lookup", "currency", "left"),
      errorVariant("refused-construction", "input"))

  final case class CurrencyAmountEntry(
      op: String,
      left: Option[AmountValue],
      right: Option[AmountValue],
      input: Option[AmountValue],
      scalar: Option[Double],
      target: Option[String],
      rate: Option[Double],
      minorUnitDigits: Option[Int],
      doubleToLongBits: Option[Long],
      captureOnly: Boolean,
      expected: Expected[AmountValue])

  val currencyAmountEntryDecoder: Decoder[CurrencyAmountEntry] =
    ParityHarness.strictObject(CurrencyAmountEntrySchema)(Decoder.instance(cursor =>
      for {
        op <- cursor.get[String]("op")
        left <- cursor.get[Option[AmountValue]]("left")
        right <- cursor.get[Option[AmountValue]]("right")
        input <- cursor.get[Option[AmountValue]]("input")
        scalar <- cursor.get[Option[Double]]("scalar")
        target <- cursor.get[Option[String]]("target")
        rate <- cursor.get[Option[Double]]("rate")
        minorUnitDigits <- cursor.get[Option[Int]]("minorUnitDigits")
        bits <- cursor.get[Option[Long]]("doubleToLongBits")
        captureOnly <- cursor.get[Option[Boolean]]("captureOnly")
        expected <- expectedOf[AmountValue](cursor)
      } yield CurrencyAmountEntry(
        op,
        left,
        right,
        input,
        scalar,
        target,
        rate,
        minorUnitDigits,
        bits,
        captureOnly.getOrElse(false),
        expected)))

  /** One captured `Money` or `BigMoney` operation; the scalar is whole, as both types require. */
  final case class MoneyEntry(
      op: String,
      currency: Option[String],
      amount: Option[Double],
      left: Option[MoneyValue],
      right: Option[MoneyValue],
      scalar: Option[Long],
      target: Option[String],
      rate: Option[Double],
      rates: Option[Vector[RateValue]],
      scale: Option[Int],
      roundingMode: Option[String],
      minorUnitDigits: Option[Int],
      captureOnly: Boolean,
      expected: Expected[MoneyValue])

  /** The body both money buckets decode with, to which each bucket's own key schema applies. */
  private val moneyEntryBody: Decoder[MoneyEntry] =
    Decoder.instance(cursor =>
      for {
        op <- cursor.get[String]("op")
        currency <- cursor.get[Option[String]]("currency")
        amount <- cursor.get[Option[Double]]("amount")
        left <- cursor.get[Option[MoneyValue]]("left")
        right <- cursor.get[Option[MoneyValue]]("right")
        scalar <- cursor.get[Option[Long]]("scalar")
        target <- cursor.get[Option[String]]("target")
        rate <- cursor.get[Option[Double]]("rate")
        rates <- cursor.get[Option[Vector[RateValue]]]("rates")
        scale <- cursor.get[Option[Int]]("scale")
        roundingMode <- cursor.get[Option[String]]("roundingMode")
        minorUnitDigits <- cursor.get[Option[Int]]("minorUnitDigits")
        captureOnly <- cursor.get[Option[Boolean]]("captureOnly")
        expected <- expectedOf[MoneyValue](cursor)
      } yield MoneyEntry(
        op,
        currency,
        amount,
        left,
        right,
        scalar,
        target,
        rate,
        rates,
        scale,
        roundingMode,
        minorUnitDigits,
        captureOnly.getOrElse(false),
        expected))

  /** One entry of `moneyResults`, read against the key sets that bucket documents. */
  val moneyEntryDecoder: Decoder[MoneyEntry] =
    ParityHarness.strictObject(MoneyEntrySchema)(moneyEntryBody)

  /** One entry of `bigMoneyResults`, read against the key sets that bucket documents. */
  val bigMoneyEntryDecoder: Decoder[MoneyEntry] =
    ParityHarness.strictObject(BigMoneyEntrySchema)(moneyEntryBody)

  final case class CurrencyAmountArrayEntry(
      op: String,
      left: Option[ArrayValue],
      right: Option[ArrayOperand],
      input: Option[ArrayInput],
      currency: Option[String],
      scalar: Option[Double],
      target: Option[String],
      rate: Option[Double],
      rates: Option[Vector[RateValue]],
      size: Option[Int],
      index: Option[Int],
      mapAmountsFn: Option[String],
      composed: Boolean,
      doubleToLongBits: Option[Long],
      expected: Expected[ArrayOperand])

  val currencyAmountArrayEntryDecoder: Decoder[CurrencyAmountArrayEntry] =
    ParityHarness.strictObject(CurrencyAmountArrayEntrySchema)(Decoder.instance(cursor =>
      for {
        op <- cursor.get[String]("op")
        left <- cursor.get[Option[ArrayValue]]("left")
        right <- cursor.get[Option[ArrayOperand]]("right")
        input <- cursor.get[Option[ArrayInput]]("input")
        currency <- cursor.get[Option[String]]("currency")
        scalar <- cursor.get[Option[Double]]("scalar")
        target <- cursor.get[Option[String]]("target")
        rate <- cursor.get[Option[Double]]("rate")
        rates <- cursor.get[Option[Vector[RateValue]]]("rates")
        size <- cursor.get[Option[Int]]("size")
        index <- cursor.get[Option[Int]]("index")
        mapAmountsFn <- cursor.get[Option[String]]("mapAmountsFn")
        composed <- cursor.get[Option[Boolean]]("composed")
        bits <- cursor.get[Option[Long]]("doubleToLongBits")
        expected <- expectedOf[ArrayOperand](cursor)
      } yield CurrencyAmountArrayEntry(
        op,
        left,
        right,
        input,
        currency,
        scalar,
        target,
        rate,
        rates,
        size,
        index,
        mapAmountsFn,
        composed.getOrElse(false),
        bits,
        expected)))

  final case class MultiCurrencyAmountEntry(
      op: String,
      amounts: Option[Vector[AmountValue]],
      left: Option[Vector[AmountValue]],
      right: Option[Vector[AmountValue]],
      input: Option[String],
      scalar: Option[Double],
      currency: Option[String],
      target: Option[String],
      rates: Option[Vector[RateValue]],
      mapAmountsFn: Option[String],
      expected: Expected[MultiOperand])

  val multiCurrencyAmountEntryDecoder: Decoder[MultiCurrencyAmountEntry] =
    ParityHarness.strictObject(MultiCurrencyAmountEntrySchema)(Decoder.instance(cursor =>
      for {
        op <- cursor.get[String]("op")
        amounts <- cursor.get[Option[Vector[AmountValue]]]("amounts")
        left <- cursor.get[Option[Vector[AmountValue]]]("left")
        right <- cursor.get[Option[Vector[AmountValue]]]("right")
        input <- cursor.get[Option[String]]("input")
        scalar <- cursor.get[Option[Double]]("scalar")
        currency <- cursor.get[Option[String]]("currency")
        target <- cursor.get[Option[String]]("target")
        rates <- cursor.get[Option[Vector[RateValue]]]("rates")
        mapAmountsFn <- cursor.get[Option[String]]("mapAmountsFn")
        expected <- expectedOf[MultiOperand](cursor)
      } yield MultiCurrencyAmountEntry(
        op,
        amounts,
        left,
        right,
        input,
        scalar,
        currency,
        target,
        rates,
        mapAmountsFn,
        expected)))

  final case class MultiCurrencyAmountArrayEntry(
      op: String,
      left: Option[MultiArrayValue],
      right: Option[MultiArrayOperand],
      input: Option[String],
      size: Option[Int],
      currency: Option[String],
      target: Option[String],
      rates: Option[Vector[RateValue]],
      scalar: Option[Double],
      mapAmountsFn: Option[String],
      composed: Boolean,
      expected: Expected[MultiArrayResult])

  val multiCurrencyAmountArrayEntryDecoder: Decoder[MultiCurrencyAmountArrayEntry] =
    ParityHarness.strictObject(MultiCurrencyAmountArrayEntrySchema)(Decoder.instance(cursor =>
      for {
        op <- cursor.get[String]("op")
        left <- cursor.get[Option[MultiArrayValue]]("left")
        right <- cursor.get[Option[MultiArrayOperand]]("right")
        input <- cursor.get[Option[String]]("input")
        size <- cursor.get[Option[Int]]("size")
        currency <- cursor.get[Option[String]]("currency")
        target <- cursor.get[Option[String]]("target")
        rates <- cursor.get[Option[Vector[RateValue]]]("rates")
        scalar <- cursor.get[Option[Double]]("scalar")
        mapAmountsFn <- cursor.get[Option[String]]("mapAmountsFn")
        composed <- cursor.get[Option[Boolean]]("composed")
        expected <- expectedOf[MultiArrayResult](cursor)
      } yield MultiCurrencyAmountArrayEntry(
        op,
        left,
        right,
        input,
        size,
        currency,
        target,
        rates,
        scalar,
        mapAmountsFn,
        composed.getOrElse(false),
        expected)))

  /**
   * One row: the identity, the `source` of its values, the seven input lists and the six
   * expectation buckets. The fifteen keys are on every row, so no field of this product is optional
   * and an unused list or bucket is an empty array; five of the six buckets are empty on every row,
   * which is the invariant the population test asserts and the one [[checkRow]] dispatches on.
   */
  final case class CurrencyMathRow(
      id: String,
      source: String,
      amounts: Vector[AmountValue],
      scalars: Vector[Double],
      arrays: Vector[ArrayValue],
      multiArrays: Vector[MultiArrayInput],
      money: Vector[MoneyValue],
      bigMoney: Vector[MoneyValue],
      rates: Vector[RateValue],
      currencyAmountResults: Vector[CurrencyAmountEntry],
      moneyResults: Vector[MoneyEntry],
      bigMoneyResults: Vector[MoneyEntry],
      currencyAmountArrayResults: Vector[CurrencyAmountArrayEntry],
      multiCurrencyAmountResults: Vector[MultiCurrencyAmountEntry],
      multiCurrencyAmountArrayResults: Vector[MultiCurrencyAmountArrayEntry])
      extends ParityRow

  /** The schema of one '''row''': the fifteen keys every row carries, with nothing optional. */
  val RowSchema: KeySchema =
    KeySchema.uniform(
      "a currency-math baseline row",
      Set(
        "id",
        "source",
        "amounts",
        "scalars",
        "arrays",
        "multiArrays",
        "money",
        "bigMoney",
        "rates",
        "currencyAmountResults",
        "moneyResults",
        "bigMoneyResults",
        "currencyAmountArrayResults",
        "multiCurrencyAmountResults",
        "multiCurrencyAmountArrayResults"))

  /**
   * The row decoder, which names the decoder of every bucket explicitly, because `moneyResults` and
   * `bigMoneyResults` share the [[MoneyEntry]] model and do '''not''' share a key schema: the
   * bucket each entry is read against has to be decided here, where the bucket is known.
   */
  implicit val currencyMathRowDecoder: Decoder[CurrencyMathRow] =
    ParityHarness.strictObject(RowSchema)(Decoder.instance(cursor =>
      for {
        id <- cursor.get[String]("id")
        source <- cursor.get[String]("source")
        amounts <- cursor.get[Vector[AmountValue]]("amounts")
        scalars <- cursor.get[Vector[Double]]("scalars")
        arrays <- cursor.get[Vector[ArrayValue]]("arrays")
        multiArrays <- cursor.get[Vector[MultiArrayInput]]("multiArrays")
        money <- cursor.get[Vector[MoneyValue]]("money")
        bigMoney <- cursor.get[Vector[MoneyValue]]("bigMoney")
        rates <- cursor.get[Vector[RateValue]]("rates")
        amountResults <- cursor.get("currencyAmountResults")(
          Decoder.decodeVector(currencyAmountEntryDecoder))
        moneyResults <- cursor.get("moneyResults")(Decoder.decodeVector(moneyEntryDecoder))
        bigMoneyResults <- cursor.get("bigMoneyResults")(
          Decoder.decodeVector(bigMoneyEntryDecoder))
        runResults <- cursor.get("currencyAmountArrayResults")(
          Decoder.decodeVector(currencyAmountArrayEntryDecoder))
        multiResults <- cursor.get("multiCurrencyAmountResults")(
          Decoder.decodeVector(multiCurrencyAmountEntryDecoder))
        multiRunResults <- cursor.get("multiCurrencyAmountArrayResults")(
          Decoder.decodeVector(multiCurrencyAmountArrayEntryDecoder))
      } yield CurrencyMathRow(
        id,
        source,
        amounts,
        scalars,
        arrays,
        multiArrays,
        money,
        bigMoney,
        rates,
        amountResults,
        moneyResults,
        bigMoneyResults,
        runResults,
        multiResults,
        multiRunResults)))

  //-------------------------------------------------------------------------
  // Building the operands. Every operand is rebuilt through this module's own public factories, so
  // an operand that cannot be built is lifted into a failed effect through the harness's two lifts
  // and recorded against the row rather than passing as a parity result of any kind. The two arrays
  // are built with `DoubleArray.copyOf`, a copy-safe public factory; the unchecked factories that
  // alias a caller's array are module-private and unavailable here.
  //-------------------------------------------------------------------------

  private def currencyOf(code: String): IO[Currency] = ParityHarness.raise(Currency.parse(code))

  private def amountOf(value: AmountValue): IO[CurrencyAmount] =
    currencyOf(value.currency).flatMap(currency =>
      ParityHarness.raise(CurrencyAmount.of(currency, value.amount)))

  /** Rebuilds a captured money value from its exact decimal text, keeping the recorded scale. */
  private def moneyOf(value: MoneyValue): IO[Money] =
    for {
      currency <- currencyOf(value.currency)
      decimal <- ParityHarness.raise(Decimal.of(value.amount))
    } yield Money.of(currency, decimal)

  private def bigMoneyOf(value: MoneyValue): IO[BigMoney] =
    for {
      currency <- currencyOf(value.currency)
      decimal <- ParityHarness.raise(Decimal.of(value.amount))
    } yield BigMoney.of(currency, decimal)

  private def runOf(value: ArrayValue): IO[CurrencyAmountArray] =
    currencyOf(value.currency)
      .map(currency => CurrencyAmountArray.of(currency, DoubleArray.copyOf(value.values)))

  /** Rebuilds a captured multi-currency amount, whose currencies are distinct by construction. */
  private def multiOf(values: Vector[AmountValue]): IO[MultiCurrencyAmount] =
    values.toList
      .traverse(amountOf)
      .flatMap(amounts => ParityHarness.raise(MultiCurrencyAmount.of(amounts)))

  /** The values a captured run holds per currency; a size stated without arrays is refused. */
  private def runEntriesOf(value: MultiArrayValue): IO[Map[Currency, DoubleArray]] =
    if (value.arrays.isEmpty && value.size > 0) {
      IO.raiseError(
        new IllegalStateException(
          s"fixture disagreement: a multi-currency run operand states size ${value.size} while " +
            "holding no values, which the map factory cannot reproduce"))
    } else {
      value.arrays.toList
        .traverse(array =>
          currencyOf(array.currency).map(currency => (currency, DoubleArray.copyOf(array.values))))
        .map(entries => entries.toMap)
    }

  private def multiRunOf(value: MultiArrayValue): IO[MultiCurrencyAmountArray] =
    runEntriesOf(value)
      .flatMap(entries => ParityHarness.raiseNec(MultiCurrencyAmountArray.of(entries)))

  private def amountsOf(input: ArrayInput, currency: Option[String]): IO[Vector[CurrencyAmount]] =
    input match {
      case ArrayInput.Values(values) =>
        currency match {
          case Some(code) =>
            currencyOf(code).flatMap(resolved =>
              values.traverse(value => ParityHarness.raise(CurrencyAmount.of(resolved, value))))
          case None =>
            IO.raiseError(
              new IllegalStateException(
                "fixture disagreement: a literal list of values names no currency, so the " +
                  "amounts it describes cannot be built"))
        }
      case ArrayInput.Described(text) => parseAmounts(text).map(amounts => amounts.toVector)
    }

  //-------------------------------------------------------------------------
  // Reading the descriptions recorded for inputs that are not a single typed value.
  //
  // Some entries build from a list that cannot be written as one of the seven typed input lists - a
  // mixed-currency list of amounts, which is rejected, or a ragged list of multi-currency amounts,
  // which is zero-filled - so the document holds a short description instead, read back here
  // through this module's own parsers: `"GBP 4"` is exactly the form `CurrencyAmount.parse` takes.
  //
  // Every parser reads its description as an anchored grammar, because a description is an operand:
  // text such as `"junk [EUR 4] trailing"` read as the one group a search happens to find would
  // compare a smaller operand against an expectation computed from the whole one and report the
  // answer as parity. So the items and the documented separators between them have to account for
  // the whole of the trimmed text, and anything left over is reported as a fixture disagreement.
  // The accepted forms are `"empty"`, a comma-separated list of amounts, a comma-separated list of
  // bracketed groups of amounts, and per-currency runs separated by a comma or by a plus; a group
  // and a run's brackets may both be empty.
  //-------------------------------------------------------------------------

  /** The numeral production, which is the language `String.toDoubleOption` reads. */
  private val NumeralSource: String =
    """[+-]?(?:Infinity|NaN|(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?)"""

  /** One numeral of a bracketed value list. */
  private val Numeral: Regex = NumeralSource.r

  /** One amount, in exactly the form `CurrencyAmount.parse` accepts: `"GBP 4"`, `"USD -1.5"`. */
  private val AmountTerm: Regex = s"""[A-Za-z]{3} $NumeralSource""".r

  /** One bracketed group of a description such as `"[EUR 4], [GBP 21, USD 32, EUR 43]"`. */
  private val BracketGroup: Regex = """\[([^\[\]]*)\]""".r

  /** One currency and its values, as in `"GBP[1,2,3]"`. */
  private val CurrencyRun: Regex = """([A-Za-z]{3})\[([^\[\]]*)\]""".r

  /** The separator between the items of a captured list: a comma, with or without spacing. */
  private val ItemSeparator: Regex = """\s*,\s*""".r

  /** The separator between runs, which the document writes as a comma or as a plus. */
  private val RunSeparator: Regex = """\s*[,+]\s*""".r

  /** Whether a description names whole runs per currency rather than individual amounts. */
  private def describesRuns(text: String): Boolean = CurrencyRun.findFirstIn(text).isDefined

  /**
   * The items of a description, when the items and the separators account for all of it.
   *
   * This is the anchoring the four parsers share: the items are found by search and then have to
   * '''tile''' the text - the first starts at its beginning, the last ends at its end, and every
   * gap between two of them is one separator in full. A text with no item at all is refused here
   * too, so an unrecognisable description cannot pass as a list of nothing.
   */
  private def itemsOf(
      text: String,
      item: Regex,
      separator: Regex): Either[String, List[Regex.Match]] =
    item.findAllMatchIn(text).toList match {
      case Nil => Left("it names no item of that form at all")
      case items @ (first :: rest) =>
        // The items are destructured rather than indexed, so the first and the last of them are
        // read totally: this branch is the one where there is at least one.
        val lastItem = rest.lastOption.getOrElse(first)
        val gaps = items.zip(rest).map { case (left, right) =>
          text.substring(left.end, right.start)
        }
        if (first.start > 0) {
          Left(s"'${text.substring(0, first.start)}' precedes its first item")
        } else if (lastItem.end < text.length) {
          Left(s"'${text.substring(lastItem.end)}' follows its last item")
        } else {
          gaps.find(gap => !separator.matches(gap)) match {
            case Some(gap) => Left(s"'$gap' separates two of its items and is not one separator")
            case None => Right(items)
          }
        }
    }

  /** Reports a description its production does not account for in full, as a failed effect. */
  private def describedFormError[A](production: String, text: String, reason: String): IO[A] =
    IO.raiseError(
      new IllegalStateException(
        s"fixture disagreement: the description '$text' is not $production, because $reason"))

  /** The amounts a description such as `"GBP 4, USD 5"` names; `"empty"` names none. */
  private def parseAmounts(text: String): IO[List[CurrencyAmount]] = {
    val trimmed = text.trim
    if (trimmed == EmptyDescription) IO.pure(Nil) else parseAmountList(trimmed)
  }

  /** The amounts a non-empty comma-separated list names, shared by a description and a group. */
  private def parseAmountList(text: String): IO[List[CurrencyAmount]] =
    itemsOf(text, AmountTerm, ItemSeparator) match {
      case Right(items) =>
        items.traverse(item => ParityHarness.raise(CurrencyAmount.parse(item.matched)))
      case Left(reason) =>
        describedFormError("a comma-separated list of amounts such as 'GBP 4, USD 5'", text, reason)
    }

  /** The multi-currency amounts a description of bracketed groups names, one per group. */
  private def parseMultiAmounts(text: String): IO[List[MultiCurrencyAmount]] = {
    val trimmed = text.trim
    itemsOf(trimmed, BracketGroup, ItemSeparator) match {
      case Right(groups) => groups.traverse(group => parseGroup(group.group(1)))
      case Left(reason) =>
        describedFormError(
          "a comma-separated list of bracketed groups such as '[EUR 4], [GBP 21, USD 32]'",
          trimmed,
          reason)
    }
  }

  /** The multi-currency amount one bracketed group names, where `"[]"` names one holding none. */
  private def parseGroup(content: String): IO[MultiCurrencyAmount] = {
    val trimmed = content.trim
    val amounts =
      if (trimmed.isEmpty) IO.pure(List.empty[CurrencyAmount]) else parseAmountList(trimmed)
    amounts.flatMap(list => ParityHarness.raise(MultiCurrencyAmount.of(list)))
  }

  /** The runs a description such as `"GBP[1,2,3], USD[1,2]"` or `"GBP[1,2] + USD[3,4]"` names. */
  private def parseRuns(text: String): IO[List[(Currency, DoubleArray)]] = {
    val trimmed = text.trim
    itemsOf(trimmed, CurrencyRun, RunSeparator) match {
      case Right(runs) =>
        runs.traverse(run =>
          for {
            currency <- currencyOf(run.group(1))
            values <- parseValues(run.group(2))
          } yield (currency, DoubleArray.copyOf(values)))
      case Left(reason) =>
        describedFormError(
          "a list of per-currency runs such as 'GBP[1,2,3], USD[1,2]', separated by a comma or a plus",
          trimmed,
          reason)
    }
  }

  /** The values a comma-separated numeral list names; `"[]"` names none and `"1,,2"` is refused. */
  private def parseValues(text: String): IO[Vector[Double]] = {
    val trimmed = text.trim
    if (trimmed.isEmpty) {
      IO.pure(Vector.empty[Double])
    } else {
      itemsOf(trimmed, Numeral, ItemSeparator) match {
        case Right(values) =>
          // The numeral production is the language `toDoubleOption` reads, so the option is read
          // through `fromOption` to keep this total rather than because a match can fail to parse.
          values.toVector.traverse(value =>
            IO.fromOption(value.matched.toDoubleOption)(
              new IllegalStateException(
                s"fixture disagreement: '${value.matched}' is not a numeral, and a captured " +
                  "description holds only numerals between its brackets")))
        case Left(reason) =>
          describedFormError(
            "a comma-separated list of numerals, or nothing at all",
            trimmed,
            reason)
      }
    }
  }

  //-------------------------------------------------------------------------
  // The rate providers. A single rate is replayed as the `FxRate` it describes, which is itself an
  // `FxRateProvider`; several are replayed as a provider answering from the first of them that can,
  // which - unlike a matrix - does not require the rates to name a connected set of currencies, and
  // the document-wide registry's rates are not connected.
  //-------------------------------------------------------------------------

  private def fxRatesOf(rates: Vector[RateValue]): IO[Vector[FxRate]] =
    rates.traverse(entry =>
      ParityHarness
        .raise(CurrencyPair.parse(entry.pair))
        .flatMap(pair => ParityHarness.raiseNec(FxRate.of(pair, entry.rate))))

  private def providerOf(rates: Vector[FxRate]): FxRateProvider =
    rates match {
      case Vector(single) => single
      case several =>
        FxRateProvider.fromFunction((base, counter) =>
          several.iterator
            .map(rate => rate.fxRate(base, counter))
            .collectFirst { case Right(rate) => rate }
            .toRight(Failure.CurrencyConversion(s"No FX rate found for $base/$counter")))
    }

  private def providerFrom(rates: Vector[RateValue]): IO[FxRateProvider] =
    fxRatesOf(rates).map(providerOf)

  /**
   * Builds the provider used by an entry that carries no rate of its own: the rates of every row
   * and of every entry, in document order and deduplicated, so every rate replayed here is read
   * from the fixture rather than written in this file.
   */
  def registryProvider(rows: Vector[CurrencyMathRow]): IO[FxRateProvider] =
    providerFrom(registeredRates(rows))

  /** Every rate the document registers, in document order, without repetition. */
  private def registeredRates(rows: Vector[CurrencyMathRow]): Vector[RateValue] =
    (rows.flatMap(_.rates) ++
      rows.flatMap(_.moneyResults).flatMap(_.rates.getOrElse(Vector.empty)) ++
      rows.flatMap(_.bigMoneyResults).flatMap(_.rates.getOrElse(Vector.empty)) ++
      rows.flatMap(_.currencyAmountArrayResults).flatMap(_.rates.getOrElse(Vector.empty)) ++
      rows.flatMap(_.multiCurrencyAmountResults).flatMap(_.rates.getOrElse(Vector.empty))).distinct

  //-------------------------------------------------------------------------
  // The comparators, each built from the harness's own: a money value is compared exactly in all
  // three of its parts, and every other value compares its currency, size and indices exactly and
  // its numbers under the parity tolerance.
  //-------------------------------------------------------------------------

  /** Compares an amount: its currency exactly, its value under the tolerance. */
  private def compareAmount(label: String, actual: CurrencyAmount, expected: AmountValue): List[String] =
    ParityHarness.assertExact(s"$label.currency", actual.currency.name, expected.currency) :::
      ParityHarness.assertParity(s"$label.amount", actual.amount, expected.amount)

  /** Compares a single-currency array element by element. */
  private def compareRun(label: String, actual: CurrencyAmountArray, expected: ArrayValue): List[String] =
    ParityHarness.assertExact(s"$label.currency", actual.currency.name, expected.currency) :::
      ParityHarness.assertParitySeq(s"$label.values", actual.values.toList, expected.values)

  /**
   * Compares a money value exactly, never under a tolerance: the currency, the decimal text and the
   * rendering. `getValue` is failable on `Money`, so it goes through [[ParityHarness.assertRight]]
   * rather than being unwrapped.
   */
  private def compareMoney(label: String, actual: Money, expected: MoneyValue): List[String] =
    ParityHarness.assertExact(s"$label.currency", actual.currency.name, expected.currency) :::
      ParityHarness.assertRight(s"$label.value", actual.getValue)(value =>
        ParityHarness.assertExact(s"$label.amount", value.toString, expected.amount)) :::
      ParityHarness.assertExact(s"$label.toString", actual.toString, expected.text)

  /** Compares a big-money value exactly, whose decimal keeps the scale it was rounded to. */
  private def compareBigMoney(label: String, actual: BigMoney, expected: MoneyValue): List[String] =
    ParityHarness.assertExact(s"$label.currency", actual.currency.name, expected.currency) :::
      ParityHarness.assertExact(s"$label.amount", actual.getValue.toString, expected.amount) :::
      ParityHarness.assertExact(s"$label.toString", actual.toString, expected.text)

  /** Compares a multi-currency amount, sorting the expectation as `getAmounts` orders its own. */
  private def compareMulti(
      label: String,
      actual: MultiCurrencyAmount,
      expected: Vector[AmountValue]): List[String] = {
    val produced = actual.getAmounts.toVector
    val wanted = expected.sortBy(_.currency)
    if (produced.size != wanted.size) {
      List(
        s"$label: actual ${produced.size} amounts (${produced.mkString(", ")}), expected " +
          s"${wanted.size} (${wanted.mkString(", ")})")
    } else {
      produced.iterator
        .zip(wanted.iterator)
        .zipWithIndex
        .flatMap { case ((amount, want), index) => compareAmount(s"$label[$index]", amount, want) }
        .toList
    }
  }

  /** Compares a multi-currency run: size and currencies exactly, values under the tolerance. */
  private def compareMultiRun(
      label: String,
      actual: MultiCurrencyAmountArray,
      expected: MultiArrayValue): List[String] = {
    val produced = actual.values.toVector
    val wanted = expected.arrays.sortBy(_.currency)
    val currencies = produced.map { case (currency, _) => currency.name }
    val size = ParityHarness.assertExact(s"$label.size", actual.size, expected.size)
    val names = ParityHarness.assertExact(s"$label.currencies", currencies, wanted.map(_.currency))
    val values =
      if (currencies != wanted.map(_.currency)) {
        Nil
      } else {
        produced.iterator
          .zip(wanted.iterator)
          .flatMap { case ((currency, run), want) =>
            ParityHarness.assertParitySeq(s"$label.values[${currency.name}]", run.toList, want.values)
          }
          .toList
      }
    size ::: names ::: values
  }

  /**
   * Compares the bit pattern of a value whose sign the fixture pins: positive and negative zero are
   * within every tolerance of each other, so the captured `doubleToLongBits` is the authority here
   * and the JSON number beside it is not.
   */
  private def compareBits(label: String, actual: Double, expected: Long): List[String] = {
    val produced = java.lang.Double.doubleToLongBits(actual)
    if (produced == expected) {
      Nil
    } else {
      List(
        s"$label: actual $actual has bit pattern $produced, expected $expected; the sign of a " +
          "zero is part of this expectation and is not within any tolerance")
    }
  }

  /** Checks that an operation refused for the documented reason; the message is never compared. */
  private def assertLeftFor(
      label: String,
      actual: Either[Failure, Any],
      reason: FailureReason): List[String] =
    actual match {
      case Left(failure) if failure.reason == reason => Nil
      case Left(failure) =>
        List(s"$label: expected a ${reason.name} failure, but the operation failed with $failure")
      case Right(value) => List(s"$label: expected a failure, but the operation returned $value")
    }

  /** Checks the minor-unit scale an entry states against the one this module's currency holds. */
  private def checkMinorUnitDigits(
      label: String,
      currency: Currency,
      captured: Option[Int]): List[String] =
    captured.toList.flatMap(digits =>
      ParityHarness.assertExact(s"$label.minorUnitDigits", currency.minorUnitDigits, digits))

  /** Checks that a composed expectation used the mapping function this spec applies. */
  private def checkMapAmountsText(label: String, captured: Option[String]): List[String] =
    captured.toList.flatMap(text =>
      ParityHarness.assertExact(s"$label.mapAmountsFn", MapAmountsText, text))

  /** The message for an entry whose shape this spec does not recognise. */
  private def unrecognised(label: String, detail: String): List[String] =
    List(
      s"fixture disagreement: $label carries $detail, which this spec does not recognise; an " +
        "entry that cannot be replayed is reported rather than passed over")

  /** The provider an entry's own rates describe, or the document-wide one when it names none. */
  private def rateProviderFor(
      rates: Option[Vector[RateValue]],
      fallback: FxRateProvider): IO[FxRateProvider] =
    rates.fold(IO.pure(fallback))(providerFrom)

  /** The rounding modes by the constant name an entry carries, which is a closed set. */
  private val RoundingModesByName: Map[String, RoundingMode] =
    RoundingMode.values().iterator.map(mode => (mode.name(), mode)).toMap

  //-------------------------------------------------------------------------
  // The `CurrencyAmount` bucket.
  //-------------------------------------------------------------------------

  /** Measures one captured `CurrencyAmount` operation, answering with every discrepancy found. */
  private def checkAmountEntry(entry: CurrencyAmountEntry): IO[List[String]] =
    entry.op match {
      case "plus" => checkAmountPair(entry, (left, right) => left.plus(right))
      case "minus" => checkAmountPair(entry, (left, right) => left.minus(right))
      case "multipliedBy" => checkAmountScalar(entry)
      case "negated" => checkAmountUnary(entry, amount => amount.negated)
      case "positive" => checkAmountUnary(entry, amount => amount.positive)
      case "negative" => checkAmountUnary(entry, amount => amount.negative)
      case "convertedTo" => checkAmountConversion(entry)
      case "of" => checkAmountFactory(entry)
      case other => IO.pure(unrecognised("a CurrencyAmount entry", s"the operation '$other'"))
    }

  /**
   * Measures an addition or a subtraction of two amounts, and decides how its refusal is observed.
   *
   * The choice is made from the operands: two amounts in '''different''' currencies cannot be
   * combined at all and that refusal is a `Left`, while two in the '''same''' currency always
   * combine, so the only refusal left is the numeric edge of the type, which
   * [[ParityHarness.attemptArgCheck]] observes.
   */
  private def checkAmountPair(
      entry: CurrencyAmountEntry,
      operation: (CurrencyAmount, CurrencyAmount) => Either[Failure, CurrencyAmount]): IO[List[String]] =
    (entry.left, entry.right) match {
      case (Some(leftValue), Some(rightValue)) =>
        for {
          left <- amountOf(leftValue)
          right <- amountOf(rightValue)
          messages <- entry.expected match {
            case Expected.Value(expected) =>
              IO.pure(
                ParityHarness.assertRight(entry.op, operation(left, right))(actual =>
                  compareAmount(entry.op, actual, expected)))
            case Expected.Failed(_) if leftValue.currency != rightValue.currency =>
              IO.pure(assertLeftFor(entry.op, operation(left, right), FailureReason.INVALID))
            case Expected.Failed(_) =>
              ParityHarness.attemptArgCheck(entry.op)(operation(left, right))
          }
        } yield messages
      case _ =>
        IO.pure(unrecognised(s"the CurrencyAmount '${entry.op}' entry", "no pair of amounts"))
    }

  /** Measures a multiplication by a number, which is total, and the minor-unit scale it states. */
  private def checkAmountScalar(entry: CurrencyAmountEntry): IO[List[String]] =
    (entry.left, entry.scalar, entry.expected) match {
      case (Some(leftValue), Some(scalar), Expected.Value(expected)) =>
        for {
          currency <- currencyOf(leftValue.currency)
          left <- ParityHarness.raise(CurrencyAmount.of(currency, leftValue.amount))
        } yield {
          val product = left.multipliedBy(scalar)
          checkMinorUnitDigits(entry.op, currency, entry.minorUnitDigits) :::
            compareAmount(entry.op, product, expected) :::
            entry.doubleToLongBits.toList
              .flatMap(bits => compareBits(s"${entry.op}.amount", product.amount, bits))
        }
      case _ =>
        IO.pure(
          unrecognised(
            s"the CurrencyAmount '${entry.op}' entry",
            "no amount, number and value expectation"))
    }

  /** Measures a sign operation, all of which are total. */
  private def checkAmountUnary(
      entry: CurrencyAmountEntry,
      operation: CurrencyAmount => CurrencyAmount): IO[List[String]] =
    (entry.left, entry.expected) match {
      case (Some(leftValue), Expected.Value(expected)) =>
        amountOf(leftValue).map(left => compareAmount(entry.op, operation(left), expected))
      case _ =>
        IO.pure(
          unrecognised(s"the CurrencyAmount '${entry.op}' entry", "no amount and value expectation"))
    }

  /** Measures a conversion at an explicit rate, whose refusals are the same-currency rule. */
  private def checkAmountConversion(entry: CurrencyAmountEntry): IO[List[String]] =
    (entry.left, entry.target, entry.rate) match {
      case (Some(leftValue), Some(target), Some(rate)) =>
        for {
          left <- amountOf(leftValue)
          targetCurrency <- currencyOf(target)
        } yield {
          val converted = left.convertedTo(targetCurrency, rate)
          entry.expected match {
            case Expected.Value(expected) =>
              ParityHarness.assertRight(entry.op, converted)(actual =>
                compareAmount(entry.op, actual, expected))
            case Expected.Failed(_) => ParityHarness.assertLeft(entry.op, converted)
          }
        }
      case _ =>
        IO.pure(
          unrecognised(
            s"the CurrencyAmount '${entry.op}' entry",
            "no amount, target currency and rate"))
    }

  /**
   * Measures the factory, where the two normalisations of the type are pinned: a value that is not
   * a number is refused in the error channel, and `-0.0` becomes `+0.0`, which is why the captured
   * bit pattern is compared here and not only the value. Infinities are accepted.
   */
  private def checkAmountFactory(entry: CurrencyAmountEntry): IO[List[String]] =
    entry.input match {
      case Some(input) =>
        currencyOf(input.currency).map { currency =>
          val built = CurrencyAmount.of(currency, input.amount)
          entry.expected match {
            case Expected.Value(expected) =>
              ParityHarness.assertRight(entry.op, built)(actual =>
                compareAmount(entry.op, actual, expected) :::
                  entry.doubleToLongBits.toList
                    .flatMap(bits => compareBits(s"${entry.op}.amount", actual.amount, bits)))
            case Expected.Failed(_) => ParityHarness.assertLeft(entry.op, built)
          }
        }
      case None =>
        IO.pure(unrecognised(s"the CurrencyAmount '${entry.op}' entry", "no construction input"))
    }

  //-------------------------------------------------------------------------
  // The `Money` and `BigMoney` buckets. Every comparison here is exact, and each operand is
  // compared against its own captured rendering as it is rebuilt, which pins the decimal text of
  // every operand of the two seventy-four-currency sweeps - text the input registry does not hold.
  //-------------------------------------------------------------------------

  /** Measures one captured `Money` operation. */
  private def checkMoneyEntry(entry: MoneyEntry, fallback: FxRateProvider): IO[List[String]] =
    entry.op match {
      case "of" => checkMoneyFactory(entry)
      case "plus" => checkMoneyPair(entry, (left, right) => left.plus(right))
      case "minus" => checkMoneyPair(entry, (left, right) => left.minus(right))
      case "multipliedBy" =>
        (entry.left, entry.scalar, entry.expected) match {
          case (Some(leftValue), Some(scalar), Expected.Value(expected)) =>
            moneyOf(leftValue).map(left =>
              compareMoney(s"${entry.op}.left", left, leftValue) :::
                compareMoney(entry.op, left.multipliedBy(scalar), expected))
          case _ =>
            IO.pure(
              unrecognised(s"the Money '${entry.op}' entry", "no value, whole number and expectation"))
        }
      case "convertedTo" => checkMoneyConversion(entry, fallback)
      case "toBigMoney" =>
        // The widening is published under the name the entry records, so that is the method
        // replayed here; `BigMoney.of(money)` is the same conversion named from the companion.
        (entry.left, entry.expected) match {
          case (Some(leftValue), Expected.Value(expected)) =>
            moneyOf(leftValue).map(left =>
              compareMoney(s"${entry.op}.left", left, leftValue) :::
                compareBigMoney(entry.op, left.toBigMoney, expected))
          case _ =>
            IO.pure(unrecognised(s"the Money '${entry.op}' entry", "no value and expectation"))
        }
      case other => IO.pure(unrecognised("a Money entry", s"the operation '$other'"))
    }

  /** Measures `Money.of`, which rounds half up to the currency's minor units. */
  private def checkMoneyFactory(entry: MoneyEntry): IO[List[String]] =
    (entry.currency, entry.amount) match {
      case (Some(code), Some(amount)) =>
        currencyOf(code).map { currency =>
          val built = Money.of(currency, amount)
          checkMinorUnitDigits(entry.op, currency, entry.minorUnitDigits) :::
            (entry.expected match {
              case Expected.Value(expected) =>
                ParityHarness.assertRight(entry.op, built)(actual =>
                  compareMoney(entry.op, actual, expected))
              case Expected.Failed(_) => ParityHarness.assertLeft(entry.op, built)
            })
        }
      case _ =>
        IO.pure(unrecognised(s"the Money '${entry.op}' entry", "no currency and number"))
    }

  /** Measures an addition or a subtraction of two money values, refused across currencies. */
  private def checkMoneyPair(
      entry: MoneyEntry,
      operation: (Money, Money) => Either[Failure, Money]): IO[List[String]] =
    (entry.left, entry.right) match {
      case (Some(leftValue), Some(rightValue)) =>
        for {
          left <- moneyOf(leftValue)
          right <- moneyOf(rightValue)
        } yield {
          val outcome = operation(left, right)
          compareMoney(s"${entry.op}.left", left, leftValue) :::
            compareMoney(s"${entry.op}.right", right, rightValue) :::
            (entry.expected match {
              case Expected.Value(expected) =>
                ParityHarness.assertRight(entry.op, outcome)(actual =>
                  compareMoney(entry.op, actual, expected))
              case Expected.Failed(_) => ParityHarness.assertLeft(entry.op, outcome)
            })
        }
      case _ => IO.pure(unrecognised(s"the Money '${entry.op}' entry", "no pair of values"))
    }

  /** Measures a conversion; an explicit rate goes through the exact-decimal overload. */
  private def checkMoneyConversion(entry: MoneyEntry, fallback: FxRateProvider): IO[List[String]] =
    (entry.left, entry.target) match {
      case (Some(leftValue), Some(target)) =>
        for {
          left <- moneyOf(leftValue)
          targetCurrency <- currencyOf(target)
          converted <- entry.rate match {
            case Some(rate) =>
              ParityHarness
                .raise(Decimal.of(rate))
                .map(decimal => left.convertedTo(targetCurrency, decimal))
            case None =>
              rateProviderFor(entry.rates, fallback)
                .map(provider => left.convertedTo(targetCurrency, provider))
          }
        } yield compareMoney(s"${entry.op}.left", left, leftValue) :::
          (entry.expected match {
            case Expected.Value(expected) =>
              ParityHarness.assertRight(entry.op, converted)(actual =>
                compareMoney(entry.op, actual, expected))
            case Expected.Failed(_) => ParityHarness.assertLeft(entry.op, converted)
          })
      case _ =>
        IO.pure(unrecognised(s"the Money '${entry.op}' entry", "no value and target currency"))
    }

  /** Measures one captured `BigMoney` operation. */
  private def checkBigMoneyEntry(entry: MoneyEntry, fallback: FxRateProvider): IO[List[String]] =
    entry.op match {
      case "of" => checkBigMoneyFactory(entry)
      case "plus" => checkBigMoneyPair(entry, (left, right) => left.plus(right))
      case "minus" => checkBigMoneyPair(entry, (left, right) => left.minus(right))
      case "multipliedBy" =>
        (entry.left, entry.scalar, entry.expected) match {
          case (Some(leftValue), Some(scalar), Expected.Value(expected)) =>
            bigMoneyOf(leftValue).map(left =>
              compareBigMoney(s"${entry.op}.left", left, leftValue) :::
                compareBigMoney(entry.op, left.multipliedBy(scalar), expected))
          case _ =>
            IO.pure(
              unrecognised(
                s"the BigMoney '${entry.op}' entry",
                "no value, whole number and expectation"))
        }
      case "toMoney" =>
        // The narrowing to the currency's minor units, which the seventy-four-currency sweep
        // measures for every currency of the closed family.
        (entry.left, entry.expected) match {
          case (Some(leftValue), Expected.Value(expected)) =>
            bigMoneyOf(leftValue).map(left =>
              compareBigMoney(s"${entry.op}.left", left, leftValue) :::
                compareMoney(entry.op, left.toMoney, expected))
          case _ =>
            IO.pure(unrecognised(s"the BigMoney '${entry.op}' entry", "no value and expectation"))
        }
      case "convertedTo" => checkBigMoneyConversion(entry, fallback)
      case "roundToScale" => checkBigMoneyRounding(entry)
      case other => IO.pure(unrecognised("a BigMoney entry", s"the operation '$other'"))
    }

  /** Measures `BigMoney.of`, which rounds half up to scale twelve rather than to minor units. */
  private def checkBigMoneyFactory(entry: MoneyEntry): IO[List[String]] =
    (entry.currency, entry.amount) match {
      case (Some(code), Some(amount)) =>
        currencyOf(code).map { currency =>
          val built = BigMoney.of(currency, amount)
          checkMinorUnitDigits(entry.op, currency, entry.minorUnitDigits) :::
            (entry.expected match {
              case Expected.Value(expected) =>
                ParityHarness.assertRight(entry.op, built)(actual =>
                  compareBigMoney(entry.op, actual, expected))
              case Expected.Failed(_) => ParityHarness.assertLeft(entry.op, built)
            })
        }
      case _ =>
        IO.pure(unrecognised(s"the BigMoney '${entry.op}' entry", "no currency and number"))
    }

  private def checkBigMoneyPair(
      entry: MoneyEntry,
      operation: (BigMoney, BigMoney) => Either[Failure, BigMoney]): IO[List[String]] =
    (entry.left, entry.right) match {
      case (Some(leftValue), Some(rightValue)) =>
        for {
          left <- bigMoneyOf(leftValue)
          right <- bigMoneyOf(rightValue)
        } yield {
          val outcome = operation(left, right)
          compareBigMoney(s"${entry.op}.left", left, leftValue) :::
            compareBigMoney(s"${entry.op}.right", right, rightValue) :::
            (entry.expected match {
              case Expected.Value(expected) =>
                ParityHarness.assertRight(entry.op, outcome)(actual =>
                  compareBigMoney(entry.op, actual, expected))
              case Expected.Failed(_) => ParityHarness.assertLeft(entry.op, outcome)
            })
        }
      case _ => IO.pure(unrecognised(s"the BigMoney '${entry.op}' entry", "no pair of values"))
    }

  private def checkBigMoneyConversion(entry: MoneyEntry, fallback: FxRateProvider): IO[List[String]] =
    (entry.left, entry.target) match {
      case (Some(leftValue), Some(target)) =>
        for {
          left <- bigMoneyOf(leftValue)
          targetCurrency <- currencyOf(target)
          converted <- entry.rate match {
            case Some(rate) =>
              ParityHarness
                .raise(Decimal.of(rate))
                .map(decimal => left.convertedTo(targetCurrency, decimal))
            case None =>
              rateProviderFor(entry.rates, fallback)
                .map(provider => left.convertedTo(targetCurrency, provider))
          }
        } yield compareBigMoney(s"${entry.op}.left", left, leftValue) :::
          (entry.expected match {
            case Expected.Value(expected) =>
              ParityHarness.assertRight(entry.op, converted)(actual =>
                compareBigMoney(entry.op, actual, expected))
            case Expected.Failed(_) => ParityHarness.assertLeft(entry.op, converted)
          })
      case _ =>
        IO.pure(unrecognised(s"the BigMoney '${entry.op}' entry", "no value and target currency"))
    }

  /** Measures the twelve captured roundings: six modes at scale two, six at negative scales. */
  private def checkBigMoneyRounding(entry: MoneyEntry): IO[List[String]] =
    (entry.left, entry.scale, entry.roundingMode, entry.expected) match {
      case (Some(leftValue), Some(scale), Some(mode), Expected.Value(expected)) =>
        RoundingModesByName.get(mode) match {
          case Some(rounding) =>
            bigMoneyOf(leftValue).map(left =>
              compareBigMoney(s"${entry.op}.left", left, leftValue) :::
                compareBigMoney(entry.op, left.roundToScale(scale, rounding), expected))
          case None =>
            IO.pure(
              unrecognised(s"the BigMoney '${entry.op}' entry", s"the rounding mode '$mode'"))
        }
      case _ =>
        IO.pure(
          unrecognised(
            s"the BigMoney '${entry.op}' entry",
            "no value, scale, rounding mode and expectation"))
    }

  //-------------------------------------------------------------------------
  // The `CurrencyAmountArray` bucket.
  //-------------------------------------------------------------------------

  /** Measures one captured `CurrencyAmountArray` operation. */
  private def checkRunEntry(
      entry: CurrencyAmountArrayEntry,
      fallback: FxRateProvider): IO[List[String]] =
    entry.op match {
      case "plus" =>
        checkRunPair(entry, (run, other) => run.plus(other), (run, amount) => run.plus(amount))
      case "minus" =>
        checkRunPair(entry, (run, other) => run.minus(other), (run, amount) => run.minus(amount))
      case "multipliedBy" =>
        (entry.left, entry.scalar, entry.expected) match {
          case (Some(leftValue), Some(scalar), Expected.Value(ArrayOperand.Run(expected))) =>
            // The expectation is composed from `of(currency, values.multipliedBy(scalar))`, and
            // measuring this module's own member against that composition is the point of it.
            runOf(leftValue).map(left => compareRun(entry.op, left.multipliedBy(scalar), expected))
          case _ =>
            IO.pure(
              unrecognised(
                s"the CurrencyAmountArray '${entry.op}' entry",
                "no array, number and array expectation"))
        }
      case "mapAmounts" =>
        (entry.left, entry.expected) match {
          case (Some(leftValue), Expected.Value(ArrayOperand.Run(expected))) =>
            runOf(leftValue).map(left =>
              checkMapAmountsText(entry.op, entry.mapAmountsFn) :::
                compareRun(entry.op, left.mapAmounts(value => mapAmountsFunction(value)), expected))
          case _ =>
            IO.pure(
              unrecognised(
                s"the CurrencyAmountArray '${entry.op}' entry",
                "no array and array expectation"))
        }
      case "convertedTo" => checkRunConversion(entry, fallback)
      case "of" => checkRunFactory(entry)
      case "get" => checkRunElement(entry)
      case other => IO.pure(unrecognised("a CurrencyAmountArray entry", s"the operation '$other'"))
    }

  /** Measures an element-wise combination; both captured refusals are data-dependent `Left`s. */
  private def checkRunPair(
      entry: CurrencyAmountArrayEntry,
      arrayOperation: (CurrencyAmountArray, CurrencyAmountArray) => Either[Failure, CurrencyAmountArray],
      amountOperation: (CurrencyAmountArray, CurrencyAmount) => Either[Failure, CurrencyAmountArray])
      : IO[List[String]] =
    (entry.left, entry.right) match {
      case (Some(leftValue), Some(right)) =>
        for {
          left <- runOf(leftValue)
          outcome <- right match {
            case ArrayOperand.Run(value) => runOf(value).map(other => arrayOperation(left, other))
            case ArrayOperand.Single(value) =>
              amountOf(value).map(amount => amountOperation(left, amount))
          }
        } yield expectRun(entry.op, outcome, entry.expected)
      case _ =>
        IO.pure(
          unrecognised(s"the CurrencyAmountArray '${entry.op}' entry", "no array and operand"))
    }

  /** Measures a conversion of a whole array, at the rate or the rate list the entry carries. */
  private def checkRunConversion(
      entry: CurrencyAmountArrayEntry,
      fallback: FxRateProvider): IO[List[String]] =
    (entry.left, entry.target) match {
      case (Some(leftValue), Some(target)) =>
        for {
          left <- runOf(leftValue)
          targetCurrency <- currencyOf(target)
          provider <- runProviderFor(entry, leftValue.currency, target, fallback)
        } yield {
          val converted = left.convertedTo(targetCurrency, provider)
          entry.expected match {
            case Expected.Value(ArrayOperand.Run(expected)) =>
              ParityHarness.assertRight(entry.op, converted)(actual =>
                compareRun(entry.op, actual, expected))
            case Expected.Value(ArrayOperand.Single(expected)) =>
              unrecognised(
                s"the CurrencyAmountArray '${entry.op}' entry",
                s"the single-amount expectation $expected where an array is produced")
            case Expected.Failed(_) =>
              assertLeftFor(entry.op, converted, FailureReason.CURRENCY_CONVERSION)
          }
        }
      case _ =>
        IO.pure(
          unrecognised(
            s"the CurrencyAmountArray '${entry.op}' entry",
            "no array and target currency"))
    }

  private def runProviderFor(
      entry: CurrencyAmountArrayEntry,
      base: String,
      counter: String,
      fallback: FxRateProvider): IO[FxRateProvider] =
    entry.rate match {
      case Some(rate) =>
        for {
          baseCurrency <- currencyOf(base)
          counterCurrency <- currencyOf(counter)
          built <- ParityHarness.raiseNec(FxRate.of(baseCurrency, counterCurrency, rate))
        } yield built
      case None => rateProviderFor(entry.rates, fallback)
    }

  /**
   * Measures the three factories of the type, told apart by the keys of the entry: a `size` marks
   * `of(size, valueFunction)`, a bit pattern marks `of(currency, DoubleArray)` - the only one of
   * the three that '''keeps''' the sign of a zero, the other two building their elements through
   * `CurrencyAmount.of`, which normalises it away - and everything else is
   * `of(Iterable[CurrencyAmount])`, whose mixed-currency refusal is one of the entries here.
   */
  private def checkRunFactory(entry: CurrencyAmountArrayEntry): IO[List[String]] =
    (entry.input, entry.size, entry.doubleToLongBits) match {
      case (Some(input), Some(size), _) =>
        amountsOf(input, entry.currency).map { amounts =>
          if (amounts.size == size) {
            expectRun(entry.op, CurrencyAmountArray.of(size, index => amounts(index)), entry.expected)
          } else {
            unrecognised(
              s"the CurrencyAmountArray '${entry.op}' entry",
              s"a size of $size with ${amounts.size} described amounts")
          }
        }
      case (Some(ArrayInput.Values(values)), None, Some(bits)) =>
        entry.currency match {
          case Some(code) =>
            currencyOf(code).map { currency =>
              val built = CurrencyAmountArray.of(currency, DoubleArray.copyOf(values))
              expectRunValue(entry.op, built, entry.expected) :::
                compareBits(s"${entry.op}.values[0]", built.values.get(0), bits)
            }
          case None =>
            IO.pure(
              unrecognised(s"the CurrencyAmountArray '${entry.op}' entry", "values but no currency"))
        }
      case (Some(input), None, _) =>
        amountsOf(input, entry.currency)
          .map(amounts => expectRun(entry.op, CurrencyAmountArray.of(amounts), entry.expected))
      case _ =>
        IO.pure(
          unrecognised(s"the CurrencyAmountArray '${entry.op}' entry", "no construction input"))
    }

  /** Measures reading one element back out, which normalises the sign of a zero again. */
  private def checkRunElement(entry: CurrencyAmountArrayEntry): IO[List[String]] =
    (entry.left, entry.index, entry.expected) match {
      case (Some(leftValue), Some(index), Expected.Value(ArrayOperand.Single(expected))) =>
        runOf(leftValue).map { left =>
          val amount = left.get(index)
          compareAmount(entry.op, amount, expected) :::
            entry.doubleToLongBits.toList
              .flatMap(bits => compareBits(s"${entry.op}.amount", amount.amount, bits))
        }
      case _ =>
        IO.pure(
          unrecognised(
            s"the CurrencyAmountArray '${entry.op}' entry",
            "no array, index and amount expectation"))
    }

  /** Compares an array outcome that may have been refused. */
  private def expectRun(
      label: String,
      outcome: Either[_, CurrencyAmountArray],
      expected: Expected[ArrayOperand]): List[String] =
    expected match {
      case Expected.Value(ArrayOperand.Run(want)) =>
        ParityHarness.assertRight(label, outcome)(actual => compareRun(label, actual, want))
      case Expected.Value(ArrayOperand.Single(want)) =>
        unrecognised(label, s"the single-amount expectation $want where an array is produced")
      case Expected.Failed(_) => ParityHarness.assertLeft(label, outcome)
    }

  /** Compares an array a total factory produced, for which a captured refusal is a disagreement. */
  private def expectRunValue(
      label: String,
      actual: CurrencyAmountArray,
      expected: Expected[ArrayOperand]): List[String] =
    expected match {
      case Expected.Value(ArrayOperand.Run(want)) => compareRun(label, actual, want)
      case Expected.Value(ArrayOperand.Single(want)) =>
        unrecognised(label, s"the single-amount expectation $want where an array is produced")
      case Expected.Failed(message) =>
        List(
          s"$label: the fixture records the refusal '$message', but the factory replayed here is " +
            s"total and produced $actual")
    }

  //-------------------------------------------------------------------------
  // The `MultiCurrencyAmount` bucket.
  //-------------------------------------------------------------------------

  /** Measures one captured `MultiCurrencyAmount` operation. */
  private def checkMultiEntry(
      entry: MultiCurrencyAmountEntry,
      fallback: FxRateProvider): IO[List[String]] =
    entry.op match {
      case "of" =>
        // `of` rejects a repeated currency where `total` merges it, and the fixture holds both over
        // the same input, so the two are never substituted for one another here.
        multiAmountsOf(entry)
          .map(amounts => expectMulti(entry.op, MultiCurrencyAmount.of(amounts), entry.expected))
      case "total" =>
        multiAmountsOf(entry)
          .map(amounts => expectMultiValue(entry.op, MultiCurrencyAmount.total(amounts), entry.expected))
      case "plus" => checkMultiPair(entry, (left, right) => left.plus(right))
      case "minus" => checkMultiPair(entry, (left, right) => left.minus(right))
      case "multipliedBy" =>
        (entry.left, entry.scalar) match {
          case (Some(leftValue), Some(scalar)) =>
            multiOf(leftValue)
              .map(left => expectMultiValue(entry.op, left.multipliedBy(scalar), entry.expected))
          case _ =>
            IO.pure(
              unrecognised(s"the MultiCurrencyAmount '${entry.op}' entry", "no value and number"))
        }
      case "mapAmounts" =>
        entry.left match {
          case Some(leftValue) =>
            multiOf(leftValue).map(left =>
              checkMapAmountsText(entry.op, entry.mapAmountsFn) :::
                expectMultiValue(
                  entry.op,
                  left.mapAmounts(value => mapAmountsFunction(value)),
                  entry.expected))
          case None =>
            IO.pure(unrecognised(s"the MultiCurrencyAmount '${entry.op}' entry", "no value"))
        }
      case "getAmount" => checkMultiAmountLookup(entry)
      case "convertedTo" => checkMultiConversion(entry, fallback)
      case other => IO.pure(unrecognised("a MultiCurrencyAmount entry", s"the operation '$other'"))
    }

  /** The amounts a factory entry names, either as captured values or as a description. */
  private def multiAmountsOf(entry: MultiCurrencyAmountEntry): IO[List[CurrencyAmount]] =
    (entry.amounts, entry.input) match {
      case (Some(amounts), None) => amounts.toList.traverse(amountOf)
      case (None, Some(text)) => parseAmounts(text)
      case _ =>
        IO.raiseError(
          new IllegalStateException(
            s"fixture disagreement: the MultiCurrencyAmount '${entry.op}' entry names its " +
              "amounts neither as a list nor as a description"))
    }

  /** Measures an addition or a subtraction of two multi-currency amounts, both of which are total. */
  private def checkMultiPair(
      entry: MultiCurrencyAmountEntry,
      operation: (MultiCurrencyAmount, MultiCurrencyAmount) => MultiCurrencyAmount): IO[List[String]] =
    (entry.left, entry.right) match {
      case (Some(leftValue), Some(rightValue)) =>
        for {
          left <- multiOf(leftValue)
          right <- multiOf(rightValue)
        } yield expectMultiValue(entry.op, operation(left, right), entry.expected)
      case _ =>
        IO.pure(unrecognised(s"the MultiCurrencyAmount '${entry.op}' entry", "no pair of values"))
    }

  /** Measures reading the amount of one currency, which is refused for a currency not held. */
  private def checkMultiAmountLookup(entry: MultiCurrencyAmountEntry): IO[List[String]] =
    (entry.left, entry.currency) match {
      case (Some(leftValue), Some(code)) =>
        for {
          left <- multiOf(leftValue)
          currency <- currencyOf(code)
        } yield {
          val outcome = left.getAmount(currency)
          entry.expected match {
            case Expected.Value(MultiOperand.Single(expected)) =>
              ParityHarness.assertRight(entry.op, outcome)(actual =>
                compareAmount(entry.op, actual, expected))
            case Expected.Value(MultiOperand.Amounts(expected)) =>
              unrecognised(
                s"the MultiCurrencyAmount '${entry.op}' entry",
                s"the multi-amount expectation $expected where one amount is produced")
            case Expected.Failed(_) => ParityHarness.assertLeft(entry.op, outcome)
          }
        }
      case _ =>
        IO.pure(unrecognised(s"the MultiCurrencyAmount '${entry.op}' entry", "no value and currency"))
    }

  /** Measures converting every amount into one currency and totalling them. */
  private def checkMultiConversion(
      entry: MultiCurrencyAmountEntry,
      fallback: FxRateProvider): IO[List[String]] =
    (entry.left, entry.target) match {
      case (Some(leftValue), Some(target)) =>
        for {
          left <- multiOf(leftValue)
          targetCurrency <- currencyOf(target)
          provider <- rateProviderFor(entry.rates, fallback)
        } yield {
          val converted = left.convertedTo(targetCurrency, provider)
          entry.expected match {
            case Expected.Value(MultiOperand.Single(expected)) =>
              ParityHarness.assertRight(entry.op, converted)(actual =>
                compareAmount(entry.op, actual, expected))
            case Expected.Value(MultiOperand.Amounts(expected)) =>
              unrecognised(
                s"the MultiCurrencyAmount '${entry.op}' entry",
                s"the multi-amount expectation $expected where one amount is produced")
            case Expected.Failed(_) =>
              assertLeftFor(entry.op, converted, FailureReason.CURRENCY_CONVERSION)
          }
        }
      case _ =>
        IO.pure(
          unrecognised(
            s"the MultiCurrencyAmount '${entry.op}' entry",
            "no value and target currency"))
    }

  /** Compares a multi-currency outcome that may have been refused. */
  private def expectMulti(
      label: String,
      outcome: Either[_, MultiCurrencyAmount],
      expected: Expected[MultiOperand]): List[String] =
    expected match {
      case Expected.Value(MultiOperand.Amounts(want)) =>
        ParityHarness.assertRight(label, outcome)(actual => compareMulti(label, actual, want))
      case Expected.Value(MultiOperand.Single(want)) =>
        unrecognised(label, s"the single-amount expectation $want where a multi-amount is produced")
      case Expected.Failed(_) => ParityHarness.assertLeft(label, outcome)
    }

  /** Compares a multi-currency value a total operation produced. */
  private def expectMultiValue(
      label: String,
      actual: MultiCurrencyAmount,
      expected: Expected[MultiOperand]): List[String] =
    expected match {
      case Expected.Value(MultiOperand.Amounts(want)) => compareMulti(label, actual, want)
      case Expected.Value(MultiOperand.Single(want)) =>
        unrecognised(label, s"the single-amount expectation $want where a multi-amount is produced")
      case Expected.Failed(message) =>
        List(
          s"$label: the fixture records the refusal '$message', but the operation replayed here " +
            s"is total and produced $actual")
    }

  //-------------------------------------------------------------------------
  // The `MultiCurrencyAmountArray` bucket.
  //-------------------------------------------------------------------------

  /** Measures one captured `MultiCurrencyAmountArray` operation. */
  private def checkMultiRunEntry(
      entry: MultiCurrencyAmountArrayEntry,
      fallback: FxRateProvider): IO[List[String]] =
    entry.op match {
      case "of" => checkMultiRunFactory(entry)
      case "getValues" => checkMultiRunValues(entry)
      case "plus" =>
        checkMultiRunPair(entry, (run, other) => run.plus(other), (run, amount) => run.plus(amount))
      case "minus" =>
        checkMultiRunPair(entry, (run, other) => run.minus(other), (run, amount) => run.minus(amount))
      case "total" =>
        entry.input match {
          case Some(text) =>
            parseRuns(text).map { runs =>
              val arrays = runs.map { case (currency, values) =>
                CurrencyAmountArray.of(currency, values)
              }
              expectMultiRun(entry.op, MultiCurrencyAmountArray.total(arrays), entry.expected)
            }
          case None =>
            IO.pure(
              unrecognised(s"the MultiCurrencyAmountArray '${entry.op}' entry", "no described runs"))
        }
      case "convertedTo" => checkMultiRunConversion(entry, fallback)
      case "multipliedBy" =>
        (entry.left, entry.scalar) match {
          case (Some(leftValue), Some(scalar)) =>
            // The expectation is composed from the same scaling applied over every currency.
            multiRunOf(leftValue)
              .map(left => expectMultiRunValue(entry.op, left.multipliedBy(scalar), entry.expected))
          case _ =>
            IO.pure(
              unrecognised(
                s"the MultiCurrencyAmountArray '${entry.op}' entry",
                "no run and number"))
        }
      case "mapAmounts" =>
        entry.left match {
          case Some(leftValue) =>
            multiRunOf(leftValue).map(left =>
              checkMapAmountsText(entry.op, entry.mapAmountsFn) :::
                expectMultiRunValue(
                  entry.op,
                  left.mapAmounts(value => mapAmountsFunction(value)),
                  entry.expected))
          case None =>
            IO.pure(unrecognised(s"the MultiCurrencyAmountArray '${entry.op}' entry", "no run"))
        }
      case other =>
        IO.pure(unrecognised("a MultiCurrencyAmountArray entry", s"the operation '$other'"))
    }

  /**
   * Measures the factories of the type, told apart by what the entry names. The map factory is the
   * one that '''rejects''' arrays of unequal length, reached from a captured run or from a
   * description naming whole runs per currency; the list and function factories take multi-currency
   * amounts, where a currency missing from one element is zero-filled and one absent from every
   * element stays unknown, and a `size` tells the function form from the list form.
   */
  private def checkMultiRunFactory(entry: MultiCurrencyAmountArrayEntry): IO[List[String]] =
    (entry.left, entry.input) match {
      case (Some(leftValue), None) =>
        runEntriesOf(leftValue)
          .map(values => expectMultiRun(entry.op, MultiCurrencyAmountArray.of(values), entry.expected))
      case (None, Some(text)) if describesRuns(text) =>
        parseRuns(text).map(runs =>
          expectMultiRun(entry.op, MultiCurrencyAmountArray.of(runs.toMap), entry.expected))
      case (None, Some(text)) =>
        parseMultiAmounts(text).map { amounts =>
          entry.size match {
            case Some(size) if size == amounts.size =>
              expectMultiRunValue(
                entry.op,
                MultiCurrencyAmountArray.of(size, index => amounts(index)),
                entry.expected)
            case Some(size) =>
              unrecognised(
                s"the MultiCurrencyAmountArray '${entry.op}' entry",
                s"a size of $size with ${amounts.size} described amounts")
            case None =>
              expectMultiRunValue(entry.op, MultiCurrencyAmountArray.of(amounts), entry.expected)
          }
        }
      case _ =>
        IO.pure(
          unrecognised(
            s"the MultiCurrencyAmountArray '${entry.op}' entry",
            "no construction input"))
    }

  /** Measures reading the values of one currency back out, refused for a currency not held. */
  private def checkMultiRunValues(entry: MultiCurrencyAmountArrayEntry): IO[List[String]] =
    (entry.left, entry.currency) match {
      case (Some(leftValue), Some(code)) =>
        for {
          left <- multiRunOf(leftValue)
          currency <- currencyOf(code)
        } yield {
          val outcome = left.getValues(currency)
          entry.expected match {
            case Expected.Value(MultiArrayResult.Values(expected)) =>
              ParityHarness.assertRight(entry.op, outcome)(actual =>
                ParityHarness.assertParitySeq(entry.op, actual.toList, expected))
            case Expected.Value(other) =>
              unrecognised(
                s"the MultiCurrencyAmountArray '${entry.op}' entry",
                s"the expectation $other where a list of values is produced")
            case Expected.Failed(_) => ParityHarness.assertLeft(entry.op, outcome)
          }
        }
      case _ =>
        IO.pure(
          unrecognised(s"the MultiCurrencyAmountArray '${entry.op}' entry", "no run and currency"))
    }

  /** Measures an element-wise combination, against another run or against one amount. */
  private def checkMultiRunPair(
      entry: MultiCurrencyAmountArrayEntry,
      runOperation: (MultiCurrencyAmountArray, MultiCurrencyAmountArray) => Either[Failure, MultiCurrencyAmountArray],
      amountOperation: (MultiCurrencyAmountArray, MultiCurrencyAmount) => Either[Failure, MultiCurrencyAmountArray])
      : IO[List[String]] =
    (entry.left, entry.right) match {
      case (Some(leftValue), Some(right)) =>
        for {
          left <- multiRunOf(leftValue)
          outcome <- right match {
            case MultiArrayOperand.Run(value) =>
              multiRunOf(value).map(other => runOperation(left, other))
            case MultiArrayOperand.Amounts(values) =>
              multiOf(values).map(amount => amountOperation(left, amount))
          }
        } yield expectMultiRun(entry.op, outcome, entry.expected)
      case _ =>
        IO.pure(
          unrecognised(s"the MultiCurrencyAmountArray '${entry.op}' entry", "no run and operand"))
    }

  /** Measures converting a whole run into one currency, at the entry's rates or the registry's. */
  private def checkMultiRunConversion(
      entry: MultiCurrencyAmountArrayEntry,
      fallback: FxRateProvider): IO[List[String]] =
    (entry.left, entry.target) match {
      case (Some(leftValue), Some(target)) =>
        for {
          left <- multiRunOf(leftValue)
          targetCurrency <- currencyOf(target)
          provider <- rateProviderFor(entry.rates, fallback)
        } yield {
          val converted = left.convertedTo(targetCurrency, provider)
          entry.expected match {
            case Expected.Value(MultiArrayResult.Converted(expected)) =>
              ParityHarness.assertRight(entry.op, converted)(actual =>
                compareRun(entry.op, actual, expected))
            case Expected.Value(other) =>
              unrecognised(
                s"the MultiCurrencyAmountArray '${entry.op}' entry",
                s"the expectation $other where a single-currency array is produced")
            case Expected.Failed(_) =>
              assertLeftFor(entry.op, converted, FailureReason.CURRENCY_CONVERSION)
          }
        }
      case _ =>
        IO.pure(
          unrecognised(
            s"the MultiCurrencyAmountArray '${entry.op}' entry",
            "no run and target currency"))
    }

  /** Compares a run outcome that may have been refused. */
  private def expectMultiRun(
      label: String,
      outcome: Either[_, MultiCurrencyAmountArray],
      expected: Expected[MultiArrayResult]): List[String] =
    expected match {
      case Expected.Value(MultiArrayResult.Run(want)) =>
        ParityHarness.assertRight(label, outcome)(actual => compareMultiRun(label, actual, want))
      case Expected.Value(other) =>
        unrecognised(label, s"the expectation $other where a multi-currency run is produced")
      case Expected.Failed(_) => ParityHarness.assertLeft(label, outcome)
    }

  /** Compares a run a total operation produced. */
  private def expectMultiRunValue(
      label: String,
      actual: MultiCurrencyAmountArray,
      expected: Expected[MultiArrayResult]): List[String] =
    expected match {
      case Expected.Value(MultiArrayResult.Run(want)) => compareMultiRun(label, actual, want)
      case Expected.Value(other) =>
        unrecognised(label, s"the expectation $other where a multi-currency run is produced")
      case Expected.Failed(message) =>
        List(
          s"$label: the fixture records the refusal '$message', but the operation replayed here " +
            s"is total and produced $actual")
    }

  //-------------------------------------------------------------------------
  // Measuring one row.
  //-------------------------------------------------------------------------

  /**
   * Measures one row of the fixture, answering with everything that differed.
   *
   * Every bucket is measured, not only the one the row's family populates: five of the six are
   * empty on every row, and dispatching on the row's identity instead would leave an entry that
   * appeared in the wrong bucket unmeasured.
   *
   * @param fallback  the provider for an entry that names no rate of its own
   * @param row  the row to measure
   * @return every discrepancy found in the row, empty where it matched in every respect
   */
  def checkRow(fallback: FxRateProvider, row: CurrencyMathRow): IO[List[String]] =
    for {
      registry <- checkRegistry(row)
      amounts <- checkEntries("currencyAmountResults", row.currencyAmountResults)(checkAmountEntry)
      money <- checkEntries("moneyResults", row.moneyResults)(entry =>
        checkMoneyEntry(entry, fallback))
      bigMoney <- checkEntries("bigMoneyResults", row.bigMoneyResults)(entry =>
        checkBigMoneyEntry(entry, fallback))
      runs <- checkEntries("currencyAmountArrayResults", row.currencyAmountArrayResults)(entry =>
        checkRunEntry(entry, fallback))
      multi <- checkEntries("multiCurrencyAmountResults", row.multiCurrencyAmountResults)(entry =>
        checkMultiEntry(entry, fallback))
      multiRuns <- checkEntries(
        "multiCurrencyAmountArrayResults",
        row.multiCurrencyAmountArrayResults)(entry => checkMultiRunEntry(entry, fallback))
    } yield checkShape(row) ::: registry ::: amounts ::: money ::: bigMoney ::: runs ::: multi :::
      multiRuns

  /** Measures every entry of one bucket, labelling each message with the bucket and the index. */
  private def checkEntries[E](bucket: String, entries: Vector[E])(
      check: E => IO[List[String]]): IO[List[String]] =
    entries.zipWithIndex.toList.flatTraverse { case (entry, index) =>
      check(entry).attempt.map {
        case Right(messages) => messages.map(message => s"$bucket[$index] $message")
        case Left(error) => List(s"$bucket[$index]: the entry could not be replayed: $error")
      }
    }

  /** Checks the row's own invariants: a known identity, and its own family's bucket alone. */
  private def checkShape(row: CurrencyMathRow): List[String] = {
    val known =
      if (RowIds.contains(row.id)) {
        Nil
      } else {
        List(
          s"fixture disagreement: '${row.id}' is not one of the six row identities this spec " +
            s"reads (${RowIds.mkString(", ")})")
      }
    val populated = populatedBuckets(row)
    val single =
      if (populated == Vector(bucketOf(row.id))) {
        Nil
      } else {
        val held = if (populated.isEmpty) "no expectation bucket" else populated.mkString(", ")
        List(
          s"fixture disagreement: the row populates $held, and a row carries the bucket of its " +
            s"own family ('${bucketOf(row.id)}') alone")
      }
    known ::: single
  }

  /**
   * Checks that every input the row registers can still be built, and still renders as it did.
   *
   * The registry is not a positional pairing and cannot be measured as one, but it is not inert
   * either: rebuilding each registered value and comparing it against its own captured form catches
   * a mistranscribed input, a currency the closed family no longer holds, and a change in the way a
   * value renders - the last of which is what a money amount's decimal text is.
   */
  def checkRegistry(row: CurrencyMathRow): IO[List[String]] =
    for {
      amounts <- row.amounts.toList.zipWithIndex.flatTraverse { case (value, index) =>
        amountOf(value).map(amount => compareAmount(s"amounts[$index]", amount, value))
      }
      arrays <- row.arrays.toList.zipWithIndex.flatTraverse { case (value, index) =>
        runOf(value).map(run => compareRun(s"arrays[$index]", run, value))
      }
      multiArrays <- row.multiArrays.toList.zipWithIndex.flatTraverse { case (value, index) =>
        // The map factory takes the size of the run from the array of the first currency in code
        // order, so that is the size the rebuilt run is required to derive.
        val expected =
          MultiArrayValue(value.arrays.sortBy(_.currency).headOption.fold(0)(_.values.size), value.arrays)
        multiRunOf(expected).map(run => compareMultiRun(s"multiArrays[$index]", run, expected))
      }
      money <- row.money.toList.zipWithIndex.flatTraverse { case (value, index) =>
        moneyOf(value).map(built => compareMoney(s"money[$index]", built, value))
      }
      bigMoney <- row.bigMoney.toList.zipWithIndex.flatTraverse { case (value, index) =>
        bigMoneyOf(value).map(built => compareBigMoney(s"bigMoney[$index]", built, value))
      }
      rates <- row.rates.toList.zipWithIndex.flatTraverse { case (value, index) =>
        ParityHarness
          .raise(CurrencyPair.parse(value.pair))
          .flatMap(pair => ParityHarness.raiseNec(FxRate.of(pair, value.rate)))
          .map(rate =>
            ParityHarness.assertExact(s"rates[$index].pair", rate.pair.toString, value.pair) :::
              ParityHarness.assertParity(s"rates[$index].rate", rate.rate, value.rate))
      }
    } yield amounts ::: arrays ::: multiArrays ::: money ::: bigMoney ::: rates

  //-------------------------------------------------------------------------
  // The population of the document, which the second test of the suite asserts: a check of
  // `failed == 0` cannot tell a complete measurement from a thinned one, so these read the
  // coverage of the fixture rather than the behaviour of the code.
  //-------------------------------------------------------------------------

  /** The bucket the row of the given identity carries. */
  def bucketOf(id: String): String =
    id match {
      case "currency-amount" => "currencyAmountResults"
      case "money" => "moneyResults"
      case "big-money" => "bigMoneyResults"
      case "currency-amount-array" => "currencyAmountArrayResults"
      case "multi-currency-amount" => "multiCurrencyAmountResults"
      case "multi-currency-amount-array" => "multiCurrencyAmountArrayResults"
      case other => s"<no bucket is defined for the row identity '$other'>"
    }

  /** The buckets the row holds entries in, in the order the row declares them. */
  def populatedBuckets(row: CurrencyMathRow): Vector[String] =
    Vector(
      ("currencyAmountResults", row.currencyAmountResults.size),
      ("moneyResults", row.moneyResults.size),
      ("bigMoneyResults", row.bigMoneyResults.size),
      ("currencyAmountArrayResults", row.currencyAmountArrayResults.size),
      ("multiCurrencyAmountResults", row.multiCurrencyAmountResults.size),
      ("multiCurrencyAmountArrayResults", row.multiCurrencyAmountArrayResults.size))
      .collect { case (bucket, count) if count > 0 => bucket }

  /** The number of expectation entries the row holds, over all six buckets. */
  def entryCountOf(row: CurrencyMathRow): Int =
    row.currencyAmountResults.size + row.moneyResults.size + row.bigMoneyResults.size +
      row.currencyAmountArrayResults.size + row.multiCurrencyAmountResults.size +
      row.multiCurrencyAmountArrayResults.size

  /** The number of entries of the row that record a refusal rather than a value. */
  def errorCountOf(row: CurrencyMathRow): Int =
    row.currencyAmountResults.count(entry => isRefusal(entry.expected)) +
      row.moneyResults.count(entry => isRefusal(entry.expected)) +
      row.bigMoneyResults.count(entry => isRefusal(entry.expected)) +
      row.currencyAmountArrayResults.count(entry => isRefusal(entry.expected)) +
      row.multiCurrencyAmountResults.count(entry => isRefusal(entry.expected)) +
      row.multiCurrencyAmountArrayResults.count(entry => isRefusal(entry.expected))

  private def isRefusal(expected: Expected[Any]): Boolean =
    expected match {
      case Expected.Failed(_) => true
      case Expected.Value(_) => false
    }

  /** The distinct minor-unit scales the document exercises. */
  def minorUnitDigitsCovered(rows: Vector[CurrencyMathRow]): Set[Int] =
    (rows.flatMap(_.moneyResults).flatMap(_.minorUnitDigits) ++
      rows.flatMap(_.bigMoneyResults).flatMap(_.minorUnitDigits) ++
      rows.flatMap(_.currencyAmountResults).flatMap(_.minorUnitDigits)).toSet

  /** The currencies one money bucket names, as a factory's currency or as an operand's. */
  def sweptCurrencies(
      rows: Vector[CurrencyMathRow],
      bucket: CurrencyMathRow => Vector[MoneyEntry]): Set[String] =
    rows
      .flatMap(bucket)
      .flatMap(entry => entry.currency.orElse(entry.left.map(value => value.currency)))
      .toSet

  /** The number of entries that pin the sign of a zero through a captured bit pattern. */
  def signedZeroEntries(rows: Vector[CurrencyMathRow]): Int =
    rows.flatMap(_.currencyAmountResults).count(entry => entry.doubleToLongBits.isDefined) +
      rows.flatMap(_.currencyAmountArrayResults).count(entry => entry.doubleToLongBits.isDefined)

  //-------------------------------------------------------------------------
  // The two schema rules, which the last two tests of the suite assert: a union is exactly one
  // captured shape, and a description is accounted for in full. Both exist to stop a malformed
  // fixture from being measured as a smaller but valid one, and neither is observable from the
  // committed document, which satisfies both - so both are measured over crafted values, and
  // nothing below reads a fixture or performs any I/O.
  //-------------------------------------------------------------------------

  /** The discrepancy of a decoding the union schema must refuse. */
  private def refusesShape[A](label: String, outcome: Decoder.Result[A]): List[String] =
    ParityHarness.assertLeft(s"$label, which the union schema must refuse", outcome)

  /** The discrepancy of a refusal whose message must quote the keys the document carried. */
  private def refusesNaming[A](
      label: String,
      outcome: Decoder.Result[A],
      quoted: String): List[String] =
    outcome match {
      case Left(failure) if failure.message.contains(quoted) => Nil
      case Left(failure) =>
        List(s"$label: the refusal must quote '$quoted', and it reads '${failure.message}'")
      case Right(value) =>
        List(s"$label: the union schema must refuse it, and it decoded as $value")
    }

  /** The discrepancy of a decoding the union schema must accept as one named alternative. */
  private def decodesAs[A](label: String, outcome: Decoder.Result[A], expected: A): List[String] =
    ParityHarness.assertExact[Decoder.Result[A]](label, outcome, Right(expected))

  /** Measures the five union decoders against the documented shapes and the malformations. */
  def unionDecoderDiscrepancies: List[String] = {
    val arrayValue = ArrayValue("GBP", Vector(1.0, 2.0))
    val amountValue = AmountValue("GBP", 4.0)
    val multiArrayValue = MultiArrayValue(2, Vector(arrayValue))
    val arrayJson =
      Json.obj(
        "currency" -> Json.fromString("GBP"),
        "values" -> Json.arr(Json.fromDoubleOrNull(1.0), Json.fromDoubleOrNull(2.0)))
    val amountJson =
      Json.obj("currency" -> Json.fromString("GBP"), "amount" -> Json.fromDoubleOrNull(4.0))
    val moneyJson = amountJson.deepMerge(Json.obj("toString" -> Json.fromString("GBP 4")))
    val runJson = Json.obj("size" -> Json.fromInt(2), "arrays" -> Json.arr(arrayJson))
    val valuesJson = Json.arr(Json.fromDoubleOrNull(1.0), Json.fromDoubleOrNull(2.0))
    val amountsJson = Json.arr(amountJson)
    val textJson = Json.fromString("GBP 4, USD 5")
    val arrayAndAmountJson = arrayJson.deepMerge(amountJson)
    val runAndArrayJson = runJson.deepMerge(arrayJson)
    val arrayWithUnknownKeyJson = arrayJson.deepMerge(Json.obj("scale" -> Json.fromInt(2)))
    val incompleteArrayJson = Json.obj("currency" -> Json.fromString("GBP"))
    // An amount-array operand: an array of amounts or one amount, and nothing else.
    decodesAs(
      "an array in an amount-array operand position",
      arrayOperandDecoder.decodeJson(arrayJson),
      ArrayOperand.Run(arrayValue)) :::
      decodesAs(
        "an amount in an amount-array operand position",
        arrayOperandDecoder.decodeJson(amountJson),
        ArrayOperand.Single(amountValue)) :::
      refusesNaming(
        "an operand carrying the keys of both alternatives",
        arrayOperandDecoder.decodeJson(arrayAndAmountJson),
        "{amount, currency, values}") :::
      refusesShape(
        "an operand carrying an unknown key",
        arrayOperandDecoder.decodeJson(arrayWithUnknownKeyJson)) :::
      refusesShape(
        "an operand missing a key of every alternative",
        arrayOperandDecoder.decodeJson(incompleteArrayJson)) :::
      refusesShape(
        "a money value in an amount-array operand position",
        arrayOperandDecoder.decodeJson(moneyJson)) :::
      refusesShape(
        "a JSON array in an amount-array operand position",
        arrayOperandDecoder.decodeJson(valuesJson)) :::
      // An amount-array construction input: a list of values or a description, told apart by shape.
      decodesAs(
        "a list of values as a construction input",
        arrayInputDecoder.decodeJson(valuesJson),
        ArrayInput.Values(Vector(1.0, 2.0))) :::
      decodesAs(
        "a description as a construction input",
        arrayInputDecoder.decodeJson(textJson),
        ArrayInput.Described("GBP 4, USD 5")) :::
      refusesShape(
        "an object as a construction input",
        arrayInputDecoder.decodeJson(arrayJson)) :::
      // A MultiCurrencyAmount expectation: every amount, or one of them.
      decodesAs(
        "a list of amounts as a MultiCurrencyAmount expectation",
        multiOperandDecoder.decodeJson(amountsJson),
        MultiOperand.Amounts(Vector(amountValue))) :::
      decodesAs(
        "one amount as a MultiCurrencyAmount expectation",
        multiOperandDecoder.decodeJson(amountJson),
        MultiOperand.Single(amountValue)) :::
      refusesShape(
        "a money value as a MultiCurrencyAmount expectation",
        multiOperandDecoder.decodeJson(moneyJson)) :::
      refusesShape(
        "an object carrying the keys of two alternatives as a MultiCurrencyAmount expectation",
        multiOperandDecoder.decodeJson(arrayAndAmountJson)) :::
      // A MultiCurrencyAmountArray operand: another run, or the amounts of one index.
      decodesAs(
        "a run as a MultiCurrencyAmountArray operand",
        multiArrayOperandDecoder.decodeJson(runJson),
        MultiArrayOperand.Run(multiArrayValue)) :::
      decodesAs(
        "a list of amounts as a MultiCurrencyAmountArray operand",
        multiArrayOperandDecoder.decodeJson(amountsJson),
        MultiArrayOperand.Amounts(Vector(amountValue))) :::
      refusesShape(
        "a run carrying the keys of an array as well",
        multiArrayOperandDecoder.decodeJson(runAndArrayJson)) :::
      refusesShape(
        "a number as a MultiCurrencyAmountArray operand",
        multiArrayOperandDecoder.decodeJson(Json.fromInt(4))) :::
      // A MultiCurrencyAmountArray expectation: a run, a converted array, or bare values.
      decodesAs(
        "a run as a MultiCurrencyAmountArray expectation",
        multiArrayResultDecoder.decodeJson(runJson),
        MultiArrayResult.Run(multiArrayValue)) :::
      decodesAs(
        "a converted array as a MultiCurrencyAmountArray expectation",
        multiArrayResultDecoder.decodeJson(arrayJson),
        MultiArrayResult.Converted(arrayValue)) :::
      decodesAs(
        "a list of values as a MultiCurrencyAmountArray expectation",
        multiArrayResultDecoder.decodeJson(valuesJson),
        MultiArrayResult.Values(Vector(1.0, 2.0))) :::
      refusesNaming(
        "an expectation carrying the keys of the run and the array alternatives",
        multiArrayResultDecoder.decodeJson(runAndArrayJson),
        "{arrays, currency, size, values}") :::
      refusesShape(
        "an amount as a MultiCurrencyAmountArray expectation",
        multiArrayResultDecoder.decodeJson(amountJson)) :::
      refusesShape(
        "a null as a MultiCurrencyAmountArray expectation",
        multiArrayResultDecoder.decodeJson(Json.Null))
  }

  /** The currency and value of every parsed amount, which is what an accepted form is compared as. */
  private def amountPairs(amounts: List[CurrencyAmount]): List[(String, Double)] =
    amounts.map(amount => (amount.currency.name, amount.amount))

  private def acceptsAmounts(text: String, expected: List[(String, Double)]): IO[List[String]] =
    parseAmounts(text).map(amounts =>
      ParityHarness.assertExact(s"the description '$text'", amountPairs(amounts), expected))

  private def acceptsGroups(
      text: String,
      expected: List[List[(String, Double)]]): IO[List[String]] =
    parseMultiAmounts(text).map(groups =>
      ParityHarness.assertExact(
        s"the description '$text'",
        groups.map(group => amountPairs(group.getAmounts.toList)),
        expected))

  private def acceptsRuns(
      text: String,
      expected: List[(String, List[Double])]): IO[List[String]] =
    parseRuns(text).map(runs =>
      ParityHarness.assertExact(
        s"the description '$text'",
        runs.map { case (currency, values) => (currency.name, values.toList) },
        expected))

  private def acceptsValues(text: String, expected: Vector[Double]): IO[List[String]] =
    parseValues(text).map(values =>
      ParityHarness.assertExact(s"the value list '$text'", values, expected))

  /** The descriptions the anchored grammars must refuse, each a partial match of a real form. */
  private def refusedDescriptions: List[(String, IO[Any])] =
    List(
      ("the empty description", parseAmounts("")),
      ("an amount list with a doubled separator", parseAmounts("GBP 1,,USD 2")),
      ("an amount list with a dangling separator", parseAmounts("GBP 1, ")),
      ("an amount list of separators alone", parseAmounts(",,")),
      ("an amount list with text before it", parseAmounts("junk GBP 1")),
      ("an amount list with text after it", parseAmounts("GBP 1 junk")),
      ("a group list with text before it", parseMultiAmounts("junk [EUR 4] trailing")),
      ("a group list with text after it", parseMultiAmounts("[EUR 4] junk")),
      ("a group list with no separator", parseMultiAmounts("[EUR 4] [GBP 1]")),
      ("a group list with a doubled separator", parseMultiAmounts("[EUR 4],, [GBP 1]")),
      ("a group list with a malformed group", parseMultiAmounts("[EUR 4], [GBP 1")),
      ("an empty group list", parseMultiAmounts("")),
      ("a run list with text before it", parseRuns("junk GBP[1,2]")),
      ("a run list with text after it", parseRuns("GBP[1,2] junk")),
      ("a run list with no separator", parseRuns("GBP[1,2] USD[3]")),
      ("a run list with two separators", parseRuns("GBP[1,2],+ USD[3]")),
      ("a group list offered to the run grammar", parseRuns("[EUR 4], [GBP 1]")),
      ("a value list with a doubled separator", parseValues("1,,2")),
      ("a value list with a dangling separator", parseValues("1,")),
      ("a value list with no separator", parseValues("1 2")),
      ("a value list of text", parseValues("junk")))

  /** The branch of the dispatch each form reaches, which makes the two grammars jointly total. */
  private def dispatchDiscrepancies: List[String] =
    List(
      ("GBP[1,2,3], USD[1,2]", true),
      ("GBP[1,2,3] + USD[10,20,30]", true),
      ("junk GBP[1,2]", true),
      ("[EUR 4], [GBP 21, USD 32, EUR 43], [EUR 44]", false),
      ("[], []", false),
      ("[EUR 4] junk", false),
      ("GBP 4, USD 5", false),
      (EmptyDescription, false)).flatMap { case (text, runs) =>
      ParityHarness.assertExact(s"the run dispatch of '$text'", describesRuns(text), runs)
    }

  /**
   * Measures the four description grammars against every form the committed document writes and
   * against text that only partially matches one of them: the accepted cases are the eight
   * description strings the fixture carries, with what each names written out, so that a grammar
   * tightened past what the document writes fails here. The refused cases are
   * [[refusedDescriptions]].
   */
  def describedFormDiscrepancies: IO[List[String]] =
    for {
      empty <- acceptsAmounts(EmptyDescription, Nil)
      twoAmounts <- acceptsAmounts("GBP 4, USD 5", List(("GBP", 4.0), ("USD", 5.0)))
      threeAmounts <- acceptsAmounts(
        "GBP 1, USD 2, GBP 3",
        List(("GBP", 1.0), ("USD", 2.0), ("GBP", 3.0)))
      sameCurrency <- acceptsAmounts("GBP 100, GBP 200", List(("GBP", 100.0), ("GBP", 200.0)))
      groups <- acceptsGroups(
        "[EUR 4], [GBP 21, USD 32, EUR 43], [EUR 44]",
        List(
          List(("EUR", 4.0)),
          List(("EUR", 43.0), ("GBP", 21.0), ("USD", 32.0)),
          List(("EUR", 44.0))))
      emptyGroups <- acceptsGroups("[], []", List(Nil, Nil))
      commaRuns <- acceptsRuns(
        "GBP[1,2,3], USD[1,2]",
        List(("GBP", List(1.0, 2.0, 3.0)), ("USD", List(1.0, 2.0))))
      plusRuns <- acceptsRuns(
        "GBP[1,2,3] + USD[10,20,30]",
        List(("GBP", List(1.0, 2.0, 3.0)), ("USD", List(10.0, 20.0, 30.0))))
      noValues <- acceptsValues("", Vector.empty)
      values <- acceptsValues("1,2,3", Vector(1.0, 2.0, 3.0))
      refused <- refusedDescriptions.traverse { case (label, effect) =>
        effect.attempt.map(outcome =>
          ParityHarness.assertLeft(s"$label, which the grammar must refuse", outcome))
      }
    } yield dispatchDiscrepancies ::: empty ::: twoAmounts ::: threeAmounts ::: sameCurrency :::
      groups ::: emptyGroups ::: commaRuns ::: plusRuns ::: noValues ::: values ::: refused.flatten

  //-------------------------------------------------------------------------
  // The strictness of the declared key sets, which the third test of the suite asserts.
  //
  // A schema that is declared and not applied reads exactly like one that is, and the difference
  // only shows on a document nobody has captured yet - which is the document the declaration exists
  // for. So each shape is probed on a hand-built object: the documented shape must decode, and the
  // same object with a key added, with one of its keys removed and with one renamed must each be
  // refused with the offending key named.
  //
  // The key each probe removes is chosen so that the smaller key set matches no other documented
  // variant of the same shape. Removing `scalar` from a scaled `CurrencyAmount` entry, for one,
  // leaves exactly the documented unary shape and is properly accepted.
  //-------------------------------------------------------------------------

  /** The key an added-key probe puts on an object, which no documented variant knows. */
  private val UnknownProbeKey: String = "parityProbeUnknownKey"

  /** The suffix a renamed-key probe gives the key it renames. */
  private val RenamedProbeSuffix: String = "Renamed"

  /**
   * One documented object shape of the fixture, with the decoder that reads it.
   *
   * @param label  what the shape is, for the message of a failed probe
   * @param documented  an object of exactly the documented shape
   * @param required  a key of that shape whose removal matches no other documented variant
   * @param decode  reads the shape, answering with the refusal where there is one
   */
  final case class StrictShape(
      label: String,
      documented: JsonObject,
      required: String,
      decode: Json => Decoder.Result[Unit])

  private def amountObject(currency: String, amount: Double): JsonObject =
    JsonObject("currency" -> Json.fromString(currency), "amount" -> Json.fromDoubleOrNull(amount))

  private def arrayObject(currency: String, values: Vector[Double]): JsonObject =
    JsonObject(
      "currency" -> Json.fromString(currency),
      "values" -> Json.arr(values.map(value => Json.fromDoubleOrNull(value)): _*))

  /** A captured `Money` or `BigMoney`, whose amount is a decimal string. */
  private def moneyObject(currency: String, amount: String): JsonObject =
    JsonObject(
      "currency" -> Json.fromString(currency),
      "amount" -> Json.fromString(amount),
      "toString" -> Json.fromString(s"$currency $amount"))

  /** A captured `MultiCurrencyAmountArray` operand, which states its size. */
  private def runObject(currency: String, values: Vector[Double]): JsonObject =
    JsonObject(
      "size" -> Json.fromInt(values.size),
      "arrays" -> Json.arr(Json.fromJsonObject(arrayObject(currency, values))))

  private def rateObject(pair: String, rate: Double): JsonObject =
    JsonObject("pair" -> Json.fromString(pair), "rate" -> Json.fromDoubleOrNull(rate))

  /** The `BigMoney.roundToScale` shape, which the `Money` bucket documents no variant of. */
  private val DocumentedBigMoneyRounding: JsonObject =
    JsonObject(
      "op" -> Json.fromString("roundToScale"),
      "left" -> Json.fromJsonObject(moneyObject("AUD", "100.125")),
      "scale" -> Json.fromInt(2),
      "roundingMode" -> Json.fromString("HALF_UP"),
      "result" -> Json.fromJsonObject(moneyObject("AUD", "100.13")))

  /** The `Money.convertedTo` shape that names a rate list, which `BigMoney` documents none of. */
  private val DocumentedMoneyProviderConversion: JsonObject =
    JsonObject(
      "op" -> Json.fromString("convertedTo"),
      "left" -> Json.fromJsonObject(moneyObject("GBP", "100.00")),
      "rates" -> Json.arr(Json.fromJsonObject(rateObject("GBP/USD", 1.6))),
      "target" -> Json.fromString("USD"),
      "result" -> Json.fromJsonObject(moneyObject("USD", "160.00")))

  /** An object of exactly the documented row shape, whose buckets hold no entry. */
  private val DocumentedRow: JsonObject =
    JsonObject(
      "id" -> Json.fromString("currency-amount"),
      "source" -> Json.fromString("com.opengamma.strata.basics.currency.CurrencyAmountTest"),
      "amounts" -> Json.arr(Json.fromJsonObject(amountObject("GBP", 100.0))),
      "scalars" -> Json.arr(Json.fromDoubleOrNull(3.5)),
      "arrays" -> Json.arr(Json.fromJsonObject(arrayObject("GBP", Vector(1.0, 2.0)))),
      "multiArrays" -> Json.arr(),
      "money" -> Json.arr(),
      "bigMoney" -> Json.arr(),
      "rates" -> Json.arr(Json.fromJsonObject(rateObject("GBP/USD", 1.6))),
      "currencyAmountResults" -> Json.arr(),
      "moneyResults" -> Json.arr(),
      "bigMoneyResults" -> Json.arr(),
      "currencyAmountArrayResults" -> Json.arr(),
      "multiCurrencyAmountResults" -> Json.arr(),
      "multiCurrencyAmountArrayResults" -> Json.arr())

  /** Builds a probe from the decoder the fixture is read with. */
  private def shapeOf[A](
      label: String,
      decoder: Decoder[A],
      documented: JsonObject,
      required: String): StrictShape =
    StrictShape(label, documented, required, json => decoder.decodeJson(json).map(_ => ()))

  /** Every shape the suite proves strict: the value shapes, the six bucket entries, and the row. */
  val StrictShapes: Vector[StrictShape] =
    Vector(
      shapeOf(
        "a captured CurrencyAmount",
        amountValueDecoder,
        amountObject("GBP", 100.0),
        "amount"),
      shapeOf(
        "a captured CurrencyAmountArray",
        arrayValueDecoder,
        arrayObject("GBP", Vector(1.0, 2.0, 3.0)),
        "values"),
      shapeOf(
        "a captured Money or BigMoney",
        moneyValueDecoder,
        moneyObject("AUD", "100.12"),
        "toString"),
      shapeOf(
        "a captured MultiCurrencyAmountArray",
        multiArrayValueDecoder,
        runObject("GBP", Vector(1.0, 2.0)),
        "size"),
      shapeOf(
        "a registered multi-currency run",
        multiArrayInputDecoder,
        JsonObject(
          "arrays" -> Json.arr(Json.fromJsonObject(arrayObject("GBP", Vector(1.0, 2.0))))),
        "arrays"),
      shapeOf("a captured FX rate", rateValueDecoder, rateObject("GBP/USD", 1.6), "pair"),
      shapeOf(
        "a currencyAmountResults entry",
        currencyAmountEntryDecoder,
        JsonObject(
          "op" -> Json.fromString("multipliedBy"),
          "left" -> Json.fromJsonObject(amountObject("GBP", 100.0)),
          "scalar" -> Json.fromDoubleOrNull(3.5),
          "result" -> Json.fromJsonObject(amountObject("GBP", 350.0))),
        "left"),
      shapeOf(
        "a moneyResults entry",
        moneyEntryDecoder,
        JsonObject(
          "op" -> Json.fromString("plus"),
          "left" -> Json.fromJsonObject(moneyObject("GBP", "100.00")),
          "right" -> Json.fromJsonObject(moneyObject("GBP", "25.50")),
          "result" -> Json.fromJsonObject(moneyObject("GBP", "125.50"))),
        "left"),
      shapeOf(
        "a bigMoneyResults entry",
        bigMoneyEntryDecoder,
        DocumentedBigMoneyRounding,
        "scale"),
      shapeOf(
        "a currencyAmountArrayResults entry",
        currencyAmountArrayEntryDecoder,
        JsonObject(
          "op" -> Json.fromString("plus"),
          "left" -> Json.fromJsonObject(arrayObject("GBP", Vector(1.0, 2.0))),
          "right" -> Json.fromJsonObject(arrayObject("GBP", Vector(3.0, 4.0))),
          "result" -> Json.fromJsonObject(arrayObject("GBP", Vector(4.0, 6.0)))),
        "right"),
      shapeOf(
        "a multiCurrencyAmountResults entry",
        multiCurrencyAmountEntryDecoder,
        JsonObject(
          "op" -> Json.fromString("plus"),
          "left" -> Json.arr(Json.fromJsonObject(amountObject("GBP", 100.0))),
          "right" -> Json.arr(Json.fromJsonObject(amountObject("USD", 25.0))),
          "result" -> Json.arr(
            Json.fromJsonObject(amountObject("GBP", 100.0)),
            Json.fromJsonObject(amountObject("USD", 25.0)))),
        "right"),
      shapeOf(
        "a multiCurrencyAmountArrayResults entry",
        multiCurrencyAmountArrayEntryDecoder,
        JsonObject(
          "op" -> Json.fromString("plus"),
          "left" -> Json.fromJsonObject(runObject("GBP", Vector(1.0, 2.0))),
          "right" -> Json.fromJsonObject(runObject("GBP", Vector(3.0, 4.0))),
          "result" -> Json.fromJsonObject(runObject("GBP", Vector(4.0, 6.0)))),
        "left"),
      shapeOf("a currency-math baseline row", currencyMathRowDecoder, DocumentedRow, "rates"))

  /** Decodes every probed shape and its three mutations, answering with everything that differed. */
  def strictnessViolations: List[String] = StrictShapes.toList.flatMap(shapeViolations)

  /** The four probes of one shape: the documented object, and its three mutations. */
  private def shapeViolations(shape: StrictShape): List[String] = {
    val documented =
      shape.decode(Json.fromJsonObject(shape.documented)) match {
        case Right(_) => Nil
        case Left(failure) =>
          List(
            s"${shape.label}: the documented shape was refused, so the declared key set does not " +
              s"describe the fixture: ${failure.message}")
      }
    val renamedKey = shape.required + RenamedProbeSuffix
    val renamed = shape.documented
      .remove(shape.required)
      .add(renamedKey, shape.documented(shape.required).getOrElse(Json.Null))
    val missing = s"is missing {${shape.required}}"
    documented :::
      refusalNaming(
        shape,
        s"'$UnknownProbeKey' added",
        shape.documented.add(UnknownProbeKey, Json.fromString("unmeasured")),
        UnknownProbeKey) :::
      refusalNaming(shape, s"'${shape.required}' removed", shape.documented.remove(shape.required), missing) :::
      refusalNaming(shape, s"'${shape.required}' renamed to '$renamedKey'", renamed, renamedKey) :::
      refusalNaming(shape, s"'${shape.required}' renamed to '$renamedKey'", renamed, missing)
  }

  /** Checks that one mutated object is refused with a message naming the offending key. */
  private def refusalNaming(
      shape: StrictShape,
      mutation: String,
      mutated: JsonObject,
      naming: String): List[String] =
    shape.decode(Json.fromJsonObject(mutated)) match {
      case Right(_) =>
        List(
          s"${shape.label}: the same object with $mutation was accepted, so the declared key set " +
            "is not enforced and what that key carries would go unmeasured")
      case Left(failure) if failure.message.contains(naming) => Nil
      case Left(failure) =>
        List(
          s"${shape.label}: the same object with $mutation was refused without naming " +
            s"'$naming': ${failure.message}")
    }

  /** Checks that the two money buckets, which share one model, refuse each other's shapes. */
  def crossFamilyViolations: List[String] =
    refusedBy(
      "moneyResults",
      moneyEntryDecoder,
      "the BigMoney 'roundToScale' shape",
      DocumentedBigMoneyRounding) :::
      refusedBy(
        "bigMoneyResults",
        bigMoneyEntryDecoder,
        "the Money 'convertedTo' shape that names a rate list",
        DocumentedMoneyProviderConversion)

  /** Checks that one bucket's decoder refuses an object of another bucket's shape. */
  private def refusedBy(
      bucket: String,
      decoder: Decoder[MoneyEntry],
      shape: String,
      candidate: JsonObject): List[String] =
    decoder.decodeJson(Json.fromJsonObject(candidate)) match {
      case Left(_) => Nil
      case Right(_) =>
        List(
          s"the '$bucket' schema accepted $shape, so an entry that belongs to the other bucket " +
            "would be measured as one of this bucket's own")
    }
}
