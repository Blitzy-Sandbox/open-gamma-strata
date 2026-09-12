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
 * Parity of the currency arithmetic of the port against the Java baseline.
 *
 * This is the measurement that pins the money layer: `CurrencyAmount` and its normalisations,
 * `Money` and `BigMoney` with their rounding to a currency's minor units and to scale twelve, the
 * two amount-array types with their element-wise arithmetic and their aggregation, and
 * `MultiCurrencyAmount` with its duplicate-rejecting factory and its merging total. The values it
 * is held to were captured from the untouched Java modules by `tools/parity-capture/capture-baseline.jsh`
 * and committed as `strata-basics/src/test/resources/parity/currency-math-baseline.json`. That
 * document is read-only here: no expectation is ever corrected, loosened or skipped, and a row
 * that disagrees with the port is reported rather than accommodated.
 *
 * The gate that consumes it (AAP section 0.10.1, Gate 3, for the user's Rule 2) runs
 *
 * {{{
 * sbt -batch "testOnly *ParitySpec"
 * }}}
 *
 * and then reads `<parity.report.dir>/currency-math.json`, requiring `failed == 0`. The
 * `ParitySpec` suffix of this class, its package, the fixture stem `currency-math` and the five
 * keys of the report document are therefore all part of that contract and none of them may drift.
 *
 * ===Two comparison rules, and which applies where===
 *
 * Every `Double` the port produces - an amount, an array element, a converted value - is compared
 * by [[ParityHarness.assertParity]], which requires the difference to be within `1e-9`
 * '''absolutely and relatively'''. That is the user's Rule 2.
 *
 * Every `Money` and `BigMoney` expectation is compared by [[ParityHarness.assertExact]] and never
 * by a tolerance. This is the whole reason the capture writes those amounts as '''decimal
 * strings''' rather than as doubles (`tools/parity-capture/README.md`, section 6): rounding to a
 * currency's minor units is a discrete decision, so the scale and the trailing digits are part of
 * the expectation. `BHD 100.120` and `BHD 100.12` are the same quantity and different answers -
 * only one of them is what `Money.of(BHD, 100.12)` produces - and a tolerance would accept either.
 * The currency of every expectation, every size, every index and every captured bit pattern is
 * compared exactly for the same reason.
 *
 * A third rule applies to the four entries that carry `doubleToLongBits`. Positive and negative
 * zero are within every tolerance of each other and are not the same representation, so where the
 * capture pinned the sign of a zero this spec compares the '''bit pattern''' of the value the port
 * produced against the captured one. Those four entries are the three different answers the
 * README describes - `CurrencyAmount.of` normalises `-0.0` away, `CurrencyAmountArray` keeps the
 * sign bit of its elements, and `CurrencyAmountArray.get` normalises it again on the way out - and
 * a sign-blind implementation satisfies none of them.
 *
 * ===Failures are values, with exactly one exception===
 *
 * The fixture carries twenty-nine `error` entries, each an operation the Java implementation
 * refused. Every one of them is an expectation in its own right. All but one are data-dependent
 * failures - a currency mismatch, an unknown currency, a size mismatch, a duplicate currency, a
 * rate that is not one where no conversion is required, a missing rate - and the port reports
 * those in the error channel, so they are checked with [[ParityHarness.assertLeft]]. There is no
 * exception-interception helper, no exception handler and nothing that reaches into an `Either`
 * anywhere in this file.
 *
 * The single exception is the entry that adds `+Infinity` to `-Infinity` in one currency. Arithmetic
 * on `CurrencyAmount` is total in signature, exactly as it is in Java, so the sum that is not a
 * number is refused by the type's invariant rather than returned as a failure (AAP section 0.3.3).
 * It is observed with [[ParityHarness.attemptArgCheck]], which is the only sanctioned way to watch
 * a precondition here, and the choice between the two is made from the operands rather than
 * configured: an `error` entry whose two amounts name '''the same''' currency can only be that
 * numeric edge, because a same-currency addition has no other way to fail.
 *
 * Captured Java message text is never asserted. Turning those exceptions into a sealed failure
 * model is a recorded divergence (AAP section 0.8.3), so the `error` string is used in diagnostics
 * only.
 *
 * ===What the fixture looks like, and why there is no positional pairing===
 *
 * `tools/parity-capture/README.md` section 6 is the schema of record, and it settles the shape of
 * this spec's decoder. The document holds six rows, one per Java test family, each with the same
 * fifteen keys: an identity, the Java test class it came from, seven input lists and six
 * expectation buckets. The input lists are a '''registry''' - the deduplicated, insertion-ordered
 * record of the values the row's expectations were computed from - and not a positional pairing:
 * an expectation names its operation in `op` and carries its own operands inline. This spec
 * therefore reads each entry on its own terms, and uses the registry for what it is good for,
 * which is checking that every input the capture recorded can still be built by the port and
 * still renders as it did (see [[CurrencyMathParitySpec.checkRegistry]]).
 *
 * Two consequences are worth stating because they are what makes the measurement complete rather
 * than merely green. An entry whose shape this spec does not recognise is '''reported''' as a
 * fixture disagreement instead of being passed over, so a capture that starts emitting a new
 * operation fails the gate rather than going unmeasured. And because `failed == 0` cannot
 * distinguish a complete measurement from a thinned one, the second test below asserts the
 * population the baseline is required to carry, including that all three distinct
 * `minorUnitDigits` values and the seventy-four-currency sweeps are present.
 *
 * ===The one place the capture left a rate unregistered===
 *
 * `MultiCurrencyAmountArray.convertedTo` is captured with neither a `rate` nor a `rates` key:
 * `capture-baseline.jsh` line 7698 passes `FxRate.of(GBP, USD, 1.6)` to it without registering
 * that rate on the row. The rate is not invented here to compensate. It is resolved from the
 * '''document-wide''' rate registry - every `rates` entry of every row and of every entry, in
 * document order - which holds exactly that rate twice over, from the two rows that do register
 * it. The fixture is not edited, no literal rate appears in this file, and the resolution is
 * confined to entries that carry no rate of their own, so no `error` expectation can be satisfied
 * by it.
 *
 * ===Obligations===
 *
 * `review_rules` reports that '''no user rules were provided''': the project's rules document is
 * empty, so enterprise-standard best practice governs this file and nothing here is held to an
 * invented rule. The numbered obligations it does answer to are requirements of the user's prompt
 * carried by AAP sections 0.7, 0.8.1 and 0.10.1: Rule 2 (the tolerance above), Rule 5 (failures as
 * values), Rule 7 (`cats.effect` confined to this package, the demo and `collect.io.Resources` -
 * its use here is correct), Rule 9 (a warning-clean `-Werror` build with no suppression anywhere)
 * and Rule 10 (no collection, optional, iterator, stream or function type of the Java platform
 * library, and no mutable Scala collection; `DoubleArray` is built through its copy-safe public
 * factories, never through the module-private unchecked ones, which this module cannot reach).
 *
 * `strata-basics/src/test/resources/manifest/java-test-mapping.csv` maps no Java test method to
 * this suite - it maps none to any `*ParitySpec`, because the parity specs measure the captured
 * baseline rather than porting a Java test class - so the test names below are this spec's own and
 * satisfy no verbatim obligation.
 *
 * ===No timing===
 *
 * Nothing here asserts a duration, and the report it publishes carries none. The fixture is read
 * twice, once to collect the rate registry and once by the driver, which is deliberate: it keeps
 * every number this spec uses sourced from the document rather than from a constant in this file.
 */
class CurrencyMathParitySpec extends AsyncFunSuite with AsyncIOSpec with Matchers {

  import CurrencyMathParitySpec._

  /*
   * The measurement is declared first, deliberately: the report it publishes is the artifact
   * Gate 3 collects, so it is written on every run of this suite rather than only on the runs
   * where some other case happens to pass first.
   */
  test("currency arithmetic reproduces the Java baseline exactly") {
    for {
      // The registry is collected from the whole document before any row is measured, because one
      // captured conversion names no rate of its own; see the class documentation.
      rows <- ParityHarness.load[CurrencyMathRow](FixtureResource)
      registry <- registryProvider(rows)
      report <- ParityHarness.runFixture[CurrencyMathRow](FixtureName, FixtureResource)(row =>
        checkRow(registry, row))
      _ <- ParityHarness.failIfAny(report)
    } yield succeed
  }

  test("the fixture carries the population the currency-math baseline is required to measure") {
    ParityHarness.load[CurrencyMathRow](FixtureResource).map { rows =>
      val entryCount = rows.map(entryCountOf).sum
      val errorCount = rows.map(errorCountOf).sum
      withClue(
        s"fixture rows: ${rows.map(_.id).mkString(", ")}; expectation entries: $entryCount; " +
          s"error entries: $errorCount: ") {
        // The six rows are named rather than counted: each of the six families has to be present,
        // and a row that vanished would otherwise be hidden by a row that was added.
        rows.map(_.id) shouldBe RowIds
        // Floors rather than equalities, so that extending the capture stays possible while
        // thinning it cannot pass. The figures are the ones the README reports as observed.
        entryCount should be >= MinimumEntries
        errorCount should be >= MinimumErrorEntries
        // Every row carries the bucket of its own family and nothing else, which is the invariant
        // the row shape rests on; `checkRow` measures a row on that basis.
        rows.filterNot(row => populatedBuckets(row) == Vector(bucketOf(row.id))).map(_.id) shouldBe empty
        // Money rounding is only measured if all three distinct minor-unit scales are exercised:
        // zero digits, two and three. This is the property the per-currency sweeps exist for.
        minorUnitDigitsCovered(rows) shouldBe RequiredMinorUnitDigits
        // The two sweeps across every configured and historic currency, which are what make the
        // rounding claim hold for the whole closed currency family rather than for a
        // representative of it.
        sweptCurrencies(rows, _.moneyResults) should have size CurrencyFamilySize.toLong
        sweptCurrencies(rows, _.bigMoneyResults) should have size CurrencyFamilySize.toLong
        // The four entries that pin the sign of a zero, without which the three different answers
        // the README describes would all be satisfied by a sign-blind port.
        signedZeroEntries(rows) should be >= MinimumSignedZeroEntries
        succeed
      }
    }
  }
}

/**
 * The row model of `currency-math-baseline.json` and the checks applied to one row.
 *
 * It lives in the companion rather than in the suite because the JSON derivation needs the row
 * types on a stable path, and because keeping the measurement out of the suite body makes it plain
 * that the suite contributes nothing to the measurement beyond ordering it. Everything here is
 * confined to this package.
 */
private[parity] object CurrencyMathParitySpec {

  //-------------------------------------------------------------------------
  // Contract constants. The first two are agreements with something outside this file - the
  // resource name with the capture script that writes it, the fixture stem with the gate script
  // that reads `<parity.report.dir>/currency-math.json` - so neither may drift.
  //-------------------------------------------------------------------------

  /** The fixture stem, which names the measurement and the report document. */
  val FixtureName: String = "currency-math"

  /** The captured baseline, on the test classpath. */
  val FixtureResource: String = "parity/currency-math-baseline.json"

  /**
   * The mapping function the composed expectations were captured with.
   *
   * The capture writes it into every entry that used it, and this spec asserts the entry's text
   * against this constant before applying [[mapAmountsFunction]]. A capture that changed the
   * function would then be reported rather than silently measured against the wrong one.
   */
  val MapAmountsText: String = "x -> x * x"

  /** The function [[MapAmountsText]] describes, which is also the one the double-array fixture uses. */
  private def mapAmountsFunction(value: Double): Double = value * value

  /** The description the capture writes for the amount that holds nothing. */
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

  /** The number of expectation entries the README reports as observed, used as a floor. */
  val MinimumEntries: Int = 355

  /** The number of `error` entries the README reports as observed, used as a floor. */
  val MinimumErrorEntries: Int = 29

  /** The three distinct minor-unit scales of the closed currency family, all of which must appear. */
  val RequiredMinorUnitDigits: Set[Int] = Set(0, 2, 3)

  /** The size of the closed currency family, which both money sweeps cover in full. */
  val CurrencyFamilySize: Int = 74

  /** The entries that pin the sign of a zero through a captured bit pattern. */
  val MinimumSignedZeroEntries: Int = 4

  //-------------------------------------------------------------------------
  // The captured value shapes.
  //
  // Each is exactly what one of the capture's `j*` writers emits, named after the port type it
  // describes. The decoders are derived where the JSON keys are the field names and written out
  // where they are not: a money value carries its rendering under the key `toString`, which is not
  // a name a field may take.
  //-------------------------------------------------------------------------

  /** A `CurrencyAmount`: `{"currency": "GBP", "amount": 100.0}`. */
  final case class AmountValue(currency: String, amount: Double)

  /** A `CurrencyAmountArray`: `{"currency": "GBP", "values": [1.0, 2.0, 3.0]}`. */
  final case class ArrayValue(currency: String, values: Vector[Double])

  /**
   * A `Money` or a `BigMoney`: `{"currency": "AUD", "amount": "100.12", "toString": "AUD 100.12"}`.
   *
   * The amount is the decimal '''string''' the capture wrote from `getValue().toString()`, never a
   * double, and `text` is the type's own `toString`. Both are compared exactly.
   */
  final case class MoneyValue(currency: String, amount: String, text: String)

  /** A `MultiCurrencyAmountArray`: `{"size": 3, "arrays": [{"currency": …, "values": …}]}`. */
  final case class MultiArrayValue(size: Int, arrays: Vector[ArrayValue])

  /** A registered multi-currency run, which the registry writes without its size. */
  final case class MultiArrayInput(arrays: Vector[ArrayValue])

  /** A rate a provider was built from: `{"pair": "GBP/USD", "rate": 1.6}`. */
  final case class RateValue(pair: String, rate: Double)

  implicit val amountValueDecoder: Decoder[AmountValue] = deriveDecoder[AmountValue]

  implicit val arrayValueDecoder: Decoder[ArrayValue] = deriveDecoder[ArrayValue]

  implicit val moneyValueDecoder: Decoder[MoneyValue] =
    Decoder.instance(cursor =>
      for {
        currency <- cursor.get[String]("currency")
        amount <- cursor.get[String]("amount")
        text <- cursor.get[String]("toString")
      } yield MoneyValue(currency, amount, text))

  implicit val multiArrayValueDecoder: Decoder[MultiArrayValue] = deriveDecoder[MultiArrayValue]

  implicit val multiArrayInputDecoder: Decoder[MultiArrayInput] = deriveDecoder[MultiArrayInput]

  implicit val rateValueDecoder: Decoder[RateValue] = deriveDecoder[RateValue]

  //-------------------------------------------------------------------------
  // The expectation of one entry.
  //-------------------------------------------------------------------------

  /**
   * What the Java implementation produced for one operation: a value, or a refusal.
   *
   * The two are modelled as a sum rather than as an `Option` because a refusal is an expectation
   * in its own right and not the absence of one. Collapsing them would leave the spec unable to
   * distinguish "Java refused this" from "the capture recorded nothing", and the first of those is
   * a quarter of the entries of some rows.
   */
  sealed trait Expected[+A]

  object Expected {

    /** Java produced this value. */
    final case class Value[+A](value: A) extends Expected[A]

    /** Java refused, with this message, which is used in diagnostics and never asserted. */
    final case class Failed(message: String) extends Expected[Nothing]
  }

  /**
   * Reads the expectation of an entry, trying `error` before `result`.
   *
   * An entry carrying both, or neither, is a fixture that no longer agrees with this spec and is
   * reported as a decoding failure rather than resolved by a precedence rule: exactly one of the
   * two keys states the expectation, which is the contract section 6 of the capture README gives.
   *
   * @param cursor  the entry
   * @return the expectation the entry states
   */
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
  // The operands whose captured shape is a union.
  //
  // Three keys carry more than one shape, because the Java members they were captured from are
  // overloaded: an amount array is added to another array or to a single amount, a multi-currency
  // run to another run or to a single multi-currency amount, and a run's expectation is a run, a
  // converted single-currency array or a bare list of values. Each union is decoded by shape
  // alone, which is unambiguous here because the alternatives have disjoint required keys - an
  // array carries `values`, an amount carries `amount`, a run carries `size` - or are told apart
  // by being a JSON array rather than an object.
  //-------------------------------------------------------------------------

  /** The right-hand operand, or the expectation, of a single-currency amount array operation. */
  sealed trait ArrayOperand

  object ArrayOperand {

    /** A whole array of amounts in one currency. */
    final case class Run(value: ArrayValue) extends ArrayOperand

    /** One amount, which an array operation broadcasts over its elements. */
    final case class Single(value: AmountValue) extends ArrayOperand
  }

  implicit val arrayOperandDecoder: Decoder[ArrayOperand] =
    arrayValueDecoder
      .map[ArrayOperand](ArrayOperand.Run(_))
      .or(amountValueDecoder.map[ArrayOperand](ArrayOperand.Single(_)))

  /** The construction input of an amount array: a literal list of values, or a description. */
  sealed trait ArrayInput

  object ArrayInput {

    /** The values the amounts were built from, all in the entry's currency. */
    final case class Values(values: Vector[Double]) extends ArrayInput

    /** A description such as `"GBP 4, USD 5"`, which names amounts in more than one currency. */
    final case class Described(text: String) extends ArrayInput
  }

  implicit val arrayInputDecoder: Decoder[ArrayInput] =
    Decoder[Vector[Double]]
      .map[ArrayInput](ArrayInput.Values(_))
      .or(Decoder[String].map[ArrayInput](ArrayInput.Described(_)))

  /** The expectation of a multi-currency amount operation: every amount, or a single one. */
  sealed trait MultiOperand

  object MultiOperand {

    /** The amounts of a multi-currency value, written sorted by currency code. */
    final case class Amounts(amounts: Vector[AmountValue]) extends MultiOperand

    /** One amount, which is what `getAmount` and `convertedTo` produce. */
    final case class Single(value: AmountValue) extends MultiOperand
  }

  implicit val multiOperandDecoder: Decoder[MultiOperand] =
    Decoder[Vector[AmountValue]]
      .map[MultiOperand](MultiOperand.Amounts(_))
      .or(amountValueDecoder.map[MultiOperand](MultiOperand.Single(_)))

  /** The right-hand operand of a multi-currency run operation: another run, or one amount. */
  sealed trait MultiArrayOperand

  object MultiArrayOperand {

    /** Another run of multi-currency amounts. */
    final case class Run(value: MultiArrayValue) extends MultiArrayOperand

    /** One multi-currency amount, which a run operation broadcasts over its indices. */
    final case class Amounts(amounts: Vector[AmountValue]) extends MultiArrayOperand
  }

  implicit val multiArrayOperandDecoder: Decoder[MultiArrayOperand] =
    multiArrayValueDecoder
      .map[MultiArrayOperand](MultiArrayOperand.Run(_))
      .or(Decoder[Vector[AmountValue]].map[MultiArrayOperand](MultiArrayOperand.Amounts(_)))

  /** The expectation of a multi-currency run operation. */
  sealed trait MultiArrayResult

  object MultiArrayResult {

    /** A run of multi-currency amounts. */
    final case class Run(value: MultiArrayValue) extends MultiArrayResult

    /** A single-currency array, which is what `convertedTo` produces. */
    final case class Converted(value: ArrayValue) extends MultiArrayResult

    /** The values held for one currency, which is what `getValues` produces. */
    final case class Values(values: Vector[Double]) extends MultiArrayResult
  }

  implicit val multiArrayResultDecoder: Decoder[MultiArrayResult] =
    multiArrayValueDecoder
      .map[MultiArrayResult](MultiArrayResult.Run(_))
      .or(arrayValueDecoder.map[MultiArrayResult](MultiArrayResult.Converted(_)))
      .or(Decoder[Vector[Double]].map[MultiArrayResult](MultiArrayResult.Values(_)))

  //-------------------------------------------------------------------------
  // The five entry models, one per expectation bucket - the two money buckets share theirs,
  // because `Money` and `BigMoney` were captured through the same writer.
  //
  // Every operand key is optional because the bucket holds several operations, and every decoder
  // is written out rather than derived because the expectation is a sum of `result` and `error`
  // that no derivation produces. An entry that carries an operand this spec does not expect for
  // its operation is reported by the check, not by the decoder: reading the document is one
  // concern and recognising an operation is another.
  //-------------------------------------------------------------------------

  /** One captured `CurrencyAmount` operation. */
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

  implicit val currencyAmountEntryDecoder: Decoder[CurrencyAmountEntry] =
    Decoder.instance(cursor =>
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
        expected))

  /**
   * One captured `Money` or `BigMoney` operation.
   *
   * The scalar is a `Long` rather than a `Double` because both types multiply by a whole number
   * only, which is the signature the implementation being ported had.
   */
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

  implicit val moneyEntryDecoder: Decoder[MoneyEntry] =
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

  /** One captured `CurrencyAmountArray` operation. */
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

  implicit val currencyAmountArrayEntryDecoder: Decoder[CurrencyAmountArrayEntry] =
    Decoder.instance(cursor =>
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
        expected))

  /** One captured `MultiCurrencyAmount` operation. */
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

  implicit val multiCurrencyAmountEntryDecoder: Decoder[MultiCurrencyAmountEntry] =
    Decoder.instance(cursor =>
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
        expected))

  /** One captured `MultiCurrencyAmountArray` operation. */
  final case class MultiCurrencyAmountArrayEntry(
      op: String,
      left: Option[MultiArrayValue],
      right: Option[MultiArrayOperand],
      input: Option[String],
      size: Option[Int],
      currency: Option[String],
      target: Option[String],
      scalar: Option[Double],
      mapAmountsFn: Option[String],
      composed: Boolean,
      expected: Expected[MultiArrayResult])

  implicit val multiCurrencyAmountArrayEntryDecoder: Decoder[MultiCurrencyAmountArrayEntry] =
    Decoder.instance(cursor =>
      for {
        op <- cursor.get[String]("op")
        left <- cursor.get[Option[MultiArrayValue]]("left")
        right <- cursor.get[Option[MultiArrayOperand]]("right")
        input <- cursor.get[Option[String]]("input")
        size <- cursor.get[Option[Int]]("size")
        currency <- cursor.get[Option[String]]("currency")
        target <- cursor.get[Option[String]]("target")
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
        scalar,
        mapAmountsFn,
        composed.getOrElse(false),
        expected))

  /**
   * One row: the identity, the Java test class it came from, the seven input lists and the six
   * expectation buckets.
   *
   * The fifteen keys are present on every row of the document, so no field of this product is
   * optional; an input list or an expectation bucket a row does not use is an empty array. Five of
   * the six buckets are empty on every row, which is the invariant the second test asserts and the
   * one [[checkRow]] dispatches on.
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

  implicit val currencyMathRowDecoder: Decoder[CurrencyMathRow] = deriveDecoder[CurrencyMathRow]

  //-------------------------------------------------------------------------
  // Building the operands.
  //
  // Every operand is rebuilt through the port's own public factories, which is the point: an
  // operand the port cannot build is a defect in the port or in the fixture rather than a parity
  // result of any kind, so it is lifted into a failed effect through the harness's two lifts and
  // recorded against the row. A captured currency code is one of the seventy-four the closed
  // family holds - the document was captured from the implementation that defines them - so a
  // refusal here is a real defect and is surfaced as one.
  //
  // The two arrays are built with `DoubleArray.copyOf`, a copy-safe public factory. The unchecked
  // factories that alias a caller's array are module-private and unavailable here by design.
  //-------------------------------------------------------------------------

  /** Resolves a captured currency code. */
  private def currencyOf(code: String): IO[Currency] = ParityHarness.raise(Currency.parse(code))

  /** Rebuilds a captured amount. */
  private def amountOf(value: AmountValue): IO[CurrencyAmount] =
    currencyOf(value.currency).flatMap(currency =>
      ParityHarness.raise(CurrencyAmount.of(currency, value.amount)))

  /**
   * Rebuilds a captured money value from its exact decimal text.
   *
   * The decimal is read from the captured string rather than from a double, so the operand carries
   * the scale the capture recorded, and the total `Money.of(Currency, Decimal)` then rounds it to
   * the currency's minor units - which is a no-op for a value that came from the same rounding.
   */
  private def moneyOf(value: MoneyValue): IO[Money] =
    for {
      currency <- currencyOf(value.currency)
      decimal <- ParityHarness.raise(Decimal.of(value.amount))
    } yield Money.of(currency, decimal)

  /** Rebuilds a captured big-money value from its exact decimal text. */
  private def bigMoneyOf(value: MoneyValue): IO[BigMoney] =
    for {
      currency <- currencyOf(value.currency)
      decimal <- ParityHarness.raise(Decimal.of(value.amount))
    } yield BigMoney.of(currency, decimal)

  /** Rebuilds a captured single-currency array. */
  private def runOf(value: ArrayValue): IO[CurrencyAmountArray] =
    currencyOf(value.currency)
      .map(currency => CurrencyAmountArray.of(currency, DoubleArray.copyOf(value.values)))

  /** Rebuilds a captured multi-currency amount, whose currencies are distinct by construction. */
  private def multiOf(values: Vector[AmountValue]): IO[MultiCurrencyAmount] =
    values.toList
      .traverse(amountOf)
      .flatMap(amounts => ParityHarness.raise(MultiCurrencyAmount.of(amounts)))

  /**
   * Reads the values a captured multi-currency run holds per currency.
   *
   * The size of a rebuilt run is derived from its arrays, as the map factory derives it, so a
   * captured operand that states a size while holding no array at all cannot be reproduced this
   * way. No operand of the document is of that shape - the one run with no currencies is captured
   * as a construction input rather than as an operand - and the case is refused explicitly rather
   * than silently producing a run of size zero.
   */
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

  /** Rebuilds a captured multi-currency run. */
  private def multiRunOf(value: MultiArrayValue): IO[MultiCurrencyAmountArray] =
    runEntriesOf(value)
      .flatMap(entries => ParityHarness.raiseNec(MultiCurrencyAmountArray.of(entries)))

  /** The amounts a captured array construction input names, all of them built by the port. */
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
  // Reading the descriptions the capture wrote for inputs that are not a single typed value.
  //
  // Four entries build from a list the capture could not write as one of the seven typed input
  // lists - a mixed-currency list of amounts, which is rejected, or a ragged list of
  // multi-currency amounts, which is zero-filled - so it wrote a short description instead
  // (capture README, section 6). Each description is read back here through the port's own
  // parsers, so the text is data rather than a special case: `"GBP 4"` is exactly the form
  // `CurrencyAmount.parse` accepts.
  //-------------------------------------------------------------------------

  /** The bracketed groups of a description such as `"[EUR 4], [GBP 21, USD 32, EUR 43], [EUR 44]"`. */
  private val BracketGroup: Regex = """\[([^\]]*)\]""".r

  /** A currency and its values, as in `"GBP[1,2,3]"`. */
  private val CurrencyRun: Regex = """([A-Za-z]{3})\[([^\]]*)\]""".r

  /** Whether a description names whole runs per currency rather than individual amounts. */
  private def describesRuns(text: String): Boolean = CurrencyRun.findFirstIn(text).isDefined

  /** The amounts a description such as `"GBP 4, USD 5"` names; `"empty"` names none. */
  private def parseAmounts(text: String): IO[List[CurrencyAmount]] =
    if (text.trim == EmptyDescription) {
      IO.pure(Nil)
    } else {
      text
        .split(",")
        .toList
        .map(_.trim)
        .filter(_.nonEmpty)
        .traverse(part => ParityHarness.raise(CurrencyAmount.parse(part)))
    }

  /** The multi-currency amounts a description of bracketed groups names, one per group. */
  private def parseMultiAmounts(text: String): IO[List[MultiCurrencyAmount]] =
    BracketGroup
      .findAllMatchIn(text)
      .map(matched => matched.group(1))
      .toList
      .traverse(group =>
        parseAmounts(group).flatMap(amounts => ParityHarness.raise(MultiCurrencyAmount.of(amounts))))

  /** The runs a description such as `"GBP[1,2,3], USD[1,2]"` or `"GBP[1,2] + USD[3,4]"` names. */
  private def parseRuns(text: String): IO[List[(Currency, DoubleArray)]] =
    CurrencyRun
      .findAllMatchIn(text)
      .toList
      .traverse(matched =>
        for {
          currency <- currencyOf(matched.group(1))
          values <- parseValues(matched.group(2))
        } yield (currency, DoubleArray.copyOf(values)))

  /** The values a comma-separated numeral list names, which may be empty. */
  private def parseValues(text: String): IO[Vector[Double]] =
    text
      .split(",")
      .toVector
      .map(_.trim)
      .filter(_.nonEmpty)
      .traverse(part =>
        IO.fromOption(part.toDoubleOption)(
          new IllegalStateException(
            s"fixture disagreement: '$part' is not a numeral, and a captured description holds " +
              "only numerals between its brackets")))

  //-------------------------------------------------------------------------
  // The rate providers.
  //
  // A captured conversion names its rates inline, and the capture supplied them as an `FxRate` -
  // which is itself an `FxRateProvider` - so a single rate is replayed as exactly that. Several
  // rates are replayed as a provider that answers from the first of them that can, which is the
  // behaviour of a list of rates and, unlike a matrix, does not additionally require the rates to
  // name a connected set of currencies. That matters for the document-wide registry, whose rates
  // are deliberately not connected.
  //-------------------------------------------------------------------------

  /** Rebuilds the rates of one entry, or of the whole document. */
  private def fxRatesOf(rates: Vector[RateValue]): IO[Vector[FxRate]] =
    rates.traverse(entry =>
      ParityHarness
        .raise(CurrencyPair.parse(entry.pair))
        .flatMap(pair => ParityHarness.raiseNec(FxRate.of(pair, entry.rate))))

  /** The provider a list of rates describes. */
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

  /** Builds the provider a captured list of rates describes. */
  private def providerFrom(rates: Vector[RateValue]): IO[FxRateProvider] =
    fxRatesOf(rates).map(providerOf)

  /**
   * Builds the provider used by an entry that carries no rate of its own.
   *
   * The rates of every row and of every entry, in document order and deduplicated. One captured
   * conversion needs it, for the reason the class documentation gives, and this is what keeps the
   * rate it uses a value read from the fixture rather than a literal written here.
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
  // The comparators of this fixture, each built from the harness's own.
  //
  // A money value is compared exactly in all three of its parts; every other value compares its
  // currency, its size and its indices exactly and its numbers under the parity tolerance.
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
   * Compares a money value exactly, never under a tolerance.
   *
   * All three captured parts are compared: the currency, the decimal text of the value - which
   * carries the scale of the currency's minor units, so `BHD 100.120` does not match `100.12` -
   * and the rendering. `getValue` is failable on `Money` because a value has to fit the fixed
   * scale it is asked for, and a value that came from that same rounding does; the check goes
   * through [[ParityHarness.assertRight]] so that a refusal is reported rather than unwrapped.
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

  /**
   * Compares a multi-currency amount against the amounts the capture wrote.
   *
   * The port holds its amounts in a `SortedMap` keyed by currency, so `getAmounts` iterates in
   * currency-code order. The capture writes them sorted the same way, and the expectation is
   * sorted here as well rather than trusted to arrive in that order: the ordering is a property of
   * the port being measured, so reading it from the fixture would make the comparison agree with
   * itself.
   */
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

  /**
   * Compares a multi-currency run: its size and currencies exactly, its values under the
   * tolerance.
   *
   * The currencies are compared as a whole list before any value is, so a run holding the wrong
   * set of currencies is reported once instead of once per currency, and the values are only
   * compared when the currencies agree.
   */
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
   * Compares the bit pattern of a value whose sign the capture pinned.
   *
   * Positive and negative zero are within every tolerance of each other, so the four entries that
   * carry `doubleToLongBits` are the only way the three different answers the port gives for a
   * signed zero can be told apart. The captured `long` is the authority, not the JSON number
   * beside it.
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

  /**
   * Checks that an operation refused for the reason the port documents for it.
   *
   * Used where the reason is unambiguous - a conversion the provider has no rate for is a
   * currency-conversion failure and nothing else - so that a refusal for some unrelated reason is
   * not accepted as the expected one. The message is never compared; only the reason is.
   */
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

  /** Checks the minor-unit scale the capture recorded against the one the port's currency holds. */
  private def checkMinorUnitDigits(
      label: String,
      currency: Currency,
      captured: Option[Int]): List[String] =
    captured.toList.flatMap(digits =>
      ParityHarness.assertExact(s"$label.minorUnitDigits", currency.minorUnitDigits, digits))

  /** Checks that a composed expectation was captured with the mapping function this spec applies. */
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

  /** The rounding modes by the constant name the capture wrote, which is a closed set. */
  private val RoundingModesByName: Map[String, RoundingMode] =
    RoundingMode.values().iterator.map(mode => (mode.name(), mode)).toMap

  //-------------------------------------------------------------------------
  // The `CurrencyAmount` bucket.
  //-------------------------------------------------------------------------

  /**
   * Measures one captured `CurrencyAmount` operation.
   *
   * @param entry  the captured operation
   * @return every discrepancy found, empty where the port reproduced it
   */
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
   * The choice is made from the operands, not from configuration. Two amounts in '''different'''
   * currencies cannot be combined at all, and the port reports that in the error channel, so the
   * refusal is a `Left`. Two amounts in the '''same''' currency always combine, and the only way
   * the outcome can be refused is the documented numeric edge of the type - infinities of opposite
   * sign summing to a value that is not a number - which the port refuses through its invariant,
   * exactly as the implementation being ported did (AAP section 0.3.3). That one is observed with
   * [[ParityHarness.attemptArgCheck]], and it is the only precondition this fixture reaches.
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

  /**
   * Measures a multiplication by a number, which is total.
   *
   * The entries that sweep the currency family carry the currency's minor-unit scale, which is
   * checked against the port's own currency data: the amount arithmetic does not depend on it, but
   * the capture recorded it and a disagreement would mean the two sides no longer hold the same
   * currency.
   */
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

  /**
   * Measures a conversion at an explicit rate.
   *
   * The captured refusals of this operation are the same-currency rule: a conversion into the
   * currency the amount is already in requires a rate of one, and the port compares it with the
   * same fuzzy equality the implementation being ported used, so the rates 1.25 and 1.5 are
   * refused while 1 is not.
   */
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
   * Measures the factory, which is where the two normalisations of the type are pinned.
   *
   * A value that is not a number is refused in the error channel, and `-0.0` is normalised to
   * `+0.0` - which is why the captured bit pattern is compared here and not only the value.
   * Infinities are accepted, as they were by the implementation being ported, so "not finite" is
   * not one category on this path.
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
  // The `Money` and `BigMoney` buckets. Every comparison here is exact.
  //
  // Each operand is compared against its own captured rendering as it is rebuilt. That costs
  // nothing and it pins the decimal text of every operand of the two seventy-four-currency sweeps,
  // which the input registry does not record.
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
        // The port expresses the widening on the companion of the wider type rather than as a
        // method of the narrower one, so `BigMoney.of(money)` is what this captured call replays.
        (entry.left, entry.expected) match {
          case (Some(leftValue), Expected.Value(expected)) =>
            moneyOf(leftValue).map(left =>
              compareMoney(s"${entry.op}.left", left, leftValue) :::
                compareBigMoney(entry.op, BigMoney.of(left), expected))
          case _ =>
            IO.pure(unrecognised(s"the Money '${entry.op}' entry", "no value and expectation"))
        }
      case other => IO.pure(unrecognised("a Money entry", s"the operation '$other'"))
    }

  /**
   * Measures `Money.of`, which rounds half up to the currency's minor units.
   *
   * The captured `minorUnitDigits` is compared against the port's currency as well, because it is
   * the number the rounding is performed to: an expectation of `BHD 100.125` only means anything
   * if both sides agree that the currency has three minor digits.
   */
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

  /**
   * Measures a conversion, at an explicit rate or through a provider.
   *
   * The explicit rate is applied through the exact-decimal overload rather than the binary
   * floating point one, which is how the capture applied it: the rate is a decimal quantity and
   * reading it as one keeps the conversion exact.
   */
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

  /** Measures an addition or a subtraction of two big-money values. */
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

  /** Measures a big-money conversion at an explicit rate or through a provider. */
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

  /**
   * Measures the twelve captured roundings: six modes at scale two and three negative scales.
   *
   * The mode is looked up in the closed set of the platform's rounding modes by the constant name
   * the capture wrote, and a name that is not one of them is reported rather than resolved, so an
   * unknown mode cannot be read as a default.
   */
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
            // The port adds this member, which the Java type does not have; the capture therefore
            // composed the expectation from `of(currency, values.multipliedBy(scalar))`. Measuring
            // the port's own member against that composition is the whole point of the entry.
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

  /**
   * Measures an element-wise combination, against another array or against one amount.
   *
   * Both captured refusals are data-dependent - the currencies differ, or the lengths do - so both
   * are `Left`, and the port reports a different failure for each.
   */
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

  /**
   * Measures a conversion of a whole array.
   *
   * The captured entry carries either the single rate the conversion was performed at - which the
   * capture supplied as the `FxRate` from the array's currency to the target - or the list of
   * rates a provider was built from, which is how the missing-rate refusal is captured: a rate for
   * an unrelated pair leaves the conversion without one, and the port reports that as a
   * currency-conversion failure rather than leaving the values unconverted.
   */
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

  /** The provider a captured array conversion was performed through. */
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
   * Measures the three factories of the type, told apart by the keys of the entry.
   *
   * An entry carrying a `size` was captured through `of(size, valueFunction)`. An entry carrying a
   * captured bit pattern was captured through `of(currency, DoubleArray)`, which is the only one
   * of the three that '''keeps''' the sign of a zero - the other two build their elements through
   * `CurrencyAmount.of`, which normalises it away - so the marker is what identifies it. Everything
   * else was captured through `of(Iterable[CurrencyAmount])`, whose mixed-currency refusal is one
   * of the entries here.
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

  /**
   * Measures reading one element back out.
   *
   * The element is handed to `CurrencyAmount.of`, which normalises the sign of a zero the array
   * itself kept, so this is the third of the three answers the captured bit patterns pin.
   */
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
        // `of` rejects a repeated currency where `total` merges it, and the fixture captures both
        // over the same input, so the two are never substituted for one another here.
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
            // Another member the port adds, captured as the composition of the same scaling over
            // every currency of the run.
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
   * Measures the factories of the type, told apart by what the entry names.
   *
   * The map factory is the one that '''rejects''' arrays of unequal length, and it is reached
   * either from a captured run or from a description naming whole runs per currency. The list and
   * function factories take multi-currency amounts instead, where a currency missing from one
   * element is zero-filled rather than rejected and a currency absent from every element stays
   * unknown - the contrast the capture records deliberately - and a `size` distinguishes the
   * function form from the list form.
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

  /**
   * Measures converting a whole run into one currency.
   *
   * This is the one captured conversion that names no rate of its own, so the provider it is
   * replayed through is the document-wide one; the class documentation records why, and the
   * provider is still built entirely from rates the fixture registers.
   */
  private def checkMultiRunConversion(
      entry: MultiCurrencyAmountArrayEntry,
      fallback: FxRateProvider): IO[List[String]] =
    (entry.left, entry.target) match {
      case (Some(leftValue), Some(target)) =>
        for {
          left <- multiRunOf(leftValue)
          targetCurrency <- currencyOf(target)
        } yield {
          val converted = left.convertedTo(targetCurrency, fallback)
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
   * empty on every row of the document and measuring them costs nothing, while dispatching on the
   * row's identity instead would leave an entry that appeared in the wrong bucket unmeasured. The
   * shape of the row is checked separately, so a bucket that is populated where it should not be is
   * reported as the fixture disagreement it is.
   *
   * @param fallback  the provider for the one captured conversion that names no rate
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

  /**
   * Measures every entry of one bucket, labelling each message with the bucket and the index.
   *
   * An entry whose measurement raises instead of answering contributes one message naming the
   * error and the remaining entries of the bucket are still measured. That matters here in a way
   * it does not for a fixture of small rows: one row of this document holds a hundred and fifteen
   * entries, and letting the first unbuildable operand end the row would take a hundred and
   * fourteen measurements with it.
   */
  private def checkEntries[E](bucket: String, entries: Vector[E])(
      check: E => IO[List[String]]): IO[List[String]] =
    entries.zipWithIndex.toList.flatTraverse { case (entry, index) =>
      check(entry).attempt.map {
        case Right(messages) => messages.map(message => s"$bucket[$index] $message")
        case Left(error) => List(s"$bucket[$index]: the entry could not be replayed: $error")
      }
    }

  /**
   * Checks the invariants of the row itself, before anything is measured.
   *
   * These are properties of the document rather than of the port: an identity this spec does not
   * know, or a row populating a bucket that is not its family's, is a fixture that has stopped
   * agreeing with this spec, and reporting it as such keeps it from being read as a defect of the
   * port.
   */
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
   * Checks that every input the row registers can still be built by the port, and still renders
   * as it did.
   *
   * The registry is not a positional pairing and cannot be measured as one, but it is not inert
   * either: it is the record of the values the row's expectations were computed from, so rebuilding
   * each of them through the port's factories and comparing the rebuilt value against its own
   * captured form catches a mistranscribed input, a currency the closed family no longer holds and
   * a change in the way a value renders - the last of which is what a money amount's decimal text
   * is. The registered numbers themselves need no construction and are consumed by the entries
   * that name them.
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
  // The population of the document, which the second test of the suite asserts.
  //
  // A gate that reads `failed == 0` cannot tell a complete measurement from a thinned one, so
  // these read the coverage of the fixture rather than the behaviour of the port.
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

  /** Whether an expectation records a refusal. */
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

  /**
   * The currencies one money bucket names, whether as the currency of a factory or as the currency
   * of the value an operation was applied to.
   *
   * Both money buckets carry a sweep across the whole closed currency family - `Money.of` in one
   * and `BigMoney.toMoney` in the other - and the two record it differently, so both places are
   * read.
   */
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
}
