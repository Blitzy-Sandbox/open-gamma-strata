/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.json

import java.time.DayOfWeek
import java.time.LocalDate
import java.time.LocalTime
import java.time.Period
import java.time.YearMonth
import java.time.ZoneId

import scala.collection.immutable.List
import scala.collection.immutable.Map

import cats.data.EitherNec
import cats.data.NonEmptyChain
import cats.data.NonEmptyList

import io.circe.Codec
import io.circe.CursorOp
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.JsonNumber
import io.circe.KeyDecoder
import io.circe.KeyEncoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder
import io.circe.parser.parse

import org.scalacheck.Gen

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import com.opengamma.strata.collect.Arbitraries
import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.array.DoubleMatrix
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.Failure

/**
 * A closed family of named values, standing in for the named families of this port.
 *
 * The shape is the one every closed family of the port uses: a sealed class whose members
 * exist only in its companion, and a name lookup built in that companion from tables written
 * in code. Nothing is read from a class path, a class or a configuration file, so what this
 * family can resolve is fixed when it is compiled.
 *
 * Two spellings beyond the three canonical names make the decode path observable. `Pomme` is
 * an ''alternate'' spelling, which the exact lookup resolves on its own. `pome` is resolvable
 * only by the ''lenient'' rewrite: the exact lookup does not hold it, folding it to upper case
 * does not reach a member either, and only the rewrite below turns it into a canonical name. A
 * codec that decoded through the exact lookup rather than through the family's parsing would
 * therefore reject it, which is what makes it worth having here.
 */
sealed abstract class SampleFruit private (val name: String) extends Named

/**
 * Holds the members of [[SampleFruit]], its two spellings and its name lookup.
 */
object SampleFruit {

  /** A member whose name the alternate and the lenient spelling both point at. */
  case object Apple extends SampleFruit("Apple")

  /** A second member, so a table of members has more than one row. */
  case object Banana extends SampleFruit("Banana")

  /** A third member, so encoding a member is not trivially the only possible string. */
  case object Cherry extends SampleFruit("Cherry")

  /** The alternate spelling of `Apple`, which the exact lookup resolves. */
  val AlternateSpelling: String = "Pomme"

  /** The upper-case form of the alternate spelling, which the lookup also registers. */
  val UpperAlternateSpelling: String = "POMME"

  /** The spelling of `Apple` that only a lenient rewrite resolves. */
  val LenientSpelling: String = "pome"

  /** A spelling no member and no table claims. */
  val UnknownSpelling: String = "Durian"

  /** The label the family gives itself when it rejects text. */
  val FamilyName: String = "SampleFruit"

  /** The members of the family, in declaration order. */
  val values: NonEmptyList[SampleFruit] = NonEmptyList.of(Apple, Banana, Cherry)

  /**
   * The name lookup of the family, with one alternate spelling and one lenient rewrite.
   *
   * The rewrite is written in upper case and its replacement is a literal, because parsing
   * folds its input to upper case before applying a rewrite and matches the whole of it.
   *
   * @return the name lookup over [[values]]
   */
  implicit val namedEnum: NamedEnum[SampleFruit] = NamedEnum.of(
    values = values,
    alternates = Map(AlternateSpelling -> Apple.name),
    lenient = List("POME".r -> Apple.name),
    familyName = FamilyName)
}

/**
 * An open value identified by text, standing in for the parsed string types of this port.
 *
 * It is deliberately not built on the typed-string support of this module: that support has
 * its own spec, and a fixture built on it would test it here a second time rather than test
 * the codec. What matters for the codec is only that the companion parses text, that parsing
 * can fail for more than one reason at once, and that rendering is the inverse of parsing.
 *
 * The two parsing factories accept exactly the same text and differ only in how much they say
 * about text they reject: [[SampleTicker.parseNec]] reports every cause, and
 * [[SampleTicker.parse]] reports the first of them. That is the difference between the two
 * codecs under test, so holding everything else equal is what isolates it.
 */
sealed abstract case class SampleTicker private (text: String)

/**
 * Holds the parsing, the rendering and the two messages of [[SampleTicker]].
 */
object SampleTicker {

  /** The message reported when the text is shorter than three characters. */
  val LengthMessage: String = "Ticker must hold at least three characters"

  /** The message reported when the text is not made only of upper-case letters. */
  val LettersMessage: String = "Ticker must be upper-case letters"

  /** The shortest text accepted. */
  private val MinimumLength: Int = 3

  /** The text accepted, which is the pattern the ported sample type validated against. */
  private val LettersPattern = "[A-Z]+".r

  /**
   * Parses text, reporting every reason it is unacceptable.
   *
   * The two checks are independent, so text that is both too short and wrongly cased produces
   * two failures, in the order the checks are written.
   *
   * @param text  the text to parse
   * @return the value, or every cause of rejection
   */
  def parseNec(text: String): EitherNec[Failure, SampleTicker] = {
    val tooShort: List[Failure] =
      if (text.length < MinimumLength) List(Failure.Invalid(LengthMessage)) else Nil
    val wronglyCased: List[Failure] =
      if (LettersPattern.pattern.matcher(text).matches()) Nil else List(Failure.Invalid(LettersMessage))
    NonEmptyChain.fromSeq(tooShort ++ wronglyCased) match {
      case Some(failures) => Left(failures)
      case None => Right(new SampleTicker(text) {})
    }
  }

  /**
   * Parses text, reporting the first reason it is unacceptable.
   *
   * @param text  the text to parse
   * @return the value, or the first cause of rejection
   */
  def parse(text: String): Either[Failure, SampleTicker] =
    parseNec(text).left.map(failures => failures.head)

  /**
   * Renders a value as the canonical text its parsing accepts.
   *
   * @param ticker  the value to render
   * @return the text of the value
   */
  def print(ticker: SampleTicker): String = ticker.text
}

/**
 * A product nested inside [[SampleConfig]], carrying an optional field of its own.
 *
 * Its only purpose is depth: an absent field one level down is what distinguishes the deep
 * removal the port relies on from a removal that only reaches the outermost object.
 *
 * @param tag  a field that always holds a value
 * @param detail  a field that may hold no value
 */
final case class SampleNote(tag: String, detail: Option[String])

/**
 * Holds the derived codec of [[SampleNote]], which the outer product's derivation needs.
 */
object SampleNote {

  /** The derived encoder, left as it comes so that the outer wrapping is the only one. */
  implicit val encoder: Encoder[SampleNote] = deriveEncoder[SampleNote]

  /** The derived decoder, which reads an absent field as holding no value. */
  implicit val decoder: Decoder[SampleNote] = deriveDecoder[SampleNote]
}

/**
 * A total product with optional fields, standing in for the derived products of this port.
 *
 * Both halves of its codec are derived at compile time from its declared shape, and the
 * encoder is published twice: once as derived, and once wrapped so that a field holding no
 * value is left out. Keeping both is what lets the spec show the difference the wrapping makes
 * rather than assert only the wrapped result.
 *
 * @param name  a field that always holds a value
 * @param note  a field that may hold no value
 * @param size  a second field that may hold no value, of a different type
 * @param nested  a nested product that carries an optional field of its own
 */
final case class SampleConfig(name: String, note: Option[String], size: Option[Int], nested: SampleNote)

/**
 * Holds the two encoders and the decoder of [[SampleConfig]].
 */
object SampleConfig {

  /** The derived encoder as it comes, which writes an absent field as an empty one. */
  val rawEncoder: Encoder[SampleConfig] = deriveEncoder[SampleConfig]

  /** The encoder a type of this port publishes, which omits an absent field. */
  val encoder: Encoder[SampleConfig] = Codecs.dropNulls(rawEncoder)

  /** The derived decoder, which is the same for both encoders. */
  implicit val decoder: Decoder[SampleConfig] = deriveDecoder[SampleConfig]
}

/**
 * The raw field shape of [[SampleBounds]], whose decoder is derived.
 *
 * A validated type of this port derives a decoder for a product of its constructor fields and
 * hands it to its own factory, so that the field shape is the compiler's business and the
 * decision about whether those fields form a legal value stays in the factory. This is that
 * product, made visible here only because a spec has to build the payloads.
 *
 * @param low  the field a payload carries for the lower bound
 * @param high  the field a payload carries for the upper bound
 */
final case class RawBounds(low: Int, high: Int)

/**
 * Holds the derived codec of [[RawBounds]].
 */
object RawBounds {

  /** The derived decoder, which the validating decoders below are built on. */
  implicit val decoder: Decoder[RawBounds] = deriveDecoder[RawBounds]

  /** The derived encoder, used by the spec to build payloads. */
  val encoder: Encoder[RawBounds] = deriveEncoder[RawBounds]
}

/**
 * A validated product, standing in for the validated types of this port.
 *
 * It is named for its meaning rather than called `Range`, because that name is already taken
 * by the standard library. Its factory has two independent invariants, so one payload can
 * break both and the spec can see how several causes are reported together.
 */
sealed abstract case class SampleBounds private (low: Int, high: Int)

/**
 * Holds the factories, the two messages and the two decoders of [[SampleBounds]].
 */
object SampleBounds {

  /** The message reported when the lower bound is below zero. */
  val NegativeMessage: String = "Bounds must not start below zero"

  /** The message reported when the upper bound is below the lower one. */
  val OrderMessage: String = "Bounds must not end below where they start"

  /**
   * Builds a value, reporting every broken invariant.
   *
   * @param low  the lower bound, which must not be below zero
   * @param high  the upper bound, which must not be below the lower bound
   * @return the value, or every cause of rejection
   */
  def of(low: Int, high: Int): EitherNec[Failure, SampleBounds] = {
    val negative: List[Failure] =
      if (low < 0) List(Failure.Invalid(NegativeMessage)) else Nil
    val disordered: List[Failure] =
      if (high < low) List(Failure.Invalid(OrderMessage)) else Nil
    NonEmptyChain.fromSeq(negative ++ disordered) match {
      case Some(failures) => Left(failures)
      case None => Right(new SampleBounds(low, high) {})
    }
  }

  /**
   * Builds a value, reporting the first broken invariant.
   *
   * @param low  the lower bound, which must not be below zero
   * @param high  the upper bound, which must not be below the lower bound
   * @return the value, or the first cause of rejection
   */
  def ofChecked(low: Int, high: Int): Either[Failure, SampleBounds] =
    of(low, high).left.map(failures => failures.head)

  /** The decoder a validated type publishes, reporting every cause. */
  val decoder: Decoder[SampleBounds] =
    Codecs.validatedDecoder[RawBounds, SampleBounds](raw => of(raw.low, raw.high))

  /** The decoder a checked type publishes, reporting one cause. */
  val checkedDecoder: Decoder[SampleBounds] =
    Codecs.checkedDecoder[RawBounds, SampleBounds](raw => ofChecked(raw.low, raw.high))
}

/**
 * A product whose every field type is carried by the nested implicits object.
 *
 * A derivation site needs the codec of each of its field types in implicit scope, and each of
 * the four types below is one for which this module publishes a single sensible choice. They
 * are the whole of what that object offers. The derivation in the companion imports them
 * together, which is the whole purpose of that object: the plain numeric codec the JSON library
 * publishes for a double would otherwise be found instead, and the values this module has to
 * carry outside the finite range would be lost.
 *
 * @param factor  a double, which adopts the tagged policy through the import
 * @param series  an immutable array of doubles
 * @param grid  an immutable matrix of doubles
 * @param day  a day of the week, the date and time type the JSON library does not cover at all
 */
final case class SampleReading(factor: Double, series: DoubleArray, grid: DoubleMatrix, day: DayOfWeek)

/**
 * Holds the codec of [[SampleReading]], derived under the imported implicits.
 */
object SampleReading {

  import Codecs.implicits._

  /** The derived encoder, which writes every field through the imported policy. */
  implicit val encoder: Encoder[SampleReading] = deriveEncoder[SampleReading]

  /** The derived decoder, which reads every field through the imported policy. */
  implicit val decoder: Decoder[SampleReading] = deriveDecoder[SampleReading]
}

/**
 * A product carrying a time of day, a type the nested implicits object deliberately omits.
 *
 * A time of day is one of the five date and time types whose instance comes from the JSON
 * library unchanged, so no member of that object covers it. This product exists to show what a
 * derivation site does under that policy: importing the object's members brings nothing for
 * this field, the library's own instance is found through its companion, and the document
 * carries the library's ISO text.
 *
 * @param venue  a plain field, so the product has more than one
 * @param opens  a time of day, written by the instance the library publishes for it
 */
final case class SampleOpening(venue: String, opens: LocalTime)

/**
 * Tests [[Codecs]], the reusable JSON machinery of this module.
 *
 * This package has no counterpart in the library being ported, so nothing here is a
 * transcription of an existing test: the original serialized a value by reading its structure
 * back from its own class while the program ran, and that mechanism has no successor to test.
 * What replaced it is a small set of helpers, each turning something a type already has - its
 * name lookup, its parsing factory, its validating factory, its derived field shape - into a
 * codec built by the compiler. Every expectation below is therefore derived from the declared
 * contract of those helpers and from the serialization policy of the port they implement.
 *
 * ===Why this spec is the one that pins the shapes===
 *
 * The helpers are generic, and the concrete instances live at the types that use them, so this
 * is the only place where each shape can be asserted once for every type that will adopt it.
 * Four of those assertions carry further than this module:
 *
 *  - the single policy for a value outside the finite range, which every double, array element
 *    and matrix element of both modules goes through;
 *  - the failure a rejected payload produces - the messages of the causes joined by a
 *    semicolon and a space, each rendered single-line and bounded and only so many of them
 *    named, positioned at the cursor that was being decoded - which every validated and
 *    every normalising type decodes through;
 *  - the omission of a field that holds no value, which every derived product encoder adopts;
 *  - the ceilings a document is read under - the length of an array, the three dimensions of a
 *    matrix, and the size of a collection read through `boundedElements` or `boundedFields` -
 *    which bound every numeric payload of both modules and every collection a type of either
 *    module bounds. Each ceiling is asserted at the figure itself and one beyond it, and each
 *    refusal is asserted to be reached without the payload having been read, since a ceiling
 *    applied after the reading would have paid for what it was meant to refuse.
 *
 * ===What is deliberately absent===
 *
 * `Codecs` publishes no codec for a failure or for a failure reason, and none for the three
 * numeric and text types built on it. The failure model owns its own instances, and the three
 * numeric types are ''consumers'' of the helpers here, which is exactly why the helpers cannot
 * mention them. Their round trips belong to their own specs, so this spec asserts none of
 * them.
 *
 * ===Exactness===
 *
 * Every round trip below is asserted exactly, and the doubles are compared by bit pattern
 * rather than by numeric equality. Two facts about IEEE-754 make that necessary rather than
 * fastidious: a value that is not a number is not numerically equal to itself, so a numeric
 * comparison against it always fails; and the two signed zeroes are numerically equal, so a
 * numeric comparison passes even when the sign has been lost. The equality of the array and
 * matrix types already compares bit patterns, so for those two a plain comparison of values is
 * both correct and sufficient. No tolerance appears anywhere in this file: agreement to a
 * tolerance is what the parity specs measure, and serialization has to be lossless.
 */
final class CodecsSpec extends AnyFunSuite with Matchers with EitherValues with ScalaCheckPropertyChecks {

  /**
   * The number of values each generated property below is checked against.
   *
   * The default of the framework is a handful, which is too few here: the element generators
   * of this module mix the IEEE-754 edge values in among ordinary numbers, so the values that
   * make a round-trip property worth asserting - one that is not a number, an infinity, a
   * signed zero - appear only in a proportion of the draws.
   */
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  /** The codec of the sample family, resolving text through the family's own parsing. */
  private val fruitCodec: Codec[SampleFruit] = Codecs.namedEnumCodec[SampleFruit]

  /** The codec of the sample parsed value whose factory reports one cause. */
  private val tickerCodec: Codec[SampleTicker] =
    Codecs.parsedStringCodec[SampleTicker](text => SampleTicker.parse(text), ticker => SampleTicker.print(ticker))

  /** The codec of the sample parsed value whose factory reports every cause. */
  private val tickerNecCodec: Codec[SampleTicker] =
    Codecs.parsedStringCodecNec[SampleTicker](text => SampleTicker.parseNec(text), ticker => SampleTicker.print(ticker))

  /**
   * The key codecs of the sample family, taken as a pair and bound to two named values.
   *
   * The helper hands back a pair rather than publishing implicits, so the two halves are
   * named here and only then made implicit. A pattern-matching implicit value would work
   * through a synthesised tuple, which is both harder to read and a source of trouble under
   * the compiler settings of this build.
   */
  private val fruitKeyCodecs: (KeyEncoder[SampleFruit], KeyDecoder[SampleFruit]) =
    Codecs.namedKeyCodecs[SampleFruit]((name: String) => SampleFruit.namedEnum.valueOf(name))

  /** The key encoder of the sample family, in implicit scope for the map cases. */
  implicit val fruitKeyEncoder: KeyEncoder[SampleFruit] = fruitKeyCodecs._1

  /** The key decoder of the sample family, in implicit scope for the map cases. */
  implicit val fruitKeyDecoder: KeyDecoder[SampleFruit] = fruitKeyCodecs._2

  /** The payloads that are not JSON strings, which every text-based codec must reject. */
  private val nonStringPayloads = Table(
    "payload",
    Json.fromInt(1),
    Json.fromDoubleOrNull(1.5),
    Json.True,
    Json.False,
    Json.Null,
    Json.arr(Json.fromString("Apple")),
    Json.obj("name" -> Json.fromString("Apple")))

  /** The payloads that are not JSON arrays, which the array and matrix codecs must reject. */
  private val nonArrayPayloads = Table(
    "payload",
    Json.fromInt(1),
    Json.fromDoubleOrNull(1.5),
    Json.fromString("[1.0]"),
    Json.True,
    Json.Null,
    Json.obj("values" -> Json.arr(Json.fromDoubleOrNull(1.0))))

  /**
   * The bit pattern of a double, which is how this spec compares doubles.
   *
   * @param value  the value to take apart
   * @return the bits of the value
   */
  private def bitsOf(value: Double): Long = java.lang.Double.doubleToLongBits(value)

  /**
   * The field names of a JSON object, in the order they appear, or none if it is not one.
   *
   * @param json  the JSON to inspect
   * @return the field names in order
   */
  private def keysOf(json: Json): List[String] = json.asObject.toList.flatMap(fields => fields.keys.toList)

  /**
   * The fields of a JSON object as a mapping, without regard to the order they appear in.
   *
   * This is the shape to assert against whenever the order of the fields is not itself part
   * of the contract under test - which is the case wherever the fields come from a collection
   * that does not define an order of its own.
   *
   * @param json  the JSON to inspect
   * @return the field names with the JSON each holds, or nothing if it is not an object
   */
  private def fieldsOf(json: Json): Map[String, Json] =
    json.asObject.fold(Map.empty[String, Json])(fields => fields.toMap)

  /**
   * The JSON a value reaches after being written out as text and read back.
   *
   * Passing through the text form is what makes a round trip complete: an assertion made only
   * in memory cannot see a value lost by the printer or by the parser.
   *
   * @param json  the JSON to write out and read back
   * @return the JSON that survived the text form
   */
  private def reparse(json: Json): Json = parse(json.noSpaces).value

  /**
   * The JSON a piece of document text states.
   *
   * Going through the parser is what makes a payload one a real document could carry: a value
   * assembled in memory can only hold what the memory representation allows, while text can
   * state anything the JSON grammar allows.
   *
   * @param text  the document text to read
   * @return the JSON it states
   */
  private def parsedText(text: String): Json = parse(text).value

  /**
   * A JSON number built straight from its digits, without a document around it.
   *
   * This is the other way a number reaches a codec: not parsed out of a document but handed
   * over as a number value by a caller that assembled it. Both routes matter, because the two
   * carry the digits differently.
   *
   * @param text  the digits of the number
   * @return the JSON number they state
   */
  private def numberFrom(text: String): Json =
    JsonNumber
      .fromString(text)
      .fold(fail(s"the text $text does not state a JSON number"))(number => Json.fromJsonNumber(number))

  //-------------------------------------------------------------------------
  // namedEnumCodec: a closed family is the string of a member's name
  //-------------------------------------------------------------------------
  test("namedEnumCodec encodes every member of a family as its canonical name") {
    forAll(Table("member", SampleFruit.values.toList: _*)) { (member: SampleFruit) =>
      fruitCodec(member) shouldBe Json.fromString(member.name)
    }
  }

  test("namedEnumCodec decodes the canonical name of every member back to that member") {
    forAll(Table("member", SampleFruit.values.toList: _*)) { (member: SampleFruit) =>
      fruitCodec.decodeJson(Json.fromString(member.name)) shouldBe Right(member)
    }
  }

  test("namedEnumCodec round-trips every member of a family") {
    forAll(Table("member", SampleFruit.values.toList: _*)) { (member: SampleFruit) =>
      fruitCodec.decodeJson(fruitCodec(member)) shouldBe Right(member)
    }
  }

  test("namedEnumCodec round-trips every member through the text form") {
    forAll(Table("member", SampleFruit.values.toList: _*)) { (member: SampleFruit) =>
      fruitCodec(member).noSpaces shouldBe "\"" + member.name + "\""
      fruitCodec.decodeJson(reparse(fruitCodec(member))) shouldBe Right(member)
    }
  }

  test("namedEnumCodec decodes an alternate spelling to the member it names") {
    fruitCodec.decodeJson(Json.fromString(SampleFruit.AlternateSpelling)) shouldBe Right(SampleFruit.Apple)
  }

  test("namedEnumCodec decodes the upper-case form of an alternate spelling") {
    fruitCodec.decodeJson(Json.fromString(SampleFruit.UpperAlternateSpelling)) shouldBe Right(SampleFruit.Apple)
  }

  test("namedEnumCodec decodes a lenient spelling that the exact lookup rejects") {
    // the premise: neither the spelling nor its upper-case form is a key of the exact lookup,
    // so only the family's lenient rewrite can resolve it - which is what the decoder uses
    SampleFruit.namedEnum.valueOf(SampleFruit.LenientSpelling) shouldBe None
    SampleFruit.namedEnum.valueOf("POME") shouldBe None
    fruitCodec.decodeJson(Json.fromString(SampleFruit.LenientSpelling)) shouldBe Right(SampleFruit.Apple)
  }

  test("namedEnumCodec decodes the upper-case form of a lenient spelling") {
    fruitCodec.decodeJson(Json.fromString("POME")) shouldBe Right(SampleFruit.Apple)
  }

  test("namedEnumCodec re-encodes a value decoded from an alternate spelling as its canonical name") {
    val decoded = fruitCodec.decodeJson(Json.fromString(SampleFruit.AlternateSpelling)).value
    fruitCodec(decoded) shouldBe Json.fromString("Apple")
  }

  test("namedEnumCodec re-encodes a value decoded from a lenient spelling as its canonical name") {
    val decoded = fruitCodec.decodeJson(Json.fromString(SampleFruit.LenientSpelling)).value
    fruitCodec(decoded) shouldBe Json.fromString("Apple")
  }

  test("namedEnumCodec rejects a name that no member and no table claims") {
    val outcome = fruitCodec.decodeJson(Json.fromString(SampleFruit.UnknownSpelling))
    outcome.left.value.message shouldBe "SampleFruit name not found: Durian"
  }

  test("namedEnumCodec reports the failure of the family at the position it was decoding") {
    val outcome = fruitCodec.decodeJson(Json.fromString(SampleFruit.UnknownSpelling))
    outcome.left.value.history shouldBe List.empty[CursorOp]
  }

  test("namedEnumCodec reports a rejected name inside a payload at the position of the field") {
    val payload = Json.obj("fruit" -> Json.fromString(SampleFruit.UnknownSpelling))
    val outcome = payload.hcursor.downField("fruit").as[SampleFruit](fruitCodec)
    outcome.left.value.message shouldBe "SampleFruit name not found: Durian"
    outcome.left.value.history shouldBe List[CursorOp](CursorOp.DownField("fruit"))
  }

  test("namedEnumCodec rejects a JSON value that is not a string") {
    forAll(nonStringPayloads) { (payload: Json) =>
      fruitCodec.decodeJson(payload).isLeft shouldBe true
    }
  }


  //-------------------------------------------------------------------------
  // parsedStringCodec and parsedStringCodecNec: an open value is its canonical text
  //-------------------------------------------------------------------------
  /** The text every parsing factory of the sample value accepts. */
  private val validTickerTexts = Table("text", "ABC", "OPENGAMMA", "XYZ")

  /** The text every parsing factory of the sample value rejects, with what is wrong with it. */
  private val invalidTickerTexts = Table(
    ("text", "messages"),
    ("ab", List(SampleTicker.LengthMessage, SampleTicker.LettersMessage)),
    ("AB", List(SampleTicker.LengthMessage)),
    ("abc", List(SampleTicker.LettersMessage)),
    ("Abc", List(SampleTicker.LettersMessage)),
    ("", List(SampleTicker.LengthMessage, SampleTicker.LettersMessage)),
    ("A1C", List(SampleTicker.LettersMessage)))

  /**
   * A value of the sample parsed type, built through its own factory.
   *
   * The type has no public constructor and no copy, exactly as a validated type of this port
   * has none, so this is the only way a spec can obtain one.
   *
   * @param text  the text of the value, which must be acceptable
   * @return the value
   */
  private def ticker(text: String): SampleTicker = SampleTicker.parse(text).value

  test("parsedStringCodec encodes a value as the canonical text its parsing accepts") {
    forAll(validTickerTexts) { (text: String) =>
      tickerCodec(ticker(text)) shouldBe Json.fromString(text)
    }
  }

  test("parsedStringCodecNec encodes a value as the canonical text its parsing accepts") {
    forAll(validTickerTexts) { (text: String) =>
      tickerNecCodec(ticker(text)) shouldBe Json.fromString(text)
    }
  }

  test("parsedStringCodec round-trips every value its parsing accepts") {
    forAll(validTickerTexts) { (text: String) =>
      tickerCodec.decodeJson(tickerCodec(ticker(text))).value.text shouldBe text
    }
  }

  test("parsedStringCodecNec round-trips every value its parsing accepts") {
    forAll(validTickerTexts) { (text: String) =>
      tickerNecCodec.decodeJson(tickerNecCodec(ticker(text))).value.text shouldBe text
    }
  }

  test("parsedStringCodec round-trips a value through the text form") {
    forAll(validTickerTexts) { (text: String) =>
      val written = tickerCodec(ticker(text))
      written.noSpaces shouldBe "\"" + text + "\""
      tickerCodec.decodeJson(reparse(written)).value.text shouldBe text
    }
  }

  test("the two forms of the parsed-string codec accept exactly the same text") {
    forAll(validTickerTexts) { (text: String) =>
      tickerCodec.decodeJson(Json.fromString(text)).isRight shouldBe true
      tickerNecCodec.decodeJson(Json.fromString(text)).isRight shouldBe true
    }
    forAll(invalidTickerTexts) { (text: String, messages: List[String]) =>
      messages should not be empty
      tickerCodec.decodeJson(Json.fromString(text)).isLeft shouldBe true
      tickerNecCodec.decodeJson(Json.fromString(text)).isLeft shouldBe true
    }
  }

  test("parsedStringCodec reports the message of the single failure its factory gave") {
    val outcome = tickerCodec.decodeJson(Json.fromString("abc"))
    outcome.left.value.message shouldBe "Ticker must be upper-case letters"
  }

  test("parsedStringCodec reports the first cause when its factory could have given more") {
    val outcome = tickerCodec.decodeJson(Json.fromString("ab"))
    outcome.left.value.message shouldBe "Ticker must hold at least three characters"
  }

  test("parsedStringCodecNec joins the messages of every cause with a semicolon and a space") {
    val outcome = tickerNecCodec.decodeJson(Json.fromString("ab"))
    outcome.left.value.message shouldBe
      "Ticker must hold at least three characters; Ticker must be upper-case letters"
  }

  test("parsedStringCodecNec reports a single cause as its own message, unadorned") {
    val outcome = tickerNecCodec.decodeJson(Json.fromString("abc"))
    outcome.left.value.message shouldBe "Ticker must be upper-case letters"
  }

  test("parsedStringCodecNec reports every cause of every rejected text") {
    forAll(invalidTickerTexts) { (text: String, messages: List[String]) =>
      val outcome = tickerNecCodec.decodeJson(Json.fromString(text))
      outcome.left.value.message shouldBe messages.mkString("; ")
    }
  }

  test("parsedStringCodec positions a rejected text at the cursor it was decoding") {
    val payload = Json.obj("ticker" -> Json.fromString("abc"))
    val outcome = payload.hcursor.downField("ticker").as[SampleTicker](tickerCodec)
    outcome.left.value.message shouldBe "Ticker must be upper-case letters"
    outcome.left.value.history shouldBe List[CursorOp](CursorOp.DownField("ticker"))
  }

  test("parsedStringCodecNec positions a rejected text at the cursor it was decoding") {
    val payload = Json.obj("ticker" -> Json.fromString("ab"))
    val outcome = payload.hcursor.downField("ticker").as[SampleTicker](tickerNecCodec)
    outcome.left.value.message shouldBe
      "Ticker must hold at least three characters; Ticker must be upper-case letters"
    outcome.left.value.history shouldBe List[CursorOp](CursorOp.DownField("ticker"))
  }

  test("parsedStringCodec rejects a JSON value that is not a string") {
    forAll(nonStringPayloads) { (payload: Json) =>
      tickerCodec.decodeJson(payload).isLeft shouldBe true
    }
  }

  test("parsedStringCodecNec rejects a JSON value that is not a string") {
    forAll(nonStringPayloads) { (payload: Json) =>
      tickerNecCodec.decodeJson(payload).isLeft shouldBe true
    }
  }

  //-------------------------------------------------------------------------
  // namedKeyCodecs: a named value keys an object by its name
  //-------------------------------------------------------------------------
  test("namedKeyCodecs encodes a key as the canonical name of the value") {
    forAll(Table("member", SampleFruit.values.toList: _*)) { (member: SampleFruit) =>
      fruitKeyCodecs._1(member) shouldBe member.name
    }
  }

  test("namedKeyCodecs decodes a key back to the value it names") {
    forAll(Table("member", SampleFruit.values.toList: _*)) { (member: SampleFruit) =>
      fruitKeyCodecs._2(member.name) shouldBe Some(member)
    }
  }

  test("namedKeyCodecs resolves a key given by an alternate spelling") {
    fruitKeyCodecs._2(SampleFruit.AlternateSpelling) shouldBe Some(SampleFruit.Apple)
  }

  test("namedKeyCodecs leaves a key that only a lenient rewrite would reach unresolved") {
    // a key is machine-written canonical output, so the lookup given to the helper here is the
    // exact one and the leniency the codec of the value itself applies is deliberately absent
    fruitKeyCodecs._2(SampleFruit.LenientSpelling) shouldBe None
  }

  test("namedKeyCodecs answers with no value for a key that no value claims") {
    fruitKeyCodecs._2(SampleFruit.UnknownSpelling) shouldBe None
  }

  test("a map keyed by named values encodes with the canonical names as its field names") {
    // the map is an ordinary one, which defines no order over its entries, so what is asserted
    // is the mapping the object states rather than the sequence its fields happen to appear in:
    // the order of the fields is the map's business and no part of the contract of a key codec
    val map: Map[SampleFruit, Int] = Map(SampleFruit.Apple -> 1, SampleFruit.Cherry -> 3)
    val json = Encoder[Map[SampleFruit, Int]].apply(map)
    fieldsOf(json) shouldBe Map("Apple" -> Json.fromInt(1), "Cherry" -> Json.fromInt(3))
  }

  test("a map keyed by one named value encodes as exactly the one field that value names") {
    // one entry leaves no room for an order, so the bytes are pinned exactly here instead
    val map: Map[SampleFruit, Int] = Map(SampleFruit.Cherry -> 3)
    Encoder[Map[SampleFruit, Int]].apply(map).noSpaces shouldBe """{"Cherry":3}"""
  }

  test("a map keyed by named values round-trips") {
    val map: Map[SampleFruit, Int] = Map(SampleFruit.Apple -> 1, SampleFruit.Cherry -> 3)
    val json = Encoder[Map[SampleFruit, Int]].apply(map)
    Decoder[Map[SampleFruit, Int]].decodeJson(json) shouldBe Right(map)
  }

  test("a map keyed by named values accepts a field name given by an alternate spelling") {
    val payload = Json.obj(SampleFruit.AlternateSpelling -> Json.fromInt(7))
    Decoder[Map[SampleFruit, Int]].decodeJson(payload) shouldBe
      Right(Map[SampleFruit, Int](SampleFruit.Apple -> 7))
  }

  test("a map keyed by named values rejects a field name that no value claims") {
    val payload = Json.obj(SampleFruit.UnknownSpelling -> Json.fromInt(1))
    val outcome = Decoder[Map[SampleFruit, Int]].decodeJson(payload)
    outcome.left.value.message shouldBe "Couldn't decode key."
    outcome.left.value.history shouldBe List[CursorOp](CursorOp.DownField("Durian"))
  }

  test("namedEnumKeyCodecs takes the exact lookup of the family as its key lookup") {
    val codecs: (KeyEncoder[SampleFruit], KeyDecoder[SampleFruit]) = Codecs.namedEnumKeyCodecs[SampleFruit]
    codecs._1(SampleFruit.Banana) shouldBe "Banana"
    codecs._2("Banana") shouldBe Some(SampleFruit.Banana)
    codecs._2(SampleFruit.AlternateSpelling) shouldBe Some(SampleFruit.Apple)
    codecs._2(SampleFruit.LenientSpelling) shouldBe None
    codecs._2(SampleFruit.UnknownSpelling) shouldBe None
  }


  //-------------------------------------------------------------------------
  // taggedDouble: a number, or one of exactly three strings
  //-------------------------------------------------------------------------
  /** The three values JSON has no syntax for, each with the string it is written as. */
  private val nonFiniteDoubles = Table(
    ("value", "tag"),
    (Double.NaN, "NaN"),
    (Double.PositiveInfinity, "Infinity"),
    (Double.NegativeInfinity, "-Infinity"))

  /** Finite values worth carrying, including both zeroes and the extremes of the range. */
  private val finiteDoubles = Table(
    "value",
    0.0,
    -0.0,
    1.0,
    -1.0,
    2.5,
    -3.0,
    0.1,
    1.0e-9,
    123456789.123456789,
    java.lang.Double.MIN_VALUE,
    java.lang.Double.MAX_VALUE,
    -java.lang.Double.MAX_VALUE)

  /**
   * Numbers the JSON grammar allows and no double can hold, each with the form it takes.
   *
   * The grammar bounds neither the digits nor the exponent of a number, so a document may
   * state a magnitude beyond the range of a double. Converting such a literal produces an
   * infinity, which would be a second spelling of a value this codec writes only as a string,
   * so every row here must be refused. Both routes a number takes into a codec are covered:
   * parsed out of document text, and handed over as a number value assembled by a caller.
   */
  private val overRangeNumbers = Table(
    ("payload", "form"),
    (parsedText("1e999"), "a positive magnitude far beyond the range, parsed from text"),
    (parsedText("-1e999"), "a negative magnitude far beyond the range, parsed from text"),
    (parsedText("1e309"), "a positive magnitude just beyond the range, parsed from text"),
    (parsedText("-1e309"), "a negative magnitude just beyond the range, parsed from text"),
    (parsedText("2e308"), "a magnitude just above the largest double, parsed from text"),
    (parsedText("1" + "0" * 309), "a magnitude written out in digits rather than with an exponent"),
    (numberFrom("1e9999"), "a positive magnitude beyond the range, as a number value"),
    (numberFrom("-1e9999"), "a negative magnitude beyond the range, as a number value"),
    (Json.fromBigDecimal(BigDecimal("1e999")), "a positive magnitude beyond the range, as an exact decimal"),
    (Json.fromBigDecimal(BigDecimal("-1e999")), "a negative magnitude beyond the range, as an exact decimal"))

  /** Text a lenient codec might have accepted, and this one must not. */
  private val rejectedDoubleTexts = Table(
    "text",
    "rubbish",
    "1.5",
    "0",
    "nan",
    "NAN",
    "Nan",
    "infinity",
    "INFINITY",
    "+Infinity",
    "inf",
    "Inf",
    "-infinity",
    "-Inf",
    " NaN",
    "NaN ",
    "")

  test("taggedDouble encodes a finite value as a JSON number") {
    forAll(finiteDoubles) { (value: Double) =>
      val json = Codecs.taggedDouble(value)
      json.isNumber shouldBe true
      json.asNumber.map(number => bitsOf(number.toDouble)) shouldBe Some(bitsOf(value))
    }
  }

  test("taggedDouble encodes each value JSON cannot express as its own tag") {
    forAll(nonFiniteDoubles) { (value: Double, tag: String) =>
      Codecs.taggedDouble(value) shouldBe Json.fromString(tag)
    }
  }

  test("taggedDouble decodes each of its three tags back to the value it names") {
    forAll(nonFiniteDoubles) { (value: Double, tag: String) =>
      bitsOf(Codecs.taggedDouble.decodeJson(Json.fromString(tag)).value) shouldBe bitsOf(value)
    }
  }

  test("taggedDouble decodes the tag of a value that is not a number to a value that is not a number") {
    Codecs.taggedDouble.decodeJson(Json.fromString("NaN")).value.isNaN shouldBe true
  }

  test("taggedDouble decodes a JSON number to the same bits") {
    forAll(finiteDoubles) { (value: Double) =>
      val decoded = Codecs.taggedDouble.decodeJson(Json.fromDoubleOrNull(value)).value
      bitsOf(decoded) shouldBe bitsOf(value)
    }
  }

  test("taggedDouble round-trips every finite value exactly") {
    forAll(finiteDoubles) { (value: Double) =>
      val decoded = Codecs.taggedDouble.decodeJson(Codecs.taggedDouble(value)).value
      bitsOf(decoded) shouldBe bitsOf(value)
    }
  }

  test("taggedDouble round-trips every finite value through the text form") {
    forAll(finiteDoubles) { (value: Double) =>
      val decoded = Codecs.taggedDouble.decodeJson(reparse(Codecs.taggedDouble(value))).value
      bitsOf(decoded) shouldBe bitsOf(value)
    }
  }

  test("taggedDouble round-trips each value JSON cannot express through the text form") {
    forAll(nonFiniteDoubles) { (value: Double, tag: String) =>
      Codecs.taggedDouble(value).noSpaces shouldBe "\"" + tag + "\""
      val decoded = Codecs.taggedDouble.decodeJson(reparse(Codecs.taggedDouble(value))).value
      bitsOf(decoded) shouldBe bitsOf(value)
    }
  }

  test("taggedDouble carries the sign of a negative zero through the whole serialization path") {
    // a numeric comparison cannot see this: -0.0 == 0.0 is true, so only the bits show the sign
    val written = Codecs.taggedDouble(-0.0)
    written.noSpaces shouldBe "-0.0"
    bitsOf(Codecs.taggedDouble.decodeJson(written).value) shouldBe bitsOf(-0.0)
    bitsOf(Codecs.taggedDouble.decodeJson(reparse(written)).value) shouldBe bitsOf(-0.0)
  }

  test("taggedDouble keeps a positive zero distinct from a negative zero") {
    bitsOf(-0.0) should not be bitsOf(0.0)
    val negative = Codecs.taggedDouble.decodeJson(reparse(Codecs.taggedDouble(-0.0))).value
    val positive = Codecs.taggedDouble.decodeJson(reparse(Codecs.taggedDouble(0.0))).value
    bitsOf(negative) shouldBe bitsOf(-0.0)
    bitsOf(positive) shouldBe bitsOf(0.0)
    bitsOf(negative) should not be bitsOf(positive)
  }

  test("taggedDouble carries the smallest positive value without losing a digit") {
    val written = Codecs.taggedDouble(java.lang.Double.MIN_VALUE)
    bitsOf(Codecs.taggedDouble.decodeJson(reparse(written)).value) shouldBe bitsOf(java.lang.Double.MIN_VALUE)
  }

  test("taggedDouble rejects text that is neither one of its three tags nor anything else it accepts") {
    forAll(rejectedDoubleTexts) { (text: String) =>
      Codecs.taggedDouble.decodeJson(Json.fromString(text)).isLeft shouldBe true
    }
  }

  test("taggedDouble rejects the digits of a number delivered as text") {
    Codecs.taggedDouble.decodeJson(Json.fromString("1.5")).isLeft shouldBe true
  }

  test("taggedDouble rejects an arbitrary string") {
    Codecs.taggedDouble.decodeJson(Json.fromString("rubbish")).isLeft shouldBe true
  }

  test("taggedDouble rejects a tag written in the wrong case") {
    Codecs.taggedDouble.decodeJson(Json.fromString("nan")).isLeft shouldBe true
    Codecs.taggedDouble.decodeJson(Json.fromString("infinity")).isLeft shouldBe true
  }

  test("taggedDouble rejects a tag that is nearly right") {
    Codecs.taggedDouble.decodeJson(Json.fromString("+Infinity")).isLeft shouldBe true
    Codecs.taggedDouble.decodeJson(Json.fromString("inf")).isLeft shouldBe true
  }

  test("taggedDouble rejects the empty string") {
    Codecs.taggedDouble.decodeJson(Json.fromString("")).isLeft shouldBe true
  }

  test("taggedDouble rejects a number no double can hold") {
    forAll(overRangeNumbers) { (payload: Json, form: String) =>
      withClue(s"$form: ") {
        payload.isNumber shouldBe true
        Codecs.taggedDouble.decodeJson(payload).isLeft shouldBe true
      }
    }
  }

  test("taggedDouble never reads a number as a value outside the finite range") {
    // this is the property the rejection exists for: the string tags are the only way a value
    // JSON cannot express enters, so no number may ever decode to one
    forAll(overRangeNumbers) { (payload: Json, form: String) =>
      withClue(s"$form: ") {
        Codecs.taggedDouble.decodeJson(payload).toOption.exists(value => !java.lang.Double.isFinite(value)) shouldBe
          false
      }
    }
  }

  test("taggedDouble reports the range it expected when it rejects a number no double can hold") {
    val outcome = Codecs.taggedDouble.decodeJson(parsedText("1e999"))
    outcome.left.value.message shouldBe
      "Expected a JSON number a double can hold; a magnitude beyond that range is written as " +
        "the string Infinity or -Infinity"
  }

  test("taggedDouble positions a rejected over-range number at the cursor it was decoding") {
    val payload = parsedText("""{"factor":1e999}""")
    val outcome = payload.hcursor.downField("factor").as[Double](Codecs.taggedDouble)
    outcome.left.value.history shouldBe List[CursorOp](CursorOp.DownField("factor"))
  }

  test("taggedDouble still accepts the tag of the value an over-range number would have produced") {
    // the value itself is representable and carried; what is refused is the numeric spelling
    bitsOf(Codecs.taggedDouble.decodeJson(Json.fromString("Infinity")).value) shouldBe
      bitsOf(Double.PositiveInfinity)
    bitsOf(Codecs.taggedDouble.decodeJson(Json.fromString("-Infinity")).value) shouldBe
      bitsOf(Double.NegativeInfinity)
  }

  test("taggedDouble accepts the largest and smallest magnitudes a double can hold as numbers") {
    // the boundary is the range of a double itself, not some narrower window
    bitsOf(Codecs.taggedDouble.decodeJson(parsedText("1.7976931348623157e308")).value) shouldBe
      bitsOf(java.lang.Double.MAX_VALUE)
    bitsOf(Codecs.taggedDouble.decodeJson(parsedText("-1.7976931348623157e308")).value) shouldBe
      bitsOf(-java.lang.Double.MAX_VALUE)
    bitsOf(Codecs.taggedDouble.decodeJson(parsedText("4.9e-324")).value) shouldBe
      bitsOf(java.lang.Double.MIN_VALUE)
  }

  test("taggedDouble reads a magnitude too small for a double as a zero") {
    // a magnitude below the smallest positive value converts to a zero, which is what reading
    // the same text as a double produces anywhere else, and stays inside the finite range this
    // codec is defined over - unlike a magnitude beyond the top of the range, which does not
    bitsOf(Codecs.taggedDouble.decodeJson(parsedText("1e-999")).value) shouldBe bitsOf(0.0)
    bitsOf(Codecs.taggedDouble.decodeJson(parsedText("-1e-999")).value) shouldBe bitsOf(-0.0)
  }

  test("taggedDouble rejects an absent value") {
    Codecs.taggedDouble.decodeJson(Json.Null).isLeft shouldBe true
  }

  test("taggedDouble rejects a boolean, an array and an object") {
    Codecs.taggedDouble.decodeJson(Json.True).isLeft shouldBe true
    Codecs.taggedDouble.decodeJson(Json.False).isLeft shouldBe true
    Codecs.taggedDouble.decodeJson(Json.arr()).isLeft shouldBe true
    Codecs.taggedDouble.decodeJson(Json.obj()).isLeft shouldBe true
  }

  test("taggedDouble reports the representation it expected when it rejects a payload") {
    val outcome = Codecs.taggedDouble.decodeJson(Json.True)
    outcome.left.value.message shouldBe
      "Expected a JSON number or one of the strings NaN, Infinity, -Infinity"
  }

  test("taggedDouble positions a rejected payload at the cursor it was decoding") {
    val payload = Json.obj("factor" -> Json.fromString("rubbish"))
    val outcome = payload.hcursor.downField("factor").as[Double](Codecs.taggedDouble)
    outcome.left.value.history shouldBe List[CursorOp](CursorOp.DownField("factor"))
  }

  test("taggedDouble round-trips every generated double exactly, in memory and as text") {
    forAll(Arbitraries.genDouble) { (value: Double) =>
      val written = Codecs.taggedDouble(value)
      bitsOf(Codecs.taggedDouble.decodeJson(written).value) shouldBe bitsOf(value)
      bitsOf(Codecs.taggedDouble.decodeJson(reparse(written)).value) shouldBe bitsOf(value)
    }
  }

  test("taggedDouble round-trips every generated IEEE-754 edge value exactly") {
    forAll(Arbitraries.genEdgeDouble) { (value: Double) =>
      bitsOf(Codecs.taggedDouble.decodeJson(reparse(Codecs.taggedDouble(value))).value) shouldBe bitsOf(value)
    }
  }

  //-------------------------------------------------------------------------
  // dayOfWeekCodec: the one date and time type the JSON library does not cover
  //-------------------------------------------------------------------------
  /** The seven days, each with the constant name it is written as. */
  private val daysOfWeek = Table(
    ("day", "name"),
    (DayOfWeek.MONDAY, "MONDAY"),
    (DayOfWeek.TUESDAY, "TUESDAY"),
    (DayOfWeek.WEDNESDAY, "WEDNESDAY"),
    (DayOfWeek.THURSDAY, "THURSDAY"),
    (DayOfWeek.FRIDAY, "FRIDAY"),
    (DayOfWeek.SATURDAY, "SATURDAY"),
    (DayOfWeek.SUNDAY, "SUNDAY"))

  /** Text that names a day to a reader but is not one of the seven constant names. */
  private val rejectedDayTexts = Table(
    "text",
    "Saturday",
    "saturday",
    "SAT",
    "Sat",
    "SATURDAYS",
    "SAT/SUN",
    "8",
    "6",
    "")

  test("the JSON library publishes no instance for a day of the week") {
    // this absence is the entire reason dayOfWeekCodec exists: the library covers the date, the
    // time of day, the time zone, the period and the year with month, and stops there
    assertDoesNotCompile("implicitly[io.circe.Decoder[java.time.DayOfWeek]]")
    assertDoesNotCompile("implicitly[io.circe.Encoder[java.time.DayOfWeek]]")
  }

  test("dayOfWeekCodec encodes every day as its constant name") {
    forAll(daysOfWeek) { (day: DayOfWeek, name: String) =>
      Codecs.dayOfWeekCodec(day) shouldBe Json.fromString(name)
    }
  }

  test("dayOfWeekCodec decodes every constant name back to its day") {
    forAll(daysOfWeek) { (day: DayOfWeek, name: String) =>
      Codecs.dayOfWeekCodec.decodeJson(Json.fromString(name)) shouldBe Right(day)
    }
  }

  test("dayOfWeekCodec round-trips every day through the text form") {
    forAll(daysOfWeek) { (day: DayOfWeek, name: String) =>
      Codecs.dayOfWeekCodec(day).noSpaces shouldBe "\"" + name + "\""
      Codecs.dayOfWeekCodec.decodeJson(reparse(Codecs.dayOfWeekCodec(day))) shouldBe Right(day)
    }
  }

  test("dayOfWeekCodec rejects text that is not exactly one of the seven constant names") {
    forAll(rejectedDayTexts) { (text: String) =>
      Codecs.dayOfWeekCodec.decodeJson(Json.fromString(text)).isLeft shouldBe true
    }
  }

  test("dayOfWeekCodec rejects a JSON value that is not a string") {
    forAll(nonStringPayloads) { (payload: Json) =>
      Codecs.dayOfWeekCodec.decodeJson(payload).isLeft shouldBe true
    }
  }

  test("dayOfWeekCodec reports the seven names it expects when it rejects a payload") {
    val outcome = Codecs.dayOfWeekCodec.decodeJson(Json.fromString("Saturday"))
    outcome.left.value.message shouldBe
      "Expected one of the day names MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY"
  }


  //-------------------------------------------------------------------------
  // doubleArrayCodec and doubleMatrixCodec: a JSON array, and an array of rows
  //-------------------------------------------------------------------------
  /**
   * A JSON array of numbers, as a payload of this spec.
   *
   * @param values  the numbers of the array, every one of which must be finite
   * @return the JSON array holding them
   */
  private def numbers(values: Double*): Json =
    Json.fromValues(values.map(value => Json.fromDoubleOrNull(value)))

  /**
   * A JSON array of a stated length, as a payload of this spec.
   *
   * The elements are one and the same JSON value repeated, which is what makes a payload at
   * the ceiling affordable to build: the element is built once and the array holds that one
   * value in every position, so the payload itself costs a run of references and nothing more.
   * What the decoder does with that stated length is precisely what is under test.
   *
   * The element is bound before the array is filled deliberately. The filling evaluates what
   * it is given once per position, so an element expression written inline would allocate a
   * distinct value per position and a payload at the ceiling would cost the elements it was
   * meant to state without holding.
   *
   * @param length  the number of elements the array states
   * @return the JSON array of that length
   */
  private def arrayPayload(length: Int): Json = {
    val element = Json.fromDoubleOrNull(1.0)
    Json.fromValues(Vector.fill(length)(element))
  }

  /**
   * A JSON array of rows of a stated shape, as a payload of this spec.
   *
   * Built the same way and for the same reason as `arrayPayload`, and with the row bound
   * before the filling for the same reason the element is: one row value is built and repeated,
   * so a payload stating a shape far larger than anything the port reads costs two runs of
   * references rather than the elements it names.
   *
   * @param rows  the number of rows the payload states
   * @param columns  the number of elements each of those rows states
   * @return the JSON array of rows of that shape
   */
  private def rowsPayload(rows: Int, columns: Int): Json = {
    val row = arrayPayload(columns)
    Json.fromValues(Vector.fill(rows)(row))
  }

  /** Payloads whose elements the array codec cannot read. */
  private val arraysWithBadElements = Table(
    "payload",
    Json.arr(Json.fromDoubleOrNull(1.0), Json.True),
    Json.arr(Json.fromDoubleOrNull(1.0), Json.fromString("1.5")),
    Json.arr(Json.fromDoubleOrNull(1.0), Json.fromString("rubbish")),
    Json.arr(Json.fromDoubleOrNull(1.0), Json.Null),
    Json.arr(Json.fromDoubleOrNull(1.0), Json.arr()),
    Json.arr(Json.fromDoubleOrNull(1.0), Json.obj()))

  test("doubleArrayCodec encodes an array as a JSON array of numbers") {
    val array = DoubleArray.of(1.0, 2.5, -3.0)
    Codecs.doubleArrayCodec(array) shouldBe numbers(1.0, 2.5, -3.0)
    Codecs.doubleArrayCodec(array).noSpaces shouldBe "[1.0,2.5,-3.0]"
  }

  test("doubleArrayCodec encodes an element JSON cannot express as its tag") {
    val array = DoubleArray.of(1.0, Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity)
    Codecs.doubleArrayCodec(array).noSpaces shouldBe """[1.0,"NaN","Infinity","-Infinity"]"""
  }

  test("doubleArrayCodec encodes an empty array as an empty JSON array") {
    Codecs.doubleArrayCodec(DoubleArray.EMPTY).noSpaces shouldBe "[]"
    Codecs.doubleArrayCodec(DoubleArray.of()).noSpaces shouldBe "[]"
  }

  test("doubleArrayCodec decodes an empty JSON array as the empty array") {
    Codecs.doubleArrayCodec.decodeJson(Json.arr()) shouldBe Right(DoubleArray.EMPTY)
    Codecs.doubleArrayCodec.decodeJson(Json.arr()).value.isEmpty shouldBe true
  }

  test("doubleArrayCodec decodes a JSON array of numbers into an array of the same elements") {
    Codecs.doubleArrayCodec.decodeJson(numbers(10.0, 20.0, 30.0)) shouldBe
      Right(DoubleArray.of(10.0, 20.0, 30.0))
  }

  test("doubleArrayCodec decodes a tagged element back to the value it names") {
    val payload = Json.arr(
      Json.fromDoubleOrNull(1.0),
      Json.fromString("NaN"),
      Json.fromString("Infinity"),
      Json.fromString("-Infinity"))
    Codecs.doubleArrayCodec.decodeJson(payload) shouldBe
      Right(DoubleArray.of(1.0, Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity))
  }

  test("doubleArrayCodec round-trips an array holding both ordinary and IEEE-754 edge elements") {
    // the equality of this type compares bit patterns, so a value that is not a number equals
    // itself here and the two signed zeroes do not - which is what makes this assertion strict
    val array = DoubleArray.of(1.5, Double.NaN, -0.0, 0.0, Double.PositiveInfinity, Double.NegativeInfinity)
    Codecs.doubleArrayCodec.decodeJson(Codecs.doubleArrayCodec(array)) shouldBe Right(array)
    Codecs.doubleArrayCodec.decodeJson(reparse(Codecs.doubleArrayCodec(array))) shouldBe Right(array)
  }

  test("doubleArrayCodec reproduces the elements of an array rather than approximating them") {
    val array = DoubleArray.of(0.1, 1.0 / 3.0, java.lang.Double.MIN_VALUE)
    val decoded = Codecs.doubleArrayCodec.decodeJson(reparse(Codecs.doubleArrayCodec(array))).value
    decoded shouldBe array
    decoded should not be DoubleArray.of(Math.nextUp(0.1), 1.0 / 3.0, java.lang.Double.MIN_VALUE)
  }

  test("doubleArrayCodec keeps the order of the elements of an array") {
    val array = DoubleArray.of(3.0, 1.0, 2.0)
    Codecs.doubleArrayCodec(array).noSpaces shouldBe "[3.0,1.0,2.0]"
    Codecs.doubleArrayCodec.decodeJson(Codecs.doubleArrayCodec(array)).value.toList shouldBe
      List(3.0, 1.0, 2.0)
  }

  test("doubleArrayCodec rejects a JSON value that is not an array") {
    forAll(nonArrayPayloads) { (payload: Json) =>
      Codecs.doubleArrayCodec.decodeJson(payload).isLeft shouldBe true
    }
  }

  test("doubleArrayCodec rejects an array holding an element it cannot read") {
    forAll(arraysWithBadElements) { (payload: Json) =>
      Codecs.doubleArrayCodec.decodeJson(payload).isLeft shouldBe true
    }
  }

  test("doubleArrayCodec rejects an array holding an element no double can hold") {
    // the elements go through the double policy, so the refusal of an over-range number is
    // inherited here rather than restated
    val outcome = Codecs.doubleArrayCodec.decodeJson(parsedText("[1.0,1e999]"))
    outcome.left.value.message shouldBe
      "Expected a JSON number a double can hold; a magnitude beyond that range is written as " +
        "the string Infinity or -Infinity"
    outcome.left.value.history shouldBe List[CursorOp](CursorOp.MoveRight, CursorOp.DownArray)
  }

  test("doubleArrayCodec accepts an array holding as many elements as it reads") {
    // the ceiling is set above every length the port itself produces, so the length exactly at
    // it is a length that decodes: this is the assertion that would fail if a later revision
    // lowered the figure to one the library's own values can reach
    val payload = arrayPayload(Codecs.MaximumArrayElements)
    val decoded = Codecs.doubleArrayCodec.decodeJson(payload).value
    decoded.size shouldBe Codecs.MaximumArrayElements
    decoded.get(0) shouldBe 1.0
    decoded.get(Codecs.MaximumArrayElements - 1) shouldBe 1.0
  }

  test("doubleArrayCodec rejects an array stating more elements than it reads") {
    // the length is the document's to state, so it is measured against the ceiling before any
    // element is read and nothing is allocated for a payload beyond it
    val payload = arrayPayload(Codecs.MaximumArrayElements + 1)
    val outcome = Codecs.doubleArrayCodec.decodeJson(payload)
    outcome.left.value.message shouldBe
      s"Expected at most ${Codecs.MaximumArrayElements} elements in the array, " +
        s"but the payload states ${Codecs.MaximumArrayElements + 1}"
  }

  test("doubleArrayCodec refuses an array beyond the ceiling without reading its elements") {
    // the answer does not depend on the elements: a payload beyond the ceiling whose every
    // element is unreadable is still refused for its length, which is what shows the length is
    // measured before anything is read, and therefore before anything is allocated for it
    val payload = Json.fromValues(Vector.fill(Codecs.MaximumArrayElements + 1)(Json.fromString("rubbish")))
    val outcome = Codecs.doubleArrayCodec.decodeJson(payload)
    outcome.left.value.message shouldBe
      s"Expected at most ${Codecs.MaximumArrayElements} elements in the array, " +
        s"but the payload states ${Codecs.MaximumArrayElements + 1}"
  }

  test("doubleArrayCodec positions an array beyond the ceiling at the cursor it was decoding") {
    val payload = Json.obj("series" -> arrayPayload(Codecs.MaximumArrayElements + 1))
    val outcome = payload.hcursor.downField("series").as[DoubleArray](Codecs.doubleArrayCodec)
    outcome.left.value.history shouldBe List[CursorOp](CursorOp.DownField("series"))
  }

  test("doubleArrayCodec round-trips every generated array exactly") {
    forAll(Arbitraries.genDoubleArray) { (array: DoubleArray) =>
      Codecs.doubleArrayCodec.decodeJson(Codecs.doubleArrayCodec(array)) shouldBe Right(array)
    }
  }

  test("doubleArrayCodec round-trips every generated array through the text form") {
    forAll(Arbitraries.genDoubleArray) { (array: DoubleArray) =>
      Codecs.doubleArrayCodec.decodeJson(reparse(Codecs.doubleArrayCodec(array))) shouldBe Right(array)
    }
  }

  //-------------------------------------------------------------------------
  test("doubleMatrixCodec encodes a matrix as a JSON array of row arrays") {
    val matrix = DoubleMatrix.of(2, 3, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    Codecs.doubleMatrixCodec(matrix).noSpaces shouldBe "[[1.0,2.0,3.0],[4.0,5.0,6.0]]"
    Codecs.doubleMatrixCodec(matrix) shouldBe Json.arr(numbers(1.0, 2.0, 3.0), numbers(4.0, 5.0, 6.0))
  }

  test("doubleMatrixCodec encodes a single column as one row array for each element") {
    val matrix = DoubleMatrix.of(3, 1, 1.0, 2.0, 3.0)
    Codecs.doubleMatrixCodec(matrix).noSpaces shouldBe "[[1.0],[2.0],[3.0]]"
  }

  test("doubleMatrixCodec encodes an element JSON cannot express as its tag") {
    val matrix = DoubleMatrix.of(1, 2, 1.0, Double.NegativeInfinity)
    Codecs.doubleMatrixCodec(matrix).noSpaces shouldBe """[[1.0,"-Infinity"]]"""
  }

  test("doubleMatrixCodec encodes the empty matrix as an empty JSON array") {
    // the type collapses a shape with no row or no column onto the empty matrix, so all four
    // of these are the same value and there is no shape whose row count survives without rows
    Codecs.doubleMatrixCodec(DoubleMatrix.EMPTY).noSpaces shouldBe "[]"
    Codecs.doubleMatrixCodec(DoubleMatrix.of()).noSpaces shouldBe "[]"
    Codecs.doubleMatrixCodec(DoubleMatrix.of(1, 0)).noSpaces shouldBe "[]"
    Codecs.doubleMatrixCodec(DoubleMatrix.of(0, 1)).noSpaces shouldBe "[]"
  }

  test("doubleMatrixCodec decodes an empty JSON array as the empty matrix") {
    val decoded = Codecs.doubleMatrixCodec.decodeJson(Json.arr()).value
    decoded shouldBe DoubleMatrix.EMPTY
    decoded.rowCount shouldBe 0
    decoded.columnCount shouldBe 0
  }

  test("doubleMatrixCodec decodes an array of row arrays into a matrix of that shape") {
    val decoded = Codecs.doubleMatrixCodec.decodeJson(Json.arr(numbers(1.0, 2.0, 3.0), numbers(4.0, 5.0, 6.0))).value
    decoded shouldBe DoubleMatrix.of(2, 3, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    decoded.rowCount shouldBe 2
    decoded.columnCount shouldBe 3
  }

  test("doubleMatrixCodec decodes a tagged element back to the value it names") {
    val payload = Json.arr(Json.arr(Json.fromDoubleOrNull(1.0), Json.fromString("NaN")))
    Codecs.doubleMatrixCodec.decodeJson(payload) shouldBe Right(DoubleMatrix.of(1, 2, 1.0, Double.NaN))
  }

  test("doubleMatrixCodec round-trips a matrix holding IEEE-754 edge elements through the text form") {
    val matrix = DoubleMatrix.of(2, 2, Double.NaN, -0.0, Double.PositiveInfinity, 2.5)
    Codecs.doubleMatrixCodec.decodeJson(reparse(Codecs.doubleMatrixCodec(matrix))) shouldBe Right(matrix)
  }

  test("doubleMatrixCodec rejects rows that do not all hold the same number of elements") {
    val ragged = Json.arr(numbers(1.0, 2.0), numbers(3.0))
    val outcome = Codecs.doubleMatrixCodec.decodeJson(ragged)
    outcome.left.value.message shouldBe
      "Expected every row of the matrix to hold the same number of elements"
  }

  test("doubleMatrixCodec rejects a longer row after a shorter one") {
    val ragged = Json.arr(numbers(1.0), numbers(2.0, 3.0))
    Codecs.doubleMatrixCodec.decodeJson(ragged).isLeft shouldBe true
  }

  test("doubleMatrixCodec rejects an empty row beside a row holding elements") {
    val ragged = Json.arr(numbers(1.0, 2.0), Json.arr())
    Codecs.doubleMatrixCodec.decodeJson(ragged).isLeft shouldBe true
  }

  test("doubleMatrixCodec rejects an empty row stated before a row holding elements") {
    // this is the payload whose two answers are told apart by the order the shape is settled
    // in: the widths are measured across every row before any dimension is taken from one of
    // them, so the disagreement between an empty row and a row of one element is seen for what
    // it is. A shape read from the first row alone would have called this matrix zero columns
    // wide and answered with the empty matrix, discarding the element the document states
    val ragged = Json.arr(Json.arr(), numbers(1.0))
    val outcome = Codecs.doubleMatrixCodec.decodeJson(ragged)
    outcome.left.value.message shouldBe
      "Expected every row of the matrix to hold the same number of elements"
    outcome.left.value.history shouldBe List.empty[CursorOp]
  }

  test("doubleMatrixCodec decodes rows that are all empty as the empty matrix") {
    // rows that agree on holding nothing describe a rectangle of no columns, and the type
    // collapses every such shape onto the single empty matrix; the contrast with the payload
    // above is that these rows agree, so there is nothing to refuse
    Codecs.doubleMatrixCodec.decodeJson(Json.arr(Json.arr(), Json.arr())).value shouldBe DoubleMatrix.EMPTY
  }

  test("doubleMatrixCodec rejects a JSON value that is not an array") {
    forAll(nonArrayPayloads) { (payload: Json) =>
      Codecs.doubleMatrixCodec.decodeJson(payload).isLeft shouldBe true
    }
  }

  test("doubleMatrixCodec rejects a row that is not an array") {
    forAll(nonArrayPayloads) { (payload: Json) =>
      Codecs.doubleMatrixCodec.decodeJson(Json.arr(payload)).isLeft shouldBe true
    }
  }

  test("doubleMatrixCodec rejects a row holding an element it cannot read") {
    forAll(arraysWithBadElements) { (payload: Json) =>
      Codecs.doubleMatrixCodec.decodeJson(Json.arr(payload)).isLeft shouldBe true
    }
  }

  test("doubleMatrixCodec rejects a row holding an element no double can hold") {
    Codecs.doubleMatrixCodec.decodeJson(parsedText("[[1.0,1e999]]")).left.value.message shouldBe
      "Expected a JSON number a double can hold; a magnitude beyond that range is written as " +
        "the string Infinity or -Infinity"
  }

  test("doubleMatrixCodec refuses rows that disagree without reading the elements of any of them") {
    // the shape is read from the payload as it stands, so raggedness is settled before a single
    // element is read: a payload that is both ragged and holds an unreadable element is refused
    // for its shape, which is the answer that did not depend on reading anything
    val outcome = Codecs.doubleMatrixCodec.decodeJson(parsedText("""[[1.0,"rubbish"],[3.0]]"""))
    outcome.left.value.message shouldBe
      "Expected every row of the matrix to hold the same number of elements"
    outcome.left.value.history shouldBe List.empty[CursorOp]
  }

  test("doubleMatrixCodec accepts a matrix stating as many elements as it reads") {
    val side = 1024
    side * side shouldBe Codecs.MaximumMatrixElements
    val decoded = Codecs.doubleMatrixCodec.decodeJson(rowsPayload(side, side)).value
    decoded.rowCount shouldBe side
    decoded.columnCount shouldBe side
  }

  test("doubleMatrixCodec rejects a matrix stating more rows than it reads") {
    val payload = rowsPayload(Codecs.MaximumMatrixRows + 1, 1)
    val outcome = Codecs.doubleMatrixCodec.decodeJson(payload)
    outcome.left.value.message shouldBe
      s"Expected at most ${Codecs.MaximumMatrixRows} rows in the matrix, " +
        s"but the payload states ${Codecs.MaximumMatrixRows + 1}"
  }

  test("doubleMatrixCodec rejects a matrix stating a row longer than it reads") {
    val payload = rowsPayload(1, Codecs.MaximumMatrixColumns + 1)
    val outcome = Codecs.doubleMatrixCodec.decodeJson(payload)
    outcome.left.value.message shouldBe
      s"Expected at most ${Codecs.MaximumMatrixColumns} elements in each row of the matrix, " +
        s"but the payload states ${Codecs.MaximumMatrixColumns + 1}"
  }

  test("doubleMatrixCodec rejects a matrix whose dimensions are each acceptable but whose product is not") {
    // neither dimension is beyond its own ceiling here; what is beyond a ceiling is the number
    // of elements the two of them together name, which is what would have been allocated
    val payload = rowsPayload(Codecs.MaximumMatrixRows, Codecs.MaximumMatrixColumns)
    val elements = Codecs.MaximumMatrixRows.toLong * Codecs.MaximumMatrixColumns.toLong
    val outcome = Codecs.doubleMatrixCodec.decodeJson(payload)
    outcome.left.value.message shouldBe
      s"Expected at most ${Codecs.MaximumMatrixElements} elements in the matrix, " +
        s"but the payload states $elements"
  }

  test("doubleMatrixCodec refuses a shape whose element count would not fit in the width of a count") {
    // a square of this side names more elements than an Int can hold, and an Int product of the
    // two dimensions would be zero - the first line below is that arithmetic, stated so the
    // reader can see what is being guarded against. The refusal names the row ceiling because
    // the row count is compared first, which is what keeps the wrapped arithmetic unreachable:
    // no shape whose product could wrap gets as far as the product at all, and the product is
    // computed in the wider width regardless
    val side = 65536
    side.toLong * side.toLong shouldBe 4294967296L
    side * side shouldBe 0
    val outcome = Codecs.doubleMatrixCodec.decodeJson(rowsPayload(side, side))
    outcome.left.value.message shouldBe
      s"Expected at most ${Codecs.MaximumMatrixRows} rows in the matrix, but the payload states $side"
  }

  test("doubleMatrixCodec refuses a matrix beyond the row ceiling without reading its rows") {
    // as with the array, the refusal stands even though not one of the rows could have been
    // read, so the shape is settled from the payload rather than from what reading it produced
    val payload = Json.fromValues(Vector.fill(Codecs.MaximumMatrixRows + 1)(Json.fromString("rubbish")))
    val outcome = Codecs.doubleMatrixCodec.decodeJson(payload)
    outcome.left.value.message shouldBe
      s"Expected at most ${Codecs.MaximumMatrixRows} rows in the matrix, " +
        s"but the payload states ${Codecs.MaximumMatrixRows + 1}"
  }

  test("doubleMatrixCodec positions a matrix beyond a ceiling at the cursor it was decoding") {
    val payload = Json.obj("grid" -> rowsPayload(Codecs.MaximumMatrixRows + 1, 1))
    val outcome = payload.hcursor.downField("grid").as[DoubleMatrix](Codecs.doubleMatrixCodec)
    outcome.left.value.history shouldBe List[CursorOp](CursorOp.DownField("grid"))
  }

  test("doubleMatrixCodec accepts a payload of many empty rows only up to the row ceiling") {
    // a row holding nothing still costs a row, so the row ceiling applies to a shape whose
    // element count is zero
    Codecs.doubleMatrixCodec.decodeJson(rowsPayload(Codecs.MaximumMatrixRows, 0)).value shouldBe
      DoubleMatrix.EMPTY
    Codecs.doubleMatrixCodec.decodeJson(rowsPayload(Codecs.MaximumMatrixRows + 1, 0)).isLeft shouldBe true
  }

  test("the published ceilings are the figures the documentation of each of them states") {
    // the figures are part of the public surface of this object, so they are pinned here: a
    // change to any of them is a change to what documents this port reads, and has to be a
    // deliberate edit of this line rather than a side effect of one somewhere else
    Codecs.MaximumArrayElements shouldBe 1048576
    Codecs.MaximumMatrixRows shouldBe 4096
    Codecs.MaximumMatrixColumns shouldBe 4096
    Codecs.MaximumMatrixElements shouldBe 1048576
    // one array's worth of elements is the most a single numeric payload can ask for, whichever
    // of the two shapes it arrives in
    Codecs.MaximumMatrixElements shouldBe Codecs.MaximumArrayElements
    // the collection ceiling is the figure the library already enforces on its own expansions,
    // a hundred thousand periods of a generated schedule and a hundred thousand steps of a
    // resolved sequence, so no collection any value of this port holds can exceed it
    Codecs.MaximumCollectionElements shouldBe 100000
  }

  test("doubleMatrixCodec round-trips every generated matrix exactly") {
    forAll(Arbitraries.genDoubleMatrix) { (matrix: DoubleMatrix) =>
      Codecs.doubleMatrixCodec.decodeJson(Codecs.doubleMatrixCodec(matrix)) shouldBe Right(matrix)
    }
  }

  test("doubleMatrixCodec round-trips every generated matrix through the text form") {
    forAll(Arbitraries.genDoubleMatrix) { (matrix: DoubleMatrix) =>
      Codecs.doubleMatrixCodec.decodeJson(reparse(Codecs.doubleMatrixCodec(matrix))) shouldBe Right(matrix)
    }
  }

  //-------------------------------------------------------------------------
  // boundedElements and boundedFields: the ceiling a collection of a value is read under
  //-------------------------------------------------------------------------
  /** The elements a bounded collection holds, standing in for the collection of any type. */
  private val elementsDecoder: Decoder[List[Int]] = Decoder.decodeList[Int]

  /** The name the refusals of `boundedElements` use below, as a type of this port would. */
  private val BoundedWhat: String = "dates in the calendar"

  /** A collection bounded at a limit small enough to state a payload beyond it inline. */
  private val boundedThree: Decoder[List[Int]] = Codecs.boundedElements(BoundedWhat, 3)(elementsDecoder)

  /** The object shape the field-bounding tests read, two collections under known names. */
  private val fieldsDecoder: Decoder[Map[String, List[Int]]] = Decoder.decodeMap[String, List[Int]]

  /** The same shape with a limit on each of its two collections, `left` examined first. */
  private val boundedLeftThenRight: Decoder[Map[String, List[Int]]] =
    Codecs.boundedFields("left" -> 2, "right" -> 3)(fieldsDecoder)

  /** The same two limits in the other order, which is what makes the order observable. */
  private val boundedRightThenLeft: Decoder[Map[String, List[Int]]] =
    Codecs.boundedFields("right" -> 3, "left" -> 2)(fieldsDecoder)

  /**
   * A JSON array of consecutive whole numbers, as a payload of these tests.
   *
   * @param length  the number of elements the array states
   * @return the JSON array holding the numbers from one upwards
   */
  private def wholeNumbers(length: Int): Json =
    Json.fromValues(Vector.tabulate(length)(index => Json.fromInt(index + 1)))

  /**
   * A JSON array of a stated length whose every element is unreadable as a number.
   *
   * This is what shows a ceiling to be applied before the payload is read: a decoder that
   * reached the elements of one of these would report an element, so a refusal that names the
   * count instead is a refusal reached without them.
   *
   * @param length  the number of elements the array states
   * @return the JSON array of that length, holding text in every position
   */
  private def rubbishElements(length: Int): Json = {
    val element = Json.fromString("rubbish")
    Json.fromValues(Vector.fill(length)(element))
  }

  test("boundedElements decodes a payload within its limit exactly as the decoder it wraps") {
    boundedThree.decodeJson(wholeNumbers(0)) shouldBe Right(List.empty[Int])
    boundedThree.decodeJson(wholeNumbers(1)) shouldBe Right(List(1))
    boundedThree.decodeJson(wholeNumbers(3)) shouldBe Right(List(1, 2, 3))
  }

  test("boundedElements rejects a payload stating more elements than its limit") {
    val outcome = boundedThree.decodeJson(wholeNumbers(4))
    outcome.left.value.message shouldBe
      s"Expected at most 3 $BoundedWhat, but the payload states 4"
  }

  test("boundedElements refuses a payload beyond its limit without reading its elements") {
    // every element here is unreadable, and the refusal still names the count: the count is
    // taken from the JSON as it stands, so nothing the wrapped decoder would have allocated is
    // allocated for a payload this refuses
    val outcome = boundedThree.decodeJson(rubbishElements(4))
    outcome.left.value.message shouldBe
      s"Expected at most 3 $BoundedWhat, but the payload states 4"
    // the same payload within the limit is refused by the wrapped decoder instead, at the
    // element it could not read, which is what the ceiling is being kept clear of
    val readable = boundedThree.decodeJson(rubbishElements(3))
    readable.left.value.message shouldBe "Int"
  }

  test("boundedElements positions its refusal at the cursor it was decoding") {
    val payload = Json.obj("holidays" -> wholeNumbers(4))
    val outcome = payload.hcursor.downField("holidays").as[List[Int]](boundedThree)
    outcome.left.value.message shouldBe
      s"Expected at most 3 $BoundedWhat, but the payload states 4"
    outcome.left.value.history shouldBe List[CursorOp](CursorOp.DownField("holidays"))
  }

  test("boundedElements passes a payload that is not an array to the decoder it wraps") {
    // a value with no elements to count has nothing for the ceiling to say about it, so what
    // is wrong with it is reported by the wrapped decoder, in its own words and at its own
    // position, exactly as it would be without the wrapper
    forAll(nonArrayPayloads) { (payload: Json) =>
      val wrapped = boundedThree.decodeJson(payload)
      val unwrapped = elementsDecoder.decodeJson(payload)
      wrapped.left.value.message shouldBe unwrapped.left.value.message
      wrapped.left.value.history shouldBe unwrapped.left.value.history
    }
  }

  test("boundedElements applies the published collection ceiling to every collection the port can build") {
    // the ceiling a type of this port actually passes is the published one, and it is above
    // every collection the library's own factories produce, so a payload at it decodes and the
    // one element beyond it is refused
    val bounded = Codecs.boundedElements("entries", Codecs.MaximumCollectionElements)(elementsDecoder)
    bounded.decodeJson(wholeNumbers(Codecs.MaximumCollectionElements)).value.size shouldBe
      Codecs.MaximumCollectionElements
    val outcome = bounded.decodeJson(rubbishElements(Codecs.MaximumCollectionElements + 1))
    outcome.left.value.message shouldBe
      s"Expected at most ${Codecs.MaximumCollectionElements} entries, " +
        s"but the payload states ${Codecs.MaximumCollectionElements + 1}"
  }

  test("boundedFields decodes an object whose named fields are within their limits") {
    val payload = Json.obj("left" -> wholeNumbers(2), "right" -> wholeNumbers(3))
    boundedLeftThenRight.decodeJson(payload) shouldBe
      Right(Map("left" -> List(1, 2), "right" -> List(1, 2, 3)))
  }

  test("boundedFields rejects an object whose named field states more elements than its limit") {
    val payload = Json.obj("left" -> wholeNumbers(3), "right" -> wholeNumbers(3))
    val outcome = boundedLeftThenRight.decodeJson(payload)
    outcome.left.value.message shouldBe
      "Expected at most 2 elements in the left field, but the payload states 3"
    outcome.left.value.history shouldBe List[CursorOp](CursorOp.DownField("left"))
  }

  test("boundedFields positions its refusal at the field, however deeply the object is nested") {
    val payload = Json.obj("calendar" -> Json.obj("left" -> wholeNumbers(3)))
    val outcome = payload.hcursor.downField("calendar").as[Map[String, List[Int]]](boundedLeftThenRight)
    outcome.left.value.history shouldBe
      List[CursorOp](CursorOp.DownField("left"), CursorOp.DownField("calendar"))
  }

  test("boundedFields refuses a field beyond its limit without reading its elements") {
    val payload = Json.obj("left" -> rubbishElements(3))
    val outcome = boundedLeftThenRight.decodeJson(payload)
    outcome.left.value.message shouldBe
      "Expected at most 2 elements in the left field, but the payload states 3"
  }

  test("boundedFields reports the first of the named fields that exceeds its limit") {
    // both fields are beyond their limits, and which one is reported is the order the limits
    // were given in: one refusal naming one field, rather than a list to be interpreted
    val payload = Json.obj("left" -> wholeNumbers(3), "right" -> wholeNumbers(4))
    boundedLeftThenRight.decodeJson(payload).left.value.message shouldBe
      "Expected at most 2 elements in the left field, but the payload states 3"
    boundedRightThenLeft.decodeJson(payload).left.value.message shouldBe
      "Expected at most 3 elements in the right field, but the payload states 4"
  }

  test("boundedFields passes over a field the object does not hold") {
    val payload = Json.obj("right" -> wholeNumbers(3))
    boundedLeftThenRight.decodeJson(payload) shouldBe Right(Map("right" -> List(1, 2, 3)))
  }

  test("boundedFields passes over a field whose value is not an array") {
    // the field holds no elements to count, so the wrapped decoder reports it exactly as it
    // would without the wrapper
    val payload = Json.obj("left" -> Json.fromString("rubbish"))
    val wrapped = boundedLeftThenRight.decodeJson(payload)
    val unwrapped = fieldsDecoder.decodeJson(payload)
    wrapped.left.value.message shouldBe unwrapped.left.value.message
    wrapped.left.value.history shouldBe unwrapped.left.value.history
  }

  test("boundedFields passes over a payload holding none of the fields it names") {
    // a value that is not an object, and an object whose fields are other than the named ones,
    // both hold nothing for a limit to apply to, so every payload of this kind reaches the
    // wrapped decoder untouched - whether that decoder then succeeds or reports the payload
    forAll(nonArrayPayloads) { (payload: Json) =>
      val wrapped = boundedLeftThenRight.decodeJson(payload)
      val unwrapped = fieldsDecoder.decodeJson(payload)
      wrapped.map(fields => fields.keySet) shouldBe unwrapped.map(fields => fields.keySet)
      wrapped.left.toOption.map(failure => failure.message) shouldBe
        unwrapped.left.toOption.map(failure => failure.message)
    }
  }

  test("boundedFields with no limits is the decoder it wraps") {
    // the limits are a varargs list, so the empty list is expressible and has to mean exactly
    // nothing: this is what keeps a caller from having to special-case a type with no bounded
    // field at all
    val payload = Json.obj("left" -> wholeNumbers(9))
    Codecs.boundedFields[Map[String, List[Int]]]()(fieldsDecoder).decodeJson(payload) shouldBe
      fieldsDecoder.decodeJson(payload)
  }


  //-------------------------------------------------------------------------
  // dropNulls: a field that holds no value is absent rather than empty
  //-------------------------------------------------------------------------
  /** A value of the sample product holding nothing in any of its optional fields. */
  private val emptyConfig: SampleConfig = SampleConfig("alpha", None, None, SampleNote("tag", None))

  /** A value of the sample product holding something in every one of its optional fields. */
  private val fullConfig: SampleConfig =
    SampleConfig("alpha", Some("note"), Some(2), SampleNote("tag", Some("detail")))

  /** Generates the nested product, with and without its optional field. */
  private val genNote: Gen[SampleNote] = for {
    tag <- Gen.oneOf("tag", "other", "")
    detail <- Gen.option(Gen.oneOf("detail", ""))
  } yield SampleNote(tag, detail)

  /** Generates the sample product across every combination of present and absent fields. */
  private val genConfig: Gen[SampleConfig] = for {
    name <- Gen.oneOf("alpha", "beta", "")
    note <- Gen.option(Gen.oneOf("note", ""))
    size <- Gen.option(Gen.choose(-5, 5))
    nested <- genNote
  } yield SampleConfig(name, note, size, nested)

  /**
   * The JSON held by a field of an object.
   *
   * @param json  the object to read
   * @param name  the name of the field
   * @return the JSON the field holds
   */
  private def fieldOf(json: Json, name: String): Json = json.hcursor.downField(name).as[Json].value

  /**
   * Whether the JSON holds a field with an explicitly empty value, at any depth.
   *
   * The removal under test reaches nested objects, so the question it answers has to be asked
   * of the whole structure rather than of its outermost object alone.
   *
   * @param json  the JSON to search
   * @return whether any field at any depth holds an explicitly empty value
   */
  private def holdsEmptyField(json: Json): Boolean =
    json.asObject.exists(fields => fields.values.exists(value => value.isNull || holdsEmptyField(value))) ||
      json.asArray.exists(values => values.exists(value => holdsEmptyField(value)))

  /** The payload of the sample product with every optional field explicitly empty. */
  private val emptyConfigPayload: Json = Json.obj(
    "name" -> Json.fromString("alpha"),
    "note" -> Json.Null,
    "size" -> Json.Null,
    "nested" -> Json.obj("tag" -> Json.fromString("tag"), "detail" -> Json.Null))

  test("the derived encoder writes a field holding no value as an explicitly empty field") {
    val written = SampleConfig.rawEncoder(emptyConfig)
    keysOf(written) shouldBe List("name", "note", "size", "nested")
    written shouldBe emptyConfigPayload
    fieldOf(written, "note").isNull shouldBe true
    fieldOf(written, "size").isNull shouldBe true
    holdsEmptyField(written) shouldBe true
  }

  test("dropNulls omits a field holding no value") {
    val written = SampleConfig.encoder(emptyConfig)
    keysOf(written) shouldBe List("name", "nested")
    written.noSpaces shouldBe """{"name":"alpha","nested":{"tag":"tag"}}"""
  }

  test("dropNulls reaches a field holding no value inside a nested object") {
    // the wrapping is applied once, to the outermost encoder, and still reaches downwards:
    // this is the difference between the deep removal used here and a shallow one
    keysOf(fieldOf(SampleConfig.rawEncoder(emptyConfig), "nested")) shouldBe List("tag", "detail")
    keysOf(fieldOf(SampleConfig.encoder(emptyConfig), "nested")) shouldBe List("tag")
  }

  test("dropNulls keeps every field that holds a value") {
    val written = SampleConfig.encoder(fullConfig)
    keysOf(written) shouldBe List("name", "note", "size", "nested")
    written.noSpaces shouldBe
      """{"name":"alpha","note":"note","size":2,"nested":{"tag":"tag","detail":"detail"}}"""
  }

  test("dropNulls keeps a field holding an empty string, which is not the same as absence") {
    val bordering = SampleConfig("", Some(""), Some(0), SampleNote("", Some("")))
    val written = SampleConfig.encoder(bordering)
    keysOf(written) shouldBe List("name", "note", "size", "nested")
    written.noSpaces shouldBe """{"name":"","note":"","size":0,"nested":{"tag":"","detail":""}}"""
  }

  test("dropNulls leaves a value with no absent field exactly as the derived encoder wrote it") {
    SampleConfig.encoder(fullConfig) shouldBe SampleConfig.rawEncoder(fullConfig)
  }

  test("the derived decoder reads an absent field as holding no value") {
    val payload = parse("""{"name":"alpha","nested":{"tag":"tag"}}""").value
    Decoder[SampleConfig].decodeJson(payload) shouldBe Right(emptyConfig)
  }

  test("the derived decoder reads an explicitly empty field as holding no value") {
    Decoder[SampleConfig].decodeJson(emptyConfigPayload) shouldBe Right(emptyConfig)
    Decoder[SampleConfig].decodeJson(reparse(emptyConfigPayload)) shouldBe Right(emptyConfig)
  }

  test("a value round-trips through the encoder that omits absent fields") {
    forAll(genConfig) { (config: SampleConfig) =>
      Decoder[SampleConfig].decodeJson(SampleConfig.encoder(config)) shouldBe Right(config)
    }
  }

  test("a value round-trips through the encoder that omits absent fields, as text") {
    forAll(genConfig) { (config: SampleConfig) =>
      Decoder[SampleConfig].decodeJson(reparse(SampleConfig.encoder(config))) shouldBe Right(config)
    }
  }

  test("dropNulls changes nothing that the derived decoder reads") {
    forAll(genConfig) { (config: SampleConfig) =>
      Decoder[SampleConfig].decodeJson(SampleConfig.encoder(config)) shouldBe
        Decoder[SampleConfig].decodeJson(SampleConfig.rawEncoder(config))
    }
  }

  test("dropNulls writes no explicitly empty field for any generated value") {
    forAll(genConfig) { (config: SampleConfig) =>
      holdsEmptyField(SampleConfig.encoder(config)) shouldBe false
    }
  }

  //-------------------------------------------------------------------------
  // validatedDecoder and checkedDecoder: the factory of the type decides
  //-------------------------------------------------------------------------
  /**
   * The payload of a pair of bounds, legal or not.
   *
   * @param low  the lower bound the payload carries
   * @param high  the upper bound the payload carries
   * @return the payload
   */
  private def rawBoundsJson(low: Int, high: Int): Json = RawBounds.encoder(RawBounds(low, high))

  /** Payloads and what the validating factory says about each of them. */
  private val boundsPayloads = Table(
    ("low", "high", "messages"),
    (0, 0, List.empty[String]),
    (1, 3, List.empty[String]),
    (3, 1, List(SampleBounds.OrderMessage)),
    (-1, 3, List(SampleBounds.NegativeMessage)),
    (-1, -4, List(SampleBounds.NegativeMessage, SampleBounds.OrderMessage)))

  test("validatedDecoder builds a value through the validating factory of the type") {
    val decoded = SampleBounds.decoder.decodeJson(rawBoundsJson(1, 3)).value
    decoded.low shouldBe 1
    decoded.high shouldBe 3
  }

  test("validatedDecoder accepts a payload at the edge of the invariants") {
    val decoded = SampleBounds.decoder.decodeJson(rawBoundsJson(0, 0)).value
    decoded.low shouldBe 0
    decoded.high shouldBe 0
  }

  test("validatedDecoder reports the single cause its factory gave as that cause's own message") {
    val outcome = SampleBounds.decoder.decodeJson(rawBoundsJson(3, 1))
    outcome.left.value.message shouldBe "Bounds must not end below where they start"
  }

  test("validatedDecoder joins the messages of every cause with a semicolon and a space") {
    val outcome = SampleBounds.decoder.decodeJson(rawBoundsJson(-1, -4))
    outcome.left.value.message shouldBe
      "Bounds must not start below zero; Bounds must not end below where they start"
  }

  test("validatedDecoder reports the causes in the order the factory accumulated them") {
    forAll(boundsPayloads) { (low: Int, high: Int, messages: List[String]) =>
      val outcome = SampleBounds.decoder.decodeJson(rawBoundsJson(low, high))
      if (messages.isEmpty) {
        outcome.isRight shouldBe true
      } else {
        outcome.left.value.message shouldBe messages.mkString("; ")
      }
    }
  }

  test("validatedDecoder reports no position for a payload decoded at the root of a document") {
    val outcome = SampleBounds.decoder.decodeJson(rawBoundsJson(-1, -4))
    outcome.left.value.history shouldBe List.empty[CursorOp]
  }

  test("validatedDecoder reports the position of the cursor it was decoding") {
    val payload = Json.obj("bounds" -> rawBoundsJson(-1, -4))
    val outcome = payload.hcursor.downField("bounds").as[SampleBounds](SampleBounds.decoder)
    outcome.left.value.message shouldBe
      "Bounds must not start below zero; Bounds must not end below where they start"
    outcome.left.value.history shouldBe List[CursorOp](CursorOp.DownField("bounds"))
  }

  test("checkedDecoder builds a value through the checking factory of the type") {
    val decoded = SampleBounds.checkedDecoder.decodeJson(rawBoundsJson(2, 5)).value
    decoded.low shouldBe 2
    decoded.high shouldBe 5
  }

  test("checkedDecoder reports the message of its single failure unchanged") {
    val outcome = SampleBounds.checkedDecoder.decodeJson(rawBoundsJson(-1, -4))
    outcome.left.value.message shouldBe "Bounds must not start below zero"
  }

  test("checkedDecoder reports the position of the cursor it was decoding") {
    val payload = Json.obj("bounds" -> rawBoundsJson(3, 1))
    val outcome = payload.hcursor.downField("bounds").as[SampleBounds](SampleBounds.checkedDecoder)
    outcome.left.value.message shouldBe "Bounds must not end below where they start"
    outcome.left.value.history shouldBe List[CursorOp](CursorOp.DownField("bounds"))
  }

  test("the two validating decoders accept exactly the same payloads") {
    forAll(boundsPayloads) { (low: Int, high: Int, messages: List[String]) =>
      val accepted = messages.isEmpty
      SampleBounds.decoder.decodeJson(rawBoundsJson(low, high)).isRight shouldBe accepted
      SampleBounds.checkedDecoder.decodeJson(rawBoundsJson(low, high)).isRight shouldBe accepted
    }
  }

  test("validatedDecoder rejects a payload whose raw fields do not decode, with the message of the library") {
    val payload = Json.obj("low" -> Json.fromString("x"), "high" -> Json.fromInt(3))
    val outcome = SampleBounds.decoder.decodeJson(payload)
    // the failure comes from the derived decoder of the raw shape, not from the factory, so it
    // carries neither of the factory's messages and is positioned at the field that broke
    outcome.left.value.message should not include "Bounds"
    outcome.left.value.history shouldBe List[CursorOp](CursorOp.DownField("low"))
  }

  test("validatedDecoder rejects a payload that is missing a raw field") {
    val outcome = SampleBounds.decoder.decodeJson(Json.obj("low" -> Json.fromInt(1)))
    outcome.left.value.message shouldBe "Missing required field"
    outcome.left.value.history shouldBe List[CursorOp](CursorOp.DownField("high"))
  }

  test("validatedDecoder rejects a payload that is not an object at all") {
    forAll(nonStringPayloads) { (payload: Json) =>
      SampleBounds.decoder.decodeJson(payload).isLeft shouldBe true
    }
  }


  //-------------------------------------------------------------------------
  // The bridge to the JSON layer is where rejected text is neutralised
  //
  // A factory of this port quotes what it refused, so the message of a failure
  // carries text a payload supplied, as it arrived. Every helper above reports
  // such a failure through one bridge, and these cases pin what that bridge
  // does with the text: each message is rendered single-line and bounded, and
  // only so many messages are named, so neither the shape nor the size of a
  // decoding failure can be dictated by whoever wrote the document. The cases
  // that pin the ordinary readings - a single cause as its own message, two
  // causes joined by a semicolon and a space, the position of the cursor - are
  // the ones above, and they are unaffected: ordinary text renders to itself.
  //-------------------------------------------------------------------------
  /**
   * The greatest number of causes one decoding failure names, before the overflow marker.
   *
   * The constant itself is private to `Codecs`, as the bound is its own business. It is
   * restated here because the cases below pin the behaviour at the boundary - ten causes
   * named and the rest counted - which is what makes the bound a contract rather than an
   * implementation detail that may drift.
   */
  private val MaxReportedFailures: Int = 10

  /**
   * The greatest number of characters of one rendered message, the bound of the renderer of
   * the failure model plus the three characters of its marker.
   *
   * Restated here for the same reason: a decoding failure inherits the bound of that renderer,
   * and a case that asserts a concrete number is what shows the inheritance is real.
   */
  private val MaxRenderedMessage: Int = 512 + 3

  /**
   * A message that quotes the text it refused, as the parsing factories of this port do.
   *
   * @param text  the text that was refused, interpolated as it stands
   * @return the message such a factory reports
   */
  private def rejectionMessage(text: String): String = s"Ticker name not found: '$text'"

  /**
   * The message of one of several accumulated causes.
   *
   * @param number  the position of the cause among those accumulated
   * @return the message that cause reports
   */
  private def causeMessage(number: Int): String = s"Element $number is not acceptable"

  /**
   * Several accumulated causes, in the order a factory would have produced them.
   *
   * @param count  how many causes to accumulate, at least one
   * @param message  the message of the cause at each position, counted from one
   * @return the causes as a chain, in that order
   */
  private def accumulated(count: Int, message: Int => String): NonEmptyChain[Failure] =
    NonEmptyChain(
      Failure.Invalid(message(1)),
      List.range(2, count + 1).map(number => Failure.Invalid(message(number))): _*)

  /**
   * A parsed-string codec whose parsing refuses every text, with the causes supplied.
   *
   * The value type is the sample parsed value of this file, so nothing about the fixture
   * differs from the codecs under test above except what its factory says when it refuses -
   * which is the only thing these cases are about.
   *
   * @param causes  the causes the parsing reports for the text it was given
   * @return the codec whose decoding always fails
   */
  private def rejectingCodec(causes: String => NonEmptyChain[Failure]): Codec[SampleTicker] =
    Codecs.parsedStringCodecNec[SampleTicker](text => Left(causes(text)), ticker => SampleTicker.print(ticker))

  test("the bridge escapes a message that could otherwise forge a line of the log holding it") {
    // The finding this case pins: the text of the payload reached the decoding failure as it
    // arrived, so a document could state a line of its own in whatever read the failure.
    val codec = rejectingCodec(text => NonEmptyChain.one(Failure.Parsing(rejectionMessage(text))))
    val forged = "GB\nPARSING: forged\rmore"
    val outcome = codec.decodeJson(Json.fromString(forged))
    val message = outcome.left.value.message
    message shouldBe "Ticker name not found: 'GB\\nPARSING: forged\\rmore'"
    message should not include "\n"
    message should not include "\r"
    message.exists(_.isControl) shouldBe false
    // The failure the factory built is unchanged - it still quotes what was refused, which is
    // what a caller correcting its document needs - so the neutralisation is the writing out.
    Failure.Parsing(rejectionMessage(forged)).message should include("\n")
    // And the position is reported as before, since only the message is rendered.
    val nested = Json.obj("ticker" -> Json.fromString(forged))
    val positioned = nested.hcursor.downField("ticker").as[SampleTicker](codec)
    positioned.left.value.message shouldBe message
    positioned.left.value.history shouldBe List[CursorOp](CursorOp.DownField("ticker"))
  }

  test("the bridge bounds the message of a cause, however much text the payload quoted") {
    val codec = rejectingCodec(text => NonEmptyChain.one(Failure.Parsing(rejectionMessage(text))))
    val payload = "A" * 10000
    val outcome = codec.decodeJson(Json.fromString(payload))
    val message = outcome.left.value.message
    message.length should be <= MaxRenderedMessage
    message.length should be < 600
    message should startWith("Ticker name not found: 'AAAA")
    message should endWith("...")
    // The cause carries the whole of the text, as the failure model requires.
    Failure.Parsing(rejectionMessage(payload)).message.length should be > 10000
  }

  test("the bridge names at most ten causes and states how many it left out") {
    val codec = rejectingCodec(_ => accumulated(25, causeMessage))
    val message = codec.decodeJson(Json.fromString("ab")).left.value.message
    message shouldBe
      (List.range(1, MaxReportedFailures + 1).map(causeMessage) :+ s"and ${25 - MaxReportedFailures} more")
        .mkString("; ")
    message.split("; ").length shouldBe MaxReportedFailures + 1
    message should include(causeMessage(MaxReportedFailures))
    message should not include causeMessage(MaxReportedFailures + 1)
    message should endWith("and 15 more")
  }

  test("no payload that drives a cause per element can make a decoding failure large") {
    // The two bounds together: fifty causes, each quoting ten thousand characters, where the
    // failures themselves hold half a million characters of payload.
    val quoted = "A" * 10000
    val codec = rejectingCodec(_ => accumulated(50, number => s"${causeMessage(number)}: '$quoted'"))
    val message = codec.decodeJson(Json.fromString("ab")).left.value.message
    message.length should be <= (MaxReportedFailures + 1) * (MaxRenderedMessage + "; ".length)
    message.length should be < 6000
    message should endWith("and 40 more")
    message.exists(_.isControl) shouldBe false
  }

  test("the bridge neutralises the causes of a validated product as well as those of a parse") {
    // The same bridge serves every helper, so a product whose factory refuses its raw fields
    // reports in exactly the shape a refused text does.
    val decoder: Decoder[SampleBounds] =
      Codecs.validatedDecoder[RawBounds, SampleBounds](raw =>
        Left(NonEmptyChain.one(Failure.Invalid(s"Bounds are unacceptable: 'low=${raw.low}\nINVALID: forged'"))))
    val outcome = decoder.decodeJson(rawBoundsJson(1, 3))
    outcome.left.value.message shouldBe "Bounds are unacceptable: 'low=1\\nINVALID: forged'"
    outcome.left.value.message should not include "\n"
    outcome.left.value.history shouldBe List.empty[CursorOp]
  }

  //-------------------------------------------------------------------------
  // Codecs.implicits: one import carries the whole policy to a derivation site
  //-------------------------------------------------------------------------
  /** A reading whose every field holds a value the plain numeric codec would lose. */
  private val edgeReading: SampleReading = SampleReading(
    Double.NaN,
    DoubleArray.of(1.0, Double.PositiveInfinity),
    DoubleMatrix.of(1, 2, 1.0, Double.NegativeInfinity),
    DayOfWeek.SATURDAY)

  /** A reading whose every field holds an ordinary value. */
  private val plainReading: SampleReading = SampleReading(
    2.5,
    DoubleArray.of(1.0, 2.0, 3.0),
    DoubleMatrix.of(2, 2, 1.0, 2.0, 3.0, 4.0),
    DayOfWeek.MONDAY)

  test("the implicits object brings all four type-driven codecs into scope together") {
    // the whole inventory of that object, so a member added to it or dropped from it without
    // the type it serves being considered shows up here
    import Codecs.implicits._
    implicitly[Codec[Double]] should be theSameInstanceAs Codecs.taggedDouble
    implicitly[Codec[DayOfWeek]] should be theSameInstanceAs Codecs.dayOfWeekCodec
    implicitly[Codec[DoubleArray]] should be theSameInstanceAs Codecs.doubleArrayCodec
    implicitly[Codec[DoubleMatrix]] should be theSameInstanceAs Codecs.doubleMatrixCodec
    // and the inventory is closed at those four: the time of day is taken from the library,
    // which publishes an encoder and a decoder for it but no combined codec, so a paired
    // instance can only be found here while a fifth member exists to be found
    assertDoesNotCompile("implicitly[io.circe.Codec[java.time.LocalTime]]")
  }

  test("a product derived under the implicits object writes every field through that policy") {
    SampleReading.encoder(edgeReading).noSpaces shouldBe
      """{"factor":"NaN","series":[1.0,"Infinity"],"grid":[[1.0,"-Infinity"]],"day":"SATURDAY"}"""
  }

  test("a product derived under the implicits object writes an ordinary double as a number") {
    SampleReading.encoder(plainReading).noSpaces shouldBe
      """{"factor":2.5,"series":[1.0,2.0,3.0],"grid":[[1.0,2.0],[3.0,4.0]],"day":"MONDAY"}"""
  }

  test("a product derived under the implicits object carries a day of the week by constant name") {
    fieldOf(SampleReading.encoder(edgeReading), "day") shouldBe Json.fromString("SATURDAY")
  }

  test("a product derived under the implicits object round-trips exactly") {
    // the fields of the edge-bearing reading are compared one by one, and its double by its
    // bits: the product is an ordinary case class, so its own equality compares that field
    // numerically, and a value that is not a number is not numerically equal to itself. The
    // array and the matrix need no such care, because their equality compares bit patterns.
    val decoded = Decoder[SampleReading].decodeJson(SampleReading.encoder(edgeReading)).value
    bitsOf(decoded.factor) shouldBe bitsOf(edgeReading.factor)
    decoded.series shouldBe edgeReading.series
    decoded.grid shouldBe edgeReading.grid
    decoded.day shouldBe edgeReading.day
    Decoder[SampleReading].decodeJson(SampleReading.encoder(plainReading)) shouldBe Right(plainReading)
  }

  test("a product derived under the implicits object round-trips through the text form") {
    val decoded = Decoder[SampleReading].decodeJson(reparse(SampleReading.encoder(edgeReading))).value
    bitsOf(decoded.factor) shouldBe bitsOf(Double.NaN)
    decoded.series shouldBe edgeReading.series
    decoded.grid shouldBe edgeReading.grid
    decoded.day shouldBe DayOfWeek.SATURDAY
  }

  test("a product derived under the implicits object rejects a field the policy does not accept") {
    val payload = parse("""{"factor":"nan","series":[1.0],"grid":[[1.0]],"day":"SATURDAY"}""").value
    Decoder[SampleReading].decodeJson(payload).isLeft shouldBe true
  }

  test("a product derived under the implicits object rejects a number no double can hold") {
    // the point of the shared instance is that a derived field inherits the whole policy, the
    // refusal of an over-range number included, without the product restating any of it
    val payload = parse("""{"factor":1e999,"series":[1.0],"grid":[[1.0]],"day":"SATURDAY"}""").value
    val outcome = Decoder[SampleReading].decodeJson(payload)
    outcome.left.value.message shouldBe
      "Expected a JSON number a double can hold; a magnitude beyond that range is written as " +
        "the string Infinity or -Infinity"
    outcome.left.value.history shouldBe List[CursorOp](CursorOp.DownField("factor"))
  }

  test("a product derived under the implicits object rejects an over-range element of an array field") {
    val payload = parse("""{"factor":1.0,"series":[1.0,1e999],"grid":[[1.0]],"day":"SATURDAY"}""").value
    Decoder[SampleReading].decodeJson(payload).isLeft shouldBe true
  }

  test("a product derived under the implicits object rejects a day name it does not know") {
    val payload = parse("""{"factor":1.0,"series":[1.0],"grid":[[1.0]],"day":"Saturday"}""").value
    Decoder[SampleReading].decodeJson(payload).isLeft shouldBe true
  }

  test("a product derived under the implicits object rejects a matrix whose rows disagree") {
    val payload = parse("""{"factor":1.0,"series":[1.0],"grid":[[1.0,2.0],[3.0]],"day":"SUNDAY"}""").value
    Decoder[SampleReading].decodeJson(payload).isLeft shouldBe true
  }

  //-------------------------------------------------------------------------
  // The date and time types the port carries in fields, which the library covers
  //-------------------------------------------------------------------------
  /**
   * The five date and time types carried in the fields of this module's products, with the
   * text each is written as.
   *
   * Every row is the form the serialization policy records, and every row is asserted against
   * the codec in scope for that type - which for all five is the one the library ships,
   * adopted unchanged because what it produces is what the policy states. `Codecs` declares
   * no date or time codec of its own beyond the day of the week, which the library does not
   * cover at all.
   *
   * The time of day is the row worth reading twice. The library's encoder formats through the
   * standard ISO pattern for a time, which always writes a seconds field, so eleven o'clock is
   * `11:00:00` rather than the shorter text the type's own `toString` gives. That is the form
   * the policy records, because the policy is to take the library's instance; the row states it
   * explicitly so a change to what the library produces is detected here rather than assumed.
   */
  private val javaTimeForms = Table(
    ("written", "text"),
    (Encoder[LocalDate].apply(LocalDate.of(2024, 1, 31)), "2024-01-31"),
    (Encoder[LocalTime].apply(LocalTime.of(11, 0)), "11:00:00"),
    (Encoder[ZoneId].apply(ZoneId.of("Europe/London")), "Europe/London"),
    (Encoder[Period].apply(Period.ofMonths(3)), "P3M"),
    (Encoder[YearMonth].apply(YearMonth.of(2024, 1)), "2024-01"))

  /**
   * Times of day, with the text each is written as, covering what a seconds field carries.
   *
   * Every expected string is the ISO pattern's output for that time, measured against the
   * library's own encoder rather than against the type's `toString`: the seconds field is
   * always written, whether it says anything or not, and a fraction of a second is written
   * with as many digits as it needs and no trailing zeroes - which is why half a second is
   * `11:00:00.5` and not `11:00:00.500`.
   */
  private val localTimeForms = Table(
    ("time", "text"),
    (LocalTime.of(11, 0), "11:00:00"),
    (LocalTime.of(0, 0), "00:00:00"),
    (LocalTime.of(23, 59), "23:59:00"),
    (LocalTime.of(11, 0, 30), "11:00:30"),
    (LocalTime.of(23, 59, 59), "23:59:59"),
    (LocalTime.of(11, 0, 0, 500000000), "11:00:00.5"),
    (LocalTime.of(23, 59, 59, 999999999), "23:59:59.999999999"))

  /** Text that is not a local date. */
  private val rejectedDateTexts = Table(
    "text",
    "2024-13-45",
    "31/01/2024",
    "2024-1-31",
    "2024-02-30",
    "2024-01-31T00:00",
    "today",
    "")

  /** Text that is not a time of day. */
  private val rejectedTimeTexts = Table(
    "text",
    "25:00",
    "11:60",
    "11",
    "eleven",
    "11:00:00 AM",
    "")

  /** Text that is not a time zone. */
  private val rejectedZoneTexts = Table(
    "text",
    "Not/AZone",
    "Europe/Nowhere",
    "GMT+abc",
    "")

  /** Text that is not a period. */
  private val rejectedPeriodTexts = Table(
    "text",
    "3M",
    "nonsense",
    "P",
    "PT3H",
    "")

  /** Text that is not a year with a month. */
  private val rejectedYearMonthTexts = Table(
    "text",
    "2024-13",
    "Jan-2024",
    "2024",
    "2024-01-31",
    "")

  test("each date and time type of the port is written in the ISO form the policy states") {
    forAll(javaTimeForms) { (written: Json, text: String) =>
      written shouldBe Json.fromString(text)
    }
  }

  test("a local date is written and read back in its ISO form") {
    val date = LocalDate.of(2024, 1, 31)
    Encoder[LocalDate].apply(date) shouldBe Json.fromString("2024-01-31")
    Decoder[LocalDate].decodeJson(Json.fromString("2024-01-31")) shouldBe Right(date)
  }

  test("a time of day is written and read back in its ISO form") {
    val time = LocalTime.of(11, 0)
    Encoder[LocalTime].apply(time) shouldBe Json.fromString("11:00:00")
    Decoder[LocalTime].decodeJson(Json.fromString("11:00:00")) shouldBe Right(time)
  }

  test("a time of day is written by the library's own instance, which the port adopts") {
    // `Codecs` declares no codec for this type, so the instance found for it here is the
    // library's: what these rows assert is the adopted behaviour, and a locally declared
    // encoder placed in front of it would have to produce exactly the same text to pass
    forAll(localTimeForms) { (time: LocalTime, text: String) =>
      Encoder[LocalTime].apply(time) shouldBe Json.fromString(text)
    }
  }

  test("a time of day round-trips every form it is written in, in memory and as text") {
    forAll(localTimeForms) { (time: LocalTime, _: String) =>
      Decoder[LocalTime].decodeJson(Encoder[LocalTime].apply(time)) shouldBe Right(time)
      Decoder[LocalTime].decodeJson(reparse(Encoder[LocalTime].apply(time))) shouldBe Right(time)
    }
  }

  test("a time of day is read back from a document that leaves its seconds out") {
    // reading is the more forgiving of the two directions: the adopted decoder accepts every
    // form the ISO text allows, so a document written by hand without the seconds field, or
    // with a fraction that says nothing, names the same time the encoder writes
    Decoder[LocalTime].decodeJson(Json.fromString("11:00")) shouldBe Right(LocalTime.of(11, 0))
    Decoder[LocalTime].decodeJson(Json.fromString("11:00:00.000")) shouldBe Right(LocalTime.of(11, 0))
    Decoder[LocalTime].decodeJson(Json.fromString("11:00:30.0")) shouldBe Right(LocalTime.of(11, 0, 30))
  }

  test("a product carrying a time of day derives the instance the port adopts from the library") {
    // this derivation deliberately imports nothing from `Codecs.implicits`, and needs to
    // import nothing: that object offers a double, a day of the week, an array of doubles and
    // a matrix of doubles and no member for a time of day, so the instance the derivation
    // finds through the library's own companion is the whole of the policy for this type. The
    // import would in fact not compile here, since an unused one is an error under this
    // build's settings - which is itself the absence stated as a fact about this file.
    val encoder: Encoder[SampleOpening] = deriveEncoder[SampleOpening]
    val decoder: Decoder[SampleOpening] = deriveDecoder[SampleOpening]
    val opening = SampleOpening("London", LocalTime.of(11, 0))
    encoder(opening).noSpaces shouldBe """{"venue":"London","opens":"11:00:00"}"""
    decoder.decodeJson(encoder(opening)) shouldBe Right(opening)
  }

  test("a time zone is written and read back by its region name") {
    val zone = ZoneId.of("Europe/London")
    Encoder[ZoneId].apply(zone) shouldBe Json.fromString("Europe/London")
    Decoder[ZoneId].decodeJson(Json.fromString("Europe/London")) shouldBe Right(zone)
  }

  test("a period is written and read back in its ISO form") {
    val period = Period.ofMonths(3)
    Encoder[Period].apply(period) shouldBe Json.fromString("P3M")
    Decoder[Period].decodeJson(Json.fromString("P3M")) shouldBe Right(period)
  }

  test("a year with a month is written and read back in its ISO form") {
    val yearMonth = YearMonth.of(2024, 1)
    Encoder[YearMonth].apply(yearMonth) shouldBe Json.fromString("2024-01")
    Decoder[YearMonth].decodeJson(Json.fromString("2024-01")) shouldBe Right(yearMonth)
  }

  test("a local date rejects text that is not one") {
    forAll(rejectedDateTexts) { (text: String) =>
      Decoder[LocalDate].decodeJson(Json.fromString(text)).isLeft shouldBe true
    }
  }

  test("a time of day rejects text that is not one") {
    forAll(rejectedTimeTexts) { (text: String) =>
      Decoder[LocalTime].decodeJson(Json.fromString(text)).isLeft shouldBe true
    }
  }

  test("a time zone rejects text that is not one") {
    forAll(rejectedZoneTexts) { (text: String) =>
      Decoder[ZoneId].decodeJson(Json.fromString(text)).isLeft shouldBe true
    }
  }

  test("a period rejects text that is not one") {
    forAll(rejectedPeriodTexts) { (text: String) =>
      Decoder[Period].decodeJson(Json.fromString(text)).isLeft shouldBe true
    }
  }

  test("a year with a month rejects text that is not one") {
    forAll(rejectedYearMonthTexts) { (text: String) =>
      Decoder[YearMonth].decodeJson(Json.fromString(text)).isLeft shouldBe true
    }
  }

  test("each date and time type rejects a JSON value that is not a string") {
    forAll(nonStringPayloads) { (payload: Json) =>
      Decoder[LocalDate].decodeJson(payload).isLeft shouldBe true
      Decoder[LocalTime].decodeJson(payload).isLeft shouldBe true
      Decoder[ZoneId].decodeJson(payload).isLeft shouldBe true
      Decoder[Period].decodeJson(payload).isLeft shouldBe true
      Decoder[YearMonth].decodeJson(payload).isLeft shouldBe true
    }
  }
}
