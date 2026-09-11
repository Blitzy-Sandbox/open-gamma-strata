/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.json

import java.time.DayOfWeek
import java.time.LocalTime

import cats.data.EitherNec
import cats.data.NonEmptyChain

import io.circe.Codec
import io.circe.CursorOp
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.KeyDecoder
import io.circe.KeyEncoder

import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.array.DoubleMatrix
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.Failure

/**
 * The reusable machinery behind the JSON representation of this port.
 *
 * This object holds the part of the serialization policy that is generic: the shapes that
 * every module shares, and the adapters that turn a type's own factory into a decoder. It
 * knows nothing about any domain concept. The concrete instances - one per serializable
 * type - are declared by each type's own companion, which reaches for the helper here that
 * matches how that type is built. Keeping the mechanism in one place and the instances at
 * the types is what lets a later module of the migration reuse all of this unchanged.
 *
 * ===What this replaces===
 *
 * The library being ported serialized a value by discovering its structure while the
 * program ran: a value's properties were read back from its own class, and the reader and
 * writer were assembled from that description on demand. Nothing of that technique survives
 * here. Every codec is built by the compiler from the declared shape of a type, so a type
 * whose codec is missing or ill-typed is a compile error rather than a run-time surprise,
 * and serialization performs no lookup of any kind against a class, a class path or a
 * configuration source. The derivation used throughout is the semi-automatic one, applied
 * explicitly at each type: the automatic variant is never used anywhere in this port,
 * because it would silently derive a shape for a type whose author never chose one.
 *
 * ===How a type picks its helper===
 *
 * A type's construction determines which helper it wants, and the five kinds line up with
 * the five groups below:
 *
 *  - a '''closed named family''' is a string on the wire, so it takes `namedEnumCodec`,
 *    which renders the member's name and resolves text back through the family's own
 *    lookup;
 *  - an '''open value identified by text''' - a pair of currencies, a tenor, an
 *    identifier - is also a string, but is parsed by its own companion rather than by a
 *    family lookup, so it takes `parsedStringCodec` or, when its factory reports several
 *    causes at once, `parsedStringCodecNec`;
 *  - a '''validated or normalising product''' derives a decoder for its raw field tuple and
 *    feeds it to its own checked factory through `validatedDecoder` or `checkedDecoder`, so
 *    a payload that would build an invalid value is rejected as a decoding failure instead
 *    of producing one;
 *  - a '''total product''' derives both halves directly and wraps its encoder in
 *    `dropNulls`, so that an optional field which is absent is left out of the output
 *    rather than written out as an explicitly empty field;
 *  - a '''numeric value''' uses `taggedDouble`, `doubleArrayCodec` or `doubleMatrixCodec`.
 *
 * ===Why the helpers are not implicit===
 *
 * Every member here is declared without `implicit`, and a type's companion assigns the one
 * it wants to an implicit of its own. Three reasons, in order of importance. An implicit
 * encoder or decoder for `Double` published at this level would compete with the one the
 * JSON library already publishes for `Double` at every site that imports this object,
 * making the choice of representation depend on import order rather than on a decision
 * anyone wrote down. The helpers that take arguments cannot be implicit values at all,
 * since the argument is the type's own factory. And a member that is not implicit cannot
 * take part in an implicit cycle, so the warnings that guard against one never arise.
 *
 * The five codecs that are driven purely by their type, and therefore have exactly one
 * sensible instance, are additionally offered as implicits by the nested `implicits`
 * object, which a derivation site imports to bring them into scope together.
 */
object Codecs {

  /** Separates the messages of accumulated failures in a single decoding failure. */
  private val FailureMessageSeparator: String = "; "

  /** The wire form of a value that is not a number. */
  private val NaNTag: String = "NaN"

  /** The wire form of positive infinity. */
  private val PositiveInfinityTag: String = "Infinity"

  /** The wire form of negative infinity. */
  private val NegativeInfinityTag: String = "-Infinity"

  /** Decodes a JSON string, used wherever a value is represented by its text form. */
  private val stringDecoder: Decoder[String] = Decoder.decodeString

  /**
   * Reports accumulated failures as a single decoding failure.
   *
   * This is the only bridge in this object from the failure model of the library to the
   * failure model of the JSON layer, and every helper that can reject a payload on the
   * strength of a type's own factory routes through it. Having exactly one bridge is what
   * makes the message uniform: the messages of the failures appear in the order they were
   * accumulated, separated by a semicolon and a space, and a single failure therefore
   * yields exactly its own message. The position within the document is taken from the
   * cursor that was being decoded, so a failure deep inside a payload still reports where
   * it happened.
   *
   * @param failures  the failures to report, at least one
   * @param history  the position within the document, taken from the decoding cursor
   * @return the decoding failure describing them all
   */
  private def decodingFailure(
      failures: NonEmptyChain[Failure],
      history: List[CursorOp]): DecodingFailure =

    DecodingFailure(
      failures.toNonEmptyList.toList.map(_.message).mkString(FailureMessageSeparator),
      history)

  //-------------------------------------------------------------------------
  /**
   * A codec for a closed family of named values, represented by the member's name.
   *
   * The name is written exactly as the member renders it, with no change of case and no
   * reformatting, because the name is the wire contract: it is the same text the ported
   * library wrote, so a document written by either side names the same member. Decoding
   * hands the text to the family's own lookup, which applies the alternate spellings and
   * the lenient rewrites that the family declares. None of that resolution logic is
   * repeated here - this codec only moves text in and out of it - so a family gains or
   * loses nothing by being serialized.
   *
   * A member named `Act/365F` is therefore the JSON string `"Act/365F"`, and text that
   * names no member fails with the message the family produced.
   *
   * @tparam A  the type of the named values of the family
   * @return the codec for the family
   */
  def namedEnumCodec[A <: Named: NamedEnum]: Codec[A] = {
    val encoder: Encoder[A] = Encoder.instance(value => Json.fromString(value.name))
    val decoder: Decoder[A] = Decoder.instance { cursor =>
      stringDecoder(cursor).flatMap { text =>
        NamedEnum[A].parse(text).left.map(failures => decodingFailure(failures, cursor.history))
      }
    }
    Codec.from(decoder, encoder)
  }

  /**
   * A codec for a value represented by text, whose parsing reports several causes at once.
   *
   * This is the form for a type whose factory accumulates: every reason the text was
   * unacceptable reaches the reader in one failure rather than only the first of them. The
   * text written is whatever `print` produces, which is expected to be the canonical form
   * that `parse` accepts, so that encoding and decoding compose back to the original value.
   *
   * @tparam A  the type of the value
   * @param parse  the companion's parsing factory, reporting every cause of rejection
   * @param print  renders a value as its canonical text
   * @return the codec for the value
   */
  def parsedStringCodecNec[A](
      parse: String => EitherNec[Failure, A],
      print: A => String): Codec[A] = {

    val encoder: Encoder[A] = Encoder.instance(value => Json.fromString(print(value)))
    val decoder: Decoder[A] = Decoder.instance { cursor =>
      stringDecoder(cursor).flatMap { text =>
        parse(text).left.map(failures => decodingFailure(failures, cursor.history))
      }
    }
    Codec.from(decoder, encoder)
  }

  /**
   * A codec for a value represented by text, whose parsing reports a single cause.
   *
   * This is the form most types identified by text want, since a factory that rejects text
   * usually has one thing to say about it. A pair of currencies rendered as `EUR/USD` is
   * the JSON string `"EUR/USD"`, and a country rendered as `GB` is `"GB"`.
   *
   * The failure is reported in exactly the shape `parsedStringCodecNec` produces - this
   * method is that one, given a factory whose single cause is lifted into a chain of one -
   * so which of the two a type happens to use is invisible to a reader of the failure.
   *
   * @tparam A  the type of the value
   * @param parse  the companion's parsing factory
   * @param print  renders a value as its canonical text
   * @return the codec for the value
   */
  def parsedStringCodec[A](
      parse: String => Either[Failure, A],
      print: A => String): Codec[A] =

    parsedStringCodecNec[A](text => parse(text).left.map(failure => NonEmptyChain.one(failure)), print)

  //-------------------------------------------------------------------------
  /**
   * Key codecs for a named value used as the key of a JSON object.
   *
   * A JSON object key is always text, so a named value keys an object by its name. The
   * lookup supplied recovers the value from that text; answering with no value is the only
   * way a key can be rejected, which is why the lookup is shaped as it is rather than
   * reporting a reason. In practice this costs nothing, because a key is machine-written
   * canonical output: text that no value claims means the document was not written by this
   * library.
   *
   * A map keyed by a value named `GBP` is therefore the object `{"GBP": ...}`.
   *
   * @tparam A  the type of the named value
   * @param lookup  recovers a value from its name
   * @return the key encoder and key decoder, in that order
   */
  def namedKeyCodecs[A <: Named](lookup: String => Option[A]): (KeyEncoder[A], KeyDecoder[A]) =
    (KeyEncoder.instance(value => value.name), KeyDecoder.instance(lookup))

  /**
   * Key codecs for a member of a closed named family used as the key of a JSON object.
   *
   * The lookup is the family's exact one, which honours the alternate spellings the family
   * declares but applies no lenient rewriting. That is the right strictness for a key: the
   * text on the wire was written by this library from a member's canonical name, so it
   * needs no rehabilitation, and the richer report that lenient parsing produces would be
   * discarded by the key decoder in any case.
   *
   * @tparam A  the type of the named values of the family
   * @return the key encoder and key decoder, in that order
   */
  def namedEnumKeyCodecs[A <: Named: NamedEnum]: (KeyEncoder[A], KeyDecoder[A]) =
    namedKeyCodecs(name => NamedEnum[A].valueOf(name))

  //-------------------------------------------------------------------------
  /**
   * A decoder that builds a value through a validating factory reporting every cause.
   *
   * This is the decoding route for every type whose construction can reject its input. The
   * type derives a decoder for a private product carrying its raw constructor fields, and
   * hands it here together with its own factory; the field shape is therefore derived by
   * the compiler, while the decision about whether those fields form a legal value stays
   * where it belongs, in the one factory the rest of the library uses. A payload the
   * factory rejects becomes a decoding failure carrying every reason it gave, so it is
   * impossible to obtain an invalid value of such a type by decoding one.
   *
   * Only the decoder needs this treatment. The matching encoder can be derived directly,
   * because a value that exists in memory was already built through the same factory and is
   * therefore known to be legal.
   *
   * @tparam R  the raw field shape, whose decoder is derived by the type
   * @tparam A  the type of the validated value
   * @param build  the type's validating factory, reporting every cause of rejection
   * @param raw  the derived decoder of the raw field shape
   * @return the decoder producing validated values
   */
  def validatedDecoder[R, A](build: R => EitherNec[Failure, A])(implicit raw: Decoder[R]): Decoder[A] =
    Decoder.instance { cursor =>
      raw(cursor).flatMap { fields =>
        build(fields).left.map(failures => decodingFailure(failures, cursor.history))
      }
    }

  /**
   * A decoder that builds a value through a checking factory reporting a single cause.
   *
   * This is `validatedDecoder` for a type whose factory has one thing to say when it
   * rejects its input, which is the common case for a value with a single invariant. The
   * failure is reported in exactly the same shape, a chain of one being just its own
   * message, so the two forms are indistinguishable to a reader of the failure.
   *
   * @tparam R  the raw field shape, whose decoder is derived by the type
   * @tparam A  the type of the checked value
   * @param build  the type's checking factory
   * @param raw  the derived decoder of the raw field shape
   * @return the decoder producing checked values
   */
  def checkedDecoder[R, A](build: R => Either[Failure, A])(implicit raw: Decoder[R]): Decoder[A] =
    validatedDecoder[R, A](fields => build(fields).left.map(failure => NonEmptyChain.one(failure)))

  /**
   * Wraps an encoder so that fields holding no value are left out of the output.
   *
   * A derived encoder writes an optional field that holds nothing as an explicitly empty
   * field. Every product encoder in this port is wrapped here instead, so the field is
   * simply absent, which keeps a document to the information it actually carries. The
   * matching derived decoder already reads an absent field as holding nothing, so wrapping
   * the encoder alone is enough to keep the round trip exact.
   *
   * The removal reaches nested objects as well as the outermost one, so wrapping only the
   * outermost encoder of a structure is sufficient.
   *
   * @tparam A  the type encoded
   * @param encoder  the encoder to wrap, ordinarily a derived one
   * @return the encoder that omits fields holding no value
   */
  def dropNulls[A](encoder: Encoder[A]): Encoder[A] = encoder.mapJson(_.deepDropNullValues)

  //-------------------------------------------------------------------------
  /** Recovers the three values that JSON cannot express as a number. */
  private val NonFiniteByTag: Map[String, Double] = Map(
    NaNTag -> Double.NaN,
    PositiveInfinityTag -> Double.PositiveInfinity,
    NegativeInfinityTag -> Double.NegativeInfinity)

  /** Reported when a payload is neither a number nor one of the three accepted strings. */
  private val TaggedDoubleExpectation: String =
    s"Expected a JSON number or one of the strings $NaNTag, $PositiveInfinityTag, $NegativeInfinityTag"

  /**
   * Reported when a number is syntactically sound but too large for a double to hold.
   *
   * The JSON grammar puts no ceiling on the magnitude of a number, so a document may carry a
   * literal such as `1e999` that no double can represent. Converting it would produce an
   * infinity, and an infinity reached that way would be a second, untagged spelling of a
   * value this port writes only as a string. The payload is rejected instead, naming the
   * spelling it should have used.
   */
  private val TaggedDoubleRangeExpectation: String =
    "Expected a JSON number a double can hold; a magnitude beyond that range is written as " +
      s"the string $PositiveInfinityTag or $NegativeInfinityTag"

  private val taggedDoubleEncoder: Encoder[Double] = Encoder.instance { value =>
    if (java.lang.Double.isFinite(value)) {
      Json.fromDoubleOrNull(value)
    } else if (value.isNaN) {
      Json.fromString(NaNTag)
    } else if (value == Double.PositiveInfinity) {
      Json.fromString(PositiveInfinityTag)
    } else {
      Json.fromString(NegativeInfinityTag)
    }
  }

  private val taggedDoubleDecoder: Decoder[Double] = Decoder.instance { cursor =>
    val json = cursor.value
    json.asNumber match {
      case Some(number) =>
        // the conversion of an over-range literal is an infinity, which is not a form this
        // codec writes and therefore not a form it reads: the two tags are the only route
        val value = number.toDouble
        if (java.lang.Double.isFinite(value)) {
          Right(value)
        } else {
          Left(DecodingFailure(TaggedDoubleRangeExpectation, cursor.history))
        }
      case None =>
        json.asString
          .flatMap(text => NonFiniteByTag.get(text))
          .toRight(DecodingFailure(TaggedDoubleExpectation, cursor.history))
    }
  }

  /**
   * The codec for a double, and the single policy of this port for a value JSON cannot
   * express as a number.
   *
   * A finite value is written as a JSON number and nothing else. The three values outside
   * that range, which the JSON grammar has no syntax for, are written as the strings
   * `"NaN"`, `"Infinity"` and `"-Infinity"`. Decoding accepts a number a double can hold, or
   * exactly one of those three strings, and rejects everything else: a differently spelled or
   * differently cased tag, the decimal digits of a number delivered as text, a boolean, an
   * object and an array all fail. Being strict here is what keeps the representation a
   * decision rather than a guess.
   *
   * ===One spelling per value===
   *
   * The three strings are the ''only'' way a value outside the finite range reaches this
   * codec. The JSON grammar bounds neither the digits nor the exponent of a number, so a
   * document can state a magnitude no double can hold - `1e999` - and converting such a
   * literal yields an infinity. That conversion is refused: a number whose converted value
   * is not finite is a decoding failure naming the string form it should have used. Were it
   * admitted, an infinity would have two spellings on the wire, only one of which this codec
   * writes, and a document could carry a value this codec could never have produced - the
   * exact leniency that makes a representation a guess. A magnitude too ''small'' for a
   * double is a different matter and is accepted: it converts to a zero, which is what the
   * platform's own reading of the same text produces, and it stays within the finite range
   * this codec is defined over.
   *
   * Whether a value outside the finite range is ''acceptable'' is never decided here. That
   * belongs to the factory of the type holding the field, and the types differ: one rejects
   * a value that is not a number while admitting the two infinities. This codec's single
   * duty is to carry whichever values that factory allows, without loss.
   *
   * ===Exactness===
   *
   * The round trip is exact to the bit, for every finite value, and this is a requirement
   * rather than a convenience. Equality on the numeric types of this port compares the bit
   * patterns of their elements, as the library being ported did: a value that is not a
   * number equals itself, and the two signed zeroes are distinguishable. A codec that lost
   * the sign of a zero, or a digit of a long mantissa, would therefore break the round-trip
   * property outright, not merely blur it. It would also put the numerical agreement with
   * the ported implementation out of reach, since the captured baselines that pin that
   * agreement are themselves carried as JSON and read back through this codec. A value is
   * accordingly written straight from its own representation and read straight back into
   * one, with no arbitrary-precision decimal anywhere on the path, because that detour
   * cannot represent a signed zero and would silently discard the sign.
   *
   * So `-0.0` is written as `-0.0` and read back as `-0.0`, and the smallest positive value
   * survives all of its digits.
   */
  val taggedDouble: Codec[Double] = Codec.from(taggedDoubleDecoder, taggedDoubleEncoder)

  //-------------------------------------------------------------------------
  /** The days of the week by constant name, built once from the closed set of days. */
  private val DayOfWeekByName: Map[String, DayOfWeek] =
    DayOfWeek.values().iterator.map(day => day.name() -> day).toMap

  /** Reported when a payload is not the name of a day of the week. */
  private val DayOfWeekExpectation: String =
    s"Expected one of the day names ${DayOfWeek.values().iterator.map(day => day.name()).mkString(", ")}"

  /**
   * The codec for a day of the week, represented by its constant name.
   *
   * Saturday is the JSON string `"SATURDAY"`. Of the date and time types this port uses in
   * its fields, four - a date, a time zone, a period and a year with month - are taken from
   * the JSON library unchanged and are deliberately not restated here, because what it
   * produces for them is exactly what the policy of this port states. Two are not: the day of
   * the week, which that library does not cover at all and which this codec supplies, and the
   * time of day, which it covers in a longer form than the policy states and which
   * `localTimeCodec` supplies below.
   *
   * Decoding consults the closed set of days, which is built once when this object is
   * initialized. It deliberately does not ask the day type itself to interpret the text,
   * because that route answers unacceptable text by raising an error, which would turn a
   * rejected payload into a thrown failure instead of a reported one. Text that is not
   * exactly one of the seven names, including a correctly spelled name in the wrong case or
   * a three-letter abbreviation, is rejected.
   */
  val dayOfWeekCodec: Codec[DayOfWeek] = {
    val encoder: Encoder[DayOfWeek] = Encoder.instance(day => Json.fromString(day.name()))
    val decoder: Decoder[DayOfWeek] = Decoder.instance { cursor =>
      stringDecoder(cursor).flatMap { text =>
        DayOfWeekByName
          .get(text)
          .toRight(DecodingFailure(DayOfWeekExpectation, cursor.history))
      }
    }
    Codec.from(decoder, encoder)
  }

  //-------------------------------------------------------------------------
  /**
   * The codec for a time of day, represented by the shortest text that states it.
   *
   * Eleven o'clock is the JSON string `"11:00"`, half past eleven with thirty seconds is
   * `"11:00:30"`, and a time carrying a fraction of a second states it, so nothing about a
   * time is ever lost. This is the form the serialization policy of this port records, and
   * the form the standard text of a time of day takes: the seconds field appears when it
   * says something and is left out when it does not.
   *
   * ===Why this one is restated===
   *
   * The JSON library publishes codecs for the five date and time types this port carries in
   * its fields, and four of them are adopted here unchanged because what they produce is
   * exactly what the policy states. The time of day is the exception. Its published encoder
   * formats through the standard pattern for such a time, whose seconds section is optional
   * only when reading: a time of day always has a seconds field to offer, so the pattern
   * always writes one, and eleven o'clock comes out as `"11:00:00"`. The policy of this port
   * states `"11:00"`, and the policy is the contract, so the encoder is the port's own.
   *
   * Reading is a different matter and is taken from the library unchanged, because the
   * published decoder already accepts every form the standard text allows - with the seconds
   * field and without it, with a fraction of a second and without one. So a document written
   * by this port, by the library, or by hand in either form is read back to the same time.
   */
  val localTimeCodec: Codec[LocalTime] = {
    val encoder: Encoder[LocalTime] = Encoder.instance(time => Json.fromString(time.toString))
    Codec.from(Decoder.decodeLocalTime, encoder)
  }

  //-------------------------------------------------------------------------
  /**
   * The greatest number of elements this port reads into an array of doubles.
   *
   * A ceiling is needed because the dimensions of a numeric payload are stated by the
   * document rather than by the reader: without one, the size of the run of values this port
   * allocates is chosen by whoever wrote the document. The value is far above anything the
   * library itself produces - the largest run any captured fixture carries is a handful of
   * elements, and the widest structure any type of this port holds is a square of currency
   * rates - while bounding one array to eight megabytes of values.
   */
  val MaximumArrayElements: Int = 1 << 20

  /** The greatest number of rows this port reads into a matrix of doubles. */
  val MaximumMatrixRows: Int = 4096

  /** The greatest number of elements in one row this port reads into a matrix of doubles. */
  val MaximumMatrixColumns: Int = 4096

  /**
   * The greatest number of elements, across every row, this port reads into a matrix.
   *
   * The row and column ceilings bound each dimension on its own; this one bounds their
   * product, which is what actually gets allocated. It is checked in a width that the
   * product of two counts cannot exceed, so a payload cannot slip past the ceiling by
   * stating dimensions whose product wraps around.
   */
  val MaximumMatrixElements: Int = 1 << 20

  /**
   * Reports a payload whose stated size is beyond what this port reads.
   *
   * @param what  what was counted, naming the ceiling that was exceeded
   * @param declared  the count the payload states
   * @param limit  the greatest count this port reads
   * @return the message describing the refusal
   */
  private def beyondCeiling(what: String, declared: Long, limit: Int): String =
    s"Expected at most $limit $what, but the payload states $declared"

  /** Renders a run of elements as a JSON array, each element through `taggedDouble`. */
  private def elementsJson(elements: Array[Double]): Json =
    Json.fromValues(elements.iterator.map(element => taggedDoubleEncoder(element)).toVector)

  private val doubleArrayEncoder: Encoder[DoubleArray] =
    Encoder.instance(values => elementsJson(values.toArrayUnsafe))

  /** Reads the elements of a JSON array, each through `taggedDouble`. */
  private val doubleElementsDecoder: Decoder[Array[Double]] =
    Decoder.decodeArray[Double](taggedDoubleDecoder, implicitly)

  private val doubleArrayDecoder: Decoder[DoubleArray] =
    Decoder.instance { cursor =>
      // the length is taken from the payload before an element is read, so a document cannot
      // choose how much this port allocates; a payload that is not an array falls through to
      // the element reader, which reports it exactly as it always did
      cursor.value.asArray match {
        case Some(elements) if elements.size > MaximumArrayElements =>
          Left(DecodingFailure(
            beyondCeiling("elements in the array", elements.size.toLong, MaximumArrayElements),
            cursor.history))
        case _ =>
          doubleElementsDecoder(cursor).map(elements => DoubleArray.ofUnsafe(elements))
      }
    }

  /**
   * The codec for an immutable array of doubles, represented by a JSON array.
   *
   * An array of three elements, the first of which is not a number, is
   * `["NaN", 2.0, -0.0]`: the array is a JSON array, and each element is written through
   * `taggedDouble`, so the element policy and its exactness are the same here as anywhere
   * else. An empty array is `[]`.
   *
   * Both directions avoid a copy that would otherwise be pure overhead, and both are safe
   * to do so for a reason particular to each. Encoding reads the elements of the array
   * being written directly and only reads them, never retaining or modifying what it saw.
   * Decoding produces a run of elements that has just been allocated for it and is
   * published nowhere else, and so may be adopted as the contents of the result rather than
   * copied into it. The two operations that make this possible are visible only within this
   * module, precisely so that this file can use them while no caller outside can: the
   * public surface of these arrays stays copy-safe from end to end, and nothing here widens
   * it.
   *
   * ===How much a document may ask for===
   *
   * How long the array is, is stated by the document, so decoding measures the payload
   * against `MaximumArrayElements` before it reads a single element. A longer payload is a
   * decoding failure naming the ceiling, and nothing is allocated for it. The order matters:
   * the ceiling is worth having only if it is applied before the work it bounds, since a
   * refusal issued after the elements have been read has already paid for them.
   */
  val doubleArrayCodec: Codec[DoubleArray] = Codec.from(doubleArrayDecoder, doubleArrayEncoder)

  //-------------------------------------------------------------------------
  /** Reported when the rows of a matrix payload are not all of the same length. */
  private val RaggedMatrixMessage: String =
    "Expected every row of the matrix to hold the same number of elements"

  private val doubleArrayVectorDecoder: Decoder[Vector[DoubleArray]] =
    Decoder.decodeVector(doubleArrayDecoder)

  private val doubleMatrixEncoder: Encoder[DoubleMatrix] =
    Encoder.instance { matrix =>
      Json.fromValues(matrix.toArrayUnsafe.iterator.map(row => elementsJson(row)).toVector)
    }

  /**
   * Measures the shape a matrix payload states, before any of it is read.
   *
   * Every conclusion here is drawn from the JSON as it already stands, and the order in which
   * they are reached is itself part of the guard. How many rows the payload states is known
   * without looking at any of them, so that count is compared with its ceiling first and a
   * payload beyond it is refused without a single row having been examined. Only a payload
   * whose row count is already known to be within the ceiling is walked to measure the widths
   * of its rows - at most as many measurements as that ceiling allows - and only then are the
   * width, the product of the two dimensions and the agreement between the rows decided.
   *
   * A row that is not an array has no width to offer and is passed over: what is wrong with
   * such a payload is the row itself, which the reader of that row reports precisely, at that
   * row's own position. Raggedness is therefore judged only when every row could be measured,
   * which is exactly when the judgement is sound.
   *
   * @param rowsJson  the rows the payload states
   * @param history  the position within the document, taken from the decoding cursor
   * @return the refusal, or nothing if the stated shape is one this port reads
   */
  private def matrixShapeRefusal(
      rowsJson: Vector[Json],
      history: List[CursorOp]): Option[DecodingFailure] = {

    val rows = rowsJson.size
    if (rows > MaximumMatrixRows) {
      Some(DecodingFailure(beyondCeiling("rows in the matrix", rows.toLong, MaximumMatrixRows), history))
    } else {
      // reached only for a row count within the ceiling, so this walk and what it collects are
      // bounded by that ceiling rather than by the payload
      val measured = rowsJson.flatMap(row => row.asArray.map(elements => elements.size))
      val columns = if (measured.isEmpty) 0 else measured.max
      if (columns > MaximumMatrixColumns) {
        Some(DecodingFailure(
          beyondCeiling("elements in each row of the matrix", columns.toLong, MaximumMatrixColumns),
          history))
      } else if (rows.toLong * columns.toLong > MaximumMatrixElements.toLong) {
        Some(DecodingFailure(
          beyondCeiling("elements in the matrix", rows.toLong * columns.toLong, MaximumMatrixElements),
          history))
      } else if (measured.size == rows && measured.exists(size => size != columns)) {
        Some(DecodingFailure(RaggedMatrixMessage, history))
      } else {
        None
      }
    }
  }

  private val doubleMatrixDecoder: Decoder[DoubleMatrix] =
    Decoder.instance { cursor =>
      // the shape comes from the payload, so it is measured against the ceilings and checked
      // for square rows before a row is read; a payload that is not an array falls through to
      // the row reader, which reports it exactly as it always did
      val refusal = cursor.value.asArray.flatMap(rowsJson => matrixShapeRefusal(rowsJson, cursor.history))
      refusal match {
        case Some(failure) => Left(failure)
        case None =>
          doubleArrayVectorDecoder(cursor).flatMap { rows =>
            if (rows.isEmpty) {
              Right(DoubleMatrix.EMPTY)
            } else {
              val columns = rows.head.size
              if (rows.forall(row => row.size == columns)) {
                Right(DoubleMatrix.ofArrayObjects(rows.size, columns)(index => rows(index)))
              } else {
                Left(DecodingFailure(RaggedMatrixMessage, cursor.history))
              }
            }
          }
      }
    }

  /**
   * The codec for an immutable matrix of doubles, represented by a JSON array of rows.
   *
   * A two-by-two matrix is `[[1.0, 2.0], [3.0, 4.0]]`: the matrix is an array of rows and
   * each row is an array of elements in the shape `doubleArrayCodec` produces, so every
   * element again goes through `taggedDouble`. A matrix with no elements is `[]`.
   *
   * A matrix is rectangular by construction, so decoding measures the rows against each
   * other and reports a payload whose rows disagree as a decoding failure. The check is made
   * here rather than being left to the factory that assembles the matrix, because that
   * factory treats a row of the wrong length as a broken caller and raises an error, which is
   * the right answer for a caller inside the library and the wrong one for a document
   * arriving from outside it.
   *
   * ===How much a document may ask for===
   *
   * The shape of the matrix is stated by the document, and both of its dimensions are taken
   * from the payload as it stands - before a row is read and therefore before any row of
   * values is allocated. Three ceilings apply, `MaximumMatrixRows`, `MaximumMatrixColumns` and
   * `MaximumMatrixElements` for their product, and a payload beyond any of them is a decoding
   * failure naming the one it exceeded. The row count is the first thing compared with its
   * ceiling, because it is the one dimension knowable without touching a row: a payload
   * stating more rows than this port reads is refused before anything examines them, so the
   * measuring that follows is bounded by the ceiling rather than by the document. Rows that
   * disagree are found in that same bounded pass, so a ragged payload is refused without its
   * rows having been read either.
   *
   * The rows are read only once the stated shape is one this port accepts, and the rows that
   * result are measured once more before the matrix is assembled. That second look is not a
   * repetition of the first: it is what keeps assembly total, since the factory reached at
   * that point answers a row of the wrong length by raising an error rather than reporting
   * one, and no payload may be able to reach it.
   *
   * The rows are handed to the existing factory that assembles a matrix from them, so the
   * nested structure that a matrix keeps internally is built in the one file that owns it.
   * That is a deliberate boundary and not an incidental one: allocating a nested structure
   * whose elements are themselves runs of values is the one allocation in this language
   * that reaches for the very run-time type machinery this port's serialization is required
   * to do without, so it is kept out of the serialization path altogether.
   */
  val doubleMatrixCodec: Codec[DoubleMatrix] = Codec.from(doubleMatrixDecoder, doubleMatrixEncoder)

  //-------------------------------------------------------------------------
  /**
   * The codecs that are determined by their type alone, offered as implicits.
   *
   * A derivation site needs the codec of every field type in implicit scope, and for these
   * five types there is exactly one sensible choice, so importing the members of this object
   * brings them all in at once. Doing it through an import is what makes the choice
   * deliberate and visible in the file that makes it, rather than ambient everywhere this
   * object is mentioned - which matters most for the two types the JSON library also
   * publishes an instance for, the double and the time of day: the instance here has to take
   * precedence over the plain numeric one and over the longer form of a time, and an import
   * is what gives it that precedence.
   *
   * The helpers that take an argument are not offered here, and cannot be: each of them
   * needs the factory of the type it serves, so it is always invoked by name.
   */
  object implicits {

    /** The single policy for a double, including the values JSON cannot express. */
    implicit val doubleCodec: Codec[Double] = taggedDouble

    /** A day of the week, by constant name. */
    implicit val dayOfWeekCodec: Codec[DayOfWeek] = Codecs.dayOfWeekCodec

    /** A time of day, in the shortest text that states it. */
    implicit val localTimeCodec: Codec[LocalTime] = Codecs.localTimeCodec

    /** An immutable array of doubles, as a JSON array. */
    implicit val doubleArrayCodec: Codec[DoubleArray] = Codecs.doubleArrayCodec

    /** An immutable matrix of doubles, as a JSON array of rows. */
    implicit val doubleMatrixCodec: Codec[DoubleMatrix] = Codecs.doubleMatrixCodec
  }
}
