/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.json

import java.time.DayOfWeek

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
 * The four codecs that are driven purely by their type, and therefore have exactly one
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
    json.asNumber
      .map(number => number.toDouble)
      .orElse(json.asString.flatMap(text => NonFiniteByTag.get(text)))
      .toRight(DecodingFailure(TaggedDoubleExpectation, cursor.history))
  }

  /**
   * The codec for a double, and the single policy of this port for a value JSON cannot
   * express as a number.
   *
   * A finite value is written as a JSON number and nothing else. The three values outside
   * that range, which the JSON grammar has no syntax for, are written as the strings
   * `"NaN"`, `"Infinity"` and `"-Infinity"`. Decoding accepts a number, or exactly one of
   * those three strings, and rejects everything else: a differently spelled or differently
   * cased tag, the decimal digits of a number delivered as text, a boolean, an object and an
   * array all fail. Being strict here is what keeps the representation a decision rather
   * than a guess.
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
   * Saturday is the JSON string `"SATURDAY"`. The JSON library publishes codecs for the
   * date and time types this port uses in its fields - a date, a time of day, a time zone, a
   * period and a year with month - so those are taken from there and are deliberately not
   * restated here; the day of the week is the one such type it does not cover.
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
  /** Renders a run of elements as a JSON array, each element through `taggedDouble`. */
  private def elementsJson(elements: Array[Double]): Json =
    Json.fromValues(elements.iterator.map(element => taggedDoubleEncoder(element)).toVector)

  private val doubleArrayEncoder: Encoder[DoubleArray] =
    Encoder.instance(values => elementsJson(values.toArrayUnsafe))

  private val doubleArrayDecoder: Decoder[DoubleArray] =
    Decoder.decodeArray[Double](taggedDoubleDecoder, implicitly).map(DoubleArray.ofUnsafe)

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

  private val doubleMatrixDecoder: Decoder[DoubleMatrix] =
    Decoder.instance { cursor =>
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

  /**
   * The codec for an immutable matrix of doubles, represented by a JSON array of rows.
   *
   * A two-by-two matrix is `[[1.0, 2.0], [3.0, 4.0]]`: the matrix is an array of rows and
   * each row is an array of elements in the shape `doubleArrayCodec` produces, so every
   * element again goes through `taggedDouble`. A matrix with no elements is `[]`.
   *
   * A matrix is rectangular by construction, so decoding measures the rows against the
   * first of them before building anything, and reports a payload whose rows disagree as a
   * decoding failure. The check is made here rather than being left to the factory that
   * assembles the matrix, because that factory treats a row of the wrong length as a broken
   * caller and raises an error, which is the right answer for a caller inside the library
   * and the wrong one for a document arriving from outside it.
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
   * four types there is exactly one sensible choice, so importing the members of this object
   * brings them all in at once. Doing it through an import is what makes the choice
   * deliberate and visible in the file that makes it, rather than ambient everywhere this
   * object is mentioned - which matters most for the double, whose instance here has to
   * take precedence over the plain numeric one the JSON library publishes.
   *
   * The helpers that take an argument are not offered here, and cannot be: each of them
   * needs the factory of the type it serves, so it is always invoked by name.
   */
  object implicits {

    /** The single policy for a double, including the values JSON cannot express. */
    implicit val doubleCodec: Codec[Double] = taggedDouble

    /** A day of the week, by constant name. */
    implicit val dayOfWeekCodec: Codec[DayOfWeek] = Codecs.dayOfWeekCodec

    /** An immutable array of doubles, as a JSON array. */
    implicit val doubleArrayCodec: Codec[DoubleArray] = Codecs.doubleArrayCodec

    /** An immutable matrix of doubles, as a JSON array of rows. */
    implicit val doubleMatrixCodec: Codec[DoubleMatrix] = Codecs.doubleMatrixCodec
  }
}
