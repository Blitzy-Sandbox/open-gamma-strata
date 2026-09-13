/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.json

import java.time.DayOfWeek

import scala.annotation.tailrec
import scala.util.control.NonFatal

import cats.data.EitherNec
import cats.data.NonEmptyChain
import cats.data.Validated

import io.circe.Codec
import io.circe.CursorOp
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
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
 * ===How much a document may ask for===
 *
 * A decoder is the one place in this port where the size of what gets allocated is stated from
 * outside it, so the sizes a document may state are bounded here rather than taken on trust.
 * The numeric codecs carry their own ceilings - `MaximumArrayElements` for the length of an
 * array, and `MaximumMatrixRows`, `MaximumMatrixColumns` and `MaximumMatrixElements` for the
 * shape of a matrix - and every one of them is compared with the JSON as it already stands,
 * before an element is read, since a refusal issued after the reading has already paid for it.
 * A type whose wire form holds a collection bounds it the same way by wrapping its decoder in
 * `boundedElements`, when the collection is the whole of that wire form, or in `boundedFields`,
 * when the collections are named fields of an object; both apply `MaximumCollectionElements`.
 *
 * Every ceiling is set above anything this port can itself produce, for the reasons recorded at
 * each of them, so `decode(encode(x))` holds for every value the library can build. What a
 * ceiling refuses is a document asking for more than this port has any use for, never a
 * document this port wrote.
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

  /**
   * The greatest number of accumulated failures whose messages one decoding failure holds,
   * before the marker that states how many were left out.
   *
   * A factory of this port reports one cause per broken invariant, and the types it is used
   * by declare a handful of them each, so this is set above what any of them can accumulate
   * and every decoding failure the library itself produces therefore names every cause. The
   * bound exists for the payload that drives a factory to accumulate a cause per element -
   * a document holding thousands of amounts, each rejected - where the report would
   * otherwise grow with the payload rather than describe it.
   */
  private val MaxReportedFailures: Int = 10

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
   * accumulated, separated by a semicolon and a space. The position within the document is
   * taken from the cursor that was being decoded, so a failure deep inside a payload still
   * reports where it happened.
   *
   * ===It is also where rejected text is neutralised===
   *
   * A payload is written by whoever sends it, and a factory that rejects one quotes what it
   * refused: the message of the failure carries that text as it arrived. A decoding failure
   * is read where a failure is read - a log, a report, a line of a console - so this is the
   * boundary at which such text has to be made safe, and it is made safe here rather than
   * when the failure is built, because [[Failure.message]] hands back what was refused to
   * the code that acts on it. Two bounds hold of the message this method produces, whatever
   * the payload was:
   *
   *  - every message is rendered through `Failure.renderDiagnostic`, so each is a single
   *    line, holds no character a line-oriented reader could act on, and is bounded in
   *    length however large the rejected value was (CWE-117, CWE-400);
   *  - at most `MaxReportedFailures` of them are reported, in the order they were
   *    accumulated, followed by one marker naming how many were left out, so a payload that
   *    drives a factory to accumulate a cause per element cannot make the report grow with
   *    the payload.
   *
   * Neither bound changes what an ordinary failure reads as: text within the bound holding
   * none of the escaped characters renders to itself, character for character, so a single
   * failure still yields exactly its own message and a handful of them still read as the
   * messages their factory wrote, joined by the separator.
   *
   * @param failures  the failures to report, at least one
   * @param history  the position within the document, taken from the decoding cursor
   * @return the decoding failure describing them all
   */
  private def decodingFailure(
      failures: NonEmptyChain[Failure],
      history: List[CursorOp]): DecodingFailure = {

    val causes = failures.toNonEmptyList.toList
    val reported = causes.take(MaxReportedFailures).map(cause => Failure.renderDiagnostic(cause.message))
    val omitted = causes.length - reported.length
    val parts = if (omitted > 0) reported :+ s"and $omitted more" else reported
    DecodingFailure(parts.mkString(FailureMessageSeparator), history)
  }

  //-------------------------------------------------------------------------
  /** Introduces the account a decoding failure gives of a factory that refused by raising. */
  private val RaisedPrefix: String = "The payload was refused by the type it describes: "

  /** Reported where a factory refused a payload by raising without saying anything. */
  private val RaisedWithoutMessage: String = "no reason was given"

  /**
   * The greatest number of characters of rendered text a raised account carries.
   *
   * The bound is on the rendering rather than on the text behind it, an escaped character
   * standing for six of these and a character standing for itself for one. An account that
   * was cut carries [[RaisedAccountEllipsis]] as well, so the whole is at most this many
   * characters plus that marker.
   */
  private val MaxRaisedAccount: Int = 256

  /** Marks an account of a raised refusal that was cut short at [[MaxRaisedAccount]]. */
  private val RaisedAccountEllipsis: String = "..."

  /**
   * Renders what a raised refusal said, bounded and on one line.
   *
   * The text comes from inside the library rather than from the document, but the value it
   * names came from the document, so its length and its content are as unconstrained as the
   * payload is - a message saying which date was refused carries that date, and a message
   * saying which name was refused carries that name. It is therefore rendered as a diagnostic
   * is rendered everywhere in these modules, by the same rule
   * `com.opengamma.strata.collect.result.Failure` applies when it writes a failure out:
   *
   *   - the three control characters a reader recognises - line feed, carriage return, tab -
   *     become their short escapes;
   *   - every other ISO control character, the two Unicode separators a reader may treat as
   *     ending a line (U+2028 and U+2029), and a surrogate standing on its own become a
   *     fixed-width `\uXXXX` escape. The separators matter as much as the control characters
   *     do: a reader that splits on them sees two lines where the log holds one, which is the
   *     forgery this rendering exists to prevent;
   *   - a surrogate '''pair''' is one character of one language or another and is kept whole;
   *   - everything else stands as it is.
   *
   * The rendering is assembled a unit at a time, a surrogate pair counting as one, and stops
   * as soon as the next unit would carry it past [[MaxRaisedAccount]]. Cutting by rendered
   * unit rather than by machine word is what keeps a pair whole and an escape entire: cutting
   * the text itself could leave half of a character at the end, which is the replacement glyph
   * this rendering avoids everywhere else.
   *
   * The rule is applied here rather than borrowed, `Failure` keeping its rendering to itself
   * for the reason stated there - rendering is what that type does when it writes a failure
   * out, not an operation it offers its callers - and the two are held together by the tests
   * of each, which state the same cases.
   *
   * @param error  the refusal that was raised
   * @return the account of it, on one line and within the ceiling
   */
  private def raisedAccount(error: Throwable): String = {
    val said = Option(error.getMessage).map(text => text.trim).filter(text => text.nonEmpty)
    renderAccount(said.getOrElse(RaisedWithoutMessage))
  }

  /**
   * Renders one piece of text as a bounded single line, by the rule [[raisedAccount]] states.
   *
   * Threading the text rendered so far through a tail-recursive step, rather than accumulating
   * into a mutable local, keeps the method free of assignment; both the intermediate and the
   * final strings are bounded by [[MaxRaisedAccount]], so the concatenation costs no more than
   * assembling the result in one pass would.
   *
   * @param text  the text to render
   * @return the rendering of it, on one line and within the ceiling
   */
  private def renderAccount(text: String): String = {
    @tailrec
    def rendering(index: Int, rendered: String): String =
      if (index >= text.length) {
        rendered
      } else {
        val head = text.charAt(index)
        val pairsWithNext =
          Character.isHighSurrogate(head) &&
            index + 1 < text.length &&
            Character.isLowSurrogate(text.charAt(index + 1))
        val unit = if (pairsWithNext) text.substring(index, index + 2) else describeChar(head)
        if (rendered.length + unit.length > MaxRaisedAccount) {
          rendered + RaisedAccountEllipsis
        } else {
          rendering(index + (if (pairsWithNext) 2 else 1), rendered + unit)
        }
      }

    rendering(0, "")
  }

  /**
   * Renders one character of an account.
   *
   * @param ch  the character to render
   * @return its short escape, its fixed-width escape, or the character itself
   */
  private def describeChar(ch: Char): String =
    if (ch == '\n') {
      "\\n"
    } else if (ch == '\r') {
      "\\r"
    } else if (ch == '\t') {
      "\\t"
    } else if (escapesAsUnicode(ch)) {
      unicodeEscape(ch)
    } else {
      ch.toString
    }

  /**
   * Tests whether a character has to be written out as an escape rather than as itself.
   *
   * Every ISO control character other than the three with a short escape, the two Unicode
   * separators a reader may treat as ending a line, and a surrogate standing on its own, which
   * is half of a character and turns into a replacement glyph wherever it is written.
   *
   * @param ch  the character to test
   * @return true where the character is written out as an escape
   */
  private def escapesAsUnicode(ch: Char): Boolean =
    Character.isISOControl(ch) || ch == '\u2028' || ch == '\u2029' || Character.isSurrogate(ch)

  /**
   * The six-character escape of a character.
   *
   * The digits are the lower-case hexadecimal ones `Integer.toHexString` produces, padded to
   * four so that the width of an escape is fixed and the ceiling above can be reasoned about
   * without knowing which character was escaped.
   *
   * @param ch  the character to escape
   * @return the `\uXXXX` escape of it
   */
  private def unicodeEscape(ch: Char): String = {
    val digits = Integer.toHexString(ch.toInt)
    s"\\u${"0" * (4 - digits.length)}$digits"
  }

  /**
   * Reports a factory that refused a payload by raising as an ordinary decoding failure.
   *
   * Routed through the one bridge above, so a refusal that arrived this way is reported in
   * exactly the shape a reported refusal is, and the position within the document is the
   * position that was being decoded.
   *
   * @param error  the refusal that was raised
   * @param history  the position within the document, taken from the decoding cursor
   * @return the decoding failure describing it
   */
  private def raisedFailure(error: Throwable, history: List[CursorOp]): DecodingFailure =
    decodingFailure(
      NonEmptyChain.one(Failure.Invalid(RaisedPrefix + raisedAccount(error))),
      history)

  /**
   * Wraps a decoder so that a refusal raised while it runs is reported rather than propagated.
   *
   * A decoder answers with a failure; that is its whole contract, and it is what lets a caller
   * decoding a document from outside the program decide what to do about a document it cannot
   * use. The factories the decoders below hand their fields to do not share that contract:
   * each of them is the factory a '''caller''' uses, and a caller supplying an argument that
   * names nothing - a date in a year no calendar can hold data for, a magnitude no search can
   * satisfy - is a fault in the calling code, which the library states by raising, as the
   * library being ported did. The two contracts meet here, at the boundary between a document
   * and a factory, and this is where the second becomes the first: what a factory raises about
   * fields that came out of a payload is a property of that payload, so it is reported at the
   * position it occurred, and the value the decoder was asked for is refused rather than the
   * refusal escaping the decoding of the document altogether.
   *
   * The catch is deliberately broad, which it is nowhere else in these modules: elsewhere a
   * guard names the exceptions it expects and lets anything else through as the defect it is,
   * but the whole point here is that '''no''' refusal may leave a decoder, and the boundary
   * cannot enumerate what every factory of every module might raise about its arguments. What
   * it does not catch is what no code can handle: an error that says the machine itself is
   * failing is left to propagate untouched.
   *
   * Both directions of the decoder are wrapped - the one that answers with the first failure
   * and the one that accumulates - so a decoder used either way is equally bounded.
   *
   * @tparam A  the type decoded
   * @param decoder  the decoder to wrap
   * @return the decoder that reports a raised refusal as a decoding failure
   */
  def guardedDecoder[A](decoder: Decoder[A]): Decoder[A] = new Decoder[A] {

    override def apply(cursor: HCursor): Decoder.Result[A] =
      try decoder(cursor)
      catch { case NonFatal(error) => Left(raisedFailure(error, cursor.history)) }

    override def decodeAccumulating(cursor: HCursor): Decoder.AccumulatingResult[A] =
      try decoder.decodeAccumulating(cursor)
      catch { case NonFatal(error) => Validated.invalidNel(raisedFailure(error, cursor.history)) }
  }

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
    val decoder: Decoder[A] = guardedDecoder(Decoder.instance { cursor =>
      stringDecoder(cursor).flatMap { text =>
        NamedEnum[A].parse(text).left.map(failures => decodingFailure(failures, cursor.history))
      }
    })
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
    val decoder: Decoder[A] = guardedDecoder(Decoder.instance { cursor =>
      stringDecoder(cursor).flatMap { text =>
        parse(text).left.map(failures => decodingFailure(failures, cursor.history))
      }
    })
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
   * That holds however the factory states its refusal. Most of them report one, in the
   * failure model of the library, and those reasons are what the decoding failure carries.
   * Some conditions are instead a fault in the calling code rather than a property of the
   * data - a date outside the years a holiday calendar can hold data for, a shift larger
   * than any search can satisfy - and a factory raises those, as the library being ported
   * did. A payload is not calling code, so `guardedDecoder` turns such a refusal into a
   * decoding failure as well: no document can make a decoder of this port raise instead of
   * answering.
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
    guardedDecoder(Decoder.instance { cursor =>
      raw(cursor).flatMap { fields =>
        build(fields).left.map(failures => decodingFailure(failures, cursor.history))
      }
    })

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
   * Saturday is the JSON string `"SATURDAY"`. Of the six date and time types carried in the
   * fields of this library, five - a date, a time of day, a time zone, a period and a year
   * with month - take the instances the JSON library publishes, which produce exactly the
   * wire form the serialization contract states, so they are deliberately not restated here.
   * The day of the week is the one type that library publishes no instance for, which is why
   * this codec exists and why it is the only date or time codec declared here: the reason is
   * the absence of an instance rather than a disagreement with one.
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
   * The greatest number of elements this port reads into an array of doubles.
   *
   * A ceiling is needed because the size of a numeric payload is stated by the document rather
   * than by the reader: without one, how large a run of values this port allocates, and how
   * long it spends reading one, are chosen by whoever wrote the document, and a few kilobytes
   * of repeated text name a run of values several gigabytes wide.
   *
   * The figure is far above anything this library itself produces. The longest run any captured
   * parity fixture carries is a handful of elements, the generators of the test suites build
   * arrays some three orders of magnitude shorter than this, and the widest numeric structure
   * any type of this port holds is a square of currency rates. So the ceiling bounds a hostile
   * document without ever standing between a value this port wrote and the reading of it back:
   * `decode(encode(x))` holds for every `x` the port can build, which is the property the
   * figure was chosen to preserve rather than one it trades away. At eight megabytes of
   * elements it is also the largest single allocation any payload can ask this object for.
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
   * The greatest number of elements this port reads into a collection of a value.
   *
   * This is the ceiling the two bounding decoders below apply, and it is deliberately the same
   * figure the library already enforces on its own expansions: schedule generation refuses to
   * produce more than a hundred thousand periods, and a sequence of value steps refuses to
   * expand past a hundred thousand steps. A collection larger than this therefore cannot be
   * part of any value this port builds - the factory that would have built it refuses first -
   * so no document the port writes is refused by this ceiling, while a document from outside it
   * can no longer ask for millions of entries to be allocated, sorted and grouped before the
   * factory collapses them to a handful.
   *
   * It is one figure rather than one per type because the cost being bounded is the same in
   * every case - the entries a decoder materialises out of a JSON array - and a per-type table
   * of limits would be a set of numbers no reader could check against anything.
   */
  val MaximumCollectionElements: Int = 100000

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

  //-------------------------------------------------------------------------
  /**
   * Bounds how many elements the decoded value's own JSON array may state.
   *
   * This is the wrapper a type reaches for when its wire form '''is''' an array - a list of
   * dates, a list of periods, a list of steps - and the ceiling is applied to that array
   * before the wrapped decoder is invoked. The count comes from the JSON as it already stands,
   * so a payload beyond the ceiling is refused without the wrapped decoder having allocated
   * anything at all: that is the whole point of the wrapper, since a refusal issued after the
   * entries have been read, sorted and grouped has already paid for them.
   *
   * A cursor whose value is not a JSON array has no count to offer and is passed to the
   * wrapped decoder untouched, which is what keeps the wrapper invisible to every other
   * refusal: what is wrong with such a payload is for the wrapped decoder to report, in its own
   * words and at its own position.
   *
   * @tparam A  the type being decoded
   * @param what  what the elements are, named as the refusal message should name them
   * @param limit  the greatest number of elements this port reads, usually
   *   `MaximumCollectionElements`
   * @param decoder  the decoder to bound
   * @return the decoder, refusing a payload that states more elements than the limit
   */
  def boundedElements[A](what: String, limit: Int)(decoder: Decoder[A]): Decoder[A] =
    Decoder.instance { cursor =>
      cursor.value.asArray match {
        case Some(elements) if elements.size > limit =>
          Left(DecodingFailure(beyondCeiling(what, elements.size.toLong, limit), cursor.history))
        case _ =>
          decoder(cursor)
      }
    }

  /**
   * Bounds how many elements the named array fields of the decoded object may state.
   *
   * This is the wrapper a product reaches for when the collections it holds are fields of its
   * object rather than its whole wire form. Each named field is examined in the order given and
   * the first one beyond its limit is the refusal, so a payload that is oversized in two fields
   * reports the first of them - one refusal, naming one field, rather than a list a reader would
   * have to interpret. The failure is positioned at the offending field, so a reader is told
   * which part of the document to correct.
   *
   * A field the object does not hold, a field whose value is not a JSON array, and a field
   * within its limit are all passed over, and a cursor whose value is not an object holds none
   * of the named fields and is therefore passed over entirely: in each case the wrapped decoder
   * runs exactly as it would have without the wrapper.
   *
   * Like `boundedElements`, every count is read from the JSON as it already stands, so nothing
   * the wrapped decoder would have allocated is allocated for a payload this refuses.
   *
   * @tparam A  the type being decoded
   * @param limits  the field names to bound, each with the greatest number of elements this
   *   port reads for it, in the order they should be examined
   * @param decoder  the decoder to bound
   * @return the decoder, refusing a payload whose named field states more elements than its
   *   limit
   */
  def boundedFields[A](limits: (String, Int)*)(decoder: Decoder[A]): Decoder[A] =
    Decoder.instance { cursor =>
      val refusal = limits.iterator
        .flatMap { case (name, limit) => fieldRefusal(cursor, name, limit).iterator }
        .nextOption()
      refusal match {
        case Some(failure) => Left(failure)
        case None => decoder(cursor)
      }
    }

  /**
   * Reports one named field of an object whose stated size is beyond what this port reads.
   *
   * The field is reached through the cursor rather than through the underlying object, so the
   * position carried by the refusal is the position of that field within the whole document,
   * however deeply the object itself is nested.
   *
   * @param cursor  the cursor being decoded
   * @param name  the name of the field to measure
   * @param limit  the greatest number of elements this port reads for that field
   * @return the refusal, or nothing if the field is absent, is not an array, or is within the
   *   limit
   */
  private def fieldRefusal(cursor: HCursor, name: String, limit: Int): Option[DecodingFailure] = {
    val field = cursor.downField(name)
    field.focus
      .flatMap(value => value.asArray)
      .filter(elements => elements.size > limit)
      .map(elements =>
        DecodingFailure(
          beyondCeiling(s"elements in the $name field", elements.size.toLong, limit),
          field.history))
  }

  //-------------------------------------------------------------------------
  /**
   * Renders the elements of an array as a JSON array, each element through `taggedDouble`.
   *
   * The elements are read one at a time, by index, through the accessor the type publishes for a
   * single element. That is the whole of what this object asks of the numeric types: neither of
   * them has a member that hands out the run of values it holds, because a member of that kind
   * would make the immutability of those types a convention rather than a property of their
   * compiled form, and encoding a value is not a reason to want one.
   *
   * @param values  the array to render
   * @return the JSON array of its elements
   */
  private def elementsJson(values: DoubleArray): Json =
    Json.fromValues(Vector.tabulate(values.size)(index => taggedDoubleEncoder(values.get(index))))

  private val doubleArrayEncoder: Encoder[DoubleArray] =
    Encoder.instance(values => elementsJson(values))

  /** Reads the elements of a JSON array, each through `taggedDouble`. */
  private val doubleElementsDecoder: Decoder[Array[Double]] =
    Decoder.decodeArray[Double](taggedDoubleDecoder, implicitly)

  // the length is taken from the payload before an element is read, so a document cannot choose
  // how much this port allocates. A payload that is not an array falls through to the element
  // reader, which reports it - and an element no double can hold - at the position where it
  // occurred, so those two refusals are unchanged by the ceiling. The run of values that reader
  // allocates is handed to the copying factory, which is the only construction path the array
  // type publishes
  private val doubleArrayDecoder: Decoder[DoubleArray] =
    Decoder.instance { cursor =>
      cursor.value.asArray match {
        case Some(elements) if elements.size > MaximumArrayElements =>
          Left(DecodingFailure(
            beyondCeiling("elements in the array", elements.size.toLong, MaximumArrayElements),
            cursor.history))
        case _ =>
          doubleElementsDecoder(cursor).map(elements => DoubleArray.copyOf(elements))
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
   * Neither direction touches the run of values an array holds. Encoding reads the elements one
   * at a time, by index, through the accessor the type publishes for a single element, and
   * decoding hands the run of elements it has just read to the copying factory - the only
   * construction path that type publishes. The immutability of these arrays is a property of
   * their compiled form rather than a convention this file could opt out of, and the two
   * aliasing members the Java original had are not ported at all, so there is nothing here for
   * the serialization layer to be careful with.
   *
   * ===How much a document may ask for===
   *
   * How long the array is, is stated by the document, so decoding measures the payload against
   * `MaximumArrayElements` before it reads a single element. A longer payload is a decoding
   * failure naming the ceiling, and nothing is allocated for it. The order matters: the ceiling
   * is worth having only if it is applied before the work it bounds, since a refusal issued
   * after the elements have been read has already paid for them.
   *
   * The ceiling does not cost the symmetry the serialization contract of this port requires.
   * The factories that build one of these arrays are total over every length the run-time can
   * allocate, so a ceiling set at a length the port could produce would make `decode(encode(x))`
   * fail for values the port itself creates and writes - a run of sensitivities, a schedule of
   * amounts, a row of currency rates. It is set orders of magnitude above every one of those,
   * for the reasons recorded at `MaximumArrayElements`, so the length of an array this port
   * wrote is never the reason a document is refused; what is refused is a document asking for
   * more than this port has any use for.
   *
   * Two other refusals are the element reader's rather than the ceiling's, and are reported
   * exactly as they would be without it: a payload that is not an array at all, and an element
   * that is not a value a double can hold. Both name the offending position.
   */
  val doubleArrayCodec: Codec[DoubleArray] = Codec.from(doubleArrayDecoder, doubleArrayEncoder)

  //-------------------------------------------------------------------------
  /** Reported when the rows of a matrix payload are not all of the same length. */
  private val RaggedMatrixMessage: String =
    "Expected every row of the matrix to hold the same number of elements"

  private val doubleArrayVectorDecoder: Decoder[Vector[DoubleArray]] =
    Decoder.decodeVector(doubleArrayDecoder)

  /**
   * Renders one row of a matrix as a JSON array, each element through `taggedDouble`.
   *
   * The elements are read by row and column index, for the reason `elementsJson` reads an array
   * by index: the matrix publishes no member that hands out the rows it holds, and a row copied
   * out to be read once would be an allocation per row with nothing to show for it.
   *
   * @param matrix  the matrix to read
   * @param row  the zero-based row index to render
   * @return the JSON array of that row's elements
   */
  private def rowJson(matrix: DoubleMatrix, row: Int): Json =
    Json.fromValues(
      Vector.tabulate(matrix.columnCount)(column => taggedDoubleEncoder(matrix.get(row, column))))

  private val doubleMatrixEncoder: Encoder[DoubleMatrix] =
    Encoder.instance { matrix =>
      Json.fromValues(Vector.tabulate(matrix.rowCount)(row => rowJson(matrix, row)))
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
   * width, the product of the two dimensions and the agreement between the rows decided. The
   * product is computed in a width that the product of two counts cannot exceed, so a stated
   * shape cannot wrap its way past the ceiling on the elements.
   *
   * Nothing here reads an element, so a payload refused for its shape costs no run of values at
   * all: the widths of the rows are visible in the payload itself, which is what lets rows that
   * cannot describe one rectangle be found and reported before any of them is read.
   *
   * A row that is not an array has no width to offer and is passed over: what is wrong with
   * such a payload is the row itself, which the reader of that row reports precisely, at that
   * row's own position. Raggedness is therefore judged only when every row could be measured,
   * which is exactly when the judgement is sound.
   *
   * @param rowsJson  the rows the payload states
   * @param history  the position within the document, taken from the decoding cursor
   * @return the refusal, or nothing if the stated shape is one this port reads and its rows
   *   agree
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
      // the shape comes from the payload, so it is measured against the ceilings and checked for
      // rows that agree before any of them is read; a payload that is not an array falls through
      // to the row reader, which reports it exactly as it always did
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
   * A matrix is an array of rows, and each row is an array of elements in the shape
   * `doubleArrayCodec` produces, so every element again goes through `taggedDouble`. A
   * two-by-two matrix is the array of its two rows, `[1.0, 2.0]` and then `[3.0, 4.0]`. A
   * matrix with no elements is `[]`.
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
   * None of the three ceilings costs the symmetry this port's serialization contract requires,
   * for the reason given at `doubleArrayCodec`: the widest numeric structure any type of this
   * port holds is a square of currency rates, of one row and one column per currency in play,
   * so every matrix the port creates and writes is read back by a decoder bounded this way,
   * and what the ceilings refuse is a shape the port has no use for.
   *
   * The rows are read only once the stated shape is one this port accepts, and the rows that
   * result are measured once more before the matrix is assembled. That second look is not a
   * repetition of the first: it is what keeps assembly total, since the factory reached at that
   * point answers a row of the wrong length by raising an error rather than reporting one, and
   * no payload may be able to reach it.
   *
   * The rows are handed to the existing factory that assembles a matrix from them, so the
   * nested structure that a matrix keeps internally is built in the one file that owns it.
   * That is a deliberate boundary and not an incidental one: allocating a nested structure
   * whose elements are themselves runs of values is the one allocation in this language that
   * reaches for the very run-time type machinery the serialization of this library is required
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
   * object is mentioned - which matters most for the one type the JSON library also publishes
   * an instance for, the double: the instance here has to take precedence over the plain
   * numeric one, and an import is what gives it that precedence.
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
