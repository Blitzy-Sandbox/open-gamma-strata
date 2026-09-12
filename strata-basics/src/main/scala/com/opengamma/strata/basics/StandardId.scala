/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import java.nio.charset.StandardCharsets

import scala.annotation.tailrec

import cats.Hash
import cats.Order
import cats.Show
import cats.syntax.apply._

import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder

import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.ValidatedFailures
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * An immutable standard identifier for an item.
 *
 * A standard identifier is used to uniquely identify domain objects. It is formed from two
 * parts, the scheme and the value.
 *
 * The scheme defines a single way of identifying items, while the value is an identifier
 * within that scheme. A value from one scheme may refer to a completely different real-world
 * item than the same value from a different scheme.
 *
 * Real-world examples of `StandardId` include instances of:
 *
 *   - Cusip
 *   - Isin
 *   - Reuters RIC
 *   - Bloomberg BUID
 *   - Bloomberg Ticker
 *   - Trading system OTC trade ID
 *
 * ===The text form is the contract===
 *
 * An identifier renders as `scheme~value`, and that string is the identity users, stored
 * documents and tests rely on:
 *
 * {{{
 * StandardId.of("OG-Ticker", "AAPL").map(_.toString)   // Right("OG-Ticker~AAPL")
 * StandardId.parse("OG-Ticker~AAPL")                   // Right(the same identifier)
 * }}}
 *
 * `toString` and [[StandardId.parse]] are inverses of one another, and the character sets
 * below are what makes that true: the separator is excluded from the characters a value may
 * hold, so the first `~` in the text is always the one that separates the two parts. The
 * JSON form is the same string, so a document holding an identifier is readable and stays
 * comparable with one written by the library this type is ported from.
 *
 * ===Obtaining one===
 *
 * The constructor is private and no `apply` or `copy` exists, so [[StandardId.of]] and
 * [[StandardId.parse]] are the only ways to obtain an identifier. Both report what was
 * wrong with their input rather than interrupting the caller, which is why every identifier
 * in existence satisfies the constraints of the type and why nothing downstream has to
 * re-check them.
 *
 * This class is immutable and thread-safe.
 *
 * @param scheme  the scheme that categorizes the identifier value, which provides the
 *   universe within which the identifier value has meaning
 * @param value  the value of the identifier within the scheme
 */
sealed abstract case class StandardId private (scheme: String, value: String) {

  /**
   * Returns the identifier in a standard string format.
   *
   * The returned string is in the form `scheme~value`, which is the form
   * [[StandardId.parse]] reads back and the form the JSON codec writes.
   *
   * @return a parsable representation of this identifier
   */
  override def toString: String = scheme + "~" + value
}

/**
 * Provides the two ways of obtaining an identifier, the encoding of text into a usable
 * scheme, and the instances for the type.
 *
 * ===What replaced the character matchers===
 *
 * The library being ported described the permitted characters with matcher objects built by
 * a third-party collection library, and the escaping of a scheme with that library's
 * percent-escaper. Both are expressed here as ordinary functions over `Char` and `Int`
 * held in precomputed fields, so the type depends on nothing but the standard library and
 * the two modules of this port, and so the permitted sets are readable in one place.
 *
 * The failures reported quote the same regular expressions the original quoted, word for
 * word, because that text reaches logs and test expectations.
 */
object StandardId {

  /** The first code point that is not ASCII, the boundary the escaper's safe set ends at. */
  private val AsciiLimit: Int = 0x80

  /**
   * The characters a scheme may hold in addition to ASCII letters and digits.
   *
   * Percent is among them so that the escapes [[encodeScheme]] emits are themselves
   * permitted scheme characters, which is the whole point of that method.
   */
  private val SchemeSpecialCharacters: Set[Char] = Set(':', '/', '+', '.', '=', '_', '-', '%')

  /**
   * The characters [[encodeScheme]] leaves alone in addition to ASCII letters and digits.
   *
   * This is the safe set of the escaper being replaced, and it deliberately excludes
   * percent: an input percent has to become `%25`, or decoding the result would not give
   * the input back.
   */
  private val EscapeSafeCharacters: Set[Char] = Set(':', '/', '+', '.', '=', '_', '-')

  /** The regular expression a rejected scheme is described against. */
  private val SchemeRegex: String = "[A-Za-z0-9:/+.=_%-]+"

  /**
   * The regular expression a rejected value is described against.
   *
   * This is message text, not the language a value has to be in: the original quoted the `+`
   * form in its failures while accepting a value of one character, and that text is
   * reproduced here word for word because it reaches logs and test expectations. The
   * language actually accepted is `[!-z][ -z]*`, which is what [[of]] documents and what
   * [[ValueCharacter]] and the length bounds of [[checkedValue]] enforce.
   */
  private val ValueRegex: String = "[!-z][ -z]+"

  /** The digits an escaped byte is written with, upper case as the escaper being replaced wrote them. */
  private val UpperHexDigits: String = "0123456789ABCDEF"

  /**
   * Accepts exactly the characters a scheme may hold.
   *
   * The set is ASCII letters, ASCII digits and [[SchemeSpecialCharacters]], which is the
   * regular expression `[A-Za-z0-9:/+.=_%-]` written as a function. The predicate is a
   * field rather than a method so that the function object is built once.
   */
  private val SchemeCharacter: Char => Boolean =
    character => isAsciiLetterOrDigit(character) || SchemeSpecialCharacters.contains(character)

  /**
   * Accepts exactly the characters a value may hold.
   *
   * The set is the inclusive range from space to lower-case `z`, which is the printable
   * ASCII characters without the four highest: opening brace, pipe, closing brace and
   * tilde. Excluding the tilde is what keeps `toString` and [[parse]] inverse, and
   * excluding the control characters below space is what keeps an identifier printable.
   */
  private val ValueCharacter: Char => Boolean =
    character => character >= ' ' && character <= 'z'

  /**
   * Obtains an instance from a scheme and a value.
   *
   * The scheme must be non-empty and match the regular expression `[A-Za-z0-9:/+.=_%-]+`.
   * This permits letters, digits, colon, forward slash, plus, dot, equals, underscore,
   * dash and percent. Text that is not a permitted scheme can be turned into one with
   * [[encodeScheme]].
   *
   * The value must be non-empty and match the regular expression `[!-z][ -z]*`. This is the
   * printable ASCII characters excluding curly brackets, pipe and tilde, and a value may
   * not begin with a space. One character is therefore enough, as it is in the library this
   * type is ported from; the `+` form the failures quote is that library's message text and
   * not the language accepted here.
   *
   * Both parts are checked, and the outcome carries a failure for each one that was
   * unacceptable rather than only the first:
   *
   * {{{
   * StandardId.of("{", "")   // Left(two failures: one for the scheme, one for the value)
   * }}}
   *
   * Each part a failure quotes back is rendered through
   * [[com.opengamma.strata.collect.result.Failure.describeInput]], so every message is bounded
   * in length and has its control characters escaped. A message reaches a log or a report, and
   * the two parts handed to this factory came from outside the library, so neither must be
   * able to forge a line of that log or to make the message as large as the part. A part within
   * the bound and free of control characters is quoted exactly as it was given, so the wording
   * of an ordinary rejection is unchanged; only a longer or a line-breaking part is now
   * described rather than reproduced.
   *
   * @param scheme  the scheme of the identifier, not empty
   * @param value  the value of the identifier, not empty
   * @return the identifier, or the failures describing why the parts were not acceptable
   */
  def of(scheme: String, value: String): ResultNec[StandardId] =
    (checkedScheme(scheme), checkedValue(value))
      .mapN((validScheme, validValue) => new StandardId(validScheme, validValue) {})
      .toEither

  /**
   * Parses an identifier from a formatted scheme and value.
   *
   * This reads the form `toString` produces, which is `scheme~value`, splitting at the
   * first tilde. Since a value may not hold a tilde, later tildes are rejected along with
   * the value that holds them rather than being treated as separators - which is why
   * `a~b~c` names no identifier.
   *
   * The failures of the two parts are presented as one, because text is a single thing to
   * correct: several causes are joined into one message and no cause is dropped.
   *
   * The text a failure quotes back - whether this method's own wording for text holding no
   * separator, or the wording of the part checks it delegates to - is rendered through
   * [[com.opengamma.strata.collect.result.Failure.describeInput]], so it is bounded in length
   * and its control characters are escaped. A message reaches a log or a report, and the text
   * handed to this method came from outside the library, so it must not be able to forge a
   * line of that log or to make the message as large as the input. Text within the bound and
   * free of control characters - every rendering of an identifier among them - is quoted
   * exactly as it was given, so the wording of an ordinary rejection is unchanged; only a
   * longer or a line-breaking input is now described rather than reproduced.
   *
   * @param str  the identifier text to parse
   * @return the identifier, or the failure describing why the text names none
   */
  def parse(str: String): FailureOr[StandardId] = {
    val separator = str.indexOf("~")
    if (separator < 0) {
      // the text is rendered rather than interpolated as it stands, which bounds the message
      // and keeps it to one line while leaving in-bound text quoted as it was given
      Left(Failure.Parsing(s"Invalid identifier format: ${Failure.describeInput(str)}"))
    } else {
      of(str.substring(0, separator), str.substring(separator + 1))
        .left
        .map(failures => Failure.collapse(failures))
    }
  }

  /**
   * Encodes text so that it is usable as a scheme.
   *
   * This is percent encoding, as a URI uses: every character that is not an ASCII letter,
   * an ASCII digit or one of `:` `/` `+` `.` `=` `_` `-` is replaced by `%` followed by two
   * upper-case hexadecimal digits per byte of its UTF-8 form. A space therefore becomes
   * `%20` rather than a plus sign, a tilde becomes `%7E`, and a character outside ASCII
   * becomes one escape group per UTF-8 byte:
   *
   * {{{
   * StandardId.encodeScheme("https://opengamma.com/foo/../~bar#test")
   * // "https://opengamma.com/foo/../%7Ebar%23test"
   * }}}
   *
   * Every character the encoding can emit is a permitted scheme character, percent among
   * them, so text holding at least one character encodes to a scheme [[of]] accepts
   * unchanged, whatever that text held. Empty text is the single exception: it encodes to
   * empty text, which [[of]] rejects because a scheme may not be empty. Encoding is text to
   * text here, as it is in the library being ported, so that is the caller's case to rule
   * out rather than a failure this method reports.
   *
   * The one behaviour that differs from the escaper being replaced is malformed text: where
   * the original rejected a surrogate character that is not part of a pair, this escapes
   * the byte the UTF-8 encoder substitutes for it, so that encoding is a total function of
   * its argument as the error-handling policy of this port requires.
   *
   * @param scheme  the text to encode
   * @return the encoded scheme
   */
  def encodeScheme(scheme: String): String = escapeFrom(scheme, 0, Nil)

  /**
   * The ordering of identifiers, which is also their hashing.
   *
   * Identifiers sort alphabetically by scheme and then by value, which is the comparison of
   * the type being ported. This is the only equality-bearing instance of the type: `Order`
   * and `Hash` both extend `Eq`, so the three can never disagree. Equality is that of the
   * values themselves - the scheme and the value, both of them text - so comparison returns
   * zero exactly when two identifiers are equal and no further tie-break is needed.
   *
   * @return the ordering of identifiers
   */
  implicit val order: Order[StandardId] with Hash[StandardId] =
    new Order[StandardId] with Hash[StandardId] {

      private val universal: Hash[StandardId] = Hash.fromUniversalHashCode[StandardId]

      override def compare(x: StandardId, y: StandardId): Int = {
        val bySchemeComparison = x.scheme.compareTo(y.scheme)
        if (bySchemeComparison != 0) bySchemeComparison else x.value.compareTo(y.value)
      }

      override def eqv(x: StandardId, y: StandardId): Boolean = universal.eqv(x, y)

      override def hash(x: StandardId): Int = universal.hash(x)
    }

  /**
   * The rendering of identifiers as text.
   *
   * An identifier renders in the `scheme~value` form, as `toString` does, so the rendered
   * form is the parsable one.
   *
   * @return the rendering of an identifier
   */
  implicit val show: Show[StandardId] = Show.show(_.toString)

  /**
   * The JSON encoder for identifiers.
   *
   * An identifier is written as the bare string of its `scheme~value` form.
   *
   * @return the encoder writing an identifier as its canonical text
   */
  implicit val encoder: Encoder[StandardId] = codec

  /**
   * The JSON decoder for identifiers.
   *
   * The string is read through [[parse]], so a document holding text that names no
   * identifier is rejected with the message of the parse failure.
   *
   * @return the decoder reading an identifier from its canonical text
   */
  implicit val decoder: Decoder[StandardId] = codec

  /**
   * Checks a scheme, reporting what was wrong with it.
   *
   * Length and characters are one check, as they were in the original, because what the
   * caller has to correct is the same in either case.
   *
   * @param scheme  the scheme to check
   * @return the scheme, or the failure describing why it is not acceptable
   */
  private def checkedScheme(scheme: String): ValidatedFailures[String] =
    Validate.matches(SchemeCharacter, 1, Int.MaxValue, scheme, "scheme", SchemeRegex)

  /**
   * Checks a value, reporting what was wrong with it.
   *
   * The two checks are sequential rather than accumulating: the leading character is only
   * worth describing once the value is known to be non-empty and to hold permitted
   * characters throughout, so a value that fails the first check is described by that
   * failure alone. Accumulation happens across the scheme and the value, which are the two
   * arguments the caller supplied, and this keeps each of them to a single failure.
   *
   * The value is rendered through [[Failure.describeInput]] rather than interpolated as it
   * stands, which bounds the message and keeps it to one line; an in-bound value free of
   * control characters renders to itself, so the wording is unchanged for every value a caller
   * would sensibly offer. The check the leading-space test follows renders its own argument the
   * same way, so both failures of this part are bounded alike.
   *
   * @param value  the value to check
   * @return the value, or the failure describing why it is not acceptable
   */
  private def checkedValue(value: String): ValidatedFailures[String] =
    Validate
      .matches(ValueCharacter, 1, Int.MaxValue, value, "value", ValueRegex)
      .andThen(checked =>
        Validate.cond(
          !checked.startsWith(" "),
          checked,
          Failure.Invalid(
            s"Invalid initial space in value '${Failure.describeInput(checked)}' " +
              s"must match regex '$ValueRegex'")))

  /**
   * The codec both JSON instances are taken from.
   *
   * The pair is built by the compiler from `parse` and `toString`, which is what makes the
   * written form and the read form the same by construction. It is exposed as the two
   * instances above rather than as one, so that each is summoned by the type it belongs to.
   *
   * Being lazy, it is built once however many instances read it, and it is immune to the
   * order in which the fields of this object are declared.
   *
   * @return the codec reading and writing an identifier as its canonical text
   */
  private lazy val codec: Codec[StandardId] =
    Codecs.parsedStringCodec[StandardId](text => parse(text), identifier => identifier.toString)

  /**
   * Whether the specified character is an ASCII letter or an ASCII digit.
   *
   * The three ranges are tested directly rather than through a character-class method of
   * the platform, because those methods accept letters and digits of every script and the
   * sets this type permits are the ASCII ones.
   *
   * @param character  the character to test
   * @return true if the character is an ASCII letter or digit
   */
  private def isAsciiLetterOrDigit(character: Char): Boolean =
    (character >= 'A' && character <= 'Z') ||
      (character >= 'a' && character <= 'z') ||
      (character >= '0' && character <= '9')

  /**
   * Whether the specified code point is left alone by [[encodeScheme]].
   *
   * Only ASCII code points can be safe, so the range is tested before the character sets
   * are consulted.
   *
   * @param codePoint  the code point to test
   * @return true if the code point needs no escaping
   */
  private def isEscapeSafe(codePoint: Int): Boolean =
    codePoint < AsciiLimit && {
      val character = codePoint.toChar
      isAsciiLetterOrDigit(character) || EscapeSafeCharacters.contains(character)
    }

  /**
   * Escapes the text of one code point as percent-escaped UTF-8 bytes.
   *
   * The text is handed over whole rather than as a code point so that a surrogate pair is
   * encoded as the four bytes of the character it forms, and so that a surrogate without a
   * partner is encoded as whatever the platform's UTF-8 encoder substitutes rather than
   * failing.
   *
   * @param text  the text of a single code point
   * @return the escaped form, one group of three characters per byte
   */
  private def percentEscape(text: String): String =
    text.getBytes(StandardCharsets.UTF_8).iterator.map(singleByte => escapeByte(singleByte)).mkString

  /**
   * Escapes one byte as a percent followed by two upper-case hexadecimal digits.
   *
   * @param singleByte  the byte to escape
   * @return the three characters the byte escapes to
   */
  private def escapeByte(singleByte: Byte): String = {
    val unsigned = java.lang.Byte.toUnsignedInt(singleByte)
    "%" + UpperHexDigits.charAt(unsigned >>> 4) + UpperHexDigits.charAt(unsigned & 0xF)
  }

  /**
   * Escapes the remainder of the specified text, one code point at a time.
   *
   * The walk is by code point rather than by character so that a character outside the
   * basic plane is escaped as one unit, and it is tail recursive so that text of any length
   * is escaped in constant stack space. The pieces are accumulated in reverse and joined
   * once, which keeps the whole method free of mutable state.
   *
   * @param scheme  the text being encoded
   * @param index  the index to continue at
   * @param escaped  the pieces produced so far, most recent first
   * @return the encoded text
   */
  @tailrec
  private def escapeFrom(scheme: String, index: Int, escaped: List[String]): String =
    if (index >= scheme.length) {
      escaped.reverse.mkString
    } else {
      val codePoint = scheme.codePointAt(index)
      val width = Character.charCount(codePoint)
      val character = scheme.substring(index, index + width)
      val piece = if (isEscapeSafe(codePoint)) character else percentEscape(character)
      escapeFrom(scheme, index + width, piece :: escaped)
    }
}
