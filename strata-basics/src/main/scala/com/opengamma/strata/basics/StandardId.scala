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
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.ResultNec
import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.ValidatedFailures
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * An immutable standard identifier for an item.
 *
 * A standard identifier uniquely identifies a domain object. It is formed from two parts,
 * the scheme and the value.
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
 * An identifier renders as `scheme~value`, and that string is the identity users and
 * stored documents rely on:
 *
 * {{{
 * StandardId.of("OG-Ticker", "AAPL").map(_.toString)   // Right("OG-Ticker~AAPL")
 * StandardId.parse("OG-Ticker~AAPL")                   // Right(the same identifier)
 * }}}
 *
 * `toString` and [[StandardId.parse]] are inverses of one another, and the character sets
 * below are what makes that true: the separator is excluded from the characters a value may
 * hold, so the first `~` in the text is always the one that separates the two parts. The
 * JSON form is the same string, so an identifier in a stored document is readable as it
 * stands and is read back by the same parse.
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
sealed abstract case class StandardId private (scheme: String, value: String)
    extends NoJavaSerialization {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means - carrying
  // a scheme or a value the checks of `of` would have refused - can be stopped is here. The
  // single implementation is the companion's hidden `Impl`.
  JvmClosure.requireSoleImplementation(this, classOf[StandardId.Impl])

  // The invariant of this type, stated over the two fields the instance actually holds rather than
  // over the arguments a factory was given, because the class file of the implementation carries a
  // public constructor whatever the source asked for: a class compiled outside this library can
  // call it directly, and identity alone would then admit an identifier holding a scheme or a
  // value outside the character sets this type fixes - one carrying the separator, say, which
  // would make `toString` and `parse` cease to be inverse, or a control character, which would
  // make it unprintable. Both statements are what the checks of `of` establish, over the same
  // predicates those checks are written with.
  JvmClosure.requireInvariant(
    "its scheme is not empty and holds only the characters a scheme may hold",
    scheme.nonEmpty && scheme.forall(character => StandardId.SchemeCharacter(character)))
  JvmClosure.requireInvariant(
    "its value is not empty, holds only the characters a value may hold, and does not begin with " +
      "a space",
    value.nonEmpty && value.forall(character => StandardId.ValueCharacter(character)) &&
      !value.startsWith(" "))

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
 * ===The permitted characters are functions over `Char`===
 *
 * The characters a scheme and a value may hold, and the safe set [[encodeScheme]] leaves
 * alone, are ordinary predicates held in precomputed fields, so each set is readable in one
 * place and the type depends on nothing beyond the standard library and the `collect`
 * module. The regular expressions the failures quote are message text stating the same sets
 * in the notation a caller correcting its input will recognise.
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
   * The set deliberately excludes percent: an input percent has to become `%25`, or
   * decoding the result would not give the input back.
   */
  private val EscapeSafeCharacters: Set[Char] = Set(':', '/', '+', '.', '=', '_', '-')

  /** The regular expression a rejected scheme is described against. */
  private val SchemeRegex: String = "[A-Za-z0-9:/+.=_%-]+"

  /**
   * The regular expression a rejected value is described against.
   *
   * This is message text rather than the language a value has to be in: the `+` form is
   * what a rejection quotes, while a value of one character is accepted. The language
   * actually accepted is `[!-z][ -z]*`, which is what [[of]] documents and what
   * [[ValueCharacter]] and the length bounds of [[checkedValue]] enforce.
   */
  private val ValueRegex: String = "[!-z][ -z]+"

  /** The digits an escaped byte is written with, upper case as a percent escape is written. */
  private val UpperHexDigits: String = "0123456789ABCDEF"

  /**
   * The longest a scheme or a value may be.
   *
   * Neither part of an identifier has a length the grammar fixes: a scheme is one or more
   * permitted characters and a value is one or more, so the only bound available is one this
   * type states. The library being ported stated none, and this port carried that over as a
   * maximum of `Int.MaxValue`, which meant a part of any size was scanned character by
   * character, stored on the instance and quoted into every failure and rendering that named it
   * (CWE-400/CWE-770).
   *
   * The bound is 65,536 characters, which is chosen from what an identifier is used for rather
   * than from what a machine can hold. Identifiers in this library name instruments, schemes,
   * tickers and exchange codes - tens of characters each - and the largest legitimate one the
   * test suite exercises is ten thousand characters, a value asserted elsewhere to be "a
   * perfectly legal identifier"; sixty-five thousand is six times that and still small enough
   * that scanning it, holding it and bounding it where it is written are all cheap. Nothing a
   * caller means to identify anything with comes near it.
   *
   * It is deliberately far above the ceilings of the period grammars of this port, which fix
   * their length at 256: there, text of more than a few dozen characters cannot name the thing
   * at all, while here a long identifier is merely unusual.
   */
  private val MaxPartLength: Int = 65536

  /**
   * The longest text [[parse]] reads, which is the longest text an identifier renders to.
   *
   * This is derived rather than chosen, and it has to be: the class documentation states that
   * `toString` and [[parse]] are inverses of one another, and a ceiling on the text that is
   * lower than the longest text the factories can produce would break that inverse rather than
   * bound it. Two parts of [[MaxPartLength]] characters are admitted by `of`, and they render as
   * `scheme~value` - so the longest text any value of this type can render to is exactly two
   * parts and the separator between them, and that is what this is.
   *
   * The bound therefore does the one job a pre-work bound has to do - a sender cannot ask for
   * work proportional to a text of its own choosing, since text past this is refused by one
   * comparison of a length before the separator is looked for - while admitting every text a
   * value this type holds can be written as. `parse` of the rendering of any value the factories
   * accept is that value, at every size up to and including the largest, which is asserted at
   * the boundary rather than left to a generator that draws short parts.
   */
  private val MaxTextLength: Int = 2 * MaxPartLength + 1

  /**
   * Reported for a part, or for text, longer than its ceiling.
   *
   * The message names the argument and the ceiling and '''not''' the text: the part is refused
   * precisely for its size, so quoting it would be the very thing the refusal exists to avoid,
   * and what the caller has to correct is the length rather than the spelling. This is the
   * shape [[com.opengamma.strata.collect.Decimal]] reports for the same condition on the numeral
   * it reads, with the name of the argument in place of its type name.
   *
   * @param name  the name of the argument that was too long, as the other failures of that
   *   argument name it
   * @return the message describing the ceiling
   */
  private def tooLongMessage(name: String): String =
    s"Argument '$name' must not exceed $MaxPartLength characters"

  /**
   * Reported by [[parse]] for text longer than [[MaxTextLength]] characters.
   *
   * A parse has no argument to name - the text is the whole of what it was given - so the
   * wording is the one [[com.opengamma.strata.collect.Decimal]] reports for the same condition,
   * naming what was being read and the ceiling and nothing else. The text is not quoted, for the
   * reason given on [[tooLongMessage]].
   *
   * The number here is the text ceiling and not the part ceiling, and the two differ: text that
   * is past this names two parts that cannot both be held, while text that is merely past the
   * part ceiling names one part that cannot be held and is reported as that part, by the checks
   * `of` performs on the two parts this reads out.
   */
  private val TextTooLongMessage: String =
    s"Identifier string must not exceed $MaxTextLength characters"

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
   * not begin with a space. One character is therefore enough; the `+` form the failures
   * quote is message text and not the language accepted here.
   *
   * Both parts are checked, and the outcome carries a failure for each one that was
   * unacceptable rather than only the first:
   *
   * {{{
   * StandardId.of("{", "")   // Left(two failures: one for the scheme, one for the value)
   * }}}
   *
   * Each failure quotes its part back exactly as it was given, so the caller is handed the
   * whole of what was refused. The two parts came from outside the library, so making them
   * safe to write out belongs to the writing:
   * [[com.opengamma.strata.collect.result.Failure.show]] and the text form of a failure bound
   * every part they write and escape anything a line-oriented reader could act on.
   *
   * @param scheme  the scheme of the identifier, not empty
   * @param value  the value of the identifier, not empty
   * @return the identifier, or the failures describing why the parts were not acceptable
   */
  def of(scheme: String, value: String): ResultNec[StandardId] =
    (checkedScheme(scheme), checkedValue(value))
      .mapN((validScheme, validValue) => new Impl(validScheme, validValue): StandardId)
      .toEither

  /**
   * The one implementation of an identifier.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it. It is declared here rather than written as an anonymous subclass at the instantiation
   * site for two reasons, both about what the class file says: a private member class is one a
   * compiler in another language refuses to name, where an anonymous class is public and can be
   * instantiated directly by such a caller; and a named class can be compared against, which is
   * what lets [[StandardId]] refuse in its own constructor to be any other implementation.
   *
   * @param scheme  the scheme, already accepted by the check of [[of]]
   * @param value  the value, already accepted by the check of [[of]]
   */
  private final class Impl(scheme: String, value: String) extends StandardId(scheme, value)

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
   * separator, or the wording of the part checks it delegates to - is quoted as it was given.
   * The text came from outside the library, so bounding it and escaping what it may hold
   * belong to the writing of a failure, which
   * [[com.opengamma.strata.collect.result.Failure.show]] and the text form of a failure
   * perform for every part they write.
   *
   * Text longer than [[MaxPartLength]] characters is refused before any of that, naming the
   * ceiling rather than the text. The bound is over the whole text here, where [[of]] applies it
   * to each part, which makes this route the stricter of the two by exactly the length of the
   * other part - a deliberate choice, since the text of an identifier is one thing a caller
   * supplies and one thing that has to be written out again. Refusing it first is what keeps the
   * two substrings, the character walks of both parts and the interpolation of a rejection off
   * text written to be large (CWE-400/CWE-770).
   *
   * @param str  the identifier text to parse
   * @return the identifier, or the failure describing why the text names none
   */
  def parse(str: String): FailureOr[StandardId] =
    // The ceiling is the longest text a value of this type renders to, so it bounds the work
    // this does without narrowing what it reads: every rendering of every value `of` admits is
    // inside it, and the two parts it reads out are held to their own ceiling by `of` itself.
    if (str.length > MaxTextLength) {
      Left(Failure.Parsing(TextTooLongMessage))
    } else {
      val separator = str.indexOf("~")
      if (separator < 0) {
        // the text is rendered rather than interpolated as it stands, which bounds the message
        // and keeps it to one line while leaving in-bound text quoted as it was given
        Left(Failure.Parsing(s"Invalid identifier format: $str"))
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
   * an ASCII digit or one of `:` `/` `+` `.` `=` `_` `-` becomes `%` followed by two
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
   * text, so that is the caller's case to rule out rather than a failure this method reports.
   *
   * Malformed text is encoded rather than refused: a surrogate character that is not part of
   * a pair is escaped as the byte the UTF-8 encoder substitutes for it, so encoding is a
   * total function of its argument.
   *
   * @param scheme  the text to encode
   * @return the encoded scheme
   */
  def encodeScheme(scheme: String): String = escapeFrom(scheme, 0, Nil)

  /**
   * The ordering of identifiers, which is also their hashing.
   *
   * Identifiers sort alphabetically by scheme and then by value. This is the only
   * equality-bearing instance of the type: `Order` and `Hash` both extend `Eq`, so the three
   * can never disagree. Equality is that of the values themselves - the scheme and the value,
   * both of them text - so comparison returns zero exactly when two identifiers are equal and
   * no further tie-break is needed.
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
   * Length and characters are one check, because what the caller has to correct is the same
   * in either case.
   *
   * The ceiling of [[MaxPartLength]] is tested ahead of that check, and reported on its own,
   * for two reasons: the character check walks the whole of the scheme, so a part that is
   * refused for its size is refused before it is walked, and its own message would quote the
   * scheme it refused. The two checks are sequential rather than accumulating, so a scheme past
   * the ceiling is described by the ceiling alone; accumulation happens across the scheme and
   * the value, which are the two arguments the caller supplied.
   *
   * @param scheme  the scheme to check
   * @return the scheme, or the failure describing why it is not acceptable
   */
  private def checkedScheme(scheme: String): ValidatedFailures[String] =
    Validate
      .cond(scheme.length <= MaxPartLength, scheme, Failure.Invalid(tooLongMessage("scheme")))
      .andThen(checked =>
        Validate.matches(SchemeCharacter, 1, MaxPartLength, checked, "scheme", SchemeRegex))

  /**
   * Checks a value, reporting what was wrong with it.
   *
   * The three checks are sequential rather than accumulating: the ceiling of [[MaxPartLength]]
   * is tested first, so a value refused for its size is refused before it is walked and without
   * its own text being quoted, and the leading character is only worth describing once the value
   * is known to be non-empty and to hold permitted characters throughout. A value that fails an
   * earlier check is therefore described by that failure alone. Accumulation happens across the
   * scheme and the value, which are the two arguments the caller supplied, and this keeps each
   * of them to a single failure.
   *
   * A value within the ceiling is quoted as it stands, and the character check the leading-space
   * test follows quotes its own argument the same way, so both failures of this part name the
   * whole of what was refused and both are bounded alike when the failure is written out.
   *
   * @param value  the value to check
   * @return the value, or the failure describing why it is not acceptable
   */
  private def checkedValue(value: String): ValidatedFailures[String] =
    Validate
      .cond(value.length <= MaxPartLength, value, Failure.Invalid(tooLongMessage("value")))
      .andThen(checked => Validate.matches(ValueCharacter, 1, MaxPartLength, checked, "value", ValueRegex))
      .andThen(checked =>
        Validate.cond(
          !checked.startsWith(" "),
          checked,
          Failure.Invalid(s"Invalid initial space in value '$checked' must match regex '$ValueRegex'")))

  /**
   * The codec both JSON instances are taken from.
   *
   * The pair is derived from `parse` and `toString`, which is what makes the written form and
   * the read form the same by construction. It is exposed as the two instances above rather
   * than as one, so that each is summoned by the type it belongs to.
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
   * @param escaped  the pieces produced up to this index, most recent first
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
