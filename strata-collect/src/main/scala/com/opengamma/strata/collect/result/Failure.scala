/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.result

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

import cats.Hash
import cats.Show
import cats.data.NonEmptyChain
import cats.data.NonEmptyList

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

/**
 * A single failure, describing why an operation did not produce a value.
 *
 * A failure carries a [[FailureReason]] that classifies it, a message written for a person
 * reading a log or a report, and a map of attributes holding the data the message refers to
 * in machine-readable form. Nothing else: a failure is a value, not an event, so it holds
 * no stack trace, no cause and no exception type, and it is never thrown. Where the Java
 * original modelled the same information as a bean that could be wrapped in an exception,
 * this port keeps the failure on the left of an `Either` and leaves the decision of what to
 * do about it to the caller.
 *
 * That is the convention of this package, carried over from the package it is ported from:
 * code here is written in a functional style, and an operation that cannot produce a result
 * returns a failure describing why instead of abandoning the call stack. A failure is
 * therefore constructed, returned, matched on, combined and serialized like any other
 * value, and building one never fails - it is the type in which every other failure of the
 * library is expressed, so it has no validation of its own to fail. A message is expected
 * to be non-empty by the code that reads it, as it was in the type being ported, but that
 * expectation is a convention of the caller and is not enforced here.
 *
 * ===The closed set of failures===
 *
 * The type is `sealed` and every member is a `final case class` declared in the companion,
 * one per reason, so the set of failures is closed and a `match` over it is checked for
 * exhaustiveness. Choosing a member is therefore the same act as choosing a reason, and the
 * two cannot drift apart: `reason` is fixed by the member and is not a constructor
 * parameter, which makes a failure whose reason contradicts its class impossible to build.
 *
 * ===Attributes===
 *
 * The attributes are a `SortedMap`, ordered by key rather than by insertion, so that two
 * failures carrying the same attributes render and serialize identically no matter how each
 * was assembled. That is what makes the JSON form of a failure byte-stable, which in turn
 * is what allows a stored failure to be compared with a newly produced one.
 *
 * Attributes are added with `withAttribute`, which returns a new failure of the same class:
 *
 * {{{
 * Failure.Invalid("Schedule is invalid").withAttribute("definition", "P3M from 2024-01-15")
 * }}}
 *
 * The attribute names are ordinary strings, chosen by the code that reports the failure;
 * there is no enumeration of permitted names to consult or extend.
 *
 * ===Messages quote what was rejected; the rendering neutralises it===
 *
 * A message names the value it is about as that value stands - the whole of the currency
 * code, identifier, name, numeral or definition that was rejected - which is what the
 * library being ported did and what makes a failure worth acting on: the caller correcting
 * its input is handed back exactly what the library refused, and code that has to compare,
 * re-report or serialize a failure reads it from `message` and `attributes` unchanged.
 *
 * Text that reached the library from outside it is therefore inside the model, and making it
 * safe to write out is the act of writing it out. That happens in one place, [[Failure.show]]
 * and the text form every member delegates to it: each part of a failure is rendered bounded
 * in length and free of any character a line-oriented reader could act on, so a value
 * supplied from outside cannot forge a line of a log that holds the failure (CWE-117) or make
 * that line as large as itself. A message and an attribute of a realistic size render to
 * themselves character for character, so a reader sees what the reporter wrote.
 *
 * ===Choosing a member===
 *
 * The member is chosen by what went wrong, and the port keeps to one convention so that a
 * caller can act on a failure it did not itself report. A definition that does not describe
 * a consistent schedule is `Invalid`, carrying the definition it rejected under the
 * `definition` attribute; a holiday calendar that the reference data cannot resolve is
 * `MissingData`; a conversion for which no rate between the two currencies is available is
 * `CurrencyConversion`; and text that names no value of the type expected - a name, a
 * currency code, a tenor - is `Parsing`. `Error` and `Other` are the general-purpose
 * members and are appropriate only where none of the specific ones is.
 *
 * ===Combining failures===
 *
 * An operation that can fail in more than one way reports a chain of failures and leaves
 * each one intact, so multiplicity lives in the chain rather than in this type. `collapse`
 * is there for the places that have to present such a chain as a single failure, and it is
 * the only thing that produces `Multiple`.
 *
 * @see [[FailureReason]] for the ten reasons a failure can carry
 */
sealed trait Failure {

  /**
   * Returns the reason classifying this failure.
   *
   * The reason is fixed by the class of the failure rather than supplied when it is built.
   *
   * @return the reason for this failure
   */
  def reason: FailureReason

  /**
   * Returns the message describing this failure.
   *
   * The message is written to be read by a person, and names the values it is about so that
   * it remains useful on its own, away from the attributes.
   *
   * @return the message describing this failure
   */
  def message: String

  /**
   * Returns the attributes of this failure, keyed by attribute name.
   *
   * The map is sorted by key, which makes the rendering and the serialized form of a
   * failure independent of the order in which its attributes were added.
   *
   * @return the attributes of this failure
   */
  def attributes: SortedMap[String, String]

  /**
   * Returns a copy of this failure carrying the specified message and attributes.
   *
   * This is the one point at which a failure is rebuilt. Every member implements it as a
   * copy of itself, so a rebuilt failure always has the class - and therefore the reason -
   * of the failure it came from. The three combinators below are written once in terms of
   * it rather than once per member, which is what makes it impossible to add a member that
   * they do not apply to.
   *
   * @param newMessage  the message the copy carries
   * @param newAttributes  the attributes the copy carries
   * @return a copy of this failure, of the same class as this one
   */
  protected def rebuild(newMessage: String, newAttributes: SortedMap[String, String]): Failure

  /**
   * Returns a copy of this failure with an additional attribute.
   *
   * The class, the reason and the message of the failure are preserved. A value already
   * held under the same key is replaced, so the last value given for a key is the one that
   * survives:
   *
   * {{{
   * Failure.MissingData("No calendar")
   *   .withAttribute("id", "GBLO")
   *   .withAttribute("id", "USNY")   // attributes are ("id" -> "USNY")
   * }}}
   *
   * @param key  the attribute name
   * @param value  the attribute value
   * @return a copy of this failure carrying the additional attribute
   */
  final def withAttribute(key: String, value: String): Failure =
    rebuild(message, attributes.updated(key, value))

  /**
   * Returns a copy of this failure with the specified attributes added.
   *
   * The class, the reason and the message of the failure are preserved. The attributes are
   * merged into those already held rather than replacing them, and where a key appears on
   * both sides the value supplied here wins. Adding an empty map therefore changes nothing.
   *
   * @param newAttributes  the attributes to add
   * @return a copy of this failure carrying the merged attributes
   */
  final def withAttributes(newAttributes: Map[String, String]): Failure = {
    val merged = newAttributes.foldLeft(attributes) { case (acc, (key, value)) =>
      acc.updated(key, value)
    }
    rebuild(message, merged)
  }

  /**
   * Returns a copy of this failure with its message transformed.
   *
   * The class, the reason and the attributes of the failure are preserved. This is how a
   * caller supplies the context that the code reporting the failure did not have, usually
   * by wrapping the message it was given:
   *
   * {{{
   * failure.mapMessage(message => s"Unable to resolve the schedule: $message")
   * }}}
   *
   * @param f  the transformation to apply to the message
   * @return a copy of this failure carrying the transformed message
   */
  final def mapMessage(f: String => String): Failure = rebuild(f(message), attributes)

  /**
   * Returns the text form of this failure, which is the rendering [[Failure.show]] defines.
   *
   * A failure is written out in one form and one only, whether that happens through the
   * instance, through interpolation, or through a library that calls this method: the reason,
   * the message, and the attributes in key order, each part bounded in length and carrying no
   * character that could forge a line. Delegating here rather than leaving each member the
   * text form its class would generate is what makes that true of every path - a failure
   * cannot be written out in a form that has not been neutralised, which is the property the
   * rendering exists for.
   *
   * The fields remain reachable as themselves: `reason`, `message` and `attributes` answer
   * with what the failure was built with, and the JSON form encodes that. This method is for
   * reading.
   *
   * @return the rendering of this failure
   */
  final override def toString: String = Failure.show.show(this)
}

/**
 * Provides the ten kinds of failure, one per failure reason, together with the two ways of
 * obtaining one from data rather than by naming a member, and the instances for the type.
 *
 * The members are declared in the order of the reasons in [[FailureReason]], so the two
 * files read against one another. `of` maps a reason that is only known at run time onto
 * its member, and `collapse` reduces a chain of failures to one.
 */
object Failure {

  /** The empty attribute map, the value every member defaults its attributes to. */
  private val NoAttributes: SortedMap[String, String] = SortedMap.empty[String, String]

  /**
   * Several failures occurred that did not agree on a reason.
   *
   * This member describes a group of failures rather than a single thing that went wrong,
   * and `collapse` is what produces it: a failure reported on its own always carries the
   * reason of that failure, and several failures that agree on a reason keep it.
   *
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure
   */
  final case class Multiple(message: String, attributes: SortedMap[String, String] = NoAttributes)
      extends Failure {
    override def reason: FailureReason = FailureReason.MULTIPLE

    override protected def rebuild(newMessage: String, newAttributes: SortedMap[String, String]): Failure =
      copy(newMessage, newAttributes)
  }

  /**
   * An error occurred.
   *
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure
   */
  final case class Error(message: String, attributes: SortedMap[String, String] = NoAttributes)
      extends Failure {
    override def reason: FailureReason = FailureReason.ERROR

    override protected def rebuild(newMessage: String, newAttributes: SortedMap[String, String]): Failure =
      copy(newMessage, newAttributes)
  }

  /**
   * The input was invalid.
   *
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure
   */
  final case class Invalid(message: String, attributes: SortedMap[String, String] = NoAttributes)
      extends Failure {
    override def reason: FailureReason = FailureReason.INVALID

    override protected def rebuild(newMessage: String, newAttributes: SortedMap[String, String]): Failure =
      copy(newMessage, newAttributes)
  }

  /**
   * Text could not be parsed as the value it was expected to name.
   *
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure
   */
  final case class Parsing(message: String, attributes: SortedMap[String, String] = NoAttributes)
      extends Failure {
    override def reason: FailureReason = FailureReason.PARSING

    override protected def rebuild(newMessage: String, newAttributes: SortedMap[String, String]): Failure =
      copy(newMessage, newAttributes)
  }

  /**
   * The operation was not applicable to this combination of inputs.
   *
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure
   */
  final case class NotApplicable(message: String, attributes: SortedMap[String, String] = NoAttributes)
      extends Failure {
    override def reason: FailureReason = FailureReason.NOT_APPLICABLE

    override protected def rebuild(newMessage: String, newAttributes: SortedMap[String, String]): Failure =
      copy(newMessage, newAttributes)
  }

  /**
   * The operation requested is not supported.
   *
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure
   */
  final case class Unsupported(message: String, attributes: SortedMap[String, String] = NoAttributes)
      extends Failure {
    override def reason: FailureReason = FailureReason.UNSUPPORTED

    override protected def rebuild(newMessage: String, newAttributes: SortedMap[String, String]): Failure =
      copy(newMessage, newAttributes)
  }

  /**
   * Data the operation required was missing.
   *
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure
   */
  final case class MissingData(message: String, attributes: SortedMap[String, String] = NoAttributes)
      extends Failure {
    override def reason: FailureReason = FailureReason.MISSING_DATA

    override protected def rebuild(newMessage: String, newAttributes: SortedMap[String, String]): Failure =
      copy(newMessage, newAttributes)
  }

  /**
   * A conversion between currencies failed.
   *
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure
   */
  final case class CurrencyConversion(message: String, attributes: SortedMap[String, String] = NoAttributes)
      extends Failure {
    override def reason: FailureReason = FailureReason.CURRENCY_CONVERSION

    override protected def rebuild(newMessage: String, newAttributes: SortedMap[String, String]): Failure =
      copy(newMessage, newAttributes)
  }

  /**
   * A calculation could not be performed.
   *
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure
   */
  final case class CalculationFailed(message: String, attributes: SortedMap[String, String] = NoAttributes)
      extends Failure {
    override def reason: FailureReason = FailureReason.CALCULATION_FAILED

    override protected def rebuild(newMessage: String, newAttributes: SortedMap[String, String]): Failure =
      copy(newMessage, newAttributes)
  }

  /**
   * The failure occurred for some other reason.
   *
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure
   */
  final case class Other(message: String, attributes: SortedMap[String, String] = NoAttributes)
      extends Failure {
    override def reason: FailureReason = FailureReason.OTHER

    override protected def rebuild(newMessage: String, newAttributes: SortedMap[String, String]): Failure =
      copy(newMessage, newAttributes)
  }

  /**
   * Obtains a failure from a reason, a message and attributes.
   *
   * This is the route to take when the reason is a value in hand rather than a choice made
   * while writing the code, which is the position generic code is in - a validating helper,
   * a name lookup or a decoder receives the reason and cannot name a member. Where the
   * reason is known statically, naming the member of this companion directly is clearer.
   *
   * The member returned is the one whose `reason` is the reason supplied, so
   * `of(reason, message).reason == reason` holds for each of the ten reasons, and the match
   * below is exhaustive over the closed family of reasons rather than falling back on a
   * default member.
   *
   * @param reason  the reason classifying the failure
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure, none by default
   * @return the failure with that reason, message and attributes
   */
  def of(
      reason: FailureReason,
      message: String,
      attributes: SortedMap[String, String] = NoAttributes): Failure =
    reason match {
      case FailureReason.MULTIPLE => Multiple(message, attributes)
      case FailureReason.ERROR => Error(message, attributes)
      case FailureReason.INVALID => Invalid(message, attributes)
      case FailureReason.PARSING => Parsing(message, attributes)
      case FailureReason.NOT_APPLICABLE => NotApplicable(message, attributes)
      case FailureReason.UNSUPPORTED => Unsupported(message, attributes)
      case FailureReason.MISSING_DATA => MissingData(message, attributes)
      case FailureReason.CURRENCY_CONVERSION => CurrencyConversion(message, attributes)
      case FailureReason.CALCULATION_FAILED => CalculationFailed(message, attributes)
      case FailureReason.OTHER => Other(message, attributes)
    }

  /**
   * Combines several failures into the single failure that describes them together.
   *
   * Multiplicity is not a property of a failure in this port: where an operation can report
   * more than one, it reports a chain of them and each keeps its own reason, message and
   * attributes. This method is for the narrower case of having to present such a chain as
   * one failure - a single error channel, a rendered line, a decoder's complaint - and it
   * summarises the chain as follows.
   *
   *  - Failures that are equal to one another are folded together first, so a cause
   *    reported twice is described once. De-duplication is observable only when identical
   *    failures are combined; failures that differ in any part are all kept.
   *  - The messages of the remaining failures are joined with `", "`, in the order the
   *    chain holds them.
   *  - The reason is the common reason when every remaining failure agrees on one, and
   *    `MULTIPLE` when they do not. This is the only place `Multiple` is produced from
   *    other failures, and it is why a `Multiple` never appears where a single failure was
   *    reported.
   *  - The attributes are merged left to right, so where two failures use the same key the
   *    value of the later one survives, which is the rule `withAttributes` follows.
   *
   * @param failures  the failures to combine, at least one
   * @return the single failure describing all of them
   */
  def collapse(failures: NonEmptyChain[Failure]): Failure = {
    val all: NonEmptyList[Failure] = failures.toNonEmptyList
    // `distinct` on the tail plus the removal of anything equal to the head is the
    // first-occurrence-wins de-duplication of the whole chain, kept non-empty throughout so
    // that the reduction below needs no case for an empty input.
    val unique: NonEmptyList[Failure] =
      NonEmptyList(all.head, all.tail.distinct.filterNot(_ == all.head))
    val message = unique.toList.iterator.map(_.message).mkString(", ")
    val reason = unique
      .map(_.reason)
      .reduceLeft((left, right) => if (left == right) left else FailureReason.MULTIPLE)
    val attributes = unique.foldLeft(NoAttributes) { (merged, failure) =>
      failure.attributes.foldLeft(merged) { case (acc, (key, value)) => acc.updated(key, value) }
    }
    of(reason, message, attributes)
  }

  //-------------------------------------------------------------------------
  /**
   * The greatest number of characters of one part of a failure - its message, or the key or
   * the value of one of its attributes - that the rendering below writes, before the marker
   * that stands for what was left out.
   *
   * The bound belongs to the rendering and not to the failure: a failure carries the whole of
   * the text it was built with, and this is the size at which the text form of that failure
   * stops writing one part of it. It is set well above every message this library writes for
   * a definition of a realistic size - the longest of them, a schedule definition attached to
   * a rejected schedule, is a few hundred characters - and far below the size of the text a
   * caller can hand to a parse, so a rendered failure stays a line that can be read while no
   * value supplied from outside can come to dominate it.
   */
  private val MaxRenderedPart: Int = 512

  /**
   * Renders one part of a failure - its message, or an attribute key or value - for a reader,
   * bounded in length and free of anything that could forge a line.
   *
   * The places a failure is read - a log, a report, a line of a console - are line-oriented
   * and of finite size, and a failure quotes the values it is about, some of which reached
   * the library from outside it. Neutralising such text is therefore the act of writing it
   * out rather than the act of reporting the failure, and this is where the library performs
   * it: [[Failure.show]] and the text form of every member render each part through this
   * method. Two properties hold of what it returns, whatever it was given:
   *
   *  - '''The rendering is bounded.''' Units of the part are taken while the rendered text
   *    stays within [[MaxRenderedPart]] characters, and the three characters `...` are
   *    appended when any of it is left over, so the result is at most `MaxRenderedPart + 3`
   *    characters long. A rendered failure can consequently not be made large by handing a
   *    large value to the operation that reported it - a ten-thousand character input renders
   *    to five hundred and fifteen characters, not to ten thousand.
   *  - '''The rendering is a single line.''' A line feed renders as the two characters `\n`,
   *    a carriage return as `\r` and a tab as `\t`; every other character for which
   *    `Character.isISOControl` holds, together with U+2028 LINE SEPARATOR and U+2029
   *    PARAGRAPH SEPARATOR, renders as a six-character `\uXXXX` escape with lower-case
   *    hexadecimal digits. Text that arrived from outside can therefore not introduce a line
   *    of its own into a log holding the failure, which is the one way a failure could
   *    otherwise be used to state something the library did not report (CWE-117).
   *
   * Every other character renders as itself, so ordinary text, punctuation, accented letters
   * and CJK are untouched. A high surrogate followed by a low surrogate is taken as one unit
   * and rendered as it stands, so an emoji survives whole and truncation never splits a pair;
   * a surrogate standing on its own is not a character and renders as an escape. A part that
   * is within the bound and holds none of the escaped characters therefore renders to itself,
   * exactly - which is why the rendering of every ordinary failure reads as the message its
   * reporter wrote, character for character.
   *
   * The method is private because rendering is what this type does with a failure at the
   * point of writing one out, not something a caller performs on the values it reports: a
   * message is built with the value it rejected interpolated as it stands, exactly as in the
   * library being ported, and [[Failure.message]] and [[Failure.attributes]] hand that value
   * back whole to code that means to act on it rather than read it.
   *
   * @param text  the part to render, as the failure carries it
   * @return the bounded, single-line rendering of that part
   */
  private def renderPart(text: String): String = {
    // The rendering is assembled a unit at a time - a surrogate pair counting as one - and
    // stops as soon as the next unit would carry it past the bound, which is what keeps a
    // pair whole and an escape entire. Threading the text rendered so far through a
    // tail-recursive step rather than accumulating into a mutable local keeps the method
    // free of assignment; both the intermediate and the final strings are bounded by
    // `MaxRenderedPart`, so the concatenation costs no more than assembling the result in
    // one pass would.
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
        if (rendered.length + unit.length > MaxRenderedPart) {
          rendered + "..."
        } else {
          rendering(index + (if (pairsWithNext) 2 else 1), rendered + unit)
        }
      }

    rendering(0, "")
  }

  // Renders one character: the three control characters that have a short escape keep it,
  // because a reader recognises them; anything else that must not be written out as itself
  // becomes a fixed-width escape; and every other character stands as it is.
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

  // The characters that have no short escape and cannot be rendered as themselves: every ISO
  // control character other than the three above, the two Unicode separators that a reader
  // may treat as ending a line, and a surrogate standing on its own, which is half of a
  // character and turns into a replacement glyph wherever the message is written.
  private def escapesAsUnicode(ch: Char): Boolean =
    Character.isISOControl(ch) || ch == '\u2028' || ch == '\u2029' || Character.isSurrogate(ch)

  // The six-character escape of a character. The digits are the lower-case hexadecimal ones
  // `Integer.toHexString` produces, padded to four so that the width of an escape is fixed
  // and the bound above can be reasoned about without knowing which character was escaped.
  private def unicodeEscape(ch: Char): String = {
    val digits = Integer.toHexString(ch.toInt)
    s"\\u${"0" * (4 - digits.length)}$digits"
  }

  /**
   * The hashing and equality of failures.
   *
   * This is the only equality-bearing instance of the type, and it is the equality of the
   * values themselves: two failures are equal when they are of the same class and carry the
   * same message and attributes. Structural equality is the right notion here because no
   * part of a failure is a floating-point number or an array, so there is no field whose
   * own equality needs special care. An `Eq` is available by subtyping, and there is
   * deliberately no `Order`: the ten members form a set of kinds, not a scale, and nothing
   * in the port sorts failures.
   *
   * @return the hashing of failures
   */
  implicit val hash: Hash[Failure] = Hash.fromUniversalHashCode[Failure]

  /**
   * The rendering of failures as text.
   *
   * A failure renders as its reason, then its message - the form the type being ported
   * rendered, less the trace that this port does not hold - followed by its attributes when
   * it has any, so that a log line holding failures of several kinds stays readable:
   *
   * {{{
   * MISSING_DATA: No holiday calendar
   * INVALID: Schedule is invalid [definition=P3M from 2024-01-15]
   * }}}
   *
   * The rendering is a function of the value alone, and the attributes are held in key
   * order, so the same failure always renders the same way.
   *
   * ===Every part is bounded and on one line===
   *
   * This is the boundary at which a failure becomes text, and it is where the library
   * neutralises what it is about to write: the message and the key and the value of every
   * attribute each go through [[renderPart]], so each is at most `MaxRenderedPart + 3`
   * characters long and holds no character a line-oriented reader could act on. A failure
   * quoting a currency code, an identifier, a name or a definition that a caller supplied -
   * text nothing bounds and nothing constrains - therefore cannot forge a line of a log that
   * holds it (CWE-117) or make that line as large as the value it quotes, however the failure
   * itself was built. A message and an attribute of a realistic size render to themselves,
   * character for character, so this costs the reading of an ordinary failure nothing.
   *
   * The failure keeps what it was built with: [[Failure.message]] and
   * [[Failure.attributes]] answer with the whole of the text, which is what code acting on a
   * failure - matching on it, testing it, re-reporting it - reads, and what the JSON encoding
   * writes, that form escaping a control character as the JSON grammar requires. The text
   * form of every member is this rendering, so writing a failure out cannot bypass it.
   *
   * @return the rendering of a failure
   */
  implicit val show: Show[Failure] = Show.show { failure =>
    val rendered = s"${failure.reason.name}: ${renderPart(failure.message)}"
    if (failure.attributes.isEmpty) {
      rendered
    } else {
      val attributes = failure.attributes.iterator.map { case (key, value) =>
        s"${renderPart(key)}=${renderPart(value)}"
      }
      s"$rendered [${attributes.mkString(", ")}]"
    }
  }

  /**
   * The JSON encoding of failures.
   *
   * The encoding is derived when this file is compiled, so no part of it inspects a class
   * while the program runs. A failure encodes as the single-key object that names its
   * member, holding the two fields of that member:
   *
   * {{{
   * {"MissingData":{"message":"No holiday calendar","attributes":{}}}
   * {"Invalid":{"message":"Schedule is invalid","attributes":{"definition":"P3M"}}}
   * }}}
   *
   * The reason is not written, because the member name already determines it; writing both
   * would allow an encoded failure to disagree with itself. The attribute object is written
   * in key order, which is what makes the encoding of a failure depend on its value alone
   * and not on the order its attributes were added in. No field of any member is optional,
   * so there is no absent value to drop from the output and the encoding needs no
   * post-processing.
   *
   * @return the JSON encoding of a failure
   */
  implicit val encoder: Encoder.AsObject[Failure] = deriveEncoder[Failure]

  /**
   * The JSON decoding of failures.
   *
   * This is the inverse of the encoding above and is likewise derived at compile time.
   * Decoding accepts the single-key object that names a member and rejects anything else,
   * so the ten members are the only failures that can be decoded - the closed set of the
   * type is enforced on the way in as well as on the way out.
   *
   * A derived decoding reads the fields a member declares and does not consult the default
   * value declared for `attributes`, so the `attributes` field has to be present. The
   * encoding always writes it, as an empty object when there are no attributes, so every
   * encoded failure decodes back to an equal failure.
   *
   * @return the JSON decoding of a failure
   */
  implicit val decoder: Decoder[Failure] = deriveDecoder[Failure]
}
