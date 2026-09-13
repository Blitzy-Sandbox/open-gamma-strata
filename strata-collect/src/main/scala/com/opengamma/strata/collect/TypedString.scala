/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import scala.util.matching.Regex

import cats.Hash
import cats.Order
import cats.Show

import _root_.io.circe.Codec
import _root_.io.circe.Decoder
import _root_.io.circe.Encoder

import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.ResultNec
import com.opengamma.strata.collect.result.ValidatedFailures

/**
 * The companion of a ''typed string'': a concept that would otherwise be carried around as
 * text, given a type of its own.
 *
 * A great many values in a financial library are, in the end, a piece of text - the symbol of
 * an instrument, the scheme of an identifier, the label of a curve. Passing them as `String`
 * costs nothing to write and everything to read: any two such values are interchangeable in a
 * signature, so an argument list of three of them is three chances to transpose two of them,
 * and the compiler has nothing to say about it. A typed string removes that: the text is
 * wrapped in a type, an API takes and returns that type, and a value of the wrong kind of text
 * stops being something a caller can accidentally supply.
 *
 * This class is the reusable half of that arrangement. It carries everything a typed string
 * needs beyond its own single field: the validating factory that is the one way to build one,
 * the ordering, hashing and rendering of its values, its JSON representation, and the extractor
 * that takes a value apart again. A concrete typed string therefore consists of a one-line
 * class and a companion that extends this one.
 *
 * ===The pattern to write===
 *
 * This is the whole of a typed string, and it is intended to be copied as it stands:
 *
 * {{{
 * final class TickerSymbol private (val name: String) extends AnyVal with Named
 *
 * object TickerSymbol
 *     extends TypedStringCompanion[TickerSymbol](
 *       TypedStringCompanion.matchingPattern(
 *         "[A-Z]{1,5}".r,
 *         "A ticker symbol must be one to five upper case letters"),
 *       new TickerSymbol(_))
 * }}}
 *
 * Every part of those five lines is load-bearing:
 *
 *   - `final class` rather than `case class`: a case class would come with a synthesised
 *     `apply` on its companion and a `copy` on its values, and each of them would be a way to
 *     build a value without validating it. Neither exists here, so `of` is not merely the
 *     recommended route to a value - it is the only one, and that is a property the compiler
 *     enforces rather than a convention a reviewer has to police.
 *   - `private` on the constructor: the class itself cannot be instantiated from outside, while
 *     the companion - which is where `make` is supplied - retains access. That is why the
 *     `new TickerSymbol(_)` above is legal exactly where it appears and nowhere else.
 *   - `extends AnyVal`: the wrapper is erased wherever the compiler can manage it, so a typed
 *     string costs no allocation in a local computation and is a plain text field once stored.
 *     The type is real to the reader and to the compiler, and largely absent at run time.
 *   - `with Named`: `Named` is a ''universal trait'' (it extends `Any`), and only a universal
 *     trait may be mixed into a value class. The single `val name` satisfies its one member, so
 *     a typed string is a named value like any other, usable wherever this library asks for
 *     one.
 *   - the type argument to this class is the typed string itself, which is what makes the
 *     inherited factory, instances and extractor speak in terms of `TickerSymbol` rather than
 *     of some shared supertype.
 *
 * One ordering constraint comes with extending a class rather than mixing in a trait: the two
 * arguments are evaluated before the extending object's own body is initialised. Build the
 * validation inline, as above, or take it from somewhere already initialised; a value declared
 * in the body of the extending object is not yet available at the point the arguments are
 * evaluated, and a factory built over it would go on to fail when first used.
 *
 * ===Building a value===
 *
 * `of` is the sole construction path, and it hands back an outcome rather than a value, so text
 * that the type does not accept is reported instead of becoming an illegal value:
 *
 * {{{
 * TickerSymbol.of("OG")      // Right(OG)
 * TickerSymbol.of("og")      // Left(chain of one Failure.Invalid)
 * TickerSymbol.of("")        // Left(chain of one Failure.Invalid, the text being empty)
 * }}}
 *
 * A caller that has a value and wants its text back reads `name`, or destructures it with the
 * inherited extractor, which composes with the outcome of `of`:
 *
 * {{{
 * TickerSymbol.of(input) match {
 *   case Right(TickerSymbol(text)) => text
 *   case Left(failures)            => failures.head.message
 * }
 * }}}
 *
 * ===Why the machinery sits in the companion===
 *
 * The library being ported expressed a typed string by inheritance: an abstract base class held
 * the text, validated it in its constructors, and implemented comparison, equality, hashing and
 * rendering once for every subclass. That arrangement is unavailable here, because a value class
 * may extend nothing but a universal trait, and giving up the value class would mean an
 * allocation for every wrapper - the cost the abstraction exists to avoid. So the design is
 * inverted: the values carry only their text, and everything that was inherited by the subclass
 * is inherited by its companion instead. The result is the same amount of code at the concrete
 * type, and a strictly stronger guarantee, since the base class's constructors were reachable by
 * any subclass whereas `of` here is the only door.
 *
 * ===Divergences from the type being ported===
 *
 * These belong in the migration note, because each is visible to a caller:
 *
 *   - '''Equality and hashing come from the text alone.''' The original mixed the run-time class
 *     of the value into both, so two different typed-string types wrapping the same text were
 *     unequal. A value class takes its equality and its hash from the value it wraps, so they
 *     are now equal if they are ever compared. Nothing is actually weakened by that: the two
 *     types are distinguished statically, which is the entire purpose of the abstraction, and a
 *     comparison between them is a type error long before it is an equality question. The
 *     instances published here are equally text-based, and they agree with each other by
 *     construction.
 *   - '''The cached hash code is gone.''' The original cached its hash in a field, using the
 *     racy single-check idiom. A value class has no field to cache in, and the hash of the text
 *     it wraps is itself cached by the platform, so the caching served no purpose here.
 *   - '''Comparison is direct.''' The original compared its text against the rendered form of
 *     the other value; ordering here is a plain lexicographic comparison of the two names, which
 *     gives the same result without the indirection.
 *   - '''Three constructors became one validation.''' The original offered a plain form, a regex
 *     form and a character-predicate form, each rejecting text by interrupting its caller.
 *     This class takes a single validation returning an accumulating outcome, and the nested
 *     object below supplies the three forms as ready-made validations. A shape that neither
 *     covers is any other function of the same type.
 *   - '''Platform serialization and the reflective text-form annotation are not carried.''' A
 *     typed string is text on the wire through the JSON codec published here, which the whole of
 *     this port uses.
 *
 * @tparam T  the typed string this companion builds, which is also the type extending it
 * @param validate  the check every candidate text has to pass, reporting every reason it does
 *   not; the nested object supplies the three forms the ported type offered
 * @param make  wraps accepted text as a value, ordinarily the private constructor of the type
 * @see [[Named]] for the universal trait a typed string mixes in
 * @see [[Validate]] for the checks a validation is assembled from
 */
abstract class TypedStringCompanion[T <: Named](
    validate: String => ValidatedFailures[String],
    make: String => T) {

  /**
   * Obtains an instance from the specified text.
   *
   * The text is checked first, and a value exists only if it passed, so every value of the type
   * is valid by construction and no other code has to re-check one. The outcome carries a chain
   * of failures rather than a single one because a validation is free to combine several
   * independent checks, in which case text that is wrong in more than one way is reported in one
   * outcome and a caller corrects its input in one pass. The three validations supplied by the
   * nested object are not of that kind - each checks emptiness and then, only if that passed,
   * the shape - so they report one reason at a time.
   *
   * This is the only construction path a typed string has: the class synthesises no factory of
   * its own and its constructor is reachable only from its companion.
   *
   * @param name  the text to build a value from
   * @return the value, or every reason the text was not accepted
   */
  def of(name: String): ResultNec[T] = validate(name).toEither.map(make)

  /**
   * Extracts the text of a value, so that a typed string destructures in a pattern.
   *
   * The extraction cannot fail - a value that exists holds exactly one piece of accepted text -
   * which is why the result is `Some` rather than `Option`: the compiler then knows the pattern
   * is irrefutable and no unreachable fall-through has to be written for it.
   *
   * Construction from the string side is `of`, whose outcome a caller matches as `Right` or
   * `Left`; the two compose, as in `case Right(TickerSymbol(text))`.
   *
   * @param value  the value to take apart
   * @return the text of the value
   */
  def unapply(value: T): Some[String] = Some(value.name)

  /**
   * The ordering of values, which is also their hashing.
   *
   * This is the only equality-bearing instance of a typed string: `Order` and `Hash` both extend
   * `Eq`, so publishing one instance that is both is what makes it impossible for equality,
   * ordering and hashing to disagree. All three read the name and nothing else - values are
   * ordered lexicographically by it, equal when it is equal, and hashed by its hash - so
   * `compare` returns zero exactly when `eqv` holds, and equal values hash alike.
   *
   * @return the ordering and hashing of values of the type
   */
  implicit val order: Order[T] with Hash[T] =
    new Order[T] with Hash[T] {

      override def compare(x: T, y: T): Int = x.name.compareTo(y.name)

      override def eqv(x: T, y: T): Boolean = x.name == y.name

      override def hash(x: T): Int = x.name.hashCode
    }

  /**
   * The rendering of values as text, for a reader.
   *
   * A typed string renders as its name, with no decoration of any kind and nothing added to or
   * taken from it: that is the form the ported type rendered, the form [[of]] accepts back, and
   * the form the JSON codec below writes, so the three text forms of a value are one text and a
   * value survives being written out and read back whatever text it carries.
   *
   * The text of a typed string is a caller's, checked only for the shape the concrete type's own
   * validation states - the plain form asks merely that the text is present, and a shape check is
   * free to accept a control character or text of any length - and it is written out as it
   * stands. Neutralising such text, so that it can neither add a line to a log (CWE-117) nor
   * fill one (CWE-400), belongs to the places that write a diagnostic:
   * [[com.opengamma.strata.collect.result.Failure.show]] and therefore the text form of every
   * failure, and the decoder bridge in [[com.opengamma.strata.collect.json.Codecs]], each of
   * which bounds and escapes what it is about to write. It does not belong to a value's own
   * rendering, which is the identity of the value - a caller comparing, re-parsing or
   * re-serializing what it rendered has to receive the whole of its own text back.
   *
   * @return the rendering of a value of the type
   */
  implicit val show: Show[T] = Show.show(value => value.name)

  /**
   * The single codec behind the two instances published below.
   *
   * Taking it from the shared helper rather than writing it here is what keeps the failure a
   * rejected document reports identical to the one every other text-valued type of this port
   * reports: the messages of the accumulated failures, in order, at the position in the document
   * where the text was found.
   */
  private val codec: Codec[T] =
    Codecs.parsedStringCodecNec[T](text => of(text), value => value.name)

  /**
   * The JSON encoder for values.
   *
   * A typed string is a JSON string holding its name, so a document written by this port carries
   * the same text as one written by the library being ported.
   *
   * @return the encoder writing a value as its name
   */
  implicit val encoder: Encoder[T] = codec

  /**
   * The JSON decoder for values.
   *
   * The string is read through `of`, so a document holding text the type does not accept is
   * rejected as a decoding failure carrying the validation messages, and decoding is therefore
   * one more route that cannot produce an invalid value.
   *
   * @return the decoder reading a value from its name
   */
  implicit val decoder: Decoder[T] = codec
}

/**
 * The validations a typed string is built from.
 *
 * Each member here returns a check of the shape [[TypedStringCompanion]] takes as its first
 * argument, and between them they cover the three forms of the type being ported: text that need
 * only be present, text whose whole shape is described by a regular expression, and text built
 * only from characters a predicate accepts. A requirement none of them expresses is any other
 * function from text to an accumulating outcome, assembled from [[Validate]] in the same way
 * these are.
 *
 * Emptiness is checked first in every case, and the shape check runs only if that passed. That
 * is the order the ported constructors used, and for the character-predicate form it is also the
 * only correct order: every character of empty text satisfies any predicate, so empty text would
 * otherwise be accepted by a check meant to constrain it.
 */
object TypedStringCompanion {

  /** The argument name the failures below report the rejected text under. */
  private val NameArgument: String = "name"

  /**
   * A validation that accepts any text holding at least one character.
   *
   * This is the plain form of the ported type, whose only requirement was that the text was not
   * empty, and it reports emptiness in the same terms as the rest of this library.
   *
   * {{{
   * object Label extends TypedStringCompanion[Label](TypedStringCompanion.nonEmpty, new Label(_))
   * }}}
   *
   * @return the validation accepting any text that is not empty
   */
  val nonEmpty: String => ValidatedFailures[String] =
    text => Validate.notEmpty(text, NameArgument)

  /**
   * A validation that accepts text matching the specified pattern in full.
   *
   * The pattern has to match the whole text and not merely some part of it, so a pattern
   * describing an identifier needs no anchors written into it. Text that is empty, or that the
   * pattern does not match, is rejected with the supplied explanation, which is built only on
   * the failing path.
   *
   * {{{
   * TypedStringCompanion.matchingPattern(
   *   "[A-Z]{1,5}".r,
   *   "A ticker symbol must be one to five upper case letters")
   * }}}
   *
   * @param pattern  the pattern the whole text has to match
   * @param message  explains the requirement, evaluated only if the text is rejected
   * @return the validation accepting text that matches the pattern
   */
  def matchingPattern(pattern: Regex, message: => String): String => ValidatedFailures[String] =
    text =>
      nonEmpty(text).andThen(checked =>
        Validate.cond(pattern.matches(checked), checked, Failure.Invalid(message)))

  /**
   * A validation that accepts text built only from characters the specified predicate accepts.
   *
   * This is the faster of the two shape checks, since it inspects characters rather than running
   * a regular expression, and it is worth preferring wherever the requirement is expressible one
   * character at a time. Text that is empty, or that holds any character the predicate rejects,
   * is rejected with the supplied explanation, which is built only on the failing path.
   *
   * {{{
   * TypedStringCompanion.matchingCharacters(
   *   character => character >= 'A' && character <= 'Z',
   *   "A region code must be upper case letters")
   * }}}
   *
   * @param accepts  the predicate every character of the text has to satisfy
   * @param message  explains the requirement, evaluated only if the text is rejected
   * @return the validation accepting text built only from accepted characters
   */
  def matchingCharacters(
      accepts: Char => Boolean,
      message: => String): String => ValidatedFailures[String] =

    text =>
      nonEmpty(text).andThen(checked =>
        Validate.cond(checked.forall(accepts), checked, Failure.Invalid(message)))
}
