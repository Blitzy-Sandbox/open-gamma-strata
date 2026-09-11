/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.location

import java.util.Locale

import scala.collection.immutable.SortedSet

import cats.Hash
import cats.Order
import cats.Show

import io.circe.Codec

import com.opengamma.strata.collect.Validate
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure
import com.opengamma.strata.collect.result.ResultNec

/**
 * A country or territory.
 *
 * This type identifies a country or territory by its two letter code. Any two letter code
 * built from the upper case letters `A` to `Z` may be used, although the intention is to use
 * the codes of ISO-3166-1 alpha-2, and the named constants below are all drawn from that
 * standard. The three letter codes of ISO-3166-1 alpha-3 are also understood, as an
 * alternative way of naming the same country rather than as a second identity: a country
 * built from `GBR` is the same value as a country built from `GB`, and [[code3Char]]
 * recovers the three letter form of a country that has one.
 *
 * ===An open set of codes, not a closed enumeration===
 *
 * The set of countries is deliberately '''open'''. The code space is what is constrained -
 * exactly two characters, each an upper case ASCII letter - and any code satisfying that is
 * accepted, whether or not it appears in the reference data or among the constants. This
 * mirrors the type being ported, which created a country on demand for any well formed code,
 * and it is why this type is a validated value rather than one of the closed named families
 * of this library. A code that no standard assigns, such as `AA`, is therefore a country
 * here; whether it means anything is a question for the data it came from, not for this type.
 *
 * ===Construction===
 *
 * A country cannot be built directly: there is no public constructor, no `apply` and no
 * `copy`, so every value comes from one of the factories on the companion and every value in
 * existence has been through the check. Construction reports what was wrong with the input
 * rather than interrupting the caller:
 *
 * {{{
 * Country.of("GB")        // Right(GB)
 * Country.of("gb")        // Left - the code has to be upper case
 * Country.parse("gb")     // Right(GB) - parse folds case first
 * Country.of3Char("GBR")  // Right(GB)
 * }}}
 *
 * Pattern matching is available, so a country can be taken apart where that reads better
 * than calling [[code]]:
 *
 * {{{
 * country match {
 *   case Country(code) => code
 * }
 * }}}
 *
 * ===Equality, ordering and rendering===
 *
 * Two countries are equal when their codes are equal, and they order alphabetically by code.
 * Ordering therefore agrees with equality exactly - `compare` returns zero precisely when the
 * two values are equal - so no secondary comparison is needed to reconcile the two, which is
 * not true of every validated type in this library. A country renders as its bare code, both
 * through `toString` and through its `Show` instance, and its JSON form is the same bare
 * string.
 *
 * ===Deliberate differences from the type being ported===
 *
 * The behaviour of this type is that of the Java original, with the following differences,
 * each of which follows from a convention of this port rather than from a change of intent:
 *
 *   - '''Instances are not interned.''' The original kept a growing global map of every
 *     country it had been asked for and handed back the stored instance, so two calls for the
 *     same code yielded the same object. A mutable global map is hidden state, which this
 *     port does not keep, so each call builds a fresh value. Equality of values is unchanged
 *     and is the only comparison that is meaningful here; two countries with the same code
 *     are equal, have equal hash codes and are interchangeable, but they need not be the same
 *     object, so identity must not be used to compare them.
 *   - '''[[Country.availableCountries]] is fixed.''' Because there is no cache to grow, the
 *     set of available countries is the constant set of everything this type knows about -
 *     the reference data and the constants - and asking for a country outside it does not
 *     change it. The original, whose set was the contents of its cache, grew by one each time
 *     a previously unseen code was requested.
 *   - '''Failures are returned, not raised.''' Where the original raised an error for a
 *     malformed code, an unknown three letter code, or a country with no three letter form,
 *     the corresponding member here reports a [[Failure]] on the left of an `Either`. The
 *     messages are those of the original, word for word, so text that reaches a log or a test
 *     expectation is unchanged.
 *   - '''An absent input is not modelled.''' The original checked its arguments for a missing
 *     reference and raised an error when it found one. This port expresses a value that may
 *     be absent as an `Option` instead, so the members here take a plain `String` and the
 *     corresponding cases of the original have no counterpart.
 *   - '''Serialization support is the JSON codec alone.''' The Java serialization hooks and
 *     the string-conversion annotations of the original are not carried over; the [[codec]]
 *     below is the supported way to write and read a country, and it writes the same bare
 *     code the original wrote.
 *   - '''Comparison against an absent value is not modelled''', following from the same
 *     reasoning as the input case above.
 *
 * ===Thread safety===
 *
 * A country is immutable, every member is a pure function of the value and its arguments, and
 * the companion holds nothing that changes after it is initialised, so values of this type may
 * be shared freely between threads.
 *
 * @see [[CountryData]] for the reference data behind the three letter codes
 */
sealed abstract case class Country private (code: String) {

  /**
   * Returns the ISO-3166-1 alpha-3 three letter code of this country.
   *
   * Not every country has one. The three letter codes are reference data, and a country
   * whose code is well formed but absent from that data - a code outside the standard, or a
   * region such as `EU` that the standard treats specially and gives no alpha-3 code - has
   * no three letter form to return. That is a property of the data rather than a mistake by
   * the caller, so it is reported as a failure rather than raised:
   *
   * {{{
   * Country.GB.code3Char   // Right("GBR")
   * Country.EU.code3Char   // Left(Failure.MissingData("Unknown country: EU"))
   * }}}
   *
   * The failure is `MissingData` because the reference data holds no row for the code, and
   * its message is the one the type being ported used.
   *
   * @return the three letter code, or the failure describing that there is none
   */
  def code3Char: Either[Failure, String] =
    CountryData.alpha2ToAlpha3
      .get(code)
      .toRight(Failure.MissingData(s"Unknown country: $code"))

  /**
   * Returns the two letter code of this country.
   *
   * The bare code is the rendering of a country everywhere in this library - in reports, in
   * logs and in JSON - so it is what this returns, rather than the generated form naming the
   * type and its field.
   *
   * @return the two letter country code
   */
  override def toString: String = code
}

/**
 * Provides the countries this library names, the factories that build one from text, and the
 * instances for the type.
 *
 * The constants are the selection of countries the type being ported declared, in the same
 * order and under the same names, grouped by region. They are a convenience for the code that
 * refers to a country by name and carry no privilege: a constant is the same value that the
 * matching call to [[of]] produces, so `Country.of("GB")` and `Country.GB` are equal.
 */
object Country {

  /**
   * Tests whether a character is one the code space allows.
   *
   * The code space is the upper case ASCII letters, which is the character matcher of the
   * type being ported written as a predicate.
   *
   * @param character  the character to test
   * @return true if the character may appear in a country code
   */
  private def isCodeChar(character: Char): Boolean = character >= 'A' && character <= 'Z'

  /**
   * Builds a country from a code already known to be valid.
   *
   * This is the only place a country is instantiated, and it performs no check, so every
   * caller has to have established validity by other means. Two kinds of caller qualify: the
   * constants below, whose codes are literals written in this file and read by the tests
   * against [[of]]; and the factories below, which call this only on the value handed back
   * by a check that passed. It is private because neither guarantee is available to code
   * outside this object.
   *
   * @param code  the valid two letter country code
   * @return the country
   */
  private def unsafe(code: String): Country = new Country(code) {}

  //-------------------------------------------------------------------------
  // selected countries of Europe
  /** The region of 'EU' - Europe (special status in ISO-3166). */
  val EU: Country = unsafe("EU")
  /** The country 'AT' - Austria. */
  val AT: Country = unsafe("AT")
  /** The country 'BE' - Belgium. */
  val BE: Country = unsafe("BE")
  /** The country 'CH' - Switzerland. */
  val CH: Country = unsafe("CH")
  /** The country 'CZ' - Czech Republic. */
  val CZ: Country = unsafe("CZ")
  /** The country 'DE' - Germany. */
  val DE: Country = unsafe("DE")
  /** The country 'DK' - Denmark. */
  val DK: Country = unsafe("DK")
  /** The country 'ES' - Spain. */
  val ES: Country = unsafe("ES")
  /** The country 'FI' - Finland. */
  val FI: Country = unsafe("FI")
  /** The country 'FR' - France. */
  val FR: Country = unsafe("FR")
  /** The country 'GB' - United Kingdom. */
  val GB: Country = unsafe("GB")
  /** The country 'GR' - Greece. */
  val GR: Country = unsafe("GR")
  /** The country 'HU' - Hungary. */
  val HU: Country = unsafe("HU")
  /** The country 'IE' - Ireland. */
  val IE: Country = unsafe("IE")
  /** The country 'IS' - Iceland. */
  val IS: Country = unsafe("IS")
  /** The country 'IT' - Italy. */
  val IT: Country = unsafe("IT")
  /** The country 'LU' - Luxembourg. */
  val LU: Country = unsafe("LU")
  /** The country 'NL' - Netherlands. */
  val NL: Country = unsafe("NL")
  /** The country 'NO' - Norway. */
  val NO: Country = unsafe("NO")
  /** The country 'PL' - Poland. */
  val PL: Country = unsafe("PL")
  /** The country 'PT' - Portugal. */
  val PT: Country = unsafe("PT")
  /** The country 'SE' - Sweden. */
  val SE: Country = unsafe("SE")
  /** The country 'SK' - Slovakia. */
  val SK: Country = unsafe("SK")
  /** The country 'TR' - Turkey. */
  val TR: Country = unsafe("TR")

  //-------------------------------------------------------------------------
  // selected countries of the Americas
  /** The country 'AR' - Argentina. */
  val AR: Country = unsafe("AR")
  /** The country 'BR' - Brazil. */
  val BR: Country = unsafe("BR")
  /** The country 'CA' - Canada. */
  val CA: Country = unsafe("CA")
  /** The country 'CL' - Chile. */
  val CL: Country = unsafe("CL")
  /** The country 'MX' - Mexico. */
  val MX: Country = unsafe("MX")
  /** The country 'US' - United States. */
  val US: Country = unsafe("US")

  //-------------------------------------------------------------------------
  // selected countries of the Rest of the World
  /** The country 'AU' - Australia. */
  val AU: Country = unsafe("AU")
  /** The country 'CN' - China. */
  val CN: Country = unsafe("CN")
  /** The country 'EG' - Egypt. */
  val EG: Country = unsafe("EG")
  /** The country 'HK' - Hong Kong. */
  val HK: Country = unsafe("HK")
  /** The country 'ID' - Indonesia. */
  val ID: Country = unsafe("ID")
  /** The country 'IL' - Israel. */
  val IL: Country = unsafe("IL")
  /** The country 'IN' - India. */
  val IN: Country = unsafe("IN")
  /** The country 'JP' - Japan. */
  val JP: Country = unsafe("JP")
  /** The country 'KR' - South Korea. */
  val KR: Country = unsafe("KR")
  /** The country 'MY' - Malaysia. */
  val MY: Country = unsafe("MY")
  /** The country 'NZ' - New Zealand. */
  val NZ: Country = unsafe("NZ")
  /** The country 'RU' - Russia. */
  val RU: Country = unsafe("RU")
  /** The country 'SA' - Saudi Arabia. */
  val SA: Country = unsafe("SA")
  /** The country 'SG' - Singapore. */
  val SG: Country = unsafe("SG")
  /** The country 'TH' - Thailand. */
  val TH: Country = unsafe("TH")
  /** The country 'ZA' - South Africa. */
  val ZA: Country = unsafe("ZA")

  /**
   * The countries named by the constants above, in the order they are declared.
   *
   * This exists so that [[availableCountries]] can be assembled from one list rather than
   * from a second transcription of the same names, and so that adding a constant extends
   * that set automatically.
   */
  private val constants: Vector[Country] = Vector(
    EU, AT, BE, CH, CZ, DE, DK, ES, FI, FR, GB, GR, HU, IE, IS, IT, LU, NL, NO, PL, PT, SE, SK, TR,
    AR, BR, CA, CL, MX, US,
    AU, CN, EG, HK, ID, IL, IN, JP, KR, MY, NZ, RU, SA, SG, TH, ZA)

  //-------------------------------------------------------------------------
  /**
   * The ordering of countries, which is also their hashing.
   *
   * This is the only equality-bearing instance of the type: `Order` and `Hash` both extend
   * `Eq`, so declaring one instance is what keeps the three from ever disagreeing. Countries
   * order alphabetically by their two letter code, which is the comparison of the type being
   * ported, and they are equal when those codes are equal. The two agree without any further
   * comparison, because the code is the whole of the value - `compare` returns zero exactly
   * when the two countries are equal - so unlike several validated types in this library this
   * one needs no secondary comparison to reconcile its ordering with its equality.
   *
   * @return the ordering of countries
   */
  implicit val order: Order[Country] with Hash[Country] =
    new Order[Country] with Hash[Country] {

      private val universal: Hash[Country] = Hash.fromUniversalHashCode[Country]

      override def compare(x: Country, y: Country): Int = x.code.compareTo(y.code)

      override def eqv(x: Country, y: Country): Boolean = universal.eqv(x, y)

      override def hash(x: Country): Int = universal.hash(x)
    }

  /**
   * The rendering of countries as text.
   *
   * A country renders as its bare two letter code, as `toString` does.
   *
   * @return the rendering of a country
   */
  implicit val show: Show[Country] = Show.show(_.code)

  //-------------------------------------------------------------------------
  /**
   * Returns the set of countries this type knows about, ordered by code.
   *
   * The set is the union of two things: every country named by the alpha-2 side of the
   * reference data in [[CountryData]], and every country named by a constant above. The
   * constants contribute exactly one country the data does not, the `EU` region, which the
   * standard treats specially and gives no three letter code, so the set holds 252 countries.
   *
   * The set is fixed. Unlike the type being ported, whose equivalent grew as codes were
   * requested because it exposed the contents of an instance cache, this one describes what
   * the library knows rather than what it has been asked for, and building a country outside
   * it - which [[of]] permits for any well formed code - leaves it unchanged.
   *
   * It is computed on first use and then held, because building it touches the whole of the
   * reference data and most callers never ask for it.
   *
   * @return the countries this type knows about
   */
  lazy val availableCountries: SortedSet[Country] =
    SortedSet.from(CountryData.alpha2Codes.iterator.map(unsafe) ++ constants.iterator)(order.toOrdering)

  //-------------------------------------------------------------------------
  /**
   * Obtains a country from its ISO-3166-1 alpha-2 two letter code.
   *
   * The code has to be exactly two characters long and built only from the upper case ASCII
   * letters `A` to `Z`; any such code is accepted, whether or not the standard assigns it,
   * which is what makes the set of countries open. Text that is too short, too long, lower
   * case, or holds anything that is not a letter is rejected:
   *
   * {{{
   * Country.of("GB")    // Right(GB)
   * Country.of("AA")    // Right(AA) - well formed, though unassigned
   * Country.of("gb")    // Left - lower case
   * Country.of("ABC")   // Left - too long
   * Country.of(" GB")   // Left - too long, and a character outside the code space
   * }}}
   *
   * The three ways of being malformed share one message, as they do in the type being
   * ported, because what the caller has to correct is the same in each case: the text does
   * not have the shape of a country code. The result is the accumulating form so that this
   * factory composes with the checks of a larger value being built around it.
   *
   * @param countryCode  the two letter country code, upper case ASCII
   * @return the country, or the failure describing why the code was rejected
   */
  def of(countryCode: String): ResultNec[Country] =
    Validate.toResult(
      Validate
        .matches(isCodeChar, 2, 2, countryCode, "countryCode", "[A-Z][A-Z]")
        .map(unsafe))

  /**
   * Parses text into a country, folding case first.
   *
   * This is [[of]] applied to the upper case of the text, so it accepts everything `of`
   * accepts and additionally accepts any mixture of case. Nothing else differs: text whose
   * upper case is not a well formed code is rejected with the message `of` would give, which
   * quotes the code as it was supplied rather than as it was folded.
   *
   * {{{
   * Country.parse("gb")   // Right(GB)
   * Country.parse("zy")   // Right(ZY)
   * Country.parse("abc")  // Left - too long, whatever its case
   * }}}
   *
   * Case is folded for the English locale specifically, and not for the default locale of
   * the running program, so that the result of parsing a code does not depend on where the
   * program happens to be running - in some locales the upper case of a Latin letter is not
   * the letter this code space expects.
   *
   * @param countryCode  the two letter country code, in any case
   * @return the country, or the failure describing why the code was rejected
   */
  def parse(countryCode: String): ResultNec[Country] = of(countryCode.toUpperCase(Locale.ENGLISH))

  //-------------------------------------------------------------------------
  /**
   * Obtains a country from its ISO-3166-1 alpha-3 three letter code.
   *
   * Unlike [[of]], this does not accept every well formed code, because a three letter code
   * is not an identity of its own: it has to be translated into the two letter code that is,
   * and only the codes held in [[CountryData]] can be translated. The two ways this can fail
   * are therefore distinct, and each reports the failure the type being ported reported:
   *
   * {{{
   * Country.of3Char("GBR")   // Right(GB)
   * Country.of3Char("zzz")   // Left(Failure.Invalid) - not the shape of a three letter code
   * Country.of3Char("ZZZ")   // Left(Failure.Parsing) - well formed, but names no country
   * }}}
   *
   * Because the causes are alternatives rather than things that can both be true of one
   * input - text that is not well formed is never looked up - the result reports a single
   * failure rather than the accumulating form used by `of`.
   *
   * @param countryCode  the three letter country code, upper case ASCII
   * @return the country, or the failure describing why the code was rejected
   */
  def of3Char(countryCode: String): Either[Failure, Country] =
    Validate
      .matches(isCodeChar, 3, 3, countryCode, "countryCode", "[A-Z][A-Z][A-Z]")
      .toEither
      .left
      .map(Failure.collapse)
      .flatMap(code =>
        CountryData.alpha3ToAlpha2
          .get(code)
          .toRight(Failure.Parsing(s"Unknown country code: $code"))
          .map(unsafe))

  //-------------------------------------------------------------------------
  /**
   * The JSON codec for countries.
   *
   * A country is written as the bare string of its two letter code, so `Country.GB` is the
   * JSON `"GB"` rather than an object naming the field. That is the form the type being
   * ported wrote, which keeps documents readable and keeps a country usable wherever a
   * string is expected.
   *
   * Reading goes through [[of]] and not through [[parse]], so the codec accepts exactly the
   * canonical form it writes: a document holding `"gb"` is rejected rather than quietly
   * folded, which is the behaviour of the string conversion this replaces. Text that names
   * no well formed code is reported with the message of the rejecting check.
   *
   * The codec is assembled from functions at compile time and reads nothing about the type
   * while it runs.
   *
   * @return the codec writing a country as its two letter code
   */
  implicit val codec: Codec[Country] = Codecs.parsedStringCodecNec(text => of(text), _.code)
}
