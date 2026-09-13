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

import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.NoJavaSerialization
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
 * accepted, whether or not it appears in the reference data or among the constants. This type
 * is therefore a validated value rather than one of the closed named families of this library:
 * a code that no standard assigns, such as `AA`, is a country here, and whether it means
 * anything is a question for the data it came from, not for this type.
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
 * two values are equal - so no secondary comparison is needed to reconcile the two. A country
 * renders as its bare code, both through `toString` and through its `Show` instance, and the
 * [[codec]] below writes and reads that same bare string, which is the whole of the support
 * for writing a country out.
 *
 * Values are not interned. Each factory call builds a fresh value, and equality of codes is
 * the only comparison that is meaningful here: two countries with the same code are equal,
 * have equal hash codes and are interchangeable, but they need not be the same object, so
 * they must never be compared by identity.
 *
 * Every member that can be given text it cannot accept - a malformed code, an unknown three
 * letter code, or a country with no three letter form - reports a [[Failure]] on the left of
 * an `Either` rather than interrupting the caller.
 *
 * ===Thread safety===
 *
 * A country is immutable, every member is a pure function of the value and its arguments, and
 * the companion holds nothing that changes after it is initialised, so values of this type may
 * be shared freely between threads.
 *
 * @see [[CountryData]] for the reference data behind the three letter codes
 */
sealed abstract case class Country private (code: String) extends NoJavaSerialization {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means - carrying
  // a code the two-letter check would have refused - can be stopped is here. The single
  // implementation is the companion's hidden `Impl`.
  JvmClosure.requireSoleImplementation(this, classOf[Country.Impl])

  // The invariant of this type, stated over the field the instance actually holds rather than over
  // the argument a factory was given, because the class file of the implementation carries a
  // public constructor whatever the source asked for: a class compiled outside this library can
  // call it directly, and identity alone would then admit a country whose code is lower case,
  // longer or shorter than two characters, or holds something that is not a letter at all - text
  // that renders as a country and matches none. The statement is what [[Country.of]] establishes,
  // over the same predicate that check is written with; it is the shape of the code and not
  // membership of a list, because the set of countries is deliberately open (see [[Country.of]]).
  JvmClosure.requireInvariant(
    "its code is two upper-case ASCII letters",
    code.length == 2 && code.forall(character => Country.isCodeChar(character)))

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
   * The failure reports missing data, because the shortfall is a row the reference data does
   * not hold rather than anything wrong with the country.
   *
   * The lookup goes through the hash index of the reference data rather than through its
   * published sorted table, so it costs one probe rather than a descent of the sorted table;
   * the answer is the same either way, the two being built from one transcription.
   *
   * @return the three letter code, or the failure describing that there is none
   */
  def code3Char: Either[Failure, String] =
    CountryData
      .alpha3CodeOf(code)
      .toRight(Failure.MissingData(s"Unknown country: $code"))

  /**
   * Returns the two letter code of this country.
   *
   * The bare code is the rendering of a country everywhere in this library - in reports, in
   * logs and in JSON - so it is what this returns.
   *
   * @return the two letter country code
   */
  override def toString: String = code
}

/**
 * Provides the countries this library names, the factories that build one from text, and the
 * instances for the type.
 *
 * The constants name a selection of countries, declared in groups by region. They are a
 * convenience for the code that refers to a country by name and carry no privilege: a constant
 * is the same value that the matching call to [[of]] produces, so `Country.of("GB")` and
 * `Country.GB` are equal.
 */
object Country {

  /**
   * Tests whether a character is one the code space allows.
   *
   * The code space is the upper case ASCII letters `A` to `Z`, and nothing else.
   *
   * @param character  the character to test
   * @return true if the character may appear in a country code
   */
  private def isCodeChar(character: Char): Boolean = character >= 'A' && character <= 'Z'

  /**
   * The length of an alpha-2 country code, which is the length [[of]] requires.
   *
   * Held as a constant because two routes read it: the check in [[of]], which is written as a
   * pair of bounds around the code space, and [[parse]], which folds the case only of text that
   * could still fold to a code - text no longer than this, since folding never produces fewer
   * characters than it was given. The length is the one property of caller text that can be
   * decided without looking at any of it.
   */
  private val CodeLength: Int = 2

  /**
   * Builds a country from a code already known to be valid.
   *
   * This is the only place a country is instantiated, and it performs no check, so every
   * caller has to have established validity by other means. Two kinds of caller qualify: the
   * constants below, whose codes are two letter upper case literals written in this file; and
   * the factories below, which call this only on the value handed back by a check that
   * passed. It is private because neither guarantee is available to code outside this object.
   *
   * @param code  the valid two letter country code
   * @return the country
   */
  private def unsafe(code: String): Country = new Impl(code)

  /**
   * The one implementation of a country.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it. It is declared here rather than written as an anonymous subclass at the instantiation
   * site for two reasons, both about what the class file says: a private member class is one a
   * compiler in another language refuses to name, where an anonymous class is public and can be
   * instantiated directly by such a caller; and a named class can be compared against, which is
   * what lets [[Country]] refuse in its own constructor to be any other implementation.
   *
   * @param code  the two letter country code, already established as valid by [[unsafe]]'s callers
   */
  private final class Impl(code: String) extends Country(code)

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

  /**
   * The ordering of countries, which is also their hashing.
   *
   * This is the only equality-bearing instance of the type: `Order` and `Hash` both extend
   * `Eq`, so declaring one instance is what keeps the three from ever disagreeing. Countries
   * order alphabetically by their two letter code and are equal when those codes are equal.
   * The two agree without any further comparison, because the code is the whole of the value -
   * `compare` returns zero exactly when the two countries are equal - so this type needs no
   * secondary comparison to reconcile its ordering with its equality.
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

  /**
   * Returns the set of countries this type knows about, ordered by code.
   *
   * The set is the union of two things: every country named by the alpha-2 side of the
   * reference data in [[CountryData]], and every country named by a constant above. The
   * constants contribute exactly one country the data does not, the `EU` region, which the
   * standard treats specially and gives no three letter code, so the set holds 252 countries.
   *
   * The set is fixed. It describes what the library knows rather than what it has been asked
   * for, so building a country outside it - which [[of]] permits for any well formed code -
   * leaves it unchanged.
   *
   * It is computed on first use and then held, because building it touches the whole of the
   * reference data and most callers never ask for it.
   *
   * @return the countries this type knows about
   */
  lazy val availableCountries: SortedSet[Country] =
    SortedSet.from(CountryData.alpha2Codes.iterator.map(unsafe) ++ constants.iterator)(order.toOrdering)

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
   * The three ways of being malformed - too short, too long, or holding a character outside
   * the code space - share one message, because what the caller has to correct is the same in
   * each case: the text does not have the shape of a country code. The result is the
   * accumulating form so that this factory composes with the checks of a larger value being
   * built around it.
   *
   * @param countryCode  the two letter country code, upper case ASCII
   * @return the country, or the failure describing why the code was rejected
   */
  def of(countryCode: String): ResultNec[Country] =
    Validate.toResult(
      Validate
        .matches(isCodeChar, CodeLength, CodeLength, countryCode, "countryCode", "[A-Z][A-Z]")
        .map(unsafe))

  /**
   * Parses text into a country, folding case first.
   *
   * This is [[of]] applied to the upper case of the text, so it accepts everything `of`
   * accepts and additionally accepts any mixture of case. Nothing else differs: text whose
   * upper case is not a well formed code is rejected with the message `of` would give, which
   * quotes the code exactly as `of` received it - the folded text for an input of two
   * characters, which is the only length a fold can help, and the text as it was supplied for
   * any other length.
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
   * Text longer than [[CodeLength]] characters is not folded, since folding never makes a text
   * shorter and no longer text can fold to a code; it is handed to [[of]] as it stands - where
   * the length is tested before the characters are, so the rejection costs no scan either.
   * Folding first made the cost of rejecting text proportional to its length, the whole of it
   * being copied to be measured once and discarded (CWE-400/CWE-770). Every text that could
   * still fold to a code is folded exactly as before, shorter text included - `ß` folds to the
   * code `SS` and is accepted as it always was - and the message a rejection carries is `of`'s
   * in both cases, quoting the text that reached it.
   *
   * @param countryCode  the two letter country code, in any case
   * @return the country, or the failure describing why the code was rejected
   */
  def parse(countryCode: String): ResultNec[Country] =
    of(if (countryCode.length <= CodeLength) countryCode.toUpperCase(Locale.ENGLISH) else countryCode)

  /**
   * Obtains a country from its ISO-3166-1 alpha-3 three letter code.
   *
   * This accepts fewer codes than [[of]] does, because a three letter code is not an identity
   * of its own: it has to be translated into the two letter code that is, and only the codes
   * held in [[CountryData]] can be translated. The two ways this can fail are therefore
   * distinct, and each reports its own cause:
   *
   * {{{
   * Country.of3Char("GBR")   // Right(GB)
   * Country.of3Char("zzz")   // Left(Failure.Invalid) - not the shape of a three letter code
   * Country.of3Char("ZZZ")   // Left(Failure.Parsing) - well formed, but names no country
   * }}}
   *
   * Because the causes are alternatives rather than things that can both be true of one
   * input - text that is not well formed is never looked up - the result reports a single
   * failure rather than the accumulating form used by `of`. The shape check therefore runs
   * first and the translation only on what it passed, which is also why the lookup may take
   * the code as given.
   *
   * The translation goes through the hash index of the reference data rather than through its
   * published sorted table, for the reason given on [[code3Char]].
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
        CountryData
          .alpha2CodeOf(code)
          .toRight(Failure.Parsing(s"Unknown country code: $code"))
          .map(unsafe))

  /**
   * The JSON codec for countries.
   *
   * A country is written as the bare string of its two letter code, so `Country.GB` is the
   * JSON `"GB"` rather than an object naming the field. That keeps documents readable and
   * keeps a country usable wherever a string is expected.
   *
   * Reading goes through [[of]] and not through [[parse]], so the codec accepts exactly the
   * canonical form it writes: a document holding `"gb"` is rejected rather than quietly
   * folded. Text that names no well formed code is reported with the message of the rejecting
   * check.
   *
   * The codec is assembled from functions at compile time and reads nothing about the type
   * while it runs.
   *
   * @return the codec writing a country as its two letter code
   */
  implicit val codec: Codec[Country] = Codecs.parsedStringCodecNec(text => of(text), _.code)
}
