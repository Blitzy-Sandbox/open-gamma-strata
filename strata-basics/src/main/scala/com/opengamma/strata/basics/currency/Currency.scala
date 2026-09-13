/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.math.BigDecimal
import java.math.RoundingMode
import java.util.Locale

import cats.Hash
import cats.Order
import cats.Show
import cats.data.NonEmptyList

import io.circe.Codec
import io.circe.KeyDecoder
import io.circe.KeyEncoder

import com.opengamma.strata.collect.Decimal
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.named.NamedEnum
import com.opengamma.strata.collect.result.Failure

/**
 * A unit of currency.
 *
 * A currency is a unit in which an amount of money is denominated, such as the British pound,
 * the euro or the US dollar, identified by the three letter code of ISO-4217. It is the
 * smallest and most widely used value of this module: an amount, a pair, a rate, a payment and
 * every index that produces a rate in a currency all carry one, and they carry it as an
 * identity rather than as data, which is why a currency holds so little of its own - a code,
 * the number of digits its minor unit has, and the currency through which a quote for it is
 * triangulated when no direct quote exists.
 *
 * A currency is obtained from a named constant, which is how nearly every caller should reach
 * one, or by resolving text:
 *
 * {{{
 * Currency.GBP                 // the constant
 * Currency.of("GBP")           // Right(GBP) - exact, case sensitive
 * Currency.parse("gbp")        // Right(GBP) - tolerates the case of the input
 * Currency.of("ZZZ")           // Left(Failure.Parsing(...)) - no such currency
 * }}}
 *
 * ===A closed family===
 *
 * This is a closed family of exactly 74 currencies. Every instance is built once, inside the
 * companion, from the compiled reference data of [[CurrencyData]], and no other code can build
 * one: the class is sealed, so it cannot be extended outside this file, and its constructor is
 * restricted to this package. Resolving a code is therefore a lookup over a set that is fixed
 * when this module is compiled, and the currencies a program can obtain are those 74 whatever
 * it has resolved before.
 *
 * 55 of the 74 are currencies in active use and each has a named constant below. The remaining
 * 19 are the historic currencies superseded by the euro; they resolve through [[Currency.of]]
 * and [[Currency.parse]] with their real minor units and their real triangulation currency, but
 * they have no constant and they are not among the [[getAvailableCurrencies]].
 *
 * A code outside those 74 is refused: [[Currency.of]] and [[Currency.parse]] answer a `Failure`
 * whose reason is `PARSING` rather than a currency. That refusal is the contract of this type
 * and is deliberate, because data invented for an unknown currency is indistinguishable from
 * data that was configured, and a mistyped or unsupported code would then become a silently
 * wrong amount of money instead of an error. The currencies a caller can name are exactly the
 * currencies whose data this module holds.
 *
 * ===Names are the contract===
 *
 * The code is the identity of a currency in text: it is what [[name]] and `toString` return,
 * what `Show` renders, what the JSON codec writes, and what keys an object whose keys are
 * currencies. All of them produce the same three letters, so a currency that reaches a document,
 * a log line or a message is the three letters of its code wherever it came from.
 *
 * This family declares no alternate spelling, no lenient rewrite and no group of external
 * names, because the reference data behind it defines none: a currency has exactly one name.
 * The only leniency available is therefore the fold to upper case that [[Currency.parse]]
 * applies.
 *
 * ===Thread safety===
 *
 * A currency is immutable and every one of its members is computed from its fields alone. The
 * companion builds its instances and its lookup tables while it initialises and changes nothing
 * afterwards, so there is no cache to synchronise and nothing to populate later. Values of this
 * type may be shared freely between threads.
 *
 * @param code               the ISO-4217 three letter currency code, upper case
 * @param minorUnitDigits    the number of digits in the minor unit, such as 2 for cents in the
 *                           dollar
 * @param triangulationCode  the three letter code of the currency to triangulate quotes
 *                           through, held as text rather than as a [[Currency]] so that the
 *                           instances of this family can be built in any order
 * @see [[CurrencyData]] for the reference data behind the family
 */
sealed abstract class Currency private[currency] (
    val code: String,
    val minorUnitDigits: Int,
    private val triangulationCode: String)
    extends Named
    with NoJavaSerialization {

  // The closure of this family, run for every member as it is constructed: `sealed` and a
  // constructor private to the package are enforced against Scala and leave nothing in the class
  // file, so a subtype compiled by other means - which would be a seventy-fifth currency, minted
  // outside the reference data exactly as the dynamic creation this port dropped used to do - is
  // refused here instead. Every member of the family is an instance of the companion's `Impl`,
  // which is declared inside `Currency` and so satisfies the check; a class declared anywhere
  // else cannot.
  JvmClosure.requireDeclaredMember(this, classOf[Currency])

  // The invariant of this family, stated over the fields the instance actually holds. The check
  // above admits the class `Impl` that the companion declares and hides, and that is not enough on
  // its own: a private member class is emitted into the class file as public, with a public
  // constructor, and only its inner-class entry records the request to hide it - which a Java
  // compiler honours and a class file naming the class directly does not. A caller taking that
  // route would hold an instance of exactly the class the check admits, carrying whatever it
  // passed: a seventy-fifth code, or a published code paired with minor units and a triangulation
  // currency of its own choosing - which is the dynamic currency creation this port removed,
  // regained through the back door.
  //
  // So every field is required to be the field the reference data declares for the code. The data
  // is the single source of the family - the companion builds each member from one of its rows -
  // and reading it here costs one map lookup per member, performed 74 times as this family
  // initialises. The lookup cannot recur into this class: `CurrencyData` holds rows of plain text
  // and numbers and names no `Currency` at all, which is why its triangulation currency is a code
  // rather than an instance, and its table is fully built before the first member is constructed
  // because the companion reads `CurrencyData.rows` to construct them.
  JvmClosure.requireInvariant(
    "its code, minor unit digits and triangulation currency are those the reference data of the " +
      "family declares for that code",
    CurrencyData.byCode
      .get(code)
      .exists(row =>
        row.minorUnitDigits == minorUnitDigits &&
          row.triangulationCurrencyCode == triangulationCode))

  /**
   * Gets the unique name of this currency, which is its three letter code.
   *
   * @return the three letter ISO-4217 code
   */
  override def name: String = code

  /**
   * Gets the preferred currency to triangulate a quote for this currency through.
   *
   * When a market quote for a currency is wanted and no direct rate is available, the rate can
   * often be built from two rates that pass through a third currency. For example there is no
   * direct rate for `CZK/SGD`, but `CZK` triangulates through `EUR` and `SGD` through `USD`, so
   * the rates `CZK/EUR`, `EUR/USD` and `USD/SGD` together determine one. Most currencies
   * triangulate through `USD`; the historic currencies superseded by the euro triangulate
   * through `EUR`.
   *
   * The field behind this is the three letter code rather than a currency, which is what makes
   * the family safe to build: the instances are created one row at a time, so the instance a
   * row triangulates through may not exist when that row is read, and the order in which `USD`
   * and `EUR` are built cannot be relied on. Resolving the code is deferred to the first caller
   * and the result is kept, by which time the whole family exists.
   *
   * @return the triangulation currency
   */
  lazy val triangulationCurrency: Currency =
    // Every triangulation code in the reference data is one of the 74 codes of the family, so
    // the lookup always succeeds. USD is named as the alternative because it is the
    // triangulation currency of 55 of the 74 rows, which makes a data error degrade to the most
    // common answer instead of making this accessor partial.
    Currency.valueOf(triangulationCode).getOrElse(Currency.USD)

  /**
   * Rounds the specified amount to the minor unit of this currency.
   *
   * Rounding is half up, away from zero at a tie, which is the rounding money is quoted with.
   * For example `USD` has two minor unit digits, so `63.347` rounds to `63.35`, while `JPY` has
   * none and `63.347` rounds to `63`.
   *
   * The amount is routed through an exact decimal rather than rounded in binary floating point,
   * because the two do not agree: a `Double` cannot represent most decimal fractions, so
   * scaling one by a power of ten and rounding the result moves ties in ways that depend on the
   * value.
   *
   * @param amount  the amount to round
   * @return the rounded amount
   */
  def roundMinorUnits(amount: Double): Double =
    roundMinorUnits(BigDecimal.valueOf(amount)).doubleValue()

  /**
   * Rounds the specified decimal amount to the minor unit of this currency.
   *
   * Rounding is half up, so `USD` rounds `63.347` to `63.35`. The result carries the scale of
   * the minor unit exactly, so rounding `63.3` for `USD` produces `63.30` rather than `63.3`,
   * which is the behaviour of setting the scale of a decimal.
   *
   * @param amount  the amount to round
   * @return the rounded amount
   */
  def roundMinorUnits(amount: BigDecimal): BigDecimal =
    amount.setScale(minorUnitDigits, RoundingMode.HALF_UP)

  /**
   * Rounds the specified decimal amount to the minor unit of this currency.
   *
   * Rounding is half up, so `USD` rounds `63.347` to `63.35`. In contrast to the arbitrary
   * precision form above, [[Decimal]] normalises its scale, so an amount that needs fewer digits
   * than the minor unit allows is returned unchanged rather than padded: rounding `63.3` for
   * `USD` produces `63.3`. That is the behaviour of the decimal type itself, to which this
   * member delegates.
   *
   * @param amount  the amount to round
   * @return the rounded amount
   */
  def roundMinorUnits(amount: Decimal): Decimal =
    amount.roundToScale(minorUnitDigits, RoundingMode.HALF_UP)

  /**
   * Checks if this currency equals another currency.
   *
   * Two currencies are equal when their codes are equal. Since the family is closed and each
   * code exists exactly once, two equal currencies are in practice the same instance; comparing
   * the codes rather than the instances keeps the answer defined for any value of this type and
   * matches [[hashCode]], the `Hash` instance and the ordering, all of which read the code.
   *
   * @param obj  the other value to compare to
   * @return true when the other value is a currency with the same code
   */
  override def equals(obj: Any): Boolean = obj match {
    case other: Currency => (this eq other) || code == other.code
    case _ => false
  }

  /**
   * Returns a hash code consistent with [[equals]].
   *
   * @return the hash code of the three letter code
   */
  override def hashCode: Int = code.hashCode

  /**
   * Returns the three letter code of this currency.
   *
   * @return the three letter ISO-4217 code
   */
  override def toString: String = code
}

/**
 * Holds the 74 currencies of the family, the constants that name them and the routes from text.
 *
 * This companion is the only place a [[Currency]] is created. It reads the compiled rows of
 * [[CurrencyData]] once, while it initialises, and every currency a program ever holds is one of
 * the instances it makes there, reached through a constant, through [[values]], or through
 * [[of]], [[parse]] and [[valueOf]].
 *
 * ===The invariant this relies on===
 *
 * The reference data behind the family is a literal table of 74 rows compiled into this module,
 * with distinct codes, whose 55 active rows are exactly the 55 codes given constants below. Two
 * places rely on that: [[values]] states that the family has at least one member, and each
 * constant reads its row by code. A row that went missing or a code that was mistyped therefore
 * fails while this module initialises, loudly and immediately, rather than leaving a family
 * with a member whose data is invented.
 */
object Currency {

  /**
   * The name of this family, as it appears when text names no currency.
   *
   * Held once so that the name lookup and the failures reported by [[of]] and [[parse]] cannot
   * describe the family differently.
   */
  private val FamilyName: String = "Currency"

  /**
   * The length of a currency code, which every name of this closed family has.
   *
   * The 74 currencies are named by three-letter ISO-4217 codes and by nothing else, and folding
   * a text never produces fewer characters than it was given, so text '''longer''' than three
   * characters names no currency whatever its case. [[parse]] reads that as the one thing it can
   * decide before doing any work: it folds the case of any text that could still fold to a code
   * - which includes shorter text, since the German sharp s folds to two letters - and hands
   * only longer text to [[of]] as it stands, which resolves it against the table and reports the
   * same failure the fold would have led to. Folding first made the cost of rejecting text
   * proportional to its length - a text of a million characters was copied in full to be looked
   * up once and discarded (CWE-400/CWE-770) - and folding is what the length test now guards.
   */
  private val CodeLength: Int = 3

  /**
   * Builds the single instance of the specified reference data row.
   *
   * The instance is an [[Impl]] because [[Currency]] is abstract: making it abstract and sealed
   * is what closes the family, since it leaves this companion as the only code able to produce a
   * value of the type. The row carries every field a currency has, so nothing here is derived or
   * defaulted.
   *
   * @param row  the reference data row to build the currency of
   * @return the currency of that row
   */
  private def instanceOf(row: CurrencyRow): Currency =
    new Impl(row.code, row.minorUnitDigits, row.triangulationCurrencyCode)

  /**
   * The one implementation of a currency, and so the class of all 74 members of the family.
   *
   * An abstract family needs a concrete subclass to be instantiated at all, and this is it. It is
   * declared rather than written as an anonymous subclass at the instantiation site for two
   * reasons, both about what the class file says: a private member class is one a Java compiler
   * refuses to name, where an anonymous class is public and can be instantiated directly by a
   * caller in another language, and a class declared inside this companion is one only these
   * sources can declare, which is what lets [[Currency]] refuse in its own constructor to be a
   * member the family does not publish.
   *
   * @param code  the ISO-4217 three letter currency code, upper case, as the row carries it
   * @param minorUnitDigits  the number of digits in the minor unit
   * @param triangulationCode  the three letter code of the currency to triangulate quotes through
   */
  private final class Impl(code: String, minorUnitDigits: Int, triangulationCode: String)
      extends Currency(code, minorUnitDigits, triangulationCode)

  /**
   * The 74 currencies, one per reference data row, in the declaration order of that data.
   *
   * This is the single point of creation of the family. Everything else - the constants, the
   * lookup tables, the set of available currencies - selects from these instances rather than
   * building its own, so a currency reached by any route is the same object as the currency
   * reached by every other, and `eq` agrees with `==` throughout.
   */
  private val instances: Vector[Currency] = CurrencyData.rows.map(instanceOf)

  /**
   * The 74 currencies keyed by their three letter code.
   *
   * Derived from [[instances]], so the two can never disagree. The codes of the reference data
   * are distinct, so the map holds all 74.
   */
  private val instancesByCode: Map[String, Currency] =
    instances.iterator.map(currency => currency.code -> currency).toMap

  /**
   * Obtains the currency of the specified code, for the constants declared below.
   *
   * Reading a constant's currency out of the table rather than constructing it is what keeps a
   * constant and the result of resolving its code the same instance with the same data. Every
   * code passed here is a code of the table, which is the invariant described on this
   * companion.
   *
   * @param code  the three letter code of a currency of the reference data
   * @return the currency with that code
   */
  private def configured(code: String): Currency = instancesByCode(code)

  // a selection of commonly traded, stable currencies
  /** The currency 'USD' - United States Dollar. */
  val USD: Currency = configured("USD")
  /** The currency 'EUR' - Euro. */
  val EUR: Currency = configured("EUR")
  /** The currency 'JPY' - Japanese Yen. */
  val JPY: Currency = configured("JPY")
  /** The currency 'GBP' - British Pound. */
  val GBP: Currency = configured("GBP")
  /** The currency 'CHF' - Swiss Franc. */
  val CHF: Currency = configured("CHF")
  /** The currency 'AUD' - Australian Dollar. */
  val AUD: Currency = configured("AUD")
  /** The currency 'CAD' - Canadian Dollar. */
  val CAD: Currency = configured("CAD")
  /** The currency 'NZD' - New Zealand Dollar. */
  val NZD: Currency = configured("NZD")

  // a selection of other currencies
  /** The currency 'AED' - UAE Dirham. */
  val AED: Currency = configured("AED")
  /** The currency 'ARS' - Argentine Peso. */
  val ARS: Currency = configured("ARS")
  /** The currency 'BGN' - Bulgarian Lev. */
  val BGN: Currency = configured("BGN")
  /** The currency 'BHD' - Bahraini Dinar. */
  val BHD: Currency = configured("BHD")
  /** The currency 'BRL' - Brazilian Real. */
  val BRL: Currency = configured("BRL")
  /** The currency 'CLP' - Chilean Peso. */
  val CLP: Currency = configured("CLP")
  /** The currency 'CNH' - Chinese Offshore Yuan. */
  val CNH: Currency = configured("CNH")
  /** The currency 'CNY' - Chinese Onshore Yuan. */
  val CNY: Currency = configured("CNY")
  /** The currency 'COP' - Colombian Peso. */
  val COP: Currency = configured("COP")
  /** The currency 'CZK' - Czech Koruna. */
  val CZK: Currency = configured("CZK")
  /** The currency 'DKK' - Danish Krone. */
  val DKK: Currency = configured("DKK")
  /** The currency 'EGP' - Egyptian Pound. */
  val EGP: Currency = configured("EGP")
  /** The currency 'HKD' - Hong Kong Dollar. */
  val HKD: Currency = configured("HKD")
  /** The currency 'HRK' - Croatian Kuna. */
  val HRK: Currency = configured("HRK")
  /** The currency 'HUF' - Hungarian Forint. */
  val HUF: Currency = configured("HUF")
  /** The currency 'IDR' - Indonesian Rupiah. */
  val IDR: Currency = configured("IDR")
  /** The currency 'ILS' - Israeli Shekel. */
  val ILS: Currency = configured("ILS")
  /** The currency 'INR' - Indian Rupee. */
  val INR: Currency = configured("INR")
  /** The currency 'ISK' - Icelandic Krona. */
  val ISK: Currency = configured("ISK")
  /** The currency 'KRW' - South Korean Won. */
  val KRW: Currency = configured("KRW")
  /** The currency 'KZT' - Kazakhstani Tenge. */
  val KZT: Currency = configured("KZT")
  /** The currency 'MAD' - Moroccan Dirham. */
  val MAD: Currency = configured("MAD")
  /** The currency 'MXN' - Mexican Peso. */
  val MXN: Currency = configured("MXN")
  /** The currency 'MYR' - Malaysian Ringgit. */
  val MYR: Currency = configured("MYR")
  /** The currency 'NOK' - Norwegian Krone. */
  val NOK: Currency = configured("NOK")
  /** The currency 'OMR' - Omani Rial. */
  val OMR: Currency = configured("OMR")
  /** The currency 'PEN' - Peruvian Nuevo Sol. */
  val PEN: Currency = configured("PEN")
  /** The currency 'PHP' - Philippine Peso. */
  val PHP: Currency = configured("PHP")
  /** The currency 'PKR' - Pakistani Rupee. */
  val PKR: Currency = configured("PKR")
  /** The currency 'PLN' - Polish Zloty. */
  val PLN: Currency = configured("PLN")
  /** The currency 'QAR' - Qatari Riyal. */
  val QAR: Currency = configured("QAR")
  /** The currency 'RON' - Romanian New Leu. */
  val RON: Currency = configured("RON")
  /** The currency 'RUB' - Russian Ruble. */
  val RUB: Currency = configured("RUB")
  /** The currency 'SAR' - Saudi Riyal. */
  val SAR: Currency = configured("SAR")
  /** The currency 'SEK' - Swedish Krona. */
  val SEK: Currency = configured("SEK")
  /** The currency 'SGD' - Singapore Dollar. */
  val SGD: Currency = configured("SGD")
  /** The currency 'THB' - Thai Baht. */
  val THB: Currency = configured("THB")
  /** The currency 'TRY' - Turkish Lira. */
  val TRY: Currency = configured("TRY")
  /** The currency 'TWD' - New Taiwan Dollar. */
  val TWD: Currency = configured("TWD")
  /** The currency 'UAH' - Ukrainian Hryvnia. */
  val UAH: Currency = configured("UAH")
  /** The currency 'VND' - Vietnamese Dong. */
  val VND: Currency = configured("VND")
  /** The currency 'ZAR' - South African Rand. */
  val ZAR: Currency = configured("ZAR")

  // special cases
  /** The currency 'XXX' - No applicable currency. */
  val XXX: Currency = configured("XXX")
  /** The currency 'XAG' - Silver (troy ounce). */
  val XAG: Currency = configured("XAG")
  /** The currency 'XAU' - Gold (troy ounce). */
  val XAU: Currency = configured("XAU")
  /** The currency 'XPD' - Palladium (troy ounce). */
  val XPD: Currency = configured("XPD")
  /** The currency 'XPT' - Platinum (troy ounce). */
  val XPT: Currency = configured("XPT")

  /**
   * The 74 currencies of the family, in the declaration order of the reference data.
   *
   * The order is the one the reference data declares - the active currencies alphabetically,
   * then the metal and unapplicable currencies, then the historic currencies superseded by the
   * euro - and not the alphabetical order the `Order` instance below imposes. It is observable
   * through any iteration a caller performs, so it is kept stable rather than re-sorted.
   *
   * The list is non-empty, which is what its type states and what lets every operation over the
   * family - a lookup table, an exhaustive report - be written without a case for a family that
   * has no members. That rests on the table being a compiled literal of 74 rows, the invariant
   * described on this companion.
   *
   * @return the 74 currencies, in reference data order
   */
  val values: NonEmptyList[Currency] = NonEmptyList.fromListUnsafe(instances.toList)

  /**
   * The name lookup for this family.
   *
   * Built from [[values]] alone. All three tables a named family may declare are empty, because
   * the reference data behind this one declares none of them: a currency has a single name, and
   * there is no retired spelling to accept, no pattern that rewrites text before it is looked
   * up, and no group of names published for another protocol. The whole name space of the
   * family is therefore its 74 codes.
   *
   * Since every code is already upper case, the two keys each member registers under coincide
   * and the lookup is a plain exact match on the code.
   *
   * @return the name lookup for the 74 currencies
   */
  implicit val namedEnum: NamedEnum[Currency] =
    NamedEnum.of(values, Map.empty, Nil, Map.empty, FamilyName)

  /**
   * Obtains the currency with the specified code, if one exists.
   *
   * The match is exact and case sensitive, so `GBP` resolves while `gbp` does not. Use [[parse]]
   * to accept text whose case is not known in advance, and [[of]] for the same exact lookup
   * reported as a failure rather than as an absent value.
   *
   * @param name  the three letter code to look up
   * @return the currency with that code, or `None` when no currency has it
   */
  def valueOf(name: String): Option[Currency] = namedEnum.valueOf(name)

  /**
   * Obtains the currency with the specified ISO-4217 code.
   *
   * The code is matched exactly, so it must already be upper case; [[parse]] is the
   * case-tolerant form. All 74 currencies resolve, including the 19 superseded by the euro, each
   * with the minor units and triangulation currency its reference data row carries:
   *
   * {{{
   * Currency.of("GBP")   // Right(GBP)
   * Currency.of("BEF")   // Right(BEF) - historic, 2 minor unit digits, triangulates via EUR
   * Currency.of("gbp")   // Left - the code is not upper case
   * Currency.of("ZZZ")   // Left - no such currency
   * }}}
   *
   * A code the family does not hold is reported rather than invented: the answer is a `Failure`
   * whose reason is `PARSING`, quoting the code back as it was given. No currency is created for
   * an unknown code, so the 74 currencies of the family are the only currencies that exist.
   *
   * @param currencyCode  the three letter currency code, upper case
   * @return the currency with that code, or the failure naming a code that is not one of the 74
   *   codes of the family, the case of the code included
   */
  def of(currencyCode: String): Either[Failure, Currency] =
    valueOf(currencyCode).toRight(notFound(currencyCode))

  /**
   * Parses text into a currency, tolerating the case of the input.
   *
   * The text is folded to upper case in the English locale and then resolved exactly as [[of]]
   * resolves a code. The English locale is named explicitly so that the fold is the same in
   * every locale a program might run in - the Turkish locale, for instance, folds `i` to a
   * dotted capital that names no currency.
   *
   * {{{
   * Currency.parse("gbp")   // Right(GBP)
   * Currency.parse("GbP")   // Right(GBP)
   * Currency.parse("ZZZ")   // Left(Failure.Parsing("Currency name not found: ZZZ"))
   * }}}
   *
   * The failure of a code the family does not hold is the same failure [[of]] reports, and the
   * text it names is the folded text that was looked up, quoted as it was folded.
   *
   * Text longer than [[CodeLength]] characters is not folded, because folding never makes a text
   * shorter and no text longer than a code can fold to one; it is resolved as it was given and
   * reported with the message [[of]] gives it, which is the message the fold would have led to.
   * Every text that could still fold to a code is folded exactly as before, shorter text
   * included - `ßP` folds to `SSP` and resolves as it always did - so nothing a caller can
   * observe about a code of any plausible spelling has changed.
   *
   * @param currencyCode  the three letter currency code, in any case
   * @return the currency the text names, or the failure naming folded text that is not one of
   *   the 74 codes of the family
   */
  def parse(currencyCode: String): Either[Failure, Currency] =
    of(
      if (currencyCode.length <= CodeLength) currencyCode.toUpperCase(Locale.ENGLISH)
      else currencyCode)

  /**
   * The failure reported for text that names no currency of the family.
   *
   * The wording is written once here, so the failure a caller sees is the same whether the code
   * was resolved directly or while decoding a document. The failure deliberately carries no
   * attributes, which makes two failures over the same text equal and therefore directly
   * comparable. The code is quoted as it stands, so the failure names exactly what was rejected.
   *
   * @param currencyCode  the code that was rejected, as it was looked up
   * @return the failure naming the family and the code
   */
  private def notFound(currencyCode: String): Failure =
    Failure.Parsing(s"$FamilyName name not found: $currencyCode")

  /**
   * The set of currencies that are in active use.
   *
   * These are the 55 currencies the reference data marks as active, which are exactly the 55
   * named by the constants above. The 19 historic currencies superseded by the euro are
   * deliberately absent: they remain resolvable through [[of]] and [[parse]] with their real
   * data, so an amount denominated in one can still be read, but they are not offered as
   * currencies to choose from. The historic flag of a reference data row is what draws that
   * distinction.
   *
   * @return the 55 currencies in active use
   */
  val getAvailableCurrencies: Set[Currency] =
    CurrencyData.nonHistoricCodes.iterator.map(configured).toSet

  /**
   * The ordering of currencies, which is also their hashing and their equality.
   *
   * This is the only equality-bearing instance of the type: `Order` and `Hash` both extend
   * `Eq`, so summoning any of the three yields this one value and the three can never disagree.
   * Currencies order alphabetically by their three letter code and they are equal when those
   * codes are equal. The two agree without any secondary comparison, because the code is the
   * identity of a currency: `compare` returns zero exactly when `eqv` holds, since the codes of
   * the family are distinct. The instance also agrees with the `equals` and `hashCode` of the
   * type, both of which read the code.
   *
   * @return the ordering of currencies by code, which is also their hashing
   */
  implicit val order: Order[Currency] with Hash[Currency] = NamedEnum.orderByName[Currency]

  /**
   * The rendering of currencies as text.
   *
   * A currency renders as its three letter code and as nothing else, which is what `toString`
   * produces as well, so the two ways of putting a currency into a message agree.
   *
   * @return the rendering of a currency as its code
   */
  implicit val show: Show[Currency] = NamedEnum.showByName[Currency]

  /**
   * The JSON codec for currencies.
   *
   * A currency is written as the bare string of its code - `"GBP"` - and never as an object, so
   * every document holding a currency holds the three letters of its code. Decoding resolves
   * the string through the name lookup of the family, so a code the family does not hold is a
   * decoding failure carrying the message that lookup produced.
   *
   * The codec is derived here at compile time from the family's own lookup; nothing about it
   * inspects a type while the program runs.
   *
   * @return the codec writing a currency as its code
   */
  implicit val codec: Codec[Currency] = Codecs.namedEnumCodec[Currency]

  /**
   * The key codecs that let a currency key a JSON object.
   *
   * Several types of this module hold an amount per currency and serialize it as an object
   * keyed by currency, so a currency has to be usable as a key and not only as a value. A key
   * is text, and the text is the code, which makes such an object read as
   * `{"GBP": 100.0, "USD": 200.0}`.
   *
   * Decoding a key uses the exact lookup rather than the case-tolerant one: the text on the
   * wire was written by this library from a code that is already upper case, so it needs no
   * rehabilitation, and a key decoder has nowhere to report a reason in any case.
   */
  private val keyCodecs: (KeyEncoder[Currency], KeyDecoder[Currency]) =
    Codecs.namedKeyCodecs[Currency](name => valueOf(name))

  /**
   * The JSON key encoder for currencies, writing a currency as its code.
   *
   * @return the key encoder
   */
  implicit val keyEncoder: KeyEncoder[Currency] = keyCodecs._1

  /**
   * The JSON key decoder for currencies, reading a currency from its code.
   *
   * @return the key decoder
   */
  implicit val keyDecoder: KeyDecoder[Currency] = keyCodecs._2
}
