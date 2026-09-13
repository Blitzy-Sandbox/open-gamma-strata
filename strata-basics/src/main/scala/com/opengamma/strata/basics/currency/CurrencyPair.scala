/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.currency

import java.util.Locale

import scala.collection.immutable.HashMap
import scala.collection.immutable.ListSet
import scala.collection.immutable.Set
import scala.util.matching.Regex

import cats.Hash
import cats.Order
import cats.Show

import io.circe.Codec

import com.opengamma.strata.collect.NoJavaSerialization
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * An ordered pair of currencies, such as `EUR/USD`.
 *
 * This identifies two currencies for the purpose of quoting a rate between them. The first
 * currency is the '''base''' and the second is the '''counter''', so in the pair `AAA/BBB` the
 * base is `AAA` and the counter is `BBB`, and a rate quoted for the pair is the amount of the
 * counter currency that one unit of the base currency buys. `FxRate` is the representation that
 * carries such a rate.
 *
 * ===Construction cannot fail===
 *
 * A pair of two currencies is always meaningful, including a pair of one currency with itself -
 * the identity pair `AAA/AAA`, whose rate is one. There is therefore nothing for a factory to
 * reject, and this is a plain `case class` whose `apply` and `copy` are public and answer a pair
 * for any two currencies. Most types of this package validate their input and answer with a
 * failure instead; a pair has no invalid input to catch. [[CurrencyPair.of]] is the same
 * construction reached under a factory name.
 *
 * ===Market convention===
 *
 * Exactly one of the two pairs that can be built from two currencies is the one the market
 * quotes: a rate between the euro and the dollar is quoted as `EUR/USD` and not as `USD/EUR`.
 * [[isConventional]] answers which, and [[toConventional]] turns either direction into it. The
 * answer comes from reference data where it is known, and from a documented deterministic rule
 * where it is not, so two pairs built independently from the same two currencies always agree on
 * which of them is conventional. See [[isConventional]] for the full decision procedure.
 *
 * ===Text form===
 *
 * A pair is rendered as its two codes separated by a slash, `EUR/USD`, by [[toString]], by the
 * `Show` instance and by the JSON codec, and [[CurrencyPair.parse]] reads that same form back.
 * This text is the identity of the pair: it is what appears in documents and messages, and the
 * three routes that write it all write these same seven characters.
 *
 * ===Thread safety===
 *
 * Both fields are immutable currencies and nothing here is computed from mutable state, so a
 * pair may be shared freely between threads. The derived answers - the convention, the rate
 * digits - are pure functions of the two currencies and the compiled reference data, so they are
 * recomputed rather than cached and are the same for every caller at every moment.
 *
 * @param base     the base currency, which is `AAA` in the pair `AAA/BBB`
 * @param counter  the counter currency, which is `BBB` in the pair `AAA/BBB`; also known as the
 *                 ''quote currency'' or the ''variable currency''
 * @see [[CurrencyPairData]] for the conventional pairs and their rate digits
 * @see [[CurrencyData]] for the market convention priority ordering
 */
final case class CurrencyPair(base: Currency, counter: Currency) extends NoJavaSerialization {

  /**
   * Gets the inverse currency pair.
   *
   * The inverse holds the same two currencies in the opposite order, so the inverse of `AAA/BBB`
   * is `BBB/AAA`. A rate quoted for the inverse is the reciprocal of a rate quoted for this
   * pair. The inverse of an identity pair is itself.
   *
   * @return the pair with the base and counter currencies exchanged
   */
  def inverse: CurrencyPair = CurrencyPair(counter, base)

  /**
   * Checks whether this pair holds the specified currency as either its base or its counter.
   *
   * @param currency  the currency to look for
   * @return true when the currency is the base or the counter of this pair
   */
  def contains(currency: Currency): Boolean = base == currency || counter == currency

  /**
   * Finds the other currency of this pair.
   *
   * Given the pair `AAA/BBB`, `AAA` yields `BBB` and `BBB` yields `AAA`. A currency that this
   * pair does not hold is reported as a failure, because which currency a caller asks about is
   * data rather than a coding error - it typically arrives from the same document or market data
   * as the pair itself. The failure names that currency and this pair.
   *
   * For an identity pair the answer is that same currency, which is what asking for "the other
   * one" means when both are the same.
   *
   * {{{
   * CurrencyPair(Currency.GBP, Currency.USD).other(Currency.GBP)   // Right(USD)
   * CurrencyPair(Currency.GBP, Currency.USD).other(Currency.USD)   // Right(GBP)
   * CurrencyPair(Currency.GBP, Currency.USD).other(Currency.EUR)   // Left(Failure.Invalid(...))
   * }}}
   *
   * @param currency  the currency whose counterpart in this pair is wanted
   * @return the other currency of the pair, or the failure naming a currency that is neither the
   *   base nor the counter of this pair
   */
  def other(currency: Currency): Either[Failure, Currency] =
    if (currency == base) {
      Right(counter)
    } else if (currency == counter) {
      Right(base)
    } else {
      // The message names the pair by its text form, as the library being ported does, and the
      // whole of the cost of this branch is that message and the failure carrying it: nothing is
      // built on the way to deciding that the currency is absent, and the two comparisons above
      // are what a caller pays when it is present. A message naming the caller's own value cannot
      // be prepared in advance the way the constant-message failures of this package are, so what
      // is kept off the succeeding path here is everything except the failure itself.
      Left(Failure.Invalid(
        s"Unable to find other currency, ${currency.code} is not present in ${this.toString}"))
    }

  /**
   * Checks whether this is an identity pair.
   *
   * The identity pair is the one whose base and counter currencies are the same, such as
   * `GBP/GBP`. Its rate is one, and it is the pair a conversion that changes nothing uses.
   *
   * @return true when the base and counter currencies are equal
   */
  def isIdentity: Boolean = base == counter

  /**
   * Checks whether this pair is the inverse of the specified pair.
   *
   * This answers whether a rate quoted for the other pair has to be inverted to serve as a rate
   * for this one. An identity pair is the inverse of itself.
   *
   * @param other  the pair to compare against
   * @return true when this pair holds the same two currencies in the opposite order
   */
  def isInverse(other: CurrencyPair): Boolean = base == other.counter && counter == other.base

  /**
   * Finds the pair that is a cross between this pair and the specified pair.
   *
   * A cross exists only when the two pairs name three distinct currencies between them and
   * neither is an identity pair: the shared currency is the one the two rates are multiplied
   * through, and the cross is the pair of the two currencies that remain, in market convention
   * order.
   *
   *  - `AAA/BBB` crossed with `BBB/CCC` gives `AAA/CCC` or `CCC/AAA`, whichever is conventional.
   *  - `AAA/BBB` crossed with `CCC/DDD` gives nothing, as there is no shared currency.
   *  - `AAA/AAA` crossed with `AAA/BBB` gives nothing, as one pair is an identity.
   *  - `AAA/BBB` crossed with `AAA/BBB` gives nothing, as only two currencies are named.
   *  - `AAA/AAA` crossed with `AAA/AAA` gives nothing, for both of the previous reasons.
   *
   * The four cases below are tested in a fixed order, and that order is part of the contract
   * rather than an implementation detail: a caller computing a cross rate reads the base and
   * counter of the returned pair to decide which of its two rates to apply in which direction,
   * so a pair sharing both a base and a counter with the other pair resolves one determinate
   * way.
   *
   * @param other  the pair to cross this one with
   * @return the cross pair in market convention order, or no pair when no cross exists
   */
  def cross(other: CurrencyPair): Option[CurrencyPair] =
    if (isIdentity || other.isIdentity || this == other || this == other.inverse) {
      None
    } else if (counter == other.base) {
      // AAA/BBB cross BBB/CCC
      Some(CurrencyPair(base, other.counter).toConventional)
    } else if (counter == other.counter) {
      // AAA/BBB cross CCC/BBB
      Some(CurrencyPair(base, other.base).toConventional)
    } else if (base == other.base) {
      // BBB/AAA cross BBB/CCC
      Some(CurrencyPair(counter, other.counter).toConventional)
    } else if (base == other.counter) {
      // BBB/AAA cross CCC/BBB
      Some(CurrencyPair(counter, other.base).toConventional)
    } else {
      None
    }

  /**
   * Checks whether this pair is the market convention pair for its two currencies.
   *
   * A market convention determines that a rate is quoted one way round and not the other, and
   * exactly one of the two pairs that can be built from two currencies follows it. The decision
   * is made in four steps, in this order:
   *
   *  1. this pair is one of the conventional pairs held in [[CurrencyPairData]], so it is
   *     conventional;
   *  1. the [[inverse]] of this pair is one of them, so this pair is the quoted direction
   *     reversed and is not conventional;
   *  1. neither direction is configured, so the market convention priority ordering held in
   *     [[CurrencyData]] decides: the currency appearing earlier in that ordering should be the
   *     base, and a currency the ordering does not list ranks behind every currency it does;
   *  1. the ordering lists neither currency, so the codes are compared lexicographically, which
   *     is arbitrary but deterministic - what matters is that two pairs built independently from
   *     the same two currencies reach the same answer.
   *
   * The reference data holds only the conventional direction of each configured pair, and that
   * asymmetry is what makes the first two steps decidable; see [[CurrencyPairData]], which
   * records the same rule from the data side.
   *
   * The final comparison is deliberately ''not strict'': equal codes answer true, which makes an
   * identity pair conventional. Conversion and matrix code depends on that, since the identity
   * pair is its own inverse and a false answer would leave it with no conventional direction at
   * all.
   *
   * {{{
   * CurrencyPair(Currency.GBP, Currency.USD).isConventional   // true - configured
   * CurrencyPair(Currency.USD, Currency.GBP).isConventional   // false - inverse of configured
   * CurrencyPair(Currency.GBP, Currency.BRL).isConventional   // true - GBP has priority
   * CurrencyPair(Currency.BHD, Currency.BRL).isConventional   // true - neither listed, BHD < BRL
   * CurrencyPair(Currency.GBP, Currency.GBP).isConventional   // true - identity
   * }}}
   *
   * The first two steps read the configured table through the single primitive-valued route
   * [[CurrencyPair.configuredRateDigits]], so "configured" means here exactly what it means to
   * [[getRateDigits]], and neither step allocates: this predicate is called once per cell by code
   * that walks a matrix of currencies, and a predicate that allocates is one the JIT cannot lift
   * out of such a walk.
   *
   * @return true when this pair follows the market convention for its two currencies
   */
  def isConventional: Boolean =
    if (CurrencyPair.configuredRateDigits(base, counter) != CurrencyPair.NotConfigured) {
      true
    } else if (CurrencyPair.configuredRateDigits(counter, base) != CurrencyPair.NotConfigured) {
      false
    } else {
      val basePriority = CurrencyPair.marketConventionPriorityOf(base)
      val counterPriority = CurrencyPair.marketConventionPriorityOf(counter)
      if (basePriority < counterPriority) {
        true
      } else if (basePriority > counterPriority) {
        false
      } else {
        // Neither currency is listed in the priority ordering, so fall back to comparing the
        // codes. The comparison is that of the `Currency` ordering, which compares codes, and it
        // is not strict so that an identity pair is conventional.
        Currency.order.compare(base, counter) <= 0
      }
    }

  /**
   * Returns the market convention pair for the two currencies of this pair.
   *
   * This pair is returned when [[isConventional]] is true, and its [[inverse]] otherwise, so the
   * result is the same for a pair and for its inverse. Applying this twice changes nothing.
   *
   * @return the market convention pair for these two currencies
   */
  def toConventional: CurrencyPair = if (isConventional) this else inverse

  /**
   * Returns the set of currencies this pair holds, iterating in market convention order.
   *
   * The iteration order is part of what this method offers: a caller listing the currencies of a
   * pair gets the conventional base first and the conventional counter second, whichever way
   * round this pair happens to be written. The set is therefore an insertion-ordered `ListSet`
   * rather than a hashed set, so the order is a property of the returned value and not an
   * accident of how few elements it holds.
   *
   * An identity pair yields a set of one currency.
   *
   * @return the one or two currencies of this pair, in market convention order
   */
  def toSet: Set[Currency] =
    if (isConventional) ListSet(base, counter) else ListSet(counter, base)

  /**
   * Gets the number of decimal digits of a market quote for this pair.
   *
   * The answer is taken from the first of these that applies:
   *
   *  1. the digits the reference data holds for this pair;
   *  1. the digits it holds for the [[inverse]] pair, since a configured pair is held in its
   *     conventional direction only and a quote carries the same precision either way round;
   *  1. the sum of the minor unit digits of the two currencies, which is the estimate used for a
   *     pair the reference data holds in neither direction.
   *
   * {{{
   * CurrencyPair(Currency.GBP, Currency.USD).getRateDigits   // 4 - configured
   * CurrencyPair(Currency.USD, Currency.GBP).getRateDigits   // 4 - configured inverse
   * CurrencyPair(Currency.BHD, Currency.BRL).getRateDigits   // 5 - unconfigured, 3 + 2
   * }}}
   *
   * The three steps are tried in that order over the primitive answers of
   * [[CurrencyPair.configuredRateDigits]], the sentinel of which is what distinguishes a direction
   * the table does not hold from one it holds with zero digits - `USD/VND` is quoted to no
   * fractional digits at all, so a zero here is a configured answer and not an absent one.
   *
   * @return the number of digits in a market quote for this pair
   */
  def getRateDigits: Int = {
    val configured: Int = CurrencyPair.configuredRateDigits(base, counter)
    if (configured != CurrencyPair.NotConfigured) {
      configured
    } else {
      val configuredInverse: Int = CurrencyPair.configuredRateDigits(counter, base)
      if (configuredInverse != CurrencyPair.NotConfigured) {
        configuredInverse
      } else {
        base.minorUnitDigits + counter.minorUnitDigits
      }
    }
  }

  /**
   * Returns the text form of this pair, which is the two codes separated by a slash.
   *
   * This is the form [[CurrencyPair.parse]] reads, the form the `Show` instance renders and the
   * form the JSON codec writes.
   *
   * @return the pair as text, such as `EUR/USD`
   */
  override def toString: String = base.code + "/" + counter.code
}

/**
 * Holds the configured pairs, the routes from text and the typeclass instances of currency pairs.
 */
object CurrencyPair {

  /**
   * The text form of a pair: three upper case letters, a slash, and three more.
   *
   * The expression is applied to the whole of the text rather than to part of it, so trailing or
   * leading characters are a rejection rather than something to ignore.
   */
  private val PairFormat: Regex = """([A-Z]{3})/([A-Z]{3})""".r

  /**
   * The length of the text form of a pair, which the expression above fixes at seven.
   *
   * Three letters, a slash and three letters is seven characters and can be nothing else, so
   * this is the one thing about a candidate text that can be known before any work is done on
   * it. [[CurrencyPair.parse]] tests it first for that reason: text '''longer''' than this
   * cannot match however its case is folded, because folding a character never produces fewer
   * characters than it was given, so folding such a text would produce a value only to discard
   * it.
   *
   * The test is an upper bound rather than an equality for exactly that reason. Folding can
   * make a text '''longer''' - the German sharp s folds to two letters - so a text of six
   * characters can still fold to a pair of seven, and it is folded and matched as it always
   * was. Only the direction that cannot happen is ruled out in advance.
   *
   * The fold is what makes the test worth writing. A text of a million characters folds to a
   * text of a million characters, allocated and then thrown away by the very next comparison
   * (CWE-400/CWE-770); testing the length costs one field read and rejects the same text with
   * the same message.
   */
  private val PairTextLength: Int = 7

  /**
   * The answer [[configuredRateDigits]] gives for a direction the reference data does not hold.
   *
   * A number of rate digits is a count of decimal places and is therefore never negative - the
   * table holds values from zero to five - so a negative value cannot collide with a configured
   * one and can stand for "not configured" without an `Option` around the answer. Zero is a
   * configured answer and not an absent one: `USD/VND` is quoted to no fractional digits, which is
   * why the sentinel is below the range rather than at its edge. That is the whole reason the
   * sentinel is used: it keeps the lookup below a primitive `Int`, which is what makes the two
   * predicates that consult it allocate nothing at all.
   */
  private val NotConfigured: Int = -1

  /**
   * The empty inner table answered for a base currency the reference data configures no pair for.
   *
   * It is held rather than built at each miss for the reason any empty immutable collection is:
   * there is exactly one of it and building it again would only construct the same value. Being
   * empty, every lookup into it misses, which is the correct answer for such a base currency.
   */
  private val NoConfiguredCounters: HashMap[Currency, Int] = HashMap.empty

  /**
   * The rate digits the reference data holds for a pair of currencies, or [[NotConfigured]].
   *
   * This is the single route from this type to the configured table, used by both questions that
   * consult it: whether a pair is conventional, which is whether the table holds it, and how
   * many digits a quote for it carries. Keeping one route means the two answers cannot disagree
   * about what "configured" means, and it is why the sentinel is compared against in exactly two
   * members and nowhere else.
   *
   * The table is keyed by the two currencies rather than by a pair, which is what lets
   * [[CurrencyPairData]] describe pairs without depending on this type; the lookup is written
   * accordingly, descending the nested view that object holds for it - base currency first,
   * counter currency second - so that a question about one direction is two hash probes and no
   * allocation at all: each level answers with `getOrElse`, which hands back the value rather than
   * an `Option` of it, and the value was boxed once when the table was built. Asking
   * `CurrencyPairData.rateDigitsByCurrencies` instead would allocate the `Tuple2` of its composite
   * key and the `Some` of its answer on every call, which for a predicate called once per cell of
   * a conversion matrix is the difference between a lookup the JIT can hoist out of the loop and
   * one it cannot.
   *
   * @param pairBase     the base currency to look up
   * @param pairCounter  the counter currency to look up
   * @return the configured rate digits, or [[NotConfigured]] when that direction is not configured
   */
  private def configuredRateDigits(pairBase: Currency, pairCounter: Currency): Int =
    CurrencyPairData.rateDigitsByBase
      .getOrElse(pairBase, NoConfiguredCounters)
      .getOrElse(pairCounter, NotConfigured)

  /**
   * The market convention priority of a currency, where a lower number means a higher priority.
   *
   * A currency the ordering does not list ranks behind every currency it does, which is what the
   * largest possible value expresses. Only the relative order of two priorities is ever
   * observed, so the base the ordering counts from is immaterial.
   *
   * @param currency  the currency whose priority is wanted
   * @return the position of the currency in the ordering, or the largest value when unlisted
   */
  private def marketConventionPriorityOf(currency: Currency): Int = {
    // `getOrElse` would hand the fallback back as a boxed integer - the priority of a currency the
    // list does not name sits far outside the range of cached values - and this sits on the
    // predicate path, so the absent case is answered with a membership test and a second lookup
    // instead. Both return a primitive, so deciding the ordering of a pair allocates nothing
    // whether the currencies are named by the list or not.
    val priorities = CurrencyData.marketConventionPriorityIndex
    val code = currency.code
    if (priorities.contains(code)) priorities(code) else Unlisted
  }

  /**
   * The market convention priority of a currency the ordered list does not name.
   *
   * Every currency the list names has a priority below this, so a currency it does not name
   * sorts after every currency it does, which is what makes the comparison of two unnamed
   * currencies fall through to the lexicographic order of their codes.
   */
  private val Unlisted: Int = Int.MaxValue

  /**
   * The set of configured currency pairs.
   *
   * These are the 92 pairs the reference data describes, each in its market convention
   * direction, so every member of this set is a pair for which `isConventional` is true and no
   * member is the inverse of another. A pair that is not a member can still be built and used;
   * it is simply one the reference data says nothing about, and its convention and rate digits
   * are then derived by the rules documented on `CurrencyPair.isConventional` and
   * `CurrencyPair.getRateDigits`.
   *
   * The set is computed from the compiled data of [[CurrencyPairData]] while this companion
   * initialises, so it holds those 92 pairs in every program and cannot be empty.
   *
   * What this set offers is membership, answered in constant time, which is what asking whether
   * a pair is configured needs. It offers no iteration order: the order of the reference data is
   * a property of [[CurrencyPairData.rows]], and a caller that wants a reproducible sequence of
   * pairs should sort by the `Order` instance this companion publishes rather than rely on the
   * traversal order of a set.
   *
   * @return the 92 configured conventional pairs
   */
  val getAvailablePairs: Set[CurrencyPair] =
    CurrencyPairData.rows.iterator.map {
      case (rowBase, rowCounter, _) => CurrencyPair(rowBase, rowCounter)
    }.toSet

  /**
   * Obtains a pair from a base and a counter currency.
   *
   * The two currencies may be the same, giving the identity pair. This is total - there is
   * nothing about two currencies to reject - and is exactly what the constructor does, offered
   * under a factory name.
   *
   * @param base     the base currency
   * @param counter  the counter currency
   * @return the currency pair
   */
  def of(base: Currency, counter: Currency): CurrencyPair = CurrencyPair(base, counter)

  /**
   * Parses text of the form `AAA/BBB` into a pair.
   *
   * The text is folded to upper case in the English locale before it is matched, so parsing is
   * insensitive to the case of the input. The English locale is named explicitly so that the
   * fold is the same in every locale a program might run in. Both codes then have to name a
   * currency, and the failure of the first that does not is the failure returned - which is
   * where a code outside the closed set of [[Currency]] is rejected, rather than a currency
   * being invented for it.
   *
   * {{{
   * CurrencyPair.parse("EUR/USD")         // Right(EUR/USD)
   * CurrencyPair.parse("eur/usd")         // Right(EUR/USD) - case insensitive
   * CurrencyPair.parse("EURUSD")          // Left(Failure.Parsing("Invalid currency pair: EURUSD"))
   * CurrencyPair.parse("EUR/US")          // Left - the counter code is not three letters
   * CurrencyPair.parse("EUR/USD extra")   // Left - the whole text has to be the pair
   * CurrencyPair.parse("EUR/ZZZ")         // Left - no currency is named ZZZ
   * }}}
   *
   * The rejection names the whole of the text as it was supplied rather than as it was folded,
   * so a user sees back what they wrote.
   *
   * ===The length is tested before the text is folded===
   *
   * The expression fixes the length of a pair at [[PairTextLength]], and folding a text never
   * makes it shorter, so text longer than that cannot match whatever its case and is rejected
   * before it is folded rather than after. Nothing observable changes: the text that could not
   * have matched is refused with the message it was always refused with, and every text that
   * could still match - including a shorter one that folds into a pair, as the German sharp s
   * does - is folded and matched exactly as before. What changes is the work a rejection costs:
   * text arriving from outside this library is no longer copied in full before its shape is
   * looked at (CWE-400/CWE-770).
   *
   * @param pairStr  the pair as text, in the form `AAA/BBB`, in any case
   * @return the pair the text names, or the failure naming text that is not two three-letter
   *   codes separated by a slash, or that holds a code no currency of the closed [[Currency]]
   *   set has
   */
  def parse(pairStr: String): Either[Failure, CurrencyPair] =
    if (pairStr.length > PairTextLength) {
      Left(invalidPair(pairStr))
    } else {
      pairStr.toUpperCase(Locale.ENGLISH) match {
        case PairFormat(baseCode, counterCode) =>
          for {
            parsedBase <- Currency.parse(baseCode)
            parsedCounter <- Currency.parse(counterCode)
          } yield CurrencyPair(parsedBase, parsedCounter)
        case _ =>
          Left(invalidPair(pairStr))
      }
    }

  /**
   * The failure reported for text that does not have the shape of a pair.
   *
   * The two routes to it - a length the expression cannot match, and a length it could match but
   * a spelling it does not - are the same rejection to a caller and report the same message, so
   * the message is built here rather than at either of them. The text is quoted as it stands;
   * bounding it and escaping what it may hold belong to the writing of a failure, which
   * [[com.opengamma.strata.collect.result.Failure.show]] and the text form of a failure perform
   * for every part they write, so a message reaching a log is a bounded single line whatever
   * spelling arrived here.
   *
   * @param pairStr  the text that named no pair
   * @return the failure naming it
   */
  private def invalidPair(pairStr: String): Failure =
    Failure.Parsing(s"Invalid currency pair: $pairStr")

  /**
   * The ordering of currency pairs, which is also their equality and hashing.
   *
   * Pairs are ordered by base currency and then by counter currency, each by its code. An order
   * is provided because a deterministic sequence of pairs is what makes output - a matrix of
   * rates, a set of conventions, a failure message listing what was missing - reproducible.
   *
   * Comparing equal is the same thing as being equal, since the comparison reads both fields and
   * they are the only fields there are. Equality and hashing are those of the case class,
   * comparing the two currencies.
   *
   * This is the only equality-bearing instance this companion declares: `Order` and `Hash` both
   * extend `Eq`, so a separate `Eq` would be a second answer to the same question.
   *
   * @return the ordering of currency pairs
   */
  implicit val order: Order[CurrencyPair] with Hash[CurrencyPair] =
    new Order[CurrencyPair] with Hash[CurrencyPair] {

      private val universal: Hash[CurrencyPair] = Hash.fromUniversalHashCode[CurrencyPair]

      override def compare(x: CurrencyPair, y: CurrencyPair): Int = {
        val baseComparison = Currency.order.compare(x.base, y.base)
        if (baseComparison != 0) baseComparison else Currency.order.compare(x.counter, y.counter)
      }

      override def eqv(x: CurrencyPair, y: CurrencyPair): Boolean = universal.eqv(x, y)

      override def hash(x: CurrencyPair): Int = universal.hash(x)
    }

  /**
   * The rendering of currency pairs as text.
   *
   * Renders what `toString` renders, the `EUR/USD` form, so the text of a pair is the same
   * however it reaches a message.
   *
   * @return the rendering of a currency pair
   */
  implicit val show: Show[CurrencyPair] = Show.show(pair => pair.toString)

  /**
   * The JSON representation of currency pairs, which is their text form.
   *
   * A pair is the JSON string `"EUR/USD"` rather than an object of two codes: the text is the
   * identity of a pair and it is short enough to read in a document. Decoding applies [[parse]],
   * so a pair that is not well formed, or that holds a code outside the closed set of
   * [[Currency]], is a decoding failure carrying the reason rather than a silently accepted
   * value.
   *
   * The codec is assembled at compile time from the parsing and rendering functions above and
   * performs no reflection.
   *
   * @return the codec writing a pair as its text form
   */
  implicit val codec: Codec[CurrencyPair] =
    Codecs.parsedStringCodec(text => parse(text), pair => pair.toString)
}
