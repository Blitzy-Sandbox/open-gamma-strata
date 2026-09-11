/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.date

import scala.annotation.tailrec

import cats.Hash
import cats.Order
import cats.Show
import cats.data.NonEmptyList

import io.circe.Codec
import io.circe.KeyDecoder
import io.circe.KeyEncoder

import com.opengamma.strata.basics.RefDataReader
import com.opengamma.strata.basics.ReferenceData
import com.opengamma.strata.basics.ReferenceDataId
import com.opengamma.strata.basics.Resolvable
import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.CurrencyPair
import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.json.Codecs
import com.opengamma.strata.collect.result.Failure

/**
 * An identifier for a holiday calendar.
 *
 * An identifier of this type names a [[HolidayCalendar]] without holding one. The calendar
 * itself - which days are business days in London, in New York, or in both - is reference
 * data, so it is obtained from a [[ReferenceData]] that the caller supplies, and an
 * identifier is what a caller supplies it with. Identifiers for the calendars this library
 * knows about are constants of [[HolidayCalendarIds]].
 *
 * An identifier is an identity and nothing more: its name is its whole content, so two
 * identifiers of the same name are equal, hash alike and resolve to the same calendar. That
 * is the contract [[ReferenceDataId]] requires of an identifier, and here it comes from the
 * type being a case class over a single element.
 *
 * ===Composite identifiers===
 *
 * Two or more calendars can be named by one identifier, and the separator says how their
 * holidays are to be read together:
 *
 *   - `'+'` '''combines''': a day is a business day only where it is a business day in
 *     ''every'' part, so `GBLO+USNY` skips both the London and the New York holidays. This is
 *     the form a payment settled in two centres wants.
 *   - `'~'` '''links''': a day is a business day where it is a business day in ''any'' part,
 *     so `GBLO~USNY` skips only the days both centres are closed.
 *
 * The two compose, with `'+'` binding more tightly than `'~'`, so `GB~EU+Fri/Sat` links `GB`
 * with the combination of `EU` and `Fri/Sat`.
 *
 * A composite name is '''normalised''' when it is built, exactly as the identifier being
 * ported normalised it: the parts are deduplicated and sorted by name, so `USNY+GBLO` and
 * `GBLO+USNY` are one identifier, with one name, that encodes to one JSON string. The
 * no-holidays calendar is absorbed rather than carried, and the two separators differ in how,
 * because their meanings differ: combining with a calendar that has no holidays removes
 * nothing, so it drops out of a `'+'` composite, while linking with it makes every day a
 * business day, so it swallows a `'~'` composite whole.
 *
 * Normalising a composite down to a single part yields that part itself, so
 * `of("GBLO+NoHolidays")` ''is'' the simple `GBLO` identifier - the same value
 * `HolidayCalendarIds.GBLO` denotes - rather than a composite of one.
 *
 * ===Resolution===
 *
 * Resolution of a composite identifier asks for the whole name first. A host that holds a
 * pre-combined `GBLO+USNY` calendar - one merged from a vendor feed, say - therefore has it
 * used as it stands, and only where the whole name is unknown is each part resolved and the
 * parts combined. This is the behaviour of the original and the reason this type overrides
 * [[resolve]] rather than inheriting it. See [[resolve]] for the failure cases.
 *
 * ===Divergences from the type being ported===
 *
 * Resolution reports a missing calendar as `Left(Failure.MissingData(...))` where the
 * original threw `ReferenceDataNotFoundException`; that exception type is not ported, because
 * a calendar that reference data does not hold is a fact about the request rather than a
 * defect in the program, and the caller is the one with the context to decide what to do
 * about it.
 *
 * The accessor reporting the runtime type of the data the identifier refers to -
 * `getReferenceDataType` - is absent, along with the low-level query primitive that signalled
 * an absent value by returning a reference to nothing. Both are discussed on
 * [[ReferenceDataId]]: the type safety they supported at run time is supplied at compile time
 * by `ReferenceData.Entry`, and this port performs no reflection at all.
 *
 * The instance cache of the original is not ported either. It existed so that two identifiers
 * of the same name were the same object, which the original's resolution relied on because it
 * compared identifiers by reference in places; equality here is by name, so nothing needs
 * interning, and dropping the cache removes a process-wide mutable map from a value type.
 * Java serialization support goes with it.
 *
 * @param name  the identifier, expressed as a normalised unique name, such as `GBLO` or
 *   `GBLO+USNY`
 * @see [[HolidayCalendarIds]] for the identifiers of the calendars this library knows about
 * @see [[HolidayCalendar]] for the calendar an identifier resolves to
 */
sealed abstract case class HolidayCalendarId private (name: String)
    extends ReferenceDataId[HolidayCalendar]
    with Resolvable[HolidayCalendar]
    with Named {

  /**
   * The parts of this identifier and how they are read together, or empty where it is simple.
   *
   * The structure is worked out once, when the identifier is built, and carried on the
   * instance rather than recovered from the name at each resolution - which is what the
   * identifier being ported did, for the same reason: a composite identifier is resolved once
   * per date adjustment, and re-splitting its name each time would allocate on that path.
   *
   * It is deliberately not part of the value. Equality, hashing and the JSON form of an
   * identifier are its name alone, and the structure is a function of that name, so carrying
   * it here cannot make two identifiers of the same name behave differently.
   *
   * @return the parts of a composite identifier, or empty where this identifier is simple
   */
  private[date] def composite: Option[HolidayCalendarId.Composite]

  /**
   * Checks whether this identifier names more than one calendar.
   *
   * A composite identifier is one built with `'+'` or `'~'`, which its normalised name still
   * carries; a simple identifier names a single calendar. The test is the one the original
   * applied - the name contains a separator - and it agrees with the structure carried by
   * [[composite]] by construction, since a composite name is built by joining two or more
   * parts and a name that survives normalisation with one part is that part's own.
   *
   * @return true if this identifier combines or links two or more calendars
   */
  def isComposite: Boolean =
    name.contains(HolidayCalendarId.CombineSeparator) || name.contains(HolidayCalendarId.LinkSeparator)

  /**
   * Resolves this identifier to a holiday calendar using the specified reference data.
   *
   * The whole name is looked up first, so reference data that holds a calendar under this
   * exact identifier - including a composite one, pre-combined by whoever supplied it -
   * answers with it. Where the whole name is unknown and this identifier is composite, each
   * part is resolved in turn and the results are combined or linked in the order the
   * normalised name gives, which is what makes `GBLO+USNY` resolvable from nothing but the
   * `GBLO` and `USNY` entries.
   *
   * Two failures are possible, and both are `Failure.MissingData`:
   *
   *   - a simple identifier the reference data does not hold, reported as `ReferenceData`
   *     itself reports it;
   *   - a part of a composite identifier that cannot be resolved, reported with a message
   *     naming both that part and this identifier, so a caller is told which of several
   *     calendars was missing and what was being built from it.
   *
   * The first unresolvable part ends the resolution, as it did in the original; a composite
   * identifier is all-or-nothing, since a calendar assembled from some of its parts would
   * silently declare business days that are holidays.
   *
   * The resolved calendar is bound to the data it was resolved against and does not follow
   * later changes to that data, so care is needed when placing it in a cache or a persistence
   * layer.
   *
   * @param refData  the reference data to resolve this identifier against
   * @return the resolved holiday calendar, or the failure explaining why it could not be
   *   resolved
   */
  override def resolve(refData: ReferenceData): Either[Failure, HolidayCalendar] =
    refData.findValue(this) match {
      case Some(calendar) => Right(calendar)
      case None =>
        composite match {
          // A simple identifier that is absent is exactly the failure `getValue` reports, so
          // the message and its attribute are taken from there rather than written again
          // here. The repeated lookup this costs happens only on the failing path.
          case None => refData.getValue(this)
          case Some(parts) => resolveParts(parts, refData)
        }
    }

  /**
   * Expresses resolution of this identifier as a function awaiting reference data.
   *
   * The override exists only to settle an inherited ambiguity: this type is both a
   * [[ReferenceDataId]] and a [[Resolvable]], and each of those supplies a `toReader` of its
   * own, so Scala requires the implementation to be named here. It is the one from
   * `ReferenceDataId`, which builds the reader from [[resolve]] and therefore picks up the
   * composite resolution above; the two inherited definitions are identical in any case.
   *
   * @return the resolution of this identifier as a function from reference data to the
   *   calendar
   */
  override def toReader: RefDataReader[HolidayCalendar] = super[ReferenceDataId].toReader

  /**
   * Combines this identifier with another, naming a calendar that observes both sets of
   * holidays.
   *
   * A day is a business day under the combined identifier only where it is a business day
   * under both, which is what a cash flow settled in two centres requires. Combining an
   * identifier with itself, or with the no-holidays identifier, changes nothing and returns
   * an existing value rather than building a composite; otherwise the two names are joined
   * and normalised by [[HolidayCalendarId.of]], so the result does not depend on which side
   * each identifier was passed as.
   *
   * @param other  the other holiday calendar identifier
   * @return the combined holiday calendar identifier
   */
  def combinedWith(other: HolidayCalendarId): HolidayCalendarId =
    if (this == other || other == HolidayCalendarId.NoHolidaysId) {
      this
    } else if (this == HolidayCalendarId.NoHolidaysId) {
      other
    } else {
      HolidayCalendarId.of(name + HolidayCalendarId.CombineSeparator + other.name)
    }

  /**
   * Links this identifier with another, naming a calendar that observes only their shared
   * holidays.
   *
   * A day is a business day under the linked identifier where it is a business day under
   * either, so only the days on which every part is closed remain holidays. Linking an
   * identifier with itself changes nothing, and linking anything with the no-holidays
   * identifier yields the no-holidays identifier, because a calendar in which every day is a
   * business day leaves no day for the others to close.
   *
   * @param other  the other holiday calendar identifier
   * @return the linked holiday calendar identifier
   */
  def linkedWith(other: HolidayCalendarId): HolidayCalendarId =
    if (this == other) {
      this
    } else if (this == HolidayCalendarId.NoHolidaysId || other == HolidayCalendarId.NoHolidaysId) {
      HolidayCalendarId.NoHolidaysId
    } else {
      HolidayCalendarId.of(name + HolidayCalendarId.LinkSeparator + other.name)
    }

  /**
   * Returns the name that uniquely identifies this calendar.
   *
   * This is [[name]] - the text the identifier is known by, which
   * [[HolidayCalendarId.of]] reads back - rather than the structural rendering a case class
   * would otherwise produce. It is also the text that appears in a failure message and in
   * the JSON form.
   *
   * @return the unique name
   */
  override def toString: String = name

  /**
   * Resolves the parts of a composite identifier and reads them together.
   *
   * Each part is resolved in the order the normalised name gives, and the results are folded
   * from the left with the operation the separator chose, so the fold follows the name.
   * `traverse` stops at the first part that cannot be resolved, which is the all-or-nothing
   * behaviour described on [[resolve]].
   *
   * @param parts  the parts of this composite identifier and how to read them together
   * @param refData  the reference data to resolve the parts against
   * @return the assembled holiday calendar, or the failure from the first unresolvable part
   */
  private def resolveParts(
      parts: HolidayCalendarId.Composite,
      refData: ReferenceData): Either[Failure, HolidayCalendar] =

    parts.components
      .traverse(component => resolvePart(component, refData))
      .map(calendars => calendars.reduceLeft(parts.combine))

  /**
   * Resolves a single part of this composite identifier.
   *
   * A part that is itself composite - as `EU+Fri/Sat` is within `EU+Fri/Sat~GB` - is resolved
   * by asking it, so that its own whole name is tried before its parts are, and it reports
   * its own missing part if one is missing. A simple part is one lookup, and its absence is
   * reported against this identifier, which is the context the caller needs.
   *
   * @param component  the part to resolve
   * @param refData  the reference data to resolve the part against
   * @return the calendar of that part, or the failure explaining why it is not available
   */
  private def resolvePart(
      component: HolidayCalendarId,
      refData: ReferenceData): Either[Failure, HolidayCalendar] =

    if (component.isComposite) {
      component.resolve(refData)
    } else {
      refData.findValue(component).toRight(partNotFound(component))
    }

  /**
   * Returns the failure reported when a part of this composite identifier is not available.
   *
   * The message is the one the original produced, naming the part that was missing and the
   * identifier being resolved, and both are carried as attributes so that a caller can act on
   * them without reading the message.
   *
   * @param component  the part that could not be resolved
   * @return the failure describing the missing part
   */
  private def partNotFound(component: HolidayCalendarId): Failure =
    Failure
      .MissingData(
        s"Reference data not found for '${component.name}' of type 'HolidayCalendarId' " +
          s"when finding '$name'")
      .withAttribute("id", component.name)
      .withAttribute("compositeId", name)
}

/**
 * Provides the ways of obtaining a holiday calendar identifier, the default calendar of a
 * currency, and the instances of the type.
 *
 * ===Construction===
 *
 * [[of]] is the only way to obtain an identifier, and it is '''total''': every string names an
 * identifier, because the name of a calendar is not this library's to judge - an application
 * is free to hold its calendars under names of its own, and a name this library has never seen
 * is not an error until it fails to resolve. That is the behaviour of the type being ported,
 * and it is why this factory returns an identifier rather than a result. What `of` does do is
 * '''normalise''': the parts of a composite name are deduplicated and sorted, and the
 * no-holidays calendar is absorbed, so that a name and its rearrangements are one value. The
 * rules are described on [[HolidayCalendarId]].
 *
 * Neither `apply` nor `copy` exists, which is what keeps that normalisation inescapable: the
 * constructor of a `sealed abstract case class` is reachable only from inside this file, and
 * the two private factories below are the only places that reach it. Pattern matching still
 * works, so `case HolidayCalendarId(name) =>` reads the name of an identifier.
 *
 * ===The default calendar of a currency===
 *
 * [[defaultByCurrency]] answers the calendar conventionally used for a currency, from the
 * table transcribed below. The original held that table in a configuration resource loaded
 * from the class path and offered the lookup twice - once throwing, once returning an optional
 * value; this port holds the table as code and offers the lookup once, returning an `Option`,
 * because a currency that has no conventional calendar is an ordinary answer rather than a
 * failure of the program.
 *
 * ===Instances===
 *
 * The companion declares one equality-bearing instance, one rendering, one codec and the two
 * halves of a key codec, which is the convention of this port: `Order` and `Hash` both extend
 * `Eq`, so declaring them as a single value makes it impossible for equality and ordering to
 * disagree, and there is deliberately no separate `Eq`.
 */
object HolidayCalendarId {

  /**
   * The separator of a combined identifier, whose parts are all observed.
   *
   * Declared as a one-character string rather than a character so that every use - the
   * membership tests, the splitting and the joining - goes through the string methods of the
   * standard library. A character would have to widen to an integer to reach
   * `String.indexOf`, and this build rejects an implicit numeric widening.
   */
  private[date] val CombineSeparator: String = "+"

  /**
   * The separator of a linked identifier, whose parts are observed in the alternative.
   *
   * Declared as a one-character string for the reason given on [[CombineSeparator]].
   */
  private[date] val LinkSeparator: String = "~"

  /** The name of the identifier of the calendar that declares no holidays at all. */
  private val NoHolidaysName: String = "NoHolidays"

  /**
   * The identifier of the calendar that declares no holidays at all.
   *
   * This is the value `HolidayCalendarIds.NO_HOLIDAYS` publishes, and it is held here rather
   * than there because [[of]], `combinedWith` and `linkedWith` all need it: an identifier
   * built by `of` must not depend on the initialisation of the constant holder, which is
   * itself built by calling `of` twenty-nine times. Keeping the one value the normalisation
   * needs on this side of that relationship is what keeps it acyclic.
   */
  private[date] val NoHolidaysId: HolidayCalendarId = simple(NoHolidaysName)

  /**
   * The parts of a composite identifier together with the operation that reads them together.
   *
   * A carrier for what [[of]] worked out while parsing a name, held on the instance so that
   * resolution does not parse it again. It is not a value: the operation is a function, so two
   * of these are never usefully compared, and nothing compares them - an identifier's equality
   * is its name.
   *
   * The parts are a `NonEmptyList` because a composite identifier always has at least two, and
   * the operation is applied by folding them; typing them this way is what lets the fold be
   * written without a case for an empty list that could not occur.
   *
   * @param components  the parts of the identifier, in the order its normalised name gives
   * @param combine  combines two resolved calendars the way this identifier's separator
   *   requires
   */
  private[date] final class Composite(
      val components: NonEmptyList[HolidayCalendarId],
      val combine: (HolidayCalendar, HolidayCalendar) => HolidayCalendar)

  //-------------------------------------------------------------------------
  /**
   * Obtains an identifier from the specified unique name.
   *
   * The name identifies the calendar, which is resolved from reference data when it is needed.
   * Any name is accepted, including one this library knows nothing about; see the discussion
   * of totality above.
   *
   * Two or more calendars are named by joining their names with `'+'`, to observe all of their
   * holidays, or with `'~'`, to observe only their shared ones. Such a name is normalised: the
   * parts are deduplicated and sorted by name, and the no-holidays calendar drops out of a
   * combination and swallows a link. `of("USNY+GBLO")` and `of("GBLO+USNY")` are therefore the
   * same identifier, named `GBLO+USNY`, and a name that normalises to a single part is that
   * part.
   *
   * {{{
   * HolidayCalendarId.of("GBLO")                // a simple identifier
   * HolidayCalendarId.of("USNY+GBLO").name      // "GBLO+USNY"
   * HolidayCalendarId.of("GBLO+NoHolidays")     // the GBLO identifier itself
   * HolidayCalendarId.of("GBLO~NoHolidays")     // the no-holidays identifier
   * }}}
   *
   * @param uniqueName  the unique name
   * @return the identifier
   */
  def of(uniqueName: String): HolidayCalendarId =
    // The separators are tested in this order because `'+'` binds more tightly than `'~'`, so
    // a name carrying both is a link of combinations. This is the order of the original.
    if (uniqueName.contains(LinkSeparator)) {
      val parts = normalise(splitOn(uniqueName, LinkSeparator))
      // linking a calendar that has no holidays makes every day a business day
      if (parts.contains(NoHolidaysId)) {
        NoHolidaysId
      } else {
        compose(parts, LinkSeparator, (first, second) => first.linkedWith(second))
      }
    } else if (uniqueName.contains(CombineSeparator)) {
      // combining a calendar that has no holidays removes nothing, so it is dropped; the
      // original drops it by name, before the parts become identifiers, and so does this
      val named = splitOn(uniqueName, CombineSeparator).filterNot(part => part == NoHolidaysName)
      compose(normalise(named), CombineSeparator, (first, second) => first.combinedWith(second))
    } else {
      simple(uniqueName)
    }

  /**
   * Checks whether an identifier names more than one calendar.
   *
   * This is the name the original gave the test, kept so that call sites read unchanged; it is
   * [[HolidayCalendarId.isComposite]] on the identifier itself.
   *
   * @param id  the holiday calendar identifier
   * @return true if the identifier combines or links two or more calendars
   */
  def isCompositeCalendar(id: HolidayCalendarId): Boolean = id.isComposite

  //-------------------------------------------------------------------------
  /**
   * Finds the calendar conventionally used for a currency.
   *
   * The answer is the market convention - the calendar of the centre a payment in that
   * currency settles in - and it is a convention rather than a rule, which is why an
   * application is free to disregard it. Thirteen of the identifiers this table can produce
   * name calendars whose holidays this library does not ship; those identifiers are returned
   * all the same, exactly as the original returned them, and they fail to resolve against
   * `ReferenceData.standard` just as they did there.
   *
   * The original offered this lookup twice, throwing where no convention was recorded and
   * returning an optional value from a second method of a different name. The two are merged
   * here into one total lookup: a currency without a conventional calendar is an ordinary
   * answer of `None`.
   *
   * {{{
   * HolidayCalendarId.defaultByCurrency(Currency.GBP)   // Some(GBLO)
   * HolidayCalendarId.defaultByCurrency(Currency.XAG)   // None
   * }}}
   *
   * @param currency  the currency to find the default for
   * @return the identifier of the calendar conventionally used for the currency, empty where
   *   no convention is recorded for it
   */
  def defaultByCurrency(currency: Currency): Option[HolidayCalendarId] = defaultsByCurrency.get(currency)

  /**
   * Finds the calendar conventionally used for a pair of currencies.
   *
   * The conventional calendars of the two currencies are combined, so that a day is a business
   * day for the pair only where it is a business day for both. A currency for which no
   * convention is recorded contributes nothing, and where neither currency has one the result
   * is the no-holidays identifier - the identity of combination - so this lookup, unlike
   * [[defaultByCurrency]], always answers with an identifier. Both behaviours are those of the
   * original, which folded the same combination over the same starting value.
   *
   * @param currencyPair  the currency pair to find the defaults for
   * @return the identifier of the calendar conventionally used for the pair
   */
  def defaultByCurrencyPair(currencyPair: CurrencyPair): HolidayCalendarId =
    currencyPair.toSet.toList
      .flatMap(currency => defaultByCurrency(currency).toList)
      .foldLeft(NoHolidaysId)((combined, calendarId) => combined.combinedWith(calendarId))

  /**
   * The calendar conventionally used for each currency, as code rather than as configuration.
   *
   * These are the thirty-one rows the original loaded from its holiday-calendar default data,
   * in the order that source lists them, and the reference data manifest of this port checks
   * them row for row against the values captured from it. Reading them from the class path is
   * not ported: the table is part of the behaviour of this library rather than something an
   * application is invited to replace, and holding it here means a lookup cannot depend on
   * what happens to be on the class path.
   *
   * The eighteen identifiers that also exist as constants of [[HolidayCalendarIds]] are
   * written as those constants, so that a mistyped code is a compile error rather than a row
   * that quietly disappears. The remaining thirteen name calendars this library does not ship
   * and so have no constant; they are built from their names and are listed together below.
   *
   * Computed once, on first use, so that neither this object nor `Currency` forces the other
   * while it is being initialised.
   */
  private lazy val defaultsByCurrency: Map[Currency, HolidayCalendarId] =
    Map(
      Currency.CHF -> HolidayCalendarIds.CHZU,
      Currency.EUR -> HolidayCalendarIds.EUTA,
      Currency.GBP -> HolidayCalendarIds.GBLO,
      Currency.JPY -> HolidayCalendarIds.JPTO,
      Currency.USD -> HolidayCalendarIds.USNY,
      Currency.AUD -> HolidayCalendarIds.AUSY,
      Currency.BRL -> HolidayCalendarIds.BRBD,
      Currency.CAD -> HolidayCalendarIds.CATO,
      Currency.CZK -> HolidayCalendarIds.CZPR,
      Currency.DKK -> HolidayCalendarIds.DKCO,
      Currency.HUF -> HolidayCalendarIds.HUBU,
      Currency.MXN -> HolidayCalendarIds.MXMC,
      Currency.NOK -> HolidayCalendarIds.NOOS,
      Currency.NZD -> HolidayCalendarIds.NZAU,
      Currency.PLN -> HolidayCalendarIds.PLWA,
      Currency.SEK -> HolidayCalendarIds.SEST,
      Currency.ZAR -> HolidayCalendarIds.ZAJO,
      // the calendars named below are not shipped with this library, in the port as in the
      // original; these identifiers resolve only against reference data that supplies them
      Currency.CLP -> of("CLSA"),
      Currency.CNY -> of("CNBE"),
      Currency.COP -> of("COBO"),
      Currency.HKD -> of("HKHK"),
      Currency.IDR -> of("IDJA"),
      Currency.ILS -> of("ILTA"),
      Currency.INR -> of("INMU"),
      Currency.KRW -> of("KRSE"),
      Currency.RUB -> of("RUMO"),
      Currency.SAR -> of("SARI"),
      Currency.SGD -> of("SGSI"),
      Currency.THB -> HolidayCalendarIds.THBA,
      Currency.TRY -> of("TRIS"),
      Currency.TWD -> of("TWTA"))

  //-------------------------------------------------------------------------
  /**
   * Builds an identifier that names a single calendar.
   *
   * One of the two places in this file that reach the constructor of the type. The name is
   * taken as it stands, which is correct because a name containing neither separator is
   * already normalised.
   *
   * @param uniqueName  the name of the calendar
   * @return the simple identifier of that name
   */
  private def simple(uniqueName: String): HolidayCalendarId =
    new HolidayCalendarId(uniqueName) {
      private[date] val composite: Option[Composite] = None
    }

  /**
   * Builds an identifier from the normalised parts of a composite name.
   *
   * The three cases are the three outcomes normalisation can have, and each is the value the
   * original produced for it:
   *
   *   - '''no parts''' - every part named the no-holidays calendar - is that calendar's
   *     identifier, which is what combining or linking it with itself yields;
   *   - '''one part''' is that part itself, so a name that normalises to `GBLO` is the simple
   *     `GBLO` identifier and reports itself as not composite;
   *   - '''two or more parts''' is a composite identifier, named by joining the parts with the
   *     separator and carrying them for resolution.
   *
   * @param parts  the normalised parts, deduplicated and sorted by name
   * @param separator  the separator to join the parts with
   * @param combine  combines two resolved calendars the way the separator requires
   * @return the identifier of the composite
   */
  private def compose(
      parts: List[HolidayCalendarId],
      separator: String,
      combine: (HolidayCalendar, HolidayCalendar) => HolidayCalendar): HolidayCalendarId =

    parts match {
      case Nil => NoHolidaysId
      case single :: Nil => single
      case first :: second :: rest =>
        val components = NonEmptyList(first, second :: rest)
        val structure = new Composite(components, combine)
        new HolidayCalendarId(components.toList.map(part => part.name).mkString(separator)) {
          private[date] val composite: Option[Composite] = Some(structure)
        }
    }

  /**
   * Turns the parts of a composite name into normalised identifiers.
   *
   * Each part becomes an identifier in its own right - which is what makes a part of a linked
   * name able to be a combination - and the parts are then deduplicated and ordered by name.
   * Sorting is what makes the name of a composite independent of the order it was written in,
   * and deduplicating is what makes `GBLO+GBLO` the simple `GBLO`. Both steps, and their
   * order, are those of the original.
   *
   * @param parts  the names of the parts, in the order they were written
   * @return the identifiers of the parts, deduplicated and sorted by name
   */
  private def normalise(parts: List[String]): List[HolidayCalendarId] =
    parts.map(part => of(part)).distinct.sortBy(part => part.name)

  /**
   * Splits a name around every occurrence of a separator.
   *
   * Written out rather than delegated to a library split for two reasons: the standard
   * library's string split is driven by a regular expression, in which both separators of this
   * type are metacharacters, and an empty part is kept rather than discarded, which is what
   * the splitter used by the original did. The loop carries its position and its result as
   * parameters, so it needs no mutable state and compiles to a jump.
   *
   * @param text  the text to split
   * @param separator  the separator to split around
   * @return the parts, in the order they appear, including empty ones
   */
  private def splitOn(text: String, separator: String): List[String] = {
    @tailrec
    def loop(from: Int, parts: List[String]): List[String] = {
      val index = text.indexOf(separator, from)
      if (index < 0) {
        (text.substring(from) :: parts).reverse
      } else {
        loop(index + separator.length, text.substring(from, index) :: parts)
      }
    }
    loop(0, Nil)
  }

  //-------------------------------------------------------------------------
  /**
   * The ordering of holiday calendar identifiers, which is also their hashing and equality.
   *
   * This is the only equality-bearing instance of the type. `Order` and `Hash` both extend
   * `Eq`, so declaring them together is what makes it impossible for the ordering, the hashing
   * and the equality of an identifier to disagree, and it is why no separate `Eq` is declared.
   *
   * All three are the name. Equality and hashing are those of the value itself, which is the
   * name alone since that is the only element the case class carries - the equality of the
   * original, which compared names and hashed the name. Ordering is the ordering of the names,
   * which agrees with that equality exactly: two identifiers compare equal precisely when
   * their names are equal, and that is precisely when they are equal. The effect is that a
   * sorted collection of identifiers reads alphabetically, and that the parts of a composite
   * name are in the order this instance would put them in.
   *
   * @return the ordering, hashing and equality of holiday calendar identifiers
   */
  implicit val order: Order[HolidayCalendarId] with Hash[HolidayCalendarId] =
    new Order[HolidayCalendarId] with Hash[HolidayCalendarId] {

      private val universal: Hash[HolidayCalendarId] = Hash.fromUniversalHashCode[HolidayCalendarId]

      override def compare(x: HolidayCalendarId, y: HolidayCalendarId): Int =
        x.name.compareTo(y.name)

      override def eqv(x: HolidayCalendarId, y: HolidayCalendarId): Boolean = universal.eqv(x, y)

      override def hash(x: HolidayCalendarId): Int = universal.hash(x)
    }

  /**
   * The text rendering of a holiday calendar identifier, which is its name.
   *
   * This is the text the original produced through its own string conversion, so a rendered
   * identifier is the text [[of]] reads back.
   *
   * @return the rendering of holiday calendar identifiers
   */
  implicit val show: Show[HolidayCalendarId] = Show.show(calendarId => calendarId.name)

  /**
   * The JSON codec for holiday calendar identifiers.
   *
   * An identifier is written as the bare string of its name, so a document holds
   * `"GBLO+USNY"` rather than an object, and the text is identical to the one the library
   * being ported wrote. Reading goes through [[of]], which accepts any name, so a document
   * naming a calendar this library does not know about is read - and fails, if at all, when it
   * is resolved, which is where a missing calendar belongs.
   *
   * Because a name is normalised when it is read and the encoded form is the name, two
   * identifiers built from differently ordered composites - `USNY+GBLO` and `GBLO+USNY` -
   * encode to identical bytes, which is the stability the round-trip tests of this port
   * require.
   *
   * @return the codec for holiday calendar identifiers
   */
  implicit val codec: Codec[HolidayCalendarId] =
    Codecs.parsedStringCodec[HolidayCalendarId](name => Right(of(name)), calendarId => calendarId.name)

  /**
   * The key codecs for holiday calendar identifiers, for use as the key of a JSON object.
   *
   * A JSON object key is always text, so an identifier keys an object by its name, and the
   * name is read back through [[of]]. Reference data is a map keyed by identifier and a
   * holiday calendar is the one kind of reference data this library serializes, so this is the
   * pair that lets a set of calendars be written as an object keyed by the identifier each is
   * held under.
   *
   * Declared as one private pair and published as two implicits, because the helper produces
   * both halves together.
   */
  private val keyCodecs: (KeyEncoder[HolidayCalendarId], KeyDecoder[HolidayCalendarId]) =
    Codecs.namedKeyCodecs[HolidayCalendarId](name => Some(of(name)))

  /**
   * The JSON key encoder for holiday calendar identifiers, writing the name.
   *
   * @return the key encoder
   */
  implicit val keyEncoder: KeyEncoder[HolidayCalendarId] = keyCodecs._1

  /**
   * The JSON key decoder for holiday calendar identifiers, reading the name through [[of]].
   *
   * @return the key decoder
   */
  implicit val keyDecoder: KeyDecoder[HolidayCalendarId] = keyCodecs._2
}


/**
 * Identifiers for the holiday calendars in common use.
 *
 * The constants here are identifiers, not calendars: each one locates a [[HolidayCalendar]] in
 * the [[ReferenceData]] a caller supplies, and every one of them is available from
 * `ReferenceData.standard`. The holiday data behind them was obtained by direct research
 * rather than from a vendor of calendar data, so it may or may not be sufficient for
 * production use; an application that needs its own holidays supplies its own `ReferenceData`,
 * mapping these same identifiers - or identifiers of its own - to whatever calendars it
 * trusts. The second route the original offered, amending the standard data by editing a
 * configuration resource on the class path, is not ported: the built-in calendars are code
 * here, so there is no resource to amend.
 *
 * The names carried by these constants are those of the library being ported -
 * `HolidayCalendarIds.GBLO`, `HolidayCalendarIds.SAT_SUN` - so that call sites, stored data
 * and documentation read the same way after the migration. So is the membership of the set:
 * these are the twenty-nine identifiers the original published, and no identifier has been
 * added. In particular there is deliberately no constant for the Wellington anniversary
 * calendar `NZBD`, which the original generated but never named here; `HolidayCalendarId.of`
 * builds that identifier, and any other, for a caller that wants it.
 *
 * @see [[HolidayCalendarId]] for what an identifier is and how composites of them are named
 */
object HolidayCalendarIds {

  /**
   * An identifier for a calendar declaring no holidays and no weekends, with code
   * 'NoHolidays'.
   *
   * This calendar has the effect of making every day a business day. It is often used to
   * indicate that a holiday calendar does not apply, and it is the identity of
   * `HolidayCalendarId.combinedWith`.
   */
  val NO_HOLIDAYS: HolidayCalendarId = HolidayCalendarId.NoHolidaysId

  /**
   * An identifier for a calendar declaring all days as business days except Saturday/Sunday
   * weekends, with code 'Sat/Sun'.
   *
   * This calendar is mostly useful in testing scenarios. Note that not all countries use
   * Saturday and Sunday weekends.
   */
  val SAT_SUN: HolidayCalendarId = HolidayCalendarId.of("Sat/Sun")

  /**
   * An identifier for a calendar declaring all days as business days except Friday/Saturday
   * weekends, with code 'Fri/Sat'.
   *
   * This calendar is mostly useful in testing scenarios.
   */
  val FRI_SAT: HolidayCalendarId = HolidayCalendarId.of("Fri/Sat")

  /**
   * An identifier for a calendar declaring all days as business days except Thursday/Friday
   * weekends, with code 'Thu/Fri'.
   *
   * This calendar is mostly useful in testing scenarios.
   */
  val THU_FRI: HolidayCalendarId = HolidayCalendarId.of("Thu/Fri")

  /**
   * An identifier for the holiday calendar of London, United Kingdom, with code 'GBLO'.
   *
   * This constant references the calendar for London bank holidays.
   */
  val GBLO: HolidayCalendarId = HolidayCalendarId.of("GBLO")

  /**
   * An identifier for the holiday calendar of Paris, France, with code 'FRPA'.
   *
   * This constant references the calendar for Paris public holidays.
   */
  val FRPA: HolidayCalendarId = HolidayCalendarId.of("FRPA")

  /**
   * An identifier for the holiday calendar of Frankfurt, Germany, with code 'DEFR'.
   *
   * This constant references the calendar for Frankfurt public holidays.
   */
  val DEFR: HolidayCalendarId = HolidayCalendarId.of("DEFR")

  /**
   * An identifier for the holiday calendar of Zurich, Switzerland, with code 'CHZU'.
   *
   * This constant references the calendar for Zurich public holidays.
   */
  val CHZU: HolidayCalendarId = HolidayCalendarId.of("CHZU")

  /**
   * An identifier for the holiday calendar of the European Union TARGET system, with code
   * 'EUTA'.
   *
   * This constant references the calendar for the TARGET interbank payment system holidays.
   *
   * Referenced by the 2006 ISDA definitions 1.8.
   */
  val EUTA: HolidayCalendarId = HolidayCalendarId.of("EUTA")

  /**
   * An identifier for the holiday calendar of United States Government Securities, with code
   * 'USGS'.
   *
   * This constant references the calendar for United States Government Securities as per
   * SIFMA.
   *
   * Referenced by the 2006 ISDA definitions 1.11.
   */
  val USGS: HolidayCalendarId = HolidayCalendarId.of("USGS")

  /**
   * An identifier for the holiday calendar of New York, United States, with code 'USNY'.
   *
   * This constant references the calendar for New York holidays.
   */
  val USNY: HolidayCalendarId = HolidayCalendarId.of("USNY")

  /**
   * An identifier for the holiday calendar of the Federal Reserve Bank of New York, with code
   * 'NYFD'.
   *
   * This constant references the calendar for the Federal Reserve Bank of New York holidays.
   *
   * Referenced by the 2006 ISDA definitions 1.9.
   */
  val NYFD: HolidayCalendarId = HolidayCalendarId.of("NYFD")

  /**
   * An identifier for the holiday calendar of the New York Stock Exchange, with code 'NYSE'.
   *
   * This constant references the calendar for the New York Stock Exchange.
   *
   * Referenced by the 2006 ISDA definitions 1.10.
   */
  val NYSE: HolidayCalendarId = HolidayCalendarId.of("NYSE")

  /**
   * An identifier for the holiday calendar of Tokyo, Japan, with code 'JPTO'.
   *
   * This constant references the calendar for Tokyo bank holidays.
   */
  val JPTO: HolidayCalendarId = HolidayCalendarId.of("JPTO")

  /**
   * An identifier for the holiday calendar of Sydney, Australia, with code 'AUSY'.
   *
   * This constant references the calendar for Sydney bank holidays.
   */
  val AUSY: HolidayCalendarId = HolidayCalendarId.of("AUSY")

  /**
   * An identifier for the holiday calendar of Brazil, with code 'BRBD'.
   *
   * This constant references the combined calendar for Brazil bank holidays. This unites
   * city-level calendars.
   */
  val BRBD: HolidayCalendarId = HolidayCalendarId.of("BRBD")

  /**
   * An identifier for the holiday calendar of Montreal, Canada, with code 'CAMO'.
   *
   * This constant references the calendar for Montreal bank holidays.
   */
  val CAMO: HolidayCalendarId = HolidayCalendarId.of("CAMO")

  /**
   * An identifier for the holiday calendar of Toronto, Canada, with code 'CATO'.
   *
   * This constant references the calendar for Toronto bank holidays.
   */
  val CATO: HolidayCalendarId = HolidayCalendarId.of("CATO")

  /**
   * An identifier for the holiday calendar of Prague, Czech Republic, with code 'CZPR'.
   *
   * This constant references the calendar for Prague bank holidays.
   */
  val CZPR: HolidayCalendarId = HolidayCalendarId.of("CZPR")

  /**
   * An identifier for the holiday calendar of Copenhagen, Denmark, with code 'DKCO'.
   *
   * This constant references the calendar for Copenhagen bank holidays.
   */
  val DKCO: HolidayCalendarId = HolidayCalendarId.of("DKCO")

  /**
   * An identifier for the holiday calendar of Budapest, Hungary, with code 'HUBU'.
   *
   * This constant references the calendar for Budapest bank holidays.
   */
  val HUBU: HolidayCalendarId = HolidayCalendarId.of("HUBU")

  /**
   * An identifier for the holiday calendar of Mexico City, Mexico, with code 'MXMC'.
   *
   * This constant references the calendar for Mexico City bank holidays.
   */
  val MXMC: HolidayCalendarId = HolidayCalendarId.of("MXMC")

  /**
   * An identifier for the holiday calendar of Oslo, Norway, with code 'NOOS'.
   *
   * This constant references the calendar for Oslo bank holidays.
   */
  val NOOS: HolidayCalendarId = HolidayCalendarId.of("NOOS")

  /**
   * An identifier for the holiday calendar of Auckland, New Zealand, with code 'NZAU'.
   *
   * This constant references the calendar for Auckland bank holidays.
   */
  val NZAU: HolidayCalendarId = HolidayCalendarId.of("NZAU")

  /**
   * An identifier for the holiday calendar of Wellington, New Zealand, with code 'NZWE'.
   *
   * This constant references the calendar for Wellington bank holidays.
   */
  val NZWE: HolidayCalendarId = HolidayCalendarId.of("NZWE")

  /**
   * An identifier for the holiday calendar of Warsaw, Poland, with code 'PLWA'.
   *
   * This constant references the calendar for Warsaw bank holidays.
   */
  val PLWA: HolidayCalendarId = HolidayCalendarId.of("PLWA")

  /**
   * An identifier for the holiday calendar of Stockholm, Sweden, with code 'SEST'.
   *
   * This constant references the calendar for Stockholm bank holidays.
   */
  val SEST: HolidayCalendarId = HolidayCalendarId.of("SEST")

  /**
   * An identifier for the holiday calendar of Bangkok, Thailand, with code 'THBA'.
   *
   * This constant references the calendar for Bangkok bank holidays.
   */
  val THBA: HolidayCalendarId = HolidayCalendarId.of("THBA")

  /**
   * An identifier for the holiday calendar of Johannesburg, South Africa, with code 'ZAJO'.
   *
   * This constant references the calendar for Johannesburg bank holidays.
   */
  val ZAJO: HolidayCalendarId = HolidayCalendarId.of("ZAJO")
}

