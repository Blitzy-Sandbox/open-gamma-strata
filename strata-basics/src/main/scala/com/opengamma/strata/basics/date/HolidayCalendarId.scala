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
import com.opengamma.strata.basics.ReferenceDataType
import com.opengamma.strata.basics.Resolvable
import com.opengamma.strata.basics.currency.Currency
import com.opengamma.strata.basics.currency.CurrencyPair
import com.opengamma.strata.collect.JvmClosure
import com.opengamma.strata.collect.Named
import com.opengamma.strata.collect.NoJavaSerialization
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
 * is the contract [[ReferenceDataId]] requires of an identifier, and here the equality comes
 * from the type being a case class over a single element, while the hash is the name's, held
 * on the instance by [[HolidayCalendarId.hashCode]].
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
 * A composite name is '''normalised''' when it is built: the parts are deduplicated and
 * sorted by name, so `USNY+GBLO` and `GBLO+USNY` are one identifier, with one name, that
 * encodes to one JSON string. The no-holidays calendar is absorbed rather than carried, and
 * the two separators differ in how, because their meanings differ: combining with a calendar
 * that has no holidays removes nothing, so it drops out of a `'+'` composite, while linking
 * with it makes every day a business day, so it swallows a `'~'` composite whole.
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
 * parts combined. That two-step lookup is why this type defines its own [[resolve]] rather
 * than taking the single lookup [[ReferenceDataId]] performs. See [[resolve]] for the failure
 * cases.
 *
 * A calendar the reference data does not hold is reported as a failure value rather than
 * raised: it is a fact about the request rather than a defect in the program, and the caller
 * is the one with the context to decide what to do about it. Which type of value an
 * identifier may be answered with is settled by [[valueType]], a witness that recognises a
 * calendar by pattern rather than by a class token, so resolution inspects no type while the
 * program runs.
 *
 * @param name  the identifier, expressed as a normalised unique name, such as `GBLO` or
 *   `GBLO+USNY`
 * @see [[HolidayCalendarIds]] for the identifiers of the calendars this library knows about
 * @see [[HolidayCalendar]] for the calendar an identifier resolves to
 */
sealed abstract case class HolidayCalendarId private (name: String)
    extends ReferenceDataId[HolidayCalendar]
    with Resolvable[HolidayCalendar]
    with Named
    with NoJavaSerialization {

  // The construction closure of this type, run for every instance of every subclass of it: the
  // `private` constructor and the `sealed` modifier are enforced against Scala, and neither
  // survives into the class file, so the only place a subtype compiled by other means - which
  // would carry a name `of` had not normalised, or a structure that disagrees with its name - can
  // be stopped is here. The single implementation is the companion's hidden `Impl`.
  JvmClosure.requireSoleImplementation(this, classOf[HolidayCalendarId.Impl])

  // The invariant of this type, stated over the two fields the instance actually holds rather
  // than over the text a factory was given, because the class file of the implementation carries
  // a public constructor whatever the source asked for: a class compiled outside this library can
  // reach it directly, and the check above would admit what it built, its runtime class being the
  // one class that check admits. What is left to state is what [[HolidayCalendarId.of]]
  // establishes. `of` accepts any text - which name an application files a calendar under is not
  // this library's to judge - so what it establishes is not a property of the text but the
  // normalisation it performs on it: a name carrying a separator is the names of its parts,
  // deduplicated, sorted and joined, and the structure carried beside it is those same parts.
  // Without these, a value could hold `USNY+GBLO` - the name the normalisation exists to remove,
  // which would be a second identifier for the calendar `GBLO+USNY` already names, unequal to it
  // and encoding to different bytes - or hold parts that resolve a different set of calendars
  // from the ones its name reads as.
  //
  // The first of them admits one further shape, because `of` produces it: a name past the
  // ceilings that factory documents is decomposed not at all and is kept whole as a simple
  // identifier, separators and all. That is the one simple identifier whose name carries a
  // separator, and stating the invariant without it would refuse a value the factory hands out.
  // The ceiling test runs only for a simple identifier whose name carries a separator, which is
  // exactly that case, so an ordinary identifier pays nothing for it.
  JvmClosure.requireInvariant(
    "the name of a simple identifier carries neither separator, unless it is a name too large " +
      "to decompose",
    composite.isDefined || !HolidayCalendarId.carriesSeparator(name) ||
      HolidayCalendarId.exceedsCompositeCeilings(name))
  JvmClosure.requireInvariant(
    "a composite identifier names two or more parts, distinct and sorted by name",
    composite.forall(parts =>
      parts.components.size >= 2 && HolidayCalendarId.ascendingByName(parts.components.toList)))
  JvmClosure.requireInvariant(
    "the name of a composite identifier is the names of its parts joined by its separator",
    composite.forall(parts =>
      name == parts.components.iterator
        .map(part => part.name)
        .mkString(HolidayCalendarId.separatorOf(name))))
  JvmClosure.requireInvariant(
    "no part of a composite identifier is the calendar that declares no holidays",
    composite.forall(parts =>
      parts.components.forall(part => part.name != HolidayCalendarId.NoHolidaysName)))

  /**
   * The parts of this identifier and how they are read together, or empty where it is simple.
   *
   * The structure is worked out once, when the identifier is built, and carried on the
   * instance rather than recovered from the name at each resolution: a composite identifier is
   * resolved once per date adjustment, and re-splitting its name each time would allocate on
   * that path.
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
   * carries; a simple identifier names a single calendar.
   *
   * Answered by asking whether this identifier carries the parts of a composite, which is the
   * question in the form that costs nothing: the parts are worked out once, when the identifier
   * is built. It agrees with what the name shows by construction, since a composite name is
   * built by joining two or more parts and a name that normalises to one part is that part's own
   * identifier, which carries no parts. Asking it of the structure rather than of the name
   * matters because the question is asked of every part of a composite identifier on every
   * resolution, and every date adjustment against such an identifier resolves it, where
   * searching the name for either separator walks the whole of it.
   *
   * @return true if this identifier combines or links two or more calendars
   */
  def isComposite: Boolean = composite.isDefined

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
   * Two conditions end resolution without a calendar, and each is reported as reference data
   * the caller did not supply:
   *
   *   - a simple identifier the reference data does not hold, reported as `ReferenceData`
   *     itself reports an identifier it cannot answer for;
   *   - a part of a composite identifier that cannot be resolved, reported naming both that
   *     part and this identifier, so a caller is told which of several calendars was missing
   *     and what was being built from it.
   *
   * A third is possible for a composite identifier alone, and is `Failure.Invalid`: parts that
   * cannot be read together, because doing so would build a calendar deeper than a calendar may
   * be. The family limits how deep a composite may read, at
   * [[com.opengamma.strata.basics.date.HolidayCalendar.MaxCompositeDepth]], and two things can
   * carry a resolution past it. One is the name: [[of]] accepts any name, so the number of
   * parts is decided by whatever text reached it, and a name of ten thousand parts would
   * describe a calendar ten thousand calendars deep. The other is the data: a name joining two
   * parts describes a calendar one deeper than its deeper part, so reference data mapping a
   * part to an already-deep composite can carry a two-part name over the limit. The width of
   * the name is checked before any part is looked up, and the depth the resolved calendars
   * reach is checked before each is read together with the ones before it. Both are reported in
   * this method's own failure channel, so that resolving an identifier answers rather than
   * raising, however the identifier was named and whatever the data holds for its parts.
   *
   * The first unresolvable part ends the resolution: a composite identifier is all-or-nothing,
   * since a calendar assembled from some of its parts would silently declare business days
   * that are holidays.
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
          // a simple identifier the data does not hold is the absence `getValue` already
          // reports, so the report is delegated to it rather than written again here; the
          // second lookup this costs is taken on the failing path only
          case None => refData.getValue(this)
          case Some(parts) if parts.componentCount > HolidayCalendar.MaxCompositeDepth =>
            Left(tooManyParts(parts.componentCount))
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
   * The witness by which reference data recognises a value this identifier may answer with.
   *
   * Every holiday calendar identifier refers to a [[HolidayCalendar]] - the composite ones
   * included, a composite resolving to a calendar exactly as a simple identifier does - so
   * the witness is the shared [[com.opengamma.strata.basics.ReferenceDataType.holidayCalendar]]
   * rather than one per identifier. It is what `ImmutableReferenceData.findValue` narrows the
   * value it finds with, and it recognises a calendar by pattern rather than by a class token.
   *
   * @return the witness for a holiday calendar
   */
  override def valueType: ReferenceDataType[HolidayCalendar] = ReferenceDataType.holidayCalendar

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
   * Returns the text form of this identifier, which is its name, exactly.
   *
   * [[name]] is the identity of an identifier and it is the whole of its content, so the text
   * form is the name and nothing else: the same text the identifier being ported answered with
   * from `getName()` and from `toString()`, the text [[HolidayCalendarId.of]] reads back into
   * this identifier, the text the JSON form and the key form carry, and the text every
   * composite rendering that names a calendar - a business day adjustment, a days adjustment,
   * a schedule definition - hands on. The structural rendering a case class would otherwise
   * give is replaced by it for exactly that reason, and every constant of
   * [[HolidayCalendarIds]] and every composite this library builds from them therefore reads
   * as it always did, `GBLO`, `Sat/Sun`, `GBLO+USNY`.
   *
   * [[HolidayCalendarId.of]] is total: which names an application files its calendars under is
   * not this library's to judge, so a name may hold a line break, another control character or
   * any length of text, and this method answers with the whole of it regardless. Neutralising
   * such text - bounding it and escaping what a line-oriented reader could act on - is the
   * business of the places that write a diagnostic, namely
   * [[com.opengamma.strata.collect.result.Failure.show]] and therefore the text form of every
   * failure, and the decoder bridge in [[com.opengamma.strata.collect.json.Codecs]]. It is not
   * the business of a value's own text form, which is the identity of the value: a name that
   * arrived from outside is still the name of the calendar an application asked for, and code
   * that reads, compares, re-parses or re-serializes an identifier has to receive it whole.
   *
   * @return the name of this identifier
   */
  override def toString: String = name

  /**
   * Returns a suitable hash code for the identifier, which is the hash of its name.
   *
   * It is computed once, when the identifier is built, and held in a field on the instance.
   * The hash a case class generates would instead walk the value and mix it afresh on every
   * call, and that cost is worth removing here because an identifier is the key of every
   * reference-data lookup: it is hashed at least once per resolution - once more for each part
   * of a composite - and every date adjustment resolves a calendar.
   *
   * It agrees with equality, as it must for the identifier to work as a key: the hash is the
   * name's and equality is the name's, since the name is the only element the value carries.
   *
   * @return the hash code of this identifier, which is the hash code of its name
   */
  override val hashCode: Int = name.hashCode

  /**
   * Resolves the parts of a composite identifier and reads them together.
   *
   * Each part is resolved in the order the normalised name gives, and each result is read
   * together with the ones before it using the operation the separator chose, so the calendar
   * built is the left fold of the parts that the name describes. The first part that cannot be
   * resolved ends the resolution and is the answer, which is the all-or-nothing behaviour
   * described on [[resolve]].
   *
   * Written as a tail-recursive walk of the parts rather than as a traversal of them because
   * this is the path a date adjustment against a composite calendar takes on every date: a
   * traversal of an applicative would build a list of resolved calendars, and the deferred
   * computations that sequence it, only to fold that list away again. The walk allocates what
   * it answers with and nothing else, and it reaches the same calendar by the same operations
   * in the same order.
   *
   * @param parts  the parts of this composite identifier and how to read them together
   * @param refData  the reference data to resolve the parts against
   * @return the assembled holiday calendar, or the failure from the first unresolvable part
   */
  private def resolveParts(
      parts: HolidayCalendarId.Composite,
      refData: ReferenceData): Either[Failure, HolidayCalendar] = {

    @tailrec
    def loop(
        remaining: List[HolidayCalendarId],
        resolved: HolidayCalendar): Either[Failure, HolidayCalendar] =

      remaining match {
        case Nil => Right(resolved)
        case component :: rest =>
          resolvePart(component, refData) match {
            // the depth a combination would reach is decided by the calendars the data
            // supplied, not by the name that asked for them: a two-part name resolves to a
            // composite as deep as its deeper part plus one, and a part the caller's data maps
            // to an already-deep composite can carry the result past the limit even though the
            // name joins two calendars. The check on the name, applied by `resolve` before any
            // lookup, bounds the parts; this one bounds what they turned out to be, and it
            // answers in this method's failure channel rather than letting the constructor of
            // the composite raise out of an `Either`
            case Right(calendar) if HolidayCalendar.exceedsCompositeDepth(resolved, calendar) =>
              Left(tooDeepToCombine(component, HolidayCalendar.compositeDepthOf(resolved, calendar)))
            case Right(calendar) => loop(rest, parts.combine(resolved, calendar))
            // the failure of a part is the failure of the whole, so it is answered with as it
            // stands rather than rebuilt
            case failed @ Left(_) => failed
          }
      }

    // the parts are a non-empty list, so the first of them is the calendar the fold starts from
    resolvePart(parts.components.head, refData) match {
      case Right(first) => loop(parts.components.tail, first)
      case failed @ Left(_) => failed
    }
  }

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
   * It names the part that was missing and the identifier being resolved, and carries both as
   * attributes so that a caller can act on them without reading the message. Both names appear
   * in the message and in the attributes exactly as the identifiers hold them, because the
   * caller supplying the reference data needs to see the identifier it failed to provide for as
   * that identifier is written rather than an approximation of it.
   *
   * Neither name is constrained. Both come from [[HolidayCalendarId.of]], which is total and
   * accepts any text, since which names an application files its calendars under is not this
   * library's to judge, so a name may hold a line break, a control character or any length of
   * text. That is exactly why the neutralising is not done here: bounding a value and escaping
   * what it may hold belong to the act of writing a failure out, where one bounded, single-line
   * rendering is applied to the message and to the key and value of every attribute. An
   * identifier cannot therefore forge or inflate a line of a log holding this failure
   * (CWE-117), while code that reads the failure to act on it rather than to display it still
   * receives both names whole.
   *
   * @param component  the part that could not be resolved
   * @return the failure describing the missing part, naming both identifiers as they stand
   */
  private def partNotFound(component: HolidayCalendarId): Failure =
    Failure
      .MissingData(
        s"Reference data not found for '${component.name}' of type 'HolidayCalendarId' " +
          s"when finding '$name'")
      .withAttribute("id", component.name)
      .withAttribute("compositeId", name)

  /**
   * Returns the failure reported when this identifier joins more parts than a calendar may read.
   *
   * The count is carried as an attribute beside the identifier, so a caller aggregating failures
   * can act on the number without reading the message. The identifier itself reaches both the
   * message and the attribute as it stands, for the reason [[partNotFound]] gives: the name
   * arrived from outside this library and the caller correcting it needs the whole of it, while
   * bounding and neutralising what it may hold belongs to the writing of a failure, which
   * [[com.opengamma.strata.collect.result.Failure.show]] and the text form of a failure perform
   * for the message and for every attribute.
   *
   * @param count  the number of parts this identifier names
   * @return the failure describing the identifier that cannot name a calendar
   */
  private def tooManyParts(count: Int): Failure =
    Failure
      .Invalid(
        s"Holiday calendar '$name' joins $count calendars, but a calendar cannot read through " +
          s"more than ${HolidayCalendar.MaxCompositeDepth}")
      .withAttribute("id", name)
      .withAttribute("components", count.toString)

  /**
   * The failure reported where the calendars the data supplied cannot be read together.
   *
   * The counterpart of [[tooManyParts]] for the depth of the resolved calendars rather than the
   * width of the name: the name may join two parts and still describe something too deep, if
   * the data maps a part to a composite that is already as deep as a calendar may be. The part
   * whose calendar carried the combination over the limit is named, since that is the entry of
   * the caller's reference data to look at.
   *
   * @param component  the part whose calendar cannot be read together with the ones before it
   * @param depth  the number of calendars the combination would read through
   * @return the failure describing the refusal
   */
  private def tooDeepToCombine(component: HolidayCalendarId, depth: Int): Failure =
    Failure
      .Invalid(
        s"Holiday calendar '$name' resolves to calendars that read through $depth calendars " +
          s"together, but a calendar cannot read through more than " +
          s"${HolidayCalendar.MaxCompositeDepth}")
      .withAttribute("id", name)
      .withAttribute("component", component.name)
      .withAttribute("depth", depth.toString)
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
 * is not an error until it fails to resolve. That is why this factory answers with an
 * identifier rather than with a result. What `of` does do is '''normalise''': the parts of a
 * composite name are deduplicated and sorted, and the no-holidays calendar is absorbed, so
 * that a name and its rearrangements are one value. The rules are described on
 * [[HolidayCalendarId]]. Normalisation is where the work of reading a name lives, so it carries
 * the two ceilings `of` documents: a name beyond them is kept whole as one opaque identifier
 * instead of being decomposed, which bounds the work without making the factory able to fail.
 *
 * Neither `apply` nor `copy` exists, which is what keeps that normalisation inescapable: the
 * constructor of a `sealed abstract case class` is reachable only from inside this file, and
 * the two private factories below are the only places that reach it. Pattern matching still
 * works, so `case HolidayCalendarId(name) =>` reads the name of an identifier.
 *
 * ===The default calendar of a currency===
 *
 * [[defaultByCurrency]] answers the calendar conventionally used for a currency, from the
 * thirty-one-row table held as code below. It is one lookup, answering with an `Option`,
 * because a currency for which no conventional calendar is recorded is an ordinary answer
 * rather than a failure of the program.
 *
 * ===Instances===
 *
 * The companion declares one equality-bearing instance, one rendering, one codec and the two
 * halves of a key codec. `Order` and `Hash` both extend `Eq`, so declaring them as a single
 * value makes it impossible for equality and ordering to disagree, and there is deliberately
 * no separate `Eq`.
 */
object HolidayCalendarId {

  /**
   * The separator of a combined identifier, whose parts are all observed.
   *
   * Held as a one-character string rather than as a character so that every use - the
   * membership check, the splitting and the joining - goes through the string-taking methods of
   * the standard library, which is what lets one value serve all three.
   */
  private[date] val CombineSeparator: String = "+"

  /**
   * The separator of a linked identifier, whose parts are observed in the alternative.
   *
   * Held as a one-character string for the reason given on [[CombineSeparator]].
   */
  private[date] val LinkSeparator: String = "~"

  /**
   * The name of the calendar that declares no holidays at all, which normalisation recognises
   * by name so that a part naming it is absorbed before it becomes an identifier.
   */
  private val NoHolidaysName: String = "NoHolidays"

  /**
   * The combine separator as a character, for the one scan that reads characters directly.
   *
   * Taken from [[CombineSeparator]] rather than written again, so the two spellings of the
   * separator cannot drift apart. Comparing two `Char`s involves no widening, which is why the
   * reason [[CombineSeparator]] gives for being a string does not apply here.
   */
  private val CombineSeparatorChar: Char = CombineSeparator.charAt(0)

  /** The link separator as a character, for the same scan and for the same reason. */
  private val LinkSeparatorChar: Char = LinkSeparator.charAt(0)

  /**
   * The longest name [[of]] decomposes into the calendars it combines or links.
   *
   * A name is not this library's to judge - `of` is total and accepts anything - but the work it
   * does on one is this library's to bound. Decomposing a name allocates an identifier per part,
   * sorts them and joins them back into a normalised name, so the cost of a name is its length
   * plus its parts times their logarithm. Both are unbounded in the text a caller supplies, and
   * a name written to be hostile is a name written to maximise them (CWE-400/CWE-770).
   *
   * Sixty-five thousand five hundred and thirty-six characters is far above every composite this
   * library or its test suite builds - the largest is a ten-thousand-character name of two
   * thousand and one calendars, which decomposes as it always did - and above any name an
   * application would write, a composite of every calendar this library knows being under two
   * hundred characters.
   */
  private val MaxCompositeNameLength: Int = 65536

  /**
   * The most parts [[of]] decomposes a name into.
   *
   * The second half of the same bound: a name within the length ceiling can still be thousands
   * of separators, and it is the number of parts that drives the allocation and the sort. Four
   * thousand and ninety-six is above every composite in use - two thousand and one parts is the
   * largest the test suite of this port builds, and a composite of every calendar this library
   * knows has twenty-nine - and it is counted rather than estimated, by the scan below.
   */
  private val MaxCompositeParts: Int = 4096

  /**
   * The ordering of the parts of a composite name, from the last name to the first.
   *
   * Held here as a plain value so that normalising a composite name does not build a comparison
   * function every time it is called, and declared before anything that uses it so that it is in
   * place whenever [[of]] runs. It is not the published [[order]] of the type, which is declared
   * below and which is also equality and hashing: this one is used only while the parts of a
   * name are being normalised, and it agrees with that one, both being the name's.
   *
   * Descending, because the parts are sorted and then walked while the duplicates among them are
   * dropped: walking a descending list and keeping each part that differs from the one kept last
   * produces the ascending list of distinct parts directly, so the normalisation is a sort and
   * one walk rather than a sort and two.
   */
  private val byNameDescending: Ordering[HolidayCalendarId] =
    Ordering.by[HolidayCalendarId, String](calendarId => calendarId.name).reverse

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
      val combine: (HolidayCalendar, HolidayCalendar) => HolidayCalendar) {

    /**
     * How many parts this identifier names.
     *
     * Counted once, here, because resolution reads it on every date adjustment against a
     * composite identifier and counting a list is a walk of it. It is what
     * [[HolidayCalendarId.resolve]] judges against
     * [[com.opengamma.strata.basics.date.HolidayCalendar.MaxCompositeDepth]]: resolving a
     * composite reads its parts together one after another, so the calendar it assembles reads
     * through as many calendars as there are parts, and a name with more parts than a calendar
     * may read through names no calendar at all.
     *
     * @return the number of parts of this identifier, at least two
     */
    val componentCount: Int = components.length
  }

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
   * ===The work of decomposing a name is bounded===
   *
   * This factory is total and stays total: it is reached from inside `Either` and Circe
   * decoding - a day count name is read through it, and so is the calendar of a document - so a
   * name it could refuse would have to be refused by raising, which would put a failure outside
   * the channel those callers read. What is bounded is therefore the '''work''', not the input:
   * a name longer than [[MaxCompositeNameLength]] characters, or holding more than
   * [[MaxCompositeParts]] separator-delimited parts, is kept as a simple non-composite
   * identifier of exactly the name that was given, rather than being decomposed, sorted and
   * rejoined. The part count is taken by one scan over the characters that allocates nothing and
   * stops as soon as the ceiling is passed, so reading a name never costs more than reading it.
   *
   * The consequence is worth stating plainly, because it is a behaviour and not only a cost: an
   * adversarially long composite name is one opaque identifier. It is not normalised, so it is
   * equal only to itself and to an identically spelled name; it reports itself as not composite;
   * and it resolves against reference data only if a host supplied that exact name, which for
   * such a name means against nothing. Every composite any application writes is orders of
   * magnitude inside both ceilings and normalises exactly as before - including the
   * ten-thousand-character name of two thousand and one calendars that the day count grammar of
   * this library admits, which still resolves to `GBLO+USNY` (CWE-400/CWE-770).
   *
   * @param uniqueName  the unique name
   * @return the identifier
   */
  def of(uniqueName: String): HolidayCalendarId =
    // The separators are tested in this order because `'+'` binds more tightly than `'~'`, so a
    // name carrying both is a link of combinations. The ceilings are tested ahead of both, since
    // a name past either of them is not decomposed at all and the test that decides that is
    // cheaper than the membership tests it precedes.
    if (exceedsCompositeCeilings(uniqueName)) {
      simple(uniqueName)
    } else if (uniqueName.contains(LinkSeparator)) {
      val parts = normalisedParts(uniqueName, LinkSeparator, dropNoHolidays = false)
      // linking a calendar that has no holidays makes every day a business day
      if (parts.contains(NoHolidaysId)) {
        NoHolidaysId
      } else {
        compose(parts, LinkSeparator, (first, second) => first.linkedWith(second))
      }
    } else if (uniqueName.contains(CombineSeparator)) {
      val parts = normalisedParts(uniqueName, CombineSeparator, dropNoHolidays = true)
      compose(parts, CombineSeparator, (first, second) => first.combinedWith(second))
    } else {
      simple(uniqueName)
    }

  /**
   * Checks whether an identifier names more than one calendar.
   *
   * The same question as [[HolidayCalendarId.isComposite]], asked of an identifier held as an
   * argument, and answered by it.
   *
   * @param id  the holiday calendar identifier
   * @return true if the identifier combines or links two or more calendars
   */
  def isCompositeCalendar(id: HolidayCalendarId): Boolean = id.isComposite

  /**
   * Finds the calendar conventionally used for a currency.
   *
   * The answer is the market convention - the calendar of the centre a payment in that
   * currency settles in - and it is a convention rather than a rule, which is why an
   * application is free to disregard it. Thirteen of the identifiers this table can produce
   * name calendars whose holidays this library does not ship; the lookup answers with them all
   * the same, and they do not resolve against `ReferenceData.standard`, so a caller that wants
   * those holidays supplies reference data holding them.
   *
   * The lookup is total: a currency for which no conventional calendar is recorded is an
   * ordinary answer of `None`.
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
   * is the no-holidays identifier - the identity of combination - so this lookup always answers
   * with an identifier, where [[defaultByCurrency]] may answer with nothing.
   *
   * @param currencyPair  the currency pair to find the defaults for
   * @return the identifier of the calendar conventionally used for the pair
   */
  def defaultByCurrencyPair(currencyPair: CurrencyPair): HolidayCalendarId =
    currencyPair.toSet.toList
      .flatMap(currency => defaultByCurrency(currency).toList)
      .foldLeft(NoHolidaysId)((combined, calendarId) => combined.combinedWith(calendarId))

  /**
   * The calendar conventionally used for each currency, held as code.
   *
   * These are thirty-one rows, and they are part of the behaviour of this library rather than
   * something an application is invited to replace: holding them here means a lookup depends on
   * nothing outside this file.
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
      // the calendars named below are not shipped with this library; these identifiers resolve
      // only against reference data that supplies them
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
  private def simple(uniqueName: String): HolidayCalendarId = new Impl(uniqueName, None)

  /**
   * Builds an identifier from the normalised parts of a composite name.
   *
   * The three cases are the three outcomes normalisation can have:
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
      case first :: rest =>
        // `rest` is the tail of the normalised parts and, the two cases above having been
        // answered, is never empty, so it becomes the tail of the component list as it stands
        val components = NonEmptyList(first, rest)
        val structure = new Composite(components, combine)
        new Impl(parts.iterator.map(part => part.name).mkString(separator), Some(structure))
    }

  /**
   * Whether a name is too large for [[of]] to decompose it.
   *
   * Two ceilings, tested in the order that costs least: the length, which is a field read, and
   * then the number of parts, which is one walk over the characters. A name past either is kept
   * whole, for the reasons and with the consequences set out on `of`.
   *
   * The part count is the number of separators of either kind plus one, and either kind counts
   * because a linked name is decomposed into its links and each of those into its combinations,
   * so the total work of a name is driven by all of its separators together. The walk carries
   * its position and its running count as parameters - no mutable state, and nothing allocated -
   * compiles to a jump, and stops the moment the count reaches the ceiling, so a name written to
   * hold a million separators is abandoned after the first few thousand.
   *
   * @param uniqueName  the name to measure
   * @return true if the name is to be kept whole rather than decomposed
   */
  private def exceedsCompositeCeilings(uniqueName: String): Boolean =
    uniqueName.length > MaxCompositeNameLength || exceedsPartCeiling(uniqueName, 0, 0)

  /**
   * Whether the name holds more separators than a decomposable name may.
   *
   * A name of `n` separators has `n + 1` parts, so more than [[MaxCompositeParts]] parts is
   * [[MaxCompositeParts]] separators or more, which is the comparison made here.
   *
   * @param uniqueName  the name being measured
   * @param index  the index to continue at
   * @param separators  the separators counted so far
   * @return true if the name holds at least [[MaxCompositeParts]] separators
   */
  @tailrec
  private def exceedsPartCeiling(uniqueName: String, index: Int, separators: Int): Boolean =
    if (separators >= MaxCompositeParts) {
      true
    } else if (index >= uniqueName.length) {
      false
    } else {
      val character = uniqueName.charAt(index)
      val counted =
        if (character == CombineSeparatorChar || character == LinkSeparatorChar) separators + 1
        else separators
      exceedsPartCeiling(uniqueName, index + 1, counted)
    }

  /**
   * The one implementation of an identifier.
   *
   * A `sealed abstract case class` needs a concrete subclass to be instantiated at all, and this
   * is it. It is declared rather than written as an anonymous subclass at each instantiation site
   * for two reasons, both about what the class file says: a private member class is one a Java
   * compiler refuses to name, where an anonymous class is public and can be instantiated directly
   * by a caller in another language, and a named class can be compared against, which is what
   * lets [[HolidayCalendarId]] refuse in its own constructor to be any other implementation.
   *
   * One class serves both the simple and the composite identifier, the structure being a
   * constructor `val` that is empty for the former - which is what lets that refusal name a
   * single class rather than admit a set of them. The structure stays off the single case element
   * of the type and therefore out of its equality, exactly as it did when each site supplied it
   * in an anonymous body.
   *
   * @param name  the normalised unique name of the identifier
   * @param composite  the parts of a composite identifier, or empty where it is simple
   */
  private final class Impl(name: String, private[date] val composite: Option[Composite])
      extends HolidayCalendarId(name)

  /**
   * Splits a composite name around a separator and normalises the parts it names.
   *
   * Each part becomes an identifier in its own right - which is what makes a part of a linked
   * name able to be a combination - and the parts are deduplicated and ordered by name. Sorting
   * is what makes the name of a composite independent of the order it was written in, and
   * deduplicating is what makes `GBLO+GBLO` the simple `GBLO`. The splitting is written here
   * rather than taken from the standard library: a library split is driven by a regular
   * expression, in which both separators of this type are metacharacters, and it discards a
   * trailing empty part, where this walk keeps it, so `GBLO+` names two parts of which one has
   * an empty name.
   *
   * The name is walked once, each part being read and turned into an identifier as it is
   * reached; the walk carries its position and its result as parameters, so it holds no mutable
   * state and recurses in constant stack space. The parts are then sorted with the one shared
   * comparison of [[byNameDescending]] and walked once more to drop the duplicates, which a
   * sorted list puts next to each other. A part therefore becomes an identifier as it is read
   * rather than through an intermediate list of names, duplicates are dropped by adjacency
   * rather than through a set, and no comparison function is built for the call. Sorting keeps
   * the work of a name of any length proportional to its parts times their logarithm, never
   * their square, so a name written to be hostile is no more than long.
   *
   * @param uniqueName  the composite name, as it was written
   * @param separator  the separator to split around
   * @param dropNoHolidays  true to drop the parts naming the no-holidays calendar, which is
   *   what combining with it does; a link absorbs it instead and so keeps it
   * @return the identifiers of the parts, deduplicated and sorted by name
   */
  private def normalisedParts(
      uniqueName: String,
      separator: String,
      dropNoHolidays: Boolean): List[HolidayCalendarId] = {

    @tailrec
    def parts(from: Int, found: List[HolidayCalendarId]): List[HolidayCalendarId] = {
      val index = uniqueName.indexOf(separator, from)
      val end = if (index < 0) uniqueName.length else index
      val partName = uniqueName.substring(from, end)
      // in a combination the no-holidays calendar removes nothing and so is dropped by name,
      // before the parts become identifiers
      val next = if (dropNoHolidays && partName == NoHolidaysName) found else of(partName) :: found
      if (index < 0) next else parts(end + separator.length, next)
    }

    distinctAscending(parts(0, Nil).sorted(byNameDescending), Nil)
  }

  /**
   * Drops the duplicates from parts sorted from the last name to the first.
   *
   * Equal parts are next to each other in a sorted list, so a part is a duplicate exactly where
   * it has the name of the part kept last. Keeping by prepending turns the descending input into
   * an ascending result, which is the order a composite name is written in.
   *
   * @param remaining  the parts still to consider, sorted from the last name to the first
   * @param kept  the distinct parts already kept, in ascending order
   * @return the distinct parts, in ascending order
   */
  @tailrec
  private def distinctAscending(
      remaining: List[HolidayCalendarId],
      kept: List[HolidayCalendarId]): List[HolidayCalendarId] =

    remaining match {
      case Nil => kept
      case part :: rest =>
        val next = kept match {
          case previous :: _ if previous.name == part.name => kept
          case _ => part :: kept
        }
        distinctAscending(rest, next)
    }

  /**
   * Whether a name is the name of a composite, which is to say that it carries a separator.
   *
   * Used by the invariant an identifier states in its own constructor to tell the two shapes of
   * name apart, in the direction [[of]] tells them apart in: a name holding either separator is
   * split and normalised, and a name holding neither is a simple identifier's own.
   *
   * @param uniqueName  the name to test
   * @return true where the name names more than one calendar
   */
  private def carriesSeparator(uniqueName: String): Boolean =
    uniqueName.contains(LinkSeparator) || uniqueName.contains(CombineSeparator)

  /**
   * The separator that joins the parts of a composite name.
   *
   * The order of the two tests is the order [[of]] applies them in, and for the same reason:
   * `'+'` binds more tightly than `'~'`, so a name carrying both is a link of combinations and
   * its own separator is the link. The parts of a link are therefore allowed to carry `'+'`,
   * while no part of either carries `'~'`, which is what makes this reading of a name unambiguous.
   *
   * @param uniqueName  the composite name
   * @return the separator its parts are joined by
   */
  private def separatorOf(uniqueName: String): String =
    if (uniqueName.contains(LinkSeparator)) LinkSeparator else CombineSeparator

  /**
   * Whether parts are in the strictly ascending order of names that normalisation leaves them in.
   *
   * Strictly, because [[distinctAscending]] both sorts and deduplicates: a part equal to the one
   * before it is one normalisation would have dropped. The comparison is the name's, which is the
   * comparison [[byNameDescending]] sorts by and the one [[order]] publishes, so the three cannot
   * disagree about a pair of parts.
   *
   * Written as a walk of adjacent pairs rather than by sorting a copy, because it is checked by
   * the invariant of every composite identifier as it is constructed and a composite is built on
   * the path of a date adjustment.
   *
   * @param parts  the parts of a composite identifier, in the order it holds them
   * @return true where each part's name precedes the next part's name
   */
  @tailrec
  private def ascendingByName(parts: List[HolidayCalendarId]): Boolean =
    parts match {
      case first :: (rest @ next :: _) =>
        first.name.compareTo(next.name) < 0 && ascendingByName(rest)
      case _ => true
    }

  /**
   * The ordering of holiday calendar identifiers, which is also their hashing and equality.
   *
   * This is the only equality-bearing instance of the type. `Order` and `Hash` both extend
   * `Eq`, so declaring them together is what makes it impossible for the ordering, the hashing
   * and the equality of an identifier to disagree, and it is why no separate `Eq` is declared.
   *
   * All three are the name. Equality and hashing are those of the value itself: its equality
   * is the name alone, since that is the only element the case class carries, and its hash is
   * the one [[HolidayCalendarId.hashCode]] holds, which is the name's. Ordering is the ordering
   * of the names, which agrees with that equality exactly: two identifiers compare equal
   * precisely when their names are equal, and that is precisely when they are equal. The effect
   * is that a sorted collection of identifiers reads alphabetically, and that the parts of a
   * composite name are in the order this instance would put them in.
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
   * The rendering is the identifier's name, character for character, for every identifier -
   * the text the identifier being ported rendered, and the text [[of]] reads back into the
   * same identifier. It is taken from [[HolidayCalendarId.toString]] rather than written again
   * here, so that the two cannot drift apart: an identifier reaches a reader the same way
   * whether it is rendered through this instance or interpolated into a string.
   *
   * @return the rendering of holiday calendar identifiers
   */
  implicit val show: Show[HolidayCalendarId] = Show.fromToString[HolidayCalendarId]

  /**
   * The JSON codec for holiday calendar identifiers.
   *
   * An identifier is written as the bare string of its name, so a document holds
   * `"GBLO+USNY"` rather than an object. Reading goes through [[of]], which accepts any name,
   * so a document naming a calendar this library does not know about is read - and fails, if at
   * all, when it is resolved, which is where a missing calendar belongs.
   *
   * Because a name is normalised when it is read and the encoded form is the name, two
   * identifiers built from differently ordered composites - `USNY+GBLO` and `GBLO+USNY` -
   * encode to identical bytes: the JSON form follows the identifier and not the way its name was
   * written. A name past the ceilings `of` describes is read as the one opaque identifier of that
   * name, so it still decodes, still encodes to the bytes it arrived as and still refuses nothing
   * on this path - the decoder needs no check of its own because `of` has no failure to report.
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
 * trusts.
 *
 * Twenty-nine identifiers are published here, being those of the calendars in common use
 * rather than the whole space of identifiers: `HolidayCalendarId.of` builds any other name a
 * caller needs. The built-in calendars are not limited to these twenty-nine either - the New
 * Zealand bank calendar `NZBD` is one of them and deliberately carries no constant here - so
 * `HolidayCalendarId.of("NZBD")` names a calendar `ReferenceData.standard` supplies all the
 * same.
 *
 * @see [[HolidayCalendarId]] for what an identifier is and how composites of them are named
 */
object HolidayCalendarIds {

  /**
   * An identifier for a calendar declaring no holidays and no weekends, with code
   * 'NoHolidays'.
   *
   * This calendar has the effect of making every day a business day. It is the usual way of
   * indicating that a holiday calendar does not apply, and it is the identity of
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
