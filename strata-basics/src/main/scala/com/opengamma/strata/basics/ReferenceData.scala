/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import cats.{Hash, Show}

import com.opengamma.strata.basics.date.StandardHolidayCalendars
import com.opengamma.strata.collect.result.Failure

/**
 * Provides access to reference data, such as holiday calendars and securities.
 *
 * Reference data is the data a calculation needs but does not compute: which days are
 * holidays in London, what a security identifier refers to, and so on. It is looked up using
 * implementations of [[ReferenceDataId]], each parameterized with the type of the data it
 * refers to, so a lookup yields the type the identifier promises without anything having to
 * be asked what it holds. The standard implementation of this trait is
 * [[ImmutableReferenceData]].
 *
 * Reference data is threaded explicitly through the calls that need it - `adjust(date,
 * refData)`, `resolve(refData)`, `createSchedule(refData)` - rather than read from ambient
 * state, which is what makes a calculation reproducible from its arguments alone. A caller
 * that has to compose several lookups before it knows which data it will run them against
 * can express each as a `ReferenceDataId.toReader` and combine the readers instead.
 *
 * ===The contract an implementation must honour===
 *
 * An implementation must be immutable and thread-safe: one instance is shared freely across
 * threads and calculations, and a lookup must give the same answer every time it is asked.
 *
 * Only [[findValue]] has to be implemented. The other three members are defined in terms of
 * it and are correct for any implementation, so a source of reference data is written by
 * answering one question: what value, if any, is held for this identifier.
 *
 * The trait is deliberately open rather than sealed. Reference data is an extension point of
 * this library: `HolidaySafeReferenceData` in the `date` package supplies a weekend-only
 * calendar for an identifier the underlying data does not know, an application backs its
 * reference data with a database or a service, and a test supplies a fixture holding two
 * entries. None of those live in this file, and every one of them overrides at least one of
 * the members below, so neither sealing the trait nor making a member `final` would be
 * correct. Notably, the three defaults are written so that overriding `findValue` alone
 * keeps them consistent, while an implementation whose membership test is cheaper than a
 * lookup - or whose answer is "yes" for every identifier of a family, as
 * `HolidaySafeReferenceData`'s is - overrides `containsValue` as well.
 *
 * ===Divergences from the type being ported===
 *
 * The Java interface signalled the absence of a value by returning a reference to nothing
 * from a low-level `queryValueOrNull`, and expressed the three members below in terms of it.
 * This port has no such convention: [[findValue]] is the primitive and returns an `Option`,
 * so that low-level method has no counterpart and neither does the `Optional`-returning
 * wrapper that sat on top of it.
 *
 * [[getValue]] returns an `Either` where the Java original threw
 * `ReferenceDataNotFoundException`. That exception type is not ported: a missing item of
 * reference data is data about the request rather than a defect in the program, so it is
 * returned as a [[com.opengamma.strata.collect.result.Failure]] and the caller - which has
 * the context to decide whether a missing calendar is fatal - decides what to do about it.
 * This is also what makes `ReferenceDataId.toReader` composition possible at all.
 *
 * @see [[ReferenceDataId]] for the identifiers a lookup is made with
 * @see [[ImmutableReferenceData]] for the standard implementation
 * @see [[CombinedReferenceData]] for the implementation [[combinedWith]] returns
 */
trait ReferenceData {

  /**
   * Finds the reference data value associated with the specified identifier.
   *
   * This is the single abstract member of the trait and the primitive the other three are
   * defined in terms of. It answers with the value held for the identifier, or `None` where
   * this source of reference data holds nothing for it; an implementation never signals
   * absence in any other way.
   *
   * The identifier's type parameter is the type of the value returned, so no cast and no
   * runtime type check is needed at the call site:
   *
   * {{{
   * val calendar: Option[HolidayCalendar] = refData.findValue(HolidayCalendarIds.GBLO)
   * }}}
   *
   * @tparam T  the type of the reference data value
   * @param id  the identifier to find
   * @return the reference data value, empty if this reference data holds none for the identifier
   */
  def findValue[T](id: ReferenceDataId[T]): Option[T]

  /**
   * Checks if this reference data contains a value for the specified identifier.
   *
   * The default implementation asks [[findValue]], so it agrees with it by construction. An
   * implementation that can answer the question without producing the value - because
   * membership is cheaper to establish than the value is to build, or because it holds a
   * value for every identifier of a family - overrides this method; such an override must
   * still agree with what `findValue` does for the same identifier.
   *
   * The identifier is accepted with its type parameter unconstrained, exactly as the Java
   * original accepted `ReferenceDataId<?>`: membership does not depend on the type of the
   * value, so requiring the caller to know it would be noise.
   *
   * @param id  the identifier to find
   * @return true if this reference data contains a value for the identifier
   */
  def containsValue(id: ReferenceDataId[_]): Boolean = findValue(id).isDefined

  /**
   * Gets the reference data value associated with the specified identifier.
   *
   * Where [[findValue]] reports absence as `None`, this method reports it as a failure
   * explaining which identifier could not be found, which is what a caller that needs the
   * value in order to continue wants to propagate:
   *
   * {{{
   * for {
   *   calendar <- refData.getValue(HolidayCalendarIds.GBLO)
   *   adjusted <- adjustment.adjust(date, refData)
   * } yield adjusted
   * }}}
   *
   * The failure is a `Failure.MissingData` naming the identifier in its message and carrying
   * it under the `id` attribute, so a caller can act on the identifier without parsing the
   * message. The Java original's message additionally named the runtime class of the
   * identifier; that fragment is deliberately dropped, because reporting it would mean
   * introspecting the class of a value at run time and this port performs no reflection at
   * all. The identifier's own `toString` is what identifies it, and every identifier family
   * of this module renders itself as the name a user would recognise.
   *
   * The failure is built only when the lookup comes back empty, which matters because this
   * method sits on the resolution path of every adjustment and schedule in the library.
   *
   * @tparam T  the type of the reference data value
   * @param id  the identifier to find
   * @return the reference data value, or the failure describing why it could not be found
   */
  def getValue[T](id: ReferenceDataId[T]): Either[Failure, T] =
    findValue(id) match {
      case Some(value) => Right(value)
      case None => Left(ReferenceData.notFound(id))
    }

  /**
   * Combines this reference data with another.
   *
   * The result answers from both sources, consulting this one first: a value held here is
   * the value returned, and the other source is asked only for identifiers this one does
   * not hold. Combining is therefore how an application layers its own data over a
   * general-purpose set without editing either - `myData.combinedWith(ReferenceData.standard)`
   * keeps `myData`'s entries and falls back to the built-in calendars - and it is not
   * symmetric: swapping the operands reverses which side wins a clash.
   *
   * The default implementation wraps both sources in a [[CombinedReferenceData]], which
   * consults them lazily and so never builds a merged store.
   * [[ImmutableReferenceData.combinedWith]] overrides it for the case where both sides are
   * already materialised stores, returning a single merged store instead of a chain of
   * wrappers.
   *
   * @param other  the other reference data
   * @return the combined reference data, preferring the values of this one
   */
  def combinedWith(other: ReferenceData): ReferenceData = CombinedReferenceData(this, other)
}

/**
 * Provides the ways of obtaining reference data, and the entry type a caller supplies it with.
 *
 * The four factories mirror the static methods of the Java interface one for one - [[of]],
 * [[standard]], [[minimal]] and [[empty]] - with the type-checking that Java performed by
 * reflection at construction time replaced by the [[Entry]] type, which cannot pair an
 * identifier with a value of the wrong type in the first place.
 *
 * `StandardReferenceData`, the package-private holder the Java original kept its two built-in
 * sets in, has no counterpart: it existed to give two constants a home, and they are
 * [[standard]] and [[minimal]] here.
 */
object ReferenceData {

  /**
   * A single item of reference data, paired with the identifier it is held under.
   *
   * This type is what makes the reference data store type-safe. The identifier fixes the
   * type of the value, so an entry pairing `HolidayCalendarIds.GBLO` with anything other
   * than a holiday calendar does not compile:
   *
   * {{{
   * ReferenceData.Entry(HolidayCalendarIds.GBLO, calendar)   // compiles
   * ReferenceData.Entry(HolidayCalendarIds.GBLO, "GBLO")     // does not compile
   * }}}
   *
   * That is precisely the guarantee the Java original obtained at run time, by asking each
   * identifier for the `Class` of the data it referred to and testing the value against it.
   * Both failure modes that check reported - a value of the wrong type, and a value that
   * was absent - are unrepresentable here, so neither the check nor the exceptions it threw
   * are ported, and the reference data store needs no reflection to be sound.
   *
   * Construction cannot fail and both fields are already immutable values, so this is a
   * total type in the sense of the port's construction policy: the constructor, `apply` and
   * `copy` are all public.
   *
   * This type deliberately has no JSON codec. An entry is half of a heterogeneous store
   * whose value type is only known through its identifier, so no encoder could be written
   * for an arbitrary entry; reference data is excluded from the port's codec inventory for
   * the same reason, and the one kind of reference data that is serializable - a holiday
   * calendar - carries its own codec.
   *
   * @tparam T  the type of the reference data value
   * @param id  the identifier the value is held under
   * @param value  the reference data value
   */
  final case class Entry[T](id: ReferenceDataId[T], value: T)

  /**
   * Provides the instances for [[Entry]].
   *
   * Both instances are given for every value type rather than built from instances for it.
   * That is deliberate: entries of differing value types are held together in one store, so
   * an instance that demanded a `Hash[T]` or a `Show[T]` would be unavailable for exactly
   * the entries this type exists to carry. Universal hashing is the correct implementation
   * regardless, because the synthesised `equals` and `hashCode` of the case class delegate
   * to those of the identifier and the value, and an identifier is required by
   * [[ReferenceDataId]] to hash by value.
   *
   * There is no `Order` instance: entries of a heterogeneous store have no meaningful
   * ordering, and the store itself is a map keyed by identifier.
   */
  object Entry {

    /**
     * Returns the hash and equality instance for an entry of any value type.
     *
     * @tparam T  the type of the reference data value
     * @return the instance, hashing and comparing the identifier and the value
     */
    implicit def hash[T]: Hash[Entry[T]] = Hash.fromUniversalHashCode

    /**
     * Returns the string rendering instance for an entry of any value type.
     *
     * @tparam T  the type of the reference data value
     * @return the instance, rendering the entry as the case class does
     */
    implicit def show[T]: Show[Entry[T]] = Show.fromToString
  }

  /**
   * Obtains an instance from a set of reference data entries.
   *
   * Each entry is one item of reference data together with the identifier it is held under.
   * The result additionally includes the [[minimal]] set of reference data - the four
   * weekend and no-holiday calendars that are needed by this library and are not open to
   * disagreement - so that a caller supplying its own securities or calendars does not have
   * to remember to supply those as well. An entry whose identifier is one of the minimal
   * four '''overrides''' the built-in value, which is what allows a caller to substitute its
   * own definition of, say, the Saturday/Sunday calendar. To obtain a store holding nothing
   * but the entries given, use [[ImmutableReferenceData.of]].
   *
   * Construction fails if two entries are supplied for the same identifier, because the
   * caller's intent is then unknowable: silently keeping one of the two values would be a
   * guess. The Java original could not report this, since it took a `Map` in which the
   * ambiguity had already been resolved by whichever entry was inserted last. The failure
   * names every duplicated identifier, ordered by its rendering rather than by the order
   * they were supplied in, so the message is the same for the same set of entries however
   * they were arranged.
   *
   * @param entries  the reference data entries
   * @return the reference data holding the entries over the minimal set, or the failure
   *   describing the duplicated identifiers
   */
  def of(entries: Entry[_]*): Either[Failure, ReferenceData] =
    ImmutableReferenceData
      .of(entries: _*)
      .map(caller => ImmutableReferenceData.fromMap(minimal.values ++ caller.values))

  /**
   * Obtains an instance containing no reference data.
   *
   * Every lookup against the result is empty, including those for the [[minimal]]
   * calendars. It behaves as the identity of [[ReferenceData.combinedWith]] - combining it
   * with a set of reference data on either side answers exactly as that set does - and it
   * is the right starting point for a test that means to assert what happens when data is
   * absent.
   *
   * @return empty reference data
   */
  def empty: ReferenceData = ImmutableReferenceData.empty

  /**
   * Obtains an instance of standard reference data.
   *
   * Standard reference data is built into the library: it holds every built-in holiday
   * calendar - the rule-generated national calendars, the published Thai calendar, and the
   * four weekend and no-holiday calendars - keyed by its own identifier, and nothing else.
   * It is what makes the demo and the test suite runnable without a source of market data,
   * and production use of this library will generally supply its own calendars instead, or
   * layer them over this set with `myData.combinedWith(ReferenceData.standard)`.
   *
   * The value is computed once, on first use. That is not merely an optimisation: the
   * calendars are generated from rules, and this package and the `date` package are
   * mutually dependent by design - a holiday calendar identifier is a
   * [[ReferenceDataId]] defined there, while the built-in calendars are read from there
   * here - exactly as they were in the Java original. Computing this eagerly would make
   * the order in which the two packages happen to be initialised load-bearing.
   *
   * @return standard reference data, holding every built-in holiday calendar
   */
  lazy val standard: ImmutableReferenceData =
    ImmutableReferenceData.fromMap(storeOf(StandardHolidayCalendars.all))

  /**
   * Obtains the minimal set of reference data.
   *
   * Where [[standard]] holds every built-in calendar, the minimal set holds only those
   * identifiers this library itself needs and that no reasonable user would define
   * differently: the no-holidays calendar and the Saturday/Sunday, Friday/Saturday and
   * Thursday/Friday weekend calendars. Those four are what [[of]] adds underneath a
   * caller's own entries.
   *
   * The value is computed once, on first use, for the reason given on [[standard]].
   *
   * @return minimal reference data, holding the four weekend and no-holiday calendars
   */
  lazy val minimal: ImmutableReferenceData =
    ImmutableReferenceData.fromMap(storeOf(StandardHolidayCalendars.minimal))

  /**
   * Converts a set of identifier-to-value pairs into a reference data store.
   *
   * The parameter is an iterable of pairs rather than a map, which is what lets a
   * `Map[HolidayCalendarId, HolidayCalendar]` be passed straight in. A map of that type is
   * not a `Map[ReferenceDataId[_], Any]`, because a map is invariant in its key type, but it
   * is an iterable of its pairs and both `Iterable` and a pair are covariant, so the
   * conversion is ordinary subtyping and needs neither a cast nor a rebuild at the call
   * site. Two things follow, and both are the reason for writing it this way. This file
   * needs no knowledge of the holiday calendar types, which matters because the `date`
   * package depends on this one and not the other way about; and the erased key and value
   * types are declared here rather than inferred at each call site, so no call can widen a
   * type to `Any` by accident - something the build, which compiles warnings as errors,
   * would reject.
   *
   * @param values  the identifier-to-value pairs
   * @return the reference data store holding the pairs
   */
  private def storeOf(values: Iterable[(ReferenceDataId[_], Any)]): Map[ReferenceDataId[_], Any] =
    values.toMap

  /**
   * Returns the failure reported when an identifier is not found.
   *
   * Extracted from [[ReferenceData.getValue]] so that the message and the attribute are
   * written once, as the Java original extracted the same message for the same reason.
   *
   * @param id  the identifier that could not be found
   * @return the failure describing the missing reference data
   */
  private def notFound(id: ReferenceDataId[_]): Failure =
    Failure
      .MissingData(s"Reference data not found for identifier '$id'")
      .withAttribute("id", id.toString)
}

/**
 * An immutable set of reference data, held as a map of values by identifier.
 *
 * This is the standard implementation of [[ReferenceData]]: every value it can answer with
 * is present in the map it was built from, so a lookup is one map lookup and nothing is
 * computed, loaded or resolved on demand. `ReferenceData.standard`, `ReferenceData.minimal`
 * and the reference data assembled by an application from its own securities and calendars
 * are all values of this type.
 *
 * ===Construction is closed===
 *
 * The store can only be built through the factories of the companion, which is why the
 * primary constructor is private and neither `apply` nor `copy` exists - `values` cannot be
 * replaced on an existing instance, and a map cannot be handed in from outside the module.
 * The reason is the one thing this type has to guarantee: that every value in the map has
 * the type its identifier promises. A caller supplies entries as
 * [[ReferenceData.Entry]] values, which can pair an identifier only with a value of its own
 * type, and the guarantee then holds by construction rather than by a check.
 *
 * Pattern matching still works, so the store can be taken apart where that is useful -
 * [[combinedWith]] does exactly that - and equality is the structural equality of the case
 * class, comparing the two maps entry by entry regardless of the order they were built in.
 *
 * ===Divergences from the type being ported===
 *
 * The Java original was a Joda bean: it validated each entry by asking the identifier for
 * the `Class` of the data it referred to and testing the value against it, and it was
 * serializable in both the Joda-Beans and the Java-serialization senses. None of that is
 * ported. The type check is unnecessary for the reason given above, and this type has no
 * JSON codec because its store is heterogeneous: the value type of an entry is known only
 * through its identifier, so no encoder for an arbitrary store can exist. The one kind of
 * reference data this library does serialize - a holiday calendar - carries its own codec,
 * so a store can be rebuilt from serialized calendars by a caller that knows which
 * identifiers it expects.
 *
 * No typeclass instances are declared for this type. It is a container of reference data
 * rather than a value of the domain, and the structural `equals` of the case class is what
 * compares two stores.
 *
 * @param values  the reference data values, keyed by the identifier each is held under
 */
sealed abstract case class ImmutableReferenceData private (values: Map[ReferenceDataId[_], Any])
    extends ReferenceData {

  /**
   * Finds the reference data value associated with the specified identifier.
   *
   * @tparam T  the type of the reference data value
   * @param id  the identifier to find
   * @return the reference data value, empty if this store holds none for the identifier
   */
  override def findValue[T](id: ReferenceDataId[T]): Option[T] =
    // This is the one cast in this module, and it is sound rather than merely convenient.
    // The store is keyed by a type-erased identifier because it holds values of many
    // types at once, but a value can only have entered it through a
    // `ReferenceData.Entry[T]` - or through `of(id, value)`, which has the same shape - and
    // both bind the type of the value to the type parameter of the identifier it is filed
    // under. So the value found under an identifier of type `ReferenceDataId[T]` is a `T`,
    // which is what the Java original established at construction time by testing the
    // value against a `Class` obtained from the identifier. Removing that reflective check
    // is the point of the `Entry` type; see the port's construction policy.
    values.get(id).map(_.asInstanceOf[T])

  /**
   * Combines this reference data with another.
   *
   * Where the other set of reference data is also a materialised store, the two maps are
   * merged into a single store rather than chained, so a lookup against the result stays one
   * map lookup however many times reference data has been combined. The entries of this
   * store win a clash, which is the same preference the general implementation on
   * [[ReferenceData]] expresses by consulting this side first; the merge is written in that
   * direction - the other side's entries first, then this side's over them - for exactly
   * that reason.
   *
   * Any other implementation is combined the general way, by wrapping both in a
   * [[CombinedReferenceData]], because there is no map to merge: the other side computes its
   * answers, and the whole point of an implementation such as `HolidaySafeReferenceData` is
   * that it answers for identifiers no finite map could enumerate.
   *
   * @param other  the other reference data
   * @return the combined reference data, preferring the values of this one
   */
  override def combinedWith(other: ReferenceData): ReferenceData =
    other match {
      case ImmutableReferenceData(otherValues) => ImmutableReferenceData.fromMap(otherValues ++ values)
      case _ => super.combinedWith(other)
    }
}

/**
 * Provides the ways of building an immutable set of reference data.
 *
 * Unlike `ReferenceData.of`, none of these factories adds the `ReferenceData.minimal`
 * calendars: the store holds exactly what it was given. That is the distinction the Java
 * original drew between the two `of` methods, and it is what makes this the right factory
 * for a test that means to assert that an identifier is absent, and the wrong one for
 * assembling the reference data of a running application.
 */
object ImmutableReferenceData {

  /** The empty store, holding no reference data at all, shared by every caller that asks. */
  val empty: ImmutableReferenceData = fromMap(Map.empty)

  /**
   * Obtains an instance from a set of reference data entries.
   *
   * The store holds exactly the entries supplied, and nothing else; use `ReferenceData.of`
   * to obtain a store that also includes the minimal set of built-in calendars.
   *
   * Construction fails if two entries are supplied for the same identifier, naming every
   * duplicated identifier ordered by its rendering, so that the failure is determined by the
   * set of entries and not by the order they arrived in. Supplying no entries is not a
   * failure and yields a store equal to [[empty]].
   *
   * @param entries  the reference data entries
   * @return the reference data holding exactly the entries, or the failure describing the
   *   duplicated identifiers
   */
  def of(entries: ReferenceData.Entry[_]*): Either[Failure, ImmutableReferenceData] = {
    val pairs: Seq[(ReferenceDataId[_], Any)] =
      entries.map(entry => (entry.id: ReferenceDataId[_]) -> (entry.value: Any))
    val duplicates: List[String] = pairs
      .groupBy { case (id, _) => id }
      .collect { case (id, group) if group.sizeIs > 1 => id.toString }
      .toList
      .sorted
    if (duplicates.isEmpty) {
      Right(fromMap(pairs.toMap))
    } else {
      val rendered = duplicates.mkString(", ")
      Left(
        Failure
          .Invalid(s"Duplicate reference data identifiers: $rendered")
          .withAttribute("duplicateIds", rendered))
    }
  }

  /**
   * Obtains an instance from a single reference data entry.
   *
   * This cannot fail - one entry cannot duplicate an identifier, and the identifier fixes
   * the type of the value - so it returns the store directly. It is primarily of interest
   * to test cases, which is what the Java original said of the method it mirrors:
   *
   * {{{
   * val refData = ImmutableReferenceData.of(HolidayCalendarIds.GBLO, calendar)
   * }}}
   *
   * As with the varargs factory, the store holds nothing but this entry.
   *
   * @tparam T  the type of the reference data value
   * @param id  the identifier the value is held under
   * @param value  the reference data value
   * @return the reference data holding exactly this entry
   */
  def of[T](id: ReferenceDataId[T], value: T): ImmutableReferenceData =
    fromMap(Map((id: ReferenceDataId[_]) -> (value: Any)))

  /**
   * Obtains an instance directly from a store of values by identifier.
   *
   * This is the one place an instance is created, and it is deliberately not public: the
   * type-safety of the store rests on values having entered it through
   * [[ReferenceData.Entry]], and a caller able to hand in a raw map of type-erased
   * identifiers to values could file a value under an identifier of another type. It is
   * visible within this module because the module's own store-building - the built-in
   * calendar sets, the seeding performed by `ReferenceData.of`, and the merge performed by
   * [[ImmutableReferenceData.combinedWith]] - assembles maps whose entries have already been
   * paired correctly at the point they were created.
   *
   * @param values  the reference data values, keyed by the identifier each is held under
   * @return the reference data holding the values
   */
  private[basics] def fromMap(values: Map[ReferenceDataId[_], Any]): ImmutableReferenceData =
    new ImmutableReferenceData(values) {}
}

/**
 * A set of reference data which combines the data from two other [[ReferenceData]] instances.
 *
 * When an item of data is requested the two underlying sets are consulted in order: if the
 * first holds a value for the identifier it is returned, and only otherwise is the second
 * asked. Neither set is copied and no merged store is built, so combining is cheap however
 * large the two sides are, and a value added to a mutable source behind either side - a
 * database-backed implementation, say - is visible through the combination.
 *
 * The intended way to obtain an instance is `ReferenceData.combinedWith`, which is where the
 * preference for the left-hand side is documented as part of that method's contract:
 *
 * {{{
 * val refData = myData.combinedWith(ReferenceData.standard)
 * }}}
 *
 * Constructing one directly is equivalent and is what a test asserting the combination
 * itself does. The Java original made this type package-private and accessible only through
 * that method; the Scala type is public because a case class synthesises a public `apply`
 * from a private constructor regardless, so hiding it would take a hand-written companion
 * for no benefit - the type has no invariant to protect, both fields being immutable
 * reference data.
 *
 * Equality is the structural equality of the case class, comparing the two underlying sets
 * in order, so combining the same two sets the other way round gives an unequal - and
 * behaviourally different - value. As with the other reference data implementations, this
 * type has no JSON codec: it holds two heterogeneous stores.
 *
 * @param refData1  the first set of reference data, whose values are preferred
 * @param refData2  the second set of reference data, consulted for identifiers the first
 *   does not hold
 */
final case class CombinedReferenceData(refData1: ReferenceData, refData2: ReferenceData) extends ReferenceData {

  /**
   * Checks if either underlying set of reference data contains a value for the identifier.
   *
   * The second set is asked only when the first does not hold the identifier, so a source
   * whose membership test is expensive is not consulted needlessly. This mirrors the
   * `containsValue` of the Java original, which short-circuited for the same reason.
   *
   * @param id  the identifier to find
   * @return true if either underlying set contains a value for the identifier
   */
  override def containsValue(id: ReferenceDataId[_]): Boolean =
    refData1.containsValue(id) || refData2.containsValue(id)

  /**
   * Finds the reference data value, preferring the first underlying set.
   *
   * The lookup against the second set is evaluated only if the first comes back empty,
   * `orElse` taking its argument by name; that is the same short circuit the Java original
   * expressed by testing whether its first result was present.
   *
   * @tparam T  the type of the reference data value
   * @param id  the identifier to find
   * @return the reference data value, empty if neither underlying set holds one for the
   *   identifier
   */
  override def findValue[T](id: ReferenceDataId[T]): Option[T] =
    refData1.findValue(id).orElse(refData2.findValue(id))
}
