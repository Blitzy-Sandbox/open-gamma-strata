/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import cats.data.Kleisli

import com.opengamma.strata.basics.date.HolidayCalendar
import com.opengamma.strata.collect.FailureOr
import com.opengamma.strata.collect.result.Failure

/**
 * An identifier for a unique item of reference data.
 *
 * Reference data - holiday calendars, securities and the like - is obtained from an instance
 * of [[ReferenceData]] using an identifier of this type. The identifier is parameterized with
 * the type of the data it refers to, so a `ReferenceDataId[HolidayCalendar]` yields a holiday
 * calendar and nothing else. That parameter is what makes a lookup type-safe at the call site:
 * neither the identifier nor the reference data has to be asked what it holds.
 *
 * ===The contract an implementation must honour===
 *
 * An implementation of this trait is an identity, not a container: it carries just enough
 * information to name one item of reference data, and the witness for the type of data it
 * names. Three consequences follow, and each is a requirement rather than a recommendation.
 *
 * Implementations must be immutable and thread-safe, because a single identifier is shared
 * freely across threads and calculations.
 *
 * Implementations must supply value-based `equals` and `hashCode`. [[ReferenceData]] holds its
 * entries in a map keyed by identifier, so an identifier that inherits reference equality can
 * never find the value stored under an equal-but-not-identical instance. Declaring the
 * implementation a `final case class` satisfies this for free, which is what the identifier
 * families of this module and the test fixtures do; anything that cannot be a case class must
 * write the two methods by hand. The Java original relied on exactly the same contract.
 *
 * Implementations must supply a [[valueType]] that recognises every value the identifier is
 * willing to be answered with, and nothing else. Reference data narrows what it finds with
 * that witness, so a witness too narrow makes a legitimate value unreachable through the
 * identifier, and one too wide gives up the guarantee this member exists for. An identifier
 * of a fixed type of data names the witness for that type; a family parameterized in its
 * value type demands one per instantiation.
 *
 * The trait is deliberately open rather than sealed. Reference data is an extension point of
 * this library: an identifier family such as `HolidayCalendarId` lives in its own file, and
 * applications and tests define identifiers of their own for data this module never sees.
 * Sealing the trait would make both impossible.
 *
 * ===Implementing it is nearly free===
 *
 * Two of the three members below have a default implementation, expressed in terms of
 * [[ReferenceData]] alone, so an identifier that is no more than an identity implements one
 * member: the [[ReferenceDataType]] witness naming the type of value it addresses.
 *
 * {{{
 * final case class TestingReferenceDataId(id: String) extends ReferenceDataId[Number] {
 *   override def valueType: ReferenceDataType[Number] = TestingReferenceDataId.number
 * }
 *
 * object TestingReferenceDataId {
 *   val number: ReferenceDataType[Number] =
 *     ReferenceDataType.of("java.lang.Number") { case value: Number => value }
 * }
 * }}}
 *
 * A family with a richer resolution rule overrides `resolve` and inherits the rest.
 * `HolidayCalendarId` does precisely that, because a composite identifier such as
 * `GBLO+USNY` is first looked up whole - letting a host supply one pre-combined calendar -
 * and only then resolved component by component.
 *
 * ===The Java runtime type token is replaced rather than dropped===
 *
 * The accessor that reported the runtime type of the data - `getReferenceDataType`, which
 * returned a `Class` - has [[valueType]] as its counterpart. The token itself is not ported,
 * because reading a value's class at run time is the reflection this migration removes; the
 * witness recognises a value with an ordinary pattern match instead, which the compiler emits
 * as a type test and no reflective call.
 *
 * Keeping a counterpart at all is load-bearing, and the reason is worth stating, because the
 * closed construction path of `ImmutableReferenceData` looks at first like enough on its own.
 * Reference data can only enter a store as a `ReferenceData.Entry[T]`, or through a factory
 * whose key type is an identifier of its value type, so a value is never '''filed''' under an
 * identifier of another type. That much is settled by the compiler, and it remains so. It does
 * not settle what a lookup '''finds''': a store is keyed by an identifier whose type argument
 * is erased, a map lookup compares keys with ordinary `equals`, and this trait is open. A
 * family parameterized in its value type is therefore legal - and is what a host writes as
 * soon as it names data of more than one type - and its instantiations are one key:
 * `GenericId[String]("x")` and `GenericId[Int]("x")` are equal values, one case class over one
 * field with the type argument gone. A lookup made with the second would be answered with the
 * `String` the first filed. [[valueType]] is what closes that: a family parameterized in its
 * value type cannot supply the witness without demanding one per instantiation, so the two
 * identifiers above carry different witnesses even though they compare equal, and
 * `ImmutableReferenceData.findValue` checks the value it found against the witness of the
 * identifier that asked for it.
 *
 * ===One member of the Java interface is deliberately absent===
 *
 * The low-level query primitive - `queryValueOrNull` - is not ported. It signalled the
 * absence of a value by returning a reference to nothing, a convention this port does not use
 * anywhere: an absence is an `Option` returned by `ReferenceData.findValue`, and a failure to
 * resolve is a `Left`. The method therefore has no counterpart.
 *
 * This trait has no JSON codec, and neither has any identifier family other than
 * `HolidayCalendarId`. An identifier is a lookup token rather than serializable data, and the
 * reference data it addresses is excluded from the port's codec inventory for the same reason.
 *
 * @tparam T the type of the reference data this identifier refers to
 */
trait ReferenceDataId[T] {

  /**
   * The witness by which a store recognises a value this identifier may be answered with.
   *
   * This is the counterpart of the Java `getReferenceDataType()`, without its `Class` token:
   * a [[ReferenceDataType]] carries the pattern that recognises a value of `T`, so
   * `ImmutableReferenceData.findValue` can check the value it found against the identifier
   * that asked for it and never hand back a value of another type. The check costs one type
   * test, and no reflection is involved at any point.
   *
   * The member is abstract deliberately, and it is the one member of this trait that an
   * identifier must implement. An identifier of a fixed value type names its witness
   * directly, as `HolidayCalendarId` names [[ReferenceDataType.holidayCalendar]]. A family
   * '''parameterized''' in its value type cannot do that - there is no single witness to
   * name - so it has to demand one per instantiation, typically as an implicit constructor
   * parameter, and that is precisely what makes the two instantiations of such a family
   * distinguishable at retrieval time even though case class equality, which sees only the
   * fields, reports them as one key. Nothing here relies on the type argument surviving
   * erasure, because the witness is a value the identifier carries.
   *
   * Two identifiers that are equal are expected to carry equal witnesses. Where they do not -
   * a generic family instantiated twice, which is the case this member exists for - the
   * disagreement is resolved in favour of the identifier that asked: a lookup made with one
   * instantiation reports the value filed by the other as absent rather than returning it
   * mistyped, and `ReferenceData.combinedWith` then goes on to consult the next source, which
   * is how a store holding the value of the requested type still answers.
   *
   * @return the witness naming, and recognising, the type of value this identifier refers to
   */
  def valueType: ReferenceDataType[T]

  /**
   * Resolves this identifier against the supplied reference data.
   *
   * This is the ordinary way to obtain a value: the identifier is asked to find itself in the
   * data it is given, so reference data is threaded explicitly through every call that needs
   * it rather than read from ambient state.
   *
   * A missing value is reported, not thrown. Where the Java original raised a
   * `ReferenceDataNotFoundException`, this method returns
   * `Left(Failure.MissingData(...))` describing the identifier that could not be found, which
   * leaves the decision of what to do about it with the caller that has the context to make
   * it. The exception type is not ported.
   *
   * The default implementation delegates to `ReferenceData.getValue`, which is correct for any
   * identifier that is simply an identity. A family whose resolution involves more than a
   * single map lookup - a composite holiday calendar identifier, for instance - overrides
   * this method; because `toReader` is defined in terms of `resolve`, such an override is
   * picked up there as well.
   *
   * @param refData the reference data to resolve this identifier against
   * @return the reference data value, or the failure explaining why it could not be resolved
   */
  def resolve(refData: ReferenceData): Either[Failure, T] =
    refData.getValue(this)

  /**
   * Expresses resolution of this identifier as a function awaiting reference data.
   *
   * `resolve` needs its reference data at the moment it is called. This method returns the
   * same resolution as a value - a `Kleisli` over `FailureOr` - so that several lookups and
   * adjustments can be composed with `map`, `flatMap` and `mapN` while the data is still
   * unknown, and the composed reader is then run once against the reference data actually
   * available:
   *
   * {{{
   * val both = (calendarId.toReader, otherId.toReader).tupled
   * val resolved = both.run(ReferenceData.standard)
   * }}}
   *
   * The `Kleisli` type arguments are spelled out rather than inferred, over the single-parameter
   * `FailureOr` alias. That alias exists for this position: the build carries no compiler plugin
   * supplying type-lambda syntax, so a failure type applied at the use site could not be written
   * here at all.
   *
   * @return the resolution of this identifier as a function from reference data to the value
   */
  def toReader: RefDataReader[T] =
    Kleisli[FailureOr, ReferenceData, T](resolve)
}

/**
 * The type of value an identifier refers to, carried as a witness rather than as a class.
 *
 * A store of reference data holds values of many types at once, so it is keyed by an
 * identifier whose type argument is erased and its values are erased along with them. This
 * type is what a lookup narrows a value with on the way out: an identifier carries the witness
 * for its own value type - see [[ReferenceDataId.valueType]] - and
 * `ImmutableReferenceData.findValue` asks it to recognise the value it found, so a value can
 * only be handed to a caller as the type that caller's identifier promised.
 *
 * ===Why this exists at all===
 *
 * The type argument of an identifier fixes the type of the value it can be '''filed''' with,
 * because a value enters a store only as a `ReferenceData.Entry[T]`. It does not fix the type
 * of the value a lookup '''finds''', because a map lookup compares keys with ordinary
 * `equals` and [[ReferenceDataId]] is open: a family parameterized in its value type, which
 * the trait permits and hosts write, compares equal across two instantiations once the type
 * argument is erased. The witness recovers the missing half of the contract, at the one place
 * the value is handed over.
 *
 * ===Why it is not a class token===
 *
 * The Java original solved the same problem with `Class<T>` and `Class.isInstance`, which is
 * reflection, and AAP decision D-5 and its Rule 6 hold that nothing on the path that reads or
 * writes data may reflect. A witness is built from the pattern that recognises its value
 * instead - `{ case value: HolidayCalendar => value }` - which the compiler emits as a type
 * test. There is no `Class`, no `ClassTag`, no `getClass` and no name to resolve, and a
 * witness can recognise what no single class could: a value type expressed as a union of
 * patterns, or one that needs a predicate as well as a type test.
 *
 * Recognition is by pattern, so it is as wide as the pattern is. A witness for
 * `java.lang.Number` recognises a boxed integer, which is correct - a boxed integer '''is''' a
 * number, and an identifier of numbers may be answered with one - and it is the reason the
 * narrowing is expressed as a pattern the author of the witness writes rather than as an
 * equality of types. An implementation supplying a witness therefore states which values its
 * identifier is willing to be answered with.
 *
 * ===Equality, and what a name is for===
 *
 * Two witnesses are equal when they carry the same name, and a name is expected to identify
 * the value type - `"HolidayCalendar"`, `"java.lang.Number"` - so that two independently
 * constructed witnesses for one type are one value, as the `Class` tokens they replace were.
 * The narrowing itself takes no part in equality, a function having no useful equality of its
 * own. A name is also what a witness renders as, which is what makes it readable in a
 * diagnostic.
 *
 * Instances are immutable and thread-safe, as an identifier that carries one must be, and are
 * normally declared once as a `val` and shared.
 *
 * @tparam T  the type of the reference data value this witness recognises
 * @param name  the name identifying the value type, which alone determines equality
 * @param narrowing  the recognising function, applied to a value of unknown type
 * @see [[ReferenceDataId.valueType]] for the member that carries a witness
 */
final class ReferenceDataType[T] private (val name: String, private val narrowing: Any => Option[T]) {

  /**
   * Narrows a value of unknown type to the type this witness recognises.
   *
   * This is the whole of what a witness does. The value comes from a store that holds values
   * of many types, and the answer is the value at the type this witness names, or empty where
   * the value is of some other type - which is how `ImmutableReferenceData.findValue` reports
   * a value filed by an equal identifier of another value type as absent rather than handing
   * it back mistyped.
   *
   * The cost is the type test of the pattern the witness was built from, so this sits on the
   * lookup path of every adjustment, schedule and observation without allocating beyond the
   * `Option` it returns.
   *
   * @param value  the value to narrow, of a type not known to the caller
   * @return the value at the type this witness names, empty where it is of another type
   */
  def narrow(value: Any): Option[T] = narrowing(value)

  /**
   * Checks if this witness names the same value type as another object.
   *
   * Equality is the name alone, for the reason given on this class: two witnesses built
   * independently for one value type are one value, and the narrowing functions they carry
   * have no equality of their own to compare.
   *
   * @param obj  the other object
   * @return true if the other object is a witness carrying the same name
   */
  override def equals(obj: Any): Boolean =
    obj match {
      case other: ReferenceDataType[_] => name == other.name
      case _ => false
    }

  /**
   * Returns a hash code consistent with [[equals]].
   *
   * It is the hash of the name, so witnesses that are equal hash alike and a witness can be
   * held in a set or used as a key.
   *
   * @return the hash code
   */
  override def hashCode: Int = name.hashCode

  /**
   * Renders this witness as the name of the value type it recognises.
   *
   * @return the name, such as `HolidayCalendar`
   */
  override def toString: String = name
}

/**
 * Provides the way of building a value type witness, and the witnesses this module needs.
 *
 * One factory and one witness: [[of]] builds a witness from the pattern that recognises its
 * value, and [[holidayCalendar]] is the witness every `HolidayCalendarId` carries, holiday
 * calendars being the one kind of reference data this module itself defines. An application
 * that defines reference data of its own declares its witnesses the same way, beside the
 * identifiers that carry them.
 */
object ReferenceDataType {

  /**
   * Obtains a witness from the pattern that recognises its value type.
   *
   * The pattern is an ordinary partial function, so it is written as the type test it is and
   * the compiler emits it as one:
   *
   * {{{
   * val holidayCalendar: ReferenceDataType[HolidayCalendar] =
   *   ReferenceDataType.of("HolidayCalendar") { case calendar: HolidayCalendar => calendar }
   * }}}
   *
   * The function is lifted once, when the witness is built, rather than at each narrowing, so
   * a witness costs one allocation however many lookups it goes on to check.
   *
   * The name is what equality and rendering use, so it should identify the value type and not
   * the identifier family that carries the witness: two witnesses for one value type declared
   * in two places are meant to be equal.
   *
   * @tparam T  the type of the reference data value
   * @param name  the name identifying the value type, which alone determines equality
   * @param recognise  the pattern recognising a value of that type among values of any type
   * @return the witness
   */
  def of[T](name: String)(recognise: PartialFunction[Any, T]): ReferenceDataType[T] =
    new ReferenceDataType[T](name, recognise.lift)

  /**
   * The witness for a holiday calendar, which is the reference data this module defines.
   *
   * It is what every `HolidayCalendarId` answers with, including the composite identifiers,
   * because a composite resolves to a calendar exactly as a simple identifier does. It is
   * implicit so that a generic identifier family instantiated at `HolidayCalendar` - the shape
   * described on [[ReferenceDataId.valueType]] - finds it without being handed it.
   */
  implicit val holidayCalendar: ReferenceDataType[HolidayCalendar] =
    of("HolidayCalendar") { case calendar: HolidayCalendar => calendar }
}
