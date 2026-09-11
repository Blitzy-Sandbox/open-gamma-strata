/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

/**
 * A [[ReferenceDataId]] used by the tests of this module, and nothing else.
 *
 * The identifier families this library ships - `HolidayCalendarId` above all - each carry a
 * resolution rule of their own, which makes every one of them a poor subject for a test of
 * [[ReferenceData]] itself: a lookup that fails could have failed in the store or in the
 * identifier. This fixture is the identifier with no rule at all. It names an item of
 * reference data with a single string and implements not one member of the trait, so a spec
 * that uses it observes the behaviour of the reference data alone.
 *
 * It also stands in for the identifiers an application defines for its own data, which is why
 * it lives in the root test package rather than beside any one spec: the trait is open
 * precisely so that a host can do this, and the fixture is the proof that a host needs to
 * write no more than the line below.
 *
 * ===Why it is a case class===
 *
 * [[ReferenceData]] holds its entries in an immutable map keyed by identifier, so an
 * identifier that inherited reference equality could never find the value stored under an
 * equal-but-not-identical instance. The Java original wrote `equals` and `hashCode` by hand
 * over its single field - `Objects.equals(id, that.id)` and `Objects.hash(id)` - and a case
 * class is the same contract, synthesised: two instances are equal exactly when their ids
 * are, equal instances hash alike, and the class is `final` so no subclass can weaken
 * either. Every consuming spec depends on that, since each builds two or more identifiers
 * from distinct ids and expects them to address distinct entries.
 *
 * ===Why the value type is `java.lang.Number` and the class is not generic===
 *
 * The Java fixture is a `ReferenceDataId<Number>`, and the tests that use it store boxed
 * numbers under it. Parameterising the class in the value type instead would multiply the
 * identifier into a family whose members differ in a type argument that never varies in any
 * spec, and would let two identifiers built from the same id be unequal - the one property
 * the fixture exists to guarantee. It stays a single, non-generic identifier of boxed
 * numbers; a spec that wants to store `1` or `123d` under it boxes the value at the call
 * site, which is where that noise belongs.
 *
 * ===Its consumers===
 *
 * Four specs use it, and its shape is fixed by all four together:
 *
 *   - `ReferenceDataSpec` - identifiers `"1"`, `"2"` and `"3"` paired with boxed numbers, to
 *     exercise `findValue`, `getValue`, `containsValue` and combination;
 *   - `CombinedReferenceDataSpec` - identifiers `"1"` to `"4"`, to exercise which side of a
 *     combination wins a clash;
 *   - `date.HolidayCalendarsSpec` - to confirm that `ReferenceData.standard` holds calendars
 *     and nothing else, by asking it for an identifier that is not a calendar identifier;
 *   - `date.HolidaySafeReferenceDataSpec` - as the non-calendar identifier that proves the
 *     weekend-only defaulting applies to a `HolidayCalendarId` alone and never to an
 *     arbitrary identifier.
 *
 * ===Two members of the Java fixture are deliberately absent===
 *
 * The Java class implemented `Serializable` and declared a `serialVersionUID`. Java
 * serialization is supported by no type in this port, so there is nothing for the fixture to
 * take part in.
 *
 * It also overrode `getReferenceDataType`, returning the runtime class of the data it refers
 * to so that a reference data implementation could check a stored value against its
 * identifier at run time. That check is performed at compile time here, by
 * `ReferenceData.Entry[T]`, which cannot pair an identifier with a value of the wrong type in
 * the first place; carrying a runtime type token again would reintroduce the reflection this
 * migration removes.
 *
 * @param id  the identifier, which alone determines equality
 */
final case class TestingReferenceDataId(id: String) extends ReferenceDataId[java.lang.Number] {

  /**
   * Renders this identifier in the form the Java fixture used.
   *
   * The synthesised rendering of a case class would read `TestingReferenceDataId(1)`. The
   * Java form is kept instead because it is what a failure message shows: a value that
   * cannot be resolved is reported as a `Failure.MissingData` naming the identifier through
   * this method, and a reader comparing such a message with the Java test it was ported from
   * should find the same text.
   *
   * @return the identifier in the form `TestingReferenceDataId [id=1]`
   */
  override def toString: String = s"TestingReferenceDataId [id=$id]"
}
