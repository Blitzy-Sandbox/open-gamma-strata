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
 * The generic shape is not therefore untested. It is a legal shape of the open contract, and
 * the one the contract's retrieval check exists for, so it has a fixture of its own in this
 * file - [[GenericTestingReferenceDataId]] - used by the two specs that assert what a store
 * does when two equal identifiers refer to different types of value.
 *
 * ===Its consumers===
 *
 * Four specs use it, and its shape is fixed by all four together:
 *
 *   - `ReferenceDataSpec` - identifiers `"1"`, `"2"` and `"3"` paired with boxed numbers, to
 *     exercise the whole reading surface - `findValue`, `getValue` and `containsValue` - over the
 *     four factories, the combination of two sets of reference data, and - this fixture
 *     implementing no member of the trait - the inherited resolution path through the identifier
 *     itself, `ReferenceDataId.resolve` and the reader `toReader` returns;
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
 * identifier at run time. The counterpart here is [[valueType]], which carries the '''pattern'''
 * that recognises a boxed number rather than its class: filing is checked at compile time by
 * `ReferenceData.Entry[T]`, which cannot pair an identifier with a value of the wrong type in
 * the first place, and retrieval is checked by that witness, neither of them by reflection.
 *
 * @param id  the identifier, which alone determines equality
 */
final case class TestingReferenceDataId(id: String) extends ReferenceDataId[java.lang.Number] {

  /**
   * The witness by which reference data recognises a value this identifier may answer with.
   *
   * The single shared witness of the companion, for the reason the class is not generic: this
   * fixture refers to a `java.lang.Number` and to nothing else, so there is one witness to
   * name and every instance names it.
   *
   * @return the witness for a boxed number
   */
  override def valueType: ReferenceDataType[java.lang.Number] = TestingReferenceDataId.number

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

/**
 * Provides the value type witness every instance of the fixture carries.
 *
 * It is declared once, here, rather than built per instance: a witness is immutable, equality
 * over witnesses is by name, and an identifier is built in almost every test of this package,
 * so there is no reason for each to allocate one.
 */
object TestingReferenceDataId {

  /**
   * The witness for the boxed numbers this fixture's identifiers refer to.
   *
   * The pattern recognises any `java.lang.Number`, which is what the fixture's value type
   * says: a spec storing `Int.box(1)` or `Double.box(123d)` under one of these identifiers
   * reads that value back, because a boxed integer and a boxed double are both numbers. It is
   * implicit so that [[GenericTestingReferenceDataId]] instantiated at `java.lang.Number`
   * finds it without being handed it.
   */
  implicit val number: ReferenceDataType[java.lang.Number] =
    ReferenceDataType.of("java.lang.Number") { case value: java.lang.Number => value }
}

/**
 * The identifier family an application writes when it names data of more than one type.
 *
 * [[TestingReferenceDataId]] is the fixture for reference data of a '''fixed''' value type,
 * and it is the one the specs of this package use for everything else. This is the other
 * shape the open contract permits, and it exists because that shape is where the contract is
 * load-bearing: an identifier parameterized in its value type has, after erasure, nothing on
 * it that distinguishes two instantiations. `GenericTestingReferenceDataId[String]("shared")`
 * and `GenericTestingReferenceDataId[Int]("shared")` are '''equal''' values - case class
 * equality is the first parameter list, the type argument is gone, and a reference data store
 * is a map keyed by identifier - so a store holding a `String` under the first would answer
 * the second with that `String` if retrieval were not checked.
 *
 * Checked it is, by the witness this family demands per instantiation: the value type is a
 * type parameter, so there is no single witness to name and the only way to supply
 * `ReferenceDataId.valueType` is to ask for one, which is exactly why that member is abstract.
 * The two instantiations above therefore carry '''different''' witnesses while comparing
 * equal, and `ReferenceDataSpec` and `CombinedReferenceDataSpec` assert what a lookup does
 * with that: it reports the value of the other type as absent, and a combination goes on to
 * find the value of the type actually asked for in the next store.
 *
 * It lives beside [[TestingReferenceDataId]], in the root test package, because both specs
 * that assert the property need it and because - like that fixture - it stands in for what a
 * host writes rather than for anything this library ships.
 *
 * @tparam A  the type of the reference data this identifier refers to
 * @param id  the identifier, which alone determines equality - deliberately, since that is
 *   the property under test
 * @param valueType  the witness for the value type, supplied per instantiation
 */
final case class GenericTestingReferenceDataId[A](id: String)(implicit val valueType: ReferenceDataType[A])
    extends ReferenceDataId[A] {

  /**
   * Renders this identifier with its value type as well as its id.
   *
   * The value type is included because two of these compare equal while referring to
   * different types of data, so a rendering that showed the id alone would make a failure
   * message about one of them unreadable. Equality is unaffected: it is the id alone, which
   * is the property the specs assert.
   *
   * @return the identifier in the form `GenericTestingReferenceDataId [id=shared, valueType=String]`
   */
  override def toString: String = s"GenericTestingReferenceDataId [id=$id, valueType=$valueType]"
}

/**
 * Provides the value type witnesses the generic fixture is instantiated with.
 *
 * Two are needed, and two is the minimum that states the property: one identifier family
 * instantiated at two unrelated value types, so that a value filed under one instantiation is
 * recognised by neither the other's witness nor any widening of it. `String` and `Int` are
 * unrelated in exactly that way - neither is the other, and a boxed integer is not a string -
 * which is why the specs use this pair rather than, say, `Int` and `java.lang.Number`, where
 * the wider witness legitimately recognises the narrower value.
 *
 * Both are implicit, since that is how the family takes them, and a spec brings them into
 * scope by importing them.
 */
object GenericTestingReferenceDataId {

  /** The witness for reference data that is text. */
  implicit val text: ReferenceDataType[String] =
    ReferenceDataType.of("String") { case value: String => value }

  /** The witness for reference data that is a count, which boxes to an `Integer`. */
  implicit val count: ReferenceDataType[Int] =
    ReferenceDataType.of("Int") { case value: Int => value }
}
