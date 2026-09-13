/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

/**
 * A test-only [[ReferenceDataId]], equal by its [[id]] alone because a store is a map keyed
 * by identifier.
 *
 * @param id  the identifier, which alone determines equality
 */
final case class TestingReferenceDataId(id: String) extends ReferenceDataId[java.lang.Number] {

  /**
   * The witness by which a store recognises a value this identifier may answer with.
   *
   * One shared witness rather than one per instance: this identifier refers to a
   * `java.lang.Number` and to nothing else.
   */
  override def valueType: ReferenceDataType[java.lang.Number] = TestingReferenceDataId.number

  override def toString: String = s"TestingReferenceDataId [id=$id]"
}

object TestingReferenceDataId {

  implicit val number: ReferenceDataType[java.lang.Number] =
    ReferenceDataType.of("java.lang.Number") { case value: java.lang.Number => value }
}

/**
 * A test-only [[ReferenceDataId]] family parameterized in the type of data it refers to.
 *
 * Two instantiations built from one `id` compare equal - equality is the first parameter
 * list and the type argument is erased - so a store keys both to one entry, while the
 * witness each carries differs, which is what retrieval tells them apart by.
 *
 * @tparam A  the type of reference data this identifier refers to
 * @param id  the identifier, which alone determines equality
 * @param valueType  the witness for `A`, supplied per instantiation because a type parameter
 *   has no single witness to name
 */
final case class GenericTestingReferenceDataId[A](id: String)(implicit val valueType: ReferenceDataType[A])
    extends ReferenceDataId[A] {

  override def toString: String = s"GenericTestingReferenceDataId [id=$id, valueType=$valueType]"
}

/**
 * The two witnesses [[GenericTestingReferenceDataId]] is instantiated with.
 *
 * `String` and `Int` are unrelated, so neither recognises a value filed under the other -
 * where a wider witness, `java.lang.Number` over `Int`, legitimately would.
 */
object GenericTestingReferenceDataId {

  implicit val text: ReferenceDataType[String] =
    ReferenceDataType.of("String") { case value: String => value }

  implicit val count: ReferenceDataType[Int] =
    ReferenceDataType.of("Int") { case value: Int => value }
}
