/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.value

import cats.Hash
import cats.Show

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder

import com.opengamma.strata.collect.array.DoubleArray
import com.opengamma.strata.collect.json.Codecs

/**
 * A value and its derivatives.
 *
 * This defines a standard way to return a value and its derivatives to certain inputs. It is
 * in particular used as a return object for Algorithmic Differentiation versions of some
 * functions.
 *
 * ===Construction===
 *
 * Construction is total. The value may be any double the caller holds, including one that is
 * not a number and the two infinities, and the derivatives are an already-immutable array, so
 * there is nothing for a factory to reject: the only check the Java original made was that the
 * array reference was present, a state the types here cannot express. The primary constructor,
 * `apply` and `copy` are therefore all public, and `of` exists alongside them purely so that
 * ported call sites read as they did before. Both fields keep the names the Java bean
 * declared, `value` and `derivatives`, which are also the keys of the JSON form.
 *
 * ===Equality===
 *
 * Equality and hashing compare the double by its bit pattern rather than by numeric
 * comparison, which is what the bean equality of the Java original did and what every
 * double-bearing type of this port does. Two consequences are deliberate and are relied upon
 * by the round-trip properties of the test suite: a value that is not a number is equal to
 * itself, so an instance always equals itself, and a negative zero is distinct from a positive
 * zero. The derivatives contribute the equality of [[DoubleArray]], which compares element by
 * element on the same terms.
 *
 * ===Thread safety===
 *
 * An instance is immutable, and so is the array it holds, so it is safe to share between any
 * number of threads without synchronisation.
 *
 * @param value  the value of the variable
 * @param derivatives  the derivatives of the variable with respect to some inputs
 */
final case class ValueDerivatives(value: Double, derivatives: DoubleArray) {

  //-------------------------------------------------------------------------
  /**
   * Gets the derivative of the variable with respect to an input.
   *
   * The index selects a derivative by its position, counting from zero, in the order the
   * derivatives were supplied. The read is delegated to the array itself, so an index outside
   * it fails exactly as reading that array directly would: with the index exception of the
   * runtime, which is the caller-contract behaviour this port keeps for an array index rather
   * than reporting as a value. No index is otherwise rejected, and a derivative that is not a
   * number is returned as it stands.
   *
   * @param index  the zero-based derivative to obtain
   * @return the derivative at the index
   * @throws IndexOutOfBoundsException if the index is outside the derivatives
   */
  def getDerivative(index: Int): Double = derivatives.get(index)

  //-------------------------------------------------------------------------
  /**
   * Checks whether this instance equals another object.
   *
   * Another instance is equal when its value has the same bit pattern and its derivatives are
   * equal as an array. The equality synthesised for a case class would compare the value with
   * the numeric comparison of the platform, under which a value that is not a number is not
   * even equal to itself, so this replaces it; the array component is delegated to
   * [[DoubleArray]], whose own equality is already bit for bit. An object of any other type is
   * not equal.
   *
   * @param obj  the object to compare to
   * @return true if the other object holds the same value and derivatives
   */
  override def equals(obj: Any): Boolean = obj match {
    case other: ValueDerivatives =>
      (this eq other) ||
        (java.lang.Double.compare(value, other.value) == 0 && derivatives == other.derivatives)
    case _ => false
  }

  /**
   * Returns a hash code consistent with `equals`.
   *
   * The mixing is that of the Java bean this replaces - a seed, then each field in declaration
   * order - with the value hashed by its bit pattern so that instances which `equals` calls
   * equal always agree here too. The seed is the hash of the type's own name rather than the
   * identity hash of its class, which the generated bean used: that makes the hash of an
   * instance a function of the instance alone, identical in every run of every program, which
   * is what the byte-stability and round-trip properties of the test suite depend on.
   *
   * @return the hash code of the value and derivatives held
   */
  override def hashCode: Int =
    (ValueDerivatives.HashSeed * 31 + java.lang.Double.hashCode(value)) * 31 +
      derivatives.hashCode

  /**
   * Returns this instance as text.
   *
   * The form is the one the Joda-Beans generated bean produced, the fields named in
   * declaration order between braces, as in `ValueDerivatives{value=123.4, derivatives=[1.0,
   * 2.0, 3.0]}`. It is kept exactly so that ported code and the logs it writes read as they
   * did before, and it is what the `Show` instance of the companion renders.
   *
   * @return the rendering of this instance
   */
  override def toString: String =
    s"ValueDerivatives{value=$value, derivatives=$derivatives}"
}

/**
 * Factories, typeclass instances and the JSON form of [[ValueDerivatives]].
 *
 * Two typeclass instances are published, and exactly two: a `Hash`, which is the single
 * equality-bearing instance of the type - `Hash` extends `Eq`, so declaring an `Eq` as well
 * would leave two instances that could disagree and one of them ambiguous - and a `Show`.
 * There is deliberately no `Order`: the Java type is not `Comparable`, and a value paired with
 * a run of derivatives has no ordering worth inventing.
 */
object ValueDerivatives {

  /**
   * The seed of the hash code, standing in for the identity hash of the class that the
   * generated bean used, so that hashing is reproducible across runs. Any constant would do;
   * the hash of the type name is used because it is stable, specified by the platform, and
   * distinct from the seed of every other type of this port.
   */
  private val HashSeed: Int = "ValueDerivatives".hashCode

  //-------------------------------------------------------------------------
  /**
   * Obtains an instance from a value and array of derivatives.
   *
   * @param value  the value
   * @param derivatives  the derivatives of the value
   * @return the object
   */
  def of(value: Double, derivatives: DoubleArray): ValueDerivatives =
    ValueDerivatives(value, derivatives)

  //-------------------------------------------------------------------------
  /**
   * The hashing and equality of values with derivatives.
   *
   * Taken from the `equals` and `hashCode` of the type, which compare the double by its bit
   * pattern and the derivatives element by element. This is the type's only equality-bearing
   * instance, and `Eq[ValueDerivatives]` is obtained from it by subtyping.
   *
   * @return the hashing of values with derivatives
   */
  implicit val hash: Hash[ValueDerivatives] = Hash.fromUniversalHashCode[ValueDerivatives]

  /**
   * The rendering of values with derivatives as text.
   *
   * Renders what `toString` renders, which is the form of the Java original.
   *
   * @return the rendering of a value with derivatives
   */
  implicit val show: Show[ValueDerivatives] = Show.show(_.toString)

  //-------------------------------------------------------------------------
  // The two field codecs, brought into scope for the derivations below and for nothing else.
  // The double has to be taken from here rather than from the JSON library, whose instance
  // cannot express a value that is not a number; importing them at this point is what makes
  // that choice deliberate and local, as the codec support of `strata-collect` intends.
  import Codecs.implicits._

  /**
   * The JSON encoding of values with derivatives.
   *
   * The encoding is derived when this file is compiled, so no part of it inspects a class while
   * the program runs. An instance encodes as an object holding its two fields under the names
   * the Java bean declared:
   *
   * {{{
   * {"value":123.4,"derivatives":[1.0,2.0,3.0]}
   * }}}
   *
   * Every double on the path - the value itself and each derivative - is written through the
   * single policy of this port for doubles, so a value that is not a number and the two
   * infinities appear as the strings `"NaN"`, `"Infinity"` and `"-Infinity"` while a finite
   * value is written as a JSON number, exactly to the bit. Neither field is optional, so there
   * is no absent value to drop from the output and the encoding needs no post-processing.
   *
   * @return the JSON encoding of a value with derivatives
   */
  implicit val encoder: Encoder.AsObject[ValueDerivatives] = deriveEncoder[ValueDerivatives]

  /**
   * The JSON decoding of values with derivatives.
   *
   * This is the inverse of the encoding above and is likewise derived at compile time. Both
   * fields have to be present, and each double is read through the same policy that wrote it,
   * so an encoded instance decodes back to an instance that is equal to it - including one
   * carrying a value that is not a number, which the bit-pattern equality of this type makes
   * equal to itself. Construction cannot fail, so nothing beyond the shape of the payload is
   * checked here: a payload of the right shape always yields a value.
   *
   * @return the JSON decoding of a value with derivatives
   */
  implicit val decoder: Decoder[ValueDerivatives] = deriveDecoder[ValueDerivatives]
}
