/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.result

import scala.collection.immutable.SortedMap

import cats.Hash
import cats.Show

/**
 * A single failure, describing why an operation did not produce a value.
 *
 * A failure carries a [[FailureReason]] that classifies it, a message written for a person
 * reading a log or a report, and a map of attributes holding the data the message refers to
 * in machine-readable form. Nothing else: a failure is a value, not an event, so it holds
 * no stack trace, no cause and no exception type, and it is never thrown. Where the Java
 * original modelled the same information as a bean that could be wrapped in an exception,
 * this port keeps the failure on the left of an `Either` and leaves the decision of what to
 * do about it to the caller.
 *
 * ===The closed set of failures===
 *
 * The type is `sealed` and every member is a `final case class` declared in the companion,
 * one per reason, so the set of failures is closed and a `match` over it is checked for
 * exhaustiveness. Choosing a member is therefore the same act as choosing a reason, and the
 * two cannot drift apart: `reason` is fixed by the member and is not a constructor
 * parameter, which makes a failure whose reason contradicts its class impossible to build.
 *
 * ===Attributes===
 *
 * The attributes are a `SortedMap`, ordered by key rather than by insertion, so that two
 * failures carrying the same attributes render and serialize identically no matter how each
 * was assembled. That is what makes the JSON form of a failure byte-stable, which in turn
 * is what allows a stored failure to be compared with a newly produced one.
 *
 * Attributes are added with `withAttribute`, which returns a new failure of the same class:
 *
 * {{{
 * Failure.Invalid("Schedule is invalid").withAttribute("definition", "P3M from 2024-01-15")
 * }}}
 *
 * @see [[FailureReason]] for the ten reasons a failure can carry
 */
sealed trait Failure {

  /**
   * Returns the reason classifying this failure.
   *
   * The reason is fixed by the class of the failure rather than supplied when it is built.
   *
   * @return the reason for this failure
   */
  def reason: FailureReason

  /**
   * Returns the message describing this failure.
   *
   * The message is written to be read by a person, and names the values it is about so that
   * it remains useful on its own, away from the attributes.
   *
   * @return the message describing this failure
   */
  def message: String

  /**
   * Returns the attributes of this failure, keyed by attribute name.
   *
   * The map is sorted by key, which makes the rendering and the serialized form of a
   * failure independent of the order in which its attributes were added.
   *
   * @return the attributes of this failure
   */
  def attributes: SortedMap[String, String]

  /**
   * Returns a copy of this failure with an additional attribute.
   *
   * The class and the reason of the failure are preserved. An attribute already present
   * under the same key is replaced.
   *
   * @param key  the attribute name
   * @param value  the attribute value
   * @return a copy of this failure carrying the additional attribute
   */
  final def withAttribute(key: String, value: String): Failure =
    withAttributes(attributes.updated(key, value))

  /**
   * Returns a copy of this failure with its attributes replaced.
   *
   * The class and the reason of the failure are preserved.
   *
   * @param newAttributes  the attributes the copy carries
   * @return a copy of this failure carrying the specified attributes
   */
  final def withAttributes(newAttributes: SortedMap[String, String]): Failure =
    this match {
      case failure: Failure.Multiple => Failure.Multiple(failure.message, newAttributes)
      case failure: Failure.Error => Failure.Error(failure.message, newAttributes)
      case failure: Failure.Invalid => Failure.Invalid(failure.message, newAttributes)
      case failure: Failure.Parsing => Failure.Parsing(failure.message, newAttributes)
      case failure: Failure.NotApplicable => Failure.NotApplicable(failure.message, newAttributes)
      case failure: Failure.Unsupported => Failure.Unsupported(failure.message, newAttributes)
      case failure: Failure.MissingData => Failure.MissingData(failure.message, newAttributes)
      case failure: Failure.CurrencyConversion => Failure.CurrencyConversion(failure.message, newAttributes)
      case failure: Failure.CalculationFailed => Failure.CalculationFailed(failure.message, newAttributes)
      case failure: Failure.Other => Failure.Other(failure.message, newAttributes)
    }

  /**
   * Returns the rendering of this failure as text.
   *
   * The form is the reason, then the message, then the attributes when there are any, which
   * keeps the classification visible in a log line that may hold failures of several kinds.
   *
   * @return the rendering of this failure
   */
  override def toString: String =
    if (attributes.isEmpty) {
      s"${reason.name}: $message"
    } else {
      val rendered = attributes.iterator.map { case (key, value) => s"$key=$value" }.mkString(", ")
      s"${reason.name}: $message [$rendered]"
    }
}

/**
 * Provides the ten kinds of failure, one per failure reason, and the instances for them.
 */
object Failure {

  /** The empty attribute map, the value every member defaults its attributes to. */
  private val NoAttributes: SortedMap[String, String] = SortedMap.empty[String, String]

  /**
   * Several failures occurred that did not agree on a reason.
   *
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure
   */
  final case class Multiple(message: String, attributes: SortedMap[String, String] = NoAttributes)
      extends Failure {
    override def reason: FailureReason = FailureReason.MULTIPLE
  }

  /**
   * An error occurred.
   *
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure
   */
  final case class Error(message: String, attributes: SortedMap[String, String] = NoAttributes)
      extends Failure {
    override def reason: FailureReason = FailureReason.ERROR
  }

  /**
   * The input was invalid.
   *
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure
   */
  final case class Invalid(message: String, attributes: SortedMap[String, String] = NoAttributes)
      extends Failure {
    override def reason: FailureReason = FailureReason.INVALID
  }

  /**
   * Text could not be parsed as the value it was expected to name.
   *
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure
   */
  final case class Parsing(message: String, attributes: SortedMap[String, String] = NoAttributes)
      extends Failure {
    override def reason: FailureReason = FailureReason.PARSING
  }

  /**
   * The operation was not applicable to this combination of inputs.
   *
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure
   */
  final case class NotApplicable(message: String, attributes: SortedMap[String, String] = NoAttributes)
      extends Failure {
    override def reason: FailureReason = FailureReason.NOT_APPLICABLE
  }

  /**
   * The operation requested is not supported.
   *
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure
   */
  final case class Unsupported(message: String, attributes: SortedMap[String, String] = NoAttributes)
      extends Failure {
    override def reason: FailureReason = FailureReason.UNSUPPORTED
  }

  /**
   * Data the operation required was missing.
   *
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure
   */
  final case class MissingData(message: String, attributes: SortedMap[String, String] = NoAttributes)
      extends Failure {
    override def reason: FailureReason = FailureReason.MISSING_DATA
  }

  /**
   * A conversion between currencies failed.
   *
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure
   */
  final case class CurrencyConversion(message: String, attributes: SortedMap[String, String] = NoAttributes)
      extends Failure {
    override def reason: FailureReason = FailureReason.CURRENCY_CONVERSION
  }

  /**
   * A calculation could not be performed.
   *
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure
   */
  final case class CalculationFailed(message: String, attributes: SortedMap[String, String] = NoAttributes)
      extends Failure {
    override def reason: FailureReason = FailureReason.CALCULATION_FAILED
  }

  /**
   * The failure occurred for some other reason.
   *
   * @param message  the message describing the failure
   * @param attributes  the attributes of the failure
   */
  final case class Other(message: String, attributes: SortedMap[String, String] = NoAttributes)
      extends Failure {
    override def reason: FailureReason = FailureReason.OTHER
  }

  /**
   * Obtains a failure from a reason and a message.
   *
   * This is the route to take when the reason is a value in hand rather than a choice made
   * while writing the code; where the reason is known statically, naming the member of this
   * companion directly is clearer. The member returned is the one whose `reason` is the
   * reason supplied, so `of(reason, message).reason == reason` for every reason.
   *
   * @param reason  the reason classifying the failure
   * @param message  the message describing the failure
   * @return the failure with that reason and message
   */
  def of(reason: FailureReason, message: String): Failure =
    reason match {
      case FailureReason.MULTIPLE => Multiple(message)
      case FailureReason.ERROR => Error(message)
      case FailureReason.INVALID => Invalid(message)
      case FailureReason.PARSING => Parsing(message)
      case FailureReason.NOT_APPLICABLE => NotApplicable(message)
      case FailureReason.UNSUPPORTED => Unsupported(message)
      case FailureReason.MISSING_DATA => MissingData(message)
      case FailureReason.CURRENCY_CONVERSION => CurrencyConversion(message)
      case FailureReason.CALCULATION_FAILED => CalculationFailed(message)
      case FailureReason.OTHER => Other(message)
    }

  /**
   * The hashing and equality of failures.
   *
   * This is the only equality-bearing instance of the type, and it is the equality of the
   * values themselves: two failures are equal when they are of the same class and carry the
   * same message and attributes.
   *
   * @return the hashing of failures
   */
  implicit val hash: Hash[Failure] = Hash.fromUniversalHashCode[Failure]

  /**
   * The rendering of failures as text.
   *
   * A failure renders as `toString` does, so the two agree.
   *
   * @return the rendering of a failure
   */
  implicit val show: Show[Failure] = Show.show(_.toString)
}
