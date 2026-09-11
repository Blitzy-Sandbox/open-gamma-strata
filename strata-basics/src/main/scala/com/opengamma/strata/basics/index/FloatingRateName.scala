/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics.index

/**
 * The name of a family of floating rate indices, without the tenor that picks one out.
 *
 * `GBP-LIBOR-3M` names an index; `GBP-LIBOR-BBA` names the family that index belongs to, and
 * says nothing about the period the rate covers. The distinction matters because market
 * conventions and trade documents are written against the family - a contract refers to sterling
 * Libor and states its tenor separately - so this is the form a floating rate arrives in from
 * FpML and from a convention, and resolving it to a concrete index is a separate step that needs
 * the tenor.
 *
 * A floating rate name carries the external name it is known by, the kind of rate it describes
 * as a [[FloatingRateType]], and the name of the index family the tenor is appended to. From
 * those it converts to a concrete index: an Ibor family plus a tenor names an [[IborIndex]], and
 * an overnight or price family names its index directly.
 *
 * ===Why the family is closed===
 *
 * The set of floating rate names is reference data published by this library, not an extension
 * point, so the type is `sealed` and its members are created only in the companion from the
 * published data. No member is declared yet: the tables that create them, and the conversions to
 * a concrete index, are ported together with the index families themselves. Until then the family
 * is a closed set with nothing in it, so a lookup by name finds nothing.
 *
 * @see [[FloatingRate]] for the abstraction this shares with a concrete index
 * @see [[FloatingRateType]] for the kind of rate a floating rate name describes
 */
sealed abstract class FloatingRateName extends FloatingRate {

  /**
   * Gets the floating rate name of this floating rate, which is this value.
   *
   * A concrete index reports the family it belongs to, losing its tenor; a family reports
   * itself, so the operation is the identity here. It is `final` because there is nothing for a
   * member to decide.
   *
   * @return this floating rate name
   */
  final def floatingRateName: FloatingRateName = this
}

/**
 * Holds the members of the floating rate name family and the lookup of a member by name.
 */
object FloatingRateName {

  /**
   * The members of the family, by name.
   *
   * The members are created from the published floating rate name data, which is ported with
   * the index families they convert to, so this table is empty until then.
   */
  private val byName: Map[String, FloatingRateName] = Map.empty

  /**
   * Looks up a floating rate name by name, answering with nothing when no member has that name.
   *
   * @param name  the floating rate name, such as `GBP-LIBOR-BBA`
   * @return the floating rate name of that name, or nothing when the family has no such member
   */
  def valueOf(name: String): Option[FloatingRateName] = byName.get(name)
}
