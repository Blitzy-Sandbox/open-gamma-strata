/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import cats.{Hash, Show}

import com.opengamma.strata.collect.NoJavaSerialization

/**
 * The target of a calculation within a system.
 *
 * All financial instruments that can be the target of calculations implement this marker
 * trait; a trade or a position, for example. The trait deliberately declares no member: it
 * exists purely to mark a type as something the calculation engine can be pointed at.
 *
 * All implementations of this trait must be immutable and thread-safe.
 *
 * The trait is intentionally open rather than sealed. Every financial instrument mixes it in
 * from its own file, and `ResolvableCalculationTarget` extends it from a sibling file in this
 * package, so sealing it would be incorrect as well as impossible.
 */
trait CalculationTarget

/**
 * A list of calculation targets.
 *
 * [[CalculationTarget]] is a marker trait that all financial instruments implement, such as
 * trades and positions. This allows them to be the target of calculations in the system. This
 * type is an immutable, ordered container of such targets and nothing more: it does not extend
 * [[CalculationTarget]] itself, so a value of this type cannot be passed where a single
 * calculation target is required.
 *
 * The list is held as a `scala.collection.immutable.List`, so the value is immutable and
 * thread-safe without any defensive copying: no caller can alter the list a value was built
 * from. Construction cannot fail, so the primary constructor, `apply` and `copy` are all
 * public, and no validating or failure-returning factory exists.
 *
 * Equality and hashing are the structural ones synthesised for the case class, which compare
 * and hash the targets element by element through each target's own `equals`/`hashCode`.
 *
 * This type deliberately has no JSON codec. `CalculationTarget` is a contract carrying no
 * data of its own, so no encoder or decoder can exist for an arbitrary element, and the list
 * carries no JSON form along with its element type.
 *
 * @param targets the targets, in the order supplied
 */
final case class CalculationTargetList(targets: List[CalculationTarget]) extends NoJavaSerialization

/**
 * Factories and typeclass instances for [[CalculationTargetList]].
 *
 * There are two `of` factories: a varargs form and a form taking an existing list. Because
 * Scala's `List` is covariant, the second form also accepts a list of any subtype of
 * `CalculationTarget`.
 *
 * Two typeclass instances are published, and exactly two: a `Hash`, which is the single
 * equality-bearing instance for this type - `Hash` extends `Eq`, so no separate `Eq` is
 * declared - and a `Show`. There is no `Order`, because a list of calculation targets has no
 * natural ordering to invent: the element type is a marker trait with no member to compare.
 */
object CalculationTargetList {

  /**
   * Obtains an instance from a list of targets.
   *
   * @param targets the targets, in the order they should be held
   * @return the list of targets
   */
  def of(targets: CalculationTarget*): CalculationTargetList =
    CalculationTargetList(targets.toList)

  /**
   * Obtains an instance from a list of targets.
   *
   * The list is used as it stands. `List` is immutable, so no copy is made and none is
   * needed: neither side can alter what the other reads.
   *
   * @param targets the targets, in the order they should be held
   * @return the list of targets
   */
  def of(targets: List[CalculationTarget]): CalculationTargetList =
    CalculationTargetList(targets)

  /**
   * Hashing and equality for the list.
   *
   * Derived from the case class's own structural `equals` and `hashCode`, which delegate to
   * the targets' `equals` and `hashCode`. This is the type's only equality-bearing instance,
   * and `Eq[CalculationTargetList]` is obtained from it by subtyping.
   */
  implicit val hash: Hash[CalculationTargetList] =
    Hash.fromUniversalHashCode[CalculationTargetList]

  /**
   * Rendering for the list.
   *
   * The list renders as `CalculationTargetList{targets=[a, b]}`. The rendering is a pure
   * function of the value: it introduces no identity hash of its own, and delegates only to
   * each target's own `toString`.
   */
  implicit val show: Show[CalculationTargetList] =
    Show.show(list => s"CalculationTargetList{targets=[${list.targets.mkString(", ")}]}")
}
