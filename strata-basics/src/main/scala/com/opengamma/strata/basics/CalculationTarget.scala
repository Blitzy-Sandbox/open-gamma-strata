/*
 * Copyright (C) 2015 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import cats.{Hash, Show}

/**
 * The target of a calculation within a system.
 *
 * All financial instruments that can be the target of calculations implement this marker
 * trait; a trade or a position, for example. The trait deliberately declares no member: it
 * exists purely to mark a type as something the calculation engine can be pointed at, which
 * is the same contract the Java `CalculationTarget` interface carries.
 *
 * All implementations of this trait must be immutable and thread-safe.
 *
 * The trait is intentionally open rather than sealed. Every future financial instrument mixes
 * it in from its own file, and `ResolvableCalculationTarget` extends it from a sibling file in
 * this package, so sealing it would be incorrect as well as impossible. It is also one of
 * the forward-path types of this port: nothing inside `strata-basics` consumes a
 * `CalculationTarget`, and it is ported now because it is part of the contract the modules
 * migrated in later slices implement.
 */
trait CalculationTarget

/**
 * A list of calculation targets.
 *
 * [[CalculationTarget]] is a marker trait that all financial instruments implement, such as
 * trades and positions. This allows them to be the target of calculations in the system. This
 * type is an immutable, ordered container of such targets and nothing more: it does not extend
 * [[CalculationTarget]] itself - the Java `CalculationTargetList` does not implement
 * `CalculationTarget` either - so a value of this type cannot be passed where a single
 * calculation target is required.
 *
 * The list is held as a `scala.collection.immutable.List`, so the value is immutable and
 * thread-safe without any defensive copying: unlike the Java original, which wrapped a
 * mutable `List` argument with `ImmutableList.copyOf`, there is no mutable input to guard
 * against. Construction cannot fail, so this is a total type in the sense of the port's
 * construction policy: the primary constructor, `apply` and `copy` are all public, and no
 * validating or failure-returning factory exists.
 *
 * Equality and hashing are the structural ones synthesised for the case class, which compare
 * and hash the targets element by element through each target's own `equals`/`hashCode` -
 * precisely the semantics of the Joda-Beans generated bean equality this type replaces.
 *
 * This type deliberately has no JSON codec. `CalculationTarget` is a contract carrying no
 * data of its own, so no encoder or decoder can exist for an arbitrary element, and the list
 * is therefore excluded from the port's codec inventory along with its element type.
 *
 * @param targets the targets, in the order supplied
 */
final case class CalculationTargetList(targets: List[CalculationTarget])

/**
 * Factories and typeclass instances for [[CalculationTargetList]].
 *
 * The two `of` factories mirror the two static factories of the Java original one for one:
 * a varargs form and a form taking an existing list. Because Scala's `List` is covariant,
 * the second form also accepts a list of any subtype of `CalculationTarget`, which is what
 * the Java signature expressed with `List<? extends CalculationTarget>`.
 *
 * Two typeclass instances are published, and exactly two: a `Hash`, which is the single
 * equality-bearing instance for this type - `Hash` extends `Eq`, so no separate `Eq` is
 * declared - and a `Show`. There is no `Order`: the Java type is not `Comparable` and a list
 * of opaque calculation targets has no natural ordering to invent.
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
   * needed; this is the one behavioural simplification relative to the Java factory, which
   * had to defend itself against a mutable argument.
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
   * Reproduces the rendering of the Java type exactly - `CalculationTargetList{targets=[a,
   * b]}` - so that ported code and its logs read as they did before. The rendering is a pure
   * function of the value: it introduces no identity hash of its own, and delegates only to
   * each target's own `toString`.
   */
  implicit val show: Show[CalculationTargetList] =
    Show.show(list => s"CalculationTargetList{targets=[${list.targets.mkString(", ")}]}")
}
