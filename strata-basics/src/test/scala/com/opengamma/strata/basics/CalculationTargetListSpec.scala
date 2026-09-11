/*
 * Copyright (C) 2016 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.basics

import scala.collection.immutable.List

import cats.Hash
import cats.Show

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/**
 * Test [[CalculationTargetList]].
 *
 * The Java original held five test methods and all five survive under their own names, so
 * that the method-level traceability of the migration - which joins each ported Java method
 * to the test case this suite emits - is preserved. Three of them port directly: the two
 * factories and the ordering of the list they build are asserted exactly as before. The
 * other two asserted machinery that this port removes, and each keeps its name while
 * asserting the fact that replaced it:
 *
 *  - `coverage` called a reflective helper, `coverImmutableBean`, over the Joda-Beans bean
 *    the Java type was. Nothing here is a bean and nothing is derived reflectively, so the
 *    properties that helper stood in for - readable properties, a construction round-trip,
 *    and one consistent notion of equality, hashing and rendering - are asserted directly.
 *  - `test_serialization` asserted a Java serialization round-trip. Java serialization is
 *    supported by no type of this port, and this type has no JSON codec either, because its
 *    element type is a contract carrying no data of its own. The test therefore asserts the
 *    nearest property that is meaningful here, and proves the absence of the codec so that
 *    adding one by accident fails loudly.
 *
 * ===Why this suite matters more than its size suggests===
 *
 * [[CalculationTarget]] and [[CalculationTargetList]] have no consumer inside this module:
 * they are part of the contract that the modules migrated in later slices implement, and
 * they are ported now so that the contract is in place when those slices land. This suite is
 * consequently the only exercise either type gets, and the only thing standing between the
 * pair and silent drift before their first consumer arrives. It is written to that brief: it
 * pins the shape of both factories, the order sensitivity of the list, the two published
 * typeclass instances, and the deliberate absence of a codec.
 *
 * ===The fixture===
 *
 * The Java test declared a nested `TestTarget` carrying an `int` and hand-wrote value-based
 * `equals` and `hashCode` over it. The counterpart in
 * [[CalculationTargetListSpec.TestTarget]] is a case class, which is given exactly those two
 * methods by the compiler. What the Java fixture additionally declared - the marker interface
 * of the Java serialization mechanism and the serial version identifier that goes with it -
 * is deliberately not reproduced: no type of this port supports Java serialization, so a
 * fixture claiming to would assert a capability the port does not have.
 */
final class CalculationTargetListSpec extends AnyFunSuite with Matchers {

  import CalculationTargetListSpec.TestTarget

  /**
   * The two targets of the Java test, in its order and with its values.
   *
   * Both are typed as `CalculationTarget` rather than as the fixture class, exactly as the
   * Java fields were. That is what makes `List(TARGET1)` below a `List[CalculationTarget]`
   * and so the argument of the collection factory rather than of the varargs one, and it
   * keeps every assertion in this suite an assertion about the published contract rather
   * than about the fixture.
   */
  private val TARGET1: CalculationTarget = TestTarget(1)
  private val TARGET2: CalculationTarget = TestTarget(2)

  //-------------------------------------------------------------------------
  test("test_array0") {
    // the varargs factory is the only one an empty argument list can bind - the collection
    // factory needs a list to be handed to it - and it must yield no targets at all rather
    // than one target that happens to be an empty sequence, which is the mistake a varargs
    // factory invites. The size is asserted separately from the equality for that reason
    val test = CalculationTargetList.of()
    test.targets shouldBe List.empty[CalculationTarget]
    test.targets.size shouldBe 0
  }

  test("test_array2") {
    // the Java assertion was `containsExactly`, so the order of the targets is part of what
    // is being asserted and not merely their membership
    val test = CalculationTargetList.of(TARGET1, TARGET2)
    test.targets shouldBe List(TARGET1, TARGET2)
    test.targets.head should be theSameInstanceAs TARGET1
    test.targets.last should be theSameInstanceAs TARGET2
  }

  test("test_collection1") {
    // this case exists to exercise the collection factory rather than the varargs one, and
    // the two are told apart by identity: the collection factory holds the list it is given,
    // because a `List` is already immutable and needs no defensive copy, whereas the varargs
    // factory necessarily builds a list of its own. A reference-identity assertion therefore
    // proves which overload bound, which an equality assertion alone could not
    val targets: List[CalculationTarget] = List(TARGET1)
    val test = CalculationTargetList.of(targets)
    test.targets shouldBe List(TARGET1)
    test.targets should be theSameInstanceAs targets
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    // The Java test swept the bean reflectively with `coverImmutableBean`, which read every
    // property, rebuilt the value through its builder, and exercised equality, hashing and
    // rendering. Nothing here is a bean, so those properties are asserted directly over the
    // published API - which is also the only place they can be asserted, since this type is
    // a total one: its primary constructor, `apply` and `copy` are all public and no
    // validating factory exists to fail
    val test = CalculationTargetList.of(TARGET1, TARGET2)

    // the single readable property, and the construction round-trip that replaces the
    // builder round-trip: the value rebuilt from what it exposes equals the value itself
    test.targets shouldBe List(TARGET1, TARGET2)
    CalculationTargetList(test.targets) shouldBe test
    test.copy(targets = test.targets) shouldBe test

    // two values built independently from equal targets are equal and hash alike, and
    // equality is decided by the targets, so replacing one of them parts the two values.
    // Equality of the targets is the fixture's own value equality, not identity: the second
    // list is built from fresh targets carrying the same values
    val same = CalculationTargetList.of(List[CalculationTarget](TestTarget(1), TestTarget(2)))
    same shouldBe test
    same.hashCode shouldBe test.hashCode
    test.copy(targets = List(TARGET1)) should not equal test

    // order is the substantive content of a list-valued equality: the same two targets in
    // the other order are a different value, which is what distinguishes this type from one
    // holding an unordered collection
    CalculationTargetList.of(TARGET2, TARGET1) should not equal test

    // and a value of another type is not equal to it, however it is rendered
    val notAList: Any = "CalculationTargetList{targets=[TestTarget(1), TestTarget(2)]}"
    test should not equal notAList

    // the companion publishes exactly two instances, and they must agree with the value's
    // own equality, hashing and rendering. `Hash` extends `Eq`, so it is the single
    // equality-bearing instance of the type and there is no separate `Eq` to disagree with
    Hash[CalculationTargetList].eqv(test, same) shouldBe true
    Hash[CalculationTargetList].eqv(test, CalculationTargetList.of(TARGET2, TARGET1)) shouldBe false
    Hash[CalculationTargetList].hash(test) shouldBe test.hashCode

    // the rendering is a pure function of the value: non-empty, naming every target, stable
    // across calls, and reproducing the form of the type being ported
    val rendered = Show[CalculationTargetList].show(test)
    rendered shouldBe "CalculationTargetList{targets=[TestTarget(1), TestTarget(2)]}"
    rendered should not be empty
    Show[CalculationTargetList].show(test) shouldBe rendered
    Show[CalculationTargetList].show(same) shouldBe rendered
    Show[CalculationTargetList].show(CalculationTargetList.of()) shouldBe
      "CalculationTargetList{targets=[]}"
  }

  test("test_serialization") {
    // The Java case asserted a Java serialization round-trip. Java serialization support is
    // out of scope for every type of this port, and this type has no JSON codec either: its
    // element type is a contract that carries no data of its own, so no encoder can exist
    // for an arbitrary element, and the list is excluded from the codec inventory along with
    // its element type. Neither round-trip is therefore available to assert, and neither is
    // missing by oversight.
    //
    // What is asserted instead is the property those round-trips were there to protect: the
    // value can be taken apart and put back together without losing a target, changing one,
    // or reordering them. The targets come back out as the very objects that went in, in the
    // order they went in, and the value rebuilt from them equals the original
    val test = CalculationTargetList.of(TARGET1, TARGET2)
    val restored = CalculationTargetList(test.targets)
    restored shouldBe test
    restored.hashCode shouldBe test.hashCode
    restored.targets should have size 2
    restored.targets.head should be theSameInstanceAs TARGET1
    restored.targets.last should be theSameInstanceAs TARGET2
    restored.targets shouldBe List(TARGET1, TARGET2)

    // and the excluded JSON round-trip is proved absent rather than merely left untested, so
    // that a codec added for this type in the future fails here instead of silently widening
    // the port's serialization surface. The summons are written out in full so that this
    // suite needs no import it would otherwise not have
    assertTypeError("io.circe.Encoder[com.opengamma.strata.basics.CalculationTargetList]")
    assertTypeError("io.circe.Decoder[com.opengamma.strata.basics.CalculationTargetList]")

    // the control for the two assertions above, without which they would also pass if the
    // summon were misspelled or the JSON library were missing from the class path: the same
    // summon, written the same way, resolves for a type that does have a codec. What the two
    // failures above show is therefore the absence of an instance for this type alone
    assertCompiles("io.circe.Encoder[Int]")
    assertCompiles("io.circe.Decoder[Int]")
  }
}

/**
 * The fixture of [[CalculationTargetListSpec]].
 *
 * The counterpart of the Java test's nested `static class TestTarget`, and nested for the
 * same reason: a calculation target that exists only to be counted and compared has no place
 * in the package, where another suite could reach it and come to depend on it.
 */
object CalculationTargetListSpec {

  /**
   * A calculation target carrying nothing but a value that tells one instance from another.
   *
   * A case class, so that the value-based `equals` and `hashCode` the Java fixture wrote by
   * hand are the ones the compiler writes, and so that `toString` renders the value in the
   * list's own rendering. The Java serialization marker and the serial version identifier the
   * Java fixture also declared are deliberately absent: no type of this port supports Java
   * serialization.
   *
   * @param value  the value distinguishing this target from another
   */
  final case class TestTarget(value: Int) extends CalculationTarget
}
