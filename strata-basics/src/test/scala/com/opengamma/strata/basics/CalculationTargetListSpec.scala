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

/** Test [[CalculationTargetList]]. */
final class CalculationTargetListSpec extends AnyFunSuite with Matchers {

  import CalculationTargetListSpec.TestTarget

  private val TARGET1: CalculationTarget = TestTarget(1)
  private val TARGET2: CalculationTarget = TestTarget(2)

  //-------------------------------------------------------------------------
  test("test_array0") {
    val test = CalculationTargetList.of()
    test.targets shouldBe List.empty[CalculationTarget]
    test.targets.size shouldBe 0
  }

  test("test_array2") {
    val test = CalculationTargetList.of(TARGET1, TARGET2)
    test.targets shouldBe List(TARGET1, TARGET2)
    test.targets.head should be theSameInstanceAs TARGET1
    test.targets.last should be theSameInstanceAs TARGET2
  }

  test("test_collection1") {
    val targets: List[CalculationTarget] = List(TARGET1)
    val test = CalculationTargetList.of(targets)
    test.targets shouldBe List(TARGET1)
    test.targets should be theSameInstanceAs targets
  }

  //-------------------------------------------------------------------------
  test("coverage") {
    val test = CalculationTargetList.of(TARGET1, TARGET2)

    test.targets shouldBe List(TARGET1, TARGET2)
    CalculationTargetList(test.targets) shouldBe test
    test.copy(targets = test.targets) shouldBe test

    val same = CalculationTargetList.of(List[CalculationTarget](TestTarget(1), TestTarget(2)))
    same shouldBe test
    same.hashCode shouldBe test.hashCode
    test.copy(targets = List(TARGET1)) should not equal test

    CalculationTargetList.of(TARGET2, TARGET1) should not equal test

    val notAList: Any = "CalculationTargetList{targets=[TestTarget(1), TestTarget(2)]}"
    test should not equal notAList

    Hash[CalculationTargetList].eqv(test, same) shouldBe true
    Hash[CalculationTargetList].eqv(test, CalculationTargetList.of(TARGET2, TARGET1)) shouldBe false
    Hash[CalculationTargetList].hash(test) shouldBe test.hashCode

    val rendered = Show[CalculationTargetList].show(test)
    rendered shouldBe "CalculationTargetList{targets=[TestTarget(1), TestTarget(2)]}"
    rendered should not be empty
    Show[CalculationTargetList].show(test) shouldBe rendered
    Show[CalculationTargetList].show(same) shouldBe rendered
    Show[CalculationTargetList].show(CalculationTargetList.of()) shouldBe
      "CalculationTargetList{targets=[]}"

    assertTypeError("""
      val target: com.opengamma.strata.basics.CalculationTarget =
        com.opengamma.strata.basics.CalculationTargetList.of()
    """)

    assertCompiles("""
      val target: com.opengamma.strata.basics.CalculationTarget =
        com.opengamma.strata.basics.CalculationTargetListSpec.TestTarget(1)
    """)
  }

  test("test_serialization") {
    val test = CalculationTargetList.of(TARGET1, TARGET2)
    val restored = CalculationTargetList(test.targets)
    restored shouldBe test
    restored.hashCode shouldBe test.hashCode
    restored.targets should have size 2
    restored.targets.head should be theSameInstanceAs TARGET1
    restored.targets.last should be theSameInstanceAs TARGET2
    restored.targets shouldBe List(TARGET1, TARGET2)

    assertTypeError("io.circe.Encoder[com.opengamma.strata.basics.CalculationTargetList]")
    assertTypeError("io.circe.Decoder[com.opengamma.strata.basics.CalculationTargetList]")

    assertCompiles("io.circe.Encoder[Int]")
    assertCompiles("io.circe.Decoder[Int]")
  }
}

object CalculationTargetListSpec {

  final case class TestTarget(value: Int) extends CalculationTarget
}
