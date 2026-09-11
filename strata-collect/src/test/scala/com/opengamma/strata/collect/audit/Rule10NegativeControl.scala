/*
 * Copyright (C) 2026 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.audit

/**
 * Negative control for the compiled-bytecode audit of the public API.
 *
 * That audit disassembles the compiled main classes of this module and of
 * `strata-basics`, looks for a Java collection, optional, stream or function
 * type in a public or protected signature, and requires that it find none -
 * which is how the port evidences that it exposes Scala collection types
 * only. An audit that reports nothing is trustworthy only if the audit itself
 * demonstrably works: a mistyped output path, a pattern that no longer matches
 * the emitted descriptors, or a disassembler whose output never reached the
 * filter would all report nothing as well, and would pass for entirely the
 * wrong reason.
 *
 * This type removes that possibility. It declares, deliberately, one Java
 * collection type in a public signature, and the audit pipeline run over its
 * own compiled form must report at least one match. Being compiled in test
 * scope, it lives in an output directory that the scan of the main classes
 * does not read, so it proves the pattern matches real descriptors without
 * contributing a match to the scan that has to stay empty.
 *
 * It consequently has - and must keep - no callers. Do not delete it as dead
 * code, do not move it to main scope, do not reference it from any other file
 * and do not write a test for it. It must also remain the only deliberate Java
 * collection signature in the build, so that every other match the audit
 * reports is a genuine finding.
 */
final class Rule10NegativeControl {

  /**
   * Returns an empty Java list, declared as the Java interface type rather
   * than the concrete implementation type so that the audit has the signature
   * it is meant to detect.
   *
   * The result is a fresh, independent list on every call, so this method
   * holds no state and has no observable effect beyond its return value.
   *
   * @return a newly created, empty Java list
   */
  def bad: java.util.List[String] = new java.util.ArrayList[String]()
}
