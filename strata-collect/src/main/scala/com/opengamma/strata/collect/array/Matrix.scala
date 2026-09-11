/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.array

/**
 * Base abstraction for every matrix type.
 *
 * A matrix is an n-dimensional collection of elements of a fixed size. The number of dimensions is
 * a property of the concrete type rather than of an individual value, so a type that models a
 * one-dimensional matrix - conventionally known as an array - reports one dimension for every value
 * it ever holds, while a type that models a two-dimensional matrix always reports two.
 *
 * The trait is intentionally open: it exists so that code which only needs to know how large a
 * matrix is, or how many dimensions it has, can be written once against every matrix type,
 * including types introduced outside this package. It carries no data and no state of its own, and
 * implementations are expected to be immutable.
 */
trait Matrix {

  /**
   * The number of dimensions of this matrix.
   *
   * Each matrix type has a fixed number of dimensions, which this method returns; it is one for a
   * one-dimensional matrix, two for a two-dimensional matrix, and so on.
   *
   * @return the number of dimensions of the matrix, never negative
   */
  def dimensions: Int

  /**
   * The size of this matrix.
   *
   * This is the total number of elements the matrix holds, counted across all of its dimensions.
   *
   * @return the total number of elements in the matrix, never negative
   */
  def size: Int

}
