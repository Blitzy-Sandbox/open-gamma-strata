/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.io

import java.io.FileNotFoundException
import java.io.InputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

import cats.effect.IO
import cats.effect.Resource

/**
 * Loads text resources as effects.
 *
 * This object is the single effectful edge of this module: every other member is a pure
 * function of its arguments. It exists so that the JSON fixtures behind the parity
 * harness, and any text the demonstration application needs, can be read without ambient
 * global state. Each method returns a description of a read, and nothing touches the
 * classpath or the file system until that description is run.
 *
 * Both readers decode the bytes as UTF-8 leniently, replacing malformed input rather than
 * rejecting it. That matches the decode of the reader being replaced, and it keeps a
 * classpath read and a file read of the same bytes identical. The character set is always
 * stated, so the platform default is never consulted.
 *
 * A resource that cannot be obtained makes the returned effect fail. A missing classpath
 * resource fails with a [[java.io.FileNotFoundException]] naming it; a file that cannot be
 * read fails with the exception the platform itself reports, unwrapped. Neither reader
 * ever substitutes a sentinel or empty text for content.
 *
 * The scope is deliberately narrow. The byte and character source hierarchy of the
 * original, its locator value type together with the prefixed forms ("classpath:",
 * "file:" and "url:") that were parsed into one, its caller-sensitive classpath search,
 * its byte-order-mark handling and its archive support are all outside this port.
 */
object Resources {

  /**
   * Reads a classpath resource as UTF-8 text.
   *
   * The name is resolved against the class loader of this module, falling back to the
   * context class loader of the calling thread and then to the platform loader, so a
   * loader is always selected. Class loader resource names are absolute and carry no
   * leading separator, so one leading `/` is removed when present: both
   * `parity/example.json` and `/parity/example.json` name the same resource. The name is
   * otherwise used exactly as supplied: the calling class plays no part in resolution.
   *
   * The stream is closed once the read finishes, and equally if the read fails or is
   * cancelled.
   *
   * @param path  the resource name, with or without a leading `/`
   * @return the content of the resource decoded as UTF-8; the effect fails with a
   *         [[java.io.FileNotFoundException]] naming the resource when the classpath
   *         holds no such entry
   */
  def readClasspathText(path: String): IO[String] =
    openClasspathStream(canonicalResourceName(path)).use(readUtf8)

  /**
   * Reads a file as UTF-8 text.
   *
   * The path is interpreted by the platform exactly as supplied, so a relative path is
   * resolved against the working directory of the process. The whole file is read in one
   * step, which owns and releases its own handle.
   *
   * @param path  the path of the file to read
   * @return the content of the file decoded as UTF-8; the effect fails with the
   *         exception the platform reports, propagated unchanged: for an absent file that
   *         is [[java.nio.file.NoSuchFileException]], otherwise [[java.io.IOException]]
   */
  def readFileText(path: String): IO[String] =
    IO.blocking(decodeUtf8(Files.readAllBytes(Paths.get(path))))

  //-------------------------------------------------------------------------
  /**
   * Opens a classpath resource as a managed stream.
   *
   * Acquisition is the lookup itself, so an entry the classpath does not hold is reported
   * as a failed effect rather than as a stream that yields no bytes. Pairing the
   * acquisition with the `close` of the stream guarantees release on every outcome.
   */
  private def openClasspathStream(name: String): Resource[IO, InputStream] =
    Resource.fromAutoCloseable(
      IO.blocking(Option(classLoader.getResourceAsStream(name)))
        .flatMap(opened =>
          IO.fromOption(opened)(new FileNotFoundException(s"Classpath resource absent: $name"))
        )
    )

  /** Reads an open stream to exhaustion and decodes it, off the compute pool. */
  private def readUtf8(stream: InputStream): IO[String] =
    IO.blocking(decodeUtf8(stream.readAllBytes()))

  /** Decodes bytes as UTF-8, shared by both readers so that they agree exactly. */
  private def decodeUtf8(bytes: Array[Byte]): String =
    new String(bytes, StandardCharsets.UTF_8)

  /**
   * Converts a resource name to the form a class loader expects, by removing a single
   * leading separator when one is present.
   */
  private def canonicalResourceName(path: String): String =
    if (path.startsWith("/")) path.substring(1) else path

  /**
   * Selects a class loader, preferring the one that loaded this module, then the context
   * loader of the calling thread, and finally the platform loader, which is always
   * available. The result is therefore total, so a lookup can only ever end in the
   * documented failure above.
   */
  private def classLoader: ClassLoader =
    Option(getClass.getClassLoader)
      .orElse(Option(Thread.currentThread().getContextClassLoader))
      .getOrElse(ClassLoader.getPlatformClassLoader)
}
