/*
 * Copyright (C) 2014 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.io

import java.io.FileNotFoundException
import java.io.IOException
import java.io.InputStream
import java.nio.ByteBuffer
import java.nio.charset.CharacterCodingException
import java.nio.charset.CharsetDecoder
import java.nio.charset.CodingErrorAction
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
 * ===Both reads are bounded===
 *
 * A reader that materialises whatever it is pointed at is a way to exhaust a heap with a
 * choice of argument, so neither method here does. [[MaxBytes]] is the documented ceiling,
 * it applies to both sources, and it is enforced '''while''' reading rather than checked
 * beforehand: one byte more than the ceiling is read, and a source that yields it fails the
 * effect naming the source and the limit. Consulting the size of a file first would prove
 * nothing, because a file can grow between the question and the read, and a classpath entry
 * has no size to consult at all. The ceiling therefore also bounds the decoded text, since
 * a character costs at least one byte.
 *
 * ===The decode is strict===
 *
 * Both readers decode UTF-8 '''strictly''': a byte sequence that is not valid UTF-8 fails
 * the effect rather than being replaced by a substitution character. The character set is
 * always stated, so the platform default is never consulted, and both sources decode
 * identically, so a classpath read and a file read of the same bytes agree exactly.
 *
 * Strictness is a deliberate narrowing of the reader being replaced, which substituted a
 * replacement character for malformed input. What this reader ingests is the captured
 * parity baselines that the port's numerical results are measured against, and a
 * substitution inside one of those is a silently altered expectation: the measurement would
 * still run, against a value nobody captured. Failing loudly is the only useful outcome, so
 * that is what happens.
 *
 * ===Failure===
 *
 * A resource that cannot be obtained makes the returned effect fail. A missing classpath
 * resource fails with a [[java.io.FileNotFoundException]] naming it; a file that cannot be
 * read fails with the exception the platform itself reports, unwrapped; a source beyond the
 * ceiling and text that is not valid UTF-8 each fail with a [[java.io.IOException]] that
 * explains which of the two it was. Neither reader ever substitutes a sentinel or empty
 * text for content.
 *
 * ===Resource lifetime===
 *
 * Each read owns a stream, and that stream is closed exactly once on every outcome -
 * success, failure and cancellation alike - because acquisition is paired with release
 * through [[cats.effect.Resource]]. Both readers share the one implementation that makes
 * that pairing, so neither can drift from the other.
 *
 * The scope is deliberately narrow. The byte and character source hierarchy of the
 * original, its locator value type together with the prefixed forms ("classpath:",
 * "file:" and "url:") that were parsed into one, its caller-sensitive classpath search,
 * its byte-order-mark handling and its archive support are all outside this port.
 */
object Resources {

  /**
   * The largest source either reader accepts, in bytes: 64 MiB.
   *
   * The ceiling is a resource bound rather than a statement about the data: it is set well
   * above anything this module is expected to read, so that a legitimate source is never
   * refused, while an arbitrarily large one still cannot be materialised. The largest text
   * read in this repository today is the day-count parity baseline at roughly 12.7 MiB, so
   * the ceiling leaves it a factor of five of headroom. Peak memory during a read is
   * therefore bounded too, at the bytes read plus the text decoded from them.
   */
  val MaxBytes: Int = 64 * 1024 * 1024

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
   *         holds no such entry, and with a [[java.io.IOException]] when the resource
   *         exceeds [[MaxBytes]] or is not valid UTF-8
   */
  def readClasspathText(path: String): IO[String] = {
    val name = canonicalResourceName(path)
    readManaged(s"classpath resource '$name'", openClasspathStream(name), MaxBytes)
  }

  /**
   * Reads a file as UTF-8 text.
   *
   * The path is interpreted by the platform exactly as supplied, so a relative path is
   * resolved against the working directory of the process. The file is opened once, read
   * under the same ceiling as a classpath resource, and closed on every outcome.
   *
   * @param path  the path of the file to read
   * @return the content of the file decoded as UTF-8; the effect fails with the
   *         exception the platform reports, propagated unchanged: for an absent file that
   *         is [[java.nio.file.NoSuchFileException]], otherwise [[java.io.IOException]] -
   *         which is also how exceeding [[MaxBytes]] and invalid UTF-8 are reported
   */
  def readFileText(path: String): IO[String] =
    readManaged(s"file '$path'", openFileStream(path), MaxBytes)

  //-------------------------------------------------------------------------
  // The shared read, and the two acquisitions it is given.
  //
  // These four members are visible across this package so that the spec beside this file
  // can drive the production read with a stream of its own and observe that the stream is
  // closed exactly once on each outcome. That is the one property of a reader which cannot
  // be observed through the public surface - the class loader is chosen in here, so no
  // caller can hand a classpath read a stream it can count - and a release finalizer no
  // test can miss the absence of is a release finalizer that quietly stops existing.
  //-------------------------------------------------------------------------

  /**
   * Reads one source to its end, under a ceiling, and decodes it.
   *
   * @param source  how the source is named in a failure message
   * @param open  the acquisition of the stream to read
   * @param maxBytes  the largest number of bytes to accept; a source yielding more fails
   * @return the decoded text
   */
  private[io] def readManaged(source: String, open: IO[InputStream], maxBytes: Int): IO[String] =
    managedStream(open).use(stream => readBoundedText(source, stream, maxBytes))

  /**
   * Pairs the acquisition of a stream with its close.
   *
   * Release runs on every outcome of whatever uses the stream, so a read that fails part
   * way through, and one that is cancelled, both reclaim the handle.
   */
  private[io] def managedStream(open: IO[InputStream]): Resource[IO, InputStream] =
    Resource.fromAutoCloseable(open)

  /**
   * Acquires a classpath resource as a stream.
   *
   * Acquisition is the lookup itself, so an entry the classpath does not hold is reported
   * as a failed effect rather than as a stream that yields no bytes.
   */
  private[io] def openClasspathStream(name: String): IO[InputStream] =
    IO.blocking(Option(classLoader.getResourceAsStream(name)))
      .flatMap(opened =>
        IO.fromOption(opened)(new FileNotFoundException(s"Classpath resource absent: $name"))
      )

  /** Acquires a file as a stream, failing the effect with whatever the platform reports. */
  private[io] def openFileStream(path: String): IO[InputStream] =
    IO.blocking(Files.newInputStream(Paths.get(path)))

  //-------------------------------------------------------------------------
  /**
   * Reads at most one byte beyond the ceiling, off the compute pool, and refuses a source
   * that supplies that byte.
   *
   * Reading `maxBytes + 1` is what makes the ceiling an enforced bound rather than an
   * assumption: the read stops there, so a source of any size costs the ceiling and no
   * more, and the extra byte is the evidence that there was more to come.
   */
  private def readBoundedText(source: String, stream: InputStream, maxBytes: Int): IO[String] = {
    // Saturating rather than wrapping, so that a ceiling at the top of the range stays a
    // ceiling instead of becoming a request for a negative number of bytes.
    val probe = if (maxBytes < Int.MaxValue) maxBytes + 1 else Int.MaxValue
    IO.blocking(stream.readNBytes(probe)).flatMap { bytes =>
      if (bytes.length > maxBytes) {
        IO.raiseError(
          new IOException(
            s"$source exceeds the $maxBytes byte limit this reader accepts, so it was not " +
              "read; the limit exists so that the size of a source cannot determine the " +
              "memory this process uses"))
      } else {
        decodeUtf8(source, bytes)
      }
    }
  }

  /**
   * Decodes bytes as UTF-8, shared by both readers so that they agree exactly.
   *
   * The decoder reports malformed and unmappable input instead of substituting for it, and
   * is built per call because a decoder carries the state of the decode it is performing.
   */
  private def decodeUtf8(source: String, bytes: Array[Byte]): IO[String] =
    IO.delay(strictUtf8Decoder.decode(ByteBuffer.wrap(bytes)).toString)
      .handleErrorWith {
        case coding: CharacterCodingException =>
          IO.raiseError(
            new IOException(
              s"$source is not valid UTF-8 text, so it was not decoded; this reader " +
                "refuses malformed input rather than substituting for it, because a " +
                "substitution inside captured data is an expectation nobody captured",
              coding))
        case other => IO.raiseError(other)
      }

  /** A decoder that refuses malformed and unmappable input rather than substituting. */
  private def strictUtf8Decoder: CharsetDecoder =
    StandardCharsets.UTF_8
      .newDecoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)

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
