/*
 * Copyright (C) 2009 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect.testkit

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets
import java.time.{LocalDate, Month}
import java.util.logging.{Handler, Level, LogRecord, Logger}

import scala.collection.mutable.ListBuffer

/**
 * Test helper - the retained helper subset used by both Scala modules.
 *
 * This object is a cross-module contract. It is compiled in the test scope of
 * `strata-collect` and made visible to the test scope of the dependent module
 * by the `test->test` dependency edge of the build, which replaces the
 * test-artifact dependency the original build declared. Specs bring the
 * members into scope with
 * `import com.opengamma.strata.collect.testkit.TestHelper._`, so every member
 * here is public and no name may drift.
 *
 * Only the helpers that survive the migration live here. The reflective
 * bean-coverage and serialization sweeps of the Java original are replaced
 * across the port by property-based round-trip and equality checks together
 * with the JSON round-trip spec; its family of exception assertions is
 * replaced by the test framework's own `assertThrows` and `intercept`; and its
 * bean and string-conversion assertions belonged to a serialization framework
 * this port does not use. Those members therefore have no counterpart, and the
 * single-method interface they accepted is replaced throughout by by-name
 * parameters.
 *
 * The two factory helpers are pure. The two capture helpers redirect
 * process-global state for the duration of the block they are given and
 * restore it afterwards, so they take turns rather than running concurrently.
 */
object TestHelper {

  /**
   * Creates a `LocalDate`, intended for import through `TestHelper._`.
   *
   * The date is built exactly as `LocalDate.of` builds it: no normalisation
   * and no extra validation is applied, so an out-of-range field is rejected
   * in precisely the way a direct call would reject it.
   *
   * @param year  the year
   * @param month  the month, from 1 (January) to 12 (December)
   * @param dayOfMonth  the day of month, from 1 to 31
   * @return the date
   */
  def date(year: Int, month: Int, dayOfMonth: Int): LocalDate =
    LocalDate.of(year, month, dayOfMonth)

  /**
   * Creates a `LocalDate` from a month enum, intended for import through
   * `TestHelper._`.
   *
   * This is an overload of the all-integer form and behaves identically; it
   * exists so that a spec may name the month where that reads better.
   *
   * @param year  the year
   * @param month  the month
   * @param dayOfMonth  the day of month, from 1 to 31
   * @return the date
   */
  def date(year: Int, month: Month, dayOfMonth: Int): LocalDate =
    LocalDate.of(year, month, dayOfMonth)

  /**
   * Creates an immutable list of the given items, intended for import through
   * `TestHelper._`.
   *
   * This single variadic method replaces the whole family of fixed-arity
   * overloads of the Java original, which existed only because that language
   * has no concise list literal. Every arity the family covered is expressible
   * here - the empty list, `list(a, b, c)`, and the spread form
   * `list(items: _*)` - and element order is always preserved.
   *
   * @tparam T  the element type
   * @param items  the items, in order
   * @return the immutable list of those items, in that order
   */
  def list[T](items: T*): List[T] =
    items.toList

  /**
   * Captures everything written to `System.out` while the given block runs.
   *
   * This is the renamed port of the Java original, whose name carried a
   * typographical error; the migration note records the rename, and the
   * misspelling is deliberately not offered as an alias because the original
   * is untouched and no source compatibility is owed to it.
   *
   * Output is buffered, decoded as UTF-8 and returned verbatim - nothing is
   * trimmed, normalised or appended - so a caller may assert on exact text.
   * `System.out` is restored before this method returns whether the block
   * completes normally or throws, and an exception thrown by the block
   * propagates unchanged and uncaught.
   *
   * Both output channels a Scala caller can reach are redirected: the
   * process-wide `System.out`, which is what the original captured and what
   * library code writing to that stream uses, and the Scala console, which
   * `println` writes to. The console has to be redirected explicitly because
   * it resolves the stream it wraps once, when it is first used, so
   * reassigning `System.out` alone would leave `println` printing to the real
   * console and capture nothing.
   *
   * Calls take turns on this object's monitor, which mirrors the static
   * synchronization of the original. That makes the helper thread-safe only so
   * long as nothing else reassigns `System.out`, because that stream is
   * process-global: anything any thread prints while the redirect is in place
   * is captured here instead of reaching the console.
   *
   * The parameter is by-name and typed `Unit`, so callers pass a statement
   * block:
   *
   * {{{
   * val captured = captureStdOut {
   *   println("hello")
   * }
   * }}}
   *
   * A block whose final expression yields a value is adapted to `Unit` at the
   * call site, and the build reports that value-discard adaptation as a
   * warning which it then treats as a build failure, so discard such a value
   * explicitly inside the block.
   *
   * @param body  the block of code to run while output is captured
   * @return the captured output, decoded as UTF-8
   */
  def captureStdOut(body: => Unit): String = synchronized {
    // A thread-local stream would permit concurrent capture, but is worth
    // introducing only if taking turns ever proves insufficient.
    val buffer = new ByteArrayOutputStream(1024)
    // The charset-typed constructor and decoder are used rather than the
    // charset-name-typed ones: the latter declare a checked exception that the
    // original had to wrap away, while these declare none.
    val captureStream = new PrintStream(buffer, false, StandardCharsets.UTF_8)
    val original = System.out
    try {
      System.setOut(captureStream)
      // The console redirect is scoped to the block and unwound by `withOut`
      // itself, including when the block throws; the stream it is given is the
      // same buffer, so output through either channel lands in capture order.
      scala.Console.withOut(captureStream)(body)
    } finally {
      // Flushing before the stream is restored is what lets buffered output
      // survive a block that throws; on the normal path it is equivalent to
      // the flush the original performed inside its try.
      captureStream.flush()
      System.setOut(original)
    }
    buffer.toString(StandardCharsets.UTF_8)
  }

  /**
   * Captures the log records published to a class's logger while the given
   * block runs.
   *
   * This is the renamed port of the Java original, whose name carried a
   * typographical error; the migration note records the rename.
   *
   * The logger is located by the class's name, a capturing handler is attached
   * to it at level `ALL`, the logger's own level is raised to `ALL` and its
   * parent handlers are suppressed, so that records are collected here rather
   * than printed. Before this method returns - whether the block completes
   * normally or throws - the handler is detached and both the logger's level
   * and its parent-handler flag are restored to the values they had on entry.
   * That restoration is a deliberate correction of the original, which
   * suppressed parent handlers permanently and never raised the logger's own
   * level, leaving a logger configured more restrictively than the records
   * under test able to filter them before any handler saw them. Neither change
   * affects which records are captured, only what is left behind afterwards.
   * An exception thrown by the block propagates unchanged and uncaught, so no
   * partial list is ever returned.
   *
   * The handler detached afterwards is only the one attached here: a handler
   * the caller had already attached to the same logger is left exactly where it
   * was found, and, being neither detached nor reconfigured, it goes on
   * receiving every record published for as long as the capture is in force.
   * The level and the parent-handler flag are handed back the values read on
   * entry rather than the framework's defaults, and a logger that had no level
   * of its own - that is, one inheriting its level from its parent - is
   * restored to inheriting it.
   *
   * Calls take turns on this object's monitor, which mirrors the static
   * synchronization of the original. A logger is process-global, so the helper
   * is thread-safe only so long as nothing else reconfigures the same logger
   * at the same time; records that other threads publish to it are captured.
   *
   * {{{
   * val records = captureLog(classOf[MyService]) {
   *   new MyService().run()
   * }
   * }}}
   *
   * @param loggerClass  the class identifying the logger to capture; its name
   *   is the logger name, exactly as the logging framework resolves it
   * @param body  the block of code to run while records are captured
   * @return the captured records, in publication order
   */
  def captureLog(loggerClass: Class[_])(body: => Unit): List[LogRecord] = synchronized {
    // The logger is held in a local for the whole method because the log
    // manager references loggers weakly: dropping this reference would let the
    // configuration applied below be collected while the block is still running.
    val logger = Logger.getLogger(loggerClass.getName)
    val handler = new CapturingHandler
    val savedLevel: Option[Level] = Option(logger.getLevel)
    val savedUseParentHandlers = logger.getUseParentHandlers
    try {
      handler.setLevel(Level.ALL)
      logger.setLevel(Level.ALL)
      logger.setUseParentHandlers(false)
      logger.addHandler(handler)
      body
      handler.snapshot
    } finally {
      logger.removeHandler(handler)
      // An absent level means "inherit from the parent logger", which is
      // exactly the state reported above when no level is set locally, so
      // handing the saved value straight back restores either case.
      logger.setLevel(savedLevel.orNull)
      logger.setUseParentHandlers(savedUseParentHandlers)
    }
  }

  /**
   * A log handler that accumulates the records published to it.
   *
   * The buffer is private and never escapes: it is observable only through
   * `snapshot`, which copies it into an immutable list. Both the append and
   * the copy hold the buffer's monitor, because the logging framework may
   * publish from any thread.
   */
  private final class CapturingHandler extends Handler {

    private val records = ListBuffer.empty[LogRecord]

    /**
     * Appends a published record to the buffer.
     *
     * @param record  the record being published
     */
    override def publish(record: LogRecord): Unit =
      records.synchronized {
        val _ = records.addOne(record)
      }

    /** Does nothing: the buffer needs no flushing. */
    override def flush(): Unit = ()

    /** Does nothing: the buffer holds no resource to release. */
    override def close(): Unit = ()

    /**
     * @return an immutable snapshot of the records published so far, in
     *   publication order
     */
    def snapshot: List[LogRecord] =
      records.synchronized {
        records.toList
      }
  }
}
