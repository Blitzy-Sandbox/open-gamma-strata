/*
 * Copyright (C) 2019 - present by OpenGamma Inc. and the OpenGamma group of companies
 *
 * Please see distribution for license.
 */
package com.opengamma.strata.collect

import java.time.{DateTimeException, LocalDate, Month}
import java.util.logging.{Handler, Level, LogRecord, Logger}

import scala.collection.mutable.ListBuffer

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import com.opengamma.strata.collect.testkit.TestHelper._

/**
 * Tests the retained helper subset of the ported test kit.
 *
 * The helper is a cross-module contract: the dependent module's specs bring its
 * members into scope with exactly the import used above, so the names and
 * signatures exercised here are what keeps that module compiling. Only the four
 * retained helper groups are covered - the two date factories, the list factory
 * and the two capture helpers. The members the migration drops have no
 * counterpart to test, and that includes the two assertion helpers the original
 * test class exercised exclusively: the test framework supplies both, so the
 * cases that covered them consolidate onto the named tests below.
 *
 * Two properties of the port are made observable rather than assumed. The list
 * factory's result is bound to the fully qualified immutable Scala list type,
 * so the test would not compile if the helper returned some other collection -
 * it is the change of return type, away from the third-party collection of the
 * original, that the binding pins down. And the whole fixed-arity family of the
 * original collapsed into one variadic member here, so each arity the family
 * offered is exercised through that single member.
 *
 * The capture helpers redirect process-global state, so this spec never asserts
 * on that state from outside a capture. Restoration is observed from inside an
 * enclosing capture instead: the helper holds its monitor for the whole of the
 * nested call, so nothing another thread captures can interleave with the
 * observation. State the block itself sees is reported the same way round, as
 * the text of a record or a line of output the capture returns, so no assertion
 * runs while the redirect is in force. Every test here is therefore
 * deterministic, and none leaves output redirection or logger configuration
 * behind to surface as flakiness in a spec that runs later in the same test
 * JVM.
 *
 * What the logger capture leaves behind is held to two kinds of logger, because
 * one kind cannot distinguish the two ways of getting it wrong. A logger left
 * at the logging framework's defaults catches a capture that restores nothing,
 * and a logger preconfigured away from those defaults - carrying a handler of
 * this spec's own - catches a capture that resets to defaults or discards a
 * handler it never installed. The second kind is set up and unwound by
 * `withPreconfiguredLogger`, which returns it to the state it was found in.
 */
final class TestHelperSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  /** A character UTF-8 encodes in two bytes: small letter e with acute. */
  private val TwoByteChar: String = 0x00e9.toChar.toString

  /** A character UTF-8 encodes in three bytes: the euro sign. */
  private val ThreeByteChar: String = 0x20ac.toChar.toString

  /**
   * A character outside the basic multilingual plane, which UTF-8 encodes in
   * four bytes and Java holds as a surrogate pair. It is built from its code
   * point rather than written as a literal so that the fixture cannot depend on
   * how this source file happens to be encoded.
   */
  private val FourByteChar: String = new String(Character.toChars(0x1f600))

  /**
   * Runs the given body against a logger whose configuration differs from both
   * the logging framework's defaults and the configuration `captureLog`
   * installs, and leaves that logger exactly as it was found.
   *
   * The level is set to `WARNING` - neither the absent level a fresh logger
   * reports nor the `ALL` the capture installs - and the parent-handler flag is
   * set to `false`, the opposite of the framework's default. A capture that
   * reset the logger to defaults instead of restoring it would therefore be
   * visible to the caller of this method, which a logger left at its defaults
   * cannot show. The handler attached here belongs to the caller, and the
   * capture is required to leave it attached and receiving records, so the body
   * is handed the instance to check identity against.
   *
   * The logger is held in a local for the whole call because the log manager
   * references loggers weakly: dropping the reference would let the
   * configuration applied here be collected while the body still runs. The
   * unwinding is the outer `finally` of every test that uses this fixture -
   * the whole module shares one test JVM, so a handler or a level left behind
   * here would surface as flakiness in a spec that runs later.
   *
   * @tparam A  the result type of the body
   * @param subject  the class whose name identifies the logger to preconfigure
   * @param body  the block to run, given the preconfigured logger and the
   *   handler attached to it by this fixture
   * @return the result of the body
   */
  private def withPreconfiguredLogger[A](subject: Class[_])(body: (Logger, SentinelLogHandler) => A): A = {
    val logger = Logger.getLogger(subject.getName)
    val sentinel = new SentinelLogHandler
    val savedLevel: Option[Level] = Option(logger.getLevel)
    val savedUseParentHandlers = logger.getUseParentHandlers
    try {
      sentinel.setLevel(Level.ALL)
      logger.setLevel(Level.WARNING)
      logger.setUseParentHandlers(false)
      logger.addHandler(sentinel)
      body(logger, sentinel)
    } finally {
      logger.removeHandler(sentinel)
      // An absent level means "inherit from the parent logger", so handing the
      // saved value straight back restores either case; this is the idiom the
      // helper under test uses to restore the same field.
      logger.setLevel(savedLevel.orNull)
      logger.setUseParentHandlers(savedUseParentHandlers)
    }
  }

  //-------------------------------------------------------------------------
  // date(Int, Int, Int)

  test("date builds the LocalDate named by its year, month and day-of-month") {
    val built = date(2015, 6, 30)
    built shouldBe LocalDate.of(2015, 6, 30)
    built.getYear shouldBe 2015
    built.getMonthValue shouldBe 6
    built.getDayOfMonth shouldBe 30
  }

  test("date accepts the leap day of a leap year") {
    date(2024, 2, 29) shouldBe LocalDate.of(2024, 2, 29)
    date(2000, 2, 29) shouldBe LocalDate.of(2000, 2, 29)
    date(2024, 2, 29).getDayOfMonth shouldBe 29
    date(2024, 2, 29).isLeapYear shouldBe true
  }

  test("date handles a month boundary") {
    date(2020, 1, 31) shouldBe LocalDate.of(2020, 1, 31)
    date(2020, 2, 1) shouldBe LocalDate.of(2020, 2, 1)
    date(2020, 1, 31).plusDays(1L) shouldBe date(2020, 2, 1)
    date(2020, 2, 29).plusDays(1L) shouldBe date(2020, 3, 1)
  }

  test("date handles a year boundary") {
    date(2019, 12, 31) shouldBe LocalDate.of(2019, 12, 31)
    date(2020, 1, 1) shouldBe LocalDate.of(2020, 1, 1)
    date(2019, 12, 31).plusDays(1L) shouldBe date(2020, 1, 1)
    date(2019, 12, 31).getDayOfYear shouldBe 365
  }

  test("date rejects an out-of-range field exactly as the platform factory does") {
    // The helper adds no validation of its own, so each of these fails in the
    // same way a direct call to the platform factory would fail.
    intercept[DateTimeException](date(2020, 13, 1))
    intercept[DateTimeException](date(2020, 0, 1))
    intercept[DateTimeException](date(2020, 4, 31))
    intercept[DateTimeException](date(2023, 2, 29))
  }

  //-------------------------------------------------------------------------
  // date(Int, Month, Int)

  test("date with a named month builds the LocalDate named by that month and day-of-month") {
    date(2016, Month.FEBRUARY, 29) shouldBe LocalDate.of(2016, 2, 29)
    date(2016, Month.DECEMBER, 31) shouldBe LocalDate.of(2016, 12, 31)
    date(2016, Month.JANUARY, 1).getMonth shouldBe Month.JANUARY
  }

  test("date with a named month agrees with the numeric form for every month of the year") {
    val months = Month.values().toList
    months should have size 12
    months.foreach { month =>
      date(2021, month, 15) shouldBe date(2021, month.getValue, 15)
    }
  }

  test("date with a named month agrees with the numeric form for arbitrary valid dates") {
    forAll(Gen.choose(1900, 2100), Gen.choose(1, 12), Gen.choose(1, 28)) {
      (year: Int, month: Int, dayOfMonth: Int) =>
        date(year, Month.of(month), dayOfMonth) shouldBe date(year, month, dayOfMonth)
    }
  }

  //-------------------------------------------------------------------------
  // list(...)

  test("list with no arguments yields the empty immutable list") {
    // The element type is given explicitly: with no argument to infer it from,
    // an unannotated call would be typed at the bottom type.
    val empty: List[String] = list[String]()
    empty shouldBe Nil
    empty.isEmpty shouldBe true
    empty.size shouldBe 0
  }

  test("list with one argument yields a single-element list") {
    list("a") shouldBe List("a")
    list("a").size shouldBe 1
    list(1) shouldBe List(1)
  }

  test("list with two arguments preserves argument order") {
    list("a", "b") shouldBe List("a", "b")
    list("b", "a") shouldBe List("b", "a")
  }

  test("list with three arguments preserves argument order") {
    list(1, 2, 3) shouldBe List(1, 2, 3)
    list(3, 1, 2) shouldBe List(3, 1, 2)
  }

  test("list with four arguments preserves argument order") {
    list(1, 2, 3, 4) shouldBe List(1, 2, 3, 4)
    list("d", "c", "b", "a") shouldBe List("d", "c", "b", "a")
  }

  test("list with five arguments preserves argument order") {
    list(1, 2, 3, 4, 5) shouldBe List(1, 2, 3, 4, 5)
    list("e", "d", "c", "b", "a") shouldBe List("e", "d", "c", "b", "a")
  }

  test("list with a spread sequence returns that whole sequence in order") {
    val items = (1 to 64).toList
    list(items: _*) shouldBe items
    list(items: _*) should have size 64
    list(items: _*).head shouldBe 1
    list(items: _*).last shouldBe 64
  }

  test("list preserves duplicate elements and their positions") {
    list("b", "a", "b", "a", "b") shouldBe List("b", "a", "b", "a", "b")
    list("b", "a", "b").distinct shouldBe List("b", "a")
  }

  test("list returns a scala.collection.immutable.List") {
    // This binding is the assertion. The helper's declared result type has to
    // be the immutable Scala list for the spec to compile at all, which is the
    // observable form of the change of return type away from the original's
    // third-party collection.
    val typed: scala.collection.immutable.List[String] = list("a", "b", "c")
    typed shouldBe ("a" :: "b" :: "c" :: Nil)
    typed.head shouldBe "a"
    typed.last shouldBe "c"
    val typedEmpty: scala.collection.immutable.List[String] = list[String]()
    typedEmpty shouldBe Nil
  }

  test("list returns exactly its arguments in order for an arbitrary sequence") {
    forAll { (items: List[String]) =>
      list(items: _*) shouldBe items
    }
  }

  //-------------------------------------------------------------------------
  // captureStdOut

  test("captureStdOut returns exactly the text the block printed") {
    val captured = captureStdOut {
      println("a line")
    }
    captured shouldBe s"a line${System.lineSeparator()}"
  }

  test("captureStdOut captures the console and the process output stream in print order") {
    // Both channels a caller can reach are redirected onto one buffer, so the
    // captured text is the interleaving of the two in the order printed.
    val captured = captureStdOut {
      print("console")
      System.out.print("-stream")
      println("-line")
    }
    captured shouldBe s"console-stream-line${System.lineSeparator()}"
  }

  test("captureStdOut flushes output written without a trailing line separator") {
    val captured = captureStdOut {
      print("no trailing separator")
    }
    captured shouldBe "no trailing separator"
    captured.endsWith(System.lineSeparator()) shouldBe false
  }

  test("captureStdOut decodes captured output as UTF-8") {
    // A character from outside the basic multilingual plane cannot survive a
    // single-byte encoding, so its round trip is what shows the capture stream
    // and the decoding of the buffer agree on UTF-8.
    val multiByte = s"$TwoByteChar$ThreeByteChar$FourByteChar"
    val captured = captureStdOut {
      print(multiByte)
    }
    captured shouldBe multiByte
    captured.codePointCount(0, captured.length) shouldBe 3
    captured should endWith(FourByteChar)
  }

  test("captureStdOut restores the output stream after the block completes") {
    // The observation is made inside an enclosing capture, which holds the
    // helper's monitor for the whole of the nested call: the text printed after
    // the nested capture returns reaches the enclosing buffer only if the
    // nested capture put the previous stream back.
    val captured = captureStdOut {
      val enclosing = System.out
      val nested = captureStdOut {
        print("nested text")
      }
      print(s"nested=[$nested];restored=${System.out eq enclosing}")
    }
    captured shouldBe "nested=[nested text];restored=true"
  }

  test("captureStdOut restores the output stream when the block throws") {
    // Restoration happens on the failure path too. Were it not to, the stream
    // in place after the failure would still be the abandoned nested buffer,
    // and the enclosing capture would come back empty rather than carrying the
    // text printed below.
    val captured = captureStdOut {
      val enclosing = System.out
      val failure = intercept[IllegalStateException] {
        captureStdOut {
          print("text the failed capture kept")
          throw new IllegalStateException("capture failure")
        }
      }
      print(s"message=[${failure.getMessage}];restored=${System.out eq enclosing}")
    }
    captured shouldBe "message=[capture failure];restored=true"
  }

  test("captureStdOut captures again after a block has thrown") {
    val failure = intercept[IllegalStateException] {
      captureStdOut {
        print("text of the failed capture")
        throw new IllegalStateException("first block failed")
      }
    }
    failure.getMessage shouldBe "first block failed"
    val captured = captureStdOut {
      print("second capture")
    }
    captured shouldBe "second capture"
  }

  test("captureStdOut supports two successive captures without leaking output between them") {
    // The helper serialises its calls rather than running them concurrently, so
    // sequential re-entrancy is the property to hold it to; a concurrent test
    // would not be deterministic.
    val first = captureStdOut {
      print("first block")
    }
    val second = captureStdOut {
      print("second block")
    }
    first shouldBe "first block"
    second shouldBe "second block"
  }

  test("captureStdOut returns the empty string for a block that prints nothing") {
    val captured = captureStdOut {
      ()
    }
    captured shouldBe ""
  }

  //-------------------------------------------------------------------------
  // captureLog

  test("captureLog returns the records the named logger published, in publication order") {
    val subject = classOf[PrimaryLogSubject]
    // The logger is held for the duration of the test: the log manager
    // references loggers weakly, so dropping it could hand the block a
    // different instance than the one configured.
    val logger = Logger.getLogger(subject.getName)
    val records = captureLog(subject) {
      logger.log(Level.FINEST, "finest")
      logger.log(Level.FINE, "fine")
      logger.log(Level.INFO, "info")
      logger.log(Level.WARNING, "warning")
      logger.log(Level.SEVERE, "severe")
    }
    records.map(_.getMessage) shouldBe List("finest", "fine", "info", "warning", "severe")
    records.map(_.getLevel) shouldBe List(Level.FINEST, Level.FINE, Level.INFO, Level.WARNING, Level.SEVERE)
    records.map(_.getLoggerName).distinct shouldBe List(subject.getName)
  }

  test("captureLog raises the level, attaches its handler and suppresses parent handlers while the block runs") {
    val subject = classOf[PrimaryLogSubject]
    val logger = Logger.getLogger(subject.getName)
    val handlersBefore = logger.getHandlers.length
    // The state observed inside the block leaves it as the text of a record at
    // the finest level, which reports the configuration in force and at the
    // same time shows that a fine-grained record is captured at all.
    val records = captureLog(subject) {
      logger.log(
        Level.FINEST,
        s"handlers=${logger.getHandlers.length}" +
          s";levelIsAll=${logger.getLevel == Level.ALL}" +
          s";parentHandlers=${logger.getUseParentHandlers}")
    }
    records.map(_.getMessage) shouldBe List(
      s"handlers=${handlersBefore + 1};levelIsAll=true;parentHandlers=false")
    records.map(_.getLevel) shouldBe List(Level.FINEST)
  }

  test("captureLog removes its handler and restores the logger configuration after the block completes") {
    val subject = classOf[RestoringLogSubject]
    val logger = Logger.getLogger(subject.getName)
    val handlersBefore = logger.getHandlers.length
    val levelBefore = Option(logger.getLevel)
    val parentHandlersBefore = logger.getUseParentHandlers
    val records = captureLog(subject) {
      logger.log(Level.INFO, "during the block")
    }
    records.map(_.getMessage) shouldBe List("during the block")
    logger.getHandlers.length shouldBe handlersBefore
    Option(logger.getLevel) shouldBe levelBefore
    logger.getUseParentHandlers shouldBe parentHandlersBefore
  }

  test("captureLog removes its handler and restores the logger configuration when the block throws") {
    val subject = classOf[FailingLogSubject]
    val logger = Logger.getLogger(subject.getName)
    val handlersBefore = logger.getHandlers.length
    val levelBefore = Option(logger.getLevel)
    val parentHandlersBefore = logger.getUseParentHandlers
    val failure = intercept[IllegalStateException] {
      captureLog(subject) {
        logger.log(Level.INFO, "before the failure")
        throw new IllegalStateException("logging block failed")
      }
    }
    failure.getMessage shouldBe "logging block failed"
    logger.getHandlers.length shouldBe handlersBefore
    Option(logger.getLevel) shouldBe levelBefore
    logger.getUseParentHandlers shouldBe parentHandlersBefore
  }

  test("captureLog restores a non-default configuration and leaves a caller's handler receiving records") {
    // The two tests above start from a logger at its defaults, which pins down
    // that something is restored but not that the entry state is what comes
    // back: an absent level and a set parent-handler flag are also what a reset
    // to defaults would produce. This logger is configured to differ from the
    // defaults and from what the capture installs, so only restoration of the
    // entry state satisfies it, and it carries a handler of the spec's own that
    // the capture must neither detach nor silence.
    val subject = classOf[PreconfiguredLogSubject]
    withPreconfiguredLogger(subject) { (logger, sentinel) =>
      val handlersAtEntry = logger.getHandlers.length
      // The state seen inside the block leaves it as the text of a record, in
      // the way the level-and-handler test does it: nothing may assert while
      // the capture holds the logger, and a record at the finest level shows
      // the raised level at the same time.
      val records = captureLog(subject) {
        logger.log(
          Level.FINEST,
          s"handlers=${logger.getHandlers.length}" +
            s";sentinelAttached=${logger.getHandlers.exists(_ eq sentinel)}" +
            s";levelIsAll=${logger.getLevel == Level.ALL}" +
            s";parentHandlers=${logger.getUseParentHandlers}")
      }
      // The capture's own purpose still holds: the record the block published
      // is what it returned, alongside the fixture's handler rather than
      // instead of it.
      records.map(_.getMessage) shouldBe List(
        s"handlers=${handlersAtEntry + 1};sentinelAttached=true;levelIsAll=true;parentHandlers=false")
      records.map(_.getLevel) shouldBe List(Level.FINEST)
      // The handler the spec installed saw the same record, which is what shows
      // the capture did not stop it receiving records for the duration.
      sentinel.snapshot.map(_.getMessage) shouldBe records.map(_.getMessage)
      // Identity, not equality: the instance installed by the fixture is the
      // one still attached, so a capture that cleared the logger's handlers and
      // installed a fresh one of the same class would fail here.
      val impostor = new SentinelLogHandler
      logger.getHandlers.count(_ eq sentinel) shouldBe 1
      logger.getHandlers.exists(_ eq impostor) shouldBe false
      // Back to the entry count, so the capture's own handler is gone.
      logger.getHandlers.length shouldBe handlersAtEntry
      logger.getLevel shouldBe Level.WARNING
      logger.getUseParentHandlers shouldBe false
    }
  }

  test("captureLog restores a non-default configuration and leaves a caller's handler receiving records when the block throws") {
    val subject = classOf[PreconfiguredFailingLogSubject]
    withPreconfiguredLogger(subject) { (logger, sentinel) =>
      val handlersAtEntry = logger.getHandlers.length
      val failure = intercept[IllegalStateException] {
        captureLog(subject) {
          logger.log(
            Level.FINEST,
            s"handlers=${logger.getHandlers.length}" +
              s";sentinelAttached=${logger.getHandlers.exists(_ eq sentinel)}" +
              s";levelIsAll=${logger.getLevel == Level.ALL}" +
              s";parentHandlers=${logger.getUseParentHandlers}")
          throw new IllegalStateException("preconfigured logging block failed")
        }
      }
      failure.getMessage shouldBe "preconfigured logging block failed"
      // A block that throws returns no records, so the handler the spec
      // installed is the only witness of what was published - which is itself
      // the evidence that it kept receiving records while the capture was in
      // force, on the path where the capture unwinds through its `finally`.
      sentinel.snapshot.map(_.getMessage) shouldBe List(
        s"handlers=${handlersAtEntry + 1};sentinelAttached=true;levelIsAll=true;parentHandlers=false")
      val impostor = new SentinelLogHandler
      logger.getHandlers.count(_ eq sentinel) shouldBe 1
      logger.getHandlers.exists(_ eq impostor) shouldBe false
      logger.getHandlers.length shouldBe handlersAtEntry
      logger.getLevel shouldBe Level.WARNING
      logger.getUseParentHandlers shouldBe false
    }
  }

  test("captureLog returns the empty list for a block that logs nothing") {
    val records = captureLog(classOf[SecondaryLogSubject]) {
      ()
    }
    records shouldBe Nil
    records.isEmpty shouldBe true
  }

  test("captureLog does not capture records published to a different logger") {
    // The two captures are nested so that the records of the inner logger are
    // collected by the inner capture rather than printed, which keeps the test
    // quiet as well as showing the two captures do not see each other's
    // records. The helper's calls are re-entrant on one thread.
    val outerSubject = classOf[PrimaryLogSubject]
    val innerSubject = classOf[SecondaryLogSubject]
    val outerLogger = Logger.getLogger(outerSubject.getName)
    val innerLogger = Logger.getLogger(innerSubject.getName)
    val outerRecords = captureLog(outerSubject) {
      outerLogger.log(Level.INFO, "outer only")
      val innerRecords = captureLog(innerSubject) {
        innerLogger.log(Level.INFO, "inner only")
      }
      outerLogger.log(Level.INFO, s"inner=[${innerRecords.map(_.getMessage).mkString(",")}]")
    }
    outerRecords.map(_.getMessage) shouldBe List("outer only", "inner=[inner only]")
  }

  test("captureLog supports two successive captures without leaking records between them") {
    val subject = classOf[PrimaryLogSubject]
    val logger = Logger.getLogger(subject.getName)
    // The count the captures must return to is the one the logger had before
    // them, not zero: a hard-coded zero is also what a capture that removed
    // handlers it never installed would leave behind.
    val handlersBefore = logger.getHandlers.length
    val first = captureLog(subject) {
      logger.log(Level.INFO, "first block")
    }
    val second = captureLog(subject) {
      logger.log(Level.INFO, "second block")
    }
    first.map(_.getMessage) shouldBe List("first block")
    second.map(_.getMessage) shouldBe List("second block")
    logger.getHandlers.length shouldBe handlersBefore
  }

  test("captureLog returns a scala.collection.immutable.List of log records") {
    val subject = classOf[PrimaryLogSubject]
    val logger = Logger.getLogger(subject.getName)
    // As for the list factory, the binding is the assertion: only the immutable
    // Scala list, holding the platform's own record type, satisfies it.
    val records: scala.collection.immutable.List[LogRecord] = captureLog(subject) {
      logger.log(Level.WARNING, "typed")
    }
    records.map(_.getMessage) shouldBe ("typed" :: Nil)
    records.map(_.getLevel) shouldBe (Level.WARNING :: Nil)
  }
}

/**
 * A class whose name identifies a logger this spec captures.
 *
 * The capture helper names its logger through a class, so each capture test
 * needs one. These subjects exist only to give this spec loggers of its own:
 * nothing else in the build refers to them, so no other spec can reconfigure a
 * logger a test here depends on, and the assertions about what the helper
 * leaves behind stay deterministic.
 */
private[collect] final class PrimaryLogSubject

/** A second logger-naming subject, used to show captures stay separate. */
private[collect] final class SecondaryLogSubject

/** A logger-naming subject used by the configuration-restoration test. */
private[collect] final class RestoringLogSubject

/** A logger-naming subject used by the restoration-on-failure test. */
private[collect] final class FailingLogSubject

/**
 * A logger-naming subject whose logger is preconfigured away from the
 * framework's defaults before it is captured.
 *
 * It is separate from the subjects above because those are deliberately left at
 * their defaults: one logger cannot serve both halves of the discrimination
 * between restoring the entry state and resetting to defaults.
 */
private[collect] final class PreconfiguredLogSubject

/**
 * A logger-naming subject whose logger is preconfigured away from the
 * framework's defaults before a capture whose block throws.
 *
 * The throwing path gets a logger of its own so that the records its capture
 * publishes cannot be confused with those of the normal path.
 */
private[collect] final class PreconfiguredFailingLogSubject

/**
 * A log handler that a test attaches to a logger itself, before that logger is
 * handed to `captureLog`.
 *
 * It stands for a handler the caller owns. The capture helper is required to
 * leave such a handler attached and to leave it receiving records, so the
 * records collected here are the evidence for the second half of that
 * requirement and the instance's own identity is the evidence for the first.
 *
 * The buffer is private and observable only through `snapshot`, which copies it
 * into an immutable list; both the append and the copy hold the buffer's
 * monitor, exactly as the helper's own handler does it, because the logging
 * framework may publish from any thread.
 */
private[collect] final class SentinelLogHandler extends Handler {

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
