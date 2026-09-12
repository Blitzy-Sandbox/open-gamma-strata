Strata-Basics
-------------
This directory contains the `strata-basics` module: the Scala port of the Java `strata-basics`.
It depends on the Scala [`strata-collect`](../strata-collect/README.md), and the two are the sbt
build's only modules - `strata-basics` is the build's root project, with its sources under
`strata-basics/src`. The package root is retained, `com.opengamma.strata.basics`, and so are the
names of the published constants and properties, so `DayCounts.ACT_365F`, `HolidayCalendarIds.GBLO`,
`IborIndices.GBP_LIBOR_3M`, `Tenor.TENOR_3M` and `Frequency.P3M` mean here what they meant there.
The type-level structure is deliberately reorganised, however: the several Java classes that made
up one concept collapse into a single Scala file - `date/DayCount.scala` holds the whole day-count
family - while the resource loaders, the Joda-Beans builders and meta-beans, and the Java exception
types have no Scala counterpart at all. [`SCALA_MIGRATION.md`](../SCALA_MIGRATION.md) records what
was collapsed, replaced and dropped, symbol by symbol.
Sources are Scala 2.13.18 on JDK 21, built by the root `build.sbt` with sbt 1.13.0.

### Overview

This module provides common financial concepts used by Strata:

* reference data
* money and currency
* day counts
* day rolling
* schedule generation
* indices

Every value type is immutable; a data-dependent failure is returned as `Either[Failure, _]` or
`EitherNec[Failure, _]` rather than thrown; the named families - currencies, day counts,
conventions, indices, floating-rate names - are closed sealed ADTs carrying a `NamedEnum` instance
in place of the Java runtime registry; and serialization is circe, derived at compile time, so
nothing on the codec path uses reflection. Reference data is threaded explicitly rather than looked
up ambiently: a `ReferenceDataId` resolves against the `ReferenceData` a caller supplies,
`ReferenceData.standard` holding the built-in holiday calendars, and an adjustment offers
`resolve(refData)` and `toReader` as well as `adjust(date, refData)`, so a run of dates resolves its
calendar once.

The currencies, currency pairs, countries, indices, floating-rate names and holiday calendars the
Java module read from INI, CSV and properties resources on the class path are held here as immutable
Scala data - the `*Data.scala` objects beside the types that read them, and the generated national
calendars as the rules that produce them. The module therefore ships no main resources and reads
nothing from the class path at run time; an application that needs calendars of its own supplies its
own `ReferenceData`, a parameter rather than a resource.


### Building and running

JDK 21 and sbt 1.13.0 are the prerequisites for the `sbt` commands below; the gate runner also
needs `git`, `python3` and the POSIX text utilities - `awk`, `sed`, `grep`, `find`, `sort`, `comm`,
`tr`, `cut`, `wc` and `diff`. The build definition is the root `build.sbt`, which compiles this
module as Scala 2.13.18 with `-release 21` under `-Werror`.

```
  sbt test
  sbt "strata-basics/run"
  sbt "testOnly *ParitySpec"
  scripts/verify-gates.sh
```

`sbt test` runs both modules' specs, because `strata-basics` is the sbt root project - which is why
this module directory holds no build file of its own - and it aggregates `strata-collect`.

`sbt "strata-basics/run"` runs the demo, `com.opengamma.strata.basics.demo.BasicsDemoApp`, a
cats-effect `IOApp.Simple`. It builds a `PeriodicSchedule`, adjusts its dates against a built-in
`HolidayCalendar` through an explicitly supplied `ReferenceData`, converts a two-currency
`MultiCurrencyAmount` into US dollars through an `FxMatrix`, then serializes the results to JSON and
prints them.

[`scripts/verify-gates.sh`](../scripts/verify-gates.sh) is the single authoritative acceptance-gate
runner: it executes every automated gate of the migration in order, writes `target/gate-report.md`,
and exits non-zero if an automated gate fails. Gate 7's manual approval of the migration note is an
out-of-band review rather than a measurement, so the script records that row as reported and never
blocks or fails on it. CI runs it in the `scala_build21` job.


### Parity harness

The port's numbers are pinned to values captured from the Java implementation rather than re-derived
by hand. `src/test/resources/parity` holds the five baseline fixtures - `daycount-baseline.json`,
`schedule-baseline.json`, `fx-baseline.json`, `currency-math-baseline.json` and
`holiday-baseline.json` - captured from the Maven-built Java jars by
`tools/parity-capture/capture-baseline.jsh`, whose procedure is in
[`tools/parity-capture/README.md`](../tools/parity-capture/README.md). `parity/ParityHarness.scala`
loads a fixture through `cats-effect` `IO`, decodes it with circe and asserts every numeric row
within 1e-9 both absolute and relative, exact equality being required of dates, lists and text. Each
fixture run writes its row, pass and fail counts to one report per fixture under
`target/parity-report/` *before* asserting that nothing failed, so the counts survive a failing run.

Two further fixtures guard transcription rather than arithmetic.
`src/test/resources/manifest/reference-data-manifest.json` is captured from the Java side by the
same script, and `ReferenceDataManifestSpec` asserts that every Scala data table - down to the
market-convention currency priority order - equals it, so a row that is self-consistent but
mistranscribed cannot pass. `src/test/resources/manifest/java-test-mapping.csv` maps every Java
`@Test` and `@ParameterizedTest` method to the Scala spec that carries it, with a status, giving
method-level traceability from the Java suite to this one.


### Migration notes

[`SCALA_MIGRATION.md`](../SCALA_MIGRATION.md) records the migration in full: the member-level table
of every `strata-collect` member `strata-basics` uses, each with its Scala replacement or an
explicit no-target entry, the collect symbols deliberately left behind, and every deliberate
divergence from the Java behaviour - among them a closed `Currency` of the 74 configured codes with
no dynamic minting, a closed `FxIndex` of the 16 configured rows with no `createFxIndex`, and
exceptions replaced by `Either` and the `Failure` ADT. [`README.md`](../README.md) is the
repository-level overview.


### Source code

This module is released as Open Source Software using the
[Apache v2.0 license](https://www.apache.org/licenses/LICENSE-2.0.html).  
Commercial support is [available](https://opengamma.com/) from the authors.

Code in this module will be maintained with backwards compatibility in mind.

[![OpenGamma](https://s3-eu-west-1.amazonaws.com/og-public-downloads/og-logo-alpha.png "OpenGamma")](https://opengamma.com/)
