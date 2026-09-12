Strata-Basics
-------------
This directory contains the `strata-basics` module: the Scala port of the Java `strata-basics`.
It depends on the Scala [`strata-collect`](../strata-collect/README.md), and the two are the sbt
build's only modules - `strata-basics` is the build's root project, with its sources under
`strata-basics/src`. The package root is retained, `com.opengamma.strata.basics`, and so are the
names of every type, constant and property, so `HolidayCalendarIds.GBLO`,
`BusinessDayConventions.MODIFIED_FOLLOWING`, `Tenor.TENOR_3M` and `Frequency.P3M` mean here what
they meant there. Sources are Scala 2.13.18 on JDK 21, built by the root `build.sbt` with sbt
1.13.0.

### Overview

This module provides the common financial concepts the rest of Strata is built from. The port is
incremental and these are the concepts it carries today:

* reference data - an explicit `ReferenceData` that a caller supplies and a `ReferenceDataId` that
  resolves against it, never an ambient lookup; `StandardId` and the standard schemes
* money and currency - `Currency`, `CurrencyPair`, `CurrencyAmount`, `FxRate` and the rate providers
* holiday calendars - the `HolidayCalendar` family, its identifiers, the twenty-five generated
  national calendars and the published Thai dates, with the holiday-safe reference data
* business-day conventions and `BusinessDayAdjustment`
* dates - `Tenor`, `DateSequence`, `PeriodAdditionConvention`, `DateAdjuster` and `LocalDateUtils`
* `Frequency`, and the value types `Rounding`, `ValueAdjustment`, `ValueAdjustmentType` and
  `ValueDerivatives`
* indices - the sealed `Index` family, the floating-rate names and types, and their reference data
* `Country` and its code tables

Every value type is immutable, every failure that depends on the data of a call is returned as
`Either[Failure, _]` or `EitherNec[Failure, _]` rather than thrown, and the named families are
closed sealed ADTs with a `NamedEnum` instance instead of a runtime registry. Serialization is
circe, derived at compile time; there is no Joda-Beans, no Java serialization and no reflection.

### Reference data is code, not configuration

The Java module loaded its currencies, currency pairs, countries, indices, floating-rate names and
holiday calendars from INI, CSV and properties resources on the class path, through an extended-enum
registry that an application could override. This port holds all of it as Scala data - the `*Data`
objects beside the types that read them - and the generated holiday calendars as the rules that
produce them, so a lookup cannot depend on what happens to be on the class path and no resource is
read at run time. The tables are checked row for row against a manifest of values captured from the
Java implementation, so a row that is self-consistent but mistranscribed cannot pass, and the
numerical results are checked against Java baselines to 1e-9, absolute and relative, by the parity
specs.

An application that needs holidays of its own supplies its own `ReferenceData` mapping the same
identifiers - or identifiers of its own - to whatever calendars it trusts. That is the one
extension point, and it is a parameter rather than a resource.

### Working with reference data, and resolving once

`ReferenceData.standard` holds the built-in holiday calendars; `ReferenceData.minimal` holds only
the four calendars whose content follows from their name. A calendar is reached by resolving an
identifier against the data a caller holds:

```scala
val calendar = HolidayCalendarIds.GBLO.resolve(ReferenceData.standard)   // Either[Failure, HolidayCalendar]
```

Types that name a calendar rather than holding one - `BusinessDayAdjustment` today, and the
adjustments and schedules that follow it - offer two forms, and they differ only in when that
resolution happens:

```scala
val adjustment = BusinessDayAdjustment.of(BusinessDayConventions.MODIFIED_FOLLOWING, HolidayCalendarIds.GBLO)

// one date: resolve and adjust in one call
val one = adjustment.adjust(date, ReferenceData.standard)

// many dates: resolve once, then adjust each date against the calendar already in hand
val many = adjustment.resolve(ReferenceData.standard).map(adjuster => dates.map(adjuster.adjust))

// or compose several adjustments before any data is available, and supply it once
val reader = adjustment.toReader.map(adjuster => dates.map(adjuster.adjust))
val result = reader.run(ReferenceData.standard)
```

**Resolve once for a run of dates, and especially for a composite calendar.** A composite
identifier such as `GBLO+USNY` is resolved by asking the reference data for the whole name and, when
it does not hold it, resolving each part and reading the results together; `adjust(date, refData)`
repeats that assembly for every date. Measured over a batch of dates, resolving once is cheaper per
date for any calendar, several times cheaper for a composite one, and allocates a fraction as much -
the wider the composite, the wider the difference. The `toReader` form costs the same as `resolve`
and composes, so there is no reason to resolve per date in a loop. A resolved adjuster is bound to the
calendar as it stood when it was resolved and does not follow later changes to the reference data,
which is the one thing the unresolved form gives you and the reason both exist.

The same rule applies to the calendars themselves: `HolidayCalendar.combinedWith` builds a
combination that reads both calendars on every query, which suits one calculation, while
`ImmutableHolidayCalendar.combined` merges their holiday data once and answers as fast as either
part - build that one up front for a combination an application will hold and reuse.

### Building and testing

```
sbt test                                     # both modules, through the root aggregation
sbt "strata-basics/testOnly com.opengamma.strata.basics.*"   # this module's specs alone
sbt "strata-basics/testOnly com.opengamma.strata.basics.date.HolidayCalendarsSpec"
sbt "strata-basics/testOnly *Parity*"        # the Java-baseline parity specs
```

`strata-basics` is the build's root project and aggregates `strata-collect`, so `test` scoped to it
runs both modules; the second form above is what runs this module's specs on their own.

The specs are ScalaTest with ScalaCheck properties, one spec per Java test class, and the mapping
from each Java test method to the spec that carries it is recorded in
`src/test/resources/manifest/java-test-mapping.csv`.

### Parity harness

The port's numbers are pinned to the Java implementation's. `src/test/resources/parity` holds the
baseline fixtures captured from the Java jars by `tools/parity-capture`, one per subject -
`daycount`, `schedule`, `fx`, `currency-math` and `holiday` - and `parity/ParityHarness` loads a
fixture through `cats-effect` `IO`, decodes it with circe and asserts every row within 1e-9 both
absolute and relative, exact equality being required of dates, lists and text. Each fixture run
writes `<parity.report.dir>/<fixture>.json` with its row, pass and fail counts *before* asserting
that nothing failed, so the counts survive a failing run; `build.sbt` points `parity.report.dir` at
`target/parity-report` in the repository root, and the tests are forked so every project writes
there.

### Source code

This module is released as Open Source Software using the
[Apache v2.0 license](https://www.apache.org/licenses/LICENSE-2.0.html).  
Commercial support is [available](https://opengamma.com/) from the authors.

Code in this module will be maintained with backwards compatibility in mind.

[![OpenGamma](https://s3-eu-west-1.amazonaws.com/og-public-downloads/og-logo-alpha.png "OpenGamma")](https://opengamma.com/)
