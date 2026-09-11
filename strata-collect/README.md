Strata-Collect
--------------
This directory contains the `strata-collect` module, ported to Scala.

This is the Scala port of the subset of the Java `strata-collect` that `strata-basics` uses, and one
of exactly two Scala modules in the sbt build; the other, `strata-basics`, depends on it. The package
is unchanged, `com.opengamma.strata.collect`, so ported code keeps its imports. The sources are
Scala 2.13.18 targeting JDK 21, built by the root `build.sbt` with sbt 1.13.0.

### Overview

This module provides the data structures, the error model and the codec helpers of the port:

* named - the `NamedEnum` typeclass for closed named enums, providing `values`, `valueOf`, `parse`,
  `alternateNames`, `externalNames(group)`, `byUpperName` and `byCanonicalName`, with the `Named`
  trait and `TypedString` support; it replaces the Java `ExtendedEnum` and its INI registry, so there
  is no runtime registry and no classpath configuration, while alias precedence, the FpML and SWIFT
  external name groups and lenient parsing survive as in-code tables
* result - the sealed `Failure` ADT, one case per reason, the ten `FailureReason` values and the
  `FailureOr`, `ResultNec`, `ValidatedFailures` and `ValueWithFailures` aliases with their
  combinators; failures are values, and this module defines no exception type
* validation - `ArgCheck` for fail-fast invariants and `Validate` for the accumulating
  `ValidatedNec[Failure, A]` checks of smart constructors, both ported from the Java `ArgChecker`
* array - immutable `DoubleArray` and `DoubleMatrix` over private primitive arrays, with the `Matrix`
  trait and the `DoubleArrayMath` helpers; the public API is copy-safe, as the Java `ofUnsafe` and
  `toArrayUnsafe` escape hatches are module-private, and these two types carry the port's 1e-9
  numerical parity responsibility
* decimal - `Decimal` and `FixedScaleDecimal`
* collections - only the `Guavate` and `MapStream` helpers `strata-basics` uses, such as
  `ensureOnlyOne` and the sorted-map and order-preserving grouping helpers, expressed over
  `scala.collection.immutable` and `cats.data`
* json - circe codec building blocks: named-enum and parsed-string codecs, key codecs, validated
  decoders, null-dropping encoders, the tagged non-finite `Double` codec and the `DayOfWeek` and
  array codecs; products derive at compile time with `io.circe.generic.semiauto`, so nothing on the
  codec path uses reflection
* io - a minimal `cats-effect` reader for classpath and file text, which is the module's only
  effectful code, everything else being pure


### Ported subset

The port follows actual use: the Java `strata-basics` main sources reference 29 distinct
`com.opengamma.strata.collect` targets and its tests three more - `TestHelper`,
`CollectProjectAssertions` and `Unchecked` - and only those are ported. Deliberately absent:

* the function, timeseries and concurrent packages, and `IntArray`, `LongArray`, `BasisPoints`,
  `Percentage`, `NumberFormatter`, `CharMatchers` and `Version` - unused by `strata-basics`
* tuple - Scala tuples are used instead
* `Messages` - string interpolation is used instead
* the io INI, CSV and `ResourceConfig` readers - the reference data they loaded is now Scala data in
  `strata-basics`
* `FailureException`, `FailureItemException`, `IllegalArgFailureException` and
  `ParseFailureException` - failures are values, not exceptions

The classpath carries no Guava, no Joda-Beans, no Joda-Convert and no Java `strata-collect` artifact.
[SCALA_MIGRATION.md](../SCALA_MIGRATION.md) holds the member-level symbol table and every deliberate
divergence.


### Forward path

This module is the foundation of an incremental migration: later slices port further Strata modules
from their Java sources and grow `strata-collect` with the symbols they need. Two constraints keep
that possible, and new code is held to both: the module keeps its original name and package, so
imports port symbol by symbol, and its public API stays free of anything `strata-basics`-specific.
`externalNames` is retained for the second reason - the FpML and SWIFT alias groups belong to the
collect contract rather than to `strata-basics`.

The module is usable and testable on its own, `sbt "strata-collect/test"` running it alone, and its
test-scope helpers `testkit.TestHelper`, `testkit.ResultMatchers` and `Arbitraries` are shared with
the `strata-basics` tests through the sbt `test->test` dependency that replaces the former Maven
test-jar. See the [root README](../README.md) for the full command set and
[verify-gates.sh](../scripts/verify-gates.sh) for the acceptance gates.


### Source code

This module is released as Open Source Software using the
[Apache v2.0 license](https://www.apache.org/licenses/LICENSE-2.0.html).  
Commercial support is [available](https://opengamma.com/) from the authors.

Code in this module will be maintained with backwards compatibility in mind.

[![OpenGamma](https://s3-eu-west-1.amazonaws.com/og-public-downloads/og-logo-alpha.png "OpenGamma")](https://opengamma.com/)
